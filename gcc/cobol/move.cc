/*
 * Copyright (c) 2021-2026 Symas Corporation
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * * Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 * * Redistributions in binary form must reproduce the above
 *   copyright notice, this list of conditions and the following disclaimer
 *   in the documentation and/or other materials provided with the
 *   distribution.
 * * Neither the name of the Symas Corporation nor the names of its
 *   contributors may be used to endorse or promote products derived from
 *   this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include "cobol-system.h"

#include "coretypes.h"
#include "tree.h"
#include "tree-iterator.h"
#include "stringpool.h"
#include "diagnostic-core.h"
#include "target.h"

#include "../../libgcobol/ec.h"
#include "../../libgcobol/common-defs.h"
#include "util.h"
#include "cbldiag.h"
#include "symbols.h"
#include "gengen.h"
#include "inspect.h"
#include "../../libgcobol/io.h"
#include "genapi.h"
#include "genutil.h"
#include "genmath.h"
#include "structs.h"
#include "../../libgcobol/gcobolio.h"
#include "../../libgcobol/cobol-endian.h"
#include "../../libgcobol/charmaps.h"
#include "../../libgcobol/valconv.h"
#include "show_parse.h"
#include "fold-const.h"
#include "realmpfr.h"
#include "compare.h"

#if 0
// This is a debugging function used from time-to-time
static void
hex_of(tree location, size_t bytes)
  {
  gg_printf("0x", NULL_TREE);
  for(size_t i=0; i<bytes; i++)
    {
    gg_printf("%2.2X", gg_indirect_i(gg_cast(UCHAR_P, location), i), NULL_TREE);
    }
  }

static void
hex_msg(const char *msg, tree location, size_t bytes)
  {
  gg_printf("%s ", gg_string_literal(msg), NULL_TREE);
  hex_of(location, bytes);
  gg_printf("\n", NULL_TREE);
  }

#endif

static cbl_figconst_t
is_figconst_t(const cbl_field_t *field)
  {
  cbl_figconst_t figconst = (cbl_figconst_t)(field->attr & FIGCONST_MASK);
  return figconst;
  }

static cbl_figconst_t
is_figconst(const cbl_refer_t &sourceref)
  {
  return is_figconst_t(sourceref.field);
  }

static void
conditional_abs(tree source, const cbl_field_t *field)
  {
  Analyze();
  if( !(field->attr & signable_e) )
    {
    gg_assign(source, gg_abs(source));
    }
  }

static tree
get_literalN_value(cbl_field_t *var)
  {
  // Get the literal N value from the integer var_decl
  tree retval = NULL_TREE;
  tree var_type = tree_type_from_field(var);
  retval = gg_cast(var_type, var->data_decl_node);
  return retval;
  }

static void
get_binary_value_from_float(tree         value,
                      const cbl_refer_t &dest,
                            cbl_field_t *source,
                            tree         source_offset
                            )
  {
  // The destination is something with rdigits; the source is FldFloat
  tree ftype;
  switch( source->data.capacity() )
    {
    case 4:
      ftype = FLOAT;
      break;
    case 8:
      ftype = DOUBLE;
      break;
    case 16:
      ftype = FLOAT128;
      break;
    default:
      gcc_unreachable();
      break;
    }
  tree fvalue = gg_define_variable(ftype);
  gg_assign(fvalue,
            gg_indirect(gg_cast(build_pointer_type(ftype),
                                gg_add( member(source->var_decl_node,"data"),
                                        source_offset))));

  // We need to convert the floating point value to an integer value with the
  // rdigits lined up properly.

  int rdigits = get_scaled_rdigits( dest.field );
  gg_assign(fvalue,
            gg_multiply(fvalue,
                        gg_float(ftype,
                                 wide_int_to_tree(INT,
                                                  get_power_of_ten(rdigits)))));

  // And we need to throw away any digits to the left of the leftmost digits:
  // At least, we need to do so in principle.  I am deferring this problem until
  // I understand it better.

  // We now have a floating point value that has been multiplied by 10**rdigits
  gg_assign(value, gg_trunc(TREE_TYPE(value), fvalue));
  }

static bool
mh_identical(const cbl_refer_t &destref,
             const cbl_refer_t &sourceref)
  {
  // Check to see if the two variables are identical types, thus allowing
  // for a simple byte-for-byte copy of the data areas:
  bool moved = false;
  if(     destref.field->type            == sourceref.field->type
      &&  destref.field->data.capacity() == sourceref.field->data.capacity()
      &&  destref.field->data.digits     == sourceref.field->data.digits
      &&  destref.field->data.rdigits    == sourceref.field->data.rdigits
      &&       (destref.field->attr   & (signable_e|separate_e|leading_e))
            == (sourceref.field->attr & (signable_e|separate_e|leading_e))
      &&  destref.field->codeset.encoding == sourceref.field->codeset.encoding
      &&  !sourceref.refmod.from
      &&  !sourceref.refmod.len
      &&  !destref.refmod.from
      &&  !destref.refmod.len
      &&  !sourceref.subscripts.size()
      &&  !destref.subscripts.size()
      &&  !(sourceref.field->attr & intermediate_e)
      &&  !(destref.field->attr & intermediate_e)
      &&  !(sourceref.field->attr & any_length_e)
      &&  !(destref.field->attr & any_length_e)
      )
    {
    // The source and destination are identical in type and the
    // source doesn't have a depending_on clause
    SHOW_PARSE1
      {
      SHOW_PARSE_INDENT
      SHOW_PARSE_TEXT("mh_identical()");
      }

    // They are identical, and they have no refmods or subscripts
    tree source;
    tree dest;
    get_location(source, sourceref);
    get_location(dest, destref);

    gg_memcpy(dest,
              source,
              build_int_cst_type(SIZE_T,
                                 destref.field->data.capacity()));
    moved = true;
    }
  return moved;
  }

static bool
mh_source_is_literalN(cbl_refer_t &destref,
                      cbl_refer_t &sourceref,
                      bool         check_for_error,
                      cbl_round_t  rounded,
                      tree         size_error)
  {
  bool moved = false;
  if( sourceref.field->type == FldLiteralN )
    {
    Analyze();
    switch( destref.field->type )
      {
      case FldGroup:
      case FldAlphanumeric:
        {
        SHOW_PARSE1
          {
          SHOW_PARSE_INDENT
          SHOW_PARSE_TEXT("mh_source_is_literalN: __gg__psz_to_alpha_move")
          }

        // In accordance with the rules of moving a numeric to an alphabetic,
        // we need to eliminate any leading sign character from the text
        // string:

        const char *original = sourceref.field->data.original();
        if( *original == ascii_plus || *original == ascii_minus )
          {
          original += 1;
          }

        // We need the data sent to __gg__psz_to_alpha_move to be in the
        // encoding of the destination.  In accordance with the rules of
        // cbl_field_t::internalize, the FldLiteralN is in source-code
        // encoding, so we have to convert.

        size_t charsout;
        const char *converted = __gg__iconverter(
                                         DEFAULT_SOURCE_ENCODING,
                                         destref.field->codeset.encoding,
                                         original,
                                         strlen(original),
                                         &charsout);
        gg_call(VOID,
                "__gg__psz_to_alpha_move",
                gg_get_address_of(destref.field->var_decl_node),
                refer_offset(destref),
                refer_size_dest(destref),
                build_string_literal(charsout, converted),
                build_int_cst_type(SIZE_T, charsout),
                NULL_TREE);
        moved = true;
        break;
        }

      case FldPointer:
      case FldIndex:
        {
        SHOW_PARSE1
          {
          SHOW_PARSE_INDENT
          SHOW_PARSE_TEXT("mh_source_is_literalN: pointer/index")
          }

        tree source;
        get_binary_value(source, sourceref);

        tree dest_type = tree_type_from_refer(destref);
        tree dest = gg_define_variable(dest_type);
        gg_assign(dest, gg_cast(dest_type, source));

        tree location;
        get_location(location, destref);
        gg_memcpy(location,
                  gg_get_address_of(dest),
                  build_int_cst_type(SIZE_T, gg_sizeof(dest_type)));
        moved = true;

        break;
        }

      case FldNumericBin5:
        {
        // We are moving from a FldLiteralN (which we know has no subscripts or
        // refmods), to a NumericBin5, which might.

        SHOW_PARSE1
          {
          SHOW_PARSE_INDENT
          SHOW_PARSE_TEXT("mh_source_is_literalN: FldNumericBin5")
          }

        // For now, we are ignoring intermediates:
        assert( !(destref.field->attr & intermediate_e) );
        tree calc_type = tree_type_from_refer(sourceref);
        tree dest_type = tree_type_from_refer(destref);

        // Pick up the source data.
        tree source = gg_define_variable(calc_type);
        tree dest   = gg_define_variable(dest_type);
        gg_assign(source, gg_cast(calc_type, sourceref.field->data_decl_node));

        // Take the absolute value, if the destination is not signable
        conditional_abs(source, destref.field);

        // Cast our source to the target:
        gg_assign(dest, gg_cast(dest_type, source));

        // See if it needs to be scaled:
        scale_by_power_of_ten_N(
                    dest,
                    destref.field->data.rdigits-sourceref.field->data.rdigits);

        if( check_for_error && size_error )
          {
          Analyzer.Message("Check to see if result fits");
          if( destref.field->data.digits )
            {
            FIXED_WIDE_INT(128) power_of_ten =
                                 get_power_of_ten(destref.field->data.digits);
            IF( dest, ge_op, wide_int_to_tree(calc_type, power_of_ten) )
              {
              gg_assign(size_error,
                        gg_bitwise_or(size_error, integer_one_node));
              }
            ELSE
              ENDIF
            }
          }

        tree dest_location;
        get_location(dest_location, destref);
        gg_memcpy(dest_location,
                  gg_get_address_of(dest),
                  build_int_cst_type(SIZE_T, gg_sizeof(dest)));
        moved = true;
        break;
        }

      case FldNumericDisplay:
      case FldNumericBinary:
      case FldNumericEdited:
      case FldPacked:
        {
        tree berror = gg_define_variable(INT);
        gg_assign(berror, integer_zero_node);
        SHOW_PARSE1
          {
          SHOW_PARSE_INDENT
          SHOW_PARSE_TEXT("calling get_literalN_value ")
          }
        tree literalN_value = get_literalN_value(sourceref.field);

        SHOW_PARSE1
          {
          SHOW_PARSE_INDENT
          SHOW_PARSE_TEXT("calling __gg__int128_to_qualified_field ")
          }

        gg_call(INT,
                "__gg__int128_to_qualified_field",
                gg_get_address_of(destref.field->var_decl_node),
                refer_offset(destref),
                refer_size_dest(destref),
                gg_cast(INT128, literalN_value),
                build_int_cst_type(INT, sourceref.field->data.rdigits),
                build_int_cst_type(INT, rounded),
                gg_get_address_of(berror),
                NULL_TREE);

        if( size_error )
          {
          IF( berror, ne_op, integer_zero_node  )
            {
            gg_assign(size_error, gg_bitwise_or(size_error, integer_one_node));
            }
          ELSE
            ENDIF
          }
        moved = true;
        break;
        }

      case FldAlphaEdited:
        {
        SHOW_PARSE1
          {
          SHOW_PARSE_INDENT
          SHOW_PARSE_TEXT(" FldAlphaEdited")
          }

        // __gg__string_to_alpha_edited expects the source string to be in
        // the same encoding as the target.  The rule in internalize is that
        // a FldLiteralN::data.initial is left in source-code space, so it
        // needs to be converted to the destination encoding.
        size_t charsout;
        const char *converted_ = __gg__iconverter(
                                            DEFAULT_SOURCE_ENCODING,
                                            destref.field->codeset.encoding,
                                            sourceref.field->data.original(),
                                            strlen(sourceref.field->data.original()),
                                            &charsout);
        // Copy converted, because __gg__string_to_alpha_edited might have its
        // own reasons to use charmap_t, which could mess up the static buffer
        // used by __gg__iconverter:
        char *converted = xstrdup(converted_);
        gg_call(VOID,
                "__gg__string_to_alpha_edited",
                gg_add( member(destref.field->var_decl_node, "data"),
                        refer_offset(destref) ),
                build_int_cst_type(INT, destref.field->codeset.encoding),
                gg_string_literal(converted),
                build_int_cst_type(INT, strlen(converted)),
                gg_string_literal(destref.field->data.picture),
                NULL_TREE);
        moved = true;
        free(converted);
        break;
        }

      case FldFloat:
        {
        tree tdest = gg_add(member(destref.field->var_decl_node, "data"),
                            refer_offset(destref) );
        switch( destref.field->data.capacity() )
          {
          case 4:
            {
            // The following generated code is the exact equivalent
            // of the C code:
            //   *(float *)dest = (float)data.value
            gg_assign(gg_indirect(gg_cast(build_pointer_type(FLOAT), tdest)),
                      gg_cast (FLOAT, sourceref.field->data.value_of()));
            break;
            }
          case 8:
            {
            gg_assign(gg_indirect(gg_cast(build_pointer_type(DOUBLE), tdest)),
                      gg_cast (DOUBLE, sourceref.field->data.value_of()));
            break;
            }
          case 16:
            {
            gg_assign(gg_indirect(gg_cast(build_pointer_type(FLOAT128), tdest)),
                      sourceref.field->data.value_of());
            break;
            }
          }
        moved=true;
        break;
        }

      default:
        cbl_internal_error(
              "In %<parser_move(%s to %s)%>, the move of FldLiteralN to %s "
              "is unimplemented",
              sourceref.field->name,
              destref.field->name,
              cbl_field_type_str(destref.field->type));
        break;
      }
    }
  return moved;
  }

static
tree float_type_of(int n)
  {
  switch(n)
    {
    case 4:
      return FLOAT;
    case 8:
      return DOUBLE;
    case 16:
      return FLOAT128;
    default:
      gcc_unreachable();
    }
  return NULL_TREE;
  }

static tree
float_type_of(const cbl_field_t *field)
  {
  gcc_assert(field->type == FldFloat);
  return float_type_of(field->data.capacity());
  }

static tree
float_type_of(const cbl_refer_t *refer)
  {
  return float_type_of(refer->field);
  }

static bool
mh_dest_is_float( cbl_refer_t &destref,
                  cbl_refer_t &sourceref,
                  TREEPLET    &tsource,
                  cbl_round_t    rounded,
                  tree         size_error) // int
  {
  bool moved = false;
  if( destref.field->type == FldFloat )
    {
    Analyze();
    switch( sourceref.field->type )
      {
      case FldPointer:
      case FldIndex:
      case FldNumericBin5:
      case FldNumericDisplay:
      case FldNumericBinary:
      case FldNumericEdited:
      case FldPacked:
        {
        switch( destref.field->data.capacity() )
          {
          case 4:
            gg_call(VOID,
                    "__gg__float32_from_int128",
                    gg_get_address_of(destref.field->var_decl_node),
                    refer_offset(destref),
                    tsource.pfield,
                    tsource.offset,
                    build_int_cst_type(INT, rounded),
                    size_error ? gg_get_address_of(size_error) : null_pointer_node,
                    NULL_TREE);
            break;
          case 8:
            gg_call(VOID,
                    "__gg__float64_from_int128",
                    gg_get_address_of(destref.field->var_decl_node),
                    refer_offset(destref),
                    tsource.pfield,
                    tsource.offset,
                    build_int_cst_type(INT, rounded),
                    size_error ? gg_get_address_of(size_error) : null_pointer_node,
                    NULL_TREE);
            break;
          case 16:
            gg_call(VOID,
                    "__gg__float128_from_int128",
                    gg_get_address_of(destref.field->var_decl_node),
                    refer_offset(destref),
                    tsource.pfield,
                    tsource.offset,
                    build_int_cst_type(INT, rounded),
                    size_error ? gg_get_address_of(size_error) : null_pointer_node,
                    NULL_TREE);
            break;
          }
        moved = true;
        break;
        }

      case FldFloat:
        {
        // We are testing for size.  First, we need to check to see if the
        // source is INFINITY.  If so, that's an automatic size error

        IF( gg_call_expr( INT,
                          "__gg__is_float_infinite",
                          tsource.pfield,
                          tsource.offset,
                          NULL_TREE),
            ne_op,
            integer_zero_node )
          {
          if( size_error )
            {
            gg_assign(size_error, integer_one_node );
            }
          }
        ELSE
          {
          // The source isn't infinite.
          // If the destination is bigger than the source, then we can
          // do an untested move:

          if( destref.field->data.capacity() >= sourceref.field->data.capacity() )
            {
            tree dtype = float_type_of(&destref);
            tree stype = float_type_of(&sourceref);

            tree tdest = gg_add(member(destref.field->var_decl_node, "data"),
                               refer_offset(destref));
            tree source = gg_add(member(sourceref.field->var_decl_node, "data"),
                                refer_offset(sourceref));
            gg_assign(gg_indirect(gg_cast(build_pointer_type(dtype), tdest)),
                      gg_cast(dtype,
                              gg_indirect(gg_cast(build_pointer_type(stype),
                                          source))));
            }
          else
            {
            // There are only three possible moves left:
            if(destref.field->data.capacity() == 8 )
              {
              if( size_error )
                {
                gg_assign(size_error,
                          gg_call_expr( INT,
                                "__gg__float64_from_128",
                                gg_get_address_of(destref.field->var_decl_node),
                                refer_offset(destref),
                                tsource.pfield,
                                tsource.offset,
                                NULL_TREE));
                }
              else
                {
                          gg_call( INT,
                                "__gg__float64_from_128",
                                gg_get_address_of(destref.field->var_decl_node),
                                refer_offset(destref),
                                tsource.pfield,
                                tsource.offset,
                                NULL_TREE);
                }
              }
            else
              {
              // The destination has to be float32
              if( sourceref.field->data.capacity() == 8 )
                {
                if( size_error )
                  {
                  gg_assign(size_error,
                            gg_call_expr( INT,
                                "__gg__float32_from_64",
                                gg_get_address_of(destref.field->var_decl_node),
                                refer_offset(destref),
                                tsource.pfield,
                                tsource.offset,
                                NULL_TREE));
                  }
                else
                  {
                            gg_call( INT,
                                "__gg__float32_from_64",
                                gg_get_address_of(destref.field->var_decl_node),
                                refer_offset(destref),
                                tsource.pfield,
                                tsource.offset,
                                NULL_TREE);
                  }

                }
              else
                {
                if( size_error )
                  {
                  gg_assign(size_error,
                            gg_call_expr( INT,
                                "__gg__float32_from_128",
                                gg_get_address_of(destref.field->var_decl_node),
                                refer_offset(destref),
                                tsource.pfield,
                                tsource.offset,
                                NULL_TREE));
                  }
                else
                  {
                            gg_call( INT,
                                "__gg__float32_from_128",
                                gg_get_address_of(destref.field->var_decl_node),
                                refer_offset(destref),
                                tsource.pfield,
                                tsource.offset,
                                NULL_TREE);
                  }
                }
              }
            }
          }
        ENDIF

        moved = true;
        break;
        }

      case FldLiteralA:
      case FldAlphanumeric:
      case FldGroup:
        {
        // Alphanumeric to float is inherently slow.  Send it off to the library
        break;
        }

      default:
        cbl_internal_error("In %<mh_dest_is_float%>(%s to %s), the "
                           "move of %s to %s is unimplemented",
              sourceref.field->name,
              destref.field->name,
              cbl_field_type_str(sourceref.field->type),
              cbl_field_type_str(destref.field->type));
        break;
      }
    }
  return moved;
  }

static void
picky_memset(tree &dest_p, unsigned char value, size_t length)
  {
  if( length )
    {
    tree dest_ep = gg_define_variable(TREE_TYPE(dest_p));
    gg_assign(dest_ep,
              gg_add( dest_p,
                      build_int_cst_type(SIZE_T, length)));
    WHILE( dest_p, lt_op, dest_ep )
      {
      gg_assign(gg_indirect(dest_p),
                build_int_cst_type(UCHAR, value));
      gg_increment(dest_p);
      }
      WEND
    }
  }

static void
picky_memcpy(tree &dest_p, const tree &source_p, size_t length, tree zero)
  {
  // This is the routine that copies digits for NumericDisplay.  In addition
  // to just moving digits from source to destination, it has to handle
  // clearing up embedded sign information.
  if( length )
    {
    tree dest_ep = gg_define_variable(TREE_TYPE(dest_p));
    gg_assign(dest_ep,
              gg_add( dest_p,
                      build_int_cst_type(SIZE_T, length)));
    WHILE( dest_p, lt_op, dest_ep )
      {
      gg_assign(gg_indirect(dest_p),
                gg_bitwise_or(zero,
                      gg_bitwise_and(gg_indirect(source_p),
                             build_int_cst_type(UCHAR, 0x0F))));
      gg_increment(dest_p);
      gg_increment(source_p);
      }
      WEND
    }
  }

static void
clear_negative_zero(const cbl_refer_t &destref,
                    const cbl_refer_t &sourceref,
                          tree        dest_location)
  {
  // It is an idiosyncracy of numeric-edited and packed-decimal that a
  // truncated value can end up zero, but with a negative flag.  This routine
  // makes such values positive.
  if(    !(sourceref.field->attr & signable_e)
      || !(destref.field->attr   & signable_e) )
    {
    return;
    }
  // They are both signable.
  // Was truncation involved?
  if(    (sourceref.field->data.digits - sourceref.field->data.rdigits)
      <= (destref.field->data.digits   - destref.field->data.rdigits  ) )
    {
    return;
    }
  // The source side was truncated.

  charmap_t *charmap =
                   __gg__get_charmap(destref.field->codeset.encoding);
  tree goto_bugout;
  tree label_bugout;
  gg_create_goto_pair(&goto_bugout,
                      &label_bugout);
  tree p     = gg_define_variable(UCHAR_P);
  tree p_end =  gg_define_variable(UCHAR_P);
  gg_assign(p, dest_location);
  gg_assign(p_end,
            gg_add(p,
                   build_int_cst_type(SIZE_T,
                                      destref.field->data.capacity()-1)));
  // All the bytes before last one have to be zero:
  tree tzero = build_int_cst_type(UCHAR,
               destref.field->type == FldPacked
               ? 0
               : charmap->mapped_character(ascii_zero));
  WHILE( p, lt_op, p_end )
    {
    IF( gg_indirect(p), ne_op, tzero )
      {
      // This byte is non-zero, so beat it.
      gg_append_statement(goto_bugout);
      }
    ELSE
      {
      }
    ENDIF
    gg_increment(p);
    }
  WEND
  if( destref.field->type == FldPacked )
    {
    // If the final byte is 0x0D, then we have to make it 0x0C
    IF( gg_indirect(p), eq_op, build_int_cst_type(UCHAR, 0x0D) )
      {
      gg_assign(gg_indirect(p), build_int_cst_type(UCHAR, 0x0C) );
      }
    ELSE
      {
      }
    ENDIF
    }
  else
    {
    // This is numeric display.
    IF( gg_bitwise_and(gg_indirect(p), build_int_cst_type(UCHAR, 0x0F)),
        eq_op,
        build_int_cst_type(UCHAR, 0x00) )
      {
      if( charmap->is_like_ebcdic() )
        {
        // We force it positive by making 0xDN into 0xFN
        gg_assign(gg_indirect(p),
                    gg_bitwise_or(gg_indirect(p),
                                  build_int_cst_type(UCHAR, 0xF0)));
        }
      else
        {
        // We force it positive by making 0x7N into 0x3N
          gg_assign(gg_indirect(p),
                    gg_bitwise_and(gg_indirect(p),
                                  build_int_cst_type(UCHAR, 0x3F)));
        }
      }
    ELSE
      {
      }
    ENDIF
    }
  gg_append_statement(label_bugout);
  }

static bool
mh_numeric_display( const cbl_refer_t &destref,
                    const cbl_refer_t &sourceref,
                    const TREEPLET    &tsource,
                          tree         size_error)
  {
  bool moved = false;

  charmap_t *charmap_source =
                       __gg__get_charmap(sourceref.field->codeset.encoding);
  if(     destref.field->type   == FldNumericDisplay
      &&  sourceref.field->type == FldNumericDisplay
      &&  !(destref.field->attr   & scaled_e)
      &&  !(sourceref.field->attr & scaled_e)
      &&  charmap_source->stride() == 1
      &&  sourceref.field->codeset.encoding == destref.field->codeset.encoding
      )
    {
    // We can do simple moves of single-byte same-encoding numeric display.
    // More complex ones get sent to __gg__move

    Analyze();
    // I believe that there are 450 pathways through the following code.
    // That's because there are five different valid combination of signable_e,
    // separate_e, and leading_e.  There are three possibilities for
    // sender/receiver rdigits (too many, too few, and just right), and the
    // same for ldigits.  5 * 5 * 3 * 3 * 2 = 450.

    // Fasten your seat belts.

    // This routine is complicated by the fact that although I had several
    // false starts of putting this into libgcobol, I keep coming back to the
    // fact that assignment of zoned values is common.  And, so, there are all
    // kinds of things that are known at compile time that would turn into
    // execution-time decisions if I moved them to the library.  So, complex
    // or not, I am doing all this code here at compile time because it will
    // minimize the code at execution time.

    // One thing to keep in mind is the problem caused by a source value being
    // internally signed.  That turns an ASCII "123" into "12t", and we
    // very probably don't want that "t" to find its way into the destination
    // value.  The internal sign characteristic of ASCII is that the high
    // nybble of the sign location is 0x30 or 0x70.  For EBCDIC, the high
    // nybble is 0xC0 for positive values, and 0xD0 for negative; all other
    // digits are 0x70.

    charmap_t *charmap_dest   =
                       __gg__get_charmap(  destref.field->codeset.encoding);

    tree source_sign_loc  = gg_define_variable(UCHAR_P);
    tree dest_sign_loc = gg_define_variable(UCHAR_P);
    tree source_sign      = gg_define_variable(INT);
    // The destination data pointer
    tree dest_p    = gg_define_variable( UCHAR_P);
    // The source data pointer
    tree source_p  = gg_define_variable( UCHAR_P);
    // When we need an end pointer
    tree source_ep = gg_define_variable(UCHAR_P);

    bool source_is_signable = sourceref.field->attr & signable_e;
    bool source_is_leading  = sourceref.field->attr & leading_e;
    bool source_is_separate = sourceref.field->attr & separate_e;

    bool dest_is_signable = destref.field->attr & signable_e;
    bool dest_is_leading  = destref.field->attr & leading_e;
    bool dest_is_separate = destref.field->attr & separate_e;

    int switch_source =   (source_is_signable ? 4 : 0 )
                        + (source_is_leading  ? 2 : 0 )
                        + (source_is_separate ? 1 : 0 ) ;

    int switch_dest   =   (dest_is_signable ? 4 : 0 )
                        + (dest_is_leading  ? 2 : 0 )
                        + (dest_is_separate ? 1 : 0 ) ;

    // Calculate the start of the source data:
    gg_assign(source_p, gg_add(member(sourceref.field, "data"),
                               tsource.offset));

    // Calculate the start of the destination data
    gg_assign(dest_p,   qualified_data_location(destref));

    // Figure out exactly where the sign is, if any, and where the input
    // digits are.

    switch( switch_source )
      {
      case 0:
      case 1:
      case 2:
      case 3:
        // not signable
        gg_assign(source_sign, integer_zero_node);
        break;
      case 4:
        //     signable, not leading, not separate
        // Calculate location of the sign byte; it's the last byte of the data
        gg_assign(source_sign_loc,
                  gg_add(source_p,
                        build_int_cst_type(SIZE_T,
                                          sourceref.field->data.capacity()-1)));
        break;
      case 5:
        //     signable, not leading,     separate
        // Calculate location of the sign byte; it's the last byte of the data
        gg_assign(source_sign_loc,
                  gg_add(source_p,
                        build_int_cst_type(SIZE_T,
                                          sourceref.field->data.capacity()-1)));
        break;
      case 6:
        //     signable,     leading, not separate
        // Calculate location of the sign byte; it's the first byte of the data
        gg_assign(source_sign_loc, source_p);
        break;
      case 7:
        //     signable,     leading,     separate
        // Calculate location of the sign byte; it's the first byte of the data
        gg_assign(source_sign_loc, source_p);
        gg_increment(source_p);
        break;
      }
    // At this point, the source sign is at source_sign_loc, and the digits
    // start at source_p

    // Let's learn what the source sign is
    if( source_is_signable && source_is_separate )
      {
      IF( gg_indirect(source_sign_loc),
          eq_op,
          build_int_cst_type(UCHAR,
                             charmap_source->mapped_character(ascii_minus)) )
        {
        // Flag the source as negative
        gg_assign(source_sign, integer_one_node);
        }
      ELSE
        {
        // Flag the source as positive
        gg_assign(source_sign, integer_zero_node);
        }
      ENDIF
      }
    if( source_is_signable && !source_is_separate )
      {
      // We need to look for an indication that we are internally signed. We
      // can tell that by checking to see if the digit is between '0' and '9'
      IF( gg_indirect(source_sign_loc),
          lt_op,
          build_int_cst_type(UCHAR,
                             charmap_source->mapped_character(ascii_0)) )
        {
        // The sign byte is less than '0', so we are negative
        gg_assign(source_sign, integer_one_node);
        }
      ELSE
        {
        IF( gg_indirect(source_sign_loc),
            gt_op,
            build_int_cst_type(UCHAR,
                               charmap_source->mapped_character(ascii_9)) )
          {
          // The sign byte is greater than '9', so we are negative
          gg_assign(source_sign, integer_one_node);
          }
        ELSE
          {
          // The sign byte is betwixt '0' and '9', so we are positive
          gg_assign(source_sign, integer_zero_node);
          }
        ENDIF
        }
      ENDIF
      }

    // We now know the source's sign, and where its digits are.

    // The first order of business is to move the digits into place.  To do
    // that, we need to know where things go in the destination:

    switch( switch_dest )
      {
      case 0:
      case 1:
      case 2:
      case 3:
        // not signable
        break;
      case 4:
        //     signable, not leading, not separate
        // Calculate location of the sign byte; it's the last byte of the data
        gg_assign(dest_sign_loc,
                  gg_add(dest_p,
                        build_int_cst_type(SIZE_T,
                                          destref.field->data.capacity()-1)));
        break;
      case 5:
        //     signable, not leading,     separate
        // Calculate location of the sign byte; it's the last byte of the data
        gg_assign(dest_sign_loc,
                  gg_add(dest_p,
                        build_int_cst_type(SIZE_T,
                                          destref.field->data.capacity()-1)));
        break;
      case 6:
        //     signable,     leading, not separate
        // Calculate location of the sign byte; it's the first byte of the data
        gg_assign(dest_sign_loc, dest_p);
        break;
      case 7:
        //     signable,     leading,     separate
        // Calculate location of the sign byte; it's the first byte of the data
        gg_assign(dest_sign_loc, dest_p);
        gg_increment(dest_p);
        break;
      }

    // We can now start copying the digits to the left of the decimal place

    int dest_ldigits   = (int)destref.field->data.digits
                              - destref.field->data.rdigits;
    int source_ldigits = (int)sourceref.field->data.digits
                              - sourceref.field->data.rdigits;

    int digit_count = 0;

    if( dest_ldigits > source_ldigits )
      {
      // The destination has more ldigits than the source, and needs some
      // leading zeroes:
      picky_memset( dest_p,
                    charmap_dest->mapped_character(ascii_0) ,
                    dest_ldigits - source_ldigits);
      // With the leading zeros set, set the number of ldigits to copy:
      digit_count = source_ldigits;
      }
    else if( dest_ldigits == source_ldigits )
      {
      // This is the Goldilocks zone.  Everything is *just* right.
      digit_count = dest_ldigits;
      }
    else // dest_ldigits < source_ldigits
      {
      // The destination is smaller than the source.  We have to throw away the
      // the high-order digits of the source.  If any of them are non-zero, then
      // we need to indicate a size error.
      gg_assign(source_ep,
                gg_add( source_p,
                        build_int_cst_type( SIZE_T,
                                            source_ldigits-dest_ldigits)));
      WHILE(source_p, lt_op, source_ep)
        {
        if( size_error )
          {
          IF( gg_indirect(source_p),
              ne_op,
              build_int_cst_type( UCHAR,
                                  charmap_source->mapped_character(ascii_0)) )
            {
            set_exception_code(ec_size_truncation_e);
            gg_assign(size_error, integer_one_node);
            }
          ELSE
            ENDIF
          }
        gg_increment(source_p);
        }
        WEND

      // Having skipped over the leading digits, we are in position to move the
      // remaining digits
      digit_count = dest_ldigits;
      }
    // We now have digit_count, which will cover the ldigits.  Augment it by
    // the number of rdigits:

    int dest_rdigits   = destref.field->data.rdigits;
    int source_rdigits = sourceref.field->data.rdigits;

    int trailing_zeros = 0;

    if( dest_rdigits > source_rdigits )
      {
      // The destination has more rdigits than the source

      // Copy over the available digits:
      digit_count += source_rdigits;

      // And then tack on the needed trailing zeroes:
      trailing_zeros = dest_rdigits - source_rdigits;
      }
    else if( dest_rdigits == source_rdigits )
      {
      // This is the Goldilocks zone.  Everything is *just* right.
      digit_count += dest_rdigits;
      }
    else
      {
      // The destination has fewer rdigits than the source.  We send
      // over only the necessary rdigits, discarding the ones to the right.
      digit_count += dest_rdigits;
      }
    picky_memcpy(dest_p,
                 source_p,
                 digit_count,
                 build_int_cst_type(UCHAR,
                                    charmap_dest->mapped_character(ascii_0)));
    picky_memset( dest_p,
                  charmap_dest->mapped_character(ascii_0),
                  trailing_zeros);

    // With the digits in place, the only thing left is to establish the sign

    switch( switch_dest )
      {
      case 0:
      case 1:
      case 2:
      case 3:
        // not signable, so there is nothing to do.
        break;
      case 4:
      case 6:
        //     signable, not leading, not separate
        if( charmap_dest->is_like_ebcdic() )
          {
          IF( source_sign, ne_op, integer_zero_node )
            {
            // It's negative ebcdic, so we have to turn the bit off.
            gg_assign(gg_indirect(dest_sign_loc),
                      gg_bitwise_and(gg_indirect(dest_sign_loc),
                             build_int_cst_type(UCHAR,
                                         ~NUMERIC_DISPLAY_SIGN_BIT_EBCDIC)));
            }
          ELSE
            {
            }
          ENDIF
          }
        else
          {
          IF( source_sign, ne_op, integer_zero_node )
            {
            // It's negative ascii, so we have to turn the bit on.
            gg_assign(gg_indirect(dest_sign_loc),
                      gg_bitwise_or(gg_indirect(dest_sign_loc),
                            build_int_cst_type(UCHAR,
                                         NUMERIC_DISPLAY_SIGN_BIT_ASCII)));
            }
          ELSE
            {
            }
          ENDIF
          }
        break;
      case 5:
      case 7:
        //     signable, not leading,     separate
        //     signable,     leading,     separate
        // Calculate location of the sign byte; it's the last byte of the data

        IF( source_sign, eq_op, integer_zero_node )
          {
          gg_assign(gg_indirect(dest_sign_loc),
                    build_int_cst_type(UCHAR,
                                  charmap_dest->mapped_character(ascii_plus)));
          }
        ELSE
          {
          gg_assign(gg_indirect(dest_sign_loc),
                    build_int_cst_type(UCHAR,
                                 charmap_dest->mapped_character(ascii_minus)));
          }
        ENDIF
        break;
      }
    moved = true;
    }

  clear_negative_zero(destref,
                      sourceref,
                      qualified_data_location(destref));

  return moved;
  }

static tree
cobol_wider_type_with_x_signedness (tree s_type, tree x_type)
  {
  gcc_assert (INTEGRAL_TYPE_P (s_type));
  gcc_assert (INTEGRAL_TYPE_P (x_type));

  unsigned int s_prec = TYPE_PRECISION (s_type);
  unsigned int x_prec = TYPE_PRECISION (x_type);

  unsigned int w_prec = MAX (s_prec, x_prec);
  int unsignedp = TYPE_UNSIGNED (x_type);

  return build_nonstandard_integer_type (w_prec, unsignedp);
  }

static bool
mh_binary_to_numdisp(const cbl_refer_t &destref,
                     const cbl_refer_t &sourceref,
                           cbl_round_t  rounded,
                           tree         size_error)
  {
  bool moved = false;

  charmap_t *charmap_dest =
                    __gg__get_charmap(destref.field->codeset.encoding);
  if(     destref.field->type   == FldNumericDisplay
      &&  !(destref.field->attr   & scaled_e)
      &&  !(sourceref.field->attr & scaled_e)
      &&  !(sourceref.field->attr & intermediate_e)
      &&  charmap_dest->stride() == 1
      &&  (    sourceref.field->type == FldNumericBinary
            || sourceref.field->type == FldNumericBin5
            || sourceref.field->type == FldLiteralN
            || sourceref.field->type == FldIndex
            || sourceref.field->type == FldPointer ) )
    {
    tree plus = build_int_cst_type(UCHAR,
                                   charmap_dest->is_like_ebcdic()
                                   ? ebcdic_plus : ascii_plus );
    tree minus = build_int_cst_type(UCHAR,
                                   charmap_dest->is_like_ebcdic()
                                   ? ebcdic_minus : ascii_minus );
    tree dest_location;
    get_location(dest_location, destref);

    tree s_type   = tree_type_from_refer(sourceref);
    tree d_type   = tree_type_from_refer(destref);

    // Our working type is the larger of the source and destination types.
    tree work_type = cobol_wider_type_with_x_signedness(d_type, s_type);

    tree value ;
    get_binary_value(value, sourceref, work_type);

    tree prohibited = gg_define_variable(INT, integer_zero_node);
    if( rounded == prohibited_e )
      {
      gg_assign(prohibited,
                gg_call_expr(INT,
                             "__gg__prohibited",
                             gg_get_address_of(destref.field->var_decl_node),
                             value,
                             NULL_TREE));
      }
    IF(prohibited, eq_op, integer_one_node)
      {
      set_exception_code(ec_size_truncation_e);
      if( size_error )
        {
        gg_assign(size_error, integer_one_node);
        }
      }
    ELSE
      {
      get_binary_value(value, sourceref, work_type);
      tree negative = gg_define_variable(INT);
      gg_assign(negative, integer_zero_node);

      tree sign_location = NULL_TREE;

      if( destref.field->attr & signable_e )
        {
        sign_location = gg_define_variable(UCHAR_P);
        if(    (destref.field->attr & separate_e)
            && (destref.field->attr & leading_e ) )
          {
          // separate and leading
          gg_assign(sign_location, dest_location);
          gg_increment(dest_location);
          }
        else if(    (destref.field->attr & separate_e)
                && !(destref.field->attr & leading_e ) )
          {
          // separate and trailing
          gg_assign(sign_location, gg_add(dest_location,
                                          build_int_cst_type(SIZE_T,
                                           destref.field->data.capacity()-1)));
          }
        else if(   !(destref.field->attr & separate_e)
                &&  (destref.field->attr & leading_e ) )
          {
          // internal and leading
          gg_assign(sign_location, dest_location);
          }
        else
          {
          // internal and trailing
          gg_assign(sign_location, gg_add(dest_location,
                                          build_int_cst_type(SIZE_T,
                                           destref.field->data.capacity()-1)));
          }
        }

      if(    (sourceref.field->attr & signable_e)
          && (destref.field->attr   & signable_e) )
        {
        // Both source and dest are signable, which means we have to preserve
        // the source sign and apply it, eventually, to the target.
        IF( value, lt_op, gg_cast(work_type, integer_zero_node) )
          {
          gg_assign(negative, integer_one_node);
          }
        ELSE {} ENDIF
        }

      // At this point we have to align the source and destination value rdigits.

      int source_rdigits = sourceref.field->data.rdigits;
      int dest_rdigits   = destref.field->data.rdigits;
      int nshift = source_rdigits - dest_rdigits;
      if(nshift < 0)
        {
        // We need to multiply the source by 10^(-nshift) to line them up.
        FIXED_WIDE_INT(128) power_of_ten = get_power_of_ten( -nshift );
        gg_assign(value, gg_multiply(value,
                                     wide_int_to_tree(work_type,
                                                      power_of_ten)));
        }
      else if(nshift > 0)
        {
        // We need to divide the source by 10^(nshift) to line them up.
        // This is a potential rounding situation.
        FIXED_WIDE_INT(128) power_of_ten = get_power_of_ten( nshift );
        tree pot = wide_int_to_tree(work_type, power_of_ten);
        gg_assign(negative,
                  gg_bitwise_and( negative,
                                  round_this_value(value,
                                                   pot,
                                                   rounded,
                                                   size_error)));
        }

      // At this point, value is lined up with the destination.

      // Make it positive

      if( !TYPE_UNSIGNED(work_type) )
        {
        gg_assign(value, gg_abs(value));
        }

      if( size_error )
        {
        // We need to see if is too big to fit
        FIXED_WIDE_INT(128) power_of_ten =
                                  get_power_of_ten(destref.field->data.digits);
        tree pot = wide_int_to_tree(work_type, power_of_ten);
        IF( gg_divide(value, pot),
            ne_op,
            gg_cast(work_type, integer_zero_node) )
            {
            // The value is too big; flag it:
            gg_assign(size_error, integer_one_node);
            }
          ELSE
            {
            }
          ENDIF
        }

      if( charmap_dest->is_like_ebcdic() )
        {
        gg_call(INT,
                "__gg__binary_to_string_ebcdic",
                dest_location,
                build_int_cst_type(INT, destref.field->data.digits),
                gg_cast(INT128, value),
                NULL_TREE);
        }
      else
        {
        gg_call(INT,
                "__gg__binary_to_string_ascii",
                dest_location,
                build_int_cst_type(INT, destref.field->data.digits),
                gg_cast(INT128, value),
                NULL_TREE);
        }

      if(    (sourceref.field->attr & signable_e )
          && (destref.field->attr   & signable_e ) )
        {
        IF( negative, ne_op, integer_zero_node )
          {
          if( destref.field->attr & separate_e )
            {
            // We flag the separate as negative
            gg_assign(gg_indirect(sign_location), minus);
            }
          else
            {
            if( charmap_dest->is_like_ebcdic() )
              {
              gg_assign(gg_indirect(sign_location),
                        gg_bitwise_and(gg_indirect(sign_location),
                                       build_int_cst_type(UCHAR, 0xDF)));
              }
            else
              {
              gg_assign(gg_indirect(sign_location),
                        gg_bitwise_or(gg_indirect(sign_location),
                                      build_int_cst_type(UCHAR, 0x70)));
              }
            }
          }
        ELSE
          {
          // The result is positive
          if( destref.field->attr & separate_e )
            {
            // We flag the separate as negative
            gg_assign(gg_indirect(sign_location), plus);
            }
          }
        ENDIF
        }
      else if(   (destref.field->attr & signable_e )
              && (destref.field->attr & separate_e ) )
        {
        // The source is not signed, but the destination is signable and
        // separate:
        gg_assign(gg_indirect(sign_location), plus);
        }

      moved = true;
      }
    ENDIF
    }

  return moved;
  }

static bool
mh_binary_to_packed(const cbl_refer_t &destref,
                    const cbl_refer_t &sourceref,
                          cbl_round_t  rounded,
                          tree         size_error)
  {
  bool moved = false;

  if(     destref.field->type   == FldPacked
      &&  !(destref.field->attr   & scaled_e)
      &&  !(sourceref.field->attr & scaled_e)
      &&  !(sourceref.field->attr & intermediate_e)
      &&  (    sourceref.field->type == FldNumericBinary
            || sourceref.field->type == FldNumericBin5
            || sourceref.field->type == FldLiteralN
            || sourceref.field->type == FldIndex
            || sourceref.field->type == FldPointer ) )
    {
    tree dest_location;
    get_location(dest_location, destref);

    tree s_type   = tree_type_from_refer(sourceref);
    tree d_type   = tree_type_from_refer(destref);

    // Our working type is the larger of the source and destination types.
    tree work_type = cobol_wider_type_with_x_signedness(d_type, s_type);

    tree value ;
    get_binary_value(value, sourceref, work_type);

    tree negative = gg_define_variable(INT);
    gg_assign(negative, integer_zero_node);

    if(    (sourceref.field->attr & signable_e)
        && (destref.field->attr   & signable_e) )
      {
      // Both source and dest are signable, which means we have to preserve
      // the source sign and apply it, eventually, to the target.
      IF( value, lt_op, gg_cast(work_type, integer_zero_node) )
        {
        gg_assign(negative, integer_one_node);
        }
      ELSE
        {
        gg_assign(negative, integer_zero_node);
        }
      ENDIF
      }

    // At this point we have to align the source and destination value rdigits.
    int source_rdigits = sourceref.field->data.rdigits;
    int dest_rdigits   = destref.field->data.rdigits;
    int nshift = source_rdigits - dest_rdigits;
    if(nshift < 0)
      {
      // We need to multiply the source by 10^(-nshift) to line them up.
      FIXED_WIDE_INT(128) power_of_ten = get_power_of_ten( -nshift );
      gg_assign(value, gg_multiply(value,
                                   wide_int_to_tree(work_type,
                                                    power_of_ten)));
      }
    else if(nshift > 0)
      {
      // We need to divide the source by 10^(nshift) to line them up.
      // This is a potential rounding situation.
      FIXED_WIDE_INT(128) power_of_ten = get_power_of_ten(nshift);
      tree pot = wide_int_to_tree(work_type, power_of_ten);
      gg_assign(negative,
                gg_bitwise_and( negative,
                                round_this_value(value,
                                                 pot,
                                                 rounded,
                                                 size_error)));
      }

    // At this point, value is lined up with the destination.

    // Make it positive

    if( !TYPE_UNSIGNED(work_type) )
      {
      gg_assign(value, gg_abs(value));
      }

    if( size_error )
      {
      // We need to see if is too big to fit
      FIXED_WIDE_INT(128) power_of_ten =
                                get_power_of_ten(destref.field->data.digits);
      tree pot = wide_int_to_tree(work_type, power_of_ten);
      IF( gg_divide(value, pot),
          ne_op,
          gg_cast(work_type, integer_zero_node) )
          {
          // The value is too big; flag it:
          gg_assign(size_error, integer_one_node);
          }
        ELSE
          {
          }
        ENDIF
      }

    // We are now ready to convert the binary to the packed byte string.

    int ndigits;
    if( !(destref.field->attr & packed_no_sign_e) )
      {
      // This is ordinary packed.  We need to multiply the value by ten to
      // make room for the sign nybble.
      gg_assign(value, gg_multiply(value, build_int_cst_type(work_type, 10)));
      ndigits = destref.field->data.digits+1;
      }
    else
      {
      ndigits = destref.field->data.digits;
      }

    gg_call(INT,
            "__gg__binary_to_packed",
            dest_location,
            build_int_cst_type(INT, ndigits),
            gg_cast(INT128, value),
            NULL_TREE);

    if( !(destref.field->attr & packed_no_sign_e) )
      {
      tree sign_loc = gg_add(dest_location,
                             build_int_cst_type(SIZE_T,
                                          destref.field->data.capacity()-1));
      if( !(destref.field->attr & signable_e) )
        {
        // This is an unsigned packed decimal.
        gg_assign(gg_indirect(sign_loc),
                  gg_bitwise_or(gg_indirect(sign_loc),
                                build_int_cst_type(UCHAR, 0x0F)));
        }
      else
        {
        // It is signable
        if( !(sourceref.field->attr & signable_e) )
          {
          // The source wasn't signable, so the destination has to be positive.
          gg_assign(gg_indirect(sign_loc),
                    gg_bitwise_or(gg_indirect(sign_loc),
                                  build_int_cst_type(UCHAR, 0x0C)));
          }
        else
          {
          // The source was signable, so we have to transfer the sign
          IF( negative, ne_op, integer_one_node )
            {
            // The result is non-negative
            gg_assign(gg_indirect(sign_loc),
                      gg_bitwise_or(gg_indirect(sign_loc),
                                    build_int_cst_type(UCHAR, 0x0C)));
            }
          ELSE
            {
            // The result is negative
            gg_assign(gg_indirect(sign_loc),
                      gg_bitwise_or(gg_indirect(sign_loc),
                                    build_int_cst_type(UCHAR, 0x0D)));
            }
          ENDIF
          }
        }
      }

    moved = true;
    }

  return moved;
  }

static void
copy_native_into_place(cbl_field_t *dest,
                              tree         dest_offset,
                              tree value,
                              int rhs_rdigits,
                              bool check_for_error,
                        const tree &size_error)
  {
  tree value_type = TREE_TYPE(value);
  tree dest_type = tree_type_from_field(dest);

  if( gg_sizeof(dest_type) > gg_sizeof(value_type) )
    {
    // Because the dest_type is greater than the value_type, we don't need to
    // do any size checking.
    check_for_error = false;
    }

  if( !(dest->attr & signable_e) )
    {
    gg_assign(value, gg_abs(value));
    }

  if( check_for_error && dest->data.digits)
    {
    // We need to see if value can fit into destref

    // We do this by comparing value to 10^(lhs.ldigits + rhs_rdigits)
    // Example:  rhs is 123.45, whichis 12345 with rdigits 2
    // lhs is 99.999.  So, lhs.digits is 5, and lhs.rdigits is 3.
    // 10^(5 - 3 + 2) is 10^4, which is 10000.  Because 12345 is >= 10000, the
    // source can't fit into the destination.

    tree abs_value = gg_define_variable(TREE_TYPE(value));
    gg_assign(abs_value, gg_abs(value));

    FIXED_WIDE_INT(128) power_of_ten = get_power_of_ten(  dest->data.digits
                                                        - dest->data.rdigits
                                                        + rhs_rdigits );
    IF( gg_cast(INT128, abs_value),
        ge_op,
        wide_int_to_tree(INT128, power_of_ten) )
      {
      // Flag the size error
      gg_assign(size_error, integer_one_node);
      }
    ELSE
      ENDIF
    }
  scale_by_power_of_ten_N(value, dest->data.rdigits - rhs_rdigits);

  // Create a variable of our target type.
  tree target = gg_define_variable(dest_type);
  // Cast the source to the target
  gg_assign(target, gg_cast(dest_type, value));

  if( check_for_error && !dest->data.digits )
    {
    // The destination is pure binary.  Make sure we fit:
    int nbytes = gg_sizeof(dest_type);
    // We are making sure that value is less than the power of two
    FIXED_WIDE_INT(128) power_of_two = get_power_of_two(nbytes);
    tree p_of_two = wide_int_to_tree(value_type, power_of_two);

    IF( value, ge_op, p_of_two )
      {
      // Flag the size error
      gg_assign(size_error, gg_bitwise_or(size_error, integer_one_node));
      }
    ELSE
      {
      }
    ENDIF
    }

  tree dest_pointer = gg_define_variable(UCHAR_P);
  gg_assign(dest_pointer, gg_add(member(dest->var_decl_node, "data"),
                                 dest_offset));

  if( dest->type == FldNumericBinary )
    {
    // We need the target to be big-endian.
    if( BYTES_BIG_ENDIAN )
      {
      // 'target' is already big-endian, so we can leave it be.
      }
    else
      {
      // 'target' is little-endian, so make it big-endian
      gg_assign(target, gg_bswap(target));
      }
    }
  else
    {
    // We need the target to be native binary, so just leave it be
    }
  // Copy the target to the destination.
  gg_memcpy(dest_pointer,
            gg_get_address_of(target),
            build_int_cst_type(SIZE_T, gg_sizeof(dest_type)));
  }

static bool
mh_to_binary( const cbl_refer_t &destref,
                  const cbl_refer_t &sourceref,
                  const TREEPLET    &tsource,
                        bool check_for_error,
                        tree size_error)
  {
  // This routine moves a numeric value to a binary destination.  The dest
  // can be little-endian or big-endian.

  /* In July of 2026, I attempted to create GENERIC for moving intermediates
     to binaries.  It's here, and it can be activated by commenting out the
     line
           &&  !(sourceref.field->attr  & (intermediate_e  ))
     But it runs slower than the library version.  I didn't attempt to figure
     out why that is (other things to do!) so I still have the code here.  But
     it isn't being used.  Dubner, 2026-07-22 */

  bool moved = false;

  cbl_figconst_t figconst = cbl_figconst_of( sourceref.field->data.original());

  if(     !figconst
      &&  !(destref.field->attr    & scaled_e)
      &&  !(sourceref.field->attr  & (intermediate_e  ))
      &&  sourceref.field->type     != FldGroup
      &&  sourceref.field->type     != FldLiteralA
      &&  sourceref.field->type     != FldAlphanumeric
      &&  sourceref.field->type     != FldNumericEdited
      &&  (     destref.field->type == FldNumericBin5
            ||  destref.field->type == FldNumericBinary
            ||  destref.field->type == FldPointer
            ||  destref.field->type == FldIndex ) )
    {
    Analyze();
    SHOW_PARSE1
      {
      SHOW_PARSE_INDENT
      SHOW_PARSE_TEXT("mh_to_binary")
      SHOW_PARSE_END
      }

    if( sourceref.field->type == FldFloat )
      {
      tree source = NULL_TREE;
      switch( sourceref.field->data.capacity() )
        {
        case 4:
          source = gg_define_variable(SHORT);
          break;
        case 8:
          source = gg_define_variable(LONG);
          break;
        case 16:
          source = gg_define_variable(INT128);
          break;
        }

      get_binary_value_from_float(source,
                                  destref,
                                  sourceref.field,
                                  tsource.offset);

      // Get binary value from float actually scales the source value to the
      // dest:: rdigits
      copy_native_into_place(destref.field,
                                    refer_offset(destref),
                                    source,
                                    destref.field->data.rdigits,
                                    check_for_error,
                                    size_error);
      }
    else
      {
      tree source_type = tree_type_from_refer(sourceref);
      tree source;
      get_binary_value(source, sourceref, source_type);
      copy_native_into_place( destref.field,
                              refer_offset(destref),
                              source,
                              sourceref.field->data.rdigits,
                              check_for_error,
                              size_error);
      }
    moved = true;
    }
  return moved;
  }

static bool
mh_source_is_group( const cbl_refer_t &destref,
                    const cbl_refer_t &sourceref,
                    const TREEPLET    &tsrc)
  {
  bool retval = false;
  charmap_t *charmap = __gg__get_charmap(destref.field->codeset.encoding);
  if(   sourceref.field->type == FldGroup && !(destref.field->attr & rjust_e)
     && sourceref.field->codeset.encoding == destref.field->codeset.encoding
     && charmap->stride() == 1)
    {
    Analyze();
    // We are moving a group to a something.  The rule here is just move as
    // many bytes as you can, and, if necessary, fill with spaces
    tree tdest   = gg_add( member(destref.field->var_decl_node, "data"),
                           refer_offset(destref));
    tree tsource = gg_add( member(sourceref.field->var_decl_node, "data"),
                           tsrc.offset);
    tree dbytes  = refer_size_dest(destref);
    tree sbytes  = tsrc.length;

    IF( sbytes, ge_op, gg_cast(TREE_TYPE(sbytes), dbytes) )
      {
      // There are too many source bytes
      gg_memcpy(tdest, tsource, dbytes);
      }
    ELSE
      {
      // There are too few source bytes:
      int dest_space = charmap->mapped_character(ascii_space);
      gg_memset(tdest, build_int_cst_type(INT, dest_space), dbytes);
      gg_memcpy(tdest, tsource, sbytes);
      }
    ENDIF
    retval = true;
    }
  return retval;
  }

static bool
mh_source_is_literalA(const cbl_refer_t &destref,
                      const cbl_refer_t &sourceref,
                            cbl_round_t rounded,
                            tree        size_error)
  {
  bool moved = false;
  if( sourceref.field->type == FldLiteralA )
    {
    // We are moving a literal somewhere.  Because a program-id can take
    // variables of ANY LENGTH, we don't know the length of the target
    // variable.  We do, however, know its encoding.  So, we are going to
    // construct a string with the same number of characters as the source, but
    // in the target variable's encoding.

    // We will then call a library routine that will be in charge of run-time
    // trimming or space filling, as necessary.

    cbl_encoding_t encoding_dest =   destref.field->codeset.encoding;
    charmap_t *charmap_dest = __gg__get_charmap(encoding_dest);

    static char *buffer = NULL;
    static size_t buffer_size = 0;
    size_t source_length;
    size_t dest_length;
    if( sourceref.field->attr & hex_encoded_e )
      {
      // Hex-encoded data is moved as-is
      source_length = sourceref.field->data.capacity();
      dest_length   = std::min(source_length,
                          static_cast<size_t>(destref.field->data.capacity()));
      }
    else
      {
      // Otherwise, data.initial prevails:
      size_t source_based_on_strlen = strlen(sourceref.field->data.original());
      size_t source_based_on_capacity = sourceref.field->data.capacity() /
                                        sourceref.field->codeset.stride() ;
      source_length = std::max( source_based_on_strlen ,
                                source_based_on_capacity );
      dest_length   = source_length * charmap_dest->stride();
      }

    if( buffer_size < dest_length )
      {
      buffer_size = dest_length;
      buffer = static_cast<char *>(xrealloc(buffer, buffer_size));
      }
    gcc_assert(buffer);

    cbl_figconst_t figconst = cbl_figconst_of( sourceref.field->data.original());
    size_t outlength;
    if( figconst )
      {
      // We are going to fill 'buffer' with a solid run of the figurative
      // constant in the destination codeset.
      char const_char = 0x7F;  // Head off a compiler warning about
      //                       // uninitialized variables
      switch(figconst)
        {
        case normal_value_e :
          // This is not possible, it says here in the fine print.
          gcc_unreachable();
          break;
        case low_value_e    :
          const_char = charmap_dest->low_value_character();
          break;
        case zero_value_e   :
          const_char = charmap_dest->mapped_character(ascii_zero);
          break;
        case space_value_e  :
          const_char = charmap_dest->mapped_character(ascii_space);
          break;
        case quote_value_e  :
          const_char = charmap_dest->quote_character();
          break;
        case high_value_e   :
          const_char = charmap_dest->high_value_character();
          break;
        case null_value_e:
          const_char = 0x00;
          break;
        }
      memset(buffer, const_char, source_length);
      }
     else
      {
      if( sourceref.field->attr & hex_encoded_e )
        {
        // hex_encoded data goes as is:
        memcpy(buffer, sourceref.field->data.original(), dest_length);
        outlength = dest_length;
        }
      else
        {
        // We are going to convert the source string to the destination
        // codeset, and then copy it to 'buffer', trimming if necessary, and
        // space-filling to the right if necessary:
        const char *source_string =
        __gg__iconverter(
                       sourceref.field->codeset.default_encodings.source->type,
                       encoding_dest,
                       sourceref.field->data.original(),
                       source_length,
                       &outlength );
        if( outlength > dest_length )
          {
          outlength = dest_length;
          }
        // Copy over the converted string
        memcpy( buffer,
                source_string,
                outlength );
        }
      }

    // Check to see if we can do a simple alphanumeric-to-alphanumeric move
    if( (   destref.field->type == FldAlphanumeric
         || destref.field->type == FldGroup )
       && !(destref.field->attr & any_length_e)
       && !sourceref.all
       && !size_error)
      {
      // A simple alpha-to-alpha move is possible
      size_t dest_bytes = destref.field->data.capacity();
      // We have 'outlength' bytes in 'buffer' that need to go to
      // destref.field->data.capacity() bytes at destref.field->data.
      char *src = static_cast<char *>(xmalloc(dest_bytes));
      size_t src_bytes = std::min(outlength, dest_bytes);
      charmap_t *charmap = __gg__get_charmap(destref.field->codeset.encoding);
      charmap->memset(src, charmap->mapped_character(ascii_space), dest_bytes);

      if( destref.field->attr & rjust_e )
        {
        size_t fill = 0;
        if( src_bytes < dest_bytes )
          {
          fill = dest_bytes - src_bytes;
          }
        memcpy(src+fill, buffer+outlength-src_bytes, src_bytes);
        }
      else
        {
        memcpy(src, buffer, src_bytes);
        }
      // src is now the desired string, space-filled if necessary on the right,
      // (or on the left, for rjust_e destinations).

      if( refer_is_clean(destref) )
        {
        gg_memcpy(member(destref.field->var_decl_node, "data"),
                  build_string_literal(dest_bytes, src),
                  build_int_cst_type(SIZE_T, dest_bytes));
        }
      else
        {
        // The refer has some information in it.
        gg_memcpy(gg_add(member(destref.field->var_decl_node, "data"),
                         refer_offset(destref)),
                  build_string_literal(dest_bytes, src),
                  refer_size_dest(destref));
        }
      free(src);
      }
    else
      {
      // This is more complicated than a simple alpha-to-alpha move
      if(    destref.refmod.from
          || destref.refmod.len )
        {
        // Let the move routine know to treat the destination as alphanumeric
        attribute_bit_set(destref.field, refmod_e);
        }
      // If the source is flagged ALL, or if we are setting the destination to
      // a figurative constant, pass along the ALL bit:
      int rounded_parameter = rounded
                             | ((sourceref.all || figconst ) ? REFER_ALL_BIT : 0);

      if( size_error )
        {
        gg_assign(size_error,
                  gg_call_expr( INT,
                                "__gg__move_literala",
                                gg_get_address_of(destref.field->var_decl_node),
                                refer_offset(destref),
                                refer_size_dest(destref),
                                build_int_cst_type(INT, rounded_parameter),
                                build_string_literal(outlength,
                                                     buffer),
                                build_int_cst_type( SIZE_T, outlength),
                                NULL_TREE));
        }
      else
        {
                  gg_call     ( INT,
                                "__gg__move_literala",
                                gg_get_address_of(destref.field->var_decl_node),
                                refer_offset(destref),
                                refer_size_dest(destref),
                                build_int_cst_type(INT, rounded_parameter),
                                build_string_literal(outlength,
                                                     buffer),
                                build_int_cst_type( SIZE_T, outlength),
                                NULL_TREE);
        }
      if(    destref.refmod.from
          || destref.refmod.len )
        {
        // Return that value to its original form
        attribute_bit_clear(destref.field, refmod_e);
        }
      }

    moved = true;
    }
  return moved;
  }

static bool
have_common_parent(const cbl_refer_t &destref,
                   const cbl_refer_t &sourceref)
  {
  /* We are trying to lay down fast code when possible.  But sometimes we have
     to go slower in order to be accurate. The COBOL specification explicitly
     says that when the storage areas of sending and receiving operands
     overlap:
      1) When the data items are not described by the same data description
         entry, the result of the statement is undefined.
      2)  When the data items are described by the same data description entry,
          the result of the statement is the same as if the data items shared
          no part of their respective storage areas.

     There is an additional paragraph:
      In the case of reference modification, the unique data item produced by
      reference modification is not considered to be the same data description
      entry as any other data description entry. Therefore, if an overlapping
      situation exists, the results of the operation are undefined.

      This routine will return TRUE when neither reference is a refmod, and
      both operands ultimately have the same parent (indicating that they are
      part of the same data description.

      The point is that when we return True, then the two are not refmods, and
      they have a common parent, so we have to use a memmove.  When we return
      False, then we can use a faster memcpy.
      */
  bool retval = true;
  if( destref.is_refmod_reference() )
    {
    retval = false;
    }
  else if( sourceref.is_refmod_reference() )
    {
    retval = false;
    }
  else
    {
    // Neither is a refmod.  Check for common parentage:
    const cbl_field_t *poppa = destref.field;
    const cbl_field_t *momma = sourceref.field;
    while( parent_of(poppa) )
      {
      // Follow the first family_tree up as far as we can.
      poppa = parent_of(poppa);
      }
    while( parent_of(momma) )
      {
      // Follow the second family_tree up as far as we can.
      momma = parent_of(momma);
      }
    if( poppa != momma )
      {
      /* Okay, so the analogy breaks down.  Think of momma and poppa as
         bacteria, or something.  */
      retval = false;
      }
    }

  return retval;
  }

static bool
mh_alpha_to_alpha(const cbl_refer_t &destref,
                  const cbl_refer_t &sourceref,
                        cbl_round_t /*rounded*/,
                        tree        size_error)
  {
  bool moved = false;
  // If a bunch of conditions are met, we can do a move without resorting to
  // the library.
  if(   sourceref.field->type == FldAlphanumeric
     && destref.field->type   == FldAlphanumeric
     && !size_error
     && sourceref.field->codeset.encoding == destref.field->codeset.encoding
     && !(destref.field->attr   & rjust_e)
     && !(sourceref.field->attr & any_length_e)
     && !(destref.field->attr   & any_length_e)
     && !(sourceref.field->attr & intermediate_e)
     && !sourceref.all
     )
    {
    void (*mover)(tree, tree, tree); // dest, source, count
    mover = have_common_parent(destref, sourceref) ? gg_memmove : gg_memcpy;

    // We are in a position to simply move bytes from the source to the dest.
    if( refer_is_clean(sourceref) && refer_is_clean(destref) )
      {
      // Source and destination are both clean
      if( destref.field->data.capacity() <= sourceref.field->data.capacity() )
        {
        // This is the simplest case of all
        mover(member(  destref.field->var_decl_node, "data"),
                  member(sourceref.field->var_decl_node, "data"),
                  build_int_cst_type(SIZE_T, destref.field->data.capacity()));
        moved = true;
        }
      else
        {
        // This is a tad more complicated.  The source is too short, so we need
        // to copy over what we can...
        mover(member(  destref.field->var_decl_node, "data"),
                 member(sourceref.field->var_decl_node, "data"),
                 build_int_cst_type(SIZE_T, sourceref.field->data.capacity()));
        // And then space-fill the rest:
        size_t fill_bytes =
            destref.field->data.capacity() - sourceref.field->data.capacity();

        // ...and then create a memory area with the fill spaces...
        char *spaces = static_cast<char *>(xmalloc(fill_bytes));
        charmap_t *charmap =__gg__get_charmap(destref.field->codeset.encoding);
        charmap->memset(spaces,
                        charmap->mapped_character(ascii_space),
                        fill_bytes);
        // ...and then copy those spaces into place.
        mover(
          gg_add(member(destref.field->var_decl_node, "data"),
                 build_int_cst_type(SIZE_T, sourceref.field->data.capacity())),
          build_string_literal(fill_bytes, spaces),
          build_int_cst_type(SIZE_T, fill_bytes));
        free(spaces);
        moved = true;
        }
      }

    if( !refer_is_clean(sourceref) && refer_is_clean(destref) )
      {
      // The source is dirty, but the destination is clean:
      tree source_data;
      tree source_len;

      tree dest_data;
      tree dest_len;

      source_data = gg_add(member(sourceref.field->var_decl_node, "data"),
                           refer_offset(sourceref));
      source_len = refer_size_source(sourceref);

      dest_data = member(destref.field->var_decl_node, "data");

      dest_len = build_int_cst_type(SIZE_T, destref.field->data.capacity());
      IF( source_len, ge_op, dest_len )
        {
        // The source has enough (or more) bytes to fill the destination:
        mover(dest_data, source_data, dest_len);
        }
      ELSE
        {
        // The source data is too short.  We need to copy over what we have...
        mover(dest_data, source_data, source_len);

        // And then right-fill the remainder with spaces. Create a buffer with
        // more than enough spaces for our purposes:
        size_t fill_bytes = destref.field->data.capacity();
        char *spaces = static_cast<char *>(xmalloc(fill_bytes));
        charmap_t *charmap =__gg__get_charmap(destref.field->codeset.encoding);
        charmap->memset(spaces,
                        charmap->mapped_character(ascii_space),
                        fill_bytes);
        // And then copy enough of those spaces into place.
        mover(gg_add(dest_data, source_len),
                  build_string_literal(fill_bytes, spaces),
                  gg_subtract(dest_len, source_len));
        free(spaces);
        }
      ENDIF
      moved = true;
      }
    if( refer_is_clean(sourceref) && !refer_is_clean(destref) )
      {
      // The source is clean but the destination is dirty:
      tree source_data;
      tree source_len;

      tree dest_data;
      tree dest_len ;

      source_data = member(sourceref.field->var_decl_node, "data");
      source_len  = build_int_cst_type(SIZE_T,
                                       sourceref.field->data.capacity());
      dest_data = gg_add(member(destref.field->var_decl_node, "data"),
                         refer_offset(destref));
      dest_len = refer_size_dest(destref);
      IF( source_len, ge_op, dest_len )
        {
        // The source has enough (or more) bytes to fill the destination:
        mover(dest_data, source_data, dest_len);
        }
      ELSE
        {
        // The source data is too short.  We need to copy over what we have...
        mover(dest_data, source_data, source_len);

        // And then right-fill the remainder with spaces. Create a buffer with
        // more than enough spaces for our purposes:
        size_t fill_bytes = destref.field->data.capacity();
        char *spaces = static_cast<char *>(xmalloc(fill_bytes));
        charmap_t *charmap =__gg__get_charmap(destref.field->codeset.encoding);
        charmap->memset(spaces,
                        charmap->mapped_character(ascii_space),
                        fill_bytes);
        // And then copy enough of those spaces into place.
        mover(gg_add(dest_data, source_len),
                  build_string_literal(fill_bytes, spaces),
                  gg_subtract(dest_len, source_len));
        free(spaces);
        }
      ENDIF

      moved = true;
      }
    if( !refer_is_clean(sourceref) && !refer_is_clean(destref) )
      {
      // Both the source and the dest are "dirty"
      tree source_data = gg_define_variable(UCHAR_P);
      tree source_len  = gg_define_variable(SIZE_T);

      tree dest_data = gg_define_variable(UCHAR_P);
      tree dest_len  = gg_define_variable(SIZE_T);

      gg_assign(source_data,
                gg_add(member(sourceref.field->var_decl_node, "data"),
                       refer_offset(sourceref)));
      gg_assign(source_len, refer_size_source(sourceref));

      gg_assign(dest_data,
                gg_add(member(destref.field->var_decl_node, "data"),
                       refer_offset(destref)));
      gg_assign(dest_len, refer_size_dest(destref));
      IF( source_len, ge_op, dest_len )
        {
        // The source has enough (or more) bytes to fill the destination:
        mover(dest_data, source_data, dest_len);
        }
      ELSE
        {
        // The source data is too short.  We need to copy over what we have...
        mover(dest_data, source_data, source_len);

        // And then right-fill the remainder with spaces. Create a buffer with
        // more than enough spaces for our purposes:
        size_t fill_bytes = destref.field->data.capacity();
        char *spaces = static_cast<char *>(xmalloc(fill_bytes));
        charmap_t *charmap =__gg__get_charmap(destref.field->codeset.encoding);
        charmap->memset(spaces,
                        charmap->mapped_character(ascii_space),
                        fill_bytes);
        // And then copy enough of those spaces into place.
        mover(gg_add(dest_data, source_len),
                  build_string_literal(fill_bytes, spaces),
                  gg_subtract(dest_len, source_len));
        free(spaces);
        }
      ENDIF

      moved = true;
      }
    }
  return moved;
  }

static bool
mh_numdisp_to_packed(const cbl_refer_t &destref,
                     const cbl_refer_t &sourceref,
                           tree         size_error,
                           bool         check_for_error)
  {
  const charmap_t *charmap =
                   __gg__get_charmap(sourceref.field->codeset.encoding);
  if(    (destref.field->type   != FldPacked         )
      || (sourceref.field->type != FldNumericDisplay )
      || (charmap->stride()     != 1                 )
      || (destref.field->attr    & scaled_e          )
      || (sourceref.field->attr  & scaled_e          )
      || (destref.field->attr    & packed_no_sign_e  )
      || (sourceref.field->attr  & leading_e         )
      || (sourceref.field->attr  & separate_e        ) )
    {
    return false;
    }
  /* Source is NumericDisplay, dest is packed, neither are scaled, the
     packed destination has a sign nybble, and the numeric source has an
     ordinarysign bit encoded in the final digit.  */
  tree uzero = build_int_cst_type(UCHAR,    0);
  tree umask = build_int_cst_type(UCHAR, 0x0F);
  tree ufour = build_int_cst_type(SIZE_T,   4);

  tree source_location;
  tree dest_location;
  tree dest_p          = gg_define_variable(UCHAR_P);
  tree source_p        = gg_define_variable(UCHAR_P);

  get_location(dest_location, destref);
  gg_assign(dest_p, dest_location);
  get_location(source_location, sourceref);

  int source_digits   = sourceref.field->data.digits;
  int source_rdigits  = sourceref.field->data.rdigits;
  int source_ldigits  = source_digits - source_rdigits;
  int dest_digits     = destref.field->data.digits;
  int dest_rdigits    = destref.field->data.rdigits;
  int dest_ldigits    = dest_digits - dest_rdigits;

  int truncate_ldigits = std::max(0, source_ldigits-dest_ldigits);
  int truncate_rdigits = std::max(0, source_rdigits-dest_rdigits);
  int leading_zeroes   = std::max(0, dest_ldigits-source_ldigits);
  int trailing_zeroes  = std::max(0, dest_rdigits-source_rdigits);

  int zero_pairs;
  int digit_pairs;
  int source_remaining;

  if( truncate_ldigits )
    {
    // We handle truncation of digits on the left by moving the starting line.
    if( check_for_error )
      {
      // We need to flag as a truncation error any truncated places that are
      // not zero.
      gg_assign(source_p, source_location);
      tree trunc_end = gg_define_variable(UCHAR_P);
      gg_assign(trunc_end,
                gg_add(source_p,
                       build_int_cst_type(SIZE_T, truncate_ldigits)));
      WHILE(source_p, lt_op, trunc_end)
        {
        gg_assign(size_error,
                  gg_bitwise_or(size_error,
                                gg_indirect(source_p)));
        gg_increment(source_p);
        }
      WEND
      // We care about only the bottom four bits.
      gg_assign(size_error,
                gg_bitwise_and(size_error, gg_cast(INT, umask)));
      }
    else
      {
      gg_assign(source_p,
                gg_add(source_location,
                       build_int_cst_type(SIZE_T, truncate_ldigits)));
      }
    source_digits  -= truncate_ldigits;
    source_ldigits -= truncate_ldigits;
    }
  else
    {
    gg_assign(source_p, source_location);
    }

  if( truncate_rdigits )
    {
    // We handle truncation of digits on the right by moving the finish line.
    source_digits  -= truncate_rdigits;
    source_ldigits -= truncate_rdigits;
    }

  if( !source_digits )
    {
    // When source_digits is zero, it means that some pervert of a COBOL
    // programmer told us to MOVE 999V TO V999.  The result has to be zero,
    // and our life down below will be easier when we know that there is at
    // least one digit that needs to be moved from the source to the
    // destination.
    gg_memset(dest_p,
              integer_zero_node,
              build_int_cst_type(SIZE_T, destref.field->data.capacity()));
    goto adjust_sign;
    }

  source_remaining = source_digits;

  // The first thing we need to do is adjust the first byte of the destination
  // so that we know where we are in left-nybble/right-nybble space.  Let's
  // call the digit at source_p "N".  (That digit might be a leading zero.)
  // When dest_digits is an even number, it means the final result is something
  // like 0N.23.4s.  So, when dest_digits is even, we have to start things off
  // with "0N".

  if( !(dest_digits & 0x01) )
    {
    // dest_digits is an even number.
    if( leading_zeroes )
      {
      // The first byte is "0N", but N is zero:
      gg_assign(gg_indirect(dest_p), uzero);
      leading_zeroes -= 1;
      }
    else
      {
      // The first byte is "0N", where N is the value from the first character
      // of the source.  We know that source_remaining is at least one at this
      // point.
      gg_assign(gg_indirect(dest_p),
                gg_bitwise_and(gg_indirect(source_p), umask));
      gg_increment(source_p);
      source_remaining -= 1;
      }
    gg_increment(dest_p);
    }

  // At this point, we know that leading + source + trailing is an odd
  // number.

  // We know that dest_p is set up to accept a left/right pair next.  Let's
  // see if we have enough leading_zeroes to warrant using memset:
  zero_pairs = leading_zeroes/2;
  if( zero_pairs )
    {
    // We can use memset to handle left-side zero-fill:
    tree tpairs = build_int_cst_type(SIZE_T, zero_pairs);
    gg_memset(dest_p, integer_zero_node, tpairs);
    gg_assign(dest_p, gg_add(dest_p, tpairs));
    leading_zeroes -= 2 * zero_pairs;
    }

  // dest-p is still set up for a left/right pair.
  if( leading_zeroes )
    {
    // But we still have one leading zero left.  We know at this point that
    // there is at least one source digit left, so build the byte using
    // zero/*source_p
    gg_assign(gg_indirect(dest_p),
              gg_bitwise_and(gg_indirect(source_p), umask));
    //leading_zeroes   -= 1;
    source_remaining -= 1;
    gg_increment(source_p);
    gg_increment(dest_p);
    }

  // At this point, we know that leading_zeroes is zero.  We know that
  // source_remaining + trailing_zeroes is an odd number.  We
  // currently have dest_p lined up on a left-right boundary.

  // We are going to transfer as many pairs of source_remaining digits as we
  // can.

  digit_pairs = source_remaining/2;
  if( digit_pairs )
    {
    tree dest_end = gg_define_variable(UCHAR_P);
    gg_assign(dest_end,
              gg_add(dest_p,
                     build_int_cst_type(SIZE_T, digit_pairs)));
    WHILE( dest_p, lt_op, dest_end )
      {
      tree left_nybble  = gg_lshift(gg_indirect(source_p), ufour);
      tree right_nybble = gg_bitwise_and(gg_indirect(source_p,
                                                     integer_one_node),
                                         umask);
      gg_assign(gg_indirect(dest_p),
                gg_bitwise_or(left_nybble, right_nybble));
      gg_increment(dest_p);
      gg_assign(source_p,
                gg_add(source_p, build_int_cst_type(SIZE_T, 2)));
      }
    WEND
    source_remaining -= 2 * digit_pairs;
    }

  // At this point, source_remaining is zero or one

  if( source_remaining )
    {
    gg_assign(gg_indirect(dest_p),
              gg_lshift(gg_indirect(source_p), ufour));
    gg_increment(dest_p);
    //source_remaining -= 1;
    if( trailing_zeroes )
      {
      trailing_zeroes -= 1;
      }
    }
  // At this point, we know trailing_zeroes has to be an even number, and we
  // need to zero out that many nybbles:

  if( trailing_zeroes >= 2 )
    {
    zero_pairs = trailing_zeroes/2;
    // We can use memset to handle left-side zero-fill:
    tree tpairs = build_int_cst_type(SIZE_T, zero_pairs);
    gg_memset(dest_p, integer_zero_node, tpairs);
    gg_assign(dest_p, gg_add(dest_p, tpairs));
    trailing_zeroes -= 2 * zero_pairs;
    }

  if( trailing_zeroes )
    {
    // There is one trailing zero left
    gg_assign(gg_indirect(dest_p), uzero);
    gg_increment(dest_p);
    //trailing_zeroes -= 1;
    }

  adjust_sign:
  gg_assign(dest_p, gg_add(dest_location,
                           build_int_cst_type(SIZE_T,
                                           destref.field->data.capacity()-1)));

  if( !(destref.field->attr & signable_e) )
    {
    // The destination is not signable
    gg_assign(gg_indirect(dest_p),
              gg_bitwise_or(gg_indirect(dest_p), umask));
    }
  else
    {
    if( sourceref.field->attr & signable_e )
      {
    // This is the location of the character with the sign flag.
      gg_assign(source_p, gg_add(source_location,
                               build_int_cst_type(SIZE_T,
                                         sourceref.field->data.capacity()-1)));
      if( charmap->is_like_ebcdic() )
        {
        // EBCDIC digits are 0xF0 through 0xF9; negative is flagged by
        // 0xD0 through 0xD9
        IF( gg_indirect(source_p), lt_op, build_int_cst_type(UCHAR, 0xF0) )
          {
          gg_assign(gg_indirect(dest_p),
                    gg_bitwise_or(gg_indirect(dest_p),
                                  build_int_cst_type(UCHAR, 0x0D)));
          }
        ELSE
          {
          gg_assign(gg_indirect(dest_p),
                    gg_bitwise_or(gg_indirect(dest_p),
                                  build_int_cst_type(UCHAR, 0x0C)));
          }
        ENDIF
        }
      else
        {
        // EBCDIC digits are 0x30 through 0x39; negative is flagged by
        // 0x70 through 0x79
        IF( gg_indirect(source_p), ge_op, build_int_cst_type(UCHAR, 0x70) )
          {
          gg_assign(gg_indirect(dest_p),
                    gg_bitwise_or(gg_indirect(dest_p),
                                  build_int_cst_type(UCHAR, 0x0D)));
          }
        ELSE
          {
          gg_assign(gg_indirect(dest_p),
                    gg_bitwise_or(gg_indirect(dest_p),
                                  build_int_cst_type(UCHAR, 0x0C)));
          }
        ENDIF
        }
      }
    else
      {
      gg_assign(gg_indirect(dest_p),
                gg_bitwise_or(gg_indirect(dest_p),
                              build_int_cst_type(UCHAR, 0x0C)));
      }
    }
  clear_negative_zero(destref,
                      sourceref,
                      dest_location);
  return true;
  }

static bool
mh_packed_to_packed(const cbl_refer_t &destref,
                    const cbl_refer_t &sourceref,
                    tree               size_error,
                    bool               check_for_error)
  {
  if(    (destref.field->type   != FldPacked        )
      || (sourceref.field->type != FldPacked        )
      || (destref.field->attr    & scaled_e         )
      || (sourceref.field->attr  & scaled_e         )
      || (destref.field->attr    & packed_no_sign_e )
      || (sourceref.field->attr  & packed_no_sign_e ) )
    {
    return false;
    }
  // Arriving here means both are packed, neither is scaled, and neither is
  // COMP-6 or PACKED NO SIGN.

  // We are going to move source to the dest doing the absolute minimum number
  // of operations.  We are thus going to use memcpy (with constant lengths)
  // as much as we can, and use conditionals and nybble operations as little
  // little as possible.

  // There are two broad cases.  The more straightforward case is where source
  // rdigits and dest rdigits are both even, or both odd.  When that is the
  // case, the source and destination decimal places are "in phase" somewhere
  // inside both the dest and the source.  Once we figure out the right
  // offsets, we can memcpy the "inside" of the source to the correct location
  // in the dest.  We fiddle with the leading digits, the trailing digits, and
  // the sign nybble as necessary.

  tree source_location;
  tree dest_location;
  tree source_sign     = gg_define_variable(UCHAR_P);
  tree dest_sign       = gg_define_variable(UCHAR_P);

  get_location(dest_location, destref);
  get_location(source_location, sourceref);

  if( check_for_error )
    {
    int source_digits  = sourceref.field->data.digits;
    if( !(source_digits & 1) )
      {
      // When source_digits is an even number, then the leftmost byte is
      // 0x0n.
      source_digits += 1;
      }
    int source_ldigits =   source_digits
                         - sourceref.field->data.rdigits;
    int dest_ldigits   =   destref.field->data.digits
                         - destref.field->data.rdigits;
    int truncate_ldigits = std::max(0, source_ldigits - dest_ldigits);
    if( truncate_ldigits )
      {
      tree truncate_p = gg_define_variable(UCHAR_P);
      gg_assign(truncate_p, source_location);
      int truncate_pairs = truncate_ldigits / 2;
      if( truncate_pairs )
        {
        tree truncate_e = gg_define_variable(UCHAR_P);
        gg_assign(truncate_e,
                  gg_add(truncate_p,
                         build_int_cst_type(SIZE_T, truncate_pairs)));
        WHILE( truncate_p, lt_op, truncate_e )
          {
          gg_assign(size_error,
                    gg_bitwise_or(size_error,
                                  gg_cast(INT, gg_indirect(truncate_p))));
          gg_increment(truncate_p);
          }
        WEND
        truncate_ldigits &= 1;
        }
      if( truncate_ldigits )
        {
        gg_assign(size_error,
                  gg_bitwise_or(size_error,
                        gg_cast(INT,
                                gg_bitwise_and(gg_indirect(truncate_p),
                                               build_int_cst_type(UCHAR,
                                                                  0xF0)))));
        }
      }
    }
  int      source_digits   = sourceref.field->data.digits;
  int      source_rdigits  = sourceref.field->data.rdigits;
  size_t   source_capacity = source_digits/2 + 1;
  if( ((destref.field->data.rdigits ^ source_rdigits) & 1) )
    {
    /* This is an "out-of-phase" move, e.g., MOVE 999v99 to 999v9.  The code
       below handles in-phase moves, so we handle this by making a left-shifted
       copy of the source side.  By left-shifting it one nybble, incrementing
       the source_rdigits, and changing the location to the shifted version, we
       turn the out-of-phase problem into an in-phase problem.  */
    size_t shifted_size;
    if( source_digits & 1 )
      {
      // The source, plus the sign nybble, fills an even number of nybbles, and
      // so the shift requires an addition byte on the left.
      shifted_size = source_capacity + 1;
      }
    else
      {
      // The highest-order source nybble is a zero, so the shift will fill it
      // without any additional storage needed.
      shifted_size = source_capacity;
      }
    // Allocate storage for the shifted version:
    tree shifted_type = build_array_type_nelts(UCHAR, shifted_size);
    tree shifted = gg_define_variable(shifted_type);
    TREE_ADDRESSABLE(shifted) = 1;
    tree source_p        = gg_define_variable(UCHAR_P);
    tree shifted_p_left  = gg_define_variable(UCHAR_P);
    tree shifted_p_right = gg_define_variable(UCHAR_P);
    tree carry      = gg_define_variable(UCHAR);
    tree carry_next = gg_define_variable(UCHAR);
    gg_assign(source_p,
              gg_add(source_location,
                     build_int_cst_type(SIZE_T,
                                        source_capacity-1)));
    gg_assign(shifted_p_left, gg_pointer_to_array(shifted));
    gg_assign(shifted_p_right,
              gg_add(shifted_p_left,
                     build_int_cst_type(SIZE_T, shifted_size-1)));
    // Start with the right side.
    // Pick up the carry, which is the left side of the rightmost byte
    gg_assign(carry,
              gg_rshift(gg_indirect(source_p),
                        build_int_cst_type(SIZE_T, 4)));
    // Keep the sign nybble in place, but with a zero to its left
    gg_assign(gg_indirect(shifted_p_right),
              gg_bitwise_and(gg_indirect(source_p),
                             build_int_cst_type(UCHAR, 0x0F)));

    gg_decrement(source_p);
    gg_decrement(shifted_p_right);
    WHILE(shifted_p_right, gt_op, shifted_p_left)
      {
      gg_assign(carry_next,
                gg_rshift(gg_indirect(source_p),
                          build_int_cst_type(SIZE_T, 4)));
      gg_assign(gg_indirect(shifted_p_right),
                gg_bitwise_or(gg_lshift(gg_indirect(source_p),
                                        build_int_cst_type(SIZE_T, 4)),
                              carry));
      gg_assign(carry, carry_next);
      gg_decrement(source_p);
      gg_decrement(shifted_p_right);
      }
    WEND
    // At this point, shifted_p_right equals shifted_p_left
    if( source_digits & 1 )
      {
      // The source, plus the sign nybble, fills an even number of nybbles, and
      // so the shift requires an addition byte on the left.
      gg_assign(gg_indirect(shifted_p_left), carry);
      }
    else
      {
      // The highest-order source nybble is a zero, so the shift will fill it
      // without any additional storage needed.
      gg_assign(gg_indirect(shifted_p_left),
                gg_bitwise_or(gg_lshift(gg_indirect(source_p),
                                        build_int_cst_type(SIZE_T, 4)),
                              carry));
      }

    // We now have the left-shifted source in 'shifted'.
    source_digits  += 1;
    source_rdigits += 1;
    source_capacity = source_digits/2 + 1;
    gg_assign(source_location, shifted_p_left);
    }
  gg_assign(source_sign,
            gg_add(source_location,
                   build_int_cst_type(SIZE_T,
                                      source_capacity-1)));
  gg_assign(dest_sign,
            gg_add(dest_location,
                   build_int_cst_type(SIZE_T,
                                      destref.field->data.capacity()-1)));

  // This is the straightforward case, where the dest and source decimal
  // places are in phase, which means that the middles of the values can
  // simply be moved.
  int dest_rbytes  = destref.field->data.rdigits/2 + 1;
  int dest_lbytes  = destref.field->data.capacity() - dest_rbytes;

  int source_rbytes = source_rdigits/2 + 1;
  int source_lbytes = source_capacity - source_rbytes;

  uint32_t dest_bytes = destref.field->data.capacity();

  if( source_lbytes > dest_lbytes )
    {
    // There are too many source lbytes.   We just skip those extra bytes,
    // truncating off those high-order digits.
    gg_assign(source_location,
              gg_add(source_location,
                     build_int_cst_type(SIZE_T,
                                        source_lbytes-dest_lbytes)));
    }
  else if( source_lbytes < dest_lbytes )
    {
    // There are too few source lbytes.  We zero out the extra bytes on the
    // left side of the destination.
    gg_memset(dest_location,
              integer_zero_node,
              build_int_cst_type(SIZE_T, dest_lbytes-source_lbytes));
    gg_assign(dest_location,
              gg_add(dest_location,
                     build_int_cst_type(SIZE_T, dest_lbytes-source_lbytes)));
    // And reduce the total number to move by the number we zeroed:
    dest_bytes -= dest_lbytes-source_lbytes;
    }
  source_lbytes = dest_lbytes;
  // We have lined up source_location and dest_location so that the source
  // lbytes will go into the correct location in the destination

  // We copy over as many bytes as we have in the source that can fit into
  // the destination:
  size_t bytes_to_copy =
            std::min(static_cast<uint32_t>(source_lbytes) + source_rbytes,
                                           dest_bytes);
  gg_memcpy(dest_location,
            source_location,
            build_int_cst_type(SIZE_T, bytes_to_copy));

  // We make sure the final sign nybble is correct.

  if( source_rbytes == dest_rbytes )
    {
    // The sign nybble from the source is now in the destination.  It might
    // need to be changed
    if(    !(sourceref.field->attr  & signable_e)
        &&  (destref.field->attr    & signable_e) )
      {
      // The unsignable source has an 0xF sign nybble, so we need to turn
      // that into an positive 0xC in the signable destination:
      gg_assign(gg_indirect(dest_sign),
                gg_bitwise_and(gg_indirect(dest_sign),
                               build_int_cst_type(UCHAR, 0xFC)));
      }
    else if(    (sourceref.field->attr  & signable_e)
            && !(destref.field->attr & signable_e) )
      {
      // The signable source has an 0xC or 0xD sign nybble, so we need to
      // turn that into an 0xF in the unsignable destination:
      gg_assign(gg_indirect(dest_sign),
                gg_bitwise_or(gg_indirect(dest_sign),
                              build_int_cst_type(UCHAR, 0x0F)));
      }
    }
  else
    {
    // There is mismatch between source and dest rdigits:
    if( source_rbytes < dest_rbytes )
      {
      // The source was too short to fill the destination, which means we
      // currently have a source's sign nybble sitting in the middle of the
      // destination.  We need to zero out that nybble
      gg_assign(gg_indirect(dest_location,
                            build_int_cst_type(SIZE_T,
                                               bytes_to_copy-1)),
                gg_bitwise_and(gg_indirect(dest_location,
                               build_int_cst_type(SIZE_T,
                                                  bytes_to_copy-1)),
                                           build_int_cst_type(UCHAR, 0xF0)));
      // And then we need to zero out the remaining dest_rbytes:
      int remaining_rbytes = dest_rbytes - source_rbytes;
      if( remaining_rbytes > 1 )
        {
        gg_memset(gg_add(dest_location,
                         build_int_cst_type(SIZE_T, bytes_to_copy)),
                  integer_zero_node,
                  build_int_cst_type(SIZE_T,
                           destref.field->data.capacity() - bytes_to_copy));
        }
      // And now we have to adjust the final nybble:

      if(    !(sourceref.field->attr  & signable_e)
          &&  (destref.field->attr & signable_e) )
        {
        // The source is unsignable, so we turn that into an positive 0xC in
        // the signable destination:
        gg_assign(gg_indirect(dest_sign), build_int_cst_type(UCHAR, 0x0C));
        }
      else if(    (sourceref.field->attr  & signable_e)
              && !(destref.field->attr & signable_e) )
        {
        gg_assign(gg_indirect(dest_sign), build_int_cst_type(UCHAR, 0x0F));
        }
      else
        {
        // The source and the destination are either both signable, or
        // both unsignable.  We copy the source's sign nybble to the dest.
        gg_assign(gg_indirect(dest_sign),
                  gg_bitwise_or(gg_indirect(dest_sign),
                                gg_bitwise_and(gg_indirect(source_sign),
                                               build_int_cst_type(UCHAR,
                                                                  0x0F))));
        }
      }
    else // source_rbytes > dest_rbytes
      {
      // There were more source_rbytes than we needed, which means the final
      // nybble of the destination is a digit that needs to be truncated
      // away and replaced with the correct sign nybble.
      if(    !(sourceref.field->attr  & signable_e)
          &&  (destref.field->attr & signable_e) )
        {
        // The source was unsignable, so we set the sign nybble to a
        // a positive 0x0C
        gg_assign(gg_indirect(dest_sign),
                  gg_bitwise_or(gg_bitwise_and(gg_indirect(dest_sign),
                                     build_int_cst_type(UCHAR, 0xF0)),
                                     build_int_cst_type(UCHAR, 0x0C)));
        }
      else if(    (sourceref.field->attr  & signable_e)
              && !(destref.field->attr & signable_e) )
        {
        // The dest is unsignable; turn the final nybble into an 0xFo
        gg_assign(gg_indirect(dest_sign),
                  gg_bitwise_or(gg_indirect(dest_sign),
                                build_int_cst_type(UCHAR, 0x0F)));
        }
      else
        {
        // The source and the destination are either both signable, or
        // both unsignable.  We copy the source's sign nybble to the dest.
        gg_assign(gg_indirect(dest_sign),
                  gg_bitwise_or(gg_bitwise_and(gg_indirect(dest_sign),
                                build_int_cst_type(UCHAR, 0xF0)),
                                gg_bitwise_and(gg_indirect(source_sign),
                                build_int_cst_type(UCHAR, 0x0F))));
        }
      }
    }
  clear_negative_zero(destref,
                      sourceref,
                      dest_location);
  return true;
  }

static bool
mh_packed_to_numdisp(const cbl_refer_t &destref,
                     const cbl_refer_t &sourceref,
                           tree         size_error,
                           bool         check_for_error)
  {
  charmap_t *charmap =
                   __gg__get_charmap(destref.field->codeset.encoding);

  if(    (sourceref.field->type != FldPacked         )
      || (destref.field->type   != FldNumericDisplay )
      || (charmap->stride()     != 1                 )
      || (sourceref.field->attr  & scaled_e          )
      || (destref.field->attr    & scaled_e          )
      || (sourceref.field->attr  & packed_no_sign_e  )
      || (destref.field->attr    & leading_e         )
      || (destref.field->attr    & separate_e        ) )
    {
    return false;
    }

  /* Source is packed, dest is numeric-display, neither are scaled, the
     packed source has a sign nybble, and the numeric-display dest has an
     ordinary sign bit encoded in the final digit.  */
  tree umask = build_int_cst_type(UCHAR, 0x0F);
  tree ufour = build_int_cst_type(SIZE_T,   4);
  tree uzero = build_int_cst_type(UCHAR,
                                  charmap->mapped_character(ascii_zero));
  tree source_location;
  tree dest_location;
  tree dest_p          = gg_define_variable(UCHAR_P);
  tree source_p        = gg_define_variable(UCHAR_P);

  get_location(dest_location, destref);
  gg_assign(dest_p, dest_location);
  get_location(source_location, sourceref);

  // source_digits will be the number of digits extracted from the source that
  // find their way into the destination.
  int source_digits   = sourceref.field->data.digits;

  if( !(source_digits & 0x01) )
    {
    // Because this is an even number, the first byte of the packed value is
    // 0x0N.  The following logic is a tad simpler when we just increment it,
    // as if the zero in the left nybble is part of the packed-decimal value.
    source_digits += 1;
    }

  int source_rdigits  = sourceref.field->data.rdigits;
  int source_ldigits  = source_digits - source_rdigits;
  int dest_digits     = destref.field->data.digits;
  int dest_rdigits    = destref.field->data.rdigits;
  int dest_ldigits    = dest_digits - dest_rdigits;

  int truncate_ldigits = std::max(0, source_ldigits-dest_ldigits);
  int truncate_rdigits = std::max(0, source_rdigits-dest_rdigits);
  int leading_zeroes   = std::max(0, dest_ldigits-source_ldigits);
  int trailing_zeroes  = std::max(0, dest_rdigits-source_rdigits);

  int digit_pairs;
  int source_remaining;

  int truncate_pairs = truncate_ldigits/2 ;
  if( truncate_pairs )
    {
    // We handle truncation of digits on the left by moving the starting line
    // one byte to the right for each full pair of digits

    if( check_for_error )
      {
      gg_assign(source_p, source_location);
      tree truncate_end = gg_define_variable(UCHAR_P);
      gg_assign(truncate_end,
                gg_add(source_location,
                       build_int_cst_type(SIZE_T, truncate_pairs)));
      WHILE( source_p, lt_op, truncate_end )
        {
        gg_assign(size_error,
                  gg_bitwise_or(size_error,
                                gg_cast(INT, gg_indirect(source_p))));
        gg_increment(source_p);
        }
      WEND
      }
    else
      {
      gg_assign(source_p,
                gg_add(source_location,
                       build_int_cst_type(SIZE_T, truncate_pairs)));
      }
    source_digits    -= 2*truncate_pairs;
    //source_ldigits   -= 2*truncate_pairs;
    truncate_ldigits &= 0x01;
    }
  else
    {
    gg_assign(source_p, source_location);
    }

  // At this point, truncate_ldigits might be one, meaning that when we
  // get around to  moving digits, we will have to skip the first one.

  if( truncate_rdigits )
    {
    // We handle truncation of digits on the right by moving the finish line
    // to the left.
    source_digits    -= truncate_rdigits;
    //source_ldigits   -= truncate_rdigits;
    }

  source_remaining = source_digits;

  // We are ready to start building our numeric-displace destination.

  if( leading_zeroes )
    {
    tree tleading_zeroes = build_int_cst_type(SIZE_T, leading_zeroes);
    gg_memset(dest_p,
              uzero,
              tleading_zeroes);
    gg_assign(dest_p, gg_add(dest_p, tleading_zeroes));
    }

  // At this point, we are ready to start moving over source_remaining digits.

  if( truncate_ldigits )
    {
    // When truncate_ldigits is one, the first byte comes from the right nybble
    // of *source_p.  We therefore skip the digit in the left nybble.
    if( check_for_error )
      {
      gg_assign(size_error,
                gg_cast(INT,
                        gg_bitwise_and(gg_indirect(source_p),
                                       build_int_cst_type(UCHAR, 0xF0))));
      }
    gg_assign(gg_indirect(dest_p),
                gg_bitwise_or(gg_bitwise_and(gg_indirect(source_p),
                                             umask),
                              uzero));
    gg_increment(source_p);
    gg_increment(dest_p);
    source_remaining -= 2;
    }

  // We now pull pairs of digits from the packed source, and put them into the
  // destination numeric-display.

  digit_pairs = source_remaining/2;

  if( digit_pairs )
    {
    tree source_end = gg_define_variable(UCHAR_P);
    gg_assign(source_end,
              gg_add(source_p,
                     build_int_cst_type(SIZE_T, digit_pairs)));
    WHILE( source_p, lt_op, source_end )
      {
      // Left digit
      gg_assign(gg_indirect(dest_p),
                gg_bitwise_or(gg_rshift(gg_indirect(source_p),
                                        ufour),
                              uzero));
      gg_increment(dest_p);
      // Right digit
      gg_assign(gg_indirect(dest_p),
                gg_bitwise_or(gg_bitwise_and(gg_indirect(source_p),
                                             umask),
                              uzero));
      gg_increment(dest_p);
      gg_increment(source_p);
      }
    WEND
    source_remaining -= 2 * digit_pairs;
    }

  // At this point, source_remaining is zero or one

  if( source_remaining )
    {
    // We have one remaining left digit;
    gg_assign(gg_indirect(dest_p),
              gg_bitwise_or(gg_rshift(gg_indirect(source_p),
                                      ufour),
                            uzero));
    gg_increment(dest_p);
    }

  if( trailing_zeroes )
    {
    tree ttrailing_zeroes = build_int_cst_type(SIZE_T, trailing_zeroes);
    gg_memset(dest_p,
              uzero,
              ttrailing_zeroes);
    }

  if(    (destref.field->attr   & signable_e)
      && (sourceref.field->attr & signable_e) )
    {
    // The source and the destination are both signable.
    gg_assign(source_p,
              gg_add(source_location,
                     build_int_cst_type(SIZE_T,
                                       sourceref.field->data.capacity()-1)));
    IF(gg_bitwise_and(gg_indirect(source_p),
                      umask),
       eq_op,
       build_int_cst_type(UCHAR, 0x0D) )
      {
      // The source is negative
      gg_assign(dest_p,
                gg_add(dest_location,
                       build_int_cst_type(SIZE_T,
                                       destref.field->data.capacity()-1)));
      if( charmap->is_like_ebcdic() )
        {
        // Turn the 0xFZ EBCDIC digit into 0xDZ to flag it as negative.
        gg_assign(gg_indirect(dest_p),
                  gg_bitwise_and(gg_indirect(dest_p),
                                 build_int_cst_type(UCHAR, 0xDF)));
        }
      else
        {
        // Turn the 0x3Z ASCII digit into 07Z to flag it as negative.
        gg_assign(gg_indirect(dest_p),
                  gg_bitwise_or(gg_indirect(dest_p),
                                 build_int_cst_type(UCHAR, 0x70)));
        }
      }
    ELSE
      {
      }
    ENDIF
    }
  clear_negative_zero(destref,
                      sourceref,
                      dest_location);
  return true;
  }

void
move_helper(tree size_error,        // This is an INT
            cbl_refer_t destref,
            cbl_refer_t sourceref,  // Call move_helper with this resolved.
            TREEPLET   &tsource,
            cbl_round_t rounded,
            // True means our caller wants to know about truncation errors
            bool check_for_error,
            bool restore_on_error
            )
  {
  Analyze();
  SHOW_PARSE1
    {
    SHOW_PARSE_INDENT
    SHOW_PARSE_TEXT("move_helper()");
    }

  if( size_error )
    {
    gg_assign(size_error, integer_zero_node);
    }

  static tree stash =
                gg_define_variable(UCHAR_P, "..move_stasher", vs_file_static);

  tree st_data = NULL_TREE;
  tree st_size = NULL_TREE;
  if( restore_on_error )
    {
    // We are creating a copy of the original destination in case we clobber it
    // and have to restore it because of a computational error.
    static bool first_time = true;
    static size_t stash_size = 1024;
    if( first_time )
      {
      first_time = false;
      gg_assign(stash, gg_cast(UCHAR_P, gg_malloc(stash_size)));
      }
    if( stash_size < destref.field->data.capacity() )
      {
      stash_size = destref.field->data.capacity();
      gg_assign(stash, gg_cast(UCHAR_P, gg_realloc(stash, stash_size)));
      }
    st_data = qualified_data_location(destref);
    st_size = refer_size_dest(destref);
    gg_memcpy(stash,
              st_data,
              st_size);
    }

  bool moved = mh_source_is_group(destref, sourceref, tsource);

  if( !moved )
    {
    moved = mh_identical(destref, sourceref);
    }

  if( !moved )
    {
    moved = mh_packed_to_packed(destref,
                                sourceref,
                                size_error,
                                check_for_error);
    }

  if( !moved )
    {
    moved = mh_numdisp_to_packed(destref,
                                sourceref,
                                size_error,
                                check_for_error);
    }

  if( !moved )
    {
    moved = mh_packed_to_numdisp(destref,
                                sourceref,
                                size_error,
                                check_for_error);
    }

  if( !moved )
    {
    moved = mh_source_is_literalN(destref,
                                  sourceref,
                                  check_for_error,
                                  rounded,
                                  size_error);
    }

  if( !moved )
    {
    moved = mh_dest_is_float( destref,
                              sourceref,
                              tsource,
                              rounded,
                              size_error);
    }

  if( !moved && rounded == truncation_e )
    {
    moved = mh_numeric_display( destref,
                                sourceref,
                                tsource,
                                size_error);
    }

  if( !moved )
    {
    moved = mh_to_binary( destref,
                              sourceref,
                              tsource,
                              check_for_error,
                              size_error);
    }

  if( !moved )
    {
    moved = mh_source_is_literalA(destref,
                                  sourceref,
                                  rounded,
                                  size_error);
    }

  if( !moved )
    {
    moved = mh_alpha_to_alpha(destref,
                              sourceref,
                              rounded,
                              size_error);
    }

  if( !moved )
    {
    moved = mh_binary_to_numdisp(destref,
                                 sourceref,
                                 rounded,
                                 size_error);
    }

  if( !moved )
    {
    moved = mh_binary_to_packed(destref,
                                sourceref,
                                rounded,
                                size_error);
    }

  if( !moved )
    {
    SHOW_PARSE1
      {
      SHOW_PARSE_INDENT
      SHOW_PARSE_TEXT("default __gg__move")
      }

    if(    destref.refmod.from
        || destref.refmod.len
        || sourceref.refmod.from
        || sourceref.refmod.len )
      {
      // Let the move routine know to treat the destination as alphanumeric
      attribute_bit_set(destref.field, refmod_e);
      }

    int nflags =   (sourceref.all      ? REFER_T_MOVE_ALL   : 0)
                 + (sourceref.addr_of  ? REFER_T_ADDRESS_OF : 0);

    if( size_error )
      {
      gg_assign(size_error,
                gg_call_expr( INT,
                              "__gg__move",
                              gg_get_address_of(destref.field->var_decl_node),
                              refer_offset(destref),
                              refer_size_dest(destref),
                              tsource.pfield,
                              tsource.offset,
                              tsource.length,
                              build_int_cst_type(INT, nflags),
                              build_int_cst_type(INT, rounded),
                              NULL_TREE));
      }
    else
      {
                gg_call     ( INT,
                              "__gg__move",
                              gg_get_address_of(destref.field->var_decl_node),
                              refer_offset(destref),
                              refer_size_dest(destref),
                              tsource.pfield,
                              tsource.offset,
                              tsource.length,
                              build_int_cst_type(INT, nflags),
                              build_int_cst_type(INT, rounded),
                              NULL_TREE);

      }
    if(    destref.refmod.from
        || destref.refmod.len
        || sourceref.refmod.from
        || sourceref.refmod.len )
      {
      // Return that value to its original form
      attribute_bit_clear(destref.field, refmod_e);
      }

    // moved = true; // commented out to quiet cppcheck
    }

  if( restore_on_error )
    {
    IF(size_error, ne_op, integer_zero_node)
      {
      gg_memcpy(st_data,
                stash,
                st_size);
      }
    ELSE
      ENDIF
    }
  else
    {
    if( check_for_error )
      {
      IF(size_error, ne_op, integer_zero_node)
        {
        // We had a size error, but there was no restore_on_error.
        // Let our lord and master know there was a truncation:
        set_exception_code(ec_size_truncation_e);
        }
      ELSE
        ENDIF
      }
    }

  SHOW_PARSE1
    {
    SHOW_PARSE_END
    }
  }

void
parser_move(cbl_refer_t destref,
            cbl_refer_t sourceref,
            cbl_round_t rounded,
            bool skip_fill_from  // Defaults to false
            )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    if( sourceref.field && is_figconst_low(sourceref.field) )
      {
      SHOW_PARSE_TEXT(" LOW-VALUE")
      }
    else if( sourceref.field && is_figconst_zero(sourceref.field) )
      {
      SHOW_PARSE_TEXT(" ZERO-VALUE")
      }
    else if( sourceref.field && is_figconst_space(sourceref.field) )
      {
      SHOW_PARSE_TEXT(" SPACE-VALUE")
      }
    else if( sourceref.field && is_figconst_quote(sourceref.field) )
      {
      SHOW_PARSE_TEXT(" QUOTE-VALUE")
      }
    else if( sourceref.field && is_figconst_high(sourceref.field) )
      {
      SHOW_PARSE_TEXT(" HIGH-VALUE")
      }
    else
      {
      SHOW_PARSE_REF(" ", sourceref)
      }
    SHOW_PARSE_REF(" TO ", destref)
    switch(rounded)
      {
      case away_from_zero_e:
        SHOW_PARSE_TEXT(" AWAY_FROM_ZERO")
        break;
      case nearest_toward_zero_e:
        SHOW_PARSE_TEXT(" NEAREST_TOWARD_ZERO")
        break;
      case toward_greater_e:
        SHOW_PARSE_TEXT(" TOWARD_GREATER")
        break;
      case toward_lesser_e:
        SHOW_PARSE_TEXT(" TOWARD_LESSER")
        break;
      case nearest_away_from_zero_e:
        SHOW_PARSE_TEXT(" NEAREST_AWAY_FROM_ZERO")
        break;
      case nearest_even_e:
        SHOW_PARSE_TEXT(" NEAREST_EVEN")
        break;
      case prohibited_e:
        SHOW_PARSE_TEXT(" PROHIBITED")
        break;
      case truncation_e:
        SHOW_PARSE_TEXT(" TRUNCATED")
        break;
      default:
        gcc_unreachable();
        break;
      }
    SHOW_PARSE_END
    }

  if( !skip_fill_from )
    {
    cbl_figconst_t figconst = is_figconst(sourceref);
    if( figconst )
      {
      sourceref.all = true;
      }
    }

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_TEXT("About to call move_helper")
    }
  TREEPLET tsource;
  treeplet_fill_source(tsource, sourceref);
  static bool dont_check_for_error = false;
  move_helper(NULL, destref, sourceref, tsource, rounded, dont_check_for_error );

  TRACE1
    {
    TRACE1_INDENT
    TRACE1_REFER_INFO("source ", sourceref)
    TRACE1_INDENT
    TRACE1_REFER_INFO("dest   ", destref)
    TRACE1_END
    }
  }

static
void
parser_move_multi(cbl_refer_t destref,
                  cbl_refer_t sourceref,
                  TREEPLET    tsource,
                  cbl_round_t rounded,
                  bool skip_fill_from )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    if( sourceref.field && is_figconst_low(sourceref.field) )
      {
      SHOW_PARSE_TEXT(" LOW-VALUE")
      }
    else if( sourceref.field && is_figconst_zero(sourceref.field) )
      {
      SHOW_PARSE_TEXT(" ZERO-VALUE")
      }
    else if( sourceref.field && is_figconst_space(sourceref.field) )
      {
      SHOW_PARSE_TEXT(" SPACE-VALUE")
      }
    else if( sourceref.field && is_figconst_quote(sourceref.field) )
      {
      SHOW_PARSE_TEXT(" QUOTE-VALUE")
      }
    else if( sourceref.field && is_figconst_high(sourceref.field) )
      {
      SHOW_PARSE_TEXT(" HIGH-VALUE")
      }
    else
      {
      SHOW_PARSE_REF(" ", sourceref)
      }
    SHOW_PARSE_REF(" TO ", destref)
    switch(rounded)
      {
      case away_from_zero_e:
        SHOW_PARSE_TEXT(" AWAY_FROM_ZERO")
        break;
      case nearest_toward_zero_e:
        SHOW_PARSE_TEXT(" NEAREST_TOWARD_ZERO")
        break;
      case toward_greater_e:
        SHOW_PARSE_TEXT(" TOWARD_GREATER")
        break;
      case toward_lesser_e:
        SHOW_PARSE_TEXT(" TOWARD_LESSER")
        break;
      case nearest_away_from_zero_e:
        SHOW_PARSE_TEXT(" NEAREST_AWAY_FROM_ZERO")
        break;
      case nearest_even_e:
        SHOW_PARSE_TEXT(" NEAREST_EVEN")
        break;
      case prohibited_e:
        SHOW_PARSE_TEXT(" PROHIBITED")
        break;
      case truncation_e:
        SHOW_PARSE_TEXT(" TRUNCATED")
        break;
      default:
        gcc_unreachable();
        break;
      }
    SHOW_PARSE_END
    }

  if( !skip_fill_from )
    {
    cbl_figconst_t figconst = is_figconst(sourceref);
    if( figconst )
      {
      sourceref.all = true;
      }
    }

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_TEXT("About to call move_helper")
    }

  static bool dont_check_for_error = false;
  move_helper(NULL, destref, sourceref, tsource, rounded, dont_check_for_error );

  TRACE1
    {
    TRACE1_INDENT
    TRACE1_REFER_INFO("source ", sourceref)
    TRACE1_INDENT
    TRACE1_REFER_INFO("dest   ", destref)
    TRACE1_END
    }
  }

void
parser_move(size_t ntgt, cbl_refer_t *tgts, cbl_refer_t src, cbl_round_t rounded)
  {
  if( mode_syntax_only() ) return;

  cbl_figconst_t figconst = is_figconst(src);
  if( figconst )
    {
    src.all = true;
    }
  TREEPLET tsource;
  treeplet_fill_source(tsource, src);
  static const bool skip_fill_from = true;
  for( cbl_refer_t *p=tgts; p < tgts + ntgt; p++ )
    {
    parser_move_multi(*p, src, tsource, rounded, skip_fill_from);
    }
  }

