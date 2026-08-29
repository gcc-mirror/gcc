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

static bool
comparably_numeric(const cbl_refer_t &refer)
  {
  // This routine returns TRUE for refers that can be treated as binary
  // integers for the purpose of comparisons.  We handle CHAR through INT128;
  // floats are broken out separately.
  bool retval;

  if( refer.refmod.from )
    {
    // Anything with a refmod is treated like an alphanumeric, not a number.
    retval = false;
    }
  else
    {
    switch(refer.field->type)
      {
      case FldNumericBinary:
      case FldNumericDisplay:
      case FldPacked:
      case FldNumericBin5:
      case FldLiteralN:
      case FldIndex:
      case FldPointer:
        retval = true;
        break;

      case FldAlphanumeric:
        {
        // An FldAlphanumeric flagged as ZERO is a number.
        retval = (cbl_figconst_t)(refer.field->attr & FIGCONST_MASK) ==
                                                                zero_value_e;
        }
      break;

      default:
        retval = false;
        break;
      }
    }

  return retval;
  }

static bool
comparably_alpha(const cbl_refer_t &refer)
  {
  // This routine returns TRUE for refers that can be treated as alphanumeric
  // strings the purpose of comparisons.

  bool retval;

  switch(refer.field->type)
    {
    case FldGroup:
    case FldAlphanumeric:
    case FldLiteralA:
    case FldNumericEdited:
    case FldAlphaEdited:
      retval = true;
      break;

    default:
      if( refer.refmod.from )
        {
        retval = true;
        }
     else
        {
        retval = false;
        }
      break;
    }
  return retval;
  }

enum {MAX_INT128_DIGITS = 38};

static void
digiter(int &digits, int &rdigits, const cbl_refer_t &refer)
  {
  digits  = refer.field->data.digits;
  rdigits = refer.field->data.rdigits;

  if( digits == 0 )
    {
    // We are dealing with a pure binary type:
    switch(refer.field->data.capacity())
      {
      case 1:
        digits = 2;
        break;
      case 2:
        digits = 4;
        break;
      case 4:
        digits = 9;
        break;
      case 8:
        digits = 19;
        break;
      case 16:
        digits = 38;
        break;
      default:
        gcc_unreachable();
        break;
      }
    }
  if( refer.field->attr & scaled_e )
    {
    if( rdigits < 0 )
      {
      // This is like 999PPPP with rdigits = -4
      // So, digits evntually becomes 7, and rdigits becomes 0
      // Our caller will have to multiply by 10^4 to get the 999 digits into
      // the right place.
      rdigits = 0;
      }
    else
      {
      // This is like PPP9999.  Digits stays 3, and rdigits becomes 7
      rdigits += digits;
      }
    }
  }

static int
total_digits(int &left_rdigits,
             int &right_rdigits,
             int &left_discard,
             int &right_discard,
             const cbl_refer_t &left_side,
             const cbl_refer_t &right_side)
  {
  int left_digits = 0;
  int right_digits = 0;

  left_discard = 0;
  right_discard = 0;

  digiter(left_digits, left_rdigits, left_side);
  digiter(right_digits, right_rdigits, right_side);

  if(    (left_side.field->attr & scaled_e)
      && left_side.field->data.rdigits < 0 )
    {
    right_rdigits -= left_side.field->data.rdigits;
    }

  if(    (right_side.field->attr & scaled_e)
      && right_side.field->data.rdigits < 0 )
    {
    left_rdigits -= right_side.field->data.rdigits;
    }

  int excess_digits = std::min(left_rdigits, right_rdigits);

  left_rdigits  -= excess_digits;
  right_rdigits -= excess_digits;

  left_digits  += right_rdigits;
  right_digits += left_rdigits;

  int ntotal_digits = std::max(left_digits, right_digits);
  int too_much = ntotal_digits - MAX_INT128_DIGITS;

  if( too_much > 0 )
    {
    gcc_assert(left_rdigits || right_rdigits);

    if( left_rdigits )
      {
      gcc_assert(too_much <= left_rdigits);
      left_rdigits -= too_much;
      left_discard = too_much;
      }
    else
      {
      gcc_assert(too_much <= right_rdigits);
      right_rdigits -= too_much;
      right_discard = too_much;
      }

    ntotal_digits = MAX_INT128_DIGITS;
    }

  return ntotal_digits;
  }

static void
total_digits_tree( tree &left_rdigits,
                   tree &right_rdigits,
                   const cbl_refer_t &left_side,
                   const cbl_refer_t &right_side)
  {
  // This routine is used when we might have to use the run-time values:
  left_rdigits = gg_define_variable(INT);
  right_rdigits = gg_define_variable(INT);

  if( left_side.field->attr & intermediate_e )
    {
    gg_assign( left_rdigits,
               gg_cast(INT,
                       member(left_side.field->var_decl_node,
                              "rdigits")));
    }
  else
    {
    gg_assign(left_rdigits,
              build_int_cst_type(INT,
                                 left_side.field->data.rdigits));
    }

  if( right_side.field->attr & intermediate_e )
    {
    gg_assign(right_rdigits,
              gg_cast(INT,
                      member(right_side.field->var_decl_node,
                             "rdigits")));
    }
  else
    {
    gg_assign(right_rdigits,
              build_int_cst_type(INT,
                                 right_side.field->data.rdigits));
    }

  if( (left_side.field->attr & scaled_e) && left_side.field->data.rdigits < 0)
    {
    // Left rdigits becomes zero; right_rdigits is augmented to the negative of
    // left rdigits
    gg_assign(left_rdigits, integer_zero_node);
    gg_assign(right_rdigits,
              gg_add(right_rdigits,
                     build_int_cst_type(INT,
                                        -left_side.field->data.rdigits)));
    }
  if( (left_side.field->attr & scaled_e) && left_side.field->data.rdigits > 0)
    {
    // left_rdigits is augmented by left_digits
    gg_assign(left_rdigits,
              gg_add(left_rdigits,
                     build_int_cst_type(INT,
                                        left_side.field->data.digits)));
    }

  if( (right_side.field->attr & scaled_e) && right_side.field->data.rdigits < 0)
    {
    // Right rdigits becomes zero; left_rdigits is augmented by the negative of
    // right_rdigits
    gg_assign(right_rdigits, integer_zero_node);
    gg_assign(left_rdigits,
              gg_add(left_rdigits,
                     build_int_cst_type(INT,
                                        -right_side.field->data.rdigits)));
    }
  if( (right_side.field->attr & scaled_e) && right_side.field->data.rdigits > 0)
    {
    // right_rdigits is augmented by right_digits
    gg_assign(right_rdigits,
              gg_add(right_rdigits,
                     build_int_cst_type(INT,
                                        right_side.field->data.digits)));
    }

  // We can reduce the two rdigits values by the common portion of both.  This
  // will leave one of them at zero
  IF( left_rdigits, gt_op, right_rdigits )
    {
    gg_assign(left_rdigits, gg_subtract(left_rdigits, right_rdigits));
    gg_assign(right_rdigits, integer_zero_node);
    }
  ELSE
    {
    gg_assign(right_rdigits, gg_subtract(right_rdigits, left_rdigits));
    gg_assign(left_rdigits, integer_zero_node);
    }
  ENDIF
  }

static tree
type_based_on_digits(int digits, bool signable)
  {
  /* When working one's way through this code, it is useful to know that the
     underlying design assumptions are:

     Supported targets are LP64.
     Values entering the 128-bit comparison path fit in signed INT128.
     Intermediate rdigits is nonnegative.
     intermediate_e and scaled_e are mutually exclusive.  */

  tree retval;
  if( signable )
    {
    // if(digits <= 2)
      // {
      // retval = SCHAR;
      // }
    // else
    if(digits <= 4)
      {
      retval = SHORT;
      }
    else if(digits <= 9)
      {
      retval = INT;
      }
    else if(digits <= 19)
      {
      retval = LONG;
      }
    else
      {
      retval = INT128;
      }
    }
  else
    {
    if(digits <= 2)
      {
      retval = UCHAR;
      }
    else if(digits <= 4)
      {
      retval = USHORT;
      }
    else if(digits <= 9)
      {
      retval = UINT;
      }
    else if(digits <= 19)
      {
      retval = ULONG;
      }
    else
      {
      retval = UINT128;
      }
    }

  return retval;
  }

static bool
numeric_compare(tree        &left,
                tree        &right,
          const cbl_refer_t &left_side,
          const cbl_refer_t &right_side)
  {
  /* This routine handles binary integer values that can fit into one, two,
     four, eight, or sixteen bytes.

     In order to do that, we have to make sure that the normalized comparable
     values both fit.

     Nomenclature: for short, I will treat a 9(8)v9(3) as 8.3.

     So consider comparing a 32.0 to a 1.9.  To normalize them, I would have to
     multiply the 32.0 by 10^9, which would mean I would be comparing a
     32.9 to a 1.9 .  This is mathematically correct; the problem is that an
     __int128 can hold only only 38 digits, and thus can't handle the 41 digits
     of a 32.9.

     So, in this routine I make sure the two values can be normalized into no
     more than INT128.  Values that are too large fall through to a library
     routine that can handle them, albeit in a slower fashion than we aspire to
     here.     */

  bool compared = false;

  if( refer_is_super_clean(left_side) && refer_is_super_clean(right_side) )
    {
    int left_rdigits;
    int right_rdigits;
    int left_discard;
    int right_discard;
    int ntotal_digits = total_digits( left_rdigits,
                                      right_rdigits,
                                      left_discard,
                                      right_discard,
                                      left_side,
                                      right_side);
    gcc_assert( ntotal_digits <= MAX_INT128_DIGITS );
    if( left_discard )
      {
      scale_by_power_of_ten_N(left, -left_discard);
      }

    if( right_discard )
      {
      scale_by_power_of_ten_N(right, -right_discard);
      }

    // Our interest is in comparison.  So, we need the two values to be
    // of the same type.  This is complicated by the problem of comparing
    // a signed value to an unsigned value.  To be sure of doing that, we
    // convert both the signed and unsigned values to be signed values of a
    // larger type.

    bool mismatched =    (left_side.field->attr  & signable_e)
                      != (right_side.field->attr & signable_e) ;

    bool are_signed = mismatched || (left_side.field->attr  & signable_e) ;
    tree type = type_based_on_digits(ntotal_digits, are_signed);

    if( mismatched )
      {
      switch( gg_sizeof(type) )
        {
        case 1:
          type = SHORT;
          break;
        case 2:
          type = INT;
          break;
        case 4:
          type = LONG;
          break;
        default:
          type = INT128;
          break;
        }
      }

    left  = get_binary_value(left_side, type);
    right = get_binary_value(right_side, type);

    // We have two good binary values, and they are the same size.

    // If left_rdigits and right_rdigits are different, then one of the
    // values needs to be scaled by the other's rdigits:

    static uint64_t powt[] =
      {
      1UL,                       // 00
      10UL,                      // 01
      100UL,                     // 02
      1000UL,                    // 03
      10000UL,                   // 04
      100000UL,                  // 05
      1000000UL,                 // 06
      10000000UL,                // 07
      100000000UL,               // 08
      1000000000UL,              // 09
      10000000000UL,             // 10
      100000000000UL,            // 11
      1000000000000UL,           // 12
      10000000000000UL,          // 13
      100000000000000UL,         // 14
      1000000000000000UL,        // 15
      10000000000000000UL,       // 16
      };

    while(left_rdigits)
      {
      // We need to multiply right by 10^left_rdigits
      int next = std::min(left_rdigits, 16);
      left_rdigits -= next;
      right =   gg_multiply(right,
                            gg_cast(type,
                                    build_int_cst_type(ULONG,
                                                       powt[next])));
      }
    while(right_rdigits)
      {
      // We need to multiply left by 10^right_rdigits
      int next = std::min(right_rdigits, 9);
      right_rdigits -= next;
      left =    gg_multiply(left,
                            gg_cast(type,
                                    build_int_cst_type(ULONG,
                                                       powt[next])));
      }
    compared = true;
    }
  else
    {
    // One or both are not in working-storage, so we have to use run-time
    // capacities and rdigits.

    // We will be needing the run-time left_rdigits and right_rdigits
    tree left_rdigits;
    tree right_rdigits;
    total_digits_tree(left_rdigits,
                      right_rdigits,
                      left_side,
                      right_side);

    // Our interest is comparison, so we convert them both to INT128.
    tree type = INT128;

    left  = get_binary_value(left_side, type);
    right = get_binary_value(right_side, type);

    // We have two good binary values, and they are the same size.

    // To normalize the positions of decimal points, each number has to be
    // multiplied by the rdigits of the other
    right =   gg_multiply(right,
                          gg_call_expr(INT128,
                                       "__gg__power_of_ten",
                                       left_rdigits,
                                       NULL_TREE));
    left =   gg_multiply(left,
                          gg_call_expr(INT128,
                                       "__gg__power_of_ten",
                                       right_rdigits,
                                       NULL_TREE));
    // The left and right values are ready to be compared
    compared = true;
    }

  return compared;
  }

static void
alpha_compare_figconst( tree        &left,
                        tree        &right,
                  const cbl_refer_t &left_side,
                  const cbl_refer_t &right_side)
  {
  // Call this when you know the right side is a figconst or refer.all
  cbl_figconst_t figconst_right
                   = (cbl_figconst_t)(right_side.field->attr & FIGCONST_MASK);

  tree location_left = get_location(left_side);
  tree length_left = get_length(left_side);

  const charmap_t *charmap_left =
                       __gg__get_charmap(left_side.field->codeset.encoding);

  // We know the result of this mapping has to be an 8-bit value, because
  // all figconsts map to a single byte.  HIGH-VALUE is a bit of a problem,
  // but when is it not?  It usually will be 0xFF in the low-order byte, so
  // that's what we assume for now.

  uint8_t char_right = char_from_figconst(figconst_right);

  size_t nbytes;
  char *converted;
  if( figconst_right )
    {
    // Comparing an alphanumeric to a figconst
    // We need to convert the char_right to the left's encoding:
    converted = __gg__iconverter(DEFAULT_SOURCE_ENCODING,
                                 left_side.field->codeset.encoding,
                                 &char_right,
                                 1,
                                 &nbytes);
    }
  else
    {
    // Comparing an alphanumeric to ALL <literal>
    converted = __gg__iconverter(right_side.field->codeset.encoding,
                                 left_side.field->codeset.encoding,
                                 right_side.field->data.initial,
                                 right_side.field->data.capacity(),
                                 &nbytes);
    }
  right = integer_zero_node;
  left = gg_call_expr(INT,
                      "__gg__compare_string_all",
                      location_left,
                      length_left,
                      build_int_cst_type(INT, charmap_left->stride()),
                      build_string_literal(nbytes, converted),
                      build_int_cst_type(SIZE_T, nbytes),
                      NULL_TREE);
  }

static bool
alpha_compare(tree        &left,
              tree        &right,
        const cbl_refer_t &left_side,
        const cbl_refer_t &right_side)
  {
  /* This routine handles these cases:

        alpha1 op alpha2
        national1 op display2
        display1 op national2  ***
        alpha op literal
        literal op alpha       ***
        alpha op all literal
        all literal op alpha   ***
        alpha op figconst
        figconst op alpha      ***
        figconst op figconst

     The four cases marked *** are handled by this routine being called a
     second time with the parameters swapped.  */

  bool retval = false;

  charmap_t *charmap_left  = __gg__get_charmap(left_side.field->codeset.encoding);
  cbl_figconst_t figconst_left
                   = (cbl_figconst_t)(left_side.field->attr & FIGCONST_MASK);
  uint8_t char_left  = charmap_left->figconst_character(figconst_left);

  charmap_t *charmap_right = __gg__get_charmap(right_side.field->codeset.encoding);
  cbl_figconst_t figconst_right
                   = (cbl_figconst_t)(right_side.field->attr & FIGCONST_MASK);
  uint8_t char_right = charmap_right->figconst_character(figconst_right);

  tree location_left;
  tree location_right;
  tree length_left;
  tree length_right;

  if( figconst_left && figconst_right )
    {
    // This is a degenerate case.  Technically it isn't allowed; ISO says that
    // comparing two literals is a syntax error.  But that's not my department.
    if( char_left < char_right )
      {
      left = integer_minus_one_node;
      }
    else if( char_left > char_right)
      {
      left = integer_one_node;
      }
    else
      {
      left = integer_zero_node;
      }
    right = integer_zero_node;
    retval = true;
    goto done;
    }
  else if( figconst_right || right_side.all )
    {
    alpha_compare_figconst(left, right, left_side, right_side);
    retval = true;
    goto done;
    }
  else if(    !figconst_left
          &&  !figconst_right
          &&   left_side.field->codeset.encoding
             != right_side.field->codeset.encoding )
    {
    /* We have two different encodings.  The logic chain is:
       If they have different strides, we convert to the larger one, else
       If one is national, we convert to it, else
       If one is display, we convert to it, else
       We pick one at random.

       In this chain, the winner has to be on the left.  If the winner is on
       the right, we don't do it now.  It will get caught during the second
       call to this routine, with the parameters flipped left-for-right.  */
    bool do_it = false;
    if( charmap_left->stride() != charmap_right->stride() )
      {
      // The strides are different.  Convert when the bigger one is on the
      // left.
      do_it = charmap_left->stride() > charmap_right->stride();
      }
    else if(   left_side.field->codeset.encoding  == __gg__national_encoding
            || right_side.field->codeset.encoding == __gg__national_encoding )
      {
      // One or the other is national.  We convert when the left one is
      // national.
      do_it = left_side.field->codeset.encoding  == __gg__national_encoding;
      }
    else if(   left_side.field->codeset.encoding  == __gg__display_encoding
            || right_side.field->codeset.encoding == __gg__display_encoding )
      {
      // One or the other is display.  We convert when the left one is
      // display.
      do_it = left_side.field->codeset.encoding  == __gg__display_encoding;
      }
    else
      {
      // They are the same size, and neither is national or display.  This is
      // highly weird.  The only thing I can think causing it would be two
      // files with different specified encodings.  Just have the left side
      // win.
      do_it = true;
      }
    if( do_it )
      {
      // Call the library routine that converts the right side to the left
      // encoding.
      location_left = get_location(left_side);
      length_left   = get_length(left_side);
      location_right = get_location(right_side);
      length_right   = get_length(right_side);
      right = integer_zero_node;
      left = gg_call_expr(INT,
                          "__gg_compare_string_different",
                          location_left,
                          length_left,
                          build_int_cst_type(INT, left_side.field->codeset.encoding),
                          location_right,
                          length_right,
                          build_int_cst_type(INT, right_side.field->codeset.encoding),
                          NULL_TREE);
      retval = true;
      goto done;
      }
    }
  else if(    !figconst_left
          &&  !figconst_right
          &&   left_side.field->codeset.encoding
             == right_side.field->codeset.encoding )
    {
    /* The two encodings are the same. */

    // When the difference in lengths of the two strings is bigger than the
    // magic number, we call the library routine.  The library routine uses
    // a const string of spaces and memcmp to handle the inferred trailing
    // spaces of the shorter string.  So, the magic number represents the
    // crossover of the time for a loop here versus the time for a call to
    // the library.  I haven't made any effort to find the best value.
    // R.J.Dubner; 2026-05-08
    static const long MAGIC_NUMBER = 16;

    // The conversion of a space can be no more than four bytes.
    char ach_space[4];
    char ch = ascii_space;
    size_t nbytes;
    const char *converted = __gg__iconverter(DEFAULT_SOURCE_ENCODING,
                                             left_side.field->codeset.encoding,
                                             &ch,
                                             1,
                                             &nbytes);
    gcc_assert(nbytes <= sizeof(ach_space));
    gcc_assert(nbytes == static_cast<size_t>(charmap_left->stride()));
    memcpy(ach_space, converted, nbytes);
    const char *the_routine;
    switch( charmap_left->stride() )
      {
      case 1:
        {
        // We are single-byte-coded.
        if( current_function->alphabet_in_use  )
          {
          // Call the routine that uses the collation table.
          the_routine = "__gg__compare_string_1";
          }
        else
          {
          size_t length_l = left_side.field->data.capacity();
          size_t length_r = right_side.field->data.capacity();
          ssize_t diff = length_l - length_r;
          if(    refer_is_super_clean(left_side)
              && refer_is_super_clean(right_side)
              && std::abs(diff) <= MAGIC_NUMBER )
            {
            // There is no collation table in use, we are single-byte encoded,
            // and both variables are in working storage at known locations and
            // with known lengths.  We can build code that is extremely
            // efficient.
            location_left  = get_location(left_side);
            location_right = get_location(right_side);
            left  = gg_define_variable(INT);
            right = integer_zero_node;
            size_t length = std::min(length_l, length_r);
            gg_assign(left,
                      gg_memcmp(location_left,
                                location_right,
                                build_int_cst_type(SIZE_T,
                                                   length)));
            if( length_l > length_r )
              {
              // We have a LEFT excess that needs to be compared to the space
              // char.
              IF( left, eq_op, integer_zero_node )
                {
                tree count = gg_define_variable(SIZE_T);
                gg_assign(count, build_int_cst_type(SIZE_T,
                                                   length));
                WHILE( count, lt_op, build_int_cst_type(SIZE_T,
                                                        length_l) )
                  {
                  IF( gg_indirect(location_left, count),
                      ne_op,
                      build_int_cst_type(UCHAR, *ach_space) )
                    {
                    // We have a difference.  We need to calculate +1/-1
                    IF( gg_indirect(location_left, count),
                        gt_op,
                        build_int_cst_type(UCHAR, *ach_space) )
                      {
                      gg_assign(left, integer_one_node);
                      }
                    ELSE
                      {
                      gg_assign(left, integer_minus_one_node);
                      }
                    ENDIF
                    // Force the end of the loop:
                    gg_assign(count, build_int_cst_type(SIZE_T,
                                                        length_l));
                    }
                  ELSE
                    {
                    // The *left is a space; keep going
                    gg_increment(count);
                    }
                  ENDIF
                  }
                WEND
                }
              ELSE
                {
                }
              ENDIF
              }

            if( length_l < length_r )
              {
              // We have a RIGHT excess that needs to be compared to the space
              // char.
              IF( left, eq_op, integer_zero_node )
                {
                tree count = gg_define_variable(SIZE_T);
                gg_assign(count, build_int_cst_type(SIZE_T,
                                                   length));
                WHILE( count, lt_op, build_int_cst_type(SIZE_T,
                                                        length_r) )
                  {
                  IF( gg_indirect(location_right, count),
                      ne_op,
                      build_int_cst_type(UCHAR, *ach_space) )
                    {
                    // We have a difference.  We need to calculate +1/-1
                    IF( gg_indirect(location_right, count),
                        lt_op,
                        build_int_cst_type(UCHAR, *ach_space) )
                      {
                      gg_assign(left, integer_one_node);
                      }
                    ELSE
                      {
                      gg_assign(left, integer_minus_one_node);
                      }
                    ENDIF
                    // Force the end of the loop:
                    gg_assign(count, build_int_cst_type(SIZE_T,
                                                        length_r));
                    }
                  ELSE
                    {
                    // The *right is a space; keep going
                    gg_increment(count);
                    }
                  ENDIF
                  }
                WEND
                }
              ELSE
                {
                }
              ENDIF
              }

            retval = true;
            goto done;
            }
          // Call the routine that uses does straight byte comparison
          if( charmap_left->is_like_ebcdic() )
            {
            the_routine = "__gg__compare_string_1e";
            }
          else
            {
            the_routine = "__gg__compare_string_1a";
            }
          }
        break;
        }
      case 2:
        {
        if( current_function->alphabet_in_use  )
          {
          // Call the routine that uses the collation table.
          the_routine = "__gg__compare_string_2";
          }
        else
          {
          // Call the routine that uses does straight short comparison
          the_routine = "__gg__compare_string_2a";
          }
        break;
        }
      case 4:
        {
        if( current_function->alphabet_in_use  )
          {
          // Call the routine that uses the collation table.
          the_routine = "__gg__compare_string_4";
          }
        else
          {
          // Call the routine that uses does straight short comparison
          the_routine = "__gg__compare_string_4a";
          }
        break;
        }
      default:
        {
        the_routine = nullptr;
        gcc_unreachable();
        break;
        }
      }
    location_left = get_location(left_side);
    length_left   = get_length(left_side);
    location_right = get_location(right_side);
    length_right   = get_length(right_side);
    right = integer_zero_node;
    left = gg_call_expr(INT,
                        the_routine,
                        location_left,
                        length_left,
                        location_right,
                        length_right,
                        build_string_literal(charmap_left->stride(), ach_space),
                        NULL_TREE);
    retval = true;
    goto done;
    }

  done:
  return retval;
  }

static bool
numeric_alpha_compare(tree        &left,
                      tree        &right,
                const cbl_refer_t &left_side,
                const cbl_refer_t &right_side)
  {
  /* The ISO specification says

    The integer operand is treated as though it were moved, according to the
    rules of the MOVE statement, to an elementary data item of the same length
    in terms of character positions as the number of digits in the integer and
    after that it is compared to the string.   */

  bool compared = false;

  const charmap_t *charmap_left =
                          __gg__get_charmap(left_side.field->codeset.encoding);
  cbl_figconst_t figconst_right
                   = (cbl_figconst_t)(right_side.field->attr & FIGCONST_MASK);
  uint8_t char_right = char_from_figconst(figconst_right);

  if( left_side.field->type == FldLiteralN )
    {
    // On the left side, we have data.original.  On the right side, we have
    // some kind of string.

    const char *left_data = left_side.field->data.original();
    if( *left_data == ascii_minus || *left_data == ascii_plus )
      {
      // The rule for moving a numeric to an alphanumeric is that sign
      // information is discarded.
      left_data += 1;
      }

    tree location_left = gg_string_literal(left_data);
    tree length_left   = build_int_cst_type(
                                   SIZE_T,
                                   strlen(left_data));

    if( figconst_right || right_side.all )
      {
      size_t nbytes;
      char *converted;
      if( figconst_right )
        {
        // Comparing an alphanumeric to a figconst
        // We need to convert the char_right to the left's encoding:
        converted = __gg__iconverter(DEFAULT_SOURCE_ENCODING,
                                     left_side.field->codeset.encoding,
                                     &char_right,
                                     1,
                                     &nbytes);
        }
      else
        {
        // Comparing an alphanumeric to ALL <literal>
        converted = __gg__iconverter(right_side.field->codeset.encoding,
                                     left_side.field->codeset.encoding,
                                     right_side.field->data.initial,
                                     right_side.field->data.capacity(),
                                     &nbytes);
        }
      right = integer_zero_node;
      left = gg_call_expr(INT,
                          "__gg__compare_string_all",
                          location_left,
                          length_left,
                          build_int_cst_type(INT, charmap_left->stride()),
                          build_string_literal(nbytes, converted),
                          build_int_cst_type(SIZE_T, nbytes),
                          NULL_TREE);
      compared = true;
      }
    else if( right_side.field->type == FldLiteralA )
      {
      // Corner cases.  One grows to dislike them.  Here we are comparing a
      // FieldLiteraN to a FldLiteralA
      right = integer_zero_node;
      tree str = gg_string_literal(right_side.field->data.original());
      tree length = build_int_cst_type(SIZE_T,
                                    strlen(right_side.field->data.original()));
      left = gg_call_expr(INT,
                          "__gg__compare_string_1a",
                          location_left,
                          length_left,
                          str,
                          length,
                          gg_cast(UCHAR_P, integer_zero_node),
                          NULL_TREE);
      compared = true;
      }
    }
  if( !compared )
    {
    // The left side is a fixed-point numeric of some kind.  First, we pick
    // up the actual numeric value.

    // The type of the variable should be straightforward.  It is not.  There
    // are various considerations in determining the type.
    tree value;
    tree type = tree_type_from_refer(left_side);

    // We have what we need to get the value:
    value = get_binary_value(left_side, type);

    // We have corner case to deal with here.  When comparing a numeric to a
    // alphanumeric that is a figconst zero_value_e, we treat the right side
    // as a numeric zero.

    if( figconst_right == zero_value_e )
      {
      value = save_expr(value);
      right = integer_zero_node;
      gg_assign(left, integer_zero_node);
      tree below = gg_logical_lt(value, gg_cast(type, integer_zero_node));
      tree above = gg_logical_gt(value, gg_cast(type, integer_zero_node));
      left = if_condition(below,
                          gg_cast(type, integer_minus_one_node),
                          if_condition(above,
                                       gg_cast(type, integer_one_node),
                                       gg_cast(type, integer_zero_node),
                                       type),
                          type);
      compared = true;
      }
    else
      {
      if( left_side.field->data.rdigits < 0 )
        {
        // We need to multiply value by 10^-rdigits
        value = scale_by_power_of_ten( value,
                                       build_int_cst_type(INT,
                                       -left_side.field->data.rdigits));
        }

      if( left_side.field->attr&signable_e )
        {
        // For numeric-alphabetic comparisons, there are no negative values:
        value = gg_abs(value);
        }

      right = integer_zero_node;

      if( figconst_right || right_side.all )
        {
        size_t nbytes;
        char *converted;
        if( figconst_right )
          {
          converted = __gg__iconverter(DEFAULT_SOURCE_ENCODING,
                                       left_side.field->codeset.encoding,
                                       &char_right,
                                       1,
                                       &nbytes);
          }
        else
          {
          converted = __gg__iconverter(right_side.field->codeset.encoding,
                                       left_side.field->codeset.encoding,
                                       right_side.field->data.initial,
                                       right_side.field->data.capacity(),
                                       &nbytes);
          }
        left = gg_call_expr(INT,
                            "__gg__compare_numeric_all",
                            gg_cast(INT128, value),
                            build_int_cst_type(SIZE_T, left_side.field->data.digits),
                            build_string_literal(nbytes, converted),
                            build_int_cst_type(SIZE_T, nbytes),
                            build_int_cst_type(INT, left_side.field->codeset.encoding),
                            NULL_TREE);
        }
      else
        {
        tree location_right;
        tree length_right;
        location_right = get_location(right_side);
        length_right = get_length(right_side);
        left = gg_call_expr(INT,
                            "__gg__compare_binary_to_string",
                            gg_cast(INT128, value),
                            build_int_cst_type(SIZE_T, left_side.field->data.digits),
                            location_right,
                            length_right,
                            build_int_cst_type(INT, right_side.field->codeset.encoding),
                            NULL_TREE);
        }
      compared = true;
      }
    }

  return compared;
  }

static bool
addr_of_compare(tree        &left,
                tree        &right,
          const cbl_refer_t &left_side,
          const cbl_refer_t &right_side)
  {
  // One or the other is addr_of
  tree type = SIZE_T;
  tree l;
  tree r;
  if( left_side.addr_of && !right_side.addr_of )
    {
    l = get_location(left_side);
    r = get_binary_value(right_side, type);
    }
  else if( !left_side.addr_of && right_side.addr_of )
    {
    l = get_binary_value(left_side, type);
    r = get_location(right_side);
    }
  else
    {
    // They are both addr_of.
    l = get_location(left_side);
    r = get_location(right_side);
    }

  left  = gg_cast(SIZE_T, l);
  right = gg_cast(SIZE_T, r);

  return true;
  }

static bool
float_compare(tree        &left,
              tree        &right,
        const cbl_refer_t &left_side,
        const cbl_refer_t &right_side)
  {
  // left_side is a float, and if right_side is also a float it is smaller than
  // left_side.

  // See the comment at type_based_on_digits

  tree type = tree_type_from_field(left_side.field);
  left  = get_binary_value(left_side, type);
  right = get_binary_value(right_side, type);

  if( right_side.field->attr & intermediate_e )
    {
    tree rdigits = gg_cast(INT,
                           member(right_side.field->var_decl_node,
                                  "rdigits"));
    right =   gg_real_divide(right,
                             gg_cast(type,
                                     gg_call_expr(INT128,
                                                  "__gg__power_of_ten",
                                                  rdigits,
                                                  NULL_TREE)));
    }
  else
    {
    int rdigits = right_side.field->data.rdigits;

    if(    (right_side.field->attr & scaled_e)
        && rdigits > 0 )
      {
      rdigits += right_side.field->data.digits;
      }

    if( rdigits < 0 )
      {
      right = gg_multiply(
                right,
                gg_cast(type,
                        gg_call_expr(INT128,
                                     "__gg__power_of_ten",
                                     build_int_cst_type(INT, -rdigits),
                                     NULL_TREE)));
      }
    else if( rdigits > 0 )
      {
      right = gg_real_divide(
                right,
                gg_cast(type,
                        gg_call_expr(INT128,
                                     "__gg__power_of_ten",
                                     build_int_cst_type(INT, rdigits),
                                     NULL_TREE)));
      }
    }

  return true;
  }

static bool
compare_class( tree        &left,
               tree        &right,
         const cbl_refer_t &left_side,
         const cbl_refer_t &right_side)
  {
  // Left side is FldClass
  right = integer_zero_node;
  tree right_loc = get_location(right_side);
  tree right_length = get_length(right_side);

  left = gg_call_expr( INT,
                       "__gg__compare_field_class",
                       gg_get_address_of(right_side.field->var_decl_node),
                       right_loc,
                       right_length,
                       gg_get_address_of(left_side.field->var_decl_node),
                       NULL_TREE);
  return true;
  }

void
cobol_compare(tree        &left,
              tree        &right,
        const cbl_refer_t &left_side,
        const cbl_refer_t &right_side)
  {
  // This routine figures out how to compare left_side to right_side, and
  // returns the trees 'left' and 'right' in numeric form that can be turned
  // into a conditional expression.

  bool compared = false;

  if( left_side.field->type == FldClass )
    {
    compared = compare_class(left, right, left_side, right_side);
    }

  if( !compared && right_side.field->type == FldClass )
    {
    compared = compare_class(right, left, right_side, left_side);
    }

  if(!compared && (left_side.addr_of || right_side.addr_of) )
    {
    compared = addr_of_compare(left, right, left_side, right_side);
    }

  if(    !compared
      && comparably_numeric(left_side) && comparably_numeric(right_side) )
    {
    compared = numeric_compare(left, right, left_side, right_side);
    }

  if(    !compared
      && comparably_alpha(left_side) && comparably_alpha(right_side) )
    {
    compared = alpha_compare(left, right, left_side, right_side);
    if( !compared )
      {
      compared = alpha_compare(right, left, right_side, left_side);
      }
    }

  if(  !compared
     && comparably_numeric(left_side) && comparably_alpha(right_side) )
    {
    compared = numeric_alpha_compare(left, right, left_side, right_side);
    }

  if(  !compared
     && comparably_numeric(right_side) && comparably_alpha(left_side) )
    {
    compared = numeric_alpha_compare(right, left, right_side, left_side);
    }

  if(   !compared
     && left_side.field->type == FldFloat
     && right_side.field->type == FldFloat)
    {
    if( left_side.field->data.capacity() >= right_side.field->data.capacity() )
      {
      compared = float_compare(left, right, left_side, right_side);
      }
    else
      {
      compared = float_compare(right, left, right_side, left_side);
      }
    }

  if(   !compared
     &&    (left_side.field->type == FldFloat
        || right_side.field->type == FldFloat) )
    {
    if( left_side.field->type == FldFloat )
      {
      compared = float_compare(left, right, left_side, right_side);
      }
    else
      {
      compared = float_compare(right, left, right_side, left_side);
      }
    }

  if( !compared )
    {
    gcc_unreachable();
    }
  }
