/*
 * Copyright (c) 2021-2025 Symas Corporation
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

/*
 * This file supports parsing without requiring access to the symbol
 * table definition.  Unlike the Bison input, this file brings in gcc
 * header files.
 */

#include "cobol-system.h"
#include "coretypes.h"
#include "tree.h"
#undef yy_flex_debug

#include <langinfo.h>

#include "coretypes.h"
#include "version.h"
#include "demangle.h"
#include "intl.h"
#include "backtrace.h"
#include "diagnostic.h"
#include "diagnostic-color.h"
#include "diagnostic-url.h"
#include "diagnostic-metadata.h"
#include "diagnostic-path.h"
#include "edit-context.h"
#include "selftest.h"
#include "selftest-diagnostic.h"
#include "opts.h"
#include "util.h"
#include "cbldiag.h"
#include "lexio.h"

#include "../../libgcobol/ec.h"
#include "../../libgcobol/common-defs.h"
#include "symbols.h"
#include "inspect.h"
#include "../../libgcobol/io.h"
#include "genapi.h"

#pragma GCC diagnostic ignored "-Wunused-result"
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"

// External declarations.
extern FILE * yyin;
extern int yyparse(void);

extern int demonstration_administrator(int N);

#if !defined (HAVE_GET_CURRENT_DIR_NAME)
/* Posix platforms might not have get_current_dir_name but should have
   getcwd() and PATH_MAX.  */
#if __has_include (<limits.h>)
# include <limits.h>
#endif
/* The Hurd doesn't define PATH_MAX.  */
#if !defined (PATH_MAX) && defined(__GNU__)
# define PATH_MAX 4096
#endif
static inline char *
get_current_dir_name ()
{
  /* Use libiberty's allocator here.  */
  char *buf = (char *) xmalloc (PATH_MAX);
  return getcwd (buf, PATH_MAX);
}
#endif

const char *
symbol_type_str( enum symbol_type_t type )
{
    switch(type) {
    case SymFilename:
        return "SymFilename";
    case SymFunction:
        return "SymFunction";
    case SymField:
        return "SymField";
    case SymLabel:
        return "SymLabel";
    case SymSpecial:
        return "SymSpecial";
    case SymAlphabet:
        return "SymAlphabet";
    case SymFile:
        return "SymFile";
    case SymDataSection:
        return "SymDataSection";
    }
    dbgmsg("%s:%d: invalid symbol_type_t %d", __func__, __LINE__, type);
    return "???";
}

const char *
cbl_field_type_str( enum cbl_field_type_t type )
{
  switch(type) {
  case FldDisplay:
    return "FldDisplay";
  case FldInvalid:
    return "Fld"; // Invalid";
  case FldGroup:
    return "FldGroup";
  case FldAlphanumeric:
    return "FldAlphanumeric";
  case FldNumericBinary:
    return "FldNumericBinary";
  case FldFloat:
    return "FldFloat";
  case FldNumericBin5:
    return "FldNumericBin5";
  case FldPacked:
    return "FldPacked";
  case FldNumericDisplay:
    return "FldNumericDisplay";
  case FldNumericEdited:
    return "FldNumericEdited";
  case FldAlphaEdited:
    return "FldAlphaEdited";
  case FldLiteralA:
    return "FldLiteralA";
  case FldLiteralN:
    return "FldLiteralN";
  case FldClass:
    return "FldClass";
  case FldConditional:
    return "FldConditional";
  case FldForward:
    return "FldForward";
  case FldIndex:
    return "FldIndex";
  case FldSwitch:
    return "FldSwitch";
  case FldPointer:
    return "FldPointer";
  case FldBlob:
    return "FldBlob";
 }
  dbgmsg("%s:%d: invalid symbol_type_t %d", __func__, __LINE__, type);
  return "???";
}

const char *
cbl_logop_str( enum logop_t op )
{
  switch(op) {
  case not_op:
    return "not_op";
  case and_op:
    return "and_op";
  case or_op:
    return "or_op";
  case xor_op:
    return "xor_op";
  case xnor_op:
    return "xnor_op";
  case true_op:
    return "true_op";
  case false_op:
    return "false_op";
  }
  dbgmsg("%s:%d: invalid logop_t %d", __func__, __LINE__, op);
  return "???";
}

cbl_field_t
determine_intermediate_type( const cbl_refer_t& aref,
                             int op __attribute__ ((unused)),
                             const cbl_refer_t& bref )
  {
  cbl_field_t output = {};

  if( aref.field->type == FldFloat || bref.field->type == FldFloat )
    {
    output.type = FldFloat;
    output.data.capacity = 16;
    output.attr = (intermediate_e );
    }
  else if(   op == '*'
          && aref.field->data.digits + bref.field->data.digits
                                                      > MAX_FIXED_POINT_DIGITS)
    {
    output.type = FldFloat;
    output.data.capacity = 16;
    output.attr = (intermediate_e );
    }
  else
    {
    output.type = FldNumericBin5;
    output.data.capacity = 16;
    output.data.digits   = MAX_FIXED_POINT_DIGITS;
    output.attr = (intermediate_e | signable_e );
    }

  return output;
  }

static char regexmsg[80];

/*
 * Scan part of the picture, parsing any repetition count.
 */
int
repeat_count( const char picture[] )
{
  char ch;
  int n, count = -1;

  n = sscanf( picture, "%c(%d)", &ch, &count );
  if( count <= 0 && 4 < n ) { // parsed count is negative
    count = 0; // zero is invalid; -1 means no repetition
  }
  return count;
}

const char *numed_message;

extern int yydebug, yy_flex_debug;

bool
is_alpha_edited( const char picture[] ) {
  static const char valid[] = "abxABX90/(),.";
  assert(picture);

  for( const char *p = picture; *p != '\0'; p++ ) {
    if( strchr(valid, *p) ) continue;
    if( ISDIGIT(*p) ) continue;
    if( symbol_decimal_point() == *p ) continue;
    if( symbol_currency(*p) ) continue;

    if( yydebug ) {
      dbgmsg( "%s: bad character '%c' at %.*s<-- in '%s'",
             __func__, *p, int(p - picture) + 1, picture, picture );
    }
    return false;
  }
  return true;
}

bool
is_numeric_edited( const char picture[] ) {
  static const char valid[] = "BbPpVvZz90/(),.+-*"; // and CR DB
  const char *p;
  assert(picture);

  if( strstr(picture, "(0)") ) {
    numed_message = "'(0)' invalid in PICTURE (ISO 2023 13.18.40.3)";
    return false;
  }

  // check for correct parenthetical constructs
  for( p=picture; (p = strchr(p, '(')) != NULL; p++ ) {
    int v, n, pos;
    n = sscanf(++p, "%d%n", &v, &pos);
    numed_message = NULL;

    if( n == -1  ) {
      numed_message = "invalid repeat-count in PICTURE";
    } else if( n == 0  ) {
      numed_message = "invalid repeat-count in PICTURE";
    } else if( p[pos] != ')' ) {
      numed_message = "unbalanced parentheses in PICTURE";
    }
    if( numed_message ) return false;
  }
  // check for dangling right parenthesis
  for( p=picture; (p = strchr(p, ')')) != NULL; p++ ) {
    auto prior = p;
    while( picture < prior-- ) {
      if( ISDIGIT(*prior) ) continue;
      if( *prior  == '(' ) break;
      numed_message = "unbalanced parentheses in PICTURE";
      return false;
    }
  }

  if( (strchr(picture, 'Z') || strchr(picture, 'z')) && strchr(picture, '*') ) {
    numed_message = "Z and * are mutually exclusive";
    return false;
  }

  for( p = picture; *p != '\0'; p++ ) {
    if( strchr(valid, *p) ) continue;
    if( ISDIGIT(*p) ) continue;
    if( symbol_decimal_point() == *p ) continue;
    if( symbol_currency(*p) ) continue;

    switch(*p) { // test for CR or DB
    case 'C': case 'c':
      if( TOUPPER(*++p) == 'R' ) continue;
      numed_message = "expected CR in PICTURE";
      break;
    case 'D': case 'd':
      if( TOUPPER(*++p) == 'B' ) continue;
      numed_message = "expected DB in PICTURE";
      break;
    default:
      numed_message = xasprintf("invalid PICTURE character "
                                "'%c' at offset %zu in '%s'",
                                *p, p - picture, picture);
      break;
    }

    dbgmsg( "%s: no, because '%c' at %.*s<-- in '%s'",
            __func__, *p, int(p - picture) + 1, picture, picture );

    return false;
  }
  return true;
}

char *
normalize_picture( char picture[] )
{
    int erc;
    char *p;

    regex_t *preg = NULL;
    const char regex[] = "([AX9])[(]([[:digit:]]+)[)]";
    int cflags = REG_EXTENDED | REG_ICASE;
    regmatch_t pmatch[4];

    if( (erc = regcomp(preg, regex, cflags)) != 0 ) {
        regerror(erc, preg, regexmsg, sizeof(regexmsg));
        dbgmsg( "%s:%d: could not compile regex: %s", __func__, __LINE__, regexmsg );
        return picture;
    }

    while( (erc = regexec(preg, picture, COUNT_OF(pmatch), pmatch, 0)) == 0 ) {
        assert(pmatch[1].rm_so != -1 && pmatch[1].rm_so < pmatch[1].rm_eo);
        size_t len = pmatch[1].rm_eo - pmatch[1].rm_so;
        assert(len == 1);
        const char *start = picture + pmatch[1].rm_so;

        assert(pmatch[2].rm_so != -2 && pmatch[2].rm_so < pmatch[2].rm_eo);
        len = pmatch[2].rm_eo - pmatch[2].rm_so;
        assert(len > 0);

        /*
         * Overwrite e.g. A(4) with AAAA.
         */
        assert(pmatch[2].rm_so == pmatch[1].rm_eo + 1); // character paren number
        p = picture + pmatch[2].rm_so;
        len = 0;
        if( 1 != sscanf(p, "%zu", &len) ) {
            dbgmsg("%s:%d: no number found in '%s'", __func__, __LINE__, p);
            goto irregular;
        }
        if( len == 0 ) {
            dbgmsg("%s:%d: ZERO length found in '%s'", __func__, __LINE__, p);
            goto irregular;
        }

	std::vector <char> pic(len + 1, '\0');
        memset(pic.data(), *start, len);
        const char *finish = picture + pmatch[2].rm_eo,
                    *eopicture = picture + strlen(picture);

        p = xasprintf( "%*s%s%*s",
		       (int)(start - picture), picture,
		       pic.data(),
		       (int)(eopicture - finish), finish );

        free(picture);
        picture = p;
        continue;
    }
    assert(erc == REG_NOMATCH);

irregular:
    regfree(preg);

    return picture;
}

static bool
memall( const char picture[], char ch )
{
    for( const char *p=picture; *p != '\0'; p++ ) {
        if( *p != ch ) {
            return false;
        }
    }
    return true;
}

static const char *
match( const char picture[], const char pattern[] )
{
    int erc;

    regex_t *preg = NULL;
    int cflags = REG_EXTENDED;
    regmatch_t pmatch[1];

    if( (erc = regcomp(preg, pattern, cflags)) != 0 ) {
        regerror(erc, preg, regexmsg, sizeof(regexmsg));
        dbgmsg( "%s:%d: could not compile regex: %s", __func__, __LINE__, regexmsg );
        return picture;
    }

    if( (erc = regexec(preg, picture, COUNT_OF(pmatch), pmatch, 0)) != 0 ) {
        assert(erc == REG_NOMATCH);
        return NULL;
    }
    assert(pmatch[0].rm_so != -1);
    return picture + pmatch[0].rm_so;
}

bool
is_elementary( enum cbl_field_type_t type )
{
    switch(type) {
    case FldDisplay:
    case FldInvalid:
    case FldGroup:
    case FldLiteralA:
    case FldLiteralN:
    case FldClass:
    case FldConditional:
    case FldForward:
    case FldIndex:
    case FldSwitch:
    case FldBlob:
      return false;
    case FldPointer:
    case FldAlphanumeric:
    case FldPacked:
    case FldNumericDisplay:
    case FldNumericEdited:
    case FldAlphaEdited:
    case FldNumericBinary:
    case FldNumericBin5:
    case FldFloat:
      return true; // takes up space
    }
    dbgmsg("%s:%d: invalid symbol_type_t %d", __func__, __LINE__, type);
    return false;
}

static bool
is_numericish( cbl_field_type_t type ) {
  return
    type == FldNumericDisplay ||
    type == FldNumericEdited  || is_numeric(type);
}

static inline bool
is_numericish( const struct cbl_field_t *field ) {
  return is_numericish(field->type);
}

static bool
integer_move_ok( const cbl_field_t *src, const cbl_field_t *tgt ) {
  if( is_numericish(src) &&
      ! (tgt->type == FldInvalid || is_literal(tgt)) ) {
    if( src->data.rdigits > 0 ) {
      dbgmsg("%s has %d rdigits", src->name, src->data.rdigits);
    }
    return src->data.rdigits == 0;
  }
  return integer_move_ok( tgt, src );
}

static bool
is_alphanumeric( const cbl_field_t *field ) {
  assert(field);

  if( is_elementary(field->type) ) {
    switch(field->type) {
    case FldAlphanumeric:
    case FldPacked:
    case FldNumericDisplay:
    case FldNumericEdited:
    case FldAlphaEdited:
    case FldNumericBinary:
      return true;
    case FldNumericBin5:
    case FldFloat:
      return false;
    default:
      break;
    }
    return false;
  }

  if( field->type != FldGroup ) return false;

  const struct symbol_elem_t *e = symbol_elem_of(field);

  for( ++e; e < symbols_end(); e++ ) {
    if( e->type != SymField ) {
      // Ignore non-fields:
      continue;
    }
    const uint32_t level = cbl_field_of(e)->level;
    if( level == 88 ) continue;
    if( level <= field->level || level == LEVEL77 ) {
      break; // stop if next field is higher in the hierarchy
    }

    if( ! is_alphanumeric(cbl_field_of(e)) ) {
      return false;
    }
  }
  return true;
}

/*
 * When setting a field's type, there is a 3-way test involving:
 *   1.  The current value of cbl_field_t::type
 *   2.  The value of cbl_field_t::usage, from USAGE or parent's USAGE
 *   3.  The candidate (proposed new type)
 *
 * cbl_field_t::usage == FldInvalid indicates no prescribed
 * type. Type-setting succeeds unless the candidate cannot override
 * the current type.
 *
 * A candidate of FldDisplay updates cbl_field_t::usage only, and only
 * if it is FldInvalid, provided the cbl_field_t::type is either
 * FldInvalid or displayable.  FldDisplay isn't really a type, but a
 * kind of type: it constrains what the type may be set to.
 *
 * When cbl_field_t::usage == FldDisplay, the candidate type must be
 * displayable, else the update is rejected.
 *
 * If the candidate passes the usage test, we consider the current type.
 *
 * cbl_field_t::type == FldInvalid indicates no defined type
 * (yet). The candidate type becomes the type.  Otherwise, the
 * candidate must match the type, or can override it.
 */
static bool
is_displayable( cbl_field_type_t type ) {
  switch(type) {
  case FldDisplay:
  case FldAlphaEdited:
  case FldAlphanumeric:
  case FldNumericDisplay:
  case FldNumericEdited:
    return true;
  default: break;
  }
  return false;
}

// disallow implausible combinations
static bool
plausible_usage( cbl_field_type_t usage, cbl_field_type_t candidate ) {
  switch(usage) {
  case FldInvalid:
    return true;
  case FldDisplay:
    return is_displayable(candidate);
  case FldGroup:
    gcc_unreachable();
  default:
    if( candidate == FldDisplay ) return false; // because overrides FldInvalid only
    break;
  }

  assert(is_elementary(usage));
  assert(is_elementary(candidate));
  return usage == candidate || (is_numericish(usage) && is_numericish(candidate));
}

cbl_field_t *
symbol_field_index_set( cbl_field_t *field ) {
  static const cbl_field_data_t data { 0, 8 };

  field->data = data;

  field->type = FldIndex;
  field->attr &= ~size_t(signable_e);

  return field;
}

bool
symbol_field_type_update( cbl_field_t *field,
                          cbl_field_type_t candidate, bool is_usage ) {

  if( is_usage && (candidate == FldIndex || candidate == FldPointer)  ) {
    field->usage = candidate;
    switch(field->type) {
    case FldInvalid:
    case FldIndex:
    case FldPointer:
      // set the type
      field->type = candidate;
      if( field->data.capacity == 0 ) {
        static const cbl_field_data_t data = {0, 8, 0, 0, NULL};
        field->data = data;
        field->attr &= ~size_t(signable_e);
      }
      return true;
    default:
      break;
    }
    return false;  // type unchanged
  }

  assert(candidate == FldDisplay || is_elementary(candidate));
  assert(field->type != FldDisplay); // can never be
  assert(field->usage == FldInvalid ||
         field->usage == FldDisplay || is_elementary(field->usage));

  if( ! (field->type == FldInvalid ||
         field->type == FldGroup   || is_elementary(field->type)) ) {
    return false; // semantic user error
  }

  // type matches itself
  if( field->type == candidate ) {
    if( is_usage ) field->usage = candidate;
    return true;
  }
  if( is_usage && field->usage == candidate ) return true;

  if( ! plausible_usage(field->usage, candidate) ) return false;

  /*
   *  FldDisplay candidate
   */
  if( candidate == FldDisplay ) {  // update usage at most
    if( field->type == FldInvalid ||
        field->type == FldGroup ||
        is_displayable(field->type) ) {
      field->usage = candidate;
      return true;
    }
    return false;
  }

  assert(field->type != candidate && is_elementary(candidate));

  /*
   *  Concrete usage candidate.  Update usage first (if USAGE clause), then type.
   */
  if( is_usage ) {
    switch(field->type) {
    case FldBlob:
    case FldDisplay:
      gcc_unreachable(); // type is never just "display"
      break;
    case FldAlphaEdited:
      break;
    case FldNumericEdited:
    case FldPointer:
      if( is_numeric(candidate) ) {
        return false;
      }
      __attribute__((fallthrough));
    case FldInvalid:
    case FldGroup:
    case FldNumericDisplay:
      field->usage = candidate;
      break;
    case FldLiteralA:
    case FldLiteralN:
    case FldClass:
    case FldConditional:
    case FldForward:
    case FldIndex:
    case FldSwitch:
      gcc_unreachable();
    case FldAlphanumeric:
      // MF allows PIC X(n) to have USAGE COMP-[5x]
      if( candidate != FldNumericBin5 ) return false;
      if( ! (dialect_mf() && field->has_attr(all_x_e)) ) {
        return false;
      }
      __attribute__((fallthrough));
    case FldFloat:
    case FldNumericBin5:
    case FldNumericBinary:
    case FldPacked:
      assert(field->type != candidate); // ensured by test at start of function
      field->usage = candidate;
    }
  }

  // Now, apply (possibly new) usage to type
  assert( !is_usage || field->usage == candidate );

  /*
   *  Concrete type candidate
   */
  switch(field->usage) {
  case FldInvalid:
    field->type = candidate;
    field->attr |= numeric_group_attrs(field);
    return true;
  case FldDisplay:
    if( is_displayable(candidate) ) {
      field->type = candidate;
      field->attr |= numeric_group_attrs(field);
      return true;
    }
    break;
  case FldAlphaEdited:
  case FldAlphanumeric:
    assert( dialect_mf() && field->has_attr(all_x_e) );
    // convert all X's alphanumeric to numeric
    field->clear_attr(all_x_e);
    field->type = field->usage;
    field->attr |= numeric_group_attrs(field);
    return true;
  case FldNumericDisplay:
  case FldNumericEdited:
  case FldGroup:
  case FldLiteralA:
  case FldLiteralN:
  case FldClass:
  case FldConditional:
  case FldForward:
  case FldSwitch:
  case FldPointer:
  case FldBlob:
    // invalid usage value
    gcc_unreachable();
    break;
  case FldIndex:
    if( field->usage == candidate ) {
      field->type = candidate;
      return true;
    }
    break;
  case FldFloat:
  case FldNumericBin5:
  case FldNumericBinary:
  case FldPacked:
    if( field->usage == candidate ) {
      field->type = candidate;
      return true;
    }
    if( candidate == FldNumericDisplay ) {
      field->type = field->usage;
      field->attr |= numeric_group_attrs(field);
      return true;
    }
    break;
  }
  return false;
}

bool
redefine_field( cbl_field_t *field ) {
  cbl_field_t *primary = symbol_redefines(field);
  bool fOK = true;

  if( !primary ) return false;

  if( field->type == FldInvalid ) { // no PICTURE
    field->type = primary->type;
    field->data = primary->data;
    field->data.initial = NULL;
  }

  if( field->data.capacity == 0 ) field->data = primary->data;

  if( is_numeric(field->type) && field->usage == FldDisplay ) {
    fOK = symbol_field_type_update(field, FldNumericDisplay, false);
  }

  return fOK;
}

void
cbl_field_t::report_invalid_initial_value(const YYLTYPE& loc) const {

  if( ! data.initial ) return;

  auto fig = cbl_figconst_of(data.initial);

  // numeric initial value
  if( is_numeric(type) ) {
    if( has_attr(quoted_e) ) {
      error_msg(loc, "numeric type %s VALUE '%s' requires numeric VALUE",
               name, data.initial);
      return;
    }
    if( ! (fig == normal_value_e || fig == zero_value_e)  ) {
        error_msg(loc, "numeric type %s VALUE '%s' requires numeric VALUE",
                 name, cbl_figconst_str(fig));
        return;
      }

    switch( type ) {
    case FldIndex:
    case FldNumericBin5:
      if( data.digits == 0 ) {
        // We are dealing with a pure binary type.  If the capacity is
        // 8 or more, we need do no further testing because we assume
        // everything fits.
        if( data.capacity < 8 ) {
          auto p = strchr(data.initial, symbol_decimal_point());
          if( p && atoll(p+1) != 0 ) {
            error_msg(loc, "integer type %s VALUE '%s' "
                     "requires integer VALUE",
                     name, data.initial);
          } else {
            // Calculate the maximum possible value that a binary with this
            // many bytes can hold
            size_t max_possible_value;
            max_possible_value = 1;
            max_possible_value <<= data.capacity*8;
            max_possible_value -= 1;
            if( attr & signable_e )
              {
                // Because it is signable, we divide by two to account for the
                // sign bit:
                max_possible_value >>= 1;
              }
            // Pick up the given VALUE
            size_t candidate;
            if( *data.initial == '-' ) {
              // We care about the magnitude, not the sign
              if( !(attr & signable_e) ){
                error_msg(loc, "integer type %s VALUE '%s' "
                         "requires a non-negative integer",
                         name, data.initial);
              }
              candidate = atoll(data.initial+1);
            }
            else {
              candidate = (size_t)atoll(data.initial);
            }
            if( candidate > max_possible_value ) {
              error_msg(loc, "integer type %s VALUE '%s' "
                       "requires an integer of magnitude no greater than %zu",
                       name, data.initial, max_possible_value);
            }
          }
        }
      }
      break;
    case FldFloat:
      break;
    default:
      if( ! has_attr(scaled_e) ) {
        /*
         * Check fraction for excess precision
         */
        const char *p = strchr(data.initial, symbol_decimal_point());
        if( p ) {
          auto pend = std::find(p, p + strlen(p), 0x20);
          int n = std::count_if( ++p, pend, isdigit );

          if( data.precision() < n) {
            if( 0 == data.rdigits ) {
              error_msg(loc, "integer type %s VALUE '%s' requires integer VALUE",
                       name, data.initial);
            } else {
              auto has_exponent = std::any_of( p, pend,
                                               []( char ch ) {
                                                 return TOUPPER(ch) == 'E';
                                               } );
              if( !has_exponent && data.precision() < pend - p ) {
                error_msg(loc, "%s cannot represent  VALUE '%s' exactly (max .%zu)",
                         name, data.initial, pend - p);
              }
            }
          }
        } else {
          p = data.initial + strlen(data.initial);
        }

        /*
         * Check magnitude, whether or not there's a decimal point.
         */
        // skip leading zeros
        auto first_digit = std::find_if( data.initial, p,
                                         []( char ch ) {
                                           return ch != '0'; } );
        // count remaining digits, up to the decimal point
        auto n = std::count_if( first_digit, p, isdigit );
        if( data.ldigits() < n ) {
          error_msg(loc, "numeric %s VALUE '%s' holds only %u digits",
                   name, data.initial,
                   data.digits);
        }
      }
      break;
    } // end type switch for normal string initial value
    return;
  } // end numeric
  assert( ! is_numeric(type) );

  // consider all-alphabetic
  if( has_attr(all_alpha_e) ) {
    bool alpha_value = fig != zero_value_e;

    if( fig == normal_value_e ) {
      alpha_value = std::all_of( data.initial,
                                 data.initial +
                                 strlen(data.initial),
                                 []( char ch ) {
                                   return ISSPACE(ch) ||
                                     ISPUNCT(ch) ||
                                     ISALPHA(ch); } );
    }
    if( ! alpha_value ) {
      error_msg(loc, "alpha-only %s VALUE '%s' contains non-alphabetic data",
               name, fig == zero_value_e? cbl_figconst_str(fig) : data.initial);
    }
  }

  return;
}

// Return the field representing the subscript whose literal value
// exceeds the OCCURS clause for that dimension, else NULL if all
// literals are in bounds.
const cbl_field_t *
literal_subscript_oob( const cbl_refer_t& r, size_t& isub /* output */)  {
  // Verify literal subscripts if dimensions are correct.
  size_t ndim(dimensions(r.field));
  if( ndim == 0 || ndim != r.nsubscript ) return NULL;
  cbl_refer_t *esub = r.subscripts + r.nsubscript;
  std::vector<cbl_field_t *> dims( ndim, NULL );
  auto pdim = dims.end();

  for( auto f = r.field; f; f = parent_of(f) ) {
    if( f->occurs.ntimes() ) {
      --pdim;
      *pdim = f;
    }
  }
  assert(dims[0] != NULL);
  assert(pdim == dims.begin());

  /*
   * For each subscript, if it is a literal, verify it is in bounds
   * for the corresponding dimension.  Return the first subscript not
   * meeting those criteria, if any.
   */
  auto p = std::find_if( r.subscripts, esub,
                         [&pdim]( const cbl_refer_t& r ) {
                           const auto& occurs((*pdim)->occurs);
                           pdim++;
                           return ! occurs.subscript_ok(r.field);
                         } );
  isub = p - r.subscripts;
  return p == esub? NULL : dims[isub];
}

size_t
cbl_refer_t::subscripts_set( const std::list<cbl_refer_t>& subs ) {
  nsubscript = subs.size();
  subscripts = new cbl_refer_t[nsubscript];
  std::copy( subs.begin(), subs.end(), subscripts );

  return dimensions(field);
}

const char *
cbl_refer_t::str() const {
  static char subscripts[64];
  sprintf(subscripts, "(%u of %zu dimensions)", nsubscript, dimensions(field));
  char *output = xasprintf("%s %s %s",
                           field? field_str(field) : "(none)",
                           0 < dimensions(field)? subscripts : "",
                           is_refmod_reference()? "(refmod)" : "" );
  return output;
}
const char *
cbl_refer_t::name() const {
  if( prog_func ) return prog_func->name;
  char *output = xasprintf("%s", field? field->name : "(none)" );
  return output;
}

const char *
cbl_refer_t::deref_str() const {
  std::vector<char> dimstr(nsubscript * 16, '\0');
  dimstr.at(0) = '(';
  auto p = dimstr.begin() + 1;

  if( !field ) return name();

  for( auto sub = subscripts; sub <  subscripts + nsubscript; sub++ ) {
    auto initial = sub->field->data.initial ? sub->field->data.initial : "?";
    size_t len = dimstr.end() - p;
    p += snprintf( &*p, len, "%s ", initial );
  }
  if( 0 < nsubscript ) {
    *--p = ')';
  }
  char *output = xasprintf("%s%s", field->name, dimstr.data());
  return output;
}

struct move_corresponding_field {
  cbl_refer_t tgt, src;

  move_corresponding_field( const cbl_refer_t& tgt, const cbl_refer_t& src )
    : tgt(tgt), src(src) {}

  void operator()( corresponding_fields_t::const_reference elem ) {
    if( elem.second == 0 ) return;
    src.field = cbl_field_of(symbol_at(elem.first));
    tgt.field = cbl_field_of(symbol_at(elem.second));

    if( yydebug ) {
      dbgmsg("move_corresponding:%d: SRC: %3zu %s", __LINE__,
            elem.first, src.str());
      dbgmsg("move_corresponding:%d:   to %3zu %s", __LINE__,
            elem.second, tgt.str());
    }

    parser_move(tgt, src);
  }
};

bool
move_corresponding( cbl_refer_t& tgt, cbl_refer_t& src )
{
  assert(tgt.field && src.field);
  assert(tgt.field->type == FldGroup);
  assert(src.field->type == FldGroup);

  corresponding_fields_t pairs = corresponding_move_fields( src.field,
                                                            tgt.field );
  if( pairs.empty() ) return false;

  std::for_each( pairs.begin(), pairs.end(),
                 move_corresponding_field(tgt, src) );
  return true;
}

bool
valid_move( const struct cbl_field_t *tgt, const struct cbl_field_t *src )
{
    // This is the base matrix of allowable moves.  Moves from Alphanumeric are
    // modified based on the attribute bit all_alpha_e, and moves from Numeric
    // types to Alphanumeric and AlphanumericEdited are allowable when the
    // Numeric type is integer, and not allowed when the type has digits to the
    // right of the decimal point.

    // Note that the ordering of elements in this matrix has to match the
    // ordering of the symbols.h elements in enum cbl_field_type_t.

    static const unsigned char matrix[FldLiteralN+1][FldLiteralN+1] = {
        // src down, tgt across
        //I  G  A  B  F  P  5 ND NE AE LA LN
        { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, },  // FldInvalid
        { 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, },  // FldGroup
        { 0, 1, 1, 8, 8, 8, 8, 8, 8, 1, 0, 0, },  // FldAlphanumeric
        { 0, 1, 6, 1, 1, 1, 1, 1, 1, 2, 0, 0, },  // FldNumericBinary  (numeric)
        { 0, 1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, },  // FldFloat
        { 0, 1, 6, 1, 1, 1, 1, 1, 1, 2, 0, 0, },  // FldPacked         (numeric)
        { 0, 1, 6, 1, 1, 1, 1, 1, 1, 2, 0, 0, },  // FldNumericBin5    (numeric)
        { 0, 1, 6, 1, 1, 1, 1, 1, 1, 2, 0, 0, },  // FldNumericDisplay (numeric)
        { 0, 1, 4, 1, 1, 1, 1, 1, 1, 1, 0, 0, },  // FldNumericEdited
        { 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, },  // FldAlphaEdited
        { 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, },  // FldLiteralA
        { 0, 1, 6, 1, 1, 1, 1, 1, 1, 2, 0, 0, },  // FldLiteralN       (numeric)
    };
  /* Needs C++11 */
  static_assert(sizeof(matrix[0]) == COUNT_OF(matrix[0]),
                "matrix should be square");

  for( const cbl_field_t *args[] = {tgt, src}, **p=args;
       p < args + COUNT_OF(args); p++ ) {
    auto& f(**p);
    switch(f.type) {
    case FldClass:
    case FldConditional:
    case FldIndex:
    case FldSwitch:
    case FldDisplay:
    case FldPointer:
      return false;
    // parser should not allow the following types here
    case FldForward:
    case FldBlob:
    default:
      if( sizeof(matrix[0]) < f.type ) {
        cbl_internal_error("logic error: MOVE %s %s invalid type:",
                           cbl_field_type_str(f.type), f.name);
      }
      break;
    }
  }

  assert(tgt->type < sizeof(matrix[0]));
  assert(src->type < sizeof(matrix[0]));

  // A value of zero  means the move is prohibited.
  // The 1 bit means the move is allowed
  // The 2 bit means the move is allowed if the source has zero rdigits,
  //                                               or is P-scaled
  // The 4 bit means the move is allowed if dest   all_alpha_e is off.
  // The 8 bit means the move is allowed if source all_alpha_e is off.

  bool retval = false;
  bool nofraction = src->data.rdigits == 0 || src->has_attr(scaled_e);
  bool alphabetic = tgt->has_attr(all_alpha_e);
  bool src_alpha =  src->has_attr(all_alpha_e);

  switch( matrix[src->type][tgt->type] )
    {
    case 0:
      if( src->type == FldLiteralA && is_numericish(tgt) && !is_literal(tgt) ) {
        // Allow if input string is an integer.
        const char *p = src->data.initial, *pend = p + src->data.capacity;
        if( p[0] == '+' || p[0] == '-' ) p++;
        retval = std::all_of( p, pend, isdigit );
        if( yydebug && ! retval ) {
          auto bad = std::find_if( p, pend,
                                   []( char ch ) { return ! ISDIGIT(ch); } );
          dbgmsg("%s:%d: offending character '%c' at position %zu",
                __func__, __LINE__, *bad, bad - p);
        }
      }
      break;
    case 1:
      retval = true;
      break;
    case 2:
      retval = nofraction;
      break;
    case 4:
      retval = !alphabetic;
      break;
    case 6:
      retval = nofraction && !alphabetic;
      break;
    case 8:
      retval = !src_alpha;
      break;
    default:
      dbgmsg("%s:%d: matrix at %s, %s is %d", __func__, __LINE__,
            cbl_field_type_str(src->type), cbl_field_type_str(tgt->type),
            matrix[src->type][tgt->type]);
      gcc_unreachable();
    }

  if( retval && src->has_attr(embiggened_e) ) {
    if( is_numeric(tgt) && tgt->data.capacity < src->data.capacity ) {
      dbgmsg("error: source no longer fits in target");
      return false;
    }
  }

  return retval;
}

bool
valid_picture( enum cbl_field_type_t type, const char picture[] )
{
  switch(type) {
  case FldBlob:
    gcc_unreachable(); // can't get here via the parser
  case FldInvalid:
  case FldGroup:
  case FldLiteralA:
  case FldLiteralN:
  case FldClass:
  case FldConditional:
  case FldForward:
  case FldIndex:
  case FldSwitch:
  case FldDisplay:
  case FldPointer:
    // These types don't take pictures; the grammar shouldn't call the function.
    dbgmsg("%s:%d: no polaroid: %s", __func__, __LINE__, cbl_field_type_str(type));
    return false;
  case FldNumericBinary:
  case FldFloat:
  case FldNumericBin5:
  case FldPacked:
    // Validated in scan.l.
    return true;
  case FldAlphanumeric:
    // Cannot be all As or all 9s.
    return !( memall(picture, 'A') || memall(picture, '9') );
  case FldNumericDisplay:
    // Must have A or X and B, 0, or /.
    return match(picture, "[AX]") && match(picture, "[B0/]");
  case FldNumericEdited:
  case FldAlphaEdited:
   break;
  }
  assert(type == FldNumericEdited );

  // Must contain at least one 0, B, /, Z, *, +, (comma), ., –, CR, DB, or cs.
  if( ! match( picture, "[$0B/Z*+.,+–]|DB$|CR$" ) ) {
    return false;
  }

  return true;
}

uint32_t
type_capacity( enum cbl_field_type_t type, uint32_t digits )
{
    switch(type) {
    case FldBlob: gcc_unreachable();
    case FldInvalid:
    case FldGroup:
    case FldAlphanumeric:
    case FldNumericDisplay:
    case FldNumericEdited:
    case FldAlphaEdited:
    case FldClass:
    case FldConditional:
    case FldForward:
    case FldIndex:
    case FldSwitch:
    case FldDisplay:
    case FldPointer:
        return digits;
    case FldFloat:
    case FldNumericBinary:
    case FldNumericBin5:
    case FldLiteralA:
    case FldLiteralN:
        break;
    case FldPacked:
        return (digits+2)/2;  // one nybble per digit + a sign nybble
    }

    static const struct sizes_t {
      std::pair<uint32_t, uint32_t> bounds;
      size_t size;
      sizes_t( uint32_t first, uint32_t last, uint32_t size )
	: bounds(first, last), size(size)
      {}
    } sizes[] = {
      { 1,  4,  2 },
      { 5,  9,  4 },
      {10, 18,  8 },
      {19, 38, 16 },
    }, *esizes = sizes + COUNT_OF(sizes);

    auto psize = std::find_if( sizes, esizes,
			  [digits]( sizes_t sizes ) {
			    return sizes.bounds.first <= digits && digits <= sizes.bounds.second;
			  } );
    if( psize != esizes ) return psize->size;

    dbgmsg( "%s:%d: invalid size %u for type %s", __func__, __LINE__,
           digits, cbl_field_type_str(type) );

    return digits;
}

typedef char hex_pair_t[2];

class scan_hex {
public:
  unsigned char operator()( const hex_pair_t input ) {
    static char buffer[ sizeof(hex_pair_t) + 1 ] = "";

    memcpy( buffer, input, sizeof(buffer) - 1 );
    unsigned int x;
    sscanf( buffer, "%x", &x );
    return x;
  }
};

/*
 * Convert hexadecimal string to ASCII, e.g. X'434154' to "CAT".
 */
char *
hex_decode( const char input[] ) {
  const size_t len = strlen(input);
  assert( 0 == len % 2 );

  auto output = static_cast<char *>(xcalloc( 1, 1 + len / 2 ));
  auto beg = reinterpret_cast<const hex_pair_t*>(input + 0),
       end = reinterpret_cast<const hex_pair_t*>(input + len);
  std::transform( beg, end, output, scan_hex() );
  return output;
}

/*
 * Verify unique procedure reference.
 *
 * Section and paragraph names need not be unique unless they are
 * referenced, for example by PERFORM.
 *
 * When a program contains sections, a paragraph can be referenced
 * without qualification if it's unique within the current section or
 * globally.  Else <para> OF <sect> is required. That means the
 * validity of a reference depends on location of reference, which is
 * why order matters. (We can't use line number because the Cobol text
 * could be all on one line.)
 *
 * We maintain a map of referenceable {section,paragraph} pairs, with
 * a count. A count of 1 means it's globally unique.
 *
 * For local calls, we maintain a multimap of sections (whose names might
 * not be unique) in order of appearance.  For each section, we have a
 * set of paragraph names defined by the section, and a count, plus a
 * list of references: {section,paragraph} names used by PERFORM or
 * similar.
 *
 * To determine if a call is valid:
 *   For each key {section}:
 *     for each reference:
 *       Local: if section is empty or matches the key, the call is valid if
 *          if the paragraph name is unique within section:
 *            valid if count == 1
 *       Global: valid if {section,paragraph} is unique in the global map
 *
 * Line numbers are just decoration.
 */

bool
procref_base_t::operator<( const procref_base_t& that ) const {
  int result = strcasecmp(section(), that.section());

  if( result == 0 ) {
    return strcasecmp(paragraph(), that.paragraph()) < 0;
  }
  return result < 0;
}

bool
procref_base_t::operator==( const procref_base_t& that ) const {
  return
    0 == strcasecmp(section(), that.section()) &&
    0 == strcasecmp(paragraph(), that.paragraph());
}

class procdef_t : public procref_base_t {
  size_t isym;
public:
  procdef_t( const char *section, const char *paragraph, size_t isym )
    : procref_base_t(section, paragraph)
    , isym(isym)
  {
    assert(isym);
  }
  procdef_t( const procref_base_t& ref )
    : procref_base_t(ref)
    , isym(0)
  {}

  bool operator<( const procdef_t& that ) const {
    return procref_base_t(*this) < procref_base_t(that);
  }

  bool operator<( const procref_base_t& that ) const {
    if( that.has_section() ) {
      return procref_base_t(*this) < that;
    }
    return strcasecmp(paragraph(), that.paragraph()) < 0;
  }

  cbl_label_t * label_of() const {
    return isym == 0? NULL : cbl_label_of(symbol_at(isym));
  }
};

/*
 * Every reference occurs in a {program,section,paragraph} context,
 * even if they're implicit.
 */

typedef std::multimap<procdef_t, std::list<procref_t>> procedures_t;

static std::map<size_t, procedures_t> programs;
static procedures_t::iterator current_procedure = programs.end()->second.end();

/*
 * If a procedure reference uses only one name, it could refer to a
 * section or paragraph. The "paragraph" name in the reference, if not
 * paired with a section name, might refer to a section.
 *
 * For a 1-name reference:
 *   a global match means the name is defined exactly once
 *   a local  match matches a unique paragraph name in the
 *            section in which the reference occurs, or the section name itself
 *
 * No paragraph can have the same name as a section.
 */
class procedure_match {
  const procref_base_t& ref;
public:
  procedure_match( const procref_base_t& ref ) : ref(ref) {}
  // Match a 2-name reference to section & paragraph, else to one or the other.
  bool operator()( procedures_t::const_reference elem ) {
    const procdef_t& key = elem.first;

    if( ref.has_section() ) return ref == key;

    bool hit =
      (!key.has_paragraph() && 0 == strcasecmp(key.section(), ref.paragraph()))
      ||                       0 == strcasecmp(key.paragraph(), ref.paragraph());
    return hit;
  }
};

static bool
globally_unique( size_t program, const procref_t& ref ) {
  const procedures_t& procedures = programs[program];
  assert(!procedures.empty());
  return 1 == count_if(procedures.begin(), procedures.end(), procedure_match(ref));
}

static bool
locally_unique( size_t program, const procdef_t& key, const procref_t& ref ) {
  const procedures_t& procedures = programs[program];
  assert(!procedures.empty());
  const char *section_name = ref.has_section()? ref.section() : key.section();
  procref_base_t full_ref(section_name, ref.paragraph());

  return 1 == procedures.count(full_ref);
}

// Add each section and paragraph to the map as it occurs in the Cobol text.
void
procedure_definition_add( size_t program, const cbl_label_t *procedure ) {
  const char *section_name = NULL, *paragraph_name = NULL;
  size_t isym = symbol_index(symbol_elem_of(procedure));

  if( procedure->type == LblParagraph ) {
    if( procedure->parent > 0) {
      section_name = cbl_label_of(symbol_at(procedure->parent))->name;
    }
    paragraph_name = procedure->name;

  } else {
    assert( procedure->type == LblSection );
    section_name = procedure->name;
  }

  procdef_t key( section_name, paragraph_name, isym );
  current_procedure =
    programs[program].insert( make_pair(key, procedures_t::mapped_type()) );
}

// Add each procedure reference as it occurs in the Cobol text, in context.
void
procedure_reference_add( const char *section, const char *paragraph,
                         int line, size_t context )
{
  current_procedure->second.push_back( procref_t(section, paragraph,
                                                 line, context) );
}

// Verify each reference in a map element is locally or globally unique
class is_unique {
  size_t program;
  procedures_t::key_type key;
public:
  is_unique( size_t program, const procedures_t::key_type& key )
    : program(program)
    , key(key)
  {}

  bool operator()( procedures_t::mapped_type::const_reference ref ) {
    return
      locally_unique( program, key, ref ) ||
      globally_unique( program, ref);
  }
};

procref_t *
ambiguous_reference( size_t program ) {
  procedures_t& procedures = programs[program];

  for( const auto& proc : procedures ) {
    procedures_t::mapped_type::const_iterator
      ambiguous = find_if_not( proc.second.begin(), proc.second.end(),
                               is_unique(program, proc.first) );
    if( proc.second.end() != ambiguous ) {
      if( yydebug ) {
        dbgmsg("%s: %s of '%s' has %zu potential matches", __func__,
              ambiguous->paragraph(), ambiguous->section(),
              procedures.count(*ambiguous));
      }
      return new procref_t(*ambiguous);
    }
  }
  return NULL;
}

/*
 * See declaratives nonterminal in parse.y
 */
// Todo: unused
cbl_label_t *
intradeclarative_reference() {
  const procedures_t& procedures = programs[current_program_index()];

  for( auto elem : procedures ) {
    procdef_t key( elem.first );
    auto L = key.label_of();
    if( L->type != LblNone ) return L;
  }
  return NULL;
}

class next_group {
  size_t isym;
public:
  next_group( symbol_elem_t *group ) : isym(symbol_index(group)) {}

  // return true if elem is not a member of the group
  bool operator()( const symbol_elem_t& elem ) {
    if( elem.type != SymField ) return false;
    if( symbol_index(&elem) == isym ) return false;
    return cbl_field_of(&elem)->parent < isym;
  }
};

static void
parent_names( const symbol_elem_t *elem,
              const symbol_elem_t *group, std::list<const char *>& names ) {

  if( is_filler(cbl_field_of(elem)) ) return;

  // dbgmsg("%s: asked about %s of %s (%zu away)", __func__,
  //       cbl_field_of(elem)->name,
  //       cbl_field_of(group)->name, elem - group);

  for( const symbol_elem_t *e=elem; e && group < e; e = symbol_parent(e) ) {
    names.push_front( cbl_field_of(e)->name );
  }
}

extern int yylineno;
class find_corresponding {
public:
  enum type_t { arith_op, move_op };
private:
  symbol_elem_t *lgroup, *rgroup;
  type_t type;
public:
  find_corresponding( symbol_elem_t *lgroup,
                      symbol_elem_t *rgroup, type_t type )
    : lgroup(lgroup), rgroup(rgroup), type(type)
  {
    dbgmsg( "%s:%d: for #%zu %s and #%zu %s on line %d", __func__, __LINE__,
             symbol_index(lgroup), cbl_field_of(lgroup)->name,
             symbol_index(rgroup), cbl_field_of(rgroup)->name, yylineno );
  }

  static bool
  any_redefines( const cbl_field_t& field, const symbol_elem_t *group ) {
    for( const cbl_field_t *f = &field; f && f->parent > 0; f = parent_of(f) ) {
      symbol_elem_t *e = symbol_at(f->parent);
      if( e == group || e->type != SymField ) break;
      if( symbol_redefines(f) ) return true;
    }
    return false;
  }

  corresponding_fields_t::value_type
  operator()( const symbol_elem_t& that ) {
    if( &that == lgroup )                  return std::make_pair(0,0);
    if( that.type != SymField )            return std::make_pair(0,0);

    const cbl_field_t& lfield( *cbl_field_of(&that) );

    switch(lfield.level) {
    case 66: case 77: case 88:
      return std::make_pair(0,0);
    default:
      if( any_redefines(lfield, lgroup) ) return std::make_pair(0,0);
      if( is_filler(&lfield) )            return std::make_pair(0,0);
      if( is_table(&lfield) )             return std::make_pair(0,0);
      break;
    }

    std::list<const char *> names;
    parent_names( &that, lgroup, names );
    names.push_front(cbl_field_of(rgroup)->name);

    symbol_elem_t *e = symbol_find_of( that.program, names, symbol_index(rgroup) );
    if( !e ) return std::make_pair(0,0);

    const cbl_field_t& rfield( *cbl_field_of(e) );

    switch(rfield.level) {
    case 66: case 77: case 88:
      return std::make_pair(0,0);
    default:
      if( any_redefines(rfield, rgroup) ) return std::make_pair(0,0);
      if( is_table(&rfield) )             return std::make_pair(0,0);
      break;
    }

    switch(type) {
    case arith_op:
      if( !(is_numeric(lfield.type) && is_numeric(rfield.type)) ) {
        return std::make_pair(0,0);
      }
      break;
    case move_op:
      if( !(is_elementary(lfield.type) || is_elementary(rfield.type)) ) {
        return std::make_pair(0,0);
      }
      break;
    }

    return std::make_pair( symbol_index(&that), symbol_index(e));
  }
};

static corresponding_fields_t
corresponding_fields( cbl_field_t *lhs, cbl_field_t *rhs,
                      find_corresponding::type_t type ) {
  corresponding_fields_t output;
  assert(lhs); assert(rhs);
  assert(lhs->type == FldGroup && rhs->type == FldGroup);

  struct { symbol_elem_t *a, *z; } lhsg;

  lhsg.a = symbols_begin(field_index(lhs));
  lhsg.z = std::find_if( lhsg.a, symbols_end(), next_group(lhsg.a) );

  dbgmsg("%s:%d: examining %zu symbols after %s", __func__, __LINE__,
          lhsg.z - lhsg.a, lhs->name);

  find_corresponding finder( symbol_at(field_index(lhs)),
                             symbol_at(field_index(rhs)), type );
  std::transform( lhsg.a, lhsg.z, std::inserter(output, output.begin()), finder );

  output.erase(0);

  dbgmsg( "%s:%d: %s and %s have %zu corresponding fields",
          __func__, __LINE__, lhs->name, rhs->name, output.size() );

  return output;
}

corresponding_fields_t
corresponding_move_fields( cbl_field_t *lhs, cbl_field_t *rhs ) {
  return corresponding_fields( lhs, rhs, find_corresponding::move_op );
}

corresponding_fields_t
corresponding_arith_fields( cbl_field_t *lhs, cbl_field_t *rhs ) {
  return corresponding_fields( lhs, rhs, find_corresponding::arith_op );
}

char
date_time_fmt( const char input[] ) {
  if( ! input ) return 0;

#define DATE_FMT_B  "(YYYYMMDD|YYYYDDD|YYYYWwwD)"
#define DATE_FMT_E  "(YYYY-MM-DD|YYYY-DDD|YYYY-Www-D)"
#define TIME_FMT1   "hhmmss([.,]s+)?"
#define TIME_FMT3   "hhmmss([.,]s+)?Z"
#define TIME_FMT5   "hhmmss([.,]s+)?[+]hhmm"
#define TIME_FMT2   "hh:mm:ss([.,]s+)?"
#define TIME_FMT4   "hh:mm:ss([.,]s+)?Z"
#define TIME_FMT6   "hh:mm:ss([.,]s+)?[+]hh:mm"

#define TIME_FMT_B  "(" TIME_FMT1 "|" TIME_FMT3 "|"  TIME_FMT5 ")"
#define TIME_FMT_E  "(" TIME_FMT2 "|" TIME_FMT4 "|"  TIME_FMT6 ")"

  static bool compiled = false;
  static struct fmts_t {
    regex_t reg; char type; char pattern[256];
  } fmts[] = {
    { regex_t(), 'D', "^((" DATE_FMT_B "T" TIME_FMT_B ")|("
                            DATE_FMT_E "T" TIME_FMT_E "))$" },
    { regex_t(), 'd', "^(" DATE_FMT_B "|" DATE_FMT_E ")$" },
    { regex_t(), 't', "^(" TIME_FMT_B "|" TIME_FMT_E ")$" },
  };
  int erc, cflags = REG_EXTENDED | REG_ICASE, eflags=0;
  regmatch_t m[5];
  char result = 0;

  if( ! compiled ) {
    for( auto& fmt : fmts ) {
      if( (erc = regcomp(&fmt.reg, fmt.pattern, cflags)) != 0 ) {
        char msg[80];
        regerror(erc, &fmt.reg, msg, sizeof(msg));
        cbl_errx( "%s: regcomp: %s", __func__, msg );
      }
    }
    compiled = true;
  }

  for( auto& fmt : fmts ) {
    if( 0 == regexec(&fmt.reg, input, COUNT_OF(m), m, eflags) ) {
      result = fmt.type;
      break;
    }
  }

  return result;
}



/*
 * Development suppport
 */

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-variable"

struct input_file_t {
  ino_t inode;
  int lineno;
  const char *name;
  const line_map *lines;

  input_file_t( const char *name, ino_t inode,
                int lineno=1, const line_map *lines = NULL )
    : inode(inode), lineno(lineno), name(name), lines(lines)
  {
    if( inode == 0 ) inode_set();
  }
  bool operator==( const input_file_t& that ) const {
    return inode == that.inode;
  }
 protected:
  void inode_set()  {
    struct stat sb;
    if( -1 == stat(name, &sb) ) {
      cbl_err( "could not stat '%s'", name);
    }
    inode = sb.st_ino;
  }
};

class unique_stack : public std::stack<input_file_t>
{
 public:
  bool push( const value_type& value ) {
    auto ok = std::none_of( c.cbegin(), c.cend(),
                            [value]( auto& that ) {
                              return value == that;
                                } );
    if( ok ) {
      std::stack<input_file_t>::push(value);
      return true;
    }
    size_t n = c.size();
    if( n > 1 ) {
      char *wd = get_current_dir_name();
      if( wd ) {
        dbgmsg( "depth line copybook filename\n"
                "----- ---- --------"
                "----------------------------------------");
        for( const auto& v : c ) {
          dbgmsg( " %4zu %4d %s", c.size() - --n, v.lineno, no_wd(wd, v.name) );
        }
      } else {
        dbgmsg("unable to get current working directory: %m");
      }
      free(wd);
    }
    return false;
  }
  const char *
  no_wd( const char *wd, const char *name ) {
    int i;
    for( i=0; wd[i] == name[i]; i++ ) i++;
    if( wd[i] == '\0' && name[i] == '/' ) i++;
    return yydebug? name : name + i;
  }
};

static const char *input_filename_vestige;
static unique_stack input_filenames;
static std::map<std::string, ino_t> old_filenames;
static const unsigned int sysp = 0;  // not a C header file, cf. line-map.h

/*
 * Maintain a stack of input filenames.  Ensure the files are unique (by
 * inode), to prevent copybook cycles. Before pushing a new name, Record the
 * line number that was is current for the current name, so that it can be
 * restored when the usurper is popped.
 *
 * Both the file-reader (lexio) and the scanner use this stack.  Lexio uses it
 * to enforce uniqueness, and the scanner to maintain line numbers.
 */
bool cobol_filename( const char *name, ino_t inode ) {
  line_map *lines = NULL;
  if( inode == 0 ) {
    auto p = old_filenames.find(name);
    if( p == old_filenames.end() ) {
      for( auto& elem : old_filenames ) {
        dbgmsg("%6zu %-30s", elem.second, elem.first.c_str());
      }
      cbl_errx( "logic error: missing inode for %s", name);
    }
    inode = p->second;
    assert(inode != 0);
  }
  linemap_add(line_table, LC_ENTER, sysp, name, 1);
  input_filename_vestige = name;
  bool pushed = input_filenames.push( input_file_t(name, inode, 1, lines) );
  input_filenames.top().lineno = yylineno = 1;
  return pushed;
}

const char *
cobol_lineno_save() {
  if( input_filenames.empty() ) return NULL;
  auto& input( input_filenames.top() );
  input.lineno = yylineno;
  return input.name;
}

const char *
cobol_filename() {
  return input_filenames.empty()? input_filename_vestige : input_filenames.top().name;
}

const char *
cobol_filename_restore() {
  assert(!input_filenames.empty());
  const input_file_t& top( input_filenames.top() );
  old_filenames[top.name] = top.inode;
  input_filename_vestige = top.name;

  input_filenames.pop();
  if( input_filenames.empty() ) return NULL;

  auto& input = input_filenames.top();

  input.lines = linemap_add(line_table, LC_LEAVE, sysp, NULL, 0);

  yylineno = input.lineno;
  return input.name;
}

static location_t token_location;

template <typename LOC>
static void
gcc_location_set_impl( const LOC& loc ) {
  token_location = linemap_line_start( line_table, loc.last_line, 80 );
  token_location = linemap_position_for_column( line_table, loc.first_column);
  location_dump(__func__, __LINE__, "parser", loc);
}

void gcc_location_set( const YYLTYPE& loc ) {
  gcc_location_set_impl(loc);
}

void gcc_location_set( const YDFLTYPE& loc ) {
  gcc_location_set_impl(loc);
}

#ifdef NDEBUG
# define verify_format(M)
#else
#include <regex.h>

static void
verify_format( const char gmsgid[] ) {
  static const char pattern[] = "%[[:digit:]][[:digit:].]*[^s]";
  static regex_t re;
  static int cflags = REG_EXTENDED;
  static int status = regcomp( &re, pattern, cflags );
  static char errbuf[80];



  if( status != 0 ) {
    int n = regerror(status, &re, errbuf, sizeof(errbuf));
    gcc_assert(size_t(n) < sizeof(errbuf));
    fprintf(stderr, "%s:%d: %s", __func__, __LINE__, errbuf);
    return;
  }
  gcc_assert(status == 0);

  regmatch_t rm[30];

  if( REG_NOMATCH != regexec(&re, gmsgid, COUNT_OF(rm), rm, 0) ){
    fprintf(stderr, "bad diagnositic format: '%s'\n", gmsgid);
  }
}
#endif

static const diagnostic_option_id option_zero;
size_t parse_error_inc();

void
ydferror( const char gmsgid[], ... ) {
  verify_format(gmsgid);
  parse_error_inc();
  auto_diagnostic_group d;
  va_list ap;
  va_start (ap, gmsgid);
  rich_location richloc (line_table, token_location);
  bool ret = global_dc->diagnostic_impl (&richloc, nullptr, option_zero,
                                         gmsgid, &ap, DK_ERROR);
  va_end (ap);
}

extern int yychar;
extern YYLTYPE yylloc;

/*
 * temp_loc_t is a hack in lieu of "%define parse.error custom".  When
 * instantiated, if there is a lookahead token (or one is provided), it sets
 * the global token_location, which is passed to the diagnostic framework. The
 * original value is restored when the instantiated variable goes out of scope.
 */
class temp_loc_t : protected YYLTYPE {
  location_t orig;
 public:
  temp_loc_t() : orig(token_location) {
    if( yychar < 3 ) return;

    gcc_location_set(yylloc); // use lookahead location
  }
  temp_loc_t( const YYLTYPE& loc) : orig(token_location) {
    gcc_location_set(loc);
  }
  temp_loc_t( const YDFLTYPE& loc) : orig(token_location) {
    YYLTYPE lloc = {
      loc.first_line, loc.first_column,
      loc.last_line,  loc.last_column };
    gcc_location_set(lloc);
  }
  ~temp_loc_t() {
    if( orig != token_location ) {
      token_location = orig;
    }
  }
};

/*
 * Both CDF and parser need to call error_msg, each with their own distinct
 * location type, not because they *need* to be different, but because they
 * are, as an artifact of using different prefixes.  Possibly a better plan
 * would be to convert cdf.y to a pure parser, using no global variables.  But
 * this is where we are.
 *
 * Because we can't reliably instantiate it as a forward-declared template
 * function, and because the paramters are variadic, we can't use a template
 * function or call one.  So, a macro.
 */

#define ERROR_MSG_BODY                                                  \
  temp_loc_t looker(loc);                                               \
  verify_format(gmsgid);                                                \
  parse_error_inc();                                                    \
  global_dc->begin_group();                                             \
  va_list ap;                                                           \
  va_start (ap, gmsgid);                                                \
  rich_location richloc (line_table, token_location);                   \
  bool ret = global_dc->diagnostic_impl (&richloc, nullptr, option_zero,        \
                                         gmsgid, &ap, DK_ERROR);        \
  va_end (ap);                                                          \
  global_dc->end_group();


void error_msg( const YYLTYPE& loc, const char gmsgid[], ... ) {
  ERROR_MSG_BODY
}

void error_msg( const YDFLTYPE& loc, const char gmsgid[], ... ) {
  ERROR_MSG_BODY
}

void
cdf_location_set(YYLTYPE loc) {
  extern YDFLTYPE ydflloc;

  ydflloc.first_line =   loc.first_line;
  ydflloc.first_column = loc.first_column;
  ydflloc.last_line =    loc.last_line;
  ydflloc.last_column =  loc.last_column;
}

void
yyerror( const char gmsgid[], ... ) {
  temp_loc_t looker;
  verify_format(gmsgid);
  parse_error_inc();
  global_dc->begin_group();
  va_list ap;
  va_start (ap, gmsgid);
  rich_location richloc (line_table, token_location);
  bool ret = global_dc->diagnostic_impl (&richloc, nullptr, option_zero,
                                         gmsgid, &ap, DK_ERROR);
  va_end (ap);
  global_dc->end_group();
}

bool
yywarn( const char gmsgid[], ... ) {
  verify_format(gmsgid);
  auto_diagnostic_group d;
  va_list ap;
  va_start (ap, gmsgid);
  auto ret = emit_diagnostic_valist( DK_WARNING, token_location,
                                     option_zero, gmsgid, &ap );
  va_end (ap);
  return ret;
}

/*
 * Sometimes during parsing an error is noticed late. This message refers back
 * to an arbitrary file and line number.
 */
void
yyerrorvl( int line, const char *filename, const char fmt[], ... ) {
  verify_format(fmt);
  parse_error_inc();
  auto_diagnostic_group d; // not needed unless we can use global_dc
  char *msg;
  va_list ap;

  va_start(ap, fmt);
  msg = xvasprintf(fmt, ap);

  if( !filename ) filename = cobol_filename();

  fprintf( stderr, "%s:%d: %s\n", filename, line, msg);

  free(msg);
  va_end(ap);
}

static inline size_t
matched_length( const regmatch_t& rm ) { return rm.rm_eo - rm.rm_so; }

const char *
cobol_fileline_set( const char line[] ) {
  static const char pattern[] = "#line +([[:alnum:]]+) +[\"']([^\"']+). *\n";
  static const int cflags = REG_EXTENDED | REG_ICASE;
  static regex_t re, *preg = NULL;

  int erc;
  regmatch_t pmatch[4];

  if( !preg ) {
    if( (erc = regcomp(&re, pattern, cflags)) != 0 ) {
        regerror(erc, &re, regexmsg, sizeof(regexmsg));
        dbgmsg( "%s:%d: could not compile regex: %s", __func__, __LINE__, regexmsg );
        return line;
    }
    preg = &re;
  }
  if( (erc = regexec(preg, line, COUNT_OF(pmatch), pmatch, 0)) != 0 ) {
    if( erc != REG_NOMATCH ) {
      regerror(erc, preg, regexmsg, sizeof(regexmsg));
      dbgmsg( "%s:%d: could not compile regex: %s", __func__, __LINE__, regexmsg );
      return line;
    }
    error_msg(yylloc, "invalid #line directive: %s", line );
    return line;
  }

  const char
    *line_str = xstrndup(line + pmatch[1].rm_so, matched_length(pmatch[1])),
    *filename = xstrndup(line + pmatch[2].rm_so, matched_length(pmatch[2]));
  int fileline;

  if( 1 != sscanf(line_str, "%d", &fileline) )
    yywarn("could not parse line number %s from #line directive", line_str);

  input_file_t input_file( filename, ino_t(0), fileline ); // constructor sets inode

  if( input_filenames.empty() ) {
    input_file.lines = linemap_add(line_table, LC_ENTER, sysp, filename, 1);
    input_filenames.push(input_file);
  }

  input_file_t& file = input_filenames.top();
  file = input_file;
  yylineno = file.lineno;

  return file.name;
}

class cbl_timespec {
  struct timespec now;
 public:
  cbl_timespec() {
    clock_gettime(CLOCK_MONOTONIC, &now);
  }
  double ns() const {
    return now.tv_sec * 1000000000 + now.tv_nsec;
  }
  friend double operator-( const cbl_timespec& now, const cbl_timespec& then );
};

double
operator-( const cbl_timespec& then, const cbl_timespec& now ) {
  return (now.ns() - then.ns()) / 1000000000;
}

static int
parse_file( const char filename[] )
{
  if( (yyin = cdftext::lex_open(filename)) == NULL) {
    cbl_err("cannot open %s", filename);
  }

  parser_enter_file(filename);

  cbl_timespec start;

  int erc = yyparse();

  cbl_timespec finish;
  double dt  = finish - start;
  parser_leave_file();

  //printf("Overall parse & generate time is %.6f seconds\n", dt);

  fclose (yyin);

  if( erc ) {
    error_at (UNKNOWN_LOCATION, "failed compiling %s", filename);
  }

  return erc;
}

#pragma GCC diagnostic pop

extern int yy_flex_debug, yydebug, ydfdebug;
extern int f_trace_debug;

void cobol_set_indicator_column( int column );

void
cobol_set_debugging( bool flex, bool yacc, bool parser )
{
  yy_flex_debug = flex? 1 : 0;
  ydfdebug = yydebug = yacc? 1 : 0;
  f_trace_debug = parser? 1 : 0;

  char *ind = getenv("INDICATOR_COLUMN");
  if( ind ) {
    int col;
    if( 1 != sscanf(ind, "%d", &col) ) {
      yywarn("ignored non-integer value for INDICATOR_COLUMN=%s", ind);
    }
    cobol_set_indicator_column(col);
  }
}

os_locale_t os_locale = { "UTF-8", xstrdup("C.UTF-8") };


void
cobol_parse_files (int nfile, const char **files)
{
  char * opaque = setlocale(LC_CTYPE, "");
  if( ! opaque ) {
    yywarn("setlocale: unable to initialize LOCALE");
  } else {
    char *codeset = nl_langinfo(CODESET);
    if( ! codeset ) {
      yywarn("nl_langinfo failed after setlocale succeeded");
    } else {
      os_locale.codeset = codeset;
    }
  }
  assert(os_locale.codeset);

  for (int i = 0; i < nfile; i++) {
    parse_file (files[i]);
  }
}

/*  Outputs the formatted string onto the file descriptor */

void
cbl_message(int fd, const char *format_string, ...)
  {
  va_list ap;
  va_start(ap, format_string);
  char *ostring = xvasprintf(format_string, ap);
  va_end(ap);
  write(fd, ostring, strlen(ostring));
  free(ostring);
  }

/*  Uses the GCC internal_error () to output the formatted string.  Processing
    ends with a stack trace  */

void
cbl_internal_error(const char *gmsgid, ...) {
  verify_format(gmsgid);
  auto_diagnostic_group d;
  va_list ap;
  va_start(ap, gmsgid);
  emit_diagnostic_valist( DK_ICE, token_location, option_zero, gmsgid, &ap );
  va_end(ap);
}

void
cbl_unimplementedw(const char *gmsgid, ...) {
  verify_format(gmsgid);
  auto_diagnostic_group d;
  va_list ap;
  va_start(ap, gmsgid);
  emit_diagnostic_valist( DK_SORRY, token_location, option_zero, gmsgid, &ap );
  va_end(ap);
}

void
cbl_unimplemented(const char *gmsgid, ...) {
  verify_format(gmsgid);
  auto_diagnostic_group d;
  va_list ap;
  va_start(ap, gmsgid);
  emit_diagnostic_valist( DK_SORRY, token_location, option_zero, gmsgid, &ap );
  va_end(ap);
}

void
cbl_unimplemented_at( const YYLTYPE& loc, const char *gmsgid, ... ) {
  temp_loc_t looker(loc);
  verify_format(gmsgid);
  auto_diagnostic_group d;
  va_list ap;
  va_start(ap, gmsgid);
  emit_diagnostic_valist( DK_SORRY, token_location, option_zero, gmsgid, &ap );
  va_end(ap);
}

/* 
 * analogs to err(3) and errx(3). 
 */

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wformat"
void
cbl_err(const char *fmt, ...) {
  auto_diagnostic_group d;
  char *gmsgid = xasprintf("%m: %s", fmt);
  verify_format(gmsgid);
  va_list ap;
  va_start(ap, fmt);
  emit_diagnostic_valist( DK_FATAL, token_location, option_zero, gmsgid, &ap );
  va_end(ap);
}
#pragma GCC diagnostic pop

void
cbl_errx(const char *gmsgid, ...) {
  verify_format(gmsgid);
  auto_diagnostic_group d;
  va_list ap;
  va_start(ap, gmsgid);
  emit_diagnostic_valist( DK_FATAL, token_location, option_zero, gmsgid, &ap );
  va_end(ap);
  }

void
dbgmsg(const char *msg, ...) {
  if( yy_flex_debug || yydebug ) {
    fflush(stdout);
    va_list ap;
    va_start(ap, msg);
    vfprintf(stderr, msg, ap);
     fprintf(stderr, "\n");
    va_end(ap);
  }
}

void
dialect_error( const YYLTYPE& loc, const char term[], const char dialect[] ) {
  error_msg(loc, "%s is not ISO syntax, requires -dialect %s",
           term, dialect);
}

bool fisdigit(int c)
  {
  return ISDIGIT(c);
  }
bool fisspace(int c)
  {
  return ISSPACE(c);
  };
int  ftolower(int c)
  {
  return TOLOWER(c);
  }
bool fisprint(int c)
  {
  return ISPRINT(c);
  };
