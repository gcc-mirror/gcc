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
// This file is included only by parse.y

#include <map>

/*
 * Intrinsics
 * types are:
 *   A Alphabetic
 *   D DBCS
 *   I Integer
 *   K Keyword
 *   N Numeric
 *   O Other
 *   U National
 *   8 UTF-8
 *   X Alphanumeric
 *   n variadic
 * We use just A, I, N, or X, choosing the most general for each parameter.
 */
static const function_descr_t function_descrs[] = {
   {         ABS,                          "ABS",
      "__gg__abs",                         "N",   {}, FldNumericBin5 },
   {         ACOS,                         "ACOS",
      "__gg__acos",                        "N",   {}, FldNumericBin5 },
   {         ANNUITY,                      "ANNUITY",
      "__gg__annuity",                     "NI",  {}, FldNumericBin5 },
   {         ASIN,                         "ASIN",
      "__gg__asin",                        "N",   {}, FldNumericBin5 },
   {         ATAN,                         "ATAN",
      "__gg__atan",                        "N",   {}, FldNumericBin5 },
   {         BASECONVERT,                         "BASECONVERT",
      "__gg__baseconvert",                        "XII",   {}, FldNumericBin5 },
   {         BIT_OF,                       "BIT-OF",
      "__gg__bit_of",                      "X",   {}, FldAlphanumeric },
   {         BIT_TO_CHAR,                  "BIT-TO-CHAR",
      "__gg__bit_to_char",                 "X",   {}, FldAlphanumeric },
   // BOOLEAN-OF-INTEGER requires FldBoolean
   {         BOOLEAN_OF_INTEGER,                         "BOOLEAN-OF-INTEGER",
      "__gg__boolean_of_integer",                        "II",   {}, FldNumericBin5 },
   {         BYTE_LENGTH,                  "BYTE-LENGTH",
       "__gg__byte_length",                "X",   {}, FldNumericBin5 },
   {         CHAR,                         "CHAR",
      "__gg__char",                        "I",   {}, FldAlphanumeric },
   {         CHAR_NATIONAL,                         "CHAR-NATIONAL",
      "__gg__char_national",                        "I",   {}, FldAlphanumeric },
   {         COMBINED_DATETIME,            "COMBINED-DATETIME",
      "__gg__combined_datetime",           "IN",  {}, FldNumericBin5 },
   {         CONCAT,                       "CONCAT",
      "__gg__concat",                      "n",   {}, FldAlphanumeric },
   {         CONVERT,                          "CONVERT",
      "__gg__convert",                         "XII",   {}, FldAlphanumeric },
   {         COS,                          "COS",
      "__gg__cos",                         "N",   {}, FldNumericBin5 },
   {         CURRENT_DATE,                 "CURRENT-DATE",
      "__gg__current_date",                 "",   {}, FldAlphanumeric },
   {         DATE_OF_INTEGER,              "DATE-OF-INTEGER",
      "__gg__date_of_integer",             "I",   {}, FldNumericBin5 },
   {         DATE_TO_YYYYMMDD,             "DATE-TO-YYYYMMDD",
      "__gg__date_to_yyyymmdd",            "III",  {}, FldNumericBin5 },
   {         DAY_OF_INTEGER,               "DAY-OF-INTEGER",
      "__gg__day_of_integer",              "I",   {}, FldNumericBin5 },
   {         DAY_TO_YYYYDDD,               "DAY-TO-YYYYDDD",
      "__gg__day_to_yyyyddd",              "III",  {}, FldNumericBin5 },
   {         DISPLAY_OF,                   "DISPLAY-OF",
      "__gg__display_of",                  "UUI", {}, FldAlphanumeric },
   {         E,                            "E",
      "__gg_e",                            "",   {}, FldNumericBin5 },

   {         EXCEPTION_FILE,               "EXCEPTION-FILE",
      "__gg__func_exception_file",         "",   {}, FldAlphanumeric },
   {         EXCEPTION_FILE_N,             "EXCEPTION-FILE-N",
      "__gg__func_exception_file_n",       "",   {}, FldAlphanumeric },
   {         EXCEPTION_LOCATION,           "EXCEPTION-LOCATION",
      "__gg__func_exception_location",     "",   {}, FldAlphanumeric },
   {         EXCEPTION_LOCATION_N,         "EXCEPTION-LOCATION-N",
      "__gg__func_exception_location_n",   "",   {}, FldAlphanumeric },
   {         EXCEPTION_STATEMENT,          "EXCEPTION-STATEMENT",
      "__gg__func_exception_statement",    "",   {}, FldAlphanumeric },
   {         EXCEPTION_STATUS,             "EXCEPTION-STATUS",
      "__gg__func_exception_status",       "",   {}, FldAlphanumeric },

   {         EXP,                          "EXP",
      "__gg__exp",                         "N",   {}, FldNumericBin5 },
   {         EXP10,                        "EXP10",
      "__gg__exp10",                       "N",   {}, FldNumericBin5 },
   {         FACTORIAL,                    "FACTORIAL",
      "__gg__factorial",                   "I",   {}, FldNumericBin5 },
   {         FIND_STRING,                    "FIND-STRING",
      "__gg__find_string",                   "AXI",   {}, FldNumericBin5 },
   {         FORMATTED_CURRENT_DATE,       "FORMATTED-CURRENT-DATE",
      "__gg__formatted_current_date",      "X",   {}, FldAlphanumeric },
   {         FORMATTED_DATE,               "FORMATTED-DATE",
      "__gg__formatted_date",              "XX",  {}, FldAlphanumeric },
   {         FORMATTED_DATETIME,           "FORMATTED-DATETIME",
      "__gg__formatted_datetime",          "XINI", {}, FldAlphanumeric },
   {         FORMATTED_TIME,               "FORMATTED-TIME",
      "__gg__formatted_time",              "INI", {}, FldNumericBin5 },
   {         FRACTION_PART,                "FRACTION-PART",
      "__gg__fraction_part",               "N",   {}, FldNumericBin5 },
   {         HEX_OF,                       "HEX-OF",
      "__gg__hex_of",                      "X",   {}, FldAlphanumeric },
   {         HEX_TO_CHAR,                  "HEX-TO-CHAR",
      "__gg__hex_to_char",                 "X",   {}, FldAlphanumeric },
   {         HIGHEST_ALGEBRAIC,            "HIGHEST-ALGEBRAIC",
      "__gg__highest_algebraic",           "N",   {}, FldNumericBin5 },
   {         INTEGER,                      "INTEGER",
      "__gg__integer",                     "N",   {}, FldNumericBin5 },
   // requires FldBoolean
   {         INTEGER_OF_BOOLEAN,                      "INTEGER-OF-BOOLEAN",
      "__gg__integer_of_boolean",                     "B",   {}, FldNumericBin5 },
   {         INTEGER_OF_DATE,              "INTEGER-OF-DATE",
      "__gg__integer_of_date",             "I",   {}, FldNumericBin5 },
   {         INTEGER_OF_DAY,               "INTEGER-OF-DAY",
      "__gg__integer_of_day",              "I",   {}, FldNumericBin5 },
   {         INTEGER_OF_FORMATTED_DATE,    "INTEGER-OF-FORMATTED-DATE",
      "__gg__integer_of_formatted_date",   "XX",  {}, FldAlphanumeric },
   {         INTEGER_PART,                 "INTEGER-PART",
      "__gg__integer_part",                "N",   {}, FldNumericBin5 },
   {         LENGTH,                       "LENGTH",
      "__gg__length",                      "X",   {}, FldNumericBin5 },
   {         LOCALE_COMPARE,               "LOCALE-COMPARE",
      "__gg__locale_compare",              "XXX",   {}, FldNumericBin5 },
   {         LOCALE_DATE,                  "LOCALE-DATE",
      "__gg__locale_date",                 "XX",   {}, FldNumericBin5 },
   {         LOCALE_TIME,                  "LOCALE-TIME",
      "__gg__locale_time",                 "XX",   {}, FldNumericBin5 },
   {         LOCALE_TIME_FROM_SECONDS,     "LOCALE-TIME-FROM-SECONDS",
      "__gg__locale_time_from_seconds",    "NX",   {}, FldNumericBin5 },

   {         LOG,                          "LOG",
      "__gg__log",                         "N",   {}, FldNumericBin5 },
   {         LOG10,                        "LOG10",
      "__gg__log10",                       "N",   {}, FldNumericBin5 },
   {         LOWER_CASE,                   "LOWER-CASE",
      "__gg__lower_case",                  "X",   {}, FldAlphanumeric },
   {         LOWEST_ALGEBRAIC,             "LOWEST-ALGEBRAIC",
      "__gg__lowest_algebraic",            "N",   {}, FldNumericBin5 },

   {         MAXX,                          "MAX",
      "__gg__max",                         "n",   {}, FldAlphanumeric },
   {         MEAN,                         "MEAN",
      "__gg__mean",                        "n",   {}, FldNumericBin5 },
   {         MEDIAN,                       "MEDIAN",
      "__gg__median",                      "n",   {}, FldNumericBin5 },
   {         MIDRANGE,                     "MIDRANGE",
      "__gg__midrange",                    "n",   {}, FldNumericBin5 },
   {         MINN,                          "MIN",
      "__gg__min",                         "n",   {}, FldAlphanumeric },
   {         MOD,                          "MOD",
      "__gg__mod",                         "IN",  {}, FldNumericBin5 },
   {         MODULE_NAME,                          "MODULE-NAME",
      "__gg__module_name",                         "I",  {}, FldAlphanumeric },
   {         NATIONAL_OF,                  "NATIONAL-OF",
      "__gg__national_of",                 "XX",  {}, FldAlphanumeric },
   {         NUMVAL,                       "NUMVAL",
      "__gg__numval",                      "X",   {}, FldNumericBin5 },
   {         NUMVAL_C,                     "NUMVAL-C",
      "__gg__numval_c",                    "XXU", {}, FldNumericBin5 },
   {         NUMVAL_F,                     "NUMVAL-F",
      "__gg__numval_f",                    "X",   {}, FldNumericBin5 },
   {         ORD,                          "ORD",
      "__gg__ord",                         "X",   {}, FldNumericBin5 },
   {         ORD_MAX,                      "ORD-MAX",
      "__gg__ord_max",                     "n",   {}, FldNumericBin5 },
   {         ORD_MIN,                      "ORD-MIN",
      "__gg__ord_min",                     "n",   {}, FldNumericBin5    },
   {         PI,                           "PI",
      "__gg__pi",                          "",   {}, FldNumericBin5 },
   {         PRESENT_VALUE,                "PRESENT-VALUE",
      "__gg__present_value",               "n",   {}, FldNumericBin5    },
   {         RANDOM,                       "RANDOM",
      "__gg__random",                      "I",   {}, FldNumericBin5 },
   {         RANGE,                        "RANGE",
      "__gg__range",                       "n",   {}, FldNumericBin5    },
   {         REM,                          "REM",
      "__gg__rem",                         "NN",  {}, FldNumericBin5 },
   {         REVERSE,                      "REVERSE",
      "__gg__reverse",                     "X",   {}, FldAlphanumeric },
   {         SECONDS_FROM_FORMATTED_TIME,  "SECONDS-FROM-FORMATTED-TIME",
      "__gg__seconds_from_formatted_time", "XX",  {}, FldAlphanumeric },
   {         SECONDS_PAST_MIDNIGHT,        "SECONDS_PAST_MIDNIGHT",
      "__gg__seconds_past_midnight",       "",    {}, FldAlphanumeric },
   {         SIGN,                         "SIGN",
      "__gg__sign",                        "N",   {}, FldNumericBin5 },
   {         SIN,                          "SIN",
      "__gg__sin",                         "N",   {}, FldNumericBin5 },
   {         SMALLEST_ALGEBRAIC,                          "SMALLEST-ALGEBRAIC",
      "__gg__smallest_algebraic",                         "N",   {}, FldNumericBin5 },
   {         SQRT,                         "SQRT",
      "__gg__sqrt",                        "N",   {}, FldNumericBin5 },
   {         STANDARD_COMPARE,                          "STANDARD-COMPARE",
      "__gg__standard_compare",                         "XXXI",   {}, FldAlphanumeric },
   {         STANDARD_DEVIATION,           "STANDARD-DEVIATION",
      "__gg__standard_deviation",          "n",   {}, FldNumericBin5 },
   {         SUBSTITUTE,                          "SUBSTITUTE",
      "__gg__substitute",                         "XXX",   {}, FldAlphanumeric },
   {         SUM,                          "SUM",
      "__gg__sum",                         "n",   {}, FldNumericBin5    },
   {         TAN,                          "TAN",
      "__gg__tan",                         "N",   {}, FldNumericBin5 },
   {         TEST_DATE_YYYYMMDD,           "TEST-DATE-YYYYMMDD",
      "__gg__test_date_yyyymmdd",          "I",   {}, FldNumericBin5 },
   {         TEST_DAY_YYYYDDD,             "TEST-DAY-YYYYDDD",
      "__gg__test_day_yyyyddd",            "I",   {}, FldNumericBin5 },
   {         TEST_FORMATTED_DATETIME,      "TEST-FORMATTED-DATETIME",
      "__gg__test_formatted_datetime",     "XX",  {}, FldNumericBin5 },
   {         TEST_NUMVAL,                  "TEST-NUMVAL",
      "__gg__test_numval",                 "X",   {}, FldNumericBin5 },
   {         TEST_NUMVAL_C,                "TEST-NUMVAL-C",
      "__gg__test_numval_c",               "XXU", {}, FldNumericBin5 },
   {         TEST_NUMVAL_F,                "TEST-NUMVAL-F",
      "__gg__test_numval_f",               "X",   {}, FldNumericBin5 },
   {         TRIM,                         "TRIM",
      "__gg__trim",                        "XI",  {}, FldNumericBin5 },
   {         ULENGTH,                      "ULENGTH",
      "__gg__ulength",                     "X",   {}, FldAlphanumeric },
   {         UPOS,                         "UPOS",
      "__gg__upos",                        "XI",  {}, FldAlphanumeric },
   {         UPPER_CASE,                   "UPPER-CASE",
      "__gg__upper_case",                  "X",   {}, FldAlphanumeric },
   {         USUBSTR,                      "USUBSTR",
      "__gg__usubstr",                     "XII", {}, FldAlphanumeric },
   {         USUPPLEMENTARY,               "USUPPLEMENTARY",
      "__gg__usupplementary",              "X",   {}, FldAlphanumeric },
   {         UUID4,                        "UUID4",
       "__gg_uuid4",                        "",   {}, FldAlphanumeric },
   {         UVALID,                       "UVALID",
      "__gg__uvalid",                      "X",   {}, FldAlphanumeric },
   {         UWIDTH,                       "UWIDTH",
      "__gg__uwidth",                      "XI",  {}, FldAlphanumeric },
   {         VARIANCE,                     "VARIANCE",
      "__gg__variance",                    "n",   {}, FldNumericBin5 },
   {         WHEN_COMPILED,                "WHEN-COMPILED",
      "__gg__when_compiled",               "",    {}, FldAlphanumeric },
   {         YEAR_TO_YYYY,                 "YEAR-TO-YYYY",
      "__gg__year_to_yyyy",                "III",  {}, FldNumericBin5 },
  };

static const
function_descr_t *function_descrs_end = function_descrs + COUNT_OF(function_descrs);

class cname_cmp {
  const char *cname;
 public:
  explicit cname_cmp( const char *cname ) : cname(cname) {}

  bool operator()( const function_descr_t& descr ) {
    return strlen(cname) == strlen(descr.cname) &&
      0 == strcmp(cname, descr.cname);
  }
  bool operator()( const char that[] ) {
    return strlen(cname) == strlen(that) &&
      0 == strcmp(cname, that);
  }
};

static int
intrinsic_token_of( const char name[] ) {
  auto pdescr = std::find_if( function_descrs, function_descrs_end,
                              [name]( const function_descr_t& descr ) {
                                return 0 == strcmp(name, descr.name);
                              } );
  return pdescr == function_descrs_end? 0 : pdescr->token;
}

/*
 * For variadic intrinsic functions, ensure all parameters are commensurate.
 * Return pointer in 1st inconsistent parameter type.
 * Return NULL to indicate success.
 */
static cbl_refer_t *
intrinsic_inconsistent_parameter( size_t n, cbl_refer_t *args ) {
  class commensurate_type {
    cbl_refer_t first;
   public:
    explicit commensurate_type( const cbl_refer_t& first ) : first(first) {}
    bool operator()( const cbl_refer_t& arg )  const {
      return is_numeric(first.field) == is_numeric(arg.field);
    }
  };

  auto p = std::find_if_not(args, args + n, commensurate_type(args[0]));
  return p == args + n? NULL : p;
}

static cbl_field_type_t
intrinsic_return_type( int token ) {
  auto p = std::find_if( function_descrs,
                         function_descrs_end,
                         [token]( const auto& descr ) {
                           return token == descr.token;
                         } );
  return p == function_descrs_end? FldAlphanumeric : p->ret_type;
}

static const char *
intrinsic_cname( int token ) {
  auto p = std::find_if( function_descrs,
                         function_descrs_end,
                         [token]( const auto& descr ) {
                           return token == descr.token;
                         } );
  return p == function_descrs_end?  NULL :  p->cname;
}

const char *
intrinsic_function_name( int token ) {
  auto p = std::find_if( function_descrs,
                         function_descrs_end,
                         [token]( const auto& descr ) {
                           return token == descr.token;
                         } );
  return p == function_descrs_end?  NULL :  p->name;
}

/*
 * Provide supplied function parameters.
 * Return index to 1st invalid parameter type.
 * Return N to indicate success.
 */
static size_t
intrinsic_invalid_parameter( int token,
                             const std::vector<cbl_refer_t>& args )
{
  auto p = std::find_if( function_descrs,
                         function_descrs_end,
                         [token]( const auto& descr ) {
                           return token == descr.token;
                         } );
  if( p == function_descrs_end ) {
    cbl_internal_error( "%s: intrinsic function %qs not found",
                        __func__, keyword_str(token) );
  }

  gcc_assert(!args.empty());
  gcc_assert(p < function_descrs_end);

  const function_descr_t& descr = *p;

  size_t i = 0;
  for( auto arg : args ) {
    if( arg.field == NULL ) {
      i++;
      continue;
    }
    assert(i < strlen(descr.types));

    switch(descr.types[i]) {
    case 'A' : //Alphabetic
    case 'I' : //Integer
    case 'N' : //Numeric
    case 'X' : //Alphanumeric
      break;
    case 'n' : //variadic
      return args.size();
      break;
    case 'D' : //DBCS
    case 'K' : //Keyword
    case 'O' : //Other
    case 'U' : //National
    case '8' : //UTF-8
    default:
      cbl_internal_error( "%s: invalid function descr type '%c'",
           __func__, descr.types[i]);
    }

    static std::map<char, const char*> typenames
      {
        { 'A', "Alphabetic" },
        { 'I', "Integer" },
        { 'N', "Numeric" },
        { 'X', "Alphanumeric" },        
      };

    switch( arg.field->type ) {
    case FldInvalid:
    case FldClass:
    case FldConditional:
    case FldForward:
    case FldIndex:
      yyerror("%s: field '%s' (%s) invalid for %s parameter",
               descr.name,
               arg.field->name, cbl_field_type_str(arg.field->type),
               typenames[descr.types[i]]);
      return i;
      break;
    case FldGroup:
    default:
      break;
    }

    if( is_numeric(arg.field) || is_integer_literal(arg.field)) {
      if( strchr("A", descr.types[i]) != NULL ) {
        yyerror("%s: numeric field '%s' (%s) invalid for %s parameter",
                 descr.name,
                 arg.field->name, cbl_field_type_str(arg.field->type),
                 typenames[descr.types[i]]);
        return i;
      }
    } else { // string field
      if( strchr("IN", descr.types[i]) != NULL ) {
        if( data_category_of(arg.field) == data_alphabetic_e ) {
          yyerror("%s: non-numeric field '%s' (%s) invalid for %s parameter",
                   descr.name,
                   arg.field->name, cbl_field_type_str(arg.field->type),
                   typenames[descr.types[i]]);
          return i;
        }
      }
    }
    i++;
  } // end loop

  return args.size();
}

/*
 * Functions used by code gen
 */

size_t
intrinsic_parameter_count( const char cname[] ) {
  const function_descr_t *descr = std::find_if(function_descrs,
                                               function_descrs_end, cname_cmp(cname));
  return descr == function_descrs_end || descr->types[0] == 'n'?
    size_t(-1) : strlen(descr->types);
}

#if 0
static int
yyreport_syntax_error (const yypcontext_t *ctx)
{
  int res = 0;
  YYLOCATION_PRINT (stderr, yypcontext_location (ctx));
  fprintf (stderr, ": syntax error");
  // Report the tokens expected at this point.
  {
    enum { TOKENMAX = 5 };
    yysymbol_kind_t expected[TOKENMAX];
    int n = yypcontext_expected_tokens (ctx, expected, TOKENMAX);
    if (n < 0)
      // Forward errors to yyparse.
      res = n;
    else
      for (int i = 0; i < n; ++i)
        fprintf (stderr, "%s %s",
                 i == 0 ? ": expected" : " or", yysymbol_name (expected[i]));
  }
  // Report the unexpected token.
  {
    yysymbol_kind_t lookahead = yypcontext_token (ctx);
    if (lookahead != YYSYMBOL_YYEMPTY)
      fprintf (stderr, " before %s", yysymbol_name (lookahead));
  }
  fprintf (stderr, "\n");
  return res;
}
#endif
