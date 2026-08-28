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

%{

#include <cassert>
#include <cctype>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>

#include <algorithm>
#include <deque>
#include <list>
#include <map>
#include <iostream>
#include <string>
#include <vector>

#include <unistd.h>

#define HAVE_DECL_BASENAME 1
#include <libiberty.h>

#define CDF_Y

#include "cbldiag.h"
#include "copybook.h"
#include "../../libgcobol/ec.h"
#include "../../libgcobol/common-defs.h"
#include "../../libgcobol/exceptl.h"
#include "exceptg.h"

// Quell warning: variable yynerrs_ set but not used
#pragma GCC diagnostic ignored "-Wunused-but-set-variable"

std::ostream&
operator<<( std::ostream& os, cbl_loc_t const& loc ) {
  os << "("
     << loc.first_line
     << ","
     << loc.first_column 
     << ") to ("
     << loc.last_line 
     << ","
     << loc.last_column
     << ")";
  return os;
}

const char * keyword_str( int token );

namespace cdf {
  static int lookahead_token_kind;
  static int lookahead( int token ) {
    if( token < 1 ) {
      dbgmsg("%s: look ahead to %s", __func__, keyword_str(token));
    } else {
      dbgmsg("%s: look ahead to token kind %d", __func__, token);
    }
    lookahead_token_kind = token;
    return lookahead_token_kind;
  }
  
  bool had_lookahead() {
    int kind = 0;
    std::swap(kind, lookahead_token_kind);
    return 0 < kind;
  }
}

#define SAVE_LOOKAHEAD cdf::lookahead(yyla.kind())
%}
                        
%code requires {
    namespace cdf
    {
      class parser;
    }
}

%code {    
    // https://learnmoderncpp.com/2020/12/17/generating-c-programs-with-flex-and-bison-2/
    namespace cdf
    {
      int cdflex( parser::semantic_type *value, cbl_loc_t *loc );
    }
}

%code provides {
  namespace cdf
  {
    template <typename F>
      int token_of( int kind, F xlate ) {
      for( int tok = -2; tok < parser::token::YDF_NEG + 1; tok++ ) {
        int token = xlate(tok);
        if( token == kind ) return tok;
      }
      assert(false);
    }
  }
}
                        
%define api.location.type {cbl_loc_t}
                        
%{
#define COUNT_OF(X) (sizeof(X) / sizeof(X[0]))

int keyword_tok( const char * text, bool include_intrinsics = false );

copybook_t copybook;

std::ostream& operator<<(std::ostream& os, const cbl_loc_t& loc);

static inline bool
is_word( int c ) {
  return c == '_' || ::isalnum(c);
}

static std::pair<long long, bool>
integer_literal( const char input[] ) {
  long long v;
  int n;
  bool fOK = 1 == sscanf(input, "%lld%n", &v, &n) &&
             n == (int)strlen(input);
  return std::make_pair(v, fOK);
}

/* "The renamed symbols include 'yyparse', 'yylex', 'yyerror',
    'yynerrs', 'yylval', 'yylloc', 'yychar' and 'yydebug'.  [...] The
    renamed macros include 'YYSTYPE', 'YYLTYPE', and 'YYDEBUG'" */

extern int yylineno, yyleng, yydebug;
extern char *yytext;

extern size_t cbl_gcobol_features;

#define PROGRAM current_program_index()

const cbl_loc_t& cobol_location();
static cbl_loc_t location_set( const cbl_loc_t& loc );
void input_file_status_notify();

#define YYLLOC_DEFAULT(Current, Rhs, N) 				\
  do {									\
      if (N)                                                            \
        {                                                               \
          (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;        \
          (Current).first_column = YYRHSLOC (Rhs, 1).first_column;      \
          (Current).last_line    = YYRHSLOC (Rhs, N).last_line;         \
          (Current).last_column  = YYRHSLOC (Rhs, N).last_column;       \
	  location_dump("cdf.c", N,					\
			"rhs N  ", YYRHSLOC (Rhs, N));			\
        }                                                               \
      else                                                              \
        {                                                               \
          (Current).first_line   =					\
	  (Current).last_line    = YYRHSLOC (Rhs, 0).last_line;		\
          (Current).first_column =					\
	  (Current).last_column  = YYRHSLOC (Rhs, 0).last_column;	\
        }                                                               \
      location_dump("cdf.c", __LINE__, "current", (Current));		\
      input_file_status_notify();					\
      location_set(Current);                                            \
  } while (0)

%}

%code requires {
  #include "cdfval.h"
  struct cbl_file_t;
  using std::map;

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-function"
  static bool
  cdfval_add( const char name[],
	       const cdfval_t& value, bool override = false )
  {
    cdf_values_t& dictionary( cdf_dictionary() );
    
    if( scanner_parsing() ) {
      if( ! override ) {
	if( dictionary.find(name) != dictionary.end() ) return false;
      }
      dictionary[name] = value;
    }
    return true;
  }
  static void
  cdfval_off( const char name[] ) {
    cdf_values_t& dictionary( cdf_dictionary() );
    
    if( scanner_parsing() ) {
      auto p = dictionary.find(name);
      if( p == dictionary.end() ) {
        dictionary[name] = cdfval_t();
      }
      dictionary[name].off = true;
    }
  }
#pragma GCC diagnostic pop

  bool operator==( const cdfval_base_t& lhs, int rhs );
  bool operator||( const cdfval_base_t& lhs, const cdfval_base_t& rhs );
  bool operator&&( const cdfval_base_t& lhs, const cdfval_base_t& rhs );

  cdfval_t operator<( const cdfval_base_t& lhs, const cdfval_base_t& rhs );
  cdfval_t operator<=( const cdfval_base_t& lhs, const cdfval_base_t& rhs );
  cdfval_t operator==( const cdfval_base_t& lhs, const cdfval_base_t& rhs );
  cdfval_t operator!=( const cdfval_base_t& lhs, const cdfval_base_t& rhs );
  cdfval_t operator>=( const cdfval_base_t& lhs, const cdfval_base_t& rhs );
  cdfval_t operator>( const cdfval_base_t& lhs, const cdfval_base_t& rhs );
  cdfval_t operator+( const cdfval_base_t& lhs, const cdfval_base_t& rhs );
  cdfval_t operator-( const cdfval_base_t& lhs, const cdfval_base_t& rhs );
  cdfval_t operator*( const cdfval_base_t& lhs, const cdfval_base_t& rhs );
  cdfval_t operator/( const cdfval_base_t& lhs, const cdfval_base_t& rhs );
  cdfval_t negate( cdfval_base_t lhs );

  void cdf_unreachable(); 
  void cdf_field_add( const cbl_loc_t&, const std::string& name, const cdfval_t& value );
  cbl_file_t * cdf_file( size_t program, const cbl_name_t name );
  size_t cdf_file_index( const cbl_file_t *file );
  const char * cdf_file_name(const cbl_file_t*);
}

%{
static char *display_msg;
const char * keyword_str( int token );

exception_turn_t exception_turn;
			
bool
apply_cdf_turn( const exception_turn_t& turn ) {
  cbl_enabled_exceptions_t& enabled_exceptions( cdf_enabled_exceptions() );
  
  for( auto elem : turn.exception_files() ) {
    std::set<size_t> files(elem.second.begin(), elem.second.end());
    enabled_exceptions.turn_on_off(turn.enabled,
                                   turn.location,
                                   elem.first, files);
  }
  if( getenv("GCOBOL_SHOW") ) enabled_exceptions.dump();
  return true;
}
%}

%union {
    bool boolean;
    int number;
    const char *string;
    cdf_arg_t     cdfarg;
    cdfval_base_t cdfval;
    cbl_file_t *file;
    std::set<size_t> *files;
}

%printer { fprintf(stderr, "'%s'", $$ ); } <string>
%printer { fprintf(stderr, "'%s'", $$? "true" : "false" ); } <boolean>
%printer { fprintf(stderr, "%s '%s'",
		   keyword_str($$.token),
		   $$.string? $$.string : "<nil>" ); } <cdfarg>
// cppcheck-suppress invalidPrintfArgType_sint 
%printer { fprintf(stderr, "%ld '%s'",
		   (long)$$.number, $$.string? $$.string : "" ); } <cdfval>
                        
%type	<string>	NAME NUMSTR LITERAL PSEUDOTEXT
%type	<string>	LSUB RSUB SUBSCRIPT
%type	<cdfarg>	namelit name_any name_one
%type	<string>	name subscript subscripts inof
%token <boolean>  BOOL
%token <number>  FEATURE 400  NUMBER 308  EXCEPTION_NAME 284    "EXCEPTION NAME"

%type	<cdfval>	cdf_expr
%type	<cdfval>	cdf_relexpr cdf_reloper cdf_and cdf_bool_expr
%type	<cdfval>	cdf_factor
%type	<boolean>	cdf_cond_expr override except_check

%type   <file>		filename
%type   <files>         filenames

%type   <number>        cdf_stackable

%token BY 520
%token COPY 397
%token CDF_DISPLAY 418    ">>DISPLAY"
%token IN 638
%token NAME 290
%token NUMSTR 310    "numeric literal"
%token OF 721
%token PSEUDOTEXT 755
%token REPLACING 777
%token READY 405  TRACE 412  RESET 406
%token LITERAL 303
%token SUPPRESS 411

%token LSUB 402    "("
%token SUBSCRIPT 410  RSUB 407    ")"

%token CDF_DEFINE 417    ">>DEFINE"
%token CDF_IF 419    ">>IF"
%token CDF_ELSE 420    ">>ELSE"
%token CDF_END_IF 421    ">>END-IF"
%token CDF_EVALUATE 422    ">>EVALUATE"
%token CDF_WHEN 423    ">>WHEN"
%token CDF_END_EVALUATE 424    ">>END-EVALUATE"

%token ALL 484
%token CALL_CONVENTION 425    ">>CALL-CONVENTION"
%token COBOL_WORDS 414    ">>COBOL-WORDS"
%token CDF_PUSH 428    ">>PUSH"
%token CDF_POP 429    ">>POP"
%token SOURCE_FORMAT 430    ">>SOURCE FORMAT"

%token AS 502  CONSTANT 396  DEFINED 398
%type	<boolean>	     DEFINED
%token OTHER 733  PARAMETER_kw 403    "PARAMETER"
%token OFF 722  OVERRIDE 404
%token THRU 979
%token TRUE_kw 848    "True"

%token CALL_COBOL 426    "CALL"
%token CALL_VERBATIM 427    "CALL (as C)"

%token TURN 850  CHECKING 530  LOCATION 682  ON 724  WITH 877

%left OR 980
%left AND 982
%right NOT 983
%left '<'  '>'  _EQ 302    "EQUAL"  _NE 984  _LE 985  _GE 986
%left '-'  '+'
%left '*'  '/'
%right NEG 988

%require "3.8.2"  // for C++ output
%language "c++"

%define api.prefix {cdf}
%define api.token.prefix{YDF_}

%locations
%define parse.error verbose
%%
top:		partials {
                SAVE_LOOKAHEAD; YYACCEPT; }
	|	copy '.'
		{
		  const char *library = copybook.library();
		  if( !library ) library = "SYSLIB";
		  const char *source = copybook.source();
		  dbgmsg("COPY %s from %s", source, library);
                  SAVE_LOOKAHEAD;
		  YYACCEPT;
		}
	|	copy error {
		  error_msg(@error, "COPY directive must end in a %<.%>");
		  YYABORT;
		}
	|	completes { SAVE_LOOKAHEAD; YYACCEPT; }
		;

completes:	complete
	|	completes complete
	|	completes partial
		;
complete:	cdf_define
	|	cdf_display
	|	cdf_trace
	|	cdf_turn
        |       cdf_call_convention
        |       cdf_push
        |       cdf_pop
		;

		/*
		 * To do: read ISO 2023 to see how >>DISPLAY is dictionary!
		 * To do: DISPLAY UPON
		 * To do: decide what to do about newlines, and when; DISPLAY has
     		 *        {}... in the specification.
		 */
cdf_display:	CDF_DISPLAY strings {
		  if( scanner_parsing() ) {
		    fprintf(stderr, "%s\n", display_msg);
		    free(display_msg);
		    display_msg = NULL;
		  }
		}
		;
strings:	LITERAL {
                  display_msg = xstrdup($1);
		}
	|	strings LITERAL {
                  char *p = display_msg;
		  display_msg = xasprintf("%s %s", p, $2);
		  free(p);
		}
		;

partials:	partial
		{
		  if( ! scanner_parsing() ) {
                    SAVE_LOOKAHEAD;
                    YYACCEPT;
                  }
		}
	|	partials partial
		{
		  if( ! scanner_parsing() ) {
                    SAVE_LOOKAHEAD;
                    YYACCEPT;
                  }
		}
		;
partial:	cdf_if            /* text */
	|	CDF_ELSE          { scanner_parsing_toggle(); }
	|	CDF_END_IF        { scanner_parsing_pop(); }
	|	cdf_evaluate      /* text */
	|	cdf_eval_when     /* text */
	|	CDF_END_EVALUATE  { scanner_parsing_pop(); }
	;

cdf_define:	CDF_DEFINE cdf_constant NAME as cdf_expr[value] override
		{
		  if( keyword_tok($NAME) ) {
		    error_msg(@NAME, "%s is a COBOL keyword", $NAME);
		    YYERROR;
		  }
		  if( !cdfval_add( $NAME, cdfval_t($value), $override) ) {
		    error_msg(@NAME, "name already in dictionary: %s", $NAME);
                    cdf_values_t& dictionary( cdf_dictionary() );
		    const cdfval_t& entry = dictionary[$NAME];
		    if( entry.filename ) {
		      error_msg(@NAME, "%s previously defined in %s:%d",
				$NAME, entry.filename, entry.lineno);
		    } else {
		      error_msg(@NAME, "%s was defined on the command line", $NAME);
		    }
		    YYERROR;
		  }
                  cdf_field_add( @NAME, $NAME, $value );

		}
	|	CDF_DEFINE cdf_constant NAME _EQ cdf_expr[value] override
		{  /* accept, but as error */
		  if( scanner_parsing() ) {
		    error_msg(@NAME, "CDF error: %s = value invalid", $NAME);
		  }
		}
	|	CDF_DEFINE cdf_constant NAME as OFF
		{
                  cdfval_off( $NAME);
		}
	|	CDF_DEFINE cdf_constant NAME as PARAMETER_kw override
		/*
		 * "If the PARAMETER phrase is specified, the value referenced
		 * by compilation-variable-name-1 is obtained from the
		 * operating environment by an implementor-defined method...."
		 * It's a noop for us, because parameters defined with -D are
		 * available regardless.
		 */
		{
		  if( 0 == cdf_dictionary().count($NAME) ) {
                    cbl_message(@NAME, CdfParameterW,
                                "CDF: '%s' is defined AS PARAMETER "
                                "but was not defined", $NAME);
		  }
		}
	|	CDF_DEFINE FEATURE as ON {
                  		  auto feature = cbl_gcobol_feature_t($2);
		  if( ! cobol_gcobol_feature_set(feature, true) ) {
		    error_msg(@FEATURE,
                              "%<>>DEFINE %%EBCDIC-MODE%> is invalid within program body");
		  }
		}
	|	CDF_DEFINE FEATURE as OFF {
                  		  auto feature = cbl_gcobol_feature_t($2);
		  if( ! cobol_gcobol_feature_set(feature, false) ) {
		    error_msg(@FEATURE,
                              "%<>>DEFINE %%EBCDIC-MODE%> is invalid within program body");
		  }
		}
		;
cdf_constant:	%empty
	|	CONSTANT 
                ;
override:	%empty   { $$ = false; }
	|	OVERRIDE { $$ = true; }
		;

cdf_trace:      READY TRACE
        |       RESET TRACE
                ;

cdf_turn:	TURN except_names except_check
		{
		  apply_cdf_turn(exception_turn);
		  exception_turn.clear();
		}
		;

cdf_call_convention:
                CALL_COBOL {
                  current_call_convention(cbl_call_cobol_e);
                }
        |       CALL_VERBATIM {
                  current_call_convention(cbl_call_verbatim_e);
                }
                ;

cdf_push:       CDF_PUSH cdf_stackable {
		  switch( $cdf_stackable ) {
                  case parser::token::YDF_ALL: 		cdf_push(); break;
                  case parser::token::YDF_CALL_CONVENTION: cdf_push_call_convention(); break;
                  case parser::token::YDF_CDF_DEFINE: 	cdf_push_dictionary(); break;
                  case parser::token::YDF_COBOL_WORDS: 	cdf_push_current_tokens(); break;
                  case parser::token::YDF_SOURCE_FORMAT:
                  default: cdf_unreachable(); 
                  }
                }
                ;
cdf_pop:        CDF_POP cdf_stackable {
		  switch( $cdf_stackable ) {
                  case parser::token::YDF_ALL: 		cdf_pop(); break;
                  case parser::token::YDF_CALL_CONVENTION: cdf_pop_call_convention(); break;
                  case parser::token::YDF_CDF_DEFINE: 	cdf_pop_dictionary(); break;
                  case parser::token::YDF_COBOL_WORDS: 	cdf_pop_current_tokens(); break;
                  case parser::token::YDF_SOURCE_FORMAT:
                  default: cdf_unreachable(); 
                  }
                }
                ;

cdf_stackable:  ALL		{ $$ = parser::token::YDF_ALL; }
        |       CALL_CONVENTION	{ $$ = parser::token::YDF_CALL_CONVENTION; }
        |       COBOL_WORDS	{ $$ = parser::token::YDF_COBOL_WORDS; }
        |       CDF_DEFINE	{ $$ = parser::token::YDF_CDF_DEFINE; }
        |       SOURCE_FORMAT	{ $$ = parser::token::YDF_SOURCE_FORMAT; }
                ;

except_names: 	except_name
	|	except_names except_name
		;
except_name:	EXCEPTION_NAME[ec] {
		  assert($ec != ec_none_e);
		  exception_turn.add_exception(ec_type_t($ec));
		}
	|	EXCEPTION_NAME[ec] filenames {
		  assert($ec != ec_none_e);
		  std::list<size_t> files($filenames->begin(), $filenames->end());
		  exception_turn.add_exception(ec_type_t($ec), files);
		}
		;

except_check:	CHECKING on  { $$ = exception_turn.enable(true); }
	|	CHECKING OFF {
                  $$ = exception_turn.enable(false);
                }
	|	CHECKING on with LOCATION
		{
                  $$ = exception_turn.enable(true, true);
		}
		;

filenames:      filename {
		  $$ = new std::set<size_t>;
		  $$->insert(cdf_file_index($1));
		}
        |       filenames filename {
		  $$ = $1;
		  auto inserted = $$->insert(cdf_file_index($2));
		  if( ! inserted.second ) {
		    error_msg(@2, "%s: No file-name shall be specified more than once "
			          "for one exception condition",
                              cdf_file_name($filename));
		  }
		}
                ;
filename:       NAME
                {
                  if( ($$ = cdf_file(PROGRAM, $1)) == nullptr ) {
		    error_msg(@NAME, "invalid file name '%s'", $NAME);
		    YYERROR;
                  }
                }
                ;

cdf_if:		CDF_IF cdf_cond_expr {
		  scanner_parsing(parser::token::YDF_CDF_IF, $2);
		}
	|	CDF_IF error CDF_END_IF { // not pushed, don't pop
                  if( ! scanner_parsing() ) {
                    SAVE_LOOKAHEAD;
                    YYACCEPT;
                  }
		}
		;

cdf_evaluate:   CDF_EVALUATE cdf_expr
	|	CDF_EVALUATE TRUE_kw 
                ;

cdf_eval_when:	CDF_WHEN cdf_eval_obj
                ;

cdf_eval_obj:	cdf_cond_expr
        |       cdf_expr THRU cdf_expr
        |       OTHER 
        ;

cdf_cond_expr:	BOOL 
	|	NAME DEFINED
		{
                  cdf_values_t& dictionary( cdf_dictionary() );
		  auto p = dictionary.find($1);
		  bool found = p != dictionary.end();
		  if( !$DEFINED ) found = ! found;
		  $$ = found;
		  if( found ) {
                    const char *aint = $DEFINED? "" : "not ";
		    dbgmsg("CDF: %s %sfound in dictionary (result %s)",
			   $1, aint, $$? "true" : "false");
		  } else {
		    dbgmsg("CDF: %s not found in dictionary (result %s)",
			   $1, $$? "true" : "false");
		  }
		}
	|	cdf_bool_expr { $$ = $1(@1) == 0? false : true; }
	|	FEATURE DEFINED {
                  const auto& feature($1);
		  $$ = (feature == int(feature & cbl_gcobol_features));
		  dbgmsg("CDF: feature 0x%02x is %s", $1, $$? "ON" : "OFF");
		}
		;

		/*
		 * "Abbreviated combined relation conditions
		 * shall not be specified."
		 */
cdf_bool_expr:	cdf_bool_expr OR cdf_and { $$ = cdfval_t($1(@1) || $3(@3)); }
	|	cdf_and
		;

cdf_and:	cdf_and AND cdf_reloper { $$ = cdfval_t($1(@1) && $3(@3)); }
	|	cdf_reloper
		;

cdf_reloper:	    cdf_relexpr
	|	NOT cdf_relexpr { $$ = cdfval_t($2.number? 1 : 0); }
		;

cdf_relexpr:	cdf_relexpr '<' cdf_expr { $$ = $1(@1) <  $3(@3); }
	|	cdf_relexpr _LE  cdf_expr { $$ = $1(@1) <= $3(@3); }
	|	cdf_relexpr _EQ cdf_expr {
		  $$ = cdfval_t(false);
		  if( ( $1.string &&  $3.string) ||
		      (!$1.string && !$3.string) )
		  {
		      $$ = $1 == $3;
		  } else {
		    const char *msg = $1.string?
		      "incommensurate comparison is FALSE: '%s' = %ld" :
		      "incommensurate comparison is FALSE: %ld = '%s'" ;
		    error_msg(@1, "%s", msg);
		  }
		}
	|	cdf_relexpr _NE cdf_expr
		{
		  $$ = cdfval_t(false);
		  if( ( $1.string &&  $3.string) ||
		      (!$1.string && !$3.string) )
		  {
		      $$ = $1 != $3;
		  } else {
		    const char *msg = $1.string?
		      "incommensurate comparison is FALSE: '%s' = %ld" :
		      "incommensurate comparison is FALSE: %ld = '%s'" ;
		    error_msg(@1, "%s", msg);
		  }
		}
	|	cdf_relexpr _GE  cdf_expr { $$ = $1(@1) >= $3(@3); }
	|	cdf_relexpr '>' cdf_expr { $$ = $1(@1) >  $3(@3); }
	|	cdf_expr
		;

cdf_expr:	cdf_expr '+' cdf_expr { $$ = $1(@1) + $3(@3); }
        |       cdf_expr '-' cdf_expr { $$ = $1(@1) - $3(@3); }
        |       cdf_expr '*' cdf_expr { $$ = $1(@1) * $3(@3); }
        |       cdf_expr '/' cdf_expr { $$ = $1(@1) / $3(@3); }
	|	         '+' cdf_expr %prec NEG { $$ = $2(@2); }
	|	         '-' cdf_expr %prec NEG { $$ = negate($2(@2)); }
	|	         '(' cdf_bool_expr ')'  { $$ = $2(@2); }
        |	cdf_factor
        ;

cdf_factor:     NAME {
		  cdf_values_t& dictionary( cdf_dictionary() );
		  auto that = dictionary.find($1);
		  if( that != dictionary.end() ) {
		    $$ = that->second;
		  } else {
		    if( ! scanner_parsing() ) {
		      cbl_message(CdfNotFoundW,
                                  "CDF skipping: no such variable '%s'", $1);
		    } else {
		      error_msg(@NAME, "CDF error: no such variable '%s'", $1);
		    }
		    $$ = cdfval_t();
		  }
		}
	|	NUMBER
                {
                                    $$ = cdfval_t($1);
                }
	|	LITERAL {
                                    $$ = cdfval_t($1);
                 }
	| 	NUMSTR {
                  		  auto value = integer_literal($NUMSTR);
		  if( !value.second ) {
		    error_msg(@1, "CDF error: parsed %qs as %lld",
		             $NUMSTR, value.first);
		    YYERROR;
		  }
		  $$ = cdfval_t(value.first);
		}
                ;

copy:		copy_impl
		;
copy_impl:	copybook_name suppress REPLACING replace_bys
	|	copybook_name suppress
		;
copybook_name: 	COPY name_one[src]
		{
		  if( -1 == copybook.open(@src, $src.string) ) {
		    error_msg(@src, "could not open copybook file "
		             "for '%s'", $src.string);
		    YYABORT;
		  }
		}
	|	COPY name_one[src] IN name_one[lib]
		{
		  copybook.library(@lib, $lib.string);
		  if( -1 == copybook.open(@src, $src.string) ) {
		    error_msg(@src, "could not open copybook file "
		             "for %<%s%> in %<%s%>", $src.string, $lib.string);
		    YYABORT;
		  }
		}
		;

replace_bys:	replace_by
	|	replace_bys replace_by
		;

replace_by:	name_any[a] BY name_any[b]
		{
		  bool add_whitespace = false;
		  replace_type_t type = {};
		  switch($a.token) {
		  case parser::token::YDF_NUMSTR:
		  case parser::token::YDF_LITERAL:
		    type = string_e;
		    break;
		  case parser::token::YDF_NAME:
		    type = token_e;
		    break;
		  case parser::token::YDF_PSEUDOTEXT:
		    type = pseudo_e;
		    add_whitespace = $b.token != parser::token::YDF_PSEUDOTEXT;
		    break;
		  default:
		      cbl_err("%s:%d: logic error on token %s",
			   __FILE__, __LINE__, keyword_str($a.token));
		      break;
		  }
		  char *replacement = const_cast<char*>($b.string);
		  if( add_whitespace ) {
		    char *s = xasprintf(" %s ", replacement);
		    free(replacement);
		    replacement = s;
		  }
		  copybook.replacement( type, $a.string, replacement );
		}
		;

suppress:	%empty
	|	SUPPRESS
		{
                  		  copybook.suppress();
		}
		;

name_any:	namelit
	|	PSEUDOTEXT {
                  		  $$ = cdf_arg_t{parser::token::YDF_PSEUDOTEXT, $1};
		}
		;

name_one:	NAME
		{
                  cdf_values_t& dictionary( cdf_dictionary() );
		  cdf_arg_t arg = { parser::token::YDF_NAME, $1 };
		  auto p = dictionary.find($1);

		  if( p != dictionary.end() ) {
		    arg.string = p->second.string;
		  }
		  $$ = arg;
		}
	|	NUMSTR {
                                    $$ = cdf_arg_t{parser::token::YDF_NUMSTR, $1};
                }
	|	LITERAL {
                                    $$ = cdf_arg_t{parser::token::YDF_LITERAL, $1};
                }
		;

namelit:	name
		{
                  cdf_values_t& dictionary( cdf_dictionary() );
		  cdf_arg_t arg = { parser::token::YDF_NAME, $1 };
		  auto p = dictionary.find($1);

		  if( p != dictionary.end() ) {
		    arg.string = p->second.string;
		  }
		  $$ = arg;
		}
	|	name subscripts
		{
		  char *s = xasprintf( "%s%s", $1, $2 );
		  free(const_cast<char*>($1));
		  free(const_cast<char*>($2));

		  cdf_arg_t arg = { parser::token::YDF_NAME, s };
		  $$ = arg;
		}
	|	NUMSTR  { $$ = cdf_arg_t{parser::token::YDF_NUMSTR, $1}; }
	|	LITERAL { $$ = cdf_arg_t{parser::token::YDF_LITERAL, $1}; }
		;

name:		NAME                   
	|	name inof NAME
		{
                  		  char *s = xasprintf( "%s %s %s", $1, $2, $3 );
		  assert($$ == $1);
		  free(const_cast<char*>($1));
		  free(const_cast<char*>($3));
		  $$ = s;
		}
                ;
inof:           IN {
                                    static const char in[] = "IN"; $$ = in;
                }
        |       OF {
                                    static const char of[] = "OF"; $$ = of;
                }
                ;

subscripts:	subscript
	|	subscripts subscript {
		  char *s = xasprintf("%s%s", $1, $2 );
		  if( $$ != $1 ) free(const_cast<char*>($$));
		  free(const_cast<char*>($1));
		  free(const_cast<char*>($2));
		  $$ = s;
		}
		;
subscript:	SUBSCRIPT 
	|	LSUB subscript RSUB
		{
                  		  char *s = xasprintf( "%s%s%s", $1, $2, $3 );
		  free(const_cast<char*>($1));
		  free(const_cast<char*>($2));
		  free(const_cast<char*>($3));
		  $$ = s;
		}
		;

as:		%empty
	|	AS 
		;

on:             %empty
        |       ON 
                ;

with:           %empty
        |       WITH 
                ;

%%

static cbl_loc_t cdf_location;

static cbl_loc_t
location_set( const cbl_loc_t& loc ) {
  return cdf_location = loc;
}

namespace cdf {
    cbl_loc_t
    location() {
        return ::cdf_location;
    }
}

bool // used by cobol1.cc
defined_cmd( const char arg[] )
{
  cdf_values_t& dictionary( cdf_dictionary() );
  cdfval_t value(1);

  char *name = xstrdup(arg);
  char *p = strchr(name, '=');
  if(p) {
    *p++ = '\0';
    int pos, number;
    if( 1 == sscanf(p, "%d%n", &number, &pos) && size_t(pos) == strlen(p) ) {
	value = cdfval_t(number);
    } else {
      value = cdfval_t(p);  // it's a string
    }
  }

  dictionary[name] = value;

  auto cdf_name = dictionary.find(name);
  assert(cdf_name != dictionary.end());
  assert(cdf_name->second.is_numeric() || cdf_name->second.string != NULL);

  if( yydebug ) {
    if( cdf_name->second.is_numeric() ) {
      dbgmsg("%s: added -D %s = %ld",
             __func__, name, (long)cdf_name->second.as_number());
    } else {
      dbgmsg("%s: added -D %s = \"%s\"", __func__, name, cdf_name->second.string);
    }
  }
  return true;
}

bool operator==( const cdfval_base_t& lhs, int rhs ) {
  gcc_assert( !lhs.string );
  return lhs.number == rhs;
}

bool operator||( const cdfval_base_t& lhs, const cdfval_base_t& rhs ) {
  gcc_assert( !lhs.string && !rhs.string );
  return lhs.number || rhs.number;
}

bool operator&&( const cdfval_base_t& lhs, const cdfval_base_t& rhs ) {
  gcc_assert( !lhs.string && !rhs.string );
  return lhs.number && rhs.number;
}

cdfval_t operator<( const cdfval_base_t& lhs, const cdfval_base_t& rhs ) {
  gcc_assert( !lhs.string && !rhs.string );
  return cdfval_t(lhs.number < rhs.number);
}

cdfval_t operator<=( const cdfval_base_t& lhs, const cdfval_base_t& rhs ) {
  gcc_assert( !lhs.string && !rhs.string );
  return cdfval_t(lhs.number <= rhs.number);
}

cdfval_t operator==( const cdfval_base_t& lhs, const cdfval_base_t& rhs ) {
  if( lhs.string && rhs.string ) {
    return cdfval_t(0 == strcasecmp(lhs.string, rhs.string));
  }
  if( !lhs.string && !rhs.string ) {
    return cdfval_t(lhs.number == rhs.number);
  }
  cbl_internal_error("incommensurate operands");
  return false;
}

cdfval_t operator!=( const cdfval_base_t& lhs, const cdfval_base_t& rhs ) {
  if( lhs.string && rhs.string ) {
    return cdfval_t(0 != strcasecmp(lhs.string, rhs.string));
  }
  if( !lhs.string && !rhs.string ) {
    return cdfval_t(lhs.number != rhs.number);
  }
  cbl_internal_error("incommensurate operands");
  return false;
}

cdfval_t operator>=( const cdfval_base_t& lhs, const cdfval_base_t& rhs ) {
  gcc_assert( !lhs.string && !rhs.string );
  return cdfval_t(lhs.number >= rhs.number);
}

cdfval_t operator>( const cdfval_base_t& lhs, const cdfval_base_t& rhs ) {
  gcc_assert( !lhs.string && !rhs.string );
  return cdfval_t(lhs.number > rhs.number);
}

cdfval_t operator+( const cdfval_base_t& lhs, const cdfval_base_t& rhs ) {
  gcc_assert( !lhs.string && !rhs.string );
  return cdfval_t(lhs.number + rhs.number);
}

cdfval_t operator-( const cdfval_base_t& lhs, const cdfval_base_t& rhs ) {
  gcc_assert( !lhs.string && !rhs.string );
  return cdfval_t(lhs.number - rhs.number);
}

cdfval_t operator*( const cdfval_base_t& lhs, const cdfval_base_t& rhs ) {
  gcc_assert( !lhs.string && !rhs.string );
  return cdfval_t(lhs.number * rhs.number);
}

cdfval_t operator/( const cdfval_base_t& lhs, const cdfval_base_t& rhs ) {
  gcc_assert( !lhs.string && !rhs.string );
  return cdfval_t(lhs.number / rhs.number);
}

cdfval_t negate( cdfval_base_t lhs ) {
  gcc_assert( !lhs.string );
  lhs.number = -lhs.number;
  return lhs;
}

bool
cdf_value( const char name[], const cdfval_t& value ) {
  cdf_values_t& dictionary( cdf_dictionary() );
  auto p = dictionary.find(name);

  if( p != dictionary.end() ) return false;

  dictionary[name] = value;
  return true;
}

const cdfval_t *
cdf_value( const char name[] ) {
  cdf_values_t& dictionary( cdf_dictionary() );
  auto p = dictionary.find(name);

  if( p == dictionary.end() ) return NULL;

  return &p->second;
}

static bool
verify_integer( const cbl_loc_t& loc, const cdfval_base_t& val ) {
  if( val.string ) {
    error_msg(loc, "'%s' is not an integer", val.string);
    return false;
  }
  return true;
}

const cdfval_base_t&
cdfval_base_t::operator()( const cbl_loc_t& loc ) {
  static cdfval_t zero(0);
  // cppcheck-suppress returnTempReference
  return verify_integer(loc, *this) ? *this : zero;
}
  
namespace cdf {
  void
  parser::error(cbl_loc_t const& loc, std::string const& msg) {
      error_msg(loc, msg.c_str());
  }
}

