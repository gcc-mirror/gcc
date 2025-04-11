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
%{

#include "cobol-system.h"
#include "coretypes.h"
#include "tree.h"
#undef yy_flex_debug
#include "../../libgcobol/ec.h"
#include "../../libgcobol/common-defs.h"
#include "util.h"
#include "cbldiag.h"
#include "symbols.h"
#include "copybook.h"
#include "../../libgcobol/exceptl.h"
#include "exceptg.h"

#define COUNT_OF(X) (sizeof(X) / sizeof(X[0]))

copybook_t copybook;

static inline bool
is_word( int c ) {
  return c == '_' || ISALNUM(c);
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

extern int yylineno, yyleng;
extern char *yytext;

static int ydflex(void);

#define PROGRAM current_program_index()

const YYLTYPE& cobol_location();
static YYLTYPE location_set( const YYLTYPE& loc );
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
      gcc_location_set( location_set(Current) );			\
  } while (0)

%}

%code requires {
  #include "cdfval.h"

  using std::map;

  static map<std::string, cdfval_t> dictionary;

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-function"
  static bool
  cdfval_add( const char name[],
	       const cdfval_t& value, bool override = false )
  {
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

}

%{
static char *display_msg;
const char * keyword_str( int token );

static class exception_turns_t {
  typedef std::list<size_t> filelist_t;
  typedef std::map<ec_type_t, filelist_t> ec_filemap_t;
  ec_filemap_t exceptions;
 public:
  bool enabled, location;

  exception_turns_t() : enabled(false), location(false) {};

  const ec_filemap_t& exception_files() const { return exceptions; }

  struct args_t {
    size_t nexception;
    cbl_exception_files_t *exceptions;
  };

  bool add_exception( ec_type_t type, const filelist_t files = filelist_t() ) {
    ec_disposition_t disposition = ec_type_disposition(type);
    if( disposition != ec_implemented(disposition) ) {
	cbl_unimplementedw("CDF: exception '%s'", ec_type_str(type));
    }
    auto elem = exceptions.find(type);
    if( elem != exceptions.end() ) return false; // cannot add twice

    exceptions[type] = files;
    return true;
  }

  args_t args() const {
    args_t args;
    args.nexception = exceptions.size();
    args.exceptions = NULL;
    if( args.nexception ) {
      args.exceptions = new cbl_exception_files_t[args.nexception];
    }
    std::transform( exceptions.begin(), exceptions.end(), args.exceptions,
                    []( auto& input ) {
                      cbl_exception_files_t output;
                      output.type = input.first;
                      output.nfile = input.second.size();
                      output.files = NULL;
                      if( output.nfile ) {
                        output.files = new size_t[output.nfile];
                        std::copy(input.second.begin(),
                                       input.second.end(),
                                       output.files );
                      }
                      return output;
                    } );
    return args;
  }

  void clear() {
    for( auto& ex : exceptions ) {
      ex.second.clear();
    }
    exceptions.clear();
    enabled = location = false;
  }

} exception_turns;


static bool
apply_cdf_turn( exception_turns_t& turns ) {
  for( auto elem : turns.exception_files() ) {
    std::set<size_t> files(elem.second.begin(), elem.second.end());
    enabled_exceptions.turn_on_off(turns.enabled,
                                   turns.location,
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

%printer { fprintf(yyo, "'%s'", $$ ); } <string>
%printer { fprintf(yyo, "%s '%s'",
		   keyword_str($$.token),
		   $$.string? $$.string : "<nil>" ); } <cdfarg>
%printer { fprintf(yyo, "%ld '%s'",
		   $$.number, $$.string? $$.string : "" ); } <cdfval>

%type	<string>	NAME NUMSTR LITERAL PSEUDOTEXT
%type	<string>	LSUB RSUB SUBSCRIPT
%type	<cdfarg>	namelit name_any name_one
%type	<string>	name subscript subscripts inof
%token <boolean>  BOOL
%token <number>  FEATURE 363  NUMBER 302  EXCEPTION_NAME 280    "EXCEPTION NAME"

%type	<cdfval>	cdf_expr
%type	<cdfval>	cdf_relexpr cdf_reloper cdf_and cdf_bool_expr
%type	<cdfval>	cdf_factor
%type	<boolean>	cdf_cond_expr override

%type   <file>		filename
%type   <files>         filenames

%token BY 476
%token COPY 360
%token CDF_DISPLAY 382    ">>DISPLAY"
%token IN 595
%token NAME 286
%token NUMSTR 304    "numeric literal"
%token OF 676
%token PSEUDOTEXT 711
%token REPLACING 733
%token LITERAL 297
%token SUPPRESS 374

%token LSUB 365    "("
%token SUBSCRIPT 373  RSUB 370    ")"

%token CDF_DEFINE 381    ">>DEFINE"
%token CDF_IF 383    ">>IF"
%token CDF_ELSE 384    ">>ELSE"
%token CDF_END_IF 385    ">>END-IF"
%token CDF_EVALUATE 386    ">>EVALUATE"
%token CDF_WHEN 387    ">>WHEN"
%token CDF_END_EVALUATE 388    ">>END-EVALUATE"

%token AS 458  CONSTANT 359  DEFINED 361
%type	<boolean>	     DEFINED
%token OTHER 688  PARAMETER_kw 366    "PARAMETER"
%token OFF 677  OVERRIDE 367
%token THRU 929
%token TRUE_kw 803    "True"

%token CALL_COBOL 389    "CALL"
%token CALL_VERBATIM 390    "CALL (as C)"

%token TURN 805  CHECKING 486  LOCATION 639  ON 679  WITH 831

%left OR 930
%left AND 931
%right NOT 932
%left '<'  '>'  '='  NE 933  LE 934  GE 935
%left '-'  '+'
%left '*'  '/'
%right NEG 937

%define api.prefix {ydf}
%define api.token.prefix{YDF_}

%locations
%define parse.error verbose
%%
top:		partials { YYACCEPT; }
	|	copy '.'
		{
		  const char *library = copybook.library();
		  if( !library ) library = "SYSLIB";
		  const char *source = copybook.source();
		  dbgmsg("COPY %s from %s", source, library);
		  YYACCEPT;
		}
	|	copy error {
		  error_msg(@error, "COPY directive must end in a '.'");
		  YYACCEPT;
		}
	|	completes { YYACCEPT; }
		;

completes:	complete
	|	completes complete
	|	completes partial
		;
complete:	cdf_define
	|	cdf_display
	|	cdf_turn
        |       cdf_call_convention
		;

		/*
		 * To do: read ISO 2022 to see how >>DISPLAY is dictionary!
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
		  if( ! scanner_parsing() ) YYACCEPT;
		}
	|	partials partial
		{
		  if( ! scanner_parsing() ) YYACCEPT;
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
		    const cdfval_t& entry = dictionary[$NAME];
		    if( entry.filename ) {
		      error_msg(@NAME, "%s previously defined in %s:%d",
				$NAME, entry.filename, entry.lineno);
		    } else {
		      error_msg(@NAME, "%s was defined on the command line", $NAME);
		    }
		    YYERROR;
		  }
		}
	|	CDF_DEFINE cdf_constant NAME '=' cdf_expr[value] override
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
		  if( 0 == dictionary.count($NAME) ) {
		    yywarn("CDF: '%s' is defined AS PARAMETER "
			    "but was not defined", $NAME);
		  }
		}
	|	CDF_DEFINE FEATURE as ON {
		  auto feature = cbl_gcobol_feature_t($2);
		  if( ! cobol_gcobol_feature_set(feature, true) ) {
		    error_msg(@FEATURE, ">>DEFINE %EBCDIC-MODE is invalid within program body");
		  }
		}
	|	CDF_DEFINE FEATURE as OFF {
		  auto feature = cbl_gcobol_feature_t($2);
		  if( ! cobol_gcobol_feature_set(feature, false) ) {
		    error_msg(@FEATURE, ">>DEFINE %EBCDIC-MODE is invalid within program body");
		  }
		}
		;
cdf_constant:	%empty
	|	CONSTANT
		;
override:	%empty   { $$ = false; }
	|	OVERRIDE { $$ = true; }
		;

cdf_turn:	TURN except_names except_check
		{
		  apply_cdf_turn(exception_turns);
		  exception_turns.clear();
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


except_names: 	except_name
	|	except_names except_name
		;
except_name:	EXCEPTION_NAME[ec] {
		  assert($ec != ec_none_e);
		  exception_turns.add_exception(ec_type_t($ec));
		}
	|	EXCEPTION_NAME[ec] filenames {
		  assert($ec != ec_none_e);
		  std::list<size_t> files;
		  std::copy( $filenames->begin(), $filenames->end(),
		                  std::back_inserter(files) );
		  exception_turns.add_exception(ec_type_t($ec), files);
		}
		;

except_check:	CHECKING on  { exception_turns.enabled = true; }
	|	CHECKING OFF { exception_turns.enabled = false; }
	|	CHECKING on with LOCATION
		{
		  exception_turns.enabled = exception_turns.location = true;
		}
		;

filenames:      filename {
		  $$ = new std::set<size_t>;
		  $$->insert(symbol_index(symbol_elem_of($1)));
		}
        |       filenames filename {
		  $$ = $1;
		  auto inserted = $$->insert(symbol_index(symbol_elem_of($2)));
		  if( ! inserted.second ) {
		    error_msg(@2, "%s: No file-name shall be specified more than "
			      " once for one exception condition", $filename->name);
		  }
		}
                ;
filename:       NAME
                {
                  struct symbol_elem_t *e = symbol_file(PROGRAM, $1);
                  if( !(e && e->type == SymFile) ) {
		    error_msg(@NAME, "invalid file name '%s'", $NAME);
		    YYERROR;
                  }
                  $$ = cbl_file_of(e);
                }
                ;

cdf_if:		CDF_IF cdf_cond_expr {
		  scanner_parsing(YDF_CDF_IF, $2);
		}
	|	CDF_IF error {
		  ////if( scanner_parsing() ) yyerrok;
		} CDF_END_IF { // not pushed, don't pop
		  if( ! scanner_parsing() ) YYACCEPT;
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
	|	NAME DEFINED[maybe]
		{
		  auto p = dictionary.find($1);
		  bool found = p != dictionary.end();
		  if( !$maybe ) found = ! found;
		  if( ! found ) {
		    $$ = !$2;
		    dbgmsg("CDF: %s not found in dictionary (result %s)",
			   $1, $$? "true" : "false");
		  } else {
		    $$ = $2;
		    dbgmsg("CDF: %s found in dictionary (result %s)",
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
	|	cdf_relexpr LE  cdf_expr { $$ = $1(@1) <= $3(@3); }
	|	cdf_relexpr '=' cdf_expr {
		  $$ = cdfval_t(false);
		  if( ( $1.string &&  $3.string) ||
		      (!$1.string && !$3.string) )
		  {
		      $$ = $1 == $3;
		  } else {
		    const char *msg = $1.string?
		      "incommensurate comparison is FALSE: '%s' = %ld" :
		      "incommensurate comparison is FALSE: %ld = '%s'" ;
		    error_msg(@1, msg);
		  }
		}
	|	cdf_relexpr NE cdf_expr
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
		    error_msg(@1, msg);
		  }
		}
	|	cdf_relexpr GE  cdf_expr { $$ = $1(@1) >= $3(@3); }
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
		  auto that = dictionary.find($1);
		  if( that != dictionary.end() ) {
		    $$ = that->second;
		  } else {
		    if( ! scanner_parsing() ) {
		      yywarn("CDF skipping: no such variable '%s' (ignored)", $1);
		    } else {
		      error_msg(@NAME, "CDF error: no such variable '%s'", $1);
		    }
		    $$ = cdfval_t();
		  }
		}
	|	NUMBER { $$ = cdfval_t($1); }
	|	LITERAL { $$ = cdfval_t($1); }
	| 	NUMSTR {
		  auto value = integer_literal($NUMSTR);
		  if( !value.second ) {
		    error_msg(@1, "CDF error: parsed %s as %ld",
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
		    YYERROR;
		  }
		}
	|	COPY name_one[src] IN name_one[lib]
		{
		  copybook.library(@lib, $lib.string);
		  if( -1 == copybook.open(@src, $src.string) ) {
		    error_msg(@src, "could not open copybook file "
		             "for '%s' in '%'s'", $src.string, $lib.string);
		    YYERROR;
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
		  case YDF_NUMSTR:
		  case YDF_LITERAL:
		    type = string_e;
		    break;
		  case YDF_NAME:
		    type = token_e;
		    break;
		  case YDF_PSEUDOTEXT:
		    type = pseudo_e;
		    add_whitespace = $b.token != YDF_PSEUDOTEXT;
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
		  $$ = cdf_arg_t{YDF_PSEUDOTEXT, $1};
		}
		;

name_one:	NAME
		{
		  cdf_arg_t arg = { YDF_NAME, $1 };
		  auto p = dictionary.find($1);

		  if( p != dictionary.end() ) {
		    arg.string = p->second.string;
		  }
		  $$ = arg;
		}
	|	NUMSTR  { $$ = cdf_arg_t{YDF_NUMSTR, $1}; }
	|	LITERAL { $$ = cdf_arg_t{YDF_LITERAL, $1}; }
		;

namelit:	name
		{
		  cdf_arg_t arg = { YDF_NAME, $1 };
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

		  cdf_arg_t arg = { YDF_NAME, s };
		  $$ = arg;
		}
	|	NUMSTR  { $$ = cdf_arg_t{YDF_NUMSTR, $1}; }
	|	LITERAL { $$ = cdf_arg_t{YDF_LITERAL, $1}; }
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
inof:           IN { static const char in[] = "IN"; $$ = in; }
        |       OF { static const char of[] = "OF"; $$ = of; }
                ;

subscripts:	subscript
	|	subscripts subscript
		{
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

static YYLTYPE cdf_location;

static YYLTYPE
location_set( const YYLTYPE& loc ) {
  return cdf_location = loc;
}

bool // used by cobol1.cc
defined_cmd( const char arg[] )
{
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
      dbgmsg("%s: added -D %s = %ld", __func__, name, cdf_name->second.as_number());
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

#undef yylex
int yylex(void);

static int ydflex(void) {
  return yylex();
}

bool
cdf_value( const char name[], cdfval_t value ) {
  auto p = dictionary.find(name);

  if( p != dictionary.end() ) return false;

  dictionary[name] = value;
  return true;
}

const cdfval_t *
cdf_value( const char name[] ) {
  auto p = dictionary.find(name);

  if( p == dictionary.end() ) return NULL;

  return &p->second;
}

static bool
verify_integer( const YDFLTYPE& loc, const cdfval_base_t& val ) {
  if( val.string ) {
    error_msg(loc, "'%s' is not an integer", val.string);
    return false;
  }
  return true;
}

const cdfval_base_t&
cdfval_base_t::operator()( const YDFLTYPE& loc ) {
  static cdfval_t zero(0);
  return verify_integer(loc, *this) ? *this : zero;
}
