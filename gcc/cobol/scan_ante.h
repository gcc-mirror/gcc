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
 * Flex override
 */
static void /* yynoreturn */ yy_fatal_error ( const char* msg  );

static void inline
die_fatal_error( const char msg[] ) {
  cbl_internal_error("scan.o: %s",  msg);
  yy_fatal_error(msg);
}

#define YY_FATAL_ERROR(msg) die_fatal_error((msg))

/*
 * External functions
 */

void parser_enter_file(const char *filename);
void parser_leave_file();

bool is_fixed_format();
bool include_debug();
int lexer_input( char buf[], int max_size, FILE *input );

const char * keyword_str( int token );

int repository_function_tok( const char name[] );

void cobol_set_indicator_column( int column );

void next_sentence_label(cbl_label_t*);

int repeat_count( const char picture[] );

size_t program_level();

int ydfparse(void);

FILE * copy_mode_start();

/*
 * Public functions and data
 */

cbl_label_t *next_sentence;

static bool echo_on = false;

void
lexer_echo( bool tf ) {
  echo_on = tf;
}

bool
lexer_echo() {
  return echo_on;
}

// IBM says a picture can be up to 50 bytes, not 1000 words.
// ISO says a picture can be up to 63 bytes.  We allow for a NUL terminator.
static char orig_picture[PICTURE_MAX];
static char orig_number[80];

const char *
original_picture() {
  const char *out = xstrdup(orig_picture);
  assert(orig_picture[0] != '\0');
  return out;
}

char *
original_number( char input[] = NULL ) {
  if( input ) {
    if(sizeof(orig_number) < strlen(input) ) return NULL;
    strcpy(orig_number, input);
    return input;
  }
  char *out = xstrdup(orig_number);
  assert(orig_number[0] != '\0');
  return out;
}

/*
 * Local functions
 */
static const char * start_condition_str( int sc );
static const char * start_condition_is();

static bool nonspace( char ch ) { return !ISSPACE(ch); }

static int
numstr_of( const char string[], radix_t radix = decimal_e ) {
  yylval.numstr.radix = radix;
  ydflval.string = yylval.numstr.string = xstrdup(string);
  char *comma = strchr(yylval.numstr.string, ',');
  if( comma && comma[1] == '\0' ) *comma = '\0';
  if( ! original_number(yylval.numstr.string) ) {
    error_msg(yylloc, "input inconceivably long");
    return NO_CONDITION;
  }

  const char *input = yylval.numstr.string;
  auto eoinput = input + strlen(input);
  auto p = std::find_if( input, eoinput,
                         []( char ch ) { return ch == 'e' || ch == 'E';} );

  if( p < eoinput ) {
    if( eoinput == std::find(input, eoinput, symbol_decimal_point()) ) {
      // no decimal point: 1E0 is a valid user-defined name
      ydflval.string = yylval.string = yylval.numstr.string;
      return NAME;
    }
    assert(input < p);
    // "The literal to the left of the 'E' represents the significand. It may
    //  be signed and shall include a decimal point. The significand shall be
    //  from 1 to 36 digits in length."
    if( p == std::find(input, p, symbol_decimal_point()) ) {
      return NO_CONDITION;
    }
    auto nx = std::count_if(input, p, fisdigit);
    if( 36 < nx ) {
      error_msg(yylloc, "significand of %s has more than 36 digits (%ld)", input, (long)nx);
      return NO_CONDITION;
    }

    // "The literal to the right of the 'E' represents the exponent. It may be
    //  signed and shall have a maximum of four digits and no decimal point. "
    // "The maximum permitted value and minimum permitted value of the
    //  exponent is implementor-defined." (We allow 9999.)
    nx = std::count_if(p, eoinput, fisdigit);
    if( 4 < nx ) {
      error_msg(yylloc, "exponent %qs more than 4 digits", ++p);
      return NO_CONDITION;
    }
    if( eoinput != std::find(p, eoinput, symbol_decimal_point()) ) {
      error_msg(yylloc, "exponent %qs includes decimal point", ++p);
      return NO_CONDITION;
    }

    // "If all the digits in the significand are zero, then all the digits of
    //  the exponent shall also be zero and neither significand nor exponent
    //  shall have a negative sign."
    bool zero_signficand = std::all_of( input, p,
                                        []( char ch ) {
                                          return !ISDIGIT(ch) || ch == '0'; } );
    if( zero_signficand ) {
      if( p != std::find(input, p, '-') ) {
        error_msg(yylloc, "zero significand of %s "
                 "cannot be negative", input);
        return NO_CONDITION;
      }
      if( eoinput != std::find(p, eoinput, '-') ) {
        error_msg(yylloc, "exponent of zero significand of %s "
                 "cannot be negative", input);
        return NO_CONDITION;
      }
    }
  }
  if( 1 < std::count(input, eoinput, symbol_decimal_point()) ) {
    error_msg(yylloc, "invalid numeric literal %qs", ++p);
    return NO_CONDITION;
  }

  return NUMSTR;
}

static char *
null_trim( char name[] ) {
  auto p = std::find_if( name, name + strlen(name), fisspace );
  if( p < name + strlen(name) ) *p = '\0';
  return name;
}

/*
 * CDF management
 */
static int final_token;

static inline const char *
boolalpha( bool tf ) { return tf? "True" : "False"; }

struct cdf_status_t {
  int lineno;
  const char *filename;
  int  token;
  bool parsing;
  cdf_status_t()
    : lineno(yylineno), filename(cobol_filename())
    , token(0), parsing(true)
  {}
  cdf_status_t( int token, bool parsing )
    : lineno(yylineno), filename(cobol_filename())
    , token(token), parsing(parsing)
  {}
  bool toggle() { return parsing = ! parsing; }

  const char * str() const {
    static char line[132];
    snprintf(line, sizeof(line), "%s:%d: %s, parsing %s",
             filename, lineno, keyword_str(token), boolalpha(parsing));
    return line;
  }
  static const char * as_string( const cdf_status_t& status ) {
    return status.str();
  }
};

/*
 * Scanning status is true if tokens are being parsed and false if not (because
 * CDF is skipping some code).  Because CDF status is nested, status is true
 * only if the whole stack is true. That is, if B is stacked on A, and A is
 * false, then all of B is skipped, regardless of >>IF and >>ELSE for B.
 */
static bool run_cdf( int token );

static class parsing_status_t : public std::stack<cdf_status_t> {
  typedef int (parser_t)(void);
  struct parsing_state_t {
    bool at_eof, expect_field_level;
    int pending_token;
    parser_t *parser;
    parsing_state_t()
      : at_eof(false)
      , expect_field_level(true)
      , pending_token(0)
      , parser(yyparse)
    {}
  } state, shadow;

 public:
  bool on() const { // true only if all true
    bool parsing = std::all_of( c.begin(), c.end(),
                               []( const auto& status ) { return status.parsing; } );
    return parsing;
  }

  bool feed_a_parser() const {
    return on() || state.parser == ydfparse;
  }

  void need_level( bool tf ) {     state.expect_field_level = tf; }
  bool need_level() const { return state.expect_field_level; }

  void parser_save( parser_t * new_parser ) {
    shadow = state;
    state.parser = new_parser;
  }
  void parser_restore() {
    state.parser = shadow.parser;
  }

  void inject_token( int token ) { state.pending_token = token; }
  int pending_token() {
    int token = state.pending_token;
    state.pending_token = 0;
    return token;
  }

  void at_eof( bool tf ) { state.at_eof = shadow.at_eof = tf; assert(tf); }
  bool at_eof() const { return state.at_eof; }

  bool in_cdf() const { return state.parser == ydfparse; }
  bool normal() const { return on() && state.parser == yyparse; }

  void splat() const {
    int i=0;
    for( const auto& status : c ) {
      yywarn( "%d %s", ++i, status.str() );
    }
  }
} parsing;

// Used only by parser, so scanner_normal() obviously true.
void field_done() { orig_picture[0] = '\0'; parsing.need_level(true); }

static int scanner_token() {
  if( parsing.empty() ) {
    error_msg(yylloc, "%<>>ELSE%> or %<>>END-IF%> without %<>>IF%>");
    return NO_CONDITION;
  }
  return parsing.top().token;
}

bool scanner_parsing() { return parsing.on(); }
bool scanner_normal()  { return parsing.normal(); }

void scanner_parsing( int token, bool tf ) {
  parsing.push( cdf_status_t(token, tf) );
  if( yydebug ) {
    yywarn("%s: parsing now %s, depth %zu",
            keyword_str(token), boolalpha(parsing.on()), parsing.size());
    parsing.splat();
  }
}
void scanner_parsing_toggle() {
  if( parsing.empty() ) {
    error_msg(yylloc, "%<>>ELSE%> without %<>>IF%>");
    return;
  }
  parsing.top().toggle();
  if( yydebug ) {
    yywarn("%s: parsing now %s",
            keyword_str(CDF_ELSE), boolalpha(parsing.on()));
  }
}
void scanner_parsing_pop() {
  if( parsing.empty() ) {
    error_msg(yylloc, "%<>>END-IF%> without %<>>IF%>");
    return;
  }
  parsing.pop();
  if( yydebug ) {
    yywarn("%s: parsing now %s, depth %zu",
            keyword_str(CDF_END_IF), boolalpha(parsing.on()),
	   parsing.size());
    parsing.splat();
  }
}


static bool level_needed() {
  return scanner_normal() && parsing.need_level();
}

static void level_found() {
  if( scanner_normal() ) parsing.need_level(false);
}

/*
 * Trim the scanned location by the amount about to re-scanned. 
 * Must be a macro because it expands yyless. 
 */
#define myless(N)				\
  do {						\
    auto n(N);					\
    trim_location(n);				\
    yyless(n);					\
  } while(0)

class enter_leave_t {
  typedef void( parser_enter_file_f)(const char *filename);
  typedef void (parser_leave_file_f)();
  parser_enter_file_f *entering;
  parser_leave_file_f *leaving;
  const char *filename;

 public:
  enter_leave_t() : entering(NULL), leaving(NULL), filename(NULL) {}
  enter_leave_t(  parser_enter_file_f *entering, const char *filename )
    : entering(entering), leaving(NULL), filename(filename)
  {}
  explicit enter_leave_t(parser_leave_file_f *leaving)
    : entering(NULL), leaving(leaving), filename(NULL) {}

  void notify() {
    if( entering ) {
      cobol_filename(filename, 0);
      if( yy_flex_debug ) dbgmsg("starting line %4d of %s",
                                 yylineno, filename);
      entering(filename);
      gcc_assert(leaving == NULL);
    }
    if( leaving ) {
      cobol_filename_restore();
      if( yy_flex_debug ) dbgmsg("resuming line %4d of %s",
                                 yylineno, cobol_filename());
      leaving();
      gcc_assert(entering == NULL);
    }
  }
};

static class input_file_status_t {
  std::queue <enter_leave_t> inputs;
 public:
  void enter(const char *filename) {
    inputs.push( enter_leave_t(parser_enter_file, filename) );
  }
  void leave() {
    inputs.push( enter_leave_t(parser_leave_file) );
  }
  void notify() {
    while( ! inputs.empty() ) {
      auto enter_leave = inputs.front();
      enter_leave.notify();
      inputs.pop();
    }
  }
} input_file_status;

void input_file_status_notify() { input_file_status.notify(); }

/*
 * parse.y and cdf.y each define a 4-integer struct to hold a token's location. 
 * parse.y uses   YYLTYPE  yylloc;
 * cdf.y   uses YDFLLTYPE ydflloc;
 * 
 * The structs have identical definitions with different types and of course
 * names.  We define "conversion" between them for convenience. 
 * 
 * Each parser expects its location value to be updated whenever it calls
 * yylex().  Therefore, here in the lexer we set both locations as each token
 * is scanned, so that both parsers see the same location.
 */
static YDFLTYPE
ydfltype_of( const YYLTYPE& loc ) {
  YDFLTYPE output { 
    loc.first_line,   loc.first_column,
    loc.last_line,    loc.last_column };
  return output;
}

/*
 * After the input filename and yylineno are set, update the location of the
 * scanned token.
 */
static void
update_location( const YYLTYPE *ploc = nullptr ) {
  YYLTYPE loc = {
    yylloc.last_line, yylloc.last_column,
    yylineno,         yylloc.last_column + yyleng
  };
  if( ploc ) loc = *ploc;

  const char *p = static_cast<char*>(memrchr(yytext, '\n', yyleng));
  if( p ) {
    loc.last_column = (yytext + yyleng) - p;
  }

  yylloc = loc;
  ydflloc = ydfltype_of(yylloc);

  dbgmsg("  SC: %s location (%d,%d) to (%d,%d)",
         start_condition_is(),
         yylloc.first_line, yylloc.first_column,
         yylloc.last_line,  yylloc.last_column);
}

static void
reset_location() {
  static const YYLTYPE loc { yylineno, 1, yylineno, 1 };
  update_location(&loc);
}

#define YY_USER_ACTION update_location();

static void
trim_location( int nkeep) {
  gcc_assert( 0 <= nkeep && nkeep <= yyleng );
  struct { char *p, *pend;
    size_t size() const { return pend - p; }
  } rescan = { yytext + nkeep, yytext + yyleng };

  auto nline = std::count(rescan.p, rescan.pend, '\n');
  dbgmsg("%s:%d: yyless(%d), rescan '%.*s' (" HOST_SIZE_T_PRINT_UNSIGNED
         " lines, " HOST_SIZE_T_PRINT_UNSIGNED " bytes)",
         __func__, __LINE__,
         nkeep,
         int(rescan.size()), rescan.p,
         (fmt_size_t)nline, (fmt_size_t)rescan.size());
  if( nline ) {
    gcc_assert( yylloc.first_line + nline <= yylloc.last_line );
    yylloc.last_line -= int(nline);
    gcc_assert( yylloc.first_line <= yylloc.last_line );
    char *p = static_cast<char*>(memrchr(rescan.p, '\n', rescan.size()));
    yylloc.last_column = rescan.pend - ++p;
    return;
  }

  gcc_assert( int(rescan.size()) < yylloc.last_column );
  yylloc.last_column -= rescan.size();
  if( yylloc.last_column < yylloc.first_column ) {
    yylloc.first_column = 1;
  }

  location_dump(__func__, __LINE__, "yylloc", yylloc);
}

static void
update_location_col( const char str[], int correction = 0) {
  auto col = yylloc.last_column - strlen(str) + correction;
  if( col > 0 ) {
    yylloc.first_column = col;
  }
  location_dump(__func__, __LINE__, "yylloc", yylloc);
}

#define not_implemented(...) cbl_unimplemented_at(yylloc, __VA_ARGS__)

#define YY_USER_INIT do {			\
    static YYLTYPE ones = {1,1, 1,1};		\
    yylloc = ones;                              \
    ydflloc = ydfltype_of(yylloc);              \
  } while(0)

/*
 * YY_DECL is the generated lexer.  The parser calls yylex().  yylex() invokes
 * next_token(), which calls this lexer function.  The Flex-generated code
 * updates neither yylval nor yylloc.  That job is left to the actions.
 *
 * The parser relies on yylex to set yylval and yylloc each time it is
 * called. It maintains a separate copy for each term, and uses
 * YYLLOC_DEFAULT() to update the location of nonterminals.
 */
#define YY_DECL int lexer(void)

# define YY_INPUT(buf, result, max_size)                        \
{                                                               \
  if( 0 == (result = lexer_input(buf, max_size, yyin)) )        \
    result = YY_NULL;                                           \
}

#define bcomputable(T, C)                               \
    yylval.computational.type=T,                        \
    yylval.computational.capacity=C,                    \
    yylval.computational.signable=true, BINARY_INTEGER
#define scomputable(T, C)                               \
    yylval.computational.type=T,                        \
    yylval.computational.capacity=C,                    \
    yylval.computational.signable=true, COMPUTATIONAL
#define ucomputable(T, C)                               \
    yylval.computational.type=T,                        \
    yylval.computational.capacity=C,                    \
    yylval.computational.signable=false, COMPUTATIONAL

static char *tmpstring = NULL;

#define PROGRAM current_program_index()

// map of alias => canonical
static std::map <std::string, std::string> keyword_aliases;

const std::string& 
keyword_alias_add( const std::string& keyword, const std::string& alias ) {
  auto p = keyword_aliases.find(alias);
  if( p != keyword_aliases.end() ) return p->second; // error: do not overwrite
  return keyword_aliases[alias] = keyword;
}

/*
 * Because numeric USAGE types don't have distinct tokens and may have aliases,
 * we keep a table of their canonical names, which we use if we encounter an
 * alias.
 */
struct bint_t {
  int token;
  cbl_field_type_t type;
  uint32_t capacity;
  bool signable;
};
static const std::map <std::string, bint_t > binary_integers {
  { "COMP-X", { COMPUTATIONAL, FldNumericBin5, 0xFF, false } }, 
  { "COMP-6", { COMPUTATIONAL, FldPacked, 0, false } }, 
  { "COMP-5", { COMPUTATIONAL, FldNumericBin5, 0, false } }, 
  { "COMP-4", { COMPUTATIONAL, FldNumericBinary, 0, true } }, 
  { "COMP-2", { COMPUTATIONAL, FldFloat, 8, false } }, 
  { "COMP-1", { COMPUTATIONAL, FldFloat, 4, false } }, 
  { "COMP", { COMPUTATIONAL, FldNumericBinary, 0, false } }, 
  { "COMPUTATIONAL-X", { COMPUTATIONAL, FldNumericBin5, 0xFF, false } }, 
  { "COMPUTATIONAL-6", { COMPUTATIONAL, FldPacked, 0, false } }, 
  { "COMPUTATIONAL-5", { COMPUTATIONAL, FldNumericBin5, 0, false } }, 
  { "COMPUTATIONAL-4", { COMPUTATIONAL, FldNumericBinary, 0, true } }, 
  { "COMPUTATIONAL-2", { COMPUTATIONAL, FldFloat, 8, false } }, 
  { "COMPUTATIONAL-1", { COMPUTATIONAL, FldFloat, 4, false } }, 
  { "COMPUTATIONAL", { COMPUTATIONAL, FldNumericBinary, 0, false } }, 
  { "BINARY", { BINARY_INTEGER, FldNumericBinary, 0, true } }, 
  { "BINARY-CHAR", { BINARY_INTEGER, FldNumericBin5, 1, true } }, 
  { "BINARY-SHORT", { BINARY_INTEGER, FldNumericBin5, 2, true } }, 
  { "BINARY-LONG", { BINARY_INTEGER, FldNumericBin5, 4, true } }, 
  { "BINARY-DOUBLE", { BINARY_INTEGER, FldNumericBin5, 8, true } }, 
  { "BINARY-LONG-LONG", { BINARY_INTEGER, FldNumericBin5, 8, true } }, 
  { "FLOAT-BINARY-32", { COMPUTATIONAL, FldFloat, 4, false } }, 
  { "FLOAT-BINARY-64", { COMPUTATIONAL, FldFloat, 8, false } }, 
  { "FLOAT-BINARY-128", { COMPUTATIONAL, FldFloat, 16, false } }, 
  { "FLOAT-EXTENDED", { COMPUTATIONAL, FldFloat, 16, false } }, 
  { "FLOAT-LONG", { COMPUTATIONAL, FldFloat, 8, false } }, 
  { "FLOAT-SHORT", { COMPUTATIONAL, FldFloat, 4, false } }, 
};

static int
binary_integer_usage( const char name[]) {
  cbl_name_t uname = {};
  std::transform(name, name + strlen(name), uname, ftoupper);

  dbgmsg("%s:%d: checking %s in %zu keyword_aliases",
	 __func__, __LINE__, uname,
	 keyword_aliases.size() );

  std::string key = uname;
  auto alias = keyword_aliases.find(key);
  if( alias != keyword_aliases.end() ) key = alias->second;
  
  auto p = binary_integers.find(key);
  if( p == binary_integers.end() ) return 0;
  
  yylval.computational.type = p->second.type;
  yylval.computational.capacity = p->second.capacity;
  yylval.computational.signable = p->second.signable;
  dbgmsg("%s:%d: %s has type %d", __func__, __LINE__,
	 uname, p->second.type );
  return p->second.token;
}
      
static void
verify_ws( const YYLTYPE& loc, const char input[], char ch ) {
  if( ! fisspace(ch) ) {
    if( ! (dialect_mf() || dialect_gnu()) ) {
      dialect_error(loc, "separator space required in %qs", input);
    }
  }
}
#define verify_ws(C) verify_ws(yylloc, yytext, C)

int
binary_integer_usage_of( const char name[] ) {
  cbl_name_t uname = {};
  std::transform(name, name + strlen(name), uname, ftoupper);

  auto p = binary_integers.find(uname);
  if( p != binary_integers.end() ) {
    int token = p->second.token;
    switch( token ) {
    case COMPUTATIONAL:
    case BINARY_INTEGER:
      return token;
    default:
      gcc_unreachable();
      assert(false);
    }
  }
  return 0;
}

static uint32_t
level_of( const char input[] ) {
  unsigned int output = 0;

  if( input[0] == '0' ) input++;

  if( 1 != sscanf(input, "%u", &output) ) {
    yywarn( "%s:%d: invalid level '%s'", __func__, __LINE__, input );
  }

  return output;
}

static inline int
ndigit(int len) {
  char *input = TOUPPER(yytext[0]) == 'V'? yytext + 1 : yytext;
  int n = repeat_count(input);
  return n == -1? len : n;
}

static int
picset( int token ) {
  static const char * const eop = orig_picture + sizeof(orig_picture);
  char *p = orig_picture + strlen(orig_picture);

  if( eop < p + yyleng ) {
    error_msg(yylloc, "PICTURE exceeds maximum size of %zu bytes",
             sizeof(orig_picture) - 1);
  }
  snprintf( p, eop - p, "%s", yytext );
  return token;
}

/**
## Script and data to produce picture_t::followers.
## Based on ISO Table 10. 
#! /usr/bin/awk -f

BEGIN  {
  str = "B0/ , . + +- +- CR/DB cs cs Z* Z* + + cs cs 9 AX S V P P 1 N E"
  split(str, cols)
}

$1 ~ /CR|DB|cs/ { next }

0 && !nlines++ {
  for( i=0; i < length(cols); i++ ) {
    print i, cols[i], "'" $i "'"
  }
}

$field == "x" {
  if( ! nout++ ) {
    printf "%2d: %5s: \"", field, cols[field - 1]
  }

  gsub(/^ +| +$/, "", $1) 
  printf "%s", $1
}

END {
  if( ! nout++ ) {
    printf "%2d: %5s: \"", field, cols[field - 1]
  }
  print "\""
}

B  x x x - x - - x - x x x x x x x x - x - x - x
0  x x x - x - - x - x x x x x x x x - x - x - x
/  x x x - x - - x - x x x x x x x x - x - x - x
,  x x x - x - - x - x x x x x x x - - x - x
.  x x - - x - - x - x - x - x - x
+  - - - - - - - - - - - - - - - - - - - - - - - x
+
–
+  x x x - - - - x x x x - - x x x - - x x x
CR x x x - - - - x x x x - - x x x - - x x x
DB x x x - - - - x x x x - - x x x - - x x x
cs - - - - x
cs x x x - x - - - - x x - - - - x - - x x x

Z  x x - - x - - x - x
*  x x - - x - - x - x
Z  x x x - x - - x - x x - - - - - - - x - x
*  x x x - x - - x - x x - - - - - - - x - x
+  x x - - - - - x - - - x
–  x x - - - - - x - - - x
+  x x x - - - - x - - - x x - - - - - x
–  x x x - - - - x - - - x x - - - - - x
cs x x - - x - - - - - - - - x
cs x x x - x - - - - - - - - x x - - - x

9  x x x x x - - x - x - x - x - x x x x - x - - x
A  x - - - - - - - - - - - - - - x x
X  x - - - - - - - - - - - - - - x x
S 
V  x x - - x - - x - x - x - x - x - x - x
P  x x - - x - - x - x - x - x - x - x - x
P  - - - - x - - x - - - - - - - - - x x - x
1  - - - - - - - - - - - - - - - - - - - - - x
N  x - - - - - - - - - - - - - - - - - - - - - x
E  x x x - x - - - - - - - - - - x
**/

class picture_t {
  static const char dot = '.', comma = ',';

  typedef std::vector<std::string> followings_t;
  static const std::map <char, followings_t> followers;
  
  const char * const begin;
  const char *p, *pend; 
  size_t pos;
  struct exclusions_t { // Nonzero if set, > 1 is false.
    // crdb means CR/DB or +/-.
    // pluses means 2 or more consecutive '+'.
    // minuses means 2 or more consecutive '-'.
    // "21) The symbol 'Z' and the symbol '*' are mutually exclusive "
    // stars means '*' or Z.
    unsigned short int crdb, currency, dot, pluses, minuses, stars, zzz;
    exclusions_t()
      : crdb(0), currency(0), dot(0), pluses(0), minuses(0), stars(0)
    {}
  } exclusions;
  YYLTYPE loc;
  
  bool is_crdb() const { // input must be uppercase for CR/DB
    if( p[0] == 'C' || p[0] == 'D' ) {
      char input[3] = { p[0], p[1] };
      return ( 0 == strcmp(input, "CR") || 0 == strcmp(input, "DB") );
    }
    return false;
  }

  const char * match_paren( const char *paren ) const {
    gcc_assert(paren[0] == '(');  // start with opening paren
    paren = std::find_if( paren, pend,
                          []( char ch ) {
                            return ch == '(' || ch == ')';
                          } );
    if( *paren == '(' ) return nullptr; // no nesting
    if( paren == pend ) return nullptr;
    return ++paren;
  }

  const char * next_not( char ch ) const {
    return std::find_if( p, pend,
                         [ch = TOUPPER(ch)]( char next ) {
                           return ch != next;
                         } );
  }

  const char * valid_next( const char *p, const std::string& valid ) const {
    if( p == pend || p + 1 == pend ) return pend;
    if( p[1] == '(' ) {
      return match_paren(++p);
    }
    auto pv = std::find(valid.begin(), valid.end(), TOUPPER(p[1]));
    return pv != valid.end()? ++p : nullptr;
  }
  const char * valid_next( const char *p,
                           bool first = true, char ch = '\0' ) const {
    if( p == pend || p + 1 == pend ) return pend;
    if( p[0] == '(' ) {
      if( (p = match_paren(p)) == nullptr ) return nullptr;
    }
    if( p[0] == '(' ) return nullptr;  // consecutive parentheses
    
    int index = first? 0 : 1;
    if( !ch ) ch = *p;   // use current character unless overridden
    auto valid = followers.find(TOUPPER(ch));
    if( valid == followers.end() ) {
      YYLTYPE loc(yylloc);
      loc.first_column += int(p - begin);
      error_msg( loc, "PICTURE: strange character %qc, giving up", ch );
      return nullptr;
    }
    return valid_next(p, valid->second[index]);
  }

  const char * start() { // start modifies exclusions, but not p
    auto pnext = p;

    switch(TOUPPER(p[0])) {
    case comma: case dot:
      // use decimal_is_comma()
      //  4:     .: "B0/,+Z*+-9E"
      exclusions.dot++;
      pnext = valid_next(p, "B0/,+Z*+-9E");
      break;
    case '+': case '-':
      //  6:    +-: "B0/,.Z*Z*9VPPE"
      exclusions.crdb++;
      pnext = next_not(p[0]);
      if( p + 1 < pnext ) {
        exclusions.pluses++;
      }
      pnext = valid_next(--pnext, "B0/,.Z*Z*9VPPE");
      break;
    case 'Z': case '*': 
      exclusions.stars++;
      pnext = next_not(p[0]);
      break;
    case 'S':
      // 19:     S: "9VP"
      pnext = valid_next(p, "9VP");
      break;
    }

    /*
     * "For fixed editing sign control, the currency symbol, when used, shall
     * be either the leftmost symbol in character-string-1, optionally preceded
     * by one of the symbols '+' or '-' "
     */
    if( pnext ) {
      if( p == pnext || p[0] == '+' || p[0] == '-' ) {
        if( symbol_currency(*pnext) ) {
          exclusions.currency++;
          pnext = next_not(*pnext);
          pnext = valid_next(--pnext, true, '$');
        }
      }
    }
    
    return pnext;
  }

  const char * next() { // modify state; do not modify position
    auto pnext = p;
    auto loc(picture_t::loc);
    loc.first_column += int(p - begin);

    if( is_crdb() ) {
      if( exclusions.crdb++ ) {
        error_msg( loc, "PICTURE: CR/DB and %c/%c may appear only once", '+', '-' );
        return nullptr;
      }
      if( p + 2 != pend ) { 
        error_msg( loc, "PICTURE: CR/DB must appear at the end" );
        return nullptr;
      }
      return pend;
    }

    if( symbol_currency(p[0]) ) {
      if( false && exclusions.currency++ ) { // not enforced
        error_msg( loc, "PICTURE: CURRENCY SYMBOL sequence may appear at most once" );
        return nullptr;
      }
      return valid_next(p, ! exclusions.dot, '$');
    }

    switch(TOUPPER(p[0])) {
    case '(':
      return match_paren(p);
      break;
    case 'B': case '0': case '/':
      pnext = valid_next(p);
      break;
    case comma: 
      if( decimal_is_comma() ) {
        if( exclusions.dot++ ) {
          error_msg( loc, "PICTURE: %qc: may appear at most once", p[0] );
          return nullptr;
        }
        pnext = valid_next(p, true, dot);
      } else {
        pnext = valid_next(p);
      }
      break;
    case dot: 
      if( p + 1 == pend ) {
        pnext = pend;
      } else {
        if( decimal_is_comma() ) {
          pnext = valid_next(p, true, comma );
        } else {
          if( exclusions.dot++ ) {
            error_msg( loc, "PICTURE: %qc: may appear at most once", p[0] );
            return nullptr;
          }
          pnext = valid_next(p);
        }
      }
      break;

    case '+': case '-':
      // 7 is trailing sign; 13 & 14 are numeric.  Leading sign handled by start(). 
      if( p + 1 == pend ) {
        if( exclusions.crdb++ ) {
          error_msg( loc, "PICTURE: %c/%c may appear at most once as a sign", '+', '-' );
          return nullptr;
        }
        pnext = pend;
      } else {
        pnext = next_not(p[0]);
        if( p + 1 < pnext ) {
          if( false && exclusions.pluses++ )  { // not enforced
            error_msg( loc, "PICTURE: %qc: sequence may appear at most once", p[0] );
            return nullptr;
          }
        }
        pnext = valid_next(pnext, ! exclusions.dot);
      }
      break;

    case 'Z': case '*':
      if( false && exclusions.stars++ ) { // not enforced 
        error_msg( loc, "PICTURE: %qc: sequence may appear at most once", p[0] );
        return nullptr;
      }
      if( (pnext = next_not(p[0])) == nullptr ) return pnext;
      pnext = valid_next(pnext, ! exclusions.dot);
      break;
    case 'P':
      pnext = valid_next(pnext, ! exclusions.dot);
      break;
    case '9':
    case 'A': case 'X':
    case 'V':
    case '1':
    case 'N':
      pnext = valid_next(p);
      break;
    case 'E':
      pnext = valid_next(p, "+9");
      if( pnext && *pnext == '+' ) {
        pnext = valid_next(p, "9");
      }
      break;
    default:
      error_msg( loc, "PICTURE: %qc: invalid character", p[0] );
      return nullptr;
    }
    return pnext;
  }
  
 public:
  picture_t( const char *p, int len )
    : begin(p)
    , p(p), pend(p + len)
    , loc(yylloc)
  { 
    assert(TOUPPER(*p) == 'P'); // as in PICTURE (or PICTURE IS)
    // move p to start of picture string
    while( (p = std::find_if(p, pend, fisspace)) != pend ) {
      this->p = p = std::find_if(p, pend,
                                 []( char ch ) { return ! fisspace(ch); } );
    }
    assert(this->p != pend);
    pos = this->p - begin;
  }

  bool is_valid() {
    if( !p ) return false;
    if( (p = start()) == nullptr ) {
      return false;
    }

    while( p && p < pend) {
      p = next();
    }
    return p == pend;
  }

  int starts_at() const { return pos; }
};

/*
 * The Followers map gives 1 or 2 lists of valid characters following a
 * character, the one in the key. If there are two lists, the correct one is
 * determined by the caller based on the state of the picture string, i.e.,
 * what has been seen before.
 */
const std::map <char, picture_t::followings_t> picture_t::followers {
  /*   B0/ */ { 'B', {"B0/,.Z*+-9AXVPNE" } },
  /*   B0/ */ { '0', {"B0/,.Z*+-9AXVPNE" } },
  /*   B0/ */ { '/', {"B0/,.Z*+-9AXVPNE" } },
  /*     , */ { ',', {"B0/,.Z*+-9VPE"} },
  /*     . */ { '.', {"B0/,Z*+-9E"} },
  /*     +    { '+', "9" }, */
  /*    +- */ { '+', {"B0/,.Z*9VPE", "" } },
  /*    +- */ { '-', {"B0/,.Z*9VPE", "" } },
  /* CR/DB    { 'C', "" }, */
  /*    cs    { 'c', "B0/,.Z*+-9VP" }, */
  /*    cs    { 'c', "+" }, */
  /*    Z* */ { 'Z', {"B0/,.+Z*9VP", "B0/,+Z*"} },
  /*    Z* */ { '*', {"B0/,.+Z*9VP", "B0/,+Z*"} },
  /*     + */ { '+', {"B0/,.+-9VP",  "B0/,+-"} },
  /*    cs */ { '$', {"B0/,.+9VP",   "B0/,+"} },
  /*     9 */ { '9', {"B0/,.+9AXVPE"} },
  /*    AX */ { 'A', {"B0/9AX"} },
  /*    AX */ { 'X', {"B0/9AX"} },
  /*     S */ { 'S', {"9VP"} },
  /*     V */ { 'V', {"B0/,+Z*+-9P"} },
  /*     P */ { 'P', {"+VP", "B0/,+Z*9P"} },
  /*     1 */ { '1', {"1"} },
  /*     N */ { 'N', {"B0/N"} },
  /*     E */ { 'E', {"+9"} },
};

/*
 * Although picture_t::is_valid return a bool, it's not used. The validation
 * routines emit messages where the error is detected. The entire string is
 * subsequently parsed by the parser, which might otherwise accept an invalid
 * string, but will usually emit a message of its own.
 */
static int 
validate_picture() {
  picture_t picture(yytext, yyleng);
  picture.is_valid();
  return  picture.starts_at();
}

static inline bool
is_integer_token( int *pvalue = NULL ) {
  int v, n = 0;
  if( pvalue == NULL ) pvalue = &v;
  return 1 == sscanf(yytext, "%d%n", pvalue, &n) && n == yyleng;
}

static bool need_nume = false;
bool need_nume_set( bool tf ) {
  dbgmsg( "need_nume now %s", tf? "true" : "false" );
  return need_nume = tf;
}

static int datetime_format_of( const char input[] );

static int symbol_function_token( const char name[] ) {
  const auto e = symbol_function( 0, name );
  return e ? symbol_index(e) : 0;
}

bool in_procedure_division(void );

static symbol_elem_t *
symbol_exists( const char name[] ) {
  typedef std::map <std::string, size_t> name_cache_t;
  static std::map <size_t, name_cache_t> cachemap;

  cbl_name_t lname;
  std::transform( name, name + strlen(name) + 1, lname, tolower );
  auto& cache = cachemap[PROGRAM];

  if( in_procedure_division() && cache.empty() ) {
    for( auto e = symbols_begin(PROGRAM) + 1;
         PROGRAM == e->program && e < symbols_end(); e++ ) {
      if( e->type == SymFile ) {
        cbl_file_t *f(cbl_file_of(e));
        cbl_name_t lname;
        std::transform( f->name, f->name + strlen(f->name) + 1, lname, tolower );
        cache[lname] = symbol_index(e);
        continue;
      }
      if( e->type == SymField ) {
        auto f(cbl_field_of(e));
        cbl_name_t lname;
        std::transform( f->name, f->name + strlen(f->name) + 1, lname, tolower );
        cache[lname] = symbol_index(e);
      }
    }
    cache.erase("");
  }
  auto p = cache.find(lname);

  if( p == cache.end() ) {
    symbol_elem_t * e = symbol_field( PROGRAM, 0, name );
    return e;
  }

  return symbol_at(p->second);
}

static int
typed_name( const char name[] ) {
  if( 0 == PROGRAM ) return NAME;
  if( need_nume ) { need_nume_set(false); return NUME; }

  int token = repository_function_tok(name);
  switch(token) {
  case 0:
    break;
  case FUNCTION_UDF_0:
    yylval.number = symbol_function_token(name);
    __attribute__((fallthrough));
  default:
    return token;
  }

  struct symbol_elem_t *e = symbol_special( PROGRAM, name );
  if( e ) return  cbl_special_name_of(e)->token;

  if( (token = redefined_token(name)) ) { return token; }

  e = symbol_exists( name );

  auto type = e && e->type == SymField? cbl_field_of(e)->type : FldInvalid;

  switch(type) {
  case FldLiteralA:
    {
      auto f = cbl_field_of(e);
      if( is_constant(f) ) {
	if(  f->data.initial ) {
	  int token = cbl_figconst_tok(f->data.initial);
	  if( token ) return token;
	}
        int token = datetime_format_of(f->data.initial);
        if( token ) {
          yylval.string = xstrdup(f->data.initial);
          return token;
        }
      }
    }
    __attribute__((fallthrough));
  case FldLiteralN:
    {
      const auto f = cbl_field_of(e);
      if( type == FldLiteralN ) {
        yylval.numstr.radix =
          f->has_attr(hex_encoded_e)? hexadecimal_e : decimal_e;
        yylval.numstr.string = xstrdup(f->data.initial);
        return NUMSTR;
      }
      if( !f->has_attr(record_key_e) ) { // not a key-name literal
        yylval.literal.set(f);
        ydflval.string = yylval.literal.data;
        return LITERAL;
      }
    }
    __attribute__((fallthrough));
  case FldInvalid:
  case FldGroup:
  case FldForward:
  case FldIndex:
  case FldAlphanumeric:
  case FldPacked:
  case FldNumericDisplay:
  case FldNumericEdited:
  case FldAlphaEdited:
  case FldNumericBinary:
  case FldFloat:
  case FldNumericBin5:
  case FldPointer:
    return NAME;
  case FldSwitch:
    return SWITCH;
  case FldClass:
    return cbl_field_of(e)->level == 88? NAME88 : CLASS_NAME;
    break;
  default:
    yywarn("%s:%d: invalid symbol type %s for symbol %qs",
          __func__, __LINE__, cbl_field_type_str(type), name);
    return NAME;
  }
  return cbl_field_of(e)->level == 88? NAME88 : NAME;
}

int
retype_name_token() {
  return typed_name(ydflval.string);
}

static char *
tmpstring_append( int len ) {
  const char *extant = tmpstring == NULL ? "" : tmpstring;
  char *s = xasprintf("%s%.*s", extant, len, yytext);
  free(tmpstring);
  return tmpstring = s;
}

#define pop_return yy_pop_state(); return

static bool is_not = false;

static uint64_t
integer_of( const char input[], bool is_hex = false) {
  uint64_t output = 0;
  const char *fmt = is_hex? "%ul" : "%hl";

  if( input[0] == '0' ) input++;

  if( 1 != sscanf(input, fmt, &output) ) {
    yywarn( "%s:%d: invalid integer '%s'", __func__, __LINE__, input );
  }

  return output;
}
