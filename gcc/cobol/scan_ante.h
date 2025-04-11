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
      error_msg(yylloc, "significand of %s has more than 36 digits (%zu)", input, nx);
      return NO_CONDITION;
    }

    // "The literal to the right of the 'E' represents the exponent. It may be
    //  signed and shall have a maximum of four digits and no decimal point. "
    // "The maximum permitted value and minimum permitted value of the
    //  exponent is implementor-defined." (We allow 9999.)
    nx = std::count_if(p, eoinput, fisdigit);
    if( 4 < nx ) {
      error_msg(yylloc, "exponent %s more than 4 digits", ++p);
      return NO_CONDITION;
    }
    if( eoinput != std::find(p, eoinput, symbol_decimal_point()) ) {
      error_msg(yylloc, "exponent includes decimal point", ++p);
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
    error_msg(yylloc, "invalid numeric literal", ++p);
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
  cdf_status_t( int token = 0, bool parsing = true )
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
      yywarn( "%4d\t%s", ++i, status.str() );
    }
  }
} parsing;

// Used only by parser, so scanner_normal() obviously true.
void field_done() { orig_picture[0] = '\0'; parsing.need_level(true); }

static int scanner_token() {
  if( parsing.empty() ) {
    error_msg(yylloc, ">>ELSE or >>END-IF without >>IF");
    return NO_CONDITION;
  }
  return parsing.top().token;
}

bool scanner_parsing() { return parsing.on(); }
bool scanner_normal()  { return parsing.normal(); }

void scanner_parsing( int token, bool tf ) {
  parsing.push( cdf_status_t(token, tf) );
  if( yydebug ) {
    yywarn("%10s: parsing now %5s, depth %zu",
            keyword_str(token), boolalpha(parsing.on()), parsing.size());
    parsing.splat();
  }
}
void scanner_parsing_toggle() {
  if( parsing.empty() ) {
    error_msg(yylloc, ">>ELSE without >>IF");
    return;
  }
  parsing.top().toggle();
  if( yydebug ) {
    yywarn("%10s: parsing now %5s",
            keyword_str(CDF_ELSE), boolalpha(parsing.on()));
  }
}
void scanner_parsing_pop() {
  if( parsing.empty() ) {
    error_msg(yylloc, ">>END-IF without >>IF");
    return;
  }
  parsing.pop();
  if( yydebug ) {
    yywarn("%10s: parsing now %5s, depth %zu",
            keyword_str(CDF_END_IF), boolalpha(parsing.on()), parsing.size());
    parsing.splat();
  }
}


static bool level_needed() {
  return scanner_normal() && parsing.need_level();
}

static void level_found() {
  if( scanner_normal() ) parsing.need_level(false);
}

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
    : entering(entering), leaving(NULL), filename(filename) {}
  enter_leave_t(parser_leave_file_f *leaving)
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
      auto name = cobol_filename_restore();
      if( yy_flex_debug ) dbgmsg("resuming line %4d of %s",
                                 yylineno, name? name : "<none>");
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
    inputs.push( parser_leave_file );
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

void cdf_location_set(YYLTYPE loc);

static void
update_location() {
  YYLTYPE loc = {
    yylloc.last_line, yylloc.last_column,
    yylineno,         yylloc.last_column + yyleng
  };

  auto nline = std::count(yytext, yytext + yyleng, '\n');
  if( nline ) {
    char *p = static_cast<char*>(memrchr(yytext, '\n', yyleng));
    loc.last_column = (yytext + yyleng) - p;
  }

  yylloc = loc;
  cdf_location_set(loc);
  location_dump(__func__, __LINE__, "yylloc", yylloc);
}

static void
trim_location( int nkeep) {
  gcc_assert( 0 <= nkeep && nkeep <= yyleng );
  struct { char *p, *pend;
    size_t size() const { return pend - p; }
  } rescan = { yytext + nkeep, yytext + yyleng };

  auto nline = std::count(rescan.p, rescan.pend, '\n');
  dbgmsg("%s:%d: yyless(%d), rescan '%.*s' (%zu lines, %d bytes)",
         __func__, __LINE__,
         nkeep,
         int(rescan.size()), rescan.p,
         nline, rescan.size());
  if( nline ) {
    gcc_assert( yylloc.first_line + nline <= yylloc.last_line );
    yylloc.last_line =- int(nline);
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
    yylloc = ones;				\
  } while(0)

/*
 * YY_DECL is the generated lexer.  The parser calls yylex().  yylex() invokes
 * next_token(), which calls this lexer function.  The Flex-generated code
 * updates neither yylval nor yylloc.  That job is left to the actions.
 *
 * The parser relies on yylex to set yylval and yylloc each time it is
 * called. It apparently maintains a separate copy for each term, and uses
 * YYLLOC_DEFAULT() to update the location of nonterminals.
 */
#define YY_DECL int lexer(void)

#define YY_USER_ACTION							\
  update_location();							\
  if( yy_flex_debug ) dbgmsg("SC: %s", start_condition_is() );

# define YY_INPUT(buf, result, max_size)                        \
{                                                               \
  if( 0 == (result = lexer_input(buf, max_size, yyin)) )        \
    result = YY_NULL;                                           \
}

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
  auto e = symbol_function( 0, name );
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
      auto f = cbl_field_of(e);
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
    yywarn("%s:%d: invalid symbol type %s for symbol \"%s\"",
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

static bool
wait_for_the_child(void) {
  pid_t pid;
  int status;

  if( (pid = wait(&status)) == -1 ) {
    yywarn("internal error: no pending child CDF parser process");
    return false;
  }

  if( WIFSIGNALED(status) ) {
    yywarn( "process %d terminated by %s", pid, strsignal(WTERMSIG(status)) );
    return false;
  }
  if( WIFEXITED(status) ) {
    if( WEXITSTATUS(status) != 0 ) {
      yywarn("process %d exited with status %d", pid, status);
      return false;
    }
  }
  if( yy_flex_debug ) {
    yywarn("process %d exited with status %d", pid, status);
  }
  return true;
}

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
