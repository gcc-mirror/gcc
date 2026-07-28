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

static const char *
start_condition_str( int sc ) {
  const char *state = "???";
  switch(sc) {
  case INITIAL: state = "INITIAL"; break;
  case addr_of: state = "addr_of"; break;
  case basis: state = "basis"; break;
  case bool_state: state = "bool_state"; break;
  case cdf_state: state = "cdf_state"; break;
  case classify: state = "classify"; break;
  case cobol_words: state = "cobol_words"; break;
  case comment_entries: state = "comment_entries"; break;
  case copy_state: state = "copy_state"; break;
  case date_state: state = "date_state"; break;
  case datetime_fmt: state = "datetime_fmt"; break;
  case dot_state: state = "dot_state"; break;
  case exception: state = "exception"; break;
  case field_level: state = "field_level"; break;
  case field_state: state = "field_state"; break;
  case function: state = "function"; break;
  case hex_state: state = "hex_state"; break;
  case ident_state: state = "ident_state"; break;
  case integer_count: state = "integer_count"; break;
  case name_state: state = "name_state"; break;
  case numeric_state: state = "numeric_state"; break;
  case numstr_state: state = "numstr_state"; break;
  case partial_name: state = "partial_name"; break;
  case picture: state = "picture"; break;
  case picture_count: state = "picture_count"; break;
  case procedure_div: state = "procedure_div"; break;
  case program_id_state: state = "program_id_state"; break;
  case quoted1: state = "quoted1"; break;
  case quoted2: state = "quoted2"; break;
  case quoteq: state = "quoteq"; break;
  case raising: state = "raising"; break;
  case sort_state: state = "sort_state"; break;
  case subscripts: state = "subscripts"; break;
  }
  return state;
}

static const char *
start_condition_is() { return start_condition_str( YY_START ); }

/*
 * Match datetime constants.
 *
 * A 78 or CONSTANT could have a special literal for formatted
 * date/time functions.
 */

static int
datetime_format_of( const char input[] ) {

  static const char date_fmt_b[] = "YYYYMMDD|YYYYDDD|YYYYWwwD";
  static const char date_fmt_e[] = "YYYY-MM-DD|YYYY-DDD|YYYY-Www-D";

  static const char time_fmt_b[] =
    "hhmmss([.,]s+)?|hhmmss([.,]s+)?Z|hhmmss([.,]s+)?[+]hhmm|";
  static const char time_fmt_e[] =
    "hh:mm:ss([.,]s+)?|hh:mm:ss([.,]s+)?Z|hh:mm:ss([.,]s+)?[+]hh:mm";

  static char date_pattern[ 3 * sizeof(date_fmt_e) ];
  static char time_pattern[ 3 * sizeof(time_fmt_e) ];
  static char datetime_pattern[ 6 * sizeof(time_fmt_e) ];

  static struct pattern_t {
    regex_t re;
    const char *regex;
    int token;
  } patterns[] = {
    { {}, datetime_pattern, DATETIME_FMT },
    { {}, date_pattern, DATE_FMT },
    { {}, time_pattern, TIME_FMT },
  }, * eopatterns = patterns + COUNT_OF(patterns);;

  // compile patterns
  if( ! date_pattern[0] ) {
    sprintf(date_pattern, "%s|%s", date_fmt_b, date_fmt_e);
    sprintf(time_pattern, "%s|%s", time_fmt_b, time_fmt_e);

    sprintf(datetime_pattern, "(%sT%s)|(%sT%s)",
            date_fmt_b, time_fmt_b,
            date_fmt_e, time_fmt_e);

    for( auto p = patterns; p < eopatterns; p++ ) {
      static const int cflags = REG_EXTENDED | REG_ICASE;
      int erc;

      if( 0 != (erc = regcomp(&p->re, p->regex, cflags)) ) {
        static char msg[80];
        regerror(erc, &p->re, msg, sizeof(msg));
        cbl_internal_error("%s:%d: %s: %s", __func__, __LINE__,
                           keyword_str(p->token), msg);
      }
    }
  }

  // applies only in the datetime_fmt start condition
  if( datetime_fmt == YY_START ) {
    yy_pop_state();
    if( input == NULL ) return 0;

    // See if the input is a date, time, or datetime pattern string.
    static const int nmatch = 3;
    regmatch_t matches[nmatch];

    auto p = std::find_if( patterns, eopatterns,
                           [input, &matches]( auto& pattern ) {
                             auto erc = regexec( &pattern.re, input,
                                                 COUNT_OF(matches), matches, 0 );
                             return erc == 0;
                           } );

    return p != eopatterns? p->token : 0;
  }
  return 0;
}


/*
 * >>DEFINE, >>IF, and >>EVALUATE
 */

static bool
is_cdf_token( int token ) {
  switch(token) {
  case CDF_DEFINE:
  case CDF_DISPLAY:
  case CDF_IF:       case CDF_ELSE: case CDF_END_IF:
  case CDF_EVALUATE: case CDF_WHEN: case CDF_END_EVALUATE:
  case CDF_PUSH:
  case CDF_POP:
    return true;
  case CALL_COBOL:
  case CALL_VERBATIM:
  case COPY:
  case TURN:
    return true;
  }
  return false;
}

static bool
is_cdf_condition_token( int token ) {
  switch(token) {
  case CDF_IF:       case CDF_ELSE: case CDF_END_IF:
  case CDF_EVALUATE: case CDF_WHEN: case CDF_END_EVALUATE:
    return true;
  }
  return false;
}

/*
 * IF and EVALUATE are partially parsed in cdf.y.  ELSE and WHEN, etc., are
 * valid only in context.
 */
static bool
valid_conditional_context( int token ) {
  switch(token) {
  case CDF_DEFINE:
  case CDF_IF:
  case CDF_EVALUATE:
    return true;
  case CDF_ELSE:
  case CDF_END_IF:
    return scanner_token() == CDF_IF;
  case CDF_WHEN:
  case CDF_END_EVALUATE:
    return scanner_token() == CDF_EVALUATE;
  }
  return true; // all other CDF tokens valid regardless of context
}

static int ydfparse() {
  return cdf_parser.parse();
}

static inline int ydfchar() {
  auto kind_type = cdf_context.lookahead().kind();
  return kind_type;
}

void
ydfdebug( bool yn ) {
  cdf_parser.set_debug_level(yn? 1 : 0);
}

static bool
run_cdf( int token ) {
  if( ! valid_conditional_context(token) ) {
    error_msg(yylloc, "CDF syntax error at '%s'", keyword_str(token));
    return false;
  }

  parsing.inject_token(token); // because it will be needed by CDF parser

  if( yy_flex_debug )
    dbgmsg("%s: CDF parser start with '%s'", __func__, keyword_str(token));

  parsing.parser_save(ydfparse);

  int erc = ydfparse();  // Parse the CDF directive.

  parsing.parser_restore();

  if( YY_START == cdf_state ) yy_pop_state();

  if( yy_flex_debug ) {
    dbgmsg("%s: CDF parser returned %d, scanner SC <%s>", __func__, 
           erc, start_condition_is());
  }

  return  0 == erc;
}

#include <queue>
struct recent_token_t {
  int token;
  YYSTYPE value;
  cbl_loc_t loc;
  recent_token_t( int token, YYSTYPE value, cbl_loc_t loc )
    : token(token), value(value), loc(loc) {}
  // for matching the current token against the token queue
  recent_token_t( int token, cbl_loc_t loc )
    : token(token), loc(loc) {}
};
#define RECENT(T) recent_token_t( (T), yylval )

namespace cdf {
  int used_token();
  cbl_loc_t location();
}

static bool
same_end(const cbl_loc_t& a, const cbl_loc_t& b ) {
  return a.last_line == b.last_line
    &&   a.last_column == b.last_column;
}

/*
 * Two tokens are one and the same if they have the type and end at the same
 * place.  The scanner's idea of the current location might embrace more
 * characters.
 */
static bool
same_token( const recent_token_t& a, const recent_token_t& b ) {
  return a.token == b.token && same_end(a.loc, b.loc);
}

/*
 * Each time the generated scanner returns a token from lexer(), it's pushed on
 * the recent_tokens queue.  The queue is 2 deep, representing the CDF parser's
 * current and lookahead tokens. 
 */
static struct recent_tokens_t : protected std::queue<recent_token_t>
{
  const recent_token_t& operator<<(int token) {
    while( 1 < size() ) pop();
    push( recent_token_t(token, yylval, yylloc) );
    return front();
  }

  bool eof() const {
    return size() == 1 && front().token == YYEOF;
  }
  
  int next() {
    int end_token = cdf::used_token();
    
    switch( size() ) {
    default: gcc_unreachable();
    case 0:
      dbgmsg("%s:%d: 0 recent tokens []",  __func__, __LINE__);
      break;
    case 1: 
      dbgmsg("%s:%d: 1 recent token  [%s]",  __func__, __LINE__,
             keyword_str(back().token));
      break;
    case 2:
      dbgmsg("%s:%d: 2 recent tokens  [%s, %s]", __func__, __LINE__,
             keyword_str(front().token),
             keyword_str(back().token));
      break;
    }      

    /*
     * Clear any used tokens.  Return the unused token, if extant, which was
     * the lookahead token.  Because it was the last one returned by lexer(),
     * the location is accurate.  Else return the next token.
     */
    recent_token_t ending(end_token, cdf::location());
    if( ! empty() && same_token(front(), ending) ) pop();
    if( ! empty() && same_token(back(), ending) ) c.clear();

    end_token = 0;

    if( ! empty() ) {
      end_token = front().token;
      if( ! eof() ) {
        pop();
        assert(empty());
      }
    }

    dbgmsg("%s:%d: returning %s", __func__, __LINE__, keyword_str(end_token));
    return end_token;
  }
} recent_tokens;

static int next_token() {
  int token = lexer();
  recent_tokens << token;
  return token;
}

// act on CDF tokens
int
prelex() {
  static bool in_cdf = false;
  int token = next_token();

  if( in_cdf ) { return token; }
  if( ! is_cdf_token(token) ) { return token; }

  in_cdf = true;

  assert(is_cdf_token(token));

  while( is_cdf_token(token) ) {

    if( ! run_cdf(token) ) {
      dbgmsg( ">>CDF parser failed, ydfchar %d", ydfchar() );
    }
    
    token = recent_tokens.next();
    
    if( recent_tokens.eof() ) {
      dbgmsg("lexer() returned EOF (%d)", token);
      return YYEOF;
    }

    if( token == 0 ) token = next_token();
 
    if( token == NO_CONDITION && parsing.at_eof() ) {
      dbgmsg("scanner at EOF, apparently (%d)", token);
      return YYEOF;
    }
    dbgmsg("next token  %s", keyword_str(token));

    // Reenter cdf parser only if next token could affect parsing state.
    if( ! parsing.on() && ! is_cdf_condition_token(token) ) break;
  }

  if( yy_flex_debug ) {
    dbgmsg("scanner SC <%s>", start_condition_is());
  }

  if( YY_START == copy_state || YY_START == cdf_state ) {
    if( token == NAME ) {
      auto tok = keyword_tok(ydflval.string);
      if( tok ) token = tok;
    }
    yy_pop_state();
    dbgmsg("scanner SC <%s>, token now %s",
           start_condition_is(), keyword_str(token));
  }

  /*
   * The final, rejected CDF token might be a LEVEL number.
   */
  if( YY_START == field_state && level_needed() ) {
    switch( token ) {
    case NUMSTR:
      dbgmsg("final token is NUMSTR");
      yylval.number = level_of(yylval.numstr.string);
      token = LEVEL;
      break;
    case cdf::parser::token::YDF_NUMBER:
      dbgmsg("final token is YDF_NUMBER");
      yylval.number = ydflval.number;
      token = LEVEL;
      break;
    }
    if( token == LEVEL ) {
      switch(yylval.number) {
      case 66:
        token = LEVEL66;
        break;
      case 78:
        token = LEVEL78;
        break;
      case 88:
        token = LEVEL88;
        break;
      }
    }
  }

  dbgmsg( ">>CDF parser done, %s returning %s, conditional compilation %s", __func__,
          keyword_str(token), 
          parsing.on()? "TRUE" : "FALSE" );
  in_cdf = false;
  return token;
}

/* There are 2 parsers and one scanner.
 * yyparse calls yylex.
 * yylex calls prelex
 *             prelex calls lexer, the scanner produced by flex.
 *                          lexer reads input from yyin via lexer_input.
 *
 * prelex intercepts CDF statements, each of which it parses with cdf::parser::parse.
 * cdf::parser::parse affects CDF variables, which may affect how yylex treats
 * the input stream.
 *
 * Because the lexer is called recursively:
 *
 *   yyparse -> yylex -> cdf::parser::parse -> yylex
 *
 * the global state of the scanner has to be managed, both before and after the
 * CDF parser runs.
 *
 * Before invoking the CDF parser, run_cdf() stashes the CDF token with
 * inject_token().  When the CDF parser turns around and calls yylex, the first
 * token returned is that stashed one, because it is the reason the CDF parser
 * was invoked.
 *
 * While the CDF parser is active, the scanner saves each token returned by
 * lexer() in the recent_tokens queue.  As the CDF parser uses a token, it
 * records it such that it can be fetched with used_token().  A token fetched
 * and *not* used was a lookahead token, and will be directed to the main
 * parser.
 *
 * When the CDF parser returns, recent_tokens.next() compares the last two
 * tokens fetched with the last one one used.  If there's a recent token in the
 * queue after the last one used, that's a lookahead token.  It is returned to
 * the main parser, which started the whole thing by calling yylex().
 *
 * It's not really necessary to keep the last two tokens for the post-parsing
 * logic to work.  It's enough to compare the last fetched token with the last
 * used token.
 *
 * It *is* really necessary to for the CDF parser to record the tokens it
 * actually uses, at least as things stand.  The Bison C++ parser (which the
 * CDF parser uses) records any token values that it doesn't define as -2,
 * "invalid token".  If the lookahead token is, say, MOVE, it cannot be
 * retrieved from the CDF parser.  That's why it's kept in the recent_tokens
 * queue, where it's preceded by the last valid, used token, as recorded by the
 * CDF parser.
 */
int
yylex(void) {
  static bool produce_next_sentence_target = false;
  int token = parsing.pending_token();

  if( parsing.at_eof() ) return  YYEOF;
  if( token ) return token;

  /*
   * NEXT SENTENCE jumps to an implied CONTINUE at the next dot ('.').
   * Documentation says variously that the implied CONTINUE is before or after
   * that dot, but the meaning is one: after the statement that precedes the
   * dot.
   *
   * When the lexer encounters the dot, it returns it to the parser, which may
   * use it as a look-ahead token to decide the grammar production.  By the
   * time it returns to the lexer looking for its next token, the parser will
   * have taken whatever actions the dot decided.  At that point, the lexer
   * injects the label that NEXT SENTENCE jumps to.
   */
  if( produce_next_sentence_target ) {
    next_sentence_label(next_sentence);
    produce_next_sentence_target = false;
  }

  do {
    token = prelex();
    if( yy_flex_debug ) {
      if( parsing.in_cdf() ) {
        dbgmsg( "%s:%d: <%s> routing %s to CDF parser", __func__, __LINE__,
               start_condition_is(), keyword_str(token) );
      } else if( !parsing.on() ) {
        dbgmsg( "eating %s because conditional compilation is FALSE",
                 keyword_str(token) );
      }
    }

  } while( token && ! parsing.feed_a_parser() );

  if( next_sentence && token == '.' ) {
    produce_next_sentence_target = true;
  }

  if( token == YYEOF && parsing.in_cdf() ) {
    if( yy_flex_debug) dbgmsg("deflecting EOF");
    parsing.at_eof(true);
    return NO_CONDITION;
  }

  return token;
}

/*
 * The CDF parser is a C++ parser.  It passes pointers to the semantic value
 * and location as parameters to the lexer.  The generated lexer OTOH does not
 * accept those parameters; rather it works with global variables in the
 * old-fashioned C-style.
 *
 * Below we define the function the CDF parser calls to acquire a token.  It
 * calls the lexer, which writes to its global variables, copies that output to
 * the passed parameters, and returns the token.
 */
namespace cdf {
  int cdflex( parser::value_type *value, cbl_loc_t *loc ) {
    int tok = ::yylex();
    *value = ydflval;
    *loc = yylloc;
    return tok;
  }
}

/*
 * Token name<->string utilities
 */

// tokens.h is generated as needed from parse.h with tokens.h.gen
current_tokens_t::tokenset_t::tokenset_t() {
#include "token_names.h"
}
