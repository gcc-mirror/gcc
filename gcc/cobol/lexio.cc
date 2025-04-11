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

#include <ext/stdio_filebuf.h>
#include "cobol-system.h"
#include "cbldiag.h"
#include "util.h"
#include "copybook.h"
#include "lexio.h"

extern int yy_flex_debug;

static struct {
  bool first_file, explicitly;
  int column, right_margin;
  bool inference_pending() {
    bool tf = first_file && !explicitly;
    first_file = false;
    return tf;
  }
} indicator = { true, false, 0, 0 };

static bool debug_mode = false;

/*
 * The "debug mode" is a little odd, because we have to make sure a
 * leading "D" doesn't start the verb DISPLAY (for example).  If
 * debug_mode is on, debug lines are included in the parse.  If
 * debug_mode is off but we're not in fixed_format, lines starting
 * with "D" are also included.
 *
 * So, the line is excluded if: fixed format and not debug mode
 * Else, it's included.
*/

static inline int left_margin() {
  return indicator.column == 0? indicator.column : indicator.column - 1;
}
static inline int right_margin() {
  return indicator.right_margin == 0?
    indicator.right_margin : indicator.right_margin - 1;
}

/*
 * When setting the indicator column explicity:
 *   To get strict fixed 72-column lines, use a negative column number.
 *   When setting back to 0 (free), the right margin is also reset to 0.
 */
void
cobol_set_indicator_column( int column )
{
  indicator.explicitly = true;
  if( column == 0 ) indicator.right_margin = 0;
  if( column < 0 ) {
    column = -column;
    indicator.right_margin = 73;
  }
  indicator.column = column;
}

bool is_fixed_format() { return indicator.column == 7; }
bool is_reference_format() {
  return indicator.column == 7 && indicator.right_margin == 73;
}
bool include_debug()      { return indicator.column == 7 && debug_mode; }
bool set_debug( bool tf ) { return debug_mode = tf && is_fixed_format(); }

static bool nonblank( const char ch ) { return !isblank(ch); }

static inline char *
start_of_line( char *bol, char *eol ) {
  bol = std::find_if(bol, eol, nonblank);
  gcc_assert(bol < eol); // must exist
  return bol;
}

static inline char *
continues_at( char *bol, char *eol ) {
  if( indicator.column == 0 ) return NULL;  // cannot continue in free format
  bol += left_margin();
  if( *bol != '-' ) return NULL; // not a continuation line
  return start_of_line(++bol, eol);
}

// Return pointer to indicator column. Test ch if provided.
// NULL means no indicator column or tested value not present.
static inline char *
indicated( char *bol, char *eol, char ch = '\0' ) {
  if( indicator.column == 0 && *bol != '*' ) {
    return NULL;  // no indicator column in free format, except for comments
  }
  gcc_assert(bol != NULL);
  auto ind = bol + left_margin();
  if( eol <= ind ) return NULL; // left margin would be after end of line
  // If TAB is in the line-number region, nothing is in the indicator column.
  bool has_tab = std::any_of(bol, ind,
                             [](const char ch) { return ch == '\t'; } );
  if( has_tab ) return NULL;
  if( (bol += left_margin()) > eol ) return NULL;
  return ch == '\0' || ch == *bol? bol : NULL;
}

static char *
remove_inline_comment( char *bol, char *eol ) {
  static char ends = '\0';
  char *nl = std::find(bol, eol, '\n');

  if( bol < nl ) {
    std::swap(*nl, ends);
    char *comment = strstr(bol, "*>");
    if( comment ) {
      std::fill(comment, nl, SPACE);
    }
    std::swap(*nl, ends);
  }
  return eol;
}

static void
erase_line( char *src, char *esrc ) {
  dbgmsg( "%s: erasing %.*s from input", __func__, int(esrc-src), src);
  erase_source(src, esrc);
}

static size_t
count_newlines( const char *beg, const char *end ) {
  return std::count(beg, end, '\n');
}

size_t
filespan_t::tab_check( const char *src, const char *esrc ) {
  static const char tab = '\t';

  const char *data = src + left_margin();
  if( data < esrc ) { // not a blank line
    const char *tab_at = std::find(src, data, tab);
    if( tab_at < data ) {
      return (tab_at - src) + 1;
    }
  }
  return 0;
}

static const auto extended_icase = regex::extended | regex::icase;

std::stack< std::list<replace_t> > replace_directives;

static bool
is_word_or_quote( char ch ) {
  return ch == '"' || ch == '\'' || ISALNUM(ch);
}
/*
 * If the replacement is not leading/trailing, the edges of the
 * matched pattern must delimit a Cobol word.  If not, add a space to
 * the replacement.
 */
static void
maybe_add_space(const span_t& pattern, replace_t& recognized) {
  static const char blank[] = " ";
  const char *befter[2] = { "", "" };
  gcc_assert(0 < recognized.before.size());

  // start of pattern and end of preceding text
  if( pattern.p[0] == '.' && is_word_or_quote(recognized.before.p[-1]) ) {
    befter[0] = blank;
  }
  // end of pattern, and start of succeeding text
  if( pattern.pend[-1] == '.' && is_word_or_quote(recognized.before.pend[0]) ) {
    befter[1] = blank;
  }

  if( befter[0] == blank || befter[1] == blank ) {
    char *s = xasprintf( "%s%.*s%s",
                         befter[0],
                         recognized.after.size(), recognized.after.p,
                         befter[1] );
    recognized.after = span_t(s, s + strlen(s));
  }
}

/*
 * Keep track of the next pending replacement for each active REPLACE
 * directive.  For the current line, apply patterns that begins on the
 * line.  (It may match input extending beyond the current eol.)
 *
 * As each replacement is identified, append it to the passsed list of
 * pending replacements.  For these elements:
 *
 *     before is a span in mfile
 *     after is dynamically allocated
 */
static void
recognize_replacements( filespan_t mfile, std::list<replace_t>& pending_replacements ) {
  static const char *top_of_stack_cache, *applies_to;

  struct future_replacement_t {
    replace_t directive;
    span_t found;
    future_replacement_t( const replace_t& replace, span_t found )
      : directive(replace), found(found)
    {}
    bool operator<( const future_replacement_t& that ) const {
      return found.p < that.found.p;
    }
  };

  static std::list<future_replacement_t> futures;

  if( replace_directives.empty() ) return;

  if( ! (top_of_stack_cache == replace_directives.top().front().before.p
         &&
         applies_to == mfile.data) )
  {
    futures.clear();
    top_of_stack_cache = replace_directives.top().front().before.p;
    applies_to = mfile.data;
  }

  if( futures.empty() ) {
    /*
     * From the current point in the file, find the next match for each
     * pattern at the top of the replacement stack.
     */
    for( const auto& directive : replace_directives.top() ) {
      regex re(directive.before.p, extended_icase);
      cmatch cm;

      span_t found(mfile.eodata, mfile.eodata);

      if( regex_search( mfile.ccur(), (const char *)mfile.eodata, cm, re) ) {
        gcc_assert(cm[1].matched);
        found = span_t( cm[1].first, cm[1].second );
        if( yy_flex_debug ) {
          size_t n = count_newlines(mfile.data, found.p);
          dbgmsg("%s:%d first '%.*s' is on line %zu (offset %zu)", __func__, __LINE__,
                 directive.before.size(), directive.before.p,
                 ++n, found.p - mfile.data);
        }
      } else {
        dbgmsg("%s:%d not found: '%s' in \n'%.*s'", __func__, __LINE__,
              directive.before.p, int(strlen(directive.before.p)), mfile.cur);
      }
      futures.push_back( future_replacement_t(directive, found) );
    }
  }

  gcc_assert(!futures.empty());
  gcc_assert(futures.size() == replace_directives.top().size());

  replace_t recognized;

  auto pnext = std::min_element(futures.begin(), futures.end());

  for( const char *bol = mfile.cur; // more than one replacement may apply to a line
       bol <= pnext->found.p && pnext->found.p < mfile.eol; ) {
    auto& next(*pnext);
    recognized = replace_t( next.found, span_t(strlen(next.directive.after.p),
                                               next.directive.after.p) );
    maybe_add_space(next.directive.before, recognized);
    pending_replacements.push_back(recognized);
    bol = next.found.pend;

    if( yy_flex_debug ) {
      size_t n = std::count((const char *)mfile.data, recognized.before.p, '\n');
      dbgmsg( "%s:%d: line %zu @ %zu: '%s'\n/%.*s/%.*s/", __func__, __LINE__,
              ++n, next.found.p - mfile.data,
              next.directive.before.p,
              int(recognized.before.size()), recognized.before.p,
              int(recognized.after.size()), recognized.after.p );
    }

    // Update the futures element for this pattern
    cmatch cm;

    next.found = span_t(mfile.eodata, mfile.eodata);

    regex re(next.directive.before.p, extended_icase);
    if( regex_search(bol, (const char *)mfile.eodata, cm, re) ) {
      gcc_assert(cm[1].matched);
      next.found = span_t( cm[1].first, cm[1].second );
      size_t n = std::count((const char *)mfile.data, next.found.p, '\n');
      if( false )
        dbgmsg("%s:%d next '%.*s' will be on line %zu (offset %zu)", __func__, __LINE__,
               next.directive.before.size(), next.directive.before.p,
               ++n, next.found.p - mfile.data);
    }
    pnext = std::min_element(futures.begin(), futures.end());
  }
}

static void
check_source_format_directive( filespan_t& mfile ) {
  const char *p = std::find(mfile.cur, mfile.eol, '>');
  if( ! (p < mfile.eol && p[1] == *p ) ) return;

  const char pattern[] =
    ">>[[:blank:]]*source[[:blank:]]+"
    "(format[[:blank:]]+)?"
    "(is[[:blank:]]+)?"
    "(fixed|free)";
  static regex re(pattern, extended_icase);

  // show contents of marked subexpressions within each match
  cmatch cm;
  if( regex_search(p, (const char *)mfile.eol, cm, re) ) {
    gcc_assert(cm.size() > 1);
    switch( cm[3].length() ) {
    case 4:
      cobol_set_indicator_column(0);
      break;
    case 5:
      cobol_set_indicator_column(-7);
      break;
    default:
      gcc_assert(cm[3].length() == 4 || cm[3].length() == 5);
      break;
    }
    mfile.cur = const_cast<char*>(cm[0].second);
    dbgmsg( "%s:%d: %s format set, on line %zu", __func__, __LINE__,
            indicator.column == 7? "FIXED" : "FREE", mfile.lineno() );
    erase_line(const_cast<char*>(cm[0].first),
               const_cast<char*>(cm[0].second));
  }
}

struct buffer_t : public bytespan_t {
  char *pos;    // current output position

  buffer_t( char *data, char *eodata )
    : bytespan_t(data, eodata)
    , pos(data)
  {
    if(pos) *pos = '\0';
  }

  size_t nline() const {
    gcc_assert(data <= pos);
    return std::count(data, pos, '\n');
  }
  size_t free_space() const { gcc_assert(pos <= eodata); return eodata - pos; }

  bool pad_lines( size_t goal ) {
    while( nline() < goal ) {
      if( pos == eodata ) return false;
      *pos++ = '\n';
    }
    return true;
  }

  void show() const {
    gcc_assert(data <= pos);
    dbgmsg("flex input buffer: '%.*s'\n[xelf]", int(pos - data), data);
  }
  void dump() const {
#ifdef GETENV_OK
    if( getenv("lexer_input") ) show();
#endif
  }
};

static bool
valid_sequence_area( const char *p, const char *eodata ) {
  const char *pend = p + 6;
  if ( eodata < pend ) return false;

  for( ; p < pend; p++ ) {
    if( ! (ISDIGIT(*p) || *p == SPACE) ) {
      return false;
    }
  }
  return true; // characters either digits or blanks
}

const char * esc( size_t len, const char input[] );

static bool
is_word_char( char ch ) {
  switch(ch) {
  case '$':
  case '-':
  case '_':
    return true;
  }
  return ISALNUM(ch);
}

static bool
is_numeric_char( char ch ) {
  return ISDIGIT(ch)
    || TOUPPER(ch) == 'E'
    || ch == '.'
    || ch == ','
    ;
}

static bool
is_numeric_term( span_t term ) {
  gcc_assert(term.p);
  if( term.p[0] == '+' || term.p[0] == '-' ) term.p++;
  auto p = std::find_if( term.p, term.pend,
                         []( char ch ) {
                           return ! is_numeric_char(ch);
                         } );
  return p == term.pend;
}

struct replacing_term_t {
  bool matched, done;
  span_t leading_trailing, term, stmt;

  replacing_term_t(const char input[]) : matched(false), done(false) {
    stmt = span_t(input, input);
  }
};

extern YYLTYPE yylloc;

static const char *
last_newline (const char *p, const char *pend ) {
  size_t len = pend - p;
  return static_cast<const char *>( memrchr( p, '\n', len ) );
}
/*
 * For some statement parsed with regex_search, set yyloc to indicate the line
 * and column spans of the term.  Assume stmt begins at the start of a line.
 */
static void
update_yylloc( const csub_match& stmt, const csub_match& term ) {
  gcc_assert(stmt.first <= term.first && term.second <= stmt.second);

  class dump_loc_on_exit {
   public:
    dump_loc_on_exit() {
      if( gcobol_getenv( "update_yylloc" ) )
        location_dump( "update_yylloc", __LINE__, "begin", yylloc);
    }
    ~dump_loc_on_exit() {
      if( gcobol_getenv( "update_yylloc" ) )
        location_dump( "update_yylloc", __LINE__, "end  ", yylloc);
    }
  } dloe;

  size_t nline = std::count( stmt.first, term.second, '\n' );
  size_t n = std::count( term.first, term.second, '\n' );

  if( nline ) {
    yylloc.last_line += nline;
    yylloc.first_line = yylloc.last_line - n;
  }

  /*
   * Set the column span for the term.
   */
  const char *p = last_newline(stmt.first, stmt.second);
  if( !p ) { // no newlines in entire statement
    yylloc.first_column = (term.first - stmt.first) + 1;
    yylloc.last_column =  (term.second - stmt.first) + 1;
    return;
  }

  p = last_newline(stmt.first, term.first);
  if( !p ) { // no newlines before term
    yylloc.first_column = (term.first - stmt.first) + 1;
    p = last_newline(term.first, term.second);
    gcc_assert(p); // newline must be in term
    yylloc.last_column = (term.second - p) + 1;
    return;
  }

  const char *bol = p; // bol points to last newline before term

  yylloc.first_column = term.first - bol;
  p = last_newline(term.first, term.second);
  if( p ) { // term has newlines, too
    yylloc.last_column =  (p - term.first);
  } else {
    yylloc.last_column = yylloc.first_column + term.length();
  }
}

static replacing_term_t
parse_replacing_term( const char *stmt, const char *estmt ) {
  gcc_assert(stmt); gcc_assert(estmt); gcc_assert(stmt < estmt);
  replacing_term_t output(stmt);

  static const char pattern[] =
    "^([[:space:];,]+(LEADING|TRAILING|BY))?"    // 1, 2
    "[[:space:];,]+" // leading space between pairs
    "("                                        // 3
            "(\""  "([\"]{2}|[^\"])*" "\")"    // 4, 5
        "|" "('"   "([']{2}|[^'])*"   "')"     // 6, 7
        "|" "("    "[+-]?[[:alnum:]]+([_-]+[[:alnum:]]+)*" ")" // 8, 9
        "|" "(==(" "(=?[^=]+)*" ")==)"         // 10, 11, 12
    ")"
    "(([[:space:];,]+[\"'=[:alnum:]+-]{1,2})" "|" "[[:space:];,]*([.]))?"  // 13, 14, 15
    ;

  static regex re(pattern, extended_icase);
  cmatch cm;

  if( ! regex_search( stmt, estmt, cm, re) ) return output;

  bool replacing_term = cm[2].matched && TOUPPER(cm[2].first[0]) == 'B';

  if( cm[2].matched && ! replacing_term ) {
    output.leading_trailing = cm[2];
  }

  // Apply such that quoted matches supersede word matches.
  if( cm[11].matched ) output.term = cm[11];
  if( cm[ 8].matched ) output.term = cm[ 8];
  if( cm[ 6].matched ) output.term = cm[ 6];
  if( cm[ 4].matched ) output.term = cm[ 4];

  // The matched segment extends to the end of the matched term, or to
  // the dot at end of statement.  Include the pseudotext ==, if found.
  output.stmt = span_t(cm[0].first, output.term.pend);
  if( cm[10].matched ) output.stmt.pend = cm[10].second;

  if( cm[15].matched && ISSPACE(cm[15].second[0]) ) { // matched end of statement
    output.done = output.matched = true;
    output.stmt = cm[0];
    gcc_assert(output.stmt.pend[-1] == '.');
    dbgmsg("%s:%d: done at '%.*s'", __func__, __LINE__,
           output.term.size(), output.term.p);
    return output;
  }

  if( is_numeric_term(output.term) ) {
    output.matched = output.stmt.p < output.term.p;
    gcc_assert(output.matched);
    // look for fractional part
    if( is_numeric_char(*output.term.pend) && ISDIGIT(output.term.pend[1]) ) {
      gcc_assert(!ISDIGIT(*output.term.pend));
      auto p = std::find_if(++output.term.pend, estmt,
                            []( char ch ) { return !ISDIGIT(ch); } );
      output.stmt.pend = output.term.pend = p;
      output.done = '.' == output.stmt.pend[0] && ISSPACE(output.stmt.pend[1]);
      if( output.done ) output.stmt.pend++;
    }
    dbgmsg("%s:%d: %s '%.*s'", __func__, __LINE__,
           output.done? "done at" : "term is",
           output.term.size(), output.term.p);
    return output;
  }

  if( yy_flex_debug ) { // should be looking only for words
    dbgmsg("%s:%d: not done, working with '%.*s'", __func__, __LINE__,
          cm[0].length(), cm[0].first);
    int i=0;
    for( auto m : cm ) {
      if( m.matched )
        dbgmsg("%4d) '%.*s'", i, m.length(), m.first);
      i++;
    }
  }

  if( !cm[8].matched ) {
    output.matched = output.stmt.p < output.term.p;
    gcc_assert(output.matched);
    dbgmsg("%s:%d: term is '%.*s'", __func__, __LINE__,
           output.term.size(), output.term.p);
    return output;
  }

  bool extraneous_replacing = 'R' == TOUPPER(cm[8].first[0]); // maybe
  if( extraneous_replacing ) { // prove it
    static const char replacing[] = "REPLACING";
    for( size_t i=0; i < strlen(replacing); i++ ) {
      if( replacing[i] != TOUPPER(cm[8].first[i]) ) {
        extraneous_replacing = false;
        break;
      }
    }
    if( extraneous_replacing ) {
      update_yylloc( cm[0], cm[8] );
      yywarn("syntax error: invalid '%.*s'", cm[8].length(), cm[8].first);
      output.matched = false;
      return output;
    }
  }

  gcc_assert(cm[8].matched);
  gcc_assert(0 < output.term.size());

  dbgmsg("%s:%d: more words starting at '%.80s'", __func__, __LINE__,
         output.term.pend);

  static const char term_pattern[] =
    "^[[:space:]]+"
    "(" "(IN|OF)[[:space:]]+" ")"                            // 1, 2
    "(" "[+-]?[[:alnum:]]+([$_-]+[[:alnum:]]+)*" ")"         // 3, 4
    "(" "[[:space:]]*[(]" ")?"                               // 5
    "(([[:space:]]+[\"'=[:alnum:]+-]{1,2})" "|" "[[:space:]]*([.]))?"  // 6, 7, 8
    ;
  static const char paren_pattern[] =
    "^[[:space:]]*"
    "(" "[()][^()]*[()]" ")"    // 1
    "(([[:space:]]+[\"'=[:alnum:]+-]{1,2})" "|" "[[:space:]]*([.]))?"  // 2, 3, 4
    ;

  regex term_re(term_pattern, extended_icase);
  regex paren_re(paren_pattern, extended_icase);
  ssize_t nsub = 0;

  while( regex_search( output.term.pend, estmt, cm, term_re) ) {
    output.stmt.pend = output.term.pend = cm[3].second; // found a word
    if( cm[5].matched ) break; // found left parenthesis
    const csub_match& done(cm[8]);
    if( done.matched ) {
      output.done = output.matched = output.stmt.p < output.term.p;
      gcc_assert(output.done);
      goto matched;
    }
  }

  // match subscripts, if any
  while( regex_search( output.term.pend, estmt, cm, paren_re) ) {
    output.stmt.pend = output.term.pend = cm[1].second;
    if( cm[1].first[0]   == '(' ) nsub++;
    if( cm[1].first[0]   == ')' ) nsub--;
    if( cm[1].second[-1] == '(' ) nsub++;
    if( cm[1].second[-1] == ')' ) nsub--;

    const csub_match& done(cm[4]);
    if( done.matched ) {
      output.matched = output.stmt.p < output.term.p;
      output.stmt.pend = done.second;
      output.done = output.stmt.pend[-1] == '.';
      goto matched;
    }

    if( nsub == 0 ) break;
  }

 matched:
  output.matched = output.stmt.p < output.term.p;

  if( yy_flex_debug ) {
    const char *status = "unmatched";
    if( output.matched ) status = output.done? "done" : "matched";
    dbgmsg("%s:%d: %s term is '%.*s'", __func__, __LINE__, status,
           output.term.size(), output.term.p? output.term.p : "");
  }
  return output;
}

struct replacing_pair_t {
  span_t leading_trailing, stmt;
  replace_t replace;

  bool matched() const { return 0 < stmt.size(); }
  bool done() const { return matched() && stmt.pend[-1] == '.'; }
};
static replacing_pair_t
parse_replacing_pair( const char *stmt, const char *estmt ) {
  replacing_pair_t pair;

  pair.replace = replace_t();
  auto parsed = parse_replacing_term( stmt, estmt ); // before
  if( parsed.matched ) {
    if( parsed.term.size() == 0 ) return pair; // failure: empty before string
    pair.leading_trailing = parsed.leading_trailing;
    pair.stmt = parsed.stmt;
    pair.replace.before = parsed.term;

    if( !parsed.done ) {
      parsed = parse_replacing_term( pair.stmt.pend, estmt ); // after
      if( parsed.matched ) {
        pair.stmt.pend = parsed.stmt.pend;
        pair.replace.after = parsed.term;
      } else {
        dbgmsg("%s:%d: not matched '%.*s'", __func__, __LINE__,
               pair.stmt.size(), pair.stmt.p);
      }
    }
    if( yy_flex_debug ) {
      const char *status = "unmatched";
      if( pair.matched() ) status = pair.done()? "done" : "matched";
      dbgmsg("%s:%d: [%s] replacing '%.*s' with '%.*s'", __func__, __LINE__,
             status,
             pair.replace.before.size(), pair.replace.before.p,
             pair.replace.after.size(), pair.replace.after.p);
    }
  } else {
    for( auto p = stmt; (p = std::find(p, estmt, '.')) < estmt; p++ ) {
      if( ISSPACE(p[1]) ) {
        pair.stmt = span_t(stmt, ++p);
        break;
      }
    }
    if( pair.stmt.p ) {
      yywarn("CDF syntax error '%*s'", (int)pair.stmt.size(), pair.stmt.p);
    }
    else {
      // This eliminated a compiler warning about "format-overflow"
      yywarn("CDF syntax error");
    }
    pair.stmt = span_t(0UL, stmt);
    pair.replace = replace_t();
  }
  return pair;
}

static std::pair<std::list<replace_t>, char *>
parse_replace_pairs( const char *stmt, const char *estmt, bool is_copy_stmt ) {
  std::list<replace_t> pairs ;

  static const char     any_ch[] = ".";
  static const char    word_ch[] = "[[:alnum:]$_-]";
  static const char nonword_ch[] = "[^[:alnum:]\"'$_-]";

  // Pattern to find one REPLACE pseudo-text pair
  static const char replace_pattern[] =
    "([[:space:]]+(LEADING|TRAILING))?"    // 1, 2
    "[[:space:]]+"
    "==(" "(=?[^=]+)+" ")=="               // 3, 4
    "[[:space:]]+BY[[:space:]]+"
    "==(" "(=?[^=]+)*" ")=="               // 5, 6
    "(([[:space:]]+[\"'=[:alnum:]+-]{1,2})" "|" "[[:space:]]*([.]))?"  // 7, 8, 9
    ;

  regex pair_re(replace_pattern, extended_icase);
  cmatch cm;
  replacing_pair_t parsed;
  bool end_of_stmt = false;

  for( auto p = stmt; p < estmt && !end_of_stmt; p = parsed.stmt.pend ) {
    if( is_copy_stmt ) {
      parsed = parse_replacing_pair(p, estmt);
      if( parsed.replace.before.size() == 0 ) break; // empty before
      if( parsed.replace.after.p == NULL ) break;    // invalid after
      end_of_stmt = parsed.done();
    } else {
      if( ! regex_search( p, estmt, cm, pair_re) ) break;
      // Report findings.
      if( false && yy_flex_debug ) {
        for( size_t i=0; i < cm.size(); i++ ) {
          dbgmsg("%s: %s %zu: '%.*s'", __func__,
                 cm[i].matched? "Pair" : "pair",
                 i,
                 cm[i].matched? int(cm[i].length()) : 0,
                 cm[i].matched? cm[i].first : "");
        }
      }
      gcc_assert(cm[3].matched);
      gcc_assert(cm[5].matched);
      parsed.leading_trailing = cm[2];
      parsed.replace.before   = cm[3];
      parsed.replace.after    = cm[5];

      parsed.stmt = cm[0];
      // If not done, exclude trailing portion from statement match.
      if( !parsed.done() && cm[8].matched ) {
        gcc_assert(!cm[9].matched);
        parsed.stmt.pend = cm[8].first;
      }
    }

    span_t& before(parsed.replace.before);
    span_t& after(parsed.replace.after);

    const char *befter[2] = { nonword_ch, nonword_ch };
    gcc_assert(before.p < before.pend);
    if( !is_word_char(before.p[0]) )     befter[0] = any_ch;
    if( !is_word_char(before.pend[-1]) ) befter[1] = any_ch;

    const char *src = esc(before.size(), before.p);

    if( parsed.leading_trailing.size() > 0 ) {
      switch( TOUPPER(parsed.leading_trailing.p[0]) ) {
      case 'L': // leading
        befter[1] = word_ch;
        break;
      case 'T': // trailing
        befter[0] = word_ch;
        break;
      default:
        gcc_unreachable();
      }
      dbgmsg("%s:%d: dealing with %.*s", __func__, __LINE__,
             int(parsed.leading_trailing.size()), parsed.leading_trailing.p);
    }

    src = xasprintf("%s(%s)%s", befter[0], src, befter[1]);

    struct {  span_t before, after; }  output;
    output.before = span_t(strlen(src), src);
    output.after = after.dup();

    gcc_assert(!before.has_nul());
    pairs.push_back( replace_t( output.before, output.after ) );

    // COPY REPLACING matches end-of-statment here
    // REPLACE matched end-of-statement in caller, and estmt[-1] == '.'
    if( is_copy_stmt && parsed.stmt.pend[-1] == '.' ) break;
  }

  if( yy_flex_debug ) {
    dbgmsg( "%s:%d: %s: %zu pairs parsed from  '%.*s'", __func__, __LINE__,
            parsed.done()? "done" : "not done",
            pairs.size(), parsed.stmt.size(), parsed.stmt.p );
    int i = 0;
    for( const auto& replace : pairs ) {
      dbgmsg("%s:%d:%4d: '%s' => '%s'", __func__, __LINE__,
             ++i, replace.before.p, replace.after.p);
    }
  }
  if( !parsed.done() ) {
    pairs.clear();
    return std::make_pair(pairs, const_cast<char*>(stmt));
  }

  return std::make_pair(pairs, const_cast<char*>(parsed.stmt.pend));
}

struct copy_descr_t {
  bool parsed;
  int fd;
  size_t nreplace;
  span_t partial_line, erased_lines;

  copy_descr_t( const char *line, const char *eol)
    : parsed(false), fd(-1), nreplace(0), partial_line(line, eol) {}
};

static YYLTYPE
location_in( const filespan_t& mfile, const csub_match cm ) {
  YYLTYPE loc {
    int(mfile.lineno() + 1), int(mfile.colno() + 1),
    int(mfile.lineno() + 1), int(mfile.colno() + 1)
  };
  gcc_assert(mfile.cur <= cm.first && cm.second <= mfile.eodata);
  auto nline = std::count(cm.first, cm.second, '\n');
  if( nline ) {
    gcc_assert(loc.first_line < nline);
    loc.first_line -= nline;
    auto p = static_cast<const char*>(memrchr(cm.first, '\n', cm.length()));
    loc.last_column = (cm.second) - p;
  }
  location_dump(__func__, __LINE__, "copy?", loc);
  return loc;
}

static copy_descr_t
parse_copy_directive( filespan_t& mfile ) {
  static const char *most_recent_buffer;
  static span_t copy_stmt(mfile.eodata, mfile.eodata);

  static const char pattern[] =
    "COPY" "[[:space:]]+"
    /* 1 */    "("
    /*2,3*/            "\"("  "([\"]{2}|[^\"])+" ")\""
    /*4,5*/        "|" "'("   "([']{2}|[^'])+"   ")[']"
    /*6,7*/        "|" "("    "[[:alnum:]]+([_-]+[[:alnum:]]+)*" ")"
    /*   */    ")"
    /* 8 */    "("
    /* 9 */        "[[:space:]]+(OF|IN)[[:space:]]+"
    /* 10*/        "("
    /*11,12*/              "(\""  "([\"]{2}|[^\"])+" "\")"
    /*13,14*/          "|" "('"   "([']{2}|[^'])+"   "')"
    /*15,16*/          "|" "("    "[[:alnum:]]+([_-]+[[:alnum:]]+)*" ")"
    /*   */        ")"
    /*   */    ")?"
    /*17,18*/  "([[:space:]]+SUPPRESS([[:space:]]+PRINTING)?)?"
    /*19,20 */ "(" "([[:space:]]*[.])" "|" "[[:space:]]+REPLACING" ")"
    ;

  static regex re(pattern, extended_icase);
  cmatch cm;
  copy_descr_t outcome(mfile.cur, mfile.cur);

  // COPY appears in current buffer?
  if( most_recent_buffer != mfile.data || copy_stmt.p < mfile.cur ) {
    most_recent_buffer = mfile.data;
    copy_stmt.p = mfile.eodata;

    if( regex_search(mfile.ccur(),
                          (const char *)mfile.eodata, cm, re) ) {
      copy_stmt = span_t( cm[0].first, cm[0].second );
      if( yy_flex_debug ) {
        size_t nnl = 1 + count_newlines(mfile.data, copy_stmt.p);
        size_t nst = 1 + count_newlines(copy_stmt.p, copy_stmt.pend);
        dbgmsg("%s:%d: line %zu: COPY directive is %zu lines '%.*s'",
               __func__, __LINE__,
               nnl, nst, copy_stmt.size(), copy_stmt.p);
      }
    }
  }

  // If COPY appears on the current line, parse it completely this time.
  if( mfile.cur <= copy_stmt.p &&
                   copy_stmt.p < mfile.eol ) {
    outcome.parsed = regex_search(copy_stmt.p, copy_stmt.pend, cm, re);
    gcc_assert(outcome.parsed);
    outcome.partial_line = span_t(mfile.cur, copy_stmt.p);

    if( yy_flex_debug ) {
      dbgmsg("%zu expressions", std::count(pattern, pattern + sizeof(pattern), '('));
      int i = 0;
      for( const auto& m : cm ) {
        if( m.matched )
          dbgmsg("%s:%d: %2d: '%.*s'", __func__, __LINE__,
                i, int(m.length()), m.first);
        i++;
      }
    }

    auto& copybook_name = cm[1];
    auto& library_name = cm[10];

    bool replacing = !cm[20].matched;

    if( library_name.matched ) {
      YYLTYPE loc = location_in( mfile, library_name );
      copybook.library( loc, xstrndup(library_name.first, library_name.length()) );
    }
    YYLTYPE loc = location_in( mfile, copybook_name );
    outcome.fd = copybook.open( loc, xstrndup(copybook_name.first,
                                              copybook_name.length()) );
    if( outcome.fd == -1 ) { // let parser report missing copybook
      dbgmsg("%s:%d: (no copybook '%s' found)", __func__, __LINE__, copybook.source());
      return outcome;
    }

    if( replacing ) {
      std::pair<std::list<replace_t>, char*>
        result = parse_replace_pairs( cm[0].second, mfile.eodata, true );

      std::list<replace_t>& replacements(result.first);
      outcome.parsed = (outcome.nreplace = replacements.size()) > 0;
      if( outcome.parsed ) {
        replace_directives.push(replacements);
      }
      copy_stmt.pend = result.second;

      // Maybe we don't need these.  We'll see.
      for( const auto& r : replacements ) {
        copybook.replacement(pseudo_e, r.before.dup().p, r.after.dup().p);
      }
    }

    // If the parse failed, pass it through to the parser for analysis.
    if( outcome.parsed ) {
      erase_line( const_cast<char*>(copy_stmt.p),
                  const_cast<char*>(copy_stmt.pend));
      outcome.erased_lines = copy_stmt;
    }

    mfile.eol = const_cast<char*>(copy_stmt.pend);
    mfile.next_line();
  }
  return outcome;
}

static char *
parse_replace_last_off( filespan_t& mfile ) {
  static const char pattern[] =
    "REPLACE" "[[:space:]]+"
    "(LAST[[:space:]]+)?OFF[[:space:]]*[.]"
    ;
  static regex re(pattern, extended_icase);
  cmatch cm;

  // REPLACE [LAST] OFF?
  bool found = regex_search(mfile.ccur(),
                                 (const char *)mfile.eodata, cm, re);
  gcc_assert(found); // caller ensures

  gcc_assert(cm.size() == 2);
  // LAST OFF removes most recent REPLACE
  if( cm[1].matched ) {
    gcc_assert(TOUPPER(cm[1].first[0]) == 'L');
    if( ! replace_directives.empty() ) {
      replace_directives.pop();
    }
  } else { // OFF clears the REPLACE stack
    while( ! replace_directives.empty() ) {
      replace_directives.pop();
    }
  }

  dbgmsg( "%s:%d: line %zu: parsed '%.*s', ", __func__, __LINE__,
          mfile.lineno(), int(cm[0].length()), cm[0].first );

  // Remove statement from input
  erase_line(const_cast<char*>(cm[0].first),
             const_cast<char*>(cm[0].second));

  return const_cast<char*>(cm[0].second);
}

static span_t
parse_replace_text( filespan_t& mfile ) {
  static const char pattern[] =
    /* 0 */    "REPLACE"
    /* 1 */    "([[:space:]]+ALSO)?"
    /* 2 */    "("
    /*3,4*/      "([[:space:]]+(LEADING|TRAILING))?"
    /* 5 */      "([[:space:]]+"
    /* 6 */         "==" "(=?[^=]+)+" "=="
    /*   */         "[[:space:]]+BY[[:space:]]+"
    /* 7 */         "==" "(=?[^=]+)*" "=="
    /*   */      ")"
    /*   */    ")+[[:space:]]*[.]"
    ;
  static regex re(pattern, extended_icase);
  cmatch cm;
  const size_t current_lineno(mfile.lineno());

  if( false && yy_flex_debug ) {
    auto pend = mfile.eol;
    gcc_assert(mfile.line_length() > 2);
    if( pend[-1] == '\n' ) pend -= 2;
    auto len = int(pend - mfile.cur);
    dbgmsg("%s:%d: line %zu: parsing '%.*s", __func__, __LINE__,
          current_lineno, len, mfile.cur);
  }

  if( ! regex_search(mfile.ccur(), (const char *)mfile.eodata, cm, re) ) {
    dbgmsg( "%s:%d: line %zu: not a REPLACE statement:\n'%.*s'",
            __func__, __LINE__, current_lineno,
            int(mfile.line_length()), mfile.cur );
    return span_t();
  }

  // Report findings.
    if( yy_flex_debug ) {
      dbgmsg("%zu expressions", std::count(pattern, pattern + sizeof(pattern), '('));
      int i = 0;
      for( const auto& m : cm ) {
        if( m.matched )
          dbgmsg("%s:%d: %2d: '%.*s'", __func__, __LINE__,
                i, int(m.length()), m.first);
        i++;
      }
    }

  gcc_assert(cm.size() > 7);

  // Update active REPLACE stack
  if( ! cm[1].matched ) { // ALSO pushes, else clear stack and push one.
    while( !replace_directives.empty() ) {
      replace_directives.pop();
    }
  } else {
    gcc_assert(TOUPPER(cm[1].first[0]) == 'A');
  }

  span_t replace_stmt(cm[0].first, cm[0].second);

  std::pair<std::list<replace_t>, char*>
        result = parse_replace_pairs(replace_stmt.p, replace_stmt.pend, false);
  std::list<replace_t>& replacements(result.first);
  replace_directives.push( replacements );

  if( yy_flex_debug ) {
    dbgmsg( "%s:%d: line %zu: %zu pairs parsed from  '%.*s'", __func__, __LINE__,
           current_lineno, replacements.size(), int(replace_stmt.size()), replace_stmt.p );
    for( const auto& replace : replacements ) {
      int i = 0;
      dbgmsg("%s:%d:%4d: '%s' => '%s'", __func__, __LINE__,
            ++i, replace.before.p, replace.after.p);
    }
  }

  // Remove statement from input
  erase_line(const_cast<char*>(replace_stmt.p),
             const_cast<char*>(replace_stmt.pend));

  return replace_stmt;
}

static span_t
parse_replace_directive( filespan_t& mfile ) {
  static const char *most_recent_buffer, *next_directive;
  static bool off_coming_up;
  static const char pattern[] =
    "REPLACE" "[[:space:]]+" "(LAST|OFF|ALSO|LEADING|TRAILING|==)";

  static regex re(pattern, extended_icase);
  cmatch cm;

  // REPLACE appears in current buffer?
  if( most_recent_buffer != mfile.data || next_directive < mfile.cur ) {
    most_recent_buffer = mfile.data;
    next_directive = mfile.eodata;

    if( regex_search(mfile.ccur(),
                          (const char *)mfile.eodata, cm, re) ) {
      gcc_assert(cm[1].matched);
      next_directive = cm[0].first;

      switch( TOUPPER(cm[1].first[0]) ) {
      case 'L':
        off_coming_up = 'A' == TOUPPER(cm[1].first[1]); // LAST OFF, else LEADING
        break;
      case 'O': // OFF
        off_coming_up = true;
        break;
      case 'A': case 'T': case '=': // [ALSO] [ eading/Trailing] == ...
        off_coming_up = false;
        break;
      default:
        gcc_unreachable();
      }
    }
  }

  span_t erased;
  // REPLACE appears on current line?
  if( mfile.cur <= next_directive &&
                   next_directive < mfile.eol ) {
    if( off_coming_up ) {
      parse_replace_last_off(mfile);
    } else {
      erased = parse_replace_text(mfile);
    }
  }
  return erased;
}

/*
 * Maintain the number of newlines by counting those that will be
 * overwritten, and appending them to the appended line.  Return the
 * new EOL pointer.
 *
 * The newlines accumulate past eodata, at the start of the blank
 * lines created by the caller.
 */
char *
bytespan_t::append( const char *input, const char *eoinput ) {
  gcc_assert(data < eodata);

#define LEXIO 0
#if LEXIO
  auto nq = std::count_if(data, eodata, isquote);
  dbgmsg("%s:%3d:  input ------ '%.*s'", __func__, __LINE__, int(eoinput - input), input);
  dbgmsg("%s:%3d:  precondition '%.*s' (%zu: %s)", __func__, __LINE__,
        int(size()), data, nq, in_string()? "in string" : "not in string");
#endif
  if( !in_string() ) { // Remove trailing space unless it's part of a literal.
    while(data < eodata && ISSPACE(eodata[-1])) eodata--;
    gcc_assert(ISSPACE(eodata[0]));
    gcc_assert(data == eodata || !ISSPACE(eodata[-1]));
  }
  // skip leading blanks
  while( input < eoinput && ISSPACE(*input) ) input++;
  if( isquote(*input) ) input++;

  size_t len = eoinput - input;
  char * pend = eodata + len;

  int nnl =   std::count(eodata,   pend, '\n');   // newlines to be overwritten
  gcc_assert(0 == std::count(input, eoinput, '\n'));  // newlines in input

  memmove(eodata, input, len);
  nnl += std::count(pend, pend + nnl, '\n'); // other newlines to be overwritten
  std::fill(pend, pend + nnl, '\n');

  eodata = pend;

#if LEXIO
  dbgmsg("%s:%3d: postcondition '%.*s'", __func__, __LINE__, int(size() + len) - 1, data);
#endif

  return eodata;
}

const char * cobol_filename();

static filespan_t&
mapped_file( FILE *input ) {
  static std::map<int, filespan_t> inputs;

  int fd = fileno(input);
  gcc_assert(fd > 0);

  filespan_t& mfile = inputs[fd];
  if( mfile.data ) {
    return mfile;
  }

  struct stat sb;
  if( 0 != fstat(fd, &sb) ) {
    cbl_err( "%s: could not stat fd %d", __func__, fd );
  }

  mfile.use_nada();

  if( sb.st_size > 0 ) {
    static const int flags = MAP_PRIVATE;

    void *p = mmap(0, sb.st_size, PROT_READ|PROT_WRITE, flags, fd, 0);
    if( p == MAP_FAILED ) {
      cbl_err( "%s: could not map fd %d", __func__, fd );
    }

    mfile.lineno_reset();
    mfile.data = mfile.cur = mfile.eol = mfile.eodata = static_cast<char*>(p);
    mfile.eodata += sb.st_size;
  }
  return mfile;
}

char filespan_t::empty_file[8] = "      \n";

static void unmap_file( filespan_t& mfile  ) {
  if( ! mfile.nada() ) {
    munmap(mfile.data, mfile.size() - 1);
  }
  mfile = filespan_t();
}

extern int yylineno;

static void
print_lexer_input( const char *buf, const char *ebuf ) {
  const char *eol, *lexio = getenv("lexio");
  int i;
  static int nbuf = 1;
  static FILE *output = NULL;

  if( !lexio ) return;
  if( !output ) {
    output = fopen( lexio, "w" );
    if( !output ) output = stderr;
  }

  fprintf( output, "*> buffer %d\n", nbuf );
  for( i = 0, eol = std::find(buf, ebuf, '\n');
       eol != ebuf; buf = eol, eol = std::find(buf, ebuf, '\n'), i++ ) {
    eol++;
    fprintf( output, "%5d %.*s", yylineno + i, int(eol - buf), buf );
  }
  if( buf < ebuf ) {
    fprintf( output, "%5d %.*s", yylineno + i, int(eol - buf), buf );
  }
  fprintf( output, "*> endbuf %d\n", nbuf++ );
  fflush(output);
}

/*
 * Fill about as much of the lexer's buffer as possible, except skip
 * leading blanks on blank lines.
 */
int
lexer_input( char buf[], int max_size, FILE *input ) {
  filespan_t& mfile( mapped_file(input) );

  if( mfile.cur == mfile.eodata ) {
    if( mfile.cur ) unmap_file(mfile);
    return 0;
  }

  gcc_assert( mfile.data <= mfile.cur && mfile.cur < mfile.eodata );

  char *next = std::min(mfile.eodata, mfile.cur + max_size);
  buffer_t output(buf, buf + max_size); // initializes pos

  // Fill output, keeping only NL for blank lines.
  for( auto p = mfile.cur; p < next; *output.pos++ = *p++ ) {
    static bool at_bol = false;
    if( at_bol ) {
      auto nonblank = std::find_if( p, next,
                                    []( char ch ) {
                                      return !isblank(ch); } );
      if( nonblank + 1 < next ) {
        if( *nonblank == '\r' ) nonblank++; // Windows
        if( *nonblank == '\n' ) {
          p = nonblank;
          continue;
        }
      }
    }
    at_bol = *p == '\n';
  }

  gcc_assert(  output.pos <= output.eodata );
  output.eodata = output.pos;

  mfile.cur = next;
  gcc_assert(mfile.cur <= mfile.eodata);

  // Buffer full or input exhausted.
  print_lexer_input(output.data, output.eodata);

  return output.size();
}

static const char *
find_filter( const char filter[] ) {

  if( 0 == access(filter, X_OK) ) {
    return filter;
  }

  const char *path = getenv("PATH");
  if( ! path ) return NULL;
  char *p = xstrdup(path), *eopath = p + strlen(p);

  while( *p != '\0' ) {
    auto pend = std::find( p, eopath, ':' );
    if( *pend == ':' ) *pend++ = '\0';

    char *name = xasprintf( "%s/%s", p, filter );

    if( 0 == access(name, X_OK) ) {
      return name;
    }
    p = pend;
  }
  return NULL;
}

bool verbose_file_reader = false;

typedef std::pair <char *, std::list<std::string> > preprocessor_filter_t;
static std::list<preprocessor_filter_t> preprocessor_filters;
static std::list<const char *> included_files;

/*
 * Keep a list of files added with -include on the command line.
 */
bool
include_file_add(const char filename[]) {
  struct stat sb;
  if( -1 == stat(filename, &sb) ) return false;
  included_files.push_back(filename);
  return true;
}

bool
preprocess_filter_add( const char input[] ) {
  std::list <std::string> options;
  std::string filter(input);
  size_t pos = filter.find(",");

  if( pos != filter.npos ) {
    std::vector<char> others( filter.size() - pos, '\0' );
    std::copy( filter.begin() + pos + 1, filter.end(), others.begin() );
    filter.resize(pos);
    char *optstr = others.data();
    for( char *opt = optstr + 1; (opt = strtok(opt, ",")); opt = NULL ) {
      options.push_back(opt);
    }
  }

  auto filename = find_filter(filter.c_str());
  if( !filename ) {
    yywarn("preprocessor '%s/%s' not found", getcwd(NULL, 0), filter);
    return false;
  }
  preprocessor_filters.push_back( std::make_pair(xstrdup(filename), options) );
  return true;
}

void
cdftext::echo_input( int input, const char filename[] ) {
  int fd;
  if( -1 == (fd = dup(input)) ) {
      yywarn( "could not open preprocessed file %s to echo to standard output",
               filename );
      return;
  }

  auto mfile = map_file(fd);

  if( -1 == write(STDOUT_FILENO, mfile.data, mfile.size()) ) {
    yywarn( "could not write preprocessed file %s to standard output",
          filename );
  }
  if( -1 == munmap(mfile.data, mfile.size()) ) {
    yywarn( "could not release mapped file" );
  }
  if( -1 == close(fd) ) {
    yywarn( "could not close mapped file" );
  }
}

static inline ino_t
inode_of( int fd ) {
  struct stat sb;
  if( -1 == fstat(fd, &sb) ) {
    cbl_err( "could not stat fd %d", fd);
  }
  return sb.st_ino;
}

FILE *
cdftext::lex_open( const char filename[] ) {
  int input = open_input( filename );
  if( input == -1 ) return NULL;

  int output = open_output();

  // Process any files supplied by the -include comamnd-line option.
  for( auto name : included_files ) {
    int input;
    if( -1 == (input = open(name, O_RDONLY)) ) {
      yyerrorvl(1, "", "cannot open -include file %s", name);
      continue;
    }
    cobol_filename(name, inode_of(input));
    filespan_t mfile( free_form_reference_format( input ) );

    process_file( mfile, output );
  }

  cobol_filename(filename, inode_of(input));
  filespan_t mfile( free_form_reference_format( input ) );

  process_file( mfile, output );

  if( lexer_echo() ) {
    echo_input(output, filename);
  }

  for( auto filter_pair : preprocessor_filters ) {
    input  = output;
    output = open_output();

    char *filter = filter_pair.first;
    std::list<std::string>& options = filter_pair.second;

    std::vector <char*> argv(2 + options.size(), NULL);
    argv[0] = filter;

    auto last_argv = std::transform( options.begin(), options.end(), argv.begin() + 1,
                                     []( std::string& opt ) {
                                       return xstrdup(opt.c_str());
                                     } );
    *last_argv = NULL;

    pid_t pid = fork();

    switch(pid){
    case -1: cbl_err( "%s", __func__);
      break;
    case 0: // child
      if( -1 == dup2(input, STDIN_FILENO) ) {
        cbl_err( "%s: could not dup input", __func__);
      }
      if( -1 == dup2(output, STDOUT_FILENO) ) {
        cbl_err( "%s: could not dup output", __func__);
      }
      if( -1 == lseek(STDIN_FILENO, SEEK_SET, 0) ) {
        cbl_err( "%s: could not seek to start of file", __func__);
      }
      int erc;
      if( -1 == (erc = execv(filter, argv.data())) ) {
        yywarn("could not execute %s", filter);
      }
      _exit(erc);
    }
    int status;
    auto kid = wait(&status);
    gcc_assert(pid == kid);
    if( kid == -1 ) cbl_err( "failed waiting for pid %d", pid);

    if( WIFSIGNALED(status) ) {
      cbl_errx( "%s pid %d terminated by %s",
           filter, kid, strsignal(WTERMSIG(status)) );
    }
    if( WIFEXITED(status) ) {
      if( (status = WEXITSTATUS(status)) != 0 ) {
        cbl_errx( "%s exited with status %d",
             filter, status);
      }
    }
    yywarn( "applied %s", filter );
  }

  return fdopen( output, "r");
}

int
cdftext::open_input( const char filename[] ) {
  int fd = open(filename, O_RDONLY);
  if( fd == -1 ) {
    dbgmsg( "could not open '%s': %m", filename );
  }

  verbose_file_reader = NULL != getenv("GCOBOL_TEMPDIR");

  if( verbose_file_reader ) {
    yywarn("verbose: opening %s for input", filename);
  }
  return fd;
}

int
cdftext::open_output() {
  char *name = getenv("GCOBOL_TEMPDIR");
  int fd;

  if( name && 0 != strcmp(name, "/") ) {
    char * stem = xasprintf("%sXXXXXX", name);
    if( -1 == (fd = mkstemp(stem)) ) {
      cbl_err( "could not open temporary file '%s' (%s)",
               name, realpath(name, stem));
    }
    return fd;
  }

  FILE *fh = tmpfile();
  if( !fh ) {
    cbl_err("could not create temporary file");
  }

  return fileno(fh);
}

filespan_t
cdftext::map_file( int fd ) {
  gcc_assert(fd > 0);

  filespan_t mfile;
  mfile.use_nada();

  struct stat sb;
  do {
    if( 0 != fstat(fd, &sb) ) {
      cbl_err( "%s: could not stat fd %d", __func__, fd );
    }
    if( S_ISFIFO(sb.st_mode) ) {
      // Copy FIFO to regular file that can be mapped.
      int input = open_output();
      std::swap(fd, input); // fd will continue to be the input
      static char block[4096 * 4];
      ssize_t n;
      while( (n = read(input, block, sizeof(block))) != 0 ) {
        ssize_t nout = write(fd, block, n);
        if( nout != n ) {
          cbl_err( "%s: could not prepare map file from FIFO %d",
              __func__, input);
        }
        if( false ) dbgmsg("%s: copied %ld bytes from FIFO",
                                __func__, nout);
      }
    }
  } while( S_ISFIFO(sb.st_mode) );

  if( sb.st_size > 0 ) {
    static const int flags = MAP_PRIVATE;

    void *p = mmap(0, sb.st_size, PROT_READ|PROT_WRITE, flags, fd, 0);
    if( p == MAP_FAILED ) {
      cbl_err( "%s: could not map fd %d", __func__, fd );
    }

    mfile.lineno_reset();
    mfile.data = mfile.cur = mfile.eol = mfile.eodata = static_cast<char*>(p);
    mfile.eodata += sb.st_size;
  }

  return mfile;
}

bool lexio_dialect_mf();

filespan_t
cdftext::free_form_reference_format( int input ) {
  filespan_t source_buffer = map_file(input);
  filespan_t mfile(source_buffer);

  /*
   * current_line_t describes the segment of mapped file that is the
   * "current line" being processed.  Its only use is for line
   * continuation, whether string literals or not.
   */
  struct current_line_t {
    size_t lineno;
    bytespan_t line;
    // construct with length zero
    current_line_t( char data[] ) : lineno(0), line(data, data) {}
  } current( mfile.data );

  /*
   * If the format is not explicitly set on the command line, test the
   * first 6 bytes of the first file to determine the format
   * heuristically. If the first 6 characters are only digits or
   * blanks, then the file is in fixed format.
   */

  if( indicator.inference_pending()  ) {
    const char *p = mfile.data;
    while( p < mfile.eodata ) {
      const char * pend =
        std::find(p, const_cast<const char *>(mfile.eodata), '\n');
      if( 6 < pend - p ) break;
      p = pend;
      if( p < mfile.eodata) p++;
    }
    if( valid_sequence_area(p, mfile.eodata) ) indicator.column = 7;

    dbgmsg("%s:%d: %s format detected", __func__, __LINE__,
           indicator.column == 7? "FIXED" : "FREE");
  }

  while( mfile.next_line() ) {
    check_source_format_directive(mfile);
    remove_inline_comment(mfile.cur, mfile.eol);

    if( mfile.is_blank_line() ) continue;

    char *indcol = indicated(mfile.cur, mfile.eol); // true only for fixed
    //                                              // format

    if( is_fixed_format() && !indcol ) { // short line
      erase_source(mfile.cur, mfile.eol);
    }

    if( indcol ) {
      // Set to blank columns 1-6 and anything past the right margin.
      erase_source(mfile.cur, indcol);
      if( is_reference_format() ) {
        if( mfile.cur + right_margin() <  mfile.eol ) {
          auto p = std::find(mfile.cur + right_margin(), mfile.eol, '\n');
          erase_source(mfile.cur + right_margin(), p);
        }
      }

      switch( TOUPPER(*indcol) ) {
      case '-':
        gcc_assert(0 < current.line.size());
        /*
         * The "current line" -- the line being continued -- may be many
         * lines earlier (with many intervening newlines) or may intrude
         * on its succeeding line.  Erase the continuation line.
         */
        {
          char *pend = mfile.eol;
          if( right_margin() ) {
            pend = std::min(mfile.cur + right_margin(), mfile.eol);
          }
          // The appended segment has no newline because the erased line retains
          // one.
          pend = std::find(indcol + 1, pend, '\n');
          char *p = current.line.append(indcol + 1, pend );
          if( (p = std::max(p, mfile.cur)) < mfile.eol ) {
            erase_source(p, mfile.eol);
          }
        }
        continue;
      case SPACE:
        break;
      case 'D':
        /*
         * Pass the D to the lexer, because WITH DEBUGGING MODE is
         * parsed in the parser.  This assumes too strict a rule: that
         * all the source is in one format. In fact, DEBUGGING MODE
         * could be set on, and >>SOURCE-FORMAT can switch back and
         * forth. To solve that, we'd have to parse WITH DEBUGGING MODE
         * in free_form_reference_format(), which is a lot of work for
         * an obsolete feature.
         */
        break;
      case '*':
      case '/':
        if( indcol < mfile.eol - 1 ) {
          erase_source(indcol, mfile.eol);
        }
        continue;
      case '$':
        if( lexio_dialect_mf() ) {
          break;
        }
        __attribute__ ((fallthrough));
      default: // flag other characters in indicator area
        if( ! ISSPACE(indcol[0]) ) {
          yyerrorvl( mfile.lineno(), cobol_filename(),
                     "error: stray indicator '%c' (0x%x): \"%.*s\"",
                     indcol[0], indcol[0],
                     int(mfile.line_length() - 1), mfile.cur );
          *indcol = SPACE;
        }
        break;
      }
    }
    current.line.update(mfile.cur, mfile.eol, right_margin());
    current.lineno = mfile.lineno();
  } // next line

  return source_buffer;
}

/*
 * process_file is a recursive routine that opens and processes
 * included files.  It uses the input file stack in two ways: to check
 * copybook uniqueness, and (via the lexer) to keep track filenames
 * and line numbers.
 *
 * When reading copybook files, the copybook object enforces the rule
 * that no copybook may include itself, even indirectly. It does that
 * by relying on the unique_stack to deny a push.  Because the reader
 * makes no attempt to count lines, line numbers in the input stack
 * are all 1 at this point.
 *
 * When returning from the top-level recursion, the input stack has
 * the original file's name on top, with depth 1.  At that point, the
 * lexer begins tokenizing the input.
 *
 * The input stream sent to the lexer is delimited by #FILE tokens
 * denoting the source filename.  As far as the lexer is concerned,
 * there's only ever one file: the name passed to lex_open() when we
 * kicked things off.  But messages and the debugger need to know
 * which file and line each statment appeared in.
 *
 * The lexer uses the input stack to keep track of names and
 * numbers. The top of the input file stack is the current file
 * context, initially set to line 1. When the lexer sees a push, it
 * updates the top-of-stack with the current line number, yylineno,
 * and then pushes the copybook filename with line 1. When it sees a
 * pop, the current file is popped, of course; its line number no
 * longer matters. Then the top-of-stack is used to update the current
 * cobol filename and yylineno.
 */
void
cdftext::process_file( filespan_t mfile, int output, bool second_pass ) {
  static size_t nfiles = 0;
  std::list<replace_t> replacements;

  __gnu_cxx::stdio_filebuf<char> outbuf(fdopen(output, "w"), std::ios::out);
  std::ostream out(&outbuf);
  std::ostream_iterator<char> ofs(out);

  // indicate current file
  static const char file_push[] = "\f#FILE PUSH ", file_pop[] = "\f#FILE POP\f";

  if( !second_pass && nfiles++ ) {
    static const char delimiter[] = "\f";
    const char *filename = cobol_filename();
    std::copy(file_push, file_push + strlen(file_push), ofs);
    std::copy(filename, filename + strlen(filename), ofs);
    std::copy(delimiter, delimiter + strlen(delimiter), ofs);
    out.flush();
  }

  // parse CDF directives
  while( mfile.next_line() ) {
    yylloc = mfile.as_location();
    auto copied = parse_copy_directive(mfile);
    if( copied.parsed && copied.fd != -1 ) {
      gcc_assert(copied.erased_lines.p);
      std::copy_if(copied.erased_lines.p, copied.erased_lines.pend, ofs,
                   []( char ch ) { return ch == '\n'; } );
      struct { int in, out; filespan_t mfile; } copy;
      dbgmsg("%s:%d: line %zu, opening %s on fd %d", __func__, __LINE__,
             mfile.lineno(),
             copybook.source(), copybook.current()->fd);
      copy.in = copybook.current()->fd;
      copy.mfile = free_form_reference_format( copy.in );

      if( copied.partial_line.size() ) {
        std::copy(copied.partial_line.p, copied.partial_line.pend, ofs);
      }
      out.flush();

      if( copied.nreplace == 0 ) {
        // process with extant REPLACE directive
        process_file(copy.mfile, output);
      } else {
        copy.out = open_output();
        // process to intermediate, applying COPY ... REPLACING
        process_file(copy.mfile, copy.out);
        copy.mfile = map_file(copy.out);
        replace_directives.pop();
        // process intermediate with extant REPLACE directive
        process_file(copy.mfile, output, true);
        // COPY statement is erased from input if processed successfully
      }
      cobol_filename_restore();
    }

    auto erased = parse_replace_directive(mfile);
    if( erased.p ) {
      std::copy_if( erased.p, erased.pend, ofs,
                    []( char ch ) { return ch == '\n'; } );
    }
    if( replace_directives.empty() ) {
      std::copy(mfile.cur, mfile.eol, ofs);
      continue; // No active REPLACE directive.
    }

    std::list<span_t> segments = segment_line(mfile); // no replace yields
    //                                                // 1 segment

    for( const auto& segment : segments ) {
      std::copy(segment.p, segment.pend, ofs);
    }

    if( segments.size() == 2 ) {
      struct {
        size_t before, after;
        int delta() const { return before - after; } } nlines;
      nlines.before = std::count(segments.front().p,
                                 segments.front().pend, '\n');
      nlines.after  = std::count(segments.back().p, segments.back().pend, '\n');
      if( nlines.delta() < 0 ) {
        yywarn("line %zu: REPLACED %zu lines with %zu lines, "
              "line count off by %d", mfile.lineno(),
              nlines.before, nlines.after, nlines.delta());
      }
      int nnl = nlines.delta();
      while( nnl-- > 0 ) {
        static const char nl[] = "\n";
        std::copy(nl, nl + 1, ofs);
      }
    }
    out.flush();
  }
  // end of file
  if( !second_pass && --nfiles ) {
    std::copy(file_pop, file_pop + strlen(file_pop), ofs);
    out.flush();
  }
}

std::list<span_t>
cdftext::segment_line( filespan_t& mfile ) {
  std::list<span_t> output;

  gcc_assert( ! replace_directives.empty() );
  std::list<replace_t> pending;
  recognize_replacements( mfile, pending );

  if( pending.empty() ) {
    output.push_back( span_t(mfile.cur, mfile.eol) );
    return output;
  }

  for( const replace_t& segment : pending ) {
    gcc_assert(mfile.cur <= segment.before.p);
    gcc_assert(segment.before.pend <= mfile.eodata);

    output.push_back( span_t(mfile.cur, segment.before.p) );
    output.push_back( span_t(segment.after.p, segment.after.pend ) );

    mfile.cur = const_cast<char*>(segment.before.pend);
  }

  if( mfile.eol < mfile.cur ) {
    if( (mfile.eol = std::find(mfile.cur, mfile.eodata, '\n')) < mfile.eodata ) {
      mfile.eol++;
    }
  }

  // last segment takes to EOL
  output.push_back( span_t(mfile.cur, mfile.eol) );

  return output;
}

//////// End of the cdf_text.h file
