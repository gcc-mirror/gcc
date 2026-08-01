/*
 * Copyright (c) 2021-2026 Symas Corporation
 * All rights reserved.
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

#include <algorithm>
#include <cassert>
#include <cctype>
#include <cstdlib>
#include <cstring>

#include <sys/mman.h>

#ifndef _LEXIO_H_
#define _LEXIO_H_

#define SPACE ' '

bool lexer_echo();
bool is_reference_format();

static inline bool isquote( char ch ) {
  return ch == '\'' || ch == '"';
}

static inline void
erase_source( char *src, char *esrc ) {
  std::replace_if(src, esrc,
                  [](char ch) { return ch != '\n'; },
                  SPACE );
}

/*
   * Column number as in Cobol, with 1 at the start of the line.
   *  0: free-format, but comment lines may start with '*'.
   *  N: columns less than N treated as space.
   *     '/' or '*' in N starts a comment
   *     'D' starts a debug line
   *     '-' is a line-continuation indicator
   *     Others ignored.
   *  Right margin is enforced if it is greater than left margin.
   */
struct bytespan_t {
  char *data, *eodata;

  bytespan_t() : data( nullptr), eodata(nullptr) {}
  
  bytespan_t( char *data, char *eodata )
    : data(data), eodata(eodata)
  {
    if( eodata < data ) {
      this->eodata  = data + strlen(data);
    }
    assert( this->data <= this->eodata );
  }
  size_t size() const { return eodata - data; }

  bool in_string( ) const {
    char open = '\0';

    for( char *q = data; (q = std::find_if(q, eodata, isquote)) != eodata; q++) {
      if( !open ) {
        open = *q; // first quote opens
        continue;
      }
      if( open == *q && q + 1 < eodata && q[0] == q[1] ) { // doubled
        q++;
        continue;
      }
      if( open == *q ) open = '\0'; // closing quote must match
    }
    return isquote(open);
  }

  char * append( const char *input, const char *eoinput );

  bytespan_t&
  update( char *line, char *eoline, size_t right_margin ) {
    *this = bytespan_t(line, eoline);
    if( right_margin && data + right_margin < eodata ) {
      erase_source(data + right_margin, eodata);
      eodata = data + right_margin;
    }
    eodata = std::find(data, eodata, '\n');
    return *this;
  }
};

size_t parse_error_inc();

struct filespan_t : public bytespan_t {
  char *cur, *eol, *quote;
 private:
  size_t iline, icol;
  size_t line_quote72;
  static char empty_file[8];
 public:
  filespan_t()
    : cur(data), eol(data), quote(NULL), iline(0), icol(0), line_quote72(0)
  {}
  filespan_t(void *p, size_t len)
    : bytespan_t( static_cast<char*>(p), static_cast<char*>(p) + len )
    , cur(data), eol(data), quote(NULL), iline(0), icol(0), line_quote72(0)
  {}

  size_t lineno() const { return iline; }
  size_t colno() const  { return icol; }

  void lineno_reset()  { iline = 0; }
  size_t colno( size_t icol )  { return this->icol = icol; }

  bool nada() const { return data == empty_file; }
  void use_nada() {
    assert(!data);
    cur = eol = data = empty_file;
    eol = eodata = empty_file + sizeof(empty_file) - 1;
  }

  const char *ccur() const { return cur; }

  /*
   * "If an alphanumeric or national literal that is to be continued on
   * the next line has as its last character a quotation mark in
   * column 72, the continuation line must start with two consecutive
   * quotation marks."
   */
  bool was_quote72() const { return iline == line_quote72 + 1; }

  bool line_contains_nul() const {
    const char *nul = std::find(cur, eol, '\0');
    return nul != eol;
  }

  void sanitize_nul() {
    char *nul = std::find(cur, eol, '\0');
    if( nul != eol ) {
      int icol = nul - cur; // cppcheck-suppress shadowVariable
      fprintf(stderr, "%s:%d:%d: error: NUL character detected in input\n%*s\n",
              cobol_filename(), int(iline), ++icol,
              int(eol - cur)-1, cur);
      parse_error_inc();
      std::replace(nul, eol, '\0', SPACE);
    }
  }

  size_t next_line(bool is_reference_format) {
    // Before advancing, mark the current line as ending in a quote, if true.
    if( is_reference_format && 72 <= line_length() ) {
      if( isquote(cur[71]) ) { line_quote72 = iline; }
    }

    cur = eol;
    assert(data <= cur && cur <= eodata);
    if( cur == eodata ) return 0;

    eol = std::find(cur, eodata, '\n');

    if( eol < eodata ) {
      ++eol;
      ++iline;
      icol = 0;
    }
    return eol - cur;
  }

  size_t line_length() const { return eol - cur; }

  bool is_blank_line() const {
    auto p = std::find_if( cur, eol, []( char ch ) { return !fisspace(ch); } );
    return p == eol;
  }

  cbl_loc_t as_location() const {
    cbl_loc_t loc;

    loc.first_line = loc.last_line = 1 + iline;
    loc.first_column = loc.last_column = 1 + icol;
    return loc;
  }

};

#if USE_STD_REGEX
# include <regex>
#else
# include "dts.h"
using dts::csub_match;
using dts::cmatch;
using dts::regex;
using dts::regex_search;
#endif

struct span_t {
 protected:
  void verify() const {
    if( !p ) {
      dbgmsg("span_t::span_t: p is NULL");
    } else if( ! (p <= pend) ) {
      dbgmsg("span_t::span_t: p %p > pend %p", p, pend);
    }
    assert(p && p <= pend);
  }
  span_t& trim() {
    while( p < pend     && isblank(p[0]) ) p++;
    while( p < pend - 1 && isblank(pend[-1]) ) pend--;
    return *this;
  }
 public:
  const char *p, *pend;
  span_t() : p(NULL), pend(NULL) {}

  span_t( size_t len, const char *data ) : p(data), pend(data + len) {
    verify();
  }
  span_t( const char *data, const char *eodata ) : p(data), pend(eodata) {
    verify();
  }
  // cppcheck-suppress operatorEqRetRefThis
  span_t& operator=( const csub_match& cm ) {
    p = cm.first;
    pend = cm.second;
    return p && pend ? trim() : *this;
  }

  int size() const { return pend - p; }

  size_t nlines() const { return p && pend? std::count(p, pend, '\n') : 0; }

  span_t dup() const {
    auto  output = new char[size() + 1];
    auto eout = std::copy(p, pend, output);
    *eout = '\0';
    return span_t(output, eout);
  }
  const char * has_nul() const {
    auto p_l = std::find(this->p, pend, '\0');
    return p_l != pend? p_l : NULL;
  }

  bool at_eol() const {
    return p < pend && '\n' == pend[-1];
  }
  const char * optional_eol() const {
    return at_eol() ? "" : "\n";
  }
};

struct replace_t {
  struct span_t before, after;
  replace_t() : before(span_t()), after(span_t()) {}
  replace_t( span_t before, span_t after )
    : before(before), after(after)
  {}
  replace_t& reset() {
    before = after = span_t();
    return *this;
  }
};

/*
 * The default source format, whether free or fixed, is determined
 * heuristically by examining the PROGRAM-ID line, if it exists, in the first
 * input file. If that file does not have such a line, the default is free
 * format.  Else the format is set to fixed if anything appears on that line
 * that would prohibit parsing it as free format,
 */
class source_format_t {
  bool first_file, explicitly;
  int left, right;
public:
  source_format_t()
    : first_file(true), explicitly(false), left(0), right(0)
  {}
  void indicator_column_set( int column ) {
    explicitly = true;
    if( column == 0 ) right = 0;
    if( column < 0 ) {
      column = -column;
      right = 73;
    }
    left = column;
  }
  
  char * indicated( char *bol, const char *eol, char ch = '\0' );

  bool inference_pending() {
    bool tf = first_file && !explicitly;
    first_file = false;
    return tf;
  }

  inline bool is_fixed() const { return left == 7; }
  inline bool is_reffmt() const { return is_fixed() && right == 73; }
  inline bool is_free() const { return ! is_fixed(); }
  
  const char * description() const {
    if( is_reffmt() ) return "REFERENCE";
    if( is_fixed() ) return "FIXED";
    if( is_free() ) return "FREE";
    gcc_unreachable();
  }    

  inline int left_margin() {
    return left == 0? left : left - 1;
  }
  inline int right_margin() {
    return right == 0? right : right - 1;
  }

  void infer( const char *bol, bool want_reference_format ) {
    if( bol ) {
      left = 7;
      if( want_reference_format ) {
        right = 73;
      }
    }
    dbgmsg("%s:%d: %s format detected", __func__, __LINE__,
           description());
  }
};

typedef std::stack<source_format_t> source_format_stack_t;

#include <cstdio>

class cdftext {
  static bool please_push_filename;
  static int command_line_indicator_column;

  friend void cobol_set_indicator_column( int column );

  static void process_file( filespan_t mfile, int output, 
                            source_format_stack_t source_format, 
                            bool second_pass = false ) ;
  static filespan_t  free_form_reference_format( int fd, source_format_stack_t& );

  static void output_push_directive( const char filename[],
                                    std::ostream_iterator<char>& ofs);

  static filespan_t map_file( int fd );

  static void echo_input( int input, const char filename[] );

  static int open_input( const char filename[] );
  static int open_output();

  static std::list<span_t> segment_line( filespan_t& mfile );

 public:
  static FILE * lex_open( const char filename[] );
};

#endif
