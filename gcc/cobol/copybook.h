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

#ifdef _COPYBOOK_H
#pragma message __FILE__ " included twice"
#else
#define _COPYBOOK_H

FILE * copy_mode_start();

const char * cobol_filename();
bool cobol_filename( const char *name, ino_t inode );

void scanner_parsing( int token, bool tf );
void scanner_parsing_toggle();
void scanner_parsing_pop();

/*
 * COPY support On encountering a COPY statement, the parser continues
 * to parse, collecting the replacement values, if any.  At statement
 * end (at the period), the system rearranges input to apply the
 * replacements before the input text is read by the lexer.
 */

enum replace_type_t { string_e, token_e, pseudo_e };

struct copybook_replace_t {
  replace_type_t type;
  const char *src, *tgt;
};
class copybook_t;

class copybook_elem_t {
  friend copybook_t;
  struct copybook_loc_t {
    YYLTYPE loc;
    const char *name;
    copybook_loc_t() : name(NULL) {}
  } source, library;
  bool suppress;
  static const char *extensions;
 public:
  struct { bool source, library; } literally;
  int  fd;
  size_t nsubexpr;
  std::deque<copybook_replace_t> replacements;

  copybook_elem_t()
    : suppress(false)
    , fd(-1)
    , nsubexpr(0)
    , regex_text(NULL)
  {
    literally = {};
  }

  void clear() {
    suppress = false;
    nsubexpr = 0;
    if( fd ) close(fd);
    fd = -1;
    // TODO: free src & tgt
    replacements.clear();
  }

  int open_file( const char dir[], bool literally = false );
  void extensions_add( const char ext[], const char alt[] );

  static inline bool is_quote( const char ch ) {
    return ch == '\'' || ch == '"';
  }
  static inline bool quoted( const char name[] ) {
    gcc_assert(name);
    return is_quote(name[0]);
  }
  static char * dequote( const char orig[] ) {
    gcc_assert(quoted(orig));
    auto name = (char*)xcalloc(1, strlen(orig));
    gcc_assert(name);
    char *tgt = name;

    // For a literal name, we de-quote it and try to open it in the
    // current working directory.  The COBOL literal could include
    // (escaped) doubled quotes, which we reduce to one.
    for( const char *src = orig; src < orig + strlen(orig); ) {
      if( is_quote(src[0]) ) {
        if( src[0] == src[1] ) {
          *tgt++ = *src++; // copy one of doubled quote
        }
        src++; // skip quote
        continue;
      }
      *tgt++ = *src++;
    }
    *tgt = '\0';

    return name;
  }

private:
  char *regex_text;
};

class uppername_t {
  std::string upper;
 public:
  uppername_t( const std::string input ) : upper(input) {
    std::transform(input.begin(), input.end(), upper.begin(), 
		   []( char ch ) { return TOUPPER(ch); } );
  }
  const char *data() const { return upper.data(); }
};

class copybook_t {
  std::list<const char *> directories;
  copybook_elem_t book;

  // Take copybook name from the environment, if defined, else use it verbatim.
  static const char * transform_name( const char name[] ) {
    uppername_t uname(name);
    const char *value = getenv(name);
    if( !value ) {
      value = getenv(uname.data()); // try uppercase of envar name
      if( !value ) value = name; // keep original unmodified
    }
    return xstrdup(value);
  }

 public:
  copybook_t() { directories.push_back(NULL); }

  void suppress( bool tf = true  ) { book.suppress = tf; }
  bool suppressed()                { return book.suppress; }
  void source( const YYLTYPE& loc, const char name[] ) {
    book.source.loc = loc;
    book.literally.source = copybook_elem_t::quoted(name);
    book.source.name = book.literally.source?
      copybook_elem_t::dequote(name) : transform_name(name);
  }
  void library( const YYLTYPE& loc, const char name[] ) {
    book.library.loc = loc;
    book.literally.library = copybook_elem_t::quoted(name);
    book.library.name = book.literally.library?
      copybook_elem_t::dequote(name) : transform_name(name);
  }
  void replacement( replace_type_t type, const char src[], const char tgt[] ) {
    copybook_replace_t elem = { type, src, tgt };
    book.replacements.push_back(elem);
  }

  copybook_elem_t *current() { return &book; }
  const char *source() const { return book.source.name; }
  const char *library() const { return book.library.name; }

  int open(YYLTYPE loc, const char name[]) {
    int fd = -1;
    book.clear();
    this->source(loc, name);

    for( auto dir : directories ) {
      if( true ) {
        dbgmsg("copybook_t::open '%s' OF '%s' %s",
               book.source.name,
               dir? dir: ".",
               book.literally.source? ", literally" : "" );
      }
      if( (fd = book.open_file(dir, book.literally.source)) != -1 ) break;
    }
    return fd;
  }

  const char * directory_add( const char name[] ) {
    directories.push_back(name);
    return name;
  }
  void extensions_add( const char ext[], const char alt[] );
};

extern copybook_t copybook;

#endif
