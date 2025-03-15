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

// NOTE:  Unlike charmaps-copy.cc and valprint-copy.cc, this file implements
// the Compiler Directives Facility for the COBOL "COPY" statement.  So, this
// file is the actual source code, and not a copy of something in libgcobol
//
// We regret any confusion engendered.

#include <glob.h>

#include "cobol-system.h"
#include "cbldiag.h"
#include "util.h"
#include "copybook.h"

#define COUNT_OF(X) (sizeof(X) / sizeof(X[0]))

/*
 * There are 3 kinds of replacement types:
 * 1.  keywords, identifiers, figurative constants, and function names
 * 2.  string literals
 * 3.  pseudo-text
 *
 * Types #1 and #3 are delimited by separators:
 * [[:space:],.;()]. String literals begin and end with ["] or [']
 * (matched).
 *
 * Space in pseudo-text is "elastic"; one or more in the matching
 * argument matches one or more in the input. Exception: when the
 * argument is only a comma or semicolon, it matches exactly.
 *
 * The matching algorithm operates on the source file word by word.
 * Comments are copied literally, as are any CDF statements.
 *
 * The candidate word is used as the beginning of all possible
 * matches, in the order they appear in the COPY statement.  If none
 * match, the word is copied to the output and the next word is
 * tried.
 *
 * On a match, the replacement is applied, the result copied to the
 * output, and the next word is tried, starting again from the first
 * match candidate.
 *
 * The parser composes the regular expressions.  It "literalizes"
 * any regex metacharacters that may appear in the COPY text and
 * constructs the correct matching expression for "stretchable"
 * space.  This function only applies them.
 */

extern int yydebug;
const char * cobol_filename();
bool is_fixed_format();
bool is_reference_format();

struct line_t {
  char *p, *pend;
  line_t( size_t len, char *data ) : p(data), pend(data + len) {
    gcc_assert(p && p <= pend);
  }
  line_t( char *data, char *eodata ) : p(data), pend(eodata) {
    gcc_assert(p && p <= pend);
  }
  ssize_t size() const { return pend - p; }
};

static bool
is_separator_space( const char *p) {
  switch( *p ) {
  case ',':
  case ';':
    if( p[1] == 0x20 ) return true;
    break;
  }
  return ISSPACE(*p);
}

static void
verify_bounds( size_t pos, size_t size, const char input[] ) {
  gcc_assert(pos < size );
  if( !( pos < size) ) {
    cbl_internal_error( "REPLACING %zu characters exceeds system capacity"
                        "'%s'", pos, input);
  }
}

/*
 * Replace any separators in the copybook's REPLACING candidate with
 * "stretchable" space.  Escape any regex metacharacters in candidate.
 *
 * "For matching purposes, each occurrence of a separator comma, a
 * separator semicolon, or a sequence of one or more separator spaces
 * is considered to be a single space."
 *
 * If the indicator column is column 7 and is a 'D', we treat that as
 * a SPACE for the purposes of matching a COPY REPLACING or REPLACE
 * directive.
 */
const char *
esc( size_t len, const char input[] ) {
  static char spaces[] = "([,;]?[[:space:]])+";
  static char spaceD[] = "(\n {6}D" "|" "[,;]?[[:space:]])+";
  static char buffer[64 * 1024];
  char *p = buffer;
  const char *eoinput = input + len;

  const char *spacex = is_reference_format()? spaceD : spaces;

  for( const char *s=input; *s && s < eoinput; s++ ) {
    *p = '\0';
    verify_bounds( 4 + size_t(p - buffer), sizeof(buffer), buffer );
    switch(*s) {
    case '^': case '$':
    case '(': case ')':
    case '*': case '+': case '?':
    case '[': case ']':
    case '{': case '}':
    case '|':
    case '.':
      *p++ = '\\';
      *p++ = *s;
      break;
    case '\\':
      *p++ = '[';
      *p++ = *s;
      *p++ = ']';
      break;

    case ';': case ',':
      if( ! (s+1 < eoinput && s[1] == 0x20) ) {
        *p++ = *s;
        break;
      }
      __attribute__((fallthrough));
    case 0x20: case '\n':
      verify_bounds( (p + sizeof(spacex)) - buffer, sizeof(buffer), buffer );
      p = stpcpy( p, spacex );
      while( s+1 < eoinput && is_separator_space(s+1) ) {
        s++;
      }
      break;
    default:
      *p++ = *s;
      break;
    }
  }
  *p = '\0';

#if 0
  dbgmsg("%s:%d: regex '%s'", __func__, __LINE__, buffer);
#endif
  return buffer; // caller must strdup static buffer
}

static int
glob_error(const char *epath, int eerrno) {
  dbgmsg("%s: COPY file search: '%s': %s", __func__, epath, xstrerror(eerrno));
  return 0;
}

void
copybook_directory_add( const char gcob_copybook[] ) {
  if( !gcob_copybook ) return;
  char *directories = xstrdup(gcob_copybook), *p = directories;
  char *eodirs = strchr(directories, '\0');
  gcc_assert(eodirs);

  do {
    char *pend = std::find(p, eodirs, ':');
    if( pend != eodirs ) {
      *pend = '\0';
    }
    copybook.directory_add(p);
    p = pend;
  } while( ++p < eodirs );

}

class case_consistent {
  int lower_upper; // -1 lower, 1 upper
public:
  case_consistent() : lower_upper(0) {}
  bool operator()( char ch ) {
    if( !ISALPHA(ch) ) return true;
    int lu = ISLOWER(ch)? -1 : 1;
    if( !lower_upper ) {
      lower_upper = lu;
      return true;
    }
    return lu == lower_upper;
  }
};

void
copybook_extension_add( const char ext[] ) {
  char *alt = NULL;
  bool one_case = std::all_of( ext, ext + strlen(ext), case_consistent() );
  if( one_case ) {
    alt = xstrdup(ext);
    gcc_assert(alt);
    auto convert = ISLOWER(ext[0])? toupper : tolower;
    std::transform( alt, alt+strlen(alt), alt, convert );
  }
  copybook.extensions_add( ext, alt );
}

extern int yydebug;

const char * copybook_elem_t::extensions;

void
copybook_t::extensions_add( const char ext[], const char alt[] ) {
  char *output;
  if( alt ) {
    output = xasprintf("%s,%s", ext, alt);
  } else {
    output = xstrdup(ext);
  }
  gcc_assert(output);
  if( book.extensions ) {
    char *s = xasprintf("%s,%s", output, book.extensions);
    free(const_cast<char*>(book.extensions));
    free(output);
    book.extensions = s;
  } else {
    book.extensions = output;
  }
}

static inline ino_t
inode_of( int fd ) {
  struct stat sb;
  if( -1 == fstat(fd, &sb) ) {
    cbl_err("could not stat fd %d", fd);
  }
  return sb.st_ino;
}

int
copybook_elem_t::open_file( const char directory[], bool literally ) {
  int erc;
  char  *pattern, *copier = xstrdup(cobol_filename());
  char *dname = NULL;

  if ( directory ) {
    dname = xstrdup(directory);
  } else {
    dname = ldirname(copier);
    gcc_assert (dname != NULL); /* out of memory  */
    if( '\0' == dname[0] ) {
      free (dname);
      dname = NULL;
    }
  }

  char *path = NULL;

  if( dname || library.name ) {
    if( dname && library.name ) {
      path = xasprintf( "%s/%s/%s", dname, library.name, source.name );
    } else {
      const char *dir = dname? dname : library.name;
      path = xasprintf( "%s/%s", dir, source.name );
    }
  } else {
    path = xasprintf( "%s", source.name );
  }

  free(dname);
  gcc_assert(path);

  if( literally ) {
    dbgmsg("copybook_elem_t::open_file: trying %s", path);

    if( (this->fd = open(path, O_RDONLY)) == -1 ) {
      dbgmsg("could not open %s: %m", path);
      return fd;
    }
    this->source.name = path;
    if( ! cobol_filename(this->source.name, inode_of(fd)) ) {
      error_msg(source.loc, "recursive copybook: '%s' includes itself", path);
      (void)! close(fd);
      fd = -1;
    }
    return fd;
  }
  gcc_assert( ! literally );

  if( extensions ) {
    pattern = xasprintf("%s{,.cpy,.CPY,.cbl,.CBL,.cob,.COB,%s}",
                        path, this->extensions);
  } else {
    pattern = xasprintf("%s{,.cpy,.CPY,.cbl,.CBL,.cob,.COB}", path);
  }

  free(copier);

  static int flags = GLOB_MARK | GLOB_BRACE | GLOB_TILDE;
  glob_t globber;

  if( (erc = glob(pattern, flags, glob_error, &globber)) != 0 ) {
    switch(erc) {
    case GLOB_NOSPACE:
      yywarn("COPY file search: out of memory");
      break;
    case GLOB_ABORTED:
      yywarn("COPY file search: read error");
      break;
    case GLOB_NOMATCH:
      dbgmsg("COPY '%s': no files match %s", this->source.name, pattern);
    default:
      break; // caller says no file found
    }
    return -1;
  }

  free(pattern);

  for( size_t i=0; i < globber.gl_pathc; i++ ) {
    auto filename = globber.gl_pathv[i];
    if( (this->fd = open(filename, O_RDONLY)) != -1 ) {
      dbgmsg("found copybook file %s", filename);
      this->source.name = xstrdup(filename);
      if( ! cobol_filename(this->source.name, inode_of(fd)) ) {
        error_msg(source.loc, "recursive copybook: '%s' includes itself", this->source);
        (void)! close(fd);
        fd = -1;
      }
      globfree(&globber);
      return fd;
    }
  }
  yywarn("could not open copy source for '%s'", source);

  globfree(&globber);
  return -1;
}
