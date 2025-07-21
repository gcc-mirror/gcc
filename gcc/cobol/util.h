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

#ifndef _UTIL_H_
#define _UTIL_H_

void cbl_message(int fd, const char *format_string, ...)
  ATTRIBUTE_PRINTF_2;
[[noreturn]] void cbl_internal_error(const char *format_string, ...)
  ATTRIBUTE_GCOBOL_DIAG(1, 2);

void cbl_err(const char *format_string, ...) ATTRIBUTE_GCOBOL_DIAG(1, 2);
void cbl_errx(const char *format_string, ...) ATTRIBUTE_GCOBOL_DIAG(1, 2);

bool fisdigit(int c);
bool fisspace(int c);
int  ftolower(int c);
int  ftoupper(int c);
bool fisprint(int c);

void cobol_set_pp_option(int opt);

void cobol_filename_restore();
const char * cobol_lineno( int );
int cobol_lineno(void);

unsigned long gb4( size_t input );

template <typename P>
static inline const void *
as_voidp( P p ) {
  return static_cast<const void *>(p);
}

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
  
  bool inference_pending() {
    bool tf = first_file && !explicitly;
    first_file = false;
    return tf;
  }

  void infer( const char *bol, bool want_reference_format );
  
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
}; 


void cdf_push();
void cdf_push_call_convention();
void cdf_push_current_tokens();
void cdf_push_dictionary();
void cdf_push_enabled_exceptions();
void cdf_push_source_format();

void cdf_pop();
void cdf_pop_call_convention();
void cdf_pop_current_tokens();
void cdf_pop_dictionary();
void cdf_pop_source_format();
void cdf_pop_enabled_exceptions();

#endif
