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

#ifndef _SEMANTIC_TOKEN_H_
#define _SEMANTIC_TOKEN_H_

struct lex_picture_t {
  size_t pos;
  uint64_t attr;
  bool blank_when_zero_ok;
  cbl_field_type_t type;
  cbl_encoding_t encoding;
  cbl_field_data_t *data; // NULL if error

  bool valid() const { return type != FldInvalid; }
  void dump( cbl_name_t input ) const {
    auto len = strlen(input);
    int pad = len < 20? 20 - len : 0;
    fprintf( stderr, "%-20s %s: '%s'%*s @ %u of %u (%s)",
             cbl_field_type_str(type),
             (attr & all_alpha_e)? "A" : " ",
             input,
             pad, "",
             unsigned(pos), unsigned(strlen(input)),
             __gg__encoding_iconv_name(encoding) );
    if( data ) {
      fprintf( stderr, "%2u{%3u,%u,%d}",
               data->memsize, data->capacity(), data->digits, data->rdigits );
    }
    fprintf(stderr, "\n");
  }
};

lex_picture_t is_valid_picture(const cbl_loc_t& lock, const char picture[]);

#endif
