/*
 * Copyright (c) 2021-2025 Symas Corporation
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

#ifndef _CDF_VAL_H_
#define _CDF_VAL_H_

#include <cassert>
#include <cstdint>
#include <cstdlib>

bool scanner_parsing();

/* cdfval_base_t has no constructor because otherwise: 
 * cobol/cdf.h:172:7: note: ‘YDFSTYPE::YDFSTYPE()’ is implicitly deleted 
 *  because the default definition would be ill-formed:
 * 172 | union YDFSTYPE
 * 
 * We use the derived type cdfval_t, which can be properly constructed and
 * operated on, but tell Bison only about its POD base class.
 */
struct YDFLTYPE;
struct cdfval_base_t {
  bool off;
  const char *string;
  int64_t number;
  const cdfval_base_t& operator()( const YDFLTYPE& loc );
};

struct cdf_arg_t {
  int token;
  const char *string;
};

extern int yylineno;
const char * cobol_filename();

struct cdfval_t : public cdfval_base_t {
  int lineno;
  const char *filename;

  cdfval_t()
    : lineno(yylineno), filename(cobol_filename())
  {
    cdfval_base_t::off  = false;
    cdfval_base_t::string = NULL;
    cdfval_base_t::number = 0;
  }
  cdfval_t( const char value[] ) // cppcheck-suppress noExplicitConstructor
    : lineno(yylineno), filename(cobol_filename())
  {
    cdfval_base_t::off  = false;
    cdfval_base_t::string = value;
    cdfval_base_t::number = 0;
  }
  cdfval_t( long long value ) // cppcheck-suppress noExplicitConstructor
    : lineno(yylineno), filename(cobol_filename())
  {
    cdfval_base_t::off  = false;
    cdfval_base_t::string = NULL;
    cdfval_base_t::number = value;
  }
  cdfval_t( long value ) // cppcheck-suppress noExplicitConstructor
    : lineno(yylineno), filename(cobol_filename())
  {
    cdfval_base_t::off  = false;
    cdfval_base_t::string = NULL;
    cdfval_base_t::number = value;
  }
  cdfval_t( int value ) // cppcheck-suppress noExplicitConstructor
    : lineno(yylineno), filename(cobol_filename())
  {
    cdfval_base_t::off  = false;
    cdfval_base_t::string = NULL;
    cdfval_base_t::number = value;
  }
  explicit cdfval_t( const REAL_VALUE_TYPE& r )
    : lineno(yylineno), filename(cobol_filename())
  {
    cdfval_base_t::off  = false;
    cdfval_base_t::string = NULL;
    HOST_WIDE_INT value = real_to_integer(&r);
    cdfval_base_t::number = value;
  }
  cdfval_t( const cdfval_base_t& value ) // cppcheck-suppress noExplicitConstructor
    : lineno(yylineno), filename(cobol_filename())
  {
    cdfval_base_t *self(this);
    *self = value;
  }

  bool is_numeric() const { return ! (off || string); }
  int64_t as_number() const { assert(is_numeric()); return number; }
};

const cdfval_t *
cdf_value( const char name[] );

bool
cdf_value( const char name[], const cdfval_t& value );

typedef std::map<std::string, cdfval_t> cdf_values_t;

cdf_values_t& cdf_dictionary();

#endif
