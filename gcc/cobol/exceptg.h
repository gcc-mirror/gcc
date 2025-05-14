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
#ifndef _EXCEPTL_H_
#define _EXCEPTL_H_

/*  This file contains exception processing declarations needed by the gcc/cobol
    compilation.  It's not included in the libgcobol compilation.  */

extern const char * ec_type_str( ec_type_t type );
extern ec_disposition_t ec_type_disposition( ec_type_t type );

extern void declarative_runtime_match(cbl_field_t *declaratives,
                                      cbl_label_t *lave );

static inline ec_disposition_t
ec_implemented( ec_disposition_t disposition ) {
  return ec_disposition_t( size_t(disposition) & ~0x80 );
}

// >>TURN arguments
class exception_turn_t;
bool apply_cdf_turn( const exception_turn_t& turn );

class exception_turn_t {
  friend bool apply_cdf_turn( const exception_turn_t& turn );
  typedef std::list<size_t> filelist_t;
  typedef std::map<ec_type_t, filelist_t> ec_filemap_t;
  ec_filemap_t exceptions;
  bool enabled, location;
 public:

  exception_turn_t() : enabled(false), location(false) {};

  exception_turn_t( ec_type_t ec, bool enabled = true )
    : enabled(enabled)
  {
    add_exception(ec);
  } 

  bool enable( bool enabled ) {
    return this->enabled = enabled;
  } 
  bool enable( bool enabled, bool location ) {
    this->location = location;
    return this->enabled = enabled;
  } 

  const ec_filemap_t& exception_files() const { return exceptions; }

  bool add_exception( ec_type_t type, const filelist_t files = filelist_t() ) {
    ec_disposition_t disposition = ec_type_disposition(type);
    if( disposition != ec_implemented(disposition) ) {
	cbl_unimplementedw("CDF: exception '%s'", ec_type_str(type));
    }
    auto elem = exceptions.find(type);
    if( elem != exceptions.end() ) return false; // cannot add twice

    exceptions[type] = files;
    return true;
  }

  void clear() {
    for( auto& ex : exceptions ) {
      ex.second.clear();
    }
    exceptions.clear();
    enabled = location = false;
  }

};

size_t symbol_declaratives_add( size_t program,
                                const std::list<cbl_declarative_t>& dcls );

#endif



