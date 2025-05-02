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

#include "cobol-system.h"
#include "coretypes.h"
#include "tree.h"
#include "../../libgcobol/ec.h"
#include "../../libgcobol/common-defs.h"
#include "util.h"
#include "cbldiag.h"
#include "symbols.h"
#include "inspect.h"
#include "../../libgcobol/io.h"
#include "genapi.h"
#include "gengen.h"
#include "../../libgcobol/exceptl.h"
#include "util.h"
#include "genutil.h"

#pragma GCC diagnostic ignored "-Wmissing-field-initializers"

static const ec_descr_t *
ec_type_descr( ec_type_t type ) {
  auto p = std::find( __gg__exception_table, __gg__exception_table_end, type );
  if( p == __gg__exception_table_end ) {
    cbl_internal_error("no such exception: 0x%04x", type);
  }
  return p;
}

const char *
ec_type_str( ec_type_t type ) {
  auto p = ec_type_descr(type);
  return p->name;
}

ec_disposition_t
ec_type_disposition( ec_type_t type ) {
  auto p = ec_type_descr(type);
  return p->disposition;
}

static size_t
ec_level( ec_type_t ec ) {
  if( ec == ec_all_e ) return 1;
  if( 0 == (static_cast<unsigned int>(ec) & ~EC_ALL_E) ) return 2;
  return 3;
}

void
cbl_enabled_exception_t::dump( int i ) const {
  cbl_message(2, "cbl_enabled_exception_t: %2d  {%s, %s, %s, %zu}",
	      i,
	      location? "location" : "    none",
	      ec_type_str(ec),
	      file );
}

cbl_enabled_exceptions_t enabled_exceptions;

void
cbl_enabled_exceptions_t::dump() const {
  extern int yydebug;
  int debug = 1;
  std::swap(debug, yydebug); // dbgmsg needs yydebug

  if( empty() ) {
    dbgmsg("cbl_enabled_exceptions_t:  no exceptions" );
    std::swap(debug, yydebug);
    return;
  }
  int i = 1;
  for( auto& elem : *this ) {
    dbgmsg("cbl_enabled_exceptions_t: %2d  {%s, %s, %zu}",
           i++,
           elem.location? "with location" : "  no location", 
           ec_type_str(elem.ec),
           elem.file );
  }
  std::swap(debug, yydebug);
}

uint32_t 
cbl_enabled_exceptions_t::status() const {
  uint32_t status_word = 0;
  for( const auto& ena : *this ) {
    status_word |= (EC_ALL_E & ena.ec );
  }
  return status_word;
}

std::vector<uint64_t>
cbl_enabled_exceptions_t::encode() const {
  std::vector<uint64_t> encoded;
  auto p = std::back_inserter(encoded);
  for( const auto& ec : *this ) {
    *p++ = ec.location;
    *p++ = ec.ec;
    *p++ = ec.file;
  }
  return encoded;
}

void
cbl_enabled_exceptions_t::turn_on_off( bool enabled,
                                       bool location,
                                       ec_type_t type,
                                       std::set<size_t> files )
{
  // Update current enabled ECs tree on leaving this function. 
  class update_parser_t {
    const cbl_enabled_exceptions_t& ecs;
  public:
    update_parser_t(const cbl_enabled_exceptions_t& ecs) : ecs(ecs) {}
    ~update_parser_t() {
      tree ena = parser_compile_ecs(ecs.encode());
      current_enabled_ecs(ena);
    }
  } update_parser(*this);
  
  // A Level 3 EC is added unilaterally; it can't affect a higher level.
  if( ec_level(type) == 3 ) {
    if( files.empty() ) {
      auto elem = cbl_enabled_exception_t(location, type);
      apply(enabled, elem);
      return;
    }

    for( size_t file : files ) {
      auto elem = cbl_enabled_exception_t(location, type, file);
      apply(enabled, elem);
    }
    return;
  }

  // A new Level 1 or Level 2 EC is likewise simply added. 
  if( enabled ) {
    if( files.empty() ) {
      auto elem = cbl_enabled_exception_t(location, type);
      apply(enabled, elem);
      return;
    }
    for( size_t file: files ) {
      auto elem = cbl_enabled_exception_t(location, type, file);
      apply(enabled, elem);
    }
    return;
  }

  assert(!enabled);
  assert(ec_level(type) < 3);

  /*
   * >> TURN EC [files] CHECKING OFF
   */
  
  if( files.empty() ) {
    // A Level 1 EC with no files disables all ECs
    if( type == ec_all_e ) {
      clear();
      return;
    }
    // Because TURN CHECKING OFF mentioned no files, Remove any matching
    // Level-2 or Level-3 ECs, regardless of their files.
    auto p = begin();
    while( end() != (p = std::find_if( begin(), end(),
                                       [ec = type]( const auto& elem ) {
                                         return
                                           elem.ec != ec_all_e &&
                                           ec_cmp(ec, elem.ec); } )) ) {
      erase(p);
    }
    // Keep the EC as an override if a higher-level would othewise apply.
    p = std::find_if( begin(), end(),
                      [ec = type]( const auto& elem ) {
                        return
                          (elem.ec == ec_all_e || elem.ec < ec) &&
                          elem.file == 0 &&
                          ec_cmp(ec, elem.ec); } );
    if( p != end() ) {
      auto elem = cbl_enabled_exception_t(location, type);
      apply(enabled, elem);
    }
  } else {
    // Remove any matching or lower-level EC for the same file.
    for( size_t file: files ) {
      auto p = begin();
      while( end() != (p = std::find_if( begin(), end(),
                                         [ec = type, file]( const auto& elem ) {
                                           return
                                             // ec is higher level and matches
                                             (ec == ec_all_e || ec <= elem.ec) &&
                                             file == elem.file &&
                                             ec_cmp(ec, elem.ec); } )) ) {
        erase(p);
      }
      // Keep the EC as an override if a higher-level would othewise apply.
      p = std::find_if( begin(), end(),
                        [ec = type, file]( const auto& elem ) {
                          return
                            (elem.ec == ec_all_e || elem.ec < ec) &&
                            file == elem.file &&
                            ec_cmp(ec, elem.ec); } );
      if( p != end() ) {
        auto elem = cbl_enabled_exception_t(location, type, file);
        apply(enabled, elem);
      }
    }
  }
  return;
}

const cbl_enabled_exception_t *
cbl_enabled_exceptions_t::match( ec_type_t type, size_t file ) const {
  auto output = enabled_exception_match( begin(), end(), type, file );
  return output != end()? &*output : NULL;
}

class choose_declarative {
  size_t program;
 public:
  choose_declarative( size_t program ) : program(program) {}

  bool operator()( const cbl_declarative_t& dcl ) {
    return dcl.global || program == symbol_at(dcl.section)->program;
  }
};

bool
sort_supers_last( const cbl_declarative_t& a, const cbl_declarative_t& b ) {
  if( symbol_at(a.section)->program == symbol_at(b.section)->program ) {
    return a.section < b.section;
  }
  return symbol_at(a.section)->program > symbol_at(b.section)->program;
}

cbl_field_t * new_temporary_decl();

/*
 * For a program, create a "DECLARATIVES" entry in the symbol table,
 * representing eligible declarative sections in priorty order:
 * in-program first, followed by any global declaratives in parent
 * programs.  These decribe the USE criteria declared for each
 * declarative section.
 *
 * The field's initial value is actually an array of
 * cbl_declarartive_t, in which the first element is unused, except
 * that array[0].section represents the number of elements, starting
 * at array[1].
 *
 * The returned value is the declarative's symbol index.  It is passed
 * to match_exception, which scans it for a declarative whose criteria
 * match the raised exception.  That function returns the
 * cbl_declarative_t::section, which the program then uses to PERFORM
 * that section.
 */
size_t
symbol_declaratives_add( size_t program,
                         const std::list<cbl_declarative_t>& dcls )
{
  auto n = dcls.size();
  if( n == 0 ) return 0;

  auto blob = new cbl_declarative_t[ 1 + n ];

  auto pend = std::copy_if( dcls.begin(), dcls.end(), blob + 1,
                            choose_declarative(program) );

  std::sort( blob + 1, pend, sort_supers_last );

  // Overload blob[0].section to be the count.
  blob[0].section = (pend - blob) - 1;

  size_t len = reinterpret_cast<char*>(pend)
             - reinterpret_cast<char*>(blob);
  assert(len == (blob[0].section + 1) * sizeof(blob[0]));

  // Construct a "blob" in the symbol table.
  static int blob_count = 1;
  char achBlob[32];
  sprintf(achBlob, "_DECLARATIVE_BLOB%d_", blob_count++);

  cbl_field_data_t data = {};
  data.memsize = capacity_cast(len);
  data.capacity = capacity_cast(len);
  data.initial = reinterpret_cast<char*>(blob);
  data.picture = reinterpret_cast<char*>(blob);
  cbl_field_t field = { 0, FldBlob, FldInvalid, constant_e,
                        0, 0, 0, cbl_occurs_t(), 0, "",
                        0, {}, data, NULL };
  strcpy(field.name, achBlob);

  auto e = symbol_field_add(program, &field);
  parser_symbol_add(cbl_field_of(e));
  return symbol_index(e);
}

/*
 * Generate the code to evaluate declaratives.  This is the "secret
 * section" right after END DECLARATIVES.  Its name is
 * _DECLARATIVES_EVAL, and it is performed after every statement that
 * could raise an exception.
 *
 * The code calls the library routine __gg__match_exception, which
 * compares the raised exception to the criteria set by the USE
 * statements in the DECLARATIVES super-section.  It returns an
 * integer, which is an index to the label in the symbol table that
 * defines the section for the matching USE criteria.
 *
 * The generated code is a sequence of IF statements comparing the
 * returned integer to that of each declarative.  If equal, that
 * section is PERFORMed, and control branches to the end of this
 * section, and thence back to the statement it came from.
 */
#include "../../libgcobol/io.h"
size_t current_file_index();
file_status_t current_file_handled_status();

void
declarative_runtime_match( cbl_field_t *declaratives, cbl_label_t *lave ) {
  if( getenv("GCOBOL_SHOW") )
    {
    fprintf(stderr, "( %d ) %s: \n", cobol_location().first_line, __func__);
    }
  if( getenv("GCOBOL_TRACE") )
    {
    gg_printf(">>>>>>( %d )(%s) declaratives:%s lave:%s\n",
              build_int_cst_type(INT, cobol_location().first_line),
              gg_string_literal(__func__),
              gg_string_literal(declaratives->name),
              gg_string_literal(lave->name),
              NULL_TREE);
    }
  static auto yes = new_temporary(FldConditional);
  static auto psection = new_temporary(FldNumericBin5);

  IF( var_decl_exception_code, ne_op, integer_zero_node ) {
    // Send blob, get declarative section index.
    auto index = new_temporary(FldNumericBin5);
    parser_match_exception(index);
    auto p = declaratives->data.initial;
    const auto dcls = reinterpret_cast<const cbl_declarative_t *>(p);
    size_t ndcl = dcls[0].section; // overloaded

    // Compare returned index to each section index.
    for( auto p = dcls + 1; p < dcls + 1 + ndcl; p++ ) {
      parser_set_numeric( psection, p->section );
      parser_relop( yes, index, eq_op, psection );
      parser_if( yes );
      auto section = cbl_label_of(symbol_at(p->section));
      parser_push_exception();
      parser_perform(section);
      parser_pop_exception();
      parser_label_goto(lave);
      parser_else();
      parser_fi();
    }
  }
  ELSE {
    if( getenv("TRACE1") )
      {
	gg_printf(">>>>>>( %d )(%s) __gg__exception_code is zero\n",
		  build_int_cst_type(INT, cobol_location().first_line),
		  gg_string_literal(__func__),
		  NULL_TREE);
      }
  }
  ENDIF

  parser_label_label(lave);
}

ec_type_t
ec_type_of( const cbl_name_t name ) {
  auto p = std::find_if( __gg__exception_table, __gg__exception_table_end,
                         [name]( const ec_descr_t& descr ) {
                           return 0 == strcasecmp(name, descr.name);
                         } );
  return p == __gg__exception_table_end? ec_none_e : p->type;
}

