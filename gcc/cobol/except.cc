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
    cbl_internal_error("no such exception: 0x%x", type);
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
  cbl_message(2, "cbl_enabled_exception_t: %2d  {%s, %s, %zu}",
              i,
              location? "location" : "    none",
              ec_type_str(ec),
              file );
}

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
  for( auto& elem : *this ) { // cppcheck-suppress constVariableReference
    dbgmsg("cbl_enabled_exceptions_t: %2d  {%s, %s, %lu}",
           i++,
           elem.location? "with location" : "  no location", 
           ec_type_str(elem.ec),
           gb4(elem.file) );
  }
  std::swap(debug, yydebug);
}

// cppcheck-suppress-begin [useStlAlgorithm] because why?
uint32_t 
cbl_enabled_exceptions_t::status() const {
  uint32_t status_word = 0;
  for( const auto& ena : *this ) {
    status_word |= (EC_ALL_E & ena.ec );
  } 
  return status_word;
}
// cppcheck-suppress-end useStlAlgorithm

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
                                       const std::set<size_t>& files )
{
  // Update current enabled ECs tree on leaving this function. 
  class update_parser_t {
    const cbl_enabled_exceptions_t& ecs;
  public:
    explicit update_parser_t(const cbl_enabled_exceptions_t& ecs) : ecs(ecs) {}
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

bool
sort_supers_last( const cbl_declarative_t& a, const cbl_declarative_t& b ) {
  if( symbol_at(a.section)->program == symbol_at(b.section)->program ) {
    return a.section < b.section;
  }
  return symbol_at(a.section)->program > symbol_at(b.section)->program;
}

cbl_field_t * new_temporary_decl();
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
declarative_runtime_match( const std::list<cbl_declarative_t>& declaratives,
			   cbl_label_t *lave )
{
  if( getenv("GCOBOL_SHOW") )
    {
    fprintf(stderr, "( %d ) %s: \n", cobol_location().first_line, __func__);
    }
  if( getenv("GCOBOL_TRACE") )
    {
    gg_printf(">>>>>>( %d )(%s) declaratives: lave:%s\n",
              build_int_cst_type(INT, cobol_location().first_line),
              gg_string_literal(__func__),
              gg_string_literal(lave->name),
              NULL_TREE);
    }
  static auto yes = new_temporary(FldConditional);
  static auto isection = new_temporary(FldNumericBin5);
  static auto index = new_temporary(FldNumericBin5);

  /*
   * Generate a sequence of COBOL IF statements to match the Declarative's
   * symbol table index to its performable section.  The entire sequence is
   * guarded by a runtime IF that evaluates to TRUE only if the "current EC" is
   * nonzero.  This way, when _DECLARATIVES_EVAL is performed, it does nothing
   * if no EC was raised.
   */
  IF( var_decl_exception_code, ne_op, integer_zero_node ) {
    // Get declarative section index matching any raised EC.
    parser_match_exception(index);

    // Compare returned index to each section index.
    for( const auto& dcl : declaratives ) {
      parser_set_numeric( isection, dcl.section );
      parser_relop( yes, index, eq_op, isection );
      parser_if( yes );
      auto section = cbl_label_of(symbol_at(dcl.section));
      parser_push_exception();
      parser_perform(section);
      parser_pop_exception();
      parser_label_goto(lave);
      parser_else();
      parser_fi();
    }
  }
  ELSE {
    if( getenv("GCOBOL_TRACE") )
      {
        gg_printf(">>>>>>( %d )(%s) __gg__exception_code is zero\n",
                  build_int_cst_type(INT, cobol_location().first_line),
                  gg_string_literal(__func__),
                  NULL_TREE);
      }
  }
  ENDIF
}

ec_type_t
ec_type_of( const cbl_name_t name ) {
  auto p = std::find_if( __gg__exception_table, __gg__exception_table_end,
                         [name]( const ec_descr_t& descr ) {
                           return 0 == strcasecmp(name, descr.name);
                         } );
  return p == __gg__exception_table_end? ec_none_e : p->type;
}

