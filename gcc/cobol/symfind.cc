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

extern int yydebug;

static bool
is_data_field( symbol_elem_t& e ) {
  if( e.type != SymField ) return false;
  auto f = cbl_field_of(&e);
  if( f->name[0] == '\0' ) return false;
  if( is_filler(f) ) return false;

  return f->type != FldForward;
}

class sym_name_t {
public: // TEMPORARY
  const char *name;
  size_t program, parent;
public:
  explicit sym_name_t( const char name[] )
    : name(name), program(0), parent(0) { assert(name[0] == '\0'); }
  sym_name_t( size_t program, const char name[], size_t parent )
    : name(name), program(program), parent(parent) {}

  const char * c_str() const { return name; }

  // Order by: Program, Name, Parent.
  bool operator<( const sym_name_t& that ) const {
    if( program == that.program ) {
      int by_name = strcasecmp(name, that.name);
      return by_name == 0? parent < that.parent : by_name < 0;
    }
    return program < that.program;
  }
  bool operator==( const char *name ) const {
    return strcasecmp(this->name, name) == 0;
  }

  bool same_program( size_t program ) const {
    return program == this->program;
  }
};

typedef std::map< sym_name_t, std::vector<size_t> > symbol_map_t;


static symbol_map_t symbol_map;

typedef std::map <field_key_t, std::list<size_t> > field_keymap_t;
static field_keymap_t symbol_map2;

/*
 * As each field is added to the symbol table, add its name and index
 * to the name map.  Initially the type is FldInvalid.  Those are
 * removed by symbols_update();
 */
void
update_symbol_map2( const symbol_elem_t *e ) {
  auto field = cbl_field_of(e);

  if( ! field->is_typedef() ) {
    switch( field->type ) {
    case FldForward:
    case FldLiteralN:
      return;
    case FldLiteralA:
      if( ! field->is_key_name() ) return;
      break;
    default:
      break;
    }
  }

  field_key_t fk( e->program, field );
  symbol_map2[fk].push_back(symbol_index(e));
}

/*
 * Purge any field whose type is FldInvalid.  Remove any names that do
 * not map to any field.
 */
void
finalize_symbol_map2() {
  std::set<field_key_t> empties;

  for( auto& elem : symbol_map2 ) {
    auto& fields( elem.second );
    fields.remove_if( []( auto isym ) {
			auto f = cbl_field_of(symbol_at(isym));
			return f->type == FldInvalid;
		      } );
    if( fields.empty() ) empties.insert(elem.first);
  }

  for( const auto& key : empties ) {
    symbol_map2.erase(key);
  }
}

static void
dump_symbol_map2( const field_key_t& key, const std::list<size_t>& candidates ) {
  if( !yydebug ) return;
  char *fields = NULL, sep[2] = "";

  for( auto candidate : candidates ) {
    char *tmp = fields;
    fields = xasprintf("%s%s %3zu", tmp? tmp : "", sep, candidate);
    sep[0] = ',';
    free(tmp);
  }

  dbgmsg( "%s:%d: %3zu %s {%s}", __func__, __LINE__,
          key.program, key.name, fields );
  free(fields);
}

void
dump_symbol_map2() {
  int n = 0;
  for( const auto& elem : symbol_map2 ) {
    const field_key_t& key( elem.first );
    const std::list<size_t>& candidates( elem.second);
    if( key.program != 0 ) {
      dump_symbol_map2( key, candidates );
      n++;
    }
  }
  dbgmsg("symbol_map2 has %d program elements", n);
}

static void
dump_symbol_map_value( const char name[], const symbol_map_t::value_type& value ) {
  if( !yydebug ) return;
  char *ancestry = NULL, sep[2] = "";
  auto p = value.second.begin();

  for( ; p != value.second.end(); p++ ) {
    char *tmp = ancestry;
    ancestry = xasprintf("%s%s %3zu", tmp? tmp : "", sep, *p);
    sep[0] = ',';
    free(tmp);
  }

  dbgmsg( "%s:%d: %s -> %-24s {%s }", __func__, __LINE__,
         name, value.first.c_str(), ancestry );
  free(ancestry);
}


static void
dump_symbol_map_value1( const symbol_map_t::value_type& value ) {
  dump_symbol_map_value( "result", value );
}

static symbol_map_t::value_type
field_structure( symbol_elem_t& sym ) {
  static const symbol_map_t::value_type
    none( symbol_map_t::key_type( 0, "", 0 ), std::vector<size_t>() );

  if( !is_data_field(sym) ) return none;

  cbl_field_t *field = cbl_field_of(&sym);

  symbol_map_t::key_type key( sym.program, field->name, field->parent );
  symbol_map_t::value_type elem( key, std::vector<size_t>() );
  symbol_map_t::mapped_type& v(elem.second);

  for( v.push_back(field_index(field)); field->parent > 0; ) {
    symbol_elem_t *par = symbol_at(field->parent);

    if( SymFile == par->type ) {
      v.push_back(field->parent);
      break;
    }
    assert( SymField == par->type );
    v.push_back(field->parent);

    field = cbl_field_of(par);

    // for C of R and B of A, where R redefines B, skip B: vector is [C, R, A].
    cbl_field_t *redefined = symbol_redefines(field); // if R redefines B
    if( redefined ) {
      field = redefined;  // We will use B's parent on next iteration
    }
  }

  return elem;
}

void erase_symbol_map_fwds( size_t beg ) {
  for( auto p = symbols_begin(beg); p < symbols_end(); p++ ) {
    if( p->type != SymField ) continue;
    const auto& field(*cbl_field_of(p));
    if( field.type == FldForward ) {
      symbol_map.erase( sym_name_t(p->program, field.name, field.parent) );
    }
  }
}

void
build_symbol_map() {
  static size_t beg = 0;
  size_t end = symbols_end() - symbols_begin();

  if( beg == end ) return;
  const size_t nsym = end - beg;

  std::transform( symbols_begin(beg), symbols_end(),
                  std::inserter(symbol_map, symbol_map.begin()),
                  field_structure );
  beg = end;

  symbol_map.erase(sym_name_t(""));

  if( yydebug ) {
    dbgmsg( "%s:%d: %zu of %zu symbols inserted into %zu in symbol_map",
           __func__, __LINE__, nsym, end, symbol_map.size() );
  }
}

bool
update_symbol_map( symbol_elem_t *e ) {
  auto output = symbol_map.insert(field_structure(*e));
  return  output.second;
}

class is_name {
  const char *name;
public:
  is_name( const char *name ) : name(name) {}
  bool operator()( symbol_map_t::value_type& elem ) {
    const bool tf = elem.first == name;
    return tf;
  }
  protected:
    void dump_key( const char tag[], const symbol_map_t::key_type& key ) const {
      dbgmsg( "symbol_map key: %s { %3zu %3zu %s }",
             tag, key.program, key.parent, key.name );
  }
};

/*
 * Construct a list of ancestors based on a set of candidate groups.
 * Presented with an item, see if any group an ancestor.  If so,
 * replace the item's ancestry with the group's ancestry (thus
 * shortening the chain).  Otherwise, return an empty element.
 */
class reduce_ancestry {
  std::vector<symbol_map_t::mapped_type> candidates;
  static symbol_map_t::mapped_type
    candidates_only( const symbol_map_t::value_type& elem ) { return elem.second; }
public:
  reduce_ancestry( const symbol_map_t& groups )
    : candidates( groups.size() )
    {
      std::transform( groups.begin(), groups.end(), candidates.begin(),
                      candidates_only );
    }
  symbol_map_t::value_type
  reduce( const symbol_map_t::value_type& item ) {
    static symbol_map_t::value_type none( "", std::vector<size_t>() );

    auto ancestors = candidates.begin();
    for( ; ancestors != candidates.end(); ancestors++ ) {
      assert(!ancestors->empty()); // ancestry always starts with self
      auto p = std::find( item.second.begin(), item.second.end(),
                                  ancestors->front() );
      if( p != item.second.end() ) {
        // Preserve symbol's index at front of ancestor list.
        symbol_map_t::mapped_type shorter(1 + ancestors->size());
        auto p = shorter.begin();
        *p = item.second.front();
        shorter.insert( ++p, ancestors->begin(), ancestors->end() );
        return make_pair(item.first, shorter);
      }
    }
    return none;
  }
  symbol_map_t::value_type
  operator()( symbol_map_t::value_type item ) { return reduce(item); }
};

class different_program {
  size_t program;
public:
  different_program( size_t program ) : program(program) {}
  bool operator()( const symbol_map_t::value_type& item ) const {
    return ! item.first.same_program(program);
  }
};

class in_scope {
  size_t program;

  static size_t prog_of( size_t program ) {
    auto L = cbl_label_of(symbol_at(program));
    return L->parent;
  }

public:
  in_scope( size_t program ) : program(program) {}

  // A symbol is in scope if it's defined by this program or by an ancestor.
  bool operator()( const symbol_map_t::value_type& item ) const {
    symbol_elem_t *e = symbol_at(item.second.front());
    for( size_t prog = this->program; prog != 0; prog = prog_of(prog) ) {
      if( e->program == prog ) return true;
    }
    return false;
  }
};

/*
 * For a field symbol and list of qualifier IN/OF names, see if the
 * namelist matches the symbol's name and ancectors' names.  Success
 * is all names match within scope.
 *
 * All symbols local to the program are in scope.  A containing
 * program's symbol matches only if global_e is set on it or one of
 * its ancestors.
 */
static bool
name_has_names( const symbol_elem_t *e,
                const std::list<const char *>& names, bool in_scope )
{
  assert( ! names.empty() );
  auto name = names.rbegin();

  while( e && e->type == SymField ) {
    auto field = cbl_field_of(e);
    if( field->type == FldForward ) return false;

    if( 0 == strcasecmp(field->name, *name) ) {
      in_scope = in_scope || (field->attr & global_e);
      if( ++name == names.rend() ) break;
    }

    // first name must match
    if( name == names.rbegin() ) break;

    // Do not chase redefines if we have an 01 record for an FD.
    if( field->file ) {
      e = symbol_at(field->file);
      assert(1 == field->level);
      assert(e->type == SymFile);
      break;
    }

    /*
     * If the current field redefines another, it is not a member of
     * its parent, but of its grandparent, if any.  Not a loop because
     * REDEFINES cannot be chained.
     */
    cbl_field_t *parent = symbol_redefines(field);
    if( parent ) {
      field = parent;
      assert( NULL == symbol_redefines(field) );
    }

    e = field->parent ? symbol_at(field->parent) : NULL;
  }

  if( e && e->type == SymFile ) {
    // first name can be a filename
    auto file = cbl_file_of(e);
    if( 0 == strcasecmp(file->name, *name) ) name++;
  }

  return in_scope && name == names.rend();
}

size_t end_of_group( size_t igroup );

static std::vector<size_t>
symbol_match2( size_t program,
               std::list<const char *> names, bool local = true )
{
  std::vector<size_t> fields;

  field_key_t key(program, names.back());

  auto plist = symbol_map2.find(key);
  if( plist != symbol_map2.end() ) {
    for( auto candidate : plist->second ) {
      auto e = symbol_at(candidate);
      if( name_has_names( e, names, local ) ) {
        fields.push_back( symbol_index(e) );
      }
    }
  }

  if( fields.empty() ){
    if( program > 0 ) { // try containing program
      program  = cbl_label_of(symbol_at(program))->parent;
      return symbol_match2( program, names, program == 0 );
    }
  }

  if( yydebug ) {
    char *ancestry = NULL;
    const char *sep = "";
    for( auto name : names ) {
      char *partial = ancestry;
      int asret = asprintf(&ancestry, "%s%s%s", partial? partial : "", sep, name);
      assert(asret);
      sep = " -> ";
      assert(ancestry);
      free(partial);
    }

    if( fields.empty() ) {
      dbgmsg("%s: '%s' matches no fields", __func__, ancestry);
      dump_symbol_map2();
    } else {
      char *fieldstr = NULL;
      sep = "";
      for( auto field : fields ) {
        char *partial = fieldstr;
        int asret = asprintf(&fieldstr, "%s%s%zu", partial? partial : "", sep, field);
        assert(asret);
        sep = ", ";
        assert(fieldstr);
        free(partial);
      }

      dbgmsg("%s: '%s' matches %zu fields: {%s}", __func__, ancestry, fields.size(), fieldstr);
      free(fieldstr);
    }
    free(ancestry);
  }

  return fields;
}

/*
 * The names list is in top-down order, front-to-back.  This function
 * iterates backwards over the list, looking for the parent of N at
 * N-1.
 */
static symbol_map_t
symbol_match( size_t program, std::list<const char *> names ) {
  auto matched = symbol_match2( program, names );
  symbol_map_t output;

  for( auto isym : matched ) {
    auto e = symbol_at(isym);
    auto f = cbl_field_of(e);

    symbol_map_t::key_type key( e->program, f->name, f->parent );
    auto p = symbol_map.find(key);
    if( p == symbol_map.end() ) {
      yyerror("%s is not defined", key.name);
      continue;
    }
    auto inserted = output.insert(*p);
    if( ! inserted.second ) {
      yyerror("%s is not a unique reference", key.name);
    }
  }
  return output;
}

static const symbol_elem_t * symbol_field_alias_01;

const symbol_elem_t *
symbol_field_alias_begin() {
  return symbol_field_alias_01 = symbol_field_current_record();
}
void
symbol_field_alias_end() {
  symbol_field_alias_01 = NULL;
}

std::pair <symbol_elem_t *, bool>
symbol_find( size_t program, std::list<const char *> names ) {
  symbol_map_t items = symbol_match(program, names);

  if( symbol_field_alias_01 && items.size() != 1 ) {
    symbol_map_t qualified;
    size_t i01( symbol_index(symbol_field_alias_01) );
    std::copy_if( items.begin(), items.end(),
                  std::inserter(qualified, qualified.begin()),
                  [i01]( auto item ) {
                    const std::vector<size_t>& ancestors(item.second);
                    return ancestors.back() == i01;
                  } );
    items = qualified;
  }

  auto unique = items.size() == 1;

  if( !unique ) {
    if( items.empty() ) {
      return std::pair<symbol_elem_t *, bool>(NULL, false);
    }
    if( yydebug ) {
      dbgmsg( "%s:%d: '%s' has %zu possible matches",
             __func__, __LINE__, names.back(), items.size() );
      std::for_each( items.begin(), items.end(), dump_symbol_map_value1 );
    }
  }

  size_t isym = items.begin()->second.front();
  auto output = std::make_pair(symbol_at(isym), unique);

  assert( FldForward != field_at(isym)->type );

  return output;
}

class in_group {
  size_t group;
public:
  in_group( size_t group ) : group(group) {}

  bool operator()( symbol_map_t::const_reference elem ) const {
    return 0 < std::count( elem.second.begin(),
                              elem.second.end(), this->group );
  }
};

symbol_elem_t *
symbol_find_of( size_t program, std::list<const char *> names, size_t group ) {
  symbol_map_t input = symbol_match(program, names);

  symbol_map_t items;
  std::copy_if( input.begin(), input.end(),
                std::inserter(items, items.begin()), in_group(group) );

  if( items.size() == 1 ) {
    size_t isym = items.begin()->second.front();
    assert( FldForward != field_at(isym)->type );
    return symbol_at(isym);
  }

  if( yydebug ) {
    dbgmsg( "%s:%d: '%s' has %zu possible matches",
           __func__, __LINE__, names.back(), input.size() );
    std::for_each( input.begin(), input.end(), dump_symbol_map_value1 );
  }

  return NULL;
}
