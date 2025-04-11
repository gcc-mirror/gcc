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

#include <fstream> // Before cobol-system because it uses poisoned functions
#include "cobol-system.h"

#include "coretypes.h"
#include "tree.h"

#include <search.h>
#include <iconv.h>
#include "../../libgcobol/ec.h"
#include "../../libgcobol/common-defs.h"
#include "util.h"
#include "cbldiag.h"
#include "symbols.h"
#include "inspect.h"
#include "../../libgcobol/io.h"
#include "genapi.h"

#pragma GCC diagnostic ignored "-Wunused-result"
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"

bool
lexio_dialect_mf() { return dialect_mf(); }

class symbol_pair_t
{
  const symbol_elem_t *first, *last;
public:
  symbol_pair_t( const symbol_elem_t * first, const symbol_elem_t * end = NULL )
    : first(first), last(end)
  {}

  // used only by std::find to locate a pointer between first and last
  bool operator==( const symbol_pair_t& that ) const {
    return this->first <= that.first && that.first < this->last;
  }

  size_t index( const symbol_elem_t *psym ) const {
    assert( first <= psym && psym < last );
    return psym - first;
  }
};

static std::map<size_t, YYLTYPE> field_locs;

void
symbol_field_location( size_t ifield, const YYLTYPE& loc ) {
  gcc_assert(field_at(ifield));
  field_locs[ifield] = loc;
}
YYLTYPE
symbol_field_location( size_t ifield ) {
  auto p = field_locs.find(ifield);
  gcc_assert(p != field_locs.end());
  return p->second;
}

static struct symbol_table_t {
  int fd;
  size_t capacity, nelem;
  size_t first_program, procedures;
  struct registers_t {
    size_t file_status, linage_counter, return_code,
           exception_condition, very_true, very_false;
    registers_t() {
      file_status = linage_counter = return_code =
        exception_condition = very_true = very_false = 0;
    }
  } registers;

  struct symbol_elem_t *elems;

  std::map<elem_key_t, size_t> specials;
  std::map<elem_key_t, std::list<size_t>> labels;

  std::vector<symbol_pair_t> mappings;

  symbol_table_t()
    : fd(-1)
    , capacity(0), nelem(0), first_program(0), procedures(0)
    , elems(NULL)
  {}

  /*
   * To compute an offset into the symbol table from an element
   * pointer, first search the mappings to determine which one it
   * belongs to.
   */
  size_t index( const symbol_elem_t * psym ) const {
    assert(psym);
    auto pend = mappings.end();
    auto p = std::find(mappings.begin(), pend, symbol_pair_t(psym));
    assert( p != pend );  // pysm does not point to a symbol in the symbol table.
    return p->index(psym);
  }

  void save() { mappings.push_back( symbol_pair_t( elems, elems + capacity ) ); }

  size_t size() const { return capacity * sizeof(elems[0]); }

  void labelmap_add( const symbol_elem_t *e ) {
    const char *name = cbl_label_of(e)->name;
    labels[ elem_key_t(e->program, name) ].push_back( symbol_index(e) );
  }
} symbols;

static symbol_table_t&
symbol_table_extend() {
  static FILE *mapped;

  if( symbols.nelem == 0 ) {  // first time: create file & set initial capacity
    assert(mapped == NULL && symbols.fd == -1);

    if( (mapped = tmpfile()) == NULL ) {
      cbl_err( "could not create temporary file for symbol table");
    }

    symbols.fd = fileno(mapped);
    assert(symbols.fd > 0);

    symbols.capacity = 64;
  } else {
    if( 0 != msync(symbols.elems, symbols.size(), MS_SYNC | MS_INVALIDATE) ) {
      cbl_err( "%s:%d: could not synchronize symbol table with mapped file",
               __func__, __LINE__ );
    }
  }

  symbols.capacity *= 2;
  off_t len = symbols.size();

  if( 0 != ftruncate(symbols.fd, len) ) {
    cbl_err( "%s:%d:could not extend symbol table to %zu elements",
        __func__, __LINE__, symbols.capacity);
  }

  /*
   * We never unmap a disused symbol table, to avoid referencing
   * invalid pointers.  The table itself contains no pointers; it uses
   * table indexes. But the parser API uses pointers, and sometimes
   * the table needs to be extended before the code generator is done
   * with them.
   *
   * By extending the file and mapping it anew, the old mapping
   * remains valid, and the new mapping extends it in a different part
   * of the virtual address space. Page 0 of the old map, for example,
   * occupies the same physical RAM as before, but is shared between
   * two mappings.
   */

  void *mem = mmap(NULL, len, PROT_READ | PROT_WRITE, MAP_SHARED, symbols.fd, 0);

  if( MAP_FAILED == mem ) {
    cbl_err( "%s:%d: could not extend symbol table", __func__, __LINE__);
  }
  symbols.elems = static_cast<struct symbol_elem_t*>(mem);

  symbols.save(); // add new mapping to list of mappings

  return symbols;
}

static struct symbol_elem_t *
symbol_at_impl( size_t index, bool internal = true ) {
  assert( index <= symbols.nelem );
  if( !internal ) assert( index < symbols.nelem );
  symbol_elem_t *e = symbols.elems + index;

  if( index == symbols.nelem ) return e;

  if( e->type == SymField && cbl_field_of(e)->type == FldForward ) {
    return symbol_field(e->program,
                        cbl_field_of(e)->parent, cbl_field_of(e)->name);
  }
  return e;
}

struct symbol_elem_t *
symbol_at( size_t index ) {
  return symbol_at_impl(index, false);
}

static char decimal_point = '.';

size_t file_status_register() { return symbols.registers.file_status; }
size_t return_code_register() { return symbols.registers.return_code; }
size_t very_true_register()   { return symbols.registers.very_true; }
size_t very_false_register()  { return symbols.registers.very_false; }
size_t ec_register() { return symbols.registers.exception_condition; }

cbl_refer_t *
cbl_refer_t::empty() {
  static cbl_refer_t empty;
  return &empty;
}

cbl_field_t *
cbl_span_t::from_field() { assert(from); return from->field; }
cbl_field_t *
cbl_span_t::len_field()  { assert(len); return len->field; }

cbl_ffi_arg_t::
cbl_ffi_arg_t( cbl_refer_t* refer, cbl_ffi_arg_attr_t attr )
  : optional(false)
  , crv(by_reference_e)
  , attr(attr)
  , refer(refer? *refer : cbl_refer_t())
{
  if( refer && refer != refer->empty() ) delete refer;
}

cbl_ffi_arg_t::
cbl_ffi_arg_t( cbl_ffi_crv_t crv,
               cbl_refer_t* refer, cbl_ffi_arg_attr_t attr )
  : optional(false)
  , crv(crv)
  , attr(attr)
  , refer(refer? *refer : cbl_refer_t())
{
  if( refer && refer != refer->empty() ) delete refer;
}

#define ERROR_FIELD(F, ...)                                \
 do{                                                        \
  auto loc = symbol_field_location(field_index(F));        \
  error_msg(loc, __VA_ARGS__);                                \
 } while(0)


cbl_field_t *
symbol_valid_udf_args( size_t function, std::list<cbl_refer_t> args ) {
  auto L = cbl_label_of(symbol_at(function));
  if( ! L->returning ) {
    dbgmsg("logic error: %s does not define RETURNING", L->name);
    return NULL;
  }
  auto e = std::find_if( symbol_at(function), symbols_end(),
                         []( auto symbol ) {
                           if( symbol.type == SymDataSection ) {
                             auto section(symbol.elem.section);
                             return section.type == linkage_sect_e;
                           }
                           return false;
                         } );
  for( auto arg : args ) {
    size_t iarg(1);
    e++; // skip over linkage_sect_e, which appears after the function
    if( e->type != SymField ) {
      ERROR_FIELD(arg.field,
                  "FUNCTION %s has no defined parameter matching arg %zu, '%s'",
                  L->name, iarg, arg.field->name );
      return NULL;
    }

    auto tgt = cbl_field_of(e);

    if( ! valid_move(tgt, arg.field) ) {
      ERROR_FIELD(tgt, "FUNCTION %s arg %zu, '%s' cannot be passed to %s, type %s",
                L->name, iarg, arg.field->pretty_name(),
                tgt->pretty_name(), 3 + cbl_field_type_str(tgt->type) );
      return NULL;
    }
  }
  return cbl_field_of(symbol_at(L->returning));
}

static const struct cbl_occurs_t nonarray = cbl_occurs_t();

#if 0
# define CONSTANT_E constant_e
#else
# define CONSTANT_E intermediate_e
#endif


class group_size_t {
  size_t size;
 public:
  group_size_t() : size(0) {}
  group_size_t& operator+( const cbl_field_t& field ) {
    size += field.data.capacity;
    return *this;
  }
  size_t capacity() const { return size; }
};

enum  { constq = constant_e | quoted_e };

static symbol_elem_t
elementize( cbl_field_t& field ) {
  symbol_elem_t sym (SymField);
  sym.elem.field = field;
  return sym;
}

size_t
field_index( const cbl_field_t *f ) {
  assert(f);
  return symbol_index(symbol_elem_of(f));
}

static inline bool
is_forward( const struct symbol_elem_t *e ) {
  return cbl_field_of(e)->type == FldForward;
}
static inline bool
is_forward( const cbl_field_t *field ) {
  return field->type == FldForward;
}

static inline bool
has_parent( const struct symbol_elem_t *e ) {
  return cbl_field_of(e)->parent > 0;
}

/*
 * A field is global if it's marked global, or if any of its parents are.
 * Actually, only 01 level can be global, but this works.
 */
bool
is_global( const cbl_field_t * field ) {
  do {
    if( (field->attr & global_e) == global_e ) {
      return true;
    }
    if( field->parent > 0 ) {
      symbol_elem_t *e = symbol_at(field->parent);
      if( SymField == e->type ) {
        field = cbl_field_of(e);
        continue;
      }
    }
    break;
  } while(true);
  return false;
}

static bool
special_pair_cmp( const cbl_special_name_t& key,
                  const cbl_special_name_t& elem ) {
  const bool matched = key.id == elem.id || 0 == strcasecmp(key.name, elem.name);

  return matched;
}

/*
 * On insertion, a label may be a definition or a forward reference.
 * On reference, a label may be qualified or not.  If not, we don't
 *               know if it refers to a section or a paragraph.
 *
 * Declarations and references always use line == 0; only definitions
 * have a line number.
 *
 * An unqualified reference is denoted LblNone.  If not found, it is
 * inserted as a declaration: LblNone, line 0.
 *
 * A qualified reference is denoted LblParagraph with a section, and
 * with line = 0.  A qualified reference updates an unqualified
 * declaration; the declation is upgraded to LblParagraph with the
 * section as its parent, but still with no line (because it's still
 * undefined).
 *
 * Matching rules (assuming names match):
 * Key                 Element       New    Effect
 * type  parent  line  type  parent  type
 * None     -          None     -           unqualified ref matches decl
 * None     -          Sect     -           unqualified ref matches section
 * None     -          Para     x           unqualified ref matches any para
 * Sect     -          None     -    Sect   section definition updates decl
 * Sect     -          Sect     -           section matches section
 * Para     S       0  None     -    Para S qualified ref updates decl
 * Para     x      >0  None     -    Para   paragraph definition updates decl
 * Para     S       0  Para     S           qualified ref matches decl or def
 * Para     x      >0  Para     x    Para   paragraph definition updates decl
 *                                          if elem.line == 0.
 *
 * All other combinations fail or are invalid by assertion.
 */
static bool label_cmp( const cbl_label_t& key,
                       const cbl_label_t& elem, bool names_matched = false ) {
  if( ! names_matched ) {
    if( 0 != strcasecmp(key.name, elem.name) ) return false;
  }

  switch( key.type ) {

  case LblNone:
    assert(0 == key.explicit_parent());
    assert(0 == key.line);
    switch( elem.type ) {
    case LblNone:
    case LblSection:
      assert(!elem.explicit_parent());
      return true;
      break;
    case LblParagraph:
      return true;
      break;
    default:
      break;
    }
    break;

  case LblSection:
    assert(0 == key.explicit_parent());
    switch( elem.type ) {
    case LblNone:
    case LblSection:
      assert(!elem.explicit_parent());
      return true;
      break;
    default:
      break;
    }
    break;

  case LblParagraph:
    switch( elem.type ) {
    case LblNone:
      if(elem.explicit_parent()) {
        cbl_errx( "%s:%d: LblNone '%s' has parent #%zu",
             __func__, __LINE__, elem.name, elem.parent );
      }
      assert(!elem.explicit_parent());
      return true;
      break;
    case LblParagraph:
      if( key.parent == elem.parent ) { // explicit or implicit
        return key.line == 0 || elem.line == 0 || key.line == elem.line;
        // negative key.line never matches (causing insertion)
      }
      break;
    default:
      break;
    }
    break;
  default:
    gcc_unreachable();
  }
  return false;
}

static int
symbol_elem_cmp( const void *K, const void *E )
{
  const struct symbol_elem_t
    *k=static_cast<const struct symbol_elem_t *>(K),
    *e=static_cast<const struct symbol_elem_t *>(E);

  if( k->type != e->type ) return 1;
  if( k->program != e->program && !is_program(*k)) return 1;

  switch( k->type ) {
  case SymFilename:
    return strcmp(k->elem.filename, e->elem.filename);
    break;
  case SymDataSection:
    return k->elem.section.type == e->elem.section.type ? 0 : 1;
    break;
  case SymFunction:
    return strcmp(k->elem.function.name, e->elem.function.name);
    break;
  case SymField:
    if( has_parent(k) && cbl_field_of(k)->parent != cbl_field_of(e)->parent ) {
      return 1;
    }
    // If the key has attributes, they must match.
    if( (cbl_field_of(k)->attr & global_e) == global_e ) {
      if( !is_global(cbl_field_of(e)) ) {
        return 1;
      }
    }
    // forwards match forwards only
    if(  is_forward(k) && !is_forward(e) ) return 1;
    if( !is_forward(k) &&  is_forward(e) ) return 1;
    break;
  case SymLabel:
    // A LblNone element (created by a forward reference) that lacks a parent
    // matches on name only. It becomes a LblParagraph or LblSection.
    // Remember: this test is for adding labels, not resolving references.
    {
      const cbl_label_t& key =  *cbl_label_of(k);
      const cbl_label_t& elem = *cbl_label_of(e);

      if( key.type != elem.type ) {
        if( !(key.type == LblNone || elem.type == LblNone) ) return 1;
      }

      switch(key.type) {
      case LblProgram: // There are no forward program labels
        if( key.parent > 0 && key.parent != elem.parent ) return 1;
        assert(key.parent == elem.parent || key.parent == 0);
        break;
      case LblNone: case LblSection: case LblParagraph:
        return label_cmp(key, elem)? 0 : 1;
        break;
      default:
        if( key.parent != elem.parent ) { // allow zero parent of LblNone
          if( !(elem.type == LblNone && elem.explicit_parent() == 0) ) return 1;
        }
        assert(key.parent == elem.parent || elem.type == LblNone);
      }

      if( key.os_name && elem.os_name ) {
        if( 0 == strcasecmp(key.os_name, elem.os_name) ) return 0; // success
      }
      return strcasecmp(key.name, elem.name);
    }
    break;
  case SymSpecial:
    return special_pair_cmp(k->elem.special, e->elem.special)? 0 : 1;
    break;
  case SymAlphabet:
    return strcasecmp(k->elem.alphabet.name, e->elem.alphabet.name);
    break;
  case SymFile:
    // If the key is global, so must be the found element.
    if( (cbl_file_of(k)->attr & global_e) == global_e &&
        (cbl_file_of(e)->attr & global_e) != global_e ) {
      return 1;
    }
    return strcasecmp(k->elem.file.name, e->elem.file.name);
    break;
  }
  assert(k->type == SymField);

#if 1
  // Used by symbol_literalA
  // Literals have no name.  They match on their constant initial value.
  if( is_literal(cbl_field_of(k)) && is_literal(cbl_field_of(e)) ) {
    return strcmp(cbl_field_of(k)->data.initial, cbl_field_of(e)->data.initial);
  }
#endif
  if( cbl_field_of(k)->has_attr(filler_e) ) {
    return 1; // filler never matches
  }

  return strcasecmp(cbl_field_of(k)->name, cbl_field_of(e)->name);
}

cbl_label_ref_t::
cbl_label_ref_t( size_t program, const cbl_label_t& context, int line,
                 const char name[], size_t isect )
  : qualified(isect != 0)
  , context(context)
  , line(line)
  , handle(NULL)
{
  cbl_label_type_t type = isect? LblParagraph : LblNone;
  struct cbl_label_t label = { type, isect, line };
  assert(strlen(name) < sizeof(label.name));
  strcpy(label.name, name);

  target = symbol_label_add(program, &label);
  assert(target);
}

struct cbl_label_t *
symbol_label( size_t program, cbl_label_type_t type, size_t section,
              const char name[],
              const char os_name[] )
{
  static cbl_name_t lname;
  std::transform(name, name + strlen(name) + 1, lname, tolower);
  elem_key_t key( program, lname );
  auto p = symbols.labels.find(key);
  if( p == symbols.labels.end()) return NULL;

  cbl_label_t protolabel = { type, section };
  protolabel.os_name = os_name;
  assert(strlen(name) < sizeof protolabel.name);
  strcpy(protolabel.name, name);

  const std::list<size_t>& syms(p->second);
  auto psym =
    std::find_if( syms.begin(), syms.end(),
                  [key=protolabel]( size_t isym ) {
                    const auto& elem = *cbl_label_of(symbol_at(isym));

                    switch(key.type) {
                    case LblProgram: // There are no forward program labels
                      if( key.parent > 0 && key.parent != elem.parent ) return false;
                      assert(key.parent == elem.parent || key.parent == 0);
                      break;
                    case LblNone: case LblSection: case LblParagraph:
                      return label_cmp(key, elem, true);
                        break;
                    default:
                      if( key.parent != elem.parent ) { // allow zero parent of LblNone
                        if( !(elem.type == LblNone && elem.explicit_parent() == 0) ) return false;
                      }
                      assert(key.parent == elem.parent || elem.type == LblNone);
                      break;
                    }

                    if( key.os_name && elem.os_name ) {
                      if( 0 == strcasecmp(key.os_name, elem.os_name) ) return true; // success
                    }
                    return true;
                  } );
  if( psym == syms.end() ) return NULL;
  return cbl_label_of(symbol_at(*psym));
}

size_t
symbol_label_id( const cbl_label_t *label ) {
  auto e = symbol_elem_of(label);
  size_t label_index = symbol_index(e);
  assert( label_index < std::numeric_limits<uint32_t>::max() );
  return label_index;
}

struct cbl_label_t *
symbol_program( size_t parent, const char name[] )
{
  cbl_label_t label = {};
  label.type = LblProgram;
  label.parent = parent;
  assert(strlen(name) < sizeof label.name);
  strcpy(label.name, name);

  struct symbol_elem_t key( SymLabel, 0 ), *e;
  key.elem.label = label;

  e = static_cast<struct symbol_elem_t *>(lfind( &key, symbols.elems,
                                                 &symbols.nelem, sizeof(key),
                                                 symbol_elem_cmp ) );
  return e? cbl_label_of(e) : NULL;
}

extern int yydebug;

static size_t
symbols_dump( size_t first, bool header );

struct symbol_elem_t *
symbol_function( size_t parent, const char name[] )
{
  auto p = std::find_if( symbols_begin(), symbols_end(),
                         [parent, name]( const auto& elem ) {
                           if( elem.type == SymLabel ) {
                             auto L = cbl_label_of(&elem);
                             if( L->type == LblFunction ) {
                               return 0 == strcasecmp(L->name, name);
                             }
                           }
                           return false;
                         } );

  if( yydebug && p == symbols_end() ) symbols_dump( symbols.first_program, true);

  return p == symbols_end()? NULL : p;

  cbl_label_t label = {};
  label.type = LblFunction;
  label.parent = parent;
  assert(strlen(name) < sizeof label.name);
  strcpy(label.name, name);

  struct symbol_elem_t key(SymLabel, 0), *e;
  key.elem.label = label;

  e = static_cast<struct symbol_elem_t *>(lfind( &key, symbols.elems,
                                                 &symbols.nelem, sizeof(key),
                                                 symbol_elem_cmp ) );
  return e;
}

struct symbol_elem_t *
symbol_special( size_t program, const char name[] )
{
  elem_key_t key( program, name );
  auto p = symbols.specials.find(key);
  if( p == symbols.specials.end() ) return NULL;
  return symbol_at(p->second);
}

struct symbol_elem_t *
symbol_alphabet( size_t program, const char name[] )
{
  cbl_alphabet_t alphabet(YYLTYPE(), custom_encoding_e);
  assert(strlen(name) < sizeof alphabet.name);
  strcpy(alphabet.name, name);

  struct symbol_elem_t key(SymAlphabet, program), *e;
  key.elem.alphabet = alphabet;

  e = static_cast<struct symbol_elem_t *>(lfind( &key, symbols.elems,
                                                 &symbols.nelem, sizeof(key),
                                                 symbol_elem_cmp ) );
  return e;
}

symbol_elem_t *
symbols_begin( size_t first )
{
  return symbols.elems + first;
}

symbol_elem_t *
symbols_end(void)
{
  return symbols.elems + symbols.nelem;
}

cbl_field_t *
symbol_redefines( const struct cbl_field_t *field ) {
  if( field->parent == 0 ) return NULL;
  struct symbol_elem_t *e = symbol_at(field->parent);
  if( e->type == SymField ) {
    cbl_field_t *parent = cbl_field_of(e);
    if( parent->level == field->level || field->level == 66) {
      return parent;
    }
    return NULL;
  }
  return NULL;
}

static cbl_field_t *
symbol_explicitly_redefines( const cbl_field_t *field ) {
  auto f = symbol_redefines(field);
  if( f && is_record_area(f) ) return NULL;
  return f;
}

static uint32_t
field_size( const struct cbl_field_t *field ) {
  size_t n = field->occurs.ntimes();
  return field->data.capacity * (n > 0? n : 1);
}

const char *
cbl_field_attr_str( cbl_field_attr_t attr ) {
  switch(attr) {
  case none_e: return "none";
  case figconst_1_e: return "figconst_1";
  case figconst_2_e: return "figconst_2";
  case figconst_4_e: return "figconst_4";
  case rjust_e: return "rjust";
  case ljust_e: return "ljust";
  case zeros_e: return "zeros";
  case signable_e: return "signable";
  case constant_e: return "constant";
  case function_e: return "function";
  case quoted_e: return "quoted";
  case filler_e: return "filler";
  case _spare_e: return "temporary";
  case intermediate_e: return "intermediate";
  case embiggened_e: return "embiggened";
  case all_alpha_e: return "all_alpha";
  case all_x_e: return "all_x";
  case all_ax_e: return "all_ax";
  case prog_ptr_e: return "prog_ptr";
  case scaled_e: return "scaled";
  case refmod_e: return "refmod";
  case based_e: return "based";
  case any_length_e: return "any_length";
  case global_e: return "global";
  case external_e: return "external";
  case blank_zero_e: return "blank_zero";
  case linkage_e: return "linkage";
  case local_e: return "local";
  case leading_e: return "leading";
  case separate_e: return "separate";
  case envar_e: return "envar";
  case dnu_1_e: return "dnu_1";
  case bool_encoded_e: return "bool";
  case hex_encoded_e: return "hex";
  case depends_on_e: return "depends_on";
  case initialized_e: return "initialized";
  case has_value_e: return "has_value";
  case ieeedec_e: return "ieeedec";
  case big_endian_e: return "big";
  case same_as_e: return "same_as";
  case record_key_e: return "record_key";
  case typedef_e: return "typedef";
  case strongdef_e: return "strongdef";
  }
  return "???";
}

uint32_t
cbl_field_t::size() const {
  return field_size(this);
}

size_t
cbl_field_t::set_attr( cbl_field_attr_t attr ) {
  if( attr == signable_e ) {
    if( ! has_attr(attr) && this->var_decl_node != NULL ) {
      parser_field_attr_set(this, attr);
    }
  }
  return this->attr |= size_t(attr);
}

size_t
cbl_field_t::clear_attr( cbl_field_attr_t attr ) {
  if( attr == signable_e ) {
    if( this->var_decl_node != nullptr && has_attr(attr) ) {
      parser_field_attr_set(this, attr, false);
    }
  }
  return this->attr &= ~size_t(attr);
}

static uint32_t
field_memsize( const struct cbl_field_t *field ) {
  uint32_t n = field->occurs.ntimes();
  n = field->data.capacity * (n > 0? n : 1);
  return std::max(n, field->data.memsize);
}

static inline  bool
field_skippable( const struct cbl_field_t *field ) {
  // skip forward references
  if( field->type == FldForward ) {
    return true;
  }

  // typedef takes no space
  if( field->is_typedef() ) {
    return true;
  }

  // skip 88s and 66s because they don't add to capacity
  if( field->level == 66 || field->level == 88 ) {
    return true;
  }

  // skip switch values because they're just compile-time constants
  if( field->type == FldSwitch ) {
    return true;
  }

  // skip INDEXED BY if its level is 0.
  if( field->level == 0 && field->type == FldIndex ) {
    return true;
  }
  return false;
}

/*
 * Start at a LEVEL01 field and walk through it until the next LEVEL01
 * or LEVEL77, if any.  Update the offset of each subfield field
 * based on the sizes of all the preceding items.
 *
 * A field whose parent is the same level is a REDEFINE.  It does not
 * use additional storage, and has an offset the same as its "parent".
 */
static struct symbol_elem_t *
update_block_offsets( struct symbol_elem_t *block)
{
  assert(block);
  assert(block->type == SymField);

  uint32_t offset = cbl_field_of(block)->offset;
  const uint32_t block_level = cbl_field_of(block)->level;

  struct symbol_elem_t *e = block;
  for( ++e; e < symbols_end(); e++ ) {
    if( e->type != SymField ) {
      // Ignore non-fields
      continue;
    }

    cbl_field_t *field = cbl_field_of(e);

    if( field->level == 66 ) {
      field->offset = parent_of(field)->offset;
      continue;
    }

    if( field_skippable(field) ) {
      continue;
    }

    if( field->level <= block_level || field->level == LEVEL77 ) {
      break;  // end of group
    }

    if( symbol_redefines(field) ) {
      field->offset = parent_of(field)->offset;
    } else {
       field->offset = offset;
       offset += field_memsize(field);
    }

    if( field->type == FldGroup ) {
      e = update_block_offsets(e) - 1;
    }
  }
  return e;
}

static inline bool
end_of_group( const cbl_field_t *group, const cbl_field_t *field ) {
  // A group ends when we strike a level less than or equal to
  // group_symbol->level, or when we hit a LEVEL77.

  // reject forward fields
  if( is_forward(field) ) return false;

  // If field redefines group, we're not at the end.
  if( group == symbol_redefines(field) ) return false;

  // An index that is part of a table is part of the group.
  if( field->level == 0 && field->type == FldIndex ) return false;

  return
    field->level <= group->level ||
    field->level == LEVEL77 ||
    field->level == 66;
}

class eog_t {
  const cbl_field_t * group;
public:
  eog_t( const symbol_elem_t *e ) : group(cbl_field_of(e)) {}

  bool operator()( symbol_elem_t& e ) {
    return e.type == SymField && end_of_group(group, cbl_field_of(&e));
  }
};

size_t
end_of_group( size_t igroup ) {
  symbol_elem_t * group(symbol_at(igroup));

  if( group->type == SymFile ) {
    cbl_field_t * first_record = symbol_file_record(cbl_file_of(group));
    assert(first_record);
    group = symbol_at(field_index(first_record));
    for( auto e = group + 1; e < symbols_end(); e++ ) {
      auto isym = symbol_index(e);
      if( e->program != group->program ) return isym;
      if( e->type == SymLabel ) return isym; // end of data division
      if( e->type == SymField ) {
        auto f = cbl_field_of(e);
        if( f->level == LEVEL77 || f->level == 66 ) return isym;
        if( f->level == 1 && f->parent != igroup ) {
          return isym;
        }
      }
    }
    return symbols.nelem;
  }

  eog_t eog(symbol_at(igroup));
  symbol_elem_t *e = std::find_if( symbols_begin(++igroup), symbols_end(), eog );
  return e - symbols_begin();
}

size_t
symbol_field_capacity( const cbl_field_t *field ) {
  class sym_field_size {
   public:
    sym_field_size() {}
    static size_t capacity( size_t n, const symbol_elem_t& elem ) {
      if( elem.type == SymField ) {
        const cbl_field_t *f = cbl_field_of(&elem);
        if( is_elementary(f->type) ) {
          return n + ::field_size(f);
        }
      }
      return n;
    }
  };
  size_t bog = field_index(const_cast<cbl_field_t*>(field));
  size_t eog = end_of_group(bog);
  size_t size = std::accumulate( symbol_at(bog), symbol_at_impl(eog),
                                 0, sym_field_size::capacity );

  if(true) dbgmsg("%s: %02u %s.data.capacity was computed as %zu", __func__,
                  field->level, field->name, size);

  return size;
}

static bool
has_odo( const symbol_elem_t& e ) {
  return e.type == SymField && cbl_field_of(&e)->occurs.depending_on > 0;
}

// a debug version of symbol_find_odo
struct cbl_field_t *
symbol_find_odo_debug( cbl_field_t * field ) {
  size_t bog = field_index(field), eog = end_of_group(bog);
  dbgmsg("%s: %s is #%zu - #%zu of %zu, ends at %s", __func__,
        field->name, bog, eog, symbols.nelem,
        eog == symbols.nelem? "[end]" : cbl_field_of(symbol_at(eog))->name );

  auto e = std::find_if( symbol_at(bog), symbol_at_impl(eog, true), has_odo );
  if( e != symbol_at_impl(eog, true) ) {
    dbgmsg("%s: %s has ODO at #%zu (return '%s')", __func__,
          field->name, symbol_index(e),
          cbl_field_of(e)->name );
  }
  return e == symbol_at_impl(eog, true)? NULL : cbl_field_of(e);
}

// Return OCCURS DEPENDING ON table subordinate to field, if any.
struct cbl_field_t *
symbol_find_odo( cbl_field_t * field ) {
  size_t bog = field_index(field), eog = end_of_group(bog);
  auto e = std::find_if( symbol_at(bog), symbol_at_impl(eog, true), has_odo );
  return e == symbol_at_impl(eog, true)? NULL : cbl_field_of(e);
}

static inline bool
is_index( const cbl_field_type_t type ) { return type == FldIndex; }

static size_t
symbols_dump( size_t first, bool header ) {
  size_t ninvalid = 0;

  if( !yydebug ) return 0;

  if( header ) {
    fprintf(stderr, "Symbol Table has %zu elements\n",
            symbols_end() - symbols_begin());
  }

  for( struct symbol_elem_t *e = symbols_begin(first); e < symbols_end(); e++ ) {
    char *s;

    switch(e->type) {
    case SymFilename:
      s = xasprintf("%4zu %-18s %s", e->program,
                    "Filename", e->elem.filename);
      break;
    case SymDataSection:
      s = xasprintf("%4zu %-18s line %d", e->program,
                    cbl_section_of(e)->name(), cbl_section_of(e)->line);
      break;
    case SymFunction:
      s = xasprintf("%4zu %-15s %s", e->program,
                    "Function", e->elem.function.name);
      break;
    case SymField: {
      auto field = cbl_field_of(e);
      char *odo_str = NULL;
      if( field->occurs.depending_on != 0 ) {
        odo_str = xasprintf("odo %zu", field->occurs.depending_on );
      }
      ninvalid += cbl_field_of(e)->type == FldInvalid? 1 : 0;
      s = xasprintf("%4zu %-18s %s (%s)", e->program,
                    cbl_field_type_str(cbl_field_of(e)->type) + 3,
                    field_str(cbl_field_of(e)),
                    odo_str? odo_str :
                    cbl_field_type_str(cbl_field_of(e)->usage) + 3);
      }
      break;
    case SymLabel:
      s = xasprintf("%4zu %-18s %s", e->program,
                    "Labe1l", e->elem.label.str());
      if( LblProgram == cbl_label_of(e)->type ) {
        const auto& L = *cbl_label_of(e);
        if( L.os_name ) {
          char *base = s;
          s = xasprintf("%s as \"%s\")", base, L.os_name);
          free(base);
        }
      }
      break;
    case SymSpecial:
      s = xasprintf("%4zu %-18s id=%2d, %s", e->program,
                    "Special", e->elem.special.id, e->elem.special.name);
      break;
    case SymAlphabet:
      s = xasprintf("%4zu %-18s encoding=%2d, '%s'", e->program, "Alphabet",
                    int(e->elem.alphabet.encoding), e->elem.alphabet.name);
      break;
    case SymFile:
      s = xasprintf("%4zu %-18s    %-20s", e->program,
                    "File", e->elem.file.name);
      {
        char same_as[26] = "";
        if( cbl_file_of(e)->same_record_as > 0 ) {
          sprintf(same_as, "s%3zu", cbl_file_of(e)->same_record_as);
        }
        const char *type = file_org_str(e->elem.file.org);
        char *part = s;

        s = xasprintf("%s %-4s %s %s %s{%zu-%zu} status=#%zu",
                      part, same_as, type,
                      e->elem.file.keys_str(),
                      cbl_file_of(e)->varies()? "varies " : "",
                      cbl_file_of(e)->varying_size.min,
                      cbl_file_of(e)->varying_size.max,
                      cbl_file_of(e)->user_status);
        free(part);
      }
      break;
    default:
      dbgmsg("%s: cannot dump symbol type %d", __func__, e->type);
      continue;
    }
    fprintf(stderr, "%4zu: %s\n", e - symbols_begin(), s);
    free(s);
  }
  return ninvalid;
}

static bool
grow_redefined_group( cbl_field_t *redefined, const cbl_field_t *field ) {
  assert(redefined);
  assert(field);
  assert(redefined == symbol_redefines(field));

  /*
   *  When this function is called, redefined elementary items are
   *  already resized, if eligible.
   */
  if( redefined->type != FldGroup ) return false;

  /*
   * 8) The storage area required for the subject of the entry
   * shall not be larger than the storage area required for the
   * data item referenced by data-name-2, unless the data item
   * referenced by data- name-2 has been specified with level
   * number 1 and without the EXTERNAL clause.
   */
  if( 1 < redefined->level ) {
    if( field_memsize(redefined) < field_memsize(field) ) {
      ERROR_FIELD(field, "line %d: %s (size %u) larger than REDEFINES %s (size %u)",
               field->line,
               field->name, field_memsize(field),
               redefined->name, field_memsize(redefined));
      return false;
    }
  }

  redefined->data.memsize = std::max(field_memsize(redefined),
                                     field_memsize(field));

  return true;
}


/*
 * Input is a symbol-table element, always a field.
 * For elementary fields, return the input.
 * For groups, return the element after the last field in the group.
 */
static struct symbol_elem_t *
 calculate_capacity( struct symbol_elem_t *e) {
  // For each group, sum capacities of children.  Exclude:
  //    FldClass, FldForward
  //    FldIndex with level 0 (really, any level 0)
  //    REDEFINES

  cbl_field_t *group = cbl_field_of(e);

  if( is_literal(group) ) return e;
  if( is_index(group->type) ) return e; // 01 can be index type.

  if( is_elementary(group->type) ) { // "group" is in fact just a field
    if( is_record_area(group) ) {
      if( group->data.capacity == 0 ) {
        const auto& file = *cbl_file_of(symbol_at(group->file));
        group->data.capacity = file.varying_size.max;
      }

      // Find 01s for the file that is not a record area field.
      for( auto p = symbols_begin(e->program) + 1; p < symbols_end(); ++p ) {
        p = std::find_if( p, symbols_end(),
                          [group](const symbol_elem_t& elem) {
                            if( elem.type == SymField ) {
                              auto field = cbl_field_of(&elem);
                              return field != group &&
                                field->file == group->file;
                            }
                            return false;
                          } );
        // If an 01 record exists for the FD/SD, use its capacity as the
        // default_record capacity.
        if( p != symbols_end() ) {
          auto record = cbl_field_of(p);
          assert(record->level == 1);
          e = calculate_capacity(p);
          auto record_size = std::max(record->data.memsize,
                                      record->data.capacity);
          group->data.capacity = std::max(group->data.capacity, record_size);
        }
      }

      // SAME AREA AS causes this record area to redefine another.
      // Reach back to that symbol to set its capacity, if need be.
      auto area = symbol_redefines(group);
      if( area ) {
        area->data.capacity = std::max(area->data.capacity,
                                      group->data.capacity);
      }

      return e; // no 01, return self
    }

    cbl_field_t *redefined = symbol_redefines(group);

    if( redefined ) {
      redefined->data.memsize = std::max(field_memsize(redefined), field_size(group));
      if( redefined->data.memsize == redefined->data.capacity ) {
        redefined->data.memsize = 0;
      }
    }
    return e;
  }

  if(yydebug && group->type != FldGroup) {
    dbgmsg("Field #%zu '%s' is not a group", symbol_index(e), group->name);
    symbols_dump(symbols.first_program, true);
  }
  if( group->type == FldInvalid ) return e;

  assert(group->type == FldGroup);

  group->data.capacity = 0;

  std::list<cbl_field_t*> members;

  while( ++e < symbols_end() ) {
    if( e->type != SymField ) continue;
    cbl_field_t *field = cbl_field_of(e);

    if( field_skippable(field) ) continue;

    // Stop if field isn't a member of the group.
    if( end_of_group(group, field) ) break;

    if( field->type == FldGroup ) {
      e = calculate_capacity(e);
      e--; // set e to last symbol processed (not next one, because ++e)
    }

    members.push_back(field);
  }

  // Print accumulating details for one group to debug log.
  bool details = false;

  // At end of group, members is a list of all immediate children, any
  // of which might have been redefined and so acquired a memsize.
  // Any element of members that redefines something redefines group.
  uint32_t max_memsize = 0;
  for( auto field : members ) {
    cbl_field_t *redefined = symbol_redefines(field);
    if( redefined ) {
      if( group != redefined ) {
        grow_redefined_group(redefined, field);
      }
      max_memsize = std::max(max_memsize, field_memsize(field));

      field->data.memsize = 0;

      if( redefined->data.memsize == redefined->data.capacity ) {
        redefined->data.memsize = 0;
      }
      continue;
    }
    group->data.capacity += field_size(field);
    group->data.memsize += field_memsize(field);

    // If group has a parent that is a record area, expand it, too.
    if( 0 < group->parent ) {
      auto redefined = symbol_redefines(group);
      if( redefined && is_record_area(redefined) ) {
        if( redefined->data.capacity < group->data.memsize ) {
          redefined->data.capacity = group->data.memsize;
        }
      }
    }

    if( details ) {
      dbgmsg("%s:%d: %s", __func__, __LINE__, field_str(field) );
      dbgmsg("%s:%d: %s", __func__, __LINE__, field_str(group) );
    }
  }

  group->data.memsize = std::max(max_memsize, group->data.memsize);
  if( group->data.memsize == group->data.capacity ) group->data.memsize = 0;

  if( 0 < group->data.memsize && group->data.memsize < group->data.capacity ) {
    if( yydebug ) {
      dbgmsg( "%s:%d: small capacity?\n\t%s", __func__, __LINE__, field_str(group) );
    }
    group->data.memsize = group->data.capacity;
  }

  if( group->data.capacity == 0 ) {
    dbgmsg( "%s:%d: zero capacity?\n\t%s", __func__, __LINE__, field_str(group) );
  }

  switch( group->level ) {
  case 1: case 77:
    if( dialect_mf() && is_table(group) ) {
      size_t elem_size = std::max(group->data.memsize, group->data.memsize);
      group->data.memsize = elem_size * group->occurs.ntimes();
    }
  }
  return e;
}

static void
verify_block( const struct symbol_elem_t *block,
              const struct symbol_elem_t *eoblock )
{
  for( const struct symbol_elem_t *e=block; e < eoblock; e++ ) {
    if( e->type != SymField ) {
      continue;
    }
  }
}

static symbol_type_t
parent_type( const cbl_field_t *f ) {
  return f->parent == 0? (symbol_type_t)-1 : symbol_at(f->parent)->type;
}

cbl_field_t *
parent_of( const cbl_field_t *f ) {
  return SymField == parent_type(f) ? cbl_field_of(symbol_at(f->parent)) : NULL;
}

const cbl_field_t *
occurs_in( const cbl_field_t *f ) {
  while( (f = parent_of(f)) != NULL ) {
    if( f->occurs.ntimes() > 0 ) break;
  }
  return f;
}

bool
immediately_follows( const cbl_field_t *field ) {
  auto esym = symbols_end();
  auto e = std::find_if( symbol_at(field_index(field)) + 1, esym,
                         []( auto& e ) {
                           if( e.type != SymField ) return false;
                           auto f = cbl_field_of(&e);
                           return f->level == 1;
                         } );
  return e == esym;
}

bool
is_variable_length( const cbl_field_t *field ) {
  bool odo = false;
  std::find_if( symbol_at(field_index(field)) + 1, symbols_end(),
                [&odo, field]( const auto& elem ) {
                  if( elem.type == SymField ) {
                    auto f = cbl_field_of(&elem);
                    if( f->level <= field->level ) return true;
                    if( f->occurs.depending_on ) {
                      odo = true;
                      return true;
                    }
                  }
                  return false;
                } );
  return odo;
}

/*
 * "None of the items within the range, including data-name-2 and
 *  data-name-3, if specified, shall be of class object, message-tag,
 *  or pointer, a strongly-typed group item, an item subordinate to a
 *  strongly- typed group item, a variable-length data item, or an
 *  occurs-depending table."
*/
cbl_field_t *
rename_not_ok( cbl_field_t *first, cbl_field_t *last) {
  symbol_elem_t
    *beg = symbol_at(field_index(first)),
    *end = symbol_at(field_index(last));
  auto e = std::find_if( beg, ++end,
                         []( auto& e ) {
                           if( e.type != SymField ) return false;
                           auto f = cbl_field_of(&e);
                           switch( f->type ) {
                           case FldPointer:
                             return true;
                           default:
                             break;
                           }
                           if( f->occurs.depending_on ) return true;
                           return false;
                        } );
  return e == end? NULL : cbl_field_of(e);
}

cbl_file_t *
symbol_record_file( const cbl_field_t *f ) {
  do {
    if( is_record_area(f) ) return cbl_file_of(symbol_at(f->parent));
    if( f->file )           return cbl_file_of(symbol_at(f->file));
  } while( (f = parent_of(f)) != NULL );
  return NULL;
}

size_t
dimensions( const cbl_field_t *f ) {
  size_t n = is_table(f)? 1 : 0;

  if( f->type == FldIndex ) return 0;

  while( (f = parent_of(f)) != NULL ) {
    if( is_table(f) ) n++;
  }

  return n;
}

const char *
cbl_figconst_str( cbl_figconst_t fig ) {
  switch(fig) {
  case normal_value_e: return "NORMAL CONSTANT";
  case low_value_e:    return "LOW-VALUES";
  case zero_value_e:   return "ZEROS";
  case space_value_e:  return "SPACES";
  case quote_value_e:  return "QUOTES";
  case null_value_e:   return "NULLS";
  case high_value_e:   return "HIGH-VALUES";
  }
  return "NOT FIGURATIVE CONSTANT";
}

static const char *
value_or_figconst_name( const char *value ) {
  auto fig = cbl_figconst_of(value);
  return normal_value_e == fig? value : cbl_figconst_str(fig);
}

const char *
cbl_field_t::attr_str( const std::vector<cbl_field_attr_t>& attrs ) const
{
  const char *sep = "";
  char *out = NULL;

  for( auto attr : attrs ) {
    char *part = out;
    if( has_attr(attr) ) {
      int erc = asprintf(&out, "%s%s%s",
                         part? part : "", sep, cbl_field_attr_str(attr));
      if( -1 == erc ) return part;
      free(part);
      sep = ", ";
    }
  }
  return out? out : "none";
}

char *
field_str( const cbl_field_t *field ) {
  static char string[3*sizeof(cbl_name_t)];
  char *pend = string;

  char name[2*sizeof(cbl_name_t)] = "";
  if( true ) {
    if( field->occurs.ntimes() == 0 ) {
      snprintf(name, sizeof(name), "%s", field->name);
    } else {
      std::vector <char> updown(1 + field->occurs.nkey, '\0');
      for( size_t i=0; i < field->occurs.nkey; i++ ) {
        updown[i] = field->occurs.keys[i].ascending? 'A' : 'D';
      }
      snprintf(name, sizeof(name), "%s[%zu]%s",
               field->name, field->occurs.ntimes(), updown.data());
    }
  }

  pend += snprintf(pend, string + sizeof(string) - pend,
                   "%02d %-20s ", field->level, name);

  char offset[32] = "";
  if( field->level > 1 ) {
    sprintf( offset, "off%3zu", field->offset );
  }

  char parredef =
    parent_of(field) != NULL && parent_of(field)->level == field->level? 'r' : 'P';
  if( 'r' == parredef && field->level == 0 ) parredef = 'p';
  if( field->has_attr(typedef_e) ) parredef = 'T';

  const char *data = field->data.initial? field->data.initial : NULL;
  if( data ) {
    auto fig = cbl_figconst_of(data);
    if( normal_value_e != fig ) {
      data = cbl_figconst_str(fig);
    } else {
      char *s;
      auto n = asprintf(&s, "'%s'", data);
      gcc_assert(n);
      auto eodata = data + field->data.capacity;
      if( eodata != std::find_if_not(data, eodata, fisprint) ) {
        char *p = reinterpret_cast<char*>(xrealloc(s, n + 8 + 2 * field->data.capacity));
        if( is_elementary(field->type) &&
                          field->type != FldPointer && p != NULL ) {
          s = p;
          p += n;
          strcat( p, "(0x" );
          p += 3;
          for( auto d=data; d < eodata; d++ ) {
            p += sprintf(p, "%02x", *d);
          }
          strcat( p++, ")" );
        }
      }
      data = s;
    }
  } else {
    data = "NULL";
    if( field->type == FldSwitch ) {
      data = xasprintf("0x%02x", field->data.upsi_mask_of()->value);
    }
  }
  if( field->level == 88 ) {
    const auto& dom = *field->data.domain_of();
    data = xasprintf("%s%s %s - %s%s",
                     dom.first.all? "A" : "",
                     value_or_figconst_name(dom.first.name()) ,
                     dom.first.is_numeric? "(num)" : "",
                     dom.last.all? "A" : "",
                     dom.last.name()? value_or_figconst_name(dom.last.name()) : "");
  }

  char storage_type = 0x20;
  assert( (field->attr & (linkage_e | local_e)) < (linkage_e | local_e) );
  if( field->attr & linkage_e ) storage_type = 'L';
  if( field->attr & local_e )   storage_type = 'w'; // because 'l' hard to read

  static const std::vector<cbl_field_attr_t> attrs {
    figconst_1_e, figconst_2_e, figconst_4_e, rjust_e, ljust_e,
    zeros_e, signable_e, constant_e, function_e, quoted_e, filler_e,
    intermediate_e, embiggened_e, all_alpha_e, all_x_e,
    all_ax_e, prog_ptr_e, scaled_e, refmod_e, based_e, any_length_e,
    /* global_e, external_e, */ blank_zero_e, /* linkage_e, local_e, */ leading_e,
    separate_e, envar_e, dnu_1_e, bool_encoded_e, hex_encoded_e,
    depends_on_e, /* initialized_e, */ has_value_e, ieeedec_e, big_endian_e,
    same_as_e, record_key_e, typedef_e, strongdef_e,
  };

  pend += snprintf(pend, string + sizeof(string) - pend,
                   "%c%3zu %-6s %c%c%c %2u{%3u,%u,%d = %s} (%s), line %d",
                   parredef, field->parent, offset,
                   (field->attr & global_e)? 'G' : 0x20,
                   (field->attr & external_e)? 'E' : 0x20,
                   storage_type,
                   field->data.memsize,
                   field->data.capacity, field->data.digits, field->data.rdigits,
                   data, field->attr_str(attrs), field->line );
  return string;
}

void
labels_dump() {
  symbols_dump( symbols.procedures, true );
}

struct capacity_of {
  uint32_t capacity;

  capacity_of() : capacity(0) {}

  capacity_of operator()( symbol_elem_t& elem ) {
    if( elem.type == SymField ) {
      cbl_field_t *f = cbl_field_of(&elem);
      if( is_elementary(f->type) ) {
        capacity += field_size(f);
      }
    }
    return *this;
  }
};

static void
extend_66_capacity( cbl_field_t *alias ) {
  static_assert(sizeof(symbol_elem_t*) == sizeof(const char *),
                "all pointers must be same size");
  assert(alias->data.picture);
  assert(alias->type == FldGroup);
  symbol_elem_t *e = symbol_at(alias->parent);
  symbol_elem_t *e2 =
    reinterpret_cast<symbol_elem_t*>(const_cast<char*>(alias->data.picture));
  assert(e < e2);
  alias->data.picture = NULL;

  capacity_of cap;
  if( alias->type == FldGroup ) {
    e2 = symbol_at_impl(end_of_group(symbol_index(e2)));
  } else {
    ++e2;
  }
  alias->data.capacity = std::for_each(e, e2, cap).capacity;
  assert(alias->data.capacity > 0);
}

bool
symbols_alphabet_set( size_t program, const char name[]) {
  struct alpha {
    void operator()( symbol_elem_t& elem ) const {
      if( elem.type == SymAlphabet ) {
        parser_alphabet( *cbl_alphabet_of(&elem) );
      }
    }
  };

  // Define alphabets for codegen.
  std::for_each(symbols_begin(), symbols_end(), alpha() );

  // Set collation sequence before parser_symbol_add.
  if( name ) {
    symbol_elem_t *e = symbol_alphabet(program, name);
    if( !e ) {
      return false;
    }
    parser_alphabet_use(*cbl_alphabet_of(e));
  }
  return true;
}

static std::ostream&
operator<<( std::ostream& os, const cbl_occurs_bounds_t& bound ) {
  return os << bound.lower << ',' << bound.upper;
}

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-function"
// Keep this debugging function around for when it is needed
static std::ostream&
operator<<( std::ostream& os, const cbl_field_data_t& field ) {
  return os << field.memsize << ','
            << field.capacity << ','
            << field.digits << ','
            << field.rdigits << ','
            << (field.picture? field.picture : "");
}

static std::ostream&
operator<<( std::ostream& os, const cbl_field_t& field ) {
  return os <<        field.parent
            << ',' << field.level
            << ',' << field.name
            << ',' << field.offset
            << ',' << cbl_field_type_str(field.type)
            << ',' << "0x" << std::hex << field.attr << std::dec
    // occurs
            << ',' << field.occurs.depending_on
            << ',' << field.occurs.bounds
            << ',' << field.line
            << ',' << field.data;
}
#pragma GCC diagnostic pop

static std::map<size_t, std::set<size_t>> same_record_areas;
size_t parse_error_count();

/*
 * This function produces a zero-filled level number, so 1 becomes "01".  It's
 * needed because the diagnostic format string doesn't support zero-filled
 * integer conversion or width.
 */
const char *
cbl_field_t::level_str( uint32_t level ) {
  char *str = xasprintf( "%02u", level );
  return str;
}

size_t
symbols_update( size_t first, bool parsed_ok ) {
  struct symbol_elem_t *p, *pend;
  std::list<cbl_field_t*> shared_record_areas;

  for( p = symbols_begin(first); p < symbols_end(); p++ ) {

    if( p->type == SymAlphabet ) continue; // Alphabets already processed.
    if( p->type == SymFile ) continue;     // Do fields before files.
    if( p->type != SymField ) continue;

    cbl_field_t *field = cbl_field_of(p);
    if( field->our_index == 0 ) field->our_index = symbol_index(p);
    if( field->type == FldForward ) continue;
    if( field->type == FldSwitch ) continue;
    if( is_literal(field) && field->var_decl_node != NULL ) continue;

    switch(field->level) {
    case 0:
      if( field->is_key_name() ) {
        update_symbol_map2(p);
        continue;
      }
      break;
    case 1:
      pend = calculate_capacity(p);
      if( dialect_mf() && is_table(field) ) {
        cbl_field_t *field = cbl_field_of(p);
        if( field->data.memsize < field->size() ) {
          field->data.memsize = field->size();
        }
      }
      update_block_offsets(p);
      verify_block(p, pend);
      break;
    case 66:
      assert(field->parent > 0);
      assert(symbol_at(field->parent)->type == SymField);
      if( field->type == FldGroup && field->data.picture ) {
        extend_66_capacity(field);
      } else {
        auto data = parent_of(field)->data;
        data.memsize = 0;
        field->data = data;
      }
      break;
      // no special processing for other levels
    }

    // Update ODO field in situ.
    if( is_table(field) ) {
      size_t& odo = field->occurs.depending_on;
      if( odo != 0 ) {
        auto odo_field = cbl_field_of(symbol_at(odo)); // get not-FldForward if exists
        if( is_forward(odo_field) ) {
          ERROR_FIELD(field, "table %s (line %d) DEPENDS ON %s, which is not defined",
                   field->name, field->line, odo_field->name);
        } else {
          // set odo to found field
          odo = field_index(odo_field);
        }
      }
    }

    bool size_invalid = field->data.memsize > 0 && symbol_redefines(field);
    if( size_invalid ) { // redefine of record area is ok
      auto redefined = symbol_redefines(field);
      size_invalid = ! is_record_area(redefined);
    }
    if( !field->is_valid() || size_invalid )
    {
      size_t isym = p - symbols_begin();
      symbols_dump(symbols.first_program, true);
      if( symbol_at(field->parent)->type == SymFile ) {
        assert(field->parent == field_index(field) + 1);
        auto e = std::find_if( symbols_begin(field->parent), symbols_end(),
                               [program = p->program, ifile = field->parent]
                               ( const auto& elem ) {
                                 if( elem.program == program ) {
                                   if( elem.type == SymField ) {
                                     auto f = cbl_field_of(&elem);
                                     return f->parent == ifile;
                                   }
                                 }
                                 return false;
                               } );
        if( e == symbols_end() ) {
          // no field redefines the file's default record
          auto file = cbl_file_of(symbol_at(field->parent));
          ERROR_FIELD(field, "line %d: %s lacks a file description",
                      file->line, file->name);
          return 0;
        }
      }
      // Better to report an error than to fail mysteriously with "0 errors".
      if( yydebug || parse_error_count() == 0 ) {
        if( field->type == FldInvalid ) {
          ERROR_FIELD(field, "line %d: %s %s requires PICTURE",
                  field->line, field->level_str(), field->name);

        } else {
          dbgmsg("%s: error: data item %s #%zu '%s' capacity %u rejected",
                   __func__,
                   3 + cbl_field_type_str(field->type),
                   isym, field->name, field->data.capacity);
        }
      }
      return 0;
    }

    if(! (field->data.memsize == 0 || field_size(field) <= field->data.memsize) ) {
      dbgmsg( "%s:%d: #%zu: invalid: %s", __func__, __LINE__,
                           symbol_index(p), field_str(cbl_field_of(p)) );
    }
    assert(field->data.memsize == 0 || field_size(field) <= field_memsize(field));
    assert( !(field->data.memsize > 0 && symbol_explicitly_redefines(field)) );
  }

  // A shared record area has no 01 child because that child redefines its parent.
  for( auto sharer : shared_record_areas ) {
    auto redefined = cbl_field_of(symbol_at(sharer->parent));
    sharer->data.capacity = redefined->data.capacity;
  }

  for( p = symbols_begin(first); p < symbols_end(); p++ ) {
    if( p->type != SymField ) continue;
    cbl_field_t *field = cbl_field_of(p);
    if( field->type == FldForward ) continue;
    if( field->type == FldSwitch ) continue;
    if( field->level == 0 && field->is_key_name() ) continue;
    if( is_literal(field) && field->var_decl_node != NULL ) continue;

    if( field->is_typedef() ) {
      auto isym = end_of_group( symbol_index(p) );
      p = symbol_at(--isym);
      continue;
    }

    // Verify REDEFINing field has no ODO components
    auto parent = symbol_redefines(field);
    if( parent && !is_record_area(parent) && is_variable_length(field) ) {
      ERROR_FIELD(field, "line %d: REDEFINES field %s cannot be variable length",
               field->line, field->name);
      return 0;
    }

    if( field->type == FldInvalid ) {
      dbgmsg("%s:%d: %s", __func__, __LINE__, field_str(field));
      ERROR_FIELD(field, "line %d: %s %s requires PICTURE",
              field->line, field->level_str(), field->name);
      continue;
    }

    assert( ! field->is_typedef() );

    if( parsed_ok ) parser_symbol_add(field);
  }

  finalize_symbol_map2();
  if( yydebug ) dump_symbol_map2();

  build_symbol_map();

  int ninvalid = 0;
  for( p = symbols_begin(first); p < symbols_end(); p++ ) {
    if( p->type == SymFile ) { // now do the files
      auto& file = *cbl_file_of(p);
      if( !file.varying_size.explicitly ) {
        auto sizes = symbol_file_record_sizes( &file );
        file.varying_size = sizes;
      }
      file.deforward();
      if( ! file.validate() ) {
        ninvalid++;
        continue;
      }
    if( parsed_ok ) parser_file_add(&file);
    }
  }

  symbols_dump(symbols.first_program, true);

  symbols.procedures = p - symbols_begin();

  return ninvalid > 0? 0 : symbols.procedures;
}

size_t
symbol_index() {
  assert( symbols.first_program <= symbols.nelem );
  return symbols.nelem - symbols.first_program;
}

size_t
symbol_index( const struct symbol_elem_t *e ) {
  assert(e);
  size_t isym = symbols.index(e);
  assert( isym < symbols.nelem );
  return isym;
}

// Match on name (implied: of forward declaration).
static int
defined_fwd_cmp( const void *K, const void *E ) {
  const struct symbol_elem_t
    *k=static_cast<const struct symbol_elem_t *>(K),
    *e=static_cast<const struct symbol_elem_t *>(E);

  if( k->type != SymField ) {
    cbl_errx( "%s: key must be field", __func__);
  }
  if( k->type != e->type ) return 1;
  if( k->program != e->program ) return 1;

  // Matches if names match, and both are fields in the same program.
  // A forward declaration doesn't have parent because only its name is mentioned.
  return strcasecmp(cbl_field_of(k)->name, cbl_field_of(e)->name);
}

/*
 * Given a symbol index that may be forward reference, return the
 * "resolved" field, if extant, else the forward field.  Forward
 * references remain in the symbol table and their index may appear in,
 * for example, cbl_file_t symbols.
 */
struct cbl_field_t *
symbol_field_forward( size_t index ) {
  assert( index < symbols.nelem );
  symbol_elem_t *e = symbol_at(index);
  if( (e->type != SymField) ) {
    dbgmsg("%s: logic error: #%zu is %s", __func__, index, symbol_type_str(e->type));
  }
  assert(e->type == SymField);

  if( cbl_field_of(e)->type == FldForward ) {

    symbol_elem_t *start = symbols_begin(++index);
    size_t nelem = symbols_end() - start;

    struct symbol_elem_t *kid =
      static_cast<struct symbol_elem_t *>(lfind( e, start,
                                                 &nelem, sizeof(*e),
                                                 defined_fwd_cmp ) );
    if( kid ) {
      return cbl_field_of(kid);
    }
  }
  return cbl_field_of(e);
}

struct symbol_elem_t *
symbol_parent( const struct symbol_elem_t *e ) {
  assert(e);
  assert(e->type == SymField);
  assert(cbl_field_of(e)->type != FldInvalid);

  if( cbl_field_of(e)->parent == 0 ) {
    return NULL;
  }

  symbol_elem_t *p = symbols.elems + cbl_field_of(e)->parent;

  assert( symbols.elems < p && p < symbols.elems + symbols.nelem );

  return p;
}

static bool
had_picture( const cbl_field_t *field ) {
  if( is_elementary(field->type) ) {
    switch(field->type) {
    case FldAlphanumeric:
      // VALUE string for alphanumeric might mean no PICTURE.
      return field->data.initial == NULL;
    case FldNumericDisplay:
    case FldNumericEdited:
    case FldAlphaEdited:
      return true;
    case FldPointer:
    case FldPacked:
    case FldNumericBinary:
    case FldNumericBin5:
    case FldFloat:
      break;
    default:
      break;
    }
  }
  return false;
}

void
name_queue_t::dump( const char tag[] ) const {
  if( ! (yydebug ) ) return;
    int i=0;
    for( const auto& namelocs : this->c ) {
      static char line[256];
      char *p = line;
      const char *sep = "";
      for( auto nameloc : namelocs ) {
        p += snprintf( p, line + sizeof(line) - p, "%s%s", sep, nameloc.name );
        sep = "::";
      }
      dbgmsg("name_queue: %s: %2d: %s", tag, ++i, line);
    }
    if( empty() ) {
      dbgmsg("name_queue: %s: is empty", tag);
    }
  }

#if 0
/*
 * When adding a symbol, set the parent as an offset into the symbol table.
 */
static symbol_elem_t *
symbol_in_file( symbol_elem_t *e ) {

  auto beg = std::reverse_iterator<symbol_elem_t *>(e);
  auto end = std::reverse_iterator<symbol_elem_t *>(symbols_begin());
  auto p = std::find_if( beg, end,
                         []( const symbol_elem_t& elem ) {
                           return elem.type == SymFilename;
                         } );

  return p != end? &*p : NULL;
}
#endif

static struct cbl_field_t *
symbol_field_parent_set( struct cbl_field_t *field )
{
  if( field->level == 01 ) return NULL;
  if( field->level == 77 ) return NULL;
  if( field->level == 78 ) return NULL;

  struct symbol_elem_t *e = symbols.elems + symbols.nelem - 1;
  struct symbol_elem_t *first = symbols.elems + symbols.first_program;

  for( ; field->parent == 0 && e >= first; e-- ) {
    if( ! (e->type == SymField && cbl_field_of(e)->level > 0) ) {
      continue; // level 0 fields are not user-declared symbols
    }

    cbl_field_t *prior = cbl_field_of(e);

    if( prior->level == 77 || prior->level == 78 ) {
      switch(field->level) {
      case 66: case 88:
        break;
      default:
        return NULL; // 77/78 cannot be a parent
      }
    }

    if( prior->level == field->level ) {
      auto redefined = symbol_redefines(prior);
      if( redefined ) prior = redefined;
      field->parent = prior->parent;
      return cbl_field_of(symbol_at(field->parent));
    }

    if( prior->level < field->level ) {
      if( prior->has_attr(same_as_e) ) {
        ERROR_FIELD(prior, "%s created with SAME AS or TYPE TO, cannot have new member %s",
                 prior->name, field->name);
        return NULL;
      }
      field->parent = e - symbols.elems;
      if( 1 < field->level && field->level < 50 ) {
        if( had_picture(prior) ) {
          ERROR_FIELD(prior, "group %s cannot have PICTURE clause", prior->name);
          return NULL;
        }
        prior->type = FldGroup;
        field->attr |= numeric_group_attrs(prior);
      }
      // verify level 88 domain value
      if( is_numeric(prior) && field->level == 88 ) {
        // domain array terminated by an element with a NULL name (value)
        auto edom = field->data.domain_of();
        while( edom->first.name() ) edom++;

        bool all_numeric =
          std::all_of( field->data.domain_of(), edom,
                       []( const cbl_domain_t& domain ) {
                         switch( cbl_figconst_of(domain.first.name()) ) {
                         case normal_value_e:
                           // parser ensures first.is_numeric == last.is_numeric
                           return domain.first.is_numeric &&
                                  domain.last.is_numeric;
                         case zero_value_e:
                           return true;
                         default:
                           break;
                         }
                         return false;
                       } );
        if( ! all_numeric ) {
          auto loc = symbol_field_location(0);
          error_msg(loc, "%s %s invalid VALUE for numeric type %s",
                    field->level_str(), field->name, prior->name);
        }
      }
      return prior;
    }
  }
  return NULL;
}

class parent_elem_set
{
private:
  size_t parent_index;
public:
  parent_elem_set( size_t parent_index )
    : parent_index(parent_index)
  {}
  void operator()( struct symbol_elem_t& e ) {
    // cannot use cbl_field_of, because symbols.elems not yet ready
    assert(e.type == SymField);
    e.elem.field.parent = this->parent_index;
  }
};

static symbol_elem_t
add_token( symbol_elem_t sym ) {
  assert(sym.type == SymSpecial);
  sym.elem.special.token = keyword_tok(sym.elem.special.name);
  return sym;
}

/*
 * When adding registers, be sure to add a complementary cblc_field_t
 * in libgcobol/constants.cc.
 */
void
symbol_table_init(void) {
  assert(symbols.fd == -1);
  assert(symbols.nelem == 0);

  symbol_table_t table = symbol_table_extend();

  // Insert known contants at the top of an empty table.
  // Constants are signified by their attribute
  // Be warned that ZEROS plays for both sides.  It is defined here as
  // quoted, but in context it can be the value zero at run-time.  Yes, it
  // is an annoyance.
  static char zeroes_for_null_pointer[8] = {0,0,0,0,0,0,0,0};

  // These should match the definitions in libgcobol/constants.cc
  static cbl_field_t constants[] = {
    { 0, FldAlphanumeric, FldInvalid, space_value_e | constq, 0, 0, 0, nonarray, 0,
      "SPACE", 0, {}, {1,1,0,0, " \0\xFF"}, NULL },
    { 0, FldAlphanumeric, FldInvalid, space_value_e | constq , 0, 0, 0, nonarray, 0,
      "SPACES", 0, {}, {1,1,0,0, " \0\xFF"}, NULL },
    { 0, FldAlphanumeric, FldInvalid, low_value_e | constq, 0, 0, 0, nonarray, 0,
      "LOW_VALUES", 0, {}, {1,1,0,0, "L\0\xFF"}, NULL },
    { 0, FldAlphanumeric, FldInvalid, zero_value_e | constq, 0, 0, 0, nonarray, 0,
      "ZEROS", 0, {}, {1,1,0,0, "0"}, NULL },
    { 0, FldAlphanumeric, FldInvalid, high_value_e | constq, 0, 0, 0, nonarray, 0,
      "HIGH_VALUES", 0, {}, {1,1,0,0, "H\0\xFF"}, NULL },
    // IBM standard: QUOTE is a double-quote unless APOST compiler option
    { 0, FldAlphanumeric, FldInvalid, quote_value_e | constq , 0, 0, 0, nonarray, 0,
      "QUOTES", 0, {}, {1,1,0,0, "\"\0\xFF"}, NULL },
    { 0, FldPointer, FldPointer, constq , 0, 0, 0, nonarray, 0,
      "NULLS", 0, {}, {8,8,0,0, zeroes_for_null_pointer}, NULL },
    // IBM defines TALLY
    // 01  TALLY GLOBAL PICTURE 9(5) USAGE BINARY VALUE ZERO.
    { 0, FldNumericBin5, FldInvalid, signable_e, 0, 0, 0, nonarray, 0,
      "_TALLY", 0, {}, {16, 16, MAX_FIXED_POINT_DIGITS, 0, NULL}, NULL },
    // 01  ARGI is the current index into the argv array
    { 0, FldNumericBin5, FldInvalid, signable_e, 0, 0, 0, nonarray, 0,
      "_ARGI", 0, {}, {16, 16, MAX_FIXED_POINT_DIGITS, 0, NULL}, NULL },

    // These last two don't require actual storage; they get BOOL var_decl_node
    // in parser_symbol_add()
    { 0, FldConditional, FldInvalid, constant_e , 0, 0, 0, nonarray, 0,
      "_VERY_TRUE", 0, {}, {1,1,0,0, ""}, NULL },
    { 0, FldConditional, FldInvalid, constant_e , 0, 0, 0, nonarray, 0,
      "_VERY_FALSE", 0, {}, {1,1,0,0, ""}, NULL },
  };
  for( struct cbl_field_t *f = constants;
       f < constants + COUNT_OF(constants); f++ ) {
    f->our_index = table.nelem;
    struct symbol_elem_t sym(SymField, 0);
    sym.elem.field = *f;
    table.elems[table.nelem++] = sym;
  }

  static symbol_elem_t environs[] = {
    { symbol_elem_t{ 0, cbl_special_name_t{0, SYSIN_e, "SYSIN", 0, "/dev/stdin"}} },
    { symbol_elem_t{ 0, cbl_special_name_t{0, SYSIPT_e, "SYSIPT", 0, "/dev/stdout"}} },
    { symbol_elem_t{ 0, cbl_special_name_t{0, SYSOUT_e, "SYSOUT", 0, "/dev/stdout"}} },
    { symbol_elem_t{ 0, cbl_special_name_t{0, SYSLIST_e, "SYSLIST", 0, "/dev/stdout"}} },
    { symbol_elem_t{ 0, cbl_special_name_t{0, SYSLST_e, "SYSLST", 0, "/dev/stdout"}} },
    { symbol_elem_t{ 0, cbl_special_name_t{0, SYSPUNCH_e, "SYSPUNCH", 0, "/dev/stderr"}} },
    { symbol_elem_t{ 0, cbl_special_name_t{0, SYSPCH_e, "SYSPCH", 0, "/dev/stderr"}} },
    { symbol_elem_t{ 0, cbl_special_name_t{0, CONSOLE_e, "CONSOLE", 0, "/dev/stdout"}} },
    { symbol_elem_t{ 0, cbl_special_name_t{0, C01_e, "C01", 0, "/dev/null"}} },
    { symbol_elem_t{ 0, cbl_special_name_t{0, C02_e, "C02", 0, "/dev/null"}} },
    { symbol_elem_t{ 0, cbl_special_name_t{0, C03_e, "C03", 0, "/dev/null"}} },
    { symbol_elem_t{ 0, cbl_special_name_t{0, C04_e, "C04", 0, "/dev/null"}} },
    { symbol_elem_t{ 0, cbl_special_name_t{0, C05_e, "C05", 0, "/dev/null"}} },
    { symbol_elem_t{ 0, cbl_special_name_t{0, C06_e, "C06", 0, "/dev/null"}} },
    { symbol_elem_t{ 0, cbl_special_name_t{0, C07_e, "C07", 0, "/dev/null"}} },
    { symbol_elem_t{ 0, cbl_special_name_t{0, C08_e, "C08", 0, "/dev/null"}} },
    { symbol_elem_t{ 0, cbl_special_name_t{0, C09_e, "C09", 0, "/dev/null"}} },
    { symbol_elem_t{ 0, cbl_special_name_t{0, C10_e, "C10", 0, "/dev/null"}} },
    { symbol_elem_t{ 0, cbl_special_name_t{0, C11_e, "C11", 0, "/dev/null"}} },
    { symbol_elem_t{ 0, cbl_special_name_t{0, C12_e, "C12", 0, "/dev/null"}} },
    { symbol_elem_t{ 0, cbl_special_name_t{0, CSP_e, "CSP", 0, "/dev/null"}} },
    { symbol_elem_t{ 0, cbl_special_name_t{0, S01_e, "S01", 0, "/dev/null"}} },
    { symbol_elem_t{ 0, cbl_special_name_t{0, S02_e, "S02", 0, "/dev/null"}} },
    { symbol_elem_t{ 0, cbl_special_name_t{0, S03_e, "S03", 0, "/dev/null"}} },
    { symbol_elem_t{ 0, cbl_special_name_t{0, S04_e, "S04", 0, "/dev/null"}} },
    { symbol_elem_t{ 0, cbl_special_name_t{0, S05_e, "S05", 0, "/dev/null"}} },
    { symbol_elem_t{ 0, cbl_special_name_t{0, AFP_5A_e, "AFP-5A", 0, "/dev/null"}} },
    { symbol_elem_t{ 0, cbl_special_name_t{0, STDIN_e, "STDIN", 0, "/dev/stdin"}} },
    { symbol_elem_t{ 0, cbl_special_name_t{0, STDOUT_e, "STDOUT", 0, "/dev/stdout"}} },
    { symbol_elem_t{ 0, cbl_special_name_t{0, STDERR_e, "STDERR", 0, "/dev/stderr"}} },
    { symbol_elem_t{ 0, cbl_special_name_t{0, SYSERR_e, "SYSERR", 0, "/dev/stderr"}} },
  };

  struct symbol_elem_t *p = table.elems + table.nelem;
  std::transform(environs, environs + COUNT_OF(environs), p, add_token);

  table.nelem += COUNT_OF(environs);

  assert(table.nelem < table.capacity);

  /**
   * Debug register record
     01 DEBUG-ITEM.
        02 DEBUG-LINE PIC X(6).
        02 FILLER PIC X VALUE SPACE.
        02 DEBUG-NAME PIC X(30).
        02 FILLER PIC X VALUE SPACE.
        02 DEBUG-SUB-1 PIC S9999 SIGN IS LEADING SEPARATE CHARACTER.
        02 FILLER PIC X VALUE SPACE.
        02 DEBUG-SUB-2 PIC S9999 SIGN IS LEADING SEPARATE CHARACTER.
        02 FILLER PIC X VALUE SPACE.
        02 DEBUG-SUB-3 PIC S9999 SIGN IS LEADING SEPARATE CHARACTER.
        02 FILLER PIC X VALUE SPACE.
        02 DEBUG-CONTENTS PIC X(76).
   **/

  static cbl_field_t debug_registers[] = {
    { 0, FldGroup, FldInvalid, global_e, 0,0,1, nonarray, 0,
      "DEBUG-ITEM", 0, {}, {132,132,0,0, NULL}, NULL },
    { 0, FldAlphanumeric, FldInvalid, global_e, 0,0,2, nonarray, 0,
      "DEBUG-LINE", 0, {}, {6,6,0,0, "      "}, NULL },
    { 0, FldAlphanumeric, FldInvalid, 0, 0,0,2, nonarray, 0,
      "FILLER", 0, {}, {1,1,0,0, " "}, NULL },
    { 0, FldAlphanumeric, FldInvalid, global_e, 0,0,2, nonarray, 0,
      "DEBUG-NAME", 0, {}, {30,30,0,0, NULL}, NULL },
    { 0, FldAlphanumeric, FldInvalid, 0, 0,0,2, nonarray, 0,
      "FILLER", 0, {}, {1,1,0,0, " "}, NULL },
    { 0, FldNumericDisplay, FldInvalid, signable_e | global_e | leading_e | separate_e, 0,0,2, nonarray, 0,
      "DEBUG-SUB-1", 0, {}, {5,5,3,0, NULL}, NULL },
    { 0, FldAlphanumeric, FldInvalid, 0, 0,0,2, nonarray, 0,
      "FILLER", 0, {}, {1,1,0,0, " "}, NULL },
    { 0, FldNumericDisplay, FldInvalid, signable_e | global_e | leading_e | separate_e, 0,0,2, nonarray, 0,
      "DEBUG-SUB-2", 0, {}, {5,5,3,0, NULL}, NULL },
    { 0, FldAlphanumeric, FldInvalid, 0, 0,0,2, nonarray, 0,
      "FILLER", 0, {}, {1,1,0,0, " "}, NULL },
    { 0, FldNumericDisplay, FldInvalid, signable_e | global_e | leading_e | separate_e, 0,0,2, nonarray, 0,
      "DEBUG-SUB-3", 0, {}, {5,5,3,0, NULL}, NULL },
    { 0, FldAlphanumeric, FldInvalid, 0, 0,0,2, nonarray, 0,
      "FILLER", 0, {}, {1,1,0,0, " "}, NULL },
    { 0, FldAlphanumeric, FldInvalid, signable_e | global_e, 0,0,2, nonarray, 0,
      "DEBUG-CONTENTS", 0, {}, {76,76,0,0, NULL}, NULL },
};

  // debug registers
  assert(table.nelem + COUNT_OF(debug_registers) < table.capacity);

  group_size_t group_size =
    std::accumulate(debug_registers,
                    debug_registers + COUNT_OF(debug_registers), group_size_t());
  debug_registers[0].data.memsize =
  debug_registers[0].data.capacity = group_size.capacity();

  auto debug_start = p = table.elems + table.nelem;
  p = std::transform(debug_registers,
                     debug_registers + COUNT_OF(debug_registers), p, elementize);
  table.nelem = p - table.elems;
  assert(table.nelem < table.capacity);
  std::for_each(debug_start+1, p, parent_elem_set(debug_start - table.elems));

  static cbl_field_t special_registers[] = {
    { 0, FldNumericDisplay, FldInvalid, 0, 0, 0, 0, nonarray, 0, "_FILE_STATUS",
      0, {}, {2,2,2,0, NULL}, NULL },
    { 0, FldNumericBin5, FldInvalid, 0, 0, 0, 0, nonarray, 0, "UPSI-0",
      0, {}, {2,2,4,0, NULL}, NULL },
    { 0, FldNumericBin5, FldInvalid, signable_e, 0, 0, 0, nonarray, 0, "RETURN-CODE",
      0, {}, {2,2,4,0, NULL}, NULL },
    { 0, FldNumericBin5, FldInvalid, 0, 0, 0, 0, nonarray, 0, "LINAGE-COUNTER",
      0, {}, {2,2,4,0, NULL}, NULL },
    { 0, FldLiteralA, FldInvalid, 0, 0, 0, 0, nonarray, 0, "_dev_stdin",
      0, {}, {0,0,0,0, "/dev/stdin"}, NULL },
    { 0, FldLiteralA, FldInvalid, constq, 0, 0, 0, nonarray, 0, "_dev_stdout",
      0, {}, {0,0,0,0, "/dev/stdout"}, NULL },
    { 0, FldLiteralA, FldInvalid, constq, 0, 0, 0, nonarray, 0, "_dev_stderr",
      0, {}, {0,0,0,0, "/dev/stderr"}, NULL },
    { 0, FldLiteralA, FldInvalid, constq, 0, 0, 0, nonarray, 0, "_dev_null",
      0, {}, {0,0,0,0, "/dev/null"}, NULL },
  };

  // special registers
  assert(table.nelem + COUNT_OF(special_registers) < table.capacity);

  p = table.elems + table.nelem;
  p = std::transform(special_registers,
                     special_registers + COUNT_OF(special_registers),
                     p, elementize);
  table.nelem = p - table.elems;
  assert(table.nelem < table.capacity);

  // Initialize symbol table.
  symbols = table;

  for( auto e = symbols.elems; e < symbols.elems + symbols.nelem; e++ ) {
    if( e->type == SymField ) {
      update_symbol_map2(e);
    }
  }

  symbols.first_program = symbols.nelem;

  symbols.registers.linage_counter = symbol_index(symbol_field(0,0,
                                                              "LINAGE-COUNTER"));
  symbols.registers.file_status = symbol_index(symbol_field(0,0, "_FILE_STATUS"));
  symbols.registers.return_code = symbol_index(symbol_field(0,0, "RETURN-CODE"));
  symbols.registers.very_true   = symbol_index(symbol_field(0,0, "_VERY_TRUE"));
  symbols.registers.very_false  = symbol_index(symbol_field(0,0, "_VERY_FALSE"));
}

/*
 * Add a symbol to the symbol table.
 */
static struct symbol_elem_t *
symbol_add( struct symbol_elem_t *elem )
{
  assert(symbols.capacity > 0);  // initialized

  if( symbols.nelem == symbols.capacity ) {
    symbol_table_extend();
  };

  assert(symbols.nelem < symbols.capacity);  // not at capacity

  if( elem->type == SymField ) {
    // Place the [soon-to-be] index of this field into the field
    cbl_field_of(elem)->our_index = symbols.nelem;
  }

  struct symbol_elem_t *p =
    static_cast<struct symbol_elem_t *>(lsearch( elem, symbols.elems,
                                                 &symbols.nelem, sizeof(*elem),
                                                 symbol_elem_cmp ) );
  assert(symbols.nelem > 1);

  if( is_program(*p) ) {
    assert(p->program == 0 || p->elem.label.os_name != NULL);
    p->program = p - symbols.elems;
  }

  if( p->program == 0 ) {
    p->program = p[-1].program;
  }

  return p;
}

static symbol_elem_t *
symbol_append( const symbol_elem_t& elem ) {
  if( symbols.nelem == symbols.capacity ) {
    symbol_table_extend();
  };

  auto e = symbols.elems + symbols.nelem++;
  *e = elem;
  return e;
}

cbl_label_t *
cbl_perform_tgt_t::finally( size_t program ) {
  assert(0 < ito);
  static const char fini[] = "_fini";
  cbl_label_t proto = *to();
  auto p = proto.name + strlen(proto.name);
  auto n = snprintf(p, proto.name + sizeof(proto.name) - p, "%s", fini);
  assert(n < int(sizeof(fini)));
  symbol_elem_t sym = {}, *e;
  sym.type = SymLabel;
  sym.program = program;
  sym.elem.label = proto;
  e = symbol_add(&sym);
  ifrom = symbol_index(e);
  return cbl_label_of(e);
}

struct symbol_elem_t *
symbol_file_add( size_t program, cbl_file_t *file ) {
  auto e = std::find_if( symbols_begin(program), symbols_end(),
                         [file]( const auto& elem ) {
                           if( elem.type == SymFile ) {
                             auto f = cbl_file_of(&elem);
                             return 0 == strcasecmp(f->name, file->name);
                           }
                           return false;
                         } );
  if( e != symbols_end() ) { // duplicate SELECT filenames not allowed
    auto f = cbl_file_of(e);
    file->line = f->line; // use called structure to capture prior line
    return NULL;
  }

  struct symbol_elem_t sym = { SymFile, program };
  sym.elem.file = *file;

  e = symbol_add(&sym);

  const auto& f = *cbl_file_of(e);
  if( f.same_record_as > 0 ) { // add to list of files sharing one record area
    same_record_areas[f.same_record_as].insert(symbol_index(e));
  }

  return e;
}

struct symbol_elem_t *
symbol_alphabet_add( size_t program, struct cbl_alphabet_t *alphabet ) {
  struct symbol_elem_t sym{ SymAlphabet, program };
  sym.elem.alphabet = *alphabet;
  return symbol_add(&sym);
}

size_t
numeric_group_attrs( const cbl_field_t *field ) {
  static const size_t inherit = signable_e | leading_e | separate_e | big_endian_e;
  static_assert(sizeof(cbl_field_t::type) < sizeof(inherit), "need bigger type");
  assert(field);
  if( field->type == FldNumericDisplay || field->type == FldGroup ) {
    if( field->parent > 0 && symbol_at(field->parent)->type == SymField ) {
      cbl_field_t *parent = parent_of(field);
      assert(parent);
      return inherit & parent->attr;
    }
  }
  return 0;
}

/*
 * "The essential characteristics of a type, which is identified by
 * its type- name, are the relative positions and lengths of the
 * elementary items defined in the type declaration, and the ALIGNED,
 * BLANK WHEN ZERO, DYNAMIC LENGTH, JUSTIFIED, PICTURE, SIGN,
 * SYNCHRONIZED, and USAGE clauses specified for each of these
 * elementary items"
 */
struct symbol_elem_t *
symbol_typedef_add( size_t program, struct cbl_field_t *field ) {
  assert(field);
  assert(field->is_typedef());

  if( field->is_strongdef() && field->level != 1 ) {
    ERROR_FIELD(field, "%s %s STRONG TYPEDEF must be level 01",
            field->level_str(), field->name);
    return NULL;
  }

  // Might have just been added to the symbol table.
  auto e = symbols_end() - 1;
  assert( symbols_begin() < e );
  if( e->type == SymField ) {
    auto f = cbl_field_of(e);
    if( f == field ) return e;
  }

  symbol_elem_t elem{ program, *field };

  e = symbol_add( &elem );

  return e;
}

typedef std::map <std::string, size_t > namemap_t;
static std::map <size_t, namemap_t > numeric_constants;

/*
 * Add a Cobol variable/literal to the symbol table.
 *
 * Each time the filename changes, a "filename" symbol is added to the
 * symbol table. We find what file a symbol was defined in by
 * searching back from the symbol for a filename entry.
 *
 * Fields may be function pointers too, from dlopen(3).
 *
 * Most symbols are Cobol variables of type cbl_field_t.  Duplicate
 * names are allowed; they just can't be referenced.
 *
 * The passed parameter contains two pointers; the initial value and
 * the picture. Except for inherited types, these pointers are NOT
 * changed.  Make them point where you want them to point.
 *
 * Literals have an initial pointer only; the picture NULL.
 *
 * Returns a pointer to the added symbol, always.
 */
struct symbol_elem_t *
symbol_field_add( size_t program, struct cbl_field_t *field )
{
  field->our_index = symbols.nelem;
  cbl_field_t *parent = symbol_field_parent_set( field );
  if( parent && parent->type == FldGroup) {
    // Inherit effects of parent's USAGE, as though it appeared 1st in the
    // member's definition.
    static const size_t inherit = global_e | external_e | local_e | linkage_e;
    field->attr = inherit & parent->attr;
    field->attr |= numeric_group_attrs(parent);
    field->usage = parent->usage;
    // BINARY-LONG, for example, sets capacity.
    if( is_numeric(parent->usage) && parent->data.capacity > 0 ) {
      field->type = parent->usage;
      field->data = parent->data;
      field->data = 0.0;
      field->data.initial = NULL;
    }
  }

  if( is_forward(field) ) {
    auto *e = symbol_field( program, field->parent, field->name );
    if( e ) {
      field = cbl_field_of(e);
      if( is_constant(field) && field->type == FldNumericBin5 ) {
        cbl_name_t lname;
        std::transform( field->name, field->name + strlen(field->name) + 1,
                       lname, tolower );
        numeric_constants[program][lname] = symbol_index(e);
      }
      return e;
    }
  }

  if( strlen(field->name) == 6 && 0 == strcasecmp("FILLER", field->name) ) {
    field->attr |= filler_e;
  }
  if( field->name[0] == '\0' ) {
    field->attr |= filler_e;
  }

  symbol_elem_t key { program, *field };

  // Literals must have an initial value;
  assert( !is_literal(field) || field->data.initial );

  /*
   * Field names need not be unique.  They exist in the symbol table
   * (and in memory) regardless, but only unique names may be referenced.
   * We don't use symbol_add, because it looks up the symbol by name.
   */

  // ensure the table has room
  if( symbols.nelem == symbols.capacity ) {
    symbol_table_extend();
  };

  assert(symbols.nelem < symbols.capacity);  // not at capacity

  // append the symbol
  struct symbol_elem_t *e = symbols_end();
  *e = key;
  symbols.nelem++;

  field = cbl_field_of(e);
  if( is_constant(field) && field->type == FldNumericBin5 ) {
    cbl_name_t lname;
    std::transform( field->name, field->name + strlen(field->name) + 1,
                    lname, tolower );
    numeric_constants[program][lname] = symbol_index(e);
  }

  update_symbol_map2( e );
  return e;
}

/*
 * TYPEDEF is relevant only in Data Division.
 */
struct symbol_elem_t *
symbol_typedef( size_t program, const char name[] )
{
  auto beg = std::reverse_iterator<symbol_elem_t *>(symbols_end());
  auto end = std::reverse_iterator<symbol_elem_t *>(symbols_begin(program));

  auto p = std::find_if( beg, end,
                         [name]( const symbol_elem_t& sym ) {
                           if( sym.type == SymField ) {
                             auto f = cbl_field_of(&sym);
                             if( f->has_attr(typedef_e) ) {
                               return 0 == strcasecmp(name, f->name);
                             }
                           }
                           return false;
                         } );

  return p != end? &*p : NULL;
}

/*
 * Search backwards during symbol-table construction for nearest name.
 */
symbol_elem_t *
symbol_field( size_t program, size_t parent, const char name[] )
{
  class match_field {
    size_t program, parent;
    const char *name;
  public:
    match_field( size_t program, size_t parent, const char name[] )
      : program(program)
      , parent(parent)
      , name(name)
    {}
    bool operator()( const symbol_elem_t& sym ) const {
      if( sym.type != SymField ) return false;
      if( sym.program != program ) return false;

      const auto& field = *cbl_field_of(&sym);

      if( parent > 0 && parent != field.parent ) return false;
      if( field.is_typedef() ) return false;

      return 0 == strcasecmp(name, field.name);
    }
  };

  auto beg = std::reverse_iterator<symbol_elem_t *>(symbols_end());
  auto end = std::reverse_iterator<symbol_elem_t *>(symbols_begin(program));
  auto p = std::find_if( beg, end, match_field(program, parent, name) );

  return p != end? &*p : NULL;
}

symbol_elem_t *
symbol_register( const char name[] )
{
  auto p = std::find_if(symbols_begin(), symbol_at(symbols.first_program),
                        [len = strlen(name), name]( auto e ) {
                          if( e.type == SymField ) {
                            if( strlen(cbl_field_of(&e)->name) == len ) {
                              return 0 == strcasecmp(cbl_field_of(&e)->name, name);
                            }
                          }
                          return false;
                        } );

  return p;
}

// Find current 01 record during Level 66 construction.
const symbol_elem_t *
symbol_field_current_record() {
  assert(symbols.nelem > 0);
  size_t program = symbols_end()[-1].program;
  auto beg = std::reverse_iterator<symbol_elem_t *>(symbols_end());
  auto end = std::reverse_iterator<symbol_elem_t *>(symbols_begin(program));
  auto p = std::find_if( beg, end,
                         []( const auto& elem ) {
                           if( elem.type == SymField ) {
                             auto f = cbl_field_of(&elem);
                             return f->level == 1;
                           }
                           return false;
                         } );
  return p != end? &*p : NULL;
}


struct symbol_elem_t *
symbol_field_forward_add( size_t program, size_t parent,
                          const char name[], int line )
{
  auto e = symbol_field(program, parent, name);
  if( e ) return e;

  struct cbl_field_t field = { 0,
                               FldForward, FldInvalid, 0, parent, 0, 0,
                               nonarray, line, "",
                               0, cbl_field_t::linkage_t(),
                               {0,0,0,0, " "}, NULL };
  if( sizeof(field.name) < strlen(name) ) {
    dbgmsg("%s:%d: logic error: name %s too long", __func__, __LINE__, name);
    return NULL;
  }
  strcpy( field.name, name);
  return symbol_field_add( program, &field );
}

struct symbol_elem_t *
symbol_literalA( size_t program, const char name[] )
{
  cbl_field_t field = {};
  field.type = FldLiteralA;
  field.data.initial = name;
  field.attr = constq;

  struct symbol_elem_t key { program, field };

  symbol_elem_t *start = symbols_begin(key.program), *e;
  size_t nelem = symbols_end() - start;

  e = static_cast<struct symbol_elem_t *>(lfind( &key, start,
                                                 &nelem, sizeof(key),
                                                 symbol_elem_cmp ) );
  return e;
}

struct symbol_elem_t *
symbol_file( size_t program, const char name[] ) {
  size_t nelem = symbols.nelem;
  struct symbol_elem_t key = { SymFile, program }, *e = &key;

  assert(strlen(name) < sizeof(key.elem.file.name));
  strcpy(key.elem.file.name, name);

  do {
    e = static_cast<struct symbol_elem_t *>(lfind( &key, symbols.elems,
                                                   &nelem, sizeof(*e),
                                                   symbol_elem_cmp ) );
    if( e ) break;
    key.program = cbl_label_of(symbol_at(key.program))->parent;
    if( key.program == 0 ) break; // no file without a program
  } while( !e );

  if( e ) {
    assert(e->type == SymFile);
    return e;
  }

  // perhaps a record name?
  for( e = symbol_field(program, 0, name); e != NULL; e = symbol_parent(e) ) {
    if( e->type == SymFile ) {
      return e;
    }
    if( e->type != SymField ) {
      dbgmsg("%s:%d: '%s' is not a file and has parent of type %s",
            __func__, __LINE__, name, symbol_type_str(e->type));
      return NULL;
    }
    if( symbol_index(e) == 0 ) {
      dbgmsg("%s:%d: '%s' is not a file and has no parent",
            __func__, __LINE__, name);
      return NULL;
    }
  }

  assert(!e);
  return e;
}

struct symbol_elem_t *
symbol_field_alias( struct symbol_elem_t *e, const char name[] )
{
  cbl_field_t alias = *cbl_field_of(e);
  cbl_field_data_t data = { alias.data.memsize, alias.data.capacity };
  alias.data = data;
  alias.data.memsize = 0;

  assert(strlen(name) < sizeof(alias.name));
  strcpy(alias.name, name);

  alias.level = 66;
  alias.parent = symbol_index(e);
  alias.var_decl_node = NULL;

  return symbol_field_add(e->program, &alias);
}

struct symbol_elem_t *
symbol_field_alias2( struct symbol_elem_t *e, struct symbol_elem_t *e2,
                     const char name[] )
{
  assert(cbl_field_of(e)->data.picture == NULL);
  e = symbol_field_alias(e, name);

  cbl_field_t& alias = *cbl_field_of(e);
  alias.type = FldGroup;

  // store THRU symbol in data.picture, capacity computed by extend_66_capacity
  alias.data.picture = reinterpret_cast<char*>(e2);

  return e;
}

static bool
target_in_src( const cbl_field_t *tgt, const cbl_field_t *src ) {
  size_t isrc = field_index(src);
  while( tgt->parent > 0 ) {
    if( tgt->parent == isrc ) return true;
    auto e = symbol_at(tgt->parent);
    if( e->type != SymField ) break;
    tgt = cbl_field_of(e);
  }
  return false;
}

class elem_group_t {
  const symbol_elem_t *bog, *eog;
public:
  elem_group_t( const symbol_elem_t *bog, const symbol_elem_t *eog )
    : bog(bog), eog(eog) {}
  const symbol_elem_t *begin() const { return bog; }
  const symbol_elem_t *end()   const { return eog; }
};

static size_t
seek_parent( const symbol_elem_t *e, size_t level ) {
  size_t program = e->program;
  const cbl_field_t *field = cbl_field_of(e);
  while( program == e->program && level <= field->level ) {
    if( e->type != SymField ) break;
    auto f = cbl_field_of(e);
    if( f->parent == 0 ) break;
    e = symbol_at(f->parent);
  }
  return symbol_index(e);
}

/*
 * For SAME AS definition, copy the field metadata and update the parent.
 * For a group, create new fields and copy members recursively.
 * Precondition:  both fields exist in the symbol table.
 * Postcondition: return final element copied.
 *
 * "The condition-name entries for a particular conditional variable
 *  shall immediately follow the entry describing the item...."
 */
struct symbol_elem_t *
symbol_field_same_as( cbl_field_t *tgt, const cbl_field_t *src ) {
  if( target_in_src(tgt, src) ) {
    ERROR_FIELD(tgt, "%s %s  may not reference itself as part of %s %s",
            tgt->level_str(), tgt->name, src->level_str(), src->name);
    return NULL;
  }
  if( tgt->level == 77 && src->type == FldGroup ) {
    ERROR_FIELD(tgt, "%s %s TYPE TO %s must be an elementary item",
            tgt->level_str(), tgt->name, src->name);
    return NULL;
  }
  auto last_elem = symbol_at(field_index(tgt));
  tgt->same_as(*src, src->is_typedef());

  size_t isrc = field_index(src);

  symbol_elem_t *bog = symbol_at(isrc);
  symbol_elem_t *eog = symbol_at_impl(end_of_group(isrc), true);

  if( src->type != FldGroup ) {
    // For scalar, check for Level 88, which if extant must follow immediately.
    eog = std::find_if( bog + 1,
                        symbols_end(),
                        []( const auto& elem ) {
                          if( elem.type == SymField ) {
                            auto f = cbl_field_of(&elem);
                            return f->level != 88;
                          }
                          return true;
                        } );
  }

  cbl_field_t dup = {};
  dup.parent = field_index(tgt);
  dup.line = tgt->line;

  elem_group_t group(++bog, eog);

  for( const auto& elem : group ) {
    const cbl_field_t *that(cbl_field_of(&elem));
    if( is_forward(that) ) {
      auto e = symbol_field(current_program_index(), 0, that->name);
      that = cbl_field_of(e); // must exist
    }
    memcpy(dup.name, that->name, sizeof(dup.name));
    dup.occurs = that->occurs;
    dup.level = that->level;
    switch( dup.level ) {
    case 0:
      assert(that->type == FldIndex);
    case 88:
      break;
    default:
      dup.level += tgt->level;
      break;
    }
    dup.parent = seek_parent(last_elem, dup.level);
    dup.same_as( *that, src->is_typedef() );

    last_elem = symbol_field_add( last_elem->program, &dup );
  }

  return last_elem;
}

static bool first_among_equals( const cbl_file_t *a, const cbl_file_t *b ) {
  return symbol_index(symbol_elem_of(a)) < symbol_index(symbol_elem_of(b));
}

size_t
symbol_file_same_record_area( std::list<cbl_file_t*>& files ) {
  auto first = std::min_element(files.begin(), files.end(), first_among_equals);
  const auto ifirst_file = symbol_index(symbol_elem_of(*first));

  for( auto file : files ) {
    if( *first == file ) {
      assert(symbol_index(symbol_elem_of(file)) == ifirst_file );
      file->same_record_as = 0;
      continue;
    }
    auto& redefines = cbl_field_of(symbol_at(file->default_record))->parent;
    redefines = (*first)->default_record;
    file->same_record_as = ifirst_file;
  }
  return ifirst_file;
}

static symbol_elem_t *
next_program( symbol_elem_t *elem ) {
  size_t start = elem? symbol_index(elem) : 0;
  symbol_elem_t * e =
    std::find_if( symbols_begin(start), symbols_end(), is_program );
  if( e == symbols_end() ) {
    return NULL;
  }
  return e;
}

bool
is_cobol_name( const char name[] ) {
  for( symbol_elem_t *e = next_program(NULL);
       e != NULL; e = next_program(++e) ) {
    if( strcmp(name, cbl_label_of(e)->name) == 0 ) return true;
    if( symbol_field(symbol_index(e), 0, name) ) return true;
    if( symbol_label(symbol_index(e), LblNone, 0, name) ) return true;
  }
  return false;
}

const char *
is_numeric_constant( const char name[] ) {
  cbl_name_t lname;
  auto program = current_program_index();
  std::transform( name,
                  name + std::min(sizeof(lname), strlen(name) + 1),
                  lname, tolower );
  auto p = numeric_constants[program].find(lname);
  if( p != numeric_constants[program].end() ) {
    size_t isym = p->second;
    return cbl_field_of(symbol_at(isym))->data.initial;
  }
  return NULL;
}

// get default record layout for a file
struct cbl_field_t *
symbol_file_record( struct cbl_file_t *file ) {
  return cbl_field_of(symbol_at(file->default_record));
}

class is_section {
  cbl_section_type_t section_type;
 public:
  is_section( cbl_section_type_t sect ) : section_type(sect) {}
  bool operator()( symbol_elem_t& e ) const {
    return e.type == SymDataSection && cbl_section_of(&e)->type == section_type;
  }
};


static bool fd_record_size_cmp( const symbol_elem_t& a, const symbol_elem_t& b ) {
  return cbl_field_of(&a)->data.capacity < cbl_field_of(&b)->data.capacity;
}

cbl_file_key_t cbl_file_t::no_key;

/*
 * Find largest and smallest record defined for a file.  The rule is:
 * cbl_file_t::varies() returns true if the record size varies,
 * whether explicit or implied. In all cases if the record size
 * varies, min < else, min max == max.
 *
 * Input:                                      Output:
 * ------------------------------------------  ------------------
 * VARIES  FROM  TO  1st-FD-size  2nd-FD-size  varies()  min  max
 * VARIES     x   y                            true        x    y
 * VARIES     x   y          any          any  true        x    y
 * VARIES     x                                true        x   -1
 * VARIES         y          any          any  true        0    y
 * VARIES     x              120          150  true        x  150
 * VARIES                    120          150  true        0  150
 * VARIES                    150               true        0  150
 *                           120          150  true      120  150
 *                           150               false     150  150
 *
 * ISO 13.4.4.2 says "When no record description entries are specified:
 * a) a RECORD clause shall be specified in the file description entry"
 *
 * If VARIES TO Y is explicit, FROM 0 is implicit, notwithstanding any
 * record description(s).
 */
cbl_file_t::varying_t
symbol_file_record_sizes( struct cbl_file_t *file ) {
  if( file->varies() ) {
    return file->varying_size;
  }

  // Compute implicit records sizes from FD 01 records
  assert( ! file->varying_size.explicitly );

  auto file_element = symbol_elem_of(file);
  auto pend = std::find_if( file_element, symbols_end(),
                            is_section(working_sect_e) );
  std::list<symbol_elem_t> records;
  std::copy_if( file_element, pend, back_inserter(records),
                [ifile = symbol_index(file_element)](const symbol_elem_t& elem) {
                  if( elem.type == SymField ) {
                    return ifile == cbl_field_of(&elem)->file;
                  }
                  return false;
                } );
  if( records.empty() ) return file->varying_size;

  auto p = std::minmax_element(records.begin(), records.end(),
                               fd_record_size_cmp);

  // Make a copy, update the sizes, and return it.
  cbl_file_t::varying_t output = file->varying_size;

  output.min = cbl_field_of(&*p.first)->data.capacity;
  output.max = cbl_field_of(&*p.second)->data.capacity;

  assert(output.min > 0 && "min record size is 0");
  assert(output.min <= output.max);

  return output;
}

/*
 * Find a symbol's type based solely on its name.
 *
 * The lexer uses this function to determine if the referenced name is
 * special in some way.  To be correct, the symbol table (or at least
 * the lookup mechanism) must reflect what the current namespace is.
 * If a symbol is ambiguous -- if a name could be a level 01 and part
 * of a group, say -- only the first match is returned.  This may lead
 * the parser astray, which is too bad.
 *
 * As of 30 Oct 2021, there are 22 instances where introducing just a
 * plain NAME in the parser where otherwise NAME X Y is needed would
 * create shift-reduce conflicts. This function allows the lexer to
 * returns a spealized name, which the parser distinguishes from a
 * generic name.  The S/R conflicts could in theory be resolved with
 * precedence, but it's not obvious to the author that's the best
 * choice, or the least effort.
 *
 * The risk seems small.  The distinction here is by field type, not
 * value.  If there are two fields FOO, one a level 88 and another a
 * variable, it's not clear if that can be resolved by the lexer, even
 * with the parser's help.  The bet is that won't matter because
 * it won't happen.
 */
enum cbl_field_type_t
symbol_field_type( size_t program, const char name[] ) {
  struct symbol_elem_t *e = symbol_field( program, 0, name );

  return e && e->type == SymField? cbl_field_of(e)->type: FldInvalid;
}

struct cbl_field_t *
constant_of( size_t isym )
{
  assert(isym < symbols.nelem);
  struct cbl_field_t *field = cbl_field_of(symbols.elems + isym);
  assert((field->attr & constant_e) == constant_e);
  return field;
}

bool
cbl_alphabet_t::assign( const YYLTYPE& loc, unsigned char ch, unsigned char high_value ) {
  if( alphabet[ch] == 0xFF || alphabet[ch] == high_value) {
    alphabet[ch] = high_value;
    last_index = ch;
    return true;
  }
  auto taken = alphabet[ch];
  error_msg(loc, "ALPHABET %s, character '%c' (X'%x') "
           "in position %d already defined at position %d",
           name,
           ISPRINT(ch)? ch : '?', ch,
           high_value, taken );
  if( yydebug ) dump();
  return false;
}

void
cbl_alphabet_t::also( const YYLTYPE& loc, size_t ch ) {
  if( ch < 256 ) {
    alphabet[ch] = alphabet[last_index];
    if( ch == high_index ) high_index--;
    return;
  } // else it's a figurative constant ...

  ch &= 0xFFFF; // High bit indicated symbol-table entry; mask off high word.
  assert( ch < 256 );
  auto field = cbl_field_of(symbol_at(ch));
  auto attr = field->attr;
  assert(attr & constant_e);

  // last_index is already set; use it as the "last value before ALSO"
  if( attr & low_value_e ) {
    alphabet[0] = alphabet[last_index];
    return;
  }
  if( attr & high_value_e ) {
    alphabet[high_index--] = alphabet[last_index];
    return;
  }
  if( attr & (space_value_e|quote_value_e) ) {
    ch = field->data.initial[0];
    alphabet[ch] = alphabet[last_index];
    return;
  }
  if( attr & (zero_value_e) ) {
    alphabet[0] = alphabet[last_index];
    error_msg(loc, "ALSO value '%s' is unknown", field->name);
    return;
  }
  error_msg(loc, "ALSO value %zu is unknown", ch);
}

using std::deque;
static deque<cbl_field_t*> stack;

static cbl_field_t *
new_temporary_impl( enum cbl_field_type_t type )
{
  extern int yylineno;
  static int nstack, nliteral;
  static const struct cbl_field_t empty_alpha = {
                                0, FldAlphanumeric, FldInvalid,
                                intermediate_e, 0, 0, 0, nonarray, 0, "",
                                0, cbl_field_t::linkage_t(),
                                {MAXIMUM_ALPHA_LENGTH, MAXIMUM_ALPHA_LENGTH, 
                                                            0, 0, NULL}, NULL };
  static const struct cbl_field_t empty_float = {
                                0, FldFloat, FldInvalid,
                                intermediate_e,
                                0, 0, 0, nonarray, 0, "",
                                0, cbl_field_t::linkage_t(),
                                {16, 16, 32, 0, NULL}, NULL };
  static const struct cbl_field_t empty_comp5 = {
                                0, FldNumericBin5, FldInvalid,
                                signable_e | intermediate_e,
                                0, 0, 0, nonarray, 0, "",
                                0, cbl_field_t::linkage_t(),
                                {16, 16, MAX_FIXED_POINT_DIGITS, 0, NULL}, NULL };
  static const struct cbl_field_t empty_conditional = {
                                0, FldConditional, FldInvalid, intermediate_e,
                                0, 0, 0, nonarray, 0, "",
                                0, cbl_field_t::linkage_t(),
                                {}, NULL };
  static struct cbl_field_t empty_literal = {
                                0, FldInvalid, FldInvalid, CONSTANT_E,
                                0, 0, 0, nonarray, 0, "",
                                0, cbl_field_t::linkage_t(),
                                {}, NULL };
  struct cbl_field_t *f = new cbl_field_t;
  f->type = type;

  switch(type) {
  case FldGroup:
  case FldAlphanumeric:
    *f = empty_alpha;
    break;
  case FldInvalid:
  case FldClass:
  case FldForward:
  case FldIndex:
  case FldSwitch:
  case FldDisplay:
  case FldPointer:
  case FldBlob:
    break;
  case FldConditional:
    *f = empty_conditional;
    break;
  case FldLiteralA:
  case FldLiteralN:
    *f = empty_literal;
    f->type = type;
    break;
  case FldNumericBin5:
  case FldNumericBinary:
  case FldNumericDisplay:
  case FldNumericEdited:
  case FldAlphaEdited:
  case FldPacked:
    *f = empty_comp5;
    break;
  case FldFloat:
    *f = empty_float;
    break;
  }

  f->line = yylineno;
  if( is_literal(type) ) {
    snprintf(f->name, sizeof(f->name), "_literal%d",++nliteral);
  } else {
    snprintf(f->name, sizeof(f->name), "_stack%d",++nstack);
  }

  return f;
}

cbl_field_t *
new_temporary_decl() {
  auto field = new_temporary_impl(FldAlphanumeric);
  strcpy(field->name, "DECLARATIVES");
  return field;
}

static inline cbl_field_t *
parser_symbol_add2( cbl_field_t *field ) {
  parser_symbol_add(field);
  return field;
}

static cbl_field_t *
new_literal_add( const char initial[], uint32_t len, enum cbl_field_attr_t attr ) {
  static char empty[2] = "\0";
  cbl_field_t *field = NULL;
  if( !(attr & quoted_e) )
    {
    field = new_temporary_impl(FldLiteralN);
    field->attr |= attr;
    field->data.valify(initial);
    }
  else
    {
    field = new_temporary_impl(FldLiteralA);
    field->attr |= attr;
    field->data.initial = len > 0? initial : empty;
    field->data.capacity = len;

    if( ! field->internalize() )
      {
        ERROR_FIELD(field, "inconsistent string literal encoding for '%s'", initial);
      }
    }

  static size_t literal_count = 1;
  sprintf(field->name,
          "%s%c_%zd",
          "_literal",
          field->type == FldLiteralA ? 'a' : 'n',
          literal_count++);

  return parser_symbol_add2(field);
}

static temporaries_t temporaries;

cbl_field_t *
temporaries_t::literal( const char value[], uint32_t len, cbl_field_attr_t attr ) {
  auto key = literal_an(value, quoted_e == (attr & quoted_e));

  if( 0 == (attr & hex_encoded_e) ) {
    auto p = literals.find(key);
    if( p != literals.end() ) {
      cbl_field_t *field = p->second;
      return field;
    }
  }
  return literals[key] = new_literal_add(value, len, attr);
}

cbl_field_t *
new_literal( uint32_t len, const char initial[], enum cbl_field_attr_t attr ) {
  return temporaries.literal(initial, len, attr);
}

void
temporaries_t::dump() const {
  extern int yylineno;
  char *output  = xasprintf("%4d: %zu Literals", yylineno, literals.size());

  for( const auto& elem : used ) {
    if( ! elem.second.empty() ) {
      char *so_far = output;
      output = xasprintf("%s, %zu %s",
                         so_far,
                         elem.second.size(),
                         3 + cbl_field_type_str(elem.first));
      free(so_far);
    }
  }
  dbgmsg("status: %s", output);
  free(output);
}

temporaries_t::~temporaries_t() {
}

cbl_field_t *
temporaries_t::add( cbl_field_t *field ) {
  auto p = used[field->type].insert(field);
  bool yn(p.second);
  assert(yn);
  return *p.first;
};

cbl_field_t *
temporaries_t::reuse( cbl_field_type_t type ) {
////  DUBNER is defeating reuse as part of investigating problems with recursion
  return NULL;
////

  auto& fields = freed[type];
  cbl_field_t *field;

  if( fields.empty() ) {
    return NULL;
  } else {
    auto p = fields.begin();
    field = *p;
    fields.erase(p);
  }

  return add(field);
}

cbl_field_t *
temporaries_t::acquire( cbl_field_type_t type ) {
  cbl_field_t *field = reuse(type);

  if( !field ) {
    field = new_temporary_impl(type);
    add(field);
  }
  return parser_symbol_add2(field); // notify of reuse
}

void
symbol_temporaries_free() {
  for( auto& elem : temporaries.used ) {
    const cbl_field_type_t& type(elem.first);
    temporaries_t::fieldset_t& used(elem.second);

    auto freed = std::inserter(temporaries.freed[type],
                               temporaries.freed[type].begin());
    std::transform( used.begin(), used.end(), freed,
                    []( auto field ) {
                      switch( field->type ) {
                      case FldConditional:
                        field->attr &= intermediate_e;
                        break;
                      case FldNumericBin5:
                        field->set_attr(signable_e);
                        break;
                      default:
                        break;
                      }
                      return field;
                    } );
    used.clear();
  }
}

cbl_field_t *
new_alphanumeric( size_t capacity ) {
  cbl_field_t * field = new_temporary_impl(FldAlphanumeric);
  field->data.capacity = capacity;
  temporaries.add(field);
  return parser_symbol_add2(field);
}

cbl_field_t *
new_temporary( enum cbl_field_type_t type, const char *initial ) {
  if( ! initial ) {
    assert( ! is_literal(type) ); // Literal type must have literal value.
    return temporaries.acquire(type);
  }
  if( is_literal(type) ) {
    auto field = temporaries.literal(initial,
                                     type == FldLiteralA? quoted_e : none_e);
    return field;
  }
  cbl_field_t *field = new_temporary_impl(type);
  field->data.capacity = strlen(field->data.initial = initial);
  temporaries.add(field);
  parser_symbol_add(field);

  return field;
}

#if needed
cbl_field_t *
keep_temporary( cbl_field_type_t type ) {
  auto field = new_temporary(type);
  bool ok = temporaries.keep(field);
  assert(ok);
  return field;
}
#endif

cbl_field_t *
new_temporary_like( cbl_field_t skel ) {
  auto field = temporaries.reuse(skel.type);
  if( ! field ) {
    field = new_temporary_impl(skel.type);
    temporaries.add(field);
  }
  memcpy(skel.name, field->name, sizeof(field->name));
  skel.var_decl_node = field->var_decl_node;
  *field = skel;

  return parser_symbol_add2(field);
}

cbl_field_t *
new_temporary_clone( const cbl_field_t *orig) {
  cbl_field_type_t type = is_literal(orig)? FldAlphanumeric : orig->type;
  auto field = temporaries.reuse(type);
  if( ! field ) {
    field = new_temporary_impl(type);
    temporaries.add(field);
  }
  field->data = orig->data;
  if( field->type == FldNumericBin5 ) field->type = orig->type;
  field->attr = intermediate_e;

  return parser_symbol_add2(field);
}

bool
cbl_field_t::is_ascii() const {
  return std::all_of( data.initial,
                      data.initial + data.capacity,
                      isascii );
}

/*
 * Convert an input source-code string literal (or VALUE) to internal encoding.
 *
 * Input encoding initially defaults to UTF-8, regardless of locale(7),
 * for two reasons:
 *   1) The source code might not match the locale
 *   2) The assumption is easily disproved with most input.  That is,
 *      input values above 0x7F will rarely look like UFT-8 unless
 *      they actually are UTF-8.
 *
 * If conversion from UTF-8 fails, the compiler's locale is examined
 * next.  If it is C, it is ignored, else it is tried.  If that fails,
 * the input is assumed to be encoded as CP1252.
 *
 * This is a global static sticky setting, meaning that during
 * compilation, if it moves off the default, it adjusts only once, and
 * never reverts.
 */
static const char standard_internal[] = "CP1252";
extern os_locale_t os_locale;

static const char *
guess_encoding() {
  static const char *fromcode;

  if( ! fromcode ) {
    return fromcode = os_locale.assumed;
  }

  if( fromcode == os_locale.assumed ) {
    fromcode = os_locale.codeset;
    if( 0 != strcmp(fromcode, "C") ) { // anything but that
      return fromcode;
    }
  }

  return standard_internal;
}

const char *
cbl_field_t::internalize() {
  static const char *tocode = standard_internal;
  static const char *fromcode = guess_encoding();
  static  iconv_t cd = iconv_open(tocode, fromcode);
  static const size_t noconv = size_t(-1);

  if (cd == (iconv_t)-1) {
    yywarn("failed iconv_open tocode = '%s' fromcode = %s", tocode, fromcode);
  }

  bool using_assumed = fromcode == os_locale.assumed;

  if( fromcode == tocode || has_attr(hex_encoded_e) ) {
    return data.initial;
  }

  if( is_ascii() ) return data.initial;
  assert(data.capacity > 0);

  std::vector<char> output(data.capacity + 2, '\0');
  char *out = output.data();
  char *in = const_cast<char*>(data.initial);
  size_t n, inbytesleft = data.capacity, outbytesleft = output.size();
  if( !is_literal(this) && inbytesleft < strlen(data.initial) ) {
    inbytesleft = strlen(data.initial);
  }

  assert(fromcode != tocode);

  while( (n = iconv( cd, &in, &inbytesleft, &out, &outbytesleft)) == noconv ) {
    if( !using_assumed ) break; // change only once
    fromcode = guess_encoding();
    cd = iconv_open(tocode, fromcode);
    dbgmsg("%s: trying input encoding %s", __func__, fromcode);
    if( fromcode == tocode ) break;
  }

  if( n == noconv ) {
    if( !using_assumed ) {
      yywarn("failed to decode '%s' as %s", data.initial, fromcode);
      return NULL;
    }
    return data.initial;
  }

  if( 0 < inbytesleft ) {
    // data.capacity + inbytesleft is not correct if the remaining portion has
    // multibyte characters.  But the fact reamins that the VALUE is too big.
    ERROR_FIELD(this, "%s %s VALUE '%s' requires %zu bytes for size %u",
            cbl_field_t::level_str(level), name, data.initial,
            data.capacity + inbytesleft, data.capacity );
  }

  // Replace data.initial only if iconv output differs.
  if( 0 != memcmp(data.initial, output.data(), out - output.data()) ) {
    assert(out <= output.data() + data.capacity);

    dbgmsg("%s: converted '%.*s' to %s",
                        __func__, data.capacity, data.initial, tocode);

    int len = int(out - output.data());
    char *mem = static_cast<char*>( xcalloc(1, output.size()) );

    // Set the new memory to all blanks, tacking a '!' on the end.
    memset(mem, 0x20, output.size() - 1);
    mem[ output.size() - 2] = '!';

    if( is_literal(this) ) {
      data.capacity = len; // trailing '!' will be overwritten
    }

    memcpy(mem, output.data(), len); // copy only as much as iconv converted

    free(const_cast<char*>(data.initial));
    data.initial = mem;
  }

  return data.initial;
}

const char *
cbl_label_t::str() const {
  char *buf;
  switch(type) {
  case LblParagraph:
    buf = xasprintf("%-12s %s OF '%s', line %d", type_str() + 3, name,
                    parent? cbl_label_of(symbol_at(parent))->name : "", line);
    break;
  case LblProgram:
    if( parent == 0 ) {
      buf = xasprintf("%-12s %s top level [%s], line %d",
                      type_str() + 3, name, mangled_name, line);
    } else {
      buf = xasprintf("%-12s %s OF #%zu '%s' [%s], line %d",
                      type_str() + 3, name, parent,
                      cbl_label_of(symbol_at(parent))->name,
                      mangled_name, line);
    }
    break;
  default:
    buf = xasprintf("%-12s %s, line %d", type_str() + 3, name, line);
  }
  return buf;
}

size_t
cbl_label_t::explicit_parent() const {
  switch(type) {
  case LblParagraph: case LblSection: case LblNone:
    if( parent != 0 ) {
      // implicit parents don't count
      symbol_elem_t *p = symbol_at(parent);
      if( p->type == SymLabel && cbl_label_of(p)->name[0] == '_' ) {
        return 0;
      }
    }
    break;
  default:
    break;
  }
  return parent;
}

cbl_prog_hier_t::cbl_prog_hier_t() {
  nlabel = std::count_if( symbols_begin(), symbols_end(), is_program );
  assert(nlabel >0);
  labels = new cbl_prog_hier_t::program_label_t[nlabel];

  std::copy_if( symbols_begin(), symbols_end(),
                labels, is_program );
}

/*
 * Map of program to its callable COMMON programs.
 */
static std::map<size_t, symbolset_t> common_callables;

symbolset_t
symbol_program_programs() {
  symbolset_t programs;

  for( const auto& elem : common_callables ) {
    if( elem.first == 0 ) continue;
    assert(symbol_at(elem.first)->type == SymLabel);
    assert(is_program(*symbol_at(elem.first))); // might be a function
    programs.insert(elem.first);
  }
  return programs;
}

static void
common_callables_update( const size_t iprog ) {
  // Add this directly contained COMMON program to the parent's set.
  auto prog = cbl_label_of(symbol_at(iprog));
  if( prog->type != LblProgram ) return;
  if( prog->common ) {
    common_callables[prog->parent].insert(iprog);
  }

  // Add all ancestors' COMMON programs to the iprog siblings and uncles.
  std::list<size_t> dnr; // do not recurse

  while( prog->parent > 0 ) {
    if( !prog->recursive ) dnr.push_back(symbol_index(symbol_elem_of(prog)));
    auto c = common_callables[prog->parent];
    common_callables[iprog].insert(c.begin(), c.end());
    prog = cbl_label_of(symbol_at(prog->parent));
  }
  // Top-level programs (parent == 0) cannot be COMMON, but are public
  // symbols.  They can be called from anywhere, except from a
  // (directly or indirectly) contained program, unless marked
  // RECURSIVE.
  assert(prog->parent == 0);
  auto itop = symbol_index(symbol_elem_of(prog));
  common_callables[0].insert(itop);
  if( prog->recursive ) {
    common_callables[iprog].insert(itop);
  }

  for( size_t isym : dnr ) {
    common_callables[iprog].erase(isym);
  }
}

/*
 * Unlike fields, there is no LblForward.  Instead, a forward
 * reference to a procedure -- section or paragraph name -- begins
 * life as LblNone.  When it is actually defined, the lookup function
 * updates the LblNone entry and defines its type, parent, and line
 * number.
 */
cbl_label_t *
symbol_label_add( size_t program, cbl_label_t *input )
{
  cbl_label_t *label = symbol_label(program, input->type,
                                    input->parent, input->name);

  if( label && label->type == LblNone ) {
    label->type = input->type;
    label->parent = input->parent;
    label->line = input->line;

    return label;
  }

  // Set the program's mangled name, dehyphenated and uniqified by parent index.
  if( input->type == LblProgram ) {
    char *psz = cobol_name_mangler(input->name);
    input->mangled_name = xasprintf("%s.%zu", psz, input->parent);
    free(psz);
  }

  struct symbol_elem_t
    elem { program, *input }, *e = &elem;

  assert(0 <= e->elem.label.line);
  e->elem.label.line = -e->elem.label.line; // force insertion

  if( (e = symbol_add(&elem)) == NULL ) {
    cbl_errx("%s:%d: could not add '%s'", __func__, __LINE__, label->name);
  }

  common_callables_update( symbol_index(e) );

  // restore munged line number unless symbol_add returned an existing label
  if( e->elem.label.line < 0 ) e->elem.label.line = -e->elem.label.line;

  symbols.labelmap_add(e);
  return cbl_label_of(e);
}

/*
 * Under ISO (and not IBM) Declaratives are followed by a Section name.  When
 * the first statement is parsed, verify, if Declaratives were used, that it
 * was preceeded by a Section name.
 */
bool
symbol_label_section_exists( size_t program ) {
  auto pblob = std::find_if( symbols_begin(program), symbols_end(),
                               []( const auto& sym ) {
                                 if( sym.type == SymField ) {
                                   auto& f( sym.elem.field );
                                   return f.type == FldBlob;
                                 }
                                 return false;
                               } );
  if( pblob == symbols_end() ) return true; // Section name not required

  bool has_section = std::any_of( ++pblob, symbols_end(),
                               []( const auto& sym ) {
                                 if( sym.type == SymLabel ) {
                                   auto& L(sym.elem.label);
                                   if( L.type == LblSection ) {
                                     if( L.name[0] != '_' ) { // not implicit
                                       return true; // Section name exists
                                     }
                                   }
                                 }
                                 return false;
                               } );
  if( yydebug && ! has_section ) {
    symbols_dump(program, true);
  }
  // Return true if no Declaratives, because the (non-)requirement is met.
  // Return false if Declaratives exist, because no Section name was found.
  return has_section;
}

cbl_label_t *
symbol_program_add( size_t program, cbl_label_t *input )
{
  symbol_elem_t elem { program, *input }, *e;

  assert( is_program(elem) );

  // Set the program's mangled name, dehyphenated and uniqified by parent index.
  char *psz = cobol_name_mangler(input->name);
  elem.elem.label.mangled_name = xasprintf("%s.%zu", psz, input->parent);
  free(psz);

  e = std::find_if( symbols_begin(program), symbols_end(),
                    [program, name = input->name]( const auto& elem ) {
                      if( elem.type == SymLabel ) {
                        if( program == elem.program ) {
                          auto L = cbl_label_of(&elem);
                          if( 0 == strcasecmp(name, L->name) ) return true;
                        }
                      }
                      return false;
                    } );
  if( e != symbols_end() ) return NULL;

  e = symbol_append(elem);

  common_callables_update( symbol_index(e) );

  return cbl_label_of(e);
}

#if 1
struct cbl_special_name_t *
symbol_special( special_name_t id ) {
  cbl_special_name_t special = { 0, id };
  struct symbol_elem_t key { 0, special }, *e;

  e = static_cast<struct symbol_elem_t *>(lfind( &key, symbols.elems,
                                                 &symbols.nelem, sizeof(key),
                                                 symbol_elem_cmp ) );
  return e? cbl_special_name_of(e) : NULL;
}
#endif

struct symbol_elem_t *
symbol_special_add( size_t program, struct cbl_special_name_t *special )
{
  // Ensure this special name isn't already defined for this program.
  struct symbol_elem_t *e = symbol_special(program, special->name);

  if( e ) {
    return e;
  }
  assert(e == NULL);

  struct symbol_elem_t elem { program, *special };

  if( (e = symbol_add(&elem)) == NULL ) {
    cbl_errx( "%s:%d: could not add '%s'", __func__, __LINE__, special->name);
  }

  elem_key_t key(program, cbl_special_name_of(e)->name);
  symbols.specials[key] = symbol_index(e);

  return e;
}

struct cbl_section_t *
symbol_section( size_t program, struct cbl_section_t *section ) {
  struct symbol_elem_t key { program, *section }, *e;

  e = static_cast<struct symbol_elem_t *>(lfind( &key, symbols.elems,
                                                 &symbols.nelem, sizeof(key),
                                                 symbol_elem_cmp ) );
  return e? cbl_section_of(e) : NULL;
}


struct symbol_elem_t *
symbol_section_add( size_t program, struct cbl_section_t *section )
{
  if( symbol_section(program, section) ) {
    return NULL; // error, exists
  }

  struct symbol_elem_t *e, elem { program, *section };

  if( (e = symbol_add(&elem)) == NULL ) {
    cbl_errx( "%s:%d: could not add '%s'", __func__, __LINE__, section->name());
  }

  return e;
}

static int
currency_char_in_string(const char *picture) {
    // This can take an unexpanded string
    int retval = 0;
    while(*picture) {
        if( symbol_currency(*picture) ){
            retval = *picture;
            break;
        }
    picture += 1;
    }
    return retval;
}

static
int l_and_r(const char *expanded_picture, int ch) {
    const char *l =  strchr(expanded_picture, ch);
    const char *r = strrchr(expanded_picture, ch);
    return r > l ? ch : 0;
}

static int
floating_char_in_string(const char *expanded_picture) {
    int ch = '+';
    if( l_and_r(expanded_picture, ch) )  {
        return ch;
    }
    ch = '-';
    if( l_and_r(expanded_picture, ch) ) {
        return ch;
    }
    ch = currency_char_in_string(expanded_picture);
    if( ch && l_and_r(expanded_picture, ch) ) {
        return ch;
    }
    return 0;
}

char *
expand_picture(const char *picture)
    {
    assert(strlen(picture) < PICTURE_MAX); // guaranteed by picset() in scanner
    size_t retval_length = PICTURE_MAX;
    char *retval = (char *)xmalloc(retval_length);
    size_t index = 0;

    int ch;
    int prior_ch = '\0';
    const char *p = picture;

    long repeat;

    int currency_symbol = currency_char_in_string(picture);

    while( (ch = (*p++ & 0xFF) ) )
        {
        if( ch == '(' )
            {
            // Pick up the number after the left parenthesis
            char *endchar;
            repeat = strtol(p, &endchar, 10);

            // We subtract one because we know that the character just before
            // the parenthesis was already placed in dest
            repeat -= 1;

            // Update p to the character after the right parenthesis
            p = endchar + 1;

            if( index + repeat >= retval_length )
                {
                retval_length <<= 1;
                retval = (char *)xrealloc(retval, retval_length);
                }

            while(repeat--)
                {
                retval[index++] = prior_ch;
                }
            }
        else
            {
            if( index >= retval_length )
                {
                retval_length <<= 1;
                retval = (char *)xrealloc(retval, retval_length);
                }
            retval[index++] = ch;
            }
        prior_ch = ch;
        }
    if( index >= retval_length )
        {
        retval_length <<= 1;
        retval = (char *)xrealloc(retval, retval_length);
        }
    retval[index++] = '\0';

    size_t dest_length = strlen(retval);

    // We have to take into account the possibility that the currency symbol
    // mapping might be to a string of more than one character:

    if( currency_symbol )
        {
        size_t sign_length = strlen(symbol_currency(currency_symbol)) - 1;
        if( sign_length )
            {
            char *pcurrency = strchr(retval, currency_symbol);
            assert(pcurrency);
            memmove(    pcurrency + sign_length,
                        pcurrency,
                        dest_length+1 - (pcurrency-retval));
            for(size_t i=0; i<sign_length; i++)
                {
                pcurrency[i] = 'B';
                }
            dest_length += sign_length;
            }
        }

    return retval;
    }

int
length_of_picture(const char *picture)
{
    // Calculate the length of a PICTURE string with the parenthetical
    // abbreviations expanded:  +9(10).9(4)CR, as an example, returns 18
    int retval = 0;
    char ch;
    char prior_char = 0; // Calm the compiler down
    const char *p = picture;
    const char *currency_sign = NULL;
    int currency_char = currency_char_in_string(picture);

    if( currency_char )
        {
        currency_sign = symbol_currency(currency_char);
        }

    while( (ch = *p++) ) {
        if( ch == '(' ) {
            // Pick up the number that starts after the left parenthesis
            char *endchar;
            int increment = strtol(p, &endchar, 10);
            if( prior_char != 'P' ) {
                retval += increment-1 ;
            }
            p = endchar + 1;
        }
        else {
            prior_char = TOUPPER(ch);
            if( prior_char != 'P' ) {
                // P-scaling characters don't count in the capacity:
                retval += 1;
            }
        }

    }
    // We need to adjust for the length of a currency sign, because it might
    // have more than one character.  We've already accounted for one of its
    // characters, so....
    if( currency_sign ) {
        retval += strlen(currency_sign) - 1;
    }
    return retval;
}

int
digits_of_picture(const char *runlength_picture, bool return_rdigits)
    {
    // This is a strangely busy routine.  The capacity is calculated elsewhere,
    // by the length_of_picture() routine.  This routine calculates the
    // total number of digits (which are the total number of digit positions)
    // and the number of rdigits (digit positions to the right of any decimal
    // point.)
    //
    // It also takes into account the possibility of the number being P-scaled.
    // The scaled_e attribute also gets set separately.  For a numeric-edited
    // scaled_e value, a positive value of rdigits means the number is less than
    // 1.000000 and has an extra rdigits's count of '0' between the decimal
    // point and the rest of the number
    //
    // A negative value of rdigits means that the number has no decimal places,
    // is zero or greater, and has an extra scaling factor of 10^(-rdigits)

    int retval;
    char *picture = expand_picture(runlength_picture);
    int digits  = 0;
    int rdigits = 0;
    int pcount  = 0;
    unsigned char ch;
    const char *p = picture;
    const char *rightmost_p = NULL;
    const char *rightmost_d = NULL;
    const char *decimal_position = NULL;
    const char *first_float = NULL;

    unsigned char floating_character = floating_char_in_string(picture);

    while( (ch = *p++) )
        {
        if( ch == decimal_point || ch == 'v' || ch == 'V')
            {
            // This is an actual or virtual decimal point
            // There should only be one of these in the picture string
            decimal_position = p-1;
            }
        else if( ch == floating_character )
            {
            // All but the first floating character acts like a digit
            // position.  We'll adjust the counts at the end
            digits += 1;
            if( decimal_position )
                {
                // Having encountered a decimal point means this is an
                // rdigit:
                rdigits += 1;
                }
            if( !first_float )
                {
                first_float = p-1;
                }
            continue;
            }
        else
            {
            switch(ch)
                {
                case '9' :
                case 'z' :
                case 'Z' :
                case '*' :
                    // These are positions that can hold a digit
                    rightmost_d = p-1;
                    digits += 1;
                    if( decimal_position )
                        {
                        // Having encountered a decimal point means this is an
                        // rdigit:
                        rdigits += 1;
                        }
                    break;

                case 'P':
                case 'p':
                    rightmost_p = p-1;
                    pcount += 1;
                    break;
                }
            }
        }

    // We have looped through all the characters

    if( floating_character )
        {
        // Account for the fact that ++ turns into +<digit>, but only one digit
        digits -= 1;

        if( decimal_position )
            {
            if( first_float > decimal_position )
                {
                // Because the first_float is to the right of the
                // decimal point, rdigits has to be reduced by one:
                rdigits -=1 ;
                }
            }
        }

    if( pcount )
        {
        // We encountered some P-scaling characters in the PICTURE string.
        if( rightmost_p < rightmost_d )
            {
            // This is a scaled variable of type PPP999
            rdigits = pcount;
            }
        else
            {
            // This is a scaled variable of type 999PPP
            rdigits = -pcount;
            }
        }

    free(picture);

    if(return_rdigits)
        {
        retval = rdigits;
        }
    else
        {
        retval = digits;
        }

    return retval;
    }


int
rdigits_of_picture(const char *picture) {
    return digits_of_picture(picture, true);
}

bool
is_picture_scaled(const char *picture) {
    bool retval = false;
    if( strchr( picture, 'P') ) {
        retval = true;
    }
    if( strchr( picture, 'p') ) {
        retval = true;
    }
    return retval;
}

/*
 * Static call support.  Return reachable programs.
 *
 * 8.4.5.2 Scope of program-names
 *
 * "The names assigned to programs that are contained directly or
 *  indirectly within the same outermost program shall be unique within
 *  that outermost program."
 *
 * At point of CALL, the target name might or might not be that of a
 * contained or COMMON program.  If no such program exists, the CALL
 * is to an external reference. If exactly one such program exists,
 * the CALL references that program.  The returned map is used to
 * enforce those rules, and to replace seemingly external calls with
 * internal ones.
 */

symbolset_t
symbol_program_callables( size_t program ) {
  symbolset_t callables = common_callables[program];

  auto self = cbl_label_of(symbol_at(program));
  auto start_with = 0 < self->parent? self->parent : program;

  // Build a list of programs reachable by the current program.
  for( auto e = symbols_begin(++start_with); e < symbols_end(); e++ ) {
    if( e->type != SymLabel ) continue;
    if( e->elem.label.type != LblProgram ) continue;

    auto prog = cbl_label_of(e);
    if( program == symbol_index(e) && !prog->recursive ) continue;

    if( (self->parent == prog->parent && prog->common) ||
        (prog->parent == program) )
    {
      callables.insert(symbol_index(e));
    }
  }

  return callables;
}


const cbl_label_t *
symbol_program_local( const char tgt_name[] ) {
  symbolset_t callables = symbol_program_callables(current_program_index());

  for( auto callable : callables ) {
    auto called = cbl_label_of(symbol_at(callable));
    if( 0 == strcasecmp(called->name, tgt_name) ) return called;
  }
  return NULL;
}

/*
 * FILE SECTION support
 */

/*
 * SPECIAL-NAMES support
 */
std::map<char, const char *> currencies;

bool
symbol_currency_add( const char symbol[], const char sign[] ) {
  // In service of CURRENCY sign PICTURE SYMBOL symbol
  // The single-character 'symbol' is replaced with multi-char 'sign'
  // by the NumericEdited processing.
  if( !symbol ) {
    symbol = xasprintf("%c", *sign);
  }
  currencies[*symbol] = sign;
  return true;
}

const char *
symbol_currency( char sign ) {
  // We need a default of '$'
  if( currencies.size() == 0 ) {
    currencies['$'] = "$";
  }
  auto result = currencies.find(sign);
  return result == currencies.end()? NULL : result->second;
}

char symbol_decimal_point_set( char ch ) { return decimal_point = ch; }
char symbol_decimal_point() { return decimal_point; }
bool decimal_is_comma() { return decimal_point == ','; }

/*
 * OCCURS support
 */

/*
 * A cbl_occurs_key_t is part of a field definition, and comprises
 * size_t symbol indexes.  A cbl_key_t is a list of field pointers,
 * and can be created ad hoc to describe a sort. We can construct a
 * cbl_key_t from cbl_occurs_key_t.
 */
cbl_key_t::
cbl_key_t( const cbl_occurs_key_t& that )
  : ascending(that.ascending)
{
  if( that.field_list.nfield == 0 ) {
    *this = cbl_key_t();
    return;
  }

  nfield = that.field_list.nfield;
  fields = static_cast<cbl_field_t**>( xcalloc(nfield,
                                                   sizeof(*fields)) );
  for( size_t i=0; i < that.field_list.nfield; i++ ) {
    fields[i] = cbl_field_of(symbol_at(that.field_list.fields[i]));
  }
}

void
cbl_occurs_t::key_alloc( bool ascending ) {
  auto nbytes = sizeof(keys[0]) * (nkey + 1);
  cbl_occurs_key_t key = { ascending, cbl_field_list_t() };

  keys = static_cast<cbl_occurs_key_t *>(xrealloc(keys, nbytes));
  keys[nkey++] = key;
}

void
cbl_occurs_t::field_add( cbl_field_list_t& field_list, cbl_field_t *field ) {
  cbl_field_list_t list = field_list;
  size_t ifield = field_index(field);
  auto nbytes = sizeof(list.fields[0]) * (list.nfield + 1);

  list.fields = static_cast<size_t*>(xrealloc(list.fields, nbytes));
  list.fields[list.nfield++] = ifield;
  field_list = list;
}

void
cbl_occurs_t::key_field_add( cbl_field_t *field ) {
  assert(nkey > 0);
  cbl_occurs_key_t& key = keys[nkey-1];
  field_add(key.field_list, field);
}

void
cbl_occurs_t::index_add( cbl_field_t *field ) {
  field_add(indexes, field);
}

class is_field_at {
  cbl_field_t *field;
 public:
 is_field_at( cbl_field_t *field ) : field(field) {}
  bool operator()( size_t isym ) const  {
    return field == field_at(isym);
  }
};

cbl_occurs_key_t *
cbl_occurs_t::key_of( cbl_field_t *field ) {
  for( auto key = keys; key < keys + nkey; key++ ) {
    size_t *fields = key->field_list.fields;
    size_t *efield = key->field_list.fields + key->field_list.nfield;
    auto f = std::find_if( fields, efield, is_field_at(field) );
    if( f < efield ) {
      return key;
    }
  }
  return NULL;
}

bool
cbl_occurs_t::subscript_ok( const cbl_field_t *subscript ) const {
  if( !is_literal(subscript) ) {
    return true; // Cannot check non-literals, so, OK.
  }
  // It must be a number.
  if( subscript->type != FldLiteralN ) return false;

  // This only gets us int64_t, which is more than adequate for a table subscript
  auto sub = real_to_integer (TREE_REAL_CST_PTR (subscript->data.value_of()));
  REAL_VALUE_TYPE csub;
  real_from_integer (&csub, VOIDmode, sub, SIGNED);

  if( sub < 1
      || !real_identical (&csub,
                          TREE_REAL_CST_PTR (subscript->data.value_of())) ) {
    return false; // zero/fraction invalid
  }
  if( bounds.fixed_size() ) {
    return (size_t)sub <= bounds.upper;
  }
  return bounds.lower <= (size_t)sub && (size_t)sub <= bounds.upper;
}

cbl_file_key_t::
cbl_file_key_t( cbl_name_t name,
                const std::list<cbl_field_t *>& fields,
                bool is_unique )
  : unique(is_unique)
  , leftmost(0)
{
  assert(name);
  memcpy(this->name, name, sizeof(this->name));
  nfield = fields.size();
  assert(nfield > 0);
  this->fields = new size_t[nfield];
  std::transform( fields.begin(), fields.end(), this->fields, field_index );
}

size_t cbl_file_key_t::
offset() const {
  return cbl_field_of(symbol_at(fields[0]))->offset;
}

/*
 * A multi-field key has a name.  A single-field key has no name.
 */
bool cbl_file_key_t::
operator==( const cbl_field_t *key_field ) {
  this->leftmost = 0;

  // match multi-field key by name
  if( 0 == strcasecmp(this->name, key_field->name) ) return true;

  // A literal key_field is a "magic" literal indicating a key name
  // (that didn't match, above).
  if( is_literal(key_field) ) return false;

  // match single-field key by its symbol index
  size_t ifield = field_index(key_field);
  if( nfield == 1 && fields[0] == ifield ) return true;

  // A literal key_field is a "magic" literal indicating a key name
  // (that didn't match, above).
  if( is_literal(key_field) ) return false;

  // Match if the field has the same offset as the key, and belongs to
  // an 01 record for the same FD.
  if( this->offset() == key_field->offset ) {
    auto this_file( symbol_record_file(cbl_field_of(symbol_at(fields[0]))) );
    auto that_file( symbol_record_file(key_field) );
    if( this_file && that_file &&
        symbol_index(symbol_elem_of(this_file)) ==
        symbol_index(symbol_elem_of(that_file)) ) {
      this->leftmost = ifield;
      return true;
    }
  }

  return false;
}

uint32_t cbl_file_key_t::
key_field_size( uint32_t sum, size_t ifield ) {
  return sum + field_size( cbl_field_of(symbol_at(ifield)) );
}

// Return size of named field in key or, if NULL, whole key
uint32_t cbl_file_key_t::
size() {
  if( leftmost != 0 ) {
    return cbl_field_of(symbol_at(leftmost))->data.capacity;
  }
  return std::accumulate(fields, fields + nfield, 0, key_field_size);
}


/*
 * Produce list of qualifier names for any key field.
 */
static std::list<const char *>
symbol_forward_names( size_t ifield ) {
  std::list<const char *> output;

  for( auto sym = symbols_begin(ifield); sym && sym->type == SymField; ) {
    const cbl_field_t *field = cbl_field_of(sym);
    if( !(field->type == FldForward) ) {
      dbgmsg("%s:%d: logic error, not FldForward: #%zu %s",
            __func__, __LINE__, symbol_index(sym), field_str(field));
    }
    assert(field->type == FldForward);

    output.push_front( field->name );

    if( 0 == field->parent) break;
    sym = symbols_begin(field->parent);
  }

  return output;
}

static size_t
symbol_forward_to( size_t fwd ) {
  std::list<const char *> names = symbol_forward_names(fwd);
  size_t program = symbols_begin(fwd)->program;

  std::pair <symbol_elem_t *, bool> elem = symbol_find( program, names );

  if( !elem.second ) {
    const auto& field = *cbl_field_of(symbols_begin(fwd));
    if( yydebug )
      dbgmsg("%s:%d: no symbol found for #%zu %s %s", __func__, __LINE__,
               fwd, cbl_field_type_str(field.type), field.name);
    return fwd;
  }

  return symbol_index(elem.first);
}

/*
 * For each FldForward, resolve to a field that is part of an FD
 * record for the file.
 */
void
cbl_file_key_t::deforward( size_t ifile ) {
  const auto file = cbl_file_of(symbol_at(ifile));
  std::transform( fields, fields + nfield, fields,
                  [ifile, file]( size_t fwd ) {
                    static std::map<size_t, int> keys;
                    auto ifield = symbol_forward_to(fwd);
                    const auto field = cbl_field_of(symbol_at(ifield));

                    if( is_forward(field) && yydebug ) {
                      dbgmsg("%s:%d: key %d: #%zu %s of %s is %s", "deforward", __LINE__,
                            keys[ifile]++, ifield, field->name, file->name,
                            cbl_field_type_str(field->type) + 3);
                    }

                    auto parent = symbol_record_file(field);

                    if( ifield == fwd ) {
                      ERROR_FIELD(field, "line %d: %s of %s "
                               "is not defined",
                               file->line, field->name, file->name);
                      return ifield;
                    }

                    // relative files have numeric keys that are not part of the record
                    if( file->org == file_relative_e ) {
                      if( parent != NULL ) {
                        ERROR_FIELD(field, "line %d: RELATIVE file %s key %s "
                                 "is defined in file description",
                                 file->line, file->name, field->name);
                        return ifield;
                      }
                      if( field->occurs.ntimes() ) {
                        ERROR_FIELD(field, "line %d: RELATIVE file %s key %s "
                                 "cannot have OCCURS clause",
                                 file->line, file->name, field->name);
                        return ifield;
                      }
                      if( ! (is_numeric(field) && 0 == field->data.rdigits) ) {
                        ERROR_FIELD(field, "line %d: RELATIVE file %s key %s "
                                    "must be integer type",
                                    file->line, file->name, field->name);
                        return ifield;
                      }
                      return ifield;
                    }
                    // looked-up field must have same file as parent
                    if( ! (parent != NULL &&
                           symbol_index(symbol_elem_of(parent)) == ifile) ) {
                      ERROR_FIELD(field, "line %d: %s of %s "
                               "is not defined in file description",
                               file->line, field->name, file->name);
                    }
                    return ifield;
                  } );
}

char *
cbl_file_key_t::str() const {
  char *output = static_cast<char*>( xcalloc(nfield, 8) ), *p = output;
  assert(output);
  const char *sep = "";

  *p++ = '[';
  for( auto f = fields; f < fields + nfield; f++) {
    auto n = sprintf(p, "%s%zu", sep, *f);
    p += n;
    sep = ", ";
  }
  *p++ = ']';
  return output;
}

/*
 * After processing FILE SECTION, replace forward references with actual ones.
 */
void
cbl_file_t::deforward() {
  if( user_status ) {
    user_status = symbol_forward_to(user_status);

    auto field = cbl_field_of(symbol_at(user_status));
    if( is_forward(field) ) {
      ERROR_FIELD(field, "%s of %s never defined in FD record",
               field->name, this->name);
    }
  }

  for( auto p = keys; p < keys + nkey; p++ ) {
    p->deforward( symbol_index(symbol_elem_of(this)) );
  }
}

char *
cbl_file_t::keys_str() const {
  std::vector <char *> ks(nkey);
  std::transform(keys, keys + nkey, ks.begin(),
                 []( const cbl_file_key_t& key ) {
                   return key.str();
                 } );
  size_t n = 4 * nkey + std::accumulate(ks.begin(), ks.end(), 0,
                                        []( int n, const char *s ) {
                                          return n +  strlen(s);
                                        } );
  char *output = static_cast<char*>( xcalloc(1, n) ), *p = output;
  const char *sep = "";

  *p++ = '[';
  for( auto k : ks ) {
    p = stpcpy(p, sep);
    p = stpcpy(p, k);
    sep = ", ";
    free(k);
  }
  *p++ = ']';
  return output;
}

/*
 * _FILE_STATUS symbols
 */

static struct file_status_field_t {
  file_status_t status;
} file_status_fields[] = {
  {FsSuccess},
  {FsDupRead},
  {FsRecordLength},
  {FsUnavail},
  {FsNotaTape},

  {FsEofSeq},
  {FsEofRel},

  {FsKeySeq},
  {FsDupWrite},
  {FsNotFound},
  {FsEofWrite},

  {FsOsError},
  {FsBoundary},
  {FsNoFile},
  {FsNoAccess},
  {FsCloseLock},
  {FsWrongType},

  {FsLogicErr},
  {FsIsOpen},
  {FsCloseNotOpen},
  {FsNoRead},
  {FsBoundWrite},
  {FsReadError},
  {FsReadNotOpen},
  {FsNoWrite},
  {FsNoDelete},

  {FsWrongThread},
  {FsPassword},
  {FsLogicOther},
  {FsNoResource},
  {FsIncomplete},
  {FsNoDD},
  {FsVsamOK},
  {FsBadEnvVar},
};

static int
cbl_file_status_cmp( const void *K, const void *E ) {
  const struct file_status_field_t
    *k=static_cast<const struct file_status_field_t *>(K),
    *e=static_cast<const struct file_status_field_t *>(E);
  return k->status == e->status? 0 : 1;
}

static long
file_status_status_of( file_status_t status ) {
  size_t n = COUNT_OF(file_status_fields);
  file_status_field_t *fs, key { status };

  fs = (file_status_field_t*)lfind( &key, file_status_fields,
                                    &n, sizeof(*fs), cbl_file_status_cmp );

  return fs? (long)fs->status : -1;
}

cbl_field_t *
ast_file_status_between( file_status_t lower, file_status_t upper ) {
  struct { cbl_field_t *lb, *ub, *both; } cond = { new_temporary(FldConditional),
                                                   new_temporary(FldConditional),
                                                   new_temporary(FldConditional) };

  cbl_field_t *file_status = cbl_field_of(symbol_field(0, 0, "_FILE_STATUS"));

  long status_lower = file_status_status_of(lower);
  long status_upper = file_status_status_of(upper);
  assert(status_lower != -1);
  assert(status_upper != -1);

  parser_relop_long( cond.lb, status_lower, le_op, file_status );
  parser_relop_long( cond.ub, status_upper, gt_op, file_status );

  parser_logop( cond.both, cond.lb, and_op, cond.ub );

  return cond.both;
}

bool
is_register_field(cbl_field_t *field)
  {
  // TRUE when the field is an executable-level global variable of the type we
  // are calling a "register", like RETURN-CODE or UPSI or the like:
  return
    (    field->parent == 0
      && field->level == 0
      && !(field->attr & intermediate_e)
      && !(field->attr & filler_e)
      && field->type != FldClass
      && field->type != FldBlob
      );
  }

bool
has_value( cbl_field_type_t type ) {
  // Indicates that the field type contains data that can be expressed as
  // a numeric value
  switch ( type ) {
  case FldInvalid:
  case FldGroup:
  case FldAlphanumeric:
  case FldNumericEdited:
  case FldAlphaEdited:
  case FldLiteralA:
  case FldClass:
  case FldConditional:
  case FldForward:
  case FldSwitch:
  case FldDisplay:
  case FldBlob:
    return false;
  case FldIndex:
  case FldPointer:
  case FldNumericDisplay:
  case FldNumericBinary:
  case FldFloat:
  case FldPacked:
  case FldNumericBin5:
  case FldLiteralN:
    return true;
  }
  dbgmsg( "%s:%d: invalid symbol_type_t %d", __func__, __LINE__, type );
  return false;
}
