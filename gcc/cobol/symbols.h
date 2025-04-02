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
#ifdef _SYMBOLS_H_
#pragma message __FILE__ " included twice"
#else
#define _SYMBOLS_H_

#include <assert.h>
#include <limits.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include <algorithm>
#include <list>
#include <map>
#include <set>
#include <stack>
#include <string>
#include <vector>

#define PICTURE_MAX 64

extern const char *numed_message;

enum cbl_dialect_t {
  dialect_gcc_e = 0x00,
  dialect_ibm_e = 0x01,
  dialect_mf_e  = 0x02,
  dialect_gnu_e = 0x04,
};

extern cbl_dialect_t cbl_dialect;
void cobol_dialect_set( cbl_dialect_t dialect );
cbl_dialect_t dialect_is();

static inline bool dialect_gcc() {
  return dialect_gcc_e  == cbl_dialect;
}

static inline bool dialect_ibm() {
  return dialect_ibm_e == (cbl_dialect & dialect_ibm_e);
}
static inline bool dialect_mf() {
  return dialect_mf_e  == (cbl_dialect & dialect_mf_e );
}

enum cbl_gcobol_feature_t {
  feature_gcc_e = 0x00,
  feature_internal_ebcdic_e = 0x01,
  feature_embiggen_e        = 0x02, // widen numeric that redefine POINTER
};

extern size_t cbl_gcobol_features;
bool cobol_gcobol_feature_set( cbl_gcobol_feature_t gcobol_feature, bool on = true );

static inline bool gcobol_feature_internal_ebcdic() {
  return feature_internal_ebcdic_e ==
    (cbl_gcobol_features & feature_internal_ebcdic_e);
}
static inline bool gcobol_feature_embiggen() {
  return feature_embiggen_e ==
    (cbl_gcobol_features & feature_embiggen_e);
}

enum cbl_division_t {
  identification_div_e,
  environment_div_e,
  data_div_e,
  procedure_div_e,
};

void mode_syntax_only( cbl_division_t division );
bool mode_syntax_only();

static inline bool
is_numeric( cbl_field_type_t type ) {
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
  case FldPointer: // not numeric because not computable, only settable
  case FldBlob:
    return false;
  // These types are computable or, in the case of FldIndex, may be
  // arbitrarily set and incremented.
  case FldNumericDisplay:
  case FldNumericBinary:
  case FldFloat:
  case FldPacked:
  case FldNumericBin5:
  case FldLiteralN:
  case FldIndex:
    return true;
  }
  yywarn( "%s:%d: invalid symbol_type_t %d", __func__, __LINE__, type );
  return false;
}

struct os_locale_t {
  char assumed[16];
  char *codeset;
};

const char * cbl_field_attr_str( cbl_field_attr_t attr );

cbl_field_attr_t literal_attr( const char prefix[] );

static inline bool
is_working_storage(uint32_t attr) {
  return 0 == (attr & (linkage_e | local_e));
}

enum cbl_figconst_t cbl_figconst_of( const char *value );
const char * cbl_figconst_str( cbl_figconst_t fig );

const char * consistent_encoding_check( const YYLTYPE& loc, const char input[] );

class cbl_domain_elem_t {
  uint32_t length;
  const char *value;
 public:
  bool is_numeric,  all;

  cbl_domain_elem_t()
    : length(0), value(NULL), is_numeric(false), all(false)
  {}
  cbl_domain_elem_t( const YYLTYPE& loc,
                     bool all,
                     uint32_t length,
                     const char *value,
                     bool is_numeric = false )
    : length(length), value(value), is_numeric(is_numeric), all(all)
  {
    if( value && ! is_numeric ) {
      auto s = consistent_encoding_check(loc, value);
      if( s ) value = s;
    }
  }
  const char *name() const { return value; }
  uint32_t size() const { return is_numeric ? strlen(value) : length; }
};

struct cbl_domain_t {
  cbl_domain_elem_t first, last;
  cbl_domain_t() : first(), last(first)
  {}
  cbl_domain_t( const YYLTYPE& loc,
                bool all,
                uint32_t length,
                const char * value,
                bool is_numeric = false )
    : first(loc, all, length, value, is_numeric), last(first)
  {}
  cbl_domain_t( const cbl_domain_elem_t& a, const cbl_domain_elem_t& z )
    : first(a)
    , last(z)
  {
    assert(a.is_numeric == z.is_numeric);
  }
};

typedef const char * (*time_now_f)( void );

const char * date2_is_now(void);
const char * day2_is_now(void);
const char * date4_is_now(void);
const char * day4_is_now(void);
const char * time_is_now(void);

struct cbl_upsi_mask_t {
  bool on_off;
  uint32_t value;
cbl_upsi_mask_t( bool on_off, uint32_t value ) : on_off(on_off), value(value) {}
};

char symbol_decimal_point_set( char ch );
char symbol_decimal_point();
bool decimal_is_comma();

enum symbol_type_t {
  SymFilename,
  SymFunction,
  SymField,
  SymLabel,                     // section, paragraph, or label
  SymSpecial,
  SymAlphabet,
  SymFile,
  SymDataSection,
};

// The ISO specification says alphanumeric literals have a maximum length of
// 8,191 characters.  It seems to be silent on the length of alphanumeric data
// items.  Our implementation requires a maximum length, so we chose to make it
// the same.
#define MAXIMUM_ALPHA_LENGTH 8192

struct cbl_field_data_t {
  uint32_t memsize;             // nonzero if larger subsequent redefining field
  uint32_t capacity,            // allocated space
           digits;              // magnitude: total digits (or characters)
  int32_t  rdigits;             // digits to the right
  const char *initial, *picture;

  enum etc_type_t { val88_e, upsi_e, value_e } etc_type;
  const char *
  etc_type_str() const {
    switch(etc_type) {
    case val88_e: return "val88_e";
    case upsi_e: return "upsi_e";
    case value_e: return "value_e";
    }
    return "???";
  }
  
  union etc_t {
    // "Domain" is an array representing the VALUE of CLASS or 88 type.
    struct val88_t {
      cbl_domain_t *false_value;
      cbl_domain_t *domain;
      val88_t() : false_value(NULL), domain(NULL) {}
    } val88;
    struct cbl_upsi_mask_t *upsi_mask;
    tree value;

    explicit etc_t( tree v = build_zero_cst (float128_type_node)) : value(v) {}
  } etc;

  cbl_field_data_t( uint32_t memsize=0,  uint32_t capacity=0 )
    : memsize(memsize)
    , capacity(capacity)
    , digits(0)
    , rdigits(0)
    , initial(0)
    , picture(0)
    , etc_type(value_e)
    , etc()
  {}

  cbl_field_data_t( uint32_t memsize,  uint32_t capacity,
                    uint32_t digits,  uint32_t rdigits,
                    const char *initial,
                    const char *picture = NULL ) 
    : memsize(memsize)
    , capacity(capacity)
    , digits(digits)
    , rdigits(rdigits)
    , initial(initial)
    , picture(picture)
    , etc_type(value_e)
    , etc()
  {}

  cbl_field_data_t( const cbl_field_data_t& that ) {
    copy_self(that);
  }
  cbl_field_data_t& operator=( const cbl_field_data_t& that ) {
    return copy_self(that);
  }

  cbl_domain_t * false_value_of() const { return etc.val88.false_value; }
  cbl_domain_t * false_value_as( cbl_domain_t * domain ) {
    etc_type = val88_e;
    return etc.val88.false_value = domain;
  }
  cbl_domain_t * domain_of() const {
    assert(etc_type == val88_e);
    return etc.val88.domain;
  }
  cbl_domain_t * domain_as(cbl_domain_t * domain) {
    etc_type = val88_e;
    return etc.val88.domain = domain;
  }
  cbl_upsi_mask_t * upsi_mask_of() const {
    assert(etc_type == upsi_e);
    return etc.upsi_mask;
  }
  cbl_upsi_mask_t * operator=( cbl_upsi_mask_t * mask) {
    etc_type = upsi_e;
    return etc.upsi_mask = mask;
  }
  tree value_of() const {
    if( etc_type != value_e ) {
      dbgmsg("%s:%d: type is %s", __func__, __LINE__, etc_type_str());
    }
    return etc.value;
  } 
  tree& operator=( tree v) {
    etc_type = value_e;
    return etc.value = v;
  } 

  void set_real_from_capacity( REAL_VALUE_TYPE *r ) const {
    real_from_integer (r, VOIDmode, capacity, SIGNED);
  }

  time_now_f time_func;

  uint32_t upsi_mask_derive() const {
    assert(initial);
    assert('0' <= initial[0] && initial[0] < '8');
    const uint32_t bitn = initial[0] - '0';
    return (1 << bitn);
  }

  int32_t precision() const { return std::max(int32_t(0), rdigits); }
  int32_t ldigits() const { return std::max(int(digits), int(digits - rdigits)); }

  cbl_field_data_t& valify() {
    assert(initial);
    const size_t len = strlen(initial);
    std::string input(len + 1, '\0'); // add a NUL
    std::copy(initial, initial + len, input.begin()); 
    if( decimal_is_comma() ) {
      std::replace(input.begin(), input.end(), ',', '.');
    }

    double d;
    int n;
    int erc = sscanf(input.c_str(), "%lf%n", &d, &n);
    
    if( erc < 0 || size_t(n) != input.size() ) {
      dbgmsg("%s: error: could not interpret '%s' of '%s' as a number",
             __func__, initial + n, initial);
    }

    REAL_VALUE_TYPE r;
    real_from_string (&r, input.c_str());
    r = real_value_truncate (TYPE_MODE (float128_type_node), r);
    etc.value = build_real (float128_type_node, r);
    return *this;
  }
  cbl_field_data_t& valify( const char *input ) {
    assert(input);
    initial = input;
    capacity = strlen(initial);
    return valify();
  }

 protected:
  cbl_field_data_t& copy_self( const cbl_field_data_t& that ) {
    memsize = that.memsize;
    capacity = that.capacity;
    digits = that.digits;
    rdigits = that.rdigits;
    initial = that.initial;
    picture = that.picture;
    etc_type = that.etc_type;

    switch(etc_type) {
      case value_e:
        etc.value = that.etc.value;
        break;
      case val88_e:
        etc.val88 = that.etc.val88;
        break;
      case upsi_e:
        etc.upsi_mask = that.etc.upsi_mask;
        break;
      } 
    return *this;
  }
};

static inline  uint32_t
capacity_cast( size_t size ) {
  uint32_t len = static_cast<uint32_t>(size);
  assert(len == size);
  return len;
}

struct cbl_occurs_bounds_t {
  // lower = upper = 0 for a non-table
  // lower = upper = occurs for a fixed table
  // lower and upper are the (inclusive) bounds for DEPENDING ON in a
  // variable size table.  lower can be zero.
  size_t lower, upper;

  cbl_occurs_bounds_t(size_t lower=0, size_t upper=0)
    : lower(lower), upper(upper) {}
  size_t ntimes() const {
    return upper;
  }
  bool fixed_size() const { return lower == upper; }
};

struct cbl_field_t; // A necessary forward reference

struct cbl_field_list_t {
  size_t nfield;
  size_t *fields;
  cbl_field_list_t() : nfield(0), fields(NULL) {}
};

struct cbl_occurs_key_t {
  bool ascending;
  cbl_field_list_t field_list;
};

struct cbl_occurs_t {
  cbl_occurs_bounds_t bounds;
  size_t depending_on;
  size_t nkey;
  cbl_occurs_key_t *keys;
  cbl_field_list_t indexes;

  cbl_occurs_t() : depending_on(0), nkey(0), keys(NULL) {}

  size_t ntimes() const { return bounds.ntimes(); }

  void key_alloc( bool ascending );
  void key_field_add( cbl_field_t *field );
  void index_add( cbl_field_t *field );
  cbl_occurs_key_t * key_of( cbl_field_t *field );
  bool subscript_ok( const cbl_field_t *subscript ) const;

protected:
  void field_add( cbl_field_list_t& fields, cbl_field_t *field );
};

/*
 * Support for CALL and Linkage Section.
 */
enum cbl_ffi_arg_attr_t { none_of_e, address_of_e, length_of_e };

enum cbl_ffi_crv_t {
  by_default_e,
  by_reference_e = 'R',
  by_content_e = 'C',
  by_value_e = 'E'
};

static inline const char *
cbl_ffi_crv_str( cbl_ffi_crv_t crv ) {
  switch (crv) {
  case by_default_e:   return "<default>";
  case by_reference_e: return "REFERENCE";
  case by_content_e:   return "CONTENT";
  case by_value_e:     return "VALUE";
  }
  return "???";
}

typedef std::pair<size_t, size_t>  cbl_bytespan_t;
struct cbl_subtable_t {
  size_t offset, isym;
};

bool is_elementary( enum cbl_field_type_t type );

/*  In cbl_field_t:
 *  'offset' is overloaded for FldAlphanumeric/temporary/intermediate variables
 *  For such variables, offset is a copy of the initial capacity.  This is in
 *  support of the FUNCTION TRIM function, which both needs to be able to
 *  reduce the capacity of the target variable, and then to reset it back to
 *  the original value
 */

struct cbl_field_t {
  size_t offset;
  enum cbl_field_type_t type, usage;
  size_t attr;
  static_assert(sizeof(attr) == sizeof(cbl_field_attr_t), "wrong attr size");
  size_t parent;    // symbols[] index of our parent
  size_t our_index; // symbols[] index of this field, set in symbol_add()
  uint32_t level;
  struct cbl_occurs_t occurs;
  int line;                     // Where it appears in the file.
  cbl_name_t name;              // Appears in the GIMPLE dump.
  size_t file;                  // nonzero if field is 01 record for a file
  struct linkage_t {
    bool optional;
    cbl_ffi_crv_t crv;            // Using by C/R/V in Linkage
    linkage_t() : optional(false), crv(by_default_e) {}
  } linkage;
  struct cbl_field_data_t data;
  tree var_decl_node;   // Reference to the pointer to the cblc_field_t structure
  tree data_decl_node;  // Reference to the run-time data of the COBOL variable
  //                    // For linkage_e variables, data_decl_node is a pointer
  //                    // to the data, rather than the actual data
  tree literal_decl_node; // This is a FLOAT128 version of data.value

  void set_linkage( cbl_ffi_crv_t crv, bool optional ) {
    linkage.optional = optional;
    linkage.crv = crv;
    assert(crv != by_content_e);
  }

  inline bool is_typedef() const {
    return has_attr(typedef_e);
  }
  inline bool is_strongdef() const {
    return has_attr(strongdef_e);
  }

  bool is_valid() const {
    return data.capacity > 0
      || level == 88
      || level == 66
      || type == FldClass
      || type == FldIndex
      || type == FldLiteralA
      || type == FldLiteralN;
  }

  bool is_zero() const {
    return real_zerop(data.value_of());
  }

  bool rename_level_ok() const {
    switch( level ) {
    case 0:
    case 1:
    case 66:
    case 77:
    case 88:
      return false;
    }
    return true;
  }

  bool reasonable_capacity() const {
    return data.capacity <= MAX_FIXED_POINT_DIGITS;
  }

  cbl_field_t& same_as( const cbl_field_t& that, bool is_typedef ) {
    type  = that.type;
    attr |= (that.attr & external_e);
    attr |= same_as_e;

    data  = that.data;

    if( ! (is_typedef || that.type == FldClass) ) {
      data.initial = NULL;
      data = build_zero_cst (float128_type_node);
    }
    return *this;
  }

  void report_invalid_initial_value(const YYLTYPE& loc) const;

  bool is_ascii() const;
  bool is_integer() const { return is_numeric(type) && data.rdigits == 0; }

  bool is_binary_integer() const {
    return type == FldNumericBinary || type == FldNumericBin5;
  }

  HOST_WIDE_INT as_integer() const {
    return real_to_integer( TREE_REAL_CST_PTR (data.value_of()) );
  }

  void embiggen( size_t eight=8 ) {
    assert(gcobol_feature_embiggen() && is_numeric(type) && size() == 4);

    type = FldNumericBin5;
    attr |= embiggened_e;
    data.capacity = eight;
    data.digits = 0;
  }

  bool has_attr( cbl_field_attr_t attr ) const {
    return cbl_field_attr_t(this->attr & attr) == attr;
  }
  size_t set_attr( cbl_field_attr_t attr );
  size_t clear_attr( cbl_field_attr_t attr );
  const char * attr_str( const std::vector<cbl_field_attr_t>& attrs ) const;

  bool is_justifiable() const {
    if( type == FldAlphanumeric ) return true;
    if( type == FldInvalid ) return true;
    return ! has_attr(rjust_e);
  }

  bool has_subordinate( const cbl_field_t *that ) const;

  const char * internalize();
  const char *value_str() const;

  bool is_key_name() const { return has_attr(record_key_e); }

  long scaled_capacity() const {
    return data.digits?
      long(data.digits) - data.rdigits
      :
      data.capacity;
  }
  uint32_t size() const; // table capacity or capacity

  const char * pretty_name() const {
    if( name[0] == '_' && data.initial ) return data.initial;
    return name;
  }
  static const char * level_str(uint32_t level );
  inline const char * level_str() const {
    return level_str(level);
  }
};

// Necessary forward referencea
struct cbl_label_t;
struct cbl_refer_t;

struct cbl_span_t {
  cbl_refer_t *from, *len;

  cbl_span_t( cbl_refer_t *from, cbl_refer_t *len = NULL )
    : from(from), len(len) {};
  bool is_active() const { return !( from == NULL && len == NULL ); }

  cbl_field_t *from_field();
  cbl_field_t *len_field();
};


struct cbl_refer_t {
  YYLTYPE loc;
  cbl_field_t *field;
  cbl_label_t *prog_func;
  bool all, addr_of;
  uint32_t nsubscript;
  cbl_refer_t *subscripts;  // indices
  cbl_span_t refmod;        // substring bounds

  cbl_refer_t()
    : field(NULL), prog_func(NULL)
    , all(NULL), addr_of(false)
    , nsubscript(0), subscripts(NULL), refmod(NULL)
  {}
  cbl_refer_t( cbl_field_t *field, bool all = false )
    : field(field), prog_func(NULL)
    , all(all), addr_of(false)
    , nsubscript(0), subscripts(NULL), refmod(NULL)
  {}
  cbl_refer_t( const YYLTYPE& loc, cbl_field_t *field, bool all = false )
    : loc(loc), field(field), prog_func(NULL)
    , all(all), addr_of(false)
    , nsubscript(0), subscripts(NULL), refmod(NULL)
  {}
  cbl_refer_t( cbl_field_t *field, cbl_span_t& refmod )
    : field(field), prog_func(NULL)
    , all(false), addr_of(false)
    , nsubscript(0), subscripts(NULL), refmod(refmod)
  {}
  cbl_refer_t( cbl_field_t *field,
               size_t nsubscript, cbl_refer_t *subscripts,
               cbl_span_t refmod = cbl_span_t(NULL) )
    : field(field), prog_func(NULL)
    , all(false), addr_of(false)
    , nsubscript(nsubscript) , subscripts( new cbl_refer_t[nsubscript] )
    , refmod(refmod)
  {
    std::copy(subscripts, subscripts + nsubscript, this->subscripts);
  }
  explicit cbl_refer_t( cbl_label_t *prog_func, bool addr_of = true )
    : field(NULL), prog_func(prog_func)
    , all(false), addr_of(addr_of)
    , nsubscript(0), subscripts(NULL), refmod(cbl_span_t(NULL))
  {}

  cbl_refer_t duplicate() const {
    return cbl_refer_t( field, nsubscript, subscripts, refmod );
  }

  static cbl_refer_t *empty();

  cbl_refer_t * name( const char name[] ) {
    assert(name);
    assert(strlen(name) < sizeof(field->name));
    strcpy(field->name, name);
    return this;
  }

  bool is_pointer() const { return addr_of || field->type == FldPointer; }
  bool is_reference() const { return nsubscript > 0 || refmod.is_active(); }
  bool is_table_reference() const  { return nsubscript > 0; }
  bool is_refmod_reference() const  { return refmod.is_active(); }

  size_t subscripts_set( const std::list<cbl_refer_t>& subs );
  const char * str() const;
  const char * deref_str() const;
  const char * name() const;
  cbl_field_t * cond() {
    assert( ! is_reference() );
    assert(field);
    if( FldConditional != field->type ) {
      dbgmsg("cbl_refer_t::cond: "
             "logic error: %s is not a condition expression", field->name);
    }
    assert( FldConditional == field->type);
    return field;
  }
};

struct elem_key_t {
  size_t program;
  const char * name;
  elem_key_t( size_t program, const cbl_name_t name  )
    : program(program)
    , name(name)
  {}
  bool operator<( const elem_key_t& that ) const {
    if( program == that.program ) {
      return  strcasecmp(name, that.name) < 0;
    }
    return program < that.program;
  }
  bool operator==( const elem_key_t& that ) const {
    if( program == that.program ) {
      return strcasecmp(name, that.name) == 0;
    }
    return false;
  }
};

struct field_key_t {
  size_t program;
  const char * name;
  field_key_t( size_t program, const cbl_field_t *field  )
    : program(program)
    , name(field->name)
  {}
  field_key_t( size_t program, const cbl_name_t name  )
    : program(program)
    , name(name)
  {}
  bool operator<( const field_key_t& that ) const {
    if( program == that.program ) {
      return  strcasecmp(name, that.name) < 0;
    }
    return program < that.program;
  }
  bool operator==( const field_key_t& that ) const {
    if( program == that.program ) {
      return strcasecmp(name, that.name) == 0;
    }
    return false;
  }
};

bool valid_move( const struct cbl_field_t *tgt, const struct cbl_field_t *src );

#define record_area_name_stem "_ra_"

static inline bool
is_record_area( const cbl_field_t *field ) {
  static const char stem[] = record_area_name_stem;
  return 0 == memcmp(field->name, stem, sizeof(stem)-1);
}

bool
is_register_field(cbl_field_t *field);

static inline bool
is_constant( const cbl_field_t *field ) {
  return field->has_attr(constant_e);
}

const char *
is_numeric_constant( const char name[] );

cbl_field_t *
symbol_field_index_set( cbl_field_t *field );

bool
symbol_field_type_update( cbl_field_t *field,
                          cbl_field_type_t type, bool is_usage );

struct sort_key_t;

struct cbl_key_t {
  bool ascending;
  size_t nfield;
  cbl_field_t **fields;

  cbl_key_t() : ascending(false), nfield(0), fields(0) {}
  cbl_key_t( size_t nfield, cbl_field_t **fields, bool ascending = true )
    : ascending(ascending), nfield(nfield), fields(fields) {}
  cbl_key_t( const sort_key_t& src );
  explicit cbl_key_t( const cbl_occurs_key_t& that );
};

enum cbl_label_type_t {
    /*
     * LblNone "matches" all types, because it exists for forward
     * references.  Labels are equal if the types match and the names
     * match.
     */
  LblNone,                      // top-level programs have no parent
  LblProgram,
  LblFunction,
  LblSection,
  LblParagraph,
  LblLoop,
  LblEvaluate,
  LblSearch,
  LblSort,
  LblString,
  LblArith,
  LblCompute,
};

struct cbl_proc_addresses_t {
  // This structure is used by 4; it very likely will never be
  // referenced elsewhere
  tree go_to;   // gg_append_statement(go_to) generates "goto label"
  tree label;   // gg_append_statement(label) generates "label:"
  tree addr;    // addr can be used as the right-hand-side of a "pointer = addr"
  tree decl;    // This is the decl used to create the other three
};

struct cbl_proc_t {
  struct cbl_label_t *label;
  struct cbl_proc_addresses_t top;
  struct cbl_proc_addresses_t exit;
  struct cbl_proc_addresses_t bottom;
  tree alter_location;  // The altered value if this paragraph is the target of an ALTER
};

struct cbl_label_addresses_t {
  // This structure is used by parser_label_label() and parser_label_goto()
  // It reuses the cbl_label_t *proc pointer; the meaning is clear from context
  tree go_to;   // gg_append_statement(go_to) generates "goto label"
  tree label;   // gg_append_statement(label) generates "label:"
};

struct cbl_refer_t;

static inline const char *
logop_str( enum logop_t logop ) {
  switch ( logop ) {
  case not_op: return "not";
  case and_op: return "and";
  case or_op: return "or";
  case xor_op: return "xor";
  case xnor_op: return "xnor";
  case true_op: return "true";
  case false_op: return "false";
  }
  return "???";
}

static inline const char *
relop_str( enum relop_t relop ) {
  switch ( relop ) {
  case lt_op:
    return "<";
  case le_op:
    return "<=";
  case eq_op:
    return "==";
  case ne_op:
    return "<>";
  case ge_op:
    return ">=";
  case gt_op:
    return ">";
  }
  return "???";
}

static inline const char *
setop_str( enum setop_t setop ) {
  switch ( setop ) {
  case is_op:
    return "is_op";
  }
  return "???";
}

struct cbl_substitute_t {
  enum subst_fl_t { subst_all_e, subst_first_e = 'F', subst_last_e = 'L'};
  bool anycase;
  subst_fl_t first_last;
  cbl_refer_t orig, replacement;

  cbl_substitute_t( bool anycase = false, char first_last = 0,
                    cbl_refer_t *orig = NULL, cbl_refer_t *replacement = NULL )
    : anycase(anycase)
    , first_last(subst_fl_t(first_last))
    , orig( orig? *orig : cbl_refer_t() )
    , replacement( replacement? *replacement : cbl_refer_t() )
  {}
};

static inline const char *
field_name( const cbl_field_t *f ) { return f? f->name : "(void)"; }

static inline const char *
field_name(const cbl_refer_t *r) { return r? field_name(r->field) : "(Nil)"; }

char * field_str( const cbl_field_t *field );

struct cbl_string_src_t {
  cbl_refer_t delimited_by; // identifier-2: BY SIZE indicated by NULL field
  size_t ninput;
  cbl_refer_t *inputs;      // identifier-1

  cbl_string_src_t( const cbl_refer_t& delimited_by,
                    size_t ninput, cbl_refer_t *inputs )
    : delimited_by(delimited_by)
    , ninput(ninput)
    , inputs(inputs)
    {}
};

struct cbl_num_result_t {
  enum cbl_round_t rounded;
  struct cbl_refer_t refer;

  static cbl_refer_t refer_of( const cbl_num_result_t& res ) { return res.refer; }
};

void parser_symbol_add( struct cbl_field_t *new_var );
void parser_local_add( struct cbl_field_t *new_var );

struct cbl_ffi_arg_t {
  bool optional;
  cbl_ffi_crv_t crv;
  cbl_ffi_arg_attr_t attr;
  cbl_refer_t refer; // refer::field == NULL is OMITTED

  cbl_ffi_arg_t( cbl_refer_t* refer = NULL,
                 cbl_ffi_arg_attr_t attr = none_of_e );
  cbl_ffi_arg_t( cbl_ffi_crv_t crv,
                 cbl_refer_t* refer,
                 cbl_ffi_arg_attr_t attr = none_of_e );
  cbl_field_t *field() { return refer.field; }
  void validate() const {
    if( refer.is_reference() ) {
      yyerror("%s is a reference", refer.field->name);
    }
    if( ! refer.field->has_attr(linkage_e) ) {
      yyerror("%s not found in LINKAGE SECTION", refer.field->name);
    }
    switch( refer.field->level ) {
    case 1: case 77:
      break;
    default:
      yyerror("%s must be LEVEL 01 or 77", refer.field->name);
    }
    // Update Linkage Section data item.
    refer.field->set_linkage(crv, optional);
  }
protected:
  bool by_value() const {
    if( crv == by_reference_e ) return false;
    return refer.field != NULL;
  }
};

// In support of serial/linear search:
struct cbl_lsearch_addresses_t {
  // This structure is used by linear_search
    struct cbl_label_addresses_t at_exit;  // The at_exit statements are at the top
    struct cbl_label_addresses_t top;      // Start of the loop of WHENS
    struct cbl_label_addresses_t bottom;   // The very bottom
};

struct cbl_lsearch_t {
  cbl_lsearch_addresses_t addresses;
  cbl_label_addresses_t jump_over;
  tree limit;
  tree counter;
  struct cbl_field_t *index;
  struct cbl_field_t *varying;
  bool first_when;
};

// This structure is used for binary searches:

struct cbl_bsearch_t {
    cbl_label_addresses_t too_small;
    cbl_label_addresses_t too_big;
    cbl_label_addresses_t top;
    cbl_label_addresses_t first_test;
    cbl_label_addresses_t bottom;
    tree left;  // This is a long
    tree right; // This is a long
    tree middle; // This is our copy of the index, so we only need to write
                 // it and never read it.
    tree compare_result; // This is an int, and avoids
    struct cbl_field_t *index;
    bool first_when;
};

struct cbl_unstring_t {
    cbl_label_addresses_t over;
    cbl_label_addresses_t into;
    cbl_label_addresses_t bottom;
};

// Used by RETURN instruction in SORT with output-procedure
struct cbl_sortreturn_t {
    cbl_label_addresses_t at_end;
    cbl_label_addresses_t not_at_end;
    cbl_label_addresses_t bottom;
};

struct cbl_call_exception_t {
    cbl_label_addresses_t over;
    cbl_label_addresses_t into;
    cbl_label_addresses_t bottom;
};

struct cbl_arith_error_t {
    cbl_label_addresses_t over;
    cbl_label_addresses_t into;
    cbl_label_addresses_t bottom;
};

struct cbl_compute_error_t {
    // This is an int.  The value is a cbl_compute_error_code_t
    tree compute_error_code;
};

struct cbl_label_t {
  enum cbl_label_type_t type;
  size_t parent;
  int line, used, lain;
  bool common, initial, recursive;
  size_t initial_section, returning;
  cbl_name_t name;
  const char *os_name, *mangled_name;
  union
    {
    // For performs, paragraphs, and sections:
    cbl_proc_t *proc;

    // For parser_label_label and parser_label_goto
    cbl_label_addresses_t *goto_trees;

    // For linear/serial search
    cbl_lsearch_t *lsearch;

    // For binary search
    cbl_bsearch_t *bsearch;

    // For UNSTRING search
    cbl_unstring_t *unstring;

    // for CALL [NOT] ON EXCEPTION
    struct cbl_call_exception_t *call_exception;

    // for arithmetic [NOT] ON SIZE_ERROR
    struct cbl_arith_error_t *arith_error;

    // for parser_op/parser_assign error tracking
    struct cbl_compute_error_t *compute_error;
    } structs;

  bool is_function() const { return type == LblFunction; }

  const char *type_str() const {
    switch(type) {
    case LblNone: return "LblNone";
    case LblProgram: return "LblProgram";
    case LblFunction: return "LblFunction";
    case LblSection: return "LblSection";
    case LblParagraph: return "LblParagraph";
    case LblLoop: return "LblLoop";
    case LblEvaluate: return "LblEvaluate";
    case LblSearch: return "LblSearch";
    case LblSort: return "LblSort";
    case LblString: return "LblString";
    case LblArith: return "LblArith";
    case LblCompute: return "LblCompute";
    }
    gcc_unreachable();
  }

  size_t explicit_parent() const;
  const char *str() const;
};

struct parser_tgt_t;

class cbl_label_ref_t {
  bool qualified;             // caller mentioned paragraph & section
  cbl_label_t *target;
  const cbl_label_t& context; // section called from
  int line;                   // point of reference
  parser_tgt_t *handle;
public:
  cbl_label_ref_t( size_t program, const cbl_label_t& context, int line,
                   const char name[], size_t isect = 0 );

  cbl_label_t * target_of() { return target; }

  parser_tgt_t * handle_of(parser_tgt_t *parser_tgt) {
    return this->handle = parser_tgt;
  }
  parser_tgt_t * handle_of() {
    return this->handle;
  }
};

static inline bool
label_lessthan( const cbl_label_t & a, const cbl_label_t & b ) {
  if ( a.type == LblNone || b.type == LblNone || a.type == b.type ) {
    return strcmp( a.name, b.name ) < 0;
  }
  return a.type < b.type;
}

static inline bool
operator<( const cbl_label_t & a, const cbl_label_t & b ) {
  return label_lessthan( a, b );
}

struct label_cmp_lessthan {
  bool operator() ( const cbl_label_t * a, const cbl_label_t * b ) {
    return label_lessthan( *a, *b );
  }
  bool operator() ( const cbl_label_t& a, const cbl_label_t& b ) {
    return label_lessthan( a, b );
  }
};

size_t field_index( const cbl_field_t *f );

cbl_field_t * new_temporary( enum cbl_field_type_t type, const char initial[] = NULL );
cbl_field_t * new_temporary_like( cbl_field_t skel );
cbl_field_t * new_temporary_clone( const cbl_field_t *orig);
cbl_field_t * keep_temporary( cbl_field_type_t type );

cbl_field_t * new_literal( uint32_t len, const char initial[],
                           enum cbl_field_attr_t attr = none_e );

void symbol_temporaries_free();

class temporaries_t {
  friend void symbol_temporaries_free();
  struct literal_an {
    bool is_quoted;
    std::string value;
    literal_an( const char value[] = "???", bool is_quoted = false )
      : is_quoted(is_quoted), value(value) {}
    literal_an& operator=( const literal_an& that ) {
      is_quoted = that.is_quoted;
      value = that.value;
      return *this;
    }
    bool operator<( const literal_an& that ) const {
      if( value == that.value ) { // alpha before numeric
        return (is_quoted? 0 : 1)  < (that.is_quoted? 0 : 1);
      }
      return value < that.value;
    }
  };

  std::map<literal_an, cbl_field_t *> literals;
  typedef std::set<cbl_field_t *> fieldset_t;
  typedef std::map<cbl_field_type_t, fieldset_t> fieldmap_t;
  fieldmap_t used, freed;

public:
  cbl_field_t * literal( const char value[], uint32_t len, cbl_field_attr_t attr  = none_e );
  cbl_field_t * reuse( cbl_field_type_t type );
  cbl_field_t * acquire( cbl_field_type_t type );
  cbl_field_t *  add( cbl_field_t *field );
  bool keep( cbl_field_t *field ) { return 1 == used[field->type].erase(field); }
  void dump() const;
  ~temporaries_t();
};


static inline bool is_table( const cbl_field_t *field ) {
  return field && field->occurs.ntimes() > 0;
}

static inline bool is_filler( const cbl_field_t *field ) {
  return field && 0 == strcasecmp("FILLER", field->name);
}

/*
 * CALL
 */

/*
 * Intrinsics
 */

enum cbl_intrinsic_trim_t {
  trim_none_e,
  trim_leading_e = 1,
  trim_trailing_e = 2,
};

enum cbl_ctype_t {
  c_unknown,
  c_bool,
  c_char,
  c_wchar,
  c_byte,
  c_ubyte,
  c_short,
  c_ushort,
  c_int,
  c_uint,
  c_long,
  c_ulong,
  c_longlong,
  c_ulonglong,
  c_size_t,
  c_ssize_t,
  c_int128,
  c_float,
  c_double,
  c_longdouble,
  c_char_p,
  c_wchar_p,
  c_void_p,
  c_nts,      // this is a null-terminated-string char_p
};

struct function_descr_arg_t {
  size_t isym;
  cbl_ffi_crv_t crv;
  bool optional;

  function_descr_arg_t()
    : isym(0), crv(by_default_e), optional(false)
  {}
  function_descr_arg_t( size_t isym, cbl_ffi_crv_t crv, bool optional )
    : isym(isym), crv(crv), optional(optional)
  {}
};

struct function_descr_t {
  int token;
  cbl_name_t name;
  char cname[48];
  char types[8];
  std::vector<function_descr_arg_t> linkage_fields;
  cbl_field_type_t ret_type;

  static function_descr_t init( const char name[] ) {
    function_descr_t descr = {};
    if( -1 == snprintf( descr.name, sizeof(descr.name), "%s", name ) ) {
      dbgmsg("name truncated to '%s' (max %zu characters)", name);
    }
    return descr;  // truncation also reported elsewhere ?
  }
  static function_descr_t init( int isym );

  static char
  parameter_type( const cbl_field_t& field ) {
    switch( field.type ) {
    case FldDisplay:
    case FldInvalid:
    case FldGroup:
    case FldLiteralA:
    case FldLiteralN:
    case FldClass:
    case FldConditional:
    case FldForward:
    case FldIndex:
    case FldSwitch:
    case FldBlob:
      return '?';
    case FldPointer:
      return 'O';
    case FldAlphanumeric:
      return field.has_attr(all_alpha_e)? 'A' : 'X';
    case FldPacked:
    case FldNumericDisplay:
    case FldNumericEdited:
    case FldAlphaEdited:
    case FldNumericBinary:
    case FldNumericBin5:
      return field.data.rdigits == 0? 'I' : 'N';
    case FldFloat:
      return 'N';
    }
    gcc_unreachable();
  }

  bool operator<( const function_descr_t& that ) const {
    return strcasecmp(name, that.name) < 0;
  }
  bool operator==( const function_descr_t& that ) const {
    return strcasecmp(name, that.name) == 0;
  }
  bool operator==( const char *name ) const {
    return strcasecmp(this->name, name) == 0;
  }
};

enum cbl_section_type_t {
  file_sect_e,
  working_sect_e,
  linkage_sect_e,
  local_sect_e,
};

struct cbl_section_t {
  cbl_section_type_t type;
  int line;
  void * node;

  const char * name() const {
    switch(type) {
    case file_sect_e:    return "file_sect_e";
    case working_sect_e: return "working_sect_e";
    case linkage_sect_e: return "linkage_sect_e";
    case local_sect_e:   return "local_sect_e";
    }
    gcc_unreachable();
  }
  uint32_t attr() const {
    switch(type) {
    case file_sect_e:
    case working_sect_e: return 0;
    case linkage_sect_e: return linkage_e;
    case local_sect_e:   return local_e;
    }
    gcc_unreachable();
  }
};

struct cbl_special_name_t {
  int token;
  enum special_name_t id;
  cbl_name_t name;
  size_t filename;
  char os_filename[16]; // short because always in /dev
};

char * hex_decode( const char text[] );

struct cbl_alphabet_t {
  YYLTYPE loc;
  cbl_name_t name;
  cbl_encoding_t encoding;
  unsigned char low_index, high_index, last_index, alphabet[256];

  cbl_alphabet_t()
    : loc { 1,1, 1,1 }
    , encoding(ASCII_e)
    , low_index(0)
    , high_index(255)
    , last_index(0)
  {
    memset(name, '\0', sizeof(name));
    memset(alphabet, 0xFF, sizeof(alphabet));
  }

  cbl_alphabet_t(const YYLTYPE& loc, cbl_encoding_t enc)
    : loc(loc)
    , encoding(enc)
    , low_index(0)
    , high_index(255)
    , last_index(0)
  {
    memset(name, '\0', sizeof(name));
    memset(alphabet, 0xFF, sizeof(alphabet));
  }

  cbl_alphabet_t( const YYLTYPE& loc, const cbl_name_t name,
                  unsigned char low_index, unsigned char high_index,
                  unsigned char alphabet[] )
    : loc(loc)
    , encoding(custom_encoding_e)
    , low_index(low_index), high_index(high_index)
    , last_index(high_index)
  {
    assert(strlen(name) < sizeof(this->name));
    strcpy(this->name, name);
    std::copy(alphabet, alphabet + sizeof(this->alphabet), this->alphabet);
  }

  unsigned char low_value() const {
    return alphabet[low_index];
  }
  unsigned char high_value() const {
    return alphabet[high_index];
  }

  void
  add_sequence( const YYLTYPE& loc, const unsigned char seq[] ) {
    if( low_index == 0 ) low_index = seq[0];

    unsigned char high_value = last_index > 0? alphabet[last_index] + 1 : 0;

    for( const unsigned char *p = seq; !end_of_string(p); p++  ) {
      assign(loc, *p, high_value++);
    }
  }

  void
  add_interval( const YYLTYPE& loc, unsigned char low, unsigned char high ) {
    if( low_index == 0 ) low_index = low;

    unsigned char high_value = alphabet[last_index];

    for( unsigned char ch = low; ch < high; ch++  ) {
      assign(loc, ch, high_value++);
    }
  }

  void also( const YYLTYPE& loc, size_t ch );
  bool assign( const YYLTYPE& loc, unsigned char ch, unsigned char value );

  static const char *
  encoding_str( cbl_encoding_t encoding ) {
    switch(encoding) {
    case ASCII_e:  return "ascii";
    case iso646_e: return "iso646";
    case EBCDIC_e: return "ebcdic";
    case custom_encoding_e: return "custom";
    }
    return "???";
  }

  void dump() const {
    yywarn("'%s': %s, '%c' to '%c' (low 0x%02x, high 0x%02x)",
          name, encoding_str(encoding),
          low_index, last_index, low_index, high_index);
    if( encoding == custom_encoding_e ) {
      fprintf(stderr, "\t"
               "  0   1   2   3   4   5   6   7"
              "   8   9   A   B   C   C   E   F");
      unsigned int row = 0;
      for( auto p = alphabet; p < alphabet + sizeof(alphabet); p++ ) {
        if( (p - alphabet) % 16 == 0 ) fprintf(stderr, "\n%4X\t", row++);
        fprintf(stderr, "%3u ", *p);
      }
      fprintf(stderr, "\n");
    }
  }
  static unsigned char nul_string[2];

 protected:
  static inline bool end_of_string( const unsigned char *p ) {
    return p != nul_string && *p == '\0';
  }
};

// a function pointer
typedef void ( *cbl_function_ptr ) ( void );

struct cbl_function_t {
  char name[NAME_MAX];
  cbl_function_ptr func;
};

static inline const char *
file_org_str( enum cbl_file_org_t org ) {
  switch ( org ) {
  case file_disorganized_e:    return "DISORGANIZED";
  case file_sequential_e:      return "SEQUENTIAL";
  case file_line_sequential_e: return "LINE_SEQUENTIAL";
  case file_indexed_e:         return "INDEXED";
  case file_relative_e:        return "RELATIVE";
  }
  return "???";
}

enum file_entry_type_t { fd_e, sd_e };

static inline const char *
file_access_str( cbl_file_access_t access  ) {
  switch(access) {
  case file_inaccessible_e: return "INACCESSIBLE";
  case file_access_seq_e:   return "SEQUENTIAL";
  case file_access_rnd_e:   return "RANDOM";
  case file_access_dyn_e:   return "DYNAMIC";
  }
  return "???";
}

enum declarative_culprit_t {
  culpa_none_e,
  culpa_input_e  = 0x01,
  culpa_output_e = 0x02,
  culpa_io_e     = 0x03,  // both input and output
  culpa_extend_e = 0x04,
};

struct cbl_file_key_t {
  bool unique;
  cbl_name_t name;
  size_t leftmost; // START or READ named leftmost field in key
  size_t nfield;
  size_t *fields;

  cbl_file_key_t( size_t field = 0, bool unique = true )
    : unique(unique)
    , leftmost(0)
    , nfield(1)
    , fields( new size_t[nfield] )
  {
    fields[0] = field;
    memset(name, '\0', sizeof(name));
  }
  cbl_file_key_t( const cbl_file_key_t *that )
    : unique(that->unique)
    , leftmost(that->leftmost)
    , nfield(that->nfield)
  {
    memcpy(name, that->name, sizeof(name));
    fields = new size_t[nfield];
    std::copy( that->fields, that->fields + that->nfield, fields );
  }

  cbl_file_key_t( cbl_name_t name,
                  const std::list<cbl_field_t *>& fields,
                  bool is_unique );

  uint32_t size();
  void deforward( size_t ifile );
  char * str() const;
  bool operator==( const cbl_field_t *key_field ); // not const, may set leftmost

 protected:
  static uint32_t key_field_size( uint32_t sum, size_t ifield );
  size_t offset() const;
};

struct cbl_file_lock_t {
  bool multiple;
  enum lock_mode_t { unlocked_e, manual_e, record_e, automatic_e } mode;
  bool mode_set( int token );
  bool locked() const { return mode != unlocked_e; }
};

struct cbl_file_t {
  static cbl_file_key_t no_key;
  enum cbl_file_org_t org;
  enum file_entry_type_t entry_type;
  uint32_t attr;
  size_t reserve, same_record_as;
  char   padding;
  bool   optional;
  // varying_size::explicitly is TRUE if if RECORD has VARYING or CONTAINS x TO y
  struct varying_t { bool explicitly; size_t min, max; } varying_size;
  cbl_file_lock_t lock;
  // "The RECORD DELIMITER clause is syntax checked, but has no effect
  //  on the execution of the program."
  enum cbl_file_access_t access;
  size_t filename;      //
  size_t default_record;
  size_t nkey;          // 1st key is primary & unique
  cbl_file_key_t *keys; // indexes into symbol table for key field(s)
  size_t password;      // index into symbol table for password (!)
  size_t user_status;   // index into symbol table for file status
  size_t vsam_status;   // index into symbol table for vsam status PIC X(6)
  size_t record_length; // DEPENDS ON
  int line;
  cbl_name_t name;
  cbl_sortreturn_t *addresses; // Used during parser_return_start, et al.
  tree var_decl_node;           // GENERIC tag for the run-time FIELD structure

  cbl_file_t()
    : org(file_disorganized_e),
      access(file_access_seq_e)
  {
    keys = &no_key;
  }
  
  bool varies() const { return varying_size.min != varying_size.max; }
  bool validate() const;
  void deforward();
  char * keys_str() const;
  int  key_one( cbl_field_t *field ) const {
    auto ekey = keys + nkey, p = ekey;
    if( (p = std::find(keys, ekey, field)) == ekey ) return 0;
    return (p - keys) + 1;
  }
  bool relative_sequential() const {
    return org == file_relative_e && access == file_access_seq_e;
  }
  bool indexed_sequential() const {
    return org == file_indexed_e && access == file_access_seq_e;
  }
  void consider_for_default( const cbl_field_t *record );
 protected:
  bool validate_forward( size_t isym ) const;
  bool validate_key( const cbl_file_key_t& key ) const;
};

static inline bool
is_sequential( const cbl_file_t *file ) {
  assert(file);
  switch(file->org) {
  case file_sequential_e:
  case file_line_sequential_e:
    return true;
  case file_disorganized_e:
  case file_indexed_e:
  case file_relative_e:
    break;
  }
  return false;
}

struct symbol_elem_t {
  enum symbol_type_t type;
  size_t program;
  union symbol_elem_u {
    char *filename;
    cbl_function_t     function;
    cbl_field_t        field;
    cbl_label_t        label;
    cbl_special_name_t special;
    cbl_alphabet_t     alphabet;
    cbl_file_t         file;
    cbl_section_t      section;
    symbol_elem_u() {
      static const cbl_field_t empty = {};
      field = empty;
    }
  } elem;
  
  symbol_elem_t( symbol_type_t type = SymField, size_t program = 0 )
    : type(type), program(program)
  {}

  symbol_elem_t( const symbol_elem_t& that )
    : type(that.type), program(that.program)
  {
    copy_by_type(that);
  }
  symbol_elem_t& operator=( const symbol_elem_t& that ) {
    type = that.type;
    program = that.program;
    return copy_by_type(that);
  }
  explicit symbol_elem_t( size_t program, const cbl_field_t& field )
    : type(SymField), program(program)
  {
    elem.field = field;
  }
  explicit symbol_elem_t( size_t program, const cbl_label_t& label )
    : type(SymLabel), program(program)
  {
    elem.label = label;
  }
  explicit symbol_elem_t( size_t program, const cbl_special_name_t& special )
    : type(SymSpecial), program(program)
  {
    elem.special = special;
  }
  explicit symbol_elem_t( size_t program, const cbl_section_t& section )
    : type(SymDataSection), program(program)
  {
    elem.section = section;
  }
  
 protected:
  symbol_elem_t& copy_by_type( const symbol_elem_t& that ) {
    switch(type) {
    case SymFilename:
      elem.filename = that.elem.filename;
      break;
    case SymFunction:
      elem.function = that.elem.function;
      break;
    case SymField:
      elem.field = that.elem.field;
      break;
    case SymLabel:
      elem.label = that.elem.label;
      break;
    case SymSpecial:
      elem.special = that.elem.special;
      break;
    case SymAlphabet:
      elem.alphabet = that.elem.alphabet;
      break;
    case SymFile:
      elem.file = that.elem.file;
      break;
    case SymDataSection:
      elem.section = that.elem.section;
      break;
    }
    return *this;
  }
};

# define offsetof(TYPE, MEMBER)  __builtin_offsetof (TYPE, MEMBER)

static inline symbol_elem_t *
symbol_elem_of( cbl_label_t *label ) {
  size_t n = offsetof(struct symbol_elem_t, elem.label);
  return
    reinterpret_cast<struct symbol_elem_t *>((char*)label - n);
}

static inline const symbol_elem_t *
symbol_elem_of( const cbl_label_t *label ) {
  size_t n = offsetof(symbol_elem_t, elem.label);
  return
    reinterpret_cast<const symbol_elem_t *>((const char*)label - n);
}

static inline symbol_elem_t *
symbol_elem_of( cbl_special_name_t *special ) {
  size_t n = offsetof(symbol_elem_t, elem.special);
  return
    reinterpret_cast<symbol_elem_t *>((char*)special - n);
}

static inline symbol_elem_t *
symbol_elem_of( cbl_alphabet_t *alphabet ) {
  size_t n = offsetof(symbol_elem_t, elem.alphabet);
  return
    reinterpret_cast<symbol_elem_t *>((char*)alphabet - n);
}

static inline symbol_elem_t *
symbol_elem_of( cbl_file_t *file ) {
  size_t n = offsetof(struct symbol_elem_t, elem.file);
  return
    reinterpret_cast<struct symbol_elem_t *>((char*)file - n);
}
static inline const symbol_elem_t *
symbol_elem_of( const cbl_file_t *file ) {
  size_t n = offsetof(symbol_elem_t, elem.file);
  return
    reinterpret_cast<const symbol_elem_t *>((const char*)file - n);
}

static inline symbol_elem_t *
symbol_elem_of( cbl_field_t *field ) {
  size_t n = offsetof(struct symbol_elem_t, elem.field);
  return
    reinterpret_cast<struct symbol_elem_t *>((char*)field - n);
}
static inline const symbol_elem_t *
symbol_elem_of( const cbl_field_t *field ) {
  size_t n = offsetof(symbol_elem_t, elem.field);
  return
    reinterpret_cast<const symbol_elem_t *>((const char*)field - n);
}

symbol_elem_t * symbols_begin( size_t first = 0 );
symbol_elem_t * symbols_end(void);
cbl_field_t   * symbol_redefines( const struct cbl_field_t *field );

void build_symbol_map();
bool update_symbol_map( symbol_elem_t *e );

void update_symbol_map2( const symbol_elem_t *elem );
void finalize_symbol_map2();
void dump_symbol_map2();

symbol_elem_t * symbol_register( const char name[] );

std::pair<symbol_elem_t *, bool>
symbol_find( size_t program, std::list<const char *> names );
symbol_elem_t * symbol_find_of( size_t program,
                                std::list<const char *> names, size_t group );

struct cbl_field_t *symbol_find_odo( cbl_field_t * field );
size_t dimensions( const cbl_field_t *field );

const symbol_elem_t * symbol_field_current_record();
const symbol_elem_t * symbol_field_alias_begin();
void symbol_field_alias_end();

typedef std::map< size_t, size_t > corresponding_fields_t;

corresponding_fields_t
corresponding_arith_fields( cbl_field_t *lhs, cbl_field_t *rhs );
corresponding_fields_t
corresponding_move_fields( cbl_field_t *lhs, cbl_field_t *rhs );

typedef std::set<size_t> symbolset_t;

symbolset_t symbol_program_programs();
symbolset_t symbol_program_callables( size_t program );
const cbl_label_t * symbol_program_local( const char called[] );

bool redefine_field( cbl_field_t *field );

// Functions to correctly extract the underlying type.
static inline struct cbl_function_t *
cbl_function_of( struct symbol_elem_t *e ) {
  assert(e->type == SymFunction);
  return &e->elem.function;
}

static inline struct cbl_section_t *
cbl_section_of( struct symbol_elem_t *e ) {
  assert(e->type == SymDataSection);
  return &e->elem.section;
}

static inline struct cbl_field_t *
cbl_field_of( struct symbol_elem_t *e ) {
  assert(e->type == SymField);
  return &e->elem.field;
}
static inline const struct cbl_field_t *
cbl_field_of( const struct symbol_elem_t *e ) {
  assert(e->type == SymField);
  return &e->elem.field;
}

static inline struct cbl_label_t *
cbl_label_of( struct symbol_elem_t *e ) {
  assert(e->type == SymLabel);
  return &e->elem.label;
}

static inline const struct cbl_label_t *
cbl_label_of( const struct symbol_elem_t *e ) {
  assert(e->type == SymLabel);
  return &e->elem.label;
}

static inline struct cbl_special_name_t *
cbl_special_name_of( struct symbol_elem_t *e ) {
  assert(e->type == SymSpecial);
  return &e->elem.special;
}

static inline struct cbl_alphabet_t *
cbl_alphabet_of( struct symbol_elem_t *e ) {
  assert(e->type == SymAlphabet);
  return &e->elem.alphabet;
}

static inline struct cbl_file_t *
cbl_file_of( struct symbol_elem_t *e ) {
  assert(e->type == SymFile);
  return &e->elem.file;
}

static inline const struct cbl_file_t *
cbl_file_of( const struct symbol_elem_t *e ) {
  assert(e->type == SymFile);
  return &e->elem.file;
}

static inline bool
is_program( const symbol_elem_t& e ) {
  return e.type == SymLabel &&
    (cbl_label_of(&e)->type == LblProgram ||
     cbl_label_of(&e)->type == LblFunction);
}

static inline bool
is_procedure( const symbol_elem_t& e ) {
  return e.type == SymLabel &&
    (cbl_label_of(&e)->type == LblParagraph ||
     cbl_label_of(&e)->type == LblSection);
}

static inline bool
is_figconst(const struct cbl_field_t *field ) {
    return ((field->attr & FIGCONST_MASK) != 0 );
}

static inline bool
is_figconst_low( const struct cbl_field_t *field ) {
  return ((field->attr & FIGCONST_MASK) == low_value_e );
}

static inline bool
is_figconst_zero( const struct cbl_field_t *field ) {
  return ((field->attr & FIGCONST_MASK) == zero_value_e );
}

static inline bool
is_figconst_space( const struct cbl_field_t *field ) {
  return ((field->attr & FIGCONST_MASK) == space_value_e );
}

static inline bool
is_figconst_quote( const struct cbl_field_t *field ) {
  return ((field->attr & FIGCONST_MASK) == quote_value_e );
}

static inline bool
is_figconst_high( const struct cbl_field_t *field ) {
  return ((field->attr & FIGCONST_MASK) == high_value_e );
}

static inline bool
is_space_value( const struct cbl_field_t *field ) {
  return( (strcmp(field->name, "SPACE") == 0)
            || (strcmp(field->name, "SPACES") == 0) );
}

static inline bool
is_quoted( const struct cbl_field_t *field ) {
  return field->has_attr(quoted_e);
}

/*
 * PERFORM support
 *
 * cbl_until_addresses_t has the goto/label pairs needed to implement the
 * PERFORM UNTIL/VARYING/TIMES possibilities
 */

#define MAXIMUM_UNTILS 64  // This was one VARYING and four AFTERs

struct cbl_until_addresses_t {
  // This structure is used by parser_perform_start() and parser_perform_until
    struct cbl_label_addresses_t top;   // The very top of the loop
    struct cbl_label_addresses_t exit;  // The implied continue at the bottom
    struct cbl_label_addresses_t test;  // The test at the bottom of the body
    struct cbl_label_addresses_t testA; // Starting point of a TEST_AFTER loop
    struct cbl_label_addresses_t setup; // The actual entry point
    size_t number_of_conditionals;
    struct cbl_label_addresses_t condover[MAXIMUM_UNTILS];  // Jumping over the conditional
    struct cbl_label_addresses_t condinto[MAXIMUM_UNTILS];  // Jumping into the conditional
    struct cbl_label_addresses_t condback[MAXIMUM_UNTILS];  // Jumping back from the conditional
    int    line_number_of_setup_code; // This is needed to thwart the too-helpful compiler
};

size_t symbol_index(); // nth after first program symbol
size_t symbol_index( const struct symbol_elem_t *e );
struct symbol_elem_t * symbol_at( size_t index );

struct cbl_options_t {
  enum arith_t {
    native_e,
    standard_e,
    standard_binary_e,
    standard_decimal_e,
  } arith;
  enum float_endidanism_t {
    high_order_left_e,
    high_order_right_e,
  } binary_endidanism, decimal_endidanism;
  enum float_encoding_t {
    binary_encoding_e,
    decimal_encoding_e,
  } float_encoding;

  cbl_round_t default_round, intermediate_round;

  struct initialize_t {
    ssize_t working, local;
    initialize_t() : working(-1), local(-1) {}
  } initial_value;

  cbl_options_t()
    : arith(cbl_options_t::native_e)
    , binary_endidanism(cbl_options_t::high_order_right_e)
    , decimal_endidanism(cbl_options_t::high_order_right_e)
    , float_encoding(cbl_options_t::binary_encoding_e)
    , default_round(nearest_away_from_zero_e)
    , intermediate_round(nearest_away_from_zero_e)
    {}
  cbl_field_t * initial_working() const {
    return initial_value.working < 0? nullptr :
      cbl_field_of(symbol_at(initial_value.working));
  }
  cbl_field_t * initial_local() const {
    return initial_value.local < 0? nullptr :
      cbl_field_of(symbol_at(initial_value.local));
  }
};
cbl_options_t current_options();

struct symbol_elem_t *
symbol_field_forward_add( size_t program, size_t parent,
                          const char name[], int line );

struct cbl_field_t * symbol_field_forward( size_t index );

struct cbl_prog_hier_t {
  size_t nlabel;
  struct program_label_t {
    size_t ordinal;
    cbl_label_t label;
    program_label_t() : ordinal(0) {}
    program_label_t( const symbol_elem_t& e ) {
      ordinal = symbol_index(&e);
      label = e.elem.label;
    }
  } *labels;

  cbl_prog_hier_t();
};

/*
 * cbl_perform_tgt_t has from and to: the 1st and last labels to be performed.
 * When only one label is being performed (no "thru"), "to" is NULL.
 * In the case of an inline perform, "from" points to a label of type LblLoop.
 */
struct cbl_perform_tgt_t {
  struct cbl_until_addresses_t addresses;

  cbl_perform_tgt_t() : ifrom(0), ito(0) {}
  cbl_perform_tgt_t( cbl_label_t * from, cbl_label_t *to = NULL )
    : ifrom( from? symbol_index(symbol_elem_of(from)) : 0 )
    , ito( to? symbol_index(symbol_elem_of(to)) : 0 )
  {
    addresses = {};
  }

  cbl_label_t * from( cbl_label_t * label ) {
    ifrom = symbol_index(symbol_elem_of(label));
    return from();
  }
  cbl_label_t * finally( size_t program );

  cbl_label_t * from() const {
    return ifrom? cbl_label_of(symbol_at(ifrom)) : NULL;
  }
  cbl_label_t * to() const {
    return ito? cbl_label_of(symbol_at(ito)) : NULL;
  }


  void dump() const {
    assert(ifrom);
    if( !ito ) {
      dbgmsg( "%s:%d: #%3zu %s", __PRETTY_FUNCTION__, __LINE__,
             ifrom, from()->str() );
    } else {
      dbgmsg( "%s:%d: #%3zu %s THRU #%3zu %s", __PRETTY_FUNCTION__, __LINE__,
             ifrom, from()->str(), ito, to()->str() );
    }
  }

  protected:
    size_t ifrom, ito;
};

struct cbl_perform_vary_t {
  struct cbl_refer_t  varying;  // numeric
  struct cbl_refer_t  from;     // numeric
  struct cbl_refer_t  by;       // numeric
  struct cbl_field_t *until;    // FldConditional

  cbl_perform_vary_t( const cbl_refer_t& varying = cbl_refer_t(),
                      const cbl_refer_t& from = cbl_refer_t(),
                      const cbl_refer_t& by = cbl_refer_t(),
                      cbl_field_t *until = NULL )
    : varying(varying)
    , from(from)
    , by(by)
    , until(until)
  {}
};

bool is_global( const cbl_field_t * field );

static inline bool
is_literal( const cbl_field_type_t type ) {
  return     type == FldLiteralA
          || type == FldLiteralN;
}

static inline bool
is_literal( const cbl_field_t *field ) {
  return is_literal(field->type);
}

static inline bool
is_signable( const struct cbl_field_t *field ) {
  return field->attr & signable_e;
}

static inline bool
is_temporary( const struct cbl_field_t *field ) {
  return field->attr & intermediate_e;
}

bool has_value( cbl_field_type_t type );


static inline bool
is_numeric( const cbl_field_t *field ) {
  assert( field );
  bool is_zero = zero_value_e == (field->attr & zero_value_e);
  return is_zero || is_numeric(field->type);
}

/*
 * Public functions
 */

bool cobol_filename( const char *name );
const char * cobol_filename();

const char * cobol_fileline_set( const char line[] );

char *cobol_name_mangler(const char *cobol_name);

bool is_elementary( enum cbl_field_type_t type );
bool is_numeric_edited( const char picture[] );

const char * intrinsic_function_name( int token );

char date_time_fmt( const char input[] );

size_t current_program_index();
const char * current_declarative_section_name();

struct cbl_nameloc_t {
  YYLTYPE loc;
  const char *name;

  cbl_nameloc_t() : loc{ 1,1, 1,1 }, name(NULL) {}
  cbl_nameloc_t( const YYLTYPE& loc, const char *name )
    : loc(loc), name(name)
  {}
};

/*
 * The lexer pushes qualified names unilaterally, regardless of the
 * state of the parser, because it runs ahead of the parser. The
 * parser adds to the queue conditionally, only if the lexer has not.
 * The parser consumes a queue element (a name list) whenever it looks
 * up a name, e.g. on the way to producing a scalar.
 */
#include <queue>
typedef std::list<const char *> cbl_namelist_t;
typedef std::list<cbl_nameloc_t> cbl_namelocs_t;
class name_queue_t : private std::queue<cbl_namelocs_t>
{
  friend void tee_up_empty();
  cbl_namelocs_t recent;

  void allocate() {
    std::queue<cbl_namelocs_t>::push( cbl_namelocs_t() );
  }
 public:
  static cbl_namelist_t
  namelist_of( const cbl_namelocs_t& namelocs ) {
    cbl_namelist_t names;
    std::transform( namelocs.begin(), namelocs.end(), std::back_inserter(names),
                    []( const cbl_nameloc_t& nameloc ) {
                      return nameloc.name;
                    } );
    return names;
  }
  size_t push( const YYLTYPE& loc, const char name[] ) {
    assert( !empty() );
    back().push_front( cbl_nameloc_t(loc, name) );
    dump(__func__);
    return size();
  }
  void qualify( const YYLTYPE& loc, const char name[] ) {
    if( empty() ) {
      allocate();
      push(loc, name);
    } else {
      back().push_front( cbl_nameloc_t(loc, name) );
    }
    dump(__func__);
  }
  cbl_namelocs_t pop() {
    assert(!empty());
    recent = front();
    std::queue<cbl_namelocs_t>::pop();
    dump(__func__);
    return recent;
  }
  cbl_namelist_t pop_as_names() {
    return namelist_of(pop());
  }

  void dump( const char tag[] ) const;

  cbl_namelocs_t peek() const { dump(__func__); return empty()? recent : back(); }

  bool  empty() const { return std::queue<cbl_namelocs_t>::empty(); }
  size_t size() const { return std::queue<cbl_namelocs_t>::size(); }

};

void tee_up_empty();
void tee_up_name( const YYLTYPE& loc, const char name[] );
cbl_namelist_t teed_up_names();

size_t end_of_group( size_t igroup );

struct symbol_elem_t * symbol_typedef( size_t program, std::list<const char *> names );
struct symbol_elem_t * symbol_typedef( size_t program, const char name[] );
struct symbol_elem_t * symbol_field( size_t program,
                                     size_t parent, const char name[] );
struct cbl_label_t *   symbol_program( size_t parent, const char name[] );
struct cbl_label_t *   symbol_label( size_t program, cbl_label_type_t type,
                                     size_t section, const char name[],
                                     const char os_name[] = NULL );
struct symbol_elem_t * symbol_function( size_t parent, const char name[] );

struct symbol_elem_t * symbol_literalA( size_t program, const char name[] );

struct cbl_special_name_t * symbol_special( special_name_t id );
struct symbol_elem_t * symbol_special( size_t program, const char name[] );
struct symbol_elem_t * symbol_alphabet( size_t program, const char name[] );

struct symbol_elem_t * symbol_file( size_t program, const char name[] );
struct cbl_field_t   * symbol_file_record( struct cbl_file_t *file );
cbl_file_t::varying_t symbol_file_record_sizes( struct cbl_file_t *file );
struct cbl_section_t * symbol_section( size_t program,
                                       struct cbl_section_t *section );

size_t symbol_label_id( const cbl_label_t *label );

struct cbl_field_t * parent_of( const cbl_field_t *f );
 const cbl_field_t * occurs_in( const cbl_field_t *f );

cbl_field_t *rename_not_ok( cbl_field_t *first, cbl_field_t *last);
bool immediately_follows( const cbl_field_t *first );
bool is_variable_length( const cbl_field_t *field );

cbl_file_t * symbol_record_file( const cbl_field_t *f );

struct cbl_field_t * symbol_find_odo( const cbl_field_t * field );

size_t numeric_group_attrs( const cbl_field_t *field );

static inline struct cbl_field_t *
field_at( size_t index ) {
  struct symbol_elem_t *e = symbol_at(index);
  assert(e->type == SymField);

  return &e->elem.field;
}

bool   symbols_alphabet_set( size_t program, const char name[]);

size_t symbols_update( size_t first, bool parsed_ok = true );

void symbol_table_init(void);
void symbol_table_check(void);

struct symbol_elem_t * symbol_typedef_add( size_t program,
                                           struct cbl_field_t *field );
struct symbol_elem_t * symbol_field_add( size_t program,
                                         struct cbl_field_t *field );
struct cbl_label_t *   symbol_label_add( size_t program,
                                         struct cbl_label_t *label );
struct cbl_label_t *   symbol_program_add( size_t program, cbl_label_t *input );
struct symbol_elem_t * symbol_special_add( size_t program,
                                         struct cbl_special_name_t *special );
struct symbol_elem_t * symbol_alphabet_add( size_t program,
                                         struct cbl_alphabet_t *alphabet );
struct symbol_elem_t * symbol_file_add( size_t program,
                                         struct cbl_file_t *file );
struct symbol_elem_t * symbol_section_add( size_t program,
                                         struct cbl_section_t *section );

void symbol_field_location( size_t ifield, const YYLTYPE& loc );
YYLTYPE symbol_field_location( size_t ifield );

bool symbol_label_section_exists( size_t program );

size_t symbol_field_capacity( const cbl_field_t *field );

size_t file_status_register();
size_t return_code_register();
size_t very_true_register();
size_t very_false_register();
size_t ec_register();

static inline size_t upsi_register() {
  return symbol_index(symbol_field(0,0,"UPSI-0"));
}

void wsclear( char ch);
const char *wsclear();

enum cbl_call_convention_t {
  cbl_call_verbatim_e = 'V',
  cbl_call_cobol_e = 'N', // native
};

cbl_call_convention_t current_call_convention();

cbl_call_convention_t
current_call_convention( cbl_call_convention_t convention);

class procref_base_t {
private:
  const char *section_name, *paragraph_name;
public:
  procref_base_t( const char *section_name = NULL,
                  const char *paragraph_name = NULL )
    : section_name(section_name)
    , paragraph_name(paragraph_name)
  {}
  procref_base_t( const procref_base_t& that )
    : section_name(that.section_name)
    , paragraph_name(that.paragraph_name)
  {}

  bool operator<( const procref_base_t& that ) const;
  bool operator==( const procref_base_t& that ) const;

  const char *section() const { return section_name? section_name : ""; }
  const char *paragraph() const { return paragraph_name? paragraph_name : ""; }

  bool has_section() const { return section_name != NULL; }
  bool has_paragraph() const { return paragraph_name != NULL; }
};

class procref_t : public procref_base_t {
  int line;
  size_t context; // section called from
public:
  procref_t( const char *section, const char *paragraph, int line, size_t context )
    : procref_base_t(section, paragraph)
    , line(line)
    , context(context)
  {
    assert(line);
    assert(context == 0 || cbl_label_of(symbol_at(context))->type == LblSection);
  }

  int line_number() const { return line; }
};

int keyword_tok( const char * text, bool include_intrinsics = false );
int redefined_token( const cbl_name_t name );

void procedure_definition_add( size_t program, const cbl_label_t *procedure );
void procedure_reference_add( const char *sect, const char *para,
                              int line, size_t context );
procref_t * ambiguous_reference( size_t program );

struct symbol_elem_t *
symbol_field_alias( struct symbol_elem_t *e, const char name[] );
struct symbol_elem_t *
symbol_field_alias2( struct symbol_elem_t *e,
                     struct symbol_elem_t *e2, const char name[] );
struct symbol_elem_t *
symbol_field_same_as( cbl_field_t *tgt, const cbl_field_t *src );

size_t symbol_file_same_record_area( std::list<cbl_file_t*>& files );

cbl_field_t *
symbol_valid_udf_args( size_t function,
                       std::list<cbl_refer_t> args = std::list<cbl_refer_t>() );

bool symbol_currency_add( const char symbol[], const char sign[] = NULL );
const char * symbol_currency( char symbol );

const char * symbol_type_str( enum symbol_type_t type );
const char * cbl_field_type_str( enum cbl_field_type_t type );
const char * cbl_logop_str( enum logop_t op );

static inline const char *
refer_type_str( const cbl_refer_t *r ) {
  return r && r->field? cbl_field_type_str(r->field->type) : "(none)";
}

enum cbl_field_type_t symbol_field_type( size_t program, const char name[] );

struct symbol_elem_t * symbol_parent( const struct symbol_elem_t *e );

int length_of_picture(const char *picture);
int rdigits_of_picture(const char *picture);
int  digits_of_picture(const char *picture, bool for_rdigits);
bool is_picture_scaled(const char *picture);

template <typename LOC>
void gcc_location_set( const LOC& loc );

// This is slightly oddball.  This is an entry point in the charutf8.cc module.
// It's the only entry point in the module, and so it seemed to me wasteful to
//  create an entire .h module.  So, I stuck it here.
size_t count_characters(const char *in, size_t length);

#endif
