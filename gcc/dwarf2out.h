/* dwarf2out.h - Various declarations for functions found in dwarf2out.cc
   Copyright (C) 1998-2025 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_DWARF2OUT_H
#define GCC_DWARF2OUT_H 1

#include "dwarf2.h"	/* ??? Remove this once only used by dwarf2foo.c.  */

typedef struct die_struct *dw_die_ref;
typedef const struct die_struct *const_dw_die_ref;

typedef struct dw_val_node *dw_val_ref;
typedef struct dw_cfi_node *dw_cfi_ref;
typedef struct dw_loc_descr_node *dw_loc_descr_ref;
typedef struct dw_loc_list_struct *dw_loc_list_ref;
typedef struct dw_discr_list_node *dw_discr_list_ref;
typedef struct dw_wide_int *dw_wide_int_ptr;


/* Call frames are described using a sequence of Call Frame
   Information instructions.  The register number, offset
   and address fields are provided as possible operands;
   their use is selected by the opcode field.  */
enum dw_cfi_oprnd_type: int {
  dw_cfi_oprnd_unused,
  dw_cfi_oprnd_reg_num,
  dw_cfi_oprnd_offset,
  dw_cfi_oprnd_addr,
  dw_cfi_oprnd_loc,
  dw_cfi_oprnd_cfa_loc
};

typedef union GTY(()) {
  unsigned int GTY ((tag ("dw_cfi_oprnd_reg_num"))) dw_cfi_reg_num;
  HOST_WIDE_INT GTY ((tag ("dw_cfi_oprnd_offset"))) dw_cfi_offset;
  const char * GTY ((tag ("dw_cfi_oprnd_addr"))) dw_cfi_addr;
  struct dw_loc_descr_node * GTY ((tag ("dw_cfi_oprnd_loc"))) dw_cfi_loc;
  struct dw_cfa_location * GTY ((tag ("dw_cfi_oprnd_cfa_loc")))
    dw_cfi_cfa_loc;
} dw_cfi_oprnd;

struct GTY(()) dw_cfi_node {
  enum dwarf_call_frame_info dw_cfi_opc;
  dw_cfi_oprnd GTY ((desc ("dw_cfi_oprnd1_desc (%1.dw_cfi_opc)")))
    dw_cfi_oprnd1;
  dw_cfi_oprnd GTY ((desc ("dw_cfi_oprnd2_desc (%1.dw_cfi_opc)")))
    dw_cfi_oprnd2;
};


typedef vec<dw_cfi_ref, va_gc> *cfi_vec;

typedef struct dw_fde_node *dw_fde_ref;

/* All call frame descriptions (FDE's) in the GCC generated DWARF
   refer to a single Common Information Entry (CIE), defined at
   the beginning of the .debug_frame section.  This use of a single
   CIE obviates the need to keep track of multiple CIE's
   in the DWARF generation routines below.  */

struct GTY(()) dw_fde_node {
  tree decl;
  const char *dw_fde_begin;
  const char *dw_fde_current_label;
  const char *dw_fde_end;
  const char *dw_fde_vms_end_prologue;
  const char *dw_fde_vms_begin_epilogue;
  const char *dw_fde_second_begin;
  const char *dw_fde_second_end;
  cfi_vec dw_fde_cfi;
  int dw_fde_switch_cfi_index; /* Last CFI before switching sections.  */
  HOST_WIDE_INT stack_realignment;

  unsigned funcdef_number;
  unsigned fde_index;

  /* Dynamic realign argument pointer register.  */
  unsigned int drap_reg;
  /* Virtual dynamic realign argument pointer register.  */
  unsigned int vdrap_reg;
  /* These 3 flags are copied from rtl_data in function.h.  */
  unsigned all_throwers_are_sibcalls : 1;
  unsigned uses_eh_lsda : 1;
  unsigned nothrow : 1;
  /* Whether we did stack realign in this call frame.  */
  unsigned stack_realign : 1;
  /* Whether dynamic realign argument pointer register has been saved.  */
  unsigned drap_reg_saved: 1;
  /* True iff dw_fde_begin label is in text_section or cold_text_section.  */
  unsigned in_std_section : 1;
  /* True iff dw_fde_second_begin label is in text_section or
     cold_text_section.  */
  unsigned second_in_std_section : 1;
  /* True if Rule 18 described in dwarf2cfi.cc is in action, i.e. for dynamic
     stack realignment in between pushing of hard frame pointer to stack
     and setting hard frame pointer to stack pointer.  The register save for
     hard frame pointer register should be emitted only on the latter
     instruction.  */
  unsigned rule18 : 1;
  /* True if this function is to be ignored by debugger.  */
  unsigned ignored_debug : 1;
};


/* This represents a register, in DWARF_FRAME_REGNUM space, for use in CFA
   definitions and expressions.
   Most architectures only need a single register number, but some (amdgcn)
   have pointers that span multiple registers.  DWARF permits arbitrary
   register sets but existing use-cases only require contiguous register
   sets, as represented here.  */
struct GTY(()) cfa_reg {
  unsigned int reg;
  unsigned short span;
  unsigned short span_width;  /* A.K.A. register mode size.  */

  cfa_reg& set_by_dwreg (unsigned int r)
    {
      reg = r;
      span = 1;
      span_width = 0;  /* Unknown size (permitted when span == 1).  */
      return *this;
    }

  bool operator== (const cfa_reg &other) const
    {
      return (reg == other.reg && span == other.span
	      && (span_width == other.span_width
		  || (span == 1
		      && (span_width == 0 || other.span_width == 0))));
    }

  bool operator!= (const cfa_reg &other) const
    {
      return !(*this == other);
    }
};

/* This is how we define the location of the CFA. We use to handle it
   as REG + OFFSET all the time,  but now it can be more complex.
   It can now be either REG + CFA_OFFSET or *(REG + BASE_OFFSET) + CFA_OFFSET.
   Instead of passing around REG and OFFSET, we pass a copy
   of this structure.  */
struct GTY(()) dw_cfa_location {
  poly_int64 offset;
  poly_int64 base_offset;
  /* REG is in DWARF_FRAME_REGNUM space, *not* normal REGNO space.  */
  struct cfa_reg reg;
  BOOL_BITFIELD indirect : 1;  /* 1 if CFA is accessed via a dereference.  */
  BOOL_BITFIELD in_use : 1;    /* 1 if a saved cfa is stored here.  */
};


/* Each DIE may have a series of attribute/value pairs.  Values
   can take on several forms.  The forms that are used in this
   implementation are listed below.  */

enum dw_val_class
{
  dw_val_class_none,
  dw_val_class_addr,
  dw_val_class_offset,
  dw_val_class_loc,
  dw_val_class_loc_list,
  dw_val_class_range_list,
  dw_val_class_const,
  dw_val_class_unsigned_const,
  dw_val_class_const_double,
  dw_val_class_wide_int,
  dw_val_class_vec,
  dw_val_class_flag,
  dw_val_class_die_ref,
  dw_val_class_fde_ref,
  dw_val_class_lbl_id,
  dw_val_class_lineptr,
  dw_val_class_str,
  dw_val_class_macptr,
  dw_val_class_loclistsptr,
  dw_val_class_file,
  dw_val_class_data8,
  dw_val_class_decl_ref,
  dw_val_class_vms_delta,
  dw_val_class_high_pc,
  dw_val_class_discr_value,
  dw_val_class_discr_list,
  dw_val_class_const_implicit,
  dw_val_class_unsigned_const_implicit,
  dw_val_class_file_implicit,
  dw_val_class_view_list,
  dw_val_class_symview
};

/* Describe a floating point constant value, or a vector constant value.  */

struct GTY(()) dw_vec_const {
  void * GTY((atomic)) array;
  unsigned length;
  unsigned elt_size;
};

/* Describe a single value that a discriminant can match.

   Discriminants (in the "record variant part" meaning) are scalars.
   dw_discr_list_ref and dw_discr_value are a mean to describe a set of
   discriminant values that are matched by a particular variant.

   Discriminants can be signed or unsigned scalars, and can be discriminants
   values.  Both have to be consistent, though.  */

struct GTY(()) dw_discr_value {
  int pos; /* Whether the discriminant value is positive (unsigned).  */
  union
    {
      HOST_WIDE_INT GTY ((tag ("0"))) sval;
      unsigned HOST_WIDE_INT GTY ((tag ("1"))) uval;
    }
  GTY ((desc ("%1.pos"))) v;
};

struct addr_table_entry;

typedef unsigned int var_loc_view;

/* Location lists are ranges + location descriptions for that range,
   so you can track variables that are in different places over
   their entire life.  */
typedef struct GTY(()) dw_loc_list_struct {
  dw_loc_list_ref dw_loc_next;
  const char *begin; /* Label and addr_entry for start of range */
  addr_table_entry *begin_entry;
  const char *end;  /* Label for end of range */
  addr_table_entry *end_entry;
  char *ll_symbol; /* Label for beginning of location list.
		      Only on head of list.  */
  char *vl_symbol; /* Label for beginning of view list.  Ditto.  */
  const char *section; /* Section this loclist is relative to */
  dw_loc_descr_ref expr;
  var_loc_view vbegin, vend;
  hashval_t hash;
  /* True if all addresses in this and subsequent lists are known to be
     resolved.  */
  bool resolved_addr;
  /* True if this list has been replaced by dw_loc_next.  */
  bool replaced;
  /* True if it has been emitted into .debug_loc* / .debug_loclists*
     section.  */
  unsigned char emitted : 1;
  /* True if hash field is index rather than hash value.  */
  unsigned char num_assigned : 1;
  /* True if .debug_loclists.dwo offset has been emitted for it already.  */
  unsigned char offset_emitted : 1;
  /* True if note_variable_value_in_expr has been called on it.  */
  unsigned char noted_variable_value : 1;
  /* True if the range should be emitted even if begin and end
     are the same.  */
  bool force;
} dw_loc_list_node;

/* The dw_val_node describes an attribute's value, as it is
   represented internally.  */

struct GTY(()) dw_val_node {
  enum dw_val_class val_class;
  /* On 64-bit host, there are 4 bytes of padding between val_class
     and val_entry.  Reuse the padding for other content of
     dw_loc_descr_node and dw_attr_struct.  */
  union dw_val_node_parent
    {
      struct dw_val_loc_descr_node
	{
	  ENUM_BITFIELD (dwarf_location_atom) dw_loc_opc_v : 8;
	  /* Used to distinguish DW_OP_addr with a direct symbol relocation
	     from DW_OP_addr with a dtp-relative symbol relocation.  */
	  unsigned int dw_loc_dtprel_v : 1;
	  /* For DW_OP_pick, DW_OP_dup and DW_OP_over operations: true iff.
	     it targets a DWARF prodecure argument.  In this case, it needs to be
	     relocated according to the current frame offset.  */
	  unsigned int dw_loc_frame_offset_rel_v : 1;
	} u1;
      int u2;
      enum dwarf_attribute u3;
    } GTY((skip)) u;
  struct addr_table_entry * GTY(()) val_entry;
  union dw_val_struct_union
    {
      rtx GTY ((tag ("dw_val_class_addr"))) val_addr;
      unsigned HOST_WIDE_INT GTY ((tag ("dw_val_class_offset"))) val_offset;
      dw_loc_list_ref GTY ((tag ("dw_val_class_loc_list"))) val_loc_list;
      dw_die_ref GTY ((tag ("dw_val_class_view_list"))) val_view_list;
      dw_loc_descr_ref GTY ((tag ("dw_val_class_loc"))) val_loc;
      HOST_WIDE_INT GTY ((default)) val_int;
      unsigned HOST_WIDE_INT
	GTY ((tag ("dw_val_class_unsigned_const"))) val_unsigned;
      double_int GTY ((tag ("dw_val_class_const_double"))) val_double;
      dw_wide_int_ptr GTY ((tag ("dw_val_class_wide_int"))) val_wide;
      dw_vec_const GTY ((tag ("dw_val_class_vec"))) val_vec;
      struct dw_val_die_union
	{
	  dw_die_ref die;
	  int external;
	} GTY ((tag ("dw_val_class_die_ref"))) val_die_ref;
      unsigned GTY ((tag ("dw_val_class_fde_ref"))) val_fde_index;
      struct indirect_string_node * GTY ((tag ("dw_val_class_str"))) val_str;
      char * GTY ((tag ("dw_val_class_lbl_id"))) val_lbl_id;
      unsigned char GTY ((tag ("dw_val_class_flag"))) val_flag;
      struct dwarf_file_data * GTY ((tag ("dw_val_class_file"))) val_file;
      struct dwarf_file_data *
	GTY ((tag ("dw_val_class_file_implicit"))) val_file_implicit;
      unsigned char GTY ((tag ("dw_val_class_data8"))) val_data8[8];
      tree GTY ((tag ("dw_val_class_decl_ref"))) val_decl_ref;
      struct dw_val_vms_delta_union
	{
	  char * lbl1;
	  char * lbl2;
	} GTY ((tag ("dw_val_class_vms_delta"))) val_vms_delta;
      dw_discr_value GTY ((tag ("dw_val_class_discr_value"))) val_discr_value;
      dw_discr_list_ref GTY ((tag ("dw_val_class_discr_list"))) val_discr_list;
      char * GTY ((tag ("dw_val_class_symview"))) val_symbolic_view;
    }
  GTY ((desc ("%1.val_class"))) v;
};

/* Locations in memory are described using a sequence of stack machine
   operations.  */

struct GTY((chain_next ("%h.dw_loc_next"))) dw_loc_descr_node {
  dw_loc_descr_ref dw_loc_next;
#define dw_loc_opc dw_loc_oprnd1.u.u1.dw_loc_opc_v
  /* Used to distinguish DW_OP_addr with a direct symbol relocation
     from DW_OP_addr with a dtp-relative symbol relocation.  */
#define dw_loc_dtprel dw_loc_oprnd1.u.u1.dw_loc_dtprel_v
  /* For DW_OP_pick, DW_OP_dup and DW_OP_over operations: true iff.
     it targets a DWARF prodecure argument.  In this case, it needs to be
     relocated according to the current frame offset.  */
#define dw_loc_frame_offset_rel dw_loc_oprnd1.u.u1.dw_loc_frame_offset_rel_v
#define dw_loc_addr dw_loc_oprnd2.u.u2
  dw_val_node dw_loc_oprnd1;
  dw_val_node dw_loc_oprnd2;
};

/* A variant (inside a record variant part) is selected when the corresponding
   discriminant matches its set of values (see the comment for dw_discr_value).
   The following datastructure holds such matching information.  */

struct GTY(()) dw_discr_list_node {
  dw_discr_list_ref dw_discr_next;

  dw_discr_value dw_discr_lower_bound;
  dw_discr_value dw_discr_upper_bound;
  /* This node represents only the value in dw_discr_lower_bound when it's
     zero.  It represents the range between the two fields (bounds included)
     otherwise.  */
  int dw_discr_range;
};

struct GTY((variable_size)) dw_wide_int {
  unsigned int precision;
  unsigned int len;
  HOST_WIDE_INT val[1];

  unsigned int get_precision () const { return precision; }
  unsigned int get_len () const { return len; }
  const HOST_WIDE_INT *get_val () const { return val; }
  inline HOST_WIDE_INT elt (unsigned int) const;
  inline bool operator == (const dw_wide_int &) const;
};

inline HOST_WIDE_INT
dw_wide_int::elt (unsigned int i) const
{
  if (i < len)
    return val[i];
  wide_int_ref ref = wi::storage_ref (val, len, precision);
  return wi::sign_mask (ref);
}

inline bool
dw_wide_int::operator == (const dw_wide_int &o) const
{
  wide_int_ref ref1 = wi::storage_ref (val, len, precision);
  wide_int_ref ref2 = wi::storage_ref (o.val, o.len, o.precision);
  return ref1 == ref2;
}

/* Interface from dwarf2out.cc to dwarf2cfi.cc.  */
extern struct dw_loc_descr_node *build_cfa_loc
  (dw_cfa_location *, poly_int64);
extern struct dw_loc_descr_node *build_cfa_aligned_loc
  (dw_cfa_location *, poly_int64, HOST_WIDE_INT);
extern struct dw_loc_descr_node *build_span_loc (struct cfa_reg);
extern struct dw_loc_descr_node *mem_loc_descriptor
  (rtx, machine_mode mode, machine_mode mem_mode,
   enum var_init_status);
extern bool loc_descr_equal_p (dw_loc_descr_ref, dw_loc_descr_ref);
extern dw_fde_ref dwarf2out_alloc_current_fde (void);

extern unsigned long size_of_locs (dw_loc_descr_ref);
extern void output_loc_sequence (dw_loc_descr_ref, int);
extern void output_loc_sequence_raw (dw_loc_descr_ref);

/* Interface from dwarf2cfi.cc to dwarf2out.cc.  */
extern void lookup_cfa_1 (dw_cfi_ref cfi, dw_cfa_location *loc,
			  dw_cfa_location *remember);
extern bool cfa_equal_p (const dw_cfa_location *, const dw_cfa_location *);

extern void output_cfi (dw_cfi_ref, dw_fde_ref, int);

extern GTY(()) cfi_vec cie_cfi_vec;

/* Interface from dwarf2*.c to the rest of the compiler.  */
extern enum dw_cfi_oprnd_type dw_cfi_oprnd1_desc (dwarf_call_frame_info cfi);
extern enum dw_cfi_oprnd_type dw_cfi_oprnd2_desc (dwarf_call_frame_info cfi);

extern void output_cfi_directive (FILE *f, dw_cfi_ref cfi);

extern void dwarf2out_emit_cfi (dw_cfi_ref cfi);

extern void debug_dwarf (void);
struct die_struct;
extern void debug_dwarf_die (struct die_struct *);
extern void debug_dwarf_loc_descr (dw_loc_descr_ref);
extern void debug (die_struct &ref);
extern void debug (die_struct *ptr);
extern void dwarf2out_set_demangle_name_func (const char *(*) (const char *));
#ifdef VMS_DEBUGGING_INFO
extern void dwarf2out_vms_debug_main_pointer (void);
#endif

enum array_descr_ordering
{
  array_descr_ordering_default,
  array_descr_ordering_row_major,
  array_descr_ordering_column_major
};

#define DWARF2OUT_ARRAY_DESCR_INFO_MAX_DIMEN 16

struct array_descr_info
{
  int ndimensions;
  enum array_descr_ordering ordering;
  tree element_type;
  tree base_decl;
  tree data_location;
  tree allocated;
  tree associated;
  tree stride;
  tree rank;
  bool stride_in_bits;
  struct array_descr_dimen
    {
      /* GCC uses sizetype for array indices, so lower_bound and upper_bound
	 will likely be "sizetype" values. However, bounds may have another
	 type in the original source code.  */
      tree bounds_type;
      tree lower_bound;
      tree upper_bound;

      /* Only Fortran uses more than one dimension for array types.  For other
	 languages, the stride can be rather specified for the whole array.  */
      tree stride;
    } dimen[DWARF2OUT_ARRAY_DESCR_INFO_MAX_DIMEN];
};

enum fixed_point_scale_factor
{
  fixed_point_scale_factor_binary,
  fixed_point_scale_factor_decimal,
  fixed_point_scale_factor_arbitrary
};

struct fixed_point_type_info
{
  /* The scale factor is the value one has to multiply the actual data with
     to get the fixed point value.  We support three ways to encode it.  */
  enum fixed_point_scale_factor scale_factor_kind;
  union
    {
      /* For a binary scale factor, the scale factor is 2 ** binary.  */
      int binary;
      /* For a decimal scale factor, the scale factor is 10 ** decimal.  */
      int decimal;
      /* For an arbitrary scale factor, the scale factor is the ratio
	 numerator / denominator.  */
      struct { tree numerator; tree denominator; } arbitrary;
    } scale_factor;
};

void dwarf2cfi_cc_finalize (void);
void dwarf2out_cc_finalize (void);

/* Some DWARF internals are exposed for the needs of DWARF-based debug
   formats.  */

/* Each DIE attribute has a field specifying the attribute kind,
   a link to the next attribute in the chain, and an attribute value.
   Attributes are typically linked below the DIE they modify.  */

typedef struct GTY(()) dw_attr_struct {
#define dw_attr dw_attr_val.u.u3
  dw_val_node dw_attr_val;
}
dw_attr_node;

extern dw_attr_node *get_AT (dw_die_ref, enum dwarf_attribute);
extern HOST_WIDE_INT AT_int (dw_attr_node *);
extern unsigned HOST_WIDE_INT AT_unsigned (dw_attr_node *a);
extern dw_loc_descr_ref AT_loc (dw_attr_node *);
extern dw_die_ref get_AT_ref (dw_die_ref, enum dwarf_attribute);
extern const char *get_AT_string (dw_die_ref, enum dwarf_attribute);
extern enum dw_val_class AT_class (dw_attr_node *);
extern unsigned HOST_WIDE_INT AT_unsigned (dw_attr_node *);
extern unsigned get_AT_unsigned (dw_die_ref, enum dwarf_attribute);
extern int get_AT_flag (dw_die_ref, enum dwarf_attribute);

extern void add_name_attribute (dw_die_ref, const char *);

extern dw_die_ref new_die_raw (enum dwarf_tag);
extern dw_die_ref base_type_die (tree, bool);

extern dw_die_ref lookup_decl_die (tree);
extern dw_die_ref lookup_type_die (tree);

extern dw_die_ref dw_get_die_child (dw_die_ref);
extern dw_die_ref dw_get_die_sib (dw_die_ref);
extern dw_die_ref dw_get_die_parent (dw_die_ref);
extern enum dwarf_tag dw_get_die_tag (dw_die_ref);

/* Data about a single source file.  */
struct GTY((for_user)) dwarf_file_data {
  const char * key;
  const char * filename;
  int emitted_number;
};

extern struct dwarf_file_data *get_AT_file (dw_die_ref,
					    enum dwarf_attribute);

#endif /* GCC_DWARF2OUT_H */
