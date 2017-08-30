/* Output Dwarf2 format symbol table information from GCC.
   Copyright (C) 1992-2017 Free Software Foundation, Inc.
   Contributed by Gary Funck (gary@intrepid.com).
   Derived from DWARF 1 implementation of Ron Guilmette (rfg@monkeys.com).
   Extensively modified by Jason Merrill (jason@cygnus.com).

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

/* TODO: Emit .debug_line header even when there are no functions, since
	   the file numbers are used by .debug_info.  Alternately, leave
	   out locations for types and decls.
	 Avoid talking about ctors and op= for PODs.
	 Factor out common prologue sequences into multiple CIEs.  */

/* The first part of this file deals with the DWARF 2 frame unwind
   information, which is also used by the GCC efficient exception handling
   mechanism.  The second part, controlled only by an #ifdef
   DWARF2_DEBUGGING_INFO, deals with the other DWARF 2 debugging
   information.  */

/* DWARF2 Abbreviation Glossary:

   CFA = Canonical Frame Address
	   a fixed address on the stack which identifies a call frame.
	   We define it to be the value of SP just before the call insn.
	   The CFA register and offset, which may change during the course
	   of the function, are used to calculate its value at runtime.

   CFI = Call Frame Instruction
	   an instruction for the DWARF2 abstract machine

   CIE = Common Information Entry
	   information describing information common to one or more FDEs

   DIE = Debugging Information Entry

   FDE = Frame Description Entry
	   information describing the stack call frame, in particular,
	   how to restore registers

   DW_CFA_... = DWARF2 CFA call frame instruction
   DW_TAG_... = DWARF2 DIE tag */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "target.h"
#include "function.h"
#include "rtl.h"
#include "tree.h"
#include "memmodel.h"
#include "tm_p.h"
#include "stringpool.h"
#include "insn-config.h"
#include "ira.h"
#include "cgraph.h"
#include "diagnostic.h"
#include "fold-const.h"
#include "stor-layout.h"
#include "varasm.h"
#include "version.h"
#include "flags.h"
#include "rtlhash.h"
#include "reload.h"
#include "output.h"
#include "expr.h"
#include "dwarf2out.h"
#include "dwarf2asm.h"
#include "toplev.h"
#include "md5.h"
#include "tree-pretty-print.h"
#include "debug.h"
#include "common/common-target.h"
#include "langhooks.h"
#include "lra.h"
#include "dumpfile.h"
#include "opts.h"
#include "tree-dfa.h"
#include "gdb/gdb-index.h"
#include "rtl-iter.h"
#include "stringpool.h"
#include "attribs.h"

static void dwarf2out_source_line (unsigned int, unsigned int, const char *,
				   int, bool);
static rtx_insn *last_var_location_insn;
static rtx_insn *cached_next_real_insn;
static void dwarf2out_decl (tree);

#ifndef XCOFF_DEBUGGING_INFO
#define XCOFF_DEBUGGING_INFO 0
#endif

#ifndef HAVE_XCOFF_DWARF_EXTRAS
#define HAVE_XCOFF_DWARF_EXTRAS 0
#endif

#ifdef VMS_DEBUGGING_INFO
int vms_file_stats_name (const char *, long long *, long *, char *, int *);

/* Define this macro to be a nonzero value if the directory specifications
    which are output in the debug info should end with a separator.  */
#define DWARF2_DIR_SHOULD_END_WITH_SEPARATOR 1
/* Define this macro to evaluate to a nonzero value if GCC should refrain
   from generating indirect strings in DWARF2 debug information, for instance
   if your target is stuck with an old version of GDB that is unable to
   process them properly or uses VMS Debug.  */
#define DWARF2_INDIRECT_STRING_SUPPORT_MISSING_ON_TARGET 1
#else
#define DWARF2_DIR_SHOULD_END_WITH_SEPARATOR 0
#define DWARF2_INDIRECT_STRING_SUPPORT_MISSING_ON_TARGET 0
#endif

/* ??? Poison these here until it can be done generically.  They've been
   totally replaced in this file; make sure it stays that way.  */
#undef DWARF2_UNWIND_INFO
#undef DWARF2_FRAME_INFO
#if (GCC_VERSION >= 3000)
 #pragma GCC poison DWARF2_UNWIND_INFO DWARF2_FRAME_INFO
#endif

/* The size of the target's pointer type.  */
#ifndef PTR_SIZE
#define PTR_SIZE (POINTER_SIZE / BITS_PER_UNIT)
#endif

/* Array of RTXes referenced by the debugging information, which therefore
   must be kept around forever.  */
static GTY(()) vec<rtx, va_gc> *used_rtx_array;

/* A pointer to the base of a list of incomplete types which might be
   completed at some later time.  incomplete_types_list needs to be a
   vec<tree, va_gc> *because we want to tell the garbage collector about
   it.  */
static GTY(()) vec<tree, va_gc> *incomplete_types;

/* A pointer to the base of a table of references to declaration
   scopes.  This table is a display which tracks the nesting
   of declaration scopes at the current scope and containing
   scopes.  This table is used to find the proper place to
   define type declaration DIE's.  */
static GTY(()) vec<tree, va_gc> *decl_scope_table;

/* Pointers to various DWARF2 sections.  */
static GTY(()) section *debug_info_section;
static GTY(()) section *debug_skeleton_info_section;
static GTY(()) section *debug_abbrev_section;
static GTY(()) section *debug_skeleton_abbrev_section;
static GTY(()) section *debug_aranges_section;
static GTY(()) section *debug_addr_section;
static GTY(()) section *debug_macinfo_section;
static const char *debug_macinfo_section_name;
static unsigned macinfo_label_base = 1;
static GTY(()) section *debug_line_section;
static GTY(()) section *debug_skeleton_line_section;
static GTY(()) section *debug_loc_section;
static GTY(()) section *debug_pubnames_section;
static GTY(()) section *debug_pubtypes_section;
static GTY(()) section *debug_str_section;
static GTY(()) section *debug_line_str_section;
static GTY(()) section *debug_str_dwo_section;
static GTY(()) section *debug_str_offsets_section;
static GTY(()) section *debug_ranges_section;
static GTY(()) section *debug_frame_section;

/* Maximum size (in bytes) of an artificially generated label.  */
#define MAX_ARTIFICIAL_LABEL_BYTES	40

/* According to the (draft) DWARF 3 specification, the initial length
   should either be 4 or 12 bytes.  When it's 12 bytes, the first 4
   bytes are 0xffffffff, followed by the length stored in the next 8
   bytes.

   However, the SGI/MIPS ABI uses an initial length which is equal to
   DWARF_OFFSET_SIZE.  It is defined (elsewhere) accordingly.  */

#ifndef DWARF_INITIAL_LENGTH_SIZE
#define DWARF_INITIAL_LENGTH_SIZE (DWARF_OFFSET_SIZE == 4 ? 4 : 12)
#endif

#ifndef DWARF_INITIAL_LENGTH_SIZE_STR
#define DWARF_INITIAL_LENGTH_SIZE_STR (DWARF_OFFSET_SIZE == 4 ? "-4" : "-12")
#endif

/* Round SIZE up to the nearest BOUNDARY.  */
#define DWARF_ROUND(SIZE,BOUNDARY) \
  ((((SIZE) + (BOUNDARY) - 1) / (BOUNDARY)) * (BOUNDARY))

/* CIE identifier.  */
#if HOST_BITS_PER_WIDE_INT >= 64
#define DWARF_CIE_ID \
  (unsigned HOST_WIDE_INT) (DWARF_OFFSET_SIZE == 4 ? DW_CIE_ID : DW64_CIE_ID)
#else
#define DWARF_CIE_ID DW_CIE_ID
#endif


/* A vector for a table that contains frame description
   information for each routine.  */
#define NOT_INDEXED (-1U)
#define NO_INDEX_ASSIGNED (-2U)

static GTY(()) vec<dw_fde_ref, va_gc> *fde_vec;

struct GTY((for_user)) indirect_string_node {
  const char *str;
  unsigned int refcount;
  enum dwarf_form form;
  char *label;
  unsigned int index;
};

struct indirect_string_hasher : ggc_ptr_hash<indirect_string_node>
{
  typedef const char *compare_type;

  static hashval_t hash (indirect_string_node *);
  static bool equal (indirect_string_node *, const char *);
};

static GTY (()) hash_table<indirect_string_hasher> *debug_str_hash;

static GTY (()) hash_table<indirect_string_hasher> *debug_line_str_hash;

/* With split_debug_info, both the comp_dir and dwo_name go in the
   main object file, rather than the dwo, similar to the force_direct
   parameter elsewhere but with additional complications:

   1) The string is needed in both the main object file and the dwo.
   That is, the comp_dir and dwo_name will appear in both places.

   2) Strings can use four forms: DW_FORM_string, DW_FORM_strp,
   DW_FORM_line_strp or DW_FORM_GNU_str_index.

   3) GCC chooses the form to use late, depending on the size and
   reference count.

   Rather than forcing the all debug string handling functions and
   callers to deal with these complications, simply use a separate,
   special-cased string table for any attribute that should go in the
   main object file.  This limits the complexity to just the places
   that need it.  */

static GTY (()) hash_table<indirect_string_hasher> *skeleton_debug_str_hash;

static GTY(()) int dw2_string_counter;

/* True if the compilation unit places functions in more than one section.  */
static GTY(()) bool have_multiple_function_sections = false;

/* Whether the default text and cold text sections have been used at all.  */

static GTY(()) bool text_section_used = false;
static GTY(()) bool cold_text_section_used = false;

/* The default cold text section.  */
static GTY(()) section *cold_text_section;

/* The DIE for C++14 'auto' in a function return type.  */
static GTY(()) dw_die_ref auto_die;

/* The DIE for C++14 'decltype(auto)' in a function return type.  */
static GTY(()) dw_die_ref decltype_auto_die;

/* Forward declarations for functions defined in this file.  */

static void output_call_frame_info (int);
static void dwarf2out_note_section_used (void);

/* Personality decl of current unit.  Used only when assembler does not support
   personality CFI.  */
static GTY(()) rtx current_unit_personality;

/* .debug_rnglists next index.  */
static unsigned int rnglist_idx;

/* Data and reference forms for relocatable data.  */
#define DW_FORM_data (DWARF_OFFSET_SIZE == 8 ? DW_FORM_data8 : DW_FORM_data4)
#define DW_FORM_ref (DWARF_OFFSET_SIZE == 8 ? DW_FORM_ref8 : DW_FORM_ref4)

#ifndef DEBUG_FRAME_SECTION
#define DEBUG_FRAME_SECTION	".debug_frame"
#endif

#ifndef FUNC_BEGIN_LABEL
#define FUNC_BEGIN_LABEL	"LFB"
#endif

#ifndef FUNC_END_LABEL
#define FUNC_END_LABEL		"LFE"
#endif

#ifndef PROLOGUE_END_LABEL
#define PROLOGUE_END_LABEL	"LPE"
#endif

#ifndef EPILOGUE_BEGIN_LABEL
#define EPILOGUE_BEGIN_LABEL	"LEB"
#endif

#ifndef FRAME_BEGIN_LABEL
#define FRAME_BEGIN_LABEL	"Lframe"
#endif
#define CIE_AFTER_SIZE_LABEL	"LSCIE"
#define CIE_END_LABEL		"LECIE"
#define FDE_LABEL		"LSFDE"
#define FDE_AFTER_SIZE_LABEL	"LASFDE"
#define FDE_END_LABEL		"LEFDE"
#define LINE_NUMBER_BEGIN_LABEL	"LSLT"
#define LINE_NUMBER_END_LABEL	"LELT"
#define LN_PROLOG_AS_LABEL	"LASLTP"
#define LN_PROLOG_END_LABEL	"LELTP"
#define DIE_LABEL_PREFIX	"DW"

/* Match the base name of a file to the base name of a compilation unit. */

static int
matches_main_base (const char *path)
{
  /* Cache the last query. */
  static const char *last_path = NULL;
  static int last_match = 0;
  if (path != last_path)
    {
      const char *base;
      int length = base_of_path (path, &base);
      last_path = path;
      last_match = (length == main_input_baselength
                    && memcmp (base, main_input_basename, length) == 0);
    }
  return last_match;
}

#ifdef DEBUG_DEBUG_STRUCT

static int
dump_struct_debug (tree type, enum debug_info_usage usage,
		   enum debug_struct_file criterion, int generic,
		   int matches, int result)
{
  /* Find the type name. */
  tree type_decl = TYPE_STUB_DECL (type);
  tree t = type_decl;
  const char *name = 0;
  if (TREE_CODE (t) == TYPE_DECL)
    t = DECL_NAME (t);
  if (t)
    name = IDENTIFIER_POINTER (t);

  fprintf (stderr, "	struct %d %s %s %s %s %d %p %s\n",
	   criterion,
           DECL_IN_SYSTEM_HEADER (type_decl) ? "sys" : "usr",
           matches ? "bas" : "hdr",
           generic ? "gen" : "ord",
           usage == DINFO_USAGE_DFN ? ";" :
             usage == DINFO_USAGE_DIR_USE ? "." : "*",
           result,
           (void*) type_decl, name);
  return result;
}
#define DUMP_GSTRUCT(type, usage, criterion, generic, matches, result) \
  dump_struct_debug (type, usage, criterion, generic, matches, result)

#else

#define DUMP_GSTRUCT(type, usage, criterion, generic, matches, result) \
  (result)

#endif

/* Get the number of HOST_WIDE_INTs needed to represent the precision
   of the number.  Some constants have a large uniform precision, so
   we get the precision needed for the actual value of the number.  */

static unsigned int
get_full_len (const wide_int &op)
{
  int prec = wi::min_precision (op, UNSIGNED);
  return ((prec + HOST_BITS_PER_WIDE_INT - 1)
	  / HOST_BITS_PER_WIDE_INT);
}

static bool
should_emit_struct_debug (tree type, enum debug_info_usage usage)
{
  enum debug_struct_file criterion;
  tree type_decl;
  bool generic = lang_hooks.types.generic_p (type);

  if (generic)
    criterion = debug_struct_generic[usage];
  else
    criterion = debug_struct_ordinary[usage];

  if (criterion == DINFO_STRUCT_FILE_NONE)
    return DUMP_GSTRUCT (type, usage, criterion, generic, false, false);
  if (criterion == DINFO_STRUCT_FILE_ANY)
    return DUMP_GSTRUCT (type, usage, criterion, generic, false, true);

  type_decl = TYPE_STUB_DECL (TYPE_MAIN_VARIANT (type));

  if (type_decl != NULL)
    {
     if (criterion == DINFO_STRUCT_FILE_SYS && DECL_IN_SYSTEM_HEADER (type_decl))
        return DUMP_GSTRUCT (type, usage, criterion, generic, false, true);

      if (matches_main_base (DECL_SOURCE_FILE (type_decl)))
        return DUMP_GSTRUCT (type, usage, criterion, generic, true, true);
    }

  return DUMP_GSTRUCT (type, usage, criterion, generic, false, false);
}

/* Switch [BACK] to eh_frame_section.  If we don't have an eh_frame_section,
   switch to the data section instead, and write out a synthetic start label
   for collect2 the first time around.  */

static void
switch_to_eh_frame_section (bool back ATTRIBUTE_UNUSED)
{
  if (eh_frame_section == 0)
    {
      int flags;

      if (EH_TABLES_CAN_BE_READ_ONLY)
	{
	  int fde_encoding;
	  int per_encoding;
	  int lsda_encoding;

	  fde_encoding = ASM_PREFERRED_EH_DATA_FORMAT (/*code=*/1,
						       /*global=*/0);
	  per_encoding = ASM_PREFERRED_EH_DATA_FORMAT (/*code=*/2,
						       /*global=*/1);
	  lsda_encoding = ASM_PREFERRED_EH_DATA_FORMAT (/*code=*/0,
							/*global=*/0);
	  flags = ((! flag_pic
		    || ((fde_encoding & 0x70) != DW_EH_PE_absptr
			&& (fde_encoding & 0x70) != DW_EH_PE_aligned
			&& (per_encoding & 0x70) != DW_EH_PE_absptr
			&& (per_encoding & 0x70) != DW_EH_PE_aligned
			&& (lsda_encoding & 0x70) != DW_EH_PE_absptr
			&& (lsda_encoding & 0x70) != DW_EH_PE_aligned))
		   ? 0 : SECTION_WRITE);
	}
      else
	flags = SECTION_WRITE;

#ifdef EH_FRAME_SECTION_NAME
      eh_frame_section = get_section (EH_FRAME_SECTION_NAME, flags, NULL);
#else
      eh_frame_section = ((flags == SECTION_WRITE)
			  ? data_section : readonly_data_section);
#endif /* EH_FRAME_SECTION_NAME */
    }

  switch_to_section (eh_frame_section);

#ifdef EH_FRAME_THROUGH_COLLECT2
  /* We have no special eh_frame section.  Emit special labels to guide
     collect2.  */
  if (!back)
    {
      tree label = get_file_function_name ("F");
      ASM_OUTPUT_ALIGN (asm_out_file, floor_log2 (PTR_SIZE));
      targetm.asm_out.globalize_label (asm_out_file,
					IDENTIFIER_POINTER (label));
      ASM_OUTPUT_LABEL (asm_out_file, IDENTIFIER_POINTER (label));
    }
#endif
}

/* Switch [BACK] to the eh or debug frame table section, depending on
   FOR_EH.  */

static void
switch_to_frame_table_section (int for_eh, bool back)
{
  if (for_eh)
    switch_to_eh_frame_section (back);
  else
    {
      if (!debug_frame_section)
	debug_frame_section = get_section (DEBUG_FRAME_SECTION,
					   SECTION_DEBUG, NULL);
      switch_to_section (debug_frame_section);
    }
}

/* Describe for the GTY machinery what parts of dw_cfi_oprnd1 are used.  */

enum dw_cfi_oprnd_type
dw_cfi_oprnd1_desc (enum dwarf_call_frame_info cfi)
{
  switch (cfi)
    {
    case DW_CFA_nop:
    case DW_CFA_GNU_window_save:
    case DW_CFA_remember_state:
    case DW_CFA_restore_state:
      return dw_cfi_oprnd_unused;

    case DW_CFA_set_loc:
    case DW_CFA_advance_loc1:
    case DW_CFA_advance_loc2:
    case DW_CFA_advance_loc4:
    case DW_CFA_MIPS_advance_loc8:
      return dw_cfi_oprnd_addr;

    case DW_CFA_offset:
    case DW_CFA_offset_extended:
    case DW_CFA_def_cfa:
    case DW_CFA_offset_extended_sf:
    case DW_CFA_def_cfa_sf:
    case DW_CFA_restore:
    case DW_CFA_restore_extended:
    case DW_CFA_undefined:
    case DW_CFA_same_value:
    case DW_CFA_def_cfa_register:
    case DW_CFA_register:
    case DW_CFA_expression:
    case DW_CFA_val_expression:
      return dw_cfi_oprnd_reg_num;

    case DW_CFA_def_cfa_offset:
    case DW_CFA_GNU_args_size:
    case DW_CFA_def_cfa_offset_sf:
      return dw_cfi_oprnd_offset;

    case DW_CFA_def_cfa_expression:
      return dw_cfi_oprnd_loc;

    default:
      gcc_unreachable ();
    }
}

/* Describe for the GTY machinery what parts of dw_cfi_oprnd2 are used.  */

enum dw_cfi_oprnd_type
dw_cfi_oprnd2_desc (enum dwarf_call_frame_info cfi)
{
  switch (cfi)
    {
    case DW_CFA_def_cfa:
    case DW_CFA_def_cfa_sf:
    case DW_CFA_offset:
    case DW_CFA_offset_extended_sf:
    case DW_CFA_offset_extended:
      return dw_cfi_oprnd_offset;

    case DW_CFA_register:
      return dw_cfi_oprnd_reg_num;

    case DW_CFA_expression:
    case DW_CFA_val_expression:
      return dw_cfi_oprnd_loc;

    default:
      return dw_cfi_oprnd_unused;
    }
}

/* Output one FDE.  */

static void
output_fde (dw_fde_ref fde, bool for_eh, bool second,
	    char *section_start_label, int fde_encoding, char *augmentation,
	    bool any_lsda_needed, int lsda_encoding)
{
  const char *begin, *end;
  static unsigned int j;
  char l1[MAX_ARTIFICIAL_LABEL_BYTES], l2[MAX_ARTIFICIAL_LABEL_BYTES];

  targetm.asm_out.emit_unwind_label (asm_out_file, fde->decl, for_eh,
				     /* empty */ 0);
  targetm.asm_out.internal_label (asm_out_file, FDE_LABEL,
				  for_eh + j);
  ASM_GENERATE_INTERNAL_LABEL (l1, FDE_AFTER_SIZE_LABEL, for_eh + j);
  ASM_GENERATE_INTERNAL_LABEL (l2, FDE_END_LABEL, for_eh + j);
  if (!XCOFF_DEBUGGING_INFO || for_eh)
    {
      if (DWARF_INITIAL_LENGTH_SIZE - DWARF_OFFSET_SIZE == 4 && !for_eh)
	dw2_asm_output_data (4, 0xffffffff, "Initial length escape value"
			     " indicating 64-bit DWARF extension");
      dw2_asm_output_delta (for_eh ? 4 : DWARF_OFFSET_SIZE, l2, l1,
			    "FDE Length");
    }
  ASM_OUTPUT_LABEL (asm_out_file, l1);

  if (for_eh)
    dw2_asm_output_delta (4, l1, section_start_label, "FDE CIE offset");
  else
    dw2_asm_output_offset (DWARF_OFFSET_SIZE, section_start_label,
			   debug_frame_section, "FDE CIE offset");

  begin = second ? fde->dw_fde_second_begin : fde->dw_fde_begin;
  end = second ? fde->dw_fde_second_end : fde->dw_fde_end;

  if (for_eh)
    {
      rtx sym_ref = gen_rtx_SYMBOL_REF (Pmode, begin);
      SYMBOL_REF_FLAGS (sym_ref) |= SYMBOL_FLAG_LOCAL;
      dw2_asm_output_encoded_addr_rtx (fde_encoding, sym_ref, false,
				       "FDE initial location");
      dw2_asm_output_delta (size_of_encoded_value (fde_encoding),
			    end, begin, "FDE address range");
    }
  else
    {
      dw2_asm_output_addr (DWARF2_ADDR_SIZE, begin, "FDE initial location");
      dw2_asm_output_delta (DWARF2_ADDR_SIZE, end, begin, "FDE address range");
    }

  if (augmentation[0])
    {
      if (any_lsda_needed)
	{
	  int size = size_of_encoded_value (lsda_encoding);

	  if (lsda_encoding == DW_EH_PE_aligned)
	    {
	      int offset = (  4		/* Length */
			    + 4		/* CIE offset */
			    + 2 * size_of_encoded_value (fde_encoding)
			    + 1		/* Augmentation size */ );
	      int pad = -offset & (PTR_SIZE - 1);

	      size += pad;
	      gcc_assert (size_of_uleb128 (size) == 1);
	    }

	  dw2_asm_output_data_uleb128 (size, "Augmentation size");

	  if (fde->uses_eh_lsda)
	    {
	      ASM_GENERATE_INTERNAL_LABEL (l1, second ? "LLSDAC" : "LLSDA",
					   fde->funcdef_number);
	      dw2_asm_output_encoded_addr_rtx (lsda_encoding,
					       gen_rtx_SYMBOL_REF (Pmode, l1),
					       false,
					       "Language Specific Data Area");
	    }
	  else
	    {
	      if (lsda_encoding == DW_EH_PE_aligned)
		ASM_OUTPUT_ALIGN (asm_out_file, floor_log2 (PTR_SIZE));
	      dw2_asm_output_data (size_of_encoded_value (lsda_encoding), 0,
				   "Language Specific Data Area (none)");
	    }
	}
      else
	dw2_asm_output_data_uleb128 (0, "Augmentation size");
    }

  /* Loop through the Call Frame Instructions associated with this FDE.  */
  fde->dw_fde_current_label = begin;
  {
    size_t from, until, i;

    from = 0;
    until = vec_safe_length (fde->dw_fde_cfi);

    if (fde->dw_fde_second_begin == NULL)
      ;
    else if (!second)
      until = fde->dw_fde_switch_cfi_index;
    else
      from = fde->dw_fde_switch_cfi_index;

    for (i = from; i < until; i++)
      output_cfi ((*fde->dw_fde_cfi)[i], fde, for_eh);
  }

  /* If we are to emit a ref/link from function bodies to their frame tables,
     do it now.  This is typically performed to make sure that tables
     associated with functions are dragged with them and not discarded in
     garbage collecting links. We need to do this on a per function basis to
     cope with -ffunction-sections.  */

#ifdef ASM_OUTPUT_DWARF_TABLE_REF
  /* Switch to the function section, emit the ref to the tables, and
     switch *back* into the table section.  */
  switch_to_section (function_section (fde->decl));
  ASM_OUTPUT_DWARF_TABLE_REF (section_start_label);
  switch_to_frame_table_section (for_eh, true);
#endif

  /* Pad the FDE out to an address sized boundary.  */
  ASM_OUTPUT_ALIGN (asm_out_file,
		    floor_log2 ((for_eh ? PTR_SIZE : DWARF2_ADDR_SIZE)));
  ASM_OUTPUT_LABEL (asm_out_file, l2);

  j += 2;
}

/* Return true if frame description entry FDE is needed for EH.  */

static bool
fde_needed_for_eh_p (dw_fde_ref fde)
{
  if (flag_asynchronous_unwind_tables)
    return true;

  if (TARGET_USES_WEAK_UNWIND_INFO && DECL_WEAK (fde->decl))
    return true;

  if (fde->uses_eh_lsda)
    return true;

  /* If exceptions are enabled, we have collected nothrow info.  */
  if (flag_exceptions && (fde->all_throwers_are_sibcalls || fde->nothrow))
    return false;

  return true;
}

/* Output the call frame information used to record information
   that relates to calculating the frame pointer, and records the
   location of saved registers.  */

static void
output_call_frame_info (int for_eh)
{
  unsigned int i;
  dw_fde_ref fde;
  dw_cfi_ref cfi;
  char l1[MAX_ARTIFICIAL_LABEL_BYTES], l2[MAX_ARTIFICIAL_LABEL_BYTES];
  char section_start_label[MAX_ARTIFICIAL_LABEL_BYTES];
  bool any_lsda_needed = false;
  char augmentation[6];
  int augmentation_size;
  int fde_encoding = DW_EH_PE_absptr;
  int per_encoding = DW_EH_PE_absptr;
  int lsda_encoding = DW_EH_PE_absptr;
  int return_reg;
  rtx personality = NULL;
  int dw_cie_version;

  /* Don't emit a CIE if there won't be any FDEs.  */
  if (!fde_vec)
    return;

  /* Nothing to do if the assembler's doing it all.  */
  if (dwarf2out_do_cfi_asm ())
    return;

  /* If we don't have any functions we'll want to unwind out of, don't emit
     any EH unwind information.  If we make FDEs linkonce, we may have to
     emit an empty label for an FDE that wouldn't otherwise be emitted.  We
     want to avoid having an FDE kept around when the function it refers to
     is discarded.  Example where this matters: a primary function template
     in C++ requires EH information, an explicit specialization doesn't.  */
  if (for_eh)
    {
      bool any_eh_needed = false;

      FOR_EACH_VEC_ELT (*fde_vec, i, fde)
	{
	  if (fde->uses_eh_lsda)
	    any_eh_needed = any_lsda_needed = true;
	  else if (fde_needed_for_eh_p (fde))
	    any_eh_needed = true;
	  else if (TARGET_USES_WEAK_UNWIND_INFO)
	    targetm.asm_out.emit_unwind_label (asm_out_file, fde->decl, 1, 1);
	}

      if (!any_eh_needed)
	return;
    }

  /* We're going to be generating comments, so turn on app.  */
  if (flag_debug_asm)
    app_enable ();

  /* Switch to the proper frame section, first time.  */
  switch_to_frame_table_section (for_eh, false);

  ASM_GENERATE_INTERNAL_LABEL (section_start_label, FRAME_BEGIN_LABEL, for_eh);
  ASM_OUTPUT_LABEL (asm_out_file, section_start_label);

  /* Output the CIE.  */
  ASM_GENERATE_INTERNAL_LABEL (l1, CIE_AFTER_SIZE_LABEL, for_eh);
  ASM_GENERATE_INTERNAL_LABEL (l2, CIE_END_LABEL, for_eh);
  if (!XCOFF_DEBUGGING_INFO || for_eh)
    {
      if (DWARF_INITIAL_LENGTH_SIZE - DWARF_OFFSET_SIZE == 4 && !for_eh)
	dw2_asm_output_data (4, 0xffffffff,
	  "Initial length escape value indicating 64-bit DWARF extension");
      dw2_asm_output_delta (for_eh ? 4 : DWARF_OFFSET_SIZE, l2, l1,
			    "Length of Common Information Entry");
    }
  ASM_OUTPUT_LABEL (asm_out_file, l1);

  /* Now that the CIE pointer is PC-relative for EH,
     use 0 to identify the CIE.  */
  dw2_asm_output_data ((for_eh ? 4 : DWARF_OFFSET_SIZE),
		       (for_eh ? 0 : DWARF_CIE_ID),
		       "CIE Identifier Tag");

  /* Use the CIE version 3 for DWARF3; allow DWARF2 to continue to
     use CIE version 1, unless that would produce incorrect results
     due to overflowing the return register column.  */
  return_reg = DWARF2_FRAME_REG_OUT (DWARF_FRAME_RETURN_COLUMN, for_eh);
  dw_cie_version = 1;
  if (return_reg >= 256 || dwarf_version > 2)
    dw_cie_version = 3;
  dw2_asm_output_data (1, dw_cie_version, "CIE Version");

  augmentation[0] = 0;
  augmentation_size = 0;

  personality = current_unit_personality;
  if (for_eh)
    {
      char *p;

      /* Augmentation:
	 z	Indicates that a uleb128 is present to size the
		augmentation section.
	 L	Indicates the encoding (and thus presence) of
		an LSDA pointer in the FDE augmentation.
	 R	Indicates a non-default pointer encoding for
		FDE code pointers.
	 P	Indicates the presence of an encoding + language
		personality routine in the CIE augmentation.  */

      fde_encoding = ASM_PREFERRED_EH_DATA_FORMAT (/*code=*/1, /*global=*/0);
      per_encoding = ASM_PREFERRED_EH_DATA_FORMAT (/*code=*/2, /*global=*/1);
      lsda_encoding = ASM_PREFERRED_EH_DATA_FORMAT (/*code=*/0, /*global=*/0);

      p = augmentation + 1;
      if (personality)
	{
	  *p++ = 'P';
	  augmentation_size += 1 + size_of_encoded_value (per_encoding);
	  assemble_external_libcall (personality);
	}
      if (any_lsda_needed)
	{
	  *p++ = 'L';
	  augmentation_size += 1;
	}
      if (fde_encoding != DW_EH_PE_absptr)
	{
	  *p++ = 'R';
	  augmentation_size += 1;
	}
      if (p > augmentation + 1)
	{
	  augmentation[0] = 'z';
	  *p = '\0';
	}

      /* Ug.  Some platforms can't do unaligned dynamic relocations at all.  */
      if (personality && per_encoding == DW_EH_PE_aligned)
	{
	  int offset = (  4		/* Length */
			+ 4		/* CIE Id */
			+ 1		/* CIE version */
			+ strlen (augmentation) + 1	/* Augmentation */
			+ size_of_uleb128 (1)		/* Code alignment */
			+ size_of_sleb128 (DWARF_CIE_DATA_ALIGNMENT)
			+ 1		/* RA column */
			+ 1		/* Augmentation size */
			+ 1		/* Personality encoding */ );
	  int pad = -offset & (PTR_SIZE - 1);

	  augmentation_size += pad;

	  /* Augmentations should be small, so there's scarce need to
	     iterate for a solution.  Die if we exceed one uleb128 byte.  */
	  gcc_assert (size_of_uleb128 (augmentation_size) == 1);
	}
    }

  dw2_asm_output_nstring (augmentation, -1, "CIE Augmentation");
  if (dw_cie_version >= 4)
    {
      dw2_asm_output_data (1, DWARF2_ADDR_SIZE, "CIE Address Size");
      dw2_asm_output_data (1, 0, "CIE Segment Size");
    }
  dw2_asm_output_data_uleb128 (1, "CIE Code Alignment Factor");
  dw2_asm_output_data_sleb128 (DWARF_CIE_DATA_ALIGNMENT,
			       "CIE Data Alignment Factor");

  if (dw_cie_version == 1)
    dw2_asm_output_data (1, return_reg, "CIE RA Column");
  else
    dw2_asm_output_data_uleb128 (return_reg, "CIE RA Column");

  if (augmentation[0])
    {
      dw2_asm_output_data_uleb128 (augmentation_size, "Augmentation size");
      if (personality)
	{
	  dw2_asm_output_data (1, per_encoding, "Personality (%s)",
			       eh_data_format_name (per_encoding));
	  dw2_asm_output_encoded_addr_rtx (per_encoding,
					   personality,
					   true, NULL);
	}

      if (any_lsda_needed)
	dw2_asm_output_data (1, lsda_encoding, "LSDA Encoding (%s)",
			     eh_data_format_name (lsda_encoding));

      if (fde_encoding != DW_EH_PE_absptr)
	dw2_asm_output_data (1, fde_encoding, "FDE Encoding (%s)",
			     eh_data_format_name (fde_encoding));
    }

  FOR_EACH_VEC_ELT (*cie_cfi_vec, i, cfi)
    output_cfi (cfi, NULL, for_eh);

  /* Pad the CIE out to an address sized boundary.  */
  ASM_OUTPUT_ALIGN (asm_out_file,
		    floor_log2 (for_eh ? PTR_SIZE : DWARF2_ADDR_SIZE));
  ASM_OUTPUT_LABEL (asm_out_file, l2);

  /* Loop through all of the FDE's.  */
  FOR_EACH_VEC_ELT (*fde_vec, i, fde)
    {
      unsigned int k;

      /* Don't emit EH unwind info for leaf functions that don't need it.  */
      if (for_eh && !fde_needed_for_eh_p (fde))
	continue;

      for (k = 0; k < (fde->dw_fde_second_begin ? 2 : 1); k++)
	output_fde (fde, for_eh, k, section_start_label, fde_encoding,
		    augmentation, any_lsda_needed, lsda_encoding);
    }

  if (for_eh && targetm.terminate_dw2_eh_frame_info)
    dw2_asm_output_data (4, 0, "End of Table");

  /* Turn off app to make assembly quicker.  */
  if (flag_debug_asm)
    app_disable ();
}

/* Emit .cfi_startproc and .cfi_personality/.cfi_lsda if needed.  */

static void
dwarf2out_do_cfi_startproc (bool second)
{
  int enc;
  rtx ref;
  rtx personality = get_personality_function (current_function_decl);

  fprintf (asm_out_file, "\t.cfi_startproc\n");

  if (personality)
    {
      enc = ASM_PREFERRED_EH_DATA_FORMAT (/*code=*/2, /*global=*/1);
      ref = personality;

      /* ??? The GAS support isn't entirely consistent.  We have to
	 handle indirect support ourselves, but PC-relative is done
	 in the assembler.  Further, the assembler can't handle any
	 of the weirder relocation types.  */
      if (enc & DW_EH_PE_indirect)
	ref = dw2_force_const_mem (ref, true);

      fprintf (asm_out_file, "\t.cfi_personality %#x,", enc);
      output_addr_const (asm_out_file, ref);
      fputc ('\n', asm_out_file);
    }

  if (crtl->uses_eh_lsda)
    {
      char lab[MAX_ARTIFICIAL_LABEL_BYTES];

      enc = ASM_PREFERRED_EH_DATA_FORMAT (/*code=*/0, /*global=*/0);
      ASM_GENERATE_INTERNAL_LABEL (lab, second ? "LLSDAC" : "LLSDA",
				   current_function_funcdef_no);
      ref = gen_rtx_SYMBOL_REF (Pmode, lab);
      SYMBOL_REF_FLAGS (ref) = SYMBOL_FLAG_LOCAL;

      if (enc & DW_EH_PE_indirect)
	ref = dw2_force_const_mem (ref, true);

      fprintf (asm_out_file, "\t.cfi_lsda %#x,", enc);
      output_addr_const (asm_out_file, ref);
      fputc ('\n', asm_out_file);
    }
}

/* Allocate CURRENT_FDE.  Immediately initialize all we can, noting that
   this allocation may be done before pass_final.  */

dw_fde_ref
dwarf2out_alloc_current_fde (void)
{
  dw_fde_ref fde;

  fde = ggc_cleared_alloc<dw_fde_node> ();
  fde->decl = current_function_decl;
  fde->funcdef_number = current_function_funcdef_no;
  fde->fde_index = vec_safe_length (fde_vec);
  fde->all_throwers_are_sibcalls = crtl->all_throwers_are_sibcalls;
  fde->uses_eh_lsda = crtl->uses_eh_lsda;
  fde->nothrow = crtl->nothrow;
  fde->drap_reg = INVALID_REGNUM;
  fde->vdrap_reg = INVALID_REGNUM;

  /* Record the FDE associated with this function.  */
  cfun->fde = fde;
  vec_safe_push (fde_vec, fde);

  return fde;
}

/* Output a marker (i.e. a label) for the beginning of a function, before
   the prologue.  */

void
dwarf2out_begin_prologue (unsigned int line ATTRIBUTE_UNUSED,
			  unsigned int column ATTRIBUTE_UNUSED,
			  const char *file ATTRIBUTE_UNUSED)
{
  char label[MAX_ARTIFICIAL_LABEL_BYTES];
  char * dup_label;
  dw_fde_ref fde;
  section *fnsec;
  bool do_frame;

  current_function_func_begin_label = NULL;

  do_frame = dwarf2out_do_frame ();

  /* ??? current_function_func_begin_label is also used by except.c for
     call-site information.  We must emit this label if it might be used.  */
  if (!do_frame
      && (!flag_exceptions
	  || targetm_common.except_unwind_info (&global_options) == UI_SJLJ))
    return;

  fnsec = function_section (current_function_decl);
  switch_to_section (fnsec);
  ASM_GENERATE_INTERNAL_LABEL (label, FUNC_BEGIN_LABEL,
			       current_function_funcdef_no);
  ASM_OUTPUT_DEBUG_LABEL (asm_out_file, FUNC_BEGIN_LABEL,
			  current_function_funcdef_no);
  dup_label = xstrdup (label);
  current_function_func_begin_label = dup_label;

  /* We can elide the fde allocation if we're not emitting debug info.  */
  if (!do_frame)
    return;

  /* Cater to the various TARGET_ASM_OUTPUT_MI_THUNK implementations that
     emit insns as rtx but bypass the bulk of rest_of_compilation, which
     would include pass_dwarf2_frame.  If we've not created the FDE yet,
     do so now.  */
  fde = cfun->fde;
  if (fde == NULL)
    fde = dwarf2out_alloc_current_fde ();

  /* Initialize the bits of CURRENT_FDE that were not available earlier.  */
  fde->dw_fde_begin = dup_label;
  fde->dw_fde_current_label = dup_label;
  fde->in_std_section = (fnsec == text_section
			 || (cold_text_section && fnsec == cold_text_section));

  /* We only want to output line number information for the genuine dwarf2
     prologue case, not the eh frame case.  */
#ifdef DWARF2_DEBUGGING_INFO
  if (file)
    dwarf2out_source_line (line, column, file, 0, true);
#endif

  if (dwarf2out_do_cfi_asm ())
    dwarf2out_do_cfi_startproc (false);
  else
    {
      rtx personality = get_personality_function (current_function_decl);
      if (!current_unit_personality)
        current_unit_personality = personality;

      /* We cannot keep a current personality per function as without CFI
	 asm, at the point where we emit the CFI data, there is no current
	 function anymore.  */
      if (personality && current_unit_personality != personality)
	sorry ("multiple EH personalities are supported only with assemblers "
	       "supporting .cfi_personality directive");
    }
}

/* Output a marker (i.e. a label) for the end of the generated code
   for a function prologue.  This gets called *after* the prologue code has
   been generated.  */

void
dwarf2out_vms_end_prologue (unsigned int line ATTRIBUTE_UNUSED,
			    const char *file ATTRIBUTE_UNUSED)
{
  char label[MAX_ARTIFICIAL_LABEL_BYTES];

  /* Output a label to mark the endpoint of the code generated for this
     function.  */
  ASM_GENERATE_INTERNAL_LABEL (label, PROLOGUE_END_LABEL,
			       current_function_funcdef_no);
  ASM_OUTPUT_DEBUG_LABEL (asm_out_file, PROLOGUE_END_LABEL,
			  current_function_funcdef_no);
  cfun->fde->dw_fde_vms_end_prologue = xstrdup (label);
}

/* Output a marker (i.e. a label) for the beginning of the generated code
   for a function epilogue.  This gets called *before* the prologue code has
   been generated.  */

void
dwarf2out_vms_begin_epilogue (unsigned int line ATTRIBUTE_UNUSED,
			  const char *file ATTRIBUTE_UNUSED)
{
  dw_fde_ref fde = cfun->fde;
  char label[MAX_ARTIFICIAL_LABEL_BYTES];

  if (fde->dw_fde_vms_begin_epilogue)
    return;

  /* Output a label to mark the endpoint of the code generated for this
     function.  */
  ASM_GENERATE_INTERNAL_LABEL (label, EPILOGUE_BEGIN_LABEL,
			       current_function_funcdef_no);
  ASM_OUTPUT_DEBUG_LABEL (asm_out_file, EPILOGUE_BEGIN_LABEL,
			  current_function_funcdef_no);
  fde->dw_fde_vms_begin_epilogue = xstrdup (label);
}

/* Output a marker (i.e. a label) for the absolute end of the generated code
   for a function definition.  This gets called *after* the epilogue code has
   been generated.  */

void
dwarf2out_end_epilogue (unsigned int line ATTRIBUTE_UNUSED,
			const char *file ATTRIBUTE_UNUSED)
{
  dw_fde_ref fde;
  char label[MAX_ARTIFICIAL_LABEL_BYTES];

  last_var_location_insn = NULL;
  cached_next_real_insn = NULL;

  if (dwarf2out_do_cfi_asm ())
    fprintf (asm_out_file, "\t.cfi_endproc\n");

  /* Output a label to mark the endpoint of the code generated for this
     function.  */
  ASM_GENERATE_INTERNAL_LABEL (label, FUNC_END_LABEL,
			       current_function_funcdef_no);
  ASM_OUTPUT_LABEL (asm_out_file, label);
  fde = cfun->fde;
  gcc_assert (fde != NULL);
  if (fde->dw_fde_second_begin == NULL)
    fde->dw_fde_end = xstrdup (label);
}

void
dwarf2out_frame_finish (void)
{
  /* Output call frame information.  */
  if (targetm.debug_unwind_info () == UI_DWARF2)
    output_call_frame_info (0);

  /* Output another copy for the unwinder.  */
  if ((flag_unwind_tables || flag_exceptions)
      && targetm_common.except_unwind_info (&global_options) == UI_DWARF2)
    output_call_frame_info (1);
}

/* Note that the current function section is being used for code.  */

static void
dwarf2out_note_section_used (void)
{
  section *sec = current_function_section ();
  if (sec == text_section)
    text_section_used = true;
  else if (sec == cold_text_section)
    cold_text_section_used = true;
}

static void var_location_switch_text_section (void);
static void set_cur_line_info_table (section *);

void
dwarf2out_switch_text_section (void)
{
  section *sect;
  dw_fde_ref fde = cfun->fde;

  gcc_assert (cfun && fde && fde->dw_fde_second_begin == NULL);

  if (!in_cold_section_p)
    {
      fde->dw_fde_end = crtl->subsections.cold_section_end_label;
      fde->dw_fde_second_begin = crtl->subsections.hot_section_label;
      fde->dw_fde_second_end = crtl->subsections.hot_section_end_label;
    }
  else
    {
      fde->dw_fde_end = crtl->subsections.hot_section_end_label;
      fde->dw_fde_second_begin = crtl->subsections.cold_section_label;
      fde->dw_fde_second_end = crtl->subsections.cold_section_end_label;
    }
  have_multiple_function_sections = true;

  /* There is no need to mark used sections when not debugging.  */
  if (cold_text_section != NULL)
    dwarf2out_note_section_used ();

  if (dwarf2out_do_cfi_asm ())
    fprintf (asm_out_file, "\t.cfi_endproc\n");

  /* Now do the real section switch.  */
  sect = current_function_section ();
  switch_to_section (sect);

  fde->second_in_std_section
    = (sect == text_section
       || (cold_text_section && sect == cold_text_section));

  if (dwarf2out_do_cfi_asm ())
    dwarf2out_do_cfi_startproc (true);

  var_location_switch_text_section ();

  if (cold_text_section != NULL)
    set_cur_line_info_table (sect);
}

/* And now, the subset of the debugging information support code necessary
   for emitting location expressions.  */

/* Data about a single source file.  */
struct GTY((for_user)) dwarf_file_data {
  const char * filename;
  int emitted_number;
};

/* Describe an entry into the .debug_addr section.  */

enum ate_kind {
  ate_kind_rtx,
  ate_kind_rtx_dtprel,
  ate_kind_label
};

struct GTY((for_user)) addr_table_entry {
  enum ate_kind kind;
  unsigned int refcount;
  unsigned int index;
  union addr_table_entry_struct_union
    {
      rtx GTY ((tag ("0"))) rtl;
      char * GTY ((tag ("1"))) label;
    }
  GTY ((desc ("%1.kind"))) addr;
};

/* Location lists are ranges + location descriptions for that range,
   so you can track variables that are in different places over
   their entire life.  */
typedef struct GTY(()) dw_loc_list_struct {
  dw_loc_list_ref dw_loc_next;
  const char *begin; /* Label and addr_entry for start of range */
  addr_table_entry *begin_entry;
  const char *end;  /* Label for end of range */
  char *ll_symbol; /* Label for beginning of location list.
		      Only on head of list */
  const char *section; /* Section this loclist is relative to */
  dw_loc_descr_ref expr;
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

static dw_loc_descr_ref int_loc_descriptor (HOST_WIDE_INT);
static dw_loc_descr_ref uint_loc_descriptor (unsigned HOST_WIDE_INT);

/* Convert a DWARF stack opcode into its string name.  */

static const char *
dwarf_stack_op_name (unsigned int op)
{
  const char *name = get_DW_OP_name (op);

  if (name != NULL)
    return name;

  return "OP_<unknown>";
}

/* Return a pointer to a newly allocated location description.  Location
   descriptions are simple expression terms that can be strung
   together to form more complicated location (address) descriptions.  */

static inline dw_loc_descr_ref
new_loc_descr (enum dwarf_location_atom op, unsigned HOST_WIDE_INT oprnd1,
	       unsigned HOST_WIDE_INT oprnd2)
{
  dw_loc_descr_ref descr = ggc_cleared_alloc<dw_loc_descr_node> ();

  descr->dw_loc_opc = op;
  descr->dw_loc_oprnd1.val_class = dw_val_class_unsigned_const;
  descr->dw_loc_oprnd1.val_entry = NULL;
  descr->dw_loc_oprnd1.v.val_unsigned = oprnd1;
  descr->dw_loc_oprnd2.val_class = dw_val_class_unsigned_const;
  descr->dw_loc_oprnd2.val_entry = NULL;
  descr->dw_loc_oprnd2.v.val_unsigned = oprnd2;

  return descr;
}

/* Return a pointer to a newly allocated location description for
   REG and OFFSET.  */

static inline dw_loc_descr_ref
new_reg_loc_descr (unsigned int reg,  unsigned HOST_WIDE_INT offset)
{
  if (reg <= 31)
    return new_loc_descr ((enum dwarf_location_atom) (DW_OP_breg0 + reg),
			  offset, 0);
  else
    return new_loc_descr (DW_OP_bregx, reg, offset);
}

/* Add a location description term to a location description expression.  */

static inline void
add_loc_descr (dw_loc_descr_ref *list_head, dw_loc_descr_ref descr)
{
  dw_loc_descr_ref *d;

  /* Find the end of the chain.  */
  for (d = list_head; (*d) != NULL; d = &(*d)->dw_loc_next)
    ;

  *d = descr;
}

/* Compare two location operands for exact equality.  */

static bool
dw_val_equal_p (dw_val_node *a, dw_val_node *b)
{
  if (a->val_class != b->val_class)
    return false;
  switch (a->val_class)
    {
    case dw_val_class_none:
      return true;
    case dw_val_class_addr:
      return rtx_equal_p (a->v.val_addr, b->v.val_addr);

    case dw_val_class_offset:
    case dw_val_class_unsigned_const:
    case dw_val_class_const:
    case dw_val_class_unsigned_const_implicit:
    case dw_val_class_const_implicit:
    case dw_val_class_range_list:
      /* These are all HOST_WIDE_INT, signed or unsigned.  */
      return a->v.val_unsigned == b->v.val_unsigned;

    case dw_val_class_loc:
      return a->v.val_loc == b->v.val_loc;
    case dw_val_class_loc_list:
      return a->v.val_loc_list == b->v.val_loc_list;
    case dw_val_class_die_ref:
      return a->v.val_die_ref.die == b->v.val_die_ref.die;
    case dw_val_class_fde_ref:
      return a->v.val_fde_index == b->v.val_fde_index;
    case dw_val_class_lbl_id:
    case dw_val_class_lineptr:
    case dw_val_class_macptr:
    case dw_val_class_loclistsptr:
    case dw_val_class_high_pc:
      return strcmp (a->v.val_lbl_id, b->v.val_lbl_id) == 0;
    case dw_val_class_str:
      return a->v.val_str == b->v.val_str;
    case dw_val_class_flag:
      return a->v.val_flag == b->v.val_flag;
    case dw_val_class_file:
    case dw_val_class_file_implicit:
      return a->v.val_file == b->v.val_file;
    case dw_val_class_decl_ref:
      return a->v.val_decl_ref == b->v.val_decl_ref;
    
    case dw_val_class_const_double:
      return (a->v.val_double.high == b->v.val_double.high
	      && a->v.val_double.low == b->v.val_double.low);

    case dw_val_class_wide_int:
      return *a->v.val_wide == *b->v.val_wide;

    case dw_val_class_vec:
      {
	size_t a_len = a->v.val_vec.elt_size * a->v.val_vec.length;
	size_t b_len = b->v.val_vec.elt_size * b->v.val_vec.length;

	return (a_len == b_len
		&& !memcmp (a->v.val_vec.array, b->v.val_vec.array, a_len));
      }

    case dw_val_class_data8:
      return memcmp (a->v.val_data8, b->v.val_data8, 8) == 0;

    case dw_val_class_vms_delta:
      return (!strcmp (a->v.val_vms_delta.lbl1, b->v.val_vms_delta.lbl1)
              && !strcmp (a->v.val_vms_delta.lbl1, b->v.val_vms_delta.lbl1));

    case dw_val_class_discr_value:
      return (a->v.val_discr_value.pos == b->v.val_discr_value.pos
	      && a->v.val_discr_value.v.uval == b->v.val_discr_value.v.uval);
    case dw_val_class_discr_list:
      /* It makes no sense comparing two discriminant value lists.  */
      return false;
    }
  gcc_unreachable ();
}

/* Compare two location atoms for exact equality.  */

static bool
loc_descr_equal_p_1 (dw_loc_descr_ref a, dw_loc_descr_ref b)
{
  if (a->dw_loc_opc != b->dw_loc_opc)
    return false;

  /* ??? This is only ever set for DW_OP_constNu, for N equal to the
     address size, but since we always allocate cleared storage it
     should be zero for other types of locations.  */
  if (a->dtprel != b->dtprel)
    return false;

  return (dw_val_equal_p (&a->dw_loc_oprnd1, &b->dw_loc_oprnd1)
	  && dw_val_equal_p (&a->dw_loc_oprnd2, &b->dw_loc_oprnd2));
}

/* Compare two complete location expressions for exact equality.  */

bool
loc_descr_equal_p (dw_loc_descr_ref a, dw_loc_descr_ref b)
{
  while (1)
    {
      if (a == b)
	return true;
      if (a == NULL || b == NULL)
	return false;
      if (!loc_descr_equal_p_1 (a, b))
	return false;

      a = a->dw_loc_next;
      b = b->dw_loc_next;
    }
}


/* Add a constant OFFSET to a location expression.  */

static void
loc_descr_plus_const (dw_loc_descr_ref *list_head, HOST_WIDE_INT offset)
{
  dw_loc_descr_ref loc;
  HOST_WIDE_INT *p;

  gcc_assert (*list_head != NULL);

  if (!offset)
    return;

  /* Find the end of the chain.  */
  for (loc = *list_head; loc->dw_loc_next != NULL; loc = loc->dw_loc_next)
    ;

  p = NULL;
  if (loc->dw_loc_opc == DW_OP_fbreg
      || (loc->dw_loc_opc >= DW_OP_breg0 && loc->dw_loc_opc <= DW_OP_breg31))
    p = &loc->dw_loc_oprnd1.v.val_int;
  else if (loc->dw_loc_opc == DW_OP_bregx)
    p = &loc->dw_loc_oprnd2.v.val_int;

  /* If the last operation is fbreg, breg{0..31,x}, optimize by adjusting its
     offset.  Don't optimize if an signed integer overflow would happen.  */
  if (p != NULL
      && ((offset > 0 && *p <= INTTYPE_MAXIMUM (HOST_WIDE_INT) - offset)
	  || (offset < 0 && *p >= INTTYPE_MINIMUM (HOST_WIDE_INT) - offset)))
    *p += offset;

  else if (offset > 0)
    loc->dw_loc_next = new_loc_descr (DW_OP_plus_uconst, offset, 0);

  else
    {
      loc->dw_loc_next
	= uint_loc_descriptor (-(unsigned HOST_WIDE_INT) offset);
      add_loc_descr (&loc->dw_loc_next, new_loc_descr (DW_OP_minus, 0, 0));
    }
}

/* Add a constant OFFSET to a location list.  */

static void
loc_list_plus_const (dw_loc_list_ref list_head, HOST_WIDE_INT offset)
{
  dw_loc_list_ref d;
  for (d = list_head; d != NULL; d = d->dw_loc_next)
    loc_descr_plus_const (&d->expr, offset);
}

#define DWARF_REF_SIZE	\
  (dwarf_version == 2 ? DWARF2_ADDR_SIZE : DWARF_OFFSET_SIZE)

/* The number of bits that can be encoded by largest DW_FORM_dataN.
   In DWARF4 and earlier it is DW_FORM_data8 with 64 bits, in DWARF5
   DW_FORM_data16 with 128 bits.  */
#define DWARF_LARGEST_DATA_FORM_BITS \
  (dwarf_version >= 5 ? 128 : 64)

/* Utility inline function for construction of ops that were GNU extension
   before DWARF 5.  */
static inline enum dwarf_location_atom
dwarf_OP (enum dwarf_location_atom op)
{
  switch (op)
    {
    case DW_OP_implicit_pointer:
      if (dwarf_version < 5)
	return DW_OP_GNU_implicit_pointer;
      break;

    case DW_OP_entry_value:
      if (dwarf_version < 5)
	return DW_OP_GNU_entry_value;
      break;

    case DW_OP_const_type:
      if (dwarf_version < 5)
	return DW_OP_GNU_const_type;
      break;

    case DW_OP_regval_type:
      if (dwarf_version < 5)
	return DW_OP_GNU_regval_type;
      break;

    case DW_OP_deref_type:
      if (dwarf_version < 5)
	return DW_OP_GNU_deref_type;
      break;

    case DW_OP_convert:
      if (dwarf_version < 5)
	return DW_OP_GNU_convert;
      break;

    case DW_OP_reinterpret:
      if (dwarf_version < 5)
	return DW_OP_GNU_reinterpret;
      break;

    default:
      break;
    }
  return op;
}

/* Similarly for attributes.  */
static inline enum dwarf_attribute
dwarf_AT (enum dwarf_attribute at)
{
  switch (at)
    {
    case DW_AT_call_return_pc:
      if (dwarf_version < 5)
	return DW_AT_low_pc;
      break;

    case DW_AT_call_tail_call:
      if (dwarf_version < 5)
	return DW_AT_GNU_tail_call;
      break;

    case DW_AT_call_origin:
      if (dwarf_version < 5)
	return DW_AT_abstract_origin;
      break;

    case DW_AT_call_target:
      if (dwarf_version < 5)
	return DW_AT_GNU_call_site_target;
      break;

    case DW_AT_call_target_clobbered:
      if (dwarf_version < 5)
	return DW_AT_GNU_call_site_target_clobbered;
      break;

    case DW_AT_call_parameter:
      if (dwarf_version < 5)
	return DW_AT_abstract_origin;
      break;

    case DW_AT_call_value:
      if (dwarf_version < 5)
	return DW_AT_GNU_call_site_value;
      break;

    case DW_AT_call_data_value:
      if (dwarf_version < 5)
	return DW_AT_GNU_call_site_data_value;
      break;

    case DW_AT_call_all_calls:
      if (dwarf_version < 5)
	return DW_AT_GNU_all_call_sites;
      break;

    case DW_AT_call_all_tail_calls:
      if (dwarf_version < 5)
	return DW_AT_GNU_all_tail_call_sites;
      break;

    case DW_AT_dwo_name:
      if (dwarf_version < 5)
	return DW_AT_GNU_dwo_name;
      break;

    default:
      break;
    }
  return at;
}

/* And similarly for tags.  */
static inline enum dwarf_tag
dwarf_TAG (enum dwarf_tag tag)
{
  switch (tag)
    {
    case DW_TAG_call_site:
      if (dwarf_version < 5)
	return DW_TAG_GNU_call_site;
      break;

    case DW_TAG_call_site_parameter:
      if (dwarf_version < 5)
	return DW_TAG_GNU_call_site_parameter;
      break;

    default:
      break;
    }
  return tag;
}

static unsigned long int get_base_type_offset (dw_die_ref);

/* Return the size of a location descriptor.  */

static unsigned long
size_of_loc_descr (dw_loc_descr_ref loc)
{
  unsigned long size = 1;

  switch (loc->dw_loc_opc)
    {
    case DW_OP_addr:
      size += DWARF2_ADDR_SIZE;
      break;
    case DW_OP_GNU_addr_index:
    case DW_OP_GNU_const_index:
      gcc_assert (loc->dw_loc_oprnd1.val_entry->index != NO_INDEX_ASSIGNED);
      size += size_of_uleb128 (loc->dw_loc_oprnd1.val_entry->index);
      break;
    case DW_OP_const1u:
    case DW_OP_const1s:
      size += 1;
      break;
    case DW_OP_const2u:
    case DW_OP_const2s:
      size += 2;
      break;
    case DW_OP_const4u:
    case DW_OP_const4s:
      size += 4;
      break;
    case DW_OP_const8u:
    case DW_OP_const8s:
      size += 8;
      break;
    case DW_OP_constu:
      size += size_of_uleb128 (loc->dw_loc_oprnd1.v.val_unsigned);
      break;
    case DW_OP_consts:
      size += size_of_sleb128 (loc->dw_loc_oprnd1.v.val_int);
      break;
    case DW_OP_pick:
      size += 1;
      break;
    case DW_OP_plus_uconst:
      size += size_of_uleb128 (loc->dw_loc_oprnd1.v.val_unsigned);
      break;
    case DW_OP_skip:
    case DW_OP_bra:
      size += 2;
      break;
    case DW_OP_breg0:
    case DW_OP_breg1:
    case DW_OP_breg2:
    case DW_OP_breg3:
    case DW_OP_breg4:
    case DW_OP_breg5:
    case DW_OP_breg6:
    case DW_OP_breg7:
    case DW_OP_breg8:
    case DW_OP_breg9:
    case DW_OP_breg10:
    case DW_OP_breg11:
    case DW_OP_breg12:
    case DW_OP_breg13:
    case DW_OP_breg14:
    case DW_OP_breg15:
    case DW_OP_breg16:
    case DW_OP_breg17:
    case DW_OP_breg18:
    case DW_OP_breg19:
    case DW_OP_breg20:
    case DW_OP_breg21:
    case DW_OP_breg22:
    case DW_OP_breg23:
    case DW_OP_breg24:
    case DW_OP_breg25:
    case DW_OP_breg26:
    case DW_OP_breg27:
    case DW_OP_breg28:
    case DW_OP_breg29:
    case DW_OP_breg30:
    case DW_OP_breg31:
      size += size_of_sleb128 (loc->dw_loc_oprnd1.v.val_int);
      break;
    case DW_OP_regx:
      size += size_of_uleb128 (loc->dw_loc_oprnd1.v.val_unsigned);
      break;
    case DW_OP_fbreg:
      size += size_of_sleb128 (loc->dw_loc_oprnd1.v.val_int);
      break;
    case DW_OP_bregx:
      size += size_of_uleb128 (loc->dw_loc_oprnd1.v.val_unsigned);
      size += size_of_sleb128 (loc->dw_loc_oprnd2.v.val_int);
      break;
    case DW_OP_piece:
      size += size_of_uleb128 (loc->dw_loc_oprnd1.v.val_unsigned);
      break;
    case DW_OP_bit_piece:
      size += size_of_uleb128 (loc->dw_loc_oprnd1.v.val_unsigned);
      size += size_of_uleb128 (loc->dw_loc_oprnd2.v.val_unsigned);
      break;
    case DW_OP_deref_size:
    case DW_OP_xderef_size:
      size += 1;
      break;
    case DW_OP_call2:
      size += 2;
      break;
    case DW_OP_call4:
      size += 4;
      break;
    case DW_OP_call_ref:
    case DW_OP_GNU_variable_value:
      size += DWARF_REF_SIZE;
      break;
    case DW_OP_implicit_value:
      size += size_of_uleb128 (loc->dw_loc_oprnd1.v.val_unsigned)
	      + loc->dw_loc_oprnd1.v.val_unsigned;
      break;
    case DW_OP_implicit_pointer:
    case DW_OP_GNU_implicit_pointer:
      size += DWARF_REF_SIZE + size_of_sleb128 (loc->dw_loc_oprnd2.v.val_int);
      break;
    case DW_OP_entry_value:
    case DW_OP_GNU_entry_value:
      {
	unsigned long op_size = size_of_locs (loc->dw_loc_oprnd1.v.val_loc);
	size += size_of_uleb128 (op_size) + op_size;
	break;
      }
    case DW_OP_const_type:
    case DW_OP_GNU_const_type:
      {
	unsigned long o
	  = get_base_type_offset (loc->dw_loc_oprnd1.v.val_die_ref.die);
	size += size_of_uleb128 (o) + 1;
	switch (loc->dw_loc_oprnd2.val_class)
	  {
	  case dw_val_class_vec:
	    size += loc->dw_loc_oprnd2.v.val_vec.length
		    * loc->dw_loc_oprnd2.v.val_vec.elt_size;
	    break;
	  case dw_val_class_const:
	    size += HOST_BITS_PER_WIDE_INT / BITS_PER_UNIT;
	    break;
	  case dw_val_class_const_double:
	    size += HOST_BITS_PER_DOUBLE_INT / BITS_PER_UNIT;
	    break;
	  case dw_val_class_wide_int:
	    size += (get_full_len (*loc->dw_loc_oprnd2.v.val_wide)
		     * HOST_BITS_PER_WIDE_INT / BITS_PER_UNIT);
	    break;
	  default:
	    gcc_unreachable ();
	  }
	break;
      }
    case DW_OP_regval_type:
    case DW_OP_GNU_regval_type:
      {
	unsigned long o
	  = get_base_type_offset (loc->dw_loc_oprnd2.v.val_die_ref.die);
	size += size_of_uleb128 (loc->dw_loc_oprnd1.v.val_unsigned)
		+ size_of_uleb128 (o);
      }
      break;
    case DW_OP_deref_type:
    case DW_OP_GNU_deref_type:
      {
	unsigned long o
	  = get_base_type_offset (loc->dw_loc_oprnd2.v.val_die_ref.die);
	size += 1 + size_of_uleb128 (o);
      }
      break;
    case DW_OP_convert:
    case DW_OP_reinterpret:
    case DW_OP_GNU_convert:
    case DW_OP_GNU_reinterpret:
      if (loc->dw_loc_oprnd1.val_class == dw_val_class_unsigned_const)
	size += size_of_uleb128 (loc->dw_loc_oprnd1.v.val_unsigned);
      else
	{
	  unsigned long o
	    = get_base_type_offset (loc->dw_loc_oprnd1.v.val_die_ref.die);
	  size += size_of_uleb128 (o);
	}
      break;
    case DW_OP_GNU_parameter_ref:
      size += 4;
      break;
    default:
      break;
    }

  return size;
}

/* Return the size of a series of location descriptors.  */

unsigned long
size_of_locs (dw_loc_descr_ref loc)
{
  dw_loc_descr_ref l;
  unsigned long size;

  /* If there are no skip or bra opcodes, don't fill in the dw_loc_addr
     field, to avoid writing to a PCH file.  */
  for (size = 0, l = loc; l != NULL; l = l->dw_loc_next)
    {
      if (l->dw_loc_opc == DW_OP_skip || l->dw_loc_opc == DW_OP_bra)
	break;
      size += size_of_loc_descr (l);
    }
  if (! l)
    return size;

  for (size = 0, l = loc; l != NULL; l = l->dw_loc_next)
    {
      l->dw_loc_addr = size;
      size += size_of_loc_descr (l);
    }

  return size;
}

/* Return the size of the value in a DW_AT_discr_value attribute.  */

static int
size_of_discr_value (dw_discr_value *discr_value)
{
  if (discr_value->pos)
    return size_of_uleb128 (discr_value->v.uval);
  else
    return size_of_sleb128 (discr_value->v.sval);
}

/* Return the size of the value in a DW_AT_discr_list attribute.  */

static int
size_of_discr_list (dw_discr_list_ref discr_list)
{
  int size = 0;

  for (dw_discr_list_ref list = discr_list;
       list != NULL;
       list = list->dw_discr_next)
    {
      /* One byte for the discriminant value descriptor, and then one or two
	 LEB128 numbers, depending on whether it's a single case label or a
	 range label.  */
      size += 1;
      size += size_of_discr_value (&list->dw_discr_lower_bound);
      if (list->dw_discr_range != 0)
	size += size_of_discr_value (&list->dw_discr_upper_bound);
    }
  return size;
}

static HOST_WIDE_INT extract_int (const unsigned char *, unsigned);
static void get_ref_die_offset_label (char *, dw_die_ref);
static unsigned long int get_ref_die_offset (dw_die_ref);

/* Output location description stack opcode's operands (if any).
   The for_eh_or_skip parameter controls whether register numbers are
   converted using DWARF2_FRAME_REG_OUT, which is needed in the case that
   hard reg numbers have been processed via DWARF_FRAME_REGNUM (i.e. for unwind
   info).  This should be suppressed for the cases that have not been converted
   (i.e. symbolic debug info), by setting the parameter < 0.  See PR47324.  */

static void
output_loc_operands (dw_loc_descr_ref loc, int for_eh_or_skip)
{
  dw_val_ref val1 = &loc->dw_loc_oprnd1;
  dw_val_ref val2 = &loc->dw_loc_oprnd2;

  switch (loc->dw_loc_opc)
    {
#ifdef DWARF2_DEBUGGING_INFO
    case DW_OP_const2u:
    case DW_OP_const2s:
      dw2_asm_output_data (2, val1->v.val_int, NULL);
      break;
    case DW_OP_const4u:
      if (loc->dtprel)
	{
	  gcc_assert (targetm.asm_out.output_dwarf_dtprel);
	  targetm.asm_out.output_dwarf_dtprel (asm_out_file, 4,
					       val1->v.val_addr);
	  fputc ('\n', asm_out_file);
	  break;
	}
      /* FALLTHRU */
    case DW_OP_const4s:
      dw2_asm_output_data (4, val1->v.val_int, NULL);
      break;
    case DW_OP_const8u:
      if (loc->dtprel)
	{
	  gcc_assert (targetm.asm_out.output_dwarf_dtprel);
	  targetm.asm_out.output_dwarf_dtprel (asm_out_file, 8,
					       val1->v.val_addr);
	  fputc ('\n', asm_out_file);
	  break;
	}
      /* FALLTHRU */
    case DW_OP_const8s:
      gcc_assert (HOST_BITS_PER_WIDE_INT >= 64);
      dw2_asm_output_data (8, val1->v.val_int, NULL);
      break;
    case DW_OP_skip:
    case DW_OP_bra:
      {
	int offset;

	gcc_assert (val1->val_class == dw_val_class_loc);
	offset = val1->v.val_loc->dw_loc_addr - (loc->dw_loc_addr + 3);

	dw2_asm_output_data (2, offset, NULL);
      }
      break;
    case DW_OP_implicit_value:
      dw2_asm_output_data_uleb128 (val1->v.val_unsigned, NULL);
      switch (val2->val_class)
	{
	case dw_val_class_const:
	  dw2_asm_output_data (val1->v.val_unsigned, val2->v.val_int, NULL);
	  break;
	case dw_val_class_vec:
	  {
	    unsigned int elt_size = val2->v.val_vec.elt_size;
	    unsigned int len = val2->v.val_vec.length;
	    unsigned int i;
	    unsigned char *p;

	    if (elt_size > sizeof (HOST_WIDE_INT))
	      {
		elt_size /= 2;
		len *= 2;
	      }
	    for (i = 0, p = (unsigned char *) val2->v.val_vec.array;
		 i < len;
		 i++, p += elt_size)
	      dw2_asm_output_data (elt_size, extract_int (p, elt_size),
				   "fp or vector constant word %u", i);
	  }
	  break;
	case dw_val_class_const_double:
	  {
	    unsigned HOST_WIDE_INT first, second;

	    if (WORDS_BIG_ENDIAN)
	      {
		first = val2->v.val_double.high;
		second = val2->v.val_double.low;
	      }
	    else
	      {
		first = val2->v.val_double.low;
		second = val2->v.val_double.high;
	      }
	    dw2_asm_output_data (HOST_BITS_PER_WIDE_INT / HOST_BITS_PER_CHAR,
				 first, NULL);
	    dw2_asm_output_data (HOST_BITS_PER_WIDE_INT / HOST_BITS_PER_CHAR,
				 second, NULL);
	  }
	  break;
	case dw_val_class_wide_int:
	  {
	    int i;
	    int len = get_full_len (*val2->v.val_wide);
	    if (WORDS_BIG_ENDIAN)
	      for (i = len - 1; i >= 0; --i)
		dw2_asm_output_data (HOST_BITS_PER_WIDE_INT / HOST_BITS_PER_CHAR,
				     val2->v.val_wide->elt (i), NULL);
	    else
	      for (i = 0; i < len; ++i)
		dw2_asm_output_data (HOST_BITS_PER_WIDE_INT / HOST_BITS_PER_CHAR,
				     val2->v.val_wide->elt (i), NULL);
	  }
	  break;
	case dw_val_class_addr:
	  gcc_assert (val1->v.val_unsigned == DWARF2_ADDR_SIZE);
	  dw2_asm_output_addr_rtx (DWARF2_ADDR_SIZE, val2->v.val_addr, NULL);
	  break;
	default:
	  gcc_unreachable ();
	}
      break;
#else
    case DW_OP_const2u:
    case DW_OP_const2s:
    case DW_OP_const4u:
    case DW_OP_const4s:
    case DW_OP_const8u:
    case DW_OP_const8s:
    case DW_OP_skip:
    case DW_OP_bra:
    case DW_OP_implicit_value:
      /* We currently don't make any attempt to make sure these are
	 aligned properly like we do for the main unwind info, so
	 don't support emitting things larger than a byte if we're
	 only doing unwinding.  */
      gcc_unreachable ();
#endif
    case DW_OP_const1u:
    case DW_OP_const1s:
      dw2_asm_output_data (1, val1->v.val_int, NULL);
      break;
    case DW_OP_constu:
      dw2_asm_output_data_uleb128 (val1->v.val_unsigned, NULL);
      break;
    case DW_OP_consts:
      dw2_asm_output_data_sleb128 (val1->v.val_int, NULL);
      break;
    case DW_OP_pick:
      dw2_asm_output_data (1, val1->v.val_int, NULL);
      break;
    case DW_OP_plus_uconst:
      dw2_asm_output_data_uleb128 (val1->v.val_unsigned, NULL);
      break;
    case DW_OP_breg0:
    case DW_OP_breg1:
    case DW_OP_breg2:
    case DW_OP_breg3:
    case DW_OP_breg4:
    case DW_OP_breg5:
    case DW_OP_breg6:
    case DW_OP_breg7:
    case DW_OP_breg8:
    case DW_OP_breg9:
    case DW_OP_breg10:
    case DW_OP_breg11:
    case DW_OP_breg12:
    case DW_OP_breg13:
    case DW_OP_breg14:
    case DW_OP_breg15:
    case DW_OP_breg16:
    case DW_OP_breg17:
    case DW_OP_breg18:
    case DW_OP_breg19:
    case DW_OP_breg20:
    case DW_OP_breg21:
    case DW_OP_breg22:
    case DW_OP_breg23:
    case DW_OP_breg24:
    case DW_OP_breg25:
    case DW_OP_breg26:
    case DW_OP_breg27:
    case DW_OP_breg28:
    case DW_OP_breg29:
    case DW_OP_breg30:
    case DW_OP_breg31:
      dw2_asm_output_data_sleb128 (val1->v.val_int, NULL);
      break;
    case DW_OP_regx:
      {
	unsigned r = val1->v.val_unsigned;
	if (for_eh_or_skip >= 0)
	  r = DWARF2_FRAME_REG_OUT (r, for_eh_or_skip);
	gcc_assert (size_of_uleb128 (r) 
		    == size_of_uleb128 (val1->v.val_unsigned));
	dw2_asm_output_data_uleb128 (r, NULL);	
      }
      break;
    case DW_OP_fbreg:
      dw2_asm_output_data_sleb128 (val1->v.val_int, NULL);
      break;
    case DW_OP_bregx:
      {
	unsigned r = val1->v.val_unsigned;
	if (for_eh_or_skip >= 0)
	  r = DWARF2_FRAME_REG_OUT (r, for_eh_or_skip);
	gcc_assert (size_of_uleb128 (r) 
		    == size_of_uleb128 (val1->v.val_unsigned));
	dw2_asm_output_data_uleb128 (r, NULL);	
	dw2_asm_output_data_sleb128 (val2->v.val_int, NULL);
      }
      break;
    case DW_OP_piece:
      dw2_asm_output_data_uleb128 (val1->v.val_unsigned, NULL);
      break;
    case DW_OP_bit_piece:
      dw2_asm_output_data_uleb128 (val1->v.val_unsigned, NULL);
      dw2_asm_output_data_uleb128 (val2->v.val_unsigned, NULL);
      break;
    case DW_OP_deref_size:
    case DW_OP_xderef_size:
      dw2_asm_output_data (1, val1->v.val_int, NULL);
      break;

    case DW_OP_addr:
      if (loc->dtprel)
	{
	  if (targetm.asm_out.output_dwarf_dtprel)
	    {
	      targetm.asm_out.output_dwarf_dtprel (asm_out_file,
						   DWARF2_ADDR_SIZE,
						   val1->v.val_addr);
	      fputc ('\n', asm_out_file);
	    }
	  else
	    gcc_unreachable ();
	}
      else
	{
#ifdef DWARF2_DEBUGGING_INFO
	  dw2_asm_output_addr_rtx (DWARF2_ADDR_SIZE, val1->v.val_addr, NULL);
#else
	  gcc_unreachable ();
#endif
	}
      break;

    case DW_OP_GNU_addr_index:
    case DW_OP_GNU_const_index:
      gcc_assert (loc->dw_loc_oprnd1.val_entry->index != NO_INDEX_ASSIGNED);
      dw2_asm_output_data_uleb128 (loc->dw_loc_oprnd1.val_entry->index,
                                   "(index into .debug_addr)");
      break;

    case DW_OP_call2:
    case DW_OP_call4:
      {
	unsigned long die_offset
	  = get_ref_die_offset (val1->v.val_die_ref.die);
	/* Make sure the offset has been computed and that we can encode it as
	   an operand.  */
	gcc_assert (die_offset > 0
		    && die_offset <= (loc->dw_loc_opc == DW_OP_call2
				     ? 0xffff
				     : 0xffffffff));
	dw2_asm_output_data ((loc->dw_loc_opc == DW_OP_call2) ? 2 : 4,
			     die_offset, NULL);
      }
      break;

    case DW_OP_call_ref:
    case DW_OP_GNU_variable_value:
      {
	char label[MAX_ARTIFICIAL_LABEL_BYTES
		   + HOST_BITS_PER_WIDE_INT / 2 + 2];
	gcc_assert (val1->val_class == dw_val_class_die_ref);
	get_ref_die_offset_label (label, val1->v.val_die_ref.die);
	dw2_asm_output_offset (DWARF_REF_SIZE, label, debug_info_section, NULL);
      }
      break;

    case DW_OP_implicit_pointer:
    case DW_OP_GNU_implicit_pointer:
      {
	char label[MAX_ARTIFICIAL_LABEL_BYTES
		   + HOST_BITS_PER_WIDE_INT / 2 + 2];
	gcc_assert (val1->val_class == dw_val_class_die_ref);
	get_ref_die_offset_label (label, val1->v.val_die_ref.die);
	dw2_asm_output_offset (DWARF_REF_SIZE, label, debug_info_section, NULL);
	dw2_asm_output_data_sleb128 (val2->v.val_int, NULL);
      }
      break;

    case DW_OP_entry_value:
    case DW_OP_GNU_entry_value:
      dw2_asm_output_data_uleb128 (size_of_locs (val1->v.val_loc), NULL);
      output_loc_sequence (val1->v.val_loc, for_eh_or_skip);
      break;

    case DW_OP_const_type:
    case DW_OP_GNU_const_type:
      {
	unsigned long o = get_base_type_offset (val1->v.val_die_ref.die), l;
	gcc_assert (o);
	dw2_asm_output_data_uleb128 (o, NULL);
	switch (val2->val_class)
	  {
	  case dw_val_class_const:
	    l = HOST_BITS_PER_WIDE_INT / HOST_BITS_PER_CHAR;
	    dw2_asm_output_data (1, l, NULL);
	    dw2_asm_output_data (l, val2->v.val_int, NULL);
	    break;
	  case dw_val_class_vec:
	    {
	      unsigned int elt_size = val2->v.val_vec.elt_size;
	      unsigned int len = val2->v.val_vec.length;
	      unsigned int i;
	      unsigned char *p;

	      l = len * elt_size;
	      dw2_asm_output_data (1, l, NULL);
	      if (elt_size > sizeof (HOST_WIDE_INT))
		{
		  elt_size /= 2;
		  len *= 2;
		}
	      for (i = 0, p = (unsigned char *) val2->v.val_vec.array;
		   i < len;
		   i++, p += elt_size)
		dw2_asm_output_data (elt_size, extract_int (p, elt_size),
				     "fp or vector constant word %u", i);
	    }
	    break;
	  case dw_val_class_const_double:
	    {
	      unsigned HOST_WIDE_INT first, second;
	      l = HOST_BITS_PER_WIDE_INT / HOST_BITS_PER_CHAR;

	      dw2_asm_output_data (1, 2 * l, NULL);
	      if (WORDS_BIG_ENDIAN)
		{
		  first = val2->v.val_double.high;
		  second = val2->v.val_double.low;
		}
	      else
		{
		  first = val2->v.val_double.low;
		  second = val2->v.val_double.high;
		}
	      dw2_asm_output_data (l, first, NULL);
	      dw2_asm_output_data (l, second, NULL);
	    }
	    break;
	  case dw_val_class_wide_int:
	    {
	      int i;
	      int len = get_full_len (*val2->v.val_wide);
	      l = HOST_BITS_PER_WIDE_INT / HOST_BITS_PER_CHAR;

	      dw2_asm_output_data (1, len * l, NULL);
	      if (WORDS_BIG_ENDIAN)
		for (i = len - 1; i >= 0; --i)
		  dw2_asm_output_data (l, val2->v.val_wide->elt (i), NULL);
	      else
		for (i = 0; i < len; ++i)
		  dw2_asm_output_data (l, val2->v.val_wide->elt (i), NULL);
	    }
	    break;
	  default:
	    gcc_unreachable ();
	  }
      }
      break;
    case DW_OP_regval_type:
    case DW_OP_GNU_regval_type:
      {
	unsigned r = val1->v.val_unsigned;
	unsigned long o = get_base_type_offset (val2->v.val_die_ref.die);
	gcc_assert (o);
	if (for_eh_or_skip >= 0)
	  {
	    r = DWARF2_FRAME_REG_OUT (r, for_eh_or_skip);
	    gcc_assert (size_of_uleb128 (r)
			== size_of_uleb128 (val1->v.val_unsigned));
	  }
	dw2_asm_output_data_uleb128 (r, NULL);
	dw2_asm_output_data_uleb128 (o, NULL);
      }
      break;
    case DW_OP_deref_type:
    case DW_OP_GNU_deref_type:
      {
	unsigned long o = get_base_type_offset (val2->v.val_die_ref.die);
	gcc_assert (o);
	dw2_asm_output_data (1, val1->v.val_int, NULL);
	dw2_asm_output_data_uleb128 (o, NULL);
      }
      break;
    case DW_OP_convert:
    case DW_OP_reinterpret:
    case DW_OP_GNU_convert:
    case DW_OP_GNU_reinterpret:
      if (loc->dw_loc_oprnd1.val_class == dw_val_class_unsigned_const)
	dw2_asm_output_data_uleb128 (val1->v.val_unsigned, NULL);
      else
	{
	  unsigned long o = get_base_type_offset (val1->v.val_die_ref.die);
	  gcc_assert (o);
	  dw2_asm_output_data_uleb128 (o, NULL);
	}
      break;

    case DW_OP_GNU_parameter_ref:
      {
	unsigned long o;
	gcc_assert (val1->val_class == dw_val_class_die_ref);
	o = get_ref_die_offset (val1->v.val_die_ref.die);
	dw2_asm_output_data (4, o, NULL);
      }
      break;

    default:
      /* Other codes have no operands.  */
      break;
    }
}

/* Output a sequence of location operations.  
   The for_eh_or_skip parameter controls whether register numbers are
   converted using DWARF2_FRAME_REG_OUT, which is needed in the case that
   hard reg numbers have been processed via DWARF_FRAME_REGNUM (i.e. for unwind
   info).  This should be suppressed for the cases that have not been converted
   (i.e. symbolic debug info), by setting the parameter < 0.  See PR47324.  */

void
output_loc_sequence (dw_loc_descr_ref loc, int for_eh_or_skip)
{
  for (; loc != NULL; loc = loc->dw_loc_next)
    {
      enum dwarf_location_atom opc = loc->dw_loc_opc;
      /* Output the opcode.  */
      if (for_eh_or_skip >= 0 
          && opc >= DW_OP_breg0 && opc <= DW_OP_breg31)
	{
	  unsigned r = (opc - DW_OP_breg0);
	  r = DWARF2_FRAME_REG_OUT (r, for_eh_or_skip);
	  gcc_assert (r <= 31);
	  opc = (enum dwarf_location_atom) (DW_OP_breg0 + r);
	}
      else if (for_eh_or_skip >= 0 
	       && opc >= DW_OP_reg0 && opc <= DW_OP_reg31)
	{
	  unsigned r = (opc - DW_OP_reg0);
	  r = DWARF2_FRAME_REG_OUT (r, for_eh_or_skip);
	  gcc_assert (r <= 31);
	  opc = (enum dwarf_location_atom) (DW_OP_reg0 + r);
	}

      dw2_asm_output_data (1, opc,
			     "%s", dwarf_stack_op_name (opc));

      /* Output the operand(s) (if any).  */
      output_loc_operands (loc, for_eh_or_skip);
    }
}

/* Output location description stack opcode's operands (if any).
   The output is single bytes on a line, suitable for .cfi_escape.  */

static void
output_loc_operands_raw (dw_loc_descr_ref loc)
{
  dw_val_ref val1 = &loc->dw_loc_oprnd1;
  dw_val_ref val2 = &loc->dw_loc_oprnd2;

  switch (loc->dw_loc_opc)
    {
    case DW_OP_addr:
    case DW_OP_GNU_addr_index:
    case DW_OP_GNU_const_index:
    case DW_OP_implicit_value:
      /* We cannot output addresses in .cfi_escape, only bytes.  */
      gcc_unreachable ();

    case DW_OP_const1u:
    case DW_OP_const1s:
    case DW_OP_pick:
    case DW_OP_deref_size:
    case DW_OP_xderef_size:
      fputc (',', asm_out_file);
      dw2_asm_output_data_raw (1, val1->v.val_int);
      break;

    case DW_OP_const2u:
    case DW_OP_const2s:
      fputc (',', asm_out_file);
      dw2_asm_output_data_raw (2, val1->v.val_int);
      break;

    case DW_OP_const4u:
    case DW_OP_const4s:
      fputc (',', asm_out_file);
      dw2_asm_output_data_raw (4, val1->v.val_int);
      break;

    case DW_OP_const8u:
    case DW_OP_const8s:
      gcc_assert (HOST_BITS_PER_WIDE_INT >= 64);
      fputc (',', asm_out_file);
      dw2_asm_output_data_raw (8, val1->v.val_int);
      break;

    case DW_OP_skip:
    case DW_OP_bra:
      {
	int offset;

	gcc_assert (val1->val_class == dw_val_class_loc);
	offset = val1->v.val_loc->dw_loc_addr - (loc->dw_loc_addr + 3);

        fputc (',', asm_out_file);
	dw2_asm_output_data_raw (2, offset);
      }
      break;

    case DW_OP_regx:
      {
	unsigned r = DWARF2_FRAME_REG_OUT (val1->v.val_unsigned, 1);
	gcc_assert (size_of_uleb128 (r) 
		    == size_of_uleb128 (val1->v.val_unsigned));
	fputc (',', asm_out_file);
	dw2_asm_output_data_uleb128_raw (r);
      }
      break;
      
    case DW_OP_constu:
    case DW_OP_plus_uconst:
    case DW_OP_piece:
      fputc (',', asm_out_file);
      dw2_asm_output_data_uleb128_raw (val1->v.val_unsigned);
      break;

    case DW_OP_bit_piece:
      fputc (',', asm_out_file);
      dw2_asm_output_data_uleb128_raw (val1->v.val_unsigned);
      dw2_asm_output_data_uleb128_raw (val2->v.val_unsigned);
      break;

    case DW_OP_consts:
    case DW_OP_breg0:
    case DW_OP_breg1:
    case DW_OP_breg2:
    case DW_OP_breg3:
    case DW_OP_breg4:
    case DW_OP_breg5:
    case DW_OP_breg6:
    case DW_OP_breg7:
    case DW_OP_breg8:
    case DW_OP_breg9:
    case DW_OP_breg10:
    case DW_OP_breg11:
    case DW_OP_breg12:
    case DW_OP_breg13:
    case DW_OP_breg14:
    case DW_OP_breg15:
    case DW_OP_breg16:
    case DW_OP_breg17:
    case DW_OP_breg18:
    case DW_OP_breg19:
    case DW_OP_breg20:
    case DW_OP_breg21:
    case DW_OP_breg22:
    case DW_OP_breg23:
    case DW_OP_breg24:
    case DW_OP_breg25:
    case DW_OP_breg26:
    case DW_OP_breg27:
    case DW_OP_breg28:
    case DW_OP_breg29:
    case DW_OP_breg30:
    case DW_OP_breg31:
    case DW_OP_fbreg:
      fputc (',', asm_out_file);
      dw2_asm_output_data_sleb128_raw (val1->v.val_int);
      break;

    case DW_OP_bregx:
      {
	unsigned r = DWARF2_FRAME_REG_OUT (val1->v.val_unsigned, 1);
	gcc_assert (size_of_uleb128 (r) 
		    == size_of_uleb128 (val1->v.val_unsigned));
	fputc (',', asm_out_file);
	dw2_asm_output_data_uleb128_raw (r);
	fputc (',', asm_out_file);
	dw2_asm_output_data_sleb128_raw (val2->v.val_int);
      }
      break;

    case DW_OP_implicit_pointer:
    case DW_OP_entry_value:
    case DW_OP_const_type:
    case DW_OP_regval_type:
    case DW_OP_deref_type:
    case DW_OP_convert:
    case DW_OP_reinterpret:
    case DW_OP_GNU_implicit_pointer:
    case DW_OP_GNU_entry_value:
    case DW_OP_GNU_const_type:
    case DW_OP_GNU_regval_type:
    case DW_OP_GNU_deref_type:
    case DW_OP_GNU_convert:
    case DW_OP_GNU_reinterpret:
    case DW_OP_GNU_parameter_ref:
      gcc_unreachable ();
      break;

    default:
      /* Other codes have no operands.  */
      break;
    }
}

void
output_loc_sequence_raw (dw_loc_descr_ref loc)
{
  while (1)
    {
      enum dwarf_location_atom opc = loc->dw_loc_opc;
      /* Output the opcode.  */
      if (opc >= DW_OP_breg0 && opc <= DW_OP_breg31)
	{
	  unsigned r = (opc - DW_OP_breg0);
	  r = DWARF2_FRAME_REG_OUT (r, 1);
	  gcc_assert (r <= 31);
	  opc = (enum dwarf_location_atom) (DW_OP_breg0 + r);
	}
      else if (opc >= DW_OP_reg0 && opc <= DW_OP_reg31)
	{
	  unsigned r = (opc - DW_OP_reg0);
	  r = DWARF2_FRAME_REG_OUT (r, 1);
	  gcc_assert (r <= 31);
	  opc = (enum dwarf_location_atom) (DW_OP_reg0 + r);
	}
      /* Output the opcode.  */
      fprintf (asm_out_file, "%#x", opc);
      output_loc_operands_raw (loc);

      if (!loc->dw_loc_next)
	break;
      loc = loc->dw_loc_next;

      fputc (',', asm_out_file);
    }
}

/* This function builds a dwarf location descriptor sequence from a
   dw_cfa_location, adding the given OFFSET to the result of the
   expression.  */

struct dw_loc_descr_node *
build_cfa_loc (dw_cfa_location *cfa, HOST_WIDE_INT offset)
{
  struct dw_loc_descr_node *head, *tmp;

  offset += cfa->offset;

  if (cfa->indirect)
    {
      head = new_reg_loc_descr (cfa->reg, cfa->base_offset);
      head->dw_loc_oprnd1.val_class = dw_val_class_const;
      head->dw_loc_oprnd1.val_entry = NULL;
      tmp = new_loc_descr (DW_OP_deref, 0, 0);
      add_loc_descr (&head, tmp);
      if (offset != 0)
	{
	  tmp = new_loc_descr (DW_OP_plus_uconst, offset, 0);
	  add_loc_descr (&head, tmp);
	}
    }
  else
    head = new_reg_loc_descr (cfa->reg, offset);

  return head;
}

/* This function builds a dwarf location descriptor sequence for
   the address at OFFSET from the CFA when stack is aligned to
   ALIGNMENT byte.  */

struct dw_loc_descr_node *
build_cfa_aligned_loc (dw_cfa_location *cfa,
		       HOST_WIDE_INT offset, HOST_WIDE_INT alignment)
{
  struct dw_loc_descr_node *head;
  unsigned int dwarf_fp
    = DWARF_FRAME_REGNUM (HARD_FRAME_POINTER_REGNUM);

  /* When CFA is defined as FP+OFFSET, emulate stack alignment.  */
  if (cfa->reg == HARD_FRAME_POINTER_REGNUM && cfa->indirect == 0)
    {
      head = new_reg_loc_descr (dwarf_fp, 0);
      add_loc_descr (&head, int_loc_descriptor (alignment));
      add_loc_descr (&head, new_loc_descr (DW_OP_and, 0, 0));
      loc_descr_plus_const (&head, offset);
    }
  else
    head = new_reg_loc_descr (dwarf_fp, offset);
  return head;
}

/* And now, the support for symbolic debugging information.  */

/* .debug_str support.  */

static void dwarf2out_init (const char *);
static void dwarf2out_finish (const char *);
static void dwarf2out_early_finish (const char *);
static void dwarf2out_assembly_start (void);
static void dwarf2out_define (unsigned int, const char *);
static void dwarf2out_undef (unsigned int, const char *);
static void dwarf2out_start_source_file (unsigned, const char *);
static void dwarf2out_end_source_file (unsigned);
static void dwarf2out_function_decl (tree);
static void dwarf2out_begin_block (unsigned, unsigned);
static void dwarf2out_end_block (unsigned, unsigned);
static bool dwarf2out_ignore_block (const_tree);
static void dwarf2out_early_global_decl (tree);
static void dwarf2out_late_global_decl (tree);
static void dwarf2out_type_decl (tree, int);
static void dwarf2out_imported_module_or_decl (tree, tree, tree, bool, bool);
static void dwarf2out_imported_module_or_decl_1 (tree, tree, tree,
						 dw_die_ref);
static void dwarf2out_abstract_function (tree);
static void dwarf2out_var_location (rtx_insn *);
static void dwarf2out_size_function (tree);
static void dwarf2out_begin_function (tree);
static void dwarf2out_end_function (unsigned int);
static void dwarf2out_register_main_translation_unit (tree unit);
static void dwarf2out_set_name (tree, tree);
static void dwarf2out_register_external_die (tree decl, const char *sym,
					     unsigned HOST_WIDE_INT off);
static bool dwarf2out_die_ref_for_decl (tree decl, const char **sym,
					unsigned HOST_WIDE_INT *off);

/* The debug hooks structure.  */

const struct gcc_debug_hooks dwarf2_debug_hooks =
{
  dwarf2out_init,
  dwarf2out_finish,
  dwarf2out_early_finish,
  dwarf2out_assembly_start,
  dwarf2out_define,
  dwarf2out_undef,
  dwarf2out_start_source_file,
  dwarf2out_end_source_file,
  dwarf2out_begin_block,
  dwarf2out_end_block,
  dwarf2out_ignore_block,
  dwarf2out_source_line,
  dwarf2out_begin_prologue,
#if VMS_DEBUGGING_INFO
  dwarf2out_vms_end_prologue,
  dwarf2out_vms_begin_epilogue,
#else
  debug_nothing_int_charstar,
  debug_nothing_int_charstar,
#endif
  dwarf2out_end_epilogue,
  dwarf2out_begin_function,
  dwarf2out_end_function,	/* end_function */
  dwarf2out_register_main_translation_unit,
  dwarf2out_function_decl,	/* function_decl */
  dwarf2out_early_global_decl,
  dwarf2out_late_global_decl,
  dwarf2out_type_decl,		/* type_decl */
  dwarf2out_imported_module_or_decl,
  dwarf2out_die_ref_for_decl,
  dwarf2out_register_external_die,
  debug_nothing_tree,		/* deferred_inline_function */
  /* The DWARF 2 backend tries to reduce debugging bloat by not
     emitting the abstract description of inline functions until
     something tries to reference them.  */
  dwarf2out_abstract_function,	/* outlining_inline_function */
  debug_nothing_rtx_code_label,	/* label */
  debug_nothing_int,		/* handle_pch */
  dwarf2out_var_location,
  dwarf2out_size_function,	/* size_function */
  dwarf2out_switch_text_section,
  dwarf2out_set_name,
  1,                            /* start_end_main_source_file */
  TYPE_SYMTAB_IS_DIE            /* tree_type_symtab_field */
};

const struct gcc_debug_hooks dwarf2_lineno_debug_hooks =
{
  dwarf2out_init,
  debug_nothing_charstar,
  debug_nothing_charstar,
  dwarf2out_assembly_start,
  debug_nothing_int_charstar,
  debug_nothing_int_charstar,
  debug_nothing_int_charstar,
  debug_nothing_int,
  debug_nothing_int_int,	         /* begin_block */
  debug_nothing_int_int,	         /* end_block */
  debug_true_const_tree,	         /* ignore_block */
  dwarf2out_source_line,		 /* source_line */
  debug_nothing_int_int_charstar,	 /* begin_prologue */
  debug_nothing_int_charstar,	         /* end_prologue */
  debug_nothing_int_charstar,	         /* begin_epilogue */
  debug_nothing_int_charstar,	         /* end_epilogue */
  debug_nothing_tree,		         /* begin_function */
  debug_nothing_int,		         /* end_function */
  debug_nothing_tree,			 /* register_main_translation_unit */
  debug_nothing_tree,		         /* function_decl */
  debug_nothing_tree,		         /* early_global_decl */
  debug_nothing_tree,		         /* late_global_decl */
  debug_nothing_tree_int,		 /* type_decl */
  debug_nothing_tree_tree_tree_bool_bool,/* imported_module_or_decl */
  debug_false_tree_charstarstar_uhwistar,/* die_ref_for_decl */
  debug_nothing_tree_charstar_uhwi,      /* register_external_die */
  debug_nothing_tree,		         /* deferred_inline_function */
  debug_nothing_tree,		         /* outlining_inline_function */
  debug_nothing_rtx_code_label,	         /* label */
  debug_nothing_int,		         /* handle_pch */
  debug_nothing_rtx_insn,	         /* var_location */
  debug_nothing_tree,			 /* size_function */
  debug_nothing_void,                    /* switch_text_section */
  debug_nothing_tree_tree,		 /* set_name */
  0,                                     /* start_end_main_source_file */
  TYPE_SYMTAB_IS_ADDRESS                 /* tree_type_symtab_field */
};

/* NOTE: In the comments in this file, many references are made to
   "Debugging Information Entries".  This term is abbreviated as `DIE'
   throughout the remainder of this file.  */

/* An internal representation of the DWARF output is built, and then
   walked to generate the DWARF debugging info.  The walk of the internal
   representation is done after the entire program has been compiled.
   The types below are used to describe the internal representation.  */

/* Whether to put type DIEs into their own section .debug_types instead
   of making them part of the .debug_info section.  Only supported for
   Dwarf V4 or higher and the user didn't disable them through
   -fno-debug-types-section.  It is more efficient to put them in a
   separate comdat sections since the linker will then be able to
   remove duplicates.  But not all tools support .debug_types sections
   yet.  For Dwarf V5 or higher .debug_types doesn't exist any more,
   it is DW_UT_type unit type in .debug_info section.  */

#define use_debug_types (dwarf_version >= 4 && flag_debug_types_section)

/* Various DIE's use offsets relative to the beginning of the
   .debug_info section to refer to each other.  */

typedef long int dw_offset;

struct comdat_type_node;

/* The entries in the line_info table more-or-less mirror the opcodes
   that are used in the real dwarf line table.  Arrays of these entries
   are collected per section when DWARF2_ASM_LINE_DEBUG_INFO is not
   supported.  */

enum dw_line_info_opcode {
  /* Emit DW_LNE_set_address; the operand is the label index.  */
  LI_set_address,

  /* Emit a row to the matrix with the given line.  This may be done
     via any combination of DW_LNS_copy, DW_LNS_advance_line, and
     special opcodes.  */
  LI_set_line,

  /* Emit a DW_LNS_set_file.  */
  LI_set_file,

  /* Emit a DW_LNS_set_column.  */
  LI_set_column,

  /* Emit a DW_LNS_negate_stmt; the operand is ignored.  */
  LI_negate_stmt,

  /* Emit a DW_LNS_set_prologue_end/epilogue_begin; the operand is ignored.  */
  LI_set_prologue_end,
  LI_set_epilogue_begin,

  /* Emit a DW_LNE_set_discriminator.  */
  LI_set_discriminator
};

typedef struct GTY(()) dw_line_info_struct {
  enum dw_line_info_opcode opcode;
  unsigned int val;
} dw_line_info_entry;


struct GTY(()) dw_line_info_table {
  /* The label that marks the end of this section.  */
  const char *end_label;

  /* The values for the last row of the matrix, as collected in the table.
     These are used to minimize the changes to the next row.  */
  unsigned int file_num;
  unsigned int line_num;
  unsigned int column_num;
  int discrim_num;
  bool is_stmt;
  bool in_use;

  vec<dw_line_info_entry, va_gc> *entries;
};


/* Each DIE attribute has a field specifying the attribute kind,
   a link to the next attribute in the chain, and an attribute value.
   Attributes are typically linked below the DIE they modify.  */

typedef struct GTY(()) dw_attr_struct {
  enum dwarf_attribute dw_attr;
  dw_val_node dw_attr_val;
}
dw_attr_node;


/* The Debugging Information Entry (DIE) structure.  DIEs form a tree.
   The children of each node form a circular list linked by
   die_sib.  die_child points to the node *before* the "first" child node.  */

typedef struct GTY((chain_circular ("%h.die_sib"), for_user)) die_struct {
  union die_symbol_or_type_node
    {
      const char * GTY ((tag ("0"))) die_symbol;
      comdat_type_node *GTY ((tag ("1"))) die_type_node;
    }
  GTY ((desc ("%0.comdat_type_p"))) die_id;
  vec<dw_attr_node, va_gc> *die_attr;
  dw_die_ref die_parent;
  dw_die_ref die_child;
  dw_die_ref die_sib;
  dw_die_ref die_definition; /* ref from a specification to its definition */
  dw_offset die_offset;
  unsigned long die_abbrev;
  int die_mark;
  unsigned int decl_id;
  enum dwarf_tag die_tag;
  /* Die is used and must not be pruned as unused.  */
  BOOL_BITFIELD die_perennial_p : 1;
  BOOL_BITFIELD comdat_type_p : 1; /* DIE has a type signature */
  /* For an external ref to die_symbol if die_offset contains an extra
     offset to that symbol.  */
  BOOL_BITFIELD with_offset : 1;
  /* Whether this DIE was removed from the DIE tree, for example via
     prune_unused_types.  We don't consider those present from the
     DIE lookup routines.  */
  BOOL_BITFIELD removed : 1;
  /* Lots of spare bits.  */
}
die_node;

/* Set to TRUE while dwarf2out_early_global_decl is running.  */
static bool early_dwarf;
static bool early_dwarf_finished;
struct set_early_dwarf {
  bool saved;
  set_early_dwarf () : saved(early_dwarf)
    {
      gcc_assert (! early_dwarf_finished);
      early_dwarf = true;
    }
  ~set_early_dwarf () { early_dwarf = saved; }
};

/* Evaluate 'expr' while 'c' is set to each child of DIE in order.  */
#define FOR_EACH_CHILD(die, c, expr) do {	\
  c = die->die_child;				\
  if (c) do {					\
    c = c->die_sib;				\
    expr;					\
  } while (c != die->die_child);		\
} while (0)

/* The pubname structure */

typedef struct GTY(()) pubname_struct {
  dw_die_ref die;
  const char *name;
}
pubname_entry;


struct GTY(()) dw_ranges {
  const char *label;
  /* If this is positive, it's a block number, otherwise it's a
     bitwise-negated index into dw_ranges_by_label.  */
  int num;
  /* Index for the range list for DW_FORM_rnglistx.  */
  unsigned int idx : 31;
  /* True if this range might be possibly in a different section
     from previous entry.  */
  unsigned int maybe_new_sec : 1;
};

/* A structure to hold a macinfo entry.  */

typedef struct GTY(()) macinfo_struct {
  unsigned char code;
  unsigned HOST_WIDE_INT lineno;
  const char *info;
}
macinfo_entry;


struct GTY(()) dw_ranges_by_label {
  const char *begin;
  const char *end;
};

/* The comdat type node structure.  */
struct GTY(()) comdat_type_node
{
  dw_die_ref root_die;
  dw_die_ref type_die;
  dw_die_ref skeleton_die;
  char signature[DWARF_TYPE_SIGNATURE_SIZE];
  comdat_type_node *next;
};

/* A list of DIEs for which we can't determine ancestry (parent_die
   field) just yet.  Later in dwarf2out_finish we will fill in the
   missing bits.  */
typedef struct GTY(()) limbo_die_struct {
  dw_die_ref die;
  /* The tree for which this DIE was created.  We use this to
     determine ancestry later.  */
  tree created_for;
  struct limbo_die_struct *next;
}
limbo_die_node;

typedef struct skeleton_chain_struct
{
  dw_die_ref old_die;
  dw_die_ref new_die;
  struct skeleton_chain_struct *parent;
}
skeleton_chain_node;

/* Define a macro which returns nonzero for a TYPE_DECL which was
   implicitly generated for a type.

   Note that, unlike the C front-end (which generates a NULL named
   TYPE_DECL node for each complete tagged type, each array type,
   and each function type node created) the C++ front-end generates
   a _named_ TYPE_DECL node for each tagged type node created.
   These TYPE_DECLs have DECL_ARTIFICIAL set, so we know not to
   generate a DW_TAG_typedef DIE for them.  Likewise with the Ada
   front-end, but for each type, tagged or not.  */

#define TYPE_DECL_IS_STUB(decl)				\
  (DECL_NAME (decl) == NULL_TREE			\
   || (DECL_ARTIFICIAL (decl)				\
       && ((decl == TYPE_STUB_DECL (TREE_TYPE (decl)))	\
	   /* This is necessary for stub decls that	\
	      appear in nested inline functions.  */	\
	   || (DECL_ABSTRACT_ORIGIN (decl) != NULL_TREE	\
	       && (decl_ultimate_origin (decl)		\
		   == TYPE_STUB_DECL (TREE_TYPE (decl)))))))

/* Information concerning the compilation unit's programming
   language, and compiler version.  */

/* Fixed size portion of the DWARF compilation unit header.  */
#define DWARF_COMPILE_UNIT_HEADER_SIZE \
  (DWARF_INITIAL_LENGTH_SIZE + DWARF_OFFSET_SIZE			\
   + (dwarf_version >= 5 ? 4 : 3))

/* Fixed size portion of the DWARF comdat type unit header.  */
#define DWARF_COMDAT_TYPE_UNIT_HEADER_SIZE \
  (DWARF_COMPILE_UNIT_HEADER_SIZE					\
   + DWARF_TYPE_SIGNATURE_SIZE + DWARF_OFFSET_SIZE)

/* Fixed size portion of the DWARF skeleton compilation unit header.  */
#define DWARF_COMPILE_UNIT_SKELETON_HEADER_SIZE \
  (DWARF_COMPILE_UNIT_HEADER_SIZE + (dwarf_version >= 5 ? 8 : 0))

/* Fixed size portion of public names info.  */
#define DWARF_PUBNAMES_HEADER_SIZE (2 * DWARF_OFFSET_SIZE + 2)

/* Fixed size portion of the address range info.  */
#define DWARF_ARANGES_HEADER_SIZE					\
  (DWARF_ROUND (DWARF_INITIAL_LENGTH_SIZE + DWARF_OFFSET_SIZE + 4,	\
		DWARF2_ADDR_SIZE * 2)					\
   - DWARF_INITIAL_LENGTH_SIZE)

/* Size of padding portion in the address range info.  It must be
   aligned to twice the pointer size.  */
#define DWARF_ARANGES_PAD_SIZE \
  (DWARF_ROUND (DWARF_INITIAL_LENGTH_SIZE + DWARF_OFFSET_SIZE + 4, \
		DWARF2_ADDR_SIZE * 2)				   \
   - (DWARF_INITIAL_LENGTH_SIZE + DWARF_OFFSET_SIZE + 4))

/* Use assembler line directives if available.  */
#ifndef DWARF2_ASM_LINE_DEBUG_INFO
#ifdef HAVE_AS_DWARF2_DEBUG_LINE
#define DWARF2_ASM_LINE_DEBUG_INFO 1
#else
#define DWARF2_ASM_LINE_DEBUG_INFO 0
#endif
#endif

/* Minimum line offset in a special line info. opcode.
   This value was chosen to give a reasonable range of values.  */
#define DWARF_LINE_BASE  -10

/* First special line opcode - leave room for the standard opcodes.  */
#define DWARF_LINE_OPCODE_BASE  ((int)DW_LNS_set_isa + 1)

/* Range of line offsets in a special line info. opcode.  */
#define DWARF_LINE_RANGE  (254-DWARF_LINE_OPCODE_BASE+1)

/* Flag that indicates the initial value of the is_stmt_start flag.
   In the present implementation, we do not mark any lines as
   the beginning of a source statement, because that information
   is not made available by the GCC front-end.  */
#define	DWARF_LINE_DEFAULT_IS_STMT_START 1

/* Maximum number of operations per instruction bundle.  */
#ifndef DWARF_LINE_DEFAULT_MAX_OPS_PER_INSN
#define DWARF_LINE_DEFAULT_MAX_OPS_PER_INSN 1
#endif

/* This location is used by calc_die_sizes() to keep track
   the offset of each DIE within the .debug_info section.  */
static unsigned long next_die_offset;

/* Record the root of the DIE's built for the current compilation unit.  */
static GTY(()) dw_die_ref single_comp_unit_die;

/* A list of type DIEs that have been separated into comdat sections.  */
static GTY(()) comdat_type_node *comdat_type_list;

/* A list of CU DIEs that have been separated.  */
static GTY(()) limbo_die_node *cu_die_list;

/* A list of DIEs with a NULL parent waiting to be relocated.  */
static GTY(()) limbo_die_node *limbo_die_list;

/* A list of DIEs for which we may have to generate
   DW_AT_{,MIPS_}linkage_name once their DECL_ASSEMBLER_NAMEs are set.  */
static GTY(()) limbo_die_node *deferred_asm_name;

struct dwarf_file_hasher : ggc_ptr_hash<dwarf_file_data>
{
  typedef const char *compare_type;

  static hashval_t hash (dwarf_file_data *);
  static bool equal (dwarf_file_data *, const char *);
};

/* Filenames referenced by this compilation unit.  */
static GTY(()) hash_table<dwarf_file_hasher> *file_table;

struct decl_die_hasher : ggc_ptr_hash<die_node>
{
  typedef tree compare_type;

  static hashval_t hash (die_node *);
  static bool equal (die_node *, tree);
};
/* A hash table of references to DIE's that describe declarations.
   The key is a DECL_UID() which is a unique number identifying each decl.  */
static GTY (()) hash_table<decl_die_hasher> *decl_die_table;

struct GTY ((for_user)) variable_value_struct {
  unsigned int decl_id;
  vec<dw_die_ref, va_gc> *dies;
};

struct variable_value_hasher : ggc_ptr_hash<variable_value_struct>
{
  typedef tree compare_type;

  static hashval_t hash (variable_value_struct *);
  static bool equal (variable_value_struct *, tree);
};
/* A hash table of DIEs that contain DW_OP_GNU_variable_value with
   dw_val_class_decl_ref class, indexed by FUNCTION_DECLs which is
   DECL_CONTEXT of the referenced VAR_DECLs.  */
static GTY (()) hash_table<variable_value_hasher> *variable_value_hash;

struct block_die_hasher : ggc_ptr_hash<die_struct>
{
  static hashval_t hash (die_struct *);
  static bool equal (die_struct *, die_struct *);
};

/* A hash table of references to DIE's that describe COMMON blocks.
   The key is DECL_UID() ^ die_parent.  */
static GTY (()) hash_table<block_die_hasher> *common_block_die_table;

typedef struct GTY(()) die_arg_entry_struct {
    dw_die_ref die;
    tree arg;
} die_arg_entry;


/* Node of the variable location list.  */
struct GTY ((chain_next ("%h.next"))) var_loc_node {
  /* Either NOTE_INSN_VAR_LOCATION, or, for SRA optimized variables,
     EXPR_LIST chain.  For small bitsizes, bitsize is encoded
     in mode of the EXPR_LIST node and first EXPR_LIST operand
     is either NOTE_INSN_VAR_LOCATION for a piece with a known
     location or NULL for padding.  For larger bitsizes,
     mode is 0 and first operand is a CONCAT with bitsize
     as first CONCAT operand and NOTE_INSN_VAR_LOCATION resp.
     NULL as second operand.  */
  rtx GTY (()) loc;
  const char * GTY (()) label;
  struct var_loc_node * GTY (()) next;
};

/* Variable location list.  */
struct GTY ((for_user)) var_loc_list_def {
  struct var_loc_node * GTY (()) first;

  /* Pointer to the last but one or last element of the
     chained list.  If the list is empty, both first and
     last are NULL, if the list contains just one node
     or the last node certainly is not redundant, it points
     to the last node, otherwise points to the last but one.
     Do not mark it for GC because it is marked through the chain.  */
  struct var_loc_node * GTY ((skip ("%h"))) last;

  /* Pointer to the last element before section switch,
     if NULL, either sections weren't switched or first
     is after section switch.  */
  struct var_loc_node * GTY ((skip ("%h"))) last_before_switch;

  /* DECL_UID of the variable decl.  */
  unsigned int decl_id;
};
typedef struct var_loc_list_def var_loc_list;

/* Call argument location list.  */
struct GTY ((chain_next ("%h.next"))) call_arg_loc_node {
  rtx GTY (()) call_arg_loc_note;
  const char * GTY (()) label;
  tree GTY (()) block;
  bool tail_call_p;
  rtx GTY (()) symbol_ref;
  struct call_arg_loc_node * GTY (()) next;
};


struct decl_loc_hasher : ggc_ptr_hash<var_loc_list>
{
  typedef const_tree compare_type;

  static hashval_t hash (var_loc_list *);
  static bool equal (var_loc_list *, const_tree);
};

/* Table of decl location linked lists.  */
static GTY (()) hash_table<decl_loc_hasher> *decl_loc_table;

/* Head and tail of call_arg_loc chain.  */
static GTY (()) struct call_arg_loc_node *call_arg_locations;
static struct call_arg_loc_node *call_arg_loc_last;

/* Number of call sites in the current function.  */
static int call_site_count = -1;
/* Number of tail call sites in the current function.  */
static int tail_call_site_count = -1;

/* A cached location list.  */
struct GTY ((for_user)) cached_dw_loc_list_def {
  /* The DECL_UID of the decl that this entry describes.  */
  unsigned int decl_id;

  /* The cached location list.  */
  dw_loc_list_ref loc_list;
};
typedef struct cached_dw_loc_list_def cached_dw_loc_list;

struct dw_loc_list_hasher : ggc_ptr_hash<cached_dw_loc_list>
{

  typedef const_tree compare_type;
  
  static hashval_t hash (cached_dw_loc_list *);
  static bool equal (cached_dw_loc_list *, const_tree);
};

/* Table of cached location lists.  */
static GTY (()) hash_table<dw_loc_list_hasher> *cached_dw_loc_list_table;

/* A vector of references to DIE's that are uniquely identified by their tag,
   presence/absence of children DIE's, and list of attribute/value pairs.  */
static GTY(()) vec<dw_die_ref, va_gc> *abbrev_die_table;

/* A hash map to remember the stack usage for DWARF procedures.  The value
   stored is the stack size difference between before the DWARF procedure
   invokation and after it returned.  In other words, for a DWARF procedure
   that consumes N stack slots and that pushes M ones, this stores M - N.  */
static hash_map<dw_die_ref, int> *dwarf_proc_stack_usage_map;

/* A global counter for generating labels for line number data.  */
static unsigned int line_info_label_num;

/* The current table to which we should emit line number information
   for the current function.  This will be set up at the beginning of
   assembly for the function.  */
static GTY(()) dw_line_info_table *cur_line_info_table;

/* The two default tables of line number info.  */
static GTY(()) dw_line_info_table *text_section_line_info;
static GTY(()) dw_line_info_table *cold_text_section_line_info;

/* The set of all non-default tables of line number info.  */
static GTY(()) vec<dw_line_info_table *, va_gc> *separate_line_info;

/* A flag to tell pubnames/types export if there is an info section to
   refer to.  */
static bool info_section_emitted;

/* A pointer to the base of a table that contains a list of publicly
   accessible names.  */
static GTY (()) vec<pubname_entry, va_gc> *pubname_table;

/* A pointer to the base of a table that contains a list of publicly
   accessible types.  */
static GTY (()) vec<pubname_entry, va_gc> *pubtype_table;

/* A pointer to the base of a table that contains a list of macro
   defines/undefines (and file start/end markers).  */
static GTY (()) vec<macinfo_entry, va_gc> *macinfo_table;

/* True if .debug_macinfo or .debug_macros section is going to be
   emitted.  */
#define have_macinfo \
  ((!XCOFF_DEBUGGING_INFO || HAVE_XCOFF_DWARF_EXTRAS) \
   && debug_info_level >= DINFO_LEVEL_VERBOSE \
   && !macinfo_table->is_empty ())

/* Vector of dies for which we should generate .debug_ranges info.  */
static GTY (()) vec<dw_ranges, va_gc> *ranges_table;

/* Vector of pairs of labels referenced in ranges_table.  */
static GTY (()) vec<dw_ranges_by_label, va_gc> *ranges_by_label;

/* Whether we have location lists that need outputting */
static GTY(()) bool have_location_lists;

/* Unique label counter.  */
static GTY(()) unsigned int loclabel_num;

/* Unique label counter for point-of-call tables.  */
static GTY(()) unsigned int poc_label_num;

/* The last file entry emitted by maybe_emit_file().  */
static GTY(()) struct dwarf_file_data * last_emitted_file;

/* Number of internal labels generated by gen_internal_sym().  */
static GTY(()) int label_num;

static GTY(()) vec<die_arg_entry, va_gc> *tmpl_value_parm_die_table;

/* Instances of generic types for which we need to generate debug
   info that describe their generic parameters and arguments. That
   generation needs to happen once all types are properly laid out so
   we do it at the end of compilation.  */
static GTY(()) vec<tree, va_gc> *generic_type_instances;

/* Offset from the "steady-state frame pointer" to the frame base,
   within the current function.  */
static HOST_WIDE_INT frame_pointer_fb_offset;
static bool frame_pointer_fb_offset_valid;

static vec<dw_die_ref> base_types;

/* Flags to represent a set of attribute classes for attributes that represent
   a scalar value (bounds, pointers, ...).  */
enum dw_scalar_form
{
  dw_scalar_form_constant = 0x01,
  dw_scalar_form_exprloc = 0x02,
  dw_scalar_form_reference = 0x04
};

/* Forward declarations for functions defined in this file.  */

static int is_pseudo_reg (const_rtx);
static tree type_main_variant (tree);
static int is_tagged_type (const_tree);
static const char *dwarf_tag_name (unsigned);
static const char *dwarf_attr_name (unsigned);
static const char *dwarf_form_name (unsigned);
static tree decl_ultimate_origin (const_tree);
static tree decl_class_context (tree);
static void add_dwarf_attr (dw_die_ref, dw_attr_node *);
static inline enum dw_val_class AT_class (dw_attr_node *);
static inline unsigned int AT_index (dw_attr_node *);
static void add_AT_flag (dw_die_ref, enum dwarf_attribute, unsigned);
static inline unsigned AT_flag (dw_attr_node *);
static void add_AT_int (dw_die_ref, enum dwarf_attribute, HOST_WIDE_INT);
static inline HOST_WIDE_INT AT_int (dw_attr_node *);
static void add_AT_unsigned (dw_die_ref, enum dwarf_attribute, unsigned HOST_WIDE_INT);
static inline unsigned HOST_WIDE_INT AT_unsigned (dw_attr_node *);
static void add_AT_double (dw_die_ref, enum dwarf_attribute,
			   HOST_WIDE_INT, unsigned HOST_WIDE_INT);
static inline void add_AT_vec (dw_die_ref, enum dwarf_attribute, unsigned int,
			       unsigned int, unsigned char *);
static void add_AT_data8 (dw_die_ref, enum dwarf_attribute, unsigned char *);
static void add_AT_string (dw_die_ref, enum dwarf_attribute, const char *);
static inline const char *AT_string (dw_attr_node *);
static enum dwarf_form AT_string_form (dw_attr_node *);
static void add_AT_die_ref (dw_die_ref, enum dwarf_attribute, dw_die_ref);
static void add_AT_specification (dw_die_ref, dw_die_ref);
static inline dw_die_ref AT_ref (dw_attr_node *);
static inline int AT_ref_external (dw_attr_node *);
static inline void set_AT_ref_external (dw_attr_node *, int);
static void add_AT_fde_ref (dw_die_ref, enum dwarf_attribute, unsigned);
static void add_AT_loc (dw_die_ref, enum dwarf_attribute, dw_loc_descr_ref);
static inline dw_loc_descr_ref AT_loc (dw_attr_node *);
static void add_AT_loc_list (dw_die_ref, enum dwarf_attribute,
			     dw_loc_list_ref);
static inline dw_loc_list_ref AT_loc_list (dw_attr_node *);
static addr_table_entry *add_addr_table_entry (void *, enum ate_kind);
static void remove_addr_table_entry (addr_table_entry *);
static void add_AT_addr (dw_die_ref, enum dwarf_attribute, rtx, bool);
static inline rtx AT_addr (dw_attr_node *);
static void add_AT_lbl_id (dw_die_ref, enum dwarf_attribute, const char *);
static void add_AT_lineptr (dw_die_ref, enum dwarf_attribute, const char *);
static void add_AT_macptr (dw_die_ref, enum dwarf_attribute, const char *);
static void add_AT_loclistsptr (dw_die_ref, enum dwarf_attribute,
				const char *);
static void add_AT_offset (dw_die_ref, enum dwarf_attribute,
			   unsigned HOST_WIDE_INT);
static void add_AT_range_list (dw_die_ref, enum dwarf_attribute,
                               unsigned long, bool);
static inline const char *AT_lbl (dw_attr_node *);
static dw_attr_node *get_AT (dw_die_ref, enum dwarf_attribute);
static const char *get_AT_low_pc (dw_die_ref);
static const char *get_AT_hi_pc (dw_die_ref);
static const char *get_AT_string (dw_die_ref, enum dwarf_attribute);
static int get_AT_flag (dw_die_ref, enum dwarf_attribute);
static unsigned get_AT_unsigned (dw_die_ref, enum dwarf_attribute);
static inline dw_die_ref get_AT_ref (dw_die_ref, enum dwarf_attribute);
static bool is_cxx (void);
static bool is_cxx (const_tree);
static bool is_fortran (void);
static bool is_ada (void);
static bool remove_AT (dw_die_ref, enum dwarf_attribute);
static void remove_child_TAG (dw_die_ref, enum dwarf_tag);
static void add_child_die (dw_die_ref, dw_die_ref);
static dw_die_ref new_die (enum dwarf_tag, dw_die_ref, tree);
static dw_die_ref lookup_type_die (tree);
static dw_die_ref strip_naming_typedef (tree, dw_die_ref);
static dw_die_ref lookup_type_die_strip_naming_typedef (tree);
static void equate_type_number_to_die (tree, dw_die_ref);
static dw_die_ref lookup_decl_die (tree);
static var_loc_list *lookup_decl_loc (const_tree);
static void equate_decl_number_to_die (tree, dw_die_ref);
static struct var_loc_node *add_var_loc_to_decl (tree, rtx, const char *);
static void print_spaces (FILE *);
static void print_die (dw_die_ref, FILE *);
static void loc_checksum (dw_loc_descr_ref, struct md5_ctx *);
static void attr_checksum (dw_attr_node *, struct md5_ctx *, int *);
static void die_checksum (dw_die_ref, struct md5_ctx *, int *);
static void checksum_sleb128 (HOST_WIDE_INT, struct md5_ctx *);
static void checksum_uleb128 (unsigned HOST_WIDE_INT, struct md5_ctx *);
static void loc_checksum_ordered (dw_loc_descr_ref, struct md5_ctx *);
static void attr_checksum_ordered (enum dwarf_tag, dw_attr_node *,
				   struct md5_ctx *, int *);
struct checksum_attributes;
static void collect_checksum_attributes (struct checksum_attributes *, dw_die_ref);
static void die_checksum_ordered (dw_die_ref, struct md5_ctx *, int *);
static void checksum_die_context (dw_die_ref, struct md5_ctx *);
static void generate_type_signature (dw_die_ref, comdat_type_node *);
static int same_loc_p (dw_loc_descr_ref, dw_loc_descr_ref, int *);
static int same_dw_val_p (const dw_val_node *, const dw_val_node *, int *);
static int same_attr_p (dw_attr_node *, dw_attr_node *, int *);
static int same_die_p (dw_die_ref, dw_die_ref, int *);
static int is_type_die (dw_die_ref);
static int is_comdat_die (dw_die_ref);
static inline bool is_template_instantiation (dw_die_ref);
static int is_declaration_die (dw_die_ref);
static int should_move_die_to_comdat (dw_die_ref);
static dw_die_ref clone_as_declaration (dw_die_ref);
static dw_die_ref clone_die (dw_die_ref);
static dw_die_ref clone_tree (dw_die_ref);
static dw_die_ref copy_declaration_context (dw_die_ref, dw_die_ref);
static void generate_skeleton_ancestor_tree (skeleton_chain_node *);
static void generate_skeleton_bottom_up (skeleton_chain_node *);
static dw_die_ref generate_skeleton (dw_die_ref);
static dw_die_ref remove_child_or_replace_with_skeleton (dw_die_ref,
                                                         dw_die_ref,
                                                         dw_die_ref);
static void break_out_comdat_types (dw_die_ref);
static void copy_decls_for_unworthy_types (dw_die_ref);

static void add_sibling_attributes (dw_die_ref);
static void output_location_lists (dw_die_ref);
static int constant_size (unsigned HOST_WIDE_INT);
static unsigned long size_of_die (dw_die_ref);
static void calc_die_sizes (dw_die_ref);
static void calc_base_type_die_sizes (void);
static void mark_dies (dw_die_ref);
static void unmark_dies (dw_die_ref);
static void unmark_all_dies (dw_die_ref);
static unsigned long size_of_pubnames (vec<pubname_entry, va_gc> *);
static unsigned long size_of_aranges (void);
static enum dwarf_form value_format (dw_attr_node *);
static void output_value_format (dw_attr_node *);
static void output_abbrev_section (void);
static void output_die_abbrevs (unsigned long, dw_die_ref);
static void output_die_symbol (dw_die_ref);
static void output_die (dw_die_ref);
static void output_compilation_unit_header (enum dwarf_unit_type);
static void output_comp_unit (dw_die_ref, int, const unsigned char *);
static void output_comdat_type_unit (comdat_type_node *);
static const char *dwarf2_name (tree, int);
static void add_pubname (tree, dw_die_ref);
static void add_enumerator_pubname (const char *, dw_die_ref);
static void add_pubname_string (const char *, dw_die_ref);
static void add_pubtype (tree, dw_die_ref);
static void output_pubnames (vec<pubname_entry, va_gc> *);
static void output_aranges (void);
static unsigned int add_ranges (const_tree, bool = false);
static void add_ranges_by_labels (dw_die_ref, const char *, const char *,
                                  bool *, bool);
static void output_ranges (void);
static dw_line_info_table *new_line_info_table (void);
static void output_line_info (bool);
static void output_file_names (void);
static dw_die_ref base_type_die (tree, bool);
static int is_base_type (tree);
static dw_die_ref subrange_type_die (tree, tree, tree, tree, dw_die_ref);
static int decl_quals (const_tree);
static dw_die_ref modified_type_die (tree, int, bool, dw_die_ref);
static dw_die_ref generic_parameter_die (tree, tree, bool, dw_die_ref);
static dw_die_ref template_parameter_pack_die (tree, tree, dw_die_ref);
static int type_is_enum (const_tree);
static unsigned int dbx_reg_number (const_rtx);
static void add_loc_descr_op_piece (dw_loc_descr_ref *, int);
static dw_loc_descr_ref reg_loc_descriptor (rtx, enum var_init_status);
static dw_loc_descr_ref one_reg_loc_descriptor (unsigned int,
						enum var_init_status);
static dw_loc_descr_ref multiple_reg_loc_descriptor (rtx, rtx,
						     enum var_init_status);
static dw_loc_descr_ref based_loc_descr (rtx, HOST_WIDE_INT,
					 enum var_init_status);
static int is_based_loc (const_rtx);
static bool resolve_one_addr (rtx *);
static dw_loc_descr_ref concat_loc_descriptor (rtx, rtx,
					       enum var_init_status);
static dw_loc_descr_ref loc_descriptor (rtx, machine_mode mode,
					enum var_init_status);
struct loc_descr_context;
static void add_loc_descr_to_each (dw_loc_list_ref list, dw_loc_descr_ref ref);
static void add_loc_list (dw_loc_list_ref *ret, dw_loc_list_ref list);
static dw_loc_list_ref loc_list_from_tree (tree, int,
					   struct loc_descr_context *);
static dw_loc_descr_ref loc_descriptor_from_tree (tree, int,
						  struct loc_descr_context *);
static HOST_WIDE_INT ceiling (HOST_WIDE_INT, unsigned int);
static tree field_type (const_tree);
static unsigned int simple_type_align_in_bits (const_tree);
static unsigned int simple_decl_align_in_bits (const_tree);
static unsigned HOST_WIDE_INT simple_type_size_in_bits (const_tree);
struct vlr_context;
static dw_loc_descr_ref field_byte_offset (const_tree, struct vlr_context *,
					   HOST_WIDE_INT *);
static void add_AT_location_description	(dw_die_ref, enum dwarf_attribute,
					 dw_loc_list_ref);
static void add_data_member_location_attribute (dw_die_ref, tree,
						struct vlr_context *);
static bool add_const_value_attribute (dw_die_ref, rtx);
static void insert_int (HOST_WIDE_INT, unsigned, unsigned char *);
static void insert_wide_int (const wide_int &, unsigned char *, int);
static void insert_float (const_rtx, unsigned char *);
static rtx rtl_for_decl_location (tree);
static bool add_location_or_const_value_attribute (dw_die_ref, tree, bool);
static bool tree_add_const_value_attribute (dw_die_ref, tree);
static bool tree_add_const_value_attribute_for_decl (dw_die_ref, tree);
static void add_name_attribute (dw_die_ref, const char *);
static void add_gnat_descriptive_type_attribute (dw_die_ref, tree, dw_die_ref);
static void add_comp_dir_attribute (dw_die_ref);
static void add_scalar_info (dw_die_ref, enum dwarf_attribute, tree, int,
			     struct loc_descr_context *);
static void add_bound_info (dw_die_ref, enum dwarf_attribute, tree,
			    struct loc_descr_context *);
static void add_subscript_info (dw_die_ref, tree, bool);
static void add_byte_size_attribute (dw_die_ref, tree);
static void add_alignment_attribute (dw_die_ref, tree);
static inline void add_bit_offset_attribute (dw_die_ref, tree,
					     struct vlr_context *);
static void add_bit_size_attribute (dw_die_ref, tree);
static void add_prototyped_attribute (dw_die_ref, tree);
static dw_die_ref add_abstract_origin_attribute (dw_die_ref, tree);
static void add_pure_or_virtual_attribute (dw_die_ref, tree);
static void add_src_coords_attributes (dw_die_ref, tree);
static void add_name_and_src_coords_attributes (dw_die_ref, tree, bool = false);
static void add_discr_value (dw_die_ref, dw_discr_value *);
static void add_discr_list (dw_die_ref, dw_discr_list_ref);
static inline dw_discr_list_ref AT_discr_list (dw_attr_node *);
static void push_decl_scope (tree);
static void pop_decl_scope (void);
static dw_die_ref scope_die_for (tree, dw_die_ref);
static inline int local_scope_p (dw_die_ref);
static inline int class_scope_p (dw_die_ref);
static inline int class_or_namespace_scope_p (dw_die_ref);
static void add_type_attribute (dw_die_ref, tree, int, bool, dw_die_ref);
static void add_calling_convention_attribute (dw_die_ref, tree);
static const char *type_tag (const_tree);
static tree member_declared_type (const_tree);
#if 0
static const char *decl_start_label (tree);
#endif
static void gen_array_type_die (tree, dw_die_ref);
static void gen_descr_array_type_die (tree, struct array_descr_info *, dw_die_ref);
#if 0
static void gen_entry_point_die (tree, dw_die_ref);
#endif
static dw_die_ref gen_enumeration_type_die (tree, dw_die_ref);
static dw_die_ref gen_formal_parameter_die (tree, tree, bool, dw_die_ref);
static dw_die_ref gen_formal_parameter_pack_die  (tree, tree, dw_die_ref, tree*);
static void gen_unspecified_parameters_die (tree, dw_die_ref);
static void gen_formal_types_die (tree, dw_die_ref);
static void gen_subprogram_die (tree, dw_die_ref);
static void gen_variable_die (tree, tree, dw_die_ref);
static void gen_const_die (tree, dw_die_ref);
static void gen_label_die (tree, dw_die_ref);
static void gen_lexical_block_die (tree, dw_die_ref);
static void gen_inlined_subroutine_die (tree, dw_die_ref);
static void gen_field_die (tree, struct vlr_context *, dw_die_ref);
static void gen_ptr_to_mbr_type_die (tree, dw_die_ref);
static dw_die_ref gen_compile_unit_die (const char *);
static void gen_inheritance_die (tree, tree, tree, dw_die_ref);
static void gen_member_die (tree, dw_die_ref);
static void gen_struct_or_union_type_die (tree, dw_die_ref,
						enum debug_info_usage);
static void gen_subroutine_type_die (tree, dw_die_ref);
static void gen_typedef_die (tree, dw_die_ref);
static void gen_type_die (tree, dw_die_ref);
static void gen_block_die (tree, dw_die_ref);
static void decls_for_scope (tree, dw_die_ref);
static bool is_naming_typedef_decl (const_tree);
static inline dw_die_ref get_context_die (tree);
static void gen_namespace_die (tree, dw_die_ref);
static dw_die_ref gen_namelist_decl (tree, dw_die_ref, tree);
static dw_die_ref gen_decl_die (tree, tree, struct vlr_context *, dw_die_ref);
static dw_die_ref force_decl_die (tree);
static dw_die_ref force_type_die (tree);
static dw_die_ref setup_namespace_context (tree, dw_die_ref);
static dw_die_ref declare_in_namespace (tree, dw_die_ref);
static struct dwarf_file_data * lookup_filename (const char *);
static void retry_incomplete_types (void);
static void gen_type_die_for_member (tree, tree, dw_die_ref);
static void gen_generic_params_dies (tree);
static void gen_tagged_type_die (tree, dw_die_ref, enum debug_info_usage);
static void gen_type_die_with_usage (tree, dw_die_ref, enum debug_info_usage);
static void splice_child_die (dw_die_ref, dw_die_ref);
static int file_info_cmp (const void *, const void *);
static dw_loc_list_ref new_loc_list (dw_loc_descr_ref, const char *,
				     const char *, const char *);
static void output_loc_list (dw_loc_list_ref);
static char *gen_internal_sym (const char *);
static bool want_pubnames (void);

static void prune_unmark_dies (dw_die_ref);
static void prune_unused_types_mark_generic_parms_dies (dw_die_ref);
static void prune_unused_types_mark (dw_die_ref, int);
static void prune_unused_types_walk (dw_die_ref);
static void prune_unused_types_walk_attribs (dw_die_ref);
static void prune_unused_types_prune (dw_die_ref);
static void prune_unused_types (void);
static int maybe_emit_file (struct dwarf_file_data *fd);
static inline const char *AT_vms_delta1 (dw_attr_node *);
static inline const char *AT_vms_delta2 (dw_attr_node *);
static inline void add_AT_vms_delta (dw_die_ref, enum dwarf_attribute,
				     const char *, const char *);
static void append_entry_to_tmpl_value_parm_die_table (dw_die_ref, tree);
static void gen_remaining_tmpl_value_param_die_attribute (void);
static bool generic_type_p (tree);
static void schedule_generic_params_dies_gen (tree t);
static void gen_scheduled_generic_parms_dies (void);
static void resolve_variable_values (void);

static const char *comp_dir_string (void);

static void hash_loc_operands (dw_loc_descr_ref, inchash::hash &);

/* enum for tracking thread-local variables whose address is really an offset
   relative to the TLS pointer, which will need link-time relocation, but will
   not need relocation by the DWARF consumer.  */

enum dtprel_bool
{
  dtprel_false = 0,
  dtprel_true = 1
};

/* Return the operator to use for an address of a variable.  For dtprel_true, we
   use DW_OP_const*.  For regular variables, which need both link-time
   relocation and consumer-level relocation (e.g., to account for shared objects
   loaded at a random address), we use DW_OP_addr*.  */

static inline enum dwarf_location_atom
dw_addr_op (enum dtprel_bool dtprel)
{
  if (dtprel == dtprel_true)
    return (dwarf_split_debug_info ? DW_OP_GNU_const_index
            : (DWARF2_ADDR_SIZE == 4 ? DW_OP_const4u : DW_OP_const8u));
  else
    return dwarf_split_debug_info ? DW_OP_GNU_addr_index : DW_OP_addr;
}

/* Return a pointer to a newly allocated address location description.  If
   dwarf_split_debug_info is true, then record the address with the appropriate
   relocation.  */
static inline dw_loc_descr_ref
new_addr_loc_descr (rtx addr, enum dtprel_bool dtprel)
{
  dw_loc_descr_ref ref = new_loc_descr (dw_addr_op (dtprel), 0, 0);

  ref->dw_loc_oprnd1.val_class = dw_val_class_addr;
  ref->dw_loc_oprnd1.v.val_addr = addr;
  ref->dtprel = dtprel;
  if (dwarf_split_debug_info)
    ref->dw_loc_oprnd1.val_entry
      = add_addr_table_entry (addr,
			      dtprel ? ate_kind_rtx_dtprel : ate_kind_rtx);
  else
    ref->dw_loc_oprnd1.val_entry = NULL;

  return ref;
}

/* Section names used to hold DWARF debugging information.  */

#ifndef DEBUG_INFO_SECTION
#define DEBUG_INFO_SECTION	".debug_info"
#endif
#ifndef DEBUG_DWO_INFO_SECTION
#define DEBUG_DWO_INFO_SECTION ".debug_info.dwo"
#endif
#ifndef DEBUG_LTO_DWO_INFO_SECTION
#define DEBUG_LTO_DWO_INFO_SECTION ".gnu.debuglto_.debug_info.dwo"
#endif
#ifndef DEBUG_LTO_INFO_SECTION
#define DEBUG_LTO_INFO_SECTION	".gnu.debuglto_.debug_info"
#endif
#ifndef DEBUG_ABBREV_SECTION
#define DEBUG_ABBREV_SECTION	".debug_abbrev"
#endif
#ifndef DEBUG_DWO_ABBREV_SECTION
#define DEBUG_DWO_ABBREV_SECTION ".debug_abbrev.dwo"
#endif
#ifndef DEBUG_LTO_DWO_ABBREV_SECTION
#define DEBUG_LTO_DWO_ABBREV_SECTION ".gnu.debuglto_.debug_abbrev.dwo"
#endif
#ifndef DEBUG_LTO_ABBREV_SECTION
#define DEBUG_LTO_ABBREV_SECTION ".gnu.debuglto_.debug_abbrev"
#endif
#ifndef DEBUG_ARANGES_SECTION
#define DEBUG_ARANGES_SECTION	".debug_aranges"
#endif
#ifndef DEBUG_ADDR_SECTION
#define DEBUG_ADDR_SECTION     ".debug_addr"
#endif
#ifndef DEBUG_MACINFO_SECTION
#define DEBUG_MACINFO_SECTION     ".debug_macinfo"
#endif
#ifndef DEBUG_DWO_MACINFO_SECTION
#define DEBUG_DWO_MACINFO_SECTION      ".debug_macinfo.dwo"
#endif
#ifndef DEBUG_LTO_DWO_MACINFO_SECTION
#define DEBUG_LTO_DWO_MACINFO_SECTION  ".gnu.debuglto_.debug_macinfo.dwo"
#endif
#ifndef DEBUG_LTO_MACINFO_SECTION
#define DEBUG_LTO_MACINFO_SECTION      ".gnu.debuglto_.debug_macinfo"
#endif
#ifndef DEBUG_DWO_MACRO_SECTION
#define DEBUG_DWO_MACRO_SECTION        ".debug_macro.dwo"
#endif
#ifndef DEBUG_MACRO_SECTION
#define DEBUG_MACRO_SECTION	".debug_macro"
#endif
#ifndef DEBUG_LTO_DWO_MACRO_SECTION
#define DEBUG_LTO_DWO_MACRO_SECTION    ".gnu.debuglto_.debug_macro.dwo"
#endif
#ifndef DEBUG_LTO_MACRO_SECTION
#define DEBUG_LTO_MACRO_SECTION ".gnu.debuglto_.debug_macro"
#endif
#ifndef DEBUG_LINE_SECTION
#define DEBUG_LINE_SECTION	".debug_line"
#endif
#ifndef DEBUG_DWO_LINE_SECTION
#define DEBUG_DWO_LINE_SECTION ".debug_line.dwo"
#endif
#ifndef DEBUG_LTO_LINE_SECTION
#define DEBUG_LTO_LINE_SECTION ".gnu.debuglto_.debug_line.dwo"
#endif
#ifndef DEBUG_LOC_SECTION
#define DEBUG_LOC_SECTION	".debug_loc"
#endif
#ifndef DEBUG_DWO_LOC_SECTION
#define DEBUG_DWO_LOC_SECTION  ".debug_loc.dwo"
#endif
#ifndef DEBUG_LOCLISTS_SECTION
#define DEBUG_LOCLISTS_SECTION	".debug_loclists"
#endif
#ifndef DEBUG_DWO_LOCLISTS_SECTION
#define DEBUG_DWO_LOCLISTS_SECTION  ".debug_loclists.dwo"
#endif
#ifndef DEBUG_PUBNAMES_SECTION
#define DEBUG_PUBNAMES_SECTION	\
  ((debug_generate_pub_sections == 2) \
   ? ".debug_gnu_pubnames" : ".debug_pubnames")
#endif
#ifndef DEBUG_PUBTYPES_SECTION
#define DEBUG_PUBTYPES_SECTION	\
  ((debug_generate_pub_sections == 2) \
   ? ".debug_gnu_pubtypes" : ".debug_pubtypes")
#endif
#ifndef DEBUG_STR_OFFSETS_SECTION
#define DEBUG_STR_OFFSETS_SECTION ".debug_str_offsets"
#endif
#ifndef DEBUG_DWO_STR_OFFSETS_SECTION
#define DEBUG_DWO_STR_OFFSETS_SECTION ".debug_str_offsets.dwo"
#endif
#ifndef DEBUG_LTO_DWO_STR_OFFSETS_SECTION
#define DEBUG_LTO_DWO_STR_OFFSETS_SECTION ".gnu.debuglto_.debug_str_offsets.dwo"
#endif
#ifndef DEBUG_STR_DWO_SECTION
#define DEBUG_STR_DWO_SECTION   ".debug_str.dwo"
#endif
#ifndef DEBUG_LTO_STR_DWO_SECTION
#define DEBUG_LTO_STR_DWO_SECTION ".gnu.debuglto_.debug_str.dwo"
#endif
#ifndef DEBUG_STR_SECTION
#define DEBUG_STR_SECTION  ".debug_str"
#endif
#ifndef DEBUG_LTO_STR_SECTION
#define DEBUG_LTO_STR_SECTION ".gnu.debuglto_.debug_str"
#endif
#ifndef DEBUG_RANGES_SECTION
#define DEBUG_RANGES_SECTION	".debug_ranges"
#endif
#ifndef DEBUG_RNGLISTS_SECTION
#define DEBUG_RNGLISTS_SECTION	".debug_rnglists"
#endif
#ifndef DEBUG_LINE_STR_SECTION
#define DEBUG_LINE_STR_SECTION  ".debug_line_str"
#endif

/* Standard ELF section names for compiled code and data.  */
#ifndef TEXT_SECTION_NAME
#define TEXT_SECTION_NAME	".text"
#endif

/* Section flags for .debug_str section.  */
#define DEBUG_STR_SECTION_FLAGS                                 \
  (HAVE_GAS_SHF_MERGE && flag_merge_debug_strings               \
   ? SECTION_DEBUG | SECTION_MERGE | SECTION_STRINGS | 1        \
   : SECTION_DEBUG)

/* Section flags for .debug_str.dwo section.  */
#define DEBUG_STR_DWO_SECTION_FLAGS (SECTION_DEBUG | SECTION_EXCLUDE)

/* Attribute used to refer to the macro section.  */
#define DEBUG_MACRO_ATTRIBUTE (dwarf_version >= 5 ? DW_AT_macros \
		   : dwarf_strict ? DW_AT_macro_info : DW_AT_GNU_macros)

/* Labels we insert at beginning sections we can reference instead of
   the section names themselves.  */

#ifndef TEXT_SECTION_LABEL
#define TEXT_SECTION_LABEL                 "Ltext"
#endif
#ifndef COLD_TEXT_SECTION_LABEL
#define COLD_TEXT_SECTION_LABEL             "Ltext_cold"
#endif
#ifndef DEBUG_LINE_SECTION_LABEL
#define DEBUG_LINE_SECTION_LABEL           "Ldebug_line"
#endif
#ifndef DEBUG_SKELETON_LINE_SECTION_LABEL
#define DEBUG_SKELETON_LINE_SECTION_LABEL   "Lskeleton_debug_line"
#endif
#ifndef DEBUG_INFO_SECTION_LABEL
#define DEBUG_INFO_SECTION_LABEL           "Ldebug_info"
#endif
#ifndef DEBUG_SKELETON_INFO_SECTION_LABEL
#define DEBUG_SKELETON_INFO_SECTION_LABEL   "Lskeleton_debug_info"
#endif
#ifndef DEBUG_ABBREV_SECTION_LABEL
#define DEBUG_ABBREV_SECTION_LABEL         "Ldebug_abbrev"
#endif
#ifndef DEBUG_SKELETON_ABBREV_SECTION_LABEL
#define DEBUG_SKELETON_ABBREV_SECTION_LABEL "Lskeleton_debug_abbrev"
#endif
#ifndef DEBUG_ADDR_SECTION_LABEL
#define DEBUG_ADDR_SECTION_LABEL           "Ldebug_addr"
#endif
#ifndef DEBUG_LOC_SECTION_LABEL
#define DEBUG_LOC_SECTION_LABEL                    "Ldebug_loc"
#endif
#ifndef DEBUG_RANGES_SECTION_LABEL
#define DEBUG_RANGES_SECTION_LABEL         "Ldebug_ranges"
#endif
#ifndef DEBUG_MACINFO_SECTION_LABEL
#define DEBUG_MACINFO_SECTION_LABEL         "Ldebug_macinfo"
#endif
#ifndef DEBUG_MACRO_SECTION_LABEL
#define DEBUG_MACRO_SECTION_LABEL          "Ldebug_macro"
#endif
#define SKELETON_COMP_DIE_ABBREV 1
#define SKELETON_TYPE_DIE_ABBREV 2

/* Definitions of defaults for formats and names of various special
   (artificial) labels which may be generated within this file (when the -g
   options is used and DWARF2_DEBUGGING_INFO is in effect.
   If necessary, these may be overridden from within the tm.h file, but
   typically, overriding these defaults is unnecessary.  */

static char text_end_label[MAX_ARTIFICIAL_LABEL_BYTES];
static char text_section_label[MAX_ARTIFICIAL_LABEL_BYTES];
static char cold_text_section_label[MAX_ARTIFICIAL_LABEL_BYTES];
static char cold_end_label[MAX_ARTIFICIAL_LABEL_BYTES];
static char abbrev_section_label[MAX_ARTIFICIAL_LABEL_BYTES];
static char debug_info_section_label[MAX_ARTIFICIAL_LABEL_BYTES];
static char debug_skeleton_info_section_label[MAX_ARTIFICIAL_LABEL_BYTES];
static char debug_skeleton_abbrev_section_label[MAX_ARTIFICIAL_LABEL_BYTES];
static char debug_line_section_label[MAX_ARTIFICIAL_LABEL_BYTES];
static char debug_addr_section_label[MAX_ARTIFICIAL_LABEL_BYTES];
static char debug_skeleton_line_section_label[MAX_ARTIFICIAL_LABEL_BYTES];
static char macinfo_section_label[MAX_ARTIFICIAL_LABEL_BYTES];
static char loc_section_label[MAX_ARTIFICIAL_LABEL_BYTES];
static char ranges_section_label[2 * MAX_ARTIFICIAL_LABEL_BYTES];
static char ranges_base_label[2 * MAX_ARTIFICIAL_LABEL_BYTES];

#ifndef TEXT_END_LABEL
#define TEXT_END_LABEL		"Letext"
#endif
#ifndef COLD_END_LABEL
#define COLD_END_LABEL          "Letext_cold"
#endif
#ifndef BLOCK_BEGIN_LABEL
#define BLOCK_BEGIN_LABEL	"LBB"
#endif
#ifndef BLOCK_END_LABEL
#define BLOCK_END_LABEL		"LBE"
#endif
#ifndef LINE_CODE_LABEL
#define LINE_CODE_LABEL		"LM"
#endif


/* Return the root of the DIE's built for the current compilation unit.  */
static dw_die_ref
comp_unit_die (void)
{
  if (!single_comp_unit_die)
    single_comp_unit_die = gen_compile_unit_die (NULL);
  return single_comp_unit_die;
}

/* We allow a language front-end to designate a function that is to be
   called to "demangle" any name before it is put into a DIE.  */

static const char *(*demangle_name_func) (const char *);

void
dwarf2out_set_demangle_name_func (const char *(*func) (const char *))
{
  demangle_name_func = func;
}

/* Test if rtl node points to a pseudo register.  */

static inline int
is_pseudo_reg (const_rtx rtl)
{
  return ((REG_P (rtl) && REGNO (rtl) >= FIRST_PSEUDO_REGISTER)
	  || (GET_CODE (rtl) == SUBREG
	      && REGNO (SUBREG_REG (rtl)) >= FIRST_PSEUDO_REGISTER));
}

/* Return a reference to a type, with its const and volatile qualifiers
   removed.  */

static inline tree
type_main_variant (tree type)
{
  type = TYPE_MAIN_VARIANT (type);

  /* ??? There really should be only one main variant among any group of
     variants of a given type (and all of the MAIN_VARIANT values for all
     members of the group should point to that one type) but sometimes the C
     front-end messes this up for array types, so we work around that bug
     here.  */
  if (TREE_CODE (type) == ARRAY_TYPE)
    while (type != TYPE_MAIN_VARIANT (type))
      type = TYPE_MAIN_VARIANT (type);

  return type;
}

/* Return nonzero if the given type node represents a tagged type.  */

static inline int
is_tagged_type (const_tree type)
{
  enum tree_code code = TREE_CODE (type);

  return (code == RECORD_TYPE || code == UNION_TYPE
	  || code == QUAL_UNION_TYPE || code == ENUMERAL_TYPE);
}

/* Set label to debug_info_section_label + die_offset of a DIE reference.  */

static void
get_ref_die_offset_label (char *label, dw_die_ref ref)
{
  sprintf (label, "%s+%ld", debug_info_section_label, ref->die_offset);
}

/* Return die_offset of a DIE reference to a base type.  */

static unsigned long int
get_base_type_offset (dw_die_ref ref)
{
  if (ref->die_offset)
    return ref->die_offset;
  if (comp_unit_die ()->die_abbrev)
    {
      calc_base_type_die_sizes ();
      gcc_assert (ref->die_offset);
    }
  return ref->die_offset;
}

/* Return die_offset of a DIE reference other than base type.  */

static unsigned long int
get_ref_die_offset (dw_die_ref ref)
{
  gcc_assert (ref->die_offset);
  return ref->die_offset;
}

/* Convert a DIE tag into its string name.  */

static const char *
dwarf_tag_name (unsigned int tag)
{
  const char *name = get_DW_TAG_name (tag);

  if (name != NULL)
    return name;

  return "DW_TAG_<unknown>";
}

/* Convert a DWARF attribute code into its string name.  */

static const char *
dwarf_attr_name (unsigned int attr)
{
  const char *name;

  switch (attr)
    {
#if VMS_DEBUGGING_INFO
    case DW_AT_HP_prologue:
      return "DW_AT_HP_prologue";
#else
    case DW_AT_MIPS_loop_unroll_factor:
      return "DW_AT_MIPS_loop_unroll_factor";
#endif

#if VMS_DEBUGGING_INFO
    case DW_AT_HP_epilogue:
      return "DW_AT_HP_epilogue";
#else
    case DW_AT_MIPS_stride:
      return "DW_AT_MIPS_stride";
#endif
    }

  name = get_DW_AT_name (attr);

  if (name != NULL)
    return name;

  return "DW_AT_<unknown>";
}

/* Convert a DWARF value form code into its string name.  */

static const char *
dwarf_form_name (unsigned int form)
{
  const char *name = get_DW_FORM_name (form);

  if (name != NULL)
    return name;

  return "DW_FORM_<unknown>";
}

/* Determine the "ultimate origin" of a decl.  The decl may be an inlined
   instance of an inlined instance of a decl which is local to an inline
   function, so we have to trace all of the way back through the origin chain
   to find out what sort of node actually served as the original seed for the
   given block.  */

static tree
decl_ultimate_origin (const_tree decl)
{
  if (!CODE_CONTAINS_STRUCT (TREE_CODE (decl), TS_DECL_COMMON))
    return NULL_TREE;

  /* DECL_ABSTRACT_ORIGIN can point to itself; ignore that if
     we're trying to output the abstract instance of this function.  */
  if (DECL_ABSTRACT_P (decl) && DECL_ABSTRACT_ORIGIN (decl) == decl)
    return NULL_TREE;

  /* Since the DECL_ABSTRACT_ORIGIN for a DECL is supposed to be the
     most distant ancestor, this should never happen.  */
  gcc_assert (!DECL_FROM_INLINE (DECL_ORIGIN (decl)));

  return DECL_ABSTRACT_ORIGIN (decl);
}

/* Get the class to which DECL belongs, if any.  In g++, the DECL_CONTEXT
   of a virtual function may refer to a base class, so we check the 'this'
   parameter.  */

static tree
decl_class_context (tree decl)
{
  tree context = NULL_TREE;

  if (TREE_CODE (decl) != FUNCTION_DECL || ! DECL_VINDEX (decl))
    context = DECL_CONTEXT (decl);
  else
    context = TYPE_MAIN_VARIANT
      (TREE_TYPE (TREE_VALUE (TYPE_ARG_TYPES (TREE_TYPE (decl)))));

  if (context && !TYPE_P (context))
    context = NULL_TREE;

  return context;
}

/* Add an attribute/value pair to a DIE.  */

static inline void
add_dwarf_attr (dw_die_ref die, dw_attr_node *attr)
{
  /* Maybe this should be an assert?  */
  if (die == NULL)
    return;

  if (flag_checking)
    {
      /* Check we do not add duplicate attrs.  Can't use get_AT here
         because that recurses to the specification/abstract origin DIE.  */
      dw_attr_node *a;
      unsigned ix;
      FOR_EACH_VEC_SAFE_ELT (die->die_attr, ix, a)
	gcc_assert (a->dw_attr != attr->dw_attr);
    }

  vec_safe_reserve (die->die_attr, 1);
  vec_safe_push (die->die_attr, *attr);
}

static inline enum dw_val_class
AT_class (dw_attr_node *a)
{
  return a->dw_attr_val.val_class;
}

/* Return the index for any attribute that will be referenced with a
   DW_FORM_GNU_addr_index or DW_FORM_GNU_str_index.  String indices
   are stored in dw_attr_val.v.val_str for reference counting
   pruning.  */

static inline unsigned int
AT_index (dw_attr_node *a)
{
  if (AT_class (a) == dw_val_class_str)
    return a->dw_attr_val.v.val_str->index;
  else if (a->dw_attr_val.val_entry != NULL)
    return a->dw_attr_val.val_entry->index;
  return NOT_INDEXED;
}

/* Add a flag value attribute to a DIE.  */

static inline void
add_AT_flag (dw_die_ref die, enum dwarf_attribute attr_kind, unsigned int flag)
{
  dw_attr_node attr;

  attr.dw_attr = attr_kind;
  attr.dw_attr_val.val_class = dw_val_class_flag;
  attr.dw_attr_val.val_entry = NULL;
  attr.dw_attr_val.v.val_flag = flag;
  add_dwarf_attr (die, &attr);
}

static inline unsigned
AT_flag (dw_attr_node *a)
{
  gcc_assert (a && AT_class (a) == dw_val_class_flag);
  return a->dw_attr_val.v.val_flag;
}

/* Add a signed integer attribute value to a DIE.  */

static inline void
add_AT_int (dw_die_ref die, enum dwarf_attribute attr_kind, HOST_WIDE_INT int_val)
{
  dw_attr_node attr;

  attr.dw_attr = attr_kind;
  attr.dw_attr_val.val_class = dw_val_class_const;
  attr.dw_attr_val.val_entry = NULL;
  attr.dw_attr_val.v.val_int = int_val;
  add_dwarf_attr (die, &attr);
}

static inline HOST_WIDE_INT
AT_int (dw_attr_node *a)
{
  gcc_assert (a && (AT_class (a) == dw_val_class_const
		    || AT_class (a) == dw_val_class_const_implicit));
  return a->dw_attr_val.v.val_int;
}

/* Add an unsigned integer attribute value to a DIE.  */

static inline void
add_AT_unsigned (dw_die_ref die, enum dwarf_attribute attr_kind,
		 unsigned HOST_WIDE_INT unsigned_val)
{
  dw_attr_node attr;

  attr.dw_attr = attr_kind;
  attr.dw_attr_val.val_class = dw_val_class_unsigned_const;
  attr.dw_attr_val.val_entry = NULL;
  attr.dw_attr_val.v.val_unsigned = unsigned_val;
  add_dwarf_attr (die, &attr);
}

static inline unsigned HOST_WIDE_INT
AT_unsigned (dw_attr_node *a)
{
  gcc_assert (a && (AT_class (a) == dw_val_class_unsigned_const
		    || AT_class (a) == dw_val_class_unsigned_const_implicit));
  return a->dw_attr_val.v.val_unsigned;
}

/* Add an unsigned wide integer attribute value to a DIE.  */

static inline void
add_AT_wide (dw_die_ref die, enum dwarf_attribute attr_kind,
	     const wide_int& w)
{
  dw_attr_node attr;

  attr.dw_attr = attr_kind;
  attr.dw_attr_val.val_class = dw_val_class_wide_int;
  attr.dw_attr_val.val_entry = NULL;
  attr.dw_attr_val.v.val_wide = ggc_alloc<wide_int> ();
  *attr.dw_attr_val.v.val_wide = w;
  add_dwarf_attr (die, &attr);
}

/* Add an unsigned double integer attribute value to a DIE.  */

static inline void
add_AT_double (dw_die_ref die, enum dwarf_attribute attr_kind,
	       HOST_WIDE_INT high, unsigned HOST_WIDE_INT low)
{
  dw_attr_node attr;

  attr.dw_attr = attr_kind;
  attr.dw_attr_val.val_class = dw_val_class_const_double;
  attr.dw_attr_val.val_entry = NULL;
  attr.dw_attr_val.v.val_double.high = high;
  attr.dw_attr_val.v.val_double.low = low;
  add_dwarf_attr (die, &attr);
}

/* Add a floating point attribute value to a DIE and return it.  */

static inline void
add_AT_vec (dw_die_ref die, enum dwarf_attribute attr_kind,
	    unsigned int length, unsigned int elt_size, unsigned char *array)
{
  dw_attr_node attr;

  attr.dw_attr = attr_kind;
  attr.dw_attr_val.val_class = dw_val_class_vec;
  attr.dw_attr_val.val_entry = NULL;
  attr.dw_attr_val.v.val_vec.length = length;
  attr.dw_attr_val.v.val_vec.elt_size = elt_size;
  attr.dw_attr_val.v.val_vec.array = array;
  add_dwarf_attr (die, &attr);
}

/* Add an 8-byte data attribute value to a DIE.  */

static inline void
add_AT_data8 (dw_die_ref die, enum dwarf_attribute attr_kind,
              unsigned char data8[8])
{
  dw_attr_node attr;

  attr.dw_attr = attr_kind;
  attr.dw_attr_val.val_class = dw_val_class_data8;
  attr.dw_attr_val.val_entry = NULL;
  memcpy (attr.dw_attr_val.v.val_data8, data8, 8);
  add_dwarf_attr (die, &attr);
}

/* Add DW_AT_low_pc and DW_AT_high_pc to a DIE.  When using
   dwarf_split_debug_info, address attributes in dies destined for the
   final executable have force_direct set to avoid using indexed
   references.  */

static inline void
add_AT_low_high_pc (dw_die_ref die, const char *lbl_low, const char *lbl_high,
                    bool force_direct)
{
  dw_attr_node attr;
  char * lbl_id;

  lbl_id = xstrdup (lbl_low);
  attr.dw_attr = DW_AT_low_pc;
  attr.dw_attr_val.val_class = dw_val_class_lbl_id;
  attr.dw_attr_val.v.val_lbl_id = lbl_id;
  if (dwarf_split_debug_info && !force_direct)
    attr.dw_attr_val.val_entry
      = add_addr_table_entry (lbl_id, ate_kind_label);
  else
    attr.dw_attr_val.val_entry = NULL;
  add_dwarf_attr (die, &attr);

  attr.dw_attr = DW_AT_high_pc;
  if (dwarf_version < 4)
    attr.dw_attr_val.val_class = dw_val_class_lbl_id;
  else
    attr.dw_attr_val.val_class = dw_val_class_high_pc;
  lbl_id = xstrdup (lbl_high);
  attr.dw_attr_val.v.val_lbl_id = lbl_id;
  if (attr.dw_attr_val.val_class == dw_val_class_lbl_id
      && dwarf_split_debug_info && !force_direct)
    attr.dw_attr_val.val_entry
      = add_addr_table_entry (lbl_id, ate_kind_label);
  else
    attr.dw_attr_val.val_entry = NULL;
  add_dwarf_attr (die, &attr);
}

/* Hash and equality functions for debug_str_hash.  */

hashval_t
indirect_string_hasher::hash (indirect_string_node *x)
{
  return htab_hash_string (x->str);
}

bool
indirect_string_hasher::equal (indirect_string_node *x1, const char *x2)
{
  return strcmp (x1->str, x2) == 0;
}

/* Add STR to the given string hash table.  */

static struct indirect_string_node *
find_AT_string_in_table (const char *str,
			 hash_table<indirect_string_hasher> *table)
{
  struct indirect_string_node *node;

  indirect_string_node **slot
    = table->find_slot_with_hash (str, htab_hash_string (str), INSERT);
  if (*slot == NULL)
    {
      node = ggc_cleared_alloc<indirect_string_node> ();
      node->str = ggc_strdup (str);
      *slot = node;
    }
  else
    node = *slot;

  node->refcount++;
  return node;
}

/* Add STR to the indirect string hash table.  */

static struct indirect_string_node *
find_AT_string (const char *str)
{
  if (! debug_str_hash)
    debug_str_hash = hash_table<indirect_string_hasher>::create_ggc (10);

  return find_AT_string_in_table (str, debug_str_hash);
}

/* Add a string attribute value to a DIE.  */

static inline void
add_AT_string (dw_die_ref die, enum dwarf_attribute attr_kind, const char *str)
{
  dw_attr_node attr;
  struct indirect_string_node *node;

  node = find_AT_string (str);

  attr.dw_attr = attr_kind;
  attr.dw_attr_val.val_class = dw_val_class_str;
  attr.dw_attr_val.val_entry = NULL;
  attr.dw_attr_val.v.val_str = node;
  add_dwarf_attr (die, &attr);
}

static inline const char *
AT_string (dw_attr_node *a)
{
  gcc_assert (a && AT_class (a) == dw_val_class_str);
  return a->dw_attr_val.v.val_str->str;
}

/* Call this function directly to bypass AT_string_form's logic to put
   the string inline in the die. */

static void
set_indirect_string (struct indirect_string_node *node)
{
  char label[MAX_ARTIFICIAL_LABEL_BYTES];
  /* Already indirect is a no op.  */
  if (node->form == DW_FORM_strp
      || node->form == DW_FORM_line_strp
      || node->form == DW_FORM_GNU_str_index)
    {
      gcc_assert (node->label);
      return;
    }
  ASM_GENERATE_INTERNAL_LABEL (label, "LASF", dw2_string_counter);
  ++dw2_string_counter;
  node->label = xstrdup (label);

  if (!dwarf_split_debug_info)
    {
      node->form = DW_FORM_strp;
      node->index = NOT_INDEXED;
    }
  else
    {
      node->form = DW_FORM_GNU_str_index;
      node->index = NO_INDEX_ASSIGNED;
    }
}

/* A helper function for dwarf2out_finish, called to reset indirect
   string decisions done for early LTO dwarf output before fat object
   dwarf output.  */

int
reset_indirect_string (indirect_string_node **h, void *)
{
  struct indirect_string_node *node = *h;
  if (node->form == DW_FORM_strp || node->form == DW_FORM_GNU_str_index)
    {
      free (node->label);
      node->label = NULL;
      node->form = (dwarf_form) 0;
      node->index = 0;
    }
  return 1;
}

/* Find out whether a string should be output inline in DIE
   or out-of-line in .debug_str section.  */

static enum dwarf_form
find_string_form (struct indirect_string_node *node)
{
  unsigned int len;

  if (node->form)
    return node->form;

  len = strlen (node->str) + 1;

  /* If the string is shorter or equal to the size of the reference, it is
     always better to put it inline.  */
  if (len <= DWARF_OFFSET_SIZE || node->refcount == 0)
    return node->form = DW_FORM_string;

  /* If we cannot expect the linker to merge strings in .debug_str
     section, only put it into .debug_str if it is worth even in this
     single module.  */
  if (DWARF2_INDIRECT_STRING_SUPPORT_MISSING_ON_TARGET
      || ((debug_str_section->common.flags & SECTION_MERGE) == 0
	  && (len - DWARF_OFFSET_SIZE) * node->refcount <= len))
    return node->form = DW_FORM_string;

  set_indirect_string (node);

  return node->form;
}

/* Find out whether the string referenced from the attribute should be
   output inline in DIE or out-of-line in .debug_str section.  */

static enum dwarf_form
AT_string_form (dw_attr_node *a)
{
  gcc_assert (a && AT_class (a) == dw_val_class_str);
  return find_string_form (a->dw_attr_val.v.val_str);
}

/* Add a DIE reference attribute value to a DIE.  */

static inline void
add_AT_die_ref (dw_die_ref die, enum dwarf_attribute attr_kind, dw_die_ref targ_die)
{
  dw_attr_node attr;
  gcc_checking_assert (targ_die != NULL);

  /* With LTO we can end up trying to reference something we didn't create
     a DIE for.  Avoid crashing later on a NULL referenced DIE.  */
  if (targ_die == NULL)
    return;

  attr.dw_attr = attr_kind;
  attr.dw_attr_val.val_class = dw_val_class_die_ref;
  attr.dw_attr_val.val_entry = NULL;
  attr.dw_attr_val.v.val_die_ref.die = targ_die;
  attr.dw_attr_val.v.val_die_ref.external = 0;
  add_dwarf_attr (die, &attr);
}

/* Change DIE reference REF to point to NEW_DIE instead.  */

static inline void
change_AT_die_ref (dw_attr_node *ref, dw_die_ref new_die)
{
  gcc_assert (ref->dw_attr_val.val_class == dw_val_class_die_ref);
  ref->dw_attr_val.v.val_die_ref.die = new_die;
  ref->dw_attr_val.v.val_die_ref.external = 0;
}

/* Add an AT_specification attribute to a DIE, and also make the back
   pointer from the specification to the definition.  */

static inline void
add_AT_specification (dw_die_ref die, dw_die_ref targ_die)
{
  add_AT_die_ref (die, DW_AT_specification, targ_die);
  gcc_assert (!targ_die->die_definition);
  targ_die->die_definition = die;
}

static inline dw_die_ref
AT_ref (dw_attr_node *a)
{
  gcc_assert (a && AT_class (a) == dw_val_class_die_ref);
  return a->dw_attr_val.v.val_die_ref.die;
}

static inline int
AT_ref_external (dw_attr_node *a)
{
  if (a && AT_class (a) == dw_val_class_die_ref)
    return a->dw_attr_val.v.val_die_ref.external;

  return 0;
}

static inline void
set_AT_ref_external (dw_attr_node *a, int i)
{
  gcc_assert (a && AT_class (a) == dw_val_class_die_ref);
  a->dw_attr_val.v.val_die_ref.external = i;
}

/* Add an FDE reference attribute value to a DIE.  */

static inline void
add_AT_fde_ref (dw_die_ref die, enum dwarf_attribute attr_kind, unsigned int targ_fde)
{
  dw_attr_node attr;

  attr.dw_attr = attr_kind;
  attr.dw_attr_val.val_class = dw_val_class_fde_ref;
  attr.dw_attr_val.val_entry = NULL;
  attr.dw_attr_val.v.val_fde_index = targ_fde;
  add_dwarf_attr (die, &attr);
}

/* Add a location description attribute value to a DIE.  */

static inline void
add_AT_loc (dw_die_ref die, enum dwarf_attribute attr_kind, dw_loc_descr_ref loc)
{
  dw_attr_node attr;

  attr.dw_attr = attr_kind;
  attr.dw_attr_val.val_class = dw_val_class_loc;
  attr.dw_attr_val.val_entry = NULL;
  attr.dw_attr_val.v.val_loc = loc;
  add_dwarf_attr (die, &attr);
}

static inline dw_loc_descr_ref
AT_loc (dw_attr_node *a)
{
  gcc_assert (a && AT_class (a) == dw_val_class_loc);
  return a->dw_attr_val.v.val_loc;
}

static inline void
add_AT_loc_list (dw_die_ref die, enum dwarf_attribute attr_kind, dw_loc_list_ref loc_list)
{
  dw_attr_node attr;

  if (XCOFF_DEBUGGING_INFO && !HAVE_XCOFF_DWARF_EXTRAS)
    return;

  attr.dw_attr = attr_kind;
  attr.dw_attr_val.val_class = dw_val_class_loc_list;
  attr.dw_attr_val.val_entry = NULL;
  attr.dw_attr_val.v.val_loc_list = loc_list;
  add_dwarf_attr (die, &attr);
  have_location_lists = true;
}

static inline dw_loc_list_ref
AT_loc_list (dw_attr_node *a)
{
  gcc_assert (a && AT_class (a) == dw_val_class_loc_list);
  return a->dw_attr_val.v.val_loc_list;
}

static inline dw_loc_list_ref *
AT_loc_list_ptr (dw_attr_node *a)
{
  gcc_assert (a && AT_class (a) == dw_val_class_loc_list);
  return &a->dw_attr_val.v.val_loc_list;
}

struct addr_hasher : ggc_ptr_hash<addr_table_entry>
{
  static hashval_t hash (addr_table_entry *);
  static bool equal (addr_table_entry *, addr_table_entry *);
};

/* Table of entries into the .debug_addr section.  */

static GTY (()) hash_table<addr_hasher> *addr_index_table;

/* Hash an address_table_entry.  */

hashval_t
addr_hasher::hash (addr_table_entry *a)
{
  inchash::hash hstate;
  switch (a->kind)
    {
      case ate_kind_rtx:
	hstate.add_int (0);
	break;
      case ate_kind_rtx_dtprel:
	hstate.add_int (1);
	break;
      case ate_kind_label:
        return htab_hash_string (a->addr.label);
      default:
        gcc_unreachable ();
    }
  inchash::add_rtx (a->addr.rtl, hstate);
  return hstate.end ();
}

/* Determine equality for two address_table_entries.  */

bool
addr_hasher::equal (addr_table_entry *a1, addr_table_entry *a2)
{
  if (a1->kind != a2->kind)
    return 0;
  switch (a1->kind)
    {
      case ate_kind_rtx:
      case ate_kind_rtx_dtprel:
        return rtx_equal_p (a1->addr.rtl, a2->addr.rtl);
      case ate_kind_label:
        return strcmp (a1->addr.label, a2->addr.label) == 0;
      default:
        gcc_unreachable ();
    }
}

/* Initialize an addr_table_entry.  */

void
init_addr_table_entry (addr_table_entry *e, enum ate_kind kind, void *addr)
{
  e->kind = kind;
  switch (kind)
    {
      case ate_kind_rtx:
      case ate_kind_rtx_dtprel:
        e->addr.rtl = (rtx) addr;
        break;
      case ate_kind_label:
        e->addr.label = (char *) addr;
        break;
    }
  e->refcount = 0;
  e->index = NO_INDEX_ASSIGNED;
}

/* Add attr to the address table entry to the table.  Defer setting an
   index until output time.  */

static addr_table_entry *
add_addr_table_entry (void *addr, enum ate_kind kind)
{
  addr_table_entry *node;
  addr_table_entry finder;

  gcc_assert (dwarf_split_debug_info);
  if (! addr_index_table)
    addr_index_table = hash_table<addr_hasher>::create_ggc (10);
  init_addr_table_entry (&finder, kind, addr);
  addr_table_entry **slot = addr_index_table->find_slot (&finder, INSERT);

  if (*slot == HTAB_EMPTY_ENTRY)
    {
      node = ggc_cleared_alloc<addr_table_entry> ();
      init_addr_table_entry (node, kind, addr);
      *slot = node;
    }
  else
    node = *slot;

  node->refcount++;
  return node;
}

/* Remove an entry from the addr table by decrementing its refcount.
   Strictly, decrementing the refcount would be enough, but the
   assertion that the entry is actually in the table has found
   bugs.  */

static void
remove_addr_table_entry (addr_table_entry *entry)
{
  gcc_assert (dwarf_split_debug_info && addr_index_table);
  /* After an index is assigned, the table is frozen.  */
  gcc_assert (entry->refcount > 0 && entry->index == NO_INDEX_ASSIGNED);
  entry->refcount--;
}

/* Given a location list, remove all addresses it refers to from the
   address_table.  */

static void
remove_loc_list_addr_table_entries (dw_loc_descr_ref descr)
{
  for (; descr; descr = descr->dw_loc_next)
    if (descr->dw_loc_oprnd1.val_entry != NULL)
      {
        gcc_assert (descr->dw_loc_oprnd1.val_entry->index == NO_INDEX_ASSIGNED);
        remove_addr_table_entry (descr->dw_loc_oprnd1.val_entry);
      }
}

/* A helper function for dwarf2out_finish called through
   htab_traverse.  Assign an addr_table_entry its index.  All entries
   must be collected into the table when this function is called,
   because the indexing code relies on htab_traverse to traverse nodes
   in the same order for each run. */

int
index_addr_table_entry (addr_table_entry **h, unsigned int *index)
{
  addr_table_entry *node = *h;

  /* Don't index unreferenced nodes.  */
  if (node->refcount == 0)
    return 1;

  gcc_assert (node->index == NO_INDEX_ASSIGNED);
  node->index = *index;
  *index += 1;

  return 1;
}

/* Add an address constant attribute value to a DIE.  When using
   dwarf_split_debug_info, address attributes in dies destined for the
   final executable should be direct references--setting the parameter
   force_direct ensures this behavior.  */

static inline void
add_AT_addr (dw_die_ref die, enum dwarf_attribute attr_kind, rtx addr,
             bool force_direct)
{
  dw_attr_node attr;

  attr.dw_attr = attr_kind;
  attr.dw_attr_val.val_class = dw_val_class_addr;
  attr.dw_attr_val.v.val_addr = addr;
  if (dwarf_split_debug_info && !force_direct)
    attr.dw_attr_val.val_entry = add_addr_table_entry (addr, ate_kind_rtx);
  else
    attr.dw_attr_val.val_entry = NULL;
  add_dwarf_attr (die, &attr);
}

/* Get the RTX from to an address DIE attribute.  */

static inline rtx
AT_addr (dw_attr_node *a)
{
  gcc_assert (a && AT_class (a) == dw_val_class_addr);
  return a->dw_attr_val.v.val_addr;
}

/* Add a file attribute value to a DIE.  */

static inline void
add_AT_file (dw_die_ref die, enum dwarf_attribute attr_kind,
	     struct dwarf_file_data *fd)
{
  dw_attr_node attr;

  attr.dw_attr = attr_kind;
  attr.dw_attr_val.val_class = dw_val_class_file;
  attr.dw_attr_val.val_entry = NULL;
  attr.dw_attr_val.v.val_file = fd;
  add_dwarf_attr (die, &attr);
}

/* Get the dwarf_file_data from a file DIE attribute.  */

static inline struct dwarf_file_data *
AT_file (dw_attr_node *a)
{
  gcc_assert (a && (AT_class (a) == dw_val_class_file
		    || AT_class (a) == dw_val_class_file_implicit));
  return a->dw_attr_val.v.val_file;
}

/* Add a vms delta attribute value to a DIE.  */

static inline void
add_AT_vms_delta (dw_die_ref die, enum dwarf_attribute attr_kind,
		  const char *lbl1, const char *lbl2)
{
  dw_attr_node attr;

  attr.dw_attr = attr_kind;
  attr.dw_attr_val.val_class = dw_val_class_vms_delta;
  attr.dw_attr_val.val_entry = NULL;
  attr.dw_attr_val.v.val_vms_delta.lbl1 = xstrdup (lbl1);
  attr.dw_attr_val.v.val_vms_delta.lbl2 = xstrdup (lbl2);
  add_dwarf_attr (die, &attr);
}

/* Add a label identifier attribute value to a DIE.  */

static inline void
add_AT_lbl_id (dw_die_ref die, enum dwarf_attribute attr_kind,
               const char *lbl_id)
{
  dw_attr_node attr;

  attr.dw_attr = attr_kind;
  attr.dw_attr_val.val_class = dw_val_class_lbl_id;
  attr.dw_attr_val.val_entry = NULL;
  attr.dw_attr_val.v.val_lbl_id = xstrdup (lbl_id);
  if (dwarf_split_debug_info)
    attr.dw_attr_val.val_entry
        = add_addr_table_entry (attr.dw_attr_val.v.val_lbl_id,
                                ate_kind_label);
  add_dwarf_attr (die, &attr);
}

/* Add a section offset attribute value to a DIE, an offset into the
   debug_line section.  */

static inline void
add_AT_lineptr (dw_die_ref die, enum dwarf_attribute attr_kind,
		const char *label)
{
  dw_attr_node attr;

  attr.dw_attr = attr_kind;
  attr.dw_attr_val.val_class = dw_val_class_lineptr;
  attr.dw_attr_val.val_entry = NULL;
  attr.dw_attr_val.v.val_lbl_id = xstrdup (label);
  add_dwarf_attr (die, &attr);
}

/* Add a section offset attribute value to a DIE, an offset into the
   debug_loclists section.  */

static inline void
add_AT_loclistsptr (dw_die_ref die, enum dwarf_attribute attr_kind,
		    const char *label)
{
  dw_attr_node attr;

  attr.dw_attr = attr_kind;
  attr.dw_attr_val.val_class = dw_val_class_loclistsptr;
  attr.dw_attr_val.val_entry = NULL;
  attr.dw_attr_val.v.val_lbl_id = xstrdup (label);
  add_dwarf_attr (die, &attr);
}

/* Add a section offset attribute value to a DIE, an offset into the
   debug_macinfo section.  */

static inline void
add_AT_macptr (dw_die_ref die, enum dwarf_attribute attr_kind,
	       const char *label)
{
  dw_attr_node attr;

  attr.dw_attr = attr_kind;
  attr.dw_attr_val.val_class = dw_val_class_macptr;
  attr.dw_attr_val.val_entry = NULL;
  attr.dw_attr_val.v.val_lbl_id = xstrdup (label);
  add_dwarf_attr (die, &attr);
}

/* Add an offset attribute value to a DIE.  */

static inline void
add_AT_offset (dw_die_ref die, enum dwarf_attribute attr_kind,
	       unsigned HOST_WIDE_INT offset)
{
  dw_attr_node attr;

  attr.dw_attr = attr_kind;
  attr.dw_attr_val.val_class = dw_val_class_offset;
  attr.dw_attr_val.val_entry = NULL;
  attr.dw_attr_val.v.val_offset = offset;
  add_dwarf_attr (die, &attr);
}

/* Add a range_list attribute value to a DIE.  When using
   dwarf_split_debug_info, address attributes in dies destined for the
   final executable should be direct references--setting the parameter
   force_direct ensures this behavior.  */

#define UNRELOCATED_OFFSET ((addr_table_entry *) 1)
#define RELOCATED_OFFSET (NULL)

static void
add_AT_range_list (dw_die_ref die, enum dwarf_attribute attr_kind,
                   long unsigned int offset, bool force_direct)
{
  dw_attr_node attr;

  attr.dw_attr = attr_kind;
  attr.dw_attr_val.val_class = dw_val_class_range_list;
  /* For the range_list attribute, use val_entry to store whether the
     offset should follow split-debug-info or normal semantics.  This
     value is read in output_range_list_offset.  */
  if (dwarf_split_debug_info && !force_direct)
    attr.dw_attr_val.val_entry = UNRELOCATED_OFFSET;
  else
    attr.dw_attr_val.val_entry = RELOCATED_OFFSET;
  attr.dw_attr_val.v.val_offset = offset;
  add_dwarf_attr (die, &attr);
}

/* Return the start label of a delta attribute.  */

static inline const char *
AT_vms_delta1 (dw_attr_node *a)
{
  gcc_assert (a && (AT_class (a) == dw_val_class_vms_delta));
  return a->dw_attr_val.v.val_vms_delta.lbl1;
}

/* Return the end label of a delta attribute.  */

static inline const char *
AT_vms_delta2 (dw_attr_node *a)
{
  gcc_assert (a && (AT_class (a) == dw_val_class_vms_delta));
  return a->dw_attr_val.v.val_vms_delta.lbl2;
}

static inline const char *
AT_lbl (dw_attr_node *a)
{
  gcc_assert (a && (AT_class (a) == dw_val_class_lbl_id
		    || AT_class (a) == dw_val_class_lineptr
		    || AT_class (a) == dw_val_class_macptr
		    || AT_class (a) == dw_val_class_loclistsptr
		    || AT_class (a) == dw_val_class_high_pc));
  return a->dw_attr_val.v.val_lbl_id;
}

/* Get the attribute of type attr_kind.  */

static dw_attr_node *
get_AT (dw_die_ref die, enum dwarf_attribute attr_kind)
{
  dw_attr_node *a;
  unsigned ix;
  dw_die_ref spec = NULL;

  if (! die)
    return NULL;

  FOR_EACH_VEC_SAFE_ELT (die->die_attr, ix, a)
    if (a->dw_attr == attr_kind)
      return a;
    else if (a->dw_attr == DW_AT_specification
	     || a->dw_attr == DW_AT_abstract_origin)
      spec = AT_ref (a);

  if (spec)
    return get_AT (spec, attr_kind);

  return NULL;
}

/* Returns the parent of the declaration of DIE.  */

static dw_die_ref
get_die_parent (dw_die_ref die)
{
  dw_die_ref t;

  if (!die)
    return NULL;

  if ((t = get_AT_ref (die, DW_AT_abstract_origin))
      || (t = get_AT_ref (die, DW_AT_specification)))
    die = t;

  return die->die_parent;
}

/* Return the "low pc" attribute value, typically associated with a subprogram
   DIE.  Return null if the "low pc" attribute is either not present, or if it
   cannot be represented as an assembler label identifier.  */

static inline const char *
get_AT_low_pc (dw_die_ref die)
{
  dw_attr_node *a = get_AT (die, DW_AT_low_pc);

  return a ? AT_lbl (a) : NULL;
}

/* Return the "high pc" attribute value, typically associated with a subprogram
   DIE.  Return null if the "high pc" attribute is either not present, or if it
   cannot be represented as an assembler label identifier.  */

static inline const char *
get_AT_hi_pc (dw_die_ref die)
{
  dw_attr_node *a = get_AT (die, DW_AT_high_pc);

  return a ? AT_lbl (a) : NULL;
}

/* Return the value of the string attribute designated by ATTR_KIND, or
   NULL if it is not present.  */

static inline const char *
get_AT_string (dw_die_ref die, enum dwarf_attribute attr_kind)
{
  dw_attr_node *a = get_AT (die, attr_kind);

  return a ? AT_string (a) : NULL;
}

/* Return the value of the flag attribute designated by ATTR_KIND, or -1
   if it is not present.  */

static inline int
get_AT_flag (dw_die_ref die, enum dwarf_attribute attr_kind)
{
  dw_attr_node *a = get_AT (die, attr_kind);

  return a ? AT_flag (a) : 0;
}

/* Return the value of the unsigned attribute designated by ATTR_KIND, or 0
   if it is not present.  */

static inline unsigned
get_AT_unsigned (dw_die_ref die, enum dwarf_attribute attr_kind)
{
  dw_attr_node *a = get_AT (die, attr_kind);

  return a ? AT_unsigned (a) : 0;
}

static inline dw_die_ref
get_AT_ref (dw_die_ref die, enum dwarf_attribute attr_kind)
{
  dw_attr_node *a = get_AT (die, attr_kind);

  return a ? AT_ref (a) : NULL;
}

static inline struct dwarf_file_data *
get_AT_file (dw_die_ref die, enum dwarf_attribute attr_kind)
{
  dw_attr_node *a = get_AT (die, attr_kind);

  return a ? AT_file (a) : NULL;
}

/* Returns the ultimate TRANSLATION_UNIT_DECL context of DECL or NULL.  */

static const_tree
get_ultimate_context (const_tree decl)
{
  while (decl && TREE_CODE (decl) != TRANSLATION_UNIT_DECL)
    {
      if (TREE_CODE (decl) == BLOCK)
	decl = BLOCK_SUPERCONTEXT (decl);
      else
	decl = get_containing_scope (decl);
    }
  return decl;
}

/* Return TRUE if the language is C++.  */

static inline bool
is_cxx (void)
{
  unsigned int lang = get_AT_unsigned (comp_unit_die (), DW_AT_language);

  return (lang == DW_LANG_C_plus_plus || lang == DW_LANG_ObjC_plus_plus
	  || lang == DW_LANG_C_plus_plus_11 || lang == DW_LANG_C_plus_plus_14);
}

/* Return TRUE if DECL was created by the C++ frontend.  */

static bool
is_cxx (const_tree decl)
{
  if (in_lto_p)
    {
      const_tree context = get_ultimate_context (decl);
      if (context && TRANSLATION_UNIT_LANGUAGE (context))
	return strncmp (TRANSLATION_UNIT_LANGUAGE (context), "GNU C++", 7) == 0;
    }
  return is_cxx ();
}

/* Return TRUE if the language is Fortran.  */

static inline bool
is_fortran (void)
{
  unsigned int lang = get_AT_unsigned (comp_unit_die (), DW_AT_language);

  return (lang == DW_LANG_Fortran77
	  || lang == DW_LANG_Fortran90
	  || lang == DW_LANG_Fortran95
	  || lang == DW_LANG_Fortran03
	  || lang == DW_LANG_Fortran08);
}

static inline bool
is_fortran (const_tree decl)
{
  if (in_lto_p)
    {
      const_tree context = get_ultimate_context (decl);
      if (context && TRANSLATION_UNIT_LANGUAGE (context))
	return (strncmp (TRANSLATION_UNIT_LANGUAGE (context),
			 "GNU Fortran", 11) == 0
		|| strcmp (TRANSLATION_UNIT_LANGUAGE (context),
			   "GNU F77") == 0);
    }
  return is_fortran ();
}

/* Return TRUE if the language is Ada.  */

static inline bool
is_ada (void)
{
  unsigned int lang = get_AT_unsigned (comp_unit_die (), DW_AT_language);

  return lang == DW_LANG_Ada95 || lang == DW_LANG_Ada83;
}

/* Remove the specified attribute if present.  Return TRUE if removal
   was successful.  */

static bool
remove_AT (dw_die_ref die, enum dwarf_attribute attr_kind)
{
  dw_attr_node *a;
  unsigned ix;

  if (! die)
    return false;

  FOR_EACH_VEC_SAFE_ELT (die->die_attr, ix, a)
    if (a->dw_attr == attr_kind)
      {
	if (AT_class (a) == dw_val_class_str)
	  if (a->dw_attr_val.v.val_str->refcount)
	    a->dw_attr_val.v.val_str->refcount--;

	/* vec::ordered_remove should help reduce the number of abbrevs
	   that are needed.  */
	die->die_attr->ordered_remove (ix);
	return true;
      }
  return false;
}

/* Remove CHILD from its parent.  PREV must have the property that
   PREV->DIE_SIB == CHILD.  Does not alter CHILD.  */

static void
remove_child_with_prev (dw_die_ref child, dw_die_ref prev)
{
  gcc_assert (child->die_parent == prev->die_parent);
  gcc_assert (prev->die_sib == child);
  if (prev == child)
    {
      gcc_assert (child->die_parent->die_child == child);
      prev = NULL;
    }
  else
    prev->die_sib = child->die_sib;
  if (child->die_parent->die_child == child)
    child->die_parent->die_child = prev;
  child->die_sib = NULL;
}

/* Replace OLD_CHILD with NEW_CHILD.  PREV must have the property that
   PREV->DIE_SIB == OLD_CHILD.  Does not alter OLD_CHILD.  */

static void
replace_child (dw_die_ref old_child, dw_die_ref new_child, dw_die_ref prev)
{
  dw_die_ref parent = old_child->die_parent;

  gcc_assert (parent == prev->die_parent);
  gcc_assert (prev->die_sib == old_child);

  new_child->die_parent = parent;
  if (prev == old_child)
    {
      gcc_assert (parent->die_child == old_child);
      new_child->die_sib = new_child;
    }
  else
    {
      prev->die_sib = new_child;
      new_child->die_sib = old_child->die_sib;
    }
  if (old_child->die_parent->die_child == old_child)
    old_child->die_parent->die_child = new_child;
  old_child->die_sib = NULL;
}

/* Move all children from OLD_PARENT to NEW_PARENT.  */

static void
move_all_children (dw_die_ref old_parent, dw_die_ref new_parent)
{
  dw_die_ref c;
  new_parent->die_child = old_parent->die_child;
  old_parent->die_child = NULL;
  FOR_EACH_CHILD (new_parent, c, c->die_parent = new_parent);
}

/* Remove child DIE whose die_tag is TAG.  Do nothing if no child
   matches TAG.  */

static void
remove_child_TAG (dw_die_ref die, enum dwarf_tag tag)
{
  dw_die_ref c;

  c = die->die_child;
  if (c) do {
    dw_die_ref prev = c;
    c = c->die_sib;
    while (c->die_tag == tag)
      {
	remove_child_with_prev (c, prev);
	c->die_parent = NULL;
	/* Might have removed every child.  */
	if (die->die_child == NULL)
	  return;
	c = prev->die_sib;
      }
  } while (c != die->die_child);
}

/* Add a CHILD_DIE as the last child of DIE.  */

static void
add_child_die (dw_die_ref die, dw_die_ref child_die)
{
  /* FIXME this should probably be an assert.  */
  if (! die || ! child_die)
    return;
  gcc_assert (die != child_die);

  child_die->die_parent = die;
  if (die->die_child)
    {
      child_die->die_sib = die->die_child->die_sib;
      die->die_child->die_sib = child_die;
    }
  else
    child_die->die_sib = child_die;
  die->die_child = child_die;
}

/* Like add_child_die, but put CHILD_DIE after AFTER_DIE.  */

static void
add_child_die_after (dw_die_ref die, dw_die_ref child_die,
		     dw_die_ref after_die)
{
  gcc_assert (die
	      && child_die
	      && after_die
	      && die->die_child
	      && die != child_die);

  child_die->die_parent = die;
  child_die->die_sib = after_die->die_sib;
  after_die->die_sib = child_die;
  if (die->die_child == after_die)
    die->die_child = child_die;
}

/* Unassociate CHILD from its parent, and make its parent be
   NEW_PARENT.  */

static void
reparent_child (dw_die_ref child, dw_die_ref new_parent)
{
  for (dw_die_ref p = child->die_parent->die_child; ; p = p->die_sib)
    if (p->die_sib == child)
      {
	remove_child_with_prev (child, p);
	break;
      }
  add_child_die (new_parent, child);
}

/* Move CHILD, which must be a child of PARENT or the DIE for which PARENT
   is the specification, to the end of PARENT's list of children.
   This is done by removing and re-adding it.  */

static void
splice_child_die (dw_die_ref parent, dw_die_ref child)
{
  /* We want the declaration DIE from inside the class, not the
     specification DIE at toplevel.  */
  if (child->die_parent != parent)
    {
      dw_die_ref tmp = get_AT_ref (child, DW_AT_specification);

      if (tmp)
	child = tmp;
    }

  gcc_assert (child->die_parent == parent
	      || (child->die_parent
		  == get_AT_ref (parent, DW_AT_specification)));

  reparent_child (child, parent);
}

/* Create and return a new die with a parent of PARENT_DIE.  If
   PARENT_DIE is NULL, the new DIE is placed in limbo and an
   associated tree T must be supplied to determine parenthood
   later.  */

static inline dw_die_ref
new_die (enum dwarf_tag tag_value, dw_die_ref parent_die, tree t)
{
  dw_die_ref die = ggc_cleared_alloc<die_node> ();

  die->die_tag = tag_value;

  if (parent_die != NULL)
    add_child_die (parent_die, die);
  else
    {
      limbo_die_node *limbo_node;

      /* No DIEs created after early dwarf should end up in limbo,
	 because the limbo list should not persist past LTO
	 streaming.  */
      if (tag_value != DW_TAG_compile_unit
	  /* These are allowed because they're generated while
	     breaking out COMDAT units late.  */
	  && tag_value != DW_TAG_type_unit
	  && tag_value != DW_TAG_skeleton_unit
	  && !early_dwarf
	  /* Allow nested functions to live in limbo because they will
	     only temporarily live there, as decls_for_scope will fix
	     them up.  */
	  && (TREE_CODE (t) != FUNCTION_DECL
	      || !decl_function_context (t))
	  /* Same as nested functions above but for types.  Types that
	     are local to a function will be fixed in
	     decls_for_scope.  */
	  && (!RECORD_OR_UNION_TYPE_P (t)
	      || !TYPE_CONTEXT (t)
	      || TREE_CODE (TYPE_CONTEXT (t)) != FUNCTION_DECL)
	  /* FIXME debug-early: Allow late limbo DIE creation for LTO,
	     especially in the ltrans stage, but once we implement LTO
	     dwarf streaming, we should remove this exception.  */
	  && !in_lto_p)
	{
	  fprintf (stderr, "symbol ended up in limbo too late:");
	  debug_generic_stmt (t);
	  gcc_unreachable ();
	}

      limbo_node = ggc_cleared_alloc<limbo_die_node> ();
      limbo_node->die = die;
      limbo_node->created_for = t;
      limbo_node->next = limbo_die_list;
      limbo_die_list = limbo_node;
    }

  return die;
}

/* Return the DIE associated with the given type specifier.  */

static inline dw_die_ref
lookup_type_die (tree type)
{
  dw_die_ref die = TYPE_SYMTAB_DIE (type);
  if (die && die->removed)
    {
      TYPE_SYMTAB_DIE (type) = NULL;
      return NULL;
    }
  return die;
}

/* Given a TYPE_DIE representing the type TYPE, if TYPE is an
   anonymous type named by the typedef TYPE_DIE, return the DIE of the
   anonymous type instead the one of the naming typedef.  */

static inline dw_die_ref
strip_naming_typedef (tree type, dw_die_ref type_die)
{
  if (type
      && TREE_CODE (type) == RECORD_TYPE
      && type_die
      && type_die->die_tag == DW_TAG_typedef
      && is_naming_typedef_decl (TYPE_NAME (type)))
    type_die = get_AT_ref (type_die, DW_AT_type);
  return type_die;
}

/* Like lookup_type_die, but if type is an anonymous type named by a
   typedef[1], return the DIE of the anonymous type instead the one of
   the naming typedef.  This is because in gen_typedef_die, we did
   equate the anonymous struct named by the typedef with the DIE of
   the naming typedef. So by default, lookup_type_die on an anonymous
   struct yields the DIE of the naming typedef.

   [1]: Read the comment of is_naming_typedef_decl to learn about what
   a naming typedef is.  */

static inline dw_die_ref
lookup_type_die_strip_naming_typedef (tree type)
{
  dw_die_ref die = lookup_type_die (type);
  return strip_naming_typedef (type, die);
}

/* Equate a DIE to a given type specifier.  */

static inline void
equate_type_number_to_die (tree type, dw_die_ref type_die)
{
  TYPE_SYMTAB_DIE (type) = type_die;
}

/* Returns a hash value for X (which really is a die_struct).  */

inline hashval_t
decl_die_hasher::hash (die_node *x)
{
  return (hashval_t) x->decl_id;
}

/* Return nonzero if decl_id of die_struct X is the same as UID of decl *Y.  */

inline bool
decl_die_hasher::equal (die_node *x, tree y)
{
  return (x->decl_id == DECL_UID (y));
}

/* Return the DIE associated with a given declaration.  */

static inline dw_die_ref
lookup_decl_die (tree decl)
{
  dw_die_ref *die = decl_die_table->find_slot_with_hash (decl, DECL_UID (decl),
							 NO_INSERT);
  if (!die)
    return NULL;
  if ((*die)->removed)
    {
      decl_die_table->clear_slot (die);
      return NULL;
    }
  return *die;
}


/* For DECL which might have early dwarf output query a SYMBOL + OFFSET
   style reference.  Return true if we found one refering to a DIE for
   DECL, otherwise return false.  */

static bool
dwarf2out_die_ref_for_decl (tree decl, const char **sym,
			    unsigned HOST_WIDE_INT *off)
{
  dw_die_ref die;

  if (flag_wpa && !decl_die_table)
    return false;

  if (TREE_CODE (decl) == BLOCK)
    die = BLOCK_DIE (decl);
  else
    die = lookup_decl_die (decl);
  if (!die)
    return false;

  /* During WPA stage we currently use DIEs to store the
     decl <-> label + offset map.  That's quite inefficient but it
     works for now.  */
  if (flag_wpa)
    {
      dw_die_ref ref = get_AT_ref (die, DW_AT_abstract_origin);
      if (!ref)
	{
	  gcc_assert (die == comp_unit_die ());
	  return false;
	}
      *off = ref->die_offset;
      *sym = ref->die_id.die_symbol;
      return true;
    }

  /* Similar to get_ref_die_offset_label, but using the "correct"
     label.  */
  *off = die->die_offset;
  while (die->die_parent)
    die = die->die_parent;
  /* For the containing CU DIE we compute a die_symbol in
     compute_comp_unit_symbol.  */
  gcc_assert (die->die_tag == DW_TAG_compile_unit
	      && die->die_id.die_symbol != NULL);
  *sym = die->die_id.die_symbol;
  return true;
}

/* Add a reference of kind ATTR_KIND to a DIE at SYMBOL + OFFSET to DIE.  */

static void
add_AT_external_die_ref (dw_die_ref die, enum dwarf_attribute attr_kind,
			 const char *symbol, HOST_WIDE_INT offset)
{
  /* Create a fake DIE that contains the reference.  Don't use
     new_die because we don't want to end up in the limbo list.  */
  dw_die_ref ref = ggc_cleared_alloc<die_node> ();
  ref->die_tag = die->die_tag;
  ref->die_id.die_symbol = IDENTIFIER_POINTER (get_identifier (symbol));
  ref->die_offset = offset;
  ref->with_offset = 1;
  add_AT_die_ref (die, attr_kind, ref);
}

/* Create a DIE for DECL if required and add a reference to a DIE
   at SYMBOL + OFFSET which contains attributes dumped early.  */

static void
dwarf2out_register_external_die (tree decl, const char *sym,
				 unsigned HOST_WIDE_INT off)
{
  if (debug_info_level == DINFO_LEVEL_NONE)
    return;

  if (flag_wpa && !decl_die_table)
    decl_die_table = hash_table<decl_die_hasher>::create_ggc (1000);

  dw_die_ref die
    = TREE_CODE (decl) == BLOCK ? BLOCK_DIE (decl) : lookup_decl_die (decl);
  gcc_assert (!die);

  tree ctx;
  dw_die_ref parent = NULL;
  /* Need to lookup a DIE for the decls context - the containing
     function or translation unit.  */
  if (TREE_CODE (decl) == BLOCK)
    {
      ctx = BLOCK_SUPERCONTEXT (decl);
      /* ???  We do not output DIEs for all scopes thus skip as
	 many DIEs as needed.  */
      while (TREE_CODE (ctx) == BLOCK
	     && !BLOCK_DIE (ctx))
	ctx = BLOCK_SUPERCONTEXT (ctx);
    }
  else
    ctx = DECL_CONTEXT (decl);
  while (ctx && TYPE_P (ctx))
    ctx = TYPE_CONTEXT (ctx);
  if (ctx)
    {
      if (TREE_CODE (ctx) == BLOCK)
	parent = BLOCK_DIE (ctx);
      else if (TREE_CODE (ctx) == TRANSLATION_UNIT_DECL
	       /* Keep the 1:1 association during WPA.  */
	       && !flag_wpa)
	/* Otherwise all late annotations go to the main CU which
	   imports the original CUs.  */
	parent = comp_unit_die ();
      else if (TREE_CODE (ctx) == FUNCTION_DECL
	       && TREE_CODE (decl) != PARM_DECL
	       && TREE_CODE (decl) != BLOCK)
	/* Leave function local entities parent determination to when
	   we process scope vars.  */
	;
      else
	parent = lookup_decl_die (ctx);
    }
  else
    /* In some cases the FEs fail to set DECL_CONTEXT properly.
       Handle this case gracefully by globalizing stuff.  */
    parent = comp_unit_die ();
  /* Create a DIE "stub".  */
  switch (TREE_CODE (decl))
    {
    case TRANSLATION_UNIT_DECL:
      if (! flag_wpa)
	{
	  die = comp_unit_die ();
	  dw_die_ref import = new_die (DW_TAG_imported_unit, die, NULL_TREE);
	  add_AT_external_die_ref (import, DW_AT_import, sym, off);
	  /* We re-target all CU decls to the LTRANS CU DIE, so no need
	     to create a DIE for the original CUs.  */
	  return;
	}
      /* Keep the 1:1 association during WPA.  */
      die = new_die (DW_TAG_compile_unit, NULL, decl);
      break;
    case NAMESPACE_DECL:
      if (is_fortran (decl))
	die = new_die (DW_TAG_module, parent, decl);
      else
	die = new_die (DW_TAG_namespace, parent, decl);
      break;
    case FUNCTION_DECL:
      die = new_die (DW_TAG_subprogram, parent, decl);
      break;
    case VAR_DECL:
      die = new_die (DW_TAG_variable, parent, decl);
      break;
    case RESULT_DECL:
      die = new_die (DW_TAG_variable, parent, decl);
      break;
    case PARM_DECL:
      die = new_die (DW_TAG_formal_parameter, parent, decl);
      break;
    case CONST_DECL:
      die = new_die (DW_TAG_constant, parent, decl);
      break;
    case LABEL_DECL:
      die = new_die (DW_TAG_label, parent, decl);
      break;
    case BLOCK:
      die = new_die (DW_TAG_lexical_block, parent, decl);
      break;
    default:
      gcc_unreachable ();
    }
  if (TREE_CODE (decl) == BLOCK)
    BLOCK_DIE (decl) = die;
  else
    equate_decl_number_to_die (decl, die);

  /* Add a reference to the DIE providing early debug at $sym + off.  */
  add_AT_external_die_ref (die, DW_AT_abstract_origin, sym, off);
}

/* Returns a hash value for X (which really is a var_loc_list).  */

inline hashval_t
decl_loc_hasher::hash (var_loc_list *x)
{
  return (hashval_t) x->decl_id;
}

/* Return nonzero if decl_id of var_loc_list X is the same as
   UID of decl *Y.  */

inline bool
decl_loc_hasher::equal (var_loc_list *x, const_tree y)
{
  return (x->decl_id == DECL_UID (y));
}

/* Return the var_loc list associated with a given declaration.  */

static inline var_loc_list *
lookup_decl_loc (const_tree decl)
{
  if (!decl_loc_table)
    return NULL;
  return decl_loc_table->find_with_hash (decl, DECL_UID (decl));
}

/* Returns a hash value for X (which really is a cached_dw_loc_list_list).  */

inline hashval_t
dw_loc_list_hasher::hash (cached_dw_loc_list *x)
{
  return (hashval_t) x->decl_id;
}

/* Return nonzero if decl_id of cached_dw_loc_list X is the same as
   UID of decl *Y.  */

inline bool
dw_loc_list_hasher::equal (cached_dw_loc_list *x, const_tree y)
{
  return (x->decl_id == DECL_UID (y));
}

/* Equate a DIE to a particular declaration.  */

static void
equate_decl_number_to_die (tree decl, dw_die_ref decl_die)
{
  unsigned int decl_id = DECL_UID (decl);

  *decl_die_table->find_slot_with_hash (decl, decl_id, INSERT) = decl_die;
  decl_die->decl_id = decl_id;
}

/* Return how many bits covers PIECE EXPR_LIST.  */

static HOST_WIDE_INT
decl_piece_bitsize (rtx piece)
{
  int ret = (int) GET_MODE (piece);
  if (ret)
    return ret;
  gcc_assert (GET_CODE (XEXP (piece, 0)) == CONCAT
	      && CONST_INT_P (XEXP (XEXP (piece, 0), 0)));
  return INTVAL (XEXP (XEXP (piece, 0), 0));
}

/* Return pointer to the location of location note in PIECE EXPR_LIST.  */

static rtx *
decl_piece_varloc_ptr (rtx piece)
{
  if ((int) GET_MODE (piece))
    return &XEXP (piece, 0);
  else
    return &XEXP (XEXP (piece, 0), 1);
}

/* Create an EXPR_LIST for location note LOC_NOTE covering BITSIZE bits.
   Next is the chain of following piece nodes.  */

static rtx_expr_list *
decl_piece_node (rtx loc_note, HOST_WIDE_INT bitsize, rtx next)
{
  if (bitsize > 0 && bitsize <= (int) MAX_MACHINE_MODE)
    return alloc_EXPR_LIST (bitsize, loc_note, next);
  else
    return alloc_EXPR_LIST (0, gen_rtx_CONCAT (VOIDmode,
					       GEN_INT (bitsize),
					       loc_note), next);
}

/* Return rtx that should be stored into loc field for
   LOC_NOTE and BITPOS/BITSIZE.  */

static rtx
construct_piece_list (rtx loc_note, HOST_WIDE_INT bitpos,
		      HOST_WIDE_INT bitsize)
{
  if (bitsize != -1)
    {
      loc_note = decl_piece_node (loc_note, bitsize, NULL_RTX);
      if (bitpos != 0)
	loc_note = decl_piece_node (NULL_RTX, bitpos, loc_note);
    }
  return loc_note;
}

/* This function either modifies location piece list *DEST in
   place (if SRC and INNER is NULL), or copies location piece list
   *SRC to *DEST while modifying it.  Location BITPOS is modified
   to contain LOC_NOTE, any pieces overlapping it are removed resp.
   not copied and if needed some padding around it is added.
   When modifying in place, DEST should point to EXPR_LIST where
   earlier pieces cover PIECE_BITPOS bits, when copying SRC points
   to the start of the whole list and INNER points to the EXPR_LIST
   where earlier pieces cover PIECE_BITPOS bits.  */

static void
adjust_piece_list (rtx *dest, rtx *src, rtx *inner,
		   HOST_WIDE_INT bitpos, HOST_WIDE_INT piece_bitpos,
		   HOST_WIDE_INT bitsize, rtx loc_note)
{
  HOST_WIDE_INT diff;
  bool copy = inner != NULL;

  if (copy)
    {
      /* First copy all nodes preceding the current bitpos.  */
      while (src != inner)
	{
	  *dest = decl_piece_node (*decl_piece_varloc_ptr (*src),
				   decl_piece_bitsize (*src), NULL_RTX);
	  dest = &XEXP (*dest, 1);
	  src = &XEXP (*src, 1);
	}
    }
  /* Add padding if needed.  */
  if (bitpos != piece_bitpos)
    {
      *dest = decl_piece_node (NULL_RTX, bitpos - piece_bitpos,
			       copy ? NULL_RTX : *dest);
      dest = &XEXP (*dest, 1);
    }
  else if (*dest && decl_piece_bitsize (*dest) == bitsize)
    {
      gcc_assert (!copy);
      /* A piece with correct bitpos and bitsize already exist,
	 just update the location for it and return.  */
      *decl_piece_varloc_ptr (*dest) = loc_note;
      return;
    }
  /* Add the piece that changed.  */
  *dest = decl_piece_node (loc_note, bitsize, copy ? NULL_RTX : *dest);
  dest = &XEXP (*dest, 1);
  /* Skip over pieces that overlap it.  */
  diff = bitpos - piece_bitpos + bitsize;
  if (!copy)
    src = dest;
  while (diff > 0 && *src)
    {
      rtx piece = *src;
      diff -= decl_piece_bitsize (piece);
      if (copy)
	src = &XEXP (piece, 1);
      else
	{
	  *src = XEXP (piece, 1);
	  free_EXPR_LIST_node (piece);
	}
    }
  /* Add padding if needed.  */
  if (diff < 0 && *src)
    {
      if (!copy)
	dest = src;
      *dest = decl_piece_node (NULL_RTX, -diff, copy ? NULL_RTX : *dest);
      dest = &XEXP (*dest, 1);
    }
  if (!copy)
    return;
  /* Finally copy all nodes following it.  */
  while (*src)
    {
      *dest = decl_piece_node (*decl_piece_varloc_ptr (*src),
			       decl_piece_bitsize (*src), NULL_RTX);
      dest = &XEXP (*dest, 1);
      src = &XEXP (*src, 1);
    }
}

/* Add a variable location node to the linked list for DECL.  */

static struct var_loc_node *
add_var_loc_to_decl (tree decl, rtx loc_note, const char *label)
{
  unsigned int decl_id;
  var_loc_list *temp;
  struct var_loc_node *loc = NULL;
  HOST_WIDE_INT bitsize = -1, bitpos = -1;

  if (VAR_P (decl) && DECL_HAS_DEBUG_EXPR_P (decl))
    {
      tree realdecl = DECL_DEBUG_EXPR (decl);
      if (handled_component_p (realdecl)
	  || (TREE_CODE (realdecl) == MEM_REF
	      && TREE_CODE (TREE_OPERAND (realdecl, 0)) == ADDR_EXPR))
	{
	  HOST_WIDE_INT maxsize;
	  bool reverse;
	  tree innerdecl
	    = get_ref_base_and_extent (realdecl, &bitpos, &bitsize, &maxsize,
				       &reverse);
	  if (!DECL_P (innerdecl)
	      || DECL_IGNORED_P (innerdecl)
	      || TREE_STATIC (innerdecl)
	      || bitsize <= 0
	      || bitpos + bitsize > 256
	      || bitsize != maxsize)
	    return NULL;
	  decl = innerdecl;
	}
    }

  decl_id = DECL_UID (decl);
  var_loc_list **slot
    = decl_loc_table->find_slot_with_hash (decl, decl_id, INSERT);
  if (*slot == NULL)
    {
      temp = ggc_cleared_alloc<var_loc_list> ();
      temp->decl_id = decl_id;
      *slot = temp;
    }
  else
    temp = *slot;

  /* For PARM_DECLs try to keep around the original incoming value,
     even if that means we'll emit a zero-range .debug_loc entry.  */
  if (temp->last
      && temp->first == temp->last
      && TREE_CODE (decl) == PARM_DECL
      && NOTE_P (temp->first->loc)
      && NOTE_VAR_LOCATION_DECL (temp->first->loc) == decl
      && DECL_INCOMING_RTL (decl)
      && NOTE_VAR_LOCATION_LOC (temp->first->loc)
      && GET_CODE (NOTE_VAR_LOCATION_LOC (temp->first->loc))
	 == GET_CODE (DECL_INCOMING_RTL (decl))
      && prev_real_insn (as_a<rtx_insn *> (temp->first->loc)) == NULL_RTX
      && (bitsize != -1
	  || !rtx_equal_p (NOTE_VAR_LOCATION_LOC (temp->first->loc),
			   NOTE_VAR_LOCATION_LOC (loc_note))
	  || (NOTE_VAR_LOCATION_STATUS (temp->first->loc)
	      != NOTE_VAR_LOCATION_STATUS (loc_note))))
    {
      loc = ggc_cleared_alloc<var_loc_node> ();
      temp->first->next = loc;
      temp->last = loc;
      loc->loc = construct_piece_list (loc_note, bitpos, bitsize);
    }
  else if (temp->last)
    {
      struct var_loc_node *last = temp->last, *unused = NULL;
      rtx *piece_loc = NULL, last_loc_note;
      HOST_WIDE_INT piece_bitpos = 0;
      if (last->next)
	{
	  last = last->next;
	  gcc_assert (last->next == NULL);
	}
      if (bitsize != -1 && GET_CODE (last->loc) == EXPR_LIST)
	{
	  piece_loc = &last->loc;
	  do
	    {
	      HOST_WIDE_INT cur_bitsize = decl_piece_bitsize (*piece_loc);
	      if (piece_bitpos + cur_bitsize > bitpos)
		break;
	      piece_bitpos += cur_bitsize;
	      piece_loc = &XEXP (*piece_loc, 1);
	    }
	  while (*piece_loc);
	}
      /* TEMP->LAST here is either pointer to the last but one or
	 last element in the chained list, LAST is pointer to the
	 last element.  */
      if (label && strcmp (last->label, label) == 0)
	{
	  /* For SRA optimized variables if there weren't any real
	     insns since last note, just modify the last node.  */
	  if (piece_loc != NULL)
	    {
	      adjust_piece_list (piece_loc, NULL, NULL,
				 bitpos, piece_bitpos, bitsize, loc_note);
	      return NULL;
	    }
	  /* If the last note doesn't cover any instructions, remove it.  */
	  if (temp->last != last)
	    {
	      temp->last->next = NULL;
	      unused = last;
	      last = temp->last;
	      gcc_assert (strcmp (last->label, label) != 0);
	    }
	  else
	    {
	      gcc_assert (temp->first == temp->last
			  || (temp->first->next == temp->last
			      && TREE_CODE (decl) == PARM_DECL));
	      memset (temp->last, '\0', sizeof (*temp->last));
	      temp->last->loc = construct_piece_list (loc_note, bitpos, bitsize);
	      return temp->last;
	    }
	}
      if (bitsize == -1 && NOTE_P (last->loc))
	last_loc_note = last->loc;
      else if (piece_loc != NULL
	       && *piece_loc != NULL_RTX
	       && piece_bitpos == bitpos
	       && decl_piece_bitsize (*piece_loc) == bitsize)
	last_loc_note = *decl_piece_varloc_ptr (*piece_loc);
      else
	last_loc_note = NULL_RTX;
      /* If the current location is the same as the end of the list,
	 and either both or neither of the locations is uninitialized,
	 we have nothing to do.  */
      if (last_loc_note == NULL_RTX
	  || (!rtx_equal_p (NOTE_VAR_LOCATION_LOC (last_loc_note),
			    NOTE_VAR_LOCATION_LOC (loc_note)))
	  || ((NOTE_VAR_LOCATION_STATUS (last_loc_note)
	       != NOTE_VAR_LOCATION_STATUS (loc_note))
	      && ((NOTE_VAR_LOCATION_STATUS (last_loc_note)
		   == VAR_INIT_STATUS_UNINITIALIZED)
		  || (NOTE_VAR_LOCATION_STATUS (loc_note)
		      == VAR_INIT_STATUS_UNINITIALIZED))))
	{
	  /* Add LOC to the end of list and update LAST.  If the last
	     element of the list has been removed above, reuse its
	     memory for the new node, otherwise allocate a new one.  */
	  if (unused)
	    {
	      loc = unused;
	      memset (loc, '\0', sizeof (*loc));
	    }
	  else
	    loc = ggc_cleared_alloc<var_loc_node> ();
	  if (bitsize == -1 || piece_loc == NULL)
	    loc->loc = construct_piece_list (loc_note, bitpos, bitsize);
	  else
	    adjust_piece_list (&loc->loc, &last->loc, piece_loc,
			       bitpos, piece_bitpos, bitsize, loc_note);
	  last->next = loc;
	  /* Ensure TEMP->LAST will point either to the new last but one
	     element of the chain, or to the last element in it.  */
	  if (last != temp->last)
	    temp->last = last;
	}
      else if (unused)
	ggc_free (unused);
    }
  else
    {
      loc = ggc_cleared_alloc<var_loc_node> ();
      temp->first = loc;
      temp->last = loc;
      loc->loc = construct_piece_list (loc_note, bitpos, bitsize);
    }
  return loc;
}

/* Keep track of the number of spaces used to indent the
   output of the debugging routines that print the structure of
   the DIE internal representation.  */
static int print_indent;

/* Indent the line the number of spaces given by print_indent.  */

static inline void
print_spaces (FILE *outfile)
{
  fprintf (outfile, "%*s", print_indent, "");
}

/* Print a type signature in hex.  */

static inline void
print_signature (FILE *outfile, char *sig)
{
  int i;

  for (i = 0; i < DWARF_TYPE_SIGNATURE_SIZE; i++)
    fprintf (outfile, "%02x", sig[i] & 0xff);
}

static inline void
print_discr_value (FILE *outfile, dw_discr_value *discr_value)
{
  if (discr_value->pos)
    fprintf (outfile, HOST_WIDE_INT_PRINT_UNSIGNED, discr_value->v.sval);
  else
    fprintf (outfile, HOST_WIDE_INT_PRINT_DEC, discr_value->v.uval);
}

static void print_loc_descr (dw_loc_descr_ref, FILE *);

/* Print the value associated to the VAL DWARF value node to OUTFILE.  If
   RECURSE, output location descriptor operations.  */

static void
print_dw_val (dw_val_node *val, bool recurse, FILE *outfile)
{
  switch (val->val_class)
    {
    case dw_val_class_addr:
      fprintf (outfile, "address");
      break;
    case dw_val_class_offset:
      fprintf (outfile, "offset");
      break;
    case dw_val_class_loc:
      fprintf (outfile, "location descriptor");
      if (val->v.val_loc == NULL)
	fprintf (outfile, " -> <null>\n");
      else if (recurse)
	{
	  fprintf (outfile, ":\n");
	  print_indent += 4;
	  print_loc_descr (val->v.val_loc, outfile);
	  print_indent -= 4;
	}
      else
	fprintf (outfile, " (%p)\n", (void *) val->v.val_loc);
      break;
    case dw_val_class_loc_list:
      fprintf (outfile, "location list -> label:%s",
	       val->v.val_loc_list->ll_symbol);
      break;
    case dw_val_class_range_list:
      fprintf (outfile, "range list");
      break;
    case dw_val_class_const:
    case dw_val_class_const_implicit:
      fprintf (outfile, HOST_WIDE_INT_PRINT_DEC, val->v.val_int);
      break;
    case dw_val_class_unsigned_const:
    case dw_val_class_unsigned_const_implicit:
      fprintf (outfile, HOST_WIDE_INT_PRINT_UNSIGNED, val->v.val_unsigned);
      break;
    case dw_val_class_const_double:
      fprintf (outfile, "constant (" HOST_WIDE_INT_PRINT_DEC","\
			HOST_WIDE_INT_PRINT_UNSIGNED")",
	       val->v.val_double.high,
	       val->v.val_double.low);
      break;
    case dw_val_class_wide_int:
      {
	int i = val->v.val_wide->get_len ();
	fprintf (outfile, "constant (");
	gcc_assert (i > 0);
	if (val->v.val_wide->elt (i - 1) == 0)
	  fprintf (outfile, "0x");
	fprintf (outfile, HOST_WIDE_INT_PRINT_HEX,
		 val->v.val_wide->elt (--i));
	while (--i >= 0)
	  fprintf (outfile, HOST_WIDE_INT_PRINT_PADDED_HEX,
		   val->v.val_wide->elt (i));
	fprintf (outfile, ")");
	break;
      }
    case dw_val_class_vec:
      fprintf (outfile, "floating-point or vector constant");
      break;
    case dw_val_class_flag:
      fprintf (outfile, "%u", val->v.val_flag);
      break;
    case dw_val_class_die_ref:
      if (val->v.val_die_ref.die != NULL)
	{
	  dw_die_ref die = val->v.val_die_ref.die;

	  if (die->comdat_type_p)
	    {
	      fprintf (outfile, "die -> signature: ");
	      print_signature (outfile,
			       die->die_id.die_type_node->signature);
	    }
	  else if (die->die_id.die_symbol)
	    {
	      fprintf (outfile, "die -> label: %s", die->die_id.die_symbol);
	      if (die->with_offset)
		fprintf (outfile, " + %ld", die->die_offset);
	    }
	  else
	    fprintf (outfile, "die -> %ld", die->die_offset);
	  fprintf (outfile, " (%p)", (void *) die);
	}
      else
	fprintf (outfile, "die -> <null>");
      break;
    case dw_val_class_vms_delta:
      fprintf (outfile, "delta: @slotcount(%s-%s)",
	       val->v.val_vms_delta.lbl2, val->v.val_vms_delta.lbl1);
      break;
    case dw_val_class_lbl_id:
    case dw_val_class_lineptr:
    case dw_val_class_macptr:
    case dw_val_class_loclistsptr:
    case dw_val_class_high_pc:
      fprintf (outfile, "label: %s", val->v.val_lbl_id);
      break;
    case dw_val_class_str:
      if (val->v.val_str->str != NULL)
	fprintf (outfile, "\"%s\"", val->v.val_str->str);
      else
	fprintf (outfile, "<null>");
      break;
    case dw_val_class_file:
    case dw_val_class_file_implicit:
      fprintf (outfile, "\"%s\" (%d)", val->v.val_file->filename,
	       val->v.val_file->emitted_number);
      break;
    case dw_val_class_data8:
      {
	int i;

	for (i = 0; i < 8; i++)
	  fprintf (outfile, "%02x", val->v.val_data8[i]);
	break;
      }
    case dw_val_class_discr_value:
      print_discr_value (outfile, &val->v.val_discr_value);
      break;
    case dw_val_class_discr_list:
      for (dw_discr_list_ref node = val->v.val_discr_list;
	   node != NULL;
	   node = node->dw_discr_next)
	{
	  if (node->dw_discr_range)
	    {
	      fprintf (outfile, " .. ");
	      print_discr_value (outfile, &node->dw_discr_lower_bound);
	      print_discr_value (outfile, &node->dw_discr_upper_bound);
	    }
	  else
	    print_discr_value (outfile, &node->dw_discr_lower_bound);

	  if (node->dw_discr_next != NULL)
	    fprintf (outfile, " | ");
	}
    default:
      break;
    }
}

/* Likewise, for a DIE attribute.  */

static void
print_attribute (dw_attr_node *a, bool recurse, FILE *outfile)
{
  print_dw_val (&a->dw_attr_val, recurse, outfile);
}


/* Print the list of operands in the LOC location description to OUTFILE.  This
   routine is a debugging aid only.  */

static void
print_loc_descr (dw_loc_descr_ref loc, FILE *outfile)
{
  dw_loc_descr_ref l = loc;

  if (loc == NULL)
    {
      print_spaces (outfile);
      fprintf (outfile, "<null>\n");
      return;
    }

  for (l = loc; l != NULL; l = l->dw_loc_next)
    {
      print_spaces (outfile);
      fprintf (outfile, "(%p) %s",
	       (void *) l,
	       dwarf_stack_op_name (l->dw_loc_opc));
      if (l->dw_loc_oprnd1.val_class != dw_val_class_none)
	{
	  fprintf (outfile, " ");
	  print_dw_val (&l->dw_loc_oprnd1, false, outfile);
	}
      if (l->dw_loc_oprnd2.val_class != dw_val_class_none)
	{
	  fprintf (outfile, ", ");
	  print_dw_val (&l->dw_loc_oprnd2, false, outfile);
	}
      fprintf (outfile, "\n");
    }
}

/* Print the information associated with a given DIE, and its children.
   This routine is a debugging aid only.  */

static void
print_die (dw_die_ref die, FILE *outfile)
{
  dw_attr_node *a;
  dw_die_ref c;
  unsigned ix;

  print_spaces (outfile);
  fprintf (outfile, "DIE %4ld: %s (%p)\n",
	   die->die_offset, dwarf_tag_name (die->die_tag),
	   (void*) die);
  print_spaces (outfile);
  fprintf (outfile, "  abbrev id: %lu", die->die_abbrev);
  fprintf (outfile, " offset: %ld", die->die_offset);
  fprintf (outfile, " mark: %d\n", die->die_mark);

  if (die->comdat_type_p)
    {
      print_spaces (outfile);
      fprintf (outfile, "  signature: ");
      print_signature (outfile, die->die_id.die_type_node->signature);
      fprintf (outfile, "\n");
    }

  FOR_EACH_VEC_SAFE_ELT (die->die_attr, ix, a)
    {
      print_spaces (outfile);
      fprintf (outfile, "  %s: ", dwarf_attr_name (a->dw_attr));

      print_attribute (a, true, outfile);
      fprintf (outfile, "\n");
    }

  if (die->die_child != NULL)
    {
      print_indent += 4;
      FOR_EACH_CHILD (die, c, print_die (c, outfile));
      print_indent -= 4;
    }
  if (print_indent == 0)
    fprintf (outfile, "\n");
}

/* Print the list of operations in the LOC location description.  */

DEBUG_FUNCTION void
debug_dwarf_loc_descr (dw_loc_descr_ref loc)
{
  print_loc_descr (loc, stderr);
}

/* Print the information collected for a given DIE.  */

DEBUG_FUNCTION void
debug_dwarf_die (dw_die_ref die)
{
  print_die (die, stderr);
}

DEBUG_FUNCTION void
debug (die_struct &ref)
{
  print_die (&ref, stderr);
}

DEBUG_FUNCTION void
debug (die_struct *ptr)
{
  if (ptr)
    debug (*ptr);
  else
    fprintf (stderr, "<nil>\n");
}


/* Print all DWARF information collected for the compilation unit.
   This routine is a debugging aid only.  */

DEBUG_FUNCTION void
debug_dwarf (void)
{
  print_indent = 0;
  print_die (comp_unit_die (), stderr);
}

/* Verify the DIE tree structure.  */

DEBUG_FUNCTION void
verify_die (dw_die_ref die)
{
  gcc_assert (!die->die_mark);
  if (die->die_parent == NULL
      && die->die_sib == NULL)
    return;
  /* Verify the die_sib list is cyclic.  */
  dw_die_ref x = die;
  do
    {
      x->die_mark = 1;
      x = x->die_sib;
    }
  while (x && !x->die_mark);
  gcc_assert (x == die);
  x = die;
  do
    {
      /* Verify all dies have the same parent.  */
      gcc_assert (x->die_parent == die->die_parent);
      if (x->die_child)
	{
	  /* Verify the child has the proper parent and recurse.  */
	  gcc_assert (x->die_child->die_parent == x);
	  verify_die (x->die_child);
	}
      x->die_mark = 0;
      x = x->die_sib;
    }
  while (x && x->die_mark);
}

/* Sanity checks on DIEs.  */

static void
check_die (dw_die_ref die)
{
  unsigned ix;
  dw_attr_node *a;
  bool inline_found = false;
  int n_location = 0, n_low_pc = 0, n_high_pc = 0, n_artificial = 0;
  int n_decl_line = 0, n_decl_column = 0, n_decl_file = 0;
  FOR_EACH_VEC_SAFE_ELT (die->die_attr, ix, a)
    {
      switch (a->dw_attr)
	{
	case DW_AT_inline:
	  if (a->dw_attr_val.v.val_unsigned)
	    inline_found = true;
	  break;
	case DW_AT_location:
	  ++n_location;
	  break;
	case DW_AT_low_pc:
	  ++n_low_pc;
	  break;
	case DW_AT_high_pc:
	  ++n_high_pc;
	  break;
	case DW_AT_artificial:
	  ++n_artificial;
	  break;
        case DW_AT_decl_column:
	  ++n_decl_column;
	  break;
	case DW_AT_decl_line:
	  ++n_decl_line;
	  break;
	case DW_AT_decl_file:
	  ++n_decl_file;
	  break;
	default:
	  break;
	}
    }
  if (n_location > 1 || n_low_pc > 1 || n_high_pc > 1 || n_artificial > 1
      || n_decl_column > 1 || n_decl_line > 1 || n_decl_file > 1)
    {
      fprintf (stderr, "Duplicate attributes in DIE:\n");
      debug_dwarf_die (die);
      gcc_unreachable ();
    }
  if (inline_found)
    {
      /* A debugging information entry that is a member of an abstract
	 instance tree [that has DW_AT_inline] should not contain any
	 attributes which describe aspects of the subroutine which vary
	 between distinct inlined expansions or distinct out-of-line
	 expansions.  */
      FOR_EACH_VEC_SAFE_ELT (die->die_attr, ix, a)
	gcc_assert (a->dw_attr != DW_AT_low_pc
		    && a->dw_attr != DW_AT_high_pc
		    && a->dw_attr != DW_AT_location
		    && a->dw_attr != DW_AT_frame_base
		    && a->dw_attr != DW_AT_call_all_calls
		    && a->dw_attr != DW_AT_GNU_all_call_sites);
    }
}

#define CHECKSUM(FOO) md5_process_bytes (&(FOO), sizeof (FOO), ctx)
#define CHECKSUM_BLOCK(FOO, SIZE) md5_process_bytes ((FOO), (SIZE), ctx)
#define CHECKSUM_STRING(FOO) md5_process_bytes ((FOO), strlen (FOO), ctx)

/* Calculate the checksum of a location expression.  */

static inline void
loc_checksum (dw_loc_descr_ref loc, struct md5_ctx *ctx)
{
  int tem;
  inchash::hash hstate;
  hashval_t hash;

  tem = (loc->dtprel << 8) | ((unsigned int) loc->dw_loc_opc);
  CHECKSUM (tem);
  hash_loc_operands (loc, hstate);
  hash = hstate.end();
  CHECKSUM (hash);
}

/* Calculate the checksum of an attribute.  */

static void
attr_checksum (dw_attr_node *at, struct md5_ctx *ctx, int *mark)
{
  dw_loc_descr_ref loc;
  rtx r;

  CHECKSUM (at->dw_attr);

  /* We don't care that this was compiled with a different compiler
     snapshot; if the output is the same, that's what matters.  */
  if (at->dw_attr == DW_AT_producer)
    return;

  switch (AT_class (at))
    {
    case dw_val_class_const:
    case dw_val_class_const_implicit:
      CHECKSUM (at->dw_attr_val.v.val_int);
      break;
    case dw_val_class_unsigned_const:
    case dw_val_class_unsigned_const_implicit:
      CHECKSUM (at->dw_attr_val.v.val_unsigned);
      break;
    case dw_val_class_const_double:
      CHECKSUM (at->dw_attr_val.v.val_double);
      break;
    case dw_val_class_wide_int:
      CHECKSUM_BLOCK (at->dw_attr_val.v.val_wide->get_val (),
		      get_full_len (*at->dw_attr_val.v.val_wide)
		      * HOST_BITS_PER_WIDE_INT / HOST_BITS_PER_CHAR);
      break;
    case dw_val_class_vec:
      CHECKSUM_BLOCK (at->dw_attr_val.v.val_vec.array,
		      (at->dw_attr_val.v.val_vec.length
		       * at->dw_attr_val.v.val_vec.elt_size));
      break;
    case dw_val_class_flag:
      CHECKSUM (at->dw_attr_val.v.val_flag);
      break;
    case dw_val_class_str:
      CHECKSUM_STRING (AT_string (at));
      break;

    case dw_val_class_addr:
      r = AT_addr (at);
      gcc_assert (GET_CODE (r) == SYMBOL_REF);
      CHECKSUM_STRING (XSTR (r, 0));
      break;

    case dw_val_class_offset:
      CHECKSUM (at->dw_attr_val.v.val_offset);
      break;

    case dw_val_class_loc:
      for (loc = AT_loc (at); loc; loc = loc->dw_loc_next)
	loc_checksum (loc, ctx);
      break;

    case dw_val_class_die_ref:
      die_checksum (AT_ref (at), ctx, mark);
      break;

    case dw_val_class_fde_ref:
    case dw_val_class_vms_delta:
    case dw_val_class_lbl_id:
    case dw_val_class_lineptr:
    case dw_val_class_macptr:
    case dw_val_class_loclistsptr:
    case dw_val_class_high_pc:
      break;

    case dw_val_class_file:
    case dw_val_class_file_implicit:
      CHECKSUM_STRING (AT_file (at)->filename);
      break;

    case dw_val_class_data8:
      CHECKSUM (at->dw_attr_val.v.val_data8);
      break;

    default:
      break;
    }
}

/* Calculate the checksum of a DIE.  */

static void
die_checksum (dw_die_ref die, struct md5_ctx *ctx, int *mark)
{
  dw_die_ref c;
  dw_attr_node *a;
  unsigned ix;

  /* To avoid infinite recursion.  */
  if (die->die_mark)
    {
      CHECKSUM (die->die_mark);
      return;
    }
  die->die_mark = ++(*mark);

  CHECKSUM (die->die_tag);

  FOR_EACH_VEC_SAFE_ELT (die->die_attr, ix, a)
    attr_checksum (a, ctx, mark);

  FOR_EACH_CHILD (die, c, die_checksum (c, ctx, mark));
}

#undef CHECKSUM
#undef CHECKSUM_BLOCK
#undef CHECKSUM_STRING

/* For DWARF-4 types, include the trailing NULL when checksumming strings.  */
#define CHECKSUM(FOO) md5_process_bytes (&(FOO), sizeof (FOO), ctx)
#define CHECKSUM_BLOCK(FOO, SIZE) md5_process_bytes ((FOO), (SIZE), ctx)
#define CHECKSUM_STRING(FOO) md5_process_bytes ((FOO), strlen (FOO) + 1, ctx)
#define CHECKSUM_SLEB128(FOO) checksum_sleb128 ((FOO), ctx)
#define CHECKSUM_ULEB128(FOO) checksum_uleb128 ((FOO), ctx)
#define CHECKSUM_ATTR(FOO) \
  if (FOO) attr_checksum_ordered (die->die_tag, (FOO), ctx, mark)

/* Calculate the checksum of a number in signed LEB128 format.  */

static void
checksum_sleb128 (HOST_WIDE_INT value, struct md5_ctx *ctx)
{
  unsigned char byte;
  bool more;

  while (1)
    {
      byte = (value & 0x7f);
      value >>= 7;
      more = !((value == 0 && (byte & 0x40) == 0)
		|| (value == -1 && (byte & 0x40) != 0));
      if (more)
	byte |= 0x80;
      CHECKSUM (byte);
      if (!more)
	break;
    }
}

/* Calculate the checksum of a number in unsigned LEB128 format.  */

static void
checksum_uleb128 (unsigned HOST_WIDE_INT value, struct md5_ctx *ctx)
{
  while (1)
    {
      unsigned char byte = (value & 0x7f);
      value >>= 7;
      if (value != 0)
	/* More bytes to follow.  */
	byte |= 0x80;
      CHECKSUM (byte);
      if (value == 0)
	break;
    }
}

/* Checksum the context of the DIE.  This adds the names of any
   surrounding namespaces or structures to the checksum.  */

static void
checksum_die_context (dw_die_ref die, struct md5_ctx *ctx)
{
  const char *name;
  dw_die_ref spec;
  int tag = die->die_tag;

  if (tag != DW_TAG_namespace
      && tag != DW_TAG_structure_type
      && tag != DW_TAG_class_type)
    return;

  name = get_AT_string (die, DW_AT_name);

  spec = get_AT_ref (die, DW_AT_specification);
  if (spec != NULL)
    die = spec;

  if (die->die_parent != NULL)
    checksum_die_context (die->die_parent, ctx);

  CHECKSUM_ULEB128 ('C');
  CHECKSUM_ULEB128 (tag);
  if (name != NULL)
    CHECKSUM_STRING (name);
}

/* Calculate the checksum of a location expression.  */

static inline void
loc_checksum_ordered (dw_loc_descr_ref loc, struct md5_ctx *ctx)
{
  /* Special case for lone DW_OP_plus_uconst: checksum as if the location
     were emitted as a DW_FORM_sdata instead of a location expression.  */
  if (loc->dw_loc_opc == DW_OP_plus_uconst && loc->dw_loc_next == NULL)
    {
      CHECKSUM_ULEB128 (DW_FORM_sdata);
      CHECKSUM_SLEB128 ((HOST_WIDE_INT) loc->dw_loc_oprnd1.v.val_unsigned);
      return;
    }

  /* Otherwise, just checksum the raw location expression.  */
  while (loc != NULL)
    {
      inchash::hash hstate;
      hashval_t hash;

      CHECKSUM_ULEB128 (loc->dtprel);
      CHECKSUM_ULEB128 (loc->dw_loc_opc);
      hash_loc_operands (loc, hstate);
      hash = hstate.end ();
      CHECKSUM (hash);
      loc = loc->dw_loc_next;
    }
}

/* Calculate the checksum of an attribute.  */

static void
attr_checksum_ordered (enum dwarf_tag tag, dw_attr_node *at,
		       struct md5_ctx *ctx, int *mark)
{
  dw_loc_descr_ref loc;
  rtx r;

  if (AT_class (at) == dw_val_class_die_ref)
    {
      dw_die_ref target_die = AT_ref (at);

      /* For pointer and reference types, we checksum only the (qualified)
	 name of the target type (if there is a name).  For friend entries,
	 we checksum only the (qualified) name of the target type or function.
	 This allows the checksum to remain the same whether the target type
	 is complete or not.  */
      if ((at->dw_attr == DW_AT_type
	   && (tag == DW_TAG_pointer_type
	       || tag == DW_TAG_reference_type
	       || tag == DW_TAG_rvalue_reference_type
	       || tag == DW_TAG_ptr_to_member_type))
	  || (at->dw_attr == DW_AT_friend
	      && tag == DW_TAG_friend))
	{
	  dw_attr_node *name_attr = get_AT (target_die, DW_AT_name);

	  if (name_attr != NULL)
	    {
	      dw_die_ref decl = get_AT_ref (target_die, DW_AT_specification);

	      if (decl == NULL)
		decl = target_die;
	      CHECKSUM_ULEB128 ('N');
	      CHECKSUM_ULEB128 (at->dw_attr);
	      if (decl->die_parent != NULL)
		checksum_die_context (decl->die_parent, ctx);
	      CHECKSUM_ULEB128 ('E');
	      CHECKSUM_STRING (AT_string (name_attr));
	      return;
	    }
	}

      /* For all other references to another DIE, we check to see if the
         target DIE has already been visited.  If it has, we emit a
         backward reference; if not, we descend recursively.  */
      if (target_die->die_mark > 0)
        {
	  CHECKSUM_ULEB128 ('R');
	  CHECKSUM_ULEB128 (at->dw_attr);
	  CHECKSUM_ULEB128 (target_die->die_mark);
        }
      else
        {
	  dw_die_ref decl = get_AT_ref (target_die, DW_AT_specification);

	  if (decl == NULL)
	    decl = target_die;
	  target_die->die_mark = ++(*mark);
	  CHECKSUM_ULEB128 ('T');
	  CHECKSUM_ULEB128 (at->dw_attr);
	  if (decl->die_parent != NULL)
	    checksum_die_context (decl->die_parent, ctx);
	  die_checksum_ordered (target_die, ctx, mark);
        }
      return;
    }

  CHECKSUM_ULEB128 ('A');
  CHECKSUM_ULEB128 (at->dw_attr);

  switch (AT_class (at))
    {
    case dw_val_class_const:
    case dw_val_class_const_implicit:
      CHECKSUM_ULEB128 (DW_FORM_sdata);
      CHECKSUM_SLEB128 (at->dw_attr_val.v.val_int);
      break;

    case dw_val_class_unsigned_const:
    case dw_val_class_unsigned_const_implicit:
      CHECKSUM_ULEB128 (DW_FORM_sdata);
      CHECKSUM_SLEB128 ((int) at->dw_attr_val.v.val_unsigned);
      break;

    case dw_val_class_const_double:
      CHECKSUM_ULEB128 (DW_FORM_block);
      CHECKSUM_ULEB128 (sizeof (at->dw_attr_val.v.val_double));
      CHECKSUM (at->dw_attr_val.v.val_double);
      break;

    case dw_val_class_wide_int:
      CHECKSUM_ULEB128 (DW_FORM_block);
      CHECKSUM_ULEB128 (get_full_len (*at->dw_attr_val.v.val_wide)
			* HOST_BITS_PER_WIDE_INT / BITS_PER_UNIT);
      CHECKSUM_BLOCK (at->dw_attr_val.v.val_wide->get_val (),
		      get_full_len (*at->dw_attr_val.v.val_wide)
		      * HOST_BITS_PER_WIDE_INT / HOST_BITS_PER_CHAR);
      break;

    case dw_val_class_vec:
      CHECKSUM_ULEB128 (DW_FORM_block);
      CHECKSUM_ULEB128 (at->dw_attr_val.v.val_vec.length
			* at->dw_attr_val.v.val_vec.elt_size);
      CHECKSUM_BLOCK (at->dw_attr_val.v.val_vec.array,
		      (at->dw_attr_val.v.val_vec.length
		       * at->dw_attr_val.v.val_vec.elt_size));
      break;

    case dw_val_class_flag:
      CHECKSUM_ULEB128 (DW_FORM_flag);
      CHECKSUM_ULEB128 (at->dw_attr_val.v.val_flag ? 1 : 0);
      break;

    case dw_val_class_str:
      CHECKSUM_ULEB128 (DW_FORM_string);
      CHECKSUM_STRING (AT_string (at));
      break;

    case dw_val_class_addr:
      r = AT_addr (at);
      gcc_assert (GET_CODE (r) == SYMBOL_REF);
      CHECKSUM_ULEB128 (DW_FORM_string);
      CHECKSUM_STRING (XSTR (r, 0));
      break;

    case dw_val_class_offset:
      CHECKSUM_ULEB128 (DW_FORM_sdata);
      CHECKSUM_ULEB128 (at->dw_attr_val.v.val_offset);
      break;

    case dw_val_class_loc:
      for (loc = AT_loc (at); loc; loc = loc->dw_loc_next)
	loc_checksum_ordered (loc, ctx);
      break;

    case dw_val_class_fde_ref:
    case dw_val_class_lbl_id:
    case dw_val_class_lineptr:
    case dw_val_class_macptr:
    case dw_val_class_loclistsptr:
    case dw_val_class_high_pc:
      break;

    case dw_val_class_file:
    case dw_val_class_file_implicit:
      CHECKSUM_ULEB128 (DW_FORM_string);
      CHECKSUM_STRING (AT_file (at)->filename);
      break;

    case dw_val_class_data8:
      CHECKSUM (at->dw_attr_val.v.val_data8);
      break;

    default:
      break;
    }
}

struct checksum_attributes
{
  dw_attr_node *at_name;
  dw_attr_node *at_type;
  dw_attr_node *at_friend;
  dw_attr_node *at_accessibility;
  dw_attr_node *at_address_class;
  dw_attr_node *at_alignment;
  dw_attr_node *at_allocated;
  dw_attr_node *at_artificial;
  dw_attr_node *at_associated;
  dw_attr_node *at_binary_scale;
  dw_attr_node *at_bit_offset;
  dw_attr_node *at_bit_size;
  dw_attr_node *at_bit_stride;
  dw_attr_node *at_byte_size;
  dw_attr_node *at_byte_stride;
  dw_attr_node *at_const_value;
  dw_attr_node *at_containing_type;
  dw_attr_node *at_count;
  dw_attr_node *at_data_location;
  dw_attr_node *at_data_member_location;
  dw_attr_node *at_decimal_scale;
  dw_attr_node *at_decimal_sign;
  dw_attr_node *at_default_value;
  dw_attr_node *at_digit_count;
  dw_attr_node *at_discr;
  dw_attr_node *at_discr_list;
  dw_attr_node *at_discr_value;
  dw_attr_node *at_encoding;
  dw_attr_node *at_endianity;
  dw_attr_node *at_explicit;
  dw_attr_node *at_is_optional;
  dw_attr_node *at_location;
  dw_attr_node *at_lower_bound;
  dw_attr_node *at_mutable;
  dw_attr_node *at_ordering;
  dw_attr_node *at_picture_string;
  dw_attr_node *at_prototyped;
  dw_attr_node *at_small;
  dw_attr_node *at_segment;
  dw_attr_node *at_string_length;
  dw_attr_node *at_string_length_bit_size;
  dw_attr_node *at_string_length_byte_size;
  dw_attr_node *at_threads_scaled;
  dw_attr_node *at_upper_bound;
  dw_attr_node *at_use_location;
  dw_attr_node *at_use_UTF8;
  dw_attr_node *at_variable_parameter;
  dw_attr_node *at_virtuality;
  dw_attr_node *at_visibility;
  dw_attr_node *at_vtable_elem_location;
};

/* Collect the attributes that we will want to use for the checksum.  */

static void
collect_checksum_attributes (struct checksum_attributes *attrs, dw_die_ref die)
{
  dw_attr_node *a;
  unsigned ix;

  FOR_EACH_VEC_SAFE_ELT (die->die_attr, ix, a)
    {
      switch (a->dw_attr)
        {
        case DW_AT_name:
          attrs->at_name = a;
          break;
        case DW_AT_type:
          attrs->at_type = a;
          break;
        case DW_AT_friend:
          attrs->at_friend = a;
          break;
        case DW_AT_accessibility:
          attrs->at_accessibility = a;
          break;
        case DW_AT_address_class:
          attrs->at_address_class = a;
          break;
	case DW_AT_alignment:
	  attrs->at_alignment = a;
	  break;
        case DW_AT_allocated:
          attrs->at_allocated = a;
          break;
        case DW_AT_artificial:
          attrs->at_artificial = a;
          break;
        case DW_AT_associated:
          attrs->at_associated = a;
          break;
        case DW_AT_binary_scale:
          attrs->at_binary_scale = a;
          break;
        case DW_AT_bit_offset:
          attrs->at_bit_offset = a;
          break;
        case DW_AT_bit_size:
          attrs->at_bit_size = a;
          break;
        case DW_AT_bit_stride:
          attrs->at_bit_stride = a;
          break;
        case DW_AT_byte_size:
          attrs->at_byte_size = a;
          break;
        case DW_AT_byte_stride:
          attrs->at_byte_stride = a;
          break;
        case DW_AT_const_value:
          attrs->at_const_value = a;
          break;
        case DW_AT_containing_type:
          attrs->at_containing_type = a;
          break;
        case DW_AT_count:
          attrs->at_count = a;
          break;
        case DW_AT_data_location:
          attrs->at_data_location = a;
          break;
        case DW_AT_data_member_location:
          attrs->at_data_member_location = a;
          break;
        case DW_AT_decimal_scale:
          attrs->at_decimal_scale = a;
          break;
        case DW_AT_decimal_sign:
          attrs->at_decimal_sign = a;
          break;
        case DW_AT_default_value:
          attrs->at_default_value = a;
          break;
        case DW_AT_digit_count:
          attrs->at_digit_count = a;
          break;
        case DW_AT_discr:
          attrs->at_discr = a;
          break;
        case DW_AT_discr_list:
          attrs->at_discr_list = a;
          break;
        case DW_AT_discr_value:
          attrs->at_discr_value = a;
          break;
        case DW_AT_encoding:
          attrs->at_encoding = a;
          break;
        case DW_AT_endianity:
          attrs->at_endianity = a;
          break;
        case DW_AT_explicit:
          attrs->at_explicit = a;
          break;
        case DW_AT_is_optional:
          attrs->at_is_optional = a;
          break;
        case DW_AT_location:
          attrs->at_location = a;
          break;
        case DW_AT_lower_bound:
          attrs->at_lower_bound = a;
          break;
        case DW_AT_mutable:
          attrs->at_mutable = a;
          break;
        case DW_AT_ordering:
          attrs->at_ordering = a;
          break;
        case DW_AT_picture_string:
          attrs->at_picture_string = a;
          break;
        case DW_AT_prototyped:
          attrs->at_prototyped = a;
          break;
        case DW_AT_small:
          attrs->at_small = a;
          break;
        case DW_AT_segment:
          attrs->at_segment = a;
          break;
        case DW_AT_string_length:
          attrs->at_string_length = a;
          break;
	case DW_AT_string_length_bit_size:
	  attrs->at_string_length_bit_size = a;
	  break;
	case DW_AT_string_length_byte_size:
	  attrs->at_string_length_byte_size = a;
	  break;
        case DW_AT_threads_scaled:
          attrs->at_threads_scaled = a;
          break;
        case DW_AT_upper_bound:
          attrs->at_upper_bound = a;
          break;
        case DW_AT_use_location:
          attrs->at_use_location = a;
          break;
        case DW_AT_use_UTF8:
          attrs->at_use_UTF8 = a;
          break;
        case DW_AT_variable_parameter:
          attrs->at_variable_parameter = a;
          break;
        case DW_AT_virtuality:
          attrs->at_virtuality = a;
          break;
        case DW_AT_visibility:
          attrs->at_visibility = a;
          break;
        case DW_AT_vtable_elem_location:
          attrs->at_vtable_elem_location = a;
          break;
        default:
          break;
        }
    }
}

/* Calculate the checksum of a DIE, using an ordered subset of attributes.  */

static void
die_checksum_ordered (dw_die_ref die, struct md5_ctx *ctx, int *mark)
{
  dw_die_ref c;
  dw_die_ref decl;
  struct checksum_attributes attrs;

  CHECKSUM_ULEB128 ('D');
  CHECKSUM_ULEB128 (die->die_tag);

  memset (&attrs, 0, sizeof (attrs));

  decl = get_AT_ref (die, DW_AT_specification);
  if (decl != NULL)
    collect_checksum_attributes (&attrs, decl);
  collect_checksum_attributes (&attrs, die);

  CHECKSUM_ATTR (attrs.at_name);
  CHECKSUM_ATTR (attrs.at_accessibility);
  CHECKSUM_ATTR (attrs.at_address_class);
  CHECKSUM_ATTR (attrs.at_allocated);
  CHECKSUM_ATTR (attrs.at_artificial);
  CHECKSUM_ATTR (attrs.at_associated);
  CHECKSUM_ATTR (attrs.at_binary_scale);
  CHECKSUM_ATTR (attrs.at_bit_offset);
  CHECKSUM_ATTR (attrs.at_bit_size);
  CHECKSUM_ATTR (attrs.at_bit_stride);
  CHECKSUM_ATTR (attrs.at_byte_size);
  CHECKSUM_ATTR (attrs.at_byte_stride);
  CHECKSUM_ATTR (attrs.at_const_value);
  CHECKSUM_ATTR (attrs.at_containing_type);
  CHECKSUM_ATTR (attrs.at_count);
  CHECKSUM_ATTR (attrs.at_data_location);
  CHECKSUM_ATTR (attrs.at_data_member_location);
  CHECKSUM_ATTR (attrs.at_decimal_scale);
  CHECKSUM_ATTR (attrs.at_decimal_sign);
  CHECKSUM_ATTR (attrs.at_default_value);
  CHECKSUM_ATTR (attrs.at_digit_count);
  CHECKSUM_ATTR (attrs.at_discr);
  CHECKSUM_ATTR (attrs.at_discr_list);
  CHECKSUM_ATTR (attrs.at_discr_value);
  CHECKSUM_ATTR (attrs.at_encoding);
  CHECKSUM_ATTR (attrs.at_endianity);
  CHECKSUM_ATTR (attrs.at_explicit);
  CHECKSUM_ATTR (attrs.at_is_optional);
  CHECKSUM_ATTR (attrs.at_location);
  CHECKSUM_ATTR (attrs.at_lower_bound);
  CHECKSUM_ATTR (attrs.at_mutable);
  CHECKSUM_ATTR (attrs.at_ordering);
  CHECKSUM_ATTR (attrs.at_picture_string);
  CHECKSUM_ATTR (attrs.at_prototyped);
  CHECKSUM_ATTR (attrs.at_small);
  CHECKSUM_ATTR (attrs.at_segment);
  CHECKSUM_ATTR (attrs.at_string_length);
  CHECKSUM_ATTR (attrs.at_string_length_bit_size);
  CHECKSUM_ATTR (attrs.at_string_length_byte_size);
  CHECKSUM_ATTR (attrs.at_threads_scaled);
  CHECKSUM_ATTR (attrs.at_upper_bound);
  CHECKSUM_ATTR (attrs.at_use_location);
  CHECKSUM_ATTR (attrs.at_use_UTF8);
  CHECKSUM_ATTR (attrs.at_variable_parameter);
  CHECKSUM_ATTR (attrs.at_virtuality);
  CHECKSUM_ATTR (attrs.at_visibility);
  CHECKSUM_ATTR (attrs.at_vtable_elem_location);
  CHECKSUM_ATTR (attrs.at_type);
  CHECKSUM_ATTR (attrs.at_friend);
  CHECKSUM_ATTR (attrs.at_alignment);

  /* Checksum the child DIEs.  */
  c = die->die_child;
  if (c) do {
    dw_attr_node *name_attr;

    c = c->die_sib;
    name_attr = get_AT (c, DW_AT_name);
    if (is_template_instantiation (c))
      {
	/* Ignore instantiations of member type and function templates.  */
      }
    else if (name_attr != NULL
	     && (is_type_die (c) || c->die_tag == DW_TAG_subprogram))
      {
	/* Use a shallow checksum for named nested types and member
	   functions.  */
        CHECKSUM_ULEB128 ('S');
        CHECKSUM_ULEB128 (c->die_tag);
        CHECKSUM_STRING (AT_string (name_attr));
      }
    else
      {
	/* Use a deep checksum for other children.  */
        /* Mark this DIE so it gets processed when unmarking.  */
        if (c->die_mark == 0)
          c->die_mark = -1;
        die_checksum_ordered (c, ctx, mark);
      }
  } while (c != die->die_child);

  CHECKSUM_ULEB128 (0);
}

/* Add a type name and tag to a hash.  */
static void
die_odr_checksum (int tag, const char *name, md5_ctx *ctx)
{
  CHECKSUM_ULEB128 (tag);
  CHECKSUM_STRING (name);
}

#undef CHECKSUM
#undef CHECKSUM_STRING
#undef CHECKSUM_ATTR
#undef CHECKSUM_LEB128
#undef CHECKSUM_ULEB128

/* Generate the type signature for DIE.  This is computed by generating an
   MD5 checksum over the DIE's tag, its relevant attributes, and its
   children.  Attributes that are references to other DIEs are processed
   by recursion, using the MARK field to prevent infinite recursion.
   If the DIE is nested inside a namespace or another type, we also
   need to include that context in the signature.  The lower 64 bits
   of the resulting MD5 checksum comprise the signature.  */

static void
generate_type_signature (dw_die_ref die, comdat_type_node *type_node)
{
  int mark;
  const char *name;
  unsigned char checksum[16];
  struct md5_ctx ctx;
  dw_die_ref decl;
  dw_die_ref parent;

  name = get_AT_string (die, DW_AT_name);
  decl = get_AT_ref (die, DW_AT_specification);
  parent = get_die_parent (die);

  /* First, compute a signature for just the type name (and its surrounding
     context, if any.  This is stored in the type unit DIE for link-time
     ODR (one-definition rule) checking.  */

  if (is_cxx () && name != NULL)
    {
      md5_init_ctx (&ctx);

      /* Checksum the names of surrounding namespaces and structures.  */
      if (parent != NULL)
        checksum_die_context (parent, &ctx);

      /* Checksum the current DIE. */
      die_odr_checksum (die->die_tag, name, &ctx);
      md5_finish_ctx (&ctx, checksum);

      add_AT_data8 (type_node->root_die, DW_AT_GNU_odr_signature, &checksum[8]);
    }

  /* Next, compute the complete type signature.  */

  md5_init_ctx (&ctx);
  mark = 1;
  die->die_mark = mark;

  /* Checksum the names of surrounding namespaces and structures.  */
  if (parent != NULL)
    checksum_die_context (parent, &ctx);

  /* Checksum the DIE and its children.  */
  die_checksum_ordered (die, &ctx, &mark);
  unmark_all_dies (die);
  md5_finish_ctx (&ctx, checksum);

  /* Store the signature in the type node and link the type DIE and the
     type node together.  */
  memcpy (type_node->signature, &checksum[16 - DWARF_TYPE_SIGNATURE_SIZE],
          DWARF_TYPE_SIGNATURE_SIZE);
  die->comdat_type_p = true;
  die->die_id.die_type_node = type_node;
  type_node->type_die = die;

  /* If the DIE is a specification, link its declaration to the type node
     as well.  */
  if (decl != NULL)
    {
      decl->comdat_type_p = true;
      decl->die_id.die_type_node = type_node;
    }
}

/* Do the location expressions look same?  */
static inline int
same_loc_p (dw_loc_descr_ref loc1, dw_loc_descr_ref loc2, int *mark)
{
  return loc1->dw_loc_opc == loc2->dw_loc_opc
	 && same_dw_val_p (&loc1->dw_loc_oprnd1, &loc2->dw_loc_oprnd1, mark)
	 && same_dw_val_p (&loc1->dw_loc_oprnd2, &loc2->dw_loc_oprnd2, mark);
}

/* Do the values look the same?  */
static int
same_dw_val_p (const dw_val_node *v1, const dw_val_node *v2, int *mark)
{
  dw_loc_descr_ref loc1, loc2;
  rtx r1, r2;

  if (v1->val_class != v2->val_class)
    return 0;

  switch (v1->val_class)
    {
    case dw_val_class_const:
    case dw_val_class_const_implicit:
      return v1->v.val_int == v2->v.val_int;
    case dw_val_class_unsigned_const:
    case dw_val_class_unsigned_const_implicit:
      return v1->v.val_unsigned == v2->v.val_unsigned;
    case dw_val_class_const_double:
      return v1->v.val_double.high == v2->v.val_double.high
	     && v1->v.val_double.low == v2->v.val_double.low;
    case dw_val_class_wide_int:
      return *v1->v.val_wide == *v2->v.val_wide;
    case dw_val_class_vec:
      if (v1->v.val_vec.length != v2->v.val_vec.length
	  || v1->v.val_vec.elt_size != v2->v.val_vec.elt_size)
	return 0;
      if (memcmp (v1->v.val_vec.array, v2->v.val_vec.array,
		  v1->v.val_vec.length * v1->v.val_vec.elt_size))
	return 0;
      return 1;
    case dw_val_class_flag:
      return v1->v.val_flag == v2->v.val_flag;
    case dw_val_class_str:
      return !strcmp (v1->v.val_str->str, v2->v.val_str->str);

    case dw_val_class_addr:
      r1 = v1->v.val_addr;
      r2 = v2->v.val_addr;
      if (GET_CODE (r1) != GET_CODE (r2))
	return 0;
      return !rtx_equal_p (r1, r2);

    case dw_val_class_offset:
      return v1->v.val_offset == v2->v.val_offset;

    case dw_val_class_loc:
      for (loc1 = v1->v.val_loc, loc2 = v2->v.val_loc;
	   loc1 && loc2;
	   loc1 = loc1->dw_loc_next, loc2 = loc2->dw_loc_next)
	if (!same_loc_p (loc1, loc2, mark))
	  return 0;
      return !loc1 && !loc2;

    case dw_val_class_die_ref:
      return same_die_p (v1->v.val_die_ref.die, v2->v.val_die_ref.die, mark);

    case dw_val_class_fde_ref:
    case dw_val_class_vms_delta:
    case dw_val_class_lbl_id:
    case dw_val_class_lineptr:
    case dw_val_class_macptr:
    case dw_val_class_loclistsptr:
    case dw_val_class_high_pc:
      return 1;

    case dw_val_class_file:
    case dw_val_class_file_implicit:
      return v1->v.val_file == v2->v.val_file;

    case dw_val_class_data8:
      return !memcmp (v1->v.val_data8, v2->v.val_data8, 8);

    default:
      return 1;
    }
}

/* Do the attributes look the same?  */

static int
same_attr_p (dw_attr_node *at1, dw_attr_node *at2, int *mark)
{
  if (at1->dw_attr != at2->dw_attr)
    return 0;

  /* We don't care that this was compiled with a different compiler
     snapshot; if the output is the same, that's what matters. */
  if (at1->dw_attr == DW_AT_producer)
    return 1;

  return same_dw_val_p (&at1->dw_attr_val, &at2->dw_attr_val, mark);
}

/* Do the dies look the same?  */

static int
same_die_p (dw_die_ref die1, dw_die_ref die2, int *mark)
{
  dw_die_ref c1, c2;
  dw_attr_node *a1;
  unsigned ix;

  /* To avoid infinite recursion.  */
  if (die1->die_mark)
    return die1->die_mark == die2->die_mark;
  die1->die_mark = die2->die_mark = ++(*mark);

  if (die1->die_tag != die2->die_tag)
    return 0;

  if (vec_safe_length (die1->die_attr) != vec_safe_length (die2->die_attr))
    return 0;

  FOR_EACH_VEC_SAFE_ELT (die1->die_attr, ix, a1)
    if (!same_attr_p (a1, &(*die2->die_attr)[ix], mark))
      return 0;

  c1 = die1->die_child;
  c2 = die2->die_child;
  if (! c1)
    {
      if (c2)
	return 0;
    }
  else
    for (;;)
      {
	if (!same_die_p (c1, c2, mark))
	  return 0;
	c1 = c1->die_sib;
	c2 = c2->die_sib;
	if (c1 == die1->die_child)
	  {
	    if (c2 == die2->die_child)
	      break;
	    else
	      return 0;
	  }
    }

  return 1;
}

/* Calculate the MD5 checksum of the compilation unit DIE UNIT_DIE and its
   children, and set die_symbol.  */

static void
compute_comp_unit_symbol (dw_die_ref unit_die)
{
  const char *die_name = get_AT_string (unit_die, DW_AT_name);
  const char *base = die_name ? lbasename (die_name) : "anonymous";
  char *name = XALLOCAVEC (char, strlen (base) + 64);
  char *p;
  int i, mark;
  unsigned char checksum[16];
  struct md5_ctx ctx;

  /* Compute the checksum of the DIE, then append part of it as hex digits to
     the name filename of the unit.  */

  md5_init_ctx (&ctx);
  mark = 0;
  die_checksum (unit_die, &ctx, &mark);
  unmark_all_dies (unit_die);
  md5_finish_ctx (&ctx, checksum);

  /* When we this for comp_unit_die () we have a DW_AT_name that might
     not start with a letter but with anything valid for filenames and
     clean_symbol_name doesn't fix that up.  Prepend 'g' if the first
     character is not a letter.  */
  sprintf (name, "%s%s.", ISALPHA (*base) ? "" : "g", base);
  clean_symbol_name (name);

  p = name + strlen (name);
  for (i = 0; i < 4; i++)
    {
      sprintf (p, "%.2x", checksum[i]);
      p += 2;
    }

  unit_die->die_id.die_symbol = xstrdup (name);
}

/* Returns nonzero if DIE represents a type, in the sense of TYPE_P.  */

static int
is_type_die (dw_die_ref die)
{
  switch (die->die_tag)
    {
    case DW_TAG_array_type:
    case DW_TAG_class_type:
    case DW_TAG_interface_type:
    case DW_TAG_enumeration_type:
    case DW_TAG_pointer_type:
    case DW_TAG_reference_type:
    case DW_TAG_rvalue_reference_type:
    case DW_TAG_string_type:
    case DW_TAG_structure_type:
    case DW_TAG_subroutine_type:
    case DW_TAG_union_type:
    case DW_TAG_ptr_to_member_type:
    case DW_TAG_set_type:
    case DW_TAG_subrange_type:
    case DW_TAG_base_type:
    case DW_TAG_const_type:
    case DW_TAG_file_type:
    case DW_TAG_packed_type:
    case DW_TAG_volatile_type:
    case DW_TAG_typedef:
      return 1;
    default:
      return 0;
    }
}

/* Returns 1 iff C is the sort of DIE that should go into a COMDAT CU.
   Basically, we want to choose the bits that are likely to be shared between
   compilations (types) and leave out the bits that are specific to individual
   compilations (functions).  */

static int
is_comdat_die (dw_die_ref c)
{
  /* I think we want to leave base types and __vtbl_ptr_type in the main CU, as
     we do for stabs.  The advantage is a greater likelihood of sharing between
     objects that don't include headers in the same order (and therefore would
     put the base types in a different comdat).  jason 8/28/00 */

  if (c->die_tag == DW_TAG_base_type)
    return 0;

  if (c->die_tag == DW_TAG_pointer_type
      || c->die_tag == DW_TAG_reference_type
      || c->die_tag == DW_TAG_rvalue_reference_type
      || c->die_tag == DW_TAG_const_type
      || c->die_tag == DW_TAG_volatile_type)
    {
      dw_die_ref t = get_AT_ref (c, DW_AT_type);

      return t ? is_comdat_die (t) : 0;
    }

  return is_type_die (c);
}

/* Returns true iff C is a compile-unit DIE.  */

static inline bool
is_cu_die (dw_die_ref c)
{
  return c && (c->die_tag == DW_TAG_compile_unit
	       || c->die_tag == DW_TAG_skeleton_unit);
}

/* Returns true iff C is a unit DIE of some sort.  */

static inline bool
is_unit_die (dw_die_ref c)
{
  return c && (c->die_tag == DW_TAG_compile_unit
	       || c->die_tag == DW_TAG_partial_unit
	       || c->die_tag == DW_TAG_type_unit
	       || c->die_tag == DW_TAG_skeleton_unit);
}

/* Returns true iff C is a namespace DIE.  */

static inline bool
is_namespace_die (dw_die_ref c)
{
  return c && c->die_tag == DW_TAG_namespace;
}

/* Returns true iff C is a class or structure DIE.  */

static inline bool
is_class_die (dw_die_ref c)
{
  return c && (c->die_tag == DW_TAG_class_type
               || c->die_tag == DW_TAG_structure_type);
}

/* Return non-zero if this DIE is a template parameter.  */

static inline bool
is_template_parameter (dw_die_ref die)
{
  switch (die->die_tag)
    {
    case DW_TAG_template_type_param:
    case DW_TAG_template_value_param:
    case DW_TAG_GNU_template_template_param:
    case DW_TAG_GNU_template_parameter_pack:
      return true;
    default:
      return false;
    }
}

/* Return non-zero if this DIE represents a template instantiation.  */

static inline bool
is_template_instantiation (dw_die_ref die)
{
  dw_die_ref c;

  if (!is_type_die (die) && die->die_tag != DW_TAG_subprogram)
    return false;
  FOR_EACH_CHILD (die, c, if (is_template_parameter (c)) return true);
  return false;
}

static char *
gen_internal_sym (const char *prefix)
{
  char buf[MAX_ARTIFICIAL_LABEL_BYTES];

  ASM_GENERATE_INTERNAL_LABEL (buf, prefix, label_num++);
  return xstrdup (buf);
}

/* Return non-zero if this DIE is a declaration.  */

static int
is_declaration_die (dw_die_ref die)
{
  dw_attr_node *a;
  unsigned ix;

  FOR_EACH_VEC_SAFE_ELT (die->die_attr, ix, a)
    if (a->dw_attr == DW_AT_declaration)
      return 1;

  return 0;
}

/* Return non-zero if this DIE is nested inside a subprogram.  */

static int
is_nested_in_subprogram (dw_die_ref die)
{
  dw_die_ref decl = get_AT_ref (die, DW_AT_specification);

  if (decl == NULL)
    decl = die;
  return local_scope_p (decl);
}

/* Return non-zero if this DIE contains a defining declaration of a
   subprogram.  */

static int
contains_subprogram_definition (dw_die_ref die)
{
  dw_die_ref c;

  if (die->die_tag == DW_TAG_subprogram && ! is_declaration_die (die))
    return 1;
  FOR_EACH_CHILD (die, c, if (contains_subprogram_definition (c)) return 1);
  return 0;
}

/* Return non-zero if this is a type DIE that should be moved to a
   COMDAT .debug_types section or .debug_info section with DW_UT_*type
   unit type.  */

static int
should_move_die_to_comdat (dw_die_ref die)
{
  switch (die->die_tag)
    {
    case DW_TAG_class_type:
    case DW_TAG_structure_type:
    case DW_TAG_enumeration_type:
    case DW_TAG_union_type:
      /* Don't move declarations, inlined instances, types nested in a
	 subprogram, or types that contain subprogram definitions.  */
      if (is_declaration_die (die)
          || get_AT (die, DW_AT_abstract_origin)
          || is_nested_in_subprogram (die)
          || contains_subprogram_definition (die))
        return 0;
      return 1;
    case DW_TAG_array_type:
    case DW_TAG_interface_type:
    case DW_TAG_pointer_type:
    case DW_TAG_reference_type:
    case DW_TAG_rvalue_reference_type:
    case DW_TAG_string_type:
    case DW_TAG_subroutine_type:
    case DW_TAG_ptr_to_member_type:
    case DW_TAG_set_type:
    case DW_TAG_subrange_type:
    case DW_TAG_base_type:
    case DW_TAG_const_type:
    case DW_TAG_file_type:
    case DW_TAG_packed_type:
    case DW_TAG_volatile_type:
    case DW_TAG_typedef:
    default:
      return 0;
    }
}

/* Make a clone of DIE.  */

static dw_die_ref
clone_die (dw_die_ref die)
{
  dw_die_ref clone;
  dw_attr_node *a;
  unsigned ix;

  clone = ggc_cleared_alloc<die_node> ();
  clone->die_tag = die->die_tag;

  FOR_EACH_VEC_SAFE_ELT (die->die_attr, ix, a)
    add_dwarf_attr (clone, a);

  return clone;
}

/* Make a clone of the tree rooted at DIE.  */

static dw_die_ref
clone_tree (dw_die_ref die)
{
  dw_die_ref c;
  dw_die_ref clone = clone_die (die);

  FOR_EACH_CHILD (die, c, add_child_die (clone, clone_tree (c)));

  return clone;
}

/* Make a clone of DIE as a declaration.  */

static dw_die_ref
clone_as_declaration (dw_die_ref die)
{
  dw_die_ref clone;
  dw_die_ref decl;
  dw_attr_node *a;
  unsigned ix;

  /* If the DIE is already a declaration, just clone it.  */
  if (is_declaration_die (die))
    return clone_die (die);

  /* If the DIE is a specification, just clone its declaration DIE.  */
  decl = get_AT_ref (die, DW_AT_specification);
  if (decl != NULL)
    {
      clone = clone_die (decl);
      if (die->comdat_type_p)
	add_AT_die_ref (clone, DW_AT_signature, die);
      return clone;
    }

  clone = ggc_cleared_alloc<die_node> ();
  clone->die_tag = die->die_tag;

  FOR_EACH_VEC_SAFE_ELT (die->die_attr, ix, a)
    {
      /* We don't want to copy over all attributes.
         For example we don't want DW_AT_byte_size because otherwise we will no
         longer have a declaration and GDB will treat it as a definition.  */

      switch (a->dw_attr)
        {
        case DW_AT_abstract_origin:
        case DW_AT_artificial:
        case DW_AT_containing_type:
        case DW_AT_external:
        case DW_AT_name:
        case DW_AT_type:
        case DW_AT_virtuality:
        case DW_AT_linkage_name:
        case DW_AT_MIPS_linkage_name:
          add_dwarf_attr (clone, a);
          break;
        case DW_AT_byte_size:
	case DW_AT_alignment:
        default:
          break;
        }
    }

  if (die->comdat_type_p)
    add_AT_die_ref (clone, DW_AT_signature, die);

  add_AT_flag (clone, DW_AT_declaration, 1);
  return clone;
}


/* Structure to map a DIE in one CU to its copy in a comdat type unit.  */

struct decl_table_entry
{
  dw_die_ref orig;
  dw_die_ref copy;
};

/* Helpers to manipulate hash table of copied declarations.  */

/* Hashtable helpers.  */

struct decl_table_entry_hasher : free_ptr_hash <decl_table_entry>
{
  typedef die_struct *compare_type;
  static inline hashval_t hash (const decl_table_entry *);
  static inline bool equal (const decl_table_entry *, const die_struct *);
};

inline hashval_t
decl_table_entry_hasher::hash (const decl_table_entry *entry)
{
  return htab_hash_pointer (entry->orig);
}

inline bool
decl_table_entry_hasher::equal (const decl_table_entry *entry1,
				const die_struct *entry2)
{
  return entry1->orig == entry2;
}

typedef hash_table<decl_table_entry_hasher> decl_hash_type;

/* Copy DIE and its ancestors, up to, but not including, the compile unit
   or type unit entry, to a new tree.  Adds the new tree to UNIT and returns
   a pointer to the copy of DIE.  If DECL_TABLE is provided, it is used
   to check if the ancestor has already been copied into UNIT.  */

static dw_die_ref
copy_ancestor_tree (dw_die_ref unit, dw_die_ref die,
		    decl_hash_type *decl_table)
{
  dw_die_ref parent = die->die_parent;
  dw_die_ref new_parent = unit;
  dw_die_ref copy;
  decl_table_entry **slot = NULL;
  struct decl_table_entry *entry = NULL;

  if (decl_table)
    {
      /* Check if the entry has already been copied to UNIT.  */
      slot = decl_table->find_slot_with_hash (die, htab_hash_pointer (die),
					      INSERT);
      if (*slot != HTAB_EMPTY_ENTRY)
        {
          entry = *slot;
          return entry->copy;
        }

      /* Record in DECL_TABLE that DIE has been copied to UNIT.  */
      entry = XCNEW (struct decl_table_entry);
      entry->orig = die;
      entry->copy = NULL;
      *slot = entry;
    }

  if (parent != NULL)
    {
      dw_die_ref spec = get_AT_ref (parent, DW_AT_specification);
      if (spec != NULL)
        parent = spec;
      if (!is_unit_die (parent))
        new_parent = copy_ancestor_tree (unit, parent, decl_table);
    }

  copy = clone_as_declaration (die);
  add_child_die (new_parent, copy);

  if (decl_table)
    {
      /* Record the pointer to the copy.  */
      entry->copy = copy;
    }

  return copy;
}
/* Copy the declaration context to the new type unit DIE.  This includes
   any surrounding namespace or type declarations.  If the DIE has an
   AT_specification attribute, it also includes attributes and children
   attached to the specification, and returns a pointer to the original
   parent of the declaration DIE.  Returns NULL otherwise.  */

static dw_die_ref
copy_declaration_context (dw_die_ref unit, dw_die_ref die)
{
  dw_die_ref decl;
  dw_die_ref new_decl;
  dw_die_ref orig_parent = NULL;

  decl = get_AT_ref (die, DW_AT_specification);
  if (decl == NULL)
    decl = die;
  else
    {
      unsigned ix;
      dw_die_ref c;
      dw_attr_node *a;

      /* The original DIE will be changed to a declaration, and must
         be moved to be a child of the original declaration DIE.  */
      orig_parent = decl->die_parent;

      /* Copy the type node pointer from the new DIE to the original
         declaration DIE so we can forward references later.  */
      decl->comdat_type_p = true;
      decl->die_id.die_type_node = die->die_id.die_type_node;

      remove_AT (die, DW_AT_specification);

      FOR_EACH_VEC_SAFE_ELT (decl->die_attr, ix, a)
        {
          if (a->dw_attr != DW_AT_name
              && a->dw_attr != DW_AT_declaration
              && a->dw_attr != DW_AT_external)
            add_dwarf_attr (die, a);
        }

      FOR_EACH_CHILD (decl, c, add_child_die (die, clone_tree (c)));
    }

  if (decl->die_parent != NULL
      && !is_unit_die (decl->die_parent))
    {
      new_decl = copy_ancestor_tree (unit, decl, NULL);
      if (new_decl != NULL)
        {
          remove_AT (new_decl, DW_AT_signature);
          add_AT_specification (die, new_decl);
        }
    }

  return orig_parent;
}

/* Generate the skeleton ancestor tree for the given NODE, then clone
   the DIE and add the clone into the tree.  */

static void
generate_skeleton_ancestor_tree (skeleton_chain_node *node)
{
  if (node->new_die != NULL)
    return;

  node->new_die = clone_as_declaration (node->old_die);

  if (node->parent != NULL)
    {
      generate_skeleton_ancestor_tree (node->parent);
      add_child_die (node->parent->new_die, node->new_die);
    }
}

/* Generate a skeleton tree of DIEs containing any declarations that are
   found in the original tree.  We traverse the tree looking for declaration
   DIEs, and construct the skeleton from the bottom up whenever we find one.  */

static void
generate_skeleton_bottom_up (skeleton_chain_node *parent)
{
  skeleton_chain_node node;
  dw_die_ref c;
  dw_die_ref first;
  dw_die_ref prev = NULL;
  dw_die_ref next = NULL;

  node.parent = parent;

  first = c = parent->old_die->die_child;
  if (c)
    next = c->die_sib;
  if (c) do {
    if (prev == NULL || prev->die_sib == c)
      prev = c;
    c = next;
    next = (c == first ? NULL : c->die_sib);
    node.old_die = c;
    node.new_die = NULL;
    if (is_declaration_die (c))
      {
	if (is_template_instantiation (c))
	  {
	    /* Instantiated templates do not need to be cloned into the
	       type unit.  Just move the DIE and its children back to
	       the skeleton tree (in the main CU).  */
	    remove_child_with_prev (c, prev);
	    add_child_die (parent->new_die, c);
	    c = prev;
	  }
	else if (c->comdat_type_p)
	  {
	    /* This is the skeleton of earlier break_out_comdat_types
	       type.  Clone the existing DIE, but keep the children
	       under the original (which is in the main CU).  */
	    dw_die_ref clone = clone_die (c);

	    replace_child (c, clone, prev);
	    generate_skeleton_ancestor_tree (parent);
	    add_child_die (parent->new_die, c);
	    c = clone;
	    continue;
	  }
	else
	  {
	    /* Clone the existing DIE, move the original to the skeleton
	       tree (which is in the main CU), and put the clone, with
	       all the original's children, where the original came from
	       (which is about to be moved to the type unit).  */
	    dw_die_ref clone = clone_die (c);
	    move_all_children (c, clone);

	    /* If the original has a DW_AT_object_pointer attribute,
	       it would now point to a child DIE just moved to the
	       cloned tree, so we need to remove that attribute from
	       the original.  */
	    remove_AT (c, DW_AT_object_pointer);

	    replace_child (c, clone, prev);
	    generate_skeleton_ancestor_tree (parent);
	    add_child_die (parent->new_die, c);
	    node.old_die = clone;
	    node.new_die = c;
	    c = clone;
	  }
      }
    generate_skeleton_bottom_up (&node);
  } while (next != NULL);
}

/* Wrapper function for generate_skeleton_bottom_up.  */

static dw_die_ref
generate_skeleton (dw_die_ref die)
{
  skeleton_chain_node node;

  node.old_die = die;
  node.new_die = NULL;
  node.parent = NULL;

  /* If this type definition is nested inside another type,
     and is not an instantiation of a template, always leave
     at least a declaration in its place.  */
  if (die->die_parent != NULL
      && is_type_die (die->die_parent)
      && !is_template_instantiation (die))
    node.new_die = clone_as_declaration (die);

  generate_skeleton_bottom_up (&node);
  return node.new_die;
}

/* Remove the CHILD DIE from its parent, possibly replacing it with a cloned
   declaration.  The original DIE is moved to a new compile unit so that
   existing references to it follow it to the new location.  If any of the
   original DIE's descendants is a declaration, we need to replace the
   original DIE with a skeleton tree and move the declarations back into the
   skeleton tree.  */

static dw_die_ref
remove_child_or_replace_with_skeleton (dw_die_ref unit, dw_die_ref child,
				       dw_die_ref prev)
{
  dw_die_ref skeleton, orig_parent;

  /* Copy the declaration context to the type unit DIE.  If the returned
     ORIG_PARENT is not NULL, the skeleton needs to be added as a child of
     that DIE.  */
  orig_parent = copy_declaration_context (unit, child);

  skeleton = generate_skeleton (child);
  if (skeleton == NULL)
    remove_child_with_prev (child, prev);
  else
    {
      skeleton->comdat_type_p = true;
      skeleton->die_id.die_type_node = child->die_id.die_type_node;

      /* If the original DIE was a specification, we need to put
         the skeleton under the parent DIE of the declaration.
	 This leaves the original declaration in the tree, but
	 it will be pruned later since there are no longer any
	 references to it.  */
      if (orig_parent != NULL)
	{
	  remove_child_with_prev (child, prev);
	  add_child_die (orig_parent, skeleton);
	}
      else
	replace_child (child, skeleton, prev);
    }

  return skeleton;
}

static void
copy_dwarf_procs_ref_in_attrs (dw_die_ref die,
			       comdat_type_node *type_node,
			       hash_map<dw_die_ref, dw_die_ref> &copied_dwarf_procs);

/* Helper for copy_dwarf_procs_ref_in_dies.  Make a copy of the DIE DWARF
   procedure, put it under TYPE_NODE and return the copy.  Continue looking for
   DWARF procedure references in the DW_AT_location attribute.  */

static dw_die_ref
copy_dwarf_procedure (dw_die_ref die,
		      comdat_type_node *type_node,
		      hash_map<dw_die_ref, dw_die_ref> &copied_dwarf_procs)
{
  gcc_assert (die->die_tag == DW_TAG_dwarf_procedure);

  /* DWARF procedures are not supposed to have children...  */
  gcc_assert (die->die_child == NULL);

  /* ... and they are supposed to have only one attribute: DW_AT_location.  */
  gcc_assert (vec_safe_length (die->die_attr) == 1
	      && ((*die->die_attr)[0].dw_attr == DW_AT_location));

  /* Do not copy more than once DWARF procedures.  */
  bool existed;
  dw_die_ref &die_copy = copied_dwarf_procs.get_or_insert (die, &existed);
  if (existed)
    return die_copy;

  die_copy = clone_die (die);
  add_child_die (type_node->root_die, die_copy);
  copy_dwarf_procs_ref_in_attrs (die_copy, type_node, copied_dwarf_procs);
  return die_copy;
}

/* Helper for copy_dwarf_procs_ref_in_dies.  Look for references to DWARF
   procedures in DIE's attributes.  */

static void
copy_dwarf_procs_ref_in_attrs (dw_die_ref die,
			       comdat_type_node *type_node,
			       hash_map<dw_die_ref, dw_die_ref> &copied_dwarf_procs)
{
  dw_attr_node *a;
  unsigned i;

  FOR_EACH_VEC_SAFE_ELT (die->die_attr, i, a)
    {
      dw_loc_descr_ref loc;

      if (a->dw_attr_val.val_class != dw_val_class_loc)
	continue;

      for (loc = a->dw_attr_val.v.val_loc; loc != NULL; loc = loc->dw_loc_next)
	{
	  switch (loc->dw_loc_opc)
	    {
	    case DW_OP_call2:
	    case DW_OP_call4:
	    case DW_OP_call_ref:
	      gcc_assert (loc->dw_loc_oprnd1.val_class
			  == dw_val_class_die_ref);
	      loc->dw_loc_oprnd1.v.val_die_ref.die
	        = copy_dwarf_procedure (loc->dw_loc_oprnd1.v.val_die_ref.die,
					type_node,
					copied_dwarf_procs);

	    default:
	      break;
	    }
	}
    }
}

/* Copy DWARF procedures that are referenced by the DIE tree to TREE_NODE and
   rewrite references to point to the copies.

   References are looked for in DIE's attributes and recursively in all its
   children attributes that are location descriptions. COPIED_DWARF_PROCS is a
   mapping from old DWARF procedures to their copy. It is used not to copy
   twice the same DWARF procedure under TYPE_NODE.  */

static void
copy_dwarf_procs_ref_in_dies (dw_die_ref die,
			      comdat_type_node *type_node,
			      hash_map<dw_die_ref, dw_die_ref> &copied_dwarf_procs)
{
  dw_die_ref c;

  copy_dwarf_procs_ref_in_attrs (die, type_node, copied_dwarf_procs);
  FOR_EACH_CHILD (die, c, copy_dwarf_procs_ref_in_dies (c,
							type_node,
							copied_dwarf_procs));
}

/* Traverse the DIE and set up additional .debug_types or .debug_info
   DW_UT_*type sections for each type worthy of being placed in a COMDAT
   section.  */

static void
break_out_comdat_types (dw_die_ref die)
{
  dw_die_ref c;
  dw_die_ref first;
  dw_die_ref prev = NULL;
  dw_die_ref next = NULL;
  dw_die_ref unit = NULL;

  first = c = die->die_child;
  if (c)
    next = c->die_sib;
  if (c) do {
    if (prev == NULL || prev->die_sib == c)
      prev = c;
    c = next;
    next = (c == first ? NULL : c->die_sib);
    if (should_move_die_to_comdat (c))
      {
        dw_die_ref replacement;
	comdat_type_node *type_node;

        /* Break out nested types into their own type units.  */
        break_out_comdat_types (c);

        /* Create a new type unit DIE as the root for the new tree, and
           add it to the list of comdat types.  */
        unit = new_die (DW_TAG_type_unit, NULL, NULL);
        add_AT_unsigned (unit, DW_AT_language,
                         get_AT_unsigned (comp_unit_die (), DW_AT_language));
        type_node = ggc_cleared_alloc<comdat_type_node> ();
        type_node->root_die = unit;
        type_node->next = comdat_type_list;
        comdat_type_list = type_node;

        /* Generate the type signature.  */
        generate_type_signature (c, type_node);

        /* Copy the declaration context, attributes, and children of the
           declaration into the new type unit DIE, then remove this DIE
	   from the main CU (or replace it with a skeleton if necessary).  */
	replacement = remove_child_or_replace_with_skeleton (unit, c, prev);
	type_node->skeleton_die = replacement;

        /* Add the DIE to the new compunit.  */
	add_child_die (unit, c);

	/* Types can reference DWARF procedures for type size or data location
	   expressions.  Calls in DWARF expressions cannot target procedures
	   that are not in the same section.  So we must copy DWARF procedures
	   along with this type and then rewrite references to them.  */
	hash_map<dw_die_ref, dw_die_ref> copied_dwarf_procs;
	copy_dwarf_procs_ref_in_dies (c, type_node, copied_dwarf_procs);

        if (replacement != NULL)
          c = replacement;
      }
    else if (c->die_tag == DW_TAG_namespace
             || c->die_tag == DW_TAG_class_type
             || c->die_tag == DW_TAG_structure_type
             || c->die_tag == DW_TAG_union_type)
      {
        /* Look for nested types that can be broken out.  */
        break_out_comdat_types (c);
      }
  } while (next != NULL);
}

/* Like clone_tree, but copy DW_TAG_subprogram DIEs as declarations.
   Enter all the cloned children into the hash table decl_table.  */

static dw_die_ref
clone_tree_partial (dw_die_ref die, decl_hash_type *decl_table)
{
  dw_die_ref c;
  dw_die_ref clone;
  struct decl_table_entry *entry;
  decl_table_entry **slot;

  if (die->die_tag == DW_TAG_subprogram)
    clone = clone_as_declaration (die);
  else
    clone = clone_die (die);

  slot = decl_table->find_slot_with_hash (die,
					  htab_hash_pointer (die), INSERT);

  /* Assert that DIE isn't in the hash table yet.  If it would be there
     before, the ancestors would be necessarily there as well, therefore
     clone_tree_partial wouldn't be called.  */
  gcc_assert (*slot == HTAB_EMPTY_ENTRY);

  entry = XCNEW (struct decl_table_entry);
  entry->orig = die;
  entry->copy = clone;
  *slot = entry;

  if (die->die_tag != DW_TAG_subprogram)
    FOR_EACH_CHILD (die, c,
		    add_child_die (clone, clone_tree_partial (c, decl_table)));

  return clone;
}

/* Walk the DIE and its children, looking for references to incomplete
   or trivial types that are unmarked (i.e., that are not in the current
   type_unit).  */

static void
copy_decls_walk (dw_die_ref unit, dw_die_ref die, decl_hash_type *decl_table)
{
  dw_die_ref c;
  dw_attr_node *a;
  unsigned ix;

  FOR_EACH_VEC_SAFE_ELT (die->die_attr, ix, a)
    {
      if (AT_class (a) == dw_val_class_die_ref)
        {
          dw_die_ref targ = AT_ref (a);
          decl_table_entry **slot;
          struct decl_table_entry *entry;

          if (targ->die_mark != 0 || targ->comdat_type_p)
            continue;

          slot = decl_table->find_slot_with_hash (targ,
						  htab_hash_pointer (targ),
						  INSERT);

          if (*slot != HTAB_EMPTY_ENTRY)
            {
              /* TARG has already been copied, so we just need to
                 modify the reference to point to the copy.  */
              entry = *slot;
              a->dw_attr_val.v.val_die_ref.die = entry->copy;
            }
          else
            {
              dw_die_ref parent = unit;
	      dw_die_ref copy = clone_die (targ);

              /* Record in DECL_TABLE that TARG has been copied.
                 Need to do this now, before the recursive call,
                 because DECL_TABLE may be expanded and SLOT
                 would no longer be a valid pointer.  */
              entry = XCNEW (struct decl_table_entry);
              entry->orig = targ;
              entry->copy = copy;
              *slot = entry;

	      /* If TARG is not a declaration DIE, we need to copy its
	         children.  */
	      if (!is_declaration_die (targ))
		{
		  FOR_EACH_CHILD (
		      targ, c,
		      add_child_die (copy,
				     clone_tree_partial (c, decl_table)));
		}

              /* Make sure the cloned tree is marked as part of the
                 type unit.  */
              mark_dies (copy);

              /* If TARG has surrounding context, copy its ancestor tree
                 into the new type unit.  */
              if (targ->die_parent != NULL
		  && !is_unit_die (targ->die_parent))
                parent = copy_ancestor_tree (unit, targ->die_parent,
                                             decl_table);

              add_child_die (parent, copy);
              a->dw_attr_val.v.val_die_ref.die = copy;

              /* Make sure the newly-copied DIE is walked.  If it was
                 installed in a previously-added context, it won't
                 get visited otherwise.  */
              if (parent != unit)
		{
		  /* Find the highest point of the newly-added tree,
		     mark each node along the way, and walk from there.  */
		  parent->die_mark = 1;
		  while (parent->die_parent
		  	 && parent->die_parent->die_mark == 0)
		    {
		      parent = parent->die_parent;
		      parent->die_mark = 1;
		    }
		  copy_decls_walk (unit, parent, decl_table);
		}
            }
        }
    }

  FOR_EACH_CHILD (die, c, copy_decls_walk (unit, c, decl_table));
}

/* Copy declarations for "unworthy" types into the new comdat section.
   Incomplete types, modified types, and certain other types aren't broken
   out into comdat sections of their own, so they don't have a signature,
   and we need to copy the declaration into the same section so that we
   don't have an external reference.  */

static void
copy_decls_for_unworthy_types (dw_die_ref unit)
{
  mark_dies (unit);
  decl_hash_type decl_table (10);
  copy_decls_walk (unit, unit, &decl_table);
  unmark_dies (unit);
}

/* Traverse the DIE and add a sibling attribute if it may have the
   effect of speeding up access to siblings.  To save some space,
   avoid generating sibling attributes for DIE's without children.  */

static void
add_sibling_attributes (dw_die_ref die)
{
  dw_die_ref c;

  if (! die->die_child)
    return;

  if (die->die_parent && die != die->die_parent->die_child)
    add_AT_die_ref (die, DW_AT_sibling, die->die_sib);

  FOR_EACH_CHILD (die, c, add_sibling_attributes (c));
}

/* Output all location lists for the DIE and its children.  */

static void
output_location_lists (dw_die_ref die)
{
  dw_die_ref c;
  dw_attr_node *a;
  unsigned ix;

  FOR_EACH_VEC_SAFE_ELT (die->die_attr, ix, a)
    if (AT_class (a) == dw_val_class_loc_list)
      output_loc_list (AT_loc_list (a));

  FOR_EACH_CHILD (die, c, output_location_lists (c));
}

/* During assign_location_list_indexes and output_loclists_offset the
   current index, after it the number of assigned indexes (i.e. how
   large the .debug_loclists* offset table should be).  */
static unsigned int loc_list_idx;

/* Output all location list offsets for the DIE and its children.  */

static void
output_loclists_offsets (dw_die_ref die)
{
  dw_die_ref c;
  dw_attr_node *a;
  unsigned ix;

  FOR_EACH_VEC_SAFE_ELT (die->die_attr, ix, a)
    if (AT_class (a) == dw_val_class_loc_list)
      {
	dw_loc_list_ref l = AT_loc_list (a);
	if (l->offset_emitted)
	  continue;
	dw2_asm_output_delta (DWARF_OFFSET_SIZE, l->ll_symbol,
			      loc_section_label, NULL);
	gcc_assert (l->hash == loc_list_idx);
	loc_list_idx++;
	l->offset_emitted = true;
      }

  FOR_EACH_CHILD (die, c, output_loclists_offsets (c));
}

/* Recursively set indexes of location lists.  */

static void
assign_location_list_indexes (dw_die_ref die)
{
  dw_die_ref c;
  dw_attr_node *a;
  unsigned ix;

  FOR_EACH_VEC_SAFE_ELT (die->die_attr, ix, a)
    if (AT_class (a) == dw_val_class_loc_list)
      {
	dw_loc_list_ref list = AT_loc_list (a);
	if (!list->num_assigned)
	  {
	    list->num_assigned = true;
	    list->hash = loc_list_idx++;
	  }
      }

  FOR_EACH_CHILD (die, c, assign_location_list_indexes (c));
}

/* We want to limit the number of external references, because they are
   larger than local references: a relocation takes multiple words, and
   even a sig8 reference is always eight bytes, whereas a local reference
   can be as small as one byte (though DW_FORM_ref is usually 4 in GCC).
   So if we encounter multiple external references to the same type DIE, we
   make a local typedef stub for it and redirect all references there.

   This is the element of the hash table for keeping track of these
   references.  */

struct external_ref
{
  dw_die_ref type;
  dw_die_ref stub;
  unsigned n_refs;
};

/* Hashtable helpers.  */

struct external_ref_hasher : free_ptr_hash <external_ref>
{
  static inline hashval_t hash (const external_ref *);
  static inline bool equal (const external_ref *, const external_ref *);
};

inline hashval_t
external_ref_hasher::hash (const external_ref *r)
{
  dw_die_ref die = r->type;
  hashval_t h = 0;

  /* We can't use the address of the DIE for hashing, because
     that will make the order of the stub DIEs non-deterministic.  */
  if (! die->comdat_type_p)
    /* We have a symbol; use it to compute a hash.  */
    h = htab_hash_string (die->die_id.die_symbol);
  else
    {
      /* We have a type signature; use a subset of the bits as the hash.
	 The 8-byte signature is at least as large as hashval_t.  */
      comdat_type_node *type_node = die->die_id.die_type_node;
      memcpy (&h, type_node->signature, sizeof (h));
    }
  return h;
}

inline bool
external_ref_hasher::equal (const external_ref *r1, const external_ref *r2)
{
  return r1->type == r2->type;
}

typedef hash_table<external_ref_hasher> external_ref_hash_type;

/* Return a pointer to the external_ref for references to DIE.  */

static struct external_ref *
lookup_external_ref (external_ref_hash_type *map, dw_die_ref die)
{
  struct external_ref ref, *ref_p;
  external_ref **slot;

  ref.type = die;
  slot = map->find_slot (&ref, INSERT);
  if (*slot != HTAB_EMPTY_ENTRY)
    return *slot;

  ref_p = XCNEW (struct external_ref);
  ref_p->type = die;
  *slot = ref_p;
  return ref_p;
}

/* Subroutine of optimize_external_refs, below.

   If we see a type skeleton, record it as our stub.  If we see external
   references, remember how many we've seen.  */

static void
optimize_external_refs_1 (dw_die_ref die, external_ref_hash_type *map)
{
  dw_die_ref c;
  dw_attr_node *a;
  unsigned ix;
  struct external_ref *ref_p;

  if (is_type_die (die)
      && (c = get_AT_ref (die, DW_AT_signature)))
    {
      /* This is a local skeleton; use it for local references.  */
      ref_p = lookup_external_ref (map, c);
      ref_p->stub = die;
    }

  /* Scan the DIE references, and remember any that refer to DIEs from
     other CUs (i.e. those which are not marked).  */
  FOR_EACH_VEC_SAFE_ELT (die->die_attr, ix, a)
    if (AT_class (a) == dw_val_class_die_ref
	&& (c = AT_ref (a))->die_mark == 0
	&& is_type_die (c))
      {
	ref_p = lookup_external_ref (map, c);
	ref_p->n_refs++;
      }

  FOR_EACH_CHILD (die, c, optimize_external_refs_1 (c, map));
}

/* htab_traverse callback function for optimize_external_refs, below.  SLOT
   points to an external_ref, DATA is the CU we're processing.  If we don't
   already have a local stub, and we have multiple refs, build a stub.  */

int
dwarf2_build_local_stub (external_ref **slot, dw_die_ref data)
{
  struct external_ref *ref_p = *slot;

  if (ref_p->stub == NULL && ref_p->n_refs > 1 && !dwarf_strict)
    {
      /* We have multiple references to this type, so build a small stub.
	 Both of these forms are a bit dodgy from the perspective of the
	 DWARF standard, since technically they should have names.  */
      dw_die_ref cu = data;
      dw_die_ref type = ref_p->type;
      dw_die_ref stub = NULL;

      if (type->comdat_type_p)
	{
	  /* If we refer to this type via sig8, use AT_signature.  */
	  stub = new_die (type->die_tag, cu, NULL_TREE);
	  add_AT_die_ref (stub, DW_AT_signature, type);
	}
      else
	{
	  /* Otherwise, use a typedef with no name.  */
	  stub = new_die (DW_TAG_typedef, cu, NULL_TREE);
	  add_AT_die_ref (stub, DW_AT_type, type);
	}

      stub->die_mark++;
      ref_p->stub = stub;
    }
  return 1;
}

/* DIE is a unit; look through all the DIE references to see if there are
   any external references to types, and if so, create local stubs for
   them which will be applied in build_abbrev_table.  This is useful because
   references to local DIEs are smaller.  */

static external_ref_hash_type *
optimize_external_refs (dw_die_ref die)
{
  external_ref_hash_type *map = new external_ref_hash_type (10);
  optimize_external_refs_1 (die, map);
  map->traverse <dw_die_ref, dwarf2_build_local_stub> (die);
  return map;
}

/* The following 3 variables are temporaries that are computed only during the
   build_abbrev_table call and used and released during the following
   optimize_abbrev_table call.  */

/* First abbrev_id that can be optimized based on usage.  */
static unsigned int abbrev_opt_start;

/* Maximum abbrev_id of a base type plus one (we can't optimize DIEs with
   abbrev_id smaller than this, because they must be already sized
   during build_abbrev_table).  */
static unsigned int abbrev_opt_base_type_end;

/* Vector of usage counts during build_abbrev_table.  Indexed by
   abbrev_id - abbrev_opt_start.  */
static vec<unsigned int> abbrev_usage_count;

/* Vector of all DIEs added with die_abbrev >= abbrev_opt_start.  */
static vec<dw_die_ref> sorted_abbrev_dies;

/* The format of each DIE (and its attribute value pairs) is encoded in an
   abbreviation table.  This routine builds the abbreviation table and assigns
   a unique abbreviation id for each abbreviation entry.  The children of each
   die are visited recursively.  */

static void
build_abbrev_table (dw_die_ref die, external_ref_hash_type *extern_map)
{
  unsigned int abbrev_id = 0;
  dw_die_ref c;
  dw_attr_node *a;
  unsigned ix;
  dw_die_ref abbrev;

  /* Scan the DIE references, and replace any that refer to
     DIEs from other CUs (i.e. those which are not marked) with
     the local stubs we built in optimize_external_refs.  */
  FOR_EACH_VEC_SAFE_ELT (die->die_attr, ix, a)
    if (AT_class (a) == dw_val_class_die_ref
	&& (c = AT_ref (a))->die_mark == 0)
      {
	struct external_ref *ref_p;
	gcc_assert (AT_ref (a)->comdat_type_p || AT_ref (a)->die_id.die_symbol);

	ref_p = lookup_external_ref (extern_map, c);
	if (ref_p->stub && ref_p->stub != die)
	  change_AT_die_ref (a, ref_p->stub);
	else
	  /* We aren't changing this reference, so mark it external.  */
	  set_AT_ref_external (a, 1);
      }

  FOR_EACH_VEC_SAFE_ELT (abbrev_die_table, abbrev_id, abbrev)
    {
      dw_attr_node *die_a, *abbrev_a;
      unsigned ix;
      bool ok = true;

      if (abbrev_id == 0)
	continue;
      if (abbrev->die_tag != die->die_tag)
	continue;
      if ((abbrev->die_child != NULL) != (die->die_child != NULL))
	continue;

      if (vec_safe_length (abbrev->die_attr) != vec_safe_length (die->die_attr))
	continue;

      FOR_EACH_VEC_SAFE_ELT (die->die_attr, ix, die_a)
	{
	  abbrev_a = &(*abbrev->die_attr)[ix];
	  if ((abbrev_a->dw_attr != die_a->dw_attr)
	      || (value_format (abbrev_a) != value_format (die_a)))
	    {
	      ok = false;
	      break;
	    }
	}
      if (ok)
	break;
    }

  if (abbrev_id >= vec_safe_length (abbrev_die_table))
    {
      vec_safe_push (abbrev_die_table, die);
      if (abbrev_opt_start)
	abbrev_usage_count.safe_push (0);
    }
  if (abbrev_opt_start && abbrev_id >= abbrev_opt_start)
    {
      abbrev_usage_count[abbrev_id - abbrev_opt_start]++;
      sorted_abbrev_dies.safe_push (die);
    }

  die->die_abbrev = abbrev_id;
  FOR_EACH_CHILD (die, c, build_abbrev_table (c, extern_map));
}

/* Callback function for sorted_abbrev_dies vector sorting.  We sort
   by die_abbrev's usage count, from the most commonly used
   abbreviation to the least.  */

static int
die_abbrev_cmp (const void *p1, const void *p2)
{
  dw_die_ref die1 = *(const dw_die_ref *) p1;
  dw_die_ref die2 = *(const dw_die_ref *) p2;

  gcc_checking_assert (die1->die_abbrev >= abbrev_opt_start);
  gcc_checking_assert (die2->die_abbrev >= abbrev_opt_start);

  if (die1->die_abbrev >= abbrev_opt_base_type_end
      && die2->die_abbrev >= abbrev_opt_base_type_end)
    {
      if (abbrev_usage_count[die1->die_abbrev - abbrev_opt_start]
	  > abbrev_usage_count[die2->die_abbrev - abbrev_opt_start])
	return -1;
      if (abbrev_usage_count[die1->die_abbrev - abbrev_opt_start]
	  < abbrev_usage_count[die2->die_abbrev - abbrev_opt_start])
	return 1;
    }

  /* Stabilize the sort.  */
  if (die1->die_abbrev < die2->die_abbrev)
    return -1;
  if (die1->die_abbrev > die2->die_abbrev)
    return 1;

  return 0;
}

/* Convert dw_val_class_const and dw_val_class_unsigned_const class attributes
   of DIEs in between sorted_abbrev_dies[first_id] and abbrev_dies[end_id - 1]
   into dw_val_class_const_implicit or
   dw_val_class_unsigned_const_implicit.  */

static void
optimize_implicit_const (unsigned int first_id, unsigned int end,
			 vec<bool> &implicit_consts)
{
  /* It never makes sense if there is just one DIE using the abbreviation.  */
  if (end < first_id + 2)
    return;

  dw_attr_node *a;
  unsigned ix, i;
  dw_die_ref die = sorted_abbrev_dies[first_id];
  FOR_EACH_VEC_SAFE_ELT (die->die_attr, ix, a)
    if (implicit_consts[ix])
      {
	enum dw_val_class new_class = dw_val_class_none;
	switch (AT_class (a))
	  {
	  case dw_val_class_unsigned_const:
	    if ((HOST_WIDE_INT) AT_unsigned (a) < 0)
	      continue;

	    /* The .debug_abbrev section will grow by
	       size_of_sleb128 (AT_unsigned (a)) and we avoid the constants
	       in all the DIEs using that abbreviation.  */
	    if (constant_size (AT_unsigned (a)) * (end - first_id)
		<= (unsigned) size_of_sleb128 (AT_unsigned (a)))
	      continue;

	    new_class = dw_val_class_unsigned_const_implicit;
	    break;

	  case dw_val_class_const:
	    new_class = dw_val_class_const_implicit;
	    break;

	  case dw_val_class_file:
	    new_class = dw_val_class_file_implicit;
	    break;

	  default:
	    continue;
	  }
	for (i = first_id; i < end; i++)
	  (*sorted_abbrev_dies[i]->die_attr)[ix].dw_attr_val.val_class
	    = new_class;
      }
}

/* Attempt to optimize abbreviation table from abbrev_opt_start
   abbreviation above.  */

static void
optimize_abbrev_table (void)
{
  if (abbrev_opt_start
      && vec_safe_length (abbrev_die_table) > abbrev_opt_start
      && (dwarf_version >= 5 || vec_safe_length (abbrev_die_table) > 127))
    {
      auto_vec<bool, 32> implicit_consts;
      sorted_abbrev_dies.qsort (die_abbrev_cmp);

      unsigned int abbrev_id = abbrev_opt_start - 1;
      unsigned int first_id = ~0U;
      unsigned int last_abbrev_id = 0;
      unsigned int i;
      dw_die_ref die;
      if (abbrev_opt_base_type_end > abbrev_opt_start)
	abbrev_id = abbrev_opt_base_type_end - 1;
      /* Reassign abbreviation ids from abbrev_opt_start above, so that
	 most commonly used abbreviations come first.  */
      FOR_EACH_VEC_ELT (sorted_abbrev_dies, i, die)
	{
	  dw_attr_node *a;
	  unsigned ix;

	  /* If calc_base_type_die_sizes has been called, the CU and
	     base types after it can't be optimized, because we've already
	     calculated their DIE offsets.  We've sorted them first.  */
	  if (die->die_abbrev < abbrev_opt_base_type_end)
	    continue;
	  if (die->die_abbrev != last_abbrev_id)
	    {
	      last_abbrev_id = die->die_abbrev;
	      if (dwarf_version >= 5 && first_id != ~0U)
		optimize_implicit_const (first_id, i, implicit_consts);
	      abbrev_id++;
	      (*abbrev_die_table)[abbrev_id] = die;
	      if (dwarf_version >= 5)
		{
		  first_id = i;
		  implicit_consts.truncate (0);

		  FOR_EACH_VEC_SAFE_ELT (die->die_attr, ix, a)
		    switch (AT_class (a))
		      {
		      case dw_val_class_const:
		      case dw_val_class_unsigned_const:
		      case dw_val_class_file:
			implicit_consts.safe_push (true);
			break;
		      default:
			implicit_consts.safe_push (false);
			break;
		      }
		}
	    }
	  else if (dwarf_version >= 5)
	    {
	      FOR_EACH_VEC_SAFE_ELT (die->die_attr, ix, a)
		if (!implicit_consts[ix])
		  continue;
		else
		  {
		    dw_attr_node *other_a
		      = &(*(*abbrev_die_table)[abbrev_id]->die_attr)[ix];
		    if (!dw_val_equal_p (&a->dw_attr_val,
					 &other_a->dw_attr_val))
		      implicit_consts[ix] = false;
		  }
	    }
	  die->die_abbrev = abbrev_id;
	}
      gcc_assert (abbrev_id == vec_safe_length (abbrev_die_table) - 1);
      if (dwarf_version >= 5 && first_id != ~0U)
	optimize_implicit_const (first_id, i, implicit_consts);
    }

  abbrev_opt_start = 0;
  abbrev_opt_base_type_end = 0;
  abbrev_usage_count.release ();
  sorted_abbrev_dies.release ();
}

/* Return the power-of-two number of bytes necessary to represent VALUE.  */

static int
constant_size (unsigned HOST_WIDE_INT value)
{
  int log;

  if (value == 0)
    log = 0;
  else
    log = floor_log2 (value);

  log = log / 8;
  log = 1 << (floor_log2 (log) + 1);

  return log;
}

/* Return the size of a DIE as it is represented in the
   .debug_info section.  */

static unsigned long
size_of_die (dw_die_ref die)
{
  unsigned long size = 0;
  dw_attr_node *a;
  unsigned ix;
  enum dwarf_form form;

  size += size_of_uleb128 (die->die_abbrev);
  FOR_EACH_VEC_SAFE_ELT (die->die_attr, ix, a)
    {
      switch (AT_class (a))
	{
	case dw_val_class_addr:
          if (dwarf_split_debug_info && AT_index (a) != NOT_INDEXED)
            {
              gcc_assert (AT_index (a) != NO_INDEX_ASSIGNED);
              size += size_of_uleb128 (AT_index (a));
            }
          else
            size += DWARF2_ADDR_SIZE;
	  break;
	case dw_val_class_offset:
	  size += DWARF_OFFSET_SIZE;
	  break;
	case dw_val_class_loc:
	  {
	    unsigned long lsize = size_of_locs (AT_loc (a));

	    /* Block length.  */
	    if (dwarf_version >= 4)
	      size += size_of_uleb128 (lsize);
	    else
	      size += constant_size (lsize);
	    size += lsize;
	  }
	  break;
	case dw_val_class_loc_list:
	  if (dwarf_split_debug_info && dwarf_version >= 5)
	    {
	      gcc_assert (AT_loc_list (a)->num_assigned);
	      size += size_of_uleb128 (AT_loc_list (a)->hash);
	    }
          else
            size += DWARF_OFFSET_SIZE;
	  break;
	case dw_val_class_range_list:
	  if (value_format (a) == DW_FORM_rnglistx)
	    {
	      gcc_assert (rnglist_idx);
	      dw_ranges *r = &(*ranges_table)[a->dw_attr_val.v.val_offset];
	      size += size_of_uleb128 (r->idx);
	    }
	  else
	    size += DWARF_OFFSET_SIZE;
	  break;
	case dw_val_class_const:
	  size += size_of_sleb128 (AT_int (a));
	  break;
	case dw_val_class_unsigned_const:
	  {
	    int csize = constant_size (AT_unsigned (a));
	    if (dwarf_version == 3
		&& a->dw_attr == DW_AT_data_member_location
		&& csize >= 4)
	      size += size_of_uleb128 (AT_unsigned (a));
	    else
	      size += csize;
	  }
	  break;
	case dw_val_class_const_implicit:
	case dw_val_class_unsigned_const_implicit:
	case dw_val_class_file_implicit:
	  /* These occupy no size in the DIE, just an extra sleb128 in
	     .debug_abbrev.  */
	  break;
	case dw_val_class_const_double:
	  size += HOST_BITS_PER_DOUBLE_INT / HOST_BITS_PER_CHAR;
	  if (HOST_BITS_PER_WIDE_INT >= DWARF_LARGEST_DATA_FORM_BITS)
	    size++; /* block */
	  break;
	case dw_val_class_wide_int:
	  size += (get_full_len (*a->dw_attr_val.v.val_wide)
		   * HOST_BITS_PER_WIDE_INT / HOST_BITS_PER_CHAR);
	  if (get_full_len (*a->dw_attr_val.v.val_wide)
	      * HOST_BITS_PER_WIDE_INT > DWARF_LARGEST_DATA_FORM_BITS)
	    size++; /* block */
	  break;
	case dw_val_class_vec:
	  size += constant_size (a->dw_attr_val.v.val_vec.length
				 * a->dw_attr_val.v.val_vec.elt_size)
		  + a->dw_attr_val.v.val_vec.length
		    * a->dw_attr_val.v.val_vec.elt_size; /* block */
	  break;
	case dw_val_class_flag:
	  if (dwarf_version >= 4)
	    /* Currently all add_AT_flag calls pass in 1 as last argument,
	       so DW_FORM_flag_present can be used.  If that ever changes,
	       we'll need to use DW_FORM_flag and have some optimization
	       in build_abbrev_table that will change those to
	       DW_FORM_flag_present if it is set to 1 in all DIEs using
	       the same abbrev entry.  */
	    gcc_assert (a->dw_attr_val.v.val_flag == 1);
	  else
	    size += 1;
	  break;
	case dw_val_class_die_ref:
	  if (AT_ref_external (a))
	    {
	      /* In DWARF4, we use DW_FORM_ref_sig8; for earlier versions
		 we use DW_FORM_ref_addr.  In DWARF2, DW_FORM_ref_addr
		 is sized by target address length, whereas in DWARF3
		 it's always sized as an offset.  */
	      if (use_debug_types)
		size += DWARF_TYPE_SIGNATURE_SIZE;
	      else if (dwarf_version == 2)
		size += DWARF2_ADDR_SIZE;
	      else
		size += DWARF_OFFSET_SIZE;
	    }
	  else
	    size += DWARF_OFFSET_SIZE;
	  break;
	case dw_val_class_fde_ref:
	  size += DWARF_OFFSET_SIZE;
	  break;
	case dw_val_class_lbl_id:
          if (dwarf_split_debug_info && AT_index (a) != NOT_INDEXED)
            {
              gcc_assert (AT_index (a) != NO_INDEX_ASSIGNED);
              size += size_of_uleb128 (AT_index (a));
            }
          else
            size += DWARF2_ADDR_SIZE;
	  break;
	case dw_val_class_lineptr:
	case dw_val_class_macptr:
	case dw_val_class_loclistsptr:
	  size += DWARF_OFFSET_SIZE;
	  break;
	case dw_val_class_str:
          form = AT_string_form (a);
	  if (form == DW_FORM_strp || form == DW_FORM_line_strp)
	    size += DWARF_OFFSET_SIZE;
	  else if (form == DW_FORM_GNU_str_index)
	    size += size_of_uleb128 (AT_index (a));
	  else
	    size += strlen (a->dw_attr_val.v.val_str->str) + 1;
	  break;
	case dw_val_class_file:
	  size += constant_size (maybe_emit_file (a->dw_attr_val.v.val_file));
	  break;
	case dw_val_class_data8:
	  size += 8;
	  break;
	case dw_val_class_vms_delta:
	  size += DWARF_OFFSET_SIZE;
	  break;
	case dw_val_class_high_pc:
	  size += DWARF2_ADDR_SIZE;
	  break;
	case dw_val_class_discr_value:
	  size += size_of_discr_value (&a->dw_attr_val.v.val_discr_value);
	  break;
	case dw_val_class_discr_list:
	    {
	      unsigned block_size = size_of_discr_list (AT_discr_list (a));

	      /* This is a block, so we have the block length and then its
		 data.  */
	      size += constant_size (block_size) + block_size;
	    }
	  break;
	default:
	  gcc_unreachable ();
	}
    }

  return size;
}

/* Size the debugging information associated with a given DIE.  Visits the
   DIE's children recursively.  Updates the global variable next_die_offset, on
   each time through.  Uses the current value of next_die_offset to update the
   die_offset field in each DIE.  */

static void
calc_die_sizes (dw_die_ref die)
{
  dw_die_ref c;

  gcc_assert (die->die_offset == 0
	      || (unsigned long int) die->die_offset == next_die_offset);
  die->die_offset = next_die_offset;
  next_die_offset += size_of_die (die);

  FOR_EACH_CHILD (die, c, calc_die_sizes (c));

  if (die->die_child != NULL)
    /* Count the null byte used to terminate sibling lists.  */
    next_die_offset += 1;
}

/* Size just the base type children at the start of the CU.
   This is needed because build_abbrev needs to size locs
   and sizing of type based stack ops needs to know die_offset
   values for the base types.  */

static void
calc_base_type_die_sizes (void)
{
  unsigned long die_offset = (dwarf_split_debug_info
			      ? DWARF_COMPILE_UNIT_SKELETON_HEADER_SIZE
			      : DWARF_COMPILE_UNIT_HEADER_SIZE);
  unsigned int i;
  dw_die_ref base_type;
#if ENABLE_ASSERT_CHECKING
  dw_die_ref prev = comp_unit_die ()->die_child;
#endif

  die_offset += size_of_die (comp_unit_die ());
  for (i = 0; base_types.iterate (i, &base_type); i++)
    {
#if ENABLE_ASSERT_CHECKING
      gcc_assert (base_type->die_offset == 0
		  && prev->die_sib == base_type
		  && base_type->die_child == NULL
		  && base_type->die_abbrev);
      prev = base_type;
#endif
      if (abbrev_opt_start
	  && base_type->die_abbrev >= abbrev_opt_base_type_end)
	abbrev_opt_base_type_end = base_type->die_abbrev + 1;
      base_type->die_offset = die_offset;
      die_offset += size_of_die (base_type);
    }
}

/* Set the marks for a die and its children.  We do this so
   that we know whether or not a reference needs to use FORM_ref_addr; only
   DIEs in the same CU will be marked.  We used to clear out the offset
   and use that as the flag, but ran into ordering problems.  */

static void
mark_dies (dw_die_ref die)
{
  dw_die_ref c;

  gcc_assert (!die->die_mark);

  die->die_mark = 1;
  FOR_EACH_CHILD (die, c, mark_dies (c));
}

/* Clear the marks for a die and its children.  */

static void
unmark_dies (dw_die_ref die)
{
  dw_die_ref c;

  if (! use_debug_types)
    gcc_assert (die->die_mark);

  die->die_mark = 0;
  FOR_EACH_CHILD (die, c, unmark_dies (c));
}

/* Clear the marks for a die, its children and referred dies.  */

static void
unmark_all_dies (dw_die_ref die)
{
  dw_die_ref c;
  dw_attr_node *a;
  unsigned ix;

  if (!die->die_mark)
    return;
  die->die_mark = 0;

  FOR_EACH_CHILD (die, c, unmark_all_dies (c));

  FOR_EACH_VEC_SAFE_ELT (die->die_attr, ix, a)
    if (AT_class (a) == dw_val_class_die_ref)
      unmark_all_dies (AT_ref (a));
}

/* Calculate if the entry should appear in the final output file.  It may be
   from a pruned a type.  */

static bool
include_pubname_in_output (vec<pubname_entry, va_gc> *table, pubname_entry *p)
{
  /* By limiting gnu pubnames to definitions only, gold can generate a
     gdb index without entries for declarations, which don't include
     enough information to be useful.  */
  if (debug_generate_pub_sections == 2 && is_declaration_die (p->die))
    return false;

  if (table == pubname_table)
    {
      /* Enumerator names are part of the pubname table, but the
         parent DW_TAG_enumeration_type die may have been pruned.
         Don't output them if that is the case.  */
      if (p->die->die_tag == DW_TAG_enumerator &&
          (p->die->die_parent == NULL
           || !p->die->die_parent->die_perennial_p))
        return false;

      /* Everything else in the pubname table is included.  */
      return true;
    }

  /* The pubtypes table shouldn't include types that have been
     pruned.  */
  return (p->die->die_offset != 0
          || !flag_eliminate_unused_debug_types);
}

/* Return the size of the .debug_pubnames or .debug_pubtypes table
   generated for the compilation unit.  */

static unsigned long
size_of_pubnames (vec<pubname_entry, va_gc> *names)
{
  unsigned long size;
  unsigned i;
  pubname_entry *p;
  int space_for_flags = (debug_generate_pub_sections == 2) ? 1 : 0;

  size = DWARF_PUBNAMES_HEADER_SIZE;
  FOR_EACH_VEC_ELT (*names, i, p)
    if (include_pubname_in_output (names, p))
      size += strlen (p->name) + DWARF_OFFSET_SIZE + 1 + space_for_flags;

  size += DWARF_OFFSET_SIZE;
  return size;
}

/* Return the size of the information in the .debug_aranges section.  */

static unsigned long
size_of_aranges (void)
{
  unsigned long size;

  size = DWARF_ARANGES_HEADER_SIZE;

  /* Count the address/length pair for this compilation unit.  */
  if (text_section_used)
    size += 2 * DWARF2_ADDR_SIZE;
  if (cold_text_section_used)
    size += 2 * DWARF2_ADDR_SIZE;
  if (have_multiple_function_sections)
    {
      unsigned fde_idx;
      dw_fde_ref fde;

      FOR_EACH_VEC_ELT (*fde_vec, fde_idx, fde)
	{
	  if (DECL_IGNORED_P (fde->decl))
	    continue;
	  if (!fde->in_std_section)
	    size += 2 * DWARF2_ADDR_SIZE;
	  if (fde->dw_fde_second_begin && !fde->second_in_std_section)
	    size += 2 * DWARF2_ADDR_SIZE;
	}
    }

  /* Count the two zero words used to terminated the address range table.  */
  size += 2 * DWARF2_ADDR_SIZE;
  return size;
}

/* Select the encoding of an attribute value.  */

static enum dwarf_form
value_format (dw_attr_node *a)
{
  switch (AT_class (a))
    {
    case dw_val_class_addr:
      /* Only very few attributes allow DW_FORM_addr.  */
      switch (a->dw_attr)
	{
	case DW_AT_low_pc:
	case DW_AT_high_pc:
	case DW_AT_entry_pc:
	case DW_AT_trampoline:
          return (AT_index (a) == NOT_INDEXED
                  ? DW_FORM_addr : DW_FORM_GNU_addr_index);
	default:
	  break;
	}
      switch (DWARF2_ADDR_SIZE)
	{
	case 1:
	  return DW_FORM_data1;
	case 2:
	  return DW_FORM_data2;
	case 4:
	  return DW_FORM_data4;
	case 8:
	  return DW_FORM_data8;
	default:
	  gcc_unreachable ();
	}
    case dw_val_class_loc_list:
      if (dwarf_split_debug_info
	  && dwarf_version >= 5
	  && AT_loc_list (a)->num_assigned)
	return DW_FORM_loclistx;
      /* FALLTHRU */
    case dw_val_class_range_list:
      /* For range lists in DWARF 5, use DW_FORM_rnglistx from .debug_info.dwo
	 but in .debug_info use DW_FORM_sec_offset, which is shorter if we
	 care about sizes of .debug* sections in shared libraries and
	 executables and don't take into account relocations that affect just
	 relocatable objects - for DW_FORM_rnglistx we'd have to emit offset
	 table in the .debug_rnglists section.  */
      if (dwarf_split_debug_info
	  && dwarf_version >= 5
	  && AT_class (a) == dw_val_class_range_list
	  && rnglist_idx
	  && a->dw_attr_val.val_entry != RELOCATED_OFFSET)
	return DW_FORM_rnglistx;
      if (dwarf_version >= 4)
	return DW_FORM_sec_offset;
      /* FALLTHRU */
    case dw_val_class_vms_delta:
    case dw_val_class_offset:
      switch (DWARF_OFFSET_SIZE)
	{
	case 4:
	  return DW_FORM_data4;
	case 8:
	  return DW_FORM_data8;
	default:
	  gcc_unreachable ();
	}
    case dw_val_class_loc:
      if (dwarf_version >= 4)
	return DW_FORM_exprloc;
      switch (constant_size (size_of_locs (AT_loc (a))))
	{
	case 1:
	  return DW_FORM_block1;
	case 2:
	  return DW_FORM_block2;
	case 4:
	  return DW_FORM_block4;
	default:
	  gcc_unreachable ();
	}
    case dw_val_class_const:
      return DW_FORM_sdata;
    case dw_val_class_unsigned_const:
      switch (constant_size (AT_unsigned (a)))
	{
	case 1:
	  return DW_FORM_data1;
	case 2:
	  return DW_FORM_data2;
	case 4:
	  /* In DWARF3 DW_AT_data_member_location with
	     DW_FORM_data4 or DW_FORM_data8 is a loclistptr, not
	     constant, so we need to use DW_FORM_udata if we need
	     a large constant.  */
	  if (dwarf_version == 3 && a->dw_attr == DW_AT_data_member_location)
	    return DW_FORM_udata;
	  return DW_FORM_data4;
	case 8:
	  if (dwarf_version == 3 && a->dw_attr == DW_AT_data_member_location)
	    return DW_FORM_udata;
	  return DW_FORM_data8;
	default:
	  gcc_unreachable ();
	}
    case dw_val_class_const_implicit:
    case dw_val_class_unsigned_const_implicit:
    case dw_val_class_file_implicit:
      return DW_FORM_implicit_const;
    case dw_val_class_const_double:
      switch (HOST_BITS_PER_WIDE_INT)
	{
	case 8:
	  return DW_FORM_data2;
	case 16:
	  return DW_FORM_data4;
	case 32:
	  return DW_FORM_data8;
	case 64:
	  if (dwarf_version >= 5)
	    return DW_FORM_data16;
	  /* FALLTHRU */
	default:
	  return DW_FORM_block1;
	}
    case dw_val_class_wide_int:
      switch (get_full_len (*a->dw_attr_val.v.val_wide) * HOST_BITS_PER_WIDE_INT)
	{
	case 8:
	  return DW_FORM_data1;
	case 16:
	  return DW_FORM_data2;
	case 32:
	  return DW_FORM_data4;
	case 64:
	  return DW_FORM_data8;
	case 128:
	  if (dwarf_version >= 5)
	    return DW_FORM_data16;
	  /* FALLTHRU */
	default:
	  return DW_FORM_block1;
	}
    case dw_val_class_vec:
      switch (constant_size (a->dw_attr_val.v.val_vec.length
			     * a->dw_attr_val.v.val_vec.elt_size))
	{
	case 1:
	  return DW_FORM_block1;
	case 2:
	  return DW_FORM_block2;
	case 4:
	  return DW_FORM_block4;
	default:
	  gcc_unreachable ();
	}
    case dw_val_class_flag:
      if (dwarf_version >= 4)
	{
	  /* Currently all add_AT_flag calls pass in 1 as last argument,
	     so DW_FORM_flag_present can be used.  If that ever changes,
	     we'll need to use DW_FORM_flag and have some optimization
	     in build_abbrev_table that will change those to
	     DW_FORM_flag_present if it is set to 1 in all DIEs using
	     the same abbrev entry.  */
	  gcc_assert (a->dw_attr_val.v.val_flag == 1);
	  return DW_FORM_flag_present;
	}
      return DW_FORM_flag;
    case dw_val_class_die_ref:
      if (AT_ref_external (a))
	return use_debug_types ? DW_FORM_ref_sig8 : DW_FORM_ref_addr;
      else
	return DW_FORM_ref;
    case dw_val_class_fde_ref:
      return DW_FORM_data;
    case dw_val_class_lbl_id:
      return (AT_index (a) == NOT_INDEXED
              ? DW_FORM_addr : DW_FORM_GNU_addr_index);
    case dw_val_class_lineptr:
    case dw_val_class_macptr:
    case dw_val_class_loclistsptr:
      return dwarf_version >= 4 ? DW_FORM_sec_offset : DW_FORM_data;
    case dw_val_class_str:
      return AT_string_form (a);
    case dw_val_class_file:
      switch (constant_size (maybe_emit_file (a->dw_attr_val.v.val_file)))
	{
	case 1:
	  return DW_FORM_data1;
	case 2:
	  return DW_FORM_data2;
	case 4:
	  return DW_FORM_data4;
	default:
	  gcc_unreachable ();
	}

    case dw_val_class_data8:
      return DW_FORM_data8;

    case dw_val_class_high_pc:
      switch (DWARF2_ADDR_SIZE)
	{
	case 1:
	  return DW_FORM_data1;
	case 2:
	  return DW_FORM_data2;
	case 4:
	  return DW_FORM_data4;
	case 8:
	  return DW_FORM_data8;
	default:
	  gcc_unreachable ();
	}

    case dw_val_class_discr_value:
      return (a->dw_attr_val.v.val_discr_value.pos
	      ? DW_FORM_udata
	      : DW_FORM_sdata);
    case dw_val_class_discr_list:
      switch (constant_size (size_of_discr_list (AT_discr_list (a))))
	{
	case 1:
	  return DW_FORM_block1;
	case 2:
	  return DW_FORM_block2;
	case 4:
	  return DW_FORM_block4;
	default:
	  gcc_unreachable ();
	}

    default:
      gcc_unreachable ();
    }
}

/* Output the encoding of an attribute value.  */

static void
output_value_format (dw_attr_node *a)
{
  enum dwarf_form form = value_format (a);

  dw2_asm_output_data_uleb128 (form, "(%s)", dwarf_form_name (form));
}

/* Given a die and id, produce the appropriate abbreviations.  */

static void
output_die_abbrevs (unsigned long abbrev_id, dw_die_ref abbrev)
{
  unsigned ix;
  dw_attr_node *a_attr;

  dw2_asm_output_data_uleb128 (abbrev_id, "(abbrev code)");
  dw2_asm_output_data_uleb128 (abbrev->die_tag, "(TAG: %s)",
                               dwarf_tag_name (abbrev->die_tag));

  if (abbrev->die_child != NULL)
    dw2_asm_output_data (1, DW_children_yes, "DW_children_yes");
  else
    dw2_asm_output_data (1, DW_children_no, "DW_children_no");

  for (ix = 0; vec_safe_iterate (abbrev->die_attr, ix, &a_attr); ix++)
    {
      dw2_asm_output_data_uleb128 (a_attr->dw_attr, "(%s)",
                                   dwarf_attr_name (a_attr->dw_attr));
      output_value_format (a_attr);
      if (value_format (a_attr) == DW_FORM_implicit_const)
	{
	  if (AT_class (a_attr) == dw_val_class_file_implicit)
	    {
	      int f = maybe_emit_file (a_attr->dw_attr_val.v.val_file);
	      const char *filename = a_attr->dw_attr_val.v.val_file->filename;
	      dw2_asm_output_data_sleb128 (f, "(%s)", filename);
	    }
	  else
	    dw2_asm_output_data_sleb128 (a_attr->dw_attr_val.v.val_int, NULL);
	}
    }

  dw2_asm_output_data (1, 0, NULL);
  dw2_asm_output_data (1, 0, NULL);
}


/* Output the .debug_abbrev section which defines the DIE abbreviation
   table.  */

static void
output_abbrev_section (void)
{
  unsigned int abbrev_id;
  dw_die_ref abbrev;

  FOR_EACH_VEC_SAFE_ELT (abbrev_die_table, abbrev_id, abbrev)
    if (abbrev_id != 0)
      output_die_abbrevs (abbrev_id, abbrev);

  /* Terminate the table.  */
  dw2_asm_output_data (1, 0, NULL);
}

/* Output a symbol we can use to refer to this DIE from another CU.  */

static inline void
output_die_symbol (dw_die_ref die)
{
  const char *sym = die->die_id.die_symbol;

  gcc_assert (!die->comdat_type_p);

  if (sym == 0)
    return;

  if (strncmp (sym, DIE_LABEL_PREFIX, sizeof (DIE_LABEL_PREFIX) - 1) == 0)
    /* We make these global, not weak; if the target doesn't support
       .linkonce, it doesn't support combining the sections, so debugging
       will break.  */
    targetm.asm_out.globalize_label (asm_out_file, sym);

  ASM_OUTPUT_LABEL (asm_out_file, sym);
}

/* Return a new location list, given the begin and end range, and the
   expression.  */

static inline dw_loc_list_ref
new_loc_list (dw_loc_descr_ref expr, const char *begin, const char *end,
	      const char *section)
{
  dw_loc_list_ref retlist = ggc_cleared_alloc<dw_loc_list_node> ();

  retlist->begin = begin;
  retlist->begin_entry = NULL;
  retlist->end = end;
  retlist->expr = expr;
  retlist->section = section;

  return retlist;
}

/* Generate a new internal symbol for this location list node, if it
   hasn't got one yet.  */

static inline void
gen_llsym (dw_loc_list_ref list)
{
  gcc_assert (!list->ll_symbol);
  list->ll_symbol = gen_internal_sym ("LLST");
}

/* Output the location list given to us.  */

static void
output_loc_list (dw_loc_list_ref list_head)
{
  if (list_head->emitted)
    return;
  list_head->emitted = true;

  ASM_OUTPUT_LABEL (asm_out_file, list_head->ll_symbol);

  dw_loc_list_ref curr = list_head;
  const char *last_section = NULL;
  const char *base_label = NULL;

  /* Walk the location list, and output each range + expression.  */
  for (curr = list_head; curr != NULL; curr = curr->dw_loc_next)
    {
      unsigned long size;
      /* Don't output an entry that starts and ends at the same address.  */
      if (strcmp (curr->begin, curr->end) == 0 && !curr->force)
	continue;
      size = size_of_locs (curr->expr);
      /* If the expression is too large, drop it on the floor.  We could
	 perhaps put it into DW_TAG_dwarf_procedure and refer to that
	 in the expression, but >= 64KB expressions for a single value
	 in a single range are unlikely very useful.  */
      if (dwarf_version < 5 && size > 0xffff)
	continue;
      if (dwarf_version >= 5)
	{
	  if (dwarf_split_debug_info)
	    {
	      /* For -gsplit-dwarf, emit DW_LLE_starx_length, which has
		 uleb128 index into .debug_addr and uleb128 length.  */
	      dw2_asm_output_data (1, DW_LLE_startx_length,
				   "DW_LLE_startx_length (%s)",
				   list_head->ll_symbol);
	      dw2_asm_output_data_uleb128 (curr->begin_entry->index,
					   "Location list range start index "
					   "(%s)", curr->begin);
	      /* FIXME: This will ICE ifndef HAVE_AS_LEB128.
		 For that case we probably need to emit DW_LLE_startx_endx,
		 but we'd need 2 .debug_addr entries rather than just one.  */
	      dw2_asm_output_delta_uleb128 (curr->end, curr->begin,
					    "Location list length (%s)",
					    list_head->ll_symbol);
	    }
	  else if (!have_multiple_function_sections && HAVE_AS_LEB128)
	    {
	      /* If all code is in .text section, the base address is
		 already provided by the CU attributes.  Use
		 DW_LLE_offset_pair where both addresses are uleb128 encoded
		 offsets against that base.  */
	      dw2_asm_output_data (1, DW_LLE_offset_pair,
				   "DW_LLE_offset_pair (%s)",
				   list_head->ll_symbol);
	      dw2_asm_output_delta_uleb128 (curr->begin, curr->section,
					    "Location list begin address (%s)",
					    list_head->ll_symbol);
	      dw2_asm_output_delta_uleb128 (curr->end, curr->section,
					    "Location list end address (%s)",
					    list_head->ll_symbol);
	    }
	  else if (HAVE_AS_LEB128)
	    {
	      /* Otherwise, find out how many consecutive entries could share
		 the same base entry.  If just one, emit DW_LLE_start_length,
		 otherwise emit DW_LLE_base_address for the base address
		 followed by a series of DW_LLE_offset_pair.  */
	      if (last_section == NULL || curr->section != last_section)
		{
		  dw_loc_list_ref curr2;
		  for (curr2 = curr->dw_loc_next; curr2 != NULL;
		       curr2 = curr2->dw_loc_next)
		    {
		      if (strcmp (curr2->begin, curr2->end) == 0
			  && !curr2->force)
			continue;
		      break;
		    }
		  if (curr2 == NULL || curr->section != curr2->section)
		    last_section = NULL;
		  else
		    {
		      last_section = curr->section;
		      base_label = curr->begin;
		      dw2_asm_output_data (1, DW_LLE_base_address,
					   "DW_LLE_base_address (%s)",
					   list_head->ll_symbol);
		      dw2_asm_output_addr (DWARF2_ADDR_SIZE, base_label,
					   "Base address (%s)",
					   list_head->ll_symbol);
		    }
		}
	      /* Only one entry with the same base address.  Use
		 DW_LLE_start_length with absolute address and uleb128
		 length.  */
	      if (last_section == NULL)
		{
		  dw2_asm_output_data (1, DW_LLE_start_length,
				       "DW_LLE_start_length (%s)",
				       list_head->ll_symbol);
		  dw2_asm_output_addr (DWARF2_ADDR_SIZE, curr->begin,
				       "Location list begin address (%s)",
				       list_head->ll_symbol);
		  dw2_asm_output_delta_uleb128 (curr->end, curr->begin,
						"Location list length "
						"(%s)", list_head->ll_symbol);
		}
	      /* Otherwise emit DW_LLE_offset_pair, relative to above emitted
		 DW_LLE_base_address.  */
	      else
		{
		  dw2_asm_output_data (1, DW_LLE_offset_pair,
				       "DW_LLE_offset_pair (%s)",
				       list_head->ll_symbol);
		  dw2_asm_output_delta_uleb128 (curr->begin, base_label,
						"Location list begin address "
						"(%s)", list_head->ll_symbol);
		  dw2_asm_output_delta_uleb128 (curr->end, base_label,
						"Location list end address "
						"(%s)", list_head->ll_symbol);
		}
	    }
	  /* The assembler does not support .uleb128 directive.  Emit
	     DW_LLE_start_end with a pair of absolute addresses.  */
	  else
	    {
	      dw2_asm_output_data (1, DW_LLE_start_end,
				   "DW_LLE_start_end (%s)",
				   list_head->ll_symbol);
	      dw2_asm_output_addr (DWARF2_ADDR_SIZE, curr->begin,
				   "Location list begin address (%s)",
				   list_head->ll_symbol);
	      dw2_asm_output_addr (DWARF2_ADDR_SIZE, curr->end,
				   "Location list end address (%s)",
				   list_head->ll_symbol);
	    }
	}
      else if (dwarf_split_debug_info)
	{
	  /* For -gsplit-dwarf -gdwarf-{2,3,4} emit index into .debug_addr
	     and 4 byte length.  */
	  dw2_asm_output_data (1, DW_LLE_GNU_start_length_entry,
			       "Location list start/length entry (%s)",
			       list_head->ll_symbol);
	  dw2_asm_output_data_uleb128 (curr->begin_entry->index,
				       "Location list range start index (%s)",
				       curr->begin);
	  /* The length field is 4 bytes.  If we ever need to support
	     an 8-byte length, we can add a new DW_LLE code or fall back
	     to DW_LLE_GNU_start_end_entry.  */
	  dw2_asm_output_delta (4, curr->end, curr->begin,
				"Location list range length (%s)",
				list_head->ll_symbol);
	}
      else if (!have_multiple_function_sections)
	{
	  /* Pair of relative addresses against start of text section.  */
	  dw2_asm_output_delta (DWARF2_ADDR_SIZE, curr->begin, curr->section,
				"Location list begin address (%s)",
				list_head->ll_symbol);
	  dw2_asm_output_delta (DWARF2_ADDR_SIZE, curr->end, curr->section,
				"Location list end address (%s)",
				list_head->ll_symbol);
	}
      else
	{
	  /* Pair of absolute addresses.  */
	  dw2_asm_output_addr (DWARF2_ADDR_SIZE, curr->begin,
			       "Location list begin address (%s)",
			       list_head->ll_symbol);
	  dw2_asm_output_addr (DWARF2_ADDR_SIZE, curr->end,
			       "Location list end address (%s)",
			       list_head->ll_symbol);
	}

      /* Output the block length for this list of location operations.  */
      if (dwarf_version >= 5)
	dw2_asm_output_data_uleb128 (size, "Location expression size");
      else
	{
	  gcc_assert (size <= 0xffff);
	  dw2_asm_output_data (2, size, "Location expression size");
	}

      output_loc_sequence (curr->expr, -1);
    }

  /* And finally list termination.  */
  if (dwarf_version >= 5)
    dw2_asm_output_data (1, DW_LLE_end_of_list,
			 "DW_LLE_end_of_list (%s)", list_head->ll_symbol);
  else if (dwarf_split_debug_info)
    dw2_asm_output_data (1, DW_LLE_GNU_end_of_list_entry,
			 "Location list terminator (%s)",
			 list_head->ll_symbol);
  else
    {
      dw2_asm_output_data (DWARF2_ADDR_SIZE, 0,
			   "Location list terminator begin (%s)",
			   list_head->ll_symbol);
      dw2_asm_output_data (DWARF2_ADDR_SIZE, 0,
			   "Location list terminator end (%s)",
			   list_head->ll_symbol);
    }
}

/* Output a range_list offset into the .debug_ranges or .debug_rnglists
   section.  Emit a relocated reference if val_entry is NULL, otherwise,
   emit an indirect reference.  */

static void
output_range_list_offset (dw_attr_node *a)
{
  const char *name = dwarf_attr_name (a->dw_attr);

  if (a->dw_attr_val.val_entry == RELOCATED_OFFSET)
    {
      if (dwarf_version >= 5)
	{
	  dw_ranges *r = &(*ranges_table)[a->dw_attr_val.v.val_offset];
	  dw2_asm_output_offset (DWARF_OFFSET_SIZE, r->label,
				 debug_ranges_section, "%s", name);
	}
      else
	{
	  char *p = strchr (ranges_section_label, '\0');
	  sprintf (p, "+" HOST_WIDE_INT_PRINT_HEX,
		   a->dw_attr_val.v.val_offset * 2 * DWARF2_ADDR_SIZE);
	  dw2_asm_output_offset (DWARF_OFFSET_SIZE, ranges_section_label,
				 debug_ranges_section, "%s", name);
	  *p = '\0';
	}
    }
  else if (dwarf_version >= 5)
    {
      dw_ranges *r = &(*ranges_table)[a->dw_attr_val.v.val_offset];
      gcc_assert (rnglist_idx);
      dw2_asm_output_data_uleb128 (r->idx, "%s", name);
    }
  else
    dw2_asm_output_data (DWARF_OFFSET_SIZE,
			 a->dw_attr_val.v.val_offset * 2 * DWARF2_ADDR_SIZE,
                         "%s (offset from %s)", name, ranges_section_label);
}

/* Output the offset into the debug_loc section.  */

static void
output_loc_list_offset (dw_attr_node *a)
{
  char *sym = AT_loc_list (a)->ll_symbol;

  gcc_assert (sym);
  if (!dwarf_split_debug_info)
    dw2_asm_output_offset (DWARF_OFFSET_SIZE, sym, debug_loc_section,
                           "%s", dwarf_attr_name (a->dw_attr));
  else if (dwarf_version >= 5)
    {
      gcc_assert (AT_loc_list (a)->num_assigned);
      dw2_asm_output_data_uleb128 (AT_loc_list (a)->hash, "%s (%s)",
				   dwarf_attr_name (a->dw_attr),
				   sym);
    }
  else
    dw2_asm_output_delta (DWARF_OFFSET_SIZE, sym, loc_section_label,
			  "%s", dwarf_attr_name (a->dw_attr));
}

/* Output an attribute's index or value appropriately.  */

static void
output_attr_index_or_value (dw_attr_node *a)
{
  const char *name = dwarf_attr_name (a->dw_attr);

  if (dwarf_split_debug_info && AT_index (a) != NOT_INDEXED)
    {
      dw2_asm_output_data_uleb128 (AT_index (a), "%s", name);
      return;
    }
  switch (AT_class (a))
    {
    case dw_val_class_addr:
      dw2_asm_output_addr_rtx (DWARF2_ADDR_SIZE, AT_addr (a), "%s", name);
      break;
    case dw_val_class_high_pc:
    case dw_val_class_lbl_id:
      dw2_asm_output_addr (DWARF2_ADDR_SIZE, AT_lbl (a), "%s", name);
      break;
    default:
      gcc_unreachable ();
    }
}

/* Output a type signature.  */

static inline void
output_signature (const char *sig, const char *name)
{
  int i;

  for (i = 0; i < DWARF_TYPE_SIGNATURE_SIZE; i++)
    dw2_asm_output_data (1, sig[i], i == 0 ? "%s" : NULL, name);
}

/* Output a discriminant value.  */

static inline void
output_discr_value (dw_discr_value *discr_value, const char *name)
{
  if (discr_value->pos)
    dw2_asm_output_data_uleb128 (discr_value->v.uval, "%s", name);
  else
    dw2_asm_output_data_sleb128 (discr_value->v.sval, "%s", name);
}

/* Output the DIE and its attributes.  Called recursively to generate
   the definitions of each child DIE.  */

static void
output_die (dw_die_ref die)
{
  dw_attr_node *a;
  dw_die_ref c;
  unsigned long size;
  unsigned ix;

  /* If someone in another CU might refer to us, set up a symbol for
     them to point to.  */
  if (! die->comdat_type_p && die->die_id.die_symbol
      /* Don't output the symbol twice.  For LTO we want the label
         on the section beginning, not on the actual DIE.  */
      && ((!flag_generate_lto && !flag_generate_offload)
	  || die->die_tag != DW_TAG_compile_unit))
    output_die_symbol (die);

  dw2_asm_output_data_uleb128 (die->die_abbrev, "(DIE (%#lx) %s)",
			       (unsigned long)die->die_offset,
			       dwarf_tag_name (die->die_tag));

  FOR_EACH_VEC_SAFE_ELT (die->die_attr, ix, a)
    {
      const char *name = dwarf_attr_name (a->dw_attr);

      switch (AT_class (a))
	{
	case dw_val_class_addr:
          output_attr_index_or_value (a);
	  break;

	case dw_val_class_offset:
	  dw2_asm_output_data (DWARF_OFFSET_SIZE, a->dw_attr_val.v.val_offset,
			       "%s", name);
	  break;

	case dw_val_class_range_list:
          output_range_list_offset (a);
	  break;

	case dw_val_class_loc:
	  size = size_of_locs (AT_loc (a));

	  /* Output the block length for this list of location operations.  */
	  if (dwarf_version >= 4)
	    dw2_asm_output_data_uleb128 (size, "%s", name);
	  else
	    dw2_asm_output_data (constant_size (size), size, "%s", name);

	  output_loc_sequence (AT_loc (a), -1);
	  break;

	case dw_val_class_const:
	  /* ??? It would be slightly more efficient to use a scheme like is
	     used for unsigned constants below, but gdb 4.x does not sign
	     extend.  Gdb 5.x does sign extend.  */
	  dw2_asm_output_data_sleb128 (AT_int (a), "%s", name);
	  break;

	case dw_val_class_unsigned_const:
	  {
	    int csize = constant_size (AT_unsigned (a));
	    if (dwarf_version == 3
		&& a->dw_attr == DW_AT_data_member_location
		&& csize >= 4)
	      dw2_asm_output_data_uleb128 (AT_unsigned (a), "%s", name);
	    else
	      dw2_asm_output_data (csize, AT_unsigned (a), "%s", name);
	  }
	  break;

	case dw_val_class_const_implicit:
	  if (flag_debug_asm)
	    fprintf (asm_out_file, "\t\t\t%s %s ("
				   HOST_WIDE_INT_PRINT_DEC ")\n",
		     ASM_COMMENT_START, name, AT_int (a));
	  break;

	case dw_val_class_unsigned_const_implicit:
	  if (flag_debug_asm)
	    fprintf (asm_out_file, "\t\t\t%s %s ("
				   HOST_WIDE_INT_PRINT_HEX ")\n",
		     ASM_COMMENT_START, name, AT_unsigned (a));
	  break;

	case dw_val_class_const_double:
	  {
	    unsigned HOST_WIDE_INT first, second;

	    if (HOST_BITS_PER_WIDE_INT >= DWARF_LARGEST_DATA_FORM_BITS)
	      dw2_asm_output_data (1,
				   HOST_BITS_PER_DOUBLE_INT
				   / HOST_BITS_PER_CHAR,
				   NULL);

	    if (WORDS_BIG_ENDIAN)
	      {
		first = a->dw_attr_val.v.val_double.high;
		second = a->dw_attr_val.v.val_double.low;
	      }
	    else
	      {
		first = a->dw_attr_val.v.val_double.low;
		second = a->dw_attr_val.v.val_double.high;
	      }

	    dw2_asm_output_data (HOST_BITS_PER_WIDE_INT / HOST_BITS_PER_CHAR,
                                 first, "%s", name);
	    dw2_asm_output_data (HOST_BITS_PER_WIDE_INT / HOST_BITS_PER_CHAR,
				 second, NULL);
	  }
	  break;

	case dw_val_class_wide_int:
	  {
	    int i;
	    int len = get_full_len (*a->dw_attr_val.v.val_wide);
	    int l = HOST_BITS_PER_WIDE_INT / HOST_BITS_PER_CHAR;
	    if (len * HOST_BITS_PER_WIDE_INT > DWARF_LARGEST_DATA_FORM_BITS)
	      dw2_asm_output_data (1, get_full_len (*a->dw_attr_val.v.val_wide)
				      * l, NULL);

	    if (WORDS_BIG_ENDIAN)
	      for (i = len - 1; i >= 0; --i)
		{
		  dw2_asm_output_data (l, a->dw_attr_val.v.val_wide->elt (i),
				       "%s", name);
		  name = "";
		}
	    else
	      for (i = 0; i < len; ++i)
		{
		  dw2_asm_output_data (l, a->dw_attr_val.v.val_wide->elt (i),
				       "%s", name);
		  name = "";
		}
	  }
	  break;

	case dw_val_class_vec:
	  {
	    unsigned int elt_size = a->dw_attr_val.v.val_vec.elt_size;
	    unsigned int len = a->dw_attr_val.v.val_vec.length;
	    unsigned int i;
	    unsigned char *p;

	    dw2_asm_output_data (constant_size (len * elt_size),
				 len * elt_size, "%s", name);
	    if (elt_size > sizeof (HOST_WIDE_INT))
	      {
		elt_size /= 2;
		len *= 2;
	      }
	    for (i = 0, p = (unsigned char *) a->dw_attr_val.v.val_vec.array;
		 i < len;
		 i++, p += elt_size)
	      dw2_asm_output_data (elt_size, extract_int (p, elt_size),
				   "fp or vector constant word %u", i);
	    break;
	  }

	case dw_val_class_flag:
	  if (dwarf_version >= 4)
	    {
	      /* Currently all add_AT_flag calls pass in 1 as last argument,
		 so DW_FORM_flag_present can be used.  If that ever changes,
		 we'll need to use DW_FORM_flag and have some optimization
		 in build_abbrev_table that will change those to
		 DW_FORM_flag_present if it is set to 1 in all DIEs using
		 the same abbrev entry.  */
	      gcc_assert (AT_flag (a) == 1);
	      if (flag_debug_asm)
		fprintf (asm_out_file, "\t\t\t%s %s\n",
			 ASM_COMMENT_START, name);
	      break;
	    }
	  dw2_asm_output_data (1, AT_flag (a), "%s", name);
	  break;

	case dw_val_class_loc_list:
	  output_loc_list_offset (a);
	  break;

	case dw_val_class_die_ref:
	  if (AT_ref_external (a))
	    {
	      if (AT_ref (a)->comdat_type_p)
	        {
		  comdat_type_node *type_node
		    = AT_ref (a)->die_id.die_type_node;

	          gcc_assert (type_node);
	          output_signature (type_node->signature, name);
	        }
	      else
	        {
		  const char *sym = AT_ref (a)->die_id.die_symbol;
		  int size;

		  gcc_assert (sym);
		  /* In DWARF2, DW_FORM_ref_addr is sized by target address
		     length, whereas in DWARF3 it's always sized as an
		     offset.  */
		  if (dwarf_version == 2)
		    size = DWARF2_ADDR_SIZE;
		  else
		    size = DWARF_OFFSET_SIZE;
		  /* ???  We cannot unconditionally output die_offset if
		     non-zero - others might create references to those
		     DIEs via symbols.
		     And we do not clear its DIE offset after outputting it
		     (and the label refers to the actual DIEs, not the
		     DWARF CU unit header which is when using label + offset
		     would be the correct thing to do).
		     ???  This is the reason for the with_offset flag.  */
		  if (AT_ref (a)->with_offset)
		    dw2_asm_output_offset (size, sym, AT_ref (a)->die_offset,
					   debug_info_section, "%s", name);
		  else
		    dw2_asm_output_offset (size, sym, debug_info_section, "%s",
					   name);
		}
	    }
	  else
	    {
	      gcc_assert (AT_ref (a)->die_offset);
	      dw2_asm_output_data (DWARF_OFFSET_SIZE, AT_ref (a)->die_offset,
				   "%s", name);
	    }
	  break;

	case dw_val_class_fde_ref:
	  {
	    char l1[MAX_ARTIFICIAL_LABEL_BYTES];

	    ASM_GENERATE_INTERNAL_LABEL (l1, FDE_LABEL,
					 a->dw_attr_val.v.val_fde_index * 2);
	    dw2_asm_output_offset (DWARF_OFFSET_SIZE, l1, debug_frame_section,
				   "%s", name);
	  }
	  break;

	case dw_val_class_vms_delta:
#ifdef ASM_OUTPUT_DWARF_VMS_DELTA
	  dw2_asm_output_vms_delta (DWARF_OFFSET_SIZE,
				    AT_vms_delta2 (a), AT_vms_delta1 (a),
				    "%s", name);
#else
	  dw2_asm_output_delta (DWARF_OFFSET_SIZE,
				AT_vms_delta2 (a), AT_vms_delta1 (a),
				"%s", name);
#endif
	  break;

	case dw_val_class_lbl_id:
	  output_attr_index_or_value (a);
	  break;

	case dw_val_class_lineptr:
	  dw2_asm_output_offset (DWARF_OFFSET_SIZE, AT_lbl (a),
				 debug_line_section, "%s", name);
	  break;

	case dw_val_class_macptr:
	  dw2_asm_output_offset (DWARF_OFFSET_SIZE, AT_lbl (a),
				 debug_macinfo_section, "%s", name);
	  break;

	case dw_val_class_loclistsptr:
	  dw2_asm_output_offset (DWARF_OFFSET_SIZE, AT_lbl (a),
				 debug_loc_section, "%s", name);
	  break;

	case dw_val_class_str:
          if (a->dw_attr_val.v.val_str->form == DW_FORM_strp)
            dw2_asm_output_offset (DWARF_OFFSET_SIZE,
                                   a->dw_attr_val.v.val_str->label,
                                   debug_str_section,
                                   "%s: \"%s\"", name, AT_string (a));
	  else if (a->dw_attr_val.v.val_str->form == DW_FORM_line_strp)
	    dw2_asm_output_offset (DWARF_OFFSET_SIZE,
				   a->dw_attr_val.v.val_str->label,
				   debug_line_str_section,
				   "%s: \"%s\"", name, AT_string (a));
          else if (a->dw_attr_val.v.val_str->form == DW_FORM_GNU_str_index)
            dw2_asm_output_data_uleb128 (AT_index (a),
                                         "%s: \"%s\"", name, AT_string (a));
          else
	    dw2_asm_output_nstring (AT_string (a), -1, "%s", name);
	  break;

	case dw_val_class_file:
	  {
	    int f = maybe_emit_file (a->dw_attr_val.v.val_file);

	    dw2_asm_output_data (constant_size (f), f, "%s (%s)", name,
				 a->dw_attr_val.v.val_file->filename);
	    break;
	  }

	case dw_val_class_file_implicit:
	  if (flag_debug_asm)
	    fprintf (asm_out_file, "\t\t\t%s %s (%d, %s)\n",
		     ASM_COMMENT_START, name,
		     maybe_emit_file (a->dw_attr_val.v.val_file),
		     a->dw_attr_val.v.val_file->filename);
	  break;

	case dw_val_class_data8:
	  {
	    int i;

	    for (i = 0; i < 8; i++)
	      dw2_asm_output_data (1, a->dw_attr_val.v.val_data8[i],
				   i == 0 ? "%s" : NULL, name);
	    break;
	  }

	case dw_val_class_high_pc:
	  dw2_asm_output_delta (DWARF2_ADDR_SIZE, AT_lbl (a),
				get_AT_low_pc (die), "DW_AT_high_pc");
	  break;

	case dw_val_class_discr_value:
	  output_discr_value (&a->dw_attr_val.v.val_discr_value, name);
	  break;

	case dw_val_class_discr_list:
	  {
	    dw_discr_list_ref list = AT_discr_list (a);
	    const int size = size_of_discr_list (list);

	    /* This is a block, so output its length first.  */
	    dw2_asm_output_data (constant_size (size), size,
				 "%s: block size", name);

	    for (; list != NULL; list = list->dw_discr_next)
	      {
		/* One byte for the discriminant value descriptor, and then as
		   many LEB128 numbers as required.  */
		if (list->dw_discr_range)
		  dw2_asm_output_data (1, DW_DSC_range,
				       "%s: DW_DSC_range", name);
		else
		  dw2_asm_output_data (1, DW_DSC_label,
				       "%s: DW_DSC_label", name);

		output_discr_value (&list->dw_discr_lower_bound, name);
		if (list->dw_discr_range)
		  output_discr_value (&list->dw_discr_upper_bound, name);
	      }
	    break;
	  }

	default:
	  gcc_unreachable ();
	}
    }

  FOR_EACH_CHILD (die, c, output_die (c));

  /* Add null byte to terminate sibling list.  */
  if (die->die_child != NULL)
    dw2_asm_output_data (1, 0, "end of children of DIE %#lx",
			 (unsigned long) die->die_offset);
}

/* Output the compilation unit that appears at the beginning of the
   .debug_info section, and precedes the DIE descriptions.  */

static void
output_compilation_unit_header (enum dwarf_unit_type ut)
{
  if (!XCOFF_DEBUGGING_INFO)
    {
      if (DWARF_INITIAL_LENGTH_SIZE - DWARF_OFFSET_SIZE == 4)
	dw2_asm_output_data (4, 0xffffffff,
	  "Initial length escape value indicating 64-bit DWARF extension");
      dw2_asm_output_data (DWARF_OFFSET_SIZE,
			   next_die_offset - DWARF_INITIAL_LENGTH_SIZE,
			   "Length of Compilation Unit Info");
    }

  dw2_asm_output_data (2, dwarf_version, "DWARF version number");
  if (dwarf_version >= 5)
    {
      const char *name;
      switch (ut)
	{
	case DW_UT_compile: name = "DW_UT_compile"; break;
	case DW_UT_type: name = "DW_UT_type"; break;
	case DW_UT_split_compile: name = "DW_UT_split_compile"; break;
	case DW_UT_split_type: name = "DW_UT_split_type"; break;
	default: gcc_unreachable ();
	}
      dw2_asm_output_data (1, ut, "%s", name);
      dw2_asm_output_data (1, DWARF2_ADDR_SIZE, "Pointer Size (in bytes)");
    }
  dw2_asm_output_offset (DWARF_OFFSET_SIZE, abbrev_section_label,
			 debug_abbrev_section,
			 "Offset Into Abbrev. Section");
  if (dwarf_version < 5)
    dw2_asm_output_data (1, DWARF2_ADDR_SIZE, "Pointer Size (in bytes)");
}

/* Output the compilation unit DIE and its children.  */

static void
output_comp_unit (dw_die_ref die, int output_if_empty,
		  const unsigned char *dwo_id)
{
  const char *secname, *oldsym;
  char *tmp;

  /* Unless we are outputting main CU, we may throw away empty ones.  */
  if (!output_if_empty && die->die_child == NULL)
    return;

  /* Even if there are no children of this DIE, we must output the information
     about the compilation unit.  Otherwise, on an empty translation unit, we
     will generate a present, but empty, .debug_info section.  IRIX 6.5 `nm'
     will then complain when examining the file.  First mark all the DIEs in
     this CU so we know which get local refs.  */
  mark_dies (die);

  external_ref_hash_type *extern_map = optimize_external_refs (die);

  /* For now, optimize only the main CU, in order to optimize the rest
     we'd need to see all of them earlier.  Leave the rest for post-linking
     tools like DWZ.  */
  if (die == comp_unit_die ())
    abbrev_opt_start = vec_safe_length (abbrev_die_table);

  build_abbrev_table (die, extern_map);

  optimize_abbrev_table ();

  delete extern_map;

  /* Initialize the beginning DIE offset - and calculate sizes/offsets.  */
  next_die_offset = (dwo_id
		     ? DWARF_COMPILE_UNIT_SKELETON_HEADER_SIZE
		     : DWARF_COMPILE_UNIT_HEADER_SIZE);
  calc_die_sizes (die);

  oldsym = die->die_id.die_symbol;
  if (oldsym && die->comdat_type_p)
    {
      tmp = XALLOCAVEC (char, strlen (oldsym) + 24);

      sprintf (tmp, ".gnu.linkonce.wi.%s", oldsym);
      secname = tmp;
      die->die_id.die_symbol = NULL;
      switch_to_section (get_section (secname, SECTION_DEBUG, NULL));
    }
  else
    {
      switch_to_section (debug_info_section);
      ASM_OUTPUT_LABEL (asm_out_file, debug_info_section_label);
      info_section_emitted = true;
    }

  /* For LTO cross unit DIE refs we want a symbol on the start of the
     debuginfo section, not on the CU DIE.  */
  if ((flag_generate_lto || flag_generate_offload) && oldsym)
    {
      /* ???  No way to get visibility assembled without a decl.  */
      tree decl = build_decl (UNKNOWN_LOCATION, VAR_DECL,
			      get_identifier (oldsym), char_type_node);
      TREE_PUBLIC (decl) = true;
      TREE_STATIC (decl) = true;
      DECL_ARTIFICIAL (decl) = true;
      DECL_VISIBILITY (decl) = VISIBILITY_HIDDEN;
      DECL_VISIBILITY_SPECIFIED (decl) = true;
      targetm.asm_out.assemble_visibility (decl, VISIBILITY_HIDDEN);
#ifdef ASM_WEAKEN_LABEL
      /* We prefer a .weak because that handles duplicates from duplicate
         archive members in a graceful way.  */
      ASM_WEAKEN_LABEL (asm_out_file, oldsym);
#else
      targetm.asm_out.globalize_label (asm_out_file, oldsym);
#endif
      ASM_OUTPUT_LABEL (asm_out_file, oldsym);
    }

  /* Output debugging information.  */
  output_compilation_unit_header (dwo_id
				  ? DW_UT_split_compile : DW_UT_compile);
  if (dwarf_version >= 5)
    {
      if (dwo_id != NULL)
	for (int i = 0; i < 8; i++)
	  dw2_asm_output_data (1, dwo_id[i], i == 0 ? "DWO id" : NULL);
    }
  output_die (die);

  /* Leave the marks on the main CU, so we can check them in
     output_pubnames.  */
  if (oldsym)
    {
      unmark_dies (die);
      die->die_id.die_symbol = oldsym;
    }
}

/* Whether to generate the DWARF accelerator tables in .debug_pubnames
   and .debug_pubtypes.  This is configured per-target, but can be
   overridden by the -gpubnames or -gno-pubnames options.  */

static inline bool
want_pubnames (void)
{
  if (debug_info_level <= DINFO_LEVEL_TERSE)
    return false;
  if (debug_generate_pub_sections != -1)
    return debug_generate_pub_sections;
  return targetm.want_debug_pub_sections;
}

/* Add the DW_AT_GNU_pubnames and DW_AT_GNU_pubtypes attributes.  */

static void
add_AT_pubnames (dw_die_ref die)
{
  if (want_pubnames ())
    add_AT_flag (die, DW_AT_GNU_pubnames, 1);
}

/* Add a string attribute value to a skeleton DIE.  */

static inline void
add_skeleton_AT_string (dw_die_ref die, enum dwarf_attribute attr_kind,
                        const char *str)
{
  dw_attr_node attr;
  struct indirect_string_node *node;

  if (! skeleton_debug_str_hash)
    skeleton_debug_str_hash
      = hash_table<indirect_string_hasher>::create_ggc (10);

  node = find_AT_string_in_table (str, skeleton_debug_str_hash);
  find_string_form (node);
  if (node->form == DW_FORM_GNU_str_index)
    node->form = DW_FORM_strp;

  attr.dw_attr = attr_kind;
  attr.dw_attr_val.val_class = dw_val_class_str;
  attr.dw_attr_val.val_entry = NULL;
  attr.dw_attr_val.v.val_str = node;
  add_dwarf_attr (die, &attr);
}

/* Helper function to generate top-level dies for skeleton debug_info and
   debug_types.  */

static void
add_top_level_skeleton_die_attrs (dw_die_ref die)
{
  const char *dwo_file_name = concat (aux_base_name, ".dwo", NULL);
  const char *comp_dir = comp_dir_string ();

  add_skeleton_AT_string (die, dwarf_AT (DW_AT_dwo_name), dwo_file_name);
  if (comp_dir != NULL)
    add_skeleton_AT_string (die, DW_AT_comp_dir, comp_dir);
  add_AT_pubnames (die);
  add_AT_lineptr (die, DW_AT_GNU_addr_base, debug_addr_section_label);
}

/* Output skeleton debug sections that point to the dwo file.  */

static void
output_skeleton_debug_sections (dw_die_ref comp_unit,
				const unsigned char *dwo_id)
{
  /* These attributes will be found in the full debug_info section.  */
  remove_AT (comp_unit, DW_AT_producer);
  remove_AT (comp_unit, DW_AT_language);

  switch_to_section (debug_skeleton_info_section);
  ASM_OUTPUT_LABEL (asm_out_file, debug_skeleton_info_section_label);

  /* Produce the skeleton compilation-unit header.  This one differs enough from
     a normal CU header that it's better not to call output_compilation_unit
     header.  */
  if (DWARF_INITIAL_LENGTH_SIZE - DWARF_OFFSET_SIZE == 4)
    dw2_asm_output_data (4, 0xffffffff,
			 "Initial length escape value indicating 64-bit "
			 "DWARF extension");

  dw2_asm_output_data (DWARF_OFFSET_SIZE,
		       DWARF_COMPILE_UNIT_SKELETON_HEADER_SIZE
                       - DWARF_INITIAL_LENGTH_SIZE
                       + size_of_die (comp_unit),
                      "Length of Compilation Unit Info");
  dw2_asm_output_data (2, dwarf_version, "DWARF version number");
  if (dwarf_version >= 5)
    {
      dw2_asm_output_data (1, DW_UT_skeleton, "DW_UT_skeleton");
      dw2_asm_output_data (1, DWARF2_ADDR_SIZE, "Pointer Size (in bytes)");
    }
  dw2_asm_output_offset (DWARF_OFFSET_SIZE, debug_skeleton_abbrev_section_label,
			 debug_skeleton_abbrev_section,
                         "Offset Into Abbrev. Section");
  if (dwarf_version < 5)
    dw2_asm_output_data (1, DWARF2_ADDR_SIZE, "Pointer Size (in bytes)");
  else
    for (int i = 0; i < 8; i++)
      dw2_asm_output_data (1, dwo_id[i], i == 0 ? "DWO id" : NULL);

  comp_unit->die_abbrev = SKELETON_COMP_DIE_ABBREV;
  output_die (comp_unit);

  /* Build the skeleton debug_abbrev section.  */
  switch_to_section (debug_skeleton_abbrev_section);
  ASM_OUTPUT_LABEL (asm_out_file, debug_skeleton_abbrev_section_label);

  output_die_abbrevs (SKELETON_COMP_DIE_ABBREV, comp_unit);

  dw2_asm_output_data (1, 0, "end of skeleton .debug_abbrev");
}

/* Output a comdat type unit DIE and its children.  */

static void
output_comdat_type_unit (comdat_type_node *node)
{
  const char *secname;
  char *tmp;
  int i;
#if defined (OBJECT_FORMAT_ELF)
  tree comdat_key;
#endif

  /* First mark all the DIEs in this CU so we know which get local refs.  */
  mark_dies (node->root_die);

  external_ref_hash_type *extern_map = optimize_external_refs (node->root_die);

  build_abbrev_table (node->root_die, extern_map);

  delete extern_map;
  extern_map = NULL;

  /* Initialize the beginning DIE offset - and calculate sizes/offsets.  */
  next_die_offset = DWARF_COMDAT_TYPE_UNIT_HEADER_SIZE;
  calc_die_sizes (node->root_die);

#if defined (OBJECT_FORMAT_ELF)
  if (dwarf_version >= 5)
    {
      if (!dwarf_split_debug_info)
	secname = ".debug_info";
      else
	secname = ".debug_info.dwo";
    }
  else if (!dwarf_split_debug_info)
    secname = ".debug_types";
  else
    secname = ".debug_types.dwo";

  tmp = XALLOCAVEC (char, 4 + DWARF_TYPE_SIGNATURE_SIZE * 2);
  sprintf (tmp, dwarf_version >= 5 ? "wi." : "wt.");
  for (i = 0; i < DWARF_TYPE_SIGNATURE_SIZE; i++)
    sprintf (tmp + 3 + i * 2, "%02x", node->signature[i] & 0xff);
  comdat_key = get_identifier (tmp);
  targetm.asm_out.named_section (secname,
                                 SECTION_DEBUG | SECTION_LINKONCE,
                                 comdat_key);
#else
  tmp = XALLOCAVEC (char, 18 + DWARF_TYPE_SIGNATURE_SIZE * 2);
  sprintf (tmp, (dwarf_version >= 5
		 ? ".gnu.linkonce.wi." : ".gnu.linkonce.wt."));
  for (i = 0; i < DWARF_TYPE_SIGNATURE_SIZE; i++)
    sprintf (tmp + 17 + i * 2, "%02x", node->signature[i] & 0xff);
  secname = tmp;
  switch_to_section (get_section (secname, SECTION_DEBUG, NULL));
#endif

  /* Output debugging information.  */
  output_compilation_unit_header (dwarf_split_debug_info
				  ? DW_UT_split_type : DW_UT_type);
  output_signature (node->signature, "Type Signature");
  dw2_asm_output_data (DWARF_OFFSET_SIZE, node->type_die->die_offset,
		       "Offset to Type DIE");
  output_die (node->root_die);

  unmark_dies (node->root_die);
}

/* Return the DWARF2/3 pubname associated with a decl.  */

static const char *
dwarf2_name (tree decl, int scope)
{
  if (DECL_NAMELESS (decl))
    return NULL;
  return lang_hooks.dwarf_name (decl, scope ? 1 : 0);
}

/* Add a new entry to .debug_pubnames if appropriate.  */

static void
add_pubname_string (const char *str, dw_die_ref die)
{
  pubname_entry e;

  e.die = die;
  e.name = xstrdup (str);
  vec_safe_push (pubname_table, e);
}

static void
add_pubname (tree decl, dw_die_ref die)
{
  if (!want_pubnames ())
    return;

  /* Don't add items to the table when we expect that the consumer will have
     just read the enclosing die.  For example, if the consumer is looking at a
     class_member, it will either be inside the class already, or will have just
     looked up the class to find the member.  Either way, searching the class is
     faster than searching the index.  */
  if ((TREE_PUBLIC (decl) && !class_scope_p (die->die_parent))
      || is_cu_die (die->die_parent) || is_namespace_die (die->die_parent))
    {
      const char *name = dwarf2_name (decl, 1);

      if (name)
	add_pubname_string (name, die);
    }
}

/* Add an enumerator to the pubnames section.  */

static void
add_enumerator_pubname (const char *scope_name, dw_die_ref die)
{
  pubname_entry e;

  gcc_assert (scope_name);
  e.name = concat (scope_name, get_AT_string (die, DW_AT_name), NULL);
  e.die = die;
  vec_safe_push (pubname_table, e);
}

/* Add a new entry to .debug_pubtypes if appropriate.  */

static void
add_pubtype (tree decl, dw_die_ref die)
{
  pubname_entry e;

  if (!want_pubnames ())
    return;

  if ((TREE_PUBLIC (decl)
       || is_cu_die (die->die_parent) || is_namespace_die (die->die_parent))
      && (die->die_tag == DW_TAG_typedef || COMPLETE_TYPE_P (decl)))
    {
      tree scope = NULL;
      const char *scope_name = "";
      const char *sep = is_cxx () ? "::" : ".";
      const char *name;

      scope = TYPE_P (decl) ? TYPE_CONTEXT (decl) : NULL;
      if (scope && TREE_CODE (scope) == NAMESPACE_DECL)
        {
          scope_name = lang_hooks.dwarf_name (scope, 1);
          if (scope_name != NULL && scope_name[0] != '\0')
            scope_name = concat (scope_name, sep, NULL);
          else
            scope_name = "";
	}

      if (TYPE_P (decl))
        name = type_tag (decl);
      else
        name = lang_hooks.dwarf_name (decl, 1);

      /* If we don't have a name for the type, there's no point in adding
	 it to the table.  */
      if (name != NULL && name[0] != '\0')
        {
          e.die = die;
          e.name = concat (scope_name, name, NULL);
          vec_safe_push (pubtype_table, e);
        }

      /* Although it might be more consistent to add the pubinfo for the
         enumerators as their dies are created, they should only be added if the
         enum type meets the criteria above.  So rather than re-check the parent
         enum type whenever an enumerator die is created, just output them all
         here.  This isn't protected by the name conditional because anonymous
         enums don't have names.  */
      if (die->die_tag == DW_TAG_enumeration_type)
        {
          dw_die_ref c;

          FOR_EACH_CHILD (die, c, add_enumerator_pubname (scope_name, c));
        }
    }
}

/* Output a single entry in the pubnames table.  */

static void
output_pubname (dw_offset die_offset, pubname_entry *entry)
{
  dw_die_ref die = entry->die;
  int is_static = get_AT_flag (die, DW_AT_external) ? 0 : 1;

  dw2_asm_output_data (DWARF_OFFSET_SIZE, die_offset, "DIE offset");

  if (debug_generate_pub_sections == 2)
    {
      /* This logic follows gdb's method for determining the value of the flag
         byte.  */
      uint32_t flags = GDB_INDEX_SYMBOL_KIND_NONE;
      switch (die->die_tag)
      {
        case DW_TAG_typedef:
        case DW_TAG_base_type:
        case DW_TAG_subrange_type:
          GDB_INDEX_SYMBOL_KIND_SET_VALUE(flags, GDB_INDEX_SYMBOL_KIND_TYPE);
          GDB_INDEX_SYMBOL_STATIC_SET_VALUE(flags, 1);
          break;
        case DW_TAG_enumerator:
          GDB_INDEX_SYMBOL_KIND_SET_VALUE(flags,
                                          GDB_INDEX_SYMBOL_KIND_VARIABLE);
	  if (!is_cxx ())
	    GDB_INDEX_SYMBOL_STATIC_SET_VALUE(flags, 1);
          break;
        case DW_TAG_subprogram:
          GDB_INDEX_SYMBOL_KIND_SET_VALUE(flags,
                                          GDB_INDEX_SYMBOL_KIND_FUNCTION);
          if (!is_ada ())
            GDB_INDEX_SYMBOL_STATIC_SET_VALUE(flags, is_static);
          break;
        case DW_TAG_constant:
          GDB_INDEX_SYMBOL_KIND_SET_VALUE(flags,
                                          GDB_INDEX_SYMBOL_KIND_VARIABLE);
          GDB_INDEX_SYMBOL_STATIC_SET_VALUE(flags, is_static);
          break;
        case DW_TAG_variable:
          GDB_INDEX_SYMBOL_KIND_SET_VALUE(flags,
                                          GDB_INDEX_SYMBOL_KIND_VARIABLE);
          GDB_INDEX_SYMBOL_STATIC_SET_VALUE(flags, is_static);
          break;
        case DW_TAG_namespace:
        case DW_TAG_imported_declaration:
          GDB_INDEX_SYMBOL_KIND_SET_VALUE(flags, GDB_INDEX_SYMBOL_KIND_TYPE);
          break;
        case DW_TAG_class_type:
        case DW_TAG_interface_type:
        case DW_TAG_structure_type:
        case DW_TAG_union_type:
        case DW_TAG_enumeration_type:
          GDB_INDEX_SYMBOL_KIND_SET_VALUE(flags, GDB_INDEX_SYMBOL_KIND_TYPE);
	  if (!is_cxx ())
	    GDB_INDEX_SYMBOL_STATIC_SET_VALUE(flags, 1);
          break;
        default:
          /* An unusual tag.  Leave the flag-byte empty.  */
          break;
      }
      dw2_asm_output_data (1, flags >> GDB_INDEX_CU_BITSIZE,
                           "GDB-index flags");
    }

  dw2_asm_output_nstring (entry->name, -1, "external name");
}


/* Output the public names table used to speed up access to externally
   visible names; or the public types table used to find type definitions.  */

static void
output_pubnames (vec<pubname_entry, va_gc> *names)
{
  unsigned i;
  unsigned long pubnames_length = size_of_pubnames (names);
  pubname_entry *pub;

  if (!XCOFF_DEBUGGING_INFO)
    {
      if (DWARF_INITIAL_LENGTH_SIZE - DWARF_OFFSET_SIZE == 4)
	dw2_asm_output_data (4, 0xffffffff,
	  "Initial length escape value indicating 64-bit DWARF extension");
      dw2_asm_output_data (DWARF_OFFSET_SIZE, pubnames_length,
			   "Pub Info Length");
    }

  /* Version number for pubnames/pubtypes is independent of dwarf version.  */
  dw2_asm_output_data (2, 2, "DWARF Version");

  if (dwarf_split_debug_info)
    dw2_asm_output_offset (DWARF_OFFSET_SIZE, debug_skeleton_info_section_label,
                           debug_skeleton_info_section,
                           "Offset of Compilation Unit Info");
  else
    dw2_asm_output_offset (DWARF_OFFSET_SIZE, debug_info_section_label,
                           debug_info_section,
                           "Offset of Compilation Unit Info");
  dw2_asm_output_data (DWARF_OFFSET_SIZE, next_die_offset,
		       "Compilation Unit Length");

  FOR_EACH_VEC_ELT (*names, i, pub)
    {
      if (include_pubname_in_output (names, pub))
	{
	  dw_offset die_offset = pub->die->die_offset;

          /* We shouldn't see pubnames for DIEs outside of the main CU.  */
          if (names == pubname_table && pub->die->die_tag != DW_TAG_enumerator)
            gcc_assert (pub->die->die_mark);

	  /* If we're putting types in their own .debug_types sections,
	     the .debug_pubtypes table will still point to the compile
	     unit (not the type unit), so we want to use the offset of
	     the skeleton DIE (if there is one).  */
	  if (pub->die->comdat_type_p && names == pubtype_table)
	    {
	      comdat_type_node *type_node = pub->die->die_id.die_type_node;

	      if (type_node != NULL)
	        die_offset = (type_node->skeleton_die != NULL
			      ? type_node->skeleton_die->die_offset
			      : comp_unit_die ()->die_offset);
	    }

          output_pubname (die_offset, pub);
	}
    }

  dw2_asm_output_data (DWARF_OFFSET_SIZE, 0, NULL);
}

/* Output public names and types tables if necessary.  */

static void
output_pubtables (void)
{
  if (!want_pubnames () || !info_section_emitted)
    return;

  switch_to_section (debug_pubnames_section);
  output_pubnames (pubname_table);
  /* ??? Only defined by DWARF3, but emitted by Darwin for DWARF2.
     It shouldn't hurt to emit it always, since pure DWARF2 consumers
     simply won't look for the section.  */
  switch_to_section (debug_pubtypes_section);
  output_pubnames (pubtype_table);
}


/* Output the information that goes into the .debug_aranges table.
   Namely, define the beginning and ending address range of the
   text section generated for this compilation unit.  */

static void
output_aranges (void)
{
  unsigned i;
  unsigned long aranges_length = size_of_aranges ();
  
  if (!XCOFF_DEBUGGING_INFO)
    {
      if (DWARF_INITIAL_LENGTH_SIZE - DWARF_OFFSET_SIZE == 4)
	dw2_asm_output_data (4, 0xffffffff,
	  "Initial length escape value indicating 64-bit DWARF extension");
      dw2_asm_output_data (DWARF_OFFSET_SIZE, aranges_length,
			   "Length of Address Ranges Info");
    }

  /* Version number for aranges is still 2, even up to DWARF5.  */
  dw2_asm_output_data (2, 2, "DWARF Version");
  if (dwarf_split_debug_info)
    dw2_asm_output_offset (DWARF_OFFSET_SIZE, debug_skeleton_info_section_label,
                           debug_skeleton_info_section,
                           "Offset of Compilation Unit Info");
  else
    dw2_asm_output_offset (DWARF_OFFSET_SIZE, debug_info_section_label,
                           debug_info_section,
                           "Offset of Compilation Unit Info");
  dw2_asm_output_data (1, DWARF2_ADDR_SIZE, "Size of Address");
  dw2_asm_output_data (1, 0, "Size of Segment Descriptor");

  /* We need to align to twice the pointer size here.  */
  if (DWARF_ARANGES_PAD_SIZE)
    {
      /* Pad using a 2 byte words so that padding is correct for any
	 pointer size.  */
      dw2_asm_output_data (2, 0, "Pad to %d byte boundary",
			   2 * DWARF2_ADDR_SIZE);
      for (i = 2; i < (unsigned) DWARF_ARANGES_PAD_SIZE; i += 2)
	dw2_asm_output_data (2, 0, NULL);
    }

  /* It is necessary not to output these entries if the sections were
     not used; if the sections were not used, the length will be 0 and
     the address may end up as 0 if the section is discarded by ld
     --gc-sections, leaving an invalid (0, 0) entry that can be
     confused with the terminator.  */
  if (text_section_used)
    {
      dw2_asm_output_addr (DWARF2_ADDR_SIZE, text_section_label, "Address");
      dw2_asm_output_delta (DWARF2_ADDR_SIZE, text_end_label,
			    text_section_label, "Length");
    }
  if (cold_text_section_used)
    {
      dw2_asm_output_addr (DWARF2_ADDR_SIZE, cold_text_section_label,
			   "Address");
      dw2_asm_output_delta (DWARF2_ADDR_SIZE, cold_end_label,
			    cold_text_section_label, "Length");
    }

  if (have_multiple_function_sections)
    {
      unsigned fde_idx;
      dw_fde_ref fde;

      FOR_EACH_VEC_ELT (*fde_vec, fde_idx, fde)
	{
	  if (DECL_IGNORED_P (fde->decl))
	    continue;
	  if (!fde->in_std_section)
	    {
	      dw2_asm_output_addr (DWARF2_ADDR_SIZE, fde->dw_fde_begin,
				   "Address");
	      dw2_asm_output_delta (DWARF2_ADDR_SIZE, fde->dw_fde_end,
				    fde->dw_fde_begin, "Length");
	    }
	  if (fde->dw_fde_second_begin && !fde->second_in_std_section)
	    {
	      dw2_asm_output_addr (DWARF2_ADDR_SIZE, fde->dw_fde_second_begin,
				   "Address");
	      dw2_asm_output_delta (DWARF2_ADDR_SIZE, fde->dw_fde_second_end,
				    fde->dw_fde_second_begin, "Length");
	    }
	}
    }

  /* Output the terminator words.  */
  dw2_asm_output_data (DWARF2_ADDR_SIZE, 0, NULL);
  dw2_asm_output_data (DWARF2_ADDR_SIZE, 0, NULL);
}

/* Add a new entry to .debug_ranges.  Return its index into
   ranges_table vector.  */

static unsigned int
add_ranges_num (int num, bool maybe_new_sec)
{
  dw_ranges r = { NULL, num, 0, maybe_new_sec };
  vec_safe_push (ranges_table, r);
  return vec_safe_length (ranges_table) - 1;
}

/* Add a new entry to .debug_ranges corresponding to a block, or a
   range terminator if BLOCK is NULL.  MAYBE_NEW_SEC is true if
   this entry might be in a different section from previous range.  */

static unsigned int
add_ranges (const_tree block, bool maybe_new_sec)
{
  return add_ranges_num (block ? BLOCK_NUMBER (block) : 0, maybe_new_sec);
}

/* Note that (*rnglist_table)[offset] is either a head of a rnglist
   chain, or middle entry of a chain that will be directly referred to.  */

static void
note_rnglist_head (unsigned int offset)
{
  if (dwarf_version < 5 || (*ranges_table)[offset].label)
    return;
  (*ranges_table)[offset].label = gen_internal_sym ("LLRL");
}

/* Add a new entry to .debug_ranges corresponding to a pair of labels.
   When using dwarf_split_debug_info, address attributes in dies destined
   for the final executable should be direct references--setting the
   parameter force_direct ensures this behavior.  */

static void
add_ranges_by_labels (dw_die_ref die, const char *begin, const char *end,
                      bool *added, bool force_direct)
{
  unsigned int in_use = vec_safe_length (ranges_by_label);
  unsigned int offset;
  dw_ranges_by_label rbl = { begin, end };
  vec_safe_push (ranges_by_label, rbl);
  offset = add_ranges_num (-(int)in_use - 1, true);
  if (!*added)
    {
      add_AT_range_list (die, DW_AT_ranges, offset, force_direct);
      *added = true;
      note_rnglist_head (offset);
    }
}

/* Emit .debug_ranges section.  */

static void
output_ranges (void)
{
  unsigned i;
  static const char *const start_fmt = "Offset %#x";
  const char *fmt = start_fmt;
  dw_ranges *r;

  switch_to_section (debug_ranges_section);
  ASM_OUTPUT_LABEL (asm_out_file, ranges_section_label);
  FOR_EACH_VEC_SAFE_ELT (ranges_table, i, r)
    {
      int block_num = r->num;

      if (block_num > 0)
	{
	  char blabel[MAX_ARTIFICIAL_LABEL_BYTES];
	  char elabel[MAX_ARTIFICIAL_LABEL_BYTES];

	  ASM_GENERATE_INTERNAL_LABEL (blabel, BLOCK_BEGIN_LABEL, block_num);
	  ASM_GENERATE_INTERNAL_LABEL (elabel, BLOCK_END_LABEL, block_num);

	  /* If all code is in the text section, then the compilation
	     unit base address defaults to DW_AT_low_pc, which is the
	     base of the text section.  */
	  if (!have_multiple_function_sections)
	    {
	      dw2_asm_output_delta (DWARF2_ADDR_SIZE, blabel,
				    text_section_label,
				    fmt, i * 2 * DWARF2_ADDR_SIZE);
	      dw2_asm_output_delta (DWARF2_ADDR_SIZE, elabel,
				    text_section_label, NULL);
	    }

	  /* Otherwise, the compilation unit base address is zero,
	     which allows us to use absolute addresses, and not worry
	     about whether the target supports cross-section
	     arithmetic.  */
	  else
	    {
	      dw2_asm_output_addr (DWARF2_ADDR_SIZE, blabel,
				   fmt, i * 2 * DWARF2_ADDR_SIZE);
	      dw2_asm_output_addr (DWARF2_ADDR_SIZE, elabel, NULL);
	    }

	  fmt = NULL;
	}

      /* Negative block_num stands for an index into ranges_by_label.  */
      else if (block_num < 0)
	{
	  int lab_idx = - block_num - 1;

	  if (!have_multiple_function_sections)
	    {
	      gcc_unreachable ();
#if 0
	      /* If we ever use add_ranges_by_labels () for a single
		 function section, all we have to do is to take out
		 the #if 0 above.  */
	      dw2_asm_output_delta (DWARF2_ADDR_SIZE,
				    (*ranges_by_label)[lab_idx].begin,
				    text_section_label,
				    fmt, i * 2 * DWARF2_ADDR_SIZE);
	      dw2_asm_output_delta (DWARF2_ADDR_SIZE,
				    (*ranges_by_label)[lab_idx].end,
				    text_section_label, NULL);
#endif
	    }
	  else
	    {
	      dw2_asm_output_addr (DWARF2_ADDR_SIZE,
				   (*ranges_by_label)[lab_idx].begin,
				   fmt, i * 2 * DWARF2_ADDR_SIZE);
	      dw2_asm_output_addr (DWARF2_ADDR_SIZE,
				   (*ranges_by_label)[lab_idx].end,
				   NULL);
	    }
	}
      else
	{
	  dw2_asm_output_data (DWARF2_ADDR_SIZE, 0, NULL);
	  dw2_asm_output_data (DWARF2_ADDR_SIZE, 0, NULL);
	  fmt = start_fmt;
	}
    }
}

/* Non-zero if .debug_line_str should be used for .debug_line section
   strings or strings that are likely shareable with those.  */
#define DWARF5_USE_DEBUG_LINE_STR \
  (!DWARF2_INDIRECT_STRING_SUPPORT_MISSING_ON_TARGET		\
   && (DEBUG_STR_SECTION_FLAGS & SECTION_MERGE) != 0		\
   /* FIXME: there is no .debug_line_str.dwo section,		\
      for -gsplit-dwarf we should use DW_FORM_strx instead.  */	\
   && !dwarf_split_debug_info)

/* Assign .debug_rnglists indexes.  */

static void
index_rnglists (void)
{
  unsigned i;
  dw_ranges *r;

  FOR_EACH_VEC_SAFE_ELT (ranges_table, i, r)
    if (r->label)
      r->idx = rnglist_idx++;
}

/* Emit .debug_rnglists section.  */

static void
output_rnglists (void)
{
  unsigned i;
  dw_ranges *r;
  char l1[MAX_ARTIFICIAL_LABEL_BYTES];
  char l2[MAX_ARTIFICIAL_LABEL_BYTES];
  char basebuf[MAX_ARTIFICIAL_LABEL_BYTES];

  switch_to_section (debug_ranges_section);
  ASM_OUTPUT_LABEL (asm_out_file, ranges_section_label);
  ASM_GENERATE_INTERNAL_LABEL (l1, DEBUG_RANGES_SECTION_LABEL, 2);
  ASM_GENERATE_INTERNAL_LABEL (l2, DEBUG_RANGES_SECTION_LABEL, 3);
  if (DWARF_INITIAL_LENGTH_SIZE - DWARF_OFFSET_SIZE == 4)
    dw2_asm_output_data (4, 0xffffffff,
			 "Initial length escape value indicating "
			 "64-bit DWARF extension");
  dw2_asm_output_delta (DWARF_OFFSET_SIZE, l2, l1,
			"Length of Range Lists");
  ASM_OUTPUT_LABEL (asm_out_file, l1);
  dw2_asm_output_data (2, dwarf_version, "DWARF Version");
  dw2_asm_output_data (1, DWARF2_ADDR_SIZE, "Address Size");
  dw2_asm_output_data (1, 0, "Segment Size");
  /* Emit the offset table only for -gsplit-dwarf.  If we don't care
     about relocation sizes and primarily care about the size of .debug*
     sections in linked shared libraries and executables, then
     the offset table plus corresponding DW_FORM_rnglistx uleb128 indexes
     into it are usually larger than just DW_FORM_sec_offset offsets
     into the .debug_rnglists section.  */
  dw2_asm_output_data (4, dwarf_split_debug_info ? rnglist_idx : 0,
		       "Offset Entry Count");
  if (dwarf_split_debug_info)
    {
      ASM_OUTPUT_LABEL (asm_out_file, ranges_base_label);
      FOR_EACH_VEC_SAFE_ELT (ranges_table, i, r)
	if (r->label)
	  dw2_asm_output_delta (DWARF_OFFSET_SIZE, r->label,
				ranges_base_label, NULL);
    }

  const char *lab = "";
  unsigned int len = vec_safe_length (ranges_table);
  const char *base = NULL;
  FOR_EACH_VEC_SAFE_ELT (ranges_table, i, r)
    {
      int block_num = r->num;

      if (r->label)
	{
	  ASM_OUTPUT_LABEL (asm_out_file, r->label);
	  lab = r->label;
	}
      if (HAVE_AS_LEB128 && (r->label || r->maybe_new_sec))
	base = NULL;
      if (block_num > 0)
	{
	  char blabel[MAX_ARTIFICIAL_LABEL_BYTES];
	  char elabel[MAX_ARTIFICIAL_LABEL_BYTES];

	  ASM_GENERATE_INTERNAL_LABEL (blabel, BLOCK_BEGIN_LABEL, block_num);
	  ASM_GENERATE_INTERNAL_LABEL (elabel, BLOCK_END_LABEL, block_num);

	  if (HAVE_AS_LEB128)
	    {
	      /* If all code is in the text section, then the compilation
		 unit base address defaults to DW_AT_low_pc, which is the
		 base of the text section.  */
	      if (!have_multiple_function_sections)
		{
		  dw2_asm_output_data (1, DW_RLE_offset_pair,
				       "DW_RLE_offset_pair (%s)", lab);
		  dw2_asm_output_delta_uleb128 (blabel, text_section_label,
						"Range begin address (%s)", lab);
		  dw2_asm_output_delta_uleb128 (elabel, text_section_label,
						"Range end address (%s)", lab);
		  continue;
		}
	      if (base == NULL)
		{
		  dw_ranges *r2 = NULL;
		  if (i < len - 1)
		    r2 = &(*ranges_table)[i + 1];
		  if (r2
		      && r2->num != 0
		      && r2->label == NULL
		      && !r2->maybe_new_sec)
		    {
		      dw2_asm_output_data (1, DW_RLE_base_address,
					   "DW_RLE_base_address (%s)", lab);
		      dw2_asm_output_addr (DWARF2_ADDR_SIZE, blabel,
					   "Base address (%s)", lab);
		      strcpy (basebuf, blabel);
		      base = basebuf;
		    }
		}
	      if (base)
		{
		  dw2_asm_output_data (1, DW_RLE_offset_pair,
				       "DW_RLE_offset_pair (%s)", lab);
		  dw2_asm_output_delta_uleb128 (blabel, base,
						"Range begin address (%s)", lab);
		  dw2_asm_output_delta_uleb128 (elabel, base,
						"Range end address (%s)", lab);
		  continue;
		}
	      dw2_asm_output_data (1, DW_RLE_start_length,
				   "DW_RLE_start_length (%s)", lab);
	      dw2_asm_output_addr (DWARF2_ADDR_SIZE, blabel,
				   "Range begin address (%s)", lab);
	      dw2_asm_output_delta_uleb128 (elabel, blabel,
					    "Range length (%s)", lab);
	    }
	  else
	    {
	      dw2_asm_output_data (1, DW_RLE_start_end,
				   "DW_RLE_start_end (%s)", lab);
	      dw2_asm_output_addr (DWARF2_ADDR_SIZE, blabel,
				   "Range begin address (%s)", lab);
	      dw2_asm_output_addr (DWARF2_ADDR_SIZE, elabel,
				   "Range end address (%s)", lab);
	    }
	}

      /* Negative block_num stands for an index into ranges_by_label.  */
      else if (block_num < 0)
	{
	  int lab_idx = - block_num - 1;
	  const char *blabel = (*ranges_by_label)[lab_idx].begin;
	  const char *elabel = (*ranges_by_label)[lab_idx].end;

	  if (!have_multiple_function_sections)
	    gcc_unreachable ();
	  if (HAVE_AS_LEB128)
	    {
	      dw2_asm_output_data (1, DW_RLE_start_length,
				   "DW_RLE_start_length (%s)", lab);
	      dw2_asm_output_addr (DWARF2_ADDR_SIZE, blabel,
				   "Range begin address (%s)", lab);
	      dw2_asm_output_delta_uleb128 (elabel, blabel,
					    "Range length (%s)", lab);
	    }
	  else
	    {
	      dw2_asm_output_data (1, DW_RLE_start_end,
				   "DW_RLE_start_end (%s)", lab);
	      dw2_asm_output_addr (DWARF2_ADDR_SIZE, blabel,
				   "Range begin address (%s)", lab);
	      dw2_asm_output_addr (DWARF2_ADDR_SIZE, elabel,
				   "Range end address (%s)", lab);
	    }
	}
      else
	dw2_asm_output_data (1, DW_RLE_end_of_list,
			     "DW_RLE_end_of_list (%s)", lab);
    }
  ASM_OUTPUT_LABEL (asm_out_file, l2);
}

/* Data structure containing information about input files.  */
struct file_info
{
  const char *path;	/* Complete file name.  */
  const char *fname;	/* File name part.  */
  int length;		/* Length of entire string.  */
  struct dwarf_file_data * file_idx;	/* Index in input file table.  */
  int dir_idx;		/* Index in directory table.  */
};

/* Data structure containing information about directories with source
   files.  */
struct dir_info
{
  const char *path;	/* Path including directory name.  */
  int length;		/* Path length.  */
  int prefix;		/* Index of directory entry which is a prefix.  */
  int count;		/* Number of files in this directory.  */
  int dir_idx;		/* Index of directory used as base.  */
};

/* Callback function for file_info comparison.  We sort by looking at
   the directories in the path.  */

static int
file_info_cmp (const void *p1, const void *p2)
{
  const struct file_info *const s1 = (const struct file_info *) p1;
  const struct file_info *const s2 = (const struct file_info *) p2;
  const unsigned char *cp1;
  const unsigned char *cp2;

  /* Take care of file names without directories.  We need to make sure that
     we return consistent values to qsort since some will get confused if
     we return the same value when identical operands are passed in opposite
     orders.  So if neither has a directory, return 0 and otherwise return
     1 or -1 depending on which one has the directory.  */
  if ((s1->path == s1->fname || s2->path == s2->fname))
    return (s2->path == s2->fname) - (s1->path == s1->fname);

  cp1 = (const unsigned char *) s1->path;
  cp2 = (const unsigned char *) s2->path;

  while (1)
    {
      ++cp1;
      ++cp2;
      /* Reached the end of the first path?  If so, handle like above.  */
      if ((cp1 == (const unsigned char *) s1->fname)
	  || (cp2 == (const unsigned char *) s2->fname))
	return ((cp2 == (const unsigned char *) s2->fname)
		- (cp1 == (const unsigned char *) s1->fname));

      /* Character of current path component the same?  */
      else if (*cp1 != *cp2)
	return *cp1 - *cp2;
    }
}

struct file_name_acquire_data
{
  struct file_info *files;
  int used_files;
  int max_files;
};

/* Traversal function for the hash table.  */

int
file_name_acquire (dwarf_file_data **slot, file_name_acquire_data *fnad)
{
  struct dwarf_file_data *d = *slot;
  struct file_info *fi;
  const char *f;

  gcc_assert (fnad->max_files >= d->emitted_number);

  if (! d->emitted_number)
    return 1;

  gcc_assert (fnad->max_files != fnad->used_files);

  fi = fnad->files + fnad->used_files++;

  /* Skip all leading "./".  */
  f = d->filename;
  while (f[0] == '.' && IS_DIR_SEPARATOR (f[1]))
    f += 2;

  /* Create a new array entry.  */
  fi->path = f;
  fi->length = strlen (f);
  fi->file_idx = d;

  /* Search for the file name part.  */
  f = strrchr (f, DIR_SEPARATOR);
#if defined (DIR_SEPARATOR_2)
  {
    char *g = strrchr (fi->path, DIR_SEPARATOR_2);

    if (g != NULL)
      {
	if (f == NULL || f < g)
	  f = g;
      }
  }
#endif

  fi->fname = f == NULL ? fi->path : f + 1;
  return 1;
}

/* Helper function for output_file_names.  Emit a FORM encoded
   string STR, with assembly comment start ENTRY_KIND and
   index IDX */

static void
output_line_string (enum dwarf_form form, const char *str,
		    const char *entry_kind, unsigned int idx)
{
  switch (form)
    {
    case DW_FORM_string:
      dw2_asm_output_nstring (str, -1, "%s: %#x", entry_kind, idx);
      break;
    case DW_FORM_line_strp:
      if (!debug_line_str_hash)
	debug_line_str_hash
	  = hash_table<indirect_string_hasher>::create_ggc (10);

      struct indirect_string_node *node;
      node = find_AT_string_in_table (str, debug_line_str_hash);
      set_indirect_string (node);
      node->form = form;
      dw2_asm_output_offset (DWARF_OFFSET_SIZE, node->label,
			     debug_line_str_section, "%s: %#x: \"%s\"",
			     entry_kind, 0, node->str);
      break;
    default:
      gcc_unreachable ();
    }
}

/* Output the directory table and the file name table.  We try to minimize
   the total amount of memory needed.  A heuristic is used to avoid large
   slowdowns with many input files.  */

static void
output_file_names (void)
{
  struct file_name_acquire_data fnad;
  int numfiles;
  struct file_info *files;
  struct dir_info *dirs;
  int *saved;
  int *savehere;
  int *backmap;
  int ndirs;
  int idx_offset;
  int i;

  if (!last_emitted_file)
    {
      if (dwarf_version >= 5)
	{
	  dw2_asm_output_data (1, 0, "Directory entry format count");
	  dw2_asm_output_data_uleb128 (0, "Directories count");
	  dw2_asm_output_data (1, 0, "File name entry format count");
	  dw2_asm_output_data_uleb128 (0, "File names count");
	}
      else
	{
	  dw2_asm_output_data (1, 0, "End directory table");
	  dw2_asm_output_data (1, 0, "End file name table");
	}
      return;
    }

  numfiles = last_emitted_file->emitted_number;

  /* Allocate the various arrays we need.  */
  files = XALLOCAVEC (struct file_info, numfiles);
  dirs = XALLOCAVEC (struct dir_info, numfiles);

  fnad.files = files;
  fnad.used_files = 0;
  fnad.max_files = numfiles;
  file_table->traverse<file_name_acquire_data *, file_name_acquire> (&fnad);
  gcc_assert (fnad.used_files == fnad.max_files);

  qsort (files, numfiles, sizeof (files[0]), file_info_cmp);

  /* Find all the different directories used.  */
  dirs[0].path = files[0].path;
  dirs[0].length = files[0].fname - files[0].path;
  dirs[0].prefix = -1;
  dirs[0].count = 1;
  dirs[0].dir_idx = 0;
  files[0].dir_idx = 0;
  ndirs = 1;

  for (i = 1; i < numfiles; i++)
    if (files[i].fname - files[i].path == dirs[ndirs - 1].length
	&& memcmp (dirs[ndirs - 1].path, files[i].path,
		   dirs[ndirs - 1].length) == 0)
      {
	/* Same directory as last entry.  */
	files[i].dir_idx = ndirs - 1;
	++dirs[ndirs - 1].count;
      }
    else
      {
	int j;

	/* This is a new directory.  */
	dirs[ndirs].path = files[i].path;
	dirs[ndirs].length = files[i].fname - files[i].path;
	dirs[ndirs].count = 1;
	dirs[ndirs].dir_idx = ndirs;
	files[i].dir_idx = ndirs;

	/* Search for a prefix.  */
	dirs[ndirs].prefix = -1;
	for (j = 0; j < ndirs; j++)
	  if (dirs[j].length < dirs[ndirs].length
	      && dirs[j].length > 1
	      && (dirs[ndirs].prefix == -1
		  || dirs[j].length > dirs[dirs[ndirs].prefix].length)
	      && memcmp (dirs[j].path, dirs[ndirs].path, dirs[j].length) == 0)
	    dirs[ndirs].prefix = j;

	++ndirs;
      }

  /* Now to the actual work.  We have to find a subset of the directories which
     allow expressing the file name using references to the directory table
     with the least amount of characters.  We do not do an exhaustive search
     where we would have to check out every combination of every single
     possible prefix.  Instead we use a heuristic which provides nearly optimal
     results in most cases and never is much off.  */
  saved = XALLOCAVEC (int, ndirs);
  savehere = XALLOCAVEC (int, ndirs);

  memset (saved, '\0', ndirs * sizeof (saved[0]));
  for (i = 0; i < ndirs; i++)
    {
      int j;
      int total;

      /* We can always save some space for the current directory.  But this
	 does not mean it will be enough to justify adding the directory.  */
      savehere[i] = dirs[i].length;
      total = (savehere[i] - saved[i]) * dirs[i].count;

      for (j = i + 1; j < ndirs; j++)
	{
	  savehere[j] = 0;
	  if (saved[j] < dirs[i].length)
	    {
	      /* Determine whether the dirs[i] path is a prefix of the
		 dirs[j] path.  */
	      int k;

	      k = dirs[j].prefix;
	      while (k != -1 && k != (int) i)
		k = dirs[k].prefix;

	      if (k == (int) i)
		{
		  /* Yes it is.  We can possibly save some memory by
		     writing the filenames in dirs[j] relative to
		     dirs[i].  */
		  savehere[j] = dirs[i].length;
		  total += (savehere[j] - saved[j]) * dirs[j].count;
		}
	    }
	}

      /* Check whether we can save enough to justify adding the dirs[i]
	 directory.  */
      if (total > dirs[i].length + 1)
	{
	  /* It's worthwhile adding.  */
	  for (j = i; j < ndirs; j++)
	    if (savehere[j] > 0)
	      {
		/* Remember how much we saved for this directory so far.  */
		saved[j] = savehere[j];

		/* Remember the prefix directory.  */
		dirs[j].dir_idx = i;
	      }
	}
    }

  /* Emit the directory name table.  */
  idx_offset = dirs[0].length > 0 ? 1 : 0;
  enum dwarf_form str_form = DW_FORM_string;
  enum dwarf_form idx_form = DW_FORM_udata;
  if (dwarf_version >= 5)
    {
      const char *comp_dir = comp_dir_string ();
      if (comp_dir == NULL)
	comp_dir = "";
      dw2_asm_output_data (1, 1, "Directory entry format count");
      if (DWARF5_USE_DEBUG_LINE_STR)
	str_form = DW_FORM_line_strp;
      dw2_asm_output_data_uleb128 (DW_LNCT_path, "DW_LNCT_path");
      dw2_asm_output_data_uleb128 (str_form, "%s",
				   get_DW_FORM_name (str_form));
      dw2_asm_output_data_uleb128 (ndirs + idx_offset, "Directories count");
      if (str_form == DW_FORM_string)
	{
	  dw2_asm_output_nstring (comp_dir, -1, "Directory Entry: %#x", 0);
	  for (i = 1 - idx_offset; i < ndirs; i++)
	    dw2_asm_output_nstring (dirs[i].path,
				    dirs[i].length
				    - !DWARF2_DIR_SHOULD_END_WITH_SEPARATOR,
				    "Directory Entry: %#x", i + idx_offset);
	}
      else
	{
	  output_line_string (str_form, comp_dir, "Directory Entry", 0);
	  for (i = 1 - idx_offset; i < ndirs; i++)
	    {
	      const char *str
		= ggc_alloc_string (dirs[i].path,
				    dirs[i].length
				    - !DWARF2_DIR_SHOULD_END_WITH_SEPARATOR);
	      output_line_string (str_form, str, "Directory Entry",
				  (unsigned) i + idx_offset);
	    }
	}
    }
  else
    {
      for (i = 1 - idx_offset; i < ndirs; i++)
	dw2_asm_output_nstring (dirs[i].path,
				dirs[i].length
				- !DWARF2_DIR_SHOULD_END_WITH_SEPARATOR,
				"Directory Entry: %#x", i + idx_offset);

      dw2_asm_output_data (1, 0, "End directory table");
    }

  /* We have to emit them in the order of emitted_number since that's
     used in the debug info generation.  To do this efficiently we
     generate a back-mapping of the indices first.  */
  backmap = XALLOCAVEC (int, numfiles);
  for (i = 0; i < numfiles; i++)
    backmap[files[i].file_idx->emitted_number - 1] = i;

  if (dwarf_version >= 5)
    {
      const char *filename0 = get_AT_string (comp_unit_die (), DW_AT_name);
      if (filename0 == NULL)
	filename0 = "";
      /* DW_LNCT_directory_index can use DW_FORM_udata, DW_FORM_data1 and
	 DW_FORM_data2.  Choose one based on the number of directories
	 and how much space would they occupy in each encoding.
	 If we have at most 256 directories, all indexes fit into
	 a single byte, so DW_FORM_data1 is most compact (if there
	 are at most 128 directories, DW_FORM_udata would be as
	 compact as that, but not shorter and slower to decode).  */
      if (ndirs + idx_offset <= 256)
	idx_form = DW_FORM_data1;
      /* If there are more than 65536 directories, we have to use
	 DW_FORM_udata, DW_FORM_data2 can't refer to them.
	 Otherwise, compute what space would occupy if all the indexes
	 used DW_FORM_udata - sum - and compare that to how large would
	 be DW_FORM_data2 encoding, and pick the more efficient one.  */
      else if (ndirs + idx_offset <= 65536)
	{
	  unsigned HOST_WIDE_INT sum = 1;
	  for (i = 0; i < numfiles; i++)
	    {
	      int file_idx = backmap[i];
	      int dir_idx = dirs[files[file_idx].dir_idx].dir_idx;
	      sum += size_of_uleb128 (dir_idx);
	    }
	  if (sum >= HOST_WIDE_INT_UC (2) * (numfiles + 1))
	    idx_form = DW_FORM_data2;
	}
#ifdef VMS_DEBUGGING_INFO
      dw2_asm_output_data (1, 4, "File name entry format count");
#else
      dw2_asm_output_data (1, 2, "File name entry format count");
#endif
      dw2_asm_output_data_uleb128 (DW_LNCT_path, "DW_LNCT_path");
      dw2_asm_output_data_uleb128 (str_form, "%s",
				   get_DW_FORM_name (str_form));
      dw2_asm_output_data_uleb128 (DW_LNCT_directory_index,
				   "DW_LNCT_directory_index");
      dw2_asm_output_data_uleb128 (idx_form, "%s",
				   get_DW_FORM_name (idx_form));
#ifdef VMS_DEBUGGING_INFO
      dw2_asm_output_data_uleb128 (DW_LNCT_timestamp, "DW_LNCT_timestamp");
      dw2_asm_output_data_uleb128 (DW_FORM_udata, "DW_FORM_udata");
      dw2_asm_output_data_uleb128 (DW_LNCT_size, "DW_LNCT_size");
      dw2_asm_output_data_uleb128 (DW_FORM_udata, "DW_FORM_udata");
#endif
      dw2_asm_output_data_uleb128 (numfiles + 1, "File names count");

      output_line_string (str_form, filename0, "File Entry", 0);

      /* Include directory index.  */
      if (idx_form != DW_FORM_udata)
	dw2_asm_output_data (idx_form == DW_FORM_data1 ? 1 : 2,
			     0, NULL);
      else
	dw2_asm_output_data_uleb128 (0, NULL);

#ifdef VMS_DEBUGGING_INFO
      dw2_asm_output_data_uleb128 (0, NULL);
      dw2_asm_output_data_uleb128 (0, NULL);
#endif
    }

  /* Now write all the file names.  */
  for (i = 0; i < numfiles; i++)
    {
      int file_idx = backmap[i];
      int dir_idx = dirs[files[file_idx].dir_idx].dir_idx;

#ifdef VMS_DEBUGGING_INFO
#define MAX_VMS_VERSION_LEN 6 /* ";32768" */

      /* Setting these fields can lead to debugger miscomparisons,
         but VMS Debug requires them to be set correctly.  */

      int ver;
      long long cdt;
      long siz;
      int maxfilelen = (strlen (files[file_idx].path)
			+ dirs[dir_idx].length
			+ MAX_VMS_VERSION_LEN + 1);
      char *filebuf = XALLOCAVEC (char, maxfilelen);

      vms_file_stats_name (files[file_idx].path, 0, 0, 0, &ver);
      snprintf (filebuf, maxfilelen, "%s;%d",
	        files[file_idx].path + dirs[dir_idx].length, ver);

      output_line_string (str_form, filebuf, "File Entry", (unsigned) i + 1);

      /* Include directory index.  */
      if (dwarf_version >= 5 && idx_form != DW_FORM_udata)
	dw2_asm_output_data (idx_form == DW_FORM_data1 ? 1 : 2,
			     dir_idx + idx_offset, NULL);
      else
	dw2_asm_output_data_uleb128 (dir_idx + idx_offset, NULL);

      /* Modification time.  */
      dw2_asm_output_data_uleb128 ((vms_file_stats_name (files[file_idx].path,
							 &cdt, 0, 0, 0) == 0)
				   ? cdt : 0, NULL);

      /* File length in bytes.  */
      dw2_asm_output_data_uleb128 ((vms_file_stats_name (files[file_idx].path,
							 0, &siz, 0, 0) == 0)
				   ? siz : 0, NULL);
#else
      output_line_string (str_form,
			  files[file_idx].path + dirs[dir_idx].length,
			  "File Entry", (unsigned) i + 1);

      /* Include directory index.  */
      if (dwarf_version >= 5 && idx_form != DW_FORM_udata)
	dw2_asm_output_data (idx_form == DW_FORM_data1 ? 1 : 2,
			     dir_idx + idx_offset, NULL);
      else
	dw2_asm_output_data_uleb128 (dir_idx + idx_offset, NULL);

      if (dwarf_version >= 5)
	continue;

      /* Modification time.  */
      dw2_asm_output_data_uleb128 (0, NULL);

      /* File length in bytes.  */
      dw2_asm_output_data_uleb128 (0, NULL);
#endif /* VMS_DEBUGGING_INFO */
    }

  if (dwarf_version < 5)
    dw2_asm_output_data (1, 0, "End file name table");
}


/* Output one line number table into the .debug_line section.  */

static void
output_one_line_info_table (dw_line_info_table *table)
{
  char line_label[MAX_ARTIFICIAL_LABEL_BYTES];
  unsigned int current_line = 1;
  bool current_is_stmt = DWARF_LINE_DEFAULT_IS_STMT_START;
  dw_line_info_entry *ent;
  size_t i;

  FOR_EACH_VEC_SAFE_ELT (table->entries, i, ent)
    {
      switch (ent->opcode)
	{
	case LI_set_address:
	  /* ??? Unfortunately, we have little choice here currently, and
	     must always use the most general form.  GCC does not know the
	     address delta itself, so we can't use DW_LNS_advance_pc.  Many
	     ports do have length attributes which will give an upper bound
	     on the address range.  We could perhaps use length attributes
	     to determine when it is safe to use DW_LNS_fixed_advance_pc.  */
	  ASM_GENERATE_INTERNAL_LABEL (line_label, LINE_CODE_LABEL, ent->val);

	  /* This can handle any delta.  This takes
	     4+DWARF2_ADDR_SIZE bytes.  */
	  dw2_asm_output_data (1, 0, "set address %s", line_label);
	  dw2_asm_output_data_uleb128 (1 + DWARF2_ADDR_SIZE, NULL);
	  dw2_asm_output_data (1, DW_LNE_set_address, NULL);
	  dw2_asm_output_addr (DWARF2_ADDR_SIZE, line_label, NULL);
	  break;

	case LI_set_line:
	  if (ent->val == current_line)
	    {
	      /* We still need to start a new row, so output a copy insn.  */
	      dw2_asm_output_data (1, DW_LNS_copy,
				   "copy line %u", current_line);
	    }
	  else
	    {
	      int line_offset = ent->val - current_line;
	      int line_delta = line_offset - DWARF_LINE_BASE;

	      current_line = ent->val;
	      if (line_delta >= 0 && line_delta < (DWARF_LINE_RANGE - 1))
		{
		  /* This can handle deltas from -10 to 234, using the current
		     definitions of DWARF_LINE_BASE and DWARF_LINE_RANGE.
		     This takes 1 byte.  */
		  dw2_asm_output_data (1, DWARF_LINE_OPCODE_BASE + line_delta,
				       "line %u", current_line);
		}
	      else
		{
		  /* This can handle any delta.  This takes at least 4 bytes,
		     depending on the value being encoded.  */
		  dw2_asm_output_data (1, DW_LNS_advance_line,
				       "advance to line %u", current_line);
		  dw2_asm_output_data_sleb128 (line_offset, NULL);
		  dw2_asm_output_data (1, DW_LNS_copy, NULL);
		}
	    }
	  break;

	case LI_set_file:
	  dw2_asm_output_data (1, DW_LNS_set_file, "set file %u", ent->val);
	  dw2_asm_output_data_uleb128 (ent->val, "%u", ent->val);
	  break;

	case LI_set_column:
	  dw2_asm_output_data (1, DW_LNS_set_column, "column %u", ent->val);
	  dw2_asm_output_data_uleb128 (ent->val, "%u", ent->val);
	  break;

	case LI_negate_stmt:
	  current_is_stmt = !current_is_stmt;
	  dw2_asm_output_data (1, DW_LNS_negate_stmt,
			       "is_stmt %d", current_is_stmt);
	  break;

	case LI_set_prologue_end:
	  dw2_asm_output_data (1, DW_LNS_set_prologue_end,
			       "set prologue end");
	  break;
	  
	case LI_set_epilogue_begin:
	  dw2_asm_output_data (1, DW_LNS_set_epilogue_begin,
			       "set epilogue begin");
	  break;

	case LI_set_discriminator:
	  dw2_asm_output_data (1, 0, "discriminator %u", ent->val);
	  dw2_asm_output_data_uleb128 (1 + size_of_uleb128 (ent->val), NULL);
	  dw2_asm_output_data (1, DW_LNE_set_discriminator, NULL);
	  dw2_asm_output_data_uleb128 (ent->val, NULL);
	  break;
	}
    }

  /* Emit debug info for the address of the end of the table.  */
  dw2_asm_output_data (1, 0, "set address %s", table->end_label);
  dw2_asm_output_data_uleb128 (1 + DWARF2_ADDR_SIZE, NULL);
  dw2_asm_output_data (1, DW_LNE_set_address, NULL);
  dw2_asm_output_addr (DWARF2_ADDR_SIZE, table->end_label, NULL);

  dw2_asm_output_data (1, 0, "end sequence");
  dw2_asm_output_data_uleb128 (1, NULL);
  dw2_asm_output_data (1, DW_LNE_end_sequence, NULL);
}

/* Output the source line number correspondence information.  This
   information goes into the .debug_line section.  */

static void
output_line_info (bool prologue_only)
{
  static unsigned int generation;
  char l1[MAX_ARTIFICIAL_LABEL_BYTES], l2[MAX_ARTIFICIAL_LABEL_BYTES];
  char p1[MAX_ARTIFICIAL_LABEL_BYTES], p2[MAX_ARTIFICIAL_LABEL_BYTES];
  bool saw_one = false;
  int opc;

  ASM_GENERATE_INTERNAL_LABEL (l1, LINE_NUMBER_BEGIN_LABEL, generation);
  ASM_GENERATE_INTERNAL_LABEL (l2, LINE_NUMBER_END_LABEL, generation);
  ASM_GENERATE_INTERNAL_LABEL (p1, LN_PROLOG_AS_LABEL, generation);
  ASM_GENERATE_INTERNAL_LABEL (p2, LN_PROLOG_END_LABEL, generation++);

  if (!XCOFF_DEBUGGING_INFO)
    {
      if (DWARF_INITIAL_LENGTH_SIZE - DWARF_OFFSET_SIZE == 4)
	dw2_asm_output_data (4, 0xffffffff,
	  "Initial length escape value indicating 64-bit DWARF extension");
      dw2_asm_output_delta (DWARF_OFFSET_SIZE, l2, l1,
			    "Length of Source Line Info");
    }

  ASM_OUTPUT_LABEL (asm_out_file, l1);

  dw2_asm_output_data (2, dwarf_version, "DWARF Version");
  if (dwarf_version >= 5)
    {
      dw2_asm_output_data (1, DWARF2_ADDR_SIZE, "Address Size");
      dw2_asm_output_data (1, 0, "Segment Size");
    }
  dw2_asm_output_delta (DWARF_OFFSET_SIZE, p2, p1, "Prolog Length");
  ASM_OUTPUT_LABEL (asm_out_file, p1);

  /* Define the architecture-dependent minimum instruction length (in bytes).
     In this implementation of DWARF, this field is used for information
     purposes only.  Since GCC generates assembly language, we have no
     a priori knowledge of how many instruction bytes are generated for each
     source line, and therefore can use only the DW_LNE_set_address and
     DW_LNS_fixed_advance_pc line information commands.  Accordingly, we fix
     this as '1', which is "correct enough" for all architectures,
     and don't let the target override.  */
  dw2_asm_output_data (1, 1, "Minimum Instruction Length");

  if (dwarf_version >= 4)
    dw2_asm_output_data (1, DWARF_LINE_DEFAULT_MAX_OPS_PER_INSN,
			 "Maximum Operations Per Instruction");
  dw2_asm_output_data (1, DWARF_LINE_DEFAULT_IS_STMT_START,
		       "Default is_stmt_start flag");
  dw2_asm_output_data (1, DWARF_LINE_BASE,
		       "Line Base Value (Special Opcodes)");
  dw2_asm_output_data (1, DWARF_LINE_RANGE,
		       "Line Range Value (Special Opcodes)");
  dw2_asm_output_data (1, DWARF_LINE_OPCODE_BASE,
		       "Special Opcode Base");

  for (opc = 1; opc < DWARF_LINE_OPCODE_BASE; opc++)
    {
      int n_op_args;
      switch (opc)
	{
	case DW_LNS_advance_pc:
	case DW_LNS_advance_line:
	case DW_LNS_set_file:
	case DW_LNS_set_column:
	case DW_LNS_fixed_advance_pc:
	case DW_LNS_set_isa:
	  n_op_args = 1;
	  break;
	default:
	  n_op_args = 0;
	  break;
	}

      dw2_asm_output_data (1, n_op_args, "opcode: %#x has %d args",
			   opc, n_op_args);
    }

  /* Write out the information about the files we use.  */
  output_file_names ();
  ASM_OUTPUT_LABEL (asm_out_file, p2);
  if (prologue_only)
    {
      /* Output the marker for the end of the line number info.  */
      ASM_OUTPUT_LABEL (asm_out_file, l2);
      return;
    }

  if (separate_line_info)
    {
      dw_line_info_table *table;
      size_t i;

      FOR_EACH_VEC_ELT (*separate_line_info, i, table)
	if (table->in_use)
	  {
	    output_one_line_info_table (table);
	    saw_one = true;
	  }
    }
  if (cold_text_section_line_info && cold_text_section_line_info->in_use)
    {
      output_one_line_info_table (cold_text_section_line_info);
      saw_one = true;
    }

  /* ??? Some Darwin linkers crash on a .debug_line section with no
     sequences.  Further, merely a DW_LNE_end_sequence entry is not
     sufficient -- the address column must also be initialized.
     Make sure to output at least one set_address/end_sequence pair,
     choosing .text since that section is always present.  */
  if (text_section_line_info->in_use || !saw_one)
    output_one_line_info_table (text_section_line_info);

  /* Output the marker for the end of the line number info.  */
  ASM_OUTPUT_LABEL (asm_out_file, l2);
}

/* Return true if DW_AT_endianity should be emitted according to REVERSE.  */

static inline bool
need_endianity_attribute_p (bool reverse)
{
  return reverse && (dwarf_version >= 3 || !dwarf_strict);
}

/* Given a pointer to a tree node for some base type, return a pointer to
   a DIE that describes the given type.  REVERSE is true if the type is
   to be interpreted in the reverse storage order wrt the target order.

   This routine must only be called for GCC type nodes that correspond to
   Dwarf base (fundamental) types.  */

static dw_die_ref
base_type_die (tree type, bool reverse)
{
  dw_die_ref base_type_result;
  enum dwarf_type encoding;
  bool fpt_used = false;
  struct fixed_point_type_info fpt_info;
  tree type_bias = NULL_TREE;

  if (TREE_CODE (type) == ERROR_MARK || TREE_CODE (type) == VOID_TYPE)
    return 0;

  /* If this is a subtype that should not be emitted as a subrange type,
     use the base type.  See subrange_type_for_debug_p.  */
  if (TREE_CODE (type) == INTEGER_TYPE && TREE_TYPE (type) != NULL_TREE)
    type = TREE_TYPE (type);

  switch (TREE_CODE (type))
    {
    case INTEGER_TYPE:
      if ((dwarf_version >= 4 || !dwarf_strict)
	  && TYPE_NAME (type)
	  && TREE_CODE (TYPE_NAME (type)) == TYPE_DECL
	  && DECL_IS_BUILTIN (TYPE_NAME (type))
	  && DECL_NAME (TYPE_NAME (type)))
	{
	  const char *name = IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (type)));
	  if (strcmp (name, "char16_t") == 0
	      || strcmp (name, "char32_t") == 0)
	    {
	      encoding = DW_ATE_UTF;
	      break;
	    }
	}
      if ((dwarf_version >= 3 || !dwarf_strict)
	  && lang_hooks.types.get_fixed_point_type_info)
	{
	  memset (&fpt_info, 0, sizeof (fpt_info));
	  if (lang_hooks.types.get_fixed_point_type_info (type, &fpt_info))
	    {
	      fpt_used = true;
	      encoding = ((TYPE_UNSIGNED (type))
			  ? DW_ATE_unsigned_fixed
			  : DW_ATE_signed_fixed);
	      break;
	    }
	}
      if (TYPE_STRING_FLAG (type))
	{
	  if (TYPE_UNSIGNED (type))
	    encoding = DW_ATE_unsigned_char;
	  else
	    encoding = DW_ATE_signed_char;
	}
      else if (TYPE_UNSIGNED (type))
	encoding = DW_ATE_unsigned;
      else
	encoding = DW_ATE_signed;

      if (!dwarf_strict
	  && lang_hooks.types.get_type_bias)
	type_bias = lang_hooks.types.get_type_bias (type);
      break;

    case REAL_TYPE:
      if (DECIMAL_FLOAT_MODE_P (TYPE_MODE (type)))
	{
	  if (dwarf_version >= 3 || !dwarf_strict)
	    encoding = DW_ATE_decimal_float;
	  else
	    encoding = DW_ATE_lo_user;
	}
      else
	encoding = DW_ATE_float;
      break;

    case FIXED_POINT_TYPE:
      if (!(dwarf_version >= 3 || !dwarf_strict))
	encoding = DW_ATE_lo_user;
      else if (TYPE_UNSIGNED (type))
	encoding = DW_ATE_unsigned_fixed;
      else
	encoding = DW_ATE_signed_fixed;
      break;

      /* Dwarf2 doesn't know anything about complex ints, so use
	 a user defined type for it.  */
    case COMPLEX_TYPE:
      if (TREE_CODE (TREE_TYPE (type)) == REAL_TYPE)
	encoding = DW_ATE_complex_float;
      else
	encoding = DW_ATE_lo_user;
      break;

    case BOOLEAN_TYPE:
      /* GNU FORTRAN/Ada/C++ BOOLEAN type.  */
      encoding = DW_ATE_boolean;
      break;

    default:
      /* No other TREE_CODEs are Dwarf fundamental types.  */
      gcc_unreachable ();
    }

  base_type_result = new_die (DW_TAG_base_type, comp_unit_die (), type);

  add_AT_unsigned (base_type_result, DW_AT_byte_size,
		   int_size_in_bytes (type));
  add_AT_unsigned (base_type_result, DW_AT_encoding, encoding);

  if (need_endianity_attribute_p (reverse))
    add_AT_unsigned (base_type_result, DW_AT_endianity,
		     BYTES_BIG_ENDIAN ? DW_END_little : DW_END_big);

  add_alignment_attribute (base_type_result, type);

  if (fpt_used)
    {
      switch (fpt_info.scale_factor_kind)
	{
	case fixed_point_scale_factor_binary:
	  add_AT_int (base_type_result, DW_AT_binary_scale,
		      fpt_info.scale_factor.binary);
	  break;

	case fixed_point_scale_factor_decimal:
	  add_AT_int (base_type_result, DW_AT_decimal_scale,
		      fpt_info.scale_factor.decimal);
	  break;

	case fixed_point_scale_factor_arbitrary:
	  /* Arbitrary scale factors cannot be described in standard DWARF,
	     yet.  */
	  if (!dwarf_strict)
	    {
	      /* Describe the scale factor as a rational constant.  */
	      const dw_die_ref scale_factor
		= new_die (DW_TAG_constant, comp_unit_die (), type);

	      add_AT_unsigned (scale_factor, DW_AT_GNU_numerator,
			       fpt_info.scale_factor.arbitrary.numerator);
	      add_AT_int (scale_factor, DW_AT_GNU_denominator,
			  fpt_info.scale_factor.arbitrary.denominator);

	      add_AT_die_ref (base_type_result, DW_AT_small, scale_factor);
	    }
	  break;

	default:
	  gcc_unreachable ();
	}
    }

  if (type_bias)
    add_scalar_info (base_type_result, DW_AT_GNU_bias, type_bias,
		     dw_scalar_form_constant
		     | dw_scalar_form_exprloc
		     | dw_scalar_form_reference,
		     NULL);

  add_pubtype (type, base_type_result);

  return base_type_result;
}

/* A C++ function with deduced return type can have a TEMPLATE_TYPE_PARM
   named 'auto' in its type: return true for it, false otherwise.  */

static inline bool
is_cxx_auto (tree type)
{
  if (is_cxx ())
    {
      tree name = TYPE_IDENTIFIER (type);
      if (name == get_identifier ("auto")
	  || name == get_identifier ("decltype(auto)"))
	return true;
    }
  return false;
}

/* Given a pointer to an arbitrary ..._TYPE tree node, return nonzero if the
   given input type is a Dwarf "fundamental" type.  Otherwise return null.  */

static inline int
is_base_type (tree type)
{
  switch (TREE_CODE (type))
    {
    case ERROR_MARK:
    case VOID_TYPE:
    case INTEGER_TYPE:
    case REAL_TYPE:
    case FIXED_POINT_TYPE:
    case COMPLEX_TYPE:
    case BOOLEAN_TYPE:
    case POINTER_BOUNDS_TYPE:
      return 1;

    case ARRAY_TYPE:
    case RECORD_TYPE:
    case UNION_TYPE:
    case QUAL_UNION_TYPE:
    case ENUMERAL_TYPE:
    case FUNCTION_TYPE:
    case METHOD_TYPE:
    case POINTER_TYPE:
    case REFERENCE_TYPE:
    case NULLPTR_TYPE:
    case OFFSET_TYPE:
    case LANG_TYPE:
    case VECTOR_TYPE:
      return 0;

    default:
      if (is_cxx_auto (type))
	return 0;
      gcc_unreachable ();
    }

  return 0;
}

/* Given a pointer to a tree node, assumed to be some kind of a ..._TYPE
   node, return the size in bits for the type if it is a constant, or else
   return the alignment for the type if the type's size is not constant, or
   else return BITS_PER_WORD if the type actually turns out to be an
   ERROR_MARK node.  */

static inline unsigned HOST_WIDE_INT
simple_type_size_in_bits (const_tree type)
{
  if (TREE_CODE (type) == ERROR_MARK)
    return BITS_PER_WORD;
  else if (TYPE_SIZE (type) == NULL_TREE)
    return 0;
  else if (tree_fits_uhwi_p (TYPE_SIZE (type)))
    return tree_to_uhwi (TYPE_SIZE (type));
  else
    return TYPE_ALIGN (type);
}

/* Similarly, but return an offset_int instead of UHWI.  */

static inline offset_int
offset_int_type_size_in_bits (const_tree type)
{
  if (TREE_CODE (type) == ERROR_MARK)
    return BITS_PER_WORD;
  else if (TYPE_SIZE (type) == NULL_TREE)
    return 0;
  else if (TREE_CODE (TYPE_SIZE (type)) == INTEGER_CST)
    return wi::to_offset (TYPE_SIZE (type));
  else
    return TYPE_ALIGN (type);
}

/*  Given a pointer to a tree node for a subrange type, return a pointer
    to a DIE that describes the given type.  */

static dw_die_ref
subrange_type_die (tree type, tree low, tree high, tree bias,
		   dw_die_ref context_die)
{
  dw_die_ref subrange_die;
  const HOST_WIDE_INT size_in_bytes = int_size_in_bytes (type);

  if (context_die == NULL)
    context_die = comp_unit_die ();

  subrange_die = new_die (DW_TAG_subrange_type, context_die, type);

  if (int_size_in_bytes (TREE_TYPE (type)) != size_in_bytes)
    {
      /* The size of the subrange type and its base type do not match,
	 so we need to generate a size attribute for the subrange type.  */
      add_AT_unsigned (subrange_die, DW_AT_byte_size, size_in_bytes);
    }

  add_alignment_attribute (subrange_die, type);

  if (low)
    add_bound_info (subrange_die, DW_AT_lower_bound, low, NULL);
  if (high)
    add_bound_info (subrange_die, DW_AT_upper_bound, high, NULL);
  if (bias && !dwarf_strict)
    add_scalar_info (subrange_die, DW_AT_GNU_bias, bias,
		     dw_scalar_form_constant
		     | dw_scalar_form_exprloc
		     | dw_scalar_form_reference,
		     NULL);

  return subrange_die;
}

/* Returns the (const and/or volatile) cv_qualifiers associated with
   the decl node.  This will normally be augmented with the
   cv_qualifiers of the underlying type in add_type_attribute.  */

static int
decl_quals (const_tree decl)
{
  return ((TREE_READONLY (decl)
	   /* The C++ front-end correctly marks reference-typed
	      variables as readonly, but from a language (and debug
	      info) standpoint they are not const-qualified.  */
	   && TREE_CODE (TREE_TYPE (decl)) != REFERENCE_TYPE
	   ? TYPE_QUAL_CONST : TYPE_UNQUALIFIED)
	  | (TREE_THIS_VOLATILE (decl)
	     ? TYPE_QUAL_VOLATILE : TYPE_UNQUALIFIED));
}

/* Determine the TYPE whose qualifiers match the largest strict subset
   of the given TYPE_QUALS, and return its qualifiers.  Ignore all
   qualifiers outside QUAL_MASK.  */

static int
get_nearest_type_subqualifiers (tree type, int type_quals, int qual_mask)
{
  tree t;
  int best_rank = 0, best_qual = 0, max_rank;

  type_quals &= qual_mask;
  max_rank = popcount_hwi (type_quals) - 1;

  for (t = TYPE_MAIN_VARIANT (type); t && best_rank < max_rank;
       t = TYPE_NEXT_VARIANT (t))
    {
      int q = TYPE_QUALS (t) & qual_mask;

      if ((q & type_quals) == q && q != type_quals
	  && check_base_type (t, type))
	{
	  int rank = popcount_hwi (q);

	  if (rank > best_rank)
	    {
	      best_rank = rank;
	      best_qual = q;
	    }
	}
    }

  return best_qual;
}

struct dwarf_qual_info_t { int q; enum dwarf_tag t; };
static const dwarf_qual_info_t dwarf_qual_info[] =
{
  { TYPE_QUAL_CONST, DW_TAG_const_type },
  { TYPE_QUAL_VOLATILE, DW_TAG_volatile_type },
  { TYPE_QUAL_RESTRICT, DW_TAG_restrict_type },
  { TYPE_QUAL_ATOMIC, DW_TAG_atomic_type }
};
static const unsigned int dwarf_qual_info_size
  = sizeof (dwarf_qual_info) / sizeof (dwarf_qual_info[0]);

/* If DIE is a qualified DIE of some base DIE with the same parent,
   return the base DIE, otherwise return NULL.  Set MASK to the
   qualifiers added compared to the returned DIE.  */

static dw_die_ref
qualified_die_p (dw_die_ref die, int *mask, unsigned int depth)
{
  unsigned int i;
  for (i = 0; i < dwarf_qual_info_size; i++)
    if (die->die_tag == dwarf_qual_info[i].t)
      break;
  if (i == dwarf_qual_info_size)
    return NULL;
  if (vec_safe_length (die->die_attr) != 1)
    return NULL;
  dw_die_ref type = get_AT_ref (die, DW_AT_type);
  if (type == NULL || type->die_parent != die->die_parent)
    return NULL;
  *mask |= dwarf_qual_info[i].q;
  if (depth)
    {
      dw_die_ref ret = qualified_die_p (type, mask, depth - 1);
      if (ret)
	return ret;
    }
  return type;
}

/* Given a pointer to an arbitrary ..._TYPE tree node, return a debugging
   entry that chains the modifiers specified by CV_QUALS in front of the
   given type.  REVERSE is true if the type is to be interpreted in the
   reverse storage order wrt the target order.  */

static dw_die_ref
modified_type_die (tree type, int cv_quals, bool reverse,
		   dw_die_ref context_die)
{
  enum tree_code code = TREE_CODE (type);
  dw_die_ref mod_type_die;
  dw_die_ref sub_die = NULL;
  tree item_type = NULL;
  tree qualified_type;
  tree name, low, high;
  dw_die_ref mod_scope;
  /* Only these cv-qualifiers are currently handled.  */
  const int cv_qual_mask = (TYPE_QUAL_CONST | TYPE_QUAL_VOLATILE
			    | TYPE_QUAL_RESTRICT | TYPE_QUAL_ATOMIC);

  if (code == ERROR_MARK)
    return NULL;

  if (lang_hooks.types.get_debug_type)
    {
      tree debug_type = lang_hooks.types.get_debug_type (type);

      if (debug_type != NULL_TREE && debug_type != type)
	return modified_type_die (debug_type, cv_quals, reverse, context_die);
    }

  cv_quals &= cv_qual_mask;

  /* Don't emit DW_TAG_restrict_type for DWARFv2, since it is a type
     tag modifier (and not an attribute) old consumers won't be able
     to handle it.  */
  if (dwarf_version < 3)
    cv_quals &= ~TYPE_QUAL_RESTRICT;

  /* Likewise for DW_TAG_atomic_type for DWARFv5.  */
  if (dwarf_version < 5)
    cv_quals &= ~TYPE_QUAL_ATOMIC;

  /* See if we already have the appropriately qualified variant of
     this type.  */
  qualified_type = get_qualified_type (type, cv_quals);

  if (qualified_type == sizetype)
    {
      /* Try not to expose the internal sizetype type's name.  */
      if (TYPE_NAME (qualified_type)
	  && TREE_CODE (TYPE_NAME (qualified_type)) == TYPE_DECL)
	{
	  tree t = TREE_TYPE (TYPE_NAME (qualified_type));

	  gcc_checking_assert (TREE_CODE (t) == INTEGER_TYPE
			       && (TYPE_PRECISION (t)
				   == TYPE_PRECISION (qualified_type))
			       && (TYPE_UNSIGNED (t)
				   == TYPE_UNSIGNED (qualified_type)));
	  qualified_type = t;
	}
      else if (qualified_type == sizetype
	       && TREE_CODE (sizetype) == TREE_CODE (size_type_node)
	       && TYPE_PRECISION (sizetype) == TYPE_PRECISION (size_type_node)
	       && TYPE_UNSIGNED (sizetype) == TYPE_UNSIGNED (size_type_node))
	qualified_type = size_type_node;
    }


  /* If we do, then we can just use its DIE, if it exists.  */
  if (qualified_type)
    {
      mod_type_die = lookup_type_die (qualified_type);

      /* DW_AT_endianity doesn't come from a qualifier on the type.  */
      if (mod_type_die
	  && (!need_endianity_attribute_p (reverse)
	      || !is_base_type (type)
	      || get_AT_unsigned (mod_type_die, DW_AT_endianity)))
	return mod_type_die;
    }

  name = qualified_type ? TYPE_NAME (qualified_type) : NULL;

  /* Handle C typedef types.  */
  if (name && TREE_CODE (name) == TYPE_DECL && DECL_ORIGINAL_TYPE (name)
      && !DECL_ARTIFICIAL (name))
    {
      tree dtype = TREE_TYPE (name);

      if (qualified_type == dtype)
	{
	  tree origin = decl_ultimate_origin (name);

	  /* Typedef variants that have an abstract origin don't get their own
	     type DIE (see gen_typedef_die), so fall back on the ultimate
	     abstract origin instead.  */
	  if (origin != NULL && origin != name)
	    return modified_type_die (TREE_TYPE (origin), cv_quals, reverse,
				      context_die);

	  /* For a named type, use the typedef.  */
	  gen_type_die (qualified_type, context_die);
	  return lookup_type_die (qualified_type);
	}
      else
	{
	  int dquals = TYPE_QUALS_NO_ADDR_SPACE (dtype);
	  dquals &= cv_qual_mask;
	  if ((dquals & ~cv_quals) != TYPE_UNQUALIFIED
	      || (cv_quals == dquals && DECL_ORIGINAL_TYPE (name) != type))
	    /* cv-unqualified version of named type.  Just use
	       the unnamed type to which it refers.  */
	    return modified_type_die (DECL_ORIGINAL_TYPE (name), cv_quals,
				      reverse, context_die);
	  /* Else cv-qualified version of named type; fall through.  */
	}
    }

  mod_scope = scope_die_for (type, context_die);

  if (cv_quals)
    {
      int sub_quals = 0, first_quals = 0;
      unsigned i;
      dw_die_ref first = NULL, last = NULL;

      /* Determine a lesser qualified type that most closely matches
	 this one.  Then generate DW_TAG_* entries for the remaining
	 qualifiers.  */
      sub_quals = get_nearest_type_subqualifiers (type, cv_quals,
						  cv_qual_mask);
      if (sub_quals && use_debug_types)
	{
	  bool needed = false;
	  /* If emitting type units, make sure the order of qualifiers
	     is canonical.  Thus, start from unqualified type if
	     an earlier qualifier is missing in sub_quals, but some later
	     one is present there.  */
	  for (i = 0; i < dwarf_qual_info_size; i++)
	    if (dwarf_qual_info[i].q & cv_quals & ~sub_quals)
	      needed = true;
	    else if (needed && (dwarf_qual_info[i].q & cv_quals))
	      {
		sub_quals = 0;
		break;
	      }
	}
      mod_type_die = modified_type_die (type, sub_quals, reverse, context_die);
      if (mod_scope && mod_type_die && mod_type_die->die_parent == mod_scope)
	{
	  /* As not all intermediate qualified DIEs have corresponding
	     tree types, ensure that qualified DIEs in the same scope
	     as their DW_AT_type are emitted after their DW_AT_type,
	     only with other qualified DIEs for the same type possibly
	     in between them.  Determine the range of such qualified
	     DIEs now (first being the base type, last being corresponding
	     last qualified DIE for it).  */
	  unsigned int count = 0;
	  first = qualified_die_p (mod_type_die, &first_quals,
				   dwarf_qual_info_size);
	  if (first == NULL)
	    first = mod_type_die;
	  gcc_assert ((first_quals & ~sub_quals) == 0);
	  for (count = 0, last = first;
	       count < (1U << dwarf_qual_info_size);
	       count++, last = last->die_sib)
	    {
	      int quals = 0;
	      if (last == mod_scope->die_child)
		break;
	      if (qualified_die_p (last->die_sib, &quals, dwarf_qual_info_size)
		  != first)
		break;
	    }
	}

      for (i = 0; i < dwarf_qual_info_size; i++)
	if (dwarf_qual_info[i].q & cv_quals & ~sub_quals)
	  {
	    dw_die_ref d;
	    if (first && first != last)
	      {
		for (d = first->die_sib; ; d = d->die_sib)
		  {
		    int quals = 0;
		    qualified_die_p (d, &quals, dwarf_qual_info_size);
		    if (quals == (first_quals | dwarf_qual_info[i].q))
		      break;
		    if (d == last)
		      {
			d = NULL;
			break;
		      }
		  }
		if (d)
		  {
		    mod_type_die = d;
		    continue;
		  }
	      }
	    if (first)
	      {
		d = ggc_cleared_alloc<die_node> ();
		d->die_tag = dwarf_qual_info[i].t;
		add_child_die_after (mod_scope, d, last);
		last = d;
	      }
	    else
	      d = new_die (dwarf_qual_info[i].t, mod_scope, type);
	    if (mod_type_die)
	      add_AT_die_ref (d, DW_AT_type, mod_type_die);
	    mod_type_die = d;
	    first_quals |= dwarf_qual_info[i].q;
	  }
    }
  else if (code == POINTER_TYPE || code == REFERENCE_TYPE)
    {
      dwarf_tag tag = DW_TAG_pointer_type;
      if (code == REFERENCE_TYPE)
	{
	  if (TYPE_REF_IS_RVALUE (type) && dwarf_version >= 4)
	    tag = DW_TAG_rvalue_reference_type;
	  else
	    tag = DW_TAG_reference_type;
	}
      mod_type_die = new_die (tag, mod_scope, type);

      add_AT_unsigned (mod_type_die, DW_AT_byte_size,
		       simple_type_size_in_bits (type) / BITS_PER_UNIT);
      add_alignment_attribute (mod_type_die, type);
      item_type = TREE_TYPE (type);

      addr_space_t as = TYPE_ADDR_SPACE (item_type);
      if (!ADDR_SPACE_GENERIC_P (as))
	{
	  int action = targetm.addr_space.debug (as);
	  if (action >= 0)
	    {
	      /* Positive values indicate an address_class.  */
	      add_AT_unsigned (mod_type_die, DW_AT_address_class, action);
	    }
	  else
	    {
	      /* Negative values indicate an (inverted) segment base reg.  */
	      dw_loc_descr_ref d
		= one_reg_loc_descriptor (~action, VAR_INIT_STATUS_INITIALIZED);
	      add_AT_loc (mod_type_die, DW_AT_segment, d);
	    }
	}
    }
  else if (code == INTEGER_TYPE
	   && TREE_TYPE (type) != NULL_TREE
	   && subrange_type_for_debug_p (type, &low, &high))
    {
      tree bias = NULL_TREE;
      if (lang_hooks.types.get_type_bias)
	bias = lang_hooks.types.get_type_bias (type);
      mod_type_die = subrange_type_die (type, low, high, bias, context_die);
      item_type = TREE_TYPE (type);
    }
  else if (is_base_type (type))
    mod_type_die = base_type_die (type, reverse);
  else
    {
      gen_type_die (type, context_die);

      /* We have to get the type_main_variant here (and pass that to the
	 `lookup_type_die' routine) because the ..._TYPE node we have
	 might simply be a *copy* of some original type node (where the
	 copy was created to help us keep track of typedef names) and
	 that copy might have a different TYPE_UID from the original
	 ..._TYPE node.  */
      if (TREE_CODE (type) == FUNCTION_TYPE
	  || TREE_CODE (type) == METHOD_TYPE)
	{
	  /* For function/method types, can't just use type_main_variant here,
	     because that can have different ref-qualifiers for C++,
	     but try to canonicalize.  */
	  tree main = TYPE_MAIN_VARIANT (type);
	  for (tree t = main; t; t = TYPE_NEXT_VARIANT (t))
	    if (TYPE_QUALS_NO_ADDR_SPACE (t) == 0
		&& check_base_type (t, main)
		&& check_lang_type (t, type))
	      return lookup_type_die (t);
	  return lookup_type_die (type);
	}
      else if (TREE_CODE (type) != VECTOR_TYPE
	       && TREE_CODE (type) != ARRAY_TYPE)
	return lookup_type_die (type_main_variant (type));
      else
	/* Vectors have the debugging information in the type,
	   not the main variant.  */
	return lookup_type_die (type);
    }

  /* Builtin types don't have a DECL_ORIGINAL_TYPE.  For those,
     don't output a DW_TAG_typedef, since there isn't one in the
     user's program; just attach a DW_AT_name to the type.
     Don't attach a DW_AT_name to DW_TAG_const_type or DW_TAG_volatile_type
     if the base type already has the same name.  */
  if (name
      && ((TREE_CODE (name) != TYPE_DECL
	   && (qualified_type == TYPE_MAIN_VARIANT (type)
	       || (cv_quals == TYPE_UNQUALIFIED)))
	  || (TREE_CODE (name) == TYPE_DECL
	      && TREE_TYPE (name) == qualified_type
	      && DECL_NAME (name))))
    {
      if (TREE_CODE (name) == TYPE_DECL)
	/* Could just call add_name_and_src_coords_attributes here,
	   but since this is a builtin type it doesn't have any
	   useful source coordinates anyway.  */
	name = DECL_NAME (name);
      add_name_attribute (mod_type_die, IDENTIFIER_POINTER (name));
    }
  /* This probably indicates a bug.  */
  else if (mod_type_die && mod_type_die->die_tag == DW_TAG_base_type)
    {
      name = TYPE_IDENTIFIER (type);
      add_name_attribute (mod_type_die,
			  name ? IDENTIFIER_POINTER (name) : "__unknown__");
    }

  if (qualified_type)
    equate_type_number_to_die (qualified_type, mod_type_die);

  if (item_type)
    /* We must do this after the equate_type_number_to_die call, in case
       this is a recursive type.  This ensures that the modified_type_die
       recursion will terminate even if the type is recursive.  Recursive
       types are possible in Ada.  */
    sub_die = modified_type_die (item_type,
				 TYPE_QUALS_NO_ADDR_SPACE (item_type),
				 reverse,
				 context_die);

  if (sub_die != NULL)
    add_AT_die_ref (mod_type_die, DW_AT_type, sub_die);

  add_gnat_descriptive_type_attribute (mod_type_die, type, context_die);
  if (TYPE_ARTIFICIAL (type))
    add_AT_flag (mod_type_die, DW_AT_artificial, 1);

  return mod_type_die;
}

/* Generate DIEs for the generic parameters of T.
   T must be either a generic type or a generic function.
   See http://gcc.gnu.org/wiki/TemplateParmsDwarf for more.  */

static void
gen_generic_params_dies (tree t)
{
  tree parms, args;
  int parms_num, i;
  dw_die_ref die = NULL;
  int non_default;

  if (!t || (TYPE_P (t) && !COMPLETE_TYPE_P (t)))
    return;

  if (TYPE_P (t))
    die = lookup_type_die (t);
  else if (DECL_P (t))
    die = lookup_decl_die (t);

  gcc_assert (die);

  parms = lang_hooks.get_innermost_generic_parms (t);
  if (!parms)
    /* T has no generic parameter. It means T is neither a generic type
       or function. End of story.  */
    return;

  parms_num = TREE_VEC_LENGTH (parms);
  args = lang_hooks.get_innermost_generic_args (t);
  if (TREE_CHAIN (args) && TREE_CODE (TREE_CHAIN (args)) == INTEGER_CST)
    non_default = int_cst_value (TREE_CHAIN (args));
  else
    non_default = TREE_VEC_LENGTH (args);
  for (i = 0; i < parms_num; i++)
    {
      tree parm, arg, arg_pack_elems;
      dw_die_ref parm_die;

      parm = TREE_VEC_ELT (parms, i);
      arg = TREE_VEC_ELT (args, i);
      arg_pack_elems = lang_hooks.types.get_argument_pack_elems (arg);
      gcc_assert (parm && TREE_VALUE (parm) && arg);

      if (parm && TREE_VALUE (parm) && arg)
	{
	  /* If PARM represents a template parameter pack,
	     emit a DW_TAG_GNU_template_parameter_pack DIE, followed
	     by DW_TAG_template_*_parameter DIEs for the argument
	     pack elements of ARG. Note that ARG would then be
	     an argument pack.  */
	  if (arg_pack_elems)
	    parm_die = template_parameter_pack_die (TREE_VALUE (parm),
						    arg_pack_elems,
						    die);
	  else
	    parm_die = generic_parameter_die (TREE_VALUE (parm), arg,
					      true /* emit name */, die);
	  if (i >= non_default)
	    add_AT_flag (parm_die, DW_AT_default_value, 1);
	}
    }
}

/* Create and return a DIE for PARM which should be
   the representation of a generic type parameter.
   For instance, in the C++ front end, PARM would be a template parameter.
   ARG is the argument to PARM.
   EMIT_NAME_P if tree, the DIE will have DW_AT_name attribute set to the
   name of the PARM.
   PARENT_DIE is the parent DIE which the new created DIE should be added to,
   as a child node.  */

static dw_die_ref
generic_parameter_die (tree parm, tree arg,
		       bool emit_name_p,
		       dw_die_ref parent_die)
{
  dw_die_ref tmpl_die = NULL;
  const char *name = NULL;

  if (!parm || !DECL_NAME (parm) || !arg)
    return NULL;

  /* We support non-type generic parameters and arguments,
     type generic parameters and arguments, as well as
     generic generic parameters (a.k.a. template template parameters in C++)
     and arguments.  */
  if (TREE_CODE (parm) == PARM_DECL)
    /* PARM is a nontype generic parameter  */
    tmpl_die = new_die (DW_TAG_template_value_param, parent_die, parm);
  else if (TREE_CODE (parm) == TYPE_DECL)
    /* PARM is a type generic parameter.  */
    tmpl_die = new_die (DW_TAG_template_type_param, parent_die, parm);
  else if (lang_hooks.decls.generic_generic_parameter_decl_p (parm))
    /* PARM is a generic generic parameter.
       Its DIE is a GNU extension. It shall have a
       DW_AT_name attribute to represent the name of the template template
       parameter, and a DW_AT_GNU_template_name attribute to represent the
       name of the template template argument.  */
    tmpl_die = new_die (DW_TAG_GNU_template_template_param,
			parent_die, parm);
  else
    gcc_unreachable ();

  if (tmpl_die)
    {
      tree tmpl_type;

      /* If PARM is a generic parameter pack, it means we are
         emitting debug info for a template argument pack element.
	 In other terms, ARG is a template argument pack element.
	 In that case, we don't emit any DW_AT_name attribute for
	 the die.  */
      if (emit_name_p)
	{
	  name = IDENTIFIER_POINTER (DECL_NAME (parm));
	  gcc_assert (name);
	  add_AT_string (tmpl_die, DW_AT_name, name);
	}

      if (!lang_hooks.decls.generic_generic_parameter_decl_p (parm))
	{
	  /* DWARF3, 5.6.8 says if PARM is a non-type generic parameter
	     TMPL_DIE should have a child DW_AT_type attribute that is set
	     to the type of the argument to PARM, which is ARG.
	     If PARM is a type generic parameter, TMPL_DIE should have a
	     child DW_AT_type that is set to ARG.  */
	  tmpl_type = TYPE_P (arg) ? arg : TREE_TYPE (arg);
	  add_type_attribute (tmpl_die, tmpl_type,
			      (TREE_THIS_VOLATILE (tmpl_type)
			       ? TYPE_QUAL_VOLATILE : TYPE_UNQUALIFIED),
			      false, parent_die);
	}
      else
	{
	  /* So TMPL_DIE is a DIE representing a
	     a generic generic template parameter, a.k.a template template
	     parameter in C++ and arg is a template.  */

	  /* The DW_AT_GNU_template_name attribute of the DIE must be set
	     to the name of the argument.  */
	  name = dwarf2_name (TYPE_P (arg) ? TYPE_NAME (arg) : arg, 1);
	  if (name)
	    add_AT_string (tmpl_die, DW_AT_GNU_template_name, name);
	}

      if (TREE_CODE (parm) == PARM_DECL)
	/* So PARM is a non-type generic parameter.
	   DWARF3 5.6.8 says we must set a DW_AT_const_value child
	   attribute of TMPL_DIE which value represents the value
	   of ARG.
	   We must be careful here:
	   The value of ARG might reference some function decls.
	   We might currently be emitting debug info for a generic
	   type and types are emitted before function decls, we don't
	   know if the function decls referenced by ARG will actually be
	   emitted after cgraph computations.
	   So must defer the generation of the DW_AT_const_value to
	   after cgraph is ready.  */
	append_entry_to_tmpl_value_parm_die_table (tmpl_die, arg);
    }

  return tmpl_die;
}

/* Generate and return a  DW_TAG_GNU_template_parameter_pack DIE representing.
   PARM_PACK must be a template parameter pack. The returned DIE
   will be child DIE of PARENT_DIE.  */

static dw_die_ref
template_parameter_pack_die (tree parm_pack,
			     tree parm_pack_args,
			     dw_die_ref parent_die)
{
  dw_die_ref die;
  int j;

  gcc_assert (parent_die && parm_pack);

  die = new_die (DW_TAG_GNU_template_parameter_pack, parent_die, parm_pack);
  add_name_and_src_coords_attributes (die, parm_pack);
  for (j = 0; j < TREE_VEC_LENGTH (parm_pack_args); j++)
    generic_parameter_die (parm_pack,
			   TREE_VEC_ELT (parm_pack_args, j),
			   false /* Don't emit DW_AT_name */,
			   die);
  return die;
}

/* Given a pointer to an arbitrary ..._TYPE tree node, return true if it is
   an enumerated type.  */

static inline int
type_is_enum (const_tree type)
{
  return TREE_CODE (type) == ENUMERAL_TYPE;
}

/* Return the DBX register number described by a given RTL node.  */

static unsigned int
dbx_reg_number (const_rtx rtl)
{
  unsigned regno = REGNO (rtl);

  gcc_assert (regno < FIRST_PSEUDO_REGISTER);

#ifdef LEAF_REG_REMAP
  if (crtl->uses_only_leaf_regs)
    {
      int leaf_reg = LEAF_REG_REMAP (regno);
      if (leaf_reg != -1)
	regno = (unsigned) leaf_reg;
    }
#endif

  regno = DBX_REGISTER_NUMBER (regno);
  gcc_assert (regno != INVALID_REGNUM);
  return regno;
}

/* Optionally add a DW_OP_piece term to a location description expression.
   DW_OP_piece is only added if the location description expression already
   doesn't end with DW_OP_piece.  */

static void
add_loc_descr_op_piece (dw_loc_descr_ref *list_head, int size)
{
  dw_loc_descr_ref loc;

  if (*list_head != NULL)
    {
      /* Find the end of the chain.  */
      for (loc = *list_head; loc->dw_loc_next != NULL; loc = loc->dw_loc_next)
	;

      if (loc->dw_loc_opc != DW_OP_piece)
	loc->dw_loc_next = new_loc_descr (DW_OP_piece, size, 0);
    }
}

/* Return a location descriptor that designates a machine register or
   zero if there is none.  */

static dw_loc_descr_ref
reg_loc_descriptor (rtx rtl, enum var_init_status initialized)
{
  rtx regs;

  if (REGNO (rtl) >= FIRST_PSEUDO_REGISTER)
    return 0;

  /* We only use "frame base" when we're sure we're talking about the
     post-prologue local stack frame.  We do this by *not* running
     register elimination until this point, and recognizing the special
     argument pointer and soft frame pointer rtx's.
     Use DW_OP_fbreg offset DW_OP_stack_value in this case.  */
  if ((rtl == arg_pointer_rtx || rtl == frame_pointer_rtx)
      && eliminate_regs (rtl, VOIDmode, NULL_RTX) != rtl)
    {
      dw_loc_descr_ref result = NULL;

      if (dwarf_version >= 4 || !dwarf_strict)
	{
	  result = mem_loc_descriptor (rtl, GET_MODE (rtl), VOIDmode,
				       initialized);
	  if (result)
	    add_loc_descr (&result,
			   new_loc_descr (DW_OP_stack_value, 0, 0));
	}
      return result;
    }

  regs = targetm.dwarf_register_span (rtl);

  if (REG_NREGS (rtl) > 1 || regs)
    return multiple_reg_loc_descriptor (rtl, regs, initialized);
  else
    {
      unsigned int dbx_regnum = dbx_reg_number (rtl);
      if (dbx_regnum == IGNORED_DWARF_REGNUM)
	return 0;
      return one_reg_loc_descriptor (dbx_regnum, initialized);
    }
}

/* Return a location descriptor that designates a machine register for
   a given hard register number.  */

static dw_loc_descr_ref
one_reg_loc_descriptor (unsigned int regno, enum var_init_status initialized)
{
  dw_loc_descr_ref reg_loc_descr;

  if (regno <= 31)
    reg_loc_descr
      = new_loc_descr ((enum dwarf_location_atom) (DW_OP_reg0 + regno), 0, 0);
  else
    reg_loc_descr = new_loc_descr (DW_OP_regx, regno, 0);

  if (initialized == VAR_INIT_STATUS_UNINITIALIZED)
    add_loc_descr (&reg_loc_descr, new_loc_descr (DW_OP_GNU_uninit, 0, 0));

  return reg_loc_descr;
}

/* Given an RTL of a register, return a location descriptor that
   designates a value that spans more than one register.  */

static dw_loc_descr_ref
multiple_reg_loc_descriptor (rtx rtl, rtx regs,
			     enum var_init_status initialized)
{
  int size, i;
  dw_loc_descr_ref loc_result = NULL;

  /* Simple, contiguous registers.  */
  if (regs == NULL_RTX)
    {
      unsigned reg = REGNO (rtl);
      int nregs;

#ifdef LEAF_REG_REMAP
      if (crtl->uses_only_leaf_regs)
	{
	  int leaf_reg = LEAF_REG_REMAP (reg);
	  if (leaf_reg != -1)
	    reg = (unsigned) leaf_reg;
	}
#endif

      gcc_assert ((unsigned) DBX_REGISTER_NUMBER (reg) == dbx_reg_number (rtl));
      nregs = REG_NREGS (rtl);

      size = GET_MODE_SIZE (GET_MODE (rtl)) / nregs;

      loc_result = NULL;
      while (nregs--)
	{
	  dw_loc_descr_ref t;

	  t = one_reg_loc_descriptor (DBX_REGISTER_NUMBER (reg),
				      VAR_INIT_STATUS_INITIALIZED);
	  add_loc_descr (&loc_result, t);
	  add_loc_descr_op_piece (&loc_result, size);
	  ++reg;
	}
      return loc_result;
    }

  /* Now onto stupid register sets in non contiguous locations.  */

  gcc_assert (GET_CODE (regs) == PARALLEL);

  size = GET_MODE_SIZE (GET_MODE (XVECEXP (regs, 0, 0)));
  loc_result = NULL;

  for (i = 0; i < XVECLEN (regs, 0); ++i)
    {
      dw_loc_descr_ref t;

      t = one_reg_loc_descriptor (dbx_reg_number (XVECEXP (regs, 0, i)),
				  VAR_INIT_STATUS_INITIALIZED);
      add_loc_descr (&loc_result, t);
      add_loc_descr_op_piece (&loc_result, size);
    }

  if (loc_result && initialized == VAR_INIT_STATUS_UNINITIALIZED)
    add_loc_descr (&loc_result, new_loc_descr (DW_OP_GNU_uninit, 0, 0));
  return loc_result;
}

static unsigned long size_of_int_loc_descriptor (HOST_WIDE_INT);

/* Return a location descriptor that designates a constant i,
   as a compound operation from constant (i >> shift), constant shift
   and DW_OP_shl.  */

static dw_loc_descr_ref
int_shift_loc_descriptor (HOST_WIDE_INT i, int shift)
{
  dw_loc_descr_ref ret = int_loc_descriptor (i >> shift);
  add_loc_descr (&ret, int_loc_descriptor (shift));
  add_loc_descr (&ret, new_loc_descr (DW_OP_shl, 0, 0));
  return ret;
}

/* Return a location descriptor that designates a constant.  */

static dw_loc_descr_ref
int_loc_descriptor (HOST_WIDE_INT i)
{
  enum dwarf_location_atom op;

  /* Pick the smallest representation of a constant, rather than just
     defaulting to the LEB encoding.  */
  if (i >= 0)
    {
      int clz = clz_hwi (i);
      int ctz = ctz_hwi (i);
      if (i <= 31)
	op = (enum dwarf_location_atom) (DW_OP_lit0 + i);
      else if (i <= 0xff)
	op = DW_OP_const1u;
      else if (i <= 0xffff)
	op = DW_OP_const2u;
      else if (clz + ctz >= HOST_BITS_PER_WIDE_INT - 5
	       && clz + 5 + 255 >= HOST_BITS_PER_WIDE_INT)
	/* DW_OP_litX DW_OP_litY DW_OP_shl takes just 3 bytes and
	   DW_OP_litX DW_OP_const1u Y DW_OP_shl takes just 4 bytes,
	   while DW_OP_const4u is 5 bytes.  */
	return int_shift_loc_descriptor (i, HOST_BITS_PER_WIDE_INT - clz - 5);
      else if (clz + ctz >= HOST_BITS_PER_WIDE_INT - 8
	       && clz + 8 + 31 >= HOST_BITS_PER_WIDE_INT)
	/* DW_OP_const1u X DW_OP_litY DW_OP_shl takes just 4 bytes,
	   while DW_OP_const4u is 5 bytes.  */
	return int_shift_loc_descriptor (i, HOST_BITS_PER_WIDE_INT - clz - 8);

      else if (DWARF2_ADDR_SIZE == 4 && i > 0x7fffffff
	       && size_of_int_loc_descriptor ((HOST_WIDE_INT) (int32_t) i)
		  <= 4)
	{
	  /* As i >= 2**31, the double cast above will yield a negative number.
	     Since wrapping is defined in DWARF expressions we can output big
	     positive integers as small negative ones, regardless of the size
	     of host wide ints.

	     Here, since the evaluator will handle 32-bit values and since i >=
	     2**31, we know it's going to be interpreted as a negative literal:
	     store it this way if we can do better than 5 bytes this way.  */
	  return int_loc_descriptor ((HOST_WIDE_INT) (int32_t) i);
	}
      else if (HOST_BITS_PER_WIDE_INT == 32 || i <= 0xffffffff)
	op = DW_OP_const4u;

      /* Past this point, i >= 0x100000000 and thus DW_OP_constu will take at
	 least 6 bytes: see if we can do better before falling back to it.  */
      else if (clz + ctz >= HOST_BITS_PER_WIDE_INT - 8
	       && clz + 8 + 255 >= HOST_BITS_PER_WIDE_INT)
	/* DW_OP_const1u X DW_OP_const1u Y DW_OP_shl takes just 5 bytes.  */
	return int_shift_loc_descriptor (i, HOST_BITS_PER_WIDE_INT - clz - 8);
      else if (clz + ctz >= HOST_BITS_PER_WIDE_INT - 16
	       && clz + 16 + (size_of_uleb128 (i) > 5 ? 255 : 31)
		  >= HOST_BITS_PER_WIDE_INT)
	/* DW_OP_const2u X DW_OP_litY DW_OP_shl takes just 5 bytes,
	   DW_OP_const2u X DW_OP_const1u Y DW_OP_shl takes 6 bytes.  */
	return int_shift_loc_descriptor (i, HOST_BITS_PER_WIDE_INT - clz - 16);
      else if (clz + ctz >= HOST_BITS_PER_WIDE_INT - 32
	       && clz + 32 + 31 >= HOST_BITS_PER_WIDE_INT
	       && size_of_uleb128 (i) > 6)
	/* DW_OP_const4u X DW_OP_litY DW_OP_shl takes just 7 bytes.  */
	return int_shift_loc_descriptor (i, HOST_BITS_PER_WIDE_INT - clz - 32);
      else
	op = DW_OP_constu;
    }
  else
    {
      if (i >= -0x80)
	op = DW_OP_const1s;
      else if (i >= -0x8000)
	op = DW_OP_const2s;
      else if (HOST_BITS_PER_WIDE_INT == 32 || i >= -0x80000000)
	{
	  if (size_of_int_loc_descriptor (i) < 5)
	    {
	      dw_loc_descr_ref ret = int_loc_descriptor (-i);
	      add_loc_descr (&ret, new_loc_descr (DW_OP_neg, 0, 0));
	      return ret;
	    }
	  op = DW_OP_const4s;
	}
      else
	{
	  if (size_of_int_loc_descriptor (i)
	      < (unsigned long) 1 + size_of_sleb128 (i))
	    {
	      dw_loc_descr_ref ret = int_loc_descriptor (-i);
	      add_loc_descr (&ret, new_loc_descr (DW_OP_neg, 0, 0));
	      return ret;
	    }
	  op = DW_OP_consts;
	}
    }

  return new_loc_descr (op, i, 0);
}

/* Likewise, for unsigned constants.  */

static dw_loc_descr_ref
uint_loc_descriptor (unsigned HOST_WIDE_INT i)
{
  const unsigned HOST_WIDE_INT max_int = INTTYPE_MAXIMUM (HOST_WIDE_INT);
  const unsigned HOST_WIDE_INT max_uint
    = INTTYPE_MAXIMUM (unsigned HOST_WIDE_INT);

  /* If possible, use the clever signed constants handling.  */
  if (i <= max_int)
    return int_loc_descriptor ((HOST_WIDE_INT) i);

  /* Here, we are left with positive numbers that cannot be represented as
     HOST_WIDE_INT, i.e.:
         max (HOST_WIDE_INT) < i <= max (unsigned HOST_WIDE_INT)

     Using DW_OP_const4/8/./u operation to encode them consumes a lot of bytes
     whereas may be better to output a negative integer: thanks to integer
     wrapping, we know that:
         x = x - 2 ** DWARF2_ADDR_SIZE
	   = x - 2 * (max (HOST_WIDE_INT) + 1)
     So numbers close to max (unsigned HOST_WIDE_INT) could be represented as
     small negative integers.  Let's try that in cases it will clearly improve
     the encoding: there is no gain turning DW_OP_const4u into
     DW_OP_const4s.  */
  if (DWARF2_ADDR_SIZE * 8 == HOST_BITS_PER_WIDE_INT
      && ((DWARF2_ADDR_SIZE == 4 && i > max_uint - 0x8000)
	  || (DWARF2_ADDR_SIZE == 8 && i > max_uint - 0x80000000)))
    {
      const unsigned HOST_WIDE_INT first_shift = i - max_int - 1;

      /* Now, -1 <  first_shift <= max (HOST_WIDE_INT)
	 i.e.  0 <= first_shift <= max (HOST_WIDE_INT).  */
      const HOST_WIDE_INT second_shift
        = (HOST_WIDE_INT) first_shift - (HOST_WIDE_INT) max_int - 1;

      /* So we finally have:
	      -max (HOST_WIDE_INT) - 1 <= second_shift <= -1.
	 i.e.  min (HOST_WIDE_INT)     <= second_shift <  0.  */
      return int_loc_descriptor (second_shift);
    }

  /* Last chance: fallback to a simple constant operation.  */
  return new_loc_descr
     ((HOST_BITS_PER_WIDE_INT == 32 || i <= 0xffffffff)
      ? DW_OP_const4u
      : DW_OP_const8u,
      i, 0);
}

/* Generate and return a location description that computes the unsigned
   comparison of the two stack top entries (a OP b where b is the top-most
   entry and a is the second one).  The KIND of comparison can be LT_EXPR,
   LE_EXPR, GT_EXPR or GE_EXPR.  */

static dw_loc_descr_ref
uint_comparison_loc_list (enum tree_code kind)
{
  enum dwarf_location_atom op, flip_op;
  dw_loc_descr_ref ret, bra_node, jmp_node, tmp;

  switch (kind)
    {
    case LT_EXPR:
      op = DW_OP_lt;
      break;
    case LE_EXPR:
      op = DW_OP_le;
      break;
    case GT_EXPR:
      op = DW_OP_gt;
      break;
    case GE_EXPR:
      op = DW_OP_ge;
      break;
    default:
      gcc_unreachable ();
    }

  bra_node = new_loc_descr (DW_OP_bra, 0, 0);
  jmp_node = new_loc_descr (DW_OP_skip, 0, 0);

  /* Until DWARFv4, operations all work on signed integers.  It is nevertheless
     possible to perform unsigned comparisons: we just have to distinguish
     three cases:

       1. when a and b have the same sign (as signed integers); then we should
	  return: a OP(signed) b;

       2. when a is a negative signed integer while b is a positive one, then a
	  is a greater unsigned integer than b; likewise when a and b's roles
	  are flipped.

     So first, compare the sign of the two operands.  */
  ret = new_loc_descr (DW_OP_over, 0, 0);
  add_loc_descr (&ret, new_loc_descr (DW_OP_over, 0, 0));
  add_loc_descr (&ret, new_loc_descr (DW_OP_xor, 0, 0));
  /* If they have different signs (i.e. they have different sign bits), then
     the stack top value has now the sign bit set and thus it's smaller than
     zero.  */
  add_loc_descr (&ret, new_loc_descr (DW_OP_lit0, 0, 0));
  add_loc_descr (&ret, new_loc_descr (DW_OP_lt, 0, 0));
  add_loc_descr (&ret, bra_node);

  /* We are in case 1.  At this point, we know both operands have the same
     sign, to it's safe to use the built-in signed comparison.  */
  add_loc_descr (&ret, new_loc_descr (op, 0, 0));
  add_loc_descr (&ret, jmp_node);

  /* We are in case 2.  Here, we know both operands do not have the same sign,
     so we have to flip the signed comparison.  */
  flip_op = (kind == LT_EXPR || kind == LE_EXPR) ? DW_OP_gt : DW_OP_lt;
  tmp = new_loc_descr (flip_op, 0, 0);
  bra_node->dw_loc_oprnd1.val_class = dw_val_class_loc;
  bra_node->dw_loc_oprnd1.v.val_loc = tmp;
  add_loc_descr (&ret, tmp);

  /* This dummy operation is necessary to make the two branches join.  */
  tmp = new_loc_descr (DW_OP_nop, 0, 0);
  jmp_node->dw_loc_oprnd1.val_class = dw_val_class_loc;
  jmp_node->dw_loc_oprnd1.v.val_loc = tmp;
  add_loc_descr (&ret, tmp);

  return ret;
}

/* Likewise, but takes the location description lists (might be destructive on
   them).  Return NULL if either is NULL or if concatenation fails.  */

static dw_loc_list_ref
loc_list_from_uint_comparison (dw_loc_list_ref left, dw_loc_list_ref right,
			       enum tree_code kind)
{
  if (left == NULL || right == NULL)
    return NULL;

  add_loc_list (&left, right);
  if (left == NULL)
    return NULL;

  add_loc_descr_to_each (left, uint_comparison_loc_list (kind));
  return left;
}

/* Return size_of_locs (int_shift_loc_descriptor (i, shift))
   without actually allocating it.  */

static unsigned long
size_of_int_shift_loc_descriptor (HOST_WIDE_INT i, int shift)
{
  return size_of_int_loc_descriptor (i >> shift)
	 + size_of_int_loc_descriptor (shift)
	 + 1;
}

/* Return size_of_locs (int_loc_descriptor (i)) without
   actually allocating it.  */

static unsigned long
size_of_int_loc_descriptor (HOST_WIDE_INT i)
{
  unsigned long s;

  if (i >= 0)
    {
      int clz, ctz;
      if (i <= 31)
	return 1;
      else if (i <= 0xff)
	return 2;
      else if (i <= 0xffff)
	return 3;
      clz = clz_hwi (i);
      ctz = ctz_hwi (i);
      if (clz + ctz >= HOST_BITS_PER_WIDE_INT - 5
	  && clz + 5 + 255 >= HOST_BITS_PER_WIDE_INT)
	return size_of_int_shift_loc_descriptor (i, HOST_BITS_PER_WIDE_INT
						    - clz - 5);
      else if (clz + ctz >= HOST_BITS_PER_WIDE_INT - 8
	       && clz + 8 + 31 >= HOST_BITS_PER_WIDE_INT)
	return size_of_int_shift_loc_descriptor (i, HOST_BITS_PER_WIDE_INT
						    - clz - 8);
      else if (DWARF2_ADDR_SIZE == 4 && i > 0x7fffffff
	       && size_of_int_loc_descriptor ((HOST_WIDE_INT) (int32_t) i)
		  <= 4)
	return size_of_int_loc_descriptor ((HOST_WIDE_INT) (int32_t) i);
      else if (HOST_BITS_PER_WIDE_INT == 32 || i <= 0xffffffff)
	return 5;
      s = size_of_uleb128 ((unsigned HOST_WIDE_INT) i);
      if (clz + ctz >= HOST_BITS_PER_WIDE_INT - 8
	  && clz + 8 + 255 >= HOST_BITS_PER_WIDE_INT)
	return size_of_int_shift_loc_descriptor (i, HOST_BITS_PER_WIDE_INT
						    - clz - 8);
      else if (clz + ctz >= HOST_BITS_PER_WIDE_INT - 16
	       && clz + 16 + (s > 5 ? 255 : 31) >= HOST_BITS_PER_WIDE_INT)
	return size_of_int_shift_loc_descriptor (i, HOST_BITS_PER_WIDE_INT
						    - clz - 16);
      else if (clz + ctz >= HOST_BITS_PER_WIDE_INT - 32
	       && clz + 32 + 31 >= HOST_BITS_PER_WIDE_INT
	       && s > 6)
	return size_of_int_shift_loc_descriptor (i, HOST_BITS_PER_WIDE_INT
						    - clz - 32);
      else
	return 1 + s;
    }
  else
    {
      if (i >= -0x80)
	return 2;
      else if (i >= -0x8000)
	return 3;
      else if (HOST_BITS_PER_WIDE_INT == 32 || i >= -0x80000000)
	{
	  if (-(unsigned HOST_WIDE_INT) i != (unsigned HOST_WIDE_INT) i)
	    {
	      s = size_of_int_loc_descriptor (-i) + 1;
	      if (s < 5)
		return s;
	    }
	  return 5;
	}
      else
	{
	  unsigned long r = 1 + size_of_sleb128 (i);
	  if (-(unsigned HOST_WIDE_INT) i != (unsigned HOST_WIDE_INT) i)
	    {
	      s = size_of_int_loc_descriptor (-i) + 1;
	      if (s < r)
		return s;
	    }
	  return r;
	}
    }
}

/* Return loc description representing "address" of integer value.
   This can appear only as toplevel expression.  */

static dw_loc_descr_ref
address_of_int_loc_descriptor (int size, HOST_WIDE_INT i)
{
  int litsize;
  dw_loc_descr_ref loc_result = NULL;

  if (!(dwarf_version >= 4 || !dwarf_strict))
    return NULL;

  litsize = size_of_int_loc_descriptor (i);
  /* Determine if DW_OP_stack_value or DW_OP_implicit_value
     is more compact.  For DW_OP_stack_value we need:
     litsize + 1 (DW_OP_stack_value)
     and for DW_OP_implicit_value:
     1 (DW_OP_implicit_value) + 1 (length) + size.  */
  if ((int) DWARF2_ADDR_SIZE >= size && litsize + 1 <= 1 + 1 + size)
    {
      loc_result = int_loc_descriptor (i);
      add_loc_descr (&loc_result,
		     new_loc_descr (DW_OP_stack_value, 0, 0));
      return loc_result;
    }

  loc_result = new_loc_descr (DW_OP_implicit_value,
			      size, 0);
  loc_result->dw_loc_oprnd2.val_class = dw_val_class_const;
  loc_result->dw_loc_oprnd2.v.val_int = i;
  return loc_result;
}

/* Return a location descriptor that designates a base+offset location.  */

static dw_loc_descr_ref
based_loc_descr (rtx reg, HOST_WIDE_INT offset,
		 enum var_init_status initialized)
{
  unsigned int regno;
  dw_loc_descr_ref result;
  dw_fde_ref fde = cfun->fde;

  /* We only use "frame base" when we're sure we're talking about the
     post-prologue local stack frame.  We do this by *not* running
     register elimination until this point, and recognizing the special
     argument pointer and soft frame pointer rtx's.  */
  if (reg == arg_pointer_rtx || reg == frame_pointer_rtx)
    {
      rtx elim = (ira_use_lra_p
		  ? lra_eliminate_regs (reg, VOIDmode, NULL_RTX)
		  : eliminate_regs (reg, VOIDmode, NULL_RTX));

      if (elim != reg)
	{
	  if (GET_CODE (elim) == PLUS)
	    {
	      offset += INTVAL (XEXP (elim, 1));
	      elim = XEXP (elim, 0);
	    }
	  gcc_assert ((SUPPORTS_STACK_ALIGNMENT
		       && (elim == hard_frame_pointer_rtx
			   || elim == stack_pointer_rtx))
	              || elim == (frame_pointer_needed
				  ? hard_frame_pointer_rtx
				  : stack_pointer_rtx));

	  /* If drap register is used to align stack, use frame
	     pointer + offset to access stack variables.  If stack
	     is aligned without drap, use stack pointer + offset to
	     access stack variables.  */
	  if (crtl->stack_realign_tried
	      && reg == frame_pointer_rtx)
	    {
	      int base_reg
		= DWARF_FRAME_REGNUM ((fde && fde->drap_reg != INVALID_REGNUM)
				      ? HARD_FRAME_POINTER_REGNUM
				      : REGNO (elim));
	      return new_reg_loc_descr (base_reg, offset);
	    }

	  gcc_assert (frame_pointer_fb_offset_valid);
	  offset += frame_pointer_fb_offset;
	  return new_loc_descr (DW_OP_fbreg, offset, 0);
	}
    }

  regno = REGNO (reg);
#ifdef LEAF_REG_REMAP
  if (crtl->uses_only_leaf_regs)
    {
      int leaf_reg = LEAF_REG_REMAP (regno);
      if (leaf_reg != -1)
	regno = (unsigned) leaf_reg;
    }
#endif
  regno = DWARF_FRAME_REGNUM (regno);

  if (!optimize && fde
      && (fde->drap_reg == regno || fde->vdrap_reg == regno))
    {
      /* Use cfa+offset to represent the location of arguments passed
	 on the stack when drap is used to align stack.
	 Only do this when not optimizing, for optimized code var-tracking
	 is supposed to track where the arguments live and the register
	 used as vdrap or drap in some spot might be used for something
	 else in other part of the routine.  */
      return new_loc_descr (DW_OP_fbreg, offset, 0);
    }

  if (regno <= 31)
    result = new_loc_descr ((enum dwarf_location_atom) (DW_OP_breg0 + regno),
			    offset, 0);
  else
    result = new_loc_descr (DW_OP_bregx, regno, offset);

  if (initialized == VAR_INIT_STATUS_UNINITIALIZED)
    add_loc_descr (&result, new_loc_descr (DW_OP_GNU_uninit, 0, 0));

  return result;
}

/* Return true if this RTL expression describes a base+offset calculation.  */

static inline int
is_based_loc (const_rtx rtl)
{
  return (GET_CODE (rtl) == PLUS
	  && ((REG_P (XEXP (rtl, 0))
	       && REGNO (XEXP (rtl, 0)) < FIRST_PSEUDO_REGISTER
	       && CONST_INT_P (XEXP (rtl, 1)))));
}

/* Try to handle TLS MEMs, for which mem_loc_descriptor on XEXP (mem, 0)
   failed.  */

static dw_loc_descr_ref
tls_mem_loc_descriptor (rtx mem)
{
  tree base;
  dw_loc_descr_ref loc_result;

  if (MEM_EXPR (mem) == NULL_TREE || !MEM_OFFSET_KNOWN_P (mem))
    return NULL;

  base = get_base_address (MEM_EXPR (mem));
  if (base == NULL
      || !VAR_P (base)
      || !DECL_THREAD_LOCAL_P (base))
    return NULL;

  loc_result = loc_descriptor_from_tree (MEM_EXPR (mem), 1, NULL);
  if (loc_result == NULL)
    return NULL;

  if (MEM_OFFSET (mem))
    loc_descr_plus_const (&loc_result, MEM_OFFSET (mem));

  return loc_result;
}

/* Output debug info about reason why we failed to expand expression as dwarf
   expression.  */

static void
expansion_failed (tree expr, rtx rtl, char const *reason)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Failed to expand as dwarf: ");
      if (expr)
	print_generic_expr (dump_file, expr, dump_flags);
      if (rtl)
	{
	  fprintf (dump_file, "\n");
	  print_rtl (dump_file, rtl);
	}
      fprintf (dump_file, "\nReason: %s\n", reason);
    }
}

/* Helper function for const_ok_for_output.  */

static bool
const_ok_for_output_1 (rtx rtl)
{
  if (GET_CODE (rtl) == UNSPEC)
    {
      /* If delegitimize_address couldn't do anything with the UNSPEC, assume
	 we can't express it in the debug info.  */
      /* Don't complain about TLS UNSPECs, those are just too hard to
	 delegitimize.  Note this could be a non-decl SYMBOL_REF such as
	 one in a constant pool entry, so testing SYMBOL_REF_TLS_MODEL
	 rather than DECL_THREAD_LOCAL_P is not just an optimization.  */
      if (flag_checking
	  && (XVECLEN (rtl, 0) == 0
	      || GET_CODE (XVECEXP (rtl, 0, 0)) != SYMBOL_REF
	      || SYMBOL_REF_TLS_MODEL (XVECEXP (rtl, 0, 0)) == TLS_MODEL_NONE))
	inform (current_function_decl
		? DECL_SOURCE_LOCATION (current_function_decl)
		: UNKNOWN_LOCATION,
#if NUM_UNSPEC_VALUES > 0
		"non-delegitimized UNSPEC %s (%d) found in variable location",
		((XINT (rtl, 1) >= 0 && XINT (rtl, 1) < NUM_UNSPEC_VALUES)
		 ? unspec_strings[XINT (rtl, 1)] : "unknown"),
		XINT (rtl, 1));
#else
		"non-delegitimized UNSPEC %d found in variable location",
		XINT (rtl, 1));
#endif
      expansion_failed (NULL_TREE, rtl,
			"UNSPEC hasn't been delegitimized.\n");
      return false;
    }

  if (targetm.const_not_ok_for_debug_p (rtl))
    {
      expansion_failed (NULL_TREE, rtl,
			"Expression rejected for debug by the backend.\n");
      return false;
    }

  /* FIXME: Refer to PR60655. It is possible for simplification
     of rtl expressions in var tracking to produce such expressions.
     We should really identify / validate expressions
     enclosed in CONST that can be handled by assemblers on various
     targets and only handle legitimate cases here.  */
  if (GET_CODE (rtl) != SYMBOL_REF)
    {
      if (GET_CODE (rtl) == NOT)
	return false;
      return true;
    }

  if (CONSTANT_POOL_ADDRESS_P (rtl))
    {
      bool marked;
      get_pool_constant_mark (rtl, &marked);
      /* If all references to this pool constant were optimized away,
	 it was not output and thus we can't represent it.  */
      if (!marked)
	{
	  expansion_failed (NULL_TREE, rtl,
			    "Constant was removed from constant pool.\n");
	  return false;
	}
    }

  if (SYMBOL_REF_TLS_MODEL (rtl) != TLS_MODEL_NONE)
    return false;

  /* Avoid references to external symbols in debug info, on several targets
     the linker might even refuse to link when linking a shared library,
     and in many other cases the relocations for .debug_info/.debug_loc are
     dropped, so the address becomes zero anyway.  Hidden symbols, guaranteed
     to be defined within the same shared library or executable are fine.  */
  if (SYMBOL_REF_EXTERNAL_P (rtl))
    {
      tree decl = SYMBOL_REF_DECL (rtl);

      if (decl == NULL || !targetm.binds_local_p (decl))
	{
	  expansion_failed (NULL_TREE, rtl,
			    "Symbol not defined in current TU.\n");
	  return false;
	}
    }

  return true;
}

/* Return true if constant RTL can be emitted in DW_OP_addr or
   DW_AT_const_value.  TLS SYMBOL_REFs, external SYMBOL_REFs or
   non-marked constant pool SYMBOL_REFs can't be referenced in it.  */

static bool
const_ok_for_output (rtx rtl)
{
  if (GET_CODE (rtl) == SYMBOL_REF)
    return const_ok_for_output_1 (rtl);

  if (GET_CODE (rtl) == CONST)
    {
      subrtx_var_iterator::array_type array;
      FOR_EACH_SUBRTX_VAR (iter, array, XEXP (rtl, 0), ALL)
	if (!const_ok_for_output_1 (*iter))
	  return false;
      return true;
    }

  return true;
}

/* Return a reference to DW_TAG_base_type corresponding to MODE and UNSIGNEDP
   if possible, NULL otherwise.  */

static dw_die_ref
base_type_for_mode (machine_mode mode, bool unsignedp)
{
  dw_die_ref type_die;
  tree type = lang_hooks.types.type_for_mode (mode, unsignedp);

  if (type == NULL)
    return NULL;
  switch (TREE_CODE (type))
    {
    case INTEGER_TYPE:
    case REAL_TYPE:
      break;
    default:
      return NULL;
    }
  type_die = lookup_type_die (type);
  if (!type_die)
    type_die = modified_type_die (type, TYPE_UNQUALIFIED, false,
				  comp_unit_die ());
  if (type_die == NULL || type_die->die_tag != DW_TAG_base_type)
    return NULL;
  return type_die;
}

/* For OP descriptor assumed to be in unsigned MODE, convert it to a unsigned
   type matching MODE, or, if MODE is narrower than or as wide as
   DWARF2_ADDR_SIZE, untyped.  Return NULL if the conversion is not
   possible.  */

static dw_loc_descr_ref
convert_descriptor_to_mode (machine_mode mode, dw_loc_descr_ref op)
{
  machine_mode outer_mode = mode;
  dw_die_ref type_die;
  dw_loc_descr_ref cvt;

  if (GET_MODE_SIZE (mode) <= DWARF2_ADDR_SIZE)
    {
      add_loc_descr (&op, new_loc_descr (dwarf_OP (DW_OP_convert), 0, 0));
      return op;
    }
  type_die = base_type_for_mode (outer_mode, 1);
  if (type_die == NULL)
    return NULL;
  cvt = new_loc_descr (dwarf_OP (DW_OP_convert), 0, 0);
  cvt->dw_loc_oprnd1.val_class = dw_val_class_die_ref;
  cvt->dw_loc_oprnd1.v.val_die_ref.die = type_die;
  cvt->dw_loc_oprnd1.v.val_die_ref.external = 0;
  add_loc_descr (&op, cvt);
  return op;
}

/* Return location descriptor for comparison OP with operands OP0 and OP1.  */

static dw_loc_descr_ref
compare_loc_descriptor (enum dwarf_location_atom op, dw_loc_descr_ref op0,
			dw_loc_descr_ref op1)
{
  dw_loc_descr_ref ret = op0;
  add_loc_descr (&ret, op1);
  add_loc_descr (&ret, new_loc_descr (op, 0, 0));
  if (STORE_FLAG_VALUE != 1)
    {
      add_loc_descr (&ret, int_loc_descriptor (STORE_FLAG_VALUE));
      add_loc_descr (&ret, new_loc_descr (DW_OP_mul, 0, 0));
    }
  return ret;
}

/* Return location descriptor for signed comparison OP RTL.  */

static dw_loc_descr_ref
scompare_loc_descriptor (enum dwarf_location_atom op, rtx rtl,
			 machine_mode mem_mode)
{
  machine_mode op_mode = GET_MODE (XEXP (rtl, 0));
  dw_loc_descr_ref op0, op1;
  int shift;

  if (op_mode == VOIDmode)
    op_mode = GET_MODE (XEXP (rtl, 1));
  if (op_mode == VOIDmode)
    return NULL;

  if (dwarf_strict
      && dwarf_version < 5
      && (!SCALAR_INT_MODE_P (op_mode)
	  || GET_MODE_SIZE (op_mode) > DWARF2_ADDR_SIZE))
    return NULL;

  op0 = mem_loc_descriptor (XEXP (rtl, 0), op_mode, mem_mode,
			    VAR_INIT_STATUS_INITIALIZED);
  op1 = mem_loc_descriptor (XEXP (rtl, 1), op_mode, mem_mode,
			    VAR_INIT_STATUS_INITIALIZED);

  if (op0 == NULL || op1 == NULL)
    return NULL;

  if (!SCALAR_INT_MODE_P (op_mode)
      || GET_MODE_SIZE (op_mode) == DWARF2_ADDR_SIZE)
    return compare_loc_descriptor (op, op0, op1);

  if (GET_MODE_SIZE (op_mode) > DWARF2_ADDR_SIZE)
    {
      dw_die_ref type_die = base_type_for_mode (op_mode, 0);
      dw_loc_descr_ref cvt;

      if (type_die == NULL)
	return NULL;
      cvt = new_loc_descr (dwarf_OP (DW_OP_convert), 0, 0);
      cvt->dw_loc_oprnd1.val_class = dw_val_class_die_ref;
      cvt->dw_loc_oprnd1.v.val_die_ref.die = type_die;
      cvt->dw_loc_oprnd1.v.val_die_ref.external = 0;
      add_loc_descr (&op0, cvt);
      cvt = new_loc_descr (dwarf_OP (DW_OP_convert), 0, 0);
      cvt->dw_loc_oprnd1.val_class = dw_val_class_die_ref;
      cvt->dw_loc_oprnd1.v.val_die_ref.die = type_die;
      cvt->dw_loc_oprnd1.v.val_die_ref.external = 0;
      add_loc_descr (&op1, cvt);
      return compare_loc_descriptor (op, op0, op1);
    }

  shift = (DWARF2_ADDR_SIZE - GET_MODE_SIZE (op_mode)) * BITS_PER_UNIT;
  /* For eq/ne, if the operands are known to be zero-extended,
     there is no need to do the fancy shifting up.  */
  if (op == DW_OP_eq || op == DW_OP_ne)
    {
      dw_loc_descr_ref last0, last1;
      for (last0 = op0; last0->dw_loc_next != NULL; last0 = last0->dw_loc_next)
	;
      for (last1 = op1; last1->dw_loc_next != NULL; last1 = last1->dw_loc_next)
	;
      /* deref_size zero extends, and for constants we can check
	 whether they are zero extended or not.  */
      if (((last0->dw_loc_opc == DW_OP_deref_size
	    && last0->dw_loc_oprnd1.v.val_int <= GET_MODE_SIZE (op_mode))
	   || (CONST_INT_P (XEXP (rtl, 0))
	       && (unsigned HOST_WIDE_INT) INTVAL (XEXP (rtl, 0))
		  == (INTVAL (XEXP (rtl, 0)) & GET_MODE_MASK (op_mode))))
	  && ((last1->dw_loc_opc == DW_OP_deref_size
	       && last1->dw_loc_oprnd1.v.val_int <= GET_MODE_SIZE (op_mode))
	      || (CONST_INT_P (XEXP (rtl, 1))
		  && (unsigned HOST_WIDE_INT) INTVAL (XEXP (rtl, 1))
		     == (INTVAL (XEXP (rtl, 1)) & GET_MODE_MASK (op_mode)))))
	return compare_loc_descriptor (op, op0, op1);

      /* EQ/NE comparison against constant in narrower type than
	 DWARF2_ADDR_SIZE can be performed either as
	 DW_OP_const1u <shift> DW_OP_shl DW_OP_const* <cst << shift>
	 DW_OP_{eq,ne}
	 or
	 DW_OP_const*u <mode_mask> DW_OP_and DW_OP_const* <cst & mode_mask>
	 DW_OP_{eq,ne}.  Pick whatever is shorter.  */
      if (CONST_INT_P (XEXP (rtl, 1))
	  && GET_MODE_BITSIZE (op_mode) < HOST_BITS_PER_WIDE_INT
	  && (size_of_int_loc_descriptor (shift) + 1
	      + size_of_int_loc_descriptor (UINTVAL (XEXP (rtl, 1)) << shift)
	      >= size_of_int_loc_descriptor (GET_MODE_MASK (op_mode)) + 1
		 + size_of_int_loc_descriptor (INTVAL (XEXP (rtl, 1))
					       & GET_MODE_MASK (op_mode))))
	{
	  add_loc_descr (&op0, int_loc_descriptor (GET_MODE_MASK (op_mode)));
	  add_loc_descr (&op0, new_loc_descr (DW_OP_and, 0, 0));
	  op1 = int_loc_descriptor (INTVAL (XEXP (rtl, 1))
				    & GET_MODE_MASK (op_mode));
	  return compare_loc_descriptor (op, op0, op1);
	}
    }
  add_loc_descr (&op0, int_loc_descriptor (shift));
  add_loc_descr (&op0, new_loc_descr (DW_OP_shl, 0, 0));
  if (CONST_INT_P (XEXP (rtl, 1)))
    op1 = int_loc_descriptor (UINTVAL (XEXP (rtl, 1)) << shift);
  else
    {
      add_loc_descr (&op1, int_loc_descriptor (shift));
      add_loc_descr (&op1, new_loc_descr (DW_OP_shl, 0, 0));
    }
  return compare_loc_descriptor (op, op0, op1);
}

/* Return location descriptor for unsigned comparison OP RTL.  */

static dw_loc_descr_ref
ucompare_loc_descriptor (enum dwarf_location_atom op, rtx rtl,
			 machine_mode mem_mode)
{
  machine_mode op_mode = GET_MODE (XEXP (rtl, 0));
  dw_loc_descr_ref op0, op1;

  if (op_mode == VOIDmode)
    op_mode = GET_MODE (XEXP (rtl, 1));
  if (op_mode == VOIDmode)
    return NULL;
  if (!SCALAR_INT_MODE_P (op_mode))
    return NULL;

  if (dwarf_strict
      && dwarf_version < 5
      && GET_MODE_SIZE (op_mode) > DWARF2_ADDR_SIZE)
    return NULL;

  op0 = mem_loc_descriptor (XEXP (rtl, 0), op_mode, mem_mode,
			    VAR_INIT_STATUS_INITIALIZED);
  op1 = mem_loc_descriptor (XEXP (rtl, 1), op_mode, mem_mode,
			    VAR_INIT_STATUS_INITIALIZED);

  if (op0 == NULL || op1 == NULL)
    return NULL;

  if (GET_MODE_SIZE (op_mode) < DWARF2_ADDR_SIZE)
    {
      HOST_WIDE_INT mask = GET_MODE_MASK (op_mode);
      dw_loc_descr_ref last0, last1;
      for (last0 = op0; last0->dw_loc_next != NULL; last0 = last0->dw_loc_next)
	;
      for (last1 = op1; last1->dw_loc_next != NULL; last1 = last1->dw_loc_next)
	;
      if (CONST_INT_P (XEXP (rtl, 0)))
	op0 = int_loc_descriptor (INTVAL (XEXP (rtl, 0)) & mask);
      /* deref_size zero extends, so no need to mask it again.  */
      else if (last0->dw_loc_opc != DW_OP_deref_size
	       || last0->dw_loc_oprnd1.v.val_int > GET_MODE_SIZE (op_mode))
	{
	  add_loc_descr (&op0, int_loc_descriptor (mask));
	  add_loc_descr (&op0, new_loc_descr (DW_OP_and, 0, 0));
	}
      if (CONST_INT_P (XEXP (rtl, 1)))
	op1 = int_loc_descriptor (INTVAL (XEXP (rtl, 1)) & mask);
      /* deref_size zero extends, so no need to mask it again.  */
      else if (last1->dw_loc_opc != DW_OP_deref_size
	       || last1->dw_loc_oprnd1.v.val_int > GET_MODE_SIZE (op_mode))
	{
	  add_loc_descr (&op1, int_loc_descriptor (mask));
	  add_loc_descr (&op1, new_loc_descr (DW_OP_and, 0, 0));
	}
    }
  else if (GET_MODE_SIZE (op_mode) == DWARF2_ADDR_SIZE)
    {
      HOST_WIDE_INT bias = 1;
      bias <<= (DWARF2_ADDR_SIZE * BITS_PER_UNIT - 1);
      add_loc_descr (&op0, new_loc_descr (DW_OP_plus_uconst, bias, 0));
      if (CONST_INT_P (XEXP (rtl, 1)))
	op1 = int_loc_descriptor ((unsigned HOST_WIDE_INT) bias
				  + INTVAL (XEXP (rtl, 1)));
      else
	add_loc_descr (&op1, new_loc_descr (DW_OP_plus_uconst,
					    bias, 0));
    }
  return compare_loc_descriptor (op, op0, op1);
}

/* Return location descriptor for {U,S}{MIN,MAX}.  */

static dw_loc_descr_ref
minmax_loc_descriptor (rtx rtl, machine_mode mode,
		       machine_mode mem_mode)
{
  enum dwarf_location_atom op;
  dw_loc_descr_ref op0, op1, ret;
  dw_loc_descr_ref bra_node, drop_node;

  if (dwarf_strict
      && dwarf_version < 5
      && (!SCALAR_INT_MODE_P (mode)
	  || GET_MODE_SIZE (mode) > DWARF2_ADDR_SIZE))
    return NULL;

  op0 = mem_loc_descriptor (XEXP (rtl, 0), mode, mem_mode,
			    VAR_INIT_STATUS_INITIALIZED);
  op1 = mem_loc_descriptor (XEXP (rtl, 1), mode, mem_mode,
			    VAR_INIT_STATUS_INITIALIZED);

  if (op0 == NULL || op1 == NULL)
    return NULL;

  add_loc_descr (&op0, new_loc_descr (DW_OP_dup, 0, 0));
  add_loc_descr (&op1, new_loc_descr (DW_OP_swap, 0, 0));
  add_loc_descr (&op1, new_loc_descr (DW_OP_over, 0, 0));
  if (GET_CODE (rtl) == UMIN || GET_CODE (rtl) == UMAX)
    {
      if (GET_MODE_SIZE (mode) < DWARF2_ADDR_SIZE)
	{
	  HOST_WIDE_INT mask = GET_MODE_MASK (mode);
	  add_loc_descr (&op0, int_loc_descriptor (mask));
	  add_loc_descr (&op0, new_loc_descr (DW_OP_and, 0, 0));
	  add_loc_descr (&op1, int_loc_descriptor (mask));
	  add_loc_descr (&op1, new_loc_descr (DW_OP_and, 0, 0));
	}
      else if (GET_MODE_SIZE (mode) == DWARF2_ADDR_SIZE)
	{
	  HOST_WIDE_INT bias = 1;
	  bias <<= (DWARF2_ADDR_SIZE * BITS_PER_UNIT - 1);
	  add_loc_descr (&op0, new_loc_descr (DW_OP_plus_uconst, bias, 0));
	  add_loc_descr (&op1, new_loc_descr (DW_OP_plus_uconst, bias, 0));
	}
    }
  else if (!SCALAR_INT_MODE_P (mode)
	   && GET_MODE_SIZE (mode) < DWARF2_ADDR_SIZE)
    {
      int shift = (DWARF2_ADDR_SIZE - GET_MODE_SIZE (mode)) * BITS_PER_UNIT;
      add_loc_descr (&op0, int_loc_descriptor (shift));
      add_loc_descr (&op0, new_loc_descr (DW_OP_shl, 0, 0));
      add_loc_descr (&op1, int_loc_descriptor (shift));
      add_loc_descr (&op1, new_loc_descr (DW_OP_shl, 0, 0));
    }
  else if (SCALAR_INT_MODE_P (mode)
	   && GET_MODE_SIZE (mode) > DWARF2_ADDR_SIZE)
    {
      dw_die_ref type_die = base_type_for_mode (mode, 0);
      dw_loc_descr_ref cvt;
      if (type_die == NULL)
	return NULL;
      cvt = new_loc_descr (dwarf_OP (DW_OP_convert), 0, 0);
      cvt->dw_loc_oprnd1.val_class = dw_val_class_die_ref;
      cvt->dw_loc_oprnd1.v.val_die_ref.die = type_die;
      cvt->dw_loc_oprnd1.v.val_die_ref.external = 0;
      add_loc_descr (&op0, cvt);
      cvt = new_loc_descr (dwarf_OP (DW_OP_convert), 0, 0);
      cvt->dw_loc_oprnd1.val_class = dw_val_class_die_ref;
      cvt->dw_loc_oprnd1.v.val_die_ref.die = type_die;
      cvt->dw_loc_oprnd1.v.val_die_ref.external = 0;
      add_loc_descr (&op1, cvt);
    }

  if (GET_CODE (rtl) == SMIN || GET_CODE (rtl) == UMIN)
    op = DW_OP_lt;
  else
    op = DW_OP_gt;
  ret = op0;
  add_loc_descr (&ret, op1);
  add_loc_descr (&ret, new_loc_descr (op, 0, 0));
  bra_node = new_loc_descr (DW_OP_bra, 0, 0);
  add_loc_descr (&ret, bra_node);
  add_loc_descr (&ret, new_loc_descr (DW_OP_swap, 0, 0));
  drop_node = new_loc_descr (DW_OP_drop, 0, 0);
  add_loc_descr (&ret, drop_node);
  bra_node->dw_loc_oprnd1.val_class = dw_val_class_loc;
  bra_node->dw_loc_oprnd1.v.val_loc = drop_node;
  if ((GET_CODE (rtl) == SMIN || GET_CODE (rtl) == SMAX)
      && SCALAR_INT_MODE_P (mode)
      && GET_MODE_SIZE (mode) > DWARF2_ADDR_SIZE)
    ret = convert_descriptor_to_mode (mode, ret);
  return ret;
}

/* Helper function for mem_loc_descriptor.  Perform OP binary op,
   but after converting arguments to type_die, afterwards
   convert back to unsigned.  */

static dw_loc_descr_ref
typed_binop (enum dwarf_location_atom op, rtx rtl, dw_die_ref type_die,
	     machine_mode mode, machine_mode mem_mode)
{
  dw_loc_descr_ref cvt, op0, op1;

  if (type_die == NULL)
    return NULL;
  op0 = mem_loc_descriptor (XEXP (rtl, 0), mode, mem_mode,
			    VAR_INIT_STATUS_INITIALIZED);
  op1 = mem_loc_descriptor (XEXP (rtl, 1), mode, mem_mode,
			    VAR_INIT_STATUS_INITIALIZED);
  if (op0 == NULL || op1 == NULL)
    return NULL;
  cvt = new_loc_descr (dwarf_OP (DW_OP_convert), 0, 0);
  cvt->dw_loc_oprnd1.val_class = dw_val_class_die_ref;
  cvt->dw_loc_oprnd1.v.val_die_ref.die = type_die;
  cvt->dw_loc_oprnd1.v.val_die_ref.external = 0;
  add_loc_descr (&op0, cvt);
  cvt = new_loc_descr (dwarf_OP (DW_OP_convert), 0, 0);
  cvt->dw_loc_oprnd1.val_class = dw_val_class_die_ref;
  cvt->dw_loc_oprnd1.v.val_die_ref.die = type_die;
  cvt->dw_loc_oprnd1.v.val_die_ref.external = 0;
  add_loc_descr (&op1, cvt);
  add_loc_descr (&op0, op1);
  add_loc_descr (&op0, new_loc_descr (op, 0, 0));
  return convert_descriptor_to_mode (mode, op0);
}

/* CLZ (where constV is CLZ_DEFINED_VALUE_AT_ZERO computed value,
   const0 is DW_OP_lit0 or corresponding typed constant,
   const1 is DW_OP_lit1 or corresponding typed constant
   and constMSB is constant with just the MSB bit set
   for the mode):
       DW_OP_dup DW_OP_bra <L1> DW_OP_drop constV DW_OP_skip <L4>
   L1: const0 DW_OP_swap
   L2: DW_OP_dup constMSB DW_OP_and DW_OP_bra <L3> const1 DW_OP_shl
       DW_OP_swap DW_OP_plus_uconst <1> DW_OP_swap DW_OP_skip <L2>
   L3: DW_OP_drop
   L4: DW_OP_nop

   CTZ is similar:
       DW_OP_dup DW_OP_bra <L1> DW_OP_drop constV DW_OP_skip <L4>
   L1: const0 DW_OP_swap
   L2: DW_OP_dup const1 DW_OP_and DW_OP_bra <L3> const1 DW_OP_shr
       DW_OP_swap DW_OP_plus_uconst <1> DW_OP_swap DW_OP_skip <L2>
   L3: DW_OP_drop
   L4: DW_OP_nop

   FFS is similar:
       DW_OP_dup DW_OP_bra <L1> DW_OP_drop const0 DW_OP_skip <L4>
   L1: const1 DW_OP_swap
   L2: DW_OP_dup const1 DW_OP_and DW_OP_bra <L3> const1 DW_OP_shr
       DW_OP_swap DW_OP_plus_uconst <1> DW_OP_swap DW_OP_skip <L2>
   L3: DW_OP_drop
   L4: DW_OP_nop  */

static dw_loc_descr_ref
clz_loc_descriptor (rtx rtl, machine_mode mode,
		    machine_mode mem_mode)
{
  dw_loc_descr_ref op0, ret, tmp;
  HOST_WIDE_INT valv;
  dw_loc_descr_ref l1jump, l1label;
  dw_loc_descr_ref l2jump, l2label;
  dw_loc_descr_ref l3jump, l3label;
  dw_loc_descr_ref l4jump, l4label;
  rtx msb;

  if (!SCALAR_INT_MODE_P (mode)
      || GET_MODE (XEXP (rtl, 0)) != mode)
    return NULL;

  op0 = mem_loc_descriptor (XEXP (rtl, 0), mode, mem_mode,
			    VAR_INIT_STATUS_INITIALIZED);
  if (op0 == NULL)
    return NULL;
  ret = op0;
  if (GET_CODE (rtl) == CLZ)
    {
      if (!CLZ_DEFINED_VALUE_AT_ZERO (mode, valv))
	valv = GET_MODE_BITSIZE (mode);
    }
  else if (GET_CODE (rtl) == FFS)
    valv = 0;
  else if (!CTZ_DEFINED_VALUE_AT_ZERO (mode, valv))
    valv = GET_MODE_BITSIZE (mode);
  add_loc_descr (&ret, new_loc_descr (DW_OP_dup, 0, 0));
  l1jump = new_loc_descr (DW_OP_bra, 0, 0);
  add_loc_descr (&ret, l1jump);
  add_loc_descr (&ret, new_loc_descr (DW_OP_drop, 0, 0));
  tmp = mem_loc_descriptor (GEN_INT (valv), mode, mem_mode,
			    VAR_INIT_STATUS_INITIALIZED);
  if (tmp == NULL)
    return NULL;
  add_loc_descr (&ret, tmp);
  l4jump = new_loc_descr (DW_OP_skip, 0, 0);
  add_loc_descr (&ret, l4jump);
  l1label = mem_loc_descriptor (GET_CODE (rtl) == FFS
				? const1_rtx : const0_rtx,
				mode, mem_mode,
				VAR_INIT_STATUS_INITIALIZED);
  if (l1label == NULL)
    return NULL;
  add_loc_descr (&ret, l1label);
  add_loc_descr (&ret, new_loc_descr (DW_OP_swap, 0, 0));
  l2label = new_loc_descr (DW_OP_dup, 0, 0);
  add_loc_descr (&ret, l2label);
  if (GET_CODE (rtl) != CLZ)
    msb = const1_rtx;
  else if (GET_MODE_BITSIZE (mode) <= HOST_BITS_PER_WIDE_INT)
    msb = GEN_INT (HOST_WIDE_INT_1U
		   << (GET_MODE_BITSIZE (mode) - 1));
  else
    msb = immed_wide_int_const
      (wi::set_bit_in_zero (GET_MODE_PRECISION (mode) - 1,
			    GET_MODE_PRECISION (mode)), mode);
  if (GET_CODE (msb) == CONST_INT && INTVAL (msb) < 0)
    tmp = new_loc_descr (HOST_BITS_PER_WIDE_INT == 32
			 ? DW_OP_const4u : HOST_BITS_PER_WIDE_INT == 64
			 ? DW_OP_const8u : DW_OP_constu, INTVAL (msb), 0);
  else
    tmp = mem_loc_descriptor (msb, mode, mem_mode,
			      VAR_INIT_STATUS_INITIALIZED);
  if (tmp == NULL)
    return NULL;
  add_loc_descr (&ret, tmp);
  add_loc_descr (&ret, new_loc_descr (DW_OP_and, 0, 0));
  l3jump = new_loc_descr (DW_OP_bra, 0, 0);
  add_loc_descr (&ret, l3jump);
  tmp = mem_loc_descriptor (const1_rtx, mode, mem_mode,
			    VAR_INIT_STATUS_INITIALIZED);
  if (tmp == NULL)
    return NULL;
  add_loc_descr (&ret, tmp);
  add_loc_descr (&ret, new_loc_descr (GET_CODE (rtl) == CLZ
				      ? DW_OP_shl : DW_OP_shr, 0, 0));
  add_loc_descr (&ret, new_loc_descr (DW_OP_swap, 0, 0));
  add_loc_descr (&ret, new_loc_descr (DW_OP_plus_uconst, 1, 0));
  add_loc_descr (&ret, new_loc_descr (DW_OP_swap, 0, 0));
  l2jump = new_loc_descr (DW_OP_skip, 0, 0);
  add_loc_descr (&ret, l2jump);
  l3label = new_loc_descr (DW_OP_drop, 0, 0);
  add_loc_descr (&ret, l3label);
  l4label = new_loc_descr (DW_OP_nop, 0, 0);
  add_loc_descr (&ret, l4label);
  l1jump->dw_loc_oprnd1.val_class = dw_val_class_loc;
  l1jump->dw_loc_oprnd1.v.val_loc = l1label;
  l2jump->dw_loc_oprnd1.val_class = dw_val_class_loc;
  l2jump->dw_loc_oprnd1.v.val_loc = l2label;
  l3jump->dw_loc_oprnd1.val_class = dw_val_class_loc;
  l3jump->dw_loc_oprnd1.v.val_loc = l3label;
  l4jump->dw_loc_oprnd1.val_class = dw_val_class_loc;
  l4jump->dw_loc_oprnd1.v.val_loc = l4label;
  return ret;
}

/* POPCOUNT (const0 is DW_OP_lit0 or corresponding typed constant,
   const1 is DW_OP_lit1 or corresponding typed constant):
       const0 DW_OP_swap
   L1: DW_OP_dup DW_OP_bra <L2> DW_OP_dup DW_OP_rot const1 DW_OP_and
       DW_OP_plus DW_OP_swap const1 DW_OP_shr DW_OP_skip <L1>
   L2: DW_OP_drop

   PARITY is similar:
   L1: DW_OP_dup DW_OP_bra <L2> DW_OP_dup DW_OP_rot const1 DW_OP_and
       DW_OP_xor DW_OP_swap const1 DW_OP_shr DW_OP_skip <L1>
   L2: DW_OP_drop  */

static dw_loc_descr_ref
popcount_loc_descriptor (rtx rtl, machine_mode mode,
			 machine_mode mem_mode)
{
  dw_loc_descr_ref op0, ret, tmp;
  dw_loc_descr_ref l1jump, l1label;
  dw_loc_descr_ref l2jump, l2label;

  if (!SCALAR_INT_MODE_P (mode)
      || GET_MODE (XEXP (rtl, 0)) != mode)
    return NULL;

  op0 = mem_loc_descriptor (XEXP (rtl, 0), mode, mem_mode,
			    VAR_INIT_STATUS_INITIALIZED);
  if (op0 == NULL)
    return NULL;
  ret = op0;
  tmp = mem_loc_descriptor (const0_rtx, mode, mem_mode,
			    VAR_INIT_STATUS_INITIALIZED);
  if (tmp == NULL)
    return NULL;
  add_loc_descr (&ret, tmp);
  add_loc_descr (&ret, new_loc_descr (DW_OP_swap, 0, 0));
  l1label = new_loc_descr (DW_OP_dup, 0, 0);
  add_loc_descr (&ret, l1label);
  l2jump = new_loc_descr (DW_OP_bra, 0, 0);
  add_loc_descr (&ret, l2jump);
  add_loc_descr (&ret, new_loc_descr (DW_OP_dup, 0, 0));
  add_loc_descr (&ret, new_loc_descr (DW_OP_rot, 0, 0));
  tmp = mem_loc_descriptor (const1_rtx, mode, mem_mode,
			    VAR_INIT_STATUS_INITIALIZED);
  if (tmp == NULL)
    return NULL;
  add_loc_descr (&ret, tmp);
  add_loc_descr (&ret, new_loc_descr (DW_OP_and, 0, 0));
  add_loc_descr (&ret, new_loc_descr (GET_CODE (rtl) == POPCOUNT
				      ? DW_OP_plus : DW_OP_xor, 0, 0));
  add_loc_descr (&ret, new_loc_descr (DW_OP_swap, 0, 0));
  tmp = mem_loc_descriptor (const1_rtx, mode, mem_mode,
			    VAR_INIT_STATUS_INITIALIZED);
  add_loc_descr (&ret, tmp);
  add_loc_descr (&ret, new_loc_descr (DW_OP_shr, 0, 0));
  l1jump = new_loc_descr (DW_OP_skip, 0, 0);
  add_loc_descr (&ret, l1jump);
  l2label = new_loc_descr (DW_OP_drop, 0, 0);
  add_loc_descr (&ret, l2label);
  l1jump->dw_loc_oprnd1.val_class = dw_val_class_loc;
  l1jump->dw_loc_oprnd1.v.val_loc = l1label;
  l2jump->dw_loc_oprnd1.val_class = dw_val_class_loc;
  l2jump->dw_loc_oprnd1.v.val_loc = l2label;
  return ret;
}

/* BSWAP (constS is initial shift count, either 56 or 24):
       constS const0
   L1: DW_OP_pick <2> constS DW_OP_pick <3> DW_OP_minus DW_OP_shr
       const255 DW_OP_and DW_OP_pick <2> DW_OP_shl DW_OP_or
       DW_OP_swap DW_OP_dup const0 DW_OP_eq DW_OP_bra <L2> const8
       DW_OP_minus DW_OP_swap DW_OP_skip <L1>
   L2: DW_OP_drop DW_OP_swap DW_OP_drop  */

static dw_loc_descr_ref
bswap_loc_descriptor (rtx rtl, machine_mode mode,
		      machine_mode mem_mode)
{
  dw_loc_descr_ref op0, ret, tmp;
  dw_loc_descr_ref l1jump, l1label;
  dw_loc_descr_ref l2jump, l2label;

  if (!SCALAR_INT_MODE_P (mode)
      || BITS_PER_UNIT != 8
      || (GET_MODE_BITSIZE (mode) != 32
	  &&  GET_MODE_BITSIZE (mode) != 64))
    return NULL;

  op0 = mem_loc_descriptor (XEXP (rtl, 0), mode, mem_mode,
			    VAR_INIT_STATUS_INITIALIZED);
  if (op0 == NULL)
    return NULL;

  ret = op0;
  tmp = mem_loc_descriptor (GEN_INT (GET_MODE_BITSIZE (mode) - 8),
			    mode, mem_mode,
			    VAR_INIT_STATUS_INITIALIZED);
  if (tmp == NULL)
    return NULL;
  add_loc_descr (&ret, tmp);
  tmp = mem_loc_descriptor (const0_rtx, mode, mem_mode,
			    VAR_INIT_STATUS_INITIALIZED);
  if (tmp == NULL)
    return NULL;
  add_loc_descr (&ret, tmp);
  l1label = new_loc_descr (DW_OP_pick, 2, 0);
  add_loc_descr (&ret, l1label);
  tmp = mem_loc_descriptor (GEN_INT (GET_MODE_BITSIZE (mode) - 8),
			    mode, mem_mode,
			    VAR_INIT_STATUS_INITIALIZED);
  add_loc_descr (&ret, tmp);
  add_loc_descr (&ret, new_loc_descr (DW_OP_pick, 3, 0));
  add_loc_descr (&ret, new_loc_descr (DW_OP_minus, 0, 0));
  add_loc_descr (&ret, new_loc_descr (DW_OP_shr, 0, 0));
  tmp = mem_loc_descriptor (GEN_INT (255), mode, mem_mode,
			    VAR_INIT_STATUS_INITIALIZED);
  if (tmp == NULL)
    return NULL;
  add_loc_descr (&ret, tmp);
  add_loc_descr (&ret, new_loc_descr (DW_OP_and, 0, 0));
  add_loc_descr (&ret, new_loc_descr (DW_OP_pick, 2, 0));
  add_loc_descr (&ret, new_loc_descr (DW_OP_shl, 0, 0));
  add_loc_descr (&ret, new_loc_descr (DW_OP_or, 0, 0));
  add_loc_descr (&ret, new_loc_descr (DW_OP_swap, 0, 0));
  add_loc_descr (&ret, new_loc_descr (DW_OP_dup, 0, 0));
  tmp = mem_loc_descriptor (const0_rtx, mode, mem_mode,
			    VAR_INIT_STATUS_INITIALIZED);
  add_loc_descr (&ret, tmp);
  add_loc_descr (&ret, new_loc_descr (DW_OP_eq, 0, 0));
  l2jump = new_loc_descr (DW_OP_bra, 0, 0);
  add_loc_descr (&ret, l2jump);
  tmp = mem_loc_descriptor (GEN_INT (8), mode, mem_mode,
			    VAR_INIT_STATUS_INITIALIZED);
  add_loc_descr (&ret, tmp);
  add_loc_descr (&ret, new_loc_descr (DW_OP_minus, 0, 0));
  add_loc_descr (&ret, new_loc_descr (DW_OP_swap, 0, 0));
  l1jump = new_loc_descr (DW_OP_skip, 0, 0);
  add_loc_descr (&ret, l1jump);
  l2label = new_loc_descr (DW_OP_drop, 0, 0);
  add_loc_descr (&ret, l2label);
  add_loc_descr (&ret, new_loc_descr (DW_OP_swap, 0, 0));
  add_loc_descr (&ret, new_loc_descr (DW_OP_drop, 0, 0));
  l1jump->dw_loc_oprnd1.val_class = dw_val_class_loc;
  l1jump->dw_loc_oprnd1.v.val_loc = l1label;
  l2jump->dw_loc_oprnd1.val_class = dw_val_class_loc;
  l2jump->dw_loc_oprnd1.v.val_loc = l2label;
  return ret;
}

/* ROTATE (constMASK is mode mask, BITSIZE is bitsize of mode):
   DW_OP_over DW_OP_over DW_OP_shl [ constMASK DW_OP_and ] DW_OP_rot
   [ DW_OP_swap constMASK DW_OP_and DW_OP_swap ] DW_OP_neg
   DW_OP_plus_uconst <BITSIZE> DW_OP_shr DW_OP_or

   ROTATERT is similar:
   DW_OP_over DW_OP_over DW_OP_neg DW_OP_plus_uconst <BITSIZE>
   DW_OP_shl [ constMASK DW_OP_and ] DW_OP_rot
   [ DW_OP_swap constMASK DW_OP_and DW_OP_swap ] DW_OP_shr DW_OP_or  */

static dw_loc_descr_ref
rotate_loc_descriptor (rtx rtl, machine_mode mode,
		       machine_mode mem_mode)
{
  rtx rtlop1 = XEXP (rtl, 1);
  dw_loc_descr_ref op0, op1, ret, mask[2] = { NULL, NULL };
  int i;

  if (!SCALAR_INT_MODE_P (mode))
    return NULL;

  if (GET_MODE (rtlop1) != VOIDmode
      && GET_MODE_BITSIZE (GET_MODE (rtlop1)) < GET_MODE_BITSIZE (mode))
    rtlop1 = gen_rtx_ZERO_EXTEND (mode, rtlop1);
  op0 = mem_loc_descriptor (XEXP (rtl, 0), mode, mem_mode,
			    VAR_INIT_STATUS_INITIALIZED);
  op1 = mem_loc_descriptor (rtlop1, mode, mem_mode,
			    VAR_INIT_STATUS_INITIALIZED);
  if (op0 == NULL || op1 == NULL)
    return NULL;
  if (GET_MODE_SIZE (mode) < DWARF2_ADDR_SIZE)
    for (i = 0; i < 2; i++)
      {
	if (GET_MODE_BITSIZE (mode) < HOST_BITS_PER_WIDE_INT)
	  mask[i] = mem_loc_descriptor (GEN_INT (GET_MODE_MASK (mode)),
					mode, mem_mode,
					VAR_INIT_STATUS_INITIALIZED);
	else if (GET_MODE_BITSIZE (mode) == HOST_BITS_PER_WIDE_INT)
	  mask[i] = new_loc_descr (HOST_BITS_PER_WIDE_INT == 32
				   ? DW_OP_const4u
				   : HOST_BITS_PER_WIDE_INT == 64
				   ? DW_OP_const8u : DW_OP_constu,
				   GET_MODE_MASK (mode), 0);
	else
	  mask[i] = NULL;
	if (mask[i] == NULL)
	  return NULL;
	add_loc_descr (&mask[i], new_loc_descr (DW_OP_and, 0, 0));
      }
  ret = op0;
  add_loc_descr (&ret, op1);
  add_loc_descr (&ret, new_loc_descr (DW_OP_over, 0, 0));
  add_loc_descr (&ret, new_loc_descr (DW_OP_over, 0, 0));
  if (GET_CODE (rtl) == ROTATERT)
    {
      add_loc_descr (&ret, new_loc_descr (DW_OP_neg, 0, 0));
      add_loc_descr (&ret, new_loc_descr (DW_OP_plus_uconst,
					  GET_MODE_BITSIZE (mode), 0));
    }
  add_loc_descr (&ret, new_loc_descr (DW_OP_shl, 0, 0));
  if (mask[0] != NULL)
    add_loc_descr (&ret, mask[0]);
  add_loc_descr (&ret, new_loc_descr (DW_OP_rot, 0, 0));
  if (mask[1] != NULL)
    {
      add_loc_descr (&ret, new_loc_descr (DW_OP_swap, 0, 0));
      add_loc_descr (&ret, mask[1]);
      add_loc_descr (&ret, new_loc_descr (DW_OP_swap, 0, 0));
    }
  if (GET_CODE (rtl) == ROTATE)
    {
      add_loc_descr (&ret, new_loc_descr (DW_OP_neg, 0, 0));
      add_loc_descr (&ret, new_loc_descr (DW_OP_plus_uconst,
					  GET_MODE_BITSIZE (mode), 0));
    }
  add_loc_descr (&ret, new_loc_descr (DW_OP_shr, 0, 0));
  add_loc_descr (&ret, new_loc_descr (DW_OP_or, 0, 0));
  return ret;
}

/* Helper function for mem_loc_descriptor.  Return DW_OP_GNU_parameter_ref
   for DEBUG_PARAMETER_REF RTL.  */

static dw_loc_descr_ref
parameter_ref_descriptor (rtx rtl)
{
  dw_loc_descr_ref ret;
  dw_die_ref ref;

  if (dwarf_strict)
    return NULL;
  gcc_assert (TREE_CODE (DEBUG_PARAMETER_REF_DECL (rtl)) == PARM_DECL);
  /* With LTO during LTRANS we get the late DIE that refers to the early
     DIE, thus we add another indirection here.  This seems to confuse
     gdb enough to make gcc.dg/guality/pr68860-1.c FAIL with LTO.  */
  ref = lookup_decl_die (DEBUG_PARAMETER_REF_DECL (rtl));
  ret = new_loc_descr (DW_OP_GNU_parameter_ref, 0, 0);
  if (ref)
    {
      ret->dw_loc_oprnd1.val_class = dw_val_class_die_ref;
      ret->dw_loc_oprnd1.v.val_die_ref.die = ref;
      ret->dw_loc_oprnd1.v.val_die_ref.external = 0;
    }
  else
    {
      ret->dw_loc_oprnd1.val_class = dw_val_class_decl_ref;
      ret->dw_loc_oprnd1.v.val_decl_ref = DEBUG_PARAMETER_REF_DECL (rtl);
    }
  return ret;
}

/* The following routine converts the RTL for a variable or parameter
   (resident in memory) into an equivalent Dwarf representation of a
   mechanism for getting the address of that same variable onto the top of a
   hypothetical "address evaluation" stack.

   When creating memory location descriptors, we are effectively transforming
   the RTL for a memory-resident object into its Dwarf postfix expression
   equivalent.  This routine recursively descends an RTL tree, turning
   it into Dwarf postfix code as it goes.

   MODE is the mode that should be assumed for the rtl if it is VOIDmode.

   MEM_MODE is the mode of the memory reference, needed to handle some
   autoincrement addressing modes.

   Return 0 if we can't represent the location.  */

dw_loc_descr_ref
mem_loc_descriptor (rtx rtl, machine_mode mode,
		    machine_mode mem_mode,
		    enum var_init_status initialized)
{
  dw_loc_descr_ref mem_loc_result = NULL;
  enum dwarf_location_atom op;
  dw_loc_descr_ref op0, op1;
  rtx inner = NULL_RTX;

  if (mode == VOIDmode)
    mode = GET_MODE (rtl);

  /* Note that for a dynamically sized array, the location we will generate a
     description of here will be the lowest numbered location which is
     actually within the array.  That's *not* necessarily the same as the
     zeroth element of the array.  */

  rtl = targetm.delegitimize_address (rtl);

  if (mode != GET_MODE (rtl) && GET_MODE (rtl) != VOIDmode)
    return NULL;

  switch (GET_CODE (rtl))
    {
    case POST_INC:
    case POST_DEC:
    case POST_MODIFY:
      return mem_loc_descriptor (XEXP (rtl, 0), mode, mem_mode, initialized);

    case SUBREG:
      /* The case of a subreg may arise when we have a local (register)
	 variable or a formal (register) parameter which doesn't quite fill
	 up an entire register.  For now, just assume that it is
	 legitimate to make the Dwarf info refer to the whole register which
	 contains the given subreg.  */
      if (!subreg_lowpart_p (rtl))
	break;
      inner = SUBREG_REG (rtl);
      /* FALLTHRU */
    case TRUNCATE:
      if (inner == NULL_RTX)
        inner = XEXP (rtl, 0);
      if (SCALAR_INT_MODE_P (mode)
	  && SCALAR_INT_MODE_P (GET_MODE (inner))
	  && (GET_MODE_SIZE (mode) <= DWARF2_ADDR_SIZE
#ifdef POINTERS_EXTEND_UNSIGNED
	      || (mode == Pmode && mem_mode != VOIDmode)
#endif
	     )
	  && GET_MODE_SIZE (GET_MODE (inner)) <= DWARF2_ADDR_SIZE)
	{
	  mem_loc_result = mem_loc_descriptor (inner,
					       GET_MODE (inner),
					       mem_mode, initialized);
	  break;
	}
      if (dwarf_strict && dwarf_version < 5)
	break;
      if (GET_MODE_SIZE (mode) > GET_MODE_SIZE (GET_MODE (inner)))
	break;
      if (GET_MODE_SIZE (mode) != GET_MODE_SIZE (GET_MODE (inner))
	  && (!SCALAR_INT_MODE_P (mode)
	      || !SCALAR_INT_MODE_P (GET_MODE (inner))))
	break;
      else
	{
	  dw_die_ref type_die;
	  dw_loc_descr_ref cvt;

	  mem_loc_result = mem_loc_descriptor (inner,
					       GET_MODE (inner),
					       mem_mode, initialized);
	  if (mem_loc_result == NULL)
	    break;
	  type_die = base_type_for_mode (mode, SCALAR_INT_MODE_P (mode));
	  if (type_die == NULL)
	    {
	      mem_loc_result = NULL;
	      break;
	    }
	  if (GET_MODE_SIZE (mode)
	      != GET_MODE_SIZE (GET_MODE (inner)))
	    cvt = new_loc_descr (dwarf_OP (DW_OP_convert), 0, 0);
	  else
	    cvt = new_loc_descr (dwarf_OP (DW_OP_reinterpret), 0, 0);
	  cvt->dw_loc_oprnd1.val_class = dw_val_class_die_ref;
	  cvt->dw_loc_oprnd1.v.val_die_ref.die = type_die;
	  cvt->dw_loc_oprnd1.v.val_die_ref.external = 0;
	  add_loc_descr (&mem_loc_result, cvt);
	  if (SCALAR_INT_MODE_P (mode)
	      && GET_MODE_SIZE (mode) <= DWARF2_ADDR_SIZE)
	    {
	      /* Convert it to untyped afterwards.  */
	      cvt = new_loc_descr (dwarf_OP (DW_OP_convert), 0, 0);
	      add_loc_descr (&mem_loc_result, cvt);
	    }
	}
      break;

    case REG:
      if (! SCALAR_INT_MODE_P (mode)
	  || (GET_MODE_SIZE (mode) > DWARF2_ADDR_SIZE
	      && rtl != arg_pointer_rtx
	      && rtl != frame_pointer_rtx
#ifdef POINTERS_EXTEND_UNSIGNED
	      && (mode != Pmode || mem_mode == VOIDmode)
#endif
	      ))
	{
	  dw_die_ref type_die;
	  unsigned int dbx_regnum;

	  if (dwarf_strict && dwarf_version < 5)
	    break;
	  if (REGNO (rtl) > FIRST_PSEUDO_REGISTER)
	    break;
	  type_die = base_type_for_mode (mode, SCALAR_INT_MODE_P (mode));
	  if (type_die == NULL)
	    break;

	  dbx_regnum = dbx_reg_number (rtl);
	  if (dbx_regnum == IGNORED_DWARF_REGNUM)
	    break;
	  mem_loc_result = new_loc_descr (dwarf_OP (DW_OP_regval_type),
					  dbx_regnum, 0);
	  mem_loc_result->dw_loc_oprnd2.val_class = dw_val_class_die_ref;
	  mem_loc_result->dw_loc_oprnd2.v.val_die_ref.die = type_die;
	  mem_loc_result->dw_loc_oprnd2.v.val_die_ref.external = 0;
	  break;
	}
      /* Whenever a register number forms a part of the description of the
	 method for calculating the (dynamic) address of a memory resident
	 object, DWARF rules require the register number be referred to as
	 a "base register".  This distinction is not based in any way upon
	 what category of register the hardware believes the given register
	 belongs to.  This is strictly DWARF terminology we're dealing with
	 here. Note that in cases where the location of a memory-resident
	 data object could be expressed as: OP_ADD (OP_BASEREG (basereg),
	 OP_CONST (0)) the actual DWARF location descriptor that we generate
	 may just be OP_BASEREG (basereg).  This may look deceptively like
	 the object in question was allocated to a register (rather than in
	 memory) so DWARF consumers need to be aware of the subtle
	 distinction between OP_REG and OP_BASEREG.  */
      if (REGNO (rtl) < FIRST_PSEUDO_REGISTER)
	mem_loc_result = based_loc_descr (rtl, 0, VAR_INIT_STATUS_INITIALIZED);
      else if (stack_realign_drap
	       && crtl->drap_reg
	       && crtl->args.internal_arg_pointer == rtl
	       && REGNO (crtl->drap_reg) < FIRST_PSEUDO_REGISTER)
	{
	  /* If RTL is internal_arg_pointer, which has been optimized
	     out, use DRAP instead.  */
	  mem_loc_result = based_loc_descr (crtl->drap_reg, 0,
					    VAR_INIT_STATUS_INITIALIZED);
	}
      break;

    case SIGN_EXTEND:
    case ZERO_EXTEND:
      if (!SCALAR_INT_MODE_P (mode))
	break;
      op0 = mem_loc_descriptor (XEXP (rtl, 0), GET_MODE (XEXP (rtl, 0)),
				mem_mode, VAR_INIT_STATUS_INITIALIZED);
      if (op0 == 0)
	break;
      else if (GET_CODE (rtl) == ZERO_EXTEND
	       && GET_MODE_SIZE (mode) <= DWARF2_ADDR_SIZE
	       && GET_MODE_BITSIZE (GET_MODE (XEXP (rtl, 0)))
		  < HOST_BITS_PER_WIDE_INT
	       /* If DW_OP_const{1,2,4}u won't be used, it is shorter
		  to expand zero extend as two shifts instead of
		  masking.  */
	       && GET_MODE_SIZE (GET_MODE (XEXP (rtl, 0))) <= 4)
	{
	  machine_mode imode = GET_MODE (XEXP (rtl, 0));
	  mem_loc_result = op0;
	  add_loc_descr (&mem_loc_result,
			 int_loc_descriptor (GET_MODE_MASK (imode)));
	  add_loc_descr (&mem_loc_result, new_loc_descr (DW_OP_and, 0, 0));
	}
      else if (GET_MODE_SIZE (mode) <= DWARF2_ADDR_SIZE)
	{
	  int shift = DWARF2_ADDR_SIZE
		      - GET_MODE_SIZE (GET_MODE (XEXP (rtl, 0)));
	  shift *= BITS_PER_UNIT;
	  if (GET_CODE (rtl) == SIGN_EXTEND)
	    op = DW_OP_shra;
	  else
	    op = DW_OP_shr;
	  mem_loc_result = op0;
	  add_loc_descr (&mem_loc_result, int_loc_descriptor (shift));
	  add_loc_descr (&mem_loc_result, new_loc_descr (DW_OP_shl, 0, 0));
	  add_loc_descr (&mem_loc_result, int_loc_descriptor (shift));
	  add_loc_descr (&mem_loc_result, new_loc_descr (op, 0, 0));
	}
      else if (!dwarf_strict || dwarf_version >= 5)
	{
	  dw_die_ref type_die1, type_die2;
	  dw_loc_descr_ref cvt;

	  type_die1 = base_type_for_mode (GET_MODE (XEXP (rtl, 0)),
					  GET_CODE (rtl) == ZERO_EXTEND);
	  if (type_die1 == NULL)
	    break;
	  type_die2 = base_type_for_mode (mode, 1);
	  if (type_die2 == NULL)
	    break;
	  mem_loc_result = op0;
	  cvt = new_loc_descr (dwarf_OP (DW_OP_convert), 0, 0);
	  cvt->dw_loc_oprnd1.val_class = dw_val_class_die_ref;
	  cvt->dw_loc_oprnd1.v.val_die_ref.die = type_die1;
	  cvt->dw_loc_oprnd1.v.val_die_ref.external = 0;
	  add_loc_descr (&mem_loc_result, cvt);
	  cvt = new_loc_descr (dwarf_OP (DW_OP_convert), 0, 0);
	  cvt->dw_loc_oprnd1.val_class = dw_val_class_die_ref;
	  cvt->dw_loc_oprnd1.v.val_die_ref.die = type_die2;
	  cvt->dw_loc_oprnd1.v.val_die_ref.external = 0;
	  add_loc_descr (&mem_loc_result, cvt);
	}
      break;

    case MEM:
      {
	rtx new_rtl = avoid_constant_pool_reference (rtl);
	if (new_rtl != rtl)
	  {
	    mem_loc_result = mem_loc_descriptor (new_rtl, mode, mem_mode,
						 initialized);
	    if (mem_loc_result != NULL)
	      return mem_loc_result;
	  }
      }
      mem_loc_result = mem_loc_descriptor (XEXP (rtl, 0),
					   get_address_mode (rtl), mode,
					   VAR_INIT_STATUS_INITIALIZED);
      if (mem_loc_result == NULL)
	mem_loc_result = tls_mem_loc_descriptor (rtl);
      if (mem_loc_result != NULL)
	{
	  if (GET_MODE_SIZE (mode) > DWARF2_ADDR_SIZE
	      || !SCALAR_INT_MODE_P(mode))
	    {
	      dw_die_ref type_die;
	      dw_loc_descr_ref deref;

	      if (dwarf_strict && dwarf_version < 5)
		return NULL;
	      type_die
		= base_type_for_mode (mode, SCALAR_INT_MODE_P (mode));
	      if (type_die == NULL)
		return NULL;
	      deref = new_loc_descr (dwarf_OP (DW_OP_deref_type),
				     GET_MODE_SIZE (mode), 0);
	      deref->dw_loc_oprnd2.val_class = dw_val_class_die_ref;
	      deref->dw_loc_oprnd2.v.val_die_ref.die = type_die;
	      deref->dw_loc_oprnd2.v.val_die_ref.external = 0;
	      add_loc_descr (&mem_loc_result, deref);
	    }
	  else if (GET_MODE_SIZE (mode) == DWARF2_ADDR_SIZE)
	    add_loc_descr (&mem_loc_result, new_loc_descr (DW_OP_deref, 0, 0));
	  else
	    add_loc_descr (&mem_loc_result,
			   new_loc_descr (DW_OP_deref_size,
					  GET_MODE_SIZE (mode), 0));
	}
      break;

    case LO_SUM:
      return mem_loc_descriptor (XEXP (rtl, 1), mode, mem_mode, initialized);

    case LABEL_REF:
      /* Some ports can transform a symbol ref into a label ref, because
	 the symbol ref is too far away and has to be dumped into a constant
	 pool.  */
    case CONST:
    case SYMBOL_REF:
      if (!SCALAR_INT_MODE_P (mode)
	  || (GET_MODE_SIZE (mode) > DWARF2_ADDR_SIZE
#ifdef POINTERS_EXTEND_UNSIGNED
	      && (mode != Pmode || mem_mode == VOIDmode)
#endif
	      ))
	break;
      if (GET_CODE (rtl) == SYMBOL_REF
	  && SYMBOL_REF_TLS_MODEL (rtl) != TLS_MODEL_NONE)
	{
	  dw_loc_descr_ref temp;

	  /* If this is not defined, we have no way to emit the data.  */
	  if (!targetm.have_tls || !targetm.asm_out.output_dwarf_dtprel)
	    break;

          temp = new_addr_loc_descr (rtl, dtprel_true);

	  /* We check for DWARF 5 here because gdb did not implement
	     DW_OP_form_tls_address until after 7.12.  */
	  mem_loc_result = new_loc_descr ((dwarf_version >= 5
					   ? DW_OP_form_tls_address
					   : DW_OP_GNU_push_tls_address),
					  0, 0);
	  add_loc_descr (&mem_loc_result, temp);

	  break;
	}

      if (!const_ok_for_output (rtl))
	{
	  if (GET_CODE (rtl) == CONST)
	    mem_loc_result = mem_loc_descriptor (XEXP (rtl, 0), mode, mem_mode,
						 initialized);
	  break;
	}

    symref:
      mem_loc_result = new_addr_loc_descr (rtl, dtprel_false);
      vec_safe_push (used_rtx_array, rtl);
      break;

    case CONCAT:
    case CONCATN:
    case VAR_LOCATION:
    case DEBUG_IMPLICIT_PTR:
      expansion_failed (NULL_TREE, rtl,
			"CONCAT/CONCATN/VAR_LOCATION is handled only by loc_descriptor");
      return 0;

    case ENTRY_VALUE:
      if (dwarf_strict && dwarf_version < 5)
	return NULL;
      if (REG_P (ENTRY_VALUE_EXP (rtl)))
	{
	  if (!SCALAR_INT_MODE_P (mode)
	      || GET_MODE_SIZE (mode) > DWARF2_ADDR_SIZE)
	    op0 = mem_loc_descriptor (ENTRY_VALUE_EXP (rtl), mode,
				      VOIDmode, VAR_INIT_STATUS_INITIALIZED);
	  else
	    {
              unsigned int dbx_regnum = dbx_reg_number (ENTRY_VALUE_EXP (rtl));
	      if (dbx_regnum == IGNORED_DWARF_REGNUM)
		return NULL;
	      op0 = one_reg_loc_descriptor (dbx_regnum,
					    VAR_INIT_STATUS_INITIALIZED);
	    }
	}
      else if (MEM_P (ENTRY_VALUE_EXP (rtl))
	       && REG_P (XEXP (ENTRY_VALUE_EXP (rtl), 0)))
	{
	  op0 = mem_loc_descriptor (ENTRY_VALUE_EXP (rtl), mode,
				    VOIDmode, VAR_INIT_STATUS_INITIALIZED);
	  if (op0 && op0->dw_loc_opc == DW_OP_fbreg)
	    return NULL;
	}
      else
	gcc_unreachable ();
      if (op0 == NULL)
	return NULL;
      mem_loc_result = new_loc_descr (dwarf_OP (DW_OP_entry_value), 0, 0);
      mem_loc_result->dw_loc_oprnd1.val_class = dw_val_class_loc;
      mem_loc_result->dw_loc_oprnd1.v.val_loc = op0;
      break;

    case DEBUG_PARAMETER_REF:
      mem_loc_result = parameter_ref_descriptor (rtl);
      break;

    case PRE_MODIFY:
      /* Extract the PLUS expression nested inside and fall into
	 PLUS code below.  */
      rtl = XEXP (rtl, 1);
      goto plus;

    case PRE_INC:
    case PRE_DEC:
      /* Turn these into a PLUS expression and fall into the PLUS code
	 below.  */
      rtl = gen_rtx_PLUS (mode, XEXP (rtl, 0),
			  gen_int_mode (GET_CODE (rtl) == PRE_INC
					? GET_MODE_UNIT_SIZE (mem_mode)
					: -GET_MODE_UNIT_SIZE (mem_mode),
					mode));

      /* fall through */

    case PLUS:
    plus:
      if (is_based_loc (rtl)
	  && (GET_MODE_SIZE (mode) <= DWARF2_ADDR_SIZE
	      || XEXP (rtl, 0) == arg_pointer_rtx
	      || XEXP (rtl, 0) == frame_pointer_rtx)
	  && SCALAR_INT_MODE_P (mode))
	mem_loc_result = based_loc_descr (XEXP (rtl, 0),
					  INTVAL (XEXP (rtl, 1)),
					  VAR_INIT_STATUS_INITIALIZED);
      else
	{
	  mem_loc_result = mem_loc_descriptor (XEXP (rtl, 0), mode, mem_mode,
					       VAR_INIT_STATUS_INITIALIZED);
	  if (mem_loc_result == 0)
	    break;

	  if (CONST_INT_P (XEXP (rtl, 1))
	      && GET_MODE_SIZE (mode) <= DWARF2_ADDR_SIZE)
	    loc_descr_plus_const (&mem_loc_result, INTVAL (XEXP (rtl, 1)));
	  else
	    {
	      op1 = mem_loc_descriptor (XEXP (rtl, 1), mode, mem_mode,
					VAR_INIT_STATUS_INITIALIZED);
	      if (op1 == 0)
		return NULL;
	      add_loc_descr (&mem_loc_result, op1);
	      add_loc_descr (&mem_loc_result,
			     new_loc_descr (DW_OP_plus, 0, 0));
	    }
	}
      break;

    /* If a pseudo-reg is optimized away, it is possible for it to
       be replaced with a MEM containing a multiply or shift.  */
    case MINUS:
      op = DW_OP_minus;
      goto do_binop;

    case MULT:
      op = DW_OP_mul;
      goto do_binop;

    case DIV:
      if ((!dwarf_strict || dwarf_version >= 5)
	  && SCALAR_INT_MODE_P (mode)
	  && GET_MODE_SIZE (mode) > DWARF2_ADDR_SIZE)
	{
	  mem_loc_result = typed_binop (DW_OP_div, rtl,
					base_type_for_mode (mode, 0),
					mode, mem_mode);
	  break;
	}
      op = DW_OP_div;
      goto do_binop;

    case UMOD:
      op = DW_OP_mod;
      goto do_binop;

    case ASHIFT:
      op = DW_OP_shl;
      goto do_shift;

    case ASHIFTRT:
      op = DW_OP_shra;
      goto do_shift;

    case LSHIFTRT:
      op = DW_OP_shr;
      goto do_shift;

    do_shift:
      if (!SCALAR_INT_MODE_P (mode))
	break;
      op0 = mem_loc_descriptor (XEXP (rtl, 0), mode, mem_mode,
				VAR_INIT_STATUS_INITIALIZED);
      {
	rtx rtlop1 = XEXP (rtl, 1);
	if (GET_MODE (rtlop1) != VOIDmode
	    && GET_MODE_BITSIZE (GET_MODE (rtlop1))
	       < GET_MODE_BITSIZE (mode))
	  rtlop1 = gen_rtx_ZERO_EXTEND (mode, rtlop1);
	op1 = mem_loc_descriptor (rtlop1, mode, mem_mode,
				  VAR_INIT_STATUS_INITIALIZED);
      }

      if (op0 == 0 || op1 == 0)
	break;

      mem_loc_result = op0;
      add_loc_descr (&mem_loc_result, op1);
      add_loc_descr (&mem_loc_result, new_loc_descr (op, 0, 0));
      break;

    case AND:
      op = DW_OP_and;
      goto do_binop;

    case IOR:
      op = DW_OP_or;
      goto do_binop;

    case XOR:
      op = DW_OP_xor;
      goto do_binop;

    do_binop:
      op0 = mem_loc_descriptor (XEXP (rtl, 0), mode, mem_mode,
				VAR_INIT_STATUS_INITIALIZED);
      op1 = mem_loc_descriptor (XEXP (rtl, 1), mode, mem_mode,
				VAR_INIT_STATUS_INITIALIZED);

      if (op0 == 0 || op1 == 0)
	break;

      mem_loc_result = op0;
      add_loc_descr (&mem_loc_result, op1);
      add_loc_descr (&mem_loc_result, new_loc_descr (op, 0, 0));
      break;

    case MOD:
      if (GET_MODE_SIZE (mode) > DWARF2_ADDR_SIZE
	  && (!dwarf_strict || dwarf_version >= 5))
	{
	  mem_loc_result = typed_binop (DW_OP_mod, rtl,
					base_type_for_mode (mode, 0),
					mode, mem_mode);
	  break;
	}

      op0 = mem_loc_descriptor (XEXP (rtl, 0), mode, mem_mode,
				VAR_INIT_STATUS_INITIALIZED);
      op1 = mem_loc_descriptor (XEXP (rtl, 1), mode, mem_mode,
				VAR_INIT_STATUS_INITIALIZED);

      if (op0 == 0 || op1 == 0)
	break;

      mem_loc_result = op0;
      add_loc_descr (&mem_loc_result, op1);
      add_loc_descr (&mem_loc_result, new_loc_descr (DW_OP_over, 0, 0));
      add_loc_descr (&mem_loc_result, new_loc_descr (DW_OP_over, 0, 0));
      add_loc_descr (&mem_loc_result, new_loc_descr (DW_OP_div, 0, 0));
      add_loc_descr (&mem_loc_result, new_loc_descr (DW_OP_mul, 0, 0));
      add_loc_descr (&mem_loc_result, new_loc_descr (DW_OP_minus, 0, 0));
      break;

    case UDIV:
      if ((!dwarf_strict || dwarf_version >= 5)
	  && SCALAR_INT_MODE_P (mode))
	{
	  if (GET_MODE_SIZE (mode) > DWARF2_ADDR_SIZE)
	    {
	      op = DW_OP_div;
	      goto do_binop;
	    }
	  mem_loc_result = typed_binop (DW_OP_div, rtl,
					base_type_for_mode (mode, 1),
					mode, mem_mode);
	}
      break;

    case NOT:
      op = DW_OP_not;
      goto do_unop;

    case ABS:
      op = DW_OP_abs;
      goto do_unop;

    case NEG:
      op = DW_OP_neg;
      goto do_unop;

    do_unop:
      op0 = mem_loc_descriptor (XEXP (rtl, 0), mode, mem_mode,
				VAR_INIT_STATUS_INITIALIZED);

      if (op0 == 0)
	break;

      mem_loc_result = op0;
      add_loc_descr (&mem_loc_result, new_loc_descr (op, 0, 0));
      break;

    case CONST_INT:
      if (GET_MODE_SIZE (mode) <= DWARF2_ADDR_SIZE
#ifdef POINTERS_EXTEND_UNSIGNED
	  || (mode == Pmode
	      && mem_mode != VOIDmode
	      && trunc_int_for_mode (INTVAL (rtl), ptr_mode) == INTVAL (rtl))
#endif
	  )
	{
	  mem_loc_result = int_loc_descriptor (INTVAL (rtl));
	  break;
	}
      if ((!dwarf_strict || dwarf_version >= 5)
	  && (GET_MODE_BITSIZE (mode) == HOST_BITS_PER_WIDE_INT
	      || GET_MODE_BITSIZE (mode) == HOST_BITS_PER_DOUBLE_INT))
	{
	  dw_die_ref type_die = base_type_for_mode (mode, 1);
	  machine_mode amode;
	  if (type_die == NULL)
	    return NULL;
	  amode = mode_for_size (DWARF2_ADDR_SIZE * BITS_PER_UNIT,
				 MODE_INT, 0);
	  if (INTVAL (rtl) >= 0
	      && amode != BLKmode
	      && trunc_int_for_mode (INTVAL (rtl), amode) == INTVAL (rtl)
	      /* const DW_OP_convert <XXX> vs.
		 DW_OP_const_type <XXX, 1, const>.  */
	      && size_of_int_loc_descriptor (INTVAL (rtl)) + 1 + 1
		 < (unsigned long) 1 + 1 + 1 + GET_MODE_SIZE (mode))
	    {
	      mem_loc_result = int_loc_descriptor (INTVAL (rtl));
	      op0 = new_loc_descr (dwarf_OP (DW_OP_convert), 0, 0);
	      op0->dw_loc_oprnd1.val_class = dw_val_class_die_ref;
	      op0->dw_loc_oprnd1.v.val_die_ref.die = type_die;
	      op0->dw_loc_oprnd1.v.val_die_ref.external = 0;
	      add_loc_descr (&mem_loc_result, op0);
	      return mem_loc_result;
	    }
	  mem_loc_result = new_loc_descr (dwarf_OP (DW_OP_const_type), 0,
					  INTVAL (rtl));
	  mem_loc_result->dw_loc_oprnd1.val_class = dw_val_class_die_ref;
	  mem_loc_result->dw_loc_oprnd1.v.val_die_ref.die = type_die;
	  mem_loc_result->dw_loc_oprnd1.v.val_die_ref.external = 0;
	  if (GET_MODE_BITSIZE (mode) == HOST_BITS_PER_WIDE_INT)
	    mem_loc_result->dw_loc_oprnd2.val_class = dw_val_class_const;
	  else
	    {
	      mem_loc_result->dw_loc_oprnd2.val_class
		= dw_val_class_const_double;
	      mem_loc_result->dw_loc_oprnd2.v.val_double
		= double_int::from_shwi (INTVAL (rtl));
	    }
	}
      break;

    case CONST_DOUBLE:
      if (!dwarf_strict || dwarf_version >= 5)
	{
	  dw_die_ref type_die;

	  /* Note that if TARGET_SUPPORTS_WIDE_INT == 0, a
	     CONST_DOUBLE rtx could represent either a large integer
	     or a floating-point constant.  If TARGET_SUPPORTS_WIDE_INT != 0,
	     the value is always a floating point constant.

	     When it is an integer, a CONST_DOUBLE is used whenever
	     the constant requires 2 HWIs to be adequately represented.
	     We output CONST_DOUBLEs as blocks.  */
	  if (mode == VOIDmode
	      || (GET_MODE (rtl) == VOIDmode
		  && GET_MODE_BITSIZE (mode) != HOST_BITS_PER_DOUBLE_INT))
	    break;
	  type_die = base_type_for_mode (mode, SCALAR_INT_MODE_P (mode));
	  if (type_die == NULL)
	    return NULL;
	  mem_loc_result = new_loc_descr (dwarf_OP (DW_OP_const_type), 0, 0);
	  mem_loc_result->dw_loc_oprnd1.val_class = dw_val_class_die_ref;
	  mem_loc_result->dw_loc_oprnd1.v.val_die_ref.die = type_die;
	  mem_loc_result->dw_loc_oprnd1.v.val_die_ref.external = 0;
#if TARGET_SUPPORTS_WIDE_INT == 0
	  if (!SCALAR_FLOAT_MODE_P (mode))
	    {
	      mem_loc_result->dw_loc_oprnd2.val_class
		= dw_val_class_const_double;
	      mem_loc_result->dw_loc_oprnd2.v.val_double
		= rtx_to_double_int (rtl);
	    }
	  else
#endif
	    {
	      unsigned int length = GET_MODE_SIZE (mode);
	      unsigned char *array = ggc_vec_alloc<unsigned char> (length);

	      insert_float (rtl, array);
	      mem_loc_result->dw_loc_oprnd2.val_class = dw_val_class_vec;
	      mem_loc_result->dw_loc_oprnd2.v.val_vec.length = length / 4;
	      mem_loc_result->dw_loc_oprnd2.v.val_vec.elt_size = 4;
	      mem_loc_result->dw_loc_oprnd2.v.val_vec.array = array;
	    }
	}
      break;

    case CONST_WIDE_INT:
      if (!dwarf_strict || dwarf_version >= 5)
	{
	  dw_die_ref type_die;

	  type_die = base_type_for_mode (mode, SCALAR_INT_MODE_P (mode));
	  if (type_die == NULL)
	    return NULL;
	  mem_loc_result = new_loc_descr (dwarf_OP (DW_OP_const_type), 0, 0);
	  mem_loc_result->dw_loc_oprnd1.val_class = dw_val_class_die_ref;
	  mem_loc_result->dw_loc_oprnd1.v.val_die_ref.die = type_die;
	  mem_loc_result->dw_loc_oprnd1.v.val_die_ref.external = 0;
	  mem_loc_result->dw_loc_oprnd2.val_class
	    = dw_val_class_wide_int;
	  mem_loc_result->dw_loc_oprnd2.v.val_wide = ggc_alloc<wide_int> ();
	  *mem_loc_result->dw_loc_oprnd2.v.val_wide = rtx_mode_t (rtl, mode);
	}
      break;

    case EQ:
      mem_loc_result = scompare_loc_descriptor (DW_OP_eq, rtl, mem_mode);
      break;

    case GE:
      mem_loc_result = scompare_loc_descriptor (DW_OP_ge, rtl, mem_mode);
      break;

    case GT:
      mem_loc_result = scompare_loc_descriptor (DW_OP_gt, rtl, mem_mode);
      break;

    case LE:
      mem_loc_result = scompare_loc_descriptor (DW_OP_le, rtl, mem_mode);
      break;

    case LT:
      mem_loc_result = scompare_loc_descriptor (DW_OP_lt, rtl, mem_mode);
      break;

    case NE:
      mem_loc_result = scompare_loc_descriptor (DW_OP_ne, rtl, mem_mode);
      break;

    case GEU:
      mem_loc_result = ucompare_loc_descriptor (DW_OP_ge, rtl, mem_mode);
      break;

    case GTU:
      mem_loc_result = ucompare_loc_descriptor (DW_OP_gt, rtl, mem_mode);
      break;

    case LEU:
      mem_loc_result = ucompare_loc_descriptor (DW_OP_le, rtl, mem_mode);
      break;

    case LTU:
      mem_loc_result = ucompare_loc_descriptor (DW_OP_lt, rtl, mem_mode);
      break;

    case UMIN:
    case UMAX:
      if (!SCALAR_INT_MODE_P (mode))
	break;
      /* FALLTHRU */
    case SMIN:
    case SMAX:
      mem_loc_result = minmax_loc_descriptor (rtl, mode, mem_mode);
      break;

    case ZERO_EXTRACT:
    case SIGN_EXTRACT:
      if (CONST_INT_P (XEXP (rtl, 1))
	  && CONST_INT_P (XEXP (rtl, 2))
	  && ((unsigned) INTVAL (XEXP (rtl, 1))
	      + (unsigned) INTVAL (XEXP (rtl, 2))
	      <= GET_MODE_BITSIZE (mode))
	  && SCALAR_INT_MODE_P (mode)
	  && GET_MODE_SIZE (mode) <= DWARF2_ADDR_SIZE
	  && GET_MODE_SIZE (GET_MODE (XEXP (rtl, 0))) <= DWARF2_ADDR_SIZE)
	{
	  int shift, size;
	  op0 = mem_loc_descriptor (XEXP (rtl, 0), GET_MODE (XEXP (rtl, 0)),
				    mem_mode, VAR_INIT_STATUS_INITIALIZED);
	  if (op0 == 0)
	    break;
	  if (GET_CODE (rtl) == SIGN_EXTRACT)
	    op = DW_OP_shra;
	  else
	    op = DW_OP_shr;
	  mem_loc_result = op0;
	  size = INTVAL (XEXP (rtl, 1));
	  shift = INTVAL (XEXP (rtl, 2));
	  if (BITS_BIG_ENDIAN)
	    shift = GET_MODE_BITSIZE (GET_MODE (XEXP (rtl, 0)))
		    - shift - size;
	  if (shift + size != (int) DWARF2_ADDR_SIZE)
	    {
	      add_loc_descr (&mem_loc_result,
			     int_loc_descriptor (DWARF2_ADDR_SIZE
						 - shift - size));
	      add_loc_descr (&mem_loc_result, new_loc_descr (DW_OP_shl, 0, 0));
	    }
	  if (size != (int) DWARF2_ADDR_SIZE)
	    {
	      add_loc_descr (&mem_loc_result,
			     int_loc_descriptor (DWARF2_ADDR_SIZE - size));
	      add_loc_descr (&mem_loc_result, new_loc_descr (op, 0, 0));
	    }
	}
      break;

    case IF_THEN_ELSE:
      {
	dw_loc_descr_ref op2, bra_node, drop_node;
	op0 = mem_loc_descriptor (XEXP (rtl, 0),
				  GET_MODE (XEXP (rtl, 0)) == VOIDmode
				  ? word_mode : GET_MODE (XEXP (rtl, 0)),
				  mem_mode, VAR_INIT_STATUS_INITIALIZED);
	op1 = mem_loc_descriptor (XEXP (rtl, 1), mode, mem_mode,
				  VAR_INIT_STATUS_INITIALIZED);
	op2 = mem_loc_descriptor (XEXP (rtl, 2), mode, mem_mode,
				  VAR_INIT_STATUS_INITIALIZED);
	if (op0 == NULL || op1 == NULL || op2 == NULL)
	  break;

	mem_loc_result = op1;
	add_loc_descr (&mem_loc_result, op2);
	add_loc_descr (&mem_loc_result, op0);
	bra_node = new_loc_descr (DW_OP_bra, 0, 0);
	add_loc_descr (&mem_loc_result, bra_node);
	add_loc_descr (&mem_loc_result, new_loc_descr (DW_OP_swap, 0, 0));
	drop_node = new_loc_descr (DW_OP_drop, 0, 0);
	add_loc_descr (&mem_loc_result, drop_node);
	bra_node->dw_loc_oprnd1.val_class = dw_val_class_loc;
	bra_node->dw_loc_oprnd1.v.val_loc = drop_node;
      }
      break;

    case FLOAT_EXTEND:
    case FLOAT_TRUNCATE:
    case FLOAT:
    case UNSIGNED_FLOAT:
    case FIX:
    case UNSIGNED_FIX:
      if (!dwarf_strict || dwarf_version >= 5)
	{
	  dw_die_ref type_die;
	  dw_loc_descr_ref cvt;

	  op0 = mem_loc_descriptor (XEXP (rtl, 0), GET_MODE (XEXP (rtl, 0)),
				    mem_mode, VAR_INIT_STATUS_INITIALIZED);
	  if (op0 == NULL)
	    break;
	  if (SCALAR_INT_MODE_P (GET_MODE (XEXP (rtl, 0)))
	      && (GET_CODE (rtl) == FLOAT
		  || GET_MODE_SIZE (GET_MODE (XEXP (rtl, 0)))
		     <= DWARF2_ADDR_SIZE))
	    {
	      type_die = base_type_for_mode (GET_MODE (XEXP (rtl, 0)),
					     GET_CODE (rtl) == UNSIGNED_FLOAT);
	      if (type_die == NULL)
		break;
	      cvt = new_loc_descr (dwarf_OP (DW_OP_convert), 0, 0);
	      cvt->dw_loc_oprnd1.val_class = dw_val_class_die_ref;
	      cvt->dw_loc_oprnd1.v.val_die_ref.die = type_die;
	      cvt->dw_loc_oprnd1.v.val_die_ref.external = 0;
	      add_loc_descr (&op0, cvt);
	    }
	  type_die = base_type_for_mode (mode, GET_CODE (rtl) == UNSIGNED_FIX);
	  if (type_die == NULL)
	    break;
	  cvt = new_loc_descr (dwarf_OP (DW_OP_convert), 0, 0);
	  cvt->dw_loc_oprnd1.val_class = dw_val_class_die_ref;
	  cvt->dw_loc_oprnd1.v.val_die_ref.die = type_die;
	  cvt->dw_loc_oprnd1.v.val_die_ref.external = 0;
	  add_loc_descr (&op0, cvt);
	  if (SCALAR_INT_MODE_P (mode)
	      && (GET_CODE (rtl) == FIX
		  || GET_MODE_SIZE (mode) < DWARF2_ADDR_SIZE))
	    {
	      op0 = convert_descriptor_to_mode (mode, op0);
	      if (op0 == NULL)
		break;
	    }
	  mem_loc_result = op0;
	}
      break;

    case CLZ:
    case CTZ:
    case FFS:
      mem_loc_result = clz_loc_descriptor (rtl, mode, mem_mode);
      break;

    case POPCOUNT:
    case PARITY:
      mem_loc_result = popcount_loc_descriptor (rtl, mode, mem_mode);
      break;

    case BSWAP:
      mem_loc_result = bswap_loc_descriptor (rtl, mode, mem_mode);
      break;

    case ROTATE:
    case ROTATERT:
      mem_loc_result = rotate_loc_descriptor (rtl, mode, mem_mode);
      break;

    case COMPARE:
      /* In theory, we could implement the above.  */
      /* DWARF cannot represent the unsigned compare operations
	 natively.  */
    case SS_MULT:
    case US_MULT:
    case SS_DIV:
    case US_DIV:
    case SS_PLUS:
    case US_PLUS:
    case SS_MINUS:
    case US_MINUS:
    case SS_NEG:
    case US_NEG:
    case SS_ABS:
    case SS_ASHIFT:
    case US_ASHIFT:
    case SS_TRUNCATE:
    case US_TRUNCATE:
    case UNORDERED:
    case ORDERED:
    case UNEQ:
    case UNGE:
    case UNGT:
    case UNLE:
    case UNLT:
    case LTGT:
    case FRACT_CONVERT:
    case UNSIGNED_FRACT_CONVERT:
    case SAT_FRACT:
    case UNSIGNED_SAT_FRACT:
    case SQRT:
    case ASM_OPERANDS:
    case VEC_MERGE:
    case VEC_SELECT:
    case VEC_CONCAT:
    case VEC_DUPLICATE:
    case UNSPEC:
    case HIGH:
    case FMA:
    case STRICT_LOW_PART:
    case CONST_VECTOR:
    case CONST_FIXED:
    case CLRSB:
    case CLOBBER:
      /* If delegitimize_address couldn't do anything with the UNSPEC, we
	 can't express it in the debug info.  This can happen e.g. with some
	 TLS UNSPECs.  */
      break;

    case CONST_STRING:
      resolve_one_addr (&rtl);
      goto symref;

    /* RTL sequences inside PARALLEL record a series of DWARF operations for
       the expression.  An UNSPEC rtx represents a raw DWARF operation,
       new_loc_descr is called for it to build the operation directly.
       Otherwise mem_loc_descriptor is called recursively.  */
    case PARALLEL:
      {
	int index = 0;
	dw_loc_descr_ref exp_result = NULL;

	for (; index < XVECLEN (rtl, 0); index++)
	  {
	    rtx elem = XVECEXP (rtl, 0, index);
	    if (GET_CODE (elem) == UNSPEC)
	      {
		/* Each DWARF operation UNSPEC contain two operands, if
		   one operand is not used for the operation, const0_rtx is
		   passed.  */
		gcc_assert (XVECLEN (elem, 0) == 2);

		HOST_WIDE_INT dw_op = XINT (elem, 1);
		HOST_WIDE_INT oprnd1 = INTVAL (XVECEXP (elem, 0, 0));
		HOST_WIDE_INT oprnd2 = INTVAL (XVECEXP (elem, 0, 1));
		exp_result
		  = new_loc_descr ((enum dwarf_location_atom) dw_op, oprnd1,
				   oprnd2);
	      }
	    else
	      exp_result
		= mem_loc_descriptor (elem, mode, mem_mode,
				      VAR_INIT_STATUS_INITIALIZED);

	    if (!mem_loc_result)
	      mem_loc_result = exp_result;
	    else
	      add_loc_descr (&mem_loc_result, exp_result);
	  }

	break;
      }

    default:
      if (flag_checking)
	{
	  print_rtl (stderr, rtl);
	  gcc_unreachable ();
	}
      break;
    }

  if (mem_loc_result && initialized == VAR_INIT_STATUS_UNINITIALIZED)
    add_loc_descr (&mem_loc_result, new_loc_descr (DW_OP_GNU_uninit, 0, 0));

  return mem_loc_result;
}

/* Return a descriptor that describes the concatenation of two locations.
   This is typically a complex variable.  */

static dw_loc_descr_ref
concat_loc_descriptor (rtx x0, rtx x1, enum var_init_status initialized)
{
  dw_loc_descr_ref cc_loc_result = NULL;
  dw_loc_descr_ref x0_ref
    = loc_descriptor (x0, VOIDmode, VAR_INIT_STATUS_INITIALIZED);
  dw_loc_descr_ref x1_ref
    = loc_descriptor (x1, VOIDmode, VAR_INIT_STATUS_INITIALIZED);

  if (x0_ref == 0 || x1_ref == 0)
    return 0;

  cc_loc_result = x0_ref;
  add_loc_descr_op_piece (&cc_loc_result, GET_MODE_SIZE (GET_MODE (x0)));

  add_loc_descr (&cc_loc_result, x1_ref);
  add_loc_descr_op_piece (&cc_loc_result, GET_MODE_SIZE (GET_MODE (x1)));

  if (initialized == VAR_INIT_STATUS_UNINITIALIZED)
    add_loc_descr (&cc_loc_result, new_loc_descr (DW_OP_GNU_uninit, 0, 0));

  return cc_loc_result;
}

/* Return a descriptor that describes the concatenation of N
   locations.  */

static dw_loc_descr_ref
concatn_loc_descriptor (rtx concatn, enum var_init_status initialized)
{
  unsigned int i;
  dw_loc_descr_ref cc_loc_result = NULL;
  unsigned int n = XVECLEN (concatn, 0);

  for (i = 0; i < n; ++i)
    {
      dw_loc_descr_ref ref;
      rtx x = XVECEXP (concatn, 0, i);

      ref = loc_descriptor (x, VOIDmode, VAR_INIT_STATUS_INITIALIZED);
      if (ref == NULL)
	return NULL;

      add_loc_descr (&cc_loc_result, ref);
      add_loc_descr_op_piece (&cc_loc_result, GET_MODE_SIZE (GET_MODE (x)));
    }

  if (cc_loc_result && initialized == VAR_INIT_STATUS_UNINITIALIZED)
    add_loc_descr (&cc_loc_result, new_loc_descr (DW_OP_GNU_uninit, 0, 0));

  return cc_loc_result;
}

/* Helper function for loc_descriptor.  Return DW_OP_implicit_pointer
   for DEBUG_IMPLICIT_PTR RTL.  */

static dw_loc_descr_ref
implicit_ptr_descriptor (rtx rtl, HOST_WIDE_INT offset)
{
  dw_loc_descr_ref ret;
  dw_die_ref ref;

  if (dwarf_strict && dwarf_version < 5)
    return NULL;
  gcc_assert (TREE_CODE (DEBUG_IMPLICIT_PTR_DECL (rtl)) == VAR_DECL
	      || TREE_CODE (DEBUG_IMPLICIT_PTR_DECL (rtl)) == PARM_DECL
	      || TREE_CODE (DEBUG_IMPLICIT_PTR_DECL (rtl)) == RESULT_DECL);
  ref = lookup_decl_die (DEBUG_IMPLICIT_PTR_DECL (rtl));
  ret = new_loc_descr (dwarf_OP (DW_OP_implicit_pointer), 0, offset);
  ret->dw_loc_oprnd2.val_class = dw_val_class_const;
  if (ref)
    {
      ret->dw_loc_oprnd1.val_class = dw_val_class_die_ref;
      ret->dw_loc_oprnd1.v.val_die_ref.die = ref;
      ret->dw_loc_oprnd1.v.val_die_ref.external = 0;
    }
  else
    {
      ret->dw_loc_oprnd1.val_class = dw_val_class_decl_ref;
      ret->dw_loc_oprnd1.v.val_decl_ref = DEBUG_IMPLICIT_PTR_DECL (rtl);
    }
  return ret;
}

/* Output a proper Dwarf location descriptor for a variable or parameter
   which is either allocated in a register or in a memory location.  For a
   register, we just generate an OP_REG and the register number.  For a
   memory location we provide a Dwarf postfix expression describing how to
   generate the (dynamic) address of the object onto the address stack.

   MODE is mode of the decl if this loc_descriptor is going to be used in
   .debug_loc section where DW_OP_stack_value and DW_OP_implicit_value are
   allowed, VOIDmode otherwise.

   If we don't know how to describe it, return 0.  */

static dw_loc_descr_ref
loc_descriptor (rtx rtl, machine_mode mode,
		enum var_init_status initialized)
{
  dw_loc_descr_ref loc_result = NULL;

  switch (GET_CODE (rtl))
    {
    case SUBREG:
      /* The case of a subreg may arise when we have a local (register)
	 variable or a formal (register) parameter which doesn't quite fill
	 up an entire register.  For now, just assume that it is
	 legitimate to make the Dwarf info refer to the whole register which
	 contains the given subreg.  */
      if (REG_P (SUBREG_REG (rtl)) && subreg_lowpart_p (rtl))
	loc_result = loc_descriptor (SUBREG_REG (rtl),
				     GET_MODE (SUBREG_REG (rtl)), initialized);
      else
	goto do_default;
      break;

    case REG:
      loc_result = reg_loc_descriptor (rtl, initialized);
      break;

    case MEM:
      loc_result = mem_loc_descriptor (XEXP (rtl, 0), get_address_mode (rtl),
				       GET_MODE (rtl), initialized);
      if (loc_result == NULL)
	loc_result = tls_mem_loc_descriptor (rtl);
      if (loc_result == NULL)
	{
	  rtx new_rtl = avoid_constant_pool_reference (rtl);
	  if (new_rtl != rtl)
	    loc_result = loc_descriptor (new_rtl, mode, initialized);
	}
      break;

    case CONCAT:
      loc_result = concat_loc_descriptor (XEXP (rtl, 0), XEXP (rtl, 1),
					  initialized);
      break;

    case CONCATN:
      loc_result = concatn_loc_descriptor (rtl, initialized);
      break;

    case VAR_LOCATION:
      /* Single part.  */
      if (GET_CODE (PAT_VAR_LOCATION_LOC (rtl)) != PARALLEL)
	{
	  rtx loc = PAT_VAR_LOCATION_LOC (rtl);
	  if (GET_CODE (loc) == EXPR_LIST)
	    loc = XEXP (loc, 0);
	  loc_result = loc_descriptor (loc, mode, initialized);
	  break;
	}

      rtl = XEXP (rtl, 1);
      /* FALLTHRU */

    case PARALLEL:
      {
	rtvec par_elems = XVEC (rtl, 0);
	int num_elem = GET_NUM_ELEM (par_elems);
	machine_mode mode;
	int i;

	/* Create the first one, so we have something to add to.  */
	loc_result = loc_descriptor (XEXP (RTVEC_ELT (par_elems, 0), 0),
				     VOIDmode, initialized);
	if (loc_result == NULL)
	  return NULL;
	mode = GET_MODE (XEXP (RTVEC_ELT (par_elems, 0), 0));
	add_loc_descr_op_piece (&loc_result, GET_MODE_SIZE (mode));
	for (i = 1; i < num_elem; i++)
	  {
	    dw_loc_descr_ref temp;

	    temp = loc_descriptor (XEXP (RTVEC_ELT (par_elems, i), 0),
				   VOIDmode, initialized);
	    if (temp == NULL)
	      return NULL;
	    add_loc_descr (&loc_result, temp);
	    mode = GET_MODE (XEXP (RTVEC_ELT (par_elems, i), 0));
	    add_loc_descr_op_piece (&loc_result, GET_MODE_SIZE (mode));
	  }
      }
      break;

    case CONST_INT:
      if (mode != VOIDmode && mode != BLKmode)
	loc_result = address_of_int_loc_descriptor (GET_MODE_SIZE (mode),
						    INTVAL (rtl));
      break;

    case CONST_DOUBLE:
      if (mode == VOIDmode)
	mode = GET_MODE (rtl);

      if (mode != VOIDmode && (dwarf_version >= 4 || !dwarf_strict))
	{
	  gcc_assert (mode == GET_MODE (rtl) || VOIDmode == GET_MODE (rtl));

	  /* Note that a CONST_DOUBLE rtx could represent either an integer
	     or a floating-point constant.  A CONST_DOUBLE is used whenever
	     the constant requires more than one word in order to be
	     adequately represented.  We output CONST_DOUBLEs as blocks.  */
	  loc_result = new_loc_descr (DW_OP_implicit_value,
				      GET_MODE_SIZE (mode), 0);
#if TARGET_SUPPORTS_WIDE_INT == 0
	  if (!SCALAR_FLOAT_MODE_P (mode))
	    {
	      loc_result->dw_loc_oprnd2.val_class = dw_val_class_const_double;
	      loc_result->dw_loc_oprnd2.v.val_double
	        = rtx_to_double_int (rtl);
	    }
	  else
#endif
	    {
	      unsigned int length = GET_MODE_SIZE (mode);
	      unsigned char *array = ggc_vec_alloc<unsigned char> (length);

	      insert_float (rtl, array);
	      loc_result->dw_loc_oprnd2.val_class = dw_val_class_vec;
	      loc_result->dw_loc_oprnd2.v.val_vec.length = length / 4;
	      loc_result->dw_loc_oprnd2.v.val_vec.elt_size = 4;
	      loc_result->dw_loc_oprnd2.v.val_vec.array = array;
	    }
	}
      break;

    case CONST_WIDE_INT:
      if (mode == VOIDmode)
	mode = GET_MODE (rtl);

      if (mode != VOIDmode && (dwarf_version >= 4 || !dwarf_strict))
	{
	  loc_result = new_loc_descr (DW_OP_implicit_value,
				      GET_MODE_SIZE (mode), 0);
	  loc_result->dw_loc_oprnd2.val_class = dw_val_class_wide_int;
	  loc_result->dw_loc_oprnd2.v.val_wide = ggc_alloc<wide_int> ();
	  *loc_result->dw_loc_oprnd2.v.val_wide = rtx_mode_t (rtl, mode);
	}
      break;

    case CONST_VECTOR:
      if (mode == VOIDmode)
	mode = GET_MODE (rtl);

      if (mode != VOIDmode && (dwarf_version >= 4 || !dwarf_strict))
	{
	  unsigned int elt_size = GET_MODE_UNIT_SIZE (GET_MODE (rtl));
	  unsigned int length = CONST_VECTOR_NUNITS (rtl);
	  unsigned char *array
	    = ggc_vec_alloc<unsigned char> (length * elt_size);
	  unsigned int i;
	  unsigned char *p;
	  machine_mode imode = GET_MODE_INNER (mode);

	  gcc_assert (mode == GET_MODE (rtl) || VOIDmode == GET_MODE (rtl));
	  switch (GET_MODE_CLASS (mode))
	    {
	    case MODE_VECTOR_INT:
	      for (i = 0, p = array; i < length; i++, p += elt_size)
		{
		  rtx elt = CONST_VECTOR_ELT (rtl, i);
		  insert_wide_int (rtx_mode_t (elt, imode), p, elt_size);
		}
	      break;

	    case MODE_VECTOR_FLOAT:
	      for (i = 0, p = array; i < length; i++, p += elt_size)
		{
		  rtx elt = CONST_VECTOR_ELT (rtl, i);
		  insert_float (elt, p);
		}
	      break;

	    default:
	      gcc_unreachable ();
	    }

	  loc_result = new_loc_descr (DW_OP_implicit_value,
				      length * elt_size, 0);
	  loc_result->dw_loc_oprnd2.val_class = dw_val_class_vec;
	  loc_result->dw_loc_oprnd2.v.val_vec.length = length;
	  loc_result->dw_loc_oprnd2.v.val_vec.elt_size = elt_size;
	  loc_result->dw_loc_oprnd2.v.val_vec.array = array;
	}
      break;

    case CONST:
      if (mode == VOIDmode
	  || CONST_SCALAR_INT_P (XEXP (rtl, 0))
	  || CONST_DOUBLE_AS_FLOAT_P (XEXP (rtl, 0))
	  || GET_CODE (XEXP (rtl, 0)) == CONST_VECTOR)
	{
	  loc_result = loc_descriptor (XEXP (rtl, 0), mode, initialized);
	  break;
	}
      /* FALLTHROUGH */
    case SYMBOL_REF:
      if (!const_ok_for_output (rtl))
	break;
      /* FALLTHROUGH */
    case LABEL_REF:
      if (mode != VOIDmode && GET_MODE_SIZE (mode) == DWARF2_ADDR_SIZE
	  && (dwarf_version >= 4 || !dwarf_strict))
	{
         loc_result = new_addr_loc_descr (rtl, dtprel_false);
	  add_loc_descr (&loc_result, new_loc_descr (DW_OP_stack_value, 0, 0));
	  vec_safe_push (used_rtx_array, rtl);
	}
      break;

    case DEBUG_IMPLICIT_PTR:
      loc_result = implicit_ptr_descriptor (rtl, 0);
      break;

    case PLUS:
      if (GET_CODE (XEXP (rtl, 0)) == DEBUG_IMPLICIT_PTR
	  && CONST_INT_P (XEXP (rtl, 1)))
	{
	  loc_result
	    = implicit_ptr_descriptor (XEXP (rtl, 0), INTVAL (XEXP (rtl, 1)));
	  break;
	}
      /* FALLTHRU */
    do_default:
    default:
      if ((SCALAR_INT_MODE_P (mode)
	   && GET_MODE (rtl) == mode
	   && GET_MODE_SIZE (GET_MODE (rtl)) <= DWARF2_ADDR_SIZE
	   && dwarf_version >= 4)
	  || (!dwarf_strict && mode != VOIDmode && mode != BLKmode))
	{
	  /* Value expression.  */
	  loc_result = mem_loc_descriptor (rtl, mode, VOIDmode, initialized);
	  if (loc_result)
	    add_loc_descr (&loc_result,
			   new_loc_descr (DW_OP_stack_value, 0, 0));
	}
      break;
    }

  return loc_result;
}

/* We need to figure out what section we should use as the base for the
   address ranges where a given location is valid.
   1. If this particular DECL has a section associated with it, use that.
   2. If this function has a section associated with it, use that.
   3. Otherwise, use the text section.
   XXX: If you split a variable across multiple sections, we won't notice.  */

static const char *
secname_for_decl (const_tree decl)
{
  const char *secname;

  if (VAR_OR_FUNCTION_DECL_P (decl)
      && (DECL_EXTERNAL (decl) || TREE_PUBLIC (decl) || TREE_STATIC (decl))
      && DECL_SECTION_NAME (decl))
    secname = DECL_SECTION_NAME (decl);
  else if (current_function_decl && DECL_SECTION_NAME (current_function_decl))
    secname = DECL_SECTION_NAME (current_function_decl);
  else if (cfun && in_cold_section_p)
    secname = crtl->subsections.cold_section_label;
  else
    secname = text_section_label;

  return secname;
}

/* Return true when DECL_BY_REFERENCE is defined and set for DECL.  */

static bool
decl_by_reference_p (tree decl)
{
  return ((TREE_CODE (decl) == PARM_DECL || TREE_CODE (decl) == RESULT_DECL
  	   || VAR_P (decl))
	  && DECL_BY_REFERENCE (decl));
}

/* Helper function for dw_loc_list.  Compute proper Dwarf location descriptor
   for VARLOC.  */

static dw_loc_descr_ref
dw_loc_list_1 (tree loc, rtx varloc, int want_address,
	       enum var_init_status initialized)
{
  int have_address = 0;
  dw_loc_descr_ref descr;
  machine_mode mode;

  if (want_address != 2)
    {
      gcc_assert (GET_CODE (varloc) == VAR_LOCATION);
      /* Single part.  */
      if (GET_CODE (PAT_VAR_LOCATION_LOC (varloc)) != PARALLEL)
	{
	  varloc = PAT_VAR_LOCATION_LOC (varloc);
	  if (GET_CODE (varloc) == EXPR_LIST)
	    varloc = XEXP (varloc, 0);
	  mode = GET_MODE (varloc);
	  if (MEM_P (varloc))
	    {
	      rtx addr = XEXP (varloc, 0);
	      descr = mem_loc_descriptor (addr, get_address_mode (varloc),
					  mode, initialized);
	      if (descr)
		have_address = 1;
	      else
		{
		  rtx x = avoid_constant_pool_reference (varloc);
		  if (x != varloc)
		    descr = mem_loc_descriptor (x, mode, VOIDmode,
						initialized);
		}
	    }
	  else
	    descr = mem_loc_descriptor (varloc, mode, VOIDmode, initialized);
	}
      else
	return 0;
    }
  else
    {
      if (GET_CODE (varloc) == VAR_LOCATION)
	mode = DECL_MODE (PAT_VAR_LOCATION_DECL (varloc));
      else
	mode = DECL_MODE (loc);
      descr = loc_descriptor (varloc, mode, initialized);
      have_address = 1;
    }

  if (!descr)
    return 0;

  if (want_address == 2 && !have_address
      && (dwarf_version >= 4 || !dwarf_strict))
    {
      if (int_size_in_bytes (TREE_TYPE (loc)) > DWARF2_ADDR_SIZE)
	{
	  expansion_failed (loc, NULL_RTX,
			    "DWARF address size mismatch");
	  return 0;
	}
      add_loc_descr (&descr, new_loc_descr (DW_OP_stack_value, 0, 0));
      have_address = 1;
    }
  /* Show if we can't fill the request for an address.  */
  if (want_address && !have_address)
    {
      expansion_failed (loc, NULL_RTX,
			"Want address and only have value");
      return 0;
    }

  /* If we've got an address and don't want one, dereference.  */
  if (!want_address && have_address)
    {
      HOST_WIDE_INT size = int_size_in_bytes (TREE_TYPE (loc));
      enum dwarf_location_atom op;

      if (size > DWARF2_ADDR_SIZE || size == -1)
	{
	  expansion_failed (loc, NULL_RTX,
			    "DWARF address size mismatch");
	  return 0;
	}
      else if (size == DWARF2_ADDR_SIZE)
	op = DW_OP_deref;
      else
	op = DW_OP_deref_size;

      add_loc_descr (&descr, new_loc_descr (op, size, 0));
    }

  return descr;
}

/* Create a DW_OP_piece or DW_OP_bit_piece for bitsize, or return NULL
   if it is not possible.  */

static dw_loc_descr_ref
new_loc_descr_op_bit_piece (HOST_WIDE_INT bitsize, HOST_WIDE_INT offset)
{
  if ((bitsize % BITS_PER_UNIT) == 0 && offset == 0)
    return new_loc_descr (DW_OP_piece, bitsize / BITS_PER_UNIT, 0);
  else if (dwarf_version >= 3 || !dwarf_strict)
    return new_loc_descr (DW_OP_bit_piece, bitsize, offset);
  else
    return NULL;
}

/* Helper function for dw_loc_list.  Compute proper Dwarf location descriptor
   for VAR_LOC_NOTE for variable DECL that has been optimized by SRA.  */

static dw_loc_descr_ref
dw_sra_loc_expr (tree decl, rtx loc)
{
  rtx p;
  unsigned HOST_WIDE_INT padsize = 0;
  dw_loc_descr_ref descr, *descr_tail;
  unsigned HOST_WIDE_INT decl_size;
  rtx varloc;
  enum var_init_status initialized;

  if (DECL_SIZE (decl) == NULL
      || !tree_fits_uhwi_p (DECL_SIZE (decl)))
    return NULL;

  decl_size = tree_to_uhwi (DECL_SIZE (decl));
  descr = NULL;
  descr_tail = &descr;

  for (p = loc; p; p = XEXP (p, 1))
    {
      unsigned HOST_WIDE_INT bitsize = decl_piece_bitsize (p);
      rtx loc_note = *decl_piece_varloc_ptr (p);
      dw_loc_descr_ref cur_descr;
      dw_loc_descr_ref *tail, last = NULL;
      unsigned HOST_WIDE_INT opsize = 0;

      if (loc_note == NULL_RTX
	  || NOTE_VAR_LOCATION_LOC (loc_note) == NULL_RTX)
	{
	  padsize += bitsize;
	  continue;
	}
      initialized = NOTE_VAR_LOCATION_STATUS (loc_note);
      varloc = NOTE_VAR_LOCATION (loc_note);
      cur_descr = dw_loc_list_1 (decl, varloc, 2, initialized);
      if (cur_descr == NULL)
	{
	  padsize += bitsize;
	  continue;
	}

      /* Check that cur_descr either doesn't use
	 DW_OP_*piece operations, or their sum is equal
	 to bitsize.  Otherwise we can't embed it.  */
      for (tail = &cur_descr; *tail != NULL;
	   tail = &(*tail)->dw_loc_next)
	if ((*tail)->dw_loc_opc == DW_OP_piece)
	  {
	    opsize += (*tail)->dw_loc_oprnd1.v.val_unsigned
		      * BITS_PER_UNIT;
	    last = *tail;
	  }
	else if ((*tail)->dw_loc_opc == DW_OP_bit_piece)
	  {
	    opsize += (*tail)->dw_loc_oprnd1.v.val_unsigned;
	    last = *tail;
	  }

      if (last != NULL && opsize != bitsize)
	{
	  padsize += bitsize;
	  /* Discard the current piece of the descriptor and release any
	     addr_table entries it uses.  */
	  remove_loc_list_addr_table_entries (cur_descr);
	  continue;
	}

      /* If there is a hole, add DW_OP_*piece after empty DWARF
	 expression, which means that those bits are optimized out.  */
      if (padsize)
	{
	  if (padsize > decl_size)
	    {
	      remove_loc_list_addr_table_entries (cur_descr);
	      goto discard_descr;
	    }
	  decl_size -= padsize;
	  *descr_tail = new_loc_descr_op_bit_piece (padsize, 0);
	  if (*descr_tail == NULL)
	    {
	      remove_loc_list_addr_table_entries (cur_descr);
	      goto discard_descr;
	    }
	  descr_tail = &(*descr_tail)->dw_loc_next;
	  padsize = 0;
	}
      *descr_tail = cur_descr;
      descr_tail = tail;
      if (bitsize > decl_size)
	goto discard_descr;
      decl_size -= bitsize;
      if (last == NULL)
	{
	  HOST_WIDE_INT offset = 0;
	  if (GET_CODE (varloc) == VAR_LOCATION
	      && GET_CODE (PAT_VAR_LOCATION_LOC (varloc)) != PARALLEL)
	    {
	      varloc = PAT_VAR_LOCATION_LOC (varloc);
	      if (GET_CODE (varloc) == EXPR_LIST)
		varloc = XEXP (varloc, 0);
	    }
	  do 
	    {
	      if (GET_CODE (varloc) == CONST
		  || GET_CODE (varloc) == SIGN_EXTEND
		  || GET_CODE (varloc) == ZERO_EXTEND)
		varloc = XEXP (varloc, 0);
	      else if (GET_CODE (varloc) == SUBREG)
		varloc = SUBREG_REG (varloc);
	      else
		break;
	    }
	  while (1);
	  /* DW_OP_bit_size offset should be zero for register
	     or implicit location descriptions and empty location
	     descriptions, but for memory addresses needs big endian
	     adjustment.  */
	  if (MEM_P (varloc))
	    {
	      unsigned HOST_WIDE_INT memsize
		= MEM_SIZE (varloc) * BITS_PER_UNIT;
	      if (memsize != bitsize)
		{
		  if (BYTES_BIG_ENDIAN != WORDS_BIG_ENDIAN
		      && (memsize > BITS_PER_WORD || bitsize > BITS_PER_WORD))
		    goto discard_descr;
		  if (memsize < bitsize)
		    goto discard_descr;
		  if (BITS_BIG_ENDIAN)
		    offset = memsize - bitsize;
		}
	    }

	  *descr_tail = new_loc_descr_op_bit_piece (bitsize, offset);
	  if (*descr_tail == NULL)
	    goto discard_descr;
	  descr_tail = &(*descr_tail)->dw_loc_next;
	}
    }

  /* If there were any non-empty expressions, add padding till the end of
     the decl.  */
  if (descr != NULL && decl_size != 0)
    {
      *descr_tail = new_loc_descr_op_bit_piece (decl_size, 0);
      if (*descr_tail == NULL)
	goto discard_descr;
    }
  return descr;

discard_descr:
  /* Discard the descriptor and release any addr_table entries it uses.  */
  remove_loc_list_addr_table_entries (descr);
  return NULL;
}

/* Return the dwarf representation of the location list LOC_LIST of
   DECL.  WANT_ADDRESS has the same meaning as in loc_list_from_tree
   function.  */

static dw_loc_list_ref
dw_loc_list (var_loc_list *loc_list, tree decl, int want_address)
{
  const char *endname, *secname;
  rtx varloc;
  enum var_init_status initialized;
  struct var_loc_node *node;
  dw_loc_descr_ref descr;
  char label_id[MAX_ARTIFICIAL_LABEL_BYTES];
  dw_loc_list_ref list = NULL;
  dw_loc_list_ref *listp = &list;

  /* Now that we know what section we are using for a base,
     actually construct the list of locations.
     The first location information is what is passed to the
     function that creates the location list, and the remaining
     locations just get added on to that list.
     Note that we only know the start address for a location
     (IE location changes), so to build the range, we use
     the range [current location start, next location start].
     This means we have to special case the last node, and generate
     a range of [last location start, end of function label].  */

  secname = secname_for_decl (decl);

  for (node = loc_list->first; node; node = node->next)
    if (GET_CODE (node->loc) == EXPR_LIST
	|| NOTE_VAR_LOCATION_LOC (node->loc) != NULL_RTX)
      {
	if (GET_CODE (node->loc) == EXPR_LIST)
	  {
	    /* This requires DW_OP_{,bit_}piece, which is not usable
	       inside DWARF expressions.  */
	    if (want_address != 2)
	      continue;
	    descr = dw_sra_loc_expr (decl, node->loc);
	    if (descr == NULL)
	      continue;
	  }
	else
	  {
	    initialized = NOTE_VAR_LOCATION_STATUS (node->loc);
	    varloc = NOTE_VAR_LOCATION (node->loc);
	    descr = dw_loc_list_1 (decl, varloc, want_address, initialized);
	  }
	if (descr)
	  {
	    bool range_across_switch = false;
	    /* If section switch happens in between node->label
	       and node->next->label (or end of function) and
	       we can't emit it as a single entry list,
	       emit two ranges, first one ending at the end
	       of first partition and second one starting at the
	       beginning of second partition.  */
	    if (node == loc_list->last_before_switch
		&& (node != loc_list->first || loc_list->first->next)
		&& current_function_decl)
	      {
		endname = cfun->fde->dw_fde_end;
		range_across_switch = true;
	      }
	    /* The variable has a location between NODE->LABEL and
	       NODE->NEXT->LABEL.  */
	    else if (node->next)
	      endname = node->next->label;
	    /* If the variable has a location at the last label
	       it keeps its location until the end of function.  */
	    else if (!current_function_decl)
	      endname = text_end_label;
	    else
	      {
		ASM_GENERATE_INTERNAL_LABEL (label_id, FUNC_END_LABEL,
					     current_function_funcdef_no);
		endname = ggc_strdup (label_id);
	      }

	    *listp = new_loc_list (descr, node->label, endname, secname);
	    if (TREE_CODE (decl) == PARM_DECL
		&& node == loc_list->first
		&& NOTE_P (node->loc)
		&& strcmp (node->label, endname) == 0)
	      (*listp)->force = true;
	    listp = &(*listp)->dw_loc_next;

	    if (range_across_switch)
	      {
		if (GET_CODE (node->loc) == EXPR_LIST)
		  descr = dw_sra_loc_expr (decl, node->loc);
		else
		  {
		    initialized = NOTE_VAR_LOCATION_STATUS (node->loc);
		    varloc = NOTE_VAR_LOCATION (node->loc);
		    descr = dw_loc_list_1 (decl, varloc, want_address,
					   initialized);
		  }
		gcc_assert (descr);
		/* The variable has a location between NODE->LABEL and
		   NODE->NEXT->LABEL.  */
		if (node->next)
		  endname = node->next->label;
		else
		  endname = cfun->fde->dw_fde_second_end;
		*listp = new_loc_list (descr,
				       cfun->fde->dw_fde_second_begin,
				       endname, secname);
		listp = &(*listp)->dw_loc_next;
	      }
	  }
      }

  /* Try to avoid the overhead of a location list emitting a location
     expression instead, but only if we didn't have more than one
     location entry in the first place.  If some entries were not
     representable, we don't want to pretend a single entry that was
     applies to the entire scope in which the variable is
     available.  */
  if (list && loc_list->first->next)
    gen_llsym (list);

  return list;
}

/* Return if the loc_list has only single element and thus can be represented
   as location description.   */

static bool
single_element_loc_list_p (dw_loc_list_ref list)
{
  gcc_assert (!list->dw_loc_next || list->ll_symbol);
  return !list->ll_symbol;
}

/* Duplicate a single element of location list.  */

static inline dw_loc_descr_ref
copy_loc_descr (dw_loc_descr_ref ref)
{
  dw_loc_descr_ref copy = ggc_alloc<dw_loc_descr_node> ();
  memcpy (copy, ref, sizeof (dw_loc_descr_node));
  return copy;
}

/* To each location in list LIST append loc descr REF.  */

static void
add_loc_descr_to_each (dw_loc_list_ref list, dw_loc_descr_ref ref)
{
  dw_loc_descr_ref copy;
  add_loc_descr (&list->expr, ref);
  list = list->dw_loc_next;
  while (list)
    {
      copy = copy_loc_descr (ref);
      add_loc_descr (&list->expr, copy);
      while (copy->dw_loc_next)
	copy = copy->dw_loc_next = copy_loc_descr (copy->dw_loc_next);
      list = list->dw_loc_next;
    }
}

/* To each location in list LIST prepend loc descr REF.  */

static void
prepend_loc_descr_to_each (dw_loc_list_ref list, dw_loc_descr_ref ref)
{
  dw_loc_descr_ref copy;
  dw_loc_descr_ref ref_end = list->expr;
  add_loc_descr (&ref, list->expr);
  list->expr = ref;
  list = list->dw_loc_next;
  while (list)
    {
      dw_loc_descr_ref end = list->expr;
      list->expr = copy = copy_loc_descr (ref);
      while (copy->dw_loc_next != ref_end)
	copy = copy->dw_loc_next = copy_loc_descr (copy->dw_loc_next);
      copy->dw_loc_next = end;
      list = list->dw_loc_next;
    }
}

/* Given two lists RET and LIST
   produce location list that is result of adding expression in LIST
   to expression in RET on each position in program.
   Might be destructive on both RET and LIST.

   TODO: We handle only simple cases of RET or LIST having at most one
   element.  General case would involve sorting the lists in program order
   and merging them that will need some additional work.
   Adding that will improve quality of debug info especially for SRA-ed
   structures.  */

static void
add_loc_list (dw_loc_list_ref *ret, dw_loc_list_ref list)
{
  if (!list)
    return;
  if (!*ret)
    {
      *ret = list;
      return;
    }
  if (!list->dw_loc_next)
    {
      add_loc_descr_to_each (*ret, list->expr);
      return;
    }
  if (!(*ret)->dw_loc_next)
    {
      prepend_loc_descr_to_each (list, (*ret)->expr);
      *ret = list;
      return;
    }
  expansion_failed (NULL_TREE, NULL_RTX,
		    "Don't know how to merge two non-trivial"
		    " location lists.\n");
  *ret = NULL;
  return;
}

/* LOC is constant expression.  Try a luck, look it up in constant
   pool and return its loc_descr of its address.  */

static dw_loc_descr_ref
cst_pool_loc_descr (tree loc)
{
  /* Get an RTL for this, if something has been emitted.  */
  rtx rtl = lookup_constant_def (loc);

  if (!rtl || !MEM_P (rtl))
    {
      gcc_assert (!rtl);
      return 0;
    }
  gcc_assert (GET_CODE (XEXP (rtl, 0)) == SYMBOL_REF);

  /* TODO: We might get more coverage if we was actually delaying expansion
     of all expressions till end of compilation when constant pools are fully
     populated.  */
  if (!TREE_ASM_WRITTEN (SYMBOL_REF_DECL (XEXP (rtl, 0))))
    {
      expansion_failed (loc, NULL_RTX,
			"CST value in contant pool but not marked.");
      return 0;
    }
  return mem_loc_descriptor (XEXP (rtl, 0), get_address_mode (rtl),
			     GET_MODE (rtl), VAR_INIT_STATUS_INITIALIZED);
}

/* Return dw_loc_list representing address of addr_expr LOC
   by looking for inner INDIRECT_REF expression and turning
   it into simple arithmetics.

   See loc_list_from_tree for the meaning of CONTEXT.  */

static dw_loc_list_ref
loc_list_for_address_of_addr_expr_of_indirect_ref (tree loc, bool toplev,
						   loc_descr_context *context)
{
  tree obj, offset;
  HOST_WIDE_INT bitsize, bitpos, bytepos;
  machine_mode mode;
  int unsignedp, reversep, volatilep = 0;
  dw_loc_list_ref list_ret = NULL, list_ret1 = NULL;

  obj = get_inner_reference (TREE_OPERAND (loc, 0),
			     &bitsize, &bitpos, &offset, &mode,
			     &unsignedp, &reversep, &volatilep);
  STRIP_NOPS (obj);
  if (bitpos % BITS_PER_UNIT)
    {
      expansion_failed (loc, NULL_RTX, "bitfield access");
      return 0;
    }
  if (!INDIRECT_REF_P (obj))
    {
      expansion_failed (obj,
			NULL_RTX, "no indirect ref in inner refrence");
      return 0;
    }
  if (!offset && !bitpos)
    list_ret = loc_list_from_tree (TREE_OPERAND (obj, 0), toplev ? 2 : 1,
				   context);
  else if (toplev
	   && int_size_in_bytes (TREE_TYPE (loc)) <= DWARF2_ADDR_SIZE
	   && (dwarf_version >= 4 || !dwarf_strict))
    {
      list_ret = loc_list_from_tree (TREE_OPERAND (obj, 0), 0, context);
      if (!list_ret)
	return 0;
      if (offset)
	{
	  /* Variable offset.  */
	  list_ret1 = loc_list_from_tree (offset, 0, context);
	  if (list_ret1 == 0)
	    return 0;
	  add_loc_list (&list_ret, list_ret1);
	  if (!list_ret)
	    return 0;
	  add_loc_descr_to_each (list_ret,
				 new_loc_descr (DW_OP_plus, 0, 0));
	}
      bytepos = bitpos / BITS_PER_UNIT;
      if (bytepos > 0)
	add_loc_descr_to_each (list_ret,
			       new_loc_descr (DW_OP_plus_uconst,
					      bytepos, 0));
      else if (bytepos < 0)
	loc_list_plus_const (list_ret, bytepos);
      add_loc_descr_to_each (list_ret,
			     new_loc_descr (DW_OP_stack_value, 0, 0));
    }
  return list_ret;
}

/* Set LOC to the next operation that is not a DW_OP_nop operation. In the case
   all operations from LOC are nops, move to the last one.  Insert in NOPS all
   operations that are skipped.  */

static void
loc_descr_to_next_no_nop (dw_loc_descr_ref &loc,
			  hash_set<dw_loc_descr_ref> &nops)
{
  while (loc->dw_loc_next != NULL && loc->dw_loc_opc == DW_OP_nop)
    {
      nops.add (loc);
      loc = loc->dw_loc_next;
    }
}

/* Helper for loc_descr_without_nops: free the location description operation
   P.  */

bool
free_loc_descr (const dw_loc_descr_ref &loc, void *data ATTRIBUTE_UNUSED)
{
  ggc_free (loc);
  return true;
}

/* Remove all DW_OP_nop operations from LOC except, if it exists, the one that
   finishes LOC.  */

static void
loc_descr_without_nops (dw_loc_descr_ref &loc)
{
  if (loc->dw_loc_opc == DW_OP_nop && loc->dw_loc_next == NULL)
    return;

  /* Set of all DW_OP_nop operations we remove.  */
  hash_set<dw_loc_descr_ref> nops;

  /* First, strip all prefix NOP operations in order to keep the head of the
     operations list.  */
  loc_descr_to_next_no_nop (loc, nops);

  for (dw_loc_descr_ref cur = loc; cur != NULL;)
    {
      /* For control flow operations: strip "prefix" nops in destination
	 labels.  */
      if (cur->dw_loc_oprnd1.val_class == dw_val_class_loc)
	loc_descr_to_next_no_nop (cur->dw_loc_oprnd1.v.val_loc, nops);
      if (cur->dw_loc_oprnd2.val_class == dw_val_class_loc)
	loc_descr_to_next_no_nop (cur->dw_loc_oprnd2.v.val_loc, nops);

      /* Do the same for the operations that follow, then move to the next
	 iteration.  */
      if (cur->dw_loc_next != NULL)
	loc_descr_to_next_no_nop (cur->dw_loc_next, nops);
      cur = cur->dw_loc_next;
    }

  nops.traverse<void *, free_loc_descr> (NULL);
}


struct dwarf_procedure_info;

/* Helper structure for location descriptions generation.  */
struct loc_descr_context
{
  /* The type that is implicitly referenced by DW_OP_push_object_address, or
     NULL_TREE if DW_OP_push_object_address in invalid for this location
     description.  This is used when processing PLACEHOLDER_EXPR nodes.  */
  tree context_type;
  /* The ..._DECL node that should be translated as a
     DW_OP_push_object_address operation.  */
  tree base_decl;
  /* Information about the DWARF procedure we are currently generating. NULL if
     we are not generating a DWARF procedure.  */
  struct dwarf_procedure_info *dpi;
  /* True if integral PLACEHOLDER_EXPR stands for the first argument passed
     by consumer.  Used for DW_TAG_generic_subrange attributes.  */
  bool placeholder_arg;
  /* True if PLACEHOLDER_EXPR has been seen.  */
  bool placeholder_seen;
};

/* DWARF procedures generation

   DWARF expressions (aka. location descriptions) are used to encode variable
   things such as sizes or offsets.  Such computations can have redundant parts
   that can be factorized in order to reduce the size of the output debug
   information.  This is the whole point of DWARF procedures.

   Thanks to stor-layout.c, size and offset expressions in GENERIC trees are
   already factorized into functions ("size functions") in order to handle very
   big and complex types.  Such functions are quite simple: they have integral
   arguments, they return an integral result and their body contains only a
   return statement with arithmetic expressions.  This is the only kind of
   function we are interested in translating into DWARF procedures, here.

   DWARF expressions and DWARF procedure are executed using a stack, so we have
   to define some calling convention for them to interact.  Let's say that:

   - Before calling a DWARF procedure, DWARF expressions must push on the stack
     all arguments in reverse order (right-to-left) so that when the DWARF
     procedure execution starts, the first argument is the top of the stack.

   - Then, when returning, the DWARF procedure must have consumed all arguments
     on the stack, must have pushed the result and touched nothing else.

   - Each integral argument and the result are integral types can be hold in a
     single stack slot.

   - We call "frame offset" the number of stack slots that are "under DWARF
     procedure control": it includes the arguments slots, the temporaries and
     the result slot. Thus, it is equal to the number of arguments when the
     procedure execution starts and must be equal to one (the result) when it
     returns.  */

/* Helper structure used when generating operations for a DWARF procedure.  */
struct dwarf_procedure_info
{
  /* The FUNCTION_DECL node corresponding to the DWARF procedure that is
     currently translated.  */
  tree fndecl;
  /* The number of arguments FNDECL takes.  */
  unsigned args_count;
};

/* Return a pointer to a newly created DIE node for a DWARF procedure.  Add
   LOCATION as its DW_AT_location attribute.  If FNDECL is not NULL_TREE,
   equate it to this DIE.  */

static dw_die_ref
new_dwarf_proc_die (dw_loc_descr_ref location, tree fndecl,
		    dw_die_ref parent_die)
{
  dw_die_ref dwarf_proc_die;

  if ((dwarf_version < 3 && dwarf_strict)
      || location == NULL)
    return NULL;

  dwarf_proc_die = new_die (DW_TAG_dwarf_procedure, parent_die, fndecl);
  if (fndecl)
    equate_decl_number_to_die (fndecl, dwarf_proc_die);
  add_AT_loc (dwarf_proc_die, DW_AT_location, location);
  return dwarf_proc_die;
}

/* Return whether TYPE is a supported type as a DWARF procedure argument
   type or return type (we handle only scalar types and pointer types that
   aren't wider than the DWARF expression evaluation stack.  */

static bool
is_handled_procedure_type (tree type)
{
  return ((INTEGRAL_TYPE_P (type)
	   || TREE_CODE (type) == OFFSET_TYPE
	   || TREE_CODE (type) == POINTER_TYPE)
	  && int_size_in_bytes (type) <= DWARF2_ADDR_SIZE);
}

/* Helper for resolve_args_picking: do the same but stop when coming across
   visited nodes.  For each node we visit, register in FRAME_OFFSETS the frame
   offset *before* evaluating the corresponding operation.  */

static bool
resolve_args_picking_1 (dw_loc_descr_ref loc, unsigned initial_frame_offset,
			struct dwarf_procedure_info *dpi,
			hash_map<dw_loc_descr_ref, unsigned> &frame_offsets)
{
  /* The "frame_offset" identifier is already used to name a macro... */
  unsigned frame_offset_ = initial_frame_offset;
  dw_loc_descr_ref l;

  for (l = loc; l != NULL;)
    {
      bool existed;
      unsigned &l_frame_offset = frame_offsets.get_or_insert (l, &existed);

      /* If we already met this node, there is nothing to compute anymore.  */
      if (existed)
	{
	  /* Make sure that the stack size is consistent wherever the execution
	     flow comes from.  */
	  gcc_assert ((unsigned) l_frame_offset == frame_offset_);
	  break;
	}
      l_frame_offset = frame_offset_;

      /* If needed, relocate the picking offset with respect to the frame
	 offset. */
      if (l->frame_offset_rel)
	{
	  unsigned HOST_WIDE_INT off;
	  switch (l->dw_loc_opc)
	    {
	    case DW_OP_pick:
	      off = l->dw_loc_oprnd1.v.val_unsigned;
	      break;
	    case DW_OP_dup:
	      off = 0;
	      break;
	    case DW_OP_over:
	      off = 1;
	      break;
	    default:
	      gcc_unreachable ();
	    }
	  /* frame_offset_ is the size of the current stack frame, including
	     incoming arguments. Besides, the arguments are pushed
	     right-to-left.  Thus, in order to access the Nth argument from
	     this operation node, the picking has to skip temporaries *plus*
	     one stack slot per argument (0 for the first one, 1 for the second
	     one, etc.).

	     The targetted argument number (N) is already set as the operand,
	     and the number of temporaries can be computed with:
	       frame_offsets_ - dpi->args_count */
	  off += frame_offset_ - dpi->args_count;

	  /* DW_OP_pick handles only offsets from 0 to 255 (inclusive)...  */
	  if (off > 255)
	    return false;

	  if (off == 0)
	    {
	      l->dw_loc_opc = DW_OP_dup;
	      l->dw_loc_oprnd1.v.val_unsigned = 0;
	    }
	  else if (off == 1)
	    {
	      l->dw_loc_opc = DW_OP_over;
	      l->dw_loc_oprnd1.v.val_unsigned = 0;
	    }
	  else
	    {
	      l->dw_loc_opc = DW_OP_pick;
	      l->dw_loc_oprnd1.v.val_unsigned = off;
	    }
	}

      /* Update frame_offset according to the effect the current operation has
	 on the stack.  */
      switch (l->dw_loc_opc)
	{
	case DW_OP_deref:
	case DW_OP_swap:
	case DW_OP_rot:
	case DW_OP_abs:
	case DW_OP_neg:
	case DW_OP_not:
	case DW_OP_plus_uconst:
	case DW_OP_skip:
	case DW_OP_reg0:
	case DW_OP_reg1:
	case DW_OP_reg2:
	case DW_OP_reg3:
	case DW_OP_reg4:
	case DW_OP_reg5:
	case DW_OP_reg6:
	case DW_OP_reg7:
	case DW_OP_reg8:
	case DW_OP_reg9:
	case DW_OP_reg10:
	case DW_OP_reg11:
	case DW_OP_reg12:
	case DW_OP_reg13:
	case DW_OP_reg14:
	case DW_OP_reg15:
	case DW_OP_reg16:
	case DW_OP_reg17:
	case DW_OP_reg18:
	case DW_OP_reg19:
	case DW_OP_reg20:
	case DW_OP_reg21:
	case DW_OP_reg22:
	case DW_OP_reg23:
	case DW_OP_reg24:
	case DW_OP_reg25:
	case DW_OP_reg26:
	case DW_OP_reg27:
	case DW_OP_reg28:
	case DW_OP_reg29:
	case DW_OP_reg30:
	case DW_OP_reg31:
	case DW_OP_bregx:
	case DW_OP_piece:
	case DW_OP_deref_size:
	case DW_OP_nop:
	case DW_OP_bit_piece:
	case DW_OP_implicit_value:
	case DW_OP_stack_value:
	  break;

	case DW_OP_addr:
	case DW_OP_const1u:
	case DW_OP_const1s:
	case DW_OP_const2u:
	case DW_OP_const2s:
	case DW_OP_const4u:
	case DW_OP_const4s:
	case DW_OP_const8u:
	case DW_OP_const8s:
	case DW_OP_constu:
	case DW_OP_consts:
	case DW_OP_dup:
	case DW_OP_over:
	case DW_OP_pick:
	case DW_OP_lit0:
	case DW_OP_lit1:
	case DW_OP_lit2:
	case DW_OP_lit3:
	case DW_OP_lit4:
	case DW_OP_lit5:
	case DW_OP_lit6:
	case DW_OP_lit7:
	case DW_OP_lit8:
	case DW_OP_lit9:
	case DW_OP_lit10:
	case DW_OP_lit11:
	case DW_OP_lit12:
	case DW_OP_lit13:
	case DW_OP_lit14:
	case DW_OP_lit15:
	case DW_OP_lit16:
	case DW_OP_lit17:
	case DW_OP_lit18:
	case DW_OP_lit19:
	case DW_OP_lit20:
	case DW_OP_lit21:
	case DW_OP_lit22:
	case DW_OP_lit23:
	case DW_OP_lit24:
	case DW_OP_lit25:
	case DW_OP_lit26:
	case DW_OP_lit27:
	case DW_OP_lit28:
	case DW_OP_lit29:
	case DW_OP_lit30:
	case DW_OP_lit31:
	case DW_OP_breg0:
	case DW_OP_breg1:
	case DW_OP_breg2:
	case DW_OP_breg3:
	case DW_OP_breg4:
	case DW_OP_breg5:
	case DW_OP_breg6:
	case DW_OP_breg7:
	case DW_OP_breg8:
	case DW_OP_breg9:
	case DW_OP_breg10:
	case DW_OP_breg11:
	case DW_OP_breg12:
	case DW_OP_breg13:
	case DW_OP_breg14:
	case DW_OP_breg15:
	case DW_OP_breg16:
	case DW_OP_breg17:
	case DW_OP_breg18:
	case DW_OP_breg19:
	case DW_OP_breg20:
	case DW_OP_breg21:
	case DW_OP_breg22:
	case DW_OP_breg23:
	case DW_OP_breg24:
	case DW_OP_breg25:
	case DW_OP_breg26:
	case DW_OP_breg27:
	case DW_OP_breg28:
	case DW_OP_breg29:
	case DW_OP_breg30:
	case DW_OP_breg31:
	case DW_OP_fbreg:
	case DW_OP_push_object_address:
	case DW_OP_call_frame_cfa:
	case DW_OP_GNU_variable_value:
	  ++frame_offset_;
	  break;

	case DW_OP_drop:
	case DW_OP_xderef:
	case DW_OP_and:
	case DW_OP_div:
	case DW_OP_minus:
	case DW_OP_mod:
	case DW_OP_mul:
	case DW_OP_or:
	case DW_OP_plus:
	case DW_OP_shl:
	case DW_OP_shr:
	case DW_OP_shra:
	case DW_OP_xor:
	case DW_OP_bra:
	case DW_OP_eq:
	case DW_OP_ge:
	case DW_OP_gt:
	case DW_OP_le:
	case DW_OP_lt:
	case DW_OP_ne:
	case DW_OP_regx:
	case DW_OP_xderef_size:
	  --frame_offset_;
	  break;

	case DW_OP_call2:
	case DW_OP_call4:
	case DW_OP_call_ref:
	  {
	    dw_die_ref dwarf_proc = l->dw_loc_oprnd1.v.val_die_ref.die;
	    int *stack_usage = dwarf_proc_stack_usage_map->get (dwarf_proc);

	    if (stack_usage == NULL)
	      return false;
	    frame_offset_ += *stack_usage;
	    break;
	  }

	case DW_OP_implicit_pointer:
	case DW_OP_entry_value:
	case DW_OP_const_type:
	case DW_OP_regval_type:
	case DW_OP_deref_type:
	case DW_OP_convert:
	case DW_OP_reinterpret:
	case DW_OP_form_tls_address:
	case DW_OP_GNU_push_tls_address:
	case DW_OP_GNU_uninit:
	case DW_OP_GNU_encoded_addr:
	case DW_OP_GNU_implicit_pointer:
	case DW_OP_GNU_entry_value:
	case DW_OP_GNU_const_type:
	case DW_OP_GNU_regval_type:
	case DW_OP_GNU_deref_type:
	case DW_OP_GNU_convert:
	case DW_OP_GNU_reinterpret:
	case DW_OP_GNU_parameter_ref:
	  /* loc_list_from_tree will probably not output these operations for
	     size functions, so assume they will not appear here.  */
	  /* Fall through...  */

	default:
	  gcc_unreachable ();
	}

      /* Now, follow the control flow (except subroutine calls).  */
      switch (l->dw_loc_opc)
	{
	case DW_OP_bra:
	  if (!resolve_args_picking_1 (l->dw_loc_next, frame_offset_, dpi,
				       frame_offsets))
	    return false;
	  /* Fall through. */

	case DW_OP_skip:
	  l = l->dw_loc_oprnd1.v.val_loc;
	  break;

	case DW_OP_stack_value:
	  return true;

	default:
	  l = l->dw_loc_next;
	  break;
	}
    }

  return true;
}

/* Make a DFS over operations reachable through LOC (i.e. follow branch
   operations) in order to resolve the operand of DW_OP_pick operations that
   target DWARF procedure arguments (DPI).  INITIAL_FRAME_OFFSET is the frame
   offset *before* LOC is executed.  Return if all relocations were
   successful.  */

static bool
resolve_args_picking (dw_loc_descr_ref loc, unsigned initial_frame_offset,
		      struct dwarf_procedure_info *dpi)
{
  /* Associate to all visited operations the frame offset *before* evaluating
     this operation.  */
  hash_map<dw_loc_descr_ref, unsigned> frame_offsets;

  return resolve_args_picking_1 (loc, initial_frame_offset, dpi,
				 frame_offsets);
}

/* Try to generate a DWARF procedure that computes the same result as FNDECL.
   Return NULL if it is not possible.  */

static dw_die_ref
function_to_dwarf_procedure (tree fndecl)
{
  struct loc_descr_context ctx;
  struct dwarf_procedure_info dpi;
  dw_die_ref dwarf_proc_die;
  tree tree_body = DECL_SAVED_TREE (fndecl);
  dw_loc_descr_ref loc_body, epilogue;

  tree cursor;
  unsigned i;

  /* Do not generate multiple DWARF procedures for the same function
     declaration.  */
  dwarf_proc_die = lookup_decl_die (fndecl);
  if (dwarf_proc_die != NULL)
    return dwarf_proc_die;

  /* DWARF procedures are available starting with the DWARFv3 standard.  */
  if (dwarf_version < 3 && dwarf_strict)
    return NULL;

  /* We handle only functions for which we still have a body, that return a
     supported type and that takes arguments with supported types.  Note that
     there is no point translating functions that return nothing.  */
  if (tree_body == NULL_TREE
      || DECL_RESULT (fndecl) == NULL_TREE
      || !is_handled_procedure_type (TREE_TYPE (DECL_RESULT (fndecl))))
    return NULL;

  for (cursor = DECL_ARGUMENTS (fndecl);
       cursor != NULL_TREE;
       cursor = TREE_CHAIN (cursor))
    if (!is_handled_procedure_type (TREE_TYPE (cursor)))
      return NULL;

  /* Match only "expr" in: RETURN_EXPR (MODIFY_EXPR (RESULT_DECL, expr)).  */
  if (TREE_CODE (tree_body) != RETURN_EXPR)
    return NULL;
  tree_body = TREE_OPERAND (tree_body, 0);
  if (TREE_CODE (tree_body) != MODIFY_EXPR
      || TREE_OPERAND (tree_body, 0) != DECL_RESULT (fndecl))
    return NULL;
  tree_body = TREE_OPERAND (tree_body, 1);

  /* Try to translate the body expression itself.  Note that this will probably
     cause an infinite recursion if its call graph has a cycle.  This is very
     unlikely for size functions, however, so don't bother with such things at
     the moment.  */
  ctx.context_type = NULL_TREE;
  ctx.base_decl = NULL_TREE;
  ctx.dpi = &dpi;
  ctx.placeholder_arg = false;
  ctx.placeholder_seen = false;
  dpi.fndecl = fndecl;
  dpi.args_count = list_length (DECL_ARGUMENTS (fndecl));
  loc_body = loc_descriptor_from_tree (tree_body, 0, &ctx);
  if (!loc_body)
    return NULL;

  /* After evaluating all operands in "loc_body", we should still have on the
     stack all arguments plus the desired function result (top of the stack).
     Generate code in order to keep only the result in our stack frame.  */
  epilogue = NULL;
  for (i = 0; i < dpi.args_count; ++i)
    {
      dw_loc_descr_ref op_couple = new_loc_descr (DW_OP_swap, 0, 0);
      op_couple->dw_loc_next = new_loc_descr (DW_OP_drop, 0, 0);
      op_couple->dw_loc_next->dw_loc_next = epilogue;
      epilogue = op_couple;
    }
  add_loc_descr (&loc_body, epilogue);
  if (!resolve_args_picking (loc_body, dpi.args_count, &dpi))
    return NULL;

  /* Trailing nops from loc_descriptor_from_tree (if any) cannot be removed
     because they are considered useful.  Now there is an epilogue, they are
     not anymore, so give it another try.   */
  loc_descr_without_nops (loc_body);

  /* fndecl may be used both as a regular DW_TAG_subprogram DIE and as
     a DW_TAG_dwarf_procedure, so we may have a conflict, here.  It's unlikely,
     though, given that size functions do not come from source, so they should
     not have a dedicated DW_TAG_subprogram DIE.  */
  dwarf_proc_die
    = new_dwarf_proc_die (loc_body, fndecl,
			  get_context_die (DECL_CONTEXT (fndecl)));

  /* The called DWARF procedure consumes one stack slot per argument and
     returns one stack slot.  */
  dwarf_proc_stack_usage_map->put (dwarf_proc_die, 1 - dpi.args_count);

  return dwarf_proc_die;
}


/* Generate Dwarf location list representing LOC.
   If WANT_ADDRESS is false, expression computing LOC will be computed
   If WANT_ADDRESS is 1, expression computing address of LOC will be returned
   if WANT_ADDRESS is 2, expression computing address useable in location
     will be returned (i.e. DW_OP_reg can be used
     to refer to register values).

   CONTEXT provides information to customize the location descriptions
   generation.  Its context_type field specifies what type is implicitly
   referenced by DW_OP_push_object_address.  If it is NULL_TREE, this operation
   will not be generated.

   Its DPI field determines whether we are generating a DWARF expression for a
   DWARF procedure, so PARM_DECL references are processed specifically.

   If CONTEXT is NULL, the behavior is the same as if context_type, base_decl
   and dpi fields were null.  */

static dw_loc_list_ref
loc_list_from_tree_1 (tree loc, int want_address,
		      struct loc_descr_context *context)
{
  dw_loc_descr_ref ret = NULL, ret1 = NULL;
  dw_loc_list_ref list_ret = NULL, list_ret1 = NULL;
  int have_address = 0;
  enum dwarf_location_atom op;

  /* ??? Most of the time we do not take proper care for sign/zero
     extending the values properly.  Hopefully this won't be a real
     problem...  */

  if (context != NULL
      && context->base_decl == loc
      && want_address == 0)
    {
      if (dwarf_version >= 3 || !dwarf_strict)
	return new_loc_list (new_loc_descr (DW_OP_push_object_address, 0, 0),
			     NULL, NULL, NULL);
      else
	return NULL;
    }

  switch (TREE_CODE (loc))
    {
    case ERROR_MARK:
      expansion_failed (loc, NULL_RTX, "ERROR_MARK");
      return 0;

    case PLACEHOLDER_EXPR:
      /* This case involves extracting fields from an object to determine the
	 position of other fields. It is supposed to appear only as the first
         operand of COMPONENT_REF nodes and to reference precisely the type
         that the context allows.  */
      if (context != NULL
          && TREE_TYPE (loc) == context->context_type
	  && want_address >= 1)
	{
	  if (dwarf_version >= 3 || !dwarf_strict)
	    {
	      ret = new_loc_descr (DW_OP_push_object_address, 0, 0);
	      have_address = 1;
	      break;
	    }
	  else
	    return NULL;
	}
      /* For DW_TAG_generic_subrange attributes, PLACEHOLDER_EXPR stands for
	 the single argument passed by consumer.  */
      else if (context != NULL
	       && context->placeholder_arg
	       && INTEGRAL_TYPE_P (TREE_TYPE (loc))
	       && want_address == 0)
	{
	  ret = new_loc_descr (DW_OP_pick, 0, 0);
	  ret->frame_offset_rel = 1;
	  context->placeholder_seen = true;
	  break;
	}
      else
	expansion_failed (loc, NULL_RTX,
			  "PLACEHOLDER_EXPR for an unexpected type");
      break;

    case CALL_EXPR:
	{
	  const int nargs = call_expr_nargs (loc);
	  tree callee = get_callee_fndecl (loc);
	  int i;
	  dw_die_ref dwarf_proc;

	  if (callee == NULL_TREE)
	    goto call_expansion_failed;

	  /* We handle only functions that return an integer.  */
	  if (!is_handled_procedure_type (TREE_TYPE (TREE_TYPE (callee))))
	    goto call_expansion_failed;

	  dwarf_proc = function_to_dwarf_procedure (callee);
	  if (dwarf_proc == NULL)
	    goto call_expansion_failed;

	  /* Evaluate arguments right-to-left so that the first argument will
	     be the top-most one on the stack.  */
	  for (i = nargs - 1; i >= 0; --i)
	    {
	      dw_loc_descr_ref loc_descr
	        = loc_descriptor_from_tree (CALL_EXPR_ARG (loc, i), 0,
					    context);

	      if (loc_descr == NULL)
		goto call_expansion_failed;

	      add_loc_descr (&ret, loc_descr);
	    }

	  ret1 = new_loc_descr (DW_OP_call4, 0, 0);
	  ret1->dw_loc_oprnd1.val_class = dw_val_class_die_ref;
	  ret1->dw_loc_oprnd1.v.val_die_ref.die = dwarf_proc;
	  ret1->dw_loc_oprnd1.v.val_die_ref.external = 0;
	  add_loc_descr (&ret, ret1);
	  break;

	call_expansion_failed:
	  expansion_failed (loc, NULL_RTX, "CALL_EXPR");
	  /* There are no opcodes for these operations.  */
	  return 0;
	}

    case PREINCREMENT_EXPR:
    case PREDECREMENT_EXPR:
    case POSTINCREMENT_EXPR:
    case POSTDECREMENT_EXPR:
      expansion_failed (loc, NULL_RTX, "PRE/POST INDCREMENT/DECREMENT");
      /* There are no opcodes for these operations.  */
      return 0;

    case ADDR_EXPR:
      /* If we already want an address, see if there is INDIRECT_REF inside
         e.g. for &this->field.  */
      if (want_address)
	{
	  list_ret = loc_list_for_address_of_addr_expr_of_indirect_ref
		       (loc, want_address == 2, context);
	  if (list_ret)
	    have_address = 1;
	  else if (decl_address_ip_invariant_p (TREE_OPERAND (loc, 0))
	  	   && (ret = cst_pool_loc_descr (loc)))
	    have_address = 1;
	}
        /* Otherwise, process the argument and look for the address.  */
      if (!list_ret && !ret)
        list_ret = loc_list_from_tree_1 (TREE_OPERAND (loc, 0), 1, context);
      else
	{
	  if (want_address)
	    expansion_failed (loc, NULL_RTX, "need address of ADDR_EXPR");
	  return NULL;
	}
      break;

    case VAR_DECL:
      if (DECL_THREAD_LOCAL_P (loc))
	{
	  rtx rtl;
         enum dwarf_location_atom tls_op;
         enum dtprel_bool dtprel = dtprel_false;

	  if (targetm.have_tls)
	    {
	      /* If this is not defined, we have no way to emit the
		 data.  */
	      if (!targetm.asm_out.output_dwarf_dtprel)
		return 0;

	       /* The way DW_OP_GNU_push_tls_address is specified, we
	     	  can only look up addresses of objects in the current
	     	  module.  We used DW_OP_addr as first op, but that's
		  wrong, because DW_OP_addr is relocated by the debug
		  info consumer, while DW_OP_GNU_push_tls_address
		  operand shouldn't be.  */
	      if (DECL_EXTERNAL (loc) && !targetm.binds_local_p (loc))
		return 0;
	      dtprel = dtprel_true;
	      /* We check for DWARF 5 here because gdb did not implement
		 DW_OP_form_tls_address until after 7.12.  */
	      tls_op = (dwarf_version >= 5 ? DW_OP_form_tls_address
			: DW_OP_GNU_push_tls_address);
	    }
	  else
	    {
	      if (!targetm.emutls.debug_form_tls_address
		  || !(dwarf_version >= 3 || !dwarf_strict))
		return 0;
	      /* We stuffed the control variable into the DECL_VALUE_EXPR
		 to signal (via DECL_HAS_VALUE_EXPR_P) that the decl should
		 no longer appear in gimple code.  We used the control
		 variable in specific so that we could pick it up here.  */
	      loc = DECL_VALUE_EXPR (loc);
              tls_op = DW_OP_form_tls_address;
	    }

	  rtl = rtl_for_decl_location (loc);
	  if (rtl == NULL_RTX)
	    return 0;

	  if (!MEM_P (rtl))
	    return 0;
	  rtl = XEXP (rtl, 0);
	  if (! CONSTANT_P (rtl))
	    return 0;

          ret = new_addr_loc_descr (rtl, dtprel);
          ret1 = new_loc_descr (tls_op, 0, 0);
	  add_loc_descr (&ret, ret1);

	  have_address = 1;
	  break;
	}
      /* FALLTHRU */

    case PARM_DECL:
      if (context != NULL && context->dpi != NULL
	  && DECL_CONTEXT (loc) == context->dpi->fndecl)
	{
	  /* We are generating code for a DWARF procedure and we want to access
	     one of its arguments: find the appropriate argument offset and let
	     the resolve_args_picking pass compute the offset that complies
	     with the stack frame size.  */
	  unsigned i = 0;
	  tree cursor;

	  for (cursor = DECL_ARGUMENTS (context->dpi->fndecl);
	       cursor != NULL_TREE && cursor != loc;
	       cursor = TREE_CHAIN (cursor), ++i)
	    ;
	  /* If we are translating a DWARF procedure, all referenced parameters
	     must belong to the current function.  */
	  gcc_assert (cursor != NULL_TREE);

	  ret = new_loc_descr (DW_OP_pick, i, 0);
	  ret->frame_offset_rel = 1;
	  break;
	}
      /* FALLTHRU */

    case RESULT_DECL:
      if (DECL_HAS_VALUE_EXPR_P (loc))
	return loc_list_from_tree_1 (DECL_VALUE_EXPR (loc),
				     want_address, context);
      /* FALLTHRU */

    case FUNCTION_DECL:
      {
	rtx rtl;
	var_loc_list *loc_list = lookup_decl_loc (loc);

	if (loc_list && loc_list->first)
	  {
	    list_ret = dw_loc_list (loc_list, loc, want_address);
	    have_address = want_address != 0;
	    break;
	  }
	rtl = rtl_for_decl_location (loc);
	if (rtl == NULL_RTX)
	  {
	    if (TREE_CODE (loc) != FUNCTION_DECL
		&& early_dwarf
		&& current_function_decl
		&& want_address != 1
		&& ! DECL_IGNORED_P (loc)
		&& (INTEGRAL_TYPE_P (TREE_TYPE (loc))
		    || POINTER_TYPE_P (TREE_TYPE (loc)))
		&& DECL_CONTEXT (loc) == current_function_decl
		&& (GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (loc)))
		    <= DWARF2_ADDR_SIZE))
	      {
		dw_die_ref ref = lookup_decl_die (loc);
		ret = new_loc_descr (DW_OP_GNU_variable_value, 0, 0);
		if (ref)
		  {
		    ret->dw_loc_oprnd1.val_class = dw_val_class_die_ref;
		    ret->dw_loc_oprnd1.v.val_die_ref.die = ref;
		    ret->dw_loc_oprnd1.v.val_die_ref.external = 0;
		  }
		else
		  {
		    ret->dw_loc_oprnd1.val_class = dw_val_class_decl_ref;
		    ret->dw_loc_oprnd1.v.val_decl_ref = loc;
		  }
		break;
	      }
	    expansion_failed (loc, NULL_RTX, "DECL has no RTL");
	    return 0;
	  }
	else if (CONST_INT_P (rtl))
	  {
	    HOST_WIDE_INT val = INTVAL (rtl);
	    if (TYPE_UNSIGNED (TREE_TYPE (loc)))
	      val &= GET_MODE_MASK (DECL_MODE (loc));
	    ret = int_loc_descriptor (val);
	  }
	else if (GET_CODE (rtl) == CONST_STRING)
	  {
	    expansion_failed (loc, NULL_RTX, "CONST_STRING");
	    return 0;
	  }
	else if (CONSTANT_P (rtl) && const_ok_for_output (rtl))
          ret = new_addr_loc_descr (rtl, dtprel_false);
	else
	  {
	    machine_mode mode, mem_mode;

	    /* Certain constructs can only be represented at top-level.  */
	    if (want_address == 2)
	      {
		ret = loc_descriptor (rtl, VOIDmode,
				      VAR_INIT_STATUS_INITIALIZED);
		have_address = 1;
	      }
	    else
	      {
		mode = GET_MODE (rtl);
		mem_mode = VOIDmode;
		if (MEM_P (rtl))
		  {
		    mem_mode = mode;
		    mode = get_address_mode (rtl);
		    rtl = XEXP (rtl, 0);
		    have_address = 1;
		  }
		ret = mem_loc_descriptor (rtl, mode, mem_mode,
					  VAR_INIT_STATUS_INITIALIZED);
	      }
	    if (!ret)
	      expansion_failed (loc, rtl,
				"failed to produce loc descriptor for rtl");
	  }
      }
      break;

    case MEM_REF:
      if (!integer_zerop (TREE_OPERAND (loc, 1)))
	{
	  have_address = 1;
	  goto do_plus;
	}
      /* Fallthru.  */
    case INDIRECT_REF:
      list_ret = loc_list_from_tree_1 (TREE_OPERAND (loc, 0), 0, context);
      have_address = 1;
      break;

    case TARGET_MEM_REF:
    case SSA_NAME:
    case DEBUG_EXPR_DECL:
      return NULL;

    case COMPOUND_EXPR:
      return loc_list_from_tree_1 (TREE_OPERAND (loc, 1), want_address,
				   context);

    CASE_CONVERT:
    case VIEW_CONVERT_EXPR:
    case SAVE_EXPR:
    case MODIFY_EXPR:
    case NON_LVALUE_EXPR:
      return loc_list_from_tree_1 (TREE_OPERAND (loc, 0), want_address,
				   context);

    case COMPONENT_REF:
    case BIT_FIELD_REF:
    case ARRAY_REF:
    case ARRAY_RANGE_REF:
    case REALPART_EXPR:
    case IMAGPART_EXPR:
      {
	tree obj, offset;
	HOST_WIDE_INT bitsize, bitpos, bytepos;
	machine_mode mode;
	int unsignedp, reversep, volatilep = 0;

	obj = get_inner_reference (loc, &bitsize, &bitpos, &offset, &mode,
				   &unsignedp, &reversep, &volatilep);

	gcc_assert (obj != loc);

	list_ret = loc_list_from_tree_1 (obj,
					 want_address == 2
					 && !bitpos && !offset ? 2 : 1,
					 context);
	/* TODO: We can extract value of the small expression via shifting even
	   for nonzero bitpos.  */
	if (list_ret == 0)
	  return 0;
	if (bitpos % BITS_PER_UNIT != 0 || bitsize % BITS_PER_UNIT != 0)
	  {
	    expansion_failed (loc, NULL_RTX,
			      "bitfield access");
	    return 0;
	  }

	if (offset != NULL_TREE)
	  {
	    /* Variable offset.  */
	    list_ret1 = loc_list_from_tree_1 (offset, 0, context);
	    if (list_ret1 == 0)
	      return 0;
	    add_loc_list (&list_ret, list_ret1);
	    if (!list_ret)
	      return 0;
	    add_loc_descr_to_each (list_ret, new_loc_descr (DW_OP_plus, 0, 0));
	  }

	bytepos = bitpos / BITS_PER_UNIT;
	if (bytepos > 0)
	  add_loc_descr_to_each (list_ret, new_loc_descr (DW_OP_plus_uconst, bytepos, 0));
	else if (bytepos < 0)
	  loc_list_plus_const (list_ret, bytepos);

	have_address = 1;
	break;
      }

    case INTEGER_CST:
      if ((want_address || !tree_fits_shwi_p (loc))
	  && (ret = cst_pool_loc_descr (loc)))
	have_address = 1;
      else if (want_address == 2
	       && tree_fits_shwi_p (loc)
	       && (ret = address_of_int_loc_descriptor
	       		   (int_size_in_bytes (TREE_TYPE (loc)),
	       		    tree_to_shwi (loc))))
	have_address = 1;
      else if (tree_fits_shwi_p (loc))
	ret = int_loc_descriptor (tree_to_shwi (loc));
      else if (tree_fits_uhwi_p (loc))
	ret = uint_loc_descriptor (tree_to_uhwi (loc));
      else
	{
	  expansion_failed (loc, NULL_RTX,
			    "Integer operand is not host integer");
	  return 0;
	}
      break;

    case CONSTRUCTOR:
    case REAL_CST:
    case STRING_CST:
    case COMPLEX_CST:
      if ((ret = cst_pool_loc_descr (loc)))
	have_address = 1;
      else if (TREE_CODE (loc) == CONSTRUCTOR)
	{
	  tree type = TREE_TYPE (loc);
	  unsigned HOST_WIDE_INT size = int_size_in_bytes (type);
	  unsigned HOST_WIDE_INT offset = 0;
	  unsigned HOST_WIDE_INT cnt;
	  constructor_elt *ce;

	  if (TREE_CODE (type) == RECORD_TYPE)
	    {
	      /* This is very limited, but it's enough to output
		 pointers to member functions, as long as the
		 referenced function is defined in the current
		 translation unit.  */
	      FOR_EACH_VEC_SAFE_ELT (CONSTRUCTOR_ELTS (loc), cnt, ce)
		{
		  tree val = ce->value;

		  tree field = ce->index;

		  if (val)
		    STRIP_NOPS (val);

		  if (!field || DECL_BIT_FIELD (field))
		    {
		      expansion_failed (loc, NULL_RTX,
					"bitfield in record type constructor");
		      size = offset = (unsigned HOST_WIDE_INT)-1;
		      ret = NULL;
		      break;
		    }

		  HOST_WIDE_INT fieldsize = tree_to_shwi (DECL_SIZE_UNIT (field));
		  unsigned HOST_WIDE_INT pos = int_byte_position (field);
		  gcc_assert (pos + fieldsize <= size);
		  if (pos < offset)
		    {
		      expansion_failed (loc, NULL_RTX,
					"out-of-order fields in record constructor");
		      size = offset = (unsigned HOST_WIDE_INT)-1;
		      ret = NULL;
		      break;
		    }
		  if (pos > offset)
		    {
		      ret1 = new_loc_descr (DW_OP_piece, pos - offset, 0);
		      add_loc_descr (&ret, ret1);
		      offset = pos;
		    }
		  if (val && fieldsize != 0)
		    {
		      ret1 = loc_descriptor_from_tree (val, want_address, context);
		      if (!ret1)
			{
			  expansion_failed (loc, NULL_RTX,
					    "unsupported expression in field");
			  size = offset = (unsigned HOST_WIDE_INT)-1;
			  ret = NULL;
			  break;
			}
		      add_loc_descr (&ret, ret1);
		    }
		  if (fieldsize)
		    {
		      ret1 = new_loc_descr (DW_OP_piece, fieldsize, 0);
		      add_loc_descr (&ret, ret1);
		      offset = pos + fieldsize;
		    }
		}

	      if (offset != size)
		{
		  ret1 = new_loc_descr (DW_OP_piece, size - offset, 0);
		  add_loc_descr (&ret, ret1);
		  offset = size;
		}

	      have_address = !!want_address;
	    }
	  else
	    expansion_failed (loc, NULL_RTX,
			      "constructor of non-record type");
	}
      else
      /* We can construct small constants here using int_loc_descriptor.  */
	expansion_failed (loc, NULL_RTX,
			  "constructor or constant not in constant pool");
      break;

    case TRUTH_AND_EXPR:
    case TRUTH_ANDIF_EXPR:
    case BIT_AND_EXPR:
      op = DW_OP_and;
      goto do_binop;

    case TRUTH_XOR_EXPR:
    case BIT_XOR_EXPR:
      op = DW_OP_xor;
      goto do_binop;

    case TRUTH_OR_EXPR:
    case TRUTH_ORIF_EXPR:
    case BIT_IOR_EXPR:
      op = DW_OP_or;
      goto do_binop;

    case FLOOR_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case TRUNC_DIV_EXPR:
    case EXACT_DIV_EXPR:
      if (TYPE_UNSIGNED (TREE_TYPE (loc)))
	return 0;
      op = DW_OP_div;
      goto do_binop;

    case MINUS_EXPR:
      op = DW_OP_minus;
      goto do_binop;

    case FLOOR_MOD_EXPR:
    case CEIL_MOD_EXPR:
    case ROUND_MOD_EXPR:
    case TRUNC_MOD_EXPR:
      if (TYPE_UNSIGNED (TREE_TYPE (loc)))
	{
	  op = DW_OP_mod;
	  goto do_binop;
	}
      list_ret = loc_list_from_tree_1 (TREE_OPERAND (loc, 0), 0, context);
      list_ret1 = loc_list_from_tree_1 (TREE_OPERAND (loc, 1), 0, context);
      if (list_ret == 0 || list_ret1 == 0)
	return 0;

      add_loc_list (&list_ret, list_ret1);
      if (list_ret == 0)
	return 0;
      add_loc_descr_to_each (list_ret, new_loc_descr (DW_OP_over, 0, 0));
      add_loc_descr_to_each (list_ret, new_loc_descr (DW_OP_over, 0, 0));
      add_loc_descr_to_each (list_ret, new_loc_descr (DW_OP_div, 0, 0));
      add_loc_descr_to_each (list_ret, new_loc_descr (DW_OP_mul, 0, 0));
      add_loc_descr_to_each (list_ret, new_loc_descr (DW_OP_minus, 0, 0));
      break;

    case MULT_EXPR:
      op = DW_OP_mul;
      goto do_binop;

    case LSHIFT_EXPR:
      op = DW_OP_shl;
      goto do_binop;

    case RSHIFT_EXPR:
      op = (TYPE_UNSIGNED (TREE_TYPE (loc)) ? DW_OP_shr : DW_OP_shra);
      goto do_binop;

    case POINTER_PLUS_EXPR:
    case PLUS_EXPR:
    do_plus:
      if (tree_fits_shwi_p (TREE_OPERAND (loc, 1)))
	{
	  /* Big unsigned numbers can fit in HOST_WIDE_INT but it may be
	     smarter to encode their opposite.  The DW_OP_plus_uconst operation
	     takes 1 + X bytes, X being the size of the ULEB128 addend.  On the
	     other hand, a "<push literal>; DW_OP_minus" pattern takes 1 + Y
	     bytes, Y being the size of the operation that pushes the opposite
	     of the addend.  So let's choose the smallest representation.  */
	  const tree tree_addend = TREE_OPERAND (loc, 1);
	  offset_int wi_addend;
	  HOST_WIDE_INT shwi_addend;
	  dw_loc_descr_ref loc_naddend;

	  list_ret = loc_list_from_tree_1 (TREE_OPERAND (loc, 0), 0, context);
	  if (list_ret == 0)
	    return 0;

	  /* Try to get the literal to push.  It is the opposite of the addend,
	     so as we rely on wrapping during DWARF evaluation, first decode
	     the literal as a "DWARF-sized" signed number.  */
	  wi_addend = wi::to_offset (tree_addend);
	  wi_addend = wi::sext (wi_addend, DWARF2_ADDR_SIZE * 8);
	  shwi_addend = wi_addend.to_shwi ();
	  loc_naddend = (shwi_addend != INTTYPE_MINIMUM (HOST_WIDE_INT))
			? int_loc_descriptor (-shwi_addend)
			: NULL;

	  if (loc_naddend != NULL
	      && ((unsigned) size_of_uleb128 (shwi_addend)
	          > size_of_loc_descr (loc_naddend)))
	    {
	      add_loc_descr_to_each (list_ret, loc_naddend);
	      add_loc_descr_to_each (list_ret,
				     new_loc_descr (DW_OP_minus, 0, 0));
	    }
	  else
	    {
	      for (dw_loc_descr_ref loc_cur = loc_naddend; loc_cur != NULL; )
		{
		  loc_naddend = loc_cur;
		  loc_cur = loc_cur->dw_loc_next;
		  ggc_free (loc_naddend);
		}
	      loc_list_plus_const (list_ret, wi_addend.to_shwi ());
	    }
	  break;
	}

      op = DW_OP_plus;
      goto do_binop;

    case LE_EXPR:
      op = DW_OP_le;
      goto do_comp_binop;

    case GE_EXPR:
      op = DW_OP_ge;
      goto do_comp_binop;

    case LT_EXPR:
      op = DW_OP_lt;
      goto do_comp_binop;

    case GT_EXPR:
      op = DW_OP_gt;
      goto do_comp_binop;

    do_comp_binop:
      if (TYPE_UNSIGNED (TREE_TYPE (TREE_OPERAND (loc, 0))))
	{
	  list_ret = loc_list_from_tree (TREE_OPERAND (loc, 0), 0, context);
	  list_ret1 = loc_list_from_tree (TREE_OPERAND (loc, 1), 0, context);
	  list_ret = loc_list_from_uint_comparison (list_ret, list_ret1,
						    TREE_CODE (loc));
	  break;
	}
      else
	goto do_binop;

    case EQ_EXPR:
      op = DW_OP_eq;
      goto do_binop;

    case NE_EXPR:
      op = DW_OP_ne;
      goto do_binop;

    do_binop:
      list_ret = loc_list_from_tree_1 (TREE_OPERAND (loc, 0), 0, context);
      list_ret1 = loc_list_from_tree_1 (TREE_OPERAND (loc, 1), 0, context);
      if (list_ret == 0 || list_ret1 == 0)
	return 0;

      add_loc_list (&list_ret, list_ret1);
      if (list_ret == 0)
	return 0;
      add_loc_descr_to_each (list_ret, new_loc_descr (op, 0, 0));
      break;

    case TRUTH_NOT_EXPR:
    case BIT_NOT_EXPR:
      op = DW_OP_not;
      goto do_unop;

    case ABS_EXPR:
      op = DW_OP_abs;
      goto do_unop;

    case NEGATE_EXPR:
      op = DW_OP_neg;
      goto do_unop;

    do_unop:
      list_ret = loc_list_from_tree_1 (TREE_OPERAND (loc, 0), 0, context);
      if (list_ret == 0)
	return 0;

      add_loc_descr_to_each (list_ret, new_loc_descr (op, 0, 0));
      break;

    case MIN_EXPR:
    case MAX_EXPR:
      {
	const enum tree_code code =
	  TREE_CODE (loc) == MIN_EXPR ? GT_EXPR : LT_EXPR;

	loc = build3 (COND_EXPR, TREE_TYPE (loc),
		      build2 (code, integer_type_node,
			      TREE_OPERAND (loc, 0), TREE_OPERAND (loc, 1)),
		      TREE_OPERAND (loc, 1), TREE_OPERAND (loc, 0));
      }

      /* fall through */

    case COND_EXPR:
      {
	dw_loc_descr_ref lhs
	  = loc_descriptor_from_tree (TREE_OPERAND (loc, 1), 0, context);
	dw_loc_list_ref rhs
	  = loc_list_from_tree_1 (TREE_OPERAND (loc, 2), 0, context);
	dw_loc_descr_ref bra_node, jump_node, tmp;

	list_ret = loc_list_from_tree_1 (TREE_OPERAND (loc, 0), 0, context);
	if (list_ret == 0 || lhs == 0 || rhs == 0)
	  return 0;

	bra_node = new_loc_descr (DW_OP_bra, 0, 0);
	add_loc_descr_to_each (list_ret, bra_node);

	add_loc_list (&list_ret, rhs);
	jump_node = new_loc_descr (DW_OP_skip, 0, 0);
	add_loc_descr_to_each (list_ret, jump_node);

	add_loc_descr_to_each (list_ret, lhs);
	bra_node->dw_loc_oprnd1.val_class = dw_val_class_loc;
	bra_node->dw_loc_oprnd1.v.val_loc = lhs;

	/* ??? Need a node to point the skip at.  Use a nop.  */
	tmp = new_loc_descr (DW_OP_nop, 0, 0);
	add_loc_descr_to_each (list_ret, tmp);
	jump_node->dw_loc_oprnd1.val_class = dw_val_class_loc;
	jump_node->dw_loc_oprnd1.v.val_loc = tmp;
      }
      break;

    case FIX_TRUNC_EXPR:
      return 0;

    default:
      /* Leave front-end specific codes as simply unknown.  This comes
	 up, for instance, with the C STMT_EXPR.  */
      if ((unsigned int) TREE_CODE (loc)
	  >= (unsigned int) LAST_AND_UNUSED_TREE_CODE)
	{
	  expansion_failed (loc, NULL_RTX,
			    "language specific tree node");
	  return 0;
	}

      /* Otherwise this is a generic code; we should just lists all of
	 these explicitly.  We forgot one.  */
      if (flag_checking)
	gcc_unreachable ();

      /* In a release build, we want to degrade gracefully: better to
	 generate incomplete debugging information than to crash.  */
      return NULL;
    }

  if (!ret && !list_ret)
    return 0;

  if (want_address == 2 && !have_address
      && (dwarf_version >= 4 || !dwarf_strict))
    {
      if (int_size_in_bytes (TREE_TYPE (loc)) > DWARF2_ADDR_SIZE)
	{
	  expansion_failed (loc, NULL_RTX,
			    "DWARF address size mismatch");
	  return 0;
	}
      if (ret)
	add_loc_descr (&ret, new_loc_descr (DW_OP_stack_value, 0, 0));
      else
	add_loc_descr_to_each (list_ret,
			       new_loc_descr (DW_OP_stack_value, 0, 0));
      have_address = 1;
    }
  /* Show if we can't fill the request for an address.  */
  if (want_address && !have_address)
    {
      expansion_failed (loc, NULL_RTX,
			"Want address and only have value");
      return 0;
    }

  gcc_assert (!ret || !list_ret);

  /* If we've got an address and don't want one, dereference.  */
  if (!want_address && have_address)
    {
      HOST_WIDE_INT size = int_size_in_bytes (TREE_TYPE (loc));

      if (size > DWARF2_ADDR_SIZE || size == -1)
	{
	  expansion_failed (loc, NULL_RTX,
			    "DWARF address size mismatch");
	  return 0;
	}
      else if (size == DWARF2_ADDR_SIZE)
	op = DW_OP_deref;
      else
	op = DW_OP_deref_size;

      if (ret)
	add_loc_descr (&ret, new_loc_descr (op, size, 0));
      else
	add_loc_descr_to_each (list_ret, new_loc_descr (op, size, 0));
    }
  if (ret)
    list_ret = new_loc_list (ret, NULL, NULL, NULL);

  return list_ret;
}

/* Likewise, but strip useless DW_OP_nop operations in the resulting
   expressions.  */

static dw_loc_list_ref
loc_list_from_tree (tree loc, int want_address,
		    struct loc_descr_context *context)
{
  dw_loc_list_ref result = loc_list_from_tree_1 (loc, want_address, context);

  for (dw_loc_list_ref loc_cur = result;
       loc_cur != NULL; loc_cur = loc_cur->dw_loc_next)
    loc_descr_without_nops (loc_cur->expr);
  return result;
}

/* Same as above but return only single location expression.  */
static dw_loc_descr_ref
loc_descriptor_from_tree (tree loc, int want_address,
			  struct loc_descr_context *context)
{
  dw_loc_list_ref ret = loc_list_from_tree (loc, want_address, context);
  if (!ret)
    return NULL;
  if (ret->dw_loc_next)
    {
      expansion_failed (loc, NULL_RTX,
			"Location list where only loc descriptor needed");
      return NULL;
    }
  return ret->expr;
}

/* Given a value, round it up to the lowest multiple of `boundary'
   which is not less than the value itself.  */

static inline HOST_WIDE_INT
ceiling (HOST_WIDE_INT value, unsigned int boundary)
{
  return (((value + boundary - 1) / boundary) * boundary);
}

/* Given a pointer to what is assumed to be a FIELD_DECL node, return a
   pointer to the declared type for the relevant field variable, or return
   `integer_type_node' if the given node turns out to be an
   ERROR_MARK node.  */

static inline tree
field_type (const_tree decl)
{
  tree type;

  if (TREE_CODE (decl) == ERROR_MARK)
    return integer_type_node;

  type = DECL_BIT_FIELD_TYPE (decl);
  if (type == NULL_TREE)
    type = TREE_TYPE (decl);

  return type;
}

/* Given a pointer to a tree node, return the alignment in bits for
   it, or else return BITS_PER_WORD if the node actually turns out to
   be an ERROR_MARK node.  */

static inline unsigned
simple_type_align_in_bits (const_tree type)
{
  return (TREE_CODE (type) != ERROR_MARK) ? TYPE_ALIGN (type) : BITS_PER_WORD;
}

static inline unsigned
simple_decl_align_in_bits (const_tree decl)
{
  return (TREE_CODE (decl) != ERROR_MARK) ? DECL_ALIGN (decl) : BITS_PER_WORD;
}

/* Return the result of rounding T up to ALIGN.  */

static inline offset_int
round_up_to_align (const offset_int &t, unsigned int align)
{
  return wi::udiv_trunc (t + align - 1, align) * align;
}

/* Compute the size of TYPE in bytes.  If possible, return NULL and store the
   size as an integer constant in CST_SIZE.  Otherwise, if possible, return a
   DWARF expression that computes the size.  Return NULL and set CST_SIZE to -1
   if we fail to return the size in one of these two forms.  */

static dw_loc_descr_ref
type_byte_size (const_tree type, HOST_WIDE_INT *cst_size)
{
  tree tree_size;
  struct loc_descr_context ctx;

  /* Return a constant integer in priority, if possible.  */
  *cst_size = int_size_in_bytes (type);
  if (*cst_size != -1)
    return NULL;

  ctx.context_type = const_cast<tree> (type);
  ctx.base_decl = NULL_TREE;
  ctx.dpi = NULL;
  ctx.placeholder_arg = false;
  ctx.placeholder_seen = false;

  type = TYPE_MAIN_VARIANT (type);
  tree_size = TYPE_SIZE_UNIT (type);
  return ((tree_size != NULL_TREE)
	  ? loc_descriptor_from_tree (tree_size, 0, &ctx)
	  : NULL);
}

/* Helper structure for RECORD_TYPE processing.  */
struct vlr_context
{
  /* Root RECORD_TYPE.  It is needed to generate data member location
     descriptions in variable-length records (VLR), but also to cope with
     variants, which are composed of nested structures multiplexed with
     QUAL_UNION_TYPE nodes.  Each time such a structure is passed to a
     function processing a FIELD_DECL, it is required to be non null.  */
  tree struct_type;
  /* When generating a variant part in a RECORD_TYPE (i.e. a nested
     QUAL_UNION_TYPE), this holds an expression that computes the offset for
     this variant part as part of the root record (in storage units).  For
     regular records, it must be NULL_TREE.  */
  tree variant_part_offset;
};

/* Given a pointer to a FIELD_DECL, compute the byte offset of the lowest
   addressed byte of the "containing object" for the given FIELD_DECL. If
   possible, return a native constant through CST_OFFSET (in which case NULL is
   returned); otherwise return a DWARF expression that computes the offset.

   Set *CST_OFFSET to 0 and return NULL if we are unable to determine what
   that offset is, either because the argument turns out to be a pointer to an
   ERROR_MARK node, or because the offset expression is too complex for us.

   CTX is required: see the comment for VLR_CONTEXT.  */

static dw_loc_descr_ref
field_byte_offset (const_tree decl, struct vlr_context *ctx,
		   HOST_WIDE_INT *cst_offset)
{
  tree tree_result;
  dw_loc_list_ref loc_result;

  *cst_offset = 0;

  if (TREE_CODE (decl) == ERROR_MARK)
    return NULL;
  else
    gcc_assert (TREE_CODE (decl) == FIELD_DECL);

  /* We cannot handle variable bit offsets at the moment, so abort if it's the
     case.  */
  if (TREE_CODE (DECL_FIELD_BIT_OFFSET (decl)) != INTEGER_CST)
    return NULL;

#ifdef PCC_BITFIELD_TYPE_MATTERS
  /* We used to handle only constant offsets in all cases.  Now, we handle
     properly dynamic byte offsets only when PCC bitfield type doesn't
     matter.  */
  if (PCC_BITFIELD_TYPE_MATTERS
      && TREE_CODE (DECL_FIELD_OFFSET (decl)) == INTEGER_CST)
    {
      offset_int object_offset_in_bits;
      offset_int object_offset_in_bytes;
      offset_int bitpos_int;
      tree type;
      tree field_size_tree;
      offset_int deepest_bitpos;
      offset_int field_size_in_bits;
      unsigned int type_align_in_bits;
      unsigned int decl_align_in_bits;
      offset_int type_size_in_bits;

      bitpos_int = wi::to_offset (bit_position (decl));
      type = field_type (decl);
      type_size_in_bits = offset_int_type_size_in_bits (type);
      type_align_in_bits = simple_type_align_in_bits (type);

      field_size_tree = DECL_SIZE (decl);

      /* The size could be unspecified if there was an error, or for
	 a flexible array member.  */
      if (!field_size_tree)
	field_size_tree = bitsize_zero_node;

      /* If the size of the field is not constant, use the type size.  */
      if (TREE_CODE (field_size_tree) == INTEGER_CST)
	field_size_in_bits = wi::to_offset (field_size_tree);
      else
	field_size_in_bits = type_size_in_bits;

      decl_align_in_bits = simple_decl_align_in_bits (decl);

      /* The GCC front-end doesn't make any attempt to keep track of the
	 starting bit offset (relative to the start of the containing
	 structure type) of the hypothetical "containing object" for a
	 bit-field.  Thus, when computing the byte offset value for the
	 start of the "containing object" of a bit-field, we must deduce
	 this information on our own. This can be rather tricky to do in
	 some cases.  For example, handling the following structure type
	 definition when compiling for an i386/i486 target (which only
	 aligns long long's to 32-bit boundaries) can be very tricky:

	 struct S { int field1; long long field2:31; };

	 Fortunately, there is a simple rule-of-thumb which can be used
	 in such cases.  When compiling for an i386/i486, GCC will
	 allocate 8 bytes for the structure shown above.  It decides to
	 do this based upon one simple rule for bit-field allocation.
	 GCC allocates each "containing object" for each bit-field at
	 the first (i.e. lowest addressed) legitimate alignment boundary
	 (based upon the required minimum alignment for the declared
	 type of the field) which it can possibly use, subject to the
	 condition that there is still enough available space remaining
	 in the containing object (when allocated at the selected point)
	 to fully accommodate all of the bits of the bit-field itself.

	 This simple rule makes it obvious why GCC allocates 8 bytes for
	 each object of the structure type shown above.  When looking
	 for a place to allocate the "containing object" for `field2',
	 the compiler simply tries to allocate a 64-bit "containing
	 object" at each successive 32-bit boundary (starting at zero)
	 until it finds a place to allocate that 64- bit field such that
	 at least 31 contiguous (and previously unallocated) bits remain
	 within that selected 64 bit field.  (As it turns out, for the
	 example above, the compiler finds it is OK to allocate the
	 "containing object" 64-bit field at bit-offset zero within the
	 structure type.)

	 Here we attempt to work backwards from the limited set of facts
	 we're given, and we try to deduce from those facts, where GCC
	 must have believed that the containing object started (within
	 the structure type). The value we deduce is then used (by the
	 callers of this routine) to generate DW_AT_location and
	 DW_AT_bit_offset attributes for fields (both bit-fields and, in
	 the case of DW_AT_location, regular fields as well).  */

      /* Figure out the bit-distance from the start of the structure to
	 the "deepest" bit of the bit-field.  */
      deepest_bitpos = bitpos_int + field_size_in_bits;

      /* This is the tricky part.  Use some fancy footwork to deduce
	 where the lowest addressed bit of the containing object must
	 be.  */
      object_offset_in_bits = deepest_bitpos - type_size_in_bits;

      /* Round up to type_align by default.  This works best for
	 bitfields.  */
      object_offset_in_bits
	= round_up_to_align (object_offset_in_bits, type_align_in_bits);

      if (wi::gtu_p (object_offset_in_bits, bitpos_int))
	{
	  object_offset_in_bits = deepest_bitpos - type_size_in_bits;

	  /* Round up to decl_align instead.  */
	  object_offset_in_bits
	    = round_up_to_align (object_offset_in_bits, decl_align_in_bits);
	}

      object_offset_in_bytes
	= wi::lrshift (object_offset_in_bits, LOG2_BITS_PER_UNIT);
      if (ctx->variant_part_offset == NULL_TREE)
	{
	  *cst_offset = object_offset_in_bytes.to_shwi ();
	  return NULL;
	}
      tree_result = wide_int_to_tree (sizetype, object_offset_in_bytes);
    }
  else
#endif /* PCC_BITFIELD_TYPE_MATTERS */
    tree_result = byte_position (decl);

  if (ctx->variant_part_offset != NULL_TREE)
    tree_result = fold_build2 (PLUS_EXPR, TREE_TYPE (tree_result),
			       ctx->variant_part_offset, tree_result);

  /* If the byte offset is a constant, it's simplier to handle a native
     constant rather than a DWARF expression.  */
  if (TREE_CODE (tree_result) == INTEGER_CST)
    {
      *cst_offset = wi::to_offset (tree_result).to_shwi ();
      return NULL;
    }
  struct loc_descr_context loc_ctx = {
    ctx->struct_type, /* context_type */
    NULL_TREE,	      /* base_decl */
    NULL,	      /* dpi */
    false,	      /* placeholder_arg */
    false	      /* placeholder_seen */
  };
  loc_result = loc_list_from_tree (tree_result, 0, &loc_ctx);

  /* We want a DWARF expression: abort if we only have a location list with
     multiple elements.  */
  if (!loc_result || !single_element_loc_list_p (loc_result))
    return NULL;
  else
    return loc_result->expr;
}

/* The following routines define various Dwarf attributes and any data
   associated with them.  */

/* Add a location description attribute value to a DIE.

   This emits location attributes suitable for whole variables and
   whole parameters.  Note that the location attributes for struct fields are
   generated by the routine `data_member_location_attribute' below.  */

static inline void
add_AT_location_description (dw_die_ref die, enum dwarf_attribute attr_kind,
			     dw_loc_list_ref descr)
{
  if (descr == 0)
    return;
  if (single_element_loc_list_p (descr))
    add_AT_loc (die, attr_kind, descr->expr);
  else
    add_AT_loc_list (die, attr_kind, descr);
}

/* Add DW_AT_accessibility attribute to DIE if needed.  */

static void
add_accessibility_attribute (dw_die_ref die, tree decl)
{
  /* In DWARF3+ the default is DW_ACCESS_private only in DW_TAG_class_type
     children, otherwise the default is DW_ACCESS_public.  In DWARF2
     the default has always been DW_ACCESS_public.  */
  if (TREE_PROTECTED (decl))
    add_AT_unsigned (die, DW_AT_accessibility, DW_ACCESS_protected);
  else if (TREE_PRIVATE (decl))
    {
      if (dwarf_version == 2
	  || die->die_parent == NULL
	  || die->die_parent->die_tag != DW_TAG_class_type)
	add_AT_unsigned (die, DW_AT_accessibility, DW_ACCESS_private);
    }
  else if (dwarf_version > 2
	   && die->die_parent
	   && die->die_parent->die_tag == DW_TAG_class_type)
    add_AT_unsigned (die, DW_AT_accessibility, DW_ACCESS_public);
}

/* Attach the specialized form of location attribute used for data members of
   struct and union types.  In the special case of a FIELD_DECL node which
   represents a bit-field, the "offset" part of this special location
   descriptor must indicate the distance in bytes from the lowest-addressed
   byte of the containing struct or union type to the lowest-addressed byte of
   the "containing object" for the bit-field.  (See the `field_byte_offset'
   function above).

   For any given bit-field, the "containing object" is a hypothetical object
   (of some integral or enum type) within which the given bit-field lives.  The
   type of this hypothetical "containing object" is always the same as the
   declared type of the individual bit-field itself (for GCC anyway... the
   DWARF spec doesn't actually mandate this).  Note that it is the size (in
   bytes) of the hypothetical "containing object" which will be given in the
   DW_AT_byte_size attribute for this bit-field.  (See the
   `byte_size_attribute' function below.)  It is also used when calculating the
   value of the DW_AT_bit_offset attribute.  (See the `bit_offset_attribute'
   function below.)

   CTX is required: see the comment for VLR_CONTEXT.  */

static void
add_data_member_location_attribute (dw_die_ref die,
				    tree decl,
				    struct vlr_context *ctx)
{
  HOST_WIDE_INT offset;
  dw_loc_descr_ref loc_descr = 0;

  if (TREE_CODE (decl) == TREE_BINFO)
    {
      /* We're working on the TAG_inheritance for a base class.  */
      if (BINFO_VIRTUAL_P (decl) && is_cxx ())
	{
	  /* For C++ virtual bases we can't just use BINFO_OFFSET, as they
	     aren't at a fixed offset from all (sub)objects of the same
	     type.  We need to extract the appropriate offset from our
	     vtable.  The following dwarf expression means

	       BaseAddr = ObAddr + *((*ObAddr) - Offset)

	     This is specific to the V3 ABI, of course.  */

	  dw_loc_descr_ref tmp;

	  /* Make a copy of the object address.  */
	  tmp = new_loc_descr (DW_OP_dup, 0, 0);
	  add_loc_descr (&loc_descr, tmp);

	  /* Extract the vtable address.  */
	  tmp = new_loc_descr (DW_OP_deref, 0, 0);
	  add_loc_descr (&loc_descr, tmp);

	  /* Calculate the address of the offset.  */
	  offset = tree_to_shwi (BINFO_VPTR_FIELD (decl));
	  gcc_assert (offset < 0);

	  tmp = int_loc_descriptor (-offset);
	  add_loc_descr (&loc_descr, tmp);
	  tmp = new_loc_descr (DW_OP_minus, 0, 0);
	  add_loc_descr (&loc_descr, tmp);

	  /* Extract the offset.  */
	  tmp = new_loc_descr (DW_OP_deref, 0, 0);
	  add_loc_descr (&loc_descr, tmp);

	  /* Add it to the object address.  */
	  tmp = new_loc_descr (DW_OP_plus, 0, 0);
	  add_loc_descr (&loc_descr, tmp);
	}
      else
	offset = tree_to_shwi (BINFO_OFFSET (decl));
    }
  else
    {
      loc_descr = field_byte_offset (decl, ctx, &offset);

      /* If loc_descr is available then we know the field offset is dynamic.
	 However, GDB does not handle dynamic field offsets very well at the
	 moment.  */
      if (loc_descr != NULL && gnat_encodings != DWARF_GNAT_ENCODINGS_MINIMAL)
	{
	  loc_descr = NULL;
	  offset = 0;
	}

      /* Data member location evalutation starts with the base address on the
	 stack.  Compute the field offset and add it to this base address.  */
      else if (loc_descr != NULL)
	add_loc_descr (&loc_descr, new_loc_descr (DW_OP_plus, 0, 0));
    }

  if (! loc_descr)
    {
      /* While DW_AT_data_bit_offset has been added already in DWARF4,
	 e.g. GDB only added support to it in November 2016.  For DWARF5
	 we need newer debug info consumers anyway.  We might change this
	 to dwarf_version >= 4 once most consumers catched up.  */
      if (dwarf_version >= 5
	  && TREE_CODE (decl) == FIELD_DECL
	  && DECL_BIT_FIELD_TYPE (decl))
	{
	  tree off = bit_position (decl);
	  if (tree_fits_uhwi_p (off) && get_AT (die, DW_AT_bit_size))
	    {
	      remove_AT (die, DW_AT_byte_size);
	      remove_AT (die, DW_AT_bit_offset);
	      add_AT_unsigned (die, DW_AT_data_bit_offset, tree_to_uhwi (off));
	      return;
	    }
	}
      if (dwarf_version > 2)
	{
	  /* Don't need to output a location expression, just the constant. */
	  if (offset < 0)
	    add_AT_int (die, DW_AT_data_member_location, offset);
	  else
	    add_AT_unsigned (die, DW_AT_data_member_location, offset);
	  return;
	}
      else
	{
	  enum dwarf_location_atom op;

	  /* The DWARF2 standard says that we should assume that the structure
	     address is already on the stack, so we can specify a structure
	     field address by using DW_OP_plus_uconst.  */
	  op = DW_OP_plus_uconst;
	  loc_descr = new_loc_descr (op, offset, 0);
	}
    }

  add_AT_loc (die, DW_AT_data_member_location, loc_descr);
}

/* Writes integer values to dw_vec_const array.  */

static void
insert_int (HOST_WIDE_INT val, unsigned int size, unsigned char *dest)
{
  while (size != 0)
    {
      *dest++ = val & 0xff;
      val >>= 8;
      --size;
    }
}

/* Reads integers from dw_vec_const array.  Inverse of insert_int.  */

static HOST_WIDE_INT
extract_int (const unsigned char *src, unsigned int size)
{
  HOST_WIDE_INT val = 0;

  src += size;
  while (size != 0)
    {
      val <<= 8;
      val |= *--src & 0xff;
      --size;
    }
  return val;
}

/* Writes wide_int values to dw_vec_const array.  */

static void
insert_wide_int (const wide_int &val, unsigned char *dest, int elt_size)
{
  int i;

  if (elt_size <= HOST_BITS_PER_WIDE_INT/BITS_PER_UNIT)
    {
      insert_int ((HOST_WIDE_INT) val.elt (0), elt_size, dest);
      return;
    }

  /* We'd have to extend this code to support odd sizes.  */
  gcc_assert (elt_size % (HOST_BITS_PER_WIDE_INT / BITS_PER_UNIT) == 0);

  int n = elt_size / (HOST_BITS_PER_WIDE_INT / BITS_PER_UNIT);

  if (WORDS_BIG_ENDIAN)
    for (i = n - 1; i >= 0; i--)
      {
	insert_int ((HOST_WIDE_INT) val.elt (i), sizeof (HOST_WIDE_INT), dest);
	dest += sizeof (HOST_WIDE_INT);
      }
  else
    for (i = 0; i < n; i++)
      {
	insert_int ((HOST_WIDE_INT) val.elt (i), sizeof (HOST_WIDE_INT), dest);
	dest += sizeof (HOST_WIDE_INT);
      }
}

/* Writes floating point values to dw_vec_const array.  */

static void
insert_float (const_rtx rtl, unsigned char *array)
{
  long val[4];
  int i;

  real_to_target (val, CONST_DOUBLE_REAL_VALUE (rtl), GET_MODE (rtl));

  /* real_to_target puts 32-bit pieces in each long.  Pack them.  */
  for (i = 0; i < GET_MODE_SIZE (GET_MODE (rtl)) / 4; i++)
    {
      insert_int (val[i], 4, array);
      array += 4;
    }
}

/* Attach a DW_AT_const_value attribute for a variable or a parameter which
   does not have a "location" either in memory or in a register.  These
   things can arise in GNU C when a constant is passed as an actual parameter
   to an inlined function.  They can also arise in C++ where declared
   constants do not necessarily get memory "homes".  */

static bool
add_const_value_attribute (dw_die_ref die, rtx rtl)
{
  switch (GET_CODE (rtl))
    {
    case CONST_INT:
      {
	HOST_WIDE_INT val = INTVAL (rtl);

	if (val < 0)
	  add_AT_int (die, DW_AT_const_value, val);
	else
	  add_AT_unsigned (die, DW_AT_const_value, (unsigned HOST_WIDE_INT) val);
      }
      return true;

    case CONST_WIDE_INT:
      {
	wide_int w1 = rtx_mode_t (rtl, MAX_MODE_INT);
	unsigned int prec = MIN (wi::min_precision (w1, UNSIGNED),
				 (unsigned int)CONST_WIDE_INT_NUNITS (rtl) * HOST_BITS_PER_WIDE_INT);
	wide_int w = wi::zext (w1, prec);
	add_AT_wide (die, DW_AT_const_value, w);
      }
      return true;

    case CONST_DOUBLE:
      /* Note that a CONST_DOUBLE rtx could represent either an integer or a
	 floating-point constant.  A CONST_DOUBLE is used whenever the
	 constant requires more than one word in order to be adequately
	 represented.  */
      {
	machine_mode mode = GET_MODE (rtl);

	if (TARGET_SUPPORTS_WIDE_INT == 0 && !SCALAR_FLOAT_MODE_P (mode))
	  add_AT_double (die, DW_AT_const_value,
			 CONST_DOUBLE_HIGH (rtl), CONST_DOUBLE_LOW (rtl));
	else
	  {
	    unsigned int length = GET_MODE_SIZE (mode);
	    unsigned char *array = ggc_vec_alloc<unsigned char> (length);

	    insert_float (rtl, array);
	    add_AT_vec (die, DW_AT_const_value, length / 4, 4, array);
	  }
      }
      return true;

    case CONST_VECTOR:
      {
	machine_mode mode = GET_MODE (rtl);
	unsigned int elt_size = GET_MODE_UNIT_SIZE (mode);
	unsigned int length = CONST_VECTOR_NUNITS (rtl);
	unsigned char *array
	  = ggc_vec_alloc<unsigned char> (length * elt_size);
	unsigned int i;
	unsigned char *p;
	machine_mode imode = GET_MODE_INNER (mode);

	switch (GET_MODE_CLASS (mode))
	  {
	  case MODE_VECTOR_INT:
	    for (i = 0, p = array; i < length; i++, p += elt_size)
	      {
		rtx elt = CONST_VECTOR_ELT (rtl, i);
		insert_wide_int (rtx_mode_t (elt, imode), p, elt_size);
	      }
	    break;

	  case MODE_VECTOR_FLOAT:
	    for (i = 0, p = array; i < length; i++, p += elt_size)
	      {
		rtx elt = CONST_VECTOR_ELT (rtl, i);
		insert_float (elt, p);
	      }
	    break;

	  default:
	    gcc_unreachable ();
	  }

	add_AT_vec (die, DW_AT_const_value, length, elt_size, array);
      }
      return true;

    case CONST_STRING:
      if (dwarf_version >= 4 || !dwarf_strict)
	{
	  dw_loc_descr_ref loc_result;
	  resolve_one_addr (&rtl);
	rtl_addr:
          loc_result = new_addr_loc_descr (rtl, dtprel_false);
	  add_loc_descr (&loc_result, new_loc_descr (DW_OP_stack_value, 0, 0));
	  add_AT_loc (die, DW_AT_location, loc_result);
	  vec_safe_push (used_rtx_array, rtl);
	  return true;
	}
      return false;

    case CONST:
      if (CONSTANT_P (XEXP (rtl, 0)))
	return add_const_value_attribute (die, XEXP (rtl, 0));
      /* FALLTHROUGH */
    case SYMBOL_REF:
      if (!const_ok_for_output (rtl))
	return false;
      /* FALLTHROUGH */
    case LABEL_REF:
      if (dwarf_version >= 4 || !dwarf_strict)
	goto rtl_addr;
      return false;

    case PLUS:
      /* In cases where an inlined instance of an inline function is passed
	 the address of an `auto' variable (which is local to the caller) we
	 can get a situation where the DECL_RTL of the artificial local
	 variable (for the inlining) which acts as a stand-in for the
	 corresponding formal parameter (of the inline function) will look
	 like (plus:SI (reg:SI FRAME_PTR) (const_int ...)).  This is not
	 exactly a compile-time constant expression, but it isn't the address
	 of the (artificial) local variable either.  Rather, it represents the
	 *value* which the artificial local variable always has during its
	 lifetime.  We currently have no way to represent such quasi-constant
	 values in Dwarf, so for now we just punt and generate nothing.  */
      return false;

    case HIGH:
    case CONST_FIXED:
      return false;

    case MEM:
      if (GET_CODE (XEXP (rtl, 0)) == CONST_STRING
	  && MEM_READONLY_P (rtl)
	  && GET_MODE (rtl) == BLKmode)
	{
	  add_AT_string (die, DW_AT_const_value, XSTR (XEXP (rtl, 0), 0));
	  return true;
	}
      return false;

    default:
      /* No other kinds of rtx should be possible here.  */
      gcc_unreachable ();
    }
  return false;
}

/* Determine whether the evaluation of EXPR references any variables
   or functions which aren't otherwise used (and therefore may not be
   output).  */
static tree
reference_to_unused (tree * tp, int * walk_subtrees,
		     void * data ATTRIBUTE_UNUSED)
{
  if (! EXPR_P (*tp) && ! CONSTANT_CLASS_P (*tp))
    *walk_subtrees = 0;

  if (DECL_P (*tp) && ! TREE_PUBLIC (*tp) && ! TREE_USED (*tp)
      && ! TREE_ASM_WRITTEN (*tp))
    return *tp;
  /* ???  The C++ FE emits debug information for using decls, so
     putting gcc_unreachable here falls over.  See PR31899.  For now
     be conservative.  */
  else if (!symtab->global_info_ready && VAR_OR_FUNCTION_DECL_P (*tp))
    return *tp;
  else if (VAR_P (*tp))
    {
      varpool_node *node = varpool_node::get (*tp);
      if (!node || !node->definition)
	return *tp;
    }
  else if (TREE_CODE (*tp) == FUNCTION_DECL
	   && (!DECL_EXTERNAL (*tp) || DECL_DECLARED_INLINE_P (*tp)))
    {
      /* The call graph machinery must have finished analyzing,
         optimizing and gimplifying the CU by now.
	 So if *TP has no call graph node associated
	 to it, it means *TP will not be emitted.  */
      if (!cgraph_node::get (*tp))
	return *tp;
    }
  else if (TREE_CODE (*tp) == STRING_CST && !TREE_ASM_WRITTEN (*tp))
    return *tp;

  return NULL_TREE;
}

/* Generate an RTL constant from a decl initializer INIT with decl type TYPE,
   for use in a later add_const_value_attribute call.  */

static rtx
rtl_for_decl_init (tree init, tree type)
{
  rtx rtl = NULL_RTX;

  STRIP_NOPS (init);

  /* If a variable is initialized with a string constant without embedded
     zeros, build CONST_STRING.  */
  if (TREE_CODE (init) == STRING_CST && TREE_CODE (type) == ARRAY_TYPE)
    {
      tree enttype = TREE_TYPE (type);
      tree domain = TYPE_DOMAIN (type);
      machine_mode mode = TYPE_MODE (enttype);

      if (GET_MODE_CLASS (mode) == MODE_INT && GET_MODE_SIZE (mode) == 1
	  && domain
	  && integer_zerop (TYPE_MIN_VALUE (domain))
	  && compare_tree_int (TYPE_MAX_VALUE (domain),
			       TREE_STRING_LENGTH (init) - 1) == 0
	  && ((size_t) TREE_STRING_LENGTH (init)
	      == strlen (TREE_STRING_POINTER (init)) + 1))
	{
	  rtl = gen_rtx_CONST_STRING (VOIDmode,
				      ggc_strdup (TREE_STRING_POINTER (init)));
	  rtl = gen_rtx_MEM (BLKmode, rtl);
	  MEM_READONLY_P (rtl) = 1;
	}
    }
  /* Other aggregates, and complex values, could be represented using
     CONCAT: FIXME!  */
  else if (AGGREGATE_TYPE_P (type)
	   || (TREE_CODE (init) == VIEW_CONVERT_EXPR
	       && AGGREGATE_TYPE_P (TREE_TYPE (TREE_OPERAND (init, 0))))
	   || TREE_CODE (type) == COMPLEX_TYPE)
    ;
  /* Vectors only work if their mode is supported by the target.
     FIXME: generic vectors ought to work too.  */
  else if (TREE_CODE (type) == VECTOR_TYPE
	   && !VECTOR_MODE_P (TYPE_MODE (type)))
    ;
  /* If the initializer is something that we know will expand into an
     immediate RTL constant, expand it now.  We must be careful not to
     reference variables which won't be output.  */
  else if (initializer_constant_valid_p (init, type)
	   && ! walk_tree (&init, reference_to_unused, NULL, NULL))
    {
      /* Convert vector CONSTRUCTOR initializers to VECTOR_CST if
	 possible.  */
      if (TREE_CODE (type) == VECTOR_TYPE)
	switch (TREE_CODE (init))
	  {
	  case VECTOR_CST:
	    break;
	  case CONSTRUCTOR:
	    if (TREE_CONSTANT (init))
	      {
		vec<constructor_elt, va_gc> *elts = CONSTRUCTOR_ELTS (init);
		bool constant_p = true;
		tree value;
		unsigned HOST_WIDE_INT ix;

		/* Even when ctor is constant, it might contain non-*_CST
		   elements (e.g. { 1.0/0.0 - 1.0/0.0, 0.0 }) and those don't
		   belong into VECTOR_CST nodes.  */
		FOR_EACH_CONSTRUCTOR_VALUE (elts, ix, value)
		  if (!CONSTANT_CLASS_P (value))
		    {
		      constant_p = false;
		      break;
		    }

		if (constant_p)
		  {
		    init = build_vector_from_ctor (type, elts);
		    break;
		  }
	      }
	    /* FALLTHRU */

	  default:
	    return NULL;
	  }

      rtl = expand_expr (init, NULL_RTX, VOIDmode, EXPAND_INITIALIZER);

      /* If expand_expr returns a MEM, it wasn't immediate.  */
      gcc_assert (!rtl || !MEM_P (rtl));
    }

  return rtl;
}

/* Generate RTL for the variable DECL to represent its location.  */

static rtx
rtl_for_decl_location (tree decl)
{
  rtx rtl;

  /* Here we have to decide where we are going to say the parameter "lives"
     (as far as the debugger is concerned).  We only have a couple of
     choices.  GCC provides us with DECL_RTL and with DECL_INCOMING_RTL.

     DECL_RTL normally indicates where the parameter lives during most of the
     activation of the function.  If optimization is enabled however, this
     could be either NULL or else a pseudo-reg.  Both of those cases indicate
     that the parameter doesn't really live anywhere (as far as the code
     generation parts of GCC are concerned) during most of the function's
     activation.  That will happen (for example) if the parameter is never
     referenced within the function.

     We could just generate a location descriptor here for all non-NULL
     non-pseudo values of DECL_RTL and ignore all of the rest, but we can be
     a little nicer than that if we also consider DECL_INCOMING_RTL in cases
     where DECL_RTL is NULL or is a pseudo-reg.

     Note however that we can only get away with using DECL_INCOMING_RTL as
     a backup substitute for DECL_RTL in certain limited cases.  In cases
     where DECL_ARG_TYPE (decl) indicates the same type as TREE_TYPE (decl),
     we can be sure that the parameter was passed using the same type as it is
     declared to have within the function, and that its DECL_INCOMING_RTL
     points us to a place where a value of that type is passed.

     In cases where DECL_ARG_TYPE (decl) and TREE_TYPE (decl) are different,
     we cannot (in general) use DECL_INCOMING_RTL as a substitute for DECL_RTL
     because in these cases DECL_INCOMING_RTL points us to a value of some
     type which is *different* from the type of the parameter itself.  Thus,
     if we tried to use DECL_INCOMING_RTL to generate a location attribute in
     such cases, the debugger would end up (for example) trying to fetch a
     `float' from a place which actually contains the first part of a
     `double'.  That would lead to really incorrect and confusing
     output at debug-time.

     So, in general, we *do not* use DECL_INCOMING_RTL as a backup for DECL_RTL
     in cases where DECL_ARG_TYPE (decl) != TREE_TYPE (decl).  There
     are a couple of exceptions however.  On little-endian machines we can
     get away with using DECL_INCOMING_RTL even when DECL_ARG_TYPE (decl) is
     not the same as TREE_TYPE (decl), but only when DECL_ARG_TYPE (decl) is
     an integral type that is smaller than TREE_TYPE (decl). These cases arise
     when (on a little-endian machine) a non-prototyped function has a
     parameter declared to be of type `short' or `char'.  In such cases,
     TREE_TYPE (decl) will be `short' or `char', DECL_ARG_TYPE (decl) will
     be `int', and DECL_INCOMING_RTL will point to the lowest-order byte of the
     passed `int' value.  If the debugger then uses that address to fetch
     a `short' or a `char' (on a little-endian machine) the result will be
     the correct data, so we allow for such exceptional cases below.

     Note that our goal here is to describe the place where the given formal
     parameter lives during most of the function's activation (i.e. between the
     end of the prologue and the start of the epilogue).  We'll do that as best
     as we can. Note however that if the given formal parameter is modified
     sometime during the execution of the function, then a stack backtrace (at
     debug-time) will show the function as having been called with the *new*
     value rather than the value which was originally passed in.  This happens
     rarely enough that it is not a major problem, but it *is* a problem, and
     I'd like to fix it.

     A future version of dwarf2out.c may generate two additional attributes for
     any given DW_TAG_formal_parameter DIE which will describe the "passed
     type" and the "passed location" for the given formal parameter in addition
     to the attributes we now generate to indicate the "declared type" and the
     "active location" for each parameter.  This additional set of attributes
     could be used by debuggers for stack backtraces. Separately, note that
     sometimes DECL_RTL can be NULL and DECL_INCOMING_RTL can be NULL also.
     This happens (for example) for inlined-instances of inline function formal
     parameters which are never referenced.  This really shouldn't be
     happening.  All PARM_DECL nodes should get valid non-NULL
     DECL_INCOMING_RTL values.  FIXME.  */

  /* Use DECL_RTL as the "location" unless we find something better.  */
  rtl = DECL_RTL_IF_SET (decl);

  /* When generating abstract instances, ignore everything except
     constants, symbols living in memory, and symbols living in
     fixed registers.  */
  if (! reload_completed)
    {
      if (rtl
	  && (CONSTANT_P (rtl)
	      || (MEM_P (rtl)
	          && CONSTANT_P (XEXP (rtl, 0)))
	      || (REG_P (rtl)
	          && VAR_P (decl)
		  && TREE_STATIC (decl))))
	{
	  rtl = targetm.delegitimize_address (rtl);
	  return rtl;
	}
      rtl = NULL_RTX;
    }
  else if (TREE_CODE (decl) == PARM_DECL)
    {
      if (rtl == NULL_RTX
	  || is_pseudo_reg (rtl)
	  || (MEM_P (rtl)
	      && is_pseudo_reg (XEXP (rtl, 0))
	      && DECL_INCOMING_RTL (decl)
	      && MEM_P (DECL_INCOMING_RTL (decl))
	      && GET_MODE (rtl) == GET_MODE (DECL_INCOMING_RTL (decl))))
	{
	  tree declared_type = TREE_TYPE (decl);
	  tree passed_type = DECL_ARG_TYPE (decl);
	  machine_mode dmode = TYPE_MODE (declared_type);
	  machine_mode pmode = TYPE_MODE (passed_type);

	  /* This decl represents a formal parameter which was optimized out.
	     Note that DECL_INCOMING_RTL may be NULL in here, but we handle
	     all cases where (rtl == NULL_RTX) just below.  */
	  if (dmode == pmode)
	    rtl = DECL_INCOMING_RTL (decl);
	  else if ((rtl == NULL_RTX || is_pseudo_reg (rtl))
		   && SCALAR_INT_MODE_P (dmode)
		   && GET_MODE_SIZE (dmode) <= GET_MODE_SIZE (pmode)
		   && DECL_INCOMING_RTL (decl))
	    {
	      rtx inc = DECL_INCOMING_RTL (decl);
	      if (REG_P (inc))
		rtl = inc;
	      else if (MEM_P (inc))
		{
		  if (BYTES_BIG_ENDIAN)
		    rtl = adjust_address_nv (inc, dmode,
					     GET_MODE_SIZE (pmode)
					     - GET_MODE_SIZE (dmode));
		  else
		    rtl = inc;
		}
	    }
	}

      /* If the parm was passed in registers, but lives on the stack, then
	 make a big endian correction if the mode of the type of the
	 parameter is not the same as the mode of the rtl.  */
      /* ??? This is the same series of checks that are made in dbxout.c before
	 we reach the big endian correction code there.  It isn't clear if all
	 of these checks are necessary here, but keeping them all is the safe
	 thing to do.  */
      else if (MEM_P (rtl)
	       && XEXP (rtl, 0) != const0_rtx
	       && ! CONSTANT_P (XEXP (rtl, 0))
	       /* Not passed in memory.  */
	       && !MEM_P (DECL_INCOMING_RTL (decl))
	       /* Not passed by invisible reference.  */
	       && (!REG_P (XEXP (rtl, 0))
		   || REGNO (XEXP (rtl, 0)) == HARD_FRAME_POINTER_REGNUM
		   || REGNO (XEXP (rtl, 0)) == STACK_POINTER_REGNUM
#if !HARD_FRAME_POINTER_IS_ARG_POINTER
		   || REGNO (XEXP (rtl, 0)) == ARG_POINTER_REGNUM
#endif
		     )
	       /* Big endian correction check.  */
	       && BYTES_BIG_ENDIAN
	       && TYPE_MODE (TREE_TYPE (decl)) != GET_MODE (rtl)
	       && (GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (decl)))
		   < UNITS_PER_WORD))
	{
	  machine_mode addr_mode = get_address_mode (rtl);
	  int offset = (UNITS_PER_WORD
			- GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (decl))));

	  rtl = gen_rtx_MEM (TYPE_MODE (TREE_TYPE (decl)),
			     plus_constant (addr_mode, XEXP (rtl, 0), offset));
	}
    }
  else if (VAR_P (decl)
	   && rtl
	   && MEM_P (rtl)
	   && GET_MODE (rtl) != TYPE_MODE (TREE_TYPE (decl))
	   && BYTES_BIG_ENDIAN)
    {
      machine_mode addr_mode = get_address_mode (rtl);
      int rsize = GET_MODE_SIZE (GET_MODE (rtl));
      int dsize = GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (decl)));

      /* If a variable is declared "register" yet is smaller than
	 a register, then if we store the variable to memory, it
	 looks like we're storing a register-sized value, when in
	 fact we are not.  We need to adjust the offset of the
	 storage location to reflect the actual value's bytes,
	 else gdb will not be able to display it.  */
      if (rsize > dsize)
	rtl = gen_rtx_MEM (TYPE_MODE (TREE_TYPE (decl)),
			   plus_constant (addr_mode, XEXP (rtl, 0),
					  rsize - dsize));
    }

  /* A variable with no DECL_RTL but a DECL_INITIAL is a compile-time constant,
     and will have been substituted directly into all expressions that use it.
     C does not have such a concept, but C++ and other languages do.  */
  if (!rtl && VAR_P (decl) && DECL_INITIAL (decl))
    rtl = rtl_for_decl_init (DECL_INITIAL (decl), TREE_TYPE (decl));

  if (rtl)
    rtl = targetm.delegitimize_address (rtl);

  /* If we don't look past the constant pool, we risk emitting a
     reference to a constant pool entry that isn't referenced from
     code, and thus is not emitted.  */
  if (rtl)
    rtl = avoid_constant_pool_reference (rtl);

  /* Try harder to get a rtl.  If this symbol ends up not being emitted
     in the current CU, resolve_addr will remove the expression referencing
     it.  */
  if (rtl == NULL_RTX
      && VAR_P (decl)
      && !DECL_EXTERNAL (decl)
      && TREE_STATIC (decl)
      && DECL_NAME (decl)
      && !DECL_HARD_REGISTER (decl)
      && DECL_MODE (decl) != VOIDmode)
    {
      rtl = make_decl_rtl_for_debug (decl);
      if (!MEM_P (rtl)
	  || GET_CODE (XEXP (rtl, 0)) != SYMBOL_REF
	  || SYMBOL_REF_DECL (XEXP (rtl, 0)) != decl)
	rtl = NULL_RTX;
    }

  return rtl;
}

/* Check whether decl is a Fortran COMMON symbol.  If not, NULL_TREE is
   returned.  If so, the decl for the COMMON block is returned, and the
   value is the offset into the common block for the symbol.  */

static tree
fortran_common (tree decl, HOST_WIDE_INT *value)
{
  tree val_expr, cvar;
  machine_mode mode;
  HOST_WIDE_INT bitsize, bitpos;
  tree offset;
  int unsignedp, reversep, volatilep = 0;

  /* If the decl isn't a VAR_DECL, or if it isn't static, or if
     it does not have a value (the offset into the common area), or if it
     is thread local (as opposed to global) then it isn't common, and shouldn't
     be handled as such.  */
  if (!VAR_P (decl)
      || !TREE_STATIC (decl)
      || !DECL_HAS_VALUE_EXPR_P (decl)
      || !is_fortran ())
    return NULL_TREE;

  val_expr = DECL_VALUE_EXPR (decl);
  if (TREE_CODE (val_expr) != COMPONENT_REF)
    return NULL_TREE;

  cvar = get_inner_reference (val_expr, &bitsize, &bitpos, &offset, &mode,
			      &unsignedp, &reversep, &volatilep);

  if (cvar == NULL_TREE
      || !VAR_P (cvar)
      || DECL_ARTIFICIAL (cvar)
      || !TREE_PUBLIC (cvar))
    return NULL_TREE;

  *value = 0;
  if (offset != NULL)
    {
      if (!tree_fits_shwi_p (offset))
	return NULL_TREE;
      *value = tree_to_shwi (offset);
    }
  if (bitpos != 0)
    *value += bitpos / BITS_PER_UNIT;

  return cvar;
}

/* Generate *either* a DW_AT_location attribute or else a DW_AT_const_value
   data attribute for a variable or a parameter.  We generate the
   DW_AT_const_value attribute only in those cases where the given variable
   or parameter does not have a true "location" either in memory or in a
   register.  This can happen (for example) when a constant is passed as an
   actual argument in a call to an inline function.  (It's possible that
   these things can crop up in other ways also.)  Note that one type of
   constant value which can be passed into an inlined function is a constant
   pointer.  This can happen for example if an actual argument in an inlined
   function call evaluates to a compile-time constant address.

   CACHE_P is true if it is worth caching the location list for DECL,
   so that future calls can reuse it rather than regenerate it from scratch.
   This is true for BLOCK_NONLOCALIZED_VARS in inlined subroutines,
   since we will need to refer to them each time the function is inlined.  */

static bool
add_location_or_const_value_attribute (dw_die_ref die, tree decl, bool cache_p)
{
  rtx rtl;
  dw_loc_list_ref list;
  var_loc_list *loc_list;
  cached_dw_loc_list *cache;

  if (early_dwarf)
    return false;

  if (TREE_CODE (decl) == ERROR_MARK)
    return false;

  if (get_AT (die, DW_AT_location)
      || get_AT (die, DW_AT_const_value))
    return true;

  gcc_assert (VAR_P (decl) || TREE_CODE (decl) == PARM_DECL
	      || TREE_CODE (decl) == RESULT_DECL);

  /* Try to get some constant RTL for this decl, and use that as the value of
     the location.  */

  rtl = rtl_for_decl_location (decl);
  if (rtl && (CONSTANT_P (rtl) || GET_CODE (rtl) == CONST_STRING)
      && add_const_value_attribute (die, rtl))
    return true;

  /* See if we have single element location list that is equivalent to
     a constant value.  That way we are better to use add_const_value_attribute
     rather than expanding constant value equivalent.  */
  loc_list = lookup_decl_loc (decl);
  if (loc_list
      && loc_list->first
      && loc_list->first->next == NULL
      && NOTE_P (loc_list->first->loc)
      && NOTE_VAR_LOCATION (loc_list->first->loc)
      && NOTE_VAR_LOCATION_LOC (loc_list->first->loc))
    {
      struct var_loc_node *node;

      node = loc_list->first;
      rtl = NOTE_VAR_LOCATION_LOC (node->loc);
      if (GET_CODE (rtl) == EXPR_LIST)
	rtl = XEXP (rtl, 0);
      if ((CONSTANT_P (rtl) || GET_CODE (rtl) == CONST_STRING)
	  && add_const_value_attribute (die, rtl))
	 return true;
    }
  /* If this decl is from BLOCK_NONLOCALIZED_VARS, we might need its
     list several times.  See if we've already cached the contents.  */
  list = NULL;
  if (loc_list == NULL || cached_dw_loc_list_table == NULL)
    cache_p = false;
  if (cache_p)
    {
      cache = cached_dw_loc_list_table->find_with_hash (decl, DECL_UID (decl));
      if (cache)
	list = cache->loc_list;
    }
  if (list == NULL)
    {
      list = loc_list_from_tree (decl, decl_by_reference_p (decl) ? 0 : 2,
				 NULL);
      /* It is usually worth caching this result if the decl is from
	 BLOCK_NONLOCALIZED_VARS and if the list has at least two elements.  */
      if (cache_p && list && list->dw_loc_next)
	{
	  cached_dw_loc_list **slot
	    = cached_dw_loc_list_table->find_slot_with_hash (decl,
							     DECL_UID (decl),
							     INSERT);
	  cache = ggc_cleared_alloc<cached_dw_loc_list> ();
	  cache->decl_id = DECL_UID (decl);
	  cache->loc_list = list;
	  *slot = cache;
	}
    }
  if (list)
    {
      add_AT_location_description (die, DW_AT_location, list);
      return true;
    }
  /* None of that worked, so it must not really have a location;
     try adding a constant value attribute from the DECL_INITIAL.  */
  return tree_add_const_value_attribute_for_decl (die, decl);
}

/* Helper function for tree_add_const_value_attribute.  Natively encode
   initializer INIT into an array.  Return true if successful.  */

static bool
native_encode_initializer (tree init, unsigned char *array, int size)
{
  tree type;

  if (init == NULL_TREE)
    return false;

  STRIP_NOPS (init);
  switch (TREE_CODE (init))
    {
    case STRING_CST:
      type = TREE_TYPE (init);
      if (TREE_CODE (type) == ARRAY_TYPE)
	{
	  tree enttype = TREE_TYPE (type);
	  machine_mode mode = TYPE_MODE (enttype);

	  if (GET_MODE_CLASS (mode) != MODE_INT || GET_MODE_SIZE (mode) != 1)
	    return false;
	  if (int_size_in_bytes (type) != size)
	    return false;
	  if (size > TREE_STRING_LENGTH (init))
	    {
	      memcpy (array, TREE_STRING_POINTER (init),
		      TREE_STRING_LENGTH (init));
	      memset (array + TREE_STRING_LENGTH (init),
		      '\0', size - TREE_STRING_LENGTH (init));
	    }
	  else
	    memcpy (array, TREE_STRING_POINTER (init), size);
	  return true;
	}
      return false;
    case CONSTRUCTOR:
      type = TREE_TYPE (init);
      if (int_size_in_bytes (type) != size)
	return false;
      if (TREE_CODE (type) == ARRAY_TYPE)
	{
	  HOST_WIDE_INT min_index;
	  unsigned HOST_WIDE_INT cnt;
	  int curpos = 0, fieldsize;
	  constructor_elt *ce;

	  if (TYPE_DOMAIN (type) == NULL_TREE
	      || !tree_fits_shwi_p (TYPE_MIN_VALUE (TYPE_DOMAIN (type))))
	    return false;

	  fieldsize = int_size_in_bytes (TREE_TYPE (type));
	  if (fieldsize <= 0)
	    return false;

	  min_index = tree_to_shwi (TYPE_MIN_VALUE (TYPE_DOMAIN (type)));
	  memset (array, '\0', size);
	  FOR_EACH_VEC_SAFE_ELT (CONSTRUCTOR_ELTS (init), cnt, ce)
	    {
	      tree val = ce->value;
	      tree index = ce->index;
	      int pos = curpos;
	      if (index && TREE_CODE (index) == RANGE_EXPR)
		pos = (tree_to_shwi (TREE_OPERAND (index, 0)) - min_index)
		      * fieldsize;
	      else if (index)
		pos = (tree_to_shwi (index) - min_index) * fieldsize;

	      if (val)
		{
		  STRIP_NOPS (val);
		  if (!native_encode_initializer (val, array + pos, fieldsize))
		    return false;
		}
	      curpos = pos + fieldsize;
	      if (index && TREE_CODE (index) == RANGE_EXPR)
		{
		  int count = tree_to_shwi (TREE_OPERAND (index, 1))
			      - tree_to_shwi (TREE_OPERAND (index, 0));
		  while (count-- > 0)
		    {
		      if (val)
			memcpy (array + curpos, array + pos, fieldsize);
		      curpos += fieldsize;
		    }
		}
	      gcc_assert (curpos <= size);
	    }
	  return true;
	}
      else if (TREE_CODE (type) == RECORD_TYPE
	       || TREE_CODE (type) == UNION_TYPE)
	{
	  tree field = NULL_TREE;
	  unsigned HOST_WIDE_INT cnt;
	  constructor_elt *ce;

	  if (int_size_in_bytes (type) != size)
	    return false;

	  if (TREE_CODE (type) == RECORD_TYPE)
	    field = TYPE_FIELDS (type);

	  FOR_EACH_VEC_SAFE_ELT (CONSTRUCTOR_ELTS (init), cnt, ce)
	    {
	      tree val = ce->value;
	      int pos, fieldsize;

	      if (ce->index != 0)
		field = ce->index;

	      if (val)
		STRIP_NOPS (val);

	      if (field == NULL_TREE || DECL_BIT_FIELD (field))
		return false;

	      if (TREE_CODE (TREE_TYPE (field)) == ARRAY_TYPE
		  && TYPE_DOMAIN (TREE_TYPE (field))
		  && ! TYPE_MAX_VALUE (TYPE_DOMAIN (TREE_TYPE (field))))
		return false;
	      else if (DECL_SIZE_UNIT (field) == NULL_TREE
		       || !tree_fits_shwi_p (DECL_SIZE_UNIT (field)))
		return false;
	      fieldsize = tree_to_shwi (DECL_SIZE_UNIT (field));
	      pos = int_byte_position (field);
	      gcc_assert (pos + fieldsize <= size);
	      if (val && fieldsize != 0
		  && !native_encode_initializer (val, array + pos, fieldsize))
		return false;
	    }
	  return true;
	}
      return false;
    case VIEW_CONVERT_EXPR:
    case NON_LVALUE_EXPR:
      return native_encode_initializer (TREE_OPERAND (init, 0), array, size);
    default:
      return native_encode_expr (init, array, size) == size;
    }
}

/* Attach a DW_AT_const_value attribute to DIE. The value of the
   attribute is the const value T.  */

static bool
tree_add_const_value_attribute (dw_die_ref die, tree t)
{
  tree init;
  tree type = TREE_TYPE (t);
  rtx rtl;

  if (!t || !TREE_TYPE (t) || TREE_TYPE (t) == error_mark_node)
    return false;

  init = t;
  gcc_assert (!DECL_P (init));

  if (! early_dwarf)
    {
      rtl = rtl_for_decl_init (init, type);
      if (rtl)
	return add_const_value_attribute (die, rtl);
    }
  /* If the host and target are sane, try harder.  */
  if (CHAR_BIT == 8 && BITS_PER_UNIT == 8
      && initializer_constant_valid_p (init, type))
    {
      HOST_WIDE_INT size = int_size_in_bytes (TREE_TYPE (init));
      if (size > 0 && (int) size == size)
	{
	  unsigned char *array = ggc_cleared_vec_alloc<unsigned char> (size);

	  if (native_encode_initializer (init, array, size))
	    {
	      add_AT_vec (die, DW_AT_const_value, size, 1, array);
	      return true;
	    }
	  ggc_free (array);
	}
    }
  return false;
}

/* Attach a DW_AT_const_value attribute to VAR_DIE. The value of the
   attribute is the const value of T, where T is an integral constant
   variable with static storage duration
   (so it can't be a PARM_DECL or a RESULT_DECL).  */

static bool
tree_add_const_value_attribute_for_decl (dw_die_ref var_die, tree decl)
{

  if (!decl
      || (!VAR_P (decl) && TREE_CODE (decl) != CONST_DECL)
      || (VAR_P (decl) && !TREE_STATIC (decl)))
    return false;

  if (TREE_READONLY (decl)
      && ! TREE_THIS_VOLATILE (decl)
      && DECL_INITIAL (decl))
    /* OK */;
  else
    return false;

  /* Don't add DW_AT_const_value if abstract origin already has one.  */
  if (get_AT (var_die, DW_AT_const_value))
    return false;

  return tree_add_const_value_attribute (var_die, DECL_INITIAL (decl));
}

/* Convert the CFI instructions for the current function into a
   location list.  This is used for DW_AT_frame_base when we targeting
   a dwarf2 consumer that does not support the dwarf3
   DW_OP_call_frame_cfa.  OFFSET is a constant to be added to all CFA
   expressions.  */

static dw_loc_list_ref
convert_cfa_to_fb_loc_list (HOST_WIDE_INT offset)
{
  int ix;
  dw_fde_ref fde;
  dw_loc_list_ref list, *list_tail;
  dw_cfi_ref cfi;
  dw_cfa_location last_cfa, next_cfa;
  const char *start_label, *last_label, *section;
  dw_cfa_location remember;

  fde = cfun->fde;
  gcc_assert (fde != NULL);

  section = secname_for_decl (current_function_decl);
  list_tail = &list;
  list = NULL;

  memset (&next_cfa, 0, sizeof (next_cfa));
  next_cfa.reg = INVALID_REGNUM;
  remember = next_cfa;

  start_label = fde->dw_fde_begin;

  /* ??? Bald assumption that the CIE opcode list does not contain
     advance opcodes.  */
  FOR_EACH_VEC_ELT (*cie_cfi_vec, ix, cfi)
    lookup_cfa_1 (cfi, &next_cfa, &remember);

  last_cfa = next_cfa;
  last_label = start_label;

  if (fde->dw_fde_second_begin && fde->dw_fde_switch_cfi_index == 0)
    {
      /* If the first partition contained no CFI adjustments, the
	 CIE opcodes apply to the whole first partition.  */
      *list_tail = new_loc_list (build_cfa_loc (&last_cfa, offset),
				 fde->dw_fde_begin, fde->dw_fde_end, section);
      list_tail =&(*list_tail)->dw_loc_next;
      start_label = last_label = fde->dw_fde_second_begin;
    }

  FOR_EACH_VEC_SAFE_ELT (fde->dw_fde_cfi, ix, cfi)
    {
      switch (cfi->dw_cfi_opc)
	{
	case DW_CFA_set_loc:
	case DW_CFA_advance_loc1:
	case DW_CFA_advance_loc2:
	case DW_CFA_advance_loc4:
	  if (!cfa_equal_p (&last_cfa, &next_cfa))
	    {
	      *list_tail = new_loc_list (build_cfa_loc (&last_cfa, offset),
					 start_label, last_label, section);

	      list_tail = &(*list_tail)->dw_loc_next;
	      last_cfa = next_cfa;
	      start_label = last_label;
	    }
	  last_label = cfi->dw_cfi_oprnd1.dw_cfi_addr;
	  break;

	case DW_CFA_advance_loc:
	  /* The encoding is complex enough that we should never emit this.  */
	  gcc_unreachable ();

	default:
	  lookup_cfa_1 (cfi, &next_cfa, &remember);
	  break;
	}
      if (ix + 1 == fde->dw_fde_switch_cfi_index)
	{
	  if (!cfa_equal_p (&last_cfa, &next_cfa))
	    {
	      *list_tail = new_loc_list (build_cfa_loc (&last_cfa, offset),
					 start_label, last_label, section);

	      list_tail = &(*list_tail)->dw_loc_next;
	      last_cfa = next_cfa;
	      start_label = last_label;
	    }
	  *list_tail = new_loc_list (build_cfa_loc (&last_cfa, offset),
				     start_label, fde->dw_fde_end, section);
	  list_tail = &(*list_tail)->dw_loc_next;
	  start_label = last_label = fde->dw_fde_second_begin;
	}
    }

  if (!cfa_equal_p (&last_cfa, &next_cfa))
    {
      *list_tail = new_loc_list (build_cfa_loc (&last_cfa, offset),
				 start_label, last_label, section);
      list_tail = &(*list_tail)->dw_loc_next;
      start_label = last_label;
    }

  *list_tail = new_loc_list (build_cfa_loc (&next_cfa, offset),
			     start_label,
			     fde->dw_fde_second_begin
			     ? fde->dw_fde_second_end : fde->dw_fde_end,
			     section);

  if (list && list->dw_loc_next)
    gen_llsym (list);

  return list;
}

/* Compute a displacement from the "steady-state frame pointer" to the
   frame base (often the same as the CFA), and store it in
   frame_pointer_fb_offset.  OFFSET is added to the displacement
   before the latter is negated.  */

static void
compute_frame_pointer_to_fb_displacement (HOST_WIDE_INT offset)
{
  rtx reg, elim;

#ifdef FRAME_POINTER_CFA_OFFSET
  reg = frame_pointer_rtx;
  offset += FRAME_POINTER_CFA_OFFSET (current_function_decl);
#else
  reg = arg_pointer_rtx;
  offset += ARG_POINTER_CFA_OFFSET (current_function_decl);
#endif

  elim = (ira_use_lra_p
	  ? lra_eliminate_regs (reg, VOIDmode, NULL_RTX)
	  : eliminate_regs (reg, VOIDmode, NULL_RTX));
  if (GET_CODE (elim) == PLUS)
    {
      offset += INTVAL (XEXP (elim, 1));
      elim = XEXP (elim, 0);
    }

  frame_pointer_fb_offset = -offset;

  /* ??? AVR doesn't set up valid eliminations when there is no stack frame
     in which to eliminate.  This is because it's stack pointer isn't 
     directly accessible as a register within the ISA.  To work around
     this, assume that while we cannot provide a proper value for
     frame_pointer_fb_offset, we won't need one either.  */
  frame_pointer_fb_offset_valid
    = ((SUPPORTS_STACK_ALIGNMENT
	&& (elim == hard_frame_pointer_rtx
	    || elim == stack_pointer_rtx))
       || elim == (frame_pointer_needed
		   ? hard_frame_pointer_rtx
		   : stack_pointer_rtx));
}

/* Generate a DW_AT_name attribute given some string value to be included as
   the value of the attribute.  */

static void
add_name_attribute (dw_die_ref die, const char *name_string)
{
  if (name_string != NULL && *name_string != 0)
    {
      if (demangle_name_func)
	name_string = (*demangle_name_func) (name_string);

      add_AT_string (die, DW_AT_name, name_string);
    }
}

/* Retrieve the descriptive type of TYPE, if any, make sure it has a
   DIE and attach a DW_AT_GNAT_descriptive_type attribute to the DIE
   of TYPE accordingly.

   ??? This is a temporary measure until after we're able to generate
   regular DWARF for the complex Ada type system.  */

static void 
add_gnat_descriptive_type_attribute (dw_die_ref die, tree type,
				     dw_die_ref context_die)
{
  tree dtype;
  dw_die_ref dtype_die;

  if (!lang_hooks.types.descriptive_type)
    return;

  dtype = lang_hooks.types.descriptive_type (type);
  if (!dtype)
    return;

  dtype_die = lookup_type_die (dtype);
  if (!dtype_die)
    {
      gen_type_die (dtype, context_die);
      dtype_die = lookup_type_die (dtype);
      gcc_assert (dtype_die);
    }

  add_AT_die_ref (die, DW_AT_GNAT_descriptive_type, dtype_die);
}

/* Retrieve the comp_dir string suitable for use with DW_AT_comp_dir.  */

static const char *
comp_dir_string (void)
{
  const char *wd;
  char *wd1;
  static const char *cached_wd = NULL;

  if (cached_wd != NULL)
    return cached_wd;

  wd = get_src_pwd ();
  if (wd == NULL)
    return NULL;

  if (DWARF2_DIR_SHOULD_END_WITH_SEPARATOR)
    {
      int wdlen;

      wdlen = strlen (wd);
      wd1 = ggc_vec_alloc<char> (wdlen + 2);
      strcpy (wd1, wd);
      wd1 [wdlen] = DIR_SEPARATOR;
      wd1 [wdlen + 1] = 0;
      wd = wd1;
    }

  cached_wd = remap_debug_filename (wd);
  return cached_wd;
}

/* Generate a DW_AT_comp_dir attribute for DIE.  */

static void
add_comp_dir_attribute (dw_die_ref die)
{
  const char * wd = comp_dir_string ();
  if (wd != NULL)
    add_AT_string (die, DW_AT_comp_dir, wd);
}

/* Given a tree node VALUE describing a scalar attribute ATTR (i.e. a bound, a
   pointer computation, ...), output a representation for that bound according
   to the accepted FORMS (see enum dw_scalar_form) and add it to DIE.  See
   loc_list_from_tree for the meaning of CONTEXT.  */

static void
add_scalar_info (dw_die_ref die, enum dwarf_attribute attr, tree value,
		 int forms, struct loc_descr_context *context)
{
  dw_die_ref context_die, decl_die;
  dw_loc_list_ref list;
  bool strip_conversions = true;
  bool placeholder_seen = false;

  while (strip_conversions)
    switch (TREE_CODE (value))
      {
      case ERROR_MARK:
      case SAVE_EXPR:
	return;

      CASE_CONVERT:
      case VIEW_CONVERT_EXPR:
	value = TREE_OPERAND (value, 0);
	break;

      default:
	strip_conversions = false;
	break;
      }

  /* If possible and permitted, output the attribute as a constant.  */
  if ((forms & dw_scalar_form_constant) != 0
      && TREE_CODE (value) == INTEGER_CST)
    {
      unsigned int prec = simple_type_size_in_bits (TREE_TYPE (value));

      /* If HOST_WIDE_INT is big enough then represent the bound as
	 a constant value.  We need to choose a form based on
	 whether the type is signed or unsigned.  We cannot just
	 call add_AT_unsigned if the value itself is positive
	 (add_AT_unsigned might add the unsigned value encoded as
	 DW_FORM_data[1248]).  Some DWARF consumers will lookup the
	 bounds type and then sign extend any unsigned values found
	 for signed types.  This is needed only for
	 DW_AT_{lower,upper}_bound, since for most other attributes,
	 consumers will treat DW_FORM_data[1248] as unsigned values,
	 regardless of the underlying type.  */
      if (prec <= HOST_BITS_PER_WIDE_INT
	  || tree_fits_uhwi_p (value))
	{
	  if (TYPE_UNSIGNED (TREE_TYPE (value)))
	    add_AT_unsigned (die, attr, TREE_INT_CST_LOW (value));
	  else
	    add_AT_int (die, attr, TREE_INT_CST_LOW (value));
	}
      else
	/* Otherwise represent the bound as an unsigned value with
	   the precision of its type.  The precision and signedness
	   of the type will be necessary to re-interpret it
	   unambiguously.  */
	add_AT_wide (die, attr, value);
      return;
    }

  /* Otherwise, if it's possible and permitted too, output a reference to
     another DIE.  */
  if ((forms & dw_scalar_form_reference) != 0)
    {
      tree decl = NULL_TREE;

      /* Some type attributes reference an outer type.  For instance, the upper
	 bound of an array may reference an embedding record (this happens in
	 Ada).  */
      if (TREE_CODE (value) == COMPONENT_REF
	  && TREE_CODE (TREE_OPERAND (value, 0)) == PLACEHOLDER_EXPR
	  && TREE_CODE (TREE_OPERAND (value, 1)) == FIELD_DECL)
	decl = TREE_OPERAND (value, 1);

      else if (VAR_P (value)
	       || TREE_CODE (value) == PARM_DECL
	       || TREE_CODE (value) == RESULT_DECL)
	decl = value;

      if (decl != NULL_TREE)
	{
	  dw_die_ref decl_die = lookup_decl_die (decl);

	  /* ??? Can this happen, or should the variable have been bound
	     first?  Probably it can, since I imagine that we try to create
	     the types of parameters in the order in which they exist in
	     the list, and won't have created a forward reference to a
	     later parameter.  */
	  if (decl_die != NULL)
	    {
	      add_AT_die_ref (die, attr, decl_die);
	      return;
	    }
	}
    }

  /* Last chance: try to create a stack operation procedure to evaluate the
     value.  Do nothing if even that is not possible or permitted.  */
  if ((forms & dw_scalar_form_exprloc) == 0)
    return;

  list = loc_list_from_tree (value, 2, context);
  if (context && context->placeholder_arg)
    {
      placeholder_seen = context->placeholder_seen;
      context->placeholder_seen = false;
    }
  if (list == NULL || single_element_loc_list_p (list))
    {
      /* If this attribute is not a reference nor constant, it is
	 a DWARF expression rather than location description.  For that
	 loc_list_from_tree (value, 0, &context) is needed.  */
      dw_loc_list_ref list2 = loc_list_from_tree (value, 0, context);
      if (list2 && single_element_loc_list_p (list2))
	{
	  if (placeholder_seen)
	    {
	      struct dwarf_procedure_info dpi;
	      dpi.fndecl = NULL_TREE;
	      dpi.args_count = 1;
	      if (!resolve_args_picking (list2->expr, 1, &dpi))
		return;
	    }
	  add_AT_loc (die, attr, list2->expr);
	  return;
	}
    }

  /* If that failed to give a single element location list, fall back to
     outputting this as a reference... still if permitted.  */
  if (list == NULL
      || (forms & dw_scalar_form_reference) == 0
      || placeholder_seen)
    return;

  if (current_function_decl == 0)
    context_die = comp_unit_die ();
  else
    context_die = lookup_decl_die (current_function_decl);

  decl_die = new_die (DW_TAG_variable, context_die, value);
  add_AT_flag (decl_die, DW_AT_artificial, 1);
  add_type_attribute (decl_die, TREE_TYPE (value), TYPE_QUAL_CONST, false,
		      context_die);
  add_AT_location_description (decl_die, DW_AT_location, list);
  add_AT_die_ref (die, attr, decl_die);
}

/* Return the default for DW_AT_lower_bound, or -1 if there is not any
   default.  */

static int
lower_bound_default (void)
{
  switch (get_AT_unsigned (comp_unit_die (), DW_AT_language))
    {
    case DW_LANG_C:
    case DW_LANG_C89:
    case DW_LANG_C99:
    case DW_LANG_C11:
    case DW_LANG_C_plus_plus:
    case DW_LANG_C_plus_plus_11:
    case DW_LANG_C_plus_plus_14:
    case DW_LANG_ObjC:
    case DW_LANG_ObjC_plus_plus:
      return 0;
    case DW_LANG_Fortran77:
    case DW_LANG_Fortran90:
    case DW_LANG_Fortran95:
    case DW_LANG_Fortran03:
    case DW_LANG_Fortran08:
      return 1;
    case DW_LANG_UPC:
    case DW_LANG_D:
    case DW_LANG_Python:
      return dwarf_version >= 4 ? 0 : -1;
    case DW_LANG_Ada95:
    case DW_LANG_Ada83:
    case DW_LANG_Cobol74:
    case DW_LANG_Cobol85:
    case DW_LANG_Modula2:
    case DW_LANG_PLI:
      return dwarf_version >= 4 ? 1 : -1;
    default:
      return -1;
    }
}

/* Given a tree node describing an array bound (either lower or upper) output
   a representation for that bound.  */

static void
add_bound_info (dw_die_ref subrange_die, enum dwarf_attribute bound_attr,
		tree bound, struct loc_descr_context *context)
{
  int dflt;

  while (1)
    switch (TREE_CODE (bound))
      {
      /* Strip all conversions.  */
      CASE_CONVERT:
      case VIEW_CONVERT_EXPR:
	bound = TREE_OPERAND (bound, 0);
	break;

      /* All fixed-bounds are represented by INTEGER_CST nodes.  Lower bounds
	 are even omitted when they are the default.  */
      case INTEGER_CST:
	/* If the value for this bound is the default one, we can even omit the
	   attribute.  */
	if (bound_attr == DW_AT_lower_bound
	    && tree_fits_shwi_p (bound)
	    && (dflt = lower_bound_default ()) != -1
	    && tree_to_shwi (bound) == dflt)
	  return;

	/* FALLTHRU */

      default:
	/* Because of the complex interaction there can be with other GNAT
	   encodings, GDB isn't ready yet to handle proper DWARF description
	   for self-referencial subrange bounds: let GNAT encodings do the
	   magic in such a case.  */
	if (is_ada ()
	    && gnat_encodings != DWARF_GNAT_ENCODINGS_MINIMAL
	    && contains_placeholder_p (bound))
	  return;

	add_scalar_info (subrange_die, bound_attr, bound,
			 dw_scalar_form_constant
			 | dw_scalar_form_exprloc
			 | dw_scalar_form_reference,
			 context);
	return;
      }
}

/* Add subscript info to TYPE_DIE, describing an array TYPE, collapsing
   possibly nested array subscripts in a flat sequence if COLLAPSE_P is true.
   Note that the block of subscript information for an array type also
   includes information about the element type of the given array type.

   This function reuses previously set type and bound information if
   available.  */

static void
add_subscript_info (dw_die_ref type_die, tree type, bool collapse_p)
{
  unsigned dimension_number;
  tree lower, upper;
  dw_die_ref child = type_die->die_child;

  for (dimension_number = 0;
       TREE_CODE (type) == ARRAY_TYPE && (dimension_number == 0 || collapse_p);
       type = TREE_TYPE (type), dimension_number++)
    {
      tree domain = TYPE_DOMAIN (type);

      if (TYPE_STRING_FLAG (type) && is_fortran () && dimension_number > 0)
	break;

      /* Arrays come in three flavors: Unspecified bounds, fixed bounds,
	 and (in GNU C only) variable bounds.  Handle all three forms
	 here.  */

      /* Find and reuse a previously generated DW_TAG_subrange_type if
	 available.

         For multi-dimensional arrays, as we iterate through the
         various dimensions in the enclosing for loop above, we also
         iterate through the DIE children and pick at each
         DW_TAG_subrange_type previously generated (if available).
         Each child DW_TAG_subrange_type DIE describes the range of
         the current dimension.  At this point we should have as many
         DW_TAG_subrange_type's as we have dimensions in the
         array.  */
      dw_die_ref subrange_die = NULL;
      if (child)
	while (1)
	  {
	    child = child->die_sib;
	    if (child->die_tag == DW_TAG_subrange_type)
	      subrange_die = child;
	    if (child == type_die->die_child)
	      {
		/* If we wrapped around, stop looking next time.  */
		child = NULL;
		break;
	      }
	    if (child->die_tag == DW_TAG_subrange_type)
	      break;
	  }
      if (!subrange_die)
	subrange_die = new_die (DW_TAG_subrange_type, type_die, NULL);

      if (domain)
	{
	  /* We have an array type with specified bounds.  */
	  lower = TYPE_MIN_VALUE (domain);
	  upper = TYPE_MAX_VALUE (domain);

	  /* Define the index type.  */
	  if (TREE_TYPE (domain)
	      && !get_AT (subrange_die, DW_AT_type))
	    {
	      /* ??? This is probably an Ada unnamed subrange type.  Ignore the
		 TREE_TYPE field.  We can't emit debug info for this
		 because it is an unnamed integral type.  */
	      if (TREE_CODE (domain) == INTEGER_TYPE
		  && TYPE_NAME (domain) == NULL_TREE
		  && TREE_CODE (TREE_TYPE (domain)) == INTEGER_TYPE
		  && TYPE_NAME (TREE_TYPE (domain)) == NULL_TREE)
		;
	      else
		add_type_attribute (subrange_die, TREE_TYPE (domain),
				    TYPE_UNQUALIFIED, false, type_die);
	    }

	  /* ??? If upper is NULL, the array has unspecified length,
	     but it does have a lower bound.  This happens with Fortran
	       dimension arr(N:*)
	     Since the debugger is definitely going to need to know N
	     to produce useful results, go ahead and output the lower
	     bound solo, and hope the debugger can cope.  */

	  if (!get_AT (subrange_die, DW_AT_lower_bound))
	    add_bound_info (subrange_die, DW_AT_lower_bound, lower, NULL);
	  if (upper && !get_AT (subrange_die, DW_AT_upper_bound))
	    add_bound_info (subrange_die, DW_AT_upper_bound, upper, NULL);
	}

      /* Otherwise we have an array type with an unspecified length.  The
	 DWARF-2 spec does not say how to handle this; let's just leave out the
	 bounds.  */
    }
}

/* Add a DW_AT_byte_size attribute to DIE with TREE_NODE's size.  */

static void
add_byte_size_attribute (dw_die_ref die, tree tree_node)
{
  dw_die_ref decl_die;
  HOST_WIDE_INT size;
  dw_loc_descr_ref size_expr = NULL;

  switch (TREE_CODE (tree_node))
    {
    case ERROR_MARK:
      size = 0;
      break;
    case ENUMERAL_TYPE:
    case RECORD_TYPE:
    case UNION_TYPE:
    case QUAL_UNION_TYPE:
      if (TREE_CODE (TYPE_SIZE_UNIT (tree_node)) == VAR_DECL
	  && (decl_die = lookup_decl_die (TYPE_SIZE_UNIT (tree_node))))
	{
	  add_AT_die_ref (die, DW_AT_byte_size, decl_die);
	  return;
	}
      size_expr = type_byte_size (tree_node, &size);
      break;
    case FIELD_DECL:
      /* For a data member of a struct or union, the DW_AT_byte_size is
	 generally given as the number of bytes normally allocated for an
	 object of the *declared* type of the member itself.  This is true
	 even for bit-fields.  */
      size = int_size_in_bytes (field_type (tree_node));
      break;
    default:
      gcc_unreachable ();
    }

  /* Support for dynamically-sized objects was introduced by DWARFv3.
     At the moment, GDB does not handle variable byte sizes very well,
     though.  */
  if ((dwarf_version >= 3 || !dwarf_strict)
      && gnat_encodings == DWARF_GNAT_ENCODINGS_MINIMAL
      && size_expr != NULL)
    add_AT_loc (die, DW_AT_byte_size, size_expr);

  /* Note that `size' might be -1 when we get to this point.  If it is, that
     indicates that the byte size of the entity in question is variable and
     that we could not generate a DWARF expression that computes it.  */
  if (size >= 0)
    add_AT_unsigned (die, DW_AT_byte_size, size);
}

/* Add a DW_AT_alignment attribute to DIE with TREE_NODE's non-default
   alignment.  */

static void
add_alignment_attribute (dw_die_ref die, tree tree_node)
{
  if (dwarf_version < 5 && dwarf_strict)
    return;

  unsigned align;

  if (DECL_P (tree_node))
    {
      if (!DECL_USER_ALIGN (tree_node))
	return;

      align = DECL_ALIGN_UNIT (tree_node);
    }
  else if (TYPE_P (tree_node))
    {
      if (!TYPE_USER_ALIGN (tree_node))
	return;

      align = TYPE_ALIGN_UNIT (tree_node);
    }
  else
    gcc_unreachable ();

  add_AT_unsigned (die, DW_AT_alignment, align);
}

/* For a FIELD_DECL node which represents a bit-field, output an attribute
   which specifies the distance in bits from the highest order bit of the
   "containing object" for the bit-field to the highest order bit of the
   bit-field itself.

   For any given bit-field, the "containing object" is a hypothetical object
   (of some integral or enum type) within which the given bit-field lives.  The
   type of this hypothetical "containing object" is always the same as the
   declared type of the individual bit-field itself.  The determination of the
   exact location of the "containing object" for a bit-field is rather
   complicated.  It's handled by the `field_byte_offset' function (above).

   CTX is required: see the comment for VLR_CONTEXT.

   Note that it is the size (in bytes) of the hypothetical "containing object"
   which will be given in the DW_AT_byte_size attribute for this bit-field.
   (See `byte_size_attribute' above).  */

static inline void
add_bit_offset_attribute (dw_die_ref die, tree decl, struct vlr_context *ctx)
{
  HOST_WIDE_INT object_offset_in_bytes;
  tree original_type = DECL_BIT_FIELD_TYPE (decl);
  HOST_WIDE_INT bitpos_int;
  HOST_WIDE_INT highest_order_object_bit_offset;
  HOST_WIDE_INT highest_order_field_bit_offset;
  HOST_WIDE_INT bit_offset;

  field_byte_offset (decl, ctx, &object_offset_in_bytes);

  /* Must be a field and a bit field.  */
  gcc_assert (original_type && TREE_CODE (decl) == FIELD_DECL);

  /* We can't yet handle bit-fields whose offsets are variable, so if we
     encounter such things, just return without generating any attribute
     whatsoever.  Likewise for variable or too large size.  */
  if (! tree_fits_shwi_p (bit_position (decl))
      || ! tree_fits_uhwi_p (DECL_SIZE (decl)))
    return;

  bitpos_int = int_bit_position (decl);

  /* Note that the bit offset is always the distance (in bits) from the
     highest-order bit of the "containing object" to the highest-order bit of
     the bit-field itself.  Since the "high-order end" of any object or field
     is different on big-endian and little-endian machines, the computation
     below must take account of these differences.  */
  highest_order_object_bit_offset = object_offset_in_bytes * BITS_PER_UNIT;
  highest_order_field_bit_offset = bitpos_int;

  if (! BYTES_BIG_ENDIAN)
    {
      highest_order_field_bit_offset += tree_to_shwi (DECL_SIZE (decl));
      highest_order_object_bit_offset +=
        simple_type_size_in_bits (original_type);
    }

  bit_offset
    = (! BYTES_BIG_ENDIAN
       ? highest_order_object_bit_offset - highest_order_field_bit_offset
       : highest_order_field_bit_offset - highest_order_object_bit_offset);

  if (bit_offset < 0)
    add_AT_int (die, DW_AT_bit_offset, bit_offset);
  else
    add_AT_unsigned (die, DW_AT_bit_offset, (unsigned HOST_WIDE_INT) bit_offset);
}

/* For a FIELD_DECL node which represents a bit field, output an attribute
   which specifies the length in bits of the given field.  */

static inline void
add_bit_size_attribute (dw_die_ref die, tree decl)
{
  /* Must be a field and a bit field.  */
  gcc_assert (TREE_CODE (decl) == FIELD_DECL
	      && DECL_BIT_FIELD_TYPE (decl));

  if (tree_fits_uhwi_p (DECL_SIZE (decl)))
    add_AT_unsigned (die, DW_AT_bit_size, tree_to_uhwi (DECL_SIZE (decl)));
}

/* If the compiled language is ANSI C, then add a 'prototyped'
   attribute, if arg types are given for the parameters of a function.  */

static inline void
add_prototyped_attribute (dw_die_ref die, tree func_type)
{
  switch (get_AT_unsigned (comp_unit_die (), DW_AT_language))
    {
    case DW_LANG_C:
    case DW_LANG_C89:
    case DW_LANG_C99:
    case DW_LANG_C11:
    case DW_LANG_ObjC:
      if (prototype_p (func_type))
	add_AT_flag (die, DW_AT_prototyped, 1);
      break;
    default:
      break;
    }
}

/* Add an 'abstract_origin' attribute below a given DIE.  The DIE is found
   by looking in the type declaration, the object declaration equate table or
   the block mapping.  */

static inline dw_die_ref
add_abstract_origin_attribute (dw_die_ref die, tree origin)
{
  dw_die_ref origin_die = NULL;

  if (DECL_P (origin))
    {
      dw_die_ref c;
      origin_die = lookup_decl_die (origin);
      /* "Unwrap" the decls DIE which we put in the imported unit context.
         We are looking for the abstract copy here.  */
      if (in_lto_p
	  && origin_die
	  && (c = get_AT_ref (origin_die, DW_AT_abstract_origin))
	  /* ???  Identify this better.  */
	  && c->with_offset)
	origin_die = c;
    }
  else if (TYPE_P (origin))
    origin_die = lookup_type_die (origin);
  else if (TREE_CODE (origin) == BLOCK)
    origin_die = BLOCK_DIE (origin);

  /* XXX: Functions that are never lowered don't always have correct block
     trees (in the case of java, they simply have no block tree, in some other
     languages).  For these functions, there is nothing we can really do to
     output correct debug info for inlined functions in all cases.  Rather
     than die, we'll just produce deficient debug info now, in that we will
     have variables without a proper abstract origin.  In the future, when all
     functions are lowered, we should re-add a gcc_assert (origin_die)
     here.  */

  if (origin_die)
    add_AT_die_ref (die, DW_AT_abstract_origin, origin_die);
  return origin_die;
}

/* We do not currently support the pure_virtual attribute.  */

static inline void
add_pure_or_virtual_attribute (dw_die_ref die, tree func_decl)
{
  if (DECL_VINDEX (func_decl))
    {
      add_AT_unsigned (die, DW_AT_virtuality, DW_VIRTUALITY_virtual);

      if (tree_fits_shwi_p (DECL_VINDEX (func_decl)))
	add_AT_loc (die, DW_AT_vtable_elem_location,
		    new_loc_descr (DW_OP_constu,
				   tree_to_shwi (DECL_VINDEX (func_decl)),
				   0));

      /* GNU extension: Record what type this method came from originally.  */
      if (debug_info_level > DINFO_LEVEL_TERSE
	  && DECL_CONTEXT (func_decl))
	add_AT_die_ref (die, DW_AT_containing_type,
			lookup_type_die (DECL_CONTEXT (func_decl)));
    }
}

/* Add a DW_AT_linkage_name or DW_AT_MIPS_linkage_name attribute for the
   given decl.  This used to be a vendor extension until after DWARF 4
   standardized it.  */

static void
add_linkage_attr (dw_die_ref die, tree decl)
{
  const char *name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));

  /* Mimic what assemble_name_raw does with a leading '*'.  */
  if (name[0] == '*')
    name = &name[1];

  if (dwarf_version >= 4)
    add_AT_string (die, DW_AT_linkage_name, name);
  else
    add_AT_string (die, DW_AT_MIPS_linkage_name, name);
}

/* Add source coordinate attributes for the given decl.  */

static void
add_src_coords_attributes (dw_die_ref die, tree decl)
{
  expanded_location s;

  if (LOCATION_LOCUS (DECL_SOURCE_LOCATION (decl)) == UNKNOWN_LOCATION)
    return;
  s = expand_location (DECL_SOURCE_LOCATION (decl));
  add_AT_file (die, DW_AT_decl_file, lookup_filename (s.file));
  add_AT_unsigned (die, DW_AT_decl_line, s.line);
  if (debug_column_info && s.column)
    add_AT_unsigned (die, DW_AT_decl_column, s.column);
}

/* Add DW_AT_{,MIPS_}linkage_name attribute for the given decl.  */

static void
add_linkage_name_raw (dw_die_ref die, tree decl)
{
  /* Defer until we have an assembler name set.  */
  if (!DECL_ASSEMBLER_NAME_SET_P (decl))
    {
      limbo_die_node *asm_name;

      asm_name = ggc_cleared_alloc<limbo_die_node> ();
      asm_name->die = die;
      asm_name->created_for = decl;
      asm_name->next = deferred_asm_name;
      deferred_asm_name = asm_name;
    }
  else if (DECL_ASSEMBLER_NAME (decl) != DECL_NAME (decl))
    add_linkage_attr (die, decl);
}

/* Add DW_AT_{,MIPS_}linkage_name attribute for the given decl if desired.  */

static void
add_linkage_name (dw_die_ref die, tree decl)
{
  if (debug_info_level > DINFO_LEVEL_NONE
      && VAR_OR_FUNCTION_DECL_P (decl)
      && TREE_PUBLIC (decl)
      && !(VAR_P (decl) && DECL_REGISTER (decl))
      && die->die_tag != DW_TAG_member)
    add_linkage_name_raw (die, decl);
}

/* Add a DW_AT_name attribute and source coordinate attribute for the
   given decl, but only if it actually has a name.  */

static void
add_name_and_src_coords_attributes (dw_die_ref die, tree decl,
				    bool no_linkage_name)
{
  tree decl_name;

  decl_name = DECL_NAME (decl);
  if (decl_name != NULL && IDENTIFIER_POINTER (decl_name) != NULL)
    {
      const char *name = dwarf2_name (decl, 0);
      if (name)
	add_name_attribute (die, name);
      if (! DECL_ARTIFICIAL (decl))
	add_src_coords_attributes (die, decl);

      if (!no_linkage_name)
	add_linkage_name (die, decl);
    }

#ifdef VMS_DEBUGGING_INFO
  /* Get the function's name, as described by its RTL.  This may be different
     from the DECL_NAME name used in the source file.  */
  if (TREE_CODE (decl) == FUNCTION_DECL && TREE_ASM_WRITTEN (decl))
    {
      add_AT_addr (die, DW_AT_VMS_rtnbeg_pd_address,
                  XEXP (DECL_RTL (decl), 0), false);
      vec_safe_push (used_rtx_array, XEXP (DECL_RTL (decl), 0));
    }
#endif /* VMS_DEBUGGING_INFO */
}

/* Add VALUE as a DW_AT_discr_value attribute to DIE.  */

static void
add_discr_value (dw_die_ref die, dw_discr_value *value)
{
  dw_attr_node attr;

  attr.dw_attr = DW_AT_discr_value;
  attr.dw_attr_val.val_class = dw_val_class_discr_value;
  attr.dw_attr_val.val_entry = NULL;
  attr.dw_attr_val.v.val_discr_value.pos = value->pos;
  if (value->pos)
    attr.dw_attr_val.v.val_discr_value.v.uval = value->v.uval;
  else
    attr.dw_attr_val.v.val_discr_value.v.sval = value->v.sval;
  add_dwarf_attr (die, &attr);
}

/* Add DISCR_LIST as a DW_AT_discr_list to DIE.  */

static void
add_discr_list (dw_die_ref die, dw_discr_list_ref discr_list)
{
  dw_attr_node attr;

  attr.dw_attr = DW_AT_discr_list;
  attr.dw_attr_val.val_class = dw_val_class_discr_list;
  attr.dw_attr_val.val_entry = NULL;
  attr.dw_attr_val.v.val_discr_list = discr_list;
  add_dwarf_attr (die, &attr);
}

static inline dw_discr_list_ref
AT_discr_list (dw_attr_node *attr)
{
  return attr->dw_attr_val.v.val_discr_list;
}

#ifdef VMS_DEBUGGING_INFO
/* Output the debug main pointer die for VMS */

void
dwarf2out_vms_debug_main_pointer (void)
{
  char label[MAX_ARTIFICIAL_LABEL_BYTES];
  dw_die_ref die;

  /* Allocate the VMS debug main subprogram die.  */
  die = ggc_cleared_alloc<die_node> ();
  die->die_tag = DW_TAG_subprogram;
  add_name_attribute (die, VMS_DEBUG_MAIN_POINTER);
  ASM_GENERATE_INTERNAL_LABEL (label, PROLOGUE_END_LABEL,
			       current_function_funcdef_no);
  add_AT_lbl_id (die, DW_AT_entry_pc, label);

  /* Make it the first child of comp_unit_die ().  */
  die->die_parent = comp_unit_die ();
  if (comp_unit_die ()->die_child)
    {
      die->die_sib = comp_unit_die ()->die_child->die_sib;
      comp_unit_die ()->die_child->die_sib = die;
    }
  else
    {
      die->die_sib = die;
      comp_unit_die ()->die_child = die;
    }
}
#endif /* VMS_DEBUGGING_INFO */

/* Push a new declaration scope.  */

static void
push_decl_scope (tree scope)
{
  vec_safe_push (decl_scope_table, scope);
}

/* Pop a declaration scope.  */

static inline void
pop_decl_scope (void)
{
  decl_scope_table->pop ();
}

/* walk_tree helper function for uses_local_type, below.  */

static tree
uses_local_type_r (tree *tp, int *walk_subtrees, void *data ATTRIBUTE_UNUSED)
{
  if (!TYPE_P (*tp))
    *walk_subtrees = 0;
  else
    {
      tree name = TYPE_NAME (*tp);
      if (name && DECL_P (name) && decl_function_context (name))
	return *tp;
    }
  return NULL_TREE;
}

/* If TYPE involves a function-local type (including a local typedef to a
   non-local type), returns that type; otherwise returns NULL_TREE.  */

static tree
uses_local_type (tree type)
{
  tree used = walk_tree_without_duplicates (&type, uses_local_type_r, NULL);
  return used;
}

/* Return the DIE for the scope that immediately contains this type.
   Non-named types that do not involve a function-local type get global
   scope.  Named types nested in namespaces or other types get their
   containing scope.  All other types (i.e. function-local named types) get
   the current active scope.  */

static dw_die_ref
scope_die_for (tree t, dw_die_ref context_die)
{
  dw_die_ref scope_die = NULL;
  tree containing_scope;

  /* Non-types always go in the current scope.  */
  gcc_assert (TYPE_P (t));

  /* Use the scope of the typedef, rather than the scope of the type
     it refers to.  */
  if (TYPE_NAME (t) && DECL_P (TYPE_NAME (t)))
    containing_scope = DECL_CONTEXT (TYPE_NAME (t));
  else
    containing_scope = TYPE_CONTEXT (t);

  /* Use the containing namespace if there is one.  */
  if (containing_scope && TREE_CODE (containing_scope) == NAMESPACE_DECL)
    {
      if (context_die == lookup_decl_die (containing_scope))
	/* OK */;
      else if (debug_info_level > DINFO_LEVEL_TERSE)
	context_die = get_context_die (containing_scope);
      else
	containing_scope = NULL_TREE;
    }

  /* Ignore function type "scopes" from the C frontend.  They mean that
     a tagged type is local to a parmlist of a function declarator, but
     that isn't useful to DWARF.  */
  if (containing_scope && TREE_CODE (containing_scope) == FUNCTION_TYPE)
    containing_scope = NULL_TREE;

  if (SCOPE_FILE_SCOPE_P (containing_scope))
    {
      /* If T uses a local type keep it local as well, to avoid references
	 to function-local DIEs from outside the function.  */
      if (current_function_decl && uses_local_type (t))
	scope_die = context_die;
      else
	scope_die = comp_unit_die ();
    }
  else if (TYPE_P (containing_scope))
    {
      /* For types, we can just look up the appropriate DIE.  */
      if (debug_info_level > DINFO_LEVEL_TERSE)
	scope_die = get_context_die (containing_scope);
      else
	{
	  scope_die = lookup_type_die_strip_naming_typedef (containing_scope);
	  if (scope_die == NULL)
	    scope_die = comp_unit_die ();
	}
    }
  else
    scope_die = context_die;

  return scope_die;
}

/* Returns nonzero if CONTEXT_DIE is internal to a function.  */

static inline int
local_scope_p (dw_die_ref context_die)
{
  for (; context_die; context_die = context_die->die_parent)
    if (context_die->die_tag == DW_TAG_inlined_subroutine
	|| context_die->die_tag == DW_TAG_subprogram)
      return 1;

  return 0;
}

/* Returns nonzero if CONTEXT_DIE is a class.  */

static inline int
class_scope_p (dw_die_ref context_die)
{
  return (context_die
	  && (context_die->die_tag == DW_TAG_structure_type
	      || context_die->die_tag == DW_TAG_class_type
	      || context_die->die_tag == DW_TAG_interface_type
	      || context_die->die_tag == DW_TAG_union_type));
}

/* Returns nonzero if CONTEXT_DIE is a class or namespace, for deciding
   whether or not to treat a DIE in this context as a declaration.  */

static inline int
class_or_namespace_scope_p (dw_die_ref context_die)
{
  return (class_scope_p (context_die)
	  || (context_die && context_die->die_tag == DW_TAG_namespace));
}

/* Many forms of DIEs require a "type description" attribute.  This
   routine locates the proper "type descriptor" die for the type given
   by 'type' plus any additional qualifiers given by 'cv_quals', and
   adds a DW_AT_type attribute below the given die.  */

static void
add_type_attribute (dw_die_ref object_die, tree type, int cv_quals,
		    bool reverse, dw_die_ref context_die)
{
  enum tree_code code  = TREE_CODE (type);
  dw_die_ref type_die  = NULL;

  /* ??? If this type is an unnamed subrange type of an integral, floating-point
     or fixed-point type, use the inner type.  This is because we have no
     support for unnamed types in base_type_die.  This can happen if this is
     an Ada subrange type.  Correct solution is emit a subrange type die.  */
  if ((code == INTEGER_TYPE || code == REAL_TYPE || code == FIXED_POINT_TYPE)
      && TREE_TYPE (type) != 0 && TYPE_NAME (type) == 0)
    type = TREE_TYPE (type), code = TREE_CODE (type);

  if (code == ERROR_MARK
      /* Handle a special case.  For functions whose return type is void, we
	 generate *no* type attribute.  (Note that no object may have type
	 `void', so this only applies to function return types).  */
      || code == VOID_TYPE)
    return;

  type_die = modified_type_die (type,
				cv_quals | TYPE_QUALS_NO_ADDR_SPACE (type),
				reverse,
				context_die);

  if (type_die != NULL)
    add_AT_die_ref (object_die, DW_AT_type, type_die);
}

/* Given an object die, add the calling convention attribute for the
   function call type.  */
static void
add_calling_convention_attribute (dw_die_ref subr_die, tree decl)
{
  enum dwarf_calling_convention value = DW_CC_normal;

  value = ((enum dwarf_calling_convention)
	   targetm.dwarf_calling_convention (TREE_TYPE (decl)));

  if (is_fortran ()
      && id_equal (DECL_ASSEMBLER_NAME (decl), "MAIN__"))
    {
      /* DWARF 2 doesn't provide a way to identify a program's source-level
	entry point.  DW_AT_calling_convention attributes are only meant
	to describe functions' calling conventions.  However, lacking a
	better way to signal the Fortran main program, we used this for 
	a long time, following existing custom.  Now, DWARF 4 has 
	DW_AT_main_subprogram, which we add below, but some tools still
	rely on the old way, which we thus keep.  */
      value = DW_CC_program;

      if (dwarf_version >= 4 || !dwarf_strict)
	add_AT_flag (subr_die, DW_AT_main_subprogram, 1);
    }

  /* Only add the attribute if the backend requests it, and
     is not DW_CC_normal.  */
  if (value && (value != DW_CC_normal))
    add_AT_unsigned (subr_die, DW_AT_calling_convention, value);
}

/* Given a tree pointer to a struct, class, union, or enum type node, return
   a pointer to the (string) tag name for the given type, or zero if the type
   was declared without a tag.  */

static const char *
type_tag (const_tree type)
{
  const char *name = 0;

  if (TYPE_NAME (type) != 0)
    {
      tree t = 0;

      /* Find the IDENTIFIER_NODE for the type name.  */
      if (TREE_CODE (TYPE_NAME (type)) == IDENTIFIER_NODE
	  && !TYPE_NAMELESS (type))
	t = TYPE_NAME (type);

      /* The g++ front end makes the TYPE_NAME of *each* tagged type point to
	 a TYPE_DECL node, regardless of whether or not a `typedef' was
	 involved.  */
      else if (TREE_CODE (TYPE_NAME (type)) == TYPE_DECL
	       && ! DECL_IGNORED_P (TYPE_NAME (type)))
	{
	  /* We want to be extra verbose.  Don't call dwarf_name if
	     DECL_NAME isn't set.  The default hook for decl_printable_name
	     doesn't like that, and in this context it's correct to return
	     0, instead of "<anonymous>" or the like.  */
	  if (DECL_NAME (TYPE_NAME (type))
	      && !DECL_NAMELESS (TYPE_NAME (type)))
	    name = lang_hooks.dwarf_name (TYPE_NAME (type), 2);
	}

      /* Now get the name as a string, or invent one.  */
      if (!name && t != 0)
	name = IDENTIFIER_POINTER (t);
    }

  return (name == 0 || *name == '\0') ? 0 : name;
}

/* Return the type associated with a data member, make a special check
   for bit field types.  */

static inline tree
member_declared_type (const_tree member)
{
  return (DECL_BIT_FIELD_TYPE (member)
	  ? DECL_BIT_FIELD_TYPE (member) : TREE_TYPE (member));
}

/* Get the decl's label, as described by its RTL. This may be different
   from the DECL_NAME name used in the source file.  */

#if 0
static const char *
decl_start_label (tree decl)
{
  rtx x;
  const char *fnname;

  x = DECL_RTL (decl);
  gcc_assert (MEM_P (x));

  x = XEXP (x, 0);
  gcc_assert (GET_CODE (x) == SYMBOL_REF);

  fnname = XSTR (x, 0);
  return fnname;
}
#endif

/* For variable-length arrays that have been previously generated, but
   may be incomplete due to missing subscript info, fill the subscript
   info.  Return TRUE if this is one of those cases.  */
static bool
fill_variable_array_bounds (tree type)
{
  if (TREE_ASM_WRITTEN (type)
      && TREE_CODE (type) == ARRAY_TYPE
      && variably_modified_type_p (type, NULL))
    {
      dw_die_ref array_die = lookup_type_die (type);
      if (!array_die)
	return false;
      add_subscript_info (array_die, type, !is_ada ());
      return true;
    }
  return false;
}

/* These routines generate the internal representation of the DIE's for
   the compilation unit.  Debugging information is collected by walking
   the declaration trees passed in from dwarf2out_decl().  */

static void
gen_array_type_die (tree type, dw_die_ref context_die)
{
  dw_die_ref array_die;

  /* GNU compilers represent multidimensional array types as sequences of one
     dimensional array types whose element types are themselves array types.
     We sometimes squish that down to a single array_type DIE with multiple
     subscripts in the Dwarf debugging info.  The draft Dwarf specification
     say that we are allowed to do this kind of compression in C, because
     there is no difference between an array of arrays and a multidimensional
     array.  We don't do this for Ada to remain as close as possible to the
     actual representation, which is especially important against the language
     flexibilty wrt arrays of variable size.  */

  bool collapse_nested_arrays = !is_ada ();

  if (fill_variable_array_bounds (type))
    return;

  dw_die_ref scope_die = scope_die_for (type, context_die);
  tree element_type;

  /* Emit DW_TAG_string_type for Fortran character types (with kind 1 only, as
     DW_TAG_string_type doesn't have DW_AT_type attribute).  */
  if (TYPE_STRING_FLAG (type)
      && TREE_CODE (type) == ARRAY_TYPE
      && is_fortran ()
      && TYPE_MODE (TREE_TYPE (type)) == TYPE_MODE (char_type_node))
    {
      HOST_WIDE_INT size;

      array_die = new_die (DW_TAG_string_type, scope_die, type);
      add_name_attribute (array_die, type_tag (type));
      equate_type_number_to_die (type, array_die);
      size = int_size_in_bytes (type);
      if (size >= 0)
	add_AT_unsigned (array_die, DW_AT_byte_size, size);
      /* ???  We can't annotate types late, but for LTO we may not
	 generate a location early either (gfortran.dg/save_6.f90).  */
      else if (! (early_dwarf && (flag_generate_lto || flag_generate_offload))
	       && TYPE_DOMAIN (type) != NULL_TREE
	       && TYPE_MAX_VALUE (TYPE_DOMAIN (type)) != NULL_TREE)
	{
	  tree szdecl = TYPE_MAX_VALUE (TYPE_DOMAIN (type));
	  tree rszdecl = szdecl;

	  size = int_size_in_bytes (TREE_TYPE (szdecl));
	  if (!DECL_P (szdecl))
	    {
	      if (TREE_CODE (szdecl) == INDIRECT_REF
		  && DECL_P (TREE_OPERAND (szdecl, 0)))
		{
		  rszdecl = TREE_OPERAND (szdecl, 0);
		  if (int_size_in_bytes (TREE_TYPE (rszdecl))
		      != DWARF2_ADDR_SIZE)
		    size = 0;
		}
	      else
		size = 0;
	    }
	  if (size > 0)
	    {
	      dw_loc_list_ref loc
		= loc_list_from_tree (rszdecl, szdecl == rszdecl ? 2 : 0,
				      NULL);
	      if (loc)
		{
		  add_AT_location_description (array_die, DW_AT_string_length,
					       loc);
		  if (size != DWARF2_ADDR_SIZE)
		    add_AT_unsigned (array_die, dwarf_version >= 5
						? DW_AT_string_length_byte_size
						: DW_AT_byte_size, size);
		}
	    }
	}
      return;
    }

  array_die = new_die (DW_TAG_array_type, scope_die, type);
  add_name_attribute (array_die, type_tag (type));
  equate_type_number_to_die (type, array_die);

  if (TREE_CODE (type) == VECTOR_TYPE)
    add_AT_flag (array_die, DW_AT_GNU_vector, 1);

  /* For Fortran multidimensional arrays use DW_ORD_col_major ordering.  */
  if (is_fortran ()
      && TREE_CODE (type) == ARRAY_TYPE
      && TREE_CODE (TREE_TYPE (type)) == ARRAY_TYPE
      && !TYPE_STRING_FLAG (TREE_TYPE (type)))
    add_AT_unsigned (array_die, DW_AT_ordering, DW_ORD_col_major);

#if 0
  /* We default the array ordering.  SDB will probably do
     the right things even if DW_AT_ordering is not present.  It's not even
     an issue until we start to get into multidimensional arrays anyway.  If
     SDB is ever caught doing the Wrong Thing for multi-dimensional arrays,
     then we'll have to put the DW_AT_ordering attribute back in.  (But if
     and when we find out that we need to put these in, we will only do so
     for multidimensional arrays.  */
  add_AT_unsigned (array_die, DW_AT_ordering, DW_ORD_row_major);
#endif

  if (TREE_CODE (type) == VECTOR_TYPE)
    {
      /* For VECTOR_TYPEs we use an array die with appropriate bounds.  */
      dw_die_ref subrange_die = new_die (DW_TAG_subrange_type, array_die, NULL);
      add_bound_info (subrange_die, DW_AT_lower_bound, size_zero_node, NULL);
      add_bound_info (subrange_die, DW_AT_upper_bound,
		      size_int (TYPE_VECTOR_SUBPARTS (type) - 1), NULL);
    }
  else
    add_subscript_info (array_die, type, collapse_nested_arrays);

  /* Add representation of the type of the elements of this array type and
     emit the corresponding DIE if we haven't done it already.  */
  element_type = TREE_TYPE (type);
  if (collapse_nested_arrays)
    while (TREE_CODE (element_type) == ARRAY_TYPE)
      {
	if (TYPE_STRING_FLAG (element_type) && is_fortran ())
	  break;
	element_type = TREE_TYPE (element_type);
      }

  add_type_attribute (array_die, element_type, TYPE_UNQUALIFIED,
		      TREE_CODE (type) == ARRAY_TYPE
		      && TYPE_REVERSE_STORAGE_ORDER (type),
		      context_die);

  add_gnat_descriptive_type_attribute (array_die, type, context_die);
  if (TYPE_ARTIFICIAL (type))
    add_AT_flag (array_die, DW_AT_artificial, 1);

  if (get_AT (array_die, DW_AT_name))
    add_pubtype (type, array_die);

  add_alignment_attribute (array_die, type);
}

/* This routine generates DIE for array with hidden descriptor, details
   are filled into *info by a langhook.  */

static void
gen_descr_array_type_die (tree type, struct array_descr_info *info,
			  dw_die_ref context_die)
{
  const dw_die_ref scope_die = scope_die_for (type, context_die);
  const dw_die_ref array_die = new_die (DW_TAG_array_type, scope_die, type);
  struct loc_descr_context context = { type, info->base_decl, NULL,
				       false, false };
  enum dwarf_tag subrange_tag = DW_TAG_subrange_type;
  int dim;

  add_name_attribute (array_die, type_tag (type));
  equate_type_number_to_die (type, array_die);

  if (info->ndimensions > 1)
    switch (info->ordering)
      {
      case array_descr_ordering_row_major:
	add_AT_unsigned (array_die, DW_AT_ordering, DW_ORD_row_major);
	break;
      case array_descr_ordering_column_major:
	add_AT_unsigned (array_die, DW_AT_ordering, DW_ORD_col_major);
	break;
      default:
	break;
      }

  if (dwarf_version >= 3 || !dwarf_strict)
    {
      if (info->data_location)
	add_scalar_info (array_die, DW_AT_data_location, info->data_location,
			 dw_scalar_form_exprloc, &context);
      if (info->associated)
	add_scalar_info (array_die, DW_AT_associated, info->associated,
			 dw_scalar_form_constant
			 | dw_scalar_form_exprloc
			 | dw_scalar_form_reference, &context);
      if (info->allocated)
	add_scalar_info (array_die, DW_AT_allocated, info->allocated,
			 dw_scalar_form_constant
			 | dw_scalar_form_exprloc
			 | dw_scalar_form_reference, &context);
      if (info->stride)
	{
	  const enum dwarf_attribute attr
	    = (info->stride_in_bits) ? DW_AT_bit_stride : DW_AT_byte_stride;
	  const int forms
	    = (info->stride_in_bits)
	      ? dw_scalar_form_constant
	      : (dw_scalar_form_constant
		 | dw_scalar_form_exprloc
		 | dw_scalar_form_reference);

	  add_scalar_info (array_die, attr, info->stride, forms, &context);
	}
    }
  if (dwarf_version >= 5)
    {
      if (info->rank)
	{
	  add_scalar_info (array_die, DW_AT_rank, info->rank,
			   dw_scalar_form_constant
			   | dw_scalar_form_exprloc, &context);
	  subrange_tag = DW_TAG_generic_subrange;
	  context.placeholder_arg = true;
	}
    }

  add_gnat_descriptive_type_attribute (array_die, type, context_die);

  for (dim = 0; dim < info->ndimensions; dim++)
    {
      dw_die_ref subrange_die = new_die (subrange_tag, array_die, NULL);

      if (info->dimen[dim].bounds_type)
	add_type_attribute (subrange_die,
			    info->dimen[dim].bounds_type, TYPE_UNQUALIFIED,
			    false, context_die);
      if (info->dimen[dim].lower_bound)
	add_bound_info (subrange_die, DW_AT_lower_bound,
			info->dimen[dim].lower_bound, &context);
      if (info->dimen[dim].upper_bound)
	add_bound_info (subrange_die, DW_AT_upper_bound,
			info->dimen[dim].upper_bound, &context);
      if ((dwarf_version >= 3 || !dwarf_strict) && info->dimen[dim].stride)
	add_scalar_info (subrange_die, DW_AT_byte_stride,
			 info->dimen[dim].stride,
			 dw_scalar_form_constant
			 | dw_scalar_form_exprloc
			 | dw_scalar_form_reference,
			 &context);
    }

  gen_type_die (info->element_type, context_die);
  add_type_attribute (array_die, info->element_type, TYPE_UNQUALIFIED,
		      TREE_CODE (type) == ARRAY_TYPE
		      && TYPE_REVERSE_STORAGE_ORDER (type),
		      context_die);

  if (get_AT (array_die, DW_AT_name))
    add_pubtype (type, array_die);

  add_alignment_attribute (array_die, type);
}

#if 0
static void
gen_entry_point_die (tree decl, dw_die_ref context_die)
{
  tree origin = decl_ultimate_origin (decl);
  dw_die_ref decl_die = new_die (DW_TAG_entry_point, context_die, decl);

  if (origin != NULL)
    add_abstract_origin_attribute (decl_die, origin);
  else
    {
      add_name_and_src_coords_attributes (decl_die, decl);
      add_type_attribute (decl_die, TREE_TYPE (TREE_TYPE (decl)),
			  TYPE_UNQUALIFIED, false, context_die);
    }

  if (DECL_ABSTRACT_P (decl))
    equate_decl_number_to_die (decl, decl_die);
  else
    add_AT_lbl_id (decl_die, DW_AT_low_pc, decl_start_label (decl));
}
#endif

/* Walk through the list of incomplete types again, trying once more to
   emit full debugging info for them.  */

static void
retry_incomplete_types (void)
{
  set_early_dwarf s;
  int i;

  for (i = vec_safe_length (incomplete_types) - 1; i >= 0; i--)
    if (should_emit_struct_debug ((*incomplete_types)[i], DINFO_USAGE_DIR_USE))
      gen_type_die ((*incomplete_types)[i], comp_unit_die ());
  vec_safe_truncate (incomplete_types, 0);
}

/* Determine what tag to use for a record type.  */

static enum dwarf_tag
record_type_tag (tree type)
{
  if (! lang_hooks.types.classify_record)
    return DW_TAG_structure_type;

  switch (lang_hooks.types.classify_record (type))
    {
    case RECORD_IS_STRUCT:
      return DW_TAG_structure_type;

    case RECORD_IS_CLASS:
      return DW_TAG_class_type;

    case RECORD_IS_INTERFACE:
      if (dwarf_version >= 3 || !dwarf_strict)
	return DW_TAG_interface_type;
      return DW_TAG_structure_type;

    default:
      gcc_unreachable ();
    }
}

/* Generate a DIE to represent an enumeration type.  Note that these DIEs
   include all of the information about the enumeration values also. Each
   enumerated type name/value is listed as a child of the enumerated type
   DIE.  */

static dw_die_ref
gen_enumeration_type_die (tree type, dw_die_ref context_die)
{
  dw_die_ref type_die = lookup_type_die (type);

  if (type_die == NULL)
    {
      type_die = new_die (DW_TAG_enumeration_type,
			  scope_die_for (type, context_die), type);
      equate_type_number_to_die (type, type_die);
      add_name_attribute (type_die, type_tag (type));
      if (dwarf_version >= 4 || !dwarf_strict)
	{
	  if (ENUM_IS_SCOPED (type))
	    add_AT_flag (type_die, DW_AT_enum_class, 1);
	  if (ENUM_IS_OPAQUE (type))
	    add_AT_flag (type_die, DW_AT_declaration, 1);
	}
      if (!dwarf_strict)
	add_AT_unsigned (type_die, DW_AT_encoding,
			 TYPE_UNSIGNED (type)
			 ? DW_ATE_unsigned
			 : DW_ATE_signed);
    }
  else if (! TYPE_SIZE (type))
    return type_die;
  else
    remove_AT (type_die, DW_AT_declaration);

  /* Handle a GNU C/C++ extension, i.e. incomplete enum types.  If the
     given enum type is incomplete, do not generate the DW_AT_byte_size
     attribute or the DW_AT_element_list attribute.  */
  if (TYPE_SIZE (type))
    {
      tree link;

      TREE_ASM_WRITTEN (type) = 1;
      add_byte_size_attribute (type_die, type);
      add_alignment_attribute (type_die, type);
      if (dwarf_version >= 3 || !dwarf_strict)
	{
	  tree underlying = lang_hooks.types.enum_underlying_base_type (type);
	  add_type_attribute (type_die, underlying, TYPE_UNQUALIFIED, false,
			      context_die);
	}
      if (TYPE_STUB_DECL (type) != NULL_TREE)
	{
	  add_src_coords_attributes (type_die, TYPE_STUB_DECL (type));
	  add_accessibility_attribute (type_die, TYPE_STUB_DECL (type));
	}

      /* If the first reference to this type was as the return type of an
	 inline function, then it may not have a parent.  Fix this now.  */
      if (type_die->die_parent == NULL)
	add_child_die (scope_die_for (type, context_die), type_die);

      for (link = TYPE_VALUES (type);
	   link != NULL; link = TREE_CHAIN (link))
	{
	  dw_die_ref enum_die = new_die (DW_TAG_enumerator, type_die, link);
	  tree value = TREE_VALUE (link);

	  add_name_attribute (enum_die,
			      IDENTIFIER_POINTER (TREE_PURPOSE (link)));

	  if (TREE_CODE (value) == CONST_DECL)
	    value = DECL_INITIAL (value);

	  if (simple_type_size_in_bits (TREE_TYPE (value))
	      <= HOST_BITS_PER_WIDE_INT || tree_fits_shwi_p (value))
	    {
	      /* For constant forms created by add_AT_unsigned DWARF
		 consumers (GDB, elfutils, etc.) always zero extend
		 the value.  Only when the actual value is negative
		 do we need to use add_AT_int to generate a constant
		 form that can represent negative values.  */
	      HOST_WIDE_INT val = TREE_INT_CST_LOW (value);
	      if (TYPE_UNSIGNED (TREE_TYPE (value)) || val >= 0)
		add_AT_unsigned (enum_die, DW_AT_const_value,
				 (unsigned HOST_WIDE_INT) val);
	      else
		add_AT_int (enum_die, DW_AT_const_value, val);
	    }
	  else
	    /* Enumeration constants may be wider than HOST_WIDE_INT.  Handle
	       that here.  TODO: This should be re-worked to use correct
	       signed/unsigned double tags for all cases.  */
	    add_AT_wide (enum_die, DW_AT_const_value, value);
	}

      add_gnat_descriptive_type_attribute (type_die, type, context_die);
      if (TYPE_ARTIFICIAL (type))
	add_AT_flag (type_die, DW_AT_artificial, 1);
    }
  else
    add_AT_flag (type_die, DW_AT_declaration, 1);

  add_alignment_attribute (type_die, type);

  add_pubtype (type, type_die);

  return type_die;
}

/* Generate a DIE to represent either a real live formal parameter decl or to
   represent just the type of some formal parameter position in some function
   type.

   Note that this routine is a bit unusual because its argument may be a
   ..._DECL node (i.e. either a PARM_DECL or perhaps a VAR_DECL which
   represents an inlining of some PARM_DECL) or else some sort of a ..._TYPE
   node.  If it's the former then this function is being called to output a
   DIE to represent a formal parameter object (or some inlining thereof).  If
   it's the latter, then this function is only being called to output a
   DW_TAG_formal_parameter DIE to stand as a placeholder for some formal
   argument type of some subprogram type.
   If EMIT_NAME_P is true, name and source coordinate attributes
   are emitted.  */

static dw_die_ref
gen_formal_parameter_die (tree node, tree origin, bool emit_name_p,
			  dw_die_ref context_die)
{
  tree node_or_origin = node ? node : origin;
  tree ultimate_origin;
  dw_die_ref parm_die = NULL;
  
  if (TREE_CODE_CLASS (TREE_CODE (node_or_origin)) == tcc_declaration)
    {
      parm_die = lookup_decl_die (node);

      /* If the contexts differ, we may not be talking about the same
	 thing.
	 ???  When in LTO the DIE parent is the "abstract" copy and the
	 context_die is the specification "copy".  But this whole block
	 should eventually be no longer needed.  */
      if (parm_die && parm_die->die_parent != context_die && !in_lto_p)
	{
	  if (!DECL_ABSTRACT_P (node))
	    {
	      /* This can happen when creating an inlined instance, in
		 which case we need to create a new DIE that will get
		 annotated with DW_AT_abstract_origin.  */
	      parm_die = NULL;
	    }
	  else
	    gcc_unreachable ();
	}

      if (parm_die && parm_die->die_parent == NULL)
	{
	  /* Check that parm_die already has the right attributes that
	     we would have added below.  If any attributes are
	     missing, fall through to add them.  */
	  if (! DECL_ABSTRACT_P (node_or_origin)
	      && !get_AT (parm_die, DW_AT_location)
	      && !get_AT (parm_die, DW_AT_const_value))
	    /* We are missing  location info, and are about to add it.  */
	    ;
	  else
	    {
	      add_child_die (context_die, parm_die);
	      return parm_die;
	    }
	}
    }

  /* If we have a previously generated DIE, use it, unless this is an
     concrete instance (origin != NULL), in which case we need a new
     DIE with a corresponding DW_AT_abstract_origin.  */
  bool reusing_die;
  if (parm_die && origin == NULL)
    reusing_die = true;
  else
    {
      parm_die = new_die (DW_TAG_formal_parameter, context_die, node);
      reusing_die = false;
    }

  switch (TREE_CODE_CLASS (TREE_CODE (node_or_origin)))
    {
    case tcc_declaration:
      ultimate_origin = decl_ultimate_origin (node_or_origin);
      if (node || ultimate_origin)
	origin = ultimate_origin;

      if (reusing_die)
	goto add_location;

      if (origin != NULL)
	add_abstract_origin_attribute (parm_die, origin);
      else if (emit_name_p)
	add_name_and_src_coords_attributes (parm_die, node);
      if (origin == NULL
	  || (! DECL_ABSTRACT_P (node_or_origin)
	      && variably_modified_type_p (TREE_TYPE (node_or_origin),
					   decl_function_context
							    (node_or_origin))))
	{
	  tree type = TREE_TYPE (node_or_origin);
	  if (decl_by_reference_p (node_or_origin))
	    add_type_attribute (parm_die, TREE_TYPE (type),
				TYPE_UNQUALIFIED,
				false, context_die);
	  else
	    add_type_attribute (parm_die, type,
				decl_quals (node_or_origin),
				false, context_die);
	}
      if (origin == NULL && DECL_ARTIFICIAL (node))
	add_AT_flag (parm_die, DW_AT_artificial, 1);
    add_location:
      if (node && node != origin)
        equate_decl_number_to_die (node, parm_die);
      if (! DECL_ABSTRACT_P (node_or_origin))
	add_location_or_const_value_attribute (parm_die, node_or_origin,
					       node == NULL);

      break;

    case tcc_type:
      /* We were called with some kind of a ..._TYPE node.  */
      add_type_attribute (parm_die, node_or_origin, TYPE_UNQUALIFIED, false,
			  context_die);
      break;

    default:
      gcc_unreachable ();
    }

  return parm_die;
}

/* Generate and return a DW_TAG_GNU_formal_parameter_pack. Also generate
   children DW_TAG_formal_parameter DIEs representing the arguments of the
   parameter pack.

   PARM_PACK must be a function parameter pack.
   PACK_ARG is the first argument of the parameter pack. Its TREE_CHAIN
   must point to the subsequent arguments of the function PACK_ARG belongs to.
   SUBR_DIE is the DIE of the function PACK_ARG belongs to.
   If NEXT_ARG is non NULL, *NEXT_ARG is set to the function argument
   following the last one for which a DIE was generated.  */

static dw_die_ref
gen_formal_parameter_pack_die  (tree parm_pack,
				tree pack_arg,
				dw_die_ref subr_die,
				tree *next_arg)
{
  tree arg;
  dw_die_ref parm_pack_die;

  gcc_assert (parm_pack
	      && lang_hooks.function_parameter_pack_p (parm_pack)
	      && subr_die);

  parm_pack_die = new_die (DW_TAG_GNU_formal_parameter_pack, subr_die, parm_pack);
  add_src_coords_attributes (parm_pack_die, parm_pack);

  for (arg = pack_arg; arg; arg = DECL_CHAIN (arg))
    {
      if (! lang_hooks.decls.function_parm_expanded_from_pack_p (arg,
								 parm_pack))
	break;
      gen_formal_parameter_die (arg, NULL,
				false /* Don't emit name attribute.  */,
				parm_pack_die);
    }
  if (next_arg)
    *next_arg = arg;
  return parm_pack_die;
}

/* Generate a special type of DIE used as a stand-in for a trailing ellipsis
   at the end of an (ANSI prototyped) formal parameters list.  */

static void
gen_unspecified_parameters_die (tree decl_or_type, dw_die_ref context_die)
{
  new_die (DW_TAG_unspecified_parameters, context_die, decl_or_type);
}

/* Generate a list of nameless DW_TAG_formal_parameter DIEs (and perhaps a
   DW_TAG_unspecified_parameters DIE) to represent the types of the formal
   parameters as specified in some function type specification (except for
   those which appear as part of a function *definition*).  */

static void
gen_formal_types_die (tree function_or_method_type, dw_die_ref context_die)
{
  tree link;
  tree formal_type = NULL;
  tree first_parm_type;
  tree arg;

  if (TREE_CODE (function_or_method_type) == FUNCTION_DECL)
    {
      arg = DECL_ARGUMENTS (function_or_method_type);
      function_or_method_type = TREE_TYPE (function_or_method_type);
    }
  else
    arg = NULL_TREE;

  first_parm_type = TYPE_ARG_TYPES (function_or_method_type);

  /* Make our first pass over the list of formal parameter types and output a
     DW_TAG_formal_parameter DIE for each one.  */
  for (link = first_parm_type; link; )
    {
      dw_die_ref parm_die;

      formal_type = TREE_VALUE (link);
      if (formal_type == void_type_node)
	break;

      /* Output a (nameless) DIE to represent the formal parameter itself.  */
      if (!POINTER_BOUNDS_TYPE_P (formal_type))
	{
	  parm_die = gen_formal_parameter_die (formal_type, NULL,
					       true /* Emit name attribute.  */,
					       context_die);
	  if (TREE_CODE (function_or_method_type) == METHOD_TYPE
	      && link == first_parm_type)
	    {
	      add_AT_flag (parm_die, DW_AT_artificial, 1);
	      if (dwarf_version >= 3 || !dwarf_strict)
		add_AT_die_ref (context_die, DW_AT_object_pointer, parm_die);
	    }
	  else if (arg && DECL_ARTIFICIAL (arg))
	    add_AT_flag (parm_die, DW_AT_artificial, 1);
	}

      link = TREE_CHAIN (link);
      if (arg)
	arg = DECL_CHAIN (arg);
    }

  /* If this function type has an ellipsis, add a
     DW_TAG_unspecified_parameters DIE to the end of the parameter list.  */
  if (formal_type != void_type_node)
    gen_unspecified_parameters_die (function_or_method_type, context_die);

  /* Make our second (and final) pass over the list of formal parameter types
     and output DIEs to represent those types (as necessary).  */
  for (link = TYPE_ARG_TYPES (function_or_method_type);
       link && TREE_VALUE (link);
       link = TREE_CHAIN (link))
    gen_type_die (TREE_VALUE (link), context_die);
}

/* We want to generate the DIE for TYPE so that we can generate the
   die for MEMBER, which has been defined; we will need to refer back
   to the member declaration nested within TYPE.  If we're trying to
   generate minimal debug info for TYPE, processing TYPE won't do the
   trick; we need to attach the member declaration by hand.  */

static void
gen_type_die_for_member (tree type, tree member, dw_die_ref context_die)
{
  gen_type_die (type, context_die);

  /* If we're trying to avoid duplicate debug info, we may not have
     emitted the member decl for this function.  Emit it now.  */
  if (TYPE_STUB_DECL (type)
      && TYPE_DECL_SUPPRESS_DEBUG (TYPE_STUB_DECL (type))
      && ! lookup_decl_die (member))
    {
      dw_die_ref type_die;
      gcc_assert (!decl_ultimate_origin (member));

      push_decl_scope (type);
      type_die = lookup_type_die_strip_naming_typedef (type);
      if (TREE_CODE (member) == FUNCTION_DECL)
	gen_subprogram_die (member, type_die);
      else if (TREE_CODE (member) == FIELD_DECL)
	{
	  /* Ignore the nameless fields that are used to skip bits but handle
	     C++ anonymous unions and structs.  */
	  if (DECL_NAME (member) != NULL_TREE
	      || TREE_CODE (TREE_TYPE (member)) == UNION_TYPE
	      || TREE_CODE (TREE_TYPE (member)) == RECORD_TYPE)
	    {
	      struct vlr_context vlr_ctx = {
		DECL_CONTEXT (member), /* struct_type */
		NULL_TREE /* variant_part_offset */
	      };
	      gen_type_die (member_declared_type (member), type_die);
	      gen_field_die (member, &vlr_ctx, type_die);
	    }
	}
      else
	gen_variable_die (member, NULL_TREE, type_die);

      pop_decl_scope ();
    }
}

/* Forward declare these functions, because they are mutually recursive
  with their set_block_* pairing functions.  */
static void set_decl_origin_self (tree);

/* Given a pointer to some BLOCK node, if the BLOCK_ABSTRACT_ORIGIN for the
   given BLOCK node is NULL, set the BLOCK_ABSTRACT_ORIGIN for the node so
   that it points to the node itself, thus indicating that the node is its
   own (abstract) origin.  Additionally, if the BLOCK_ABSTRACT_ORIGIN for
   the given node is NULL, recursively descend the decl/block tree which
   it is the root of, and for each other ..._DECL or BLOCK node contained
   therein whose DECL_ABSTRACT_ORIGINs or BLOCK_ABSTRACT_ORIGINs are also
   still NULL, set *their* DECL_ABSTRACT_ORIGIN or BLOCK_ABSTRACT_ORIGIN
   values to point to themselves.  */

static void
set_block_origin_self (tree stmt)
{
  if (BLOCK_ABSTRACT_ORIGIN (stmt) == NULL_TREE)
    {
      BLOCK_ABSTRACT_ORIGIN (stmt) = stmt;

      {
	tree local_decl;

	for (local_decl = BLOCK_VARS (stmt);
	     local_decl != NULL_TREE;
	     local_decl = DECL_CHAIN (local_decl))
	  /* Do not recurse on nested functions since the inlining status
	     of parent and child can be different as per the DWARF spec.  */
	  if (TREE_CODE (local_decl) != FUNCTION_DECL
	      && !DECL_EXTERNAL (local_decl))
	    set_decl_origin_self (local_decl);
      }

      {
	tree subblock;

	for (subblock = BLOCK_SUBBLOCKS (stmt);
	     subblock != NULL_TREE;
	     subblock = BLOCK_CHAIN (subblock))
	  set_block_origin_self (subblock);	/* Recurse.  */
      }
    }
}

/* Given a pointer to some ..._DECL node, if the DECL_ABSTRACT_ORIGIN for
   the given ..._DECL node is NULL, set the DECL_ABSTRACT_ORIGIN for the
   node to so that it points to the node itself, thus indicating that the
   node represents its own (abstract) origin.  Additionally, if the
   DECL_ABSTRACT_ORIGIN for the given node is NULL, recursively descend
   the decl/block tree of which the given node is the root of, and for
   each other ..._DECL or BLOCK node contained therein whose
   DECL_ABSTRACT_ORIGINs or BLOCK_ABSTRACT_ORIGINs are also still NULL,
   set *their* DECL_ABSTRACT_ORIGIN or BLOCK_ABSTRACT_ORIGIN values to
   point to themselves.  */

static void
set_decl_origin_self (tree decl)
{
  if (DECL_ABSTRACT_ORIGIN (decl) == NULL_TREE)
    {
      DECL_ABSTRACT_ORIGIN (decl) = decl;
      if (TREE_CODE (decl) == FUNCTION_DECL)
	{
	  tree arg;

	  for (arg = DECL_ARGUMENTS (decl); arg; arg = DECL_CHAIN (arg))
	    DECL_ABSTRACT_ORIGIN (arg) = arg;
	  if (DECL_INITIAL (decl) != NULL_TREE
	      && DECL_INITIAL (decl) != error_mark_node)
	    set_block_origin_self (DECL_INITIAL (decl));
	}
    }
}

/* Mark the early DIE for DECL as the abstract instance.  */

static void
dwarf2out_abstract_function (tree decl)
{
  dw_die_ref old_die;

  /* Make sure we have the actual abstract inline, not a clone.  */
  decl = DECL_ORIGIN (decl);

  if (DECL_IGNORED_P (decl))
    return;

  old_die = lookup_decl_die (decl);
  /* With early debug we always have an old DIE unless we are in LTO
     and the user did not compile but only link with debug.  */
  if (in_lto_p && ! old_die)
    return;
  gcc_assert (old_die != NULL);
  if (get_AT (old_die, DW_AT_inline)
      || get_AT (old_die, DW_AT_abstract_origin))
    /* We've already generated the abstract instance.  */
    return;

  /* Go ahead and put DW_AT_inline on the DIE.  */
  if (DECL_DECLARED_INLINE_P (decl))
    {
      if (cgraph_function_possibly_inlined_p (decl))
	add_AT_unsigned (old_die, DW_AT_inline, DW_INL_declared_inlined);
      else
	add_AT_unsigned (old_die, DW_AT_inline, DW_INL_declared_not_inlined);
    }
  else
    {
      if (cgraph_function_possibly_inlined_p (decl))
	add_AT_unsigned (old_die, DW_AT_inline, DW_INL_inlined);
      else
	add_AT_unsigned (old_die, DW_AT_inline, DW_INL_not_inlined);
    }

  if (DECL_DECLARED_INLINE_P (decl)
      && lookup_attribute ("artificial", DECL_ATTRIBUTES (decl)))
    add_AT_flag (old_die, DW_AT_artificial, 1);

  set_decl_origin_self (decl);
}

/* Helper function of premark_used_types() which gets called through
   htab_traverse.

   Marks the DIE of a given type in *SLOT as perennial, so it never gets
   marked as unused by prune_unused_types.  */

bool
premark_used_types_helper (tree const &type, void *)
{
  dw_die_ref die;

  die = lookup_type_die (type);
  if (die != NULL)
    die->die_perennial_p = 1;
  return true;
}

/* Helper function of premark_types_used_by_global_vars which gets called
   through htab_traverse.

   Marks the DIE of a given type in *SLOT as perennial, so it never gets
   marked as unused by prune_unused_types. The DIE of the type is marked
   only if the global variable using the type will actually be emitted.  */

int
premark_types_used_by_global_vars_helper (types_used_by_vars_entry **slot,
					  void *)
{
  struct types_used_by_vars_entry *entry;
  dw_die_ref die;

  entry = (struct types_used_by_vars_entry *) *slot;
  gcc_assert (entry->type != NULL
	      && entry->var_decl != NULL);
  die = lookup_type_die (entry->type);
  if (die)
    {
      /* Ask cgraph if the global variable really is to be emitted.
         If yes, then we'll keep the DIE of ENTRY->TYPE.  */
      varpool_node *node = varpool_node::get (entry->var_decl);
      if (node && node->definition)
	{
	  die->die_perennial_p = 1;
	  /* Keep the parent DIEs as well.  */
	  while ((die = die->die_parent) && die->die_perennial_p == 0)
	    die->die_perennial_p = 1;
	}
    }
  return 1;
}

/* Mark all members of used_types_hash as perennial.  */

static void
premark_used_types (struct function *fun)
{
  if (fun && fun->used_types_hash)
    fun->used_types_hash->traverse<void *, premark_used_types_helper> (NULL);
}

/* Mark all members of types_used_by_vars_entry as perennial.  */

static void
premark_types_used_by_global_vars (void)
{
  if (types_used_by_vars_hash)
    types_used_by_vars_hash
      ->traverse<void *, premark_types_used_by_global_vars_helper> (NULL);
}

/* Generate a DW_TAG_call_site DIE in function DECL under SUBR_DIE
   for CA_LOC call arg loc node.  */

static dw_die_ref
gen_call_site_die (tree decl, dw_die_ref subr_die,
		   struct call_arg_loc_node *ca_loc)
{
  dw_die_ref stmt_die = NULL, die;
  tree block = ca_loc->block;

  while (block
	 && block != DECL_INITIAL (decl)
	 && TREE_CODE (block) == BLOCK)
    {
      stmt_die = BLOCK_DIE (block);
      if (stmt_die)
	break;
      block = BLOCK_SUPERCONTEXT (block);
    }
  if (stmt_die == NULL)
    stmt_die = subr_die;
  die = new_die (dwarf_TAG (DW_TAG_call_site), stmt_die, NULL_TREE);
  add_AT_lbl_id (die, dwarf_AT (DW_AT_call_return_pc), ca_loc->label);
  if (ca_loc->tail_call_p)
    add_AT_flag (die, dwarf_AT (DW_AT_call_tail_call), 1);
  if (ca_loc->symbol_ref)
    {
      dw_die_ref tdie = lookup_decl_die (SYMBOL_REF_DECL (ca_loc->symbol_ref));
      if (tdie)
	add_AT_die_ref (die, dwarf_AT (DW_AT_call_origin), tdie);
      else
	add_AT_addr (die, dwarf_AT (DW_AT_call_origin), ca_loc->symbol_ref,
		     false);
    }
  return die;
}

/* Generate a DIE to represent a declared function (either file-scope or
   block-local).  */

static void
gen_subprogram_die (tree decl, dw_die_ref context_die)
{
  tree origin = decl_ultimate_origin (decl);
  dw_die_ref subr_die;
  dw_die_ref old_die = lookup_decl_die (decl);

  /* This function gets called multiple times for different stages of
     the debug process.  For example, for func() in this code:

	namespace S
	{
	  void func() { ... }
	}

     ...we get called 4 times.  Twice in early debug and twice in
     late debug:

     Early debug
     -----------

       1. Once while generating func() within the namespace.  This is
          the declaration.  The declaration bit below is set, as the
          context is the namespace.

	  A new DIE will be generated with DW_AT_declaration set.

       2. Once for func() itself.  This is the specification.  The
          declaration bit below is clear as the context is the CU.

	  We will use the cached DIE from (1) to create a new DIE with
	  DW_AT_specification pointing to the declaration in (1).

     Late debug via rest_of_handle_final()
     -------------------------------------

       3. Once generating func() within the namespace.  This is also the
          declaration, as in (1), but this time we will early exit below
          as we have a cached DIE and a declaration needs no additional
          annotations (no locations), as the source declaration line
          info is enough.

       4. Once for func() itself.  As in (2), this is the specification,
          but this time we will re-use the cached DIE, and just annotate
          it with the location information that should now be available.

     For something without namespaces, but with abstract instances, we
     are also called a multiple times:

        class Base
	{
	public:
	  Base ();	  // constructor declaration (1)
	};

	Base::Base () { } // constructor specification (2)

    Early debug
    -----------

       1. Once for the Base() constructor by virtue of it being a
          member of the Base class.  This is done via
          rest_of_type_compilation.

	  This is a declaration, so a new DIE will be created with
	  DW_AT_declaration.

       2. Once for the Base() constructor definition, but this time
          while generating the abstract instance of the base
          constructor (__base_ctor) which is being generated via early
          debug of reachable functions.

	  Even though we have a cached version of the declaration (1),
	  we will create a DW_AT_specification of the declaration DIE
	  in (1).

       3. Once for the __base_ctor itself, but this time, we generate
          an DW_AT_abstract_origin version of the DW_AT_specification in
	  (2).

    Late debug via rest_of_handle_final
    -----------------------------------

       4. One final time for the __base_ctor (which will have a cached
          DIE with DW_AT_abstract_origin created in (3).  This time,
          we will just annotate the location information now
          available.
  */
  int declaration = (current_function_decl != decl
		     || class_or_namespace_scope_p (context_die));

  /* Now that the C++ front end lazily declares artificial member fns, we
     might need to retrofit the declaration into its class.  */
  if (!declaration && !origin && !old_die
      && DECL_CONTEXT (decl) && TYPE_P (DECL_CONTEXT (decl))
      && !class_or_namespace_scope_p (context_die)
      && debug_info_level > DINFO_LEVEL_TERSE)
    old_die = force_decl_die (decl);

  /* A concrete instance, tag a new DIE with DW_AT_abstract_origin.  */
  if (origin != NULL)
    {
      gcc_assert (!declaration || local_scope_p (context_die));

      /* Fixup die_parent for the abstract instance of a nested
	 inline function.  */
      if (old_die && old_die->die_parent == NULL)
	add_child_die (context_die, old_die);

      if (old_die && get_AT_ref (old_die, DW_AT_abstract_origin))
	{
	  /* If we have a DW_AT_abstract_origin we have a working
	     cached version.  */
	  subr_die = old_die;
	}
      else
	{
	  subr_die = new_die (DW_TAG_subprogram, context_die, decl);
	  add_abstract_origin_attribute (subr_die, origin);
	  /*  This is where the actual code for a cloned function is.
	      Let's emit linkage name attribute for it.  This helps
	      debuggers to e.g, set breakpoints into
	      constructors/destructors when the user asks "break
	      K::K".  */
	  add_linkage_name (subr_die, decl);
	}
    }
  /* A cached copy, possibly from early dwarf generation.  Reuse as
     much as possible.  */
  else if (old_die)
    {
      /* A declaration that has been previously dumped needs no
	 additional information.  */
      if (declaration)
	return;

      if (!get_AT_flag (old_die, DW_AT_declaration)
	  /* We can have a normal definition following an inline one in the
	     case of redefinition of GNU C extern inlines.
	     It seems reasonable to use AT_specification in this case.  */
	  && !get_AT (old_die, DW_AT_inline))
	{
	  /* Detect and ignore this case, where we are trying to output
	     something we have already output.  */
	  if (get_AT (old_die, DW_AT_low_pc)
	      || get_AT (old_die, DW_AT_ranges))
	    return;

	  /* If we have no location information, this must be a
	     partially generated DIE from early dwarf generation.
	     Fall through and generate it.  */
	}

      /* If the definition comes from the same place as the declaration,
	 maybe use the old DIE.  We always want the DIE for this function
	 that has the *_pc attributes to be under comp_unit_die so the
	 debugger can find it.  We also need to do this for abstract
	 instances of inlines, since the spec requires the out-of-line copy
	 to have the same parent.  For local class methods, this doesn't
	 apply; we just use the old DIE.  */
      expanded_location s = expand_location (DECL_SOURCE_LOCATION (decl));
      struct dwarf_file_data * file_index = lookup_filename (s.file);
      if ((is_cu_die (old_die->die_parent)
	   /* This condition fixes the inconsistency/ICE with the
	      following Fortran test (or some derivative thereof) while
	      building libgfortran:

		 module some_m
		 contains
		    logical function funky (FLAG)
		      funky = .true.
		   end function
		 end module
	   */
	   || (old_die->die_parent
	       && old_die->die_parent->die_tag == DW_TAG_module)
	   || context_die == NULL)
	   && (DECL_ARTIFICIAL (decl)
	       /* The location attributes may be in the abstract origin
		  which in the case of LTO might be not available to
		  look at.  */
	       || get_AT (old_die, DW_AT_abstract_origin)
	       || (get_AT_file (old_die, DW_AT_decl_file) == file_index
		   && (get_AT_unsigned (old_die, DW_AT_decl_line)
		       == (unsigned) s.line)
		   && (!debug_column_info
		       || s.column == 0
		       || (get_AT_unsigned (old_die, DW_AT_decl_column)
			   == (unsigned) s.column)))))
	{
	  subr_die = old_die;

	  /* Clear out the declaration attribute, but leave the
	     parameters so they can be augmented with location
	     information later.  Unless this was a declaration, in
	     which case, wipe out the nameless parameters and recreate
	     them further down.  */
	  if (remove_AT (subr_die, DW_AT_declaration))
	    {

	      remove_AT (subr_die, DW_AT_object_pointer);
	      remove_child_TAG (subr_die, DW_TAG_formal_parameter);
	    }
	}
      /* Make a specification pointing to the previously built
	 declaration.  */
      else
	{
	  subr_die = new_die (DW_TAG_subprogram, context_die, decl);
	  add_AT_specification (subr_die, old_die);
          add_pubname (decl, subr_die);
	  if (get_AT_file (old_die, DW_AT_decl_file) != file_index)
	    add_AT_file (subr_die, DW_AT_decl_file, file_index);
	  if (get_AT_unsigned (old_die, DW_AT_decl_line) != (unsigned) s.line)
	    add_AT_unsigned (subr_die, DW_AT_decl_line, s.line);
	  if (debug_column_info
	      && s.column
	      && (get_AT_unsigned (old_die, DW_AT_decl_column)
		  != (unsigned) s.column))
	    add_AT_unsigned (subr_die, DW_AT_decl_column, s.column);

	  /* If the prototype had an 'auto' or 'decltype(auto)' return type,
	     emit the real type on the definition die.  */
	  if (is_cxx () && debug_info_level > DINFO_LEVEL_TERSE)
	    {
	      dw_die_ref die = get_AT_ref (old_die, DW_AT_type);
	      if (die == auto_die || die == decltype_auto_die)
		add_type_attribute (subr_die, TREE_TYPE (TREE_TYPE (decl)),
				    TYPE_UNQUALIFIED, false, context_die);
	    }

	  /* When we process the method declaration, we haven't seen
	     the out-of-class defaulted definition yet, so we have to
	     recheck now.  */
	  if ((dwarf_version >= 5 || ! dwarf_strict)
	      && !get_AT (subr_die, DW_AT_defaulted))
	    {
	      int defaulted
		= lang_hooks.decls.decl_dwarf_attribute (decl,
							 DW_AT_defaulted);
	      if (defaulted != -1)
		{
		  /* Other values must have been handled before.  */
		  gcc_assert (defaulted == DW_DEFAULTED_out_of_class);
		  add_AT_unsigned (subr_die, DW_AT_defaulted, defaulted);
		}
	    }
	}
    }
  /* Create a fresh DIE for anything else.  */
  else
    {
      subr_die = new_die (DW_TAG_subprogram, context_die, decl);

      if (TREE_PUBLIC (decl))
	add_AT_flag (subr_die, DW_AT_external, 1);

      add_name_and_src_coords_attributes (subr_die, decl);
      add_pubname (decl, subr_die);
      if (debug_info_level > DINFO_LEVEL_TERSE)
	{
	  add_prototyped_attribute (subr_die, TREE_TYPE (decl));
	  add_type_attribute (subr_die, TREE_TYPE (TREE_TYPE (decl)),
			      TYPE_UNQUALIFIED, false, context_die);
	}

      add_pure_or_virtual_attribute (subr_die, decl);
      if (DECL_ARTIFICIAL (decl))
	add_AT_flag (subr_die, DW_AT_artificial, 1);

      if (TREE_THIS_VOLATILE (decl) && (dwarf_version >= 5 || !dwarf_strict))
	add_AT_flag (subr_die, DW_AT_noreturn, 1);

      add_alignment_attribute (subr_die, decl);

      add_accessibility_attribute (subr_die, decl);
    }

  /* Unless we have an existing non-declaration DIE, equate the new
     DIE.  */
  if (!old_die || is_declaration_die (old_die))
    equate_decl_number_to_die (decl, subr_die);

  if (declaration)
    {
      if (!old_die || !get_AT (old_die, DW_AT_inline))
	{
	  add_AT_flag (subr_die, DW_AT_declaration, 1);

	  /* If this is an explicit function declaration then generate
	     a DW_AT_explicit attribute.  */
	  if ((dwarf_version >= 3 || !dwarf_strict)
	      && lang_hooks.decls.decl_dwarf_attribute (decl,
							DW_AT_explicit) == 1)
	    add_AT_flag (subr_die, DW_AT_explicit, 1);

	  /* If this is a C++11 deleted special function member then generate
	     a DW_AT_deleted attribute.  */
	  if ((dwarf_version >= 5 || !dwarf_strict)
	      && lang_hooks.decls.decl_dwarf_attribute (decl,
							DW_AT_deleted) == 1)
	    add_AT_flag (subr_die, DW_AT_deleted, 1);

	  /* If this is a C++11 defaulted special function member then
	     generate a DW_AT_defaulted attribute.  */
	  if (dwarf_version >= 5 || !dwarf_strict)
	    {
	      int defaulted
		= lang_hooks.decls.decl_dwarf_attribute (decl,
							 DW_AT_defaulted);
	      if (defaulted != -1)
		add_AT_unsigned (subr_die, DW_AT_defaulted, defaulted);
	    }

	  /* If this is a C++11 non-static member function with & ref-qualifier
	     then generate a DW_AT_reference attribute.  */
	  if ((dwarf_version >= 5 || !dwarf_strict)
	      && lang_hooks.decls.decl_dwarf_attribute (decl,
							DW_AT_reference) == 1)
	    add_AT_flag (subr_die, DW_AT_reference, 1);

	  /* If this is a C++11 non-static member function with &&
	     ref-qualifier then generate a DW_AT_reference attribute.  */
	  if ((dwarf_version >= 5 || !dwarf_strict)
	      && lang_hooks.decls.decl_dwarf_attribute (decl,
							DW_AT_rvalue_reference)
		 == 1)
	    add_AT_flag (subr_die, DW_AT_rvalue_reference, 1);
	}
    }
  /* For non DECL_EXTERNALs, if range information is available, fill
     the DIE with it.  */
  else if (!DECL_EXTERNAL (decl) && !early_dwarf)
    {
      HOST_WIDE_INT cfa_fb_offset;

      struct function *fun = DECL_STRUCT_FUNCTION (decl);

      if (!crtl->has_bb_partition)
	{
	  dw_fde_ref fde = fun->fde;
	  if (fde->dw_fde_begin)
	    {
	      /* We have already generated the labels.  */
             add_AT_low_high_pc (subr_die, fde->dw_fde_begin,
                                 fde->dw_fde_end, false);
	    }
	  else
	    {
	      /* Create start/end labels and add the range.  */
	      char label_id_low[MAX_ARTIFICIAL_LABEL_BYTES];
	      char label_id_high[MAX_ARTIFICIAL_LABEL_BYTES];
	      ASM_GENERATE_INTERNAL_LABEL (label_id_low, FUNC_BEGIN_LABEL,
					   current_function_funcdef_no);
	      ASM_GENERATE_INTERNAL_LABEL (label_id_high, FUNC_END_LABEL,
					   current_function_funcdef_no);
             add_AT_low_high_pc (subr_die, label_id_low, label_id_high,
                                 false);
	    }

#if VMS_DEBUGGING_INFO
      /* HP OpenVMS Industry Standard 64: DWARF Extensions
	 Section 2.3 Prologue and Epilogue Attributes:
	 When a breakpoint is set on entry to a function, it is generally
	 desirable for execution to be suspended, not on the very first
	 instruction of the function, but rather at a point after the
	 function's frame has been set up, after any language defined local
	 declaration processing has been completed, and before execution of
	 the first statement of the function begins. Debuggers generally
	 cannot properly determine where this point is.  Similarly for a
	 breakpoint set on exit from a function. The prologue and epilogue
	 attributes allow a compiler to communicate the location(s) to use.  */

      {
        if (fde->dw_fde_vms_end_prologue)
          add_AT_vms_delta (subr_die, DW_AT_HP_prologue,
	    fde->dw_fde_begin, fde->dw_fde_vms_end_prologue);

        if (fde->dw_fde_vms_begin_epilogue)
          add_AT_vms_delta (subr_die, DW_AT_HP_epilogue,
	    fde->dw_fde_begin, fde->dw_fde_vms_begin_epilogue);
      }
#endif

	}
      else
	{
	  /* Generate pubnames entries for the split function code ranges.  */
	  dw_fde_ref fde = fun->fde;

	  if (fde->dw_fde_second_begin)
	    {
	      if (dwarf_version >= 3 || !dwarf_strict)
		{
		  /* We should use ranges for non-contiguous code section 
		     addresses.  Use the actual code range for the initial
		     section, since the HOT/COLD labels might precede an 
		     alignment offset.  */
		  bool range_list_added = false;
		  add_ranges_by_labels (subr_die, fde->dw_fde_begin,
					fde->dw_fde_end, &range_list_added,
					false);
		  add_ranges_by_labels (subr_die, fde->dw_fde_second_begin,
					fde->dw_fde_second_end,
					&range_list_added, false);
		  if (range_list_added)
		    add_ranges (NULL);
		}
	      else
		{
		  /* There is no real support in DW2 for this .. so we make
		     a work-around.  First, emit the pub name for the segment
		     containing the function label.  Then make and emit a
		     simplified subprogram DIE for the second segment with the
		     name pre-fixed by __hot/cold_sect_of_.  We use the same
		     linkage name for the second die so that gdb will find both
		     sections when given "b foo".  */
		  const char *name = NULL;
		  tree decl_name = DECL_NAME (decl);
		  dw_die_ref seg_die;

		  /* Do the 'primary' section.   */
		  add_AT_low_high_pc (subr_die, fde->dw_fde_begin,
                                      fde->dw_fde_end, false);

		  /* Build a minimal DIE for the secondary section.  */
		  seg_die = new_die (DW_TAG_subprogram,
				     subr_die->die_parent, decl);

		  if (TREE_PUBLIC (decl))
		    add_AT_flag (seg_die, DW_AT_external, 1);

		  if (decl_name != NULL 
		      && IDENTIFIER_POINTER (decl_name) != NULL)
		    {
		      name = dwarf2_name (decl, 1);
		      if (! DECL_ARTIFICIAL (decl))
			add_src_coords_attributes (seg_die, decl);

		      add_linkage_name (seg_die, decl);
		    }
		  gcc_assert (name != NULL);
		  add_pure_or_virtual_attribute (seg_die, decl);
		  if (DECL_ARTIFICIAL (decl))
		    add_AT_flag (seg_die, DW_AT_artificial, 1);

		  name = concat ("__second_sect_of_", name, NULL); 
		  add_AT_low_high_pc (seg_die, fde->dw_fde_second_begin,
                                      fde->dw_fde_second_end, false);
		  add_name_attribute (seg_die, name);
		  if (want_pubnames ())
		    add_pubname_string (name, seg_die);
		}
	    }
	  else
           add_AT_low_high_pc (subr_die, fde->dw_fde_begin, fde->dw_fde_end,
                               false);
	}

      cfa_fb_offset = CFA_FRAME_BASE_OFFSET (decl);

      /* We define the "frame base" as the function's CFA.  This is more
	 convenient for several reasons: (1) It's stable across the prologue
	 and epilogue, which makes it better than just a frame pointer,
	 (2) With dwarf3, there exists a one-byte encoding that allows us
	 to reference the .debug_frame data by proxy, but failing that,
	 (3) We can at least reuse the code inspection and interpretation
	 code that determines the CFA position at various points in the
	 function.  */
      if (dwarf_version >= 3 && targetm.debug_unwind_info () == UI_DWARF2)
	{
	  dw_loc_descr_ref op = new_loc_descr (DW_OP_call_frame_cfa, 0, 0);
	  add_AT_loc (subr_die, DW_AT_frame_base, op);
	}
      else
	{
	  dw_loc_list_ref list = convert_cfa_to_fb_loc_list (cfa_fb_offset);
	  if (list->dw_loc_next)
	    add_AT_loc_list (subr_die, DW_AT_frame_base, list);
	  else
	    add_AT_loc (subr_die, DW_AT_frame_base, list->expr);
	}

      /* Compute a displacement from the "steady-state frame pointer" to
	 the CFA.  The former is what all stack slots and argument slots
	 will reference in the rtl; the latter is what we've told the
	 debugger about.  We'll need to adjust all frame_base references
	 by this displacement.  */
      compute_frame_pointer_to_fb_displacement (cfa_fb_offset);

      if (fun->static_chain_decl)
	{
	  /* DWARF requires here a location expression that computes the
	     address of the enclosing subprogram's frame base.  The machinery
	     in tree-nested.c is supposed to store this specific address in the
	     last field of the FRAME record.  */
	  const tree frame_type
	    = TREE_TYPE (TREE_TYPE (fun->static_chain_decl));
	  const tree fb_decl = tree_last (TYPE_FIELDS (frame_type));

	  tree fb_expr
	    = build1 (INDIRECT_REF, frame_type, fun->static_chain_decl);
	  fb_expr = build3 (COMPONENT_REF, TREE_TYPE (fb_decl),
			    fb_expr, fb_decl, NULL_TREE);

	  add_AT_location_description (subr_die, DW_AT_static_link,
				       loc_list_from_tree (fb_expr, 0, NULL));
	}

      resolve_variable_values ();
    }

  /* Generate child dies for template paramaters.  */
  if (early_dwarf && debug_info_level > DINFO_LEVEL_TERSE)
    gen_generic_params_dies (decl);

  /* Now output descriptions of the arguments for this function. This gets
     (unnecessarily?) complex because of the fact that the DECL_ARGUMENT list
     for a FUNCTION_DECL doesn't indicate cases where there was a trailing
     `...' at the end of the formal parameter list.  In order to find out if
     there was a trailing ellipsis or not, we must instead look at the type
     associated with the FUNCTION_DECL.  This will be a node of type
     FUNCTION_TYPE. If the chain of type nodes hanging off of this
     FUNCTION_TYPE node ends with a void_type_node then there should *not* be
     an ellipsis at the end.  */

  /* In the case where we are describing a mere function declaration, all we
     need to do here (and all we *can* do here) is to describe the *types* of
     its formal parameters.  */
  if (debug_info_level <= DINFO_LEVEL_TERSE)
    ;
  else if (declaration)
    gen_formal_types_die (decl, subr_die);
  else
    {
      /* Generate DIEs to represent all known formal parameters.  */
      tree parm = DECL_ARGUMENTS (decl);
      tree generic_decl = early_dwarf
	? lang_hooks.decls.get_generic_function_decl (decl) : NULL;
      tree generic_decl_parm = generic_decl
				? DECL_ARGUMENTS (generic_decl)
				: NULL;

      /* Now we want to walk the list of parameters of the function and
	 emit their relevant DIEs.

	 We consider the case of DECL being an instance of a generic function
	 as well as it being a normal function.

	 If DECL is an instance of a generic function we walk the
	 parameters of the generic function declaration _and_ the parameters of
	 DECL itself. This is useful because we want to emit specific DIEs for
	 function parameter packs and those are declared as part of the
	 generic function declaration. In that particular case,
	 the parameter pack yields a DW_TAG_GNU_formal_parameter_pack DIE.
	 That DIE has children DIEs representing the set of arguments
	 of the pack. Note that the set of pack arguments can be empty.
	 In that case, the DW_TAG_GNU_formal_parameter_pack DIE will not have any
	 children DIE.

	 Otherwise, we just consider the parameters of DECL.  */
      while (generic_decl_parm || parm)
	{
	  if (generic_decl_parm
	      && lang_hooks.function_parameter_pack_p (generic_decl_parm))
	    gen_formal_parameter_pack_die (generic_decl_parm,
					   parm, subr_die,
					   &parm);
	  else if (parm && !POINTER_BOUNDS_P (parm))
	    {
	      dw_die_ref parm_die = gen_decl_die (parm, NULL, NULL, subr_die);

	      if (early_dwarf
		  && parm == DECL_ARGUMENTS (decl)
		  && TREE_CODE (TREE_TYPE (decl)) == METHOD_TYPE
		  && parm_die
		  && (dwarf_version >= 3 || !dwarf_strict))
		add_AT_die_ref (subr_die, DW_AT_object_pointer, parm_die);

	      parm = DECL_CHAIN (parm);
	    }
	  else if (parm)
	    parm = DECL_CHAIN (parm);

	  if (generic_decl_parm)
	    generic_decl_parm = DECL_CHAIN (generic_decl_parm);
	}

      /* Decide whether we need an unspecified_parameters DIE at the end.
	 There are 2 more cases to do this for: 1) the ansi ... declaration -
	 this is detectable when the end of the arg list is not a
	 void_type_node 2) an unprototyped function declaration (not a
	 definition).  This just means that we have no info about the
	 parameters at all.  */
      if (early_dwarf)
	{
	  if (prototype_p (TREE_TYPE (decl)))
	    {
	      /* This is the prototyped case, check for....  */
	      if (stdarg_p (TREE_TYPE (decl)))
		gen_unspecified_parameters_die (decl, subr_die);
	    }
	  else if (DECL_INITIAL (decl) == NULL_TREE)
	    gen_unspecified_parameters_die (decl, subr_die);
	}
    }

  if (subr_die != old_die)
    /* Add the calling convention attribute if requested.  */
    add_calling_convention_attribute (subr_die, decl);

  /* Output Dwarf info for all of the stuff within the body of the function
     (if it has one - it may be just a declaration).

     OUTER_SCOPE is a pointer to the outermost BLOCK node created to represent
     a function.  This BLOCK actually represents the outermost binding contour
     for the function, i.e. the contour in which the function's formal
     parameters and labels get declared. Curiously, it appears that the front
     end doesn't actually put the PARM_DECL nodes for the current function onto
     the BLOCK_VARS list for this outer scope, but are strung off of the
     DECL_ARGUMENTS list for the function instead.

     The BLOCK_VARS list for the `outer_scope' does provide us with a list of
     the LABEL_DECL nodes for the function however, and we output DWARF info
     for those in decls_for_scope.  Just within the `outer_scope' there will be
     a BLOCK node representing the function's outermost pair of curly braces,
     and any blocks used for the base and member initializers of a C++
     constructor function.  */
  tree outer_scope = DECL_INITIAL (decl);
  if (! declaration && outer_scope && TREE_CODE (outer_scope) != ERROR_MARK)
    {
      int call_site_note_count = 0;
      int tail_call_site_note_count = 0;

      /* Emit a DW_TAG_variable DIE for a named return value.  */
      if (DECL_NAME (DECL_RESULT (decl)))
	gen_decl_die (DECL_RESULT (decl), NULL, NULL, subr_die);

      /* The first time through decls_for_scope we will generate the
	 DIEs for the locals.  The second time, we fill in the
	 location info.  */
      decls_for_scope (outer_scope, subr_die);

      if (call_arg_locations && (!dwarf_strict || dwarf_version >= 5))
	{
	  struct call_arg_loc_node *ca_loc;
	  for (ca_loc = call_arg_locations; ca_loc; ca_loc = ca_loc->next)
	    {
	      dw_die_ref die = NULL;
	      rtx tloc = NULL_RTX, tlocc = NULL_RTX;
	      rtx arg, next_arg;

	      for (arg = (ca_loc->call_arg_loc_note != NULL_RTX
			  ? NOTE_VAR_LOCATION (ca_loc->call_arg_loc_note)
			  : NULL_RTX);
		   arg; arg = next_arg)
		{
		  dw_loc_descr_ref reg, val;
		  machine_mode mode = GET_MODE (XEXP (XEXP (arg, 0), 1));
		  dw_die_ref cdie, tdie = NULL;

		  next_arg = XEXP (arg, 1);
		  if (REG_P (XEXP (XEXP (arg, 0), 0))
		      && next_arg
		      && MEM_P (XEXP (XEXP (next_arg, 0), 0))
		      && REG_P (XEXP (XEXP (XEXP (next_arg, 0), 0), 0))
		      && REGNO (XEXP (XEXP (arg, 0), 0))
			 == REGNO (XEXP (XEXP (XEXP (next_arg, 0), 0), 0)))
		    next_arg = XEXP (next_arg, 1);
		  if (mode == VOIDmode)
		    {
		      mode = GET_MODE (XEXP (XEXP (arg, 0), 0));
		      if (mode == VOIDmode)
			mode = GET_MODE (XEXP (arg, 0));
		    }
		  if (mode == VOIDmode || mode == BLKmode)
		    continue;
		  /* Get dynamic information about call target only if we
		     have no static information: we cannot generate both
		     DW_AT_call_origin and DW_AT_call_target
		     attributes.  */
		  if (ca_loc->symbol_ref == NULL_RTX)
		    {
		      if (XEXP (XEXP (arg, 0), 0) == pc_rtx)
			{
			  tloc = XEXP (XEXP (arg, 0), 1);
			  continue;
			}
		      else if (GET_CODE (XEXP (XEXP (arg, 0), 0)) == CLOBBER
			       && XEXP (XEXP (XEXP (arg, 0), 0), 0) == pc_rtx)
			{
			  tlocc = XEXP (XEXP (arg, 0), 1);
			  continue;
			}
		    }
		  reg = NULL;
		  if (REG_P (XEXP (XEXP (arg, 0), 0)))
		    reg = reg_loc_descriptor (XEXP (XEXP (arg, 0), 0),
					      VAR_INIT_STATUS_INITIALIZED);
		  else if (MEM_P (XEXP (XEXP (arg, 0), 0)))
		    {
		      rtx mem = XEXP (XEXP (arg, 0), 0);
		      reg = mem_loc_descriptor (XEXP (mem, 0),
						get_address_mode (mem),
						GET_MODE (mem),
						VAR_INIT_STATUS_INITIALIZED);
		    }
		  else if (GET_CODE (XEXP (XEXP (arg, 0), 0))
			   == DEBUG_PARAMETER_REF)
		    {
		      tree tdecl
			= DEBUG_PARAMETER_REF_DECL (XEXP (XEXP (arg, 0), 0));
		      tdie = lookup_decl_die (tdecl);
		      if (tdie == NULL)
			continue;
		    }
		  else
		    continue;
		  if (reg == NULL
		      && GET_CODE (XEXP (XEXP (arg, 0), 0))
			 != DEBUG_PARAMETER_REF)
		    continue;
		  val = mem_loc_descriptor (XEXP (XEXP (arg, 0), 1), mode,
					    VOIDmode,
					    VAR_INIT_STATUS_INITIALIZED);
		  if (val == NULL)
		    continue;
		  if (die == NULL)
		    die = gen_call_site_die (decl, subr_die, ca_loc);
		  cdie = new_die (dwarf_TAG (DW_TAG_call_site_parameter), die,
				  NULL_TREE);
		  if (reg != NULL)
		    add_AT_loc (cdie, DW_AT_location, reg);
		  else if (tdie != NULL)
		    add_AT_die_ref (cdie, dwarf_AT (DW_AT_call_parameter),
				    tdie);
		  add_AT_loc (cdie, dwarf_AT (DW_AT_call_value), val);
		  if (next_arg != XEXP (arg, 1))
		    {
		      mode = GET_MODE (XEXP (XEXP (XEXP (arg, 1), 0), 1));
		      if (mode == VOIDmode)
			mode = GET_MODE (XEXP (XEXP (XEXP (arg, 1), 0), 0));
		      val = mem_loc_descriptor (XEXP (XEXP (XEXP (arg, 1),
							    0), 1),
						mode, VOIDmode,
						VAR_INIT_STATUS_INITIALIZED);
		      if (val != NULL)
			add_AT_loc (cdie, dwarf_AT (DW_AT_call_data_value),
				    val);
		    }
		}
	      if (die == NULL
		  && (ca_loc->symbol_ref || tloc))
		die = gen_call_site_die (decl, subr_die, ca_loc);
	      if (die != NULL && (tloc != NULL_RTX || tlocc != NULL_RTX))
		{
		  dw_loc_descr_ref tval = NULL;

		  if (tloc != NULL_RTX)
		    tval = mem_loc_descriptor (tloc,
					       GET_MODE (tloc) == VOIDmode
					       ? Pmode : GET_MODE (tloc),
					       VOIDmode,
					       VAR_INIT_STATUS_INITIALIZED);
		  if (tval)
		    add_AT_loc (die, dwarf_AT (DW_AT_call_target), tval);
		  else if (tlocc != NULL_RTX)
		    {
		      tval = mem_loc_descriptor (tlocc,
						 GET_MODE (tlocc) == VOIDmode
						 ? Pmode : GET_MODE (tlocc),
						 VOIDmode,
						 VAR_INIT_STATUS_INITIALIZED);
		      if (tval)
			add_AT_loc (die,
				    dwarf_AT (DW_AT_call_target_clobbered),
				    tval);
		    }
		}
	      if (die != NULL)
		{
		  call_site_note_count++;
		  if (ca_loc->tail_call_p)
		    tail_call_site_note_count++;
		}
	    }
	}
      call_arg_locations = NULL;
      call_arg_loc_last = NULL;
      if (tail_call_site_count >= 0
	  && tail_call_site_count == tail_call_site_note_count
	  && (!dwarf_strict || dwarf_version >= 5))
	{
	  if (call_site_count >= 0
	      && call_site_count == call_site_note_count)
	    add_AT_flag (subr_die, dwarf_AT (DW_AT_call_all_calls), 1);
	  else
	    add_AT_flag (subr_die, dwarf_AT (DW_AT_call_all_tail_calls), 1);
	}
      call_site_count = -1;
      tail_call_site_count = -1;
    }

  /* Mark used types after we have created DIEs for the functions scopes.  */
  premark_used_types (DECL_STRUCT_FUNCTION (decl));
}

/* Returns a hash value for X (which really is a die_struct).  */

hashval_t
block_die_hasher::hash (die_struct *d)
{
  return (hashval_t) d->decl_id ^ htab_hash_pointer (d->die_parent);
}

/* Return nonzero if decl_id and die_parent of die_struct X is the same
   as decl_id and die_parent of die_struct Y.  */

bool
block_die_hasher::equal (die_struct *x, die_struct *y)
{
  return x->decl_id == y->decl_id && x->die_parent == y->die_parent;
}

/* Return TRUE if DECL, which may have been previously generated as
   OLD_DIE, is a candidate for a DW_AT_specification.  DECLARATION is
   true if decl (or its origin) is either an extern declaration or a
   class/namespace scoped declaration.

   The declare_in_namespace support causes us to get two DIEs for one
   variable, both of which are declarations.  We want to avoid
   considering one to be a specification, so we must test for
   DECLARATION and DW_AT_declaration.  */
static inline bool
decl_will_get_specification_p (dw_die_ref old_die, tree decl, bool declaration)
{
  return (old_die && TREE_STATIC (decl) && !declaration
	  && get_AT_flag (old_die, DW_AT_declaration) == 1);
}

/* Return true if DECL is a local static.  */

static inline bool
local_function_static (tree decl)
{
  gcc_assert (VAR_P (decl));
  return TREE_STATIC (decl)
    && DECL_CONTEXT (decl)
    && TREE_CODE (DECL_CONTEXT (decl)) == FUNCTION_DECL;
}

/* Generate a DIE to represent a declared data object.
   Either DECL or ORIGIN must be non-null.  */

static void
gen_variable_die (tree decl, tree origin, dw_die_ref context_die)
{
  HOST_WIDE_INT off = 0;
  tree com_decl;
  tree decl_or_origin = decl ? decl : origin;
  tree ultimate_origin;
  dw_die_ref var_die;
  dw_die_ref old_die = decl ? lookup_decl_die (decl) : NULL;
  bool declaration = (DECL_EXTERNAL (decl_or_origin)
		      || class_or_namespace_scope_p (context_die));
  bool specialization_p = false;
  bool no_linkage_name = false;

  /* While C++ inline static data members have definitions inside of the
     class, force the first DIE to be a declaration, then let gen_member_die
     reparent it to the class context and call gen_variable_die again
     to create the outside of the class DIE for the definition.  */
  if (!declaration
      && old_die == NULL
      && decl
      && DECL_CONTEXT (decl)
      && TYPE_P (DECL_CONTEXT (decl))
      && lang_hooks.decls.decl_dwarf_attribute (decl, DW_AT_inline) != -1)
    {
      declaration = true;
      if (dwarf_version < 5)
	no_linkage_name = true;
    }

  ultimate_origin = decl_ultimate_origin (decl_or_origin);
  if (decl || ultimate_origin)
    origin = ultimate_origin;
  com_decl = fortran_common (decl_or_origin, &off);

  /* Symbol in common gets emitted as a child of the common block, in the form
     of a data member.  */
  if (com_decl)
    {
      dw_die_ref com_die;
      dw_loc_list_ref loc = NULL;
      die_node com_die_arg;

      var_die = lookup_decl_die (decl_or_origin);
      if (var_die)
	{
	  if (! early_dwarf && get_AT (var_die, DW_AT_location) == NULL)
	    {
	      loc = loc_list_from_tree (com_decl, off ? 1 : 2, NULL);
	      if (loc)
		{
		  if (off)
		    {
		      /* Optimize the common case.  */
		      if (single_element_loc_list_p (loc)
			  && loc->expr->dw_loc_opc == DW_OP_addr
			  && loc->expr->dw_loc_next == NULL
			  && GET_CODE (loc->expr->dw_loc_oprnd1.v.val_addr)
			     == SYMBOL_REF)
			{
			  rtx x = loc->expr->dw_loc_oprnd1.v.val_addr;
			  loc->expr->dw_loc_oprnd1.v.val_addr
			    = plus_constant (GET_MODE (x), x , off);
			}
		      else
			loc_list_plus_const (loc, off);
		    }
		  add_AT_location_description (var_die, DW_AT_location, loc);
		  remove_AT (var_die, DW_AT_declaration);
		}
	    }
	  return;
	}

      if (common_block_die_table == NULL)
	common_block_die_table = hash_table<block_die_hasher>::create_ggc (10);

      com_die_arg.decl_id = DECL_UID (com_decl);
      com_die_arg.die_parent = context_die;
      com_die = common_block_die_table->find (&com_die_arg);
      if (! early_dwarf)
	loc = loc_list_from_tree (com_decl, 2, NULL);
      if (com_die == NULL)
	{
	  const char *cnam
	    = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (com_decl));
	  die_node **slot;

	  com_die = new_die (DW_TAG_common_block, context_die, decl);
	  add_name_and_src_coords_attributes (com_die, com_decl);
	  if (loc)
	    {
	      add_AT_location_description (com_die, DW_AT_location, loc);
	      /* Avoid sharing the same loc descriptor between
		 DW_TAG_common_block and DW_TAG_variable.  */
	      loc = loc_list_from_tree (com_decl, 2, NULL);
	    }
	  else if (DECL_EXTERNAL (decl_or_origin))
	    add_AT_flag (com_die, DW_AT_declaration, 1);
	  if (want_pubnames ())
	    add_pubname_string (cnam, com_die); /* ??? needed? */
	  com_die->decl_id = DECL_UID (com_decl);
	  slot = common_block_die_table->find_slot (com_die, INSERT);
	  *slot = com_die;
	}
      else if (get_AT (com_die, DW_AT_location) == NULL && loc)
	{
	  add_AT_location_description (com_die, DW_AT_location, loc);
	  loc = loc_list_from_tree (com_decl, 2, NULL);
	  remove_AT (com_die, DW_AT_declaration);
	}
      var_die = new_die (DW_TAG_variable, com_die, decl);
      add_name_and_src_coords_attributes (var_die, decl_or_origin);
      add_type_attribute (var_die, TREE_TYPE (decl_or_origin),
			  decl_quals (decl_or_origin), false,
			  context_die);
      add_alignment_attribute (var_die, decl);
      add_AT_flag (var_die, DW_AT_external, 1);
      if (loc)
	{
	  if (off)
	    {
	      /* Optimize the common case.  */
	      if (single_element_loc_list_p (loc)
                  && loc->expr->dw_loc_opc == DW_OP_addr
		  && loc->expr->dw_loc_next == NULL
		  && GET_CODE (loc->expr->dw_loc_oprnd1.v.val_addr) == SYMBOL_REF)
		{
		  rtx x = loc->expr->dw_loc_oprnd1.v.val_addr;
		  loc->expr->dw_loc_oprnd1.v.val_addr
		    = plus_constant (GET_MODE (x), x, off);
		}
	      else
		loc_list_plus_const (loc, off);
	    }
	  add_AT_location_description (var_die, DW_AT_location, loc);
	}
      else if (DECL_EXTERNAL (decl_or_origin))
	add_AT_flag (var_die, DW_AT_declaration, 1);
      if (decl)
	equate_decl_number_to_die (decl, var_die);
      return;
    }

  if (old_die)
    {
      if (declaration)
	{
	  /* A declaration that has been previously dumped, needs no
	     further annotations, since it doesn't need location on
	     the second pass.  */
	  return;
	}
      else if (decl_will_get_specification_p (old_die, decl, declaration)
	       && !get_AT (old_die, DW_AT_specification))
	{
	  /* Fall-thru so we can make a new variable die along with a
	     DW_AT_specification.  */
	}
      else if (origin && old_die->die_parent != context_die)
	{
	  /* If we will be creating an inlined instance, we need a
	     new DIE that will get annotated with
	     DW_AT_abstract_origin.  Clear things so we can get a
	     new DIE.  */
	  gcc_assert (!DECL_ABSTRACT_P (decl));
	  old_die = NULL;
	}
      else
	{
	  /* If a DIE was dumped early, it still needs location info.
	     Skip to where we fill the location bits.  */
	  var_die = old_die;

	  /* ???  In LTRANS we cannot annotate early created variably
	     modified type DIEs without copying them and adjusting all
	     references to them.  Thus we dumped them again, also add a
	     reference to them.  */
	  tree type = TREE_TYPE (decl_or_origin);
	  if (in_lto_p
	      && variably_modified_type_p
		   (type, decl_function_context (decl_or_origin)))
	    {
	      if (decl_by_reference_p (decl_or_origin))
		add_type_attribute (var_die, TREE_TYPE (type),
				    TYPE_UNQUALIFIED, false, context_die);
	      else
		add_type_attribute (var_die, type, decl_quals (decl_or_origin),
				    false, context_die);
	    }

	  goto gen_variable_die_location;
	}
    }

  /* For static data members, the declaration in the class is supposed
     to have DW_TAG_member tag in DWARF{3,4} and we emit it for compatibility
     also in DWARF2; the specification should still be DW_TAG_variable
     referencing the DW_TAG_member DIE.  */
  if (declaration && class_scope_p (context_die) && dwarf_version < 5)
    var_die = new_die (DW_TAG_member, context_die, decl);
  else
    var_die = new_die (DW_TAG_variable, context_die, decl);

  if (origin != NULL)
    add_abstract_origin_attribute (var_die, origin);

  /* Loop unrolling can create multiple blocks that refer to the same
     static variable, so we must test for the DW_AT_declaration flag.

     ??? Loop unrolling/reorder_blocks should perhaps be rewritten to
     copy decls and set the DECL_ABSTRACT_P flag on them instead of
     sharing them.

     ??? Duplicated blocks have been rewritten to use .debug_ranges.  */
  else if (decl_will_get_specification_p (old_die, decl, declaration))
    {
      /* This is a definition of a C++ class level static.  */
      add_AT_specification (var_die, old_die);
      specialization_p = true;
      if (DECL_NAME (decl))
	{
	  expanded_location s = expand_location (DECL_SOURCE_LOCATION (decl));
	  struct dwarf_file_data * file_index = lookup_filename (s.file);

	  if (get_AT_file (old_die, DW_AT_decl_file) != file_index)
	    add_AT_file (var_die, DW_AT_decl_file, file_index);

	  if (get_AT_unsigned (old_die, DW_AT_decl_line) != (unsigned) s.line)
	    add_AT_unsigned (var_die, DW_AT_decl_line, s.line);

	  if (debug_column_info
	      && s.column
	      && (get_AT_unsigned (old_die, DW_AT_decl_column)
		  != (unsigned) s.column))
	    add_AT_unsigned (var_die, DW_AT_decl_column, s.column);

	  if (old_die->die_tag == DW_TAG_member)
	    add_linkage_name (var_die, decl);
	}
    }
  else
    add_name_and_src_coords_attributes (var_die, decl, no_linkage_name);

  if ((origin == NULL && !specialization_p)
      || (origin != NULL
	  && !DECL_ABSTRACT_P (decl_or_origin)
	  && variably_modified_type_p (TREE_TYPE (decl_or_origin),
				       decl_function_context
							(decl_or_origin))))
    {
      tree type = TREE_TYPE (decl_or_origin);

      if (decl_by_reference_p (decl_or_origin))
	add_type_attribute (var_die, TREE_TYPE (type), TYPE_UNQUALIFIED, false,
			    context_die);
      else
	add_type_attribute (var_die, type, decl_quals (decl_or_origin), false,
			    context_die);
    }

  if (origin == NULL && !specialization_p)
    {
      if (TREE_PUBLIC (decl))
	add_AT_flag (var_die, DW_AT_external, 1);

      if (DECL_ARTIFICIAL (decl))
	add_AT_flag (var_die, DW_AT_artificial, 1);

      add_alignment_attribute (var_die, decl);

      add_accessibility_attribute (var_die, decl);
    }

  if (declaration)
    add_AT_flag (var_die, DW_AT_declaration, 1);

  if (decl && (DECL_ABSTRACT_P (decl)
	       || !old_die || is_declaration_die (old_die)))
    equate_decl_number_to_die (decl, var_die);

 gen_variable_die_location:
  if (! declaration
      && (! DECL_ABSTRACT_P (decl_or_origin)
	  /* Local static vars are shared between all clones/inlines,
	     so emit DW_AT_location on the abstract DIE if DECL_RTL is
	     already set.  */
	  || (VAR_P (decl_or_origin)
	      && TREE_STATIC (decl_or_origin)
	      && DECL_RTL_SET_P (decl_or_origin))))
    {
      if (early_dwarf)
	add_pubname (decl_or_origin, var_die);
      else
	add_location_or_const_value_attribute (var_die, decl_or_origin,
					       decl == NULL);
    }
  else
    tree_add_const_value_attribute_for_decl (var_die, decl_or_origin);

  if ((dwarf_version >= 4 || !dwarf_strict)
      && lang_hooks.decls.decl_dwarf_attribute (decl_or_origin,
						DW_AT_const_expr) == 1
      && !get_AT (var_die, DW_AT_const_expr)
      && !specialization_p)
    add_AT_flag (var_die, DW_AT_const_expr, 1);

  if (!dwarf_strict)
    {
      int inl = lang_hooks.decls.decl_dwarf_attribute (decl_or_origin,
						       DW_AT_inline);
      if (inl != -1
	  && !get_AT (var_die, DW_AT_inline)
	  && !specialization_p)
	add_AT_unsigned (var_die, DW_AT_inline, inl);
    }
}

/* Generate a DIE to represent a named constant.  */

static void
gen_const_die (tree decl, dw_die_ref context_die)
{
  dw_die_ref const_die;
  tree type = TREE_TYPE (decl);

  const_die = lookup_decl_die (decl);
  if (const_die)
    return;

  const_die = new_die (DW_TAG_constant, context_die, decl);
  equate_decl_number_to_die (decl, const_die);
  add_name_and_src_coords_attributes (const_die, decl);
  add_type_attribute (const_die, type, TYPE_QUAL_CONST, false, context_die);
  if (TREE_PUBLIC (decl))
    add_AT_flag (const_die, DW_AT_external, 1);
  if (DECL_ARTIFICIAL (decl))
    add_AT_flag (const_die, DW_AT_artificial, 1);
  tree_add_const_value_attribute_for_decl (const_die, decl);
}

/* Generate a DIE to represent a label identifier.  */

static void
gen_label_die (tree decl, dw_die_ref context_die)
{
  tree origin = decl_ultimate_origin (decl);
  dw_die_ref lbl_die = lookup_decl_die (decl);
  rtx insn;
  char label[MAX_ARTIFICIAL_LABEL_BYTES];

  if (!lbl_die)
    {
      lbl_die = new_die (DW_TAG_label, context_die, decl);
      equate_decl_number_to_die (decl, lbl_die);

      if (origin != NULL)
	add_abstract_origin_attribute (lbl_die, origin);
      else
	add_name_and_src_coords_attributes (lbl_die, decl);
    }

  if (DECL_ABSTRACT_P (decl))
    equate_decl_number_to_die (decl, lbl_die);
  else if (! early_dwarf)
    {
      insn = DECL_RTL_IF_SET (decl);

      /* Deleted labels are programmer specified labels which have been
	 eliminated because of various optimizations.  We still emit them
	 here so that it is possible to put breakpoints on them.  */
      if (insn
	  && (LABEL_P (insn)
	      || ((NOTE_P (insn)
	           && NOTE_KIND (insn) == NOTE_INSN_DELETED_LABEL))))
	{
	  /* When optimization is enabled (via -O) some parts of the compiler
	     (e.g. jump.c and cse.c) may try to delete CODE_LABEL insns which
	     represent source-level labels which were explicitly declared by
	     the user.  This really shouldn't be happening though, so catch
	     it if it ever does happen.  */
	  gcc_assert (!as_a<rtx_insn *> (insn)->deleted ());

	  ASM_GENERATE_INTERNAL_LABEL (label, "L", CODE_LABEL_NUMBER (insn));
          add_AT_lbl_id (lbl_die, DW_AT_low_pc, label);
	}
      else if (insn
	       && NOTE_P (insn)
	       && NOTE_KIND (insn) == NOTE_INSN_DELETED_DEBUG_LABEL
	       && CODE_LABEL_NUMBER (insn) != -1)
	{
	  ASM_GENERATE_INTERNAL_LABEL (label, "LDL", CODE_LABEL_NUMBER (insn));
          add_AT_lbl_id (lbl_die, DW_AT_low_pc, label);
	}
    }
}

/* A helper function for gen_inlined_subroutine_die.  Add source coordinate
   attributes to the DIE for a block STMT, to describe where the inlined
   function was called from.  This is similar to add_src_coords_attributes.  */

static inline void
add_call_src_coords_attributes (tree stmt, dw_die_ref die)
{
  expanded_location s = expand_location (BLOCK_SOURCE_LOCATION (stmt));

  if (dwarf_version >= 3 || !dwarf_strict)
    {
      add_AT_file (die, DW_AT_call_file, lookup_filename (s.file));
      add_AT_unsigned (die, DW_AT_call_line, s.line);
      if (debug_column_info && s.column)
	add_AT_unsigned (die, DW_AT_call_column, s.column);
    }
}


/* A helper function for gen_lexical_block_die and gen_inlined_subroutine_die.
   Add low_pc and high_pc attributes to the DIE for a block STMT.  */

static inline void
add_high_low_attributes (tree stmt, dw_die_ref die)
{
  char label[MAX_ARTIFICIAL_LABEL_BYTES];

  if (BLOCK_FRAGMENT_CHAIN (stmt)
      && (dwarf_version >= 3 || !dwarf_strict))
    {
      tree chain, superblock = NULL_TREE;
      dw_die_ref pdie;
      dw_attr_node *attr = NULL;

      if (inlined_function_outer_scope_p (stmt))
	{
	  ASM_GENERATE_INTERNAL_LABEL (label, BLOCK_BEGIN_LABEL,
				       BLOCK_NUMBER (stmt));
          add_AT_lbl_id (die, DW_AT_entry_pc, label);
	}

      /* Optimize duplicate .debug_ranges lists or even tails of
	 lists.  If this BLOCK has same ranges as its supercontext,
	 lookup DW_AT_ranges attribute in the supercontext (and
	 recursively so), verify that the ranges_table contains the
	 right values and use it instead of adding a new .debug_range.  */
      for (chain = stmt, pdie = die;
	   BLOCK_SAME_RANGE (chain);
	   chain = BLOCK_SUPERCONTEXT (chain))
	{
	  dw_attr_node *new_attr;

	  pdie = pdie->die_parent;
	  if (pdie == NULL)
	    break;
	  if (BLOCK_SUPERCONTEXT (chain) == NULL_TREE)
	    break;
	  new_attr = get_AT (pdie, DW_AT_ranges);
	  if (new_attr == NULL
	      || new_attr->dw_attr_val.val_class != dw_val_class_range_list)
	    break;
	  attr = new_attr;
	  superblock = BLOCK_SUPERCONTEXT (chain);
	}
      if (attr != NULL
	  && ((*ranges_table)[attr->dw_attr_val.v.val_offset].num
	      == BLOCK_NUMBER (superblock))
	  && BLOCK_FRAGMENT_CHAIN (superblock))
	{
	  unsigned long off = attr->dw_attr_val.v.val_offset;
	  unsigned long supercnt = 0, thiscnt = 0;
	  for (chain = BLOCK_FRAGMENT_CHAIN (superblock);
	       chain; chain = BLOCK_FRAGMENT_CHAIN (chain))
	    {
	      ++supercnt;
	      gcc_checking_assert ((*ranges_table)[off + supercnt].num
				   == BLOCK_NUMBER (chain));
	    }
	  gcc_checking_assert ((*ranges_table)[off + supercnt + 1].num == 0);
	  for (chain = BLOCK_FRAGMENT_CHAIN (stmt);
	       chain; chain = BLOCK_FRAGMENT_CHAIN (chain))
	    ++thiscnt;
	  gcc_assert (supercnt >= thiscnt);
	  add_AT_range_list (die, DW_AT_ranges, off + supercnt - thiscnt,
			     false);
	  note_rnglist_head (off + supercnt - thiscnt);
	  return;
	}

      unsigned int offset = add_ranges (stmt, true);
      add_AT_range_list (die, DW_AT_ranges, offset, false);
      note_rnglist_head (offset);

      bool prev_in_cold = BLOCK_IN_COLD_SECTION_P (stmt);
      chain = BLOCK_FRAGMENT_CHAIN (stmt);
      do
	{
	  add_ranges (chain, prev_in_cold != BLOCK_IN_COLD_SECTION_P (chain));
	  prev_in_cold = BLOCK_IN_COLD_SECTION_P (chain);
	  chain = BLOCK_FRAGMENT_CHAIN (chain);
	}
      while (chain);
      add_ranges (NULL);
    }
  else
    {
      char label_high[MAX_ARTIFICIAL_LABEL_BYTES];
      ASM_GENERATE_INTERNAL_LABEL (label, BLOCK_BEGIN_LABEL,
				   BLOCK_NUMBER (stmt));
      ASM_GENERATE_INTERNAL_LABEL (label_high, BLOCK_END_LABEL,
				   BLOCK_NUMBER (stmt));
      add_AT_low_high_pc (die, label, label_high, false);
    }
}

/* Generate a DIE for a lexical block.  */

static void
gen_lexical_block_die (tree stmt, dw_die_ref context_die)
{
  dw_die_ref old_die = BLOCK_DIE (stmt);
  dw_die_ref stmt_die = NULL;
  if (!old_die)
    {
      stmt_die = new_die (DW_TAG_lexical_block, context_die, stmt);
      BLOCK_DIE (stmt) = stmt_die;
    }

  if (BLOCK_ABSTRACT (stmt))
    {
      if (old_die)
	{
	  /* This must have been generated early and it won't even
	     need location information since it's a DW_AT_inline
	     function.  */
	  if (flag_checking)
	    for (dw_die_ref c = context_die; c; c = c->die_parent)
	      if (c->die_tag == DW_TAG_inlined_subroutine
		  || c->die_tag == DW_TAG_subprogram)
		{
		  gcc_assert (get_AT (c, DW_AT_inline));
		  break;
		}
	  return;
	}
    }
  else if (BLOCK_ABSTRACT_ORIGIN (stmt))
    {
      /* If this is an inlined instance, create a new lexical die for
	 anything below to attach DW_AT_abstract_origin to.  */
      if (old_die)
	{
	  stmt_die = new_die (DW_TAG_lexical_block, context_die, stmt);
	  BLOCK_DIE (stmt) = stmt_die;
	  old_die = NULL;
	}

      tree origin = block_ultimate_origin (stmt);
      if (origin != NULL_TREE && origin != stmt)
	add_abstract_origin_attribute (stmt_die, origin);
    }

  if (old_die)
    stmt_die = old_die;

  /* A non abstract block whose blocks have already been reordered
     should have the instruction range for this block.  If so, set the
     high/low attributes.  */
  if (!early_dwarf && !BLOCK_ABSTRACT (stmt) && TREE_ASM_WRITTEN (stmt))
    {
      gcc_assert (stmt_die);
      add_high_low_attributes (stmt, stmt_die);
    }

  decls_for_scope (stmt, stmt_die);
}

/* Generate a DIE for an inlined subprogram.  */

static void
gen_inlined_subroutine_die (tree stmt, dw_die_ref context_die)
{
  tree decl;

  /* The instance of function that is effectively being inlined shall not
     be abstract.  */
  gcc_assert (! BLOCK_ABSTRACT (stmt));

  decl = block_ultimate_origin (stmt);

  /* Make sure any inlined functions are known to be inlineable.  */
  gcc_checking_assert (DECL_ABSTRACT_P (decl)
		       || cgraph_function_possibly_inlined_p (decl));

  if (! BLOCK_ABSTRACT (stmt))
    {
      dw_die_ref subr_die
	= new_die (DW_TAG_inlined_subroutine, context_die, stmt);

      if (call_arg_locations)
	BLOCK_DIE (stmt) = subr_die;
      add_abstract_origin_attribute (subr_die, decl);
      if (TREE_ASM_WRITTEN (stmt))
        add_high_low_attributes (stmt, subr_die);
      add_call_src_coords_attributes (stmt, subr_die);

      decls_for_scope (stmt, subr_die);
    }
}

/* Generate a DIE for a field in a record, or structure.  CTX is required: see
   the comment for VLR_CONTEXT.  */

static void
gen_field_die (tree decl, struct vlr_context *ctx, dw_die_ref context_die)
{
  dw_die_ref decl_die;

  if (TREE_TYPE (decl) == error_mark_node)
    return;

  decl_die = new_die (DW_TAG_member, context_die, decl);
  add_name_and_src_coords_attributes (decl_die, decl);
  add_type_attribute (decl_die, member_declared_type (decl), decl_quals (decl),
		      TYPE_REVERSE_STORAGE_ORDER (DECL_FIELD_CONTEXT (decl)),
		      context_die);

  if (DECL_BIT_FIELD_TYPE (decl))
    {
      add_byte_size_attribute (decl_die, decl);
      add_bit_size_attribute (decl_die, decl);
      add_bit_offset_attribute (decl_die, decl, ctx);
    }

  add_alignment_attribute (decl_die, decl);

  /* If we have a variant part offset, then we are supposed to process a member
     of a QUAL_UNION_TYPE, which is how we represent variant parts in
     trees.  */
  gcc_assert (ctx->variant_part_offset == NULL_TREE
	      || TREE_CODE (DECL_FIELD_CONTEXT (decl)) != QUAL_UNION_TYPE);
  if (TREE_CODE (DECL_FIELD_CONTEXT (decl)) != UNION_TYPE)
    add_data_member_location_attribute (decl_die, decl, ctx);

  if (DECL_ARTIFICIAL (decl))
    add_AT_flag (decl_die, DW_AT_artificial, 1);

  add_accessibility_attribute (decl_die, decl);

  /* Equate decl number to die, so that we can look up this decl later on.  */
  equate_decl_number_to_die (decl, decl_die);
}

/* Generate a DIE for a pointer to a member type.  TYPE can be an
   OFFSET_TYPE, for a pointer to data member, or a RECORD_TYPE, for a
   pointer to member function.  */

static void
gen_ptr_to_mbr_type_die (tree type, dw_die_ref context_die)
{
  if (lookup_type_die (type))
    return;

  dw_die_ref ptr_die = new_die (DW_TAG_ptr_to_member_type,
				scope_die_for (type, context_die), type);

  equate_type_number_to_die (type, ptr_die);
  add_AT_die_ref (ptr_die, DW_AT_containing_type,
		  lookup_type_die (TYPE_OFFSET_BASETYPE (type)));
  add_type_attribute (ptr_die, TREE_TYPE (type), TYPE_UNQUALIFIED, false,
		      context_die);
  add_alignment_attribute (ptr_die, type);

  if (TREE_CODE (TREE_TYPE (type)) != FUNCTION_TYPE
      && TREE_CODE (TREE_TYPE (type)) != METHOD_TYPE)
    {
      dw_loc_descr_ref op = new_loc_descr (DW_OP_plus, 0, 0);
      add_AT_loc (ptr_die, DW_AT_use_location, op);
    }
}

static char *producer_string;

/* Return a heap allocated producer string including command line options
   if -grecord-gcc-switches.  */

static char *
gen_producer_string (void)
{
  size_t j;
  auto_vec<const char *> switches;
  const char *language_string = lang_hooks.name;
  char *producer, *tail;
  const char *p;
  size_t len = dwarf_record_gcc_switches ? 0 : 3;
  size_t plen = strlen (language_string) + 1 + strlen (version_string);

  for (j = 1; dwarf_record_gcc_switches && j < save_decoded_options_count; j++)
    switch (save_decoded_options[j].opt_index)
      {
      case OPT_o:
      case OPT_d:
      case OPT_dumpbase:
      case OPT_dumpdir:
      case OPT_auxbase:
      case OPT_auxbase_strip:
      case OPT_quiet:
      case OPT_version:
      case OPT_v:
      case OPT_w:
      case OPT_L:
      case OPT_D:
      case OPT_I:
      case OPT_U:
      case OPT_SPECIAL_unknown:
      case OPT_SPECIAL_ignore:
      case OPT_SPECIAL_program_name:
      case OPT_SPECIAL_input_file:
      case OPT_grecord_gcc_switches:
      case OPT_gno_record_gcc_switches:
      case OPT__output_pch_:
      case OPT_fdiagnostics_show_location_:
      case OPT_fdiagnostics_show_option:
      case OPT_fdiagnostics_show_caret:
      case OPT_fdiagnostics_color_:
      case OPT_fverbose_asm:
      case OPT____:
      case OPT__sysroot_:
      case OPT_nostdinc:
      case OPT_nostdinc__:
      case OPT_fpreprocessed:
      case OPT_fltrans_output_list_:
      case OPT_fresolution_:
      case OPT_fdebug_prefix_map_:
	/* Ignore these.  */
	continue;
      default:
        if (cl_options[save_decoded_options[j].opt_index].flags
	    & CL_NO_DWARF_RECORD)
	  continue;
        gcc_checking_assert (save_decoded_options[j].canonical_option[0][0]
			     == '-');
        switch (save_decoded_options[j].canonical_option[0][1])
	  {
	  case 'M':
	  case 'i':
	  case 'W':
	    continue;
	  case 'f':
	    if (strncmp (save_decoded_options[j].canonical_option[0] + 2,
			 "dump", 4) == 0)
	      continue;
	    break;
	  default:
	    break;
	  }
	switches.safe_push (save_decoded_options[j].orig_option_with_args_text);
	len += strlen (save_decoded_options[j].orig_option_with_args_text) + 1;
	break;
      }

  producer = XNEWVEC (char, plen + 1 + len + 1);
  tail = producer;
  sprintf (tail, "%s %s", language_string, version_string);
  tail += plen;

  FOR_EACH_VEC_ELT (switches, j, p)
    {
      len = strlen (p);
      *tail = ' ';
      memcpy (tail + 1, p, len);
      tail += len + 1;
    }

  *tail = '\0';
  return producer;
}

/* Given a C and/or C++ language/version string return the "highest".
   C++ is assumed to be "higher" than C in this case.  Used for merging
   LTO translation unit languages.  */
static const char *
highest_c_language (const char *lang1, const char *lang2)
{
  if (strcmp ("GNU C++14", lang1) == 0 || strcmp ("GNU C++14", lang2) == 0)
    return "GNU C++14";
  if (strcmp ("GNU C++11", lang1) == 0 || strcmp ("GNU C++11", lang2) == 0)
    return "GNU C++11";
  if (strcmp ("GNU C++98", lang1) == 0 || strcmp ("GNU C++98", lang2) == 0)
    return "GNU C++98";

  if (strcmp ("GNU C11", lang1) == 0 || strcmp ("GNU C11", lang2) == 0)
    return "GNU C11";
  if (strcmp ("GNU C99", lang1) == 0 || strcmp ("GNU C99", lang2) == 0)
    return "GNU C99";
  if (strcmp ("GNU C89", lang1) == 0 || strcmp ("GNU C89", lang2) == 0)
    return "GNU C89";

  gcc_unreachable ();
}


/* Generate the DIE for the compilation unit.  */

static dw_die_ref
gen_compile_unit_die (const char *filename)
{
  dw_die_ref die;
  const char *language_string = lang_hooks.name;
  int language;

  die = new_die (DW_TAG_compile_unit, NULL, NULL);

  if (filename)
    {
      add_name_attribute (die, filename);
      /* Don't add cwd for <built-in>.  */
      if (filename[0] != '<')
	add_comp_dir_attribute (die);
    }

  add_AT_string (die, DW_AT_producer, producer_string ? producer_string : "");

  /* If our producer is LTO try to figure out a common language to use
     from the global list of translation units.  */
  if (strcmp (language_string, "GNU GIMPLE") == 0)
    {
      unsigned i;
      tree t;
      const char *common_lang = NULL;

      FOR_EACH_VEC_SAFE_ELT (all_translation_units, i, t)
	{
	  if (!TRANSLATION_UNIT_LANGUAGE (t))
	    continue;
	  if (!common_lang)
	    common_lang = TRANSLATION_UNIT_LANGUAGE (t);
	  else if (strcmp (common_lang, TRANSLATION_UNIT_LANGUAGE (t)) == 0)
	    ;
	  else if (strncmp (common_lang, "GNU C", 5) == 0
		    && strncmp (TRANSLATION_UNIT_LANGUAGE (t), "GNU C", 5) == 0)
	    /* Mixing C and C++ is ok, use C++ in that case.  */
	    common_lang = highest_c_language (common_lang,
					      TRANSLATION_UNIT_LANGUAGE (t));
	  else
	    {
	      /* Fall back to C.  */
	      common_lang = NULL;
	      break;
	    }
	}

      if (common_lang)
	language_string = common_lang;
    }

  language = DW_LANG_C;
  if (strncmp (language_string, "GNU C", 5) == 0
      && ISDIGIT (language_string[5]))
    {
      language = DW_LANG_C89;
      if (dwarf_version >= 3 || !dwarf_strict)
	{
	  if (strcmp (language_string, "GNU C89") != 0)
	    language = DW_LANG_C99;

	  if (dwarf_version >= 5 /* || !dwarf_strict */)
	    if (strcmp (language_string, "GNU C11") == 0)
	      language = DW_LANG_C11;
	}
    }
  else if (strncmp (language_string, "GNU C++", 7) == 0)
    {
      language = DW_LANG_C_plus_plus;
      if (dwarf_version >= 5 /* || !dwarf_strict */)
	{
	  if (strcmp (language_string, "GNU C++11") == 0)
	    language = DW_LANG_C_plus_plus_11;
	  else if (strcmp (language_string, "GNU C++14") == 0)
	    language = DW_LANG_C_plus_plus_14;
	}
    }
  else if (strcmp (language_string, "GNU F77") == 0)
    language = DW_LANG_Fortran77;
  else if (dwarf_version >= 3 || !dwarf_strict)
    {
      if (strcmp (language_string, "GNU Ada") == 0)
	language = DW_LANG_Ada95;
      else if (strncmp (language_string, "GNU Fortran", 11) == 0)
	{
	  language = DW_LANG_Fortran95;
	  if (dwarf_version >= 5 /* || !dwarf_strict */)
	    {
	      if (strcmp (language_string, "GNU Fortran2003") == 0)
		language = DW_LANG_Fortran03;
	      else if (strcmp (language_string, "GNU Fortran2008") == 0)
		language = DW_LANG_Fortran08;
	    }
	}
      else if (strcmp (language_string, "GNU Objective-C") == 0)
	language = DW_LANG_ObjC;
      else if (strcmp (language_string, "GNU Objective-C++") == 0)
	language = DW_LANG_ObjC_plus_plus;
      else if (dwarf_version >= 5 || !dwarf_strict)
	{
	  if (strcmp (language_string, "GNU Go") == 0)
	    language = DW_LANG_Go;
	}
    }
  /* Use a degraded Fortran setting in strict DWARF2 so is_fortran works.  */
  else if (strncmp (language_string, "GNU Fortran", 11) == 0)
    language = DW_LANG_Fortran90;

  add_AT_unsigned (die, DW_AT_language, language);

  switch (language)
    {
    case DW_LANG_Fortran77:
    case DW_LANG_Fortran90:
    case DW_LANG_Fortran95:
    case DW_LANG_Fortran03:
    case DW_LANG_Fortran08:
      /* Fortran has case insensitive identifiers and the front-end
	 lowercases everything.  */
      add_AT_unsigned (die, DW_AT_identifier_case, DW_ID_down_case);
      break;
    default:
      /* The default DW_ID_case_sensitive doesn't need to be specified.  */
      break;
    }
  return die;
}

/* Generate the DIE for a base class.  */

static void
gen_inheritance_die (tree binfo, tree access, tree type,
		     dw_die_ref context_die)
{
  dw_die_ref die = new_die (DW_TAG_inheritance, context_die, binfo);
  struct vlr_context ctx = { type, NULL };

  add_type_attribute (die, BINFO_TYPE (binfo), TYPE_UNQUALIFIED, false,
		      context_die);
  add_data_member_location_attribute (die, binfo, &ctx);

  if (BINFO_VIRTUAL_P (binfo))
    add_AT_unsigned (die, DW_AT_virtuality, DW_VIRTUALITY_virtual);

  /* In DWARF3+ the default is DW_ACCESS_private only in DW_TAG_class_type
     children, otherwise the default is DW_ACCESS_public.  In DWARF2
     the default has always been DW_ACCESS_private.  */
  if (access == access_public_node)
    {
      if (dwarf_version == 2
	  || context_die->die_tag == DW_TAG_class_type)
      add_AT_unsigned (die, DW_AT_accessibility, DW_ACCESS_public);
    }
  else if (access == access_protected_node)
    add_AT_unsigned (die, DW_AT_accessibility, DW_ACCESS_protected);
  else if (dwarf_version > 2
	   && context_die->die_tag != DW_TAG_class_type)
    add_AT_unsigned (die, DW_AT_accessibility, DW_ACCESS_private);
}

/* Return whether DECL is a FIELD_DECL that represents the variant part of a
   structure.  */
static bool
is_variant_part (tree decl)
{
  return (TREE_CODE (decl) == FIELD_DECL
	  && TREE_CODE (TREE_TYPE (decl)) == QUAL_UNION_TYPE);
}

/* Check that OPERAND is a reference to a field in STRUCT_TYPE.  If it is,
   return the FIELD_DECL.  Return NULL_TREE otherwise.  */

static tree
analyze_discr_in_predicate (tree operand, tree struct_type)
{
  bool continue_stripping = true;
  while (continue_stripping)
    switch (TREE_CODE (operand))
      {
      CASE_CONVERT:
	operand = TREE_OPERAND (operand, 0);
	break;
      default:
	continue_stripping = false;
	break;
      }

  /* Match field access to members of struct_type only.  */
  if (TREE_CODE (operand) == COMPONENT_REF
      && TREE_CODE (TREE_OPERAND (operand, 0)) == PLACEHOLDER_EXPR
      && TREE_TYPE (TREE_OPERAND (operand, 0)) == struct_type
      && TREE_CODE (TREE_OPERAND (operand, 1)) == FIELD_DECL)
    return TREE_OPERAND (operand, 1);
  else
    return NULL_TREE;
}

/* Check that SRC is a constant integer that can be represented as a native
   integer constant (either signed or unsigned).  If so, store it into DEST and
   return true.  Return false otherwise. */

static bool
get_discr_value (tree src, dw_discr_value *dest)
{
  tree discr_type = TREE_TYPE (src);

  if (lang_hooks.types.get_debug_type)
    {
      tree debug_type = lang_hooks.types.get_debug_type (discr_type);
      if (debug_type != NULL)
	discr_type = debug_type;
    }

  if (TREE_CODE (src) != INTEGER_CST || !INTEGRAL_TYPE_P (discr_type))
    return false;

  /* Signedness can vary between the original type and the debug type. This
     can happen for character types in Ada for instance: the character type
     used for code generation can be signed, to be compatible with the C one,
     but from a debugger point of view, it must be unsigned.  */
  bool is_orig_unsigned = TYPE_UNSIGNED (TREE_TYPE (src));
  bool is_debug_unsigned = TYPE_UNSIGNED (discr_type);

  if (is_orig_unsigned != is_debug_unsigned)
    src = fold_convert (discr_type, src);

  if (!(is_debug_unsigned ? tree_fits_uhwi_p (src) : tree_fits_shwi_p (src)))
    return false;

  dest->pos = is_debug_unsigned;
  if (is_debug_unsigned)
    dest->v.uval = tree_to_uhwi (src);
  else
    dest->v.sval = tree_to_shwi (src);

  return true;
}

/* Try to extract synthetic properties out of VARIANT_PART_DECL, which is a
   FIELD_DECL in STRUCT_TYPE that represents a variant part.  If unsuccessful,
   store NULL_TREE in DISCR_DECL.  Otherwise:

     - store the discriminant field in STRUCT_TYPE that controls the variant
       part to *DISCR_DECL

     - put in *DISCR_LISTS_P an array where for each variant, the item
       represents the corresponding matching list of discriminant values.

     - put in *DISCR_LISTS_LENGTH the number of variants, which is the size of
       the above array.

   Note that when the array is allocated (i.e. when the analysis is
   successful), it is up to the caller to free the array.  */

static void
analyze_variants_discr (tree variant_part_decl,
			tree struct_type,
			tree *discr_decl,
			dw_discr_list_ref **discr_lists_p,
			unsigned *discr_lists_length)
{
  tree variant_part_type = TREE_TYPE (variant_part_decl);
  tree variant;
  dw_discr_list_ref *discr_lists;
  unsigned i;

  /* Compute how many variants there are in this variant part.  */
  *discr_lists_length = 0;
  for (variant = TYPE_FIELDS (variant_part_type);
       variant != NULL_TREE;
       variant = DECL_CHAIN (variant))
    ++*discr_lists_length;

  *discr_decl = NULL_TREE;
  *discr_lists_p
    = (dw_discr_list_ref *) xcalloc (*discr_lists_length,
				     sizeof (**discr_lists_p));
  discr_lists = *discr_lists_p;

  /* And then analyze all variants to extract discriminant information for all
     of them.  This analysis is conservative: as soon as we detect something we
     do not support, abort everything and pretend we found nothing.  */
  for (variant = TYPE_FIELDS (variant_part_type), i = 0;
       variant != NULL_TREE;
       variant = DECL_CHAIN (variant), ++i)
    {
      tree match_expr = DECL_QUALIFIER (variant);

      /* Now, try to analyze the predicate and deduce a discriminant for
	 it.  */
      if (match_expr == boolean_true_node)
	/* Typically happens for the default variant: it matches all cases that
	   previous variants rejected.  Don't output any matching value for
	   this one.  */
	continue;

      /* The following loop tries to iterate over each discriminant
	 possibility: single values or ranges.  */
      while (match_expr != NULL_TREE)
	{
	  tree next_round_match_expr;
	  tree candidate_discr = NULL_TREE;
	  dw_discr_list_ref new_node = NULL;

	  /* Possibilities are matched one after the other by nested
	     TRUTH_ORIF_EXPR expressions.  Process the current possibility and
	     continue with the rest at next iteration.  */
	  if (TREE_CODE (match_expr) == TRUTH_ORIF_EXPR)
	    {
	      next_round_match_expr = TREE_OPERAND (match_expr, 0);
	      match_expr = TREE_OPERAND (match_expr, 1);
	    }
	  else
	    next_round_match_expr = NULL_TREE;

	  if (match_expr == boolean_false_node)
	    /* This sub-expression matches nothing: just wait for the next
	       one.  */
	    ;

	  else if (TREE_CODE (match_expr) == EQ_EXPR)
	    {
	      /* We are matching:  <discr_field> == <integer_cst>
		 This sub-expression matches a single value.  */
	      tree integer_cst = TREE_OPERAND (match_expr, 1);

	      candidate_discr
	       = analyze_discr_in_predicate (TREE_OPERAND (match_expr, 0),
					     struct_type);

	      new_node = ggc_cleared_alloc<dw_discr_list_node> ();
	      if (!get_discr_value (integer_cst,
				    &new_node->dw_discr_lower_bound))
		goto abort;
	      new_node->dw_discr_range = false;
	    }

	  else if (TREE_CODE (match_expr) == TRUTH_ANDIF_EXPR)
	    {
	      /* We are matching:
		   <discr_field> > <integer_cst>
		   && <discr_field> < <integer_cst>.
		 This sub-expression matches the range of values between the
		 two matched integer constants.  Note that comparisons can be
		 inclusive or exclusive.  */
	      tree candidate_discr_1, candidate_discr_2;
	      tree lower_cst, upper_cst;
	      bool lower_cst_included, upper_cst_included;
	      tree lower_op = TREE_OPERAND (match_expr, 0);
	      tree upper_op = TREE_OPERAND (match_expr, 1);

	      /* When the comparison is exclusive, the integer constant is not
		 the discriminant range bound we are looking for: we will have
		 to increment or decrement it.  */
	      if (TREE_CODE (lower_op) == GE_EXPR)
		lower_cst_included = true;
	      else if (TREE_CODE (lower_op) == GT_EXPR)
		lower_cst_included = false;
	      else
		goto abort;

	      if (TREE_CODE (upper_op) == LE_EXPR)
		upper_cst_included = true;
	      else if (TREE_CODE (upper_op) == LT_EXPR)
		upper_cst_included = false;
	      else
		goto abort;

	      /* Extract the discriminant from the first operand and check it
		 is consistant with the same analysis in the second
		 operand.  */
	      candidate_discr_1
	        = analyze_discr_in_predicate (TREE_OPERAND (lower_op, 0),
					      struct_type);
	      candidate_discr_2
	        = analyze_discr_in_predicate (TREE_OPERAND (upper_op, 0),
					      struct_type);
	      if (candidate_discr_1 == candidate_discr_2)
		candidate_discr = candidate_discr_1;
	      else
		goto abort;

	      /* Extract bounds from both.  */
	      new_node = ggc_cleared_alloc<dw_discr_list_node> ();
	      lower_cst = TREE_OPERAND (lower_op, 1);
	      upper_cst = TREE_OPERAND (upper_op, 1);

	      if (!lower_cst_included)
		lower_cst
		  = fold_build2 (PLUS_EXPR, TREE_TYPE (lower_cst), lower_cst,
				 build_int_cst (TREE_TYPE (lower_cst), 1));
	      if (!upper_cst_included)
		upper_cst
		  = fold_build2 (MINUS_EXPR, TREE_TYPE (upper_cst), upper_cst,
				 build_int_cst (TREE_TYPE (upper_cst), 1));

	      if (!get_discr_value (lower_cst,
				    &new_node->dw_discr_lower_bound)
		  || !get_discr_value (upper_cst,
				       &new_node->dw_discr_upper_bound))
		goto abort;

	      new_node->dw_discr_range = true;
	    }

	  else
	    /* Unsupported sub-expression: we cannot determine the set of
	       matching discriminant values.  Abort everything.  */
	    goto abort;

	  /* If the discriminant info is not consistant with what we saw so
	     far, consider the analysis failed and abort everything.  */
	  if (candidate_discr == NULL_TREE
	      || (*discr_decl != NULL_TREE && candidate_discr != *discr_decl))
	    goto abort;
	  else
	    *discr_decl = candidate_discr;

	  if (new_node != NULL)
	    {
	      new_node->dw_discr_next = discr_lists[i];
	      discr_lists[i] = new_node;
	    }
	  match_expr = next_round_match_expr;
	}
    }

  /* If we reach this point, we could match everything we were interested
     in.  */
  return;

abort:
  /* Clean all data structure and return no result.  */
  free (*discr_lists_p);
  *discr_lists_p = NULL;
  *discr_decl = NULL_TREE;
}

/* Generate a DIE to represent VARIANT_PART_DECL, a variant part that is part
   of STRUCT_TYPE, a record type.  This new DIE is emitted as the next child
   under CONTEXT_DIE.

   Variant parts are supposed to be implemented as a FIELD_DECL whose type is a
   QUAL_UNION_TYPE: this is the VARIANT_PART_DECL parameter.  The members for
   this type, which are record types, represent the available variants and each
   has a DECL_QUALIFIER attribute.  The discriminant and the discriminant
   values are inferred from these attributes.

   In trees, the offsets for the fields inside these sub-records are relative
   to the variant part itself, whereas the corresponding DIEs should have
   offset attributes that are relative to the embedding record base address.
   This is why the caller must provide a VARIANT_PART_OFFSET expression: it
   must be an expression that computes the offset of the variant part to
   describe in DWARF.  */

static void
gen_variant_part (tree variant_part_decl, struct vlr_context *vlr_ctx,
		  dw_die_ref context_die)
{
  const tree variant_part_type = TREE_TYPE (variant_part_decl);
  tree variant_part_offset = vlr_ctx->variant_part_offset;
  struct loc_descr_context ctx = {
    vlr_ctx->struct_type, /* context_type */
    NULL_TREE,		  /* base_decl */
    NULL,		  /* dpi */
    false,		  /* placeholder_arg */
    false		  /* placeholder_seen */
  };

  /* The FIELD_DECL node in STRUCT_TYPE that acts as the discriminant, or
     NULL_TREE if there is no such field.  */
  tree discr_decl = NULL_TREE;
  dw_discr_list_ref *discr_lists;
  unsigned discr_lists_length = 0;
  unsigned i;

  dw_die_ref dwarf_proc_die = NULL;
  dw_die_ref variant_part_die
    = new_die (DW_TAG_variant_part, context_die, variant_part_type);

  equate_decl_number_to_die (variant_part_decl, variant_part_die);

  analyze_variants_discr (variant_part_decl, vlr_ctx->struct_type,
			  &discr_decl, &discr_lists, &discr_lists_length);

  if (discr_decl != NULL_TREE)
    {
      dw_die_ref discr_die = lookup_decl_die (discr_decl);

      if (discr_die)
	add_AT_die_ref (variant_part_die, DW_AT_discr, discr_die);
      else
	/* We have no DIE for the discriminant, so just discard all
	   discrimimant information in the output.  */
	discr_decl = NULL_TREE;
    }

  /* If the offset for this variant part is more complex than a constant,
     create a DWARF procedure for it so that we will not have to generate DWARF
     expressions for it for each member.  */
  if (TREE_CODE (variant_part_offset) != INTEGER_CST
      && (dwarf_version >= 3 || !dwarf_strict))
    {
      const tree dwarf_proc_fndecl
        = build_decl (UNKNOWN_LOCATION, FUNCTION_DECL, NULL_TREE,
		      build_function_type (TREE_TYPE (variant_part_offset),
					   NULL_TREE));
      const tree dwarf_proc_call = build_call_expr (dwarf_proc_fndecl, 0);
      const dw_loc_descr_ref dwarf_proc_body
        = loc_descriptor_from_tree (variant_part_offset, 0, &ctx);

      dwarf_proc_die = new_dwarf_proc_die (dwarf_proc_body,
					   dwarf_proc_fndecl, context_die);
      if (dwarf_proc_die != NULL)
	variant_part_offset = dwarf_proc_call;
    }

  /* Output DIEs for all variants.  */
  i = 0;
  for (tree variant = TYPE_FIELDS (variant_part_type);
       variant != NULL_TREE;
       variant = DECL_CHAIN (variant), ++i)
    {
      tree variant_type = TREE_TYPE (variant);
      dw_die_ref variant_die;

      /* All variants (i.e. members of a variant part) are supposed to be
	 encoded as structures.  Sub-variant parts are QUAL_UNION_TYPE fields
	 under these records.  */
      gcc_assert (TREE_CODE (variant_type) == RECORD_TYPE);

      variant_die = new_die (DW_TAG_variant, variant_part_die, variant_type);
      equate_decl_number_to_die (variant, variant_die);

      /* Output discriminant values this variant matches, if any.  */
      if (discr_decl == NULL || discr_lists[i] == NULL)
	/* In the case we have discriminant information at all, this is
	   probably the default variant: as the standard says, don't
	   output any discriminant value/list attribute.  */
	;
      else if (discr_lists[i]->dw_discr_next == NULL
	       && !discr_lists[i]->dw_discr_range)
	/* If there is only one accepted value, don't bother outputting a
	   list.  */
	add_discr_value (variant_die, &discr_lists[i]->dw_discr_lower_bound);
      else
	add_discr_list (variant_die, discr_lists[i]);

      for (tree member = TYPE_FIELDS (variant_type);
	   member != NULL_TREE;
	   member = DECL_CHAIN (member))
	{
	  struct vlr_context vlr_sub_ctx = {
	    vlr_ctx->struct_type, /* struct_type */
	    NULL		  /* variant_part_offset */
	  };
	  if (is_variant_part (member))
	    {
	      /* All offsets for fields inside variant parts are relative to
		 the top-level embedding RECORD_TYPE's base address.  On the
		 other hand, offsets in GCC's types are relative to the
		 nested-most variant part.  So we have to sum offsets each time
		 we recurse.  */

	      vlr_sub_ctx.variant_part_offset
		= fold_build2 (PLUS_EXPR, TREE_TYPE (variant_part_offset),
			       variant_part_offset, byte_position (member));
	      gen_variant_part (member, &vlr_sub_ctx, variant_die);
	    }
	  else
	    {
	      vlr_sub_ctx.variant_part_offset = variant_part_offset;
	      gen_decl_die (member, NULL, &vlr_sub_ctx, variant_die);
	    }
	}
    }

  free (discr_lists);
}

/* Generate a DIE for a class member.  */

static void
gen_member_die (tree type, dw_die_ref context_die)
{
  tree member;
  tree binfo = TYPE_BINFO (type);

  gcc_assert (TYPE_MAIN_VARIANT (type) == type);

  /* If this is not an incomplete type, output descriptions of each of its
     members. Note that as we output the DIEs necessary to represent the
     members of this record or union type, we will also be trying to output
     DIEs to represent the *types* of those members. However the `type'
     function (above) will specifically avoid generating type DIEs for member
     types *within* the list of member DIEs for this (containing) type except
     for those types (of members) which are explicitly marked as also being
     members of this (containing) type themselves.  The g++ front- end can
     force any given type to be treated as a member of some other (containing)
     type by setting the TYPE_CONTEXT of the given (member) type to point to
     the TREE node representing the appropriate (containing) type.  */

  /* First output info about the base classes.  */
  if (binfo)
    {
      vec<tree, va_gc> *accesses = BINFO_BASE_ACCESSES (binfo);
      int i;
      tree base;

      for (i = 0; BINFO_BASE_ITERATE (binfo, i, base); i++)
	gen_inheritance_die (base,
			     (accesses ? (*accesses)[i] : access_public_node),
			     type,
			     context_die);
    }

  /* Now output info about the data members and type members.  */
  for (member = TYPE_FIELDS (type); member; member = DECL_CHAIN (member))
    {
      struct vlr_context vlr_ctx = { type, NULL_TREE };
      bool static_inline_p
	= (TREE_STATIC (member)
	   && (lang_hooks.decls.decl_dwarf_attribute (member, DW_AT_inline)
	       != -1));

      /* Ignore clones.  */
      if (DECL_ABSTRACT_ORIGIN (member))
	continue;

      /* If we thought we were generating minimal debug info for TYPE
	 and then changed our minds, some of the member declarations
	 may have already been defined.  Don't define them again, but
	 do put them in the right order.  */

      if (dw_die_ref child = lookup_decl_die (member))
	{
	  /* Handle inline static data members, which only have in-class
	     declarations.  */
	  dw_die_ref ref = NULL; 
	  if (child->die_tag == DW_TAG_variable
	      && child->die_parent == comp_unit_die ())
	    {
	      ref = get_AT_ref (child, DW_AT_specification);
	      /* For C++17 inline static data members followed by redundant
		 out of class redeclaration, we might get here with
		 child being the DIE created for the out of class
		 redeclaration and with its DW_AT_specification being
		 the DIE created for in-class definition.  We want to
		 reparent the latter, and don't want to create another
		 DIE with DW_AT_specification in that case, because
		 we already have one.  */
	      if (ref
		  && static_inline_p
		  && ref->die_tag == DW_TAG_variable
		  && ref->die_parent == comp_unit_die ()
		  && get_AT (ref, DW_AT_specification) == NULL)
		{
		  child = ref;
		  ref = NULL;
		  static_inline_p = false;
		}
	    }

	  if (child->die_tag == DW_TAG_variable
	      && child->die_parent == comp_unit_die ()
	      && ref == NULL)
	    {
	      reparent_child (child, context_die);
	      if (dwarf_version < 5)
		child->die_tag = DW_TAG_member;
	    }
	  else
	    splice_child_die (context_die, child);
	}

      /* Do not generate standard DWARF for variant parts if we are generating
	 the corresponding GNAT encodings: DIEs generated for both would
	 conflict in our mappings.  */
      else if (is_variant_part (member)
	       && gnat_encodings == DWARF_GNAT_ENCODINGS_MINIMAL)
	{
	  vlr_ctx.variant_part_offset = byte_position (member);
	  gen_variant_part (member, &vlr_ctx, context_die);
	}
      else
	{
	  vlr_ctx.variant_part_offset = NULL_TREE;
	  gen_decl_die (member, NULL, &vlr_ctx, context_die);
	}

      /* For C++ inline static data members emit immediately a DW_TAG_variable
	 DIE that will refer to that DW_TAG_member/DW_TAG_variable through
	 DW_AT_specification.  */
      if (static_inline_p)
	{
	  int old_extern = DECL_EXTERNAL (member);
	  DECL_EXTERNAL (member) = 0;
	  gen_decl_die (member, NULL, NULL, comp_unit_die ());
	  DECL_EXTERNAL (member) = old_extern;
	}
    }
}

/* Generate a DIE for a structure or union type.  If TYPE_DECL_SUPPRESS_DEBUG
   is set, we pretend that the type was never defined, so we only get the
   member DIEs needed by later specification DIEs.  */

static void
gen_struct_or_union_type_die (tree type, dw_die_ref context_die,
				enum debug_info_usage usage)
{
  if (TREE_ASM_WRITTEN (type))
    {
      /* Fill in the bound of variable-length fields in late dwarf if
	 still incomplete.  */
      if (!early_dwarf && variably_modified_type_p (type, NULL))
	for (tree member = TYPE_FIELDS (type);
	     member;
	     member = DECL_CHAIN (member))
	  fill_variable_array_bounds (TREE_TYPE (member));
      return;
    }

  dw_die_ref type_die = lookup_type_die (type);
  dw_die_ref scope_die = 0;
  int nested = 0;
  int complete = (TYPE_SIZE (type)
		  && (! TYPE_STUB_DECL (type)
		      || ! TYPE_DECL_SUPPRESS_DEBUG (TYPE_STUB_DECL (type))));
  int ns_decl = (context_die && context_die->die_tag == DW_TAG_namespace);
  complete = complete && should_emit_struct_debug (type, usage);

  if (type_die && ! complete)
    return;

  if (TYPE_CONTEXT (type) != NULL_TREE
      && (AGGREGATE_TYPE_P (TYPE_CONTEXT (type))
	  || TREE_CODE (TYPE_CONTEXT (type)) == NAMESPACE_DECL))
    nested = 1;

  scope_die = scope_die_for (type, context_die);

  /* Generate child dies for template paramaters.  */
  if (!type_die && debug_info_level > DINFO_LEVEL_TERSE)
    schedule_generic_params_dies_gen (type);

  if (! type_die || (nested && is_cu_die (scope_die)))
    /* First occurrence of type or toplevel definition of nested class.  */
    {
      dw_die_ref old_die = type_die;

      type_die = new_die (TREE_CODE (type) == RECORD_TYPE
			  ? record_type_tag (type) : DW_TAG_union_type,
			  scope_die, type);
      equate_type_number_to_die (type, type_die);
      if (old_die)
	add_AT_specification (type_die, old_die);
      else
	add_name_attribute (type_die, type_tag (type));
    }
  else
    remove_AT (type_die, DW_AT_declaration);

  /* If this type has been completed, then give it a byte_size attribute and
     then give a list of members.  */
  if (complete && !ns_decl)
    {
      /* Prevent infinite recursion in cases where the type of some member of
	 this type is expressed in terms of this type itself.  */
      TREE_ASM_WRITTEN (type) = 1;
      add_byte_size_attribute (type_die, type);
      add_alignment_attribute (type_die, type);
      if (TYPE_STUB_DECL (type) != NULL_TREE)
	{
	  add_src_coords_attributes (type_die, TYPE_STUB_DECL (type));
	  add_accessibility_attribute (type_die, TYPE_STUB_DECL (type));
	}

      /* If the first reference to this type was as the return type of an
	 inline function, then it may not have a parent.  Fix this now.  */
      if (type_die->die_parent == NULL)
	add_child_die (scope_die, type_die);

      push_decl_scope (type);
      gen_member_die (type, type_die);
      pop_decl_scope ();

      add_gnat_descriptive_type_attribute (type_die, type, context_die);
      if (TYPE_ARTIFICIAL (type))
	add_AT_flag (type_die, DW_AT_artificial, 1);

      /* GNU extension: Record what type our vtable lives in.  */
      if (TYPE_VFIELD (type))
	{
	  tree vtype = DECL_FCONTEXT (TYPE_VFIELD (type));

	  gen_type_die (vtype, context_die);
	  add_AT_die_ref (type_die, DW_AT_containing_type,
			  lookup_type_die (vtype));
	}
    }
  else
    {
      add_AT_flag (type_die, DW_AT_declaration, 1);

      /* We don't need to do this for function-local types.  */
      if (TYPE_STUB_DECL (type)
	  && ! decl_function_context (TYPE_STUB_DECL (type)))
	vec_safe_push (incomplete_types, type);
    }

  if (get_AT (type_die, DW_AT_name))
    add_pubtype (type, type_die);
}

/* Generate a DIE for a subroutine _type_.  */

static void
gen_subroutine_type_die (tree type, dw_die_ref context_die)
{
  tree return_type = TREE_TYPE (type);
  dw_die_ref subr_die
    = new_die (DW_TAG_subroutine_type,
	       scope_die_for (type, context_die), type);

  equate_type_number_to_die (type, subr_die);
  add_prototyped_attribute (subr_die, type);
  add_type_attribute (subr_die, return_type, TYPE_UNQUALIFIED, false,
		      context_die);
  add_alignment_attribute (subr_die, type);
  gen_formal_types_die (type, subr_die);

  if (get_AT (subr_die, DW_AT_name))
    add_pubtype (type, subr_die);
  if ((dwarf_version >= 5 || !dwarf_strict)
      && lang_hooks.types.type_dwarf_attribute (type, DW_AT_reference) != -1)
    add_AT_flag (subr_die, DW_AT_reference, 1);
  if ((dwarf_version >= 5 || !dwarf_strict)
      && lang_hooks.types.type_dwarf_attribute (type,
						DW_AT_rvalue_reference) != -1)
    add_AT_flag (subr_die, DW_AT_rvalue_reference, 1);
}

/* Generate a DIE for a type definition.  */

static void
gen_typedef_die (tree decl, dw_die_ref context_die)
{
  dw_die_ref type_die;
  tree type;

  if (TREE_ASM_WRITTEN (decl))
    {
      if (DECL_ORIGINAL_TYPE (decl))
	fill_variable_array_bounds (DECL_ORIGINAL_TYPE (decl));
      return;
    }

  /* As we avoid creating DIEs for local typedefs (see decl_ultimate_origin
     checks in process_scope_var and modified_type_die), this should be called
     only for original types.  */
  gcc_assert (decl_ultimate_origin (decl) == NULL
	      || decl_ultimate_origin (decl) == decl);

  TREE_ASM_WRITTEN (decl) = 1;
  type_die = new_die (DW_TAG_typedef, context_die, decl);

  add_name_and_src_coords_attributes (type_die, decl);
  if (DECL_ORIGINAL_TYPE (decl))
    {
      type = DECL_ORIGINAL_TYPE (decl);
      if (type == error_mark_node)
	return;

      gcc_assert (type != TREE_TYPE (decl));
      equate_type_number_to_die (TREE_TYPE (decl), type_die);
    }
  else
    {
      type = TREE_TYPE (decl);
      if (type == error_mark_node)
	return;

      if (is_naming_typedef_decl (TYPE_NAME (type)))
	{
	  /* Here, we are in the case of decl being a typedef naming
	     an anonymous type, e.g:
		 typedef struct {...} foo;
	     In that case TREE_TYPE (decl) is not a typedef variant
	     type and TYPE_NAME of the anonymous type is set to the
	     TYPE_DECL of the typedef. This construct is emitted by
	     the C++ FE.

	     TYPE is the anonymous struct named by the typedef
	     DECL. As we need the DW_AT_type attribute of the
	     DW_TAG_typedef to point to the DIE of TYPE, let's
	     generate that DIE right away. add_type_attribute
	     called below will then pick (via lookup_type_die) that
	     anonymous struct DIE.  */
	  if (!TREE_ASM_WRITTEN (type))
	    gen_tagged_type_die (type, context_die, DINFO_USAGE_DIR_USE);

	  /* This is a GNU Extension.  We are adding a
	     DW_AT_linkage_name attribute to the DIE of the
	     anonymous struct TYPE.  The value of that attribute
	     is the name of the typedef decl naming the anonymous
	     struct.  This greatly eases the work of consumers of
	     this debug info.  */
	  add_linkage_name_raw (lookup_type_die (type), decl);
	}
    }

  add_type_attribute (type_die, type, decl_quals (decl), false,
		      context_die);

  if (is_naming_typedef_decl (decl))
    /* We want that all subsequent calls to lookup_type_die with
       TYPE in argument yield the DW_TAG_typedef we have just
       created.  */
    equate_type_number_to_die (type, type_die);

  add_alignment_attribute (type_die, TREE_TYPE (decl));

  add_accessibility_attribute (type_die, decl);

  if (DECL_ABSTRACT_P (decl))
    equate_decl_number_to_die (decl, type_die);

  if (get_AT (type_die, DW_AT_name))
    add_pubtype (decl, type_die);
}

/* Generate a DIE for a struct, class, enum or union type.  */

static void
gen_tagged_type_die (tree type,
		     dw_die_ref context_die,
		     enum debug_info_usage usage)
{
  int need_pop;

  if (type == NULL_TREE
      || !is_tagged_type (type))
    return;

  if (TREE_ASM_WRITTEN (type))
    need_pop = 0;
  /* If this is a nested type whose containing class hasn't been written
     out yet, writing it out will cover this one, too.  This does not apply
     to instantiations of member class templates; they need to be added to
     the containing class as they are generated.  FIXME: This hurts the
     idea of combining type decls from multiple TUs, since we can't predict
     what set of template instantiations we'll get.  */
  else if (TYPE_CONTEXT (type)
      && AGGREGATE_TYPE_P (TYPE_CONTEXT (type))
      && ! TREE_ASM_WRITTEN (TYPE_CONTEXT (type)))
    {
      gen_type_die_with_usage (TYPE_CONTEXT (type), context_die, usage);

      if (TREE_ASM_WRITTEN (type))
	return;

      /* If that failed, attach ourselves to the stub.  */
      push_decl_scope (TYPE_CONTEXT (type));
      context_die = lookup_type_die (TYPE_CONTEXT (type));
      need_pop = 1;
    }
  else if (TYPE_CONTEXT (type) != NULL_TREE
	   && (TREE_CODE (TYPE_CONTEXT (type)) == FUNCTION_DECL))
    {
      /* If this type is local to a function that hasn't been written
	 out yet, use a NULL context for now; it will be fixed up in
	 decls_for_scope.  */
      context_die = lookup_decl_die (TYPE_CONTEXT (type));
      /* A declaration DIE doesn't count; nested types need to go in the
	 specification.  */
      if (context_die && is_declaration_die (context_die))
	context_die = NULL;
      need_pop = 0;
    }
  else
    {
      context_die = declare_in_namespace (type, context_die);
      need_pop = 0;
    }

  if (TREE_CODE (type) == ENUMERAL_TYPE)
    {
      /* This might have been written out by the call to
	 declare_in_namespace.  */
      if (!TREE_ASM_WRITTEN (type))
	gen_enumeration_type_die (type, context_die);
    }
  else
    gen_struct_or_union_type_die (type, context_die, usage);

  if (need_pop)
    pop_decl_scope ();

  /* Don't set TREE_ASM_WRITTEN on an incomplete struct; we want to fix
     it up if it is ever completed.  gen_*_type_die will set it for us
     when appropriate.  */
}

/* Generate a type description DIE.  */

static void
gen_type_die_with_usage (tree type, dw_die_ref context_die,
			 enum debug_info_usage usage)
{
  struct array_descr_info info;

  if (type == NULL_TREE || type == error_mark_node)
    return;

  if (flag_checking && type)
     verify_type (type);

  if (TYPE_NAME (type) != NULL_TREE
      && TREE_CODE (TYPE_NAME (type)) == TYPE_DECL
      && is_redundant_typedef (TYPE_NAME (type))
      && DECL_ORIGINAL_TYPE (TYPE_NAME (type)))
    /* The DECL of this type is a typedef we don't want to emit debug
       info for but we want debug info for its underlying typedef.
       This can happen for e.g, the injected-class-name of a C++
       type.  */
    type = DECL_ORIGINAL_TYPE (TYPE_NAME (type));

  /* If TYPE is a typedef type variant, let's generate debug info
     for the parent typedef which TYPE is a type of.  */
  if (typedef_variant_p (type))
    {
      if (TREE_ASM_WRITTEN (type))
	return;

      tree name = TYPE_NAME (type);
      tree origin = decl_ultimate_origin (name);
      if (origin != NULL && origin != name)
	{
	  gen_decl_die (origin, NULL, NULL, context_die);
	  return;
	}

      /* Prevent broken recursion; we can't hand off to the same type.  */
      gcc_assert (DECL_ORIGINAL_TYPE (name) != type);

      /* Give typedefs the right scope.  */
      context_die = scope_die_for (type, context_die);

      TREE_ASM_WRITTEN (type) = 1;

      gen_decl_die (name, NULL, NULL, context_die);
      return;
    }

  /* If type is an anonymous tagged type named by a typedef, let's
     generate debug info for the typedef.  */
  if (is_naming_typedef_decl (TYPE_NAME (type)))
    {
      /* Use the DIE of the containing namespace as the parent DIE of
         the type description DIE we want to generate.  */
      if (DECL_CONTEXT (TYPE_NAME (type))
	  && TREE_CODE (DECL_CONTEXT (TYPE_NAME (type))) == NAMESPACE_DECL)
	context_die = get_context_die (DECL_CONTEXT (TYPE_NAME (type)));

      gen_decl_die (TYPE_NAME (type), NULL, NULL, context_die);
      return;
    }

  if (lang_hooks.types.get_debug_type)
    {
      tree debug_type = lang_hooks.types.get_debug_type (type);

      if (debug_type != NULL_TREE && debug_type != type)
	{
	  gen_type_die_with_usage (debug_type, context_die, usage);
	  return;
	}
    }

  /* We are going to output a DIE to represent the unqualified version
     of this type (i.e. without any const or volatile qualifiers) so
     get the main variant (i.e. the unqualified version) of this type
     now.  (Vectors and arrays are special because the debugging info is in the
     cloned type itself.  Similarly function/method types can contain extra
     ref-qualification).  */
  if (TREE_CODE (type) == FUNCTION_TYPE
      || TREE_CODE (type) == METHOD_TYPE)
    {
      /* For function/method types, can't use type_main_variant here,
	 because that can have different ref-qualifiers for C++,
	 but try to canonicalize.  */
      tree main = TYPE_MAIN_VARIANT (type);
      for (tree t = main; t; t = TYPE_NEXT_VARIANT (t))
	if (TYPE_QUALS_NO_ADDR_SPACE (t) == 0
	    && check_base_type (t, main)
	    && check_lang_type (t, type))
	  {
	    type = t;
	    break;
	  }
    }
  else if (TREE_CODE (type) != VECTOR_TYPE
	   && TREE_CODE (type) != ARRAY_TYPE)
    type = type_main_variant (type);

  /* If this is an array type with hidden descriptor, handle it first.  */
  if (!TREE_ASM_WRITTEN (type)
      && lang_hooks.types.get_array_descr_info)
    {
      memset (&info, 0, sizeof (info));
      if (lang_hooks.types.get_array_descr_info (type, &info))
	{
	  /* Fortran sometimes emits array types with no dimension.  */
	  gcc_assert (info.ndimensions >= 0
		      && (info.ndimensions
			  <= DWARF2OUT_ARRAY_DESCR_INFO_MAX_DIMEN));
	  gen_descr_array_type_die (type, &info, context_die);
	  TREE_ASM_WRITTEN (type) = 1;
	  return;
	}
    }

  if (TREE_ASM_WRITTEN (type))
    {
      /* Variable-length types may be incomplete even if
	 TREE_ASM_WRITTEN.  For such types, fall through to
	 gen_array_type_die() and possibly fill in
	 DW_AT_{upper,lower}_bound attributes.  */
      if ((TREE_CODE (type) != ARRAY_TYPE
	   && TREE_CODE (type) != RECORD_TYPE
	   && TREE_CODE (type) != UNION_TYPE
	   && TREE_CODE (type) != QUAL_UNION_TYPE)
	  || !variably_modified_type_p (type, NULL))
	return;
    }

  switch (TREE_CODE (type))
    {
    case ERROR_MARK:
      break;

    case POINTER_TYPE:
    case REFERENCE_TYPE:
      /* We must set TREE_ASM_WRITTEN in case this is a recursive type.  This
	 ensures that the gen_type_die recursion will terminate even if the
	 type is recursive.  Recursive types are possible in Ada.  */
      /* ??? We could perhaps do this for all types before the switch
	 statement.  */
      TREE_ASM_WRITTEN (type) = 1;

      /* For these types, all that is required is that we output a DIE (or a
	 set of DIEs) to represent the "basis" type.  */
      gen_type_die_with_usage (TREE_TYPE (type), context_die,
			       DINFO_USAGE_IND_USE);
      break;

    case OFFSET_TYPE:
      /* This code is used for C++ pointer-to-data-member types.
	 Output a description of the relevant class type.  */
      gen_type_die_with_usage (TYPE_OFFSET_BASETYPE (type), context_die,
			       DINFO_USAGE_IND_USE);

      /* Output a description of the type of the object pointed to.  */
      gen_type_die_with_usage (TREE_TYPE (type), context_die,
			       DINFO_USAGE_IND_USE);

      /* Now output a DIE to represent this pointer-to-data-member type
	 itself.  */
      gen_ptr_to_mbr_type_die (type, context_die);
      break;

    case FUNCTION_TYPE:
      /* Force out return type (in case it wasn't forced out already).  */
      gen_type_die_with_usage (TREE_TYPE (type), context_die,
			       DINFO_USAGE_DIR_USE);
      gen_subroutine_type_die (type, context_die);
      break;

    case METHOD_TYPE:
      /* Force out return type (in case it wasn't forced out already).  */
      gen_type_die_with_usage (TREE_TYPE (type), context_die,
			       DINFO_USAGE_DIR_USE);
      gen_subroutine_type_die (type, context_die);
      break;

    case ARRAY_TYPE:
    case VECTOR_TYPE:
      gen_array_type_die (type, context_die);
      break;

    case ENUMERAL_TYPE:
    case RECORD_TYPE:
    case UNION_TYPE:
    case QUAL_UNION_TYPE:
      gen_tagged_type_die (type, context_die, usage);
      return;

    case VOID_TYPE:
    case INTEGER_TYPE:
    case REAL_TYPE:
    case FIXED_POINT_TYPE:
    case COMPLEX_TYPE:
    case BOOLEAN_TYPE:
    case POINTER_BOUNDS_TYPE:
      /* No DIEs needed for fundamental types.  */
      break;

    case NULLPTR_TYPE:
    case LANG_TYPE:
      /* Just use DW_TAG_unspecified_type.  */
      {
        dw_die_ref type_die = lookup_type_die (type);
        if (type_die == NULL)
          {
	    tree name = TYPE_IDENTIFIER (type);
            type_die = new_die (DW_TAG_unspecified_type, comp_unit_die (),
				type);
            add_name_attribute (type_die, IDENTIFIER_POINTER (name));
            equate_type_number_to_die (type, type_die);
          }
      }
      break;

    default:
      if (is_cxx_auto (type))
	{
	  tree name = TYPE_IDENTIFIER (type);
	  dw_die_ref *die = (name == get_identifier ("auto")
			     ? &auto_die : &decltype_auto_die);
	  if (!*die)
	    {
	      *die = new_die (DW_TAG_unspecified_type,
			      comp_unit_die (), NULL_TREE);
	      add_name_attribute (*die, IDENTIFIER_POINTER (name));
	    }
	  equate_type_number_to_die (type, *die);
	  break;
	}
      gcc_unreachable ();
    }

  TREE_ASM_WRITTEN (type) = 1;
}

static void
gen_type_die (tree type, dw_die_ref context_die)
{
  if (type != error_mark_node)
    {
      gen_type_die_with_usage (type, context_die, DINFO_USAGE_DIR_USE);
      if (flag_checking)
	{
	  dw_die_ref die = lookup_type_die (type);
	  if (die)
	    check_die (die);
	}
    }
}

/* Generate a DW_TAG_lexical_block DIE followed by DIEs to represent all of the
   things which are local to the given block.  */

static void
gen_block_die (tree stmt, dw_die_ref context_die)
{
  int must_output_die = 0;
  bool inlined_func;

  /* Ignore blocks that are NULL.  */
  if (stmt == NULL_TREE)
    return;

  inlined_func = inlined_function_outer_scope_p (stmt);

  /* If the block is one fragment of a non-contiguous block, do not
     process the variables, since they will have been done by the
     origin block.  Do process subblocks.  */
  if (BLOCK_FRAGMENT_ORIGIN (stmt))
    {
      tree sub;

      for (sub = BLOCK_SUBBLOCKS (stmt); sub; sub = BLOCK_CHAIN (sub))
	gen_block_die (sub, context_die);

      return;
    }

  /* Determine if we need to output any Dwarf DIEs at all to represent this
     block.  */
  if (inlined_func)
    /* The outer scopes for inlinings *must* always be represented.  We
       generate DW_TAG_inlined_subroutine DIEs for them.  (See below.) */
    must_output_die = 1;
  else
    {
      /* Determine if this block directly contains any "significant"
	 local declarations which we will need to output DIEs for.  */
      if (debug_info_level > DINFO_LEVEL_TERSE)
	/* We are not in terse mode so *any* local declaration counts
	   as being a "significant" one.  */
	must_output_die = ((BLOCK_VARS (stmt) != NULL
			    || BLOCK_NUM_NONLOCALIZED_VARS (stmt))
			   && (TREE_USED (stmt)
			       || TREE_ASM_WRITTEN (stmt)
			       || BLOCK_ABSTRACT (stmt)));
      else if ((TREE_USED (stmt)
		|| TREE_ASM_WRITTEN (stmt)
		|| BLOCK_ABSTRACT (stmt))
      	       && !dwarf2out_ignore_block (stmt))
	must_output_die = 1;
    }

  /* It would be a waste of space to generate a Dwarf DW_TAG_lexical_block
     DIE for any block which contains no significant local declarations at
     all.  Rather, in such cases we just call `decls_for_scope' so that any
     needed Dwarf info for any sub-blocks will get properly generated. Note
     that in terse mode, our definition of what constitutes a "significant"
     local declaration gets restricted to include only inlined function
     instances and local (nested) function definitions.  */
  if (must_output_die)
    {
      if (inlined_func)
	{
	  /* If STMT block is abstract, that means we have been called
	     indirectly from dwarf2out_abstract_function.
	     That function rightfully marks the descendent blocks (of
	     the abstract function it is dealing with) as being abstract,
	     precisely to prevent us from emitting any
	     DW_TAG_inlined_subroutine DIE as a descendent
	     of an abstract function instance. So in that case, we should
	     not call gen_inlined_subroutine_die.

	     Later though, when cgraph asks dwarf2out to emit info
	     for the concrete instance of the function decl into which
	     the concrete instance of STMT got inlined, the later will lead
	     to the generation of a DW_TAG_inlined_subroutine DIE.  */
	  if (! BLOCK_ABSTRACT (stmt))
	    gen_inlined_subroutine_die (stmt, context_die);
	}
      else
	gen_lexical_block_die (stmt, context_die);
    }
  else
    decls_for_scope (stmt, context_die);
}

/* Process variable DECL (or variable with origin ORIGIN) within
   block STMT and add it to CONTEXT_DIE.  */
static void
process_scope_var (tree stmt, tree decl, tree origin, dw_die_ref context_die)
{
  dw_die_ref die;
  tree decl_or_origin = decl ? decl : origin;

  if (TREE_CODE (decl_or_origin) == FUNCTION_DECL)
    die = lookup_decl_die (decl_or_origin);
  else if (TREE_CODE (decl_or_origin) == TYPE_DECL)
    {
      if (TYPE_DECL_IS_STUB (decl_or_origin))
	die = lookup_type_die (TREE_TYPE (decl_or_origin));
      else
	die = lookup_decl_die (decl_or_origin);
      /* Avoid re-creating the DIE late if it was optimized as unused early.  */
      if (! die && ! early_dwarf)
	return;
    }
  else
    die = NULL;

  /* Avoid creating DIEs for local typedefs and concrete static variables that
     will only be pruned later.  */
  if ((origin || decl_ultimate_origin (decl))
      && (TREE_CODE (decl_or_origin) == TYPE_DECL
	  || (VAR_P (decl_or_origin) && TREE_STATIC (decl_or_origin))))
    {
      origin = decl_ultimate_origin (decl_or_origin);
      if (decl && VAR_P (decl) && die != NULL)
	{
	  die = lookup_decl_die (origin);
	  if (die != NULL)
	    equate_decl_number_to_die (decl, die);
	}
      return;
    }

  if (die != NULL && die->die_parent == NULL)
    add_child_die (context_die, die);
  else if (TREE_CODE (decl_or_origin) == IMPORTED_DECL)
    {
      if (early_dwarf)
	dwarf2out_imported_module_or_decl_1 (decl_or_origin, DECL_NAME (decl_or_origin),
					     stmt, context_die);
    }
  else
    {
      if (decl && DECL_P (decl))
	{
	  die = lookup_decl_die (decl);

	  /* Early created DIEs do not have a parent as the decls refer
	     to the function as DECL_CONTEXT rather than the BLOCK.  */
	  if (die && die->die_parent == NULL)
	    {
	      gcc_assert (in_lto_p);
	      add_child_die (context_die, die);
	    }
	}

      gen_decl_die (decl, origin, NULL, context_die);
    }
}

/* Generate all of the decls declared within a given scope and (recursively)
   all of its sub-blocks.  */

static void
decls_for_scope (tree stmt, dw_die_ref context_die)
{
  tree decl;
  unsigned int i;
  tree subblocks;

  /* Ignore NULL blocks.  */
  if (stmt == NULL_TREE)
    return;

  /* Output the DIEs to represent all of the data objects and typedefs
     declared directly within this block but not within any nested
     sub-blocks.  Also, nested function and tag DIEs have been
     generated with a parent of NULL; fix that up now.  We don't
     have to do this if we're at -g1.  */
  if (debug_info_level > DINFO_LEVEL_TERSE)
    {
      for (decl = BLOCK_VARS (stmt); decl != NULL; decl = DECL_CHAIN (decl))
	process_scope_var (stmt, decl, NULL_TREE, context_die);
      /* BLOCK_NONLOCALIZED_VARs simply generate DIE stubs with abstract
	 origin - avoid doing this twice as we have no good way to see
	 if we've done it once already.  */
      if (! early_dwarf)
	for (i = 0; i < BLOCK_NUM_NONLOCALIZED_VARS (stmt); i++)
	  {
	    decl = BLOCK_NONLOCALIZED_VAR (stmt, i);
	    if (decl == current_function_decl)
	      /* Ignore declarations of the current function, while they
		 are declarations, gen_subprogram_die would treat them
		 as definitions again, because they are equal to
		 current_function_decl and endlessly recurse.  */;
	    else if (TREE_CODE (decl) == FUNCTION_DECL)
	      process_scope_var (stmt, decl, NULL_TREE, context_die);
	    else
	      process_scope_var (stmt, NULL_TREE, decl, context_die);
	  }
    }

  /* Even if we're at -g1, we need to process the subblocks in order to get
     inlined call information.  */

  /* Output the DIEs to represent all sub-blocks (and the items declared
     therein) of this block.  */
  for (subblocks = BLOCK_SUBBLOCKS (stmt);
       subblocks != NULL;
       subblocks = BLOCK_CHAIN (subblocks))
    gen_block_die (subblocks, context_die);
}

/* Is this a typedef we can avoid emitting?  */

bool
is_redundant_typedef (const_tree decl)
{
  if (TYPE_DECL_IS_STUB (decl))
    return true;

  if (DECL_ARTIFICIAL (decl)
      && DECL_CONTEXT (decl)
      && is_tagged_type (DECL_CONTEXT (decl))
      && TREE_CODE (TYPE_NAME (DECL_CONTEXT (decl))) == TYPE_DECL
      && DECL_NAME (decl) == DECL_NAME (TYPE_NAME (DECL_CONTEXT (decl))))
    /* Also ignore the artificial member typedef for the class name.  */
    return true;

  return false;
}

/* Return TRUE if TYPE is a typedef that names a type for linkage
   purposes. This kind of typedefs is produced by the C++ FE for
   constructs like:

   typedef struct {...} foo;

   In that case, there is no typedef variant type produced for foo.
   Rather, the TREE_TYPE of the TYPE_DECL of foo is the anonymous
   struct type.  */

static bool
is_naming_typedef_decl (const_tree decl)
{
  if (decl == NULL_TREE
      || TREE_CODE (decl) != TYPE_DECL
      || DECL_NAMELESS (decl)
      || !is_tagged_type (TREE_TYPE (decl))
      || DECL_IS_BUILTIN (decl)
      || is_redundant_typedef (decl)
      /* It looks like Ada produces TYPE_DECLs that are very similar
         to C++ naming typedefs but that have different
         semantics. Let's be specific to c++ for now.  */
      || !is_cxx (decl))
    return FALSE;

  return (DECL_ORIGINAL_TYPE (decl) == NULL_TREE
	  && TYPE_NAME (TREE_TYPE (decl)) == decl
	  && (TYPE_STUB_DECL (TREE_TYPE (decl))
	      != TYPE_NAME (TREE_TYPE (decl))));
}

/* Looks up the DIE for a context.  */

static inline dw_die_ref
lookup_context_die (tree context)
{
  if (context)
    {
      /* Find die that represents this context.  */
      if (TYPE_P (context))
	{
	  context = TYPE_MAIN_VARIANT (context);
	  dw_die_ref ctx = lookup_type_die (context);
	  if (!ctx)
	    return NULL;
	  return strip_naming_typedef (context, ctx);
	}
      else
	return lookup_decl_die (context);
    }
  return comp_unit_die ();
}

/* Returns the DIE for a context.  */

static inline dw_die_ref
get_context_die (tree context)
{
  if (context)
    {
      /* Find die that represents this context.  */
      if (TYPE_P (context))
	{
	  context = TYPE_MAIN_VARIANT (context);
	  return strip_naming_typedef (context, force_type_die (context));
	}
      else
	return force_decl_die (context);
    }
  return comp_unit_die ();
}

/* Returns the DIE for decl.  A DIE will always be returned.  */

static dw_die_ref
force_decl_die (tree decl)
{
  dw_die_ref decl_die;
  unsigned saved_external_flag;
  tree save_fn = NULL_TREE;
  decl_die = lookup_decl_die (decl);
  if (!decl_die)
    {
      dw_die_ref context_die = get_context_die (DECL_CONTEXT (decl));

      decl_die = lookup_decl_die (decl);
      if (decl_die)
	return decl_die;

      switch (TREE_CODE (decl))
	{
	case FUNCTION_DECL:
	  /* Clear current_function_decl, so that gen_subprogram_die thinks
	     that this is a declaration. At this point, we just want to force
	     declaration die.  */
	  save_fn = current_function_decl;
	  current_function_decl = NULL_TREE;
	  gen_subprogram_die (decl, context_die);
	  current_function_decl = save_fn;
	  break;

	case VAR_DECL:
	  /* Set external flag to force declaration die. Restore it after
	   gen_decl_die() call.  */
	  saved_external_flag = DECL_EXTERNAL (decl);
	  DECL_EXTERNAL (decl) = 1;
	  gen_decl_die (decl, NULL, NULL, context_die);
	  DECL_EXTERNAL (decl) = saved_external_flag;
	  break;

	case NAMESPACE_DECL:
	  if (dwarf_version >= 3 || !dwarf_strict)
	    dwarf2out_decl (decl);
	  else
	    /* DWARF2 has neither DW_TAG_module, nor DW_TAG_namespace.  */
	    decl_die = comp_unit_die ();
	  break;

	case TRANSLATION_UNIT_DECL:
	  decl_die = comp_unit_die ();
	  break;

	default:
	  gcc_unreachable ();
	}

      /* We should be able to find the DIE now.  */
      if (!decl_die)
	decl_die = lookup_decl_die (decl);
      gcc_assert (decl_die);
    }

  return decl_die;
}

/* Returns the DIE for TYPE, that must not be a base type.  A DIE is
   always returned.  */

static dw_die_ref
force_type_die (tree type)
{
  dw_die_ref type_die;

  type_die = lookup_type_die (type);
  if (!type_die)
    {
      dw_die_ref context_die = get_context_die (TYPE_CONTEXT (type));

      type_die = modified_type_die (type, TYPE_QUALS_NO_ADDR_SPACE (type),
				    false, context_die);
      gcc_assert (type_die);
    }
  return type_die;
}

/* Force out any required namespaces to be able to output DECL,
   and return the new context_die for it, if it's changed.  */

static dw_die_ref
setup_namespace_context (tree thing, dw_die_ref context_die)
{
  tree context = (DECL_P (thing)
		  ? DECL_CONTEXT (thing) : TYPE_CONTEXT (thing));
  if (context && TREE_CODE (context) == NAMESPACE_DECL)
    /* Force out the namespace.  */
    context_die = force_decl_die (context);

  return context_die;
}

/* Emit a declaration DIE for THING (which is either a DECL or a tagged
   type) within its namespace, if appropriate.

   For compatibility with older debuggers, namespace DIEs only contain
   declarations; all definitions are emitted at CU scope, with
   DW_AT_specification pointing to the declaration (like with class
   members).  */

static dw_die_ref
declare_in_namespace (tree thing, dw_die_ref context_die)
{
  dw_die_ref ns_context;

  if (debug_info_level <= DINFO_LEVEL_TERSE)
    return context_die;

  /* External declarations in the local scope only need to be emitted
     once, not once in the namespace and once in the scope.

     This avoids declaring the `extern' below in the
     namespace DIE as well as in the innermost scope:

          namespace S
	  {
            int i=5;
            int foo()
	    {
              int i=8;
              extern int i;
     	      return i;
	    }
          }
  */
  if (DECL_P (thing) && DECL_EXTERNAL (thing) && local_scope_p (context_die))
    return context_die;

  /* If this decl is from an inlined function, then don't try to emit it in its
     namespace, as we will get confused.  It would have already been emitted
     when the abstract instance of the inline function was emitted anyways.  */
  if (DECL_P (thing) && DECL_ABSTRACT_ORIGIN (thing))
    return context_die;

  ns_context = setup_namespace_context (thing, context_die);

  if (ns_context != context_die)
    {
      if (is_fortran ())
	return ns_context;
      if (DECL_P (thing))
	gen_decl_die (thing, NULL, NULL, ns_context);
      else
	gen_type_die (thing, ns_context);
    }
  return context_die;
}

/* Generate a DIE for a namespace or namespace alias.  */

static void
gen_namespace_die (tree decl, dw_die_ref context_die)
{
  dw_die_ref namespace_die;

  /* Namespace aliases have a DECL_ABSTRACT_ORIGIN of the namespace
     they are an alias of.  */
  if (DECL_ABSTRACT_ORIGIN (decl) == NULL)
    {
      /* Output a real namespace or module.  */
      context_die = setup_namespace_context (decl, comp_unit_die ());
      namespace_die = new_die (is_fortran ()
			       ? DW_TAG_module : DW_TAG_namespace,
			       context_die, decl);
      /* For Fortran modules defined in different CU don't add src coords.  */
      if (namespace_die->die_tag == DW_TAG_module && DECL_EXTERNAL (decl))
	{
	  const char *name = dwarf2_name (decl, 0);
	  if (name)
	    add_name_attribute (namespace_die, name);
	}
      else
	add_name_and_src_coords_attributes (namespace_die, decl);
      if (DECL_EXTERNAL (decl))
	add_AT_flag (namespace_die, DW_AT_declaration, 1);
      equate_decl_number_to_die (decl, namespace_die);
    }
  else
    {
      /* Output a namespace alias.  */

      /* Force out the namespace we are an alias of, if necessary.  */
      dw_die_ref origin_die
	= force_decl_die (DECL_ABSTRACT_ORIGIN (decl));

      if (DECL_FILE_SCOPE_P (decl)
	  || TREE_CODE (DECL_CONTEXT (decl)) == NAMESPACE_DECL)
	context_die = setup_namespace_context (decl, comp_unit_die ());
      /* Now create the namespace alias DIE.  */
      namespace_die = new_die (DW_TAG_imported_declaration, context_die, decl);
      add_name_and_src_coords_attributes (namespace_die, decl);
      add_AT_die_ref (namespace_die, DW_AT_import, origin_die);
      equate_decl_number_to_die (decl, namespace_die);
    }
  if ((dwarf_version >= 5 || !dwarf_strict)
      && lang_hooks.decls.decl_dwarf_attribute (decl,
						DW_AT_export_symbols) == 1)
    add_AT_flag (namespace_die, DW_AT_export_symbols, 1);

  /* Bypass dwarf2_name's check for DECL_NAMELESS.  */
  if (want_pubnames ())
    add_pubname_string (lang_hooks.dwarf_name (decl, 1), namespace_die);
}

/* Generate Dwarf debug information for a decl described by DECL.
   The return value is currently only meaningful for PARM_DECLs,
   for all other decls it returns NULL.

   If DECL is a FIELD_DECL, CTX is required: see the comment for VLR_CONTEXT.
   It can be NULL otherwise.  */

static dw_die_ref
gen_decl_die (tree decl, tree origin, struct vlr_context *ctx,
	      dw_die_ref context_die)
{
  tree decl_or_origin = decl ? decl : origin;
  tree class_origin = NULL, ultimate_origin;

  if (DECL_P (decl_or_origin) && DECL_IGNORED_P (decl_or_origin))
    return NULL;

  /* Ignore pointer bounds decls.  */
  if (DECL_P (decl_or_origin)
      && TREE_TYPE (decl_or_origin)
      && POINTER_BOUNDS_P (decl_or_origin))
    return NULL;

  switch (TREE_CODE (decl_or_origin))
    {
    case ERROR_MARK:
      break;

    case CONST_DECL:
      if (!is_fortran () && !is_ada ())
	{
	  /* The individual enumerators of an enum type get output when we output
	     the Dwarf representation of the relevant enum type itself.  */
	  break;
	}

      /* Emit its type.  */
      gen_type_die (TREE_TYPE (decl), context_die);

      /* And its containing namespace.  */
      context_die = declare_in_namespace (decl, context_die);

      gen_const_die (decl, context_die);
      break;

    case FUNCTION_DECL:
#if 0
      /* FIXME */
      /* This doesn't work because the C frontend sets DECL_ABSTRACT_ORIGIN
	 on local redeclarations of global functions.  That seems broken.  */
      if (current_function_decl != decl)
	/* This is only a declaration.  */;
#endif

      /* We should have abstract copies already and should not generate
	 stray type DIEs in late LTO dumping.  */
      if (! early_dwarf)
	;

      /* If we're emitting a clone, emit info for the abstract instance.  */
      else if (origin || DECL_ORIGIN (decl) != decl)
	dwarf2out_abstract_function (origin
				     ? DECL_ORIGIN (origin)
				     : DECL_ABSTRACT_ORIGIN (decl));

      /* If we're emitting a possibly inlined function emit it as
         abstract instance.  */
      else if (cgraph_function_possibly_inlined_p (decl)
	       && ! DECL_ABSTRACT_P (decl)
	       && ! class_or_namespace_scope_p (context_die)
	       /* dwarf2out_abstract_function won't emit a die if this is just
		  a declaration.  We must avoid setting DECL_ABSTRACT_ORIGIN in
		  that case, because that works only if we have a die.  */
	       && DECL_INITIAL (decl) != NULL_TREE)
	dwarf2out_abstract_function (decl);

      /* Otherwise we're emitting the primary DIE for this decl.  */
      else if (debug_info_level > DINFO_LEVEL_TERSE)
	{
	  /* Before we describe the FUNCTION_DECL itself, make sure that we
	     have its containing type.  */
	  if (!origin)
	    origin = decl_class_context (decl);
	  if (origin != NULL_TREE)
	    gen_type_die (origin, context_die);

	  /* And its return type.  */
	  gen_type_die (TREE_TYPE (TREE_TYPE (decl)), context_die);

	  /* And its virtual context.  */
	  if (DECL_VINDEX (decl) != NULL_TREE)
	    gen_type_die (DECL_CONTEXT (decl), context_die);

	  /* Make sure we have a member DIE for decl.  */
	  if (origin != NULL_TREE)
	    gen_type_die_for_member (origin, decl, context_die);

	  /* And its containing namespace.  */
	  context_die = declare_in_namespace (decl, context_die);
	}

      /* Now output a DIE to represent the function itself.  */
      if (decl)
        gen_subprogram_die (decl, context_die);
      break;

    case TYPE_DECL:
      /* If we are in terse mode, don't generate any DIEs to represent any
	 actual typedefs.  */
      if (debug_info_level <= DINFO_LEVEL_TERSE)
	break;

      /* In the special case of a TYPE_DECL node representing the declaration
	 of some type tag, if the given TYPE_DECL is marked as having been
	 instantiated from some other (original) TYPE_DECL node (e.g. one which
	 was generated within the original definition of an inline function) we
	 used to generate a special (abbreviated) DW_TAG_structure_type,
	 DW_TAG_union_type, or DW_TAG_enumeration_type DIE here.  But nothing
	 should be actually referencing those DIEs, as variable DIEs with that
	 type would be emitted already in the abstract origin, so it was always
	 removed during unused type prunning.  Don't add anything in this
	 case.  */
      if (TYPE_DECL_IS_STUB (decl) && decl_ultimate_origin (decl) != NULL_TREE)
	break;

      if (is_redundant_typedef (decl))
	gen_type_die (TREE_TYPE (decl), context_die);
      else
	/* Output a DIE to represent the typedef itself.  */
	gen_typedef_die (decl, context_die);
      break;

    case LABEL_DECL:
      if (debug_info_level >= DINFO_LEVEL_NORMAL)
	gen_label_die (decl, context_die);
      break;

    case VAR_DECL:
    case RESULT_DECL:
      /* If we are in terse mode, don't generate any DIEs to represent any
	 variable declarations or definitions.  */
      if (debug_info_level <= DINFO_LEVEL_TERSE)
	break;

      /* Avoid generating stray type DIEs during late dwarf dumping.
         All types have been dumped early.  */
      if (early_dwarf
	  /* ???  But in LTRANS we cannot annotate early created variably
	     modified type DIEs without copying them and adjusting all
	     references to them.  Dump them again as happens for inlining
	     which copies both the decl and the types.  */
	  /* ???  And even non-LTO needs to re-visit type DIEs to fill
	     in VLA bound information for example.  */
	  || (decl && variably_modified_type_p (TREE_TYPE (decl),
						current_function_decl)))
	{
	  /* Output any DIEs that are needed to specify the type of this data
	     object.  */
	  if (decl_by_reference_p (decl_or_origin))
	    gen_type_die (TREE_TYPE (TREE_TYPE (decl_or_origin)), context_die);
	  else
	    gen_type_die (TREE_TYPE (decl_or_origin), context_die);
	}

      if (early_dwarf)
	{
	  /* And its containing type.  */
	  class_origin = decl_class_context (decl_or_origin);
	  if (class_origin != NULL_TREE)
	    gen_type_die_for_member (class_origin, decl_or_origin, context_die);

	  /* And its containing namespace.  */
	  context_die = declare_in_namespace (decl_or_origin, context_die);
	}

      /* Now output the DIE to represent the data object itself.  This gets
	 complicated because of the possibility that the VAR_DECL really
	 represents an inlined instance of a formal parameter for an inline
	 function.  */
      ultimate_origin = decl_ultimate_origin (decl_or_origin);
      if (ultimate_origin != NULL_TREE
	  && TREE_CODE (ultimate_origin) == PARM_DECL)
	gen_formal_parameter_die (decl, origin,
				  true /* Emit name attribute.  */,
				  context_die);
      else
	gen_variable_die (decl, origin, context_die);
      break;

    case FIELD_DECL:
      gcc_assert (ctx != NULL && ctx->struct_type != NULL);
      /* Ignore the nameless fields that are used to skip bits but handle C++
	 anonymous unions and structs.  */
      if (DECL_NAME (decl) != NULL_TREE
	  || TREE_CODE (TREE_TYPE (decl)) == UNION_TYPE
	  || TREE_CODE (TREE_TYPE (decl)) == RECORD_TYPE)
	{
	  gen_type_die (member_declared_type (decl), context_die);
	  gen_field_die (decl, ctx, context_die);
	}
      break;

    case PARM_DECL:
      /* Avoid generating stray type DIEs during late dwarf dumping.
         All types have been dumped early.  */
      if (early_dwarf
	  /* ???  But in LTRANS we cannot annotate early created variably
	     modified type DIEs without copying them and adjusting all
	     references to them.  Dump them again as happens for inlining
	     which copies both the decl and the types.  */
	  /* ???  And even non-LTO needs to re-visit type DIEs to fill
	     in VLA bound information for example.  */
	  || (decl && variably_modified_type_p (TREE_TYPE (decl),
						current_function_decl)))
	{
	  if (DECL_BY_REFERENCE (decl_or_origin))
	    gen_type_die (TREE_TYPE (TREE_TYPE (decl_or_origin)), context_die);
	  else
	    gen_type_die (TREE_TYPE (decl_or_origin), context_die);
	}
      return gen_formal_parameter_die (decl, origin,
				       true /* Emit name attribute.  */,
				       context_die);

    case NAMESPACE_DECL:
      if (dwarf_version >= 3 || !dwarf_strict)
	gen_namespace_die (decl, context_die);
      break;

    case IMPORTED_DECL:
      dwarf2out_imported_module_or_decl_1 (decl, DECL_NAME (decl),
					   DECL_CONTEXT (decl), context_die);
      break;

    case NAMELIST_DECL:
      gen_namelist_decl (DECL_NAME (decl), context_die,
			 NAMELIST_DECL_ASSOCIATED_DECL (decl));
      break;

    default:
      /* Probably some frontend-internal decl.  Assume we don't care.  */
      gcc_assert ((int)TREE_CODE (decl) > NUM_TREE_CODES);
      break;
    }

  return NULL;
}

/* Output initial debug information for global DECL.  Called at the
   end of the parsing process.

   This is the initial debug generation process.  As such, the DIEs
   generated may be incomplete.  A later debug generation pass
   (dwarf2out_late_global_decl) will augment the information generated
   in this pass (e.g., with complete location info).  */

static void
dwarf2out_early_global_decl (tree decl)
{
  set_early_dwarf s;

  /* gen_decl_die() will set DECL_ABSTRACT because
     cgraph_function_possibly_inlined_p() returns true.  This is in
     turn will cause DW_AT_inline attributes to be set.

     This happens because at early dwarf generation, there is no
     cgraph information, causing cgraph_function_possibly_inlined_p()
     to return true.  Trick cgraph_function_possibly_inlined_p()
     while we generate dwarf early.  */
  bool save = symtab->global_info_ready;
  symtab->global_info_ready = true;

  /* We don't handle TYPE_DECLs.  If required, they'll be reached via
     other DECLs and they can point to template types or other things
     that dwarf2out can't handle when done via dwarf2out_decl.  */
  if (TREE_CODE (decl) != TYPE_DECL
      && TREE_CODE (decl) != PARM_DECL)
    {
      tree save_fndecl = current_function_decl;
      if (TREE_CODE (decl) == FUNCTION_DECL)
	{
	  /* For nested functions, make sure we have DIEs for the parents first
	     so that all nested DIEs are generated at the proper scope in the
	     first shot.  */
	  tree context = decl_function_context (decl);
	  if (context != NULL && lookup_decl_die (context) == NULL)
	    {
	      current_function_decl = context;
	      dwarf2out_decl (context);
	    }

	  /* Emit an abstract origin of a function first.  This happens
	     with C++ constructor clones for example and makes
	     dwarf2out_abstract_function happy which requires the early
	     DIE of the abstract instance to be present.  */
	  tree origin = DECL_ABSTRACT_ORIGIN (decl);
	  dw_die_ref origin_die;
	  if (origin != NULL
	      /* Do not emit the DIE multiple times but make sure to
	         process it fully here in case we just saw a declaration.  */
	      && ((origin_die = lookup_decl_die (origin)) == NULL
		  || is_declaration_die (origin_die)))
	    {
	      current_function_decl = origin;
	      dwarf2out_decl (origin);
	    }

	  current_function_decl = decl;
	}
      dwarf2out_decl (decl);
      if (TREE_CODE (decl) == FUNCTION_DECL)
	current_function_decl = save_fndecl;
    }
  symtab->global_info_ready = save;
}

/* Output debug information for global decl DECL.  Called from
   toplev.c after compilation proper has finished.  */

static void
dwarf2out_late_global_decl (tree decl)
{
  /* Fill-in any location information we were unable to determine
     on the first pass.  */
  if (VAR_P (decl) && !POINTER_BOUNDS_P (decl))
    {
      dw_die_ref die = lookup_decl_die (decl);

      /* We may have to generate early debug late for LTO in case debug
         was not enabled at compile-time or the target doesn't support
	 the LTO early debug scheme.  */
      if (! die && in_lto_p)
	{
	  dwarf2out_decl (decl);
	  die = lookup_decl_die (decl);
	}

      if (die)
	{
	  /* We get called via the symtab code invoking late_global_decl
	     for symbols that are optimized out.  Do not add locations
	     for those, except if they have a DECL_VALUE_EXPR, in which case
	     they are relevant for debuggers.  */
	  varpool_node *node = varpool_node::get (decl);
	  if ((! node || ! node->definition) && ! DECL_HAS_VALUE_EXPR_P (decl))
	    tree_add_const_value_attribute_for_decl (die, decl);
	  else
	    add_location_or_const_value_attribute (die, decl, false);
	}
    }
}

/* Output debug information for type decl DECL.  Called from toplev.c
   and from language front ends (to record built-in types).  */
static void
dwarf2out_type_decl (tree decl, int local)
{
  if (!local)
    {
      set_early_dwarf s;
      dwarf2out_decl (decl);
    }
}

/* Output debug information for imported module or decl DECL.
   NAME is non-NULL name in the lexical block if the decl has been renamed.
   LEXICAL_BLOCK is the lexical block (which TREE_CODE is a BLOCK)
   that DECL belongs to.
   LEXICAL_BLOCK_DIE is the DIE of LEXICAL_BLOCK.  */
static void
dwarf2out_imported_module_or_decl_1 (tree decl,
				     tree name,
				     tree lexical_block,
				     dw_die_ref lexical_block_die)
{
  expanded_location xloc;
  dw_die_ref imported_die = NULL;
  dw_die_ref at_import_die;

  if (TREE_CODE (decl) == IMPORTED_DECL)
    {
      xloc = expand_location (DECL_SOURCE_LOCATION (decl));
      decl = IMPORTED_DECL_ASSOCIATED_DECL (decl);
      gcc_assert (decl);
    }
  else
    xloc = expand_location (input_location);

  if (TREE_CODE (decl) == TYPE_DECL || TREE_CODE (decl) == CONST_DECL)
    {
      at_import_die = force_type_die (TREE_TYPE (decl));
      /* For namespace N { typedef void T; } using N::T; base_type_die
	 returns NULL, but DW_TAG_imported_declaration requires
	 the DW_AT_import tag.  Force creation of DW_TAG_typedef.  */
      if (!at_import_die)
	{
	  gcc_assert (TREE_CODE (decl) == TYPE_DECL);
	  gen_typedef_die (decl, get_context_die (DECL_CONTEXT (decl)));
	  at_import_die = lookup_type_die (TREE_TYPE (decl));
	  gcc_assert (at_import_die);
	}
    }
  else
    {
      at_import_die = lookup_decl_die (decl);
      if (!at_import_die)
	{
	  /* If we're trying to avoid duplicate debug info, we may not have
	     emitted the member decl for this field.  Emit it now.  */
	  if (TREE_CODE (decl) == FIELD_DECL)
	    {
	      tree type = DECL_CONTEXT (decl);

	      if (TYPE_CONTEXT (type)
		  && TYPE_P (TYPE_CONTEXT (type))
		  && !should_emit_struct_debug (TYPE_CONTEXT (type),
						DINFO_USAGE_DIR_USE))
		return;
	      gen_type_die_for_member (type, decl,
				       get_context_die (TYPE_CONTEXT (type)));
	    }
	  if (TREE_CODE (decl) == NAMELIST_DECL)
	    at_import_die = gen_namelist_decl (DECL_NAME (decl),
					 get_context_die (DECL_CONTEXT (decl)),
					 NULL_TREE);
	  else
	    at_import_die = force_decl_die (decl);
	}
    }

  if (TREE_CODE (decl) == NAMESPACE_DECL)
    {
      if (dwarf_version >= 3 || !dwarf_strict)
	imported_die = new_die (DW_TAG_imported_module,
				lexical_block_die,
				lexical_block);
      else
	return;
    }
  else
    imported_die = new_die (DW_TAG_imported_declaration,
			    lexical_block_die,
			    lexical_block);

  add_AT_file (imported_die, DW_AT_decl_file, lookup_filename (xloc.file));
  add_AT_unsigned (imported_die, DW_AT_decl_line, xloc.line);
  if (debug_column_info && xloc.column)
    add_AT_unsigned (imported_die, DW_AT_decl_column, xloc.column);
  if (name)
    add_AT_string (imported_die, DW_AT_name,
		   IDENTIFIER_POINTER (name));
  add_AT_die_ref (imported_die, DW_AT_import, at_import_die);
}

/* Output debug information for imported module or decl DECL.
   NAME is non-NULL name in context if the decl has been renamed.
   CHILD is true if decl is one of the renamed decls as part of
   importing whole module.
   IMPLICIT is set if this hook is called for an implicit import
   such as inline namespace.  */

static void
dwarf2out_imported_module_or_decl (tree decl, tree name, tree context,
				   bool child, bool implicit)
{
  /* dw_die_ref at_import_die;  */
  dw_die_ref scope_die;

  if (debug_info_level <= DINFO_LEVEL_TERSE)
    return;

  gcc_assert (decl);

  /* For DWARF5, just DW_AT_export_symbols on the DW_TAG_namespace
     should be enough, for DWARF4 and older even if we emit as extension
     DW_AT_export_symbols add the implicit DW_TAG_imported_module anyway
     for the benefit of consumers unaware of DW_AT_export_symbols.  */
  if (implicit
      && dwarf_version >= 5
      && lang_hooks.decls.decl_dwarf_attribute (decl,
						DW_AT_export_symbols) == 1)
    return;

  set_early_dwarf s;

  /* To emit DW_TAG_imported_module or DW_TAG_imported_decl, we need two DIEs.
     We need decl DIE for reference and scope die. First, get DIE for the decl
     itself.  */

  /* Get the scope die for decl context. Use comp_unit_die for global module
     or decl. If die is not found for non globals, force new die.  */
  if (context
      && TYPE_P (context)
      && !should_emit_struct_debug (context, DINFO_USAGE_DIR_USE))
    return;

  scope_die = get_context_die (context);

  if (child)
    {
      /* DW_TAG_imported_module was introduced in the DWARFv3 specification, so
	 there is nothing we can do, here.  */
      if (dwarf_version < 3 && dwarf_strict)
	return;

      gcc_assert (scope_die->die_child);
      gcc_assert (scope_die->die_child->die_tag == DW_TAG_imported_module);
      gcc_assert (TREE_CODE (decl) != NAMESPACE_DECL);
      scope_die = scope_die->die_child;
    }

  /* OK, now we have DIEs for decl as well as scope. Emit imported die.  */
  dwarf2out_imported_module_or_decl_1 (decl, name, context, scope_die);
}

/* Output debug information for namelists.   */

static dw_die_ref
gen_namelist_decl (tree name, dw_die_ref scope_die, tree item_decls)
{
  dw_die_ref nml_die, nml_item_die, nml_item_ref_die;
  tree value;
  unsigned i;

  if (debug_info_level <= DINFO_LEVEL_TERSE)
    return NULL;

  gcc_assert (scope_die != NULL);
  nml_die = new_die (DW_TAG_namelist, scope_die, NULL);
  add_AT_string (nml_die, DW_AT_name, IDENTIFIER_POINTER (name));

  /* If there are no item_decls, we have a nondefining namelist, e.g.
     with USE association; hence, set DW_AT_declaration.  */
  if (item_decls == NULL_TREE)
    {
      add_AT_flag (nml_die, DW_AT_declaration, 1);
      return nml_die;
    }

  FOR_EACH_CONSTRUCTOR_VALUE (CONSTRUCTOR_ELTS (item_decls), i, value)
    {
      nml_item_ref_die = lookup_decl_die (value);
      if (!nml_item_ref_die)
	nml_item_ref_die = force_decl_die (value);

      nml_item_die = new_die (DW_TAG_namelist_item, nml_die, NULL);
      add_AT_die_ref (nml_item_die, DW_AT_namelist_items, nml_item_ref_die);
    }
  return nml_die;
}


/* Write the debugging output for DECL and return the DIE.  */

static void
dwarf2out_decl (tree decl)
{
  dw_die_ref context_die = comp_unit_die ();

  switch (TREE_CODE (decl))
    {
    case ERROR_MARK:
      return;

    case FUNCTION_DECL:
      /* If we're a nested function, initially use a parent of NULL; if we're
	 a plain function, this will be fixed up in decls_for_scope.  If
	 we're a method, it will be ignored, since we already have a DIE.  */
      if (decl_function_context (decl)
	  /* But if we're in terse mode, we don't care about scope.  */
	  && debug_info_level > DINFO_LEVEL_TERSE)
	context_die = NULL;
      break;

    case VAR_DECL:
      /* For local statics lookup proper context die.  */
      if (local_function_static (decl))
	context_die = lookup_decl_die (DECL_CONTEXT (decl));

      /* If we are in terse mode, don't generate any DIEs to represent any
	 variable declarations or definitions.  */
      if (debug_info_level <= DINFO_LEVEL_TERSE)
	return;
      break;

    case CONST_DECL:
      if (debug_info_level <= DINFO_LEVEL_TERSE)
	return;
      if (!is_fortran () && !is_ada ())
	return;
      if (TREE_STATIC (decl) && decl_function_context (decl))
	context_die = lookup_decl_die (DECL_CONTEXT (decl));
      break;

    case NAMESPACE_DECL:
    case IMPORTED_DECL:
      if (debug_info_level <= DINFO_LEVEL_TERSE)
	return;
      if (lookup_decl_die (decl) != NULL)
	return;
      break;

    case TYPE_DECL:
      /* Don't emit stubs for types unless they are needed by other DIEs.  */
      if (TYPE_DECL_SUPPRESS_DEBUG (decl))
	return;

      /* Don't bother trying to generate any DIEs to represent any of the
	 normal built-in types for the language we are compiling.  */
      if (DECL_IS_BUILTIN (decl))
	return;

      /* If we are in terse mode, don't generate any DIEs for types.  */
      if (debug_info_level <= DINFO_LEVEL_TERSE)
	return;

      /* If we're a function-scope tag, initially use a parent of NULL;
	 this will be fixed up in decls_for_scope.  */
      if (decl_function_context (decl))
	context_die = NULL;

      break;

    case NAMELIST_DECL:
      break;

    default:
      return;
    }

  gen_decl_die (decl, NULL, NULL, context_die);

  if (flag_checking)
    {
      dw_die_ref die = lookup_decl_die (decl);
      if (die)
	check_die (die);
    }
}

/* Write the debugging output for DECL.  */

static void
dwarf2out_function_decl (tree decl)
{
  dwarf2out_decl (decl);
  call_arg_locations = NULL;
  call_arg_loc_last = NULL;
  call_site_count = -1;
  tail_call_site_count = -1;
  decl_loc_table->empty ();
  cached_dw_loc_list_table->empty ();
}

/* Output a marker (i.e. a label) for the beginning of the generated code for
   a lexical block.  */

static void
dwarf2out_begin_block (unsigned int line ATTRIBUTE_UNUSED,
		       unsigned int blocknum)
{
  switch_to_section (current_function_section ());
  ASM_OUTPUT_DEBUG_LABEL (asm_out_file, BLOCK_BEGIN_LABEL, blocknum);
}

/* Output a marker (i.e. a label) for the end of the generated code for a
   lexical block.  */

static void
dwarf2out_end_block (unsigned int line ATTRIBUTE_UNUSED, unsigned int blocknum)
{
  switch_to_section (current_function_section ());
  ASM_OUTPUT_DEBUG_LABEL (asm_out_file, BLOCK_END_LABEL, blocknum);
}

/* Returns nonzero if it is appropriate not to emit any debugging
   information for BLOCK, because it doesn't contain any instructions.

   Don't allow this for blocks with nested functions or local classes
   as we would end up with orphans, and in the presence of scheduling
   we may end up calling them anyway.  */

static bool
dwarf2out_ignore_block (const_tree block)
{
  tree decl;
  unsigned int i;

  for (decl = BLOCK_VARS (block); decl; decl = DECL_CHAIN (decl))
    if (TREE_CODE (decl) == FUNCTION_DECL
	|| (TREE_CODE (decl) == TYPE_DECL && TYPE_DECL_IS_STUB (decl)))
      return 0;
  for (i = 0; i < BLOCK_NUM_NONLOCALIZED_VARS (block); i++)
    {
      decl = BLOCK_NONLOCALIZED_VAR (block, i);
      if (TREE_CODE (decl) == FUNCTION_DECL
	  || (TREE_CODE (decl) == TYPE_DECL && TYPE_DECL_IS_STUB (decl)))
      return 0;
    }

  return 1;
}

/* Hash table routines for file_hash.  */

bool
dwarf_file_hasher::equal (dwarf_file_data *p1, const char *p2)
{
  return filename_cmp (p1->filename, p2) == 0;
}

hashval_t
dwarf_file_hasher::hash (dwarf_file_data *p)
{
  return htab_hash_string (p->filename);
}

/* Lookup FILE_NAME (in the list of filenames that we know about here in
   dwarf2out.c) and return its "index".  The index of each (known) filename is
   just a unique number which is associated with only that one filename.  We
   need such numbers for the sake of generating labels (in the .debug_sfnames
   section) and references to those files numbers (in the .debug_srcinfo
   and .debug_macinfo sections).  If the filename given as an argument is not
   found in our current list, add it to the list and assign it the next
   available unique index number.  */

static struct dwarf_file_data *
lookup_filename (const char *file_name)
{
  struct dwarf_file_data * created;

  if (!file_name)
    return NULL;

  dwarf_file_data **slot
    = file_table->find_slot_with_hash (file_name, htab_hash_string (file_name),
				       INSERT);
  if (*slot)
    return *slot;

  created = ggc_alloc<dwarf_file_data> ();
  created->filename = file_name;
  created->emitted_number = 0;
  *slot = created;
  return created;
}

/* If the assembler will construct the file table, then translate the compiler
   internal file table number into the assembler file table number, and emit
   a .file directive if we haven't already emitted one yet.  The file table
   numbers are different because we prune debug info for unused variables and
   types, which may include filenames.  */

static int
maybe_emit_file (struct dwarf_file_data * fd)
{
  if (! fd->emitted_number)
    {
      if (last_emitted_file)
	fd->emitted_number = last_emitted_file->emitted_number + 1;
      else
	fd->emitted_number = 1;
      last_emitted_file = fd;

      if (DWARF2_ASM_LINE_DEBUG_INFO)
	{
	  fprintf (asm_out_file, "\t.file %u ", fd->emitted_number);
	  output_quoted_string (asm_out_file,
				remap_debug_filename (fd->filename));
	  fputc ('\n', asm_out_file);
	}
    }

  return fd->emitted_number;
}

/* Schedule generation of a DW_AT_const_value attribute to DIE.
   That generation should happen after function debug info has been
   generated. The value of the attribute is the constant value of ARG.  */

static void
append_entry_to_tmpl_value_parm_die_table (dw_die_ref die, tree arg)
{
  die_arg_entry entry;

  if (!die || !arg)
    return;

  gcc_assert (early_dwarf);

  if (!tmpl_value_parm_die_table)
    vec_alloc (tmpl_value_parm_die_table, 32);

  entry.die = die;
  entry.arg = arg;
  vec_safe_push (tmpl_value_parm_die_table, entry);
}

/* Return TRUE if T is an instance of generic type, FALSE
   otherwise.  */

static bool
generic_type_p (tree t)
{
  if (t == NULL_TREE || !TYPE_P (t))
    return false;
  return lang_hooks.get_innermost_generic_parms (t) != NULL_TREE;
}

/* Schedule the generation of the generic parameter dies for the
  instance of generic type T. The proper generation itself is later
  done by gen_scheduled_generic_parms_dies. */

static void
schedule_generic_params_dies_gen (tree t)
{
  if (!generic_type_p (t))
    return;

  gcc_assert (early_dwarf);

  if (!generic_type_instances)
    vec_alloc (generic_type_instances, 256);

  vec_safe_push (generic_type_instances, t);
}

/* Add a DW_AT_const_value attribute to DIEs that were scheduled
   by append_entry_to_tmpl_value_parm_die_table. This function must
   be called after function DIEs have been generated.  */

static void
gen_remaining_tmpl_value_param_die_attribute (void)
{
  if (tmpl_value_parm_die_table)
    {
      unsigned i, j;
      die_arg_entry *e;

      /* We do this in two phases - first get the cases we can
	 handle during early-finish, preserving those we cannot
	 (containing symbolic constants where we don't yet know
	 whether we are going to output the referenced symbols).
	 For those we try again at late-finish.  */
      j = 0;
      FOR_EACH_VEC_ELT (*tmpl_value_parm_die_table, i, e)
	{
	  if (!e->die->removed
	      && !tree_add_const_value_attribute (e->die, e->arg))
	    {
	      dw_loc_descr_ref loc = NULL;
	      if (! early_dwarf
		  && (dwarf_version >= 5 || !dwarf_strict))
		loc = loc_descriptor_from_tree (e->arg, 2, NULL);
	      if (loc)
		add_AT_loc (e->die, DW_AT_location, loc);
	      else
		(*tmpl_value_parm_die_table)[j++] = *e;
	    }
	}
      tmpl_value_parm_die_table->truncate (j);
    }
}

/* Generate generic parameters DIEs for instances of generic types
   that have been previously scheduled by
   schedule_generic_params_dies_gen. This function must be called
   after all the types of the CU have been laid out.  */

static void
gen_scheduled_generic_parms_dies (void)
{
  unsigned i;
  tree t;

  if (!generic_type_instances)
    return;
  
  FOR_EACH_VEC_ELT (*generic_type_instances, i, t)
    if (COMPLETE_TYPE_P (t))
      gen_generic_params_dies (t);

  generic_type_instances = NULL;
}


/* Replace DW_AT_name for the decl with name.  */

static void
dwarf2out_set_name (tree decl, tree name)
{
  dw_die_ref die;
  dw_attr_node *attr;
  const char *dname;

  die = TYPE_SYMTAB_DIE (decl);
  if (!die)
    return;

  dname = dwarf2_name (name, 0);
  if (!dname)
    return;

  attr = get_AT (die, DW_AT_name);
  if (attr)
    {
      struct indirect_string_node *node;

      node = find_AT_string (dname);
      /* replace the string.  */
      attr->dw_attr_val.v.val_str = node;
    }

  else
    add_name_attribute (die, dname);
}

/* True if before or during processing of the first function being emitted.  */
static bool in_first_function_p = true;
/* True if loc_note during dwarf2out_var_location call might still be
   before first real instruction at address equal to .Ltext0.  */
static bool maybe_at_text_label_p = true;
/* One above highest N where .LVLN label might be equal to .Ltext0 label.  */
static unsigned int first_loclabel_num_not_at_text_label;

/* Called by the final INSN scan whenever we see a var location.  We
   use it to drop labels in the right places, and throw the location in
   our lookup table.  */

static void
dwarf2out_var_location (rtx_insn *loc_note)
{
  char loclabel[MAX_ARTIFICIAL_LABEL_BYTES + 2];
  struct var_loc_node *newloc;
  rtx_insn *next_real, *next_note;
  rtx_insn *call_insn = NULL;
  static const char *last_label;
  static const char *last_postcall_label;
  static bool last_in_cold_section_p;
  static rtx_insn *expected_next_loc_note;
  tree decl;
  bool var_loc_p;

  if (!NOTE_P (loc_note))
    {
      if (CALL_P (loc_note))
	{
	  call_site_count++;
	  if (SIBLING_CALL_P (loc_note))
	    tail_call_site_count++;
	  if (optimize == 0 && !flag_var_tracking)
	    {
	      /* When the var-tracking pass is not running, there is no note
		 for indirect calls whose target is compile-time known. In this
		 case, process such calls specifically so that we generate call
		 sites for them anyway.  */
	      rtx x = PATTERN (loc_note);
	      if (GET_CODE (x) == PARALLEL)
		x = XVECEXP (x, 0, 0);
	      if (GET_CODE (x) == SET)
		x = SET_SRC (x);
	      if (GET_CODE (x) == CALL)
		x = XEXP (x, 0);
	      if (!MEM_P (x)
		  || GET_CODE (XEXP (x, 0)) != SYMBOL_REF
		  || !SYMBOL_REF_DECL (XEXP (x, 0))
		  || (TREE_CODE (SYMBOL_REF_DECL (XEXP (x, 0)))
		      != FUNCTION_DECL))
		{
		  call_insn = loc_note;
		  loc_note = NULL;
		  var_loc_p = false;

		  next_real = next_real_insn (call_insn);
		  next_note = NULL;
		  cached_next_real_insn = NULL;
		  goto create_label;
		}
	    }
	}
      return;
    }

  var_loc_p = NOTE_KIND (loc_note) == NOTE_INSN_VAR_LOCATION;
  if (var_loc_p && !DECL_P (NOTE_VAR_LOCATION_DECL (loc_note)))
    return;

  /* Optimize processing a large consecutive sequence of location
     notes so we don't spend too much time in next_real_insn.  If the
     next insn is another location note, remember the next_real_insn
     calculation for next time.  */
  next_real = cached_next_real_insn;
  if (next_real)
    {
      if (expected_next_loc_note != loc_note)
	next_real = NULL;
    }

  next_note = NEXT_INSN (loc_note);
  if (! next_note
      || next_note->deleted ()
      || ! NOTE_P (next_note)
      || (NOTE_KIND (next_note) != NOTE_INSN_VAR_LOCATION
	  && NOTE_KIND (next_note) != NOTE_INSN_CALL_ARG_LOCATION))
    next_note = NULL;

  if (! next_real)
    next_real = next_real_insn (loc_note);

  if (next_note)
    {
      expected_next_loc_note = next_note;
      cached_next_real_insn = next_real;
    }
  else
    cached_next_real_insn = NULL;

  /* If there are no instructions which would be affected by this note,
     don't do anything.  */
  if (var_loc_p
      && next_real == NULL_RTX
      && !NOTE_DURING_CALL_P (loc_note))
    return;

create_label:

  if (next_real == NULL_RTX)
    next_real = get_last_insn ();

  /* If there were any real insns between note we processed last time
     and this note (or if it is the first note), clear
     last_{,postcall_}label so that they are not reused this time.  */
  if (last_var_location_insn == NULL_RTX
      || last_var_location_insn != next_real
      || last_in_cold_section_p != in_cold_section_p)
    {
      last_label = NULL;
      last_postcall_label = NULL;
    }

  if (var_loc_p)
    {
      decl = NOTE_VAR_LOCATION_DECL (loc_note);
      newloc = add_var_loc_to_decl (decl, loc_note,
				    NOTE_DURING_CALL_P (loc_note)
				    ? last_postcall_label : last_label);
      if (newloc == NULL)
	return;
    }
  else
    {
      decl = NULL_TREE;
      newloc = NULL;
    }

  /* If there were no real insns between note we processed last time
     and this note, use the label we emitted last time.  Otherwise
     create a new label and emit it.  */
  if (last_label == NULL)
    {
      ASM_GENERATE_INTERNAL_LABEL (loclabel, "LVL", loclabel_num);
      ASM_OUTPUT_DEBUG_LABEL (asm_out_file, "LVL", loclabel_num);
      loclabel_num++;
      last_label = ggc_strdup (loclabel);
      /* See if loclabel might be equal to .Ltext0.  If yes,
	 bump first_loclabel_num_not_at_text_label.  */
      if (!have_multiple_function_sections
	  && in_first_function_p
	  && maybe_at_text_label_p)
	{
	  static rtx_insn *last_start;
	  rtx_insn *insn;
	  for (insn = loc_note; insn; insn = previous_insn (insn))
	    if (insn == last_start)
	      break;
	    else if (!NONDEBUG_INSN_P (insn))
	      continue;
	    else
	      {
		rtx body = PATTERN (insn);
		if (GET_CODE (body) == USE || GET_CODE (body) == CLOBBER)
		  continue;
		/* Inline asm could occupy zero bytes.  */
		else if (GET_CODE (body) == ASM_INPUT
			 || asm_noperands (body) >= 0)
		  continue;
#ifdef HAVE_attr_length
		else if (get_attr_min_length (insn) == 0)
		  continue;
#endif
		else
		  {
		    /* Assume insn has non-zero length.  */
		    maybe_at_text_label_p = false;
		    break;
		  }
	      }
	  if (maybe_at_text_label_p)
	    {
	      last_start = loc_note;
	      first_loclabel_num_not_at_text_label = loclabel_num;
	    }
	}
    }

  gcc_assert ((loc_note == NULL_RTX && call_insn != NULL_RTX)
	      || (loc_note != NULL_RTX && call_insn == NULL_RTX));

  if (!var_loc_p)
    {
      struct call_arg_loc_node *ca_loc
	= ggc_cleared_alloc<call_arg_loc_node> ();
      rtx_insn *prev
        = loc_note != NULL_RTX ? prev_real_insn (loc_note) : call_insn;

      ca_loc->call_arg_loc_note = loc_note;
      ca_loc->next = NULL;
      ca_loc->label = last_label;
      gcc_assert (prev
		  && (CALL_P (prev)
		      || (NONJUMP_INSN_P (prev)
			  && GET_CODE (PATTERN (prev)) == SEQUENCE
			  && CALL_P (XVECEXP (PATTERN (prev), 0, 0)))));
      if (!CALL_P (prev))
	prev = as_a <rtx_sequence *> (PATTERN (prev))->insn (0);
      ca_loc->tail_call_p = SIBLING_CALL_P (prev);

      /* Look for a SYMBOL_REF in the "prev" instruction.  */
      rtx x = get_call_rtx_from (PATTERN (prev));
      if (x)
	{
	  /* Try to get the call symbol, if any.  */
	  if (MEM_P (XEXP (x, 0)))
	    x = XEXP (x, 0);
	  /* First, look for a memory access to a symbol_ref.  */
	  if (GET_CODE (XEXP (x, 0)) == SYMBOL_REF
	      && SYMBOL_REF_DECL (XEXP (x, 0))
	      && TREE_CODE (SYMBOL_REF_DECL (XEXP (x, 0))) == FUNCTION_DECL)
	    ca_loc->symbol_ref = XEXP (x, 0);
	  /* Otherwise, look at a compile-time known user-level function
	     declaration.  */
	  else if (MEM_P (x)
		   && MEM_EXPR (x)
		   && TREE_CODE (MEM_EXPR (x)) == FUNCTION_DECL)
	    ca_loc->symbol_ref = XEXP (DECL_RTL (MEM_EXPR (x)), 0);
	}

      ca_loc->block = insn_scope (prev);
      if (call_arg_locations)
	call_arg_loc_last->next = ca_loc;
      else
	call_arg_locations = ca_loc;
      call_arg_loc_last = ca_loc;
    }
  else if (loc_note != NULL_RTX && !NOTE_DURING_CALL_P (loc_note))
    newloc->label = last_label;
  else
    {
      if (!last_postcall_label)
	{
	  sprintf (loclabel, "%s-1", last_label);
	  last_postcall_label = ggc_strdup (loclabel);
	}
      newloc->label = last_postcall_label;
    }

  last_var_location_insn = next_real;
  last_in_cold_section_p = in_cold_section_p;
}

/* Called from finalize_size_functions for size functions so that their body
   can be encoded in the debug info to describe the layout of variable-length
   structures.  */

static void
dwarf2out_size_function (tree decl)
{
  function_to_dwarf_procedure (decl);
}

/* Note in one location list that text section has changed.  */

int
var_location_switch_text_section_1 (var_loc_list **slot, void *)
{
  var_loc_list *list = *slot;
  if (list->first)
    list->last_before_switch
      = list->last->next ? list->last->next : list->last;
  return 1;
}

/* Note in all location lists that text section has changed.  */

static void
var_location_switch_text_section (void)
{
  if (decl_loc_table == NULL)
    return;

  decl_loc_table->traverse<void *, var_location_switch_text_section_1> (NULL);
}

/* Create a new line number table.  */

static dw_line_info_table *
new_line_info_table (void)
{
  dw_line_info_table *table;

  table = ggc_cleared_alloc<dw_line_info_table> ();
  table->file_num = 1;
  table->line_num = 1;
  table->is_stmt = DWARF_LINE_DEFAULT_IS_STMT_START;

  return table;
}

/* Lookup the "current" table into which we emit line info, so
   that we don't have to do it for every source line.  */

static void
set_cur_line_info_table (section *sec)
{
  dw_line_info_table *table;

  if (sec == text_section)
    table = text_section_line_info;
  else if (sec == cold_text_section)
    {
      table = cold_text_section_line_info;
      if (!table)
	{
	  cold_text_section_line_info = table = new_line_info_table ();
	  table->end_label = cold_end_label;
	}
    }
  else
    {
      const char *end_label;

      if (crtl->has_bb_partition)
	{
	  if (in_cold_section_p)
	    end_label = crtl->subsections.cold_section_end_label;
	  else
	    end_label = crtl->subsections.hot_section_end_label;
	}
      else
	{
	  char label[MAX_ARTIFICIAL_LABEL_BYTES];
	  ASM_GENERATE_INTERNAL_LABEL (label, FUNC_END_LABEL,
				       current_function_funcdef_no);
	  end_label = ggc_strdup (label);
	}

      table = new_line_info_table ();
      table->end_label = end_label;

      vec_safe_push (separate_line_info, table);
    }

  if (DWARF2_ASM_LINE_DEBUG_INFO)
    table->is_stmt = (cur_line_info_table
		      ? cur_line_info_table->is_stmt
		      : DWARF_LINE_DEFAULT_IS_STMT_START);
  cur_line_info_table = table;
}


/* We need to reset the locations at the beginning of each
   function. We can't do this in the end_function hook, because the
   declarations that use the locations won't have been output when
   that hook is called.  Also compute have_multiple_function_sections here.  */

static void
dwarf2out_begin_function (tree fun)
{
  section *sec = function_section (fun);

  if (sec != text_section)
    have_multiple_function_sections = true;

  if (crtl->has_bb_partition && !cold_text_section)
    {
      gcc_assert (current_function_decl == fun);
      cold_text_section = unlikely_text_section ();
      switch_to_section (cold_text_section);
      ASM_OUTPUT_LABEL (asm_out_file, cold_text_section_label);
      switch_to_section (sec);
    }

  dwarf2out_note_section_used ();
  call_site_count = 0;
  tail_call_site_count = 0;

  set_cur_line_info_table (sec);
}

/* Helper function of dwarf2out_end_function, called only after emitting
   the very first function into assembly.  Check if some .debug_loc range
   might end with a .LVL* label that could be equal to .Ltext0.
   In that case we must force using absolute addresses in .debug_loc ranges,
   because this range could be .LVLN-.Ltext0 .. .LVLM-.Ltext0 for
   .LVLN == .LVLM == .Ltext0, thus 0 .. 0, which is a .debug_loc
   list terminator.
   Set have_multiple_function_sections to true in that case and
   terminate htab traversal.  */

int
find_empty_loc_ranges_at_text_label (var_loc_list **slot, int)
{
  var_loc_list *entry = *slot;
  struct var_loc_node *node;

  node = entry->first;
  if (node && node->next && node->next->label)
    {
      unsigned int i;
      const char *label = node->next->label;
      char loclabel[MAX_ARTIFICIAL_LABEL_BYTES];

      for (i = 0; i < first_loclabel_num_not_at_text_label; i++)
	{
	  ASM_GENERATE_INTERNAL_LABEL (loclabel, "LVL", i);
	  if (strcmp (label, loclabel) == 0)
	    {
	      have_multiple_function_sections = true;
	      return 0;
	    }
	}
    }
  return 1;
}

/* Hook called after emitting a function into assembly.
   This does something only for the very first function emitted.  */

static void
dwarf2out_end_function (unsigned int)
{
  if (in_first_function_p
      && !have_multiple_function_sections
      && first_loclabel_num_not_at_text_label
      && decl_loc_table)
    decl_loc_table->traverse<int, find_empty_loc_ranges_at_text_label> (0);
  in_first_function_p = false;
  maybe_at_text_label_p = false;
}

/* Temporary holder for dwarf2out_register_main_translation_unit.  Used to let
   front-ends register a translation unit even before dwarf2out_init is
   called.  */
static tree main_translation_unit = NULL_TREE;

/* Hook called by front-ends after they built their main translation unit.
   Associate comp_unit_die to UNIT.  */

static void
dwarf2out_register_main_translation_unit (tree unit)
{
  gcc_assert (TREE_CODE (unit) == TRANSLATION_UNIT_DECL
	      && main_translation_unit == NULL_TREE);
  main_translation_unit = unit;
  /* If dwarf2out_init has not been called yet, it will perform the association
     itself looking at main_translation_unit.  */
  if (decl_die_table != NULL)
    equate_decl_number_to_die (unit, comp_unit_die ());
}

/* Add OPCODE+VAL as an entry at the end of the opcode array in TABLE.  */

static void
push_dw_line_info_entry (dw_line_info_table *table,
			 enum dw_line_info_opcode opcode, unsigned int val)
{
  dw_line_info_entry e;
  e.opcode = opcode;
  e.val = val;
  vec_safe_push (table->entries, e);
}

/* Output a label to mark the beginning of a source code line entry
   and record information relating to this source line, in
   'line_info_table' for later output of the .debug_line section.  */
/* ??? The discriminator parameter ought to be unsigned.  */

static void
dwarf2out_source_line (unsigned int line, unsigned int column,
		       const char *filename,
                       int discriminator, bool is_stmt)
{
  unsigned int file_num;
  dw_line_info_table *table;

  if (debug_info_level < DINFO_LEVEL_TERSE || line == 0)
    return;

  /* The discriminator column was added in dwarf4.  Simplify the below
     by simply removing it if we're not supposed to output it.  */
  if (dwarf_version < 4 && dwarf_strict)
    discriminator = 0;

  if (!debug_column_info)
    column = 0;

  table = cur_line_info_table;
  file_num = maybe_emit_file (lookup_filename (filename));

  /* ??? TODO: Elide duplicate line number entries.  Traditionally,
     the debugger has used the second (possibly duplicate) line number
     at the beginning of the function to mark the end of the prologue.
     We could eliminate any other duplicates within the function.  For
     Dwarf3, we ought to include the DW_LNS_set_prologue_end mark in
     that second line number entry.  */
  /* Recall that this end-of-prologue indication is *not* the same thing
     as the end_prologue debug hook.  The NOTE_INSN_PROLOGUE_END note,
     to which the hook corresponds, follows the last insn that was 
     emitted by gen_prologue.  What we need is to precede the first insn
     that had been emitted after NOTE_INSN_FUNCTION_BEG, i.e. the first
     insn that corresponds to something the user wrote.  These may be
     very different locations once scheduling is enabled.  */

  if (0 && file_num == table->file_num
      && line == table->line_num
      && column == table->column_num
      && discriminator == table->discrim_num
      && is_stmt == table->is_stmt)
    return;

  switch_to_section (current_function_section ());

  /* If requested, emit something human-readable.  */
  if (flag_debug_asm)
    {
      if (debug_column_info)
	fprintf (asm_out_file, "\t%s %s:%d:%d\n", ASM_COMMENT_START,
		 filename, line, column);
      else
	fprintf (asm_out_file, "\t%s %s:%d\n", ASM_COMMENT_START,
		 filename, line);
    }

  if (DWARF2_ASM_LINE_DEBUG_INFO)
    {
      /* Emit the .loc directive understood by GNU as.  */
      /* "\t.loc %u %u 0 is_stmt %u discriminator %u",
	 file_num, line, is_stmt, discriminator */
      fputs ("\t.loc ", asm_out_file);
      fprint_ul (asm_out_file, file_num);
      putc (' ', asm_out_file);
      fprint_ul (asm_out_file, line);
      putc (' ', asm_out_file);
      if (debug_column_info)
	fprint_ul (asm_out_file, column);
      else
	putc ('0', asm_out_file);

      if (is_stmt != table->is_stmt)
	{
	  fputs (" is_stmt ", asm_out_file);
	  putc (is_stmt ? '1' : '0', asm_out_file);
	}
      if (SUPPORTS_DISCRIMINATOR && discriminator != 0)
	{
	  gcc_assert (discriminator > 0);
	  fputs (" discriminator ", asm_out_file);
	  fprint_ul (asm_out_file, (unsigned long) discriminator);
	}
      putc ('\n', asm_out_file);
    }
  else
    {
      unsigned int label_num = ++line_info_label_num;

      targetm.asm_out.internal_label (asm_out_file, LINE_CODE_LABEL, label_num);

      push_dw_line_info_entry (table, LI_set_address, label_num);
      if (file_num != table->file_num)
	push_dw_line_info_entry (table, LI_set_file, file_num);
      if (discriminator != table->discrim_num)
	push_dw_line_info_entry (table, LI_set_discriminator, discriminator);
      if (is_stmt != table->is_stmt)
	push_dw_line_info_entry (table, LI_negate_stmt, 0);
      push_dw_line_info_entry (table, LI_set_line, line);
      if (debug_column_info)
	push_dw_line_info_entry (table, LI_set_column, column);
    }

  table->file_num = file_num;
  table->line_num = line;
  table->column_num = column;
  table->discrim_num = discriminator;
  table->is_stmt = is_stmt;
  table->in_use = true;
}

/* Record the beginning of a new source file.  */

static void
dwarf2out_start_source_file (unsigned int lineno, const char *filename)
{
  if (debug_info_level >= DINFO_LEVEL_VERBOSE)
    {
      macinfo_entry e;
      e.code = DW_MACINFO_start_file;
      e.lineno = lineno;
      e.info = ggc_strdup (filename);
      vec_safe_push (macinfo_table, e);
    }
}

/* Record the end of a source file.  */

static void
dwarf2out_end_source_file (unsigned int lineno ATTRIBUTE_UNUSED)
{
  if (debug_info_level >= DINFO_LEVEL_VERBOSE)
    {
      macinfo_entry e;
      e.code = DW_MACINFO_end_file;
      e.lineno = lineno;
      e.info = NULL;
      vec_safe_push (macinfo_table, e);
    }
}

/* Called from debug_define in toplev.c.  The `buffer' parameter contains
   the tail part of the directive line, i.e. the part which is past the
   initial whitespace, #, whitespace, directive-name, whitespace part.  */

static void
dwarf2out_define (unsigned int lineno ATTRIBUTE_UNUSED,
		  const char *buffer ATTRIBUTE_UNUSED)
{
  if (debug_info_level >= DINFO_LEVEL_VERBOSE)
    {
      macinfo_entry e;
      /* Insert a dummy first entry to be able to optimize the whole
	 predefined macro block using DW_MACRO_import.  */
      if (macinfo_table->is_empty () && lineno <= 1)
	{
	  e.code = 0;
	  e.lineno = 0;
	  e.info = NULL;
	  vec_safe_push (macinfo_table, e);
	}
      e.code = DW_MACINFO_define;
      e.lineno = lineno;
      e.info = ggc_strdup (buffer);
      vec_safe_push (macinfo_table, e);
    }
}

/* Called from debug_undef in toplev.c.  The `buffer' parameter contains
   the tail part of the directive line, i.e. the part which is past the
   initial whitespace, #, whitespace, directive-name, whitespace part.  */

static void
dwarf2out_undef (unsigned int lineno ATTRIBUTE_UNUSED,
		 const char *buffer ATTRIBUTE_UNUSED)
{
  if (debug_info_level >= DINFO_LEVEL_VERBOSE)
    {
      macinfo_entry e;
      /* Insert a dummy first entry to be able to optimize the whole
	 predefined macro block using DW_MACRO_import.  */
      if (macinfo_table->is_empty () && lineno <= 1)
	{
	  e.code = 0;
	  e.lineno = 0;
	  e.info = NULL;
	  vec_safe_push (macinfo_table, e);
	}
      e.code = DW_MACINFO_undef;
      e.lineno = lineno;
      e.info = ggc_strdup (buffer);
      vec_safe_push (macinfo_table, e);
    }
}

/* Helpers to manipulate hash table of CUs.  */

struct macinfo_entry_hasher : nofree_ptr_hash <macinfo_entry>
{
  static inline hashval_t hash (const macinfo_entry *);
  static inline bool equal (const macinfo_entry *, const macinfo_entry *);
};

inline hashval_t
macinfo_entry_hasher::hash (const macinfo_entry *entry)
{
  return htab_hash_string (entry->info);
}

inline bool
macinfo_entry_hasher::equal (const macinfo_entry *entry1,
			     const macinfo_entry *entry2)
{
  return !strcmp (entry1->info, entry2->info);
}

typedef hash_table<macinfo_entry_hasher> macinfo_hash_type;

/* Output a single .debug_macinfo entry.  */

static void
output_macinfo_op (macinfo_entry *ref)
{
  int file_num;
  size_t len;
  struct indirect_string_node *node;
  char label[MAX_ARTIFICIAL_LABEL_BYTES];
  struct dwarf_file_data *fd;

  switch (ref->code)
    {
    case DW_MACINFO_start_file:
      fd = lookup_filename (ref->info);
      file_num = maybe_emit_file (fd);
      dw2_asm_output_data (1, DW_MACINFO_start_file, "Start new file");
      dw2_asm_output_data_uleb128 (ref->lineno,
				   "Included from line number %lu", 
				   (unsigned long) ref->lineno);
      dw2_asm_output_data_uleb128 (file_num, "file %s", ref->info);
      break;
    case DW_MACINFO_end_file:
      dw2_asm_output_data (1, DW_MACINFO_end_file, "End file");
      break;
    case DW_MACINFO_define:
    case DW_MACINFO_undef:
      len = strlen (ref->info) + 1;
      if (!dwarf_strict
	  && len > DWARF_OFFSET_SIZE
	  && !DWARF2_INDIRECT_STRING_SUPPORT_MISSING_ON_TARGET
	  && (debug_str_section->common.flags & SECTION_MERGE) != 0)
	{
	  ref->code = ref->code == DW_MACINFO_define
		      ? DW_MACRO_define_strp : DW_MACRO_undef_strp;
	  output_macinfo_op (ref);
	  return;
	}
      dw2_asm_output_data (1, ref->code,
			   ref->code == DW_MACINFO_define
			   ? "Define macro" : "Undefine macro");
      dw2_asm_output_data_uleb128 (ref->lineno, "At line number %lu", 
				   (unsigned long) ref->lineno);
      dw2_asm_output_nstring (ref->info, -1, "The macro");
      break;
    case DW_MACRO_define_strp:
    case DW_MACRO_undef_strp:
      node = find_AT_string (ref->info);
      gcc_assert (node
		  && (node->form == DW_FORM_strp
		      || node->form == DW_FORM_GNU_str_index));
      dw2_asm_output_data (1, ref->code,
			   ref->code == DW_MACRO_define_strp
			   ? "Define macro strp"
			   : "Undefine macro strp");
      dw2_asm_output_data_uleb128 (ref->lineno, "At line number %lu",
				   (unsigned long) ref->lineno);
      if (node->form == DW_FORM_strp)
        dw2_asm_output_offset (DWARF_OFFSET_SIZE, node->label,
                               debug_str_section, "The macro: \"%s\"",
                               ref->info);
      else
        dw2_asm_output_data_uleb128 (node->index, "The macro: \"%s\"",
                                     ref->info);
      break;
    case DW_MACRO_import:
      dw2_asm_output_data (1, ref->code, "Import");
      ASM_GENERATE_INTERNAL_LABEL (label,
				   DEBUG_MACRO_SECTION_LABEL,
				   ref->lineno + macinfo_label_base);
      dw2_asm_output_offset (DWARF_OFFSET_SIZE, label, NULL, NULL);
      break;
    default:
      fprintf (asm_out_file, "%s unrecognized macinfo code %lu\n",
	       ASM_COMMENT_START, (unsigned long) ref->code);
      break;
    }
}

/* Attempt to make a sequence of define/undef macinfo ops shareable with
   other compilation unit .debug_macinfo sections.  IDX is the first
   index of a define/undef, return the number of ops that should be
   emitted in a comdat .debug_macinfo section and emit
   a DW_MACRO_import entry referencing it.
   If the define/undef entry should be emitted normally, return 0.  */

static unsigned
optimize_macinfo_range (unsigned int idx, vec<macinfo_entry, va_gc> *files,
			macinfo_hash_type **macinfo_htab)
{
  macinfo_entry *first, *second, *cur, *inc;
  char linebuf[sizeof (HOST_WIDE_INT) * 3 + 1];
  unsigned char checksum[16];
  struct md5_ctx ctx;
  char *grp_name, *tail;
  const char *base;
  unsigned int i, count, encoded_filename_len, linebuf_len;
  macinfo_entry **slot;

  first = &(*macinfo_table)[idx];
  second = &(*macinfo_table)[idx + 1];

  /* Optimize only if there are at least two consecutive define/undef ops,
     and either all of them are before first DW_MACINFO_start_file
     with lineno {0,1} (i.e. predefined macro block), or all of them are
     in some included header file.  */
  if (second->code != DW_MACINFO_define && second->code != DW_MACINFO_undef)
    return 0;
  if (vec_safe_is_empty (files))
    {
      if (first->lineno > 1 || second->lineno > 1)
	return 0;
    }
  else if (first->lineno == 0)
    return 0;

  /* Find the last define/undef entry that can be grouped together
     with first and at the same time compute md5 checksum of their
     codes, linenumbers and strings.  */
  md5_init_ctx (&ctx);
  for (i = idx; macinfo_table->iterate (i, &cur); i++)
    if (cur->code != DW_MACINFO_define && cur->code != DW_MACINFO_undef)
      break;
    else if (vec_safe_is_empty (files) && cur->lineno > 1)
      break;
    else
      {
	unsigned char code = cur->code;
	md5_process_bytes (&code, 1, &ctx);
	checksum_uleb128 (cur->lineno, &ctx);
	md5_process_bytes (cur->info, strlen (cur->info) + 1, &ctx);
      }
  md5_finish_ctx (&ctx, checksum);
  count = i - idx;

  /* From the containing include filename (if any) pick up just
     usable characters from its basename.  */
  if (vec_safe_is_empty (files))
    base = "";
  else
    base = lbasename (files->last ().info);
  for (encoded_filename_len = 0, i = 0; base[i]; i++)
    if (ISIDNUM (base[i]) || base[i] == '.')
      encoded_filename_len++;
  /* Count . at the end.  */
  if (encoded_filename_len)
    encoded_filename_len++;

  sprintf (linebuf, HOST_WIDE_INT_PRINT_UNSIGNED, first->lineno);
  linebuf_len = strlen (linebuf);

  /* The group name format is: wmN.[<encoded filename>.]<lineno>.<md5sum>  */
  grp_name = XALLOCAVEC (char, 4 + encoded_filename_len + linebuf_len + 1
			 + 16 * 2 + 1);
  memcpy (grp_name, DWARF_OFFSET_SIZE == 4 ? "wm4." : "wm8.", 4);
  tail = grp_name + 4;
  if (encoded_filename_len)
    {
      for (i = 0; base[i]; i++)
	if (ISIDNUM (base[i]) || base[i] == '.')
	  *tail++ = base[i];
      *tail++ = '.';
    }
  memcpy (tail, linebuf, linebuf_len);
  tail += linebuf_len;
  *tail++ = '.';
  for (i = 0; i < 16; i++)
    sprintf (tail + i * 2, "%02x", checksum[i] & 0xff);

  /* Construct a macinfo_entry for DW_MACRO_import
     in the empty vector entry before the first define/undef.  */
  inc = &(*macinfo_table)[idx - 1];
  inc->code = DW_MACRO_import;
  inc->lineno = 0;
  inc->info = ggc_strdup (grp_name);
  if (!*macinfo_htab)
    *macinfo_htab = new macinfo_hash_type (10);
  /* Avoid emitting duplicates.  */
  slot = (*macinfo_htab)->find_slot (inc, INSERT);
  if (*slot != NULL)
    {
      inc->code = 0;
      inc->info = NULL;
      /* If such an entry has been used before, just emit
	 a DW_MACRO_import op.  */
      inc = *slot;
      output_macinfo_op (inc);
      /* And clear all macinfo_entry in the range to avoid emitting them
	 in the second pass.  */
      for (i = idx; macinfo_table->iterate (i, &cur) && i < idx + count; i++)
	{
	  cur->code = 0;
	  cur->info = NULL;
	}
    }
  else
    {
      *slot = inc;
      inc->lineno = (*macinfo_htab)->elements ();
      output_macinfo_op (inc);
    }
  return count;
}

/* Save any strings needed by the macinfo table in the debug str
   table.  All strings must be collected into the table by the time
   index_string is called.  */

static void
save_macinfo_strings (void)
{
  unsigned len;
  unsigned i;
  macinfo_entry *ref;

  for (i = 0; macinfo_table && macinfo_table->iterate (i, &ref); i++)
    {
      switch (ref->code)
        {
          /* Match the logic in output_macinfo_op to decide on
             indirect strings.  */
          case DW_MACINFO_define:
          case DW_MACINFO_undef:
            len = strlen (ref->info) + 1;
            if (!dwarf_strict
                && len > DWARF_OFFSET_SIZE
                && !DWARF2_INDIRECT_STRING_SUPPORT_MISSING_ON_TARGET
                && (debug_str_section->common.flags & SECTION_MERGE) != 0)
              set_indirect_string (find_AT_string (ref->info));
            break;
	  case DW_MACRO_define_strp:
	  case DW_MACRO_undef_strp:
            set_indirect_string (find_AT_string (ref->info));
            break;
          default:
            break;
        }
    }
}

/* Output macinfo section(s).  */

static void
output_macinfo (const char *debug_line_label, bool early_lto_debug)
{
  unsigned i;
  unsigned long length = vec_safe_length (macinfo_table);
  macinfo_entry *ref;
  vec<macinfo_entry, va_gc> *files = NULL;
  macinfo_hash_type *macinfo_htab = NULL;
  char dl_section_ref[MAX_ARTIFICIAL_LABEL_BYTES];

  if (! length)
    return;

  /* output_macinfo* uses these interchangeably.  */
  gcc_assert ((int) DW_MACINFO_define == (int) DW_MACRO_define
	      && (int) DW_MACINFO_undef == (int) DW_MACRO_undef
	      && (int) DW_MACINFO_start_file == (int) DW_MACRO_start_file
	      && (int) DW_MACINFO_end_file == (int) DW_MACRO_end_file);

  /* AIX Assembler inserts the length, so adjust the reference to match the
     offset expected by debuggers.  */
  strcpy (dl_section_ref, debug_line_label);
  if (XCOFF_DEBUGGING_INFO)
    strcat (dl_section_ref, DWARF_INITIAL_LENGTH_SIZE_STR);

  /* For .debug_macro emit the section header.  */
  if (!dwarf_strict || dwarf_version >= 5)
    {
      dw2_asm_output_data (2, dwarf_version >= 5 ? 5 : 4,
			   "DWARF macro version number");
      if (DWARF_OFFSET_SIZE == 8)
	dw2_asm_output_data (1, 3, "Flags: 64-bit, lineptr present");
      else
	dw2_asm_output_data (1, 2, "Flags: 32-bit, lineptr present");
      dw2_asm_output_offset (DWARF_OFFSET_SIZE, debug_line_label,
                             debug_line_section, NULL);
    }

  /* In the first loop, it emits the primary .debug_macinfo section
     and after each emitted op the macinfo_entry is cleared.
     If a longer range of define/undef ops can be optimized using
     DW_MACRO_import, the DW_MACRO_import op is emitted and kept in
     the vector before the first define/undef in the range and the
     whole range of define/undef ops is not emitted and kept.  */
  for (i = 0; macinfo_table->iterate (i, &ref); i++)
    {
      switch (ref->code)
	{
	case DW_MACINFO_start_file:
	  vec_safe_push (files, *ref);
	  break;
	case DW_MACINFO_end_file:
	  if (!vec_safe_is_empty (files))
	    files->pop ();
	  break;
	case DW_MACINFO_define:
	case DW_MACINFO_undef:
	  if ((!dwarf_strict || dwarf_version >= 5)
	      && HAVE_COMDAT_GROUP
	      && vec_safe_length (files) != 1
	      && i > 0
	      && i + 1 < length
	      && (*macinfo_table)[i - 1].code == 0)
	    {
	      unsigned count = optimize_macinfo_range (i, files, &macinfo_htab);
	      if (count)
		{
		  i += count - 1;
		  continue;
		}
	    }
	  break;
	case 0:
	  /* A dummy entry may be inserted at the beginning to be able
	     to optimize the whole block of predefined macros.  */
	  if (i == 0)
	    continue;
	default:
	  break;
	}
      output_macinfo_op (ref);
      ref->info = NULL;
      ref->code = 0;
    }

  if (!macinfo_htab)
    return;

  /* Save the number of transparent includes so we can adjust the
     label number for the fat LTO object DWARF.  */
  unsigned macinfo_label_base_adj = macinfo_htab->elements ();

  delete macinfo_htab;
  macinfo_htab = NULL;

  /* If any DW_MACRO_import were used, on those DW_MACRO_import entries
     terminate the current chain and switch to a new comdat .debug_macinfo
     section and emit the define/undef entries within it.  */
  for (i = 0; macinfo_table->iterate (i, &ref); i++)
    switch (ref->code)
      {
      case 0:
	continue;
      case DW_MACRO_import:
	{
	  char label[MAX_ARTIFICIAL_LABEL_BYTES];
	  tree comdat_key = get_identifier (ref->info);
	  /* Terminate the previous .debug_macinfo section.  */
	  dw2_asm_output_data (1, 0, "End compilation unit");
	  targetm.asm_out.named_section (debug_macinfo_section_name,
					 SECTION_DEBUG
					 | SECTION_LINKONCE
					 | (early_lto_debug
					    ? SECTION_EXCLUDE : 0),
					 comdat_key);
	  ASM_GENERATE_INTERNAL_LABEL (label,
				       DEBUG_MACRO_SECTION_LABEL,
				       ref->lineno + macinfo_label_base);
	  ASM_OUTPUT_LABEL (asm_out_file, label);
	  ref->code = 0;
	  ref->info = NULL;
	  dw2_asm_output_data (2, dwarf_version >= 5 ? 5 : 4,
			       "DWARF macro version number");
	  if (DWARF_OFFSET_SIZE == 8)
	    dw2_asm_output_data (1, 1, "Flags: 64-bit");
	  else
	    dw2_asm_output_data (1, 0, "Flags: 32-bit");
	}
	break;
      case DW_MACINFO_define:
      case DW_MACINFO_undef:
	output_macinfo_op (ref);
	ref->code = 0;
	ref->info = NULL;
	break;
      default:
	gcc_unreachable ();
      }

  macinfo_label_base += macinfo_label_base_adj;
}

/* Initialize the various sections and labels for dwarf output and prefix
   them with PREFIX if non-NULL.  */

static void
init_sections_and_labels (bool early_lto_debug)
{
  /* As we may get called multiple times have a generation count for labels.  */
  static unsigned generation = 0;

  if (early_lto_debug)
    {
      if (!dwarf_split_debug_info)
	{
	  debug_info_section = get_section (DEBUG_LTO_INFO_SECTION,
					    SECTION_DEBUG | SECTION_EXCLUDE,
					    NULL);
	  debug_abbrev_section = get_section (DEBUG_LTO_ABBREV_SECTION,
					      SECTION_DEBUG | SECTION_EXCLUDE,
					      NULL);
	  debug_macinfo_section_name = ((dwarf_strict && dwarf_version < 5)
					? DEBUG_LTO_MACINFO_SECTION
					: DEBUG_LTO_MACRO_SECTION);
	  debug_macinfo_section = get_section (debug_macinfo_section_name,
					       SECTION_DEBUG
					       | SECTION_EXCLUDE, NULL);
	  /* For macro info we have to refer to a debug_line section, so similar
	     to split-dwarf emit a skeleton one for early debug.  */
	  debug_skeleton_line_section
	    = get_section (DEBUG_LTO_LINE_SECTION,
			   SECTION_DEBUG | SECTION_EXCLUDE, NULL);
	  ASM_GENERATE_INTERNAL_LABEL (debug_skeleton_line_section_label,
				       DEBUG_SKELETON_LINE_SECTION_LABEL,
				       generation);
	}
      else
	{
	  /* ???  Which of the following do we need early?  */
	  debug_info_section = get_section (DEBUG_LTO_DWO_INFO_SECTION,
					    SECTION_DEBUG | SECTION_EXCLUDE,
					    NULL);
	  debug_abbrev_section = get_section (DEBUG_LTO_DWO_ABBREV_SECTION,
					      SECTION_DEBUG | SECTION_EXCLUDE,
					      NULL);
	  debug_skeleton_info_section = get_section (DEBUG_LTO_INFO_SECTION,
						     SECTION_DEBUG
						     | SECTION_EXCLUDE, NULL);
	  debug_skeleton_abbrev_section = get_section (DEBUG_LTO_ABBREV_SECTION,
						       SECTION_DEBUG
						       | SECTION_EXCLUDE, NULL);
	  ASM_GENERATE_INTERNAL_LABEL (debug_skeleton_abbrev_section_label,
				       DEBUG_SKELETON_ABBREV_SECTION_LABEL,
				       generation);

	  /* Somewhat confusing detail: The skeleton_[abbrev|info] sections stay in
	     the main .o, but the skeleton_line goes into the split off dwo.  */
	  debug_skeleton_line_section
	    = get_section (DEBUG_LTO_LINE_SECTION,
			   SECTION_DEBUG | SECTION_EXCLUDE, NULL);
	  ASM_GENERATE_INTERNAL_LABEL (debug_skeleton_line_section_label,
				       DEBUG_SKELETON_LINE_SECTION_LABEL,
				       generation);
	  debug_str_offsets_section
	    = get_section (DEBUG_LTO_DWO_STR_OFFSETS_SECTION,
			   SECTION_DEBUG | SECTION_EXCLUDE,
			   NULL);
	  ASM_GENERATE_INTERNAL_LABEL (debug_skeleton_info_section_label,
				       DEBUG_SKELETON_INFO_SECTION_LABEL,
				       generation);
	  debug_str_dwo_section = get_section (DEBUG_LTO_STR_DWO_SECTION,
					       DEBUG_STR_DWO_SECTION_FLAGS, NULL);
	  debug_macinfo_section_name
	    = (dwarf_strict
	       ? DEBUG_LTO_DWO_MACINFO_SECTION : DEBUG_LTO_DWO_MACRO_SECTION);
	  debug_macinfo_section = get_section (debug_macinfo_section_name,
					       SECTION_DEBUG | SECTION_EXCLUDE,
					       NULL);
	}
      debug_str_section = get_section (DEBUG_LTO_STR_SECTION,
				       DEBUG_STR_SECTION_FLAGS
				       | SECTION_EXCLUDE, NULL);
    }
  else
    {
      if (!dwarf_split_debug_info)
	{
	  debug_info_section = get_section (DEBUG_INFO_SECTION,
					    SECTION_DEBUG, NULL);
	  debug_abbrev_section = get_section (DEBUG_ABBREV_SECTION,
					      SECTION_DEBUG, NULL);
	  debug_loc_section = get_section (DEBUG_LOC_SECTION,
					   SECTION_DEBUG, NULL);
	  debug_macinfo_section_name
	      = dwarf_strict ? DEBUG_MACINFO_SECTION : DEBUG_MACRO_SECTION;
	  debug_macinfo_section = get_section (debug_macinfo_section_name,
					       SECTION_DEBUG, NULL);
	}
      else
	{
	  debug_info_section = get_section (DEBUG_DWO_INFO_SECTION,
					    SECTION_DEBUG | SECTION_EXCLUDE,
					    NULL);
	  debug_abbrev_section = get_section (DEBUG_DWO_ABBREV_SECTION,
					      SECTION_DEBUG | SECTION_EXCLUDE,
					      NULL);
	  debug_addr_section = get_section (DEBUG_ADDR_SECTION,
					    SECTION_DEBUG, NULL);
	  debug_skeleton_info_section = get_section (DEBUG_INFO_SECTION,
						     SECTION_DEBUG, NULL);
	  debug_skeleton_abbrev_section = get_section (DEBUG_ABBREV_SECTION,
						       SECTION_DEBUG, NULL);
	  ASM_GENERATE_INTERNAL_LABEL (debug_skeleton_abbrev_section_label,
				       DEBUG_SKELETON_ABBREV_SECTION_LABEL,
				       generation);

	  /* Somewhat confusing detail: The skeleton_[abbrev|info] sections
	     stay in the main .o, but the skeleton_line goes into the
	     split off dwo.  */
	  debug_skeleton_line_section
	      = get_section (DEBUG_DWO_LINE_SECTION,
			     SECTION_DEBUG | SECTION_EXCLUDE, NULL);
	  ASM_GENERATE_INTERNAL_LABEL (debug_skeleton_line_section_label,
				       DEBUG_SKELETON_LINE_SECTION_LABEL,
				       generation);
	  debug_str_offsets_section
	    = get_section (DEBUG_DWO_STR_OFFSETS_SECTION,
			   SECTION_DEBUG | SECTION_EXCLUDE, NULL);
	  ASM_GENERATE_INTERNAL_LABEL (debug_skeleton_info_section_label,
				       DEBUG_SKELETON_INFO_SECTION_LABEL,
				       generation);
	  debug_loc_section = get_section (DEBUG_DWO_LOC_SECTION,
					   SECTION_DEBUG | SECTION_EXCLUDE,
					   NULL);
	  debug_str_dwo_section = get_section (DEBUG_STR_DWO_SECTION,
					       DEBUG_STR_DWO_SECTION_FLAGS,
					       NULL);
	  debug_macinfo_section_name
	    = (dwarf_strict && dwarf_version < 5)
	      ? DEBUG_DWO_MACINFO_SECTION : DEBUG_DWO_MACRO_SECTION;
	  debug_macinfo_section = get_section (debug_macinfo_section_name,
					       SECTION_DEBUG | SECTION_EXCLUDE,
					       NULL);
	}
      debug_aranges_section = get_section (DEBUG_ARANGES_SECTION,
					   SECTION_DEBUG, NULL);
      debug_line_section = get_section (DEBUG_LINE_SECTION,
					SECTION_DEBUG, NULL);
      debug_pubnames_section = get_section (DEBUG_PUBNAMES_SECTION,
					    SECTION_DEBUG, NULL);
      debug_pubtypes_section = get_section (DEBUG_PUBTYPES_SECTION,
					    SECTION_DEBUG, NULL);
      debug_str_section = get_section (DEBUG_STR_SECTION,
				       DEBUG_STR_SECTION_FLAGS, NULL);
      debug_ranges_section = get_section (dwarf_version >= 5
					  ? DEBUG_RNGLISTS_SECTION
					  : DEBUG_RANGES_SECTION,
					  SECTION_DEBUG, NULL);
      debug_frame_section = get_section (DEBUG_FRAME_SECTION,
					 SECTION_DEBUG, NULL);
    }

  ASM_GENERATE_INTERNAL_LABEL (abbrev_section_label,
			       DEBUG_ABBREV_SECTION_LABEL, generation);
  ASM_GENERATE_INTERNAL_LABEL (debug_info_section_label,
			       DEBUG_INFO_SECTION_LABEL, generation);
  info_section_emitted = false;
  ASM_GENERATE_INTERNAL_LABEL (debug_line_section_label,
			       DEBUG_LINE_SECTION_LABEL, generation);
  ASM_GENERATE_INTERNAL_LABEL (ranges_section_label,
			       DEBUG_RANGES_SECTION_LABEL, generation);
  if (dwarf_version >= 5 && dwarf_split_debug_info)
    ASM_GENERATE_INTERNAL_LABEL (ranges_base_label,
				 DEBUG_RANGES_SECTION_LABEL, 2 + generation);
  ASM_GENERATE_INTERNAL_LABEL (debug_addr_section_label,
                               DEBUG_ADDR_SECTION_LABEL, generation);
  ASM_GENERATE_INTERNAL_LABEL (macinfo_section_label,
			       (dwarf_strict && dwarf_version < 5)
			       ? DEBUG_MACINFO_SECTION_LABEL
			       : DEBUG_MACRO_SECTION_LABEL, generation);
  ASM_GENERATE_INTERNAL_LABEL (loc_section_label, DEBUG_LOC_SECTION_LABEL,
			       generation);

  ++generation;
}

/* Set up for Dwarf output at the start of compilation.  */

static void
dwarf2out_init (const char *filename ATTRIBUTE_UNUSED)
{
  /* Allocate the file_table.  */
  file_table = hash_table<dwarf_file_hasher>::create_ggc (50);

#ifndef DWARF2_LINENO_DEBUGGING_INFO
  /* Allocate the decl_die_table.  */
  decl_die_table = hash_table<decl_die_hasher>::create_ggc (10);

  /* Allocate the decl_loc_table.  */
  decl_loc_table = hash_table<decl_loc_hasher>::create_ggc (10);

  /* Allocate the cached_dw_loc_list_table.  */
  cached_dw_loc_list_table = hash_table<dw_loc_list_hasher>::create_ggc (10);

  /* Allocate the initial hunk of the decl_scope_table.  */
  vec_alloc (decl_scope_table, 256);

  /* Allocate the initial hunk of the abbrev_die_table.  */
  vec_alloc (abbrev_die_table, 256);
  /* Zero-th entry is allocated, but unused.  */
  abbrev_die_table->quick_push (NULL);

  /* Allocate the dwarf_proc_stack_usage_map.  */
  dwarf_proc_stack_usage_map = new hash_map<dw_die_ref, int>;

  /* Allocate the pubtypes and pubnames vectors.  */
  vec_alloc (pubname_table, 32);
  vec_alloc (pubtype_table, 32);

  vec_alloc (incomplete_types, 64);

  vec_alloc (used_rtx_array, 32);

  if (debug_info_level >= DINFO_LEVEL_VERBOSE)
    vec_alloc (macinfo_table, 64);
#endif

  /* If front-ends already registered a main translation unit but we were not
     ready to perform the association, do this now.  */
  if (main_translation_unit != NULL_TREE)
    equate_decl_number_to_die (main_translation_unit, comp_unit_die ());
}

/* Called before compile () starts outputtting functions, variables
   and toplevel asms into assembly.  */

static void
dwarf2out_assembly_start (void)
{
#ifndef DWARF2_LINENO_DEBUGGING_INFO
  ASM_GENERATE_INTERNAL_LABEL (text_section_label, TEXT_SECTION_LABEL, 0);
  ASM_GENERATE_INTERNAL_LABEL (text_end_label, TEXT_END_LABEL, 0);
  ASM_GENERATE_INTERNAL_LABEL (cold_text_section_label,
			       COLD_TEXT_SECTION_LABEL, 0);
  ASM_GENERATE_INTERNAL_LABEL (cold_end_label, COLD_END_LABEL, 0);

  switch_to_section (text_section);
  ASM_OUTPUT_LABEL (asm_out_file, text_section_label);
#endif

  /* Make sure the line number table for .text always exists.  */
  text_section_line_info = new_line_info_table ();
  text_section_line_info->end_label = text_end_label;

#ifdef DWARF2_LINENO_DEBUGGING_INFO
  cur_line_info_table = text_section_line_info;
#endif

  if (HAVE_GAS_CFI_SECTIONS_DIRECTIVE
      && dwarf2out_do_cfi_asm ()
      && (!(flag_unwind_tables || flag_exceptions)
	  || targetm_common.except_unwind_info (&global_options) != UI_DWARF2))
    fprintf (asm_out_file, "\t.cfi_sections\t.debug_frame\n");
}

/* A helper function for dwarf2out_finish called through
   htab_traverse.  Assign a string its index.  All strings must be
   collected into the table by the time index_string is called,
   because the indexing code relies on htab_traverse to traverse nodes
   in the same order for each run. */

int
index_string (indirect_string_node **h, unsigned int *index)
{
  indirect_string_node *node = *h;

  find_string_form (node);
  if (node->form == DW_FORM_GNU_str_index && node->refcount > 0)
    {
      gcc_assert (node->index == NO_INDEX_ASSIGNED);
      node->index = *index;
      *index += 1;
    }
  return 1;
}

/* A helper function for output_indirect_strings called through
   htab_traverse.  Output the offset to a string and update the
   current offset.  */

int
output_index_string_offset (indirect_string_node **h, unsigned int *offset)
{
  indirect_string_node *node = *h;

  if (node->form == DW_FORM_GNU_str_index && node->refcount > 0)
    {
      /* Assert that this node has been assigned an index.  */
      gcc_assert (node->index != NO_INDEX_ASSIGNED
                  && node->index != NOT_INDEXED);
      dw2_asm_output_data (DWARF_OFFSET_SIZE, *offset,
                           "indexed string 0x%x: %s", node->index, node->str);
      *offset += strlen (node->str) + 1;
    }
  return 1;
}

/* A helper function for dwarf2out_finish called through
   htab_traverse.  Output the indexed string.  */

int
output_index_string (indirect_string_node **h, unsigned int *cur_idx)
{
  struct indirect_string_node *node = *h;

  if (node->form == DW_FORM_GNU_str_index && node->refcount > 0)
    {
      /* Assert that the strings are output in the same order as their
         indexes were assigned.  */
      gcc_assert (*cur_idx == node->index);
      assemble_string (node->str, strlen (node->str) + 1);
      *cur_idx += 1;
    }
  return 1;
}

/* A helper function for dwarf2out_finish called through
   htab_traverse.  Emit one queued .debug_str string.  */

int
output_indirect_string (indirect_string_node **h, enum dwarf_form form)
{
  struct indirect_string_node *node = *h;

  node->form = find_string_form (node);
  if (node->form == form && node->refcount > 0)
    {
      ASM_OUTPUT_LABEL (asm_out_file, node->label);
      assemble_string (node->str, strlen (node->str) + 1);
    }

  return 1;
}

/* Output the indexed string table.  */

static void
output_indirect_strings (void)
{
  switch_to_section (debug_str_section);
  if (!dwarf_split_debug_info)
    debug_str_hash->traverse<enum dwarf_form,
			     output_indirect_string> (DW_FORM_strp);
  else
    {
      unsigned int offset = 0;
      unsigned int cur_idx = 0;

      skeleton_debug_str_hash->traverse<enum dwarf_form,
					output_indirect_string> (DW_FORM_strp);

      switch_to_section (debug_str_offsets_section);
      debug_str_hash->traverse_noresize
	<unsigned int *, output_index_string_offset> (&offset);
      switch_to_section (debug_str_dwo_section);
      debug_str_hash->traverse_noresize<unsigned int *, output_index_string>
	(&cur_idx);
    }
}

/* Callback for htab_traverse to assign an index to an entry in the
   table, and to write that entry to the .debug_addr section.  */

int
output_addr_table_entry (addr_table_entry **slot, unsigned int *cur_index)
{
  addr_table_entry *entry = *slot;

  if (entry->refcount == 0)
    {
      gcc_assert (entry->index == NO_INDEX_ASSIGNED
                  || entry->index == NOT_INDEXED);
      return 1;
    }

  gcc_assert (entry->index == *cur_index);
  (*cur_index)++;

  switch (entry->kind)
    {
      case ate_kind_rtx:
        dw2_asm_output_addr_rtx (DWARF2_ADDR_SIZE, entry->addr.rtl,
                                 "0x%x", entry->index);
        break;
      case ate_kind_rtx_dtprel:
        gcc_assert (targetm.asm_out.output_dwarf_dtprel);
        targetm.asm_out.output_dwarf_dtprel (asm_out_file,
                                             DWARF2_ADDR_SIZE,
                                             entry->addr.rtl);
        fputc ('\n', asm_out_file);
        break;
      case ate_kind_label:
        dw2_asm_output_addr (DWARF2_ADDR_SIZE, entry->addr.label,
                                 "0x%x", entry->index);
        break;
      default:
        gcc_unreachable ();
    }
  return 1;
}

/* Produce the .debug_addr section.  */

static void
output_addr_table (void)
{
  unsigned int index = 0;
  if (addr_index_table == NULL || addr_index_table->size () == 0)
    return;

  switch_to_section (debug_addr_section);
  addr_index_table
    ->traverse_noresize<unsigned int *, output_addr_table_entry> (&index);
}

#if ENABLE_ASSERT_CHECKING
/* Verify that all marks are clear.  */

static void
verify_marks_clear (dw_die_ref die)
{
  dw_die_ref c;

  gcc_assert (! die->die_mark);
  FOR_EACH_CHILD (die, c, verify_marks_clear (c));
}
#endif /* ENABLE_ASSERT_CHECKING */

/* Clear the marks for a die and its children.
   Be cool if the mark isn't set.  */

static void
prune_unmark_dies (dw_die_ref die)
{
  dw_die_ref c;

  if (die->die_mark)
    die->die_mark = 0;
  FOR_EACH_CHILD (die, c, prune_unmark_dies (c));
}

/* Given LOC that is referenced by a DIE we're marking as used, find all
   referenced DWARF procedures it references and mark them as used.  */

static void
prune_unused_types_walk_loc_descr (dw_loc_descr_ref loc)
{
  for (; loc != NULL; loc = loc->dw_loc_next)
    switch (loc->dw_loc_opc)
      {
      case DW_OP_implicit_pointer:
      case DW_OP_convert:
      case DW_OP_reinterpret:
      case DW_OP_GNU_implicit_pointer:
      case DW_OP_GNU_convert:
      case DW_OP_GNU_reinterpret:
	if (loc->dw_loc_oprnd1.val_class == dw_val_class_die_ref)
	  prune_unused_types_mark (loc->dw_loc_oprnd1.v.val_die_ref.die, 1);
	break;
      case DW_OP_GNU_variable_value:
	if (loc->dw_loc_oprnd1.val_class == dw_val_class_decl_ref)
	  {
	    dw_die_ref ref
	      = lookup_decl_die (loc->dw_loc_oprnd1.v.val_decl_ref);
	    if (ref == NULL)
	      break;
	    loc->dw_loc_oprnd1.val_class = dw_val_class_die_ref;
	    loc->dw_loc_oprnd1.v.val_die_ref.die = ref;
	    loc->dw_loc_oprnd1.v.val_die_ref.external = 0;
	  }
	/* FALLTHRU */
      case DW_OP_call2:
      case DW_OP_call4:
      case DW_OP_call_ref:
      case DW_OP_const_type:
      case DW_OP_GNU_const_type:
      case DW_OP_GNU_parameter_ref:
	gcc_assert (loc->dw_loc_oprnd1.val_class == dw_val_class_die_ref);
	prune_unused_types_mark (loc->dw_loc_oprnd1.v.val_die_ref.die, 1);
	break;
      case DW_OP_regval_type:
      case DW_OP_deref_type:
      case DW_OP_GNU_regval_type:
      case DW_OP_GNU_deref_type:
	gcc_assert (loc->dw_loc_oprnd2.val_class == dw_val_class_die_ref);
	prune_unused_types_mark (loc->dw_loc_oprnd2.v.val_die_ref.die, 1);
	break;
      case DW_OP_entry_value:
      case DW_OP_GNU_entry_value:
	gcc_assert (loc->dw_loc_oprnd1.val_class == dw_val_class_loc);
	prune_unused_types_walk_loc_descr (loc->dw_loc_oprnd1.v.val_loc);
	break;
      default:
	break;
      }
}

/* Given DIE that we're marking as used, find any other dies
   it references as attributes and mark them as used.  */

static void
prune_unused_types_walk_attribs (dw_die_ref die)
{
  dw_attr_node *a;
  unsigned ix;

  FOR_EACH_VEC_SAFE_ELT (die->die_attr, ix, a)
    {
      switch (AT_class (a))
	{
	/* Make sure DWARF procedures referenced by location descriptions will
	   get emitted.  */
	case dw_val_class_loc:
	  prune_unused_types_walk_loc_descr (AT_loc (a));
	  break;
	case dw_val_class_loc_list:
	  for (dw_loc_list_ref list = AT_loc_list (a);
	       list != NULL;
	       list = list->dw_loc_next)
	    prune_unused_types_walk_loc_descr (list->expr);
	  break;

	case dw_val_class_die_ref:
	  /* A reference to another DIE.
	     Make sure that it will get emitted.
	     If it was broken out into a comdat group, don't follow it.  */
          if (! AT_ref (a)->comdat_type_p
              || a->dw_attr == DW_AT_specification)
	    prune_unused_types_mark (a->dw_attr_val.v.val_die_ref.die, 1);
	  break;

	case dw_val_class_str:
	  /* Set the string's refcount to 0 so that prune_unused_types_mark
	     accounts properly for it.  */
	  a->dw_attr_val.v.val_str->refcount = 0;
	  break;

	default:
	  break;
	}
    }
}

/* Mark the generic parameters and arguments children DIEs of DIE.  */

static void
prune_unused_types_mark_generic_parms_dies (dw_die_ref die)
{
  dw_die_ref c;

  if (die == NULL || die->die_child == NULL)
    return;
  c = die->die_child;
  do
    {
      if (is_template_parameter (c))
	prune_unused_types_mark (c, 1);
      c = c->die_sib;
    } while (c && c != die->die_child);
}

/* Mark DIE as being used.  If DOKIDS is true, then walk down
   to DIE's children.  */

static void
prune_unused_types_mark (dw_die_ref die, int dokids)
{
  dw_die_ref c;

  if (die->die_mark == 0)
    {
      /* We haven't done this node yet.  Mark it as used.  */
      die->die_mark = 1;
      /* If this is the DIE of a generic type instantiation,
	 mark the children DIEs that describe its generic parms and
	 args.  */
      prune_unused_types_mark_generic_parms_dies (die);

      /* We also have to mark its parents as used.
	 (But we don't want to mark our parent's kids due to this,
	 unless it is a class.)  */
      if (die->die_parent)
	prune_unused_types_mark (die->die_parent,
				 class_scope_p (die->die_parent));

      /* Mark any referenced nodes.  */
      prune_unused_types_walk_attribs (die);

      /* If this node is a specification,
	 also mark the definition, if it exists.  */
      if (get_AT_flag (die, DW_AT_declaration) && die->die_definition)
	prune_unused_types_mark (die->die_definition, 1);
    }

  if (dokids && die->die_mark != 2)
    {
      /* We need to walk the children, but haven't done so yet.
	 Remember that we've walked the kids.  */
      die->die_mark = 2;

      /* If this is an array type, we need to make sure our
	 kids get marked, even if they're types.  If we're
	 breaking out types into comdat sections, do this
	 for all type definitions.  */
      if (die->die_tag == DW_TAG_array_type
          || (use_debug_types
              && is_type_die (die) && ! is_declaration_die (die)))
	FOR_EACH_CHILD (die, c, prune_unused_types_mark (c, 1));
      else
	FOR_EACH_CHILD (die, c, prune_unused_types_walk (c));
    }
}

/* For local classes, look if any static member functions were emitted
   and if so, mark them.  */

static void
prune_unused_types_walk_local_classes (dw_die_ref die)
{
  dw_die_ref c;

  if (die->die_mark == 2)
    return;

  switch (die->die_tag)
    {
    case DW_TAG_structure_type:
    case DW_TAG_union_type:
    case DW_TAG_class_type:
      break;

    case DW_TAG_subprogram:
      if (!get_AT_flag (die, DW_AT_declaration)
	  || die->die_definition != NULL)
	prune_unused_types_mark (die, 1);
      return;

    default:
      return;
    }

  /* Mark children.  */
  FOR_EACH_CHILD (die, c, prune_unused_types_walk_local_classes (c));
}

/* Walk the tree DIE and mark types that we actually use.  */

static void
prune_unused_types_walk (dw_die_ref die)
{
  dw_die_ref c;

  /* Don't do anything if this node is already marked and
     children have been marked as well.  */
  if (die->die_mark == 2)
    return;

  switch (die->die_tag)
    {
    case DW_TAG_structure_type:
    case DW_TAG_union_type:
    case DW_TAG_class_type:
      if (die->die_perennial_p)
	break;

      for (c = die->die_parent; c; c = c->die_parent)
	if (c->die_tag == DW_TAG_subprogram)
	  break;

      /* Finding used static member functions inside of classes
	 is needed just for local classes, because for other classes
	 static member function DIEs with DW_AT_specification
	 are emitted outside of the DW_TAG_*_type.  If we ever change
	 it, we'd need to call this even for non-local classes.  */
      if (c)
	prune_unused_types_walk_local_classes (die);

      /* It's a type node --- don't mark it.  */
      return;

    case DW_TAG_const_type:
    case DW_TAG_packed_type:
    case DW_TAG_pointer_type:
    case DW_TAG_reference_type:
    case DW_TAG_rvalue_reference_type:
    case DW_TAG_volatile_type:
    case DW_TAG_typedef:
    case DW_TAG_array_type:
    case DW_TAG_interface_type:
    case DW_TAG_friend:
    case DW_TAG_enumeration_type:
    case DW_TAG_subroutine_type:
    case DW_TAG_string_type:
    case DW_TAG_set_type:
    case DW_TAG_subrange_type:
    case DW_TAG_ptr_to_member_type:
    case DW_TAG_file_type:
      /* Type nodes are useful only when other DIEs reference them --- don't
	 mark them.  */
      /* FALLTHROUGH */

    case DW_TAG_dwarf_procedure:
      /* Likewise for DWARF procedures.  */

      if (die->die_perennial_p)
	break;

      return;

    default:
      /* Mark everything else.  */
      break;
  }

  if (die->die_mark == 0)
    {
      die->die_mark = 1;

      /* Now, mark any dies referenced from here.  */
      prune_unused_types_walk_attribs (die);
    }

  die->die_mark = 2;

  /* Mark children.  */
  FOR_EACH_CHILD (die, c, prune_unused_types_walk (c));
}

/* Increment the string counts on strings referred to from DIE's
   attributes.  */

static void
prune_unused_types_update_strings (dw_die_ref die)
{
  dw_attr_node *a;
  unsigned ix;

  FOR_EACH_VEC_SAFE_ELT (die->die_attr, ix, a)
    if (AT_class (a) == dw_val_class_str)
      {
	struct indirect_string_node *s = a->dw_attr_val.v.val_str;
	s->refcount++;
	/* Avoid unnecessarily putting strings that are used less than
	   twice in the hash table.  */
	if (s->refcount
	    == ((DEBUG_STR_SECTION_FLAGS & SECTION_MERGE) ? 1 : 2))
	  {
	    indirect_string_node **slot
	      = debug_str_hash->find_slot_with_hash (s->str,
						     htab_hash_string (s->str),
						     INSERT);
	    gcc_assert (*slot == NULL);
	    *slot = s;
	  }
      }
}

/* Mark DIE and its children as removed.  */

static void
mark_removed (dw_die_ref die)
{
  dw_die_ref c;
  die->removed = true;
  FOR_EACH_CHILD (die, c, mark_removed (c));
}

/* Remove from the tree DIE any dies that aren't marked.  */

static void
prune_unused_types_prune (dw_die_ref die)
{
  dw_die_ref c;

  gcc_assert (die->die_mark);
  prune_unused_types_update_strings (die);

  if (! die->die_child)
    return;

  c = die->die_child;
  do {
    dw_die_ref prev = c, next;
    for (c = c->die_sib; ! c->die_mark; c = next)
      if (c == die->die_child)
	{
	  /* No marked children between 'prev' and the end of the list.  */
	  if (prev == c)
	    /* No marked children at all.  */
	    die->die_child = NULL;
	  else
	    {
	      prev->die_sib = c->die_sib;
	      die->die_child = prev;
	    }
	  c->die_sib = NULL;
	  mark_removed (c);
	  return;
	}
      else
	{
	  next = c->die_sib;
	  c->die_sib = NULL;
	  mark_removed (c);
	}

    if (c != prev->die_sib)
      prev->die_sib = c;
    prune_unused_types_prune (c);
  } while (c != die->die_child);
}

/* Remove dies representing declarations that we never use.  */

static void
prune_unused_types (void)
{
  unsigned int i;
  limbo_die_node *node;
  comdat_type_node *ctnode;
  pubname_entry *pub;
  dw_die_ref base_type;

#if ENABLE_ASSERT_CHECKING
  /* All the marks should already be clear.  */
  verify_marks_clear (comp_unit_die ());
  for (node = limbo_die_list; node; node = node->next)
    verify_marks_clear (node->die);
  for (ctnode = comdat_type_list; ctnode; ctnode = ctnode->next)
    verify_marks_clear (ctnode->root_die);
#endif /* ENABLE_ASSERT_CHECKING */

  /* Mark types that are used in global variables.  */
  premark_types_used_by_global_vars ();

  /* Set the mark on nodes that are actually used.  */
  prune_unused_types_walk (comp_unit_die ());
  for (node = limbo_die_list; node; node = node->next)
    prune_unused_types_walk (node->die);
  for (ctnode = comdat_type_list; ctnode; ctnode = ctnode->next)
    {
      prune_unused_types_walk (ctnode->root_die);
      prune_unused_types_mark (ctnode->type_die, 1);
    }

  /* Also set the mark on nodes referenced from the pubname_table.  Enumerators
     are unusual in that they are pubnames that are the children of pubtypes.
     They should only be marked via their parent DW_TAG_enumeration_type die,
     not as roots in themselves.  */
  FOR_EACH_VEC_ELT (*pubname_table, i, pub)
    if (pub->die->die_tag != DW_TAG_enumerator)
      prune_unused_types_mark (pub->die, 1);
  for (i = 0; base_types.iterate (i, &base_type); i++)
    prune_unused_types_mark (base_type, 1);

  /* For -fvar-tracking-assignments, also set the mark on nodes that could be
     referenced by DW_TAG_call_site DW_AT_call_origin (i.e. direct call
     callees).  */
  cgraph_node *cnode;
  FOR_EACH_FUNCTION (cnode)
    if (cnode->referred_to_p (false))
      {
	dw_die_ref die = lookup_decl_die (cnode->decl);
	if (die == NULL || die->die_mark)
	  continue;
	for (cgraph_edge *e = cnode->callers; e; e = e->next_caller)
	  if (e->caller != cnode
	      && opt_for_fn (e->caller->decl, flag_var_tracking_assignments))
	    {
	      prune_unused_types_mark (die, 1);
	      break;
	    }
      }

  if (debug_str_hash)
    debug_str_hash->empty ();
  if (skeleton_debug_str_hash)
    skeleton_debug_str_hash->empty ();
  prune_unused_types_prune (comp_unit_die ());
  for (limbo_die_node **pnode = &limbo_die_list; *pnode; )
    {
      node = *pnode;
      if (!node->die->die_mark)
	*pnode = node->next;
      else
	{
	  prune_unused_types_prune (node->die);
	  pnode = &node->next;
	}
    }
  for (ctnode = comdat_type_list; ctnode; ctnode = ctnode->next)
    prune_unused_types_prune (ctnode->root_die);

  /* Leave the marks clear.  */
  prune_unmark_dies (comp_unit_die ());
  for (node = limbo_die_list; node; node = node->next)
    prune_unmark_dies (node->die);
  for (ctnode = comdat_type_list; ctnode; ctnode = ctnode->next)
    prune_unmark_dies (ctnode->root_die);
}

/* Helpers to manipulate hash table of comdat type units.  */

struct comdat_type_hasher : nofree_ptr_hash <comdat_type_node>
{
  static inline hashval_t hash (const comdat_type_node *);
  static inline bool equal (const comdat_type_node *, const comdat_type_node *);
};

inline hashval_t
comdat_type_hasher::hash (const comdat_type_node *type_node)
{
  hashval_t h;
  memcpy (&h, type_node->signature, sizeof (h));
  return h;
}

inline bool
comdat_type_hasher::equal (const comdat_type_node *type_node_1,
			   const comdat_type_node *type_node_2)
{
  return (! memcmp (type_node_1->signature, type_node_2->signature,
                    DWARF_TYPE_SIGNATURE_SIZE));
}

/* Move a DW_AT_{,MIPS_}linkage_name attribute just added to dw_die_ref
   to the location it would have been added, should we know its
   DECL_ASSEMBLER_NAME when we added other attributes.  This will
   probably improve compactness of debug info, removing equivalent
   abbrevs, and hide any differences caused by deferring the
   computation of the assembler name, triggered by e.g. PCH.  */

static inline void
move_linkage_attr (dw_die_ref die)
{
  unsigned ix = vec_safe_length (die->die_attr);
  dw_attr_node linkage = (*die->die_attr)[ix - 1];

  gcc_assert (linkage.dw_attr == DW_AT_linkage_name
	      || linkage.dw_attr == DW_AT_MIPS_linkage_name);

  while (--ix > 0)
    {
      dw_attr_node *prev = &(*die->die_attr)[ix - 1];

      if (prev->dw_attr == DW_AT_decl_line
	  || prev->dw_attr == DW_AT_decl_column
	  || prev->dw_attr == DW_AT_name)
	break;
    }

  if (ix != vec_safe_length (die->die_attr) - 1)
    {
      die->die_attr->pop ();
      die->die_attr->quick_insert (ix, linkage);
    }
}

/* Helper function for resolve_addr, mark DW_TAG_base_type nodes
   referenced from typed stack ops and count how often they are used.  */

static void
mark_base_types (dw_loc_descr_ref loc)
{
  dw_die_ref base_type = NULL;

  for (; loc; loc = loc->dw_loc_next)
    {
      switch (loc->dw_loc_opc)
	{
	case DW_OP_regval_type:
	case DW_OP_deref_type:
	case DW_OP_GNU_regval_type:
	case DW_OP_GNU_deref_type:
	  base_type = loc->dw_loc_oprnd2.v.val_die_ref.die;
	  break;
	case DW_OP_convert:
	case DW_OP_reinterpret:
	case DW_OP_GNU_convert:
	case DW_OP_GNU_reinterpret:
	  if (loc->dw_loc_oprnd1.val_class == dw_val_class_unsigned_const)
	    continue;
	  /* FALLTHRU */
	case DW_OP_const_type:
	case DW_OP_GNU_const_type:
	  base_type = loc->dw_loc_oprnd1.v.val_die_ref.die;
	  break;
	case DW_OP_entry_value:
	case DW_OP_GNU_entry_value:
	  mark_base_types (loc->dw_loc_oprnd1.v.val_loc);
	  continue;
	default:
	  continue;
	}
      gcc_assert (base_type->die_parent == comp_unit_die ());
      if (base_type->die_mark)
	base_type->die_mark++;
      else
	{
	  base_types.safe_push (base_type);
	  base_type->die_mark = 1;
	}
    }
}

/* Comparison function for sorting marked base types.  */

static int
base_type_cmp (const void *x, const void *y)
{
  dw_die_ref dx = *(const dw_die_ref *) x;
  dw_die_ref dy = *(const dw_die_ref *) y;
  unsigned int byte_size1, byte_size2;
  unsigned int encoding1, encoding2;
  unsigned int align1, align2;
  if (dx->die_mark > dy->die_mark)
    return -1;
  if (dx->die_mark < dy->die_mark)
    return 1;
  byte_size1 = get_AT_unsigned (dx, DW_AT_byte_size);
  byte_size2 = get_AT_unsigned (dy, DW_AT_byte_size);
  if (byte_size1 < byte_size2)
    return 1;
  if (byte_size1 > byte_size2)
    return -1;
  encoding1 = get_AT_unsigned (dx, DW_AT_encoding);
  encoding2 = get_AT_unsigned (dy, DW_AT_encoding);
  if (encoding1 < encoding2)
    return 1;
  if (encoding1 > encoding2)
    return -1;
  align1 = get_AT_unsigned (dx, DW_AT_alignment);
  align2 = get_AT_unsigned (dy, DW_AT_alignment);
  if (align1 < align2)
    return 1;
  if (align1 > align2)
    return -1;
  return 0;
}

/* Move base types marked by mark_base_types as early as possible
   in the CU, sorted by decreasing usage count both to make the
   uleb128 references as small as possible and to make sure they
   will have die_offset already computed by calc_die_sizes when
   sizes of typed stack loc ops is computed.  */

static void
move_marked_base_types (void)
{
  unsigned int i;
  dw_die_ref base_type, die, c;

  if (base_types.is_empty ())
    return;

  /* Sort by decreasing usage count, they will be added again in that
     order later on.  */
  base_types.qsort (base_type_cmp);
  die = comp_unit_die ();
  c = die->die_child;
  do
    {
      dw_die_ref prev = c;
      c = c->die_sib;
      while (c->die_mark)
	{
	  remove_child_with_prev (c, prev);
	  /* As base types got marked, there must be at least
	     one node other than DW_TAG_base_type.  */
	  gcc_assert (die->die_child != NULL);
	  c = prev->die_sib;
	}
    }
  while (c != die->die_child);
  gcc_assert (die->die_child);
  c = die->die_child;
  for (i = 0; base_types.iterate (i, &base_type); i++)
    {
      base_type->die_mark = 0;
      base_type->die_sib = c->die_sib;
      c->die_sib = base_type;
      c = base_type;
    }
}

/* Helper function for resolve_addr, attempt to resolve
   one CONST_STRING, return true if successful.  Similarly verify that
   SYMBOL_REFs refer to variables emitted in the current CU.  */

static bool
resolve_one_addr (rtx *addr)
{
  rtx rtl = *addr;

  if (GET_CODE (rtl) == CONST_STRING)
    {
      size_t len = strlen (XSTR (rtl, 0)) + 1;
      tree t = build_string (len, XSTR (rtl, 0));
      tree tlen = size_int (len - 1);
      TREE_TYPE (t)
	= build_array_type (char_type_node, build_index_type (tlen));
      rtl = lookup_constant_def (t);
      if (!rtl || !MEM_P (rtl))
	return false;
      rtl = XEXP (rtl, 0);
      if (GET_CODE (rtl) == SYMBOL_REF
	  && SYMBOL_REF_DECL (rtl)
	  && !TREE_ASM_WRITTEN (SYMBOL_REF_DECL (rtl)))
	return false;
      vec_safe_push (used_rtx_array, rtl);
      *addr = rtl;
      return true;
    }

  if (GET_CODE (rtl) == SYMBOL_REF
      && SYMBOL_REF_DECL (rtl))
    {
      if (TREE_CONSTANT_POOL_ADDRESS_P (rtl))
	{
	  if (!TREE_ASM_WRITTEN (DECL_INITIAL (SYMBOL_REF_DECL (rtl))))
	    return false;
	}
      else if (!TREE_ASM_WRITTEN (SYMBOL_REF_DECL (rtl)))
	return false;
    }

  if (GET_CODE (rtl) == CONST)
    {
      subrtx_ptr_iterator::array_type array;
      FOR_EACH_SUBRTX_PTR (iter, array, &XEXP (rtl, 0), ALL)
	if (!resolve_one_addr (*iter))
	  return false;
    }

  return true;
}

/* For STRING_CST, return SYMBOL_REF of its constant pool entry,
   if possible, and create DW_TAG_dwarf_procedure that can be referenced
   from DW_OP_implicit_pointer if the string hasn't been seen yet.  */

static rtx
string_cst_pool_decl (tree t)
{
  rtx rtl = output_constant_def (t, 1);
  unsigned char *array;
  dw_loc_descr_ref l;
  tree decl;
  size_t len;
  dw_die_ref ref;

  if (!rtl || !MEM_P (rtl))
    return NULL_RTX;
  rtl = XEXP (rtl, 0);
  if (GET_CODE (rtl) != SYMBOL_REF
      || SYMBOL_REF_DECL (rtl) == NULL_TREE)
    return NULL_RTX;

  decl = SYMBOL_REF_DECL (rtl);
  if (!lookup_decl_die (decl))
    {
      len = TREE_STRING_LENGTH (t);
      vec_safe_push (used_rtx_array, rtl);
      ref = new_die (DW_TAG_dwarf_procedure, comp_unit_die (), decl);
      array = ggc_vec_alloc<unsigned char> (len);
      memcpy (array, TREE_STRING_POINTER (t), len);
      l = new_loc_descr (DW_OP_implicit_value, len, 0);
      l->dw_loc_oprnd2.val_class = dw_val_class_vec;
      l->dw_loc_oprnd2.v.val_vec.length = len;
      l->dw_loc_oprnd2.v.val_vec.elt_size = 1;
      l->dw_loc_oprnd2.v.val_vec.array = array;
      add_AT_loc (ref, DW_AT_location, l);
      equate_decl_number_to_die (decl, ref);
    }
  return rtl;
}

/* Helper function of resolve_addr_in_expr.  LOC is
   a DW_OP_addr followed by DW_OP_stack_value, either at the start
   of exprloc or after DW_OP_{,bit_}piece, and val_addr can't be
   resolved.  Replace it (both DW_OP_addr and DW_OP_stack_value)
   with DW_OP_implicit_pointer if possible
   and return true, if unsuccessful, return false.  */

static bool
optimize_one_addr_into_implicit_ptr (dw_loc_descr_ref loc)
{
  rtx rtl = loc->dw_loc_oprnd1.v.val_addr;
  HOST_WIDE_INT offset = 0;
  dw_die_ref ref = NULL;
  tree decl;

  if (GET_CODE (rtl) == CONST
      && GET_CODE (XEXP (rtl, 0)) == PLUS
      && CONST_INT_P (XEXP (XEXP (rtl, 0), 1)))
    {
      offset = INTVAL (XEXP (XEXP (rtl, 0), 1));
      rtl = XEXP (XEXP (rtl, 0), 0);
    }
  if (GET_CODE (rtl) == CONST_STRING)
    {
      size_t len = strlen (XSTR (rtl, 0)) + 1;
      tree t = build_string (len, XSTR (rtl, 0));
      tree tlen = size_int (len - 1);

      TREE_TYPE (t)
	= build_array_type (char_type_node, build_index_type (tlen));
      rtl = string_cst_pool_decl (t);
      if (!rtl)
	return false;
    }
  if (GET_CODE (rtl) == SYMBOL_REF && SYMBOL_REF_DECL (rtl))
    {
      decl = SYMBOL_REF_DECL (rtl);
      if (VAR_P (decl) && !DECL_EXTERNAL (decl))
	{
	  ref = lookup_decl_die (decl);
	  if (ref && (get_AT (ref, DW_AT_location)
		      || get_AT (ref, DW_AT_const_value)))
	    {
	      loc->dw_loc_opc = dwarf_OP (DW_OP_implicit_pointer);
	      loc->dw_loc_oprnd1.val_class = dw_val_class_die_ref;
	      loc->dw_loc_oprnd1.val_entry = NULL;
	      loc->dw_loc_oprnd1.v.val_die_ref.die = ref;
	      loc->dw_loc_oprnd1.v.val_die_ref.external = 0;
	      loc->dw_loc_next = loc->dw_loc_next->dw_loc_next;
	      loc->dw_loc_oprnd2.v.val_int = offset;
	      return true;
	    }
	}
    }
  return false;
}

/* Helper function for resolve_addr, handle one location
   expression, return false if at least one CONST_STRING or SYMBOL_REF in
   the location list couldn't be resolved.  */

static bool
resolve_addr_in_expr (dw_attr_node *a, dw_loc_descr_ref loc)
{
  dw_loc_descr_ref keep = NULL;
  for (dw_loc_descr_ref prev = NULL; loc; prev = loc, loc = loc->dw_loc_next)
    switch (loc->dw_loc_opc)
      {
      case DW_OP_addr:
	if (!resolve_one_addr (&loc->dw_loc_oprnd1.v.val_addr))
	  {
	    if ((prev == NULL
		 || prev->dw_loc_opc == DW_OP_piece
		 || prev->dw_loc_opc == DW_OP_bit_piece)
		&& loc->dw_loc_next
		&& loc->dw_loc_next->dw_loc_opc == DW_OP_stack_value
		&& (!dwarf_strict || dwarf_version >= 5)
		&& optimize_one_addr_into_implicit_ptr (loc))
	      break;
	    return false;
	  }
	break;
      case DW_OP_GNU_addr_index:
      case DW_OP_GNU_const_index:
	if (loc->dw_loc_opc == DW_OP_GNU_addr_index
            || (loc->dw_loc_opc == DW_OP_GNU_const_index && loc->dtprel))
          {
            rtx rtl = loc->dw_loc_oprnd1.val_entry->addr.rtl;
            if (!resolve_one_addr (&rtl))
              return false;
            remove_addr_table_entry (loc->dw_loc_oprnd1.val_entry);
	    loc->dw_loc_oprnd1.val_entry
	      = add_addr_table_entry (rtl, ate_kind_rtx);
          }
	break;
      case DW_OP_const4u:
      case DW_OP_const8u:
	if (loc->dtprel
	    && !resolve_one_addr (&loc->dw_loc_oprnd1.v.val_addr))
	  return false;
	break;
      case DW_OP_plus_uconst:
	if (size_of_loc_descr (loc)
	    > size_of_int_loc_descriptor (loc->dw_loc_oprnd1.v.val_unsigned)
	      + 1
	    && loc->dw_loc_oprnd1.v.val_unsigned > 0)
	  {
	    dw_loc_descr_ref repl
	      = int_loc_descriptor (loc->dw_loc_oprnd1.v.val_unsigned);
	    add_loc_descr (&repl, new_loc_descr (DW_OP_plus, 0, 0));
	    add_loc_descr (&repl, loc->dw_loc_next);
	    *loc = *repl;
	  }
	break;
      case DW_OP_implicit_value:
	if (loc->dw_loc_oprnd2.val_class == dw_val_class_addr
	    && !resolve_one_addr (&loc->dw_loc_oprnd2.v.val_addr))
	  return false;
	break;
      case DW_OP_implicit_pointer:
      case DW_OP_GNU_implicit_pointer:
      case DW_OP_GNU_parameter_ref:
      case DW_OP_GNU_variable_value:
	if (loc->dw_loc_oprnd1.val_class == dw_val_class_decl_ref)
	  {
	    dw_die_ref ref
	      = lookup_decl_die (loc->dw_loc_oprnd1.v.val_decl_ref);
	    if (ref == NULL)
	      return false;
	    loc->dw_loc_oprnd1.val_class = dw_val_class_die_ref;
	    loc->dw_loc_oprnd1.v.val_die_ref.die = ref;
	    loc->dw_loc_oprnd1.v.val_die_ref.external = 0;
	  }
	if (loc->dw_loc_opc == DW_OP_GNU_variable_value)
	  {
	    if (prev == NULL
		&& loc->dw_loc_next == NULL
		&& AT_class (a) == dw_val_class_loc)
	      switch (a->dw_attr)
		{
		  /* Following attributes allow both exprloc and reference,
		     so if the whole expression is DW_OP_GNU_variable_value
		     alone we could transform it into reference.  */
		case DW_AT_byte_size:
		case DW_AT_bit_size:
		case DW_AT_lower_bound:
		case DW_AT_upper_bound:
		case DW_AT_bit_stride:
		case DW_AT_count:
		case DW_AT_allocated:
		case DW_AT_associated:
		case DW_AT_byte_stride:
		  a->dw_attr_val.val_class = dw_val_class_die_ref;
		  a->dw_attr_val.val_entry = NULL;
		  a->dw_attr_val.v.val_die_ref.die
		    = loc->dw_loc_oprnd1.v.val_die_ref.die;
		  a->dw_attr_val.v.val_die_ref.external = 0;
		  return true;
		default:
		  break;
		}
	    if (dwarf_strict)
	      return false;
	  }
	break;
      case DW_OP_const_type:
      case DW_OP_regval_type:
      case DW_OP_deref_type:
      case DW_OP_convert:
      case DW_OP_reinterpret:
      case DW_OP_GNU_const_type:
      case DW_OP_GNU_regval_type:
      case DW_OP_GNU_deref_type:
      case DW_OP_GNU_convert:
      case DW_OP_GNU_reinterpret:
	while (loc->dw_loc_next
	       && (loc->dw_loc_next->dw_loc_opc == DW_OP_convert
		   || loc->dw_loc_next->dw_loc_opc == DW_OP_GNU_convert))
	  {
	    dw_die_ref base1, base2;
	    unsigned enc1, enc2, size1, size2;
	    if (loc->dw_loc_opc == DW_OP_regval_type
		|| loc->dw_loc_opc == DW_OP_deref_type
		|| loc->dw_loc_opc == DW_OP_GNU_regval_type
		|| loc->dw_loc_opc == DW_OP_GNU_deref_type)
	      base1 = loc->dw_loc_oprnd2.v.val_die_ref.die;
	    else if (loc->dw_loc_oprnd1.val_class
		     == dw_val_class_unsigned_const)
	      break;
	    else
	      base1 = loc->dw_loc_oprnd1.v.val_die_ref.die;
	    if (loc->dw_loc_next->dw_loc_oprnd1.val_class
		== dw_val_class_unsigned_const)
	      break;
	    base2 = loc->dw_loc_next->dw_loc_oprnd1.v.val_die_ref.die;
	    gcc_assert (base1->die_tag == DW_TAG_base_type
			&& base2->die_tag == DW_TAG_base_type);
	    enc1 = get_AT_unsigned (base1, DW_AT_encoding);
	    enc2 = get_AT_unsigned (base2, DW_AT_encoding);
	    size1 = get_AT_unsigned (base1, DW_AT_byte_size);
	    size2 = get_AT_unsigned (base2, DW_AT_byte_size);
	    if (size1 == size2
		&& (((enc1 == DW_ATE_unsigned || enc1 == DW_ATE_signed)
		     && (enc2 == DW_ATE_unsigned || enc2 == DW_ATE_signed)
		     && loc != keep)
		    || enc1 == enc2))
	      {
		/* Optimize away next DW_OP_convert after
		   adjusting LOC's base type die reference.  */
		if (loc->dw_loc_opc == DW_OP_regval_type
		    || loc->dw_loc_opc == DW_OP_deref_type
		    || loc->dw_loc_opc == DW_OP_GNU_regval_type
		    || loc->dw_loc_opc == DW_OP_GNU_deref_type)
		  loc->dw_loc_oprnd2.v.val_die_ref.die = base2;
		else
		  loc->dw_loc_oprnd1.v.val_die_ref.die = base2;
		loc->dw_loc_next = loc->dw_loc_next->dw_loc_next;
		continue;
	      }
	    /* Don't change integer DW_OP_convert after e.g. floating
	       point typed stack entry.  */
	    else if (enc1 != DW_ATE_unsigned && enc1 != DW_ATE_signed)
	      keep = loc->dw_loc_next;
	    break;
	  }
	break;
      default:
	break;
      }
  return true;
}

/* Helper function of resolve_addr.  DIE had DW_AT_location of
   DW_OP_addr alone, which referred to DECL in DW_OP_addr's operand
   and DW_OP_addr couldn't be resolved.  resolve_addr has already
   removed the DW_AT_location attribute.  This function attempts to
   add a new DW_AT_location attribute with DW_OP_implicit_pointer
   to it or DW_AT_const_value attribute, if possible.  */

static void
optimize_location_into_implicit_ptr (dw_die_ref die, tree decl)
{
  if (!VAR_P (decl)
      || lookup_decl_die (decl) != die
      || DECL_EXTERNAL (decl)
      || !TREE_STATIC (decl)
      || DECL_INITIAL (decl) == NULL_TREE
      || DECL_P (DECL_INITIAL (decl))
      || get_AT (die, DW_AT_const_value))
    return;

  tree init = DECL_INITIAL (decl);
  HOST_WIDE_INT offset = 0;
  /* For variables that have been optimized away and thus
     don't have a memory location, see if we can emit
     DW_AT_const_value instead.  */
  if (tree_add_const_value_attribute (die, init))
    return;
  if (dwarf_strict && dwarf_version < 5)
    return;
  /* If init is ADDR_EXPR or POINTER_PLUS_EXPR of ADDR_EXPR,
     and ADDR_EXPR refers to a decl that has DW_AT_location or
     DW_AT_const_value (but isn't addressable, otherwise
     resolving the original DW_OP_addr wouldn't fail), see if
     we can add DW_OP_implicit_pointer.  */
  STRIP_NOPS (init);
  if (TREE_CODE (init) == POINTER_PLUS_EXPR
      && tree_fits_shwi_p (TREE_OPERAND (init, 1)))
    {
      offset = tree_to_shwi (TREE_OPERAND (init, 1));
      init = TREE_OPERAND (init, 0);
      STRIP_NOPS (init);
    }
  if (TREE_CODE (init) != ADDR_EXPR)
    return;
  if ((TREE_CODE (TREE_OPERAND (init, 0)) == STRING_CST
       && !TREE_ASM_WRITTEN (TREE_OPERAND (init, 0)))
      || (TREE_CODE (TREE_OPERAND (init, 0)) == VAR_DECL
	  && !DECL_EXTERNAL (TREE_OPERAND (init, 0))
	  && TREE_OPERAND (init, 0) != decl))
    {
      dw_die_ref ref;
      dw_loc_descr_ref l;

      if (TREE_CODE (TREE_OPERAND (init, 0)) == STRING_CST)
	{
	  rtx rtl = string_cst_pool_decl (TREE_OPERAND (init, 0));
	  if (!rtl)
	    return;
	  decl = SYMBOL_REF_DECL (rtl);
	}
      else
	decl = TREE_OPERAND (init, 0);
      ref = lookup_decl_die (decl);
      if (ref == NULL
	  || (!get_AT (ref, DW_AT_location)
	      && !get_AT (ref, DW_AT_const_value)))
	return;
      l = new_loc_descr (dwarf_OP (DW_OP_implicit_pointer), 0, offset);
      l->dw_loc_oprnd1.val_class = dw_val_class_die_ref;
      l->dw_loc_oprnd1.v.val_die_ref.die = ref;
      l->dw_loc_oprnd1.v.val_die_ref.external = 0;
      add_AT_loc (die, DW_AT_location, l);
    }
}

/* Return NULL if l is a DWARF expression, or first op that is not
   valid DWARF expression.  */

static dw_loc_descr_ref
non_dwarf_expression (dw_loc_descr_ref l)
{
  while (l)
    {
      if (l->dw_loc_opc >= DW_OP_reg0 && l->dw_loc_opc <= DW_OP_reg31)
	return l;
      switch (l->dw_loc_opc)
	{
	case DW_OP_regx:
	case DW_OP_implicit_value:
	case DW_OP_stack_value:
	case DW_OP_implicit_pointer:
	case DW_OP_GNU_implicit_pointer:
	case DW_OP_GNU_parameter_ref:
	case DW_OP_piece:
	case DW_OP_bit_piece:
	  return l;
	default:
	  break;
	}
      l = l->dw_loc_next;
    }
  return NULL;
}

/* Return adjusted copy of EXPR:
   If it is empty DWARF expression, return it.
   If it is valid non-empty DWARF expression,
   return copy of EXPR with DW_OP_deref appended to it.
   If it is DWARF expression followed by DW_OP_reg{N,x}, return
   copy of the DWARF expression with DW_OP_breg{N,x} <0> appended.
   If it is DWARF expression followed by DW_OP_stack_value, return
   copy of the DWARF expression without anything appended.
   Otherwise, return NULL.  */

static dw_loc_descr_ref
copy_deref_exprloc (dw_loc_descr_ref expr)
{
  dw_loc_descr_ref tail = NULL;

  if (expr == NULL)
    return NULL;

  dw_loc_descr_ref l = non_dwarf_expression (expr);
  if (l && l->dw_loc_next)
    return NULL;

  if (l)
    {
      if (l->dw_loc_opc >= DW_OP_reg0 && l->dw_loc_opc <= DW_OP_reg31)
	tail = new_loc_descr ((enum dwarf_location_atom)
			      (DW_OP_breg0 + (l->dw_loc_opc - DW_OP_reg0)),
			      0, 0);
      else
	switch (l->dw_loc_opc)
	  {
	  case DW_OP_regx:
	    tail = new_loc_descr (DW_OP_bregx,
				  l->dw_loc_oprnd1.v.val_unsigned, 0);
	    break;
	  case DW_OP_stack_value:
	    break;
	  default:
	    return NULL;
	  }
    }
  else
    tail = new_loc_descr (DW_OP_deref, 0, 0);

  dw_loc_descr_ref ret = NULL, *p = &ret;
  while (expr != l)
    {
      *p = new_loc_descr (expr->dw_loc_opc, 0, 0);
      (*p)->dw_loc_oprnd1 = expr->dw_loc_oprnd1;
      (*p)->dw_loc_oprnd2 = expr->dw_loc_oprnd2;
      p = &(*p)->dw_loc_next;
      expr = expr->dw_loc_next;
    }
  *p = tail;
  return ret;
}

/* For DW_AT_string_length attribute with DW_OP_GNU_variable_value
   reference to a variable or argument, adjust it if needed and return:
   -1 if the DW_AT_string_length attribute and DW_AT_{string_length_,}byte_size
      attribute if present should be removed
   0 keep the attribute perhaps with minor modifications, no need to rescan
   1 if the attribute has been successfully adjusted.  */

static int
optimize_string_length (dw_attr_node *a)
{
  dw_loc_descr_ref l = AT_loc (a), lv;
  dw_die_ref die;
  if (l->dw_loc_oprnd1.val_class == dw_val_class_decl_ref)
    {
      tree decl = l->dw_loc_oprnd1.v.val_decl_ref;
      die = lookup_decl_die (decl);
      if (die)
	{
	  l->dw_loc_oprnd1.val_class = dw_val_class_die_ref;
	  l->dw_loc_oprnd1.v.val_die_ref.die = die;
	  l->dw_loc_oprnd1.v.val_die_ref.external = 0;
	}
      else
	return -1;
    }
  else
    die = l->dw_loc_oprnd1.v.val_die_ref.die;

  /* DWARF5 allows reference class, so we can then reference the DIE.
     Only do this for DW_OP_GNU_variable_value DW_OP_stack_value.  */
  if (l->dw_loc_next != NULL && dwarf_version >= 5)
    {
      a->dw_attr_val.val_class = dw_val_class_die_ref;
      a->dw_attr_val.val_entry = NULL;
      a->dw_attr_val.v.val_die_ref.die = die;
      a->dw_attr_val.v.val_die_ref.external = 0;
      return 0;
    }

  dw_attr_node *av = get_AT (die, DW_AT_location);
  dw_loc_list_ref d;
  bool non_dwarf_expr = false;

  if (av == NULL)
    return dwarf_strict ? -1 : 0;
  switch (AT_class (av))
    {
    case dw_val_class_loc_list:
      for (d = AT_loc_list (av); d != NULL; d = d->dw_loc_next)
	if (d->expr && non_dwarf_expression (d->expr))
	  non_dwarf_expr = true;
      break;
    case dw_val_class_loc:
      lv = AT_loc (av);
      if (lv == NULL)
	return dwarf_strict ? -1 : 0;
      if (non_dwarf_expression (lv))
	non_dwarf_expr = true;
      break;
    default:
      return dwarf_strict ? -1 : 0;
    }

  /* If it is safe to transform DW_OP_GNU_variable_value DW_OP_stack_value
     into DW_OP_call4  or DW_OP_GNU_variable_value into
     DW_OP_call4 DW_OP_deref, do so.  */
  if (!non_dwarf_expr
      && (l->dw_loc_next != NULL || AT_class (av) == dw_val_class_loc))
    {
      l->dw_loc_opc = DW_OP_call4;
      if (l->dw_loc_next)
	l->dw_loc_next = NULL;
      else
	l->dw_loc_next = new_loc_descr (DW_OP_deref, 0, 0);
      return 0;
    }

  /* For DW_OP_GNU_variable_value DW_OP_stack_value, we can just
     copy over the DW_AT_location attribute from die to a.  */
  if (l->dw_loc_next != NULL)
    {
      a->dw_attr_val = av->dw_attr_val;
      return 1;
    }

  dw_loc_list_ref list, *p;
  switch (AT_class (av))
    {
    case dw_val_class_loc_list:
      p = &list;
      list = NULL;
      for (d = AT_loc_list (av); d != NULL; d = d->dw_loc_next)
	{
	  lv = copy_deref_exprloc (d->expr);
	  if (lv)
	    {
	      *p = new_loc_list (lv, d->begin, d->end, d->section);
	      p = &(*p)->dw_loc_next;
	    }
	  else if (!dwarf_strict && d->expr)
	    return 0;
	}
      if (list == NULL)
	return dwarf_strict ? -1 : 0;
      a->dw_attr_val.val_class = dw_val_class_loc_list;
      gen_llsym (list);
      *AT_loc_list_ptr (a) = list;
      return 1;
    case dw_val_class_loc:
      lv = copy_deref_exprloc (AT_loc (av));
      if (lv == NULL)
	return dwarf_strict ? -1 : 0;
      a->dw_attr_val.v.val_loc = lv;
      return 1;
    default:
      gcc_unreachable ();
    }
}

/* Resolve DW_OP_addr and DW_AT_const_value CONST_STRING arguments to
   an address in .rodata section if the string literal is emitted there,
   or remove the containing location list or replace DW_AT_const_value
   with DW_AT_location and empty location expression, if it isn't found
   in .rodata.  Similarly for SYMBOL_REFs, keep only those that refer
   to something that has been emitted in the current CU.  */

static void
resolve_addr (dw_die_ref die)
{
  dw_die_ref c;
  dw_attr_node *a;
  dw_loc_list_ref *curr, *start, loc;
  unsigned ix;
  bool remove_AT_byte_size = false;

  FOR_EACH_VEC_SAFE_ELT (die->die_attr, ix, a)
    switch (AT_class (a))
      {
      case dw_val_class_loc_list:
	start = curr = AT_loc_list_ptr (a);
	loc = *curr;
	gcc_assert (loc);
	/* The same list can be referenced more than once.  See if we have
	   already recorded the result from a previous pass.  */
	if (loc->replaced)
	  *curr = loc->dw_loc_next;
	else if (!loc->resolved_addr)
	  {
	    /* As things stand, we do not expect or allow one die to
	       reference a suffix of another die's location list chain.
	       References must be identical or completely separate.
	       There is therefore no need to cache the result of this
	       pass on any list other than the first; doing so
	       would lead to unnecessary writes.  */
	    while (*curr)
	      {
		gcc_assert (!(*curr)->replaced && !(*curr)->resolved_addr);
		if (!resolve_addr_in_expr (a, (*curr)->expr))
		  {
		    dw_loc_list_ref next = (*curr)->dw_loc_next;
                    dw_loc_descr_ref l = (*curr)->expr;

		    if (next && (*curr)->ll_symbol)
		      {
			gcc_assert (!next->ll_symbol);
			next->ll_symbol = (*curr)->ll_symbol;
		      }
                    if (dwarf_split_debug_info)
                      remove_loc_list_addr_table_entries (l);
		    *curr = next;
		  }
		else
		  {
		    mark_base_types ((*curr)->expr);
		    curr = &(*curr)->dw_loc_next;
		  }
	      }
	    if (loc == *start)
	      loc->resolved_addr = 1;
	    else
	      {
		loc->replaced = 1;
		loc->dw_loc_next = *start;
	      }
	  }
	if (!*start)
	  {
	    remove_AT (die, a->dw_attr);
	    ix--;
	  }
	break;
      case dw_val_class_loc:
	{
	  dw_loc_descr_ref l = AT_loc (a);
	  /* DW_OP_GNU_variable_value DW_OP_stack_value or
	     DW_OP_GNU_variable_value in DW_AT_string_length can be converted
	     into DW_OP_call4 or DW_OP_call4 DW_OP_deref, which is standard
	     DWARF4 unlike DW_OP_GNU_variable_value.  Or for DWARF5
	     DW_OP_GNU_variable_value DW_OP_stack_value can be replaced
	     with DW_FORM_ref referencing the same DIE as
	     DW_OP_GNU_variable_value used to reference.  */
	  if (a->dw_attr == DW_AT_string_length
	      && l
	      && l->dw_loc_opc == DW_OP_GNU_variable_value
	      && (l->dw_loc_next == NULL
		  || (l->dw_loc_next->dw_loc_next == NULL
		      && l->dw_loc_next->dw_loc_opc == DW_OP_stack_value)))
	    {
	      switch (optimize_string_length (a))
		{
		case -1:
		  remove_AT (die, a->dw_attr);
		  ix--;
		  /* If we drop DW_AT_string_length, we need to drop also
		     DW_AT_{string_length_,}byte_size.  */
		  remove_AT_byte_size = true;
		  continue;
		default:
		  break;
		case 1:
		  /* Even if we keep the optimized DW_AT_string_length,
		     it might have changed AT_class, so process it again.  */
		  ix--;
		  continue;
		}
	    }
	  /* For -gdwarf-2 don't attempt to optimize
	     DW_AT_data_member_location containing
	     DW_OP_plus_uconst - older consumers might
	     rely on it being that op instead of a more complex,
	     but shorter, location description.  */
	  if ((dwarf_version > 2
	       || a->dw_attr != DW_AT_data_member_location
	       || l == NULL
	       || l->dw_loc_opc != DW_OP_plus_uconst
	       || l->dw_loc_next != NULL)
	      && !resolve_addr_in_expr (a, l))
	    {
	      if (dwarf_split_debug_info)
		remove_loc_list_addr_table_entries (l);
	      if (l != NULL
		  && l->dw_loc_next == NULL
		  && l->dw_loc_opc == DW_OP_addr
		  && GET_CODE (l->dw_loc_oprnd1.v.val_addr) == SYMBOL_REF
		  && SYMBOL_REF_DECL (l->dw_loc_oprnd1.v.val_addr)
		  && a->dw_attr == DW_AT_location)
		{
		  tree decl = SYMBOL_REF_DECL (l->dw_loc_oprnd1.v.val_addr);
		  remove_AT (die, a->dw_attr);
		  ix--;
		  optimize_location_into_implicit_ptr (die, decl);
		  break;
		}
	      if (a->dw_attr == DW_AT_string_length)
		/* If we drop DW_AT_string_length, we need to drop also
		   DW_AT_{string_length_,}byte_size.  */
		remove_AT_byte_size = true;
	      remove_AT (die, a->dw_attr);
	      ix--;
	    }
	  else
	    mark_base_types (l);
	}
	break;
      case dw_val_class_addr:
	if (a->dw_attr == DW_AT_const_value
	    && !resolve_one_addr (&a->dw_attr_val.v.val_addr))
	  {
            if (AT_index (a) != NOT_INDEXED)
              remove_addr_table_entry (a->dw_attr_val.val_entry);
	    remove_AT (die, a->dw_attr);
	    ix--;
	  }
	if ((die->die_tag == DW_TAG_call_site
	     && a->dw_attr == DW_AT_call_origin)
	    || (die->die_tag == DW_TAG_GNU_call_site
		&& a->dw_attr == DW_AT_abstract_origin))
	  {
	    tree tdecl = SYMBOL_REF_DECL (a->dw_attr_val.v.val_addr);
	    dw_die_ref tdie = lookup_decl_die (tdecl);
	    dw_die_ref cdie;
	    if (tdie == NULL
		&& DECL_EXTERNAL (tdecl)
		&& DECL_ABSTRACT_ORIGIN (tdecl) == NULL_TREE
		&& (cdie = lookup_context_die (DECL_CONTEXT (tdecl))))
	      {
		dw_die_ref pdie = cdie;
		/* Make sure we don't add these DIEs into type units.
		   We could emit skeleton DIEs for context (namespaces,
		   outer structs/classes) and a skeleton DIE for the
		   innermost context with DW_AT_signature pointing to the
		   type unit.  See PR78835.  */
		while (pdie && pdie->die_tag != DW_TAG_type_unit)
		  pdie = pdie->die_parent;
		if (pdie == NULL)
		  {
		    /* Creating a full DIE for tdecl is overly expensive and
		       at this point even wrong when in the LTO phase
		       as it can end up generating new type DIEs we didn't
		       output and thus optimize_external_refs will crash.  */
		    tdie = new_die (DW_TAG_subprogram, cdie, NULL_TREE);
		    add_AT_flag (tdie, DW_AT_external, 1);
		    add_AT_flag (tdie, DW_AT_declaration, 1);
		    add_linkage_attr (tdie, tdecl);
		    add_name_and_src_coords_attributes (tdie, tdecl, true);
		    equate_decl_number_to_die (tdecl, tdie);
		  }
	      }
	    if (tdie)
	      {
		a->dw_attr_val.val_class = dw_val_class_die_ref;
		a->dw_attr_val.v.val_die_ref.die = tdie;
		a->dw_attr_val.v.val_die_ref.external = 0;
	      }
	    else
	      {
                if (AT_index (a) != NOT_INDEXED)
                  remove_addr_table_entry (a->dw_attr_val.val_entry);
		remove_AT (die, a->dw_attr);
		ix--;
	      }
	  }
	break;
      default:
	break;
      }

  if (remove_AT_byte_size)
    remove_AT (die, dwarf_version >= 5
		    ? DW_AT_string_length_byte_size
		    : DW_AT_byte_size);

  FOR_EACH_CHILD (die, c, resolve_addr (c));
}

/* Helper routines for optimize_location_lists.
   This pass tries to share identical local lists in .debug_loc
   section.  */

/* Iteratively hash operands of LOC opcode into HSTATE.  */

static void
hash_loc_operands (dw_loc_descr_ref loc, inchash::hash &hstate)
{
  dw_val_ref val1 = &loc->dw_loc_oprnd1;
  dw_val_ref val2 = &loc->dw_loc_oprnd2;

  switch (loc->dw_loc_opc)
    {
    case DW_OP_const4u:
    case DW_OP_const8u:
      if (loc->dtprel)
	goto hash_addr;
      /* FALLTHRU */
    case DW_OP_const1u:
    case DW_OP_const1s:
    case DW_OP_const2u:
    case DW_OP_const2s:
    case DW_OP_const4s:
    case DW_OP_const8s:
    case DW_OP_constu:
    case DW_OP_consts:
    case DW_OP_pick:
    case DW_OP_plus_uconst:
    case DW_OP_breg0:
    case DW_OP_breg1:
    case DW_OP_breg2:
    case DW_OP_breg3:
    case DW_OP_breg4:
    case DW_OP_breg5:
    case DW_OP_breg6:
    case DW_OP_breg7:
    case DW_OP_breg8:
    case DW_OP_breg9:
    case DW_OP_breg10:
    case DW_OP_breg11:
    case DW_OP_breg12:
    case DW_OP_breg13:
    case DW_OP_breg14:
    case DW_OP_breg15:
    case DW_OP_breg16:
    case DW_OP_breg17:
    case DW_OP_breg18:
    case DW_OP_breg19:
    case DW_OP_breg20:
    case DW_OP_breg21:
    case DW_OP_breg22:
    case DW_OP_breg23:
    case DW_OP_breg24:
    case DW_OP_breg25:
    case DW_OP_breg26:
    case DW_OP_breg27:
    case DW_OP_breg28:
    case DW_OP_breg29:
    case DW_OP_breg30:
    case DW_OP_breg31:
    case DW_OP_regx:
    case DW_OP_fbreg:
    case DW_OP_piece:
    case DW_OP_deref_size:
    case DW_OP_xderef_size:
      hstate.add_object (val1->v.val_int);
      break;
    case DW_OP_skip:
    case DW_OP_bra:
      {
	int offset;

	gcc_assert (val1->val_class == dw_val_class_loc);
	offset = val1->v.val_loc->dw_loc_addr - (loc->dw_loc_addr + 3);
	hstate.add_object (offset);
      }
      break;
    case DW_OP_implicit_value:
      hstate.add_object (val1->v.val_unsigned);
      switch (val2->val_class)
	{
	case dw_val_class_const:
	  hstate.add_object (val2->v.val_int);
	  break;
	case dw_val_class_vec:
	  {
	    unsigned int elt_size = val2->v.val_vec.elt_size;
	    unsigned int len = val2->v.val_vec.length;

	    hstate.add_int (elt_size);
	    hstate.add_int (len);
	    hstate.add (val2->v.val_vec.array, len * elt_size);
	  }
	  break;
	case dw_val_class_const_double:
	  hstate.add_object (val2->v.val_double.low);
	  hstate.add_object (val2->v.val_double.high);
	  break;
	case dw_val_class_wide_int:
	  hstate.add (val2->v.val_wide->get_val (),
		      get_full_len (*val2->v.val_wide)
		      * HOST_BITS_PER_WIDE_INT / HOST_BITS_PER_CHAR);
	  break;
	case dw_val_class_addr:	
	  inchash::add_rtx (val2->v.val_addr, hstate);
	  break;
	default:
	  gcc_unreachable ();
	}
      break;
    case DW_OP_bregx:
    case DW_OP_bit_piece:
      hstate.add_object (val1->v.val_int);
      hstate.add_object (val2->v.val_int);
      break;
    case DW_OP_addr:
    hash_addr:
      if (loc->dtprel)
	{
	  unsigned char dtprel = 0xd1;
	  hstate.add_object (dtprel);
	}
      inchash::add_rtx (val1->v.val_addr, hstate);
      break;
    case DW_OP_GNU_addr_index:
    case DW_OP_GNU_const_index:
      {
        if (loc->dtprel)
          {
            unsigned char dtprel = 0xd1;
	    hstate.add_object (dtprel);
          }
        inchash::add_rtx (val1->val_entry->addr.rtl, hstate);
      }
      break;
    case DW_OP_implicit_pointer:
    case DW_OP_GNU_implicit_pointer:
      hstate.add_int (val2->v.val_int);
      break;
    case DW_OP_entry_value:
    case DW_OP_GNU_entry_value:
      hstate.add_object (val1->v.val_loc);
      break;
    case DW_OP_regval_type:
    case DW_OP_deref_type:
    case DW_OP_GNU_regval_type:
    case DW_OP_GNU_deref_type:
      {
	unsigned int byte_size
	  = get_AT_unsigned (val2->v.val_die_ref.die, DW_AT_byte_size);
	unsigned int encoding
	  = get_AT_unsigned (val2->v.val_die_ref.die, DW_AT_encoding);
	hstate.add_object (val1->v.val_int);
	hstate.add_object (byte_size);
	hstate.add_object (encoding);
      }
      break;
    case DW_OP_convert:
    case DW_OP_reinterpret:
    case DW_OP_GNU_convert:
    case DW_OP_GNU_reinterpret:
      if (val1->val_class == dw_val_class_unsigned_const)
	{
	  hstate.add_object (val1->v.val_unsigned);
	  break;
	}
      /* FALLTHRU */
    case DW_OP_const_type:
    case DW_OP_GNU_const_type:
      {
	unsigned int byte_size
	  = get_AT_unsigned (val1->v.val_die_ref.die, DW_AT_byte_size);
	unsigned int encoding
	  = get_AT_unsigned (val1->v.val_die_ref.die, DW_AT_encoding);
	hstate.add_object (byte_size);
	hstate.add_object (encoding);
	if (loc->dw_loc_opc != DW_OP_const_type
	    && loc->dw_loc_opc != DW_OP_GNU_const_type)
	  break;
	hstate.add_object (val2->val_class);
	switch (val2->val_class)
	  {
	  case dw_val_class_const:
	    hstate.add_object (val2->v.val_int);
	    break;
	  case dw_val_class_vec:
	    {
	      unsigned int elt_size = val2->v.val_vec.elt_size;
	      unsigned int len = val2->v.val_vec.length;

	      hstate.add_object (elt_size);
	      hstate.add_object (len);
	      hstate.add (val2->v.val_vec.array, len * elt_size);
	    }
	    break;
	  case dw_val_class_const_double:
	    hstate.add_object (val2->v.val_double.low);
	    hstate.add_object (val2->v.val_double.high);
	    break;
	  case dw_val_class_wide_int:
	    hstate.add (val2->v.val_wide->get_val (),
			get_full_len (*val2->v.val_wide)
			* HOST_BITS_PER_WIDE_INT / HOST_BITS_PER_CHAR);
	    break;
	  default:
	    gcc_unreachable ();
	  }
      }
      break;

    default:
      /* Other codes have no operands.  */
      break;
    }
}

/* Iteratively hash the whole DWARF location expression LOC into HSTATE.  */

static inline void
hash_locs (dw_loc_descr_ref loc, inchash::hash &hstate)
{
  dw_loc_descr_ref l;
  bool sizes_computed = false;
  /* Compute sizes, so that DW_OP_skip/DW_OP_bra can be checksummed.  */
  size_of_locs (loc);

  for (l = loc; l != NULL; l = l->dw_loc_next)
    {
      enum dwarf_location_atom opc = l->dw_loc_opc;
      hstate.add_object (opc);
      if ((opc == DW_OP_skip || opc == DW_OP_bra) && !sizes_computed)
	{
	  size_of_locs (loc);
	  sizes_computed = true;
	}
      hash_loc_operands (l, hstate);
    }
}

/* Compute hash of the whole location list LIST_HEAD.  */

static inline void
hash_loc_list (dw_loc_list_ref list_head)
{
  dw_loc_list_ref curr = list_head;
  inchash::hash hstate;

  for (curr = list_head; curr != NULL; curr = curr->dw_loc_next)
    {
      hstate.add (curr->begin, strlen (curr->begin) + 1);
      hstate.add (curr->end, strlen (curr->end) + 1);
      if (curr->section)
	hstate.add (curr->section, strlen (curr->section) + 1);
      hash_locs (curr->expr, hstate);
    }
  list_head->hash = hstate.end ();
}

/* Return true if X and Y opcodes have the same operands.  */

static inline bool
compare_loc_operands (dw_loc_descr_ref x, dw_loc_descr_ref y)
{
  dw_val_ref valx1 = &x->dw_loc_oprnd1;
  dw_val_ref valx2 = &x->dw_loc_oprnd2;
  dw_val_ref valy1 = &y->dw_loc_oprnd1;
  dw_val_ref valy2 = &y->dw_loc_oprnd2;

  switch (x->dw_loc_opc)
    {
    case DW_OP_const4u:
    case DW_OP_const8u:
      if (x->dtprel)
	goto hash_addr;
      /* FALLTHRU */
    case DW_OP_const1u:
    case DW_OP_const1s:
    case DW_OP_const2u:
    case DW_OP_const2s:
    case DW_OP_const4s:
    case DW_OP_const8s:
    case DW_OP_constu:
    case DW_OP_consts:
    case DW_OP_pick:
    case DW_OP_plus_uconst:
    case DW_OP_breg0:
    case DW_OP_breg1:
    case DW_OP_breg2:
    case DW_OP_breg3:
    case DW_OP_breg4:
    case DW_OP_breg5:
    case DW_OP_breg6:
    case DW_OP_breg7:
    case DW_OP_breg8:
    case DW_OP_breg9:
    case DW_OP_breg10:
    case DW_OP_breg11:
    case DW_OP_breg12:
    case DW_OP_breg13:
    case DW_OP_breg14:
    case DW_OP_breg15:
    case DW_OP_breg16:
    case DW_OP_breg17:
    case DW_OP_breg18:
    case DW_OP_breg19:
    case DW_OP_breg20:
    case DW_OP_breg21:
    case DW_OP_breg22:
    case DW_OP_breg23:
    case DW_OP_breg24:
    case DW_OP_breg25:
    case DW_OP_breg26:
    case DW_OP_breg27:
    case DW_OP_breg28:
    case DW_OP_breg29:
    case DW_OP_breg30:
    case DW_OP_breg31:
    case DW_OP_regx:
    case DW_OP_fbreg:
    case DW_OP_piece:
    case DW_OP_deref_size:
    case DW_OP_xderef_size:
      return valx1->v.val_int == valy1->v.val_int;
    case DW_OP_skip:
    case DW_OP_bra:
      /* If splitting debug info, the use of DW_OP_GNU_addr_index
        can cause irrelevant differences in dw_loc_addr.  */
      gcc_assert (valx1->val_class == dw_val_class_loc
		  && valy1->val_class == dw_val_class_loc
                  && (dwarf_split_debug_info
                      || x->dw_loc_addr == y->dw_loc_addr));
      return valx1->v.val_loc->dw_loc_addr == valy1->v.val_loc->dw_loc_addr;
    case DW_OP_implicit_value:
      if (valx1->v.val_unsigned != valy1->v.val_unsigned
	  || valx2->val_class != valy2->val_class)
	return false;
      switch (valx2->val_class)
	{
	case dw_val_class_const:
	  return valx2->v.val_int == valy2->v.val_int;
	case dw_val_class_vec:
	  return valx2->v.val_vec.elt_size == valy2->v.val_vec.elt_size
		 && valx2->v.val_vec.length == valy2->v.val_vec.length
		 && memcmp (valx2->v.val_vec.array, valy2->v.val_vec.array,
			    valx2->v.val_vec.elt_size
			    * valx2->v.val_vec.length) == 0;
	case dw_val_class_const_double:
	  return valx2->v.val_double.low == valy2->v.val_double.low
		 && valx2->v.val_double.high == valy2->v.val_double.high;
	case dw_val_class_wide_int:
	  return *valx2->v.val_wide == *valy2->v.val_wide;
	case dw_val_class_addr:
	  return rtx_equal_p (valx2->v.val_addr, valy2->v.val_addr);
	default:
	  gcc_unreachable ();
	}
    case DW_OP_bregx:
    case DW_OP_bit_piece:
      return valx1->v.val_int == valy1->v.val_int
	     && valx2->v.val_int == valy2->v.val_int;
    case DW_OP_addr:
    hash_addr:
      return rtx_equal_p (valx1->v.val_addr, valy1->v.val_addr);
    case DW_OP_GNU_addr_index:
    case DW_OP_GNU_const_index:
      {
        rtx ax1 = valx1->val_entry->addr.rtl;
        rtx ay1 = valy1->val_entry->addr.rtl;
        return rtx_equal_p (ax1, ay1);
      }
    case DW_OP_implicit_pointer:
    case DW_OP_GNU_implicit_pointer:
      return valx1->val_class == dw_val_class_die_ref
	     && valx1->val_class == valy1->val_class
	     && valx1->v.val_die_ref.die == valy1->v.val_die_ref.die
	     && valx2->v.val_int == valy2->v.val_int;
    case DW_OP_entry_value:
    case DW_OP_GNU_entry_value:
      return compare_loc_operands (valx1->v.val_loc, valy1->v.val_loc);
    case DW_OP_const_type:
    case DW_OP_GNU_const_type:
      if (valx1->v.val_die_ref.die != valy1->v.val_die_ref.die
	  || valx2->val_class != valy2->val_class)
	return false;
      switch (valx2->val_class)
	{
	case dw_val_class_const:
	  return valx2->v.val_int == valy2->v.val_int;
	case dw_val_class_vec:
	  return valx2->v.val_vec.elt_size == valy2->v.val_vec.elt_size
		 && valx2->v.val_vec.length == valy2->v.val_vec.length
		 && memcmp (valx2->v.val_vec.array, valy2->v.val_vec.array,
			    valx2->v.val_vec.elt_size
			    * valx2->v.val_vec.length) == 0;
	case dw_val_class_const_double:
	  return valx2->v.val_double.low == valy2->v.val_double.low
		 && valx2->v.val_double.high == valy2->v.val_double.high;
	case dw_val_class_wide_int:
	  return *valx2->v.val_wide == *valy2->v.val_wide;
	default:
	  gcc_unreachable ();
	}
    case DW_OP_regval_type:
    case DW_OP_deref_type:
    case DW_OP_GNU_regval_type:
    case DW_OP_GNU_deref_type:
      return valx1->v.val_int == valy1->v.val_int
	     && valx2->v.val_die_ref.die == valy2->v.val_die_ref.die;
    case DW_OP_convert:
    case DW_OP_reinterpret:
    case DW_OP_GNU_convert:
    case DW_OP_GNU_reinterpret:
      if (valx1->val_class != valy1->val_class)
	return false;
      if (valx1->val_class == dw_val_class_unsigned_const)
	return valx1->v.val_unsigned == valy1->v.val_unsigned;
      return valx1->v.val_die_ref.die == valy1->v.val_die_ref.die;
    case DW_OP_GNU_parameter_ref:
      return valx1->val_class == dw_val_class_die_ref
	     && valx1->val_class == valy1->val_class
	     && valx1->v.val_die_ref.die == valy1->v.val_die_ref.die;
    default:
      /* Other codes have no operands.  */
      return true;
    }
}

/* Return true if DWARF location expressions X and Y are the same.  */

static inline bool
compare_locs (dw_loc_descr_ref x, dw_loc_descr_ref y)
{
  for (; x != NULL && y != NULL; x = x->dw_loc_next, y = y->dw_loc_next)
    if (x->dw_loc_opc != y->dw_loc_opc
	|| x->dtprel != y->dtprel
	|| !compare_loc_operands (x, y))
      break;
  return x == NULL && y == NULL;
}

/* Hashtable helpers.  */

struct loc_list_hasher : nofree_ptr_hash <dw_loc_list_struct>
{
  static inline hashval_t hash (const dw_loc_list_struct *);
  static inline bool equal (const dw_loc_list_struct *,
			    const dw_loc_list_struct *);
};

/* Return precomputed hash of location list X.  */

inline hashval_t
loc_list_hasher::hash (const dw_loc_list_struct *x)
{
  return x->hash;
}

/* Return true if location lists A and B are the same.  */

inline bool
loc_list_hasher::equal (const dw_loc_list_struct *a,
			const dw_loc_list_struct *b)
{
  if (a == b)
    return 1;
  if (a->hash != b->hash)
    return 0;
  for (; a != NULL && b != NULL; a = a->dw_loc_next, b = b->dw_loc_next)
    if (strcmp (a->begin, b->begin) != 0
	|| strcmp (a->end, b->end) != 0
	|| (a->section == NULL) != (b->section == NULL)
	|| (a->section && strcmp (a->section, b->section) != 0)
	|| !compare_locs (a->expr, b->expr))
      break;
  return a == NULL && b == NULL;
}

typedef hash_table<loc_list_hasher> loc_list_hash_type;


/* Recursively optimize location lists referenced from DIE
   children and share them whenever possible.  */

static void
optimize_location_lists_1 (dw_die_ref die, loc_list_hash_type *htab)
{
  dw_die_ref c;
  dw_attr_node *a;
  unsigned ix;
  dw_loc_list_struct **slot;

  FOR_EACH_VEC_SAFE_ELT (die->die_attr, ix, a)
    if (AT_class (a) == dw_val_class_loc_list)
      {
	dw_loc_list_ref list = AT_loc_list (a);
	/* TODO: perform some optimizations here, before hashing
	   it and storing into the hash table.  */
	hash_loc_list (list);
	slot = htab->find_slot_with_hash (list, list->hash, INSERT);
	if (*slot == NULL)
	  *slot = list;
	else
          a->dw_attr_val.v.val_loc_list = *slot;
      }

  FOR_EACH_CHILD (die, c, optimize_location_lists_1 (c, htab));
}


/* Recursively assign each location list a unique index into the debug_addr
   section.  */

static void
index_location_lists (dw_die_ref die)
{
  dw_die_ref c;
  dw_attr_node *a;
  unsigned ix;

  FOR_EACH_VEC_SAFE_ELT (die->die_attr, ix, a)
    if (AT_class (a) == dw_val_class_loc_list)
      {
        dw_loc_list_ref list = AT_loc_list (a);
        dw_loc_list_ref curr;
        for (curr = list; curr != NULL; curr = curr->dw_loc_next)
          {
            /* Don't index an entry that has already been indexed
               or won't be output.  */
            if (curr->begin_entry != NULL
                || (strcmp (curr->begin, curr->end) == 0 && !curr->force))
              continue;

            curr->begin_entry
	      = add_addr_table_entry (xstrdup (curr->begin), ate_kind_label);
          }
      }

  FOR_EACH_CHILD (die, c, index_location_lists (c));
}

/* Optimize location lists referenced from DIE
   children and share them whenever possible.  */

static void
optimize_location_lists (dw_die_ref die)
{
  loc_list_hash_type htab (500);
  optimize_location_lists_1 (die, &htab);
}

/* Traverse the limbo die list, and add parent/child links.  The only
   dies without parents that should be here are concrete instances of
   inline functions, and the comp_unit_die.  We can ignore the comp_unit_die.
   For concrete instances, we can get the parent die from the abstract
   instance.  */

static void
flush_limbo_die_list (void)
{
  limbo_die_node *node;

  /* get_context_die calls force_decl_die, which can put new DIEs on the
     limbo list in LTO mode when nested functions are put in a different
     partition than that of their parent function.  */
  while ((node = limbo_die_list))
    {
      dw_die_ref die = node->die;
      limbo_die_list = node->next;

      if (die->die_parent == NULL)
	{
	  dw_die_ref origin = get_AT_ref (die, DW_AT_abstract_origin);

	  if (origin && origin->die_parent)
	    add_child_die (origin->die_parent, die);
	  else if (is_cu_die (die))
	    ;
	  else if (seen_error ())
	    /* It's OK to be confused by errors in the input.  */
	    add_child_die (comp_unit_die (), die);
	  else
	    {
	      /* In certain situations, the lexical block containing a
		 nested function can be optimized away, which results
		 in the nested function die being orphaned.  Likewise
		 with the return type of that nested function.  Force
		 this to be a child of the containing function.

		 It may happen that even the containing function got fully
		 inlined and optimized out.  In that case we are lost and
		 assign the empty child.  This should not be big issue as
		 the function is likely unreachable too.  */
	      gcc_assert (node->created_for);

	      if (DECL_P (node->created_for))
		origin = get_context_die (DECL_CONTEXT (node->created_for));
	      else if (TYPE_P (node->created_for))
		origin = scope_die_for (node->created_for, comp_unit_die ());
	      else
		origin = comp_unit_die ();

	      add_child_die (origin, die);
	    }
	}
    }
}

/* Reset DIEs so we can output them again.  */

static void
reset_dies (dw_die_ref die)
{
  dw_die_ref c;

  /* Remove stuff we re-generate.  */
  die->die_mark = 0;
  die->die_offset = 0;
  die->die_abbrev = 0;
  remove_AT (die, DW_AT_sibling);

  FOR_EACH_CHILD (die, c, reset_dies (c));
}

/* Output stuff that dwarf requires at the end of every file,
   and generate the DWARF-2 debugging info.  */

static void
dwarf2out_finish (const char *)
{
  comdat_type_node *ctnode;
  dw_die_ref main_comp_unit_die;
  unsigned char checksum[16];
  char dl_section_ref[MAX_ARTIFICIAL_LABEL_BYTES];

  /* Flush out any latecomers to the limbo party.  */
  flush_limbo_die_list ();

  if (flag_checking)
    {
      verify_die (comp_unit_die ());
      for (limbo_die_node *node = cu_die_list; node; node = node->next)
	verify_die (node->die);
    }

  /* We shouldn't have any symbols with delayed asm names for
     DIEs generated after early finish.  */
  gcc_assert (deferred_asm_name == NULL);

  gen_remaining_tmpl_value_param_die_attribute ();

  if (flag_generate_lto || flag_generate_offload)
    {
      gcc_assert (flag_fat_lto_objects || flag_generate_offload);

      /* Prune stuff so that dwarf2out_finish runs successfully
	 for the fat part of the object.  */
      reset_dies (comp_unit_die ());
      for (limbo_die_node *node = cu_die_list; node; node = node->next)
	reset_dies (node->die);

      hash_table<comdat_type_hasher> comdat_type_table (100);
      for (ctnode = comdat_type_list; ctnode != NULL; ctnode = ctnode->next)
	{
	  comdat_type_node **slot
	      = comdat_type_table.find_slot (ctnode, INSERT);

	  /* Don't reset types twice.  */
	  if (*slot != HTAB_EMPTY_ENTRY)
	    continue;

	  /* Add a pointer to the line table for the main compilation unit
	     so that the debugger can make sense of DW_AT_decl_file
	     attributes.  */
	  if (debug_info_level >= DINFO_LEVEL_TERSE)
	    reset_dies (ctnode->root_die);

	  *slot = ctnode;
	}

      /* Reset die CU symbol so we don't output it twice.  */
      comp_unit_die ()->die_id.die_symbol = NULL;

      /* Remove DW_AT_macro from the early output.  */
      if (have_macinfo)
	remove_AT (comp_unit_die (), DEBUG_MACRO_ATTRIBUTE);

      /* Remove indirect string decisions.  */
      debug_str_hash->traverse<void *, reset_indirect_string> (NULL);
    }

#if ENABLE_ASSERT_CHECKING
  {
    dw_die_ref die = comp_unit_die (), c;
    FOR_EACH_CHILD (die, c, gcc_assert (! c->die_mark));
  }
#endif
  resolve_addr (comp_unit_die ());
  move_marked_base_types ();

  /* Initialize sections and labels used for actual assembler output.  */
  init_sections_and_labels (false);

  /* Traverse the DIE's and add sibling attributes to those DIE's that
     have children.  */
  add_sibling_attributes (comp_unit_die ());
  limbo_die_node *node;
  for (node = cu_die_list; node; node = node->next)
    add_sibling_attributes (node->die);
  for (ctnode = comdat_type_list; ctnode != NULL; ctnode = ctnode->next)
    add_sibling_attributes (ctnode->root_die);

  /* When splitting DWARF info, we put some attributes in the
     skeleton compile_unit DIE that remains in the .o, while
     most attributes go in the DWO compile_unit_die.  */
  if (dwarf_split_debug_info)
    {
      limbo_die_node *cu;
      main_comp_unit_die = gen_compile_unit_die (NULL);
      if (dwarf_version >= 5)
	main_comp_unit_die->die_tag = DW_TAG_skeleton_unit;
      cu = limbo_die_list;
      gcc_assert (cu->die == main_comp_unit_die);
      limbo_die_list = limbo_die_list->next;
      cu->next = cu_die_list;
      cu_die_list = cu;
    }
  else
    main_comp_unit_die = comp_unit_die ();

  /* Output a terminator label for the .text section.  */
  switch_to_section (text_section);
  targetm.asm_out.internal_label (asm_out_file, TEXT_END_LABEL, 0);
  if (cold_text_section)
    {
      switch_to_section (cold_text_section);
      targetm.asm_out.internal_label (asm_out_file, COLD_END_LABEL, 0);
    }

  /* We can only use the low/high_pc attributes if all of the code was
     in .text.  */
  if (!have_multiple_function_sections 
      || (dwarf_version < 3 && dwarf_strict))
    {
      /* Don't add if the CU has no associated code.  */
      if (text_section_used)
        add_AT_low_high_pc (main_comp_unit_die, text_section_label,
                            text_end_label, true);
    }
  else
    {
      unsigned fde_idx;
      dw_fde_ref fde;
      bool range_list_added = false;

      if (text_section_used)
        add_ranges_by_labels (main_comp_unit_die, text_section_label,
                              text_end_label, &range_list_added, true);
      if (cold_text_section_used)
        add_ranges_by_labels (main_comp_unit_die, cold_text_section_label,
                              cold_end_label, &range_list_added, true);

      FOR_EACH_VEC_ELT (*fde_vec, fde_idx, fde)
	{
	  if (DECL_IGNORED_P (fde->decl))
	    continue;
	  if (!fde->in_std_section)
            add_ranges_by_labels (main_comp_unit_die, fde->dw_fde_begin,
                                  fde->dw_fde_end, &range_list_added,
                                  true);
	  if (fde->dw_fde_second_begin && !fde->second_in_std_section)
            add_ranges_by_labels (main_comp_unit_die, fde->dw_fde_second_begin,
                                  fde->dw_fde_second_end, &range_list_added,
                                  true);
	}

      if (range_list_added)
	{
	  /* We need to give .debug_loc and .debug_ranges an appropriate
	     "base address".  Use zero so that these addresses become
	     absolute.  Historically, we've emitted the unexpected
	     DW_AT_entry_pc instead of DW_AT_low_pc for this purpose.
	     Emit both to give time for other tools to adapt.  */
          add_AT_addr (main_comp_unit_die, DW_AT_low_pc, const0_rtx, true);
	  if (! dwarf_strict && dwarf_version < 4)
            add_AT_addr (main_comp_unit_die, DW_AT_entry_pc, const0_rtx, true);

	  add_ranges (NULL);
	}
    }

  /* AIX Assembler inserts the length, so adjust the reference to match the
     offset expected by debuggers.  */
  strcpy (dl_section_ref, debug_line_section_label);
  if (XCOFF_DEBUGGING_INFO)
    strcat (dl_section_ref, DWARF_INITIAL_LENGTH_SIZE_STR);

  if (debug_info_level >= DINFO_LEVEL_TERSE)
    add_AT_lineptr (main_comp_unit_die, DW_AT_stmt_list,
		    dl_section_ref);

  if (have_macinfo)
    add_AT_macptr (comp_unit_die (), DEBUG_MACRO_ATTRIBUTE,
		   macinfo_section_label);

  if (dwarf_split_debug_info)
    {
      if (have_location_lists)
	{
	  if (dwarf_version >= 5)
	    add_AT_loclistsptr (comp_unit_die (), DW_AT_loclists_base,
				loc_section_label);
	  /* optimize_location_lists calculates the size of the lists,
	     so index them first, and assign indices to the entries.
	     Although optimize_location_lists will remove entries from
	     the table, it only does so for duplicates, and therefore
	     only reduces ref_counts to 1.  */
	  index_location_lists (comp_unit_die ());
	}

      if (addr_index_table != NULL)
        {
          unsigned int index = 0;
          addr_index_table
	    ->traverse_noresize<unsigned int *, index_addr_table_entry>
	    (&index);
        }
    }

  loc_list_idx = 0;
  if (have_location_lists)
    {
      optimize_location_lists (comp_unit_die ());
      /* And finally assign indexes to the entries for -gsplit-dwarf.  */
      if (dwarf_version >= 5 && dwarf_split_debug_info)
	assign_location_list_indexes (comp_unit_die ());
    }

  save_macinfo_strings ();

  if (dwarf_split_debug_info)
    {
      unsigned int index = 0;

      /* Add attributes common to skeleton compile_units and
         type_units.  Because these attributes include strings, it
         must be done before freezing the string table.  Top-level
         skeleton die attrs are added when the skeleton type unit is
         created, so ensure it is created by this point.  */
      add_top_level_skeleton_die_attrs (main_comp_unit_die);
      debug_str_hash->traverse_noresize<unsigned int *, index_string> (&index);
    }

  /* Output all of the compilation units.  We put the main one last so that
     the offsets are available to output_pubnames.  */
  for (node = cu_die_list; node; node = node->next)
    output_comp_unit (node->die, 0, NULL);

  hash_table<comdat_type_hasher> comdat_type_table (100);
  for (ctnode = comdat_type_list; ctnode != NULL; ctnode = ctnode->next)
    {
      comdat_type_node **slot = comdat_type_table.find_slot (ctnode, INSERT);

      /* Don't output duplicate types.  */
      if (*slot != HTAB_EMPTY_ENTRY)
        continue;

      /* Add a pointer to the line table for the main compilation unit
         so that the debugger can make sense of DW_AT_decl_file
         attributes.  */
      if (debug_info_level >= DINFO_LEVEL_TERSE)
        add_AT_lineptr (ctnode->root_die, DW_AT_stmt_list,
                        (!dwarf_split_debug_info
                         ? dl_section_ref
                         : debug_skeleton_line_section_label));

      output_comdat_type_unit (ctnode);
      *slot = ctnode;
    }

  if (dwarf_split_debug_info)
    {
      int mark;
      struct md5_ctx ctx;

      if (dwarf_version >= 5 && !vec_safe_is_empty (ranges_table))
	index_rnglists ();

      /* Compute a checksum of the comp_unit to use as the dwo_id.  */
      md5_init_ctx (&ctx);
      mark = 0;
      die_checksum (comp_unit_die (), &ctx, &mark);
      unmark_all_dies (comp_unit_die ());
      md5_finish_ctx (&ctx, checksum);

      if (dwarf_version < 5)
	{
	  /* Use the first 8 bytes of the checksum as the dwo_id,
	     and add it to both comp-unit DIEs.  */
	  add_AT_data8 (main_comp_unit_die, DW_AT_GNU_dwo_id, checksum);
	  add_AT_data8 (comp_unit_die (), DW_AT_GNU_dwo_id, checksum);
	}

      /* Add the base offset of the ranges table to the skeleton
        comp-unit DIE.  */
      if (!vec_safe_is_empty (ranges_table))
	{
	  if (dwarf_version >= 5)
	    add_AT_lineptr (main_comp_unit_die, DW_AT_rnglists_base,
			    ranges_base_label);
	  else
	    add_AT_lineptr (main_comp_unit_die, DW_AT_GNU_ranges_base,
			    ranges_section_label);
	}

      switch_to_section (debug_addr_section);
      ASM_OUTPUT_LABEL (asm_out_file, debug_addr_section_label);
      output_addr_table ();
    }

  /* Output the main compilation unit if non-empty or if .debug_macinfo
     or .debug_macro will be emitted.  */
  output_comp_unit (comp_unit_die (), have_macinfo,
		    dwarf_split_debug_info ? checksum : NULL);

  if (dwarf_split_debug_info && info_section_emitted)
    output_skeleton_debug_sections (main_comp_unit_die, checksum);

  /* Output the abbreviation table.  */
  if (vec_safe_length (abbrev_die_table) != 1)
    {
      switch_to_section (debug_abbrev_section);
      ASM_OUTPUT_LABEL (asm_out_file, abbrev_section_label);
      output_abbrev_section ();
    }

  /* Output location list section if necessary.  */
  if (have_location_lists)
    {
      char l1[MAX_ARTIFICIAL_LABEL_BYTES];
      char l2[MAX_ARTIFICIAL_LABEL_BYTES];
      /* Output the location lists info.  */
      switch_to_section (debug_loc_section);
      if (dwarf_version >= 5)
	{
	  ASM_GENERATE_INTERNAL_LABEL (l1, DEBUG_LOC_SECTION_LABEL, 1);
	  ASM_GENERATE_INTERNAL_LABEL (l2, DEBUG_LOC_SECTION_LABEL, 2);
	  if (DWARF_INITIAL_LENGTH_SIZE - DWARF_OFFSET_SIZE == 4)
	    dw2_asm_output_data (4, 0xffffffff,
				 "Initial length escape value indicating "
				 "64-bit DWARF extension");
	  dw2_asm_output_delta (DWARF_OFFSET_SIZE, l2, l1,
			    "Length of Location Lists");
	  ASM_OUTPUT_LABEL (asm_out_file, l1);
	  dw2_asm_output_data (2, dwarf_version, "DWARF Version");
	  dw2_asm_output_data (1, DWARF2_ADDR_SIZE, "Address Size");
	  dw2_asm_output_data (1, 0, "Segment Size");
	  dw2_asm_output_data (4, dwarf_split_debug_info ? loc_list_idx : 0,
			       "Offset Entry Count");
	}
      ASM_OUTPUT_LABEL (asm_out_file, loc_section_label);
      if (dwarf_version >= 5 && dwarf_split_debug_info)
	{
	  unsigned int save_loc_list_idx = loc_list_idx;
	  loc_list_idx = 0;
	  output_loclists_offsets (comp_unit_die ());
	  gcc_assert (save_loc_list_idx == loc_list_idx);
	}
      output_location_lists (comp_unit_die ());
      if (dwarf_version >= 5)
	ASM_OUTPUT_LABEL (asm_out_file, l2);
    }

  output_pubtables ();

  /* Output the address range information if a CU (.debug_info section)
     was emitted.  We output an empty table even if we had no functions
     to put in it.  This because the consumer has no way to tell the
     difference between an empty table that we omitted and failure to
     generate a table that would have contained data.  */
  if (info_section_emitted)
    {
      switch_to_section (debug_aranges_section);
      output_aranges ();
    }

  /* Output ranges section if necessary.  */
  if (!vec_safe_is_empty (ranges_table))
    {
      if (dwarf_version >= 5)
	output_rnglists ();
      else
	output_ranges ();
    }

  /* Have to end the macro section.  */
  if (have_macinfo)
    {
      switch_to_section (debug_macinfo_section);
      ASM_OUTPUT_LABEL (asm_out_file, macinfo_section_label);
      output_macinfo (!dwarf_split_debug_info ? debug_line_section_label
		      : debug_skeleton_line_section_label, false);
      dw2_asm_output_data (1, 0, "End compilation unit");
    }

  /* Output the source line correspondence table.  We must do this
     even if there is no line information.  Otherwise, on an empty
     translation unit, we will generate a present, but empty,
     .debug_info section.  IRIX 6.5 `nm' will then complain when
     examining the file.  This is done late so that any filenames
     used by the debug_info section are marked as 'used'.  */
  switch_to_section (debug_line_section);
  ASM_OUTPUT_LABEL (asm_out_file, debug_line_section_label);
  if (! DWARF2_ASM_LINE_DEBUG_INFO)
    output_line_info (false);

  if (dwarf_split_debug_info && info_section_emitted)
    {
      switch_to_section (debug_skeleton_line_section);
      ASM_OUTPUT_LABEL (asm_out_file, debug_skeleton_line_section_label);
      output_line_info (true);
    }

  /* If we emitted any indirect strings, output the string table too.  */
  if (debug_str_hash || skeleton_debug_str_hash)
    output_indirect_strings ();
  if (debug_line_str_hash)
    {
      switch_to_section (debug_line_str_section);
      const enum dwarf_form form = DW_FORM_line_strp;
      debug_line_str_hash->traverse<enum dwarf_form,
				    output_indirect_string> (form);
    }
}

/* Returns a hash value for X (which really is a variable_value_struct).  */

inline hashval_t
variable_value_hasher::hash (variable_value_struct *x)
{
  return (hashval_t) x->decl_id;
}

/* Return nonzero if decl_id of variable_value_struct X is the same as
   UID of decl Y.  */

inline bool
variable_value_hasher::equal (variable_value_struct *x, tree y)
{
  return x->decl_id == DECL_UID (y);
}

/* Helper function for resolve_variable_value, handle
   DW_OP_GNU_variable_value in one location expression.
   Return true if exprloc has been changed into loclist.  */

static bool
resolve_variable_value_in_expr (dw_attr_node *a, dw_loc_descr_ref loc)
{
  dw_loc_descr_ref next;
  for (dw_loc_descr_ref prev = NULL; loc; prev = loc, loc = next)
    {
      next = loc->dw_loc_next;
      if (loc->dw_loc_opc != DW_OP_GNU_variable_value
	  || loc->dw_loc_oprnd1.val_class != dw_val_class_decl_ref)
	continue;

      tree decl = loc->dw_loc_oprnd1.v.val_decl_ref;
      if (DECL_CONTEXT (decl) != current_function_decl)
	continue;

      dw_die_ref ref = lookup_decl_die (decl);
      if (ref)
	{
	  loc->dw_loc_oprnd1.val_class = dw_val_class_die_ref;
	  loc->dw_loc_oprnd1.v.val_die_ref.die = ref;
	  loc->dw_loc_oprnd1.v.val_die_ref.external = 0;
	  continue;
	}
      dw_loc_list_ref l = loc_list_from_tree (decl, 0, NULL);
      if (l == NULL)
	continue;
      if (l->dw_loc_next)
	{
	  if (AT_class (a) != dw_val_class_loc)
	    continue;
	  switch (a->dw_attr)
	    {
	    /* Following attributes allow both exprloc and loclist
	       classes, so we can change them into a loclist.  */
	    case DW_AT_location:
	    case DW_AT_string_length:
	    case DW_AT_return_addr:
	    case DW_AT_data_member_location:
	    case DW_AT_frame_base:
	    case DW_AT_segment:
	    case DW_AT_static_link:
	    case DW_AT_use_location:
	    case DW_AT_vtable_elem_location:
	      if (prev)
		{
		  prev->dw_loc_next = NULL;
		  prepend_loc_descr_to_each (l, AT_loc (a));
		}
	      if (next)
		add_loc_descr_to_each (l, next);
	      a->dw_attr_val.val_class = dw_val_class_loc_list;
	      a->dw_attr_val.val_entry = NULL;
	      a->dw_attr_val.v.val_loc_list = l;
	      have_location_lists = true;
	      return true;
	    /* Following attributes allow both exprloc and reference,
	       so if the whole expression is DW_OP_GNU_variable_value alone
	       we could transform it into reference.  */
	    case DW_AT_byte_size:
	    case DW_AT_bit_size:
	    case DW_AT_lower_bound:
	    case DW_AT_upper_bound:
	    case DW_AT_bit_stride:
	    case DW_AT_count:
	    case DW_AT_allocated:
	    case DW_AT_associated:
	    case DW_AT_byte_stride:
	      if (prev == NULL && next == NULL)
		break;
	      /* FALLTHRU */
	    default:
	      if (dwarf_strict)
		continue;
	      break;
	    }
	  /* Create DW_TAG_variable that we can refer to.  */
	  gen_decl_die (decl, NULL_TREE, NULL,
			lookup_decl_die (current_function_decl));
	  ref = lookup_decl_die (decl);
	  if (ref)
	    {
	      loc->dw_loc_oprnd1.val_class = dw_val_class_die_ref;
	      loc->dw_loc_oprnd1.v.val_die_ref.die = ref;
	      loc->dw_loc_oprnd1.v.val_die_ref.external = 0;
	    }
	  continue;
	}
      if (prev)
	{
	  prev->dw_loc_next = l->expr;
	  add_loc_descr (&prev->dw_loc_next, next);
	  free_loc_descr (loc, NULL);
	  next = prev->dw_loc_next;
	}
      else
	{
	  memcpy (loc, l->expr, sizeof (dw_loc_descr_node));
	  add_loc_descr (&loc, next);
	  next = loc;
	}
      loc = prev;
    }
  return false;
}

/* Attempt to resolve DW_OP_GNU_variable_value using loc_list_from_tree.  */

static void
resolve_variable_value (dw_die_ref die)
{
  dw_attr_node *a;
  dw_loc_list_ref loc;
  unsigned ix;

  FOR_EACH_VEC_SAFE_ELT (die->die_attr, ix, a)
    switch (AT_class (a))
      {
      case dw_val_class_loc:
	if (!resolve_variable_value_in_expr (a, AT_loc (a)))
	  break;
	/* FALLTHRU */
      case dw_val_class_loc_list:
	loc = AT_loc_list (a);
	gcc_assert (loc);
	for (; loc; loc = loc->dw_loc_next)
	  resolve_variable_value_in_expr (a, loc->expr);
	break;
      default:
	break;
      }
}

/* Attempt to optimize DW_OP_GNU_variable_value refering to
   temporaries in the current function.  */

static void
resolve_variable_values (void)
{
  if (!variable_value_hash || !current_function_decl)
    return;

  struct variable_value_struct *node
    = variable_value_hash->find_with_hash (current_function_decl,
					   DECL_UID (current_function_decl));

  if (node == NULL)
    return;

  unsigned int i;
  dw_die_ref die;
  FOR_EACH_VEC_SAFE_ELT (node->dies, i, die)
    resolve_variable_value (die);
}

/* Helper function for note_variable_value, handle one location
   expression.  */

static void
note_variable_value_in_expr (dw_die_ref die, dw_loc_descr_ref loc)
{
  for (; loc; loc = loc->dw_loc_next)
    if (loc->dw_loc_opc == DW_OP_GNU_variable_value
	&& loc->dw_loc_oprnd1.val_class == dw_val_class_decl_ref)
      {
	tree decl = loc->dw_loc_oprnd1.v.val_decl_ref;
	dw_die_ref ref = lookup_decl_die (decl);
	if (! ref && (flag_generate_lto || flag_generate_offload))
	  {
	    /* ???  This is somewhat a hack because we do not create DIEs
	       for variables not in BLOCK trees early but when generating
	       early LTO output we need the dw_val_class_decl_ref to be
	       fully resolved.  For fat LTO objects we'd also like to
	       undo this after LTO dwarf output.  */
	    gcc_assert (DECL_CONTEXT (decl));
	    dw_die_ref ctx = lookup_decl_die (DECL_CONTEXT (decl));
	    gcc_assert (ctx != NULL);
	    gen_decl_die (decl, NULL_TREE, NULL, ctx);
	    ref = lookup_decl_die (decl);
	    gcc_assert (ref != NULL);
	  }
	if (ref)
	  {
	    loc->dw_loc_oprnd1.val_class = dw_val_class_die_ref;
	    loc->dw_loc_oprnd1.v.val_die_ref.die = ref;
	    loc->dw_loc_oprnd1.v.val_die_ref.external = 0;
	    continue;
	  }
	if (VAR_P (decl)
	    && DECL_CONTEXT (decl)
	    && TREE_CODE (DECL_CONTEXT (decl)) == FUNCTION_DECL
	    && lookup_decl_die (DECL_CONTEXT (decl)))
	  {
	    if (!variable_value_hash)
	      variable_value_hash
		= hash_table<variable_value_hasher>::create_ggc (10);

	    tree fndecl = DECL_CONTEXT (decl);
	    struct variable_value_struct *node;
	    struct variable_value_struct **slot
	      = variable_value_hash->find_slot_with_hash (fndecl,
							  DECL_UID (fndecl),
							  INSERT);
	    if (*slot == NULL)
	      {
		node = ggc_cleared_alloc<variable_value_struct> ();
		node->decl_id = DECL_UID (fndecl);
		*slot = node;
	      }
	    else
	      node = *slot;

	    vec_safe_push (node->dies, die);
	  }
      }
}

/* Walk the tree DIE and note DIEs with DW_OP_GNU_variable_value still
   with dw_val_class_decl_ref operand.  */

static void
note_variable_value (dw_die_ref die)
{
  dw_die_ref c;
  dw_attr_node *a;
  dw_loc_list_ref loc;
  unsigned ix;

  FOR_EACH_VEC_SAFE_ELT (die->die_attr, ix, a)
    switch (AT_class (a))
      {
      case dw_val_class_loc_list:
	loc = AT_loc_list (a);
	gcc_assert (loc);
	if (!loc->noted_variable_value)
	  {
	    loc->noted_variable_value = 1;
	    for (; loc; loc = loc->dw_loc_next)
	      note_variable_value_in_expr (die, loc->expr);
	  }
	break;
      case dw_val_class_loc:
	note_variable_value_in_expr (die, AT_loc (a));
	break;
      default:
	break;
      }

  /* Mark children.  */
  FOR_EACH_CHILD (die, c, note_variable_value (c));
}

/* Perform any cleanups needed after the early debug generation pass
   has run.  */

static void
dwarf2out_early_finish (const char *filename)
{
  set_early_dwarf s;

  /* PCH might result in DW_AT_producer string being restored from the
     header compilation, so always fill it with empty string initially
     and overwrite only here.  */
  dw_attr_node *producer = get_AT (comp_unit_die (), DW_AT_producer);
  producer_string = gen_producer_string ();
  producer->dw_attr_val.v.val_str->refcount--;
  producer->dw_attr_val.v.val_str = find_AT_string (producer_string);

  /* Add the name for the main input file now.  We delayed this from
     dwarf2out_init to avoid complications with PCH.  */
  add_name_attribute (comp_unit_die (), remap_debug_filename (filename));
  add_comp_dir_attribute (comp_unit_die ());

  /* When emitting DWARF5 .debug_line_str, move DW_AT_name and
     DW_AT_comp_dir into .debug_line_str section.  */
  if (!DWARF2_ASM_LINE_DEBUG_INFO
      && dwarf_version >= 5
      && DWARF5_USE_DEBUG_LINE_STR)
    {
      for (int i = 0; i < 2; i++)
	{
	  dw_attr_node *a = get_AT (comp_unit_die (),
				    i ? DW_AT_comp_dir : DW_AT_name);
	  if (a == NULL
	      || AT_class (a) != dw_val_class_str
	      || strlen (AT_string (a)) + 1 <= DWARF_OFFSET_SIZE)
	    continue;

	  if (! debug_line_str_hash)
	    debug_line_str_hash
	      = hash_table<indirect_string_hasher>::create_ggc (10);

	  struct indirect_string_node *node
	    = find_AT_string_in_table (AT_string (a), debug_line_str_hash);
	  set_indirect_string (node);
	  node->form = DW_FORM_line_strp;
	  a->dw_attr_val.v.val_str->refcount--;
	  a->dw_attr_val.v.val_str = node;
	}
    }

  /* With LTO early dwarf was really finished at compile-time, so make
     sure to adjust the phase after annotating the LTRANS CU DIE.  */
  if (in_lto_p)
    {
      early_dwarf_finished = true;
      return;
    }

  /* Walk through the list of incomplete types again, trying once more to
     emit full debugging info for them.  */
  retry_incomplete_types ();

  /* The point here is to flush out the limbo list so that it is empty
     and we don't need to stream it for LTO.  */
  flush_limbo_die_list ();

  gen_scheduled_generic_parms_dies ();
  gen_remaining_tmpl_value_param_die_attribute ();

  /* Add DW_AT_linkage_name for all deferred DIEs.  */
  for (limbo_die_node *node = deferred_asm_name; node; node = node->next)
    {
      tree decl = node->created_for;
      if (DECL_ASSEMBLER_NAME (decl) != DECL_NAME (decl)
	  /* A missing DECL_ASSEMBLER_NAME can be a constant DIE that
	     ended up in deferred_asm_name before we knew it was
	     constant and never written to disk.  */
	  && DECL_ASSEMBLER_NAME (decl))
	{
	  add_linkage_attr (node->die, decl);
	  move_linkage_attr (node->die);
	}
    }
  deferred_asm_name = NULL;

  if (flag_eliminate_unused_debug_types)
    prune_unused_types ();

  /* Generate separate COMDAT sections for type DIEs. */
  if (use_debug_types)
    {
      break_out_comdat_types (comp_unit_die ());

      /* Each new type_unit DIE was added to the limbo die list when created.
         Since these have all been added to comdat_type_list, clear the
         limbo die list.  */
      limbo_die_list = NULL;

      /* For each new comdat type unit, copy declarations for incomplete
         types to make the new unit self-contained (i.e., no direct
         references to the main compile unit).  */
      for (comdat_type_node *ctnode = comdat_type_list;
	   ctnode != NULL; ctnode = ctnode->next)
        copy_decls_for_unworthy_types (ctnode->root_die);
      copy_decls_for_unworthy_types (comp_unit_die ());

      /* In the process of copying declarations from one unit to another,
         we may have left some declarations behind that are no longer
         referenced.  Prune them.  */
      prune_unused_types ();
    }

  /* Traverse the DIE's and note DIEs with DW_OP_GNU_variable_value still
     with dw_val_class_decl_ref operand.  */
  note_variable_value (comp_unit_die ());
  for (limbo_die_node *node = cu_die_list; node; node = node->next)
    note_variable_value (node->die);
  for (comdat_type_node *ctnode = comdat_type_list; ctnode != NULL;
       ctnode = ctnode->next)
    note_variable_value (ctnode->root_die);
  for (limbo_die_node *node = limbo_die_list; node; node = node->next)
    note_variable_value (node->die);

  /* The AT_pubnames attribute needs to go in all skeleton dies, including
     both the main_cu and all skeleton TUs.  Making this call unconditional
     would end up either adding a second copy of the AT_pubnames attribute, or
     requiring a special case in add_top_level_skeleton_die_attrs.  */
  if (!dwarf_split_debug_info)
    add_AT_pubnames (comp_unit_die ());

  /* The early debug phase is now finished.  */
  early_dwarf_finished = true;

  /* Do not generate DWARF assembler now when not producing LTO bytecode.  */
  if (!flag_generate_lto && !flag_generate_offload)
    return;

  /* Now as we are going to output for LTO initialize sections and labels
     to the LTO variants.  We don't need a random-seed postfix as other
     LTO sections as linking the LTO debug sections into one in a partial
     link is fine.  */
  init_sections_and_labels (true);

  /* The output below is modeled after dwarf2out_finish with all
     location related output removed and some LTO specific changes.
     Some refactoring might make both smaller and easier to match up.  */

  /* Traverse the DIE's and add add sibling attributes to those DIE's
     that have children.  */
  add_sibling_attributes (comp_unit_die ());
  for (limbo_die_node *node = limbo_die_list; node; node = node->next)
    add_sibling_attributes (node->die);
  for (comdat_type_node *ctnode = comdat_type_list;
       ctnode != NULL; ctnode = ctnode->next)
    add_sibling_attributes (ctnode->root_die);

  if (have_macinfo)
    add_AT_macptr (comp_unit_die (), DEBUG_MACRO_ATTRIBUTE,
		   macinfo_section_label);

  save_macinfo_strings ();

  /* Output all of the compilation units.  We put the main one last so that
     the offsets are available to output_pubnames.  */
  for (limbo_die_node *node = limbo_die_list; node; node = node->next)
    output_comp_unit (node->die, 0, NULL);

  hash_table<comdat_type_hasher> comdat_type_table (100);
  for (comdat_type_node *ctnode = comdat_type_list;
       ctnode != NULL; ctnode = ctnode->next)
    {
      comdat_type_node **slot = comdat_type_table.find_slot (ctnode, INSERT);

      /* Don't output duplicate types.  */
      if (*slot != HTAB_EMPTY_ENTRY)
        continue;

      /* Add a pointer to the line table for the main compilation unit
         so that the debugger can make sense of DW_AT_decl_file
         attributes.  */
      if (debug_info_level >= DINFO_LEVEL_TERSE)
        add_AT_lineptr (ctnode->root_die, DW_AT_stmt_list,
                        (!dwarf_split_debug_info
                         ? debug_line_section_label
                         : debug_skeleton_line_section_label));

      output_comdat_type_unit (ctnode);
      *slot = ctnode;
    }

  /* Stick a unique symbol to the main debuginfo section.  */
  compute_comp_unit_symbol (comp_unit_die ());

  /* Output the main compilation unit.  We always need it if only for
     the CU symbol.  */
  output_comp_unit (comp_unit_die (), true, NULL);

  /* Output the abbreviation table.  */
  if (vec_safe_length (abbrev_die_table) != 1)
    {
      switch_to_section (debug_abbrev_section);
      ASM_OUTPUT_LABEL (asm_out_file, abbrev_section_label);
      output_abbrev_section ();
    }

  /* Have to end the macro section.  */
  if (have_macinfo)
    {
      /* We have to save macinfo state if we need to output it again
	 for the FAT part of the object.  */
      vec<macinfo_entry, va_gc> *saved_macinfo_table = macinfo_table;
      if (flag_fat_lto_objects)
	macinfo_table = macinfo_table->copy ();

      switch_to_section (debug_macinfo_section);
      ASM_OUTPUT_LABEL (asm_out_file, macinfo_section_label);
      output_macinfo (debug_skeleton_line_section_label, true);
      dw2_asm_output_data (1, 0, "End compilation unit");

      /* Emit a skeleton debug_line section.  */
      switch_to_section (debug_skeleton_line_section);
      ASM_OUTPUT_LABEL (asm_out_file, debug_skeleton_line_section_label);
      output_line_info (true);

      if (flag_fat_lto_objects)
	{
	  vec_free (macinfo_table);
	  macinfo_table = saved_macinfo_table;
	}
    }


  /* If we emitted any indirect strings, output the string table too.  */
  if (debug_str_hash || skeleton_debug_str_hash)
    output_indirect_strings ();

  /* Switch back to the text section.  */
  switch_to_section (text_section);
}

/* Reset all state within dwarf2out.c so that we can rerun the compiler
   within the same process.  For use by toplev::finalize.  */

void
dwarf2out_c_finalize (void)
{
  last_var_location_insn = NULL;
  cached_next_real_insn = NULL;
  used_rtx_array = NULL;
  incomplete_types = NULL;
  decl_scope_table = NULL;
  debug_info_section = NULL;
  debug_skeleton_info_section = NULL;
  debug_abbrev_section = NULL;
  debug_skeleton_abbrev_section = NULL;
  debug_aranges_section = NULL;
  debug_addr_section = NULL;
  debug_macinfo_section = NULL;
  debug_line_section = NULL;
  debug_skeleton_line_section = NULL;
  debug_loc_section = NULL;
  debug_pubnames_section = NULL;
  debug_pubtypes_section = NULL;
  debug_str_section = NULL;
  debug_line_str_section = NULL;
  debug_str_dwo_section = NULL;
  debug_str_offsets_section = NULL;
  debug_ranges_section = NULL;
  debug_frame_section = NULL;
  fde_vec = NULL;
  debug_str_hash = NULL;
  debug_line_str_hash = NULL;
  skeleton_debug_str_hash = NULL;
  dw2_string_counter = 0;
  have_multiple_function_sections = false;
  text_section_used = false;
  cold_text_section_used = false;
  cold_text_section = NULL;
  current_unit_personality = NULL;

  early_dwarf = false;
  early_dwarf_finished = false;

  next_die_offset = 0;
  single_comp_unit_die = NULL;
  comdat_type_list = NULL;
  limbo_die_list = NULL;
  file_table = NULL;
  decl_die_table = NULL;
  common_block_die_table = NULL;
  decl_loc_table = NULL;
  call_arg_locations = NULL;
  call_arg_loc_last = NULL;
  call_site_count = -1;
  tail_call_site_count = -1;
  cached_dw_loc_list_table = NULL;
  abbrev_die_table = NULL;
  delete dwarf_proc_stack_usage_map;
  dwarf_proc_stack_usage_map = NULL;
  line_info_label_num = 0;
  cur_line_info_table = NULL;
  text_section_line_info = NULL;
  cold_text_section_line_info = NULL;
  separate_line_info = NULL;
  info_section_emitted = false;
  pubname_table = NULL;
  pubtype_table = NULL;
  macinfo_table = NULL;
  ranges_table = NULL;
  ranges_by_label = NULL;
  rnglist_idx = 0;
  have_location_lists = false;
  loclabel_num = 0;
  poc_label_num = 0;
  last_emitted_file = NULL;
  label_num = 0;
  tmpl_value_parm_die_table = NULL;
  generic_type_instances = NULL;
  frame_pointer_fb_offset = 0;
  frame_pointer_fb_offset_valid = false;
  base_types.release ();
  XDELETEVEC (producer_string);
  producer_string = NULL;
}

#include "gt-dwarf2out.h"
