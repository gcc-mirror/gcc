/* Output Dwarf2 format symbol table information from the GNU C compiler.
   Copyright (C) 1992, 1993, 1995, 1996 Free Software Foundation, Inc.
   Contributed by Gary Funck (gary@intrepid.com).  Derived from the
   DWARF 1 implementation written by Ron Guilmette (rfg@monkeys.com).

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "config.h"

#if defined(DWARF_DEBUGGING_INFO) && defined(DWARF_VERSION) \
    && DWARF_VERSION == 2
#include <stdio.h>
#include "dwarf2.h"
#include "tree.h"
#include "flags.h"
#include "rtl.h"
#include "hard-reg-set.h"
#include "regs.h"
#include "insn-config.h"
#include "reload.h"
#include "output.h"
#include "defaults.h"

/* #define NDEBUG 1 */
#include "assert.h"
#if defined(DWARF_TIMESTAMPS)
#if defined(POSIX)
#include <time.h>
#else /* !defined(POSIX) */
#include <sys/types.h>
#if defined(__STDC__)
extern time_t time (time_t *);
#else /* !defined(__STDC__) */
extern time_t time ();
#endif /* !defined(__STDC__) */
#endif /* !defined(POSIX) */
#endif /* defined(DWARF_TIMESTAMPS) */

extern char *getpwd ();
extern char *index ();
extern char *rindex ();

/* IMPORTANT NOTE: Please see the file README.DWARF for important details
   regarding the GNU implementation of DWARF.  */

/* NOTE: In the comments in this file, many references are made to
   "Debugging Information Entries".  This term is abbreviated as `DIE'
   throughout the remainder of this file.  */

/* NOTE: The implementation of C++ support is unfinished.  */

#if defined(__GNUC__) && (NDEBUG == 1)
#define inline static inline
#else
#define inline static
#endif


/* An internal representation of the DWARF output is built, and then
   walked to generate the DWARF debugging info.  The walk of the internal
   representation is done after the entire program has been compiled.
   The types below are used to describe the internal representation.  */

/* Each DIE may have a series of attribute/value pairs.  Values
   can take on several forms.  The forms that are used in this
   impelementation are listed below.  */
typedef enum
  {
    dw_val_class_addr,
    dw_val_class_loc,
    dw_val_class_const,
    dw_val_class_unsigned_const,
    dw_val_class_double_const,
    dw_val_class_flag,
    dw_val_class_die_ref,
    dw_val_class_fde_ref,
    dw_val_class_lbl_id,
    dw_val_class_section_offset,
    dw_val_class_str
  }
dw_val_class;

/* Various DIE's use offsets relative to the beginning of the
   .debug_info section to refer to each other.  */
typedef long int dw_offset;

/* Define typedefs here to avoid circular dependencies.  */
typedef struct die_struct *dw_die_ref;
typedef struct dw_attr_struct *dw_attr_ref;
typedef struct dw_val_struct *dw_val_ref;
typedef struct dw_line_info_struct *dw_line_info_ref;
typedef struct dw_loc_descr_struct *dw_loc_descr_ref;
typedef struct dw_cfi_struct *dw_cfi_ref;
typedef struct dw_fde_struct *dw_fde_ref;
typedef union  dw_cfi_oprnd_struct *dw_cfi_oprnd_ref;
typedef struct backchain *backchain_ref;

/* Describe a double word constant value.  */
typedef struct dw_double_const_struct
  {
    unsigned long dw_dbl_hi;
    unsigned long dw_dbl_low;
  }
dw_dbl_const;

/* Each entry in the line_info_table maintains the file and
   line nuber associated with the label generated for that
   entry.  The label gives the PC value associated with
   the line number entry.  */
typedef struct dw_line_info_struct
  {
    unsigned long dw_file_num;
    unsigned long dw_line_num;
  }
dw_line_info_entry;

/* The dw_val_node describes an attibute's value, as it is
   represnted internally.  */
typedef struct dw_val_struct
  {
    dw_val_class val_class;
    union
      {
	char *val_addr;
	dw_loc_descr_ref val_loc;
	long int val_int;
	long unsigned val_unsigned;
	dw_dbl_const val_dbl_const;
	dw_die_ref val_die_ref;
	unsigned val_fde_index;
	char *val_str;
	char *val_lbl_id;
	char *val_section;
	unsigned char val_flag;
      }
    v;
  }
dw_val_node;

/* Locations in memory are described using a sequence of stack machine
   operations.  */
typedef struct dw_loc_descr_struct
  {
    dw_loc_descr_ref dw_loc_next;
    enum dwarf_location_atom dw_loc_opc;
    dw_val_node dw_loc_oprnd1;
    dw_val_node dw_loc_oprnd2;
  }
dw_loc_descr_node;

/* Each DIE attribute has a field specifying the attribute kind,
   a link to the next attribute in the chain, and an attribute value.
   Attributes are typically linked below the DIE they modify.  */
typedef struct dw_attr_struct
  {
    enum dwarf_attribute dw_attr;
    dw_attr_ref dw_attr_next;
    dw_val_node dw_attr_val;
  }
dw_attr_node;

/* Call frames are described using a sequence of Call Frame
   Information instructions.  The register number, offset
   and address fields are provided as possible operands;
   their use is selected by the opcode field.  */
typedef union dw_cfi_oprnd_struct
  {
    unsigned long dw_cfi_reg_num;
    long int dw_cfi_offset;
    char *dw_cfi_addr;
  }
dw_cfi_oprnd;

typedef struct dw_cfi_struct
  {
    dw_cfi_ref dw_cfi_next;
    enum dwarf_call_frame_info dw_cfi_opc;
    dw_cfi_oprnd dw_cfi_oprnd1;
    dw_cfi_oprnd dw_cfi_oprnd2;
  }
dw_cfi_node;

/* All call frame descriptions (FDE's) in the GCC generated DWARF
   refer to a signle Common Information Entry (CIE), defined at
   the beginning of the .debug_frame section.  This used of a single
   CIE obviates the need to keep track of multiple CIE's
   in the DWARF generation routines below.  */
typedef struct dw_fde_struct
  {
    unsigned long dw_fde_offset;
    char *dw_fde_begin;
    char *dw_fde_end_prolog;
    char *dw_fde_begin_epilogue;
    char *dw_fde_end;
    dw_cfi_ref dw_fde_cfi;
  }
dw_fde_node;

/* The Debugging Information Entry (DIE) structure */
typedef struct die_struct
  {
    enum dwarf_tag die_tag;
    dw_attr_ref die_attr;
    dw_attr_ref die_attr_last;
    dw_die_ref die_parent;
    dw_die_ref die_child;
    dw_die_ref die_child_last;
    dw_die_ref die_sib;
    dw_offset die_offset;
    unsigned long die_abbrev;
  }
die_node;

/* The structure for backchaining support, when structure tags are declared
   before they are defined.  */

typedef struct backchain
  {
    tree type;
    dw_die_ref placeholder;
    backchain_ref next;
  }
backchain_t;

/* How to start an assembler comment.  */
#ifndef ASM_COMMENT_START
#define ASM_COMMENT_START ";#"
#endif

/* Define a macro which returns non-zero for any tagged type which is used
   (directly or indirectly) in the specification of either some function's
   return type or some formal parameter of some function. We use this macro
   when we are operating in "terse" mode to help us know what tagged types
   have to be represented in Dwarf (even in terse mode) and which ones don't.
   A flag bit with this meaning really should be a part of the normal GCC
   ..._TYPE nodes, but at the moment, there is no such bit defined for these
   nodes.  For now, we have to just fake it.  It it safe for us to simply
   return zero for all complete tagged types (which will get forced out
   anyway if they were used in the specification of some formal or return
   type) and non-zero for all incomplete tagged types.  */
#define TYPE_USED_FOR_FUNCTION(tagged_type) (TYPE_SIZE (tagged_type) == 0)

/* Information concerning the compilation unit's programming
   language, and compiler version.  */
extern int flag_traditional;
extern char *version_string;
extern char *language_string;

/* Maximum size (in bytes) of an artificially generated label.   */
#define MAX_ARTIFICIAL_LABEL_BYTES	30

/* Make sure we know the sizes of the various types dwarf can describe. These
   are only defaults.  If the sizes are different for your target, you should
   override these values by defining the appropriate symbols in your tm.h
   file.  */
#ifndef CHAR_TYPE_SIZE
#define CHAR_TYPE_SIZE BITS_PER_UNIT
#endif
#ifndef SHORT_TYPE_SIZE
#define SHORT_TYPE_SIZE (BITS_PER_UNIT * 2)
#endif
#ifndef INT_TYPE_SIZE
#define INT_TYPE_SIZE BITS_PER_WORD
#endif
#ifndef LONG_TYPE_SIZE
#define LONG_TYPE_SIZE BITS_PER_WORD
#endif
#ifndef LONG_LONG_TYPE_SIZE
#define LONG_LONG_TYPE_SIZE (BITS_PER_WORD * 2)
#endif
#ifndef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE INT_TYPE_SIZE
#endif
#ifndef WCHAR_UNSIGNED
#define WCHAR_UNSIGNED 0
#endif
#ifndef FLOAT_TYPE_SIZE
#define FLOAT_TYPE_SIZE BITS_PER_WORD
#endif
#ifndef DOUBLE_TYPE_SIZE
#define DOUBLE_TYPE_SIZE (BITS_PER_WORD * 2)
#endif
#ifndef LONG_DOUBLE_TYPE_SIZE
#define LONG_DOUBLE_TYPE_SIZE (BITS_PER_WORD * 2)
#endif
#ifndef PTR_SIZE
#define PTR_SIZE (POINTER_SIZE / 8)
#endif

/* Fixed size portion of the DWARF compilation unit header.  */
#define DWARF_COMPILE_UNIT_HEADER_SIZE 11

/* Fixed size portion of debugging line information prolog.  */
#define DWARF_LINE_PROLOG_HEADER_SIZE 5

/* Fixed size portion of public names info.  */
#define DWARF_PUBNAMES_HEADER_SIZE 10

/* Fixed size portion of the address range info.  */
#define DWARF_ARANGES_HEADER_SIZE 12

/* Fixed size portion of the Common Information Entry (including
   the length field).  */
#define DWARF_CIE_HEADER_SIZE 16

/* Fixed size of the Common Information Entry in the call frame
   information (.debug_frame) section rounded up to an 8 byte boundary.  */
#define DWARF_CIE_SIZE ((DWARF_CIE_HEADER_SIZE + 7) & ~7)

/* Offsets recorded in opcodes are a multiple of this alignment factor.  */
#define DWARF_CIE_DATA_ALIGNMENT -4

/* Fixed size portion of the FDE.  */
#define DWARF_FDE_HEADER_SIZE (4 + 4 + (2 * PTR_SIZE))

/* Define the architecture-dependent minimum instruction length (in bytes).
   In this implementation of DWARF, this field is used for information
   purposes only.  Since GCC generates assembly language, we have
   no a priori knowledge of how many instruction bytes are generated
   for each source line, and therefore can use only the  DW_LNE_set_address
   and DW_LNS_fixed_advance_pc line information commands.  */
#ifndef DWARF_LINE_MIN_INSTR_LENGTH
#define DWARF_LINE_MIN_INSTR_LENGTH 4
#endif

/* Minimum line offset in a special line info. opcode.
   This value was chosen to give a reasonable range of values.  */
#define DWARF_LINE_BASE  -10

/* First special line opcde - leave room for the standard opcodes.  */
#define DWARF_LINE_OPCODE_BASE  10

/* Range of line offsets in a special line info. opcode.  */
#define DWARF_LINE_RANGE  (254-DWARF_LINE_OPCODE_BASE+1)

/* Flag that indicates the initial value of the is_stmt_start flag.
   In the present implementation, we do not mark any lines as
   the beginning of a source statement, because that information
   is not made available by the GCC front-end.  */
#define	DWARF_LINE_DEFAULT_IS_STMT_START 1

/* This location is used by calc_die_sizes() to keep track
   the offset of each DIE within the .debug_info section.  */
static unsigned long next_die_offset;

/* This location is used by calc_fde_sizes() to keep track
   the offset of each FDE within the .debug_frame section.  */
static unsigned long next_fde_offset;

/* Record the root of the DIE's built for the current compilation unit.  */
dw_die_ref comp_unit_die;

/* Pointer to an array of filenames referenced by this compilation unit.  */
static char **file_table;

/* Total number of entries in the table (i.e. array) pointed to by
   `file_table'.  This is the *total* and includes both used and unused
   slots.  */
static unsigned file_table_allocated;

/* Number of entries in the file_table which are actually in use.  */
static unsigned file_table_in_use;

/* Size (in elements) of increments by which we may expand the filename
   table.  */
#define FILE_TABLE_INCREMENT 64

/* Local pointer to the name of the main input file.  Initialized in
   dwarfout_init.  */
static char *primary_filename;

/* For Dwarf output, we must assign lexical-blocks id numbers in the order in
   which their beginnings are encountered. We output Dwarf debugging info
   that refers to the beginnings and ends of the ranges of code for each
   lexical block.  The labels themselves are generated in final.c, which
   assigns numbers to the blocks in the same way.  */
static unsigned next_block_number = 2;

/* Non-zero if we are performing the file-scope finalization pass and if we
   should force out Dwarf descriptions of any and all file-scope tagged types
   which are still incomplete types.  */
static int finalizing = 0;

/* A pointer to the base of a list of references to DIE's that describe
   types.  The table is indexed by TYPE_UID() which is a unique number,
   indentifying each type.  */
static dw_die_ref *type_die_table;

/* Number of elements currently allocated for type_die_table.  */
static unsigned type_die_table_allocated;

/* Number of elements in type_die_table currently in use.  */
static unsigned type_die_table_in_use;

/* Size (in elements) of increments by which we may expand the
   type_die_table.  */
#define TYPE_DIE_TABLE_INCREMENT 4096

/* A pointer to the base of a table of references to DIE's that describe
   declarations.  The table is indexed by DECL_UID() which is a unique
   number, indentifying each decl.  */
static dw_die_ref *decl_die_table;

/* Number of elements currently allocated for the decl_die_table.  */
static unsigned decl_die_table_allocated;

/* Number of elements in decl_die_table currently in use.  */
static unsigned decl_die_table_in_use;

/* Size (in elements) of increments by which we may expand the
   decl_die_table.  */
#define DECL_DIE_TABLE_INCREMENT 256

/* A pointer to the base of a table of references to declaration
   scopes.  This table is a display which tracks the nesting
   of declaration scopes at the current scope and containing
   scopes.  This table is used to find the proper place to
   define type declaration DIE's.  */
static tree *decl_scope_table;

/* Number of elements currently allocated for the decl_scope_table.  */
static unsigned decl_scope_table_allocated;

/* Current level of nesting of declataion scopes.  */
static unsigned decl_scope_depth;

/* Size (in elements) of increments by which we may expand the
   decl_scope_table.  */
#define DECL_SCOPE_TABLE_INCREMENT 64

/* A pointer to the base of a list of references to DIE's that
   are uniquely identified by their tag, presence/absence of
   children DIE's, and list of attribute/value pairs.  */
static dw_die_ref *abbrev_die_table;

/* Number of elements currently allocated for abbrev_die_table.  */
static unsigned abbrev_die_table_allocated;

/* Number of elements in type_die_table currently in use.  */
static unsigned abbrev_die_table_in_use;

/* Size (in elements) of increments by which we may expand the
   abbrev_die_table.  */
#define ABBREV_DIE_TABLE_INCREMENT 256

/* A pointer to the base of a table that contains line information
   for each source code line in the compilation unit.  */
static dw_line_info_ref line_info_table;

/* Number of elements currently allocated for line_info_table.  */
static unsigned line_info_table_allocated;

/* Number of elements in line_info_table currently in use.  */
static unsigned line_info_table_in_use;

/* Size (in elements) of increments by which we may expand the
   line_info_table.  */
#define LINE_INFO_TABLE_INCREMENT 1024

/* Keep track of the last line_info_table entry number, returned
   by the prior call to lookup_filename().  This serves as a
   cache used to speed up file name look ups.  */
static unsigned prev_file_entry_num = (unsigned) -1;

/* A pointer to the base of a table that contains frame description
   information for each routine.  */
static dw_fde_ref fde_table;

/* Number of elements currently allocated for fde_table.  */
static unsigned fde_table_allocated;

/* Number of elements in fde_table currently in use.  */
static unsigned fde_table_in_use;

/* Size (in elements) of increments by which we may expand the
   fde_table.  */
#define FDE_TABLE_INCREMENT 256

/* The number of the current function definition for which debugging
   information is being generated.  These numbers range from 1 up to the
   maximum number of function definitions contained within the current
   compilation unit.  These numbers are used to create unique label id's
   unique to each function definition.  */
static unsigned current_funcdef_number = 1;

/* Some DWARF extensions (e.g., MIPS/SGI) implement a subprogram
   attribute that accelerates the lookup of the FDE associated
   with the subprogram.  This variable holds the table index of the FDE 
   associated with the current function (body) definition.  */
static unsigned current_funcdef_fde;

/* Record the size of the frame, so that the DW_AT_frame_base
   attribute can be set properly in gen_subprogram_die.  */
static long int current_funcdef_frame_size = 0;

/* DWARF requires that the compiler's primary datatypes
   are mapped into a reference to a DIE that defines that
   primary (base) type.  The base_type_info structure is used
   to track the correspondence between the name of a
   base type used by GCC, and its corresponding type
   characteristics.  Note, that the bt_size field below
   is the size in bits.  */
typedef struct base_type_struct *base_type_ref;
typedef struct base_type_struct
  {
    char *bt_name;
    enum dwarf_type bt_type;
    int bt_is_signed;
    int bt_size;
  }
base_type_info;

/* Characteristics of base types used by the compiler.  */
static base_type_info base_type_table[] =
{
  {"void", DW_ATE_unsigned, 0, 0},
  /* TODO: on some architectures, "char" may be signed. */
  {"char", DW_ATE_unsigned_char, 0, CHAR_TYPE_SIZE},
  {"unsigned char", DW_ATE_unsigned_char, 0, CHAR_TYPE_SIZE},
  {"signed char", DW_ATE_signed_char, 1, CHAR_TYPE_SIZE},
  {"int", DW_ATE_signed, 1, /* INT_TYPE_SIZE */ 4*8},
  {"unsigned int", DW_ATE_unsigned, 0, /* INT_TYPE_SIZE */ 4*8},
  {"short", DW_ATE_signed, 1, SHORT_TYPE_SIZE},
  {"short int", DW_ATE_signed, 1, SHORT_TYPE_SIZE},
  {"short unsigned int", DW_ATE_unsigned, 0, SHORT_TYPE_SIZE},
  {"long", DW_ATE_signed, 1, /* LONG_TYPE_SIZE */ 4*8},
  {"long int", DW_ATE_signed, 1, /* LONG_TYPE_SIZE */ 4*8},
  {"long unsigned int", DW_ATE_unsigned, 0, /* LONG_TYPE_SIZE */ 4*8},
  {"long long int", DW_ATE_signed, 1, LONG_LONG_TYPE_SIZE},
  {"long long unsigned int", DW_ATE_unsigned, 0, LONG_LONG_TYPE_SIZE},
  {"float", DW_ATE_float, 1, /* FLOAT_TYPE_SIZE */ 4*8},
  {"double", DW_ATE_float, 1, DOUBLE_TYPE_SIZE},
  {"long double", DW_ATE_float, 1, LONG_DOUBLE_TYPE_SIZE},
  {"complex", DW_ATE_complex_float, 1, 2 * /* FLOAT_TYPE_SIZE */ 4*8},
  {"double complex", DW_ATE_complex_float, 1, 2 * DOUBLE_TYPE_SIZE},
  {"long double complex", DW_ATE_complex_float, 1, 2 * LONG_DOUBLE_TYPE_SIZE}
};
#define NUM_BASE_TYPES (sizeof(base_type_table)/sizeof(base_type_info))

/* Record the DIE associated with a given base type  This table is
   parallel to the base_type_table, and records the DIE genereated
   to describe base type that has been previously referenced.  */
static dw_die_ref base_type_die_table[NUM_BASE_TYPES];

/* This predefined base type is used to create certain anonymous types */
static dw_die_ref int_base_type_die;

/* A pointer to the ..._DECL node which we have most recently been working
   on.  We keep this around just in case something about it looks screwy and
   we want to tell the user what the source coordinates for the actual
   declaration are.  */
static tree dwarf_last_decl;

/* A list of DIE reference attributes values that need backchaining
   support.  */
static backchain_ref backchain;

/* Forward declarations for functions defined in this file.  */
static void gen_type_die ();
static void add_type_attribute ();
static void decls_for_scope ();
static void gen_decl_die ();
static unsigned lookup_filename ();


/* Definitions of defaults for assembler-dependent names of various
   pseudo-ops and section names.
   Theses may be overridden in the tm.h file (if necessary) for a particular
   assembler.  */
#ifndef UNALIGNED_SHORT_ASM_OP
#define UNALIGNED_SHORT_ASM_OP	".2byte"
#endif
#ifndef UNALIGNED_INT_ASM_OP
#define UNALIGNED_INT_ASM_OP	".4byte"
#endif
#ifndef ASM_BYTE_OP
#define ASM_BYTE_OP		".byte"
#endif

/* Pseudo-op for defining a new section.  */
#ifndef SECTION_ASM_OP
#define SECTION_ASM_OP	".section"
#endif

/* The default format used by the ASM_OUTPUT_SECTION macro (see below) to
   print the SECTION_ASM_OP and the section name.  The default here works for
   almost all svr4 assemblers, except for the sparc, where the section name
   must be enclosed in double quotes.  (See sparcv4.h).  */
#ifndef SECTION_FORMAT
#define SECTION_FORMAT	"\t%s\t%s\n"
#endif

/* Section names used to hold DWARF debugging information.  */
#ifndef DEBUG_SECTION
#define DEBUG_SECTION		".debug_info"
#endif
#ifndef ABBREV_SECTION
#define ABBREV_SECTION		".debug_abbrev"
#endif
#ifndef ARANGES_SECTION
#define ARANGES_SECTION		".debug_aranges"
#endif
#ifndef DW_MACINFO_SECTION
#define DW_MACINFO_SECTION	".debug_macinfo"
#endif
#ifndef FRAME_SECTION
#define FRAME_SECTION		".debug_frame"
#endif
#ifndef LINE_SECTION
#define LINE_SECTION		".debug_line"
#endif
#ifndef LOC_SECTION
#define LOC_SECTION		".debug_loc"
#endif
#ifndef PUBNAMES_SECTION
#define PUBNAMES_SECTION	".debug_pubnames"
#endif
#ifndef STR_SECTION
#define STR_SECTION		".debug_str"
#endif

/* Standerd ELF section names for compiled code and data.  */
#ifndef TEXT_SECTION
#define TEXT_SECTION		".text"
#endif
#ifndef DATA_SECTION
#define DATA_SECTION		".data"
#endif
#ifndef DATA1_SECTION
#define DATA1_SECTION		".data1"
#endif
#ifndef RODATA_SECTION
#define RODATA_SECTION		".rodata"
#endif
#ifndef RODATA1_SECTION
#define RODATA1_SECTION		".rodata1"
#endif
#ifndef BSS_SECTION
#define BSS_SECTION		".bss"
#endif


/* Definitions of defaults for formats and names of various special
   (artificial) labels which may be generated within this file (when the -g
   options is used and DWARF_DEBUGGING_INFO is in effect.
   If necessary, these may be overridden from within the tm.h file, but
   typically, overriding these defaults is unnecessary.
   These labels have been hacked so that they all begin with a
   `.L' sequence to appease the stock sparc/svr4 assembler and the
   stock m88k/svr4 assembler, both of which need to see .L at the start of a
   label in order to prevent that label from going into the linker symbol
   table).  Eventually, the ASM_GENERATE_INTERNAL_LABEL and 
   ASM_OUTPUT_INTERNAL_LABEL should be used, but that will require
   a major rework.  */
#ifndef TEXT_BEGIN_LABEL
#define TEXT_BEGIN_LABEL	".L_text_b"
#endif
#ifndef TEXT_END_LABEL
#define TEXT_END_LABEL		".L_text_e"
#endif
#ifndef DATA_BEGIN_LABEL
#define DATA_BEGIN_LABEL	".L_data_b"
#endif
#ifndef DATA_END_LABEL
#define DATA_END_LABEL		".L_data_e"
#endif
#ifndef RODATA_BEGIN_LABEL
#define RODATA_BEGIN_LABEL	".L_rodata_b"
#endif
#ifndef RODATA_END_LABEL
#define RODATA_END_LABEL	".L_rodata_e"
#endif
#ifndef BSS_BEGIN_LABEL
#define BSS_BEGIN_LABEL		".L_bss_b"
#endif
#ifndef BSS_END_LABEL
#define BSS_END_LABEL		".L_bss_e"
#endif
#ifndef LINE_BEGIN_LABEL
#define LINE_BEGIN_LABEL	".L_line_b"
#endif
#ifndef LINE_END_LABEL
#define LINE_END_LABEL		".L_line_e"
#endif
#ifndef INSN_LABEL_FMT
#define INSN_LABEL_FMT		".L_I%u_%u"
#endif
#ifndef BLOCK_BEGIN_LABEL_FMT
#define BLOCK_BEGIN_LABEL_FMT	".L_B%u"
#endif
#ifndef BLOCK_END_LABEL_FMT
#define BLOCK_END_LABEL_FMT	".L_B%u_e"
#endif
#ifndef BODY_BEGIN_LABEL_FMT
#define BODY_BEGIN_LABEL_FMT	".L_b%u"
#endif
#ifndef BODY_END_LABEL_FMT
#define BODY_END_LABEL_FMT	".L_b%u_e"
#endif
#ifndef FUNC_END_LABEL_FMT
#define FUNC_END_LABEL_FMT	".L_f%u_e"
#endif
#ifndef LINE_CODE_LABEL_FMT
#define LINE_CODE_LABEL_FMT	".L_LC%u"
#endif
#ifndef SFNAMES_ENTRY_LABEL_FMT
#define SFNAMES_ENTRY_LABEL_FMT	".L_F%u"
#endif


/* Definitions of defaults for various types of primitive assembly language
   output operations.  These may be overridden from within the tm.h file,
   but typically, that is unecessary.  */
#ifndef ASM_OUTPUT_SECTION
#define ASM_OUTPUT_SECTION(FILE, SECTION) \
  fprintf ((FILE), SECTION_FORMAT, SECTION_ASM_OP, SECTION)
#endif

#ifndef ASM_OUTPUT_DWARF_DELTA2
#define ASM_OUTPUT_DWARF_DELTA2(FILE,LABEL1,LABEL2)			\
 do {	fprintf ((FILE), "\t%s\t", UNALIGNED_SHORT_ASM_OP);		\
	assemble_name (FILE, LABEL1);					\
	fprintf (FILE, "-");						\
	assemble_name (FILE, LABEL2);					\
  } while (0)
#endif

#ifndef ASM_OUTPUT_DWARF_DELTA4
#define ASM_OUTPUT_DWARF_DELTA4(FILE,LABEL1,LABEL2)			\
 do {	fprintf ((FILE), "\t%s\t", UNALIGNED_INT_ASM_OP);		\
	assemble_name (FILE, LABEL1);					\
	fprintf (FILE, "-");						\
	assemble_name (FILE, LABEL2);					\
  } while (0)
#endif

#ifndef ASM_OUTPUT_DWARF_ADDR
#define ASM_OUTPUT_DWARF_ADDR(FILE,LABEL)				\
 do {	fprintf ((FILE), "\t%s\t", UNALIGNED_INT_ASM_OP);		\
	assemble_name (FILE, LABEL);					\
  } while (0)
#endif

#ifndef ASM_OUTPUT_DWARF_ADDR_CONST
#define ASM_OUTPUT_DWARF_ADDR_CONST(FILE,ADDR)				\
    fprintf ((FILE), "\t%s\t%s", UNALIGNED_INT_ASM_OP, (ADDR))
#endif

#ifndef ASM_OUTPUT_DWARF_DATA1
#define ASM_OUTPUT_DWARF_DATA1(FILE,VALUE) \
  fprintf ((FILE), "\t%s\t0x%x", ASM_BYTE_OP, VALUE)
#endif

#ifndef ASM_OUTPUT_DWARF_DATA2
#define ASM_OUTPUT_DWARF_DATA2(FILE,VALUE) \
  fprintf ((FILE), "\t%s\t0x%x", UNALIGNED_SHORT_ASM_OP, (unsigned) VALUE)
#endif

#ifndef ASM_OUTPUT_DWARF_DATA4
#define ASM_OUTPUT_DWARF_DATA4(FILE,VALUE) \
  fprintf ((FILE), "\t%s\t0x%x", UNALIGNED_INT_ASM_OP, (unsigned) VALUE)
#endif

#ifndef ASM_OUTPUT_DWARF_DATA8
#define ASM_OUTPUT_DWARF_DATA8(FILE,HIGH_VALUE,LOW_VALUE)		\
  do {									\
    if (WORDS_BIG_ENDIAN)						\
      {									\
	fprintf ((FILE), "\t%s\t0x%x\n", UNALIGNED_INT_ASM_OP, HIGH_VALUE); \
	fprintf ((FILE), "\t%s\t0x%x", UNALIGNED_INT_ASM_OP, LOW_VALUE);\
      }									\
    else								\
      {									\
	fprintf ((FILE), "\t%s\t0x%x\n", UNALIGNED_INT_ASM_OP, LOW_VALUE);\
	fprintf ((FILE), "\t%s\t0x%x", UNALIGNED_INT_ASM_OP, HIGH_VALUE); \
      }									\
  } while (0)
#endif

/* This is similar to the default ASM_OUTPUT_ASCII, except that no trailing
   newline is produced.  When flag_verbose_asm is asserted, we add commnetary
   at the end of the line, so we must avoid output of a newline here.  */
#ifndef ASM_OUTPUT_DWARF_STRING
#define ASM_OUTPUT_DWARF_STRING(FILE,P) \
  do {									      \
    register int slen = strlen(P);                                            \
    register char *p = (P);  	                                              \
    register int i;					                      \
    fprintf (FILE, "\t.ascii \"");				              \
    for (i = 0; i < slen; i++)					              \
      {								              \
	  register int c = p[i];					      \
	  if (c == '\"' || c == '\\')					      \
	    putc ('\\', FILE);					              \
	  if (c >= ' ' && c < 0177)					      \
	    putc (c, FILE);					              \
	  else								      \
	    {								      \
	      fprintf (FILE, "\\%o", c);			              \
	    }							 	      \
      }								              \
    fprintf (FILE, "\\0\"");					              \
  }									      \
  while (0)
#endif

/* Convert a reference to the assembler name of a C-level name.  This
   macro has the same effect as ASM_OUTPUT_LABELREF, but copies to
   a string rather than writing to a file.  */
#ifndef ASM_NAME_TO_STRING
#define ASM_NAME_TO_STRING(STR, NAME) \
  do {									      \
      if ((NAME)[0] == '*')						      \
	strcpy (STR, NAME+1);						      \
      else								      \
	strcpy (STR, NAME);                                                   \
  }                                                                           \
  while (0)
#endif


/************************ general utility functions **************************/

/* Return a pointer to a copy of the section string name 's' with all
   attributes stripped off.  */
inline char *
stripattributes (s)
     register char *s;
{
  register char *stripped, *p;
  stripped = xstrdup (s);
  p = stripped;
  while (*p && *p != ',')
    p++;
  *p = '\0';
  return stripped;
}

/* Convert an integer constant expression into assembler syntax.
   Addition and subtraction are the only arithmetic
   that may appear in these expressions.   This is an adaptation
   of output_addr_const() in final.c.   Here, the target of the
   conversion is a string buffer.  We can't use output_addr_const
   directly, because it writes to a file.  */
static void
addr_const_to_string (str, x)
     char *str;
     rtx x;
{
  char buf1[256];
  char buf2[256];

restart:
  str[0] = '\0';
  switch (GET_CODE (x))
    {
    case PC:
      if (flag_pic)
	strcat (str, ",");
      else
	abort ();
      break;

    case SYMBOL_REF:
      ASM_NAME_TO_STRING (buf1, XSTR (x, 0));
      strcat (str, buf1);
      break;

    case LABEL_REF:
      ASM_GENERATE_INTERNAL_LABEL (buf1, "L", CODE_LABEL_NUMBER (XEXP (x, 0)));
      ASM_NAME_TO_STRING (buf2, buf1);
      strcat (str, buf2);
      break;

    case CODE_LABEL:
      ASM_GENERATE_INTERNAL_LABEL (buf1, "L", CODE_LABEL_NUMBER (x));
      ASM_NAME_TO_STRING (buf2, buf1);
      strcat (str, buf2);
      break;

    case CONST_INT:
      sprintf (buf1,
#if HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_INT
	       "%d",
#else
	       "%ld",
#endif
	       INTVAL (x));
      strcat (str, buf1);
      break;

    case CONST:
      /* This used to output parentheses around the expression, but that does 
         not work on the 386 (either ATT or BSD assembler).  */
      addr_const_to_string (buf1, XEXP (x, 0));
      strcat (str, buf1);
      break;

    case CONST_DOUBLE:
      if (GET_MODE (x) == VOIDmode)
	{
	  /* We can use %d if the number is one word and positive.  */
	  if (CONST_DOUBLE_HIGH (x))
	    sprintf (buf1,
#if HOST_BITS_PER_WIDE_INT == 64
#if HOST_BITS_PER_WIDE_INT != HOST_BITS_PER_INT
		     "0x%lx%016lx",
#else
		     "0x%x%016x",
#endif
#else
#if HOST_BITS_PER_WIDE_INT != HOST_BITS_PER_INT
		     "0x%lx%08lx",
#else
		     "0x%x%08x",
#endif
#endif
		     CONST_DOUBLE_HIGH (x), CONST_DOUBLE_LOW (x));
	  else if (CONST_DOUBLE_LOW (x) < 0)
	    sprintf (buf1,
#if HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_INT
		     "0x%x",
#else
		     "0x%lx",
#endif
		     CONST_DOUBLE_LOW (x));
	  else
	    sprintf (buf1,
#if HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_INT
		     "%d",
#else
		     "%ld",
#endif
		     CONST_DOUBLE_LOW (x));
	  strcat (str, buf1);
	}
      else
	/* We can't handle floating point constants; PRINT_OPERAND must
	   handle them.  */
	output_operand_lossage ("floating constant misused");
      break;

    case PLUS:
      /* Some assemblers need integer constants to appear last (eg masm).  */
      if (GET_CODE (XEXP (x, 0)) == CONST_INT)
	{
	  addr_const_to_string (buf1, XEXP (x, 1));
	  strcat (str, buf1);
	  if (INTVAL (XEXP (x, 0)) >= 0)
	    strcat (str, "+");
	  addr_const_to_string (buf1, XEXP (x, 0));
	  strcat (str, buf1);
	}
      else
	{
	  addr_const_to_string (buf1, XEXP (x, 0));
	  strcat (str, buf1);
	  if (INTVAL (XEXP (x, 0)) >= 0)
	    strcat (str, "+");
	  addr_const_to_string (buf1, XEXP (x, 1));
	  strcat (str, buf2);
	}
      break;

    case MINUS:
      /* Avoid outputting things like x-x or x+5-x, since some assemblers
         can't handle that.  */
      x = simplify_subtraction (x);
      if (GET_CODE (x) != MINUS)
	goto restart;

      addr_const_to_string (buf1, XEXP (x, 0));
      strcat (str, buf1);
      strcat (str, "-");
      if (GET_CODE (XEXP (x, 1)) == CONST_INT
	  && INTVAL (XEXP (x, 1)) < 0)
	{
	  strcat (str, ASM_OPEN_PAREN);
	  addr_const_to_string (buf1, XEXP (x, 1));
	  strcat (str, buf1);
	  strcat (str, ASM_CLOSE_PAREN);
	}
      else
	{
	  addr_const_to_string (buf1, XEXP (x, 1));
	  strcat (str, buf1);
	}
      break;

    case ZERO_EXTEND:
    case SIGN_EXTEND:
      addr_const_to_string (buf1, XEXP (x, 0));
      strcat (str, buf1);
      break;

    default:
      output_operand_lossage ("invalid expression as operand");
    }
}

/* Convert an address constant to a string, and return a pointer to
   a copy of the result, located on the heap.  */
static char *
addr_to_string (x)
     rtx x;
{
  char buf[1024];
  addr_const_to_string (buf, x);
  return xstrdup (buf);
}

/* Test if rtl node points to a psuedo register.  */
inline int
is_pseudo_reg (rtl)
     register rtx rtl;
{
  return (((GET_CODE (rtl) == REG) && (REGNO (rtl) >= FIRST_PSEUDO_REGISTER))
	  || ((GET_CODE (rtl) == SUBREG)
	      && (REGNO (XEXP (rtl, 0)) >= FIRST_PSEUDO_REGISTER)));
}


/* Return a reference to a type, with its const and volatile qualifiers
   removed.  */
inline tree
type_main_variant (type)
     register tree type;
{
  type = TYPE_MAIN_VARIANT (type);

  /* There really should be only one main variant among any group of variants 
     of a given type (and all of the MAIN_VARIANT values for all members of
     the group should point to that one type) but sometimes the C front-end
     messes this up for array types, so we work around that bug here.  */
  if (TREE_CODE (type) == ARRAY_TYPE)
    {
      while (type != TYPE_MAIN_VARIANT (type))
	type = TYPE_MAIN_VARIANT (type);
    }
  return type;
}

/* Return non-zero if the given type node represents a tagged type.  */
inline int
is_tagged_type (type)
     register tree type;
{
  register enum tree_code code = TREE_CODE (type);

  return (code == RECORD_TYPE || code == UNION_TYPE
	  || code == QUAL_UNION_TYPE || code == ENUMERAL_TYPE);
}

/* Convert a DIE tag into its string name.  */
static char *
dwarf_tag_name (tag)
     register unsigned tag;
{
  switch (tag)
    {
    case DW_TAG_padding:
      return "DW_TAG_padding";
    case DW_TAG_array_type:
      return "DW_TAG_array_type";
    case DW_TAG_class_type:
      return "DW_TAG_class_type";
    case DW_TAG_entry_point:
      return "DW_TAG_entry_point";
    case DW_TAG_enumeration_type:
      return "DW_TAG_enumeration_type";
    case DW_TAG_formal_parameter:
      return "DW_TAG_formal_parameter";
    case DW_TAG_imported_declaration:
      return "DW_TAG_imported_declaration";
    case DW_TAG_label:
      return "DW_TAG_label";
    case DW_TAG_lexical_block:
      return "DW_TAG_lexical_block";
    case DW_TAG_member:
      return "DW_TAG_member";
    case DW_TAG_pointer_type:
      return "DW_TAG_pointer_type";
    case DW_TAG_reference_type:
      return "DW_TAG_reference_type";
    case DW_TAG_compile_unit:
      return "DW_TAG_compile_unit";
    case DW_TAG_string_type:
      return "DW_TAG_string_type";
    case DW_TAG_structure_type:
      return "DW_TAG_structure_type";
    case DW_TAG_subroutine_type:
      return "DW_TAG_subroutine_type";
    case DW_TAG_typedef:
      return "DW_TAG_typedef";
    case DW_TAG_union_type:
      return "DW_TAG_union_type";
    case DW_TAG_unspecified_parameters:
      return "DW_TAG_unspecified_parameters";
    case DW_TAG_variant:
      return "DW_TAG_variant";
    case DW_TAG_common_block:
      return "DW_TAG_common_block";
    case DW_TAG_common_inclusion:
      return "DW_TAG_common_inclusion";
    case DW_TAG_inheritance:
      return "DW_TAG_inheritance";
    case DW_TAG_inlined_subroutine:
      return "DW_TAG_inlined_subroutine";
    case DW_TAG_module:
      return "DW_TAG_module";
    case DW_TAG_ptr_to_member_type:
      return "DW_TAG_ptr_to_member_type";
    case DW_TAG_set_type:
      return "DW_TAG_set_type";
    case DW_TAG_subrange_type:
      return "DW_TAG_subrange_type";
    case DW_TAG_with_stmt:
      return "DW_TAG_with_stmt";
    case DW_TAG_access_declaration:
      return "DW_TAG_access_declaration";
    case DW_TAG_base_type:
      return "DW_TAG_base_type";
    case DW_TAG_catch_block:
      return "DW_TAG_catch_block";
    case DW_TAG_const_type:
      return "DW_TAG_const_type";
    case DW_TAG_constant:
      return "DW_TAG_constant";
    case DW_TAG_enumerator:
      return "DW_TAG_enumerator";
    case DW_TAG_file_type:
      return "DW_TAG_file_type";
    case DW_TAG_friend:
      return "DW_TAG_friend";
    case DW_TAG_namelist:
      return "DW_TAG_namelist";
    case DW_TAG_namelist_item:
      return "DW_TAG_namelist_item";
    case DW_TAG_packed_type:
      return "DW_TAG_packed_type";
    case DW_TAG_subprogram:
      return "DW_TAG_subprogram";
    case DW_TAG_template_type_param:
      return "DW_TAG_template_type_param";
    case DW_TAG_template_value_param:
      return "DW_TAG_template_value_param";
    case DW_TAG_thrown_type:
      return "DW_TAG_thrown_type";
    case DW_TAG_try_block:
      return "DW_TAG_try_block";
    case DW_TAG_variant_part:
      return "DW_TAG_variant_part";
    case DW_TAG_variable:
      return "DW_TAG_variable";
    case DW_TAG_volatile_type:
      return "DW_TAG_volatile_type";
    case DW_TAG_MIPS_loop:
      return "DW_TAG_MIPS_loop";
    case DW_TAG_format_label:
      return "DW_TAG_format_label";
    case DW_TAG_function_template:
      return "DW_TAG_function_template";
    case DW_TAG_class_template:
      return "DW_TAG_class_template";
    default:
      return "DW_TAG_<unknown>";
    }
}

/* Convert a DWARF attribute code into its string name.  */
static char *
dwarf_attr_name (attr)
     register unsigned attr;
{
  switch (attr)
    {
    case DW_AT_sibling:
      return "DW_AT_sibling";
    case DW_AT_location:
      return "DW_AT_location";
    case DW_AT_name:
      return "DW_AT_name";
    case DW_AT_ordering:
      return "DW_AT_ordering";
    case DW_AT_subscr_data:
      return "DW_AT_subscr_data";
    case DW_AT_byte_size:
      return "DW_AT_byte_size";
    case DW_AT_bit_offset:
      return "DW_AT_bit_offset";
    case DW_AT_bit_size:
      return "DW_AT_bit_size";
    case DW_AT_element_list:
      return "DW_AT_element_list";
    case DW_AT_stmt_list:
      return "DW_AT_stmt_list";
    case DW_AT_low_pc:
      return "DW_AT_low_pc";
    case DW_AT_high_pc:
      return "DW_AT_high_pc";
    case DW_AT_language:
      return "DW_AT_language";
    case DW_AT_member:
      return "DW_AT_member";
    case DW_AT_discr:
      return "DW_AT_discr";
    case DW_AT_discr_value:
      return "DW_AT_discr_value";
    case DW_AT_visibility:
      return "DW_AT_visibility";
    case DW_AT_import:
      return "DW_AT_import";
    case DW_AT_string_length:
      return "DW_AT_string_length";
    case DW_AT_common_reference:
      return "DW_AT_common_reference";
    case DW_AT_comp_dir:
      return "DW_AT_comp_dir";
    case DW_AT_const_value:
      return "DW_AT_const_value";
    case DW_AT_containing_type:
      return "DW_AT_containing_type";
    case DW_AT_default_value:
      return "DW_AT_default_value";
    case DW_AT_inline:
      return "DW_AT_inline";
    case DW_AT_is_optional:
      return "DW_AT_is_optional";
    case DW_AT_lower_bound:
      return "DW_AT_lower_bound";
    case DW_AT_producer:
      return "DW_AT_producer";
    case DW_AT_prototyped:
      return "DW_AT_prototyped";
    case DW_AT_return_addr:
      return "DW_AT_return_addr";
    case DW_AT_start_scope:
      return "DW_AT_start_scope";
    case DW_AT_stride_size:
      return "DW_AT_stride_size";
    case DW_AT_upper_bound:
      return "DW_AT_upper_bound";
    case DW_AT_abstract_origin:
      return "DW_AT_abstract_origin";
    case DW_AT_accessibility:
      return "DW_AT_accessibility";
    case DW_AT_address_class:
      return "DW_AT_address_class";
    case DW_AT_artificial:
      return "DW_AT_artificial";
    case DW_AT_base_types:
      return "DW_AT_base_types";
    case DW_AT_calling_convention:
      return "DW_AT_calling_convention";
    case DW_AT_count:
      return "DW_AT_count";
    case DW_AT_data_member_location:
      return "DW_AT_data_member_location";
    case DW_AT_decl_column:
      return "DW_AT_decl_column";
    case DW_AT_decl_file:
      return "DW_AT_decl_file";
    case DW_AT_decl_line:
      return "DW_AT_decl_line";
    case DW_AT_declaration:
      return "DW_AT_declaration";
    case DW_AT_discr_list:
      return "DW_AT_discr_list";
    case DW_AT_encoding:
      return "DW_AT_encoding";
    case DW_AT_external:
      return "DW_AT_external";
    case DW_AT_frame_base:
      return "DW_AT_frame_base";
    case DW_AT_friend:
      return "DW_AT_friend";
    case DW_AT_identifier_case:
      return "DW_AT_identifier_case";
    case DW_AT_macro_info:
      return "DW_AT_macro_info";
    case DW_AT_namelist_items:
      return "DW_AT_namelist_items";
    case DW_AT_priority:
      return "DW_AT_priority";
    case DW_AT_segment:
      return "DW_AT_segment";
    case DW_AT_specification:
      return "DW_AT_specification";
    case DW_AT_static_link:
      return "DW_AT_static_link";
    case DW_AT_type:
      return "DW_AT_type";
    case DW_AT_use_location:
      return "DW_AT_use_location";
    case DW_AT_variable_parameter:
      return "DW_AT_variable_parameter";
    case DW_AT_virtuality:
      return "DW_AT_virtuality";
    case DW_AT_vtable_elem_location:
      return "DW_AT_vtable_elem_location";

#ifdef MIPS_DEBUGGING_INFO
    case DW_AT_MIPS_fde:
      return "DW_AT_MIPS_fde";
    case DW_AT_MIPS_loop_begin:
      return "DW_AT_MIPS_loop_begin";
    case DW_AT_MIPS_tail_loop_begin:
      return "DW_AT_MIPS_tail_loop_begin";
    case DW_AT_MIPS_epilog_begin:
      return "DW_AT_MIPS_epilog_begin";
    case DW_AT_MIPS_loop_unroll_factor:
      return "DW_AT_MIPS_loop_unroll_factor";
    case DW_AT_MIPS_software_pipeline_depth:
      return "DW_AT_MIPS_software_pipeline_depth";
    case DW_AT_MIPS_linkage_name:
      return "DW_AT_MIPS_linkage_name";
#endif

    case DW_AT_sf_names:
      return "DW_AT_sf_names";
    case DW_AT_src_info:
      return "DW_AT_src_info";
    case DW_AT_mac_info:
      return "DW_AT_mac_info";
    case DW_AT_src_coords:
      return "DW_AT_src_coords";
    case DW_AT_body_begin:
      return "DW_AT_body_begin";
    case DW_AT_body_end:
      return "DW_AT_body_end";
    default:
      return "DW_AT_<unknown>";
    }
}

/* Convert a DWARF value form code into its string name.  */
static char *
dwarf_form_name (form)
     register unsigned form;
{
  switch (form)
    {
    case DW_FORM_addr:
      return "DW_FORM_addr";
    case DW_FORM_block2:
      return "DW_FORM_block2";
    case DW_FORM_block4:
      return "DW_FORM_block4";
    case DW_FORM_data2:
      return "DW_FORM_data2";
    case DW_FORM_data4:
      return "DW_FORM_data4";
    case DW_FORM_data8:
      return "DW_FORM_data8";
    case DW_FORM_string:
      return "DW_FORM_string";
    case DW_FORM_block:
      return "DW_FORM_block";
    case DW_FORM_block1:
      return "DW_FORM_block1";
    case DW_FORM_data1:
      return "DW_FORM_data1";
    case DW_FORM_flag:
      return "DW_FORM_flag";
    case DW_FORM_sdata:
      return "DW_FORM_sdata";
    case DW_FORM_strp:
      return "DW_FORM_strp";
    case DW_FORM_udata:
      return "DW_FORM_udata";
    case DW_FORM_ref_addr:
      return "DW_FORM_ref_addr";
    case DW_FORM_ref1:
      return "DW_FORM_ref1";
    case DW_FORM_ref2:
      return "DW_FORM_ref2";
    case DW_FORM_ref4:
      return "DW_FORM_ref4";
    case DW_FORM_ref8:
      return "DW_FORM_ref8";
    case DW_FORM_ref_udata:
      return "DW_FORM_ref_udata";
    case DW_FORM_indirect:
      return "DW_FORM_indirect";
    default:
      return "DW_FORM_<unknown>";
    }
}

/* Convert a DWARF stack opcode into its string name.  */
static char *
dwarf_stack_op_name (op)
     register unsigned op;
{
  switch (op)
    {
    case DW_OP_addr:
      return "DW_OP_addr";
    case DW_OP_deref:
      return "DW_OP_deref";
    case DW_OP_const1u:
      return "DW_OP_const1u";
    case DW_OP_const1s:
      return "DW_OP_const1s";
    case DW_OP_const2u:
      return "DW_OP_const2u";
    case DW_OP_const2s:
      return "DW_OP_const2s";
    case DW_OP_const4u:
      return "DW_OP_const4u";
    case DW_OP_const4s:
      return "DW_OP_const4s";
    case DW_OP_const8u:
      return "DW_OP_const8u";
    case DW_OP_const8s:
      return "DW_OP_const8s";
    case DW_OP_constu:
      return "DW_OP_constu";
    case DW_OP_consts:
      return "DW_OP_consts";
    case DW_OP_dup:
      return "DW_OP_dup";
    case DW_OP_drop:
      return "DW_OP_drop";
    case DW_OP_over:
      return "DW_OP_over";
    case DW_OP_pick:
      return "DW_OP_pick";
    case DW_OP_swap:
      return "DW_OP_swap";
    case DW_OP_rot:
      return "DW_OP_rot";
    case DW_OP_xderef:
      return "DW_OP_xderef";
    case DW_OP_abs:
      return "DW_OP_abs";
    case DW_OP_and:
      return "DW_OP_and";
    case DW_OP_div:
      return "DW_OP_div";
    case DW_OP_minus:
      return "DW_OP_minus";
    case DW_OP_mod:
      return "DW_OP_mod";
    case DW_OP_mul:
      return "DW_OP_mul";
    case DW_OP_neg:
      return "DW_OP_neg";
    case DW_OP_not:
      return "DW_OP_not";
    case DW_OP_or:
      return "DW_OP_or";
    case DW_OP_plus:
      return "DW_OP_plus";
    case DW_OP_plus_uconst:
      return "DW_OP_plus_uconst";
    case DW_OP_shl:
      return "DW_OP_shl";
    case DW_OP_shr:
      return "DW_OP_shr";
    case DW_OP_shra:
      return "DW_OP_shra";
    case DW_OP_xor:
      return "DW_OP_xor";
    case DW_OP_bra:
      return "DW_OP_bra";
    case DW_OP_eq:
      return "DW_OP_eq";
    case DW_OP_ge:
      return "DW_OP_ge";
    case DW_OP_gt:
      return "DW_OP_gt";
    case DW_OP_le:
      return "DW_OP_le";
    case DW_OP_lt:
      return "DW_OP_lt";
    case DW_OP_ne:
      return "DW_OP_ne";
    case DW_OP_skip:
      return "DW_OP_skip";
    case DW_OP_lit0:
      return "DW_OP_lit0";
    case DW_OP_lit1:
      return "DW_OP_lit1";
    case DW_OP_lit2:
      return "DW_OP_lit2";
    case DW_OP_lit3:
      return "DW_OP_lit3";
    case DW_OP_lit4:
      return "DW_OP_lit4";
    case DW_OP_lit5:
      return "DW_OP_lit5";
    case DW_OP_lit6:
      return "DW_OP_lit6";
    case DW_OP_lit7:
      return "DW_OP_lit7";
    case DW_OP_lit8:
      return "DW_OP_lit8";
    case DW_OP_lit9:
      return "DW_OP_lit9";
    case DW_OP_lit10:
      return "DW_OP_lit10";
    case DW_OP_lit11:
      return "DW_OP_lit11";
    case DW_OP_lit12:
      return "DW_OP_lit12";
    case DW_OP_lit13:
      return "DW_OP_lit13";
    case DW_OP_lit14:
      return "DW_OP_lit14";
    case DW_OP_lit15:
      return "DW_OP_lit15";
    case DW_OP_lit16:
      return "DW_OP_lit16";
    case DW_OP_lit17:
      return "DW_OP_lit17";
    case DW_OP_lit18:
      return "DW_OP_lit18";
    case DW_OP_lit19:
      return "DW_OP_lit19";
    case DW_OP_lit20:
      return "DW_OP_lit20";
    case DW_OP_lit21:
      return "DW_OP_lit21";
    case DW_OP_lit22:
      return "DW_OP_lit22";
    case DW_OP_lit23:
      return "DW_OP_lit23";
    case DW_OP_lit24:
      return "DW_OP_lit24";
    case DW_OP_lit25:
      return "DW_OP_lit25";
    case DW_OP_lit26:
      return "DW_OP_lit26";
    case DW_OP_lit27:
      return "DW_OP_lit27";
    case DW_OP_lit28:
      return "DW_OP_lit28";
    case DW_OP_lit29:
      return "DW_OP_lit29";
    case DW_OP_lit30:
      return "DW_OP_lit30";
    case DW_OP_lit31:
      return "DW_OP_lit31";
    case DW_OP_reg0:
      return "DW_OP_reg0";
    case DW_OP_reg1:
      return "DW_OP_reg1";
    case DW_OP_reg2:
      return "DW_OP_reg2";
    case DW_OP_reg3:
      return "DW_OP_reg3";
    case DW_OP_reg4:
      return "DW_OP_reg4";
    case DW_OP_reg5:
      return "DW_OP_reg5";
    case DW_OP_reg6:
      return "DW_OP_reg6";
    case DW_OP_reg7:
      return "DW_OP_reg7";
    case DW_OP_reg8:
      return "DW_OP_reg8";
    case DW_OP_reg9:
      return "DW_OP_reg9";
    case DW_OP_reg10:
      return "DW_OP_reg10";
    case DW_OP_reg11:
      return "DW_OP_reg11";
    case DW_OP_reg12:
      return "DW_OP_reg12";
    case DW_OP_reg13:
      return "DW_OP_reg13";
    case DW_OP_reg14:
      return "DW_OP_reg14";
    case DW_OP_reg15:
      return "DW_OP_reg15";
    case DW_OP_reg16:
      return "DW_OP_reg16";
    case DW_OP_reg17:
      return "DW_OP_reg17";
    case DW_OP_reg18:
      return "DW_OP_reg18";
    case DW_OP_reg19:
      return "DW_OP_reg19";
    case DW_OP_reg20:
      return "DW_OP_reg20";
    case DW_OP_reg21:
      return "DW_OP_reg21";
    case DW_OP_reg22:
      return "DW_OP_reg22";
    case DW_OP_reg23:
      return "DW_OP_reg23";
    case DW_OP_reg24:
      return "DW_OP_reg24";
    case DW_OP_reg25:
      return "DW_OP_reg25";
    case DW_OP_reg26:
      return "DW_OP_reg26";
    case DW_OP_reg27:
      return "DW_OP_reg27";
    case DW_OP_reg28:
      return "DW_OP_reg28";
    case DW_OP_reg29:
      return "DW_OP_reg29";
    case DW_OP_reg30:
      return "DW_OP_reg30";
    case DW_OP_reg31:
      return "DW_OP_reg31";
    case DW_OP_breg0:
      return "DW_OP_breg0";
    case DW_OP_breg1:
      return "DW_OP_breg1";
    case DW_OP_breg2:
      return "DW_OP_breg2";
    case DW_OP_breg3:
      return "DW_OP_breg3";
    case DW_OP_breg4:
      return "DW_OP_breg4";
    case DW_OP_breg5:
      return "DW_OP_breg5";
    case DW_OP_breg6:
      return "DW_OP_breg6";
    case DW_OP_breg7:
      return "DW_OP_breg7";
    case DW_OP_breg8:
      return "DW_OP_breg8";
    case DW_OP_breg9:
      return "DW_OP_breg9";
    case DW_OP_breg10:
      return "DW_OP_breg10";
    case DW_OP_breg11:
      return "DW_OP_breg11";
    case DW_OP_breg12:
      return "DW_OP_breg12";
    case DW_OP_breg13:
      return "DW_OP_breg13";
    case DW_OP_breg14:
      return "DW_OP_breg14";
    case DW_OP_breg15:
      return "DW_OP_breg15";
    case DW_OP_breg16:
      return "DW_OP_breg16";
    case DW_OP_breg17:
      return "DW_OP_breg17";
    case DW_OP_breg18:
      return "DW_OP_breg18";
    case DW_OP_breg19:
      return "DW_OP_breg19";
    case DW_OP_breg20:
      return "DW_OP_breg20";
    case DW_OP_breg21:
      return "DW_OP_breg21";
    case DW_OP_breg22:
      return "DW_OP_breg22";
    case DW_OP_breg23:
      return "DW_OP_breg23";
    case DW_OP_breg24:
      return "DW_OP_breg24";
    case DW_OP_breg25:
      return "DW_OP_breg25";
    case DW_OP_breg26:
      return "DW_OP_breg26";
    case DW_OP_breg27:
      return "DW_OP_breg27";
    case DW_OP_breg28:
      return "DW_OP_breg28";
    case DW_OP_breg29:
      return "DW_OP_breg29";
    case DW_OP_breg30:
      return "DW_OP_breg30";
    case DW_OP_breg31:
      return "DW_OP_breg31";
    case DW_OP_regx:
      return "DW_OP_regx";
    case DW_OP_fbreg:
      return "DW_OP_fbreg";
    case DW_OP_bregx:
      return "DW_OP_bregx";
    case DW_OP_piece:
      return "DW_OP_piece";
    case DW_OP_deref_size:
      return "DW_OP_deref_size";
    case DW_OP_xderef_size:
      return "DW_OP_xderef_size";
    case DW_OP_nop:
      return "DW_OP_nop";
    default:
      return "OP_<unknown>";
    }
}

/* Convert a DWARF type code into its string name.  */
static char *
dwarf_type_encoding_name (enc)
     register unsigned enc;
{
  switch (enc)
    {
    case DW_ATE_address:
      return "DW_ATE_address";
    case DW_ATE_boolean:
      return "DW_ATE_boolean";
    case DW_ATE_complex_float:
      return "DW_ATE_complex_float";
    case DW_ATE_float:
      return "DW_ATE_float";
    case DW_ATE_signed:
      return "DW_ATE_signed";
    case DW_ATE_signed_char:
      return "DW_ATE_signed_char";
    case DW_ATE_unsigned:
      return "DW_ATE_unsigned";
    case DW_ATE_unsigned_char:
      return "DW_ATE_unsigned_char";
    default:
      return "DW_ATE_<unknown>";
    }
}

/* Convert a DWARF call frame info. operation to its string name */
static char *
dwarf_cfi_name (cfi_opc)
     register unsigned cfi_opc;
{
  switch (cfi_opc)
    {
    case DW_CFA_advance_loc:
      return "DW_CFA_advance_loc";
    case DW_CFA_offset:
      return "DW_CFA_offset";
    case DW_CFA_restore:
      return "DW_CFA_restore";
    case DW_CFA_nop:
      return "DW_CFA_nop";
    case DW_CFA_set_loc:
      return "DW_CFA_set_loc";
    case DW_CFA_advance_loc1:
      return "DW_CFA_advance_loc1";
    case DW_CFA_advance_loc2:
      return "DW_CFA_advance_loc2";
    case DW_CFA_advance_loc4:
      return "DW_CFA_advance_loc4";
    case DW_CFA_offset_extended:
      return "DW_CFA_offset_extended";
    case DW_CFA_restore_extended:
      return "DW_CFA_restore_extended";
    case DW_CFA_undefined:
      return "DW_CFA_undefined";
    case DW_CFA_same_value:
      return "DW_CFA_same_value";
    case DW_CFA_register:
      return "DW_CFA_register";
    case DW_CFA_remember_state:
      return "DW_CFA_remember_state";
    case DW_CFA_restore_state:
      return "DW_CFA_restore_state";
    case DW_CFA_def_cfa:
      return "DW_CFA_def_cfa";
    case DW_CFA_def_cfa_register:
      return "DW_CFA_def_cfa_register";
    case DW_CFA_def_cfa_offset:
      return "DW_CFA_def_cfa_offset";
    /* SGI/MIPS specific */
    case DW_CFA_MIPS_advance_loc8:
      return "DW_CFA_MIPS_advance_loc8";
    default:
      return "DW_CFA_<unknown>";
    }
}

/* Determine the "ultimate origin" of a decl.  The decl may be an inlined
   instance of an inlined instance of a decl which is local to an inline
   function, so we have to trace all of the way back through the origin chain
   to find out what sort of node actually served as the original seed for the
   given block.  */
static tree
decl_ultimate_origin (decl)
     register tree decl;
{
  register tree immediate_origin = DECL_ABSTRACT_ORIGIN (decl);

  if (immediate_origin == NULL)
    return NULL;
  else
    {
      register tree ret_val;
      register tree lookahead = immediate_origin;

      do
	{
	  ret_val = lookahead;
	  lookahead = DECL_ABSTRACT_ORIGIN (ret_val);
	}
      while (lookahead != NULL && lookahead != ret_val);
      return ret_val;
    }
}

/* Determine the "ultimate origin" of a block.  The block may be an inlined
   instance of an inlined instance of a block which is local to an inline
   function, so we have to trace all of the way back through the origin chain
   to find out what sort of node actually served as the original seed for the
   given block.  */
static tree
block_ultimate_origin (block)
     register tree block;
{
  register tree immediate_origin = BLOCK_ABSTRACT_ORIGIN (block);

  if (immediate_origin == NULL)
    return NULL;
  else
    {
      register tree ret_val;
      register tree lookahead = immediate_origin;

      do
	{
	  ret_val = lookahead;
	  lookahead = (TREE_CODE (ret_val) == BLOCK)
	    ? BLOCK_ABSTRACT_ORIGIN (ret_val)
	    : NULL;
	}
      while (lookahead != NULL && lookahead != ret_val);
      return ret_val;
    }
}

/**************** DIE internal representation constturction *******************/

/* Add an attribute/value pair to a DIE */
inline void
add_dwarf_attr (die, attr)
     register dw_die_ref die;
     register dw_attr_ref attr;
{
  if (die != NULL && attr != NULL)
    {
      if (die->die_attr == NULL)
	{
	  die->die_attr = attr;
	  die->die_attr_last = attr;
	}
      else
	{
	  die->die_attr_last->dw_attr_next = attr;
	  die->die_attr_last = attr;
	}
    }
}

/* Add a flag value attribute to a DIE.  */
inline void
add_AT_flag (die, attr_kind, flag)
     register dw_die_ref die;
     register enum dwarf_attribute attr_kind;
     register unsigned flag;
{
  register dw_attr_ref attr = (dw_attr_ref) xmalloc (sizeof (dw_attr_node));
  if (attr != NULL)
    {
      attr->dw_attr_next = NULL;
      attr->dw_attr = attr_kind;
      attr->dw_attr_val.val_class = dw_val_class_flag;
      attr->dw_attr_val.v.val_flag = flag;
      add_dwarf_attr (die, attr);
    }
}

/* Add a signed integer attribute value to a DIE.  */
inline void
add_AT_int (die, attr_kind, int_val)
     register dw_die_ref die;
     register enum dwarf_attribute attr_kind;
     register long int int_val;
{
  register dw_attr_ref attr = (dw_attr_ref) xmalloc (sizeof (dw_attr_node));
  if (attr != NULL)
    {
      attr->dw_attr_next = NULL;
      attr->dw_attr = attr_kind;
      attr->dw_attr_val.val_class = dw_val_class_const;
      attr->dw_attr_val.v.val_int = int_val;
      add_dwarf_attr (die, attr);
    }
}

/* Add an unsigned integer attribute value to a DIE.  */
inline void
add_AT_unsigned (die, attr_kind, unsigned_val)
     register dw_die_ref die;
     register enum dwarf_attribute attr_kind;
     register unsigned long unsigned_val;
{
  register dw_attr_ref attr = (dw_attr_ref) xmalloc (sizeof (dw_attr_node));
  if (attr != NULL)
    {
      attr->dw_attr_next = NULL;
      attr->dw_attr = attr_kind;
      attr->dw_attr_val.val_class = dw_val_class_unsigned_const;
      attr->dw_attr_val.v.val_unsigned = unsigned_val;
      add_dwarf_attr (die, attr);
    }
}

/* Add an unsigned double integer attribute value to a DIE.  */
inline void
add_AT_double (die, attr_kind, val_hi, val_low)
     register dw_die_ref die;
     register enum dwarf_attribute attr_kind;
     register unsigned long val_hi;
     register unsigned long val_low;
{
  register dw_attr_ref attr = (dw_attr_ref) xmalloc (sizeof (dw_attr_node));
  if (attr != NULL)
    {
      attr->dw_attr_next = NULL;
      attr->dw_attr = attr_kind;
      attr->dw_attr_val.val_class = dw_val_class_double_const;
      attr->dw_attr_val.v.val_dbl_const.dw_dbl_hi = val_hi;
      attr->dw_attr_val.v.val_dbl_const.dw_dbl_low = val_low;
      add_dwarf_attr (die, attr);
    }
}

/* Add a string attribute value to a DIE.  */
inline void
add_AT_string (die, attr_kind, str)
     register dw_die_ref die;
     register enum dwarf_attribute attr_kind;
     register char *str;
{
  register dw_attr_ref attr = (dw_attr_ref) xmalloc (sizeof (dw_attr_node));
  if (attr != NULL)
    {
      attr->dw_attr_next = NULL;
      attr->dw_attr = attr_kind;
      attr->dw_attr_val.val_class = dw_val_class_str;
      attr->dw_attr_val.v.val_str = xstrdup (str);
      add_dwarf_attr (die, attr);
    }
}

/* Add a DIE reference attribute value to a DIE.  */
inline void
add_AT_die_ref (die, attr_kind, targ_die)
     register dw_die_ref die;
     register enum dwarf_attribute attr_kind;
     register dw_die_ref targ_die;
{
  register dw_attr_ref attr = (dw_attr_ref) xmalloc (sizeof (dw_attr_node));
  if (attr != NULL)
    {
      attr->dw_attr_next = NULL;
      attr->dw_attr = attr_kind;
      attr->dw_attr_val.val_class = dw_val_class_die_ref;
      attr->dw_attr_val.v.val_die_ref = targ_die;
      add_dwarf_attr (die, attr);
    }
}

/* Add an FDE reference attribute value to a DIE.  */
inline void
add_AT_fde_ref (die, attr_kind, targ_fde)
     register dw_die_ref die;
     register enum dwarf_attribute attr_kind;
     register unsigned targ_fde;
{
  register dw_attr_ref attr;

  attr = (dw_attr_ref) xmalloc (sizeof (dw_attr_node));
  if (attr != NULL)
    {
      attr->dw_attr_next = NULL;
      attr->dw_attr = attr_kind;
      attr->dw_attr_val.val_class = dw_val_class_fde_ref;
      attr->dw_attr_val.v.val_fde_index = targ_fde;
      add_dwarf_attr (die, attr);
    }
}

/* Add a location description attribute value to a DIE.  */
inline void
add_AT_loc (die, attr_kind, loc)
     register dw_die_ref die;
     register enum dwarf_attribute attr_kind;
     register dw_loc_descr_ref loc;
{
  register dw_attr_ref attr = (dw_attr_ref) xmalloc (sizeof (dw_attr_node));
  if (attr != NULL)
    {
      attr->dw_attr_next = NULL;
      attr->dw_attr = attr_kind;
      attr->dw_attr_val.val_class = dw_val_class_loc;
      attr->dw_attr_val.v.val_loc = loc;
      add_dwarf_attr (die, attr);
    }
}

/* Add an address constant attribute value to a DIE.  */
inline void
add_AT_addr (die, attr_kind, addr)
     register dw_die_ref die;
     register enum dwarf_attribute attr_kind;
     char *addr;
{
  register dw_attr_ref attr = (dw_attr_ref) xmalloc (sizeof (dw_attr_node));
  if (attr != NULL)
    {
      attr->dw_attr_next = NULL;
      attr->dw_attr = attr_kind;
      attr->dw_attr_val.val_class = dw_val_class_addr;
      attr->dw_attr_val.v.val_addr = addr;
      add_dwarf_attr (die, attr);
    }
}

/* Add a label identifier attribute value to a DIE.  */
inline void
add_AT_lbl_id (die, attr_kind, lbl_id)
     register dw_die_ref die;
     register enum dwarf_attribute attr_kind;
     register char *lbl_id;
{
  register dw_attr_ref attr = (dw_attr_ref) xmalloc (sizeof (dw_attr_node));
  if (attr != NULL)
    {
      attr->dw_attr_next = NULL;
      attr->dw_attr = attr_kind;
      attr->dw_attr_val.val_class = dw_val_class_lbl_id;
      attr->dw_attr_val.v.val_lbl_id = xstrdup (lbl_id);
      add_dwarf_attr (die, attr);
    }
}

/* Add a section offset attribute value to a DIE.  */
inline void
add_AT_section_offset (die, attr_kind, section)
     register dw_die_ref die;
     register enum dwarf_attribute attr_kind;
     register char *section;
{
  register dw_attr_ref attr = (dw_attr_ref) xmalloc (sizeof (dw_attr_node));
  if (attr != NULL)
    {
      attr->dw_attr_next = NULL;
      attr->dw_attr = attr_kind;
      attr->dw_attr_val.val_class = dw_val_class_section_offset;
      attr->dw_attr_val.v.val_section = section;
      add_dwarf_attr (die, attr);
    }
}

/* Save a DIE reference attribute value to a DIE for later backchaining.  */
inline void
backchain_AT_die_ref (type, placeholder)
     register tree type;
     register dw_die_ref placeholder;
{
  register backchain_ref back = (backchain_ref) xmalloc (sizeof (backchain_t));
  if (back != NULL)
    {
      back->type = type;
      back->placeholder = placeholder;

      back->next = backchain;
      backchain = back;
    }
}

/* Test if die refers to an external subroutine.  */
inline int
is_extern_subr_die (die)
     register dw_die_ref die;
{
  register dw_attr_ref a;
  register int is_subr = FALSE;
  register int is_extern = FALSE;
  if (die != NULL && die->die_tag == DW_TAG_subprogram)
    {
      is_subr = TRUE;
      for (a = die->die_attr; a != NULL; a = a->dw_attr_next)
	{
	  if (a->dw_attr == DW_AT_external
	      && a->dw_attr_val.val_class == dw_val_class_flag
	      && a->dw_attr_val.v.val_flag != 0)
	    {
	      is_extern = TRUE;
	      break;
	    }
	}
    }
  return is_subr && is_extern;
}

/* Return the "low pc" attribute value, typically associated with
   a subprogram DIE.  Return null if the "low pc" attribute is
   either not prsent, or if it cannot be represented as an
   assembler label identifier.  */
inline char *
get_AT_low_pc (die)
     register dw_die_ref die;
{
  register dw_attr_ref a;
  register char *low_pc = NULL;
  if (die != NULL)
    {
      for (a = die->die_attr; a != NULL; a = a->dw_attr_next)
	{
	  if (a->dw_attr == DW_AT_low_pc
	      && a->dw_attr_val.val_class == dw_val_class_lbl_id)
	    {
	      low_pc = a->dw_attr_val.v.val_lbl_id;
	      break;
	    }
	}
    }
  return low_pc;
}


/* Return the "high pc" attribute value, typically associated with
   a subprogram DIE.  Return null if the "high pc" attribute is
   either not prsent, or if it cannot be represented as an
   assembler label identifier.  */
inline char *
get_AT_hi_pc (die)
     register dw_die_ref die;
{
  register dw_attr_ref a;
  register char *hi_pc = NULL;
  if (die != NULL)
    {
      for (a = die->die_attr; a != NULL; a = a->dw_attr_next)
	{
	  if (a->dw_attr == DW_AT_high_pc
	      && a->dw_attr_val.val_class == dw_val_class_lbl_id)
	    {
	      hi_pc = a->dw_attr_val.v.val_lbl_id;
	      break;
	    }
	}
    }
  return hi_pc;
}

/* Add a child DIE below its parent.  */
inline void
add_child_die (die, child_die)
     register dw_die_ref die;
     register dw_die_ref child_die;
{
  if (die != NULL && child_die != NULL)
    {
      assert (die != child_die);
      child_die->die_parent = die;
      child_die->die_sib = NULL;
      if (die->die_child == NULL)
	{
	  die->die_child = child_die;
	  die->die_child_last = child_die;
	}
      else
	{
	  die->die_child_last->die_sib = child_die;
	  die->die_child_last = child_die;
	}
    }
}

/* Return a pointer to a newly created DIE node.  */
inline dw_die_ref
new_die (tag_value, parent_die)
     register enum dwarf_tag tag_value;
     register dw_die_ref parent_die;
{
  register dw_die_ref die = (dw_die_ref) xmalloc (sizeof (die_node));
  if (die != NULL)
    {
      die->die_tag = tag_value;
      die->die_abbrev = 0;
      die->die_offset = 0;
      die->die_child = NULL;
      die->die_parent = NULL;
      die->die_sib = NULL;
      die->die_child_last = NULL;
      die->die_attr = NULL;
      die->die_attr_last = NULL;
      if (parent_die != NULL)
	{
	  add_child_die (parent_die, die);
	}
    }
  return die;
}

/* Return the DIE associated with the given type specifier.  */
inline dw_die_ref
lookup_type_die (type)
     register tree type;
{
  register unsigned type_id = TYPE_UID (type);
  return (type_id < type_die_table_in_use)
    ? type_die_table[type_id] : NULL;
}

/* Equate a DIE to a given type specifier.  */
static void
equate_type_number_to_die (type, type_die)
     register tree type;
     register dw_die_ref type_die;
{
  register unsigned type_id = TYPE_UID (type);
  register unsigned i;
  register unsigned num_allocated;
  if (type_id >= type_die_table_allocated)
    {
      num_allocated = (((type_id + 1)
			+ TYPE_DIE_TABLE_INCREMENT - 1)
		       / TYPE_DIE_TABLE_INCREMENT)
	* TYPE_DIE_TABLE_INCREMENT;
      type_die_table = (dw_die_ref *) xrealloc (type_die_table,
				       sizeof (dw_die_ref) * num_allocated);
      bzero (&type_die_table[type_die_table_allocated],
	  (num_allocated - type_die_table_allocated) * sizeof (dw_die_ref));
      type_die_table_allocated = num_allocated;
    }
  if (type_id >= type_die_table_in_use)
    {
      type_die_table_in_use = (type_id + 1);
    }
  type_die_table[type_id] = type_die;
}

/* Return the DIE associated with a given declaration.  */
inline dw_die_ref
lookup_decl_die (decl)
     register tree decl;
{
  register unsigned decl_id = DECL_UID (decl);
  return (decl_id < decl_die_table_in_use)
    ? decl_die_table[decl_id] : NULL;
}

/* Equate a DIE to a particular declaration.  */
static void
equate_decl_number_to_die (decl, decl_die)
     register tree decl;
     register dw_die_ref decl_die;
{
  register unsigned decl_id = DECL_UID (decl);
  register unsigned i;
  register unsigned num_allocated;
  if (decl_id >= decl_die_table_allocated)
    {
      num_allocated = (((decl_id + 1)
			+ DECL_DIE_TABLE_INCREMENT - 1)
		       / DECL_DIE_TABLE_INCREMENT)
	* DECL_DIE_TABLE_INCREMENT;
      decl_die_table = (dw_die_ref *) xrealloc (decl_die_table,
				       sizeof (dw_die_ref) * num_allocated);
      bzero (&decl_die_table[decl_die_table_allocated],
	  (num_allocated - decl_die_table_allocated) * sizeof (dw_die_ref));
      decl_die_table_allocated = num_allocated;
    }
  if (decl_id >= decl_die_table_in_use)
    {
      decl_die_table_in_use = (decl_id + 1);
    }
  decl_die_table[decl_id] = decl_die;
}

/* Return a pointer to a newly allocated location description.  Location
   descriptions are simple expression terms that can be strung
   together to form more complicated location (address) descriptions.  */
inline dw_loc_descr_ref
new_loc_descr (op, oprnd1, oprnd2)
     register enum dwarf_location_atom op;
     register unsigned long oprnd1;
     register unsigned long oprnd2;
{
  register dw_loc_descr_ref descr =
  (dw_loc_descr_ref) xmalloc (sizeof (dw_loc_descr_node));
  if (descr != NULL)
    {
      descr->dw_loc_next = NULL;
      descr->dw_loc_opc = op;
      descr->dw_loc_oprnd1.val_class = dw_val_class_unsigned_const;
      descr->dw_loc_oprnd1.v.val_unsigned = oprnd1;
      descr->dw_loc_oprnd2.val_class = dw_val_class_unsigned_const;
      descr->dw_loc_oprnd2.v.val_unsigned = oprnd2;
    }
  return descr;
}

/* Add a location description term to a location description expression.  */
inline void
add_loc_descr (list_head, descr)
     register dw_loc_descr_ref *list_head;
     register dw_loc_descr_ref descr;
{
  register dw_loc_descr_ref *d;
  /* find the end of the chain.  */
  for (d = list_head; (*d) != NULL; d = &(*d)->dw_loc_next)
    {
      /* nothing */ ;
    }
  *d = descr;
}

/* Return a pointer to a newly allocated Call Frame Instruction.  */
inline dw_cfi_ref
new_cfi ()
{
  register dw_cfi_ref cfi = (dw_cfi_ref) xmalloc (sizeof (dw_cfi_node));
  if (cfi != NULL)
    {
      cfi->dw_cfi_next = NULL;
      cfi->dw_cfi_oprnd1.dw_cfi_reg_num = 0;
      cfi->dw_cfi_oprnd2.dw_cfi_reg_num = 0;
    }
  return cfi;
}

/* Add a Call Frame Instruction to list of instructions.  */
inline void
add_cfi (list_head, cfi)
     register dw_cfi_ref *list_head;
     register dw_cfi_ref cfi;
{
  register dw_cfi_ref *p;
  /* find the end of the chain.  */
  for (p = list_head; (*p) != NULL; p = &(*p)->dw_cfi_next)
    {
      /* nothing */ ;
    }
  *p = cfi;
}

/********* Print DWARF Internal Representation (debugging aids) ***************/

/* Keep track of the number of spaces used to indent the
   output of the debugging routines that print the structure of
   the DIE internal representation.  */
static int print_indent;

/* Indent the line the number of spaces given by print_indent.  */
inline void
print_spaces (outfile)
     FILE *outfile;
{
  fprintf (outfile, "%*s", print_indent, "");
}

/* Print the information assoaciated with a given DIE, and its children.
   This routine is a debugging aid only.  */
static void
print_die (die, outfile)
     dw_die_ref die;
     FILE *outfile;
{
  register dw_attr_ref a;
  register dw_die_ref c;
  print_spaces (outfile);
  fprintf (outfile, "DIE %4u: %s\n",
	   die->die_offset, dwarf_tag_name (die->die_tag));
  print_spaces (outfile);
  fprintf (outfile, "  abbrev id: %u", die->die_abbrev);
  fprintf (outfile, " offset: %u\n", die->die_offset);
  for (a = die->die_attr; a != NULL; a = a->dw_attr_next)
    {
      print_spaces (outfile);
      fprintf (outfile, "  %s: ", dwarf_attr_name (a->dw_attr));
      switch (a->dw_attr_val.val_class)
	{
	case dw_val_class_addr:
	  fprintf (outfile, "address");
	  break;
	case dw_val_class_loc:
	  fprintf (outfile, "location descriptor");
	  break;
	case dw_val_class_const:
	  fprintf (outfile, "%d", a->dw_attr_val.v.val_int);
	  break;
	case dw_val_class_unsigned_const:
	  fprintf (outfile, "%u", a->dw_attr_val.v.val_unsigned);
	  break;
	case dw_val_class_double_const:
	  fprintf (outfile, "constant (%u,%u)",
		  a->dw_attr_val.v.val_dbl_const.dw_dbl_hi,
		  a->dw_attr_val.v.val_dbl_const.dw_dbl_low);
	  break;
	case dw_val_class_flag:
	  fprintf (outfile, "%u", a->dw_attr_val.v.val_flag);
	  break;
	case dw_val_class_die_ref:
	  if (a->dw_attr_val.v.val_die_ref != NULL)
	    {
	      fprintf (outfile, "die -> %u",
		       a->dw_attr_val.v.val_die_ref->die_offset);
	    }
	  else
	    {
	      fprintf (outfile, "die -> <null>");
	    }
	  break;
	case dw_val_class_lbl_id:
	  fprintf (outfile, "label: %s", a->dw_attr_val.v.val_lbl_id);
	  break;
	case dw_val_class_section_offset:
	  fprintf (outfile, "section: %s", a->dw_attr_val.v.val_section);
	  break;
	case dw_val_class_str:
	  if (a->dw_attr_val.v.val_str != NULL)
	    {
	      fprintf (outfile, "\"%s\"", a->dw_attr_val.v.val_str);
	    }
	  else
	    {
	      fprintf (outfile, "<null>");
	    }
	  break;
	}
      fprintf (outfile, "\n");
    }
  if (die->die_child != NULL)
    {
      print_indent += 4;
      for (c = die->die_child; c != NULL; c = c->die_sib)
	{
	  print_die (c, outfile);
	}
      print_indent -= 4;
    }
}

/* Print the contents of the source code line number correspondence table.
   This routine is a debugging aid only.  */
static void
print_dwarf_line_table (outfile)
     FILE *outfile;
{
  register unsigned i;
  register dw_line_info_ref line_info;
  fprintf (outfile, "\n\nDWARF source line information\n");
  for (i = 1; i < line_info_table_in_use; ++i)
    {
      line_info = &line_info_table[i];
      fprintf (outfile, "%5d: ", i);
      fprintf (outfile, "%-20s", file_table[line_info->dw_file_num]);
      fprintf (outfile, "%6d", line_info->dw_line_num);
      fprintf (outfile, "\n");
    }
  fprintf (outfile, "\n\n");
}

/* Print the information collected for a given DIE.  */
void
debug_dwarf_die (die)
     dw_die_ref die;
{
  print_die (die, stderr);
}

/* Print all DWARF informaiton collected for the compilation unit.
   This routine is a debugging aid only.  */
void
debug_dwarf ()
{
  print_indent = 0;
  print_die (comp_unit_die, stderr);
  print_dwarf_line_table (stderr);
}


/***************** DWARF Information Construction Support *********************/

/* Traverse the DIE, and add a sibling attribute if it may have the
   effect of speeding up access to siblings.  To save some space,
   avoid generating sibling attributes for DIE's without children.  */
static void
add_sibling_attributes(die)
     register dw_die_ref die;
{
  register dw_die_ref c;
  register dw_attr_ref attr;
  if (die != comp_unit_die && die->die_child != NULL)
    {
      attr = (dw_attr_ref) xmalloc (sizeof (dw_attr_node));
      if (attr != NULL)
	{
	  attr->dw_attr_next = NULL;
	  attr->dw_attr = DW_AT_sibling;
	  attr->dw_attr_val.val_class = dw_val_class_die_ref;
	  attr->dw_attr_val.v.val_die_ref = die->die_sib;
	}
      /* add the sibling link to the front of the attribute list.  */
      attr->dw_attr_next = die->die_attr;
      if (die->die_attr == NULL)
	{
	  die->die_attr_last = attr;
	}
      die->die_attr = attr;
    }
  for (c = die->die_child; c != NULL; c = c->die_sib)
    {
      add_sibling_attributes (c);
    }
}

/* The format of each DIE (and its attribute value pairs)
   is encoded in an abbreviation table.  This routine builds the
   abbreviation table and assigns a unique abbreviation id for
   each abbreviation entry.  The children of each die are visited
   recursively.  */
static void
build_abbrev_table (die)
     register dw_die_ref die;
{
  register unsigned long abbrev_id;
  register unsigned long n_alloc;
  register dw_die_ref c;
  register dw_attr_ref d_attr, a_attr;
  for (abbrev_id = 1; abbrev_id < abbrev_die_table_in_use; ++abbrev_id)
    {
      register dw_die_ref abbrev = abbrev_die_table[abbrev_id];
      if (abbrev->die_tag == die->die_tag)
	{
	  if ((abbrev->die_child != NULL) == (die->die_child != NULL))
	    {
	      a_attr = abbrev->die_attr;
	      d_attr = die->die_attr;
	      while (a_attr != NULL && d_attr != NULL)
		{
		  if ((a_attr->dw_attr != d_attr->dw_attr)
		      || (a_attr->dw_attr_val.val_class
			  != d_attr->dw_attr_val.val_class))
		    {
		      break;
		    }
		  a_attr = a_attr->dw_attr_next;
		  d_attr = d_attr->dw_attr_next;
		}
	      if (a_attr == NULL && d_attr == NULL)
		{
		  break;
		}
	    }
	}
    }
  if (abbrev_id >= abbrev_die_table_in_use)
    {
      if (abbrev_die_table_in_use >= abbrev_die_table_allocated)
	{
	  n_alloc = abbrev_die_table_allocated + ABBREV_DIE_TABLE_INCREMENT;
	  abbrev_die_table = (dw_die_ref *)
	    xmalloc (abbrev_die_table,
		     sizeof (dw_die_ref) * n_alloc);
	  bzero (&abbrev_die_table[abbrev_die_table_allocated],
	      (n_alloc - abbrev_die_table_allocated) * sizeof (dw_die_ref));
	  abbrev_die_table_allocated = n_alloc;
	}
      ++abbrev_die_table_in_use;
      abbrev_die_table[abbrev_id] = die;
    }
  die->die_abbrev = abbrev_id;
  for (c = die->die_child; c != NULL; c = c->die_sib)
    {
      build_abbrev_table (c);
    }
}


/**********************  DWARF Information Sizing *****************************/

/* Return the size of an unsigned LEB128 quantity.  */
inline unsigned long
size_of_uleb128 (value)
     register unsigned long value;
{
  register unsigned long size = 0;
  register unsigned byte;
  do
    {
      byte = (value & 0x7f);
      value >>= 7;
      size += 1;
    }
  while (value != 0);
  return size;
}

/* Return the size of a signed LEB128 quantity.  */
inline unsigned long
size_of_sleb128 (value)
     register long value;
{
  register unsigned long size = 0;
  register unsigned byte;
  do
    {
      byte = (value & 0x7f);
      value >>= 7;
      size += 1;
    }
  while (!(((value == 0) && ((byte & 0x40) == 0))
	   || ((value == -1) && ((byte & 0x40) != 0))));
  return size;
}

/* Return the size of a string, including the null byte.  */
static unsigned long
size_of_string (str)
     register char *str;
{
  register unsigned long size = 0;
  register unsigned long slen = strlen (str);
  register unsigned long i;
  register unsigned c;
  for (i = 0; i < slen; ++i)
    {
      c = str[i];
      if (c == '\\')
	{
	  ++i;
	}
      size += 1;
    }
  /* Null terminator.  */
  size += 1;
  return size;
}

/* Return the size of a location descriptor.  */
static unsigned long
size_of_loc_descr (loc)
     register dw_loc_descr_ref loc;
{
  register unsigned long size = 1;
  switch (loc->dw_loc_opc)
    {
    case DW_OP_addr:
      size += PTR_SIZE;
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
    case DW_OP_deref_size:
    case DW_OP_xderef_size:
      size += 1;
      break;
    default:
      break;
    }
  return size;
}

/* Return the size of a DIE, as it is represented in the
   .debug_info section.  */
static unsigned long
size_of_die (die)
     register dw_die_ref die;
{
  register unsigned long size = 0;
  register dw_attr_ref a;
  register dw_loc_descr_ref loc;
  size += size_of_uleb128 (die->die_abbrev);
  for (a = die->die_attr; a != NULL; a = a->dw_attr_next)
    {
      switch (a->dw_attr_val.val_class)
	{
	case dw_val_class_addr:
	  size += 4;
	  break;
	case dw_val_class_loc:
	  /* Block length.  */
	  size += 2;
	  for (loc = a->dw_attr_val.v.val_loc; loc != NULL;
	       loc = loc->dw_loc_next)
	    {
	      size += size_of_loc_descr (loc);
	    }
	  break;
	case dw_val_class_const:
	  size += 4;
	  break;
	case dw_val_class_unsigned_const:
	  size += 4;
	  break;
	case dw_val_class_double_const:
	  size += 8;
	  break;
	case dw_val_class_flag:
	  size += 1;
	  break;
	case dw_val_class_die_ref:
	  size += 4;
	  break;
	case dw_val_class_fde_ref:
	  size += 4;
	  break;
	case dw_val_class_lbl_id:
	  size += 4;
	  break;
	case dw_val_class_section_offset:
	  size += 4;
	  break;
	case dw_val_class_str:
	  size += size_of_string (a->dw_attr_val.v.val_str);
	  break;
	default:
	  abort ();
	}
    }
  return size;
}

/* Size the debgging information associted with a given DIE.
   Visits the DIE's children recursively.  Updates the global
   variable next_die_offset, on each time through.  Uses the
   current value of next_die_offset to updete the die_offset
   field in each DIE.  */
static void
calc_die_sizes (die)
     dw_die_ref die;
{
  register dw_die_ref c;
  register unsigned long die_size;
  die->die_offset = next_die_offset;
  next_die_offset += size_of_die (die);
  for (c = die->die_child; c != NULL; c = c->die_sib)
    {
      calc_die_sizes (c);
    }
  if (die->die_child != NULL)
    {
      /* Count the null byte used to terminate sibling lists.  */
      next_die_offset += 1;
    }
}

/* Return the size of the line information prolog generated for the
   compilation unit.  */
static unsigned long
size_of_line_prolog ()
{
  register unsigned long size;
  register unsigned opc;
  register unsigned n_op_args;
  register unsigned long ft_index;
  size = DWARF_LINE_PROLOG_HEADER_SIZE;
  /* Count the size of the table giving number of args for each
     standard opcode.  */
  size += DWARF_LINE_OPCODE_BASE - 1;
  /* Include directory table is empty (at present).  Count only the
     the null byte used to terminate the table.  */
  size += 1;
  for (ft_index = 1; ft_index < file_table_in_use; ++ft_index)
    {
      /* File name entry.  */
      size += size_of_string (file_table[ft_index]);
      /* Include directory index.  */
      size += size_of_uleb128 (0);
      /* Modification time.  */
      size += size_of_uleb128 (0);
      /* File length in bytes.  */
      size += size_of_uleb128 (0);
    }
  /* Count the file table terminator.  */
  size += 1;
  return size;
}

/* Return the size of the line information generated for this
   compilation unit.  */
static unsigned long
size_of_line_info ()
{
  register unsigned long size;
  register dw_line_info_ref line_info;
  register unsigned long lt_index;
  register unsigned long current_line;
  register long line_offset;
  register long line_delta;
  register unsigned long current_file;
  /* Version number.  */
  size = 2;
  /* Prolog length specifier.  */
  size += 4;
  /* Prolog.  */
  size += size_of_line_prolog ();
  /* Set address register instruction.  */
  size += 1 + size_of_uleb128 (1 + PTR_SIZE)
    + 1 + PTR_SIZE;
  current_file = 1;
  current_line = 1;
  for (lt_index = 1; lt_index < line_info_table_in_use; ++lt_index)
    {
      /* Advance pc instruction.  */
      size += 1 + 2;
      line_info = &line_info_table[lt_index];
      if (line_info->dw_file_num != current_file)
	{
	  /* Set file number instruction.  */
	  size += 1;
	  current_file = line_info->dw_file_num;
	  size += size_of_uleb128 (current_file);
	}
      if (line_info->dw_line_num != current_line)
	{
	  line_offset = line_info->dw_line_num - current_line;
	  line_delta = line_offset - DWARF_LINE_BASE;
	  current_line = line_info->dw_line_num;
	  if (line_delta >= 0 && line_delta < (DWARF_LINE_RANGE - 1))
	    {
	      /* 1-byte special line number instruction.  */
	      size += 1;
	    }
	  else
	    {
	      /* Advance line instruction.  */
	      size += 1;
	      size += size_of_sleb128 (line_offset);
	      /* Generate line entry instruction.  */
	      size += 1;
	    }
	}
    }
  /* Set address register instruction.  */
  size += 1 + size_of_uleb128 (1 + PTR_SIZE)
    + 1 + PTR_SIZE;
  /* End of line number info. marker.  */
  size += 1 + size_of_uleb128 (1) + 1;
  return size;
}

/* Return the size of the .debug_pubnames table  generated for the
   compilation unit.  */
static unsigned long
size_of_pubnames ()
{
  dw_die_ref die;
  register unsigned long size;
  size = DWARF_PUBNAMES_HEADER_SIZE;
  for (die = comp_unit_die->die_child; die != NULL; die = die->die_sib)
    {
      if (is_extern_subr_die (die))
	{
	  char *low_pc = get_AT_low_pc (die);
	  if (low_pc != NULL)
	    {
	      size += 4;
	      size += size_of_string (low_pc);
	    }
	}
    }
  size += 4;
  return size;
}

/* Return the size of the information in the .debug_aranges seciton.  */
static unsigned long
size_of_aranges ()
{
  register unsigned long size;
  size = DWARF_ARANGES_HEADER_SIZE;
  /* Count the address/length pair for this compilation unit.  */
  size += 8;
  /* Count the two zero words used to terminated the address range table.  */
  size += 8;
  return size;
}

/**************** DWARF Debug Information Output *****************************/

/* Output an unsigned LEB128 quantity.  */
static void
output_uleb128 (value)
     register unsigned long value;
{
  fprintf (asm_out_file, "\t%s\t", ASM_BYTE_OP);
  do
    {
      register unsigned byte = (value & 0x7f);
      value >>= 7;
      if (value != 0)
	{
	  /* More bytes to follow.  */
	  byte |= 0x80;
	}
      fprintf (asm_out_file, "0x%x", byte);
      if (value != 0)
	{
	  fprintf (asm_out_file, ",");
	}
    }
  while (value != 0);
}

/* Output an signed LEB128 quantity.  */
static void
output_sleb128 (value)
     register long value;
{
  register int more;
  register unsigned byte;
  fprintf (asm_out_file, "\t%s\t", ASM_BYTE_OP);
  do
    {
      byte = (value & 0x7f);
      /* arithmetic shift */
      value >>= 7;
      more = !((((value == 0) && ((byte & 0x40) == 0))
		|| ((value == -1) && ((byte & 0x40) != 0))));
      if (more)
	{
	  byte |= 0x80;
	}
      fprintf (asm_out_file, "0x%x", byte);
      if (more)
	{
	  fprintf (asm_out_file, ",");
	}
    }
  while (more);
}

/* Output the encoding of an attribute value.  */
static void
output_value_format (v)
     dw_val_ref v;
{
  enum dwarf_form form;
  switch (v->val_class)
    {
    case dw_val_class_addr:
      form = DW_FORM_addr;
      break;
    case dw_val_class_loc:
      form = DW_FORM_block2;
      break;
    case dw_val_class_const:
      form = DW_FORM_data4;
      break;
    case dw_val_class_unsigned_const:
      form = DW_FORM_data4;
      break;
    case dw_val_class_double_const:
      form = DW_FORM_data8;
      break;
    case dw_val_class_flag:
      form = DW_FORM_flag;
      break;
    case dw_val_class_die_ref:
      form = DW_FORM_ref4;
      break;
    case dw_val_class_fde_ref:
      form = DW_FORM_data4;
      break;
    case dw_val_class_lbl_id:
      form = DW_FORM_addr;
      break;
    case dw_val_class_section_offset:
      form = DW_FORM_data4;
      break;
    case dw_val_class_str:
      form = DW_FORM_string;
      break;
    default:
      abort ();
    }
  output_uleb128 (form);
  if (flag_verbose_asm)
    {
      fprintf (asm_out_file, "\t%s %s",
	       ASM_COMMENT_START, dwarf_form_name (form));
    }
  fputc ('\n', asm_out_file);
}

/* Output the .debug_abbrev section which defines the DIE abbreviation
   table.  */
static void
output_abbrev_section ()
{
  unsigned long abbrev_id;
  dw_attr_ref a_attr;
  for (abbrev_id = 1; abbrev_id < abbrev_die_table_in_use; ++abbrev_id)
    {
      register dw_die_ref abbrev = abbrev_die_table[abbrev_id];
      output_uleb128 (abbrev_id);
      if (flag_verbose_asm)
	{
	  fprintf (asm_out_file, "\t%s abbrev code = %u",
		   ASM_COMMENT_START, abbrev_id);
	}
      fputc ('\n', asm_out_file);
      output_uleb128 (abbrev->die_tag);
      if (flag_verbose_asm)
	{
	  fprintf (asm_out_file, "\t%s TAG: %s",
		   ASM_COMMENT_START, dwarf_tag_name (abbrev->die_tag));
	}
      fputc ('\n', asm_out_file);
      fprintf (asm_out_file, "\t%s\t0x%x", ASM_BYTE_OP,
	       (abbrev->die_child != NULL)
	       ? DW_children_yes : DW_children_no);
      if (flag_verbose_asm)
	{
	  fprintf (asm_out_file, "\t%s %s",
		   ASM_COMMENT_START,
		   (abbrev->die_child != NULL)
		   ? "DW_children_yes" : "DW_children_no");
	}
      fputc ('\n', asm_out_file);
      for (a_attr = abbrev->die_attr; a_attr != NULL;
	   a_attr = a_attr->dw_attr_next)
	{
	  output_uleb128 (a_attr->dw_attr);
	  if (flag_verbose_asm)
	    {
	      fprintf (asm_out_file, "\t%s %s",
		       ASM_COMMENT_START,
		       dwarf_attr_name (a_attr->dw_attr));
	    }
	  fputc ('\n', asm_out_file);
	  output_value_format (&a_attr->dw_attr_val);
	}
      fprintf (asm_out_file, "\t%s\t0,0\n", ASM_BYTE_OP);
    }
}

/* Output location description stack opcode's operands (if any).  */
static void
output_loc_operands (loc)
     register dw_loc_descr_ref loc;
{
  register dw_val_ref val1 = &loc->dw_loc_oprnd1;
  register dw_val_ref val2 = &loc->dw_loc_oprnd2;
  switch (loc->dw_loc_opc)
    {
    case DW_OP_addr:
      ASM_OUTPUT_DWARF_ADDR_CONST (asm_out_file, val1->v.val_addr);
      fputc ('\n', asm_out_file);
      break;
    case DW_OP_const1u:
    case DW_OP_const1s:
      ASM_OUTPUT_DWARF_DATA1 (asm_out_file, val1->v.val_flag);
      fputc ('\n', asm_out_file);
      break;
    case DW_OP_const2u:
    case DW_OP_const2s:
      ASM_OUTPUT_DWARF_DATA2 (asm_out_file, val1->v.val_int);
      fputc ('\n', asm_out_file);
      break;
    case DW_OP_const4u:
    case DW_OP_const4s:
      ASM_OUTPUT_DWARF_DATA4 (asm_out_file, val1->v.val_int);
      fputc ('\n', asm_out_file);
      break;
    case DW_OP_const8u:
    case DW_OP_const8s:
      ASM_OUTPUT_DWARF_DATA8 (asm_out_file,
			      val1->v.val_dbl_const.dw_dbl_hi,
			      val2->v.val_dbl_const.dw_dbl_low);
      fputc ('\n', asm_out_file);
      break;
    case DW_OP_constu:
      output_uleb128 (val1->v.val_unsigned);
      fputc ('\n', asm_out_file);
      break;
    case DW_OP_consts:
      output_sleb128 (val1->v.val_unsigned);
      fputc ('\n', asm_out_file);
      break;
    case DW_OP_pick:
      ASM_OUTPUT_DWARF_DATA1 (asm_out_file, val1->v.val_int);
      fputc ('\n', asm_out_file);
      break;
    case DW_OP_plus_uconst:
      output_uleb128 (val1->v.val_unsigned);
      fputc ('\n', asm_out_file);
      break;
    case DW_OP_skip:
    case DW_OP_bra:
      ASM_OUTPUT_DWARF_DATA2 (asm_out_file, val1->v.val_int);
      fputc ('\n', asm_out_file);
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
      output_sleb128 (val1->v.val_int);
      fputc ('\n', asm_out_file);
      break;
    case DW_OP_regx:
      output_uleb128 (val1->v.val_unsigned);
      fputc ('\n', asm_out_file);
      break;
    case DW_OP_fbreg:
      output_sleb128 (val1->v.val_unsigned);
      fputc ('\n', asm_out_file);
      break;
    case DW_OP_bregx:
      output_uleb128 (val1->v.val_unsigned);
      fputc ('\n', asm_out_file);
      output_sleb128 (val2->v.val_unsigned);
      fputc ('\n', asm_out_file);
      break;
    case DW_OP_piece:
      output_uleb128 (val1->v.val_unsigned);
      fputc ('\n', asm_out_file);
      break;
    case DW_OP_deref_size:
    case DW_OP_xderef_size:
      ASM_OUTPUT_DWARF_DATA1 (asm_out_file, val1->v.val_flag);
      fputc ('\n', asm_out_file);
      break;
    default:
      break;
    }
}

/* Compute the offset of a sibling.  */
static unsigned long
sibling_offset (die)
     dw_die_ref die;
{
  unsigned long offset;
  if (die->die_child_last == NULL)
    {
      offset = die->die_offset + size_of_die (die);
    }
  else
    {
      offset = sibling_offset (die->die_child_last) + 1;
    }
  return offset;
}

/* Output the DIE and its attributes.  Called recursively to generate
   the definitions of each child DIE.  */
static void
output_die (die)
     register dw_die_ref die;
{
  register dw_attr_ref a;
  register dw_die_ref c;
  register unsigned long ref_offset;
  register unsigned long size;
  register dw_loc_descr_ref loc;
  output_uleb128 (die->die_abbrev);
  if (flag_verbose_asm)
    {
      fprintf (asm_out_file, "\t%s DIE (0x%x) %s",
	       ASM_COMMENT_START,
	       die->die_offset,
	       dwarf_tag_name (die->die_tag));
    }
  fputc ('\n', asm_out_file);
  for (a = die->die_attr; a != NULL; a = a->dw_attr_next)
    {
      switch (a->dw_attr_val.val_class)
	{
	case dw_val_class_addr:
	  ASM_OUTPUT_DWARF_ADDR_CONST (asm_out_file,
				       a->dw_attr_val.v.val_addr);
	  break;
	case dw_val_class_loc:
	  size = 0;
	  for (loc = a->dw_attr_val.v.val_loc; loc != NULL;
	       loc = loc->dw_loc_next)
	    {
	      size += size_of_loc_descr (loc);
	    }
	  /* Output the block length for this list of location operations.  */
	  ASM_OUTPUT_DWARF_DATA2 (asm_out_file, size);
	  if (flag_verbose_asm)
	    {
	      fprintf (asm_out_file, "\t%s %s",
		       ASM_COMMENT_START, dwarf_attr_name (a->dw_attr));
	    }
	  fputc ('\n', asm_out_file);
	  for (loc = a->dw_attr_val.v.val_loc; loc != NULL;
	       loc = loc->dw_loc_next)
	    {
	      /* Output the opcode.  */
	      ASM_OUTPUT_DWARF_DATA1 (asm_out_file, loc->dw_loc_opc);
	      if (flag_verbose_asm)
		{
		  fprintf (asm_out_file, "\t%s %s",
			   ASM_COMMENT_START,
			   dwarf_stack_op_name (loc->dw_loc_opc));
		}
	      fputc ('\n', asm_out_file);
	      /* Output the operand(s) (if any).  */
	      output_loc_operands (loc);
	    }
	  break;
	case dw_val_class_const:
	  ASM_OUTPUT_DWARF_DATA4 (asm_out_file, a->dw_attr_val.v.val_int);
	  break;
	case dw_val_class_unsigned_const:
	  ASM_OUTPUT_DWARF_DATA4 (asm_out_file, a->dw_attr_val.v.val_unsigned);
	  break;
	case dw_val_class_double_const:
	  ASM_OUTPUT_DWARF_DATA8 (asm_out_file,
				  a->dw_attr_val.v.val_dbl_const.dw_dbl_hi,
				  a->dw_attr_val.v.val_dbl_const.dw_dbl_low);
	  break;
	case dw_val_class_flag:
	  ASM_OUTPUT_DWARF_DATA1 (asm_out_file, a->dw_attr_val.v.val_flag);
	  break;
	case dw_val_class_die_ref:
	  if (a->dw_attr_val.v.val_die_ref != NULL)
	    {
	      ref_offset = a->dw_attr_val.v.val_die_ref->die_offset;
	    }
	  else if (a->dw_attr == DW_AT_sibling)
	    {
	      ref_offset = sibling_offset(die);
	    }
	  else
	    {
	      abort ();
	    }
	  ASM_OUTPUT_DWARF_DATA4 (asm_out_file, ref_offset);
	  break;
	case dw_val_class_fde_ref:
	  ref_offset = fde_table[a->dw_attr_val.v.val_fde_index].dw_fde_offset;
	  fprintf (asm_out_file, "\t%s\t%s+0x%x", UNALIGNED_INT_ASM_OP,
		   stripattributes (FRAME_SECTION), ref_offset);
	  break;
	case dw_val_class_lbl_id:
	  ASM_OUTPUT_DWARF_ADDR (asm_out_file, a->dw_attr_val.v.val_lbl_id);
	  break;
	case dw_val_class_section_offset:
	  ASM_OUTPUT_DWARF_ADDR (asm_out_file,
				 stripattributes (a->dw_attr_val.v.val_section));
	  break;
	case dw_val_class_str:
	  ASM_OUTPUT_DWARF_STRING (asm_out_file, a->dw_attr_val.v.val_str);
	  break;
	default:
	  abort ();
	}
      if (a->dw_attr_val.val_class != dw_val_class_loc)
	{
	  if (flag_verbose_asm)
	    {
	      fprintf (asm_out_file, "\t%s %s",
		       ASM_COMMENT_START, dwarf_attr_name (a->dw_attr));
	    }
	  fputc ('\n', asm_out_file);
	}
    }
  for (c = die->die_child; c != NULL; c = c->die_sib)
    {
      output_die (c);
    }
  if (die->die_child != NULL)
    {
      /* Add null byte to terminate sibling list. */
      ASM_OUTPUT_DWARF_DATA1 (asm_out_file, 0);
      fputc ('\n', asm_out_file);
    }
}

/* Output the compilation unit that appears at the beginning of the
   .debug_info section, and precedes the DIE descriptions.  */
static void
output_compilation_unit_header ()
{
  /* ??? The dwarf standard says this must be a 4 byte integer, but the
     SGI dwarf reader assumes this is the same size as a pointer.  */
  fprintf (asm_out_file, "\t%s\t0x%x",
	   UNALIGNED_INT_ASM_OP, next_die_offset - 4);
  if (flag_verbose_asm)
    {
      fprintf (asm_out_file, "\t%s Length of Compilation Unit Info.",
	       ASM_COMMENT_START);
    }
  fputc ('\n', asm_out_file);
  fprintf (asm_out_file, "\t%s\t0x%x", UNALIGNED_SHORT_ASM_OP, DWARF_VERSION);
  if (flag_verbose_asm)
    {
      fprintf (asm_out_file, "\t%s DWARF version number",
	       ASM_COMMENT_START);
    }
  fputc ('\n', asm_out_file);
  fprintf (asm_out_file, "\t%s\t%s", UNALIGNED_INT_ASM_OP,
	   stripattributes (ABBREV_SECTION));
  if (flag_verbose_asm)
    {
      fprintf (asm_out_file, "\t%s Offset Into Abbrev. Section",
	       ASM_COMMENT_START);
    }
  fputc ('\n', asm_out_file);
  fprintf (asm_out_file, "\t%s\t0x%x", ASM_BYTE_OP, PTR_SIZE);
  if (flag_verbose_asm)
    {
      fprintf (asm_out_file, "\t%s Pointer Size (in bytes)",
	       ASM_COMMENT_START);
    }
  fputc ('\n', asm_out_file);
}

/* Return the size of a Call Frame Instruction.  */
static unsigned long
size_of_cfi (cfi)
     dw_cfi_ref cfi;
{
  register unsigned long size;
  /* count the 1-byte opcode */
  size = 1;
  switch (cfi->dw_cfi_opc)
    {
    case DW_CFA_offset:
      size += size_of_uleb128(cfi->dw_cfi_oprnd2.dw_cfi_offset);
      break;
    case DW_CFA_set_loc:
      size += PTR_SIZE;
      break;
    case DW_CFA_advance_loc1:
      size += 1;
      break;
    case DW_CFA_advance_loc2:
      size += 2;
      break;
    case DW_CFA_advance_loc4:
      size += 4;
      break;
#ifdef MIPS_DEBUGGING_INFO
    case DW_CFA_MIPS_advance_loc8:
      size += 8;
      break;
#endif
    case DW_CFA_offset_extended:
    case DW_CFA_def_cfa:
      size += size_of_uleb128(cfi->dw_cfi_oprnd1.dw_cfi_reg_num);
      size += size_of_uleb128(cfi->dw_cfi_oprnd2.dw_cfi_offset);
      break;
    case DW_CFA_restore_extended:
    case DW_CFA_undefined:
      size += size_of_uleb128(cfi->dw_cfi_oprnd1.dw_cfi_reg_num);
      break;
    case DW_CFA_same_value:
    case DW_CFA_def_cfa_register:
      size += size_of_uleb128(cfi->dw_cfi_oprnd1.dw_cfi_reg_num);
      break;
    case DW_CFA_register:
      size += size_of_uleb128(cfi->dw_cfi_oprnd1.dw_cfi_reg_num);
      size += size_of_uleb128(cfi->dw_cfi_oprnd2.dw_cfi_reg_num);
      break;
    case DW_CFA_def_cfa_offset:
      size += size_of_uleb128(cfi->dw_cfi_oprnd1.dw_cfi_offset);
      break;
    default:
      break;
    }
    return size;
}

/* Return the size of an FDE sans the length word.  */
inline unsigned long
size_of_fde (fde, npad)
    dw_fde_ref fde;
    unsigned long *npad;
{
  register dw_cfi_ref cfi;
  register unsigned long aligned_size;
  register unsigned long size;
  size = DWARF_FDE_HEADER_SIZE;
  for (cfi = fde->dw_fde_cfi; cfi != NULL; cfi = cfi->dw_cfi_next)
    {
	size += size_of_cfi(cfi);
    }
  /* Round the size up to an 8 byte boundary.  */
  aligned_size = (size + 7) & ~7;
  *npad = aligned_size - size;
  return aligned_size;
}

/* Calculate the size of the FDE table, and establish the offset
   of each FDE in the .debug_frame section.  */
static void
calc_fde_sizes ()
{
  register unsigned long i;
  register dw_fde_ref fde;
  register unsigned long fde_size;
  unsigned long fde_pad;
  for (i = 0; i < fde_table_in_use; ++i)
    {
      fde = &fde_table[i];
      fde->dw_fde_offset = next_fde_offset;
      fde_size = size_of_fde (fde, &fde_pad);
      next_fde_offset += fde_size;
    }
}

/* Output a Call Frame Information opcode and its operand(s).  */
static void
output_cfi (cfi, fde)
     register dw_cfi_ref cfi;
     register dw_fde_ref fde;
{
  if (cfi->dw_cfi_opc == DW_CFA_advance_loc)
    {
      ASM_OUTPUT_DWARF_DATA1 (asm_out_file,
			      cfi->dw_cfi_opc
			      | (cfi->dw_cfi_oprnd1.dw_cfi_offset & 0x3f));
      if (flag_verbose_asm)
	{
	  fprintf (asm_out_file, "\t%s DW_CFA_advance_loc", ASM_COMMENT_START);
	}
      fputc ('\n', asm_out_file);
    }
  else if (cfi->dw_cfi_opc == DW_CFA_offset)
    {
      ASM_OUTPUT_DWARF_DATA1 (asm_out_file,
			      cfi->dw_cfi_opc
			      | (cfi->dw_cfi_oprnd1.dw_cfi_reg_num & 0x3f));
      if (flag_verbose_asm)
	{
	  fprintf (asm_out_file, "\t%s DW_CFA_offset", ASM_COMMENT_START);
	}
      fputc ('\n', asm_out_file);
      output_uleb128(cfi->dw_cfi_oprnd2.dw_cfi_offset);
      fputc ('\n', asm_out_file);
    }
  else if (cfi->dw_cfi_opc == DW_CFA_restore)
    {
      ASM_OUTPUT_DWARF_DATA1 (asm_out_file,
			      cfi->dw_cfi_opc
			      | (cfi->dw_cfi_oprnd1.dw_cfi_reg_num & 0x3f));
      if (flag_verbose_asm)
	{
	  fprintf (asm_out_file, "\t%s DW_CFA_restore", ASM_COMMENT_START);
	}
      fputc ('\n', asm_out_file);
    }
  else
    {
      ASM_OUTPUT_DWARF_DATA1 (asm_out_file, cfi->dw_cfi_opc);
      if (flag_verbose_asm)
	{
	  fprintf (asm_out_file, "\t%s %s",
		   ASM_COMMENT_START,
		   dwarf_cfi_name (cfi->dw_cfi_opc));
	}
      fputc ('\n', asm_out_file);
      switch (cfi->dw_cfi_opc)
	{
	case DW_CFA_set_loc:
          ASM_OUTPUT_DWARF_ADDR (asm_out_file,
				 cfi->dw_cfi_oprnd1.dw_cfi_addr);
          fputc ('\n', asm_out_file);
	  break;
	case DW_CFA_advance_loc1:
	  /* TODO: not currently implemented.  */
	  abort ();
	  break;
	case DW_CFA_advance_loc2:
          ASM_OUTPUT_DWARF_DELTA2 (asm_out_file,
				 cfi->dw_cfi_oprnd1.dw_cfi_addr,
      				 fde->dw_fde_begin);
          fputc ('\n', asm_out_file);
	  break;
	case DW_CFA_advance_loc4:
          ASM_OUTPUT_DWARF_DELTA4 (asm_out_file,
				 cfi->dw_cfi_oprnd1.dw_cfi_addr,
      				 fde->dw_fde_begin);
          fputc ('\n', asm_out_file);
	  break;
#ifdef MIPS_DEBUGGING_INFO
	case DW_CFA_MIPS_advance_loc8:
	  /* TODO: not currently implemented.  */
	  abort ();
	  break;
#endif
	case DW_CFA_offset_extended:
	case DW_CFA_def_cfa:
	  output_uleb128(cfi->dw_cfi_oprnd1.dw_cfi_reg_num);
          fputc ('\n', asm_out_file);
	  output_uleb128(cfi->dw_cfi_oprnd2.dw_cfi_offset);
          fputc ('\n', asm_out_file);
	  break;
	case DW_CFA_restore_extended:
	case DW_CFA_undefined:
	  output_uleb128(cfi->dw_cfi_oprnd1.dw_cfi_reg_num);
          fputc ('\n', asm_out_file);
	  break;
	case DW_CFA_same_value:
	case DW_CFA_def_cfa_register:
	  output_uleb128(cfi->dw_cfi_oprnd1.dw_cfi_reg_num);
          fputc ('\n', asm_out_file);
	  break;
	case DW_CFA_register:
	  output_uleb128(cfi->dw_cfi_oprnd1.dw_cfi_reg_num);
          fputc ('\n', asm_out_file);
	  output_uleb128(cfi->dw_cfi_oprnd2.dw_cfi_reg_num);
          fputc ('\n', asm_out_file);
	  break;
	case DW_CFA_def_cfa_offset:
	  output_uleb128(cfi->dw_cfi_oprnd1.dw_cfi_offset);
          fputc ('\n', asm_out_file);
	  break;
	default:
	  break;
	}
     }
}

/* Output the call frame information used to used to record information
   that relates to calculating the frame pointer, and records the
   location of saved registers.  */
static void
output_call_frame_info ()
{
  register unsigned long i, j;
  register dw_fde_ref fde;
  register unsigned long fde_size;
  dw_cfi_node cfi_node;
  register dw_cfi_ref cfi;
  unsigned long fde_pad;

  /* Output the CIE. */
  ASM_OUTPUT_DWARF_DATA4 (asm_out_file, DWARF_CIE_SIZE - 4);
  if (flag_verbose_asm)
    {
      fprintf (asm_out_file, "\t%s Length of Common Information Entry",
	       ASM_COMMENT_START);
    }
  fputc ('\n', asm_out_file);
  ASM_OUTPUT_DWARF_DATA4 (asm_out_file, DW_CIE_ID);
  if (flag_verbose_asm)
    {
      fprintf (asm_out_file, "\t%s CIE Identifier Tag",
	       ASM_COMMENT_START);
    }
  fputc ('\n', asm_out_file);
  ASM_OUTPUT_DWARF_DATA1 (asm_out_file, DW_CIE_VERSION);
  if (flag_verbose_asm)
    {
      fprintf (asm_out_file, "\t%s CIE Version",
	       ASM_COMMENT_START);
    }
  fputc ('\n', asm_out_file);
  ASM_OUTPUT_DWARF_DATA1 (asm_out_file, 0);
  if (flag_verbose_asm)
    {
      fprintf (asm_out_file, "\t%s CIE Augmentation (none)",
	       ASM_COMMENT_START);
    }
  fputc ('\n', asm_out_file);
  output_uleb128 (1);
  if (flag_verbose_asm)
    {
      fprintf (asm_out_file, "\t%s CIE Code Alignment Factor",
	       ASM_COMMENT_START);
    }
  fputc ('\n', asm_out_file);
  output_sleb128 (DWARF_CIE_DATA_ALIGNMENT);
  if (flag_verbose_asm)
    {
      fprintf (asm_out_file, "\t%s CIE Data Alignment Factor",
	       ASM_COMMENT_START);
    }
  fputc ('\n', asm_out_file);
  ASM_OUTPUT_DWARF_DATA1 (asm_out_file, DW_FRAME_RA_COL);
  if (flag_verbose_asm)
    {
      fprintf (asm_out_file, "\t%s CIE RA Column",
	       ASM_COMMENT_START);
    }
  fputc ('\n', asm_out_file);

  /* Output the CFA instructions common to all FDE's. */

#ifdef MIPS_DEBUGGING_INFO

  /* Set the RA on entry to be the contents of r31.  */
  bzero (&cfi_node, sizeof (dw_cfi_node));
  cfi = &cfi_node;
  cfi->dw_cfi_opc = DW_CFA_register;
  cfi->dw_cfi_oprnd1.dw_cfi_reg_num = DW_FRAME_RA_COL;
  cfi->dw_cfi_oprnd2.dw_cfi_reg_num = DW_FRAME_REG31;
  output_cfi (cfi);

#endif

  /* Pad the CIE out to an address sized boundary.  */
  for (i = DWARF_CIE_HEADER_SIZE; i < DWARF_CIE_SIZE; ++i)
    {
      /* Pad out to a pointer size boundary */
      ASM_OUTPUT_DWARF_DATA1 (asm_out_file, DW_CFA_nop);
      if (flag_verbose_asm)
	{
	  fprintf (asm_out_file, "\t%s CIE DW_CFA_nop (pad)",
		   ASM_COMMENT_START);
	}
      fputc ('\n', asm_out_file);
    }

  /* Loop through all of the FDE's.  */
  for (i = 0; i < fde_table_in_use; ++i)
    {
      fde = &fde_table[i];
      fde_size = size_of_fde (fde, &fde_pad);
      ASM_OUTPUT_DWARF_DATA4 (asm_out_file, fde_size - 4);
      if (flag_verbose_asm)
	{
	  fprintf (asm_out_file, "\t%s FDE Length",
		   ASM_COMMENT_START);
	}
      fputc ('\n', asm_out_file);
      ASM_OUTPUT_DWARF_ADDR (asm_out_file, stripattributes (FRAME_SECTION));
      if (flag_verbose_asm)
	{
	  fprintf (asm_out_file, "\t%s FDE CIE offset",
		   ASM_COMMENT_START);
	}
      fputc ('\n', asm_out_file);
      ASM_OUTPUT_DWARF_ADDR (asm_out_file, fde->dw_fde_begin);
      if (flag_verbose_asm)
	{
	  fprintf (asm_out_file, "\t%s FDE initial location",
		   ASM_COMMENT_START);
	}
      fputc ('\n', asm_out_file);
      ASM_OUTPUT_DWARF_DELTA4 (asm_out_file,
			       fde->dw_fde_end, fde->dw_fde_begin);
      if (flag_verbose_asm)
	{
	  fprintf (asm_out_file, "\t%s FDE address range",
		   ASM_COMMENT_START);
	}
      fputc ('\n', asm_out_file);

      /* Loop through the Call Frame Instructions associated with
	 this FDE.  */
      for (cfi = fde->dw_fde_cfi; cfi != NULL; cfi = cfi->dw_cfi_next)
	{
	  output_cfi (cfi, fde);
	}

      /* Pad to a double word boundary.  */
      for (j = 0; j < fde_pad; ++j)
	{
	  ASM_OUTPUT_DWARF_DATA1 (asm_out_file, DW_CFA_nop);
	  if (flag_verbose_asm)
	    {
	      fprintf (asm_out_file, "\t%s CIE DW_CFA_nop (pad)",
		       ASM_COMMENT_START);
	    }
	  fputc ('\n', asm_out_file);
	}
    }
}

/* Output the public names table used to speed up access to externally
   visible names.  For now, only generate entries for externally
   visible procedures.  */
static void
output_pubnames ()
{
  dw_die_ref die;
  register unsigned long pubnames_length = size_of_pubnames ();
  ASM_OUTPUT_DWARF_DATA4 (asm_out_file, pubnames_length);
  if (flag_verbose_asm)
    {
      fprintf (asm_out_file, "\t%s Length of Public Names Info.",
	       ASM_COMMENT_START);
    }
  fputc ('\n', asm_out_file);
  ASM_OUTPUT_DWARF_DATA2 (asm_out_file, DWARF_VERSION);
  if (flag_verbose_asm)
    {
      fprintf (asm_out_file, "\t%s DWARF Version",
	       ASM_COMMENT_START);
    }
  fputc ('\n', asm_out_file);
  ASM_OUTPUT_DWARF_ADDR (asm_out_file, stripattributes (DEBUG_SECTION));
  if (flag_verbose_asm)
    {
      fprintf (asm_out_file, "\t%s Offset of Compilation Unit Info.",
	       ASM_COMMENT_START);
    }
  fputc ('\n', asm_out_file);
  ASM_OUTPUT_DWARF_DATA4 (asm_out_file, next_die_offset);
  if (flag_verbose_asm)
    {
      fprintf (asm_out_file, "\t%s Compilation Unit Length",
	       ASM_COMMENT_START);
    }
  fputc ('\n', asm_out_file);
  for (die = comp_unit_die->die_child; die != NULL; die = die->die_sib)
    {
      if (is_extern_subr_die (die))
	{
	  char *low_pc = get_AT_low_pc (die);
	  if (low_pc != NULL)
	    {
	      ASM_OUTPUT_DWARF_DATA4 (asm_out_file, die->die_offset);
	      if (flag_verbose_asm)
		{
		  fprintf (asm_out_file, "\t%s DIE offset",
			   ASM_COMMENT_START);
		}
	      fputc ('\n', asm_out_file);
	      ASM_OUTPUT_DWARF_STRING (asm_out_file, low_pc);
	      if (flag_verbose_asm)
		{
		  fprintf (asm_out_file, "%s external name",
			   ASM_COMMENT_START);
		}
	      fputc ('\n', asm_out_file);
	    }
	}
    }
  ASM_OUTPUT_DWARF_DATA4 (asm_out_file, 0);
  fputc ('\n', asm_out_file);
}

/* Output the information that goes into the .debug_aranges table.
   Namely, define the beginning and ending address range of the
   text section generated for this compilation unit.  */
static void
output_aranges ()
{
  dw_die_ref die;
  register unsigned long aranges_length = size_of_aranges ();
  ASM_OUTPUT_DWARF_DATA4 (asm_out_file, aranges_length);
  if (flag_verbose_asm)
    {
      fprintf (asm_out_file, "\t%s Length of Address Ranges Info.",
	       ASM_COMMENT_START);
    }
  fputc ('\n', asm_out_file);
  ASM_OUTPUT_DWARF_DATA2 (asm_out_file, DWARF_VERSION);
  if (flag_verbose_asm)
    {
      fprintf (asm_out_file, "\t%s DWARF Version",
	       ASM_COMMENT_START);
    }
  fputc ('\n', asm_out_file);
  ASM_OUTPUT_DWARF_ADDR (asm_out_file, stripattributes (DEBUG_SECTION));
  if (flag_verbose_asm)
    {
      fprintf (asm_out_file, "\t%s Offset of Compilation Unit Info.",
	       ASM_COMMENT_START);
    }
  fputc ('\n', asm_out_file);
  ASM_OUTPUT_DWARF_DATA1 (asm_out_file, PTR_SIZE);
  if (flag_verbose_asm)
    {
      fprintf (asm_out_file, "\t%s Size of Address",
	       ASM_COMMENT_START);
    }
  fputc ('\n', asm_out_file);
  ASM_OUTPUT_DWARF_DATA1 (asm_out_file, 0);
  if (flag_verbose_asm)
    {
      fprintf (asm_out_file, "\t%s Size of Segment Descriptor",
	       ASM_COMMENT_START);
    }
  fputc ('\n', asm_out_file);
  ASM_OUTPUT_DWARF_DATA4 (asm_out_file, 4);
  if (flag_verbose_asm)
    {
      fprintf (asm_out_file, "\t%s Pad to 8 byte boundary",
	       ASM_COMMENT_START);
    }
  fputc ('\n', asm_out_file);
  ASM_OUTPUT_DWARF_ADDR (asm_out_file, TEXT_BEGIN_LABEL);
  if (flag_verbose_asm)
    {
      fprintf (asm_out_file, "\t%s Address", ASM_COMMENT_START);
    }
  fputc ('\n', asm_out_file);
  ASM_OUTPUT_DWARF_DELTA4 (asm_out_file, TEXT_END_LABEL, TEXT_BEGIN_LABEL);
  if (flag_verbose_asm)
    {
      fprintf (asm_out_file, "%s Length", ASM_COMMENT_START);
    }
  fputc ('\n', asm_out_file);
  /* Output the terminator words.  */
  ASM_OUTPUT_DWARF_DATA4 (asm_out_file, 0);
  fputc ('\n', asm_out_file);
  ASM_OUTPUT_DWARF_DATA4 (asm_out_file, 0);
  fputc ('\n', asm_out_file);
}

/* Output the source line number correspondence information.  This
   information goes into the .debug_line section.  */
static void
output_line_info ()
{
  register unsigned long line_info_len;
  register unsigned long line_info_prolog_len;
  char line_label[MAX_ARTIFICIAL_LABEL_BYTES];
  char prev_line_label[MAX_ARTIFICIAL_LABEL_BYTES];
  register unsigned opc;
  register unsigned n_op_args;
  register dw_line_info_ref line_info;
  register unsigned long ft_index;
  register unsigned long lt_index;
  register unsigned long current_line;
  register long line_offset;
  register long line_delta;
  register unsigned long current_file;
  line_info_len = size_of_line_info ();
  ASM_OUTPUT_DWARF_DATA4 (asm_out_file, line_info_len);
  if (flag_verbose_asm)
    {
      fprintf (asm_out_file, "\t%s Length of Source Line Info.",
	       ASM_COMMENT_START);
    }
  fputc ('\n', asm_out_file);
  ASM_OUTPUT_DWARF_DATA2 (asm_out_file, DWARF_VERSION);
  if (flag_verbose_asm)
    {
      fprintf (asm_out_file, "\t%s DWARF Version",
	       ASM_COMMENT_START);
    }
  fputc ('\n', asm_out_file);
  line_info_prolog_len = size_of_line_prolog ();
  ASM_OUTPUT_DWARF_DATA4 (asm_out_file, line_info_prolog_len);
  if (flag_verbose_asm)
    {
      fprintf (asm_out_file, "\t%s Prolog Length",
	       ASM_COMMENT_START);
    }
  fputc ('\n', asm_out_file);
  ASM_OUTPUT_DWARF_DATA1 (asm_out_file, DWARF_LINE_MIN_INSTR_LENGTH);
  if (flag_verbose_asm)
    {
      fprintf (asm_out_file, "\t%s Minimum Instruction Length",
	       ASM_COMMENT_START);
    }
  fputc ('\n', asm_out_file);
  ASM_OUTPUT_DWARF_DATA1 (asm_out_file, DWARF_LINE_DEFAULT_IS_STMT_START);
  if (flag_verbose_asm)
    {
      fprintf (asm_out_file, "\t%s Default is_stmt_start flag",
	       ASM_COMMENT_START);
    }
  fputc ('\n', asm_out_file);
  fprintf (asm_out_file, "\t%s\t%d", ASM_BYTE_OP, DWARF_LINE_BASE);
  if (flag_verbose_asm)
    {
      fprintf (asm_out_file, "\t%s Line Base Value (Special Opcodes)",
	       ASM_COMMENT_START);
    }
  fputc ('\n', asm_out_file);
  fprintf (asm_out_file, "\t%s\t%u", ASM_BYTE_OP, DWARF_LINE_RANGE);
  if (flag_verbose_asm)
    {
      fprintf (asm_out_file, "\t%s Line Range Value (Special Opcodes)",
	       ASM_COMMENT_START);
    }
  fputc ('\n', asm_out_file);
  fprintf (asm_out_file, "\t%s\t%u", ASM_BYTE_OP, DWARF_LINE_OPCODE_BASE);
  if (flag_verbose_asm)
    {
      fprintf (asm_out_file, "\t%s Special Opcode Base",
	       ASM_COMMENT_START);
    }
  fputc ('\n', asm_out_file);
  for (opc = 1; opc < DWARF_LINE_OPCODE_BASE; ++opc)
    {
      switch (opc)
	{
	case DW_LNS_advance_pc:
	case DW_LNS_advance_line:
	case DW_LNS_set_file:
	case DW_LNS_set_column:
	case DW_LNS_fixed_advance_pc:
	  n_op_args = 1;
	  break;
	default:
	  n_op_args = 0;
	  break;
	}
      ASM_OUTPUT_DWARF_DATA1 (asm_out_file, n_op_args);
      if (flag_verbose_asm)
	{
	  fprintf (asm_out_file, "\t%s opcode: 0x%x has %d args",
		   ASM_COMMENT_START, opc, n_op_args);
	}
      fputc ('\n', asm_out_file);
    }
  if (flag_verbose_asm)
    {
      fprintf (asm_out_file, "%s Include Directory Table\n",
	       ASM_COMMENT_START);
    }
  /* Include directory table is empty, at present */
  ASM_OUTPUT_DWARF_DATA1 (asm_out_file, 0);
  fputc ('\n', asm_out_file);
  if (flag_verbose_asm)
    {
      fprintf (asm_out_file, "%s File Name Table\n", ASM_COMMENT_START);
    }
  for (ft_index = 1; ft_index < file_table_in_use; ++ft_index)
    {
      ASM_OUTPUT_DWARF_STRING (asm_out_file, file_table[ft_index]);
      if (flag_verbose_asm)
	{
	  fprintf (asm_out_file, "%s File Entry: 0x%x",
		   ASM_COMMENT_START, ft_index);
	}
      fputc ('\n', asm_out_file);
      /* Include directory index */
      output_uleb128 (0);
      fputc ('\n', asm_out_file);
      /* Modification time */
      output_uleb128 (0);
      fputc ('\n', asm_out_file);
      /* File length in bytes */
      output_uleb128 (0);
      fputc ('\n', asm_out_file);
    }
  /* Terminate the file name table */
  ASM_OUTPUT_DWARF_DATA1 (asm_out_file, 0);
  fputc ('\n', asm_out_file);

  /* Set the address register to the first location in the text section */
  ASM_OUTPUT_DWARF_DATA1 (asm_out_file, 0);
  if (flag_verbose_asm)
    {
      fprintf (asm_out_file, "\t%s DW_LNE_set_address", ASM_COMMENT_START);
    }
  fputc ('\n', asm_out_file);
  output_uleb128 (1 + PTR_SIZE);
  fputc ('\n', asm_out_file);
  ASM_OUTPUT_DWARF_DATA1 (asm_out_file, DW_LNE_set_address);
  fputc ('\n', asm_out_file);
  ASM_OUTPUT_DWARF_ADDR (asm_out_file, TEXT_BEGIN_LABEL);
  fputc ('\n', asm_out_file);

  /* Generate the line number to PC correspondence table, encoded as
     a series of state machine operations.  */
  current_file = 1;
  current_line = 1;
  strcpy (prev_line_label, TEXT_BEGIN_LABEL);
  for (lt_index = 1; lt_index < line_info_table_in_use; ++lt_index)
    {
      ASM_OUTPUT_DWARF_DATA1 (asm_out_file, DW_LNS_fixed_advance_pc);
      if (flag_verbose_asm)
	{
	  fprintf (asm_out_file, "\t%s DW_LNS_fixed_advance_pc",
		   ASM_COMMENT_START);
	}
      fputc ('\n', asm_out_file);
      sprintf (line_label, LINE_CODE_LABEL_FMT, lt_index);
      ASM_OUTPUT_DWARF_DELTA2 (asm_out_file, line_label, prev_line_label);
      fputc ('\n', asm_out_file);
      line_info = &line_info_table[lt_index];
      if (line_info->dw_file_num != current_file)
	{
	  current_file = line_info->dw_file_num;
	  ASM_OUTPUT_DWARF_DATA1 (asm_out_file, DW_LNS_set_file);
	  if (flag_verbose_asm)
	    {
	      fprintf (asm_out_file,
		       "\t%s DW_LNS_set_file", ASM_COMMENT_START);
	    }
	  fputc ('\n', asm_out_file);
	  output_uleb128 (current_file);
	  if (flag_verbose_asm)
	    {
	      fprintf (asm_out_file, "\t%s \"%s\"",
		       ASM_COMMENT_START, file_table[current_file]);
	    }
	  fputc ('\n', asm_out_file);
	}
      if (line_info->dw_line_num != current_line)
	{
	  line_offset = line_info->dw_line_num - current_line;
	  line_delta = line_offset - DWARF_LINE_BASE;
	  current_line = line_info->dw_line_num;
	  if (line_delta >= 0 && line_delta < (DWARF_LINE_RANGE - 1))
	    {
	      ASM_OUTPUT_DWARF_DATA1 (asm_out_file,
				      DWARF_LINE_OPCODE_BASE + line_delta);
	      if (flag_verbose_asm)
		{
		  fprintf (asm_out_file,
			   "\t%s line %d", ASM_COMMENT_START, current_line);
		}
	      fputc ('\n', asm_out_file);
	    }
	  else
	    {
	      ASM_OUTPUT_DWARF_DATA1 (asm_out_file, DW_LNS_advance_line);
	      if (flag_verbose_asm)
		{
		  fprintf (asm_out_file,
			   "\t%s advance to line %d",
			   ASM_COMMENT_START, current_line);
		}
	      fputc ('\n', asm_out_file);
	      output_sleb128 (line_offset);
	      fputc ('\n', asm_out_file);
	      ASM_OUTPUT_DWARF_DATA1 (asm_out_file, DW_LNS_copy);
	      fputc ('\n', asm_out_file);
	    }
	}
      strcpy (prev_line_label, line_label);
    }

  ASM_OUTPUT_DWARF_DATA1 (asm_out_file, 0);
  if (flag_verbose_asm)
    {
      fprintf (asm_out_file, "\t%s DW_LNE_set_address", ASM_COMMENT_START);
    }
  fputc ('\n', asm_out_file);
  output_uleb128 (1 + PTR_SIZE);
  fputc ('\n', asm_out_file);
  ASM_OUTPUT_DWARF_DATA1 (asm_out_file, DW_LNE_set_address);
  fputc ('\n', asm_out_file);
  ASM_OUTPUT_DWARF_ADDR (asm_out_file, TEXT_END_LABEL);
  fputc ('\n', asm_out_file);
  /* Output the marker for the end of the line number info.  */
  ASM_OUTPUT_DWARF_DATA1 (asm_out_file, 0);
  if (flag_verbose_asm)
    {
      fprintf (asm_out_file, "\t%s DW_LNE_end_sequence", ASM_COMMENT_START);
    }
  fputc ('\n', asm_out_file);
  output_uleb128 (1);
  fputc ('\n', asm_out_file);
  ASM_OUTPUT_DWARF_DATA1 (asm_out_file, DW_LNE_end_sequence);
  fputc ('\n', asm_out_file);
}

/**************** attribute support utilities ********************************/

/*
 * Given a pointer to a BLOCK node return non-zero if (and only if) the node
 * in question represents the outermost pair of curly braces (i.e. the "body
 * block") of a function or method.
 *
 * For any BLOCK node representing a "body block" of a function or method, the
 * BLOCK_SUPERCONTEXT of the node will point to another BLOCK node which
 * represents the outermost (function) scope for the function or method (i.e.
 * the one which includes the formal parameters).  The BLOCK_SUPERCONTEXT of
 * *that* node in turn will point to the relevant FUNCTION_DECL node.
 */
inline int
is_body_block (stmt)
     register tree stmt;
{
  if (TREE_CODE (stmt) == BLOCK)
    {
      register tree parent = BLOCK_SUPERCONTEXT (stmt);

      if (TREE_CODE (parent) == BLOCK)
	{
	  register tree grandparent = BLOCK_SUPERCONTEXT (parent);

	  if (TREE_CODE (grandparent) == FUNCTION_DECL)
	    return 1;
	}
    }
  return 0;
}

/* Reset the base type to DIE table, and build a special predefined
   base type entry for the "int" signed integer base type.  The
   "int" base type is used to construct subscript index range
   definitions, in situations where an anonymous integer type
   is required.  */
inline void
init_base_type_table ()
{
  register int i;
  register base_type_ref bt;
  for (i = 0; i < NUM_BASE_TYPES; ++i)
    {
      base_type_die_table[i] = NULL;
    }
  assert (comp_unit_die != 0);
  for (i = 0; i < NUM_BASE_TYPES; ++i)
    {
      bt = &base_type_table[i];
      if (strcmp (bt->bt_name, "int") == 0)
	{
	  int_base_type_die = new_die (DW_TAG_base_type, comp_unit_die);
	  base_type_die_table[i] = int_base_type_die;
	  add_AT_string (int_base_type_die, DW_AT_name, bt->bt_name);
	  add_AT_unsigned (int_base_type_die,
			   DW_AT_byte_size, bt->bt_size / 8);
	  add_AT_unsigned (int_base_type_die, DW_AT_encoding, bt->bt_type);
	  break;
	}
    }
}

/* Given a pointer to a tree node for some base type, return a pointer to
   a DIE that describes the given type.

   This routine must only be called for GCC type nodes that correspond to
   Dwarf base (fundamental) types.  */
static dw_die_ref
base_type_die (type)
     register tree type;
{
  register dw_die_ref base_type_result = NULL;
  register char *type_name = NULL;
  register int type_index = 0;
  register base_type_ref bt;
  register int i;

  if (TREE_CODE (type) == ERROR_MARK)
    return 0;

  switch (TREE_CODE (type))
    {
    case VOID_TYPE:
    case ERROR_MARK:
      break;

    case INTEGER_TYPE:
      /* Carefully distinguish all the standard types of C, without messing
         up if the language is not C. Note that we check only for the names
         that contain spaces; other names might occur by coincidence in other 
         languages.  */
      if (TYPE_NAME (type) != 0
	  && TREE_CODE (TYPE_NAME (type)) == TYPE_DECL
	  && DECL_NAME (TYPE_NAME (type)) != 0
	  && TREE_CODE (DECL_NAME (TYPE_NAME (type))) == IDENTIFIER_NODE)
	{
	  type_name = IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (type)));
	  for (i = 0; i < NUM_BASE_TYPES; ++i)
	    {
	      bt = &base_type_table[i];
	      if (strcmp (type_name, bt->bt_name) == 0)
		{
		  type_index = i;
		  break;
		}
	    }
	}

      /* Most integer types will be sorted out above, however, for the sake
         of special `array index' integer types, the following code is also
         provided.  */
      if (type_index == 0)
	{
	  for (i = 0; i < NUM_BASE_TYPES; ++i)
	    {
	      bt = &base_type_table[i];
	      if (bt->bt_size == TYPE_PRECISION (type)
		  && (TREE_UNSIGNED (type) == 0) == bt->bt_is_signed)
		{
		  type_index = i;
		  break;
		}
	    }
	}
      break;

    case REAL_TYPE:
      /* Carefully distinguish all the standard types of C, without messing
         up if the language is not C.  */
      for (i = 0; i < NUM_BASE_TYPES; ++i)
	{
	  bt = &base_type_table[i];
	  if ((bt->bt_type == DW_ATE_float)
	      && (bt->bt_size == TYPE_PRECISION (type)))
	    {
	      type_index = i;
	      break;
	    }
	}
      break;

    case COMPLEX_TYPE:
      for (i = 0; i < NUM_BASE_TYPES; ++i)
	{
	  bt = &base_type_table[i];
	  if ((bt->bt_type == DW_ATE_complex_float)
	      && (bt->bt_size == TYPE_PRECISION (type)))
	    {
	      type_index = i;
	      break;
	    }
	}
      break;

    case CHAR_TYPE:
      /* GNU Pascal/Ada CHAR type.  Not used in C.  */
      for (i = 0; i < NUM_BASE_TYPES; ++i)
	{
	  bt = &base_type_table[i];
	  if (bt->bt_type == DW_ATE_signed_char
	      || bt->bt_type == DW_ATE_unsigned_char)
	    {
	      if (bt->bt_size == TYPE_PRECISION (type)
		  && ((TREE_UNSIGNED (type) == 0) == bt->bt_is_signed))
		{
		  type_index = i;
		  break;
		}
	    }
	}
      break;

    case BOOLEAN_TYPE:
      /* GNU FORTRAN/Ada BOOLEAN type.  */
      for (i = 0; i < NUM_BASE_TYPES; ++i)
	{
	  bt = &base_type_table[i];
	  if (bt->bt_type == DW_ATE_boolean
	      && bt->bt_size == TYPE_PRECISION (type))
	    {
	      type_index = i;
	      break;
	    }
	}
      break;

    default:
      abort ();			/* No other TREE_CODEs are Dwarf fundamental
				   types.  */
    }

  if (type_index == 0)
    {
      base_type_result = NULL;
    }
  else
    {
      base_type_result = base_type_die_table[type_index];
      if (base_type_result == NULL)
	{
	  bt = &base_type_table[type_index];
	  base_type_result = new_die (DW_TAG_base_type, comp_unit_die);
	  base_type_die_table[type_index] = base_type_result;
	  add_AT_string (base_type_result, DW_AT_name, bt->bt_name);
	  add_AT_unsigned (base_type_result, DW_AT_byte_size, bt->bt_size / 8);
	  add_AT_unsigned (base_type_result, DW_AT_encoding, bt->bt_type);
	}

    }

  return base_type_result;
}

/* Given a pointer to an arbitrary ..._TYPE tree node, return a pointer to
   the Dwarf "root" type for the given input type.  The Dwarf "root" type of
   a given type is generally the same as the given type, except that if the
   given type is a pointer or reference type, then the root type of the given
   type is the root type of the "basis" type for the pointer or reference
   type.  (This definition of the "root" type is recursive.) Also, the root
   type of a `const' qualified type or a `volatile' qualified type is the
   root type of the given type without the qualifiers.  */
static tree
root_type (type)
     register tree type;
{
  if (TREE_CODE (type) == ERROR_MARK)
    return error_mark_node;

  switch (TREE_CODE (type))
    {
    case ERROR_MARK:
      return error_mark_node;

    case POINTER_TYPE:
    case REFERENCE_TYPE:
      return type_main_variant (root_type (TREE_TYPE (type)));

    default:
      return type_main_variant (type);
    }
}

/* Given a pointer to an arbitrary ..._TYPE tree node, return non-zero if the
   given input type is a Dwarf "fundamental" type.  Otherwise return null.  */
inline int
is_base_type (type)
     register tree type;
{
  switch (TREE_CODE (type))
    {
    case ERROR_MARK:
    case VOID_TYPE:
    case INTEGER_TYPE:
    case REAL_TYPE:
    case COMPLEX_TYPE:
    case BOOLEAN_TYPE:
    case CHAR_TYPE:
      return 1;

    case SET_TYPE:
    case ARRAY_TYPE:
    case RECORD_TYPE:
    case UNION_TYPE:
    case QUAL_UNION_TYPE:
    case ENUMERAL_TYPE:
    case FUNCTION_TYPE:
    case METHOD_TYPE:
    case POINTER_TYPE:
    case REFERENCE_TYPE:
    case FILE_TYPE:
    case OFFSET_TYPE:
    case LANG_TYPE:
      return 0;

    default:
      abort ();
    }
  return 0;
}

/* Given a pointer to an arbitrary ..._TYPE tree node, return a debugging
   entry that chains various modifiers in front of the given type.  */
static dw_die_ref
modified_type_die (type, is_const_type, is_volatile_type, context_die)
     register tree type;
     register int is_const_type;
     register int is_volatile_type;
     register dw_die_ref context_die;
{
  register enum tree_code code = TREE_CODE (type);
  register dw_die_ref mod_type_die = NULL;
  register dw_die_ref sub_die = NULL;
  register tree item_type;

  if (code != ERROR_MARK)
    {
      if (is_const_type)
	{
	  mod_type_die = new_die (DW_TAG_const_type, context_die);
	  sub_die = modified_type_die (type,
				       0, is_volatile_type, context_die);
	}
      else if (is_volatile_type)
	{
	  mod_type_die = new_die (DW_TAG_volatile_type, context_die);
	  sub_die = modified_type_die (type, 0, 0, context_die);
	}
      else if (code == POINTER_TYPE)
	{
	  mod_type_die = new_die (DW_TAG_pointer_type, context_die);
	  add_AT_unsigned (mod_type_die, DW_AT_byte_size, PTR_SIZE);
	  add_AT_unsigned (mod_type_die, DW_AT_address_class, 0);
	  item_type = TREE_TYPE (type);
	  sub_die = modified_type_die (item_type,
				       TYPE_READONLY (item_type),
				       TYPE_VOLATILE (item_type),
				       context_die);
	}
      else if (code == REFERENCE_TYPE)
	{
	  mod_type_die = new_die (DW_TAG_reference_type, context_die);
	  add_AT_unsigned (mod_type_die, DW_AT_byte_size, PTR_SIZE);
	  add_AT_unsigned (mod_type_die, DW_AT_address_class, 0);
	  item_type = TREE_TYPE (type);
	  sub_die = modified_type_die (item_type,
				       TYPE_READONLY (item_type),
				       TYPE_VOLATILE (item_type),
				       context_die);
	}
      else if (is_base_type (type))
	{
	  mod_type_die = base_type_die (type);
	}
      else
	{
	  /* We have to get the type_main_variant here (and pass that to the
	     `lookup_type_die' routine) because the ..._TYPE node we have
	     might simply be a *copy* of some original type node (where the
	     copy was created to help us keep track of typedef names) and
	     that copy might have a different TYPE_UID from the original
	     ..._TYPE node.  (Note that when `equate_type_number_to_die' is
	     labeling a given type DIE for future reference, it always only
	     handles DIEs representing *main variants*, and it never even
	     knows about non-main-variants.).  */
	  mod_type_die = lookup_type_die (type_main_variant (type));

	  /* Normally, we assume that all types are defined before they are
	     referenced.  If this is not the case, then mod_type_die will
	     be NULL here, and we must backchain.  This can happen as the
	     result of a forward declaration of a structure tag.  */
	  if (mod_type_die == NULL)
	    {
	      dw_die_ref placeholder_die = new_die (DW_TAG_padding,
						    context_die);
	      backchain_AT_die_ref (type, placeholder_die);
	    }
	}
    }
  if (sub_die != NULL)
    {
      add_AT_die_ref (mod_type_die, DW_AT_type, sub_die);
    }
  return mod_type_die;
}

/* Fix all unresolved die references that resulted from forward
   declarations.  */
static void
resolve_backchains ()
{
  register backchain_ref back;

  back = backchain;
  while (back)
    {
      register dw_die_ref type_die;

      type_die = lookup_type_die (type_main_variant (back->type));

      assert (type_die != NULL);
				    
      /* ??? It would be cleaner to find the die attribute, and change
	 the val_dir_ref field to point to this new die.  Just overwriting
	 the temporary die with the correct one is easier though, and seems
	 to work just as well.  */
      memcpy (back->placeholder, type_die, sizeof (die_node));

      back = back->next;
    }
}

/* Given a pointer to an arbitrary ..._TYPE tree node, return true if it is
   an enumerated type.   */
inline int
type_is_enum (type)
     register tree type;
{
  return TREE_CODE (type) == ENUMERAL_TYPE;
}

/* Return the register number described by a given RTL node.  */
static unsigned
reg_number (rtl)
     register rtx rtl;
{
  register unsigned regno = REGNO (rtl);

  if (regno >= FIRST_PSEUDO_REGISTER)
    {
      warning_with_decl (dwarf_last_decl, "internal regno botch: regno = %d\n",
			 regno);
      regno = 0;
    }
  regno = DBX_REGISTER_NUMBER (regno);
  return regno;
}

/* Return a location descriptor that designates a machine register.  */
static dw_loc_descr_ref
reg_loc_descriptor (rtl)
     register rtx rtl;
{
  register dw_loc_descr_ref loc_result = NULL;
  register unsigned reg = reg_number (rtl);
  if (reg >= 0 && reg <= 31)
    {
      loc_result = new_loc_descr (DW_OP_reg0 + reg, 0);
    }
  else
    {
      loc_result = new_loc_descr (DW_OP_regx, reg, 0);
    }
  return loc_result;
}

/* Return a location descriptor that designates a base+offset location.  */
static dw_loc_descr_ref
based_loc_descr (reg, offset)
     unsigned reg;
     long int offset;
{
  register dw_loc_descr_ref loc_result;
  register unsigned fp_reg = (frame_pointer_needed)
				      ? FRAME_POINTER_REGNUM
			              : STACK_POINTER_REGNUM;
  if (reg == fp_reg)
    {
      loc_result = new_loc_descr (DW_OP_fbreg,
				  offset - current_funcdef_frame_size, 0);
    }
  else if (reg >= 0 && reg <= 31)
    {
      loc_result = new_loc_descr (DW_OP_breg0 + reg, offset);
    }
  else
    {
      loc_result = new_loc_descr (DW_OP_bregx, reg, offset);
    }
  return loc_result;
}

/* Return true if this RTL expression describes a base+offset calculation.  */
inline int
is_based_loc (rtl)
     register rtx rtl;
{
    return GET_CODE (rtl) == PLUS
	   && ((GET_CODE (XEXP (rtl, 0)) == REG
	        && GET_CODE (XEXP (rtl, 1)) == CONST_INT));
}

/* The following routine converts the RTL for a variable or parameter
   (resident in memory) into an equivalent Dwarf representation of a
   mechanism for getting the address of that same variable onto the top of a
   hypothetical "address evaluation" stack.
   When creating memory location descriptors, we are effectively transforming
   the RTL for a memory-resident object into its Dwarf postfix expression
   equivalent.  This routine recursively descends an RTL tree, turning
   it into Dwarf postfix code as it goes.  */
static dw_loc_descr_ref
mem_loc_descriptor (rtl)
     register rtx rtl;
{
  dw_loc_descr_ref mem_loc_result = NULL;
  /* Note that for a dynamically sized array, the location we will generate a 
     description of here will be the lowest numbered location which is
     actually within the array.  That's *not* necessarily the same as the
     zeroth element of the array.  */
  switch (GET_CODE (rtl))
    {
    case SUBREG:
      /* The case of a subreg may arise when we have a local (register)
         variable or a formal (register) parameter which doesn't quite fill
         up an entire register.  For now, just assume that it is
         legitimate to make the Dwarf info refer to the whole register which
         contains the given subreg.  */
      rtl = XEXP (rtl, 0);
      /* Drop thru.  */

    case REG:
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
      mem_loc_result = based_loc_descr (reg_number (rtl), 0);
      break;

    case MEM:
      mem_loc_result = mem_loc_descriptor (XEXP (rtl, 0));
      add_loc_descr (&mem_loc_result, new_loc_descr (DW_OP_deref, 0, 0));
      break;

    case CONST:
    case SYMBOL_REF:
      mem_loc_result = new_loc_descr (DW_OP_addr, 0, 0);
      mem_loc_result->dw_loc_oprnd1.val_class = dw_val_class_addr;
      mem_loc_result->dw_loc_oprnd1.v.val_addr = addr_to_string (rtl);
      break;

    case PLUS:
      if (is_based_loc (rtl))
	{
	  mem_loc_result = based_loc_descr (
			      reg_number (XEXP (rtl, 0)),
			      INTVAL (XEXP (rtl, 1)));
	}
      else
	{
	  add_loc_descr (&mem_loc_result, mem_loc_descriptor (XEXP (rtl, 0)));
	  add_loc_descr (&mem_loc_result, mem_loc_descriptor (XEXP (rtl, 1)));
	  add_loc_descr (&mem_loc_result, new_loc_descr (DW_OP_plus, 0, 0));
	}
      break;

    case CONST_INT:
      mem_loc_result = new_loc_descr (DW_OP_constu, INTVAL (rtl), 0);
      break;

    default:
      abort ();
    }
  return mem_loc_result;
}

/* Output a proper Dwarf location descriptor for a variable or parameter
   which is either allocated in a register or in a memory location.  For a
   register, we just generate an OP_REG and the register number.  For a
   memory location we provide a Dwarf postfix expression describing how to
   generate the (dynamic) address of the object onto the address stack.  */
static dw_loc_descr_ref
loc_descriptor (rtl)
     register rtx rtl;
{
  dw_loc_descr_ref loc_result = NULL;
  switch (GET_CODE (rtl))
    {
    case SUBREG:

      /* The case of a subreg may arise when we have a local (register)
         variable or a formal (register) parameter which doesn't quite fill
         up an entire register.       For now, just assume that it is
         legitimate to make the Dwarf info refer to the whole register which
         contains the given subreg.  */

      rtl = XEXP (rtl, 0);
      loc_result = new_loc_descr (DW_OP_regx, reg_number (rtl), 0);
      break;

    case REG:
      loc_result = new_loc_descr (DW_OP_regx, reg_number (rtl), 0);
      break;

    case MEM:
      loc_result = mem_loc_descriptor (XEXP (rtl, 0));
      break;

    default:
      abort ();			/* Should never happen */
    }
  return loc_result;
}

/* Given an unsigned value, round it up to the lowest multiple of `boundary'
   which is not less than the value itself.  */
inline unsigned
ceiling (value, boundary)
     register unsigned value;
     register unsigned boundary;
{
  return (((value + boundary - 1) / boundary) * boundary);
}

/* Given a pointer to what is assumed to be a FIELD_DECL node, return a
   pointer to the declared type for the relevant field variable, or return
   `integer_type_node' if the given node turns out to be an
   ERROR_MARK node.  */
inline tree
field_type (decl)
     register tree decl;
{
  register tree type;

  if (TREE_CODE (decl) == ERROR_MARK)
    return integer_type_node;

  type = DECL_BIT_FIELD_TYPE (decl);
  if (type == NULL)
    type = TREE_TYPE (decl);

  return type;
}

/* Given a pointer to a tree node, assumed to be some kind of a ..._TYPE
   node, return the alignment in bits for the type, or else return
   BITS_PER_WORD if the node actually turns out to be an
   ERROR_MARK node.  */
inline unsigned
simple_type_align_in_bits (type)
     register tree type;
{
  return (TREE_CODE (type) != ERROR_MARK) ? TYPE_ALIGN (type) : BITS_PER_WORD;
}

/* Given a pointer to a tree node, assumed to be some kind of a ..._TYPE
   node, return the size in bits for the type if it is a constant, or else
   return the alignment for the type if the type's size is not constant, or
   else return BITS_PER_WORD if the type actually turns out to be an
   ERROR_MARK node.  */
inline unsigned
simple_type_size_in_bits (type)
     register tree type;
{
  if (TREE_CODE (type) == ERROR_MARK)
    return BITS_PER_WORD;
  else
    {
      register tree type_size_tree = TYPE_SIZE (type);

      if (TREE_CODE (type_size_tree) != INTEGER_CST)
	return TYPE_ALIGN (type);

      return (unsigned) TREE_INT_CST_LOW (type_size_tree);
    }
}

/* Given a pointer to what is assumed to be a FIELD_DECL node, compute and
   return the byte offset of the lowest addressed byte of the "containing
   object" for the given FIELD_DECL, or return 0 if we are unable to
   determine what that offset is, either because the argument turns out to
   be a pointer to an ERROR_MARK node, or because the offset is actually
   variable.  (We can't handle the latter case just yet).  */
static unsigned
field_byte_offset (decl)
     register tree decl;
{
  register unsigned type_align_in_bytes;
  register unsigned type_align_in_bits;
  register unsigned type_size_in_bits;
  register unsigned object_offset_in_align_units;
  register unsigned object_offset_in_bits;
  register unsigned object_offset_in_bytes;
  register tree type;
  register tree bitpos_tree;
  register tree field_size_tree;
  register unsigned bitpos_int;
  register unsigned deepest_bitpos;
  register unsigned field_size_in_bits;

  if (TREE_CODE (decl) == ERROR_MARK)
    return 0;

  if (TREE_CODE (decl) != FIELD_DECL)
    abort ();

  type = field_type (decl);

  bitpos_tree = DECL_FIELD_BITPOS (decl);
  field_size_tree = DECL_SIZE (decl);

  /* We cannot yet cope with fields whose positions or sizes are variable, so 
     for now, when we see such things, we simply return 0.  Someday, we may
     be able to handle such cases, but it will be damn difficult.  */
  if (TREE_CODE (bitpos_tree) != INTEGER_CST)
    return 0;
  bitpos_int = (unsigned) TREE_INT_CST_LOW (bitpos_tree);

  if (TREE_CODE (field_size_tree) != INTEGER_CST)
    return 0;
  field_size_in_bits = (unsigned) TREE_INT_CST_LOW (field_size_tree);

  type_size_in_bits = simple_type_size_in_bits (type);

  type_align_in_bits = simple_type_align_in_bits (type);
  type_align_in_bytes = type_align_in_bits / BITS_PER_UNIT;

  /* Note that the GCC front-end doesn't make any attempt to keep track of
     the starting bit offset (relative to the start of the containing
     structure type) of the hypothetical "containing object" for a bit-
     field.  Thus, when computing the byte offset value for the start of the
     "containing object" of a bit-field, we must deduce this information on 
     our own. This can be rather tricky to do in some cases.  For example,
     handling the following structure type definition when compiling for an
     i386/i486 target (which only aligns long long's to 32-bit boundaries)
     can be very tricky:

	 struct S { int field1; long long field2:31; };

     Fortunately, there is a simple rule-of-thumb which can be
     used in such cases.  When compiling for an i386/i486, GCC will allocate
     8 bytes for the structure shown above.  It decides to do this based upon 
     one simple rule for bit-field allocation.  Quite simply, GCC allocates
     each "containing object" for each bit-field at the first (i.e. lowest
     addressed) legitimate alignment boundary (based upon the required
     minimum alignment for the declared type of the field) which it can
     possibly use, subject to the condition that there is still enough
     available space remaining in the containing object (when allocated at
     the selected point) to fully accommodate all of the bits of the
     bit-field itself.  This simple rule makes it obvious why GCC allocates
     8 bytes for each object of the structure type shown above.  When looking
     for a place to allocate the "containing object" for `field2', the
     compiler simply tries to allocate a 64-bit "containing object" at each
     successive 32-bit boundary (starting at zero) until it finds a place to
     allocate that 64- bit field such that at least 31 contiguous (and
     previously unallocated) bits remain within that selected 64 bit field.
     (As it turns out, for the example above, the compiler finds that it is
     OK to allocate the "containing object" 64-bit field at bit-offset zero
     within the structure type.) Here we attempt to work backwards from the
     limited set of facts we're given, and we try to deduce from those facts, 
     where GCC must have believed that the containing object started (within
     the structure type). The value we deduce is then used (by the callers of 
     this routine) to generate DW_AT_location and DW_AT_bit_offset attributes 
     for fields (both bit-fields and, in the case of DW_AT_location, regular
     fields as well).  */

  /* Figure out the bit-distance from the start of the structure to the
     "deepest" bit of the bit-field.  */
  deepest_bitpos = bitpos_int + field_size_in_bits;

  /* This is the tricky part.  Use some fancy footwork to deduce where the
     lowest addressed bit of the containing object must be.  */
  object_offset_in_bits
    = ceiling (deepest_bitpos, type_align_in_bits) - type_size_in_bits;

  /* Compute the offset of the containing object in "alignment units".  */
  object_offset_in_align_units = object_offset_in_bits / type_align_in_bits;

  /* Compute the offset of the containing object in bytes.  */
  object_offset_in_bytes = object_offset_in_align_units * type_align_in_bytes;

  return object_offset_in_bytes;
}



/****************************** attributes *********************************/

/* The following routines define various Dwarf attributes
   (and any data associated with them).  */


/* Output the form of location attributes suitable for whole variables and
   whole parameters.  Note that the location attributes for struct fields are
   generated by the routine `data_member_location_attribute' below.  */
static void
add_location_attribute (die, rtl)
     dw_die_ref die;
     register rtx rtl;
{
  dw_loc_descr_ref loc_descr = NULL;

  /* Handle a special case.  If we are about to output a location descriptor
     for a variable or parameter which has been optimized out of existence,
     don't do that.  Instead we output a null location descriptor value as
     part of the location attribute. A variable which has been optimized out
     of existence will have a DECL_RTL value which denotes a pseudo-reg.
     Currently, in some rare cases, variables can have DECL_RTL values which
     look like (MEM (REG pseudo-reg#)).  These cases are due to bugs
     elsewhere in the compiler.  We treat such cases as if the variable(s) in 
     question had been optimized out of existence. Note that in all cases
     where we wish to express the fact that a variable has been optimized out 
     of existence, we do not simply suppress the generation of the entire
     location attribute because the absence of a location attribute in
     certain kinds of DIEs is used to indicate something else entirely...
     i.e. that the DIE represents an object declaration, but not a
     definition.  So sayeth the PLSIG.  */
  if (!is_pseudo_reg (rtl)
      && (GET_CODE (rtl) != MEM
	  || !is_pseudo_reg (XEXP (rtl, 0))))
    {
      loc_descr = loc_descriptor (eliminate_regs (rtl, 0, NULL_RTX));
    }

#ifdef MIPS_DEBUGGING_INFO
  /* ??? SGI's dwarf reader is buggy, and will not accept a zero size
     location descriptor.  Lets just use r0 for now to represent a
     variable that has been optimized away.  */
  if (loc_descr == NULL)
    {
      loc_descr = loc_descriptor (gen_rtx (REG, word_mode, 0));
    }
#endif

  add_AT_loc (die, DW_AT_location, loc_descr);
}

/* Attach the specialized form of location attribute used for data
   members of struct and union types.  In the special case of a
   FIELD_DECL node which represents a bit-field, the "offset" part
   of this special location descriptor must indicate the distance
   in bytes from the lowest-addressed byte of the containing struct
   or union type to the lowest-addressed byte of the "containing
   object" for the bit-field.  (See the `field_byte_offset' function
   above).. For any given bit-field, the "containing object" is a
   hypothetical object (of some integral or enum type) within which
   the given bit-field lives.  The type of this hypothetical
   "containing object" is always the same as the declared type of
   the individual bit-field itself (for GCC anyway... the DWARF
   spec doesn't actually mandate this).  Note that it is the size
   (in bytes) of the hypothetical "containing object" which will
   be given in the DW_AT_byte_size attribute for this bit-field.
   (See the `byte_size_attribute' function below.)  It is also used
   when calculating the value of the DW_AT_bit_offset attribute.
   (See the `bit_offset_attribute' function below).  */
static void
add_data_member_location_attribute (die, decl)
     register dw_die_ref die;
     register tree decl;
{
  register unsigned long offset = field_byte_offset (decl);
  register dw_loc_descr_ref loc_descr;
  register enum dwarf_location_atom op;

  /* The DWARF2 standard says that we should assume that the structure address
     is already on the stack, so we can specify a structure field address
     by using DW_OP_plus_uconst.  */
#ifdef MIPS_DEBUGGING_INFO
  /* ??? The SGI dwarf reader does not handle the DW_OP_plus_uconst operator
     correctly.  It works only if we leave the offset on the stack.  */
  op = DW_OP_constu;
#else
  op = DW_OP_plus_uconst;
#endif
  loc_descr = new_loc_descr (op, offset, 0);
  add_AT_loc (die, DW_AT_data_member_location, loc_descr);
}

/* Attach an DW_AT_const_value attribute for a variable or a parameter which
   does not have a "location" either in memory or in a register.  These
   things can arise in GNU C when a constant is passed as an actual parameter
   to an inlined function.  They can also arise in C++ where declared
   constants do not necessarily get memory "homes".  */
static void
add_const_value_attribute (die, rtl)
     register dw_die_ref die;
     register rtx rtl;
{
  switch (GET_CODE (rtl))
    {
    case CONST_INT:
      /* Note that a CONST_INT rtx could represent either an integer or a
         floating-point constant.  A CONST_INT is used whenever the constant
         will fit into a single word.  In all such cases, the original mode
         of the constant value is wiped out, and the CONST_INT rtx is
         assigned VOIDmode.  */
      add_AT_unsigned (die, DW_AT_const_value, (unsigned) INTVAL (rtl));
      break;

    case CONST_DOUBLE:
      /* Note that a CONST_DOUBLE rtx could represent either an integer or a
         floating-point constant.  A CONST_DOUBLE is used whenever the
         constant requires more than one word in order to be adequately
         represented.  In all such cases, the original mode of the constant
         value is preserved as the mode of the CONST_DOUBLE rtx, but for
         simplicity we always just output CONST_DOUBLEs using 8 bytes.  */
      add_AT_double (die, DW_AT_const_value,
		     (unsigned) CONST_DOUBLE_HIGH (rtl),
		     (unsigned) CONST_DOUBLE_LOW (rtl));
      break;

    case CONST_STRING:
      add_AT_string (die, DW_AT_const_value, XSTR (rtl, 0));
      break;

    case SYMBOL_REF:
    case LABEL_REF:
    case CONST:
      add_AT_addr (die, DW_AT_const_value, addr_to_string (rtl));
      break;

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
         values in Dwarf, so for now we just punt and generate an
         DW_AT_const_value attribute with null address.  */
      add_AT_addr (die, DW_AT_const_value, addr_to_string (const0_rtx));
      break;

    default:
      /* No other kinds of rtx should be possible here.  */
      abort ();
    }

}

/* Generate *either* an DW_AT_location attribute or else an DW_AT_const_value
   data attribute for a variable or a parameter.  We generate the
   DW_AT_const_value attribute only in those cases where the given variable
   or parameter does not have a true "location" either in memory or in a
   register.  This can happen (for example) when a constant is passed as an
   actual argument in a call to an inline function.  (It's possible that
   these things can crop up in other ways also.)  Note that one type of
   constant value which can be passed into an inlined function is a constant
   pointer.  This can happen for example if an actual argument in an inlined
   function call evaluates to a compile-time constant address.  */
static void
add_location_or_const_value_attribute (die, decl)
     register dw_die_ref die;
     register tree decl;
{
  register rtx rtl;
  register tree declared_type;
  register tree passed_type;

  if (TREE_CODE (decl) == ERROR_MARK)
    {
      return;
    }
  if ((TREE_CODE (decl) != VAR_DECL)
      && (TREE_CODE (decl) != PARM_DECL))
    {
      /* Should never happen.  */
      abort ();
      return;
    }
  /* Here we have to decide where we are going to say the parameter "lives"
     (as far as the debugger is concerned).  We only have a couple of
     choices.  GCC provides us with DECL_RTL and with DECL_INCOMING_RTL.
     DECL_RTL normally indicates where the parameter lives during most of the 
     activa- tion of the function.  If optimization is enabled however, this
     could be either NULL or else a pseudo-reg.  Both of those cases indicate 
     that the parameter doesn't really live anywhere (as far as the code
     generation parts of GCC are concerned) during most of the function's
     activation.  That will happen (for example) if the parameter is never
     referenced within the function.  We could just generate a location
     descriptor here for all non-NULL non-pseudo values of DECL_RTL and
     ignore all of the rest, but we can be a little nicer than that if we
     also consider DECL_INCOMING_RTL in cases where DECL_RTL is NULL or is a
     pseudo-reg. Note however that we can only get away with using
     DECL_INCOMING_RTL as a backup substitute for DECL_RTL in certain limited 
     cases.  In cases where DECL_ARG_TYPE(decl) indicates the same type as
     TREE_TYPE(decl) we can be sure that the parameter was passed using the
     same type as it is declared to have within the function, and that its
     DECL_INCOMING_RTL points us to a place where a value of that type is
     passed.  In cases where DECL_ARG_TYPE(decl) and TREE_TYPE(decl) are
     different types however, we cannot (in general) use DECL_INCOMING_RTL as 
     a backup substitute for DECL_RTL because in these cases,
     DECL_INCOMING_RTL points us to a value of some type which is *different* 
     from the type of the parameter itself.  Thus, if we tried to use
     DECL_INCOMING_RTL to generate a location attribute in such cases, the
     debugger would end up (for example) trying to fetch a `float' from a
     place which actually contains the first part of a `double'.  That would
     lead to really incorrect and confusing output at debug-time, and we
     don't want that now do we? So in general, we DO NOT use
     DECL_INCOMING_RTL as a backup for DECL_RTL in cases where
     DECL_ARG_TYPE(decl) != TREE_TYPE(decl).  There are a couple of cute
     exceptions however.  On little-endian machines we can get away with
     using DECL_INCOMING_RTL even when DECL_ARG_TYPE(decl) is not the same as 
     TREE_TYPE(decl) but only when DECL_ARG_TYPE(decl) is an integral type
     which is smaller than TREE_TYPE(decl). These cases arise when (on a
     little-endian machine) a non-prototyped function has a parameter
     declared to be of type `short' or `char'.  In such cases,
     TREE_TYPE(decl) will be `short' or `char', DECL_ARG_TYPE(decl) will be
     `int', and DECL_INCOMING_RTL will point to the lowest-order byte of the
     passed `int' value.  If the debugger then uses that address to fetch a
     `short' or a `char' (on a little-endian machine) the result will be the
     correct data, so we allow for such exceptional cases below. Note that
     our goal here is to describe the place where the given formal parameter
     lives during most of the function's activation (i.e. between the end of
     the prologue and the start of the epilogue).  We'll do that as best as
     we can. Note however that if the given formal parameter is modified
     sometime during the execution of the function, then a stack backtrace
     (at debug-time) will show the function as having been called with the
     *new* value rather than the value which was originally passed in.  This
     happens rarely enough that it is not a major problem, but it *is* a
     problem, and I'd like to fix it.  A future version of dwarfout.c may
     generate two additional attributes for any given DW_TAG_formal_parameter 
     DIE which will describe the "passed type" and the "passed location" for
     the given formal parameter in addition to the attributes we now generate 
     to indicate the "declared type" and the "active location" for each
     parameter.  This additional set of attributes could be used by debuggers 
     for stack backtraces. Separately, note that sometimes DECL_RTL can be
     NULL and DECL_INCOMING_RTL can be NULL also.  This happens (for example) 
     for inlined-instances of inline function formal parameters which are
     never referenced.  This really shouldn't be happening.  All PARM_DECL
     nodes should get valid non-NULL DECL_INCOMING_RTL values, but
     integrate.c doesn't currently generate these values for inlined
     instances of inline function parameters, so when we see such cases, we
     are just SOL (shit-out-of-luck) for the time being (until integrate.c
     gets fixed).  */

  /* Use DECL_RTL as the "location" unless we find something better.  */
  rtl = DECL_RTL (decl);

  if (TREE_CODE (decl) == PARM_DECL)
    {
      if (rtl == NULL_RTX || is_pseudo_reg (rtl))
	{
	  declared_type = type_main_variant (TREE_TYPE (decl));
	  passed_type = type_main_variant (DECL_ARG_TYPE (decl));
	  /* This decl represents a formal parameter which was
	     optimized out.

	     Note that DECL_INCOMING_RTL may be NULL in here, but we handle
	     all* cases where (rtl == NULL_RTX) just below.  */
	  if (declared_type == passed_type)
	    {
	      rtl = DECL_INCOMING_RTL (decl);
	    }
	  else if (!BYTES_BIG_ENDIAN)
	    {
	      if (TREE_CODE (declared_type) == INTEGER_TYPE)
		{
		  if (TYPE_SIZE (declared_type) <= TYPE_SIZE (passed_type))
		    {
		      rtl = DECL_INCOMING_RTL (decl);
		    }
		}
	    }
	  if (rtl == NULL_RTX)
	    {
	      return;
	    }
	}
    }
  switch (GET_CODE (rtl))
    {
    case CONST_INT:
    case CONST_DOUBLE:
    case CONST_STRING:
    case SYMBOL_REF:
    case LABEL_REF:
    case CONST:
    case PLUS:
      /* DECL_RTL could be (plus (reg ...) (const_int ...)) */
      add_const_value_attribute (die, rtl);
      break;

    case MEM:
    case REG:
    case SUBREG:
      add_location_attribute (die, rtl);
      break;

    default:
      abort ();			/* Should never happen.  */
    }
}

/* Generate an DW_AT_name attribute given some string value to be included as
   the value of the attribute.  */
inline void
add_name_attribute (die, name_string)
     register dw_die_ref die;
     register char *name_string;
{
  if (name_string && *name_string)
    {
      add_AT_string (die, DW_AT_name, name_string);
    }
}

/* Given a tree node describing an array bound (either lower or upper) output
   a representation for that bound.  */
static void
add_bound_info (subrange_die, bound_attr, bound)
     register dw_die_ref subrange_die;
     register enum dwarf_attribute bound_attr;
     register tree bound;
{
  register dw_loc_descr_ref bound_loc = NULL;
  register unsigned bound_value = 0;
  switch (TREE_CODE (bound))
    {
    case ERROR_MARK:
      return;

    /* All fixed-bounds are represented by INTEGER_CST nodes.        */
    case INTEGER_CST:
      bound_value = TREE_INT_CST_LOW (bound);
      /* TODO: we need to check for C language below, or some flag
	 derived from the language.  C implies a lower bound of 0.   */
      if (!(bound_attr == DW_AT_lower_bound && bound_value == 0))
	{
	  add_AT_unsigned (subrange_die, bound_attr, bound_value);
        }
      break;

    /* Dynamic bounds may be represented by NOP_EXPR nodes containing
       SAVE_EXPR nodes.  */
    case NOP_EXPR:
      bound = TREE_OPERAND (bound, 0);
      /* ... fall thru...  */

    case SAVE_EXPR:
      /* If optimization is turned on, the SAVE_EXPRs that describe how to
         access the upper bound values are essentially bogus. They only
         describe (at best) how to get at these values at the points in the
         generated code right after they have just been computed.  Worse yet, 
         in the typical case, the upper bound values will not even *be*
         computed in the optimized code, so these SAVE_EXPRs are entirely
         bogus. In order to compensate for this fact, we check here to see if
         optimization is enabled, and if so, we effectively create an empty
         location description for the (unknown and unknowable) upper bound.
         This should not cause too much trouble for existing (stupid?)
         debuggers because they have to deal with empty upper bounds location
         descriptions anyway in order to be able to deal with incomplete array 
         types.  Of course an intelligent debugger (GDB?) should be able to
         comprehend that a missing upper bound specification in a array type
         used for a storage class `auto' local array variable indicates that
         the upper bound is both unknown (at compile- time) and unknowable (at
         run-time) due to optimization.  */
      if (!optimize)
	{
	  bound_loc = mem_loc_descriptor (
				      eliminate_regs (SAVE_EXPR_RTL (bound),
						      0, NULL_RTX));
	}
      else
	{
	  bound_loc = NULL;
	}
      add_AT_loc (subrange_die, bound_attr, bound_loc);
      break;

    default:
      abort ();
    }
}

/* Note that the block of subscript information for an array type also
   includes information about the element type of type given array type.  */
static void
add_subscript_info (type_die, type)
     register dw_die_ref type_die;
     register tree type;
{
  register unsigned dimension_number;
  register tree lower, upper;
  register dw_die_ref subrange_die;

  /* The GNU compilers represent multidimensional array types as sequences of 
     one dimensional array types whose element types are themselves array
     types.  Here we squish that down, so that each multidimensional array
     type gets only one array_type DIE in the Dwarf debugging info. The draft 
     Dwarf specification say that we are allowed to do this kind of
     compression in C (because there is no difference between an array or
     arrays and a multidimensional array in C) but for other source languages 
     (e.g. Ada) we probably shouldn't do this.  */
  /* ??? The SGI dwarf reader fails for multidimensional arrays with a
     const enum type.  E.g. const enum machine_mode insn_operand_mode[2][10].
     We work around this by disabling this feature.  See also
     gen_array_type_die.  */
#ifndef MIPS_DEBUGGING_INFO
  for (dimension_number = 0;
       TREE_CODE (type) == ARRAY_TYPE;
       type = TREE_TYPE (type), dimension_number++)
    {
#endif
      register tree domain = TYPE_DOMAIN (type);

      /* Arrays come in three flavors: Unspecified bounds, fixed bounds,
	 and (in GNU C only) variable bounds.  Handle all three forms 
         here.  */
      subrange_die = new_die (DW_TAG_subrange_type, type_die);
      if (domain)
	{
	  /* We have an array type with specified bounds.  */
	  lower = TYPE_MIN_VALUE (domain);
	  upper = TYPE_MAX_VALUE (domain);

	  /* TODO: establish DW_AT_type for the basis type a byte_size
	     attribute if the byte size is non-standard */
	  add_bound_info (subrange_die, DW_AT_lower_bound, lower);
	  add_bound_info (subrange_die, DW_AT_upper_bound, upper);
	}
      else
	{
	  /* We have an array type with an unspecified length. For C and C++
	     we can assume that this really means that (a) the index type is
	     an integral type, and (b) the lower bound is zero. Note that
	     Dwarf defines the representation of an unspecified (upper) bound 
	     as being a zero-length location description.  */

	  /* define the (assumed) index type.  */
	  add_AT_die_ref (subrange_die, DW_AT_type, int_base_type_die);

	  /* Add the (assumed) lower bound (constant) value.   */
	  add_AT_unsigned (subrange_die, DW_AT_lower_bound, 0);

	  /* Add the (empty) location description for the upper bound.  */
	  add_AT_loc (subrange_die, DW_AT_upper_bound, NULL);
	}
#ifndef MIPS_DEBUGGING_INFO
    }
#endif
}

static void
add_byte_size_attribute (die, tree_node)
     dw_die_ref die;
     register tree tree_node;
{
  register unsigned size;

  switch (TREE_CODE (tree_node))
    {
    case ERROR_MARK:
      size = 0;
      break;
    case ENUMERAL_TYPE:
    case RECORD_TYPE:
    case UNION_TYPE:
    case QUAL_UNION_TYPE:
      size = int_size_in_bytes (tree_node);
      break;
    case FIELD_DECL:
      /* For a data member of a struct or union, the DW_AT_byte_size is
         generally given as the number of bytes normally allocated for an
         object of the *declared* type of the member itself.  This is true
         even for bit-fields.  */
      size = simple_type_size_in_bits (field_type (tree_node)) / BITS_PER_UNIT;
      break;
    default:
      abort ();
    }

  /* Note that `size' might be -1 when we get to this point.  If it is, that
     indicates that the byte size of the entity in question is variable.  We
     have no good way of expressing this fact in Dwarf at the present time,
     so just let the -1 pass on through.  */

  add_AT_unsigned (die, DW_AT_byte_size, size);
}

/* For a FIELD_DECL node which represents a bit-field, output an attribute
   which specifies the distance in bits from the highest order bit of the
   "containing object" for the bit-field to the highest order bit of the
   bit-field itself.

   For any given bit-field, the "containing object" is a hypothetical object (of
   some integral or enum type) within which the given bit-field lives.  The
   type of this hypothetical "containing object" is always the same as the
   declared type of the individual bit-field itself.
   The determination of the exact location of the "containing object" for a
   bit-field is rather complicated.  It's handled by the `field_byte_offset'
   function (above).

   Note that it is the size (in bytes) of the hypothetical "containing object"
   which will be given in the DW_AT_byte_size attribute for this bit-field.
   (See `byte_size_attribute' above).  */
inline void
add_bit_offset_attribute (die, decl)
     register dw_die_ref die;
     register tree decl;
{
  register unsigned object_offset_in_bytes = field_byte_offset (decl);
  register tree type = DECL_BIT_FIELD_TYPE (decl);
  register tree bitpos_tree = DECL_FIELD_BITPOS (decl);
  register unsigned bitpos_int;
  register unsigned highest_order_object_bit_offset;
  register unsigned highest_order_field_bit_offset;
  register unsigned bit_offset;

  assert (TREE_CODE (decl) == FIELD_DECL);	/* Must be a field.  */
  assert (type);				/* Must be a bit field.  */

  /* We can't yet handle bit-fields whose offsets are variable, so if we
     encounter such things, just return without generating any attribute
     whatsoever.  */
  if (TREE_CODE (bitpos_tree) != INTEGER_CST)
    {
      return;
    }
  bitpos_int = (unsigned) TREE_INT_CST_LOW (bitpos_tree);

  /* Note that the bit offset is always the distance (in bits) from the
     highest-order bit of the "containing object" to the highest-order bit of 
     the bit-field itself.  Since the "high-order end" of any object or field 
     is different on big-endian and little-endian machines, the computation
     below must take account of these differences.  */
  highest_order_object_bit_offset = object_offset_in_bytes * BITS_PER_UNIT;
  highest_order_field_bit_offset = bitpos_int;

  if (!BYTES_BIG_ENDIAN)
    {
      highest_order_field_bit_offset
	+= (unsigned) TREE_INT_CST_LOW (DECL_SIZE (decl));

      highest_order_object_bit_offset += simple_type_size_in_bits (type);
    }
  bit_offset =
    (!BYTES_BIG_ENDIAN
     ? highest_order_object_bit_offset - highest_order_field_bit_offset
     : highest_order_field_bit_offset - highest_order_object_bit_offset);

  add_AT_unsigned (die, DW_AT_bit_offset, bit_offset);
}

/* For a FIELD_DECL node which represents a bit field, output an attribute
   which specifies the length in bits of the given field.  */
inline void
add_bit_size_attribute (die, decl)
     register dw_die_ref die;
     register tree decl;
{
  assert (TREE_CODE (decl) == FIELD_DECL);	/* Must be a field.  */
  assert (DECL_BIT_FIELD_TYPE (decl));		/* Must be a bit field.  */
  add_AT_unsigned (die, DW_AT_bit_size,
		   (unsigned) TREE_INT_CST_LOW (DECL_SIZE (decl)));
}

inline void
add_member_attribute (die, context)
     register dw_die_ref die;
     register tree context;
{
  register dw_die_ref type_die;

  /* Generate this attribute only for members in C++.  */
  if (context != NULL && is_tagged_type (context))
    {
      type_die = lookup_type_die (context);
      add_AT_die_ref (die, DW_AT_member, type_die);
    }
}

/* If the compiled language is GNU C, then add a 'prototyped'
   attribute, if arg types are given for the parameters of a function.  */
inline void
add_prototyped_attribute (die, func_type)
     register dw_die_ref die;
     register tree func_type;
{
  if ((strcmp (language_string, "GNU C") == 0)
      && (TYPE_ARG_TYPES (func_type) != NULL))
    {
      add_AT_flag (die, DW_AT_prototyped, 0);
    }
}


/* Add an 'abstract_origin' attribute below a given DIE.  The DIE is found
   by looking in either the type declaration or object declaration
   equate table.  */
inline void
add_abstract_origin_attribute (die, origin)
     register dw_die_ref die;
     register tree origin;
{
  dw_die_ref origin_die = NULL;
  if (TREE_CODE_CLASS (TREE_CODE (origin)) == 'd')
    {
      origin_die = lookup_decl_die (origin);
    }
  else if (TREE_CODE_CLASS (TREE_CODE (origin)) == 't')
    {
      origin_die = lookup_type_die (origin);
    }
  add_AT_die_ref (die, DW_AT_abstract_origin, origin_die);
}

/* If the compiled source program is  C++, define the pure_virtual
   attribute.  */
inline void
add_pure_or_virtual_attribute (die, func_decl)
     register dw_die_ref die;
     register tree func_decl;
{
  if (DECL_VIRTUAL_P (func_decl))
    {
      if ((strcmp (language_string, "GNU C++") == 0)
	  && (DECL_VIRTUAL_P (func_decl)))
	{
	  add_AT_unsigned (die, DW_AT_virtuality, DW_VIRTUALITY_pure_virtual);
	}
      else
	{
	  add_AT_unsigned (die, DW_AT_virtuality, DW_VIRTUALITY_virtual);
	}
    }
}

/********************* utility routines for DIEs *************************/

/* Add an DW_AT_name attribute and source coordinate attribute for the
   given decl, but only if it actually has a name.  */
static void
add_name_and_src_coords_attributes (die, decl)
     register dw_die_ref die;
     register tree decl;
{
  register tree decl_name = DECL_NAME (decl);
  register unsigned file_index;
  if (decl_name && IDENTIFIER_POINTER (decl_name))
    {
      add_name_attribute (die, IDENTIFIER_POINTER (decl_name));
      file_index = lookup_filename (DECL_SOURCE_FILE (decl));
      add_AT_unsigned (die, DW_AT_decl_file, file_index);
      add_AT_unsigned (die, DW_AT_decl_line, DECL_SOURCE_LINE (decl));
    }
}

/* Push a new declaration scope. */
static void
push_decl_scope (scope)
     tree scope;
{
  /* Make room in the decl_scope_table, if necessary.  */
  if (decl_scope_table_allocated == decl_scope_depth)
    {
      decl_scope_table_allocated += DECL_SCOPE_TABLE_INCREMENT;
      decl_scope_table = (tree *) xrealloc (decl_scope_table,
  	                   decl_scope_table_allocated * sizeof (tree));
    }
  decl_scope_table[decl_scope_depth++] = scope;
}

/* Return the DIE for the scope the immediately contains this declaration.  */
static dw_die_ref
scope_die_for_type (type, context_die)
    register tree type; 
    register dw_die_ref context_die;
{
  register dw_die_ref scope_die = NULL;
  register tree containing_scope;
  register unsigned long i;

  /* Walk back up the declaration tree looking for a place to define
     this type.  */
  containing_scope = TYPE_CONTEXT (type);
  if (containing_scope == NULL)
    {
      scope_die = comp_unit_die;
    }
  else
    {
      for (i = decl_scope_depth - 1, scope_die = context_die;
	   i >= 0
	   && scope_die != NULL
	   && decl_scope_table[i] != containing_scope;
	   --i, scope_die = scope_die->die_parent)
	{
	  /* nothing */ ;
	}
      if (scope_die == NULL)
	{
	  scope_die = context_die;
	}
    }
  return scope_die;
}

/* Pop a declaration scope.  */
inline void
pop_decl_scope ()
{
  assert (decl_scope_depth > 0);
  --decl_scope_depth;
}

/* Many forms of DIEs require a "type description" attribute.  This
   routine locates the proper "type descriptor" die for the type given
   by 'type', and adds an DW_AT_type attribute below the given die.  */
static void
add_type_attribute (object_die, type, decl_const, decl_volatile, context_die)
     register dw_die_ref object_die;
     register tree type;
     register int decl_const;
     register int decl_volatile;
     register dw_die_ref context_die;
{
  register enum tree_code code  = TREE_CODE (type);
  register dw_die_ref scope_die = NULL;
  register dw_die_ref type_die  = NULL;

  if (code == ERROR_MARK)
    {
      return;
    }

  /* Handle a special case.  For functions whose return type is void, we
     generate *no* type attribute.  (Note that no object may have type
     `void', so this only applies to function return types).  */
  if (code == VOID_TYPE)
    {
      return;
    }

  scope_die = scope_die_for_type (type, context_die);
  type_die = modified_type_die (type,
				decl_const || TYPE_READONLY (type),
				decl_volatile || TYPE_VOLATILE (type),
				scope_die);
  if (type_die != NULL)
    {
      add_AT_die_ref (object_die, DW_AT_type, type_die);
    }
}

/* Given a tree pointer to a struct, class, union, or enum type node, return
   a pointer to the (string) tag name for the given type, or zero if the type
   was declared without a tag.  */
static char *
type_tag (type)
     register tree type;
{
  register char *name = 0;

  if (TYPE_NAME (type) != 0)
    {
      register tree t = 0;

      /* Find the IDENTIFIER_NODE for the type name.  */
      if (TREE_CODE (TYPE_NAME (type)) == IDENTIFIER_NODE)
	t = TYPE_NAME (type);
#if 0
      /* The g++ front end makes the TYPE_NAME of *each* tagged type point to 
         a TYPE_DECL node, regardless of whether or not a `typedef' was
         involved.  This is distinctly different from what the gcc front-end
         does.  It always makes the TYPE_NAME for each tagged type be either
         NULL (signifying an anonymous tagged type) or else a pointer to an
         IDENTIFIER_NODE.  Obviously, we would like to generate correct Dwarf 
         for both C and C++, but given this inconsistency in the TREE
         representation of tagged types for C and C++ in the GNU front-ends,
         we cannot support both languages correctly unless we introduce some
         front-end specific code here, and rms objects to that, so we can
         only generate correct Dwarf for one of these two languages.  C is
         more important, so for now we'll do the right thing for C and let
         g++ go fish.  */
      else if (TREE_CODE (TYPE_NAME (type)) == TYPE_DECL)
	t = DECL_NAME (TYPE_NAME (type));
#endif
      /* Now get the name as a string, or invent one.  */
      if (t != 0)
	{
	  name = IDENTIFIER_POINTER (t);
	}
    }
  return (name == 0 || *name == '\0') ? 0 : name;
}

/* Return the type associated with a data member, make a special check
   for bit field types.  */
inline tree
member_declared_type (member)
     register tree member;
{
  return (DECL_BIT_FIELD_TYPE (member))
    ? DECL_BIT_FIELD_TYPE (member)
    : TREE_TYPE (member);
}

/* Get the function's label, as described by its RTL. This may be different
   from the DECL_NAME name used in the source file.  */
static char *
function_start_label (decl)
     register tree decl;
{
  rtx x;
  char *fnname;
  x = DECL_RTL (decl);
  if (GET_CODE (x) != MEM)
    {
      abort ();
    }
  x = XEXP (x, 0);
  if (GET_CODE (x) != SYMBOL_REF)
    {
      abort ();
    }
  fnname = XSTR (x, 0);
  return fnname;
}

/******************************* DIE Generation *************************/

/* These routines generate the internnal representation of the DIE's for
   the compilation unit.  Debugging information is collected by walking
   the declaration trees passed in from dwarfout_file_scope_decl().  */

static void
gen_array_type_die (type, context_die)
     register tree type;
     register dw_die_ref context_die;
{
  register dw_die_ref scope_die = scope_die_for_type (type, context_die);
  register dw_die_ref array_die = new_die (DW_TAG_array_type, scope_die);
  register tree element_type;
  /* TODO: why a member_attribute under an array?
     member_attribute (array_die, TYPE_CONTEXT (type)); */
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

  add_subscript_info (array_die, type);

  equate_type_number_to_die (type, array_die);

  /* Add representation of the type of the elements of this array type.  */
  element_type = TREE_TYPE (type);
  /* ??? The SGI dwarf reader fails for multidimensional arrays with a
     const enum type.  E.g. const enum machine_mode insn_operand_mode[2][10].
     We work around this by disabling this feature.  See also
     add_subscript_info.  */
#ifndef MIPS_DEBUGGING_INFO
  while (TREE_CODE (element_type) == ARRAY_TYPE)
    {
      element_type = TREE_TYPE (element_type);
    }
#endif
  gen_type_die (element_type, context_die);

  add_type_attribute (array_die, element_type, 0, 0, context_die);
}

static void
gen_set_type_die (type, context_die)
     register tree type;
     register dw_die_ref context_die;
{
  register dw_die_ref type_die;
  type_die = new_die (DW_TAG_set_type, scope_die_for_type (type, context_die));
  equate_type_number_to_die (type, type_die);
  add_member_attribute (type_die, TYPE_CONTEXT (type));
  add_type_attribute (type_die, TREE_TYPE (type), 0, 0, context_die);
}

static void
gen_entry_point_die (decl, context_die)
     register tree decl;
     register dw_die_ref context_die;
{
  register tree origin = decl_ultimate_origin (decl);
  register dw_die_ref decl_die = new_die (DW_TAG_entry_point, context_die);
  if (origin != NULL)
    {
      add_abstract_origin_attribute (decl_die, origin);
    }
  else
    {
      add_name_and_src_coords_attributes (decl_die, decl);
      add_member_attribute (decl_die, DECL_CONTEXT (decl));
      add_type_attribute (decl_die, TREE_TYPE (TREE_TYPE (decl)),
			  0, 0, context_die);
    }
  if (DECL_ABSTRACT (decl))
    {
      equate_decl_number_to_die (decl, decl_die);
    }
  else
    {
      add_AT_lbl_id (decl_die, DW_AT_low_pc, function_start_label (decl));
    }
}

/* Generate a DIE to represent an inlined instance of an enumeration type.  */
static void
gen_inlined_enumeration_type_die (type, context_die)
     register tree type;
     register dw_die_ref context_die;
{
  register dw_die_ref type_die;
  type_die = new_die (DW_TAG_enumeration_type,
		      scope_die_for_type (type, context_die));
  assert (TREE_ASM_WRITTEN (type));
  add_abstract_origin_attribute (type_die, type);
}

/* Generate a DIE to represent an inlined instance of a structure type.  */
static void
gen_inlined_structure_type_die (type, context_die)
     register tree type;
     register dw_die_ref context_die;
{
  register dw_die_ref type_die;
  type_die = new_die (DW_TAG_structure_type,
		      scope_die_for_type (type, context_die));
  assert (TREE_ASM_WRITTEN (type));
  add_abstract_origin_attribute (type_die, type);
}

/* Generate a DIE to represent an inlined instance of a union type.  */
static void
gen_inlined_union_type_die (type, context_die)
     register tree type;
     register dw_die_ref context_die;
{
  register dw_die_ref type_die;
  type_die = new_die (DW_TAG_union_type,
		      scope_die_for_type (type, context_die));
  assert (TREE_ASM_WRITTEN (type));
  add_abstract_origin_attribute (type_die, type);
}

/* Generate a DIE to represent an enumeration type.  Note that these DIEs
   include all of the information about the enumeration values also. Each
   enumerated type name/value is listed as a child of the enumerated type DIE */
static void
gen_enumeration_type_die (type, is_complete, context_die)
     register tree type;
     register unsigned is_complete;
     register dw_die_ref context_die;
{
  register dw_die_ref type_die;
  register dw_die_ref enum_die;
  register tree link;
  type_die = lookup_type_die (type);
  if (type_die == NULL)
    {
      type_die = new_die (DW_TAG_enumeration_type,
			  scope_die_for_type (type, context_die));
      equate_type_number_to_die (type, type_die);
      add_name_attribute (type_die, type_tag (type));
      add_member_attribute (type_die, TYPE_CONTEXT (type));
    }
  if (is_complete)
    {
      /* Handle a GNU C/C++ extension, i.e. incomplete enum types.  If the
         given enum type is incomplete, do not generate the DW_AT_byte_size
         attribute or the DW_AT_element_list attribute.  */
      if (TYPE_SIZE (type))
	{
	  add_byte_size_attribute (type_die, type);
	  for (link = TYPE_FIELDS (type);
	       link != NULL; link = TREE_CHAIN (link))
	    {
	      enum_die = new_die (DW_TAG_enumerator, type_die);
	      add_name_attribute (enum_die,
				  IDENTIFIER_POINTER (TREE_PURPOSE (link)));
	      add_AT_unsigned (enum_die, DW_AT_const_value,
			   (unsigned) TREE_INT_CST_LOW (TREE_VALUE (link)));
	    }
	}
    }
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
   argument type of some subprogram type.  */
static void
gen_formal_parameter_die (node, context_die)
     register tree node;
     register dw_die_ref context_die;
{
  register dw_die_ref parm_die = new_die (DW_TAG_formal_parameter,
					  context_die);
  register tree origin;
  switch (TREE_CODE_CLASS (TREE_CODE (node)))
    {
      /* We were called with some kind of a ..._DECL node.  */
    case 'd':
      origin = decl_ultimate_origin (node);
      if (origin != NULL)
	{
	  add_abstract_origin_attribute (parm_die, origin);
	}
      else
	{
	  add_name_and_src_coords_attributes (parm_die, node);
	  add_type_attribute (parm_die, TREE_TYPE (node),
			      TREE_READONLY (node),
			      TREE_THIS_VOLATILE (node),
			      context_die);
	}
      if (DECL_ABSTRACT (node))
	{
	  equate_decl_number_to_die (node, parm_die);
	}
      else
	{
	  add_location_or_const_value_attribute (parm_die, node);
	}
      break;

      /* We were called with some kind of a ..._TYPE node.  */
    case 't':
      add_type_attribute (parm_die, node, 0, 0, context_die);
      break;

      /* Should never happen.  */
    default:
      abort ();
    }
}

/* Generate a special type of DIE used as a stand-in for a trailing ellipsis
   at the end of an (ANSI prototyped) formal parameters list.  */
static void
gen_unspecified_parameters_die (decl_or_type, context_die)
     register tree decl_or_type;
     register dw_die_ref context_die;
{
  register dw_die_ref parm_die = new_die (DW_TAG_unspecified_parameters,
					  context_die);
  /* This kludge is here only for the sake of being compatible with what the
     USL CI5 C compiler does.  The specification of Dwarf Version 1 doesn't
     say that DW_TAG_unspecified_parameters DIEs should contain any
     attributes other than the DW_AT_sibling attribute, but they are
     certainly allowed to contain additional attributes, and the CI5 compiler 
     generates DW_AT_name, DW_AT_base_type, and DW_AT_location attributes
     within DW_TAG_unspecified_parameters DIEs which appear in the child
     lists for DIEs representing function definitions, so we do likewise
     here.  */
  if (TREE_CODE (decl_or_type) == FUNCTION_DECL
      && DECL_INITIAL (decl_or_type))
    {
      add_name_attribute (parm_die, "...");
      add_AT_die_ref (parm_die, DW_AT_type, int_base_type_die);
    }
}

/* Generate a list of nameless DW_TAG_formal_parameter DIEs (and perhaps a
   DW_TAG_unspecified_parameters DIE) to represent the types of the formal
   parameters as specified in some function type specification (except for
   those which appear as part of a function *definition*).
   Note that we must be careful here to output all of the parameter DIEs before*
   we output any DIEs needed to represent the types of the formal parameters.
   This keeps svr4 SDB happy because it (incorrectly) thinks that the first
   non-parameter DIE it sees ends the formal parameter list.  */
static void
gen_formal_types_die (function_or_method_type, context_die)
     register tree function_or_method_type;
     register dw_die_ref context_die;
{
  register tree link;
  register tree formal_type = NULL;
  register tree first_parm_type = TYPE_ARG_TYPES (function_or_method_type);

  /* In the case where we are generating a formal types list for a C++
     non-static member function type, skip over the first thing on the
     TYPE_ARG_TYPES list because it only represents the type of the hidden
     `this pointer'.  The debugger should be able to figure out (without
     being explicitly told) that this non-static member function type takes a 
     `this pointer' and should be able to figure what the type of that hidden 
     parameter is from the DW_AT_member attribute of the parent
     DW_TAG_subroutine_type DIE.  */
  if (TREE_CODE (function_or_method_type) == METHOD_TYPE)
    first_parm_type = TREE_CHAIN (first_parm_type);

  /* Make our first pass over the list of formal parameter types and output a 
     DW_TAG_formal_parameter DIE for each one.  */
  for (link = first_parm_type; link; link = TREE_CHAIN (link))
    {
      formal_type = TREE_VALUE (link);
      if (formal_type == void_type_node)
	break;

      /* Output a (nameless) DIE to represent the formal parameter itself.  */
      gen_formal_parameter_die (formal_type, context_die);
    }

  /* If this function type has an ellipsis, add a
     DW_TAG_unspecified_parameters DIE to the end of the parameter list.  */
  if (formal_type != void_type_node)
    gen_unspecified_parameters_die (function_or_method_type, context_die);

  /* Make our second (and final) pass over the list of formal parameter types 
     and output DIEs to represent those types (as necessary).  */
  for (link = TYPE_ARG_TYPES (function_or_method_type);
       link;
       link = TREE_CHAIN (link))
    {
      formal_type = TREE_VALUE (link);
      if (formal_type == void_type_node)
	break;

      gen_type_die (formal_type, function_or_method_type, context_die);
    }
}

/* Generate a DIE to represent a declared function (either file-scope or
   block-local).  */
static void
gen_subprogram_die (decl, context_die)
     register tree decl;
     register dw_die_ref context_die;
{
  char label_id[MAX_ARTIFICIAL_LABEL_BYTES];
  register tree origin = decl_ultimate_origin (decl);
  register dw_die_ref subr_die = new_die (DW_TAG_subprogram, context_die);
  register dw_loc_descr_ref fp_loc = NULL;
  register unsigned fp_reg;
  register tree type;
  register tree fn_arg_types;
  register tree outer_scope;
  register tree label;

  if (origin != NULL)
    {
      add_abstract_origin_attribute (subr_die, origin);
    }
  else
    {
      if (TREE_PUBLIC (decl) || DECL_EXTERNAL (decl))
	{
	  add_AT_flag (subr_die, DW_AT_external, 1);
	}
      add_name_and_src_coords_attributes (subr_die, decl);
      if (DECL_INLINE (decl))
	{
	  add_AT_unsigned (subr_die, DW_AT_inline, DW_INL_inlined);
	}
      type = TREE_TYPE (decl);
      add_prototyped_attribute (subr_die, type);
      add_member_attribute (subr_die, DECL_CONTEXT (decl));
      add_type_attribute (subr_die, TREE_TYPE (type), 0, 0, context_die);
      add_pure_or_virtual_attribute (subr_die, decl);
    }
  if (DECL_ABSTRACT (decl))
    {
      equate_decl_number_to_die (decl, subr_die);
    }
  else if (!DECL_EXTERNAL (decl))
    {
      if (origin == NULL)
	equate_decl_number_to_die (decl, subr_die);
      add_AT_lbl_id (subr_die, DW_AT_low_pc, function_start_label (decl));
      sprintf (label_id, FUNC_END_LABEL_FMT, current_funcdef_number);
      add_AT_lbl_id (subr_die, DW_AT_high_pc, label_id);

#ifdef MIPS_DEBUGGING_INFO

      /* Add a reference to the FDE for this routine.  */
      add_AT_fde_ref (subr_die, DW_AT_MIPS_fde, current_funcdef_fde);
#endif

      /* Define the frame pointer location for this routine.  */
      fp_reg = (frame_pointer_needed) ? FRAME_POINTER_REGNUM
				      : STACK_POINTER_REGNUM;
      assert (fp_reg >= 0 && fp_reg <= 31);
      fp_loc = new_loc_descr (DW_OP_breg0 + fp_reg, current_funcdef_frame_size);
      add_AT_loc (subr_die, DW_AT_frame_base, fp_loc);

#ifdef DWARF_GNU_EXTENSIONS
      sprintf (label_id, BODY_BEGIN_LABEL_FMT, current_funcdef_number);
      add_AT_lbl_id (subr_die, DW_AT_body_begin, label_id);
      sprintf (label_id, BODY_END_LABEL_FMT, current_funcdef_number);
      add_AT_lbl_id (subr_die, DW_AT_body_end, label_id);
#endif

    }

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
  if (DECL_INITIAL (decl) == NULL_TREE)
    {
      gen_formal_types_die (TREE_TYPE (decl), subr_die);
    }
  else
    {
      /* Generate DIEs to represent all known formal parameters */
      register tree arg_decls = DECL_ARGUMENTS (decl);
      register tree parm;

      /* When generating DIEs, generate the unspecified_parameters DIE
         instead if we come across the arg "__builtin_va_alist" */
      for (parm = arg_decls; parm; parm = TREE_CHAIN (parm))
	{
	  if (TREE_CODE (parm) == PARM_DECL)
	    {
	      if (DECL_NAME (parm) &&
		  !strcmp (IDENTIFIER_POINTER (DECL_NAME (parm)),
			   "__builtin_va_alist"))
		{
		  gen_unspecified_parameters_die (parm, subr_die);
		}
	      else
		{
		  gen_decl_die (parm, subr_die);
		}
	    }
	}

      /* Decide whether we need a unspecified_parameters DIE at the end.
         There are 2 more cases to do this for: 1) the ansi ... declaration - 
         this is detectable when the end of the arg list is not a
         void_type_node 2) an unprototyped function declaration (not a
         definition).  This just means that we have no info about the
         parameters at all.  */
      fn_arg_types = TYPE_ARG_TYPES (TREE_TYPE (decl));
      if (fn_arg_types)
	{
	  /* this is the prototyped case, check for ...  */
	  if (TREE_VALUE (tree_last (fn_arg_types)) != void_type_node)
	    {
	      gen_unspecified_parameters_die (decl, subr_die);
	    }
	}
      else
	{
	  /* this is unprotoyped, check for undefined (just declaration) */
	  if (!DECL_INITIAL (decl))
	    {
	      gen_unspecified_parameters_die (decl, subr_die);
	    }
	}
    }

  /* Output Dwarf info for all of the stuff within the body of the function
     (if it has one - it may be just a declaration).  */
  outer_scope = DECL_INITIAL (decl);

  if (outer_scope && TREE_CODE (outer_scope) != ERROR_MARK)
    {
      /* Note that here, `outer_scope' is a pointer to the outermost BLOCK
         node created to represent a function. This outermost BLOCK actually
         represents the outermost binding contour for the function, i.e. the
         contour in which the function's formal parameters and labels get
         declared. Curiously, it appears that the front end doesn't actually
         put the PARM_DECL nodes for the current function onto the BLOCK_VARS 
         list for this outer scope.  (They are strung off of the
         DECL_ARGUMENTS list for the function instead.) The BLOCK_VARS list
         for the `outer_scope' does provide us with a list of the LABEL_DECL
         nodes for the function however, and we output DWARF info for those
         here. Just within the `outer_scope' there will be another BLOCK node
         representing the function's outermost pair of curly braces.  We
         musn't generate a lexical_block DIE for this outermost pair of curly
         braces because that is not really an independent scope according to
         ANSI C rules.  Rather, it is the same scope in which the parameters
         were declared.  */
      for (label = BLOCK_VARS (outer_scope);
	   label;
	   label = TREE_CHAIN (label))
	{
	  gen_decl_die (label, subr_die);
	}

      /* Note here that `BLOCK_SUBBLOCKS (outer_scope)' points to a list of
         BLOCK nodes which is always only one element long. That one element
         represents the outermost pair of curley braces for the function
         body.  */
      decls_for_scope (BLOCK_SUBBLOCKS (outer_scope), subr_die);
    }
}

/* Generate a DIE to represent a declared data object.  */
static void
gen_variable_die (decl, context_die)
     register tree decl;
     register dw_die_ref context_die;
{
  register tree origin = decl_ultimate_origin (decl);
  register dw_die_ref var_die = new_die (DW_TAG_variable, context_die);
  if (TREE_PUBLIC (decl) || DECL_EXTERNAL (decl))
    {
      add_AT_flag (var_die, DW_AT_external, 1);
    }
  if (origin != NULL)
    {
      add_abstract_origin_attribute (var_die, origin);
    }
  else
    {
      add_name_and_src_coords_attributes (var_die, decl);
      add_member_attribute (var_die, DECL_CONTEXT (decl));
      add_type_attribute (var_die, TREE_TYPE (decl),
			  TREE_READONLY (decl),
			  TREE_THIS_VOLATILE (decl), context_die);
    }
  if (DECL_ABSTRACT (decl))
    {
      equate_decl_number_to_die (decl, var_die);
    }
  else if (!DECL_EXTERNAL (decl))
    {
      add_location_or_const_value_attribute (var_die, decl);
    }
}

/* Generate a DIE to represent a label identifier.  */
static void
gen_label_die (decl, context_die)
     register tree decl;
     register dw_die_ref context_die;
{
  register tree origin = decl_ultimate_origin (decl);
  register dw_die_ref lbl_die = new_die (DW_TAG_label, context_die);
  register rtx insn;
  char label[MAX_ARTIFICIAL_LABEL_BYTES];
  if (origin != NULL)
    {
      add_abstract_origin_attribute (lbl_die, origin);
    }
  else
    {
      add_name_and_src_coords_attributes (lbl_die, decl);
    }
  if (DECL_ABSTRACT (decl))
    {
      equate_decl_number_to_die (decl, lbl_die);
    }
  else
    {
      insn = DECL_RTL (decl);
      if (GET_CODE (insn) == CODE_LABEL)
	{
	  /* When optimization is enabled (via -O) some parts of the compiler 
	     (e.g. jump.c and cse.c) may try to delete CODE_LABEL insns which 
	     represent source-level labels which were explicitly declared by
	     the user.  This really shouldn't be happening though, so catch
	     it if it ever does happen.  */
	  if (INSN_DELETED_P (insn))
	    {
	      abort ();		/* Should never happen.  */
	    }
	  sprintf (label, INSN_LABEL_FMT, current_funcdef_number,
		   (unsigned) INSN_UID (insn));
	  add_AT_lbl_id (lbl_die, DW_AT_low_pc, label);
	}
    }
}

/* Generate a DIE for a lexical block.  */
static void
gen_lexical_block_die (stmt, context_die)
     register tree stmt;
     register dw_die_ref context_die;
{
  register dw_die_ref stmt_die = new_die (DW_TAG_lexical_block, context_die);
  char label[MAX_ARTIFICIAL_LABEL_BYTES];
  if (!BLOCK_ABSTRACT (stmt))
    {
      sprintf (label, BLOCK_BEGIN_LABEL_FMT, next_block_number);
      add_AT_lbl_id (stmt_die, DW_AT_low_pc, label);
      sprintf (label, BLOCK_END_LABEL_FMT, next_block_number);
      add_AT_lbl_id (stmt_die, DW_AT_high_pc, label);
    }
  decls_for_scope (stmt, stmt_die);
}

/* Generate a DIE for an inlined subprogram.  */
static void
gen_inlined_subroutine_die (stmt, context_die)
     register tree stmt;
     register dw_die_ref context_die;
{
  register dw_die_ref subr_die = new_die (DW_TAG_inlined_subroutine,
					  context_die);
  char label[MAX_ARTIFICIAL_LABEL_BYTES];
  add_abstract_origin_attribute (subr_die, block_ultimate_origin (stmt));
  if (!BLOCK_ABSTRACT (stmt))
    {
      sprintf (label, BLOCK_BEGIN_LABEL_FMT, next_block_number);
      add_AT_lbl_id (subr_die, DW_AT_low_pc, label);
      sprintf (label, BLOCK_END_LABEL_FMT, next_block_number);
      add_AT_lbl_id (subr_die, DW_AT_high_pc, label);
    }
  decls_for_scope (stmt, subr_die);
}

/* Generate a DIE for a field in a record, or structure.  */
static void
gen_field_die (decl, context_die)
     register tree decl;
     register dw_die_ref context_die;
{
  register dw_die_ref decl_die = new_die (DW_TAG_member, context_die);
  add_name_and_src_coords_attributes (decl_die, decl);
  add_member_attribute (decl_die, DECL_CONTEXT (decl));
  add_type_attribute (decl_die, member_declared_type (decl),
		      TREE_READONLY (decl), TREE_THIS_VOLATILE (decl),
		      context_die);
  /* If this is a bit field...  */
  if (DECL_BIT_FIELD_TYPE (decl))
    {
      add_byte_size_attribute (decl_die, decl);
      add_bit_size_attribute (decl_die, decl);
      add_bit_offset_attribute (decl_die, decl);
    }
  add_data_member_location_attribute (decl_die, decl);
}

/* Don't generate either pointer_type DIEs or reference_type DIEs.
   Use modified type DIE's instead.
   We keep this code here just in case these types of DIEs may be needed to
   represent certain things in other languages (e.g. Pascal) someday.  */
static void
gen_pointer_type_die (type, context_die)
     register tree type;
     register dw_die_ref context_die;
{
  register dw_die_ref ptr_die = new_die (DW_TAG_pointer_type, context_die);
  equate_type_number_to_die (type, ptr_die);
  add_member_attribute (ptr_die, TYPE_CONTEXT (type));
  add_type_attribute (ptr_die, TREE_TYPE (type), 0, 0, context_die);
}

/* Don't generate either pointer_type DIEs or reference_type DIEs.
   Use modified type DIE's instead.
   We keep this code here just in case these types of DIEs may be needed to
   represent certain things in other languages (e.g. Pascal) someday.  */
static void
gen_reference_type_die (type, context_die)
     register tree type;
     register dw_die_ref context_die;
{
  register dw_die_ref ref_die = new_die (DW_TAG_reference_type, context_die);
  equate_type_number_to_die (type, ref_die);
  add_member_attribute (ref_die, TYPE_CONTEXT (type));
  add_type_attribute (ref_die, TREE_TYPE (type), 0, 0, context_die);
}

/* Generate a DIE for a pointer to a member type.  */
static void
gen_ptr_to_mbr_type_die (type, context_die)
     register tree type;
     register dw_die_ref context_die;
{
  register dw_die_ref ptr_die = new_die (DW_TAG_ptr_to_member_type,
					 context_die);
  equate_type_number_to_die (type, ptr_die);
  add_member_attribute (ptr_die, TYPE_CONTEXT (type));
  add_AT_die_ref (ptr_die, DW_AT_containing_type,
	      lookup_type_die (TYPE_OFFSET_BASETYPE (type)));
  add_type_attribute (ptr_die, TREE_TYPE (type), 0, 0, context_die);
}

/* Generate the DIE for the compilation unit.  */
static void
gen_compile_unit_die (main_input_filename)
     register char *main_input_filename;
{
  char producer[250];
  char full_src_name[1024];
  char *wd = getpwd ();

  comp_unit_die = new_die (DW_TAG_compile_unit, NULL);

  /* MIPS/SGI requires the full pathname of the input file.  */
  if (main_input_filename[0] == '/')
    {
      add_name_attribute (comp_unit_die, main_input_filename);
    }
  else
    {
      sprintf (full_src_name, "%s/%s", wd, main_input_filename);
      add_name_attribute (comp_unit_die, full_src_name);
    }

  sprintf (producer, "%s %s", language_string, version_string);

#ifdef MIPS_DEBUGGING_INFO
  /* The MIPS/SGI compilers place the 'cc' command line options in the producer
     string.  The SGI debugger looks for -g, -g1, -g2, or -g3; if they do
     not appear in the producer string, the debugger reaches the conclusion
     that the object file is stripped and has no debugging information.
     To get the MIPS/SGI debugger to believe that there is debugging
     information in the object file, we add a -g to the producer string.  */
  if (write_symbols != NO_DEBUG)
    {
       strcpy (producer, " -g");
    }

#endif

  add_AT_string (comp_unit_die, DW_AT_producer, producer);
  if (strcmp (language_string, "GNU C++") == 0)
    {
      add_AT_unsigned (comp_unit_die, DW_AT_language, DW_LANG_C_plus_plus);
    }
  else if (strcmp (language_string, "GNU Ada") == 0)
    {
      add_AT_unsigned (comp_unit_die, DW_AT_language, DW_LANG_Ada83);
    }
  else if (flag_traditional)
    {
      add_AT_unsigned (comp_unit_die, DW_AT_language, DW_LANG_C);
    }
  else
    {
      add_AT_unsigned (comp_unit_die, DW_AT_language, DW_LANG_C89);
    }
  add_AT_lbl_id (comp_unit_die, DW_AT_low_pc, TEXT_BEGIN_LABEL);
  add_AT_lbl_id (comp_unit_die, DW_AT_high_pc, TEXT_END_LABEL);
  if (wd)
    {
      add_AT_string (comp_unit_die, DW_AT_comp_dir, wd);
    }
  if (debug_info_level >= DINFO_LEVEL_NORMAL)
    {
      add_AT_section_offset (comp_unit_die, DW_AT_stmt_list, LINE_SECTION);
      if (debug_info_level >= DINFO_LEVEL_VERBOSE)
	{
	  add_AT_unsigned (comp_unit_die, DW_AT_macro_info, 0);
	}
    }
}

/* Generate a DIE for a string type.  */
static void
gen_string_type_die (type, context_die)
     register tree type;
     register dw_die_ref context_die;
{
  register dw_die_ref type_die;
  type_die = new_die (DW_TAG_string_type,
		      scope_die_for_type (type, context_die));
  add_member_attribute (type_die, TYPE_CONTEXT (type));

  /* Fudge the string length attribute for now.  */

  /* TODO: add string length info.
     string_length_attribute (TYPE_MAX_VALUE (TYPE_DOMAIN (type)));
			      bound_representation (upper_bound, 0, 'u'); */
}

/* Genearate a DIE for a class member.  */
static void
gen_member_die (type, context_die)
     register tree type;
     register dw_die_ref context_die;
{
  register tree normal_member;
  register tree vec_base;
  register tree first_func_member;
  register tree func_member;
  /* If this is not an incomplete type, output descriptions of each of its
     members. Note that as we output the DIEs necessary to represent the
     members of this record or union type, we will also be trying to output
     DIEs to represent the *types* of those members. However the `type'
     function (above) will specifically avoid generating type DIEs for member 
     types *within* the list of member DIEs for this (containing) type execpt 
     for those types (of members) which are explicitly marked as also being
     members of this (containing) type themselves.  The g++ front- end can
     force any given type to be treated as a member of some other
     (containing) type by setting the TYPE_CONTEXT of the given (member) type 
     to point to the TREE node representing the appropriate (containing)
     type.  */

  /* First output info about the data members and type members.  */
  for (normal_member = TYPE_FIELDS (type);
       normal_member;
       normal_member = TREE_CHAIN (normal_member))
    {
      gen_decl_die (normal_member, context_die);
    }

  /* Now output info about the function members (if any).  */
  vec_base = TYPE_METHODS (type);
  if (vec_base)
    {
      first_func_member = TREE_VEC_ELT (vec_base, 0);
      /* This isn't documented, but the first element of the vector of member 
         functions can be NULL in cases where the class type in question
         didn't have either a constructor or a destructor declared for it.
         We have to make allowances for that here.  */
      if (first_func_member == NULL)
	{
	  first_func_member = TREE_VEC_ELT (vec_base, 1);
	}

      for (func_member = first_func_member;
	   func_member;
	   func_member = TREE_CHAIN (func_member))
	{
	  gen_decl_die (func_member, context_die);
	}
    }
}

/* Generate a DIE for a structure or union type.  */
static void
gen_struct_or_union_type_die (type, is_complete, context_die)
     register tree type;
     register unsigned is_complete;
     register dw_die_ref context_die;
{
  register dw_die_ref type_die;
  type_die = lookup_type_die (type);
  if (type_die == NULL)
    {
      type_die = new_die (TREE_CODE (type) == RECORD_TYPE
			  ? DW_TAG_structure_type : DW_TAG_union_type,
			  scope_die_for_type (type, context_die));
      equate_type_number_to_die (type, type_die);
      add_name_attribute (type_die, type_tag (type));
      add_member_attribute (type_die, TYPE_CONTEXT (type));
    }

  /* If this type has been completed, then give it a byte_size attribute and
     then give a list of members.  */
  if (is_complete)
    {
      /* Prevent infinite recursion in cases where the type of some member of 
         this type is expressed in terms of this type itself.  */
      TREE_ASM_WRITTEN (type) = 1;
      if (TYPE_SIZE (type))
	{
	  add_byte_size_attribute (type_die, type);
	  gen_member_die (type, type_die);
	}
    }
}

/* Generate a DIE for a subroutine _type_.  */
static void
gen_subroutine_type_die (type, context_die)
     register tree type;
     register dw_die_ref context_die;
{
  register tree return_type = TREE_TYPE (type);
  register dw_die_ref subr_die = new_die (DW_TAG_subroutine_type, context_die);
  equate_type_number_to_die (type, subr_die);
  add_prototyped_attribute (subr_die, type);
  add_member_attribute (subr_die, TYPE_CONTEXT (type));
  add_type_attribute (subr_die, return_type, 0, 0, context_die);
  gen_formal_types_die (type, context_die);
}

/* Generate a DIE for a type definition */
static void
gen_typedef_die (decl, context_die)
     register tree decl;
     register dw_die_ref context_die;
{
  register tree origin = decl_ultimate_origin (decl);
  register dw_die_ref type_die;
  type_die = new_die (DW_TAG_typedef,
		      scope_die_for_type (decl, context_die));
  if (origin != NULL)
    {
      add_abstract_origin_attribute (type_die, origin);
    }
  else
    {
      add_name_and_src_coords_attributes (type_die, decl);
      add_member_attribute (type_die, DECL_CONTEXT (decl));
      add_type_attribute (type_die, TREE_TYPE (decl),
			  TREE_READONLY (decl),
			  TREE_THIS_VOLATILE (decl),
			  context_die);
    }
  if (DECL_ABSTRACT (decl))
    {
      equate_decl_number_to_die (decl, type_die);
    }
}

/* Generate a type description DIE.  */
static void
gen_type_die (type, context_die)
     register tree type;
     register dw_die_ref context_die;
{
  register unsigned is_complete;
  if (type == 0 || type == error_mark_node)
    {
      return;
    }

  /* We are going to output a DIE to represent the unqualified version of of
     this type (i.e. without any const or volatile qualifiers) so get the
     main variant (i.e. the unqualified version) of this type now.  */
  type = type_main_variant (type);

  if (TREE_ASM_WRITTEN (type))
    {
      return;
    }

  switch (TREE_CODE (type))
    {
    case ERROR_MARK:
      break;

    case POINTER_TYPE:
    case REFERENCE_TYPE:
      /* For these types, all that is required is that we output a DIE (or a
         set of DIEs) to represent the "basis" type.  */
      gen_type_die (TREE_TYPE (type), context_die);
      break;

    case OFFSET_TYPE:
      /* This code is used for C++ pointer-to-data-member types.  */
      /* Output a description of the relevant class type.  */
      gen_type_die (TYPE_OFFSET_BASETYPE (type), context_die);
      /* Output a description of the type of the object pointed to.  */
      gen_type_die (TREE_TYPE (type), context_die);
      /* Now output a DIE to represent this pointer-to-data-member type
         itself.  */
      gen_ptr_to_mbr_type_die (type, context_die);
      break;

    case SET_TYPE:
      gen_type_die (TYPE_DOMAIN (type), context_die);
      gen_set_type_die (type, context_die);
      break;

    case FILE_TYPE:
      gen_type_die (TREE_TYPE (type), context_die);
      abort ();			/* No way to represent these in Dwarf yet!  */
      break;

    case FUNCTION_TYPE:
      /* Force out return type (in case it wasn't forced out already).  */
      gen_type_die (TREE_TYPE (type), context_die);
      gen_subroutine_type_die (type, context_die);
      break;

    case METHOD_TYPE:
      /* Force out return type (in case it wasn't forced out already).  */
      gen_type_die (TREE_TYPE (type), context_die);
      gen_subroutine_type_die (type, context_die);
      break;

    case ARRAY_TYPE:
      if (TYPE_STRING_FLAG (type) && TREE_CODE (TREE_TYPE (type)) == CHAR_TYPE)
	{
	  gen_type_die (TREE_TYPE (type), context_die);
	  gen_string_type_die (type, context_die);
	}
      else
	{
	  gen_array_type_die (type, context_die);
	}
      break;

    case ENUMERAL_TYPE:
    case RECORD_TYPE:
    case UNION_TYPE:
    case QUAL_UNION_TYPE:
      /* For a non-file-scope tagged type, we can always go ahead and output
         a Dwarf description of this type right now, even if the type in
         question is still incomplete, because if this local type *was* ever
         completed anywhere within its scope, that complete definition would
         already have been attached to this RECORD_TYPE, UNION_TYPE,
         QUAL_UNION_TYPE or ENUMERAL_TYPE node by the time we reach this
         point.  That's true because of the way the front-end does its
         processing of file-scope declarations (of functions and class types) 
         within which other types might be nested.  The C and C++ front-ends
         always gobble up such "local scope" things en-mass before they try
         to output *any* debugging information for any of the stuff contained 
         inside them and thus, we get the benefit here of what is (in effect) 
         a pre-resolution of forward references to tagged types in local
         scopes. Note however that for file-scope tagged types we cannot
         assume that such pre-resolution of forward references has taken
         place. A given file-scope tagged type may appear to be incomplete
         when we reach this point, but it may yet be given a full definition
         (at file-scope) later on during compilation.  In order to avoid
         generating a premature (and possibly incorrect) set of Dwarf DIEs
         for such (as yet incomplete) file-scope tagged types, we generate
         nothing at all for as-yet incomplete file-scope tagged types here
         unless we are making our special "finalization" pass for file-scope
         things at the very end of compilation.  At that time, we will
         certainly know as much about each file-scope tagged type as we are
         ever going to know, so at that point in time, we can safely generate 
         correct Dwarf descriptions for these file-scope tagged types.  */
      is_complete = TYPE_SIZE (type) != 0
	|| TYPE_CONTEXT (type) != NULL
	|| finalizing;
      if (TREE_CODE (type) == ENUMERAL_TYPE)
	{
	  gen_enumeration_type_die (type, is_complete, context_die);
	}
      else
	{
	  gen_struct_or_union_type_die (type, is_complete, context_die);
	}
      break;

    case VOID_TYPE:
    case INTEGER_TYPE:
    case REAL_TYPE:
    case COMPLEX_TYPE:
    case BOOLEAN_TYPE:
    case CHAR_TYPE:
      /* No DIEs needed for fundamental types.  */
      break;

    case LANG_TYPE:
      /* No Dwarf representation currently defined.  */
      break;

    default:
      abort ();
    }

  TREE_ASM_WRITTEN (type) = 1;
}

/* Generate a DIE for a tagged type instantiation.  */
static void
gen_tagged_type_instantiation_die (type, context_die)
     register tree type;
     register dw_die_ref context_die;
{
  if (type == 0 || type == error_mark_node)
    {
      return;
    }

  /* We are going to output a DIE to represent the unqualified version of of
     this type (i.e. without any const or volatile qualifiers) so make sure
     that we have the main variant (i.e. the unqualified version) of this
     type now.  */
  assert (type == type_main_variant (type));
  assert (TREE_ASM_WRITTEN (type));

  switch (TREE_CODE (type))
    {
    case ERROR_MARK:
      break;

    case ENUMERAL_TYPE:
      gen_inlined_enumeration_type_die (type, context_die);
      break;

    case RECORD_TYPE:
      gen_inlined_structure_type_die (type, context_die);
      break;

    case UNION_TYPE:
    case QUAL_UNION_TYPE:
      gen_inlined_union_type_die (type, context_die);
      break;

    default:
      abort ();			/* Should never happen.  */
    }
}

/* Generate a DW_TAG_lexical_block DIE followed by DIEs to represent all of the
   things which are local to the given block.  */
static void
gen_block_die (stmt, context_die)
     register tree stmt;
     register dw_die_ref context_die;
{
  register int must_output_die = 0;
  register tree origin;
  register tree decl;
  register enum tree_code origin_code;

  /* Ignore blocks never really used to make RTL.  */

  if (!stmt || !TREE_USED (stmt))
    {
      return;
    }

  /* Determine the "ultimate origin" of this block.  This block may be an
     inlined instance of an inlined instance of inline function, so we have
     to trace all of the way back through the origin chain to find out what
     sort of node actually served as the original seed for the creation of
     the current block.  */
  origin = block_ultimate_origin (stmt);
  origin_code = (origin != NULL) ? TREE_CODE (origin) : ERROR_MARK;

  /* Determine if we need to output any Dwarf DIEs at all to represent this
     block.  */
  if (origin_code == FUNCTION_DECL)
    {
      /* The outer scopes for inlinings *must* always be represented.  We
         generate DW_TAG_inlined_subroutine DIEs for them.  (See below.) */
      must_output_die = 1;
    }
  else
    {
      /* In the case where the current block represents an inlining of the
         "body block" of an inline function, we must *NOT* output any DIE for 
         this block because we have already output a DIE to represent the
         whole inlined function scope and the "body block" of any function
         doesn't really represent a different scope according to ANSI C
         rules.  So we check here to make sure that this block does not
         represent a "body block inlining" before trying to set the
         `must_output_die' flag.  */
      if (origin == NULL || !is_body_block (origin))
	{
	  /* Determine if this block directly contains any "significant"
	     local declarations which we will need to output DIEs for.  */
	  if (debug_info_level > DINFO_LEVEL_TERSE)
	    {
	      /* We are not in terse mode so *any* local declaration counts
	         as being a "significant" one.  */
	      must_output_die = (BLOCK_VARS (stmt) != NULL);
	    }
	  else
	    {
	      /* We are in terse mode, so only local (nested) function
	         definitions count as "significant" local declarations.  */
	      for (decl = BLOCK_VARS (stmt);
		   decl != NULL; decl = TREE_CHAIN (decl))
		{
		  if (TREE_CODE (decl) == FUNCTION_DECL
		      && DECL_INITIAL (decl))
		    {
		      must_output_die = 1;
		      break;
		    }
		}
	    }
	}
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
      if (origin_code == FUNCTION_DECL)
	{
	  gen_inlined_subroutine_die (stmt, context_die);
	}
      else
	{
	  gen_lexical_block_die (stmt, context_die);
	}
    }
  else
    decls_for_scope (stmt, context_die);
}

/* Generate all of the decls declared within a given scope and (recursively)
   all of it's sub-blocks.  */
static void
decls_for_scope (stmt, context_die)
     register tree stmt;
     register dw_die_ref context_die;
{
  register tree decl;
  register tree subblocks;
  /* Ignore blocks never really used to make RTL.  */
  if (!stmt || !TREE_USED (stmt))
    {
      return;
    }
  if (!BLOCK_ABSTRACT (stmt))
    {
      next_block_number++;
    }

  /* Output the DIEs to represent all of the data objects, functions,
     typedefs, and tagged types declared directly within this block but not
     within any nested sub-blocks.  */
  for (decl = BLOCK_VARS (stmt);
       decl != NULL; decl = TREE_CHAIN (decl))
    {
      gen_decl_die (decl, context_die);
    }

  /* Output the DIEs to represent all sub-blocks (and the items declared
     therein) of this block.  */
  for (subblocks = BLOCK_SUBBLOCKS (stmt);
       subblocks != NULL;
       subblocks = BLOCK_CHAIN (subblocks))
    {
      gen_block_die (subblocks, context_die);
    }
}

/* Generate Dwarf debug information for a decl described by DECL.  */
static void
gen_decl_die (decl, context_die)
     register tree decl;
     register dw_die_ref context_die;
{
  register tree origin;
  /* Make a note of the decl node we are going to be working on.  We may need 
     to give the user the source coordinates of where it appeared in case we
     notice (later on) that something about it looks screwy.  */
  dwarf_last_decl = decl;

  if (TREE_CODE (decl) == ERROR_MARK)
    {
      return;
    }

  /* If this ..._DECL node is marked to be ignored, then ignore it. But don't 
     ignore a function definition, since that would screw up our count of
     blocks, and that it turn will completely screw up the the labels we will 
     reference in subsequent DW_AT_low_pc and DW_AT_high_pc attributes (for
     subsequent blocks).  */
  if (DECL_IGNORED_P (decl) && TREE_CODE (decl) != FUNCTION_DECL)
    {
      return;
    }

  push_decl_scope (DECL_CONTEXT (decl));
  switch (TREE_CODE (decl))
    {
    case CONST_DECL:
      /* The individual enumerators of an enum type get output when we output 
         the Dwarf representation of the relevant enum type itself.  */
      break;

    case FUNCTION_DECL:
      /* If we are in terse mode, don't output any DIEs to represent mere
         function declarations.  */
      if (DECL_INITIAL (decl) == NULL_TREE)
	{
	  break;
	}
      /* Before we describe the FUNCTION_DECL itself, make sure that we have
         described its return type.  */
      gen_type_die (TREE_TYPE (TREE_TYPE (decl)), context_die);

      /* Now output a DIE to represent the function itself.  */
      gen_subprogram_die (decl, context_die);
      break;

    case TYPE_DECL:
      /* If we are in terse mode, don't generate any DIEs to represent any
         actual typedefs.  Note that even when we are in terse mode, we must
         still output DIEs to represent those tagged types which are used
         (directly or indirectly) in the specification of either a return
         type or a formal parameter type of some function.  */
      if (debug_info_level <= DINFO_LEVEL_TERSE)
	{
	  if (DECL_NAME (decl) != NULL
	      || !TYPE_USED_FOR_FUNCTION (TREE_TYPE (decl)))
	    {
	      break;
	    }
	}

      /* In the special case of a null-named TYPE_DECL node (representing the 
         declaration of some type tag), if the given TYPE_DECL is marked as
         having been instantiated from some other (original) TYPE_DECL node
         (e.g. one which was generated within the original definition of an
         inline function) we have to generate a special (abbreviated)
         DW_TAG_structure_type, DW_TAG_union_type, or DW_TAG_enumeration-type 
         DIE here.  */
      if (!DECL_NAME (decl) && DECL_ABSTRACT_ORIGIN (decl))
	{
	  gen_tagged_type_instantiation_die (TREE_TYPE (decl), context_die);
	  break;
	}
      gen_type_die (TREE_TYPE (decl), context_die);

      /* Note that unlike the gcc front end (which generates a NULL named
         TYPE_DECL node for each complete tagged type, each array type, and
         each function type node created) the g++ front end generates a
         _named_ TYPE_DECL node for each tagged type node created.
         Unfortunately, these g++ TYPE_DECL nodes cause us to output many
         superfluous and unnecessary DW_TAG_typedef DIEs here.  When g++ is
         fixed to stop generating these superfluous named TYPE_DECL nodes,
         the superfluous DW_TAG_typedef DIEs will likewise cease.  */
      if (DECL_NAME (decl))
	{
	  /* Output a DIE to represent the typedef itself.  */
	  gen_typedef_die (decl, context_die);
	}
      break;

    case LABEL_DECL:
      if (debug_info_level >= DINFO_LEVEL_NORMAL)
	{
	  gen_label_die (decl, context_die);
	}
      break;

    case VAR_DECL:
      /* If we are in terse mode, don't generate any DIEs to represent any
         variable declarations or definitions.  */
      if (debug_info_level <= DINFO_LEVEL_TERSE)
	{
	  break;
	}

      /* Output any DIEs that are needed to specify the type of this data
         object.  */
      gen_type_die (TREE_TYPE (decl), context_die);

      /* Now output the DIE to represent the data object itself.  This gets
         complicated because of the possibility that the VAR_DECL really
         represents an inlined instance of a formal parameter for an inline
         function.  */
      origin = decl_ultimate_origin (decl);
      if (origin != NULL && TREE_CODE (origin) == PARM_DECL)
	{
	  gen_formal_parameter_die (decl, context_die);
	}
      else
	{
	  gen_variable_die (decl, context_die);
	}
      break;

    case FIELD_DECL:
      /* Ignore the nameless fields that are used to skip bits.  */
      if (DECL_NAME (decl) != 0)
	{
	  gen_type_die (member_declared_type (decl), context_die);
	  gen_field_die (decl, context_die);
	}
      break;

    case PARM_DECL:
      gen_type_die (TREE_TYPE (decl), context_die);
      gen_formal_parameter_die (decl, context_die);
      break;

    default:
      abort ();
    }
  pop_decl_scope ();
}

/***************** Debug Information Generation Hooks ***********************/
void
dwarfout_file_scope_decl (decl, set_finalizing)
     register tree decl;
     register int set_finalizing;
{
  if (TREE_CODE (decl) == ERROR_MARK)
    {
      return;
    }

  /* If this ..._DECL node is marked to be ignored, then ignore it.  We gotta 
     hope that the node in question doesn't represent a function definition.
     If it does, then totally ignoring it is bound to screw up our count of
     blocks, and that it turn will completely screw up the the labels we will 
     reference in subsequent DW_AT_low_pc and DW_AT_high_pc attributes (for
     subsequent blocks).  (It's too bad that BLOCK nodes don't carry their
     own sequence numbers with them!) */
  if (DECL_IGNORED_P (decl))
    {
      if (TREE_CODE (decl) == FUNCTION_DECL
	  && DECL_INITIAL (decl) != NULL)
	{
	  abort ();
	}
      return;
    }

  switch (TREE_CODE (decl))
    {
    case FUNCTION_DECL:
      /* Ignore this FUNCTION_DECL if it refers to a builtin declaration of a 
         builtin function.  Explicit programmer-supplied declarations of
         these same functions should NOT be ignored however.  */
      if (DECL_EXTERNAL (decl) && DECL_FUNCTION_CODE (decl))
	{
	  return;
	}

      /* What we would really like to do here is to filter out all mere
         file-scope declarations of file-scope functions which are never
         referenced later within this translation unit (and keep all of ones
         that *are* referenced later on) but we aren't clarvoiant, so we have 
         no idea which functions will be referenced in the future (i.e. later 
         on within the current translation unit). So here we just ignore all
         file-scope function declarations which are not also definitions.  If 
         and when the debugger needs to know something about these funcstion, 
         it wil have to hunt around and find the DWARF information associated 
         with the definition of the function. Note that we can't just check
         `DECL_EXTERNAL' to find out which FUNCTION_DECL nodes represent
         definitions and which ones represent mere declarations.  We have to
         check `DECL_INITIAL' instead. That's because the C front-end
         supports some weird semantics for "extern inline" function
         definitions.  These can get inlined within the current translation
         unit (an thus, we need to generate DWARF info for their abstract
         instances so that the DWARF info for the concrete inlined instances
         can have something to refer to) but the compiler never generates any 
         out-of-lines instances of such things (despite the fact that they
         *are* definitions).  The important point is that the C front-end
         marks these "extern inline" functions as DECL_EXTERNAL, but we need
         to generate DWARf for them anyway. Note that the C++ front-end also
         plays some similar games for inline function definitions appearing
         within include files which also contain 
	 `#pragma interface' pragmas.  */
      if (DECL_INITIAL (decl) == NULL_TREE)
	{
	  return;
	}
      break;

    case VAR_DECL:
      /* Ignore this VAR_DECL if it refers to a file-scope extern data object 
         declaration and if the declaration was never even referenced from
         within this entire compilation unit.  We suppress these DIEs in
         order to save space in the .debug section (by eliminating entries
         which are probably useless).  Note that we must not suppress
         block-local extern declarations (whether used or not) because that
         would screw-up the debugger's name lookup mechanism and cause it to
         miss things which really ought to be in scope at a given point.  */
      if (DECL_EXTERNAL (decl) && !TREE_USED (decl))
	{
	  return;
	}

      /* If we are in terse mode, don't generate any DIEs to represent any
         variable declarations or definitions.  */
      if (debug_info_level <= DINFO_LEVEL_TERSE)
	{
	  return;
	}
      break;

    case TYPE_DECL:
      /* Don't bother trying to generate any DIEs to represent any of the
         normal built-in types for the language we are compiling, except in
         cases where the types in question are *not* DWARF fundamental types. 
         We make an exception in the case of non-fundamental types for the
         sake of objective C (and perhaps C++) because the GNU front-ends for 
         these languages may in fact create certain "built-in" types which
         are (for example) RECORD_TYPEs.  In such cases, we really need to
         output these (non-fundamental) types because other DIEs may contain
         references to them.  */
      if (DECL_SOURCE_LINE (decl) == 0
	  && is_base_type (TREE_TYPE (decl)))
	{
	  return;
	}

      /* If we are in terse mode, don't generate any DIEs to represent any
         actual typedefs.  Note that even when we are in terse mode, we must
         still output DIEs to represent those tagged types which are used
         (directly or indirectly) in the specification of either a return
         type or a formal parameter type of some function.  */
      if (debug_info_level <= DINFO_LEVEL_TERSE)
	{
	  if (DECL_NAME (decl) != NULL
	      || !TYPE_USED_FOR_FUNCTION (TREE_TYPE (decl)))
	    {
	      return;
	    }
	}
      break;

    default:
      return;
    }

  finalizing = set_finalizing;
  gen_decl_die (decl, comp_unit_die);

  if (TREE_CODE (decl) == FUNCTION_DECL
      && DECL_INITIAL (decl) != NULL)
    {
      current_funcdef_number++;
    }

}

/* Output a marker (i.e. a label) for the beginning of the generated code for
   a lexical block.  */
void
dwarfout_begin_block (blocknum)
     register unsigned blocknum;
{
  char label[MAX_ARTIFICIAL_LABEL_BYTES];

  function_section (current_function_decl);
  sprintf (label, BLOCK_BEGIN_LABEL_FMT, blocknum);
  ASM_OUTPUT_LABEL (asm_out_file, label);
}

/* Output a marker (i.e. a label) for the end of the generated code for a
   lexical block.  */
void
dwarfout_end_block (blocknum)
     register unsigned blocknum;
{
  char label[MAX_ARTIFICIAL_LABEL_BYTES];

  function_section (current_function_decl);
  sprintf (label, BLOCK_END_LABEL_FMT, blocknum);
  ASM_OUTPUT_LABEL (asm_out_file, label);
}

/* Output a marker (i.e. a label) at a point in the assembly code which
   corresponds to a given source level label.  */
void
dwarfout_label (insn)
     register rtx insn;
{
  char label[MAX_ARTIFICIAL_LABEL_BYTES];
  if (debug_info_level >= DINFO_LEVEL_NORMAL)
    {
      function_section (current_function_decl);
      sprintf (label, INSN_LABEL_FMT, current_funcdef_number,
	       (unsigned) INSN_UID (insn));
      ASM_OUTPUT_LABEL (asm_out_file, label);
    }
}

/* Output a marker (i.e. a label) for the point in the generated code where
   the real body of the function begins (after parameters have been moved to
   their home locations).  */
void
dwarfout_begin_function ()
{
  char label[MAX_ARTIFICIAL_LABEL_BYTES];
  register long int offset;
  register dw_fde_ref fde;
  register dw_cfi_ref cfi;

  function_section (current_function_decl);
  sprintf (label, BODY_BEGIN_LABEL_FMT, current_funcdef_number);
  ASM_OUTPUT_LABEL (asm_out_file, label);

  /* Expand the fde table if necessary.  */
  if (fde_table_in_use == fde_table_allocated)
    {
      fde_table_allocated += FDE_TABLE_INCREMENT;
      fde_table = (dw_fde_ref) xrealloc (fde_table,
	       fde_table_allocated * sizeof (dw_fde_node));
    }

  /* Record the FDE associated with this function.  */
  current_funcdef_fde = fde_table_in_use;

  /* Add the new FDE at the end of the fde_table.  */
  fde = &fde_table[fde_table_in_use++];
  fde->dw_fde_begin = xstrdup (function_start_label (current_function_decl));
  fde->dw_fde_end_prolog = xstrdup (label);
  fde->dw_fde_begin_epilogue = NULL;
  fde->dw_fde_end = NULL;
  fde->dw_fde_cfi = NULL;

#ifdef MIPS_DEBUGGING_INFO

  /* On entry, the Call Frame Address is in the stack pointer register.  */
  cfi = new_cfi ();
  cfi->dw_cfi_opc = DW_CFA_def_cfa;
  cfi->dw_cfi_oprnd1.dw_cfi_reg_num = STACK_POINTER_REGNUM;
  cfi->dw_cfi_oprnd2.dw_cfi_offset = 0;
  add_cfi (&fde->dw_fde_cfi, cfi);

  /* Set the location counter to the end of the function prolog.  */
  cfi = new_cfi ();
  cfi->dw_cfi_opc = DW_CFA_advance_loc4;
  cfi->dw_cfi_oprnd1.dw_cfi_addr = xstrdup (label);
  add_cfi (&fde->dw_fde_cfi, cfi);

  /* Define the CFA as either an explicit frame pointer register,
     or an offset from the stack pointer.  */
  cfi = new_cfi ();
  cfi->dw_cfi_opc = DW_CFA_def_cfa;
  cfi->dw_cfi_oprnd1.dw_cfi_reg_num = (frame_pointer_needed)
				       ? FRAME_POINTER_REGNUM
				       : STACK_POINTER_REGNUM;
  offset = current_frame_info.total_size;
  cfi->dw_cfi_oprnd2.dw_cfi_offset = offset;
  add_cfi (&fde->dw_fde_cfi, cfi);

  /* record the frame size for later definition of the DW_AT_frame_base
     attribute.  */
  current_funcdef_frame_size = offset;

  /* Define the rule for restoring the stack pointer.  */
  if (frame_pointer_needed)
    {
      /* Restore the stack register from the frame pointer.  */
      cfi = new_cfi ();
      cfi->dw_cfi_opc = DW_CFA_register;
      cfi->dw_cfi_oprnd1.dw_cfi_reg_num = STACK_POINTER_REGNUM;
      cfi->dw_cfi_oprnd2.dw_cfi_reg_num = FRAME_POINTER_REGNUM;
      add_cfi (&fde->dw_fde_cfi, cfi);
    }

  /* If RA is saved on the stack, define it here.  */
  if (regs_ever_live[31])
    {
      offset = current_frame_info.gp_save_offset / DWARF_CIE_DATA_ALIGNMENT;
      assert (offset >= 0);
      cfi = new_cfi ();
      cfi->dw_cfi_opc = DW_CFA_offset_extended;
      cfi->dw_cfi_oprnd1.dw_cfi_reg_num = DW_FRAME_RA_COL;
      cfi->dw_cfi_oprnd2.dw_cfi_offset = offset;
      add_cfi (&fde->dw_fde_cfi, cfi);
    }

  /* If FP is saved on the stack, define it here.  */
  if (current_frame_info.mask & (1 << 30))
    {
      offset = (current_frame_info.gp_save_offset
	         - (((current_frame_info.mask >> 31) & 1) * UNITS_PER_WORD))
               / DWARF_CIE_DATA_ALIGNMENT;
      assert (offset >= 0);
      cfi = new_cfi ();
      cfi->dw_cfi_opc = DW_CFA_offset;
      cfi->dw_cfi_oprnd1.dw_cfi_reg_num = FRAME_POINTER_REGNUM;
      cfi->dw_cfi_oprnd2.dw_cfi_offset = offset;
      add_cfi (&fde->dw_fde_cfi, cfi);
    }

#endif

}

/* Output a marker (i.e. a label) for the point in the generated code where
   the real body of the function ends (just before the epilogue code).  */
void
dwarfout_end_function ()
{
  dw_fde_ref fde;
  char label[MAX_ARTIFICIAL_LABEL_BYTES];
  function_section (current_function_decl);
  sprintf (label, BODY_END_LABEL_FMT, current_funcdef_number);
  ASM_OUTPUT_LABEL (asm_out_file, label);
  /* Record the ending code location in the FDE.  */
  fde = &fde_table[fde_table_in_use - 1];
  fde->dw_fde_begin_epilogue = xstrdup(label);
}

/* Output a marker (i.e. a label) for the absolute end of the generated code
   for a function definition.  This gets called *after* the epilogue code has
   been generated.  */
void
dwarfout_end_epilogue ()
{
  dw_fde_ref fde;
  char label[MAX_ARTIFICIAL_LABEL_BYTES];
  /* Output a label to mark the endpoint of the code generated for this
     function.        */
  sprintf (label, FUNC_END_LABEL_FMT, current_funcdef_number);
  ASM_OUTPUT_LABEL (asm_out_file, label);
  fde = &fde_table[fde_table_in_use - 1];
  fde->dw_fde_end = xstrdup (label);
}

/* Lookup a filename (in the list of filenames that we know about here in
   dwarfout.c) and return its "index".  The index of each (known) filename is
   just a unique number which is associated with only that one filename.
   We need such numbers for the sake of generating labels
   (in the .debug_sfnames section) and references to those
   files  numbers (in the .debug_srcinfo and.debug_macinfo sections).
   If the filename given as an argument is not found in our current list,
   add it to the list and assign it the next available unique index number.
   In order to speed up searches, we remember the index of the filename
   was looked up last.  This handles the majority of all searches.  */
static unsigned
lookup_filename (file_name)
     char *file_name;
{
  static unsigned last_file_lookup_index = 0;
  register char *fn;
  register unsigned i;

  /* Check to see if the file name that was searched on the previous call
     matches this file name. If so, return the index.  */
  if (last_file_lookup_index != 0)
    {
      fn = file_table[last_file_lookup_index];
      if (strcmp (file_name, fn) == 0)
	{
	  return last_file_lookup_index;
	}
    }

  /* Didn't match the previous lookup, search the table */
  for (i = 1; i < file_table_in_use; ++i)
    {
      fn = file_table[i];
      if (strcmp (file_name, fn) == 0)
	{
	  last_file_lookup_index = i;
	  return i;
	}
    }

  /* Prepare to add a new table entry by making sure there is enough space in 
     the table to do so.  If not, expand the current table.  */
  if (file_table_in_use == file_table_allocated)
    {
      file_table_allocated += FILE_TABLE_INCREMENT;
      file_table
	= (char **)
	xrealloc (file_table, file_table_allocated * sizeof (char *));
    }

  /* add the new entry to the end of the filename table.  */
  file_table[file_table_in_use] = xstrdup (file_name);
  last_file_lookup_index = file_table_in_use++;
  return last_file_lookup_index;
}

/* Output a label to mark the beginning of a source code line entry
   and record information relating to this source line, in
   'line_info_table' for later output of the .debug_line section.  */
void
dwarfout_line (filename, line)
     register char *filename;
     register unsigned line;
{
  char label[MAX_ARTIFICIAL_LABEL_BYTES];
  register unsigned this_file_entry_num = lookup_filename (filename);
  register dw_line_info_ref line_info;
  if (debug_info_level >= DINFO_LEVEL_NORMAL)
    {
      function_section (current_function_decl);
      sprintf (label, LINE_CODE_LABEL_FMT, line_info_table_in_use);
      ASM_OUTPUT_LABEL (asm_out_file, label);
      fputc ('\n', asm_out_file);

      /* expand the line info table if necessary */
      if (line_info_table_in_use == line_info_table_allocated)
	{
	  line_info_table_allocated += LINE_INFO_TABLE_INCREMENT;
	  line_info_table
	    = (dw_line_info_ref)
	    xrealloc (line_info_table,
		   line_info_table_allocated * sizeof (dw_line_info_entry));
	}
      /* add the new entry at the end of the line_info_table.  */
      line_info = &line_info_table[line_info_table_in_use++];
      line_info->dw_file_num = lookup_filename (filename);
      line_info->dw_line_num = line;
    }
}

/* Record the beginning of a new source file, for later output
   of the .debug_macinfo section.  At present, unimplemented.  */
void
dwarfout_start_new_source_file (filename)
     register char *filename;
{
}

/* Record the resumption of a source file, for later output
   of the .debug_macinfo section.  At present, unimplemented.  */
void
dwarfout_resume_previous_source_file (lineno)
     register unsigned lineno;
{
}

/* Called from check_newline in c-parse.y.  The `buffer' parameter contains
   the tail part of the directive line, i.e. the part which is past the
   initial whitespace, #, whitespace, directive-name, whitespace part.  */
void
dwarfout_define (lineno, buffer)
     register unsigned lineno;
     register char *buffer;
{
  static int initialized = 0;
  if (!initialized)
    {
      dwarfout_start_new_source_file (primary_filename);
      initialized = 1;
    }
}

/* Called from check_newline in c-parse.y.  The `buffer' parameter contains
   the tail part of the directive line, i.e. the part which is past the
   initial whitespace, #, whitespace, directive-name, whitespace part.  */
void
dwarfout_undef (lineno, buffer)
     register unsigned lineno;
     register char *buffer;
{
}

/* Set up for Dwarf output at the start of compilation.  */
void
dwarfout_init (asm_out_file, main_input_filename)
     register FILE *asm_out_file;
     register char *main_input_filename;
{

  /* Remember the name of the primary input file.  */
  primary_filename = main_input_filename;

  /* Allocate the initial hunk of the file_table.  */
  file_table = (char **) xmalloc (FILE_TABLE_INCREMENT * sizeof (char *));
  bzero (file_table, FILE_TABLE_INCREMENT * sizeof (char *));
  file_table_allocated = FILE_TABLE_INCREMENT;
  /* skip the first entry - file numbers begin at 1 */
  file_table_in_use = 1;

  /* Allocate the initial hunk of the type_die_table.  */
  type_die_table
    = (dw_die_ref *) xmalloc (TYPE_DIE_TABLE_INCREMENT * sizeof (dw_die_ref));
  bzero (type_die_table, TYPE_DIE_TABLE_INCREMENT * sizeof (dw_die_ref));
  type_die_table_allocated = TYPE_DIE_TABLE_INCREMENT;
  type_die_table_in_use = 0;

  /* Allocate the initial hunk of the decl_die_table.  */
  decl_die_table
    = (dw_die_ref *) xmalloc (DECL_DIE_TABLE_INCREMENT * sizeof (dw_die_ref));
  bzero (decl_die_table, DECL_DIE_TABLE_INCREMENT * sizeof (dw_die_ref));
  decl_die_table_allocated = DECL_DIE_TABLE_INCREMENT;
  decl_die_table_in_use = 0;

  /* Allocate the initial hunk of the decl_scope_table.  */
  decl_scope_table
    = (tree *) xmalloc (DECL_SCOPE_TABLE_INCREMENT * sizeof (tree));
  bzero (decl_scope_table, DECL_SCOPE_TABLE_INCREMENT * sizeof (tree));
  decl_scope_table_allocated = DECL_SCOPE_TABLE_INCREMENT;
  decl_scope_depth = 0;

  /* Allocate the initial hunk of the abbrev_die_table.  */
  abbrev_die_table
    = (dw_die_ref *) xmalloc (ABBREV_DIE_TABLE_INCREMENT
			      * sizeof (dw_die_ref));
  bzero (abbrev_die_table, ABBREV_DIE_TABLE_INCREMENT * sizeof (dw_die_ref));
  abbrev_die_table_allocated = ABBREV_DIE_TABLE_INCREMENT;
  /* zero-th entry is allocated, but unused */
  abbrev_die_table_in_use = 1;

  /* Allocate the initial hunk of the line_info_table.  */
  line_info_table
    = (dw_line_info_ref) xmalloc (LINE_INFO_TABLE_INCREMENT
				  * sizeof (dw_line_info_entry));
  bzero (line_info_table, LINE_INFO_TABLE_INCREMENT
	 * sizeof (dw_line_info_entry));
  line_info_table_allocated = LINE_INFO_TABLE_INCREMENT;
  /* zero-th entry is allocated, but unused */
  line_info_table_in_use = 1;

  /* Allocate the initial hunk of the fde_table.  */
  fde_table = (dw_fde_ref) xmalloc (FDE_TABLE_INCREMENT * sizeof (dw_fde_node));
  bzero (fde_table, FDE_TABLE_INCREMENT * sizeof (dw_fde_node));
  fde_table_allocated = FDE_TABLE_INCREMENT;
  fde_table_in_use = 0;

  /* Output a starting label for the .text section.  */
  fputc ('\n', asm_out_file);
  ASM_OUTPUT_SECTION (asm_out_file, TEXT_SECTION);
  ASM_OUTPUT_LABEL (asm_out_file, TEXT_BEGIN_LABEL);

  /* Output a starting label for the .data section.  */
  fputc ('\n', asm_out_file);
  ASM_OUTPUT_SECTION (asm_out_file, DATA_SECTION);
  ASM_OUTPUT_LABEL (asm_out_file, DATA_BEGIN_LABEL);

  /* Output a starting label for the .rodata section.  */
  fputc ('\n', asm_out_file);
  ASM_OUTPUT_SECTION (asm_out_file, RODATA_SECTION);
  ASM_OUTPUT_LABEL (asm_out_file, RODATA_BEGIN_LABEL);

  /* Output a starting label for the .bss section.  */
  fputc ('\n', asm_out_file);
  ASM_OUTPUT_SECTION (asm_out_file, BSS_SECTION);
  ASM_OUTPUT_LABEL (asm_out_file, BSS_BEGIN_LABEL);

  /* Generate the initial DIE for the .debug section.  Note that the (string) 
     value given in the DW_AT_name attribute of the DW_TAG_compile_unit DIE
     will (typically) be a relative pathname and that this pathname should be 
     taken as being relative to the directory from which the compiler was
     invoked when the given (base) source file was compiled.  */
  gen_compile_unit_die (main_input_filename);

  /* clear the association between base types and their DIE's */
  init_base_type_table ();

  /* clear the backchain list.  */
  backchain = NULL;
}

/* Output stuff that dwarf requires at the end of every file,
   and generate the DWARF-2 debugging info.  */
void
dwarfout_finish ()
{

  resolve_backchains ();

  /* Traverse the DIE tree and add sibling attributes to those DIE's
     that have children.  */
  add_sibling_attributes (comp_unit_die);

  /* Output a terminator label for the .text section.  */
  fputc ('\n', asm_out_file);
  ASM_OUTPUT_SECTION (asm_out_file, TEXT_SECTION);
  ASM_OUTPUT_LABEL (asm_out_file, TEXT_END_LABEL);

  /* Output a terminator label for the .data section.  */
  fputc ('\n', asm_out_file);
  ASM_OUTPUT_SECTION (asm_out_file, DATA_SECTION);
  ASM_OUTPUT_LABEL (asm_out_file, DATA_END_LABEL);

  /* Output a terminator label for the .rodata section.  */
  fputc ('\n', asm_out_file);
  ASM_OUTPUT_SECTION (asm_out_file, RODATA_SECTION);
  ASM_OUTPUT_LABEL (asm_out_file, RODATA_END_LABEL);

  /* Output a terminator label for the .bss section.  */
  fputc ('\n', asm_out_file);
  ASM_OUTPUT_SECTION (asm_out_file, BSS_SECTION);
  ASM_OUTPUT_LABEL (asm_out_file, BSS_END_LABEL);

  /* Output the abbreviation table.  */
  fputc ('\n', asm_out_file);
  ASM_OUTPUT_SECTION (asm_out_file, ABBREV_SECTION);
  build_abbrev_table (comp_unit_die);
  output_abbrev_section ();

  /* Output the source line correspondence table.  */
  fputc ('\n', asm_out_file);
  ASM_OUTPUT_SECTION (asm_out_file, LINE_SECTION);
  output_line_info ();

  /* Initialize the beginning DIE offset - and calculate sizes/offsets.   */
  next_die_offset = DWARF_COMPILE_UNIT_HEADER_SIZE;
  calc_die_sizes (comp_unit_die);

  /* Initialize the beginning FDE offset - and calculate sizes/offsets.  */
  next_fde_offset = DWARF_CIE_SIZE;
  calc_fde_sizes ();

  /* Output debugging information.  */
  fputc ('\n', asm_out_file);
  ASM_OUTPUT_SECTION (asm_out_file, DEBUG_SECTION);
  output_compilation_unit_header ();
  output_die (comp_unit_die);

  if (fde_table_in_use)
    {
      /* Output call frame information.  */
      fputc ('\n', asm_out_file);
      ASM_OUTPUT_SECTION (asm_out_file, FRAME_SECTION);
      output_call_frame_info ();

      /* Output public names table.  */
      fputc ('\n', asm_out_file);
      ASM_OUTPUT_SECTION (asm_out_file, PUBNAMES_SECTION);
      output_pubnames ();

      /* Output the address range information.  */
      fputc ('\n', asm_out_file);
      ASM_OUTPUT_SECTION (asm_out_file, ARANGES_SECTION);
      output_aranges ();
    }
}
#endif /* DWARF_DEBUGGING_INFO  && DWARF_VERSION == 2 */
