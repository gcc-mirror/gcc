/* Output Dwarf format symbol table information from the GNU C compiler.
   Copyright (C) 1992, 1993 Free Software Foundation, Inc.

   Written by Ron Guilmette (rfg@netcom.com) for
   Network Computing Devices, August, September, October, November 1990.
   Generously contributed by NCD to the Free Software Foundation.

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

#ifdef DWARF_DEBUGGING_INFO
#include <stdio.h>
#include "dwarf.h"
#include "tree.h"
#include "flags.h"
#include "rtl.h"
#include "hard-reg-set.h"
#include "insn-config.h"
#include "reload.h"
#include "output.h"
#include "defaults.h"

#ifndef DWARF_VERSION
#define DWARF_VERSION 1
#endif

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
   regarding the GNU implementation of Dwarf.  */

/* NOTE: In the comments in this file, many references are made to
   so called "Debugging Information Entries".  For the sake of brevity,
   this term is abbreviated to `DIE' throughout the remainder of this
   file.  */

/* Note that the implementation of C++ support herein is (as yet) unfinished.
   If you want to try to complete it, more power to you.  */

#if defined(__GNUC__) && (NDEBUG == 1)
#define inline static inline
#else
#define inline static
#endif

/* How to start an assembler comment.  */
#ifndef ASM_COMMENT_START
#define ASM_COMMENT_START ";#"
#endif

/* How to print out a register name.  */
#ifndef PRINT_REG
#define PRINT_REG(RTX, CODE, FILE) \
  fprintf ((FILE), "%s", reg_names[REGNO (RTX)])
#endif

/* Define a macro which returns non-zero for any tagged type which is
   used (directly or indirectly) in the specification of either some
   function's return type or some formal parameter of some function.
   We use this macro when we are operating in "terse" mode to help us
   know what tagged types have to be represented in Dwarf (even in
   terse mode) and which ones don't.

   A flag bit with this meaning really should be a part of the normal
   GCC ..._TYPE nodes, but at the moment, there is no such bit defined
   for these nodes.  For now, we have to just fake it.  It it safe for
   us to simply return zero for all complete tagged types (which will
   get forced out anyway if they were used in the specification of some
   formal or return type) and non-zero for all incomplete tagged types.
*/

#define TYPE_USED_FOR_FUNCTION(tagged_type) (TYPE_SIZE (tagged_type) == 0)

extern int flag_traditional;
extern char *version_string;
extern char *language_string;

/* Maximum size (in bytes) of an artificially generated label.	*/

#define MAX_ARTIFICIAL_LABEL_BYTES	30

/* Make sure we know the sizes of the various types dwarf can describe.
   These are only defaults.  If the sizes are different for your target,
   you should override these values by defining the appropriate symbols
   in your tm.h file.  */

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

/* Structure to keep track of source filenames.  */

struct filename_entry {
  unsigned	number;
  char *	name;
};

typedef struct filename_entry filename_entry;

/* Pointer to an array of elements, each one having the structure above. */

static filename_entry *filename_table;

/* Total number of entries in the table (i.e. array) pointed to by
   `filename_table'.  This is the *total* and includes both used and
   unused slots.  */

static unsigned ft_entries_allocated;

/* Number of entries in the filename_table which are actually in use.  */

static unsigned ft_entries;

/* Size (in elements) of increments by which we may expand the filename
   table.  Actually, a single hunk of space of this size should be enough
   for most typical programs.	 */

#define FT_ENTRIES_INCREMENT 64

/* Local pointer to the name of the main input file.  Initialized in
   dwarfout_init.  */

static char *primary_filename;

/* Pointer to the most recent filename for which we produced some line info.  */

static char *last_filename;

/* For Dwarf output, we must assign lexical-blocks id numbers
   in the order in which their beginnings are encountered.
   We output Dwarf debugging info that refers to the beginnings
   and ends of the ranges of code for each lexical block with
   assembler labels ..Bn and ..Bn.e, where n is the block number.
   The labels themselves are generated in final.c, which assigns
   numbers to the blocks in the same way.  */

static unsigned next_block_number = 2;

/* Counter to generate unique names for DIEs. */

static unsigned next_unused_dienum = 1;

/* Number of the DIE which is currently being generated.  */

static unsigned current_dienum;

/* Number to use for the special "pubname" label on the next DIE which
   represents a function or data object defined in this compilation
   unit which has "extern" linkage.  */

static next_pubname_number = 0;

#define NEXT_DIE_NUM pending_sibling_stack[pending_siblings-1]

/* Pointer to a dynamically allocated list of pre-reserved and still
   pending sibling DIE numbers.	 Note that this list will grow as needed.  */

static unsigned *pending_sibling_stack;

/* Counter to keep track of the number of pre-reserved and still pending
   sibling DIE numbers.	 */

static unsigned pending_siblings;

/* The currently allocated size of the above list (expressed in number of
   list elements).  */

static unsigned pending_siblings_allocated;

/* Size (in elements) of increments by which we may expand the pending
   sibling stack.  Actually, a single hunk of space of this size should
   be enough for most typical programs.	 */

#define PENDING_SIBLINGS_INCREMENT 64

/* Non-zero if we are performing our file-scope finalization pass and if
   we should force out Dwarf descriptions of any and all file-scope
   tagged types which are still incomplete types.  */

static int finalizing = 0;

/* A pointer to the base of a list of pending types which we haven't
   generated DIEs for yet, but which we will have to come back to
   later on.  */

static tree *pending_types_list;

/* Number of elements currently allocated for the pending_types_list.  */

static unsigned pending_types_allocated;

/* Number of elements of pending_types_list currently in use.  */

static unsigned pending_types;

/* Size (in elements) of increments by which we may expand the pending
   types list.  Actually, a single hunk of space of this size should
   be enough for most typical programs.	 */

#define PENDING_TYPES_INCREMENT 64

/* Pointer to an artificial RECORD_TYPE which we create in dwarfout_init.
   This is used in a hack to help us get the DIEs describing types of
   formal parameters to come *after* all of the DIEs describing the formal
   parameters themselves.  That's necessary in order to be compatible
   with what the brain-damaged svr4 SDB debugger requires.  */

static tree fake_containing_scope;

/* The number of the current function definition that we are generating
   debugging information for.  These numbers range from 1 up to the maximum
   number of function definitions contained within the current compilation
   unit.  These numbers are used to create unique labels for various things
   contained within various function definitions.  */

static unsigned current_funcdef_number = 1;

/* A pointer to the ..._DECL node which we have most recently been working
   on.  We keep this around just in case something about it looks screwy
   and we want to tell the user what the source coordinates for the actual
   declaration are.  */

static tree dwarf_last_decl;

/* Forward declarations for functions defined in this file.  */

static void output_type ();
static void type_attribute ();
static void output_decls_for_scope ();
static void output_decl ();
static unsigned lookup_filename ();

/* Definitions of defaults for assembler-dependent names of various
   pseudo-ops and section names.

   Theses may be overridden in your tm.h file (if necessary) for your
   particular assembler.  The default values provided here correspond to
   what is expected by "standard" AT&T System V.4 assemblers.  */

#ifndef FILE_ASM_OP
#define FILE_ASM_OP		".file"
#endif
#ifndef VERSION_ASM_OP
#define VERSION_ASM_OP		".version"
#endif
#ifndef UNALIGNED_SHORT_ASM_OP
#define UNALIGNED_SHORT_ASM_OP	".2byte"
#endif
#ifndef UNALIGNED_INT_ASM_OP
#define UNALIGNED_INT_ASM_OP	".4byte"
#endif
#ifndef ASM_BYTE_OP
#define ASM_BYTE_OP		".byte"
#endif
#ifndef SET_ASM_OP
#define SET_ASM_OP		".set"
#endif

/* Pseudo-ops for pushing the current section onto the section stack (and
   simultaneously changing to a new section) and for poping back to the
   section we were in immediately before this one.  Note that most svr4
   assemblers only maintain a one level stack... you can push all the
   sections you want, but you can only pop out one level.  (The sparc
   svr4 assembler is an exception to this general rule.)  That's
   OK because we only use at most one level of the section stack herein.  */

#ifndef PUSHSECTION_ASM_OP
#define PUSHSECTION_ASM_OP	".section"
#endif
#ifndef POPSECTION_ASM_OP
#define POPSECTION_ASM_OP	".previous"
#endif

/* The default format used by the ASM_OUTPUT_PUSH_SECTION macro (see below)
   to print the PUSHSECTION_ASM_OP and the section name.  The default here
   works for almost all svr4 assemblers, except for the sparc, where the
   section name must be enclosed in double quotes.  (See sparcv4.h.)  */

#ifndef PUSHSECTION_FORMAT
#define PUSHSECTION_FORMAT	"%s\t%s\n"
#endif

#ifndef DEBUG_SECTION
#define DEBUG_SECTION		".debug"
#endif
#ifndef LINE_SECTION
#define LINE_SECTION		".line"
#endif
#ifndef SFNAMES_SECTION
#define SFNAMES_SECTION		".debug_sfnames"
#endif
#ifndef SRCINFO_SECTION
#define SRCINFO_SECTION		".debug_srcinfo"
#endif
#ifndef MACINFO_SECTION
#define MACINFO_SECTION		".debug_macinfo"
#endif
#ifndef PUBNAMES_SECTION
#define PUBNAMES_SECTION	".debug_pubnames"
#endif
#ifndef ARANGES_SECTION
#define ARANGES_SECTION		".debug_aranges"
#endif
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
   (artificial) labels which may be generated within this file (when
   the -g options is used and DWARF_DEBUGGING_INFO is in effect.

   If necessary, these may be overridden from within your tm.h file,
   but typically, you should never need to override these.

   These labels have been hacked (temporarily) so that they all begin with
   a `.L' sequence so as to appease the stock sparc/svr4 assembler and the
   stock m88k/svr4 assembler, both of which need to see .L at the start of
   a label in order to prevent that label from going into the linker symbol
   table).  When I get time, I'll have to fix this the right way so that we
   will use ASM_GENERATE_INTERNAL_LABEL and ASM_OUTPUT_INTERNAL_LABEL herein,
   but that will require a rather massive set of changes.  For the moment,
   the following definitions out to produce the right results for all svr4
   and svr3 assemblers. -- rfg
*/

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

#ifndef DATA1_BEGIN_LABEL
#define DATA1_BEGIN_LABEL	".L_data1_b"
#endif
#ifndef DATA1_END_LABEL
#define DATA1_END_LABEL		".L_data1_e"
#endif

#ifndef RODATA_BEGIN_LABEL
#define RODATA_BEGIN_LABEL	".L_rodata_b"
#endif
#ifndef RODATA_END_LABEL
#define RODATA_END_LABEL	".L_rodata_e"
#endif

#ifndef RODATA1_BEGIN_LABEL
#define RODATA1_BEGIN_LABEL	".L_rodata1_b"
#endif
#ifndef RODATA1_END_LABEL
#define RODATA1_END_LABEL	".L_rodata1_e"
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
#ifndef LINE_LAST_ENTRY_LABEL
#define LINE_LAST_ENTRY_LABEL	".L_line_last"
#endif
#ifndef LINE_END_LABEL
#define LINE_END_LABEL		".L_line_e"
#endif

#ifndef DEBUG_BEGIN_LABEL
#define DEBUG_BEGIN_LABEL	".L_debug_b"
#endif
#ifndef SFNAMES_BEGIN_LABEL
#define SFNAMES_BEGIN_LABEL	".L_sfnames_b"
#endif
#ifndef SRCINFO_BEGIN_LABEL
#define SRCINFO_BEGIN_LABEL	".L_srcinfo_b"
#endif
#ifndef MACINFO_BEGIN_LABEL
#define MACINFO_BEGIN_LABEL	".L_macinfo_b"
#endif

#ifndef DIE_BEGIN_LABEL_FMT
#define DIE_BEGIN_LABEL_FMT	".L_D%u"
#endif
#ifndef DIE_END_LABEL_FMT
#define DIE_END_LABEL_FMT	".L_D%u_e"
#endif
#ifndef PUB_DIE_LABEL_FMT
#define PUB_DIE_LABEL_FMT	".L_P%u"
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
#ifndef SS_BEGIN_LABEL_FMT
#define SS_BEGIN_LABEL_FMT	".L_s%u"
#endif
#ifndef SS_END_LABEL_FMT
#define SS_END_LABEL_FMT	".L_s%u_e"
#endif
#ifndef EE_BEGIN_LABEL_FMT
#define EE_BEGIN_LABEL_FMT	".L_e%u"
#endif
#ifndef EE_END_LABEL_FMT
#define EE_END_LABEL_FMT	".L_e%u_e"
#endif
#ifndef MT_BEGIN_LABEL_FMT
#define MT_BEGIN_LABEL_FMT	".L_t%u"
#endif
#ifndef MT_END_LABEL_FMT
#define MT_END_LABEL_FMT	".L_t%u_e"
#endif
#ifndef LOC_BEGIN_LABEL_FMT
#define LOC_BEGIN_LABEL_FMT	".L_l%u"
#endif
#ifndef LOC_END_LABEL_FMT
#define LOC_END_LABEL_FMT	".L_l%u_e"
#endif
#ifndef BOUND_BEGIN_LABEL_FMT
#define BOUND_BEGIN_LABEL_FMT	".L_b%u_%u_%c"
#endif
#ifndef BOUND_END_LABEL_FMT
#define BOUND_END_LABEL_FMT	".L_b%u_%u_%c_e"
#endif
#ifndef DERIV_BEGIN_LABEL_FMT
#define DERIV_BEGIN_LABEL_FMT	".L_d%u"
#endif
#ifndef DERIV_END_LABEL_FMT
#define DERIV_END_LABEL_FMT	".L_d%u_e"
#endif
#ifndef SL_BEGIN_LABEL_FMT
#define SL_BEGIN_LABEL_FMT	".L_sl%u"
#endif
#ifndef SL_END_LABEL_FMT
#define SL_END_LABEL_FMT	".L_sl%u_e"
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
#ifndef TYPE_NAME_FMT
#define TYPE_NAME_FMT		".L_T%u"
#endif
#ifndef DECL_NAME_FMT
#define DECL_NAME_FMT		".L_E%u"
#endif
#ifndef LINE_CODE_LABEL_FMT
#define LINE_CODE_LABEL_FMT	".L_LC%u"
#endif
#ifndef SFNAMES_ENTRY_LABEL_FMT
#define SFNAMES_ENTRY_LABEL_FMT	".L_F%u"
#endif
#ifndef LINE_ENTRY_LABEL_FMT
#define LINE_ENTRY_LABEL_FMT	".L_LE%u"
#endif

/* Definitions of defaults for various types of primitive assembly language
   output operations.

   If necessary, these may be overridden from within your tm.h file,
   but typically, you shouldn't need to override these.  */

#ifndef ASM_OUTPUT_PUSH_SECTION
#define ASM_OUTPUT_PUSH_SECTION(FILE, SECTION) \
  fprintf ((FILE), PUSHSECTION_FORMAT, PUSHSECTION_ASM_OP, SECTION)
#endif

#ifndef ASM_OUTPUT_POP_SECTION
#define ASM_OUTPUT_POP_SECTION(FILE) \
  fprintf ((FILE), "\t%s\n", POPSECTION_ASM_OP)
#endif

#ifndef ASM_OUTPUT_SOURCE_FILENAME
#define ASM_OUTPUT_SOURCE_FILENAME(FILE,NAME) \
  do {	fprintf (FILE, "\t%s\t", FILE_ASM_OP);				\
	output_quoted_string (FILE, NAME);				\
	fputc ('\n', FILE);						\
  } while (0)
#endif

#ifndef ASM_OUTPUT_DWARF_DELTA2
#define ASM_OUTPUT_DWARF_DELTA2(FILE,LABEL1,LABEL2)			\
 do {	fprintf ((FILE), "\t%s\t", UNALIGNED_SHORT_ASM_OP);		\
	assemble_name (FILE, LABEL1);					\
	fprintf (FILE, "-");						\
	assemble_name (FILE, LABEL2);					\
	fprintf (FILE, "\n");						\
  } while (0)
#endif

#ifndef ASM_OUTPUT_DWARF_DELTA4
#define ASM_OUTPUT_DWARF_DELTA4(FILE,LABEL1,LABEL2)			\
 do {	fprintf ((FILE), "\t%s\t", UNALIGNED_INT_ASM_OP);		\
	assemble_name (FILE, LABEL1);					\
	fprintf (FILE, "-");						\
	assemble_name (FILE, LABEL2);					\
	fprintf (FILE, "\n");						\
  } while (0)
#endif

#ifndef ASM_OUTPUT_DWARF_TAG
#define ASM_OUTPUT_DWARF_TAG(FILE,TAG)					\
  do {									\
    fprintf ((FILE), "\t%s\t0x%x",					\
		     UNALIGNED_SHORT_ASM_OP, (unsigned) TAG);		\
    if (flag_verbose_asm)						\
      fprintf ((FILE), "\t%s %s",					\
		       ASM_COMMENT_START, dwarf_tag_name (TAG));	\
    fputc ('\n', (FILE));						\
  } while (0)
#endif

#ifndef ASM_OUTPUT_DWARF_ATTRIBUTE
#define ASM_OUTPUT_DWARF_ATTRIBUTE(FILE,ATTR)				\
  do {									\
    fprintf ((FILE), "\t%s\t0x%x",					\
		     UNALIGNED_SHORT_ASM_OP, (unsigned) ATTR);		\
    if (flag_verbose_asm)						\
      fprintf ((FILE), "\t%s %s",					\
		       ASM_COMMENT_START, dwarf_attr_name (ATTR));	\
    fputc ('\n', (FILE));						\
  } while (0)
#endif

#ifndef ASM_OUTPUT_DWARF_STACK_OP
#define ASM_OUTPUT_DWARF_STACK_OP(FILE,OP)				\
  do {									\
    fprintf ((FILE), "\t%s\t0x%x", ASM_BYTE_OP, (unsigned) OP);		\
    if (flag_verbose_asm)						\
      fprintf ((FILE), "\t%s %s",					\
		       ASM_COMMENT_START, dwarf_stack_op_name (OP));	\
    fputc ('\n', (FILE));						\
  } while (0)
#endif

#ifndef ASM_OUTPUT_DWARF_FUND_TYPE
#define ASM_OUTPUT_DWARF_FUND_TYPE(FILE,FT)				\
  do {									\
    fprintf ((FILE), "\t%s\t0x%x",					\
		     UNALIGNED_SHORT_ASM_OP, (unsigned) FT);		\
    if (flag_verbose_asm)						\
      fprintf ((FILE), "\t%s %s",					\
		       ASM_COMMENT_START, dwarf_fund_type_name (FT));	\
    fputc ('\n', (FILE));						\
  } while (0)
#endif

#ifndef ASM_OUTPUT_DWARF_FMT_BYTE
#define ASM_OUTPUT_DWARF_FMT_BYTE(FILE,FMT)				\
  do {									\
    fprintf ((FILE), "\t%s\t0x%x", ASM_BYTE_OP, (unsigned) FMT);	\
    if (flag_verbose_asm)						\
      fprintf ((FILE), "\t%s %s",					\
		       ASM_COMMENT_START, dwarf_fmt_byte_name (FMT));	\
    fputc ('\n', (FILE));						\
  } while (0)
#endif

#ifndef ASM_OUTPUT_DWARF_TYPE_MODIFIER
#define ASM_OUTPUT_DWARF_TYPE_MODIFIER(FILE,MOD)			\
  do {									\
    fprintf ((FILE), "\t%s\t0x%x", ASM_BYTE_OP, (unsigned) MOD);	\
    if (flag_verbose_asm)						\
      fprintf ((FILE), "\t%s %s",					\
		       ASM_COMMENT_START, dwarf_typemod_name (MOD));	\
    fputc ('\n', (FILE));						\
  } while (0)
#endif

#ifndef ASM_OUTPUT_DWARF_ADDR
#define ASM_OUTPUT_DWARF_ADDR(FILE,LABEL)				\
 do {	fprintf ((FILE), "\t%s\t", UNALIGNED_INT_ASM_OP);		\
	assemble_name (FILE, LABEL);					\
	fprintf (FILE, "\n");						\
  } while (0)
#endif

#ifndef ASM_OUTPUT_DWARF_ADDR_CONST
#define ASM_OUTPUT_DWARF_ADDR_CONST(FILE,RTX)				\
  do {									\
    fprintf ((FILE), "\t%s\t", UNALIGNED_INT_ASM_OP);			\
    output_addr_const ((FILE), (RTX));					\
    fputc ('\n', (FILE));						\
  } while (0)
#endif

#ifndef ASM_OUTPUT_DWARF_REF
#define ASM_OUTPUT_DWARF_REF(FILE,LABEL)				\
 do {	fprintf ((FILE), "\t%s\t", UNALIGNED_INT_ASM_OP);		\
	assemble_name (FILE, LABEL);					\
	fprintf (FILE, "\n");						\
  } while (0)
#endif

#ifndef ASM_OUTPUT_DWARF_DATA1
#define ASM_OUTPUT_DWARF_DATA1(FILE,VALUE) \
  fprintf ((FILE), "\t%s\t0x%x\n", ASM_BYTE_OP, VALUE)
#endif

#ifndef ASM_OUTPUT_DWARF_DATA2
#define ASM_OUTPUT_DWARF_DATA2(FILE,VALUE) \
  fprintf ((FILE), "\t%s\t0x%x\n", UNALIGNED_SHORT_ASM_OP, (unsigned) VALUE)
#endif

#ifndef ASM_OUTPUT_DWARF_DATA4
#define ASM_OUTPUT_DWARF_DATA4(FILE,VALUE) \
  fprintf ((FILE), "\t%s\t0x%x\n", UNALIGNED_INT_ASM_OP, (unsigned) VALUE)
#endif

#ifndef ASM_OUTPUT_DWARF_DATA8
#define ASM_OUTPUT_DWARF_DATA8(FILE,HIGH_VALUE,LOW_VALUE)		\
  do {									\
    if (WORDS_BIG_ENDIAN)						\
      {									\
	fprintf ((FILE), "\t%s\t0x%x\n", UNALIGNED_INT_ASM_OP, HIGH_VALUE); \
	fprintf ((FILE), "\t%s\t0x%x\n", UNALIGNED_INT_ASM_OP, LOW_VALUE);\
      }									\
    else								\
      {									\
	fprintf ((FILE), "\t%s\t0x%x\n", UNALIGNED_INT_ASM_OP, LOW_VALUE);\
	fprintf ((FILE), "\t%s\t0x%x\n", UNALIGNED_INT_ASM_OP, HIGH_VALUE); \
      }									\
  } while (0)
#endif

#ifndef ASM_OUTPUT_DWARF_STRING
#define ASM_OUTPUT_DWARF_STRING(FILE,P) \
  ASM_OUTPUT_ASCII ((FILE), P, strlen (P)+1)
#endif

/************************ general utility functions **************************/

inline char *
xstrdup (s)
     register char *s;
{
  register char *p = (char *) xmalloc (strlen (s) + 1);

  strcpy (p, s);
  return p;
}

inline int
is_pseudo_reg (rtl)
     register rtx rtl;
{
  return (((GET_CODE (rtl) == REG) && (REGNO (rtl) >= FIRST_PSEUDO_REGISTER))
          || ((GET_CODE (rtl) == SUBREG)
	      && (REGNO (XEXP (rtl, 0)) >= FIRST_PSEUDO_REGISTER)));
}

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

static char *
dwarf_tag_name (tag)
     register unsigned tag;
{
  switch (tag)
    {
    case TAG_padding:			return "TAG_padding";
    case TAG_array_type:		return "TAG_array_type";
    case TAG_class_type:		return "TAG_class_type";
    case TAG_entry_point:		return "TAG_entry_point";
    case TAG_enumeration_type:		return "TAG_enumeration_type";
    case TAG_formal_parameter:		return "TAG_formal_parameter";
    case TAG_global_subroutine:		return "TAG_global_subroutine";
    case TAG_global_variable:		return "TAG_global_variable";
    case TAG_label:			return "TAG_label";
    case TAG_lexical_block:		return "TAG_lexical_block";
    case TAG_local_variable:		return "TAG_local_variable";
    case TAG_member:			return "TAG_member";
    case TAG_pointer_type:		return "TAG_pointer_type";
    case TAG_reference_type:		return "TAG_reference_type";
    case TAG_compile_unit:		return "TAG_compile_unit";
    case TAG_string_type:		return "TAG_string_type";
    case TAG_structure_type:		return "TAG_structure_type";
    case TAG_subroutine:		return "TAG_subroutine";
    case TAG_subroutine_type:		return "TAG_subroutine_type";
    case TAG_typedef:			return "TAG_typedef";
    case TAG_union_type:		return "TAG_union_type";
    case TAG_unspecified_parameters:	return "TAG_unspecified_parameters";
    case TAG_variant:			return "TAG_variant";
    case TAG_common_block:		return "TAG_common_block";
    case TAG_common_inclusion:		return "TAG_common_inclusion";
    case TAG_inheritance:		return "TAG_inheritance";
    case TAG_inlined_subroutine:	return "TAG_inlined_subroutine";
    case TAG_module:			return "TAG_module";
    case TAG_ptr_to_member_type:	return "TAG_ptr_to_member_type";
    case TAG_set_type:			return "TAG_set_type";
    case TAG_subrange_type:		return "TAG_subrange_type";
    case TAG_with_stmt:			return "TAG_with_stmt";

    /* GNU extensions.  */

    case TAG_format_label:		return "TAG_format_label";
    case TAG_namelist:			return "TAG_namelist";
    case TAG_function_template:		return "TAG_function_template";
    case TAG_class_template:		return "TAG_class_template";

    default:				return "TAG_<unknown>";
    }
}

static char *
dwarf_attr_name (attr)
     register unsigned attr;
{
  switch (attr)
    {
    case AT_sibling:			return "AT_sibling";
    case AT_location:			return "AT_location";
    case AT_name:			return "AT_name";
    case AT_fund_type:			return "AT_fund_type";
    case AT_mod_fund_type:		return "AT_mod_fund_type";
    case AT_user_def_type:		return "AT_user_def_type";
    case AT_mod_u_d_type:		return "AT_mod_u_d_type";
    case AT_ordering:			return "AT_ordering";
    case AT_subscr_data:		return "AT_subscr_data";
    case AT_byte_size:			return "AT_byte_size";
    case AT_bit_offset:			return "AT_bit_offset";
    case AT_bit_size:			return "AT_bit_size";
    case AT_element_list:		return "AT_element_list";
    case AT_stmt_list:			return "AT_stmt_list";
    case AT_low_pc:			return "AT_low_pc";
    case AT_high_pc:			return "AT_high_pc";
    case AT_language:			return "AT_language";
    case AT_member:			return "AT_member";
    case AT_discr:			return "AT_discr";
    case AT_discr_value:		return "AT_discr_value";
    case AT_string_length:		return "AT_string_length";
    case AT_common_reference:		return "AT_common_reference";
    case AT_comp_dir:			return "AT_comp_dir";
    case AT_const_value_string:		return "AT_const_value_string";
    case AT_const_value_data2:		return "AT_const_value_data2";
    case AT_const_value_data4:		return "AT_const_value_data4";
    case AT_const_value_data8:		return "AT_const_value_data8";
    case AT_const_value_block2:		return "AT_const_value_block2";
    case AT_const_value_block4:		return "AT_const_value_block4";
    case AT_containing_type:		return "AT_containing_type";
    case AT_default_value_addr:		return "AT_default_value_addr";
    case AT_default_value_data2:	return "AT_default_value_data2";
    case AT_default_value_data4:	return "AT_default_value_data4";
    case AT_default_value_data8:	return "AT_default_value_data8";
    case AT_default_value_string:	return "AT_default_value_string";
    case AT_friends:			return "AT_friends";
    case AT_inline:			return "AT_inline";
    case AT_is_optional:		return "AT_is_optional";
    case AT_lower_bound_ref:		return "AT_lower_bound_ref";
    case AT_lower_bound_data2:		return "AT_lower_bound_data2";
    case AT_lower_bound_data4:		return "AT_lower_bound_data4";
    case AT_lower_bound_data8:		return "AT_lower_bound_data8";
    case AT_private:			return "AT_private";
    case AT_producer:			return "AT_producer";
    case AT_program:			return "AT_program";
    case AT_protected:			return "AT_protected";
    case AT_prototyped:			return "AT_prototyped";
    case AT_public:			return "AT_public";
    case AT_pure_virtual:		return "AT_pure_virtual";
    case AT_return_addr:		return "AT_return_addr";
    case AT_abstract_origin:		return "AT_abstract_origin";
    case AT_start_scope:		return "AT_start_scope";
    case AT_stride_size:		return "AT_stride_size";
    case AT_upper_bound_ref:		return "AT_upper_bound_ref";
    case AT_upper_bound_data2:		return "AT_upper_bound_data2";
    case AT_upper_bound_data4:		return "AT_upper_bound_data4";
    case AT_upper_bound_data8:		return "AT_upper_bound_data8";
    case AT_virtual:			return "AT_virtual";

    /* GNU extensions */

    case AT_sf_names:			return "AT_sf_names";
    case AT_src_info:			return "AT_src_info";
    case AT_mac_info:			return "AT_mac_info";
    case AT_src_coords:			return "AT_src_coords";
    case AT_body_begin:			return "AT_body_begin";
    case AT_body_end:			return "AT_body_end";

    default:				return "AT_<unknown>";
    }
}

static char *
dwarf_stack_op_name (op)
     register unsigned op;
{
  switch (op)
    {
    case OP_REG:		return "OP_REG";
    case OP_BASEREG:		return "OP_BASEREG";
    case OP_ADDR:		return "OP_ADDR";
    case OP_CONST:		return "OP_CONST";
    case OP_DEREF2:		return "OP_DEREF2";
    case OP_DEREF4:		return "OP_DEREF4";
    case OP_ADD:		return "OP_ADD";
    default:			return "OP_<unknown>";
    }
}

static char *
dwarf_typemod_name (mod)
     register unsigned mod;
{
  switch (mod)
    {
    case MOD_pointer_to:	return "MOD_pointer_to";
    case MOD_reference_to:	return "MOD_reference_to";
    case MOD_const:		return "MOD_const";
    case MOD_volatile:		return "MOD_volatile";
    default:			return "MOD_<unknown>";
    }
}

static char *
dwarf_fmt_byte_name (fmt)
     register unsigned fmt;
{
  switch (fmt)
    {
    case FMT_FT_C_C:	return "FMT_FT_C_C";
    case FMT_FT_C_X:	return "FMT_FT_C_X";
    case FMT_FT_X_C:	return "FMT_FT_X_C";
    case FMT_FT_X_X:	return "FMT_FT_X_X";
    case FMT_UT_C_C:	return "FMT_UT_C_C";
    case FMT_UT_C_X:	return "FMT_UT_C_X";
    case FMT_UT_X_C:	return "FMT_UT_X_C";
    case FMT_UT_X_X:	return "FMT_UT_X_X";
    case FMT_ET:	return "FMT_ET";
    default:		return "FMT_<unknown>";
    }
}
static char *
dwarf_fund_type_name (ft)
     register unsigned ft;
{
  switch (ft)
    {
    case FT_char:		return "FT_char";
    case FT_signed_char:	return "FT_signed_char";
    case FT_unsigned_char:	return "FT_unsigned_char";
    case FT_short:		return "FT_short";
    case FT_signed_short:	return "FT_signed_short";
    case FT_unsigned_short:	return "FT_unsigned_short";
    case FT_integer:		return "FT_integer";
    case FT_signed_integer:	return "FT_signed_integer";
    case FT_unsigned_integer:	return "FT_unsigned_integer";
    case FT_long:		return "FT_long";
    case FT_signed_long:	return "FT_signed_long";
    case FT_unsigned_long:	return "FT_unsigned_long";
    case FT_pointer:		return "FT_pointer";
    case FT_float:		return "FT_float";
    case FT_dbl_prec_float:	return "FT_dbl_prec_float";
    case FT_ext_prec_float:	return "FT_ext_prec_float";
    case FT_complex:		return "FT_complex";
    case FT_dbl_prec_complex:	return "FT_dbl_prec_complex";
    case FT_void:		return "FT_void";
    case FT_boolean:		return "FT_boolean";
    case FT_ext_prec_complex:	return "FT_ext_prec_complex";
    case FT_label:		return "FT_label";

    /* GNU extensions.  */

    case FT_long_long:		return "FT_long_long";
    case FT_signed_long_long:	return "FT_signed_long_long";
    case FT_unsigned_long_long: return "FT_unsigned_long_long";

    case FT_int8:		return "FT_int8";
    case FT_signed_int8:	return "FT_signed_int8";
    case FT_unsigned_int8:	return "FT_unsigned_int8";
    case FT_int16:		return "FT_int16";
    case FT_signed_int16:	return "FT_signed_int16";
    case FT_unsigned_int16:	return "FT_unsigned_int16";
    case FT_int32:		return "FT_int32";
    case FT_signed_int32:	return "FT_signed_int32";
    case FT_unsigned_int32:	return "FT_unsigned_int32";
    case FT_int64:		return "FT_int64";
    case FT_signed_int64:	return "FT_signed_int64";
    case FT_unsigned_int64:	return "FT_signed_int64";

    case FT_real32:		return "FT_real32";
    case FT_real64:		return "FT_real64";
    case FT_real96:		return "FT_real96";
    case FT_real128:		return "FT_real128";

    default:			return "FT_<unknown>";
    }
}

/* Determine the "ultimate origin" of a decl.  The decl may be an
   inlined instance of an inlined instance of a decl which is local
   to an inline function, so we have to trace all of the way back
   through the origin chain to find out what sort of node actually
   served as the original seed for the given block.  */

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

/* Determine the "ultimate origin" of a block.  The block may be an
   inlined instance of an inlined instance of a block which is local
   to an inline function, so we have to trace all of the way back
   through the origin chain to find out what sort of node actually
   served as the original seed for the given block.  */

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

static void
output_unsigned_leb128 (value)
     register unsigned long value;
{
  register unsigned long orig_value = value;

  do
    {
      register unsigned byte = (value & 0x7f);

      value >>= 7;
      if (value != 0)	/* more bytes to follow */
	byte |= 0x80;
      fprintf (asm_out_file, "\t%s\t0x%x", ASM_BYTE_OP, (unsigned) byte);
      if (flag_verbose_asm && value == 0)
	fprintf (asm_out_file, "\t%s ULEB128 number - value = %u",
		 ASM_COMMENT_START, orig_value);
      fputc ('\n', asm_out_file);
    }
  while (value != 0);
}

static void
output_signed_leb128 (value)
     register long value;
{
  register long orig_value = value;
  register int negative = (value < 0);
  register int more;

  do
    {
      register unsigned byte = (value & 0x7f);

      value >>= 7;
      if (negative)
	value |= 0xfe000000;  /* manually sign extend */
      if (((value == 0) && ((byte & 0x40) == 0))
          || ((value == -1) && ((byte & 0x40) == 1)))
	more = 0;
      else
	{
	  byte |= 0x80;
	  more = 1;
	}
      fprintf (asm_out_file, "\t%s\t0x%x", ASM_BYTE_OP, (unsigned) byte);
      if (flag_verbose_asm && more == 0)
	fprintf (asm_out_file, "\t%s SLEB128 number - value = %d",
		 ASM_COMMENT_START, orig_value);
      fputc ('\n', asm_out_file);
    }
  while (more);
}

/**************** utility functions for attribute functions ******************/

/* Given a pointer to a BLOCK node return non-zero if (and only if) the
   node in question represents the outermost pair of curly braces (i.e.
   the "body block") of a function or method.

   For any BLOCK node representing a "body block" of a function or method,
   the BLOCK_SUPERCONTEXT of the node will point to another BLOCK node
   which represents the outermost (function) scope for the function or
   method (i.e. the one which includes the formal parameters).  The
   BLOCK_SUPERCONTEXT of *that* node in turn will point to the relevant
   FUNCTION_DECL node.
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

/* Given a pointer to a tree node for some type, return a Dwarf fundamental
   type code for the given type.

   This routine must only be called for GCC type nodes that correspond to
   Dwarf fundamental types.

   The current Dwarf draft specification calls for Dwarf fundamental types
   to accurately reflect the fact that a given type was either a "plain"
   integral type or an explicitly "signed" integral type.  Unfortunately,
   we can't always do this, because GCC may already have thrown away the
   information about the precise way in which the type was originally
   specified, as in:

	typedef signed int my_type;

	struct s { my_type f; };

   Since we may be stuck here without enought information to do exactly
   what is called for in the Dwarf draft specification, we do the best
   that we can under the circumstances and always use the "plain" integral
   fundamental type codes for int, short, and long types.  That's probably
   good enough.  The additional accuracy called for in the current DWARF
   draft specification is probably never even useful in practice.  */

static int
fundamental_type_code (type)
     register tree type;
{
  if (TREE_CODE (type) == ERROR_MARK)
    return 0;

  switch (TREE_CODE (type))
    {
      case ERROR_MARK:
	return FT_void;

      case VOID_TYPE:
	return FT_void;

      case INTEGER_TYPE:
	/* Carefully distinguish all the standard types of C,
	   without messing up if the language is not C.
	   Note that we check only for the names that contain spaces;
	   other names might occur by coincidence in other languages.  */
	if (TYPE_NAME (type) != 0
	    && TREE_CODE (TYPE_NAME (type)) == TYPE_DECL
	    && DECL_NAME (TYPE_NAME (type)) != 0
	    && TREE_CODE (DECL_NAME (TYPE_NAME (type))) == IDENTIFIER_NODE)
	  {
	    char *name = IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (type)));

	    if (!strcmp (name, "unsigned char"))
	      return FT_unsigned_char;
	    if (!strcmp (name, "signed char"))
	      return FT_signed_char;
	    if (!strcmp (name, "unsigned int"))
	      return FT_unsigned_integer;
	    if (!strcmp (name, "short int"))
	      return FT_short;
	    if (!strcmp (name, "short unsigned int"))
	      return FT_unsigned_short;
	    if (!strcmp (name, "long int"))
	      return FT_long;
	    if (!strcmp (name, "long unsigned int"))
	      return FT_unsigned_long;
	    if (!strcmp (name, "long long int"))
	      return FT_long_long;		/* Not grok'ed by svr4 SDB */
	    if (!strcmp (name, "long long unsigned int"))
	      return FT_unsigned_long_long;	/* Not grok'ed by svr4 SDB */
	  }

	/* Most integer types will be sorted out above, however, for the
	   sake of special `array index' integer types, the following code
	   is also provided.  */

	if (TYPE_PRECISION (type) == INT_TYPE_SIZE)
	  return (TREE_UNSIGNED (type) ? FT_unsigned_integer : FT_integer);

	if (TYPE_PRECISION (type) == LONG_TYPE_SIZE)
	  return (TREE_UNSIGNED (type) ? FT_unsigned_long : FT_long);

	if (TYPE_PRECISION (type) == LONG_LONG_TYPE_SIZE)
	  return (TREE_UNSIGNED (type) ? FT_unsigned_long_long : FT_long_long);

	if (TYPE_PRECISION (type) == SHORT_TYPE_SIZE)
	  return (TREE_UNSIGNED (type) ? FT_unsigned_short : FT_short);

	if (TYPE_PRECISION (type) == CHAR_TYPE_SIZE)
	  return (TREE_UNSIGNED (type) ? FT_unsigned_char : FT_char);

	abort ();

      case REAL_TYPE:
	/* Carefully distinguish all the standard types of C,
	   without messing up if the language is not C.  */
	if (TYPE_NAME (type) != 0
	    && TREE_CODE (TYPE_NAME (type)) == TYPE_DECL
	    && DECL_NAME (TYPE_NAME (type)) != 0
	    && TREE_CODE (DECL_NAME (TYPE_NAME (type))) == IDENTIFIER_NODE)
	  {
	    char *name = IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (type)));

	    /* Note that here we can run afowl of a serious bug in "classic"
	       svr4 SDB debuggers.  They don't seem to understand the
	       FT_ext_prec_float type (even though they should).  */

	    if (!strcmp (name, "long double"))
	      return FT_ext_prec_float;
	  }

	if (TYPE_PRECISION (type) == DOUBLE_TYPE_SIZE)
	  return FT_dbl_prec_float;
	if (TYPE_PRECISION (type) == FLOAT_TYPE_SIZE)
	  return FT_float;

	/* Note that here we can run afowl of a serious bug in "classic"
	   svr4 SDB debuggers.  They don't seem to understand the
	   FT_ext_prec_float type (even though they should).  */

	if (TYPE_PRECISION (type) == LONG_DOUBLE_TYPE_SIZE)
	  return FT_ext_prec_float;
	abort ();

      case COMPLEX_TYPE:
	return FT_complex;	/* GNU FORTRAN COMPLEX type.  */

      case CHAR_TYPE:
	return FT_char;		/* GNU Pascal CHAR type.  Not used in C.  */

      case BOOLEAN_TYPE:
	return FT_boolean;	/* GNU FORTRAN BOOLEAN type.  */

      default:
	abort ();	/* No other TREE_CODEs are Dwarf fundamental types.  */
    }
  return 0;
}

/* Given a pointer to an arbitrary ..._TYPE tree node, return a pointer to
   the Dwarf "root" type for the given input type.  The Dwarf "root" type
   of a given type is generally the same as the given type, except that if
   the	given type is a pointer or reference type, then the root type of
   the given type is the root type of the "basis" type for the pointer or
   reference type.  (This definition of the "root" type is recursive.)
   Also, the root type of a `const' qualified type or a `volatile'
   qualified type is the root type of the given type without the
   qualifiers.  */

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

/* Given a pointer to an arbitrary ..._TYPE tree node, write out a sequence
   of zero or more Dwarf "type-modifier" bytes applicable to the type.	*/

static void
write_modifier_bytes (type, decl_const, decl_volatile)
     register tree type;
     register int decl_const;
     register int decl_volatile;
{
  if (TREE_CODE (type) == ERROR_MARK)
    return;

  if (TYPE_READONLY (type) || decl_const)
    ASM_OUTPUT_DWARF_TYPE_MODIFIER (asm_out_file, MOD_const);
  if (TYPE_VOLATILE (type) || decl_volatile)
    ASM_OUTPUT_DWARF_TYPE_MODIFIER (asm_out_file, MOD_volatile);
  switch (TREE_CODE (type))
    {
      case POINTER_TYPE:
	ASM_OUTPUT_DWARF_TYPE_MODIFIER (asm_out_file, MOD_pointer_to);
	write_modifier_bytes (TREE_TYPE (type), 0, 0);
	return;

      case REFERENCE_TYPE:
	ASM_OUTPUT_DWARF_TYPE_MODIFIER (asm_out_file, MOD_reference_to);
	write_modifier_bytes (TREE_TYPE (type), 0, 0);
	return;

      case ERROR_MARK:
      default:
	return;
    }
}

/* Given a pointer to an arbitrary ..._TYPE tree node, return non-zero if the
   given input type is a Dwarf "fundamental" type.  Otherwise return zero.  */

inline int
type_is_fundamental (type)
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

/* Given a pointer to some ..._DECL tree node, generate an assembly language
   equate directive which will associate a symbolic name with the current DIE.

   The name used is an artificial label generated from the DECL_UID number
   associated with the given decl node.  The name it gets equated to is the
   symbolic label that we (previously) output at the start of the DIE that
   we are currently generating.

   Calling this function while generating some "decl related" form of DIE
   makes it possible to later refer to the DIE which represents the given
   decl simply by re-generating the symbolic name from the ..._DECL node's
   UID number.	*/

static void
equate_decl_number_to_die_number (decl)
     register tree decl;
{
  /* In the case where we are generating a DIE for some ..._DECL node
     which represents either some inline function declaration or some
     entity declared within an inline function declaration/definition,
     setup a symbolic name for the current DIE so that we have a name
     for this DIE that we can easily refer to later on within
     AT_abstract_origin attributes.  */

  char decl_label[MAX_ARTIFICIAL_LABEL_BYTES];
  char die_label[MAX_ARTIFICIAL_LABEL_BYTES];

  sprintf (decl_label, DECL_NAME_FMT, DECL_UID (decl));
  sprintf (die_label, DIE_BEGIN_LABEL_FMT, current_dienum);
  ASM_OUTPUT_DEF (asm_out_file, decl_label, die_label);
}

/* Given a pointer to some ..._TYPE tree node, generate an assembly language
   equate directive which will associate a symbolic name with the current DIE.

   The name used is an artificial label generated from the TYPE_UID number
   associated with the given type node.  The name it gets equated to is the
   symbolic label that we (previously) output at the start of the DIE that
   we are currently generating.

   Calling this function while generating some "type related" form of DIE
   makes it easy to later refer to the DIE which represents the given type
   simply by re-generating the alternative name from the ..._TYPE node's
   UID number.	*/

inline void
equate_type_number_to_die_number (type)
     register tree type;
{
  char type_label[MAX_ARTIFICIAL_LABEL_BYTES];
  char die_label[MAX_ARTIFICIAL_LABEL_BYTES];

  /* We are generating a DIE to represent the main variant of this type
     (i.e the type without any const or volatile qualifiers) so in order
     to get the equate to come out right, we need to get the main variant
     itself here.  */

  type = type_main_variant (type);

  sprintf (type_label, TYPE_NAME_FMT, TYPE_UID (type));
  sprintf (die_label, DIE_BEGIN_LABEL_FMT, current_dienum);
  ASM_OUTPUT_DEF (asm_out_file, type_label, die_label);
}

static void
output_reg_number (rtl)
     register rtx rtl;
{
  register unsigned regno = REGNO (rtl);

  if (regno >= FIRST_PSEUDO_REGISTER)
    {
      warning_with_decl (dwarf_last_decl, "internal regno botch: regno = %d\n",
			 regno);
      regno = 0;
    }
  fprintf (asm_out_file, "\t%s\t0x%x",
	   UNALIGNED_INT_ASM_OP, DBX_REGISTER_NUMBER (regno));
  if (flag_verbose_asm)
    {
      fprintf (asm_out_file, "\t%s ", ASM_COMMENT_START);
      PRINT_REG (rtl, 0, asm_out_file);
    }
  fputc ('\n', asm_out_file);
}

/* The following routine is a nice and simple transducer.  It converts the
   RTL for a variable or parameter (resident in memory) into an equivalent
   Dwarf representation of a mechanism for getting the address of that same
   variable onto the top of a hypothetical "address evaluation" stack.

   When creating memory location descriptors, we are effectively trans-
   forming the RTL for a memory-resident object into its Dwarf postfix
   expression equivalent.  This routine just recursively descends an
   RTL tree, turning it into Dwarf postfix code as it goes.  */

static void
output_mem_loc_descriptor (rtl)
      register rtx rtl;
{
  /* Note that for a dynamically sized array, the location we will
     generate a description of here will be the lowest numbered location
     which is actually within the array.  That's *not* necessarily the
     same as the zeroth element of the array.  */

  switch (GET_CODE (rtl))
    {
      case SUBREG:

	/* The case of a subreg may arise when we have a local (register)
	   variable or a formal (register) parameter which doesn't quite
	   fill up an entire register.	For now, just assume that it is
	   legitimate to make the Dwarf info refer to the whole register
	   which contains the given subreg.  */

	rtl = XEXP (rtl, 0);
	/* Drop thru.  */

      case REG:

	/* Whenever a register number forms a part of the description of
	   the method for calculating the (dynamic) address of a memory
	   resident object, DWARF rules require the register number to
	   be referred to as a "base register".  This distinction is not
	   based in any way upon what category of register the hardware
	   believes the given register belongs to.  This is strictly
	   DWARF terminology we're dealing with here.

	   Note that in cases where the location of a memory-resident data
	   object could be expressed as:

		    OP_ADD (OP_BASEREG (basereg), OP_CONST (0))

	   the actual DWARF location descriptor that we generate may just
	   be OP_BASEREG (basereg).  This may look deceptively like the
	   object in question was allocated to a register (rather than
	   in memory) so DWARF consumers need to be aware of the subtle
	   distinction between OP_REG and OP_BASEREG.  */

	ASM_OUTPUT_DWARF_STACK_OP (asm_out_file, OP_BASEREG);
	output_reg_number (rtl);
	break;

      case MEM:
	output_mem_loc_descriptor (XEXP (rtl, 0));
	ASM_OUTPUT_DWARF_STACK_OP (asm_out_file, OP_DEREF4);
	break;

      case CONST:
      case SYMBOL_REF:
	ASM_OUTPUT_DWARF_STACK_OP (asm_out_file, OP_ADDR);
	ASM_OUTPUT_DWARF_ADDR_CONST (asm_out_file, rtl);
	break;

      case PLUS:
	output_mem_loc_descriptor (XEXP (rtl, 0));
	output_mem_loc_descriptor (XEXP (rtl, 1));
	ASM_OUTPUT_DWARF_STACK_OP (asm_out_file, OP_ADD);
	break;

      case CONST_INT:
	ASM_OUTPUT_DWARF_STACK_OP (asm_out_file, OP_CONST);
	ASM_OUTPUT_DWARF_DATA4 (asm_out_file, INTVAL (rtl));
	break;

      default:
	abort ();
    }
}

/* Output a proper Dwarf location descriptor for a variable or parameter
   which is either allocated in a register or in a memory location.  For
   a register, we just generate an OP_REG and the register number.  For a
   memory location we provide a Dwarf postfix expression describing how to
   generate the (dynamic) address of the object onto the address stack.  */

static void
output_loc_descriptor (rtl)
     register rtx rtl;
{
  switch (GET_CODE (rtl))
    {
    case SUBREG:

	/* The case of a subreg may arise when we have a local (register)
	   variable or a formal (register) parameter which doesn't quite
	   fill up an entire register.	For now, just assume that it is
	   legitimate to make the Dwarf info refer to the whole register
	   which contains the given subreg.  */

	rtl = XEXP (rtl, 0);
	/* Drop thru.  */

    case REG:
	ASM_OUTPUT_DWARF_STACK_OP (asm_out_file, OP_REG);
	output_reg_number (rtl);
	break;

    case MEM:
      output_mem_loc_descriptor (XEXP (rtl, 0));
      break;

    default:
      abort ();		/* Should never happen */
    }
}

/* Given a tree node describing an array bound (either lower or upper)
   output a representation for that bound.  */

static void
output_bound_representation (bound, dim_num, u_or_l)
     register tree bound;
     register unsigned dim_num; /* For multi-dimensional arrays.  */
     register char u_or_l;	/* Designates upper or lower bound.  */
{
  switch (TREE_CODE (bound))
    {

      case ERROR_MARK:
	return;

      /* All fixed-bounds are represented by INTEGER_CST nodes.	 */

      case INTEGER_CST:
	ASM_OUTPUT_DWARF_DATA4 (asm_out_file,
				(unsigned) TREE_INT_CST_LOW (bound));
	break;

      /* Dynamic bounds may be represented by NOP_EXPR nodes containing
	 SAVE_EXPR nodes.  */

      case NOP_EXPR:
	bound = TREE_OPERAND (bound, 0);
	/* ... fall thru... */

      case SAVE_EXPR:
	{
	  char begin_label[MAX_ARTIFICIAL_LABEL_BYTES];
	  char end_label[MAX_ARTIFICIAL_LABEL_BYTES];

	  sprintf (begin_label, BOUND_BEGIN_LABEL_FMT,
				current_dienum, dim_num, u_or_l);

	  sprintf (end_label,	BOUND_END_LABEL_FMT,
				current_dienum, dim_num, u_or_l);

	  ASM_OUTPUT_DWARF_DELTA2 (asm_out_file, end_label, begin_label);
	  ASM_OUTPUT_LABEL (asm_out_file, begin_label);

	  /* If we are working on a bound for a dynamic dimension in C,
	     the dynamic dimension in question had better have a static
	     (zero) lower bound and a dynamic *upper* bound.  */

	  if (u_or_l != 'u')
	    abort ();

	  /* If optimization is turned on, the SAVE_EXPRs that describe
	     how to access the upper bound values are essentially bogus.
	     They only describe (at best) how to get at these values at
	     the points in the generated code right after they have just
	     been computed.  Worse yet, in the typical case, the upper
	     bound values will not even *be* computed in the optimized
	     code, so these SAVE_EXPRs are entirely bogus.

	     In order to compensate for this fact, we check here to see
	     if optimization is enabled, and if so, we effectively create
	     an empty location description for the (unknown and unknowable)
	     upper bound.

	     This should not cause too much trouble for existing (stupid?)
	     debuggers because they have to deal with empty upper bounds
	     location descriptions anyway in order to be able to deal with
	     incomplete array types.

	     Of course an intelligent debugger (GDB?) should be able to
	     comprehend that a missing upper bound specification in a
	     array type used for a storage class `auto' local array variable
	     indicates that the upper bound is both unknown (at compile-
	     time) and unknowable (at run-time) due to optimization.
	  */

	  if (! optimize)
	    output_loc_descriptor
	      (eliminate_regs (SAVE_EXPR_RTL (bound), 0, NULL_RTX));

	  ASM_OUTPUT_LABEL (asm_out_file, end_label);
	}
	break;

      default:
	abort ();
    }
}

/* Recursive function to output a sequence of value/name pairs for
   enumeration constants in reversed order.  This is called from
   enumeration_type_die.  */

static void
output_enumeral_list (link)
     register tree link;
{
  if (link)
    {
      output_enumeral_list (TREE_CHAIN (link));
      ASM_OUTPUT_DWARF_DATA4 (asm_out_file,
			      (unsigned) TREE_INT_CST_LOW (TREE_VALUE (link)));
      ASM_OUTPUT_DWARF_STRING (asm_out_file,
			       IDENTIFIER_POINTER (TREE_PURPOSE (link)));
    }
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
   `integer_type_node' if the given node turns out to be an ERROR_MARK node.  */

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
   BITS_PER_WORD if the node actually turns out to be an ERROR_MARK node.  */

inline unsigned
simple_type_align_in_bits (type)
     register tree type;
{
  return (TREE_CODE (type) != ERROR_MARK) ? TYPE_ALIGN (type) : BITS_PER_WORD;
}

/* Given a pointer to a tree node, assumed to be some kind of a ..._TYPE
   node, return the size in bits for the type if it is a constant, or
   else return the alignment for the type if the type's size is not
   constant, or else return BITS_PER_WORD if the type actually turns out
   to be an ERROR_MARK node.  */

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
   object" for the given FIELD_DECL, or return 0 if we are unable to deter-
   mine what that offset is, either because the argument turns out to be a
   pointer to an ERROR_MARK node, or because the offset is actually variable.
   (We can't handle the latter case just yet.)  */

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

  /* We cannot yet cope with fields whose positions or sizes are variable,
     so for now, when we see such things, we simply return 0.  Someday,
     we may be able to handle such cases, but it will be damn difficult.  */

  if (TREE_CODE (bitpos_tree) != INTEGER_CST)
    return 0;
  bitpos_int = (unsigned) TREE_INT_CST_LOW (bitpos_tree);

  if (TREE_CODE (field_size_tree) != INTEGER_CST)
    return 0;
  field_size_in_bits = (unsigned) TREE_INT_CST_LOW (field_size_tree);

  type_size_in_bits = simple_type_size_in_bits (type);

  type_align_in_bits = simple_type_align_in_bits (type);
  type_align_in_bytes = type_align_in_bits / BITS_PER_UNIT;

  /* Note that the GCC front-end doesn't make any attempt to keep track
     of the starting bit offset (relative to the start of the containing
     structure type) of the hypothetical "containing object" for a bit-
     field.  Thus, when computing the byte offset value for the start of
     the "containing object" of a bit-field, we must deduce this infor-
     mation on our own.

     This can be rather tricky to do in some cases.  For example, handling
     the following structure type definition when compiling for an i386/i486
     target (which only aligns long long's to 32-bit boundaries) can be very
     tricky:

		struct S {
			int		field1;
			long long	field2:31;
		};

     Fortunately, there is a simple rule-of-thumb which can be used in such
     cases.  When compiling for an i386/i486, GCC will allocate 8 bytes for
     the structure shown above.  It decides to do this based upon one simple
     rule for bit-field allocation.  Quite simply, GCC allocates each "con-
     taining object" for each bit-field at the first (i.e. lowest addressed)
     legitimate alignment boundary (based upon the required minimum alignment
     for the declared type of the field) which it can possibly use, subject
     to the condition that there is still enough available space remaining
     in the containing object (when allocated at the selected point) to
     fully accommodate all of the bits of the bit-field itself.

     This simple rule makes it obvious why GCC allocates 8 bytes for each
     object of the structure type shown above.  When looking for a place to
     allocate the "containing object" for `field2', the compiler simply tries
     to allocate a 64-bit "containing object" at each successive 32-bit
     boundary (starting at zero) until it finds a place to allocate that 64-
     bit field such that at least 31 contiguous (and previously unallocated)
     bits remain within that selected 64 bit field.  (As it turns out, for
     the example above, the compiler finds that it is OK to allocate the
     "containing object" 64-bit field at bit-offset zero within the
     structure type.)

     Here we attempt to work backwards from the limited set of facts we're
     given, and we try to deduce from those facts, where GCC must have
     believed that the containing object started (within the structure type).

     The value we deduce is then used (by the callers of this routine) to
     generate AT_location and AT_bit_offset attributes for fields (both
     bit-fields and, in the case of AT_location, regular fields as well).
  */

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

/* The following routines are responsible for writing out the various types
   of Dwarf attributes (and any following data bytes associated with them).
   These routines are listed in order based on the numerical codes of their
   associated attributes.  */

/* Generate an AT_sibling attribute.  */

inline void
sibling_attribute ()
{
  char label[MAX_ARTIFICIAL_LABEL_BYTES];

  ASM_OUTPUT_DWARF_ATTRIBUTE (asm_out_file, AT_sibling);
  sprintf (label, DIE_BEGIN_LABEL_FMT, NEXT_DIE_NUM);
  ASM_OUTPUT_DWARF_REF (asm_out_file, label);
}

/* Output the form of location attributes suitable for whole variables and
   whole parameters.  Note that the location attributes for struct fields
   are generated by the routine `data_member_location_attribute' below.  */

static void
location_attribute (rtl)
     register rtx rtl;
{
  char begin_label[MAX_ARTIFICIAL_LABEL_BYTES];
  char end_label[MAX_ARTIFICIAL_LABEL_BYTES];

  ASM_OUTPUT_DWARF_ATTRIBUTE (asm_out_file, AT_location);
  sprintf (begin_label, LOC_BEGIN_LABEL_FMT, current_dienum);
  sprintf (end_label, LOC_END_LABEL_FMT, current_dienum);
  ASM_OUTPUT_DWARF_DELTA2 (asm_out_file, end_label, begin_label);
  ASM_OUTPUT_LABEL (asm_out_file, begin_label);

  /* Handle a special case.  If we are about to output a location descriptor
     for a variable or parameter which has been optimized out of existence,
     don't do that.  Instead we output a zero-length location descriptor
     value as part of the location attribute.

     A variable which has been optimized out of existence will have a
     DECL_RTL value which denotes a pseudo-reg.

     Currently, in some rare cases, variables can have DECL_RTL values
     which look like (MEM (REG pseudo-reg#)).  These cases are due to
     bugs elsewhere in the compiler.  We treat such cases
     as if the variable(s) in question had been optimized out of existence.

     Note that in all cases where we wish to express the fact that a
     variable has been optimized out of existence, we do not simply
     suppress the generation of the entire location attribute because
     the absence of a location attribute in certain kinds of DIEs is
     used to indicate something else entirely... i.e. that the DIE
     represents an object declaration, but not a definition.  So sayeth
     the PLSIG.
  */

  if (! is_pseudo_reg (rtl)
      && (GET_CODE (rtl) != MEM || ! is_pseudo_reg (XEXP (rtl, 0))))
    output_loc_descriptor (eliminate_regs (rtl, 0, NULL_RTX));

  ASM_OUTPUT_LABEL (asm_out_file, end_label);
}

/* Output the specialized form of location attribute used for data members
   of struct and union types.

   In the special case of a FIELD_DECL node which represents a bit-field,
   the "offset" part of this special location descriptor must indicate the
   distance in bytes from the lowest-addressed byte of the containing
   struct or union type to the lowest-addressed byte of the "containing
   object" for the bit-field.  (See the `field_byte_offset' function above.)

   For any given bit-field, the "containing object" is a hypothetical
   object (of some integral or enum type) within which the given bit-field
   lives.  The type of this hypothetical "containing object" is always the
   same as the declared type of the individual bit-field itself (for GCC
   anyway... the DWARF spec doesn't actually mandate this).

   Note that it is the size (in bytes) of the hypothetical "containing
   object" which will be given in the AT_byte_size attribute for this
   bit-field.  (See the `byte_size_attribute' function below.)  It is
   also used when calculating the value of the AT_bit_offset attribute.
   (See the `bit_offset_attribute' function below.)
*/

static void
data_member_location_attribute (decl)
     register tree decl;
{
  register unsigned object_offset_in_bytes = field_byte_offset (decl);
  char begin_label[MAX_ARTIFICIAL_LABEL_BYTES];
  char end_label[MAX_ARTIFICIAL_LABEL_BYTES];

  ASM_OUTPUT_DWARF_ATTRIBUTE (asm_out_file, AT_location);
  sprintf (begin_label, LOC_BEGIN_LABEL_FMT, current_dienum);
  sprintf (end_label, LOC_END_LABEL_FMT, current_dienum);
  ASM_OUTPUT_DWARF_DELTA2 (asm_out_file, end_label, begin_label);
  ASM_OUTPUT_LABEL (asm_out_file, begin_label);
  ASM_OUTPUT_DWARF_STACK_OP (asm_out_file, OP_CONST);
  ASM_OUTPUT_DWARF_DATA4 (asm_out_file, object_offset_in_bytes);
  ASM_OUTPUT_DWARF_STACK_OP (asm_out_file, OP_ADD);
  ASM_OUTPUT_LABEL (asm_out_file, end_label);
}

/* Output an AT_const_value attribute for a variable or a parameter which
   does not have a "location" either in memory or in a register.  These
   things can arise in GNU C when a constant is passed as an actual
   parameter to an inlined function.  They can also arise in C++ where
   declared constants do not necessarily get memory "homes".  */

static void
const_value_attribute (rtl)
     register rtx rtl;
{
  char begin_label[MAX_ARTIFICIAL_LABEL_BYTES];
  char end_label[MAX_ARTIFICIAL_LABEL_BYTES];

  ASM_OUTPUT_DWARF_ATTRIBUTE (asm_out_file, AT_const_value_block4);
  sprintf (begin_label, LOC_BEGIN_LABEL_FMT, current_dienum);
  sprintf (end_label, LOC_END_LABEL_FMT, current_dienum);
  ASM_OUTPUT_DWARF_DELTA4 (asm_out_file, end_label, begin_label);
  ASM_OUTPUT_LABEL (asm_out_file, begin_label);

  switch (GET_CODE (rtl))
    {
      case CONST_INT:
	/* Note that a CONST_INT rtx could represent either an integer or
	   a floating-point constant.  A CONST_INT is used whenever the
	   constant will fit into a single word.  In all such cases, the
	   original mode of the constant value is wiped out, and the
	   CONST_INT rtx is assigned VOIDmode.  Since we no longer have
	   precise mode information for these constants, we always just
	   output them using 4 bytes.  */

	ASM_OUTPUT_DWARF_DATA4 (asm_out_file, (unsigned) INTVAL (rtl));
	break;

      case CONST_DOUBLE:
	/* Note that a CONST_DOUBLE rtx could represent either an integer
	   or a floating-point constant.  A CONST_DOUBLE is used whenever
	   the constant requires more than one word in order to be adequately
	   represented.  In all such cases, the original mode of the constant
	   value is preserved as the mode of the CONST_DOUBLE rtx, but for
	   simplicity we always just output CONST_DOUBLEs using 8 bytes.  */

	ASM_OUTPUT_DWARF_DATA8 (asm_out_file,
				(unsigned HOST_WIDE_INT) CONST_DOUBLE_HIGH (rtl),
				(unsigned HOST_WIDE_INT) CONST_DOUBLE_LOW (rtl));
	break;

      case CONST_STRING:
	ASM_OUTPUT_DWARF_STRING (asm_out_file, XSTR (rtl, 0));
	break;

      case SYMBOL_REF:
      case LABEL_REF:
      case CONST:
	ASM_OUTPUT_DWARF_ADDR_CONST (asm_out_file, rtl);
	break;

      case PLUS:
	/* In cases where an inlined instance of an inline function is passed
	   the address of an `auto' variable (which is local to the caller)
	   we can get a situation where the DECL_RTL of the artificial
	   local variable (for the inlining) which acts as a stand-in for
	   the corresponding formal parameter (of the inline function)
	   will look like (plus:SI (reg:SI FRAME_PTR) (const_int ...)).
	   This is not exactly a compile-time constant expression, but it
	   isn't the address of the (artificial) local variable either.
	   Rather, it represents the *value* which the artificial local
	   variable always has during its lifetime.  We currently have no
	   way to represent such quasi-constant values in Dwarf, so for now
	   we just punt and generate an AT_const_value attribute with form
	   FORM_BLOCK4 and a length of zero.  */
	break;

      default:
	abort ();  /* No other kinds of rtx should be possible here.  */
    }

  ASM_OUTPUT_LABEL (asm_out_file, end_label);
}

/* Generate *either* an AT_location attribute or else an AT_const_value
   data attribute for a variable or a parameter.  We generate the
   AT_const_value attribute only in those cases where the given
   variable or parameter does not have a true "location" either in
   memory or in a register.  This can happen (for example) when a
   constant is passed as an actual argument in a call to an inline
   function.  (It's possible that these things can crop up in other
   ways also.)  Note that one type of constant value which can be
   passed into an inlined function is a constant pointer.  This can
   happen for example if an actual argument in an inlined function
   call evaluates to a compile-time constant address.  */

static void
location_or_const_value_attribute (decl)
     register tree decl;
{
  register rtx rtl;

  if (TREE_CODE (decl) == ERROR_MARK)
    return;

  if ((TREE_CODE (decl) != VAR_DECL) && (TREE_CODE (decl) != PARM_DECL))
    {
      /* Should never happen.  */
      abort ();
      return;
    }

  /* Here we have to decide where we are going to say the parameter "lives"
     (as far as the debugger is concerned).  We only have a couple of choices.
     GCC provides us with DECL_RTL and with DECL_INCOMING_RTL.  DECL_RTL
     normally indicates where the parameter lives during most of the activa-
     tion of the function.  If optimization is enabled however, this could
     be either NULL or else a pseudo-reg.  Both of those cases indicate that
     the parameter doesn't really live anywhere (as far as the code generation
     parts of GCC are concerned) during most of the function's activation.
     That will happen (for example) if the parameter is never referenced
     within the function.

     We could just generate a location descriptor here for all non-NULL
     non-pseudo values of DECL_RTL and ignore all of the rest, but we can
     be a little nicer than that if we also consider DECL_INCOMING_RTL in
     cases where DECL_RTL is NULL or is a pseudo-reg.

     Note however that we can only get away with using DECL_INCOMING_RTL as
     a backup substitute for DECL_RTL in certain limited cases.  In cases
     where DECL_ARG_TYPE(decl) indicates the same type as TREE_TYPE(decl)
     we can be sure that the parameter was passed using the same type as it
     is declared to have within the function, and that its DECL_INCOMING_RTL
     points us to a place where a value of that type is passed.  In cases
     where DECL_ARG_TYPE(decl) and TREE_TYPE(decl) are different types
     however, we cannot (in general) use DECL_INCOMING_RTL as a backup
     substitute for DECL_RTL because in these cases, DECL_INCOMING_RTL
     points us to a value of some type which is *different* from the type
     of the parameter itself.  Thus, if we tried to use DECL_INCOMING_RTL
     to generate a location attribute in such cases, the debugger would
     end up (for example) trying to fetch a `float' from a place which
     actually contains the first part of a `double'.  That would lead to
     really incorrect and confusing output at debug-time, and we don't
     want that now do we?

     So in general, we DO NOT use DECL_INCOMING_RTL as a backup for DECL_RTL
     in cases where DECL_ARG_TYPE(decl) != TREE_TYPE(decl).  There are a
     couple of cute exceptions however.  On little-endian machines we can
     get away with using DECL_INCOMING_RTL even when DECL_ARG_TYPE(decl) is
     not the same as TREE_TYPE(decl) but only when DECL_ARG_TYPE(decl) is
     an integral type which is smaller than TREE_TYPE(decl).  These cases
     arise when (on a little-endian machine) a non-prototyped function has
     a parameter declared to be of type `short' or `char'.  In such cases,
     TREE_TYPE(decl) will be `short' or `char', DECL_ARG_TYPE(decl) will be
     `int', and DECL_INCOMING_RTL will point to the lowest-order byte of the
     passed `int' value.  If the debugger then uses that address to fetch a
     `short' or a `char' (on a little-endian machine) the result will be the
     correct data, so we allow for such exceptional cases below.

     Note that our goal here is to describe the place where the given formal
     parameter lives during most of the function's activation (i.e. between
     the end of the prologue and the start of the epilogue).  We'll do that
     as best as we can.  Note however that if the given formal parameter is
     modified sometime during the execution of the function, then a stack
     backtrace (at debug-time) will show the function as having been called
     with the *new* value rather than the value which was originally passed
     in.  This happens rarely enough that it is not a major problem, but it
     *is* a problem, and I'd like to fix it.  A future version of dwarfout.c
     may generate two additional attributes for any given TAG_formal_parameter
     DIE which will describe the "passed type" and the "passed location" for
     the given formal parameter in addition to the attributes we now generate
     to indicate the "declared type" and the "active location" for each
     parameter.  This additional set of attributes could be used by debuggers
     for stack backtraces.

     Separately, note that sometimes DECL_RTL can be NULL and DECL_INCOMING_RTL
     can be NULL also.  This happens (for example) for inlined-instances of
     inline function formal parameters which are never referenced.  This really
     shouldn't be happening.  All PARM_DECL nodes should get valid non-NULL
     DECL_INCOMING_RTL values, but integrate.c doesn't currently generate
     these values for inlined instances of inline function parameters, so
     when we see such cases, we are just SOL (shit-out-of-luck) for the time
     being (until integrate.c gets fixed).
  */

  /* Use DECL_RTL as the "location" unless we find something better.  */
  rtl = DECL_RTL (decl);

  if (TREE_CODE (decl) == PARM_DECL)
    if (rtl == NULL_RTX || is_pseudo_reg (rtl))
      {
	/* This decl represents a formal parameter which was optimized out.  */
        register tree declared_type = type_main_variant (TREE_TYPE (decl));
        register tree passed_type = type_main_variant (DECL_ARG_TYPE (decl));

	/* Note that DECL_INCOMING_RTL may be NULL in here, but we handle
	   *all* cases where (rtl == NULL_RTX) just below.  */

	if (declared_type == passed_type)
	  rtl = DECL_INCOMING_RTL (decl);
#if (BYTES_BIG_ENDIAN == 0)
	else
	  if (TREE_CODE (declared_type) == INTEGER_TYPE)
	    if (TYPE_SIZE (declared_type) <= TYPE_SIZE (passed_type))
	      rtl = DECL_INCOMING_RTL (decl);
#endif /* (BYTES_BIG_ENDIAN == 0) */
      }

  if (rtl == NULL_RTX)
    return;

  switch (GET_CODE (rtl))
    {
    case CONST_INT:
    case CONST_DOUBLE:
    case CONST_STRING:
    case SYMBOL_REF:
    case LABEL_REF:
    case CONST:
    case PLUS:	/* DECL_RTL could be (plus (reg ...) (const_int ...)) */
      const_value_attribute (rtl);
      break;

    case MEM:
    case REG:
    case SUBREG:
      location_attribute (rtl);
      break;

    default:
      abort ();		/* Should never happen.  */
    }
}

/* Generate an AT_name attribute given some string value to be included as
   the value of the attribute.	*/

inline void
name_attribute (name_string)
     register char *name_string;
{
  if (name_string && *name_string)
    {
      ASM_OUTPUT_DWARF_ATTRIBUTE (asm_out_file, AT_name);
      ASM_OUTPUT_DWARF_STRING (asm_out_file, name_string);
    }
}

inline void
fund_type_attribute (ft_code)
     register unsigned ft_code;
{
  ASM_OUTPUT_DWARF_ATTRIBUTE (asm_out_file, AT_fund_type);
  ASM_OUTPUT_DWARF_FUND_TYPE (asm_out_file, ft_code);
}

static void
mod_fund_type_attribute (type, decl_const, decl_volatile)
     register tree type;
     register int decl_const;
     register int decl_volatile;
{
  char begin_label[MAX_ARTIFICIAL_LABEL_BYTES];
  char end_label[MAX_ARTIFICIAL_LABEL_BYTES];

  ASM_OUTPUT_DWARF_ATTRIBUTE (asm_out_file, AT_mod_fund_type);
  sprintf (begin_label, MT_BEGIN_LABEL_FMT, current_dienum);
  sprintf (end_label, MT_END_LABEL_FMT, current_dienum);
  ASM_OUTPUT_DWARF_DELTA2 (asm_out_file, end_label, begin_label);
  ASM_OUTPUT_LABEL (asm_out_file, begin_label);
  write_modifier_bytes (type, decl_const, decl_volatile);
  ASM_OUTPUT_DWARF_FUND_TYPE (asm_out_file,
			      fundamental_type_code (root_type (type)));
  ASM_OUTPUT_LABEL (asm_out_file, end_label);
}

inline void
user_def_type_attribute (type)
     register tree type;
{
  char ud_type_name[MAX_ARTIFICIAL_LABEL_BYTES];

  ASM_OUTPUT_DWARF_ATTRIBUTE (asm_out_file, AT_user_def_type);
  sprintf (ud_type_name, TYPE_NAME_FMT, TYPE_UID (type));
  ASM_OUTPUT_DWARF_REF (asm_out_file, ud_type_name);
}

static void
mod_u_d_type_attribute (type, decl_const, decl_volatile)
     register tree type;
     register int decl_const;
     register int decl_volatile;
{
  char begin_label[MAX_ARTIFICIAL_LABEL_BYTES];
  char end_label[MAX_ARTIFICIAL_LABEL_BYTES];
  char ud_type_name[MAX_ARTIFICIAL_LABEL_BYTES];

  ASM_OUTPUT_DWARF_ATTRIBUTE (asm_out_file, AT_mod_u_d_type);
  sprintf (begin_label, MT_BEGIN_LABEL_FMT, current_dienum);
  sprintf (end_label, MT_END_LABEL_FMT, current_dienum);
  ASM_OUTPUT_DWARF_DELTA2 (asm_out_file, end_label, begin_label);
  ASM_OUTPUT_LABEL (asm_out_file, begin_label);
  write_modifier_bytes (type, decl_const, decl_volatile);
  sprintf (ud_type_name, TYPE_NAME_FMT, TYPE_UID (root_type (type)));
  ASM_OUTPUT_DWARF_REF (asm_out_file, ud_type_name);
  ASM_OUTPUT_LABEL (asm_out_file, end_label);
}

#ifdef USE_ORDERING_ATTRIBUTE
inline void
ordering_attribute (ordering)
     register unsigned ordering;
{
  ASM_OUTPUT_DWARF_ATTRIBUTE (asm_out_file, AT_ordering);
  ASM_OUTPUT_DWARF_DATA2 (asm_out_file, ordering);
}
#endif /* defined(USE_ORDERING_ATTRIBUTE) */

/* Note that the block of subscript information for an array type also
   includes information about the element type of type given array type.  */

static void
subscript_data_attribute (type)
     register tree type;
{
  register unsigned dimension_number;
  char begin_label[MAX_ARTIFICIAL_LABEL_BYTES];
  char end_label[MAX_ARTIFICIAL_LABEL_BYTES];

  ASM_OUTPUT_DWARF_ATTRIBUTE (asm_out_file, AT_subscr_data);
  sprintf (begin_label, SS_BEGIN_LABEL_FMT, current_dienum);
  sprintf (end_label, SS_END_LABEL_FMT, current_dienum);
  ASM_OUTPUT_DWARF_DELTA2 (asm_out_file, end_label, begin_label);
  ASM_OUTPUT_LABEL (asm_out_file, begin_label);

  /* The GNU compilers represent multidimensional array types as sequences
     of one dimensional array types whose element types are themselves array
     types.  Here we squish that down, so that each multidimensional array
     type gets only one array_type DIE in the Dwarf debugging info.  The
     draft Dwarf specification say that we are allowed to do this kind
     of compression in C (because there is no difference between an
     array or arrays and a multidimensional array in C) but for other
     source languages (e.g. Ada) we probably shouldn't do this.  */

  for (dimension_number = 0;
	TREE_CODE (type) == ARRAY_TYPE;
	type = TREE_TYPE (type), dimension_number++)
    {
      register tree domain = TYPE_DOMAIN (type);

      /* Arrays come in three flavors.	Unspecified bounds, fixed
	 bounds, and (in GNU C only) variable bounds.  Handle all
	 three forms here.  */

      if (domain)
	{
	  /* We have an array type with specified bounds.  */

	  register tree lower = TYPE_MIN_VALUE (domain);
	  register tree upper = TYPE_MAX_VALUE (domain);

	  /* Handle only fundamental types as index types for now.  */

	  if (! type_is_fundamental (domain))
	    abort ();

	  /* Output the representation format byte for this dimension. */

	  ASM_OUTPUT_DWARF_FMT_BYTE (asm_out_file,
				  FMT_CODE (1,
					    TREE_CODE (lower) == INTEGER_CST,
					    TREE_CODE (upper) == INTEGER_CST));

	  /* Output the index type for this dimension.	*/

	  ASM_OUTPUT_DWARF_FUND_TYPE (asm_out_file,
				      fundamental_type_code (domain));

	  /* Output the representation for the lower bound.  */

	  output_bound_representation (lower, dimension_number, 'l');

	  /* Output the representation for the upper bound.  */

	  output_bound_representation (upper, dimension_number, 'u');
	}
      else
	{
	  /* We have an array type with an unspecified length.	For C and
	     C++ we can assume that this really means that (a) the index
	     type is an integral type, and (b) the lower bound is zero.
	     Note that Dwarf defines the representation of an unspecified
	     (upper) bound as being a zero-length location description.	 */

	  /* Output the array-bounds format byte.  */

	  ASM_OUTPUT_DWARF_FMT_BYTE (asm_out_file, FMT_FT_C_X);

	  /* Output the (assumed) index type.  */

	  ASM_OUTPUT_DWARF_FUND_TYPE (asm_out_file, FT_integer);

	  /* Output the (assumed) lower bound (constant) value.	 */

	  ASM_OUTPUT_DWARF_DATA4 (asm_out_file, 0);

	  /* Output the (empty) location description for the upper bound.  */

	  ASM_OUTPUT_DWARF_DATA2 (asm_out_file, 0);
	}
    }

  /* Output the prefix byte that says that the element type is comming up.  */

  ASM_OUTPUT_DWARF_FMT_BYTE (asm_out_file, FMT_ET);

  /* Output a representation of the type of the elements of this array type.  */

  type_attribute (type, 0, 0);

  ASM_OUTPUT_LABEL (asm_out_file, end_label);
}

static void
byte_size_attribute (tree_node)
     register tree tree_node;
{
  register unsigned size;

  ASM_OUTPUT_DWARF_ATTRIBUTE (asm_out_file, AT_byte_size);
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
	/* For a data member of a struct or union, the AT_byte_size is
	   generally given as the number of bytes normally allocated for
	   an object of the *declared* type of the member itself.  This
	   is true even for bit-fields.  */
	size = simple_type_size_in_bits (field_type (tree_node))
	       / BITS_PER_UNIT;
	break;

      default:
	abort ();
    }

  /* Note that `size' might be -1 when we get to this point.  If it
     is, that indicates that the byte size of the entity in question
     is variable.  We have no good way of expressing this fact in Dwarf
     at the present time, so just let the -1 pass on through.  */

  ASM_OUTPUT_DWARF_DATA4 (asm_out_file, size);
}

/* For a FIELD_DECL node which represents a bit-field, output an attribute
   which specifies the distance in bits from the highest order bit of the
   "containing object" for the bit-field to the highest order bit of the
   bit-field itself.

   For any given bit-field, the "containing object" is a hypothetical
   object (of some integral or enum type) within which the given bit-field
   lives.  The type of this hypothetical "containing object" is always the
   same as the declared type of the individual bit-field itself.

   The determination of the exact location of the "containing object" for
   a bit-field is rather complicated.  It's handled by the `field_byte_offset'
   function (above).

   Note that it is the size (in bytes) of the hypothetical "containing
   object" which will be given in the AT_byte_size attribute for this
   bit-field.  (See `byte_size_attribute' above.)
*/

inline void
bit_offset_attribute (decl)
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
  assert (type);				/* Must be a bit field.	 */

  /* We can't yet handle bit-fields whose offsets are variable, so if we
     encounter such things, just return without generating any attribute
     whatsoever.  */

  if (TREE_CODE (bitpos_tree) != INTEGER_CST)
    return;
  bitpos_int = (unsigned) TREE_INT_CST_LOW (bitpos_tree);

  /* Note that the bit offset is always the distance (in bits) from the
     highest-order bit of the "containing object" to the highest-order
     bit of the bit-field itself.  Since the "high-order end" of any
     object or field is different on big-endian and little-endian machines,
     the computation below must take account of these differences.  */

  highest_order_object_bit_offset = object_offset_in_bytes * BITS_PER_UNIT;
  highest_order_field_bit_offset = bitpos_int;

#if (BYTES_BIG_ENDIAN == 0)
  highest_order_field_bit_offset
    += (unsigned) TREE_INT_CST_LOW (DECL_SIZE (decl));

  highest_order_object_bit_offset += simple_type_size_in_bits (type);
#endif /* (BYTES_BIG_ENDIAN == 0) */

  bit_offset =
#if (BYTES_BIG_ENDIAN == 0)
	  highest_order_object_bit_offset - highest_order_field_bit_offset;
#else /* (BYTES_BIG_ENDIAN != 0) */
	  highest_order_field_bit_offset - highest_order_object_bit_offset;
#endif /* (BYTES_BIG_ENDIAN != 0) */

  ASM_OUTPUT_DWARF_ATTRIBUTE (asm_out_file, AT_bit_offset);
  ASM_OUTPUT_DWARF_DATA2 (asm_out_file, bit_offset);
}

/* For a FIELD_DECL node which represents a bit field, output an attribute
   which specifies the length in bits of the given field.  */

inline void
bit_size_attribute (decl)
    register tree decl;
{
  assert (TREE_CODE (decl) == FIELD_DECL);	/* Must be a field.  */
  assert (DECL_BIT_FIELD_TYPE (decl));		/* Must be a bit field.	 */

  ASM_OUTPUT_DWARF_ATTRIBUTE (asm_out_file, AT_bit_size);
  ASM_OUTPUT_DWARF_DATA4 (asm_out_file,
			  (unsigned) TREE_INT_CST_LOW (DECL_SIZE (decl)));
}

/* The following routine outputs the `element_list' attribute for enumeration
   type DIEs.  The element_lits attribute includes the names and values of
   all of the enumeration constants associated with the given enumeration
   type.  */

inline void
element_list_attribute (element)
     register tree element;
{
  char begin_label[MAX_ARTIFICIAL_LABEL_BYTES];
  char end_label[MAX_ARTIFICIAL_LABEL_BYTES];

  ASM_OUTPUT_DWARF_ATTRIBUTE (asm_out_file, AT_element_list);
  sprintf (begin_label, EE_BEGIN_LABEL_FMT, current_dienum);
  sprintf (end_label, EE_END_LABEL_FMT, current_dienum);
  ASM_OUTPUT_DWARF_DELTA4 (asm_out_file, end_label, begin_label);
  ASM_OUTPUT_LABEL (asm_out_file, begin_label);

  /* Here we output a list of value/name pairs for each enumeration constant
     defined for this enumeration type (as required), but we do it in REVERSE
     order.  The order is the one required by the draft #5 Dwarf specification
     published by the UI/PLSIG.  */

  output_enumeral_list (element);   /* Recursively output the whole list.  */

  ASM_OUTPUT_LABEL (asm_out_file, end_label);
}

/* Generate an AT_stmt_list attribute.	These are normally present only in
   DIEs with a TAG_compile_unit tag.  */

inline void
stmt_list_attribute (label)
    register char *label;
{
  ASM_OUTPUT_DWARF_ATTRIBUTE (asm_out_file, AT_stmt_list);
  /* Don't use ASM_OUTPUT_DWARF_DATA4 here.  */
  ASM_OUTPUT_DWARF_ADDR (asm_out_file, label);
}

/* Generate an AT_low_pc attribute for a label DIE, a lexical_block DIE or
   for a subroutine DIE.  */

inline void
low_pc_attribute (asm_low_label)
     register char *asm_low_label;
{
  ASM_OUTPUT_DWARF_ATTRIBUTE (asm_out_file, AT_low_pc);
  ASM_OUTPUT_DWARF_ADDR (asm_out_file, asm_low_label);
}

/* Generate an AT_high_pc attribute for a lexical_block DIE or for a
   subroutine DIE.  */

inline void
high_pc_attribute (asm_high_label)
    register char *asm_high_label;
{
  ASM_OUTPUT_DWARF_ATTRIBUTE (asm_out_file, AT_high_pc);
  ASM_OUTPUT_DWARF_ADDR (asm_out_file, asm_high_label);
}

/* Generate an AT_body_begin attribute for a subroutine DIE.  */

inline void
body_begin_attribute (asm_begin_label)
     register char *asm_begin_label;
{
  ASM_OUTPUT_DWARF_ATTRIBUTE (asm_out_file, AT_body_begin);
  ASM_OUTPUT_DWARF_ADDR (asm_out_file, asm_begin_label);
}

/* Generate an AT_body_end attribute for a subroutine DIE.  */

inline void
body_end_attribute (asm_end_label)
     register char *asm_end_label;
{
  ASM_OUTPUT_DWARF_ATTRIBUTE (asm_out_file, AT_body_end);
  ASM_OUTPUT_DWARF_ADDR (asm_out_file, asm_end_label);
}

/* Generate an AT_language attribute given a LANG value.  These attributes
   are used only within TAG_compile_unit DIEs.  */

inline void
language_attribute (language_code)
     register unsigned language_code;
{
  ASM_OUTPUT_DWARF_ATTRIBUTE (asm_out_file, AT_language);
  ASM_OUTPUT_DWARF_DATA4 (asm_out_file, language_code);
}

inline void
member_attribute (context)
    register tree context;
{
  char label[MAX_ARTIFICIAL_LABEL_BYTES];

  /* Generate this attribute only for members in C++.  */

  if (context != NULL && is_tagged_type (context))
    {
      ASM_OUTPUT_DWARF_ATTRIBUTE (asm_out_file, AT_member);
      sprintf (label, TYPE_NAME_FMT, TYPE_UID (context));
      ASM_OUTPUT_DWARF_REF (asm_out_file, label);
    }
}

inline void
string_length_attribute (upper_bound)
     register tree upper_bound;
{
  char begin_label[MAX_ARTIFICIAL_LABEL_BYTES];
  char end_label[MAX_ARTIFICIAL_LABEL_BYTES];

  ASM_OUTPUT_DWARF_ATTRIBUTE (asm_out_file, AT_string_length);
  sprintf (begin_label, SL_BEGIN_LABEL_FMT, current_dienum);
  sprintf (end_label, SL_END_LABEL_FMT, current_dienum);
  ASM_OUTPUT_DWARF_DELTA2 (asm_out_file, end_label, begin_label);
  ASM_OUTPUT_LABEL (asm_out_file, begin_label);
  output_bound_representation (upper_bound, 0, 'u');
  ASM_OUTPUT_LABEL (asm_out_file, end_label);
}

inline void
comp_dir_attribute (dirname)
     register char *dirname;
{
  ASM_OUTPUT_DWARF_ATTRIBUTE (asm_out_file, AT_comp_dir);
  ASM_OUTPUT_DWARF_STRING (asm_out_file, dirname);
}

inline void
sf_names_attribute (sf_names_start_label)
     register char *sf_names_start_label;
{
  ASM_OUTPUT_DWARF_ATTRIBUTE (asm_out_file, AT_sf_names);
  /* Don't use ASM_OUTPUT_DWARF_DATA4 here.  */
  ASM_OUTPUT_DWARF_ADDR (asm_out_file, sf_names_start_label);
}

inline void
src_info_attribute (src_info_start_label)
     register char *src_info_start_label;
{
  ASM_OUTPUT_DWARF_ATTRIBUTE (asm_out_file, AT_src_info);
  /* Don't use ASM_OUTPUT_DWARF_DATA4 here.  */
  ASM_OUTPUT_DWARF_ADDR (asm_out_file, src_info_start_label);
}

inline void
mac_info_attribute (mac_info_start_label)
     register char *mac_info_start_label;
{
  ASM_OUTPUT_DWARF_ATTRIBUTE (asm_out_file, AT_mac_info);
  /* Don't use ASM_OUTPUT_DWARF_DATA4 here.  */
  ASM_OUTPUT_DWARF_ADDR (asm_out_file, mac_info_start_label);
}

inline void
prototyped_attribute (func_type)
     register tree func_type;
{
  if ((strcmp (language_string, "GNU C") == 0)
      && (TYPE_ARG_TYPES (func_type) != NULL))
    {
      ASM_OUTPUT_DWARF_ATTRIBUTE (asm_out_file, AT_prototyped);
      ASM_OUTPUT_DWARF_STRING (asm_out_file, "");
    }
}

inline void
producer_attribute (producer)
     register char *producer;
{
  ASM_OUTPUT_DWARF_ATTRIBUTE (asm_out_file, AT_producer);
  ASM_OUTPUT_DWARF_STRING (asm_out_file, producer);
}

inline void
inline_attribute (decl)
     register tree decl;
{
  if (DECL_INLINE (decl))
    {
      ASM_OUTPUT_DWARF_ATTRIBUTE (asm_out_file, AT_inline);
      ASM_OUTPUT_DWARF_STRING (asm_out_file, "");
    }
}

inline void
containing_type_attribute (containing_type)
     register tree containing_type;
{
  char label[MAX_ARTIFICIAL_LABEL_BYTES];

  ASM_OUTPUT_DWARF_ATTRIBUTE (asm_out_file, AT_containing_type);
  sprintf (label, TYPE_NAME_FMT, TYPE_UID (containing_type));
  ASM_OUTPUT_DWARF_REF (asm_out_file, label);
}

inline void
abstract_origin_attribute (origin)
     register tree origin;
{
  char label[MAX_ARTIFICIAL_LABEL_BYTES];

  ASM_OUTPUT_DWARF_ATTRIBUTE (asm_out_file, AT_abstract_origin);
  switch (TREE_CODE_CLASS (TREE_CODE (origin)))
    {
    case 'd':
      sprintf (label, DECL_NAME_FMT, DECL_UID (origin));
      break;

    case 't':
      sprintf (label, TYPE_NAME_FMT, TYPE_UID (origin));
      break;

    default:
      abort ();		/* Should never happen.  */

    }
  ASM_OUTPUT_DWARF_REF (asm_out_file, label);
}

#ifdef DWARF_DECL_COORDINATES
inline void
src_coords_attribute (src_fileno, src_lineno)
     register unsigned src_fileno;
     register unsigned src_lineno;
{
  ASM_OUTPUT_DWARF_ATTRIBUTE (asm_out_file, AT_src_coords);
  ASM_OUTPUT_DWARF_DATA2 (asm_out_file, src_fileno);
  ASM_OUTPUT_DWARF_DATA2 (asm_out_file, src_lineno);
}
#endif /* defined(DWARF_DECL_COORDINATES) */

inline void
pure_or_virtual_attribute (func_decl)
     register tree func_decl;
{
  if (DECL_VIRTUAL_P (func_decl))
    {
#if 0 /* DECL_ABSTRACT_VIRTUAL_P is C++-specific.  */
      if (DECL_ABSTRACT_VIRTUAL_P (func_decl))
        ASM_OUTPUT_DWARF_ATTRIBUTE (asm_out_file, AT_pure_virtual);
      else
#endif
        ASM_OUTPUT_DWARF_ATTRIBUTE (asm_out_file, AT_virtual);
      ASM_OUTPUT_DWARF_STRING (asm_out_file, "");
    }
}

/************************* end of attributes *****************************/

/********************* utility routines for DIEs *************************/

/* Output an AT_name attribute and an AT_src_coords attribute for the
   given decl, but only if it actually has a name.  */

static void
name_and_src_coords_attributes (decl)
    register tree decl;
{
  register tree decl_name = DECL_NAME (decl);

  if (decl_name && IDENTIFIER_POINTER (decl_name))
    {
      name_attribute (IDENTIFIER_POINTER (decl_name));
#ifdef DWARF_DECL_COORDINATES
      {
	register unsigned file_index;

	/* This is annoying, but we have to pop out of the .debug section
	   for a moment while we call `lookup_filename' because calling it
	   may cause a temporary switch into the .debug_sfnames section and
	   most svr4 assemblers are not smart enough be be able to nest
	   section switches to any depth greater than one.  Note that we
	   also can't skirt this issue by delaying all output to the
	   .debug_sfnames section unit the end of compilation because that
	   would cause us to have inter-section forward references and
	   Fred Fish sez that m68k/svr4 assemblers botch those.  */

	ASM_OUTPUT_POP_SECTION (asm_out_file);
	file_index = lookup_filename (DECL_SOURCE_FILE (decl));
	ASM_OUTPUT_PUSH_SECTION (asm_out_file, DEBUG_SECTION);

        src_coords_attribute (file_index, DECL_SOURCE_LINE (decl));
      }
#endif /* defined(DWARF_DECL_COORDINATES) */
    }
}

/* Many forms of DIEs contain a "type description" part.  The following
   routine writes out these "type descriptor" parts.  */

static void
type_attribute (type, decl_const, decl_volatile)
     register tree type;
     register int decl_const;
     register int decl_volatile;
{
  register enum tree_code code = TREE_CODE (type);
  register int root_type_modified;

  if (TREE_CODE (type) == ERROR_MARK)
    return;

  /* Handle a special case.  For functions whose return type is void,
     we generate *no* type attribute.  (Note that no object may have
     type `void', so this only applies to function return types.  */

  if (TREE_CODE (type) == VOID_TYPE)
    return;

  root_type_modified = (code == POINTER_TYPE || code == REFERENCE_TYPE
			|| decl_const || decl_volatile
			|| TYPE_READONLY (type) || TYPE_VOLATILE (type));

  if (type_is_fundamental (root_type (type)))
    if (root_type_modified)
	mod_fund_type_attribute (type, decl_const, decl_volatile);
    else
	fund_type_attribute (fundamental_type_code (type));
  else
    if (root_type_modified)
	mod_u_d_type_attribute (type, decl_const, decl_volatile);
    else
	/* We have to get the type_main_variant here (and pass that to the
	   `user_def_type_attribute' routine) because the ..._TYPE node we
	   have might simply be a *copy* of some original type node (where
	   the copy was created to help us keep track of typedef names)
	   and that copy might have a different TYPE_UID from the original
	   ..._TYPE node.  (Note that when `equate_type_number_to_die_number'
	   is labeling a given type DIE for future reference, it always and
	   only creates labels for DIEs representing *main variants*, and it
	   never even knows about non-main-variants.)  */
	user_def_type_attribute (type_main_variant (type));
}

/* Given a tree pointer to a struct, class, union, or enum type node, return
   a pointer to the (string) tag name for the given type, or zero if the
   type was declared without a tag.  */

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
      /* The g++ front end makes the TYPE_NAME of *each* tagged type point
	 to a TYPE_DECL node, regardless of whether or not a `typedef' was
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

      else
	if (TREE_CODE (TYPE_NAME (type)) == TYPE_DECL)
	  t = DECL_NAME (TYPE_NAME (type));
#endif
      /* Now get the name as a string, or invent one.  */
      if (t != 0)
	name = IDENTIFIER_POINTER (t);
    }

  return (name == 0 || *name == '\0') ? 0 : name;
}

inline void
dienum_push ()
{
  /* Start by checking if the pending_sibling_stack needs to be expanded.
     If necessary, expand it.  */

  if (pending_siblings == pending_siblings_allocated)
    {
      pending_siblings_allocated += PENDING_SIBLINGS_INCREMENT;
      pending_sibling_stack
	= (unsigned *) xrealloc (pending_sibling_stack,
				 pending_siblings_allocated * sizeof(unsigned));
    }

  pending_siblings++;
  NEXT_DIE_NUM = next_unused_dienum++;
}

/* Pop the sibling stack so that the most recently pushed DIEnum becomes the
   NEXT_DIE_NUM.  */

inline void
dienum_pop ()
{
  pending_siblings--;
}

inline tree
member_declared_type (member)
     register tree member;
{
  return (DECL_BIT_FIELD_TYPE (member))
	   ? DECL_BIT_FIELD_TYPE (member)
	   : TREE_TYPE (member);
}

/* Get the function's label, as described by its RTL.
   This may be different from the DECL_NAME name used
   in the source file.  */

static char *
function_start_label (decl)
    register tree decl;
{
  rtx x;
  char *fnname;

  x = DECL_RTL (decl);
  if (GET_CODE (x) != MEM)
    abort ();
  x = XEXP (x, 0);
  if (GET_CODE (x) != SYMBOL_REF)
	       abort ();
  fnname = XSTR (x, 0);
  return fnname;
}


/******************************* DIEs ************************************/

/* Output routines for individual types of DIEs.  */

/* Note that every type of DIE (except a null DIE) gets a sibling.  */

static void
output_array_type_die (arg)
     register void *arg;
{
  register tree type = arg;

  ASM_OUTPUT_DWARF_TAG (asm_out_file, TAG_array_type);
  sibling_attribute ();
  equate_type_number_to_die_number (type);
  member_attribute (TYPE_CONTEXT (type));

  /* I believe that we can default the array ordering.  SDB will probably
     do the right things even if AT_ordering is not present.  It's not
     even an issue until we start to get into multidimensional arrays
     anyway.  If SDB is ever caught doing the Wrong Thing for multi-
     dimensional arrays, then we'll have to put the AT_ordering attribute
     back in.  (But if and when we find out that we need to put these in,
     we will only do so for multidimensional arrays.  After all, we don't
     want to waste space in the .debug section now do we?)  */

#ifdef USE_ORDERING_ATTRIBUTE
  ordering_attribute (ORD_row_major);
#endif /* defined(USE_ORDERING_ATTRIBUTE) */

  subscript_data_attribute (type);
}

static void
output_set_type_die (arg)
     register void *arg;
{
  register tree type = arg;

  ASM_OUTPUT_DWARF_TAG (asm_out_file, TAG_set_type);
  sibling_attribute ();
  equate_type_number_to_die_number (type);
  member_attribute (TYPE_CONTEXT (type));
  type_attribute (TREE_TYPE (type), 0, 0);
}

#if 0
/* Implement this when there is a GNU FORTRAN or GNU Ada front end.  */
static void
output_entry_point_die (arg)
     register void *arg;
{
  register tree decl = arg;
  register tree origin = decl_ultimate_origin (decl);

  ASM_OUTPUT_DWARF_TAG (asm_out_file, TAG_entry_point);
  sibling_attribute ();
  dienum_push ();
  if (origin != NULL)
    abstract_origin_attribute (origin);
  else
    {
      name_and_src_coords_attributes (decl);
      member_attribute (DECL_CONTEXT (decl));
      type_attribute (TREE_TYPE (TREE_TYPE (decl)), 0, 0);
    }
  if (DECL_ABSTRACT (decl))
    equate_decl_number_to_die_number (decl);
  else
    low_pc_attribute (function_start_label (decl));
}
#endif

/* Output a DIE to represent an inlined instance of an enumeration type.  */

static void
output_inlined_enumeration_type_die (arg)
     register void *arg;
{
  register tree type = arg;

  ASM_OUTPUT_DWARF_TAG (asm_out_file, TAG_enumeration_type);
  sibling_attribute ();
  assert (TREE_ASM_WRITTEN (type));
  abstract_origin_attribute (type);
}

/* Output a DIE to represent an inlined instance of a structure type.  */

static void
output_inlined_structure_type_die (arg)
     register void *arg;
{
  register tree type = arg;

  ASM_OUTPUT_DWARF_TAG (asm_out_file, TAG_structure_type);
  sibling_attribute ();
  assert (TREE_ASM_WRITTEN (type));
  abstract_origin_attribute (type);
}

/* Output a DIE to represent an inlined instance of a union type.  */

static void
output_inlined_union_type_die (arg)
     register void *arg;
{
  register tree type = arg;

  ASM_OUTPUT_DWARF_TAG (asm_out_file, TAG_union_type);
  sibling_attribute ();
  assert (TREE_ASM_WRITTEN (type));
  abstract_origin_attribute (type);
}

/* Output a DIE to represent an enumeration type.  Note that these DIEs
   include all of the information about the enumeration values also.
   This information is encoded into the element_list attribute.	 */

static void
output_enumeration_type_die (arg)
     register void *arg;
{
  register tree type = arg;

  ASM_OUTPUT_DWARF_TAG (asm_out_file, TAG_enumeration_type);
  sibling_attribute ();
  equate_type_number_to_die_number (type);
  name_attribute (type_tag (type));
  member_attribute (TYPE_CONTEXT (type));

  /* Handle a GNU C/C++ extension, i.e. incomplete enum types.  If the
     given enum type is incomplete, do not generate the AT_byte_size
     attribute or the AT_element_list attribute.  */

  if (TYPE_SIZE (type))
    {
      byte_size_attribute (type);
      element_list_attribute (TYPE_FIELDS (type));
    }
}

/* Output a DIE to represent either a real live formal parameter decl or
   to represent just the type of some formal parameter position in some
   function type.

   Note that this routine is a bit unusual because its argument may be
   a ..._DECL node (i.e. either a PARM_DECL or perhaps a VAR_DECL which
   represents an inlining of some PARM_DECL) or else some sort of a
   ..._TYPE node.  If it's the former then this function is being called
   to output a DIE to represent a formal parameter object (or some inlining
   thereof).  If it's the latter, then this function is only being called
   to output a TAG_formal_parameter DIE to stand as a placeholder for some
   formal argument type of some subprogram type.  */

static void
output_formal_parameter_die (arg)
     register void *arg;
{
  register tree node = arg;

  ASM_OUTPUT_DWARF_TAG (asm_out_file, TAG_formal_parameter);
  sibling_attribute ();

  switch (TREE_CODE_CLASS (TREE_CODE (node)))
    {
    case 'd':	/* We were called with some kind of a ..._DECL node.  */
      {
	register tree origin = decl_ultimate_origin (node);

	if (origin != NULL)
	  abstract_origin_attribute (origin);
	else
	  {
	    name_and_src_coords_attributes (node);
	    type_attribute (TREE_TYPE (node),
			    TREE_READONLY (node), TREE_THIS_VOLATILE (node));
	  }
	if (DECL_ABSTRACT (node))
	  equate_decl_number_to_die_number (node);
	else
	  location_or_const_value_attribute (node);
      }
      break;

    case 't':	/* We were called with some kind of a ..._TYPE node.  */
      type_attribute (node, 0, 0);
      break;

    default:
      abort ();	/* Should never happen.  */
    }
}

/* Output a DIE to represent a declared function (either file-scope
   or block-local) which has "external linkage" (according to ANSI-C).  */

static void
output_global_subroutine_die (arg)
     register void *arg;
{
  register tree decl = arg;
  register tree origin = decl_ultimate_origin (decl);

  ASM_OUTPUT_DWARF_TAG (asm_out_file, TAG_global_subroutine);
  sibling_attribute ();
  dienum_push ();
  if (origin != NULL)
    abstract_origin_attribute (origin);
  else
    {
      register tree type = TREE_TYPE (decl);

      name_and_src_coords_attributes (decl);
      inline_attribute (decl);
      prototyped_attribute (type);
      member_attribute (DECL_CONTEXT (decl));
      type_attribute (TREE_TYPE (type), 0, 0);
      pure_or_virtual_attribute (decl);
    }
  if (DECL_ABSTRACT (decl))
    equate_decl_number_to_die_number (decl);
  else
    {
      if (! DECL_EXTERNAL (decl))
	{
	  char label[MAX_ARTIFICIAL_LABEL_BYTES];

	  low_pc_attribute (function_start_label (decl));
	  sprintf (label, FUNC_END_LABEL_FMT, current_funcdef_number);
	  high_pc_attribute (label);
	  sprintf (label, BODY_BEGIN_LABEL_FMT, current_funcdef_number);
	  body_begin_attribute (label);
	  sprintf (label, BODY_END_LABEL_FMT, current_funcdef_number);
	  body_end_attribute (label);
	}
    }
}

/* Output a DIE to represent a declared data object (either file-scope
   or block-local) which has "external linkage" (according to ANSI-C).  */

static void
output_global_variable_die (arg)
     register void *arg;
{
  register tree decl = arg;
  register tree origin = decl_ultimate_origin (decl);

  ASM_OUTPUT_DWARF_TAG (asm_out_file, TAG_global_variable);
  sibling_attribute ();
  if (origin != NULL)
    abstract_origin_attribute (origin);
  else
    {
      name_and_src_coords_attributes (decl);
      member_attribute (DECL_CONTEXT (decl));
      type_attribute (TREE_TYPE (decl),
		      TREE_READONLY (decl), TREE_THIS_VOLATILE (decl));
    }
  if (DECL_ABSTRACT (decl))
    equate_decl_number_to_die_number (decl);
  else
    {
      if (!DECL_EXTERNAL (decl))
	location_or_const_value_attribute (decl);
    }
}

static void
output_label_die (arg)
     register void *arg;
{
  register tree decl = arg;
  register tree origin = decl_ultimate_origin (decl);

  ASM_OUTPUT_DWARF_TAG (asm_out_file, TAG_label);
  sibling_attribute ();
  if (origin != NULL)
    abstract_origin_attribute (origin);
  else
    name_and_src_coords_attributes (decl);
  if (DECL_ABSTRACT (decl))
    equate_decl_number_to_die_number (decl);
  else
    {
      register rtx insn = DECL_RTL (decl);

      if (GET_CODE (insn) == CODE_LABEL)
	{
	  char label[MAX_ARTIFICIAL_LABEL_BYTES];

	  /* When optimization is enabled (via -O) some parts of the compiler
	     (e.g. jump.c and cse.c) may try to delete CODE_LABEL insns which
	     represent source-level labels which were explicitly declared by
	     the user.  This really shouldn't be happening though, so catch
	     it if it ever does happen.  */

	  if (INSN_DELETED_P (insn))
	    abort ();	/* Should never happen.  */

	  sprintf (label, INSN_LABEL_FMT, current_funcdef_number,
				          (unsigned) INSN_UID (insn));
	  low_pc_attribute (label);
	}
    }
}

static void
output_lexical_block_die (arg)
     register void *arg;
{
  register tree stmt = arg;

  ASM_OUTPUT_DWARF_TAG (asm_out_file, TAG_lexical_block);
  sibling_attribute ();
  dienum_push ();
  if (! BLOCK_ABSTRACT (stmt))
    {
      char begin_label[MAX_ARTIFICIAL_LABEL_BYTES];
      char end_label[MAX_ARTIFICIAL_LABEL_BYTES];

      sprintf (begin_label, BLOCK_BEGIN_LABEL_FMT, next_block_number);
      low_pc_attribute (begin_label);
      sprintf (end_label, BLOCK_END_LABEL_FMT, next_block_number);
      high_pc_attribute (end_label);
    }
}

static void
output_inlined_subroutine_die (arg)
     register void *arg;
{
  register tree stmt = arg;

  ASM_OUTPUT_DWARF_TAG (asm_out_file, TAG_inlined_subroutine);
  sibling_attribute ();
  dienum_push ();
  abstract_origin_attribute (block_ultimate_origin (stmt));
  if (! BLOCK_ABSTRACT (stmt))
    {
      char begin_label[MAX_ARTIFICIAL_LABEL_BYTES];
      char end_label[MAX_ARTIFICIAL_LABEL_BYTES];

      sprintf (begin_label, BLOCK_BEGIN_LABEL_FMT, next_block_number);
      low_pc_attribute (begin_label);
      sprintf (end_label, BLOCK_END_LABEL_FMT, next_block_number);
      high_pc_attribute (end_label);
    }
}

/* Output a DIE to represent a declared data object (either file-scope
   or block-local) which has "internal linkage" (according to ANSI-C).  */

static void
output_local_variable_die (arg)
     register void *arg;
{
  register tree decl = arg;
  register tree origin = decl_ultimate_origin (decl);

  ASM_OUTPUT_DWARF_TAG (asm_out_file, TAG_local_variable);
  sibling_attribute ();
  if (origin != NULL)
    abstract_origin_attribute (origin);
  else
    {
      name_and_src_coords_attributes (decl);
      member_attribute (DECL_CONTEXT (decl));
      type_attribute (TREE_TYPE (decl),
		      TREE_READONLY (decl), TREE_THIS_VOLATILE (decl));
    }
  if (DECL_ABSTRACT (decl))
    equate_decl_number_to_die_number (decl);
  else
    location_or_const_value_attribute (decl);
}

static void
output_member_die (arg)
     register void *arg;
{
  register tree decl = arg;

  ASM_OUTPUT_DWARF_TAG (asm_out_file, TAG_member);
  sibling_attribute ();
  name_and_src_coords_attributes (decl);
  member_attribute (DECL_CONTEXT (decl));
  type_attribute (member_declared_type (decl),
		  TREE_READONLY (decl), TREE_THIS_VOLATILE (decl));
  if (DECL_BIT_FIELD_TYPE (decl))	/* If this is a bit field... */
    {
      byte_size_attribute (decl);
      bit_size_attribute (decl);
      bit_offset_attribute (decl);
    }
  data_member_location_attribute (decl);
}

#if 0
/* Don't generate either pointer_type DIEs or reference_type DIEs.  Use
   modified types instead.

   We keep this code here just in case these types of DIEs may be needed
   to represent certain things in other languages (e.g. Pascal) someday.
*/

static void
output_pointer_type_die (arg)
     register void *arg;
{
  register tree type = arg;

  ASM_OUTPUT_DWARF_TAG (asm_out_file, TAG_pointer_type);
  sibling_attribute ();
  equate_type_number_to_die_number (type);
  member_attribute (TYPE_CONTEXT (type));
  type_attribute (TREE_TYPE (type), 0, 0);
}

static void
output_reference_type_die (arg)
     register void *arg;
{
  register tree type = arg;

  ASM_OUTPUT_DWARF_TAG (asm_out_file, TAG_reference_type);
  sibling_attribute ();
  equate_type_number_to_die_number (type);
  member_attribute (TYPE_CONTEXT (type));
  type_attribute (TREE_TYPE (type), 0, 0);
}
#endif

static void
output_ptr_to_mbr_type_die (arg)
     register void *arg;
{
  register tree type = arg;

  ASM_OUTPUT_DWARF_TAG (asm_out_file, TAG_ptr_to_member_type);
  sibling_attribute ();
  equate_type_number_to_die_number (type);
  member_attribute (TYPE_CONTEXT (type));
  containing_type_attribute (TYPE_OFFSET_BASETYPE (type));
  type_attribute (TREE_TYPE (type), 0, 0);
}

static void
output_compile_unit_die (arg)
     register void *arg;
{
  register char *main_input_filename = arg;

  ASM_OUTPUT_DWARF_TAG (asm_out_file, TAG_compile_unit);
  sibling_attribute ();
  dienum_push ();
  name_attribute (main_input_filename);

  {
    char producer[250];

    sprintf (producer, "%s %s", language_string, version_string);
    producer_attribute (producer);
  }

  if (strcmp (language_string, "GNU C++") == 0)
    language_attribute (LANG_C_PLUS_PLUS);
  else if (strcmp (language_string, "GNU Ada") == 0)
    language_attribute (LANG_ADA83);
  else if (flag_traditional)
    language_attribute (LANG_C);
  else
    language_attribute (LANG_C89);
  low_pc_attribute (TEXT_BEGIN_LABEL);
  high_pc_attribute (TEXT_END_LABEL);
  if (debug_info_level >= DINFO_LEVEL_NORMAL)
    stmt_list_attribute (LINE_BEGIN_LABEL);
  last_filename = xstrdup (main_input_filename);

  {
    char *wd = getpwd ();
    if (wd)
      comp_dir_attribute (wd);
  }

  if (debug_info_level >= DINFO_LEVEL_NORMAL)
    {
      sf_names_attribute (SFNAMES_BEGIN_LABEL);
      src_info_attribute (SRCINFO_BEGIN_LABEL);
      if (debug_info_level >= DINFO_LEVEL_VERBOSE)
        mac_info_attribute (MACINFO_BEGIN_LABEL);
    }
}

static void
output_string_type_die (arg)
     register void *arg;
{
  register tree type = arg;

  ASM_OUTPUT_DWARF_TAG (asm_out_file, TAG_string_type);
  sibling_attribute ();
  member_attribute (TYPE_CONTEXT (type));

  /* Fudge the string length attribute for now.  */

  string_length_attribute (TYPE_MAX_VALUE (TYPE_DOMAIN (type)));
}

static void
output_structure_type_die (arg)
     register void *arg;
{
  register tree type = arg;

  ASM_OUTPUT_DWARF_TAG (asm_out_file, TAG_structure_type);
  sibling_attribute ();
  equate_type_number_to_die_number (type);
  name_attribute (type_tag (type));
  member_attribute (TYPE_CONTEXT (type));

  /* If this type has been completed, then give it a byte_size attribute
     and prepare to give a list of members.  Otherwise, don't do either of
     these things.  In the latter case, we will not be generating a list
     of members (since we don't have any idea what they might be for an
     incomplete type).	*/

  if (TYPE_SIZE (type))
    {
      dienum_push ();
      byte_size_attribute (type);
    }
}

/* Output a DIE to represent a declared function (either file-scope
   or block-local) which has "internal linkage" (according to ANSI-C).  */

static void
output_local_subroutine_die (arg)
     register void *arg;
{
  register tree decl = arg;
  register tree origin = decl_ultimate_origin (decl);

  ASM_OUTPUT_DWARF_TAG (asm_out_file, TAG_subroutine);
  sibling_attribute ();
  dienum_push ();
  if (origin != NULL)
    abstract_origin_attribute (origin);
  else
    {
      register tree type = TREE_TYPE (decl);

      name_and_src_coords_attributes (decl);
      inline_attribute (decl);
      prototyped_attribute (type);
      member_attribute (DECL_CONTEXT (decl));
      type_attribute (TREE_TYPE (type), 0, 0);
      pure_or_virtual_attribute (decl);
    }
  if (DECL_ABSTRACT (decl))
    equate_decl_number_to_die_number (decl);
  else
    {
      /* Avoid getting screwed up in cases where a function was declared
	 static but where no definition was ever given for it.  */

      if (TREE_ASM_WRITTEN (decl))
	{
	  char label[MAX_ARTIFICIAL_LABEL_BYTES];
	  low_pc_attribute (function_start_label (decl));
	  sprintf (label, FUNC_END_LABEL_FMT, current_funcdef_number);
	  high_pc_attribute (label);
	  sprintf (label, BODY_BEGIN_LABEL_FMT, current_funcdef_number);
	  body_begin_attribute (label);
	  sprintf (label, BODY_END_LABEL_FMT, current_funcdef_number);
	  body_end_attribute (label);
	}
    }
}

static void
output_subroutine_type_die (arg)
     register void *arg;
{
  register tree type = arg;
  register tree return_type = TREE_TYPE (type);

  ASM_OUTPUT_DWARF_TAG (asm_out_file, TAG_subroutine_type);
  sibling_attribute ();
  dienum_push ();
  equate_type_number_to_die_number (type);
  prototyped_attribute (type);
  member_attribute (TYPE_CONTEXT (type));
  type_attribute (return_type, 0, 0);
}

static void
output_typedef_die (arg)
     register void *arg;
{
  register tree decl = arg;
  register tree origin = decl_ultimate_origin (decl);

  ASM_OUTPUT_DWARF_TAG (asm_out_file, TAG_typedef);
  sibling_attribute ();
  if (origin != NULL)
    abstract_origin_attribute (origin);
  else
    {
      name_and_src_coords_attributes (decl);
      member_attribute (DECL_CONTEXT (decl));
      type_attribute (TREE_TYPE (decl),
		      TREE_READONLY (decl), TREE_THIS_VOLATILE (decl));
    }
  if (DECL_ABSTRACT (decl))
    equate_decl_number_to_die_number (decl);
}

static void
output_union_type_die (arg)
     register void *arg;
{
  register tree type = arg;

  ASM_OUTPUT_DWARF_TAG (asm_out_file, TAG_union_type);
  sibling_attribute ();
  equate_type_number_to_die_number (type);
  name_attribute (type_tag (type));
  member_attribute (TYPE_CONTEXT (type));

  /* If this type has been completed, then give it a byte_size attribute
     and prepare to give a list of members.  Otherwise, don't do either of
     these things.  In the latter case, we will not be generating a list
     of members (since we don't have any idea what they might be for an
     incomplete type).	*/

  if (TYPE_SIZE (type))
    {
      dienum_push ();
      byte_size_attribute (type);
    }
}

/* Generate a special type of DIE used as a stand-in for a trailing ellipsis
   at the end of an (ANSI prototyped) formal parameters list.  */

static void
output_unspecified_parameters_die (arg)
     register void *arg;
{
  register tree decl_or_type = arg;

  ASM_OUTPUT_DWARF_TAG (asm_out_file, TAG_unspecified_parameters);
  sibling_attribute ();

  /* This kludge is here only for the sake of being compatible with what
     the USL CI5 C compiler does.  The specification of Dwarf Version 1
     doesn't say that TAG_unspecified_parameters DIEs should contain any
     attributes other than the AT_sibling attribute, but they are certainly
     allowed to contain additional attributes, and the CI5 compiler
     generates AT_name, AT_fund_type, and AT_location attributes within
     TAG_unspecified_parameters DIEs which appear in the child lists for
     DIEs representing function definitions, so we do likewise here.  */

  if (TREE_CODE (decl_or_type) == FUNCTION_DECL && DECL_INITIAL (decl_or_type))
    {
      name_attribute ("...");
      fund_type_attribute (FT_pointer);
      /* location_attribute (?); */
    }
}

static void
output_padded_null_die (arg)
     register void *arg;
{
  ASM_OUTPUT_ALIGN (asm_out_file, 2);	/* 2**2 == 4 */
}

/*************************** end of DIEs *********************************/

/* Generate some type of DIE.  This routine generates the generic outer
   wrapper stuff which goes around all types of DIE's (regardless of their
   TAGs.  All forms of DIEs start with a DIE-specific label, followed by a
   DIE-length word, followed by the guts of the DIE itself.  After the guts
   of the DIE, there must always be a terminator label for the DIE.  */

static void
output_die (die_specific_output_function, param)
     register void (*die_specific_output_function)();
     register void *param;
{
  char begin_label[MAX_ARTIFICIAL_LABEL_BYTES];
  char end_label[MAX_ARTIFICIAL_LABEL_BYTES];

  current_dienum = NEXT_DIE_NUM;
  NEXT_DIE_NUM = next_unused_dienum;

  sprintf (begin_label, DIE_BEGIN_LABEL_FMT, current_dienum);
  sprintf (end_label, DIE_END_LABEL_FMT, current_dienum);

  /* Write a label which will act as the name for the start of this DIE.  */

  ASM_OUTPUT_LABEL (asm_out_file, begin_label);

  /* Write the DIE-length word.	 */

  ASM_OUTPUT_DWARF_DELTA4 (asm_out_file, end_label, begin_label);

  /* Fill in the guts of the DIE.  */

  next_unused_dienum++;
  die_specific_output_function (param);

  /* Write a label which will act as the name for the end of this DIE.	*/

  ASM_OUTPUT_LABEL (asm_out_file, end_label);
}

static void
end_sibling_chain ()
{
  char begin_label[MAX_ARTIFICIAL_LABEL_BYTES];

  current_dienum = NEXT_DIE_NUM;
  NEXT_DIE_NUM = next_unused_dienum;

  sprintf (begin_label, DIE_BEGIN_LABEL_FMT, current_dienum);

  /* Write a label which will act as the name for the start of this DIE.  */

  ASM_OUTPUT_LABEL (asm_out_file, begin_label);

  /* Write the DIE-length word.	 */

  ASM_OUTPUT_DWARF_DATA4 (asm_out_file, 4);

  dienum_pop ();
}

/* Generate a list of nameless TAG_formal_parameter DIEs (and perhaps a
   TAG_unspecified_parameters DIE) to represent the types of the formal
   parameters as specified in some function type specification (except
   for those which appear as part of a function *definition*).

   Note that we must be careful here to output all of the parameter DIEs
   *before* we output any DIEs needed to represent the types of the formal
   parameters.  This keeps svr4 SDB happy because it (incorrectly) thinks
   that the first non-parameter DIE it sees ends the formal parameter list.
*/

static void
output_formal_types (function_or_method_type)
     register tree function_or_method_type;
{
  register tree link;
  register tree formal_type = NULL;
  register tree first_parm_type = TYPE_ARG_TYPES (function_or_method_type);

  /* In the case where we are generating a formal types list for a C++
     non-static member function type, skip over the first thing on the
     TYPE_ARG_TYPES list because it only represents the type of the
     hidden `this pointer'.  The debugger should be able to figure
     out (without being explicitly told) that this non-static member
     function type takes a `this pointer' and should be able to figure
     what the type of that hidden parameter is from the AT_member
     attribute of the parent TAG_subroutine_type DIE.  */

  if (TREE_CODE (function_or_method_type) == METHOD_TYPE)
    first_parm_type = TREE_CHAIN (first_parm_type);

  /* Make our first pass over the list of formal parameter types and output
     a TAG_formal_parameter DIE for each one.  */

  for (link = first_parm_type; link; link = TREE_CHAIN (link))
    {
      formal_type = TREE_VALUE (link);
      if (formal_type == void_type_node)
	break;

      /* Output a (nameless) DIE to represent the formal parameter itself.  */

      output_die (output_formal_parameter_die, formal_type);
    }

  /* If this function type has an ellipsis, add a TAG_unspecified_parameters
     DIE to the end of the parameter list.  */

  if (formal_type != void_type_node)
    output_die (output_unspecified_parameters_die, function_or_method_type);

  /* Make our second (and final) pass over the list of formal parameter types
     and output DIEs to represent those types (as necessary).  */

  for (link = TYPE_ARG_TYPES (function_or_method_type);
       link;
       link = TREE_CHAIN (link))
    {
      formal_type = TREE_VALUE (link);
      if (formal_type == void_type_node)
	break;

      output_type (formal_type, function_or_method_type);
    }
}

/* Remember a type in the pending_types_list.  */

static void
pend_type (type)
     register tree type;
{
  if (pending_types == pending_types_allocated)
    {
      pending_types_allocated += PENDING_TYPES_INCREMENT;
      pending_types_list
	= (tree *) xrealloc (pending_types_list,
			     sizeof (tree) * pending_types_allocated);
    }
  pending_types_list[pending_types++] = type;

  /* Mark the pending type as having been output already (even though
     it hasn't been).  This prevents the type from being added to the
     pending_types_list more than once.  */

  TREE_ASM_WRITTEN (type) = 1;
}

/* Return non-zero if it is legitimate to output DIEs to represent a
   given type while we are generating the list of child DIEs for some
   DIE (e.g. a function or lexical block DIE) associated with a given scope.

   See the comments within the function for a description of when it is
   considered legitimate to output DIEs for various kinds of types.

   Note that TYPE_CONTEXT(type) may be NULL (to indicate global scope)
   or it may point to a BLOCK node (for types local to a block), or to a
   FUNCTION_DECL node (for types local to the heading of some function
   definition), or to a FUNCTION_TYPE node (for types local to the
   prototyped parameter list of a function type specification), or to a
   RECORD_TYPE, UNION_TYPE, or QUAL_UNION_TYPE node
   (in the case of C++ nested types).

   The `scope' parameter should likewise be NULL or should point to a
   BLOCK node, a FUNCTION_DECL node, a FUNCTION_TYPE node, a RECORD_TYPE
   node, a UNION_TYPE node, or a QUAL_UNION_TYPE node.

   This function is used only for deciding when to "pend" and when to
   "un-pend" types to/from the pending_types_list.

   Note that we sometimes make use of this "type pending" feature in a
   rather twisted way to temporarily delay the production of DIEs for the
   types of formal parameters.  (We do this just to make svr4 SDB happy.)
   It order to delay the production of DIEs representing types of formal
   parameters, callers of this function supply `fake_containing_scope' as
   the `scope' parameter to this function.  Given that fake_containing_scope
   is a tagged type which is *not* the containing scope for *any* other type,
   the desired effect is achieved, i.e. output of DIEs representing types
   is temporarily suspended, and any type DIEs which would have otherwise
   been output are instead placed onto the pending_types_list.  Later on,
   we force these (temporarily pended) types to be output simply by calling
   `output_pending_types_for_scope' with an actual argument equal to the
   true scope of the types we temporarily pended.
*/

inline int
type_ok_for_scope (type, scope)
    register tree type;
    register tree scope;
{
  /* Tagged types (i.e. struct, union, and enum types) must always be
     output only in the scopes where they actually belong (or else the
     scoping of their own tag names and the scoping of their member
     names will be incorrect).  Non-tagged-types on the other hand can
     generally be output anywhere, except that svr4 SDB really doesn't
     want to see them nested within struct or union types, so here we
     say it is always OK to immediately output any such a (non-tagged)
     type, so long as we are not within such a context.  Note that the
     only kinds of non-tagged types which we will be dealing with here
     (for C and C++ anyway) will be array types and function types.  */

  return is_tagged_type (type)
	 ? (TYPE_CONTEXT (type) == scope)
	 : (scope == NULL_TREE || ! is_tagged_type (scope));
}

/* Output any pending types (from the pending_types list) which we can output
   now (taking into account the scope that we are working on now).

   For each type output, remove the given type from the pending_types_list
   *before* we try to output it.

   Note that we have to process the list in beginning-to-end order,
   because the call made here to output_type may cause yet more types
   to be added to the end of the list, and we may have to output some
   of them too.
*/

static void
output_pending_types_for_scope (containing_scope)
     register tree containing_scope;
{
  register unsigned i;

  for (i = 0; i < pending_types; )
    {
      register tree type = pending_types_list[i];

      if (type_ok_for_scope (type, containing_scope))
	{
	  register tree *mover;
	  register tree *limit;

	  pending_types--;
	  limit = &pending_types_list[pending_types];
	  for (mover = &pending_types_list[i]; mover < limit; mover++)
	    *mover = *(mover+1);

	  /* Un-mark the type as having been output already (because it
	     hasn't been, really).  Then call output_type to generate a
	     Dwarf representation of it.  */

	  TREE_ASM_WRITTEN (type) = 0;
	  output_type (type, containing_scope);

	  /* Don't increment the loop counter in this case because we
	     have shifted all of the subsequent pending types down one
	     element in the pending_types_list array.  */
	}
      else
	i++;
    }
}

static void
output_type (type, containing_scope)
     register tree type;
     register tree containing_scope;
{
  if (type == 0 || type == error_mark_node)
    return;

  /* We are going to output a DIE to represent the unqualified version of
     of this type (i.e. without any const or volatile qualifiers) so get
     the main variant (i.e. the unqualified version) of this type now.  */

  type = type_main_variant (type);

  if (TREE_ASM_WRITTEN (type))
    return;

  /* Don't generate any DIEs for this type now unless it is OK to do so
     (based upon what `type_ok_for_scope' tells us).  */

  if (! type_ok_for_scope (type, containing_scope))
    {
      pend_type (type);
      return;
    }

  switch (TREE_CODE (type))
    {
      case ERROR_MARK:
	break;

      case POINTER_TYPE:
      case REFERENCE_TYPE:
	/* For these types, all that is required is that we output a DIE
	   (or a set of DIEs) to represent the "basis" type.  */
	output_type (TREE_TYPE (type), containing_scope);
	break;

      case OFFSET_TYPE:
	/* This code is used for C++ pointer-to-data-member types.  */
	/* Output a description of the relevant class type.  */
	output_type (TYPE_OFFSET_BASETYPE (type), containing_scope);
	/* Output a description of the type of the object pointed to.  */
	output_type (TREE_TYPE (type), containing_scope);
	/* Now output a DIE to represent this pointer-to-data-member type
	   itself.  */
	output_die (output_ptr_to_mbr_type_die, type);
	break;

      case SET_TYPE:
	output_type (TYPE_DOMAIN (type), containing_scope);
	output_die (output_set_type_die, type);
	break;

      case FILE_TYPE:
	output_type (TREE_TYPE (type), containing_scope);
	abort ();	/* No way to represent these in Dwarf yet!  */
	break;

      case FUNCTION_TYPE:
	/* Force out return type (in case it wasn't forced out already).  */
	output_type (TREE_TYPE (type), containing_scope);
	output_die (output_subroutine_type_die, type);
	output_formal_types (type);
	end_sibling_chain ();
	break;

      case METHOD_TYPE:
	/* Force out return type (in case it wasn't forced out already).  */
	output_type (TREE_TYPE (type), containing_scope);
	output_die (output_subroutine_type_die, type);
	output_formal_types (type);
	end_sibling_chain ();
	break;

      case ARRAY_TYPE:	
	if (TYPE_STRING_FLAG (type) && TREE_CODE(TREE_TYPE(type)) == CHAR_TYPE)
	  {
	    output_type (TREE_TYPE (type), containing_scope);
	    output_die (output_string_type_die, type);
	  }
	else
	  {
	    register tree element_type;

	    element_type = TREE_TYPE (type);
	    while (TREE_CODE (element_type) == ARRAY_TYPE)
	      element_type = TREE_TYPE (element_type);

	    output_type (element_type, containing_scope);
	    output_die (output_array_type_die, type);
	  }
	break;

      case ENUMERAL_TYPE:
      case RECORD_TYPE:
      case UNION_TYPE:
      case QUAL_UNION_TYPE:

	/* For a non-file-scope tagged type, we can always go ahead and
	   output a Dwarf description of this type right now, even if
	   the type in question is still incomplete, because if this
	   local type *was* ever completed anywhere within its scope,
	   that complete definition would already have been attached to
	   this RECORD_TYPE, UNION_TYPE, QUAL_UNION_TYPE or ENUMERAL_TYPE
	   node by the time we reach this point.  That's true because of the
	   way the front-end does its processing of file-scope declarations (of
	   functions and class types) within which other types might be
	   nested.  The C and C++ front-ends always gobble up such "local
	   scope" things en-mass before they try to output *any* debugging
	   information for any of the stuff contained inside them and thus,
	   we get the benefit here of what is (in effect) a pre-resolution
	   of forward references to tagged types in local scopes.

	   Note however that for file-scope tagged types we cannot assume
	   that such pre-resolution of forward references has taken place.
	   A given file-scope tagged type may appear to be incomplete when
	   we reach this point, but it may yet be given a full definition
	   (at file-scope) later on during compilation.  In order to avoid
	   generating a premature (and possibly incorrect) set of Dwarf
	   DIEs for such (as yet incomplete) file-scope tagged types, we
	   generate nothing at all for as-yet incomplete file-scope tagged
	   types here unless we are making our special "finalization" pass
	   for file-scope things at the very end of compilation.  At that
	   time, we will certainly know as much about each file-scope tagged
	   type as we are ever going to know, so at that point in time, we
	   can safely generate correct Dwarf descriptions for these file-
	   scope tagged types.
	*/

	if (TYPE_SIZE (type) == 0 && TYPE_CONTEXT (type) == NULL && !finalizing)
	  return;	/* EARLY EXIT!  Avoid setting TREE_ASM_WRITTEN.  */

	/* Prevent infinite recursion in cases where the type of some
	   member of this type is expressed in terms of this type itself.  */

	TREE_ASM_WRITTEN (type) = 1;

	/* Output a DIE to represent the tagged type itself.  */

	switch (TREE_CODE (type))
	  {
	  case ENUMERAL_TYPE:
	    output_die (output_enumeration_type_die, type);
	    return;  /* a special case -- nothing left to do so just return */

	  case RECORD_TYPE:
	    output_die (output_structure_type_die, type);
	    break;

	  case UNION_TYPE:
	  case QUAL_UNION_TYPE:
	    output_die (output_union_type_die, type);
	    break;

	  default:
	    abort ();	/* Should never happen.  */
	  }

	/* If this is not an incomplete type, output descriptions of
	   each of its members.

	   Note that as we output the DIEs necessary to represent the
	   members of this record or union type, we will also be trying
	   to output DIEs to represent the *types* of those members.
	   However the `output_type' function (above) will specifically
	   avoid generating type DIEs for member types *within* the list
	   of member DIEs for this (containing) type execpt for those
	   types (of members) which are explicitly marked as also being
	   members of this (containing) type themselves.  The g++ front-
	   end can force any given type to be treated as a member of some
	   other (containing) type by setting the TYPE_CONTEXT of the
	   given (member) type to point to the TREE node representing the
	   appropriate (containing) type.
	*/

	if (TYPE_SIZE (type))
	  {
	    {
	      register tree normal_member;

	      /* First output info about the data members and type members.  */

	      for (normal_member = TYPE_FIELDS (type);
		   normal_member;
		   normal_member = TREE_CHAIN (normal_member))
	        output_decl (normal_member, type);
	    }

	    {
	      register tree vec_base;

	      /* Now output info about the function members (if any).  */

	      vec_base = TYPE_METHODS (type);
	      if (vec_base)
		{
		  register tree first_func_member = TREE_VEC_ELT (vec_base, 0);
		  register tree func_member;

		  /* This isn't documented, but the first element of the
		     vector of member functions can be NULL in cases where
		     the class type in question didn't have either a
		     constructor or a destructor declared for it.  We have
		     to make allowances for that here.  */

		  if (first_func_member == NULL)
		    first_func_member = TREE_VEC_ELT (vec_base, 1);

		  for (func_member = first_func_member;
		       func_member;
		       func_member = TREE_CHAIN (func_member))
		    output_decl (func_member, type);
		}
	    }

	    /* RECORD_TYPEs, UNION_TYPEs, and QUAL_UNION_TYPEs are themselves
	       scopes (at least in C++) so we must now output any nested
	       pending types which are local just to this type.  */

	    output_pending_types_for_scope (type);

	    end_sibling_chain ();	/* Terminate member chain.  */
	  }

	break;

      case VOID_TYPE:
      case INTEGER_TYPE:
      case REAL_TYPE:
      case COMPLEX_TYPE:
      case BOOLEAN_TYPE:
      case CHAR_TYPE:
	break;		/* No DIEs needed for fundamental types.  */

      case LANG_TYPE:	/* No Dwarf representation currently defined.  */
	break;

      default:
	abort ();
    }

  TREE_ASM_WRITTEN (type) = 1;
}

static void
output_tagged_type_instantiation (type)
     register tree type;
{
  if (type == 0 || type == error_mark_node)
    return;

  /* We are going to output a DIE to represent the unqualified version of
     of this type (i.e. without any const or volatile qualifiers) so make
     sure that we have the main variant (i.e. the unqualified version) of
     this type now.  */

  assert (type == type_main_variant (type));

  assert (TREE_ASM_WRITTEN (type));

  switch (TREE_CODE (type))
    {
      case ERROR_MARK:
	break;

      case ENUMERAL_TYPE:
	output_die (output_inlined_enumeration_type_die, type);
	break;

      case RECORD_TYPE:
	output_die (output_inlined_structure_type_die, type);
	break;

      case UNION_TYPE:
      case QUAL_UNION_TYPE:
	output_die (output_inlined_union_type_die, type);
	break;

      default:
	abort ();	/* Should never happen.  */
    }
}

/* Output a TAG_lexical_block DIE followed by DIEs to represent all of
   the things which are local to the given block.  */

static void
output_block (stmt)
    register tree stmt;
{
  register int must_output_die = 0;
  register tree origin;
  register enum tree_code origin_code;

  /* Ignore blocks never really used to make RTL.  */

  if (! stmt || ! TREE_USED (stmt))
    return;

  /* Determine the "ultimate origin" of this block.  This block may be an
     inlined instance of an inlined instance of inline function, so we
     have to trace all of the way back through the origin chain to find
     out what sort of node actually served as the original seed for the
     creation of the current block.  */

  origin = block_ultimate_origin (stmt);
  origin_code = (origin != NULL) ? TREE_CODE (origin) : ERROR_MARK;

  /* Determine if we need to output any Dwarf DIEs at all to represent this
     block.  */

  if (origin_code == FUNCTION_DECL)
    /* The outer scopes for inlinings *must* always be represented.  We
       generate TAG_inlined_subroutine DIEs for them.  (See below.)  */
    must_output_die = 1;
  else
    {
      /* In the case where the current block represents an inlining of the
	 "body block" of an inline function, we must *NOT* output any DIE
	 for this block because we have already output a DIE to represent
	 the whole inlined function scope and the "body block" of any
	 function doesn't really represent a different scope according to
	 ANSI C rules.  So we check here to make sure that this block does
	 not represent a "body block inlining" before trying to set the
	 `must_output_die' flag.  */

      if (origin == NULL || ! is_body_block (origin))
	{
	  /* Determine if this block directly contains any "significant"
	     local declarations which we will need to output DIEs for.  */

	  if (debug_info_level > DINFO_LEVEL_TERSE)
	    /* We are not in terse mode so *any* local declaration counts
	       as being a "significant" one.  */
	    must_output_die = (BLOCK_VARS (stmt) != NULL);
	  else
	    {
	      register tree decl;

	      /* We are in terse mode, so only local (nested) function
	         definitions count as "significant" local declarations.  */

	      for (decl = BLOCK_VARS (stmt); decl; decl = TREE_CHAIN (decl))
		if (TREE_CODE (decl) == FUNCTION_DECL && DECL_INITIAL (decl))
		  {
		    must_output_die = 1;
		    break;
		  }
	    }
	}
    }

  /* It would be a waste of space to generate a Dwarf TAG_lexical_block
     DIE for any block which contains no significant local declarations
     at all.  Rather, in such cases we just call `output_decls_for_scope'
     so that any needed Dwarf info for any sub-blocks will get properly
     generated.  Note that in terse mode, our definition of what constitutes
     a "significant" local declaration gets restricted to include only
     inlined function instances and local (nested) function definitions.  */

  if (must_output_die)
    {
      output_die ((origin_code == FUNCTION_DECL)
		    ? output_inlined_subroutine_die
		    : output_lexical_block_die,
		  stmt);
      output_decls_for_scope (stmt);
      end_sibling_chain ();
    }
  else
    output_decls_for_scope (stmt);
}

/* Output all of the decls declared within a given scope (also called
   a `binding contour') and (recursively) all of it's sub-blocks.  */

static void
output_decls_for_scope (stmt)
     register tree stmt;
{
  /* Ignore blocks never really used to make RTL.  */

  if (! stmt || ! TREE_USED (stmt))
    return;

  if (! BLOCK_ABSTRACT (stmt))
    next_block_number++;

  /* Output the DIEs to represent all of the data objects, functions,
     typedefs, and tagged types declared directly within this block
     but not within any nested sub-blocks.  */

  {
    register tree decl;

    for (decl = BLOCK_VARS (stmt); decl; decl = TREE_CHAIN (decl))
      output_decl (decl, stmt);
  }

  output_pending_types_for_scope (stmt);

  /* Output the DIEs to represent all sub-blocks (and the items declared
     therein) of this block.	 */

  {
    register tree subblocks;

    for (subblocks = BLOCK_SUBBLOCKS (stmt);
         subblocks;
         subblocks = BLOCK_CHAIN (subblocks))
      output_block (subblocks);
  }
}

/* Output Dwarf .debug information for a decl described by DECL.  */

static void
output_decl (decl, containing_scope)
     register tree decl;
     register tree containing_scope;
{
  /* Make a note of the decl node we are going to be working on.  We may
     need to give the user the source coordinates of where it appeared in
     case we notice (later on) that something about it looks screwy.  */

  dwarf_last_decl = decl;

  if (TREE_CODE (decl) == ERROR_MARK)
    return;

  /* If this ..._DECL node is marked to be ignored, then ignore it.
     But don't ignore a function definition, since that would screw
     up our count of blocks, and that it turn will completely screw up the
     the labels we will reference in subsequent AT_low_pc and AT_high_pc
     attributes (for subsequent blocks).  */

  if (DECL_IGNORED_P (decl) && TREE_CODE (decl) != FUNCTION_DECL)
    return;

  switch (TREE_CODE (decl))
    {
    case CONST_DECL:
      /* The individual enumerators of an enum type get output when we
	 output the Dwarf representation of the relevant enum type itself.  */
      break;

    case FUNCTION_DECL:
      /* If we are in terse mode, don't output any DIEs to represent
	 mere function declarations.  Also, if we are conforming
	 to the DWARF version 1 specification, don't output DIEs for
	 mere function declarations.  */

      if (DECL_INITIAL (decl) == NULL_TREE)
#if (DWARF_VERSION > 1)
	if (debug_info_level <= DINFO_LEVEL_TERSE)
#endif
	  break;

      /* Before we describe the FUNCTION_DECL itself, make sure that we
	 have described its return type.  */

      output_type (TREE_TYPE (TREE_TYPE (decl)), containing_scope);

      /* If the following DIE will represent a function definition for a
	 function with "extern" linkage, output a special "pubnames" DIE
	 label just ahead of the actual DIE.  A reference to this label
	 was already generated in the .debug_pubnames section sub-entry
	 for this function definition.  */

      if (TREE_PUBLIC (decl))
	{
	  char label[MAX_ARTIFICIAL_LABEL_BYTES];

	  sprintf (label, PUB_DIE_LABEL_FMT, next_pubname_number++);
	  ASM_OUTPUT_LABEL (asm_out_file, label);
	}

      /* Now output a DIE to represent the function itself.  */

      output_die (TREE_PUBLIC (decl) || DECL_EXTERNAL (decl)
				? output_global_subroutine_die
				: output_local_subroutine_die,
		  decl);

      /* Now output descriptions of the arguments for this function.
	 This gets (unnecessarily?) complex because of the fact that
	 the DECL_ARGUMENT list for a FUNCTION_DECL doesn't indicate
	 cases where there was a trailing `...' at the end of the formal
	 parameter list.  In order to find out if there was a trailing
	 ellipsis or not, we must instead look at the type associated
	 with the FUNCTION_DECL.  This will be a node of type FUNCTION_TYPE.
	 If the chain of type nodes hanging off of this FUNCTION_TYPE node
	 ends with a void_type_node then there should *not* be an ellipsis
	 at the end.  */

      /* In the case where we are describing a mere function declaration, all
	 we need to do here (and all we *can* do here) is to describe
	 the *types* of its formal parameters.  */

      if (DECL_INITIAL (decl) == NULL_TREE)
	output_formal_types (TREE_TYPE (decl));
      else
	{
	  /* Generate DIEs to represent all known formal parameters */

	  register tree arg_decls = DECL_ARGUMENTS (decl);
	  register tree parm;

	  /* WARNING!  Kludge zone ahead!  Here we have a special
	     hack for svr4 SDB compatibility.  Instead of passing the
	     current FUNCTION_DECL node as the second parameter (i.e.
	     the `containing_scope' parameter) to `output_decl' (as
	     we ought to) we instead pass a pointer to our own private
	     fake_containing_scope node.  That node is a RECORD_TYPE
	     node which NO OTHER TYPE may ever actually be a member of.

	     This pointer will ultimately get passed into `output_type'
	     as its `containing_scope' parameter.  `Output_type' will
	     then perform its part in the hack... i.e. it will pend
	     the type of the formal parameter onto the pending_types
	     list.  Later on, when we are done generating the whole
	     sequence of formal parameter DIEs for this function
	     definition, we will un-pend all previously pended types
	     of formal parameters for this function definition.

	     This whole kludge prevents any type DIEs from being
	     mixed in with the formal parameter DIEs.  That's good
	     because svr4 SDB believes that the list of formal
	     parameter DIEs for a function ends wherever the first
	     non-formal-parameter DIE appears.  Thus, we have to
	     keep the formal parameter DIEs segregated.  They must
	     all appear (consecutively) at the start of the list of
	     children for the DIE representing the function definition.
	     Then (and only then) may we output any additional DIEs
	     needed to represent the types of these formal parameters.
	  */

	  /*
	     When generating DIEs, generate the unspecified_parameters
	     DIE instead if we come across the arg "__builtin_va_alist"
	  */

	  for (parm = arg_decls; parm; parm = TREE_CHAIN (parm))
	    if (TREE_CODE (parm) == PARM_DECL)
              {
		if (DECL_NAME(parm) &&
		    !strcmp(IDENTIFIER_POINTER(DECL_NAME(parm)),
			    "__builtin_va_alist") )
		  output_die (output_unspecified_parameters_die, decl);
	        else
		  output_decl (parm, fake_containing_scope);
	      }

	  /*
	     Now that we have finished generating all of the DIEs to
	     represent the formal parameters themselves, force out
	     any DIEs needed to represent their types.  We do this
	     simply by un-pending all previously pended types which
	     can legitimately go into the chain of children DIEs for
	     the current FUNCTION_DECL.
	  */

	  output_pending_types_for_scope (decl);

	  /*
	    Decide whether we need a unspecified_parameters DIE at the end.
	    There are 2 more cases to do this for:
	    1) the ansi ... declaration - this is detectable when the end
		of the arg list is not a void_type_node
	    2) an unprototyped function declaration (not a definition).  This
		just means that we have no info about the parameters at all.
	  */

	  {
	    register tree fn_arg_types = TYPE_ARG_TYPES (TREE_TYPE (decl));

	    if (fn_arg_types)
	      {
	      /* this is the prototyped case, check for ... */
	      if (TREE_VALUE (tree_last (fn_arg_types)) != void_type_node)
	        output_die (output_unspecified_parameters_die, decl);
              }
            else
              {
	      /* this is unprotoyped, check for undefined (just declaration) */
              if (!DECL_INITIAL (decl))
                output_die (output_unspecified_parameters_die, decl);
              }
	  }
	}

      /* Output Dwarf info for all of the stuff within the body of the
	 function (if it has one - it may be just a declaration).  */

      {
	register tree outer_scope = DECL_INITIAL (decl);

	if (outer_scope && TREE_CODE (outer_scope) != ERROR_MARK)
	  {
	    /* Note that here, `outer_scope' is a pointer to the outermost
	       BLOCK node created to represent a function.
	       This outermost BLOCK actually represents the outermost
	       binding contour for the function, i.e. the contour in which
	       the function's formal parameters and labels get declared.

	       Curiously, it appears that the front end doesn't actually
	       put the PARM_DECL nodes for the current function onto the
	       BLOCK_VARS list for this outer scope.  (They are strung
	       off of the DECL_ARGUMENTS list for the function instead.)
	       The BLOCK_VARS list for the `outer_scope' does provide us
	       with a list of the LABEL_DECL nodes for the function however,
	       and we output DWARF info for those here.

	       Just within the `outer_scope' there will be another BLOCK
	       node representing the function's outermost pair of curly
	       braces.  We musn't generate a lexical_block DIE for this
	       outermost pair of curly braces because that is not really an
	       independent scope according to ANSI C rules.  Rather, it is
	       the same scope in which the parameters were declared.  */

	    {
	      register tree label;

	      for (label = BLOCK_VARS (outer_scope);
		   label;
		   label = TREE_CHAIN (label))
		output_decl (label, outer_scope);
	    }

	    /* Note here that `BLOCK_SUBBLOCKS (outer_scope)' points to a
	       list of BLOCK nodes which is always only one element long.
	       That one element represents the outermost pair of curley
	       braces for the function body.  */

	    output_decls_for_scope (BLOCK_SUBBLOCKS (outer_scope));

	    /* Finally, force out any pending types which are local to the
	       outermost block of this function definition.  These will
	       all have a TYPE_CONTEXT which points to the FUNCTION_DECL
	       node itself.  */

	    output_pending_types_for_scope (decl);
	  }
      }

      /* Generate a terminator for the list of stuff `owned' by this
	 function.  */

      end_sibling_chain ();

      break;

    case TYPE_DECL:
      /* If we are in terse mode, don't generate any DIEs to represent
	 any actual typedefs.  Note that even when we are in terse mode,
	 we must still output DIEs to represent those tagged types which
	 are used (directly or indirectly) in the specification of either
	 a return type or a formal parameter type of some function.  */

      if (debug_info_level <= DINFO_LEVEL_TERSE)
	if (DECL_NAME (decl) != NULL
	    || ! TYPE_USED_FOR_FUNCTION (TREE_TYPE (decl)))
          return;

      /* In the special case of a null-named TYPE_DECL node (representing
	 the declaration of some type tag), if the given TYPE_DECL is
	 marked as having been instantiated from some other (original)
	 TYPE_DECL node (e.g. one which was generated within the original
	 definition of an inline function) we have to generate a special
	 (abbreviated) TAG_structure_type, TAG_union_type, or
	 TAG_enumeration-type DIE here.  */

      if (! DECL_NAME (decl) && DECL_ABSTRACT_ORIGIN (decl))
	{
	  output_tagged_type_instantiation (TREE_TYPE (decl));
	  return;
	}

      output_type (TREE_TYPE (decl), containing_scope);

      /* Note that unlike the gcc front end (which generates a NULL named
	 TYPE_DECL node for each complete tagged type, each array type,
	 and each function type node created) the g++ front end generates
	 a *named* TYPE_DECL node for each tagged type node created.
	 Unfortunately, these g++ TYPE_DECL nodes cause us to output many
	 superfluous and unnecessary TAG_typedef DIEs here.  When g++ is
	 fixed to stop generating these superfluous named TYPE_DECL nodes,
	 the superfluous TAG_typedef DIEs will likewise cease.  */

      if (DECL_NAME (decl))
	/* Output a DIE to represent the typedef itself.  */
	output_die (output_typedef_die, decl);
      break;

    case LABEL_DECL:
      if (debug_info_level >= DINFO_LEVEL_NORMAL)
	output_die (output_label_die, decl);
      break;

    case VAR_DECL:
      /* If we are conforming to the DWARF version 1 specification, don't
	 generated any DIEs to represent mere external object declarations.  */

#if (DWARF_VERSION <= 1)
      if (DECL_EXTERNAL (decl) && ! TREE_PUBLIC (decl))
	break;
#endif

      /* If we are in terse mode, don't generate any DIEs to represent
	 any variable declarations or definitions.  */

      if (debug_info_level <= DINFO_LEVEL_TERSE)
        break;

      /* Output any DIEs that are needed to specify the type of this data
	 object.  */

      output_type (TREE_TYPE (decl), containing_scope);

      /* If the following DIE will represent a data object definition for a
	 data object with "extern" linkage, output a special "pubnames" DIE
	 label just ahead of the actual DIE.  A reference to this label
	 was already generated in the .debug_pubnames section sub-entry
	 for this data object definition.  */

      if (TREE_PUBLIC (decl) && ! DECL_ABSTRACT (decl))
	{
	  char label[MAX_ARTIFICIAL_LABEL_BYTES];

	  sprintf (label, PUB_DIE_LABEL_FMT, next_pubname_number++);
	  ASM_OUTPUT_LABEL (asm_out_file, label);
	}

      /* Now output the DIE to represent the data object itself.  This gets
	 complicated because of the possibility that the VAR_DECL really
	 represents an inlined instance of a formal parameter for an inline
	 function.  */

      {
        register void (*func) ();
	register tree origin = decl_ultimate_origin (decl);

	if (origin != NULL && TREE_CODE (origin) == PARM_DECL)
	  func = output_formal_parameter_die;
	else
	  {
	    if (TREE_PUBLIC (decl) || DECL_EXTERNAL (decl))
	      func = output_global_variable_die;
	    else
	      func = output_local_variable_die;
	  }
	output_die (func, decl);
      }
      break;

    case FIELD_DECL:
      /* Ignore the nameless fields that are used to skip bits.  */
      if (DECL_NAME (decl) != 0)
	{
	  output_type (member_declared_type (decl), containing_scope);
          output_die (output_member_die, decl);
	}
      break;

    case PARM_DECL:
     /* Force out the type of this formal, if it was not forced out yet.
	Note that here we can run afowl of a bug in "classic" svr4 SDB.
	It should be able to grok the presence of type DIEs within a list
	of TAG_formal_parameter DIEs, but it doesn't.  */

      output_type (TREE_TYPE (decl), containing_scope);
      output_die (output_formal_parameter_die, decl);
      break;

    default:
      abort ();
    }
}

void
dwarfout_file_scope_decl (decl, set_finalizing)
     register tree decl;
     register int set_finalizing;
{
  if (TREE_CODE (decl) == ERROR_MARK)
    return;

  /* If this ..._DECL node is marked to be ignored, then ignore it.  We
     gotta hope that the node in question doesn't represent a function
     definition.  If it does, then totally ignoring it is bound to screw
     up our count of blocks, and that it turn will completely screw up the
     the labels we will reference in subsequent AT_low_pc and AT_high_pc
     attributes (for subsequent blocks).  (It's too bad that BLOCK nodes
     don't carry their own sequence numbers with them!)  */

  if (DECL_IGNORED_P (decl))
    {
      if (TREE_CODE (decl) == FUNCTION_DECL && DECL_INITIAL (decl) != NULL)
	abort ();
      return;
    }

  switch (TREE_CODE (decl))
    {
    case FUNCTION_DECL:

      /* Ignore this FUNCTION_DECL if it refers to a builtin declaration of
	 a builtin function.  Explicit programmer-supplied declarations of
	 these same functions should NOT be ignored however.  */

      if (DECL_EXTERNAL (decl) && DECL_FUNCTION_CODE (decl))
        return;

      /* What we would really like to do here is to filter out all mere
	 file-scope declarations of file-scope functions which are never
	 referenced later within this translation unit (and keep all of
	 ones that *are* referenced later on) but we aren't clarvoiant,
	 so we have no idea which functions will be referenced in the
	 future (i.e. later on within the current translation unit).
	 So here we just ignore all file-scope function declarations
	 which are not also definitions.  If and when the debugger needs
	 to know something about these funcstion, it wil have to hunt
	 around and find the DWARF information associated with the
	 *definition* of the function.

	 Note that we can't just check `DECL_EXTERNAL' to find out which
	 FUNCTION_DECL nodes represent definitions and which ones represent
	 mere declarations.  We have to check `DECL_INITIAL' instead.  That's
	 because the C front-end supports some weird semantics for "extern
	 inline" function definitions.  These can get inlined within the
	 current translation unit (an thus, we need to generate DWARF info
	 for their abstract instances so that the DWARF info for the
	 concrete inlined instances can have something to refer to) but
	 the compiler never generates any out-of-lines instances of such
	 things (despite the fact that they *are* definitions).  The
	 important point is that the C front-end marks these "extern inline"
	 functions as DECL_EXTERNAL, but we need to generate DWARf for them
	 anyway.

	 Note that the C++ front-end also plays some similar games for inline
	 function definitions appearing within include files which also
	 contain `#pragma interface' pragmas.  */

      if (DECL_INITIAL (decl) == NULL_TREE)
	return;

      if (TREE_PUBLIC (decl)
	  && ! DECL_EXTERNAL (decl)
	  && ! DECL_ABSTRACT (decl))
	{
	  char label[MAX_ARTIFICIAL_LABEL_BYTES];

	  /* Output a .debug_pubnames entry for a public function
	     defined in this compilation unit.  */

	  fputc ('\n', asm_out_file);
	  ASM_OUTPUT_PUSH_SECTION (asm_out_file, PUBNAMES_SECTION);
	  sprintf (label, PUB_DIE_LABEL_FMT, next_pubname_number);
	  ASM_OUTPUT_DWARF_ADDR (asm_out_file, label);
	  ASM_OUTPUT_DWARF_STRING (asm_out_file,
				   IDENTIFIER_POINTER (DECL_NAME (decl)));
	  ASM_OUTPUT_POP_SECTION (asm_out_file);
	}

      break;

    case VAR_DECL:

      /* Ignore this VAR_DECL if it refers to a file-scope extern data
	 object declaration and if the declaration was never even
	 referenced from within this entire compilation unit.  We
	 suppress these DIEs in order to save space in the .debug section
	 (by eliminating entries which are probably useless).  Note that
	 we must not suppress block-local extern declarations (whether
	 used or not) because that would screw-up the debugger's name
	 lookup mechanism and cause it to miss things which really ought
	 to be in scope at a given point.  */

      if (DECL_EXTERNAL (decl) && !TREE_USED (decl))
	return;

      if (TREE_PUBLIC (decl)
	  && ! DECL_EXTERNAL (decl)
	  && GET_CODE (DECL_RTL (decl)) == MEM
	  && ! DECL_ABSTRACT (decl))
	{
	  char label[MAX_ARTIFICIAL_LABEL_BYTES];

	  if (debug_info_level >= DINFO_LEVEL_NORMAL)
	    {
	      /* Output a .debug_pubnames entry for a public variable
	         defined in this compilation unit.  */

	      fputc ('\n', asm_out_file);
	      ASM_OUTPUT_PUSH_SECTION (asm_out_file, PUBNAMES_SECTION);
	      sprintf (label, PUB_DIE_LABEL_FMT, next_pubname_number);
	      ASM_OUTPUT_DWARF_ADDR (asm_out_file, label);
	      ASM_OUTPUT_DWARF_STRING (asm_out_file,
				       IDENTIFIER_POINTER (DECL_NAME (decl)));
	      ASM_OUTPUT_POP_SECTION (asm_out_file);
	    }

	  if (DECL_INITIAL (decl) == NULL)
	    {
	      /* Output a .debug_aranges entry for a public variable
		 which is tentatively defined in this compilation unit.  */

	      fputc ('\n', asm_out_file);
	      ASM_OUTPUT_PUSH_SECTION (asm_out_file, ARANGES_SECTION);
	      ASM_OUTPUT_DWARF_ADDR (asm_out_file,
			      IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl)));
	      ASM_OUTPUT_DWARF_DATA4 (asm_out_file, 
			(unsigned) int_size_in_bytes (TREE_TYPE (decl)));
	      ASM_OUTPUT_POP_SECTION (asm_out_file);
	    }
	}

      /* If we are in terse mode, don't generate any DIEs to represent
	 any variable declarations or definitions.  */

      if (debug_info_level <= DINFO_LEVEL_TERSE)
        return;

      break;

    case TYPE_DECL:
      /* Don't bother trying to generate any DIEs to represent any of the
	 normal built-in types for the language we are compiling, except
	 in cases where the types in question are *not* DWARF fundamental
	 types.  We make an exception in the case of non-fundamental types
	 for the sake of objective C (and perhaps C++) because the GNU
	 front-ends for these languages may in fact create certain "built-in"
	 types which are (for example) RECORD_TYPEs.  In such cases, we
	 really need to output these (non-fundamental) types because other
	 DIEs may contain references to them.  */

      if (DECL_SOURCE_LINE (decl) == 0
	  && type_is_fundamental (TREE_TYPE (decl)))
	return;

      /* If we are in terse mode, don't generate any DIEs to represent
	 any actual typedefs.  Note that even when we are in terse mode,
	 we must still output DIEs to represent those tagged types which
	 are used (directly or indirectly) in the specification of either
	 a return type or a formal parameter type of some function.  */

      if (debug_info_level <= DINFO_LEVEL_TERSE)
	if (DECL_NAME (decl) != NULL
	    || ! TYPE_USED_FOR_FUNCTION (TREE_TYPE (decl)))
          return;

      break;

    default:
      return;
    }

  fputc ('\n', asm_out_file);
  ASM_OUTPUT_PUSH_SECTION (asm_out_file, DEBUG_SECTION);
  finalizing = set_finalizing;
  output_decl (decl, NULL_TREE);

  /* NOTE:  The call above to `output_decl' may have caused one or more
     file-scope named types (i.e. tagged types) to be placed onto the
     pending_types_list.  We have to get those types off of that list
     at some point, and this is the perfect time to do it.  If we didn't
     take them off now, they might still be on the list when cc1 finally
     exits.  That might be OK if it weren't for the fact that when we put
     types onto the pending_types_list, we set the TREE_ASM_WRITTEN flag
     for these types, and that causes them never to be output unless
     `output_pending_types_for_scope' takes them off of the list and un-sets
     their TREE_ASM_WRITTEN flags.  */

  output_pending_types_for_scope (NULL_TREE);

  /* The above call should have totally emptied the pending_types_list.  */

  assert (pending_types == 0);

  ASM_OUTPUT_POP_SECTION (asm_out_file);

  if (TREE_CODE (decl) == FUNCTION_DECL && DECL_INITIAL (decl) != NULL)
    current_funcdef_number++;
}

/* Output a marker (i.e. a label) for the beginning of the generated code
   for a lexical block.	 */

void
dwarfout_begin_block (blocknum)
     register unsigned blocknum;
{
  char label[MAX_ARTIFICIAL_LABEL_BYTES];

  text_section ();
  sprintf (label, BLOCK_BEGIN_LABEL_FMT, blocknum);
  ASM_OUTPUT_LABEL (asm_out_file, label);
}

/* Output a marker (i.e. a label) for the end of the generated code
   for a lexical block.	 */

void
dwarfout_end_block (blocknum)
     register unsigned blocknum;
{
  char label[MAX_ARTIFICIAL_LABEL_BYTES];

  text_section ();
  sprintf (label, BLOCK_END_LABEL_FMT, blocknum);
  ASM_OUTPUT_LABEL (asm_out_file, label);
}

/* Output a marker (i.e. a label) at a point in the assembly code which
   corresponds to a given source level label.  */

void
dwarfout_label (insn)
     register rtx insn;
{
  if (debug_info_level >= DINFO_LEVEL_NORMAL)
    {
      char label[MAX_ARTIFICIAL_LABEL_BYTES];

      text_section ();
      sprintf (label, INSN_LABEL_FMT, current_funcdef_number,
				      (unsigned) INSN_UID (insn));
      ASM_OUTPUT_LABEL (asm_out_file, label);
    }
}

/* Output a marker (i.e. a label) for the point in the generated code where
   the real body of the function begins (after parameters have been moved
   to their home locations).  */

void
dwarfout_begin_function ()
{
  char label[MAX_ARTIFICIAL_LABEL_BYTES];

  text_section ();
  sprintf (label, BODY_BEGIN_LABEL_FMT, current_funcdef_number);
  ASM_OUTPUT_LABEL (asm_out_file, label);
}

/* Output a marker (i.e. a label) for the point in the generated code where
   the real body of the function ends (just before the epilogue code).  */

void
dwarfout_end_function ()
{
  char label[MAX_ARTIFICIAL_LABEL_BYTES];

  text_section ();
  sprintf (label, BODY_END_LABEL_FMT, current_funcdef_number);
  ASM_OUTPUT_LABEL (asm_out_file, label);
}

/* Output a marker (i.e. a label) for the absolute end of the generated code
   for a function definition.  This gets called *after* the epilogue code
   has been generated.	*/

void
dwarfout_end_epilogue ()
{
  char label[MAX_ARTIFICIAL_LABEL_BYTES];

  /* Output a label to mark the endpoint of the code generated for this
     function.	*/

  sprintf (label, FUNC_END_LABEL_FMT, current_funcdef_number);
  ASM_OUTPUT_LABEL (asm_out_file, label);
}

static void
shuffle_filename_entry (new_zeroth)
     register filename_entry *new_zeroth;
{
  filename_entry temp_entry;
  register filename_entry *limit_p;
  register filename_entry *move_p;

  if (new_zeroth == &filename_table[0])
    return;

  temp_entry = *new_zeroth;

  /* Shift entries up in the table to make room at [0].  */

  limit_p = &filename_table[0];
  for (move_p = new_zeroth; move_p > limit_p; move_p--)
    *move_p = *(move_p-1);

  /* Install the found entry at [0].  */

  filename_table[0] = temp_entry;
}

/* Create a new (string) entry for the .debug_sfnames section.  */

static void
generate_new_sfname_entry ()
{
  char label[MAX_ARTIFICIAL_LABEL_BYTES];

  fputc ('\n', asm_out_file);
  ASM_OUTPUT_PUSH_SECTION (asm_out_file, SFNAMES_SECTION);
  sprintf (label, SFNAMES_ENTRY_LABEL_FMT, filename_table[0].number);
  ASM_OUTPUT_LABEL (asm_out_file, label);
  ASM_OUTPUT_DWARF_STRING (asm_out_file,
    			   filename_table[0].name
			     ? filename_table[0].name
			     : "");
  ASM_OUTPUT_POP_SECTION (asm_out_file);
}

/* Lookup a filename (in the list of filenames that we know about here in
   dwarfout.c) and return its "index".  The index of each (known) filename
   is just a unique number which is associated with only that one filename.
   We need such numbers for the sake of generating labels (in the
   .debug_sfnames section) and references to those unique labels (in the
   .debug_srcinfo and .debug_macinfo sections).

   If the filename given as an argument is not found in our current list,
   add it to the list and assign it the next available unique index number.

   Whatever we do (i.e. whether we find a pre-existing filename or add a new
   one), we shuffle the filename found (or added) up to the zeroth entry of
   our list of filenames (which is always searched linearly).  We do this so
   as to optimize the most common case for these filename lookups within
   dwarfout.c.  The most common case by far is the case where we call
   lookup_filename to lookup the very same filename that we did a lookup
   on the last time we called lookup_filename.  We make sure that this
   common case is fast because such cases will constitute 99.9% of the
   lookups we ever do (in practice).

   If we add a new filename entry to our table, we go ahead and generate
   the corresponding entry in the .debug_sfnames section right away.
   Doing so allows us to avoid tickling an assembler bug (present in some
   m68k assemblers) which yields assembly-time errors in cases where the
   difference of two label addresses is taken and where the two labels
   are in a section *other* than the one where the difference is being
   calculated, and where at least one of the two symbol references is a
   forward reference.  (This bug could be tickled by our .debug_srcinfo
   entries if we don't output their corresponding .debug_sfnames entries
   before them.)
*/

static unsigned
lookup_filename (file_name)
     char *file_name;
{
  register filename_entry *search_p;
  register filename_entry *limit_p = &filename_table[ft_entries];

  for (search_p = filename_table; search_p < limit_p; search_p++)
    if (!strcmp (file_name, search_p->name))
      {
	/* When we get here, we have found the filename that we were
	   looking for in the filename_table.  Now we want to make sure
	   that it gets moved to the zero'th entry in the table (if it
	   is not already there) so that subsequent attempts to find the
	   same filename will find it as quickly as possible.  */

	shuffle_filename_entry (search_p);
        return filename_table[0].number;
      }

  /* We come here whenever we have a new filename which is not registered
     in the current table.  Here we add it to the table.  */

  /* Prepare to add a new table entry by making sure there is enough space
     in the table to do so.  If not, expand the current table.  */

  if (ft_entries == ft_entries_allocated)
    {
      ft_entries_allocated += FT_ENTRIES_INCREMENT;
      filename_table
	= (filename_entry *)
	  xrealloc (filename_table,
		    ft_entries_allocated * sizeof (filename_entry));
    }

  /* Initially, add the new entry at the end of the filename table.  */

  filename_table[ft_entries].number = ft_entries;
  filename_table[ft_entries].name = xstrdup (file_name);

  /* Shuffle the new entry into filename_table[0].  */

  shuffle_filename_entry (&filename_table[ft_entries]);

  if (debug_info_level >= DINFO_LEVEL_NORMAL)
    generate_new_sfname_entry ();

  ft_entries++;
  return filename_table[0].number;
}

static void
generate_srcinfo_entry (line_entry_num, files_entry_num)
     unsigned line_entry_num;
     unsigned files_entry_num;
{
  char label[MAX_ARTIFICIAL_LABEL_BYTES];

  fputc ('\n', asm_out_file);
  ASM_OUTPUT_PUSH_SECTION (asm_out_file, SRCINFO_SECTION);
  sprintf (label, LINE_ENTRY_LABEL_FMT, line_entry_num);
  ASM_OUTPUT_DWARF_DELTA4 (asm_out_file, label, LINE_BEGIN_LABEL);
  sprintf (label, SFNAMES_ENTRY_LABEL_FMT, files_entry_num);
  ASM_OUTPUT_DWARF_DELTA4 (asm_out_file, label, SFNAMES_BEGIN_LABEL);
  ASM_OUTPUT_POP_SECTION (asm_out_file);
}

void
dwarfout_line (filename, line)
     register char *filename;
     register unsigned line;
{
  if (debug_info_level >= DINFO_LEVEL_NORMAL)
    {
      char label[MAX_ARTIFICIAL_LABEL_BYTES];
      static unsigned last_line_entry_num = 0;
      static unsigned prev_file_entry_num = (unsigned) -1;
      register unsigned this_file_entry_num = lookup_filename (filename);

      text_section ();
      sprintf (label, LINE_CODE_LABEL_FMT, ++last_line_entry_num);
      ASM_OUTPUT_LABEL (asm_out_file, label);

      fputc ('\n', asm_out_file);
      ASM_OUTPUT_PUSH_SECTION (asm_out_file, LINE_SECTION);

      if (this_file_entry_num != prev_file_entry_num)
        {
          char line_entry_label[MAX_ARTIFICIAL_LABEL_BYTES];

          sprintf (line_entry_label, LINE_ENTRY_LABEL_FMT, last_line_entry_num);
          ASM_OUTPUT_LABEL (asm_out_file, line_entry_label);
        }

      {
        register char *tail = rindex (filename, '/');

        if (tail != NULL)
          filename = tail;
      }

      fprintf (asm_out_file, "\t%s\t%u\t%s %s:%u\n",
	       UNALIGNED_INT_ASM_OP, line, ASM_COMMENT_START,
	       filename, line);
      ASM_OUTPUT_DWARF_DATA2 (asm_out_file, 0xffff);
      ASM_OUTPUT_DWARF_DELTA4 (asm_out_file, label, TEXT_BEGIN_LABEL);
      ASM_OUTPUT_POP_SECTION (asm_out_file);

      if (this_file_entry_num != prev_file_entry_num)
        generate_srcinfo_entry (last_line_entry_num, this_file_entry_num);
      prev_file_entry_num = this_file_entry_num;
    }
}

/* Generate an entry in the .debug_macinfo section.  */

static void
generate_macinfo_entry (type_and_offset, string)
     register char *type_and_offset;
     register char *string;
{
  fputc ('\n', asm_out_file);
  ASM_OUTPUT_PUSH_SECTION (asm_out_file, MACINFO_SECTION);
  fprintf (asm_out_file, "\t%s\t%s\n", UNALIGNED_INT_ASM_OP, type_and_offset);
  ASM_OUTPUT_DWARF_STRING (asm_out_file, string);
  ASM_OUTPUT_POP_SECTION (asm_out_file);
}

void
dwarfout_start_new_source_file (filename)
     register char *filename;
{
  char label[MAX_ARTIFICIAL_LABEL_BYTES];
  char type_and_offset[MAX_ARTIFICIAL_LABEL_BYTES*3];

  sprintf (label, SFNAMES_ENTRY_LABEL_FMT, lookup_filename (filename));
  sprintf (type_and_offset, "0x%08x+%s-%s",
	   ((unsigned) MACINFO_start << 24), label, SFNAMES_BEGIN_LABEL);
  generate_macinfo_entry (type_and_offset, "");
}

void
dwarfout_resume_previous_source_file (lineno)
     register unsigned lineno;
{
  char type_and_offset[MAX_ARTIFICIAL_LABEL_BYTES*2];

  sprintf (type_and_offset, "0x%08x+%u",
	   ((unsigned) MACINFO_resume << 24), lineno);
  generate_macinfo_entry (type_and_offset, "");
}

/* Called from check_newline in c-parse.y.  The `buffer' parameter
   contains the tail part of the directive line, i.e. the part which
   is past the initial whitespace, #, whitespace, directive-name,
   whitespace part.  */

void
dwarfout_define (lineno, buffer)
     register unsigned lineno;
     register char *buffer;
{
  static int initialized = 0;
  char type_and_offset[MAX_ARTIFICIAL_LABEL_BYTES*2];

  if (!initialized)
    {
      dwarfout_start_new_source_file (primary_filename);
      initialized = 1;
    }
  sprintf (type_and_offset, "0x%08x+%u",
	   ((unsigned) MACINFO_define << 24), lineno);
  generate_macinfo_entry (type_and_offset, buffer);
}

/* Called from check_newline in c-parse.y.  The `buffer' parameter
   contains the tail part of the directive line, i.e. the part which
   is past the initial whitespace, #, whitespace, directive-name,
   whitespace part.  */

void
dwarfout_undef (lineno, buffer)
     register unsigned lineno;
     register char *buffer;
{
  char type_and_offset[MAX_ARTIFICIAL_LABEL_BYTES*2];

  sprintf (type_and_offset, "0x%08x+%u",
	   ((unsigned) MACINFO_undef << 24), lineno);
  generate_macinfo_entry (type_and_offset, buffer);
}

/* Set up for Dwarf output at the start of compilation.	 */

void
dwarfout_init (asm_out_file, main_input_filename)
     register FILE *asm_out_file;
     register char *main_input_filename;
{
  /* Remember the name of the primary input file.  */

  primary_filename = main_input_filename;

  /* Allocate the initial hunk of the pending_sibling_stack.  */

  pending_sibling_stack
    = (unsigned *)
	xmalloc (PENDING_SIBLINGS_INCREMENT * sizeof (unsigned));
  pending_siblings_allocated = PENDING_SIBLINGS_INCREMENT;
  pending_siblings = 1;

  /* Allocate the initial hunk of the filename_table.  */

  filename_table
    = (filename_entry *)
	xmalloc (FT_ENTRIES_INCREMENT * sizeof (filename_entry));
  ft_entries_allocated = FT_ENTRIES_INCREMENT;
  ft_entries = 0;

  /* Allocate the initial hunk of the pending_types_list.  */

  pending_types_list
    = (tree *) xmalloc (PENDING_TYPES_INCREMENT * sizeof (tree));
  pending_types_allocated = PENDING_TYPES_INCREMENT;
  pending_types = 0;

  /* Create an artificial RECORD_TYPE node which we can use in our hack
     to get the DIEs representing types of formal parameters to come out
     only *after* the DIEs for the formal parameters themselves.  */

  fake_containing_scope = make_node (RECORD_TYPE);

  /* Output a starting label for the .text section.  */

  fputc ('\n', asm_out_file);
  ASM_OUTPUT_PUSH_SECTION (asm_out_file, TEXT_SECTION);
  ASM_OUTPUT_LABEL (asm_out_file, TEXT_BEGIN_LABEL);
  ASM_OUTPUT_POP_SECTION (asm_out_file);

  /* Output a starting label for the .data section.  */

  fputc ('\n', asm_out_file);
  ASM_OUTPUT_PUSH_SECTION (asm_out_file, DATA_SECTION);
  ASM_OUTPUT_LABEL (asm_out_file, DATA_BEGIN_LABEL);
  ASM_OUTPUT_POP_SECTION (asm_out_file);

#if 0 /* GNU C doesn't currently use .data1.  */
  /* Output a starting label for the .data1 section.  */

  fputc ('\n', asm_out_file);
  ASM_OUTPUT_PUSH_SECTION (asm_out_file, DATA1_SECTION);
  ASM_OUTPUT_LABEL (asm_out_file, DATA1_BEGIN_LABEL);
  ASM_OUTPUT_POP_SECTION (asm_out_file);
#endif

  /* Output a starting label for the .rodata section.  */

  fputc ('\n', asm_out_file);
  ASM_OUTPUT_PUSH_SECTION (asm_out_file, RODATA_SECTION);
  ASM_OUTPUT_LABEL (asm_out_file, RODATA_BEGIN_LABEL);
  ASM_OUTPUT_POP_SECTION (asm_out_file);

#if 0 /* GNU C doesn't currently use .rodata1.  */
  /* Output a starting label for the .rodata1 section.  */

  fputc ('\n', asm_out_file);
  ASM_OUTPUT_PUSH_SECTION (asm_out_file, RODATA1_SECTION);
  ASM_OUTPUT_LABEL (asm_out_file, RODATA1_BEGIN_LABEL);
  ASM_OUTPUT_POP_SECTION (asm_out_file);
#endif

  /* Output a starting label for the .bss section.  */

  fputc ('\n', asm_out_file);
  ASM_OUTPUT_PUSH_SECTION (asm_out_file, BSS_SECTION);
  ASM_OUTPUT_LABEL (asm_out_file, BSS_BEGIN_LABEL);
  ASM_OUTPUT_POP_SECTION (asm_out_file);

  if (debug_info_level >= DINFO_LEVEL_NORMAL)
    {
      /* Output a starting label and an initial (compilation directory)
	 entry for the .debug_sfnames section.  The starting label will be
	 referenced by the initial entry in the .debug_srcinfo section.  */
    
      fputc ('\n', asm_out_file);
      ASM_OUTPUT_PUSH_SECTION (asm_out_file, SFNAMES_SECTION);
      ASM_OUTPUT_LABEL (asm_out_file, SFNAMES_BEGIN_LABEL);
      {
	register char *pwd;
	register unsigned len;
	register char *dirname;

	pwd = getpwd ();
	if (!pwd)
	  pfatal_with_name ("getpwd");
	len = strlen (pwd);
	dirname = (char *) xmalloc (len + 2);
    
	strcpy (dirname, pwd);
	strcpy (dirname + len, "/");
        ASM_OUTPUT_DWARF_STRING (asm_out_file, dirname);
        free (dirname);
      }
      ASM_OUTPUT_POP_SECTION (asm_out_file);
    
      if (debug_info_level >= DINFO_LEVEL_VERBOSE)
	{
          /* Output a starting label for the .debug_macinfo section.  This
	     label will be referenced by the AT_mac_info attribute in the
	     TAG_compile_unit DIE.  */
        
          fputc ('\n', asm_out_file);
          ASM_OUTPUT_PUSH_SECTION (asm_out_file, MACINFO_SECTION);
          ASM_OUTPUT_LABEL (asm_out_file, MACINFO_BEGIN_LABEL);
          ASM_OUTPUT_POP_SECTION (asm_out_file);
	}

      /* Generate the initial entry for the .line section.  */
    
      fputc ('\n', asm_out_file);
      ASM_OUTPUT_PUSH_SECTION (asm_out_file, LINE_SECTION);
      ASM_OUTPUT_LABEL (asm_out_file, LINE_BEGIN_LABEL);
      ASM_OUTPUT_DWARF_DELTA4 (asm_out_file, LINE_END_LABEL, LINE_BEGIN_LABEL);
      ASM_OUTPUT_DWARF_ADDR (asm_out_file, TEXT_BEGIN_LABEL);
      ASM_OUTPUT_POP_SECTION (asm_out_file);
    
      /* Generate the initial entry for the .debug_srcinfo section.  */
    
      fputc ('\n', asm_out_file);
      ASM_OUTPUT_PUSH_SECTION (asm_out_file, SRCINFO_SECTION);
      ASM_OUTPUT_LABEL (asm_out_file, SRCINFO_BEGIN_LABEL);
      ASM_OUTPUT_DWARF_ADDR (asm_out_file, LINE_BEGIN_LABEL);
      ASM_OUTPUT_DWARF_ADDR (asm_out_file, SFNAMES_BEGIN_LABEL);
      ASM_OUTPUT_DWARF_ADDR (asm_out_file, TEXT_BEGIN_LABEL);
      ASM_OUTPUT_DWARF_ADDR (asm_out_file, TEXT_END_LABEL);
#ifdef DWARF_TIMESTAMPS
      ASM_OUTPUT_DWARF_DATA4 (asm_out_file, time (NULL));
#else
      ASM_OUTPUT_DWARF_DATA4 (asm_out_file, -1);
#endif
      ASM_OUTPUT_POP_SECTION (asm_out_file);
    
      /* Generate the initial entry for the .debug_pubnames section.  */
    
      fputc ('\n', asm_out_file);
      ASM_OUTPUT_PUSH_SECTION (asm_out_file, PUBNAMES_SECTION);
      ASM_OUTPUT_DWARF_ADDR (asm_out_file, DEBUG_BEGIN_LABEL);
      ASM_OUTPUT_POP_SECTION (asm_out_file);
    
      /* Generate the initial entry for the .debug_aranges section.  */
    
      fputc ('\n', asm_out_file);
      ASM_OUTPUT_PUSH_SECTION (asm_out_file, ARANGES_SECTION);
      ASM_OUTPUT_DWARF_ADDR (asm_out_file, DEBUG_BEGIN_LABEL);
      ASM_OUTPUT_POP_SECTION (asm_out_file);
    }

  /* Setup first DIE number == 1.  */
  NEXT_DIE_NUM = next_unused_dienum++;

  /* Generate the initial DIE for the .debug section.  Note that the
     (string) value given in the AT_name attribute of the TAG_compile_unit
     DIE will (typically) be a relative pathname and that this pathname
     should be taken as being relative to the directory from which the
     compiler was invoked when the given (base) source file was compiled.  */

  fputc ('\n', asm_out_file);
  ASM_OUTPUT_PUSH_SECTION (asm_out_file, DEBUG_SECTION);
  ASM_OUTPUT_LABEL (asm_out_file, DEBUG_BEGIN_LABEL);
  output_die (output_compile_unit_die, main_input_filename);
  ASM_OUTPUT_POP_SECTION (asm_out_file);

  fputc ('\n', asm_out_file);
}

/* Output stuff that dwarf requires at the end of every file.  */

void
dwarfout_finish ()
{
  char label[MAX_ARTIFICIAL_LABEL_BYTES];

  fputc ('\n', asm_out_file);
  ASM_OUTPUT_PUSH_SECTION (asm_out_file, DEBUG_SECTION);

  /* Mark the end of the chain of siblings which represent all file-scope
     declarations in this compilation unit.  */

  /* The (null) DIE which represents the terminator for the (sibling linked)
     list of file-scope items is *special*.  Normally, we would just call
     end_sibling_chain at this point in order to output a word with the
     value `4' and that word would act as the terminator for the list of
     DIEs describing file-scope items.  Unfortunately, if we were to simply
     do that, the label that would follow this DIE in the .debug section
     (i.e. `..D2') would *not* be properly aligned (as it must be on some
     machines) to a 4 byte boundary.

     In order to force the label `..D2' to get aligned to a 4 byte boundary,
     the trick used is to insert extra (otherwise useless) padding bytes
     into the (null) DIE that we know must precede the ..D2 label in the
     .debug section.  The amount of padding required can be anywhere between
     0 and 3 bytes.  The length word at the start of this DIE (i.e. the one
     with the padding) would normally contain the value 4, but now it will
     also have to include the padding bytes, so it will instead have some
     value in the range 4..7.

     Fortunately, the rules of Dwarf say that any DIE whose length word
     contains *any* value less than 8 should be treated as a null DIE, so
     this trick works out nicely.  Clever, eh?  Don't give me any credit
     (or blame).  I didn't think of this scheme.  I just conformed to it.
  */

  output_die (output_padded_null_die, (void *)0);
  dienum_pop ();

  sprintf (label, DIE_BEGIN_LABEL_FMT, NEXT_DIE_NUM);
  ASM_OUTPUT_LABEL (asm_out_file, label);	/* should be ..D2 */
  ASM_OUTPUT_POP_SECTION (asm_out_file);

  /* Output a terminator label for the .text section.  */

  fputc ('\n', asm_out_file);
  ASM_OUTPUT_PUSH_SECTION (asm_out_file, TEXT_SECTION);
  ASM_OUTPUT_LABEL (asm_out_file, TEXT_END_LABEL);
  ASM_OUTPUT_POP_SECTION (asm_out_file);

  /* Output a terminator label for the .data section.  */

  fputc ('\n', asm_out_file);
  ASM_OUTPUT_PUSH_SECTION (asm_out_file, DATA_SECTION);
  ASM_OUTPUT_LABEL (asm_out_file, DATA_END_LABEL);
  ASM_OUTPUT_POP_SECTION (asm_out_file);

#if 0 /* GNU C doesn't currently use .data1.  */
  /* Output a terminator label for the .data1 section.  */

  fputc ('\n', asm_out_file);
  ASM_OUTPUT_PUSH_SECTION (asm_out_file, DATA1_SECTION);
  ASM_OUTPUT_LABEL (asm_out_file, DATA1_END_LABEL);
  ASM_OUTPUT_POP_SECTION (asm_out_file);
#endif

  /* Output a terminator label for the .rodata section.  */

  fputc ('\n', asm_out_file);
  ASM_OUTPUT_PUSH_SECTION (asm_out_file, RODATA_SECTION);
  ASM_OUTPUT_LABEL (asm_out_file, RODATA_END_LABEL);
  ASM_OUTPUT_POP_SECTION (asm_out_file);

#if 0 /* GNU C doesn't currently use .rodata1.  */
  /* Output a terminator label for the .rodata1 section.  */

  fputc ('\n', asm_out_file);
  ASM_OUTPUT_PUSH_SECTION (asm_out_file, RODATA1_SECTION);
  ASM_OUTPUT_LABEL (asm_out_file, RODATA1_END_LABEL);
  ASM_OUTPUT_POP_SECTION (asm_out_file);
#endif

  /* Output a terminator label for the .bss section.  */

  fputc ('\n', asm_out_file);
  ASM_OUTPUT_PUSH_SECTION (asm_out_file, BSS_SECTION);
  ASM_OUTPUT_LABEL (asm_out_file, BSS_END_LABEL);
  ASM_OUTPUT_POP_SECTION (asm_out_file);

  if (debug_info_level >= DINFO_LEVEL_NORMAL)
    {
      /* Output a terminating entry for the .line section.  */
    
      fputc ('\n', asm_out_file);
      ASM_OUTPUT_PUSH_SECTION (asm_out_file, LINE_SECTION);
      ASM_OUTPUT_LABEL (asm_out_file, LINE_LAST_ENTRY_LABEL);
      ASM_OUTPUT_DWARF_DATA4 (asm_out_file, 0);
      ASM_OUTPUT_DWARF_DATA2 (asm_out_file, 0xffff);
      ASM_OUTPUT_DWARF_DELTA4 (asm_out_file, TEXT_END_LABEL, TEXT_BEGIN_LABEL);
      ASM_OUTPUT_LABEL (asm_out_file, LINE_END_LABEL);
      ASM_OUTPUT_POP_SECTION (asm_out_file);
    
      /* Output a terminating entry for the .debug_srcinfo section.  */
    
      fputc ('\n', asm_out_file);
      ASM_OUTPUT_PUSH_SECTION (asm_out_file, SRCINFO_SECTION);
      ASM_OUTPUT_DWARF_DELTA4 (asm_out_file,
			       LINE_LAST_ENTRY_LABEL, LINE_BEGIN_LABEL);
      ASM_OUTPUT_DWARF_DATA4 (asm_out_file, -1);
      ASM_OUTPUT_POP_SECTION (asm_out_file);

      if (debug_info_level >= DINFO_LEVEL_VERBOSE)
	{
	  /* Output terminating entries for the .debug_macinfo section.  */
	
	  dwarfout_resume_previous_source_file (0);

	  fputc ('\n', asm_out_file);
	  ASM_OUTPUT_PUSH_SECTION (asm_out_file, MACINFO_SECTION);
	  ASM_OUTPUT_DWARF_DATA4 (asm_out_file, 0);
	  ASM_OUTPUT_DWARF_STRING (asm_out_file, "");
	  ASM_OUTPUT_POP_SECTION (asm_out_file);
	}
    
      /* Generate the terminating entry for the .debug_pubnames section.  */
    
      fputc ('\n', asm_out_file);
      ASM_OUTPUT_PUSH_SECTION (asm_out_file, PUBNAMES_SECTION);
      ASM_OUTPUT_DWARF_DATA4 (asm_out_file, 0);
      ASM_OUTPUT_DWARF_STRING (asm_out_file, "");
      ASM_OUTPUT_POP_SECTION (asm_out_file);
    
      /* Generate the terminating entries for the .debug_aranges section.

	 Note that we want to do this only *after* we have output the end
	 labels (for the various program sections) which we are going to
	 refer to here.  This allows us to work around a bug in the m68k
	 svr4 assembler.  That assembler gives bogus assembly-time errors
	 if (within any given section) you try to take the difference of
	 two relocatable symbols, both of which are located within some
	 other section, and if one (or both?) of the symbols involved is
	 being forward-referenced.  By generating the .debug_aranges
	 entries at this late point in the assembly output, we skirt the
	 issue simply by avoiding forward-references.
      */
    
      fputc ('\n', asm_out_file);
      ASM_OUTPUT_PUSH_SECTION (asm_out_file, ARANGES_SECTION);

      ASM_OUTPUT_DWARF_ADDR (asm_out_file, TEXT_BEGIN_LABEL);
      ASM_OUTPUT_DWARF_DELTA4 (asm_out_file, TEXT_END_LABEL, TEXT_BEGIN_LABEL);

      ASM_OUTPUT_DWARF_ADDR (asm_out_file, DATA_BEGIN_LABEL);
      ASM_OUTPUT_DWARF_DELTA4 (asm_out_file, DATA_END_LABEL, DATA_BEGIN_LABEL);

#if 0 /* GNU C doesn't currently use .data1.  */
      ASM_OUTPUT_DWARF_ADDR (asm_out_file, DATA1_BEGIN_LABEL);
      ASM_OUTPUT_DWARF_DELTA4 (asm_out_file, DATA1_END_LABEL,
					     DATA1_BEGIN_LABEL);
#endif

      ASM_OUTPUT_DWARF_ADDR (asm_out_file, RODATA_BEGIN_LABEL);
      ASM_OUTPUT_DWARF_DELTA4 (asm_out_file, RODATA_END_LABEL,
					     RODATA_BEGIN_LABEL);

#if 0 /* GNU C doesn't currently use .rodata1.  */
      ASM_OUTPUT_DWARF_ADDR (asm_out_file, RODATA1_BEGIN_LABEL);
      ASM_OUTPUT_DWARF_DELTA4 (asm_out_file, RODATA1_END_LABEL,
					     RODATA1_BEGIN_LABEL);
#endif

      ASM_OUTPUT_DWARF_ADDR (asm_out_file, BSS_BEGIN_LABEL);
      ASM_OUTPUT_DWARF_DELTA4 (asm_out_file, BSS_END_LABEL, BSS_BEGIN_LABEL);

      ASM_OUTPUT_DWARF_DATA4 (asm_out_file, 0);
      ASM_OUTPUT_DWARF_DATA4 (asm_out_file, 0);

      ASM_OUTPUT_POP_SECTION (asm_out_file);
    }
}

#endif /* DWARF_DEBUGGING_INFO */
