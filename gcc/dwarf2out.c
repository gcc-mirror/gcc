/* Output Dwarf2 format symbol table information from the GNU C compiler.
   Copyright (C) 1992, 1993, 1995, 1996, 1997, 1998, 1999, 2000, 2001
   Free Software Foundation, Inc.
   Contributed by Gary Funck (gary@intrepid.com).
   Derived from DWARF 1 implementation of Ron Guilmette (rfg@monkeys.com).
   Extensively modified by Jason Merrill (jason@cygnus.com).

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
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* TODO: Implement .debug_str handling, and share entries somehow.
	 Emit .debug_line header even when there are no functions, since
	   the file numbers are used by .debug_info.  Alternately, leave
	   out locations for types and decls.
	 Avoid talking about ctors and op= for PODs.
	 Factor out common prologue sequences into multiple CIEs.  */

/* The first part of this file deals with the DWARF 2 frame unwind
   information, which is also used by the GCC efficient exception handling
   mechanism.  The second part, controlled only by an #ifdef
   DWARF2_DEBUGGING_INFO, deals with the other DWARF 2 debugging
   information.  */

#include "config.h"
#include "system.h"
#include "tree.h"
#include "flags.h"
#include "rtl.h"
#include "hard-reg-set.h"
#include "regs.h"
#include "insn-config.h"
#include "reload.h"
#include "function.h"
#include "output.h"
#include "expr.h"
#include "libfuncs.h"
#include "except.h"
#include "dwarf2.h"
#include "dwarf2out.h"
#include "dwarf2asm.h"
#include "toplev.h"
#include "varray.h"
#include "ggc.h"
#include "md5.h"
#include "tm_p.h"
#include "diagnostic.h"
#include "debug.h"

#ifdef DWARF2_DEBUGGING_INFO
static void dwarf2out_source_line	PARAMS ((unsigned int, const char *));
#endif

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

/* Decide whether we want to emit frame unwind information for the current
   translation unit.  */

int
dwarf2out_do_frame ()
{
  return (write_symbols == DWARF2_DEBUG
#ifdef DWARF2_FRAME_INFO
	  || DWARF2_FRAME_INFO
#endif
#ifdef DWARF2_UNWIND_INFO
	  || flag_unwind_tables
	  || (flag_exceptions && ! USING_SJLJ_EXCEPTIONS)
#endif
	  );
}

/* The number of the current function definition for which debugging
   information is being generated.  These numbers range from 1 up to the
   maximum number of function definitions contained within the current
   compilation unit.  These numbers are used to create unique label id's
   unique to each function definition.  */
unsigned current_funcdef_number = 0;

#if defined (DWARF2_DEBUGGING_INFO) || defined (DWARF2_UNWIND_INFO)

/* How to start an assembler comment.  */
#ifndef ASM_COMMENT_START
#define ASM_COMMENT_START ";#"
#endif

typedef struct dw_cfi_struct *dw_cfi_ref;
typedef struct dw_fde_struct *dw_fde_ref;
typedef union  dw_cfi_oprnd_struct *dw_cfi_oprnd_ref;

/* Call frames are described using a sequence of Call Frame
   Information instructions.  The register number, offset
   and address fields are provided as possible operands;
   their use is selected by the opcode field.  */

typedef union dw_cfi_oprnd_struct
{
  unsigned long dw_cfi_reg_num;
  long int dw_cfi_offset;
  const char *dw_cfi_addr;
  struct dw_loc_descr_struct *dw_cfi_loc;
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

/* This is how we define the location of the CFA. We use to handle it
   as REG + OFFSET all the time,  but now it can be more complex.
   It can now be either REG + CFA_OFFSET or *(REG + BASE_OFFSET) + CFA_OFFSET.
   Instead of passing around REG and OFFSET, we pass a copy
   of this structure.  */
typedef struct cfa_loc
{
  unsigned long reg;
  long offset;
  long base_offset;
  int indirect;            /* 1 if CFA is accessed via a dereference.  */
} dw_cfa_location;

/* All call frame descriptions (FDE's) in the GCC generated DWARF
   refer to a single Common Information Entry (CIE), defined at
   the beginning of the .debug_frame section.  This used of a single
   CIE obviates the need to keep track of multiple CIE's
   in the DWARF generation routines below.  */

typedef struct dw_fde_struct
{
  const char *dw_fde_begin;
  const char *dw_fde_current_label;
  const char *dw_fde_end;
  dw_cfi_ref dw_fde_cfi;
  unsigned funcdef_number;
  unsigned nothrow : 1;
  unsigned uses_eh_lsda : 1;
}
dw_fde_node;

/* Maximum size (in bytes) of an artificially generated label.   */
#define MAX_ARTIFICIAL_LABEL_BYTES	30

/* The size of the target's pointer type.  */
#ifndef PTR_SIZE
#define PTR_SIZE (POINTER_SIZE / BITS_PER_UNIT)
#endif

/* The size of addresses as they appear in the Dwarf 2 data.
   Some architectures use word addresses to refer to code locations,
   but Dwarf 2 info always uses byte addresses.  On such machines,
   Dwarf 2 addresses need to be larger than the architecture's
   pointers.  */
#ifndef DWARF2_ADDR_SIZE
#define DWARF2_ADDR_SIZE (POINTER_SIZE / BITS_PER_UNIT)
#endif

/* The size in bytes of a DWARF field indicating an offset or length
   relative to a debug info section, specified to be 4 bytes in the
   DWARF-2 specification.  The SGI/MIPS ABI defines it to be the same
   as PTR_SIZE.  */

#ifndef DWARF_OFFSET_SIZE
#define DWARF_OFFSET_SIZE 4
#endif

#define DWARF_VERSION 2

/* Round SIZE up to the nearest BOUNDARY.  */
#define DWARF_ROUND(SIZE,BOUNDARY) \
  ((((SIZE) + (BOUNDARY) - 1) / (BOUNDARY)) * (BOUNDARY))

/* Offsets recorded in opcodes are a multiple of this alignment factor.  */
#ifndef DWARF_CIE_DATA_ALIGNMENT
#ifdef STACK_GROWS_DOWNWARD
#define DWARF_CIE_DATA_ALIGNMENT (-((int) UNITS_PER_WORD))
#else
#define DWARF_CIE_DATA_ALIGNMENT ((int) UNITS_PER_WORD)
#endif
#endif /* not DWARF_CIE_DATA_ALIGNMENT */

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

/* A list of call frame insns for the CIE.  */
static dw_cfi_ref cie_cfi_head;

/* Some DWARF extensions (e.g., MIPS/SGI) implement a subprogram
   attribute that accelerates the lookup of the FDE associated
   with the subprogram.  This variable holds the table index of the FDE
   associated with the current function (body) definition.  */
static unsigned current_funcdef_fde;

/* Forward declarations for functions defined in this file.  */

static char *stripattributes		PARAMS ((const char *));
static const char *dwarf_cfi_name	PARAMS ((unsigned));
static dw_cfi_ref new_cfi		PARAMS ((void));
static void add_cfi			PARAMS ((dw_cfi_ref *, dw_cfi_ref));
static void add_fde_cfi			PARAMS ((const char *, dw_cfi_ref));
static void lookup_cfa_1		PARAMS ((dw_cfi_ref, dw_cfa_location *));
static void lookup_cfa			PARAMS ((dw_cfa_location *));
static void reg_save			PARAMS ((const char *, unsigned,
						 unsigned, long));
static void initial_return_save		PARAMS ((rtx));
static long stack_adjust_offset		PARAMS ((rtx));
static void output_cfi			PARAMS ((dw_cfi_ref, dw_fde_ref, int));
static void output_call_frame_info	PARAMS ((int));
static void dwarf2out_stack_adjust	PARAMS ((rtx));
static void queue_reg_save		PARAMS ((const char *, rtx, long));
static void flush_queued_reg_saves	PARAMS ((void));
static bool clobbers_queued_reg_save	PARAMS ((rtx));
static void dwarf2out_frame_debug_expr	PARAMS ((rtx, const char *));

/* Support for complex CFA locations.  */
static void output_cfa_loc 		PARAMS ((dw_cfi_ref));
static void get_cfa_from_loc_descr 	PARAMS ((dw_cfa_location *,
					        struct dw_loc_descr_struct *));
static struct dw_loc_descr_struct *build_cfa_loc
					PARAMS ((dw_cfa_location *));
static void def_cfa_1		 	PARAMS ((const char *, dw_cfa_location *));

/* How to start an assembler comment.  */
#ifndef ASM_COMMENT_START
#define ASM_COMMENT_START ";#"
#endif

/* Data and reference forms for relocatable data.  */
#define DW_FORM_data (DWARF_OFFSET_SIZE == 8 ? DW_FORM_data8 : DW_FORM_data4)
#define DW_FORM_ref (DWARF_OFFSET_SIZE == 8 ? DW_FORM_ref8 : DW_FORM_ref4)

/* Pseudo-op for defining a new section.  */
#ifndef SECTION_ASM_OP
#define SECTION_ASM_OP	"\t.section\t"
#endif

#ifndef DEBUG_FRAME_SECTION
#define DEBUG_FRAME_SECTION	".debug_frame"
#endif

#ifndef FUNC_BEGIN_LABEL
#define FUNC_BEGIN_LABEL	"LFB"
#endif
#ifndef FUNC_END_LABEL
#define FUNC_END_LABEL		"LFE"
#endif
#define CIE_AFTER_SIZE_LABEL	"LSCIE"
#define CIE_END_LABEL		"LECIE"
#define CIE_LENGTH_LABEL	"LLCIE"
#define FDE_LABEL		"LSFDE"
#define FDE_AFTER_SIZE_LABEL	"LASFDE"
#define FDE_END_LABEL		"LEFDE"
#define FDE_LENGTH_LABEL	"LLFDE"
#define LINE_NUMBER_BEGIN_LABEL	"LSLT"
#define LINE_NUMBER_END_LABEL	"LELT"
#define LN_PROLOG_AS_LABEL	"LASLTP"
#define LN_PROLOG_END_LABEL	"LELTP"
#define DIE_LABEL_PREFIX	"DW"

/* Definitions of defaults for various types of primitive assembly language
   output operations.  These may be overridden from within the tm.h file,
   but typically, that is unnecessary.  */

#ifdef SET_ASM_OP
#ifndef ASM_OUTPUT_DEFINE_LABEL_DIFFERENCE_SYMBOL
#define ASM_OUTPUT_DEFINE_LABEL_DIFFERENCE_SYMBOL(FILE, SY, HI, LO)    	\
 do {									\
  fprintf (FILE, "%s", SET_ASM_OP);					\
  assemble_name (FILE, SY);						\
  fputc (',', FILE);							\
  assemble_name (FILE, HI);						\
  fputc ('-', FILE);							\
  assemble_name (FILE, LO);						\
 } while (0)
#endif
#endif /* SET_ASM_OP */

/* The DWARF 2 CFA column which tracks the return address.  Normally this
   is the column for PC, or the first column after all of the hard
   registers.  */
#ifndef DWARF_FRAME_RETURN_COLUMN
#ifdef PC_REGNUM
#define DWARF_FRAME_RETURN_COLUMN 	DWARF_FRAME_REGNUM (PC_REGNUM)
#else
#define DWARF_FRAME_RETURN_COLUMN 	DWARF_FRAME_REGISTERS
#endif
#endif

/* The mapping from gcc register number to DWARF 2 CFA column number.  By
   default, we just provide columns for all registers.  */
#ifndef DWARF_FRAME_REGNUM
#define DWARF_FRAME_REGNUM(REG) DBX_REGISTER_NUMBER (REG)
#endif

/* Hook used by __throw.  */

rtx
expand_builtin_dwarf_fp_regnum ()
{
  return GEN_INT (DWARF_FRAME_REGNUM (HARD_FRAME_POINTER_REGNUM));
}

/* The offset from the incoming value of %sp to the top of the stack frame
   for the current function.  */
#ifndef INCOMING_FRAME_SP_OFFSET
#define INCOMING_FRAME_SP_OFFSET 0
#endif

/* Return a pointer to a copy of the section string name S with all
   attributes stripped off, and an asterisk prepended (for assemble_name).  */

static inline char *
stripattributes (s)
     const char *s;
{
  char *stripped = xmalloc (strlen (s) + 2);
  char *p = stripped;

  *p++ = '*';

  while (*s && *s != ',')
    *p++ = *s++;

  *p = '\0';
  return stripped;
}

/* Generate code to initialize the register size table.  */

void
expand_builtin_init_dwarf_reg_sizes (address)
     tree address;
{
  int i;
  enum machine_mode mode = TYPE_MODE (char_type_node);
  rtx addr = expand_expr (address, NULL_RTX, VOIDmode, 0);
  rtx mem = gen_rtx_MEM (mode, addr);

  for (i = 0; i < DWARF_FRAME_REGISTERS; ++i)
    {
      int offset = DWARF_FRAME_REGNUM (i) * GET_MODE_SIZE (mode);
      int size = GET_MODE_SIZE (reg_raw_mode[i]);

      if (offset < 0)
	continue;

      emit_move_insn (adjust_address (mem, mode, offset), GEN_INT (size));
    }
}

/* Convert a DWARF call frame info. operation to its string name */

static const char *
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
    case DW_CFA_def_cfa_expression:
      return "DW_CFA_def_cfa_expression";

    /* SGI/MIPS specific */
    case DW_CFA_MIPS_advance_loc8:
      return "DW_CFA_MIPS_advance_loc8";

    /* GNU extensions */
    case DW_CFA_GNU_window_save:
      return "DW_CFA_GNU_window_save";
    case DW_CFA_GNU_args_size:
      return "DW_CFA_GNU_args_size";
    case DW_CFA_GNU_negative_offset_extended:
      return "DW_CFA_GNU_negative_offset_extended";

    default:
      return "DW_CFA_<unknown>";
    }
}

/* Return a pointer to a newly allocated Call Frame Instruction.  */

static inline dw_cfi_ref
new_cfi ()
{
  register dw_cfi_ref cfi = (dw_cfi_ref) xmalloc (sizeof (dw_cfi_node));

  cfi->dw_cfi_next = NULL;
  cfi->dw_cfi_oprnd1.dw_cfi_reg_num = 0;
  cfi->dw_cfi_oprnd2.dw_cfi_reg_num = 0;

  return cfi;
}

/* Add a Call Frame Instruction to list of instructions.  */

static inline void
add_cfi (list_head, cfi)
     register dw_cfi_ref *list_head;
     register dw_cfi_ref cfi;
{
  register dw_cfi_ref *p;

  /* Find the end of the chain.  */
  for (p = list_head; (*p) != NULL; p = &(*p)->dw_cfi_next)
    ;

  *p = cfi;
}

/* Generate a new label for the CFI info to refer to.  */

char *
dwarf2out_cfi_label ()
{
  static char label[20];
  static unsigned long label_num = 0;

  ASM_GENERATE_INTERNAL_LABEL (label, "LCFI", label_num++);
  ASM_OUTPUT_LABEL (asm_out_file, label);

  return label;
}

/* Add CFI to the current fde at the PC value indicated by LABEL if specified,
   or to the CIE if LABEL is NULL.  */

static void
add_fde_cfi (label, cfi)
     register const char *label;
     register dw_cfi_ref cfi;
{
  if (label)
    {
      register dw_fde_ref fde = &fde_table[fde_table_in_use - 1];

      if (*label == 0)
	label = dwarf2out_cfi_label ();

      if (fde->dw_fde_current_label == NULL
	  || strcmp (label, fde->dw_fde_current_label) != 0)
	{
	  register dw_cfi_ref xcfi;

	  fde->dw_fde_current_label = label = xstrdup (label);

	  /* Set the location counter to the new label.  */
	  xcfi = new_cfi ();
	  xcfi->dw_cfi_opc = DW_CFA_advance_loc4;
	  xcfi->dw_cfi_oprnd1.dw_cfi_addr = label;
	  add_cfi (&fde->dw_fde_cfi, xcfi);
	}

      add_cfi (&fde->dw_fde_cfi, cfi);
    }

  else
    add_cfi (&cie_cfi_head, cfi);
}

/* Subroutine of lookup_cfa.  */

static inline void
lookup_cfa_1 (cfi, loc)
     register dw_cfi_ref cfi;
     register dw_cfa_location *loc;
{
  switch (cfi->dw_cfi_opc)
    {
    case DW_CFA_def_cfa_offset:
      loc->offset = cfi->dw_cfi_oprnd1.dw_cfi_offset;
      break;
    case DW_CFA_def_cfa_register:
      loc->reg = cfi->dw_cfi_oprnd1.dw_cfi_reg_num;
      break;
    case DW_CFA_def_cfa:
      loc->reg = cfi->dw_cfi_oprnd1.dw_cfi_reg_num;
      loc->offset = cfi->dw_cfi_oprnd2.dw_cfi_offset;
      break;
    case DW_CFA_def_cfa_expression:
      get_cfa_from_loc_descr (loc, cfi->dw_cfi_oprnd1.dw_cfi_loc);
      break;
    default:
      break;
    }
}

/* Find the previous value for the CFA.  */

static void
lookup_cfa (loc)
     register dw_cfa_location *loc;
{
  register dw_cfi_ref cfi;

  loc->reg = (unsigned long) -1;
  loc->offset = 0;
  loc->indirect = 0;
  loc->base_offset = 0;

  for (cfi = cie_cfi_head; cfi; cfi = cfi->dw_cfi_next)
    lookup_cfa_1 (cfi, loc);

  if (fde_table_in_use)
    {
      register dw_fde_ref fde = &fde_table[fde_table_in_use - 1];
      for (cfi = fde->dw_fde_cfi; cfi; cfi = cfi->dw_cfi_next)
	lookup_cfa_1 (cfi, loc);
    }
}

/* The current rule for calculating the DWARF2 canonical frame address.  */
static dw_cfa_location cfa;

/* The register used for saving registers to the stack, and its offset
   from the CFA.  */
static dw_cfa_location cfa_store;

/* The running total of the size of arguments pushed onto the stack.  */
static long args_size;

/* The last args_size we actually output.  */
static long old_args_size;

/* Entry point to update the canonical frame address (CFA).
   LABEL is passed to add_fde_cfi.  The value of CFA is now to be
   calculated from REG+OFFSET.  */

void
dwarf2out_def_cfa (label, reg, offset)
     register const char *label;
     unsigned reg;
     long offset;
{
  dw_cfa_location loc;
  loc.indirect = 0;
  loc.base_offset = 0;
  loc.reg = reg;
  loc.offset = offset;
  def_cfa_1 (label, &loc);
}

/* This routine does the actual work.  The CFA is now calculated from
   the dw_cfa_location structure.  */
static void
def_cfa_1 (label, loc_p)
     register const char *label;
     dw_cfa_location *loc_p;
{
  register dw_cfi_ref cfi;
  dw_cfa_location old_cfa, loc;

  cfa = *loc_p;
  loc = *loc_p;

  if (cfa_store.reg == loc.reg && loc.indirect == 0)
    cfa_store.offset = loc.offset;

  loc.reg = DWARF_FRAME_REGNUM (loc.reg);
  lookup_cfa (&old_cfa);

  if (loc.reg == old_cfa.reg && loc.offset == old_cfa.offset &&
      loc.indirect == old_cfa.indirect)
    {
      if (loc.indirect == 0
	  || loc.base_offset == old_cfa.base_offset)
	/* Nothing changed so no need to issue any call frame
           instructions.  */
	return;
    }

  cfi = new_cfi ();

  if (loc.reg == old_cfa.reg && !loc.indirect)
    {
      /* Construct a "DW_CFA_def_cfa_offset <offset>" instruction,
	 indicating the CFA register did not change but the offset
	 did.  */
      cfi->dw_cfi_opc = DW_CFA_def_cfa_offset;
      cfi->dw_cfi_oprnd1.dw_cfi_offset = loc.offset;
    }

#ifndef MIPS_DEBUGGING_INFO  /* SGI dbx thinks this means no offset.  */
  else if (loc.offset == old_cfa.offset && old_cfa.reg != (unsigned long) -1
	   && !loc.indirect)
    {
      /* Construct a "DW_CFA_def_cfa_register <register>" instruction,
	 indicating the CFA register has changed to <register> but the
	 offset has not changed.  */
      cfi->dw_cfi_opc = DW_CFA_def_cfa_register;
      cfi->dw_cfi_oprnd1.dw_cfi_reg_num = loc.reg;
    }
#endif

  else if (loc.indirect == 0)
    {
      /* Construct a "DW_CFA_def_cfa <register> <offset>" instruction,
	 indicating the CFA register has changed to <register> with
	 the specified offset.  */
      cfi->dw_cfi_opc = DW_CFA_def_cfa;
      cfi->dw_cfi_oprnd1.dw_cfi_reg_num = loc.reg;
      cfi->dw_cfi_oprnd2.dw_cfi_offset = loc.offset;
    }
  else
    {
      /* Construct a DW_CFA_def_cfa_expression instruction to
	 calculate the CFA using a full location expression since no
	 register-offset pair is available.  */
      struct dw_loc_descr_struct *loc_list;
      cfi->dw_cfi_opc = DW_CFA_def_cfa_expression;
      loc_list = build_cfa_loc (&loc);
      cfi->dw_cfi_oprnd1.dw_cfi_loc = loc_list;
    }

  add_fde_cfi (label, cfi);
}

/* Add the CFI for saving a register.  REG is the CFA column number.
   LABEL is passed to add_fde_cfi.
   If SREG is -1, the register is saved at OFFSET from the CFA;
   otherwise it is saved in SREG.  */

static void
reg_save (label, reg, sreg, offset)
     register const char *label;
     register unsigned reg;
     register unsigned sreg;
     register long offset;
{
  register dw_cfi_ref cfi = new_cfi ();

  cfi->dw_cfi_oprnd1.dw_cfi_reg_num = reg;

  /* The following comparison is correct. -1 is used to indicate that
     the value isn't a register number.  */
  if (sreg == (unsigned int) -1)
    {
      if (reg & ~0x3f)
	/* The register number won't fit in 6 bits, so we have to use
	   the long form.  */
	cfi->dw_cfi_opc = DW_CFA_offset_extended;
      else
	cfi->dw_cfi_opc = DW_CFA_offset;

#ifdef ENABLE_CHECKING
      {
	/* If we get an offset that is not a multiple of
	   DWARF_CIE_DATA_ALIGNMENT, there is either a bug in the
	   definition of DWARF_CIE_DATA_ALIGNMENT, or a bug in the machine
	   description.  */
	long check_offset = offset / DWARF_CIE_DATA_ALIGNMENT;

	if (check_offset * DWARF_CIE_DATA_ALIGNMENT != offset)
	  abort ();
      }
#endif
      offset /= DWARF_CIE_DATA_ALIGNMENT;
      if (offset < 0)
	{
	  cfi->dw_cfi_opc = DW_CFA_GNU_negative_offset_extended;
	  offset = -offset;
	}
      cfi->dw_cfi_oprnd2.dw_cfi_offset = offset;
    }
  else if (sreg == reg)
    /* We could emit a DW_CFA_same_value in this case, but don't bother.  */
    return;
  else
    {
      cfi->dw_cfi_opc = DW_CFA_register;
      cfi->dw_cfi_oprnd2.dw_cfi_reg_num = sreg;
    }

  add_fde_cfi (label, cfi);
}

/* Add the CFI for saving a register window.  LABEL is passed to reg_save.
   This CFI tells the unwinder that it needs to restore the window registers
   from the previous frame's window save area.

   ??? Perhaps we should note in the CIE where windows are saved (instead of
   assuming 0(cfa)) and what registers are in the window.  */

void
dwarf2out_window_save (label)
     register const char *label;
{
  register dw_cfi_ref cfi = new_cfi ();
  cfi->dw_cfi_opc = DW_CFA_GNU_window_save;
  add_fde_cfi (label, cfi);
}

/* Add a CFI to update the running total of the size of arguments
   pushed onto the stack.  */

void
dwarf2out_args_size (label, size)
     const char *label;
     long size;
{
  register dw_cfi_ref cfi;

  if (size == old_args_size)
    return;
  old_args_size = size;

  cfi = new_cfi ();
  cfi->dw_cfi_opc = DW_CFA_GNU_args_size;
  cfi->dw_cfi_oprnd1.dw_cfi_offset = size;
  add_fde_cfi (label, cfi);
}

/* Entry point for saving a register to the stack.  REG is the GCC register
   number.  LABEL and OFFSET are passed to reg_save.  */

void
dwarf2out_reg_save (label, reg, offset)
     register const char *label;
     register unsigned reg;
     register long offset;
{
  reg_save (label, DWARF_FRAME_REGNUM (reg), -1, offset);
}

/* Entry point for saving the return address in the stack.
   LABEL and OFFSET are passed to reg_save.  */

void
dwarf2out_return_save (label, offset)
     register const char *label;
     register long offset;
{
  reg_save (label, DWARF_FRAME_RETURN_COLUMN, -1, offset);
}

/* Entry point for saving the return address in a register.
   LABEL and SREG are passed to reg_save.  */

void
dwarf2out_return_reg (label, sreg)
     register const char *label;
     register unsigned sreg;
{
  reg_save (label, DWARF_FRAME_RETURN_COLUMN, sreg, 0);
}

/* Record the initial position of the return address.  RTL is
   INCOMING_RETURN_ADDR_RTX.  */

static void
initial_return_save (rtl)
     register rtx rtl;
{
  unsigned int reg = (unsigned int) -1;
  long offset = 0;

  switch (GET_CODE (rtl))
    {
    case REG:
      /* RA is in a register.  */
      reg = DWARF_FRAME_REGNUM (REGNO (rtl));
      break;
    case MEM:
      /* RA is on the stack.  */
      rtl = XEXP (rtl, 0);
      switch (GET_CODE (rtl))
	{
	case REG:
	  if (REGNO (rtl) != STACK_POINTER_REGNUM)
	    abort ();
	  offset = 0;
	  break;
	case PLUS:
	  if (REGNO (XEXP (rtl, 0)) != STACK_POINTER_REGNUM)
	    abort ();
	  offset = INTVAL (XEXP (rtl, 1));
	  break;
	case MINUS:
	  if (REGNO (XEXP (rtl, 0)) != STACK_POINTER_REGNUM)
	    abort ();
	  offset = -INTVAL (XEXP (rtl, 1));
	  break;
	default:
	  abort ();
	}
      break;
    case PLUS:
      /* The return address is at some offset from any value we can
	 actually load.  For instance, on the SPARC it is in %i7+8. Just
	 ignore the offset for now; it doesn't matter for unwinding frames.  */
      if (GET_CODE (XEXP (rtl, 1)) != CONST_INT)
	abort ();
      initial_return_save (XEXP (rtl, 0));
      return;
    default:
      abort ();
    }

  reg_save (NULL, DWARF_FRAME_RETURN_COLUMN, reg, offset - cfa.offset);
}

/* Given a SET, calculate the amount of stack adjustment it
   contains.  */

static long
stack_adjust_offset (pattern)
  rtx pattern;
{
  rtx src = SET_SRC (pattern);
  rtx dest = SET_DEST (pattern);
  long offset = 0;
  enum rtx_code code;

  if (dest == stack_pointer_rtx)
    {
      /* (set (reg sp) (plus (reg sp) (const_int))) */
      code = GET_CODE (src);
      if (! (code == PLUS || code == MINUS)
	  || XEXP (src, 0) != stack_pointer_rtx
	  || GET_CODE (XEXP (src, 1)) != CONST_INT)
	return 0;

      offset = INTVAL (XEXP (src, 1));
    }
  else if (GET_CODE (dest) == MEM)
    {
      /* (set (mem (pre_dec (reg sp))) (foo)) */
      src = XEXP (dest, 0);
      code = GET_CODE (src);

      if (! (code == PRE_DEC || code == PRE_INC
	     || code == PRE_MODIFY)
	  || XEXP (src, 0) != stack_pointer_rtx)
	return 0;

      if (code == PRE_MODIFY)
	{
	  rtx val = XEXP (XEXP (src, 1), 1);
	  /* We handle only adjustments by constant amount.  */
	  if (GET_CODE (XEXP (src, 1)) != PLUS ||
	      GET_CODE (val) != CONST_INT)
	    abort();
	  offset = -INTVAL (val);
	}
      else offset = GET_MODE_SIZE (GET_MODE (dest));
    }
  else
    return 0;

  if (code == PLUS || code == PRE_INC)
    offset = -offset;

  return offset;
}

/* Check INSN to see if it looks like a push or a stack adjustment, and
   make a note of it if it does.  EH uses this information to find out how
   much extra space it needs to pop off the stack.  */

static void
dwarf2out_stack_adjust (insn)
     rtx insn;
{
  long offset;
  const char *label;

  if (! flag_non_call_exceptions && GET_CODE (insn) == CALL_INSN)
    {
      /* Extract the size of the args from the CALL rtx itself.  */

      insn = PATTERN (insn);
      if (GET_CODE (insn) == PARALLEL)
	insn = XVECEXP (insn, 0, 0);
      if (GET_CODE (insn) == SET)
	insn = SET_SRC (insn);
      if (GET_CODE (insn) != CALL)
	abort ();
      dwarf2out_args_size ("", INTVAL (XEXP (insn, 1)));
      return;
    }

  /* If only calls can throw, and we have a frame pointer,
     save up adjustments until we see the CALL_INSN.  */
  else if (! flag_non_call_exceptions
	   && cfa.reg != STACK_POINTER_REGNUM)
    return;

  if (GET_CODE (insn) == BARRIER)
    {
      /* When we see a BARRIER, we know to reset args_size to 0.  Usually
	 the compiler will have already emitted a stack adjustment, but
	 doesn't bother for calls to noreturn functions.  */
#ifdef STACK_GROWS_DOWNWARD
      offset = -args_size;
#else
      offset = args_size;
#endif
    }
  else if (GET_CODE (PATTERN (insn)) == SET)
    {
      offset = stack_adjust_offset (PATTERN (insn));
    }
  else if (GET_CODE (PATTERN (insn)) == PARALLEL
	   || GET_CODE (PATTERN (insn)) == SEQUENCE)
    {
      /* There may be stack adjustments inside compound insns.  Search
         for them.  */
      int j;

      offset = 0;
      for (j = XVECLEN (PATTERN (insn), 0) - 1; j >= 0; j--)
	{
	  rtx pattern = XVECEXP (PATTERN (insn), 0, j);
	  if (GET_CODE (pattern) == SET)
	    offset += stack_adjust_offset (pattern);
	}
    }
  else
    return;

  if (offset == 0)
    return;

  if (cfa.reg == STACK_POINTER_REGNUM)
    cfa.offset += offset;

#ifndef STACK_GROWS_DOWNWARD
  offset = -offset;
#endif
  args_size += offset;
  if (args_size < 0)
    args_size = 0;

  label = dwarf2out_cfi_label ();
  def_cfa_1 (label, &cfa);
  dwarf2out_args_size (label, args_size);
}

/* We delay emitting a register save until either (a) we reach the end
   of the prologue or (b) the register is clobbered.  This clusters
   register saves so that there are fewer pc advances.  */

struct queued_reg_save
{
  struct queued_reg_save *next;
  rtx reg;
  long cfa_offset;
};

static struct queued_reg_save *queued_reg_saves;
static const char *last_reg_save_label;

static void
queue_reg_save (label, reg, offset)
     const char *label;
     rtx reg;
     long offset;
{
  struct queued_reg_save *q = (struct queued_reg_save *) xmalloc (sizeof (*q));

  q->next = queued_reg_saves;
  q->reg = reg;
  q->cfa_offset = offset;
  queued_reg_saves = q;

  last_reg_save_label = label;
}

static void
flush_queued_reg_saves ()
{
  struct queued_reg_save *q, *next;

  for (q = queued_reg_saves; q ; q = next)
    {
      dwarf2out_reg_save (last_reg_save_label, REGNO (q->reg), q->cfa_offset);
      next = q->next;
      free (q);
    }

  queued_reg_saves = NULL;
  last_reg_save_label = NULL;
}

static bool
clobbers_queued_reg_save (insn)
     rtx insn;
{
  struct queued_reg_save *q;

  for (q = queued_reg_saves; q ; q = q->next)
    if (modified_in_p (q->reg, insn))
      return true;

  return false;
}
  

/* A temporary register holding an integral value used in adjusting SP
   or setting up the store_reg.  The "offset" field holds the integer
   value, not an offset.  */
static dw_cfa_location cfa_temp;

/* Record call frame debugging information for an expression EXPR,
   which either sets SP or FP (adjusting how we calculate the frame
   address) or saves a register to the stack.  LABEL indicates the
   address of EXPR.

   This function encodes a state machine mapping rtxes to actions on
   cfa, cfa_store, and cfa_temp.reg.  We describe these rules so
   users need not read the source code.

  The High-Level Picture

  Changes in the register we use to calculate the CFA: Currently we
  assume that if you copy the CFA register into another register, we
  should take the other one as the new CFA register; this seems to
  work pretty well.  If it's wrong for some target, it's simple
  enough not to set RTX_FRAME_RELATED_P on the insn in question.

  Changes in the register we use for saving registers to the stack:
  This is usually SP, but not always.  Again, we deduce that if you
  copy SP into another register (and SP is not the CFA register),
  then the new register is the one we will be using for register
  saves.  This also seems to work.

  Register saves: There's not much guesswork about this one; if
  RTX_FRAME_RELATED_P is set on an insn which modifies memory, it's a
  register save, and the register used to calculate the destination
  had better be the one we think we're using for this purpose.

  Except: If the register being saved is the CFA register, and the
  offset is non-zero, we are saving the CFA, so we assume we have to
  use DW_CFA_def_cfa_expression.  If the offset is 0, we assume that
  the intent is to save the value of SP from the previous frame.

  Invariants / Summaries of Rules

  cfa	       current rule for calculating the CFA.  It usually
	       consists of a register and an offset.
  cfa_store    register used by prologue code to save things to the stack
	       cfa_store.offset is the offset from the value of
	       cfa_store.reg to the actual CFA
  cfa_temp     register holding an integral value.  cfa_temp.offset
	       stores the value, which will be used to adjust the
	       stack pointer.  cfa_temp is also used like cfa_store,
	       to track stores to the stack via fp or a temp reg.
 
  Rules  1- 4: Setting a register's value to cfa.reg or an expression
  	       with cfa.reg as the first operand changes the cfa.reg and its
	       cfa.offset.  Rule 1 and 4 also set cfa_temp.reg and
	       cfa_temp.offset.

  Rules  6- 9: Set a non-cfa.reg register value to a constant or an
	       expression yielding a constant.  This sets cfa_temp.reg
	       and cfa_temp.offset.

  Rule 5:      Create a new register cfa_store used to save items to the
	       stack.

  Rules 10-14: Save a register to the stack.  Define offset as the
	       difference of the original location and cfa_store's
	       location (or cfa_temp's location if cfa_temp is used).

  The Rules

  "{a,b}" indicates a choice of a xor b.
  "<reg>:cfa.reg" indicates that <reg> must equal cfa.reg.

  Rule 1:
  (set <reg1> <reg2>:cfa.reg)
  effects: cfa.reg = <reg1>
           cfa.offset unchanged
	   cfa_temp.reg = <reg1>
	   cfa_temp.offset = cfa.offset

  Rule 2:
  (set sp ({minus,plus,losum} {sp,fp}:cfa.reg {<const_int>,<reg>:cfa_temp.reg}))
  effects: cfa.reg = sp if fp used
 	   cfa.offset += {+/- <const_int>, cfa_temp.offset} if cfa.reg==sp
	   cfa_store.offset += {+/- <const_int>, cfa_temp.offset}
	     if cfa_store.reg==sp

  Rule 3:
  (set fp ({minus,plus,losum} <reg>:cfa.reg <const_int>))
  effects: cfa.reg = fp
  	   cfa_offset += +/- <const_int>

  Rule 4:
  (set <reg1> ({plus,losum} <reg2>:cfa.reg <const_int>))
  constraints: <reg1> != fp
  	       <reg1> != sp
  effects: cfa.reg = <reg1>
	   cfa_temp.reg = <reg1>
	   cfa_temp.offset = cfa.offset

  Rule 5:
  (set <reg1> (plus <reg2>:cfa_temp.reg sp:cfa.reg))
  constraints: <reg1> != fp
  	       <reg1> != sp
  effects: cfa_store.reg = <reg1>
  	   cfa_store.offset = cfa.offset - cfa_temp.offset

  Rule 6:
  (set <reg> <const_int>)
  effects: cfa_temp.reg = <reg>
  	   cfa_temp.offset = <const_int>

  Rule 7:
  (set <reg1>:cfa_temp.reg (ior <reg2>:cfa_temp.reg <const_int>))
  effects: cfa_temp.reg = <reg1>
	   cfa_temp.offset |= <const_int>

  Rule 8:
  (set <reg> (high <exp>))
  effects: none

  Rule 9:
  (set <reg> (lo_sum <exp> <const_int>))
  effects: cfa_temp.reg = <reg>
  	   cfa_temp.offset = <const_int>

  Rule 10:
  (set (mem (pre_modify sp:cfa_store (???? <reg1> <const_int>))) <reg2>)
  effects: cfa_store.offset -= <const_int>
	   cfa.offset = cfa_store.offset if cfa.reg == sp
	   cfa.reg = sp
	   cfa.base_offset = -cfa_store.offset

  Rule 11:
  (set (mem ({pre_inc,pre_dec} sp:cfa_store.reg)) <reg>)
  effects: cfa_store.offset += -/+ mode_size(mem)
	   cfa.offset = cfa_store.offset if cfa.reg == sp
	   cfa.reg = sp
	   cfa.base_offset = -cfa_store.offset

  Rule 12:
  (set (mem ({minus,plus,losum} <reg1>:{cfa_store,cfa_temp} <const_int>)) <reg2>)
  effects: cfa.reg = <reg1>
	   cfa.base_offset = -/+ <const_int> - {cfa_store,cfa_temp}.offset

  Rule 13:
  (set (mem <reg1>:{cfa_store,cfa_temp}) <reg2>)
  effects: cfa.reg = <reg1>
	   cfa.base_offset = -{cfa_store,cfa_temp}.offset

  Rule 14:
  (set (mem (postinc <reg1>:cfa_temp <const_int>)) <reg2>)
  effects: cfa.reg = <reg1>
	   cfa.base_offset = -cfa_temp.offset
	   cfa_temp.offset -= mode_size(mem)  */

static void
dwarf2out_frame_debug_expr (expr, label)
     rtx expr;
     const char *label;
{
  rtx src, dest;
  long offset;

  /* If RTX_FRAME_RELATED_P is set on a PARALLEL, process each member of
     the PARALLEL independently. The first element is always processed if
     it is a SET. This is for backward compatibility.   Other elements
     are processed only if they are SETs and the RTX_FRAME_RELATED_P
     flag is set in them.  */

  if (GET_CODE (expr) == PARALLEL
      || GET_CODE (expr) == SEQUENCE)
    {
      int par_index;
      int limit = XVECLEN (expr, 0);

      for (par_index = 0; par_index < limit; par_index++)
	{
	  rtx x = XVECEXP (expr, 0, par_index);

	  if (GET_CODE (x) == SET &&
	      (RTX_FRAME_RELATED_P (x) || par_index == 0))
	    dwarf2out_frame_debug_expr (x, label);
	}
      return;
    }

  if (GET_CODE (expr) != SET)
    abort ();

  src = SET_SRC (expr);
  dest = SET_DEST (expr);

  switch (GET_CODE (dest))
    {
    case REG:
      /* Rule 1 */
      /* Update the CFA rule wrt SP or FP.  Make sure src is
         relative to the current CFA register.  */
      switch (GET_CODE (src))
	{
	  /* Setting FP from SP.  */
	case REG:
	  if (cfa.reg == (unsigned) REGNO (src))
	    /* OK.  */
	    ;
	  else
	    abort ();

	  /* We used to require that dest be either SP or FP, but the
	     ARM copies SP to a temporary register, and from there to
	     FP.  So we just rely on the backends to only set
	     RTX_FRAME_RELATED_P on appropriate insns.  */
	  cfa.reg = REGNO (dest);
	  cfa_temp.reg = cfa.reg;
	  cfa_temp.offset = cfa.offset;
	  break;

	case PLUS:
	case MINUS:
	case LO_SUM:
	  if (dest == stack_pointer_rtx)
	    {
	      /* Rule 2 */
	      /* Adjusting SP.  */
	      switch (GET_CODE (XEXP (src, 1)))
		{
		case CONST_INT:
		  offset = INTVAL (XEXP (src, 1));
		  break;
		case REG:
		  if ((unsigned) REGNO (XEXP (src, 1)) != cfa_temp.reg)
		    abort ();
		  offset = cfa_temp.offset;
		  break;
		default:
		  abort ();
		}

	      if (XEXP (src, 0) == hard_frame_pointer_rtx)
		{
		  /* Restoring SP from FP in the epilogue.  */
		  if (cfa.reg != (unsigned) HARD_FRAME_POINTER_REGNUM)
		    abort ();
		  cfa.reg = STACK_POINTER_REGNUM;
		}
	      else if (GET_CODE (src) == LO_SUM)
		/* Assume we've set the source reg of the LO_SUM from sp.  */
		;
	      else if (XEXP (src, 0) != stack_pointer_rtx)
		abort ();

	      if (GET_CODE (src) != MINUS)
		offset = -offset;
	      if (cfa.reg == STACK_POINTER_REGNUM)
		cfa.offset += offset;
	      if (cfa_store.reg == STACK_POINTER_REGNUM)
		cfa_store.offset += offset;
	    }
	  else if (dest == hard_frame_pointer_rtx)
	    {
	      /* Rule 3 */
	      /* Either setting the FP from an offset of the SP,
		 or adjusting the FP */
	      if (! frame_pointer_needed)
		abort ();

	      if (GET_CODE (XEXP (src, 0)) == REG
		  && (unsigned) REGNO (XEXP (src, 0)) == cfa.reg
		  && GET_CODE (XEXP (src, 1)) == CONST_INT)
		{
		  offset = INTVAL (XEXP (src, 1));
		  if (GET_CODE (src) != MINUS)
		    offset = -offset;
		  cfa.offset += offset;
		  cfa.reg = HARD_FRAME_POINTER_REGNUM;
		}
	      else
		abort ();
	    }
	  else
	    {
	      if (GET_CODE (src) == MINUS)
		abort ();

	      /* Rule 4 */
	      if (GET_CODE (XEXP (src, 0)) == REG
		  && REGNO (XEXP (src, 0)) == cfa.reg
		  && GET_CODE (XEXP (src, 1)) == CONST_INT)
		{
		  /* Setting a temporary CFA register that will be copied
		     into the FP later on.  */
		  offset = - INTVAL (XEXP (src, 1));
		  cfa.offset += offset;
		  cfa.reg = REGNO (dest);
		  /* Or used to save regs to the stack.  */
		  cfa_temp.reg = cfa.reg;
		  cfa_temp.offset = cfa.offset;
		}
	      /* Rule 5 */
	      else if (GET_CODE (XEXP (src, 0)) == REG
		       && REGNO (XEXP (src, 0)) == cfa_temp.reg
		       && XEXP (src, 1) == stack_pointer_rtx)
		{
		  /* Setting a scratch register that we will use instead
		     of SP for saving registers to the stack.  */
		  if (cfa.reg != STACK_POINTER_REGNUM)
		    abort ();
		  cfa_store.reg = REGNO (dest);
		  cfa_store.offset = cfa.offset - cfa_temp.offset;
		}
	      /* Rule 9 */
	      else if (GET_CODE (src) == LO_SUM
		       && GET_CODE (XEXP (src, 1)) == CONST_INT)
		{
		  cfa_temp.reg = REGNO (dest);
		  cfa_temp.offset = INTVAL (XEXP (src, 1));
		}
	      else
		abort ();
	    }
	  break;

	  /* Rule 6 */
	case CONST_INT:
	  cfa_temp.reg = REGNO (dest);
	  cfa_temp.offset = INTVAL (src);
	  break;

	  /* Rule 7 */
	case IOR:
	  if (GET_CODE (XEXP (src, 0)) != REG
	      || (unsigned) REGNO (XEXP (src, 0)) != cfa_temp.reg
	      || GET_CODE (XEXP (src, 1)) != CONST_INT)
	    abort ();
	  if ((unsigned) REGNO (dest) != cfa_temp.reg)
	    cfa_temp.reg = REGNO (dest);
	  cfa_temp.offset |= INTVAL (XEXP (src, 1));
	  break;

	  /* Skip over HIGH, assuming it will be followed by a LO_SUM,
	     which will fill in all of the bits.  */
	  /* Rule 8 */
	case HIGH:
	  break;

	default:
	  abort ();
	}
      def_cfa_1 (label, &cfa);
      break;

    case MEM:
      if (GET_CODE (src) != REG)
	abort ();

      /* Saving a register to the stack.  Make sure dest is relative to the
	 CFA register.  */
      switch (GET_CODE (XEXP (dest, 0)))
	{
	  /* Rule 10 */
	  /* With a push.  */
	case PRE_MODIFY:
	  /* We can't handle variable size modifications.  */
	  if (GET_CODE (XEXP (XEXP (XEXP (dest, 0), 1), 1)) != CONST_INT)
	    abort();
	  offset = -INTVAL (XEXP (XEXP (XEXP (dest, 0), 1), 1));

	  if (REGNO (XEXP (XEXP (dest, 0), 0)) != STACK_POINTER_REGNUM
	      || cfa_store.reg != STACK_POINTER_REGNUM)
	    abort ();
	  cfa_store.offset += offset;
	  if (cfa.reg == STACK_POINTER_REGNUM)
	    cfa.offset = cfa_store.offset;

	  offset = -cfa_store.offset;
	  break;
	  /* Rule 11 */
	case PRE_INC:
	case PRE_DEC:
	  offset = GET_MODE_SIZE (GET_MODE (dest));
	  if (GET_CODE (XEXP (dest, 0)) == PRE_INC)
	    offset = -offset;

	  if (REGNO (XEXP (XEXP (dest, 0), 0)) != STACK_POINTER_REGNUM
	      || cfa_store.reg != STACK_POINTER_REGNUM)
	    abort ();
	  cfa_store.offset += offset;
	  if (cfa.reg == STACK_POINTER_REGNUM)
	    cfa.offset = cfa_store.offset;

	  offset = -cfa_store.offset;
	  break;

	  /* Rule 12 */
	  /* With an offset.  */
	case PLUS:
	case MINUS:
	case LO_SUM:
	  if (GET_CODE (XEXP (XEXP (dest, 0), 1)) != CONST_INT)
	    abort ();
	  offset = INTVAL (XEXP (XEXP (dest, 0), 1));
	  if (GET_CODE (XEXP (dest, 0)) == MINUS)
	    offset = -offset;

	  if (cfa_store.reg == (unsigned) REGNO (XEXP (XEXP (dest, 0), 0)))
	    offset -= cfa_store.offset;
	  else if (cfa_temp.reg == (unsigned) REGNO (XEXP (XEXP (dest, 0), 0)))
	    offset -= cfa_temp.offset;
	  else
	    abort ();
	  break;

	  /* Rule 13 */
	  /* Without an offset.  */
	case REG:
	  if (cfa_store.reg == (unsigned) REGNO (XEXP (dest, 0)))
	    offset = -cfa_store.offset;
	  else if (cfa_temp.reg == (unsigned) REGNO (XEXP (dest, 0)))
	    offset = -cfa_temp.offset;
	  else
	    abort ();
	  break;

	  /* Rule 14 */
	case POST_INC:
	  if (cfa_temp.reg != (unsigned) REGNO (XEXP (XEXP (dest, 0), 0)))
	    abort ();
	  offset = -cfa_temp.offset;
	  cfa_temp.offset -= GET_MODE_SIZE (GET_MODE (dest));
	  break;

	default:
	  abort ();
	}

      if (REGNO (src) != STACK_POINTER_REGNUM
	  && REGNO (src) != HARD_FRAME_POINTER_REGNUM
	  && (unsigned) REGNO (src) == cfa.reg)
	{
	  /* We're storing the current CFA reg into the stack.  */

	  if (cfa.offset == 0)
	    {
	      /* If the source register is exactly the CFA, assume
		 we're saving SP like any other register; this happens
		 on the ARM.  */

	      def_cfa_1 (label, &cfa);
	      queue_reg_save (label, stack_pointer_rtx, offset);
	      break;
	    }
	  else
	    {
	      /* Otherwise, we'll need to look in the stack to
                 calculate the CFA.  */

	      rtx x = XEXP (dest, 0);
	      if (GET_CODE (x) != REG)
		x = XEXP (x, 0);
	      if (GET_CODE (x) != REG)
		abort ();
	      cfa.reg = (unsigned) REGNO (x);
	      cfa.base_offset = offset;
	      cfa.indirect = 1;
	      def_cfa_1 (label, &cfa);
	      break;
	    }
	}

      def_cfa_1 (label, &cfa);
      queue_reg_save (label, src, offset);
      break;

    default:
      abort ();
    }
}

/* Record call frame debugging information for INSN, which either
   sets SP or FP (adjusting how we calculate the frame address) or saves a
   register to the stack.  If INSN is NULL_RTX, initialize our state.  */

void
dwarf2out_frame_debug (insn)
     rtx insn;
{
  const char *label;
  rtx src;

  if (insn == NULL_RTX)
    {
      /* Flush any queued register saves.  */
      flush_queued_reg_saves ();

      /* Set up state for generating call frame debug info.  */
      lookup_cfa (&cfa);
      if (cfa.reg != (unsigned long) DWARF_FRAME_REGNUM (STACK_POINTER_REGNUM))
	abort ();
      cfa.reg = STACK_POINTER_REGNUM;
      cfa_store = cfa;
      cfa_temp.reg = -1;
      cfa_temp.offset = 0;
      return;
    }

  if (GET_CODE (insn) != INSN || clobbers_queued_reg_save (insn))
    flush_queued_reg_saves ();

  if (! RTX_FRAME_RELATED_P (insn))
    {
      if (!ACCUMULATE_OUTGOING_ARGS)
        dwarf2out_stack_adjust (insn);
      return;
    }

  label = dwarf2out_cfi_label ();

  src = find_reg_note (insn, REG_FRAME_RELATED_EXPR, NULL_RTX);
  if (src)
    insn = XEXP (src, 0);
  else
    insn = PATTERN (insn);

  dwarf2out_frame_debug_expr (insn, label);
}

/* Output a Call Frame Information opcode and its operand(s).  */

static void
output_cfi (cfi, fde, for_eh)
     register dw_cfi_ref cfi;
     register dw_fde_ref fde;
     int for_eh;
{
  if (cfi->dw_cfi_opc == DW_CFA_advance_loc)
    {
      dw2_asm_output_data (1, (cfi->dw_cfi_opc
			       | (cfi->dw_cfi_oprnd1.dw_cfi_offset & 0x3f)),
			   "DW_CFA_advance_loc 0x%lx",
			   cfi->dw_cfi_oprnd1.dw_cfi_offset);
    }
  else if (cfi->dw_cfi_opc == DW_CFA_offset)
    {
      dw2_asm_output_data (1, (cfi->dw_cfi_opc
			       | (cfi->dw_cfi_oprnd1.dw_cfi_reg_num & 0x3f)),
			   "DW_CFA_offset, column 0x%lx",
			   cfi->dw_cfi_oprnd1.dw_cfi_reg_num);
      dw2_asm_output_data_uleb128 (cfi->dw_cfi_oprnd2.dw_cfi_offset, NULL);
    }
  else if (cfi->dw_cfi_opc == DW_CFA_restore)
    {
      dw2_asm_output_data (1, (cfi->dw_cfi_opc
			       | (cfi->dw_cfi_oprnd1.dw_cfi_reg_num & 0x3f)),
			   "DW_CFA_restore, column 0x%lx",
			   cfi->dw_cfi_oprnd1.dw_cfi_reg_num);
    }
  else
    {
      dw2_asm_output_data (1, cfi->dw_cfi_opc,
			   "%s", dwarf_cfi_name (cfi->dw_cfi_opc));

      switch (cfi->dw_cfi_opc)
	{
	case DW_CFA_set_loc:
	  if (for_eh)
	    dw2_asm_output_encoded_addr_rtx (
		ASM_PREFERRED_EH_DATA_FORMAT (/*code=*/1, /*global=*/0),
		gen_rtx_SYMBOL_REF (Pmode, cfi->dw_cfi_oprnd1.dw_cfi_addr),
		NULL);
	  else
	    dw2_asm_output_addr (DWARF2_ADDR_SIZE,
				 cfi->dw_cfi_oprnd1.dw_cfi_addr, NULL);
	  break;
	case DW_CFA_advance_loc1:
	  dw2_asm_output_delta (1, cfi->dw_cfi_oprnd1.dw_cfi_addr,
				fde->dw_fde_current_label, NULL);
	  fde->dw_fde_current_label = cfi->dw_cfi_oprnd1.dw_cfi_addr;
	  break;
	case DW_CFA_advance_loc2:
	  dw2_asm_output_delta (2, cfi->dw_cfi_oprnd1.dw_cfi_addr,
				fde->dw_fde_current_label, NULL);
	  fde->dw_fde_current_label = cfi->dw_cfi_oprnd1.dw_cfi_addr;
	  break;
	case DW_CFA_advance_loc4:
	  dw2_asm_output_delta (4, cfi->dw_cfi_oprnd1.dw_cfi_addr,
				fde->dw_fde_current_label, NULL);
	  fde->dw_fde_current_label = cfi->dw_cfi_oprnd1.dw_cfi_addr;
	  break;
	case DW_CFA_MIPS_advance_loc8:
	  dw2_asm_output_delta (8, cfi->dw_cfi_oprnd1.dw_cfi_addr,
				fde->dw_fde_current_label, NULL);
	  fde->dw_fde_current_label = cfi->dw_cfi_oprnd1.dw_cfi_addr;
	  break;
	case DW_CFA_offset_extended:
	case DW_CFA_GNU_negative_offset_extended:
	case DW_CFA_def_cfa:
	  dw2_asm_output_data_uleb128 (cfi->dw_cfi_oprnd1.dw_cfi_reg_num, NULL);
	  dw2_asm_output_data_uleb128 (cfi->dw_cfi_oprnd2.dw_cfi_offset, NULL);
	  break;
	case DW_CFA_restore_extended:
	case DW_CFA_undefined:
	case DW_CFA_same_value:
	case DW_CFA_def_cfa_register:
	  dw2_asm_output_data_uleb128 (cfi->dw_cfi_oprnd1.dw_cfi_reg_num, NULL);
	  break;
	case DW_CFA_register:
	  dw2_asm_output_data_uleb128 (cfi->dw_cfi_oprnd1.dw_cfi_reg_num, NULL);
	  dw2_asm_output_data_uleb128 (cfi->dw_cfi_oprnd2.dw_cfi_reg_num, NULL);
	  break;
	case DW_CFA_def_cfa_offset:
	case DW_CFA_GNU_args_size:
	  dw2_asm_output_data_uleb128 (cfi->dw_cfi_oprnd1.dw_cfi_offset, NULL);
	  break;
	case DW_CFA_GNU_window_save:
	  break;
	case DW_CFA_def_cfa_expression:
	  output_cfa_loc (cfi);
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
output_call_frame_info (for_eh)
     int for_eh;
{
  register unsigned int i;
  register dw_fde_ref fde;
  register dw_cfi_ref cfi;
  char l1[20], l2[20];
  int any_lsda_needed = 0;
  char augmentation[6];
  int augmentation_size;
  int fde_encoding = DW_EH_PE_absptr;
  int per_encoding = DW_EH_PE_absptr;
  int lsda_encoding = DW_EH_PE_absptr;

  /* If we don't have any functions we'll want to unwind out of, don't
     emit any EH unwind information.  */
  if (for_eh)
    {
      int any_eh_needed = 0;
      for (i = 0; i < fde_table_in_use; ++i)
	if (fde_table[i].uses_eh_lsda)
	  any_eh_needed = any_lsda_needed = 1;
	else if (! fde_table[i].nothrow)
	  any_eh_needed = 1;

      if (! any_eh_needed)
	return;
    }

  /* We're going to be generating comments, so turn on app.  */
  if (flag_debug_asm)
    app_enable ();

  if (for_eh)
    {
#ifdef EH_FRAME_SECTION_NAME
      named_section_flags (EH_FRAME_SECTION_NAME, SECTION_WRITE,
			   DWARF_OFFSET_SIZE);
#else
      tree label = get_file_function_name ('F');

      data_section ();
      ASM_OUTPUT_ALIGN (asm_out_file, floor_log2 (PTR_SIZE));
      ASM_GLOBALIZE_LABEL (asm_out_file, IDENTIFIER_POINTER (label));
      ASM_OUTPUT_LABEL (asm_out_file, IDENTIFIER_POINTER (label));
#endif
      assemble_label ("__FRAME_BEGIN__");
    }
  else
    named_section_flags (DEBUG_FRAME_SECTION, SECTION_DEBUG, 1);

  /* Output the CIE.  */
  ASM_GENERATE_INTERNAL_LABEL (l1, CIE_AFTER_SIZE_LABEL, for_eh);
  ASM_GENERATE_INTERNAL_LABEL (l2, CIE_END_LABEL, for_eh);
  dw2_asm_output_delta (for_eh ? 4 : DWARF_OFFSET_SIZE, l2, l1,
			"Length of Common Information Entry");
  ASM_OUTPUT_LABEL (asm_out_file, l1);

  /* Now that the CIE pointer is PC-relative for EH,
     use 0 to identify the CIE.  */
  dw2_asm_output_data ((for_eh ? 4 : DWARF_OFFSET_SIZE),
		       (for_eh ? 0 : DW_CIE_ID),
		       "CIE Identifier Tag");

  dw2_asm_output_data (1, DW_CIE_VERSION, "CIE Version");

  augmentation[0] = 0;
  augmentation_size = 0;
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
      if (eh_personality_libfunc)
	{
	  *p++ = 'P';
	  augmentation_size += 1 + size_of_encoded_value (per_encoding);
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
      if (eh_personality_libfunc && per_encoding == DW_EH_PE_aligned)
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
	  if (size_of_uleb128 (augmentation_size) != 1)
	    abort ();
	}
    }
  dw2_asm_output_nstring (augmentation, -1, "CIE Augmentation");

  dw2_asm_output_data_uleb128 (1, "CIE Code Alignment Factor");

  dw2_asm_output_data_sleb128 (DWARF_CIE_DATA_ALIGNMENT,
			       "CIE Data Alignment Factor");

  dw2_asm_output_data (1, DWARF_FRAME_RETURN_COLUMN, "CIE RA Column");

  if (augmentation[0])
    {
      dw2_asm_output_data_uleb128 (augmentation_size, "Augmentation size");
      if (eh_personality_libfunc)
	{
	  dw2_asm_output_data (1, per_encoding, "Personality (%s)",
			       eh_data_format_name (per_encoding));
	  dw2_asm_output_encoded_addr_rtx (per_encoding,
					   eh_personality_libfunc, NULL);
	}
      if (any_lsda_needed)
	dw2_asm_output_data (1, lsda_encoding, "LSDA Encoding (%s)",
			     eh_data_format_name (lsda_encoding));
      if (fde_encoding != DW_EH_PE_absptr)
	dw2_asm_output_data (1, fde_encoding, "FDE Encoding (%s)",
			     eh_data_format_name (fde_encoding));
    }

  for (cfi = cie_cfi_head; cfi != NULL; cfi = cfi->dw_cfi_next)
    output_cfi (cfi, NULL, for_eh);

  /* Pad the CIE out to an address sized boundary.  */
  ASM_OUTPUT_ALIGN (asm_out_file, 
		    floor_log2 (for_eh ? PTR_SIZE : DWARF2_ADDR_SIZE));
  ASM_OUTPUT_LABEL (asm_out_file, l2);

  /* Loop through all of the FDE's.  */
  for (i = 0; i < fde_table_in_use; ++i)
    {
      fde = &fde_table[i];

      /* Don't emit EH unwind info for leaf functions that don't need it.  */
      if (for_eh && fde->nothrow && ! fde->uses_eh_lsda)
	continue;

      ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, FDE_LABEL, for_eh + i * 2);
      ASM_GENERATE_INTERNAL_LABEL (l1, FDE_AFTER_SIZE_LABEL, for_eh + i * 2);
      ASM_GENERATE_INTERNAL_LABEL (l2, FDE_END_LABEL, for_eh + i * 2);
      dw2_asm_output_delta (for_eh ? 4 : DWARF_OFFSET_SIZE, l2, l1,
			    "FDE Length");
      ASM_OUTPUT_LABEL (asm_out_file, l1);

      /* ??? This always emits a 4 byte offset when for_eh is true, but it
	 emits a target dependent sized offset when for_eh is not true.
	 This inconsistency may confuse gdb.  The only case where we need a
	 non-4 byte offset is for the Irix6 N64 ABI, so we may lose SGI
	 compatibility if we emit a 4 byte offset.  We need a 4 byte offset
	 though in order to be compatible with the dwarf_fde struct in frame.c.
	 If the for_eh case is changed, then the struct in frame.c has
	 to be adjusted appropriately.  */
      if (for_eh)
	dw2_asm_output_delta (4, l1, "__FRAME_BEGIN__", "FDE CIE offset");
      else
	dw2_asm_output_offset (DWARF_OFFSET_SIZE,
			       stripattributes (DEBUG_FRAME_SECTION),
			       "FDE CIE offset");

      if (for_eh)
	{
	  dw2_asm_output_encoded_addr_rtx (fde_encoding,
		   gen_rtx_SYMBOL_REF (Pmode, fde->dw_fde_begin),
		   "FDE initial location");
	  dw2_asm_output_delta (size_of_encoded_value (fde_encoding),
				fde->dw_fde_end, fde->dw_fde_begin, 
				"FDE address range");
	}
      else
	{
	  dw2_asm_output_addr (DWARF2_ADDR_SIZE, fde->dw_fde_begin,
			       "FDE initial location");
	  dw2_asm_output_delta (DWARF2_ADDR_SIZE, 
				fde->dw_fde_end, fde->dw_fde_begin, 
				"FDE address range");
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
		  if (size_of_uleb128 (size) != 1)
		    abort ();
		}

	      dw2_asm_output_data_uleb128 (size, "Augmentation size");

	      if (fde->uses_eh_lsda)
	        {
	          ASM_GENERATE_INTERNAL_LABEL (l1, "LLSDA",
					       fde->funcdef_number);
	          dw2_asm_output_encoded_addr_rtx (
			lsda_encoding, gen_rtx_SYMBOL_REF (Pmode, l1),
		 	"Language Specific Data Area");
	        }
	      else
		{
		  if (lsda_encoding == DW_EH_PE_aligned)
		    ASM_OUTPUT_ALIGN (asm_out_file, floor_log2 (PTR_SIZE));
		  dw2_asm_output_data (size_of_encoded_value (lsda_encoding),
				       0, "Language Specific Data Area (none)");
		}
	    }
	  else
	    dw2_asm_output_data_uleb128 (0, "Augmentation size");
	}

      /* Loop through the Call Frame Instructions associated with
	 this FDE.  */
      fde->dw_fde_current_label = fde->dw_fde_begin;
      for (cfi = fde->dw_fde_cfi; cfi != NULL; cfi = cfi->dw_cfi_next)
	output_cfi (cfi, fde, for_eh);

      /* Pad the FDE out to an address sized boundary.  */
      ASM_OUTPUT_ALIGN (asm_out_file, 
		        floor_log2 ((for_eh ? PTR_SIZE : DWARF2_ADDR_SIZE)));
      ASM_OUTPUT_LABEL (asm_out_file, l2);
    }

#ifndef EH_FRAME_SECTION_NAME
  if (for_eh)
    dw2_asm_output_data (4, 0, "End of Table");
#endif
#ifdef MIPS_DEBUGGING_INFO
  /* Work around Irix 6 assembler bug whereby labels at the end of a section
     get a value of 0.  Putting .align 0 after the label fixes it.  */
  ASM_OUTPUT_ALIGN (asm_out_file, 0);
#endif

  /* Turn off app to make assembly quicker.  */
  if (flag_debug_asm)
    app_disable ();
}

/* Output a marker (i.e. a label) for the beginning of a function, before
   the prologue.  */

void
dwarf2out_begin_prologue (line, file)
     unsigned int line ATTRIBUTE_UNUSED;
     const char *file ATTRIBUTE_UNUSED;
{
  char label[MAX_ARTIFICIAL_LABEL_BYTES];
  register dw_fde_ref fde;

  current_function_func_begin_label = 0;

#ifdef IA64_UNWIND_INFO
  /* ??? current_function_func_begin_label is also used by except.c
     for call-site information.  We must emit this label if it might
     be used.  */
  if ((! flag_exceptions || USING_SJLJ_EXCEPTIONS)
      && ! dwarf2out_do_frame ())
    return;
#else
  if (! dwarf2out_do_frame ())
    return;
#endif

  ++current_funcdef_number;

  function_section (current_function_decl);
  ASM_GENERATE_INTERNAL_LABEL (label, FUNC_BEGIN_LABEL,
			       current_funcdef_number);
  ASM_OUTPUT_DEBUG_LABEL (asm_out_file, FUNC_BEGIN_LABEL,
			  current_funcdef_number);
  current_function_func_begin_label = get_identifier (label);

#ifdef IA64_UNWIND_INFO
  /* We can elide the fde allocation if we're not emitting debug info.  */
  if (! dwarf2out_do_frame ())
    return;
#endif

  /* Expand the fde table if necessary.  */
  if (fde_table_in_use == fde_table_allocated)
    {
      fde_table_allocated += FDE_TABLE_INCREMENT;
      fde_table
	= (dw_fde_ref) xrealloc (fde_table,
				 fde_table_allocated * sizeof (dw_fde_node));
    }

  /* Record the FDE associated with this function.  */
  current_funcdef_fde = fde_table_in_use;

  /* Add the new FDE at the end of the fde_table.  */
  fde = &fde_table[fde_table_in_use++];
  fde->dw_fde_begin = xstrdup (label);
  fde->dw_fde_current_label = NULL;
  fde->dw_fde_end = NULL;
  fde->dw_fde_cfi = NULL;
  fde->funcdef_number = current_funcdef_number;
  fde->nothrow = current_function_nothrow;
  fde->uses_eh_lsda = cfun->uses_eh_lsda;

  args_size = old_args_size = 0;

  /* We only want to output line number information for the genuine
     dwarf2 prologue case, not the eh frame case.  */
#ifdef DWARF2_DEBUGGING_INFO
  if (file)
    dwarf2out_source_line (line, file);
#endif
}

/* Output a marker (i.e. a label) for the absolute end of the generated code
   for a function definition.  This gets called *after* the epilogue code has
   been generated.  */

void
dwarf2out_end_epilogue ()
{
  dw_fde_ref fde;
  char label[MAX_ARTIFICIAL_LABEL_BYTES];

  /* Output a label to mark the endpoint of the code generated for this
     function.        */
  ASM_GENERATE_INTERNAL_LABEL (label, FUNC_END_LABEL, current_funcdef_number);
  ASM_OUTPUT_LABEL (asm_out_file, label);
  fde = &fde_table[fde_table_in_use - 1];
  fde->dw_fde_end = xstrdup (label);
}

void
dwarf2out_frame_init ()
{
  /* Allocate the initial hunk of the fde_table.  */
  fde_table = (dw_fde_ref) xcalloc (FDE_TABLE_INCREMENT, sizeof (dw_fde_node));
  fde_table_allocated = FDE_TABLE_INCREMENT;
  fde_table_in_use = 0;

  /* Generate the CFA instructions common to all FDE's.  Do it now for the
     sake of lookup_cfa.  */

#ifdef DWARF2_UNWIND_INFO
  /* On entry, the Canonical Frame Address is at SP.  */
  dwarf2out_def_cfa (NULL, STACK_POINTER_REGNUM, INCOMING_FRAME_SP_OFFSET);
  initial_return_save (INCOMING_RETURN_ADDR_RTX);
#endif
}

void
dwarf2out_frame_finish ()
{
  /* Output call frame information.  */
#ifdef MIPS_DEBUGGING_INFO
  if (write_symbols == DWARF2_DEBUG)
    output_call_frame_info (0);
  if (flag_unwind_tables || (flag_exceptions && ! USING_SJLJ_EXCEPTIONS))
    output_call_frame_info (1);
#else
  if (write_symbols == DWARF2_DEBUG
      || flag_unwind_tables || (flag_exceptions && ! USING_SJLJ_EXCEPTIONS))
    output_call_frame_info (1);
#endif
}

/* And now, the subset of the debugging information support code necessary
   for emitting location expressions.  */

typedef struct dw_val_struct *dw_val_ref;
typedef struct die_struct *dw_die_ref;
typedef struct dw_loc_descr_struct *dw_loc_descr_ref;
typedef struct dw_loc_list_struct *dw_loc_list_ref;

/* Each DIE may have a series of attribute/value pairs.  Values
   can take on several forms.  The forms that are used in this
   implementation are listed below.  */

typedef enum
{
  dw_val_class_addr,
  dw_val_class_offset,
  dw_val_class_loc,
  dw_val_class_loc_list,
  dw_val_class_const,
  dw_val_class_unsigned_const,
  dw_val_class_long_long,
  dw_val_class_float,
  dw_val_class_flag,
  dw_val_class_die_ref,
  dw_val_class_fde_ref,
  dw_val_class_lbl_id,
  dw_val_class_lbl_offset,
  dw_val_class_str
}
dw_val_class;

/* Describe a double word constant value.  */
/* ??? Every instance of long_long in the code really means CONST_DOUBLE.  */

typedef struct dw_long_long_struct
{
  unsigned long hi;
  unsigned long low;
}
dw_long_long_const;

/* Describe a floating point constant value.  */

typedef struct dw_fp_struct
{
  long *array;
  unsigned length;
}
dw_float_const;

/* The dw_val_node describes an attribute's value, as it is
   represented internally.  */

typedef struct dw_val_struct
{
  dw_val_class val_class;
  union
    {
      rtx val_addr;
      long unsigned val_offset;
      dw_loc_list_ref  val_loc_list;
      dw_loc_descr_ref val_loc;
      long int val_int;
      long unsigned val_unsigned;
      dw_long_long_const val_long_long;
      dw_float_const val_float;
      struct {
	dw_die_ref die;
	int external;
      } val_die_ref;
      unsigned val_fde_index;
      char *val_str;
      char *val_lbl_id;
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
  int dw_loc_addr;
}
dw_loc_descr_node;

/* Location lists are ranges + location descriptions for that range,
   so you can track variables that are in different places over
   their entire life.  */
typedef struct dw_loc_list_struct
{
  dw_loc_list_ref dw_loc_next;
  const char *begin; /* Label for begin address of range */
  const char *end;  /* Label for end address of range */
  char *ll_symbol; /* Label for beginning of location list. Only on head of list */
  const char *section; /* Section this loclist is relative to */
  dw_loc_descr_ref expr;
} dw_loc_list_node;

static const char *dwarf_stack_op_name	PARAMS ((unsigned));
static dw_loc_descr_ref new_loc_descr	PARAMS ((enum dwarf_location_atom,
						 unsigned long,
						 unsigned long));
static void add_loc_descr		PARAMS ((dw_loc_descr_ref *,
						 dw_loc_descr_ref));
static unsigned long size_of_loc_descr	PARAMS ((dw_loc_descr_ref));
static unsigned long size_of_locs	PARAMS ((dw_loc_descr_ref));
static void output_loc_operands		PARAMS ((dw_loc_descr_ref));
static void output_loc_sequence		PARAMS ((dw_loc_descr_ref));

/* Convert a DWARF stack opcode into its string name.  */

static const char *
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

/* Return a pointer to a newly allocated location description.  Location
   descriptions are simple expression terms that can be strung
   together to form more complicated location (address) descriptions.  */

static inline dw_loc_descr_ref
new_loc_descr (op, oprnd1, oprnd2)
     register enum dwarf_location_atom op;
     register unsigned long oprnd1;
     register unsigned long oprnd2;
{
  /* Use xcalloc here so we clear out all of the long_long constant in
     the union.  */
  register dw_loc_descr_ref descr
    = (dw_loc_descr_ref) xcalloc (1, sizeof (dw_loc_descr_node));

  descr->dw_loc_opc = op;
  descr->dw_loc_oprnd1.val_class = dw_val_class_unsigned_const;
  descr->dw_loc_oprnd1.v.val_unsigned = oprnd1;
  descr->dw_loc_oprnd2.val_class = dw_val_class_unsigned_const;
  descr->dw_loc_oprnd2.v.val_unsigned = oprnd2;

  return descr;
}


/* Add a location description term to a location description expression.  */

static inline void
add_loc_descr (list_head, descr)
     register dw_loc_descr_ref *list_head;
     register dw_loc_descr_ref descr;
{
  register dw_loc_descr_ref *d;

  /* Find the end of the chain.  */
  for (d = list_head; (*d) != NULL; d = &(*d)->dw_loc_next)
    ;

  *d = descr;
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
      size += DWARF2_ADDR_SIZE;
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

/* Return the size of a series of location descriptors.  */

static unsigned long
size_of_locs (loc)
     register dw_loc_descr_ref loc;
{
  register unsigned long size = 0;

  for (; loc != NULL; loc = loc->dw_loc_next)
    {
      loc->dw_loc_addr = size;
      size += size_of_loc_descr (loc);
    }

  return size;
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
#ifdef DWARF2_DEBUGGING_INFO
    case DW_OP_addr:
      dw2_asm_output_addr_rtx (DWARF2_ADDR_SIZE, val1->v.val_addr, NULL);
      break;
    case DW_OP_const2u:
    case DW_OP_const2s:
      dw2_asm_output_data (2, val1->v.val_int, NULL);
      break;
    case DW_OP_const4u:
    case DW_OP_const4s:
      dw2_asm_output_data (4, val1->v.val_int, NULL);
      break;
    case DW_OP_const8u:
    case DW_OP_const8s:
      if (HOST_BITS_PER_LONG < 64)
	abort ();
      dw2_asm_output_data (8, val1->v.val_int, NULL);
      break;
    case DW_OP_skip:
    case DW_OP_bra:
      {
	int offset;

	if (val1->val_class == dw_val_class_loc)
	  offset = val1->v.val_loc->dw_loc_addr - (loc->dw_loc_addr + 3);
	else
	  abort ();

	dw2_asm_output_data (2, offset, NULL);
      }
      break;
#else
    case DW_OP_addr:
    case DW_OP_const2u:
    case DW_OP_const2s:
    case DW_OP_const4u:
    case DW_OP_const4s:
    case DW_OP_const8u:
    case DW_OP_const8s:
    case DW_OP_skip:
    case DW_OP_bra:
      /* We currently don't make any attempt to make sure these are
         aligned properly like we do for the main unwind info, so
         don't support emitting things larger than a byte if we're
         only doing unwinding.  */
      abort ();
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
      dw2_asm_output_data_uleb128 (val1->v.val_unsigned, NULL);
      break;
    case DW_OP_fbreg:
      dw2_asm_output_data_sleb128 (val1->v.val_int, NULL);
      break;
    case DW_OP_bregx:
      dw2_asm_output_data_uleb128 (val1->v.val_unsigned, NULL);
      dw2_asm_output_data_sleb128 (val2->v.val_int, NULL);
      break;
    case DW_OP_piece:
      dw2_asm_output_data_uleb128 (val1->v.val_unsigned, NULL);
      break;
    case DW_OP_deref_size:
    case DW_OP_xderef_size:
      dw2_asm_output_data (1, val1->v.val_int, NULL);
      break;
    default:
      /* Other codes have no operands.  */
      break;
    }
}

/* Output a sequence of location operations.  */

static void
output_loc_sequence (loc)
     dw_loc_descr_ref loc;
{
  for (; loc != NULL; loc = loc->dw_loc_next)
    {
      /* Output the opcode.  */
      dw2_asm_output_data (1, loc->dw_loc_opc,
			   "%s", dwarf_stack_op_name (loc->dw_loc_opc));

      /* Output the operand(s) (if any).  */
      output_loc_operands (loc);
    }
}

/* This routine will generate the correct assembly data for a location
   description based on a cfi entry with a complex address.  */

static void
output_cfa_loc (cfi)
     dw_cfi_ref cfi;
{
  dw_loc_descr_ref loc;
  unsigned long size;

  /* Output the size of the block.  */
  loc = cfi->dw_cfi_oprnd1.dw_cfi_loc;
  size = size_of_locs (loc);
  dw2_asm_output_data_uleb128 (size, NULL);

  /* Now output the operations themselves.  */
  output_loc_sequence (loc);
}

/* This function builds a dwarf location descriptor sequence from
   a dw_cfa_location.  */

static struct dw_loc_descr_struct *
build_cfa_loc (cfa)
     dw_cfa_location *cfa;
{
  struct dw_loc_descr_struct *head, *tmp;

  if (cfa->indirect == 0)
    abort ();

  if (cfa->base_offset)
    {
      if (cfa->reg <= 31)
	head = new_loc_descr (DW_OP_breg0 + cfa->reg, cfa->base_offset, 0);
      else
	head = new_loc_descr (DW_OP_bregx, cfa->reg, cfa->base_offset);
    }
  else if (cfa->reg <= 31)
    head = new_loc_descr (DW_OP_reg0 + cfa->reg, 0, 0);
  else
    head = new_loc_descr (DW_OP_regx, cfa->reg, 0);
  head->dw_loc_oprnd1.val_class = dw_val_class_const;
  tmp = new_loc_descr (DW_OP_deref, 0, 0);
  add_loc_descr (&head, tmp);
  if (cfa->offset != 0)
    {
      tmp = new_loc_descr (DW_OP_plus_uconst, cfa->offset, 0);
      add_loc_descr (&head, tmp);
    }
  return head;
}

/* This function fills in aa dw_cfa_location structure from a
   dwarf location descriptor sequence.  */

static void
get_cfa_from_loc_descr (cfa, loc)
     dw_cfa_location *cfa;
     struct dw_loc_descr_struct *loc;
{
  struct dw_loc_descr_struct *ptr;
  cfa->offset = 0;
  cfa->base_offset = 0;
  cfa->indirect = 0;
  cfa->reg = -1;

  for (ptr = loc; ptr != NULL; ptr = ptr->dw_loc_next)
    {
      enum dwarf_location_atom op = ptr->dw_loc_opc;
      switch (op)
	{
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
	  cfa->reg = op - DW_OP_reg0;
	  break;
	case DW_OP_regx:
	  cfa->reg = ptr->dw_loc_oprnd1.v.val_int;
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
	  cfa->reg = op - DW_OP_breg0;
	  cfa->base_offset = ptr->dw_loc_oprnd1.v.val_int;
	  break;
	case DW_OP_bregx:
	  cfa->reg = ptr->dw_loc_oprnd1.v.val_int;
	  cfa->base_offset = ptr->dw_loc_oprnd2.v.val_int;
	  break;
	case DW_OP_deref:
	  cfa->indirect = 1;
	  break;
	case DW_OP_plus_uconst:
	  cfa->offset = ptr->dw_loc_oprnd1.v.val_unsigned;
	  break;
	default:
	  internal_error ("DW_LOC_OP %s not implememnted\n",
			  dwarf_stack_op_name (ptr->dw_loc_opc));
	}
    }
}
#endif /* .debug_frame support */

/* And now, the support for symbolic debugging information.  */
#ifdef DWARF2_DEBUGGING_INFO

static void dwarf2out_init 		PARAMS ((const char *));
static void dwarf2out_finish		PARAMS ((const char *));
static void dwarf2out_define	        PARAMS ((unsigned int, const char *));
static void dwarf2out_undef	        PARAMS ((unsigned int, const char *));
static void dwarf2out_start_source_file	PARAMS ((unsigned, const char *));
static void dwarf2out_end_source_file	PARAMS ((unsigned));
static void dwarf2out_begin_block	PARAMS ((unsigned, unsigned));
static void dwarf2out_end_block		PARAMS ((unsigned, unsigned));
static bool dwarf2out_ignore_block	PARAMS ((tree));
static void dwarf2out_global_decl	PARAMS ((tree));
static void dwarf2out_abstract_function PARAMS ((tree));

/* The debug hooks structure.  */

struct gcc_debug_hooks dwarf2_debug_hooks =
{
  dwarf2out_init,
  dwarf2out_finish,
  dwarf2out_define,
  dwarf2out_undef,
  dwarf2out_start_source_file,
  dwarf2out_end_source_file,
  dwarf2out_begin_block,
  dwarf2out_end_block,
  dwarf2out_ignore_block,
  dwarf2out_source_line,
  dwarf2out_begin_prologue,
  debug_nothing_int,		/* end_prologue */
  dwarf2out_end_epilogue,
  debug_nothing_tree,		/* begin_function */
  debug_nothing_int,		/* end_function */
  dwarf2out_decl,		/* function_decl */
  dwarf2out_global_decl,
  debug_nothing_tree,		/* deferred_inline_function */
  /* The DWARF 2 backend tries to reduce debugging bloat by not
     emitting the abstract description of inline functions until
     something tries to reference them.  */
  dwarf2out_abstract_function,	/* outlining_inline_function */
  debug_nothing_rtx		/* label */
};

/* NOTE: In the comments in this file, many references are made to
   "Debugging Information Entries".  This term is abbreviated as `DIE'
   throughout the remainder of this file.  */

/* An internal representation of the DWARF output is built, and then
   walked to generate the DWARF debugging info.  The walk of the internal
   representation is done after the entire program has been compiled.
   The types below are used to describe the internal representation.  */

/* Various DIE's use offsets relative to the beginning of the
   .debug_info section to refer to each other.  */

typedef long int dw_offset;

/* Define typedefs here to avoid circular dependencies.  */

typedef struct dw_attr_struct *dw_attr_ref;
typedef struct dw_line_info_struct *dw_line_info_ref;
typedef struct dw_separate_line_info_struct *dw_separate_line_info_ref;
typedef struct pubname_struct *pubname_ref;
typedef struct dw_ranges_struct *dw_ranges_ref;

/* Each entry in the line_info_table maintains the file and
   line number associated with the label generated for that
   entry.  The label gives the PC value associated with
   the line number entry.  */

typedef struct dw_line_info_struct
{
  unsigned long dw_file_num;
  unsigned long dw_line_num;
}
dw_line_info_entry;

/* Line information for functions in separate sections; each one gets its
   own sequence.  */
typedef struct dw_separate_line_info_struct
{
  unsigned long dw_file_num;
  unsigned long dw_line_num;
  unsigned long function;
}
dw_separate_line_info_entry;

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

/* The Debugging Information Entry (DIE) structure */

typedef struct die_struct
{
  enum dwarf_tag die_tag;
  char *die_symbol;
  dw_attr_ref die_attr;
  dw_die_ref die_parent;
  dw_die_ref die_child;
  dw_die_ref die_sib;
  dw_offset die_offset;
  unsigned long die_abbrev;
  int die_mark;
}
die_node;

/* The pubname structure */

typedef struct pubname_struct
{
  dw_die_ref die;
  char *name;
}
pubname_entry;

struct dw_ranges_struct
{
  int block_num;
};

/* The limbo die list structure.  */
typedef struct limbo_die_struct
{
  dw_die_ref die;
  struct limbo_die_struct *next;
}
limbo_die_node;

/* How to start an assembler comment.  */
#ifndef ASM_COMMENT_START
#define ASM_COMMENT_START ";#"
#endif

/* Define a macro which returns non-zero for a TYPE_DECL which was
   implicitly generated for a tagged type.

   Note that unlike the gcc front end (which generates a NULL named
   TYPE_DECL node for each complete tagged type, each array type, and
   each function type node created) the g++ front end generates a
   _named_ TYPE_DECL node for each tagged type node created.
   These TYPE_DECLs have DECL_ARTIFICIAL set, so we know not to
   generate a DW_TAG_typedef DIE for them.  */

#define TYPE_DECL_IS_STUB(decl)				\
  (DECL_NAME (decl) == NULL_TREE			\
   || (DECL_ARTIFICIAL (decl)				\
       && is_tagged_type (TREE_TYPE (decl))		\
       && ((decl == TYPE_STUB_DECL (TREE_TYPE (decl)))	\
	   /* This is necessary for stub decls that	\
	      appear in nested inline functions.  */	\
	   || (DECL_ABSTRACT_ORIGIN (decl) != NULL_TREE	\
	       && (decl_ultimate_origin (decl)		\
		   == TYPE_STUB_DECL (TREE_TYPE (decl)))))))

/* Information concerning the compilation unit's programming
   language, and compiler version.  */

extern int flag_traditional;

/* Fixed size portion of the DWARF compilation unit header.  */
#define DWARF_COMPILE_UNIT_HEADER_SIZE (2 * DWARF_OFFSET_SIZE + 3)

/* Fixed size portion of debugging line information prolog.  */
#define DWARF_LINE_PROLOG_HEADER_SIZE 5

/* Fixed size portion of public names info.  */
#define DWARF_PUBNAMES_HEADER_SIZE (2 * DWARF_OFFSET_SIZE + 2)

/* Fixed size portion of the address range info.  */
#define DWARF_ARANGES_HEADER_SIZE					\
  (DWARF_ROUND (2 * DWARF_OFFSET_SIZE + 4, DWARF2_ADDR_SIZE * 2)	\
   - DWARF_OFFSET_SIZE)

/* Size of padding portion in the address range info.  It must be
   aligned to twice the pointer size.  */
#define DWARF_ARANGES_PAD_SIZE \
  (DWARF_ROUND (2 * DWARF_OFFSET_SIZE + 4, DWARF2_ADDR_SIZE * 2) \
   - (2 * DWARF_OFFSET_SIZE + 4))

/* Use assembler line directives if available.  */
#ifndef DWARF2_ASM_LINE_DEBUG_INFO
#ifdef HAVE_AS_DWARF2_DEBUG_LINE
#define DWARF2_ASM_LINE_DEBUG_INFO 1
#else
#define DWARF2_ASM_LINE_DEBUG_INFO 0
#endif
#endif

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

/* Record the root of the DIE's built for the current compilation unit.  */
static dw_die_ref comp_unit_die;

/* A list of DIEs with a NULL parent waiting to be relocated.  */
static limbo_die_node *limbo_die_list = 0;

/* Structure used by lookup_filename to manage sets of filenames.  */
struct file_table
{
  char **table;
  unsigned allocated;
  unsigned in_use;
  unsigned last_lookup_index;
};

/* Size (in elements) of increments by which we may expand the filename
   table.  */
#define FILE_TABLE_INCREMENT 64

/* Filenames referenced by this compilation unit.  */
static struct file_table file_table;

/* Local pointer to the name of the main input file.  Initialized in
   dwarf2out_init.  */
static const char *primary_filename;

/* A pointer to the base of a table of references to DIE's that describe
   declarations.  The table is indexed by DECL_UID() which is a unique
   number identifying each decl.  */
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
static int decl_scope_table_allocated;

/* Current level of nesting of declaration scopes.  */
static int decl_scope_depth;

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
   for each source code line in .text in the compilation unit.  */
static dw_line_info_ref line_info_table;

/* Number of elements currently allocated for line_info_table.  */
static unsigned line_info_table_allocated;

/* Number of elements in separate_line_info_table currently in use.  */
static unsigned separate_line_info_table_in_use;

/* A pointer to the base of a table that contains line information
   for each source code line outside of .text in the compilation unit.  */
static dw_separate_line_info_ref separate_line_info_table;

/* Number of elements currently allocated for separate_line_info_table.  */
static unsigned separate_line_info_table_allocated;

/* Number of elements in line_info_table currently in use.  */
static unsigned line_info_table_in_use;

/* Size (in elements) of increments by which we may expand the
   line_info_table.  */
#define LINE_INFO_TABLE_INCREMENT 1024

/* A pointer to the base of a table that contains a list of publicly
   accessible names.  */
static pubname_ref pubname_table;

/* Number of elements currently allocated for pubname_table.  */
static unsigned pubname_table_allocated;

/* Number of elements in pubname_table currently in use.  */
static unsigned pubname_table_in_use;

/* Size (in elements) of increments by which we may expand the
   pubname_table.  */
#define PUBNAME_TABLE_INCREMENT 64

/* Array of dies for which we should generate .debug_arange info.  */
static dw_die_ref *arange_table;

/* Number of elements currently allocated for arange_table.  */
static unsigned arange_table_allocated;

/* Number of elements in arange_table currently in use.  */
static unsigned arange_table_in_use;

/* Size (in elements) of increments by which we may expand the
   arange_table.  */
#define ARANGE_TABLE_INCREMENT 64

/* Array of dies for which we should generate .debug_ranges info.  */
static dw_ranges_ref ranges_table;

/* Number of elements currently allocated for ranges_table.  */
static unsigned ranges_table_allocated;

/* Number of elements in ranges_table currently in use.  */
static unsigned ranges_table_in_use;

/* Size (in elements) of increments by which we may expand the
   ranges_table.  */
#define RANGES_TABLE_INCREMENT 64

/* Whether we have location lists that need outputting */
static unsigned have_location_lists;

/* A pointer to the base of a list of incomplete types which might be
   completed at some later time.  */

static tree *incomplete_types_list;

/* Number of elements currently allocated for the incomplete_types_list.  */
static unsigned incomplete_types_allocated;

/* Number of elements of incomplete_types_list currently in use.  */
static unsigned incomplete_types;

/* Size (in elements) of increments by which we may expand the incomplete
   types list.  Actually, a single hunk of space of this size should
   be enough for most typical programs.	 */
#define INCOMPLETE_TYPES_INCREMENT 64

/* Record whether the function being analyzed contains inlined functions.  */
static int current_function_has_inlines;
#if 0 && defined (MIPS_DEBUGGING_INFO)
static int comp_unit_has_inlines;
#endif

/* Array of RTXes referenced by the debugging information, which therefore
   must be kept around forever.  We do this rather than perform GC on
   the dwarf info because almost all of the dwarf info lives forever, and
   it's easier to support non-GC frontends this way.  */
static varray_type used_rtx_varray;

/* Forward declarations for functions defined in this file.  */

static int is_pseudo_reg		PARAMS ((rtx));
static tree type_main_variant		PARAMS ((tree));
static int is_tagged_type		PARAMS ((tree));
static const char *dwarf_tag_name	PARAMS ((unsigned));
static const char *dwarf_attr_name	PARAMS ((unsigned));
static const char *dwarf_form_name	PARAMS ((unsigned));
#if 0
static const char *dwarf_type_encoding_name PARAMS ((unsigned));
#endif
static tree decl_ultimate_origin	PARAMS ((tree));
static tree block_ultimate_origin	PARAMS ((tree));
static tree decl_class_context		PARAMS ((tree));
static void add_dwarf_attr		PARAMS ((dw_die_ref, dw_attr_ref));
static void add_AT_flag			PARAMS ((dw_die_ref,
						 enum dwarf_attribute,
						 unsigned));
static void add_AT_int			PARAMS ((dw_die_ref,
						 enum dwarf_attribute, long));
static void add_AT_unsigned		PARAMS ((dw_die_ref,
						 enum dwarf_attribute,
						 unsigned long));
static void add_AT_long_long		PARAMS ((dw_die_ref,
						 enum dwarf_attribute,
						 unsigned long,
						 unsigned long));
static void add_AT_float		PARAMS ((dw_die_ref,
						 enum dwarf_attribute,
						 unsigned, long *));
static void add_AT_string		PARAMS ((dw_die_ref,
						 enum dwarf_attribute,
						 const char *));
static void add_AT_die_ref		PARAMS ((dw_die_ref,
						 enum dwarf_attribute,
						 dw_die_ref));
static void add_AT_fde_ref		PARAMS ((dw_die_ref,
						 enum dwarf_attribute,
						 unsigned));
static void add_AT_loc			PARAMS ((dw_die_ref,
						 enum dwarf_attribute,
						 dw_loc_descr_ref));
static void add_AT_loc_list		PARAMS ((dw_die_ref,
						 enum dwarf_attribute,
						 dw_loc_list_ref));
static void add_AT_addr			PARAMS ((dw_die_ref,
						 enum dwarf_attribute,
						 rtx));
static void add_AT_lbl_id		PARAMS ((dw_die_ref,
						 enum dwarf_attribute,
						 const char *));
static void add_AT_lbl_offset		PARAMS ((dw_die_ref,
						 enum dwarf_attribute,
						 const char *));
static void add_AT_offset		PARAMS ((dw_die_ref,
						 enum dwarf_attribute,
						 unsigned long));
static dw_attr_ref get_AT		PARAMS ((dw_die_ref,
						 enum dwarf_attribute));
static const char *get_AT_low_pc	PARAMS ((dw_die_ref));
static const char *get_AT_hi_pc		PARAMS ((dw_die_ref));
static const char *get_AT_string	PARAMS ((dw_die_ref,
						 enum dwarf_attribute));
static int get_AT_flag			PARAMS ((dw_die_ref,
						 enum dwarf_attribute));
static unsigned get_AT_unsigned		PARAMS ((dw_die_ref,
						 enum dwarf_attribute));
static inline dw_die_ref get_AT_ref 	PARAMS ((dw_die_ref,
						 enum dwarf_attribute));
static int is_c_family			PARAMS ((void));
static int is_java			PARAMS ((void));
static int is_fortran			PARAMS ((void));
static void remove_AT			PARAMS ((dw_die_ref,
						 enum dwarf_attribute));
static void remove_children		PARAMS ((dw_die_ref));
static void add_child_die		PARAMS ((dw_die_ref, dw_die_ref));
static dw_die_ref new_die		PARAMS ((enum dwarf_tag, dw_die_ref));
static dw_die_ref lookup_type_die	PARAMS ((tree));
static void equate_type_number_to_die	PARAMS ((tree, dw_die_ref));
static dw_die_ref lookup_decl_die	PARAMS ((tree));
static void equate_decl_number_to_die	PARAMS ((tree, dw_die_ref));
static void print_spaces		PARAMS ((FILE *));
static void print_die			PARAMS ((dw_die_ref, FILE *));
static void print_dwarf_line_table	PARAMS ((FILE *));
static void reverse_die_lists		PARAMS ((dw_die_ref));
static void reverse_all_dies		PARAMS ((dw_die_ref));
static dw_die_ref push_new_compile_unit PARAMS ((dw_die_ref, dw_die_ref));
static dw_die_ref pop_compile_unit	PARAMS ((dw_die_ref));
static void loc_checksum	 PARAMS ((dw_loc_descr_ref, struct md5_ctx *));
static void attr_checksum 	      PARAMS ((dw_attr_ref, struct md5_ctx *));
static void die_checksum	       PARAMS ((dw_die_ref, struct md5_ctx *));
static void compute_section_prefix	PARAMS ((dw_die_ref));
static int is_type_die			PARAMS ((dw_die_ref));
static int is_comdat_die 		PARAMS ((dw_die_ref));
static int is_symbol_die 		PARAMS ((dw_die_ref));
static void assign_symbol_names		PARAMS ((dw_die_ref));
static void break_out_includes		PARAMS ((dw_die_ref));
static void add_sibling_attributes	PARAMS ((dw_die_ref));
static void build_abbrev_table		PARAMS ((dw_die_ref));
static void output_location_lists   	PARAMS ((dw_die_ref));
static unsigned long size_of_string	PARAMS ((const char *));
static int constant_size		PARAMS ((long unsigned));
static unsigned long size_of_die	PARAMS ((dw_die_ref));
static void calc_die_sizes		PARAMS ((dw_die_ref));
static void mark_dies			PARAMS ((dw_die_ref));
static void unmark_dies			PARAMS ((dw_die_ref));
static unsigned long size_of_pubnames	PARAMS ((void));
static unsigned long size_of_aranges	PARAMS ((void));
static enum dwarf_form value_format	PARAMS ((dw_attr_ref));
static void output_value_format		PARAMS ((dw_attr_ref));
static void output_abbrev_section	PARAMS ((void));
static void output_die_symbol		PARAMS ((dw_die_ref));
static void output_die			PARAMS ((dw_die_ref));
static void output_compilation_unit_header PARAMS ((void));
static void output_comp_unit		PARAMS ((dw_die_ref));
static const char *dwarf2_name		PARAMS ((tree, int));
static void add_pubname			PARAMS ((tree, dw_die_ref));
static void output_pubnames		PARAMS ((void));
static void add_arange			PARAMS ((tree, dw_die_ref));
static void output_aranges		PARAMS ((void));
static unsigned int add_ranges		PARAMS ((tree));
static void output_ranges		PARAMS ((void));
static void output_line_info		PARAMS ((void));
static void output_file_names           PARAMS ((void));
static dw_die_ref base_type_die		PARAMS ((tree));
static tree root_type			PARAMS ((tree));
static int is_base_type			PARAMS ((tree));
static dw_die_ref modified_type_die	PARAMS ((tree, int, int, dw_die_ref));
static int type_is_enum			PARAMS ((tree));
static unsigned int reg_number		PARAMS ((rtx));
static dw_loc_descr_ref reg_loc_descriptor PARAMS ((rtx));
static dw_loc_descr_ref int_loc_descriptor PARAMS ((HOST_WIDE_INT));
static dw_loc_descr_ref based_loc_descr	PARAMS ((unsigned, long));
static int is_based_loc			PARAMS ((rtx));
static dw_loc_descr_ref mem_loc_descriptor PARAMS ((rtx, enum machine_mode mode));
static dw_loc_descr_ref concat_loc_descriptor PARAMS ((rtx, rtx));
static dw_loc_descr_ref loc_descriptor	PARAMS ((rtx));
static dw_loc_descr_ref loc_descriptor_from_tree PARAMS ((tree, int));
static HOST_WIDE_INT ceiling		PARAMS ((HOST_WIDE_INT, unsigned int));
static tree field_type			PARAMS ((tree));
static unsigned int simple_type_align_in_bits PARAMS ((tree));
static unsigned int simple_decl_align_in_bits PARAMS ((tree));
static unsigned HOST_WIDE_INT simple_type_size_in_bits PARAMS ((tree));
static HOST_WIDE_INT field_byte_offset	PARAMS ((tree));
static void add_AT_location_description	PARAMS ((dw_die_ref,
						 enum dwarf_attribute, rtx));
static void add_data_member_location_attribute PARAMS ((dw_die_ref, tree));
static void add_const_value_attribute	PARAMS ((dw_die_ref, rtx));
static rtx rtl_for_decl_location	PARAMS ((tree));
static void add_location_or_const_value_attribute PARAMS ((dw_die_ref, tree));
static void tree_add_const_value_attribute PARAMS ((dw_die_ref, tree));
static void add_name_attribute		PARAMS ((dw_die_ref, const char *));
static void add_bound_info		PARAMS ((dw_die_ref,
						 enum dwarf_attribute, tree));
static void add_subscript_info		PARAMS ((dw_die_ref, tree));
static void add_byte_size_attribute	PARAMS ((dw_die_ref, tree));
static void add_bit_offset_attribute	PARAMS ((dw_die_ref, tree));
static void add_bit_size_attribute	PARAMS ((dw_die_ref, tree));
static void add_prototyped_attribute	PARAMS ((dw_die_ref, tree));
static void add_abstract_origin_attribute PARAMS ((dw_die_ref, tree));
static void add_pure_or_virtual_attribute PARAMS ((dw_die_ref, tree));
static void add_src_coords_attributes	PARAMS ((dw_die_ref, tree));
static void add_name_and_src_coords_attributes PARAMS ((dw_die_ref, tree));
static void push_decl_scope		PARAMS ((tree));
static dw_die_ref scope_die_for		PARAMS ((tree, dw_die_ref));
static void pop_decl_scope		PARAMS ((void));
static void add_type_attribute		PARAMS ((dw_die_ref, tree, int, int,
						 dw_die_ref));
static const char *type_tag	        PARAMS ((tree));
static tree member_declared_type	PARAMS ((tree));
#if 0
static const char *decl_start_label	PARAMS ((tree));
#endif
static void gen_array_type_die		PARAMS ((tree, dw_die_ref));
static void gen_set_type_die		PARAMS ((tree, dw_die_ref));
#if 0
static void gen_entry_point_die		PARAMS ((tree, dw_die_ref));
#endif
static void gen_inlined_enumeration_type_die PARAMS ((tree, dw_die_ref));
static void gen_inlined_structure_type_die PARAMS ((tree, dw_die_ref));
static void gen_inlined_union_type_die	PARAMS ((tree, dw_die_ref));
static void gen_enumeration_type_die	PARAMS ((tree, dw_die_ref));
static dw_die_ref gen_formal_parameter_die PARAMS ((tree, dw_die_ref));
static void gen_unspecified_parameters_die PARAMS ((tree, dw_die_ref));
static void gen_formal_types_die	PARAMS ((tree, dw_die_ref));
static void gen_subprogram_die		PARAMS ((tree, dw_die_ref));
static void gen_variable_die		PARAMS ((tree, dw_die_ref));
static void gen_label_die		PARAMS ((tree, dw_die_ref));
static void gen_lexical_block_die	PARAMS ((tree, dw_die_ref, int));
static void gen_inlined_subroutine_die	PARAMS ((tree, dw_die_ref, int));
static void gen_field_die		PARAMS ((tree, dw_die_ref));
static void gen_ptr_to_mbr_type_die	PARAMS ((tree, dw_die_ref));
static dw_die_ref gen_compile_unit_die	PARAMS ((const char *));
static void gen_string_type_die		PARAMS ((tree, dw_die_ref));
static void gen_inheritance_die		PARAMS ((tree, dw_die_ref));
static void gen_member_die		PARAMS ((tree, dw_die_ref));
static void gen_struct_or_union_type_die PARAMS ((tree, dw_die_ref));
static void gen_subroutine_type_die	PARAMS ((tree, dw_die_ref));
static void gen_typedef_die		PARAMS ((tree, dw_die_ref));
static void gen_type_die		PARAMS ((tree, dw_die_ref));
static void gen_tagged_type_instantiation_die PARAMS ((tree, dw_die_ref));
static void gen_block_die		PARAMS ((tree, dw_die_ref, int));
static void decls_for_scope		PARAMS ((tree, dw_die_ref, int));
static int is_redundant_typedef		PARAMS ((tree));
static void gen_decl_die		PARAMS ((tree, dw_die_ref));
static unsigned lookup_filename		PARAMS ((const char *));
static void init_file_table		PARAMS ((void));
static void add_incomplete_type		PARAMS ((tree));
static void retry_incomplete_types	PARAMS ((void));
static void gen_type_die_for_member	PARAMS ((tree, tree, dw_die_ref));
static rtx save_rtx			PARAMS ((rtx));
static void splice_child_die		PARAMS ((dw_die_ref, dw_die_ref));
static int file_info_cmp		PARAMS ((const void *, const void *));
static dw_loc_list_ref new_loc_list     PARAMS ((dw_loc_descr_ref, 
						 const char *, const char *,
						 const char *, unsigned));
static void add_loc_descr_to_loc_list   PARAMS ((dw_loc_list_ref *,
						 dw_loc_descr_ref,
						 const char *, const char *, const char *));
static void output_loc_list		PARAMS ((dw_loc_list_ref));
static char *gen_internal_sym 		PARAMS ((const char *));

/* Section names used to hold DWARF debugging information.  */
#ifndef DEBUG_INFO_SECTION
#define DEBUG_INFO_SECTION	".debug_info"
#endif
#ifndef DEBUG_ABBREV_SECTION
#define DEBUG_ABBREV_SECTION	".debug_abbrev"
#endif
#ifndef DEBUG_ARANGES_SECTION
#define DEBUG_ARANGES_SECTION	".debug_aranges"
#endif
#ifndef DEBUG_MACINFO_SECTION
#define DEBUG_MACINFO_SECTION	".debug_macinfo"
#endif
#ifndef DEBUG_LINE_SECTION
#define DEBUG_LINE_SECTION	".debug_line"
#endif
#ifndef DEBUG_LOC_SECTION
#define DEBUG_LOC_SECTION	".debug_loc"
#endif
#ifndef DEBUG_PUBNAMES_SECTION
#define DEBUG_PUBNAMES_SECTION	".debug_pubnames"
#endif
#ifndef DEBUG_STR_SECTION
#define DEBUG_STR_SECTION	".debug_str"
#endif
#ifndef DEBUG_RANGES_SECTION
#define DEBUG_RANGES_SECTION	".debug_ranges"
#endif

/* Standard ELF section names for compiled code and data.  */
#ifndef TEXT_SECTION
#define TEXT_SECTION		".text"
#endif
#ifndef DATA_SECTION
#define DATA_SECTION		".data"
#endif
#ifndef BSS_SECTION
#define BSS_SECTION		".bss"
#endif

/* Labels we insert at beginning sections we can reference instead of
   the section names themselves.  */

#ifndef TEXT_SECTION_LABEL
#define TEXT_SECTION_LABEL		"Ltext"
#endif
#ifndef DEBUG_LINE_SECTION_LABEL
#define DEBUG_LINE_SECTION_LABEL	"Ldebug_line"
#endif
#ifndef DEBUG_INFO_SECTION_LABEL
#define DEBUG_INFO_SECTION_LABEL	"Ldebug_info"
#endif
#ifndef DEBUG_ABBREV_SECTION_LABEL
#define DEBUG_ABBREV_SECTION_LABEL	"Ldebug_abbrev"
#endif
#ifndef DEBUG_LOC_SECTION_LABEL
#define DEBUG_LOC_SECTION_LABEL		"Ldebug_loc"
#endif
#ifndef DEBUG_MACINFO_SECTION_LABEL
#define DEBUG_MACINFO_SECTION_LABEL     "Ldebug_macinfo"
#endif

/* Definitions of defaults for formats and names of various special
   (artificial) labels which may be generated within this file (when the -g
   options is used and DWARF_DEBUGGING_INFO is in effect.
   If necessary, these may be overridden from within the tm.h file, but
   typically, overriding these defaults is unnecessary.  */

static char text_end_label[MAX_ARTIFICIAL_LABEL_BYTES];
static char text_section_label[MAX_ARTIFICIAL_LABEL_BYTES];
static char abbrev_section_label[MAX_ARTIFICIAL_LABEL_BYTES];
static char debug_info_section_label[MAX_ARTIFICIAL_LABEL_BYTES];
static char debug_line_section_label[MAX_ARTIFICIAL_LABEL_BYTES];
static char macinfo_section_label[MAX_ARTIFICIAL_LABEL_BYTES];
static char loc_section_label[MAX_ARTIFICIAL_LABEL_BYTES];
#ifndef TEXT_END_LABEL
#define TEXT_END_LABEL		"Letext"
#endif
#ifndef DATA_END_LABEL
#define DATA_END_LABEL		"Ledata"
#endif
#ifndef BSS_END_LABEL
#define BSS_END_LABEL           "Lebss"
#endif
#ifndef BLOCK_BEGIN_LABEL
#define BLOCK_BEGIN_LABEL	"LBB"
#endif
#ifndef BLOCK_END_LABEL
#define BLOCK_END_LABEL		"LBE"
#endif
#ifndef BODY_BEGIN_LABEL
#define BODY_BEGIN_LABEL	"Lbb"
#endif
#ifndef BODY_END_LABEL
#define BODY_END_LABEL		"Lbe"
#endif
#ifndef LINE_CODE_LABEL
#define LINE_CODE_LABEL		"LM"
#endif
#ifndef SEPARATE_LINE_CODE_LABEL
#define SEPARATE_LINE_CODE_LABEL	"LSM"
#endif

/* We allow a language front-end to designate a function that is to be
   called to "demangle" any name before it it put into a DIE.  */

static const char *(*demangle_name_func) PARAMS ((const char *));

void
dwarf2out_set_demangle_name_func (func)
     const char *(*func) PARAMS ((const char *));
{
  demangle_name_func = func;
}

/* Return an rtx like ORIG which lives forever.  If we're doing GC,
   that means adding it to used_rtx_varray.  If not, that means making
   a copy on the permanent_obstack.  */

static rtx
save_rtx (orig)
     register rtx orig;
{
  VARRAY_PUSH_RTX (used_rtx_varray, orig);

  return orig;
}

/* Test if rtl node points to a pseudo register.  */

static inline int
is_pseudo_reg (rtl)
     register rtx rtl;
{
  return ((GET_CODE (rtl) == REG && REGNO (rtl) >= FIRST_PSEUDO_REGISTER)
	  || (GET_CODE (rtl) == SUBREG
	      && REGNO (SUBREG_REG (rtl)) >= FIRST_PSEUDO_REGISTER));
}

/* Return a reference to a type, with its const and volatile qualifiers
   removed.  */

static inline tree
type_main_variant (type)
     register tree type;
{
  type = TYPE_MAIN_VARIANT (type);

  /* There really should be only one main variant among any group of variants
     of a given type (and all of the MAIN_VARIANT values for all members of
     the group should point to that one type) but sometimes the C front-end
     messes this up for array types, so we work around that bug here.  */

  if (TREE_CODE (type) == ARRAY_TYPE)
    while (type != TYPE_MAIN_VARIANT (type))
      type = TYPE_MAIN_VARIANT (type);

  return type;
}

/* Return non-zero if the given type node represents a tagged type.  */

static inline int
is_tagged_type (type)
     register tree type;
{
  register enum tree_code code = TREE_CODE (type);

  return (code == RECORD_TYPE || code == UNION_TYPE
	  || code == QUAL_UNION_TYPE || code == ENUMERAL_TYPE);
}

/* Convert a DIE tag into its string name.  */

static const char *
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
    case DW_TAG_GNU_BINCL:
      return "DW_TAG_GNU_BINCL";
    case DW_TAG_GNU_EINCL:
      return "DW_TAG_GNU_EINCL";
    default:
      return "DW_TAG_<unknown>";
    }
}

/* Convert a DWARF attribute code into its string name.  */

static const char *
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

    case DW_AT_allocated:
      return "DW_AT_allocated";
    case DW_AT_associated:
      return "DW_AT_associated";
    case DW_AT_data_location:
      return "DW_AT_data_location";
    case DW_AT_stride:
      return "DW_AT_stride";
    case DW_AT_entry_pc:
      return "DW_AT_entry_pc";
    case DW_AT_use_UTF8:
      return "DW_AT_use_UTF8";
    case DW_AT_extension:
      return "DW_AT_extension";
    case DW_AT_ranges:
      return "DW_AT_ranges";
    case DW_AT_trampoline:
      return "DW_AT_trampoline";
    case DW_AT_call_column:
      return "DW_AT_call_column";
    case DW_AT_call_file:
      return "DW_AT_call_file";
    case DW_AT_call_line:
      return "DW_AT_call_line";

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
    case DW_AT_MIPS_stride:
      return "DW_AT_MIPS_stride";
    case DW_AT_MIPS_abstract_name:
      return "DW_AT_MIPS_abstract_name";
    case DW_AT_MIPS_clone_origin:
      return "DW_AT_MIPS_clone_origin";
    case DW_AT_MIPS_has_inlines:
      return "DW_AT_MIPS_has_inlines";

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

static const char *
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

/* Convert a DWARF type code into its string name.  */

#if 0
static const char *
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
#endif

/* Determine the "ultimate origin" of a decl.  The decl may be an inlined
   instance of an inlined instance of a decl which is local to an inline
   function, so we have to trace all of the way back through the origin chain
   to find out what sort of node actually served as the original seed for the
   given block.  */

static tree
decl_ultimate_origin (decl)
     register tree decl;
{
  /* output_inline_function sets DECL_ABSTRACT_ORIGIN for all the
     nodes in the function to point to themselves; ignore that if
     we're trying to output the abstract instance of this function.  */
  if (DECL_ABSTRACT (decl) && DECL_ABSTRACT_ORIGIN (decl) == decl)
    return NULL_TREE;

#ifdef ENABLE_CHECKING
  if (DECL_FROM_INLINE (DECL_ORIGIN (decl)))
    /* Since the DECL_ABSTRACT_ORIGIN for a DECL is supposed to be the
       most distant ancestor, this should never happen.  */
    abort ();
#endif

  return DECL_ABSTRACT_ORIGIN (decl);
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

  /* output_inline_function sets BLOCK_ABSTRACT_ORIGIN for all the
     nodes in the function to point to themselves; ignore that if
     we're trying to output the abstract instance of this function.  */
  if (BLOCK_ABSTRACT (block) && immediate_origin == block)
    return NULL_TREE;

  if (immediate_origin == NULL_TREE)
    return NULL_TREE;
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

/* Get the class to which DECL belongs, if any.  In g++, the DECL_CONTEXT
   of a virtual function may refer to a base class, so we check the 'this'
   parameter.  */

static tree
decl_class_context (decl)
     tree decl;
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

/* Add an attribute/value pair to a DIE.  We build the lists up in reverse
   addition order, and correct that in reverse_all_dies.  */

static inline void
add_dwarf_attr (die, attr)
     register dw_die_ref die;
     register dw_attr_ref attr;
{
  if (die != NULL && attr != NULL)
    {
      attr->dw_attr_next = die->die_attr;
      die->die_attr = attr;
    }
}

static inline dw_val_class AT_class PARAMS ((dw_attr_ref));
static inline dw_val_class
AT_class (a)
     dw_attr_ref a;
{
  return a->dw_attr_val.val_class;
}

/* Add a flag value attribute to a DIE.  */

static inline void
add_AT_flag (die, attr_kind, flag)
     register dw_die_ref die;
     register enum dwarf_attribute attr_kind;
     register unsigned flag;
{
  register dw_attr_ref attr = (dw_attr_ref) xmalloc (sizeof (dw_attr_node));

  attr->dw_attr_next = NULL;
  attr->dw_attr = attr_kind;
  attr->dw_attr_val.val_class = dw_val_class_flag;
  attr->dw_attr_val.v.val_flag = flag;
  add_dwarf_attr (die, attr);
}

static inline unsigned AT_flag PARAMS ((dw_attr_ref));
static inline unsigned
AT_flag (a)
     register dw_attr_ref a;
{
  if (a && AT_class (a) == dw_val_class_flag)
    return a->dw_attr_val.v.val_flag;

  abort ();
}

/* Add a signed integer attribute value to a DIE.  */

static inline void
add_AT_int (die, attr_kind, int_val)
     register dw_die_ref die;
     register enum dwarf_attribute attr_kind;
     register long int int_val;
{
  register dw_attr_ref attr = (dw_attr_ref) xmalloc (sizeof (dw_attr_node));

  attr->dw_attr_next = NULL;
  attr->dw_attr = attr_kind;
  attr->dw_attr_val.val_class = dw_val_class_const;
  attr->dw_attr_val.v.val_int = int_val;
  add_dwarf_attr (die, attr);
}

static inline long int AT_int PARAMS ((dw_attr_ref));
static inline long int
AT_int (a)
     register dw_attr_ref a;
{
  if (a && AT_class (a) == dw_val_class_const)
    return a->dw_attr_val.v.val_int;

  abort ();
}

/* Add an unsigned integer attribute value to a DIE.  */

static inline void
add_AT_unsigned (die, attr_kind, unsigned_val)
     register dw_die_ref die;
     register enum dwarf_attribute attr_kind;
     register unsigned long unsigned_val;
{
  register dw_attr_ref attr = (dw_attr_ref) xmalloc (sizeof (dw_attr_node));

  attr->dw_attr_next = NULL;
  attr->dw_attr = attr_kind;
  attr->dw_attr_val.val_class = dw_val_class_unsigned_const;
  attr->dw_attr_val.v.val_unsigned = unsigned_val;
  add_dwarf_attr (die, attr);
}

static inline unsigned long AT_unsigned PARAMS ((dw_attr_ref));
static inline unsigned long
AT_unsigned (a)
     register dw_attr_ref a;
{
  if (a && AT_class (a) == dw_val_class_unsigned_const)
    return a->dw_attr_val.v.val_unsigned;

  abort ();
}

/* Add an unsigned double integer attribute value to a DIE.  */

static inline void
add_AT_long_long (die, attr_kind, val_hi, val_low)
     register dw_die_ref die;
     register enum dwarf_attribute attr_kind;
     register unsigned long val_hi;
     register unsigned long val_low;
{
  register dw_attr_ref attr = (dw_attr_ref) xmalloc (sizeof (dw_attr_node));

  attr->dw_attr_next = NULL;
  attr->dw_attr = attr_kind;
  attr->dw_attr_val.val_class = dw_val_class_long_long;
  attr->dw_attr_val.v.val_long_long.hi = val_hi;
  attr->dw_attr_val.v.val_long_long.low = val_low;
  add_dwarf_attr (die, attr);
}

/* Add a floating point attribute value to a DIE and return it.  */

static inline void
add_AT_float (die, attr_kind, length, array)
     register dw_die_ref die;
     register enum dwarf_attribute attr_kind;
     register unsigned length;
     register long *array;
{
  register dw_attr_ref attr = (dw_attr_ref) xmalloc (sizeof (dw_attr_node));

  attr->dw_attr_next = NULL;
  attr->dw_attr = attr_kind;
  attr->dw_attr_val.val_class = dw_val_class_float;
  attr->dw_attr_val.v.val_float.length = length;
  attr->dw_attr_val.v.val_float.array = array;
  add_dwarf_attr (die, attr);
}

/* Add a string attribute value to a DIE.  */

static inline void
add_AT_string (die, attr_kind, str)
     register dw_die_ref die;
     register enum dwarf_attribute attr_kind;
     register const char *str;
{
  register dw_attr_ref attr = (dw_attr_ref) xmalloc (sizeof (dw_attr_node));

  attr->dw_attr_next = NULL;
  attr->dw_attr = attr_kind;
  attr->dw_attr_val.val_class = dw_val_class_str;
  attr->dw_attr_val.v.val_str = xstrdup (str);
  add_dwarf_attr (die, attr);
}

static inline const char *AT_string PARAMS ((dw_attr_ref));
static inline const char *
AT_string (a)
     register dw_attr_ref a;
{
  if (a && AT_class (a) == dw_val_class_str)
    return a->dw_attr_val.v.val_str;

  abort ();
}

/* Add a DIE reference attribute value to a DIE.  */

static inline void
add_AT_die_ref (die, attr_kind, targ_die)
     register dw_die_ref die;
     register enum dwarf_attribute attr_kind;
     register dw_die_ref targ_die;
{
  register dw_attr_ref attr = (dw_attr_ref) xmalloc (sizeof (dw_attr_node));

  attr->dw_attr_next = NULL;
  attr->dw_attr = attr_kind;
  attr->dw_attr_val.val_class = dw_val_class_die_ref;
  attr->dw_attr_val.v.val_die_ref.die = targ_die;
  attr->dw_attr_val.v.val_die_ref.external = 0;
  add_dwarf_attr (die, attr);
}

static inline dw_die_ref AT_ref PARAMS ((dw_attr_ref));
static inline dw_die_ref
AT_ref (a)
     register dw_attr_ref a;
{
  if (a && AT_class (a) == dw_val_class_die_ref)
    return a->dw_attr_val.v.val_die_ref.die;

  abort ();
}

static inline int AT_ref_external PARAMS ((dw_attr_ref));
static inline int
AT_ref_external (a)
     register dw_attr_ref a;
{
  if (a && AT_class (a) == dw_val_class_die_ref)
    return a->dw_attr_val.v.val_die_ref.external;

  return 0;
}

static inline void set_AT_ref_external PARAMS ((dw_attr_ref, int));
static inline void
set_AT_ref_external (a, i)
     register dw_attr_ref a;
     int i;
{
  if (a && AT_class (a) == dw_val_class_die_ref)
    a->dw_attr_val.v.val_die_ref.external = i;
  else
    abort ();
}

/* Add an FDE reference attribute value to a DIE.  */

static inline void
add_AT_fde_ref (die, attr_kind, targ_fde)
     register dw_die_ref die;
     register enum dwarf_attribute attr_kind;
     register unsigned targ_fde;
{
  register dw_attr_ref attr = (dw_attr_ref) xmalloc (sizeof (dw_attr_node));

  attr->dw_attr_next = NULL;
  attr->dw_attr = attr_kind;
  attr->dw_attr_val.val_class = dw_val_class_fde_ref;
  attr->dw_attr_val.v.val_fde_index = targ_fde;
  add_dwarf_attr (die, attr);
}

/* Add a location description attribute value to a DIE.  */

static inline void
add_AT_loc (die, attr_kind, loc)
     register dw_die_ref die;
     register enum dwarf_attribute attr_kind;
     register dw_loc_descr_ref loc;
{
  register dw_attr_ref attr = (dw_attr_ref) xmalloc (sizeof (dw_attr_node));

  attr->dw_attr_next = NULL;
  attr->dw_attr = attr_kind;
  attr->dw_attr_val.val_class = dw_val_class_loc;
  attr->dw_attr_val.v.val_loc = loc;
  add_dwarf_attr (die, attr);
}

static inline dw_loc_descr_ref AT_loc PARAMS ((dw_attr_ref));
static inline dw_loc_descr_ref
AT_loc (a)
     register dw_attr_ref a;
{
  if (a && AT_class (a) == dw_val_class_loc)
    return a->dw_attr_val.v.val_loc;

  abort ();
}

static inline void
add_AT_loc_list (die, attr_kind, loc_list)
     register dw_die_ref die;
     register enum dwarf_attribute attr_kind;
     register dw_loc_list_ref loc_list;
{
  register dw_attr_ref attr = (dw_attr_ref) xmalloc (sizeof (dw_attr_node));

  attr->dw_attr_next = NULL;
  attr->dw_attr = attr_kind;
  attr->dw_attr_val.val_class = dw_val_class_loc_list;
  attr->dw_attr_val.v.val_loc_list = loc_list;
  add_dwarf_attr (die, attr);
  have_location_lists = 1;
}

static inline dw_loc_list_ref AT_loc_list PARAMS ((dw_attr_ref));

static inline dw_loc_list_ref
AT_loc_list (a)
     register dw_attr_ref a;
{
  if (a && AT_class (a) == dw_val_class_loc_list)
    return a->dw_attr_val.v.val_loc_list;

  abort ();
}

/* Add an address constant attribute value to a DIE.  */

static inline void
add_AT_addr (die, attr_kind, addr)
     register dw_die_ref die;
     register enum dwarf_attribute attr_kind;
     rtx addr;
{
  register dw_attr_ref attr = (dw_attr_ref) xmalloc (sizeof (dw_attr_node));

  attr->dw_attr_next = NULL;
  attr->dw_attr = attr_kind;
  attr->dw_attr_val.val_class = dw_val_class_addr;
  attr->dw_attr_val.v.val_addr = addr;
  add_dwarf_attr (die, attr);
}

static inline rtx AT_addr PARAMS ((dw_attr_ref));
static inline rtx
AT_addr (a)
     register dw_attr_ref a;
{
  if (a && AT_class (a) == dw_val_class_addr)
    return a->dw_attr_val.v.val_addr;

  abort ();
}

/* Add a label identifier attribute value to a DIE.  */

static inline void
add_AT_lbl_id (die, attr_kind, lbl_id)
     register dw_die_ref die;
     register enum dwarf_attribute attr_kind;
     register const char *lbl_id;
{
  register dw_attr_ref attr = (dw_attr_ref) xmalloc (sizeof (dw_attr_node));

  attr->dw_attr_next = NULL;
  attr->dw_attr = attr_kind;
  attr->dw_attr_val.val_class = dw_val_class_lbl_id;
  attr->dw_attr_val.v.val_lbl_id = xstrdup (lbl_id);
  add_dwarf_attr (die, attr);
}

/* Add a section offset attribute value to a DIE.  */

static inline void
add_AT_lbl_offset (die, attr_kind, label)
     register dw_die_ref die;
     register enum dwarf_attribute attr_kind;
     register const char *label;
{
  register dw_attr_ref attr = (dw_attr_ref) xmalloc (sizeof (dw_attr_node));

  attr->dw_attr_next = NULL;
  attr->dw_attr = attr_kind;
  attr->dw_attr_val.val_class = dw_val_class_lbl_offset;
  attr->dw_attr_val.v.val_lbl_id = xstrdup (label);
  add_dwarf_attr (die, attr);
}

/* Add an offset attribute value to a DIE.  */

static void
add_AT_offset (die, attr_kind, offset)
     register dw_die_ref die;
     register enum dwarf_attribute attr_kind;
     register unsigned long offset;
{
  register dw_attr_ref attr = (dw_attr_ref) xmalloc (sizeof (dw_attr_node));

  attr->dw_attr_next = NULL;
  attr->dw_attr = attr_kind;
  attr->dw_attr_val.val_class = dw_val_class_offset;
  attr->dw_attr_val.v.val_offset = offset;
  add_dwarf_attr (die, attr);
}

static inline const char *AT_lbl PARAMS ((dw_attr_ref));
static inline const char *
AT_lbl (a)
     register dw_attr_ref a;
{
  if (a && (AT_class (a) == dw_val_class_lbl_id
	    || AT_class (a) == dw_val_class_lbl_offset))
    return a->dw_attr_val.v.val_lbl_id;

  abort ();
}

/* Get the attribute of type attr_kind.  */

static inline dw_attr_ref
get_AT (die, attr_kind)
     register dw_die_ref die;
     register enum dwarf_attribute attr_kind;
{
  register dw_attr_ref a;
  register dw_die_ref spec = NULL;

  if (die != NULL)
    {
      for (a = die->die_attr; a != NULL; a = a->dw_attr_next)
	{
	  if (a->dw_attr == attr_kind)
	    return a;

	  if (a->dw_attr == DW_AT_specification
	      || a->dw_attr == DW_AT_abstract_origin)
	    spec = AT_ref (a);
	}

      if (spec)
	return get_AT (spec, attr_kind);
    }

  return NULL;
}

/* Return the "low pc" attribute value, typically associated with
   a subprogram DIE.  Return null if the "low pc" attribute is
   either not prsent, or if it cannot be represented as an
   assembler label identifier.  */

static inline const char *
get_AT_low_pc (die)
     register dw_die_ref die;
{
  register dw_attr_ref a = get_AT (die, DW_AT_low_pc);
  return a ? AT_lbl (a) : NULL;
}

/* Return the "high pc" attribute value, typically associated with
   a subprogram DIE.  Return null if the "high pc" attribute is
   either not prsent, or if it cannot be represented as an
   assembler label identifier.  */

static inline const char *
get_AT_hi_pc (die)
     register dw_die_ref die;
{
  register dw_attr_ref a = get_AT (die, DW_AT_high_pc);
  return a ? AT_lbl (a) : NULL;
}

/* Return the value of the string attribute designated by ATTR_KIND, or
   NULL if it is not present.  */

static inline const char *
get_AT_string (die, attr_kind)
     register dw_die_ref die;
     register enum dwarf_attribute attr_kind;
{
  register dw_attr_ref a = get_AT (die, attr_kind);
  return a ? AT_string (a) : NULL;
}

/* Return the value of the flag attribute designated by ATTR_KIND, or -1
   if it is not present.  */

static inline int
get_AT_flag (die, attr_kind)
     register dw_die_ref die;
     register enum dwarf_attribute attr_kind;
{
  register dw_attr_ref a = get_AT (die, attr_kind);
  return a ? AT_flag (a) : 0;
}

/* Return the value of the unsigned attribute designated by ATTR_KIND, or 0
   if it is not present.  */

static inline unsigned
get_AT_unsigned (die, attr_kind)
     register dw_die_ref die;
     register enum dwarf_attribute attr_kind;
{
  register dw_attr_ref a = get_AT (die, attr_kind);
  return a ? AT_unsigned (a) : 0;
}

static inline dw_die_ref
get_AT_ref (die, attr_kind)
     dw_die_ref die;
     register enum dwarf_attribute attr_kind;
{
  register dw_attr_ref a = get_AT (die, attr_kind);
  return a ? AT_ref (a) : NULL;
}

static inline int
is_c_family ()
{
  register unsigned lang = get_AT_unsigned (comp_unit_die, DW_AT_language);

  return (lang == DW_LANG_C || lang == DW_LANG_C89
	  || lang == DW_LANG_C_plus_plus);
}

static inline int
is_fortran ()
{
  register unsigned lang = get_AT_unsigned (comp_unit_die, DW_AT_language);

  return (lang == DW_LANG_Fortran77 || lang == DW_LANG_Fortran90);
}

static inline int
is_java ()
{
  register unsigned lang = get_AT_unsigned (comp_unit_die, DW_AT_language);

  return (lang == DW_LANG_Java);
}

/* Free up the memory used by A.  */

static inline void free_AT PARAMS ((dw_attr_ref));
static inline void
free_AT (a)
     dw_attr_ref a;
{
  switch (AT_class (a))
    {
    case dw_val_class_str:
    case dw_val_class_lbl_id:
    case dw_val_class_lbl_offset:
      free (a->dw_attr_val.v.val_str);
      break;

    case dw_val_class_float:
      free (a->dw_attr_val.v.val_float.array);
      break;

    default:
      break;
    }

  free (a);
}

/* Remove the specified attribute if present.  */

static void
remove_AT (die, attr_kind)
     register dw_die_ref die;
     register enum dwarf_attribute attr_kind;
{
  register dw_attr_ref *p;
  register dw_attr_ref removed = NULL;

  if (die != NULL)
    {
      for (p = &(die->die_attr); *p; p = &((*p)->dw_attr_next))
	if ((*p)->dw_attr == attr_kind)
	  {
	    removed = *p;
	    *p = (*p)->dw_attr_next;
	    break;
	  }

      if (removed != 0)
	free_AT (removed);
    }
}

/* Free up the memory used by DIE.  */

static inline void free_die PARAMS ((dw_die_ref));
static inline void
free_die (die)
     dw_die_ref die;
{
  remove_children (die);
  free (die);
}

/* Discard the children of this DIE.  */

static void
remove_children (die)
     register dw_die_ref die;
{
  register dw_die_ref child_die = die->die_child;

  die->die_child = NULL;

  while (child_die != NULL)
    {
      register dw_die_ref tmp_die = child_die;
      register dw_attr_ref a;

      child_die = child_die->die_sib;

      for (a = tmp_die->die_attr; a != NULL;)
	{
	  register dw_attr_ref tmp_a = a;

	  a = a->dw_attr_next;
	  free_AT (tmp_a);
	}

      free_die (tmp_die);
    }
}

/* Add a child DIE below its parent.  We build the lists up in reverse
   addition order, and correct that in reverse_all_dies.  */

static inline void
add_child_die (die, child_die)
     register dw_die_ref die;
     register dw_die_ref child_die;
{
  if (die != NULL && child_die != NULL)
    {
      if (die == child_die)
	abort ();
      child_die->die_parent = die;
      child_die->die_sib = die->die_child;
      die->die_child = child_die;
    }
}

/* Move CHILD, which must be a child of PARENT or the DIE for which PARENT
   is the specification, to the front of PARENT's list of children.  */

static void
splice_child_die (parent, child)
     dw_die_ref parent, child;
{
  dw_die_ref *p;

  /* We want the declaration DIE from inside the class, not the
     specification DIE at toplevel.  */
  if (child->die_parent != parent)
    {
      dw_die_ref tmp = get_AT_ref (child, DW_AT_specification);
      if (tmp)
	child = tmp;
    }

  if (child->die_parent != parent
      && child->die_parent != get_AT_ref (parent, DW_AT_specification))
    abort ();

  for (p = &(child->die_parent->die_child); *p; p = &((*p)->die_sib))
    if (*p == child)
      {
	*p = child->die_sib;
	break;
      }

  child->die_sib = parent->die_child;
  parent->die_child = child;
}

/* Return a pointer to a newly created DIE node.  */

static inline dw_die_ref
new_die (tag_value, parent_die)
     register enum dwarf_tag tag_value;
     register dw_die_ref parent_die;
{
  register dw_die_ref die = (dw_die_ref) xcalloc (1, sizeof (die_node));

  die->die_tag = tag_value;

  if (parent_die != NULL)
    add_child_die (parent_die, die);
  else
    {
      limbo_die_node *limbo_node;

      limbo_node = (limbo_die_node *) xmalloc (sizeof (limbo_die_node));
      limbo_node->die = die;
      limbo_node->next = limbo_die_list;
      limbo_die_list = limbo_node;
    }

  return die;
}

/* Return the DIE associated with the given type specifier.  */

static inline dw_die_ref
lookup_type_die (type)
     register tree type;
{
  if (TREE_CODE (type) == VECTOR_TYPE)
    type = TYPE_DEBUG_REPRESENTATION_TYPE (type);
  return (dw_die_ref) TYPE_SYMTAB_POINTER (type);
}

/* Equate a DIE to a given type specifier.  */

static inline void
equate_type_number_to_die (type, type_die)
     register tree type;
     register dw_die_ref type_die;
{
  TYPE_SYMTAB_POINTER (type) = (char *) type_die;
}

/* Return the DIE associated with a given declaration.  */

static inline dw_die_ref
lookup_decl_die (decl)
     register tree decl;
{
  register unsigned decl_id = DECL_UID (decl);

  return (decl_id < decl_die_table_in_use
	  ? decl_die_table[decl_id] : NULL);
}

/* Equate a DIE to a particular declaration.  */

static void
equate_decl_number_to_die (decl, decl_die)
     register tree decl;
     register dw_die_ref decl_die;
{
  register unsigned decl_id = DECL_UID (decl);
  register unsigned num_allocated;

  if (decl_id >= decl_die_table_allocated)
    {
      num_allocated
	= ((decl_id + 1 + DECL_DIE_TABLE_INCREMENT - 1)
	   / DECL_DIE_TABLE_INCREMENT)
	  * DECL_DIE_TABLE_INCREMENT;

      decl_die_table
	= (dw_die_ref *) xrealloc (decl_die_table,
				   sizeof (dw_die_ref) * num_allocated);

      memset ((char *) &decl_die_table[decl_die_table_allocated], 0,
	     (num_allocated - decl_die_table_allocated) * sizeof (dw_die_ref));
      decl_die_table_allocated = num_allocated;
    }

  if (decl_id >= decl_die_table_in_use)
    decl_die_table_in_use = (decl_id + 1);

  decl_die_table[decl_id] = decl_die;
}

/* Keep track of the number of spaces used to indent the
   output of the debugging routines that print the structure of
   the DIE internal representation.  */
static int print_indent;

/* Indent the line the number of spaces given by print_indent.  */

static inline void
print_spaces (outfile)
     FILE *outfile;
{
  fprintf (outfile, "%*s", print_indent, "");
}

/* Print the information associated with a given DIE, and its children.
   This routine is a debugging aid only.  */

static void
print_die (die, outfile)
     dw_die_ref die;
     FILE *outfile;
{
  register dw_attr_ref a;
  register dw_die_ref c;

  print_spaces (outfile);
  fprintf (outfile, "DIE %4lu: %s\n",
	   die->die_offset, dwarf_tag_name (die->die_tag));
  print_spaces (outfile);
  fprintf (outfile, "  abbrev id: %lu", die->die_abbrev);
  fprintf (outfile, " offset: %lu\n", die->die_offset);

  for (a = die->die_attr; a != NULL; a = a->dw_attr_next)
    {
      print_spaces (outfile);
      fprintf (outfile, "  %s: ", dwarf_attr_name (a->dw_attr));

      switch (AT_class (a))
	{
	case dw_val_class_addr:
	  fprintf (outfile, "address");
	  break;
	case dw_val_class_offset:
	  fprintf (outfile, "offset");
	  break;
	case dw_val_class_loc:
	  fprintf (outfile, "location descriptor");
	  break;
	case dw_val_class_loc_list:
	  fprintf (outfile, "location list -> label:%s",
		   AT_loc_list (a)->ll_symbol);
	  break;
	case dw_val_class_const:
	  fprintf (outfile, "%ld", AT_int (a));
	  break;
	case dw_val_class_unsigned_const:
	  fprintf (outfile, "%lu", AT_unsigned (a));
	  break;
	case dw_val_class_long_long:
	  fprintf (outfile, "constant (%lu,%lu)",
		   a->dw_attr_val.v.val_long_long.hi,
		   a->dw_attr_val.v.val_long_long.low);
	  break;
	case dw_val_class_float:
	  fprintf (outfile, "floating-point constant");
	  break;
	case dw_val_class_flag:
	  fprintf (outfile, "%u", AT_flag (a));
	  break;
	case dw_val_class_die_ref:
	  if (AT_ref (a) != NULL)
	    {
	      if (AT_ref (a)->die_symbol)
		fprintf (outfile, "die -> label: %s", AT_ref (a)->die_symbol);
	      else
		fprintf (outfile, "die -> %lu", AT_ref (a)->die_offset);
	    }
	  else
	    fprintf (outfile, "die -> <null>");
	  break;
	case dw_val_class_lbl_id:
	case dw_val_class_lbl_offset:
	  fprintf (outfile, "label: %s", AT_lbl (a));
	  break;
	case dw_val_class_str:
	  if (AT_string (a) != NULL)
	    fprintf (outfile, "\"%s\"", AT_string (a));
	  else
	    fprintf (outfile, "<null>");
	  break;
	default:
	  break;
	}

      fprintf (outfile, "\n");
    }

  if (die->die_child != NULL)
    {
      print_indent += 4;
      for (c = die->die_child; c != NULL; c = c->die_sib)
	print_die (c, outfile);

      print_indent -= 4;
    }
  if (print_indent == 0)
    fprintf (outfile, "\n");
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
      fprintf (outfile, "%-20s", file_table.table[line_info->dw_file_num]);
      fprintf (outfile, "%6ld", line_info->dw_line_num);
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

/* Print all DWARF information collected for the compilation unit.
   This routine is a debugging aid only.  */

void
debug_dwarf ()
{
  print_indent = 0;
  print_die (comp_unit_die, stderr);
  if (! DWARF2_ASM_LINE_DEBUG_INFO)
    print_dwarf_line_table (stderr);
}

/* We build up the lists of children and attributes by pushing new ones
   onto the beginning of the list.  Reverse the lists for DIE so that
   they are in order of addition.  */

static void
reverse_die_lists (die)
     register dw_die_ref die;
{
  register dw_die_ref c, cp, cn;
  register dw_attr_ref a, ap, an;

  for (a = die->die_attr, ap = 0; a; a = an)
    {
      an = a->dw_attr_next;
      a->dw_attr_next = ap;
      ap = a;
    }
  die->die_attr = ap;

  for (c = die->die_child, cp = 0; c; c = cn)
    {
      cn = c->die_sib;
      c->die_sib = cp;
      cp = c;
    }
  die->die_child = cp;
}

/* reverse_die_lists only reverses the single die you pass it. Since
   we used to reverse all dies in add_sibling_attributes, which runs
   through all the dies, it would reverse all the dies.  Now, however,
   since we don't call reverse_die_lists in add_sibling_attributes, we
   need a routine to recursively reverse all the dies. This is that
   routine.  */

static void
reverse_all_dies (die)
     register dw_die_ref die;
{
  register dw_die_ref c;

  reverse_die_lists (die);

  for (c = die->die_child; c; c = c->die_sib)
    reverse_all_dies (c);
}

/* Start a new compilation unit DIE for an include file.  OLD_UNIT is
   the CU for the enclosing include file, if any.  BINCL_DIE is the
   DW_TAG_GNU_BINCL DIE that marks the start of the DIEs for this
   include file.  */

static dw_die_ref
push_new_compile_unit (old_unit, bincl_die)
     dw_die_ref old_unit, bincl_die;
{
  const char *filename = get_AT_string (bincl_die, DW_AT_name);
  dw_die_ref new_unit = gen_compile_unit_die (filename);
  new_unit->die_sib = old_unit;
  return new_unit;
}

/* Close an include-file CU and reopen the enclosing one.  */

static dw_die_ref
pop_compile_unit (old_unit)
     dw_die_ref old_unit;
{
  dw_die_ref new_unit = old_unit->die_sib;
  old_unit->die_sib = NULL;
  return new_unit;
}

#define PROCESS(FOO) md5_process_bytes (&(FOO), sizeof (FOO), ctx)
#define PROCESS_STRING(FOO) md5_process_bytes ((FOO), strlen (FOO), ctx)

/* Calculate the checksum of a location expression.  */

static inline void
loc_checksum (loc, ctx)
     dw_loc_descr_ref loc;
     struct md5_ctx *ctx;
{
  PROCESS (loc->dw_loc_opc);
  PROCESS (loc->dw_loc_oprnd1);
  PROCESS (loc->dw_loc_oprnd2);
}

/* Calculate the checksum of an attribute.  */

static void
attr_checksum (at, ctx)
     dw_attr_ref at;
     struct md5_ctx *ctx;
{
  dw_loc_descr_ref loc;
  rtx r;

  PROCESS (at->dw_attr);

  /* We don't care about differences in file numbering.  */
  if (at->dw_attr == DW_AT_decl_file
      /* Or that this was compiled with a different compiler snapshot; if
	 the output is the same, that's what matters.  */
      || at->dw_attr == DW_AT_producer)
    return;

  switch (AT_class (at))
    {
    case dw_val_class_const:
      PROCESS (at->dw_attr_val.v.val_int);
      break;
    case dw_val_class_unsigned_const:
      PROCESS (at->dw_attr_val.v.val_unsigned);
      break;
    case dw_val_class_long_long:
      PROCESS (at->dw_attr_val.v.val_long_long);
      break;
    case dw_val_class_float:
      PROCESS (at->dw_attr_val.v.val_float);
      break;
    case dw_val_class_flag:
      PROCESS (at->dw_attr_val.v.val_flag);
      break;

    case dw_val_class_str:
      PROCESS_STRING (AT_string (at));
      break;

    case dw_val_class_addr:
      r = AT_addr (at);
      switch (GET_CODE (r))
	{
	case SYMBOL_REF:
	  PROCESS_STRING (XSTR (r, 0));
	  break;

	default:
	  abort ();
	}
      break;

    case dw_val_class_offset:
      PROCESS (at->dw_attr_val.v.val_offset);
      break;

    case dw_val_class_loc:
      for (loc = AT_loc (at); loc; loc = loc->dw_loc_next)
	loc_checksum (loc, ctx);
      break;

    case dw_val_class_die_ref:
      if (AT_ref (at)->die_offset)
	PROCESS (AT_ref (at)->die_offset);
      /* FIXME else use target die name or something.  */

    case dw_val_class_fde_ref:
    case dw_val_class_lbl_id:
    case dw_val_class_lbl_offset:
      break;

    default:
      break;
    }
}

/* Calculate the checksum of a DIE.  */

static void
die_checksum (die, ctx)
     dw_die_ref die;
     struct md5_ctx *ctx;
{
  dw_die_ref c;
  dw_attr_ref a;

  PROCESS (die->die_tag);

  for (a = die->die_attr; a; a = a->dw_attr_next)
    attr_checksum (a, ctx);

  for (c = die->die_child; c; c = c->die_sib)
    die_checksum (c, ctx);
}

#undef PROCESS
#undef PROCESS_STRING

/* The prefix to attach to symbols on DIEs in the current comdat debug
   info section.  */
static char *comdat_symbol_id;

/* The index of the current symbol within the current comdat CU.  */
static unsigned int comdat_symbol_number;

/* Calculate the MD5 checksum of the compilation unit DIE UNIT_DIE and its
   children, and set comdat_symbol_id accordingly.  */

static void
compute_section_prefix (unit_die)
     dw_die_ref unit_die;
{
  char *name;
  int i;
  unsigned char checksum[16];
  struct md5_ctx ctx;

  md5_init_ctx (&ctx);
  die_checksum (unit_die, &ctx);
  md5_finish_ctx (&ctx, checksum);

  {
    const char *p = lbasename (get_AT_string (unit_die, DW_AT_name));
    name = (char *) alloca (strlen (p) + 64);
    sprintf (name, "%s.", p);
  }

  clean_symbol_name (name);

  {
    char *p = name + strlen (name);
    for (i = 0; i < 4; ++i)
      {
	sprintf (p, "%.2x", checksum[i]);
	p += 2;
      }
  }

  comdat_symbol_id = unit_die->die_symbol = xstrdup (name);
  comdat_symbol_number = 0;
}

/* Returns nonzero iff DIE represents a type, in the sense of TYPE_P.  */

static int
is_type_die (die)
     dw_die_ref die;
{
  switch (die->die_tag)
    {
    case DW_TAG_array_type:
    case DW_TAG_class_type:
    case DW_TAG_enumeration_type:
    case DW_TAG_pointer_type:
    case DW_TAG_reference_type:
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
is_comdat_die (c)
     dw_die_ref c;
{
#if 1
  /* I think we want to leave base types and __vtbl_ptr_type in the
     main CU, as we do for stabs.  The advantage is a greater
     likelihood of sharing between objects that don't include headers
     in the same order (and therefore would put the base types in a
     different comdat).  jason 8/28/00 */
  if (c->die_tag == DW_TAG_base_type)
    return 0;

  if (c->die_tag == DW_TAG_pointer_type
      || c->die_tag == DW_TAG_reference_type
      || c->die_tag == DW_TAG_const_type
      || c->die_tag == DW_TAG_volatile_type)
    {
      dw_die_ref t = get_AT_ref (c, DW_AT_type);
      return t ? is_comdat_die (t) : 0;
    }
#endif

  return is_type_die (c);
}

/* Returns 1 iff C is the sort of DIE that might be referred to from another
   compilation unit.  */

static int
is_symbol_die (c)
     dw_die_ref c;
{
  if (is_type_die (c))
    return 1;
  if (get_AT (c, DW_AT_declaration) 
      && ! get_AT (c, DW_AT_specification))
    return 1;
  return 0;
}

static char *
gen_internal_sym (prefix)
	const char *prefix;
{
  char buf[256];
  static int label_num;
  ASM_GENERATE_INTERNAL_LABEL (buf, prefix, label_num++);
  return xstrdup (buf);
}

/* Assign symbols to all worthy DIEs under DIE.  */

static void
assign_symbol_names (die)
     register dw_die_ref die;
{
  register dw_die_ref c;

  if (is_symbol_die (die))
    {
      if (comdat_symbol_id)
	{
	  char *p = alloca (strlen (comdat_symbol_id) + 64);
	  sprintf (p, "%s.%s.%x", DIE_LABEL_PREFIX,
		   comdat_symbol_id, comdat_symbol_number++);
	  die->die_symbol = xstrdup (p);
	}
      else
	die->die_symbol = gen_internal_sym ("LDIE");
    }

  for (c = die->die_child; c != NULL; c = c->die_sib)
    assign_symbol_names (c);
}

/* Traverse the DIE (which is always comp_unit_die), and set up
   additional compilation units for each of the include files we see
   bracketed by BINCL/EINCL.  */

static void
break_out_includes (die)
     register dw_die_ref die;
{
  dw_die_ref *ptr;
  register dw_die_ref unit = NULL;
  limbo_die_node *node;

  for (ptr = &(die->die_child); *ptr; )
    {
      register dw_die_ref c = *ptr;

      if (c->die_tag == DW_TAG_GNU_BINCL
	  || c->die_tag == DW_TAG_GNU_EINCL
	  || (unit && is_comdat_die (c)))
	{
	  /* This DIE is for a secondary CU; remove it from the main one.  */
	  *ptr = c->die_sib;

	  if (c->die_tag == DW_TAG_GNU_BINCL)
	    {
	      unit = push_new_compile_unit (unit, c);
	      free_die (c);
	    }
	  else if (c->die_tag == DW_TAG_GNU_EINCL)
	    {
	      unit = pop_compile_unit (unit);
	      free_die (c);
	    }
	  else
	    add_child_die (unit, c);
	}
      else
	{
	  /* Leave this DIE in the main CU.  */
	  ptr = &(c->die_sib);
	  continue;
	}
    }

#if 0
  /* We can only use this in debugging, since the frontend doesn't check
     to make sure that we leave every include file we enter.  */
  if (unit != NULL)
    abort ();
#endif

  assign_symbol_names (die);
  for (node = limbo_die_list; node; node = node->next)
    {
      compute_section_prefix (node->die);
      assign_symbol_names (node->die);
    }
}

/* Traverse the DIE and add a sibling attribute if it may have the
   effect of speeding up access to siblings.  To save some space,
   avoid generating sibling attributes for DIE's without children.  */

static void
add_sibling_attributes (die)
     register dw_die_ref die;
{
  register dw_die_ref c;

  if (die->die_tag != DW_TAG_compile_unit
      && die->die_sib && die->die_child != NULL)
    /* Add the sibling link to the front of the attribute list.  */
    add_AT_die_ref (die, DW_AT_sibling, die->die_sib);

  for (c = die->die_child; c != NULL; c = c->die_sib)
    add_sibling_attributes (c);
}

/* Output all location lists for the DIE and it's children */
static void
output_location_lists (die)
     register dw_die_ref die;
{
  dw_die_ref c;
  dw_attr_ref d_attr;
  for (d_attr = die->die_attr; d_attr; d_attr = d_attr->dw_attr_next)
    {
      if (AT_class (d_attr) == dw_val_class_loc_list)
	{
	  output_loc_list (AT_loc_list (d_attr));
	}
    }
  for (c = die->die_child; c != NULL; c = c->die_sib)
    output_location_lists (c);

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
  register unsigned int n_alloc;
  register dw_die_ref c;
  register dw_attr_ref d_attr, a_attr;

  /* Scan the DIE references, and mark as external any that refer to
     DIEs from other CUs (i.e. those which are not marked).  */
  for (d_attr = die->die_attr; d_attr; d_attr = d_attr->dw_attr_next)
    {
      if (AT_class (d_attr) == dw_val_class_die_ref
	  && AT_ref (d_attr)->die_mark == 0)
	{
	  if (AT_ref (d_attr)->die_symbol == 0)
	    abort ();
	  set_AT_ref_external (d_attr, 1);
	}
    }

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
		      || (value_format (a_attr) != value_format (d_attr)))
		    break;

		  a_attr = a_attr->dw_attr_next;
		  d_attr = d_attr->dw_attr_next;
		}

	      if (a_attr == NULL && d_attr == NULL)
		break;
	    }
	}
    }

  if (abbrev_id >= abbrev_die_table_in_use)
    {
      if (abbrev_die_table_in_use >= abbrev_die_table_allocated)
	{
	  n_alloc = abbrev_die_table_allocated + ABBREV_DIE_TABLE_INCREMENT;
	  abbrev_die_table
	    = (dw_die_ref *) xrealloc (abbrev_die_table,
				       sizeof (dw_die_ref) * n_alloc);

	  memset ((char *) &abbrev_die_table[abbrev_die_table_allocated], 0,
		 (n_alloc - abbrev_die_table_allocated) * sizeof (dw_die_ref));
	  abbrev_die_table_allocated = n_alloc;
	}

      ++abbrev_die_table_in_use;
      abbrev_die_table[abbrev_id] = die;
    }

  die->die_abbrev = abbrev_id;
  for (c = die->die_child; c != NULL; c = c->die_sib)
    build_abbrev_table (c);
}

/* Return the size of a string, including the null byte.

   This used to treat backslashes as escapes, and hence they were not included
   in the count.  However, that conflicts with what ASM_OUTPUT_ASCII does,
   which treats a backslash as a backslash, escaping it if necessary, and hence
   we must include them in the count.  */

static unsigned long
size_of_string (str)
     register const char *str;
{
  return strlen (str) + 1;
}

/* Return the power-of-two number of bytes necessary to represent VALUE.  */

static int
constant_size (value)
     long unsigned value;
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

/* Return the size of a DIE, as it is represented in the
   .debug_info section.  */

static unsigned long
size_of_die (die)
     register dw_die_ref die;
{
  register unsigned long size = 0;
  register dw_attr_ref a;

  size += size_of_uleb128 (die->die_abbrev);
  for (a = die->die_attr; a != NULL; a = a->dw_attr_next)
    {
      switch (AT_class (a))
	{
	case dw_val_class_addr:
	  size += DWARF2_ADDR_SIZE;
	  break;
	case dw_val_class_offset:
	  size += DWARF_OFFSET_SIZE;
	  break;
	case dw_val_class_loc:
	  {
	    register unsigned long lsize = size_of_locs (AT_loc (a));

	    /* Block length.  */
	    size += constant_size (lsize);
	    size += lsize;
	  }
	  break;
	case dw_val_class_loc_list:
	  size += DWARF_OFFSET_SIZE;
	  break;
	case dw_val_class_const:
	  size += size_of_sleb128 (AT_int (a));
	  break;
	case dw_val_class_unsigned_const:
	  size += constant_size (AT_unsigned (a));
	  break;
	case dw_val_class_long_long:
	  size += 1 + 2*HOST_BITS_PER_LONG/HOST_BITS_PER_CHAR; /* block */
	  break;
	case dw_val_class_float:
	  size += 1 + a->dw_attr_val.v.val_float.length * 4; /* block */
	  break;
	case dw_val_class_flag:
	  size += 1;
	  break;
	case dw_val_class_die_ref:
	  size += DWARF_OFFSET_SIZE;
	  break;
	case dw_val_class_fde_ref:
	  size += DWARF_OFFSET_SIZE;
	  break;
	case dw_val_class_lbl_id:
	  size += DWARF2_ADDR_SIZE;
	  break;
	case dw_val_class_lbl_offset:
	  size += DWARF_OFFSET_SIZE;
	  break;
	case dw_val_class_str:
	  size += size_of_string (AT_string (a));
	  break;
	default:
	  abort ();
	}
    }

  return size;
}

/* Size the debugging information associated with a given DIE.
   Visits the DIE's children recursively.  Updates the global
   variable next_die_offset, on each time through.  Uses the
   current value of next_die_offset to update the die_offset
   field in each DIE.  */

static void
calc_die_sizes (die)
     dw_die_ref die;
{
  register dw_die_ref c;
  die->die_offset = next_die_offset;
  next_die_offset += size_of_die (die);

  for (c = die->die_child; c != NULL; c = c->die_sib)
    calc_die_sizes (c);

  if (die->die_child != NULL)
    /* Count the null byte used to terminate sibling lists.  */
    next_die_offset += 1;
}

/* Set the marks for a die and its children.  We do this so
   that we know whether or not a reference needs to use FORM_ref_addr; only
   DIEs in the same CU will be marked.  We used to clear out the offset
   and use that as the flag, but ran into ordering problems.  */

static void
mark_dies (die)
     dw_die_ref die;
{
  register dw_die_ref c;
  die->die_mark = 1;
  for (c = die->die_child; c; c = c->die_sib)
    mark_dies (c);
}

/* Clear the marks for a die and its children.  */

static void
unmark_dies (die)
     dw_die_ref die;
{
  register dw_die_ref c;
  die->die_mark = 0;
  for (c = die->die_child; c; c = c->die_sib)
    unmark_dies (c);
}

/* Return the size of the .debug_pubnames table  generated for the
   compilation unit.  */

static unsigned long
size_of_pubnames ()
{
  register unsigned long size;
  register unsigned i;

  size = DWARF_PUBNAMES_HEADER_SIZE;
  for (i = 0; i < pubname_table_in_use; ++i)
    {
      register pubname_ref p = &pubname_table[i];
      size += DWARF_OFFSET_SIZE + size_of_string (p->name);
    }

  size += DWARF_OFFSET_SIZE;
  return size;
}

/* Return the size of the information in the .debug_aranges section.  */

static unsigned long
size_of_aranges ()
{
  register unsigned long size;

  size = DWARF_ARANGES_HEADER_SIZE;

  /* Count the address/length pair for this compilation unit.  */
  size += 2 * DWARF2_ADDR_SIZE;
  size += 2 * DWARF2_ADDR_SIZE * arange_table_in_use;

  /* Count the two zero words used to terminated the address range table.  */
  size += 2 * DWARF2_ADDR_SIZE;
  return size;
}

/* Select the encoding of an attribute value.  */

static enum dwarf_form
value_format (a)
     dw_attr_ref a;
{
  switch (a->dw_attr_val.val_class)
    {
    case dw_val_class_addr:
      return DW_FORM_addr;
    case dw_val_class_offset:
      if (DWARF_OFFSET_SIZE == 4)
	return DW_FORM_data4;
      if (DWARF_OFFSET_SIZE == 8)
	return DW_FORM_data8;
      abort ();
    case dw_val_class_loc_list:
      /* FIXME: Could be DW_FORM_data8, with a > 32 bit size
	 .debug_loc section */
      return DW_FORM_data4;
    case dw_val_class_loc:
      switch (constant_size (size_of_locs (AT_loc (a))))
	{
	case 1:
	  return DW_FORM_block1;
	case 2:
	  return DW_FORM_block2;
	default:
	  abort ();
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
	  return DW_FORM_data4;
	case 8:
	  return DW_FORM_data8;
	default:
	  abort ();
	}
    case dw_val_class_long_long:
      return DW_FORM_block1;
    case dw_val_class_float:
      return DW_FORM_block1;
    case dw_val_class_flag:
      return DW_FORM_flag;
    case dw_val_class_die_ref:
      if (AT_ref_external (a))
	return DW_FORM_ref_addr;
      else
	return DW_FORM_ref;
    case dw_val_class_fde_ref:
      return DW_FORM_data;
    case dw_val_class_lbl_id:
      return DW_FORM_addr;
    case dw_val_class_lbl_offset:
      return DW_FORM_data;
    case dw_val_class_str:
      return DW_FORM_string;

    default:
      abort ();
    }
}

/* Output the encoding of an attribute value.  */

static void
output_value_format (a)
     dw_attr_ref a;
{
  enum dwarf_form form = value_format (a);
  dw2_asm_output_data_uleb128 (form, "(%s)", dwarf_form_name (form));
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

      dw2_asm_output_data_uleb128 (abbrev_id, "(abbrev code)");

      dw2_asm_output_data_uleb128 (abbrev->die_tag, "(TAG: %s)",
				   dwarf_tag_name (abbrev->die_tag));

      if (abbrev->die_child != NULL)
	dw2_asm_output_data (1, DW_children_yes, "DW_children_yes");
      else
	dw2_asm_output_data (1, DW_children_no, "DW_children_no");

      for (a_attr = abbrev->die_attr; a_attr != NULL;
	   a_attr = a_attr->dw_attr_next)
	{
	  dw2_asm_output_data_uleb128 (a_attr->dw_attr, "(%s)",
				       dwarf_attr_name (a_attr->dw_attr));
	  output_value_format (a_attr);
	}

      dw2_asm_output_data (1, 0, NULL);
      dw2_asm_output_data (1, 0, NULL);
    }

  /* Terminate the table.  */
  dw2_asm_output_data (1, 0, NULL);
}

/* Output a symbol we can use to refer to this DIE from another CU.  */

static inline void
output_die_symbol (die)
     register dw_die_ref die;
{
  char *sym = die->die_symbol;

  if (sym == 0)
    return;

  if (strncmp (sym, DIE_LABEL_PREFIX, sizeof (DIE_LABEL_PREFIX) - 1) == 0)
    /* We make these global, not weak; if the target doesn't support
       .linkonce, it doesn't support combining the sections, so debugging
       will break.  */
    ASM_GLOBALIZE_LABEL (asm_out_file, sym);
  ASM_OUTPUT_LABEL (asm_out_file, sym);
}

/* Return a new location list, given the begin and end range, and the
   expression. gensym tells us whether to generate a new internal
   symbol for this location list node, which is done for the head of
   the list only.  */ 
static inline dw_loc_list_ref
new_loc_list (expr, begin, end, section, gensym)
     register dw_loc_descr_ref expr;
     register const char *begin;
     register const char *end;
     register const char *section;
     register unsigned gensym;
{
  register dw_loc_list_ref retlist
    = (dw_loc_list_ref) xcalloc (1, sizeof (dw_loc_list_node));
  retlist->begin = begin;
  retlist->end = end;
  retlist->expr = expr;
  retlist->section = section;
  if (gensym) 
    retlist->ll_symbol = gen_internal_sym ("LLST");
  return retlist;
}

/* Add a location description expression to a location list */
static inline void
add_loc_descr_to_loc_list (list_head, descr, begin, end, section)
     register dw_loc_list_ref *list_head;
     register dw_loc_descr_ref descr;
     register const char *begin;
     register const char *end;
     register const char *section;
{
  register dw_loc_list_ref *d;
  
  /* Find the end of the chain.  */
  for (d = list_head; (*d) != NULL; d = &(*d)->dw_loc_next)
    ;
  /* Add a new location list node to the list */
  *d = new_loc_list (descr, begin, end, section, 0);
}

/* Output the location list given to us */
static void
output_loc_list (list_head)
     register dw_loc_list_ref list_head;
{
  register dw_loc_list_ref curr=list_head;
  ASM_OUTPUT_LABEL (asm_out_file, list_head->ll_symbol);

  /* ??? This shouldn't be needed now that we've forced the
     compilation unit base address to zero when there is code
     in more than one section.  */
  if (strcmp (curr->section, ".text") == 0)
    {
      /* dw2_asm_output_data will mask off any extra bits in the ~0.  */
      dw2_asm_output_data (DWARF2_ADDR_SIZE, ~(unsigned HOST_WIDE_INT)0,
			   "Location list base address specifier fake entry");
      dw2_asm_output_offset (DWARF2_ADDR_SIZE, curr->section,
			     "Location list base address specifier base");
    }
  for (curr = list_head; curr != NULL; curr=curr->dw_loc_next)
    {
      int size;
      dw2_asm_output_delta (DWARF2_ADDR_SIZE, curr->begin, curr->section,
			    "Location list begin address (%s)",
			    list_head->ll_symbol);
      dw2_asm_output_delta (DWARF2_ADDR_SIZE, curr->end, curr->section,
			    "Location list end address (%s)",
			    list_head->ll_symbol);
      size = size_of_locs (curr->expr);
      
      /* Output the block length for this list of location operations.  */
      dw2_asm_output_data (constant_size (size), size, "%s",
			   "Location expression size");
      
      output_loc_sequence (curr->expr);
    }
  dw2_asm_output_data (DWARF_OFFSET_SIZE, 0,
		       "Location list terminator begin (%s)",
		       list_head->ll_symbol);
  dw2_asm_output_data (DWARF_OFFSET_SIZE, 0,
		       "Location list terminator end (%s)",
		       list_head->ll_symbol);
}
/* Output the DIE and its attributes.  Called recursively to generate
   the definitions of each child DIE.  */

static void
output_die (die)
     register dw_die_ref die;
{
  register dw_attr_ref a;
  register dw_die_ref c;
  register unsigned long size;

  /* If someone in another CU might refer to us, set up a symbol for
     them to point to.  */
  if (die->die_symbol)
    output_die_symbol (die);

  dw2_asm_output_data_uleb128 (die->die_abbrev, "(DIE (0x%lx) %s)",
			       die->die_offset, dwarf_tag_name (die->die_tag));

  for (a = die->die_attr; a != NULL; a = a->dw_attr_next)
    {
      const char *name = dwarf_attr_name (a->dw_attr);

      switch (AT_class (a))
	{
	case dw_val_class_addr:
	  dw2_asm_output_addr_rtx (DWARF2_ADDR_SIZE, AT_addr (a), "%s", name);
	  break;

	case dw_val_class_offset:
	  dw2_asm_output_data (DWARF_OFFSET_SIZE, a->dw_attr_val.v.val_offset,
			       "%s", name);
	  break;

	case dw_val_class_loc:
	  size = size_of_locs (AT_loc (a));

	  /* Output the block length for this list of location operations.  */
	  dw2_asm_output_data (constant_size (size), size, "%s", name);

	  output_loc_sequence (AT_loc (a));
	  break;

	case dw_val_class_const:
	  /* ??? It would be slightly more efficient to use a scheme like is
	     used for unsigned constants below, but gdb 4.x does not sign
	     extend.  Gdb 5.x does sign extend.  */
	  dw2_asm_output_data_sleb128 (AT_int (a), "%s", name);
	  break;

	case dw_val_class_unsigned_const:
	  dw2_asm_output_data (constant_size (AT_unsigned (a)),
			       AT_unsigned (a), "%s", name);
	  break;

	case dw_val_class_long_long:
	  {
	    unsigned HOST_WIDE_INT first, second;

	    dw2_asm_output_data (1, 2*HOST_BITS_PER_LONG/HOST_BITS_PER_CHAR,
			         "%s", name);

	    if (WORDS_BIG_ENDIAN)
	      {
		first = a->dw_attr_val.v.val_long_long.hi;
		second = a->dw_attr_val.v.val_long_long.low;
	      }
	    else
	      {
		first = a->dw_attr_val.v.val_long_long.low;
		second = a->dw_attr_val.v.val_long_long.hi;
	      }
	    dw2_asm_output_data (HOST_BITS_PER_LONG/HOST_BITS_PER_CHAR,
				 first, "long long constant");
	    dw2_asm_output_data (HOST_BITS_PER_LONG/HOST_BITS_PER_CHAR,
				 second, NULL);
	  }
	  break;

	case dw_val_class_float:
	  {
	    register unsigned int i;

	    dw2_asm_output_data (1, a->dw_attr_val.v.val_float.length * 4,
			         "%s", name);

	    for (i = 0; i < a->dw_attr_val.v.val_float.length; ++i)
	      dw2_asm_output_data (4, a->dw_attr_val.v.val_float.array[i],
				   "fp constant word %u", i);
	    break;
	  }

	case dw_val_class_flag:
	  dw2_asm_output_data (1, AT_flag (a), "%s", name);
	  break;

        case dw_val_class_loc_list:
	  {
	    char *sym = AT_loc_list (a)->ll_symbol;
	    if (sym == 0)
	      abort();
	    dw2_asm_output_delta (DWARF_OFFSET_SIZE, sym,
				  loc_section_label, "%s", name);
	  }
	  break;

	case dw_val_class_die_ref:
	  if (AT_ref_external (a))
	    {
	      char *sym = AT_ref (a)->die_symbol;
	      if (sym == 0)
		abort ();
	      dw2_asm_output_offset (DWARF2_ADDR_SIZE, sym, "%s", name);
	    }
	  else if (AT_ref (a)->die_offset == 0)
	    abort ();
	  else
	    dw2_asm_output_data (DWARF_OFFSET_SIZE, AT_ref (a)->die_offset,
				 "%s", name);
	  break;

	case dw_val_class_fde_ref:
	  {
	    char l1[20];
	    ASM_GENERATE_INTERNAL_LABEL (l1, FDE_LABEL,
					 a->dw_attr_val.v.val_fde_index * 2);
	    dw2_asm_output_offset (DWARF_OFFSET_SIZE, l1, "%s", name);
	  }
	  break;

	case dw_val_class_lbl_id:
	  dw2_asm_output_addr (DWARF2_ADDR_SIZE, AT_lbl (a), "%s", name);
	  break;

	case dw_val_class_lbl_offset:
	  dw2_asm_output_offset (DWARF_OFFSET_SIZE, AT_lbl (a), "%s", name);
	  break;

	case dw_val_class_str:
	  dw2_asm_output_nstring (AT_string (a), -1, "%s", name);
	  break;

	default:
	  abort ();
	}
    }

  for (c = die->die_child; c != NULL; c = c->die_sib)
    output_die (c);

  if (die->die_child != NULL)
    {
      /* Add null byte to terminate sibling list.  */
      dw2_asm_output_data (1, 0, "end of children of DIE 0x%lx",
			   die->die_offset);
    }
}

/* Output the compilation unit that appears at the beginning of the
   .debug_info section, and precedes the DIE descriptions.  */

static void
output_compilation_unit_header ()
{
  dw2_asm_output_data (DWARF_OFFSET_SIZE, next_die_offset - DWARF_OFFSET_SIZE,
		       "Length of Compilation Unit Info");

  dw2_asm_output_data (2, DWARF_VERSION, "DWARF version number");

  dw2_asm_output_offset (DWARF_OFFSET_SIZE, abbrev_section_label,
			 "Offset Into Abbrev. Section");

  dw2_asm_output_data (1, DWARF2_ADDR_SIZE, "Pointer Size (in bytes)");
}

/* Output the compilation unit DIE and its children.  */

static void
output_comp_unit (die)
     dw_die_ref die;
{
  const char *secname;

  /* Even if there are no children of this DIE, we must output the
     information about the compilation unit.  Otherwise, on an empty
     translation unit, we will generate a present, but empty,
     .debug_info section.  IRIX 6.5 `nm' will then complain when
     examining the file.
     
     Mark all the DIEs in this CU so we know which get local refs.  */
  mark_dies (die);

  build_abbrev_table (die);

  /* Initialize the beginning DIE offset - and calculate sizes/offsets.   */
  next_die_offset = DWARF_COMPILE_UNIT_HEADER_SIZE;
  calc_die_sizes (die);

  if (die->die_symbol)
    {
      char *tmp = (char *) alloca (strlen (die->die_symbol) + 24);
      sprintf (tmp, ".gnu.linkonce.wi.%s", die->die_symbol);
      secname = tmp;
      die->die_symbol = NULL;
    }
  else
    secname = (const char *) DEBUG_INFO_SECTION;

  /* Output debugging information.  */
  named_section_flags (secname, SECTION_DEBUG, 1);
  output_compilation_unit_header ();
  output_die (die);

  /* Leave the marks on the main CU, so we can check them in
     output_pubnames.  */
  if (die->die_symbol)
    unmark_dies (die);
}

/* The DWARF2 pubname for a nested thingy looks like "A::f".  The output
   of decl_printable_name for C++ looks like "A::f(int)".  Let's drop the
   argument list, and maybe the scope.  */

static const char *
dwarf2_name (decl, scope)
     tree decl;
     int scope;
{
  return (*decl_printable_name) (decl, scope ? 1 : 0);
}

/* Add a new entry to .debug_pubnames if appropriate.  */

static void
add_pubname (decl, die)
     tree decl;
     dw_die_ref die;
{
  pubname_ref p;

  if (! TREE_PUBLIC (decl))
    return;

  if (pubname_table_in_use == pubname_table_allocated)
    {
      pubname_table_allocated += PUBNAME_TABLE_INCREMENT;
      pubname_table = (pubname_ref) xrealloc
	(pubname_table, pubname_table_allocated * sizeof (pubname_entry));
    }

  p = &pubname_table[pubname_table_in_use++];
  p->die = die;

  p->name = xstrdup (dwarf2_name (decl, 1));
}

/* Output the public names table used to speed up access to externally
   visible names.  For now, only generate entries for externally
   visible procedures.  */

static void
output_pubnames ()
{
  register unsigned i;
  register unsigned long pubnames_length = size_of_pubnames ();

  dw2_asm_output_data (DWARF_OFFSET_SIZE, pubnames_length,
		       "Length of Public Names Info");

  dw2_asm_output_data (2, DWARF_VERSION, "DWARF Version");

  dw2_asm_output_offset (DWARF_OFFSET_SIZE, debug_info_section_label,
			 "Offset of Compilation Unit Info");

  dw2_asm_output_data (DWARF_OFFSET_SIZE, next_die_offset,
		       "Compilation Unit Length");

  for (i = 0; i < pubname_table_in_use; ++i)
    {
      register pubname_ref pub = &pubname_table[i];

      /* We shouldn't see pubnames for DIEs outside of the main CU.  */
      if (pub->die->die_mark == 0)
	abort ();

      dw2_asm_output_data (DWARF_OFFSET_SIZE, pub->die->die_offset,
			   "DIE offset");

      dw2_asm_output_nstring (pub->name, -1, "external name");
    }

  dw2_asm_output_data (DWARF_OFFSET_SIZE, 0, NULL);
}

/* Add a new entry to .debug_aranges if appropriate.  */

static void
add_arange (decl, die)
     tree decl;
     dw_die_ref die;
{
  if (! DECL_SECTION_NAME (decl))
    return;

  if (arange_table_in_use == arange_table_allocated)
    {
      arange_table_allocated += ARANGE_TABLE_INCREMENT;
      arange_table = (dw_die_ref *)
	xrealloc (arange_table, arange_table_allocated * sizeof (dw_die_ref));
    }

  arange_table[arange_table_in_use++] = die;
}

/* Output the information that goes into the .debug_aranges table.
   Namely, define the beginning and ending address range of the
   text section generated for this compilation unit.  */

static void
output_aranges ()
{
  register unsigned i;
  register unsigned long aranges_length = size_of_aranges ();

  dw2_asm_output_data (DWARF_OFFSET_SIZE, aranges_length,
		       "Length of Address Ranges Info");

  dw2_asm_output_data (2, DWARF_VERSION, "DWARF Version");

  dw2_asm_output_offset (DWARF_OFFSET_SIZE, debug_info_section_label,
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

  dw2_asm_output_addr (DWARF2_ADDR_SIZE, text_section_label, "Address");
  dw2_asm_output_delta (DWARF2_ADDR_SIZE, text_end_label,
			text_section_label, "Length");

  for (i = 0; i < arange_table_in_use; ++i)
    {
      dw_die_ref die = arange_table[i];

      /* We shouldn't see aranges for DIEs outside of the main CU.  */
      if (die->die_mark == 0)
	abort ();

      if (die->die_tag == DW_TAG_subprogram)
	{
	  dw2_asm_output_addr (DWARF2_ADDR_SIZE, get_AT_low_pc (die),
				 "Address");
	  dw2_asm_output_delta (DWARF2_ADDR_SIZE, get_AT_hi_pc (die),
				get_AT_low_pc (die), "Length");
	}
      else
	{
	  /* A static variable; extract the symbol from DW_AT_location.
	     Note that this code isn't currently hit, as we only emit
	     aranges for functions (jason 9/23/99).  */

	  dw_attr_ref a = get_AT (die, DW_AT_location);
	  dw_loc_descr_ref loc;
	  if (! a || AT_class (a) != dw_val_class_loc)
	    abort ();

	  loc = AT_loc (a);
	  if (loc->dw_loc_opc != DW_OP_addr)
	    abort ();

	  dw2_asm_output_addr_rtx (DWARF2_ADDR_SIZE,
				   loc->dw_loc_oprnd1.v.val_addr, "Address");
	  dw2_asm_output_data (DWARF2_ADDR_SIZE,
			       get_AT_unsigned (die, DW_AT_byte_size),
			       "Length");
	}
    }

  /* Output the terminator words.  */
  dw2_asm_output_data (DWARF2_ADDR_SIZE, 0, NULL);
  dw2_asm_output_data (DWARF2_ADDR_SIZE, 0, NULL);
}

/* Add a new entry to .debug_ranges.  Return the offset at which it
   was placed.  */

static unsigned int
add_ranges (block)
     tree block;
{
  unsigned int in_use = ranges_table_in_use;

  if (in_use == ranges_table_allocated)
    {
      ranges_table_allocated += RANGES_TABLE_INCREMENT;
      ranges_table = (dw_ranges_ref)
	xrealloc (ranges_table, (ranges_table_allocated
				 * sizeof (struct dw_ranges_struct)));
    }

  ranges_table[in_use].block_num = (block ? BLOCK_NUMBER (block) : 0);
  ranges_table_in_use = in_use + 1;

  return in_use * 2 * DWARF2_ADDR_SIZE;
}

static void
output_ranges ()
{
  register unsigned i;
  const char *start_fmt = "Offset 0x%x";
  const char *fmt = start_fmt;

  for (i = 0; i < ranges_table_in_use; ++i)
    {
      int block_num = ranges_table[i].block_num;

      if (block_num)
	{
	  char blabel[MAX_ARTIFICIAL_LABEL_BYTES];
	  char elabel[MAX_ARTIFICIAL_LABEL_BYTES];

	  ASM_GENERATE_INTERNAL_LABEL (blabel, BLOCK_BEGIN_LABEL, block_num);
	  ASM_GENERATE_INTERNAL_LABEL (elabel, BLOCK_END_LABEL, block_num);

	  /* If all code is in the text section, then the compilation
	     unit base address defaults to DW_AT_low_pc, which is the
	     base of the text section.  */
	  if (separate_line_info_table_in_use == 0)
	    {
	      dw2_asm_output_delta (DWARF2_ADDR_SIZE, blabel,
				    text_section_label,
				    fmt, i * 2 * DWARF2_ADDR_SIZE);
	      dw2_asm_output_delta (DWARF2_ADDR_SIZE, elabel,
				    text_section_label, NULL);
	    }
	  /* Otherwise, we add a DW_AT_entry_pc attribute to force the
	     compilation unit base address to zero, which allows us to
	     use absolute addresses, and not worry about whether the
	     target supports cross-section arithmetic.  */
	  else
	    {
	      dw2_asm_output_addr (DWARF2_ADDR_SIZE, blabel,
				   fmt, i * 2 * DWARF2_ADDR_SIZE);
	      dw2_asm_output_addr (DWARF2_ADDR_SIZE, elabel, NULL);
	    }

	  fmt = NULL;
	}
      else
	{
	  dw2_asm_output_data (DWARF2_ADDR_SIZE, 0, NULL);
	  dw2_asm_output_data (DWARF2_ADDR_SIZE, 0, NULL);
	  fmt = start_fmt;
	}
    }
}

/* Data structure containing information about input files.  */
struct file_info
{
  char *path;		/* Complete file name.  */
  char *fname;		/* File name part.  */
  int length;		/* Length of entire string.  */
  int file_idx;		/* Index in input file table.  */
  int dir_idx;		/* Index in directory table.  */
};

/* Data structure containing information about directories with source
   files.  */
struct dir_info
{
  char *path;		/* Path including directory name.  */
  int length;		/* Path length.  */
  int prefix;		/* Index of directory entry which is a prefix.  */
  int count;		/* Number of files in this directory.  */
  int dir_idx;		/* Index of directory used as base.  */
  int used;		/* Used in the end?  */
};

/* Callback function for file_info comparison.  We sort by looking at
   the directories in the path.  */
static int
file_info_cmp (p1, p2)
     const void *p1;
     const void *p2;
{
  const struct file_info *s1 = p1;
  const struct file_info *s2 = p2;
  unsigned char *cp1;
  unsigned char *cp2;

  /* Take care of file names without directories.  */
  if (s1->path == s1->fname)
    return -1;
  else if (s2->path == s2->fname)
    return 1;

  cp1 = (unsigned char *) s1->path;
  cp2 = (unsigned char *) s2->path;

  while (1)
    {
      ++cp1;
      ++cp2;
      /* Reached the end of the first path?  */
      if (cp1 == (unsigned char *) s1->fname)
	/* It doesn't really matter in which order files from the
	   same directory are sorted in.  Therefore don't test for
	   the second path reaching the end.  */
	return -1;
      else if (cp2 == (unsigned char *) s2->fname)
	return 1;

      /* Character of current path component the same?  */
      if (*cp1 != *cp2)
	return *cp1 - *cp2;
    }
}

/* Output the directory table and the file name table.  We try to minimize
   the total amount of memory needed.  A heuristic is used to avoid large
   slowdowns with many input files.  */
static void
output_file_names ()
{
  struct file_info *files;
  struct dir_info *dirs;
  int *saved;
  int *savehere;
  int *backmap;
  int ndirs;
  int idx_offset;
  int i;
  int idx;

  /* Allocate the various arrays we need.  */
  files = (struct file_info *) alloca (file_table.in_use
				       * sizeof (struct file_info));
  dirs = (struct dir_info *) alloca (file_table.in_use
				     * sizeof (struct dir_info));

  /* Sort the file names.  */
  for (i = 1; i < (int) file_table.in_use; ++i)
    {
      char *f;

      /* Skip all leading "./".  */
      f = file_table.table[i];
      while (f[0] == '.' && f[1] == '/')
	f += 2;

      /* Create a new array entry.  */
      files[i].path = f;
      files[i].length = strlen (f);
      files[i].file_idx = i;

      /* Search for the file name part.  */
      f = strrchr (f, '/');
      files[i].fname = f == NULL ? files[i].path : f + 1;
    }
  qsort (files + 1, file_table.in_use - 1, sizeof (files[0]), file_info_cmp);

  /* Find all the different directories used.  */
  dirs[0].path = files[1].path;
  dirs[0].length = files[1].fname - files[1].path;
  dirs[0].prefix = -1;
  dirs[0].count = 1;
  dirs[0].dir_idx = 0;
  dirs[0].used = 0;
  files[1].dir_idx = 0;
  ndirs = 1;

  for (i = 2; i < (int) file_table.in_use; ++i)
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
	dirs[ndirs].used = 0;
	files[i].dir_idx = ndirs;

	/* Search for a prefix.  */
	dirs[ndirs].prefix = -1;
	for (j = 0; j < ndirs; ++j)
	  if (dirs[j].length < dirs[ndirs].length
	      && dirs[j].length > 1
	      && (dirs[ndirs].prefix == -1
		  || dirs[j].length > dirs[dirs[ndirs].prefix].length)
	      && memcmp (dirs[j].path, dirs[ndirs].path, dirs[j].length) == 0)
	    dirs[ndirs].prefix = j;

	++ndirs;
      }

  /* Now to the actual work.  We have to find a subset of the
     directories which allow expressing the file name using references
     to the directory table with the least amount of characters.  We
     do not do an exhaustive search where we would have to check out
     every combination of every single possible prefix.  Instead we
     use a heuristic which provides nearly optimal results in most
     cases and never is much off.  */
  saved = (int *) alloca (ndirs * sizeof (int));
  savehere = (int *) alloca (ndirs * sizeof (int));

  memset (saved, '\0', ndirs * sizeof (saved[0]));
  for (i = 0; i < ndirs; ++i)
    {
      int j;
      int total;

      /* We can always save some space for the current directory.  But
	 this does not mean it will be enough to justify adding the
	 directory.  */
      savehere[i] = dirs[i].length;
      total = (savehere[i] - saved[i]) * dirs[i].count;

      for (j = i + 1; j < ndirs; ++j)
	{
	  savehere[j] = 0;

	  if (saved[j] < dirs[i].length)
	    {
	      /* Determine whether the dirs[i] path is a prefix of the
		 dirs[j] path.  */
	      int k;

	      k = dirs[j].prefix;
	      while (k != -1 && k != i)
		k = dirs[k].prefix;

	      if (k == i)
		{
		  /* Yes it is.  We can possibly safe some memory but
		     writing the filenames in dirs[j] relative to
		     dirs[i].  */
		  savehere[j] = dirs[i].length;
		  total += (savehere[j] - saved[j]) * dirs[j].count;
		}
	    }
	}

      /* Check whether we can safe enough to justify adding the dirs[i]
	 directory.  */
      if (total > dirs[i].length + 1)
	{
	  /* It's worthwhile adding.  */
          for (j = i; j < ndirs; ++j)
	    if (savehere[j] > 0)
	      {
		/* Remember how much we saved for this directory so far.  */
		saved[j] = savehere[j];

		/* Remember the prefix directory.  */
		dirs[j].dir_idx = i;
	      }
	}
    }

  /* We have to emit them in the order they appear in the file_table
     array since the index is used in the debug info generation.  To
     do this efficiently we generate a back-mapping of the indices
     first.  */
  backmap = (int *) alloca (file_table.in_use * sizeof (int));
  for (i = 1; i < (int) file_table.in_use; ++i)
    {
      backmap[files[i].file_idx] = i;
      /* Mark this directory as used.  */
      dirs[dirs[files[i].dir_idx].dir_idx].used = 1;
    }

  /* That was it.  We are ready to emit the information.  First the
     directory name table.  Here we have to make sure that the first
     actually emitted directory name has the index one.  Zero is
     reserved for the current working directory.  Make sure we do not
     confuse these indices with the one for the constructed table
     (even though most of the time they are identical).  */
  idx = 1;
  idx_offset = dirs[0].length > 0 ? 1 : 0;
  for (i = 1 - idx_offset; i < ndirs; ++i)
    if (dirs[i].used != 0)
      {
	dirs[i].used = idx++;
	dw2_asm_output_nstring (dirs[i].path, dirs[i].length - 1,
				"Directory Entry: 0x%x", dirs[i].used);
      }
  dw2_asm_output_data (1, 0, "End directory table");

  /* Correct the index for the current working directory entry if it
     exists.  */
  if (idx_offset == 0)
    dirs[0].used = 0;

  /* Now write all the file names.  */
  for (i = 1; i < (int) file_table.in_use; ++i)
    {
      int file_idx = backmap[i];
      int dir_idx = dirs[files[file_idx].dir_idx].dir_idx;

      dw2_asm_output_nstring (files[file_idx].path + dirs[dir_idx].length, -1,
			      "File Entry: 0x%x", i);

      /* Include directory index.  */
      dw2_asm_output_data_uleb128 (dirs[dir_idx].used, NULL);

      /* Modification time.  */
      dw2_asm_output_data_uleb128 (0, NULL);

      /* File length in bytes.  */
      dw2_asm_output_data_uleb128 (0, NULL);
    }
  dw2_asm_output_data (1, 0, "End file name table");
}


/* Output the source line number correspondence information.  This
   information goes into the .debug_line section.  */

static void
output_line_info ()
{
  char l1[20], l2[20], p1[20], p2[20];
  char line_label[MAX_ARTIFICIAL_LABEL_BYTES];
  char prev_line_label[MAX_ARTIFICIAL_LABEL_BYTES];
  register unsigned opc;
  register unsigned n_op_args;
  register unsigned long lt_index;
  register unsigned long current_line;
  register long line_offset;
  register long line_delta;
  register unsigned long current_file;
  register unsigned long function;

  ASM_GENERATE_INTERNAL_LABEL (l1, LINE_NUMBER_BEGIN_LABEL, 0);
  ASM_GENERATE_INTERNAL_LABEL (l2, LINE_NUMBER_END_LABEL, 0);
  ASM_GENERATE_INTERNAL_LABEL (p1, LN_PROLOG_AS_LABEL, 0);
  ASM_GENERATE_INTERNAL_LABEL (p2, LN_PROLOG_END_LABEL, 0);

  dw2_asm_output_delta (DWARF_OFFSET_SIZE, l2, l1,
			"Length of Source Line Info");
  ASM_OUTPUT_LABEL (asm_out_file, l1);

  dw2_asm_output_data (2, DWARF_VERSION, "DWARF Version");

  dw2_asm_output_delta (DWARF_OFFSET_SIZE, p2, p1, "Prolog Length");
  ASM_OUTPUT_LABEL (asm_out_file, p1);

  dw2_asm_output_data (1, DWARF_LINE_MIN_INSTR_LENGTH,
		       "Minimum Instruction Length");

  dw2_asm_output_data (1, DWARF_LINE_DEFAULT_IS_STMT_START,
		       "Default is_stmt_start flag");

  dw2_asm_output_data (1, DWARF_LINE_BASE,
		       "Line Base Value (Special Opcodes)");

  dw2_asm_output_data (1, DWARF_LINE_RANGE,
		       "Line Range Value (Special Opcodes)");

  dw2_asm_output_data (1, DWARF_LINE_OPCODE_BASE,
		       "Special Opcode Base");

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

      dw2_asm_output_data (1, n_op_args, "opcode: 0x%x has %d args",
			   opc, n_op_args);
    }

  /* Write out the information about the files we use.  */
  output_file_names ();
  ASM_OUTPUT_LABEL (asm_out_file, p2);

  /* We used to set the address register to the first location in the text
     section here, but that didn't accomplish anything since we already
     have a line note for the opening brace of the first function.  */

  /* Generate the line number to PC correspondence table, encoded as
     a series of state machine operations.  */
  current_file = 1;
  current_line = 1;
  strcpy (prev_line_label, text_section_label);
  for (lt_index = 1; lt_index < line_info_table_in_use; ++lt_index)
    {
      register dw_line_info_ref line_info = &line_info_table[lt_index];

#if 0
      /* Disable this optimization for now; GDB wants to see two line notes
	 at the beginning of a function so it can find the end of the
	 prologue.  */

      /* Don't emit anything for redundant notes.  Just updating the
         address doesn't accomplish anything, because we already assume
         that anything after the last address is this line.  */
      if (line_info->dw_line_num == current_line
	  && line_info->dw_file_num == current_file)
	continue;
#endif

      /* Emit debug info for the address of the current line.

	 Unfortunately, we have little choice here currently, and must always
	 use the most general form.  Gcc does not know the address delta
	 itself, so we can't use DW_LNS_advance_pc.  Many ports do have length
	 attributes which will give an upper bound on the address range.  We
	 could perhaps use length attributes to determine when it is safe to
	 use DW_LNS_fixed_advance_pc.  */

      ASM_GENERATE_INTERNAL_LABEL (line_label, LINE_CODE_LABEL, lt_index);
      if (0)
	{
	  /* This can handle deltas up to 0xffff.  This takes 3 bytes.  */
	  dw2_asm_output_data (1, DW_LNS_fixed_advance_pc,
			       "DW_LNS_fixed_advance_pc");
	  dw2_asm_output_delta (2, line_label, prev_line_label, NULL);
	}
      else
	{
	  /* This can handle any delta.  This takes
             4+DWARF2_ADDR_SIZE bytes.  */
	  dw2_asm_output_data (1, 0, "DW_LNE_set_address");
	  dw2_asm_output_data_uleb128 (1 + DWARF2_ADDR_SIZE, NULL);
	  dw2_asm_output_data (1, DW_LNE_set_address, NULL);
	  dw2_asm_output_addr (DWARF2_ADDR_SIZE, line_label, NULL);
	}
      strcpy (prev_line_label, line_label);

      /* Emit debug info for the source file of the current line, if
	 different from the previous line.  */
      if (line_info->dw_file_num != current_file)
	{
	  current_file = line_info->dw_file_num;
	  dw2_asm_output_data (1, DW_LNS_set_file, "DW_LNS_set_file");
	  dw2_asm_output_data_uleb128 (current_file, "(\"%s\")",
				       file_table.table[current_file]);
	}

      /* Emit debug info for the current line number, choosing the encoding
	 that uses the least amount of space.  */
      if (line_info->dw_line_num != current_line)
	{
	  line_offset = line_info->dw_line_num - current_line;
	  line_delta = line_offset - DWARF_LINE_BASE;
	  current_line = line_info->dw_line_num;
	  if (line_delta >= 0 && line_delta < (DWARF_LINE_RANGE - 1))
	    {
	      /* This can handle deltas from -10 to 234, using the current
		 definitions of DWARF_LINE_BASE and DWARF_LINE_RANGE.  This
		 takes 1 byte.  */
	      dw2_asm_output_data (1, DWARF_LINE_OPCODE_BASE + line_delta,
				   "line %lu", current_line);
	    }
	  else
	    {
	      /* This can handle any delta.  This takes at least 4 bytes,
		 depending on the value being encoded.  */
	      dw2_asm_output_data (1, DW_LNS_advance_line,
				   "advance to line %lu", current_line);
	      dw2_asm_output_data_sleb128 (line_offset, NULL);
	      dw2_asm_output_data (1, DW_LNS_copy, "DW_LNS_copy");
	    }
	}
      else
	{
	  /* We still need to start a new row, so output a copy insn.  */
	  dw2_asm_output_data (1, DW_LNS_copy, "DW_LNS_copy");
	}
    }

  /* Emit debug info for the address of the end of the function.  */
  if (0)
    {
      dw2_asm_output_data (1, DW_LNS_fixed_advance_pc,
			   "DW_LNS_fixed_advance_pc");
      dw2_asm_output_delta (2, text_end_label, prev_line_label, NULL);
    }
  else
    {
      dw2_asm_output_data (1, 0, "DW_LNE_set_address");
      dw2_asm_output_data_uleb128 (1 + DWARF2_ADDR_SIZE, NULL);
      dw2_asm_output_data (1, DW_LNE_set_address, NULL);
      dw2_asm_output_addr (DWARF2_ADDR_SIZE, text_end_label, NULL);
    }

  dw2_asm_output_data (1, 0, "DW_LNE_end_sequence");
  dw2_asm_output_data_uleb128 (1, NULL);
  dw2_asm_output_data (1, DW_LNE_end_sequence, NULL);

  function = 0;
  current_file = 1;
  current_line = 1;
  for (lt_index = 0; lt_index < separate_line_info_table_in_use;)
    {
      register dw_separate_line_info_ref line_info
	= &separate_line_info_table[lt_index];

#if 0
      /* Don't emit anything for redundant notes.  */
      if (line_info->dw_line_num == current_line
	  && line_info->dw_file_num == current_file
	  && line_info->function == function)
	goto cont;
#endif

      /* Emit debug info for the address of the current line.  If this is
	 a new function, or the first line of a function, then we need
	 to handle it differently.  */
      ASM_GENERATE_INTERNAL_LABEL (line_label, SEPARATE_LINE_CODE_LABEL,
				   lt_index);
      if (function != line_info->function)
	{
	  function = line_info->function;

	  /* Set the address register to the first line in the function */
	  dw2_asm_output_data (1, 0, "DW_LNE_set_address");
	  dw2_asm_output_data_uleb128 (1 + DWARF2_ADDR_SIZE, NULL);
	  dw2_asm_output_data (1, DW_LNE_set_address, NULL);
	  dw2_asm_output_addr (DWARF2_ADDR_SIZE, line_label, NULL);
	}
      else
	{
	  /* ??? See the DW_LNS_advance_pc comment above.  */
	  if (0)
	    {
	      dw2_asm_output_data (1, DW_LNS_fixed_advance_pc,
				   "DW_LNS_fixed_advance_pc");
	      dw2_asm_output_delta (2, line_label, prev_line_label, NULL);
	    }
	  else
	    {
	      dw2_asm_output_data (1, 0, "DW_LNE_set_address");
	      dw2_asm_output_data_uleb128 (1 + DWARF2_ADDR_SIZE, NULL);
	      dw2_asm_output_data (1, DW_LNE_set_address, NULL);
	      dw2_asm_output_addr (DWARF2_ADDR_SIZE, line_label, NULL);
	    }
	}
      strcpy (prev_line_label, line_label);

      /* Emit debug info for the source file of the current line, if
	 different from the previous line.  */
      if (line_info->dw_file_num != current_file)
	{
	  current_file = line_info->dw_file_num;
	  dw2_asm_output_data (1, DW_LNS_set_file, "DW_LNS_set_file");
	  dw2_asm_output_data_uleb128 (current_file, "(\"%s\")",
				       file_table.table[current_file]);
	}

      /* Emit debug info for the current line number, choosing the encoding
	 that uses the least amount of space.  */
      if (line_info->dw_line_num != current_line)
	{
	  line_offset = line_info->dw_line_num - current_line;
	  line_delta = line_offset - DWARF_LINE_BASE;
	  current_line = line_info->dw_line_num;
	  if (line_delta >= 0 && line_delta < (DWARF_LINE_RANGE - 1))
	    dw2_asm_output_data (1, DWARF_LINE_OPCODE_BASE + line_delta,
				 "line %lu", current_line);
	  else
	    {
	      dw2_asm_output_data (1, DW_LNS_advance_line,
				   "advance to line %lu", current_line);
	      dw2_asm_output_data_sleb128 (line_offset, NULL);
	      dw2_asm_output_data (1, DW_LNS_copy, "DW_LNS_copy");
	    }
	}
      else
	dw2_asm_output_data (1, DW_LNS_copy, "DW_LNS_copy");

#if 0
    cont:
#endif
      ++lt_index;

      /* If we're done with a function, end its sequence.  */
      if (lt_index == separate_line_info_table_in_use
	  || separate_line_info_table[lt_index].function != function)
	{
	  current_file = 1;
	  current_line = 1;

	  /* Emit debug info for the address of the end of the function.  */
	  ASM_GENERATE_INTERNAL_LABEL (line_label, FUNC_END_LABEL, function);
	  if (0)
	    {
	      dw2_asm_output_data (1, DW_LNS_fixed_advance_pc,
				   "DW_LNS_fixed_advance_pc");
	      dw2_asm_output_delta (2, line_label, prev_line_label, NULL);
	    }
	  else
	    {
	      dw2_asm_output_data (1, 0, "DW_LNE_set_address");
	      dw2_asm_output_data_uleb128 (1 + DWARF2_ADDR_SIZE, NULL);
	      dw2_asm_output_data (1, DW_LNE_set_address, NULL);
	      dw2_asm_output_addr (DWARF2_ADDR_SIZE, line_label, NULL);
	    }

	  /* Output the marker for the end of this sequence.  */
	  dw2_asm_output_data (1, 0, "DW_LNE_end_sequence");
	  dw2_asm_output_data_uleb128 (1, NULL);
	  dw2_asm_output_data (1, DW_LNE_end_sequence, NULL);
	}
    }

  /* Output the marker for the end of the line number info.  */
  ASM_OUTPUT_LABEL (asm_out_file, l2);
}

/* Given a pointer to a tree node for some base type, return a pointer to
   a DIE that describes the given type.

   This routine must only be called for GCC type nodes that correspond to
   Dwarf base (fundamental) types.  */

static dw_die_ref
base_type_die (type)
     register tree type;
{
  register dw_die_ref base_type_result;
  register const char *type_name;
  register enum dwarf_type encoding;
  register tree name = TYPE_NAME (type);

  if (TREE_CODE (type) == ERROR_MARK
      || TREE_CODE (type) == VOID_TYPE)
    return 0;

  if (name)
    {
      if (TREE_CODE (name) == TYPE_DECL)
	name = DECL_NAME (name);

      type_name = IDENTIFIER_POINTER (name);
    }
  else
    type_name = "__unknown__";

  switch (TREE_CODE (type))
    {
    case INTEGER_TYPE:
      /* Carefully distinguish the C character types, without messing
         up if the language is not C. Note that we check only for the names
         that contain spaces; other names might occur by coincidence in other
         languages.  */
      if (! (TYPE_PRECISION (type) == CHAR_TYPE_SIZE
	     && (type == char_type_node
		 || ! strcmp (type_name, "signed char")
		 || ! strcmp (type_name, "unsigned char"))))
	{
	  if (TREE_UNSIGNED (type))
	    encoding = DW_ATE_unsigned;
	  else
	    encoding = DW_ATE_signed;
	  break;
	}
      /* else fall through.  */

    case CHAR_TYPE:
      /* GNU Pascal/Ada CHAR type.  Not used in C.  */
      if (TREE_UNSIGNED (type))
	encoding = DW_ATE_unsigned_char;
      else
	encoding = DW_ATE_signed_char;
      break;

    case REAL_TYPE:
      encoding = DW_ATE_float;
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
      abort (); /* No other TREE_CODEs are Dwarf fundamental types.  */
    }

  base_type_result = new_die (DW_TAG_base_type, comp_unit_die);
  if (demangle_name_func)
    type_name = (*demangle_name_func) (type_name);

  add_AT_string (base_type_result, DW_AT_name, type_name);
  add_AT_unsigned (base_type_result, DW_AT_byte_size,
		   int_size_in_bytes (type));
  add_AT_unsigned (base_type_result, DW_AT_encoding, encoding);

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

static inline int
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
    case VECTOR_TYPE:
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
  register tree item_type = NULL;

  if (code != ERROR_MARK)
    {
      tree qualified_type;

      /* See if we already have the appropriately qualified variant of
	 this type.  */
      qualified_type 
	= get_qualified_type (type,
			      ((is_const_type ? TYPE_QUAL_CONST : 0)
			       | (is_volatile_type 
				  ? TYPE_QUAL_VOLATILE : 0)));
      /* If we do, then we can just use its DIE, if it exists.  */
      if (qualified_type)
	{
	  mod_type_die = lookup_type_die (qualified_type);
	  if (mod_type_die)
	    return mod_type_die;
	}

      /* Handle C typedef types.  */
      if (qualified_type && TYPE_NAME (qualified_type) 
	  && TREE_CODE (TYPE_NAME (qualified_type)) == TYPE_DECL
	  && DECL_ORIGINAL_TYPE (TYPE_NAME (qualified_type)))
	{
	  tree type_name = TYPE_NAME (qualified_type);
	  tree dtype = TREE_TYPE (type_name);
	  if (qualified_type == dtype)
	    {
	      /* For a named type, use the typedef.  */
	      gen_type_die (qualified_type, context_die);
	      mod_type_die = lookup_type_die (qualified_type);
	    }

	  else if (is_const_type < TYPE_READONLY (dtype)
		   || is_volatile_type < TYPE_VOLATILE (dtype))
	    /* cv-unqualified version of named type.  Just use the unnamed
	       type to which it refers.  */
	    mod_type_die
	      = modified_type_die (DECL_ORIGINAL_TYPE (type_name),
				   is_const_type, is_volatile_type,
				   context_die);
	  /* Else cv-qualified version of named type; fall through.  */
	}

      if (mod_type_die)
	/* OK.  */
	;
      else if (is_const_type)
	{
	  mod_type_die = new_die (DW_TAG_const_type, comp_unit_die);
	  sub_die = modified_type_die (type, 0, is_volatile_type, context_die);
	}
      else if (is_volatile_type)
	{
	  mod_type_die = new_die (DW_TAG_volatile_type, comp_unit_die);
	  sub_die = modified_type_die (type, 0, 0, context_die);
	}
      else if (code == POINTER_TYPE)
	{
	  mod_type_die = new_die (DW_TAG_pointer_type, comp_unit_die);
	  add_AT_unsigned (mod_type_die, DW_AT_byte_size, PTR_SIZE);
#if 0
	  add_AT_unsigned (mod_type_die, DW_AT_address_class, 0);
#endif
	  item_type = TREE_TYPE (type);
	}
      else if (code == REFERENCE_TYPE)
	{
	  mod_type_die = new_die (DW_TAG_reference_type, comp_unit_die);
	  add_AT_unsigned (mod_type_die, DW_AT_byte_size, PTR_SIZE);
#if 0
	  add_AT_unsigned (mod_type_die, DW_AT_address_class, 0);
#endif
	  item_type = TREE_TYPE (type);
	}
      else if (is_base_type (type))
	mod_type_die = base_type_die (type);
      else
	{
	  gen_type_die (type, context_die);

	  /* We have to get the type_main_variant here (and pass that to the
	     `lookup_type_die' routine) because the ..._TYPE node we have
	     might simply be a *copy* of some original type node (where the
	     copy was created to help us keep track of typedef names) and
	     that copy might have a different TYPE_UID from the original
	     ..._TYPE node.  */
	  mod_type_die = lookup_type_die (type_main_variant (type));
	  if (mod_type_die == NULL)
	    abort ();
	}

      /* We want to equate the qualified type to the die below.  */
      if (qualified_type)
	type = qualified_type;
    }

  equate_type_number_to_die (type, mod_type_die);
  if (item_type)
    /* We must do this after the equate_type_number_to_die call, in case
       this is a recursive type.  This ensures that the modified_type_die
       recursion will terminate even if the type is recursive.  Recursive
       types are possible in Ada.  */
    sub_die = modified_type_die (item_type,
				 TYPE_READONLY (item_type),
				 TYPE_VOLATILE (item_type),
				 context_die);

  if (sub_die != NULL)
    add_AT_die_ref (mod_type_die, DW_AT_type, sub_die);

  return mod_type_die;
}

/* Given a pointer to an arbitrary ..._TYPE tree node, return true if it is
   an enumerated type.   */

static inline int
type_is_enum (type)
     register tree type;
{
  return TREE_CODE (type) == ENUMERAL_TYPE;
}

/* Return the register number described by a given RTL node.  */

static unsigned int
reg_number (rtl)
     register rtx rtl;
{
  register unsigned regno = REGNO (rtl);

  if (regno >= FIRST_PSEUDO_REGISTER)
    {
      warning ("internal regno botch: regno = %d\n", regno);
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

  if (reg <= 31)
    loc_result = new_loc_descr (DW_OP_reg0 + reg, 0, 0);
  else
    loc_result = new_loc_descr (DW_OP_regx, reg, 0);

  return loc_result;
}

/* Return a location descriptor that designates a constant.  */

static dw_loc_descr_ref
int_loc_descriptor (i)
     HOST_WIDE_INT i;
{
  enum dwarf_location_atom op;

  /* Pick the smallest representation of a constant, rather than just
     defaulting to the LEB encoding.  */
  if (i >= 0)
    {
      if (i <= 31)
	op = DW_OP_lit0 + i;
      else if (i <= 0xff)
	op = DW_OP_const1u;
      else if (i <= 0xffff)
	op = DW_OP_const2u;
      else if (HOST_BITS_PER_WIDE_INT == 32
	       || i <= 0xffffffff)
	op = DW_OP_const4u;
      else
	op = DW_OP_constu;
    }
  else
    {
      if (i >= -0x80)
	op = DW_OP_const1s;
      else if (i >= -0x8000)
	op = DW_OP_const2s;
      else if (HOST_BITS_PER_WIDE_INT == 32
	       || i >= -0x80000000)
	op = DW_OP_const4s;
      else
	op = DW_OP_consts;
    }

  return new_loc_descr (op, i, 0);
}

/* Return a location descriptor that designates a base+offset location.  */

static dw_loc_descr_ref
based_loc_descr (reg, offset)
     unsigned reg;
     long int offset;
{
  register dw_loc_descr_ref loc_result;
  /* For the "frame base", we use the frame pointer or stack pointer
     registers, since the RTL for local variables is relative to one of
     them.  */
  register unsigned fp_reg = DBX_REGISTER_NUMBER (frame_pointer_needed
						  ? HARD_FRAME_POINTER_REGNUM
						  : STACK_POINTER_REGNUM);

  if (reg == fp_reg)
    loc_result = new_loc_descr (DW_OP_fbreg, offset, 0);
  else if (reg <= 31)
    loc_result = new_loc_descr (DW_OP_breg0 + reg, offset, 0);
  else
    loc_result = new_loc_descr (DW_OP_bregx, reg, offset);

  return loc_result;
}

/* Return true if this RTL expression describes a base+offset calculation.  */

static inline int
is_based_loc (rtl)
     register rtx rtl;
{
    return (GET_CODE (rtl) == PLUS
	    && ((GET_CODE (XEXP (rtl, 0)) == REG
		 && GET_CODE (XEXP (rtl, 1)) == CONST_INT)));
}

/* The following routine converts the RTL for a variable or parameter
   (resident in memory) into an equivalent Dwarf representation of a
   mechanism for getting the address of that same variable onto the top of a
   hypothetical "address evaluation" stack.

   When creating memory location descriptors, we are effectively transforming
   the RTL for a memory-resident object into its Dwarf postfix expression
   equivalent.  This routine recursively descends an RTL tree, turning
   it into Dwarf postfix code as it goes.

   MODE is the mode of the memory reference, needed to handle some
   autoincrement addressing modes.  */

static dw_loc_descr_ref
mem_loc_descriptor (rtl, mode)
     register rtx rtl;
     enum machine_mode mode;
{
  dw_loc_descr_ref mem_loc_result = NULL;
  /* Note that for a dynamically sized array, the location we will generate a
     description of here will be the lowest numbered location which is
     actually within the array.  That's *not* necessarily the same as the
     zeroth element of the array.  */

#ifdef ASM_SIMPLIFY_DWARF_ADDR
  rtl = ASM_SIMPLIFY_DWARF_ADDR (rtl);
#endif

  switch (GET_CODE (rtl))
    {
    case POST_INC:
    case POST_DEC:
    case POST_MODIFY:
      /* POST_INC and POST_DEC can be handled just like a SUBREG.  So we
	 just fall into the SUBREG code.  */

      /* Fall through.  */

    case SUBREG:
      /* The case of a subreg may arise when we have a local (register)
         variable or a formal (register) parameter which doesn't quite fill
         up an entire register.  For now, just assume that it is
         legitimate to make the Dwarf info refer to the whole register which
         contains the given subreg.  */
      rtl = SUBREG_REG (rtl);

      /* Fall through.  */

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
      mem_loc_result = mem_loc_descriptor (XEXP (rtl, 0), GET_MODE (rtl));
      add_loc_descr (&mem_loc_result, new_loc_descr (DW_OP_deref, 0, 0));
      break;

    case LABEL_REF:
      /* Some ports can transform a symbol ref into a label ref, because
 	 the symbol ref is too far away and has to be dumped into a constant
 	 pool.  */
    case CONST:
    case SYMBOL_REF:
      /* Alternatively, the symbol in the constant pool might be referenced
	 by a different symbol.  */
      if (GET_CODE (rtl) == SYMBOL_REF
	  && CONSTANT_POOL_ADDRESS_P (rtl))
	{
	  rtx tmp = get_pool_constant (rtl);
	  if (GET_CODE (tmp) == SYMBOL_REF)
	    rtl = tmp;
	}

      mem_loc_result = new_loc_descr (DW_OP_addr, 0, 0);
      mem_loc_result->dw_loc_oprnd1.val_class = dw_val_class_addr;
      mem_loc_result->dw_loc_oprnd1.v.val_addr = save_rtx (rtl);
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
      rtl = gen_rtx_PLUS (word_mode, XEXP (rtl, 0),
			  GEN_INT (GET_CODE (rtl) == PRE_INC
				   ? GET_MODE_UNIT_SIZE (mode)
				   : -GET_MODE_UNIT_SIZE (mode)));

      /* Fall through.  */

    case PLUS:
    plus:
      if (is_based_loc (rtl))
	mem_loc_result = based_loc_descr (reg_number (XEXP (rtl, 0)),
					  INTVAL (XEXP (rtl, 1)));
      else
	{
	  mem_loc_result = mem_loc_descriptor (XEXP (rtl, 0), mode);

	  if (GET_CODE (XEXP (rtl, 1)) == CONST_INT
	      && INTVAL (XEXP (rtl, 1)) >= 0)
	    {
	      add_loc_descr (&mem_loc_result,
			     new_loc_descr (DW_OP_plus_uconst,
					    INTVAL (XEXP (rtl, 1)), 0));
	    }
	  else
	    {
	      add_loc_descr (&mem_loc_result,
			     mem_loc_descriptor (XEXP (rtl, 1), mode));
	      add_loc_descr (&mem_loc_result,
			     new_loc_descr (DW_OP_plus, 0, 0));
	    }
	}
      break;

    case MULT:
      /* If a pseudo-reg is optimized away, it is possible for it to
	 be replaced with a MEM containing a multiply.  */
      add_loc_descr (&mem_loc_result,
		     mem_loc_descriptor (XEXP (rtl, 0), mode));
      add_loc_descr (&mem_loc_result,
		     mem_loc_descriptor (XEXP (rtl, 1), mode));
      add_loc_descr (&mem_loc_result, new_loc_descr (DW_OP_mul, 0, 0));
      break;

    case CONST_INT:
      mem_loc_result = int_loc_descriptor (INTVAL (rtl));
      break;

    default:
      abort ();
    }

  return mem_loc_result;
}

/* Return a descriptor that describes the concatenation of two locations.
   This is typically a complex variable.  */

static dw_loc_descr_ref
concat_loc_descriptor (x0, x1)
     register rtx x0, x1;
{
  dw_loc_descr_ref cc_loc_result = NULL;

  if (!is_pseudo_reg (x0)
      && (GET_CODE (x0) != MEM || !is_pseudo_reg (XEXP (x0, 0))))
    add_loc_descr (&cc_loc_result, loc_descriptor (x0));
  add_loc_descr (&cc_loc_result,
	         new_loc_descr (DW_OP_piece, GET_MODE_SIZE (GET_MODE (x0)), 0));

  if (!is_pseudo_reg (x1)
      && (GET_CODE (x1) != MEM || !is_pseudo_reg (XEXP (x1, 0))))
    add_loc_descr (&cc_loc_result, loc_descriptor (x1));
  add_loc_descr (&cc_loc_result,
		 new_loc_descr (DW_OP_piece, GET_MODE_SIZE (GET_MODE (x1)), 0));

  return cc_loc_result;
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
         up an entire register.  For now, just assume that it is
         legitimate to make the Dwarf info refer to the whole register which
         contains the given subreg.  */
      rtl = SUBREG_REG (rtl);

      /* Fall through.  */

    case REG:
      loc_result = reg_loc_descriptor (rtl);
      break;

    case MEM:
      loc_result = mem_loc_descriptor (XEXP (rtl, 0), GET_MODE (rtl));
      break;

    case CONCAT:
      loc_result = concat_loc_descriptor (XEXP (rtl, 0), XEXP (rtl, 1));
      break;

    default:
      abort ();
    }

  return loc_result;
}

/* Similar, but generate the descriptor from trees instead of rtl.
   This comes up particularly with variable length arrays.  */

static dw_loc_descr_ref
loc_descriptor_from_tree (loc, addressp)
     tree loc;
     int addressp;
{
  dw_loc_descr_ref ret = NULL;
  int indirect_size = 0;
  int unsignedp = TREE_UNSIGNED (TREE_TYPE (loc));
  enum dwarf_location_atom op;

  /* ??? Most of the time we do not take proper care for sign/zero
     extending the values properly.  Hopefully this won't be a real
     problem...  */

  switch (TREE_CODE (loc))
    {
    case ERROR_MARK:
      break;

    case WITH_RECORD_EXPR:
      /* This case involves extracting fields from an object to determine the
	 position of other fields.  We don't try to encode this here.  The
	 only user of this is Ada, which encodes the needed information using
	 the names of types.  */
      return ret;

    case VAR_DECL:
    case PARM_DECL:
      {
	rtx rtl = rtl_for_decl_location (loc);
	enum machine_mode mode = DECL_MODE (loc);

	if (rtl == NULL_RTX)
	  break;
	else if (CONSTANT_P (rtl))
	  {
	    ret = new_loc_descr (DW_OP_addr, 0, 0);
	    ret->dw_loc_oprnd1.val_class = dw_val_class_addr;
	    ret->dw_loc_oprnd1.v.val_addr = rtl;
	    indirect_size = GET_MODE_SIZE (mode);
	  }
	else
	  {
	    if (GET_CODE (rtl) == MEM)
	      {
		indirect_size = GET_MODE_SIZE (mode);
		rtl = XEXP (rtl, 0);
	      }
	    ret = mem_loc_descriptor (rtl, mode);
	  }
      }
      break;

    case INDIRECT_REF:
      ret = loc_descriptor_from_tree (TREE_OPERAND (loc, 0), 0);
      indirect_size = GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (loc)));
      break;

    case NOP_EXPR:
    case CONVERT_EXPR:
    case NON_LVALUE_EXPR:
    case SAVE_EXPR:
      return loc_descriptor_from_tree (TREE_OPERAND (loc, 0), addressp);

    case COMPONENT_REF:
    case BIT_FIELD_REF:
    case ARRAY_REF:
    case ARRAY_RANGE_REF:
      {
	tree obj, offset;
	HOST_WIDE_INT bitsize, bitpos, bytepos;
	enum machine_mode mode;
	int volatilep;
	unsigned int alignment;

	obj = get_inner_reference (loc, &bitsize, &bitpos, &offset, &mode,
				   &unsignedp, &volatilep, &alignment);
	ret = loc_descriptor_from_tree (obj, 1);

	if (offset != NULL_TREE)
	  {
	    /* Variable offset.  */
	    add_loc_descr (&ret, loc_descriptor_from_tree (offset, 0));
	    add_loc_descr (&ret, new_loc_descr (DW_OP_plus, 0, 0));
	  }

	if (addressp)
	  {
	    /* We cannot address anything not on a unit boundary.  */
	    if (bitpos % BITS_PER_UNIT != 0)
	      abort ();
	  }
	else
	  {
	    if (bitpos % BITS_PER_UNIT != 0
		|| bitsize % BITS_PER_UNIT != 0)
	      {
		/* ??? We could handle this by loading and shifting etc.
		   Wait until someone needs it before expending the effort.  */
		abort ();
	      }

	    indirect_size = bitsize / BITS_PER_UNIT;
	  }

	bytepos = bitpos / BITS_PER_UNIT;
	if (bytepos > 0)
	  add_loc_descr (&ret, new_loc_descr (DW_OP_plus_uconst, bytepos, 0));
	else if (bytepos < 0)
	  {
	    add_loc_descr (&ret, int_loc_descriptor (bytepos));
	    add_loc_descr (&ret, new_loc_descr (DW_OP_plus, 0, 0));
	  }
	break;
      }

    case INTEGER_CST:
      if (host_integerp (loc, 0))
	ret = int_loc_descriptor (tree_low_cst (loc, 0));
      break;

    case BIT_AND_EXPR:
      op = DW_OP_and;
      goto do_binop;
    case BIT_XOR_EXPR:
      op = DW_OP_xor;
      goto do_binop;
    case BIT_IOR_EXPR:
      op = DW_OP_or;
      goto do_binop;
    case TRUNC_DIV_EXPR:
      op = DW_OP_div;
      goto do_binop;
    case MINUS_EXPR:
      op = DW_OP_minus;
      goto do_binop;
    case TRUNC_MOD_EXPR:
      op = DW_OP_mod;
      goto do_binop;
    case MULT_EXPR:
      op = DW_OP_mul;
      goto do_binop;
    case LSHIFT_EXPR:
      op = DW_OP_shl;
      goto do_binop;
    case RSHIFT_EXPR:
      op = (unsignedp ? DW_OP_shr : DW_OP_shra);
      goto do_binop;
    case PLUS_EXPR:
      if (TREE_CODE (TREE_OPERAND (loc, 1)) == INTEGER_CST
	  && host_integerp (TREE_OPERAND (loc, 1), 0))
	{
	  ret = loc_descriptor_from_tree (TREE_OPERAND (loc, 0), 0);
	  add_loc_descr (&ret,
			 new_loc_descr (DW_OP_plus_uconst,
					tree_low_cst (TREE_OPERAND (loc, 1),
						      0),
					0));
	  break;
	}
      op = DW_OP_plus;
      goto do_binop;
    case LE_EXPR:
      if (TREE_UNSIGNED (TREE_TYPE (TREE_OPERAND (loc, 0))))
	break;
      op = DW_OP_le;
      goto do_binop;
    case GE_EXPR:
      if (TREE_UNSIGNED (TREE_TYPE (TREE_OPERAND (loc, 0))))
	break;
      op = DW_OP_ge;
      goto do_binop;
    case LT_EXPR:
      if (TREE_UNSIGNED (TREE_TYPE (TREE_OPERAND (loc, 0))))
	break;
      op = DW_OP_lt;
      goto do_binop;
    case GT_EXPR:
      if (TREE_UNSIGNED (TREE_TYPE (TREE_OPERAND (loc, 0))))
	break;
      op = DW_OP_gt;
      goto do_binop;
    case EQ_EXPR:
      op = DW_OP_eq;
      goto do_binop;
    case NE_EXPR:
      op = DW_OP_ne;
      goto do_binop;

    do_binop:
      ret = loc_descriptor_from_tree (TREE_OPERAND (loc, 0), 0);
      add_loc_descr (&ret, loc_descriptor_from_tree (TREE_OPERAND (loc, 1), 0));
      add_loc_descr (&ret, new_loc_descr (op, 0, 0));
      break;

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
      ret = loc_descriptor_from_tree (TREE_OPERAND (loc, 0), 0);
      add_loc_descr (&ret, new_loc_descr (op, 0, 0));
      break;

    case MAX_EXPR:
      loc = build (COND_EXPR, TREE_TYPE (loc),
		   build (LT_EXPR, integer_type_node,
			  TREE_OPERAND (loc, 0), TREE_OPERAND (loc, 1)),
		   TREE_OPERAND (loc, 1), TREE_OPERAND (loc, 0));
      /* FALLTHRU */

    case COND_EXPR:
      {
	dw_loc_descr_ref bra_node, jump_node, tmp;

	ret = loc_descriptor_from_tree (TREE_OPERAND (loc, 0), 0);
	bra_node = new_loc_descr (DW_OP_bra, 0, 0);
	add_loc_descr (&ret, bra_node);

	tmp = loc_descriptor_from_tree (TREE_OPERAND (loc, 2), 0);
	add_loc_descr (&ret, tmp);
	jump_node = new_loc_descr (DW_OP_skip, 0, 0);
	add_loc_descr (&ret, jump_node);

	tmp = loc_descriptor_from_tree (TREE_OPERAND (loc, 1), 0);
	add_loc_descr (&ret, tmp);
	bra_node->dw_loc_oprnd1.val_class = dw_val_class_loc;
	bra_node->dw_loc_oprnd1.v.val_loc = tmp;

	/* ??? Need a node to point the skip at.  Use a nop.  */
	tmp = new_loc_descr (DW_OP_nop, 0, 0);
	add_loc_descr (&ret, tmp);
	jump_node->dw_loc_oprnd1.val_class = dw_val_class_loc;
	jump_node->dw_loc_oprnd1.v.val_loc = tmp;
      }
      break;

    default:
      abort ();
    }

  /* If we can't fill the request for an address, die.  */
  if (addressp && indirect_size == 0)
    abort ();

  /* If we've got an address and don't want one, dereference.  */
  if (!addressp && indirect_size > 0)
    {
      if (indirect_size > DWARF2_ADDR_SIZE)
	abort ();
      if (indirect_size == DWARF2_ADDR_SIZE)
	op = DW_OP_deref;
      else
	op = DW_OP_deref_size;
      add_loc_descr (&ret, new_loc_descr (op, indirect_size, 0));
    }

  return ret;
}

/* Given a value, round it up to the lowest multiple of `boundary'
   which is not less than the value itself.  */

static inline HOST_WIDE_INT
ceiling (value, boundary)
     HOST_WIDE_INT value;
     unsigned int boundary;
{
  return (((value + boundary - 1) / boundary) * boundary);
}

/* Given a pointer to what is assumed to be a FIELD_DECL node, return a
   pointer to the declared type for the relevant field variable, or return
   `integer_type_node' if the given node turns out to be an
   ERROR_MARK node.  */

static inline tree
field_type (decl)
     register tree decl;
{
  register tree type;

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
simple_type_align_in_bits (type)
     register tree type;
{
  return (TREE_CODE (type) != ERROR_MARK) ? TYPE_ALIGN (type) : BITS_PER_WORD;
}

static inline unsigned
simple_decl_align_in_bits (decl)
     register tree decl;
{
  return (TREE_CODE (decl) != ERROR_MARK) ? DECL_ALIGN (decl) : BITS_PER_WORD;
}

/* Given a pointer to a tree node, assumed to be some kind of a ..._TYPE
   node, return the size in bits for the type if it is a constant, or else
   return the alignment for the type if the type's size is not constant, or
   else return BITS_PER_WORD if the type actually turns out to be an
   ERROR_MARK node.  */

static inline unsigned HOST_WIDE_INT
simple_type_size_in_bits (type)
     register tree type;
{
  tree type_size_tree;

  if (TREE_CODE (type) == ERROR_MARK)
    return BITS_PER_WORD;
  type_size_tree = TYPE_SIZE (type);

  if (type_size_tree == NULL_TREE)
    return 0;
  if (! host_integerp (type_size_tree, 1))
    return TYPE_ALIGN (type);
  return tree_low_cst (type_size_tree, 1);
}

/* Given a pointer to what is assumed to be a FIELD_DECL node, compute and
   return the byte offset of the lowest addressed byte of the "containing
   object" for the given FIELD_DECL, or return 0 if we are unable to
   determine what that offset is, either because the argument turns out to
   be a pointer to an ERROR_MARK node, or because the offset is actually
   variable.  (We can't handle the latter case just yet).  */

static HOST_WIDE_INT
field_byte_offset (decl)
     register tree decl;
{
  unsigned int type_align_in_bits;
  unsigned int decl_align_in_bits;
  unsigned HOST_WIDE_INT type_size_in_bits;
  HOST_WIDE_INT object_offset_in_bits;
  HOST_WIDE_INT object_offset_in_bytes;
  tree type;
  tree field_size_tree;
  HOST_WIDE_INT bitpos_int;
  HOST_WIDE_INT deepest_bitpos;
  unsigned HOST_WIDE_INT field_size_in_bits;

  if (TREE_CODE (decl) == ERROR_MARK)
    return 0;

  if (TREE_CODE (decl) != FIELD_DECL)
    abort ();

  type = field_type (decl);
  field_size_tree = DECL_SIZE (decl);

  /* The size could be unspecified if there was an error, or for
     a flexible array member.  */
  if (! field_size_tree)
    field_size_tree = bitsize_zero_node;

  /* We cannot yet cope with fields whose positions are variable, so
     for now, when we see such things, we simply return 0.  Someday, we may
     be able to handle such cases, but it will be damn difficult.  */
  if (! host_integerp (bit_position (decl), 0))
    return 0;

  bitpos_int = int_bit_position (decl);

  /* If we don't know the size of the field, pretend it's a full word.  */
  if (host_integerp (field_size_tree, 1))
    field_size_in_bits = tree_low_cst (field_size_tree, 1);
  else
    field_size_in_bits = BITS_PER_WORD;

  type_size_in_bits = simple_type_size_in_bits (type);
  type_align_in_bits = simple_type_align_in_bits (type);
  decl_align_in_bits = simple_decl_align_in_bits (decl);

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
  object_offset_in_bits = deepest_bitpos - type_size_in_bits;

  /* Round up to type_align by default.  This works best for bitfields.  */
  object_offset_in_bits += type_align_in_bits - 1;
  object_offset_in_bits /= type_align_in_bits;
  object_offset_in_bits *= type_align_in_bits;

  if (object_offset_in_bits > bitpos_int)
    {
      /* Sigh, the decl must be packed.  */
      object_offset_in_bits = deepest_bitpos - type_size_in_bits;

      /* Round up to decl_align instead.  */
      object_offset_in_bits += decl_align_in_bits - 1;
      object_offset_in_bits /= decl_align_in_bits;
      object_offset_in_bits *= decl_align_in_bits;
    }

  object_offset_in_bytes = object_offset_in_bits / BITS_PER_UNIT;

  return object_offset_in_bytes;
}

/* The following routines define various Dwarf attributes and any data
   associated with them.  */

/* Add a location description attribute value to a DIE.

   This emits location attributes suitable for whole variables and
   whole parameters.  Note that the location attributes for struct fields are
   generated by the routine `data_member_location_attribute' below.  */

static void
add_AT_location_description (die, attr_kind, rtl)
     dw_die_ref die;
     enum dwarf_attribute attr_kind;
     register rtx rtl;
{
  /* Handle a special case.  If we are about to output a location descriptor
     for a variable or parameter which has been optimized out of existence,
     don't do that.  A variable which has been optimized out
     of existence will have a DECL_RTL value which denotes a pseudo-reg.
     Currently, in some rare cases, variables can have DECL_RTL values which
     look like (MEM (REG pseudo-reg#)).  These cases are due to bugs
     elsewhere in the compiler.  We treat such cases as if the variable(s) in
     question had been optimized out of existence.  */

  if (is_pseudo_reg (rtl)
      || (GET_CODE (rtl) == MEM
	  && is_pseudo_reg (XEXP (rtl, 0)))
      /* This can happen for a PARM_DECL with a DECL_INCOMING_RTL which
	 references the internal argument pointer (a pseudo) in a function
	 where all references to the internal argument pointer were
	 eliminated via the optimizers.  */
      || (GET_CODE (rtl) == MEM
	  && GET_CODE (XEXP (rtl, 0)) == PLUS
	  && is_pseudo_reg (XEXP (XEXP (rtl, 0), 0)))
      || (GET_CODE (rtl) == CONCAT
	  && is_pseudo_reg (XEXP (rtl, 0))
	  && is_pseudo_reg (XEXP (rtl, 1))))
    return;

  add_AT_loc (die, attr_kind, loc_descriptor (rtl));
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
  register unsigned long offset;
  register dw_loc_descr_ref loc_descr;
  register enum dwarf_location_atom op;

  if (TREE_CODE (decl) == TREE_VEC)
    offset = tree_low_cst (BINFO_OFFSET (decl), 0);
  else
    offset = field_byte_offset (decl);

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
      /* Note that a CONST_INT rtx could represent either an integer
	 or a floating-point constant.  A CONST_INT is used whenever
	 the constant will fit into a single word.  In all such
	 cases, the original mode of the constant value is wiped
	 out, and the CONST_INT rtx is assigned VOIDmode.  */
      {
	HOST_WIDE_INT val = INTVAL (rtl);
	
	/* ??? We really should be using HOST_WIDE_INT throughout.  */
	if (val < 0)
	  {
	    if ((long) val != val)
	      abort ();
	    add_AT_int (die, DW_AT_const_value, (long) val);
	  }
	else
	  {
	    if ((unsigned long) val != (unsigned HOST_WIDE_INT) val)
	      abort ();
	    add_AT_unsigned (die, DW_AT_const_value, (unsigned long) val);
	  }
      }
      break;

    case CONST_DOUBLE:
      /* Note that a CONST_DOUBLE rtx could represent either an integer or a
         floating-point constant.  A CONST_DOUBLE is used whenever the
         constant requires more than one word in order to be adequately
         represented.  We output CONST_DOUBLEs as blocks.  */
      {
	register enum machine_mode mode = GET_MODE (rtl);

	if (GET_MODE_CLASS (mode) == MODE_FLOAT)
	  {
	    register unsigned length = GET_MODE_SIZE (mode) / 4;
 	    long *array = (long *) xmalloc (sizeof (long) * length);
	    REAL_VALUE_TYPE rv;

	    REAL_VALUE_FROM_CONST_DOUBLE (rv, rtl);
	    switch (mode)
	      {
	      case SFmode:
		REAL_VALUE_TO_TARGET_SINGLE (rv, array[0]);
		break;

	      case DFmode:
		REAL_VALUE_TO_TARGET_DOUBLE (rv, array);
		break;

	      case XFmode:
	      case TFmode:
		REAL_VALUE_TO_TARGET_LONG_DOUBLE (rv, array);
		break;

	      default:
		abort ();
	      }

	    add_AT_float (die, DW_AT_const_value, length, array);
	  }
	else
	  {
	    /* ??? We really should be using HOST_WIDE_INT throughout.  */
	    if (HOST_BITS_PER_LONG != HOST_BITS_PER_WIDE_INT)
	      abort ();
	    add_AT_long_long (die, DW_AT_const_value,
			      CONST_DOUBLE_HIGH (rtl), CONST_DOUBLE_LOW (rtl));
	  }
      }
      break;

    case CONST_STRING:
      add_AT_string (die, DW_AT_const_value, XSTR (rtl, 0));
      break;

    case SYMBOL_REF:
    case LABEL_REF:
    case CONST:
      add_AT_addr (die, DW_AT_const_value, save_rtx (rtl));
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
         values in Dwarf, so for now we just punt and generate nothing.  */
      break;

    default:
      /* No other kinds of rtx should be possible here.  */
      abort ();
    }

}

static rtx
rtl_for_decl_location (decl)
     tree decl;
{
  register rtx rtl;

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
     parameter lives during most of the function's activation (i.e. between
     the end of the prologue and the start of the epilogue).  We'll do that
     as best as we can. Note however that if the given formal parameter is
     modified sometime during the execution of the function, then a stack
     backtrace (at debug-time) will show the function as having been
     called with the *new* value rather than the value which was
     originally passed in.  This happens rarely enough that it is not
     a major problem, but it *is* a problem, and I'd like to fix it.

     A future version of dwarf2out.c may generate two additional
     attributes for any given DW_TAG_formal_parameter DIE which will
     describe the "passed type" and the "passed location" for the
     given formal parameter in addition to the attributes we now
     generate to indicate the "declared type" and the "active
     location" for each parameter.  This additional set of attributes
     could be used by debuggers for stack backtraces. Separately, note
     that sometimes DECL_RTL can be NULL and DECL_INCOMING_RTL can be
     NULL also.  This happens (for example) for inlined-instances of
     inline function formal parameters which are never referenced.
     This really shouldn't be happening.  All PARM_DECL nodes should
     get valid non-NULL DECL_INCOMING_RTL values, but integrate.c
     doesn't currently generate these values for inlined instances of
     inline function parameters, so when we see such cases, we are
     just out-of-luck for the time being (until integrate.c
     gets fixed).  */

  /* Use DECL_RTL as the "location" unless we find something better.  */
  rtl = DECL_RTL_IF_SET (decl);

  if (TREE_CODE (decl) == PARM_DECL)
    {
      if (rtl == NULL_RTX || is_pseudo_reg (rtl))
	{
	  tree declared_type = type_main_variant (TREE_TYPE (decl));
	  tree passed_type = type_main_variant (DECL_ARG_TYPE (decl));

	  /* This decl represents a formal parameter which was optimized out.
	     Note that DECL_INCOMING_RTL may be NULL in here, but we handle
	     all* cases where (rtl == NULL_RTX) just below.  */
	  if (declared_type == passed_type)
	    rtl = DECL_INCOMING_RTL (decl);
	  else if (! BYTES_BIG_ENDIAN
		   && TREE_CODE (declared_type) == INTEGER_TYPE
		   && (GET_MODE_SIZE (TYPE_MODE (declared_type))
		       <= GET_MODE_SIZE (TYPE_MODE (passed_type))))
	    rtl = DECL_INCOMING_RTL (decl);
	}

      /* If the parm was passed in registers, but lives on the stack, then
	 make a big endian correction if the mode of the type of the
	 parameter is not the same as the mode of the rtl.  */
      /* ??? This is the same series of checks that are made in dbxout.c before
	 we reach the big endian correction code there.  It isn't clear if all
	 of these checks are necessary here, but keeping them all is the safe
	 thing to do.  */
      else if (GET_CODE (rtl) == MEM
	       && XEXP (rtl, 0) != const0_rtx
	       && ! CONSTANT_P (XEXP (rtl, 0))
	       /* Not passed in memory.  */
	       && GET_CODE (DECL_INCOMING_RTL (decl)) != MEM
	       /* Not passed by invisible reference.  */
	       && (GET_CODE (XEXP (rtl, 0)) != REG
		   || REGNO (XEXP (rtl, 0)) == HARD_FRAME_POINTER_REGNUM
		   || REGNO (XEXP (rtl, 0)) == STACK_POINTER_REGNUM
#if ARG_POINTER_REGNUM != HARD_FRAME_POINTER_REGNUM
		   || REGNO (XEXP (rtl, 0)) == ARG_POINTER_REGNUM
#endif
		     )
	       /* Big endian correction check.  */
	       && BYTES_BIG_ENDIAN
	       && TYPE_MODE (TREE_TYPE (decl)) != GET_MODE (rtl)
	       && (GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (decl)))
		   < UNITS_PER_WORD))
	{
	  int offset = (UNITS_PER_WORD
			- GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (decl))));
	  rtl = gen_rtx_MEM (TYPE_MODE (TREE_TYPE (decl)),
			     plus_constant (XEXP (rtl, 0), offset));
	}
    }

  if (rtl != NULL_RTX)
    {
      rtl = eliminate_regs (rtl, 0, NULL_RTX);
#ifdef LEAF_REG_REMAP
      if (current_function_uses_only_leaf_regs)
	leaf_renumber_regs_insn (rtl);
#endif
    }

  return rtl;
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

  if (TREE_CODE (decl) == ERROR_MARK)
    return;

  if (TREE_CODE (decl) != VAR_DECL && TREE_CODE (decl) != PARM_DECL)
    abort ();

  rtl = rtl_for_decl_location (decl);
  if (rtl == NULL_RTX)
    return;

  /* If we don't look past the constant pool, we risk emitting a
     reference to a constant pool entry that isn't referenced from
     code, and thus is not emitted.  */
  rtl = avoid_constant_pool_reference (rtl);

  switch (GET_CODE (rtl))
    {
    case ADDRESSOF:
      /* The address of a variable that was optimized away; don't emit
	 anything.  */
      break;

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
    case CONCAT:
      add_AT_location_description (die, DW_AT_location, rtl);
      break;

    default:
      abort ();
    }
}

/* If we don't have a copy of this variable in memory for some reason (such
   as a C++ member constant that doesn't have an out-of-line definition),
   we should tell the debugger about the constant value.  */

static void
tree_add_const_value_attribute (var_die, decl)
     dw_die_ref var_die;
     tree decl;
{
  tree init = DECL_INITIAL (decl);
  tree type = TREE_TYPE (decl);

  if (TREE_READONLY (decl) && ! TREE_THIS_VOLATILE (decl) && init
      && initializer_constant_valid_p (init, type) == null_pointer_node)
    /* OK */;
  else
    return;

  switch (TREE_CODE (type))
    {
    case INTEGER_TYPE:
      if (host_integerp (init, 0))
	add_AT_unsigned (var_die, DW_AT_const_value,
			 TREE_INT_CST_LOW (init));
      else
	add_AT_long_long (var_die, DW_AT_const_value,
			  TREE_INT_CST_HIGH (init),
			  TREE_INT_CST_LOW (init));
      break;

    default:;
    }
}

/* Generate an DW_AT_name attribute given some string value to be included as
   the value of the attribute.  */

static inline void
add_name_attribute (die, name_string)
     register dw_die_ref die;
     register const char *name_string;
{
  if (name_string != NULL && *name_string != 0)
    {
      if (demangle_name_func)
	name_string = (*demangle_name_func) (name_string);

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
  /* If this is an Ada unconstrained array type, then don't emit any debug
     info because the array bounds are unknown.  They are parameterized when
     the type is instantiated.  */
  if (contains_placeholder_p (bound))
    return;

  switch (TREE_CODE (bound))
    {
    case ERROR_MARK:
      return;

    /* All fixed-bounds are represented by INTEGER_CST nodes.        */
    case INTEGER_CST:
      if (! host_integerp (bound, 0)
	  || (bound_attr == DW_AT_lower_bound
	      && (((is_c_family () || is_java ()) &&  integer_zerop (bound))
		  || (is_fortran () && integer_onep (bound)))))
	/* use the default */
	;
      else
	add_AT_unsigned (subrange_die, bound_attr, tree_low_cst (bound, 0));
      break;

    case CONVERT_EXPR:
    case NOP_EXPR:
    case NON_LVALUE_EXPR:
      add_bound_info (subrange_die, bound_attr, TREE_OPERAND (bound, 0));
      break;

    case SAVE_EXPR:
      /* If optimization is turned on, the SAVE_EXPRs that describe how to
         access the upper bound values may be bogus.  If they refer to a
         register, they may only describe how to get at these values at the
         points in the generated code right after they have just been
         computed.  Worse yet, in the typical case, the upper bound values
         will not even *be* computed in the optimized code (though the
         number of elements will), so these SAVE_EXPRs are entirely
         bogus. In order to compensate for this fact, we check here to see
         if optimization is enabled, and if so, we don't add an attribute
         for the (unknown and unknowable) upper bound.  This should not
         cause too much trouble for existing (stupid?)  debuggers because
         they have to deal with empty upper bounds location descriptions
         anyway in order to be able to deal with incomplete array types.
         Of course an intelligent debugger (GDB?)  should be able to
         comprehend that a missing upper bound specification in a array
         type used for a storage class `auto' local array variable
         indicates that the upper bound is both unknown (at compile- time)
         and unknowable (at run-time) due to optimization.

	 We assume that a MEM rtx is safe because gcc wouldn't put the
	 value there unless it was going to be used repeatedly in the
	 function, i.e. for cleanups.  */
      if (SAVE_EXPR_RTL (bound)
	  && (! optimize || GET_CODE (SAVE_EXPR_RTL (bound)) == MEM))
	{
	  register dw_die_ref ctx = lookup_decl_die (current_function_decl);
	  register dw_die_ref decl_die = new_die (DW_TAG_variable, ctx);
	  register rtx loc = SAVE_EXPR_RTL (bound);

	  /* If the RTL for the SAVE_EXPR is memory, handle the case where
	     it references an outer function's frame.  */

	  if (GET_CODE (loc) == MEM)
	    {
	      rtx new_addr = fix_lexical_addr (XEXP (loc, 0), bound);

	      if (XEXP (loc, 0) != new_addr)
		loc = gen_rtx_MEM (GET_MODE (loc), new_addr);
	    }

	  add_AT_flag (decl_die, DW_AT_artificial, 1);
	  add_type_attribute (decl_die, TREE_TYPE (bound), 1, 0, ctx);
	  add_AT_location_description (decl_die, DW_AT_location, loc);
	  add_AT_die_ref (subrange_die, bound_attr, decl_die);
	}

      /* Else leave out the attribute.  */
      break;

    case VAR_DECL:
    case PARM_DECL:
      {
	dw_die_ref decl_die = lookup_decl_die (bound);

	/* ??? Can this happen, or should the variable have been bound
	   first?  Probably it can, since I imagine that we try to create
	   the types of parameters in the order in which they exist in
	   the list, and won't have created a forward reference to a
	   later parameter.  */
	if (decl_die != NULL)
	  add_AT_die_ref (subrange_die, bound_attr, decl_die);
	break;
      }

    default:
      {
	/* Otherwise try to create a stack operation procedure to
	   evaluate the value of the array bound.  */

	dw_die_ref ctx, decl_die;
	dw_loc_descr_ref loc;

	loc = loc_descriptor_from_tree (bound, 0);
	if (loc == NULL)
	  break;

	ctx = lookup_decl_die (current_function_decl);

	decl_die = new_die (DW_TAG_variable, ctx);
	add_AT_flag (decl_die, DW_AT_artificial, 1);
	add_type_attribute (decl_die, TREE_TYPE (bound), 1, 0, ctx);
	add_AT_loc (decl_die, DW_AT_location, loc);

	add_AT_die_ref (subrange_die, bound_attr, decl_die);
	break;
      }
    }
}

/* Note that the block of subscript information for an array type also
   includes information about the element type of type given array type.  */

static void
add_subscript_info (type_die, type)
     register dw_die_ref type_die;
     register tree type;
{
#ifndef MIPS_DEBUGGING_INFO
  register unsigned dimension_number;
#endif
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

	  /* define the index type.  */
	  if (TREE_TYPE (domain))
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
		add_type_attribute (subrange_die, TREE_TYPE (domain), 0, 0,
				    type_die);
	    }

	  /* ??? If upper is NULL, the array has unspecified length,
	     but it does have a lower bound.  This happens with Fortran
	       dimension arr(N:*)
       	     Since the debugger is definitely going to need to know N
	     to produce useful results, go ahead and output the lower
	     bound solo, and hope the debugger can cope.  */

	  add_bound_info (subrange_die, DW_AT_lower_bound, lower);
	  if (upper)
	    add_bound_info (subrange_die, DW_AT_upper_bound, upper);
	}
      else
	/* We have an array type with an unspecified length.  The DWARF-2
	     spec does not say how to handle this; let's just leave out the
	     bounds.  */
        {;}

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

   For any given bit-field, the "containing object" is a hypothetical
   object (of some integral or enum type) within which the given bit-field
   lives.  The type of this hypothetical "containing object" is always the
   same as the declared type of the individual bit-field itself.  The
   determination of the exact location of the "containing object" for a
   bit-field is rather complicated.  It's handled by the
   `field_byte_offset' function (above).

   Note that it is the size (in bytes) of the hypothetical "containing object"
   which will be given in the DW_AT_byte_size attribute for this bit-field.
   (See `byte_size_attribute' above).  */

static inline void
add_bit_offset_attribute (die, decl)
     register dw_die_ref die;
     register tree decl;
{
  HOST_WIDE_INT object_offset_in_bytes = field_byte_offset (decl);
  tree type = DECL_BIT_FIELD_TYPE (decl);
  HOST_WIDE_INT bitpos_int;
  HOST_WIDE_INT highest_order_object_bit_offset;
  HOST_WIDE_INT highest_order_field_bit_offset;
  HOST_WIDE_INT unsigned bit_offset;

  /* Must be a field and a bit field.  */
  if (!type
      || TREE_CODE (decl) != FIELD_DECL)
    abort ();

  /* We can't yet handle bit-fields whose offsets are variable, so if we
     encounter such things, just return without generating any attribute
     whatsoever.  Likewise for variable or too large size.  */
  if (! host_integerp (bit_position (decl), 0)
      || ! host_integerp (DECL_SIZE (decl), 1))
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
      highest_order_field_bit_offset += tree_low_cst (DECL_SIZE (decl), 0);
      highest_order_object_bit_offset += simple_type_size_in_bits (type);
    }

  bit_offset
    = (! BYTES_BIG_ENDIAN
       ? highest_order_object_bit_offset - highest_order_field_bit_offset
       : highest_order_field_bit_offset - highest_order_object_bit_offset);

  add_AT_unsigned (die, DW_AT_bit_offset, bit_offset);
}

/* For a FIELD_DECL node which represents a bit field, output an attribute
   which specifies the length in bits of the given field.  */

static inline void
add_bit_size_attribute (die, decl)
     register dw_die_ref die;
     register tree decl;
{
  /* Must be a field and a bit field.  */
  if (TREE_CODE (decl) != FIELD_DECL
      || ! DECL_BIT_FIELD_TYPE (decl))
    abort ();

  if (host_integerp (DECL_SIZE (decl), 1))
    add_AT_unsigned (die, DW_AT_bit_size, tree_low_cst (DECL_SIZE (decl), 1));
}

/* If the compiled language is ANSI C, then add a 'prototyped'
   attribute, if arg types are given for the parameters of a function.  */

static inline void
add_prototyped_attribute (die, func_type)
     register dw_die_ref die;
     register tree func_type;
{
  if (get_AT_unsigned (comp_unit_die, DW_AT_language) == DW_LANG_C89
      && TYPE_ARG_TYPES (func_type) != NULL)
    add_AT_flag (die, DW_AT_prototyped, 1);
}

/* Add an 'abstract_origin' attribute below a given DIE.  The DIE is found
   by looking in either the type declaration or object declaration
   equate table.  */

static inline void
add_abstract_origin_attribute (die, origin)
     register dw_die_ref die;
     register tree origin;
{
  dw_die_ref origin_die = NULL;

  if (TREE_CODE (origin) != FUNCTION_DECL)
    {
      /* We may have gotten separated from the block for the inlined
	 function, if we're in an exception handler or some such; make
	 sure that the abstract function has been written out.

         Doing this for nested functions is wrong, however; functions are
	 distinct units, and our context might not even be inline.  */
      tree fn = origin;
      if (TYPE_P (fn))
	fn = TYPE_STUB_DECL (fn);
      fn = decl_function_context (fn);
      if (fn)
	dwarf2out_abstract_function (fn);
    }

  if (DECL_P (origin))
    origin_die = lookup_decl_die (origin);
  else if (TYPE_P (origin))
    origin_die = lookup_type_die (origin);

  if (origin_die == NULL)
    abort ();

  add_AT_die_ref (die, DW_AT_abstract_origin, origin_die);
}

/* We do not currently support the pure_virtual attribute.  */

static inline void
add_pure_or_virtual_attribute (die, func_decl)
     register dw_die_ref die;
     register tree func_decl;
{
  if (DECL_VINDEX (func_decl))
    {
      add_AT_unsigned (die, DW_AT_virtuality, DW_VIRTUALITY_virtual);

      if (host_integerp (DECL_VINDEX (func_decl), 0))
	add_AT_loc (die, DW_AT_vtable_elem_location,
		    new_loc_descr (DW_OP_constu,
				   tree_low_cst (DECL_VINDEX (func_decl), 0),
				   0));

      /* GNU extension: Record what type this method came from originally.  */
      if (debug_info_level > DINFO_LEVEL_TERSE)
	add_AT_die_ref (die, DW_AT_containing_type,
			lookup_type_die (DECL_CONTEXT (func_decl)));
    }
}

/* Add source coordinate attributes for the given decl.  */

static void
add_src_coords_attributes (die, decl)
     register dw_die_ref die;
     register tree decl;
{
  register unsigned file_index = lookup_filename (DECL_SOURCE_FILE (decl));

  add_AT_unsigned (die, DW_AT_decl_file, file_index);
  add_AT_unsigned (die, DW_AT_decl_line, DECL_SOURCE_LINE (decl));
}

/* Add an DW_AT_name attribute and source coordinate attribute for the
   given decl, but only if it actually has a name.  */

static void
add_name_and_src_coords_attributes (die, decl)
     register dw_die_ref die;
     register tree decl;
{
  register tree decl_name;

  decl_name = DECL_NAME (decl);
  if (decl_name != NULL && IDENTIFIER_POINTER (decl_name) != NULL)
    {
      add_name_attribute (die, dwarf2_name (decl, 0));
      if (! DECL_ARTIFICIAL (decl))
	add_src_coords_attributes (die, decl);

      if ((TREE_CODE (decl) == FUNCTION_DECL || TREE_CODE (decl) == VAR_DECL)
	  && TREE_PUBLIC (decl)
	  && DECL_ASSEMBLER_NAME (decl) != DECL_NAME (decl)
	  && !DECL_ABSTRACT (decl))
	add_AT_string (die, DW_AT_MIPS_linkage_name,
		       IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl)));
    }
}

/* Push a new declaration scope.  */

static void
push_decl_scope (scope)
     tree scope;
{
  /* Make room in the decl_scope_table, if necessary.  */
  if (decl_scope_table_allocated == decl_scope_depth)
    {
      decl_scope_table_allocated += DECL_SCOPE_TABLE_INCREMENT;
      decl_scope_table
	= (tree *) xrealloc (decl_scope_table,
			     decl_scope_table_allocated * sizeof (tree));
    }

  decl_scope_table[decl_scope_depth] = scope;
  decl_scope_depth++;
}

/* Pop a declaration scope.  */
static inline void
pop_decl_scope ()
{
  if (decl_scope_depth <= 0)
    abort ();
  --decl_scope_depth;
}

/* Return the DIE for the scope that immediately contains this type.
   Non-named types get global scope.  Named types nested in other
   types get their containing scope if it's open, or global scope
   otherwise.  All other types (i.e. function-local named types) get
   the current active scope.  */

static dw_die_ref
scope_die_for (t, context_die)
     register tree t;
     register dw_die_ref context_die;
{
  register dw_die_ref scope_die = NULL;
  register tree containing_scope;
  register int i;

  /* Non-types always go in the current scope.  */
  if (! TYPE_P (t))
    abort ();

  containing_scope = TYPE_CONTEXT (t);

  /* Ignore namespaces for the moment.  */
  if (containing_scope && TREE_CODE (containing_scope) == NAMESPACE_DECL)
    containing_scope = NULL_TREE;

  /* Ignore function type "scopes" from the C frontend.  They mean that
     a tagged type is local to a parmlist of a function declarator, but
     that isn't useful to DWARF.  */
  if (containing_scope && TREE_CODE (containing_scope) == FUNCTION_TYPE)
    containing_scope = NULL_TREE;

  if (containing_scope == NULL_TREE)
    scope_die = comp_unit_die;
  else if (TYPE_P (containing_scope))
    {
      /* For types, we can just look up the appropriate DIE.  But
	 first we check to see if we're in the middle of emitting it
	 so we know where the new DIE should go.  */

      for (i = decl_scope_depth - 1; i >= 0; --i)
	if (decl_scope_table[i] == containing_scope)
	  break;

      if (i < 0)
	{
	  if (debug_info_level > DINFO_LEVEL_TERSE
	      && !TREE_ASM_WRITTEN (containing_scope))
	    abort ();

	  /* If none of the current dies are suitable, we get file scope.  */
	  scope_die = comp_unit_die;
	}
      else
	scope_die = lookup_type_die (containing_scope);
    }
  else
    scope_die = context_die;

  return scope_die;
}

/* Returns nonzero iff CONTEXT_DIE is internal to a function.  */

static inline int local_scope_p PARAMS ((dw_die_ref));
static inline int
local_scope_p (context_die)
     dw_die_ref context_die;
{
  for (; context_die; context_die = context_die->die_parent)
    if (context_die->die_tag == DW_TAG_inlined_subroutine
	|| context_die->die_tag == DW_TAG_subprogram)
      return 1;
  return 0;
}

/* Returns nonzero iff CONTEXT_DIE is a class.  */

static inline int class_scope_p PARAMS ((dw_die_ref));
static inline int
class_scope_p (context_die)
     dw_die_ref context_die;
{
  return (context_die
	  && (context_die->die_tag == DW_TAG_structure_type
	      || context_die->die_tag == DW_TAG_union_type));
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
  register dw_die_ref type_die  = NULL;

  /* ??? If this type is an unnamed subrange type of an integral or
     floating-point type, use the inner type.  This is because we have no
     support for unnamed types in base_type_die.  This can happen if this is
     an Ada subrange type.  Correct solution is emit a subrange type die.  */
  if ((code == INTEGER_TYPE || code == REAL_TYPE)
      && TREE_TYPE (type) != 0 && TYPE_NAME (type) == 0)
    type = TREE_TYPE (type), code = TREE_CODE (type);

  if (code == ERROR_MARK)
    return;

  /* Handle a special case.  For functions whose return type is void, we
     generate *no* type attribute.  (Note that no object may have type
     `void', so this only applies to function return types).  */
  if (code == VOID_TYPE)
    return;

  type_die = modified_type_die (type,
				decl_const || TYPE_READONLY (type),
				decl_volatile || TYPE_VOLATILE (type),
				context_die);
  if (type_die != NULL)
    add_AT_die_ref (object_die, DW_AT_type, type_die);
}

/* Given a tree pointer to a struct, class, union, or enum type node, return
   a pointer to the (string) tag name for the given type, or zero if the type
   was declared without a tag.  */

static const char *
type_tag (type)
     register tree type;
{
  register const char *name = 0;

  if (TYPE_NAME (type) != 0)
    {
      register tree t = 0;

      /* Find the IDENTIFIER_NODE for the type name.  */
      if (TREE_CODE (TYPE_NAME (type)) == IDENTIFIER_NODE)
	t = TYPE_NAME (type);

      /* The g++ front end makes the TYPE_NAME of *each* tagged type point to
         a TYPE_DECL node, regardless of whether or not a `typedef' was
         involved.  */
      else if (TREE_CODE (TYPE_NAME (type)) == TYPE_DECL
	       && ! DECL_IGNORED_P (TYPE_NAME (type)))
	t = DECL_NAME (TYPE_NAME (type));

      /* Now get the name as a string, or invent one.  */
      if (t != 0)
	name = IDENTIFIER_POINTER (t);
    }

  return (name == 0 || *name == '\0') ? 0 : name;
}

/* Return the type associated with a data member, make a special check
   for bit field types.  */

static inline tree
member_declared_type (member)
     register tree member;
{
  return (DECL_BIT_FIELD_TYPE (member)
	  ? DECL_BIT_FIELD_TYPE (member)
	  : TREE_TYPE (member));
}

/* Get the decl's label, as described by its RTL. This may be different
   from the DECL_NAME name used in the source file.  */

#if 0
static const char *
decl_start_label (decl)
     register tree decl;
{
  rtx x;
  const char *fnname;
  x = DECL_RTL (decl);
  if (GET_CODE (x) != MEM)
    abort ();

  x = XEXP (x, 0);
  if (GET_CODE (x) != SYMBOL_REF)
    abort ();

  fnname = XSTR (x, 0);
  return fnname;
}
#endif

/* These routines generate the internal representation of the DIE's for
   the compilation unit.  Debugging information is collected by walking
   the declaration trees passed in from dwarf2out_decl().  */

static void
gen_array_type_die (type, context_die)
     register tree type;
     register dw_die_ref context_die;
{
  register dw_die_ref scope_die = scope_die_for (type, context_die);
  register dw_die_ref array_die;
  register tree element_type;

  /* ??? The SGI dwarf reader fails for array of array of enum types unless
     the inner array type comes before the outer array type.  Thus we must
     call gen_type_die before we call new_die.  See below also.  */
#ifdef MIPS_DEBUGGING_INFO
  gen_type_die (TREE_TYPE (type), context_die);
#endif

  array_die = new_die (DW_TAG_array_type, scope_die);

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

#ifdef MIPS_DEBUGGING_INFO
  /* The SGI compilers handle arrays of unknown bound by setting
     AT_declaration and not emitting any subrange DIEs.  */
  if (! TYPE_DOMAIN (type))
    add_AT_unsigned (array_die, DW_AT_declaration, 1);
  else
#endif
    add_subscript_info (array_die, type);

  add_name_attribute (array_die, type_tag (type));
  equate_type_number_to_die (type, array_die);

  /* Add representation of the type of the elements of this array type.  */
  element_type = TREE_TYPE (type);

  /* ??? The SGI dwarf reader fails for multidimensional arrays with a
     const enum type.  E.g. const enum machine_mode insn_operand_mode[2][10].
     We work around this by disabling this feature.  See also
     add_subscript_info.  */
#ifndef MIPS_DEBUGGING_INFO
  while (TREE_CODE (element_type) == ARRAY_TYPE)
    element_type = TREE_TYPE (element_type);

  gen_type_die (element_type, context_die);
#endif

  add_type_attribute (array_die, element_type, 0, 0, context_die);
}

static void
gen_set_type_die (type, context_die)
     register tree type;
     register dw_die_ref context_die;
{
  register dw_die_ref type_die
    = new_die (DW_TAG_set_type, scope_die_for (type, context_die));

  equate_type_number_to_die (type, type_die);
  add_type_attribute (type_die, TREE_TYPE (type), 0, 0, context_die);
}

#if 0
static void
gen_entry_point_die (decl, context_die)
     register tree decl;
     register dw_die_ref context_die;
{
  register tree origin = decl_ultimate_origin (decl);
  register dw_die_ref decl_die = new_die (DW_TAG_entry_point, context_die);
  if (origin != NULL)
    add_abstract_origin_attribute (decl_die, origin);
  else
    {
      add_name_and_src_coords_attributes (decl_die, decl);
      add_type_attribute (decl_die, TREE_TYPE (TREE_TYPE (decl)),
			  0, 0, context_die);
    }

  if (DECL_ABSTRACT (decl))
    equate_decl_number_to_die (decl, decl_die);
  else
    add_AT_lbl_id (decl_die, DW_AT_low_pc, decl_start_label (decl));
}
#endif

/* Remember a type in the incomplete_types_list.  */

static void
add_incomplete_type (type)
     tree type;
{
  if (incomplete_types == incomplete_types_allocated)
    {
      incomplete_types_allocated += INCOMPLETE_TYPES_INCREMENT;
      incomplete_types_list
	= (tree *) xrealloc (incomplete_types_list,
			     sizeof (tree) * incomplete_types_allocated);
    }

  incomplete_types_list[incomplete_types++] = type;
}

/* Walk through the list of incomplete types again, trying once more to
   emit full debugging info for them.  */

static void
retry_incomplete_types ()
{
  register tree type;

  while (incomplete_types)
    {
      --incomplete_types;
      type = incomplete_types_list[incomplete_types];
      gen_type_die (type, comp_unit_die);
    }
}

/* Generate a DIE to represent an inlined instance of an enumeration type.  */

static void
gen_inlined_enumeration_type_die (type, context_die)
     register tree type;
     register dw_die_ref context_die;
{
  register dw_die_ref type_die = new_die (DW_TAG_enumeration_type,
					  context_die);
  /* We do not check for TREE_ASM_WRITTEN (type) being set, as the type may
     be incomplete and such types are not marked.  */
  add_abstract_origin_attribute (type_die, type);
}

/* Generate a DIE to represent an inlined instance of a structure type.  */

static void
gen_inlined_structure_type_die (type, context_die)
     register tree type;
     register dw_die_ref context_die;
{
  register dw_die_ref type_die = new_die (DW_TAG_structure_type, context_die);

  /* We do not check for TREE_ASM_WRITTEN (type) being set, as the type may
     be incomplete and such types are not marked.  */
  add_abstract_origin_attribute (type_die, type);
}

/* Generate a DIE to represent an inlined instance of a union type.  */

static void
gen_inlined_union_type_die (type, context_die)
     register tree type;
     register dw_die_ref context_die;
{
  register dw_die_ref type_die = new_die (DW_TAG_union_type, context_die);

  /* We do not check for TREE_ASM_WRITTEN (type) being set, as the type may
     be incomplete and such types are not marked.  */
  add_abstract_origin_attribute (type_die, type);
}

/* Generate a DIE to represent an enumeration type.  Note that these DIEs
   include all of the information about the enumeration values also. Each
   enumerated type name/value is listed as a child of the enumerated type
   DIE.  */

static void
gen_enumeration_type_die (type, context_die)
     register tree type;
     register dw_die_ref context_die;
{
  register dw_die_ref type_die = lookup_type_die (type);

  if (type_die == NULL)
    {
      type_die = new_die (DW_TAG_enumeration_type,
			  scope_die_for (type, context_die));
      equate_type_number_to_die (type, type_die);
      add_name_attribute (type_die, type_tag (type));
    }
  else if (! TYPE_SIZE (type))
    return;
  else
    remove_AT (type_die, DW_AT_declaration);

  /* Handle a GNU C/C++ extension, i.e. incomplete enum types.  If the
     given enum type is incomplete, do not generate the DW_AT_byte_size
     attribute or the DW_AT_element_list attribute.  */
  if (TYPE_SIZE (type))
    {
      register tree link;

      TREE_ASM_WRITTEN (type) = 1;
      add_byte_size_attribute (type_die, type);
      if (TYPE_STUB_DECL (type) != NULL_TREE)
	add_src_coords_attributes (type_die, TYPE_STUB_DECL (type));

      /* If the first reference to this type was as the return type of an
	 inline function, then it may not have a parent.  Fix this now.  */
      if (type_die->die_parent == NULL)
	add_child_die (scope_die_for (type, context_die), type_die);

      for (link = TYPE_FIELDS (type);
	   link != NULL; link = TREE_CHAIN (link))
	{
	  register dw_die_ref enum_die = new_die (DW_TAG_enumerator, type_die);

	  add_name_attribute (enum_die,
			      IDENTIFIER_POINTER (TREE_PURPOSE (link)));

	  if (host_integerp (TREE_VALUE (link), 0))
	    {
	      if (tree_int_cst_sgn (TREE_VALUE (link)) < 0)
		add_AT_int (enum_die, DW_AT_const_value,
			    tree_low_cst (TREE_VALUE (link), 0));
	      else
		add_AT_unsigned (enum_die, DW_AT_const_value,
				 tree_low_cst (TREE_VALUE (link), 0));
	    }
	}
    }
  else
    add_AT_flag (type_die, DW_AT_declaration, 1);
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

static dw_die_ref
gen_formal_parameter_die (node, context_die)
     register tree node;
     register dw_die_ref context_die;
{
  register dw_die_ref parm_die
    = new_die (DW_TAG_formal_parameter, context_die);
  register tree origin;

  switch (TREE_CODE_CLASS (TREE_CODE (node)))
    {
    case 'd':
      origin = decl_ultimate_origin (node);
      if (origin != NULL)
	add_abstract_origin_attribute (parm_die, origin);
      else
	{
	  add_name_and_src_coords_attributes (parm_die, node);
	  add_type_attribute (parm_die, TREE_TYPE (node),
			      TREE_READONLY (node),
			      TREE_THIS_VOLATILE (node),
			      context_die);
	  if (DECL_ARTIFICIAL (node))
	    add_AT_flag (parm_die, DW_AT_artificial, 1);
	}

      equate_decl_number_to_die (node, parm_die);
      if (! DECL_ABSTRACT (node))
	add_location_or_const_value_attribute (parm_die, node);

      break;

    case 't':
      /* We were called with some kind of a ..._TYPE node.  */
      add_type_attribute (parm_die, node, 0, 0, context_die);
      break;

    default:
      abort ();
    }

  return parm_die;
}

/* Generate a special type of DIE used as a stand-in for a trailing ellipsis
   at the end of an (ANSI prototyped) formal parameters list.  */

static void
gen_unspecified_parameters_die (decl_or_type, context_die)
     register tree decl_or_type ATTRIBUTE_UNUSED;
     register dw_die_ref context_die;
{
  new_die (DW_TAG_unspecified_parameters, context_die);
}

/* Generate a list of nameless DW_TAG_formal_parameter DIEs (and perhaps a
   DW_TAG_unspecified_parameters DIE) to represent the types of the formal
   parameters as specified in some function type specification (except for
   those which appear as part of a function *definition*).  */

static void
gen_formal_types_die (function_or_method_type, context_die)
     register tree function_or_method_type;
     register dw_die_ref context_die;
{
  register tree link;
  register tree formal_type = NULL;
  register tree first_parm_type;
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
      register dw_die_ref parm_die;

      formal_type = TREE_VALUE (link);
      if (formal_type == void_type_node)
	break;

      /* Output a (nameless) DIE to represent the formal parameter itself.  */
      parm_die = gen_formal_parameter_die (formal_type, context_die);
      if ((TREE_CODE (function_or_method_type) == METHOD_TYPE
	   && link == first_parm_type)
	  || (arg && DECL_ARTIFICIAL (arg)))
	add_AT_flag (parm_die, DW_AT_artificial, 1);

      link = TREE_CHAIN (link);
      if (arg)
	arg = TREE_CHAIN (arg);
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

      gen_type_die (formal_type, context_die);
    }
}

/* We want to generate the DIE for TYPE so that we can generate the
   die for MEMBER, which has been defined; we will need to refer back
   to the member declaration nested within TYPE.  If we're trying to
   generate minimal debug info for TYPE, processing TYPE won't do the
   trick; we need to attach the member declaration by hand.  */

static void
gen_type_die_for_member (type, member, context_die)
     tree type, member;
     dw_die_ref context_die;
{
  gen_type_die (type, context_die);

  /* If we're trying to avoid duplicate debug info, we may not have
     emitted the member decl for this function.  Emit it now.  */
  if (TYPE_DECL_SUPPRESS_DEBUG (TYPE_STUB_DECL (type))
      && ! lookup_decl_die (member))
    {
      if (decl_ultimate_origin (member))
	abort ();

      push_decl_scope (type);
      if (TREE_CODE (member) == FUNCTION_DECL)
	gen_subprogram_die (member, lookup_type_die (type));
      else
	gen_variable_die (member, lookup_type_die (type));
      pop_decl_scope ();
    }
}

/* Generate the DWARF2 info for the "abstract" instance
   of a function which we may later generate inlined and/or
   out-of-line instances of.  */

static void
dwarf2out_abstract_function (decl)
     tree decl;
{
  register dw_die_ref old_die;
  tree save_fn;
  tree context;
  int was_abstract = DECL_ABSTRACT (decl);

  /* Make sure we have the actual abstract inline, not a clone.  */
  decl = DECL_ORIGIN (decl);

  old_die = lookup_decl_die (decl);  
  if (old_die && get_AT_unsigned (old_die, DW_AT_inline))
    /* We've already generated the abstract instance.  */
    return;

  /* Be sure we've emitted the in-class declaration DIE (if any) first, so
     we don't get confused by DECL_ABSTRACT.  */
  if (debug_info_level > DINFO_LEVEL_TERSE)
    {
      context = decl_class_context (decl);
      if (context)
	gen_type_die_for_member
	  (context, decl, decl_function_context (decl) ? NULL : comp_unit_die);
    }
 
  /* Pretend we've just finished compiling this function.  */
  save_fn = current_function_decl;
  current_function_decl = decl;

  set_decl_abstract_flags (decl, 1);
  dwarf2out_decl (decl);
  if (! was_abstract)
    set_decl_abstract_flags (decl, 0);

  current_function_decl = save_fn;
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
  register dw_die_ref subr_die;
  register rtx fp_reg;
  register tree fn_arg_types;
  register tree outer_scope;
  register dw_die_ref old_die = lookup_decl_die (decl);
  register int declaration = (current_function_decl != decl
			      || class_scope_p (context_die));

  /* Note that it is possible to have both DECL_ABSTRACT and `declaration'
     be true, if we started to generate the abstract instance of an inline,
     decided to output its containing class, and proceeded to emit the
     declaration of the inline from the member list for the class.  In that
     case, `declaration' takes priority; we'll get back to the abstract
     instance when we're done with the class.  */

  /* The class-scope declaration DIE must be the primary DIE.  */
  if (origin && declaration && class_scope_p (context_die))
    {
      origin = NULL;
      if (old_die)
	abort ();
    }

  if (origin != NULL)
    {
      if (declaration && ! local_scope_p (context_die))
	abort ();

      /* Fixup die_parent for the abstract instance of a nested
	 inline function.  */
      if (old_die && old_die->die_parent == NULL)
	add_child_die (context_die, old_die);

      subr_die = new_die (DW_TAG_subprogram, context_die);
      add_abstract_origin_attribute (subr_die, origin);
    }
  else if (old_die)
    {
      unsigned file_index = lookup_filename (DECL_SOURCE_FILE (decl));

      if (!get_AT_flag (old_die, DW_AT_declaration)
	  /* We can have a normal definition following an inline one in the
	     case of redefinition of GNU C extern inlines.
	     It seems reasonable to use AT_specification in this case.  */
	  && !get_AT_unsigned (old_die, DW_AT_inline))
	{
	  /* ??? This can happen if there is a bug in the program, for
	     instance, if it has duplicate function definitions.  Ideally,
	     we should detect this case and ignore it.  For now, if we have
	     already reported an error, any error at all, then assume that
	     we got here because of a input error, not a dwarf2 bug.  */
	  if (errorcount)
	    return;
	  abort ();
	}

      /* If the definition comes from the same place as the declaration,
	 maybe use the old DIE.  We always want the DIE for this function
	 that has the *_pc attributes to be under comp_unit_die so the
	 debugger can find it.  We also need to do this for abstract
	 instances of inlines, since the spec requires the out-of-line copy
	 to have the same parent.  For local class methods, this doesn't
	 apply; we just use the old DIE.  */
      if ((old_die->die_parent == comp_unit_die || context_die == NULL)
	  && (DECL_ARTIFICIAL (decl)
	      || (get_AT_unsigned (old_die, DW_AT_decl_file) == file_index
		  && (get_AT_unsigned (old_die, DW_AT_decl_line)
		      == (unsigned) DECL_SOURCE_LINE (decl)))))
	{
	  subr_die = old_die;

	  /* Clear out the declaration attribute and the parm types.  */
	  remove_AT (subr_die, DW_AT_declaration);
	  remove_children (subr_die);
	}
      else
	{
	  subr_die = new_die (DW_TAG_subprogram, context_die);
	  add_AT_die_ref (subr_die, DW_AT_specification, old_die);
	  if (get_AT_unsigned (old_die, DW_AT_decl_file) != file_index)
	    add_AT_unsigned (subr_die, DW_AT_decl_file, file_index);
	  if (get_AT_unsigned (old_die, DW_AT_decl_line)
	      != (unsigned) DECL_SOURCE_LINE (decl))
	    add_AT_unsigned
	      (subr_die, DW_AT_decl_line, DECL_SOURCE_LINE (decl));
	}
    }
  else
    {
      subr_die = new_die (DW_TAG_subprogram, context_die);

      if (TREE_PUBLIC (decl))
	add_AT_flag (subr_die, DW_AT_external, 1);

      add_name_and_src_coords_attributes (subr_die, decl);
      if (debug_info_level > DINFO_LEVEL_TERSE)
	{
	  register tree type = TREE_TYPE (decl);

	  add_prototyped_attribute (subr_die, type);
	  add_type_attribute (subr_die, TREE_TYPE (type), 0, 0, context_die);
	}

      add_pure_or_virtual_attribute (subr_die, decl);
      if (DECL_ARTIFICIAL (decl))
	add_AT_flag (subr_die, DW_AT_artificial, 1);
      if (TREE_PROTECTED (decl))
	add_AT_unsigned (subr_die, DW_AT_accessibility, DW_ACCESS_protected);
      else if (TREE_PRIVATE (decl))
	add_AT_unsigned (subr_die, DW_AT_accessibility, DW_ACCESS_private);
    }

  if (declaration)
    {
      if (!(old_die && get_AT_unsigned (old_die, DW_AT_inline)))
	{
	  add_AT_flag (subr_die, DW_AT_declaration, 1);

	  /* The first time we see a member function, it is in the context of
	     the class to which it belongs.  We make sure of this by emitting
	     the class first.  The next time is the definition, which is
	     handled above.  The two may come from the same source text.  */
	  if (DECL_CONTEXT (decl) || DECL_ABSTRACT (decl))
	    equate_decl_number_to_die (decl, subr_die);
	}
    }
  else if (DECL_ABSTRACT (decl))
    {
      if (DECL_INLINE (decl) && !flag_no_inline)
	{
	  /* ??? Checking DECL_DEFER_OUTPUT is correct for static
	     inline functions, but not for extern inline functions.
	     We can't get this completely correct because information
	     about whether the function was declared inline is not
	     saved anywhere.  */
	  if (DECL_DEFER_OUTPUT (decl))
	    add_AT_unsigned (subr_die, DW_AT_inline, DW_INL_declared_inlined);
	  else
	    add_AT_unsigned (subr_die, DW_AT_inline, DW_INL_inlined);
	}
      else
	add_AT_unsigned (subr_die, DW_AT_inline, DW_INL_declared_not_inlined);

      equate_decl_number_to_die (decl, subr_die);
    }
  else if (!DECL_EXTERNAL (decl))
    {
      if (!(old_die && get_AT_unsigned (old_die, DW_AT_inline)))
	equate_decl_number_to_die (decl, subr_die);

      ASM_GENERATE_INTERNAL_LABEL (label_id, FUNC_BEGIN_LABEL,
				   current_funcdef_number);
      add_AT_lbl_id (subr_die, DW_AT_low_pc, label_id);
      ASM_GENERATE_INTERNAL_LABEL (label_id, FUNC_END_LABEL,
				   current_funcdef_number);
      add_AT_lbl_id (subr_die, DW_AT_high_pc, label_id);

      add_pubname (decl, subr_die);
      add_arange (decl, subr_die);

#ifdef MIPS_DEBUGGING_INFO
      /* Add a reference to the FDE for this routine.  */
      add_AT_fde_ref (subr_die, DW_AT_MIPS_fde, current_funcdef_fde);
#endif

      /* Define the "frame base" location for this routine.  We use the
         frame pointer or stack pointer registers, since the RTL for local
         variables is relative to one of them.  */
      fp_reg
	= frame_pointer_needed ? hard_frame_pointer_rtx : stack_pointer_rtx;
      add_AT_loc (subr_die, DW_AT_frame_base, reg_loc_descriptor (fp_reg));

#if 0
      /* ??? This fails for nested inline functions, because context_display
	 is not part of the state saved/restored for inline functions.  */
      if (current_function_needs_context)
	add_AT_location_description (subr_die, DW_AT_static_link,
				     lookup_static_chain (decl));
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
  if (debug_info_level <= DINFO_LEVEL_TERSE)
    ;
  else if (declaration)
    gen_formal_types_die (decl, subr_die);
  else
    {
      /* Generate DIEs to represent all known formal parameters */
      register tree arg_decls = DECL_ARGUMENTS (decl);
      register tree parm;

      /* When generating DIEs, generate the unspecified_parameters DIE
         instead if we come across the arg "__builtin_va_alist" */
      for (parm = arg_decls; parm; parm = TREE_CHAIN (parm))
	if (TREE_CODE (parm) == PARM_DECL)
	  {
	    if (DECL_NAME (parm)
		&& !strcmp (IDENTIFIER_POINTER (DECL_NAME (parm)),
			    "__builtin_va_alist"))
	      gen_unspecified_parameters_die (parm, subr_die);
	    else
	      gen_decl_die (parm, subr_die);
	  }

      /* Decide whether we need a unspecified_parameters DIE at the end.
         There are 2 more cases to do this for: 1) the ansi ... declaration -
         this is detectable when the end of the arg list is not a
         void_type_node 2) an unprototyped function declaration (not a
         definition).  This just means that we have no info about the
         parameters at all.  */
      fn_arg_types = TYPE_ARG_TYPES (TREE_TYPE (decl));
      if (fn_arg_types != NULL)
	{
	  /* this is the prototyped case, check for ...  */
	  if (TREE_VALUE (tree_last (fn_arg_types)) != void_type_node)
	    gen_unspecified_parameters_die (decl, subr_die);
	}
      else if (DECL_INITIAL (decl) == NULL_TREE)
	gen_unspecified_parameters_die (decl, subr_die);
    }

  /* Output Dwarf info for all of the stuff within the body of the function
     (if it has one - it may be just a declaration).  */
  outer_scope = DECL_INITIAL (decl);

  /* Note that here, `outer_scope' is a pointer to the outermost BLOCK
     node created to represent a function. This outermost BLOCK actually
     represents the outermost binding contour for the function, i.e. the
     contour in which the function's formal parameters and labels get
     declared. Curiously, it appears that the front end doesn't actually
     put the PARM_DECL nodes for the current function onto the BLOCK_VARS
     list for this outer scope.  (They are strung off of the DECL_ARGUMENTS
     list for the function instead.) The BLOCK_VARS list for the
     `outer_scope' does provide us with a list of the LABEL_DECL nodes for
     the function however, and we output DWARF info for those in
     decls_for_scope.  Just within the `outer_scope' there will be a BLOCK
     node representing the function's outermost pair of curly braces, and
     any blocks used for the base and member initializers of a C++
     constructor function.  */
  if (! declaration && TREE_CODE (outer_scope) != ERROR_MARK)
    {
      current_function_has_inlines = 0;
      decls_for_scope (outer_scope, subr_die, 0);

#if 0 && defined (MIPS_DEBUGGING_INFO)
      if (current_function_has_inlines)
	{
	  add_AT_flag (subr_die, DW_AT_MIPS_has_inlines, 1);
	  if (! comp_unit_has_inlines)
	    {
	      add_AT_flag (comp_unit_die, DW_AT_MIPS_has_inlines, 1);
	      comp_unit_has_inlines = 1;
	    }
	}
#endif
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

  dw_die_ref old_die = lookup_decl_die (decl);
  int declaration = (DECL_EXTERNAL (decl)
		     || class_scope_p (context_die));

  if (origin != NULL)
    add_abstract_origin_attribute (var_die, origin);
  /* Loop unrolling can create multiple blocks that refer to the same
     static variable, so we must test for the DW_AT_declaration flag.  */
  /* ??? Loop unrolling/reorder_blocks should perhaps be rewritten to
     copy decls and set the DECL_ABSTRACT flag on them instead of
     sharing them.  */
  /* ??? Duplicated blocks have been rewritten to use .debug_ranges.  */
  else if (old_die && TREE_STATIC (decl)
 	   && get_AT_flag (old_die, DW_AT_declaration) == 1)
    {
      /* This is a definition of a C++ class level static.  */
      add_AT_die_ref (var_die, DW_AT_specification, old_die);
      if (DECL_NAME (decl))
	{
	  unsigned file_index = lookup_filename (DECL_SOURCE_FILE (decl));

	  if (get_AT_unsigned (old_die, DW_AT_decl_file) != file_index)
	    add_AT_unsigned (var_die, DW_AT_decl_file, file_index);

	  if (get_AT_unsigned (old_die, DW_AT_decl_line)
	      != (unsigned) DECL_SOURCE_LINE (decl))

	    add_AT_unsigned (var_die, DW_AT_decl_line,
			     DECL_SOURCE_LINE (decl));
	}
    }
  else
    {
      add_name_and_src_coords_attributes (var_die, decl);
      add_type_attribute (var_die, TREE_TYPE (decl),
			  TREE_READONLY (decl),
			  TREE_THIS_VOLATILE (decl), context_die);

      if (TREE_PUBLIC (decl))
	add_AT_flag (var_die, DW_AT_external, 1);

      if (DECL_ARTIFICIAL (decl))
	add_AT_flag (var_die, DW_AT_artificial, 1);

      if (TREE_PROTECTED (decl))
	add_AT_unsigned (var_die, DW_AT_accessibility, DW_ACCESS_protected);

      else if (TREE_PRIVATE (decl))
	add_AT_unsigned (var_die, DW_AT_accessibility, DW_ACCESS_private);
    }

  if (declaration)
    add_AT_flag (var_die, DW_AT_declaration, 1);

  if (class_scope_p (context_die) || DECL_ABSTRACT (decl))
    equate_decl_number_to_die (decl, var_die);

  if (! declaration && ! DECL_ABSTRACT (decl))
    {
      add_location_or_const_value_attribute (var_die, decl);
      add_pubname (decl, var_die);
    }
  else
    tree_add_const_value_attribute (var_die, decl);
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
    add_abstract_origin_attribute (lbl_die, origin);
  else
    add_name_and_src_coords_attributes (lbl_die, decl);

  if (DECL_ABSTRACT (decl))
    equate_decl_number_to_die (decl, lbl_die);
  else
    {
      insn = DECL_RTL (decl);

      /* Deleted labels are programmer specified labels which have been
	 eliminated because of various optimisations.  We still emit them
	 here so that it is possible to put breakpoints on them.  */
      if (GET_CODE (insn) == CODE_LABEL
	  || ((GET_CODE (insn) == NOTE
	       && NOTE_LINE_NUMBER (insn) == NOTE_INSN_DELETED_LABEL)))
	{
	  /* When optimization is enabled (via -O) some parts of the compiler
	     (e.g. jump.c and cse.c) may try to delete CODE_LABEL insns which
	     represent source-level labels which were explicitly declared by
	     the user.  This really shouldn't be happening though, so catch
	     it if it ever does happen.  */
	  if (INSN_DELETED_P (insn))
	    abort ();

	  ASM_GENERATE_INTERNAL_LABEL (label, "L", CODE_LABEL_NUMBER (insn));
	  add_AT_lbl_id (lbl_die, DW_AT_low_pc, label);
	}
    }
}

/* Generate a DIE for a lexical block.  */

static void
gen_lexical_block_die (stmt, context_die, depth)
     register tree stmt;
     register dw_die_ref context_die;
     int depth;
{
  register dw_die_ref stmt_die = new_die (DW_TAG_lexical_block, context_die);
  char label[MAX_ARTIFICIAL_LABEL_BYTES];

  if (! BLOCK_ABSTRACT (stmt))
    {
      if (BLOCK_FRAGMENT_CHAIN (stmt))
	{
	  tree chain;

	  add_AT_offset (stmt_die, DW_AT_ranges, add_ranges (stmt));

	  chain = BLOCK_FRAGMENT_CHAIN (stmt);
	  do
	    {
	      add_ranges (chain);
	      chain = BLOCK_FRAGMENT_CHAIN (chain);
	    }
	  while (chain);
	  add_ranges (NULL);
	}
      else
	{
	  ASM_GENERATE_INTERNAL_LABEL (label, BLOCK_BEGIN_LABEL,
				       BLOCK_NUMBER (stmt));
	  add_AT_lbl_id (stmt_die, DW_AT_low_pc, label);
	  ASM_GENERATE_INTERNAL_LABEL (label, BLOCK_END_LABEL,
				       BLOCK_NUMBER (stmt));
	  add_AT_lbl_id (stmt_die, DW_AT_high_pc, label);
	}
    }

  decls_for_scope (stmt, stmt_die, depth);
}

/* Generate a DIE for an inlined subprogram.  */

static void
gen_inlined_subroutine_die (stmt, context_die, depth)
     register tree stmt;
     register dw_die_ref context_die;
     int depth;
{
  if (! BLOCK_ABSTRACT (stmt))
    {
      register dw_die_ref subr_die
	= new_die (DW_TAG_inlined_subroutine, context_die);
      register tree decl = block_ultimate_origin (stmt);
      char label[MAX_ARTIFICIAL_LABEL_BYTES];

      /* Emit info for the abstract instance first, if we haven't yet.  */
      dwarf2out_abstract_function (decl);

      add_abstract_origin_attribute (subr_die, decl);
      ASM_GENERATE_INTERNAL_LABEL (label, BLOCK_BEGIN_LABEL,
				   BLOCK_NUMBER (stmt));
      add_AT_lbl_id (subr_die, DW_AT_low_pc, label);
      ASM_GENERATE_INTERNAL_LABEL (label, BLOCK_END_LABEL,
				   BLOCK_NUMBER (stmt));
      add_AT_lbl_id (subr_die, DW_AT_high_pc, label);
      decls_for_scope (stmt, subr_die, depth);
      current_function_has_inlines = 1;
    }
}

/* Generate a DIE for a field in a record, or structure.  */

static void
gen_field_die (decl, context_die)
     register tree decl;
     register dw_die_ref context_die;
{
  register dw_die_ref decl_die = new_die (DW_TAG_member, context_die);

  add_name_and_src_coords_attributes (decl_die, decl);
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

  if (TREE_CODE (DECL_FIELD_CONTEXT (decl)) != UNION_TYPE)
    add_data_member_location_attribute (decl_die, decl);

  if (DECL_ARTIFICIAL (decl))
    add_AT_flag (decl_die, DW_AT_artificial, 1);

  if (TREE_PROTECTED (decl))
    add_AT_unsigned (decl_die, DW_AT_accessibility, DW_ACCESS_protected);

  else if (TREE_PRIVATE (decl))
    add_AT_unsigned (decl_die, DW_AT_accessibility, DW_ACCESS_private);
}

#if 0
/* Don't generate either pointer_type DIEs or reference_type DIEs here.
   Use modified_type_die instead.
   We keep this code here just in case these types of DIEs may be needed to
   represent certain things in other languages (e.g. Pascal) someday.  */
static void
gen_pointer_type_die (type, context_die)
     register tree type;
     register dw_die_ref context_die;
{
  register dw_die_ref ptr_die
    = new_die (DW_TAG_pointer_type, scope_die_for (type, context_die));

  equate_type_number_to_die (type, ptr_die);
  add_type_attribute (ptr_die, TREE_TYPE (type), 0, 0, context_die);
  add_AT_unsigned (mod_type_die, DW_AT_byte_size, PTR_SIZE);
}

/* Don't generate either pointer_type DIEs or reference_type DIEs here.
   Use modified_type_die instead.
   We keep this code here just in case these types of DIEs may be needed to
   represent certain things in other languages (e.g. Pascal) someday.  */
static void
gen_reference_type_die (type, context_die)
     register tree type;
     register dw_die_ref context_die;
{
  register dw_die_ref ref_die
    = new_die (DW_TAG_reference_type, scope_die_for (type, context_die));

  equate_type_number_to_die (type, ref_die);
  add_type_attribute (ref_die, TREE_TYPE (type), 0, 0, context_die);
  add_AT_unsigned (mod_type_die, DW_AT_byte_size, PTR_SIZE);
}
#endif

/* Generate a DIE for a pointer to a member type.  */
static void
gen_ptr_to_mbr_type_die (type, context_die)
     register tree type;
     register dw_die_ref context_die;
{
  register dw_die_ref ptr_die
    = new_die (DW_TAG_ptr_to_member_type, scope_die_for (type, context_die));

  equate_type_number_to_die (type, ptr_die);
  add_AT_die_ref (ptr_die, DW_AT_containing_type,
		  lookup_type_die (TYPE_OFFSET_BASETYPE (type)));
  add_type_attribute (ptr_die, TREE_TYPE (type), 0, 0, context_die);
}

/* Generate the DIE for the compilation unit.  */

static dw_die_ref
gen_compile_unit_die (filename)
     register const char *filename;
{
  register dw_die_ref die;
  char producer[250];
  const char *wd = getpwd ();
  int language;

  die = new_die (DW_TAG_compile_unit, NULL);
  add_name_attribute (die, filename);

  if (wd != NULL && filename[0] != DIR_SEPARATOR)
    add_AT_string (die, DW_AT_comp_dir, wd);

  sprintf (producer, "%s %s", language_string, version_string);

#ifdef MIPS_DEBUGGING_INFO
  /* The MIPS/SGI compilers place the 'cc' command line options in the producer
     string.  The SGI debugger looks for -g, -g1, -g2, or -g3; if they do
     not appear in the producer string, the debugger reaches the conclusion
     that the object file is stripped and has no debugging information.
     To get the MIPS/SGI debugger to believe that there is debugging
     information in the object file, we add a -g to the producer string.  */
  if (debug_info_level > DINFO_LEVEL_TERSE)
    strcat (producer, " -g");
#endif

  add_AT_string (die, DW_AT_producer, producer);

  if (strcmp (language_string, "GNU C++") == 0)
    language = DW_LANG_C_plus_plus;
  else if (strcmp (language_string, "GNU Ada") == 0)
    language = DW_LANG_Ada83;
  else if (strcmp (language_string, "GNU F77") == 0)
    language = DW_LANG_Fortran77;
  else if (strcmp (language_string, "GNU Pascal") == 0)
    language = DW_LANG_Pascal83;
  else if (strcmp (language_string, "GNU Java") == 0)
    language = DW_LANG_Java;
  else if (flag_traditional)
    language = DW_LANG_C;
  else
    language = DW_LANG_C89;

  add_AT_unsigned (die, DW_AT_language, language);

  return die;
}

/* Generate a DIE for a string type.  */

static void
gen_string_type_die (type, context_die)
     register tree type;
     register dw_die_ref context_die;
{
  register dw_die_ref type_die
    = new_die (DW_TAG_string_type, scope_die_for (type, context_die));

  equate_type_number_to_die (type, type_die);

  /* Fudge the string length attribute for now.  */

  /* TODO: add string length info.
   string_length_attribute (TYPE_MAX_VALUE (TYPE_DOMAIN (type)));
			      bound_representation (upper_bound, 0, 'u'); */
}

/* Generate the DIE for a base class.  */

static void
gen_inheritance_die (binfo, context_die)
     register tree binfo;
     register dw_die_ref context_die;
{
  dw_die_ref die = new_die (DW_TAG_inheritance, context_die);

  add_type_attribute (die, BINFO_TYPE (binfo), 0, 0, context_die);
  add_data_member_location_attribute (die, binfo);

  if (TREE_VIA_VIRTUAL (binfo))
    add_AT_unsigned (die, DW_AT_virtuality, DW_VIRTUALITY_virtual);
  if (TREE_VIA_PUBLIC (binfo))
    add_AT_unsigned (die, DW_AT_accessibility, DW_ACCESS_public);
  else if (TREE_VIA_PROTECTED (binfo))
    add_AT_unsigned (die, DW_AT_accessibility, DW_ACCESS_protected);
}

/* Generate a DIE for a class member.  */

static void
gen_member_die (type, context_die)
     register tree type;
     register dw_die_ref context_die;
{
  register tree member;
  dw_die_ref child;

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

  /* First output info about the base classes.  */
  if (TYPE_BINFO (type) && TYPE_BINFO_BASETYPES (type))
    {
      register tree bases = TYPE_BINFO_BASETYPES (type);
      register int n_bases = TREE_VEC_LENGTH (bases);
      register int i;

      for (i = 0; i < n_bases; i++)
	gen_inheritance_die (TREE_VEC_ELT (bases, i), context_die);
    }

  /* Now output info about the data members and type members.  */
  for (member = TYPE_FIELDS (type); member; member = TREE_CHAIN (member))
    {
      /* If we thought we were generating minimal debug info for TYPE
	 and then changed our minds, some of the member declarations
	 may have already been defined.  Don't define them again, but
	 do put them in the right order.  */

      child = lookup_decl_die (member);
      if (child)
	splice_child_die (context_die, child);
      else
	gen_decl_die (member, context_die);
    }

  /* Now output info about the function members (if any).  */
  for (member = TYPE_METHODS (type); member; member = TREE_CHAIN (member))
    {
      /* Don't include clones in the member list.  */
      if (DECL_ABSTRACT_ORIGIN (member))
	continue;

      child = lookup_decl_die (member);
      if (child)
	splice_child_die (context_die, child);
      else
	gen_decl_die (member, context_die);
    }
}

/* Generate a DIE for a structure or union type.  If TYPE_DECL_SUPPRESS_DEBUG
   is set, we pretend that the type was never defined, so we only get the
   member DIEs needed by later specification DIEs.  */

static void
gen_struct_or_union_type_die (type, context_die)
     register tree type;
     register dw_die_ref context_die;
{
  register dw_die_ref type_die = lookup_type_die (type);
  register dw_die_ref scope_die = 0;
  register int nested = 0;
  int complete = (TYPE_SIZE (type)
		  && (! TYPE_STUB_DECL (type)
		      || ! TYPE_DECL_SUPPRESS_DEBUG (TYPE_STUB_DECL (type))));

  if (type_die && ! complete)
    return;

  if (TYPE_CONTEXT (type) != NULL_TREE
      && AGGREGATE_TYPE_P (TYPE_CONTEXT (type)))
    nested = 1;

  scope_die = scope_die_for (type, context_die);

  if (! type_die || (nested && scope_die == comp_unit_die))
    /* First occurrence of type or toplevel definition of nested class.  */
    {
      register dw_die_ref old_die = type_die;

      type_die = new_die (TREE_CODE (type) == RECORD_TYPE
			  ? DW_TAG_structure_type : DW_TAG_union_type,
			  scope_die);
      equate_type_number_to_die (type, type_die);
      if (old_die)
	add_AT_die_ref (type_die, DW_AT_specification, old_die);
      else
	add_name_attribute (type_die, type_tag (type));
    }
  else
    remove_AT (type_die, DW_AT_declaration);

  /* If this type has been completed, then give it a byte_size attribute and
     then give a list of members.  */
  if (complete)
    {
      /* Prevent infinite recursion in cases where the type of some member of
         this type is expressed in terms of this type itself.  */
      TREE_ASM_WRITTEN (type) = 1;
      add_byte_size_attribute (type_die, type);
      if (TYPE_STUB_DECL (type) != NULL_TREE)
	add_src_coords_attributes (type_die, TYPE_STUB_DECL (type));

      /* If the first reference to this type was as the return type of an
	 inline function, then it may not have a parent.  Fix this now.  */
      if (type_die->die_parent == NULL)
	add_child_die (scope_die, type_die);

      push_decl_scope (type);
      gen_member_die (type, type_die);
      pop_decl_scope ();

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
      if (! decl_function_context (TYPE_STUB_DECL (type)))
	add_incomplete_type (type);
    }
}

/* Generate a DIE for a subroutine _type_.  */

static void
gen_subroutine_type_die (type, context_die)
     register tree type;
     register dw_die_ref context_die;
{
  register tree return_type = TREE_TYPE (type);
  register dw_die_ref subr_die
    = new_die (DW_TAG_subroutine_type, scope_die_for (type, context_die));

  equate_type_number_to_die (type, subr_die);
  add_prototyped_attribute (subr_die, type);
  add_type_attribute (subr_die, return_type, 0, 0, context_die);
  gen_formal_types_die (type, subr_die);
}

/* Generate a DIE for a type definition */

static void
gen_typedef_die (decl, context_die)
     register tree decl;
     register dw_die_ref context_die;
{
  register dw_die_ref type_die;
  register tree origin;

  if (TREE_ASM_WRITTEN (decl))
    return;
  TREE_ASM_WRITTEN (decl) = 1;

  type_die = new_die (DW_TAG_typedef, context_die);
  origin = decl_ultimate_origin (decl);
  if (origin != NULL)
    add_abstract_origin_attribute (type_die, origin);
  else
    {
      register tree type;
      add_name_and_src_coords_attributes (type_die, decl);
      if (DECL_ORIGINAL_TYPE (decl))
	{
	  type = DECL_ORIGINAL_TYPE (decl);

	  if (type == TREE_TYPE (decl))
	    abort ();
	  else
	    equate_type_number_to_die (TREE_TYPE (decl), type_die);
	}
      else
	type = TREE_TYPE (decl);
      add_type_attribute (type_die, type, TREE_READONLY (decl),
			  TREE_THIS_VOLATILE (decl), context_die);
    }

  if (DECL_ABSTRACT (decl))
    equate_decl_number_to_die (decl, type_die);
}

/* Generate a type description DIE.  */

static void
gen_type_die (type, context_die)
     register tree type;
     register dw_die_ref context_die;
{
  int need_pop;

  if (type == NULL_TREE || type == error_mark_node)
    return;

  /* We are going to output a DIE to represent the unqualified version of
     this type (i.e. without any const or volatile qualifiers) so get the
     main variant (i.e. the unqualified version) of this type now.  */
  type = type_main_variant (type);

  if (TREE_ASM_WRITTEN (type))
    return;

  if (TYPE_NAME (type) && TREE_CODE (TYPE_NAME (type)) == TYPE_DECL
      && DECL_ORIGINAL_TYPE (TYPE_NAME (type)))
    {
      TREE_ASM_WRITTEN (type) = 1;
      gen_decl_die (TYPE_NAME (type), context_die);
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
      gen_type_die (TREE_TYPE (type), context_die);
      break;

    case OFFSET_TYPE:
      /* This code is used for C++ pointer-to-data-member types.
	 Output a description of the relevant class type.  */
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
	gen_array_type_die (type, context_die);
      break;

    case VECTOR_TYPE:
      gen_type_die (TYPE_DEBUG_REPRESENTATION_TYPE (type), context_die);
      break;

    case ENUMERAL_TYPE:
    case RECORD_TYPE:
    case UNION_TYPE:
    case QUAL_UNION_TYPE:
      /* If this is a nested type whose containing class hasn't been
	 written out yet, writing it out will cover this one, too.
         This does not apply to instantiations of member class templates;
	 they need to be added to the containing class as they are
	 generated.  FIXME: This hurts the idea of combining type decls
         from multiple TUs, since we can't predict what set of template
         instantiations we'll get.  */
      if (TYPE_CONTEXT (type)
	  && AGGREGATE_TYPE_P (TYPE_CONTEXT (type))
	  && ! TREE_ASM_WRITTEN (TYPE_CONTEXT (type)))
	{
	  gen_type_die (TYPE_CONTEXT (type), context_die);

	  if (TREE_ASM_WRITTEN (type))
	    return;

	  /* If that failed, attach ourselves to the stub.  */
	  push_decl_scope (TYPE_CONTEXT (type));
	  context_die = lookup_type_die (TYPE_CONTEXT (type));
	  need_pop = 1;
	}
      else
	need_pop = 0;

      if (TREE_CODE (type) == ENUMERAL_TYPE)
	gen_enumeration_type_die (type, context_die);
      else
	gen_struct_or_union_type_die (type, context_die);

      if (need_pop)
	pop_decl_scope ();

      /* Don't set TREE_ASM_WRITTEN on an incomplete struct; we want to fix
	 it up if it is ever completed.  gen_*_type_die will set it for us
	 when appropriate.  */
      return;

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
  if (type == NULL_TREE || type == error_mark_node)
    return;

  /* We are going to output a DIE to represent the unqualified version of
     this type (i.e. without any const or volatile qualifiers) so make sure
     that we have the main variant (i.e. the unqualified version) of this
     type now.  */
  if (type != type_main_variant (type))
    abort ();

  /* Do not check TREE_ASM_WRITTEN (type) as it may not be set if this is
     an instance of an unresolved type.  */

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
      abort ();
    }
}

/* Generate a DW_TAG_lexical_block DIE followed by DIEs to represent all of the
   things which are local to the given block.  */

static void
gen_block_die (stmt, context_die, depth)
     register tree stmt;
     register dw_die_ref context_die;
     int depth;
{
  register int must_output_die = 0;
  register tree origin;
  register tree decl;
  register enum tree_code origin_code;

  /* Ignore blocks never really used to make RTL.  */
  if (stmt == NULL_TREE || !TREE_USED (stmt)
      || (!TREE_ASM_WRITTEN (stmt) && !BLOCK_ABSTRACT (stmt)))
    return;

  /* If the block is one fragment of a non-contiguous block, do not
     process the variables, since they will have been done by the
     origin block.  Do process subblocks.  */
  if (BLOCK_FRAGMENT_ORIGIN (stmt))
    {
      tree sub;

      for (sub= BLOCK_SUBBLOCKS (stmt); sub; sub = BLOCK_CHAIN (sub))
	gen_block_die (sub, context_die, depth + 1);
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
    /* The outer scopes for inlinings *must* always be represented.  We
       generate DW_TAG_inlined_subroutine DIEs for them.  (See below.) */
    must_output_die = 1;
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
      if (! is_body_block (origin ? origin : stmt))
	{
	  /* Determine if this block directly contains any "significant"
	     local declarations which we will need to output DIEs for.  */
	  if (debug_info_level > DINFO_LEVEL_TERSE)
	    /* We are not in terse mode so *any* local declaration counts
	       as being a "significant" one.  */
	    must_output_die = (BLOCK_VARS (stmt) != NULL);
	  else
	    /* We are in terse mode, so only local (nested) function
	       definitions count as "significant" local declarations.  */
	    for (decl = BLOCK_VARS (stmt);
		 decl != NULL; decl = TREE_CHAIN (decl))
	      if (TREE_CODE (decl) == FUNCTION_DECL
		  && DECL_INITIAL (decl))
		{
		  must_output_die = 1;
		  break;
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
	gen_inlined_subroutine_die (stmt, context_die, depth);
      else
	gen_lexical_block_die (stmt, context_die, depth);
    }
  else
    decls_for_scope (stmt, context_die, depth);
}

/* Generate all of the decls declared within a given scope and (recursively)
   all of its sub-blocks.  */

static void
decls_for_scope (stmt, context_die, depth)
     register tree stmt;
     register dw_die_ref context_die;
     int depth;
{
  register tree decl;
  register tree subblocks;

  /* Ignore blocks never really used to make RTL.  */
  if (stmt == NULL_TREE || ! TREE_USED (stmt))
    return;

  /* Output the DIEs to represent all of the data objects and typedefs
     declared directly within this block but not within any nested
     sub-blocks.  Also, nested function and tag DIEs have been
     generated with a parent of NULL; fix that up now.  */
  for (decl = BLOCK_VARS (stmt);
       decl != NULL; decl = TREE_CHAIN (decl))
    {
      register dw_die_ref die;

      if (TREE_CODE (decl) == FUNCTION_DECL)
	die = lookup_decl_die (decl);
      else if (TREE_CODE (decl) == TYPE_DECL && TYPE_DECL_IS_STUB (decl))
	die = lookup_type_die (TREE_TYPE (decl));
      else
	die = NULL;

      if (die != NULL && die->die_parent == NULL)
	add_child_die (context_die, die);
      else
	gen_decl_die (decl, context_die);
    }

  /* Output the DIEs to represent all sub-blocks (and the items declared
     therein) of this block.  */
  for (subblocks = BLOCK_SUBBLOCKS (stmt);
       subblocks != NULL;
       subblocks = BLOCK_CHAIN (subblocks))
    gen_block_die (subblocks, context_die, depth + 1);
}

/* Is this a typedef we can avoid emitting?  */

static inline int
is_redundant_typedef (decl)
     register tree decl;
{
  if (TYPE_DECL_IS_STUB (decl))
    return 1;

  if (DECL_ARTIFICIAL (decl)
      && DECL_CONTEXT (decl)
      && is_tagged_type (DECL_CONTEXT (decl))
      && TREE_CODE (TYPE_NAME (DECL_CONTEXT (decl))) == TYPE_DECL
      && DECL_NAME (decl) == DECL_NAME (TYPE_NAME (DECL_CONTEXT (decl))))
    /* Also ignore the artificial member typedef for the class name.  */
    return 1;

  return 0;
}

/* Generate Dwarf debug information for a decl described by DECL.  */

static void
gen_decl_die (decl, context_die)
     register tree decl;
     register dw_die_ref context_die;
{
  register tree origin;

  if (TREE_CODE (decl) == ERROR_MARK)
    return;

  /* If this ..._DECL node is marked to be ignored, then ignore it.  */
  if (DECL_IGNORED_P (decl))
    return;

  switch (TREE_CODE (decl))
    {
    case CONST_DECL:
      /* The individual enumerators of an enum type get output when we output
         the Dwarf representation of the relevant enum type itself.  */
      break;

    case FUNCTION_DECL:
      /* Don't output any DIEs to represent mere function declarations,
	 unless they are class members or explicit block externs.  */
      if (DECL_INITIAL (decl) == NULL_TREE && DECL_CONTEXT (decl) == NULL_TREE
	  && (current_function_decl == NULL_TREE || DECL_ARTIFICIAL (decl)))
	break;

      /* If we're emitting a clone, emit info for the abstract instance.  */
      if (DECL_ORIGIN (decl) != decl)
	dwarf2out_abstract_function (DECL_ABSTRACT_ORIGIN (decl));
      /* If we're emitting an out-of-line copy of an inline function,
	 emit info for the abstract instance and set up to refer to it.  */
      else if (DECL_INLINE (decl) && ! DECL_ABSTRACT (decl)
	       && ! class_scope_p (context_die)
	       /* dwarf2out_abstract_function won't emit a die if this is just
		  a declaration.  We must avoid setting DECL_ABSTRACT_ORIGIN in
		  that case, because that works only if we have a die.  */
	       && DECL_INITIAL (decl) != NULL_TREE)
	{
	  dwarf2out_abstract_function (decl);
	  set_decl_origin_self (decl);
	}
      /* Otherwise we're emitting the primary DIE for this decl.  */
      else if (debug_info_level > DINFO_LEVEL_TERSE)
	{
	  /* Before we describe the FUNCTION_DECL itself, make sure that we
	     have described its return type.  */
	  gen_type_die (TREE_TYPE (TREE_TYPE (decl)), context_die);

	  /* And its virtual context.  */
	  if (DECL_VINDEX (decl) != NULL_TREE)
	    gen_type_die (DECL_CONTEXT (decl), context_die);

	  /* And its containing type.  */
	  origin = decl_class_context (decl);
	  if (origin != NULL_TREE)
	    gen_type_die_for_member (origin, decl, context_die);
	}

      /* Now output a DIE to represent the function itself.  */
      gen_subprogram_die (decl, context_die);
      break;

    case TYPE_DECL:
      /* If we are in terse mode, don't generate any DIEs to represent any
         actual typedefs.  */
      if (debug_info_level <= DINFO_LEVEL_TERSE)
	break;

      /* In the special case of a TYPE_DECL node representing the
         declaration of some type tag, if the given TYPE_DECL is marked as
         having been instantiated from some other (original) TYPE_DECL node
         (e.g. one which was generated within the original definition of an
         inline function) we have to generate a special (abbreviated)
         DW_TAG_structure_type, DW_TAG_union_type, or DW_TAG_enumeration_type
         DIE here.  */
      if (TYPE_DECL_IS_STUB (decl) && decl_ultimate_origin (decl) != NULL_TREE)
	{
	  gen_tagged_type_instantiation_die (TREE_TYPE (decl), context_die);
	  break;
	}

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
      /* If we are in terse mode, don't generate any DIEs to represent any
         variable declarations or definitions.  */
      if (debug_info_level <= DINFO_LEVEL_TERSE)
	break;

      /* Output any DIEs that are needed to specify the type of this data
         object.  */
      gen_type_die (TREE_TYPE (decl), context_die);

      /* And its containing type.  */
      origin = decl_class_context (decl);
      if (origin != NULL_TREE)
	gen_type_die_for_member (origin, decl, context_die);

      /* Now output the DIE to represent the data object itself.  This gets
         complicated because of the possibility that the VAR_DECL really
         represents an inlined instance of a formal parameter for an inline
         function.  */
      origin = decl_ultimate_origin (decl);
      if (origin != NULL_TREE && TREE_CODE (origin) == PARM_DECL)
	gen_formal_parameter_die (decl, context_die);
      else
	gen_variable_die (decl, context_die);
      break;

    case FIELD_DECL:
      /* Ignore the nameless fields that are used to skip bits, but
	 handle C++ anonymous unions.  */
      if (DECL_NAME (decl) != NULL_TREE
	  || TREE_CODE (TREE_TYPE (decl)) == UNION_TYPE)
	{
	  gen_type_die (member_declared_type (decl), context_die);
	  gen_field_die (decl, context_die);
	}
      break;

    case PARM_DECL:
      gen_type_die (TREE_TYPE (decl), context_die);
      gen_formal_parameter_die (decl, context_die);
      break;

    case NAMESPACE_DECL:
      /* Ignore for now.  */
      break;

    default:
      abort ();
    }
}

/* Add Ada "use" clause information for SGI Workshop debugger.  */

void
dwarf2out_add_library_unit_info (filename, context_list)
     const char *filename;
     const char *context_list;
{
  unsigned int file_index;

  if (filename != NULL)
    {
      dw_die_ref unit_die = new_die (DW_TAG_module, comp_unit_die);
      tree context_list_decl
	= build_decl (LABEL_DECL, get_identifier (context_list),
		      void_type_node);

      TREE_PUBLIC (context_list_decl) = TRUE;
      add_name_attribute (unit_die, context_list);
      file_index = lookup_filename (filename);
      add_AT_unsigned (unit_die, DW_AT_decl_file, file_index);
      add_pubname (context_list_decl, unit_die);
    }
}

/* Debug information for a global DECL.  Called from toplev.c after
   compilation proper has finished.  */
static void
dwarf2out_global_decl (decl)
     tree decl;
{
  /* Output DWARF2 information for file-scope tentative data object
     declarations, file-scope (extern) function declarations (which
     had no corresponding body) and file-scope tagged type
     declarations and definitions which have not yet been forced out.  */

  if (TREE_CODE (decl) != FUNCTION_DECL || !DECL_INITIAL (decl))
    dwarf2out_decl (decl);
}

/* Write the debugging output for DECL.  */

void
dwarf2out_decl (decl)
     register tree decl;
{
  register dw_die_ref context_die = comp_unit_die;

  if (TREE_CODE (decl) == ERROR_MARK)
    return;

  /* If this ..._DECL node is marked to be ignored, then ignore it.  */
  if (DECL_IGNORED_P (decl))
    return;

  switch (TREE_CODE (decl))
    {
    case FUNCTION_DECL:
      /* Ignore this FUNCTION_DECL if it refers to a builtin declaration of a
         builtin function.  Explicit programmer-supplied declarations of
         these same functions should NOT be ignored however.  */
      if (DECL_EXTERNAL (decl) && DECL_BUILT_IN (decl))
	return;

      /* What we would really like to do here is to filter out all mere
         file-scope declarations of file-scope functions which are never
         referenced later within this translation unit (and keep all of ones
         that *are* referenced later on) but we aren't clairvoyant, so we have
         no idea which functions will be referenced in the future (i.e. later
         on within the current translation unit). So here we just ignore all
         file-scope function declarations which are not also definitions.  If
         and when the debugger needs to know something about these functions,
         it will have to hunt around and find the DWARF information associated
         with the definition of the function.  Note that we can't just check
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
         to generate DWARF for them anyway. Note that the C++ front-end also
         plays some similar games for inline function definitions appearing
         within include files which also contain
	 `#pragma interface' pragmas.  */
      if (DECL_INITIAL (decl) == NULL_TREE)
	return;

      /* If we're a nested function, initially use a parent of NULL; if we're
	 a plain function, this will be fixed up in decls_for_scope.  If
	 we're a method, it will be ignored, since we already have a DIE.  */
      if (decl_function_context (decl))
	context_die = NULL;

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
	return;

      /* If we are in terse mode, don't generate any DIEs to represent any
         variable declarations or definitions.  */
      if (debug_info_level <= DINFO_LEVEL_TERSE)
	return;
      break;

    case TYPE_DECL:
      /* Don't emit stubs for types unless they are needed by other DIEs.  */
      if (TYPE_DECL_SUPPRESS_DEBUG (decl))
	return;

      /* Don't bother trying to generate any DIEs to represent any of the
         normal built-in types for the language we are compiling.  */
      if (DECL_SOURCE_LINE (decl) == 0)
	{
	  /* OK, we need to generate one for `bool' so GDB knows what type
             comparisons have.  */
	  if ((get_AT_unsigned (comp_unit_die, DW_AT_language)
	       == DW_LANG_C_plus_plus)
	      && TREE_CODE (TREE_TYPE (decl)) == BOOLEAN_TYPE)
	    modified_type_die (TREE_TYPE (decl), 0, 0, NULL);

	  return;
	}

      /* If we are in terse mode, don't generate any DIEs for types.  */
      if (debug_info_level <= DINFO_LEVEL_TERSE)
	return;

      /* If we're a function-scope tag, initially use a parent of NULL;
	 this will be fixed up in decls_for_scope.  */
      if (decl_function_context (decl))
	context_die = NULL;

      break;

    default:
      return;
    }

  gen_decl_die (decl, context_die);
}

/* Output a marker (i.e. a label) for the beginning of the generated code for
   a lexical block.  */

static void
dwarf2out_begin_block (line, blocknum)
     unsigned int line ATTRIBUTE_UNUSED;
     unsigned int blocknum;
{
  function_section (current_function_decl);
  ASM_OUTPUT_DEBUG_LABEL (asm_out_file, BLOCK_BEGIN_LABEL, blocknum);
}

/* Output a marker (i.e. a label) for the end of the generated code for a
   lexical block.  */

static void
dwarf2out_end_block (line, blocknum)
     unsigned int line ATTRIBUTE_UNUSED;
     unsigned int blocknum;
{
  function_section (current_function_decl);
  ASM_OUTPUT_DEBUG_LABEL (asm_out_file, BLOCK_END_LABEL, blocknum);
}

/* Returns nonzero if it is appropriate not to emit any debugging
   information for BLOCK, because it doesn't contain any instructions.

   Don't allow this for blocks with nested functions or local classes
   as we would end up with orphans, and in the presence of scheduling
   we may end up calling them anyway.  */

static bool
dwarf2out_ignore_block (block)
     tree block;
{
  tree decl;
  for (decl = BLOCK_VARS (block); decl; decl = TREE_CHAIN (decl))
    if (TREE_CODE (decl) == FUNCTION_DECL
	|| (TREE_CODE (decl) == TYPE_DECL && TYPE_DECL_IS_STUB (decl)))
      return 0;
  return 1;
}

/* Lookup a filename (in the list of filenames that we know about here in
   dwarf2out.c) and return its "index".  The index of each (known) filename is
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
     const char *file_name;
{
  register unsigned i;

  /* ??? Why isn't DECL_SOURCE_FILE left null instead.  */
  if (strcmp (file_name, "<internal>") == 0
      || strcmp (file_name, "<built-in>") == 0)
    return 0;

  /* Check to see if the file name that was searched on the previous
     call matches this file name.  If so, return the index.  */
  if (file_table.last_lookup_index != 0)
    if (strcmp (file_name, file_table.table[file_table.last_lookup_index]) == 0)
      return file_table.last_lookup_index;

  /* Didn't match the previous lookup, search the table */
  for (i = 1; i < file_table.in_use; ++i)
    if (strcmp (file_name, file_table.table[i]) == 0)
      {
	file_table.last_lookup_index = i;
	return i;
      }

  /* Prepare to add a new table entry by making sure there is enough space in
     the table to do so.  If not, expand the current table.  */
  if (i == file_table.allocated)
    {
      file_table.allocated = i + FILE_TABLE_INCREMENT;
      file_table.table = (char **)
	xrealloc (file_table.table, file_table.allocated * sizeof (char *));
    }

  /* Add the new entry to the end of the filename table.  */
  file_table.table[i] = xstrdup (file_name);
  file_table.in_use = i + 1;
  file_table.last_lookup_index = i;

  if (DWARF2_ASM_LINE_DEBUG_INFO)
    fprintf (asm_out_file, "\t.file %u \"%s\"\n", i, file_name);

  return i;
}

static void
init_file_table ()
{
  /* Allocate the initial hunk of the file_table.  */
  file_table.table = (char **) xcalloc (FILE_TABLE_INCREMENT, sizeof (char *));
  file_table.allocated = FILE_TABLE_INCREMENT;

  /* Skip the first entry - file numbers begin at 1.  */
  file_table.in_use = 1;
  file_table.last_lookup_index = 0;
}

/* Output a label to mark the beginning of a source code line entry
   and record information relating to this source line, in
   'line_info_table' for later output of the .debug_line section.  */

static void
dwarf2out_source_line (line, filename)
     unsigned int line;
     register const char *filename;
{
  if (debug_info_level >= DINFO_LEVEL_NORMAL)
    {
      function_section (current_function_decl);

      /* If requested, emit something human-readable.  */
      if (flag_debug_asm)
	fprintf (asm_out_file, "\t%s %s:%d\n", ASM_COMMENT_START,
		 filename, line);

      if (DWARF2_ASM_LINE_DEBUG_INFO)
	{
	  unsigned file_num = lookup_filename (filename);

	  /* Emit the .loc directive understood by GNU as.  */
	  fprintf (asm_out_file, "\t.loc %d %d 0\n", file_num, line);

	  /* Indicate that line number info exists.  */
	  ++line_info_table_in_use;

	  /* Indicate that multiple line number tables exist.  */
	  if (DECL_SECTION_NAME (current_function_decl))
	    ++separate_line_info_table_in_use;
	}
      else if (DECL_SECTION_NAME (current_function_decl))
	{
	  register dw_separate_line_info_ref line_info;
	  ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, SEPARATE_LINE_CODE_LABEL,
				     separate_line_info_table_in_use);

	  /* expand the line info table if necessary */
	  if (separate_line_info_table_in_use
	      == separate_line_info_table_allocated)
	    {
	      separate_line_info_table_allocated += LINE_INFO_TABLE_INCREMENT;
	      separate_line_info_table
		= (dw_separate_line_info_ref)
		  xrealloc (separate_line_info_table,
			    separate_line_info_table_allocated
			    * sizeof (dw_separate_line_info_entry));
	    }

	  /* Add the new entry at the end of the line_info_table.  */
	  line_info
	    = &separate_line_info_table[separate_line_info_table_in_use++];
	  line_info->dw_file_num = lookup_filename (filename);
	  line_info->dw_line_num = line;
	  line_info->function = current_funcdef_number;
	}
      else
	{
	  register dw_line_info_ref line_info;

	  ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, LINE_CODE_LABEL,
				     line_info_table_in_use);

	  /* Expand the line info table if necessary.  */
	  if (line_info_table_in_use == line_info_table_allocated)
	    {
	      line_info_table_allocated += LINE_INFO_TABLE_INCREMENT;
	      line_info_table
		= (dw_line_info_ref)
		  xrealloc (line_info_table,
			    (line_info_table_allocated
			     * sizeof (dw_line_info_entry)));
	    }

	  /* Add the new entry at the end of the line_info_table.  */
	  line_info = &line_info_table[line_info_table_in_use++];
	  line_info->dw_file_num = lookup_filename (filename);
	  line_info->dw_line_num = line;
	}
    }
}

/* Record the beginning of a new source file.  */

static void
dwarf2out_start_source_file (lineno, filename)
     register unsigned int lineno;
     register const char *filename;
{
  if (flag_eliminate_dwarf2_dups)
    {
      /* Record the beginning of the file for break_out_includes.  */
      dw_die_ref bincl_die = new_die (DW_TAG_GNU_BINCL, comp_unit_die);
      add_AT_string (bincl_die, DW_AT_name, filename);
    }
  if (debug_info_level >= DINFO_LEVEL_VERBOSE)
    {
      named_section_flags (DEBUG_MACINFO_SECTION, SECTION_DEBUG, 1);
      dw2_asm_output_data (1, DW_MACINFO_start_file, "Start new file");
      dw2_asm_output_data_uleb128 (lineno, "Included from line number %d",
				   lineno);
      dw2_asm_output_data_uleb128 (lookup_filename (filename),
				   "Filename we just started");
    }
}

/* Record the end of a source file.  */

static void
dwarf2out_end_source_file (lineno)
     unsigned int lineno ATTRIBUTE_UNUSED;
{
  if (flag_eliminate_dwarf2_dups)
    {
      /* Record the end of the file for break_out_includes.  */
      new_die (DW_TAG_GNU_EINCL, comp_unit_die);
    }
  if (debug_info_level >= DINFO_LEVEL_VERBOSE)
    {
      named_section_flags (DEBUG_MACINFO_SECTION, SECTION_DEBUG, 1);
      dw2_asm_output_data (1, DW_MACINFO_end_file, "End file");
    }
}

/* Called from debug_define in toplev.c.  The `buffer' parameter contains
   the tail part of the directive line, i.e. the part which is past the
   initial whitespace, #, whitespace, directive-name, whitespace part.  */

static void
dwarf2out_define (lineno, buffer)
     register unsigned lineno ATTRIBUTE_UNUSED;
     register const char *buffer ATTRIBUTE_UNUSED;
{
  static int initialized = 0;
  if (!initialized)
    {
      dwarf2out_start_source_file (0, primary_filename);
      initialized = 1;
    }
  if (debug_info_level >= DINFO_LEVEL_VERBOSE)
    {
      named_section_flags (DEBUG_MACINFO_SECTION, SECTION_DEBUG, 1);
      dw2_asm_output_data (1, DW_MACINFO_define, "Define macro");
      dw2_asm_output_data_uleb128 (lineno, "At line number %d", lineno);
      dw2_asm_output_nstring (buffer, -1, "The macro");
    }
}

/* Called from debug_undef in toplev.c.  The `buffer' parameter contains
   the tail part of the directive line, i.e. the part which is past the
   initial whitespace, #, whitespace, directive-name, whitespace part.  */

static void
dwarf2out_undef (lineno, buffer)
     register unsigned lineno ATTRIBUTE_UNUSED;
     register const char *buffer ATTRIBUTE_UNUSED;
{
  if (debug_info_level >= DINFO_LEVEL_VERBOSE)
    {
      named_section_flags (DEBUG_MACINFO_SECTION, SECTION_DEBUG, 1);
      dw2_asm_output_data (1, DW_MACINFO_undef, "Undefine macro");
      dw2_asm_output_data_uleb128 (lineno, "At line number %d", lineno);
      dw2_asm_output_nstring (buffer, -1, "The macro");
    }
}

/* Set up for Dwarf output at the start of compilation.  */

static void
dwarf2out_init (main_input_filename)
     register const char *main_input_filename;
{
  init_file_table ();

  /* Remember the name of the primary input file.  */
  primary_filename = main_input_filename;

  /* Add it to the file table first, under the assumption that we'll
     be emitting line number data for it first, which avoids having
     to add an initial DW_LNS_set_file.  */
  lookup_filename (main_input_filename);

  /* Allocate the initial hunk of the decl_die_table.  */
  decl_die_table
    = (dw_die_ref *) xcalloc (DECL_DIE_TABLE_INCREMENT, sizeof (dw_die_ref));
  decl_die_table_allocated = DECL_DIE_TABLE_INCREMENT;
  decl_die_table_in_use = 0;

  /* Allocate the initial hunk of the decl_scope_table.  */
  decl_scope_table
    = (tree *) xcalloc (DECL_SCOPE_TABLE_INCREMENT, sizeof (tree));
  decl_scope_table_allocated = DECL_SCOPE_TABLE_INCREMENT;
  decl_scope_depth = 0;

  /* Allocate the initial hunk of the abbrev_die_table.  */
  abbrev_die_table
    = (dw_die_ref *) xcalloc (ABBREV_DIE_TABLE_INCREMENT,
			      sizeof (dw_die_ref));
  abbrev_die_table_allocated = ABBREV_DIE_TABLE_INCREMENT;
  /* Zero-th entry is allocated, but unused */
  abbrev_die_table_in_use = 1;

  /* Allocate the initial hunk of the line_info_table.  */
  line_info_table
    = (dw_line_info_ref) xcalloc (LINE_INFO_TABLE_INCREMENT,
				  sizeof (dw_line_info_entry));
  line_info_table_allocated = LINE_INFO_TABLE_INCREMENT;
  /* Zero-th entry is allocated, but unused */
  line_info_table_in_use = 1;

  /* Generate the initial DIE for the .debug section.  Note that the (string)
     value given in the DW_AT_name attribute of the DW_TAG_compile_unit DIE
     will (typically) be a relative pathname and that this pathname should be
     taken as being relative to the directory from which the compiler was
     invoked when the given (base) source file was compiled.  */
  comp_unit_die = gen_compile_unit_die (main_input_filename);

  VARRAY_RTX_INIT (used_rtx_varray, 32, "used_rtx_varray");
  ggc_add_rtx_varray_root (&used_rtx_varray, 1);

  ASM_GENERATE_INTERNAL_LABEL (text_end_label, TEXT_END_LABEL, 0);
  ASM_GENERATE_INTERNAL_LABEL (abbrev_section_label,
			       DEBUG_ABBREV_SECTION_LABEL, 0);
  if (DWARF2_GENERATE_TEXT_SECTION_LABEL)
    ASM_GENERATE_INTERNAL_LABEL (text_section_label, TEXT_SECTION_LABEL, 0);
  else
    strcpy (text_section_label, stripattributes (TEXT_SECTION));
  ASM_GENERATE_INTERNAL_LABEL (debug_info_section_label,
			       DEBUG_INFO_SECTION_LABEL, 0);
  ASM_GENERATE_INTERNAL_LABEL (debug_line_section_label,
			       DEBUG_LINE_SECTION_LABEL, 0);
  ASM_GENERATE_INTERNAL_LABEL (loc_section_label, DEBUG_LOC_SECTION_LABEL, 0);
  named_section_flags (DEBUG_LOC_SECTION, SECTION_DEBUG, 1);
  ASM_OUTPUT_LABEL (asm_out_file, loc_section_label);
  named_section_flags (DEBUG_ABBREV_SECTION, SECTION_DEBUG, 1);
  ASM_OUTPUT_LABEL (asm_out_file, abbrev_section_label);
  named_section_flags (DEBUG_INFO_SECTION, SECTION_DEBUG, 1);
  ASM_OUTPUT_LABEL (asm_out_file, debug_info_section_label);
  named_section_flags (DEBUG_LINE_SECTION, SECTION_DEBUG, 1);
  ASM_OUTPUT_LABEL (asm_out_file, debug_line_section_label);
  if (debug_info_level >= DINFO_LEVEL_VERBOSE)
    {
      named_section_flags (DEBUG_MACINFO_SECTION, SECTION_DEBUG, 1);
      ASM_GENERATE_INTERNAL_LABEL (macinfo_section_label,
				   DEBUG_MACINFO_SECTION_LABEL, 0);
      ASM_OUTPUT_LABEL (asm_out_file, macinfo_section_label);
    }

  if (DWARF2_GENERATE_TEXT_SECTION_LABEL)
    {
      text_section ();
      ASM_OUTPUT_LABEL (asm_out_file, text_section_label);
    }
}

/* Output stuff that dwarf requires at the end of every file,
   and generate the DWARF-2 debugging info.  */

static void
dwarf2out_finish (input_filename)
     register const char *input_filename ATTRIBUTE_UNUSED;
{
  limbo_die_node *node, *next_node;
  dw_die_ref die = 0;

  /* Traverse the limbo die list, and add parent/child links.  The only
     dies without parents that should be here are concrete instances of
     inline functions, and the comp_unit_die.  We can ignore the comp_unit_die.
     For concrete instances, we can get the parent die from the abstract
     instance.  */
  for (node = limbo_die_list; node; node = next_node)
    {
      next_node = node->next;
      die = node->die;

      if (die->die_parent == NULL)
	{
	  dw_die_ref origin = get_AT_ref (die, DW_AT_abstract_origin);
	  if (origin)
	    add_child_die (origin->die_parent, die);
	  else if (die == comp_unit_die)
	    ;
	  else
	    abort ();
	}
      free (node);
    }
  limbo_die_list = NULL;

  /* Walk through the list of incomplete types again, trying once more to
     emit full debugging info for them.  */
  retry_incomplete_types ();

  /* We need to reverse all the dies before break_out_includes, or
     we'll see the end of an include file before the beginning.  */
  reverse_all_dies (comp_unit_die);

  /* Generate separate CUs for each of the include files we've seen.
     They will go into limbo_die_list.  */
  if (flag_eliminate_dwarf2_dups)
    break_out_includes (comp_unit_die);

  /* Traverse the DIE's and add add sibling attributes to those DIE's
     that have children.  */
  add_sibling_attributes (comp_unit_die);
  for (node = limbo_die_list; node; node = node->next)
    add_sibling_attributes (node->die);

  /* Output a terminator label for the .text section.  */
  text_section ();
  ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, TEXT_END_LABEL, 0);

  /* Output the source line correspondence table.  We must do this
     even if there is no line information.  Otherwise, on an empty
     translation unit, we will generate a present, but empty,
     .debug_info section.  IRIX 6.5 `nm' will then complain when
     examining the file.  */
  if (! DWARF2_ASM_LINE_DEBUG_INFO)
    {
      named_section_flags (DEBUG_LINE_SECTION, SECTION_DEBUG, 1);
      output_line_info ();
    }

  /* We can only use the low/high_pc attributes if all of the code was
     in .text.  */
  if (separate_line_info_table_in_use == 0)
    {
      add_AT_lbl_id (comp_unit_die, DW_AT_low_pc, text_section_label);
      add_AT_lbl_id (comp_unit_die, DW_AT_high_pc, text_end_label);
    }
  /* And if it wasn't, we need to give .debug_loc and .debug_ranges
     an appropriate "base address".  Use zero so that these addresses
     become absolute.  */
  else if (have_location_lists || ranges_table_in_use)
    add_AT_addr (comp_unit_die, DW_AT_entry_pc, const0_rtx);

  if (debug_info_level >= DINFO_LEVEL_NORMAL)
    add_AT_lbl_offset (comp_unit_die, DW_AT_stmt_list,
		       debug_line_section_label);

  if (debug_info_level >= DINFO_LEVEL_VERBOSE)
    add_AT_lbl_offset (comp_unit_die, DW_AT_macro_info, macinfo_section_label);

  /* Output all of the compilation units.  We put the main one last so that
     the offsets are available to output_pubnames.  */
  for (node = limbo_die_list; node; node = node->next)
    output_comp_unit (node->die);
  output_comp_unit (comp_unit_die);

  /* Output the abbreviation table.  */
  named_section_flags (DEBUG_ABBREV_SECTION, SECTION_DEBUG, 1);
  output_abbrev_section ();

  if (pubname_table_in_use)
    {
      /* Output public names table.  */
      named_section_flags (DEBUG_PUBNAMES_SECTION, SECTION_DEBUG, 1);
      output_pubnames ();
    }

  /* We only put functions in the arange table, so don't write it out if
     we don't have any.  */
  if (fde_table_in_use)
    {
      /* Output the address range information.  */
      named_section_flags (DEBUG_ARANGES_SECTION, SECTION_DEBUG, 1);
      output_aranges ();
    }

  /* Output location list section if necessary.  */
  if (have_location_lists)
    {
      /* Output the location lists info.  */
      named_section_flags (DEBUG_LOC_SECTION, SECTION_DEBUG, 1);
      output_location_lists (die);
      have_location_lists = 0;
    }

  /* Output ranges section if necessary.  */
  if (ranges_table_in_use)
    {
      named_section_flags (DEBUG_RANGES_SECTION, SECTION_DEBUG, 1);
      output_ranges ();
    }

  /* Have to end the primary source file.  */
  if (debug_info_level >= DINFO_LEVEL_VERBOSE)
    { 
      named_section_flags (DEBUG_MACINFO_SECTION, SECTION_DEBUG, 1);
      dw2_asm_output_data (1, DW_MACINFO_end_file, "End file");
    }
}
#endif /* DWARF2_DEBUGGING_INFO */
