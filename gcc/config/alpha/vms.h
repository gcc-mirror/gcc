/* Output variables, constants and external declarations, for GNU compiler.
   Copyright (C) 1996, 1997, 1998, 2000, 2001, 2002
   Free Software Foundation, Inc.

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

#define TARGET_OBJECT_SUFFIX ".obj"
#define TARGET_EXECUTABLE_SUFFIX ".exe"

/* This enables certain macros in alpha.h, which will make an indirect
   reference to an external symbol an invalid address.  This needs to be
   defined before we include alpha.h, since it determines which macros
   are used for GO_IF_*.  */

#define NO_EXTERNAL_INDIRECT_ADDRESS

#define TARGET_OS_CPP_BUILTINS()		\
    do {					\
	builtin_define_std ("vms");		\
	builtin_define_std ("VMS");		\
	builtin_define ("__ALPHA");		\
	builtin_assert ("system=vms");		\
	if (TARGET_FLOAT_VAX)			\
	  builtin_define ("__G_FLOAT");		\
	else					\
	  builtin_define ("__IEEE_FLOAT");	\
    } while (0)

/* By default, allow $ to be part of an identifier.  */
#define DOLLARS_IN_IDENTIFIERS 2

#undef TARGET_DEFAULT
#define TARGET_DEFAULT (MASK_FP|MASK_FPREGS|MASK_GAS)
#undef TARGET_ABI_OPEN_VMS
#define TARGET_ABI_OPEN_VMS 1

#undef TARGET_NAME   
#define TARGET_NAME "OpenVMS/Alpha"
#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (%s)", TARGET_NAME);           

/* The structure return address arrives as an "argument" on VMS.  */
#undef STRUCT_VALUE_REGNUM
#define STRUCT_VALUE 0
#undef PCC_STATIC_STRUCT_RETURN

/* "long" is 32 bits, but 64 bits for Ada.  */
#undef LONG_TYPE_SIZE
#define LONG_TYPE_SIZE 32
#define ADA_LONG_TYPE_SIZE 64

/* Pointer is 32 bits but the hardware has 64-bit addresses, sign extended.  */
#undef POINTER_SIZE
#define POINTER_SIZE 32
#define POINTERS_EXTEND_UNSIGNED 0

#define MAX_OFILE_ALIGNMENT 524288  /* 8 x 2^16 by DEC Ada Test CD40VRA */

#undef FIXED_REGISTERS
#define FIXED_REGISTERS  \
 {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, \
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 }

#undef CALL_USED_REGISTERS
#define CALL_USED_REGISTERS  \
 {1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, \
  1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, \
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 }

/* List the order in which to allocate registers.  Each register must be
   listed once, even those in FIXED_REGISTERS.

   We allocate in the following order:
   $f1			(nonsaved floating-point register)
   $f10-$f15		(likewise)
   $f22-$f30		(likewise)
   $f21-$f16		(likewise, but input args)
   $f0			(nonsaved, but return value)
   $f2-$f9		(saved floating-point registers)
   $1			(nonsaved integer registers)
   $22-$25		(likewise)
   $28			(likewise)
   $0			(likewise, but return value)
   $21-$16		(likewise, but input args)
   $27			(procedure value in OSF, nonsaved in NT)
   $2-$8		(saved integer registers)
   $9-$14		(saved integer registers)
   $26			(return PC)
   $15			(frame pointer)
   $29			(global pointer)
   $30, $31, $f31	(stack pointer and always zero/ap & fp)  */

#undef REG_ALLOC_ORDER
#define REG_ALLOC_ORDER		\
  {33,					\
   42, 43, 44, 45, 46, 47,		\
   54, 55, 56, 57, 58, 59, 60, 61, 62,	\
   53, 52, 51, 50, 49, 48,		\
   32,					\
   34, 35, 36, 37, 38, 39, 40, 41,	\
   1,					\
   22, 23, 24, 25,			\
   28,					\
   0,					\
   21, 20, 19, 18, 17, 16,		\
   27,					\
   2, 3, 4, 5, 6, 7, 8,			\
   9, 10, 11, 12, 13, 14,		\
   26,					\
   15,					\
   29,					\
   30, 31, 63 }

#undef HARD_FRAME_POINTER_REGNUM
#define HARD_FRAME_POINTER_REGNUM 29

/* Define registers used by the epilogue and return instruction.  */
#undef EPILOGUE_USES
#define EPILOGUE_USES(REGNO)    ((REGNO) == 26 || (REGNO) == 29)

#undef CAN_ELIMINATE
#define CAN_ELIMINATE(FROM, TO)  \
((TO) != STACK_POINTER_REGNUM || ! alpha_using_fp ())

#undef INITIAL_ELIMINATION_OFFSET
#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET)			\
{ if ((FROM) == FRAME_POINTER_REGNUM)					\
    (OFFSET) = alpha_sa_size () + alpha_pv_save_size ();		\
  else if ((FROM) == ARG_POINTER_REGNUM)				\
    (OFFSET) = (ALPHA_ROUND (alpha_sa_size () + alpha_pv_save_size ()	\
			     + get_frame_size ()			\
			     + current_function_pretend_args_size)	\
		- current_function_pretend_args_size);			\
  else									\
    abort();								\
  if ((TO) == STACK_POINTER_REGNUM)					\
    (OFFSET) += ALPHA_ROUND (current_function_outgoing_args_size);	\
}

/* Define a data type for recording info about an argument list
   during the scan of that argument list.  This data type should
   hold all necessary information about the function itself
   and about the args processed so far, enough to enable macros
   such as FUNCTION_ARG to determine where the next arg should go.

   On Alpha/VMS, this is a structure that contains the number of
   arguments and, for each argument, the datatype of that argument.

   The number of arguments is a number of words of arguments scanned so far.
   Thus 6 or more means all following args should go on the stack.  */

enum avms_arg_type {I64, FF, FD, FG, FS, FT};
typedef struct {int num_args; enum avms_arg_type atypes[6];} avms_arg_info;

#undef CUMULATIVE_ARGS
#define CUMULATIVE_ARGS avms_arg_info

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.  */

#undef INIT_CUMULATIVE_ARGS
#define INIT_CUMULATIVE_ARGS(CUM,FNTYPE,LIBNAME,INDIRECT) \
  (CUM).num_args = 0;						\
  (CUM).atypes[0] = (CUM).atypes[1] = (CUM).atypes[2] = I64;	\
  (CUM).atypes[3] = (CUM).atypes[4] = (CUM).atypes[5] = I64;

#undef FUNCTION_ARG_ADVANCE
#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED)			\
  if (MUST_PASS_IN_STACK (MODE, TYPE))					\
    (CUM).num_args += 6;						\
  else									\
    {									\
      if ((CUM).num_args < 6)						\
        (CUM).atypes[(CUM).num_args] = alpha_arg_type (MODE);		\
									\
     (CUM).num_args += ALPHA_ARG_SIZE (MODE, TYPE, NAMED);		\
    }

/* For an arg passed partly in registers and partly in memory,
   this is the number of registers used.
   For args passed entirely in registers or entirely in memory, zero.  */

#undef FUNCTION_ARG_PARTIAL_NREGS
#define FUNCTION_ARG_PARTIAL_NREGS(CUM, MODE, TYPE, NAMED)		\
((CUM).num_args < 6 && 6 < (CUM).num_args				\
   + ALPHA_ARG_SIZE (MODE, TYPE, NAMED)					\
 ? 6 - (CUM).num_args : 0)

/* Perform any needed actions needed for a function that is receiving a
   variable number of arguments. 

   CUM is as for INIT_CUMULATIVE_ARGS.

   MODE and TYPE are the mode and type of the current parameter.

   PRETEND_SIZE is a variable that should be set to the amount of stack
   that must be pushed by the prolog to pretend that our caller pushed
   it.

   Normally, this macro will push all remaining incoming registers on the
   stack and set PRETEND_SIZE to the length of the registers pushed. 

   For VMS, we allocate space for all 6 arg registers plus a count.

   However, if NO registers need to be saved, don't allocate any space.
   This is not only because we won't need the space, but because AP includes
   the current_pretend_args_size and we don't want to mess up any
   ap-relative addresses already made.  */

#undef SETUP_INCOMING_VARARGS
#define SETUP_INCOMING_VARARGS(CUM,MODE,TYPE,PRETEND_SIZE,NO_RTL)	\
{ if ((CUM).num_args < 6)				\
    {							\
      if (! (NO_RTL))					\
	{						\
	  emit_move_insn (gen_rtx_REG (DImode, 1),	\
			  virtual_incoming_args_rtx);	\
	  emit_insn (gen_arg_home ());			\
	}						\
						        \
      PRETEND_SIZE = 7 * UNITS_PER_WORD;		\
    }							\
}

/* ABI has stack checking, but it's broken.  */
#undef STACK_CHECK_BUILTIN
#define STACK_CHECK_BUILTIN 0

#undef ASM_FILE_START
#define ASM_FILE_START(FILE)					\
{								\
  alpha_write_verstamp (FILE);					\
  fprintf (FILE, "\t.set noreorder\n");				\
  fprintf (FILE, "\t.set volatile\n");				\
  if (TARGET_BWX | TARGET_MAX | TARGET_FIX | TARGET_CIX)	\
    {								\
      fprintf (FILE, "\t.arch %s\n",				\
               (TARGET_CPU_EV6 ? "ev6"				\
                : TARGET_MAX ? "pca56" : "ev56"));		\
    }								\
  ASM_OUTPUT_SOURCE_FILENAME (FILE, main_input_filename);	\
}

#define LINK_SECTION_ASM_OP "\t.link"
#define READONLY_DATA_SECTION_ASM_OP "\t.rdata"
#define LITERALS_SECTION_ASM_OP "\t.literals"
#define CTORS_SECTION_ASM_OP "\t.ctors"
#define DTORS_SECTION_ASM_OP "\t.dtors"

#undef EXTRA_SECTIONS
#define EXTRA_SECTIONS	in_link, in_literals

#undef EXTRA_SECTION_FUNCTIONS
#define EXTRA_SECTION_FUNCTIONS					\
void								\
link_section ()							\
{								\
  if (in_section != in_link)					\
    {								\
      fprintf (asm_out_file, "%s\n", LINK_SECTION_ASM_OP); 	\
      in_section = in_link;					\
    }								\
}                                                               \
void								\
literals_section ()						\
{								\
  if (in_section != in_literals)				\
    {								\
      fprintf (asm_out_file, "%s\n", LITERALS_SECTION_ASM_OP); 	\
      in_section = in_literals;					\
    }								\
}

extern void link_section	PARAMS ((void));
extern void literals_section	PARAMS ((void));

#undef ASM_OUTPUT_ADDR_DIFF_ELT
#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, BODY, VALUE, REL) abort ()

#undef ASM_OUTPUT_ADDR_VEC_ELT
#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE) \
  fprintf (FILE, "\t.quad $L%d\n", (VALUE))

#undef CASE_VECTOR_MODE
#define CASE_VECTOR_MODE DImode
#undef CASE_VECTOR_PC_RELATIVE

#undef ASM_OUTPUT_CASE_LABEL
#define ASM_OUTPUT_CASE_LABEL(FILE,PREFIX,NUM,TABLEINSN)	\
{ ASM_OUTPUT_ALIGN (FILE, 3); ASM_OUTPUT_INTERNAL_LABEL (FILE, PREFIX, NUM); }

/* This says how to output assembler code to declare an                
   uninitialized external linkage data object.  */ 

#define COMMON_ASM_OP "\t.comm\t"

#undef ASM_OUTPUT_ALIGNED_COMMON
#define ASM_OUTPUT_ALIGNED_COMMON(FILE, NAME, SIZE, ALIGN)		\
do {									\
  fprintf ((FILE), "%s", COMMON_ASM_OP);				\
  assemble_name ((FILE), (NAME));					\
  fprintf ((FILE), ",%u,%u\n", (SIZE), (ALIGN) / BITS_PER_UNIT);	\
} while (0)


/* Output assembler code for a block containing the constant parts
   of a trampoline, leaving space for the variable parts.

   The trampoline should set the static chain pointer to value placed
   into the trampoline and should branch to the specified routine.  
   Note that $27 has been set to the address of the trampoline, so we can
   use it for addressability of the two data items.  */

#undef TRAMPOLINE_TEMPLATE
#define TRAMPOLINE_TEMPLATE(FILE)		\
{						\
  fprintf (FILE, "\t.quad 0\n");		\
  fprintf (FILE, "\t.linkage __tramp\n");	\
  fprintf (FILE, "\t.quad 0\n");		\
}

/* Length in units of the trampoline for entering a nested function.  */

#undef TRAMPOLINE_SIZE
#define TRAMPOLINE_SIZE    32

/* The alignment of a trampoline, in bits.  */

#undef TRAMPOLINE_ALIGNMENT
#define TRAMPOLINE_ALIGNMENT  64

/* Emit RTL insns to initialize the variable parts of a trampoline.
   FNADDR is an RTX for the address of the function's pure code.
   CXT is an RTX for the static chain value for the function.  */

#undef INITIALIZE_TRAMPOLINE
#define INITIALIZE_TRAMPOLINE(TRAMP, FNADDR, CXT) \
  alpha_initialize_trampoline (TRAMP, FNADDR, CXT, 16, 24, -1)

/* Control how constructors and destructors are emitted.  */
#define TARGET_ASM_CONSTRUCTOR  vms_asm_out_constructor
#define TARGET_ASM_DESTRUCTOR   vms_asm_out_destructor

#undef SDB_DEBUGGING_INFO
#undef MIPS_DEBUGGING_INFO
#undef DBX_DEBUGGING_INFO

#define DWARF2_DEBUGGING_INFO 1
#define VMS_DEBUGGING_INFO 1

#define DWARF2_UNWIND_INFO 1

#undef EH_RETURN_HANDLER_RTX
#define EH_RETURN_HANDLER_RTX \
  gen_rtx_MEM (Pmode, plus_constant (stack_pointer_rtx, 8))

#define LINK_EH_SPEC "vms-dwarf2eh.o%s "

#ifdef IN_LIBGCC2
#include <pdscdef.h>

#define MD_FALLBACK_FRAME_STATE_FOR(CONTEXT, FS, SUCCESS)		\
 do {									\
  PDSCDEF *pv = *((PDSCDEF **) (CONTEXT)->reg [29]);                    \
									\
  if (pv && ((long) pv & 0x7) == 0) /* low bits 0 means address */      \
    pv = *(PDSCDEF **) pv;                                              \
									\
  if (pv && ((pv->pdsc$w_flags & 0xf) == PDSC$K_KIND_FP_STACK))		\
    {									\
      int i, j;								\
									\
      (FS)->cfa_offset = pv->pdsc$l_size;				\
      (FS)->cfa_reg = pv->pdsc$w_flags & PDSC$M_BASE_REG_IS_FP ? 29 : 30; \
      (FS)->retaddr_column = 26;					\
      (FS)->cfa_how = CFA_REG_OFFSET;					\
      (FS)->regs.reg[27].loc.offset = -pv->pdsc$l_size;			\
      (FS)->regs.reg[27].how = REG_SAVED_OFFSET;			\
      (FS)->regs.reg[26].loc.offset					\
	 = -(pv->pdsc$l_size - pv->pdsc$w_rsa_offset);			\
      (FS)->regs.reg[26].how = REG_SAVED_OFFSET;			\
									\
      for (i = 0, j = 0; i < 32; i++)					\
	if (1<<i & pv->pdsc$l_ireg_mask)				\
	  {								\
	    (FS)->regs.reg[i].loc.offset				\
	      = -(pv->pdsc$l_size - pv->pdsc$w_rsa_offset - 8 * ++j);	\
	    (FS)->regs.reg[i].how = REG_SAVED_OFFSET;			\
	  }								\
									\
      goto SUCCESS;							\
    }									\
  else if (pv && ((pv->pdsc$w_flags & 0xf) == PDSC$K_KIND_FP_REGISTER))	\
    {									\
      (FS)->cfa_offset = pv->pdsc$l_size;				\
      (FS)->cfa_reg = pv->pdsc$w_flags & PDSC$M_BASE_REG_IS_FP ? 29 : 30; \
      (FS)->retaddr_column = 26;					\
      (FS)->cfa_how = CFA_REG_OFFSET;					\
      (FS)->regs.reg[26].loc.reg = pv->pdsc$b_save_ra;			\
      (FS)->regs.reg[26].how = REG_SAVED_REG;			        \
      (FS)->regs.reg[29].loc.reg = pv->pdsc$b_save_fp;			\
      (FS)->regs.reg[29].how = REG_SAVED_REG;			        \
									\
      goto SUCCESS;							\
    }									\
} while (0)
#endif

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */

#undef ASM_OUTPUT_ALIGN
#define ASM_OUTPUT_ALIGN(FILE,LOG)	\
    fprintf (FILE, "\t.align %d\n", LOG);

/* Switch into a generic section.  */
#define TARGET_ASM_NAMED_SECTION vms_asm_named_section

#define ASM_OUTPUT_DEF(FILE,LABEL1,LABEL2)				\
  do {	literals_section();                                             \
	fprintf ((FILE), "\t");						\
	assemble_name (FILE, LABEL1);					\
	fprintf (FILE, " = ");						\
	assemble_name (FILE, LABEL2);					\
	fprintf (FILE, "\n");						\
  } while (0)

#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE VMS_AND_DWARF2_DEBUG

#undef ASM_FORMAT_PRIVATE_NAME
#define ASM_FORMAT_PRIVATE_NAME(OUTPUT, NAME, LABELNO)	\
( (OUTPUT) = (char *) alloca (strlen ((NAME)) + 12),	\
  sprintf ((OUTPUT), "%s___%d", (NAME), (LABELNO)))

/* ??? VMS uses different linkage.  */
#undef TARGET_ASM_OUTPUT_MI_THUNK

#undef ASM_SPEC
#undef ASM_FINAL_SPEC

/* The VMS convention is to always provide minimal debug info
   for a traceback unless specifically overridden.  Defaulting this here
   is a kludge.  */

#define OPTIMIZATION_OPTIONS(OPTIMIZE, OPTIMIZE_SIZE) \
{                                                  \
   write_symbols = VMS_DEBUG;                      \
   debug_info_level = (enum debug_info_level) 1;   \
}

/* Override traceback debug info on -g0.  */
#undef OVERRIDE_OPTIONS
#define OVERRIDE_OPTIONS                           \
{                                                  \
   if (write_symbols == NO_DEBUG)                  \
     debug_info_level = (enum debug_info_level) 0; \
   override_options ();                            \
}

/* Link with vms-dwarf2.o if -g (except -g0). This causes the
   VMS link to pull all the dwarf2 debug sections together. */
#undef LINK_SPEC
#define LINK_SPEC "%{g:-g vms-dwarf2.o%s} %{g0} %{g1:-g1 vms-dwarf2.o%s} \
%{g2:-g2 vms-dwarf2.o%s} %{g3:-g3 vms-dwarf2.o%s} %{shared} %{v} %{map}"

#undef STARTFILE_SPEC
#define STARTFILE_SPEC "%{!shared:%{mvms-return-codes:vcrt0.o%s} \
%{!mvms-return-codes:pcrt0.o%s}}"

#undef LIB_SPEC
#define LIB_SPEC "-lc"

/* Define the names of the division and modulus functions.  */
#define DIVSI3_LIBCALL "OTS$DIV_I"
#define DIVDI3_LIBCALL "OTS$DIV_L"
#define UDIVSI3_LIBCALL "OTS$DIV_UI"
#define UDIVDI3_LIBCALL "OTS$DIV_UL"
#define MODSI3_LIBCALL "OTS$REM_I"
#define MODDI3_LIBCALL "OTS$REM_L"
#define UMODSI3_LIBCALL "OTS$REM_UI"
#define UMODDI3_LIBCALL "OTS$REM_UL"

#define NAME__MAIN "__gccmain"
#define SYMBOL__MAIN __gccmain

#define MD_EXEC_PREFIX "/gnu/lib/gcc-lib/"
#define MD_STARTFILE_PREFIX "/gnu/lib/gcc-lib/"

/* Specify the list of include file directories.  */
#define INCLUDE_DEFAULTS		   \
{					   \
  { "/gnu/lib/gcc-lib/include", 0, 0, 0 }, \
  { "/gnu_gxx_include", 0, 1, 1 },	   \
  { "/gnu_cc_include", 0, 0, 0 },	   \
  { "/gnu/include", 0, 0, 0 },	           \
  { 0, 0, 0, 0 }			   \
}

#define LONGLONG_STANDALONE 1
