/* Output variables, constants and external declarations, for GNU compiler.
   Copyright (C) 1996, 1997, 1998 Free Software Foundation, Inc.

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

#define OPEN_VMS 1

/* This enables certain macros in alpha.h, which will make an indirect
   reference to an external symbol an invalid address.  This needs to be
   defined before we include alpha.h, since it determines which macros
   are used for GO_IF_*.  */

#define NO_EXTERNAL_INDIRECT_ADDRESS

#include "alpha/alpha.h"

#undef CPP_PREDEFINES
#define CPP_PREDEFINES \
"-D__ALPHA -Dvms -DVMS -D__vms__ -D__VMS__ -Asystem(vms)"

#undef CPP_SUBTARGET_SPEC
#define CPP_SUBTARGET_SPEC "\
%{mfloat-ieee:-D__IEEE_FLOAT} \
%{mfloat-vax:-D__G_FLOAT} \
%{!mfloat-vax:-D__IEEE_FLOAT}"

/* Under OSF4, -p and -pg require -lprof1, and -lprof1 requires -lpdf.  */

#define LIB_SPEC "%{p:-lprof1 -lpdf} %{pg:-lprof1 -lpdf} %{a:-lprof2} -lc"

/* Pass "-G 8" to ld because Alpha's CC does.  Pass -O3 if we are
   optimizing, -O1 if we are not.  Pass -shared, -non_shared or
   -call_shared as appropriate.  Also pass -pg.  */
#define LINK_SPEC  \
  "-G 8 %{O*:-O3} %{!O*:-O1} %{static:-non_shared} \
   %{!static:%{shared:-shared} %{!shared:-call_shared}} %{pg} %{taso} \
   %{rpath*}"

/* We allow $'s in identifiers unless -ansi is used .. */

#define DOLLARS_IN_IDENTIFIERS 2

/* These match the definitions used in DECCRTL, the VMS C run-time library

#define SIZE_TYPE	"unsigned int"
#define PTRDIFF_TYPE	"int"
*/

/* Use memcpy for structure copying, and so forth.  */
#define TARGET_MEM_FUNCTIONS

/* By default, allow $ to be part of an identifier.  */
#define DOLLARS_IN_IDENTIFIERS 2

#undef TARGET_DEFAULT
#define TARGET_DEFAULT (MASK_FP|MASK_FPREGS|MASK_GAS)
#undef TARGET_OPEN_VMS
#define TARGET_OPEN_VMS 1

#undef TARGET_NAME   
#define TARGET_NAME "OpenVMS/Alpha"
#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (%s)", TARGET_NAME);           

/* The structure return address arrives as an "argument" on VMS.  */
#undef STRUCT_VALUE_REGNUM
#define STRUCT_VALUE 0
#undef PCC_STATIC_STRUCT_RETURN

/* no floating emulation.  */
#undef REAL_ARITHMETIC

/* "long" is 32 bits.  */
#undef LONG_TYPE_SIZE
#define LONG_TYPE_SIZE 32

/* Pointer is 32 bits but the hardware has 64-bit addresses, sign extended. */
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

#undef HARD_FRAME_POINTER_REGNUM
#define HARD_FRAME_POINTER_REGNUM 29

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
typedef struct {char num_args; enum avms_arg_type atypes[6];} avms_arg_info;

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

/* Update the data in CUM to advance over an argument
   of mode MODE and data type TYPE.
   (TYPE is null for libcalls where that information may not be available.)  */

extern enum avms_arg_type alpha_arg_type ();

/* Determine where to put an argument to a function.
   Value is zero to push the argument on the stack,
   or a hard register in which to store the argument.

   MODE is the argument's machine mode (or VOIDmode for no more args).
   TYPE is the data type of the argument (as a tree).
    This is null for libcalls where that information may
    not be available.
   CUM is a variable of type CUMULATIVE_ARGS which gives info about
    the preceding args and about the function being called.
   NAMED is nonzero if this argument is a named parameter
    (otherwise it is an extra parameter matching an ellipsis).

   On Alpha the first 6 words of args are normally in registers
   and the rest are pushed.  */

extern struct rtx_def *alpha_arg_info_reg_val ();
#undef FUNCTION_ARG
#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED)	\
((MODE) == VOIDmode ? alpha_arg_info_reg_val (CUM)		\
 : ((CUM.num_args) < 6 && ! MUST_PASS_IN_STACK (MODE, TYPE)	\
    ? gen_rtx(REG, (MODE),					\
	      ((CUM).num_args + 16				\
	       + ((TARGET_FPREGS				\
		   && (GET_MODE_CLASS (MODE) == MODE_COMPLEX_FLOAT \
		       || GET_MODE_CLASS (MODE) == MODE_FLOAT)) \
		  * 32)))			\
    : 0))

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
   ap-relative addresses already made.   */

#undef SETUP_INCOMING_VARARGS
#define SETUP_INCOMING_VARARGS(CUM,MODE,TYPE,PRETEND_SIZE,NO_RTL)	\
{ if ((CUM).num_args < 6)				\
    {							\
      if (! (NO_RTL))					\
	{						\
	  emit_move_insn (gen_rtx (REG, DImode, 1),	\
			  virtual_incoming_args_rtx);	\
	  emit_insn (gen_arg_home ());			\
	}						\
						        \
      PRETEND_SIZE = 7 * UNITS_PER_WORD;		\
    }							\
}

#undef ASM_FILE_START
#define ASM_FILE_START(FILE)					\
{								\
  alpha_write_verstamp (FILE);					\
  fprintf (FILE, "\t.set noreorder\n");				\
  fprintf (FILE, "\t.set volatile\n");				\
  ASM_OUTPUT_SOURCE_FILENAME (FILE, main_input_filename);	\
}

#undef ASM_OUTPUT_FLOAT
#define ASM_OUTPUT_FLOAT(FILE,VALUE)					\
  {									\
    if (REAL_VALUE_ISINF (VALUE)					\
        || REAL_VALUE_ISNAN (VALUE)					\
	|| REAL_VALUE_MINUS_ZERO (VALUE))				\
      {									\
	long t;								\
	REAL_VALUE_TO_TARGET_SINGLE ((VALUE), t);			\
	fprintf (FILE, "\t.long 0x%lx\n", t & 0xffffffff);		\
      }									\
    else								\
      {									\
	char str[30];							\
	REAL_VALUE_TO_DECIMAL ((VALUE), "%.20e", str);			\
	fprintf (FILE, "\t.%c_floating %s\n", (TARGET_FLOAT_VAX)?'f':'s', str);	\
      }									\
  }

#define LINK_SECTION_ASM_OP ".link"
#define READONLY_SECTION_ASM_OP ".rdata"
#define LITERALS_SECTION_ASM_OP ".literals"
#define CTORS_SECTION_ASM_OP ".ctors"
#define DTORS_SECTION_ASM_OP ".dtors"

#undef EXTRA_SECTIONS
#define EXTRA_SECTIONS	in_link, in_rdata, in_literals, in_ctors, in_dtors

#undef EXTRA_SECTION_FUNCTIONS
#define EXTRA_SECTION_FUNCTIONS					\
void								\
readonly_section ()						\
{								\
  if (in_section != in_rdata)				\
    {								\
      fprintf (asm_out_file, "%s\n", READONLY_SECTION_ASM_OP);	\
      in_section = in_rdata;				\
    }								\
}								\
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
}								\
void								\
ctors_section ()						\
{								\
  if (in_section != in_ctors)					\
    {								\
      fprintf (asm_out_file, "%s\n", CTORS_SECTION_ASM_OP);	\
      in_section = in_ctors;					\
    }								\
}								\
void								\
dtors_section ()						\
{								\
  if (in_section != in_dtors)					\
    {								\
      fprintf (asm_out_file, "%s\n", DTORS_SECTION_ASM_OP);	\
      in_section = in_dtors;					\
    }								\
}

#undef ASM_OUTPUT_ADDR_DIFF_ELT
#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, BODY, VALUE, REL) abort ()

#undef ASM_OUTPUT_ADDR_VEC_ELT
#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE) \
  fprintf (FILE, "\t.quad $L%d\n", (VALUE))

#undef READONLY_DATA_SECTION
#define READONLY_DATA_SECTION readonly_section

#define ASM_FILE_END(FILE) alpha_write_linkage (FILE);

#undef CASE_VECTOR_MODE
#define CASE_VECTOR_MODE DImode
#undef CASE_VECTOR_PC_RELATIVE

#undef ASM_OUTPUT_CASE_LABEL
#define ASM_OUTPUT_CASE_LABEL(FILE,PREFIX,NUM,TABLEINSN)	\
{ ASM_OUTPUT_ALIGN (FILE, 3); ASM_OUTPUT_INTERNAL_LABEL (FILE, PREFIX, NUM); }

/* This says how to output assembler code to declare an                
   uninitialized external linkage data object.  */ 

#define COMMON_ASM_OP ".comm"

#undef ASM_OUTPUT_ALIGNED_COMMON
#define ASM_OUTPUT_ALIGNED_COMMON(FILE, NAME, SIZE, ALIGN)		\
do {									\
  fprintf ((FILE), "\t%s\t", COMMON_ASM_OP);				\
  assemble_name ((FILE), (NAME));					\
  fprintf ((FILE), ",%u,%u\n", (SIZE), (ALIGN) / BITS_PER_UNIT);	\
} while (0)

#define NO_MD_PROTOTYPES

/* Output assembler code for a block containing the constant parts
   of a trampoline, leaving space for the variable parts.

   The trampoline should set the static chain pointer to value placed
   into the trampoline and should branch to the specified routine.  
   Note that $27 has been set to the address of the trampoline, so we can
   use it for addressability of the two data items.  Trampolines are always
   aligned to FUNCTION_BOUNDARY, which is 64 bits.  */

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

/* Emit RTL insns to initialize the variable parts of a trampoline.
   FNADDR is an RTX for the address of the function's pure code.
   CXT is an RTX for the static chain value for the function.  */

#undef INITIALIZE_TRAMPOLINE
#define INITIALIZE_TRAMPOLINE(TRAMP, FNADDR, CXT) \
  alpha_initialize_trampoline (TRAMP, FNADDR, CXT, 16, 24, -1)

/* A C statement (sans semicolon) to output an element in the table of
   global constructors.  */
#define ASM_OUTPUT_CONSTRUCTOR(FILE,NAME)		\
  do {							\
    ctors_section ();					\
    fprintf (FILE, "\t.quad "); 			\
    assemble_name (FILE, NAME); 			\
    fprintf (FILE, "\n");				\
  } while (0)

/* A C statement (sans semicolon) to output an element in the table of
   global destructors.	*/
#define ASM_OUTPUT_DESTRUCTOR(FILE,NAME)		\
  do {							\
    dtors_section ();					\
    fprintf (FILE, "\t.quad "); 			\
    assemble_name (FILE, NAME); 			\
    fprintf (FILE, "\n");				\
  } while (0)

#define VALID_MACHINE_DECL_ATTRIBUTE(DECL, ATTRIBUTES, NAME, ARGS) \
  (vms_valid_decl_attribute_p (DECL, ATTRIBUTES, NAME, ARGS))
extern int vms_valid_decl_attribute_p ();

#undef SDB_DEBUGGING_INFO
#undef MIPS_DEBUGGING_INFO
#undef DBX_DEBUGGING_INFO

#define DWARF2_DEBUGGING_INFO

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */

#undef ASM_OUTPUT_ALIGN
#define ASM_OUTPUT_ALIGN(FILE,LOG)	\
    fprintf (FILE, "\t.align %d\n", LOG);

#define ASM_OUTPUT_SECTION(FILE,SECTION)			\
   (strcmp (SECTION, ".text") == 0)				\
     ? text_section ()						\
     : named_section (NULL_TREE, SECTION, 0),			\
       ASM_OUTPUT_ALIGN (FILE, 0)				\

#define ASM_OUTPUT_SECTION_NAME(FILE,DECL,NAME,RELOC)		\
  do								\
    {								\
      char *flags;					 	\
      int ovr = 0;						\
      if (DECL && DECL_MACHINE_ATTRIBUTES (DECL)		\
	  && lookup_attribute					\
	      ("overlaid", DECL_MACHINE_ATTRIBUTES (DECL)))	\
	flags = ",OVR", ovr = 1;				\
      else if (strncmp (NAME,".debug", 6) == 0)			\
	flags = ",NOWRT";					\
      else							\
	flags = "";						\
      fputc ('\n', (FILE));					\
      fprintf (FILE, ".section\t%s%s\n", NAME, flags);		\
      if (ovr)							\
        (NAME) = "";						\
    } while (0)

#define ASM_OUTPUT_DEF(FILE,LABEL1,LABEL2)				\
  do {	literals_section();                                             \
	fprintf ((FILE), "\t");						\
	assemble_name (FILE, LABEL1);					\
	fprintf (FILE, " = ");						\
	assemble_name (FILE, LABEL2);					\
	fprintf (FILE, "\n");						\
  } while (0)

#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG

#undef ASM_FORMAT_PRIVATE_NAME
#define ASM_FORMAT_PRIVATE_NAME(OUTPUT, NAME, LABELNO)	\
( (OUTPUT) = (char *) alloca (strlen ((NAME)) + 12),	\
  sprintf ((OUTPUT), "%s___%d", (NAME), (LABELNO)))

/* ??? VMS uses different linkage.  */
#undef ASM_OUTPUT_MI_THUNK

#undef ASM_SPEC
#undef ASM_FINAL_SPEC
#undef LINK_SPEC
#undef STARTFILE_SPEC
#define ASM_SPEC "-nocpp %{pg}"
#define LINK_SPEC "%{g3:-g3} %{g0:-g0} %{shared:-shared} %{v:-v}"

/* Define the names of the division and modulus functions.  */
#define DIVSI3_LIBCALL "OTS$DIV_I"
#define DIVDI3_LIBCALL "OTS$DIV_L"
#define UDIVSI3_LIBCALL "OTS$DIV_UI"
#define UDIVDI3_LIBCALL "OTS$DIV_UL"
#define MODSI3_LIBCALL "OTS$REM_I"
#define MODDI3_LIBCALL "OTS$REM_L"
#define UMODSI3_LIBCALL "OTS$REM_UI"
#define UMODDI3_LIBCALL "OTS$REM_UL"

#define DIR_SEPARATOR ']'

#define PREFIX "GNU_ROOT:"
