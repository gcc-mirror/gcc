/* Output variables, constants and external declarations, for GNU compiler.
   Copyright (C) 1996-2013 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* Alpha/VMS object format is not really Elf, but this makes compiling
   crtstuff.c and dealing with shared library initialization much easier.  */
#define OBJECT_FORMAT_ELF

/* Do not use TM clone registry as it currently doesn't work.  Alpha/VMS
   object is too far from ELF for supporting TM out of the box.  */
#define USE_TM_CLONE_REGISTRY 0

/* This enables certain macros in alpha.h, which will make an indirect
   reference to an external symbol an invalid address.  This needs to be
   defined before we include alpha.h, since it determines which macros
   are used for GO_IF_*.  */

#define NO_EXTERNAL_INDIRECT_ADDRESS

#define SUBTARGET_OS_CPP_BUILTINS()		\
    do {					\
      builtin_define ("__ALPHA");		\
      if (TARGET_FLOAT_VAX)			\
        builtin_define ("__G_FLOAT");		\
      else					\
        builtin_define ("__IEEE_FLOAT");	\
    } while (0)

#undef PCC_STATIC_STRUCT_RETURN

#define MAX_OFILE_ALIGNMENT 524288  /* 8 x 2^16 by DEC Ada Test CD40VRA */

/* The maximum alignment 'malloc' honors.  */
#undef  MALLOC_ABI_ALIGNMENT
#define MALLOC_ABI_ALIGNMENT \
  ((flag_vms_malloc64 && flag_vms_pointer_size != VMS_POINTER_SIZE_NONE \
   ? 16 : 8) * BITS_PER_UNIT)

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

#undef INITIAL_ELIMINATION_OFFSET
#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET)			\
  ((OFFSET) = alpha_vms_initial_elimination_offset(FROM, TO))


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
#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, INDIRECT, N_NAMED_ARGS) \
  (CUM).num_args = 0;						\
  (CUM).atypes[0] = (CUM).atypes[1] = (CUM).atypes[2] = I64;	\
  (CUM).atypes[3] = (CUM).atypes[4] = (CUM).atypes[5] = I64;

#define DEFAULT_PCC_STRUCT_RETURN 0

/* Even though pointers are 64bits, only 32bit ever remain significant in code
   addresses.  */
#define MASK_RETURN_ADDR                                \
  (flag_vms_pointer_size == VMS_POINTER_SIZE_NONE       \
   ? constm1_rtx                                        \
   : GEN_INT (0xffffffff))

#undef  ASM_WEAKEN_LABEL
#define ASM_WEAKEN_LABEL(FILE, NAME)                            \
   do { fputs ("\t.weak\t", FILE); assemble_name (FILE, NAME);  \
        fputc ('\n', FILE); } while (0)

#define READONLY_DATA_SECTION_ASM_OP "\t.rdata"
#define CTORS_SECTION_ASM_OP "\t.ctors"
#define DTORS_SECTION_ASM_OP "\t.dtors"
#define SDATA_SECTION_ASM_OP "\t.sdata"
#define CRT_CALL_STATIC_FUNCTION(SECTION_OP, FUNC)              \
   asm (SECTION_OP "\n\t.long " #FUNC"\n");

#undef ASM_OUTPUT_ADDR_DIFF_ELT

#undef ASM_OUTPUT_ADDR_VEC_ELT
#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE) \
  fprintf (FILE, "\t.quad $L%d\n", (VALUE))

#undef CASE_VECTOR_MODE
#define CASE_VECTOR_MODE DImode
#undef CASE_VECTOR_PC_RELATIVE

#undef ASM_OUTPUT_CASE_LABEL
#define ASM_OUTPUT_CASE_LABEL(FILE,PREFIX,NUM,TABLEINSN)	\
{ ASM_OUTPUT_ALIGN (FILE, 3); (*targetm.asm_out.internal_label) (FILE, PREFIX, NUM); }

/* This says how to output assembler code to declare an                
   uninitialized external linkage data object.  */ 

#define COMMON_ASM_OP "\t.comm\t"

#undef ASM_OUTPUT_ALIGNED_DECL_COMMON
#define ASM_OUTPUT_ALIGNED_DECL_COMMON(FILE, DECL, NAME, SIZE, ALIGN) \
  vms_output_aligned_decl_common (FILE, DECL, NAME, SIZE, ALIGN)

/* Control how constructors and destructors are emitted.  */
#define TARGET_ASM_CONSTRUCTOR  vms_asm_out_constructor
#define TARGET_ASM_DESTRUCTOR   vms_asm_out_destructor

#define DWARF2_DEBUGGING_INFO 1
#define VMS_DEBUGGING_INFO 1

#define DWARF2_UNWIND_INFO 1

#undef EH_RETURN_HANDLER_RTX
#define EH_RETURN_HANDLER_RTX \
  gen_rtx_MEM (Pmode, plus_constant (Pmode, stack_pointer_rtx, 8))

#define LINK_EH_SPEC "vms-dwarf2eh.o%s "
#define LINK_GCC_C_SEQUENCE_SPEC "%G"

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */

#define ASM_OUTPUT_ALIGN(FILE,LOG)	\
    fprintf (FILE, "\t.align %d\n", LOG);

/* This is how to advance the location counter by SIZE bytes.  */

#define ASM_OUTPUT_SKIP(FILE,SIZE)  \
  fprintf (FILE, "\t.space "HOST_WIDE_INT_PRINT_UNSIGNED"\n", (SIZE))

/* This says how to output an assembler line
   to define a global common symbol.  */

#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE,ROUNDED)	\
( fputs ("\t.lcomm ", (FILE)),				\
  assemble_name ((FILE), (NAME)),			\
  fprintf ((FILE), ","HOST_WIDE_INT_PRINT_UNSIGNED"\n", (SIZE)))

/* Switch into a generic section.  */
#define TARGET_ASM_NAMED_SECTION vms_asm_named_section

#define ASM_OUTPUT_DEF(FILE,LABEL1,LABEL2)      \
  do                                            \
    {                                           \
      fprintf ((FILE), "\t");                   \
      assemble_name (FILE, LABEL1);             \
      fprintf (FILE, " = ");                    \
      assemble_name (FILE, LABEL2);             \
      fprintf (FILE, "\n");                     \
    }                                           \
 while (0)

#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE VMS_AND_DWARF2_DEBUG

#define ASM_PN_FORMAT "%s___%lu"

/* ??? VMS uses different linkage.  */
#undef TARGET_ASM_OUTPUT_MI_THUNK

#undef ASM_SPEC
#undef ASM_FINAL_SPEC

/* The VMS convention is to always provide minimal debug info
   for a traceback unless specifically overridden.

   Because ASM_OUTPUT_ADDR_DIFF_ELT is not defined for alpha-vms,
   jump tables cannot be output for PIC code, because you can't put
   an absolute address in a readonly section.  Putting the table in
   a writable section is a security hole.  Therefore, we unset the
   flag_jump_tables flag, forcing switch statements to be expanded
   using decision trees.  There are probably other ways to address
   this issue, but using a decision tree is clearly safe.  */

#undef SUBTARGET_OVERRIDE_OPTIONS
#define SUBTARGET_OVERRIDE_OPTIONS                  \
do {                                                \
  if (write_symbols == NO_DEBUG                     \
      && debug_info_level == DINFO_LEVEL_NONE)      \
    {                                               \
      write_symbols = VMS_DEBUG;                    \
      debug_info_level = DINFO_LEVEL_TERSE;         \
    }                                               \
  if (flag_pic)                                     \
    flag_jump_tables = 0;                           \
} while (0)

#undef LINK_SPEC
#if HAVE_GNU_LD
/* GNU-ld built-in linker script already handles the dwarf2 debug sections.  */
#define LINK_SPEC "%{shared} %{v}"
#else
/* Link with vms-dwarf2.o if -g (except -g0). This causes the
   VMS link to pull all the dwarf2 debug sections together.  */
#define LINK_SPEC "%{g0} %{g*:-g vms-dwarf2.o%s} %{shared} %{v} %{map}"
#endif

#undef STARTFILE_SPEC
#define STARTFILE_SPEC "%{!shared:crt0.o%s crtbegin.o%s} \
 %{!static:%{shared:crtbeginS.o%s}}"

#define ENDFILE_SPEC "%{!shared:crtend.o%s} %{!static:%{shared:crtendS.o%s}}"

#define INIT_SECTION_ASM_OP "\t.section LIB$INITIALIZE,GBL,NOWRT"

#define LONGLONG_STANDALONE 1

#undef TARGET_VALID_POINTER_MODE
#define TARGET_VALID_POINTER_MODE vms_valid_pointer_mode

/* Default values for _CRTL_VER and _VMS_VER.  */
#define VMS_DEFAULT_CRTL_VER 70320000
#define VMS_DEFAULT_VMS_VER 70320000
