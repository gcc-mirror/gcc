/* Configuration for GNU C-compiler for m68k Amiga, running AmigaOS.
   Copyright (C) 1992, 1993, 1994, 1995, 1996, 1997, 1998, 2003
   Free Software Foundation, Inc.  
   Contributed by Markus M. Wild (wild@amiga.physik.unizh.ch).
   Heavily modified by Kamil Iskra (iskra@student.uci.agh.edu.pl).
   
 
This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#ifndef TARGET_AMIGAOS
#define TARGET_AMIGAOS 1
#endif

#if 0
/*  The function name __transfer_from_trampoline is not actually used.
   The function definition just permits use of asm with operands"
   (though the operand list is empty).  */
   
#undef TRANSFER_FROM_TRAMPOLINE				 

/* Call __flush_cache() after building the trampoline: it will call
   an appropriate OS cache-clearing routine.  */

#undef FINALIZE_TRAMPOLINE
#define FINALIZE_TRAMPOLINE(TRAMP)					\
  emit_library_call (gen_rtx_SYMBOL_REF (Pmode, "__flush_cache"),	\
		     0, VOIDmode, 2, (TRAMP), Pmode,			\
		     GEN_INT (TRAMPOLINE_SIZE), SImode)

#endif

/* Compile using the first 'm68k_regparm' data, address and float
   registers for arguments passing.  */      
/*#define SUBTARGET_OPTIONS     { "regparm=",		&m68k_regparm_string,				\
    N_("Use this register count to pass arguments"), 0},*/	


/* Nonzero if we need to generate special stack-allocating insns.
   On most systems they are not needed.
   When they are needed, also define ALTERNATE_ALLOCATE_STACK (see m68k.md)
   to perform the necessary actions.  */
//#undef TARGET_ALTERNATE_ALLOCATE_STACK
//#define TARGET_ALTERNATE_ALLOCATE_STACK 0  


/* Compile with stack extension.  */

#define MASK_STACKEXTEND 0x40000000 /* 1 << 30 */
#define TARGET_STACKEXTEND (((target_flags & MASK_STACKEXTEND)		\
  && !lookup_attribute ("interrupt",					\
			TYPE_ATTRIBUTES (TREE_TYPE (current_function_decl)))) \
  || lookup_attribute ("stackext",					\
		       TYPE_ATTRIBUTES (TREE_TYPE (current_function_decl))))

///* Compile with stack checking.  */
//
#define MASK_STACKCHECK 0x20000000 /* 1 << 29 */
#define TARGET_STACKCHECK ((target_flags & MASK_STACKCHECK)		\
  && !(target_flags & MASK_STACKEXTEND)					\
  && !lookup_attribute ("interrupt",					\
			TYPE_ATTRIBUTES (TREE_TYPE (current_function_decl))) \
  && !lookup_attribute ("stackext",					\
			TYPE_ATTRIBUTES (TREE_TYPE (current_function_decl))))

/* Compile with a4 restoring in public functions.  */

#define MASK_RESTORE_A4 0x10000000 /* 1 << 28 */
#define TARGET_RESTORE_A4						\
  ((target_flags & MASK_RESTORE_A4) && TREE_PUBLIC (current_function_decl))

/* Compile with a4 restoring in all functions.  */

#define MASK_ALWAYS_RESTORE_A4 0x8000000 /* 1 << 27 */
#define TARGET_ALWAYS_RESTORE_A4 (target_flags & MASK_ALWAYS_RESTORE_A4)

/* Provide a dummy entry for the '-msmall-code' switch.  This is used by
   the assembler and '*_SPEC'.  */

#undef SUBTARGET_SWITCHES
#define SUBTARGET_SWITCHES						\
    { "small-code", 0,							\
      "" /* Undocumented. */ },						\
    { "stackcheck", MASK_STACKCHECK,					\
      N_("Generate stack-check code") },				\
    { "no-stackcheck", - MASK_STACKCHECK,				\
      N_("Do not generate stack-check code") },				\
    { "stackextend", MASK_STACKEXTEND,					\
      N_("Generate stack-extension code") },				\
    { "no-stackextend", - MASK_STACKEXTEND,				\
      N_("Do not generate stack-extension code") },			\
    { "fixedstack", - (MASK_STACKCHECK|MASK_STACKEXTEND),		\
      N_("Do not generate stack-check/stack-extension code") },		\
    { "restore-a4", MASK_RESTORE_A4,					\
      N_("Restore a4 in public functions") },				\
    { "no-restore-a4", - MASK_RESTORE_A4,				\
      N_("Do not restore a4 in public functions") },			\
    { "always-restore-a4", MASK_ALWAYS_RESTORE_A4,			\
      N_("Restore a4 in all functions") },				\
    { "no-always-restore-a4", - MASK_ALWAYS_RESTORE_A4,			\
      N_("Do not restore a4 in all functions") }


/* Support sections in chip memory, currently '.datachip' only.  */
extern void
amiga_named_section (const char *name, unsigned int flags, tree decl);

#undef TARGET_ASM_NAMED_SECTION
#define TARGET_ASM_NAMED_SECTION amiga_named_section

/* Various ABI issues.  */

/* This is (almost;-) BSD, so it wants DBX format.  */
#undef DBX_DEBUGGING_INFO
#define DBX_DEBUGGING_INFO

/* GDB goes mad if it sees the function end marker.  */

#define NO_DBX_FUNCTION_END 1

/* Allow folding division by zero.  */

#define REAL_INFINITY

/* Don't try using XFmode since we don't have appropriate runtime software
   support.  */
#undef LONG_DOUBLE_TYPE_SIZE
#define LONG_DOUBLE_TYPE_SIZE 64

/* We use A4 for the PIC pointer, not A5, which is the framepointer.  */

#undef PIC_OFFSET_TABLE_REGNUM
#define PIC_OFFSET_TABLE_REGNUM (flag_pic ? 12 : INVALID_REGNUM)
 
/* Use A5 as framepointer instead of A6, since the AmigaOS ABI requires A6
   to be used as a shared library base pointer in direct library calls.  */

#undef FRAME_POINTER_REGNUM
#define FRAME_POINTER_REGNUM 13

#undef M68K_REGNAME
#define M68K_REGNAME(r) (reg_names[(r)])

/* The AmigaOS ABI does not define how structures should be returned, so,
   contrary to 'm68k.h', we prefer a multithread-safe solution.  */

#undef PCC_STATIC_STRUCT_RETURN

/* Setup a default shell return value for those (gazillion..) programs that
   (inspite of ANSI-C) declare main() to be void (or even VOID...) and thus
   cause the shell to randomly caugh upon executing such programs (contrary
   to Unix, AmigaOS scripts are terminated with an error if a program returns
   with an error code above the `error' or even `failure' level
   (which is configurable with the FAILAT command)).  */

//+2004-06-24  Ulrich Weigand  <uweigand@de.ibm.com>
//+
//+	* c-decl.c (finish_function): Do not check for DEFAULT_MAIN_RETURN.
//+	* system.h (DEFAULT_MAIN_RETURN): Poison.
//+	* doc/tm.texi (DEFAULT_MAIN_RETURN): Remove documentation.
//+

//poison VAR
//#define DEFAULT_MAIN_RETURN c_expand_return (integer_zero_node)

#undef WCHAR_TYPE
#define WCHAR_TYPE "unsigned short"

/* XXX: section support */
#if 0
/* We define TARGET_ASM_NAMED_SECTION, but we don't support arbitrary sections,
   including '.gcc_except_table', so we emulate the standard behaviour.  */
#undef TARGET_ASM_EXCEPTION_SECTION
#define TARGET_ASM_EXCEPTION_SECTION amiga_exception_section

#undef TARGET_ASM_EH_FRAME_SECTION
#define TARGET_ASM_EH_FRAME_SECTION amiga_eh_frame_section
#endif

/* Use sjlj exceptions because dwarf work only on elf targets */
#undef DWARF2_UNWIND_INFO
#define DWARF2_UNWIND_INFO	0


/* This is how to output an assembler line that says to advance the
   location counter to a multiple of 2**LOG bytes.  */

#ifndef ALIGN_ASM_OP
#define ALIGN_ASM_OP "\t.align\t"
#endif

/* GAS supports alignment up to 32768 bytes.  */
#undef ASM_OUTPUT_ALIGN
#define ASM_OUTPUT_ALIGN(FILE, LOG)					\
do									\
  {									\
    if ((LOG) == 1)							\
      fprintf ((FILE), "\t.even\n");					\
    else								\
      fprintf ((FILE), "\t.align %d\n", (LOG));				\
  }									\
while (0)

#if 0

/* Define this macro if references to a symbol must be treated
   differently depending on something about the variable or
   function named by the symbol (such as what section it is in).

   The macro definition, if any, is executed immediately after the
   rtl for DECL or other node is created.
   The value of the rtl will be a `mem' whose address is a
   `symbol_ref'.

   The usual thing for this macro to do is to a flag in the
   `symbol_ref' (such as `SYMBOL_REF_FLAG') or to store a modified
   name string in the `symbol_ref' (if one bit is not enough
   information).

   On the Amiga we use this to indicate if references to a symbol should be
   absolute or base relative.  */

#undef TARGET_ENCODE_SECTION_INFO
#define TARGET_ENCODE_SECTION_INFO amigaos_encode_section_info

#define LIBCALL_ENCODE_SECTION_INFO(FUN)				\
do									\
  {									\
    if (flag_pic >= 3)							\
      SYMBOL_REF_FLAG (FUN) = 1;					\
  }									\
while (0)

/* Select and switch to a section for EXP.  */

//#undef TARGET_ASM_SELECT_SECTION
//#define TARGET_ASM_SELECT_SECTION amigaos_select_section

/* Preserve A4 for baserel code if necessary.  */

#define EXTRA_SAVE_REG(REGNO)						\
do {									\
  if (flag_pic && flag_pic >= 3 && REGNO == PIC_OFFSET_TABLE_REGNUM	\
      && amigaos_restore_a4())						\
    return true;							\
} while (0)

/* Predicate for ALTERNATE_PIC_SETUP.  */

#define HAVE_ALTERNATE_PIC_SETUP (flag_pic >= 3)

/* Make a4 point at data hunk.  */

#define ALTERNATE_PIC_SETUP(STREAM)					\
  (amigaos_alternate_pic_setup (STREAM))

/* Attribute support.  */

/* Generate the test of d0 before return to set cc register in 'interrupt'
   function.  */

#define EPILOGUE_END_HOOK(STREAM)					\
do									\
  {									\
    if (lookup_attribute ("interrupt",					\
			  TYPE_ATTRIBUTES (TREE_TYPE (current_function_decl)))) \
      asm_fprintf ((STREAM), "\ttstl %Rd0\n");				\
  }									\
while (0)


/* Stack checking and automatic extension support.  */

#define PROLOGUE_BEGIN_HOOK(STREAM, FSIZE)				\
  (amigaos_prologue_begin_hook ((STREAM), (FSIZE)))

#define HAVE_ALTERNATE_FRAME_SETUP_F(FSIZE) TARGET_STACKEXTEND

#define ALTERNATE_FRAME_SETUP_F(STREAM, FSIZE)				\
  (amigaos_alternate_frame_setup_f ((STREAM), (FSIZE)))

#define HAVE_ALTERNATE_FRAME_SETUP(FSIZE) TARGET_STACKEXTEND

#define ALTERNATE_FRAME_SETUP(STREAM, FSIZE)				\
  (amigaos_alternate_frame_setup ((STREAM), (FSIZE)))

#define HAVE_ALTERNATE_FRAME_DESTR_F(FSIZE)				\
  (TARGET_STACKEXTEND && current_function_calls_alloca)

#define ALTERNATE_FRAME_DESTR_F(STREAM, FSIZE)				\
  (asm_fprintf ((STREAM), "\tjra %U__unlk_a5_rts\n"))

#define HAVE_ALTERNATE_RETURN						\
  (TARGET_STACKEXTEND && frame_pointer_needed &&			\
   current_function_calls_alloca)

#define ALTERNATE_RETURN(STREAM)

#if 0
#define HAVE_restore_stack_nonlocal TARGET_STACKEXTEND
#define gen_restore_stack_nonlocal gen_stack_cleanup_call

#define HAVE_restore_stack_function TARGET_STACKEXTEND
#define gen_restore_stack_function gen_stack_cleanup_call

#define HAVE_restore_stack_block TARGET_STACKEXTEND
#define gen_restore_stack_block gen_stack_cleanup_call

#undef TARGET_ALTERNATE_ALLOCATE_STACK
#define TARGET_ALTERNATE_ALLOCATE_STACK 1

#define ALTERNATE_ALLOCATE_STACK(OPERANDS)				\
do									\
  {									\
    amigaos_alternate_allocate_stack (OPERANDS);			\
    DONE;								\
  }									\
while (0)
#endif

/* begin-GG-local: dynamic libraries */

extern int amigaos_do_collecting (void);
extern void amigaos_gccopts_hook (const char *);
extern void amigaos_libname_hook (const char* arg);
extern void amigaos_collect2_cleanup (void);
extern void amigaos_prelink_hook (const char **, int *);
extern void amigaos_postlink_hook (const char *);

/* This macro is used to check if all collect2 facilities should be used.
   We need a few special ones, like stripping after linking.  */

#define DO_COLLECTING (do_collecting || amigaos_do_collecting())
#define COLLECT2_POSTLINK_HOOK(OUTPUT_FILE) amigaos_postlink_hook(OUTPUT_FILE) //new

/* This macro is called in collect2 for every GCC argument name.
   ARG is a part of commandline (without '\0' at the end).  */

#define COLLECT2_GCC_OPTIONS_HOOK(ARG) amigaos_gccopts_hook(ARG)

/* This macro is called in collect2 for every ld's "-l" or "*.o" or "*.a"
   argument.  ARG is a complete argument, with '\0' at the end.  */

#define COLLECT2_LIBNAME_HOOK(ARG) amigaos_libname_hook(ARG)

/* This macro is called at collect2 exit, to clean everything up.  */

#define COLLECT2_EXTRA_CLEANUP amigaos_collect2_cleanup

/* This macro is called just before the first linker invocation.
   LD1_ARGV is "char** argv", which will be passed to "ld".  STRIP is an
   *address* of "strip_flag" variable.  */

#define COLLECT2_PRELINK_HOOK(LD1_ARGV, STRIP) \
amigaos_prelink_hook((const char **)(LD1_ARGV), (STRIP))

/* This macro is called just after the first linker invocation, in place of
   "nm" and "ldd".  OUTPUT_FILE is the executable's filename.  */

#define COLLECT2_POSTLINK_HOOK(OUTPUT_FILE) amigaos_postlink_hook(OUTPUT_FILE)
/* end-GG-local */ 

#endif

/* begin-GG-local: explicit register specification for parameters */

/* Note: this is an extension of m68k_args */


#undef CLASS_MAX_NREGS
#define CLASS_MAX_NREGS(CLASS, MODE)	\
 ((CLASS) == FP_REGS ? GET_MODE_NUNITS (MODE) \
 : ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD))


/*
   On the m68k, this is a structure:
   num_of_regs: number of data, address and float registers to use for
     arguments passing (if it's 2, than pass arguments in d0, d1, a0, a1,
     fp0 and fp1). 0 - pass everything on stack. vararg calls are
     always passed entirely on stack.
   regs_already_used: bitmask of the already used registers.
   last_arg_reg - register number of the most recently passed argument.
     -1 if passed on stack.
   last_arg_len - number of registers used by the most recently passed
     argument.
*/

extern void amigaos_init_cumulative_args (CUMULATIVE_ARGS *cum, tree);
extern void amigaos_function_arg_advance (cumulative_args_t, machine_mode, const_tree, bool);
extern rtx amigaos_function_arg (cumulative_args_t, machine_mode, const_tree, bool);
extern cumulative_args_t amigaos_pack_cumulative_args (CUMULATIVE_ARGS *);
extern int amigaos_comp_type_attributes (const_tree, const_tree);
extern tree amigaos_handle_type_attribute(tree *, tree, tree, int, bool*);

/* Update the data in CUM to advance over an argument
   of mode MODE and data type TYPE.
   (TYPE is null for libcalls where that information may not be available.)  */

#undef TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE amigaos_function_arg_advance

/* A C expression that controls whether a function argument is passed
   in a register, and which register. */

#undef TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG amigaos_function_arg

#undef TARGET_PACK_CUMULATIVE_ARGS
#define TARGET_PACK_CUMULATIVE_ARGS(CUM) \
    (amigaos_pack_cumulative_args(&(CUM)))

#undef  TARGET_COMP_TYPE_ATTRIBUTES
#define TARGET_COMP_TYPE_ATTRIBUTES amigaos_comp_type_attributes


/* end-GG-local */

#undef SUBTARGET_OVERRIDE_OPTIONS
#define SUBTARGET_OVERRIDE_OPTIONS					\
do									\
  {									\
    if (!TARGET_68020 && flag_pic==4)					\
      error ("-fbaserel32 is not supported on the 68000 or 68010\n");	\
    if (amigaos_regparm > 0 && amigaos_regparm > AMIGAOS_MAX_REGPARM)   \
      error ("-mregparm=x with 1 <= x <= %d\n", AMIGAOS_MAX_REGPARM);   \
  }									\
while (0)

/* { name, min_len, max_len, decl_req, type_req, fn_type_req, handler,
     affects_type_identity } */
#define SUBTARGET_ATTRIBUTES                                            \
  { "regparm", 1, 1, true,  false, false, amigaos_handle_type_attribute,\
    false }, \
  { "stkparm", 0, 0, true,  false, false, amigaos_handle_type_attribute,\
    false },

#define GOT_SYMBOL_NAME ""

#undef TARGET_RTX_COSTS
#define TARGET_RTX_COSTS amigaos_rtx_costs
bool
amigaos_rtx_costs (rtx, machine_mode, int, int, int *, bool);

/* SBF: macro to test for const via pic_reg. */
#define CONST_PLUS_PIC_REG_CONST_UNSPEC_P(x) \
     (GET_CODE(x) == CONST \
    && GET_CODE(XEXP(x, 0)) == PLUS \
    && REG_P(XEXP(XEXP(x, 0), 0)) \
    && REGNO(XEXP(XEXP(x, 0), 0)) == PIC_REG \
    && GET_CODE(XEXP(XEXP(x, 0), 1)) == CONST \
    && GET_CODE(XEXP(XEXP(XEXP(x, 0), 1), 0)) == UNSPEC \
    )

