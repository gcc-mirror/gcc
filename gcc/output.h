/* Declarations for insn-output.c.  These functions are defined in recog.c,
   final.c, and varasm.c.
   Copyright (C) 1987, 1991, 1994, 97-99, 2000 Free Software Foundation, Inc.

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

/* Initialize data in final at the beginning of a compilation.  */
extern void init_final		PROTO((const char *));

/* Called at end of source file,
   to output the block-profiling table for this entire compilation.  */
extern void end_final		PROTO((const char *));

/* Enable APP processing of subsequent output.
   Used before the output from an `asm' statement.  */
extern void app_enable		PROTO((void));

/* Disable APP processing of subsequent output.
   Called from varasm.c before most kinds of output.  */
extern void app_disable		PROTO((void));

/* Return the number of slots filled in the current 
   delayed branch sequence (we don't count the insn needing the
   delay slot).   Zero if not in a delayed branch sequence.  */
extern int dbr_sequence_length	PROTO((void));

/* Indicate that branch shortening hasn't yet been done.  */
extern void init_insn_lengths	PROTO((void));

#ifdef RTX_CODE
/* Obtain the current length of an insn.  If branch shortening has been done,
   get its actual length.  Otherwise, get its maximum length.  */
extern int get_attr_length	PROTO((rtx));

/* Make a pass over all insns and compute their actual lengths by shortening
   any branches of variable length if possible.  */
extern void shorten_branches	PROTO((rtx));

/* Output assembler code for the start of a function,
   and initialize some of the variables in this file
   for the new function.  The label for the function and associated
   assembler pseudo-ops have already been output in
   `assemble_start_function'.  */
extern void final_start_function  PROTO((rtx, FILE *, int));

/* Output assembler code for the end of a function.
   For clarity, args are same as those of `final_start_function'
   even though not all of them are needed.  */
extern void final_end_function  PROTO((rtx, FILE *, int));

/* Output assembler code for some insns: all or part of a function.  */
extern void final		PROTO((rtx, FILE *, int, int));

/* The final scan for one insn, INSN.  Args are same as in `final', except
   that INSN is the insn being scanned.  Value returned is the next insn to
   be scanned.  */
extern rtx final_scan_insn	PROTO((rtx, FILE *, int, int, int));

/* Replace a SUBREG with a REG or a MEM, based on the thing it is a
   subreg of.  */
extern rtx alter_subreg PROTO((rtx));

/* Report inconsistency between the assembler template and the operands.
   In an `asm', it's the user's fault; otherwise, the compiler's fault.  */
extern void output_operand_lossage  PROTO((const char *));

/* Output a string of assembler code, substituting insn operands.
   Defined in final.c.  */
extern void output_asm_insn	PROTO((const char *, rtx *));

/* Compute a worst-case reference address of a branch so that it
   can be safely used in the presence of aligned labels.
   Defined in final.c.  */
extern int insn_current_reference_address	PROTO((rtx));

/* Find the alignment associated with a CODE_LABEL.
   Defined in final.c.  */
extern int label_to_alignment	PROTO((rtx));

/* Output a LABEL_REF, or a bare CODE_LABEL, as an assembler symbol.  */
extern void output_asm_label	PROTO((rtx));

/* Print a memory reference operand for address X
   using machine-dependent assembler syntax.  */
extern void output_address	PROTO((rtx));

/* Print an integer constant expression in assembler syntax.
   Addition and subtraction are the only arithmetic
   that may appear in these expressions.  */
extern void output_addr_const PROTO((FILE *, rtx));

/* Output a string of assembler code, substituting numbers, strings
   and fixed syntactic prefixes.  */
extern void asm_fprintf		PVPROTO((FILE *file, const char *p, ...));

/* Split up a CONST_DOUBLE or integer constant rtx into two rtx's for single
   words.  */
extern void split_double	PROTO((rtx, rtx *, rtx *));

/* Return nonzero if this function has no function calls.  */
extern int leaf_function_p	PROTO((void));

/* Return 1 if this function uses only the registers that can be
   safely renumbered.  */
extern int only_leaf_regs_used	PROTO((void));

/* Scan IN_RTX and its subexpressions, and renumber all regs into those
   available in leaf functions.  */
extern void leaf_renumber_regs_insn PROTO((rtx));

/* Locate the proper template for the given insn-code.  */
extern const char *get_insn_template PROTO((int, rtx));

/* Functions in flow.c */
extern void allocate_for_life_analysis	PROTO((void));
extern int regno_uninitialized		PROTO((int));
extern int regno_clobbered_at_setjmp	PROTO((int));
extern void dump_flow_info		PROTO((FILE *));
extern void find_basic_blocks         PROTO((rtx, int, FILE *, int));
extern void calculate_loop_depth      PROTO((FILE *));
extern void free_basic_block_vars     PROTO((int));
extern void set_block_num             PROTO((rtx, int));
extern void life_analysis             PROTO((rtx, int, FILE *, int));
#endif

/* Functions in varasm.c.  */

/* Tell assembler to switch to text section.  */
extern void text_section		PROTO((void));

/* Tell assembler to switch to data section.  */
extern void data_section		PROTO((void));

/* Tell assembler to make sure its in the data section.  */
extern void force_data_section		PROTO((void));

/* Tell assembler to switch to read-only data section.  This is normally
   the text section.  */
extern void readonly_data_section	PROTO((void));

/* Determine if we're in the text section. */
extern int in_text_section		PROTO((void));

#ifdef EH_FRAME_SECTION_ASM_OP
extern void eh_frame_section		PROTO ((void));
#endif

#ifdef CTORS_SECTION_ASM_OP
extern void ctors_section PARAMS ((void));
#endif

#ifdef DTORS_SECTION_ASM_OP
extern void dtors_section PARAMS ((void));
#endif

#ifdef BSS_SECTION_ASM_OP
extern void bss_section PARAMS ((void));
#endif

#ifdef CONST_SECTION_ASM_OP
extern void const_section PARAMS ((void));
#endif

#ifdef INIT_SECTION_ASM_OP
extern void init_section PARAMS ((void));
#endif

#ifdef FINI_SECTION_ASM_OP
extern void fini_section PARAMS ((void));
#endif

#ifdef TDESC_SECTION_ASM_OP
extern void tdesc_section PARAMS ((void));
#endif

#ifdef TREE_CODE
/* Tell assembler to change to section NAME for DECL.
   If DECL is NULL, just switch to section NAME.
   If NAME is NULL, get the name from DECL.
   If RELOC is 1, the initializer for DECL contains relocs.  */
extern void named_section		PROTO((tree, const char *, int));

/* Tell assembler to switch to the section for function DECL.  */
extern void function_section		PROTO((tree));

/* Tell assembler to switch to the section for the exception table.  */
extern void exception_section		PROTO((void));

/* Create the rtl to represent a function, for a function definition.
   DECL is a FUNCTION_DECL node which describes which function.
   The rtl is stored into DECL.  */
extern void make_function_rtl		PROTO((tree));

/* Declare DECL to be a weak symbol.  */
extern void declare_weak		PROTO ((tree));
#endif /* TREE_CODE */

/* Emit any pending weak declarations.  */
extern void weak_finish			PROTO ((void));

/* Decode an `asm' spec for a declaration as a register name.
   Return the register number, or -1 if nothing specified,
   or -2 if the ASMSPEC is not `cc' or `memory' and is not recognized,
   or -3 if ASMSPEC is `cc' and is not recognized,
   or -4 if ASMSPEC is `memory' and is not recognized.
   Accept an exact spelling or a decimal number.
   Prefixes such as % are optional.  */
extern int decode_reg_name		PROTO((const char *));

#ifdef TREE_CODE
/* Create the DECL_RTL for a declaration for a static or external variable
   or static or external function.
   ASMSPEC, if not 0, is the string which the user specified
   as the assembler symbol name.
   TOP_LEVEL is nonzero if this is a file-scope variable.

   This is never called for PARM_DECL nodes.  */
extern void make_decl_rtl		PROTO((tree, const char *, int));

/* Make the rtl for variable VAR be volatile.
   Use this only for static variables.  */
extern void make_var_volatile		PROTO((tree));

/* Output alignment directive to align for constant expression EXP.  */
extern void assemble_constant_align	PROTO((tree));

extern void assemble_alias		PROTO((tree, tree));

/* Output a string of literal assembler code
   for an `asm' keyword used between functions.  */
extern void assemble_asm		PROTO((tree));

/* Output assembler code for the constant pool of a function and associated
   with defining the name of the function.  DECL describes the function.
   NAME is the function's name.  For the constant pool, we use the current
   constant pool data.  */
extern void assemble_start_function	PROTO((tree, char *));

/* Output assembler code associated with defining the size of the
   function.  DECL describes the function.  NAME is the function's name.  */
extern void assemble_end_function	PROTO((tree, const char *));

/* Assemble everything that is needed for a variable or function declaration.
   Not used for automatic variables, and not used for function definitions.
   Should not be called for variables of incomplete structure type.

   TOP_LEVEL is nonzero if this variable has file scope.
   AT_END is nonzero if this is the special handling, at end of compilation,
   to define things that have had only tentative definitions.
   DONT_OUTPUT_DATA if nonzero means don't actually output the
   initial value (that will be done by the caller).  */
extern void assemble_variable		PROTO((tree, int, int, int));

/* Output something to declare an external symbol to the assembler.
   (Most assemblers don't need this, so we normally output nothing.)
   Do nothing if DECL is not external.  */
extern void assemble_external		PROTO((tree));
#endif /* TREE_CODE */

/* Record an element in the table of global destructors.
   How this is done depends on what sort of assembler and linker
   are in use.

   NAME should be the name of a global function to be called
   at exit time.  This name is output using assemble_name.  */
extern void assemble_destructor		PROTO((const char *));

/* Likewise for global constructors.  */
extern void assemble_constructor	PROTO((const char *));

/* Likewise for entries we want to record for garbage collection.
   Garbage collection is still under development.  */
extern void assemble_gc_entry		PROTO((const char *));

/* Assemble code to leave SIZE bytes of zeros.  */
extern void assemble_zeros		PROTO((int));

/* Assemble an alignment pseudo op for an ALIGN-bit boundary.  */
extern void assemble_align		PROTO((int));

/* Assemble a string constant with the specified C string as contents.  */
extern void assemble_string		PROTO((const char *, int));

#ifdef RTX_CODE
/* Similar, for calling a library function FUN.  */
extern void assemble_external_libcall	PROTO((rtx));
#endif

/* Declare the label NAME global.  */
extern void assemble_global		PROTO((const char *));

/* Assemble a label named NAME.  */
extern void assemble_label		PROTO((const char *));

/* Output to FILE a reference to the assembler name of a C-level name NAME.
   If NAME starts with a *, the rest of NAME is output verbatim.
   Otherwise NAME is transformed in an implementation-defined way
   (usually by the addition of an underscore).
   Many macros in the tm file are defined to call this function.  */
extern void assemble_name		PROTO((FILE *, const char *));

#ifdef RTX_CODE
/* Assemble the integer constant X into an object of SIZE bytes.
   X must be either a CONST_INT or CONST_DOUBLE.

   Return 1 if we were able to output the constant, otherwise 0.  If FORCE is
   non-zero, abort if we can't output the constant.  */
extern int assemble_integer		PROTO((rtx, int, int));

#ifdef EMUSHORT
/* Assemble the floating-point constant D into an object of size MODE.  */
extern void assemble_real		PROTO((REAL_VALUE_TYPE,
					       enum machine_mode));
#endif
#endif

/* At the end of a function, forget the memory-constants
   previously made for CONST_DOUBLEs.  Mark them as not on real_constant_chain.
   Also clear out real_constant_chain and clear out all the chain-pointers.  */
extern void clear_const_double_mem	PROTO((void));

/* Start deferring output of subconstants.  */
extern void defer_addressed_constants	PROTO((void));

/* Stop deferring output of subconstants,
   and output now all those that have been deferred.  */
extern void output_deferred_addressed_constants PROTO((void));

/* Return the size of the constant pool.  */
extern int get_pool_size		PROTO((void));

#ifdef HAVE_peephole
extern rtx peephole			PROTO((rtx));
#endif

#ifdef TREE_CODE
/* Write all the constants in the constant pool.  */
extern void output_constant_pool	PROTO((const char *, tree));

/* Return nonzero if VALUE is a valid constant-valued expression
   for use in initializing a static variable; one that can be an
   element of a "constant" initializer.

   Return null_pointer_node if the value is absolute;
   if it is relocatable, return the variable that determines the relocation.
   We assume that VALUE has been folded as much as possible;
   therefore, we do not need to check for such things as
   arithmetic-combinations of integers.  */
extern tree initializer_constant_valid_p	PROTO((tree, tree));

/* Output assembler code for constant EXP to FILE, with no label.
   This includes the pseudo-op such as ".int" or ".byte", and a newline.
   Assumes output_addressed_constants has been done on EXP already.

   Generate exactly SIZE bytes of assembler data, padding at the end
   with zeros if necessary.  SIZE must always be specified.  */
extern void output_constant		PROTO((tree, int));
#endif

/* When outputting assembler code, indicates which alternative
   of the constraints was actually satisfied.  */
extern int which_alternative;

#ifdef RTX_CODE
/* When outputting delayed branch sequences, this rtx holds the
   sequence being output.  It is null when no delayed branch
   sequence is being output, so it can be used as a test in the
   insn output code.

   This variable is defined  in final.c.  */
extern rtx final_sequence;
#endif

/* Nonzero means generate position-independent code.
   This is not fully implemented yet.  */

extern int flag_pic;

/* The line number of the beginning of the current function.
   sdbout.c needs this so that it can output relative linenumbers.  */

#ifdef SDB_DEBUGGING_INFO /* Avoid undef sym in certain broken linkers.  */
extern int sdb_begin_function_line;
#endif

/* File in which assembler code is being written.  */

#ifdef BUFSIZ
extern FILE *asm_out_file;
#endif
/* Nonzero if function being compiled doesn't contain any calls
   (ignoring the prologue and epilogue).  This is set prior to
   local register allocation and is valid for the remaining
   compiler passes. */

extern int current_function_is_leaf;

/* Nonzero if function being compiled doesn't modify the stack pointer
   (ignoring the prologue and epilogue).  This is only valid after
   life_analysis has run. */

extern int current_function_sp_is_unchanging;

/* Nonzero if the function being compiled is a leaf function which only
   uses leaf registers.  This is valid after reload (specifically after
   sched2) and is useful only if the port defines LEAF_REGISTERS.  */

extern int current_function_uses_only_leaf_regs;

/* Default file in which to dump debug output.  */

#ifdef BUFSIZ
extern FILE *rtl_dump_file;
#endif

/* Decide whether DECL needs to be in a writable section.  RELOC is the same
   as for SELECT_SECTION.  */

#define DECL_READONLY_SECTION(DECL,RELOC)		\
  (TREE_READONLY (DECL)					\
   && ! TREE_THIS_VOLATILE (DECL)			\
   && DECL_INITIAL (DECL)				\
   && (DECL_INITIAL (DECL) == error_mark_node		\
       || TREE_CONSTANT (DECL_INITIAL (DECL)))		\
   && ! (RELOC && (flag_pic || DECL_ONE_ONLY (DECL))))

/* User label prefix in effect for this compilation.  */
extern const char *user_label_prefix;

/* This macro gets just the user-specified name
   out of the string in a SYMBOL_REF.  On most machines,
   we discard the * if any and that's all.  */
#ifndef STRIP_NAME_ENCODING
#define STRIP_NAME_ENCODING(VAR,SYMBOL_NAME) \
  (VAR) = ((SYMBOL_NAME) + ((SYMBOL_NAME)[0] == '*'))
#endif
