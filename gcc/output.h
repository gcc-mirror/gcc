/* Declarations for insn-output.c.  These functions are defined in recog.c,
   final.c, and varasm.c.
   Copyright (C) 1987, 1991, 1994, 1997, 1998,
   1999, 2000, 2001, 2002 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

/* Compute branch alignments based on frequency information in the CFG.  */
extern void compute_alignments  PARAMS ((void));

/* Initialize data in final at the beginning of a compilation.  */
extern void init_final		PARAMS ((const char *));

/* Called at end of source file,
   to output the block-profiling table for this entire compilation.  */
extern void end_final		PARAMS ((const char *));

/* Enable APP processing of subsequent output.
   Used before the output from an `asm' statement.  */
extern void app_enable		PARAMS ((void));

/* Disable APP processing of subsequent output.
   Called from varasm.c before most kinds of output.  */
extern void app_disable		PARAMS ((void));

/* Return the number of slots filled in the current
   delayed branch sequence (we don't count the insn needing the
   delay slot).   Zero if not in a delayed branch sequence.  */
extern int dbr_sequence_length	PARAMS ((void));

/* Indicate that branch shortening hasn't yet been done.  */
extern void init_insn_lengths	PARAMS ((void));

#ifdef RTX_CODE
/* Obtain the current length of an insn.  If branch shortening has been done,
   get its actual length.  Otherwise, get its maximum length.  */
extern int get_attr_length	PARAMS ((rtx));

/* Make a pass over all insns and compute their actual lengths by shortening
   any branches of variable length if possible.  */
extern void shorten_branches	PARAMS ((rtx));

/* Output assembler code for the start of a function,
   and initialize some of the variables in this file
   for the new function.  The label for the function and associated
   assembler pseudo-ops have already been output in
   `assemble_start_function'.  */
extern void final_start_function  PARAMS ((rtx, FILE *, int));

/* Output assembler code for the end of a function.
   For clarity, args are same as those of `final_start_function'
   even though not all of them are needed.  */
extern void final_end_function  PARAMS ((void));

/* Output assembler code for some insns: all or part of a function.  */
extern void final		PARAMS ((rtx, FILE *, int, int));

/* The final scan for one insn, INSN.  Args are same as in `final', except
   that INSN is the insn being scanned.  Value returned is the next insn to
   be scanned.  */
extern rtx final_scan_insn	PARAMS ((rtx, FILE *, int, int, int));

/* Replace a SUBREG with a REG or a MEM, based on the thing it is a
   subreg of.  */
extern rtx alter_subreg PARAMS ((rtx *));

/* Report inconsistency between the assembler template and the operands.
   In an `asm', it's the user's fault; otherwise, the compiler's fault.  */
extern void output_operand_lossage  PARAMS ((const char *, ...)) ATTRIBUTE_PRINTF_1;

/* Output a string of assembler code, substituting insn operands.
   Defined in final.c.  */
extern void output_asm_insn	PARAMS ((const char *, rtx *));

/* Compute a worst-case reference address of a branch so that it
   can be safely used in the presence of aligned labels.
   Defined in final.c.  */
extern int insn_current_reference_address	PARAMS ((rtx));

/* Find the alignment associated with a CODE_LABEL.
   Defined in final.c.  */
extern int label_to_alignment	PARAMS ((rtx));

/* Output a LABEL_REF, or a bare CODE_LABEL, as an assembler symbol.  */
extern void output_asm_label	PARAMS ((rtx));

/* Print a memory reference operand for address X
   using machine-dependent assembler syntax.  */
extern void output_address	PARAMS ((rtx));

/* Print an integer constant expression in assembler syntax.
   Addition and subtraction are the only arithmetic
   that may appear in these expressions.  */
extern void output_addr_const PARAMS ((FILE *, rtx));

/* Output a string of assembler code, substituting numbers, strings
   and fixed syntactic prefixes.  */
extern void asm_fprintf		PARAMS ((FILE *file, const char *p, ...));

/* Split up a CONST_DOUBLE or integer constant rtx into two rtx's for single
   words.  */
extern void split_double	PARAMS ((rtx, rtx *, rtx *));

/* Return nonzero if this function has no function calls.  */
extern int leaf_function_p	PARAMS ((void));

/* Return 1 if branch is an forward branch.
   Uses insn_shuid array, so it works only in the final pass.  May be used by
   output templates to add branch prediction hints, for example.  */
extern int final_forward_branch_p PARAMS ((rtx));

/* Return 1 if this function uses only the registers that can be
   safely renumbered.  */
extern int only_leaf_regs_used	PARAMS ((void));

/* Scan IN_RTX and its subexpressions, and renumber all regs into those
   available in leaf functions.  */
extern void leaf_renumber_regs_insn PARAMS ((rtx));

/* Locate the proper template for the given insn-code.  */
extern const char *get_insn_template PARAMS ((int, rtx));

/* Add function NAME to the weak symbols list.  VALUE is a weak alias
   associated with NAME.  */
extern int add_weak PARAMS ((const char *, const char *));

/* Functions in flow.c */
extern void allocate_for_life_analysis	PARAMS ((void));
extern int regno_uninitialized		PARAMS ((unsigned int));
extern int regno_clobbered_at_setjmp	PARAMS ((int));
extern void find_basic_blocks		PARAMS ((rtx, int, FILE *));
extern bool cleanup_cfg			PARAMS ((int));
extern void check_function_return_warnings PARAMS ((void));
#endif

/* Functions in varasm.c.  */

/* Tell assembler to switch to text section.  */
extern void text_section		PARAMS ((void));

/* Tell assembler to switch to data section.  */
extern void data_section		PARAMS ((void));

/* Tell assembler to make sure its in the data section.  */
extern void force_data_section		PARAMS ((void));

/* Tell assembler to switch to read-only data section.  This is normally
   the text section.  */
extern void readonly_data_section	PARAMS ((void));

/* Determine if we're in the text section.  */
extern int in_text_section		PARAMS ((void));

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

#ifdef EXPORTS_SECTION_ASM_OP
extern void exports_section PARAMS ((void));
#endif

#ifdef TDESC_SECTION_ASM_OP
extern void tdesc_section PARAMS ((void));
#endif

#ifdef DRECTVE_SECTION_ASM_OP
extern void drectve_section PARAMS ((void));
#endif

#ifdef SDATA_SECTION_ASM_OP
extern void sdata_section PARAMS ((void));
#endif

#ifdef RDATA_SECTION_ASM_OP
extern void rdata_section PARAMS ((void));
#endif

#ifdef TREE_CODE
/* Tell assembler to change to section NAME for DECL.
   If DECL is NULL, just switch to section NAME.
   If NAME is NULL, get the name from DECL.
   If RELOC is 1, the initializer for DECL contains relocs.  */
extern void named_section		PARAMS ((tree, const char *, int));

/* Tell assembler to switch to the section for function DECL.  */
extern void function_section		PARAMS ((tree));

/* Tell assembler to switch to the section for string merging.  */
extern void mergeable_string_section	PARAMS ((tree, unsigned HOST_WIDE_INT,
						 unsigned int));

/* Tell assembler to switch to the section for constant merging.  */
extern void mergeable_constant_section	PARAMS ((enum machine_mode,
						 unsigned HOST_WIDE_INT,
						 unsigned int));

/* Declare DECL to be a weak symbol.  */
extern void declare_weak		PARAMS ((tree));
/* Merge weak status.  */
extern void merge_weak			PARAMS ((tree, tree));
#endif /* TREE_CODE */

/* Emit any pending weak declarations.  */
extern void weak_finish			PARAMS ((void));

/* Decode an `asm' spec for a declaration as a register name.
   Return the register number, or -1 if nothing specified,
   or -2 if the ASMSPEC is not `cc' or `memory' and is not recognized,
   or -3 if ASMSPEC is `cc' and is not recognized,
   or -4 if ASMSPEC is `memory' and is not recognized.
   Accept an exact spelling or a decimal number.
   Prefixes such as % are optional.  */
extern int decode_reg_name		PARAMS ((const char *));

#ifdef TREE_CODE
/* Make the rtl for variable VAR be volatile.
   Use this only for static variables.  */
extern void make_var_volatile		PARAMS ((tree));

/* Output alignment directive to align for constant expression EXP.  */
extern void assemble_constant_align	PARAMS ((tree));

extern void assemble_alias		PARAMS ((tree, tree));

/* Output a string of literal assembler code
   for an `asm' keyword used between functions.  */
extern void assemble_asm		PARAMS ((tree));

/* Output assembler code for the constant pool of a function and associated
   with defining the name of the function.  DECL describes the function.
   NAME is the function's name.  For the constant pool, we use the current
   constant pool data.  */
extern void assemble_start_function	PARAMS ((tree, const char *));

/* Output assembler code associated with defining the size of the
   function.  DECL describes the function.  NAME is the function's name.  */
extern void assemble_end_function	PARAMS ((tree, const char *));

/* Assemble everything that is needed for a variable or function declaration.
   Not used for automatic variables, and not used for function definitions.
   Should not be called for variables of incomplete structure type.

   TOP_LEVEL is nonzero if this variable has file scope.
   AT_END is nonzero if this is the special handling, at end of compilation,
   to define things that have had only tentative definitions.
   DONT_OUTPUT_DATA if nonzero means don't actually output the
   initial value (that will be done by the caller).  */
extern void assemble_variable		PARAMS ((tree, int, int, int));

/* Output something to declare an external symbol to the assembler.
   (Most assemblers don't need this, so we normally output nothing.)
   Do nothing if DECL is not external.  */
extern void assemble_external		PARAMS ((tree));
#endif /* TREE_CODE */

/* Assemble code to leave SIZE bytes of zeros.  */
extern void assemble_zeros		PARAMS ((int));

/* Assemble an alignment pseudo op for an ALIGN-bit boundary.  */
extern void assemble_align		PARAMS ((int));
extern void assemble_eh_align		PARAMS ((int));

/* Assemble a string constant with the specified C string as contents.  */
extern void assemble_string		PARAMS ((const char *, int));

#ifdef RTX_CODE
/* Similar, for calling a library function FUN.  */
extern void assemble_external_libcall	PARAMS ((rtx));
#endif

/* Declare the label NAME global.  */
extern void assemble_global		PARAMS ((const char *));

/* Assemble a label named NAME.  */
extern void assemble_label		PARAMS ((const char *));
extern void assemble_eh_label		PARAMS ((const char *));

/* Output to FILE a reference to the assembler name of a C-level name NAME.
   If NAME starts with a *, the rest of NAME is output verbatim.
   Otherwise NAME is transformed in an implementation-defined way
   (usually by the addition of an underscore).
   Many macros in the tm file are defined to call this function.  */
extern void assemble_name		PARAMS ((FILE *, const char *));

/* Return the assembler directive for creating a given kind of integer
   object.  SIZE is the number of bytes in the object and ALIGNED_P
   indicates whether it is known to be aligned.  Return NULL if the
   assembly dialect has no such directive.

   The returned string should be printed at the start of a new line and
   be followed immediately by the object's initial value.  */
extern const char *integer_asm_op	PARAMS ((int, int));

#ifdef RTX_CODE
/* Use directive OP to assemble an integer object X.  Print OP at the
   start of the line, followed immediately by the value of X.  */
extern void assemble_integer_with_op	PARAMS ((const char *, rtx));

/* The default implementation of the asm_out.integer target hook.  */
extern bool default_assemble_integer	PARAMS ((rtx, unsigned int, int));

/* Assemble the integer constant X into an object of SIZE bytes.  ALIGN is
   the alignment of the integer in bits.  Return 1 if we were able to output
   the constant, otherwise 0.  If FORCE is non-zero, abort if we can't output
   the constant.  */
extern bool assemble_integer		PARAMS ((rtx, unsigned, unsigned, int));

/* An interface to assemble_integer for the common case in which a value is
   fully aligned and must be printed.  VALUE is the value of the integer
   object and SIZE is the number of bytes it contains.  */
#define assemble_aligned_integer(SIZE, VALUE) \
  assemble_integer (VALUE, SIZE, (SIZE) * BITS_PER_UNIT, 1)

#ifdef REAL_VALUE_TYPE
/* Assemble the floating-point constant D into an object of size MODE.  */
extern void assemble_real		PARAMS ((REAL_VALUE_TYPE,
					         enum machine_mode,
						 unsigned));
#endif
#endif

/* At the end of a function, forget the memory-constants
   previously made for CONST_DOUBLEs.  Mark them as not on real_constant_chain.
   Also clear out real_constant_chain and clear out all the chain-pointers.  */
extern void clear_const_double_mem	PARAMS ((void));

/* Start deferring output of subconstants.  */
extern void defer_addressed_constants	PARAMS ((void));

/* Stop deferring output of subconstants,
   and output now all those that have been deferred.  */
extern void output_deferred_addressed_constants PARAMS ((void));

/* Return the size of the constant pool.  */
extern int get_pool_size		PARAMS ((void));

#ifdef HAVE_peephole
extern rtx peephole			PARAMS ((rtx));
#endif

#ifdef TREE_CODE
/* Write all the constants in the constant pool.  */
extern void output_constant_pool	PARAMS ((const char *, tree));

/* Return nonzero if VALUE is a valid constant-valued expression
   for use in initializing a static variable; one that can be an
   element of a "constant" initializer.

   Return null_pointer_node if the value is absolute;
   if it is relocatable, return the variable that determines the relocation.
   We assume that VALUE has been folded as much as possible;
   therefore, we do not need to check for such things as
   arithmetic-combinations of integers.  */
extern tree initializer_constant_valid_p	PARAMS ((tree, tree));

/* Output assembler code for constant EXP to FILE, with no label.
   This includes the pseudo-op such as ".int" or ".byte", and a newline.
   Assumes output_addressed_constants has been done on EXP already.

   Generate exactly SIZE bytes of assembler data, padding at the end
   with zeros if necessary.  SIZE must always be specified.

   ALIGN is the alignment in bits that may be assumed for the data.  */
extern void output_constant		PARAMS ((tree, HOST_WIDE_INT,
						 unsigned int));
#endif

#ifdef RTX_CODE
/* When outputting delayed branch sequences, this rtx holds the
   sequence being output.  It is null when no delayed branch
   sequence is being output, so it can be used as a test in the
   insn output code.

   This variable is defined  in final.c.  */
extern rtx final_sequence;
#endif

/* The line number of the beginning of the current function.  Various
   md code needs this so that it can output relative linenumbers.  */

#ifdef SDB_DEBUGGING_INFO /* Avoid undef sym in certain broken linkers.  */
extern int sdb_begin_function_line;
#endif

/* File in which assembler code is being written.  */

#ifdef BUFSIZ
extern FILE *asm_out_file;
#endif

/* The first global object in the file.  */
extern const char *first_global_object_name;

/* The first weak object in the file.  */
extern const char *weak_global_object_name;

/* Nonzero if function being compiled doesn't contain any calls
   (ignoring the prologue and epilogue).  This is set prior to
   local register allocation and is valid for the remaining
   compiler passes.  */

extern int current_function_is_leaf;

/* Nonzero if function being compiled doesn't contain any instructions
   that can throw an exception.  This is set prior to final.  */

extern int current_function_nothrow;

/* Nonzero if function being compiled doesn't modify the stack pointer
   (ignoring the prologue and epilogue).  This is only valid after
   life_analysis has run.  */

extern int current_function_sp_is_unchanging;

/* Nonzero if the function being compiled is a leaf function which only
   uses leaf registers.  This is valid after reload (specifically after
   sched2) and is useful only if the port defines LEAF_REGISTERS.  */

extern int current_function_uses_only_leaf_regs;

/* Default file in which to dump debug output.  */

#ifdef BUFSIZ
extern FILE *rtl_dump_file;
#endif

/* Nonnull if the insn currently being emitted was a COND_EXEC pattern.  */
extern struct rtx_def *current_insn_predicate;

/* Last insn processed by final_scan_insn.  */
extern struct rtx_def *current_output_insn;

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

/* Default target function prologue and epilogue assembler output.  */
extern void default_function_pro_epilogue PARAMS ((FILE *, HOST_WIDE_INT));

/* Tell assembler to switch to the section for the exception table.  */
extern void default_exception_section	PARAMS ((void));

/* Tell assembler to switch to the section for the EH frames.  */
extern void default_eh_frame_section	PARAMS ((void));

/* Default target hook that outputs nothing to a stream.  */
extern void no_asm_to_stream PARAMS ((FILE *));

/* Flags controling properties of a section.  */
#define SECTION_ENTSIZE	 0x000ff	/* entity size in section */
#define SECTION_CODE	 0x00100	/* contains code */
#define SECTION_WRITE	 0x00200	/* data is writable */
#define SECTION_DEBUG	 0x00400	/* contains debug data */
#define SECTION_LINKONCE 0x00800	/* is linkonce */
#define SECTION_SMALL	 0x01000	/* contains "small data" */
#define SECTION_BSS	 0x02000	/* contains zeros only */
#define SECTION_FORGET	 0x04000	/* forget that we've entered the section */
#define SECTION_MERGE	 0x08000	/* contains mergeable data */
#define SECTION_STRINGS  0x10000	/* contains zero terminated strings without
					   embedded zeros */
#define SECTION_OVERRIDE 0x20000	/* allow override of default flags */
#define SECTION_MACH_DEP 0x40000	/* subsequent bits reserved for target */

extern unsigned int get_named_section_flags PARAMS ((const char *));
extern bool set_named_section_flags	PARAMS ((const char *, unsigned int));
extern void named_section_flags		PARAMS ((const char *, unsigned int));
extern bool named_section_first_declaration PARAMS((const char *));

union tree_node;
extern unsigned int default_section_type_flags PARAMS ((union tree_node *,
							const char *, int));

extern void default_no_named_section PARAMS ((const char *, unsigned int));
extern void default_elf_asm_named_section PARAMS ((const char *, unsigned int));
extern void default_coff_asm_named_section PARAMS ((const char *,
						    unsigned int));
extern void default_pe_asm_named_section PARAMS ((const char *, unsigned int));

extern void default_stabs_asm_out_destructor PARAMS ((struct rtx_def *, int));
extern void default_named_section_asm_out_destructor PARAMS ((struct rtx_def *,
							      int));
extern void default_dtor_section_asm_out_destructor PARAMS ((struct rtx_def *,
							     int));
extern void default_stabs_asm_out_constructor PARAMS ((struct rtx_def *, int));
extern void default_named_section_asm_out_constructor PARAMS ((struct rtx_def *,
							       int));
extern void default_ctor_section_asm_out_constructor PARAMS ((struct rtx_def *,
							      int));

/* Emit data for vtable gc for GNU binutils.  */
extern void assemble_vtable_entry PARAMS ((struct rtx_def *, HOST_WIDE_INT));
extern void assemble_vtable_inherit PARAMS ((struct rtx_def *,
					     struct rtx_def *));
