/* Definitions for code generation pass of GNU compiler.
   Copyright (C) 1987, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2001, 2002, 2003 Free Software Foundation, Inc.

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

/* The default branch cost is 1.  */
#ifndef BRANCH_COST
#define BRANCH_COST 1
#endif

/* Macros to access the slots of a QUEUED rtx.
   Here rather than in rtl.h because only the expansion pass
   should ever encounter a QUEUED.  */

/* The variable for which an increment is queued.  */
#define QUEUED_VAR(P) XEXP (P, 0)
/* If the increment has been emitted, this is the insn
   that does the increment.  It is zero before the increment is emitted.
   If more than one insn is emitted, this is the first insn.  */
#define QUEUED_INSN(P) XEXP (P, 1)
/* If a pre-increment copy has been generated, this is the copy
   (it is a temporary reg).  Zero if no copy made yet.  */
#define QUEUED_COPY(P) XEXP (P, 2)
/* This is the body to use for the insn to do the increment.
   It is used to emit the increment.  */
#define QUEUED_BODY(P) XEXP (P, 3)
/* Next QUEUED in the queue.  */
#define QUEUED_NEXT(P) XEXP (P, 4)

/* This is the 4th arg to `expand_expr'.
   EXPAND_STACK_PARM means we are possibly expanding a call param onto
   the stack.  Choosing a value of 2 isn't special;  It just allows
   some code optimization in store_expr.
   EXPAND_SUM means it is ok to return a PLUS rtx or MULT rtx.
   EXPAND_INITIALIZER is similar but also record any labels on forced_labels.
   EXPAND_CONST_ADDRESS means it is ok to return a MEM whose address
    is a constant that is not a legitimate address.
   EXPAND_WRITE means we are only going to write to the resulting rtx.
   EXPAND_MEMORY means we are interested in a memory result, even if
    the memory is constant and we could have propagated a constant value.  */
enum expand_modifier {EXPAND_NORMAL = 0, EXPAND_STACK_PARM = 2, EXPAND_SUM,
		      EXPAND_CONST_ADDRESS, EXPAND_INITIALIZER, EXPAND_WRITE,
		      EXPAND_MEMORY};

/* Prevent the compiler from deferring stack pops.  See
   inhibit_defer_pop for more information.  */
#define NO_DEFER_POP (inhibit_defer_pop += 1)

/* Allow the compiler to defer stack pops.  See inhibit_defer_pop for
   more information.  */
#define OK_DEFER_POP (inhibit_defer_pop -= 1)

#ifdef TREE_CODE /* Don't lose if tree.h not included.  */
/* Structure to record the size of a sequence of arguments
   as the sum of a tree-expression and a constant.  This structure is
   also used to store offsets from the stack, which might be negative,
   so the variable part must be ssizetype, not sizetype.  */

struct args_size
{
  HOST_WIDE_INT constant;
  tree var;
};
#endif

/* Add the value of the tree INC to the `struct args_size' TO.  */

#define ADD_PARM_SIZE(TO, INC)				\
do {							\
  tree inc = (INC);					\
  if (host_integerp (inc, 0))				\
    (TO).constant += tree_low_cst (inc, 0);		\
  else if ((TO).var == 0)				\
    (TO).var = convert (ssizetype, inc);		\
  else							\
    (TO).var = size_binop (PLUS_EXPR, (TO).var,		\
			   convert (ssizetype, inc));	\
} while (0)

#define SUB_PARM_SIZE(TO, DEC)				\
do {							\
  tree dec = (DEC);					\
  if (host_integerp (dec, 0))				\
    (TO).constant -= tree_low_cst (dec, 0);		\
  else if ((TO).var == 0)				\
    (TO).var = size_binop (MINUS_EXPR, ssize_int (0),	\
			   convert (ssizetype, dec));	\
  else							\
    (TO).var = size_binop (MINUS_EXPR, (TO).var,	\
			   convert (ssizetype, dec));	\
} while (0)

/* Convert the implicit sum in a `struct args_size' into a tree
   of type ssizetype.  */
#define ARGS_SIZE_TREE(SIZE)					\
((SIZE).var == 0 ? ssize_int ((SIZE).constant)			\
 : size_binop (PLUS_EXPR, convert (ssizetype, (SIZE).var),	\
	       ssize_int ((SIZE).constant)))

/* Convert the implicit sum in a `struct args_size' into an rtx.  */
#define ARGS_SIZE_RTX(SIZE)					\
((SIZE).var == 0 ? GEN_INT ((SIZE).constant)			\
 : expand_expr (ARGS_SIZE_TREE (SIZE), NULL_RTX, VOIDmode, 0))

/* Supply a default definition for FUNCTION_ARG_PADDING:
   usually pad upward, but pad short args downward on
   big-endian machines.  */

enum direction {none, upward, downward};  /* Value has this type.  */

#ifndef FUNCTION_ARG_PADDING
#define FUNCTION_ARG_PADDING(MODE, TYPE)				\
  (! BYTES_BIG_ENDIAN							\
   ? upward								\
   : (((MODE) == BLKmode						\
       ? ((TYPE) && TREE_CODE (TYPE_SIZE (TYPE)) == INTEGER_CST		\
	  && int_size_in_bytes (TYPE) < (PARM_BOUNDARY / BITS_PER_UNIT)) \
       : GET_MODE_BITSIZE (MODE) < PARM_BOUNDARY)			\
      ? downward : upward))
#endif

/* Supply a default definition for FUNCTION_ARG_BOUNDARY.  Normally, we let
   FUNCTION_ARG_PADDING, which also pads the length, handle any needed
   alignment.  */

#ifndef FUNCTION_ARG_BOUNDARY
#define FUNCTION_ARG_BOUNDARY(MODE, TYPE)	PARM_BOUNDARY
#endif

/* Provide a default value for STRICT_ARGUMENT_NAMING.  */
#ifndef STRICT_ARGUMENT_NAMING
#define STRICT_ARGUMENT_NAMING 0
#endif

/* Provide a default value for PRETEND_OUTGOING_VARARGS_NAMED.  */
#ifdef SETUP_INCOMING_VARARGS
#ifndef PRETEND_OUTGOING_VARARGS_NAMED
#define PRETEND_OUTGOING_VARARGS_NAMED 1
#endif
#else
/* It is an error to define PRETEND_OUTGOING_VARARGS_NAMED without
   defining SETUP_INCOMING_VARARGS.  */
#define PRETEND_OUTGOING_VARARGS_NAMED 0
#endif

/* Nonzero if we do not know how to pass TYPE solely in registers.
   We cannot do so in the following cases:

   - if the type has variable size
   - if the type is marked as addressable (it is required to be constructed
     into the stack)
   - if the padding and mode of the type is such that a copy into a register
     would put it into the wrong part of the register.

   Which padding can't be supported depends on the byte endianness.

   A value in a register is implicitly padded at the most significant end.
   On a big-endian machine, that is the lower end in memory.
   So a value padded in memory at the upper end can't go in a register.
   For a little-endian machine, the reverse is true.  */

#ifndef MUST_PASS_IN_STACK
#define MUST_PASS_IN_STACK(MODE,TYPE)			\
  ((TYPE) != 0						\
   && (TREE_CODE (TYPE_SIZE (TYPE)) != INTEGER_CST	\
       || TREE_ADDRESSABLE (TYPE)			\
       || ((MODE) == BLKmode 				\
	   && ! ((TYPE) != 0 && TREE_CODE (TYPE_SIZE (TYPE)) == INTEGER_CST \
		 && 0 == (int_size_in_bytes (TYPE)	\
			  % (PARM_BOUNDARY / BITS_PER_UNIT))) \
	   && (FUNCTION_ARG_PADDING (MODE, TYPE)	\
	       == (BYTES_BIG_ENDIAN ? upward : downward)))))
#endif

/* Nonzero if type TYPE should be returned in memory.
   Most machines can use the following default definition.  */

#ifndef RETURN_IN_MEMORY
#define RETURN_IN_MEMORY(TYPE) (TYPE_MODE (TYPE) == BLKmode)
#endif

/* Supply a default definition of STACK_SAVEAREA_MODE for emit_stack_save.
   Normally move_insn, so Pmode stack pointer.  */

#ifndef STACK_SAVEAREA_MODE
#define STACK_SAVEAREA_MODE(LEVEL) Pmode
#endif

/* Supply a default definition of STACK_SIZE_MODE for
   allocate_dynamic_stack_space.  Normally PLUS/MINUS, so word_mode.  */

#ifndef STACK_SIZE_MODE
#define STACK_SIZE_MODE word_mode
#endif

/* Provide default values for the macros controlling stack checking.  */

#ifndef STACK_CHECK_BUILTIN
#define STACK_CHECK_BUILTIN 0
#endif

/* The default interval is one page.  */
#ifndef STACK_CHECK_PROBE_INTERVAL
#define STACK_CHECK_PROBE_INTERVAL 4096
#endif

/* The default is to do a store into the stack.  */
#ifndef STACK_CHECK_PROBE_LOAD
#define STACK_CHECK_PROBE_LOAD 0
#endif

/* This value is arbitrary, but should be sufficient for most machines.  */
#ifndef STACK_CHECK_PROTECT
#define STACK_CHECK_PROTECT (75 * UNITS_PER_WORD)
#endif

/* Make the maximum frame size be the largest we can and still only need
   one probe per function.  */
#ifndef STACK_CHECK_MAX_FRAME_SIZE
#define STACK_CHECK_MAX_FRAME_SIZE \
  (STACK_CHECK_PROBE_INTERVAL - UNITS_PER_WORD)
#endif

/* This is arbitrary, but should be large enough everywhere.  */
#ifndef STACK_CHECK_FIXED_FRAME_SIZE
#define STACK_CHECK_FIXED_FRAME_SIZE (4 * UNITS_PER_WORD)
#endif

/* Provide a reasonable default for the maximum size of an object to
   allocate in the fixed frame.  We may need to be able to make this
   controllable by the user at some point.  */
#ifndef STACK_CHECK_MAX_VAR_SIZE
#define STACK_CHECK_MAX_VAR_SIZE (STACK_CHECK_MAX_FRAME_SIZE / 100)
#endif

/* Functions from optabs.c, commonly used, and without need for the optabs
   tables:  */

/* Passed to expand_simple_binop and expand_binop to say which options
   to try to use if the requested operation can't be open-coded on the
   requisite mode.  Either OPTAB_LIB or OPTAB_LIB_WIDEN says try using
   a library call.  Either OPTAB_WIDEN or OPTAB_LIB_WIDEN says try
   using a wider mode.  OPTAB_MUST_WIDEN says try widening and don't
   try anything else.  */

enum optab_methods
{
  OPTAB_DIRECT,
  OPTAB_LIB,
  OPTAB_WIDEN,
  OPTAB_LIB_WIDEN,
  OPTAB_MUST_WIDEN
};

/* Generate code for a simple binary or unary operation.  "Simple" in
   this case means "can be unambiguously described by a (mode, code)
   pair and mapped to a single optab."  */
extern rtx expand_simple_binop PARAMS ((enum machine_mode, enum rtx_code, rtx,
					rtx, rtx, int, enum optab_methods));
extern rtx expand_simple_unop PARAMS ((enum machine_mode, enum rtx_code,
				       rtx, rtx, int));

/* Report whether the machine description contains an insn which can
   perform the operation described by CODE and MODE.  */
extern int have_insn_for PARAMS ((enum rtx_code, enum machine_mode));

/* Emit code to make a call to a constant function or a library call.  */
extern void emit_libcall_block PARAMS ((rtx, rtx, rtx, rtx));

/* Create but don't emit one rtl instruction to perform certain operations.
   Modes must match; operands must meet the operation's predicates.
   Likewise for subtraction and for just copying.
   These do not call protect_from_queue; caller must do so.  */
extern rtx gen_add2_insn PARAMS ((rtx, rtx));
extern rtx gen_add3_insn PARAMS ((rtx, rtx, rtx));
extern rtx gen_sub2_insn PARAMS ((rtx, rtx));
extern rtx gen_sub3_insn PARAMS ((rtx, rtx, rtx));
extern rtx gen_move_insn PARAMS ((rtx, rtx));
extern int have_add2_insn PARAMS ((rtx, rtx));
extern int have_sub2_insn PARAMS ((rtx, rtx));

/* Emit a pair of rtl insns to compare two rtx's and to jump
   to a label if the comparison is true.  */
extern void emit_cmp_and_jump_insns PARAMS ((rtx, rtx, enum rtx_code, rtx,
					     enum machine_mode, int, rtx));

/* Generate code to indirectly jump to a location given in the rtx LOC.  */
extern void emit_indirect_jump PARAMS ((rtx));

#ifdef HAVE_conditional_move
/* Emit a conditional move operation.  */
rtx emit_conditional_move PARAMS ((rtx, enum rtx_code, rtx, rtx,
				   enum machine_mode, rtx, rtx,
				   enum machine_mode, int));

/* Return nonzero if the conditional move is supported.  */
int can_conditionally_move_p PARAMS ((enum machine_mode mode));

#endif


/* Functions from expmed.c:  */

/* Arguments MODE, RTX: return an rtx for the negation of that value.
   May emit insns.  */
extern rtx negate_rtx PARAMS ((enum machine_mode, rtx));

/* Expand a logical AND operation.  */
extern rtx expand_and PARAMS ((enum machine_mode, rtx, rtx, rtx));

/* Emit a store-flag operation.  */
extern rtx emit_store_flag PARAMS ((rtx, enum rtx_code, rtx, rtx,
				    enum machine_mode, int, int));

/* Like emit_store_flag, but always succeeds.  */
extern rtx emit_store_flag_force PARAMS ((rtx, enum rtx_code, rtx, rtx,
					  enum machine_mode, int, int));

/* Functions from loop.c:  */

/* Given an insn and condition, return a canonical description of
   the test being made.  */
extern rtx canonicalize_condition PARAMS ((rtx, rtx, int, rtx *, rtx));

/* Given a JUMP_INSN, return a canonical description of the test
   being made.  */
extern rtx get_condition PARAMS ((rtx, rtx *));

/* Generate a conditional trap instruction.  */
extern rtx gen_cond_trap PARAMS ((enum rtx_code, rtx, rtx, rtx));

/* Functions from builtins.c:  */
extern rtx expand_builtin PARAMS ((tree, rtx, rtx, enum machine_mode, int));
extern void std_expand_builtin_va_start PARAMS ((tree, rtx));
extern rtx std_expand_builtin_va_arg PARAMS ((tree, tree));
extern rtx expand_builtin_va_arg PARAMS ((tree, tree));
extern void default_init_builtins PARAMS ((void));
extern rtx default_expand_builtin PARAMS ((tree, rtx, rtx,
					   enum machine_mode, int));
extern void expand_builtin_setjmp_setup PARAMS ((rtx, rtx));
extern void expand_builtin_setjmp_receiver PARAMS ((rtx));
extern void expand_builtin_longjmp PARAMS ((rtx, rtx));
extern rtx expand_builtin_saveregs PARAMS ((void));
extern void expand_builtin_trap PARAMS ((void));
extern HOST_WIDE_INT get_varargs_alias_set PARAMS ((void));
extern HOST_WIDE_INT get_frame_alias_set PARAMS ((void));
extern void record_base_value		PARAMS ((unsigned int, rtx, int));
extern void record_alias_subset         PARAMS ((HOST_WIDE_INT,
						 HOST_WIDE_INT));
extern HOST_WIDE_INT new_alias_set		PARAMS ((void));
extern int can_address_p		PARAMS ((tree));

/* Functions from expr.c:  */

/* This is run once per compilation to set up which modes can be used
   directly in memory and to initialize the block move optab.  */
extern void init_expr_once PARAMS ((void));

/* This is run at the start of compiling a function.  */
extern void init_expr PARAMS ((void));

/* This is run at the end of compiling a function.  */
extern void finish_expr_for_function PARAMS ((void));

/* Use protect_from_queue to convert a QUEUED expression
   into something that you can put immediately into an instruction.  */
extern rtx protect_from_queue PARAMS ((rtx, int));

/* Perform all the pending incrementations.  */
extern void emit_queue PARAMS ((void));

/* Tell if something has a queued subexpression.  */
extern int queued_subexp_p PARAMS ((rtx));

/* Emit some rtl insns to move data between rtx's, converting machine modes.
   Both modes must be floating or both fixed.  */
extern void convert_move PARAMS ((rtx, rtx, int));

/* Convert an rtx to specified machine mode and return the result.  */
extern rtx convert_to_mode PARAMS ((enum machine_mode, rtx, int));

/* Convert an rtx to MODE from OLDMODE and return the result.  */
extern rtx convert_modes PARAMS ((enum machine_mode, enum machine_mode,
				  rtx, int));

/* Emit code to move a block Y to a block X.  */

enum block_op_methods
{
  BLOCK_OP_NORMAL,
  BLOCK_OP_NO_LIBCALL,
  BLOCK_OP_CALL_PARM
};

extern rtx emit_block_move PARAMS ((rtx, rtx, rtx, enum block_op_methods));

/* Copy all or part of a value X into registers starting at REGNO.
   The number of registers to be filled is NREGS.  */
extern void move_block_to_reg PARAMS ((int, rtx, int, enum machine_mode));

/* Copy all or part of a BLKmode value X out of registers starting at REGNO.
   The number of registers to be filled is NREGS.  */
extern void move_block_from_reg PARAMS ((int, rtx, int, int));

/* Generate a non-consecutive group of registers represented by a PARALLEL.  */
extern rtx gen_group_rtx PARAMS ((rtx));

/* Load a BLKmode value into non-consecutive registers represented by a
   PARALLEL.  */
extern void emit_group_load PARAMS ((rtx, rtx, int));

/* Move a non-consecutive group of registers represented by a PARALLEL into
   a non-consecutive group of registers represented by a PARALLEL.  */
extern void emit_group_move PARAMS ((rtx, rtx));

/* Store a BLKmode value from non-consecutive registers represented by a
   PARALLEL.  */
extern void emit_group_store PARAMS ((rtx, rtx, int));

#ifdef TREE_CODE
/* Copy BLKmode object from a set of registers.  */
extern rtx copy_blkmode_from_reg PARAMS ((rtx,rtx,tree));
#endif

/* Mark REG as holding a parameter for the next CALL_INSN.  */
extern void use_reg PARAMS ((rtx *, rtx));

/* Mark NREGS consecutive regs, starting at REGNO, as holding parameters
   for the next CALL_INSN.  */
extern void use_regs PARAMS ((rtx *, int, int));

/* Mark a PARALLEL as holding a parameter for the next CALL_INSN.  */
extern void use_group_regs PARAMS ((rtx *, rtx));

/* Write zeros through the storage of OBJECT.
   If OBJECT has BLKmode, SIZE is its length in bytes.  */
extern rtx clear_storage PARAMS ((rtx, rtx));

/* Return nonzero if it is desirable to store LEN bytes generated by
   CONSTFUN with several move instructions by store_by_pieces
   function.  CONSTFUNDATA is a pointer which will be passed as argument
   in every CONSTFUN call.
   ALIGN is maximum alignment we can assume.  */
extern int can_store_by_pieces PARAMS ((unsigned HOST_WIDE_INT,
					rtx (*) (PTR, HOST_WIDE_INT,
						 enum machine_mode),
					PTR, unsigned int));

/* Generate several move instructions to store LEN bytes generated by
   CONSTFUN to block TO.  (A MEM rtx with BLKmode).  CONSTFUNDATA is a
   pointer which will be passed as argument in every CONSTFUN call.
   ALIGN is maximum alignment we can assume.  */
extern void store_by_pieces PARAMS ((rtx, unsigned HOST_WIDE_INT,
				     rtx (*) (PTR, HOST_WIDE_INT,
					      enum machine_mode),
				     PTR, unsigned int));

/* Emit insns to set X from Y.  */
extern rtx emit_move_insn PARAMS ((rtx, rtx));

/* Emit insns to set X from Y, with no frills.  */
extern rtx emit_move_insn_1 PARAMS ((rtx, rtx));

/* Push a block of length SIZE (perhaps variable)
   and return an rtx to address the beginning of the block.  */
extern rtx push_block PARAMS ((rtx, int, int));

#ifdef TREE_CODE
/* Generate code to push something onto the stack, given its mode and type.  */
extern void emit_push_insn PARAMS ((rtx, enum machine_mode, tree, rtx,
				    unsigned int, int, rtx, int, rtx, rtx,
				    int, rtx));

/* Expand an assignment that stores the value of FROM into TO.  */
extern rtx expand_assignment PARAMS ((tree, tree, int, int));

/* Generate code for computing expression EXP,
   and storing the value into TARGET.
   If SUGGEST_REG is nonzero, copy the value through a register
   and return that register, if that is possible.  */
extern rtx store_expr PARAMS ((tree, rtx, int));
#endif

/* Given an rtx that may include add and multiply operations,
   generate them as insns and return a pseudo-reg containing the value.
   Useful after calling expand_expr with 1 as sum_ok.  */
extern rtx force_operand PARAMS ((rtx, rtx));

/* Return an object on the placeholder list that matches EXP, a
   PLACEHOLDER_EXPR.  An object "matches" if it is of the type of the
   PLACEHOLDER_EXPR or a pointer type to it.  For further information, see
   tree.def.  If no such object is found, abort.  If PLIST is nonzero, it is
   a location which initially points to a starting location in the
   placeholder list (zero means start of the list) and where a pointer into
   the placeholder list at which the object is found is placed.  */
extern tree find_placeholder PARAMS ((tree, tree *));

/* Generate code for computing expression EXP.
   An rtx for the computed value is returned.  The value is never null.
   In the case of a void EXP, const0_rtx is returned.  */
extern rtx expand_expr PARAMS ((tree, rtx, enum machine_mode,
				enum expand_modifier));

/* At the start of a function, record that we have no previously-pushed
   arguments waiting to be popped.  */
extern void init_pending_stack_adjust PARAMS ((void));

/* When exiting from function, if safe, clear out any pending stack adjust
   so the adjustment won't get done.  */
extern void clear_pending_stack_adjust PARAMS ((void));

/* Pop any previously-pushed arguments that have not been popped yet.  */
extern void do_pending_stack_adjust PARAMS ((void));

#ifdef TREE_CODE
/* Return the tree node and offset if a given argument corresponds to
   a string constant.  */
extern tree string_constant PARAMS ((tree, tree *));

/* Generate code to evaluate EXP and jump to LABEL if the value is zero.  */
extern void jumpifnot PARAMS ((tree, rtx));

/* Generate code to evaluate EXP and jump to LABEL if the value is nonzero.  */
extern void jumpif PARAMS ((tree, rtx));

/* Generate code to evaluate EXP and jump to IF_FALSE_LABEL if
   the result is zero, or IF_TRUE_LABEL if the result is one.  */
extern void do_jump PARAMS ((tree, rtx, rtx));
#endif

/* Generate rtl to compare two rtx's, will call emit_cmp_insn.  */
extern rtx compare_from_rtx PARAMS ((rtx, rtx, enum rtx_code, int,
				     enum machine_mode, rtx));
extern void do_compare_rtx_and_jump PARAMS ((rtx, rtx, enum rtx_code, int,
					     enum machine_mode, rtx,
					     rtx, rtx));

/* Two different ways of generating switch statements.  */
extern int try_casesi    PARAMS ((tree, tree, tree, tree, rtx, rtx));
extern int try_tablejump PARAMS ((tree, tree, tree, tree, rtx, rtx));

/* Smallest number of adjacent cases before we use a jump table.
   XXX Should be a target hook.  */
extern unsigned int case_values_threshold PARAMS ((void));


#ifdef TREE_CODE
/* rtl.h and tree.h were included.  */
/* Return an rtx for the size in bytes of the value of an expr.  */
extern rtx expr_size PARAMS ((tree));

/* Return a wide integer for the size in bytes of the value of EXP, or -1
   if the size can vary or is larger than an integer.  */
extern HOST_WIDE_INT int_expr_size PARAMS ((tree));

extern rtx lookup_static_chain PARAMS ((tree));

/* Convert a stack slot address ADDR valid in function FNDECL
   into an address valid in this function (using a static chain).  */
extern rtx fix_lexical_addr PARAMS ((rtx, tree));

/* Return the address of the trampoline for entering nested fn FUNCTION.  */
extern rtx trampoline_address PARAMS ((tree));

/* Return an rtx that refers to the value returned by a function
   in its original home.  This becomes invalid if any more code is emitted.  */
extern rtx hard_function_value PARAMS ((tree, tree, int));

extern rtx prepare_call_address	PARAMS ((rtx, tree, rtx *, int, int));

extern rtx expand_call PARAMS ((tree, rtx, int));

extern rtx expand_shift PARAMS ((enum tree_code, enum machine_mode, rtx, tree,
				 rtx, int));
extern rtx expand_divmod PARAMS ((int, enum tree_code, enum machine_mode, rtx,
				  rtx, rtx, int));
extern void locate_and_pad_parm PARAMS ((enum machine_mode, tree, int, tree,
					 struct args_size *,
					 struct args_size *,
					 struct args_size *,
					 struct args_size *));
extern rtx expand_inline_function PARAMS ((tree, tree, rtx, int, tree, rtx));

/* Return the CODE_LABEL rtx for a LABEL_DECL, creating it if necessary.  */
extern rtx label_rtx PARAMS ((tree));
#endif

/* Indicate how an input argument register was promoted.  */
extern rtx promoted_input_arg PARAMS ((unsigned int, enum machine_mode *,
				       int *));

/* Return an rtx like arg but sans any constant terms.
   Returns the original rtx if it has no constant terms.
   The constant terms are added and stored via a second arg.  */
extern rtx eliminate_constant_term PARAMS ((rtx, rtx *));

/* Convert arg to a valid memory address for specified machine mode,
   by emitting insns to perform arithmetic if nec.  */
extern rtx memory_address PARAMS ((enum machine_mode, rtx));

/* Like `memory_address' but pretent `flag_force_addr' is 0.  */
extern rtx memory_address_noforce PARAMS ((enum machine_mode, rtx));

/* Set the alias set of MEM to SET.  */
extern void set_mem_alias_set PARAMS ((rtx, HOST_WIDE_INT));

/* Set the alignment of MEM to ALIGN bits.  */
extern void set_mem_align PARAMS ((rtx, unsigned int));

/* Set the expr for MEM to EXPR.  */
extern void set_mem_expr PARAMS ((rtx, tree));

/* Set the offset for MEM to OFFSET.  */
extern void set_mem_offset PARAMS ((rtx, rtx));

/* Set the size for MEM to SIZE.  */
extern void set_mem_size PARAMS ((rtx, rtx));

/* Return a memory reference like MEMREF, but with its mode changed
   to MODE and its address changed to ADDR.
   (VOIDmode means don't change the mode.
   NULL for ADDR means don't change the address.)  */
extern rtx change_address PARAMS ((rtx, enum machine_mode, rtx));

/* Return a memory reference like MEMREF, but with its mode changed
   to MODE and its address offset by OFFSET bytes.  */
#define adjust_address(MEMREF, MODE, OFFSET) \
  adjust_address_1 (MEMREF, MODE, OFFSET, 1, 1)

/* Likewise, but the reference is not required to be valid.  */
#define adjust_address_nv(MEMREF, MODE, OFFSET) \
  adjust_address_1 (MEMREF, MODE, OFFSET, 0, 1)

/* Return a memory reference like MEMREF, but with its mode changed
   to MODE and its address changed to ADDR, which is assumed to be
   increased by OFFSET bytes from MEMREF.  */
#define adjust_automodify_address(MEMREF, MODE, ADDR, OFFSET) \
  adjust_automodify_address_1 (MEMREF, MODE, ADDR, OFFSET, 1)

/* Likewise, but the reference is not required to be valid.  */
#define adjust_automodify_address_nv(MEMREF, MODE, ADDR, OFFSET) \
  adjust_automodify_address_1 (MEMREF, MODE, ADDR, OFFSET, 0)

extern rtx adjust_address_1 PARAMS ((rtx, enum machine_mode, HOST_WIDE_INT,
				     int, int));
extern rtx adjust_automodify_address_1 PARAMS ((rtx, enum machine_mode,
						rtx, HOST_WIDE_INT, int));

/* Return a memory reference like MEMREF, but whose address is changed by
   adding OFFSET, an RTX, to it.  POW2 is the highest power of two factor
   known to be in OFFSET (possibly 1).  */
extern rtx offset_address PARAMS ((rtx, rtx, HOST_WIDE_INT));

/* Return a memory reference like MEMREF, but with its address changed to
   ADDR.  The caller is asserting that the actual piece of memory pointed
   to is the same, just the form of the address is being changed, such as
   by putting something into a register.  */
extern rtx replace_equiv_address PARAMS ((rtx, rtx));

/* Likewise, but the reference is not required to be valid.  */
extern rtx replace_equiv_address_nv PARAMS ((rtx, rtx));

/* Return a memory reference like MEMREF, but with its mode widened to
   MODE and adjusted by OFFSET.  */
extern rtx widen_memory_access PARAMS ((rtx, enum machine_mode, HOST_WIDE_INT));

/* Return a memory reference like MEMREF, but which is known to have a
   valid address.  */
extern rtx validize_mem PARAMS ((rtx));

#ifdef TREE_CODE
/* Given REF, either a MEM or a REG, and T, either the type of X or
   the expression corresponding to REF, set RTX_UNCHANGING_P if
   appropriate.  */
extern void maybe_set_unchanging PARAMS ((rtx, tree));

/* Given REF, a MEM, and T, either the type of X or the expression
   corresponding to REF, set the memory attributes.  OBJECTP is nonzero
   if we are making a new object of this type.  */
extern void set_mem_attributes PARAMS ((rtx, tree, int));

/* Similar, except that BITPOS has not yet been applied to REF, so if
   we alter MEM_OFFSET according to T then we should subtract BITPOS
   expecting that it'll be added back in later.  */
extern void set_mem_attributes_minus_bitpos PARAMS ((rtx, tree, int,
						     HOST_WIDE_INT));
#endif

/* Assemble the static constant template for function entry trampolines.  */
extern rtx assemble_trampoline_template PARAMS ((void));

/* Given rtx, return new rtx whose address won't be affected by
   any side effects.  It has been copied to a new temporary reg.  */
extern rtx stabilize PARAMS ((rtx));

/* Given an rtx, copy all regs it refers to into new temps
   and return a modified copy that refers to the new temps.  */
extern rtx copy_all_regs PARAMS ((rtx));

/* Copy given rtx to a new temp reg and return that.  */
extern rtx copy_to_reg PARAMS ((rtx));

/* Like copy_to_reg but always make the reg Pmode.  */
extern rtx copy_addr_to_reg PARAMS ((rtx));

/* Like copy_to_reg but always make the reg the specified mode MODE.  */
extern rtx copy_to_mode_reg PARAMS ((enum machine_mode, rtx));

/* Copy given rtx to given temp reg and return that.  */
extern rtx copy_to_suggested_reg PARAMS ((rtx, rtx, enum machine_mode));

/* Copy a value to a register if it isn't already a register.
   Args are mode (in case value is a constant) and the value.  */
extern rtx force_reg PARAMS ((enum machine_mode, rtx));

/* Return given rtx, copied into a new temp reg if it was in memory.  */
extern rtx force_not_mem PARAMS ((rtx));

#ifdef TREE_CODE
/* Return mode and signedness to use when object is promoted.  */
extern enum machine_mode promote_mode PARAMS ((tree, enum machine_mode,
					       int *, int));
#endif

/* Remove some bytes from the stack.  An rtx says how many.  */
extern void adjust_stack PARAMS ((rtx));

/* Add some bytes to the stack.  An rtx says how many.  */
extern void anti_adjust_stack PARAMS ((rtx));

/* This enum is used for the following two functions.  */
enum save_level {SAVE_BLOCK, SAVE_FUNCTION, SAVE_NONLOCAL};

/* Save the stack pointer at the specified level.  */
extern void emit_stack_save PARAMS ((enum save_level, rtx *, rtx));

/* Restore the stack pointer from a save area of the specified level.  */
extern void emit_stack_restore PARAMS ((enum save_level, rtx, rtx));

/* Allocate some space on the stack dynamically and return its address.  An rtx
   says how many bytes.  */
extern rtx allocate_dynamic_stack_space PARAMS ((rtx, rtx, int));

/* Probe a range of stack addresses from FIRST to FIRST+SIZE, inclusive.
   FIRST is a constant and size is a Pmode RTX.  These are offsets from the
   current stack pointer.  STACK_GROWS_DOWNWARD says whether to add or
   subtract from the stack.  If SIZE is constant, this is done
   with a fixed number of probes.  Otherwise, we must make a loop.  */
extern void probe_stack_range PARAMS ((HOST_WIDE_INT, rtx));

/* Return an rtx that refers to the value returned by a library call
   in its original home.  This becomes invalid if any more code is emitted.  */
extern rtx hard_libcall_value PARAMS ((enum machine_mode));

/* Given an rtx, return an rtx for a value rounded up to a multiple
   of STACK_BOUNDARY / BITS_PER_UNIT.  */
extern rtx round_push PARAMS ((rtx));

/* Return the mode desired by operand N of a particular bitfield
   insert/extract insn, or MAX_MACHINE_MODE if no such insn is
   available.  */

enum extraction_pattern { EP_insv, EP_extv, EP_extzv };
extern enum machine_mode
mode_for_extraction PARAMS ((enum extraction_pattern, int));

extern rtx store_bit_field PARAMS ((rtx, unsigned HOST_WIDE_INT,
				    unsigned HOST_WIDE_INT,
				    enum machine_mode, rtx, HOST_WIDE_INT));
extern rtx extract_bit_field PARAMS ((rtx, unsigned HOST_WIDE_INT,
				      unsigned HOST_WIDE_INT, int, rtx,
				      enum machine_mode, enum machine_mode,
				      HOST_WIDE_INT));
extern rtx expand_mult PARAMS ((enum machine_mode, rtx, rtx, rtx, int));
extern bool const_mult_add_overflow_p PARAMS ((rtx, rtx, rtx, enum machine_mode, int));
extern rtx expand_mult_add PARAMS ((rtx, rtx, rtx, rtx,enum machine_mode, int));
extern rtx expand_mult_highpart_adjust PARAMS ((enum machine_mode, rtx, rtx, rtx, rtx, int));

extern rtx assemble_static_space PARAMS ((int));

/* Hook called by expand_expr for language-specific tree codes.
   It is up to the language front end to install a hook
   if it has any such codes that expand_expr needs to know about.  */
extern rtx (*lang_expand_expr) PARAMS ((union tree_node *, rtx,
					enum machine_mode,
					enum expand_modifier modifier));

extern int safe_from_p PARAMS ((rtx, tree, int));

/* Call this once to initialize the contents of the optabs
   appropriately for the current target machine.  */
extern void init_optabs				PARAMS ((void));
extern void init_all_optabs			PARAMS ((void));

/* Call this to initialize an optab function entry.  */
extern rtx init_one_libfunc			PARAMS ((const char *));

extern void do_jump_by_parts_equality_rtx	PARAMS ((rtx, rtx, rtx));
extern void do_jump_by_parts_greater_rtx	PARAMS ((enum machine_mode,
							 int, rtx, rtx, rtx,
							 rtx));

#ifdef TREE_CODE   /* Don't lose if tree.h not included.  */
extern void mark_seen_cases			PARAMS ((tree, unsigned char *,
							 HOST_WIDE_INT, int));
#endif

extern int vector_mode_valid_p		PARAMS ((enum machine_mode));
