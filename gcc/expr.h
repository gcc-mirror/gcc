/* Definitions for code generation pass of GNU compiler.
   Copyright (C) 1987, 91-98, 1999 Free Software Foundation, Inc.

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
   EXPAND_SUM means it is ok to return a PLUS rtx or MULT rtx.
   EXPAND_INITIALIZER is similar but also record any labels on forced_labels.
   EXPAND_CONST_ADDRESS means it is ok to return a MEM whose address
    is a constant that is not a legitimate address.
   EXPAND_MEMORY_USE_* are explained below.  */
enum expand_modifier {EXPAND_NORMAL, EXPAND_SUM,
		      EXPAND_CONST_ADDRESS, EXPAND_INITIALIZER,
		      EXPAND_MEMORY_USE_WO, EXPAND_MEMORY_USE_RW,
		      EXPAND_MEMORY_USE_BAD, EXPAND_MEMORY_USE_DONT};

/* Argument for chkr_* functions.
   MEMORY_USE_RO: the pointer reads memory.
   MEMORY_USE_WO: the pointer writes to memory.
   MEMORY_USE_RW: the pointer modifies memory (ie it reads and writes). An
                  example is (*ptr)++
   MEMORY_USE_BAD: use this if you don't know the behavior of the pointer, or
                   if you know there are no pointers.  Using an INDIRECT_REF
                   with MEMORY_USE_BAD will abort.
   MEMORY_USE_TW: just test for writing, without update.  Special.
   MEMORY_USE_DONT: the memory is neither read nor written.  This is used by
   		   '->' and '.'.  */
enum memory_use_mode {MEMORY_USE_BAD = 0, MEMORY_USE_RO = 1,
		      MEMORY_USE_WO = 2, MEMORY_USE_RW = 3,
		      MEMORY_USE_TW = 6, MEMORY_USE_DONT = 99};

/* List of labels that must never be deleted.  */
extern rtx forced_labels;

/* List (chain of EXPR_LISTs) of pseudo-regs of SAVE_EXPRs.
   So we can mark them all live at the end of the function, if stupid.  */
extern rtx save_expr_regs;

extern int current_function_calls_alloca;
extern int current_function_outgoing_args_size;

/* This is the offset from the arg pointer to the place where the first
   anonymous arg can be found, if there is one.  */
extern rtx current_function_arg_offset_rtx;

/* This is nonzero if the current function uses the constant pool.  */
extern int current_function_uses_const_pool;

/* This is nonzero if the current function uses pic_offset_table_rtx.  */
extern int current_function_uses_pic_offset_table;

/* The arg pointer hard register, or the pseudo into which it was copied.  */
extern rtx current_function_internal_arg_pointer;

/* This is nonzero if memory access checking be enabled in the current
   function.  */
extern int current_function_check_memory_usage;

/* Under some ABIs, it is the caller's responsibility to pop arguments
   pushed for function calls.  A naive implementation would simply pop
   the arguments immediately after each call.  However, if several
   function calls are made in a row, it is typically cheaper to pop
   all the arguments after all of the calls are complete since a
   single pop instruction can be used.  Therefore, GCC attempts to
   defer popping the arguments until absolutely necessary.  (For
   example, at the end of a conditional, the arguments must be popped,
   since code outside the conditional won't know whether or not the
   arguments need to be popped.)

   When INHIBIT_DEFER_POP is non-zero, however, the compiler does not
   attempt to defer pops.  Instead, the stack is popped immediately
   after each call.  Rather then setting this variable directly, use
   NO_DEFER_POP and OK_DEFER_POP.  */
extern int inhibit_defer_pop;

/* Prevent the compiler from deferring stack pops.  See
   inhibit_defer_pop for more information.  */
#define NO_DEFER_POP (inhibit_defer_pop += 1)

/* Allow the compiler to defer stack pops.  See inhibit_defer_pop for
   more information.  */
#define OK_DEFER_POP (inhibit_defer_pop -= 1)

/* Number of function calls seen so far in current function.  */

extern int function_call_count;

/* List (chain of EXPR_LIST) of stack slots that hold the current handlers
   for nonlocal gotos.  There is one for every nonlocal label in the function;
   this list matches the one in nonlocal_labels.
   Zero when function does not have nonlocal labels.  */

extern rtx nonlocal_goto_handler_slots;

/* RTX for stack slot that holds the stack pointer value to restore
   for a nonlocal goto.
   Zero when function does not have nonlocal labels.  */

extern rtx nonlocal_goto_stack_level;

/* List (chain of TREE_LIST) of LABEL_DECLs for all nonlocal labels
   (labels to which there can be nonlocal gotos from nested functions)
   in this function.  */

#ifdef TREE_CODE   /* Don't lose if tree.h not included.  */
extern tree nonlocal_labels;
#endif

/* Number of units that we should eventually pop off the stack.
   These are the arguments to function calls that have already returned.  */
extern int pending_stack_adjust;

/* When temporaries are created by TARGET_EXPRs, they are created at
   this level of temp_slot_level, so that they can remain allocated
   until no longer needed.  CLEANUP_POINT_EXPRs define the lifetime
   of TARGET_EXPRs.  */
extern int target_temp_slot_level;

/* Current level for normal temporaries.  */

extern int temp_slot_level;

#ifdef TREE_CODE /* Don't lose if tree.h not included.  */
/* Structure to record the size of a sequence of arguments
   as the sum of a tree-expression and a constant.  */

struct args_size
{
  HOST_WIDE_INT constant;
  tree var;
};
#endif

/* Add the value of the tree INC to the `struct args_size' TO.  */

#define ADD_PARM_SIZE(TO, INC)	\
{ tree inc = (INC);				\
  if (TREE_CODE (inc) == INTEGER_CST)		\
    (TO).constant += TREE_INT_CST_LOW (inc);	\
  else if ((TO).var == 0)			\
    (TO).var = inc;				\
  else						\
    (TO).var = size_binop (PLUS_EXPR, (TO).var, inc); }

#define SUB_PARM_SIZE(TO, DEC)	\
{ tree dec = (DEC);				\
  if (TREE_CODE (dec) == INTEGER_CST)		\
    (TO).constant -= TREE_INT_CST_LOW (dec);	\
  else if ((TO).var == 0)			\
    (TO).var = size_binop (MINUS_EXPR, integer_zero_node, dec); \
  else						\
    (TO).var = size_binop (MINUS_EXPR, (TO).var, dec); }

/* Convert the implicit sum in a `struct args_size' into an rtx.  */
#define ARGS_SIZE_RTX(SIZE)						\
((SIZE).var == 0 ? GEN_INT ((SIZE).constant)	\
 : expand_expr (size_binop (PLUS_EXPR, (SIZE).var,			\
			    size_int ((SIZE).constant)),		\
		NULL_RTX, VOIDmode, EXPAND_MEMORY_USE_BAD))

/* Convert the implicit sum in a `struct args_size' into a tree.  */
#define ARGS_SIZE_TREE(SIZE)						\
((SIZE).var == 0 ? size_int ((SIZE).constant)				\
 : size_binop (PLUS_EXPR, (SIZE).var, size_int ((SIZE).constant)))

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

/* Optabs are tables saying how to generate insn bodies
   for various machine modes and numbers of operands.
   Each optab applies to one operation.
   For example, add_optab applies to addition.

   The insn_code slot is the enum insn_code that says how to
   generate an insn for this operation on a particular machine mode.
   It is CODE_FOR_nothing if there is no such insn on the target machine.

   The `lib_call' slot is the name of the library function that
   can be used to perform the operation.

   A few optabs, such as move_optab and cmp_optab, are used
   by special code.  */

/* Everything that uses expr.h needs to define enum insn_code
   but we don't list it in the Makefile dependencies just for that.  */
#include "insn-codes.h"

typedef struct optab
{
  enum rtx_code code;
  struct {
    enum insn_code insn_code;
    rtx libfunc;
  } handlers [NUM_MACHINE_MODES];
} * optab;

/* Given an enum insn_code, access the function to construct
   the body of that kind of insn.  */
#ifdef FUNCTION_CONVERSION_BUG
/* Some compilers fail to convert a function properly to a
   pointer-to-function when used as an argument.
   So produce the pointer-to-function directly.
   Luckily, these compilers seem to work properly when you
   call the pointer-to-function.  */
#define GEN_FCN(CODE) (insn_gen_function[(int) (CODE)])
#else
#define GEN_FCN(CODE) (*insn_gen_function[(int) (CODE)])
#endif

extern rtx (*const insn_gen_function[]) PROTO ((rtx, ...));

extern optab add_optab;
extern optab sub_optab;
extern optab smul_optab;	/* Signed and floating-point multiply */
extern optab smul_highpart_optab; /* Signed multiply, return high word */
extern optab umul_highpart_optab;
extern optab smul_widen_optab;	/* Signed multiply with result 
				   one machine mode wider than args */
extern optab umul_widen_optab;
extern optab sdiv_optab;	/* Signed divide */
extern optab sdivmod_optab;	/* Signed divide-and-remainder in one */
extern optab udiv_optab;
extern optab udivmod_optab;
extern optab smod_optab;	/* Signed remainder */
extern optab umod_optab;
extern optab flodiv_optab;	/* Optab for floating divide. */
extern optab ftrunc_optab;	/* Convert float to integer in float fmt */
extern optab and_optab;		/* Logical and */
extern optab ior_optab;		/* Logical or */
extern optab xor_optab;		/* Logical xor */
extern optab ashl_optab;	/* Arithmetic shift left */
extern optab ashr_optab;	/* Arithmetic shift right */
extern optab lshr_optab;	/* Logical shift right */
extern optab rotl_optab;	/* Rotate left */
extern optab rotr_optab;	/* Rotate right */
extern optab smin_optab;	/* Signed and floating-point minimum value */
extern optab smax_optab;	/* Signed and floating-point maximum value */
extern optab umin_optab;	/* Unsigned minimum value */
extern optab umax_optab;	/* Unsigned maximum value */

extern optab mov_optab;		/* Move instruction.  */
extern optab movstrict_optab;	/* Move, preserving high part of register.  */

extern optab cmp_optab;		/* Compare insn; two operands.  */
extern optab tst_optab;		/* tst insn; compare one operand against 0 */

/* Unary operations */
extern optab neg_optab;		/* Negation */
extern optab abs_optab;		/* Abs value */
extern optab one_cmpl_optab;	/* Bitwise not */
extern optab ffs_optab;		/* Find first bit set */
extern optab sqrt_optab;	/* Square root */
extern optab sin_optab;		/* Sine */
extern optab cos_optab;		/* Cosine */
extern optab strlen_optab;	/* String length */

/* Tables of patterns for extending one integer mode to another.  */
extern enum insn_code extendtab[MAX_MACHINE_MODE][MAX_MACHINE_MODE][2];

/* Tables of patterns for converting between fixed and floating point. */
extern enum insn_code fixtab[NUM_MACHINE_MODES][NUM_MACHINE_MODES][2];
extern enum insn_code fixtrunctab[NUM_MACHINE_MODES][NUM_MACHINE_MODES][2];
extern enum insn_code floattab[NUM_MACHINE_MODES][NUM_MACHINE_MODES][2];

/* Contains the optab used for each rtx code.  */
extern optab code_to_optab[NUM_RTX_CODE + 1];

/* Passed to expand_binop and expand_unop to say which options to try to use
   if the requested operation can't be open-coded on the requisite mode.
   Either OPTAB_LIB or OPTAB_LIB_WIDEN says try using a library call.
   Either OPTAB_WIDEN or OPTAB_LIB_WIDEN says try using a wider mode.
   OPTAB_MUST_WIDEN says try widening and don't try anything else.  */

enum optab_methods
{
  OPTAB_DIRECT,
  OPTAB_LIB,
  OPTAB_WIDEN,
  OPTAB_LIB_WIDEN,
  OPTAB_MUST_WIDEN
};

/* SYMBOL_REF rtx's for the library functions that are called
   implicitly and not via optabs.  */

extern rtx extendsfdf2_libfunc;
extern rtx extendsfxf2_libfunc;
extern rtx extendsftf2_libfunc;
extern rtx extenddfxf2_libfunc;
extern rtx extenddftf2_libfunc;

extern rtx truncdfsf2_libfunc;
extern rtx truncxfsf2_libfunc;
extern rtx trunctfsf2_libfunc;
extern rtx truncxfdf2_libfunc;
extern rtx trunctfdf2_libfunc;

extern rtx memcpy_libfunc;
extern rtx bcopy_libfunc;
extern rtx memcmp_libfunc;
extern rtx bcmp_libfunc;
extern rtx memset_libfunc;
extern rtx bzero_libfunc;

extern rtx throw_libfunc;
extern rtx rethrow_libfunc;
extern rtx sjthrow_libfunc;
extern rtx sjpopnthrow_libfunc;
extern rtx terminate_libfunc;
extern rtx setjmp_libfunc;
extern rtx longjmp_libfunc;
extern rtx eh_rtime_match_libfunc;

extern rtx eqhf2_libfunc;
extern rtx nehf2_libfunc;
extern rtx gthf2_libfunc;
extern rtx gehf2_libfunc;
extern rtx lthf2_libfunc;
extern rtx lehf2_libfunc;

extern rtx eqsf2_libfunc;
extern rtx nesf2_libfunc;
extern rtx gtsf2_libfunc;
extern rtx gesf2_libfunc;
extern rtx ltsf2_libfunc;
extern rtx lesf2_libfunc;

extern rtx eqdf2_libfunc;
extern rtx nedf2_libfunc;
extern rtx gtdf2_libfunc;
extern rtx gedf2_libfunc;
extern rtx ltdf2_libfunc;
extern rtx ledf2_libfunc;

extern rtx eqxf2_libfunc;
extern rtx nexf2_libfunc;
extern rtx gtxf2_libfunc;
extern rtx gexf2_libfunc;
extern rtx ltxf2_libfunc;
extern rtx lexf2_libfunc;

extern rtx eqtf2_libfunc;
extern rtx netf2_libfunc;
extern rtx gttf2_libfunc;
extern rtx getf2_libfunc;
extern rtx lttf2_libfunc;
extern rtx letf2_libfunc;

extern rtx floatsisf_libfunc;
extern rtx floatdisf_libfunc;
extern rtx floattisf_libfunc;

extern rtx floatsidf_libfunc;
extern rtx floatdidf_libfunc;
extern rtx floattidf_libfunc;

extern rtx floatsixf_libfunc;
extern rtx floatdixf_libfunc;
extern rtx floattixf_libfunc;

extern rtx floatsitf_libfunc;
extern rtx floatditf_libfunc;
extern rtx floattitf_libfunc;

extern rtx fixsfsi_libfunc;
extern rtx fixsfdi_libfunc;
extern rtx fixsfti_libfunc;

extern rtx fixdfsi_libfunc;
extern rtx fixdfdi_libfunc;
extern rtx fixdfti_libfunc;

extern rtx fixxfsi_libfunc;
extern rtx fixxfdi_libfunc;
extern rtx fixxfti_libfunc;

extern rtx fixtfsi_libfunc;
extern rtx fixtfdi_libfunc;
extern rtx fixtfti_libfunc;

extern rtx fixunssfsi_libfunc;
extern rtx fixunssfdi_libfunc;
extern rtx fixunssfti_libfunc;

extern rtx fixunsdfsi_libfunc;
extern rtx fixunsdfdi_libfunc;
extern rtx fixunsdfti_libfunc;

extern rtx fixunsxfsi_libfunc;
extern rtx fixunsxfdi_libfunc;
extern rtx fixunsxfti_libfunc;

extern rtx fixunstfsi_libfunc;
extern rtx fixunstfdi_libfunc;
extern rtx fixunstfti_libfunc;

/* For check-memory-usage.  */
extern rtx chkr_check_addr_libfunc;
extern rtx chkr_set_right_libfunc;
extern rtx chkr_copy_bitmap_libfunc;
extern rtx chkr_check_exec_libfunc;
extern rtx chkr_check_str_libfunc;

/* For instrument-functions.  */
extern rtx profile_function_entry_libfunc;
extern rtx profile_function_exit_libfunc;

typedef rtx (*rtxfun) PROTO ((rtx));

/* Indexed by the rtx-code for a conditional (eg. EQ, LT,...)
   gives the gen_function to make a branch to test that condition.  */

extern rtxfun bcc_gen_fctn[NUM_RTX_CODE];

/* Indexed by the rtx-code for a conditional (eg. EQ, LT,...)
   gives the insn code to make a store-condition insn
   to test that condition.  */

extern enum insn_code setcc_gen_code[NUM_RTX_CODE];

#ifdef HAVE_conditional_move
/* Indexed by the machine mode, gives the insn code to make a conditional
   move insn.  */

extern enum insn_code movcc_gen_code[NUM_MACHINE_MODES];
#endif

/* This array records the insn_code of insns to perform block moves.  */
extern enum insn_code movstr_optab[NUM_MACHINE_MODES];

/* This array records the insn_code of insns to perform block clears.  */
extern enum insn_code clrstr_optab[NUM_MACHINE_MODES];

/* Define functions given in optabs.c.  */

/* Expand a binary operation given optab and rtx operands.  */
extern rtx expand_binop PROTO((enum machine_mode, optab, rtx, rtx, rtx,
			       int, enum optab_methods));

/* Expand a binary operation with both signed and unsigned forms.  */
extern rtx sign_expand_binop PROTO((enum machine_mode, optab, optab, rtx,
				    rtx, rtx, int, enum optab_methods));

/* Generate code to perform an operation on two operands with two results.  */
extern int expand_twoval_binop PROTO((optab, rtx, rtx, rtx, rtx, int));

/* Expand a unary arithmetic operation given optab rtx operand.  */
extern rtx expand_unop PROTO((enum machine_mode, optab, rtx, rtx, int));

/* Expand the absolute value operation.  */
extern rtx expand_abs PROTO((enum machine_mode, rtx, rtx, int));

/* Expand the complex absolute value operation.  */
extern rtx expand_complex_abs PROTO((enum machine_mode, rtx, rtx, int));

/* Generate an instruction with a given INSN_CODE with an output and
   an input.  */
extern void emit_unop_insn PROTO((int, rtx, rtx, enum rtx_code));

/* Emit code to perform a series of operations on a multi-word quantity, one
   word at a time.  */
extern rtx emit_no_conflict_block PROTO((rtx, rtx, rtx, rtx, rtx));

/* Emit code to make a call to a constant function or a library call. */
extern void emit_libcall_block PROTO((rtx, rtx, rtx, rtx));

/* Emit one rtl instruction to store zero in specified rtx.  */
extern void emit_clr_insn PROTO((rtx));

/* Emit one rtl insn to store 1 in specified rtx assuming it contains 0.  */
extern void emit_0_to_1_insn PROTO((rtx));

/* Emit one rtl insn to compare two rtx's.  */
extern void emit_cmp_insn PROTO((rtx, rtx, enum rtx_code, rtx,
				 enum machine_mode, int, int));

/* Emit a pair of rtl insns to compare two rtx's and to jump 
   to a label if the comparison is true.  */
extern void emit_cmp_and_jump_insns PROTO((rtx, rtx, enum rtx_code, rtx,
					   enum machine_mode, int, int, rtx));

/* Nonzero if a compare of mode MODE can be done straightforwardly
   (without splitting it into pieces).  */
extern int can_compare_p PROTO((enum machine_mode));

/* Emit a library call comparison between floating point X and Y.
   COMPARISON is the rtl operator to compare with (EQ, NE, GT, etc.).  */
extern void emit_float_lib_cmp PROTO((rtx, rtx, enum rtx_code));

/* Generate code to indirectly jump to a location given in the rtx LOC.  */
extern void emit_indirect_jump PROTO((rtx));

#ifdef HAVE_conditional_move
/* Emit a conditional move operation.  */
rtx emit_conditional_move PROTO((rtx, enum rtx_code, rtx, rtx,
				 enum machine_mode, rtx, rtx,
				 enum machine_mode, int));

/* Return non-zero if the conditional move is supported.  */
int can_conditionally_move_p PROTO((enum machine_mode mode));

#endif

/* Create but don't emit one rtl instruction to add one rtx into another.
   Modes must match; operands must meet the operation's predicates.
   Likewise for subtraction and for just copying.
   These do not call protect_from_queue; caller must do so.  */
extern rtx gen_add2_insn PROTO((rtx, rtx));
extern rtx gen_sub2_insn PROTO((rtx, rtx));
extern rtx gen_move_insn PROTO((rtx, rtx));
extern int have_add2_insn PROTO((enum machine_mode));
extern int have_sub2_insn PROTO((enum machine_mode));

/* Return the INSN_CODE to use for an extend operation.  */
extern enum insn_code can_extend_p PROTO((enum machine_mode,
					  enum machine_mode, int));

/* Generate the body of an insn to extend Y (with mode MFROM)
   into X (with mode MTO).  Do zero-extension if UNSIGNEDP is nonzero.  */
extern rtx gen_extend_insn PROTO((rtx, rtx, enum machine_mode,
				  enum machine_mode, int));

/* Initialize the tables that control conversion between fixed and
   floating values.  */
extern void init_fixtab PROTO((void));
extern void init_floattab PROTO((void));

/* Generate code for a FLOAT_EXPR.  */
extern void expand_float PROTO((rtx, rtx, int));

/* Generate code for a FIX_EXPR.  */
extern void expand_fix PROTO((rtx, rtx, int));

/* Call this once to initialize the contents of the optabs
   appropriately for the current target machine.  */
extern void init_optabs	PROTO((void));

/* Functions from expmed.c:  */

/* Arguments MODE, RTX: return an rtx for the negation of that value.
   May emit insns.  */
extern rtx negate_rtx PROTO((enum machine_mode, rtx));

/* Expand a logical AND operation.  */
extern rtx expand_and PROTO((rtx, rtx, rtx));

/* Emit a store-flag operation.  */
extern rtx emit_store_flag PROTO((rtx, enum rtx_code, rtx, rtx,
				  enum machine_mode, int, int));

/* Like emit_store_flag, but always succeeds.  */
extern rtx emit_store_flag_force PROTO((rtx, enum rtx_code, rtx, rtx,
					enum machine_mode, int, int));

/* Functions from loop.c:  */

/* Given a JUMP_INSN, return a description of the test being made.  */
extern rtx get_condition PROTO((rtx, rtx *));

/* Generate a conditional trap instruction.  */
extern rtx gen_cond_trap PROTO((enum rtx_code, rtx, rtx, rtx));

/* Functions from expr.c:  */

/* This is run once per compilation to set up which modes can be used
   directly in memory and to initialize the block move optab.  */
extern void init_expr_once PROTO((void));

/* This is run at the start of compiling a function.  */
extern void init_expr PROTO((void));

/* Use protect_from_queue to convert a QUEUED expression
   into something that you can put immediately into an instruction.  */
extern rtx protect_from_queue PROTO((rtx, int));

/* Perform all the pending incrementations.  */
extern void emit_queue PROTO((void));

/* Tell if something has a queued subexpression.  */
extern int queued_subexp_p PROTO((rtx));

/* Emit some rtl insns to move data between rtx's, converting machine modes.
   Both modes must be floating or both fixed.  */
extern void convert_move PROTO((rtx, rtx, int));

/* Convert an rtx to specified machine mode and return the result.  */
extern rtx convert_to_mode PROTO((enum machine_mode, rtx, int));

/* Convert an rtx to MODE from OLDMODE and return the result.  */
extern rtx convert_modes PROTO((enum machine_mode, enum machine_mode, rtx, int));

/* Emit code to move a block Y to a block X.  */
extern rtx emit_block_move PROTO((rtx, rtx, rtx, int));

/* Copy all or part of a value X into registers starting at REGNO.
   The number of registers to be filled is NREGS.  */
extern void move_block_to_reg PROTO((int, rtx, int, enum machine_mode));

/* Copy all or part of a BLKmode value X out of registers starting at REGNO.
   The number of registers to be filled is NREGS.  */
extern void move_block_from_reg PROTO((int, rtx, int, int));

/* Load a BLKmode value into non-consecutive registers represented by a
   PARALLEL.  */
extern void emit_group_load PROTO((rtx, rtx, int, int));
/* Store a BLKmode value from non-consecutive registers represented by a
   PARALLEL.  */
extern void emit_group_store PROTO((rtx, rtx, int, int));

#ifdef TREE_CODE
/* Copy BLKmode object from a set of registers. */
extern rtx copy_blkmode_from_reg PROTO((rtx,rtx,tree));
#endif

/* Mark REG as holding a parameter for the next CALL_INSN.  */
extern void use_reg PROTO((rtx *, rtx));
/* Mark NREGS consecutive regs, starting at REGNO, as holding parameters
   for the next CALL_INSN.  */
extern void use_regs PROTO((rtx *, int, int));
/* Mark a PARALLEL as holding a parameter for the next CALL_INSN.  */
extern void use_group_regs PROTO((rtx *, rtx));

/* Write zeros through the storage of OBJECT.
   If OBJECT has BLKmode, SIZE is its length in bytes and ALIGN is its
   alignment.  */
extern rtx clear_storage PROTO((rtx, rtx, int));

/* Emit insns to set X from Y.  */
extern rtx emit_move_insn PROTO((rtx, rtx));

/* Emit insns to set X from Y, with no frills.  */
extern rtx emit_move_insn_1 PROTO ((rtx, rtx));

/* Push a block of length SIZE (perhaps variable)
   and return an rtx to address the beginning of the block.  */
extern rtx push_block PROTO((rtx, int, int));

/* Make an operand to push something on the stack.  */
extern rtx gen_push_operand PROTO((void));

#ifdef TREE_CODE
/* Generate code to push something onto the stack, given its mode and type.  */
extern void emit_push_insn PROTO((rtx, enum machine_mode, tree, rtx, int,
				  int, rtx, int, rtx, rtx, int));

/* Emit library call.  */
extern void emit_library_call PVPROTO((rtx orgfun, int no_queue,
  enum machine_mode outmode, int nargs, ...));
extern rtx emit_library_call_value PVPROTO((rtx orgfun, rtx value, int no_queue,
  enum machine_mode outmode, int nargs, ...));

/* Expand an assignment that stores the value of FROM into TO. */
extern rtx expand_assignment PROTO((tree, tree, int, int));

/* Generate code for computing expression EXP,
   and storing the value into TARGET.
   If SUGGEST_REG is nonzero, copy the value through a register
   and return that register, if that is possible.  */
extern rtx store_expr PROTO((tree, rtx, int));
#endif

/* Given an rtx that may include add and multiply operations,
   generate them as insns and return a pseudo-reg containing the value.
   Useful after calling expand_expr with 1 as sum_ok.  */
extern rtx force_operand PROTO((rtx, rtx));

extern void expand_builtin_setjmp_setup PARAMS ((rtx, rtx));
extern void expand_builtin_setjmp_receiver PARAMS ((rtx));

#ifdef TREE_CODE
/* Generate code for computing expression EXP.
   An rtx for the computed value is returned.  The value is never null.
   In the case of a void EXP, const0_rtx is returned.  */
extern rtx expand_expr PROTO((tree, rtx, enum machine_mode,
			      enum expand_modifier));
#endif

/* At the start of a function, record that we have no previously-pushed
   arguments waiting to be popped.  */
extern void init_pending_stack_adjust PROTO((void));

/* When exiting from function, if safe, clear out any pending stack adjust
   so the adjustment won't get done.  */
extern void clear_pending_stack_adjust PROTO((void));

/* Pop any previously-pushed arguments that have not been popped yet.  */
extern void do_pending_stack_adjust PROTO((void));

#ifdef TREE_CODE
/* Generate code to evaluate EXP and jump to LABEL if the value is zero.  */
extern void jumpifnot PROTO((tree, rtx));

/* Generate code to evaluate EXP and jump to LABEL if the value is nonzero.  */
extern void jumpif PROTO((tree, rtx));

/* Generate code to evaluate EXP and jump to IF_FALSE_LABEL if
   the result is zero, or IF_TRUE_LABEL if the result is one.  */
extern void do_jump PROTO((tree, rtx, rtx));
#endif

/* Generate rtl to compare two rtx's, will call emit_cmp_insn.  */
extern rtx compare_from_rtx PROTO((rtx, rtx, enum rtx_code, int,
				   enum machine_mode, rtx, int));

/* Generate a tablejump instruction (used for switch statements).  */
extern void do_tablejump PROTO((rtx, enum machine_mode, rtx, rtx, rtx));

#ifdef TREE_CODE
/* rtl.h and tree.h were included.  */
/* Return an rtx for the size in bytes of the value of an expr.  */
extern rtx expr_size PROTO((tree));

extern rtx lookup_static_chain PROTO((tree));

/* Convert a stack slot address ADDR valid in function FNDECL
   into an address valid in this function (using a static chain).  */
extern rtx fix_lexical_addr PROTO((rtx, tree));

/* Return the address of the trampoline for entering nested fn FUNCTION.  */
extern rtx trampoline_address PROTO((tree));

/* Return an rtx that refers to the value returned by a function
   in its original home.  This becomes invalid if any more code is emitted.  */
extern rtx hard_function_value PROTO((tree, tree));

extern rtx prepare_call_address	PROTO((rtx, tree, rtx *, int));

extern rtx expand_call PROTO((tree, rtx, int));

extern rtx expand_shift PROTO((enum tree_code, enum machine_mode, rtx, tree, rtx, int));
extern rtx expand_divmod PROTO((int, enum tree_code, enum machine_mode, rtx, rtx, rtx, int));
extern void locate_and_pad_parm PROTO((enum machine_mode, tree, int, tree, struct args_size *, struct args_size *, struct args_size *));
extern rtx expand_inline_function PROTO((tree, tree, rtx, int, tree, rtx));
/* Return the CODE_LABEL rtx for a LABEL_DECL, creating it if necessary.  */
extern rtx label_rtx PROTO((tree));
#endif

/* Indicate how an input argument register was promoted.  */
extern rtx promoted_input_arg PROTO((int, enum machine_mode *, int *));

/* Return an rtx like arg but sans any constant terms.
   Returns the original rtx if it has no constant terms.
   The constant terms are added and stored via a second arg.  */
extern rtx eliminate_constant_term PROTO((rtx, rtx *));

/* Convert arg to a valid memory address for specified machine mode,
   by emitting insns to perform arithmetic if nec.  */
extern rtx memory_address PROTO((enum machine_mode, rtx));

/* Like `memory_address' but pretent `flag_force_addr' is 0.  */
extern rtx memory_address_noforce PROTO((enum machine_mode, rtx));

/* Return a memory reference like MEMREF, but with its mode changed
   to MODE and its address changed to ADDR.
   (VOIDmode means don't change the mode.
   NULL for ADDR means don't change the address.)  */
extern rtx change_address PROTO((rtx, enum machine_mode, rtx));

/* Return a memory reference like MEMREF, but which is known to have a
   valid address.  */

extern rtx validize_mem PROTO((rtx));

/* Assemble the static constant template for function entry trampolines.  */
extern rtx assemble_trampoline_template PROTO((void));

/* Return 1 if two rtx's are equivalent in structure and elements.  */
extern int rtx_equal_p PROTO((rtx, rtx));

/* Given rtx, return new rtx whose address won't be affected by
   any side effects.  It has been copied to a new temporary reg.  */
extern rtx stabilize PROTO((rtx));

/* Given an rtx, copy all regs it refers to into new temps
   and return a modified copy that refers to the new temps.  */
extern rtx copy_all_regs PROTO((rtx));

/* Copy given rtx to a new temp reg and return that.  */
extern rtx copy_to_reg PROTO((rtx));

/* Like copy_to_reg but always make the reg Pmode.  */
extern rtx copy_addr_to_reg PROTO((rtx));

/* Like copy_to_reg but always make the reg the specified mode MODE.  */
extern rtx copy_to_mode_reg PROTO((enum machine_mode, rtx));

/* Copy given rtx to given temp reg and return that.  */
extern rtx copy_to_suggested_reg PROTO((rtx, rtx, enum machine_mode));

/* Copy a value to a register if it isn't already a register.
   Args are mode (in case value is a constant) and the value.  */
extern rtx force_reg PROTO((enum machine_mode, rtx));

/* Return given rtx, copied into a new temp reg if it was in memory.  */
extern rtx force_not_mem PROTO((rtx));

#ifdef TREE_CODE
/* Return mode and signedness to use when object is promoted.  */
extern enum machine_mode promote_mode PROTO((tree, enum machine_mode,
					     int *, int));
#endif

/* Remove some bytes from the stack.  An rtx says how many.  */
extern void adjust_stack PROTO((rtx));

/* Add some bytes to the stack.  An rtx says how many.  */
extern void anti_adjust_stack PROTO((rtx));

/* This enum is used for the following two functions.  */
enum save_level {SAVE_BLOCK, SAVE_FUNCTION, SAVE_NONLOCAL};

/* Save the stack pointer at the specified level.  */
extern void emit_stack_save PROTO((enum save_level, rtx *, rtx));

/* Restore the stack pointer from a save area of the specified level.  */
extern void emit_stack_restore PROTO((enum save_level, rtx, rtx));

/* Allocate some space on the stack dynamically and return its address.  An rtx
   says how many bytes.  */
extern rtx allocate_dynamic_stack_space PROTO((rtx, rtx, int));

/* Probe a range of stack addresses from FIRST to FIRST+SIZE, inclusive. 
   FIRST is a constant and size is a Pmode RTX.  These are offsets from the
   current stack pointer.  STACK_GROWS_DOWNWARD says whether to add or
   subtract from the stack.  If SIZE is constant, this is done
   with a fixed number of probes.  Otherwise, we must make a loop.  */
extern void probe_stack_range PROTO((HOST_WIDE_INT, rtx));

/* Return an rtx that refers to the value returned by a library call
   in its original home.  This becomes invalid if any more code is emitted.  */
extern rtx hard_libcall_value PROTO((enum machine_mode));

/* Given an rtx, return an rtx for a value rounded up to a multiple
   of STACK_BOUNDARY / BITS_PER_UNIT.  */
extern rtx round_push PROTO((rtx));

extern rtx store_bit_field PROTO((rtx, int, int, enum machine_mode, rtx, int, int));
extern rtx extract_bit_field PROTO((rtx, int, int, int, rtx, enum machine_mode, enum machine_mode, int, int));
extern rtx expand_mult PROTO((enum machine_mode, rtx, rtx, rtx, int));
extern rtx expand_mult_add PROTO((rtx, rtx, rtx, rtx,enum machine_mode, int));
extern rtx expand_mult_highpart_adjust PROTO((enum machine_mode, rtx, rtx, rtx, rtx, int));

extern rtx assemble_static_space PROTO((int));

/* Hook called by expand_expr for language-specific tree codes.
   It is up to the language front end to install a hook
   if it has any such codes that expand_expr needs to know about.  */
extern rtx (*lang_expand_expr) PROTO ((union tree_node *, rtx,
				       enum machine_mode,
				       enum expand_modifier modifier));

#ifdef TREE_CODE
/* Hook called by output_constant for language-specific tree codes.
   It is up to the language front-end to install a hook if it has any
   such codes that output_constant needs to know about.  Returns a
   language-independent constant equivalent to its input.  */
extern tree (*lang_expand_constant) PROTO((tree));
#endif

extern void init_all_optabs			PROTO ((void));
extern void init_mov_optab			PROTO ((void));
extern void do_jump_by_parts_equality_rtx	PROTO((rtx, rtx, rtx));
extern void do_jump_by_parts_greater_rtx	PROTO ((enum machine_mode, int,
							rtx, rtx, rtx, rtx));

#ifdef TREE_CODE   /* Don't lose if tree.h not included.  */
extern void mark_seen_cases			PROTO ((tree, unsigned char *,
							long, int));
#endif
