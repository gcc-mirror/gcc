/* Definitions for code generation pass of GNU compiler.
   Copyright (C) 1987, 91-99, 2000 Free Software Foundation, Inc.

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

/* Prevent the compiler from deferring stack pops.  See
   inhibit_defer_pop for more information.  */
#define NO_DEFER_POP (inhibit_defer_pop += 1)

/* Allow the compiler to defer stack pops.  See inhibit_defer_pop for
   more information.  */
#define OK_DEFER_POP (inhibit_defer_pop -= 1)

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
#define GEN_FCN(CODE) (insn_data[(int) (CODE)].genfun)
#else
#define GEN_FCN(CODE) (*insn_data[(int) (CODE)].genfun)
#endif

/* Enumeration of valid indexes into optab_table.  */
enum optab_index
{
  OTI_add,
  OTI_sub,

  /* Signed and fp multiply */
  OTI_smul,
  /* Signed multiply, return high word */
  OTI_smul_highpart,
  OTI_umul_highpart,
  /* Signed multiply with result one machine mode wider than args */
  OTI_smul_widen,
  OTI_umul_widen,

  /* Signed divide */
  OTI_sdiv,
  /* Signed divide-and-remainder in one */
  OTI_sdivmod,
  OTI_udiv,
  OTI_udivmod,
  /* Signed remainder */
  OTI_smod,
  OTI_umod,
  /* Optab for floating divide. */
  OTI_flodiv,
  /* Convert float to integer in float fmt */
  OTI_ftrunc,

  /* Logical and */
  OTI_and,
  /* Logical or */
  OTI_ior,
  /* Logical xor */
  OTI_xor,

  /* Arithmetic shift left */
  OTI_ashl,
  /* Logical shift right */
  OTI_lshr,  
  /* Arithmetic shift right */
  OTI_ashr,
  /* Rotate left */
  OTI_rotl,
  /* Rotate right */
  OTI_rotr,
  /* Signed and floating-point minimum value */
  OTI_smin,
  /* Signed and floating-point maximum value */
  OTI_smax,
  /* Unsigned minimum value */
  OTI_umin,
  /* Unsigned maximum value */
  OTI_umax,

  /* Move instruction.  */
  OTI_mov,
  /* Move, preserving high part of register.  */
  OTI_movstrict,

  /* Unary operations */
  /* Negation */
  OTI_neg,
  /* Abs value */
  OTI_abs,
  /* Bitwise not */
  OTI_one_cmpl,
  /* Find first bit set */
  OTI_ffs,
  /* Square root */
  OTI_sqrt,
  /* Sine */
  OTI_sin,
  /* Cosine */
  OTI_cos,

  /* Compare insn; two operands.  */
  OTI_cmp,
  /* Used only for libcalls for unsigned comparisons.  */
  OTI_ucmp,
  /* tst insn; compare one operand against 0 */
  OTI_tst,

  /* String length */
  OTI_strlen,

  /* Combined compare & jump/store flags/move operations.  */
  OTI_cbranch,
  OTI_cmov,
  OTI_cstore,
    
  OTI_MAX
};

extern optab optab_table[OTI_MAX];

#define add_optab (optab_table[OTI_add])
#define sub_optab (optab_table[OTI_sub])
#define smul_optab (optab_table[OTI_smul])
#define smul_highpart_optab (optab_table[OTI_smul_highpart])
#define umul_highpart_optab (optab_table[OTI_umul_highpart])
#define smul_widen_optab (optab_table[OTI_smul_widen])
#define umul_widen_optab (optab_table[OTI_umul_widen])
#define sdiv_optab (optab_table[OTI_sdiv])
#define sdivmod_optab (optab_table[OTI_sdivmod])
#define udiv_optab (optab_table[OTI_udiv])
#define udivmod_optab (optab_table[OTI_udivmod])
#define smod_optab (optab_table[OTI_smod])
#define umod_optab (optab_table[OTI_umod])
#define flodiv_optab (optab_table[OTI_flodiv])
#define ftrunc_optab (optab_table[OTI_ftrunc])
#define and_optab (optab_table[OTI_and])
#define ior_optab (optab_table[OTI_ior])
#define xor_optab (optab_table[OTI_xor])
#define ashl_optab (optab_table[OTI_ashl])
#define lshr_optab (optab_table[OTI_lshr])
#define ashr_optab (optab_table[OTI_ashr])
#define rotl_optab (optab_table[OTI_rotl])
#define rotr_optab (optab_table[OTI_rotr])
#define smin_optab (optab_table[OTI_smin])
#define smax_optab (optab_table[OTI_smax])
#define umin_optab (optab_table[OTI_umin])
#define umax_optab (optab_table[OTI_umax])

#define mov_optab (optab_table[OTI_mov])
#define movstrict_optab (optab_table[OTI_movstrict])

#define neg_optab (optab_table[OTI_neg])
#define abs_optab (optab_table[OTI_abs])
#define one_cmpl_optab (optab_table[OTI_one_cmpl])
#define ffs_optab (optab_table[OTI_ffs])
#define sqrt_optab (optab_table[OTI_sqrt])
#define sin_optab (optab_table[OTI_sin])
#define cos_optab (optab_table[OTI_cos])

#define cmp_optab (optab_table[OTI_cmp])
#define ucmp_optab (optab_table[OTI_ucmp])
#define tst_optab (optab_table[OTI_tst])

#define strlen_optab (optab_table[OTI_strlen])

#define cbranch_optab (optab_table[OTI_cbranch])
#define cmov_optab (optab_table[OTI_cmov])
#define cstore_optab (optab_table[OTI_cstore])

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

/* Enumeration of indexes into libfunc_table.  */
enum libfunc_index
{
  LTI_extendsfdf2,
  LTI_extendsfxf2,
  LTI_extendsftf2,
  LTI_extenddfxf2,
  LTI_extenddftf2,

  LTI_truncdfsf2,
  LTI_truncxfsf2,
  LTI_trunctfsf2,
  LTI_truncxfdf2,
  LTI_trunctfdf2,

  LTI_memcpy,
  LTI_bcopy,
  LTI_memcmp,
  LTI_bcmp,
  LTI_memset,
  LTI_bzero,

  LTI_throw,
  LTI_rethrow,
  LTI_sjthrow,
  LTI_sjpopnthrow,
  LTI_terminate,
  LTI_setjmp,
  LTI_longjmp,
  LTI_eh_rtime_match,

  LTI_eqhf2,
  LTI_nehf2,
  LTI_gthf2,
  LTI_gehf2,
  LTI_lthf2,
  LTI_lehf2,
  LTI_unordhf2,

  LTI_eqsf2,
  LTI_nesf2,
  LTI_gtsf2,
  LTI_gesf2,
  LTI_ltsf2,
  LTI_lesf2,
  LTI_unordsf2,

  LTI_eqdf2,
  LTI_nedf2,
  LTI_gtdf2,
  LTI_gedf2,
  LTI_ltdf2,
  LTI_ledf2,
  LTI_unorddf2,

  LTI_eqxf2,
  LTI_nexf2,
  LTI_gtxf2,
  LTI_gexf2,
  LTI_ltxf2,
  LTI_lexf2,
  LTI_unordxf2,

  LTI_eqtf2,
  LTI_netf2,
  LTI_gttf2,
  LTI_getf2,
  LTI_lttf2,
  LTI_letf2,
  LTI_unordtf2,

  LTI_floatsisf,
  LTI_floatdisf,
  LTI_floattisf,

  LTI_floatsidf,
  LTI_floatdidf,
  LTI_floattidf,

  LTI_floatsixf,
  LTI_floatdixf,
  LTI_floattixf,

  LTI_floatsitf,
  LTI_floatditf,
  LTI_floattitf,

  LTI_fixsfsi,
  LTI_fixsfdi,
  LTI_fixsfti,

  LTI_fixdfsi,
  LTI_fixdfdi,
  LTI_fixdfti,

  LTI_fixxfsi,
  LTI_fixxfdi,
  LTI_fixxfti,

  LTI_fixtfsi,
  LTI_fixtfdi,
  LTI_fixtfti,

  LTI_fixunssfsi,
  LTI_fixunssfdi,
  LTI_fixunssfti,

  LTI_fixunsdfsi,
  LTI_fixunsdfdi,
  LTI_fixunsdfti,

  LTI_fixunsxfsi,
  LTI_fixunsxfdi,
  LTI_fixunsxfti,

  LTI_fixunstfsi,
  LTI_fixunstfdi,
  LTI_fixunstfti,

  LTI_chkr_check_addr,
  LTI_chkr_set_right,
  LTI_chkr_copy_bitmap,
  LTI_chkr_check_exec,
  LTI_chkr_check_str,

  LTI_profile_function_entry,
  LTI_profile_function_exit,

  LTI_MAX
};

/* SYMBOL_REF rtx's for the library functions that are called
   implicitly and not via optabs.  */
extern rtx libfunc_table[LTI_MAX];

/* Accessor macros for libfunc_table.  */
#define extendsfdf2_libfunc	(libfunc_table[LTI_extendsfdf2])
#define extendsfxf2_libfunc	(libfunc_table[LTI_extendsfxf2])
#define extendsftf2_libfunc	(libfunc_table[LTI_extendsftf2])
#define extenddfxf2_libfunc	(libfunc_table[LTI_extenddfxf2])
#define extenddftf2_libfunc	(libfunc_table[LTI_extenddftf2])

#define truncdfsf2_libfunc	(libfunc_table[LTI_truncdfsf2])
#define truncxfsf2_libfunc	(libfunc_table[LTI_truncxfsf2])
#define trunctfsf2_libfunc	(libfunc_table[LTI_trunctfsf2])
#define truncxfdf2_libfunc	(libfunc_table[LTI_truncxfdf2])
#define trunctfdf2_libfunc	(libfunc_table[LTI_trunctfdf2])

#define memcpy_libfunc	(libfunc_table[LTI_memcpy])
#define bcopy_libfunc	(libfunc_table[LTI_bcopy])
#define memcmp_libfunc	(libfunc_table[LTI_memcmp])
#define bcmp_libfunc	(libfunc_table[LTI_bcmp])
#define memset_libfunc	(libfunc_table[LTI_memset])
#define bzero_libfunc	(libfunc_table[LTI_bzero])

#define throw_libfunc	(libfunc_table[LTI_throw])
#define rethrow_libfunc	(libfunc_table[LTI_rethrow])
#define sjthrow_libfunc	(libfunc_table[LTI_sjthrow])
#define sjpopnthrow_libfunc	(libfunc_table[LTI_sjpopnthrow])
#define terminate_libfunc	(libfunc_table[LTI_terminate])
#define setjmp_libfunc	(libfunc_table[LTI_setjmp])
#define longjmp_libfunc	(libfunc_table[LTI_longjmp])
#define eh_rtime_match_libfunc	(libfunc_table[LTI_eh_rtime_match])

#define eqhf2_libfunc	(libfunc_table[LTI_eqhf2])
#define nehf2_libfunc	(libfunc_table[LTI_nehf2])
#define gthf2_libfunc	(libfunc_table[LTI_gthf2])
#define gehf2_libfunc	(libfunc_table[LTI_gehf2])
#define lthf2_libfunc	(libfunc_table[LTI_lthf2])
#define lehf2_libfunc	(libfunc_table[LTI_lehf2])
#define unordhf2_libfunc	(libfunc_table[LTI_unordhf2])

#define eqsf2_libfunc	(libfunc_table[LTI_eqsf2])
#define nesf2_libfunc	(libfunc_table[LTI_nesf2])
#define gtsf2_libfunc	(libfunc_table[LTI_gtsf2])
#define gesf2_libfunc	(libfunc_table[LTI_gesf2])
#define ltsf2_libfunc	(libfunc_table[LTI_ltsf2])
#define lesf2_libfunc	(libfunc_table[LTI_lesf2])
#define unordsf2_libfunc	(libfunc_table[LTI_unordsf2])

#define eqdf2_libfunc	(libfunc_table[LTI_eqdf2])
#define nedf2_libfunc	(libfunc_table[LTI_nedf2])
#define gtdf2_libfunc	(libfunc_table[LTI_gtdf2])
#define gedf2_libfunc	(libfunc_table[LTI_gedf2])
#define ltdf2_libfunc	(libfunc_table[LTI_ltdf2])
#define ledf2_libfunc	(libfunc_table[LTI_ledf2])
#define unorddf2_libfunc	(libfunc_table[LTI_unorddf2])

#define eqxf2_libfunc	(libfunc_table[LTI_eqxf2])
#define nexf2_libfunc	(libfunc_table[LTI_nexf2])
#define gtxf2_libfunc	(libfunc_table[LTI_gtxf2])
#define gexf2_libfunc	(libfunc_table[LTI_gexf2])
#define ltxf2_libfunc	(libfunc_table[LTI_ltxf2])
#define lexf2_libfunc	(libfunc_table[LTI_lexf2])
#define unordxf2_libfunc	(libfunc_table[LTI_unordxf2])

#define eqtf2_libfunc	(libfunc_table[LTI_eqtf2])
#define netf2_libfunc	(libfunc_table[LTI_netf2])
#define gttf2_libfunc	(libfunc_table[LTI_gttf2])
#define getf2_libfunc	(libfunc_table[LTI_getf2])
#define lttf2_libfunc	(libfunc_table[LTI_lttf2])
#define letf2_libfunc	(libfunc_table[LTI_letf2])
#define unordtf2_libfunc	(libfunc_table[LTI_unordtf2])

#define floatsisf_libfunc	(libfunc_table[LTI_floatsisf])
#define floatdisf_libfunc	(libfunc_table[LTI_floatdisf])
#define floattisf_libfunc	(libfunc_table[LTI_floattisf])

#define floatsidf_libfunc	(libfunc_table[LTI_floatsidf])
#define floatdidf_libfunc	(libfunc_table[LTI_floatdidf])
#define floattidf_libfunc	(libfunc_table[LTI_floattidf])

#define floatsixf_libfunc	(libfunc_table[LTI_floatsixf])
#define floatdixf_libfunc	(libfunc_table[LTI_floatdixf])
#define floattixf_libfunc	(libfunc_table[LTI_floattixf])

#define floatsitf_libfunc	(libfunc_table[LTI_floatsitf])
#define floatditf_libfunc	(libfunc_table[LTI_floatditf])
#define floattitf_libfunc	(libfunc_table[LTI_floattitf])

#define fixsfsi_libfunc	(libfunc_table[LTI_fixsfsi])
#define fixsfdi_libfunc	(libfunc_table[LTI_fixsfdi])
#define fixsfti_libfunc	(libfunc_table[LTI_fixsfti])

#define fixdfsi_libfunc	(libfunc_table[LTI_fixdfsi])
#define fixdfdi_libfunc	(libfunc_table[LTI_fixdfdi])
#define fixdfti_libfunc	(libfunc_table[LTI_fixdfti])

#define fixxfsi_libfunc	(libfunc_table[LTI_fixxfsi])
#define fixxfdi_libfunc	(libfunc_table[LTI_fixxfdi])
#define fixxfti_libfunc	(libfunc_table[LTI_fixxfti])

#define fixtfsi_libfunc	(libfunc_table[LTI_fixtfsi])
#define fixtfdi_libfunc	(libfunc_table[LTI_fixtfdi])
#define fixtfti_libfunc	(libfunc_table[LTI_fixtfti])

#define fixunssfsi_libfunc	(libfunc_table[LTI_fixunssfsi])
#define fixunssfdi_libfunc	(libfunc_table[LTI_fixunssfdi])
#define fixunssfti_libfunc	(libfunc_table[LTI_fixunssfti])

#define fixunsdfsi_libfunc	(libfunc_table[LTI_fixunsdfsi])
#define fixunsdfdi_libfunc	(libfunc_table[LTI_fixunsdfdi])
#define fixunsdfti_libfunc	(libfunc_table[LTI_fixunsdfti])

#define fixunsxfsi_libfunc	(libfunc_table[LTI_fixunsxfsi])
#define fixunsxfdi_libfunc	(libfunc_table[LTI_fixunsxfdi])
#define fixunsxfti_libfunc	(libfunc_table[LTI_fixunsxfti])

#define fixunstfsi_libfunc	(libfunc_table[LTI_fixunstfsi])
#define fixunstfdi_libfunc	(libfunc_table[LTI_fixunstfdi])
#define fixunstfti_libfunc	(libfunc_table[LTI_fixunstfti])

#define chkr_check_addr_libfunc	(libfunc_table[LTI_chkr_check_addr])
#define chkr_set_right_libfunc	(libfunc_table[LTI_chkr_set_right])
#define chkr_copy_bitmap_libfunc	(libfunc_table[LTI_chkr_copy_bitmap])
#define chkr_check_exec_libfunc	(libfunc_table[LTI_chkr_check_exec])
#define chkr_check_str_libfunc	(libfunc_table[LTI_chkr_check_str])

#define profile_function_entry_libfunc	(libfunc_table[LTI_profile_function_entry])
#define profile_function_exit_libfunc	(libfunc_table[LTI_profile_function_exit])

typedef rtx (*rtxfun) PARAMS ((rtx));

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
extern rtx expand_binop PARAMS ((enum machine_mode, optab, rtx, rtx, rtx,
				 int, enum optab_methods));

/* Expand a binary operation with both signed and unsigned forms.  */
extern rtx sign_expand_binop PARAMS ((enum machine_mode, optab, optab, rtx,
				      rtx, rtx, int, enum optab_methods));

/* Generate code to perform an operation on two operands with two results.  */
extern int expand_twoval_binop PARAMS ((optab, rtx, rtx, rtx, rtx, int));

/* Expand a unary arithmetic operation given optab rtx operand.  */
extern rtx expand_unop PARAMS ((enum machine_mode, optab, rtx, rtx, int));

/* Expand the absolute value operation.  */
extern rtx expand_abs PARAMS ((enum machine_mode, rtx, rtx, int));

/* Expand the complex absolute value operation.  */
extern rtx expand_complex_abs PARAMS ((enum machine_mode, rtx, rtx, int));

/* Generate an instruction with a given INSN_CODE with an output and
   an input.  */
extern void emit_unop_insn PARAMS ((int, rtx, rtx, enum rtx_code));

/* Emit code to perform a series of operations on a multi-word quantity, one
   word at a time.  */
extern rtx emit_no_conflict_block PARAMS ((rtx, rtx, rtx, rtx, rtx));

/* Emit code to make a call to a constant function or a library call. */
extern void emit_libcall_block PARAMS ((rtx, rtx, rtx, rtx));

/* Emit one rtl instruction to store zero in specified rtx.  */
extern void emit_clr_insn PARAMS ((rtx));

/* Emit one rtl insn to store 1 in specified rtx assuming it contains 0.  */
extern void emit_0_to_1_insn PARAMS ((rtx));

/* Emit one rtl insn to compare two rtx's.  */
extern void emit_cmp_insn PARAMS ((rtx, rtx, enum rtx_code, rtx,
				   enum machine_mode, int, int));

/* Emit a pair of rtl insns to compare two rtx's and to jump 
   to a label if the comparison is true.  */
extern void emit_cmp_and_jump_insns PARAMS ((rtx, rtx, enum rtx_code, rtx,
					     enum machine_mode, int, int, rtx));

/* The various uses that a comparison can have; used by can_compare_p:
   jumps, conditional moves, store flag operations.  */
enum can_compare_purpose
{
  ccp_jump,
  ccp_cmov,
  ccp_store_flag
};

/* Nonzero if a compare of mode MODE can be done straightforwardly
   (without splitting it into pieces).  */
extern int can_compare_p PARAMS ((enum rtx_code, enum machine_mode,
				  enum can_compare_purpose));

extern void prepare_cmp_insn PARAMS ((rtx *, rtx *, enum rtx_code *, rtx,
				      enum machine_mode *, int *, int,
				      enum can_compare_purpose));

extern rtx prepare_operand PARAMS ((int, rtx, int, enum machine_mode,
				    enum machine_mode, int));

/* Generate code to indirectly jump to a location given in the rtx LOC.  */
extern void emit_indirect_jump PARAMS ((rtx));

#ifdef HAVE_conditional_move
/* Emit a conditional move operation.  */
rtx emit_conditional_move PARAMS ((rtx, enum rtx_code, rtx, rtx,
				   enum machine_mode, rtx, rtx,
				   enum machine_mode, int));

/* Return non-zero if the conditional move is supported.  */
int can_conditionally_move_p PARAMS ((enum machine_mode mode));

#endif

/* Create but don't emit one rtl instruction to add one rtx into another.
   Modes must match; operands must meet the operation's predicates.
   Likewise for subtraction and for just copying.
   These do not call protect_from_queue; caller must do so.  */
extern rtx gen_add2_insn PARAMS ((rtx, rtx));
extern rtx gen_sub2_insn PARAMS ((rtx, rtx));
extern rtx gen_move_insn PARAMS ((rtx, rtx));
extern int have_add2_insn PARAMS ((enum machine_mode));
extern int have_sub2_insn PARAMS ((enum machine_mode));

/* Return the INSN_CODE to use for an extend operation.  */
extern enum insn_code can_extend_p PARAMS ((enum machine_mode,
					    enum machine_mode, int));

/* Generate the body of an insn to extend Y (with mode MFROM)
   into X (with mode MTO).  Do zero-extension if UNSIGNEDP is nonzero.  */
extern rtx gen_extend_insn PARAMS ((rtx, rtx, enum machine_mode,
				    enum machine_mode, int));

/* Initialize the tables that control conversion between fixed and
   floating values.  */
extern void init_fixtab PARAMS ((void));
extern void init_floattab PARAMS ((void));

/* Generate code for a FLOAT_EXPR.  */
extern void expand_float PARAMS ((rtx, rtx, int));

/* Generate code for a FIX_EXPR.  */
extern void expand_fix PARAMS ((rtx, rtx, int));

/* Call this to initialize an optab function entry.  */
extern rtx init_one_libfunc PARAMS ((const char *));

/* Call this once to initialize the contents of the optabs
   appropriately for the current target machine.  */
extern void init_optabs	PARAMS ((void));

/* Functions from expmed.c:  */

/* Arguments MODE, RTX: return an rtx for the negation of that value.
   May emit insns.  */
extern rtx negate_rtx PARAMS ((enum machine_mode, rtx));

/* Expand a logical AND operation.  */
extern rtx expand_and PARAMS ((rtx, rtx, rtx));

/* Emit a store-flag operation.  */
extern rtx emit_store_flag PARAMS ((rtx, enum rtx_code, rtx, rtx,
				    enum machine_mode, int, int));

/* Like emit_store_flag, but always succeeds.  */
extern rtx emit_store_flag_force PARAMS ((rtx, enum rtx_code, rtx, rtx,
					  enum machine_mode, int, int));

/* Functions from loop.c:  */

/* Given a JUMP_INSN, return a description of the test being made.  */
extern rtx get_condition PARAMS ((rtx, rtx *));

/* Generate a conditional trap instruction.  */
extern rtx gen_cond_trap PARAMS ((enum rtx_code, rtx, rtx, rtx));

/* Functions from builtins.c:  */
#ifdef TREE_CODE
extern rtx expand_builtin PARAMS ((tree, rtx, rtx, enum machine_mode, int));
extern tree expand_tree_builtin PARAMS ((tree, tree, tree));
extern void std_expand_builtin_va_start PARAMS ((int, tree, rtx));
extern rtx std_expand_builtin_va_arg PARAMS ((tree, tree));
extern rtx expand_builtin_va_arg PARAMS ((tree, tree));
#endif

extern rtx expand_builtin_setjmp PARAMS ((rtx, rtx, rtx, rtx));
extern void expand_builtin_longjmp PARAMS ((rtx, rtx));
extern rtx expand_builtin_saveregs PARAMS ((void));
extern int get_varargs_alias_set PARAMS ((void));

/* Functions from expr.c:  */

/* This is run once per compilation to set up which modes can be used
   directly in memory and to initialize the block move optab.  */
extern void init_expr_once PARAMS ((void));

/* This is run at the start of compiling a function.  */
extern void init_expr PARAMS ((void));

/* This function is run once to initialize stor-layout.c.  */

extern void init_stor_layout_once PARAMS ((void));

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
extern rtx convert_modes PARAMS ((enum machine_mode, enum machine_mode, rtx, int));

/* Emit code to move a block Y to a block X.  */
extern rtx emit_block_move PARAMS ((rtx, rtx, rtx, int));

/* Copy all or part of a value X into registers starting at REGNO.
   The number of registers to be filled is NREGS.  */
extern void move_block_to_reg PARAMS ((int, rtx, int, enum machine_mode));

/* Copy all or part of a BLKmode value X out of registers starting at REGNO.
   The number of registers to be filled is NREGS.  */
extern void move_block_from_reg PARAMS ((int, rtx, int, int));

/* Load a BLKmode value into non-consecutive registers represented by a
   PARALLEL.  */
extern void emit_group_load PARAMS ((rtx, rtx, int, int));
/* Store a BLKmode value from non-consecutive registers represented by a
   PARALLEL.  */
extern void emit_group_store PARAMS ((rtx, rtx, int, int));

#ifdef TREE_CODE
/* Copy BLKmode object from a set of registers. */
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
   If OBJECT has BLKmode, SIZE is its length in bytes and ALIGN is its
   alignment.  */
extern rtx clear_storage PARAMS ((rtx, rtx, int));

/* Emit insns to set X from Y.  */
extern rtx emit_move_insn PARAMS ((rtx, rtx));

/* Emit insns to set X from Y, with no frills.  */
extern rtx emit_move_insn_1 PARAMS ((rtx, rtx));

/* Push a block of length SIZE (perhaps variable)
   and return an rtx to address the beginning of the block.  */
extern rtx push_block PARAMS ((rtx, int, int));

/* Make an operand to push something on the stack.  */
extern rtx gen_push_operand PARAMS ((void));

#ifdef TREE_CODE
/* Generate code to push something onto the stack, given its mode and type.  */
extern void emit_push_insn PARAMS ((rtx, enum machine_mode, tree, rtx, int,
				    int, rtx, int, rtx, rtx, int, rtx));

/* Emit library call.  */
extern void emit_library_call PARAMS ((rtx orgfun, int no_queue,
  enum machine_mode outmode, int nargs, ...));
extern rtx emit_library_call_value PARAMS ((rtx orgfun, rtx value, int no_queue,
  enum machine_mode outmode, int nargs, ...));

/* Expand an assignment that stores the value of FROM into TO. */
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

#ifdef TREE_CODE
/* Generate code for computing expression EXP.
   An rtx for the computed value is returned.  The value is never null.
   In the case of a void EXP, const0_rtx is returned.  */
extern rtx expand_expr PARAMS ((tree, rtx, enum machine_mode,
				enum expand_modifier));
#endif

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
				     enum machine_mode, rtx, int));
extern void do_compare_rtx_and_jump PARAMS ((rtx, rtx, enum rtx_code, int,
					     enum machine_mode, rtx, int,
					     rtx, rtx));

/* Generate a tablejump instruction (used for switch statements).  */
extern void do_tablejump PARAMS ((rtx, enum machine_mode, rtx, rtx, rtx));

#ifdef TREE_CODE
/* rtl.h and tree.h were included.  */
/* Return an rtx for the size in bytes of the value of an expr.  */
extern rtx expr_size PARAMS ((tree));

extern rtx lookup_static_chain PARAMS ((tree));

/* Convert a stack slot address ADDR valid in function FNDECL
   into an address valid in this function (using a static chain).  */
extern rtx fix_lexical_addr PARAMS ((rtx, tree));

/* Return the address of the trampoline for entering nested fn FUNCTION.  */
extern rtx trampoline_address PARAMS ((tree));

/* Return an rtx that refers to the value returned by a function
   in its original home.  This becomes invalid if any more code is emitted.  */
extern rtx hard_function_value PARAMS ((tree, tree, int));

extern rtx prepare_call_address	PARAMS ((rtx, tree, rtx *, int));

extern rtx expand_call PARAMS ((tree, rtx, int));

extern rtx expand_shift PARAMS ((enum tree_code, enum machine_mode, rtx, tree, rtx, int));
extern rtx expand_divmod PARAMS ((int, enum tree_code, enum machine_mode, rtx, rtx, rtx, int));
extern void locate_and_pad_parm PARAMS ((enum machine_mode, tree, int, tree, struct args_size *, struct args_size *, struct args_size *, struct args_size *));
extern rtx expand_inline_function PARAMS ((tree, tree, rtx, int, tree, rtx));
/* Return the CODE_LABEL rtx for a LABEL_DECL, creating it if necessary.  */
extern rtx label_rtx PARAMS ((tree));
#endif

/* Indicate how an input argument register was promoted.  */
extern rtx promoted_input_arg PARAMS ((int, enum machine_mode *, int *));

/* Return an rtx like arg but sans any constant terms.
   Returns the original rtx if it has no constant terms.
   The constant terms are added and stored via a second arg.  */
extern rtx eliminate_constant_term PARAMS ((rtx, rtx *));

/* Convert arg to a valid memory address for specified machine mode,
   by emitting insns to perform arithmetic if nec.  */
extern rtx memory_address PARAMS ((enum machine_mode, rtx));

/* Like `memory_address' but pretent `flag_force_addr' is 0.  */
extern rtx memory_address_noforce PARAMS ((enum machine_mode, rtx));

/* Return a memory reference like MEMREF, but with its mode changed
   to MODE and its address changed to ADDR.
   (VOIDmode means don't change the mode.
   NULL for ADDR means don't change the address.)  */
extern rtx change_address PARAMS ((rtx, enum machine_mode, rtx));

/* Return a memory reference like MEMREF, but which is known to have a
   valid address.  */

extern rtx validize_mem PARAMS ((rtx));

/* Assemble the static constant template for function entry trampolines.  */
extern rtx assemble_trampoline_template PARAMS ((void));

/* Return 1 if two rtx's are equivalent in structure and elements.  */
extern int rtx_equal_p PARAMS ((rtx, rtx));

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

extern rtx store_bit_field PARAMS ((rtx, int, int, enum machine_mode, rtx, int, int));
extern rtx extract_bit_field PARAMS ((rtx, int, int, int, rtx, enum machine_mode, enum machine_mode, int, int));
extern rtx expand_mult PARAMS ((enum machine_mode, rtx, rtx, rtx, int));
extern rtx expand_mult_add PARAMS ((rtx, rtx, rtx, rtx,enum machine_mode, int));
extern rtx expand_mult_highpart_adjust PARAMS ((enum machine_mode, rtx, rtx, rtx, rtx, int));

extern rtx assemble_static_space PARAMS ((int));

/* Hook called by expand_expr for language-specific tree codes.
   It is up to the language front end to install a hook
   if it has any such codes that expand_expr needs to know about.  */
extern rtx (*lang_expand_expr) PARAMS ((union tree_node *, rtx,
					enum machine_mode,
					enum expand_modifier modifier));

#ifdef TREE_CODE
/* Hook called by output_constant for language-specific tree codes.
   It is up to the language front-end to install a hook if it has any
   such codes that output_constant needs to know about.  Returns a
   language-independent constant equivalent to its input.  */
extern tree (*lang_expand_constant) PARAMS ((tree));
#endif

extern void init_all_optabs			PARAMS ((void));
extern void do_jump_by_parts_equality_rtx	PARAMS ((rtx, rtx, rtx));
extern void do_jump_by_parts_greater_rtx	PARAMS ((enum machine_mode,
							 int, rtx, rtx, rtx,
							 rtx));

#ifdef TREE_CODE   /* Don't lose if tree.h not included.  */
extern void mark_seen_cases			PARAMS ((tree, unsigned char *,
							 long, int));
#endif
