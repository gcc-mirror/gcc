/* Register Transfer Language (RTL) definitions for GNU C-Compiler
   Copyright (C) 1987, 1991, 1992 Free Software Foundation, Inc.

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


#include "machmode.h"

#undef FFS  /* Some systems predefine this symbol; don't let it interfere.  */
#undef FLOAT /* Likewise.  */

/* Register Transfer Language EXPRESSIONS CODES */

#define RTX_CODE	enum rtx_code
enum rtx_code  {

#define DEF_RTL_EXPR(ENUM, NAME, FORMAT, CLASS)   ENUM ,
#include "rtl.def"		/* rtl expressions are documented here */
#undef DEF_RTL_EXPR

  LAST_AND_UNUSED_RTX_CODE};	/* A convenient way to get a value for
				   NUM_RTX_CODE.
				   Assumes default enum value assignment.  */

#define NUM_RTX_CODE ((int)LAST_AND_UNUSED_RTX_CODE)
				/* The cast here, saves many elsewhere.  */

extern int rtx_length[];
#define GET_RTX_LENGTH(CODE)		(rtx_length[(int)(CODE)])

extern char *rtx_name[];
#define GET_RTX_NAME(CODE)		(rtx_name[(int)(CODE)])

extern char *rtx_format[];
#define GET_RTX_FORMAT(CODE)		(rtx_format[(int)(CODE)])

extern char rtx_class[];
#define GET_RTX_CLASS(CODE)		(rtx_class[(int)(CODE)])

/* Common union for an element of an rtx.  */

typedef union rtunion_def
{
  HOST_WIDE_INT rtwint;
  int rtint;
  char *rtstr;
  struct rtx_def *rtx;
  struct rtvec_def *rtvec;
  enum machine_mode rttype;
} rtunion;

/* RTL expression ("rtx").  */

typedef struct rtx_def
{
#ifdef ONLY_INT_FIELDS
#ifdef CODE_FIELD_BUG
  unsigned int code : 16;
#else
  unsigned short code;
#endif
#else
  /* The kind of expression this is.  */
  enum rtx_code code : 16;
#endif
  /* The kind of value the expression has.  */
#ifdef ONLY_INT_FIELDS
  int mode : 8;
#else
  enum machine_mode mode : 8;
#endif
  /* 1 in an INSN if it can alter flow of control
     within this function.  Not yet used!  */
  unsigned int jump : 1;
  /* 1 in an INSN if it can call another function.  Not yet used!  */
  unsigned int call : 1;
  /* 1 in a MEM or REG if value of this expression will never change
     during the current function, even though it is not
     manifestly constant.
     1 in a SUBREG if it is from a promoted variable that is unsigned.
     1 in a SYMBOL_REF if it addresses something in the per-function
     constants pool.
     1 in a CALL_INSN if it is a const call.
     1 in a JUMP_INSN if it is a branch that should be annulled.  Valid from
     reorg until end of compilation; cleared before used.  */
  unsigned int unchanging : 1;
  /* 1 in a MEM expression if contents of memory are volatile.
     1 in an INSN, CALL_INSN, JUMP_INSN, CODE_LABEL or BARRIER
     if it is deleted.
     1 in a REG expression if corresponds to a variable declared by the user.
     0 for an internally generated temporary.
     In a SYMBOL_REF, this flag is used for machine-specific purposes.
     In a LABEL_REF or in a REG_LABEL note, this is LABEL_REF_NONLOCAL_P.  */
  unsigned int volatil : 1;
  /* 1 in a MEM referring to a field of a structure (not a union!).
     0 if the MEM was a variable or the result of a * operator in C;
     1 if it was the result of a . or -> operator (on a struct) in C.
     1 in a REG if the register is used only in exit code a loop.
     1 in a SUBREG expression if was generated from a variable with a 
     promoted mode.
     1 in a CODE_LABEL if the label is used for nonlocal gotos
     and must not be deleted even if its count is zero.
     1 in a LABEL_REF if this is a reference to a label outside the
     current loop.
     1 in an INSN, JUMP_INSN, or CALL_INSN if this insn must be scheduled
     together with the preceding insn.  Valid only within sched.
     1 in an INSN, JUMP_INSN, or CALL_INSN if insn is in a delay slot and
     from the target of a branch.  Valid from reorg until end of compilation;
     cleared before used.  */
  unsigned int in_struct : 1;
  /* 1 if this rtx is used.  This is used for copying shared structure.
     See `unshare_all_rtl'.
     In a REG, this is not needed for that purpose, and used instead 
     in `leaf_renumber_regs_insn'.
     In a SYMBOL_REF, means that emit_library_call
     has used it as the function.  */
  unsigned int used : 1;
  /* Nonzero if this rtx came from procedure integration.
     In a REG, nonzero means this reg refers to the return value
     of the current function.  */
  unsigned integrated : 1;
  /* The first element of the operands of this rtx.
     The number of operands and their types are controlled
     by the `code' field, according to rtl.def.  */
  rtunion fld[1];
} *rtx;

/* Add prototype support.  */
#ifndef PROTO
#if defined (USE_PROTOTYPES) ? USE_PROTOTYPES : defined (__STDC__)
#define PROTO(ARGS) ARGS
#else
#define PROTO(ARGS) ()
#endif
#endif

#define NULL_RTX (rtx) 0

/* Define a generic NULL if one hasn't already been defined.  */

#ifndef NULL
#define NULL 0
#endif

#ifndef GENERIC_PTR
#if defined (USE_PROTOTYPES) ? USE_PROTOTYPES : defined (__STDC__)
#define GENERIC_PTR void *
#else
#define GENERIC_PTR char *
#endif
#endif

#ifndef NULL_PTR
#define NULL_PTR ((GENERIC_PTR)0)
#endif

/* Define macros to access the `code' field of the rtx.  */

#ifdef SHORT_ENUM_BUG
#define GET_CODE(RTX)		((enum rtx_code) ((RTX)->code))
#define PUT_CODE(RTX, CODE)	((RTX)->code = ((short) (CODE)))
#else
#define GET_CODE(RTX)		((RTX)->code)
#define PUT_CODE(RTX, CODE)	((RTX)->code = (CODE))
#endif

#define GET_MODE(RTX)		((RTX)->mode)
#define PUT_MODE(RTX, MODE)	((RTX)->mode = (MODE))

#define RTX_INTEGRATED_P(RTX) ((RTX)->integrated)
#define RTX_UNCHANGING_P(RTX) ((RTX)->unchanging)

/* RTL vector.  These appear inside RTX's when there is a need
   for a variable number of things.  The principle use is inside
   PARALLEL expressions.  */

typedef struct rtvec_def{
  unsigned num_elem;		/* number of elements */
  rtunion elem[1];
} *rtvec;

#define NULL_RTVEC (rtvec) 0

#define GET_NUM_ELEM(RTVEC)		((RTVEC)->num_elem)
#define PUT_NUM_ELEM(RTVEC, NUM)	((RTVEC)->num_elem = (unsigned) NUM)

#define RTVEC_ELT(RTVEC, I)  ((RTVEC)->elem[(I)].rtx)

/* 1 if X is a REG.  */

#define REG_P(X) (GET_CODE (X) == REG)

/* 1 if X is a constant value that is an integer.  */

#define CONSTANT_P(X)   \
  (GET_CODE (X) == LABEL_REF || GET_CODE (X) == SYMBOL_REF		\
   || GET_CODE (X) == CONST_INT || GET_CODE (X) == CONST_DOUBLE		\
   || GET_CODE (X) == CONST || GET_CODE (X) == HIGH)

/* General accessor macros for accessing the fields of an rtx.  */

#define XEXP(RTX, N)	((RTX)->fld[N].rtx)
#define XINT(RTX, N)	((RTX)->fld[N].rtint)
#define XWINT(RTX, N)	((RTX)->fld[N].rtwint)
#define XSTR(RTX, N)	((RTX)->fld[N].rtstr)
#define XVEC(RTX, N)	((RTX)->fld[N].rtvec)
#define XVECLEN(RTX, N)	((RTX)->fld[N].rtvec->num_elem)
#define XVECEXP(RTX,N,M)((RTX)->fld[N].rtvec->elem[M].rtx)

/* ACCESS MACROS for particular fields of insns.  */

/* Holds a unique number for each insn.
   These are not necessarily sequentially increasing.  */
#define INSN_UID(INSN)	((INSN)->fld[0].rtint)

/* Chain insns together in sequence.  */
#define PREV_INSN(INSN)	((INSN)->fld[1].rtx)
#define NEXT_INSN(INSN)	((INSN)->fld[2].rtx)

/* The body of an insn.  */
#define PATTERN(INSN)	((INSN)->fld[3].rtx)

/* Code number of instruction, from when it was recognized.
   -1 means this instruction has not been recognized yet.  */
#define INSN_CODE(INSN) ((INSN)->fld[4].rtint)

/* Set up in flow.c; empty before then.
   Holds a chain of INSN_LIST rtx's whose first operands point at
   previous insns with direct data-flow connections to this one.
   That means that those insns set variables whose next use is in this insn.
   They are always in the same basic block as this insn.  */
#define LOG_LINKS(INSN)		((INSN)->fld[5].rtx)

/* 1 if insn has been deleted.  */
#define INSN_DELETED_P(INSN) ((INSN)->volatil)

/* 1 if insn is a call to a const function.  */
#define CONST_CALL_P(INSN) ((INSN)->unchanging)

/* 1 if insn is a branch that should not unconditionally execute its
   delay slots, i.e., it is an annulled branch.   */
#define INSN_ANNULLED_BRANCH_P(INSN) ((INSN)->unchanging)

/* 1 if insn is in a delay slot and is from the target of the branch.  If
   the branch insn has INSN_ANNULLED_BRANCH_P set, this insn should only be
   executed if the branch is taken.  For annulled branches with this bit
   clear, the insn should be executed only if the branch is not taken.  */
#define INSN_FROM_TARGET_P(INSN) ((INSN)->in_struct)

/* Holds a list of notes on what this insn does to various REGs.
   It is a chain of EXPR_LIST rtx's, where the second operand
   is the chain pointer and the first operand is the REG being described.
   The mode field of the EXPR_LIST contains not a real machine mode
   but a value that says what this note says about the REG:
     REG_DEAD means that the value in REG dies in this insn (i.e., it is
   not needed past this insn).  If REG is set in this insn, the REG_DEAD
   note may, but need not, be omitted.
     REG_INC means that the REG is autoincremented or autodecremented.
     REG_EQUIV describes the insn as a whole; it says that the
   insn sets a register to a constant value or to be equivalent to
   a memory address.  If the
   register is spilled to the stack then the constant value
   should be substituted for it.  The contents of the REG_EQUIV
   is the constant value or memory address, which may be different
   from the source of the SET although it has the same value. 
     REG_EQUAL is like REG_EQUIV except that the destination
   is only momentarily equal to the specified rtx.  Therefore, it
   cannot be used for substitution; but it can be used for cse.
     REG_RETVAL means that this insn copies the return-value of
   a library call out of the hard reg for return values.  This note
   is actually an INSN_LIST and it points to the first insn involved
   in setting up arguments for the call.  flow.c uses this to delete
   the entire library call when its result is dead.
     REG_LIBCALL is the inverse of REG_RETVAL: it goes on the first insn
   of the library call and points at the one that has the REG_RETVAL.
     REG_WAS_0 says that the register set in this insn held 0 before the insn.
   The contents of the note is the insn that stored the 0.
   If that insn is deleted or patched to a NOTE, the REG_WAS_0 is inoperative.
   The REG_WAS_0 note is actually an INSN_LIST, not an EXPR_LIST.
     REG_NONNEG means that the register is always nonnegative during
   the containing loop.  This is used in branches so that decrement and
   branch instructions terminating on zero can be matched.  There must be
   an insn pattern in the md file named `decrement_and_branch_until_zero'
   or else this will never be added to any instructions.
     REG_NO_CONFLICT means there is no conflict *after this insn*
   between the register in the note and the destination of this insn.
     REG_UNUSED identifies a register set in this insn and never used.
     REG_CC_SETTER and REG_CC_USER link a pair of insns that set and use
   CC0, respectively.  Normally, these are required to be consecutive insns,
   but we permit putting a cc0-setting insn in the delay slot of a branch
   as long as only one copy of the insn exists.  In that case, these notes
   point from one to the other to allow code generation to determine what
   any require information and to properly update CC_STATUS.
     REG_LABEL points to a CODE_LABEL.  Used by non-JUMP_INSNs to
   say that the CODE_LABEL contained in the REG_LABEL note is used
   by the insn.
     REG_DEP_ANTI is used in LOG_LINKS which represent anti (write after read)
   dependencies.  REG_DEP_OUTPUT is used in LOG_LINKS which represent output
   (write after write) dependencies.  Data dependencies, which are the only
   type of LOG_LINK created by flow, are represented by a 0 reg note kind.  */

#define REG_NOTES(INSN)	((INSN)->fld[6].rtx)

/* Don't forget to change reg_note_name in rtl.c.  */
enum reg_note { REG_DEAD = 1, REG_INC = 2, REG_EQUIV = 3, REG_WAS_0 = 4,
		REG_EQUAL = 5, REG_RETVAL = 6, REG_LIBCALL = 7,
		REG_NONNEG = 8, REG_NO_CONFLICT = 9, REG_UNUSED = 10,
		REG_CC_SETTER = 11, REG_CC_USER = 12, REG_LABEL = 13,
		REG_DEP_ANTI = 14, REG_DEP_OUTPUT = 15 };

/* Define macros to extract and insert the reg-note kind in an EXPR_LIST.  */
#define REG_NOTE_KIND(LINK) ((enum reg_note) GET_MODE (LINK))
#define PUT_REG_NOTE_KIND(LINK,KIND) PUT_MODE(LINK, (enum machine_mode) (KIND))

/* Names for REG_NOTE's in EXPR_LIST insn's.  */

extern char *reg_note_name[];
#define GET_REG_NOTE_NAME(MODE) (reg_note_name[(int)(MODE)])

/* The label-number of a code-label.  The assembler label
   is made from `L' and the label-number printed in decimal.
   Label numbers are unique in a compilation.  */
#define CODE_LABEL_NUMBER(INSN)	((INSN)->fld[3].rtint)

#define LINE_NUMBER NOTE

/* In a NOTE that is a line number, this is a string for the file name
   that the line is in.  We use the same field to record block numbers
   temporarily in NOTE_INSN_BLOCK_BEG and NOTE_INSN_BLOCK_END notes.
   (We avoid lots of casts between ints and pointers if we use a
   different macro for the bock number.)  */

#define NOTE_SOURCE_FILE(INSN)  ((INSN)->fld[3].rtstr)
#define NOTE_BLOCK_NUMBER(INSN) ((INSN)->fld[3].rtint)

/* In a NOTE that is a line number, this is the line number.
   Other kinds of NOTEs are identified by negative numbers here.  */
#define NOTE_LINE_NUMBER(INSN) ((INSN)->fld[4].rtint)

/* Codes that appear in the NOTE_LINE_NUMBER field
   for kinds of notes that are not line numbers.  */

/* This note indicates the end of the real body of the function,
   after moving the parms into their homes, etc.  */
#define NOTE_INSN_FUNCTION_BEG 0

/* This note is used to get rid of an insn
   when it isn't safe to patch the insn out of the chain.  */
#define NOTE_INSN_DELETED -1
#define NOTE_INSN_BLOCK_BEG -2
#define NOTE_INSN_BLOCK_END -3
#define NOTE_INSN_LOOP_BEG -4
#define NOTE_INSN_LOOP_END -5
/* This kind of note is generated at the end of the function body,
   just before the return insn or return label.
   In an optimizing compilation it is deleted by the first jump optimization,
   after enabling that optimizer to determine whether control can fall
   off the end of the function body without a return statement.  */
#define NOTE_INSN_FUNCTION_END -6
/* This kind of note is generated just after each call to `setjmp', et al.  */
#define NOTE_INSN_SETJMP -7
/* Generated at the place in a loop that `continue' jumps to.  */
#define NOTE_INSN_LOOP_CONT -8
/* Generated at the start of a duplicated exit test.  */
#define NOTE_INSN_LOOP_VTOP -9
/* This marks the point immediately after the last prologue insn.  */
#define NOTE_INSN_PROLOGUE_END -10
/* This marks the point immediately prior to the first epilogue insn.  */
#define NOTE_INSN_EPILOGUE_BEG -11
/* Generated in place of user-declared labels when they are deleted.  */
#define NOTE_INSN_DELETED_LABEL -12
/* Don't forget to change note_insn_name in rtl.c.  */


#if 0 /* These are not used, and I don't know what they were for. --rms.  */
#define NOTE_DECL_NAME(INSN) ((INSN)->fld[3].rtstr)
#define NOTE_DECL_CODE(INSN) ((INSN)->fld[4].rtint)
#define NOTE_DECL_RTL(INSN) ((INSN)->fld[5].rtx)
#define NOTE_DECL_IDENTIFIER(INSN) ((INSN)->fld[6].rtint)
#define NOTE_DECL_TYPE(INSN) ((INSN)->fld[7].rtint)
#endif /* 0 */

/* Names for NOTE insn's other than line numbers.  */

extern char *note_insn_name[];
#define GET_NOTE_INSN_NAME(NOTE_CODE) (note_insn_name[-(NOTE_CODE)])

/* The name of a label, in case it corresponds to an explicit label
   in the input source code.  */
#define LABEL_NAME(LABEL) ((LABEL)->fld[4].rtstr)

/* In jump.c, each label contains a count of the number
   of LABEL_REFs that point at it, so unused labels can be deleted.  */
#define LABEL_NUSES(LABEL) ((LABEL)->fld[5].rtint)

/* In jump.c, each JUMP_INSN can point to a label that it can jump to,
   so that if the JUMP_INSN is deleted, the label's LABEL_NUSES can
   be decremented and possibly the label can be deleted.  */
#define JUMP_LABEL(INSN)   ((INSN)->fld[7].rtx)

/* Once basic blocks are found in flow.c,
   each CODE_LABEL starts a chain that goes through
   all the LABEL_REFs that jump to that label.
   The chain eventually winds up at the CODE_LABEL; it is circular.  */
#define LABEL_REFS(LABEL) ((LABEL)->fld[5].rtx)

/* This is the field in the LABEL_REF through which the circular chain
   of references to a particular label is linked.
   This chain is set up in flow.c.  */

#define LABEL_NEXTREF(REF) ((REF)->fld[1].rtx)

/* Once basic blocks are found in flow.c,
   Each LABEL_REF points to its containing instruction with this field.  */

#define CONTAINING_INSN(RTX) ((RTX)->fld[2].rtx)

/* For a REG rtx, REGNO extracts the register number.  */

#define REGNO(RTX) ((RTX)->fld[0].rtint)

/* For a REG rtx, REG_FUNCTION_VALUE_P is nonzero if the reg
   is the current function's return value.  */

#define REG_FUNCTION_VALUE_P(RTX) ((RTX)->integrated)

/* 1 in a REG rtx if it corresponds to a variable declared by the user.  */
#define REG_USERVAR_P(RTX) ((RTX)->volatil)

/* For a CONST_INT rtx, INTVAL extracts the integer.  */

#define INTVAL(RTX) ((RTX)->fld[0].rtwint)

/* For a SUBREG rtx, SUBREG_REG extracts the value we want a subreg of.
   SUBREG_WORD extracts the word-number.  */

#define SUBREG_REG(RTX) ((RTX)->fld[0].rtx)
#define SUBREG_WORD(RTX) ((RTX)->fld[1].rtint)

/* 1 if the REG contained in SUBREG_REG is already known to be
   sign- or zero-extended from the mode of the SUBREG to the mode of
   the reg.  SUBREG_PROMOTED_UNSIGNED_P gives the signedness of the
   extension.  

   When used as a LHS, is means that this extension must be done
   when assigning to SUBREG_REG.  */

#define SUBREG_PROMOTED_VAR_P(RTX) ((RTX)->in_struct)
#define SUBREG_PROMOTED_UNSIGNED_P(RTX) ((RTX)->unchanging)

/* Access various components of an ASM_OPERANDS rtx.  */

#define ASM_OPERANDS_TEMPLATE(RTX) XSTR ((RTX), 0)
#define ASM_OPERANDS_OUTPUT_CONSTRAINT(RTX) XSTR ((RTX), 1)
#define ASM_OPERANDS_OUTPUT_IDX(RTX) XINT ((RTX), 2)
#define ASM_OPERANDS_INPUT_VEC(RTX) XVEC ((RTX), 3)
#define ASM_OPERANDS_INPUT_CONSTRAINT_VEC(RTX) XVEC ((RTX), 4)
#define ASM_OPERANDS_INPUT(RTX, N) XVECEXP ((RTX), 3, (N))
#define ASM_OPERANDS_INPUT_LENGTH(RTX) XVECLEN ((RTX), 3)
#define ASM_OPERANDS_INPUT_CONSTRAINT(RTX, N) XSTR (XVECEXP ((RTX), 4, (N)), 0)
#define ASM_OPERANDS_INPUT_MODE(RTX, N) GET_MODE (XVECEXP ((RTX), 4, (N)))
#define ASM_OPERANDS_SOURCE_FILE(RTX) XSTR ((RTX), 5)
#define ASM_OPERANDS_SOURCE_LINE(RTX) XINT ((RTX), 6)

/* For a MEM rtx, 1 if it's a volatile reference.
   Also in an ASM_OPERANDS rtx.  */
#define MEM_VOLATILE_P(RTX) ((RTX)->volatil)

/* For a MEM rtx, 1 if it refers to a structure or union component.  */
#define MEM_IN_STRUCT_P(RTX) ((RTX)->in_struct)

/* For a LABEL_REF, 1 means that this reference is to a label outside the
   loop containing the reference.  */
#define LABEL_OUTSIDE_LOOP_P(RTX) ((RTX)->in_struct)

/* For a LABEL_REF, 1 means it is for a nonlocal label.  */
/* Likewise in an EXPR_LIST for a REG_LABEL note.  */
#define LABEL_REF_NONLOCAL_P(RTX) ((RTX)->volatil)

/* For a CODE_LABEL, 1 means always consider this label to be needed.  */
#define LABEL_PRESERVE_P(RTX) ((RTX)->in_struct)

/* For a REG, 1 means the register is used only in an exit test of a loop.  */
#define REG_LOOP_TEST_P(RTX) ((RTX)->in_struct)

/* During sched, for an insn, 1 means that the insn must be scheduled together
   with the preceding insn.  */
#define SCHED_GROUP_P(INSN) ((INSN)->in_struct)

/* During sched, for the LOG_LINKS of an insn, these cache the adjusted
   cost of the dependence link.  The cost of executing an instruction
   may vary based on how the results are used.  LINK_COST_ZERO is 1 when
   the cost through the link varies and is unchanged (i.e., the link has
   zero additional cost).  LINK_COST_FREE is 1 when the cost through the
   link is zero (i.e., the link makes the cost free).  In other cases,
   the adjustment to the cost is recomputed each time it is needed.  */
#define LINK_COST_ZERO(X) ((X)->jump)
#define LINK_COST_FREE(X) ((X)->call)

/* For a SET rtx, SET_DEST is the place that is set
   and SET_SRC is the value it is set to.  */
#define SET_DEST(RTX) ((RTX)->fld[0].rtx)
#define SET_SRC(RTX) ((RTX)->fld[1].rtx)

/* For a TRAP_IF rtx, TRAP_CONDITION is an expression.  */
#define TRAP_CONDITION(RTX) ((RTX)->fld[0].rtx)

/* 1 in a SYMBOL_REF if it addresses this function's constants pool.  */
#define CONSTANT_POOL_ADDRESS_P(RTX) ((RTX)->unchanging)

/* Flag in a SYMBOL_REF for machine-specific purposes.  */
#define SYMBOL_REF_FLAG(RTX) ((RTX)->volatil)

/* 1 means a SYMBOL_REF has been the library function in emit_library_call.  */
#define SYMBOL_REF_USED(RTX) ((RTX)->used)

/* For an INLINE_HEADER rtx, FIRST_FUNCTION_INSN is the first insn
   of the function that is not involved in copying parameters to
   pseudo-registers.  FIRST_PARM_INSN is the very first insn of
   the function, including the parameter copying.
   We keep this around in case we must splice
   this function into the assembly code at the end of the file.
   FIRST_LABELNO is the first label number used by the function (inclusive).
   LAST_LABELNO is the last label used by the function (exclusive).
   MAX_REGNUM is the largest pseudo-register used by that function.
   FUNCTION_ARGS_SIZE is the size of the argument block in the stack.
   POPS_ARGS is the number of bytes of input arguments popped by the function
   STACK_SLOT_LIST is the list of stack slots.
   FUNCTION_FLAGS are where single-bit flags are saved.
   OUTGOING_ARGS_SIZE is the size of the largest outgoing stack parameter list.
   ORIGINAL_ARG_VECTOR is a vector of the original DECL_RTX values
    for the function arguments.
   ORIGINAL_DECL_INITIAL is a pointer to the original DECL_INITIAL for the
    function.

   We want this to lay down like an INSN.  The PREV_INSN field
   is always NULL.  The NEXT_INSN field always points to the
   first function insn of the function being squirreled away.  */

#define FIRST_FUNCTION_INSN(RTX) ((RTX)->fld[2].rtx)
#define FIRST_PARM_INSN(RTX) ((RTX)->fld[3].rtx)
#define FIRST_LABELNO(RTX) ((RTX)->fld[4].rtint)
#define LAST_LABELNO(RTX) ((RTX)->fld[5].rtint)
#define MAX_PARMREG(RTX) ((RTX)->fld[6].rtint)
#define MAX_REGNUM(RTX) ((RTX)->fld[7].rtint)
#define FUNCTION_ARGS_SIZE(RTX) ((RTX)->fld[8].rtint)
#define POPS_ARGS(RTX) ((RTX)->fld[9].rtint)
#define STACK_SLOT_LIST(RTX) ((RTX)->fld[10].rtx)
#define FUNCTION_FLAGS(RTX) ((RTX)->fld[11].rtint)
#define OUTGOING_ARGS_SIZE(RTX) ((RTX)->fld[12].rtint)
#define ORIGINAL_ARG_VECTOR(RTX) ((RTX)->fld[13].rtvec)
#define ORIGINAL_DECL_INITIAL(RTX) ((RTX)->fld[14].rtx)

/* In FUNCTION_FLAGS we save some variables computed when emitting the code
   for the function and which must be `or'ed into the current flag values when
   insns from that function are being inlined.  */

/* These ought to be an enum, but non-ANSI compilers don't like that.  */
#define FUNCTION_FLAGS_CALLS_ALLOCA 01
#define FUNCTION_FLAGS_CALLS_SETJMP 02
#define FUNCTION_FLAGS_RETURNS_STRUCT 04
#define FUNCTION_FLAGS_RETURNS_PCC_STRUCT 010
#define FUNCTION_FLAGS_NEEDS_CONTEXT 020
#define FUNCTION_FLAGS_HAS_NONLOCAL_LABEL 040
#define FUNCTION_FLAGS_RETURNS_POINTER 0100
#define FUNCTION_FLAGS_USES_CONST_POOL 0200
#define FUNCTION_FLAGS_CALLS_LONGJMP 0400
#define FUNCTION_FLAGS_USES_PIC_OFFSET_TABLE 01000

/* Define a macro to look for REG_INC notes,
   but save time on machines where they never exist.  */

/* Don't continue this line--convex cc version 4.1 would lose.  */
#if (defined (HAVE_PRE_INCREMENT) || defined (HAVE_PRE_DECREMENT) || defined (HAVE_POST_INCREMENT) || defined (HAVE_POST_DECREMENT))
#define FIND_REG_INC_NOTE(insn, reg) (find_reg_note ((insn), REG_INC, (reg)))
#else
#define FIND_REG_INC_NOTE(insn, reg) 0
#endif

/* Indicate whether the machine has any sort of auto increment addressing.
   If not, we can avoid checking for REG_INC notes.  */

/* Don't continue this line--convex cc version 4.1 would lose.  */
#if (defined (HAVE_PRE_INCREMENT) || defined (HAVE_PRE_DECREMENT) || defined (HAVE_POST_INCREMENT) || defined (HAVE_POST_DECREMENT))
#define AUTO_INC_DEC
#endif

/* Generally useful functions.  */

/* The following functions accept a wide integer argument.  Rather than
   having to cast on every function call, we use a macro instead, that is
   defined here and in tree.h.  */

#ifndef exact_log2
#define exact_log2(N) exact_log2_wide ((HOST_WIDE_INT) (N))
#define floor_log2(N) floor_log2_wide ((HOST_WIDE_INT) (N))
#endif

#define plus_constant(X,C) plus_constant_wide (X, (HOST_WIDE_INT) (C))

#define plus_constant_for_output(X,C)  \
  plus_constant_for_output_wide (X, (HOST_WIDE_INT) (C))

extern rtx plus_constant_wide		 PROTO((rtx, HOST_WIDE_INT));
extern rtx plus_constant_for_output_wide PROTO((rtx, HOST_WIDE_INT));

#define GEN_INT(N) gen_rtx (CONST_INT, VOIDmode, (N))

#if 0
/* We cannot define prototypes for the variable argument functions,
   since they have not been ANSI-fied, and an ANSI compiler would
   complain when compiling the definition of these functions.  */

extern rtx gen_rtx			PROTO((enum rtx_code, enum machine_mode, ...));
extern rtvec gen_rtvec			PROTO((int, ...));

#else
extern rtx gen_rtx ();
extern rtvec gen_rtvec ();
#endif

#ifdef BUFSIZ			/* stdio.h has been included */
extern rtx read_rtx			PROTO((FILE *));
#else
extern rtx read_rtx ();
#endif

#if 0
/* At present, don't prototype xrealloc, since all of the callers don't
   cast their pointers to char *, and all of the xrealloc's don't use
   void * yet.  */
extern char *xrealloc			PROTO((void *, unsigned));
#else
extern char *xrealloc ();
#endif

extern char *xmalloc			PROTO((unsigned));
extern char *oballoc			PROTO((int));
extern char *permalloc			PROTO((int));
extern void free			PROTO((void *));
extern rtx rtx_alloc			PROTO((RTX_CODE));
extern rtvec rtvec_alloc		PROTO((int));
extern rtx find_reg_note		PROTO((rtx, enum reg_note, rtx));
extern rtx find_regno_note		PROTO((rtx, enum reg_note, int));
extern HOST_WIDE_INT get_integer_term	PROTO((rtx));
extern rtx get_related_value		PROTO((rtx));
extern rtx single_set			PROTO((rtx));
extern rtx find_last_value		PROTO((rtx, rtx *, rtx));
extern rtx copy_rtx			PROTO((rtx));
extern rtx copy_rtx_if_shared		PROTO((rtx));
extern rtx copy_most_rtx		PROTO((rtx, rtx));
extern rtx replace_rtx			PROTO((rtx, rtx, rtx));
extern rtvec gen_rtvec_v		PROTO((int, rtx *));
extern rtx gen_reg_rtx			PROTO((enum machine_mode));
extern rtx gen_label_rtx		PROTO((void));
extern rtx gen_inline_header_rtx	PROTO((rtx, rtx, int, int, int, int, int, int, rtx, int, int, rtvec, rtx));
extern rtx gen_lowpart_common		PROTO((enum machine_mode, rtx));
extern rtx gen_lowpart			PROTO((enum machine_mode, rtx));
extern rtx gen_lowpart_if_possible	PROTO((enum machine_mode, rtx));
extern rtx gen_highpart			PROTO((enum machine_mode, rtx));
extern rtx gen_realpart			PROTO((enum machine_mode, rtx));
extern rtx gen_imagpart			PROTO((enum machine_mode, rtx));
extern rtx operand_subword		PROTO((rtx, int, int, enum machine_mode));
extern rtx operand_subword_force	PROTO((rtx, int, enum machine_mode));
extern int subreg_lowpart_p		PROTO((rtx));
extern rtx make_safe_from		PROTO((rtx, rtx));
extern rtx memory_address		PROTO((enum machine_mode, rtx));
extern rtx get_insns			PROTO((void));
extern rtx get_last_insn		PROTO((void));
extern rtx get_last_insn_anywhere	PROTO((void));
extern void start_sequence		PROTO((void));
extern void push_to_sequence		PROTO((rtx));
extern void end_sequence		PROTO((void));
extern rtx gen_sequence			PROTO((void));
extern rtx immed_double_const		PROTO((HOST_WIDE_INT, HOST_WIDE_INT, enum machine_mode));
extern rtx force_const_mem		PROTO((enum machine_mode, rtx));
extern rtx force_reg			PROTO((enum machine_mode, rtx));
extern rtx get_pool_constant		PROTO((rtx));
extern enum machine_mode get_pool_mode	PROTO((rtx));
extern int get_pool_offset		PROTO((rtx));
extern rtx simplify_subtraction		PROTO((rtx));
extern rtx assign_stack_local		PROTO((enum machine_mode, int, int));
extern rtx assign_stack_temp		PROTO((enum machine_mode, int, int));
extern rtx protect_from_queue		PROTO((rtx, int));
extern void emit_queue			PROTO((void));
extern rtx emit_move_insn		PROTO((rtx, rtx));
extern rtx emit_insn_before		PROTO((rtx, rtx));
extern rtx emit_jump_insn_before	PROTO((rtx, rtx));
extern rtx emit_call_insn_before	PROTO((rtx, rtx));
extern rtx emit_barrier_before		PROTO((rtx));
extern rtx emit_note_before		PROTO((int, rtx));
extern rtx emit_insn_after		PROTO((rtx, rtx));
extern rtx emit_jump_insn_after		PROTO((rtx, rtx));
extern rtx emit_barrier_after		PROTO((rtx));
extern rtx emit_label_after		PROTO((rtx, rtx));
extern rtx emit_note_after		PROTO((int, rtx));
extern rtx emit_line_note_after		PROTO((char *, int, rtx));
extern rtx emit_insn			PROTO((rtx));
extern rtx emit_insns			PROTO((rtx));
extern rtx emit_insns_before		PROTO((rtx, rtx));
extern rtx emit_jump_insn		PROTO((rtx));
extern rtx emit_call_insn		PROTO((rtx));
extern rtx emit_label			PROTO((rtx));
extern rtx emit_barrier			PROTO((void));
extern rtx emit_line_note		PROTO((char *, int));
extern rtx emit_note			PROTO((char *, int));
extern rtx emit_line_note_force		PROTO((char *, int));
extern rtx make_insn_raw		PROTO((rtx));
extern rtx previous_insn		PROTO((rtx));
extern rtx next_insn			PROTO((rtx));
extern rtx prev_nonnote_insn		PROTO((rtx));
extern rtx next_nonnote_insn		PROTO((rtx));
extern rtx prev_real_insn		PROTO((rtx));
extern rtx next_real_insn		PROTO((rtx));
extern rtx prev_active_insn		PROTO((rtx));
extern rtx next_active_insn		PROTO((rtx));
extern rtx prev_label			PROTO((rtx));
extern rtx next_label			PROTO((rtx));
extern rtx next_cc0_user		PROTO((rtx));
extern rtx prev_cc0_setter		PROTO((rtx));
extern rtx reg_set_last			PROTO((rtx, rtx));
extern rtx next_nondeleted_insn		PROTO((rtx));
extern enum rtx_code reverse_condition	PROTO((enum rtx_code));
extern enum rtx_code swap_condition	PROTO((enum rtx_code));
extern enum rtx_code unsigned_condition	PROTO((enum rtx_code));
extern enum rtx_code signed_condition	PROTO((enum rtx_code));
extern rtx find_equiv_reg		PROTO((rtx, rtx, enum reg_class, int, short *, int, enum machine_mode));
extern rtx squeeze_notes		PROTO((rtx, rtx));
extern rtx delete_insn			PROTO((rtx));
extern void delete_jump			PROTO((rtx));
extern rtx get_label_before		PROTO((rtx));
extern rtx get_label_after		PROTO((rtx));
extern rtx follow_jumps			PROTO((rtx));
extern rtx adj_offsettable_operand	PROTO((rtx, int));
extern rtx try_split			PROTO((rtx, rtx, int));
extern rtx split_insns			PROTO((rtx, rtx));
extern rtx simplify_unary_operation	PROTO((enum rtx_code, enum machine_mode, rtx, enum machine_mode));
extern rtx simplify_binary_operation	PROTO((enum rtx_code, enum machine_mode, rtx, rtx));
extern rtx simplify_ternary_operation	PROTO((enum rtx_code, enum machine_mode, enum machine_mode, rtx, rtx, rtx));
extern rtx simplify_relational_operation PROTO((enum rtx_code, enum machine_mode, rtx, rtx));
extern rtx nonlocal_label_rtx_list	PROTO((void));
extern rtx gen_move_insn		PROTO((rtx, rtx));
extern rtx gen_jump			PROTO((rtx));
extern rtx gen_beq			PROTO((rtx));
extern rtx gen_bge			PROTO((rtx));
extern rtx gen_ble			PROTO((rtx));
extern rtx eliminate_constant_term	PROTO((rtx, rtx *));
extern rtx expand_complex_abs		PROTO((enum machine_mode, rtx, rtx, int));

/* Maximum number of parallel sets and clobbers in any insn in this fn.
   Always at least 3, since the combiner could put that many togetherm
   and we want this to remain correct for all the remaining passes.  */

extern int max_parallel;

extern int asm_noperands		PROTO((rtx));
extern char *decode_asm_operands	PROTO((rtx, rtx *, rtx **, char **, enum machine_mode *));

extern enum reg_class reg_preferred_class PROTO((int));
extern enum reg_class reg_alternate_class PROTO((int));

extern rtx get_first_nonparm_insn	PROTO((void));

/* Standard pieces of rtx, to be substituted directly into things.  */
extern rtx pc_rtx;
extern rtx cc0_rtx;
extern rtx const0_rtx;
extern rtx const1_rtx;
extern rtx const2_rtx;
extern rtx constm1_rtx;
extern rtx const_true_rtx;

extern rtx const_tiny_rtx[3][(int) MAX_MACHINE_MODE];

/* Returns a constant 0 rtx in mode MODE.  Integer modes are treated the 
   same as VOIDmode.  */

#define CONST0_RTX(MODE) (const_tiny_rtx[0][(int) (MODE)])

/* Likewise, for the constants 1 and 2.  */

#define CONST1_RTX(MODE) (const_tiny_rtx[1][(int) (MODE)])
#define CONST2_RTX(MODE) (const_tiny_rtx[2][(int) (MODE)])

/* All references to certain hard regs, except those created
   by allocating pseudo regs into them (when that's possible),
   go through these unique rtx objects.  */
extern rtx stack_pointer_rtx;
extern rtx frame_pointer_rtx;
extern rtx arg_pointer_rtx;
extern rtx pic_offset_table_rtx;
extern rtx struct_value_rtx;
extern rtx struct_value_incoming_rtx;
extern rtx static_chain_rtx;
extern rtx static_chain_incoming_rtx;

/* Virtual registers are used during RTL generation to refer to locations into
   the stack frame when the actual location isn't known until RTL generation
   is complete.  The routine instantiate_virtual_regs replaces these with
   the proper value, which is normally {frame,arg,stack}_pointer_rtx plus
   a constant.  */

#define FIRST_VIRTUAL_REGISTER	(FIRST_PSEUDO_REGISTER)

/* This points to the first word of the incoming arguments passed on the stack,
   either by the caller or by the callee when pretending it was passed by the
   caller.  */

extern rtx virtual_incoming_args_rtx;

#define VIRTUAL_INCOMING_ARGS_REGNUM	(FIRST_VIRTUAL_REGISTER)

/* If FRAME_GROWS_DOWNWARDS, this points to immediately above the first
   variable on the stack.  Otherwise, it points to the first variable on
   the stack.  */

extern rtx virtual_stack_vars_rtx;

#define VIRTUAL_STACK_VARS_REGNUM	((FIRST_VIRTUAL_REGISTER) + 1)

/* This points to the location of dynamically-allocated memory on the stack
   immediately after the stack pointer has been adjusted by the amount
   desired.  */

extern rtx virtual_stack_dynamic_rtx;

#define VIRTUAL_STACK_DYNAMIC_REGNUM	((FIRST_VIRTUAL_REGISTER) + 2)

/* This points to the location in the stack at which outgoing arguments should
   be written when the stack is pre-pushed (arguments pushed using push
   insns always use sp).  */

extern rtx virtual_outgoing_args_rtx;

#define VIRTUAL_OUTGOING_ARGS_REGNUM	((FIRST_VIRTUAL_REGISTER) + 3)

#define LAST_VIRTUAL_REGISTER	((FIRST_VIRTUAL_REGISTER) + 3)

extern rtx find_next_ref		PROTO((rtx, rtx));
extern rtx *find_single_use		PROTO((rtx, rtx, rtx *));

/* It is hard to write the prototype for expand_expr, since it needs
   expr.h to be included for the enumeration.  */

extern rtx expand_expr ();
extern rtx immed_real_const_1();

#ifdef TREE_CODE
/* rtl.h and tree.h were included.  */
extern rtx  output_constant_def PROTO((tree));
extern rtx  immed_real_const	PROTO((tree));
extern rtx  immed_real_const_1	PROTO((REAL_VALUE_TYPE, enum machine_mode));
extern tree make_tree		PROTO((tree, rtx));

#else
extern rtx output_constant_def ();
extern rtx immed_real_const ();
extern rtx immed_real_const_1 ();
#endif

/* Define a default value for STORE_FLAG_VALUE.  */

#ifndef STORE_FLAG_VALUE
#define STORE_FLAG_VALUE 1
#endif

/* Nonzero after end of reload pass.
   Set to 1 or 0 by toplev.c.  */

extern int reload_completed;

/* Set to 1 while reload_as_needed is operating.
   Required by some machines to handle any generated moves differently.  */

extern int reload_in_progress;

/* If this is nonzero, we do not bother generating VOLATILE
   around volatile memory references, and we are willing to
   output indirect addresses.  If cse is to follow, we reject
   indirect addresses so a useful potential cse is generated;
   if it is used only once, instruction combination will produce
   the same indirect address eventually.  */
extern int cse_not_expected;

/* Indexed by pseudo register number, gives the rtx for that pseudo.
   Allocated in parallel with regno_pointer_flag.  */
extern rtx *regno_reg_rtx;
