/* Register Transfer Language (RTL) definitions for GNU C-Compiler
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

#ifndef _RTL_H
#define _RTL_H

#include "machmode.h"

#undef FFS  /* Some systems predefine this symbol; don't let it interfere.  */
#undef FLOAT /* Likewise.  */
#undef ABS /* Likewise.  */
#undef PC /* Likewise.  */

#ifndef TREE_CODE
union tree_node;
#endif

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
#define GET_RTX_LENGTH(CODE)		(rtx_length[(int) (CODE)])

extern char *rtx_name[];
#define GET_RTX_NAME(CODE)		(rtx_name[(int) (CODE)])

extern char *rtx_format[];
#define GET_RTX_FORMAT(CODE)		(rtx_format[(int) (CODE)])

extern char rtx_class[];
#define GET_RTX_CLASS(CODE)		(rtx_class[(int) (CODE)])

/* The flags and bitfields of an ADDR_DIFF_VEC.  BASE is the base label
   relative to which the offsets are calculated, as explained in rtl.def.  */
typedef struct
{
  /* Set at the start of shorten_branches - ONLY WHEN OPTIMIZING - : */
  unsigned min_align: 8;
  /* Flags: */
  unsigned base_after_vec: 1; /* BASE is after the ADDR_DIFF_VEC.  */
  unsigned min_after_vec: 1;  /* minimum address target label is after the ADDR_DIFF_VEC.  */
  unsigned max_after_vec: 1;  /* maximum address target label is after the ADDR_DIFF_VEC.  */
  unsigned min_after_base: 1; /* minimum address target label is after BASE.  */
  unsigned max_after_base: 1; /* maximum address target label is after BASE.  */
  /* Set by the actual branch shortening process - ONLY WHEN OPTIMIZING - : */
  unsigned offset_unsigned: 1; /* offsets have to be treated as unsigned.  */
  unsigned : 2;
  unsigned scale : 8;
} addr_diff_vec_flags;

/* Common union for an element of an rtx.  */

typedef union rtunion_def
{
  HOST_WIDE_INT rtwint;
  int rtint;
  char *rtstr;
  struct rtx_def *rtx;
  struct rtvec_def *rtvec;
  enum machine_mode rttype;
  addr_diff_vec_flags rt_addr_diff_vec_flags;
  struct bitmap_head_def *rtbit;
  union tree_node *rttree;
  struct basic_block_def *bb;
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
  /* LINK_COST_ZERO in an INSN_LIST.  */
  unsigned int jump : 1;
  /* LINK_COST_FREE in an INSN_LIST.  */
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
  /* 1 in a MEM referring to a field of an aggregate.
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
  /* 1 in an INSN or a SET if this rtx is related to the call frame,
     either changing how we compute the frame address or saving and
     restoring registers in the prologue and epilogue.  
     1 in a MEM if the MEM refers to a scalar, rather than a member of
     an aggregate.  */
  unsigned frame_related : 1;
  /* The first element of the operands of this rtx.
     The number of operands and their types are controlled
     by the `code' field, according to rtl.def.  */
  rtunion fld[1];
} *rtx;

#define NULL_RTX (rtx) 0

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
#define RTX_FRAME_RELATED_P(RTX) ((RTX)->frame_related)

/* RTL vector.  These appear inside RTX's when there is a need
   for a variable number of things.  The principle use is inside
   PARALLEL expressions.  */

typedef struct rtvec_def{
  int num_elem;		/* number of elements */
  rtunion elem[1];
} *rtvec;

#define NULL_RTVEC (rtvec) 0

#define GET_NUM_ELEM(RTVEC)		((RTVEC)->num_elem)
#define PUT_NUM_ELEM(RTVEC, NUM)	((RTVEC)->num_elem = (NUM))

#define RTVEC_ELT(RTVEC, I)  ((RTVEC)->elem[(I)].rtx)

/* 1 if X is a REG.  */

#define REG_P(X) (GET_CODE (X) == REG)

/* 1 if X is a constant value that is an integer.  */

#define CONSTANT_P(X)   \
  (GET_CODE (X) == LABEL_REF || GET_CODE (X) == SYMBOL_REF		\
   || GET_CODE (X) == CONST_INT || GET_CODE (X) == CONST_DOUBLE		\
   || GET_CODE (X) == CONST || GET_CODE (X) == HIGH			\
   || GET_CODE (X) == CONSTANT_P_RTX)

/* General accessor macros for accessing the fields of an rtx.  */

#define XEXP(RTX, N)	((RTX)->fld[N].rtx)
#define XINT(RTX, N)	((RTX)->fld[N].rtint)
#define XWINT(RTX, N)	((RTX)->fld[N].rtwint)
#define XSTR(RTX, N)	((RTX)->fld[N].rtstr)
#define XVEC(RTX, N)	((RTX)->fld[N].rtvec)
#define XVECLEN(RTX, N)	((RTX)->fld[N].rtvec->num_elem)
#define XVECEXP(RTX,N,M)((RTX)->fld[N].rtvec->elem[M].rtx)
#define XBITMAP(RTX, N) ((RTX)->fld[N].rtbit)
#define XTREE(RTX, N)   ((RTX)->fld[N].rttree)


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
     REG_EQUIV describes the insn as a whole; it says that the insn
   sets a register to a constant value or to be equivalent to a memory
   address.  If the register is spilled to the stack then the constant
   value should be substituted for it.  The contents of the REG_EQUIV
   is the constant value or memory address, which may be different
   from the source of the SET although it has the same value.  A
   REG_EQUIV note may also appear on an insn which copies a register
   parameter to a pseudo-register, if there is a memory address which
   could be used to hold that pseudo-register throughout the function.
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
/*   REG_BR_PROB is attached to JUMP_INSNs and CALL_INSNs when the flag
   -fbranch-probabilities is given.  It has an integer value.  For jumps,
   it is the probability that this is a taken branch.  For calls, it is the
   probability that this call won't return.
     REG_EXEC_COUNT is attached to the first insn of each basic block, and
   the first insn after each CALL_INSN.  It indicates how many times this
   block was executed.
     REG_SAVE_AREA is used to optimize rtl generated by dynamic stack
   allocations for targets where SETJMP_VIA_SAVE_AREA is true.
     REG_BR_PRED is attached to JUMP_INSNs only, it holds the branch prediction
   flags computed by get_jump_flags() after dbr scheduling is complete.
     REG_FRAME_RELATED_EXPR is attached to insns that are RTX_FRAME_RELATED_P,
   but are too complex for DWARF to interpret what they imply.  The attached
   rtx is used instead of intuition.  */
/*   REG_EH_REGION is used to indicate what exception region an INSN
   belongs in.  This can be used to indicate what region a call may throw
   to.  A REGION of 0 indicates that a call cannot throw at all.
   A REGION  of -1 indicates that it cannot throw, nor will it execute
   a non-local goto.
     REG_EH_RETHROW is used to indicate what that a call is actually a
   call to rethrow, and specifies which region the rethrow is targetting.
   This provides a way to generate the non standard flow edges required 
   for a rethrow.  */
   

#define REG_NOTES(INSN)	((INSN)->fld[6].rtx)

#define ADDR_DIFF_VEC_FLAGS(RTX) ((RTX)->fld[4].rt_addr_diff_vec_flags)

/* Don't forget to change reg_note_name in rtl.c.  */
enum reg_note { REG_DEAD = 1, REG_INC = 2, REG_EQUIV = 3, REG_WAS_0 = 4,
		REG_EQUAL = 5, REG_RETVAL = 6, REG_LIBCALL = 7,
		REG_NONNEG = 8, REG_NO_CONFLICT = 9, REG_UNUSED = 10,
		REG_CC_SETTER = 11, REG_CC_USER = 12, REG_LABEL = 13,
		REG_DEP_ANTI = 14, REG_DEP_OUTPUT = 15, REG_BR_PROB = 16,
		REG_EXEC_COUNT = 17, REG_NOALIAS = 18, REG_SAVE_AREA = 19,
		REG_BR_PRED = 20, REG_EH_CONTEXT = 21,
		REG_FRAME_RELATED_EXPR = 22, REG_EH_REGION = 23,
		REG_EH_RETHROW = 24 };
/* The base value for branch probability notes.  */
#define REG_BR_PROB_BASE  10000

/* Define macros to extract and insert the reg-note kind in an EXPR_LIST.  */
#define REG_NOTE_KIND(LINK) ((enum reg_note) GET_MODE (LINK))
#define PUT_REG_NOTE_KIND(LINK,KIND) PUT_MODE(LINK, (enum machine_mode) (KIND))

/* Names for REG_NOTE's in EXPR_LIST insn's.  */

extern char *reg_note_name[];
#define GET_REG_NOTE_NAME(MODE) (reg_note_name[(int) (MODE)])

/* This field is only present on CALL_INSNs.  It holds a chain of EXPR_LIST of
   USE and CLOBBER expressions.
     USE expressions list the registers filled with arguments that
   are passed to the function.
     CLOBBER expressions document the registers explicitly clobbered
   by this CALL_INSN.
     Pseudo registers can not be mentioned in this list.  */
#define CALL_INSN_FUNCTION_USAGE(INSN)	((INSN)->fld[7].rtx)

/* The label-number of a code-label.  The assembler label
   is made from `L' and the label-number printed in decimal.
   Label numbers are unique in a compilation.  */
#define CODE_LABEL_NUMBER(INSN)	((INSN)->fld[3].rtint)

#define LINE_NUMBER NOTE

/* In a NOTE that is a line number, this is a string for the file name that the
   line is in.  We use the same field to record block numbers temporarily in
   NOTE_INSN_BLOCK_BEG and NOTE_INSN_BLOCK_END notes.  (We avoid lots of casts
   between ints and pointers if we use a different macro for the block number.)
   The NOTE_INSN_RANGE_{START,END} and NOTE_INSN_LIVE notes record their
   information as a rtx in the field.  */

#define NOTE_SOURCE_FILE(INSN)  ((INSN)->fld[3].rtstr)
#define NOTE_BLOCK_NUMBER(INSN) ((INSN)->fld[3].rtint)
#define NOTE_RANGE_INFO(INSN)   ((INSN)->fld[3].rtx)
#define NOTE_LIVE_INFO(INSN)    ((INSN)->fld[3].rtx)
#define NOTE_BASIC_BLOCK(INSN)	((INSN)->fld[3].bb)

/* If the NOTE_BLOCK_NUMBER field gets a -1, it means create a new
   block node for a live range block.  */
#define NOTE_BLOCK_LIVE_RANGE_BLOCK -1

/* In a NOTE that is a line number, this is the line number.
   Other kinds of NOTEs are identified by negative numbers here.  */
#define NOTE_LINE_NUMBER(INSN) ((INSN)->fld[4].rtint)

/* Codes that appear in the NOTE_LINE_NUMBER field
   for kinds of notes that are not line numbers.

   Notice that we do not try to use zero here for any of
   the special note codes because sometimes the source line
   actually can be zero!  This happens (for example) when we
   are generating code for the per-translation-unit constructor
   and destructor routines for some C++ translation unit.

   If you should change any of the following values, or if you
   should add a new value here, don't forget to change the
   note_insn_name array in rtl.c.  */

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
/* This note indicates the start of the real body of the function,
   i.e. the point just after all of the parms have been moved into
   their homes, etc.  */
#define NOTE_INSN_FUNCTION_BEG -13
/* These note where exception handling regions begin and end.  */
#define NOTE_INSN_EH_REGION_BEG -14
#define NOTE_INSN_EH_REGION_END -15
/* Generated whenever a duplicate line number note is output.  For example,
   one is output after the end of an inline function, in order to prevent
   the line containing the inline call from being counted twice in gcov. */
#define NOTE_REPEATED_LINE_NUMBER -16

/* Start/end of a live range region, where pseudos allocated on the stack can
   be allocated to temporary registers.  */
#define NOTE_INSN_RANGE_START -17
#define NOTE_INSN_RANGE_END -18
/* Record which registers are currently live.  */
#define NOTE_INSN_LIVE -19
/* Record the struct for the following basic block.  */
#define NOTE_INSN_BASIC_BLOCK -20

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

/* The original regno this ADDRESSOF was built for.  */
#define ADDRESSOF_REGNO(RTX) ((RTX)->fld[1].rtint)

/* The variable in the register we took the address of.  */
#define ADDRESSOF_DECL(X) ((tree) XEXP ((X), 2))
#define SET_ADDRESSOF_DECL(X, T) (XEXP ((X), 2) = (rtx) (T))

/* In jump.c, each JUMP_INSN can point to a label that it can jump to,
   so that if the JUMP_INSN is deleted, the label's LABEL_NUSES can
   be decremented and possibly the label can be deleted.  */
#define JUMP_LABEL(INSN)   ((INSN)->fld[7].rtx)

/* Once basic blocks are found in flow.c,
   each CODE_LABEL starts a chain that goes through
   all the LABEL_REFs that jump to that label.
   The chain eventually winds up at the CODE_LABEL; it is circular.  */
#define LABEL_REFS(LABEL) ((LABEL)->fld[6].rtx)

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

/* For a MEM rtx, 1 if it refers to a field of an aggregate.  If zero,
   RTX may or may not refer to a field of an aggregate.  */
#define MEM_IN_STRUCT_P(RTX) ((RTX)->in_struct)

/* For a MEM rtx, 1 if it refers to a scalar.  If zero, RTX may or may
   not refer to a scalar.*/
#define MEM_SCALAR_P(RTX) ((RTX)->frame_related)

/* Copy the MEM_VOLATILE_P, MEM_IN_STRUCT_P, and MEM_SCALAR_P
   attributes from RHS to LHS.  */
#define MEM_COPY_ATTRIBUTES(LHS, RHS)			\
  (MEM_VOLATILE_P (LHS) = MEM_VOLATILE_P (RHS),		\
   MEM_IN_STRUCT_P (LHS) = MEM_IN_STRUCT_P (RHS),	\
   MEM_SCALAR_P (LHS) = MEM_SCALAR_P (RHS))

/* If VAL is non-zero, set MEM_IN_STRUCT_P and clear MEM_SCALAR_P in
   RTX.  Otherwise, vice versa.  Use this macro only when you are
   *sure* that you know that the MEM is in a structure, or is a
   scalar.  VAL is evaluated only once.  */
#define MEM_SET_IN_STRUCT_P(RTX, VAL) 				\
  ((VAL) ? (MEM_IN_STRUCT_P (RTX) = 1, MEM_SCALAR_P (RTX) = 0)	\
   : (MEM_IN_STRUCT_P (RTX) = 0, MEM_SCALAR_P (RTX) = 1))

/* For a MEM rtx, the alias set.  If 0, this MEM is not in any alias
   set, and may alias anything.  Otherwise, the MEM can only alias
   MEMs in the same alias set.  This value is set in a
   language-dependent manner in the front-end, and should not be
   altered in the back-end.  These set numbers are tested for zero,
   and compared for equality; they have no other significance.  In
   some front-ends, these numbers may correspond in some way to types,
   or other language-level entities, but they need not, and the
   back-end makes no such assumptions.  */
#define MEM_ALIAS_SET(RTX) (XINT (RTX, 1))

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
#define TRAP_CODE(RTX) (RTX)->fld[1].rtx

/* 1 in a SYMBOL_REF if it addresses this function's constants pool.  */
#define CONSTANT_POOL_ADDRESS_P(RTX) ((RTX)->unchanging)

/* Flag in a SYMBOL_REF for machine-specific purposes.  */
#define SYMBOL_REF_FLAG(RTX) ((RTX)->volatil)

/* 1 in a SYMBOL_REF if it represents a symbol which might have to change
   if its inlined or unrolled. */
#define SYMBOL_REF_NEED_ADJUST(RTX)  ((RTX)->in_struct)

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
   FORCED_LABELS is the list of labels whose address was taken.
   FUNCTION_FLAGS are where single-bit flags are saved.
   OUTGOING_ARGS_SIZE is the size of the largest outgoing stack parameter list.
   ORIGINAL_ARG_VECTOR is a vector of the original DECL_RTX values
    for the function arguments.
   ORIGINAL_DECL_INITIAL is a pointer to the original DECL_INITIAL for the
    function.
   INLINE_REGNO_REG_RTX, INLINE_REGNO_POINTER_FLAG, and
    INLINE_REGNO_POINTER_ALIGN are pointers to the corresponding arrays.

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
#define FORCED_LABELS(RTX) ((RTX)->fld[11].rtx)
#define FUNCTION_FLAGS(RTX) ((RTX)->fld[12].rtint)
#define OUTGOING_ARGS_SIZE(RTX) ((RTX)->fld[13].rtint)
#define ORIGINAL_ARG_VECTOR(RTX) ((RTX)->fld[14].rtvec)
#define ORIGINAL_DECL_INITIAL(RTX) ((RTX)->fld[15].rtx)
#define INLINE_REGNO_REG_RTX(RTX) ((RTX)->fld[16].rtvec)
#define INLINE_REGNO_POINTER_FLAG(RTX) ((RTX)->fld[17].rtstr)
#define INLINE_REGNO_POINTER_ALIGN(RTX) ((RTX)->fld[18].rtstr)
#define PARMREG_STACK_LOC(RTX) ((RTX)->fld[19].rtvec)

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
#define FUNCTION_FLAGS_HAS_COMPUTED_JUMP 02000

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

#ifndef HAVE_PRE_INCREMENT
#define HAVE_PRE_INCREMENT 0
#endif

#ifndef HAVE_PRE_DECREMENT
#define HAVE_PRE_DECREMENT 0
#endif

#ifndef HAVE_POST_INCREMENT
#define HAVE_POST_INCREMENT 0
#endif

#ifndef HAVE_POST_DECREMENT
#define HAVE_POST_DECREMENT 0
#endif


/* Some architectures do not have complete pre/post increment/decrement
   instruction sets, or only move some modes efficiently.  These macros
   allow us to tune autoincrement generation.  */

#ifndef USE_LOAD_POST_INCREMENT
#define USE_LOAD_POST_INCREMENT(MODE)   HAVE_POST_INCREMENT
#endif

#ifndef USE_LOAD_POST_DECREMENT
#define USE_LOAD_POST_DECREMENT(MODE)   HAVE_POST_DECREMENT
#endif

#ifndef USE_LOAD_PRE_INCREMENT
#define USE_LOAD_PRE_INCREMENT(MODE)    HAVE_PRE_INCREMENT
#endif

#ifndef USE_LOAD_PRE_DECREMENT
#define USE_LOAD_PRE_DECREMENT(MODE)    HAVE_PRE_DECREMENT
#endif

#ifndef USE_STORE_POST_INCREMENT
#define USE_STORE_POST_INCREMENT(MODE)  HAVE_POST_INCREMENT
#endif

#ifndef USE_STORE_POST_DECREMENT
#define USE_STORE_POST_DECREMENT(MODE)  HAVE_POST_DECREMENT
#endif

#ifndef USE_STORE_PRE_INCREMENT
#define USE_STORE_PRE_INCREMENT(MODE)   HAVE_PRE_INCREMENT
#endif

#ifndef USE_STORE_PRE_DECREMENT
#define USE_STORE_PRE_DECREMENT(MODE)   HAVE_PRE_DECREMENT
#endif


/* Accessors for RANGE_INFO.  */
/* For RANGE_{START,END} notes return the RANGE_START note.  */
#define RANGE_INFO_NOTE_START(INSN) (XEXP (INSN, 0))

/* For RANGE_{START,END} notes return the RANGE_START note.  */
#define RANGE_INFO_NOTE_END(INSN) (XEXP (INSN, 1))

/* For RANGE_{START,END} notes, return the vector containing the registers used
   in the range.  */
#define RANGE_INFO_REGS(INSN) (XVEC (INSN, 2))
#define RANGE_INFO_REGS_REG(INSN, N) (XVECEXP (INSN, 2, N))
#define RANGE_INFO_NUM_REGS(INSN) (XVECLEN (INSN, 2))

/* For RANGE_{START,END} notes, the number of calls within the range.  */
#define RANGE_INFO_NCALLS(INSN) (XINT (INSN, 3))

/* For RANGE_{START,END} notes, the number of insns within the range.  */
#define RANGE_INFO_NINSNS(INSN) (XINT (INSN, 4))

/* For RANGE_{START,END} notes, a unique # to identify this range.  */
#define RANGE_INFO_UNIQUE(INSN) (XINT (INSN, 5))

/* For RANGE_{START,END} notes, the basic block # the range starts with. */
#define RANGE_INFO_BB_START(INSN) (XINT (INSN, 6))

/* For RANGE_{START,END} notes, the basic block # the range ends with. */
#define RANGE_INFO_BB_END(INSN) (XINT (INSN, 7))

/* For RANGE_{START,END} notes, the loop depth the range is in.  */
#define RANGE_INFO_LOOP_DEPTH(INSN) (XINT (INSN, 8))

/* For RANGE_{START,END} notes, the bitmap of live registers at the start
   of the range.  */
#define RANGE_INFO_LIVE_START(INSN) (XBITMAP (INSN, 9))

/* For RANGE_{START,END} notes, the bitmap of live registers at the end
   of the range.  */
#define RANGE_INFO_LIVE_END(INSN) (XBITMAP (INSN, 10))

/* For RANGE_START notes, the marker # of the start of the range.  */
#define RANGE_INFO_MARKER_START(INSN) (XINT (INSN, 11))

/* For RANGE_START notes, the marker # of the end of the range.  */
#define RANGE_INFO_MARKER_END(INSN) (XINT (INSN, 12))

/* Original pseudo register # for a live range note.  */
#define RANGE_REG_PSEUDO(INSN,N) (XINT (XVECEXP (INSN, 2, N), 0))

/* Pseudo register # original register is copied into or -1.  */
#define RANGE_REG_COPY(INSN,N) (XINT (XVECEXP (INSN, 2, N), 1))

/* How many times a register in a live range note was referenced.  */
#define RANGE_REG_REFS(INSN,N) (XINT (XVECEXP (INSN, 2, N), 2))

/* How many times a register in a live range note was set.  */
#define RANGE_REG_SETS(INSN,N) (XINT (XVECEXP (INSN, 2, N), 3))

/* How many times a register in a live range note died.  */
#define RANGE_REG_DEATHS(INSN,N) (XINT (XVECEXP (INSN, 2, N), 4))

/* Whether the original value is needed to be copied into the range register at
   the start of the range. */
#define RANGE_REG_COPY_FLAGS(INSN,N) (XINT (XVECEXP (INSN, 2, N), 5))

/* # of insns the register copy is live over.  */
#define RANGE_REG_LIVE_LENGTH(INSN,N) (XINT (XVECEXP (INSN, 2, N), 6))

/* # of calls the register copy is live over.  */
#define RANGE_REG_N_CALLS(INSN,N) (XINT (XVECEXP (INSN, 2, N), 7))

/* DECL_NODE pointer of the declaration if the register is a user defined
   variable.  */
#define RANGE_REG_SYMBOL_NODE(INSN,N) (XTREE (XVECEXP (INSN, 2, N), 8))

/* BLOCK_NODE pointer to the block the variable is declared in if the
   register is a user defined variable.  */
#define RANGE_REG_BLOCK_NODE(INSN,N) (XTREE (XVECEXP (INSN, 2, N), 9))

/* EXPR_LIST of the distinct ranges a variable is in.  */
#define RANGE_VAR_LIST(INSN) (XEXP (INSN, 0))

/* Block a variable is declared in.  */
#define RANGE_VAR_BLOCK(INSN) (XTREE (INSN, 1))

/* # of distinct ranges a variable is in.  */
#define RANGE_VAR_NUM(INSN) (XINT (INSN, 2))

/* For a NOTE_INSN_LIVE note, the registers which are currently live.  */
#define RANGE_LIVE_BITMAP(INSN) (XBITMAP (INSN, 0))

/* For a NOTE_INSN_LIVE note, the original basic block number.  */
#define RANGE_LIVE_ORIG_BLOCK(INSN) (XINT (INSN, 1))

/* Generally useful functions.  */

/* The following functions accept a wide integer argument.  Rather than
   having to cast on every function call, we use a macro instead, that is
   defined here and in tree.h.  */

#ifndef exact_log2
#define exact_log2(N) exact_log2_wide ((unsigned HOST_WIDE_INT) (N))
#define floor_log2(N) floor_log2_wide ((unsigned HOST_WIDE_INT) (N))
#endif
extern int exact_log2_wide		PROTO((unsigned HOST_WIDE_INT));
extern int floor_log2_wide		PROTO((unsigned HOST_WIDE_INT));

/* In expmed.c */
extern int ceil_log2			PROTO((unsigned HOST_WIDE_INT));

#define plus_constant(X,C) plus_constant_wide (X, (HOST_WIDE_INT) (C))

#define plus_constant_for_output(X,C)  \
  plus_constant_for_output_wide (X, (HOST_WIDE_INT) (C))

/* In explow.c */
extern rtx plus_constant_wide		 PROTO((rtx, HOST_WIDE_INT));
extern rtx plus_constant_for_output_wide PROTO((rtx, HOST_WIDE_INT));
extern void optimize_save_area_alloca	PROTO((rtx));

extern rtx gen_rtx			PVPROTO((enum rtx_code,
						 enum machine_mode, ...));
extern rtvec gen_rtvec			PVPROTO((int, ...));

#ifdef BUFSIZ
extern rtx read_rtx			PROTO((FILE *));
#endif

extern char *oballoc			PROTO((int));
extern char *permalloc			PROTO((int));
extern rtx rtx_alloc			PROTO((RTX_CODE));
extern rtvec rtvec_alloc		PROTO((int));
extern rtx copy_rtx			PROTO((rtx));
extern rtx copy_rtx_if_shared		PROTO((rtx));
extern rtx copy_most_rtx		PROTO((rtx, rtx));
extern rtx shallow_copy_rtx		PROTO((rtx));
extern rtvec gen_rtvec_v		PROTO((int, rtx *));
extern rtvec gen_rtvec_vv		PROTO((int, rtunion *));
extern rtx gen_reg_rtx			PROTO((enum machine_mode));
extern rtx gen_label_rtx		PROTO((void));
extern rtx gen_inline_header_rtx	PROTO((rtx, rtx, int, int, int, int,
					       int, int, rtx, rtx, int, int,
					       rtvec, rtx,
					       rtvec, char *, char *, rtvec));
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
extern rtx convert_memory_address	PROTO((enum machine_mode, rtx));
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
extern rtx assign_stack_local		PROTO((enum machine_mode,
					       HOST_WIDE_INT, int));
extern rtx assign_stack_temp		PROTO((enum machine_mode,
					       HOST_WIDE_INT, int));
extern rtx assign_temp			PROTO((union tree_node *,
					       int, int, int));
extern rtx protect_from_queue		PROTO((rtx, int));
extern void emit_queue			PROTO((void));
extern rtx emit_move_insn		PROTO((rtx, rtx));
extern rtx emit_insn_before		PROTO((rtx, rtx));
extern rtx emit_jump_insn_before	PROTO((rtx, rtx));
extern rtx emit_call_insn_before	PROTO((rtx, rtx));
extern rtx emit_barrier_before		PROTO((rtx));
extern rtx emit_label_before		PROTO((rtx, rtx));
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
extern rtx emit_insns_after		PROTO((rtx, rtx));
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
extern rtx gen_move_insn		PROTO((rtx, rtx));
extern rtx gen_jump			PROTO((rtx));
extern rtx gen_beq			PROTO((rtx));
extern rtx gen_bge			PROTO((rtx));
extern rtx gen_ble			PROTO((rtx));
extern rtx gen_mem_addressof		PROTO((rtx, union tree_node *));
extern rtx eliminate_constant_term	PROTO((rtx, rtx *));
extern rtx expand_complex_abs		PROTO((enum machine_mode, rtx, rtx, int));
extern enum machine_mode choose_hard_reg_mode PROTO((int, int));
extern void set_unique_reg_note         PROTO((rtx, enum reg_note, rtx));

/* Functions in rtlanal.c */

extern int rtx_unstable_p		PROTO((rtx));
extern int rtx_varies_p			PROTO((rtx));
extern int rtx_addr_varies_p		PROTO((rtx));
extern HOST_WIDE_INT get_integer_term	PROTO((rtx));
extern rtx get_related_value		PROTO((rtx));
extern int reg_mentioned_p		PROTO((rtx, rtx));
extern int reg_referenced_p		PROTO((rtx, rtx));
extern int reg_used_between_p		PROTO((rtx, rtx, rtx));
extern int reg_referenced_between_p	PROTO((rtx, rtx, rtx));
extern int reg_set_between_p		PROTO((rtx, rtx, rtx));
extern int regs_set_between_p		PROTO((rtx, rtx, rtx));
extern int modified_between_p		PROTO((rtx, rtx, rtx));
extern int no_labels_between_p		PROTO((rtx, rtx));
extern int no_jumps_between_p		PROTO((rtx, rtx));
extern int modified_in_p		PROTO((rtx, rtx));
extern int reg_set_p			PROTO((rtx, rtx));
extern rtx single_set			PROTO((rtx));
extern int multiple_sets		PROTO((rtx));
extern rtx find_last_value		PROTO((rtx, rtx *, rtx, int));
extern int refers_to_regno_p		PROTO((int, int, rtx, rtx *));
extern int reg_overlap_mentioned_p	PROTO((rtx, rtx));
extern void note_stores			PROTO((rtx, void (*)(rtx, rtx)));
extern rtx reg_set_last			PROTO((rtx, rtx));
extern int rtx_equal_p			PROTO((rtx, rtx));
extern int dead_or_set_p		PROTO((rtx, rtx));
extern int dead_or_set_regno_p		PROTO((rtx, int));
extern rtx find_reg_note		PROTO((rtx, enum reg_note, rtx));
extern rtx find_regno_note		PROTO((rtx, enum reg_note, int));
extern int find_reg_fusage		PROTO((rtx, enum rtx_code, rtx));
extern int find_regno_fusage		PROTO((rtx, enum rtx_code, int));
extern void remove_note			PROTO((rtx, rtx));
extern int side_effects_p		PROTO((rtx));
extern int volatile_refs_p		PROTO((rtx));
extern int volatile_insn_p		PROTO((rtx));
extern int may_trap_p			PROTO((rtx));
extern int inequality_comparisons_p	PROTO ((rtx));
extern rtx replace_rtx			PROTO((rtx, rtx, rtx));
extern rtx replace_regs			PROTO((rtx, rtx *, int, int));
extern int computed_jump_p		PROTO((rtx));
typedef int (*rtx_function)             PROTO((rtx *, void *));
extern int for_each_rtx                 PROTO((rtx *, rtx_function, void *));
extern rtx regno_use_in			PROTO((int, rtx));
extern int auto_inc_p			PROTO((rtx));
extern void remove_node_from_expr_list	PROTO((rtx, rtx *));

/* flow.c */

extern rtx find_use_as_address		PROTO((rtx, rtx, HOST_WIDE_INT));

/* regclass.c */

/* Maximum number of parallel sets and clobbers in any insn in this fn.
   Always at least 3, since the combiner could put that many togetherm
   and we want this to remain correct for all the remaining passes.  */

extern int max_parallel;

/* Free up register info memory.  */
extern void free_reg_info		PROTO((void));

/* recog.c */
extern int asm_noperands		PROTO((rtx));
extern char *decode_asm_operands	PROTO((rtx, rtx *, rtx **,
					       const char **,
					       enum machine_mode *));

extern enum reg_class reg_preferred_class PROTO((int));
extern enum reg_class reg_alternate_class PROTO((int));

extern rtx get_first_nonparm_insn	PROTO((void));

extern void split_block_insns		PROTO((int, int));
extern void update_flow_info		PROTO((rtx, rtx, rtx, rtx));

/* Standard pieces of rtx, to be substituted directly into things.  */
#define pc_rtx		(&global_rtl.pc_val)
#define cc0_rtx		(&global_rtl.cc0_val)

#define MAX_SAVED_CONST_INT 64
extern struct rtx_def const_int_rtx[MAX_SAVED_CONST_INT * 2 + 1];

#define const0_rtx	(&const_int_rtx[MAX_SAVED_CONST_INT])
#define const1_rtx	(&const_int_rtx[MAX_SAVED_CONST_INT+1])
#define const2_rtx	(&const_int_rtx[MAX_SAVED_CONST_INT+2])
#define constm1_rtx	(&const_int_rtx[MAX_SAVED_CONST_INT-1])
extern rtx const_true_rtx;

extern rtx const_tiny_rtx[3][(int) MAX_MACHINE_MODE];

/* Returns a constant 0 rtx in mode MODE.  Integer modes are treated the 
   same as VOIDmode.  */

#define CONST0_RTX(MODE) (const_tiny_rtx[0][(int) (MODE)])

/* Likewise, for the constants 1 and 2.  */

#define CONST1_RTX(MODE) (const_tiny_rtx[1][(int) (MODE)])
#define CONST2_RTX(MODE) (const_tiny_rtx[2][(int) (MODE)])

extern struct _global_rtl
{
  struct rtx_def pc_val, cc0_val;
  struct rtx_def stack_pointer_val, frame_pointer_val;
  struct rtx_def hard_frame_pointer_val;
  struct rtx_def arg_pointer_val;
  struct rtx_def virtual_incoming_args_val;
  struct rtx_def virtual_stack_vars_val;
  struct rtx_def virtual_stack_dynamic_val;
  struct rtx_def virtual_outgoing_args_val;
  struct rtx_def virtual_cfa_val;
} global_rtl;

/* All references to certain hard regs, except those created
   by allocating pseudo regs into them (when that's possible),
   go through these unique rtx objects.  */
#define stack_pointer_rtx	(&global_rtl.stack_pointer_val)
#define frame_pointer_rtx	(&global_rtl.frame_pointer_val)

extern rtx pic_offset_table_rtx;
extern rtx struct_value_rtx;
extern rtx struct_value_incoming_rtx;
extern rtx static_chain_rtx;
extern rtx static_chain_incoming_rtx;
extern rtx return_address_pointer_rtx;

/* Include the RTL generation functions.  */

#ifndef NO_GENRTL_H
#include "genrtl.h"
#endif

/* There are some RTL codes that require special attention; the
   generation functions included above do the raw handling.  If you
   add to this list, modify special_rtx in gengenrtl.c as well.  You
   should also modify gen_rtx to use the special function.  */

extern rtx gen_rtx_CONST_DOUBLE PROTO((enum machine_mode, rtx,
				       HOST_WIDE_INT, HOST_WIDE_INT));
extern rtx gen_rtx_CONST_INT PROTO((enum machine_mode, HOST_WIDE_INT));
extern rtx gen_rtx_REG PROTO((enum machine_mode, int));
extern rtx gen_rtx_MEM PROTO((enum machine_mode, rtx));

/* We need the cast here to ensure that we get the same result both with
   and without prototypes.  */
#define GEN_INT(N)  gen_rtx_CONST_INT (VOIDmode, (HOST_WIDE_INT) (N))


/* If HARD_FRAME_POINTER_REGNUM is defined, then a special dummy reg
   is used to represent the frame pointer.  This is because the
   hard frame pointer and the automatic variables are separated by an amount
   that cannot be determined until after register allocation.  We can assume
   that in this case ELIMINABLE_REGS will be defined, one action of which
   will be to eliminate FRAME_POINTER_REGNUM into HARD_FRAME_POINTER_REGNUM. */
#ifndef HARD_FRAME_POINTER_REGNUM
#define HARD_FRAME_POINTER_REGNUM FRAME_POINTER_REGNUM
#endif

/* For register elimination to work properly these hard_frame_pointer_rtx,
   frame_pointer_rtx, and arg_pointer_rtx must be the same if they refer to
   the same register.  */
#if HARD_FRAME_POINTER_REGNUM == FRAME_POINTER_REGNUM
#define hard_frame_pointer_rtx	(&global_rtl.frame_pointer_val)
#else
#define hard_frame_pointer_rtx	(&global_rtl.hard_frame_pointer_val)
#endif

#if FRAME_POINTER_REGNUM == ARG_POINTER_REGNUM
#define arg_pointer_rtx		(&global_rtl.frame_pointer_val)
#else
#if HARD_FRAME_POINTER_REGNUM == ARG_POINTER_REGNUM
#define arg_pointer_rtx		(&global_rtl.hard_frame_pointer_val)
#else
#define arg_pointer_rtx		(&global_rtl.arg_pointer_val)
#endif
#endif

/* Virtual registers are used during RTL generation to refer to locations into
   the stack frame when the actual location isn't known until RTL generation
   is complete.  The routine instantiate_virtual_regs replaces these with
   the proper value, which is normally {frame,arg,stack}_pointer_rtx plus
   a constant.  */

#define FIRST_VIRTUAL_REGISTER	(FIRST_PSEUDO_REGISTER)

/* This points to the first word of the incoming arguments passed on the stack,
   either by the caller or by the callee when pretending it was passed by the
   caller.  */

#define virtual_incoming_args_rtx (&global_rtl.virtual_incoming_args_val)

#define VIRTUAL_INCOMING_ARGS_REGNUM	(FIRST_VIRTUAL_REGISTER)

/* If FRAME_GROWS_DOWNWARD, this points to immediately above the first
   variable on the stack.  Otherwise, it points to the first variable on
   the stack.  */

#define virtual_stack_vars_rtx	(&global_rtl.virtual_stack_vars_val)

#define VIRTUAL_STACK_VARS_REGNUM	((FIRST_VIRTUAL_REGISTER) + 1)

/* This points to the location of dynamically-allocated memory on the stack
   immediately after the stack pointer has been adjusted by the amount
   desired.  */

#define virtual_stack_dynamic_rtx	(&global_rtl.virtual_stack_dynamic_val)

#define VIRTUAL_STACK_DYNAMIC_REGNUM	((FIRST_VIRTUAL_REGISTER) + 2)

/* This points to the location in the stack at which outgoing arguments should
   be written when the stack is pre-pushed (arguments pushed using push
   insns always use sp).  */

#define virtual_outgoing_args_rtx	(&global_rtl.virtual_outgoing_args_val)

#define VIRTUAL_OUTGOING_ARGS_REGNUM	((FIRST_VIRTUAL_REGISTER) + 3)

/* This points to the Canonical Frame Address of the function.  This
   should corrospond to the CFA produced by INCOMING_FRAME_SP_OFFSET,
   but is calculated relative to the arg pointer for simplicity; the
   frame pointer nor stack pointer are necessarily fixed relative to 
   the CFA until after reload.  */

#define virtual_cfa_rtx			(&global_rtl.virtual_cfa_val)

#define VIRTUAL_CFA_REGNUM		((FIRST_VIRTUAL_REGISTER) + 4)

#define LAST_VIRTUAL_REGISTER		((FIRST_VIRTUAL_REGISTER) + 4)

extern rtx find_next_ref		PROTO((rtx, rtx));
extern rtx *find_single_use		PROTO((rtx, rtx, rtx *));

extern rtx output_constant_def		PROTO((union tree_node *));
extern rtx immed_real_const		PROTO((union tree_node *));
extern union tree_node *make_tree	PROTO((union tree_node *, rtx));

/* Define a default value for STORE_FLAG_VALUE.  */

#ifndef STORE_FLAG_VALUE
#define STORE_FLAG_VALUE 1
#endif

/* Nonzero after the second flow pass has completed.
   Set to 1 or 0 by toplev.c  */
extern int flow2_completed;

/* Nonzero after end of reload pass.
   Set to 1 or 0 by reload1.c.  */

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

/* Set to nonzero before life analysis to indicate that it is unsafe to
   generate any new pseudo registers.  */
extern int no_new_pseudos;

/* Indexed by pseudo register number, gives the rtx for that pseudo.
   Allocated in parallel with regno_pointer_flag.  */
extern rtx *regno_reg_rtx;

/* Vector indexed by regno; contain the alignment in bytes and type
   pointed to for a register that contains a pointer, if known.  */
extern char *regno_pointer_align;
#define REGNO_POINTER_ALIGN(REGNO) regno_pointer_align[REGNO]

/* Translates rtx code to tree code, for those codes needed by
   REAL_ARITHMETIC.  The function returns an int because the caller may not
   know what `enum tree_code' means.  */

extern int rtx_to_tree_code	PROTO((enum rtx_code));

/* In tree.c */
extern void obfree			PROTO ((char *));
struct obstack;
extern void gcc_obstack_init		PROTO ((struct obstack *));
extern void pop_obstacks		PROTO ((void));
extern void push_obstacks		PROTO ((struct obstack *,
						struct obstack *));
#ifdef BUFSIZ
extern int read_skip_spaces		PROTO ((FILE *));
#endif

/* In cse.c */
struct cse_basic_block_data;
extern int rtx_cost			PROTO ((rtx, enum rtx_code));
extern void delete_trivially_dead_insns	PROTO ((rtx, int));
#ifdef BUFSIZ
extern int cse_main			PROTO ((rtx, int, int, FILE *));
#endif
extern void cse_end_of_basic_block	PROTO ((rtx,
						struct cse_basic_block_data *,
						int, int, int));

/* In jump.c */
extern int comparison_dominates_p	PROTO ((enum rtx_code, enum rtx_code));
extern int condjump_p			PROTO ((rtx));
extern rtx condjump_label		PROTO ((rtx));
extern int simplejump_p			PROTO ((rtx));
extern int returnjump_p			PROTO ((rtx));
extern int sets_cc0_p			PROTO ((rtx));
extern int invert_jump			PROTO ((rtx, rtx));
extern int rtx_renumbered_equal_p	PROTO ((rtx, rtx));
extern int true_regnum			PROTO ((rtx));
extern int redirect_jump		PROTO ((rtx, rtx));
extern void jump_optimize		PROTO ((rtx, int, int, int));
extern void rebuild_jump_labels		PROTO ((rtx));
extern void thread_jumps		PROTO ((rtx, int, int));
extern int redirect_exp			PROTO ((rtx *, rtx, rtx, rtx));
extern int rtx_equal_for_thread_p	PROTO ((rtx, rtx, rtx));
extern int invert_exp			PROTO ((rtx, rtx));
extern int can_reverse_comparison_p	PROTO ((rtx, rtx));
extern void delete_for_peephole		PROTO ((rtx, rtx));
extern int condjump_in_parallel_p	PROTO ((rtx));

/* Flags for jump_optimize() */
#define JUMP_CROSS_JUMP		1
#define JUMP_NOOP_MOVES		1
#define JUMP_AFTER_REGSCAN	1

/* In emit-rtl.c. */
extern int max_reg_num				PROTO ((void));
extern int max_label_num			PROTO ((void));
extern int get_first_label_num			PROTO ((void));
extern void delete_insns_since			PROTO ((rtx));
extern void mark_reg_pointer			PROTO ((rtx, int));
extern void mark_user_reg			PROTO ((rtx));
extern void reset_used_flags			PROTO ((rtx));
extern void reorder_insns			PROTO ((rtx, rtx, rtx));
extern int get_max_uid				PROTO ((void));
extern int in_sequence_p			PROTO ((void));
extern void force_next_line_note		PROTO ((void));
extern void init_emit				PROTO ((void));
extern void init_emit_once			PROTO ((int));
extern void push_topmost_sequence		PROTO ((void));
extern void pop_topmost_sequence		PROTO ((void));
extern int subreg_realpart_p			PROTO ((rtx));
extern void reverse_comparison			PROTO ((rtx));
extern void set_new_first_and_last_insn		PROTO ((rtx, rtx));
extern void set_new_first_and_last_label_num	PROTO ((int, int));
extern void unshare_all_rtl			PROTO ((rtx));
extern void set_last_insn			PROTO ((rtx));
extern void link_cc0_insns			PROTO ((rtx));
extern void add_insn				PROTO ((rtx));
extern void add_insn_before			PROTO ((rtx, rtx));
extern void add_insn_after			PROTO ((rtx, rtx));
extern void remove_insn				PROTO ((rtx));
extern void reorder_insns_with_line_notes	PROTO ((rtx, rtx, rtx));
extern void emit_insn_after_with_line_notes	PROTO ((rtx, rtx, rtx));
extern enum rtx_code classify_insn		PROTO ((rtx));
extern void init_virtual_regs                   PROTO ((void));
extern rtx emit					PROTO ((rtx));
/* Query and clear/ restore no_line_numbers.  This is used by the
   switch / case handling in stmt.c to give proper line numbers in
   warnings about unreachable code.  */
int force_line_numbers PROTO((void));
void restore_line_number_status PROTO((int old_value));

/* In insn-emit.c */
extern void add_clobbers		PROTO ((rtx, int));

/* In combine.c */
extern void combine_instructions	PROTO ((rtx, int));
extern int extended_count		PROTO ((rtx, enum machine_mode, int));
extern rtx remove_death			PROTO ((int, rtx));
#ifdef BUFSIZ
extern void dump_combine_stats		PROTO ((FILE *));
extern void dump_combine_total_stats	PROTO ((FILE *));
#endif

/* In sched.c. */
#ifdef BUFSIZ
extern void schedule_insns		PROTO ((FILE *));
#endif
#ifdef HAIFA
extern void fix_sched_param		PROTO ((char *, char *));
#endif

/* In print-rtl.c */
extern void debug_rtx			PROTO ((rtx));
extern void debug_rtx_list		PROTO ((rtx, int));
extern rtx debug_rtx_find		PROTO ((rtx, int));
#ifdef BUFSIZ
extern void print_rtl			PROTO ((FILE *, rtx));
extern int print_rtl_single		PROTO ((FILE *, rtx));
extern void print_inline_rtx		PROTO ((FILE *, rtx, int));
#endif

/* In loop.c */
extern void init_loop			PROTO ((void));
extern rtx libcall_other_reg		PROTO ((rtx, rtx));
#ifdef BUFSIZ
extern void loop_optimize		PROTO ((rtx, FILE *, int, int));
#endif
extern void record_excess_regs		PROTO ((rtx, rtx, rtx *));

/* In function.c */
extern void reposition_prologue_and_epilogue_notes	PROTO ((rtx));
extern void thread_prologue_and_epilogue_insns		PROTO ((rtx));
extern void use_variable				PROTO ((rtx));
extern HOST_WIDE_INT get_frame_size			PROTO ((void));
extern void preserve_rtl_expr_result			PROTO ((rtx));
extern void mark_temp_addr_taken			PROTO ((rtx));
extern void update_temp_slot_address			PROTO ((rtx, rtx));
extern void use_variable_after				PROTO ((rtx, rtx));
extern void purge_addressof				PROTO ((rtx));

/* In reload.c */
extern int operands_match_p		PROTO ((rtx, rtx));
extern int safe_from_earlyclobber	PROTO ((rtx, rtx));

/* In stmt.c */
extern void expand_null_return		PROTO((void));
extern void emit_jump			PROTO ((rtx));
extern int preserve_subexpressions_p	PROTO ((void));

/* List (chain of EXPR_LIST) of labels heading the current handlers for
   nonlocal gotos.  */
extern rtx nonlocal_goto_handler_labels;

/* In expr.c */
extern void init_expr_once		PROTO ((void));
extern void move_by_pieces		PROTO ((rtx, rtx, int, int));


/* In stupid.c */
#ifdef BUFSIZ
extern void stupid_life_analysis	PROTO ((rtx, int, FILE *));
#endif

/* In flow.c */
extern void allocate_bb_life_data	PROTO ((void));
extern void allocate_reg_life_data	PROTO ((void));
extern void recompute_reg_usage		PROTO ((rtx, int));
#ifdef BUFSIZ
extern void dump_flow_info		PROTO ((FILE *));
#endif
extern void free_bb_mem			PROTO ((void));

/* In expmed.c */
extern void init_expmed			PROTO ((void));
extern void expand_inc			PROTO ((rtx, rtx));
extern void expand_dec			PROTO ((rtx, rtx));
extern rtx expand_mult_highpart		PROTO ((enum machine_mode, rtx,
						unsigned HOST_WIDE_INT, rtx,
						int, int));

/* In gcse.c */
#ifdef BUFSIZ
extern int gcse_main			PROTO ((rtx, FILE *));
#endif

/* In global.c */
extern void mark_elimination		PROTO ((int, int));
#ifdef BUFSIZ
extern int global_alloc			PROTO ((FILE *));
extern void dump_global_regs		PROTO ((FILE *));
#endif
#ifdef HARD_CONST
extern void retry_global_alloc		PROTO ((int, HARD_REG_SET));
#endif

/* In regclass.c */
extern int reg_classes_intersect_p	PROTO ((enum reg_class, enum reg_class));
extern int reg_class_subset_p		PROTO ((enum reg_class, enum reg_class));
extern void globalize_reg		PROTO ((int));
extern void init_regs			PROTO ((void));
extern void init_reg_sets		PROTO ((void));
extern void regset_release_memory	PROTO ((void));
extern void regclass_init		PROTO ((void));
extern void regclass			PROTO ((rtx, int));
extern void reg_scan			PROTO ((rtx, int, int));
extern void reg_scan_update		PROTO ((rtx, rtx, int));
extern void fix_register		PROTO ((char *, int, int));

/* In regmove.c */
#ifdef BUFSIZ
extern void regmove_optimize		PROTO ((rtx, int, FILE *));
#endif

/* In reorg.c */
#ifdef BUFSIZ
extern void dbr_schedule		PROTO ((rtx, FILE *));
#endif

/* In optabs.c */
extern void init_optabs			PROTO ((void));

/* In local-alloc.c */
#ifdef BUFSIZ
extern void dump_local_alloc		PROTO ((FILE *));
#endif
extern int local_alloc			PROTO ((void));
extern int function_invariant_p		PROTO ((rtx));

/* In reload1.c */
extern void reload_cse_regs		PROTO ((rtx));
extern void init_reload			PROTO ((void));
extern void mark_home_live		PROTO ((int));
#ifdef BUFSIZ
extern int reload			PROTO ((rtx, int, FILE *));
#endif

/* In caller-save.c */
extern void init_caller_save		PROTO ((void));

/* In profile.c */
extern void init_branch_prob		PROTO ((const char *));
#ifdef BUFSIZ
extern void branch_prob			PROTO ((rtx, FILE *));
extern void end_branch_prob		PROTO ((FILE *));
#endif
extern void output_func_start_profiler	PROTO ((void));

/* In reg-stack.c */
#ifdef BUFSIZ
extern void reg_to_stack		PROTO ((rtx, FILE *));
#endif
extern int stack_regs_mentioned_p	PROTO ((rtx));

/* In fold-const.c */
extern int add_double		PROTO ((HOST_WIDE_INT, HOST_WIDE_INT,
					HOST_WIDE_INT, HOST_WIDE_INT,
					HOST_WIDE_INT *, HOST_WIDE_INT *));
extern int neg_double		PROTO ((HOST_WIDE_INT, HOST_WIDE_INT,
					HOST_WIDE_INT *, HOST_WIDE_INT *));
extern int mul_double		PROTO ((HOST_WIDE_INT, HOST_WIDE_INT,
					HOST_WIDE_INT, HOST_WIDE_INT,
					HOST_WIDE_INT *, HOST_WIDE_INT *));
extern void lshift_double	PROTO ((HOST_WIDE_INT, HOST_WIDE_INT,
					HOST_WIDE_INT, int, HOST_WIDE_INT *,
					HOST_WIDE_INT *, int));
extern void rshift_double	PROTO ((HOST_WIDE_INT, HOST_WIDE_INT,
					HOST_WIDE_INT, int,
					HOST_WIDE_INT *, HOST_WIDE_INT *, int));
extern void lrotate_double	PROTO ((HOST_WIDE_INT, HOST_WIDE_INT,
					HOST_WIDE_INT, int, HOST_WIDE_INT *,
					HOST_WIDE_INT *));
extern void rrotate_double	PROTO ((HOST_WIDE_INT, HOST_WIDE_INT,
					HOST_WIDE_INT, int, HOST_WIDE_INT *,
					HOST_WIDE_INT *));

/* In calls.c */
/* Emit library call.  */                                           
extern void emit_library_call		PVPROTO ((rtx, int, enum machine_mode,
						  int, ...));
extern rtx emit_library_call_value	PVPROTO((rtx, rtx, int,
						 enum machine_mode,
						 int, ...));

/* In unroll.c */
extern int set_dominates_use		PROTO ((int, int, int, rtx, rtx));

/* In varasm.c */
extern void bss_section			PROTO ((void));
extern int in_data_section		PROTO ((void));
extern int supports_one_only		PROTO ((void));

/* In rtl.c */
extern void init_rtl			PROTO ((void));
extern void rtx_free			PROTO ((rtx));

/* In alias.c */
extern int true_dependence		PROTO ((rtx, enum machine_mode, rtx,
						int (*)(rtx)));
extern int read_dependence		PROTO ((rtx, rtx));
extern int anti_dependence		PROTO ((rtx, rtx));
extern int output_dependence		PROTO ((rtx, rtx));
extern void init_alias_once		PROTO ((void));
extern void init_alias_analysis		PROTO ((void));
extern void end_alias_analysis		PROTO ((void));

extern void record_base_value		PROTO ((int, rtx, int));
extern void record_alias_subset         PROTO ((int, int));
extern rtx addr_side_effect_eval	PROTO ((rtx, int, int));

#ifdef STACK_REGS
extern int stack_regs_mentioned		PROTO((rtx insn));
#endif

#endif /* _RTL_H */
