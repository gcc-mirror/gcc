;; Machine description for AArch64 SVE.
;; Copyright (C) 2009-2019 Free Software Foundation, Inc.
;; Contributed by ARM Ltd.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; GCC is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;; The file is organised into the following sections (search for the full
;; line):
;;
;; == General notes
;; ---- Note on the handling of big-endian SVE
;; ---- Description of UNSPEC_PTEST
;; ---- Description of UNSPEC_PRED_Z
;; ---- Note on predicated integer arithemtic and UNSPEC_PRED_X
;; ---- Note on predicated FP arithmetic patterns and GP "strictness"
;;
;; == Moves
;; ---- Moves of single vectors
;; ---- Moves of multiple vectors
;; ---- Moves of predicates
;;
;; == Loads
;; ---- Normal contiguous loads
;; ---- Normal gather loads
;;
;; == Stores
;; ---- Normal contiguous stores
;; ---- Normal scatter stores
;;
;; == Vector creation
;; ---- [INT,FP] Duplicate element
;; ---- [INT,FP] Initialize from individual elements
;; ---- [INT] Linear series
;; ---- [PRED] Duplicate element
;;
;; == Vector decomposition
;; ---- [INT,FP] Extract index
;; ---- [INT,FP] Extract active element
;; ---- [PRED] Extract index
;;
;; == Unary arithmetic
;; ---- [INT] General unary arithmetic corresponding to rtx codes
;; ---- [INT] General unary arithmetic corresponding to unspecs
;; ---- [INT] Zero extension
;; ---- [INT] Logical inverse
;; ---- [FP] General unary arithmetic corresponding to unspecs
;; ---- [PRED] Inverse

;; == Binary arithmetic
;; ---- [INT] General binary arithmetic corresponding to rtx codes
;; ---- [INT] Addition
;; ---- [INT] Subtraction
;; ---- [INT] Take address
;; ---- [INT] Absolute difference
;; ---- [INT] Highpart multiplication
;; ---- [INT] Division
;; ---- [INT] Binary logical operations
;; ---- [INT] Binary logical operations (inverted second input)
;; ---- [INT] Shifts
;; ---- [INT] Shifts (rounding towards 0)
;; ---- [FP] General binary arithmetic corresponding to rtx codes
;; ---- [FP] General binary arithmetic corresponding to unspecs
;; ---- [FP] Addition
;; ---- [FP] Subtraction
;; ---- [FP] Absolute difference
;; ---- [FP] Multiplication
;; ---- [FP] Binary logical operations
;; ---- [FP] Sign copying
;; ---- [FP] Maximum and minimum
;; ---- [PRED] Binary logical operations
;; ---- [PRED] Binary logical operations (inverted second input)
;; ---- [PRED] Binary logical operations (inverted result)
;;
;; == Ternary arithmetic
;; ---- [INT] MLA and MAD
;; ---- [INT] MLS and MSB
;; ---- [INT] Dot product
;; ---- [INT] Sum of absolute differences
;; ---- [FP] General ternary arithmetic corresponding to unspecs
;;
;; == Comparisons and selects
;; ---- [INT,FP] Select based on predicates
;; ---- [INT,FP] Compare and select
;; ---- [INT] Comparisons
;; ---- [INT] While tests
;; ---- [FP] Direct comparisons
;; ---- [FP] Absolute comparisons
;; ---- [PRED] Test bits
;;
;; == Reductions
;; ---- [INT,FP] Conditional reductions
;; ---- [INT] Tree reductions
;; ---- [FP] Tree reductions
;; ---- [FP] Left-to-right reductions
;;
;; == Permutes
;; ---- [INT,FP] General permutes
;; ---- [INT,FP] Special-purpose unary permutes
;; ---- [INT,FP] Special-purpose binary permutes
;; ---- [PRED] Special-purpose unary permutes
;; ---- [PRED] Special-purpose binary permutes
;;
;; == Conversions
;; ---- [INT<-INT] Packs
;; ---- [INT<-INT] Unpacks
;; ---- [INT<-FP] Conversions
;; ---- [INT<-FP] Packs
;; ---- [INT<-FP] Unpacks
;; ---- [FP<-INT] Conversions
;; ---- [FP<-INT] Packs
;; ---- [FP<-INT] Unpacks
;; ---- [FP<-FP] Packs
;; ---- [FP<-FP] Unpacks
;; ---- [PRED<-PRED] Packs
;; ---- [PRED<-PRED] Unpacks

;; =========================================================================
;; == General notes
;; =========================================================================
;;
;; -------------------------------------------------------------------------
;; ---- Note on the handling of big-endian SVE
;; -------------------------------------------------------------------------
;;
;; On big-endian systems, Advanced SIMD mov<mode> patterns act in the
;; same way as movdi or movti would: the first byte of memory goes
;; into the most significant byte of the register and the last byte
;; of memory goes into the least significant byte of the register.
;; This is the most natural ordering for Advanced SIMD and matches
;; the ABI layout for 64-bit and 128-bit vector types.
;;
;; As a result, the order of bytes within the register is what GCC
;; expects for a big-endian target, and subreg offsets therefore work
;; as expected, with the first element in memory having subreg offset 0
;; and the last element in memory having the subreg offset associated
;; with a big-endian lowpart.  However, this ordering also means that
;; GCC's lane numbering does not match the architecture's numbering:
;; GCC always treats the element at the lowest address in memory
;; (subreg offset 0) as element 0, while the architecture treats
;; the least significant end of the register as element 0.
;;
;; The situation for SVE is different.  We want the layout of the
;; SVE register to be same for mov<mode> as it is for maskload<mode>:
;; logically, a mov<mode> load must be indistinguishable from a
;; maskload<mode> whose mask is all true.  We therefore need the
;; register layout to match LD1 rather than LDR.  The ABI layout of
;; SVE types also matches LD1 byte ordering rather than LDR byte ordering.
;;
;; As a result, the architecture lane numbering matches GCC's lane
;; numbering, with element 0 always being the first in memory.
;; However:
;;
;; - Applying a subreg offset to a register does not give the element
;;   that GCC expects: the first element in memory has the subreg offset
;;   associated with a big-endian lowpart while the last element in memory
;;   has subreg offset 0.  We handle this via TARGET_CAN_CHANGE_MODE_CLASS.
;;
;; - We cannot use LDR and STR for spill slots that might be accessed
;;   via subregs, since although the elements have the order GCC expects,
;;   the order of the bytes within the elements is different.  We instead
;;   access spill slots via LD1 and ST1, using secondary reloads to
;;   reserve a predicate register.
;;
;; -------------------------------------------------------------------------
;; ---- Description of UNSPEC_PTEST
;; -------------------------------------------------------------------------
;;
;; SVE provides a PTEST instruction for testing the active lanes of a
;; predicate and setting the flags based on the result.  The associated
;; condition code tests are:
;;
;; - any   (= ne): at least one active bit is set
;; - none  (= eq): all active bits are clear (*)
;; - first (= mi): the first active bit is set
;; - nfrst (= pl): the first active bit is clear (*)
;; - last  (= cc): the last active bit is set
;; - nlast (= cs): the last active bit is clear (*)
;;
;; where the conditions marked (*) are also true when there are no active
;; lanes (i.e. when the governing predicate is a PFALSE).  The flags results
;; of a PTEST use the condition code mode CC_NZC.
;;
;; PTEST is always a .B operation (i.e. it always operates on VNx16BI).
;; This means that for other predicate modes, we need a governing predicate
;; in which all bits are defined.
;;
;; For example, most predicated .H operations ignore the odd bits of the
;; governing predicate, so that an active lane is represented by the
;; bits "1x" and an inactive lane by the bits "0x", where "x" can be
;; any value.  To test a .H predicate, we instead need "10" and "00"
;; respectively, so that the condition only tests the even bits of the
;; predicate.
;;
;; Several instructions set the flags as a side-effect, in the same way
;; that a separate PTEST would.  It's important for code quality that we
;; use these flags results as often as possible, particularly in the case
;; of WHILE* and RDFFR.
;;
;; Also, some of the instructions that set the flags are unpredicated
;; and instead implicitly test all .B, .H, .S or .D elements, as though
;; they were predicated on a PTRUE of that size.  For example, a .S
;; WHILELO sets the flags in the same way as a PTEST with a .S PTRUE
;; would.
;;
;; We therefore need to represent PTEST operations in a way that
;; makes it easy to combine them with both predicated and unpredicated
;; operations, while using a VNx16BI governing predicate for all
;; predicate modes.  We do this using:
;;
;;   (unspec:CC_NZC [gp cast_gp ptrue_flag op] UNSPEC_PTEST)
;;
;; where:
;;
;; - GP is the real VNx16BI governing predicate
;;
;; - CAST_GP is GP cast to the mode of OP.  All bits dropped by casting
;;   GP to CAST_GP are guaranteed to be clear in GP.
;;
;; - PTRUE_FLAG is a CONST_INT (conceptually of mode SI) that has the value
;;   SVE_KNOWN_PTRUE if we know that CAST_GP (rather than GP) is all-true and
;;   SVE_MAYBE_NOT_PTRUE otherwise.
;;
;; - OP is the predicate we want to test, of the same mode as CAST_GP.
;;
;; -------------------------------------------------------------------------
;; ---- Description of UNSPEC_PRED_Z
;; -------------------------------------------------------------------------
;;
;; SVE integer comparisons are predicated and return zero for inactive
;; lanes.  Sometimes we use them with predicates that are all-true and
;; sometimes we use them with general predicates.
;;
;; The integer comparisons also set the flags and so build-in the effect
;; of a PTEST.  We therefore want to be able to combine integer comparison
;; patterns with PTESTs of the result.  One difficulty with doing this is
;; that (as noted above) the PTEST is always a .B operation and so can place
;; stronger requirements on the governing predicate than the comparison does.
;;
;; For example, when applying a separate PTEST to the result of a full-vector
;; .H comparison, the PTEST must be predicated on a .H PTRUE instead of a
;; .B PTRUE.  In constrast, the comparison might be predicated on either
;; a .H PTRUE or a .B PTRUE, since the values of odd-indexed predicate
;; bits don't matter for .H operations.
;;
;; We therefore can't rely on a full-vector comparison using the same
;; predicate register as a following PTEST.  We instead need to remember
;; whether a comparison is known to be a full-vector comparison and use
;; this information in addition to a check for equal predicate registers.
;; At the same time, it's useful to have a common representation for all
;; integer comparisons, so that they can be handled by a single set of
;; patterns.
;;
;; We therefore take a similar approach to UNSPEC_PTEST above and use:
;;
;;   (unspec:<M:VPRED> [gp ptrue_flag (code:M op0 op1)] UNSPEC_PRED_Z)
;;
;; where:
;;
;; - GP is the governing predicate, of mode <M:VPRED>
;;
;; - PTRUE_FLAG is a CONST_INT (conceptually of mode SI) that has the value
;;   SVE_KNOWN_PTRUE if we know that GP is all-true and SVE_MAYBE_NOT_PTRUE
;;   otherwise
;;
;; - CODE is the comparison code
;;
;; - OP0 and OP1 are the values being compared, of mode M
;;
;; The "Z" in UNSPEC_PRED_Z indicates that inactive lanes are zero.
;;
;; -------------------------------------------------------------------------
;; ---- Note on predicated integer arithemtic and UNSPEC_PRED_X
;; -------------------------------------------------------------------------
;;
;; Many SVE integer operations are predicated.  We can generate them
;; from four sources:
;;
;; (1) Using normal unpredicated optabs.  In this case we need to create
;;     an all-true predicate register to act as the governing predicate
;;     for the SVE instruction.  There are no inactive lanes, and thus
;;     the values of inactive lanes don't matter.
;;
;; (2) Using _x ACLE functions.  In this case the function provides a
;;     specific predicate and some lanes might be inactive.  However,
;;     as for (1), the values of the inactive lanes don't matter.
;;     We can make extra lanes active without changing the behavior
;;     (although for code-quality reasons we should avoid doing so
;;     needlessly).
;;
;; (3) Using cond_* optabs that correspond to IFN_COND_* internal functions.
;;     These optabs have a predicate operand that specifies which lanes are
;;     active and another operand that provides the values of inactive lanes.
;;
;; (4) Using _m and _z ACLE functions.  These functions map to the same
;;     patterns as (3), with the _z functions setting inactive lanes to zero
;;     and the _m functions setting the inactive lanes to one of the function
;;     arguments.
;;
;; For (1) and (2) we need a way of attaching the predicate to a normal
;; unpredicated integer operation.  We do this using:
;;
;;   (unspec:M [pred (code:M (op0 op1 ...))] UNSPEC_PRED_X)
;;
;; where (code:M (op0 op1 ...)) is the normal integer operation and PRED
;; is a predicate of mode <M:VPRED>.  PRED might or might not be a PTRUE;
;; it always is for (1), but might not be for (2).
;;
;; The unspec as a whole has the same value as (code:M ...) when PRED is
;; all-true.  It is always semantically valid to replace PRED with a PTRUE,
;; but as noted above, we should only do so if there's a specific benefit.
;;
;; (The "_X" in the unspec is named after the ACLE functions in (2).)
;;
;; For (3) and (4) we can simply use the SVE port's normal representation
;; of a predicate-based select:
;;
;;   (unspec:M [pred (code:M (op0 op1 ...)) inactive] UNSPEC_SEL)
;;
;; where INACTIVE specifies the values of inactive lanes.
;;
;; We can also use the UNSPEC_PRED_X wrapper in the UNSPEC_SEL rather
;; than inserting the integer operation directly.  This is mostly useful
;; if we want the combine pass to merge an integer operation with an explicit
;; vcond_mask (in other words, with a following SEL instruction).  However,
;; it's generally better to merge such operations at the gimple level
;; using (3).
;;
;; -------------------------------------------------------------------------
;; ---- Note on predicated FP arithmetic patterns and GP "strictness"
;; -------------------------------------------------------------------------
;;
;; Most SVE floating-point operations are predicated.  We can generate
;; them from four sources:
;;
;; (1) Using normal unpredicated optabs.  In this case we need to create
;;     an all-true predicate register to act as the governing predicate
;;     for the SVE instruction.  There are no inactive lanes, and thus
;;     the values of inactive lanes don't matter.
;;
;; (2) Using _x ACLE functions.  In this case the function provides a
;;     specific predicate and some lanes might be inactive.  However,
;;     as for (1), the values of the inactive lanes don't matter.
;;
;;     The instruction must have the same exception behavior as the
;;     function call unless things like command-line flags specifically
;;     allow otherwise.  For example, with -ffast-math, it is OK to
;;     raise exceptions for inactive lanes, but normally it isn't.
;;
;; (3) Using cond_* optabs that correspond to IFN_COND_* internal functions.
;;     These optabs have a predicate operand that specifies which lanes are
;;     active and another operand that provides the values of inactive lanes.
;;
;; (4) Using _m and _z ACLE functions.  These functions map to the same
;;     patterns as (3), with the _z functions setting inactive lanes to zero
;;     and the _m functions setting the inactive lanes to one of the function
;;     arguments.
;;
;; So:
;;
;; - In (1), the predicate is known to be all true and the pattern can use
;;   unpredicated operations where available.
;;
;; - In (2), the predicate might or might not be all true.  The pattern can
;;   use unpredicated instructions if the predicate is all-true or if things
;;   like command-line flags allow exceptions for inactive lanes.
;;
;; - (3) and (4) represent a native SVE predicated operation.  Some lanes
;;   might be inactive and inactive lanes of the result must have specific
;;   values.  There is no scope for using unpredicated instructions (and no
;;   reason to want to), so the question about command-line flags doesn't
;;   arise.
;;
;; It would be inaccurate to model (2) as an rtx code like (sqrt ...)
;; in combination with a separate predicate operand, e.g.
;;
;;   (unspec [(match_operand:<VPRED> 1 "register_operand" "Upl")
;;	      (sqrt:SVE_F 2 "register_operand" "w")]
;;	     ....)
;;
;; because (sqrt ...) can raise an exception for any lane, including
;; inactive ones.  We therefore need to use an unspec instead.
;;
;; Also, (2) requires some way of distinguishing the case in which the
;; predicate might have inactive lanes and cannot be changed from the
;; case in which the predicate has no inactive lanes or can be changed.
;; This information is also useful when matching combined FP patterns
;; in which the predicates might not be equal.
;;
;; We therefore model FP operations as an unspec of the form:
;;
;;   (unspec [pred strictness op0 op1 ...] UNSPEC_COND_<MNEMONIC>)
;;
;; where:
;;
;; - PRED is the governing predicate.
;;
;; - STRICTNESS is a CONST_INT that conceptually has mode SI.  It has the
;;   value SVE_STRICT_GP if PRED might have inactive lanes and if those
;;   lanes must remain inactive.  It has the value SVE_RELAXED_GP otherwise.
;;
;; - OP0 OP1 ... are the normal input operands to the operation.
;;
;; - MNEMONIC is the mnemonic of the associated SVE instruction.

;; =========================================================================
;; == Moves
;; =========================================================================

;; -------------------------------------------------------------------------
;; ---- Moves of single vectors
;; -------------------------------------------------------------------------
;; Includes:
;; - MOV  (including aliases)
;; - LD1B (contiguous form)
;; - LD1D (    "    "     )
;; - LD1H (    "    "     )
;; - LD1W (    "    "     )
;; - LDR
;; - ST1B (contiguous form)
;; - ST1D (    "    "     )
;; - ST1H (    "    "     )
;; - ST1W (    "    "     )
;; - STR
;; -------------------------------------------------------------------------

(define_expand "mov<mode>"
  [(set (match_operand:SVE_ALL 0 "nonimmediate_operand")
	(match_operand:SVE_ALL 1 "general_operand"))]
  "TARGET_SVE"
  {
    /* Use the predicated load and store patterns where possible.
       This is required for big-endian targets (see the comment at the
       head of the file) and increases the addressing choices for
       little-endian.  */
    if ((MEM_P (operands[0]) || MEM_P (operands[1]))
	&& can_create_pseudo_p ())
      {
	aarch64_expand_sve_mem_move (operands[0], operands[1], <VPRED>mode);
	DONE;
      }

    if (CONSTANT_P (operands[1]))
      {
	aarch64_expand_mov_immediate (operands[0], operands[1]);
	DONE;
      }

    /* Optimize subregs on big-endian targets: we can use REV[BHW]
       instead of going through memory.  */
    if (BYTES_BIG_ENDIAN
	&& aarch64_maybe_expand_sve_subreg_move (operands[0], operands[1]))
      DONE;
  }
)

(define_expand "movmisalign<mode>"
  [(set (match_operand:SVE_ALL 0 "nonimmediate_operand")
	(match_operand:SVE_ALL 1 "general_operand"))]
  "TARGET_SVE"
  {
    /* Equivalent to a normal move for our purpooses.  */
    emit_move_insn (operands[0], operands[1]);
    DONE;
  }
)

;; Unpredicated moves (little-endian).  Only allow memory operations
;; during and after RA; before RA we want the predicated load and
;; store patterns to be used instead.
(define_insn "*aarch64_sve_mov<mode>_le"
  [(set (match_operand:SVE_ALL 0 "aarch64_sve_nonimmediate_operand" "=w, Utr, w, w")
	(match_operand:SVE_ALL 1 "aarch64_sve_general_operand" "Utr, w, w, Dn"))]
  "TARGET_SVE
   && !BYTES_BIG_ENDIAN
   && ((lra_in_progress || reload_completed)
       || (register_operand (operands[0], <MODE>mode)
	   && nonmemory_operand (operands[1], <MODE>mode)))"
  "@
   ldr\t%0, %1
   str\t%1, %0
   mov\t%0.d, %1.d
   * return aarch64_output_sve_mov_immediate (operands[1]);"
)

;; Unpredicated moves (big-endian).  Memory accesses require secondary
;; reloads.
(define_insn "*aarch64_sve_mov<mode>_be"
  [(set (match_operand:SVE_ALL 0 "register_operand" "=w, w")
	(match_operand:SVE_ALL 1 "aarch64_nonmemory_operand" "w, Dn"))]
  "TARGET_SVE && BYTES_BIG_ENDIAN"
  "@
   mov\t%0.d, %1.d
   * return aarch64_output_sve_mov_immediate (operands[1]);"
)

;; Handle big-endian memory reloads.  We use byte PTRUE for all modes
;; to try to encourage reuse.
;; This pattern needs constraints due to TARGET_SECONDARY_RELOAD hook.
(define_expand "aarch64_sve_reload_be"
  [(parallel
     [(set (match_operand 0)
	   (match_operand 1))
      (clobber (match_operand:VNx16BI 2 "register_operand" "=Upl"))])]
  "TARGET_SVE && BYTES_BIG_ENDIAN"
  {
    /* Create a PTRUE.  */
    emit_move_insn (operands[2], CONSTM1_RTX (VNx16BImode));

    /* Refer to the PTRUE in the appropriate mode for this move.  */
    machine_mode mode = GET_MODE (operands[0]);
    machine_mode pred_mode
      = aarch64_sve_pred_mode (GET_MODE_UNIT_SIZE (mode)).require ();
    rtx pred = gen_lowpart (pred_mode, operands[2]);

    /* Emit a predicated load or store.  */
    aarch64_emit_sve_pred_move (operands[0], pred, operands[1]);
    DONE;
  }
)

;; A predicated move in which the predicate is known to be all-true.
;; Note that this pattern is generated directly by aarch64_emit_sve_pred_move,
;; so changes to this pattern will need changes there as well.
(define_insn_and_split "@aarch64_pred_mov<mode>"
  [(set (match_operand:SVE_ALL 0 "nonimmediate_operand" "=w, w, m")
	(unspec:SVE_ALL
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl, Upl")
	   (match_operand:SVE_ALL 2 "nonimmediate_operand" "w, m, w")]
	  UNSPEC_PRED_X))]
  "TARGET_SVE
   && (register_operand (operands[0], <MODE>mode)
       || register_operand (operands[2], <MODE>mode))"
  "@
   #
   ld1<Vesize>\t%0.<Vetype>, %1/z, %2
   st1<Vesize>\t%2.<Vetype>, %1, %0"
  "&& register_operand (operands[0], <MODE>mode)
   && register_operand (operands[2], <MODE>mode)"
  [(set (match_dup 0) (match_dup 2))]
)

;; A pattern for optimizing SUBREGs that have a reinterpreting effect
;; on big-endian targets; see aarch64_maybe_expand_sve_subreg_move
;; for details.  We use a special predicate for operand 2 to reduce
;; the number of patterns.
(define_insn_and_split "*aarch64_sve_mov<mode>_subreg_be"
  [(set (match_operand:SVE_ALL 0 "aarch64_sve_nonimmediate_operand" "=w")
	(unspec:SVE_ALL
	  [(match_operand:VNx16BI 1 "register_operand" "Upl")
	   (match_operand 2 "aarch64_any_register_operand" "w")]
	  UNSPEC_REV_SUBREG))]
  "TARGET_SVE && BYTES_BIG_ENDIAN"
  "#"
  "&& reload_completed"
  [(const_int 0)]
  {
    aarch64_split_sve_subreg_move (operands[0], operands[1], operands[2]);
    DONE;
  }
)

;; Reinterpret operand 1 in operand 0's mode, without changing its contents.
;; This is equivalent to a subreg on little-endian targets but not for
;; big-endian; see the comment at the head of the file for details.
(define_expand "@aarch64_sve_reinterpret<mode>"
  [(set (match_operand:SVE_ALL 0 "register_operand")
	(unspec:SVE_ALL [(match_operand 1 "aarch64_any_register_operand")]
			UNSPEC_REINTERPRET))]
  "TARGET_SVE"
  {
    if (!BYTES_BIG_ENDIAN)
      {
	emit_move_insn (operands[0], gen_lowpart (<MODE>mode, operands[1]));
	DONE;
      }
  }
)

;; A pattern for handling type punning on big-endian targets.  We use a
;; special predicate for operand 1 to reduce the number of patterns.
(define_insn_and_split "*aarch64_sve_reinterpret<mode>"
  [(set (match_operand:SVE_ALL 0 "register_operand" "=w")
	(unspec:SVE_ALL [(match_operand 1 "aarch64_any_register_operand" "0")]
			UNSPEC_REINTERPRET))]
  "TARGET_SVE"
  "#"
  "&& reload_completed"
  [(set (match_dup 0) (match_dup 1))]
  {
    emit_note (NOTE_INSN_DELETED);
    DONE;
  }
)

;; -------------------------------------------------------------------------
;; ---- Moves of multiple vectors
;; -------------------------------------------------------------------------
;; All patterns in this section are synthetic and split to real
;; instructions after reload.
;; -------------------------------------------------------------------------

(define_expand "mov<mode>"
  [(set (match_operand:SVE_STRUCT 0 "nonimmediate_operand")
	(match_operand:SVE_STRUCT 1 "general_operand"))]
  "TARGET_SVE"
  {
    /* Big-endian loads and stores need to be done via LD1 and ST1;
       see the comment at the head of the file for details.  */
    if ((MEM_P (operands[0]) || MEM_P (operands[1]))
	&& BYTES_BIG_ENDIAN)
      {
	gcc_assert (can_create_pseudo_p ());
	aarch64_expand_sve_mem_move (operands[0], operands[1], <VPRED>mode);
	DONE;
      }

    if (CONSTANT_P (operands[1]))
      {
	aarch64_expand_mov_immediate (operands[0], operands[1]);
	DONE;
      }
  }
)

;; Unpredicated structure moves (little-endian).
(define_insn "*aarch64_sve_mov<mode>_le"
  [(set (match_operand:SVE_STRUCT 0 "aarch64_sve_nonimmediate_operand" "=w, Utr, w, w")
	(match_operand:SVE_STRUCT 1 "aarch64_sve_general_operand" "Utr, w, w, Dn"))]
  "TARGET_SVE && !BYTES_BIG_ENDIAN"
  "#"
  [(set_attr "length" "<insn_length>")]
)

;; Unpredicated structure moves (big-endian).  Memory accesses require
;; secondary reloads.
(define_insn "*aarch64_sve_mov<mode>_be"
  [(set (match_operand:SVE_STRUCT 0 "register_operand" "=w, w")
	(match_operand:SVE_STRUCT 1 "aarch64_nonmemory_operand" "w, Dn"))]
  "TARGET_SVE && BYTES_BIG_ENDIAN"
  "#"
  [(set_attr "length" "<insn_length>")]
)

;; Split unpredicated structure moves into pieces.  This is the same
;; for both big-endian and little-endian code, although it only needs
;; to handle memory operands for little-endian code.
(define_split
  [(set (match_operand:SVE_STRUCT 0 "aarch64_sve_nonimmediate_operand")
	(match_operand:SVE_STRUCT 1 "aarch64_sve_general_operand"))]
  "TARGET_SVE && reload_completed"
  [(const_int 0)]
  {
    rtx dest = operands[0];
    rtx src = operands[1];
    if (REG_P (dest) && REG_P (src))
      aarch64_simd_emit_reg_reg_move (operands, <VSINGLE>mode, <vector_count>);
    else
      for (unsigned int i = 0; i < <vector_count>; ++i)
	{
	  rtx subdest = simplify_gen_subreg (<VSINGLE>mode, dest, <MODE>mode,
					     i * BYTES_PER_SVE_VECTOR);
	  rtx subsrc = simplify_gen_subreg (<VSINGLE>mode, src, <MODE>mode,
					    i * BYTES_PER_SVE_VECTOR);
	  emit_insn (gen_rtx_SET (subdest, subsrc));
	}
    DONE;
  }
)

;; Predicated structure moves.  This works for both endiannesses but in
;; practice is only useful for big-endian.
(define_insn_and_split "@aarch64_pred_mov<mode>"
  [(set (match_operand:SVE_STRUCT 0 "aarch64_sve_struct_nonimmediate_operand" "=w, w, Utx")
	(unspec:SVE_STRUCT
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl, Upl")
	   (match_operand:SVE_STRUCT 2 "aarch64_sve_struct_nonimmediate_operand" "w, Utx, w")]
	  UNSPEC_PRED_X))]
  "TARGET_SVE
   && (register_operand (operands[0], <MODE>mode)
       || register_operand (operands[2], <MODE>mode))"
  "#"
  "&& reload_completed"
  [(const_int 0)]
  {
    for (unsigned int i = 0; i < <vector_count>; ++i)
      {
	rtx subdest = simplify_gen_subreg (<VSINGLE>mode, operands[0],
					   <MODE>mode,
					   i * BYTES_PER_SVE_VECTOR);
	rtx subsrc = simplify_gen_subreg (<VSINGLE>mode, operands[2],
					  <MODE>mode,
					  i * BYTES_PER_SVE_VECTOR);
	aarch64_emit_sve_pred_move (subdest, operands[1], subsrc);
      }
    DONE;
  }
  [(set_attr "length" "<insn_length>")]
)

;; -------------------------------------------------------------------------
;; ---- Moves of predicates
;; -------------------------------------------------------------------------
;; Includes:
;; - MOV
;; - LDR
;; - PFALSE
;; - PTRUE
;; - STR
;; -------------------------------------------------------------------------

(define_expand "mov<mode>"
  [(set (match_operand:PRED_ALL 0 "nonimmediate_operand")
	(match_operand:PRED_ALL 1 "general_operand"))]
  "TARGET_SVE"
  {
    if (GET_CODE (operands[0]) == MEM)
      operands[1] = force_reg (<MODE>mode, operands[1]);

    if (CONSTANT_P (operands[1]))
      {
	aarch64_expand_mov_immediate (operands[0], operands[1]);
	DONE;
      }
  }
)

(define_insn "*aarch64_sve_mov<mode>"
  [(set (match_operand:PRED_ALL 0 "nonimmediate_operand" "=Upa, m, Upa, Upa")
	(match_operand:PRED_ALL 1 "aarch64_mov_operand" "Upa, Upa, m, Dn"))]
  "TARGET_SVE
   && (register_operand (operands[0], <MODE>mode)
       || register_operand (operands[1], <MODE>mode))"
  "@
   mov\t%0.b, %1.b
   str\t%1, %0
   ldr\t%0, %1
   * return aarch64_output_sve_mov_immediate (operands[1]);"
)

;; =========================================================================
;; == Loads
;; =========================================================================

;; -------------------------------------------------------------------------
;; ---- Normal contiguous loads
;; -------------------------------------------------------------------------
;; Includes contiguous forms of:
;; - LD1B
;; - LD1D
;; - LD1H
;; - LD1W
;; - LD2B
;; - LD2D
;; - LD2H
;; - LD2W
;; - LD3B
;; - LD3D
;; - LD3H
;; - LD3W
;; - LD4B
;; - LD4D
;; - LD4H
;; - LD4W
;; -------------------------------------------------------------------------

;; Predicated LD1.
(define_insn "maskload<mode><vpred>"
  [(set (match_operand:SVE_ALL 0 "register_operand" "=w")
	(unspec:SVE_ALL
	  [(match_operand:<VPRED> 2 "register_operand" "Upl")
	   (match_operand:SVE_ALL 1 "memory_operand" "m")]
	  UNSPEC_LD1_SVE))]
  "TARGET_SVE"
  "ld1<Vesize>\t%0.<Vetype>, %2/z, %1"
)

;; Unpredicated LD[234].
(define_expand "vec_load_lanes<mode><vsingle>"
  [(set (match_operand:SVE_STRUCT 0 "register_operand")
	(unspec:SVE_STRUCT
	  [(match_dup 2)
	   (match_operand:SVE_STRUCT 1 "memory_operand")]
	  UNSPEC_LDN))]
  "TARGET_SVE"
  {
    operands[2] = aarch64_ptrue_reg (<VPRED>mode);
  }
)

;; Predicated LD[234].
(define_insn "vec_mask_load_lanes<mode><vsingle>"
  [(set (match_operand:SVE_STRUCT 0 "register_operand" "=w")
	(unspec:SVE_STRUCT
	  [(match_operand:<VPRED> 2 "register_operand" "Upl")
	   (match_operand:SVE_STRUCT 1 "memory_operand" "m")]
	  UNSPEC_LDN))]
  "TARGET_SVE"
  "ld<vector_count><Vesize>\t%0, %2/z, %1"
)

;; -------------------------------------------------------------------------
;; ---- Normal gather loads
;; -------------------------------------------------------------------------
;; Includes gather forms of:
;; - LD1D
;; - LD1W
;; -------------------------------------------------------------------------

;; Unpredicated gather loads.
(define_expand "gather_load<mode>"
  [(set (match_operand:SVE_SD 0 "register_operand")
	(unspec:SVE_SD
	  [(match_dup 5)
	   (match_operand:DI 1 "aarch64_reg_or_zero")
	   (match_operand:<V_INT_EQUIV> 2 "register_operand")
	   (match_operand:DI 3 "const_int_operand")
	   (match_operand:DI 4 "aarch64_gather_scale_operand_<Vesize>")
	   (mem:BLK (scratch))]
	  UNSPEC_LD1_GATHER))]
  "TARGET_SVE"
  {
    operands[5] = aarch64_ptrue_reg (<VPRED>mode);
  }
)

;; Predicated gather loads for 32-bit elements.  Operand 3 is true for
;; unsigned extension and false for signed extension.
(define_insn "mask_gather_load<mode>"
  [(set (match_operand:SVE_S 0 "register_operand" "=w, w, w, w, w")
	(unspec:SVE_S
	  [(match_operand:<VPRED> 5 "register_operand" "Upl, Upl, Upl, Upl, Upl")
	   (match_operand:DI 1 "aarch64_reg_or_zero" "Z, rk, rk, rk, rk")
	   (match_operand:<V_INT_EQUIV> 2 "register_operand" "w, w, w, w, w")
	   (match_operand:DI 3 "const_int_operand" "i, Z, Ui1, Z, Ui1")
	   (match_operand:DI 4 "aarch64_gather_scale_operand_w" "Ui1, Ui1, Ui1, i, i")
	   (mem:BLK (scratch))]
	  UNSPEC_LD1_GATHER))]
  "TARGET_SVE"
  "@
   ld1w\t%0.s, %5/z, [%2.s]
   ld1w\t%0.s, %5/z, [%1, %2.s, sxtw]
   ld1w\t%0.s, %5/z, [%1, %2.s, uxtw]
   ld1w\t%0.s, %5/z, [%1, %2.s, sxtw %p4]
   ld1w\t%0.s, %5/z, [%1, %2.s, uxtw %p4]"
)

;; Predicated gather loads for 64-bit elements.  The value of operand 3
;; doesn't matter in this case.
(define_insn "mask_gather_load<mode>"
  [(set (match_operand:SVE_D 0 "register_operand" "=w, w, w")
	(unspec:SVE_D
	  [(match_operand:<VPRED> 5 "register_operand" "Upl, Upl, Upl")
	   (match_operand:DI 1 "aarch64_reg_or_zero" "Z, rk, rk")
	   (match_operand:<V_INT_EQUIV> 2 "register_operand" "w, w, w")
	   (match_operand:DI 3 "const_int_operand")
	   (match_operand:DI 4 "aarch64_gather_scale_operand_d" "Ui1, Ui1, i")
	   (mem:BLK (scratch))]
	  UNSPEC_LD1_GATHER))]
  "TARGET_SVE"
  "@
   ld1d\t%0.d, %5/z, [%2.d]
   ld1d\t%0.d, %5/z, [%1, %2.d]
   ld1d\t%0.d, %5/z, [%1, %2.d, lsl %p4]"
)

;; =========================================================================
;; == Stores
;; =========================================================================

;; -------------------------------------------------------------------------
;; ---- Normal contiguous stores
;; -------------------------------------------------------------------------
;; Includes contiguous forms of:
;; - ST1B
;; - ST1D
;; - ST1H
;; - ST1W
;; - ST2B
;; - ST2D
;; - ST2H
;; - ST2W
;; - ST3B
;; - ST3D
;; - ST3H
;; - ST3W
;; - ST4B
;; - ST4D
;; - ST4H
;; - ST4W
;; -------------------------------------------------------------------------

;; Predicated ST1.
(define_insn "maskstore<mode><vpred>"
  [(set (match_operand:SVE_ALL 0 "memory_operand" "+m")
	(unspec:SVE_ALL [(match_operand:<VPRED> 2 "register_operand" "Upl")
			 (match_operand:SVE_ALL 1 "register_operand" "w")
			 (match_dup 0)]
			UNSPEC_ST1_SVE))]
  "TARGET_SVE"
  "st1<Vesize>\t%1.<Vetype>, %2, %0"
)

;; Unpredicated ST[234].  This is always a full update, so the dependence
;; on the old value of the memory location (via (match_dup 0)) is redundant.
;; There doesn't seem to be any obvious benefit to treating the all-true
;; case differently though.  In particular, it's very unlikely that we'll
;; only find out during RTL that a store_lanes is dead.
(define_expand "vec_store_lanes<mode><vsingle>"
  [(set (match_operand:SVE_STRUCT 0 "memory_operand")
	(unspec:SVE_STRUCT
	  [(match_dup 2)
	   (match_operand:SVE_STRUCT 1 "register_operand")
	   (match_dup 0)]
	  UNSPEC_STN))]
  "TARGET_SVE"
  {
    operands[2] = aarch64_ptrue_reg (<VPRED>mode);
  }
)

;; Predicated ST[234].
(define_insn "vec_mask_store_lanes<mode><vsingle>"
  [(set (match_operand:SVE_STRUCT 0 "memory_operand" "+m")
	(unspec:SVE_STRUCT
	  [(match_operand:<VPRED> 2 "register_operand" "Upl")
	   (match_operand:SVE_STRUCT 1 "register_operand" "w")
	   (match_dup 0)]
	  UNSPEC_STN))]
  "TARGET_SVE"
  "st<vector_count><Vesize>\t%1, %2, %0"
)

;; -------------------------------------------------------------------------
;; ---- Normal scatter stores
;; -------------------------------------------------------------------------
;; Includes scatter forms of:
;; - ST1D
;; - ST1W
;; -------------------------------------------------------------------------

;; Unpredicated scatter stores.
(define_expand "scatter_store<mode>"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(match_dup 5)
	   (match_operand:DI 0 "aarch64_reg_or_zero")
	   (match_operand:<V_INT_EQUIV> 1 "register_operand")
	   (match_operand:DI 2 "const_int_operand")
	   (match_operand:DI 3 "aarch64_gather_scale_operand_<Vesize>")
	   (match_operand:SVE_SD 4 "register_operand")]
	  UNSPEC_ST1_SCATTER))]
  "TARGET_SVE"
  {
    operands[5] = aarch64_ptrue_reg (<VPRED>mode);
  }
)

;; Predicated scatter stores for 32-bit elements.  Operand 2 is true for
;; unsigned extension and false for signed extension.
(define_insn "mask_scatter_store<mode>"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(match_operand:<VPRED> 5 "register_operand" "Upl, Upl, Upl, Upl, Upl")
	   (match_operand:DI 0 "aarch64_reg_or_zero" "Z, rk, rk, rk, rk")
	   (match_operand:<V_INT_EQUIV> 1 "register_operand" "w, w, w, w, w")
	   (match_operand:DI 2 "const_int_operand" "i, Z, Ui1, Z, Ui1")
	   (match_operand:DI 3 "aarch64_gather_scale_operand_w" "Ui1, Ui1, Ui1, i, i")
	   (match_operand:SVE_S 4 "register_operand" "w, w, w, w, w")]
	  UNSPEC_ST1_SCATTER))]
  "TARGET_SVE"
  "@
   st1w\t%4.s, %5, [%1.s]
   st1w\t%4.s, %5, [%0, %1.s, sxtw]
   st1w\t%4.s, %5, [%0, %1.s, uxtw]
   st1w\t%4.s, %5, [%0, %1.s, sxtw %p3]
   st1w\t%4.s, %5, [%0, %1.s, uxtw %p3]"
)

;; Predicated scatter stores for 64-bit elements.  The value of operand 2
;; doesn't matter in this case.
(define_insn "mask_scatter_store<mode>"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(match_operand:<VPRED> 5 "register_operand" "Upl, Upl, Upl")
	   (match_operand:DI 0 "aarch64_reg_or_zero" "Z, rk, rk")
	   (match_operand:<V_INT_EQUIV> 1 "register_operand" "w, w, w")
	   (match_operand:DI 2 "const_int_operand")
	   (match_operand:DI 3 "aarch64_gather_scale_operand_d" "Ui1, Ui1, i")
	   (match_operand:SVE_D 4 "register_operand" "w, w, w")]
	  UNSPEC_ST1_SCATTER))]
  "TARGET_SVE"
  "@
   st1d\t%4.d, %5, [%1.d]
   st1d\t%4.d, %5, [%0, %1.d]
   st1d\t%4.d, %5, [%0, %1.d, lsl %p3]"
)

;; =========================================================================
;; == Vector creation
;; =========================================================================

;; -------------------------------------------------------------------------
;; ---- [INT,FP] Duplicate element
;; -------------------------------------------------------------------------
;; Includes:
;; - MOV
;; - LD1RB
;; - LD1RD
;; - LD1RH
;; - LD1RW
;; - LD1RQB
;; - LD1RQD
;; - LD1RQH
;; - LD1RQW
;; -------------------------------------------------------------------------

(define_expand "vec_duplicate<mode>"
  [(parallel
    [(set (match_operand:SVE_ALL 0 "register_operand")
	  (vec_duplicate:SVE_ALL
	    (match_operand:<VEL> 1 "aarch64_sve_dup_operand")))
     (clobber (scratch:VNx16BI))])]
  "TARGET_SVE"
  {
    if (MEM_P (operands[1]))
      {
	rtx ptrue = aarch64_ptrue_reg (<VPRED>mode);
	emit_insn (gen_sve_ld1r<mode> (operands[0], ptrue, operands[1],
				       CONST0_RTX (<MODE>mode)));
	DONE;
      }
  }
)

;; Accept memory operands for the benefit of combine, and also in case
;; the scalar input gets spilled to memory during RA.  We want to split
;; the load at the first opportunity in order to allow the PTRUE to be
;; optimized with surrounding code.
(define_insn_and_split "*vec_duplicate<mode>_reg"
  [(set (match_operand:SVE_ALL 0 "register_operand" "=w, w, w")
	(vec_duplicate:SVE_ALL
	  (match_operand:<VEL> 1 "aarch64_sve_dup_operand" "r, w, Uty")))
   (clobber (match_scratch:VNx16BI 2 "=X, X, Upl"))]
  "TARGET_SVE"
  "@
   mov\t%0.<Vetype>, %<vwcore>1
   mov\t%0.<Vetype>, %<Vetype>1
   #"
  "&& MEM_P (operands[1])"
  [(const_int 0)]
  {
    if (GET_CODE (operands[2]) == SCRATCH)
      operands[2] = gen_reg_rtx (VNx16BImode);
    emit_move_insn (operands[2], CONSTM1_RTX (VNx16BImode));
    rtx gp = gen_lowpart (<VPRED>mode, operands[2]);
    emit_insn (gen_sve_ld1r<mode> (operands[0], gp, operands[1],
				   CONST0_RTX (<MODE>mode)));
    DONE;
  }
  [(set_attr "length" "4,4,8")]
)

;; Duplicate an Advanced SIMD vector to fill an SVE vector (LE version).
(define_insn "@aarch64_vec_duplicate_vq<mode>_le"
  [(set (match_operand:SVE_ALL 0 "register_operand" "=w")
	(vec_duplicate:SVE_ALL
	  (match_operand:<V128> 1 "register_operand" "w")))]
  "TARGET_SVE && !BYTES_BIG_ENDIAN"
  {
    operands[1] = gen_rtx_REG (<MODE>mode, REGNO (operands[1]));
    return "dup\t%0.q, %1.q[0]";
  }
)

;; Duplicate an Advanced SIMD vector to fill an SVE vector (BE version).
;; The SVE register layout puts memory lane N into (architectural)
;; register lane N, whereas the Advanced SIMD layout puts the memory
;; lsb into the register lsb.  We therefore have to describe this in rtl
;; terms as a reverse of the V128 vector followed by a duplicate.
(define_insn "@aarch64_vec_duplicate_vq<mode>_be"
  [(set (match_operand:SVE_ALL 0 "register_operand" "=w")
	(vec_duplicate:SVE_ALL
	  (vec_select:<V128>
	    (match_operand:<V128> 1 "register_operand" "w")
	    (match_operand 2 "descending_int_parallel"))))]
  "TARGET_SVE
   && BYTES_BIG_ENDIAN
   && known_eq (INTVAL (XVECEXP (operands[2], 0, 0)),
		GET_MODE_NUNITS (<V128>mode) - 1)"
  {
    operands[1] = gen_rtx_REG (<MODE>mode, REGNO (operands[1]));
    return "dup\t%0.q, %1.q[0]";
  }
)

;; This is used for vec_duplicate<mode>s from memory, but can also
;; be used by combine to optimize selects of a a vec_duplicate<mode>
;; with zero.
(define_insn "sve_ld1r<mode>"
  [(set (match_operand:SVE_ALL 0 "register_operand" "=w")
	(unspec:SVE_ALL
	  [(match_operand:<VPRED> 1 "register_operand" "Upl")
	   (vec_duplicate:SVE_ALL
	     (match_operand:<VEL> 2 "aarch64_sve_ld1r_operand" "Uty"))
	   (match_operand:SVE_ALL 3 "aarch64_simd_imm_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  "ld1r<Vesize>\t%0.<Vetype>, %1/z, %2"
)

;; Load 128 bits from memory under predicate control and duplicate to
;; fill a vector.
(define_insn "@aarch64_sve_ld1rq<mode>"
  [(set (match_operand:SVE_ALL 0 "register_operand" "=w")
	(unspec:SVE_ALL
	  [(match_operand:<VPRED> 2 "register_operand" "Upl")
	   (match_operand:<V128> 1 "aarch64_sve_ld1rq_operand" "UtQ")]
	  UNSPEC_LD1RQ))]
  "TARGET_SVE"
  {
    operands[1] = gen_rtx_MEM (<VEL>mode, XEXP (operands[1], 0));
    return "ld1rq<Vesize>\t%0.<Vetype>, %2/z, %1";
  }
)

;; -------------------------------------------------------------------------
;; ---- [INT,FP] Initialize from individual elements
;; -------------------------------------------------------------------------
;; Includes:
;; - INSR
;; -------------------------------------------------------------------------

(define_expand "vec_init<mode><Vel>"
  [(match_operand:SVE_ALL 0 "register_operand")
    (match_operand 1 "")]
  "TARGET_SVE"
  {
    aarch64_sve_expand_vector_init (operands[0], operands[1]);
    DONE;
  }
)

;; Shift an SVE vector left and insert a scalar into element 0.
(define_insn "vec_shl_insert_<mode>"
  [(set (match_operand:SVE_ALL 0 "register_operand" "=?w, w, ??&w, ?&w")
	(unspec:SVE_ALL
	  [(match_operand:SVE_ALL 1 "register_operand" "0, 0, w, w")
	   (match_operand:<VEL> 2 "aarch64_reg_or_zero" "rZ, w, rZ, w")]
	  UNSPEC_INSR))]
  "TARGET_SVE"
  "@
   insr\t%0.<Vetype>, %<vwcore>2
   insr\t%0.<Vetype>, %<Vetype>2
   movprfx\t%0, %1\;insr\t%0.<Vetype>, %<vwcore>2
   movprfx\t%0, %1\;insr\t%0.<Vetype>, %<Vetype>2"
  [(set_attr "movprfx" "*,*,yes,yes")]
)

;; -------------------------------------------------------------------------
;; ---- [INT] Linear series
;; -------------------------------------------------------------------------
;; Includes:
;; - INDEX
;; -------------------------------------------------------------------------

(define_insn "vec_series<mode>"
  [(set (match_operand:SVE_I 0 "register_operand" "=w, w, w")
	(vec_series:SVE_I
	  (match_operand:<VEL> 1 "aarch64_sve_index_operand" "Usi, r, r")
	  (match_operand:<VEL> 2 "aarch64_sve_index_operand" "r, Usi, r")))]
  "TARGET_SVE"
  "@
   index\t%0.<Vetype>, #%1, %<vw>2
   index\t%0.<Vetype>, %<vw>1, #%2
   index\t%0.<Vetype>, %<vw>1, %<vw>2"
)

;; Optimize {x, x, x, x, ...} + {0, n, 2*n, 3*n, ...} if n is in range
;; of an INDEX instruction.
(define_insn "*vec_series<mode>_plus"
  [(set (match_operand:SVE_I 0 "register_operand" "=w")
	(plus:SVE_I
	  (vec_duplicate:SVE_I
	    (match_operand:<VEL> 1 "register_operand" "r"))
	  (match_operand:SVE_I 2 "immediate_operand")))]
  "TARGET_SVE && aarch64_check_zero_based_sve_index_immediate (operands[2])"
  {
    operands[2] = aarch64_check_zero_based_sve_index_immediate (operands[2]);
    return "index\t%0.<Vetype>, %<vw>1, #%2";
  }
)

;; -------------------------------------------------------------------------
;; ---- [PRED] Duplicate element
;; -------------------------------------------------------------------------
;; The patterns in this section are synthetic.
;; -------------------------------------------------------------------------

;; Implement a predicate broadcast by shifting the low bit of the scalar
;; input into the top bit and using a WHILELO.  An alternative would be to
;; duplicate the input and do a compare with zero.
(define_expand "vec_duplicate<mode>"
  [(set (match_operand:PRED_ALL 0 "register_operand")
	(vec_duplicate:PRED_ALL (match_operand 1 "register_operand")))]
  "TARGET_SVE"
  {
    rtx tmp = gen_reg_rtx (DImode);
    rtx op1 = gen_lowpart (DImode, operands[1]);
    emit_insn (gen_ashldi3 (tmp, op1, gen_int_mode (63, DImode)));
    emit_insn (gen_while_ultdi<mode> (operands[0], const0_rtx, tmp));
    DONE;
  }
)

;; =========================================================================
;; == Vector decomposition
;; =========================================================================

;; -------------------------------------------------------------------------
;; ---- [INT,FP] Extract index
;; -------------------------------------------------------------------------
;; Includes:
;; - DUP    (Advanced SIMD)
;; - DUP    (SVE)
;; - EXT    (SVE)
;; - ST1    (Advanced SIMD)
;; - UMOV   (Advanced SIMD)
;; -------------------------------------------------------------------------

(define_expand "vec_extract<mode><Vel>"
  [(set (match_operand:<VEL> 0 "register_operand")
	(vec_select:<VEL>
	  (match_operand:SVE_ALL 1 "register_operand")
	  (parallel [(match_operand:SI 2 "nonmemory_operand")])))]
  "TARGET_SVE"
  {
    poly_int64 val;
    if (poly_int_rtx_p (operands[2], &val)
	&& known_eq (val, GET_MODE_NUNITS (<MODE>mode) - 1))
      {
	/* The last element can be extracted with a LASTB and a false
	   predicate.  */
	rtx sel = aarch64_pfalse_reg (<VPRED>mode);
	emit_insn (gen_extract_last_<mode> (operands[0], sel, operands[1]));
	DONE;
      }
    if (!CONST_INT_P (operands[2]))
      {
	/* Create an index with operand[2] as the base and -1 as the step.
	   It will then be zero for the element we care about.  */
	rtx index = gen_lowpart (<VEL_INT>mode, operands[2]);
	index = force_reg (<VEL_INT>mode, index);
	rtx series = gen_reg_rtx (<V_INT_EQUIV>mode);
	emit_insn (gen_vec_series<v_int_equiv> (series, index, constm1_rtx));

	/* Get a predicate that is true for only that element.  */
	rtx zero = CONST0_RTX (<V_INT_EQUIV>mode);
	rtx cmp = gen_rtx_EQ (<V_INT_EQUIV>mode, series, zero);
	rtx sel = gen_reg_rtx (<VPRED>mode);
	emit_insn (gen_vec_cmp<v_int_equiv><vpred> (sel, cmp, series, zero));

	/* Select the element using LASTB.  */
	emit_insn (gen_extract_last_<mode> (operands[0], sel, operands[1]));
	DONE;
      }
  }
)

;; Extract element zero.  This is a special case because we want to force
;; the registers to be the same for the second alternative, and then
;; split the instruction into nothing after RA.
(define_insn_and_split "*vec_extract<mode><Vel>_0"
  [(set (match_operand:<VEL> 0 "aarch64_simd_nonimmediate_operand" "=r, w, Utv")
	(vec_select:<VEL>
	  (match_operand:SVE_ALL 1 "register_operand" "w, 0, w")
	  (parallel [(const_int 0)])))]
  "TARGET_SVE"
  {
    operands[1] = gen_rtx_REG (<V128>mode, REGNO (operands[1]));
    switch (which_alternative)
      {
	case 0:
	  return "umov\\t%<vwcore>0, %1.<Vetype>[0]";
	case 1:
	  return "#";
	case 2:
	  return "st1\\t{%1.<Vetype>}[0], %0";
	default:
	  gcc_unreachable ();
      }
  }
  "&& reload_completed
   && REG_P (operands[0])
   && REGNO (operands[0]) == REGNO (operands[1])"
  [(const_int 0)]
  {
    emit_note (NOTE_INSN_DELETED);
    DONE;
  }
  [(set_attr "type" "neon_to_gp_q, untyped, neon_store1_one_lane_q")]
)

;; Extract an element from the Advanced SIMD portion of the register.
;; We don't just reuse the aarch64-simd.md pattern because we don't
;; want any change in lane number on big-endian targets.
(define_insn "*vec_extract<mode><Vel>_v128"
  [(set (match_operand:<VEL> 0 "aarch64_simd_nonimmediate_operand" "=r, w, Utv")
	(vec_select:<VEL>
	  (match_operand:SVE_ALL 1 "register_operand" "w, w, w")
	  (parallel [(match_operand:SI 2 "const_int_operand")])))]
  "TARGET_SVE
   && IN_RANGE (INTVAL (operands[2]) * GET_MODE_SIZE (<VEL>mode), 1, 15)"
  {
    operands[1] = gen_rtx_REG (<V128>mode, REGNO (operands[1]));
    switch (which_alternative)
      {
	case 0:
	  return "umov\\t%<vwcore>0, %1.<Vetype>[%2]";
	case 1:
	  return "dup\\t%<Vetype>0, %1.<Vetype>[%2]";
	case 2:
	  return "st1\\t{%1.<Vetype>}[%2], %0";
	default:
	  gcc_unreachable ();
      }
  }
  [(set_attr "type" "neon_to_gp_q, neon_dup_q, neon_store1_one_lane_q")]
)

;; Extract an element in the range of DUP.  This pattern allows the
;; source and destination to be different.
(define_insn "*vec_extract<mode><Vel>_dup"
  [(set (match_operand:<VEL> 0 "register_operand" "=w")
	(vec_select:<VEL>
	  (match_operand:SVE_ALL 1 "register_operand" "w")
	  (parallel [(match_operand:SI 2 "const_int_operand")])))]
  "TARGET_SVE
   && IN_RANGE (INTVAL (operands[2]) * GET_MODE_SIZE (<VEL>mode), 16, 63)"
  {
    operands[0] = gen_rtx_REG (<MODE>mode, REGNO (operands[0]));
    return "dup\t%0.<Vetype>, %1.<Vetype>[%2]";
  }
)

;; Extract an element outside the range of DUP.  This pattern requires the
;; source and destination to be the same.
(define_insn "*vec_extract<mode><Vel>_ext"
  [(set (match_operand:<VEL> 0 "register_operand" "=w, ?&w")
	(vec_select:<VEL>
	  (match_operand:SVE_ALL 1 "register_operand" "0, w")
	  (parallel [(match_operand:SI 2 "const_int_operand")])))]
  "TARGET_SVE && INTVAL (operands[2]) * GET_MODE_SIZE (<VEL>mode) >= 64"
  {
    operands[0] = gen_rtx_REG (<MODE>mode, REGNO (operands[0]));
    operands[2] = GEN_INT (INTVAL (operands[2]) * GET_MODE_SIZE (<VEL>mode));
    return (which_alternative == 0
	    ? "ext\t%0.b, %0.b, %0.b, #%2"
	    : "movprfx\t%0, %1\;ext\t%0.b, %0.b, %1.b, #%2");
  }
  [(set_attr "movprfx" "*,yes")]
)

;; -------------------------------------------------------------------------
;; ---- [INT,FP] Extract active element
;; -------------------------------------------------------------------------
;; Includes:
;; - LASTB
;; -------------------------------------------------------------------------

;; Extract the last active element of operand 1 into operand 0.
;; If no elements are active, extract the last inactive element instead.
(define_insn "extract_last_<mode>"
  [(set (match_operand:<VEL> 0 "register_operand" "=r, w")
	(unspec:<VEL>
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl")
	   (match_operand:SVE_ALL 2 "register_operand" "w, w")]
	  UNSPEC_LASTB))]
  "TARGET_SVE"
  "@
   lastb\t%<vwcore>0, %1, %2.<Vetype>
   lastb\t%<Vetype>0, %1, %2.<Vetype>"
)

;; -------------------------------------------------------------------------
;; ---- [PRED] Extract index
;; -------------------------------------------------------------------------
;; The patterns in this section are synthetic.
;; -------------------------------------------------------------------------

;; Handle extractions from a predicate by converting to an integer vector
;; and extracting from there.
(define_expand "vec_extract<vpred><Vel>"
  [(match_operand:<VEL> 0 "register_operand")
   (match_operand:<VPRED> 1 "register_operand")
   (match_operand:SI 2 "nonmemory_operand")
   ;; Dummy operand to which we can attach the iterator.
   (reg:SVE_I V0_REGNUM)]
  "TARGET_SVE"
  {
    rtx tmp = gen_reg_rtx (<MODE>mode);
    emit_insn (gen_vcond_mask_<mode><vpred> (tmp, operands[1],
					     CONST1_RTX (<MODE>mode),
					     CONST0_RTX (<MODE>mode)));
    emit_insn (gen_vec_extract<mode><Vel> (operands[0], tmp, operands[2]));
    DONE;
  }
)

;; =========================================================================
;; == Unary arithmetic
;; =========================================================================

;; -------------------------------------------------------------------------
;; ---- [INT] General unary arithmetic corresponding to rtx codes
;; -------------------------------------------------------------------------
;; Includes:
;; - ABS
;; - CLS (= clrsb)
;; - CLZ
;; - CNT (= popcount)
;; - NEG
;; - NOT
;; -------------------------------------------------------------------------

;; Unpredicated integer unary arithmetic.
(define_expand "<optab><mode>2"
  [(set (match_operand:SVE_I 0 "register_operand")
	(unspec:SVE_I
	  [(match_dup 2)
	   (SVE_INT_UNARY:SVE_I (match_operand:SVE_I 1 "register_operand"))]
	  UNSPEC_PRED_X))]
  "TARGET_SVE"
  {
    operands[2] = aarch64_ptrue_reg (<VPRED>mode);
  }
)

;; Integer unary arithmetic predicated with a PTRUE.
(define_insn "*<optab><mode>2"
  [(set (match_operand:SVE_I 0 "register_operand" "=w")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand" "Upl")
	   (SVE_INT_UNARY:SVE_I
	     (match_operand:SVE_I 2 "register_operand" "w"))]
	  UNSPEC_PRED_X))]
  "TARGET_SVE"
  "<sve_int_op>\t%0.<Vetype>, %1/m, %2.<Vetype>"
)

;; Predicated integer unary arithmetic, merging with the first input.
(define_insn "*cond_<optab><mode>_2"
  [(set (match_operand:SVE_I 0 "register_operand" "=w, ?&w")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl")
	   (SVE_INT_UNARY:SVE_I
	     (match_operand:SVE_I 2 "register_operand" "0, w"))
	   (match_dup 2)]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  "@
   <sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>
   movprfx\t%0, %2\;<sve_int_op>\t%0.<Vetype>, %1/m, %2.<Vetype>"
  [(set_attr "movprfx" "*,yes")]
)

;; Predicated integer unary arithmetic, merging with an independent value.
;;
;; The earlyclobber isn't needed for the first alternative, but omitting
;; it would only help the case in which operands 2 and 3 are the same,
;; which is handled above rather than here.  Marking all the alternatives
;; as earlyclobber helps to make the instruction more regular to the
;; register allocator.
(define_insn "*cond_<optab><mode>_any"
  [(set (match_operand:SVE_I 0 "register_operand" "=&w, ?&w, ?&w")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl, Upl")
	   (SVE_INT_UNARY:SVE_I
	     (match_operand:SVE_I 2 "register_operand" "w, w, w"))
	   (match_operand:SVE_I 3 "aarch64_simd_reg_or_zero" "0, Dz, w")]
	  UNSPEC_SEL))]
  "TARGET_SVE && !rtx_equal_p (operands[2], operands[3])"
  "@
   <sve_int_op>\t%0.<Vetype>, %1/m, %2.<Vetype>
   movprfx\t%0.<Vetype>, %1/z, %2.<Vetype>\;<sve_int_op>\t%0.<Vetype>, %1/m, %2.<Vetype>
   movprfx\t%0, %3\;<sve_int_op>\t%0.<Vetype>, %1/m, %2.<Vetype>"
  [(set_attr "movprfx" "*,yes,yes")]
)

;; -------------------------------------------------------------------------
;; ---- [INT] General unary arithmetic corresponding to unspecs
;; -------------------------------------------------------------------------
;; Includes
;; - REVB
;; - REVH
;; - REVW
;; -------------------------------------------------------------------------

;; Predicated integer unary operations.
(define_insn "@aarch64_pred_<optab><mode>"
  [(set (match_operand:SVE_I 0 "register_operand" "=w")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand" "Upl")
	   (unspec:SVE_I
	     [(match_operand:SVE_I 2 "register_operand" "w")]
	     SVE_INT_UNARY)]
	  UNSPEC_PRED_X))]
  "TARGET_SVE && <elem_bits> >= <min_elem_bits>"
  "<sve_int_op>\t%0.<Vetype>, %1/m, %2.<Vetype>"
)

;; -------------------------------------------------------------------------
;; ---- [INT] Zero extension
;; -------------------------------------------------------------------------
;; Includes:
;; - UXTB
;; - UXTH
;; - UXTW
;; -------------------------------------------------------------------------

;; Match UXT[BHW] as a conditional AND of a constant, merging with the
;; first input.
(define_insn "*cond_uxt<mode>_2"
  [(set (match_operand:SVE_I 0 "register_operand" "=w, ?&w")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl")
	   (and:SVE_I
	     (match_operand:SVE_I 2 "register_operand" "0, w")
	     (match_operand:SVE_I 3 "aarch64_sve_uxt_immediate"))
	   (match_dup 2)]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  "@
   uxt%e3\t%0.<Vetype>, %1/m, %0.<Vetype>
   movprfx\t%0, %2\;uxt%e3\t%0.<Vetype>, %1/m, %2.<Vetype>"
  [(set_attr "movprfx" "*,yes")]
)

;; Match UXT[BHW] as a conditional AND of a constant, merging with an
;; independent value.
;;
;; The earlyclobber isn't needed for the first alternative, but omitting
;; it would only help the case in which operands 2 and 4 are the same,
;; which is handled above rather than here.  Marking all the alternatives
;; as early-clobber helps to make the instruction more regular to the
;; register allocator.
(define_insn "*cond_uxt<mode>_any"
  [(set (match_operand:SVE_I 0 "register_operand" "=&w, ?&w, ?&w")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl, Upl")
	   (and:SVE_I
	     (match_operand:SVE_I 2 "register_operand" "w, w, w")
	     (match_operand:SVE_I 3 "aarch64_sve_uxt_immediate"))
	   (match_operand:SVE_I 4 "aarch64_simd_reg_or_zero" "0, Dz, w")]
	  UNSPEC_SEL))]
  "TARGET_SVE && !rtx_equal_p (operands[2], operands[4])"
  "@
   uxt%e3\t%0.<Vetype>, %1/m, %2.<Vetype>
   movprfx\t%0.<Vetype>, %1/z, %2.<Vetype>\;uxt%e3\t%0.<Vetype>, %1/m, %2.<Vetype>
   movprfx\t%0, %4\;uxt%e3\t%0.<Vetype>, %1/m, %2.<Vetype>"
  [(set_attr "movprfx" "*,yes,yes")]
)

;; -------------------------------------------------------------------------
;; ---- [INT] Logical inverse
;; -------------------------------------------------------------------------

;; Predicated logical inverse.
(define_insn "*cnot<mode>"
  [(set (match_operand:SVE_I 0 "register_operand" "=w")
	(unspec:SVE_I
	  [(unspec:<VPRED>
	     [(match_operand:<VPRED> 1 "register_operand" "Upl")
	      (match_operand:SI 5 "aarch64_sve_ptrue_flag")
	      (eq:<VPRED>
		(match_operand:SVE_I 2 "register_operand" "w")
		(match_operand:SVE_I 3 "aarch64_simd_imm_zero"))]
	     UNSPEC_PRED_Z)
	   (match_operand:SVE_I 4 "aarch64_simd_imm_one")
	   (match_dup 3)]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  "cnot\t%0.<Vetype>, %1/m, %2.<Vetype>"
)

;; Predicated logical inverse, merging with the first input.
(define_insn_and_rewrite "*cond_cnot<mode>_2"
  [(set (match_operand:SVE_I 0 "register_operand" "=w, ?&w")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl")
	   ;; Logical inverse of operand 2 (as above).
	   (unspec:SVE_I
	     [(unspec:<VPRED>
		[(match_operand 5)
		 (const_int SVE_KNOWN_PTRUE)
		 (eq:<VPRED>
		   (match_operand:SVE_I 2 "register_operand" "0, w")
		   (match_operand:SVE_I 3 "aarch64_simd_imm_zero"))]
		UNSPEC_PRED_Z)
	      (match_operand:SVE_I 4 "aarch64_simd_imm_one")
	      (match_dup 3)]
	     UNSPEC_SEL)
	   (match_dup 2)]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  "@
   cnot\t%0.<Vetype>, %1/m, %0.<Vetype>
   movprfx\t%0, %2\;cnot\t%0.<Vetype>, %1/m, %2.<Vetype>"
  "&& !CONSTANT_P (operands[5])"
  {
    operands[5] = CONSTM1_RTX (<VPRED>mode);
  }
  [(set_attr "movprfx" "*,yes")]
)

;; Predicated logical inverse, merging with an independent value.
;;
;; The earlyclobber isn't needed for the first alternative, but omitting
;; it would only help the case in which operands 2 and 6 are the same,
;; which is handled above rather than here.  Marking all the alternatives
;; as earlyclobber helps to make the instruction more regular to the
;; register allocator.
(define_insn_and_rewrite "*cond_cnot<mode>_any"
  [(set (match_operand:SVE_I 0 "register_operand" "=&w, ?&w, ?&w")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl, Upl")
	   ;; Logical inverse of operand 2 (as above).
	   (unspec:SVE_I
	     [(unspec:<VPRED>
		[(match_operand 5)
		 (const_int SVE_KNOWN_PTRUE)
		 (eq:<VPRED>
		   (match_operand:SVE_I 2 "register_operand" "w, w, w")
		   (match_operand:SVE_I 3 "aarch64_simd_imm_zero"))]
		UNSPEC_PRED_Z)
	      (match_operand:SVE_I 4 "aarch64_simd_imm_one")
	      (match_dup 3)]
	     UNSPEC_SEL)
	   (match_operand:SVE_I 6 "aarch64_simd_reg_or_zero" "0, Dz, w")]
	  UNSPEC_SEL))]
  "TARGET_SVE && !rtx_equal_p (operands[2], operands[6])"
  "@
   cnot\t%0.<Vetype>, %1/m, %2.<Vetype>
   movprfx\t%0.<Vetype>, %1/z, %2.<Vetype>\;cnot\t%0.<Vetype>, %1/m, %2.<Vetype>
   movprfx\t%0, %6\;cnot\t%0.<Vetype>, %1/m, %2.<Vetype>"
  "&& !CONSTANT_P (operands[5])"
  {
    operands[5] = CONSTM1_RTX (<VPRED>mode);
  }
  [(set_attr "movprfx" "*,yes,yes")]
)

;; -------------------------------------------------------------------------
;; ---- [FP] General unary arithmetic corresponding to unspecs
;; -------------------------------------------------------------------------
;; Includes:
;; - FABS
;; - FNEG
;; - FRINTA
;; - FRINTI
;; - FRINTM
;; - FRINTN
;; - FRINTP
;; - FRINTX
;; - FRINTZ
;; - FSQRT
;; -------------------------------------------------------------------------

;; Unpredicated floating-point unary operations.
(define_expand "<optab><mode>2"
  [(set (match_operand:SVE_F 0 "register_operand")
	(unspec:SVE_F
	  [(match_dup 2)
	   (const_int SVE_RELAXED_GP)
	   (match_operand:SVE_F 1 "register_operand")]
	  SVE_COND_FP_UNARY))]
  "TARGET_SVE"
  {
    operands[2] = aarch64_ptrue_reg (<VPRED>mode);
  }
)

;; Predicated floating-point unary operations.
(define_insn "*<optab><mode>2"
  [(set (match_operand:SVE_F 0 "register_operand" "=w")
	(unspec:SVE_F
	  [(match_operand:<VPRED> 1 "register_operand" "Upl")
	   (match_operand:SI 3 "aarch64_sve_gp_strictness")
	   (match_operand:SVE_F 2 "register_operand" "w")]
	  SVE_COND_FP_UNARY))]
  "TARGET_SVE"
  "<sve_fp_op>\t%0.<Vetype>, %1/m, %2.<Vetype>"
)

;; Predicated floating-point unary arithmetic, merging with the first input.
(define_insn_and_rewrite "*cond_<optab><mode>_2"
  [(set (match_operand:SVE_F 0 "register_operand" "=w, ?&w")
	(unspec:SVE_F
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl")
	   (unspec:SVE_F
	     [(match_operand 3)
	      (match_operand:SI 4 "aarch64_sve_gp_strictness")
	      (match_operand:SVE_F 2 "register_operand" "0, w")]
	     SVE_COND_FP_UNARY)
	   (match_dup 2)]
	  UNSPEC_SEL))]
  "TARGET_SVE && aarch64_sve_pred_dominates_p (&operands[3], operands[1])"
  "@
   <sve_fp_op>\t%0.<Vetype>, %1/m, %0.<Vetype>
   movprfx\t%0, %2\;<sve_fp_op>\t%0.<Vetype>, %1/m, %2.<Vetype>"
  "&& !rtx_equal_p (operands[1], operands[3])"
  {
    operands[3] = copy_rtx (operands[1]);
  }
  [(set_attr "movprfx" "*,yes")]
)

;; Predicated floating-point unary arithmetic, merging with an independent
;; value.
;;
;; The earlyclobber isn't needed for the first alternative, but omitting
;; it would only help the case in which operands 2 and 3 are the same,
;; which is handled above rather than here.  Marking all the alternatives
;; as earlyclobber helps to make the instruction more regular to the
;; register allocator.
(define_insn_and_rewrite "*cond_<optab><mode>_any"
  [(set (match_operand:SVE_F 0 "register_operand" "=&w, ?&w, ?&w")
	(unspec:SVE_F
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl, Upl")
	   (unspec:SVE_F
	     [(match_operand 4)
	      (match_operand:SI 5 "aarch64_sve_gp_strictness")
	      (match_operand:SVE_F 2 "register_operand" "w, w, w")]
	     SVE_COND_FP_UNARY)
	   (match_operand:SVE_F 3 "aarch64_simd_reg_or_zero" "0, Dz, w")]
	  UNSPEC_SEL))]
  "TARGET_SVE
   && !rtx_equal_p (operands[2], operands[3])
   && aarch64_sve_pred_dominates_p (&operands[4], operands[1])"
  "@
   <sve_fp_op>\t%0.<Vetype>, %1/m, %2.<Vetype>
   movprfx\t%0.<Vetype>, %1/z, %2.<Vetype>\;<sve_fp_op>\t%0.<Vetype>, %1/m, %2.<Vetype>
   movprfx\t%0, %3\;<sve_fp_op>\t%0.<Vetype>, %1/m, %2.<Vetype>"
  "&& !rtx_equal_p (operands[1], operands[4])"
  {
    operands[4] = copy_rtx (operands[1]);
  }
  [(set_attr "movprfx" "*,yes,yes")]
)

;; -------------------------------------------------------------------------
;; ---- [PRED] Inverse
;; -------------------------------------------------------------------------
;; Includes:
;; - NOT
;; -------------------------------------------------------------------------

;; Unpredicated predicate inverse.
(define_expand "one_cmpl<mode>2"
  [(set (match_operand:PRED_ALL 0 "register_operand")
	(and:PRED_ALL
	  (not:PRED_ALL (match_operand:PRED_ALL 1 "register_operand"))
	  (match_dup 2)))]
  "TARGET_SVE"
  {
    operands[2] = aarch64_ptrue_reg (<MODE>mode);
  }
)

;; Predicated predicate inverse.
(define_insn "*one_cmpl<mode>3"
  [(set (match_operand:PRED_ALL 0 "register_operand" "=Upa")
	(and:PRED_ALL
	  (not:PRED_ALL (match_operand:PRED_ALL 2 "register_operand" "Upa"))
	  (match_operand:PRED_ALL 1 "register_operand" "Upa")))]
  "TARGET_SVE"
  "not\t%0.b, %1/z, %2.b"
)

;; =========================================================================
;; == Binary arithmetic
;; =========================================================================

;; -------------------------------------------------------------------------
;; ---- [INT] General binary arithmetic corresponding to rtx codes
;; -------------------------------------------------------------------------
;; Includes:
;; - ADD    (merging form only)
;; - AND    (merging form only)
;; - ASR    (merging form only)
;; - EOR    (merging form only)
;; - LSL    (merging form only)
;; - LSR    (merging form only)
;; - MUL
;; - ORR    (merging form only)
;; - SMAX
;; - SMIN
;; - SUB    (merging form only)
;; - UMAX
;; - UMIN
;; -------------------------------------------------------------------------

;; Unpredicated integer binary operations that have an immediate form.
(define_expand "<optab><mode>3"
  [(set (match_operand:SVE_I 0 "register_operand")
	(unspec:SVE_I
	  [(match_dup 3)
	   (SVE_INT_BINARY_IMM:SVE_I
	     (match_operand:SVE_I 1 "register_operand")
	     (match_operand:SVE_I 2 "aarch64_sve_<sve_imm_con>_operand"))]
	  UNSPEC_PRED_X))]
  "TARGET_SVE"
  {
    operands[3] = aarch64_ptrue_reg (<VPRED>mode);
  }
)

;; Integer binary operations that have an immediate form, predicated
;; with a PTRUE.  We don't actually need the predicate for the first
;; and third alternatives, but using Upa or X isn't likely to gain much
;; and would make the instruction seem less uniform to the register
;; allocator.
(define_insn_and_split "*<optab><mode>3"
  [(set (match_operand:SVE_I 0 "register_operand" "=w, w, ?&w, ?&w")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl, Upl, Upl")
	   (SVE_INT_BINARY_IMM:SVE_I
	     (match_operand:SVE_I 2 "register_operand" "%0, 0, w, w")
	     (match_operand:SVE_I 3 "aarch64_sve_<sve_imm_con>_operand" "<sve_imm_con>, w, <sve_imm_con>, w"))]
	  UNSPEC_PRED_X))]
  "TARGET_SVE"
  "@
   #
   <sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
   #
   movprfx\t%0, %2\;<sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>"
  ; Split the unpredicated form after reload, so that we don't have
  ; the unnecessary PTRUE.
  "&& reload_completed
   && !register_operand (operands[3], <MODE>mode)"
  [(set (match_dup 0) (SVE_INT_BINARY_IMM:SVE_I (match_dup 2) (match_dup 3)))]
  ""
  [(set_attr "movprfx" "*,*,yes,yes")]
)

;; Unpredicated binary operations with a constant (post-RA only).
;; These are generated by splitting a predicated instruction whose
;; predicate is unused.
(define_insn "*post_ra_<optab><mode>3"
  [(set (match_operand:SVE_I 0 "register_operand" "=w, ?&w")
	(SVE_INT_BINARY_IMM:SVE_I
	  (match_operand:SVE_I 1 "register_operand" "0, w")
	  (match_operand:SVE_I 2 "aarch64_sve_<sve_imm_con>_immediate")))]
  "TARGET_SVE && reload_completed"
  "@
   <sve_int_op>\t%0.<Vetype>, %0.<Vetype>, #%<sve_imm_prefix>2
   movprfx\t%0, %1\;<sve_int_op>\t%0.<Vetype>, %0.<Vetype>, #%<sve_imm_prefix>2"
  [(set_attr "movprfx" "*,yes")]
)

;; Predicated integer operations with merging.
(define_expand "@cond_<optab><mode>"
  [(set (match_operand:SVE_I 0 "register_operand")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand")
	   (SVE_INT_BINARY:SVE_I
	     (match_operand:SVE_I 2 "register_operand")
	     (match_operand:SVE_I 3 "<sve_pred_int_rhs2_operand>"))
	   (match_operand:SVE_I 4 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE"
)

;; Predicated integer operations, merging with the first input.
(define_insn "*cond_<optab><mode>_2"
  [(set (match_operand:SVE_I 0 "register_operand" "=w, ?&w")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl")
	   (SVE_INT_BINARY:SVE_I
	     (match_operand:SVE_I 2 "register_operand" "0, w")
	     (match_operand:SVE_I 3 "register_operand" "w, w"))
	   (match_dup 2)]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  "@
   <sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
   movprfx\t%0, %2\;<sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>"
  [(set_attr "movprfx" "*,yes")]
)

;; Predicated integer operations, merging with the second input.
(define_insn "*cond_<optab><mode>_3"
  [(set (match_operand:SVE_I 0 "register_operand" "=w, ?&w")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl")
	   (SVE_INT_BINARY:SVE_I
	     (match_operand:SVE_I 2 "register_operand" "w, w")
	     (match_operand:SVE_I 3 "register_operand" "0, w"))
	   (match_dup 3)]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  "@
   <sve_int_op_rev>\t%0.<Vetype>, %1/m, %0.<Vetype>, %2.<Vetype>
   movprfx\t%0, %3\;<sve_int_op_rev>\t%0.<Vetype>, %1/m, %0.<Vetype>, %2.<Vetype>"
  [(set_attr "movprfx" "*,yes")]
)

;; Predicated integer operations, merging with an independent value.
(define_insn_and_rewrite "*cond_<optab><mode>_any"
  [(set (match_operand:SVE_I 0 "register_operand" "=&w, &w, &w, &w, ?&w")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl, Upl, Upl, Upl")
	   (SVE_INT_BINARY:SVE_I
	     (match_operand:SVE_I 2 "register_operand" "0, w, w, w, w")
	     (match_operand:SVE_I 3 "register_operand" "w, 0, w, w, w"))
	   (match_operand:SVE_I 4 "aarch64_simd_reg_or_zero" "Dz, Dz, Dz, 0, w")]
	  UNSPEC_SEL))]
  "TARGET_SVE
   && !rtx_equal_p (operands[2], operands[4])
   && !rtx_equal_p (operands[3], operands[4])"
  "@
   movprfx\t%0.<Vetype>, %1/z, %0.<Vetype>\;<sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
   movprfx\t%0.<Vetype>, %1/z, %0.<Vetype>\;<sve_int_op_rev>\t%0.<Vetype>, %1/m, %0.<Vetype>, %2.<Vetype>
   movprfx\t%0.<Vetype>, %1/z, %2.<Vetype>\;<sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
   movprfx\t%0.<Vetype>, %1/m, %2.<Vetype>\;<sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
   #"
  "&& reload_completed
   && register_operand (operands[4], <MODE>mode)
   && !rtx_equal_p (operands[0], operands[4])"
  {
    emit_insn (gen_vcond_mask_<mode><vpred> (operands[0], operands[2],
					     operands[4], operands[1]));
    operands[4] = operands[2] = operands[0];
  }
  [(set_attr "movprfx" "yes")]
)

;; -------------------------------------------------------------------------
;; ---- [INT] Addition
;; -------------------------------------------------------------------------
;; Includes:
;; - ADD
;; - DECB
;; - DECD
;; - DECH
;; - DECW
;; - INCB
;; - INCD
;; - INCH
;; - INCW
;; - SUB
;; -------------------------------------------------------------------------

(define_insn "add<mode>3"
  [(set (match_operand:SVE_I 0 "register_operand" "=w, w, w, ?w, ?w, w")
	(plus:SVE_I
	  (match_operand:SVE_I 1 "register_operand" "%0, 0, 0, w, w, w")
	  (match_operand:SVE_I 2 "aarch64_sve_add_operand" "vsa, vsn, vsi, vsa, vsn, w")))]
  "TARGET_SVE"
  "@
   add\t%0.<Vetype>, %0.<Vetype>, #%D2
   sub\t%0.<Vetype>, %0.<Vetype>, #%N2
   * return aarch64_output_sve_vector_inc_dec (\"%0.<Vetype>\", operands[2]);
   movprfx\t%0, %1\;add\t%0.<Vetype>, %0.<Vetype>, #%D2
   movprfx\t%0, %1\;sub\t%0.<Vetype>, %0.<Vetype>, #%N2
   add\t%0.<Vetype>, %1.<Vetype>, %2.<Vetype>"
  [(set_attr "movprfx" "*,*,*,yes,yes,*")]
)

;; Merging forms are handled through SVE_INT_BINARY.

;; -------------------------------------------------------------------------
;; ---- [INT] Subtraction
;; -------------------------------------------------------------------------
;; Includes:
;; - SUB
;; - SUBR
;; -------------------------------------------------------------------------

(define_insn "sub<mode>3"
  [(set (match_operand:SVE_I 0 "register_operand" "=w, w, ?&w")
	(minus:SVE_I
	  (match_operand:SVE_I 1 "aarch64_sve_arith_operand" "w, vsa, vsa")
	  (match_operand:SVE_I 2 "register_operand" "w, 0, w")))]
  "TARGET_SVE"
  "@
   sub\t%0.<Vetype>, %1.<Vetype>, %2.<Vetype>
   subr\t%0.<Vetype>, %0.<Vetype>, #%D1
   movprfx\t%0, %2\;subr\t%0.<Vetype>, %0.<Vetype>, #%D1"
  [(set_attr "movprfx" "*,*,yes")]
)

;; Merging forms are handled through SVE_INT_BINARY.

;; -------------------------------------------------------------------------
;; ---- [INT] Take address
;; -------------------------------------------------------------------------
;; Includes:
;; - ADR
;; -------------------------------------------------------------------------

;; Unshifted ADR, with the offset being zero-extended from the low 32 bits.
(define_insn "*aarch64_adr_uxtw"
  [(set (match_operand:VNx2DI 0 "register_operand" "=w")
	(plus:VNx2DI
	  (and:VNx2DI
	    (match_operand:VNx2DI 2 "register_operand" "w")
	    (match_operand:VNx2DI 3 "aarch64_sve_uxtw_immediate"))
	  (match_operand:VNx2DI 1 "register_operand" "w")))]
  "TARGET_SVE"
  "adr\t%0.d, [%1.d, %2.d, uxtw]"
)

;; ADR with a nonzero shift.
(define_insn_and_rewrite "*aarch64_adr<mode>_shift"
  [(set (match_operand:SVE_SDI 0 "register_operand" "=w")
	(plus:SVE_SDI
	  (unspec:SVE_SDI
	    [(match_operand 4)
	     (ashift:SVE_SDI
	       (match_operand:SVE_SDI 2 "register_operand" "w")
	       (match_operand:SVE_SDI 3 "const_1_to_3_operand"))]
	    UNSPEC_PRED_X)
	  (match_operand:SVE_SDI 1 "register_operand" "w")))]
  "TARGET_SVE"
  "adr\t%0.<Vetype>, [%1.<Vetype>, %2.<Vetype>, lsl %3]"
  "&& !CONSTANT_P (operands[4])"
  {
    operands[4] = CONSTM1_RTX (<VPRED>mode);
  }
)

;; Same, but with the index being zero-extended from the low 32 bits.
(define_insn_and_rewrite "*aarch64_adr_shift_uxtw"
  [(set (match_operand:VNx2DI 0 "register_operand" "=w")
	(plus:VNx2DI
	  (unspec:VNx2DI
	    [(match_operand 5)
	     (ashift:VNx2DI
	       (and:VNx2DI
		 (match_operand:VNx2DI 2 "register_operand" "w")
		 (match_operand:VNx2DI 4 "aarch64_sve_uxtw_immediate"))
	       (match_operand:VNx2DI 3 "const_1_to_3_operand"))]
	    UNSPEC_PRED_X)
	  (match_operand:VNx2DI 1 "register_operand" "w")))]
  "TARGET_SVE"
  "adr\t%0.d, [%1.d, %2.d, uxtw %3]"
  "&& !CONSTANT_P (operands[5])"
  {
    operands[5] = CONSTM1_RTX (VNx2BImode);
  }
)

;; -------------------------------------------------------------------------
;; ---- [INT] Absolute difference
;; -------------------------------------------------------------------------
;; Includes:
;; - SABD
;; - UABD
;; -------------------------------------------------------------------------

;; Unpredicated integer absolute difference.
(define_expand "<su>abd<mode>_3"
  [(use (match_operand:SVE_I 0 "register_operand"))
   (USMAX:SVE_I (match_operand:SVE_I 1 "register_operand")
		(match_operand:SVE_I 2 "register_operand"))]
  "TARGET_SVE"
  {
    rtx pred = aarch64_ptrue_reg (<VPRED>mode);
    emit_insn (gen_aarch64_<su>abd<mode>_3 (operands[0], pred, operands[1],
					    operands[2]));
    DONE;
  }
)

;; Predicated integer absolute difference.
(define_insn "aarch64_<su>abd<mode>_3"
  [(set (match_operand:SVE_I 0 "register_operand" "=w, ?&w")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl")
	   (minus:SVE_I
	     (USMAX:SVE_I
	       (match_operand:SVE_I 2 "register_operand" "%0, w")
	       (match_operand:SVE_I 3 "register_operand" "w, w"))
	     (<max_opp>:SVE_I
	       (match_dup 2)
	       (match_dup 3)))]
	  UNSPEC_PRED_X))]
  "TARGET_SVE"
  "@
   <su>abd\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
   movprfx\t%0, %2\;<su>abd\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>"
  [(set_attr "movprfx" "*,yes")]
)

;; Predicated integer absolute difference, merging with the first input.
(define_insn_and_rewrite "*aarch64_cond_<su>abd<mode>_2"
  [(set (match_operand:SVE_I 0 "register_operand" "=w, ?&w")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl")
	   (minus:SVE_I
	     (unspec:SVE_I
	       [(match_operand 4)
		(USMAX:SVE_I
		  (match_operand:SVE_I 2 "register_operand" "0, w")
		  (match_operand:SVE_I 3 "register_operand" "w, w"))]
	       UNSPEC_PRED_X)
	     (unspec:SVE_I
	       [(match_operand 5)
		(<max_opp>:SVE_I
		  (match_dup 2)
		  (match_dup 3))]
	       UNSPEC_PRED_X))
	   (match_dup 2)]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  "@
   <su>abd\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
   movprfx\t%0, %2\;<su>abd\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>"
  "&& (!CONSTANT_P (operands[4]) || !CONSTANT_P (operands[5]))"
  {
    operands[4] = operands[5] = CONSTM1_RTX (<VPRED>mode);
  }
  [(set_attr "movprfx" "*,yes")]
)

;; Predicated integer absolute difference, merging with an independent value.
(define_insn_and_rewrite "*aarch64_cond_<su>abd<mode>_any"
  [(set (match_operand:SVE_I 0 "register_operand" "=&w, &w, &w, &w, ?&w")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl, Upl, Upl, Upl")
	   (minus:SVE_I
	     (unspec:SVE_I
	       [(match_operand 5)
		(USMAX:SVE_I
		  (match_operand:SVE_I 2 "register_operand" "0, w, w, w, w")
		  (match_operand:SVE_I 3 "register_operand" "w, 0, w, w, w"))]
	       UNSPEC_PRED_X)
	     (unspec:SVE_I
	       [(match_operand 6)
		(<max_opp>:SVE_I
		  (match_dup 2)
		  (match_dup 3))]
	       UNSPEC_PRED_X))
	   (match_operand:SVE_I 4 "aarch64_simd_reg_or_zero" "Dz, Dz, Dz, 0, w")]
	  UNSPEC_SEL))]
  "TARGET_SVE
   && !rtx_equal_p (operands[2], operands[4])
   && !rtx_equal_p (operands[3], operands[4])"
  "@
   movprfx\t%0.<Vetype>, %1/z, %0.<Vetype>\;<su>abd\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
   movprfx\t%0.<Vetype>, %1/z, %0.<Vetype>\;<su>abd\t%0.<Vetype>, %1/m, %0.<Vetype>, %2.<Vetype>
   movprfx\t%0.<Vetype>, %1/z, %2.<Vetype>\;<su>abd\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
   movprfx\t%0.<Vetype>, %1/m, %2.<Vetype>\;<su>abd\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
   #"
  "&& 1"
  {
    if (!CONSTANT_P (operands[5]) || !CONSTANT_P (operands[6]))
      operands[5] = operands[6] = CONSTM1_RTX (<VPRED>mode);
    else if (reload_completed
	     && register_operand (operands[4], <MODE>mode)
	     && !rtx_equal_p (operands[0], operands[4]))
      {
	emit_insn (gen_vcond_mask_<mode><vpred> (operands[0], operands[2],
						 operands[4], operands[1]));
	operands[4] = operands[2] = operands[0];
      }
    else
      FAIL;
  }
  [(set_attr "movprfx" "yes")]
)

;; -------------------------------------------------------------------------
;; ---- [INT] Highpart multiplication
;; -------------------------------------------------------------------------
;; Includes:
;; - SMULH
;; - UMULH
;; -------------------------------------------------------------------------

;; Unpredicated highpart multiplication.
(define_expand "<su>mul<mode>3_highpart"
  [(set (match_operand:SVE_I 0 "register_operand")
	(unspec:SVE_I
	  [(match_dup 3)
	   (unspec:SVE_I [(match_operand:SVE_I 1 "register_operand")
			  (match_operand:SVE_I 2 "register_operand")]
			 MUL_HIGHPART)]
	  UNSPEC_PRED_X))]
  "TARGET_SVE"
  {
    operands[3] = aarch64_ptrue_reg (<VPRED>mode);
  }
)

;; Predicated highpart multiplication.
(define_insn "*<su>mul<mode>3_highpart"
  [(set (match_operand:SVE_I 0 "register_operand" "=w, ?&w")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl")
	   (unspec:SVE_I [(match_operand:SVE_I 2 "register_operand" "%0, w")
			  (match_operand:SVE_I 3 "register_operand" "w, w")]
			 MUL_HIGHPART)]
	  UNSPEC_PRED_X))]
  "TARGET_SVE"
  "@
   <su>mulh\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
   movprfx\t%0, %2\;<su>mulh\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>"
  [(set_attr "movprfx" "*,yes")]
)

;; -------------------------------------------------------------------------
;; ---- [INT] Division
;; -------------------------------------------------------------------------
;; Includes:
;; - SDIV
;; - SDIVR
;; - UDIV
;; - UDIVR
;; -------------------------------------------------------------------------

;; Unpredicated integer division.
(define_expand "<optab><mode>3"
  [(set (match_operand:SVE_SDI 0 "register_operand")
	(unspec:SVE_SDI
	  [(match_dup 3)
	   (SVE_INT_BINARY_SD:SVE_SDI
	     (match_operand:SVE_SDI 1 "register_operand")
	     (match_operand:SVE_SDI 2 "register_operand"))]
	  UNSPEC_PRED_X))]
  "TARGET_SVE"
  {
    operands[3] = aarch64_ptrue_reg (<VPRED>mode);
  }
)

;; Integer division predicated with a PTRUE.
(define_insn "*<optab><mode>3"
  [(set (match_operand:SVE_SDI 0 "register_operand" "=w, w, ?&w")
	(unspec:SVE_SDI
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl, Upl")
	   (SVE_INT_BINARY_SD:SVE_SDI
	     (match_operand:SVE_SDI 2 "register_operand" "0, w, w")
	     (match_operand:SVE_SDI 3 "register_operand" "w, 0, w"))]
	  UNSPEC_PRED_X))]
  "TARGET_SVE"
  "@
   <sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
   <sve_int_op>r\t%0.<Vetype>, %1/m, %0.<Vetype>, %2.<Vetype>
   movprfx\t%0, %2\;<sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>"
  [(set_attr "movprfx" "*,*,yes")]
)

;; Predicated integer division with merging.
(define_expand "cond_<optab><mode>"
  [(set (match_operand:SVE_SDI 0 "register_operand")
	(unspec:SVE_SDI
	  [(match_operand:<VPRED> 1 "register_operand")
	   (SVE_INT_BINARY_SD:SVE_SDI
	     (match_operand:SVE_SDI 2 "register_operand")
	     (match_operand:SVE_SDI 3 "register_operand"))
	   (match_operand:SVE_SDI 4 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE"
)

;; Predicated integer division, merging with the first input.
(define_insn "*cond_<optab><mode>_2"
  [(set (match_operand:SVE_SDI 0 "register_operand" "=w, ?&w")
	(unspec:SVE_SDI
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl")
	   (SVE_INT_BINARY_SD:SVE_SDI
	     (match_operand:SVE_SDI 2 "register_operand" "0, w")
	     (match_operand:SVE_SDI 3 "register_operand" "w, w"))
	   (match_dup 2)]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  "@
   <sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
   movprfx\t%0, %2\;<sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>"
  [(set_attr "movprfx" "*,yes")]
)

;; Predicated integer division, merging with the second input.
(define_insn "*cond_<optab><mode>_3"
  [(set (match_operand:SVE_SDI 0 "register_operand" "=w, ?&w")
	(unspec:SVE_SDI
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl")
	   (SVE_INT_BINARY_SD:SVE_SDI
	     (match_operand:SVE_SDI 2 "register_operand" "w, w")
	     (match_operand:SVE_SDI 3 "register_operand" "0, w"))
	   (match_dup 3)]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  "@
   <sve_int_op_rev>\t%0.<Vetype>, %1/m, %0.<Vetype>, %2.<Vetype>
   movprfx\t%0, %3\;<sve_int_op_rev>\t%0.<Vetype>, %1/m, %0.<Vetype>, %2.<Vetype>"
  [(set_attr "movprfx" "*,yes")]
)

;; Predicated integer division, merging with an independent value.
(define_insn_and_rewrite "*cond_<optab><mode>_any"
  [(set (match_operand:SVE_SDI 0 "register_operand" "=&w, &w, &w, &w, ?&w")
	(unspec:SVE_SDI
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl, Upl, Upl, Upl")
	   (SVE_INT_BINARY_SD:SVE_SDI
	     (match_operand:SVE_SDI 2 "register_operand" "0, w, w, w, w")
	     (match_operand:SVE_SDI 3 "register_operand" "w, 0, w, w, w"))
	   (match_operand:SVE_SDI 4 "aarch64_simd_reg_or_zero" "Dz, Dz, Dz, 0, w")]
	  UNSPEC_SEL))]
  "TARGET_SVE
   && !rtx_equal_p (operands[2], operands[4])
   && !rtx_equal_p (operands[3], operands[4])"
  "@
   movprfx\t%0.<Vetype>, %1/z, %0.<Vetype>\;<sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
   movprfx\t%0.<Vetype>, %1/z, %0.<Vetype>\;<sve_int_op_rev>\t%0.<Vetype>, %1/m, %0.<Vetype>, %2.<Vetype>
   movprfx\t%0.<Vetype>, %1/z, %2.<Vetype>\;<sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
   movprfx\t%0.<Vetype>, %1/m, %2.<Vetype>\;<sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
   #"
  "&& reload_completed
   && register_operand (operands[4], <MODE>mode)
   && !rtx_equal_p (operands[0], operands[4])"
  {
    emit_insn (gen_vcond_mask_<mode><vpred> (operands[0], operands[2],
					     operands[4], operands[1]));
    operands[4] = operands[2] = operands[0];
  }
  [(set_attr "movprfx" "yes")]
)

;; -------------------------------------------------------------------------
;; ---- [INT] Binary logical operations
;; -------------------------------------------------------------------------
;; Includes:
;; - AND
;; - EOR
;; - ORR
;; -------------------------------------------------------------------------

;; Unpredicated integer binary logical operations.
(define_insn "<optab><mode>3"
  [(set (match_operand:SVE_I 0 "register_operand" "=w, ?w, w")
	(LOGICAL:SVE_I
	  (match_operand:SVE_I 1 "register_operand" "%0, w, w")
	  (match_operand:SVE_I 2 "aarch64_sve_logical_operand" "vsl, vsl, w")))]
  "TARGET_SVE"
  "@
   <logical>\t%0.<Vetype>, %0.<Vetype>, #%C2
   movprfx\t%0, %1\;<logical>\t%0.<Vetype>, %0.<Vetype>, #%C2
   <logical>\t%0.d, %1.d, %2.d"
  [(set_attr "movprfx" "*,yes,*")]
)

;; Merging forms are handled through SVE_INT_BINARY.

;; -------------------------------------------------------------------------
;; ---- [INT] Binary logical operations (inverted second input)
;; -------------------------------------------------------------------------
;; Includes:
;; - BIC
;; -------------------------------------------------------------------------

(define_insn_and_rewrite "*bic<mode>3"
  [(set (match_operand:SVE_I 0 "register_operand" "=w")
	(and:SVE_I
	  (unspec:SVE_I
	    [(match_operand 3)
	     (not:SVE_I (match_operand:SVE_I 2 "register_operand" "w"))]
	    UNSPEC_PRED_X)
	  (match_operand:SVE_I 1 "register_operand" "w")))]
  "TARGET_SVE"
  "bic\t%0.d, %1.d, %2.d"
  "&& !CONSTANT_P (operands[3])"
  {
    operands[3] = CONSTM1_RTX (<VPRED>mode);
  }
)

;; Predicated integer BIC, merging with the first input.
(define_insn "*cond_bic<mode>_2"
  [(set (match_operand:SVE_I 0 "register_operand" "=w, ?&w")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl")
	   (and:SVE_I
	     (not:SVE_I (match_operand:SVE_I 3 "register_operand" "w, w"))
	     (match_operand:SVE_I 2 "register_operand" "0, w"))
	   (match_dup 2)]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  "@
   bic\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
   movprfx\t%0, %2\;bic\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>"
  [(set_attr "movprfx" "*,yes")]
)

;; Predicated integer BIC, merging with an independent value.
(define_insn_and_rewrite "*cond_bic<mode>_any"
  [(set (match_operand:SVE_I 0 "register_operand" "=&w, &w, &w, ?&w")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl, Upl, Upl")
	   (and:SVE_I
	     (not:SVE_I (match_operand:SVE_I 3 "register_operand" "w, w, w, w"))
	     (match_operand:SVE_I 2 "register_operand" "0, w, w, w"))
	   (match_operand:SVE_I 4 "aarch64_simd_reg_or_zero" "Dz, Dz, 0, w")]
	  UNSPEC_SEL))]
  "TARGET_SVE && !rtx_equal_p (operands[2], operands[4])"
  "@
   movprfx\t%0.<Vetype>, %1/z, %0.<Vetype>\;bic\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
   movprfx\t%0.<Vetype>, %1/z, %2.<Vetype>\;bic\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
   movprfx\t%0.<Vetype>, %1/m, %2.<Vetype>\;bic\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
   #"
  "&& reload_completed
   && register_operand (operands[4], <MODE>mode)
   && !rtx_equal_p (operands[0], operands[4])"
  {
    emit_insn (gen_vcond_mask_<mode><vpred> (operands[0], operands[2],
					     operands[4], operands[1]));
    operands[4] = operands[2] = operands[0];
  }
  [(set_attr "movprfx" "yes")]
)

;; -------------------------------------------------------------------------
;; ---- [INT] Shifts
;; -------------------------------------------------------------------------
;; Includes:
;; - ASR
;; - LSL
;; - LSR
;; -------------------------------------------------------------------------

;; Unpredicated shift by a scalar, which expands into one of the vector
;; shifts below.
(define_expand "<ASHIFT:optab><mode>3"
  [(set (match_operand:SVE_I 0 "register_operand")
	(ASHIFT:SVE_I (match_operand:SVE_I 1 "register_operand")
		      (match_operand:<VEL> 2 "general_operand")))]
  "TARGET_SVE"
  {
    rtx amount;
    if (CONST_INT_P (operands[2]))
      {
	amount = gen_const_vec_duplicate (<MODE>mode, operands[2]);
	if (!aarch64_sve_<lr>shift_operand (operands[2], <MODE>mode))
	  amount = force_reg (<MODE>mode, amount);
      }
    else
      {
	amount = gen_reg_rtx (<MODE>mode);
	emit_insn (gen_vec_duplicate<mode> (amount,
					    convert_to_mode (<VEL>mode,
							     operands[2], 0)));
      }
    emit_insn (gen_v<optab><mode>3 (operands[0], operands[1], amount));
    DONE;
  }
)

;; Unpredicated shift by a vector.
(define_expand "v<optab><mode>3"
  [(set (match_operand:SVE_I 0 "register_operand")
	(unspec:SVE_I
	  [(match_dup 3)
	   (ASHIFT:SVE_I
	     (match_operand:SVE_I 1 "register_operand")
	     (match_operand:SVE_I 2 "aarch64_sve_<lr>shift_operand"))]
	  UNSPEC_PRED_X))]
  "TARGET_SVE"
  {
    operands[3] = aarch64_ptrue_reg (<VPRED>mode);
  }
)

;; Shift by a vector, predicated with a PTRUE.  We don't actually need
;; the predicate for the first alternative, but using Upa or X isn't
;; likely to gain much and would make the instruction seem less uniform
;; to the register allocator.
(define_insn_and_split "*v<optab><mode>3"
  [(set (match_operand:SVE_I 0 "register_operand" "=w, w, w, ?&w")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl, Upl, Upl")
	   (ASHIFT:SVE_I
	     (match_operand:SVE_I 2 "register_operand" "w, 0, w, w")
	     (match_operand:SVE_I 3 "aarch64_sve_<lr>shift_operand" "D<lr>, w, 0, w"))]
	  UNSPEC_PRED_X))]
  "TARGET_SVE"
  "@
   #
   <shift>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
   <shift>r\t%0.<Vetype>, %1/m, %3.<Vetype>, %2.<Vetype>
   movprfx\t%0, %2\;<shift>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>"
  "&& reload_completed
   && !register_operand (operands[3], <MODE>mode)"
  [(set (match_dup 0) (ASHIFT:SVE_I (match_dup 2) (match_dup 3)))]
  ""
  [(set_attr "movprfx" "*,*,*,yes")]
)

;; Unpredicated shift operations by a constant (post-RA only).
;; These are generated by splitting a predicated instruction whose
;; predicate is unused.
(define_insn "*post_ra_v<optab><mode>3"
  [(set (match_operand:SVE_I 0 "register_operand" "=w")
	(ASHIFT:SVE_I
	  (match_operand:SVE_I 1 "register_operand" "w")
	  (match_operand:SVE_I 2 "aarch64_simd_<lr>shift_imm")))]
  "TARGET_SVE && reload_completed"
  "<shift>\t%0.<Vetype>, %1.<Vetype>, #%2"
)

;; Predicated integer shift, merging with the first input.
(define_insn "*cond_<optab><mode>_2_const"
  [(set (match_operand:SVE_I 0 "register_operand" "=w, ?&w")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl")
	   (ASHIFT:SVE_I
	     (match_operand:SVE_I 2 "register_operand" "0, w")
	     (match_operand:SVE_I 3 "aarch64_simd_<lr>shift_imm"))
	   (match_dup 2)]
	 UNSPEC_SEL))]
  "TARGET_SVE"
  "@
   <shift>\t%0.<Vetype>, %1/m, %0.<Vetype>, #%3
   movprfx\t%0, %2\;<shift>\t%0.<Vetype>, %1/m, %0.<Vetype>, #%3"
  [(set_attr "movprfx" "*,yes")]
)

;; Predicated integer shift, merging with an independent value.
(define_insn_and_rewrite "*cond_<optab><mode>_any_const"
  [(set (match_operand:SVE_I 0 "register_operand" "=w, &w, ?&w")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl, Upl")
	   (ASHIFT:SVE_I
	     (match_operand:SVE_I 2 "register_operand" "w, w, w")
	     (match_operand:SVE_I 3 "aarch64_simd_<lr>shift_imm"))
	   (match_operand:SVE_I 4 "aarch64_simd_reg_or_zero" "Dz, 0, w")]
	 UNSPEC_SEL))]
  "TARGET_SVE && !rtx_equal_p (operands[2], operands[4])"
  "@
   movprfx\t%0.<Vetype>, %1/z, %2.<Vetype>\;<shift>\t%0.<Vetype>, %1/m, %0.<Vetype>, #%3
   movprfx\t%0.<Vetype>, %1/m, %2.<Vetype>\;<shift>\t%0.<Vetype>, %1/m, %0.<Vetype>, #%3
   #"
  "&& reload_completed
   && register_operand (operands[4], <MODE>mode)
   && !rtx_equal_p (operands[0], operands[4])"
  {
    emit_insn (gen_vcond_mask_<mode><vpred> (operands[0], operands[2],
					     operands[4], operands[1]));
    operands[4] = operands[2] = operands[0];
  }
  [(set_attr "movprfx" "yes")]
)

;; -------------------------------------------------------------------------
;; ---- [INT] Shifts (rounding towards 0)
;; -------------------------------------------------------------------------
;; Includes:
;; - ASRD
;; -------------------------------------------------------------------------

;; Unpredicated arithmetic right shift for division by power-of-2.
(define_expand "sdiv_pow2<mode>3"
  [(set (match_operand:SVE_I 0 "register_operand")
	(unspec:SVE_I
	  [(match_dup 3)
	   (unspec:SVE_I
	     [(match_operand:SVE_I 1 "register_operand")
	      (match_operand 2 "aarch64_simd_rshift_imm")]
	    UNSPEC_ASRD)]
	 UNSPEC_PRED_X))]
  "TARGET_SVE"
  {
    operands[3] = aarch64_ptrue_reg (<VPRED>mode);
  }
)

;; Predicated ASRD with PTRUE.
(define_insn "*sdiv_pow2<mode>3"
  [(set (match_operand:SVE_I 0 "register_operand" "=w, ?&w")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl")
	   (unspec:SVE_I
	     [(match_operand:SVE_I 2 "register_operand" "0, w")
	      (match_operand 3 "aarch64_simd_rshift_imm")]
	    UNSPEC_ASRD)]
	 UNSPEC_PRED_X))]
  "TARGET_SVE"
  "@
  asrd\t%0.<Vetype>, %1/m, %0.<Vetype>, #%3
  movprfx\t%0, %2\;asrd\t%0.<Vetype>, %1/m, %0.<Vetype>, #%3"
  [(set_attr "movprfx" "*,yes")]
)

;; -------------------------------------------------------------------------
;; ---- [FP] General binary arithmetic corresponding to rtx codes
;; -------------------------------------------------------------------------
;; Includes post-RA forms of:
;; - FADD
;; - FMUL
;; - FSUB
;; -------------------------------------------------------------------------

;; Unpredicated floating-point binary operations (post-RA only).
;; These are generated by splitting a predicated instruction whose
;; predicate is unused.
(define_insn "*post_ra_<sve_fp_op><mode>3"
  [(set (match_operand:SVE_F 0 "register_operand" "=w")
	(SVE_UNPRED_FP_BINARY:SVE_F
	  (match_operand:SVE_F 1 "register_operand" "w")
	  (match_operand:SVE_F 2 "register_operand" "w")))]
  "TARGET_SVE && reload_completed"
  "<sve_fp_op>\t%0.<Vetype>, %1.<Vetype>, %2.<Vetype>")

;; -------------------------------------------------------------------------
;; ---- [FP] General binary arithmetic corresponding to unspecs
;; -------------------------------------------------------------------------
;; Includes merging forms of:
;; - FADD    (constant forms handled in the "Addition" section)
;; - FDIV
;; - FDIVR
;; - FMAXNM  (including #0.0 and #1.0)
;; - FMINNM  (including #0.0 and #1.0)
;; - FMUL    (including #0.5 and #2.0)
;; - FSUB    (constant forms handled in the "Addition" section)
;; - FSUBR   (constant forms handled in the "Subtraction" section)
;; -------------------------------------------------------------------------

;; Unpredicated floating-point binary operations.
(define_expand "<optab><mode>3"
  [(set (match_operand:SVE_F 0 "register_operand")
	(unspec:SVE_F
	  [(match_dup 3)
	   (const_int SVE_RELAXED_GP)
	   (match_operand:SVE_F 1 "<sve_pred_fp_rhs1_operand>")
	   (match_operand:SVE_F 2 "<sve_pred_fp_rhs2_operand>")]
	  SVE_COND_FP_BINARY))]
  "TARGET_SVE"
  {
    operands[3] = aarch64_ptrue_reg (<VPRED>mode);
  }
)

;; Predicated floating-point binary operations that have no immediate forms.
(define_insn "*<optab><mode>3"
  [(set (match_operand:SVE_F 0 "register_operand" "=w, w, ?&w")
	(unspec:SVE_F
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl, Upl")
	   (match_operand:SI 4 "aarch64_sve_gp_strictness")
	   (match_operand:SVE_F 2 "register_operand" "0, w, w")
	   (match_operand:SVE_F 3 "register_operand" "w, 0, w")]
	  SVE_COND_FP_BINARY_REG))]
  "TARGET_SVE"
  "@
   <sve_fp_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
   <sve_fp_op_rev>\t%0.<Vetype>, %1/m, %0.<Vetype>, %2.<Vetype>
   movprfx\t%0, %2\;<sve_fp_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>"
  [(set_attr "movprfx" "*,*,yes")]
)

;; Predicated floating-point operations with merging.
(define_expand "cond_<optab><mode>"
  [(set (match_operand:SVE_F 0 "register_operand")
	(unspec:SVE_F
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_F
	     [(match_dup 1)
	      (const_int SVE_STRICT_GP)
	      (match_operand:SVE_F 2 "<sve_pred_fp_rhs1_operand>")
	      (match_operand:SVE_F 3 "<sve_pred_fp_rhs2_operand>")]
	     SVE_COND_FP_BINARY)
	   (match_operand:SVE_F 4 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE"
)

;; Predicated floating-point operations, merging with the first input.
(define_insn_and_rewrite "*cond_<optab><mode>_2"
  [(set (match_operand:SVE_F 0 "register_operand" "=w, ?&w")
	(unspec:SVE_F
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl")
	   (unspec:SVE_F
	     [(match_operand 4)
	      (match_operand:SI 5 "aarch64_sve_gp_strictness")
	      (match_operand:SVE_F 2 "register_operand" "0, w")
	      (match_operand:SVE_F 3 "register_operand" "w, w")]
	     SVE_COND_FP_BINARY)
	   (match_dup 2)]
	  UNSPEC_SEL))]
  "TARGET_SVE && aarch64_sve_pred_dominates_p (&operands[4], operands[1])"
  "@
   <sve_fp_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
   movprfx\t%0, %2\;<sve_fp_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>"
  "&& !rtx_equal_p (operands[1], operands[4])"
  {
    operands[4] = copy_rtx (operands[1]);
  }
  [(set_attr "movprfx" "*,yes")]
)

;; Same for operations that take a 1-bit constant.
(define_insn_and_rewrite "*cond_<optab><mode>_2_const"
  [(set (match_operand:SVE_F 0 "register_operand" "=w, ?w")
	(unspec:SVE_F
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl")
	   (unspec:SVE_F
	     [(match_operand 4)
	      (match_operand:SI 5 "aarch64_sve_gp_strictness")
	      (match_operand:SVE_F 2 "register_operand" "0, w")
	      (match_operand:SVE_F 3 "<sve_pred_fp_rhs2_immediate>")]
	     SVE_COND_FP_BINARY_I1)
	   (match_dup 2)]
	  UNSPEC_SEL))]
  "TARGET_SVE && aarch64_sve_pred_dominates_p (&operands[4], operands[1])"
  "@
   <sve_fp_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, #%3
   movprfx\t%0, %2\;<sve_fp_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, #%3"
  "&& !rtx_equal_p (operands[1], operands[4])"
  {
    operands[4] = copy_rtx (operands[1]);
  }
  [(set_attr "movprfx" "*,yes")]
)

;; Predicated floating-point operations, merging with the second input.
(define_insn_and_rewrite "*cond_<optab><mode>_3"
  [(set (match_operand:SVE_F 0 "register_operand" "=w, ?&w")
	(unspec:SVE_F
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl")
	   (unspec:SVE_F
	     [(match_operand 4)
	      (match_operand:SI 5 "aarch64_sve_gp_strictness")
	      (match_operand:SVE_F 2 "register_operand" "w, w")
	      (match_operand:SVE_F 3 "register_operand" "0, w")]
	     SVE_COND_FP_BINARY)
	   (match_dup 3)]
	  UNSPEC_SEL))]
  "TARGET_SVE && aarch64_sve_pred_dominates_p (&operands[4], operands[1])"
  "@
   <sve_fp_op_rev>\t%0.<Vetype>, %1/m, %0.<Vetype>, %2.<Vetype>
   movprfx\t%0, %3\;<sve_fp_op_rev>\t%0.<Vetype>, %1/m, %0.<Vetype>, %2.<Vetype>"
  "&& !rtx_equal_p (operands[1], operands[4])"
  {
    operands[4] = copy_rtx (operands[1]);
  }
  [(set_attr "movprfx" "*,yes")]
)

;; Predicated floating-point operations, merging with an independent value.
(define_insn_and_rewrite "*cond_<optab><mode>_any"
  [(set (match_operand:SVE_F 0 "register_operand" "=&w, &w, &w, &w, ?&w")
	(unspec:SVE_F
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl, Upl, Upl, Upl")
	   (unspec:SVE_F
	     [(match_operand 5)
	      (match_operand:SI 6 "aarch64_sve_gp_strictness")
	      (match_operand:SVE_F 2 "register_operand" "0, w, w, w, w")
	      (match_operand:SVE_F 3 "register_operand" "w, 0, w, w, w")]
	     SVE_COND_FP_BINARY)
	   (match_operand:SVE_F 4 "aarch64_simd_reg_or_zero" "Dz, Dz, Dz, 0, w")]
	  UNSPEC_SEL))]
  "TARGET_SVE
   && !rtx_equal_p (operands[2], operands[4])
   && !rtx_equal_p (operands[3], operands[4])
   && aarch64_sve_pred_dominates_p (&operands[5], operands[1])"
  "@
   movprfx\t%0.<Vetype>, %1/z, %0.<Vetype>\;<sve_fp_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
   movprfx\t%0.<Vetype>, %1/z, %0.<Vetype>\;<sve_fp_op_rev>\t%0.<Vetype>, %1/m, %0.<Vetype>, %2.<Vetype>
   movprfx\t%0.<Vetype>, %1/z, %2.<Vetype>\;<sve_fp_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
   movprfx\t%0.<Vetype>, %1/m, %2.<Vetype>\;<sve_fp_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
   #"
  "&& 1"
  {
    if (reload_completed
        && register_operand (operands[4], <MODE>mode)
        && !rtx_equal_p (operands[0], operands[4]))
      {
	emit_insn (gen_vcond_mask_<mode><vpred> (operands[0], operands[2],
						 operands[4], operands[1]));
	operands[4] = operands[2] = operands[0];
      }
    else if (!rtx_equal_p (operands[1], operands[5]))
      operands[5] = copy_rtx (operands[1]);
    else
      FAIL;
  }
  [(set_attr "movprfx" "yes")]
)

;; Same for operations that take a 1-bit constant.
(define_insn_and_rewrite "*cond_<optab><mode>_any_const"
  [(set (match_operand:SVE_F 0 "register_operand" "=w, w, ?w")
	(unspec:SVE_F
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl, Upl")
	   (unspec:SVE_F
	     [(match_operand 5)
	      (match_operand:SI 6 "aarch64_sve_gp_strictness")
	      (match_operand:SVE_F 2 "register_operand" "w, w, w")
	      (match_operand:SVE_F 3 "<sve_pred_fp_rhs2_immediate>")]
	     SVE_COND_FP_BINARY_I1)
	   (match_operand:SVE_F 4 "aarch64_simd_reg_or_zero" "Dz, 0, w")]
	  UNSPEC_SEL))]
  "TARGET_SVE
   && !rtx_equal_p (operands[2], operands[4])
   && aarch64_sve_pred_dominates_p (&operands[5], operands[1])"
  "@
   movprfx\t%0.<Vetype>, %1/z, %2.<Vetype>\;<sve_fp_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, #%3
   movprfx\t%0.<Vetype>, %1/m, %2.<Vetype>\;<sve_fp_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, #%3
   #"
  "&& 1"
  {
    if (reload_completed
        && register_operand (operands[4], <MODE>mode)
        && !rtx_equal_p (operands[0], operands[4]))
      {
	emit_insn (gen_vcond_mask_<mode><vpred> (operands[0], operands[2],
						 operands[4], operands[1]));
	operands[4] = operands[2] = operands[0];
      }
    else if (!rtx_equal_p (operands[1], operands[5]))
      operands[5] = copy_rtx (operands[1]);
    else
      FAIL;
  }
  [(set_attr "movprfx" "yes")]
)

;; -------------------------------------------------------------------------
;; ---- [FP] Addition
;; -------------------------------------------------------------------------
;; Includes:
;; - FADD
;; - FSUB
;; -------------------------------------------------------------------------

;; Predicated floating-point addition.
(define_insn_and_split "*add<mode>3"
  [(set (match_operand:SVE_F 0 "register_operand" "=w, w, w, ?&w, ?&w")
	(unspec:SVE_F
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl, Upl, Upl, Upl")
	   (match_operand:SI 4 "aarch64_sve_gp_strictness" "i, i, Z, i, i")
	   (match_operand:SVE_F 2 "register_operand" "%0, 0, w, w, w")
	   (match_operand:SVE_F 3 "aarch64_sve_float_arith_with_sub_operand" "vsA, vsN, w, vsA, vsN")]
	  UNSPEC_COND_FADD))]
  "TARGET_SVE"
  "@
   fadd\t%0.<Vetype>, %1/m, %0.<Vetype>, #%3
   fsub\t%0.<Vetype>, %1/m, %0.<Vetype>, #%N3
   #
   movprfx\t%0, %2\;fadd\t%0.<Vetype>, %1/m, %0.<Vetype>, #%3
   movprfx\t%0, %2\;fsub\t%0.<Vetype>, %1/m, %0.<Vetype>, #%N3"
  ; Split the unpredicated form after reload, so that we don't have
  ; the unnecessary PTRUE.
  "&& reload_completed
   && register_operand (operands[3], <MODE>mode)"
  [(set (match_dup 0) (plus:SVE_F (match_dup 2) (match_dup 3)))]
  ""
  [(set_attr "movprfx" "*,*,*,yes,yes")]
)

;; Predicated floating-point addition of a constant, merging with the
;; first input.
(define_insn_and_rewrite "*cond_add<mode>_2_const"
  [(set (match_operand:SVE_F 0 "register_operand" "=w, w, ?w, ?w")
	(unspec:SVE_F
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl, Upl, Upl")
	   (unspec:SVE_F
	     [(match_operand 4)
	      (match_operand:SI 5 "aarch64_sve_gp_strictness")
	      (match_operand:SVE_F 2 "register_operand" "0, 0, w, w")
	      (match_operand:SVE_F 3 "aarch64_sve_float_arith_with_sub_immediate" "vsA, vsN, vsA, vsN")]
	     UNSPEC_COND_FADD)
	   (match_dup 2)]
	  UNSPEC_SEL))]
  "TARGET_SVE && aarch64_sve_pred_dominates_p (&operands[4], operands[1])"
  "@
   fadd\t%0.<Vetype>, %1/m, %0.<Vetype>, #%3
   fsub\t%0.<Vetype>, %1/m, %0.<Vetype>, #%N3
   movprfx\t%0, %2\;fadd\t%0.<Vetype>, %1/m, %0.<Vetype>, #%3
   movprfx\t%0, %2\;fsub\t%0.<Vetype>, %1/m, %0.<Vetype>, #%N3"
  "&& !rtx_equal_p (operands[1], operands[4])"
  {
    operands[4] = copy_rtx (operands[1]);
  }
  [(set_attr "movprfx" "*,*,yes,yes")]
)

;; Predicated floating-point addition of a constant, merging with an
;; independent value.
(define_insn_and_rewrite "*cond_add<mode>_any_const"
  [(set (match_operand:SVE_F 0 "register_operand" "=w, w, w, w, ?w, ?w")
	(unspec:SVE_F
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl, Upl, Upl, Upl, Upl")
	   (unspec:SVE_F
	     [(match_operand 5)
	      (match_operand:SI 6 "aarch64_sve_gp_strictness")
	      (match_operand:SVE_F 2 "register_operand" "w, w, w, w, w, w")
	      (match_operand:SVE_F 3 "aarch64_sve_float_arith_with_sub_immediate" "vsA, vsN, vsA, vsN, vsA, vsN")]
	     UNSPEC_COND_FADD)
	   (match_operand:SVE_F 4 "aarch64_simd_reg_or_zero" "Dz, Dz, 0, 0, w, w")]
	  UNSPEC_SEL))]
  "TARGET_SVE
   && !rtx_equal_p (operands[2], operands[4])
   && aarch64_sve_pred_dominates_p (&operands[5], operands[1])"
  "@
   movprfx\t%0.<Vetype>, %1/z, %2.<Vetype>\;fadd\t%0.<Vetype>, %1/m, %0.<Vetype>, #%3
   movprfx\t%0.<Vetype>, %1/z, %2.<Vetype>\;fsub\t%0.<Vetype>, %1/m, %0.<Vetype>, #%N3
   movprfx\t%0.<Vetype>, %1/m, %2.<Vetype>\;fadd\t%0.<Vetype>, %1/m, %0.<Vetype>, #%3
   movprfx\t%0.<Vetype>, %1/m, %2.<Vetype>\;fsub\t%0.<Vetype>, %1/m, %0.<Vetype>, #%N3
   #
   #"
  "&& 1"
  {
    if (reload_completed
        && register_operand (operands[4], <MODE>mode)
        && !rtx_equal_p (operands[0], operands[4]))
      {
	emit_insn (gen_vcond_mask_<mode><vpred> (operands[0], operands[2],
						 operands[4], operands[1]));
	operands[4] = operands[2] = operands[0];
      }
    else if (!rtx_equal_p (operands[1], operands[5]))
      operands[5] = copy_rtx (operands[1]);
    else
      FAIL;
  }
  [(set_attr "movprfx" "yes")]
)

;; Register merging forms are handled through SVE_COND_FP_BINARY.

;; -------------------------------------------------------------------------
;; ---- [FP] Subtraction
;; -------------------------------------------------------------------------
;; Includes:
;; - FSUB
;; - FSUBR
;; -------------------------------------------------------------------------

;; Predicated floating-point subtraction.
(define_insn_and_split "*sub<mode>3"
  [(set (match_operand:SVE_F 0 "register_operand" "=w, w, ?&w")
	(unspec:SVE_F
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl, Upl")
	   (match_operand:SI 4 "aarch64_sve_gp_strictness" "i, Z, i")
	   (match_operand:SVE_F 2 "aarch64_sve_float_arith_operand" "vsA, w, vsA")
	   (match_operand:SVE_F 3 "register_operand" "0, w, 0")]
	  UNSPEC_COND_FSUB))]
  "TARGET_SVE"
  "@
   fsubr\t%0.<Vetype>, %1/m, %0.<Vetype>, #%2
   #
   movprfx\t%0, %3\;fsubr\t%0.<Vetype>, %1/m, %0.<Vetype>, #%2"
  ; Split the unpredicated form after reload, so that we don't have
  ; the unnecessary PTRUE.
  "&& reload_completed
   && register_operand (operands[2], <MODE>mode)"
  [(set (match_dup 0) (minus:SVE_F (match_dup 2) (match_dup 3)))]
  ""
  [(set_attr "movprfx" "*,*,yes")]
)

;; Predicated floating-point subtraction from a constant, merging with the
;; second input.
(define_insn_and_rewrite "*cond_sub<mode>_3_const"
  [(set (match_operand:SVE_F 0 "register_operand" "=w, ?w")
	(unspec:SVE_F
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl")
	   (unspec:SVE_F
	     [(match_operand 4)
	      (match_operand:SI 5 "aarch64_sve_gp_strictness")
	      (match_operand:SVE_F 2 "aarch64_sve_float_arith_immediate")
	      (match_operand:SVE_F 3 "register_operand" "0, w")]
	     UNSPEC_COND_FSUB)
	   (match_dup 3)]
	  UNSPEC_SEL))]
  "TARGET_SVE && aarch64_sve_pred_dominates_p (&operands[4], operands[1])"
  "@
   fsubr\t%0.<Vetype>, %1/m, %0.<Vetype>, #%2
   movprfx\t%0, %3\;fsubr\t%0.<Vetype>, %1/m, %0.<Vetype>, #%2"
  "&& !rtx_equal_p (operands[1], operands[4])"
  {
    operands[4] = copy_rtx (operands[1]);
  }
  [(set_attr "movprfx" "*,yes")]
)

;; Predicated floating-point subtraction from a constant, merging with an
;; independent value.
(define_insn_and_rewrite "*cond_sub<mode>_any_const"
  [(set (match_operand:SVE_F 0 "register_operand" "=w, w, ?w")
	(unspec:SVE_F
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl, Upl")
	   (unspec:SVE_F
	     [(match_operand 5)
	      (match_operand:SI 6 "aarch64_sve_gp_strictness")
	      (match_operand:SVE_F 2 "aarch64_sve_float_arith_immediate")
	      (match_operand:SVE_F 3 "register_operand" "w, w, w")]
	     UNSPEC_COND_FSUB)
	   (match_operand:SVE_F 4 "aarch64_simd_reg_or_zero" "Dz, 0, w")]
	  UNSPEC_SEL))]
  "TARGET_SVE
   && !rtx_equal_p (operands[3], operands[4])
   && aarch64_sve_pred_dominates_p (&operands[5], operands[1])"
  "@
   movprfx\t%0.<Vetype>, %1/z, %3.<Vetype>\;fsubr\t%0.<Vetype>, %1/m, %0.<Vetype>, #%2
   movprfx\t%0.<Vetype>, %1/m, %3.<Vetype>\;fsubr\t%0.<Vetype>, %1/m, %0.<Vetype>, #%2
   #"
  "&& 1"
  {
    if (reload_completed
        && register_operand (operands[4], <MODE>mode)
        && !rtx_equal_p (operands[0], operands[4]))
      {
	emit_insn (gen_vcond_mask_<mode><vpred> (operands[0], operands[3],
						 operands[4], operands[1]));
	operands[4] = operands[3] = operands[0];
      }
    else if (!rtx_equal_p (operands[1], operands[5]))
      operands[5] = copy_rtx (operands[1]);
    else
      FAIL;
  }
  [(set_attr "movprfx" "yes")]
)

;; Register merging forms are handled through SVE_COND_FP_BINARY.

;; -------------------------------------------------------------------------
;; ---- [FP] Absolute difference
;; -------------------------------------------------------------------------
;; Includes:
;; - FABD
;; -------------------------------------------------------------------------

;; Predicated floating-point absolute difference.
(define_insn_and_rewrite "*fabd<mode>3"
  [(set (match_operand:SVE_F 0 "register_operand" "=w, ?&w")
	(unspec:SVE_F
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl")
	   (match_operand:SI 4 "aarch64_sve_gp_strictness")
	   (unspec:SVE_F
	     [(match_operand 5)
	      (match_operand:SI 6 "aarch64_sve_gp_strictness")
	      (match_operand:SVE_F 2 "register_operand" "%0, w")
	      (match_operand:SVE_F 3 "register_operand" "w, w")]
	     UNSPEC_COND_FSUB)]
	  UNSPEC_COND_FABS))]
  "TARGET_SVE && aarch64_sve_pred_dominates_p (&operands[5], operands[1])"
  "@
   fabd\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
   movprfx\t%0, %2\;fabd\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>"
  "&& !rtx_equal_p (operands[1], operands[5])"
  {
    operands[5] = copy_rtx (operands[1]);
  }
  [(set_attr "movprfx" "*,yes")]
)

;; Predicated floating-point absolute difference, merging with the first
;; input.
(define_insn_and_rewrite "*aarch64_cond_abd<mode>_2"
  [(set (match_operand:SVE_F 0 "register_operand" "=w, ?&w")
	(unspec:SVE_F
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl")
	   (unspec:SVE_F
	     [(match_operand 4)
	      (match_operand:SI 5 "aarch64_sve_gp_strictness")
	      (unspec:SVE_F
		[(match_operand 6)
		 (match_operand:SI 7 "aarch64_sve_gp_strictness")
		 (match_operand:SVE_F 2 "register_operand" "0, w")
		 (match_operand:SVE_F 3 "register_operand" "w, w")]
		UNSPEC_COND_FSUB)]
	     UNSPEC_COND_FABS)
	   (match_dup 2)]
	  UNSPEC_SEL))]
  "TARGET_SVE
   && aarch64_sve_pred_dominates_p (&operands[4], operands[1])
   && aarch64_sve_pred_dominates_p (&operands[6], operands[1])"
  "@
   fabd\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
   movprfx\t%0, %2\;fabd\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>"
  "&& (!rtx_equal_p (operands[1], operands[4])
       || !rtx_equal_p (operands[1], operands[6]))"
  {
    operands[4] = copy_rtx (operands[1]);
    operands[6] = copy_rtx (operands[1]);
  }
  [(set_attr "movprfx" "*,yes")]
)

;; Predicated floating-point absolute difference, merging with the second
;; input.
(define_insn_and_rewrite "*aarch64_cond_abd<mode>_3"
  [(set (match_operand:SVE_F 0 "register_operand" "=w, ?&w")
	(unspec:SVE_F
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl")
	   (unspec:SVE_F
	     [(match_operand 4)
	      (match_operand:SI 5 "aarch64_sve_gp_strictness")
	      (unspec:SVE_F
		[(match_operand 6)
		 (match_operand:SI 7 "aarch64_sve_gp_strictness")
		 (match_operand:SVE_F 2 "register_operand" "w, w")
		 (match_operand:SVE_F 3 "register_operand" "0, w")]
		UNSPEC_COND_FSUB)]
	     UNSPEC_COND_FABS)
	   (match_dup 3)]
	  UNSPEC_SEL))]
  "TARGET_SVE
   && aarch64_sve_pred_dominates_p (&operands[4], operands[1])
   && aarch64_sve_pred_dominates_p (&operands[6], operands[1])"
  "@
   fabd\t%0.<Vetype>, %1/m, %0.<Vetype>, %2.<Vetype>
   movprfx\t%0, %3\;fabd\t%0.<Vetype>, %1/m, %0.<Vetype>, %2.<Vetype>"
  "&& (!rtx_equal_p (operands[1], operands[4])
       || !rtx_equal_p (operands[1], operands[6]))"
  {
    operands[4] = copy_rtx (operands[1]);
    operands[6] = copy_rtx (operands[1]);
  }
  [(set_attr "movprfx" "*,yes")]
)

;; Predicated floating-point absolute difference, merging with an
;; independent value.
(define_insn_and_rewrite "*aarch64_cond_abd<mode>_any"
  [(set (match_operand:SVE_F 0 "register_operand" "=&w, &w, &w, &w, ?&w")
	(unspec:SVE_F
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl, Upl, Upl, Upl")
	   (unspec:SVE_F
	     [(match_operand 5)
	      (match_operand:SI 6 "aarch64_sve_gp_strictness")
	      (unspec:SVE_F
		[(match_operand 7)
		 (match_operand:SI 8 "aarch64_sve_gp_strictness")
		 (match_operand:SVE_F 2 "register_operand" "0, w, w, w, w")
		 (match_operand:SVE_F 3 "register_operand" "w, 0, w, w, w")]
		UNSPEC_COND_FSUB)]
	     UNSPEC_COND_FABS)
	   (match_operand:SVE_F 4 "aarch64_simd_reg_or_zero" "Dz, Dz, Dz, 0, w")]
	  UNSPEC_SEL))]
  "TARGET_SVE
   && !rtx_equal_p (operands[2], operands[4])
   && !rtx_equal_p (operands[3], operands[4])
   && aarch64_sve_pred_dominates_p (&operands[5], operands[1])
   && aarch64_sve_pred_dominates_p (&operands[7], operands[1])"
  "@
   movprfx\t%0.<Vetype>, %1/z, %0.<Vetype>\;fabd\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
   movprfx\t%0.<Vetype>, %1/z, %0.<Vetype>\;fabd\t%0.<Vetype>, %1/m, %0.<Vetype>, %2.<Vetype>
   movprfx\t%0.<Vetype>, %1/z, %2.<Vetype>\;fabd\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
   movprfx\t%0.<Vetype>, %1/m, %2.<Vetype>\;fabd\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
   #"
  "&& 1"
  {
    if (reload_completed
        && register_operand (operands[4], <MODE>mode)
        && !rtx_equal_p (operands[0], operands[4]))
      {
	emit_insn (gen_vcond_mask_<mode><vpred> (operands[0], operands[3],
						 operands[4], operands[1]));
	operands[4] = operands[3] = operands[0];
      }
    else if (!rtx_equal_p (operands[1], operands[5])
	     || !rtx_equal_p (operands[1], operands[7]))
      {
	operands[5] = copy_rtx (operands[1]);
	operands[7] = copy_rtx (operands[1]);
      }
    else
      FAIL;
  }
  [(set_attr "movprfx" "yes")]
)

;; -------------------------------------------------------------------------
;; ---- [FP] Multiplication
;; -------------------------------------------------------------------------
;; Includes:
;; - FMUL
;; -------------------------------------------------------------------------

;; Predicated floating-point multiplication.
(define_insn_and_split "*mul<mode>3"
  [(set (match_operand:SVE_F 0 "register_operand" "=w, w, ?&w")
	(unspec:SVE_F
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl, Upl")
	   (match_operand:SI 4 "aarch64_sve_gp_strictness" "i, Z, i")
	   (match_operand:SVE_F 2 "register_operand" "%0, w, 0")
	   (match_operand:SVE_F 3 "aarch64_sve_float_mul_operand" "vsM, w, vsM")]
	  UNSPEC_COND_FMUL))]
  "TARGET_SVE"
  "@
   fmul\t%0.<Vetype>, %1/m, %0.<Vetype>, #%3
   #
   movprfx\t%0, %2\;fmul\t%0.<Vetype>, %1/m, %0.<Vetype>, #%3"
  ; Split the unpredicated form after reload, so that we don't have
  ; the unnecessary PTRUE.
  "&& reload_completed
   && register_operand (operands[3], <MODE>mode)"
  [(set (match_dup 0) (mult:SVE_F (match_dup 2) (match_dup 3)))]
  ""
  [(set_attr "movprfx" "*,*,yes")]
)

;; Merging forms are handled through SVE_COND_FP_BINARY and
;; SVE_COND_FP_BINARY_I1.

;; -------------------------------------------------------------------------
;; ---- [FP] Binary logical operations
;; -------------------------------------------------------------------------
;; Includes
;; - AND
;; - EOR
;; - ORR
;; -------------------------------------------------------------------------

;; Binary logical operations on floating-point modes.  We avoid subregs
;; by providing this, but we need to use UNSPECs since rtx logical ops
;; aren't defined for floating-point modes.
(define_insn "*<optab><mode>3"
  [(set (match_operand:SVE_F 0 "register_operand" "=w")
	(unspec:SVE_F [(match_operand:SVE_F 1 "register_operand" "w")
		       (match_operand:SVE_F 2 "register_operand" "w")]
		      LOGICALF))]
  "TARGET_SVE"
  "<logicalf_op>\t%0.d, %1.d, %2.d"
)

;; -------------------------------------------------------------------------
;; ---- [FP] Sign copying
;; -------------------------------------------------------------------------
;; The patterns in this section are synthetic.
;; -------------------------------------------------------------------------

(define_expand "copysign<mode>3"
  [(match_operand:SVE_F 0 "register_operand")
   (match_operand:SVE_F 1 "register_operand")
   (match_operand:SVE_F 2 "register_operand")]
  "TARGET_SVE"
  {
    rtx sign = gen_reg_rtx (<V_INT_EQUIV>mode);
    rtx mant = gen_reg_rtx (<V_INT_EQUIV>mode);
    rtx int_res = gen_reg_rtx (<V_INT_EQUIV>mode);
    int bits = GET_MODE_UNIT_BITSIZE (<MODE>mode) - 1;

    rtx arg1 = lowpart_subreg (<V_INT_EQUIV>mode, operands[1], <MODE>mode);
    rtx arg2 = lowpart_subreg (<V_INT_EQUIV>mode, operands[2], <MODE>mode);

    emit_insn (gen_and<v_int_equiv>3
	       (sign, arg2,
		aarch64_simd_gen_const_vector_dup (<V_INT_EQUIV>mode,
						   HOST_WIDE_INT_M1U
						   << bits)));
    emit_insn (gen_and<v_int_equiv>3
	       (mant, arg1,
		aarch64_simd_gen_const_vector_dup (<V_INT_EQUIV>mode,
						   ~(HOST_WIDE_INT_M1U
						     << bits))));
    emit_insn (gen_ior<v_int_equiv>3 (int_res, sign, mant));
    emit_move_insn (operands[0], gen_lowpart (<MODE>mode, int_res));
    DONE;
  }
)

(define_expand "xorsign<mode>3"
  [(match_operand:SVE_F 0 "register_operand")
   (match_operand:SVE_F 1 "register_operand")
   (match_operand:SVE_F 2 "register_operand")]
  "TARGET_SVE"
  {
    rtx sign = gen_reg_rtx (<V_INT_EQUIV>mode);
    rtx int_res = gen_reg_rtx (<V_INT_EQUIV>mode);
    int bits = GET_MODE_UNIT_BITSIZE (<MODE>mode) - 1;

    rtx arg1 = lowpart_subreg (<V_INT_EQUIV>mode, operands[1], <MODE>mode);
    rtx arg2 = lowpart_subreg (<V_INT_EQUIV>mode, operands[2], <MODE>mode);

    emit_insn (gen_and<v_int_equiv>3
	       (sign, arg2,
		aarch64_simd_gen_const_vector_dup (<V_INT_EQUIV>mode,
						   HOST_WIDE_INT_M1U
						   << bits)));
    emit_insn (gen_xor<v_int_equiv>3 (int_res, arg1, sign));
    emit_move_insn (operands[0], gen_lowpart (<MODE>mode, int_res));
    DONE;
  }
)

;; -------------------------------------------------------------------------
;; ---- [FP] Maximum and minimum
;; -------------------------------------------------------------------------
;; Includes:
;; - FMAXNM
;; - FMINNM
;; -------------------------------------------------------------------------

;; Unpredicated fmax/fmin (the libm functions).  The optabs for the
;; smin/smax rtx codes are handled in the generic section above.
(define_expand "<maxmin_uns><mode>3"
  [(set (match_operand:SVE_F 0 "register_operand")
	(unspec:SVE_F
	  [(match_dup 3)
	   (const_int SVE_RELAXED_GP)
	   (match_operand:SVE_F 1 "register_operand")
	   (match_operand:SVE_F 2 "aarch64_sve_float_maxmin_operand")]
	  SVE_COND_FP_MAXMIN_PUBLIC))]
  "TARGET_SVE"
  {
    operands[3] = aarch64_ptrue_reg (<VPRED>mode);
  }
)

;; Predicated floating-point maximum/minimum.
(define_insn "*<optab><mode>3"
  [(set (match_operand:SVE_F 0 "register_operand" "=w, w, ?&w, ?&w")
	(unspec:SVE_F
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl, Upl, Upl")
	   (match_operand:SI 4 "aarch64_sve_gp_strictness")
	   (match_operand:SVE_F 2 "register_operand" "%0, 0, w, w")
	   (match_operand:SVE_F 3 "aarch64_sve_float_maxmin_operand" "vsB, w, vsB, w")]
	  SVE_COND_FP_MAXMIN_PUBLIC))]
  "TARGET_SVE"
  "@
   <sve_fp_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, #%3
   <sve_fp_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
   movprfx\t%0, %2\;<sve_fp_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, #%3
   movprfx\t%0, %2\;<sve_fp_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>"
  [(set_attr "movprfx" "*,*,yes,yes")]
)

;; Merging forms are handled through SVE_COND_FP_BINARY and
;; SVE_COND_FP_BINARY_I1.

;; -------------------------------------------------------------------------
;; ---- [PRED] Binary logical operations
;; -------------------------------------------------------------------------
;; Includes:
;; - AND
;; - ANDS
;; - EOR
;; - EORS
;; - ORR
;; - ORRS
;; -------------------------------------------------------------------------

;; Predicate AND.  We can reuse one of the inputs as the GP.
;; Doubling the second operand is the preferred implementation
;; of the MOV alias, so we use that instead of %1/z, %1, %2.
(define_insn "and<mode>3"
  [(set (match_operand:PRED_ALL 0 "register_operand" "=Upa")
	(and:PRED_ALL (match_operand:PRED_ALL 1 "register_operand" "Upa")
		      (match_operand:PRED_ALL 2 "register_operand" "Upa")))]
  "TARGET_SVE"
  "and\t%0.b, %1/z, %2.b, %2.b"
)

;; Unpredicated predicate EOR and ORR.
(define_expand "<optab><mode>3"
  [(set (match_operand:PRED_ALL 0 "register_operand")
	(and:PRED_ALL
	  (LOGICAL_OR:PRED_ALL
	    (match_operand:PRED_ALL 1 "register_operand")
	    (match_operand:PRED_ALL 2 "register_operand"))
	  (match_dup 3)))]
  "TARGET_SVE"
  {
    operands[3] = aarch64_ptrue_reg (<MODE>mode);
  }
)

;; Predicated predicate AND, EOR and ORR.
(define_insn "@aarch64_pred_<optab><mode>_z"
  [(set (match_operand:PRED_ALL 0 "register_operand" "=Upa")
	(and:PRED_ALL
	  (LOGICAL:PRED_ALL
	    (match_operand:PRED_ALL 2 "register_operand" "Upa")
	    (match_operand:PRED_ALL 3 "register_operand" "Upa"))
	  (match_operand:PRED_ALL 1 "register_operand" "Upa")))]
  "TARGET_SVE"
  "<logical>\t%0.b, %1/z, %2.b, %3.b"
)

;; Perform a logical operation on operands 2 and 3, using operand 1 as
;; the GP.  Store the result in operand 0 and set the flags in the same
;; way as for PTEST.
(define_insn "*<optab><mode>3_cc"
  [(set (reg:CC_NZC CC_REGNUM)
	(unspec:CC_NZC
	  [(match_operand:VNx16BI 1 "register_operand" "Upa")
	   (match_operand 4)
	   (match_operand:SI 5 "aarch64_sve_ptrue_flag")
	   (and:PRED_ALL
	     (LOGICAL:PRED_ALL
	       (match_operand:PRED_ALL 2 "register_operand" "Upa")
	       (match_operand:PRED_ALL 3 "register_operand" "Upa"))
	     (match_dup 4))]
	  UNSPEC_PTEST))
   (set (match_operand:PRED_ALL 0 "register_operand" "=Upa")
	(and:PRED_ALL (LOGICAL:PRED_ALL (match_dup 2) (match_dup 3))
		      (match_dup 4)))]
  "TARGET_SVE"
  "<logical>s\t%0.b, %1/z, %2.b, %3.b"
)

;; -------------------------------------------------------------------------
;; ---- [PRED] Binary logical operations (inverted second input)
;; -------------------------------------------------------------------------
;; Includes:
;; - BIC
;; - ORN
;; -------------------------------------------------------------------------

;; Predicated predicate BIC and ORN.
(define_insn "*<nlogical><mode>3"
  [(set (match_operand:PRED_ALL 0 "register_operand" "=Upa")
	(and:PRED_ALL
	  (NLOGICAL:PRED_ALL
	    (not:PRED_ALL (match_operand:PRED_ALL 3 "register_operand" "Upa"))
	    (match_operand:PRED_ALL 2 "register_operand" "Upa"))
	  (match_operand:PRED_ALL 1 "register_operand" "Upa")))]
  "TARGET_SVE"
  "<nlogical>\t%0.b, %1/z, %2.b, %3.b"
)

;; -------------------------------------------------------------------------
;; ---- [PRED] Binary logical operations (inverted result)
;; -------------------------------------------------------------------------
;; Includes:
;; - NAND
;; - NOR
;; -------------------------------------------------------------------------

;; Predicated predicate NAND and NOR.
(define_insn "*<logical_nn><mode>3"
  [(set (match_operand:PRED_ALL 0 "register_operand" "=Upa")
	(and:PRED_ALL
	  (NLOGICAL:PRED_ALL
	    (not:PRED_ALL (match_operand:PRED_ALL 2 "register_operand" "Upa"))
	    (not:PRED_ALL (match_operand:PRED_ALL 3 "register_operand" "Upa")))
	  (match_operand:PRED_ALL 1 "register_operand" "Upa")))]
  "TARGET_SVE"
  "<logical_nn>\t%0.b, %1/z, %2.b, %3.b"
)

;; =========================================================================
;; == Ternary arithmetic
;; =========================================================================

;; -------------------------------------------------------------------------
;; ---- [INT] MLA and MAD
;; -------------------------------------------------------------------------
;; Includes:
;; - MAD
;; - MLA
;; -------------------------------------------------------------------------

;; Unpredicated integer addition of product.
(define_expand "fma<mode>4"
  [(set (match_operand:SVE_I 0 "register_operand")
	(plus:SVE_I
	  (unspec:SVE_I
	    [(match_dup 4)
	     (mult:SVE_I (match_operand:SVE_I 1 "register_operand")
			 (match_operand:SVE_I 2 "nonmemory_operand"))]
	    UNSPEC_PRED_X)
	  (match_operand:SVE_I 3 "register_operand")))]
  "TARGET_SVE"
  {
    if (aarch64_prepare_sve_int_fma (operands, PLUS))
      DONE;
    operands[4] = aarch64_ptrue_reg (<VPRED>mode);
  }
)

;; Predicated integer addition of product.
(define_insn "*fma<mode>4"
  [(set (match_operand:SVE_I 0 "register_operand" "=w, w, ?&w")
	(plus:SVE_I
	  (unspec:SVE_I
	    [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl, Upl")
	     (mult:SVE_I (match_operand:SVE_I 2 "register_operand" "%0, w, w")
			 (match_operand:SVE_I 3 "register_operand" "w, w, w"))]
	    UNSPEC_PRED_X)
	  (match_operand:SVE_I 4 "register_operand" "w, 0, w")))]
  "TARGET_SVE"
  "@
   mad\t%0.<Vetype>, %1/m, %3.<Vetype>, %4.<Vetype>
   mla\t%0.<Vetype>, %1/m, %2.<Vetype>, %3.<Vetype>
   movprfx\t%0, %4\;mla\t%0.<Vetype>, %1/m, %2.<Vetype>, %3.<Vetype>"
  [(set_attr "movprfx" "*,*,yes")]
)

;; Predicated integer addition of product with merging.
(define_expand "cond_fma<mode>"
  [(set (match_operand:SVE_I 0 "register_operand")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand")
	   (plus:SVE_I
	     (mult:SVE_I (match_operand:SVE_I 2 "register_operand")
			 (match_operand:SVE_I 3 "general_operand"))
	     (match_operand:SVE_I 4 "register_operand"))
	   (match_operand:SVE_I 5 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  {
    if (aarch64_prepare_sve_cond_int_fma (operands, PLUS))
      DONE;
    /* Swap the multiplication operands if the fallback value is the
       second of the two.  */
    if (rtx_equal_p (operands[3], operands[5]))
      std::swap (operands[2], operands[3]);
  }
)

;; Predicated integer addition of product, merging with the first input.
(define_insn "*cond_fma<mode>_2"
  [(set (match_operand:SVE_I 0 "register_operand" "=w, ?&w")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl")
	   (plus:SVE_I
	     (mult:SVE_I (match_operand:SVE_I 2 "register_operand" "0, w")
			 (match_operand:SVE_I 3 "register_operand" "w, w"))
	     (match_operand:SVE_I 4 "register_operand" "w, w"))
	   (match_dup 2)]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  "@
   mad\t%0.<Vetype>, %1/m, %3.<Vetype>, %4.<Vetype>
   movprfx\t%0, %2\;mad\t%0.<Vetype>, %1/m, %3.<Vetype>, %4.<Vetype>"
  [(set_attr "movprfx" "*,yes")]
)

;; Predicated integer addition of product, merging with the third input.
(define_insn "*cond_fma<mode>_4"
  [(set (match_operand:SVE_I 0 "register_operand" "=w, ?&w")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl")
	   (plus:SVE_I
	     (mult:SVE_I (match_operand:SVE_I 2 "register_operand" "w, w")
			 (match_operand:SVE_I 3 "register_operand" "w, w"))
	     (match_operand:SVE_I 4 "register_operand" "0, w"))
	   (match_dup 4)]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  "@
   mla\t%0.<Vetype>, %1/m, %2.<Vetype>, %3.<Vetype>
   movprfx\t%0, %4\;mla\t%0.<Vetype>, %1/m, %2.<Vetype>, %3.<Vetype>"
  [(set_attr "movprfx" "*,yes")]
)

;; Predicated integer addition of product, merging with an independent value.
(define_insn_and_rewrite "*cond_fma<mode>_any"
  [(set (match_operand:SVE_I 0 "register_operand" "=&w, &w, &w, &w, &w, ?&w")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl, Upl, Upl, Upl, Upl")
	   (plus:SVE_I
	     (mult:SVE_I (match_operand:SVE_I 2 "register_operand" "w, w, 0, w, w, w")
			 (match_operand:SVE_I 3 "register_operand" "w, w, w, 0, w, w"))
	     (match_operand:SVE_I 4 "register_operand" "w, 0, w, w, w, w"))
	   (match_operand:SVE_I 5 "aarch64_simd_reg_or_zero" "Dz, Dz, Dz, Dz, 0, w")]
	  UNSPEC_SEL))]
  "TARGET_SVE
   && !rtx_equal_p (operands[2], operands[5])
   && !rtx_equal_p (operands[3], operands[5])
   && !rtx_equal_p (operands[4], operands[5])"
  "@
   movprfx\t%0.<Vetype>, %1/z, %4.<Vetype>\;mla\t%0.<Vetype>, %1/m, %2.<Vetype>, %3.<Vetype>
   movprfx\t%0.<Vetype>, %1/z, %0.<Vetype>\;mla\t%0.<Vetype>, %1/m, %2.<Vetype>, %3.<Vetype>
   movprfx\t%0.<Vetype>, %1/z, %0.<Vetype>\;mad\t%0.<Vetype>, %1/m, %3.<Vetype>, %4.<Vetype>
   movprfx\t%0.<Vetype>, %1/z, %0.<Vetype>\;mad\t%0.<Vetype>, %1/m, %2.<Vetype>, %4.<Vetype>
   movprfx\t%0.<Vetype>, %1/m, %4.<Vetype>\;mla\t%0.<Vetype>, %1/m, %2.<Vetype>, %3.<Vetype>
   #"
  "&& reload_completed
   && register_operand (operands[5], <MODE>mode)
   && !rtx_equal_p (operands[0], operands[5])"
  {
    emit_insn (gen_vcond_mask_<mode><vpred> (operands[0], operands[4],
					     operands[5], operands[1]));
    operands[5] = operands[4] = operands[0];
  }
  [(set_attr "movprfx" "yes")]
)

;; -------------------------------------------------------------------------
;; ---- [INT] MLS and MSB
;; -------------------------------------------------------------------------
;; Includes:
;; - MLS
;; - MSB
;; -------------------------------------------------------------------------

;; Unpredicated integer subtraction of product.
(define_expand "fnma<mode>4"
  [(set (match_operand:SVE_I 0 "register_operand")
	(minus:SVE_I
	  (match_operand:SVE_I 3 "register_operand")
	  (unspec:SVE_I
	    [(match_dup 4)
	     (mult:SVE_I (match_operand:SVE_I 1 "register_operand")
			 (match_operand:SVE_I 2 "general_operand"))]
	    UNSPEC_PRED_X)))]
  "TARGET_SVE"
  {
    if (aarch64_prepare_sve_int_fma (operands, MINUS))
      DONE;
    operands[4] = aarch64_ptrue_reg (<VPRED>mode);
  }
)

;; Predicated integer subtraction of product.
(define_insn "*fnma<mode>3"
  [(set (match_operand:SVE_I 0 "register_operand" "=w, w, ?&w")
	(minus:SVE_I
	  (match_operand:SVE_I 4 "register_operand" "w, 0, w")
	  (unspec:SVE_I
	    [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl, Upl")
	     (mult:SVE_I (match_operand:SVE_I 2 "register_operand" "%0, w, w")
			 (match_operand:SVE_I 3 "register_operand" "w, w, w"))]
	    UNSPEC_PRED_X)))]
  "TARGET_SVE"
  "@
   msb\t%0.<Vetype>, %1/m, %3.<Vetype>, %4.<Vetype>
   mls\t%0.<Vetype>, %1/m, %2.<Vetype>, %3.<Vetype>
   movprfx\t%0, %4\;mls\t%0.<Vetype>, %1/m, %2.<Vetype>, %3.<Vetype>"
  [(set_attr "movprfx" "*,*,yes")]
)

;; Predicated integer subtraction of product with merging.
(define_expand "cond_fnma<mode>"
  [(set (match_operand:SVE_I 0 "register_operand")
   (unspec:SVE_I
	[(match_operand:<VPRED> 1 "register_operand")
	 (minus:SVE_I
	   (match_operand:SVE_I 4 "register_operand")
	   (mult:SVE_I (match_operand:SVE_I 2 "register_operand")
		       (match_operand:SVE_I 3 "general_operand")))
	 (match_operand:SVE_I 5 "aarch64_simd_reg_or_zero")]
	UNSPEC_SEL))]
  "TARGET_SVE"
  {
    if (aarch64_prepare_sve_cond_int_fma (operands, MINUS))
      DONE;
    /* Swap the multiplication operands if the fallback value is the
       second of the two.  */
    if (rtx_equal_p (operands[3], operands[5]))
      std::swap (operands[2], operands[3]);
  }
)

;; Predicated integer subtraction of product, merging with the first input.
(define_insn "*cond_fnma<mode>_2"
  [(set (match_operand:SVE_I 0 "register_operand" "=w, ?&w")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl")
	   (minus:SVE_I
	     (match_operand:SVE_I 4 "register_operand" "w, w")
	     (mult:SVE_I (match_operand:SVE_I 2 "register_operand" "0, w")
			 (match_operand:SVE_I 3 "register_operand" "w, w")))
	   (match_dup 2)]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  "@
   msb\t%0.<Vetype>, %1/m, %3.<Vetype>, %4.<Vetype>
   movprfx\t%0, %2\;msb\t%0.<Vetype>, %1/m, %3.<Vetype>, %4.<Vetype>"
  [(set_attr "movprfx" "*,yes")]
)

;; Predicated integer subtraction of product, merging with the third input.
(define_insn "*cond_fnma<mode>_4"
  [(set (match_operand:SVE_I 0 "register_operand" "=w, ?&w")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl")
	   (minus:SVE_I
	     (match_operand:SVE_I 4 "register_operand" "0, w")
	     (mult:SVE_I (match_operand:SVE_I 2 "register_operand" "w, w")
			 (match_operand:SVE_I 3 "register_operand" "w, w")))
	   (match_dup 4)]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  "@
   mls\t%0.<Vetype>, %1/m, %2.<Vetype>, %3.<Vetype>
   movprfx\t%0, %4\;mls\t%0.<Vetype>, %1/m, %2.<Vetype>, %3.<Vetype>"
  [(set_attr "movprfx" "*,yes")]
)

;; Predicated integer subtraction of product, merging with an
;; independent value.
(define_insn_and_rewrite "*cond_fnma<mode>_any"
  [(set (match_operand:SVE_I 0 "register_operand" "=&w, &w, &w, &w, &w, ?&w")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl, Upl, Upl, Upl, Upl")
	   (minus:SVE_I
	     (match_operand:SVE_I 4 "register_operand" "w, 0, w, w, w, w")
	     (mult:SVE_I (match_operand:SVE_I 2 "register_operand" "w, w, 0, w, w, w")
			 (match_operand:SVE_I 3 "register_operand" "w, w, w, 0, w, w")))
	   (match_operand:SVE_I 5 "aarch64_simd_reg_or_zero" "Dz, Dz, Dz, Dz, 0, w")]
	  UNSPEC_SEL))]
  "TARGET_SVE
   && !rtx_equal_p (operands[2], operands[5])
   && !rtx_equal_p (operands[3], operands[5])
   && !rtx_equal_p (operands[4], operands[5])"
  "@
   movprfx\t%0.<Vetype>, %1/z, %4.<Vetype>\;mls\t%0.<Vetype>, %1/m, %2.<Vetype>, %3.<Vetype>
   movprfx\t%0.<Vetype>, %1/z, %0.<Vetype>\;mls\t%0.<Vetype>, %1/m, %2.<Vetype>, %3.<Vetype>
   movprfx\t%0.<Vetype>, %1/z, %0.<Vetype>\;msb\t%0.<Vetype>, %1/m, %3.<Vetype>, %4.<Vetype>
   movprfx\t%0.<Vetype>, %1/z, %0.<Vetype>\;msb\t%0.<Vetype>, %1/m, %2.<Vetype>, %4.<Vetype>
   movprfx\t%0.<Vetype>, %1/m, %4.<Vetype>\;mls\t%0.<Vetype>, %1/m, %2.<Vetype>, %3.<Vetype>
   #"
  "&& reload_completed
   && register_operand (operands[5], <MODE>mode)
   && !rtx_equal_p (operands[0], operands[5])"
  {
    emit_insn (gen_vcond_mask_<mode><vpred> (operands[0], operands[4],
					     operands[5], operands[1]));
    operands[5] = operands[4] = operands[0];
  }
  [(set_attr "movprfx" "yes")]
)

;; -------------------------------------------------------------------------
;; ---- [INT] Dot product
;; -------------------------------------------------------------------------
;; Includes:
;; - SDOT
;; - UDOT
;; -------------------------------------------------------------------------

;; Four-element integer dot-product with accumulation.
(define_insn "<sur>dot_prod<vsi2qi>"
  [(set (match_operand:SVE_SDI 0 "register_operand" "=w, ?&w")
	(plus:SVE_SDI
	  (unspec:SVE_SDI
	    [(match_operand:<VSI2QI> 1 "register_operand" "w, w")
	     (match_operand:<VSI2QI> 2 "register_operand" "w, w")]
	    DOTPROD)
	  (match_operand:SVE_SDI 3 "register_operand" "0, w")))]
  "TARGET_SVE"
  "@
   <sur>dot\\t%0.<Vetype>, %1.<Vetype_fourth>, %2.<Vetype_fourth>
   movprfx\t%0, %3\;<sur>dot\\t%0.<Vetype>, %1.<Vetype_fourth>, %2.<Vetype_fourth>"
  [(set_attr "movprfx" "*,yes")]
)

;; -------------------------------------------------------------------------
;; ---- [INT] Sum of absolute differences
;; -------------------------------------------------------------------------
;; The patterns in this section are synthetic.
;; -------------------------------------------------------------------------

;; Emit a sequence to produce a sum-of-absolute-differences of the inputs in
;; operands 1 and 2.  The sequence also has to perform a widening reduction of
;; the difference into a vector and accumulate that into operand 3 before
;; copying that into the result operand 0.
;; Perform that with a sequence of:
;; MOV		ones.b, #1
;; [SU]ABD	diff.b, p0/m, op1.b, op2.b
;; MOVPRFX	op0, op3	// If necessary
;; UDOT		op0.s, diff.b, ones.b
(define_expand "<sur>sad<vsi2qi>"
  [(use (match_operand:SVE_SDI 0 "register_operand"))
   (unspec:<VSI2QI> [(use (match_operand:<VSI2QI> 1 "register_operand"))
		    (use (match_operand:<VSI2QI> 2 "register_operand"))] ABAL)
   (use (match_operand:SVE_SDI 3 "register_operand"))]
  "TARGET_SVE"
  {
    rtx ones = force_reg (<VSI2QI>mode, CONST1_RTX (<VSI2QI>mode));
    rtx diff = gen_reg_rtx (<VSI2QI>mode);
    emit_insn (gen_<sur>abd<vsi2qi>_3 (diff, operands[1], operands[2]));
    emit_insn (gen_udot_prod<vsi2qi> (operands[0], diff, ones, operands[3]));
    DONE;
  }
)

;; -------------------------------------------------------------------------
;; ---- [FP] General ternary arithmetic corresponding to unspecs
;; -------------------------------------------------------------------------
;; Includes merging patterns for:
;; - FMAD
;; - FMLA
;; - FMLS
;; - FMSB
;; - FNMAD
;; - FNMLA
;; - FNMLS
;; - FNMSB
;; -------------------------------------------------------------------------

;; Unpredicated floating-point ternary operations.
(define_expand "<optab><mode>4"
  [(set (match_operand:SVE_F 0 "register_operand")
	(unspec:SVE_F
	  [(match_dup 4)
	   (const_int SVE_RELAXED_GP)
	   (match_operand:SVE_F 1 "register_operand")
	   (match_operand:SVE_F 2 "register_operand")
	   (match_operand:SVE_F 3 "register_operand")]
	  SVE_COND_FP_TERNARY))]
  "TARGET_SVE"
  {
    operands[4] = aarch64_ptrue_reg (<VPRED>mode);
  }
)

;; Predicated floating-point ternary operations.
(define_insn "*<optab><mode>4"
  [(set (match_operand:SVE_F 0 "register_operand" "=w, w, ?&w")
	(unspec:SVE_F
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl, Upl")
	   (match_operand:SI 5 "aarch64_sve_gp_strictness")
	   (match_operand:SVE_F 2 "register_operand" "%w, 0, w")
	   (match_operand:SVE_F 3 "register_operand" "w, w, w")
	   (match_operand:SVE_F 4 "register_operand" "0, w, w")]
	  SVE_COND_FP_TERNARY))]
  "TARGET_SVE"
  "@
   <sve_fmla_op>\t%0.<Vetype>, %1/m, %2.<Vetype>, %3.<Vetype>
   <sve_fmad_op>\t%0.<Vetype>, %1/m, %3.<Vetype>, %4.<Vetype>
   movprfx\t%0, %4\;<sve_fmla_op>\t%0.<Vetype>, %1/m, %2.<Vetype>, %3.<Vetype>"
  [(set_attr "movprfx" "*,*,yes")]
)

;; Predicated floating-point ternary operations with merging.
(define_expand "cond_<optab><mode>"
  [(set (match_operand:SVE_F 0 "register_operand")
	(unspec:SVE_F
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_F
	     [(match_dup 1)
	      (const_int SVE_STRICT_GP)
	      (match_operand:SVE_F 2 "register_operand")
	      (match_operand:SVE_F 3 "register_operand")
	      (match_operand:SVE_F 4 "register_operand")]
	     SVE_COND_FP_TERNARY)
	   (match_operand:SVE_F 5 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE"
{
  /* Swap the multiplication operands if the fallback value is the
     second of the two.  */
  if (rtx_equal_p (operands[3], operands[5]))
    std::swap (operands[2], operands[3]);
})

;; Predicated floating-point ternary operations, merging with the
;; first input.
(define_insn_and_rewrite "*cond_<optab><mode>_2"
  [(set (match_operand:SVE_F 0 "register_operand" "=w, ?&w")
	(unspec:SVE_F
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl")
	   (unspec:SVE_F
	     [(match_operand 5)
	      (match_operand:SI 6 "aarch64_sve_gp_strictness")
	      (match_operand:SVE_F 2 "register_operand" "0, w")
	      (match_operand:SVE_F 3 "register_operand" "w, w")
	      (match_operand:SVE_F 4 "register_operand" "w, w")]
	     SVE_COND_FP_TERNARY)
	   (match_dup 2)]
	  UNSPEC_SEL))]
  "TARGET_SVE && aarch64_sve_pred_dominates_p (&operands[5], operands[1])"
  "@
   <sve_fmad_op>\t%0.<Vetype>, %1/m, %3.<Vetype>, %4.<Vetype>
   movprfx\t%0, %2\;<sve_fmad_op>\t%0.<Vetype>, %1/m, %3.<Vetype>, %4.<Vetype>"
  "&& !rtx_equal_p (operands[1], operands[5])"
  {
    operands[5] = copy_rtx (operands[1]);
  }
  [(set_attr "movprfx" "*,yes")]
)

;; Predicated floating-point ternary operations, merging with the
;; third input.
(define_insn_and_rewrite "*cond_<optab><mode>_4"
  [(set (match_operand:SVE_F 0 "register_operand" "=w, ?&w")
	(unspec:SVE_F
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl")
	   (unspec:SVE_F
	     [(match_operand 5)
	      (match_operand:SI 6 "aarch64_sve_gp_strictness")
	      (match_operand:SVE_F 2 "register_operand" "w, w")
	      (match_operand:SVE_F 3 "register_operand" "w, w")
	      (match_operand:SVE_F 4 "register_operand" "0, w")]
	     SVE_COND_FP_TERNARY)
	   (match_dup 4)]
	  UNSPEC_SEL))]
  "TARGET_SVE && aarch64_sve_pred_dominates_p (&operands[5], operands[1])"
  "@
   <sve_fmla_op>\t%0.<Vetype>, %1/m, %2.<Vetype>, %3.<Vetype>
   movprfx\t%0, %4\;<sve_fmla_op>\t%0.<Vetype>, %1/m, %2.<Vetype>, %3.<Vetype>"
  "&& !rtx_equal_p (operands[1], operands[5])"
  {
    operands[5] = copy_rtx (operands[1]);
  }
  [(set_attr "movprfx" "*,yes")]
)

;; Predicated floating-point ternary operations, merging with an
;; independent value.
(define_insn_and_rewrite "*cond_<optab><mode>_any"
  [(set (match_operand:SVE_F 0 "register_operand" "=&w, &w, &w, &w, &w, ?&w")
	(unspec:SVE_F
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl, Upl, Upl, Upl, Upl")
	   (unspec:SVE_F
	     [(match_operand 6)
	      (match_operand:SI 7 "aarch64_sve_gp_strictness")
	      (match_operand:SVE_F 2 "register_operand" "w, w, 0, w, w, w")
	      (match_operand:SVE_F 3 "register_operand" "w, w, w, 0, w, w")
	      (match_operand:SVE_F 4 "register_operand" "w, 0, w, w, w, w")]
	     SVE_COND_FP_TERNARY)
	   (match_operand:SVE_F 5 "aarch64_simd_reg_or_zero" "Dz, Dz, Dz, Dz, 0, w")]
	  UNSPEC_SEL))]
  "TARGET_SVE
   && !rtx_equal_p (operands[2], operands[5])
   && !rtx_equal_p (operands[3], operands[5])
   && !rtx_equal_p (operands[4], operands[5])
   && aarch64_sve_pred_dominates_p (&operands[6], operands[1])"
  "@
   movprfx\t%0.<Vetype>, %1/z, %4.<Vetype>\;<sve_fmla_op>\t%0.<Vetype>, %1/m, %2.<Vetype>, %3.<Vetype>
   movprfx\t%0.<Vetype>, %1/z, %0.<Vetype>\;<sve_fmla_op>\t%0.<Vetype>, %1/m, %2.<Vetype>, %3.<Vetype>
   movprfx\t%0.<Vetype>, %1/z, %0.<Vetype>\;<sve_fmad_op>\t%0.<Vetype>, %1/m, %3.<Vetype>, %4.<Vetype>
   movprfx\t%0.<Vetype>, %1/z, %0.<Vetype>\;<sve_fmad_op>\t%0.<Vetype>, %1/m, %2.<Vetype>, %4.<Vetype>
   movprfx\t%0.<Vetype>, %1/m, %4.<Vetype>\;<sve_fmla_op>\t%0.<Vetype>, %1/m, %2.<Vetype>, %3.<Vetype>
   #"
  "&& 1"
  {
    if (reload_completed
        && register_operand (operands[5], <MODE>mode)
        && !rtx_equal_p (operands[0], operands[5]))
      {
	emit_insn (gen_vcond_mask_<mode><vpred> (operands[0], operands[4],
						 operands[5], operands[1]));
	operands[5] = operands[4] = operands[0];
      }
    else if (!rtx_equal_p (operands[1], operands[6]))
      operands[6] = copy_rtx (operands[1]);
    else
      FAIL;
  }
  [(set_attr "movprfx" "yes")]
)

;; =========================================================================
;; == Comparisons and selects
;; =========================================================================

;; -------------------------------------------------------------------------
;; ---- [INT,FP] Select based on predicates
;; -------------------------------------------------------------------------
;; Includes merging patterns for:
;; - FMOV
;; - MOV
;; - SEL
;; -------------------------------------------------------------------------

;; vcond_mask operand order: true, false, mask
;; UNSPEC_SEL operand order: mask, true, false (as for VEC_COND_EXPR)
;; SEL operand order:        mask, true, false
(define_expand "@vcond_mask_<mode><vpred>"
  [(set (match_operand:SVE_ALL 0 "register_operand")
	(unspec:SVE_ALL
	  [(match_operand:<VPRED> 3 "register_operand")
	   (match_operand:SVE_ALL 1 "aarch64_sve_reg_or_dup_imm")
	   (match_operand:SVE_ALL 2 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  {
    if (register_operand (operands[1], <MODE>mode))
      operands[2] = force_reg (<MODE>mode, operands[2]);
  }
)

;; Selects between:
;; - two registers
;; - a duplicated immediate and a register
;; - a duplicated immediate and zero
(define_insn "*vcond_mask_<mode><vpred>"
  [(set (match_operand:SVE_ALL 0 "register_operand" "=w, w, w, w, ?w, ?&w, ?&w")
	(unspec:SVE_ALL
	  [(match_operand:<VPRED> 3 "register_operand" "Upa, Upa, Upa, Upa, Upl, Upl, Upl")
	   (match_operand:SVE_ALL 1 "aarch64_sve_reg_or_dup_imm" "w, vss, vss, Ufc, Ufc, vss, Ufc")
	   (match_operand:SVE_ALL 2 "aarch64_simd_reg_or_zero" "w, 0, Dz, 0, Dz, w, w")]
	  UNSPEC_SEL))]
  "TARGET_SVE
   && (!register_operand (operands[1], <MODE>mode)
       || register_operand (operands[2], <MODE>mode))"
  "@
   sel\t%0.<Vetype>, %3, %1.<Vetype>, %2.<Vetype>
   mov\t%0.<Vetype>, %3/m, #%I1
   mov\t%0.<Vetype>, %3/z, #%I1
   fmov\t%0.<Vetype>, %3/m, #%1
   movprfx\t%0.<Vetype>, %3/z, %0.<Vetype>\;fmov\t%0.<Vetype>, %3/m, #%1
   movprfx\t%0, %2\;mov\t%0.<Vetype>, %3/m, #%I1
   movprfx\t%0, %2\;fmov\t%0.<Vetype>, %3/m, #%1"
  [(set_attr "movprfx" "*,*,*,*,yes,yes,yes")]
)

;; Optimize selects between a duplicated scalar variable and another vector,
;; the latter of which can be a zero constant or a variable.  Treat duplicates
;; of GPRs as being more expensive than duplicates of FPRs, since they
;; involve a cross-file move.
(define_insn "*aarch64_sel_dup<mode>"
  [(set (match_operand:SVE_ALL 0 "register_operand" "=?w, w, ??w, ?&w, ??&w, ?&w")
	(unspec:SVE_ALL
	  [(match_operand:<VPRED> 3 "register_operand" "Upa, Upa, Upl, Upl, Upl, Upl")
	   (vec_duplicate:SVE_ALL
	     (match_operand:<VEL> 1 "register_operand" "r, w, r, w, r, w"))
	   (match_operand:SVE_ALL 2 "aarch64_simd_reg_or_zero" "0, 0, Dz, Dz, w, w")]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  "@
   mov\t%0.<Vetype>, %3/m, %<vwcore>1
   mov\t%0.<Vetype>, %3/m, %<Vetype>1
   movprfx\t%0.<Vetype>, %3/z, %0.<Vetype>\;mov\t%0.<Vetype>, %3/m, %<vwcore>1
   movprfx\t%0.<Vetype>, %3/z, %0.<Vetype>\;mov\t%0.<Vetype>, %3/m, %<Vetype>1
   movprfx\t%0, %2\;mov\t%0.<Vetype>, %3/m, %<vwcore>1
   movprfx\t%0, %2\;mov\t%0.<Vetype>, %3/m, %<Vetype>1"
  [(set_attr "movprfx" "*,*,yes,yes,yes,yes")]
)

;; -------------------------------------------------------------------------
;; ---- [INT,FP] Compare and select
;; -------------------------------------------------------------------------
;; The patterns in this section are synthetic.
;; -------------------------------------------------------------------------

;; Integer (signed) vcond.  Don't enforce an immediate range here, since it
;; depends on the comparison; leave it to aarch64_expand_sve_vcond instead.
(define_expand "vcond<mode><v_int_equiv>"
  [(set (match_operand:SVE_ALL 0 "register_operand")
	(if_then_else:SVE_ALL
	  (match_operator 3 "comparison_operator"
	    [(match_operand:<V_INT_EQUIV> 4 "register_operand")
	     (match_operand:<V_INT_EQUIV> 5 "nonmemory_operand")])
	  (match_operand:SVE_ALL 1 "nonmemory_operand")
	  (match_operand:SVE_ALL 2 "nonmemory_operand")))]
  "TARGET_SVE"
  {
    aarch64_expand_sve_vcond (<MODE>mode, <V_INT_EQUIV>mode, operands);
    DONE;
  }
)

;; Integer vcondu.  Don't enforce an immediate range here, since it
;; depends on the comparison; leave it to aarch64_expand_sve_vcond instead.
(define_expand "vcondu<mode><v_int_equiv>"
  [(set (match_operand:SVE_ALL 0 "register_operand")
	(if_then_else:SVE_ALL
	  (match_operator 3 "comparison_operator"
	    [(match_operand:<V_INT_EQUIV> 4 "register_operand")
	     (match_operand:<V_INT_EQUIV> 5 "nonmemory_operand")])
	  (match_operand:SVE_ALL 1 "nonmemory_operand")
	  (match_operand:SVE_ALL 2 "nonmemory_operand")))]
  "TARGET_SVE"
  {
    aarch64_expand_sve_vcond (<MODE>mode, <V_INT_EQUIV>mode, operands);
    DONE;
  }
)

;; Floating-point vcond.  All comparisons except FCMUO allow a zero operand;
;; aarch64_expand_sve_vcond handles the case of an FCMUO with zero.
(define_expand "vcond<mode><v_fp_equiv>"
  [(set (match_operand:SVE_HSD 0 "register_operand")
	(if_then_else:SVE_HSD
	  (match_operator 3 "comparison_operator"
	    [(match_operand:<V_FP_EQUIV> 4 "register_operand")
	     (match_operand:<V_FP_EQUIV> 5 "aarch64_simd_reg_or_zero")])
	  (match_operand:SVE_HSD 1 "nonmemory_operand")
	  (match_operand:SVE_HSD 2 "nonmemory_operand")))]
  "TARGET_SVE"
  {
    aarch64_expand_sve_vcond (<MODE>mode, <V_FP_EQUIV>mode, operands);
    DONE;
  }
)

;; -------------------------------------------------------------------------
;; ---- [INT] Comparisons
;; -------------------------------------------------------------------------
;; Includes merging patterns for:
;; - CMPEQ
;; - CMPGE
;; - CMPGT
;; - CMPHI
;; - CMPHS
;; - CMPLE
;; - CMPLO
;; - CMPLS
;; - CMPLT
;; - CMPNE
;; -------------------------------------------------------------------------

;; Signed integer comparisons.  Don't enforce an immediate range here, since
;; it depends on the comparison; leave it to aarch64_expand_sve_vec_cmp_int
;; instead.
(define_expand "vec_cmp<mode><vpred>"
  [(parallel
    [(set (match_operand:<VPRED> 0 "register_operand")
	  (match_operator:<VPRED> 1 "comparison_operator"
	    [(match_operand:SVE_I 2 "register_operand")
	     (match_operand:SVE_I 3 "nonmemory_operand")]))
     (clobber (reg:CC_NZC CC_REGNUM))])]
  "TARGET_SVE"
  {
    aarch64_expand_sve_vec_cmp_int (operands[0], GET_CODE (operands[1]),
				    operands[2], operands[3]);
    DONE;
  }
)

;; Unsigned integer comparisons.  Don't enforce an immediate range here, since
;; it depends on the comparison; leave it to aarch64_expand_sve_vec_cmp_int
;; instead.
(define_expand "vec_cmpu<mode><vpred>"
  [(parallel
    [(set (match_operand:<VPRED> 0 "register_operand")
	  (match_operator:<VPRED> 1 "comparison_operator"
	    [(match_operand:SVE_I 2 "register_operand")
	     (match_operand:SVE_I 3 "nonmemory_operand")]))
     (clobber (reg:CC_NZC CC_REGNUM))])]
  "TARGET_SVE"
  {
    aarch64_expand_sve_vec_cmp_int (operands[0], GET_CODE (operands[1]),
				    operands[2], operands[3]);
    DONE;
  }
)

;; Predicated integer comparisons.
(define_insn "@aarch64_pred_cmp<cmp_op><mode>"
  [(set (match_operand:<VPRED> 0 "register_operand" "=Upa, Upa")
	(unspec:<VPRED>
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl")
	   (match_operand:SI 2 "aarch64_sve_ptrue_flag")
	   (SVE_INT_CMP:<VPRED>
	     (match_operand:SVE_I 3 "register_operand" "w, w")
	     (match_operand:SVE_I 4 "aarch64_sve_cmp_<sve_imm_con>_operand" "<sve_imm_con>, w"))]
	  UNSPEC_PRED_Z))
   (clobber (reg:CC_NZC CC_REGNUM))]
  "TARGET_SVE"
  "@
   cmp<cmp_op>\t%0.<Vetype>, %1/z, %3.<Vetype>, #%4
   cmp<cmp_op>\t%0.<Vetype>, %1/z, %3.<Vetype>, %4.<Vetype>"
)

;; Predicated integer comparisons in which both the flag and predicate
;; results are interesting.
(define_insn_and_rewrite "*cmp<cmp_op><mode>_cc"
  [(set (reg:CC_NZC CC_REGNUM)
	(unspec:CC_NZC
	  [(match_operand:VNx16BI 1 "register_operand" "Upl, Upl")
	   (match_operand 4)
	   (match_operand:SI 5 "aarch64_sve_ptrue_flag")
	   (unspec:<VPRED>
	     [(match_operand 6)
	      (match_operand:SI 7 "aarch64_sve_ptrue_flag")
	      (SVE_INT_CMP:<VPRED>
		(match_operand:SVE_I 2 "register_operand" "w, w")
		(match_operand:SVE_I 3 "aarch64_sve_cmp_<sve_imm_con>_operand" "<sve_imm_con>, w"))]
	     UNSPEC_PRED_Z)]
	  UNSPEC_PTEST))
   (set (match_operand:<VPRED> 0 "register_operand" "=Upa, Upa")
	(unspec:<VPRED>
	  [(match_dup 6)
	   (match_dup 7)
	   (SVE_INT_CMP:<VPRED>
	     (match_dup 2)
	     (match_dup 3))]
	  UNSPEC_PRED_Z))]
  "TARGET_SVE
   && aarch64_sve_same_pred_for_ptest_p (&operands[4], &operands[6])"
  "@
   cmp<cmp_op>\t%0.<Vetype>, %1/z, %2.<Vetype>, #%3
   cmp<cmp_op>\t%0.<Vetype>, %1/z, %2.<Vetype>, %3.<Vetype>"
  "&& !rtx_equal_p (operands[4], operands[6])"
  {
    operands[6] = copy_rtx (operands[4]);
    operands[7] = operands[5];
  }
)

;; Predicated integer comparisons in which only the flags result is
;; interesting.
(define_insn_and_rewrite "*cmp<cmp_op><mode>_ptest"
  [(set (reg:CC_NZC CC_REGNUM)
	(unspec:CC_NZC
	  [(match_operand:VNx16BI 1 "register_operand" "Upl, Upl")
	   (match_operand 4)
	   (match_operand:SI 5 "aarch64_sve_ptrue_flag")
	   (unspec:<VPRED>
	     [(match_operand 6)
	      (match_operand:SI 7 "aarch64_sve_ptrue_flag")
	      (SVE_INT_CMP:<VPRED>
		(match_operand:SVE_I 2 "register_operand" "w, w")
		(match_operand:SVE_I 3 "aarch64_sve_cmp_<sve_imm_con>_operand" "<sve_imm_con>, w"))]
	     UNSPEC_PRED_Z)]
	  UNSPEC_PTEST))
   (clobber (match_scratch:<VPRED> 0 "=Upa, Upa"))]
  "TARGET_SVE
   && aarch64_sve_same_pred_for_ptest_p (&operands[4], &operands[6])"
  "@
   cmp<cmp_op>\t%0.<Vetype>, %1/z, %2.<Vetype>, #%3
   cmp<cmp_op>\t%0.<Vetype>, %1/z, %2.<Vetype>, %3.<Vetype>"
  "&& !rtx_equal_p (operands[4], operands[6])"
  {
    operands[6] = copy_rtx (operands[4]);
    operands[7] = operands[5];
  }
)

;; Predicated integer comparisons, formed by combining a PTRUE-predicated
;; comparison with an AND.  Split the instruction into its preferred form
;; at the earliest opportunity, in order to get rid of the redundant
;; operand 4.
(define_insn_and_split "*cmp<cmp_op><mode>_and"
  [(set (match_operand:<VPRED> 0 "register_operand" "=Upa, Upa")
	(and:<VPRED>
	  (unspec:<VPRED>
	    [(match_operand 4)
	     (const_int SVE_KNOWN_PTRUE)
	     (SVE_INT_CMP:<VPRED>
	       (match_operand:SVE_I 2 "register_operand" "w, w")
	       (match_operand:SVE_I 3 "aarch64_sve_cmp_<sve_imm_con>_operand" "<sve_imm_con>, w"))]
	    UNSPEC_PRED_Z)
	  (match_operand:<VPRED> 1 "register_operand" "Upl, Upl")))
   (clobber (reg:CC_NZC CC_REGNUM))]
  "TARGET_SVE"
  "#"
  "&& 1"
  [(parallel
     [(set (match_dup 0)
	   (unspec:<VPRED>
	     [(match_dup 1)
	      (const_int SVE_MAYBE_NOT_PTRUE)
	      (SVE_INT_CMP:<VPRED>
		(match_dup 2)
		(match_dup 3))]
	     UNSPEC_PRED_Z))
      (clobber (reg:CC_NZC CC_REGNUM))])]
)

;; -------------------------------------------------------------------------
;; ---- [INT] While tests
;; -------------------------------------------------------------------------
;; Includes:
;; - WHILELO
;; -------------------------------------------------------------------------

;; Set element I of the result if operand1 + J < operand2 for all J in [0, I],
;; with the comparison being unsigned.
(define_insn "@while_ult<GPI:mode><PRED_ALL:mode>"
  [(set (match_operand:PRED_ALL 0 "register_operand" "=Upa")
	(unspec:PRED_ALL [(match_operand:GPI 1 "aarch64_reg_or_zero" "rZ")
			  (match_operand:GPI 2 "aarch64_reg_or_zero" "rZ")]
			 UNSPEC_WHILE_LO))
   (clobber (reg:CC_NZC CC_REGNUM))]
  "TARGET_SVE"
  "whilelo\t%0.<PRED_ALL:Vetype>, %<w>1, %<w>2"
)

;; WHILELO sets the flags in the same way as a PTEST with a PTRUE GP.
;; Handle the case in which both results are useful.  The GP operands
;; to the PTEST aren't needed, so we allow them to be anything.
(define_insn_and_rewrite "*while_ult<GPI:mode><PRED_ALL:mode>_cc"
  [(set (reg:CC_NZC CC_REGNUM)
	(unspec:CC_NZC
	  [(match_operand 3)
	   (match_operand 4)
	   (const_int SVE_KNOWN_PTRUE)
	   (unspec:PRED_ALL
	     [(match_operand:GPI 1 "aarch64_reg_or_zero" "rZ")
	      (match_operand:GPI 2 "aarch64_reg_or_zero" "rZ")]
	     UNSPEC_WHILE_LO)]
	  UNSPEC_PTEST))
   (set (match_operand:PRED_ALL 0 "register_operand" "=Upa")
	(unspec:PRED_ALL [(match_dup 1)
			  (match_dup 2)]
			 UNSPEC_WHILE_LO))]
  "TARGET_SVE"
  "whilelo\t%0.<PRED_ALL:Vetype>, %<w>1, %<w>2"
  ;; Force the compiler to drop the unused predicate operand, so that we
  ;; don't have an unnecessary PTRUE.
  "&& (!CONSTANT_P (operands[3]) || !CONSTANT_P (operands[4]))"
  {
    operands[3] = CONSTM1_RTX (VNx16BImode);
    operands[4] = CONSTM1_RTX (<PRED_ALL:MODE>mode);
  }
)

;; -------------------------------------------------------------------------
;; ---- [FP] Direct comparisons
;; -------------------------------------------------------------------------
;; Includes:
;; - FCMEQ
;; - FCMGE
;; - FCMGT
;; - FCMLE
;; - FCMLT
;; - FCMNE
;; - FCMUO
;; -------------------------------------------------------------------------

;; Floating-point comparisons.  All comparisons except FCMUO allow a zero
;; operand; aarch64_expand_sve_vec_cmp_float handles the case of an FCMUO
;; with zero.
(define_expand "vec_cmp<mode><vpred>"
  [(set (match_operand:<VPRED> 0 "register_operand")
	(match_operator:<VPRED> 1 "comparison_operator"
	  [(match_operand:SVE_F 2 "register_operand")
	   (match_operand:SVE_F 3 "aarch64_simd_reg_or_zero")]))]
  "TARGET_SVE"
  {
    aarch64_expand_sve_vec_cmp_float (operands[0], GET_CODE (operands[1]),
				      operands[2], operands[3], false);
    DONE;
  }
)

;; Predicated floating-point comparisons.
(define_insn "*fcm<cmp_op><mode>"
  [(set (match_operand:<VPRED> 0 "register_operand" "=Upa, Upa")
	(unspec:<VPRED>
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl")
	   (match_operand:SI 4 "aarch64_sve_ptrue_flag")
	   (match_operand:SVE_F 2 "register_operand" "w, w")
	   (match_operand:SVE_F 3 "aarch64_simd_reg_or_zero" "Dz, w")]
	  SVE_COND_FP_CMP_I0))]
  "TARGET_SVE"
  "@
   fcm<cmp_op>\t%0.<Vetype>, %1/z, %2.<Vetype>, #0.0
   fcm<cmp_op>\t%0.<Vetype>, %1/z, %2.<Vetype>, %3.<Vetype>"
)

;; Same for unordered comparisons.
(define_insn "*fcmuo<mode>"
  [(set (match_operand:<VPRED> 0 "register_operand" "=Upa")
	(unspec:<VPRED>
	  [(match_operand:<VPRED> 1 "register_operand" "Upl")
	   (match_operand:SI 4 "aarch64_sve_ptrue_flag")
	   (match_operand:SVE_F 2 "register_operand" "w")
	   (match_operand:SVE_F 3 "register_operand" "w")]
	  UNSPEC_COND_FCMUO))]
  "TARGET_SVE"
  "fcmuo\t%0.<Vetype>, %1/z, %2.<Vetype>, %3.<Vetype>"
)

;; Floating-point comparisons predicated on a PTRUE, with the results ANDed
;; with another predicate P.  This does not have the same trapping behavior
;; as predicating the comparison itself on P, but it's a legitimate fold,
;; since we can drop any potentially-trapping operations whose results
;; are not needed.
;;
;; Split the instruction into its preferred form (below) at the earliest
;; opportunity, in order to get rid of the redundant operand 1.
(define_insn_and_split "*fcm<cmp_op><mode>_and_combine"
  [(set (match_operand:<VPRED> 0 "register_operand" "=Upa, Upa")
	(and:<VPRED>
	  (unspec:<VPRED>
	    [(match_operand:<VPRED> 1)
	     (const_int SVE_KNOWN_PTRUE)
	     (match_operand:SVE_F 2 "register_operand" "w, w")
	     (match_operand:SVE_F 3 "aarch64_simd_reg_or_zero" "Dz, w")]
	    SVE_COND_FP_CMP_I0)
	  (match_operand:<VPRED> 4 "register_operand" "Upl, Upl")))]
  "TARGET_SVE"
  "#"
  "&& 1"
  [(set (match_dup 0)
	(unspec:<VPRED>
	  [(match_dup 4)
	   (const_int SVE_MAYBE_NOT_PTRUE)
	   (match_dup 2)
	   (match_dup 3)]
	  SVE_COND_FP_CMP_I0))]
)

;; Same for unordered comparisons.
(define_insn_and_split "*fcmuo<mode>_and_combine"
  [(set (match_operand:<VPRED> 0 "register_operand" "=Upa")
	(and:<VPRED>
	  (unspec:<VPRED>
	    [(match_operand:<VPRED> 1)
	     (const_int SVE_KNOWN_PTRUE)
	     (match_operand:SVE_F 2 "register_operand" "w")
	     (match_operand:SVE_F 3 "register_operand" "w")]
	    UNSPEC_COND_FCMUO)
	  (match_operand:<VPRED> 4 "register_operand" "Upl")))]
  "TARGET_SVE"
  "#"
  "&& 1"
  [(set (match_dup 0)
	(unspec:<VPRED>
	  [(match_dup 4)
	   (const_int SVE_MAYBE_NOT_PTRUE)
	   (match_dup 2)
	   (match_dup 3)]
	  UNSPEC_COND_FCMUO))]
)

;; -------------------------------------------------------------------------
;; ---- [FP] Absolute comparisons
;; -------------------------------------------------------------------------
;; Includes:
;; - FACGE
;; - FACGT
;; - FACLE
;; - FACLT
;; -------------------------------------------------------------------------

;; Predicated floating-point absolute comparisons.
(define_insn_and_rewrite "*aarch64_pred_fac<cmp_op><mode>"
  [(set (match_operand:<VPRED> 0 "register_operand" "=Upa")
	(unspec:<VPRED>
	  [(match_operand:<VPRED> 1 "register_operand" "Upl")
	   (match_operand:SI 4 "aarch64_sve_ptrue_flag")
	   (unspec:SVE_F
	     [(match_operand 5)
	      (match_operand:SI 6 "aarch64_sve_gp_strictness")
	      (match_operand:SVE_F 2 "register_operand" "w")]
	     UNSPEC_COND_FABS)
	   (unspec:SVE_F
	     [(match_operand 7)
	      (match_operand:SI 8 "aarch64_sve_gp_strictness")
	      (match_operand:SVE_F 3 "register_operand" "w")]
	     UNSPEC_COND_FABS)]
	  SVE_COND_FP_ABS_CMP))]
  "TARGET_SVE
   && aarch64_sve_pred_dominates_p (&operands[5], operands[1])
   && aarch64_sve_pred_dominates_p (&operands[7], operands[1])"
  "fac<cmp_op>\t%0.<Vetype>, %1/z, %2.<Vetype>, %3.<Vetype>"
  "&& (!rtx_equal_p (operands[1], operands[5])
       || !rtx_equal_p (operands[1], operands[7]))"
  {
    operands[5] = copy_rtx (operands[1]);
    operands[7] = copy_rtx (operands[1]);
  }
)

;; -------------------------------------------------------------------------
;; ---- [PRED] Test bits
;; -------------------------------------------------------------------------
;; Includes:
;; - PTEST
;; -------------------------------------------------------------------------

;; Branch based on predicate equality or inequality.
(define_expand "cbranch<mode>4"
  [(set (pc)
	(if_then_else
	  (match_operator 0 "aarch64_equality_operator"
	    [(match_operand:PRED_ALL 1 "register_operand")
	     (match_operand:PRED_ALL 2 "aarch64_simd_reg_or_zero")])
	  (label_ref (match_operand 3 ""))
	  (pc)))]
  ""
  {
    rtx ptrue = force_reg (VNx16BImode, aarch64_ptrue_all (<data_bytes>));
    rtx cast_ptrue = gen_lowpart (<MODE>mode, ptrue);
    rtx ptrue_flag = gen_int_mode (SVE_KNOWN_PTRUE, SImode);
    rtx pred;
    if (operands[2] == CONST0_RTX (<MODE>mode))
      pred = operands[1];
    else
      {
	pred = gen_reg_rtx (<MODE>mode);
	emit_insn (gen_aarch64_pred_xor<mode>_z (pred, cast_ptrue, operands[1],
						 operands[2]));
      }
    emit_insn (gen_aarch64_ptest<mode> (ptrue, cast_ptrue, ptrue_flag, pred));
    operands[1] = gen_rtx_REG (CC_NZCmode, CC_REGNUM);
    operands[2] = const0_rtx;
  }
)

;; See "Description of UNSPEC_PTEST" above for details.
(define_insn "aarch64_ptest<mode>"
  [(set (reg:CC_NZC CC_REGNUM)
	(unspec:CC_NZC [(match_operand:VNx16BI 0 "register_operand" "Upa")
			(match_operand 1)
			(match_operand:SI 2 "aarch64_sve_ptrue_flag")
			(match_operand:PRED_ALL 3 "register_operand" "Upa")]
		       UNSPEC_PTEST))]
  "TARGET_SVE"
  "ptest\t%0, %3.b"
)

;; =========================================================================
;; == Reductions
;; =========================================================================

;; -------------------------------------------------------------------------
;; ---- [INT,FP] Conditional reductions
;; -------------------------------------------------------------------------
;; Includes:
;; - CLASTB
;; -------------------------------------------------------------------------

;; Set operand 0 to the last active element in operand 3, or to tied
;; operand 1 if no elements are active.
(define_insn "fold_extract_last_<mode>"
  [(set (match_operand:<VEL> 0 "register_operand" "=?r, w")
	(unspec:<VEL>
	  [(match_operand:<VEL> 1 "register_operand" "0, 0")
	   (match_operand:<VPRED> 2 "register_operand" "Upl, Upl")
	   (match_operand:SVE_ALL 3 "register_operand" "w, w")]
	  UNSPEC_CLASTB))]
  "TARGET_SVE"
  "@
   clastb\t%<vwcore>0, %2, %<vwcore>0, %3.<Vetype>
   clastb\t%<Vetype>0, %2, %<Vetype>0, %3.<Vetype>"
)

;; -------------------------------------------------------------------------
;; ---- [INT] Tree reductions
;; -------------------------------------------------------------------------
;; Includes:
;; - ANDV
;; - EORV
;; - ORV
;; - SMAXV
;; - SMINV
;; - UADDV
;; - UMAXV
;; - UMINV
;; -------------------------------------------------------------------------

;; Unpredicated integer add reduction.
(define_expand "reduc_plus_scal_<mode>"
  [(set (match_operand:<VEL> 0 "register_operand")
	(unspec:<VEL> [(match_dup 2)
		       (match_operand:SVE_I 1 "register_operand")]
		      UNSPEC_ADDV))]
  "TARGET_SVE"
  {
    operands[2] = aarch64_ptrue_reg (<VPRED>mode);
  }
)

;; Predicated integer add reduction.  The result is always 64-bits.
(define_insn "*reduc_plus_scal_<mode>"
  [(set (match_operand:<VEL> 0 "register_operand" "=w")
	(unspec:<VEL> [(match_operand:<VPRED> 1 "register_operand" "Upl")
		       (match_operand:SVE_I 2 "register_operand" "w")]
		      UNSPEC_ADDV))]
  "TARGET_SVE"
  "uaddv\t%d0, %1, %2.<Vetype>"
)

;; Unpredicated integer reductions.
(define_expand "reduc_<optab>_scal_<mode>"
  [(set (match_operand:<VEL> 0 "register_operand")
	(unspec:<VEL> [(match_dup 2)
		       (match_operand:SVE_I 1 "register_operand")]
		      SVE_INT_REDUCTION))]
  "TARGET_SVE"
  {
    operands[2] = aarch64_ptrue_reg (<VPRED>mode);
  }
)

;; Predicated integer reductions.
(define_insn "*reduc_<optab>_scal_<mode>"
  [(set (match_operand:<VEL> 0 "register_operand" "=w")
	(unspec:<VEL> [(match_operand:<VPRED> 1 "register_operand" "Upl")
		       (match_operand:SVE_I 2 "register_operand" "w")]
		      SVE_INT_REDUCTION))]
  "TARGET_SVE"
  "<sve_int_op>\t%<Vetype>0, %1, %2.<Vetype>"
)

;; -------------------------------------------------------------------------
;; ---- [FP] Tree reductions
;; -------------------------------------------------------------------------
;; Includes:
;; - FADDV
;; - FMAXNMV
;; - FMAXV
;; - FMINNMV
;; - FMINV
;; -------------------------------------------------------------------------

;; Unpredicated floating-point tree reductions.
(define_expand "reduc_<optab>_scal_<mode>"
  [(set (match_operand:<VEL> 0 "register_operand")
	(unspec:<VEL> [(match_dup 2)
		       (match_operand:SVE_F 1 "register_operand")]
		      SVE_FP_REDUCTION))]
  "TARGET_SVE"
  {
    operands[2] = aarch64_ptrue_reg (<VPRED>mode);
  }
)

;; Predicated floating-point tree reductions.
(define_insn "*reduc_<optab>_scal_<mode>"
  [(set (match_operand:<VEL> 0 "register_operand" "=w")
	(unspec:<VEL> [(match_operand:<VPRED> 1 "register_operand" "Upl")
		       (match_operand:SVE_F 2 "register_operand" "w")]
		      SVE_FP_REDUCTION))]
  "TARGET_SVE"
  "<sve_fp_op>\t%<Vetype>0, %1, %2.<Vetype>"
)

;; -------------------------------------------------------------------------
;; ---- [FP] Left-to-right reductions
;; -------------------------------------------------------------------------
;; Includes:
;; - FADDA
;; -------------------------------------------------------------------------

;; Unpredicated in-order FP reductions.
(define_expand "fold_left_plus_<mode>"
  [(set (match_operand:<VEL> 0 "register_operand")
	(unspec:<VEL> [(match_dup 3)
		       (match_operand:<VEL> 1 "register_operand")
		       (match_operand:SVE_F 2 "register_operand")]
		      UNSPEC_FADDA))]
  "TARGET_SVE"
  {
    operands[3] = aarch64_ptrue_reg (<VPRED>mode);
  }
)

;; Predicated in-order FP reductions.
(define_insn "mask_fold_left_plus_<mode>"
  [(set (match_operand:<VEL> 0 "register_operand" "=w")
	(unspec:<VEL> [(match_operand:<VPRED> 3 "register_operand" "Upl")
		       (match_operand:<VEL> 1 "register_operand" "0")
		       (match_operand:SVE_F 2 "register_operand" "w")]
		      UNSPEC_FADDA))]
  "TARGET_SVE"
  "fadda\t%<Vetype>0, %3, %<Vetype>0, %2.<Vetype>"
)

;; =========================================================================
;; == Permutes
;; =========================================================================

;; -------------------------------------------------------------------------
;; ---- [INT,FP] General permutes
;; -------------------------------------------------------------------------
;; Includes:
;; - TBL
;; -------------------------------------------------------------------------

(define_expand "vec_perm<mode>"
  [(match_operand:SVE_ALL 0 "register_operand")
   (match_operand:SVE_ALL 1 "register_operand")
   (match_operand:SVE_ALL 2 "register_operand")
   (match_operand:<V_INT_EQUIV> 3 "aarch64_sve_vec_perm_operand")]
  "TARGET_SVE && GET_MODE_NUNITS (<MODE>mode).is_constant ()"
  {
    aarch64_expand_sve_vec_perm (operands[0], operands[1],
				 operands[2], operands[3]);
    DONE;
  }
)

(define_insn "*aarch64_sve_tbl<mode>"
  [(set (match_operand:SVE_ALL 0 "register_operand" "=w")
	(unspec:SVE_ALL
	  [(match_operand:SVE_ALL 1 "register_operand" "w")
	   (match_operand:<V_INT_EQUIV> 2 "register_operand" "w")]
	  UNSPEC_TBL))]
  "TARGET_SVE"
  "tbl\t%0.<Vetype>, %1.<Vetype>, %2.<Vetype>"
)

;; -------------------------------------------------------------------------
;; ---- [INT,FP] Special-purpose unary permutes
;; -------------------------------------------------------------------------
;; Includes:
;; - DUP
;; - REV
;; -------------------------------------------------------------------------

;; Duplicate one element of a vector.
(define_insn "*aarch64_sve_dup_lane<mode>"
  [(set (match_operand:SVE_ALL 0 "register_operand" "=w")
	(vec_duplicate:SVE_ALL
	  (vec_select:<VEL>
	    (match_operand:SVE_ALL 1 "register_operand" "w")
	    (parallel [(match_operand:SI 2 "const_int_operand")]))))]
  "TARGET_SVE
   && IN_RANGE (INTVAL (operands[2]) * GET_MODE_SIZE (<VEL>mode), 0, 63)"
  "dup\t%0.<Vetype>, %1.<Vetype>[%2]"
)

;; Reverse the order of elements within a full vector.
(define_insn "@aarch64_sve_rev<mode>"
  [(set (match_operand:SVE_ALL 0 "register_operand" "=w")
	(unspec:SVE_ALL [(match_operand:SVE_ALL 1 "register_operand" "w")]
			UNSPEC_REV))]
  "TARGET_SVE"
  "rev\t%0.<Vetype>, %1.<Vetype>")

;; -------------------------------------------------------------------------
;; ---- [INT,FP] Special-purpose binary permutes
;; -------------------------------------------------------------------------
;; Includes:
;; - TRN1
;; - TRN2
;; - UZP1
;; - UZP2
;; - ZIP1
;; - ZIP2
;; -------------------------------------------------------------------------

;; Permutes that take half the elements from one vector and half the
;; elements from the other.
(define_insn "aarch64_sve_<perm_insn><mode>"
  [(set (match_operand:SVE_ALL 0 "register_operand" "=w")
	(unspec:SVE_ALL [(match_operand:SVE_ALL 1 "register_operand" "w")
			 (match_operand:SVE_ALL 2 "register_operand" "w")]
			PERMUTE))]
  "TARGET_SVE"
  "<perm_insn>\t%0.<Vetype>, %1.<Vetype>, %2.<Vetype>"
)

;; Concatenate two vectors and extract a subvector.  Note that the
;; immediate (third) operand is the lane index not the byte index.
(define_insn "*aarch64_sve_ext<mode>"
  [(set (match_operand:SVE_ALL 0 "register_operand" "=w, ?&w")
	(unspec:SVE_ALL [(match_operand:SVE_ALL 1 "register_operand" "0, w")
			 (match_operand:SVE_ALL 2 "register_operand" "w, w")
			 (match_operand:SI 3 "const_int_operand")]
			UNSPEC_EXT))]
  "TARGET_SVE
   && IN_RANGE (INTVAL (operands[3]) * GET_MODE_SIZE (<VEL>mode), 0, 255)"
  {
    operands[3] = GEN_INT (INTVAL (operands[3]) * GET_MODE_SIZE (<VEL>mode));
    return (which_alternative == 0
	    ? "ext\\t%0.b, %0.b, %2.b, #%3"
	    : "movprfx\t%0, %1\;ext\\t%0.b, %0.b, %2.b, #%3");
  }
  [(set_attr "movprfx" "*,yes")]
)

;; -------------------------------------------------------------------------
;; ---- [PRED] Special-purpose unary permutes
;; -------------------------------------------------------------------------
;; Includes:
;; - REV
;; -------------------------------------------------------------------------

(define_insn "@aarch64_sve_rev<mode>"
  [(set (match_operand:PRED_ALL 0 "register_operand" "=Upa")
	(unspec:PRED_ALL [(match_operand:PRED_ALL 1 "register_operand" "Upa")]
			 UNSPEC_REV))]
  "TARGET_SVE"
  "rev\t%0.<Vetype>, %1.<Vetype>")

;; -------------------------------------------------------------------------
;; ---- [PRED] Special-purpose binary permutes
;; -------------------------------------------------------------------------
;; Includes:
;; - TRN1
;; - TRN2
;; - UZP1
;; - UZP2
;; - ZIP1
;; - ZIP2
;; -------------------------------------------------------------------------

;; Permutes that take half the elements from one vector and half the
;; elements from the other.
(define_insn "@aarch64_sve_<perm_insn><mode>"
  [(set (match_operand:PRED_ALL 0 "register_operand" "=Upa")
	(unspec:PRED_ALL [(match_operand:PRED_ALL 1 "register_operand" "Upa")
			  (match_operand:PRED_ALL 2 "register_operand" "Upa")]
			 PERMUTE))]
  "TARGET_SVE"
  "<perm_insn>\t%0.<Vetype>, %1.<Vetype>, %2.<Vetype>"
)

;; =========================================================================
;; == Conversions
;; =========================================================================

;; -------------------------------------------------------------------------
;; ---- [INT<-INT] Packs
;; -------------------------------------------------------------------------
;; Includes:
;; - UZP1
;; -------------------------------------------------------------------------

;; Integer pack.  Use UZP1 on the narrower type, which discards
;; the high part of each wide element.
(define_insn "vec_pack_trunc_<Vwide>"
  [(set (match_operand:SVE_BHSI 0 "register_operand" "=w")
	(unspec:SVE_BHSI
	  [(match_operand:<VWIDE> 1 "register_operand" "w")
	   (match_operand:<VWIDE> 2 "register_operand" "w")]
	  UNSPEC_PACK))]
  "TARGET_SVE"
  "uzp1\t%0.<Vetype>, %1.<Vetype>, %2.<Vetype>"
)

;; -------------------------------------------------------------------------
;; ---- [INT<-INT] Unpacks
;; -------------------------------------------------------------------------
;; Includes:
;; - SUNPKHI
;; - SUNPKLO
;; - UUNPKHI
;; - UUNPKLO
;; -------------------------------------------------------------------------

;; Unpack the low or high half of a vector, where "high" refers to
;; the low-numbered lanes for big-endian and the high-numbered lanes
;; for little-endian.
(define_expand "vec_unpack<su>_<perm_hilo>_<SVE_BHSI:mode>"
  [(match_operand:<VWIDE> 0 "register_operand")
   (unspec:<VWIDE> [(match_operand:SVE_BHSI 1 "register_operand")] UNPACK)]
  "TARGET_SVE"
  {
    emit_insn ((<hi_lanes_optab>
		? gen_aarch64_sve_<su>unpkhi_<SVE_BHSI:mode>
		: gen_aarch64_sve_<su>unpklo_<SVE_BHSI:mode>)
	       (operands[0], operands[1]));
    DONE;
  }
)

(define_insn "aarch64_sve_<su>unpk<perm_hilo>_<SVE_BHSI:mode>"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
	(unspec:<VWIDE> [(match_operand:SVE_BHSI 1 "register_operand" "w")]
			UNPACK))]
  "TARGET_SVE"
  "<su>unpk<perm_hilo>\t%0.<Vewtype>, %1.<Vetype>"
)

;; -------------------------------------------------------------------------
;; ---- [INT<-FP] Conversions
;; -------------------------------------------------------------------------
;; Includes:
;; - FCVTZS
;; - FCVTZU
;; -------------------------------------------------------------------------

;; Unpredicated conversion of floats to integers of the same size (HF to HI,
;; SF to SI or DF to DI).
(define_expand "<optab><mode><v_int_equiv>2"
  [(set (match_operand:<V_INT_EQUIV> 0 "register_operand")
	(unspec:<V_INT_EQUIV>
	  [(match_dup 2)
	   (const_int SVE_RELAXED_GP)
	   (match_operand:SVE_F 1 "register_operand")]
	  SVE_COND_FCVTI))]
  "TARGET_SVE"
  {
    operands[2] = aarch64_ptrue_reg (<VPRED>mode);
  }
)

;; Predicated float-to-integer conversion, either to the same width or wider.
(define_insn "*aarch64_sve_<optab>_nontrunc<SVE_F:mode><SVE_HSDI:mode>"
  [(set (match_operand:SVE_HSDI 0 "register_operand" "=w")
	(unspec:SVE_HSDI
	  [(match_operand:<SVE_HSDI:VPRED> 1 "register_operand" "Upl")
	   (match_operand:SI 3 "aarch64_sve_gp_strictness")
	   (match_operand:SVE_F 2 "register_operand" "w")]
	  SVE_COND_FCVTI))]
  "TARGET_SVE && <SVE_HSDI:elem_bits> >= <SVE_F:elem_bits>"
  "fcvtz<su>\t%0.<SVE_HSDI:Vetype>, %1/m, %2.<SVE_F:Vetype>"
)

;; Predicated narrowing float-to-integer conversion.
(define_insn "*aarch64_sve_<optab>_trunc<VNx2DF_ONLY:mode><VNx4SI_ONLY:mode>"
  [(set (match_operand:VNx4SI_ONLY 0 "register_operand" "=w")
	(unspec:VNx4SI_ONLY
	  [(match_operand:VNx2BI 1 "register_operand" "Upl")
	   (match_operand:SI 3 "aarch64_sve_gp_strictness")
	   (match_operand:VNx2DF_ONLY 2 "register_operand" "w")]
	  SVE_COND_FCVTI))]
  "TARGET_SVE"
  "fcvtz<su>\t%0.<VNx4SI_ONLY:Vetype>, %1/m, %2.<VNx2DF_ONLY:Vetype>"
)

;; Predicated float-to-integer conversion with merging, either to the same
;; width or wider.
;;
;; The first alternative doesn't need the earlyclobber, but the only case
;; it would help is the uninteresting one in which operands 2 and 3 are
;; the same register (despite having different modes).  Making all the
;; alternatives earlyclobber makes things more consistent for the
;; register allocator.
(define_insn_and_rewrite "*cond_<optab>_nontrunc<SVE_F:mode><SVE_HSDI:mode>"
  [(set (match_operand:SVE_HSDI 0 "register_operand" "=&w, &w, ?&w")
	(unspec:SVE_HSDI
	  [(match_operand:<SVE_HSDI:VPRED> 1 "register_operand" "Upl, Upl, Upl")
	   (unspec:SVE_HSDI
	     [(match_operand 4)
	      (match_operand:SI 5 "aarch64_sve_gp_strictness")
	      (match_operand:SVE_F 2 "register_operand" "w, w, w")]
	     SVE_COND_FCVTI)
	   (match_operand:SVE_HSDI 3 "aarch64_simd_reg_or_zero" "0, Dz, w")]
	  UNSPEC_SEL))]
  "TARGET_SVE
   && <SVE_HSDI:elem_bits> >= <SVE_F:elem_bits>
   && aarch64_sve_pred_dominates_p (&operands[4], operands[1])"
  "@
   fcvtz<su>\t%0.<SVE_HSDI:Vetype>, %1/m, %2.<SVE_F:Vetype>
   movprfx\t%0.<SVE_HSDI:Vetype>, %1/z, %2.<SVE_HSDI:Vetype>\;fcvtz<su>\t%0.<SVE_HSDI:Vetype>, %1/m, %2.<SVE_F:Vetype>
   movprfx\t%0, %3\;fcvtz<su>\t%0.<SVE_HSDI:Vetype>, %1/m, %2.<SVE_F:Vetype>"
  "&& !rtx_equal_p (operands[1], operands[4])"
  {
    operands[4] = copy_rtx (operands[1]);
  }
  [(set_attr "movprfx" "*,yes,yes")]
)

;; -------------------------------------------------------------------------
;; ---- [INT<-FP] Packs
;; -------------------------------------------------------------------------
;; The patterns in this section are synthetic.
;; -------------------------------------------------------------------------

;; Convert two vectors of DF to SI and pack the results into a single vector.
(define_expand "vec_pack_<su>fix_trunc_vnx2df"
  [(set (match_dup 4)
	(unspec:VNx4SI
	  [(match_dup 3)
	   (const_int SVE_RELAXED_GP)
	   (match_operand:VNx2DF 1 "register_operand")]
	  SVE_COND_FCVTI))
   (set (match_dup 5)
	(unspec:VNx4SI
	  [(match_dup 3)
	   (const_int SVE_RELAXED_GP)
	   (match_operand:VNx2DF 2 "register_operand")]
	  SVE_COND_FCVTI))
   (set (match_operand:VNx4SI 0 "register_operand")
	(unspec:VNx4SI [(match_dup 4) (match_dup 5)] UNSPEC_UZP1))]
  "TARGET_SVE"
  {
    operands[3] = aarch64_ptrue_reg (VNx2BImode);
    operands[4] = gen_reg_rtx (VNx4SImode);
    operands[5] = gen_reg_rtx (VNx4SImode);
  }
)

;; -------------------------------------------------------------------------
;; ---- [INT<-FP] Unpacks
;; -------------------------------------------------------------------------
;; No patterns here yet!
;; -------------------------------------------------------------------------

;; -------------------------------------------------------------------------
;; ---- [FP<-INT] Conversions
;; -------------------------------------------------------------------------
;; Includes:
;; - SCVTF
;; - UCVTF
;; -------------------------------------------------------------------------

;; Unpredicated conversion of integers to floats of the same size
;; (HI to HF, SI to SF or DI to DF).
(define_expand "<optab><v_int_equiv><mode>2"
  [(set (match_operand:SVE_F 0 "register_operand")
	(unspec:SVE_F
	  [(match_dup 2)
	   (const_int SVE_RELAXED_GP)
	   (match_operand:<V_INT_EQUIV> 1 "register_operand")]
	  SVE_COND_ICVTF))]
  "TARGET_SVE"
  {
    operands[2] = aarch64_ptrue_reg (<VPRED>mode);
  }
)

;; Predicated integer-to-float conversion, either to the same width or
;; narrower.
(define_insn "*aarch64_sve_<optab>_nonextend<SVE_HSDI:mode><SVE_F:mode>"
  [(set (match_operand:SVE_F 0 "register_operand" "=w")
	(unspec:SVE_F
	  [(match_operand:<SVE_HSDI:VPRED> 1 "register_operand" "Upl")
	   (match_operand:SI 3 "aarch64_sve_gp_strictness")
	   (match_operand:SVE_HSDI 2 "register_operand" "w")]
	  SVE_COND_ICVTF))]
  "TARGET_SVE && <SVE_HSDI:elem_bits> >= <SVE_F:elem_bits>"
  "<su>cvtf\t%0.<SVE_F:Vetype>, %1/m, %2.<SVE_HSDI:Vetype>"
)

;; Predicated widening integer-to-float conversion.
(define_insn "aarch64_sve_<optab>_extend<VNx4SI_ONLY:mode><VNx2DF_ONLY:mode>"
  [(set (match_operand:VNx2DF_ONLY 0 "register_operand" "=w")
	(unspec:VNx2DF_ONLY
	  [(match_operand:VNx2BI 1 "register_operand" "Upl")
	   (match_operand:SI 3 "aarch64_sve_gp_strictness")
	   (match_operand:VNx4SI_ONLY 2 "register_operand" "w")]
	  SVE_COND_ICVTF))]
  "TARGET_SVE"
  "<su>cvtf\t%0.<VNx2DF_ONLY:Vetype>, %1/m, %2.<VNx4SI_ONLY:Vetype>"
)

;; Predicated integer-to-float conversion with merging, either to the same
;; width or narrower.
;;
;; The first alternative doesn't need the earlyclobber, but the only case
;; it would help is the uninteresting one in which operands 2 and 3 are
;; the same register (despite having different modes).  Making all the
;; alternatives earlyclobber makes things more consistent for the
;; register allocator.
(define_insn_and_rewrite "*cond_<optab>_nonextend<SVE_HSDI:mode><SVE_F:mode>"
  [(set (match_operand:SVE_F 0 "register_operand" "=&w, &w, ?&w")
	(unspec:SVE_F
	  [(match_operand:<SVE_HSDI:VPRED> 1 "register_operand" "Upl, Upl, Upl")
	   (unspec:SVE_F
	     [(match_operand 4)
	      (match_operand:SI 5 "aarch64_sve_gp_strictness")
	      (match_operand:SVE_HSDI 2 "register_operand" "w, w, w")]
	     SVE_COND_ICVTF)
	   (match_operand:SVE_F 3 "aarch64_simd_reg_or_zero" "0, Dz, w")]
	  UNSPEC_SEL))]
  "TARGET_SVE
   && <SVE_HSDI:elem_bits> >= <SVE_F:elem_bits>
   && aarch64_sve_pred_dominates_p (&operands[4], operands[1])"
  "@
   <su>cvtf\t%0.<SVE_F:Vetype>, %1/m, %2.<SVE_HSDI:Vetype>
   movprfx\t%0.<SVE_HSDI:Vetype>, %1/z, %2.<SVE_HSDI:Vetype>\;<su>cvtf\t%0.<SVE_F:Vetype>, %1/m, %2.<SVE_HSDI:Vetype>
   movprfx\t%0, %3\;<su>cvtf\t%0.<SVE_F:Vetype>, %1/m, %2.<SVE_HSDI:Vetype>"
  "&& !rtx_equal_p (operands[1], operands[4])"
  {
    operands[4] = copy_rtx (operands[1]);
  }
  [(set_attr "movprfx" "*,yes,yes")]
)

;; -------------------------------------------------------------------------
;; ---- [FP<-INT] Packs
;; -------------------------------------------------------------------------
;; No patterns here yet!
;; -------------------------------------------------------------------------

;; -------------------------------------------------------------------------
;; ---- [FP<-INT] Unpacks
;; -------------------------------------------------------------------------
;; The patterns in this section are synthetic.
;; -------------------------------------------------------------------------

;; Unpack one half of a VNx4SI to VNx2DF.  First unpack from VNx4SI
;; to VNx2DI, reinterpret the VNx2DI as a VNx4SI, then convert the
;; unpacked VNx4SI to VNx2DF.
(define_expand "vec_unpack<su_optab>_float_<perm_hilo>_vnx4si"
  [(match_operand:VNx2DF 0 "register_operand")
   (FLOATUORS:VNx2DF
     (unspec:VNx2DI [(match_operand:VNx4SI 1 "register_operand")]
		    UNPACK_UNSIGNED))]
  "TARGET_SVE"
  {
    /* Use ZIP to do the unpack, since we don't care about the upper halves
       and since it has the nice property of not needing any subregs.
       If using UUNPK* turns out to be preferable, we could model it as
       a ZIP whose first operand is zero.  */
    rtx temp = gen_reg_rtx (VNx4SImode);
    emit_insn ((<hi_lanes_optab>
		? gen_aarch64_sve_zip2vnx4si
		: gen_aarch64_sve_zip1vnx4si)
	       (temp, operands[1], operands[1]));
    rtx ptrue = aarch64_ptrue_reg (VNx2BImode);
    rtx strictness = gen_int_mode (SVE_RELAXED_GP, SImode);
    emit_insn (gen_aarch64_sve_<FLOATUORS:optab>_extendvnx4sivnx2df
	       (operands[0], ptrue, temp, strictness));
    DONE;
  }
)

;; -------------------------------------------------------------------------
;; ---- [FP<-FP] Packs
;; -------------------------------------------------------------------------
;; Includes:
;; - FCVT
;; -------------------------------------------------------------------------

;; Convert two vectors of DF to SF, or two vectors of SF to HF, and pack
;; the results into a single vector.
(define_expand "vec_pack_trunc_<Vwide>"
  [(set (match_dup 4)
	(unspec:SVE_HSF
	  [(match_dup 3)
	   (const_int SVE_RELAXED_GP)
	   (match_operand:<VWIDE> 1 "register_operand")]
	  UNSPEC_COND_FCVT))
   (set (match_dup 5)
	(unspec:SVE_HSF
	  [(match_dup 3)
	   (const_int SVE_RELAXED_GP)
	   (match_operand:<VWIDE> 2 "register_operand")]
	  UNSPEC_COND_FCVT))
   (set (match_operand:SVE_HSF 0 "register_operand")
	(unspec:SVE_HSF [(match_dup 4) (match_dup 5)] UNSPEC_UZP1))]
  "TARGET_SVE"
  {
    operands[3] = aarch64_ptrue_reg (<VWIDE_PRED>mode);
    operands[4] = gen_reg_rtx (<MODE>mode);
    operands[5] = gen_reg_rtx (<MODE>mode);
  }
)

;; Predicated float-to-float truncation.
(define_insn "*aarch64_sve_<optab>_trunc<SVE_SDF:mode><SVE_HSF:mode>"
  [(set (match_operand:SVE_HSF 0 "register_operand" "=w")
	(unspec:SVE_HSF
	  [(match_operand:<SVE_SDF:VPRED> 1 "register_operand" "Upl")
	   (match_operand:SI 3 "aarch64_sve_gp_strictness")
	   (match_operand:SVE_SDF 2 "register_operand" "w")]
	  SVE_COND_FCVT))]
  "TARGET_SVE && <SVE_SDF:elem_bits> > <SVE_HSF:elem_bits>"
  "fcvt\t%0.<SVE_HSF:Vetype>, %1/m, %2.<SVE_SDF:Vetype>"
)

;; -------------------------------------------------------------------------
;; ---- [FP<-FP] Unpacks
;; -------------------------------------------------------------------------
;; Includes:
;; - FCVT
;; -------------------------------------------------------------------------

;; Unpack one half of a VNx4SF to VNx2DF, or one half of a VNx8HF to VNx4SF.
;; First unpack the source without conversion, then float-convert the
;; unpacked source.
(define_expand "vec_unpacks_<perm_hilo>_<mode>"
  [(match_operand:<VWIDE> 0 "register_operand")
   (unspec:SVE_HSF [(match_operand:SVE_HSF 1 "register_operand")]
		   UNPACK_UNSIGNED)]
  "TARGET_SVE"
  {
    /* Use ZIP to do the unpack, since we don't care about the upper halves
       and since it has the nice property of not needing any subregs.
       If using UUNPK* turns out to be preferable, we could model it as
       a ZIP whose first operand is zero.  */
    rtx temp = gen_reg_rtx (<MODE>mode);
    emit_insn ((<hi_lanes_optab>
		? gen_aarch64_sve_zip2<mode>
		: gen_aarch64_sve_zip1<mode>)
		(temp, operands[1], operands[1]));
    rtx ptrue = aarch64_ptrue_reg (<VWIDE_PRED>mode);
    rtx strictness = gen_int_mode (SVE_RELAXED_GP, SImode);
    emit_insn (gen_aarch64_sve_fcvt_nontrunc<mode><Vwide>
	       (operands[0], ptrue, temp, strictness));
    DONE;
  }
)

;; Predicated float-to-float extension.
(define_insn "aarch64_sve_<optab>_nontrunc<SVE_HSF:mode><SVE_SDF:mode>"
  [(set (match_operand:SVE_SDF 0 "register_operand" "=w")
	(unspec:SVE_SDF
	  [(match_operand:<SVE_SDF:VPRED> 1 "register_operand" "Upl")
	   (match_operand:SI 3 "aarch64_sve_gp_strictness")
	   (match_operand:SVE_HSF 2 "register_operand" "w")]
	  SVE_COND_FCVT))]
  "TARGET_SVE && <SVE_SDF:elem_bits> > <SVE_HSF:elem_bits>"
  "fcvt\t%0.<SVE_SDF:Vetype>, %1/m, %2.<SVE_HSF:Vetype>"
)

;; -------------------------------------------------------------------------
;; ---- [PRED<-PRED] Packs
;; -------------------------------------------------------------------------
;; Includes:
;; - UZP1
;; -------------------------------------------------------------------------

;; Predicate pack.  Use UZP1 on the narrower type, which discards
;; the high part of each wide element.
(define_insn "vec_pack_trunc_<Vwide>"
  [(set (match_operand:PRED_BHS 0 "register_operand" "=Upa")
	(unspec:PRED_BHS
	  [(match_operand:<VWIDE> 1 "register_operand" "Upa")
	   (match_operand:<VWIDE> 2 "register_operand" "Upa")]
	  UNSPEC_PACK))]
  "TARGET_SVE"
  "uzp1\t%0.<Vetype>, %1.<Vetype>, %2.<Vetype>"
)

;; -------------------------------------------------------------------------
;; ---- [PRED<-PRED] Unpacks
;; -------------------------------------------------------------------------
;; Includes:
;; - PUNPKHI
;; - PUNPKLO
;; -------------------------------------------------------------------------

;; Unpack the low or high half of a predicate, where "high" refers to
;; the low-numbered lanes for big-endian and the high-numbered lanes
;; for little-endian.
(define_expand "vec_unpack<su>_<perm_hilo>_<mode>"
  [(match_operand:<VWIDE> 0 "register_operand")
   (unspec:<VWIDE> [(match_operand:PRED_BHS 1 "register_operand")]
		   UNPACK)]
  "TARGET_SVE"
  {
    emit_insn ((<hi_lanes_optab>
		? gen_aarch64_sve_punpkhi_<PRED_BHS:mode>
		: gen_aarch64_sve_punpklo_<PRED_BHS:mode>)
	       (operands[0], operands[1]));
    DONE;
  }
)

(define_insn "aarch64_sve_punpk<perm_hilo>_<mode>"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=Upa")
	(unspec:<VWIDE> [(match_operand:PRED_BHS 1 "register_operand" "Upa")]
			UNPACK_UNSIGNED))]
  "TARGET_SVE"
  "punpk<perm_hilo>\t%0.h, %1.b"
)
