;; Machine description for AArch64 SVE.
;; Copyright (C) 2009-2025 Free Software Foundation, Inc.
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
;; ---- Note on FFR handling
;;
;; == Moves
;; ---- Moves of single vectors
;; ---- Moves of multiple vectors
;; ---- Moves of predicates
;; ---- Moves of multiple predicates
;; ---- Moves relating to the FFR
;;
;; == Loads
;; ---- Normal contiguous loads
;; ---- Extending contiguous loads
;; ---- First-faulting contiguous loads
;; ---- First-faulting extending contiguous loads
;; ---- Non-temporal contiguous loads
;; ---- Normal gather loads
;; ---- Extending gather loads
;; ---- First-faulting gather loads
;; ---- First-faulting extending gather loads
;;
;; == Prefetches
;; ---- Contiguous prefetches
;; ---- Gather prefetches
;;
;; == Stores
;; ---- Normal contiguous stores
;; ---- Truncating contiguous stores
;; ---- Non-temporal contiguous stores
;; ---- Normal scatter stores
;; ---- Truncating scatter stores
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
;; ---- [INT] Sign and zero extension
;; ---- [INT] Truncation
;; ---- [INT] Logical inverse
;; ---- [FP<-INT] General unary arithmetic that maps to unspecs
;; ---- [FP] General unary arithmetic corresponding to unspecs
;; ---- [FP] Square root
;; ---- [FP] Reciprocal square root
;; ---- [PRED] Inverse

;; == Binary arithmetic
;; ---- [INT] General binary arithmetic corresponding to rtx codes
;; ---- [INT] Addition
;; ---- [INT] Subtraction
;; ---- [INT] Take address
;; ---- [INT] Absolute difference
;; ---- [INT] Saturating addition and subtraction
;; ---- [INT] Highpart multiplication
;; ---- [INT] Division
;; ---- [INT] Binary logical operations
;; ---- [INT] Binary logical operations (inverted second input)
;; ---- [INT] Shifts (rounding towards -Inf)
;; ---- [INT] Shifts (rounding towards 0)
;; ---- [FP<-INT] General binary arithmetic corresponding to unspecs
;; ---- [FP] General binary arithmetic corresponding to rtx codes
;; ---- [FP] General binary arithmetic corresponding to unspecs
;; ---- [FP] Addition
;; ---- [FP] Complex addition
;; ---- [FP] Subtraction
;; ---- [FP] Absolute difference
;; ---- [FP] Multiplication
;; ---- [FP] Division
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
;; ---- [INT] Matrix multiply-accumulate
;; ---- [FP] General ternary arithmetic corresponding to unspecs
;; ---- [FP] Complex multiply-add
;; ---- [FP] Trigonometric multiply-add
;; ---- [FP] Bfloat16 long ternary arithmetic (SF,BF,BF)
;; ---- [FP] Matrix multiply-accumulate
;;
;; == Comparisons and selects
;; ---- [INT,FP] Select based on predicates
;; ---- [INT] Comparisons
;; ---- [INT] While tests
;; ---- [FP] Direct comparisons
;; ---- [FP] Absolute comparisons
;; ---- [PRED] Select
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
;; ---- [FP<-FP] Packs (bfloat16)
;; ---- [FP<-FP] Unpacks
;; ---- [PRED<-PRED] Packs
;; ---- [PRED<-PRED] Unpacks
;;
;; == Vector partitioning
;; ---- [PRED] Unary partitioning
;; ---- [PRED] Binary partitioning
;; ---- [PRED] Scalarization
;;
;; == Counting elements
;; ---- [INT] Count elements in a pattern (scalar)
;; ---- [INT] Increment by the number of elements in a pattern (scalar)
;; ---- [INT] Increment by the number of elements in a pattern (vector)
;; ---- [INT] Decrement by the number of elements in a pattern (scalar)
;; ---- [INT] Decrement by the number of elements in a pattern (vector)
;; ---- [INT] Count elements in a predicate (scalar)
;; ---- [INT] Increment by the number of elements in a predicate (scalar)
;; ---- [INT] Increment by the number of elements in a predicate (vector)
;; ---- [INT] Decrement by the number of elements in a predicate (scalar)
;; ---- [INT] Decrement by the number of elements in a predicate (vector)

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
;;	      (sqrt:SVE_FULL_F 2 "register_operand" "w")]
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
;;
;; For (3) and (4), we combine these operations with an UNSPEC_SEL
;; that selects between the result of the FP operation and the "else"
;; value.  (This else value is a merge input for _m ACLE functions
;; and zero for _z ACLE functions.)  The outer pattern then has the form:
;;
;;   (unspec [pred fp_operation else_value] UNSPEC_SEL)
;;
;; This means that the patterns for (3) and (4) have two predicates:
;; one for the FP operation itself and one for the UNSPEC_SEL.
;; This pattern is equivalent to the result of combining an instance
;; of (1) or (2) with a separate vcond instruction, so these patterns
;; are useful as combine targets too.
;;
;; However, in the combine case, the instructions that we want to
;; combine might use different predicates.  Then:
;;
;; - Some of the active lanes of the FP operation might be discarded
;;   by the UNSPEC_SEL.  It's OK to drop the FP operation on those lanes,
;;   even for SVE_STRICT_GP, since the operations on those lanes are
;;   effectively dead code.
;;
;; - Some of the inactive lanes of the FP operation might be selected
;;   by the UNSPEC_SEL, giving unspecified values for those lanes.
;;   SVE_RELAXED_GP lets us extend the FP operation to cover these
;;   extra lanes, but SVE_STRICT_GP does not.
;;
;; Thus SVE_RELAXED_GP allows us to ignore the predicate on the FP operation
;; and operate on exactly the lanes selected by the UNSPEC_SEL predicate.
;; This typically leads to patterns like:
;;
;;    (unspec [(match_operand 1 "register_operand" "Upl")
;;             (unspec [(match_operand N)
;;                      (const_int SVE_RELAXED_GP)
;;                      ...]
;;                     UNSPEC_COND_<MNEMONIC>)
;;             ...])
;;
;; where operand N is allowed to be anything.  These instructions then
;; have rewrite rules to replace operand N with operand 1, which gives the
;; instructions a canonical form and means that the original operand N is
;; not kept live unnecessarily.
;;
;; In contrast, SVE_STRICT_GP only allows the UNSPEC_SEL predicate to be
;; a subset of the FP operation predicate.  This case isn't interesting
;; for FP operations that have an all-true predicate, since such operations
;; use SVE_RELAXED_GP instead.  And it is not possible for instruction
;; conditions to track the subset relationship for arbitrary registers.
;; So in practice, the only useful case for SVE_STRICT_GP is the one
;; in which the predicates match:
;;
;;    (unspec [(match_operand 1 "register_operand" "Upl")
;;             (unspec [(match_dup 1)
;;                      (const_int SVE_STRICT_GP)
;;                      ...]
;;                     UNSPEC_COND_<MNEMONIC>)
;;             ...])
;;
;; This pattern would also be correct for SVE_RELAXED_GP, but it would
;; be redundant with the one above.  However, if the combine pattern
;; has multiple FP operations, using a match_operand allows combinations
;; of SVE_STRICT_GP and SVE_RELAXED_GP in the same operation, provided
;; that the predicates are the same:
;;
;;    (unspec [(match_operand 1 "register_operand" "Upl")
;;             (...
;;                (unspec [(match_dup 1)
;;                         (match_operand:SI N "aarch64_sve_gp_strictness")
;;                         ...]
;;                        UNSPEC_COND_<MNEMONIC1>)
;;                (unspec [(match_dup 1)
;;                         (match_operand:SI M "aarch64_sve_gp_strictness")
;;                         ...]
;;                        UNSPEC_COND_<MNEMONIC2>) ...)
;;             ...])
;;
;; The fully-relaxed version of this pattern is:
;;
;;    (unspec [(match_operand 1 "register_operand" "Upl")
;;             (...
;;                (unspec [(match_operand:SI N)
;;                         (const_int SVE_RELAXED_GP)
;;                         ...]
;;                        UNSPEC_COND_<MNEMONIC1>)
;;                (unspec [(match_operand:SI M)
;;                         (const_int SVE_RELAXED_GP)
;;                         ...]
;;                        UNSPEC_COND_<MNEMONIC2>) ...)
;;             ...])
;;
;; -------------------------------------------------------------------------
;; ---- Note on FFR handling
;; -------------------------------------------------------------------------
;;
;; Logically we want to divide FFR-related instructions into regions
;; that contain exactly one of:
;;
;; - a single write to the FFR
;; - any number of reads from the FFR (but only one read is likely)
;; - any number of LDFF1 and LDNF1 instructions
;;
;; However, LDFF1 and LDNF1 instructions should otherwise behave like
;; normal loads as far as possible.  This means that they should be
;; schedulable within a region in the same way that LD1 would be,
;; and they should be deleted as dead if the result is unused.  The loads
;; should therefore not write to the FFR, since that would both serialize
;; the loads with respect to each other and keep the loads live for any
;; later RDFFR.
;;
;; We get around this by using a fake "FFR token" (FFRT) to help describe
;; the dependencies.  Writing to the FFRT starts a new "FFRT region",
;; while using the FFRT keeps the instruction within its region.
;; Specifically:
;;
;; - Writes start a new FFRT region as well as setting the FFR:
;;
;;       W1: parallel (FFRT = <new value>, FFR = <actual FFR value>)
;;
;; - Loads use an LD1-like instruction that also uses the FFRT, so that the
;;   loads stay within the same FFRT region:
;;
;;       L1: load data while using the FFRT
;;
;;   In addition, any FFRT region that includes a load also has at least one
;;   instance of:
;;
;;       L2: FFR = update(FFR, FFRT)  [type == no_insn]
;;
;;   to make it clear that the region both reads from and writes to the FFR.
;;
;; - Reads do the following:
;;
;;       R1: FFRT = FFR               [type == no_insn]
;;       R2: read from the FFRT
;;       R3: FFRT = update(FFRT)      [type == no_insn]
;;
;;   R1 and R3 both create new FFRT regions, so that previous LDFF1s and
;;   LDNF1s cannot move forwards across R1 and later LDFF1s and LDNF1s
;;   cannot move backwards across R3.
;;
;; This way, writes are only kept alive by later loads or reads,
;; and write/read pairs fold normally.  For two consecutive reads,
;; the first R3 is made dead by the second R1, which in turn becomes
;; redundant with the first R1.  We then have:
;;
;;     first R1: FFRT = FFR
;;     first read from the FFRT
;;     second read from the FFRT
;;     second R3: FFRT = update(FFRT)
;;
;; i.e. the two FFRT regions collapse into a single one with two
;; independent reads.
;;
;; The model still prevents some valid optimizations though.  For example,
;; if all loads in an FFRT region are deleted as dead, nothing would remove
;; the L2 instructions.

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

;; Unpredicated moves that can use LDR and STR, i.e. full vectors for which
;; little-endian ordering is acceptable.  Only allow memory operations during
;; and after RA; before RA we want the predicated load and store patterns to
;; be used instead.
(define_insn "*aarch64_sve_mov<mode>_ldr_str"
  [(set (match_operand:SVE_FULL 0 "aarch64_sve_nonimmediate_operand")
	(match_operand:SVE_FULL 1 "aarch64_sve_general_operand"))]
  "TARGET_SVE
   && (<MODE>mode == VNx16QImode || !BYTES_BIG_ENDIAN)
   && ((lra_in_progress || reload_completed)
       || (register_operand (operands[0], <MODE>mode)
	   && nonmemory_operand (operands[1], <MODE>mode)))"
  {@ [ cons: =0 , 1    ]
     [ w        , Utr  ] ldr\t%0, %1
     [ Utr      , w    ] str\t%1, %0
     [ w        , w    ] mov\t%0.d, %1.d
     [ w        , Dn   ] << aarch64_output_sve_mov_immediate (operands[1]);
  }
)

;; Unpredicated moves that cannot use LDR and STR, i.e. partial vectors
;; or vectors for which little-endian ordering isn't acceptable.  Memory
;; accesses require secondary reloads.
(define_insn "*aarch64_sve_mov<mode>_no_ldr_str"
  [(set (match_operand:SVE_ALL 0 "register_operand")
	(match_operand:SVE_ALL 1 "aarch64_nonmemory_operand"))]
  "TARGET_SVE
   && <MODE>mode != VNx16QImode
   && (BYTES_BIG_ENDIAN
       || maybe_ne (BYTES_PER_SVE_VECTOR, GET_MODE_SIZE (<MODE>mode)))"
  {@ [ cons: =0 , 1   ]
     [ w        , w   ] mov\t%0.d, %1.d
     [ w        , Dn  ] << aarch64_output_sve_mov_immediate (operands[1]);
  }
)

;; Handle memory reloads for modes that can't use LDR and STR.  We use
;; byte PTRUE for all modes to try to encourage reuse.  This pattern
;; needs constraints because it is returned by TARGET_SECONDARY_RELOAD.
(define_expand "aarch64_sve_reload_mem"
  [(parallel
     [(set (match_operand 0)
	   (match_operand 1))
      (clobber (match_operand:VNx16BI 2 "register_operand" "=Upl"))])]
  "TARGET_SVE"
  {
    /* Create a PTRUE.  */
    emit_move_insn (operands[2], CONSTM1_RTX (VNx16BImode));

    /* Refer to the PTRUE in the appropriate mode for this move.  */
    machine_mode mode = GET_MODE (operands[0]);
    rtx pred = gen_lowpart (aarch64_sve_pred_mode (mode), operands[2]);

    /* Emit a predicated load or store.  */
    aarch64_emit_sve_pred_move (operands[0], pred, operands[1]);
    DONE;
  }
)

;; A predicated move in which the predicate is known to be all-true.
;; Note that this pattern is generated directly by aarch64_emit_sve_pred_move,
;; so changes to this pattern will need changes there as well.
(define_insn_and_split "@aarch64_pred_mov<mode>"
  [(set (match_operand:SVE_ALL 0 "nonimmediate_operand")
	(unspec:SVE_ALL
	  [(match_operand:<VPRED> 1 "register_operand")
	   (match_operand:SVE_ALL 2 "nonimmediate_operand")]
	  UNSPEC_PRED_X))]
  "TARGET_SVE
   && (register_operand (operands[0], <MODE>mode)
       || register_operand (operands[2], <MODE>mode))"
  {@ [ cons: =0 , 1   , 2  ]
     [ w        , Upl , w  ] #
     [ w        , Upl , m  ] ld1<Vesize>\t%0.<Vctype>, %1/z, %2
     [ m        , Upl , w  ] st1<Vesize>\t%2.<Vctype>, %1, %0
  }
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
  [(set (match_operand:SVE_ALL_STRUCT 0 "register_operand")
	(unspec:SVE_ALL_STRUCT
	  [(match_operand 1 "aarch64_any_register_operand")]
	  UNSPEC_REINTERPRET))]
  "TARGET_SVE"
  {
    machine_mode src_mode = GET_MODE (operands[1]);
    if (targetm.can_change_mode_class (<MODE>mode, src_mode, FP_REGS))
      {
	emit_move_insn (operands[0], gen_lowpart (<MODE>mode, operands[1]));
	DONE;
      }
  }
)

;; A pattern for handling type punning on big-endian targets.  We use a
;; special predicate for operand 1 to reduce the number of patterns.
(define_insn_and_split "*aarch64_sve_reinterpret<mode>"
  [(set (match_operand:SVE_ALL_STRUCT 0 "register_operand" "=w")
	(unspec:SVE_ALL_STRUCT
	  [(match_operand 1 "aarch64_any_register_operand" "w")]
	  UNSPEC_REINTERPRET))]
  "TARGET_SVE"
  "#"
  "&& reload_completed"
  [(set (match_dup 0) (match_dup 1))]
  {
    operands[1] = aarch64_replace_reg_mode (operands[1], <MODE>mode);
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
;; - PTRUES
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
  [(set (match_operand:PRED_ALL 0 "nonimmediate_operand")
	(match_operand:PRED_ALL 1 "aarch64_mov_operand"))]
  "TARGET_SVE
   && (register_operand (operands[0], <MODE>mode)
       || register_operand (operands[1], <MODE>mode))"
  {@ [ cons: =0 , 1    ]
     [ Upa      , Upa  ] mov\t%0.b, %1.b
     [ m        , Upa  ] str\t%1, %0
     [ Upa      , m    ] ldr\t%0, %1
     [ Upa      , Dn   ] << aarch64_output_sve_mov_immediate (operands[1]);
  }
)

;; Match PTRUES Pn.B when both the predicate and flags are useful.
(define_insn_and_rewrite "*aarch64_sve_ptruevnx16bi_cc"
  [(set (reg:CC_NZC CC_REGNUM)
	(unspec:CC_NZC
	  [(match_operand 2)
	   (match_operand 3)
	   (const_int SVE_KNOWN_PTRUE)
	   (match_operator:VNx16BI 1 "aarch64_sve_ptrue_svpattern_immediate"
	     [(unspec:VNx16BI
		[(match_operand:SI 4 "const_int_operand")
		 (match_operand:VNx16BI 5 "aarch64_simd_imm_zero")]
		UNSPEC_PTRUE)])]
	  UNSPEC_PTEST))
   (set (match_operand:VNx16BI 0 "register_operand" "=Upa")
	(match_dup 1))]
  "TARGET_SVE"
  {
    return aarch64_output_sve_ptrues (operands[1]);
  }
  "&& (!CONSTANT_P (operands[2]) || !CONSTANT_P (operands[3]))"
  {
    operands[2] = operands[3] = CONSTM1_RTX (VNx16BImode);
  }
)

;; Match PTRUES Pn.[HSD] when both the predicate and flags are useful.
(define_insn_and_rewrite "*aarch64_sve_ptrue<mode>_cc"
  [(set (reg:CC_NZC CC_REGNUM)
	(unspec:CC_NZC
	  [(match_operand 2)
	   (match_operand 3)
	   (const_int SVE_KNOWN_PTRUE)
	   (subreg:PRED_HSD
	     (match_operator:VNx16BI 1 "aarch64_sve_ptrue_svpattern_immediate"
	       [(unspec:VNx16BI
		  [(match_operand:SI 4 "const_int_operand")
		   (match_operand:PRED_HSD 5 "aarch64_simd_imm_zero")]
		  UNSPEC_PTRUE)]) 0)]
	  UNSPEC_PTEST))
   (set (match_operand:VNx16BI 0 "register_operand" "=Upa")
	(match_dup 1))]
  "TARGET_SVE"
  {
    return aarch64_output_sve_ptrues (operands[1]);
  }
  "&& (!CONSTANT_P (operands[2]) || !CONSTANT_P (operands[3]))"
  {
    operands[2] = CONSTM1_RTX (VNx16BImode);
    operands[3] = CONSTM1_RTX (<MODE>mode);
  }
)

;; Match PTRUES Pn.B when only the flags result is useful (which is
;; a way of testing VL).
(define_insn_and_rewrite "*aarch64_sve_ptruevnx16bi_ptest"
  [(set (reg:CC_NZC CC_REGNUM)
	(unspec:CC_NZC
	  [(match_operand 2)
	   (match_operand 3)
	   (const_int SVE_KNOWN_PTRUE)
	   (match_operator:VNx16BI 1 "aarch64_sve_ptrue_svpattern_immediate"
	     [(unspec:VNx16BI
		[(match_operand:SI 4 "const_int_operand")
		 (match_operand:VNx16BI 5 "aarch64_simd_imm_zero")]
		UNSPEC_PTRUE)])]
	  UNSPEC_PTEST))
   (clobber (match_scratch:VNx16BI 0 "=Upa"))]
  "TARGET_SVE"
  {
    return aarch64_output_sve_ptrues (operands[1]);
  }
  "&& (!CONSTANT_P (operands[2]) || !CONSTANT_P (operands[3]))"
  {
    operands[2] = operands[3] = CONSTM1_RTX (VNx16BImode);
  }
)

;; Match PTRUES Pn.[HWD] when only the flags result is useful (which is
;; a way of testing VL).
(define_insn_and_rewrite "*aarch64_sve_ptrue<mode>_ptest"
  [(set (reg:CC_NZC CC_REGNUM)
	(unspec:CC_NZC
	  [(match_operand 2)
	   (match_operand 3)
	   (const_int SVE_KNOWN_PTRUE)
	   (subreg:PRED_HSD
	     (match_operator:VNx16BI 1 "aarch64_sve_ptrue_svpattern_immediate"
	       [(unspec:VNx16BI
		  [(match_operand:SI 4 "const_int_operand")
		   (match_operand:PRED_HSD 5 "aarch64_simd_imm_zero")]
		  UNSPEC_PTRUE)]) 0)]
	  UNSPEC_PTEST))
   (clobber (match_scratch:VNx16BI 0 "=Upa"))]
  "TARGET_SVE"
  {
    return aarch64_output_sve_ptrues (operands[1]);
  }
  "&& (!CONSTANT_P (operands[2]) || !CONSTANT_P (operands[3]))"
  {
    operands[2] = CONSTM1_RTX (VNx16BImode);
    operands[3] = CONSTM1_RTX (<MODE>mode);
  }
)

;; -------------------------------------------------------------------------
;; ---- Moves of multiple predicates
;; -------------------------------------------------------------------------

(define_insn_and_split "mov<mode>"
  [(set (match_operand:SVE_STRUCT_BI 0 "nonimmediate_operand")
	(match_operand:SVE_STRUCT_BI 1 "aarch64_mov_operand"))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   ]
     [ Upa      , Upa ] #
     [ Upa      , m   ] #
     [ m        , Upa ] #
  }
  "&& reload_completed"
  [(const_int 0)]
  {
    aarch64_split_move (operands[0], operands[1], VNx16BImode);
    DONE;
  }
)

;; -------------------------------------------------------------------------
;; ---- Moves relating to the FFR
;; -------------------------------------------------------------------------
;; RDFFR
;; RDFFRS
;; SETFFR
;; WRFFR
;; -------------------------------------------------------------------------

;; [W1 in the block comment above about FFR handling]
;;
;; Write to the FFR and start a new FFRT scheduling region.
(define_insn "aarch64_wrffr"
  [(set (reg:VNx16BI FFR_REGNUM)
	(match_operand:VNx16BI 0 "aarch64_simd_reg_or_minus_one"))
   (set (reg:VNx16BI FFRT_REGNUM)
	(unspec:VNx16BI [(match_dup 0)] UNSPEC_WRFFR))]
  "TARGET_SVE && TARGET_NON_STREAMING"
  {@ [ cons: 0 ]
     [ Dm      ] setffr
     [ Upa     ] wrffr\t%0.b
  }
)

;; [L2 in the block comment above about FFR handling]
;;
;; Introduce a read from and write to the FFR in the current FFRT region,
;; so that the FFR value is live on entry to the region and so that the FFR
;; value visibly changes within the region.  This is used (possibly multiple
;; times) in an FFRT region that includes LDFF1 or LDNF1 instructions.
(define_insn "aarch64_update_ffr_for_load"
  [(set (reg:VNx16BI FFR_REGNUM)
	(unspec:VNx16BI [(reg:VNx16BI FFRT_REGNUM)
			 (reg:VNx16BI FFR_REGNUM)] UNSPEC_UPDATE_FFR))]
  "TARGET_SVE"
  ""
  [(set_attr "type" "no_insn")]
)

;; [R1 in the block comment above about FFR handling]
;;
;; Notionally copy the FFR to the FFRT, so that the current FFR value
;; can be read from there by the RDFFR instructions below.  This acts
;; as a scheduling barrier for earlier LDFF1 and LDNF1 instructions and
;; creates a natural dependency with earlier writes.
(define_insn "aarch64_copy_ffr_to_ffrt"
  [(set (reg:VNx16BI FFRT_REGNUM)
	(reg:VNx16BI FFR_REGNUM))]
  "TARGET_SVE"
  ""
  [(set_attr "type" "no_insn")]
)

;; [R2 in the block comment above about FFR handling]
;;
;; Read the FFR via the FFRT.
(define_insn "aarch64_rdffr"
  [(set (match_operand:VNx16BI 0 "register_operand" "=Upa")
	(reg:VNx16BI FFRT_REGNUM))]
  "TARGET_SVE && TARGET_NON_STREAMING"
  "rdffr\t%0.b"
)

;; Likewise with zero predication.
(define_insn "aarch64_rdffr_z"
  [(set (match_operand:VNx16BI 0 "register_operand")
	(and:VNx16BI
	  (reg:VNx16BI FFRT_REGNUM)
	  (match_operand:VNx16BI 1 "register_operand")))]
  "TARGET_SVE && TARGET_NON_STREAMING"
  {@ [ cons: =0, 1   ; attrs: pred_clobber ]
     [ &Upa    , Upa ; yes                 ] rdffr\t%0.b, %1/z
     [ ?Upa    , 0Upa; yes                 ] ^
     [ Upa     , Upa ; no                  ] ^
  }
)

;; Read the FFR to test for a fault, without using the predicate result.
(define_insn "*aarch64_rdffr_z_ptest"
  [(set (reg:CC_NZC CC_REGNUM)
	(unspec:CC_NZC
	  [(match_operand:VNx16BI 1 "register_operand")
	   (match_dup 1)
	   (match_operand:SI 2 "aarch64_sve_ptrue_flag")
	   (and:VNx16BI
	     (reg:VNx16BI FFRT_REGNUM)
	     (match_dup 1))]
	  UNSPEC_PTEST))
   (clobber (match_scratch:VNx16BI 0))]
  "TARGET_SVE && TARGET_NON_STREAMING"
  {@ [ cons: =0, 1   ; attrs: pred_clobber ]
     [ &Upa    , Upa ; yes                 ] rdffrs\t%0.b, %1/z
     [ ?Upa    , 0Upa; yes                 ] ^
     [ Upa     , Upa ; no                  ] ^
  }
)

;; Same for unpredicated RDFFR when tested with a known PTRUE.
(define_insn "*aarch64_rdffr_ptest"
  [(set (reg:CC_NZC CC_REGNUM)
	(unspec:CC_NZC
	  [(match_operand:VNx16BI 1 "register_operand")
	   (match_dup 1)
	   (const_int SVE_KNOWN_PTRUE)
	   (reg:VNx16BI FFRT_REGNUM)]
	  UNSPEC_PTEST))
   (clobber (match_scratch:VNx16BI 0))]
  "TARGET_SVE && TARGET_NON_STREAMING"
  {@ [ cons: =0, 1   ; attrs: pred_clobber ]
     [ &Upa    , Upa ; yes                 ] rdffrs\t%0.b, %1/z
     [ ?Upa    , 0Upa; yes                 ] ^
     [ Upa     , Upa ; no                  ] ^
  }
)

;; Read the FFR with zero predication and test the result.
(define_insn "*aarch64_rdffr_z_cc"
  [(set (reg:CC_NZC CC_REGNUM)
	(unspec:CC_NZC
	  [(match_operand:VNx16BI 1 "register_operand")
	   (match_dup 1)
	   (match_operand:SI 2 "aarch64_sve_ptrue_flag")
	   (and:VNx16BI
	     (reg:VNx16BI FFRT_REGNUM)
	     (match_dup 1))]
	  UNSPEC_PTEST))
   (set (match_operand:VNx16BI 0 "register_operand")
	(and:VNx16BI
	  (reg:VNx16BI FFRT_REGNUM)
	  (match_dup 1)))]
  "TARGET_SVE && TARGET_NON_STREAMING"
  {@ [ cons: =0, 1   ; attrs: pred_clobber ]
     [ &Upa    , Upa ; yes                 ] rdffrs\t%0.b, %1/z
     [ ?Upa    , 0Upa; yes                 ] ^
     [ Upa     , Upa ; no                  ] ^
  }
)

;; Same for unpredicated RDFFR when tested with a known PTRUE.
(define_insn "*aarch64_rdffr_cc"
  [(set (reg:CC_NZC CC_REGNUM)
	(unspec:CC_NZC
	  [(match_operand:VNx16BI 1 "register_operand")
	   (match_dup 1)
	   (const_int SVE_KNOWN_PTRUE)
	   (reg:VNx16BI FFRT_REGNUM)]
	  UNSPEC_PTEST))
   (set (match_operand:VNx16BI 0 "register_operand")
	(reg:VNx16BI FFRT_REGNUM))]
  "TARGET_SVE && TARGET_NON_STREAMING"
  {@ [ cons: =0, 1   ; attrs: pred_clobber ]
     [ &Upa    , Upa ; yes                 ] rdffrs\t%0.b, %1/z
     [ ?Upa    , 0Upa; yes                 ] ^
     [ Upa     , Upa ; no                  ] ^
  }
)

;; [R3 in the block comment above about FFR handling]
;;
;; Arbitrarily update the FFRT after a read from the FFR.  This acts as
;; a scheduling barrier for later LDFF1 and LDNF1 instructions.
(define_insn "aarch64_update_ffrt"
  [(set (reg:VNx16BI FFRT_REGNUM)
	(unspec:VNx16BI [(reg:VNx16BI FFRT_REGNUM)] UNSPEC_UPDATE_FFRT))]
  "TARGET_SVE"
  ""
  [(set_attr "type" "no_insn")]
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

;; Predicated LD1 (single).
(define_insn "maskload<mode><vpred>"
  [(set (match_operand:SVE_ALL 0 "register_operand" "=w")
	(unspec:SVE_ALL
	  [(match_operand:<VPRED> 2 "register_operand" "Upl")
	   (match_operand:SVE_ALL 1 "memory_operand" "m")
	   (match_operand:SVE_ALL 3 "aarch64_maskload_else_operand")]
	  UNSPEC_LD1_SVE))]
  "TARGET_SVE"
  "ld1<Vesize>\t%0.<Vctype>, %2/z, %1"
)

;; Unpredicated LD[234].
(define_expand "vec_load_lanes<mode><vsingle>"
  [(set (match_operand:SVE_STRUCT 0 "register_operand")
	(unspec:SVE_STRUCT
	  [(match_dup 2)
	   (match_operand:SVE_STRUCT 1 "memory_operand")
	   (match_dup 3)]
	  UNSPEC_LDN))]
  "TARGET_SVE"
  {
    operands[2] = aarch64_ptrue_reg (<VPRED>mode);
    operands[3] = CONST0_RTX (<MODE>mode);
  }
)

;; Predicated LD[234].
(define_insn "vec_mask_load_lanes<mode><vsingle>"
  [(set (match_operand:SVE_STRUCT 0 "register_operand" "=w")
	(unspec:SVE_STRUCT
	  [(match_operand:<VPRED> 2 "register_operand" "Upl")
	   (match_operand:SVE_STRUCT 1 "memory_operand" "m")
	   (match_operand 3 "aarch64_maskload_else_operand")]
	  UNSPEC_LDN))]
  "TARGET_SVE"
  "ld<vector_count><Vesize>\t%0, %2/z, %1"
)

;; -------------------------------------------------------------------------
;; ---- Extending contiguous loads
;; -------------------------------------------------------------------------
;; Includes contiguous forms of:
;; LD1B
;; LD1H
;; LD1SB
;; LD1SH
;; LD1SW
;; LD1W
;; -------------------------------------------------------------------------

;; Predicated load and extend, with 8 elements per 128-bit block.
(define_insn_and_rewrite "@aarch64_load_<ANY_EXTEND:optab><SVE_HSDI:mode><SVE_PARTIAL_I:mode>"
  [(set (match_operand:SVE_HSDI 0 "register_operand" "=w")
	(unspec:SVE_HSDI
	  [(match_operand:<SVE_HSDI:VPRED> 3 "general_operand" "UplDnm")
	   (ANY_EXTEND:SVE_HSDI
	     (unspec:SVE_PARTIAL_I
	       [(match_operand:<SVE_PARTIAL_I:VPRED> 2 "register_operand" "Upl")
		(match_operand:SVE_PARTIAL_I 1 "memory_operand" "m")
		(match_operand:SVE_PARTIAL_I 4 "aarch64_maskload_else_operand")]
	       UNSPEC_LD1_SVE))]
	  UNSPEC_PRED_X))]
  "TARGET_SVE && (~<SVE_HSDI:narrower_mask> & <SVE_PARTIAL_I:self_mask>) == 0"
  "ld1<ANY_EXTEND:s><SVE_PARTIAL_I:Vesize>\t%0.<SVE_HSDI:Vctype>, %2/z, %1"
  "&& !CONSTANT_P (operands[3])"
  {
    operands[3] = CONSTM1_RTX (<SVE_HSDI:VPRED>mode);
  }
)

;; Same as above without the maskload_else_operand to still allow combine to
;; match a sign-extended pred_mov pattern.
(define_insn_and_rewrite "*aarch64_load_<ANY_EXTEND:optab>_mov<SVE_HSDI:mode><SVE_PARTIAL_I:mode>"
  [(set (match_operand:SVE_HSDI 0 "register_operand" "=w")
	(unspec:SVE_HSDI
	  [(match_operand:<SVE_HSDI:VPRED> 3 "general_operand" "UplDnm")
	   (ANY_EXTEND:SVE_HSDI
	     (unspec:SVE_PARTIAL_I
	       [(match_operand:<SVE_PARTIAL_I:VPRED> 2 "register_operand" "Upl")
		(match_operand:SVE_PARTIAL_I 1 "memory_operand" "m")]
		UNSPEC_PRED_X))]
	   UNSPEC_PRED_X))]
  "TARGET_SVE && (~<SVE_HSDI:narrower_mask> & <SVE_PARTIAL_I:self_mask>) == 0"
  "ld1<ANY_EXTEND:s><SVE_PARTIAL_I:Vesize>\t%0.<SVE_HSDI:Vctype>, %2/z, %1"
  "&& !CONSTANT_P (operands[3])"
  {
    operands[3] = CONSTM1_RTX (<SVE_HSDI:VPRED>mode);
  }
)

;; -------------------------------------------------------------------------
;; ---- First-faulting contiguous loads
;; -------------------------------------------------------------------------
;; Includes contiguous forms of:
;; - LDFF1B
;; - LDFF1D
;; - LDFF1H
;; - LDFF1W
;; - LDNF1B
;; - LDNF1D
;; - LDNF1H
;; - LDNF1W
;; -------------------------------------------------------------------------

;; Contiguous non-extending first-faulting or non-faulting loads.
(define_insn "@aarch64_ld<fn>f1<mode>"
  [(set (match_operand:SVE_FULL 0 "register_operand" "=w")
	(unspec:SVE_FULL
	  [(match_operand:<VPRED> 2 "register_operand" "Upl")
	   (match_operand:SVE_FULL 1 "aarch64_sve_ld<fn>f1_operand" "Ut<fn>")
	   (reg:VNx16BI FFRT_REGNUM)]
	  SVE_LDFF1_LDNF1))]
  "TARGET_SVE && TARGET_NON_STREAMING"
  "ld<fn>f1<Vesize>\t%0.<Vetype>, %2/z, %1"
)

;; -------------------------------------------------------------------------
;; ---- First-faulting extending contiguous loads
;; -------------------------------------------------------------------------
;; Includes contiguous forms of:
;; - LDFF1B
;; - LDFF1H
;; - LDFF1SB
;; - LDFF1SH
;; - LDFF1SW
;; - LDFF1W
;; - LDNF1B
;; - LDNF1H
;; - LDNF1SB
;; - LDNF1SH
;; - LDNF1SW
;; - LDNF1W
;; -------------------------------------------------------------------------

;; Predicated first-faulting or non-faulting load and extend.
(define_insn_and_rewrite "@aarch64_ld<fn>f1_<ANY_EXTEND:optab><SVE_HSDI:mode><SVE_PARTIAL_I:mode>"
  [(set (match_operand:SVE_HSDI 0 "register_operand" "=w")
	(unspec:SVE_HSDI
	  [(match_operand:<SVE_HSDI:VPRED> 3 "general_operand" "UplDnm")
	   (ANY_EXTEND:SVE_HSDI
	     (unspec:SVE_PARTIAL_I
	       [(match_operand:<SVE_PARTIAL_I:VPRED> 2 "register_operand" "Upl")
		(match_operand:SVE_PARTIAL_I 1 "aarch64_sve_ld<fn>f1_operand" "Ut<fn>")
		(reg:VNx16BI FFRT_REGNUM)]
	       SVE_LDFF1_LDNF1))]
	  UNSPEC_PRED_X))]
  "TARGET_SVE
   && TARGET_NON_STREAMING
   && (~<SVE_HSDI:narrower_mask> & <SVE_PARTIAL_I:self_mask>) == 0"
  "ld<fn>f1<ANY_EXTEND:s><SVE_PARTIAL_I:Vesize>\t%0.<SVE_HSDI:Vctype>, %2/z, %1"
  "&& !CONSTANT_P (operands[3])"
  {
    operands[3] = CONSTM1_RTX (<SVE_HSDI:VPRED>mode);
  }
)

;; -------------------------------------------------------------------------
;; ---- Non-temporal contiguous loads
;; -------------------------------------------------------------------------
;; Includes:
;; - LDNT1B
;; - LDNT1D
;; - LDNT1H
;; - LDNT1W
;; -------------------------------------------------------------------------

;; Predicated contiguous non-temporal load (single).
(define_insn "@aarch64_ldnt1<mode>"
  [(set (match_operand:SVE_FULL 0 "register_operand" "=w")
	(unspec:SVE_FULL
	  [(match_operand:<VPRED> 2 "register_operand" "Upl")
	   (match_operand:SVE_FULL 1 "memory_operand" "m")
	   (match_operand:SVE_FULL 3 "aarch64_maskload_else_operand")]
	  UNSPEC_LDNT1_SVE))]
  "TARGET_SVE"
  "ldnt1<Vesize>\t%0.<Vetype>, %2/z, %1"
)

;; -------------------------------------------------------------------------
;; ---- Normal gather loads
;; -------------------------------------------------------------------------
;; Includes gather forms of:
;; - LD1D
;; - LD1W
;; -------------------------------------------------------------------------

;; Unpredicated gather loads.
(define_expand "gather_load<mode><v_int_container>"
  [(set (match_operand:SVE_24 0 "register_operand")
	(unspec:SVE_24
	  [(match_dup 5)
	   (match_operand:DI 1 "aarch64_sve_gather_offset_<Vesize>")
	   (match_operand:<V_INT_CONTAINER> 2 "register_operand")
	   (match_operand:DI 3 "const_int_operand")
	   (match_operand:DI 4 "aarch64_gather_scale_operand_<Vesize>")
	   (match_dup 6)
	   (mem:BLK (scratch))]
	  UNSPEC_LD1_GATHER))]
  "TARGET_SVE && TARGET_NON_STREAMING"
  {
    operands[5] = aarch64_ptrue_reg (<VPRED>mode);
    operands[6] = CONST0_RTX (<MODE>mode);
  }
)

;; Predicated gather loads for 32-bit elements.  Operand 3 is true for
;; unsigned extension and false for signed extension.
(define_insn "mask_gather_load<mode><v_int_container>"
  [(set (match_operand:SVE_4 0 "register_operand")
	(unspec:SVE_4
	  [(match_operand:VNx4BI 5 "register_operand")
	   (match_operand:DI 1 "aarch64_sve_gather_offset_<Vesize>")
	   (match_operand:VNx4SI 2 "register_operand")
	   (match_operand:DI 3 "const_int_operand")
	   (match_operand:DI 4 "aarch64_gather_scale_operand_<Vesize>")
	   (match_operand:SVE_4 6 "aarch64_maskload_else_operand")
	   (mem:BLK (scratch))]
	  UNSPEC_LD1_GATHER))]
  "TARGET_SVE && TARGET_NON_STREAMING"
  {@ [cons: =0, 1, 2, 3, 4, 5  ]
     [&w, Z,   w, Ui1, Ui1, Upl] ld1<Vesize>\t%0.s, %5/z, [%2.s]
     [?w, Z,   0, Ui1, Ui1, Upl] ^
     [&w, vgw, w, Ui1, Ui1, Upl] ld1<Vesize>\t%0.s, %5/z, [%2.s, #%1]
     [?w, vgw, 0, Ui1, Ui1, Upl] ^
     [&w, rk,  w, Z,   Ui1, Upl] ld1<Vesize>\t%0.s, %5/z, [%1, %2.s, sxtw]
     [?w, rk,  0, Z,   Ui1, Upl] ^
     [&w, rk,  w, Ui1, Ui1, Upl] ld1<Vesize>\t%0.s, %5/z, [%1, %2.s, uxtw]
     [?w, rk,  0, Ui1, Ui1, Upl] ^
     [&w, rk,  w, Z,   i,   Upl] ld1<Vesize>\t%0.s, %5/z, [%1, %2.s, sxtw %p4]
     [?w, rk,  0, Z,   i,   Upl] ^
     [&w, rk,  w, Ui1, i,   Upl] ld1<Vesize>\t%0.s, %5/z, [%1, %2.s, uxtw %p4]
     [?w, rk,  0, Ui1, i,   Upl] ^
  }
)

;; Predicated gather loads for 64-bit elements.  The value of operand 3
;; doesn't matter in this case.
(define_insn "mask_gather_load<mode><v_int_container>"
  [(set (match_operand:SVE_2 0 "register_operand")
	(unspec:SVE_2
	  [(match_operand:VNx2BI 5 "register_operand")
	   (match_operand:DI 1 "aarch64_sve_gather_offset_<Vesize>")
	   (match_operand:VNx2DI 2 "register_operand")
	   (match_operand:DI 3 "const_int_operand")
	   (match_operand:DI 4 "aarch64_gather_scale_operand_<Vesize>")
	   (match_operand:SVE_2 6 "aarch64_maskload_else_operand")
	   (mem:BLK (scratch))]
	  UNSPEC_LD1_GATHER))]
  "TARGET_SVE && TARGET_NON_STREAMING"
  {@ [cons: =0, 1, 2, 3, 4, 5]
     [&w, Z,   w, i, Ui1, Upl] ld1<Vesize>\t%0.d, %5/z, [%2.d]
     [?w, Z,   0, i, Ui1, Upl] ^
     [&w, vgd, w, i, Ui1, Upl] ld1<Vesize>\t%0.d, %5/z, [%2.d, #%1]
     [?w, vgd, 0, i, Ui1, Upl] ^
     [&w, rk,  w, i, Ui1, Upl] ld1<Vesize>\t%0.d, %5/z, [%1, %2.d]
     [?w, rk,  0, i, Ui1, Upl] ^
     [&w, rk,  w, i, i,   Upl] ld1<Vesize>\t%0.d, %5/z, [%1, %2.d, lsl %p4]
     [?w, rk,  0, i, i,   Upl] ^
  }
)

;; Likewise, but with the offset being extended from 32 bits.
(define_insn_and_rewrite "*mask_gather_load<mode><v_int_container>_<su>xtw_unpacked"
  [(set (match_operand:SVE_2 0 "register_operand")
	(unspec:SVE_2
	  [(match_operand:VNx2BI 5 "register_operand")
	   (match_operand:DI 1 "register_operand")
	   (unspec:VNx2DI
	     [(match_operand 6)
	      (ANY_EXTEND:VNx2DI
		(match_operand:VNx2SI 2 "register_operand"))]
	     UNSPEC_PRED_X)
	   (match_operand:DI 3 "const_int_operand")
	   (match_operand:DI 4 "aarch64_gather_scale_operand_<Vesize>")
	   (match_operand:SVE_2 7 "aarch64_maskload_else_operand")
	   (mem:BLK (scratch))]
	  UNSPEC_LD1_GATHER))]
  "TARGET_SVE && TARGET_NON_STREAMING"
  {@ [cons: =0, 1, 2, 3, 4, 5]
     [&w, rk, w, i, Ui1, Upl ] ld1<Vesize>\t%0.d, %5/z, [%1, %2.d, <su>xtw]
     [?w, rk, 0, i, Ui1, Upl ] ^
     [&w, rk, w, i, i,   Upl ] ld1<Vesize>\t%0.d, %5/z, [%1, %2.d, <su>xtw %p4]
     [?w, rk, 0, i, i,   Upl ] ^
  }
  "&& !CONSTANT_P (operands[6])"
  {
    operands[6] = CONSTM1_RTX (VNx2BImode);
  }
)

;; Likewise, but with the offset being truncated to 32 bits and then
;; sign-extended.
(define_insn_and_rewrite "*mask_gather_load<mode><v_int_container>_sxtw"
  [(set (match_operand:SVE_2 0 "register_operand")
	(unspec:SVE_2
	  [(match_operand:VNx2BI 5 "register_operand")
	   (match_operand:DI 1 "register_operand")
	   (unspec:VNx2DI
	     [(match_operand 6)
	      (sign_extend:VNx2DI
		(truncate:VNx2SI
		  (match_operand:VNx2DI 2 "register_operand")))]
	     UNSPEC_PRED_X)
	   (match_operand:DI 3 "const_int_operand")
	   (match_operand:DI 4 "aarch64_gather_scale_operand_<Vesize>")
	   (match_operand:SVE_2 7 "aarch64_maskload_else_operand")
	   (mem:BLK (scratch))]
	  UNSPEC_LD1_GATHER))]
  "TARGET_SVE && TARGET_NON_STREAMING"
  {@ [cons: =0, 1, 2, 3, 4, 5]
     [&w, rk, w, i, Ui1, Upl ] ld1<Vesize>\t%0.d, %5/z, [%1, %2.d, sxtw]
     [?w, rk, 0, i, Ui1, Upl ] ^
     [&w, rk, w, i, i,   Upl ] ld1<Vesize>\t%0.d, %5/z, [%1, %2.d, sxtw %p4]
     [?w, rk, 0, i, i,   Upl ] ^
  }
  "&& !CONSTANT_P (operands[6])"
  {
    operands[6] = CONSTM1_RTX (VNx2BImode);
  }
)

;; Likewise, but with the offset being truncated to 32 bits and then
;; zero-extended.
(define_insn "*mask_gather_load<mode><v_int_container>_uxtw"
  [(set (match_operand:SVE_2 0 "register_operand")
	(unspec:SVE_2
	  [(match_operand:VNx2BI 5 "register_operand")
	   (match_operand:DI 1 "register_operand")
	   (and:VNx2DI
	     (match_operand:VNx2DI 2 "register_operand")
	     (match_operand:VNx2DI 6 "aarch64_sve_uxtw_immediate"))
	   (match_operand:DI 3 "const_int_operand")
	   (match_operand:DI 4 "aarch64_gather_scale_operand_<Vesize>")
	   (match_operand:SVE_2 7 "aarch64_maskload_else_operand")
	   (mem:BLK (scratch))]
	  UNSPEC_LD1_GATHER))]
  "TARGET_SVE && TARGET_NON_STREAMING"
  {@ [cons: =0, 1, 2, 3, 4, 5]
     [&w, rk, w, i, Ui1, Upl ] ld1<Vesize>\t%0.d, %5/z, [%1, %2.d, uxtw]
     [?w, rk, 0, i, Ui1, Upl ] ^
     [&w, rk, w, i, i,   Upl ] ld1<Vesize>\t%0.d, %5/z, [%1, %2.d, uxtw %p4]
     [?w, rk, 0, i, i,   Upl ] ^
  }
)

;; -------------------------------------------------------------------------
;; ---- Extending gather loads
;; -------------------------------------------------------------------------
;; Includes gather forms of:
;; - LD1B
;; - LD1H
;; - LD1SB
;; - LD1SH
;; - LD1SW
;; - LD1W
;; -------------------------------------------------------------------------

;; Predicated extending gather loads for 32-bit elements.  Operand 3 is
;; true for unsigned extension and false for signed extension.
(define_insn_and_rewrite "@aarch64_gather_load_<ANY_EXTEND:optab><SVE_4HSI:mode><SVE_4BHI:mode>"
  [(set (match_operand:SVE_4HSI 0 "register_operand")
	(unspec:SVE_4HSI
	  [(match_operand:VNx4BI 6 "general_operand")
	   (ANY_EXTEND:SVE_4HSI
	     (unspec:SVE_4BHI
	       [(match_operand:VNx4BI 5 "register_operand")
		(match_operand:DI 1 "aarch64_sve_gather_offset_<SVE_4BHI:Vesize>")
		(match_operand:VNx4SI 2 "register_operand")
		(match_operand:DI 3 "const_int_operand")
		(match_operand:DI 4 "aarch64_gather_scale_operand_<SVE_4BHI:Vesize>")
		(match_operand:SVE_4BHI 7 "aarch64_maskload_else_operand")
		(mem:BLK (scratch))]
	       UNSPEC_LD1_GATHER))]
	  UNSPEC_PRED_X))]
  "TARGET_SVE
   && TARGET_NON_STREAMING
   && (~<SVE_4HSI:narrower_mask> & <SVE_4BHI:self_mask>) == 0"
  {@ [cons: =0, 1, 2, 3, 4, 5, 6]
     [&w, Z,                   w, Ui1, Ui1, Upl, UplDnm] ld1<ANY_EXTEND:s><SVE_4BHI:Vesize>\t%0.s, %5/z, [%2.s]
     [?w, Z,                   0, Ui1, Ui1, Upl, UplDnm] ^
     [&w, vg<SVE_4BHI:Vesize>, w, Ui1, Ui1, Upl, UplDnm] ld1<ANY_EXTEND:s><SVE_4BHI:Vesize>\t%0.s, %5/z, [%2.s, #%1]
     [?w, vg<SVE_4BHI:Vesize>, 0, Ui1, Ui1, Upl, UplDnm] ^
     [&w, rk,                  w, Z,   Ui1, Upl, UplDnm] ld1<ANY_EXTEND:s><SVE_4BHI:Vesize>\t%0.s, %5/z, [%1, %2.s, sxtw]
     [?w, rk,                  0, Z,   Ui1, Upl, UplDnm] ^
     [&w, rk,                  w, Ui1, Ui1, Upl, UplDnm] ld1<ANY_EXTEND:s><SVE_4BHI:Vesize>\t%0.s, %5/z, [%1, %2.s, uxtw]
     [?w, rk,                  0, Ui1, Ui1, Upl, UplDnm] ^
     [&w, rk,                  w, Z,   i,   Upl, UplDnm] ld1<ANY_EXTEND:s><SVE_4BHI:Vesize>\t%0.s, %5/z, [%1, %2.s, sxtw %p4]
     [?w, rk,                  0, Z,   i,   Upl, UplDnm] ^
     [&w, rk,                  w, Ui1, i,   Upl, UplDnm] ld1<ANY_EXTEND:s><SVE_4BHI:Vesize>\t%0.s, %5/z, [%1, %2.s, uxtw %p4]
     [?w, rk,                  0, Ui1, i,   Upl, UplDnm] ^
  }
  "&& !CONSTANT_P (operands[6])"
  {
    operands[6] = CONSTM1_RTX (VNx4BImode);
  }
)

;; Predicated extending gather loads for 64-bit elements.  The value of
;; operand 3 doesn't matter in this case.
(define_insn_and_rewrite "@aarch64_gather_load_<ANY_EXTEND:optab><SVE_2HSDI:mode><SVE_2BHSI:mode>"
  [(set (match_operand:SVE_2HSDI 0 "register_operand")
	(unspec:SVE_2HSDI
	  [(match_operand:VNx2BI 6 "general_operand")
	   (ANY_EXTEND:SVE_2HSDI
	     (unspec:SVE_2BHSI
	       [(match_operand:VNx2BI 5 "register_operand")
		(match_operand:DI 1 "aarch64_sve_gather_offset_<SVE_2BHSI:Vesize>")
		(match_operand:VNx2DI 2 "register_operand")
		(match_operand:DI 3 "const_int_operand")
		(match_operand:DI 4 "aarch64_gather_scale_operand_<SVE_2BHSI:Vesize>")
		(match_operand:SVE_2BHSI 7 "aarch64_maskload_else_operand")
		(mem:BLK (scratch))]
	       UNSPEC_LD1_GATHER))]
	  UNSPEC_PRED_X))]
  "TARGET_SVE
   && TARGET_NON_STREAMING
   && (~<SVE_2HSDI:narrower_mask> & <SVE_2BHSI:self_mask>) == 0"
  {@ [cons: =0, 1, 2, 3, 4, 5, 6]
     [&w, Z,                    w, i, Ui1, Upl, UplDnm] ld1<ANY_EXTEND:s><SVE_2BHSI:Vesize>\t%0.d, %5/z, [%2.d]
     [?w, Z,                    0, i, Ui1, Upl, UplDnm] ^
     [&w, vg<SVE_2BHSI:Vesize>, w, i, Ui1, Upl, UplDnm] ld1<ANY_EXTEND:s><SVE_2BHSI:Vesize>\t%0.d, %5/z, [%2.d, #%1]
     [?w, vg<SVE_2BHSI:Vesize>, 0, i, Ui1, Upl, UplDnm] ^
     [&w, rk,                   w, i, Ui1, Upl, UplDnm] ld1<ANY_EXTEND:s><SVE_2BHSI:Vesize>\t%0.d, %5/z, [%1, %2.d]
     [?w, rk,                   0, i, Ui1, Upl, UplDnm] ^
     [&w, rk,                   w, i, i,   Upl, UplDnm] ld1<ANY_EXTEND:s><SVE_2BHSI:Vesize>\t%0.d, %5/z, [%1, %2.d, lsl %p4]
     [?w, rk,                   0, i, i,   Upl, UplDnm] ^
  }
  "&& !CONSTANT_P (operands[6])"
  {
    operands[6] = CONSTM1_RTX (VNx2BImode);
  }
)

;; Likewise, but with the offset being extended from 32 bits.
(define_insn_and_rewrite "*aarch64_gather_load_<ANY_EXTEND:optab><SVE_2HSDI:mode><SVE_2BHSI:mode>_<ANY_EXTEND2:su>xtw_unpacked"
  [(set (match_operand:SVE_2HSDI 0 "register_operand")
	(unspec:SVE_2HSDI
	  [(match_operand 6)
	   (ANY_EXTEND:SVE_2HSDI
	     (unspec:SVE_2BHSI
	       [(match_operand:VNx2BI 5 "register_operand")
		(match_operand:DI 1 "aarch64_reg_or_zero")
		(unspec:VNx2DI
		  [(match_operand 7)
		   (ANY_EXTEND2:VNx2DI
		     (match_operand:VNx2SI 2 "register_operand"))]
		  UNSPEC_PRED_X)
		(match_operand:DI 3 "const_int_operand")
		(match_operand:DI 4 "aarch64_gather_scale_operand_<SVE_2BHSI:Vesize>")
		(match_operand:SVE_2BHSI 8 "aarch64_maskload_else_operand")
		(mem:BLK (scratch))]
	       UNSPEC_LD1_GATHER))]
	  UNSPEC_PRED_X))]
  "TARGET_SVE
   && TARGET_NON_STREAMING
   && (~<SVE_2HSDI:narrower_mask> & <SVE_2BHSI:self_mask>) == 0"
  {@ [cons: =0, 1, 2, 3, 4, 5]
     [&w, rk, w, i, Ui1, Upl ] ld1<ANY_EXTEND:s><SVE_2BHSI:Vesize>\t%0.d, %5/z, [%1, %2.d, <ANY_EXTEND2:su>xtw]
     [?w, rk, 0, i, Ui1, Upl ] ^
     [&w, rk, w, i, i,   Upl ] ld1<ANY_EXTEND:s><SVE_2BHSI:Vesize>\t%0.d, %5/z, [%1, %2.d, <ANY_EXTEND2:su>xtw %p4]
     [?w, rk, 0, i, i,   Upl ] ^
  }
  "&& (!CONSTANT_P (operands[6]) || !CONSTANT_P (operands[7]))"
  {
    operands[6] = CONSTM1_RTX (VNx2BImode);
    operands[7] = CONSTM1_RTX (VNx2BImode);
  }
)

;; Likewise, but with the offset being truncated to 32 bits and then
;; sign-extended.
(define_insn_and_rewrite "*aarch64_gather_load_<ANY_EXTEND:optab><SVE_2HSDI:mode><SVE_2BHSI:mode>_sxtw"
  [(set (match_operand:SVE_2HSDI 0 "register_operand")
	(unspec:SVE_2HSDI
	  [(match_operand 6)
	   (ANY_EXTEND:SVE_2HSDI
	     (unspec:SVE_2BHSI
	       [(match_operand:VNx2BI 5 "register_operand")
		(match_operand:DI 1 "aarch64_reg_or_zero")
		(unspec:VNx2DI
		  [(match_operand 7)
		   (sign_extend:VNx2DI
		     (truncate:VNx2SI
		       (match_operand:VNx2DI 2 "register_operand")))]
		  UNSPEC_PRED_X)
		(match_operand:DI 3 "const_int_operand")
		(match_operand:DI 4 "aarch64_gather_scale_operand_<SVE_2BHSI:Vesize>")
		(match_operand:SVE_2BHSI 8 "aarch64_maskload_else_operand")
		(mem:BLK (scratch))]
	       UNSPEC_LD1_GATHER))]
	  UNSPEC_PRED_X))]
  "TARGET_SVE
   && TARGET_NON_STREAMING
   && (~<SVE_2HSDI:narrower_mask> & <SVE_2BHSI:self_mask>) == 0"
  {@ [cons: =0, 1, 2, 3, 4, 5]
     [&w, rk, w, i, Ui1, Upl ] ld1<ANY_EXTEND:s><SVE_2BHSI:Vesize>\t%0.d, %5/z, [%1, %2.d, sxtw]
     [?w, rk, 0, i, Ui1, Upl ] ^
     [&w, rk, w, i, i,   Upl ] ld1<ANY_EXTEND:s><SVE_2BHSI:Vesize>\t%0.d, %5/z, [%1, %2.d, sxtw %p4]
     [?w, rk, 0, i, i,   Upl ] ^
  }
  "&& (!CONSTANT_P (operands[6]) || !CONSTANT_P (operands[7]))"
  {
    operands[6] = CONSTM1_RTX (VNx2BImode);
    operands[7] = CONSTM1_RTX (VNx2BImode);
  }
)

;; Likewise, but with the offset being truncated to 32 bits and then
;; zero-extended.
(define_insn_and_rewrite "*aarch64_gather_load_<ANY_EXTEND:optab><SVE_2HSDI:mode><SVE_2BHSI:mode>_uxtw"
  [(set (match_operand:SVE_2HSDI 0 "register_operand")
	(unspec:SVE_2HSDI
	  [(match_operand 7)
	   (ANY_EXTEND:SVE_2HSDI
	     (unspec:SVE_2BHSI
	       [(match_operand:VNx2BI 5 "register_operand")
		(match_operand:DI 1 "aarch64_reg_or_zero")
		(and:VNx2DI
		  (match_operand:VNx2DI 2 "register_operand")
		  (match_operand:VNx2DI 6 "aarch64_sve_uxtw_immediate"))
		(match_operand:DI 3 "const_int_operand")
		(match_operand:DI 4 "aarch64_gather_scale_operand_<SVE_2BHSI:Vesize>")
		(match_operand:SVE_2BHSI 8 "aarch64_maskload_else_operand")
		(mem:BLK (scratch))]
	       UNSPEC_LD1_GATHER))]
	  UNSPEC_PRED_X))]
  "TARGET_SVE
   && TARGET_NON_STREAMING
   && (~<SVE_2HSDI:narrower_mask> & <SVE_2BHSI:self_mask>) == 0"
  {@ [cons: =0, 1, 2, 3, 4, 5]
     [&w, rk, w, i, Ui1, Upl ] ld1<ANY_EXTEND:s><SVE_2BHSI:Vesize>\t%0.d, %5/z, [%1, %2.d, uxtw]
     [?w, rk, 0, i, Ui1, Upl ] ^
     [&w, rk, w, i, i,   Upl ] ld1<ANY_EXTEND:s><SVE_2BHSI:Vesize>\t%0.d, %5/z, [%1, %2.d, uxtw %p4]
     [?w, rk, 0, i, i,   Upl ] ^
  }
  "&& !CONSTANT_P (operands[7])"
  {
    operands[7] = CONSTM1_RTX (VNx2BImode);
  }
)

;; -------------------------------------------------------------------------
;; ---- First-faulting gather loads
;; -------------------------------------------------------------------------
;; Includes gather forms of:
;; - LDFF1D
;; - LDFF1W
;; -------------------------------------------------------------------------

;; Predicated first-faulting gather loads for 32-bit elements.  Operand
;; 3 is true for unsigned extension and false for signed extension.
(define_insn "@aarch64_ldff1_gather<mode>"
  [(set (match_operand:SVE_FULL_S 0 "register_operand")
	(unspec:SVE_FULL_S
	  [(match_operand:VNx4BI 5 "register_operand")
	   (match_operand:DI 1 "aarch64_sve_gather_offset_w")
	   (match_operand:VNx4SI 2 "register_operand")
	   (match_operand:DI 3 "const_int_operand")
	   (match_operand:DI 4 "aarch64_gather_scale_operand_w")
	   (mem:BLK (scratch))
	   (reg:VNx16BI FFRT_REGNUM)]
	  UNSPEC_LDFF1_GATHER))]
  "TARGET_SVE && TARGET_NON_STREAMING"
  {@ [cons: =0, 1, 2, 3, 4, 5  ]
     [&w, Z,   w, i,   Ui1, Upl] ldff1w\t%0.s, %5/z, [%2.s]
     [?w, Z,   0, i,   Ui1, Upl] ^
     [&w, vgw, w, i,   Ui1, Upl] ldff1w\t%0.s, %5/z, [%2.s, #%1]
     [?w, vgw, 0, i,   Ui1, Upl] ^
     [&w, rk,  w, Z,   Ui1, Upl] ldff1w\t%0.s, %5/z, [%1, %2.s, sxtw]
     [?w, rk,  0, Z,   Ui1, Upl] ^
     [&w, rk,  w, Ui1, Ui1, Upl] ldff1w\t%0.s, %5/z, [%1, %2.s, uxtw]
     [?w, rk,  0, Ui1, Ui1, Upl] ^
     [&w, rk,  w, Z,   i,   Upl] ldff1w\t%0.s, %5/z, [%1, %2.s, sxtw %p4]
     [?w, rk,  0, Z,   i,   Upl] ^
     [&w, rk,  w, Ui1, i,   Upl] ldff1w\t%0.s, %5/z, [%1, %2.s, uxtw %p4]
     [?w, rk,  0, Ui1, i,   Upl] ^
  }
)

;; Predicated first-faulting gather loads for 64-bit elements.  The value
;; of operand 3 doesn't matter in this case.
(define_insn "@aarch64_ldff1_gather<mode>"
  [(set (match_operand:SVE_FULL_D 0 "register_operand")
	(unspec:SVE_FULL_D
	  [(match_operand:VNx2BI 5 "register_operand")
	   (match_operand:DI 1 "aarch64_sve_gather_offset_d")
	   (match_operand:VNx2DI 2 "register_operand")
	   (match_operand:DI 3 "const_int_operand")
	   (match_operand:DI 4 "aarch64_gather_scale_operand_d")
	   (mem:BLK (scratch))
	   (reg:VNx16BI FFRT_REGNUM)]
	  UNSPEC_LDFF1_GATHER))]
  "TARGET_SVE && TARGET_NON_STREAMING"
  {@ [cons: =0, 1, 2, 3, 4, 5 ]
     [&w, Z,   w, i, Ui1, Upl ] ldff1d\t%0.d, %5/z, [%2.d]
     [?w, Z,   0, i, Ui1, Upl ] ^
     [&w, vgd, w, i, Ui1, Upl ] ldff1d\t%0.d, %5/z, [%2.d, #%1]
     [?w, vgd, 0, i, Ui1, Upl ] ^
     [&w, rk,  w, i, Ui1, Upl ] ldff1d\t%0.d, %5/z, [%1, %2.d]
     [?w, rk,  0, i, Ui1, Upl ] ^
     [&w, rk,  w, i, i,   Upl ] ldff1d\t%0.d, %5/z, [%1, %2.d, lsl %p4]
     [?w, rk,  0, i, i,   Upl ] ^
  }
)

;; Likewise, but with the offset being sign-extended from 32 bits.
(define_insn_and_rewrite "*aarch64_ldff1_gather<mode>_sxtw"
  [(set (match_operand:SVE_FULL_D 0 "register_operand")
	(unspec:SVE_FULL_D
	  [(match_operand:VNx2BI 5 "register_operand")
	   (match_operand:DI 1 "register_operand")
	   (unspec:VNx2DI
	     [(match_operand 6)
	      (sign_extend:VNx2DI
		(truncate:VNx2SI
		  (match_operand:VNx2DI 2 "register_operand")))]
	     UNSPEC_PRED_X)
	   (match_operand:DI 3 "const_int_operand")
	   (match_operand:DI 4 "aarch64_gather_scale_operand_d")
	   (mem:BLK (scratch))
	   (reg:VNx16BI FFRT_REGNUM)]
	  UNSPEC_LDFF1_GATHER))]
  "TARGET_SVE && TARGET_NON_STREAMING"
  {@ [cons: =0, 1, 2, 3, 4, 5]
     [&w, rk, w, i, Ui1, Upl ] ldff1d\t%0.d, %5/z, [%1, %2.d, sxtw]
     [?w, rk, 0, i, Ui1, Upl ] ^
     [&w, rk, w, i, i,   Upl ] ldff1d\t%0.d, %5/z, [%1, %2.d, sxtw %p4]
     [?w, rk, 0, i, i,   Upl ] ^
  }
  "&& !CONSTANT_P (operands[6])"
  {
    operands[6] = CONSTM1_RTX (VNx2BImode);
  }
)

;; Likewise, but with the offset being zero-extended from 32 bits.
(define_insn "*aarch64_ldff1_gather<mode>_uxtw"
  [(set (match_operand:SVE_FULL_D 0 "register_operand")
	(unspec:SVE_FULL_D
	  [(match_operand:VNx2BI 5 "register_operand")
	   (match_operand:DI 1 "register_operand")
	   (and:VNx2DI
	     (match_operand:VNx2DI 2 "register_operand")
	     (match_operand:VNx2DI 6 "aarch64_sve_uxtw_immediate"))
	   (match_operand:DI 3 "const_int_operand")
	   (match_operand:DI 4 "aarch64_gather_scale_operand_d")
	   (mem:BLK (scratch))
	   (reg:VNx16BI FFRT_REGNUM)]
	  UNSPEC_LDFF1_GATHER))]
  "TARGET_SVE && TARGET_NON_STREAMING"
  {@ [cons: =0, 1, 2, 3, 4, 5]
     [&w, rk, w, i, Ui1, Upl ] ldff1d\t%0.d, %5/z, [%1, %2.d, uxtw]
     [?w, rk, 0, i, Ui1, Upl ] ^
     [&w, rk, w, i, i,   Upl ] ldff1d\t%0.d, %5/z, [%1, %2.d, uxtw %p4]
     [?w, rk, 0, i, i,   Upl ] ^
  }
)

;; -------------------------------------------------------------------------
;; ---- First-faulting extending gather loads
;; -------------------------------------------------------------------------
;; Includes gather forms of:
;; - LDFF1B
;; - LDFF1H
;; - LDFF1SB
;; - LDFF1SH
;; - LDFF1SW
;; - LDFF1W
;; -------------------------------------------------------------------------

;; Predicated extending first-faulting gather loads for 32-bit elements.
;; Operand 3 is true for unsigned extension and false for signed extension.
(define_insn_and_rewrite "@aarch64_ldff1_gather_<ANY_EXTEND:optab><VNx4_WIDE:mode><VNx4_NARROW:mode>"
  [(set (match_operand:VNx4_WIDE 0 "register_operand")
	(unspec:VNx4_WIDE
	  [(match_operand:VNx4BI 6 "general_operand")
	   (ANY_EXTEND:VNx4_WIDE
	     (unspec:VNx4_NARROW
	       [(match_operand:VNx4BI 5 "register_operand")
		(match_operand:DI 1 "aarch64_sve_gather_offset_<VNx4_NARROW:Vesize>")
		(match_operand:VNx4_WIDE 2 "register_operand")
		(match_operand:DI 3 "const_int_operand")
		(match_operand:DI 4 "aarch64_gather_scale_operand_<VNx4_NARROW:Vesize>")
		(mem:BLK (scratch))
		(reg:VNx16BI FFRT_REGNUM)]
	       UNSPEC_LDFF1_GATHER))]
	  UNSPEC_PRED_X))]
  "TARGET_SVE && TARGET_NON_STREAMING"
  {@ [cons: =0, 1, 2, 3, 4, 5, 6]
     [&w, Z,                      w, i,   Ui1, Upl, UplDnm] ldff1<ANY_EXTEND:s><VNx4_NARROW:Vesize>\t%0.s, %5/z, [%2.s]
     [?w, Z,                      0, i,   Ui1, Upl, UplDnm] ^
     [&w, vg<VNx4_NARROW:Vesize>, w, i,   Ui1, Upl, UplDnm] ldff1<ANY_EXTEND:s><VNx4_NARROW:Vesize>\t%0.s, %5/z, [%2.s, #%1]
     [?w, vg<VNx4_NARROW:Vesize>, 0, i,   Ui1, Upl, UplDnm] ^
     [&w, rk,                     w, Z,   Ui1, Upl, UplDnm] ldff1<ANY_EXTEND:s><VNx4_NARROW:Vesize>\t%0.s, %5/z, [%1, %2.s, sxtw]
     [?w, rk,                     0, Z,   Ui1, Upl, UplDnm] ^
     [&w, rk,                     w, Ui1, Ui1, Upl, UplDnm] ldff1<ANY_EXTEND:s><VNx4_NARROW:Vesize>\t%0.s, %5/z, [%1, %2.s, uxtw]
     [?w, rk,                     0, Ui1, Ui1, Upl, UplDnm] ^
     [&w, rk,                     w, Z,   i,   Upl, UplDnm] ldff1<ANY_EXTEND:s><VNx4_NARROW:Vesize>\t%0.s, %5/z, [%1, %2.s, sxtw %p4]
     [?w, rk,                     0, Z,   i,   Upl, UplDnm] ^
     [&w, rk,                     w, Ui1, i,   Upl, UplDnm] ldff1<ANY_EXTEND:s><VNx4_NARROW:Vesize>\t%0.s, %5/z, [%1, %2.s, uxtw %p4]
     [?w, rk,                     0, Ui1, i,   Upl, UplDnm] ^
  }
  "&& !CONSTANT_P (operands[6])"
  {
    operands[6] = CONSTM1_RTX (VNx4BImode);
  }
)

;; Predicated extending first-faulting gather loads for 64-bit elements.
;; The value of operand 3 doesn't matter in this case.
(define_insn_and_rewrite "@aarch64_ldff1_gather_<ANY_EXTEND:optab><VNx2_WIDE:mode><VNx2_NARROW:mode>"
  [(set (match_operand:VNx2_WIDE 0 "register_operand")
	(unspec:VNx2_WIDE
	  [(match_operand:VNx2BI 6 "general_operand")
	   (ANY_EXTEND:VNx2_WIDE
	     (unspec:VNx2_NARROW
	       [(match_operand:VNx2BI 5 "register_operand")
		(match_operand:DI 1 "aarch64_sve_gather_offset_<VNx2_NARROW:Vesize>")
		(match_operand:VNx2_WIDE 2 "register_operand")
		(match_operand:DI 3 "const_int_operand")
		(match_operand:DI 4 "aarch64_gather_scale_operand_<VNx2_NARROW:Vesize>")
		(mem:BLK (scratch))
		(reg:VNx16BI FFRT_REGNUM)]
	       UNSPEC_LDFF1_GATHER))]
	  UNSPEC_PRED_X))]
  "TARGET_SVE && TARGET_NON_STREAMING"
  {@ [cons: =0, 1, 2, 3, 4, 5, 6]
     [&w, Z,                      w, i, Ui1, Upl, UplDnm] ldff1<ANY_EXTEND:s><VNx2_NARROW:Vesize>\t%0.d, %5/z, [%2.d]
     [?w, Z,                      0, i, Ui1, Upl, UplDnm] ^
     [&w, vg<VNx2_NARROW:Vesize>, w, i, Ui1, Upl, UplDnm] ldff1<ANY_EXTEND:s><VNx2_NARROW:Vesize>\t%0.d, %5/z, [%2.d, #%1]
     [?w, vg<VNx2_NARROW:Vesize>, 0, i, Ui1, Upl, UplDnm] ^
     [&w, rk,                     w, i, Ui1, Upl, UplDnm] ldff1<ANY_EXTEND:s><VNx2_NARROW:Vesize>\t%0.d, %5/z, [%1, %2.d]
     [?w, rk,                     0, i, Ui1, Upl, UplDnm] ^
     [&w, rk,                     w, i, i,   Upl, UplDnm] ldff1<ANY_EXTEND:s><VNx2_NARROW:Vesize>\t%0.d, %5/z, [%1, %2.d, lsl %p4]
     [?w, rk,                     w, i, i,   Upl, UplDnm] ^
  }
  "&& !CONSTANT_P (operands[6])"
  {
    operands[6] = CONSTM1_RTX (VNx2BImode);
  }
)

;; Likewise, but with the offset being sign-extended from 32 bits.
(define_insn_and_rewrite "*aarch64_ldff1_gather_<ANY_EXTEND:optab><VNx2_WIDE:mode><VNx2_NARROW:mode>_sxtw"
  [(set (match_operand:VNx2_WIDE 0 "register_operand")
	(unspec:VNx2_WIDE
	  [(match_operand 6)
	   (ANY_EXTEND:VNx2_WIDE
	     (unspec:VNx2_NARROW
	       [(match_operand:VNx2BI 5 "register_operand")
		(match_operand:DI 1 "aarch64_reg_or_zero")
		(unspec:VNx2DI
		  [(match_operand 7)
		   (sign_extend:VNx2DI
		     (truncate:VNx2SI
		       (match_operand:VNx2DI 2 "register_operand")))]
		  UNSPEC_PRED_X)
		(match_operand:DI 3 "const_int_operand")
		(match_operand:DI 4 "aarch64_gather_scale_operand_<VNx2_NARROW:Vesize>")
		(mem:BLK (scratch))
		(reg:VNx16BI FFRT_REGNUM)]
	       UNSPEC_LDFF1_GATHER))]
	  UNSPEC_PRED_X))]
  "TARGET_SVE && TARGET_NON_STREAMING"
  {@ [cons: =0, 1, 2, 3, 4, 5]
     [&w, rk, w, i, Ui1, Upl ] ldff1<ANY_EXTEND:s><VNx2_NARROW:Vesize>\t%0.d, %5/z, [%1, %2.d, sxtw]
     [?w, rk, 0, i, Ui1, Upl ] ^
     [&w, rk, w, i, i,   Upl ] ldff1<ANY_EXTEND:s><VNx2_NARROW:Vesize>\t%0.d, %5/z, [%1, %2.d, sxtw %p4]
     [?w, rk, 0, i, i,   Upl ] ^
  }
  "&& (!CONSTANT_P (operands[6]) || !CONSTANT_P (operands[7]))"
  {
    operands[6] = CONSTM1_RTX (VNx2BImode);
    operands[7] = CONSTM1_RTX (VNx2BImode);
  }
)

;; Likewise, but with the offset being zero-extended from 32 bits.
(define_insn_and_rewrite "*aarch64_ldff1_gather_<ANY_EXTEND:optab><VNx2_WIDE:mode><VNx2_NARROW:mode>_uxtw"
  [(set (match_operand:VNx2_WIDE 0 "register_operand")
	(unspec:VNx2_WIDE
	  [(match_operand 7)
	   (ANY_EXTEND:VNx2_WIDE
	     (unspec:VNx2_NARROW
	       [(match_operand:VNx2BI 5 "register_operand")
		(match_operand:DI 1 "aarch64_reg_or_zero")
		(and:VNx2DI
		  (match_operand:VNx2DI 2 "register_operand")
		  (match_operand:VNx2DI 6 "aarch64_sve_uxtw_immediate"))
		(match_operand:DI 3 "const_int_operand")
		(match_operand:DI 4 "aarch64_gather_scale_operand_<VNx2_NARROW:Vesize>")
		(mem:BLK (scratch))
		(reg:VNx16BI FFRT_REGNUM)]
	       UNSPEC_LDFF1_GATHER))]
	  UNSPEC_PRED_X))]
  "TARGET_SVE && TARGET_NON_STREAMING"
  {@ [cons: =0, 1, 2, 3, 4, 5]
     [&w, rk, w, i, Ui1, Upl ] ldff1<ANY_EXTEND:s><VNx2_NARROW:Vesize>\t%0.d, %5/z, [%1, %2.d, uxtw]
     [?w, rk, 0, i, Ui1, Upl ] ^
     [&w, rk, w, i, i,   Upl ] ldff1<ANY_EXTEND:s><VNx2_NARROW:Vesize>\t%0.d, %5/z, [%1, %2.d, uxtw %p4]
     [?w, rk, 0, i, i,   Upl ] ^
  }
  "&& !CONSTANT_P (operands[7])"
  {
    operands[7] = CONSTM1_RTX (VNx2BImode);
  }
)

;; =========================================================================
;; == Prefetches
;; =========================================================================

;; -------------------------------------------------------------------------
;; ---- Contiguous prefetches
;; -------------------------------------------------------------------------
;; Includes contiguous forms of:
;; - PRFB
;; - PRFD
;; - PRFH
;; - PRFW
;; -------------------------------------------------------------------------

;; Contiguous predicated prefetches.  Operand 2 gives the real prefetch
;; operation (as an svprfop), with operands 3 and 4 providing distilled
;; information.
(define_insn "@aarch64_sve_prefetch<mode>"
  [(prefetch (unspec:DI
	       [(match_operand:<VPRED> 0 "register_operand" "Upl")
		(match_operand:SVE_FULL_I 1 "aarch64_sve_prefetch_operand" "UP<Vesize>")
		(match_operand:DI 2 "const_int_operand")]
	       UNSPEC_SVE_PREFETCH)
	     (match_operand:DI 3 "const_int_operand")
	     (match_operand:DI 4 "const_int_operand"))]
  "TARGET_SVE"
  {
    operands[1] = gen_rtx_MEM (<MODE>mode, operands[1]);
    return aarch64_output_sve_prefetch ("prf<Vesize>", operands[2], "%0, %1");
  }
)

;; -------------------------------------------------------------------------
;; ---- Gather prefetches
;; -------------------------------------------------------------------------
;; Includes gather forms of:
;; - PRFB
;; - PRFD
;; - PRFH
;; - PRFW
;; -------------------------------------------------------------------------

;; Predicated gather prefetches for 32-bit bases and offsets.  The operands
;; are:
;; 0: the governing predicate
;; 1: the scalar component of the address
;; 2: the vector component of the address
;; 3: 1 for zero extension, 0 for sign extension
;; 4: the scale multiplier
;; 5: a vector zero that identifies the mode of data being accessed
;; 6: the prefetch operator (an svprfop)
;; 7: the normal RTL prefetch rw flag
;; 8: the normal RTL prefetch locality value
(define_insn "@aarch64_sve_gather_prefetch<SVE_FULL_I:mode><VNx4SI_ONLY:mode>"
  [(prefetch (unspec:DI
	       [(match_operand:VNx4BI 0 "register_operand" "Upl, Upl, Upl, Upl, Upl, Upl")
		(match_operand:DI 1 "aarch64_sve_gather_offset_<SVE_FULL_I:Vesize>" "Z, vg<SVE_FULL_I:Vesize>, rk, rk, rk, rk")
		(match_operand:VNx4SI_ONLY 2 "register_operand" "w, w, w, w, w, w")
		(match_operand:DI 3 "const_int_operand" "i, i, Z, Ui1, Z, Ui1")
		(match_operand:DI 4 "aarch64_gather_scale_operand_<SVE_FULL_I:Vesize>" "Ui1, Ui1, Ui1, Ui1, i, i")
		(match_operand:SVE_FULL_I 5 "aarch64_simd_imm_zero")
		(match_operand:DI 6 "const_int_operand")]
	       UNSPEC_SVE_PREFETCH_GATHER)
	     (match_operand:DI 7 "const_int_operand")
	     (match_operand:DI 8 "const_int_operand"))]
  "TARGET_SVE && TARGET_NON_STREAMING"
  {
    static const char *const insns[][2] = {
      "prf<SVE_FULL_I:Vesize>", "%0, [%2.s]",
      "prf<SVE_FULL_I:Vesize>", "%0, [%2.s, #%1]",
      "prfb", "%0, [%1, %2.s, sxtw]",
      "prfb", "%0, [%1, %2.s, uxtw]",
      "prf<SVE_FULL_I:Vesize>", "%0, [%1, %2.s, sxtw %p4]",
      "prf<SVE_FULL_I:Vesize>", "%0, [%1, %2.s, uxtw %p4]"
    };
    const char *const *parts = insns[which_alternative];
    return aarch64_output_sve_prefetch (parts[0], operands[6], parts[1]);
  }
)

;; Predicated gather prefetches for 64-bit elements.  The value of operand 3
;; doesn't matter in this case.
(define_insn "@aarch64_sve_gather_prefetch<SVE_FULL_I:mode><VNx2DI_ONLY:mode>"
  [(prefetch (unspec:DI
	       [(match_operand:VNx2BI 0 "register_operand" "Upl, Upl, Upl, Upl")
		(match_operand:DI 1 "aarch64_sve_gather_offset_<SVE_FULL_I:Vesize>" "Z, vg<SVE_FULL_I:Vesize>, rk, rk")
		(match_operand:VNx2DI_ONLY 2 "register_operand" "w, w, w, w")
		(match_operand:DI 3 "const_int_operand")
		(match_operand:DI 4 "aarch64_gather_scale_operand_<SVE_FULL_I:Vesize>" "Ui1, Ui1, Ui1, i")
		(match_operand:SVE_FULL_I 5 "aarch64_simd_imm_zero")
		(match_operand:DI 6 "const_int_operand")]
	       UNSPEC_SVE_PREFETCH_GATHER)
	     (match_operand:DI 7 "const_int_operand")
	     (match_operand:DI 8 "const_int_operand"))]
  "TARGET_SVE && TARGET_NON_STREAMING"
  {
    static const char *const insns[][2] = {
      "prf<SVE_FULL_I:Vesize>", "%0, [%2.d]",
      "prf<SVE_FULL_I:Vesize>", "%0, [%2.d, #%1]",
      "prfb", "%0, [%1, %2.d]",
      "prf<SVE_FULL_I:Vesize>", "%0, [%1, %2.d, lsl %p4]"
    };
    const char *const *parts = insns[which_alternative];
    return aarch64_output_sve_prefetch (parts[0], operands[6], parts[1]);
  }
)

;; Likewise, but with the offset being sign-extended from 32 bits.
(define_insn_and_rewrite "*aarch64_sve_gather_prefetch<SVE_FULL_I:mode><VNx2DI_ONLY:mode>_sxtw"
  [(prefetch (unspec:DI
	       [(match_operand:VNx2BI 0 "register_operand" "Upl, Upl")
		(match_operand:DI 1 "register_operand" "rk, rk")
		(unspec:VNx2DI_ONLY
		  [(match_operand 9)
		   (sign_extend:VNx2DI
		     (truncate:VNx2SI
		       (match_operand:VNx2DI 2 "register_operand" "w, w")))]
		  UNSPEC_PRED_X)
		(match_operand:DI 3 "const_int_operand")
		(match_operand:DI 4 "aarch64_gather_scale_operand_<SVE_FULL_I:Vesize>" "Ui1, i")
		(match_operand:SVE_FULL_I 5 "aarch64_simd_imm_zero")
		(match_operand:DI 6 "const_int_operand")]
	       UNSPEC_SVE_PREFETCH_GATHER)
	     (match_operand:DI 7 "const_int_operand")
	     (match_operand:DI 8 "const_int_operand"))]
  "TARGET_SVE && TARGET_NON_STREAMING"
  {
    static const char *const insns[][2] = {
      "prfb", "%0, [%1, %2.d, sxtw]",
      "prf<SVE_FULL_I:Vesize>", "%0, [%1, %2.d, sxtw %p4]"
    };
    const char *const *parts = insns[which_alternative];
    return aarch64_output_sve_prefetch (parts[0], operands[6], parts[1]);
  }
  "&& !rtx_equal_p (operands[0], operands[9])"
  {
    operands[9] = copy_rtx (operands[0]);
  }
)

;; Likewise, but with the offset being zero-extended from 32 bits.
(define_insn "*aarch64_sve_gather_prefetch<SVE_FULL_I:mode><VNx2DI_ONLY:mode>_uxtw"
  [(prefetch (unspec:DI
	       [(match_operand:VNx2BI 0 "register_operand" "Upl, Upl")
		(match_operand:DI 1 "register_operand" "rk, rk")
		(and:VNx2DI_ONLY
		  (match_operand:VNx2DI 2 "register_operand" "w, w")
		  (match_operand:VNx2DI 9 "aarch64_sve_uxtw_immediate"))
		(match_operand:DI 3 "const_int_operand")
		(match_operand:DI 4 "aarch64_gather_scale_operand_<SVE_FULL_I:Vesize>" "Ui1, i")
		(match_operand:SVE_FULL_I 5 "aarch64_simd_imm_zero")
		(match_operand:DI 6 "const_int_operand")]
	       UNSPEC_SVE_PREFETCH_GATHER)
	     (match_operand:DI 7 "const_int_operand")
	     (match_operand:DI 8 "const_int_operand"))]
  "TARGET_SVE && TARGET_NON_STREAMING"
  {
    static const char *const insns[][2] = {
      "prfb", "%0, [%1, %2.d, uxtw]",
      "prf<SVE_FULL_I:Vesize>", "%0, [%1, %2.d, uxtw %p4]"
    };
    const char *const *parts = insns[which_alternative];
    return aarch64_output_sve_prefetch (parts[0], operands[6], parts[1]);
  }
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

;; Predicated ST1 (single).
(define_insn "maskstore<mode><vpred>"
  [(set (match_operand:SVE_ALL 0 "memory_operand" "+m")
	(unspec:SVE_ALL
	  [(match_operand:<VPRED> 2 "register_operand" "Upl")
	   (match_operand:SVE_ALL 1 "register_operand" "w")
	   (match_dup 0)]
	  UNSPEC_ST1_SVE))]
  "TARGET_SVE"
  "st1<Vesize>\t%1.<Vctype>, %2, %0"
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
;; ---- Truncating contiguous stores
;; -------------------------------------------------------------------------
;; Includes:
;; - ST1B
;; - ST1H
;; - ST1W
;; -------------------------------------------------------------------------

;; Predicated truncate and store, with 8 elements per 128-bit block.
(define_insn "@aarch64_store_trunc<VNx8_NARROW:mode><VNx8_WIDE:mode>"
  [(set (match_operand:VNx8_NARROW 0 "memory_operand" "+m")
	(unspec:VNx8_NARROW
	  [(match_operand:VNx8BI 2 "register_operand" "Upl")
	   (truncate:VNx8_NARROW
	     (match_operand:VNx8_WIDE 1 "register_operand" "w"))
	   (match_dup 0)]
	  UNSPEC_ST1_SVE))]
  "TARGET_SVE"
  "st1<VNx8_NARROW:Vesize>\t%1.<VNx8_WIDE:Vetype>, %2, %0"
)

;; Predicated truncate and store, with 4 elements per 128-bit block.
(define_insn "@aarch64_store_trunc<VNx4_NARROW:mode><VNx4_WIDE:mode>"
  [(set (match_operand:VNx4_NARROW 0 "memory_operand" "+m")
	(unspec:VNx4_NARROW
	  [(match_operand:VNx4BI 2 "register_operand" "Upl")
	   (truncate:VNx4_NARROW
	     (match_operand:VNx4_WIDE 1 "register_operand" "w"))
	   (match_dup 0)]
	  UNSPEC_ST1_SVE))]
  "TARGET_SVE"
  "st1<VNx4_NARROW:Vesize>\t%1.<VNx4_WIDE:Vetype>, %2, %0"
)

;; Predicated truncate and store, with 2 elements per 128-bit block.
(define_insn "@aarch64_store_trunc<VNx2_NARROW:mode><VNx2_WIDE:mode>"
  [(set (match_operand:VNx2_NARROW 0 "memory_operand" "+m")
	(unspec:VNx2_NARROW
	  [(match_operand:VNx2BI 2 "register_operand" "Upl")
	   (truncate:VNx2_NARROW
	     (match_operand:VNx2_WIDE 1 "register_operand" "w"))
	   (match_dup 0)]
	  UNSPEC_ST1_SVE))]
  "TARGET_SVE"
  "st1<VNx2_NARROW:Vesize>\t%1.<VNx2_WIDE:Vetype>, %2, %0"
)

;; -------------------------------------------------------------------------
;; ---- Non-temporal contiguous stores
;; -------------------------------------------------------------------------
;; Includes:
;; - STNT1B
;; - STNT1D
;; - STNT1H
;; - STNT1W
;; -------------------------------------------------------------------------

(define_insn "@aarch64_stnt1<mode>"
  [(set (match_operand:SVE_FULL 0 "memory_operand" "+m")
	(unspec:SVE_FULL
	  [(match_operand:<VPRED> 2 "register_operand" "Upl")
	   (match_operand:SVE_FULL 1 "register_operand" "w")
	   (match_dup 0)]
	  UNSPEC_STNT1_SVE))]
  "TARGET_SVE"
  "stnt1<Vesize>\t%1.<Vetype>, %2, %0"
)

;; -------------------------------------------------------------------------
;; ---- Normal scatter stores
;; -------------------------------------------------------------------------
;; Includes scatter forms of:
;; - ST1D
;; - ST1W
;; -------------------------------------------------------------------------

;; Unpredicated scatter stores.
(define_expand "scatter_store<mode><v_int_container>"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(match_dup 5)
	   (match_operand:DI 0 "aarch64_sve_gather_offset_<Vesize>")
	   (match_operand:<V_INT_CONTAINER> 1 "register_operand")
	   (match_operand:DI 2 "const_int_operand")
	   (match_operand:DI 3 "aarch64_gather_scale_operand_<Vesize>")
	   (match_operand:SVE_24 4 "register_operand")]
	  UNSPEC_ST1_SCATTER))]
  "TARGET_SVE && TARGET_NON_STREAMING"
  {
    operands[5] = aarch64_ptrue_reg (<VPRED>mode);
  }
)

;; Predicated scatter stores for 32-bit elements.  Operand 2 is true for
;; unsigned extension and false for signed extension.
(define_insn "mask_scatter_store<mode><v_int_container>"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(match_operand:VNx4BI 5 "register_operand")
	   (match_operand:DI 0 "aarch64_sve_gather_offset_<Vesize>")
	   (match_operand:VNx4SI 1 "register_operand")
	   (match_operand:DI 2 "const_int_operand")
	   (match_operand:DI 3 "aarch64_gather_scale_operand_<Vesize>")
	   (match_operand:SVE_4 4 "register_operand")]
	  UNSPEC_ST1_SCATTER))]
  "TARGET_SVE && TARGET_NON_STREAMING"
  {@ [ cons: 0 , 1 , 2   , 3   , 4 , 5    ]
     [ Z       , w , Ui1 , Ui1 , w , Upl  ] st1<Vesize>\t%4.s, %5, [%1.s]
     [ vgw     , w , Ui1 , Ui1 , w , Upl  ] st1<Vesize>\t%4.s, %5, [%1.s, #%0]
     [ rk      , w , Z   , Ui1 , w , Upl  ] st1<Vesize>\t%4.s, %5, [%0, %1.s, sxtw]
     [ rk      , w , Ui1 , Ui1 , w , Upl  ] st1<Vesize>\t%4.s, %5, [%0, %1.s, uxtw]
     [ rk      , w , Z   , i   , w , Upl  ] st1<Vesize>\t%4.s, %5, [%0, %1.s, sxtw %p3]
     [ rk      , w , Ui1 , i   , w , Upl  ] st1<Vesize>\t%4.s, %5, [%0, %1.s, uxtw %p3]
  }
)

;; Predicated scatter stores for 64-bit elements.  The value of operand 2
;; doesn't matter in this case.
(define_insn "mask_scatter_store<mode><v_int_container>"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(match_operand:VNx2BI 5 "register_operand")
	   (match_operand:DI 0 "aarch64_sve_gather_offset_<Vesize>")
	   (match_operand:VNx2DI 1 "register_operand")
	   (match_operand:DI 2 "const_int_operand")
	   (match_operand:DI 3 "aarch64_gather_scale_operand_<Vesize>")
	   (match_operand:SVE_2 4 "register_operand")]
	  UNSPEC_ST1_SCATTER))]
  "TARGET_SVE && TARGET_NON_STREAMING"
  {@ [ cons: 0 , 1 , 3   , 4 , 5    ]
     [ Z       , w , Ui1 , w , Upl  ] st1<Vesize>\t%4.d, %5, [%1.d]
     [ vgd     , w , Ui1 , w , Upl  ] st1<Vesize>\t%4.d, %5, [%1.d, #%0]
     [ rk      , w , Ui1 , w , Upl  ] st1<Vesize>\t%4.d, %5, [%0, %1.d]
     [ rk      , w , i   , w , Upl  ] st1<Vesize>\t%4.d, %5, [%0, %1.d, lsl %p3]
  }
)

;; Likewise, but with the offset being extended from 32 bits.
(define_insn_and_rewrite "*mask_scatter_store<mode><v_int_container>_<su>xtw_unpacked"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(match_operand:VNx2BI 5 "register_operand")
	   (match_operand:DI 0 "register_operand")
	   (unspec:VNx2DI
	     [(match_operand 6)
	      (ANY_EXTEND:VNx2DI
		(match_operand:VNx2SI 1 "register_operand"))]
	     UNSPEC_PRED_X)
	   (match_operand:DI 2 "const_int_operand")
	   (match_operand:DI 3 "aarch64_gather_scale_operand_<Vesize>")
	   (match_operand:SVE_2 4 "register_operand")]
	  UNSPEC_ST1_SCATTER))]
  "TARGET_SVE && TARGET_NON_STREAMING"
  {@ [ cons: 0 , 1 , 3   , 4 , 5    ]
     [ rk      , w , Ui1 , w , Upl  ] st1<Vesize>\t%4.d, %5, [%0, %1.d, <su>xtw]
     [ rk      , w , i   , w , Upl  ] st1<Vesize>\t%4.d, %5, [%0, %1.d, <su>xtw %p3]
  }
  "&& !CONSTANT_P (operands[6])"
  {
    operands[6] = CONSTM1_RTX (<VPRED>mode);
  }
)

;; Likewise, but with the offset being truncated to 32 bits and then
;; sign-extended.
(define_insn_and_rewrite "*mask_scatter_store<mode><v_int_container>_sxtw"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(match_operand:VNx2BI 5 "register_operand")
	   (match_operand:DI 0 "register_operand")
	   (unspec:VNx2DI
	     [(match_operand 6)
	      (sign_extend:VNx2DI
		(truncate:VNx2SI
		  (match_operand:VNx2DI 1 "register_operand")))]
	     UNSPEC_PRED_X)
	   (match_operand:DI 2 "const_int_operand")
	   (match_operand:DI 3 "aarch64_gather_scale_operand_<Vesize>")
	   (match_operand:SVE_2 4 "register_operand")]
	  UNSPEC_ST1_SCATTER))]
  "TARGET_SVE && TARGET_NON_STREAMING"
  {@ [ cons: 0 , 1 , 3   , 4 , 5    ]
     [ rk      , w , Ui1 , w , Upl  ] st1<Vesize>\t%4.d, %5, [%0, %1.d, sxtw]
     [ rk      , w , i   , w , Upl  ] st1<Vesize>\t%4.d, %5, [%0, %1.d, sxtw %p3]
  }
  "&& !CONSTANT_P (operands[6])"
  {
    operands[6] = CONSTM1_RTX (<VPRED>mode);
  }
)

;; Likewise, but with the offset being truncated to 32 bits and then
;; zero-extended.
(define_insn "*mask_scatter_store<mode><v_int_container>_uxtw"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(match_operand:VNx2BI 5 "register_operand")
	   (match_operand:DI 0 "aarch64_reg_or_zero")
	   (and:VNx2DI
	     (match_operand:VNx2DI 1 "register_operand")
	     (match_operand:VNx2DI 6 "aarch64_sve_uxtw_immediate"))
	   (match_operand:DI 2 "const_int_operand")
	   (match_operand:DI 3 "aarch64_gather_scale_operand_<Vesize>")
	   (match_operand:SVE_2 4 "register_operand")]
	  UNSPEC_ST1_SCATTER))]
  "TARGET_SVE && TARGET_NON_STREAMING"
  {@ [ cons: 0 , 1 , 3   , 4 , 5    ]
     [ rk      , w , Ui1 , w , Upl  ] st1<Vesize>\t%4.d, %5, [%0, %1.d, uxtw]
     [ rk      , w , i   , w , Upl  ] st1<Vesize>\t%4.d, %5, [%0, %1.d, uxtw %p3]
  }
)

;; -------------------------------------------------------------------------
;; ---- Truncating scatter stores
;; -------------------------------------------------------------------------
;; Includes scatter forms of:
;; - ST1B
;; - ST1H
;; - ST1W
;; -------------------------------------------------------------------------

;; Predicated truncating scatter stores for 32-bit elements.  Operand 2 is
;; true for unsigned extension and false for signed extension.
(define_insn "@aarch64_scatter_store_trunc<VNx4_NARROW:mode><VNx4_WIDE:mode>"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(match_operand:VNx4BI 5 "register_operand")
	   (match_operand:DI 0 "aarch64_sve_gather_offset_<VNx4_NARROW:Vesize>" "Z, vg<VNx4_NARROW:Vesize>, rk, rk, rk, rk")
	   (match_operand:VNx4SI 1 "register_operand")
	   (match_operand:DI 2 "const_int_operand")
	   (match_operand:DI 3 "aarch64_gather_scale_operand_<VNx4_NARROW:Vesize>" "Ui1, Ui1, Ui1, Ui1, i, i")
	   (truncate:VNx4_NARROW
	     (match_operand:VNx4_WIDE 4 "register_operand"))]
	  UNSPEC_ST1_SCATTER))]
  "TARGET_SVE && TARGET_NON_STREAMING"
  {@ [ cons: 1 , 2   , 4 , 5    ]
     [ w       , Ui1 , w , Upl  ] st1<VNx4_NARROW:Vesize>\t%4.s, %5, [%1.s]
     [ w       , Ui1 , w , Upl  ] st1<VNx4_NARROW:Vesize>\t%4.s, %5, [%1.s, #%0]
     [ w       , Z   , w , Upl  ] st1<VNx4_NARROW:Vesize>\t%4.s, %5, [%0, %1.s, sxtw]
     [ w       , Ui1 , w , Upl  ] st1<VNx4_NARROW:Vesize>\t%4.s, %5, [%0, %1.s, uxtw]
     [ w       , Z   , w , Upl  ] st1<VNx4_NARROW:Vesize>\t%4.s, %5, [%0, %1.s, sxtw %p3]
     [ w       , Ui1 , w , Upl  ] st1<VNx4_NARROW:Vesize>\t%4.s, %5, [%0, %1.s, uxtw %p3]
  }
)

;; Predicated truncating scatter stores for 64-bit elements.  The value of
;; operand 2 doesn't matter in this case.
(define_insn "@aarch64_scatter_store_trunc<VNx2_NARROW:mode><VNx2_WIDE:mode>"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(match_operand:VNx2BI 5 "register_operand")
	   (match_operand:DI 0 "aarch64_sve_gather_offset_<VNx2_NARROW:Vesize>" "Z, vg<VNx2_NARROW:Vesize>, rk, rk")
	   (match_operand:VNx2DI 1 "register_operand")
	   (match_operand:DI 2 "const_int_operand")
	   (match_operand:DI 3 "aarch64_gather_scale_operand_<VNx2_NARROW:Vesize>" "Ui1, Ui1, Ui1, i")
	   (truncate:VNx2_NARROW
	     (match_operand:VNx2_WIDE 4 "register_operand"))]
	  UNSPEC_ST1_SCATTER))]
  "TARGET_SVE && TARGET_NON_STREAMING"
  {@ [ cons: 1 , 4 , 5    ]
     [ w       , w , Upl  ] st1<VNx2_NARROW:Vesize>\t%4.d, %5, [%1.d]
     [ w       , w , Upl  ] st1<VNx2_NARROW:Vesize>\t%4.d, %5, [%1.d, #%0]
     [ w       , w , Upl  ] st1<VNx2_NARROW:Vesize>\t%4.d, %5, [%0, %1.d]
     [ w       , w , Upl  ] st1<VNx2_NARROW:Vesize>\t%4.d, %5, [%0, %1.d, lsl %p3]
  }
)

;; Likewise, but with the offset being sign-extended from 32 bits.
(define_insn_and_rewrite "*aarch64_scatter_store_trunc<VNx2_NARROW:mode><VNx2_WIDE:mode>_sxtw"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(match_operand:VNx2BI 5 "register_operand")
	   (match_operand:DI 0 "register_operand")
	   (unspec:VNx2DI
	     [(match_operand 6)
	      (sign_extend:VNx2DI
		(truncate:VNx2SI
		  (match_operand:VNx2DI 1 "register_operand")))]
	     UNSPEC_PRED_X)
	   (match_operand:DI 2 "const_int_operand")
	   (match_operand:DI 3 "aarch64_gather_scale_operand_<VNx2_NARROW:Vesize>" "Ui1, i")
	   (truncate:VNx2_NARROW
	     (match_operand:VNx2_WIDE 4 "register_operand"))]
	  UNSPEC_ST1_SCATTER))]
  "TARGET_SVE && TARGET_NON_STREAMING"
  {@ [ cons: 0 , 1 , 4 , 5    ]
     [ rk      , w , w , Upl  ] st1<VNx2_NARROW:Vesize>\t%4.d, %5, [%0, %1.d, sxtw]
     [ rk      , w , w , Upl  ] st1<VNx2_NARROW:Vesize>\t%4.d, %5, [%0, %1.d, sxtw %p3]
  }
  "&& !rtx_equal_p (operands[5], operands[6])"
  {
    operands[6] = copy_rtx (operands[5]);
  }
)

;; Likewise, but with the offset being zero-extended from 32 bits.
(define_insn "*aarch64_scatter_store_trunc<VNx2_NARROW:mode><VNx2_WIDE:mode>_uxtw"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(match_operand:VNx2BI 5 "register_operand")
	   (match_operand:DI 0 "aarch64_reg_or_zero")
	   (and:VNx2DI
	     (match_operand:VNx2DI 1 "register_operand")
	     (match_operand:VNx2DI 6 "aarch64_sve_uxtw_immediate"))
	   (match_operand:DI 2 "const_int_operand")
	   (match_operand:DI 3 "aarch64_gather_scale_operand_<VNx2_NARROW:Vesize>" "Ui1, i")
	   (truncate:VNx2_NARROW
	     (match_operand:VNx2_WIDE 4 "register_operand"))]
	  UNSPEC_ST1_SCATTER))]
  "TARGET_SVE && TARGET_NON_STREAMING"
  {@ [ cons: 0 , 1 , 4 , 5    ]
     [ rk      , w , w , Upl  ] st1<VNx2_NARROW:Vesize>\t%4.d, %5, [%0, %1.d, uxtw]
     [ rk      , w , w , Upl  ] st1<VNx2_NARROW:Vesize>\t%4.d, %5, [%0, %1.d, uxtw %p3]
  }
)

;; =========================================================================
;; == Vector creation
;; =========================================================================

;; -------------------------------------------------------------------------
;; ---- [INT,FP] Duplicate element
;; -------------------------------------------------------------------------
;; Includes:
;; - DUP
;; - MOV
;; - LD1RB
;; - LD1RD
;; - LD1RH
;; - LD1RW
;; - LD1ROB (F64MM)
;; - LD1ROD (F64MM)
;; - LD1ROH (F64MM)
;; - LD1ROW (F64MM)
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
  [(set (match_operand:SVE_ALL 0 "register_operand")
	(vec_duplicate:SVE_ALL
	  (match_operand:<VEL> 1 "aarch64_sve_dup_operand")))
   (clobber (match_scratch:VNx16BI 2 "=X, X, Upl"))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   ; attrs: length ]
     [ w        , r   ; 4             ] mov\t%0.<Vetype>, %<vwcore>1
     [ w        , w   ; 4             ] mov\t%0.<Vetype>, %<Vetype>1
     [ w        , Uty ; 8             ] #
  }
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
)

;; Duplicate an Advanced SIMD vector to fill an SVE vector (LE version).
;;
;; The addressing mode range of LD1RQ does not match the addressing mode
;; range of LDR Qn.  If the predicate enforced the LD1RQ range, we would
;; not be able to combine LDR Qns outside that range.  The predicate
;; therefore accepts all memory operands, with only the constraints
;; enforcing the actual restrictions.  If the instruction is split
;; before RA, we need to load invalid addresses into a temporary.

(define_insn_and_split "@aarch64_vec_duplicate_vq<mode>_le"
  [(set (match_operand:SVE_FULL 0 "register_operand" "=w, w")
	(vec_duplicate:SVE_FULL
	  (match_operand:<V128> 1 "nonimmediate_operand" "w, UtQ")))
   (clobber (match_scratch:VNx16BI 2 "=X, Upl"))]
  "TARGET_SVE && !BYTES_BIG_ENDIAN"
  {
    switch (which_alternative)
      {
	case 0:
	  operands[1] = gen_rtx_REG (<MODE>mode, REGNO (operands[1]));
	  return "dup\t%0.q, %1.q[0]";
	case 1:
	  return "#";
	default:
	  gcc_unreachable ();
      }
  }
  "&& MEM_P (operands[1])"
  [(const_int 0)]
  {
    if (can_create_pseudo_p ()
        && !aarch64_sve_ld1rq_operand (operands[1], <V128>mode))
      operands[1] = force_reload_address (operands[1]);
    if (GET_CODE (operands[2]) == SCRATCH)
      operands[2] = gen_reg_rtx (VNx16BImode);
    emit_move_insn (operands[2], CONSTM1_RTX (VNx16BImode));
    rtx gp = gen_lowpart (<VPRED>mode, operands[2]);
    emit_insn (gen_aarch64_sve_ld1rq<mode> (operands[0], operands[1], gp));
    DONE;
  }
)

;; Duplicate an Advanced SIMD vector to fill an SVE vector (BE version).
;; The SVE register layout puts memory lane N into (architectural)
;; register lane N, whereas the Advanced SIMD layout puts the memory
;; lsb into the register lsb.  We therefore have to describe this in rtl
;; terms as a reverse of the V128 vector followed by a duplicate.
(define_insn "@aarch64_vec_duplicate_vq<mode>_be"
  [(set (match_operand:SVE_FULL 0 "register_operand" "=w")
	(vec_duplicate:SVE_FULL
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
;; be used by combine to optimize selects of a vec_duplicate<mode>
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
  [(set (match_operand:SVE_FULL 0 "register_operand" "=w")
	(unspec:SVE_FULL
	  [(match_operand:<VPRED> 2 "register_operand" "Upl")
	   (match_operand:<V128> 1 "aarch64_sve_ld1rq_operand" "UtQ")]
	  UNSPEC_LD1RQ))]
  "TARGET_SVE"
  {
    operands[1] = gen_rtx_MEM (<VEL>mode, XEXP (operands[1], 0));
    return "ld1rq<Vesize>\t%0.<Vetype>, %2/z, %1";
  }
)

(define_insn "@aarch64_sve_ld1ro<mode>"
  [(set (match_operand:SVE_FULL 0 "register_operand" "=w")
	(unspec:SVE_FULL
	  [(match_operand:<VPRED> 2 "register_operand" "Upl")
	   (match_operand:OI 1 "aarch64_sve_ld1ro_operand_<Vesize>"
			       "UO<Vesize>")]
	  UNSPEC_LD1RO))]
  "TARGET_SVE_F64MM && TARGET_NON_STREAMING"
  {
    operands[1] = gen_rtx_MEM (<VEL>mode, XEXP (operands[1], 0));
    return "ld1ro<Vesize>\t%0.<Vetype>, %2/z, %1";
  }
)

;; -------------------------------------------------------------------------
;; ---- [INT,FP] Initialize from individual elements
;; -------------------------------------------------------------------------
;; Includes:
;; - INSR
;; -------------------------------------------------------------------------

(define_expand "vec_init<mode><Vel>"
  [(match_operand:SVE_FULL 0 "register_operand")
    (match_operand 1 "")]
  "TARGET_SVE"
  {
    aarch64_sve_expand_vector_init (operands[0], operands[1]);
    DONE;
  }
)

;; Vector constructor combining two half vectors { a, b }
(define_expand "vec_init<mode><Vhalf>"
  [(match_operand:SVE_NO2E 0 "register_operand")
   (match_operand 1 "")]
  "TARGET_SVE"
  {
    aarch64_sve_expand_vector_init_subvector (operands[0], operands[1]);
    DONE;
  }
)

;; Vector constructor combining four quad vectors { a, b, c, d }
(define_expand "vec_init<mode><Vquad>"
  [(match_operand:SVE_NO4E 0 "register_operand")
   (match_operand 1 "")]
  "TARGET_SVE"
  {
    aarch64_sve_expand_vector_init_subvector (operands[0], operands[1]);
    DONE;
  }
)

;; Vector constructor combining eight vectors { a, b, c, d, ... }
(define_expand "vec_initvnx16qivnx2qi"
  [(match_operand:VNx16QI 0 "register_operand")
   (match_operand 1 "")]
  "TARGET_SVE"
  {
    aarch64_sve_expand_vector_init_subvector (operands[0], operands[1]);
    DONE;
  }
)

;; Shift an SVE vector left and insert a scalar into element 0.
(define_insn "vec_shl_insert_<mode>"
  [(set (match_operand:SVE_FULL 0 "register_operand")
	(unspec:SVE_FULL
	  [(match_operand:SVE_FULL 1 "register_operand")
	   (match_operand:<VEL> 2 "aarch64_reg_or_zero")]
	  UNSPEC_INSR))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1 , 2  ; attrs: movprfx ]
     [ ?w       , 0 , rZ ; *              ] insr\t%0.<Vetype>, %<vwcore>2
     [ w        , 0 , w  ; *              ] insr\t%0.<Vetype>, %<Vetype>2
     [ ??&w     , w , rZ ; yes            ] movprfx\t%0, %1\;insr\t%0.<Vetype>, %<vwcore>2
     [ ?&w      , w , w  ; yes            ] movprfx\t%0, %1\;insr\t%0.<Vetype>, %<Vetype>2
  }
)

;; -------------------------------------------------------------------------
;; ---- [INT] Linear series
;; -------------------------------------------------------------------------
;; Includes:
;; - INDEX
;; -------------------------------------------------------------------------

(define_insn "vec_series<mode>"
  [(set (match_operand:SVE_I 0 "register_operand")
	(vec_series:SVE_I
	  (match_operand:<VEL> 1 "aarch64_sve_index_operand")
	  (match_operand:<VEL> 2 "aarch64_sve_index_operand")))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2    ]
     [ w        , Usi , r    ] index\t%0.<Vctype>, #%1, %<vccore>2
     [ w        , r   , Usi  ] index\t%0.<Vctype>, %<vccore>1, #%2
     [ w        , r   , r    ] index\t%0.<Vctype>, %<vccore>1, %<vccore>2
  }
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
    return "index\t%0.<Vctype>, %<vccore>1, #%2";
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
	(vec_duplicate:PRED_ALL (match_operand:QI 1 "register_operand")))]
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
	  (match_operand:SVE_FULL 1 "register_operand")
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
	  (match_operand:SVE_FULL 1 "register_operand" "w, 0, w")
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
	  (match_operand:SVE_FULL 1 "register_operand" "w, w, w")
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
	  (match_operand:SVE_FULL 1 "register_operand" "w")
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
	  (match_operand:SVE_FULL 1 "register_operand" "0, w")
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
;; - LASTA
;; - LASTB
;; -------------------------------------------------------------------------

;; Extract the last active element of operand 1 into operand 0.
;; If no elements are active, extract the last inactive element instead.
(define_insn "@extract_<last_op>_<mode>"
  [(set (match_operand:<VEL> 0 "register_operand")
	(unspec:<VEL>
	  [(match_operand:<VPRED> 1 "register_operand")
	   (match_operand:SVE_FULL 2 "register_operand")]
	  LAST))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2  ]
     [ ?r       , Upl , w  ] last<ab>\t%<vwcore>0, %1, %2.<Vetype>
     [ w        , Upl , w  ] last<ab>\t%<Vetype>0, %1, %2.<Vetype>
  }
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
   (reg:SVE_FULL_I V0_REGNUM)]
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
;; - RBIT (= bitreverse)
;; - NEG
;; - NOT
;; -------------------------------------------------------------------------

(define_expand "ctz<mode>2"
  [(set (match_operand:SVE_I 0 "register_operand")
	(unspec:SVE_I
	  [(match_dup 2)
	   (ctz:SVE_I
	     (match_operand:SVE_I 1 "register_operand"))]
	  UNSPEC_PRED_X))]
  "TARGET_SVE"
  {
     rtx pred = aarch64_ptrue_reg (<VPRED>mode);
     rtx temp = gen_reg_rtx (<MODE>mode);
     emit_insn (gen_aarch64_pred_rbit<mode> (temp, pred, operands[1]));
     emit_insn (gen_aarch64_pred_clz<mode> (operands[0], pred, temp));
     DONE;
  }
)

;; Unpredicated integer unary arithmetic.
(define_expand "<optab><mode>2"
  [(set (match_operand:SVE_I 0 "register_operand")
	(unspec:SVE_I
	  [(match_dup 2)
	   (SVE_INT_UNARY:SVE_I
	     (match_operand:SVE_I 1 "register_operand"))]
	  UNSPEC_PRED_X))]
  "TARGET_SVE"
  {
    operands[2] = aarch64_ptrue_reg (<VPRED>mode);
  }
)

;; Integer unary arithmetic predicated with a PTRUE.
(define_insn "@aarch64_pred_<optab><mode>"
  [(set (match_operand:SVE_VDQ_I 0 "register_operand")
	(unspec:SVE_VDQ_I
	  [(match_operand:<VPRED> 1 "register_operand")
	   (SVE_INT_UNARY:SVE_VDQ_I
	     (match_operand:SVE_VDQ_I 2 "register_operand"))]
	  UNSPEC_PRED_X))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2 ; attrs: movprfx ]
     [ w        , Upl , 0 ; *              ] <sve_int_op>\t%Z0.<Vetype>, %1/m, %Z2.<Vetype>
     [ ?&w      , Upl , w ; yes            ] movprfx\t%Z0, %Z2\;<sve_int_op>\t%Z0.<Vetype>, %1/m, %Z2.<Vetype>
  }
)

;; Predicated integer unary arithmetic with merging.
(define_expand "@cond_<optab><mode>"
  [(set (match_operand:SVE_I 0 "register_operand")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand")
	   (SVE_INT_UNARY:SVE_I
	     (match_operand:SVE_I 2 "register_operand"))
	   (match_operand:SVE_I 3 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE"
)

;; Predicated integer unary arithmetic, merging with the first input.
(define_insn "*cond_<optab><mode>_2"
  [(set (match_operand:SVE_I 0 "register_operand")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand")
	   (SVE_INT_UNARY:SVE_I
	     (match_operand:SVE_I 2 "register_operand"))
	   (match_dup 2)]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2 ; attrs: movprfx ]
     [ w        , Upl , 0 ; *              ] <sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>
     [ ?&w      , Upl , w ; yes            ] movprfx\t%0, %2\;<sve_int_op>\t%0.<Vetype>, %1/m, %2.<Vetype>
  }
)

;; Predicated integer unary arithmetic, merging with an independent value.
;;
;; The earlyclobber isn't needed for the first alternative, but omitting
;; it would only help the case in which operands 2 and 3 are the same,
;; which is handled above rather than here.  Marking all the alternatives
;; as earlyclobber helps to make the instruction more regular to the
;; register allocator.
(define_insn "*cond_<optab><mode>_any"
  [(set (match_operand:SVE_I 0 "register_operand")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand")
	   (SVE_INT_UNARY:SVE_I
	     (match_operand:SVE_I 2 "register_operand"))
	   (match_operand:SVE_I 3 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE && !rtx_equal_p (operands[2], operands[3])"
  {@ [ cons: =0 , 1   , 2 , 3  ; attrs: movprfx ]
     [ &w       , Upl , w , 0  ; *              ] <sve_int_op>\t%0.<Vetype>, %1/m, %2.<Vetype>
     [ ?&w      , Upl , w , Dz ; yes            ] movprfx\t%0.<Vetype>, %1/z, %2.<Vetype>\;<sve_int_op>\t%0.<Vetype>, %1/m, %2.<Vetype>
     [ ?&w      , Upl , w , w  ; yes            ] movprfx\t%0, %3\;<sve_int_op>\t%0.<Vetype>, %1/m, %2.<Vetype>
  }
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
  [(set (match_operand:SVE_FULL_I 0 "register_operand")
	(unspec:SVE_FULL_I
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_I
	     [(match_operand:SVE_FULL_I 2 "register_operand")]
	     SVE_INT_UNARY)]
	  UNSPEC_PRED_X))]
  "TARGET_SVE && <elem_bits> >= <min_elem_bits>"
  {@ [ cons: =0 , 1   , 2 ; attrs: movprfx ]
     [ w        , Upl , 0 ; *              ] <sve_int_op>\t%0.<Vetype>, %1/m, %2.<Vetype>
     [ ?&w      , Upl , w ; yes            ] movprfx\t%0, %2\;<sve_int_op>\t%0.<Vetype>, %1/m, %2.<Vetype>
  }
)

;; Another way of expressing the REVB, REVH and REVW patterns, with this
;; form being easier for permutes.  The predicate mode determines the number
;; of lanes and the data mode decides the granularity of the reversal within
;; each lane.
(define_insn "@aarch64_sve_revbhw_<SVE_ALL:mode><PRED_HSD:mode>"
  [(set (match_operand:SVE_ALL 0 "register_operand")
	(unspec:SVE_ALL
	  [(match_operand:PRED_HSD 1 "register_operand")
	   (unspec:SVE_ALL
	     [(match_operand:SVE_ALL 2 "register_operand")]
	     UNSPEC_REVBHW)]
	  UNSPEC_PRED_X))]
  "TARGET_SVE && <PRED_HSD:elem_bits> > <SVE_ALL:container_bits>"
  {@ [ cons: =0 , 1   , 2 ; attrs: movprfx ]
     [ w        , Upl , 0 ; *              ] rev<SVE_ALL:Vcwtype>\t%0.<PRED_HSD:Vetype>, %1/m, %2.<PRED_HSD:Vetype>
     [ ?&w      , Upl , w ; yes            ] movprfx\t%0, %2\;rev<SVE_ALL:Vcwtype>\t%0.<PRED_HSD:Vetype>, %1/m, %2.<PRED_HSD:Vetype>
  }
)

;; Predicated integer unary operations with merging.
(define_insn "@cond_<optab><mode>"
  [(set (match_operand:SVE_FULL_I 0 "register_operand")
	(unspec:SVE_FULL_I
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_I
	     [(match_operand:SVE_FULL_I 2 "register_operand")]
	     SVE_INT_UNARY)
	   (match_operand:SVE_FULL_I 3 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE && <elem_bits> >= <min_elem_bits>"
  {@ [ cons: =0 , 1   , 2 , 3  ; attrs: movprfx ]
     [ w        , Upl , w , 0  ; *              ] <sve_int_op>\t%0.<Vetype>, %1/m, %2.<Vetype>
     [ ?&w      , Upl , w , Dz ; yes            ] movprfx\t%0.<Vetype>, %1/z, %2.<Vetype>\;<sve_int_op>\t%0.<Vetype>, %1/m, %2.<Vetype>
     [ ?&w      , Upl , w , w  ; yes            ] movprfx\t%0, %3\;<sve_int_op>\t%0.<Vetype>, %1/m, %2.<Vetype>
  }
)

;; -------------------------------------------------------------------------
;; ---- [INT] Sign and zero extension
;; -------------------------------------------------------------------------
;; Includes:
;; - SXTB
;; - SXTH
;; - SXTW
;; - UXTB
;; - UXTH
;; - UXTW
;; -------------------------------------------------------------------------

;; Unpredicated sign and zero extension from a narrower mode.
(define_expand "<optab><SVE_PARTIAL_I:mode><SVE_HSDI:mode>2"
  [(set (match_operand:SVE_HSDI 0 "register_operand")
	(unspec:SVE_HSDI
	  [(match_dup 2)
	   (ANY_EXTEND:SVE_HSDI
	     (match_operand:SVE_PARTIAL_I 1 "register_operand"))]
	  UNSPEC_PRED_X))]
  "TARGET_SVE && (~<SVE_HSDI:narrower_mask> & <SVE_PARTIAL_I:self_mask>) == 0"
  {
    operands[2] = aarch64_ptrue_reg (<SVE_HSDI:VPRED>mode);
  }
)

;; Predicated sign and zero extension from a narrower mode.
(define_insn "*<optab><SVE_PARTIAL_I:mode><SVE_HSDI:mode>2"
  [(set (match_operand:SVE_HSDI 0 "register_operand")
	(unspec:SVE_HSDI
	  [(match_operand:<SVE_HSDI:VPRED> 1 "register_operand")
	   (ANY_EXTEND:SVE_HSDI
	     (match_operand:SVE_PARTIAL_I 2 "register_operand"))]
	  UNSPEC_PRED_X))]
  "TARGET_SVE && (~<SVE_HSDI:narrower_mask> & <SVE_PARTIAL_I:self_mask>) == 0"
  {@ [ cons: =0 , 1   , 2 ; attrs: movprfx ]
     [ w        , Upl , 0 ; *              ] <su>xt<SVE_PARTIAL_I:Vesize>\t%0.<SVE_HSDI:Vetype>, %1/m, %2.<SVE_HSDI:Vetype>
     [ ?&w      , Upl , w ; yes            ] movprfx\t%0, %2\;<su>xt<SVE_PARTIAL_I:Vesize>\t%0.<SVE_HSDI:Vetype>, %1/m, %2.<SVE_HSDI:Vetype>
  }
)

;; Predicated truncate-and-sign-extend operations.
(define_insn "@aarch64_pred_sxt<SVE_FULL_HSDI:mode><SVE_PARTIAL_I:mode>"
  [(set (match_operand:SVE_FULL_HSDI 0 "register_operand")
	(unspec:SVE_FULL_HSDI
	  [(match_operand:<SVE_FULL_HSDI:VPRED> 1 "register_operand")
	   (sign_extend:SVE_FULL_HSDI
	     (truncate:SVE_PARTIAL_I
	       (match_operand:SVE_FULL_HSDI 2 "register_operand")))]
	  UNSPEC_PRED_X))]
  "TARGET_SVE
   && (~<SVE_FULL_HSDI:narrower_mask> & <SVE_PARTIAL_I:self_mask>) == 0"
  {@ [ cons: =0 , 1   , 2 ; attrs: movprfx ]
     [ w        , Upl , 0 ; *              ] sxt<SVE_PARTIAL_I:Vesize>\t%0.<SVE_FULL_HSDI:Vetype>, %1/m, %2.<SVE_FULL_HSDI:Vetype>
     [ ?&w      , Upl , w ; yes            ] movprfx\t%0, %2\;sxt<SVE_PARTIAL_I:Vesize>\t%0.<SVE_FULL_HSDI:Vetype>, %1/m, %2.<SVE_FULL_HSDI:Vetype>
  }
)

;; Predicated truncate-and-sign-extend operations with merging.
(define_insn "@aarch64_cond_sxt<SVE_FULL_HSDI:mode><SVE_PARTIAL_I:mode>"
  [(set (match_operand:SVE_FULL_HSDI 0 "register_operand")
	(unspec:SVE_FULL_HSDI
	  [(match_operand:<SVE_FULL_HSDI:VPRED> 1 "register_operand")
	   (sign_extend:SVE_FULL_HSDI
	     (truncate:SVE_PARTIAL_I
	       (match_operand:SVE_FULL_HSDI 2 "register_operand")))
	   (match_operand:SVE_FULL_HSDI 3 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE
   && (~<SVE_FULL_HSDI:narrower_mask> & <SVE_PARTIAL_I:self_mask>) == 0"
  {@ [ cons: =0 , 1   , 2 , 3  ; attrs: movprfx ]
     [ w        , Upl , w , 0  ; *              ] sxt<SVE_PARTIAL_I:Vesize>\t%0.<SVE_FULL_HSDI:Vetype>, %1/m, %2.<SVE_FULL_HSDI:Vetype>
     [ ?&w      , Upl , w , Dz ; yes            ] movprfx\t%0.<SVE_FULL_HSDI:Vetype>, %1/z, %2.<SVE_FULL_HSDI:Vetype>\;sxt<SVE_PARTIAL_I:Vesize>\t%0.<SVE_FULL_HSDI:Vetype>, %1/m, %2.<SVE_FULL_HSDI:Vetype>
     [ ?&w      , Upl , w , w  ; yes            ] movprfx\t%0, %3\;sxt<SVE_PARTIAL_I:Vesize>\t%0.<SVE_FULL_HSDI:Vetype>, %1/m, %2.<SVE_FULL_HSDI:Vetype>
  }
)

;; Predicated truncate-and-zero-extend operations, merging with the
;; first input.
;;
;; The canonical form of this operation is an AND of a constant rather
;; than (zero_extend (truncate ...)).
(define_insn "*cond_uxt<mode>_2"
  [(set (match_operand:SVE_I 0 "register_operand")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand")
	   (and:SVE_I
	     (match_operand:SVE_I 2 "register_operand")
	     (match_operand:SVE_I 3 "aarch64_sve_uxt_immediate"))
	   (match_dup 2)]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2 ; attrs: movprfx ]
     [ w        , Upl , 0 ; *              ] uxt%e3\t%0.<Vetype>, %1/m, %0.<Vetype>
     [ ?&w      , Upl , w ; yes            ] movprfx\t%0, %2\;uxt%e3\t%0.<Vetype>, %1/m, %2.<Vetype>
  }
)

;; Predicated truncate-and-zero-extend operations, merging with an
;; independent value.
;;
;; The earlyclobber isn't needed for the first alternative, but omitting
;; it would only help the case in which operands 2 and 4 are the same,
;; which is handled above rather than here.  Marking all the alternatives
;; as early-clobber helps to make the instruction more regular to the
;; register allocator.
(define_insn "*cond_uxt<mode>_any"
  [(set (match_operand:SVE_I 0 "register_operand")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand")
	   (and:SVE_I
	     (match_operand:SVE_I 2 "register_operand")
	     (match_operand:SVE_I 3 "aarch64_sve_uxt_immediate"))
	   (match_operand:SVE_I 4 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE && !rtx_equal_p (operands[2], operands[4])"
  {@ [ cons: =0 , 1   , 2 , 4  ; attrs: movprfx ]
     [ &w       , Upl , w , 0  ; *              ] uxt%e3\t%0.<Vetype>, %1/m, %2.<Vetype>
     [ ?&w      , Upl , w , Dz ; yes            ] movprfx\t%0.<Vetype>, %1/z, %2.<Vetype>\;uxt%e3\t%0.<Vetype>, %1/m, %2.<Vetype>
     [ ?&w      , Upl , w , w  ; yes            ] movprfx\t%0, %4\;uxt%e3\t%0.<Vetype>, %1/m, %2.<Vetype>
  }
)

;; -------------------------------------------------------------------------
;; ---- [INT] Truncation
;; -------------------------------------------------------------------------
;; The patterns in this section are synthetic.
;; -------------------------------------------------------------------------

;; Truncate to a partial SVE vector from either a full vector or a
;; wider partial vector.  This is a no-op, because we can just ignore
;; the unused upper bits of the source.
(define_insn_and_split "trunc<SVE_HSDI:mode><SVE_PARTIAL_I:mode>2"
  [(set (match_operand:SVE_PARTIAL_I 0 "register_operand" "=w")
	(truncate:SVE_PARTIAL_I
	  (match_operand:SVE_HSDI 1 "register_operand" "w")))]
  "TARGET_SVE && (~<SVE_HSDI:narrower_mask> & <SVE_PARTIAL_I:self_mask>) == 0"
  "#"
  "&& reload_completed"
  [(set (match_dup 0) (match_dup 1))]
  {
    operands[1] = aarch64_replace_reg_mode (operands[1],
					    <SVE_PARTIAL_I:MODE>mode);
  }
)

;; -------------------------------------------------------------------------
;; ---- [INT] Logical inverse
;; -------------------------------------------------------------------------
;; Includes:
;; - CNOT
;; -------------------------------------------------------------------------

;; Logical inverse, predicated with a ptrue.
(define_expand "@aarch64_ptrue_cnot<mode>"
  [(set (match_operand:SVE_FULL_I 0 "register_operand")
	(unspec:SVE_FULL_I
	  [(unspec:<VPRED>
	     [(match_operand:<VPRED> 1 "register_operand")
	      (const_int SVE_KNOWN_PTRUE)
	      (eq:<VPRED>
		(match_operand:SVE_FULL_I 2 "register_operand")
		(match_dup 3))]
	     UNSPEC_PRED_Z)
	   (match_dup 4)
	   (match_dup 3)]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  {
    operands[3] = CONST0_RTX (<MODE>mode);
    operands[4] = CONST1_RTX (<MODE>mode);
  }
)

(define_insn "*cnot<mode>"
  [(set (match_operand:SVE_I 0 "register_operand")
	(unspec:SVE_I
	  [(unspec:<VPRED>
	     [(match_operand:<VPRED> 1 "register_operand")
	      (const_int SVE_KNOWN_PTRUE)
	      (eq:<VPRED>
		(match_operand:SVE_I 2 "register_operand")
		(match_operand:SVE_I 3 "aarch64_simd_imm_zero"))]
	     UNSPEC_PRED_Z)
	   (match_operand:SVE_I 4 "aarch64_simd_imm_one")
	   (match_dup 3)]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2 ; attrs: movprfx ]
     [ w        , Upl , 0 ; *              ] cnot\t%0.<Vetype>, %1/m, %2.<Vetype>
     [ ?&w      , Upl , w ; yes            ] movprfx\t%0, %2\;cnot\t%0.<Vetype>, %1/m, %2.<Vetype>
  }
)

;; Predicated logical inverse with merging.
(define_expand "@cond_cnot<mode>"
  [(set (match_operand:SVE_FULL_I 0 "register_operand")
	(unspec:SVE_FULL_I
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_I
	     [(unspec:<VPRED>
		[(match_dup 4)
		 (const_int SVE_KNOWN_PTRUE)
		 (eq:<VPRED>
		   (match_operand:SVE_FULL_I 2 "register_operand")
		   (match_dup 5))]
		UNSPEC_PRED_Z)
	      (match_dup 6)
	      (match_dup 5)]
	     UNSPEC_SEL)
	   (match_operand:SVE_FULL_I 3 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  {
    operands[4] = CONSTM1_RTX (<VPRED>mode);
    operands[5] = CONST0_RTX (<MODE>mode);
    operands[6] = CONST1_RTX (<MODE>mode);
  }
)

;; Predicated logical inverse, merging with the first input.
(define_insn_and_rewrite "*cond_cnot<mode>_2"
  [(set (match_operand:SVE_I 0 "register_operand")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand")
	   ;; Logical inverse of operand 2 (as above).
	   (unspec:SVE_I
	     [(unspec:<VPRED>
		[(match_operand 5)
		 (const_int SVE_KNOWN_PTRUE)
		 (eq:<VPRED>
		   (match_operand:SVE_I 2 "register_operand")
		   (match_operand:SVE_I 3 "aarch64_simd_imm_zero"))]
		UNSPEC_PRED_Z)
	      (match_operand:SVE_I 4 "aarch64_simd_imm_one")
	      (match_dup 3)]
	     UNSPEC_SEL)
	   (match_dup 2)]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2 ; attrs: movprfx ]
     [ w        , Upl , 0 ; *              ] cnot\t%0.<Vetype>, %1/m, %0.<Vetype>
     [ ?&w      , Upl , w ; yes            ] movprfx\t%0, %2\;cnot\t%0.<Vetype>, %1/m, %2.<Vetype>
  }
  "&& !CONSTANT_P (operands[5])"
  {
    operands[5] = CONSTM1_RTX (<VPRED>mode);
  }
)

;; Predicated logical inverse, merging with an independent value.
;;
;; The earlyclobber isn't needed for the first alternative, but omitting
;; it would only help the case in which operands 2 and 6 are the same,
;; which is handled above rather than here.  Marking all the alternatives
;; as earlyclobber helps to make the instruction more regular to the
;; register allocator.
(define_insn_and_rewrite "*cond_cnot<mode>_any"
  [(set (match_operand:SVE_I 0 "register_operand")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand")
	   ;; Logical inverse of operand 2 (as above).
	   (unspec:SVE_I
	     [(unspec:<VPRED>
		[(match_operand 5)
		 (const_int SVE_KNOWN_PTRUE)
		 (eq:<VPRED>
		   (match_operand:SVE_I 2 "register_operand")
		   (match_operand:SVE_I 3 "aarch64_simd_imm_zero"))]
		UNSPEC_PRED_Z)
	      (match_operand:SVE_I 4 "aarch64_simd_imm_one")
	      (match_dup 3)]
	     UNSPEC_SEL)
	   (match_operand:SVE_I 6 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE && !rtx_equal_p (operands[2], operands[6])"
  {@ [ cons: =0 , 1   , 2 , 6  ; attrs: movprfx ]
     [ &w       , Upl , w , 0  ; *              ] cnot\t%0.<Vetype>, %1/m, %2.<Vetype>
     [ ?&w      , Upl , w , Dz ; yes            ] movprfx\t%0.<Vetype>, %1/z, %2.<Vetype>\;cnot\t%0.<Vetype>, %1/m, %2.<Vetype>
     [ ?&w      , Upl , w , w  ; yes            ] movprfx\t%0, %6\;cnot\t%0.<Vetype>, %1/m, %2.<Vetype>
  }
  "&& !CONSTANT_P (operands[5])"
  {
    operands[5] = CONSTM1_RTX (<VPRED>mode);
  }
)

;; -------------------------------------------------------------------------
;; ---- [FP<-INT] General unary arithmetic that maps to unspecs
;; -------------------------------------------------------------------------
;; Includes:
;; - FEXPA
;; -------------------------------------------------------------------------

;; Unpredicated unary operations that take an integer and return a float.
(define_insn "@aarch64_sve_<optab><mode>"
  [(set (match_operand:SVE_FULL_F 0 "register_operand" "=w")
	(unspec:SVE_FULL_F
	  [(match_operand:<V_INT_EQUIV> 1 "register_operand" "w")]
	  SVE_FP_UNARY_INT))]
  "TARGET_SVE"
  "<sve_fp_op>\t%0.<Vetype>, %1.<Vetype>"
)

;; -------------------------------------------------------------------------
;; ---- [FP] General unary arithmetic corresponding to unspecs
;; -------------------------------------------------------------------------
;; Includes:
;; - FABS
;; - FNEG
;; - FRECPE
;; - FRECPX
;; - FRINTA
;; - FRINTI
;; - FRINTM
;; - FRINTN
;; - FRINTP
;; - FRINTX
;; - FRINTZ
;; - FRSQRTE
;; - FSQRT
;; -------------------------------------------------------------------------

;; Unpredicated floating-point unary operations.
(define_insn "@aarch64_sve_<optab><mode>"
  [(set (match_operand:SVE_FULL_F 0 "register_operand" "=w")
	(unspec:SVE_FULL_F
	  [(match_operand:SVE_FULL_F 1 "register_operand" "w")]
	  SVE_FP_UNARY))]
  "TARGET_SVE"
  "<sve_fp_op>\t%0.<Vetype>, %1.<Vetype>"
)

;; Unpredicated floating-point unary operations.
(define_expand "<optab><mode>2"
  [(set (match_operand:SVE_FULL_F 0 "register_operand")
	(unspec:SVE_FULL_F
	  [(match_dup 2)
	   (const_int SVE_RELAXED_GP)
	   (match_operand:SVE_FULL_F 1 "register_operand")]
	  SVE_COND_FP_UNARY_OPTAB))]
  "TARGET_SVE"
  {
    operands[2] = aarch64_ptrue_reg (<VPRED>mode);
  }
)

;; Predicated floating-point unary operations.
(define_insn "@aarch64_pred_<optab><mode>"
  [(set (match_operand:SVE_FULL_F 0 "register_operand")
	(unspec:SVE_FULL_F
	  [(match_operand:<VPRED> 1 "register_operand")
	   (match_operand:SI 3 "aarch64_sve_gp_strictness")
	   (match_operand:SVE_FULL_F 2 "register_operand")]
	  SVE_COND_FP_UNARY))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2 ; attrs: movprfx ]
     [ w        , Upl , 0 ; *              ] <sve_fp_op>\t%0.<Vetype>, %1/m, %2.<Vetype>
     [ ?&w      , Upl , w ; yes            ] movprfx\t%0, %2\;<sve_fp_op>\t%0.<Vetype>, %1/m, %2.<Vetype>
  }
)

;; Predicated floating-point unary arithmetic with merging.
(define_expand "@cond_<optab><mode>"
  [(set (match_operand:SVE_FULL_F 0 "register_operand")
	(unspec:SVE_FULL_F
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_F
	     [(match_dup 1)
	      (const_int SVE_STRICT_GP)
	      (match_operand:SVE_FULL_F 2 "register_operand")]
	     SVE_COND_FP_UNARY)
	   (match_operand:SVE_FULL_F 3 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE"
)

;; Predicated floating-point unary arithmetic, merging with the first input.
(define_insn_and_rewrite "*cond_<optab><mode>_2_relaxed"
  [(set (match_operand:SVE_FULL_F 0 "register_operand")
	(unspec:SVE_FULL_F
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_F
	     [(match_operand 3)
	      (const_int SVE_RELAXED_GP)
	      (match_operand:SVE_FULL_F 2 "register_operand")]
	     SVE_COND_FP_UNARY)
	   (match_dup 2)]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2 ; attrs: movprfx ]
     [ w        , Upl , 0 ; *              ] <sve_fp_op>\t%0.<Vetype>, %1/m, %0.<Vetype>
     [ ?&w      , Upl , w ; yes            ] movprfx\t%0, %2\;<sve_fp_op>\t%0.<Vetype>, %1/m, %2.<Vetype>
  }
  "&& !rtx_equal_p (operands[1], operands[3])"
  {
    operands[3] = copy_rtx (operands[1]);
  }
)

(define_insn "*cond_<optab><mode>_2_strict"
  [(set (match_operand:SVE_FULL_F 0 "register_operand")
	(unspec:SVE_FULL_F
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_F
	     [(match_dup 1)
	      (const_int SVE_STRICT_GP)
	      (match_operand:SVE_FULL_F 2 "register_operand")]
	     SVE_COND_FP_UNARY)
	   (match_dup 2)]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2 ; attrs: movprfx ]
     [ w        , Upl , 0 ; *              ] <sve_fp_op>\t%0.<Vetype>, %1/m, %0.<Vetype>
     [ ?&w      , Upl , w ; yes            ] movprfx\t%0, %2\;<sve_fp_op>\t%0.<Vetype>, %1/m, %2.<Vetype>
  }
)

;; Predicated floating-point unary arithmetic, merging with an independent
;; value.
;;
;; The earlyclobber isn't needed for the first alternative, but omitting
;; it would only help the case in which operands 2 and 3 are the same,
;; which is handled above rather than here.  Marking all the alternatives
;; as earlyclobber helps to make the instruction more regular to the
;; register allocator.
(define_insn_and_rewrite "*cond_<optab><mode>_any_relaxed"
  [(set (match_operand:SVE_FULL_F 0 "register_operand")
	(unspec:SVE_FULL_F
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_F
	     [(match_operand 4)
	      (const_int SVE_RELAXED_GP)
	      (match_operand:SVE_FULL_F 2 "register_operand")]
	     SVE_COND_FP_UNARY)
	   (match_operand:SVE_FULL_F 3 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE && !rtx_equal_p (operands[2], operands[3])"
  {@ [ cons: =0 , 1   , 2 , 3  ; attrs: movprfx ]
     [ &w       , Upl , w , 0  ; *              ] <sve_fp_op>\t%0.<Vetype>, %1/m, %2.<Vetype>
     [ ?&w      , Upl , w , Dz ; yes            ] movprfx\t%0.<Vetype>, %1/z, %2.<Vetype>\;<sve_fp_op>\t%0.<Vetype>, %1/m, %2.<Vetype>
     [ ?&w      , Upl , w , w  ; yes            ] movprfx\t%0, %3\;<sve_fp_op>\t%0.<Vetype>, %1/m, %2.<Vetype>
  }
  "&& !rtx_equal_p (operands[1], operands[4])"
  {
    operands[4] = copy_rtx (operands[1]);
  }
)

(define_insn "*cond_<optab><mode>_any_strict"
  [(set (match_operand:SVE_FULL_F 0 "register_operand")
	(unspec:SVE_FULL_F
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_F
	     [(match_dup 1)
	      (const_int SVE_STRICT_GP)
	      (match_operand:SVE_FULL_F 2 "register_operand")]
	     SVE_COND_FP_UNARY)
	   (match_operand:SVE_FULL_F 3 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE && !rtx_equal_p (operands[2], operands[3])"
  {@ [ cons: =0 , 1   , 2 , 3  ; attrs: movprfx ]
     [ &w       , Upl , w , 0  ; *              ] <sve_fp_op>\t%0.<Vetype>, %1/m, %2.<Vetype>
     [ ?&w      , Upl , w , Dz ; yes            ] movprfx\t%0.<Vetype>, %1/z, %2.<Vetype>\;<sve_fp_op>\t%0.<Vetype>, %1/m, %2.<Vetype>
     [ ?&w      , Upl , w , w  ; yes            ] movprfx\t%0, %3\;<sve_fp_op>\t%0.<Vetype>, %1/m, %2.<Vetype>
  }
)

;; -------------------------------------------------------------------------
;; ---- [FP] Square root
;; -------------------------------------------------------------------------

(define_expand "sqrt<mode>2"
  [(set (match_operand:SVE_FULL_F 0 "register_operand")
	(unspec:SVE_FULL_F
	  [(match_dup 2)
	   (const_int SVE_RELAXED_GP)
	   (match_operand:SVE_FULL_F 1 "register_operand")]
	  UNSPEC_COND_FSQRT))]
  "TARGET_SVE"
{
  if (aarch64_emit_approx_sqrt (operands[0], operands[1], false))
    DONE;
  operands[2] = aarch64_ptrue_reg (<VPRED>mode);
})

;; -------------------------------------------------------------------------
;; ---- [FP] Reciprocal square root
;; -------------------------------------------------------------------------

(define_expand "rsqrt<mode>2"
  [(set (match_operand:SVE_FULL_SDF 0 "register_operand")
	(unspec:SVE_FULL_SDF
	  [(match_operand:SVE_FULL_SDF 1 "register_operand")]
	  UNSPEC_RSQRT))]
  "TARGET_SVE"
{
  aarch64_emit_approx_sqrt (operands[0], operands[1], true);
  DONE;
})

(define_expand "@aarch64_rsqrte<mode>"
  [(set (match_operand:SVE_FULL_SDF 0 "register_operand")
	(unspec:SVE_FULL_SDF
	  [(match_operand:SVE_FULL_SDF 1 "register_operand")]
	  UNSPEC_RSQRTE))]
  "TARGET_SVE"
)

(define_expand "@aarch64_rsqrts<mode>"
  [(set (match_operand:SVE_FULL_SDF 0 "register_operand")
	(unspec:SVE_FULL_SDF
	  [(match_operand:SVE_FULL_SDF 1 "register_operand")
	   (match_operand:SVE_FULL_SDF 2 "register_operand")]
	  UNSPEC_RSQRTS))]
  "TARGET_SVE"
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
;; - SQADD  (SVE2 merging form only)
;; - SQSUB  (SVE2 merging form only)
;; - SUB    (merging form only)
;; - UMAX
;; - UMIN
;; - UQADD  (SVE2 merging form only)
;; - UQSUB  (SVE2 merging form only)
;; -------------------------------------------------------------------------

;; Unpredicated integer binary operations that have an immediate form.
(define_expand "<optab><mode>3"
  [(set (match_operand:SVE_I 0 "register_operand")
	(unspec:SVE_I
	  [(match_dup 3)
	   (SVE_INT_BINARY_MULTI:SVE_I
	     (match_operand:SVE_I 1 "register_operand")
	     (match_operand:SVE_I 2 "aarch64_sve_<sve_imm_con>_operand"))]
	  UNSPEC_PRED_X))]
  "TARGET_SVE"
  {
    operands[3] = aarch64_ptrue_reg (<VPRED>mode);
  }
)

;; Unpredicated integer binary operations that have an immediate form.
;; Advanced SIMD does not support vector DImode MUL, but SVE does.
;; Make use of the overlap between Z and V registers to implement the V2DI
;; optab for TARGET_SVE.  The mulvnx2di3 expander can
;; handle the TARGET_SVE2 case transparently.
(define_expand "mul<mode>3"
  [(set (match_operand:SVE_I_SIMD_DI 0 "register_operand")
	(unspec:SVE_I_SIMD_DI
	  [(match_dup 3)
	   (mult:SVE_I_SIMD_DI
	     (match_operand:SVE_I_SIMD_DI 1 "register_operand")
	     (match_operand:SVE_I_SIMD_DI 2 "aarch64_sve_vsm_operand"))]
	  UNSPEC_PRED_X))]
  "TARGET_SVE"
  {
    /* SVE2 supports the MUL (vectors, unpredicated) form.  Emit the simple
       pattern for it here rather than splitting off the MULT expander
       separately.  */
    if (TARGET_SVE2)
      {
	emit_move_insn (operands[0], gen_rtx_MULT (<MODE>mode,
						   operands[1], operands[2]));
	DONE;
      }
    operands[3] = aarch64_ptrue_reg (<VPRED>mode);
  }
)

;; Integer binary operations that have an immediate form, predicated
;; with a PTRUE.  We don't actually need the predicate for the first
;; and third alternatives, but using Upa or X isn't likely to gain much
;; and would make the instruction seem less uniform to the register
;; allocator.
(define_insn_and_split "@aarch64_pred_<optab><mode>"
  [(set (match_operand:SVE_I_SIMD_DI 0 "register_operand")
	(unspec:SVE_I_SIMD_DI
	  [(match_operand:<VPRED> 1 "register_operand")
	   (SVE_INT_BINARY_IMM:SVE_I_SIMD_DI
	     (match_operand:SVE_I_SIMD_DI 2 "register_operand")
	     (match_operand:SVE_I_SIMD_DI 3 "aarch64_sve_<sve_imm_con>_operand"))]
	  UNSPEC_PRED_X))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2  , 3             ; attrs: movprfx ]
     [ w        , Upl , %0 , <sve_imm_con> ; *              ] #
     [ w        , Upl , 0  , w             ; *              ] <sve_int_op>\t%Z0.<Vetype>, %1/m, %Z0.<Vetype>, %Z3.<Vetype>
     [ ?&w      , Upl , w  , <sve_imm_con> ; yes            ] #
     [ ?&w      , Upl , w  , w             ; yes            ] movprfx\t%Z0, %Z2\;<sve_int_op>\t%Z0.<Vetype>, %1/m, %Z0.<Vetype>, %Z3.<Vetype>
  }
  ; Split the unpredicated form after reload, so that we don't have
  ; the unnecessary PTRUE.
  "&& reload_completed
   && !register_operand (operands[3], <MODE>mode)"
  [(set (match_dup 0)
	(SVE_INT_BINARY_IMM:SVE_I_SIMD_DI (match_dup 2) (match_dup 3)))]
  ""
)

;; Unpredicated binary operations with a constant (post-RA only).
;; These are generated by splitting a predicated instruction whose
;; predicate is unused.
(define_insn "*post_ra_<optab><mode>3"
  [(set (match_operand:SVE_I_SIMD_DI 0 "register_operand" "=w, ?&w")
	(SVE_INT_BINARY_IMM:SVE_I_SIMD_DI
	  (match_operand:SVE_I_SIMD_DI 1 "register_operand" "0, w")
	  (match_operand:SVE_I_SIMD_DI 2 "aarch64_sve_<sve_imm_con>_immediate")))]
  "TARGET_SVE && reload_completed"
  "@
   <sve_int_op>\t%Z0.<Vetype>, %Z0.<Vetype>, #%<sve_imm_prefix>2
   movprfx\t%Z0, %Z1\;<sve_int_op>\t%Z0.<Vetype>, %Z0.<Vetype>, #%<sve_imm_prefix>2"
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
  [(set (match_operand:SVE_I 0 "register_operand")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand")
	   (SVE_INT_BINARY:SVE_I
	     (match_operand:SVE_I 2 "register_operand")
	     (match_operand:SVE_I 3 "register_operand"))
	   (match_dup 2)]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2 , 3 ; attrs: movprfx ]
     [ w        , Upl , 0 , w ; *              ] <sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
     [ ?&w      , Upl , w , w ; yes            ] movprfx\t%0, %2\;<sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
  }
)

;; Predicated integer operations, merging with the second input.
(define_insn "*cond_<optab><mode>_3"
  [(set (match_operand:SVE_I 0 "register_operand")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand")
	   (SVE_INT_BINARY:SVE_I
	     (match_operand:SVE_I 2 "register_operand")
	     (match_operand:SVE_I 3 "register_operand"))
	   (match_dup 3)]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2 , 3 ; attrs: movprfx ]
     [ w        , Upl , w , 0 ; *              ] <sve_int_op_rev>\t%0.<Vetype>, %1/m, %0.<Vetype>, %2.<Vetype>
     [ ?&w      , Upl , w , w ; yes            ] movprfx\t%0, %3\;<sve_int_op_rev>\t%0.<Vetype>, %1/m, %0.<Vetype>, %2.<Vetype>
  }
)

;; Predicated integer operations, merging with an independent value.
(define_insn_and_rewrite "*cond_<optab><mode>_any"
  [(set (match_operand:SVE_I 0 "register_operand")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand")
	   (SVE_INT_BINARY:SVE_I
	     (match_operand:SVE_I 2 "register_operand")
	     (match_operand:SVE_I 3 "register_operand"))
	   (match_operand:SVE_I 4 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE
   && !rtx_equal_p (operands[2], operands[4])
   && !rtx_equal_p (operands[3], operands[4])"
  {@ [ cons: =0 , 1   , 2 , 3 , 4   ]
     [ &w       , Upl , 0 , w , Dz  ] movprfx\t%0.<Vetype>, %1/z, %0.<Vetype>\;<sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
     [ &w       , Upl , w , 0 , Dz  ] movprfx\t%0.<Vetype>, %1/z, %0.<Vetype>\;<sve_int_op_rev>\t%0.<Vetype>, %1/m, %0.<Vetype>, %2.<Vetype>
     [ &w       , Upl , w , w , Dz  ] movprfx\t%0.<Vetype>, %1/z, %2.<Vetype>\;<sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
     [ &w       , Upl , w , w , 0   ] movprfx\t%0.<Vetype>, %1/m, %2.<Vetype>\;<sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
     [ ?&w      , Upl , w , w , w   ] #
  }
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
  [(set (match_operand:SVE_I 0 "register_operand")
	(plus:SVE_I
	  (match_operand:SVE_I 1 "register_operand")
	  (match_operand:SVE_I 2 "aarch64_sve_add_operand")))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1  , 2   ; attrs: movprfx ]
     [ w        , %0 , vsa ; *              ] add\t%0.<Vetype>, %0.<Vetype>, #%D2
     [ w        , 0  , vsn ; *              ] sub\t%0.<Vetype>, %0.<Vetype>, #%N2
     [ w        , 0  , vsi ; *              ] << aarch64_output_sve_vector_inc_dec ("%0.<Vetype>", operands[2]);
     [ ?w       , w  , vsa ; yes            ] movprfx\t%0, %1\;add\t%0.<Vetype>, %0.<Vetype>, #%D2
     [ ?w       , w  , vsn ; yes            ] movprfx\t%0, %1\;sub\t%0.<Vetype>, %0.<Vetype>, #%N2
     [ w        , w  , w   ; *              ] add\t%0.<Vetype>, %1.<Vetype>, %2.<Vetype>
  }
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
  [(set (match_operand:SVE_I 0 "register_operand")
	(minus:SVE_I
	  (match_operand:SVE_I 1 "aarch64_sve_arith_operand")
	  (match_operand:SVE_I 2 "register_operand")))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2 ; attrs: movprfx ]
     [ w        , w   , w ; *              ] sub\t%0.<Vetype>, %1.<Vetype>, %2.<Vetype>
     [ w        , vsa , 0 ; *              ] subr\t%0.<Vetype>, %0.<Vetype>, #%D1
     [ ?&w      , vsa , w ; yes            ] movprfx\t%0, %2\;subr\t%0.<Vetype>, %0.<Vetype>, #%D1
  }
)

;; Merging forms are handled through SVE_INT_BINARY.

;; -------------------------------------------------------------------------
;; ---- [INT] Take address
;; -------------------------------------------------------------------------
;; Includes:
;; - ADR
;; -------------------------------------------------------------------------

;; An unshifted and unscaled ADR.  This is functionally equivalent to an ADD,
;; but the svadrb intrinsics should preserve the user's choice.
(define_insn "@aarch64_adr<mode>"
  [(set (match_operand:SVE_FULL_SDI 0 "register_operand" "=w")
	(unspec:SVE_FULL_SDI
	  [(match_operand:SVE_FULL_SDI 1 "register_operand" "w")
	   (match_operand:SVE_FULL_SDI 2 "register_operand" "w")]
	  UNSPEC_ADR))]
  "TARGET_SVE && TARGET_NON_STREAMING"
  "adr\t%0.<Vetype>, [%1.<Vetype>, %2.<Vetype>]"
)

;; Same, but with the offset being sign-extended from the low 32 bits.
(define_insn_and_rewrite "*aarch64_adr_sxtw"
  [(set (match_operand:VNx2DI 0 "register_operand" "=w")
	(unspec:VNx2DI
	  [(match_operand:VNx2DI 1 "register_operand" "w")
	   (unspec:VNx2DI
	     [(match_operand 3)
	      (sign_extend:VNx2DI
		(truncate:VNx2SI
		  (match_operand:VNx2DI 2 "register_operand" "w")))]
	     UNSPEC_PRED_X)]
	  UNSPEC_ADR))]
  "TARGET_SVE && TARGET_NON_STREAMING"
  "adr\t%0.d, [%1.d, %2.d, sxtw]"
  "&& !CONSTANT_P (operands[3])"
  {
    operands[3] = CONSTM1_RTX (VNx2BImode);
  }
)

;; Same, but with the offset being zero-extended from the low 32 bits.
(define_insn "*aarch64_adr_uxtw_unspec"
  [(set (match_operand:VNx2DI 0 "register_operand" "=w")
	(unspec:VNx2DI
	  [(match_operand:VNx2DI 1 "register_operand" "w")
	   (and:VNx2DI
	     (match_operand:VNx2DI 2 "register_operand" "w")
	     (match_operand:VNx2DI 3 "aarch64_sve_uxtw_immediate"))]
	  UNSPEC_ADR))]
  "TARGET_SVE && TARGET_NON_STREAMING"
  "adr\t%0.d, [%1.d, %2.d, uxtw]"
)

;; Same, matching as a PLUS rather than unspec.
(define_insn "*aarch64_adr_uxtw_and"
  [(set (match_operand:VNx2DI 0 "register_operand" "=w")
	(plus:VNx2DI
	  (and:VNx2DI
	    (match_operand:VNx2DI 2 "register_operand" "w")
	    (match_operand:VNx2DI 3 "aarch64_sve_uxtw_immediate"))
	  (match_operand:VNx2DI 1 "register_operand" "w")))]
  "TARGET_SVE && TARGET_NON_STREAMING"
  "adr\t%0.d, [%1.d, %2.d, uxtw]"
)

;; ADR with a nonzero shift.
(define_expand "@aarch64_adr<mode>_shift"
  [(set (match_operand:SVE_FULL_SDI 0 "register_operand")
	(plus:SVE_FULL_SDI
	  (unspec:SVE_FULL_SDI
	    [(match_dup 4)
	     (ashift:SVE_FULL_SDI
	       (match_operand:SVE_FULL_SDI 2 "register_operand")
	       (match_operand:SVE_FULL_SDI 3 "const_1_to_3_operand"))]
	    UNSPEC_PRED_X)
	  (match_operand:SVE_FULL_SDI 1 "register_operand")))]
  "TARGET_SVE && TARGET_NON_STREAMING"
  {
    operands[4] = CONSTM1_RTX (<VPRED>mode);
  }
)

(define_insn_and_rewrite "*aarch64_adr<mode>_shift"
  [(set (match_operand:SVE_24I 0 "register_operand" "=w")
	(plus:SVE_24I
	  (unspec:SVE_24I
	    [(match_operand 4)
	     (ashift:SVE_24I
	       (match_operand:SVE_24I 2 "register_operand" "w")
	       (match_operand:SVE_24I 3 "const_1_to_3_operand"))]
	    UNSPEC_PRED_X)
	  (match_operand:SVE_24I 1 "register_operand" "w")))]
  "TARGET_SVE && TARGET_NON_STREAMING"
  "adr\t%0.<Vctype>, [%1.<Vctype>, %2.<Vctype>, lsl %3]"
  "&& !CONSTANT_P (operands[4])"
  {
    operands[4] = CONSTM1_RTX (<VPRED>mode);
  }
)

;; Same, but with the index being sign-extended from the low 32 bits.
(define_insn_and_rewrite "*aarch64_adr_shift_sxtw"
  [(set (match_operand:VNx2DI 0 "register_operand" "=w")
	(plus:VNx2DI
	  (unspec:VNx2DI
	    [(match_operand 4)
	     (ashift:VNx2DI
	       (unspec:VNx2DI
		 [(match_operand 5)
		  (sign_extend:VNx2DI
		    (truncate:VNx2SI
		      (match_operand:VNx2DI 2 "register_operand" "w")))]
		 UNSPEC_PRED_X)
	       (match_operand:VNx2DI 3 "const_1_to_3_operand"))]
	    UNSPEC_PRED_X)
	  (match_operand:VNx2DI 1 "register_operand" "w")))]
  "TARGET_SVE && TARGET_NON_STREAMING"
  "adr\t%0.d, [%1.d, %2.d, sxtw %3]"
  "&& (!CONSTANT_P (operands[4]) || !CONSTANT_P (operands[5]))"
  {
    operands[5] = operands[4] = CONSTM1_RTX (VNx2BImode);
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
  "TARGET_SVE && TARGET_NON_STREAMING"
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
(define_expand "<su>abd<mode>3"
  [(use (match_operand:SVE_I 0 "register_operand"))
   (USMAX:SVE_I
     (match_operand:SVE_I 1 "register_operand")
     (match_operand:SVE_I 2 "register_operand"))]
  "TARGET_SVE"
  {
    rtx pred = aarch64_ptrue_reg (<VPRED>mode);
    emit_insn (gen_aarch64_pred_<su>abd<mode> (operands[0], pred, operands[1],
					       operands[2]));
    DONE;
  }
)

;; Predicated integer absolute difference.
(define_insn "@aarch64_pred_<su>abd<mode>"
  [(set (match_operand:SVE_I 0 "register_operand")
	(minus:SVE_I
	  (unspec:SVE_I
	    [(match_operand:<VPRED> 1 "register_operand")
	     (USMAX:SVE_I
	       (match_operand:SVE_I 2 "register_operand")
	       (match_operand:SVE_I 3 "register_operand"))]
	    UNSPEC_PRED_X)
	  (unspec:SVE_I
	    [(match_dup 1)
	     (<max_opp>:SVE_I
	       (match_dup 2)
	       (match_dup 3))]
	    UNSPEC_PRED_X)))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2  , 3 ; attrs: movprfx ]
     [ w        , Upl , %0 , w ; *              ] <su>abd\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
     [ ?&w      , Upl , w  , w ; yes            ] movprfx\t%0, %2\;<su>abd\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
  }
)

(define_expand "@aarch64_cond_<su>abd<mode>"
  [(set (match_operand:SVE_FULL_I 0 "register_operand")
	(unspec:SVE_FULL_I
	  [(match_operand:<VPRED> 1 "register_operand")
	   (minus:SVE_FULL_I
	     (unspec:SVE_FULL_I
	       [(match_dup 1)
		(USMAX:SVE_FULL_I
		  (match_operand:SVE_FULL_I 2 "register_operand")
		  (match_operand:SVE_FULL_I 3 "register_operand"))]
	       UNSPEC_PRED_X)
	     (unspec:SVE_FULL_I
	       [(match_dup 1)
		(<max_opp>:SVE_FULL_I
		  (match_dup 2)
		  (match_dup 3))]
	       UNSPEC_PRED_X))
	   (match_operand:SVE_FULL_I 4 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE"
{
  if (rtx_equal_p (operands[3], operands[4]))
    std::swap (operands[2], operands[3]);
})

;; Predicated integer absolute difference, merging with the first input.
(define_insn_and_rewrite "*aarch64_cond_<su>abd<mode>_2"
  [(set (match_operand:SVE_I 0 "register_operand")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand")
	   (minus:SVE_I
	     (unspec:SVE_I
	       [(match_operand 4)
		(USMAX:SVE_I
		  (match_operand:SVE_I 2 "register_operand")
		  (match_operand:SVE_I 3 "register_operand"))]
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
  {@ [ cons: =0 , 1   , 2 , 3 ; attrs: movprfx ]
     [ w        , Upl , 0 , w ; *              ] <su>abd\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
     [ ?&w      , Upl , w , w ; yes            ] movprfx\t%0, %2\;<su>abd\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
  }
  "&& (!CONSTANT_P (operands[4]) || !CONSTANT_P (operands[5]))"
  {
    operands[4] = operands[5] = CONSTM1_RTX (<VPRED>mode);
  }
)

;; Predicated integer absolute difference, merging with the second input.
(define_insn_and_rewrite "*aarch64_cond_<su>abd<mode>_3"
  [(set (match_operand:SVE_I 0 "register_operand")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand")
	   (minus:SVE_I
	     (unspec:SVE_I
	       [(match_operand 4)
		(USMAX:SVE_I
		  (match_operand:SVE_I 2 "register_operand")
		  (match_operand:SVE_I 3 "register_operand"))]
	       UNSPEC_PRED_X)
	     (unspec:SVE_I
	       [(match_operand 5)
		(<max_opp>:SVE_I
		  (match_dup 2)
		  (match_dup 3))]
	       UNSPEC_PRED_X))
	   (match_dup 3)]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2 , 3 ; attrs: movprfx ]
     [ w        , Upl , w , 0 ; *              ] <su>abd\t%0.<Vetype>, %1/m, %0.<Vetype>, %2.<Vetype>
     [ ?&w      , Upl , w , w ; yes            ] movprfx\t%0, %3\;<su>abd\t%0.<Vetype>, %1/m, %0.<Vetype>, %2.<Vetype>
  }
  "&& (!CONSTANT_P (operands[4]) || !CONSTANT_P (operands[5]))"
  {
    operands[4] = operands[5] = CONSTM1_RTX (<VPRED>mode);
  }
)

;; Predicated integer absolute difference, merging with an independent value.
(define_insn_and_rewrite "*aarch64_cond_<su>abd<mode>_any"
  [(set (match_operand:SVE_I 0 "register_operand")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand")
	   (minus:SVE_I
	     (unspec:SVE_I
	       [(match_operand 5)
		(USMAX:SVE_I
		  (match_operand:SVE_I 2 "register_operand")
		  (match_operand:SVE_I 3 "register_operand"))]
	       UNSPEC_PRED_X)
	     (unspec:SVE_I
	       [(match_operand 6)
		(<max_opp>:SVE_I
		  (match_dup 2)
		  (match_dup 3))]
	       UNSPEC_PRED_X))
	   (match_operand:SVE_I 4 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE
   && !rtx_equal_p (operands[2], operands[4])
   && !rtx_equal_p (operands[3], operands[4])"
  {@ [ cons: =0 , 1   , 2 , 3 , 4   ]
     [ &w       , Upl , 0 , w , Dz  ] movprfx\t%0.<Vetype>, %1/z, %0.<Vetype>\;<su>abd\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
     [ &w       , Upl , w , 0 , Dz  ] movprfx\t%0.<Vetype>, %1/z, %0.<Vetype>\;<su>abd\t%0.<Vetype>, %1/m, %0.<Vetype>, %2.<Vetype>
     [ &w       , Upl , w , w , Dz  ] movprfx\t%0.<Vetype>, %1/z, %2.<Vetype>\;<su>abd\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
     [ &w       , Upl , w , w , 0   ] movprfx\t%0.<Vetype>, %1/m, %2.<Vetype>\;<su>abd\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
     [ ?&w      , Upl , w , w , w   ] #
  }
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
;; ---- [INT] Saturating addition and subtraction
;; -------------------------------------------------------------------------
;; - SQADD
;; - SQSUB
;; - UQADD
;; - UQSUB
;; -------------------------------------------------------------------------

;; Unpredicated saturating signed addition and subtraction.
(define_insn "<su_optab>s<addsub><mode>3"
  [(set (match_operand:SVE_FULL_I 0 "register_operand")
	(SBINQOPS:SVE_FULL_I
	  (match_operand:SVE_FULL_I 1 "register_operand")
	  (match_operand:SVE_FULL_I 2 "aarch64_sve_sqadd_operand")))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1 , 2   ; attrs: movprfx ]
     [ w        , 0 , vsQ ; *              ] <binqops_op>\t%0.<Vetype>, %0.<Vetype>, #%D2
     [ w        , 0 , vsS ; *              ] <binqops_op_rev>\t%0.<Vetype>, %0.<Vetype>, #%N2
     [ ?&w      , w , vsQ ; yes            ] movprfx\t%0, %1\;<binqops_op>\t%0.<Vetype>, %0.<Vetype>, #%D2
     [ ?&w      , w , vsS ; yes            ] movprfx\t%0, %1\;<binqops_op_rev>\t%0.<Vetype>, %0.<Vetype>, #%N2
     [ w        , w , w   ; *              ] <binqops_op>\t%0.<Vetype>, %1.<Vetype>, %2.<Vetype>
  }
)

;; Unpredicated saturating unsigned addition and subtraction.
(define_insn "<su_optab>s<addsub><mode>3"
  [(set (match_operand:SVE_FULL_I 0 "register_operand")
	(UBINQOPS:SVE_FULL_I
	  (match_operand:SVE_FULL_I 1 "register_operand")
	  (match_operand:SVE_FULL_I 2 "aarch64_sve_arith_operand")))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1 , 2   ; attrs: movprfx ]
     [ w        , 0 , vsa ; *              ] <binqops_op>\t%0.<Vetype>, %0.<Vetype>, #%D2
     [ ?&w      , w , vsa ; yes            ] movprfx\t%0, %1\;<binqops_op>\t%0.<Vetype>, %0.<Vetype>, #%D2
     [ w        , w , w   ; *              ] <binqops_op>\t%0.<Vetype>, %1.<Vetype>, %2.<Vetype>
  }
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
	   (unspec:SVE_I
	     [(match_operand:SVE_I 1 "register_operand")
	      (match_operand:SVE_I 2 "register_operand")]
	     MUL_HIGHPART)]
	  UNSPEC_PRED_X))]
  "TARGET_SVE"
  {
    operands[3] = aarch64_ptrue_reg (<VPRED>mode);
  }
)

;; Predicated highpart multiplication.
(define_insn "@aarch64_pred_<optab><mode>"
  [(set (match_operand:SVE_I 0 "register_operand")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_I
	     [(match_operand:SVE_I 2 "register_operand")
	      (match_operand:SVE_I 3 "register_operand")]
	     MUL_HIGHPART)]
	  UNSPEC_PRED_X))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2  , 3 ; attrs: movprfx ]
     [ w        , Upl , %0 , w ; *              ] <su>mulh\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
     [ ?&w      , Upl , w  , w ; yes            ] movprfx\t%0, %2\;<su>mulh\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
  }
)

;; Predicated highpart multiplications with merging.
(define_expand "@cond_<optab><mode>"
  [(set (match_operand:SVE_FULL_I 0 "register_operand")
	(unspec:SVE_FULL_I
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_I
	     [(match_operand:SVE_FULL_I 2 "register_operand")
	      (match_operand:SVE_FULL_I 3 "register_operand")]
	     MUL_HIGHPART)
	   (match_operand:SVE_FULL_I 4 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE"
{
  /* Only target code is aware of these operations, so we don't need
     to handle the fully-general case.  */
  gcc_assert (rtx_equal_p (operands[2], operands[4])
	      || CONSTANT_P (operands[4]));
})

;; Predicated highpart multiplications, merging with the first input.
(define_insn "*cond_<optab><mode>_2"
  [(set (match_operand:SVE_FULL_I 0 "register_operand")
	(unspec:SVE_FULL_I
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_I
	     [(match_operand:SVE_FULL_I 2 "register_operand")
	      (match_operand:SVE_FULL_I 3 "register_operand")]
	     MUL_HIGHPART)
	   (match_dup 2)]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2 , 3 ; attrs: movprfx ]
     [ w        , Upl , 0 , w ; *              ] <sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
     [ ?&w      , Upl , w , w ; yes            ] movprfx\t%0, %2\;<sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
  }
)

;; Predicated highpart multiplications, merging with zero.
(define_insn "*cond_<optab><mode>_z"
  [(set (match_operand:SVE_FULL_I 0 "register_operand")
	(unspec:SVE_FULL_I
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_I
	     [(match_operand:SVE_FULL_I 2 "register_operand")
	      (match_operand:SVE_FULL_I 3 "register_operand")]
	     MUL_HIGHPART)
	   (match_operand:SVE_FULL_I 4 "aarch64_simd_imm_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2  , 3  ]
     [ &w       , Upl , %0 , w  ] movprfx\t%0.<Vetype>, %1/z, %0.<Vetype>\;<sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
     [ &w       , Upl , w  , w  ] movprfx\t%0.<Vetype>, %1/z, %2.<Vetype>\;<sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
  }
  [(set_attr "movprfx" "yes")])

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
;; SVE has vector integer divisions, unlike Advanced SIMD.
;; We can use it with Advanced SIMD modes to expose the V2DI and V4SI
;; optabs to the midend.
(define_expand "<optab><mode>3"
  [(set (match_operand:SVE_FULL_SDI_SIMD 0 "register_operand")
	(unspec:SVE_FULL_SDI_SIMD
	  [(match_dup 3)
	   (SVE_INT_BINARY_SD:SVE_FULL_SDI_SIMD
	     (match_operand:SVE_FULL_SDI_SIMD 1 "register_operand")
	     (match_operand:SVE_FULL_SDI_SIMD 2 "register_operand"))]
	  UNSPEC_PRED_X))]
  "TARGET_SVE"
  {
    operands[3] = aarch64_ptrue_reg (<VPRED>mode);
  }
)

;; Integer division predicated with a PTRUE.
(define_insn "@aarch64_pred_<optab><mode>"
  [(set (match_operand:SVE_FULL_SDI_SIMD 0 "register_operand")
	(unspec:SVE_FULL_SDI_SIMD
	  [(match_operand:<VPRED> 1 "register_operand")
	   (SVE_INT_BINARY_SD:SVE_FULL_SDI_SIMD
	     (match_operand:SVE_FULL_SDI_SIMD 2 "register_operand")
	     (match_operand:SVE_FULL_SDI_SIMD 3 "register_operand"))]
	  UNSPEC_PRED_X))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2 , 3 ; attrs: movprfx ]
     [ w        , Upl , 0 , w ; *              ] <sve_int_op>\t%Z0.<Vetype>, %1/m, %Z0.<Vetype>, %Z3.<Vetype>
     [ w        , Upl , w , 0 ; *              ] <sve_int_op>r\t%Z0.<Vetype>, %1/m, %Z0.<Vetype>, %Z2.<Vetype>
     [ ?&w      , Upl , w , w ; yes            ] movprfx\t%Z0, %Z2\;<sve_int_op>\t%Z0.<Vetype>, %1/m, %Z0.<Vetype>, %Z3.<Vetype>
  }
)

;; Predicated integer division with merging.
(define_expand "@cond_<optab><mode>"
  [(set (match_operand:SVE_FULL_SDI 0 "register_operand")
	(unspec:SVE_FULL_SDI
	  [(match_operand:<VPRED> 1 "register_operand")
	   (SVE_INT_BINARY_SD:SVE_FULL_SDI
	     (match_operand:SVE_FULL_SDI 2 "register_operand")
	     (match_operand:SVE_FULL_SDI 3 "register_operand"))
	   (match_operand:SVE_FULL_SDI 4 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE"
)

;; Predicated integer division, merging with the first input.
(define_insn "*cond_<optab><mode>_2"
  [(set (match_operand:SVE_FULL_SDI 0 "register_operand")
	(unspec:SVE_FULL_SDI
	  [(match_operand:<VPRED> 1 "register_operand")
	   (SVE_INT_BINARY_SD:SVE_FULL_SDI
	     (match_operand:SVE_FULL_SDI 2 "register_operand")
	     (match_operand:SVE_FULL_SDI 3 "register_operand"))
	   (match_dup 2)]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2 , 3 ; attrs: movprfx ]
     [ w        , Upl , 0 , w ; *              ] <sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
     [ ?&w      , Upl , w , w ; yes            ] movprfx\t%0, %2\;<sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
  }
)

;; Predicated integer division, merging with the second input.
(define_insn "*cond_<optab><mode>_3"
  [(set (match_operand:SVE_FULL_SDI 0 "register_operand")
	(unspec:SVE_FULL_SDI
	  [(match_operand:<VPRED> 1 "register_operand")
	   (SVE_INT_BINARY_SD:SVE_FULL_SDI
	     (match_operand:SVE_FULL_SDI 2 "register_operand")
	     (match_operand:SVE_FULL_SDI 3 "register_operand"))
	   (match_dup 3)]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2 , 3 ; attrs: movprfx ]
     [ w        , Upl , w , 0 ; *              ] <sve_int_op_rev>\t%0.<Vetype>, %1/m, %0.<Vetype>, %2.<Vetype>
     [ ?&w      , Upl , w , w ; yes            ] movprfx\t%0, %3\;<sve_int_op_rev>\t%0.<Vetype>, %1/m, %0.<Vetype>, %2.<Vetype>
  }
)

;; Predicated integer division, merging with an independent value.
(define_insn_and_rewrite "*cond_<optab><mode>_any"
  [(set (match_operand:SVE_FULL_SDI 0 "register_operand")
	(unspec:SVE_FULL_SDI
	  [(match_operand:<VPRED> 1 "register_operand")
	   (SVE_INT_BINARY_SD:SVE_FULL_SDI
	     (match_operand:SVE_FULL_SDI 2 "register_operand")
	     (match_operand:SVE_FULL_SDI 3 "register_operand"))
	   (match_operand:SVE_FULL_SDI 4 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE
   && !rtx_equal_p (operands[2], operands[4])
   && !rtx_equal_p (operands[3], operands[4])"
  {@ [ cons: =0 , 1   , 2 , 3 , 4   ]
     [ &w       , Upl , 0 , w , Dz  ] movprfx\t%0.<Vetype>, %1/z, %0.<Vetype>\;<sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
     [ &w       , Upl , w , 0 , Dz  ] movprfx\t%0.<Vetype>, %1/z, %0.<Vetype>\;<sve_int_op_rev>\t%0.<Vetype>, %1/m, %0.<Vetype>, %2.<Vetype>
     [ &w       , Upl , w , w , Dz  ] movprfx\t%0.<Vetype>, %1/z, %2.<Vetype>\;<sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
     [ &w       , Upl , w , w , 0   ] movprfx\t%0.<Vetype>, %1/m, %2.<Vetype>\;<sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
     [ ?&w      , Upl , w , w , w   ] #
  }
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
  [(set (match_operand:SVE_I 0 "register_operand")
	(LOGICAL:SVE_I
	  (match_operand:SVE_I 1 "register_operand")
	  (match_operand:SVE_I 2 "aarch64_sve_logical_operand")))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1  , 2   ; attrs: movprfx ]
     [ w        , %0 , vsl ; *              ] <logical>\t%0.<Vetype>, %0.<Vetype>, #%C2
     [ ?w       , w  , vsl ; yes            ] movprfx\t%0, %1\;<logical>\t%0.<Vetype>, %0.<Vetype>, #%C2
     [ w        , w  , w   ; *              ] <logical>\t%0.d, %1.d, %2.d
  }
)

;; Merging forms are handled through SVE_INT_BINARY.

;; -------------------------------------------------------------------------
;; ---- [INT] Binary logical operations (inverted second input)
;; -------------------------------------------------------------------------
;; Includes:
;; - BIC
;; -------------------------------------------------------------------------

;; Unpredicated BIC; andn named pattern.
(define_expand "andn<mode>3"
  [(set (match_operand:SVE_I 0 "register_operand")
	(and:SVE_I
	  (unspec:SVE_I
	    [(match_dup 3)
	     (not:SVE_I (match_operand:SVE_I 2 "register_operand"))]
	    UNSPEC_PRED_X)
	  (match_operand:SVE_I 1 "register_operand")))]
  "TARGET_SVE"
  {
    operands[3] = CONSTM1_RTX (<VPRED>mode);
  }
)

;; Predicated BIC.
(define_insn_and_rewrite "*bic<mode>3"
  [(set (match_operand:SVE_I 0 "register_operand" "=w")
	(and:SVE_I
	  (unspec:SVE_I
	    [(match_operand 3)
	     (not:SVE_I
	       (match_operand:SVE_I 2 "register_operand" "w"))]
	    UNSPEC_PRED_X)
	  (match_operand:SVE_I 1 "register_operand" "w")))]
  "TARGET_SVE"
  "bic\t%0.d, %1.d, %2.d"
  "&& !CONSTANT_P (operands[3])"
  {
    operands[3] = CONSTM1_RTX (<VPRED>mode);
  }
)

;; Predicated BIC with merging.
(define_expand "@cond_bic<mode>"
  [(set (match_operand:SVE_FULL_I 0 "register_operand")
	(unspec:SVE_FULL_I
	  [(match_operand:<VPRED> 1 "register_operand")
	   (and:SVE_FULL_I
	     (not:SVE_FULL_I (match_operand:SVE_FULL_I 3 "register_operand"))
	     (match_operand:SVE_FULL_I 2 "register_operand"))
	   (match_operand:SVE_FULL_I 4 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE"
)

;; Predicated integer BIC, merging with the first input.
(define_insn "*cond_bic<mode>_2"
  [(set (match_operand:SVE_I 0 "register_operand")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand")
	   (and:SVE_I
	     (not:SVE_I
	       (match_operand:SVE_I 3 "register_operand"))
	     (match_operand:SVE_I 2 "register_operand"))
	   (match_dup 2)]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2 , 3 ; attrs: movprfx ]
     [ w        , Upl , 0 , w ; *              ] bic\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
     [ ?&w      , Upl , w , w ; yes            ] movprfx\t%0, %2\;bic\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
  }
)

;; Predicated integer BIC, merging with an independent value.
(define_insn_and_rewrite "*cond_bic<mode>_any"
  [(set (match_operand:SVE_I 0 "register_operand")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand")
	   (and:SVE_I
	     (not:SVE_I
	       (match_operand:SVE_I 3 "register_operand"))
	     (match_operand:SVE_I 2 "register_operand"))
	   (match_operand:SVE_I 4 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE && !rtx_equal_p (operands[2], operands[4])"
  {@ [ cons: =0 , 1   , 2 , 3 , 4   ]
     [ &w       , Upl , 0 , w , Dz  ] movprfx\t%0.<Vetype>, %1/z, %0.<Vetype>\;bic\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
     [ &w       , Upl , w , w , Dz  ] movprfx\t%0.<Vetype>, %1/z, %2.<Vetype>\;bic\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
     [ &w       , Upl , w , w , 0   ] movprfx\t%0.<Vetype>, %1/m, %2.<Vetype>\;bic\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
     [ ?&w      , Upl , w , w , w   ] #
  }
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
;; ---- [INT] Shifts (rounding towards -Inf)
;; -------------------------------------------------------------------------
;; Includes:
;; - ASR
;; - ASRR
;; - LSL
;; - LSLR
;; - LSR
;; - LSRR
;; -------------------------------------------------------------------------

;; Unpredicated shift by a scalar, which expands into one of the vector
;; shifts below.
(define_expand "<ASHIFT:optab><mode>3"
  [(set (match_operand:SVE_I 0 "register_operand")
	(ASHIFT:SVE_I
	  (match_operand:SVE_I 1 "register_operand")
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
	amount = convert_to_mode (<VEL>mode, operands[2], 0);
	amount = expand_vector_broadcast (<MODE>mode, amount);
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
(define_insn_and_split "@aarch64_pred_<optab><mode>"
  [(set (match_operand:SVE_I 0 "register_operand")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand")
	   (ASHIFT:SVE_I
	     (match_operand:SVE_I 2 "register_operand")
	     (match_operand:SVE_I 3 "aarch64_sve_<lr>shift_operand"))]
	  UNSPEC_PRED_X))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2 , 3     ; attrs: movprfx ]
     [ w        , Upl , w , D<lr> ; *              ] #
     [ w        , Upl , 0 , w     ; *              ] <shift>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
     [ w        , Upl , w , 0     ; *              ] <shift>r\t%0.<Vetype>, %1/m, %3.<Vetype>, %2.<Vetype>
     [ ?&w      , Upl , w , w     ; yes            ] movprfx\t%0, %2\;<shift>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
  }
  "&& reload_completed
   && !register_operand (operands[3], <MODE>mode)"
  [(set (match_dup 0) (ASHIFT:SVE_I (match_dup 2) (match_dup 3)))]
  ""
)

;; Unpredicated shift operations by a constant (post-RA only).
;; These are generated by splitting a predicated instruction whose
;; predicate is unused.
(define_insn "*post_ra_v_ashl<mode>3"
  [(set (match_operand:SVE_I 0 "register_operand")
	(ashift:SVE_I
	  (match_operand:SVE_I 1 "register_operand")
	  (match_operand:SVE_I 2 "aarch64_simd_lshift_imm")))]
  "TARGET_SVE && reload_completed"
  {@ [ cons: =0 , 1 , 2   ]
     [ w	, w , vs1 ] add\t%0.<Vetype>, %1.<Vetype>, %1.<Vetype>
     [ w	, w , Dl  ] lsl\t%0.<Vetype>, %1.<Vetype>, #%2
  }
)

(define_insn "*post_ra_v_<optab><mode>3"
  [(set (match_operand:SVE_I 0 "register_operand" "=w")
	(SHIFTRT:SVE_I
	  (match_operand:SVE_I 1 "register_operand" "w")
	  (match_operand:SVE_I 2 "aarch64_simd_rshift_imm")))]
  "TARGET_SVE && reload_completed"
  "<shift>\t%0.<Vetype>, %1.<Vetype>, #%2"
)

;; Predicated integer shift, merging with the first input.
(define_insn "*cond_<optab><mode>_2_const"
  [(set (match_operand:SVE_I 0 "register_operand")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand")
	   (ASHIFT:SVE_I
	     (match_operand:SVE_I 2 "register_operand")
	     (match_operand:SVE_I 3 "aarch64_simd_<lr>shift_imm"))
	   (match_dup 2)]
	 UNSPEC_SEL))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2 ; attrs: movprfx ]
     [ w        , Upl , 0 ; *              ] <shift>\t%0.<Vetype>, %1/m, %0.<Vetype>, #%3
     [ ?&w      , Upl , w ; yes            ] movprfx\t%0, %2\;<shift>\t%0.<Vetype>, %1/m, %0.<Vetype>, #%3
  }
)

;; Predicated integer shift, merging with an independent value.
(define_insn_and_rewrite "*cond_<optab><mode>_any_const"
  [(set (match_operand:SVE_I 0 "register_operand")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand")
	   (ASHIFT:SVE_I
	     (match_operand:SVE_I 2 "register_operand")
	     (match_operand:SVE_I 3 "aarch64_simd_<lr>shift_imm"))
	   (match_operand:SVE_I 4 "aarch64_simd_reg_or_zero")]
	 UNSPEC_SEL))]
  "TARGET_SVE && !rtx_equal_p (operands[2], operands[4])"
  {@ [ cons: =0 , 1   , 2 , 4   ]
     [ w        , Upl , w , Dz  ] movprfx\t%0.<Vetype>, %1/z, %2.<Vetype>\;<shift>\t%0.<Vetype>, %1/m, %0.<Vetype>, #%3
     [ &w       , Upl , w , 0   ] movprfx\t%0.<Vetype>, %1/m, %2.<Vetype>\;<shift>\t%0.<Vetype>, %1/m, %0.<Vetype>, #%3
     [ ?&w      , Upl , w , w   ] #
  }
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

;; Unpredicated shifts of narrow elements by 64-bit amounts.
(define_insn "@aarch64_sve_<sve_int_op><mode>"
  [(set (match_operand:SVE_FULL_BHSI 0 "register_operand" "=w")
	(unspec:SVE_FULL_BHSI
	  [(match_operand:SVE_FULL_BHSI 1 "register_operand" "w")
	   (match_operand:VNx2DI 2 "register_operand" "w")]
	  SVE_SHIFT_WIDE))]
  "TARGET_SVE"
  "<sve_int_op>\t%0.<Vetype>, %1.<Vetype>, %2.d"
)

;; Merging predicated shifts of narrow elements by 64-bit amounts.
(define_expand "@cond_<sve_int_op><mode>"
  [(set (match_operand:SVE_FULL_BHSI 0 "register_operand")
	(unspec:SVE_FULL_BHSI
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_BHSI
	     [(match_operand:SVE_FULL_BHSI 2 "register_operand")
	      (match_operand:VNx2DI 3 "register_operand")]
	     SVE_SHIFT_WIDE)
	   (match_operand:SVE_FULL_BHSI 4 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE"
)

;; Predicated shifts of narrow elements by 64-bit amounts, merging with
;; the first input.
(define_insn "*cond_<sve_int_op><mode>_m"
  [(set (match_operand:SVE_FULL_BHSI 0 "register_operand")
	(unspec:SVE_FULL_BHSI
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_BHSI
	     [(match_operand:SVE_FULL_BHSI 2 "register_operand")
	      (match_operand:VNx2DI 3 "register_operand")]
	     SVE_SHIFT_WIDE)
	   (match_dup 2)]
	 UNSPEC_SEL))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2 , 3 ; attrs: movprfx ]
     [ w        , Upl , 0 , w ; *              ] <sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.d
     [ ?&w      , Upl , w , w ; yes            ] movprfx\t%0, %2\;<sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.d
  }
)

;; Predicated shifts of narrow elements by 64-bit amounts, merging with zero.
(define_insn "*cond_<sve_int_op><mode>_z"
  [(set (match_operand:SVE_FULL_BHSI 0 "register_operand")
	(unspec:SVE_FULL_BHSI
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_BHSI
	     [(match_operand:SVE_FULL_BHSI 2 "register_operand")
	      (match_operand:VNx2DI 3 "register_operand")]
	     SVE_SHIFT_WIDE)
	   (match_operand:SVE_FULL_BHSI 4 "aarch64_simd_imm_zero")]
	 UNSPEC_SEL))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2 , 3  ]
     [ &w       , Upl , 0 , w  ] movprfx\t%0.<Vetype>, %1/z, %0.<Vetype>\;<sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.d
     [ &w       , Upl , w , w  ] movprfx\t%0.<Vetype>, %1/z, %2.<Vetype>\;<sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.d
  }
  [(set_attr "movprfx" "yes")])

;; -------------------------------------------------------------------------
;; ---- [INT] Shifts (rounding towards 0)
;; -------------------------------------------------------------------------
;; Includes:
;; - ASRD
;; - SQSHLU (SVE2)
;; - SRSHR (SVE2)
;; - URSHR (SVE2)
;; -------------------------------------------------------------------------

;; Unpredicated ASRD.
(define_expand "sdiv_pow2<mode>3"
  [(set (match_operand:SVE_VDQ_I 0 "register_operand")
	(unspec:SVE_VDQ_I
	  [(match_dup 3)
	   (unspec:SVE_VDQ_I
	     [(match_operand:SVE_VDQ_I 1 "register_operand")
	      (match_operand 2 "aarch64_simd_rshift_imm")]
	     UNSPEC_ASRD)]
	 UNSPEC_PRED_X))]
  "TARGET_SVE"
  {
    operands[3] = aarch64_ptrue_reg (<VPRED>mode, <MODE>mode);
  }
)

;; Predicated ASRD.
(define_insn "*sdiv_pow2<mode>3"
  [(set (match_operand:SVE_VDQ_I 0 "register_operand")
	(unspec:SVE_VDQ_I
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_VDQ_I
	     [(match_operand:SVE_VDQ_I 2 "register_operand")
	      (match_operand:SVE_VDQ_I 3 "aarch64_simd_rshift_imm")]
	     UNSPEC_ASRD)]
	  UNSPEC_PRED_X))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2 ; attrs: movprfx ]
     [ w        , Upl , 0 ; *              ] asrd\t%Z0.<Vetype>, %1/m, %Z0.<Vetype>, #%3
     [ ?&w      , Upl , w ; yes            ] movprfx\t%Z0, %Z2\;asrd\t%Z0.<Vetype>, %1/m, %Z0.<Vetype>, #%3
  }
)

;; Predicated shift with merging.
(define_expand "@cond_<sve_int_op><mode>"
  [(set (match_operand:SVE_I 0 "register_operand")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_I
	     [(match_dup 5)
	      (unspec:SVE_I
		[(match_operand:SVE_I 2 "register_operand")
		 (match_operand:SVE_I 3 "aarch64_simd_<lr>shift_imm")]
		SVE_INT_SHIFT_IMM)]
	     UNSPEC_PRED_X)
	   (match_operand:SVE_I 4 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  {
    operands[5] = aarch64_ptrue_reg (<VPRED>mode);
  }
)

;; Predicated shift, merging with the first input.
(define_insn_and_rewrite "*cond_<sve_int_op><mode>_2"
  [(set (match_operand:SVE_I 0 "register_operand")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_I
	     [(match_operand 4)
	      (unspec:SVE_I
		[(match_operand:SVE_I 2 "register_operand")
		 (match_operand:SVE_I 3 "aarch64_simd_<lr>shift_imm")]
		SVE_INT_SHIFT_IMM)]
	     UNSPEC_PRED_X)
	   (match_dup 2)]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2 ; attrs: movprfx ]
     [ w        , Upl , 0 ; *              ] <sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, #%3
     [ ?&w      , Upl , w ; yes            ] movprfx\t%0, %2\;<sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, #%3
  }
  "&& !CONSTANT_P (operands[4])"
  {
    operands[4] = CONSTM1_RTX (<VPRED>mode);
  }
)

;; Predicated shift, merging with an independent value.
(define_insn_and_rewrite "*cond_<sve_int_op><mode>_any"
  [(set (match_operand:SVE_I 0 "register_operand")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_I
	     [(match_operand 5)
	      (unspec:SVE_I
		[(match_operand:SVE_I 2 "register_operand")
		 (match_operand:SVE_I 3 "aarch64_simd_<lr>shift_imm")]
		SVE_INT_SHIFT_IMM)]
	     UNSPEC_PRED_X)
	   (match_operand:SVE_I 4 "aarch64_simd_reg_or_zero")]
	 UNSPEC_SEL))]
  "TARGET_SVE && !rtx_equal_p (operands[2], operands[4])"
  {@ [ cons: =0 , 1   , 2 , 4   ]
     [ w        , Upl , w , Dz  ] movprfx\t%0.<Vetype>, %1/z, %2.<Vetype>\;<sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, #%3
     [ &w       , Upl , w , 0   ] movprfx\t%0.<Vetype>, %1/m, %2.<Vetype>\;<sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, #%3
     [ ?&w      , Upl , w , w   ] #
  }
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
;; ---- [FP<-INT] General binary arithmetic corresponding to unspecs
;; -------------------------------------------------------------------------
;; Includes:
;; - FSCALE
;; - FTSMUL
;; - FTSSEL
;; -------------------------------------------------------------------------

(define_expand "ldexp<mode>3"
 [(set (match_operand:GPF_HF 0 "register_operand")
       (unspec:GPF_HF
	 [(match_dup 3)
	  (const_int SVE_STRICT_GP)
	  (match_operand:GPF_HF 1 "register_operand")
	  (match_operand:<V_INT_EQUIV> 2 "register_operand")]
	 UNSPEC_COND_FSCALE))]
 "TARGET_SVE"
 {
   operands[3] = aarch64_ptrue_reg (<VPRED>mode,
				    GET_MODE_UNIT_SIZE (<MODE>mode));
 }
)

;; Unpredicated floating-point binary operations that take an integer as
;; their second operand.
(define_insn "@aarch64_sve_<optab><mode>"
  [(set (match_operand:SVE_FULL_F 0 "register_operand" "=w")
	(unspec:SVE_FULL_F
	  [(match_operand:SVE_FULL_F 1 "register_operand" "w")
	   (match_operand:<V_INT_EQUIV> 2 "register_operand" "w")]
	  SVE_FP_BINARY_INT))]
  "TARGET_SVE"
  "<sve_fp_op>\t%0.<Vetype>, %1.<Vetype>, %2.<Vetype>"
)

;; Predicated floating-point binary operations that take an integer
;; as their second operand.
(define_insn "@aarch64_pred_<optab><mode>"
  [(set (match_operand:SVE_FULL_F_SCALAR 0 "register_operand")
	(unspec:SVE_FULL_F_SCALAR
	  [(match_operand:<VPRED> 1 "register_operand")
	   (match_operand:SI 4 "aarch64_sve_gp_strictness")
	   (match_operand:SVE_FULL_F_SCALAR 2 "register_operand")
	   (match_operand:<V_INT_EQUIV> 3 "register_operand")]
	  SVE_COND_FP_BINARY_INT))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2 , 3 ; attrs: movprfx ]
     [ w        , Upl , 0 , w ; *              ] <sve_fp_op>\t%Z0.<Vetype>, %1/m, %Z0.<Vetype>, %Z3.<Vetype>
     [ ?&w      , Upl , w , w ; yes            ] movprfx\t%Z0, %Z2\;<sve_fp_op>\t%Z0.<Vetype>, %1/m, %Z0.<Vetype>, %Z3.<Vetype>
  }
)

;; Predicated floating-point binary operations with merging, taking an
;; integer as their second operand.
(define_expand "@cond_<optab><mode>"
  [(set (match_operand:SVE_FULL_F 0 "register_operand")
	(unspec:SVE_FULL_F
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_F
	     [(match_dup 1)
	      (const_int SVE_STRICT_GP)
	      (match_operand:SVE_FULL_F 2 "register_operand")
	      (match_operand:<V_INT_EQUIV> 3 "register_operand")]
	     SVE_COND_FP_BINARY_INT)
	   (match_operand:SVE_FULL_F 4 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE"
)

;; Predicated floating-point binary operations that take an integer as their
;; second operand, with inactive lanes coming from the first operand.
(define_insn_and_rewrite "*cond_<optab><mode>_2_relaxed"
  [(set (match_operand:SVE_FULL_F 0 "register_operand")
	(unspec:SVE_FULL_F
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_F
	     [(match_operand 4)
	      (const_int SVE_RELAXED_GP)
	      (match_operand:SVE_FULL_F 2 "register_operand")
	      (match_operand:<V_INT_EQUIV> 3 "register_operand")]
	     SVE_COND_FP_BINARY_INT)
	   (match_dup 2)]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2 , 3 ; attrs: movprfx ]
     [ w        , Upl , 0 , w ; *              ] <sve_fp_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
     [ ?&w      , Upl , w , w ; yes            ] movprfx\t%0, %2\;<sve_fp_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
  }
  "&& !rtx_equal_p (operands[1], operands[4])"
  {
    operands[4] = copy_rtx (operands[1]);
  }
)

(define_insn "*cond_<optab><mode>_2_strict"
  [(set (match_operand:SVE_FULL_F 0 "register_operand")
	(unspec:SVE_FULL_F
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_F
	     [(match_dup 1)
	      (const_int SVE_STRICT_GP)
	      (match_operand:SVE_FULL_F 2 "register_operand")
	      (match_operand:<V_INT_EQUIV> 3 "register_operand")]
	     SVE_COND_FP_BINARY_INT)
	   (match_dup 2)]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2 , 3 ; attrs: movprfx ]
     [ w        , Upl , 0 , w ; *              ] <sve_fp_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
     [ ?&w      , Upl , w , w ; yes            ] movprfx\t%0, %2\;<sve_fp_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
  }
)

;; Predicated floating-point binary operations that take an integer as
;; their second operand, with the values of inactive lanes being distinct
;; from the other inputs.
(define_insn_and_rewrite "*cond_<optab><mode>_any_relaxed"
  [(set (match_operand:SVE_FULL_F 0 "register_operand")
	(unspec:SVE_FULL_F
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_F
	     [(match_operand 5)
	      (const_int SVE_RELAXED_GP)
	      (match_operand:SVE_FULL_F 2 "register_operand")
	      (match_operand:<V_INT_EQUIV> 3 "register_operand")]
	     SVE_COND_FP_BINARY_INT)
	   (match_operand:SVE_FULL_F 4 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE && !rtx_equal_p (operands[2], operands[4])"
  {@ [ cons: =0 , 1   , 2 , 3 , 4   ]
     [ &w       , Upl , 0 , w , Dz  ] movprfx\t%0.<Vetype>, %1/z, %2.<Vetype>\;<sve_fp_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
     [ &w       , Upl , w , w , Dz  ] movprfx\t%0.<Vetype>, %1/z, %2.<Vetype>\;<sve_fp_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
     [ &w       , Upl , w , w , 0   ] movprfx\t%0.<Vetype>, %1/m, %2.<Vetype>\;<sve_fp_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
     [ ?&w      , Upl , w , w , w   ] #
  }
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

(define_insn_and_rewrite "*cond_<optab><mode>_any_strict"
  [(set (match_operand:SVE_FULL_F 0 "register_operand")
	(unspec:SVE_FULL_F
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_F
	     [(match_dup 1)
	      (const_int SVE_STRICT_GP)
	      (match_operand:SVE_FULL_F 2 "register_operand")
	      (match_operand:<V_INT_EQUIV> 3 "register_operand")]
	     SVE_COND_FP_BINARY_INT)
	   (match_operand:SVE_FULL_F 4 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE && !rtx_equal_p (operands[2], operands[4])"
  {@ [ cons: =0 , 1   , 2 , 3 , 4   ]
     [ &w       , Upl , 0 , w , Dz  ] movprfx\t%0.<Vetype>, %1/z, %2.<Vetype>\;<sve_fp_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
     [ &w       , Upl , w , w , Dz  ] movprfx\t%0.<Vetype>, %1/z, %2.<Vetype>\;<sve_fp_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
     [ &w       , Upl , w , w , 0   ] movprfx\t%0.<Vetype>, %1/m, %2.<Vetype>\;<sve_fp_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
     [ ?&w      , Upl , w , w , w   ] #
  }
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
;; ---- [FP] General binary arithmetic corresponding to rtx codes
;; -------------------------------------------------------------------------
;; Includes post-RA forms of:
;; - BFADD (SVE_B16B16)
;; - BFMUL (SVE_B16B16)
;; - BFSUB (SVE_B16B16)
;; - FADD
;; - FMUL
;; - FSUB
;; -------------------------------------------------------------------------

;; Split a predicated instruction whose predicate is unused into an
;; unpredicated instruction.
(define_split
  [(set (match_operand:SVE_FULL_F_BF 0 "register_operand")
	(unspec:SVE_FULL_F_BF
	  [(match_operand:<VPRED> 1 "register_operand")
	   (match_operand:SI 4 "aarch64_sve_gp_strictness")
	   (match_operand:SVE_FULL_F_BF 2 "register_operand")
	   (match_operand:SVE_FULL_F_BF 3 "register_operand")]
	  <SVE_COND_FP>))]
  "TARGET_SVE
   && reload_completed
   && INTVAL (operands[4]) == SVE_RELAXED_GP"
  [(set (match_dup 0)
	(SVE_UNPRED_FP_BINARY:SVE_FULL_F_BF (match_dup 2) (match_dup 3)))]
)

;; Unpredicated floating-point binary operations (post-RA only).
;; These are generated by the split above.
(define_insn "*post_ra_<sve_fp_op><mode>3"
  [(set (match_operand:SVE_FULL_F_BF 0 "register_operand" "=w")
	(SVE_UNPRED_FP_BINARY:SVE_FULL_F_BF
	  (match_operand:SVE_FULL_F_BF 1 "register_operand" "w")
	  (match_operand:SVE_FULL_F_BF 2 "register_operand" "w")))]
  "TARGET_SVE && reload_completed"
  "<b><sve_fp_op>\t%0.<Vetype>, %1.<Vetype>, %2.<Vetype>")

;; -------------------------------------------------------------------------
;; ---- [FP] General binary arithmetic corresponding to unspecs
;; -------------------------------------------------------------------------
;; Includes merging forms of:
;; - BFADD   (SVE_B16B16)
;; - BFMAX   (SVE_B16B16)
;; - BFMAXNM (SVE_B16B16)
;; - BFMIN   (SVE_B16B16)
;; - BFMINNM (SVE_B16B16)
;; - BFMUL   (SVE_B16B16)
;; - BFSUB   (SVE_B16B16)
;; - FADD    (constant forms handled in the "Addition" section)
;; - FDIV
;; - FDIVR
;; - FMAX
;; - FMAXNM  (including #0.0 and #1.0)
;; - FMIN
;; - FMINNM  (including #0.0 and #1.0)
;; - FMUL    (including #0.5 and #2.0)
;; - FMULX
;; - FRECPS
;; - FRSQRTS
;; - FSUB    (constant forms handled in the "Addition" section)
;; - FSUBR   (constant forms handled in the "Subtraction" section)
;; -------------------------------------------------------------------------

;; Unpredicated floating-point binary operations.
(define_insn "@aarch64_sve_<optab><mode>"
  [(set (match_operand:SVE_FULL_F 0 "register_operand" "=w")
	(unspec:SVE_FULL_F
	  [(match_operand:SVE_FULL_F 1 "register_operand" "w")
	   (match_operand:SVE_FULL_F 2 "register_operand" "w")]
	  SVE_FP_BINARY))]
  "TARGET_SVE"
  "<sve_fp_op>\t%0.<Vetype>, %1.<Vetype>, %2.<Vetype>"
)

;; Unpredicated floating-point binary operations that need to be predicated
;; for SVE.
(define_expand "<optab><mode>3"
  [(set (match_operand:SVE_FULL_F_BF 0 "register_operand")
	(unspec:SVE_FULL_F_BF
	  [(match_dup 3)
	   (const_int SVE_RELAXED_GP)
	   (match_operand:SVE_FULL_F_BF 1 "<sve_pred_fp_rhs1_operand>")
	   (match_operand:SVE_FULL_F_BF 2 "<sve_pred_fp_rhs2_operand>")]
	  SVE_COND_FP_BINARY_OPTAB))]
  "TARGET_SVE && (<supports_bf16> || !<is_bf16>)"
  {
    operands[3] = aarch64_ptrue_reg (<VPRED>mode);
  }
)

;; Predicated floating-point binary operations that have no immediate forms.
(define_insn "@aarch64_pred_<optab><mode>"
  [(set (match_operand:SVE_FULL_F 0 "register_operand")
	(unspec:SVE_FULL_F
	  [(match_operand:<VPRED> 1 "register_operand")
	   (match_operand:SI 4 "aarch64_sve_gp_strictness")
	   (match_operand:SVE_FULL_F 2 "register_operand")
	   (match_operand:SVE_FULL_F 3 "register_operand")]
	  SVE_COND_FP_BINARY_REG))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2 , 3 ; attrs: movprfx ]
     [ w        , Upl , 0 , w ; *              ] <sve_fp_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
     [ w        , Upl , w , 0 ; *              ] <sve_fp_op_rev>\t%0.<Vetype>, %1/m, %0.<Vetype>, %2.<Vetype>
     [ ?&w      , Upl , w , w ; yes            ] movprfx\t%0, %2\;<sve_fp_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
  }
)

;; Predicated floating-point operations with merging.
(define_expand "@cond_<optab><mode>"
  [(set (match_operand:SVE_FULL_F_BF 0 "register_operand")
	(unspec:SVE_FULL_F_BF
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_F_BF
	     [(match_dup 1)
	      (const_int SVE_STRICT_GP)
	      (match_operand:SVE_FULL_F_BF 2 "<sve_pred_fp_rhs1_operand>")
	      (match_operand:SVE_FULL_F_BF 3 "<sve_pred_fp_rhs2_operand>")]
	     SVE_COND_FP_BINARY)
	   (match_operand:SVE_FULL_F_BF 4 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE && (<supports_bf16> || !<is_bf16>)"
)

;; Predicated floating-point operations, merging with the first input.
(define_insn_and_rewrite "*cond_<optab><mode>_2_relaxed"
  [(set (match_operand:SVE_FULL_F_BF 0 "register_operand")
	(unspec:SVE_FULL_F_BF
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_F_BF
	     [(match_operand 4)
	      (const_int SVE_RELAXED_GP)
	      (match_operand:SVE_FULL_F_BF 2 "register_operand")
	      (match_operand:SVE_FULL_F_BF 3 "register_operand")]
	     SVE_COND_FP_BINARY)
	   (match_dup 2)]
	  UNSPEC_SEL))]
  "TARGET_SVE && (<supports_bf16> || !<is_bf16>)"
  {@ [ cons: =0 , 1   , 2 , 3 ; attrs: movprfx ]
     [ w        , Upl , 0 , w ; *              ] <b><sve_fp_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
     [ ?&w      , Upl , w , w ; yes            ] movprfx\t%0, %2\;<b><sve_fp_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
  }
  "&& !rtx_equal_p (operands[1], operands[4])"
  {
    operands[4] = copy_rtx (operands[1]);
  }
)

(define_insn "*cond_<optab><mode>_2_strict"
  [(set (match_operand:SVE_FULL_F_BF 0 "register_operand")
	(unspec:SVE_FULL_F_BF
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_F_BF
	     [(match_dup 1)
	      (const_int SVE_STRICT_GP)
	      (match_operand:SVE_FULL_F_BF 2 "register_operand")
	      (match_operand:SVE_FULL_F_BF 3 "register_operand")]
	     SVE_COND_FP_BINARY)
	   (match_dup 2)]
	  UNSPEC_SEL))]
  "TARGET_SVE && (<supports_bf16> || !<is_bf16>)"
  {@ [ cons: =0 , 1   , 2 , 3 ; attrs: movprfx ]
     [ w        , Upl , 0 , w ; *              ] <b><sve_fp_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
     [ ?&w      , Upl , w , w ; yes            ] movprfx\t%0, %2\;<b><sve_fp_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
  }
)

;; Same for operations that take a 1-bit constant.
(define_insn_and_rewrite "*cond_<optab><mode>_2_const_relaxed"
  [(set (match_operand:SVE_FULL_F 0 "register_operand")
	(unspec:SVE_FULL_F
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_F
	     [(match_operand 4)
	      (const_int SVE_RELAXED_GP)
	      (match_operand:SVE_FULL_F 2 "register_operand")
	      (match_operand:SVE_FULL_F 3 "<sve_pred_fp_rhs2_immediate>")]
	     SVE_COND_FP_BINARY_I1)
	   (match_dup 2)]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2 ; attrs: movprfx ]
     [ w        , Upl , 0 ; *              ] <sve_fp_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, #%3
     [ ?w       , Upl , w ; yes            ] movprfx\t%0, %2\;<sve_fp_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, #%3
  }
  "&& !rtx_equal_p (operands[1], operands[4])"
  {
    operands[4] = copy_rtx (operands[1]);
  }
)

(define_insn "*cond_<optab><mode>_2_const_strict"
  [(set (match_operand:SVE_FULL_F 0 "register_operand")
	(unspec:SVE_FULL_F
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_F
	     [(match_dup 1)
	      (const_int SVE_STRICT_GP)
	      (match_operand:SVE_FULL_F 2 "register_operand")
	      (match_operand:SVE_FULL_F 3 "<sve_pred_fp_rhs2_immediate>")]
	     SVE_COND_FP_BINARY_I1)
	   (match_dup 2)]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2 ; attrs: movprfx ]
     [ w        , Upl , 0 ; *              ] <sve_fp_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, #%3
     [ ?w       , Upl , w ; yes            ] movprfx\t%0, %2\;<sve_fp_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, #%3
  }
)

;; Predicated floating-point operations, merging with the second input.
(define_insn_and_rewrite "*cond_<optab><mode>_3_relaxed"
  [(set (match_operand:SVE_FULL_F_BF 0 "register_operand")
	(unspec:SVE_FULL_F_BF
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_F_BF
	     [(match_operand 4)
	      (const_int SVE_RELAXED_GP)
	      (match_operand:SVE_FULL_F_BF 2 "register_operand")
	      (match_operand:SVE_FULL_F_BF 3 "register_operand")]
	     SVE_COND_FP_BINARY)
	   (match_dup 3)]
	  UNSPEC_SEL))]
  "TARGET_SVE && (<supports_bf16_rev> || !<is_bf16>)"
  {@ [ cons: =0 , 1   , 2 , 3 ; attrs: movprfx ]
     [ w        , Upl , w , 0 ; *              ] <b><sve_fp_op_rev>\t%0.<Vetype>, %1/m, %0.<Vetype>, %2.<Vetype>
     [ ?&w      , Upl , w , w ; yes            ] movprfx\t%0, %3\;<b><sve_fp_op_rev>\t%0.<Vetype>, %1/m, %0.<Vetype>, %2.<Vetype>
  }
  "&& !rtx_equal_p (operands[1], operands[4])"
  {
    operands[4] = copy_rtx (operands[1]);
  }
)

(define_insn "*cond_<optab><mode>_3_strict"
  [(set (match_operand:SVE_FULL_F_BF 0 "register_operand")
	(unspec:SVE_FULL_F_BF
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_F_BF
	     [(match_dup 1)
	      (const_int SVE_STRICT_GP)
	      (match_operand:SVE_FULL_F_BF 2 "register_operand")
	      (match_operand:SVE_FULL_F_BF 3 "register_operand")]
	     SVE_COND_FP_BINARY)
	   (match_dup 3)]
	  UNSPEC_SEL))]
  "TARGET_SVE && (<supports_bf16_rev> || !<is_bf16>)"
  {@ [ cons: =0 , 1   , 2 , 3 ; attrs: movprfx ]
     [ w        , Upl , w , 0 ; *              ] <b><sve_fp_op_rev>\t%0.<Vetype>, %1/m, %0.<Vetype>, %2.<Vetype>
     [ ?&w      , Upl , w , w ; yes            ] movprfx\t%0, %3\;<b><sve_fp_op_rev>\t%0.<Vetype>, %1/m, %0.<Vetype>, %2.<Vetype>
  }
)

;; Predicated floating-point operations, merging with an independent value.
(define_insn_and_rewrite "*cond_<optab><mode>_any_relaxed"
  [(set (match_operand:SVE_FULL_F_BF 0 "register_operand")
	(unspec:SVE_FULL_F_BF
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_F_BF
	     [(match_operand 5)
	      (const_int SVE_RELAXED_GP)
	      (match_operand:SVE_FULL_F_BF 2 "register_operand")
	      (match_operand:SVE_FULL_F_BF 3 "register_operand")]
	     SVE_COND_FP_BINARY)
	   (match_operand:SVE_FULL_F_BF 4 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE
   && (<supports_bf16> || !<is_bf16>)
   && !rtx_equal_p (operands[2], operands[4])
   && !((<supports_bf16_rev> || !<is_bf16>)
	&& rtx_equal_p (operands[3], operands[4]))"
  {@ [ cons: =0 , 1   , 2 , 3 , 4  ; attrs: is_rev ]
     [ &w       , Upl , 0 , w , Dz ; *    ] movprfx\t%0.<Vetype>, %1/z, %0.<Vetype>\;<b><sve_fp_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
     [ &w       , Upl , w , 0 , Dz ; true ] movprfx\t%0.<Vetype>, %1/z, %0.<Vetype>\;<b><sve_fp_op_rev>\t%0.<Vetype>, %1/m, %0.<Vetype>, %2.<Vetype>
     [ &w       , Upl , w , w , Dz ; *    ] movprfx\t%0.<Vetype>, %1/z, %2.<Vetype>\;<b><sve_fp_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
     [ &w       , Upl , w , w , 0  ; *    ] movprfx\t%0.<Vetype>, %1/m, %2.<Vetype>\;<b><sve_fp_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
     [ ?&w      , Upl , w , w , w  ; *    ] #
  }
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
  [(set_attr "movprfx" "yes")
   (set_attr "is_bf16" "<is_bf16>")
   (set_attr "supports_bf16_rev" "<supports_bf16_rev>")]
)

(define_insn_and_rewrite "*cond_<optab><mode>_any_strict"
  [(set (match_operand:SVE_FULL_F_BF 0 "register_operand")
	(unspec:SVE_FULL_F_BF
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_F_BF
	     [(match_dup 1)
	      (const_int SVE_STRICT_GP)
	      (match_operand:SVE_FULL_F_BF 2 "register_operand")
	      (match_operand:SVE_FULL_F_BF 3 "register_operand")]
	     SVE_COND_FP_BINARY)
	   (match_operand:SVE_FULL_F_BF 4 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE
   && (<supports_bf16> || !<is_bf16>)
   && !rtx_equal_p (operands[2], operands[4])
   && !((<supports_bf16_rev> || !<is_bf16>)
	&& rtx_equal_p (operands[3], operands[4]))"
  {@ [ cons: =0 , 1   , 2 , 3 , 4  ; attrs: is_rev ]
     [ &w       , Upl , 0 , w , Dz ; *    ] movprfx\t%0.<Vetype>, %1/z, %0.<Vetype>\;<b><sve_fp_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
     [ &w       , Upl , w , 0 , Dz ; true ] movprfx\t%0.<Vetype>, %1/z, %0.<Vetype>\;<b><sve_fp_op_rev>\t%0.<Vetype>, %1/m, %0.<Vetype>, %2.<Vetype>
     [ &w       , Upl , w , w , Dz ; *    ] movprfx\t%0.<Vetype>, %1/z, %2.<Vetype>\;<b><sve_fp_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
     [ &w       , Upl , w , w , 0  ; *    ] movprfx\t%0.<Vetype>, %1/m, %2.<Vetype>\;<b><sve_fp_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
     [ ?&w      , Upl , w , w , w  ; *    ] #
  }
  "&& reload_completed
   && register_operand (operands[4], <MODE>mode)
   && !rtx_equal_p (operands[0], operands[4])"
  {
    emit_insn (gen_vcond_mask_<mode><vpred> (operands[0], operands[2],
					     operands[4], operands[1]));
    operands[4] = operands[2] = operands[0];
  }
  [(set_attr "movprfx" "yes")
   (set_attr "is_bf16" "<is_bf16>")
   (set_attr "supports_bf16_rev" "<supports_bf16_rev>")]
)

;; Same for operations that take a 1-bit constant.
(define_insn_and_rewrite "*cond_<optab><mode>_any_const_relaxed"
  [(set (match_operand:SVE_FULL_F 0 "register_operand")
	(unspec:SVE_FULL_F
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_F
	     [(match_operand 5)
	      (const_int SVE_RELAXED_GP)
	      (match_operand:SVE_FULL_F 2 "register_operand")
	      (match_operand:SVE_FULL_F 3 "<sve_pred_fp_rhs2_immediate>")]
	     SVE_COND_FP_BINARY_I1)
	   (match_operand:SVE_FULL_F 4 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE && !rtx_equal_p (operands[2], operands[4])"
  {@ [ cons: =0 , 1   , 2 , 4   ]
     [ w        , Upl , w , Dz  ] movprfx\t%0.<Vetype>, %1/z, %2.<Vetype>\;<sve_fp_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, #%3
     [ w        , Upl , w , 0   ] movprfx\t%0.<Vetype>, %1/m, %2.<Vetype>\;<sve_fp_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, #%3
     [ ?w       , Upl , w , w   ] #
  }
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

(define_insn_and_rewrite "*cond_<optab><mode>_any_const_strict"
  [(set (match_operand:SVE_FULL_F 0 "register_operand")
	(unspec:SVE_FULL_F
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_F
	     [(match_dup 1)
	      (const_int SVE_STRICT_GP)
	      (match_operand:SVE_FULL_F 2 "register_operand")
	      (match_operand:SVE_FULL_F 3 "<sve_pred_fp_rhs2_immediate>")]
	     SVE_COND_FP_BINARY_I1)
	   (match_operand:SVE_FULL_F 4 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE && !rtx_equal_p (operands[2], operands[4])"
  {@ [ cons: =0 , 1   , 2 , 4   ]
     [ w        , Upl , w , Dz  ] movprfx\t%0.<Vetype>, %1/z, %2.<Vetype>\;<sve_fp_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, #%3
     [ w        , Upl , w , 0   ] movprfx\t%0.<Vetype>, %1/m, %2.<Vetype>\;<sve_fp_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, #%3
     [ ?w       , Upl , w , w   ] #
  }
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
;; ---- [FP] Addition
;; -------------------------------------------------------------------------
;; Includes:
;; - FADD
;; - FSUB
;; -------------------------------------------------------------------------

;; Predicated floating-point addition.
(define_insn "@aarch64_pred_<optab><mode>"
  [(set (match_operand:SVE_FULL_F 0 "register_operand")
	(unspec:SVE_FULL_F
	  [(match_operand:<VPRED> 1 "register_operand")
	   (match_operand:SI 4 "aarch64_sve_gp_strictness")
	   (match_operand:SVE_FULL_F 2 "register_operand")
	   (match_operand:SVE_FULL_F 3 "aarch64_sve_float_arith_with_sub_operand")]
	  SVE_COND_FP_ADD))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2  , 3   , 4   ; attrs: movprfx ]
     [ w        , Upl , %0 , vsA , i   ; *              ] fadd\t%0.<Vetype>, %1/m, %0.<Vetype>, #%3
     [ w        , Upl , 0  , vsN , i   ; *              ] fsub\t%0.<Vetype>, %1/m, %0.<Vetype>, #%N3
     [ w        , Upl , w  , w   , Z   ; *              ] #
     [ w        , Upl , 0  , w   , Ui1 ; *              ] fadd\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
     [ ?&w      , Upl , w  , vsA , i   ; yes            ] movprfx\t%0, %2\;fadd\t%0.<Vetype>, %1/m, %0.<Vetype>, #%3
     [ ?&w      , Upl , w  , vsN , i   ; yes            ] movprfx\t%0, %2\;fsub\t%0.<Vetype>, %1/m, %0.<Vetype>, #%N3
     [ ?&w      , Upl , w  , w   , Ui1 ; yes            ] movprfx\t%0, %2\;fadd\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
  }
)

;; Predicated floating-point addition of a constant, merging with the
;; first input.
(define_insn_and_rewrite "*cond_add<mode>_2_const_relaxed"
  [(set (match_operand:SVE_FULL_F 0 "register_operand")
	(unspec:SVE_FULL_F
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_F
	     [(match_operand 4)
	      (const_int SVE_RELAXED_GP)
	      (match_operand:SVE_FULL_F 2 "register_operand")
	      (match_operand:SVE_FULL_F 3 "aarch64_sve_float_arith_with_sub_immediate")]
	     UNSPEC_COND_FADD)
	   (match_dup 2)]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2 , 3   ; attrs: movprfx ]
     [ w        , Upl , 0 , vsA ; *              ] fadd\t%0.<Vetype>, %1/m, %0.<Vetype>, #%3
     [ w        , Upl , 0 , vsN ; *              ] fsub\t%0.<Vetype>, %1/m, %0.<Vetype>, #%N3
     [ ?w       , Upl , w , vsA ; yes            ] movprfx\t%0, %2\;fadd\t%0.<Vetype>, %1/m, %0.<Vetype>, #%3
     [ ?w       , Upl , w , vsN ; yes            ] movprfx\t%0, %2\;fsub\t%0.<Vetype>, %1/m, %0.<Vetype>, #%N3
  }
  "&& !rtx_equal_p (operands[1], operands[4])"
  {
    operands[4] = copy_rtx (operands[1]);
  }
)

(define_insn "*cond_add<mode>_2_const_strict"
  [(set (match_operand:SVE_FULL_F 0 "register_operand")
	(unspec:SVE_FULL_F
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_F
	     [(match_dup 1)
	      (const_int SVE_STRICT_GP)
	      (match_operand:SVE_FULL_F 2 "register_operand")
	      (match_operand:SVE_FULL_F 3 "aarch64_sve_float_arith_with_sub_immediate")]
	     UNSPEC_COND_FADD)
	   (match_dup 2)]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2 , 3   ; attrs: movprfx ]
     [ w        , Upl , 0 , vsA ; *              ] fadd\t%0.<Vetype>, %1/m, %0.<Vetype>, #%3
     [ w        , Upl , 0 , vsN ; *              ] fsub\t%0.<Vetype>, %1/m, %0.<Vetype>, #%N3
     [ ?w       , Upl , w , vsA ; yes            ] movprfx\t%0, %2\;fadd\t%0.<Vetype>, %1/m, %0.<Vetype>, #%3
     [ ?w       , Upl , w , vsN ; yes            ] movprfx\t%0, %2\;fsub\t%0.<Vetype>, %1/m, %0.<Vetype>, #%N3
  }
)

;; Predicated floating-point addition of a constant, merging with an
;; independent value.
(define_insn_and_rewrite "*cond_add<mode>_any_const_relaxed"
  [(set (match_operand:SVE_FULL_F 0 "register_operand")
	(unspec:SVE_FULL_F
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_F
	     [(match_operand 5)
	      (const_int SVE_RELAXED_GP)
	      (match_operand:SVE_FULL_F 2 "register_operand")
	      (match_operand:SVE_FULL_F 3 "aarch64_sve_float_arith_with_sub_immediate")]
	     UNSPEC_COND_FADD)
	   (match_operand:SVE_FULL_F 4 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE && !rtx_equal_p (operands[2], operands[4])"
  {@ [ cons: =0 , 1   , 2 , 3   , 4   ]
     [ w        , Upl , w , vsA , Dz  ] movprfx\t%0.<Vetype>, %1/z, %2.<Vetype>\;fadd\t%0.<Vetype>, %1/m, %0.<Vetype>, #%3
     [ w        , Upl , w , vsN , Dz  ] movprfx\t%0.<Vetype>, %1/z, %2.<Vetype>\;fsub\t%0.<Vetype>, %1/m, %0.<Vetype>, #%N3
     [ w        , Upl , w , vsA , 0   ] movprfx\t%0.<Vetype>, %1/m, %2.<Vetype>\;fadd\t%0.<Vetype>, %1/m, %0.<Vetype>, #%3
     [ w        , Upl , w , vsN , 0   ] movprfx\t%0.<Vetype>, %1/m, %2.<Vetype>\;fsub\t%0.<Vetype>, %1/m, %0.<Vetype>, #%N3
     [ ?w       , Upl , w , vsA , w   ] #
     [ ?w       , Upl , w , vsN , w   ] #
  }
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

(define_insn_and_rewrite "*cond_add<mode>_any_const_strict"
  [(set (match_operand:SVE_FULL_F 0 "register_operand")
	(unspec:SVE_FULL_F
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_F
	     [(match_dup 1)
	      (const_int SVE_STRICT_GP)
	      (match_operand:SVE_FULL_F 2 "register_operand")
	      (match_operand:SVE_FULL_F 3 "aarch64_sve_float_arith_with_sub_immediate")]
	     UNSPEC_COND_FADD)
	   (match_operand:SVE_FULL_F 4 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE && !rtx_equal_p (operands[2], operands[4])"
  {@ [ cons: =0 , 1   , 2 , 3   , 4   ]
     [ w        , Upl , w , vsA , Dz  ] movprfx\t%0.<Vetype>, %1/z, %2.<Vetype>\;fadd\t%0.<Vetype>, %1/m, %0.<Vetype>, #%3
     [ w        , Upl , w , vsN , Dz  ] movprfx\t%0.<Vetype>, %1/z, %2.<Vetype>\;fsub\t%0.<Vetype>, %1/m, %0.<Vetype>, #%N3
     [ w        , Upl , w , vsA , 0   ] movprfx\t%0.<Vetype>, %1/m, %2.<Vetype>\;fadd\t%0.<Vetype>, %1/m, %0.<Vetype>, #%3
     [ w        , Upl , w , vsN , 0   ] movprfx\t%0.<Vetype>, %1/m, %2.<Vetype>\;fsub\t%0.<Vetype>, %1/m, %0.<Vetype>, #%N3
     [ ?w       , Upl , w , vsA , w   ] #
     [ ?w       , Upl , w , vsN , w   ] #
  }
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

;; Register merging forms are handled through SVE_COND_FP_BINARY.

;; -------------------------------------------------------------------------
;; ---- [FP] Complex addition
;; -------------------------------------------------------------------------
;; Includes:
;; - FCADD
;; -------------------------------------------------------------------------

;; Predicated FCADD.
(define_insn "@aarch64_pred_<optab><mode>"
  [(set (match_operand:SVE_FULL_F 0 "register_operand")
	(unspec:SVE_FULL_F
	  [(match_operand:<VPRED> 1 "register_operand")
	   (match_operand:SI 4 "aarch64_sve_gp_strictness")
	   (match_operand:SVE_FULL_F 2 "register_operand")
	   (match_operand:SVE_FULL_F 3 "register_operand")]
	  SVE_COND_FCADD))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2 , 3 ; attrs: movprfx ]
     [ w        , Upl , 0 , w ; *              ] fcadd\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>, #<rot>
     [ ?&w      , Upl , w , w ; yes            ] movprfx\t%0, %2\;fcadd\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>, #<rot>
  }
)

;; Predicated FCADD with merging.
(define_expand "@cond_<optab><mode>"
  [(set (match_operand:SVE_FULL_F 0 "register_operand")
	(unspec:SVE_FULL_F
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_F
	     [(match_dup 1)
	      (const_int SVE_STRICT_GP)
	      (match_operand:SVE_FULL_F 2 "register_operand")
	      (match_operand:SVE_FULL_F 3 "register_operand")]
	     SVE_COND_FCADD)
	   (match_operand:SVE_FULL_F 4 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE"
)

;; Predicated FCADD using ptrue for unpredicated optab for auto-vectorizer
(define_expand "@cadd<rot><mode>3"
  [(set (match_operand:SVE_FULL_F 0 "register_operand")
	(unspec:SVE_FULL_F
	  [(match_dup 3)
	   (const_int SVE_RELAXED_GP)
	   (match_operand:SVE_FULL_F 1 "register_operand")
	   (match_operand:SVE_FULL_F 2 "register_operand")]
	  SVE_COND_FCADD))]
  "TARGET_SVE"
{
  operands[3] = aarch64_ptrue_reg (<VPRED>mode);
})

;; Predicated FCADD, merging with the first input.
(define_insn_and_rewrite "*cond_<optab><mode>_2_relaxed"
  [(set (match_operand:SVE_FULL_F 0 "register_operand")
	(unspec:SVE_FULL_F
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_F
	     [(match_operand 4)
	      (const_int SVE_RELAXED_GP)
	      (match_operand:SVE_FULL_F 2 "register_operand")
	      (match_operand:SVE_FULL_F 3 "register_operand")]
	     SVE_COND_FCADD)
	   (match_dup 2)]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2 , 3 ; attrs: movprfx ]
     [ w        , Upl , 0 , w ; *              ] fcadd\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>, #<rot>
     [ ?&w      , Upl , w , w ; yes            ] movprfx\t%0, %2\;fcadd\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>, #<rot>
  }
  "&& !rtx_equal_p (operands[1], operands[4])"
  {
    operands[4] = copy_rtx (operands[1]);
  }
)

(define_insn "*cond_<optab><mode>_2_strict"
  [(set (match_operand:SVE_FULL_F 0 "register_operand")
	(unspec:SVE_FULL_F
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_F
	     [(match_dup 1)
	      (const_int SVE_STRICT_GP)
	      (match_operand:SVE_FULL_F 2 "register_operand")
	      (match_operand:SVE_FULL_F 3 "register_operand")]
	     SVE_COND_FCADD)
	   (match_dup 2)]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2 , 3 ; attrs: movprfx ]
     [ w        , Upl , 0 , w ; *              ] fcadd\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>, #<rot>
     [ ?&w      , Upl , w , w ; yes            ] movprfx\t%0, %2\;fcadd\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>, #<rot>
  }
)

;; Predicated FCADD, merging with an independent value.
(define_insn_and_rewrite "*cond_<optab><mode>_any_relaxed"
  [(set (match_operand:SVE_FULL_F 0 "register_operand")
	(unspec:SVE_FULL_F
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_F
	     [(match_operand 5)
	      (const_int SVE_RELAXED_GP)
	      (match_operand:SVE_FULL_F 2 "register_operand")
	      (match_operand:SVE_FULL_F 3 "register_operand")]
	     SVE_COND_FCADD)
	   (match_operand:SVE_FULL_F 4 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE && !rtx_equal_p (operands[2], operands[4])"
  {@ [ cons: =0 , 1   , 2 , 3 , 4   ]
     [ &w       , Upl , w , w , Dz  ] movprfx\t%0.<Vetype>, %1/z, %2.<Vetype>\;fcadd\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>, #<rot>
     [ &w       , Upl , 0 , w , Dz  ] movprfx\t%0.<Vetype>, %1/z, %0.<Vetype>\;fcadd\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>, #<rot>
     [ &w       , Upl , w , w , 0   ] movprfx\t%0.<Vetype>, %1/m, %2.<Vetype>\;fcadd\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>, #<rot>
     [ ?&w      , Upl , w , w , w   ] #
  }
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

(define_insn_and_rewrite "*cond_<optab><mode>_any_strict"
  [(set (match_operand:SVE_FULL_F 0 "register_operand")
	(unspec:SVE_FULL_F
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_F
	     [(match_dup 1)
	      (const_int SVE_STRICT_GP)
	      (match_operand:SVE_FULL_F 2 "register_operand")
	      (match_operand:SVE_FULL_F 3 "register_operand")]
	     SVE_COND_FCADD)
	   (match_operand:SVE_FULL_F 4 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE && !rtx_equal_p (operands[2], operands[4])"
  {@ [ cons: =0 , 1   , 2 , 3 , 4   ]
     [ &w       , Upl , w , w , Dz  ] movprfx\t%0.<Vetype>, %1/z, %2.<Vetype>\;fcadd\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>, #<rot>
     [ &w       , Upl , 0 , w , Dz  ] movprfx\t%0.<Vetype>, %1/z, %0.<Vetype>\;fcadd\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>, #<rot>
     [ &w       , Upl , w , w , 0   ] movprfx\t%0.<Vetype>, %1/m, %2.<Vetype>\;fcadd\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>, #<rot>
     [ ?&w      , Upl , w , w , w   ] #
  }
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
;; ---- [FP] Subtraction
;; -------------------------------------------------------------------------
;; Includes:
;; - FSUB
;; - FSUBR
;; -------------------------------------------------------------------------

;; Predicated floating-point subtraction.
(define_insn "@aarch64_pred_<optab><mode>"
  [(set (match_operand:SVE_FULL_F 0 "register_operand")
	(unspec:SVE_FULL_F
	  [(match_operand:<VPRED> 1 "register_operand")
	   (match_operand:SI 4 "aarch64_sve_gp_strictness")
	   (match_operand:SVE_FULL_F 2 "aarch64_sve_float_arith_operand")
	   (match_operand:SVE_FULL_F 3 "register_operand")]
	  SVE_COND_FP_SUB))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2   , 3 , 4   ; attrs: movprfx ]
     [ w        , Upl , vsA , 0 , i   ; *              ] fsubr\t%0.<Vetype>, %1/m, %0.<Vetype>, #%2
     [ w        , Upl , w   , w , Z   ; *              ] #
     [ w        , Upl , 0   , w , Ui1 ; *              ] fsub\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
     [ w        , Upl , w   , 0 , Ui1 ; *              ] fsubr\t%0.<Vetype>, %1/m, %0.<Vetype>, %2.<Vetype>
     [ ?&w      , Upl , vsA , w , i   ; yes            ] movprfx\t%0, %3\;fsubr\t%0.<Vetype>, %1/m, %0.<Vetype>, #%2
     [ ?&w      , Upl , w   , w , Ui1 ; yes            ] movprfx\t%0, %2\;fsub\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
  }
)

;; Predicated floating-point subtraction from a constant, merging with the
;; second input.
(define_insn_and_rewrite "*cond_sub<mode>_3_const_relaxed"
  [(set (match_operand:SVE_FULL_F 0 "register_operand")
	(unspec:SVE_FULL_F
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_F
	     [(match_operand 4)
	      (const_int SVE_RELAXED_GP)
	      (match_operand:SVE_FULL_F 2 "aarch64_sve_float_arith_immediate")
	      (match_operand:SVE_FULL_F 3 "register_operand")]
	     UNSPEC_COND_FSUB)
	   (match_dup 3)]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 3 ; attrs: movprfx ]
     [ w        , Upl , 0 ; *              ] fsubr\t%0.<Vetype>, %1/m, %0.<Vetype>, #%2
     [ ?w       , Upl , w ; yes            ] movprfx\t%0, %3\;fsubr\t%0.<Vetype>, %1/m, %0.<Vetype>, #%2
  }
  "&& !rtx_equal_p (operands[1], operands[4])"
  {
    operands[4] = copy_rtx (operands[1]);
  }
)

(define_insn "*cond_sub<mode>_3_const_strict"
  [(set (match_operand:SVE_FULL_F 0 "register_operand")
	(unspec:SVE_FULL_F
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_F
	     [(match_dup 1)
	      (const_int SVE_STRICT_GP)
	      (match_operand:SVE_FULL_F 2 "aarch64_sve_float_arith_immediate")
	      (match_operand:SVE_FULL_F 3 "register_operand")]
	     UNSPEC_COND_FSUB)
	   (match_dup 3)]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 3 ; attrs: movprfx ]
     [ w        , Upl , 0 ; *              ] fsubr\t%0.<Vetype>, %1/m, %0.<Vetype>, #%2
     [ ?w       , Upl , w ; yes            ] movprfx\t%0, %3\;fsubr\t%0.<Vetype>, %1/m, %0.<Vetype>, #%2
  }
)

;; Predicated floating-point subtraction from a constant, merging with an
;; independent value.
(define_insn_and_rewrite "*cond_sub<mode>_const_relaxed"
  [(set (match_operand:SVE_FULL_F 0 "register_operand")
	(unspec:SVE_FULL_F
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_F
	     [(match_operand 5)
	      (const_int SVE_RELAXED_GP)
	      (match_operand:SVE_FULL_F 2 "aarch64_sve_float_arith_immediate")
	      (match_operand:SVE_FULL_F 3 "register_operand")]
	     UNSPEC_COND_FSUB)
	   (match_operand:SVE_FULL_F 4 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE && !rtx_equal_p (operands[3], operands[4])"
  {@ [ cons: =0 , 1   , 3 , 4   ]
     [ w        , Upl , w , Dz  ] movprfx\t%0.<Vetype>, %1/z, %3.<Vetype>\;fsubr\t%0.<Vetype>, %1/m, %0.<Vetype>, #%2
     [ w        , Upl , w , 0   ] movprfx\t%0.<Vetype>, %1/m, %3.<Vetype>\;fsubr\t%0.<Vetype>, %1/m, %0.<Vetype>, #%2
     [ ?w       , Upl , w , w   ] #
  }
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

(define_insn_and_rewrite "*cond_sub<mode>_const_strict"
  [(set (match_operand:SVE_FULL_F 0 "register_operand")
	(unspec:SVE_FULL_F
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_F
	     [(match_dup 1)
	      (const_int SVE_STRICT_GP)
	      (match_operand:SVE_FULL_F 2 "aarch64_sve_float_arith_immediate")
	      (match_operand:SVE_FULL_F 3 "register_operand")]
	     UNSPEC_COND_FSUB)
	   (match_operand:SVE_FULL_F 4 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE && !rtx_equal_p (operands[3], operands[4])"
  {@ [ cons: =0 , 1   , 3 , 4   ]
     [ w        , Upl , w , Dz  ] movprfx\t%0.<Vetype>, %1/z, %3.<Vetype>\;fsubr\t%0.<Vetype>, %1/m, %0.<Vetype>, #%2
     [ w        , Upl , w , 0   ] movprfx\t%0.<Vetype>, %1/m, %3.<Vetype>\;fsubr\t%0.<Vetype>, %1/m, %0.<Vetype>, #%2
     [ ?w       , Upl , w , w   ] #
  }
  "&& reload_completed
   && register_operand (operands[4], <MODE>mode)
   && !rtx_equal_p (operands[0], operands[4])"
  {
    emit_insn (gen_vcond_mask_<mode><vpred> (operands[0], operands[3],
                                             operands[4], operands[1]));
    operands[4] = operands[3] = operands[0];
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
(define_expand "@aarch64_pred_abd<mode>"
  [(set (match_operand:SVE_FULL_F 0 "register_operand")
	(unspec:SVE_FULL_F
	  [(match_operand:<VPRED> 1 "register_operand")
	   (match_operand:SI 4 "aarch64_sve_gp_strictness")
	   (unspec:SVE_FULL_F
	     [(match_dup 1)
	      (match_dup 4)
	      (match_operand:SVE_FULL_F 2 "register_operand")
	      (match_operand:SVE_FULL_F 3 "register_operand")]
	     UNSPEC_COND_FSUB)]
	  UNSPEC_COND_FABS))]
  "TARGET_SVE"
)

;; Predicated floating-point absolute difference.
(define_insn_and_rewrite "*aarch64_pred_abd<mode>_relaxed"
  [(set (match_operand:SVE_FULL_F 0 "register_operand")
	(unspec:SVE_FULL_F
	  [(match_operand:<VPRED> 1 "register_operand")
	   (match_operand:SI 4 "aarch64_sve_gp_strictness")
	   (unspec:SVE_FULL_F
	     [(match_operand 5)
	      (const_int SVE_RELAXED_GP)
	      (match_operand:SVE_FULL_F 2 "register_operand")
	      (match_operand:SVE_FULL_F 3 "register_operand")]
	     UNSPEC_COND_FSUB)]
	  UNSPEC_COND_FABS))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2  , 3 ; attrs: movprfx ]
     [ w        , Upl , %0 , w ; *              ] fabd\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
     [ ?&w      , Upl , w  , w ; yes            ] movprfx\t%0, %2\;fabd\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
  }
  "&& !rtx_equal_p (operands[1], operands[5])"
  {
    operands[5] = copy_rtx (operands[1]);
  }
)

(define_insn "*aarch64_pred_abd<mode>_strict"
  [(set (match_operand:SVE_FULL_F 0 "register_operand")
	(unspec:SVE_FULL_F
	  [(match_operand:<VPRED> 1 "register_operand")
	   (match_operand:SI 4 "aarch64_sve_gp_strictness")
	   (unspec:SVE_FULL_F
	     [(match_dup 1)
	      (const_int SVE_STRICT_GP)
	      (match_operand:SVE_FULL_F 2 "register_operand")
	      (match_operand:SVE_FULL_F 3 "register_operand")]
	     UNSPEC_COND_FSUB)]
	  UNSPEC_COND_FABS))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2  , 3 ; attrs: movprfx ]
     [ w        , Upl , %0 , w ; *              ] fabd\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
     [ ?&w      , Upl , w  , w ; yes            ] movprfx\t%0, %2\;fabd\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
  }
)

(define_expand "@aarch64_cond_abd<mode>"
  [(set (match_operand:SVE_FULL_F 0 "register_operand")
	(unspec:SVE_FULL_F
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_F
	     [(match_dup 1)
	      (const_int SVE_STRICT_GP)
	      (unspec:SVE_FULL_F
		[(match_dup 1)
		 (const_int SVE_STRICT_GP)
		 (match_operand:SVE_FULL_F 2 "register_operand")
		 (match_operand:SVE_FULL_F 3 "register_operand")]
		UNSPEC_COND_FSUB)]
	     UNSPEC_COND_FABS)
	   (match_operand:SVE_FULL_F 4 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE"
{
  if (rtx_equal_p (operands[3], operands[4]))
    std::swap (operands[2], operands[3]);
})

;; Predicated floating-point absolute difference, merging with the first
;; input.
(define_insn_and_rewrite "*aarch64_cond_abd<mode>_2_relaxed"
  [(set (match_operand:SVE_FULL_F 0 "register_operand")
	(unspec:SVE_FULL_F
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_F
	     [(match_operand 4)
	      (const_int SVE_RELAXED_GP)
	      (unspec:SVE_FULL_F
		[(match_operand 5)
		 (const_int SVE_RELAXED_GP)
		 (match_operand:SVE_FULL_F 2 "register_operand")
		 (match_operand:SVE_FULL_F 3 "register_operand")]
		UNSPEC_COND_FSUB)]
	     UNSPEC_COND_FABS)
	   (match_dup 2)]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2 , 3 ; attrs: movprfx ]
     [ w        , Upl , 0 , w ; *              ] fabd\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
     [ ?&w      , Upl , w , w ; yes            ] movprfx\t%0, %2\;fabd\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
  }
  "&& (!rtx_equal_p (operands[1], operands[4])
       || !rtx_equal_p (operands[1], operands[5]))"
  {
    operands[4] = copy_rtx (operands[1]);
    operands[5] = copy_rtx (operands[1]);
  }
)

(define_insn "*aarch64_cond_abd<mode>_2_strict"
  [(set (match_operand:SVE_FULL_F 0 "register_operand")
	(unspec:SVE_FULL_F
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_F
	     [(match_dup 1)
	      (match_operand:SI 4 "aarch64_sve_gp_strictness")
	      (unspec:SVE_FULL_F
		[(match_dup 1)
		 (match_operand:SI 5 "aarch64_sve_gp_strictness")
		 (match_operand:SVE_FULL_F 2 "register_operand")
		 (match_operand:SVE_FULL_F 3 "register_operand")]
		UNSPEC_COND_FSUB)]
	     UNSPEC_COND_FABS)
	   (match_dup 2)]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2 , 3 ; attrs: movprfx ]
     [ w        , Upl , 0 , w ; *              ] fabd\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
     [ ?&w      , Upl , w , w ; yes            ] movprfx\t%0, %2\;fabd\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
  }
)

;; Predicated floating-point absolute difference, merging with the second
;; input.
(define_insn_and_rewrite "*aarch64_cond_abd<mode>_3_relaxed"
  [(set (match_operand:SVE_FULL_F 0 "register_operand")
	(unspec:SVE_FULL_F
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_F
	     [(match_operand 4)
	      (const_int SVE_RELAXED_GP)
	      (unspec:SVE_FULL_F
		[(match_operand 5)
		 (const_int SVE_RELAXED_GP)
		 (match_operand:SVE_FULL_F 2 "register_operand")
		 (match_operand:SVE_FULL_F 3 "register_operand")]
		UNSPEC_COND_FSUB)]
	     UNSPEC_COND_FABS)
	   (match_dup 3)]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2 , 3 ; attrs: movprfx ]
     [ w        , Upl , w , 0 ; *              ] fabd\t%0.<Vetype>, %1/m, %0.<Vetype>, %2.<Vetype>
     [ ?&w      , Upl , w , w ; yes            ] movprfx\t%0, %3\;fabd\t%0.<Vetype>, %1/m, %0.<Vetype>, %2.<Vetype>
  }
  "&& (!rtx_equal_p (operands[1], operands[4])
       || !rtx_equal_p (operands[1], operands[5]))"
  {
    operands[4] = copy_rtx (operands[1]);
    operands[5] = copy_rtx (operands[1]);
  }
)

(define_insn "*aarch64_cond_abd<mode>_3_strict"
  [(set (match_operand:SVE_FULL_F 0 "register_operand")
	(unspec:SVE_FULL_F
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_F
	     [(match_dup 1)
	      (match_operand:SI 4 "aarch64_sve_gp_strictness")
	      (unspec:SVE_FULL_F
		[(match_dup 1)
		 (match_operand:SI 5 "aarch64_sve_gp_strictness")
		 (match_operand:SVE_FULL_F 2 "register_operand")
		 (match_operand:SVE_FULL_F 3 "register_operand")]
		UNSPEC_COND_FSUB)]
	     UNSPEC_COND_FABS)
	   (match_dup 3)]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2 , 3 ; attrs: movprfx ]
     [ w        , Upl , w , 0 ; *              ] fabd\t%0.<Vetype>, %1/m, %0.<Vetype>, %2.<Vetype>
     [ ?&w      , Upl , w , w ; yes            ] movprfx\t%0, %3\;fabd\t%0.<Vetype>, %1/m, %0.<Vetype>, %2.<Vetype>
  }
)

;; Predicated floating-point absolute difference, merging with an
;; independent value.
(define_insn_and_rewrite "*aarch64_cond_abd<mode>_any_relaxed"
  [(set (match_operand:SVE_FULL_F 0 "register_operand")
	(unspec:SVE_FULL_F
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_F
	     [(match_operand 5)
	      (const_int SVE_RELAXED_GP)
	      (unspec:SVE_FULL_F
		[(match_operand 6)
		 (const_int SVE_RELAXED_GP)
		 (match_operand:SVE_FULL_F 2 "register_operand")
		 (match_operand:SVE_FULL_F 3 "register_operand")]
		UNSPEC_COND_FSUB)]
	     UNSPEC_COND_FABS)
	   (match_operand:SVE_FULL_F 4 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE
   && !rtx_equal_p (operands[2], operands[4])
   && !rtx_equal_p (operands[3], operands[4])"
  {@ [ cons: =0 , 1   , 2 , 3 , 4   ]
     [ &w       , Upl , 0 , w , Dz  ] movprfx\t%0.<Vetype>, %1/z, %0.<Vetype>\;fabd\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
     [ &w       , Upl , w , 0 , Dz  ] movprfx\t%0.<Vetype>, %1/z, %0.<Vetype>\;fabd\t%0.<Vetype>, %1/m, %0.<Vetype>, %2.<Vetype>
     [ &w       , Upl , w , w , Dz  ] movprfx\t%0.<Vetype>, %1/z, %2.<Vetype>\;fabd\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
     [ &w       , Upl , w , w , 0   ] movprfx\t%0.<Vetype>, %1/m, %2.<Vetype>\;fabd\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
     [ ?&w      , Upl , w , w , w   ] #
  }
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
	     || !rtx_equal_p (operands[1], operands[6]))
      {
	operands[5] = copy_rtx (operands[1]);
	operands[6] = copy_rtx (operands[1]);
      }
    else
      FAIL;
  }
  [(set_attr "movprfx" "yes")]
)

(define_insn_and_rewrite "*aarch64_cond_abd<mode>_any_strict"
  [(set (match_operand:SVE_FULL_F 0 "register_operand")
	(unspec:SVE_FULL_F
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_F
	     [(match_dup 1)
	      (match_operand:SI 5 "aarch64_sve_gp_strictness")
	      (unspec:SVE_FULL_F
		[(match_dup 1)
		 (match_operand:SI 6 "aarch64_sve_gp_strictness")
		 (match_operand:SVE_FULL_F 2 "register_operand")
		 (match_operand:SVE_FULL_F 3 "register_operand")]
		UNSPEC_COND_FSUB)]
	     UNSPEC_COND_FABS)
	   (match_operand:SVE_FULL_F 4 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE
   && !rtx_equal_p (operands[2], operands[4])
   && !rtx_equal_p (operands[3], operands[4])"
  {@ [ cons: =0 , 1   , 2 , 3 , 4   ]
     [ &w       , Upl , 0 , w , Dz  ] movprfx\t%0.<Vetype>, %1/z, %0.<Vetype>\;fabd\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
     [ &w       , Upl , w , 0 , Dz  ] movprfx\t%0.<Vetype>, %1/z, %0.<Vetype>\;fabd\t%0.<Vetype>, %1/m, %0.<Vetype>, %2.<Vetype>
     [ &w       , Upl , w , w , Dz  ] movprfx\t%0.<Vetype>, %1/z, %2.<Vetype>\;fabd\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
     [ &w       , Upl , w , w , 0   ] movprfx\t%0.<Vetype>, %1/m, %2.<Vetype>\;fabd\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
     [ ?&w      , Upl , w , w , w   ] #
  }
  "&& reload_completed
   && register_operand (operands[4], <MODE>mode)
   && !rtx_equal_p (operands[0], operands[4])"
  {
    emit_insn (gen_vcond_mask_<mode><vpred> (operands[0], operands[3],
					     operands[4], operands[1]));
    operands[4] = operands[3] = operands[0];
  }
  [(set_attr "movprfx" "yes")]
)

;; -------------------------------------------------------------------------
;; ---- [FP] Multiplication
;; -------------------------------------------------------------------------
;; Includes:
;; - BFMUL (SVE_B16B16)
;; - FMUL
;; -------------------------------------------------------------------------

;; Predicated floating-point multiplication.
(define_insn "@aarch64_pred_<optab><mode>"
  [(set (match_operand:SVE_FULL_F 0 "register_operand")
	(unspec:SVE_FULL_F
	  [(match_operand:<VPRED> 1 "register_operand")
	   (match_operand:SI 4 "aarch64_sve_gp_strictness")
	   (match_operand:SVE_FULL_F 2 "register_operand")
	   (match_operand:SVE_FULL_F 3 "aarch64_sve_float_mul_operand")]
	  SVE_COND_FP_MUL))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2  , 3   , 4   ; attrs: movprfx ]
     [ w        , Upl , %0 , vsM , i   ; *              ] fmul\t%0.<Vetype>, %1/m, %0.<Vetype>, #%3
     [ w        , Upl , w  , w   , Z   ; *              ] #
     [ w        , Upl , 0  , w   , Ui1 ; *              ] fmul\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
     [ ?&w      , Upl , w  , vsM , i   ; yes            ] movprfx\t%0, %2\;fmul\t%0.<Vetype>, %1/m, %0.<Vetype>, #%3
     [ ?&w      , Upl , w  , w   , Ui1 ; yes            ] movprfx\t%0, %2\;fmul\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
  }
)

;; Merging forms are handled through SVE_COND_FP_BINARY and
;; SVE_COND_FP_BINARY_I1.

;; Unpredicated multiplication by selected lanes.
(define_insn "@aarch64_mul_lane_<mode>"
  [(set (match_operand:SVE_FULL_F_BF 0 "register_operand" "=w")
	(mult:SVE_FULL_F_BF
	  (unspec:SVE_FULL_F_BF
	    [(match_operand:SVE_FULL_F_BF 2 "register_operand" "<sve_lane_con>")
	     (match_operand:SI 3 "const_int_operand")]
	    UNSPEC_SVE_LANE_SELECT)
	  (match_operand:SVE_FULL_F_BF 1 "register_operand" "w")))]
  "TARGET_SVE"
  "<b>fmul\t%0.<Vetype>, %1.<Vetype>, %2.<Vetype>[%3]"
)

;; -------------------------------------------------------------------------
;; ---- [FP] Division
;; -------------------------------------------------------------------------
;; The patterns in this section are synthetic.
;; -------------------------------------------------------------------------

(define_expand "div<mode>3"
  [(set (match_operand:SVE_FULL_F 0 "register_operand")
	(unspec:SVE_FULL_F
	  [(match_dup 3)
	   (const_int SVE_RELAXED_GP)
	   (match_operand:SVE_FULL_F 1 "nonmemory_operand")
	   (match_operand:SVE_FULL_F 2 "register_operand")]
	  UNSPEC_COND_FDIV))]
  "TARGET_SVE"
  {
    if (aarch64_emit_approx_div (operands[0], operands[1], operands[2]))
      DONE;

    operands[1] = force_reg (<MODE>mode, operands[1]);
    operands[3] = aarch64_ptrue_reg (<VPRED>mode);
  }
)

(define_expand "@aarch64_frecpe<mode>"
  [(set (match_operand:SVE_FULL_F 0 "register_operand")
	(unspec:SVE_FULL_F
	  [(match_operand:SVE_FULL_F 1 "register_operand")]
	  UNSPEC_FRECPE))]
  "TARGET_SVE"
)

(define_expand "@aarch64_frecps<mode>"
  [(set (match_operand:SVE_FULL_F 0 "register_operand")
	(unspec:SVE_FULL_F
	  [(match_operand:SVE_FULL_F 1 "register_operand")
	   (match_operand:SVE_FULL_F 2 "register_operand")]
	  UNSPEC_FRECPS))]
  "TARGET_SVE"
)

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
	(unspec:SVE_F
	  [(match_operand:SVE_F 1 "register_operand" "w")
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
  [(match_operand:SVE_FULL_F 0 "register_operand")
   (match_operand:SVE_FULL_F 1 "register_operand")
   (match_operand:SVE_FULL_F 2 "nonmemory_operand")]
  "TARGET_SVE"
  {
    rtx sign = gen_reg_rtx (<V_INT_EQUIV>mode);
    rtx mant = gen_reg_rtx (<V_INT_EQUIV>mode);
    rtx int_res = gen_reg_rtx (<V_INT_EQUIV>mode);
    int bits = GET_MODE_UNIT_BITSIZE (<MODE>mode) - 1;

    rtx arg1 = lowpart_subreg (<V_INT_EQUIV>mode, operands[1], <MODE>mode);
    rtx arg2 = lowpart_subreg (<V_INT_EQUIV>mode, operands[2], <MODE>mode);

    rtx v_sign_bitmask
      = aarch64_simd_gen_const_vector_dup (<V_INT_EQUIV>mode,
					   HOST_WIDE_INT_M1U << bits);

    /* copysign (x, -1) should instead be expanded as orr with the sign
       bit.  */
    if (!REG_P (operands[2]))
      {
	rtx op2_elt = unwrap_const_vec_duplicate (operands[2]);
	if (GET_CODE (op2_elt) == CONST_DOUBLE
	    && real_isneg (CONST_DOUBLE_REAL_VALUE (op2_elt)))
	  {
	    emit_insn (gen_ior<v_int_equiv>3 (int_res, arg1, v_sign_bitmask));
	    emit_move_insn (operands[0], gen_lowpart (<MODE>mode, int_res));
	    DONE;
	  }
      }

    operands[2] = force_reg (<MODE>mode, operands[2]);
    emit_insn (gen_and<v_int_equiv>3 (sign, arg2, v_sign_bitmask));
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

(define_expand "cond_copysign<mode>"
  [(match_operand:SVE_FULL_F 0 "register_operand")
   (match_operand:<VPRED> 1 "register_operand")
   (match_operand:SVE_FULL_F 2 "register_operand")
   (match_operand:SVE_FULL_F 3 "nonmemory_operand")
   (match_operand:SVE_FULL_F 4 "aarch64_simd_reg_or_zero")]
  "TARGET_SVE"
  {
    rtx sign = gen_reg_rtx (<V_INT_EQUIV>mode);
    rtx mant = gen_reg_rtx (<V_INT_EQUIV>mode);
    rtx int_res = gen_reg_rtx (<V_INT_EQUIV>mode);
    int bits = GET_MODE_UNIT_BITSIZE (<MODE>mode) - 1;

    rtx arg2 = lowpart_subreg (<V_INT_EQUIV>mode, operands[2], <MODE>mode);
    rtx arg3 = lowpart_subreg (<V_INT_EQUIV>mode, operands[3], <MODE>mode);
    rtx arg4 = lowpart_subreg (<V_INT_EQUIV>mode, operands[4], <MODE>mode);

    rtx v_sign_bitmask
      = aarch64_simd_gen_const_vector_dup (<V_INT_EQUIV>mode,
					   HOST_WIDE_INT_M1U << bits);

    /* copysign (x, -1) should instead be expanded as orr with the sign
       bit.  */
    if (!REG_P (operands[3]))
      {
	rtx op2_elt = unwrap_const_vec_duplicate (operands[3]);
	if (GET_CODE (op2_elt) == CONST_DOUBLE
	    && real_isneg (CONST_DOUBLE_REAL_VALUE (op2_elt)))
	  {
	    arg3 = force_reg (<V_INT_EQUIV>mode, v_sign_bitmask);
	    emit_insn (gen_cond_ior<v_int_equiv> (int_res, operands[1], arg2,
						  arg3, arg4));
	    emit_move_insn (operands[0], gen_lowpart (<MODE>mode, int_res));
	    DONE;
	  }
      }

    operands[2] = force_reg (<MODE>mode, operands[3]);
    emit_insn (gen_and<v_int_equiv>3 (sign, arg3, v_sign_bitmask));
    emit_insn (gen_and<v_int_equiv>3
	       (mant, arg2,
		aarch64_simd_gen_const_vector_dup (<V_INT_EQUIV>mode,
						   ~(HOST_WIDE_INT_M1U
						     << bits))));
    emit_insn (gen_cond_ior<v_int_equiv> (int_res, operands[1], sign, mant,
					  arg4));
    emit_move_insn (operands[0], gen_lowpart (<MODE>mode, int_res));
    DONE;
  }
)

(define_expand "xorsign<mode>3"
  [(match_operand:SVE_FULL_F 0 "register_operand")
   (match_operand:SVE_FULL_F 1 "register_operand")
   (match_operand:SVE_FULL_F 2 "register_operand")]
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
;; - FMAX
;; - FMAXNM
;; - FMIN
;; - FMINNM
;; -------------------------------------------------------------------------

;; Predicated floating-point maximum/minimum.
(define_insn "@aarch64_pred_<optab><mode>"
  [(set (match_operand:SVE_FULL_F 0 "register_operand")
	(unspec:SVE_FULL_F
	  [(match_operand:<VPRED> 1 "register_operand")
	   (match_operand:SI 4 "aarch64_sve_gp_strictness")
	   (match_operand:SVE_FULL_F 2 "register_operand")
	   (match_operand:SVE_FULL_F 3 "aarch64_sve_float_maxmin_operand")]
	  SVE_COND_FP_MAXMIN))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2  , 3   ; attrs: movprfx ]
     [ w        , Upl , %0 , vsB ; *              ] <sve_fp_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, #%3
     [ w        , Upl , 0  , w   ; *              ] <sve_fp_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
     [ ?&w      , Upl , w  , vsB ; yes            ] movprfx\t%0, %2\;<sve_fp_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, #%3
     [ ?&w      , Upl , w  , w   ; yes            ] movprfx\t%0, %2\;<sve_fp_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
  }
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
  [(set (match_operand:PRED_ALL 0 "register_operand")
	(and:PRED_ALL (match_operand:PRED_ALL 1 "register_operand")
		      (match_operand:PRED_ALL 2 "register_operand")))]
  "TARGET_SVE"
  {@ [ cons: =0, 1   , 2   ; attrs: pred_clobber ]
     [ &Upa    , Upa , Upa ; yes                 ] and\t%0.b, %1/z, %2.b, %2.b
     [ ?Upa    , 0Upa, 0Upa; yes                 ] ^
     [ Upa     , Upa , Upa ; no                  ] ^
  }
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
  [(set (match_operand:PRED_ALL 0 "register_operand")
	(and:PRED_ALL
	  (LOGICAL:PRED_ALL
	    (match_operand:PRED_ALL 2 "register_operand")
	    (match_operand:PRED_ALL 3 "register_operand"))
	  (match_operand:PRED_ALL 1 "register_operand")))]
  "TARGET_SVE"
  {@ [ cons: =0, 1   , 2   , 3   ; attrs: pred_clobber ]
     [ &Upa    , Upa , Upa , Upa ; yes                 ] <logical>\t%0.b, %1/z, %2.b, %3.b
     [ ?Upa    , 0Upa, 0Upa, 0Upa; yes                 ] ^
     [ Upa     , Upa , Upa , Upa ; no                  ] ^
  }
)

;; Perform a logical operation on operands 2 and 3, using operand 1 as
;; the GP.  Store the result in operand 0 and set the flags in the same
;; way as for PTEST.
(define_insn "*<optab><mode>3_cc"
  [(set (reg:CC_NZC CC_REGNUM)
	(unspec:CC_NZC
	  [(match_operand:VNx16BI 1 "register_operand")
	   (match_operand 4)
	   (match_operand:SI 5 "aarch64_sve_ptrue_flag")
	   (and:PRED_ALL
	     (LOGICAL:PRED_ALL
	       (match_operand:PRED_ALL 2 "register_operand")
	       (match_operand:PRED_ALL 3 "register_operand"))
	     (match_dup 4))]
	  UNSPEC_PTEST))
   (set (match_operand:PRED_ALL 0 "register_operand")
	(and:PRED_ALL (LOGICAL:PRED_ALL (match_dup 2) (match_dup 3))
		      (match_dup 4)))]
  "TARGET_SVE"
  {@ [ cons: =0, 1   , 2   , 3   ; attrs: pred_clobber ]
     [ &Upa    , Upa , Upa , Upa ; yes                 ] <logical>s\t%0.b, %1/z, %2.b, %3.b
     [ ?Upa    , 0Upa, 0Upa, 0Upa; yes                 ] ^
     [ Upa     , Upa , Upa , Upa ; no                  ] ^
  }
)

;; Same with just the flags result.
(define_insn "*<optab><mode>3_ptest"
  [(set (reg:CC_NZC CC_REGNUM)
	(unspec:CC_NZC
	  [(match_operand:VNx16BI 1 "register_operand")
	   (match_operand 4)
	   (match_operand:SI 5 "aarch64_sve_ptrue_flag")
	   (and:PRED_ALL
	     (LOGICAL:PRED_ALL
	       (match_operand:PRED_ALL 2 "register_operand")
	       (match_operand:PRED_ALL 3 "register_operand"))
	     (match_dup 4))]
	  UNSPEC_PTEST))
   (clobber (match_scratch:VNx16BI 0))]
  "TARGET_SVE"
  {@ [ cons: =0, 1   , 2   , 3   ; attrs: pred_clobber ]
     [ &Upa    , Upa , Upa , Upa ; yes                 ] <logical>s\t%0.b, %1/z, %2.b, %3.b
     [ ?Upa    , 0Upa, 0Upa, 0Upa; yes                 ] ^
     [ Upa     , Upa , Upa , Upa ; no                  ] ^
  }
)

;; -------------------------------------------------------------------------
;; ---- [PRED] Binary logical operations (inverted second input)
;; -------------------------------------------------------------------------
;; Includes:
;; - BIC
;; - ORN
;; -------------------------------------------------------------------------

;; Predicated predicate BIC and ORN.
(define_insn "aarch64_pred_<nlogical><mode>_z"
  [(set (match_operand:PRED_ALL 0 "register_operand")
	(and:PRED_ALL
	  (NLOGICAL:PRED_ALL
	    (not:PRED_ALL (match_operand:PRED_ALL 3 "register_operand"))
	    (match_operand:PRED_ALL 2 "register_operand"))
	  (match_operand:PRED_ALL 1 "register_operand")))]
  "TARGET_SVE"
  {@ [ cons: =0, 1   , 2   , 3   ; attrs: pred_clobber ]
     [ &Upa    , Upa , Upa , Upa ; yes                 ] <nlogical>\t%0.b, %1/z, %2.b, %3.b
     [ ?Upa    , 0Upa, 0Upa, 0Upa; yes                 ] ^
     [ Upa     , Upa , Upa , Upa ; no                  ] ^
  }
)

;; Same, but set the flags as a side-effect.
(define_insn "*<nlogical><mode>3_cc"
  [(set (reg:CC_NZC CC_REGNUM)
	(unspec:CC_NZC
	  [(match_operand:VNx16BI 1 "register_operand")
	   (match_operand 4)
	   (match_operand:SI 5 "aarch64_sve_ptrue_flag")
	   (and:PRED_ALL
	     (NLOGICAL:PRED_ALL
	       (not:PRED_ALL
		 (match_operand:PRED_ALL 3 "register_operand"))
	       (match_operand:PRED_ALL 2 "register_operand"))
	     (match_dup 4))]
	  UNSPEC_PTEST))
   (set (match_operand:PRED_ALL 0 "register_operand")
	(and:PRED_ALL (NLOGICAL:PRED_ALL
			(not:PRED_ALL (match_dup 3))
			(match_dup 2))
		      (match_dup 4)))]
  "TARGET_SVE"
  {@ [ cons: =0, 1   , 2   , 3   ; attrs: pred_clobber ]
     [ &Upa    , Upa , Upa , Upa ; yes                 ] <nlogical>s\t%0.b, %1/z, %2.b, %3.b
     [ ?Upa    , 0Upa, 0Upa, 0Upa; yes                 ] ^
     [ Upa     , Upa , Upa , Upa ; no                  ] ^
  }
)

;; Same with just the flags result.
(define_insn "*<nlogical><mode>3_ptest"
  [(set (reg:CC_NZC CC_REGNUM)
	(unspec:CC_NZC
	  [(match_operand:VNx16BI 1 "register_operand")
	   (match_operand 4)
	   (match_operand:SI 5 "aarch64_sve_ptrue_flag")
	   (and:PRED_ALL
	     (NLOGICAL:PRED_ALL
	       (not:PRED_ALL
		 (match_operand:PRED_ALL 3 "register_operand"))
	       (match_operand:PRED_ALL 2 "register_operand"))
	     (match_dup 4))]
	  UNSPEC_PTEST))
   (clobber (match_scratch:VNx16BI 0))]
  "TARGET_SVE"
  {@ [ cons:  =0, 1   , 2   , 3   ; attrs: pred_clobber ]
     [ &Upa     , Upa , Upa , Upa ; yes                 ] <nlogical>s\t%0.b, %1/z, %2.b, %3.b
     [ ?Upa     , 0Upa, 0Upa, 0Upa; yes                 ] ^
     [ Upa      , Upa , Upa , Upa ; no                  ] ^
  }
)

;; -------------------------------------------------------------------------
;; ---- [PRED] Binary logical operations (inverted result)
;; -------------------------------------------------------------------------
;; Includes:
;; - NAND
;; - NOR
;; -------------------------------------------------------------------------

;; Predicated predicate NAND and NOR.
(define_insn "aarch64_pred_<logical_nn><mode>_z"
  [(set (match_operand:PRED_ALL 0 "register_operand")
	(and:PRED_ALL
	  (NLOGICAL:PRED_ALL
	    (not:PRED_ALL (match_operand:PRED_ALL 2 "register_operand"))
	    (not:PRED_ALL (match_operand:PRED_ALL 3 "register_operand")))
	  (match_operand:PRED_ALL 1 "register_operand")))]
  "TARGET_SVE"
  {@ [ cons: =0,  1  , 2   , 3   ; attrs: pred_clobber ]
     [ &Upa    , Upa , Upa , Upa ; yes                 ] <logical_nn>\t%0.b, %1/z, %2.b, %3.b
     [ ?Upa    , 0Upa, 0Upa, 0Upa; yes                 ] ^
     [ Upa     , Upa , Upa , Upa ; no                  ] ^
  }
)

;; Same, but set the flags as a side-effect.
(define_insn "*<logical_nn><mode>3_cc"
  [(set (reg:CC_NZC CC_REGNUM)
	(unspec:CC_NZC
	  [(match_operand:VNx16BI 1 "register_operand")
	   (match_operand 4)
	   (match_operand:SI 5 "aarch64_sve_ptrue_flag")
	   (and:PRED_ALL
	     (NLOGICAL:PRED_ALL
	       (not:PRED_ALL
		 (match_operand:PRED_ALL 2 "register_operand"))
	       (not:PRED_ALL
		 (match_operand:PRED_ALL 3 "register_operand")))
	     (match_dup 4))]
	  UNSPEC_PTEST))
   (set (match_operand:PRED_ALL 0 "register_operand")
	(and:PRED_ALL (NLOGICAL:PRED_ALL
			(not:PRED_ALL (match_dup 2))
			(not:PRED_ALL (match_dup 3)))
		      (match_dup 4)))]
  "TARGET_SVE"
  {@ [ cons: =0, 1   , 2   , 3   ; attrs: pred_clobber ]
     [ &Upa    , Upa , Upa , Upa ; yes                 ] <logical_nn>s\t%0.b, %1/z, %2.b, %3.b
     [ ?Upa    , 0Upa, 0Upa, 0Upa; yes                 ] ^
     [ Upa     , Upa , Upa , Upa ; no                  ] ^
  }
)

;; Same with just the flags result.
(define_insn "*<logical_nn><mode>3_ptest"
  [(set (reg:CC_NZC CC_REGNUM)
	(unspec:CC_NZC
	  [(match_operand:VNx16BI 1 "register_operand")
	   (match_operand 4)
	   (match_operand:SI 5 "aarch64_sve_ptrue_flag")
	   (and:PRED_ALL
	     (NLOGICAL:PRED_ALL
	       (not:PRED_ALL
		 (match_operand:PRED_ALL 2 "register_operand"))
	       (not:PRED_ALL
		 (match_operand:PRED_ALL 3 "register_operand")))
	     (match_dup 4))]
	  UNSPEC_PTEST))
   (clobber (match_scratch:VNx16BI 0))]
  "TARGET_SVE"
  {@ [ cons: =0, 1   , 2   , 3   ; attrs: pred_clobber ]
     [ &Upa    , Upa , Upa , Upa ; yes                 ] <logical_nn>s\t%0.b, %1/z, %2.b, %3.b
     [ ?Upa    , 0Upa, 0Upa, 0Upa; yes                 ] ^
     [ Upa     , Upa , Upa , Upa ; no                  ] ^
  }
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
	     (mult:SVE_I
	       (match_operand:SVE_I 1 "register_operand")
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
(define_insn "@aarch64_pred_fma<mode>"
  [(set (match_operand:SVE_I 0 "register_operand")
	(plus:SVE_I
	  (unspec:SVE_I
	    [(match_operand:<VPRED> 1 "register_operand")
	     (mult:SVE_I
	       (match_operand:SVE_I 2 "register_operand")
	       (match_operand:SVE_I 3 "register_operand"))]
	    UNSPEC_PRED_X)
	  (match_operand:SVE_I 4 "register_operand")))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2  , 3 , 4 ; attrs: movprfx ]
     [ w        , Upl , %0 , w , w ; *              ] mad\t%0.<Vetype>, %1/m, %3.<Vetype>, %4.<Vetype>
     [ w        , Upl , w  , w , 0 ; *              ] mla\t%0.<Vetype>, %1/m, %2.<Vetype>, %3.<Vetype>
     [ ?&w      , Upl , w  , w , w ; yes            ] movprfx\t%0, %4\;mla\t%0.<Vetype>, %1/m, %2.<Vetype>, %3.<Vetype>
  }
)

;; Predicated integer addition of product with merging.
(define_expand "cond_fma<mode>"
  [(set (match_operand:SVE_I 0 "register_operand")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand")
	   (plus:SVE_I
	     (mult:SVE_I
	       (match_operand:SVE_I 2 "register_operand")
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
  [(set (match_operand:SVE_I 0 "register_operand")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand")
	   (plus:SVE_I
	     (mult:SVE_I
	       (match_operand:SVE_I 2 "register_operand")
	       (match_operand:SVE_I 3 "register_operand"))
	     (match_operand:SVE_I 4 "register_operand"))
	   (match_dup 2)]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2 , 3 , 4 ; attrs: movprfx ]
     [ w        , Upl , 0 , w , w ; *              ] mad\t%0.<Vetype>, %1/m, %3.<Vetype>, %4.<Vetype>
     [ ?&w      , Upl , w , w , w ; yes            ] movprfx\t%0, %2\;mad\t%0.<Vetype>, %1/m, %3.<Vetype>, %4.<Vetype>
  }
)

;; Predicated integer addition of product, merging with the third input.
(define_insn "*cond_fma<mode>_4"
  [(set (match_operand:SVE_I 0 "register_operand")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand")
	   (plus:SVE_I
	     (mult:SVE_I
	       (match_operand:SVE_I 2 "register_operand")
	       (match_operand:SVE_I 3 "register_operand"))
	     (match_operand:SVE_I 4 "register_operand"))
	   (match_dup 4)]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2 , 3 , 4 ; attrs: movprfx ]
     [ w        , Upl , w , w , 0 ; *              ] mla\t%0.<Vetype>, %1/m, %2.<Vetype>, %3.<Vetype>
     [ ?&w      , Upl , w , w , w ; yes            ] movprfx\t%0, %4\;mla\t%0.<Vetype>, %1/m, %2.<Vetype>, %3.<Vetype>
  }
)

;; Predicated integer addition of product, merging with an independent value.
(define_insn_and_rewrite "*cond_fma<mode>_any"
  [(set (match_operand:SVE_I 0 "register_operand")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand")
	   (plus:SVE_I
	     (mult:SVE_I
	       (match_operand:SVE_I 2 "register_operand")
	       (match_operand:SVE_I 3 "register_operand"))
	     (match_operand:SVE_I 4 "register_operand"))
	   (match_operand:SVE_I 5 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE
   && !rtx_equal_p (operands[2], operands[5])
   && !rtx_equal_p (operands[3], operands[5])
   && !rtx_equal_p (operands[4], operands[5])"
  {@ [ cons: =0 , 1   , 2 , 3 , 4 , 5   ]
     [ &w       , Upl , w , w , w , Dz  ] movprfx\t%0.<Vetype>, %1/z, %4.<Vetype>\;mla\t%0.<Vetype>, %1/m, %2.<Vetype>, %3.<Vetype>
     [ &w       , Upl , w , w , 0 , Dz  ] movprfx\t%0.<Vetype>, %1/z, %0.<Vetype>\;mla\t%0.<Vetype>, %1/m, %2.<Vetype>, %3.<Vetype>
     [ &w       , Upl , 0 , w , w , Dz  ] movprfx\t%0.<Vetype>, %1/z, %0.<Vetype>\;mad\t%0.<Vetype>, %1/m, %3.<Vetype>, %4.<Vetype>
     [ &w       , Upl , w , 0 , w , Dz  ] movprfx\t%0.<Vetype>, %1/z, %0.<Vetype>\;mad\t%0.<Vetype>, %1/m, %2.<Vetype>, %4.<Vetype>
     [ &w       , Upl , w , w , w , 0   ] movprfx\t%0.<Vetype>, %1/m, %4.<Vetype>\;mla\t%0.<Vetype>, %1/m, %2.<Vetype>, %3.<Vetype>
     [ ?&w      , Upl , w , w , w , w   ] #
  }
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
	     (mult:SVE_I
	       (match_operand:SVE_I 1 "register_operand")
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
(define_insn "@aarch64_pred_fnma<mode>"
  [(set (match_operand:SVE_I 0 "register_operand")
	(minus:SVE_I
	  (match_operand:SVE_I 4 "register_operand")
	  (unspec:SVE_I
	    [(match_operand:<VPRED> 1 "register_operand")
	     (mult:SVE_I
	       (match_operand:SVE_I 2 "register_operand")
	       (match_operand:SVE_I 3 "register_operand"))]
	    UNSPEC_PRED_X)))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2  , 3 , 4 ; attrs: movprfx ]
     [ w        , Upl , %0 , w , w ; *              ] msb\t%0.<Vetype>, %1/m, %3.<Vetype>, %4.<Vetype>
     [ w        , Upl , w  , w , 0 ; *              ] mls\t%0.<Vetype>, %1/m, %2.<Vetype>, %3.<Vetype>
     [ ?&w      , Upl , w  , w , w ; yes            ] movprfx\t%0, %4\;mls\t%0.<Vetype>, %1/m, %2.<Vetype>, %3.<Vetype>
  }
)

;; Predicated integer subtraction of product with merging.
(define_expand "cond_fnma<mode>"
  [(set (match_operand:SVE_I 0 "register_operand")
   (unspec:SVE_I
	[(match_operand:<VPRED> 1 "register_operand")
	 (minus:SVE_I
	   (match_operand:SVE_I 4 "register_operand")
	   (mult:SVE_I
	     (match_operand:SVE_I 2 "register_operand")
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
  [(set (match_operand:SVE_I 0 "register_operand")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand")
	   (minus:SVE_I
	     (match_operand:SVE_I 4 "register_operand")
	     (mult:SVE_I
	       (match_operand:SVE_I 2 "register_operand")
	       (match_operand:SVE_I 3 "register_operand")))
	   (match_dup 2)]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2 , 3 , 4 ; attrs: movprfx ]
     [ w        , Upl , 0 , w , w ; *              ] msb\t%0.<Vetype>, %1/m, %3.<Vetype>, %4.<Vetype>
     [ ?&w      , Upl , w , w , w ; yes            ] movprfx\t%0, %2\;msb\t%0.<Vetype>, %1/m, %3.<Vetype>, %4.<Vetype>
  }
)

;; Predicated integer subtraction of product, merging with the third input.
(define_insn "*cond_fnma<mode>_4"
  [(set (match_operand:SVE_I 0 "register_operand")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand")
	   (minus:SVE_I
	     (match_operand:SVE_I 4 "register_operand")
	     (mult:SVE_I
	       (match_operand:SVE_I 2 "register_operand")
	       (match_operand:SVE_I 3 "register_operand")))
	   (match_dup 4)]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2 , 3 , 4 ; attrs: movprfx ]
     [ w        , Upl , w , w , 0 ; *              ] mls\t%0.<Vetype>, %1/m, %2.<Vetype>, %3.<Vetype>
     [ ?&w      , Upl , w , w , w ; yes            ] movprfx\t%0, %4\;mls\t%0.<Vetype>, %1/m, %2.<Vetype>, %3.<Vetype>
  }
)

;; Predicated integer subtraction of product, merging with an
;; independent value.
(define_insn_and_rewrite "*cond_fnma<mode>_any"
  [(set (match_operand:SVE_I 0 "register_operand")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand")
	   (minus:SVE_I
	     (match_operand:SVE_I 4 "register_operand")
	     (mult:SVE_I
	       (match_operand:SVE_I 2 "register_operand")
	       (match_operand:SVE_I 3 "register_operand")))
	   (match_operand:SVE_I 5 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE
   && !rtx_equal_p (operands[2], operands[5])
   && !rtx_equal_p (operands[3], operands[5])
   && !rtx_equal_p (operands[4], operands[5])"
  {@ [ cons: =0 , 1   , 2 , 3 , 4 , 5   ]
     [ &w       , Upl , w , w , w , Dz  ] movprfx\t%0.<Vetype>, %1/z, %4.<Vetype>\;mls\t%0.<Vetype>, %1/m, %2.<Vetype>, %3.<Vetype>
     [ &w       , Upl , w , w , 0 , Dz  ] movprfx\t%0.<Vetype>, %1/z, %0.<Vetype>\;mls\t%0.<Vetype>, %1/m, %2.<Vetype>, %3.<Vetype>
     [ &w       , Upl , 0 , w , w , Dz  ] movprfx\t%0.<Vetype>, %1/z, %0.<Vetype>\;msb\t%0.<Vetype>, %1/m, %3.<Vetype>, %4.<Vetype>
     [ &w       , Upl , w , 0 , w , Dz  ] movprfx\t%0.<Vetype>, %1/z, %0.<Vetype>\;msb\t%0.<Vetype>, %1/m, %2.<Vetype>, %4.<Vetype>
     [ &w       , Upl , w , w , w , 0   ] movprfx\t%0.<Vetype>, %1/m, %4.<Vetype>\;mls\t%0.<Vetype>, %1/m, %2.<Vetype>, %3.<Vetype>
     [ ?&w      , Upl , w , w , w , w   ] #
  }
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
;; - SUDOT   (I8MM)
;; - UDOT
;; - USDOT   (I8MM)
;; -------------------------------------------------------------------------

;; Four-element integer dot-product with accumulation.
(define_insn "<sur>dot_prod<mode><vsi2qi>"
  [(set (match_operand:SVE_FULL_SDI 0 "register_operand")
	(plus:SVE_FULL_SDI
	  (unspec:SVE_FULL_SDI
	    [(match_operand:<VSI2QI> 1 "register_operand")
	     (match_operand:<VSI2QI> 2 "register_operand")]
	    DOTPROD)
	  (match_operand:SVE_FULL_SDI 3 "register_operand")))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1 , 2 , 3 ; attrs: movprfx ]
     [ w        , w , w , 0 ; *              ] <sur>dot\t%0.<Vetype>, %1.<Vetype_fourth>, %2.<Vetype_fourth>
     [ ?&w      , w , w , w ; yes            ] movprfx\t%0, %3\;<sur>dot\t%0.<Vetype>, %1.<Vetype_fourth>, %2.<Vetype_fourth>
  }
)

;; Four-element integer dot-product by selected lanes with accumulation.
(define_insn "@aarch64_<sur>dot_prod_lane<SVE_FULL_SDI:mode><SVE_FULL_BHI:mode>"
  [(set (match_operand:SVE_FULL_SDI 0 "register_operand")
	(plus:SVE_FULL_SDI
	  (unspec:SVE_FULL_SDI
	    [(match_operand:SVE_FULL_BHI 1 "register_operand")
	     (unspec:SVE_FULL_BHI
	       [(match_operand:SVE_FULL_BHI 2 "register_operand")
		(match_operand:SI 3 "const_int_operand")]
	       UNSPEC_SVE_LANE_SELECT)]
	    DOTPROD)
	  (match_operand:SVE_FULL_SDI 4 "register_operand")))]
  "TARGET_SVE
   && (<SVE_FULL_SDI:elem_bits> == <SVE_FULL_BHI:elem_bits> * 4
       || (TARGET_SVE2p1_OR_SME2
	   && <SVE_FULL_SDI:elem_bits> == 32
	   && <SVE_FULL_BHI:elem_bits> == 16))"
  {@ [ cons: =0 , 1 , 2                           , 4 ; attrs: movprfx ]
     [ w        , w , <SVE_FULL_SDI:sve_lane_con> , 0 ; *              ] <sur>dot\t%0.<SVE_FULL_SDI:Vetype>, %1.<SVE_FULL_BHI:Vetype>, %2.<SVE_FULL_BHI:Vetype>[%3]
     [ ?&w      , w , <SVE_FULL_SDI:sve_lane_con> , w ; yes            ] movprfx\t%0, %4\;<sur>dot\t%0.<SVE_FULL_SDI:Vetype>, %1.<SVE_FULL_BHI:Vetype>, %2.<SVE_FULL_BHI:Vetype>[%3]
  }
)

(define_insn "@<sur>dot_prod<mode><vsi2qi>"
  [(set (match_operand:VNx4SI_ONLY 0 "register_operand")
        (plus:VNx4SI_ONLY
	  (unspec:VNx4SI_ONLY
	    [(match_operand:<VSI2QI> 1 "register_operand")
	     (match_operand:<VSI2QI> 2 "register_operand")]
	    DOTPROD_US_ONLY)
	  (match_operand:VNx4SI_ONLY 3 "register_operand")))]
  "TARGET_SVE_I8MM"
  {@ [ cons: =0 , 1 , 2 , 3 ; attrs: movprfx ]
     [ w        , w , w , 0 ; *              ] <sur>dot\t%0.s, %1.b, %2.b
     [ ?&w      , w , w , w ; yes            ] movprfx\t%0, %3\;<sur>dot\t%0.s, %1.b, %2.b
  }
)

(define_insn "@aarch64_<sur>dot_prod_lane<VNx4SI_ONLY:mode><VNx16QI_ONLY:mode>"
  [(set (match_operand:VNx4SI_ONLY 0 "register_operand")
	(plus:VNx4SI_ONLY
	  (unspec:VNx4SI_ONLY
	    [(match_operand:VNx16QI_ONLY 1 "register_operand")
	     (unspec:VNx16QI_ONLY
	       [(match_operand:VNx16QI_ONLY 2 "register_operand")
		(match_operand:SI 3 "const_int_operand")]
	       UNSPEC_SVE_LANE_SELECT)]
	    DOTPROD_I8MM)
	  (match_operand:VNx4SI_ONLY 4 "register_operand")))]
  "TARGET_SVE_I8MM"
  {@ [ cons: =0 , 1 , 2 , 4 ; attrs: movprfx ]
     [ w        , w , y , 0 ; *              ] <sur>dot\t%0.s, %1.b, %2.b[%3]
     [ ?&w      , w , y , w ; yes            ] movprfx\t%0, %4\;<sur>dot\t%0.s, %1.b, %2.b[%3]
  }
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
(define_expand "<su>sad<vsi2qi>"
  [(use (match_operand:SVE_FULL_SDI 0 "register_operand"))
   (USMAX:<VSI2QI> (match_operand:<VSI2QI> 1 "register_operand")
		   (match_operand:<VSI2QI> 2 "register_operand"))
   (use (match_operand:SVE_FULL_SDI 3 "register_operand"))]
  "TARGET_SVE"
  {
    rtx ones = force_reg (<VSI2QI>mode, CONST1_RTX (<VSI2QI>mode));
    rtx diff = gen_reg_rtx (<VSI2QI>mode);
    emit_insn (gen_<su>abd<vsi2qi>3 (diff, operands[1], operands[2]));
    emit_insn (gen_udot_prod<mode><vsi2qi> (operands[0], diff, ones,
					    operands[3]));
    DONE;
  }
)

;; -------------------------------------------------------------------------
;; ---- [INT] Matrix multiply-accumulate
;; -------------------------------------------------------------------------
;; Includes:
;; - SMMLA (I8MM)
;; - UMMLA (I8MM)
;; - USMMLA (I8MM)
;; -------------------------------------------------------------------------

(define_insn "@aarch64_sve_add_<optab><vsi2qi>"
  [(set (match_operand:VNx4SI_ONLY 0 "register_operand")
	(plus:VNx4SI_ONLY
	  (unspec:VNx4SI_ONLY
	    [(match_operand:<VSI2QI> 2 "register_operand")
	     (match_operand:<VSI2QI> 3 "register_operand")]
	    MATMUL)
	  (match_operand:VNx4SI_ONLY 1 "register_operand")))]
  "TARGET_SVE_I8MM && TARGET_NON_STREAMING"
  {@ [ cons: =0 , 1 , 2 , 3 ; attrs: movprfx ]
     [ w        , 0 , w , w ; *              ] <sur>mmla\t%0.s, %2.b, %3.b
     [ ?&w      , w , w , w ; yes            ] movprfx\t%0, %1\;<sur>mmla\t%0.s, %2.b, %3.b
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
  [(set (match_operand:SVE_FULL_F_BF 0 "register_operand")
	(unspec:SVE_FULL_F_BF
	  [(match_dup 4)
	   (const_int SVE_RELAXED_GP)
	   (match_operand:SVE_FULL_F_BF 1 "register_operand")
	   (match_operand:SVE_FULL_F_BF 2 "register_operand")
	   (match_operand:SVE_FULL_F_BF 3 "register_operand")]
	  SVE_COND_FP_TERNARY))]
  "TARGET_SVE && (<supports_bf16> || !<is_bf16>)"
  {
    operands[4] = aarch64_ptrue_reg (<VPRED>mode);
  }
)

;; Predicated floating-point ternary operations.
(define_insn "@aarch64_pred_<optab><mode>"
  [(set (match_operand:SVE_FULL_F_BF 0 "register_operand")
	(unspec:SVE_FULL_F_BF
	  [(match_operand:<VPRED> 1 "register_operand")
	   (match_operand:SI 5 "aarch64_sve_gp_strictness")
	   (match_operand:SVE_FULL_F_BF 2 "register_operand")
	   (match_operand:SVE_FULL_F_BF 3 "register_operand")
	   (match_operand:SVE_FULL_F_BF 4 "register_operand")]
	  SVE_COND_FP_TERNARY))]
  "TARGET_SVE && (<supports_bf16> || !<is_bf16>)"
  {@ [ cons: =0 , 1   , 2  , 3 , 4 ; attrs: movprfx , is_rev ]
     [ w        , Upl , %w , w , 0 ; *   , *    ] <b><sve_fmla_op>\t%0.<Vetype>, %1/m, %2.<Vetype>, %3.<Vetype>
     [ w        , Upl , 0  , w , w ; *   , true ] <b><sve_fmad_op>\t%0.<Vetype>, %1/m, %3.<Vetype>, %4.<Vetype>
     [ ?&w      , Upl , w  , w , w ; yes , *    ] movprfx\t%0, %4\;<b><sve_fmla_op>\t%0.<Vetype>, %1/m, %2.<Vetype>, %3.<Vetype>
  }
  [(set_attr "is_bf16" "<is_bf16>")
   (set_attr "supports_bf16_rev" "false")]
)

;; Predicated floating-point ternary operations with merging.
(define_expand "@cond_<optab><mode>"
  [(set (match_operand:SVE_FULL_F_BF 0 "register_operand")
	(unspec:SVE_FULL_F_BF
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_F_BF
	     [(match_dup 1)
	      (const_int SVE_STRICT_GP)
	      (match_operand:SVE_FULL_F_BF 2 "register_operand")
	      (match_operand:SVE_FULL_F_BF 3 "register_operand")
	      (match_operand:SVE_FULL_F_BF 4 "register_operand")]
	     SVE_COND_FP_TERNARY)
	   (match_operand:SVE_FULL_F_BF 5 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE && (<supports_bf16> || !<is_bf16>)"
{
  /* Swap the multiplication operands if the fallback value is the
     second of the two.  */
  if (rtx_equal_p (operands[3], operands[5]))
    std::swap (operands[2], operands[3]);
})

;; Predicated floating-point ternary operations, merging with the
;; first input.
(define_insn_and_rewrite "*cond_<optab><mode>_2_relaxed"
  [(set (match_operand:SVE_FULL_F 0 "register_operand")
	(unspec:SVE_FULL_F
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_F
	     [(match_operand 5)
	      (const_int SVE_RELAXED_GP)
	      (match_operand:SVE_FULL_F 2 "register_operand")
	      (match_operand:SVE_FULL_F 3 "register_operand")
	      (match_operand:SVE_FULL_F 4 "register_operand")]
	     SVE_COND_FP_TERNARY)
	   (match_dup 2)]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2 , 3 , 4 ; attrs: movprfx ]
     [ w        , Upl , 0 , w , w ; *              ] <sve_fmad_op>\t%0.<Vetype>, %1/m, %3.<Vetype>, %4.<Vetype>
     [ ?&w      , Upl , w , w , w ; yes            ] movprfx\t%0, %2\;<sve_fmad_op>\t%0.<Vetype>, %1/m, %3.<Vetype>, %4.<Vetype>
  }
  "&& !rtx_equal_p (operands[1], operands[5])"
  {
    operands[5] = copy_rtx (operands[1]);
  }
)

(define_insn "*cond_<optab><mode>_2_strict"
  [(set (match_operand:SVE_FULL_F 0 "register_operand")
	(unspec:SVE_FULL_F
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_F
	     [(match_dup 1)
	      (const_int SVE_STRICT_GP)
	      (match_operand:SVE_FULL_F 2 "register_operand")
	      (match_operand:SVE_FULL_F 3 "register_operand")
	      (match_operand:SVE_FULL_F 4 "register_operand")]
	     SVE_COND_FP_TERNARY)
	   (match_dup 2)]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2 , 3 , 4 ; attrs: movprfx ]
     [ w        , Upl , 0 , w , w ; *              ] <sve_fmad_op>\t%0.<Vetype>, %1/m, %3.<Vetype>, %4.<Vetype>
     [ ?&w      , Upl , w , w , w ; yes            ] movprfx\t%0, %2\;<sve_fmad_op>\t%0.<Vetype>, %1/m, %3.<Vetype>, %4.<Vetype>
  }
)

;; Predicated floating-point ternary operations, merging with the
;; third input.
(define_insn_and_rewrite "*cond_<optab><mode>_4_relaxed"
  [(set (match_operand:SVE_FULL_F_BF 0 "register_operand")
	(unspec:SVE_FULL_F_BF
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_F_BF
	     [(match_operand 5)
	      (const_int SVE_RELAXED_GP)
	      (match_operand:SVE_FULL_F_BF 2 "register_operand")
	      (match_operand:SVE_FULL_F_BF 3 "register_operand")
	      (match_operand:SVE_FULL_F_BF 4 "register_operand")]
	     SVE_COND_FP_TERNARY)
	   (match_dup 4)]
	  UNSPEC_SEL))]
  "TARGET_SVE && (<supports_bf16> || !<is_bf16>)"
  {@ [ cons: =0 , 1   , 2 , 3 , 4 ; attrs: movprfx ]
     [ w        , Upl , w , w , 0 ; *              ] <b><sve_fmla_op>\t%0.<Vetype>, %1/m, %2.<Vetype>, %3.<Vetype>
     [ ?&w      , Upl , w , w , w ; yes            ] movprfx\t%0, %4\;<b><sve_fmla_op>\t%0.<Vetype>, %1/m, %2.<Vetype>, %3.<Vetype>
  }
  "&& !rtx_equal_p (operands[1], operands[5])"
  {
    operands[5] = copy_rtx (operands[1]);
  }
)

(define_insn "*cond_<optab><mode>_4_strict"
  [(set (match_operand:SVE_FULL_F_BF 0 "register_operand")
	(unspec:SVE_FULL_F_BF
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_F_BF
	     [(match_dup 1)
	      (const_int SVE_STRICT_GP)
	      (match_operand:SVE_FULL_F_BF 2 "register_operand")
	      (match_operand:SVE_FULL_F_BF 3 "register_operand")
	      (match_operand:SVE_FULL_F_BF 4 "register_operand")]
	     SVE_COND_FP_TERNARY)
	   (match_dup 4)]
	  UNSPEC_SEL))]
  "TARGET_SVE && (<supports_bf16> || !<is_bf16>)"
  {@ [ cons: =0 , 1   , 2 , 3 , 4 ; attrs: movprfx ]
     [ w        , Upl , w , w , 0 ; *              ] <b><sve_fmla_op>\t%0.<Vetype>, %1/m, %2.<Vetype>, %3.<Vetype>
     [ ?&w      , Upl , w , w , w ; yes            ] movprfx\t%0, %4\;<b><sve_fmla_op>\t%0.<Vetype>, %1/m, %2.<Vetype>, %3.<Vetype>
  }
)

;; Predicated floating-point ternary operations, merging with an
;; independent value.
(define_insn_and_rewrite "*cond_<optab><mode>_any_relaxed"
  [(set (match_operand:SVE_FULL_F_BF 0 "register_operand")
	(unspec:SVE_FULL_F_BF
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_F_BF
	     [(match_operand 6)
	      (const_int SVE_RELAXED_GP)
	      (match_operand:SVE_FULL_F_BF 2 "register_operand")
	      (match_operand:SVE_FULL_F_BF 3 "register_operand")
	      (match_operand:SVE_FULL_F_BF 4 "register_operand")]
	     SVE_COND_FP_TERNARY)
	   (match_operand:SVE_FULL_F_BF 5 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE
   && (<supports_bf16> || !<is_bf16>)
   && (<is_bf16> || !rtx_equal_p (operands[2], operands[5]))
   && (<is_bf16> || !rtx_equal_p (operands[3], operands[5]))
   && !rtx_equal_p (operands[4], operands[5])"
  {@ [ cons: =0 , 1   , 2 , 3 , 4 , 5  ; attrs: is_rev ]
     [ &w       , Upl , w , w , w , Dz ; *    ] movprfx\t%0.<Vetype>, %1/z, %4.<Vetype>\;<b><sve_fmla_op>\t%0.<Vetype>, %1/m, %2.<Vetype>, %3.<Vetype>
     [ &w       , Upl , w , w , 0 , Dz ; *    ] movprfx\t%0.<Vetype>, %1/z, %0.<Vetype>\;<b><sve_fmla_op>\t%0.<Vetype>, %1/m, %2.<Vetype>, %3.<Vetype>
     [ &w       , Upl , 0 , w , w , Dz ; true ] movprfx\t%0.<Vetype>, %1/z, %0.<Vetype>\;<b><sve_fmad_op>\t%0.<Vetype>, %1/m, %3.<Vetype>, %4.<Vetype>
     [ &w       , Upl , w , 0 , w , Dz ; true ] movprfx\t%0.<Vetype>, %1/z, %0.<Vetype>\;<b><sve_fmad_op>\t%0.<Vetype>, %1/m, %2.<Vetype>, %4.<Vetype>
     [ &w       , Upl , w , w , w , 0  ; *    ] movprfx\t%0.<Vetype>, %1/m, %4.<Vetype>\;<b><sve_fmla_op>\t%0.<Vetype>, %1/m, %2.<Vetype>, %3.<Vetype>
     [ ?&w      , Upl , w , w , w , w  ; *    ] #
  }
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
  [(set_attr "movprfx" "yes")
   (set_attr "is_bf16" "<is_bf16>")
   (set_attr "supports_bf16_rev" "false")]
)

(define_insn_and_rewrite "*cond_<optab><mode>_any_strict"
  [(set (match_operand:SVE_FULL_F_BF 0 "register_operand")
	(unspec:SVE_FULL_F_BF
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_F_BF
	     [(match_dup 1)
	      (const_int SVE_STRICT_GP)
	      (match_operand:SVE_FULL_F_BF 2 "register_operand")
	      (match_operand:SVE_FULL_F_BF 3 "register_operand")
	      (match_operand:SVE_FULL_F_BF 4 "register_operand")]
	     SVE_COND_FP_TERNARY)
	   (match_operand:SVE_FULL_F_BF 5 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE
   && (<supports_bf16> || !<is_bf16>)
   && (<is_bf16> || !rtx_equal_p (operands[2], operands[5]))
   && (<is_bf16> || !rtx_equal_p (operands[3], operands[5]))
   && !rtx_equal_p (operands[4], operands[5])"
  {@ [ cons: =0 , 1   , 2 , 3 , 4 , 5  ; attrs: is_rev ]
     [ &w       , Upl , w , w , w , Dz ; *    ] movprfx\t%0.<Vetype>, %1/z, %4.<Vetype>\;<b><sve_fmla_op>\t%0.<Vetype>, %1/m, %2.<Vetype>, %3.<Vetype>
     [ &w       , Upl , w , w , 0 , Dz ; *    ] movprfx\t%0.<Vetype>, %1/z, %0.<Vetype>\;<b><sve_fmla_op>\t%0.<Vetype>, %1/m, %2.<Vetype>, %3.<Vetype>
     [ &w       , Upl , 0 , w , w , Dz ; true ] movprfx\t%0.<Vetype>, %1/z, %0.<Vetype>\;<b><sve_fmad_op>\t%0.<Vetype>, %1/m, %3.<Vetype>, %4.<Vetype>
     [ &w       , Upl , w , 0 , w , Dz ; true ] movprfx\t%0.<Vetype>, %1/z, %0.<Vetype>\;<b><sve_fmad_op>\t%0.<Vetype>, %1/m, %2.<Vetype>, %4.<Vetype>
     [ &w       , Upl , w , w , w , 0  ; *    ] movprfx\t%0.<Vetype>, %1/m, %4.<Vetype>\;<b><sve_fmla_op>\t%0.<Vetype>, %1/m, %2.<Vetype>, %3.<Vetype>
     [ ?&w      , Upl , w , w , w , w  ; *    ] #
  }
  "&& reload_completed
   && register_operand (operands[5], <MODE>mode)
   && !rtx_equal_p (operands[0], operands[5])"
  {
    emit_insn (gen_vcond_mask_<mode><vpred> (operands[0], operands[4],
					     operands[5], operands[1]));
    operands[5] = operands[4] = operands[0];
  }
  [(set_attr "movprfx" "yes")
   (set_attr "is_bf16" "<is_bf16>")
   (set_attr "supports_bf16_rev" "false")]
)

;; Unpredicated FMLA and FMLS by selected lanes.  It doesn't seem worth using
;; (fma ...) since target-independent code won't understand the indexing.
(define_insn "@aarch64_<optab>_lane_<mode>"
  [(set (match_operand:SVE_FULL_F_BF 0 "register_operand")
	(unspec:SVE_FULL_F_BF
	  [(match_operand:SVE_FULL_F_BF 1 "register_operand")
	   (unspec:SVE_FULL_F_BF
	     [(match_operand:SVE_FULL_F_BF 2 "register_operand")
	      (match_operand:SI 3 "const_int_operand")]
	     UNSPEC_SVE_LANE_SELECT)
	   (match_operand:SVE_FULL_F_BF 4 "register_operand")]
	  SVE_FP_TERNARY_LANE))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1 , 2              , 4 ; attrs: movprfx ]
     [ w        , w , <sve_lane_con> , 0 ; *              ] <b><sve_fp_op>\t%0.<Vetype>, %1.<Vetype>, %2.<Vetype>[%3]
     [ ?&w      , w , <sve_lane_con> , w ; yes            ] movprfx\t%0, %4\;<b><sve_fp_op>\t%0.<Vetype>, %1.<Vetype>, %2.<Vetype>[%3]
  }
)

;; -------------------------------------------------------------------------
;; ---- [FP] Complex multiply-add
;; -------------------------------------------------------------------------
;; Includes merging patterns for:
;; - FCMLA
;; -------------------------------------------------------------------------

;; Predicated FCMLA.
(define_insn "@aarch64_pred_<optab><mode>"
  [(set (match_operand:SVE_FULL_F 0 "register_operand")
	(unspec:SVE_FULL_F
	  [(match_operand:<VPRED> 1 "register_operand")
	   (match_operand:SI 5 "aarch64_sve_gp_strictness")
	   (match_operand:SVE_FULL_F 2 "register_operand")
	   (match_operand:SVE_FULL_F 3 "register_operand")
	   (match_operand:SVE_FULL_F 4 "register_operand")]
	  SVE_COND_FCMLA))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2 , 3 , 4 ; attrs: movprfx ]
     [ w        , Upl , w , w , 0 ; *              ] fcmla\t%0.<Vetype>, %1/m, %2.<Vetype>, %3.<Vetype>, #<rot>
     [ ?&w      , Upl , w , w , w ; yes            ] movprfx\t%0, %4\;fcmla\t%0.<Vetype>, %1/m, %2.<Vetype>, %3.<Vetype>, #<rot>
  }
)

;; unpredicated optab pattern for auto-vectorizer
;; The complex mla/mls operations always need to expand to two instructions.
;; The first operation does half the computation and the second does the
;; remainder.  Because of this, expand early.
(define_expand "cml<fcmac1><conj_op><mode>4"
  [(set (match_operand:SVE_FULL_F 0 "register_operand")
	(unspec:SVE_FULL_F
	  [(match_dup 4)
	   (match_dup 5)
	   (match_operand:SVE_FULL_F 1 "register_operand")
	   (match_operand:SVE_FULL_F 2 "register_operand")
	   (match_operand:SVE_FULL_F 3 "register_operand")]
	  FCMLA_OP))]
  "TARGET_SVE"
{
  operands[4] = aarch64_ptrue_reg (<VPRED>mode);
  operands[5] = gen_int_mode (SVE_RELAXED_GP, SImode);
  rtx tmp = gen_reg_rtx (<MODE>mode);
  emit_insn
    (gen_aarch64_pred_fcmla<sve_rot1><mode> (tmp, operands[4],
					     operands[2], operands[1],
					     operands[3], operands[5]));
  emit_insn
    (gen_aarch64_pred_fcmla<sve_rot2><mode> (operands[0], operands[4],
					     operands[2], operands[1],
					     tmp, operands[5]));
  DONE;
})

;; unpredicated optab pattern for auto-vectorizer
;; The complex mul operations always need to expand to two instructions.
;; The first operation does half the computation and the second does the
;; remainder.  Because of this, expand early.
(define_expand "cmul<conj_op><mode>3"
  [(set (match_operand:SVE_FULL_F 0 "register_operand")
	(unspec:SVE_FULL_F
	   [(match_operand:SVE_FULL_F 1 "register_operand")
	    (match_operand:SVE_FULL_F 2 "register_operand")]
	  FCMUL_OP))]
  "TARGET_SVE"
{
  rtx pred_reg = aarch64_ptrue_reg (<VPRED>mode);
  rtx gp_mode = gen_int_mode (SVE_RELAXED_GP, SImode);
  rtx accum = force_reg (<MODE>mode, CONST0_RTX (<MODE>mode));
  rtx tmp = gen_reg_rtx (<MODE>mode);
  emit_insn
    (gen_aarch64_pred_fcmla<sve_rot1><mode> (tmp, pred_reg,
					     operands[2], operands[1],
					     accum, gp_mode));
  emit_insn
    (gen_aarch64_pred_fcmla<sve_rot2><mode> (operands[0], pred_reg,
					     operands[2], operands[1],
					     tmp, gp_mode));
  DONE;
})

;; Predicated FCMLA with merging.
(define_expand "@cond_<optab><mode>"
  [(set (match_operand:SVE_FULL_F 0 "register_operand")
	(unspec:SVE_FULL_F
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_F
	     [(match_dup 1)
	      (const_int SVE_STRICT_GP)
	      (match_operand:SVE_FULL_F 2 "register_operand")
	      (match_operand:SVE_FULL_F 3 "register_operand")
	      (match_operand:SVE_FULL_F 4 "register_operand")]
	     SVE_COND_FCMLA)
	   (match_operand:SVE_FULL_F 5 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE"
)

;; Predicated FCMLA, merging with the third input.
(define_insn_and_rewrite "*cond_<optab><mode>_4_relaxed"
  [(set (match_operand:SVE_FULL_F 0 "register_operand")
	(unspec:SVE_FULL_F
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_F
	     [(match_operand 5)
	      (const_int SVE_RELAXED_GP)
	      (match_operand:SVE_FULL_F 2 "register_operand")
	      (match_operand:SVE_FULL_F 3 "register_operand")
	      (match_operand:SVE_FULL_F 4 "register_operand")]
	     SVE_COND_FCMLA)
	   (match_dup 4)]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2 , 3 , 4 ; attrs: movprfx ]
     [ w        , Upl , w , w , 0 ; *              ] fcmla\t%0.<Vetype>, %1/m, %2.<Vetype>, %3.<Vetype>, #<rot>
     [ ?&w      , Upl , w , w , w ; yes            ] movprfx\t%0, %4\;fcmla\t%0.<Vetype>, %1/m, %2.<Vetype>, %3.<Vetype>, #<rot>
  }
  "&& !rtx_equal_p (operands[1], operands[5])"
  {
    operands[5] = copy_rtx (operands[1]);
  }
)

(define_insn "*cond_<optab><mode>_4_strict"
  [(set (match_operand:SVE_FULL_F 0 "register_operand")
	(unspec:SVE_FULL_F
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_F
	     [(match_dup 1)
	      (const_int SVE_STRICT_GP)
	      (match_operand:SVE_FULL_F 2 "register_operand")
	      (match_operand:SVE_FULL_F 3 "register_operand")
	      (match_operand:SVE_FULL_F 4 "register_operand")]
	     SVE_COND_FCMLA)
	   (match_dup 4)]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2 , 3 , 4 ; attrs: movprfx ]
     [ w        , Upl , w , w , 0 ; *              ] fcmla\t%0.<Vetype>, %1/m, %2.<Vetype>, %3.<Vetype>, #<rot>
     [ ?&w      , Upl , w , w , w ; yes            ] movprfx\t%0, %4\;fcmla\t%0.<Vetype>, %1/m, %2.<Vetype>, %3.<Vetype>, #<rot>
  }
)

;; Predicated FCMLA, merging with an independent value.
(define_insn_and_rewrite "*cond_<optab><mode>_any_relaxed"
  [(set (match_operand:SVE_FULL_F 0 "register_operand")
	(unspec:SVE_FULL_F
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_F
	     [(match_operand 6)
	      (const_int SVE_RELAXED_GP)
	      (match_operand:SVE_FULL_F 2 "register_operand")
	      (match_operand:SVE_FULL_F 3 "register_operand")
	      (match_operand:SVE_FULL_F 4 "register_operand")]
	     SVE_COND_FCMLA)
	   (match_operand:SVE_FULL_F 5 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE && !rtx_equal_p (operands[4], operands[5])"
  {@ [ cons: =0 , 1   , 2 , 3 , 4 , 5   ]
     [ &w       , Upl , w , w , w , Dz  ] movprfx\t%0.<Vetype>, %1/z, %4.<Vetype>\;fcmla\t%0.<Vetype>, %1/m, %2.<Vetype>, %3.<Vetype>, #<rot>
     [ &w       , Upl , w , w , 0 , Dz  ] movprfx\t%0.<Vetype>, %1/z, %0.<Vetype>\;fcmla\t%0.<Vetype>, %1/m, %2.<Vetype>, %3.<Vetype>, #<rot>
     [ &w       , Upl , w , w , w , 0   ] movprfx\t%0.<Vetype>, %1/m, %4.<Vetype>\;fcmla\t%0.<Vetype>, %1/m, %2.<Vetype>, %3.<Vetype>, #<rot>
     [ ?&w      , Upl , w , w , w , w   ] #
  }
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

(define_insn_and_rewrite "*cond_<optab><mode>_any_strict"
  [(set (match_operand:SVE_FULL_F 0 "register_operand")
	(unspec:SVE_FULL_F
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_F
	     [(match_dup 1)
	      (const_int SVE_STRICT_GP)
	      (match_operand:SVE_FULL_F 2 "register_operand")
	      (match_operand:SVE_FULL_F 3 "register_operand")
	      (match_operand:SVE_FULL_F 4 "register_operand")]
	     SVE_COND_FCMLA)
	   (match_operand:SVE_FULL_F 5 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE && !rtx_equal_p (operands[4], operands[5])"
  {@ [ cons: =0 , 1   , 2 , 3 , 4 , 5   ]
     [ &w       , Upl , w , w , w , Dz  ] movprfx\t%0.<Vetype>, %1/z, %4.<Vetype>\;fcmla\t%0.<Vetype>, %1/m, %2.<Vetype>, %3.<Vetype>, #<rot>
     [ &w       , Upl , w , w , 0 , Dz  ] movprfx\t%0.<Vetype>, %1/z, %0.<Vetype>\;fcmla\t%0.<Vetype>, %1/m, %2.<Vetype>, %3.<Vetype>, #<rot>
     [ &w       , Upl , w , w , w , 0   ] movprfx\t%0.<Vetype>, %1/m, %4.<Vetype>\;fcmla\t%0.<Vetype>, %1/m, %2.<Vetype>, %3.<Vetype>, #<rot>
     [ ?&w      , Upl , w , w , w , w   ] #
  }
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

;; Unpredicated FCMLA with indexing.
(define_insn "@aarch64_<optab>_lane_<mode>"
  [(set (match_operand:SVE_FULL_HSF 0 "register_operand")
	(unspec:SVE_FULL_HSF
	  [(match_operand:SVE_FULL_HSF 1 "register_operand")
	   (unspec:SVE_FULL_HSF
	     [(match_operand:SVE_FULL_HSF 2 "register_operand")
	      (match_operand:SI 3 "const_int_operand")]
	     UNSPEC_SVE_LANE_SELECT)
	   (match_operand:SVE_FULL_HSF 4 "register_operand")]
	  FCMLA))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1 , 2                   , 4 ; attrs: movprfx ]
     [ w        , w , <sve_lane_pair_con> , 0 ; *              ] fcmla\t%0.<Vetype>, %1.<Vetype>, %2.<Vetype>[%3], #<rot>
     [ ?&w      , w , <sve_lane_pair_con> , w ; yes            ] movprfx\t%0, %4\;fcmla\t%0.<Vetype>, %1.<Vetype>, %2.<Vetype>[%3], #<rot>
  }
)

;; -------------------------------------------------------------------------
;; ---- [FP] Trigonometric multiply-add
;; -------------------------------------------------------------------------
;; Includes:
;; - FTMAD
;; -------------------------------------------------------------------------

(define_insn "@aarch64_sve_tmad<mode>"
  [(set (match_operand:SVE_FULL_F 0 "register_operand")
	(unspec:SVE_FULL_F
	  [(match_operand:SVE_FULL_F 1 "register_operand")
	   (match_operand:SVE_FULL_F 2 "register_operand")
	   (match_operand:DI 3 "const_int_operand")]
	  UNSPEC_FTMAD))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1 , 2 ; attrs: movprfx ]
     [ w        , 0 , w ; *              ] ftmad\t%0.<Vetype>, %0.<Vetype>, %2.<Vetype>, #%3
     [ ?&w      , w , w ; yes            ] movprfx\t%0, %1\;ftmad\t%0.<Vetype>, %0.<Vetype>, %2.<Vetype>, #%3
  }
)

;; -------------------------------------------------------------------------
;; ---- [FP] Bfloat16 long ternary arithmetic (SF,BF,BF)
;; -------------------------------------------------------------------------
;; Includes:
;; - BFDOT (BF16)
;; - BFMLALB (BF16)
;; - BFMLALT (BF16)
;; - BFMLSLB (SVE2p1, SME2)
;; - BFMLSLT (SVE2p1, SME2)
;; - BFMMLA (BF16)
;; -------------------------------------------------------------------------

(define_insn "@aarch64_sve_<sve_fp_op>vnx4sf"
  [(set (match_operand:VNx4SF 0 "register_operand")
	(unspec:VNx4SF
	  [(match_operand:VNx4SF 1 "register_operand")
	   (match_operand:VNx8BF 2 "register_operand")
	   (match_operand:VNx8BF 3 "register_operand")]
	  SVE_BFLOAT_TERNARY_LONG))]
  ""
  {@ [ cons: =0 , 1 , 2 , 3 ; attrs: movprfx ]
     [ w        , 0 , w , w ; *              ] <sve_fp_op>\t%0.s, %2.h, %3.h
     [ ?&w      , w , w , w ; yes            ] movprfx\t%0, %1\;<sve_fp_op>\t%0.s, %2.h, %3.h
  }
)

;; The immediate range is enforced before generating the instruction.
(define_insn "@aarch64_sve_<sve_fp_op>_lanevnx4sf"
  [(set (match_operand:VNx4SF 0 "register_operand")
	(unspec:VNx4SF
	  [(match_operand:VNx4SF 1 "register_operand")
	   (match_operand:VNx8BF 2 "register_operand")
	   (match_operand:VNx8BF 3 "register_operand")
	   (match_operand:SI 4 "const_int_operand")]
	  SVE_BFLOAT_TERNARY_LONG_LANE))]
  ""
  {@ [ cons: =0 , 1 , 2 , 3 ; attrs: movprfx ]
     [ w        , 0 , w , y ; *              ] <sve_fp_op>\t%0.s, %2.h, %3.h[%4]
     [ ?&w      , w , w , y ; yes            ] movprfx\t%0, %1\;<sve_fp_op>\t%0.s, %2.h, %3.h[%4]
  }
)

;; -------------------------------------------------------------------------
;; ---- [FP] Matrix multiply-accumulate
;; -------------------------------------------------------------------------
;; Includes:
;; - FMMLA (F32MM,F64MM)
;; -------------------------------------------------------------------------

;; The mode iterator enforces the target requirements.
(define_insn "@aarch64_sve_<sve_fp_op><mode>"
  [(set (match_operand:SVE_MATMULF 0 "register_operand")
	(unspec:SVE_MATMULF
	  [(match_operand:SVE_MATMULF 2 "register_operand")
	   (match_operand:SVE_MATMULF 3 "register_operand")
	   (match_operand:SVE_MATMULF 1 "register_operand")]
	  FMMLA))]
  "TARGET_SVE && TARGET_NON_STREAMING"
  {@ [ cons: =0 , 1 , 2 , 3 ; attrs: movprfx ]
     [ w        , 0 , w , w ; *              ] <sve_fp_op>\t%0.<Vetype>, %2.<Vetype>, %3.<Vetype>
     [ ?&w      , w , w , w ; yes            ] movprfx\t%0, %1\;<sve_fp_op>\t%0.<Vetype>, %2.<Vetype>, %3.<Vetype>
  }
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
;;
;; For unpacked vectors, it doesn't really matter whether SEL uses the
;; the container size or the element size.  If SEL used the container size,
;; it would ignore undefined bits of the predicate but would copy the
;; upper (undefined) bits of each container along with the defined bits.
;; If SEL used the element size, it would use undefined bits of the predicate
;; to select between undefined elements in each input vector.  Thus the only
;; difference is whether the undefined bits in a container always come from
;; the same input as the defined bits, or whether the choice can vary
;; independently of the defined bits.
;;
;; For the other instructions, using the element size is more natural,
;; so we do that for SEL as well.
(define_insn "*vcond_mask_<mode><vpred>"
  [(set (match_operand:SVE_ALL 0 "register_operand")
	(unspec:SVE_ALL
	  [(match_operand:<VPRED> 3 "register_operand")
	   (match_operand:SVE_ALL 1 "aarch64_sve_reg_or_dup_imm")
	   (match_operand:SVE_ALL 2 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE
   && (!register_operand (operands[1], <MODE>mode)
       || register_operand (operands[2], <MODE>mode))"
  {@ [ cons: =0 , 1   , 2  , 3   ; attrs: movprfx ]
     [ w        , w   , w  , Upa ; *              ] sel\t%0.<Vetype>, %3, %1.<Vetype>, %2.<Vetype>
     [ w        , vss , 0  , Upa ; *              ] mov\t%0.<Vetype>, %3/m, #%I1
     [ w        , vss , Dz , Upa ; *              ] mov\t%0.<Vetype>, %3/z, #%I1
     [ w        , Ufc , 0  , Upa ; *              ] fmov\t%0.<Vetype>, %3/m, #%1
     [ ?w       , Ufc , Dz , Upl ; yes            ] movprfx\t%0.<Vetype>, %3/z, %0.<Vetype>\;fmov\t%0.<Vetype>, %3/m, #%1
     [ ?&w      , vss , w  , Upa ; yes            ] movprfx\t%0, %2\;mov\t%0.<Vetype>, %3/m, #%I1
     [ ?&w      , Ufc , w  , Upa ; yes            ] movprfx\t%0, %2\;fmov\t%0.<Vetype>, %3/m, #%1
  }
)

;; Optimize selects between a duplicated scalar variable and another vector,
;; the latter of which can be a zero constant or a variable.  Treat duplicates
;; of GPRs as being more expensive than duplicates of FPRs, since they
;; involve a cross-file move.
(define_insn "@aarch64_sel_dup<mode>"
  [(set (match_operand:SVE_ALL 0 "register_operand")
	(unspec:SVE_ALL
	  [(match_operand:<VPRED> 3 "register_operand")
	   (vec_duplicate:SVE_ALL
	     (match_operand:<VEL> 1 "register_operand"))
	   (match_operand:SVE_ALL 2 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1 , 2  , 3   ; attrs: movprfx ]
     [ ?w       , r , 0  , Upl ; *              ] mov\t%0.<Vetype>, %3/m, %<vwcore>1
     [ w        , w , 0  , Upl ; *              ] mov\t%0.<Vetype>, %3/m, %<Vetype>1
     [ ??w      , r , Dz , Upl ; yes            ] movprfx\t%0.<Vetype>, %3/z, %0.<Vetype>\;mov\t%0.<Vetype>, %3/m, %<vwcore>1
     [ ?&w      , w , Dz , Upl ; yes            ] movprfx\t%0.<Vetype>, %3/z, %0.<Vetype>\;mov\t%0.<Vetype>, %3/m, %<Vetype>1
     [ ??&w     , r , w  , Upl ; yes            ] movprfx\t%0, %2\;mov\t%0.<Vetype>, %3/m, %<vwcore>1
     [ ?&w      , w , w  , Upl ; yes            ] movprfx\t%0, %2\;mov\t%0.<Vetype>, %3/m, %<Vetype>1
  }
)

;; -------------------------------------------------------------------------
;; ---- [INT] Comparisons
;; -------------------------------------------------------------------------
;; Includes:
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
;;
;; For unpacked vectors, only the lowpart element in each input container
;; has a defined value, and only the predicate bits associated with
;; those elements are defined.  For example, when comparing two VNx2SIs:
;;
;; - The VNx2SIs can be seem as VNx2DIs in which the low halves of each
;;   DI container store an SI element.  The upper bits of each DI container
;;   are undefined.
;;
;; - Alternatively, the VNx2SIs can be seen as VNx4SIs in which the
;;   even elements are defined and the odd elements are undefined.
;;
;; - The associated predicate mode is VNx2BI.  This means that only the
;;   low bit in each predicate byte is defined (on input and on output).
;;
;; - We use a .s comparison to compare VNx2SIs, under the control of a
;;   VNx2BI governing predicate, to produce a VNx2BI result.  If we view
;;   the .s operation as operating on VNx4SIs then for odd lanes:
;;
;;   - the input governing predicate bit is undefined
;;   - the SI elements being compared are undefined
;;   - the predicate result bit is therefore undefined, but
;;   - the predicate result bit is in the undefined part of a VNx2BI,
;;     so its value doesn't matter anyway.
(define_insn "@aarch64_pred_cmp<cmp_op><mode>"
  [(set (match_operand:<VPRED> 0 "register_operand")
	(unspec:<VPRED>
	  [(match_operand:<VPRED> 1 "register_operand")
	   (match_operand:SI 2 "aarch64_sve_ptrue_flag")
	   (SVE_INT_CMP:<VPRED>
	     (match_operand:SVE_I 3 "register_operand")
	     (match_operand:SVE_I 4 "aarch64_sve_cmp_<sve_imm_con>_operand"))]
	  UNSPEC_PRED_Z))
   (clobber (reg:CC_NZC CC_REGNUM))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1  , 3 , 4            ; attrs: pred_clobber ]
     [ &Upa     , Upl, w , <sve_imm_con>; yes                 ] cmp<cmp_op>\t%0.<Vetype>, %1/z, %3.<Vetype>, #%4
     [ ?Upl     , 0  , w , <sve_imm_con>; yes                 ] ^
     [ Upa      , Upl, w , <sve_imm_con>; no                  ] ^
     [ &Upa     , Upl, w , w            ; yes                 ] cmp<cmp_op>\t%0.<Vetype>, %1/z, %3.<Vetype>, %4.<Vetype>
     [ ?Upl     , 0  , w , w            ; yes                 ] ^
     [ Upa      , Upl, w , w            ; no                  ] ^
  }
)

;; Predicated integer comparisons in which both the flag and predicate
;; results are interesting.
(define_insn_and_rewrite "*cmp<cmp_op><mode>_cc"
  [(set (reg:CC_NZC CC_REGNUM)
	(unspec:CC_NZC
	  [(match_operand:VNx16BI 1 "register_operand")
	   (match_operand 4)
	   (match_operand:SI 5 "aarch64_sve_ptrue_flag")
	   (unspec:<VPRED>
	     [(match_operand 6)
	      (match_operand:SI 7 "aarch64_sve_ptrue_flag")
	      (SVE_INT_CMP:<VPRED>
		(match_operand:SVE_I 2 "register_operand")
		(match_operand:SVE_I 3 "aarch64_sve_cmp_<sve_imm_con>_operand"))]
	     UNSPEC_PRED_Z)]
	  UNSPEC_PTEST))
   (set (match_operand:<VPRED> 0 "register_operand")
	(unspec:<VPRED>
	  [(match_dup 6)
	   (match_dup 7)
	   (SVE_INT_CMP:<VPRED>
	     (match_dup 2)
	     (match_dup 3))]
	  UNSPEC_PRED_Z))]
  "TARGET_SVE
   && aarch64_sve_same_pred_for_ptest_p (&operands[4], &operands[6])"
  {@ [ cons: =0 , 1   , 2 , 3            ; attrs: pred_clobber ]
     [ &Upa     ,  Upl, w , <sve_imm_con>; yes                 ] cmp<cmp_op>\t%0.<Vetype>, %1/z, %2.<Vetype>, #%3
     [ ?Upl     ,  0  , w , <sve_imm_con>; yes                 ] ^
     [ Upa      ,  Upl, w , <sve_imm_con>; no                  ] ^
     [ &Upa     ,  Upl, w , w            ; yes                 ] cmp<cmp_op>\t%0.<Vetype>, %1/z, %2.<Vetype>, %3.<Vetype>
     [ ?Upl     ,  0  , w , w            ; yes                 ] ^
     [ Upa      ,  Upl, w , w            ; no                  ] ^
  }
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
	  [(match_operand:VNx16BI 1 "register_operand")
	   (match_operand 4)
	   (match_operand:SI 5 "aarch64_sve_ptrue_flag")
	   (unspec:<VPRED>
	     [(match_operand 6)
	      (match_operand:SI 7 "aarch64_sve_ptrue_flag")
	      (SVE_INT_CMP:<VPRED>
		(match_operand:SVE_I 2 "register_operand")
		(match_operand:SVE_I 3 "aarch64_sve_cmp_<sve_imm_con>_operand"))]
	     UNSPEC_PRED_Z)]
	  UNSPEC_PTEST))
   (clobber (match_scratch:<VPRED> 0))]
  "TARGET_SVE
   && aarch64_sve_same_pred_for_ptest_p (&operands[4], &operands[6])"
  {@ [ cons: =0, 1    , 2 , 3            ; attrs: pred_clobber ]
     [ &Upa    ,  Upl, w , <sve_imm_con>; yes                 ] cmp<cmp_op>\t%0.<Vetype>, %1/z, %2.<Vetype>, #%3
     [ ?Upl    ,  0  , w , <sve_imm_con>; yes                 ] ^
     [ Upa     ,  Upl, w , <sve_imm_con>; no                  ] ^
     [ &Upa    ,  Upl, w , w            ; yes                 ] cmp<cmp_op>\t%0.<Vetype>, %1/z, %2.<Vetype>, %3.<Vetype>
     [ ?Upl    ,  0  , w , w            ; yes                 ] ^
     [ Upa     ,  Upl, w , w            ; no                  ] ^
  }
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

;; Predicated integer wide comparisons.
(define_insn "@aarch64_pred_cmp<cmp_op><mode>_wide"
  [(set (match_operand:<VPRED> 0 "register_operand")
	(unspec:<VPRED>
	  [(match_operand:VNx16BI 1 "register_operand")
	   (match_operand:SI 2 "aarch64_sve_ptrue_flag")
	   (unspec:<VPRED>
	     [(match_operand:SVE_FULL_BHSI 3 "register_operand")
	      (match_operand:VNx2DI 4 "register_operand")]
	     SVE_COND_INT_CMP_WIDE)]
	  UNSPEC_PRED_Z))
   (clobber (reg:CC_NZC CC_REGNUM))]
  "TARGET_SVE"
  {@ [ cons: =0, 1   , 2, 3, 4; attrs: pred_clobber ]
     [ &Upa    ,  Upl,  , w, w; yes                 ] cmp<cmp_op>\t%0.<Vetype>, %1/z, %3.<Vetype>, %4.d
     [ ?Upl    ,  0  ,  , w, w; yes                 ] ^
     [ Upa     ,  Upl,  , w, w; no                  ] ^
  }
)

;; Predicated integer wide comparisons in which both the flag and
;; predicate results are interesting.
(define_insn "*aarch64_pred_cmp<cmp_op><mode>_wide_cc"
  [(set (reg:CC_NZC CC_REGNUM)
	(unspec:CC_NZC
	  [(match_operand:VNx16BI 1 "register_operand")
	   (match_operand 4)
	   (match_operand:SI 5 "aarch64_sve_ptrue_flag")
	   (unspec:<VPRED>
	     [(match_operand:VNx16BI 6 "register_operand")
	      (match_operand:SI 7 "aarch64_sve_ptrue_flag")
	      (unspec:<VPRED>
		[(match_operand:SVE_FULL_BHSI 2 "register_operand")
		 (match_operand:VNx2DI 3 "register_operand")]
		SVE_COND_INT_CMP_WIDE)]
	     UNSPEC_PRED_Z)]
	  UNSPEC_PTEST))
   (set (match_operand:<VPRED> 0 "register_operand")
	(unspec:<VPRED>
	  [(match_dup 6)
	   (match_dup 7)
	   (unspec:<VPRED>
	     [(match_dup 2)
	      (match_dup 3)]
	     SVE_COND_INT_CMP_WIDE)]
	  UNSPEC_PRED_Z))]
  "TARGET_SVE
   && aarch64_sve_same_pred_for_ptest_p (&operands[4], &operands[6])"
  {@ [ cons: =0, 1   , 2, 3, 6  ; attrs: pred_clobber ]
     [ &Upa    ,  Upl, w, w, Upl; yes                 ] cmp<cmp_op>\t%0.<Vetype>, %1/z, %2.<Vetype>, %3.d
     [ ?Upl    ,  0  , w, w, Upl; yes                 ] ^
     [ Upa     ,  Upl, w, w, Upl; no                  ] ^
  }
)

;; Predicated integer wide comparisons in which only the flags result
;; is interesting.
(define_insn "*aarch64_pred_cmp<cmp_op><mode>_wide_ptest"
  [(set (reg:CC_NZC CC_REGNUM)
	(unspec:CC_NZC
	  [(match_operand:VNx16BI 1 "register_operand")
	   (match_operand 4)
	   (match_operand:SI 5 "aarch64_sve_ptrue_flag")
	   (unspec:<VPRED>
	     [(match_operand:VNx16BI 6 "register_operand")
	      (match_operand:SI 7 "aarch64_sve_ptrue_flag")
	      (unspec:<VPRED>
		[(match_operand:SVE_FULL_BHSI 2 "register_operand")
		 (match_operand:VNx2DI 3 "register_operand")]
		SVE_COND_INT_CMP_WIDE)]
	     UNSPEC_PRED_Z)]
	  UNSPEC_PTEST))
   (clobber (match_scratch:<VPRED> 0))]
  "TARGET_SVE
   && aarch64_sve_same_pred_for_ptest_p (&operands[4], &operands[6])"
  {@ [ cons:  =0, 1   , 2, 3, 6  ; attrs: pred_clobber ]
     [ &Upa     ,  Upl, w, w, Upl; yes                 ] cmp<cmp_op>\t%0.<Vetype>, %1/z, %2.<Vetype>, %3.d
     [ ?Upl     ,  0  , w, w, Upl; yes                 ] ^
     [ Upa      ,  Upl, w, w, Upl; no                  ] ^
  }
)

;; -------------------------------------------------------------------------
;; ---- [INT] While tests
;; -------------------------------------------------------------------------
;; Includes:
;; - WHILEGE (SVE2)
;; - WHILEGT (SVE2)
;; - WHILEHI (SVE2)
;; - WHILEHS (SVE2)
;; - WHILELE
;; - WHILELO
;; - WHILELS
;; - WHILELT
;; - WHILERW (SVE2)
;; - WHILEWR (SVE2)
;; -------------------------------------------------------------------------

(define_constants [
  (SVE_WHILE_B 0)
  (SVE_WHILE_B_X2 1)
  (SVE_WHILE_C 2)
])

;; Set element I of the result if (cmp (plus operand1 J) operand2) is
;; true for all J in [0, I].
(define_insn "@while_<while_optab_cmp><GPI:mode><PRED_ALL:mode>"
  [(set (match_operand:PRED_ALL 0 "register_operand" "=Upa")
	(unspec:PRED_ALL [(const_int SVE_WHILE_B)
			  (match_operand:GPI 1 "aarch64_reg_or_zero" "rZ")
			  (match_operand:GPI 2 "aarch64_reg_or_zero" "rZ")]
			 SVE_WHILE))
   (clobber (reg:CC_NZC CC_REGNUM))]
  "TARGET_SVE"
  "while<cmp_op>\t%0.<PRED_ALL:Vetype>, %<w>1, %<w>2"
)

;; The WHILE instructions set the flags in the same way as a PTEST with
;; a PTRUE GP.  Handle the case in which both results are useful.  The GP
;; operands to the PTEST aren't needed, so we allow them to be anything.
(define_insn_and_rewrite "*while_<while_optab_cmp><GPI:mode><PRED_ALL:mode>_cc"
  [(set (reg:CC_NZC CC_REGNUM)
	(unspec:CC_NZC
	  [(match_operand 3)
	   (match_operand 4)
	   (const_int SVE_KNOWN_PTRUE)
	   (unspec:PRED_ALL
	     [(const_int SVE_WHILE_B)
	      (match_operand:GPI 1 "aarch64_reg_or_zero" "rZ")
	      (match_operand:GPI 2 "aarch64_reg_or_zero" "rZ")]
	     SVE_WHILE)]
	  UNSPEC_PTEST))
   (set (match_operand:PRED_ALL 0 "register_operand" "=Upa")
	(unspec:PRED_ALL [(const_int SVE_WHILE_B)
			  (match_dup 1)
			  (match_dup 2)]
			 SVE_WHILE))]
  "TARGET_SVE"
  "while<cmp_op>\t%0.<PRED_ALL:Vetype>, %<w>1, %<w>2"
  ;; Force the compiler to drop the unused predicate operand, so that we
  ;; don't have an unnecessary PTRUE.
  "&& (!CONSTANT_P (operands[3]) || !CONSTANT_P (operands[4]))"
  {
    operands[3] = CONSTM1_RTX (VNx16BImode);
    operands[4] = CONSTM1_RTX (<PRED_ALL:MODE>mode);
  }
)

;; Same, but handle the case in which only the flags result is useful.
(define_insn_and_rewrite "@while_<while_optab_cmp><GPI:mode><PRED_ALL:mode>_ptest"
  [(set (reg:CC_NZC CC_REGNUM)
	(unspec:CC_NZC
	  [(match_operand 3)
	   (match_operand 4)
	   (const_int SVE_KNOWN_PTRUE)
	   (unspec:PRED_ALL
	     [(const_int SVE_WHILE_B)
	      (match_operand:GPI 1 "aarch64_reg_or_zero" "rZ")
	      (match_operand:GPI 2 "aarch64_reg_or_zero" "rZ")]
	     SVE_WHILE)]
	  UNSPEC_PTEST))
   (clobber (match_scratch:PRED_ALL 0 "=Upa"))]
  "TARGET_SVE"
  "while<cmp_op>\t%0.<PRED_ALL:Vetype>, %<w>1, %<w>2"
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
	  [(match_operand:SVE_FULL_F 2 "register_operand")
	   (match_operand:SVE_FULL_F 3 "aarch64_simd_reg_or_zero")]))]
  "TARGET_SVE"
  {
    aarch64_expand_sve_vec_cmp_float (operands[0], GET_CODE (operands[1]),
				      operands[2], operands[3], false);
    DONE;
  }
)

;; Predicated floating-point comparisons.
(define_insn "@aarch64_pred_fcm<cmp_op><mode>"
  [(set (match_operand:<VPRED> 0 "register_operand")
	(unspec:<VPRED>
	  [(match_operand:<VPRED> 1 "register_operand")
	   (match_operand:SI 2 "aarch64_sve_ptrue_flag")
	   (match_operand:SVE_FULL_F 3 "register_operand")
	   (match_operand:SVE_FULL_F 4 "aarch64_simd_reg_or_zero")]
	  SVE_COND_FP_CMP_I0))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 3 , 4   ]
     [ Upa      , Upl , w , Dz  ] fcm<cmp_op>\t%0.<Vetype>, %1/z, %3.<Vetype>, #0.0
     [ Upa      , Upl , w , w   ] fcm<cmp_op>\t%0.<Vetype>, %1/z, %3.<Vetype>, %4.<Vetype>
  }
)

;; Same for unordered comparisons.
(define_insn "@aarch64_pred_fcmuo<mode>"
  [(set (match_operand:<VPRED> 0 "register_operand" "=Upa")
	(unspec:<VPRED>
	  [(match_operand:<VPRED> 1 "register_operand" "Upl")
	   (match_operand:SI 2 "aarch64_sve_ptrue_flag")
	   (match_operand:SVE_FULL_F 3 "register_operand" "w")
	   (match_operand:SVE_FULL_F 4 "register_operand" "w")]
	  UNSPEC_COND_FCMUO))]
  "TARGET_SVE"
  "fcmuo\t%0.<Vetype>, %1/z, %3.<Vetype>, %4.<Vetype>"
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
	     (match_operand:SVE_FULL_F 2 "register_operand" "w, w")
	     (match_operand:SVE_FULL_F 3 "aarch64_simd_reg_or_zero" "Dz, w")]
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
	     (match_operand:SVE_FULL_F 2 "register_operand" "w")
	     (match_operand:SVE_FULL_F 3 "register_operand" "w")]
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

;; Similar to *fcm<cmp_op><mode>_and_combine, but for BIC rather than AND.
;; In this case, we still need a separate NOT/BIC operation, but predicating
;; the comparison on the BIC operand removes the need for a PTRUE.
(define_insn_and_split "*fcm<cmp_op><mode>_bic_combine"
  [(set (match_operand:<VPRED> 0 "register_operand" "=Upa")
	(and:<VPRED>
	  (and:<VPRED>
	    (not:<VPRED>
	      (unspec:<VPRED>
	        [(match_operand:<VPRED> 1)
	         (const_int SVE_KNOWN_PTRUE)
	         (match_operand:SVE_FULL_F 2 "register_operand" "w")
	         (match_operand:SVE_FULL_F 3 "aarch64_simd_reg_or_zero" "wDz")]
	        SVE_COND_FP_CMP_I0))
	    (match_operand:<VPRED> 4 "register_operand" "Upa"))
	  (match_dup:<VPRED> 1)))
   (clobber (match_scratch:<VPRED> 5 "=&Upl"))]
  "TARGET_SVE"
  "#"
  "&& 1"
  [(set (match_dup 5)
	(unspec:<VPRED>
	  [(match_dup 4)
	   (const_int SVE_MAYBE_NOT_PTRUE)
	   (match_dup 2)
	   (match_dup 3)]
	  SVE_COND_FP_CMP_I0))
   (set (match_dup 0)
	(and:<VPRED>
	  (not:<VPRED>
	    (match_dup 5))
	  (match_dup 4)))]
{
  if (can_create_pseudo_p ())
    operands[5] = gen_reg_rtx (<VPRED>mode);
}
)

;; Make sure that we expand to a nor when the operand 4 of
;; *fcm<cmp_op><mode>_bic_combine is a not.
(define_insn_and_split "*fcm<cmp_op><mode>_nor_combine"
  [(set (match_operand:<VPRED> 0 "register_operand" "=Upa")
	(and:<VPRED>
	  (and:<VPRED>
	    (not:<VPRED>
	      (unspec:<VPRED>
	        [(match_operand:<VPRED> 1)
	         (const_int SVE_KNOWN_PTRUE)
	         (match_operand:SVE_FULL_F 2 "register_operand" "w")
	         (match_operand:SVE_FULL_F 3 "aarch64_simd_reg_or_zero" "wDz")]
	        SVE_COND_FP_CMP_I0))
	    (not:<VPRED>
	      (match_operand:<VPRED> 4 "register_operand" "Upa")))
	  (match_dup:<VPRED> 1)))
   (clobber (match_scratch:<VPRED> 5 "=&Upl"))]
  "TARGET_SVE"
  "#"
  "&& 1"
  [(set (match_dup 5)
	(unspec:<VPRED>
	  [(match_dup 1)
	   (const_int SVE_KNOWN_PTRUE)
	   (match_dup 2)
	   (match_dup 3)]
	  SVE_COND_FP_CMP_I0))
   (set (match_dup 0)
	(and:<VPRED>
	  (and:<VPRED>
	    (not:<VPRED>
	      (match_dup 5))
	    (not:<VPRED>
	      (match_dup 4)))
	  (match_dup 1)))]
{
  if (can_create_pseudo_p ())
    operands[5] = gen_reg_rtx (<VPRED>mode);
}
)

(define_insn_and_split "*fcmuo<mode>_bic_combine"
  [(set (match_operand:<VPRED> 0 "register_operand" "=Upa")
	(and:<VPRED>
	  (and:<VPRED>
	    (not:<VPRED>
	      (unspec:<VPRED>
	        [(match_operand:<VPRED> 1)
	         (const_int SVE_KNOWN_PTRUE)
	         (match_operand:SVE_FULL_F 2 "register_operand" "w")
	         (match_operand:SVE_FULL_F 3 "register_operand" "w")]
	        UNSPEC_COND_FCMUO))
	    (match_operand:<VPRED> 4 "register_operand" "Upa"))
	  (match_dup:<VPRED> 1)))
   (clobber (match_scratch:<VPRED> 5 "=&Upl"))]
  "TARGET_SVE"
  "#"
  "&& 1"
  [(set (match_dup 5)
	(unspec:<VPRED>
	  [(match_dup 4)
	   (const_int SVE_MAYBE_NOT_PTRUE)
	   (match_dup 2)
	   (match_dup 3)]
	  UNSPEC_COND_FCMUO))
   (set (match_dup 0)
	(and:<VPRED>
	  (not:<VPRED>
	    (match_dup 5))
	  (match_dup 4)))]
{
  if (can_create_pseudo_p ())
    operands[5] = gen_reg_rtx (<VPRED>mode);
}
)

;; Same for unordered comparisons.
(define_insn_and_split "*fcmuo<mode>_nor_combine"
  [(set (match_operand:<VPRED> 0 "register_operand" "=Upa")
	(and:<VPRED>
	  (and:<VPRED>
	    (not:<VPRED>
	      (unspec:<VPRED>
	        [(match_operand:<VPRED> 1)
	         (const_int SVE_KNOWN_PTRUE)
	         (match_operand:SVE_FULL_F 2 "register_operand" "w")
	         (match_operand:SVE_FULL_F 3 "register_operand" "w")]
	        UNSPEC_COND_FCMUO))
	    (not:<VPRED>
	      (match_operand:<VPRED> 4 "register_operand" "Upa")))
	  (match_dup:<VPRED> 1)))
   (clobber (match_scratch:<VPRED> 5 "=&Upl"))]
  "TARGET_SVE"
  "#"
  "&& 1"
  [(set (match_dup 5)
	(unspec:<VPRED>
	  [(match_dup 1)
	   (const_int SVE_KNOWN_PTRUE)
	   (match_dup 2)
	   (match_dup 3)]
	  UNSPEC_COND_FCMUO))
   (set (match_dup 0)
	(and:<VPRED>
	  (and:<VPRED>
	    (not:<VPRED>
	      (match_dup 5))
	    (not:<VPRED>
	      (match_dup 4)))
	  (match_dup 1)))]
{
  if (can_create_pseudo_p ())
    operands[5] = gen_reg_rtx (<VPRED>mode);
}
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
(define_expand "@aarch64_pred_fac<cmp_op><mode>"
  [(set (match_operand:<VPRED> 0 "register_operand")
	(unspec:<VPRED>
	  [(match_operand:<VPRED> 1 "register_operand")
	   (match_operand:SI 2 "aarch64_sve_ptrue_flag")
	   (unspec:SVE_FULL_F
	     [(match_dup 1)
	      (match_dup 2)
	      (match_operand:SVE_FULL_F 3 "register_operand")]
	     UNSPEC_COND_FABS)
	   (unspec:SVE_FULL_F
	     [(match_dup 1)
	      (match_dup 2)
	      (match_operand:SVE_FULL_F 4 "register_operand")]
	     UNSPEC_COND_FABS)]
	  SVE_COND_FP_ABS_CMP))]
  "TARGET_SVE"
)

(define_insn_and_rewrite "*aarch64_pred_fac<cmp_op><mode>_relaxed"
  [(set (match_operand:<VPRED> 0 "register_operand" "=Upa")
	(unspec:<VPRED>
	  [(match_operand:<VPRED> 1 "register_operand" "Upl")
	   (match_operand:SI 4 "aarch64_sve_ptrue_flag")
	   (unspec:SVE_FULL_F
	     [(match_operand 5)
	      (const_int SVE_RELAXED_GP)
	      (match_operand:SVE_FULL_F 2 "register_operand" "w")]
	     UNSPEC_COND_FABS)
	   (unspec:SVE_FULL_F
	     [(match_operand 6)
	      (const_int SVE_RELAXED_GP)
	      (match_operand:SVE_FULL_F 3 "register_operand" "w")]
	     UNSPEC_COND_FABS)]
	  SVE_COND_FP_ABS_CMP))]
  "TARGET_SVE"
  "fac<cmp_op>\t%0.<Vetype>, %1/z, %2.<Vetype>, %3.<Vetype>"
  "&& (!rtx_equal_p (operands[1], operands[5])
       || !rtx_equal_p (operands[1], operands[6]))"
  {
    operands[5] = copy_rtx (operands[1]);
    operands[6] = copy_rtx (operands[1]);
  }
)

(define_insn "*aarch64_pred_fac<cmp_op><mode>_strict"
  [(set (match_operand:<VPRED> 0 "register_operand" "=Upa")
	(unspec:<VPRED>
	  [(match_operand:<VPRED> 1 "register_operand" "Upl")
	   (match_operand:SI 4 "aarch64_sve_ptrue_flag")
	   (unspec:SVE_FULL_F
	     [(match_dup 1)
	      (match_operand:SI 5 "aarch64_sve_gp_strictness")
	      (match_operand:SVE_FULL_F 2 "register_operand" "w")]
	     UNSPEC_COND_FABS)
	   (unspec:SVE_FULL_F
	     [(match_dup 1)
	      (match_operand:SI 6 "aarch64_sve_gp_strictness")
	      (match_operand:SVE_FULL_F 3 "register_operand" "w")]
	     UNSPEC_COND_FABS)]
	  SVE_COND_FP_ABS_CMP))]
  "TARGET_SVE"
  "fac<cmp_op>\t%0.<Vetype>, %1/z, %2.<Vetype>, %3.<Vetype>"
)

;; -------------------------------------------------------------------------
;; ---- [PRED] Select
;; -------------------------------------------------------------------------
;; Includes:
;; - SEL
;; -------------------------------------------------------------------------

(define_insn "@vcond_mask_<mode><mode>"
  [(set (match_operand:PRED_ALL 0 "register_operand" "=Upa")
	(ior:PRED_ALL
	  (and:PRED_ALL
	    (match_operand:PRED_ALL 3 "register_operand" "Upa")
	    (match_operand:PRED_ALL 1 "register_operand" "Upa"))
	  (and:PRED_ALL
	    (not (match_dup 3))
	    (match_operand:PRED_ALL 2 "register_operand" "Upa"))))]
  "TARGET_SVE"
  "sel\t%0.b, %3, %1.b, %2.b"
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
;; - CLASTA
;; - CLASTB
;; -------------------------------------------------------------------------

;; Set operand 0 to the last active element in operand 3, or to tied
;; operand 1 if no elements are active.
(define_insn "@fold_extract_<last_op>_<mode>"
  [(set (match_operand:<VEL> 0 "register_operand")
	(unspec:<VEL>
	  [(match_operand:<VEL> 1 "register_operand")
	   (match_operand:<VPRED> 2 "register_operand")
	   (match_operand:SVE_FULL 3 "register_operand")]
	  CLAST))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1 , 2   , 3  ]
     [ ?r       , 0 , Upl , w  ] clast<ab>\t%<vwcore>0, %2, %<vwcore>0, %3.<Vetype>
     [ w        , 0 , Upl , w  ] clast<ab>\t%<Vetype>0, %2, %<Vetype>0, %3.<Vetype>
  }
)

(define_insn "@aarch64_fold_extract_vector_<last_op>_<mode>"
  [(set (match_operand:SVE_FULL 0 "register_operand")
	(unspec:SVE_FULL
	  [(match_operand:SVE_FULL 1 "register_operand")
	   (match_operand:<VPRED> 2 "register_operand")
	   (match_operand:SVE_FULL 3 "register_operand")]
	  CLAST))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1 , 2   , 3  ]
     [ w        , 0 , Upl , w  ] clast<ab>\t%0.<Vetype>, %2, %0.<Vetype>, %3.<Vetype>
     [ ?&w      , w , Upl , w  ] movprfx\t%0, %1\;clast<ab>\t%0.<Vetype>, %2, %0.<Vetype>, %3.<Vetype>
  }
)

;; -------------------------------------------------------------------------
;; ---- [INT] Tree reductions
;; -------------------------------------------------------------------------
;; Includes:
;; - ANDV
;; - EORV
;; - ORV
;; - SADDV
;; - SMAXV
;; - SMINV
;; - UADDV
;; - UMAXV
;; - UMINV
;; -------------------------------------------------------------------------

;; Unpredicated integer add reduction.
(define_expand "reduc_plus_scal_<mode>"
  [(match_operand:<VEL> 0 "register_operand")
   (match_operand:SVE_FULL_I 1 "register_operand")]
  "TARGET_SVE"
  {
    rtx pred = aarch64_ptrue_reg (<VPRED>mode);
    rtx tmp = <VEL>mode == DImode ? operands[0] : gen_reg_rtx (DImode);
    emit_insn (gen_aarch64_pred_reduc_uadd_<mode> (tmp, pred, operands[1]));
    if (tmp != operands[0])
      emit_move_insn (operands[0], gen_lowpart (<VEL>mode, tmp));
    DONE;
  }
)

;; Predicated integer add reduction.  The result is always 64-bits.
(define_insn "@aarch64_pred_reduc_<optab>_<mode>"
  [(set (match_operand:DI 0 "register_operand" "=w")
	(unspec:DI [(match_operand:<VPRED> 1 "register_operand" "Upl")
		    (match_operand:SVE_FULL_I 2 "register_operand" "w")]
		   SVE_INT_ADDV))]
  "TARGET_SVE && <max_elem_bits> >= <elem_bits>"
  "<su>addv\t%d0, %1, %2.<Vetype>"
)

;; Unpredicated integer reductions.
(define_expand "reduc_<optab>_scal_<mode>"
  [(set (match_operand:<VEL> 0 "register_operand")
	(unspec:<VEL> [(match_dup 2)
		       (match_operand:SVE_FULL_I 1 "register_operand")]
		      SVE_INT_REDUCTION))]
  "TARGET_SVE"
  {
    operands[2] = aarch64_ptrue_reg (<VPRED>mode);
  }
)

;; Predicated integer reductions.
(define_insn "@aarch64_pred_reduc_<optab>_<mode>"
  [(set (match_operand:<VEL> 0 "register_operand" "=w")
	(unspec:<VEL> [(match_operand:<VPRED> 1 "register_operand" "Upl")
		       (match_operand:SVE_FULL_I 2 "register_operand" "w")]
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
		       (match_operand:SVE_FULL_F 1 "register_operand")]
		      SVE_FP_REDUCTION))]
  "TARGET_SVE"
  {
    operands[2] = aarch64_ptrue_reg (<VPRED>mode);
  }
)

(define_expand "reduc_<fmaxmin>_scal_<mode>"
  [(match_operand:<VEL> 0 "register_operand")
   (unspec:<VEL> [(match_operand:SVE_FULL_F 1 "register_operand")]
		 FMAXMINNMV)]
  "TARGET_SVE"
  {
    emit_insn (gen_reduc_<optab>_scal_<mode> (operands[0], operands[1]));
    DONE;
  }
)

;; Predicated floating-point tree reductions.
(define_insn "@aarch64_pred_reduc_<optab>_<mode>"
  [(set (match_operand:<VEL> 0 "register_operand" "=w")
	(unspec:<VEL> [(match_operand:<VPRED> 1 "register_operand" "Upl")
		       (match_operand:SVE_FULL_F 2 "register_operand" "w")]
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
		       (match_operand:SVE_FULL_F 2 "register_operand")]
		      UNSPEC_FADDA))]
  "TARGET_SVE && TARGET_NON_STREAMING"
  {
    operands[3] = aarch64_ptrue_reg (<VPRED>mode);
  }
)

;; Predicated in-order FP reductions.
(define_insn "mask_fold_left_plus_<mode>"
  [(set (match_operand:<VEL> 0 "register_operand" "=w")
	(unspec:<VEL> [(match_operand:<VPRED> 3 "register_operand" "Upl")
		       (match_operand:<VEL> 1 "register_operand" "0")
		       (match_operand:SVE_FULL_F 2 "register_operand" "w")]
		      UNSPEC_FADDA))]
  "TARGET_SVE && TARGET_NON_STREAMING"
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
;; - TBLQ (SVE2p1)
;; -------------------------------------------------------------------------

(define_expand "vec_perm<mode>"
  [(match_operand:SVE_FULL 0 "register_operand")
   (match_operand:SVE_FULL 1 "register_operand")
   (match_operand:SVE_FULL 2 "register_operand")
   (match_operand:<V_INT_EQUIV> 3 "aarch64_sve_vec_perm_operand")]
  "TARGET_SVE && GET_MODE_NUNITS (<MODE>mode).is_constant ()"
  {
    aarch64_expand_sve_vec_perm (operands[0], operands[1],
				 operands[2], operands[3]);
    DONE;
  }
)

(define_insn "@aarch64_sve_<perm_insn><mode>"
  [(set (match_operand:SVE_FULL 0 "register_operand" "=w")
	(unspec:SVE_FULL
	  [(match_operand:SVE_FULL 1 "register_operand" "w")
	   (match_operand:<V_INT_EQUIV> 2 "register_operand" "w")]
	  SVE_TBL))]
  "TARGET_SVE"
  "<perm_insn>\t%0.<Vetype>, {%1.<Vetype>}, %2.<Vetype>"
)

;; -------------------------------------------------------------------------
;; ---- [INT,FP] Special-purpose unary permutes
;; -------------------------------------------------------------------------
;; Includes:
;; - COMPACT
;; - DUP
;; - REV
;; -------------------------------------------------------------------------

;; Compact active elements and pad with zeros.
(define_insn "@aarch64_sve_compact<mode>"
  [(set (match_operand:SVE_FULL_SD 0 "register_operand" "=w")
	(unspec:SVE_FULL_SD
	  [(match_operand:<VPRED> 1 "register_operand" "Upl")
	   (match_operand:SVE_FULL_SD 2 "register_operand" "w")]
	  UNSPEC_SVE_COMPACT))]
  "TARGET_SVE && TARGET_NON_STREAMING"
  "compact\t%0.<Vetype>, %1, %2.<Vetype>"
)

;; Duplicate one element of a vector.
(define_insn "@aarch64_sve_dup_lane<mode>"
  [(set (match_operand:SVE_ALL 0 "register_operand" "=w")
	(vec_duplicate:SVE_ALL
	  (vec_select:<VEL>
	    (match_operand:SVE_ALL 1 "register_operand" "w")
	    (parallel [(match_operand:SI 2 "const_int_operand")]))))]
  "TARGET_SVE
   && IN_RANGE (INTVAL (operands[2]) * <container_bits> / 8, 0, 63)"
  "dup\t%0.<Vctype>, %1.<Vctype>[%2]"
)

;; Use DUP.Q to duplicate a 128-bit segment of a register.
;;
;; The vec_select:<V128> sets memory lane number N of the V128 to lane
;; number op2 + N of op1.  (We don't need to distinguish between memory
;; and architectural register lane numbering for op1 or op0, since the
;; two numbering schemes are the same for SVE.)
;;
;; The vec_duplicate:SVE_FULL then copies memory lane number N of the
;; V128 (and thus lane number op2 + N of op1) to lane numbers N + I * STEP
;; of op0.  We therefore get the correct result for both endiannesses.
;;
;; The wrinkle is that for big-endian V128 registers, memory lane numbering
;; is in the opposite order to architectural register lane numbering.
;; Thus if we were to do this operation via a V128 temporary register,
;; the vec_select and vec_duplicate would both involve a reverse operation
;; for big-endian targets.  In this fused pattern the two reverses cancel
;; each other out.
(define_insn "@aarch64_sve_dupq_lane<mode>"
  [(set (match_operand:SVE_FULL 0 "register_operand" "=w")
	(vec_duplicate:SVE_FULL
	  (vec_select:<V128>
	    (match_operand:SVE_FULL 1 "register_operand" "w")
	    (match_operand 2 "ascending_int_parallel"))))]
  "TARGET_SVE
   && (INTVAL (XVECEXP (operands[2], 0, 0))
       * GET_MODE_SIZE (<VEL>mode)) % 16 == 0
   && IN_RANGE (INTVAL (XVECEXP (operands[2], 0, 0))
		* GET_MODE_SIZE (<VEL>mode), 0, 63)"
  {
    unsigned int byte = (INTVAL (XVECEXP (operands[2], 0, 0))
			 * GET_MODE_SIZE (<VEL>mode));
    operands[2] = gen_int_mode (byte / 16, DImode);
    return "dup\t%0.q, %1.q[%2]";
  }
)

;; Reverse the order of elements within a full vector.
(define_insn "@aarch64_sve_rev<mode>"
  [(set (match_operand:SVE_ALL 0 "register_operand" "=w")
	(unspec:SVE_ALL
	  [(match_operand:SVE_ALL 1 "register_operand" "w")]
	  UNSPEC_REV))]
  "TARGET_SVE"
  "rev\t%0.<Vctype>, %1.<Vctype>")

;; -------------------------------------------------------------------------
;; ---- [INT,FP] Special-purpose binary permutes
;; -------------------------------------------------------------------------
;; Includes:
;; - EXT
;; - SPLICE
;; - TRN1
;; - TRN2
;; - UZP1
;; - UZPQ1 (SVE2p1)
;; - UZP2
;; - UZPQ2 (SVE2p1)
;; - ZIP1
;; - ZIPQ1 (SVE2p1)
;; - ZIP2
;; - ZIPQ2 (SVE2p1)
;; -------------------------------------------------------------------------

;; Like EXT, but start at the first active element.
(define_insn "@aarch64_sve_splice<mode>"
  [(set (match_operand:SVE_FULL 0 "register_operand")
	(unspec:SVE_FULL
	  [(match_operand:<VPRED> 1 "register_operand")
	   (match_operand:SVE_FULL 2 "register_operand")
	   (match_operand:SVE_FULL 3 "register_operand")]
	  UNSPEC_SVE_SPLICE))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2 , 3 ; attrs: movprfx ]
     [ w        , Upl , 0 , w ; *              ] splice\t%0.<Vetype>, %1, %0.<Vetype>, %3.<Vetype>
     [ ?&w      , Upl , w , w ; yes            ] movprfx\t%0, %2\;splice\t%0.<Vetype>, %1, %0.<Vetype>, %3.<Vetype>
  }
)

;; Permutes that take half the elements from one vector and half the
;; elements from the other.
(define_insn "@aarch64_sve_<perm_insn><mode>"
  [(set (match_operand:SVE_ALL 0 "register_operand" "=w")
	(unspec:SVE_ALL
	  [(match_operand:SVE_ALL 1 "register_operand" "w")
	   (match_operand:SVE_ALL 2 "register_operand" "w")]
	  SVE_PERMUTE))]
  "TARGET_SVE"
  "<perm_insn>\t%0.<Vctype>, %1.<Vctype>, %2.<Vctype>"
)

;; Apply PERMUTE to 128-bit sequences.  The behavior of these patterns
;; doesn't depend on the mode.
(define_insn "@aarch64_sve_<optab><mode>"
  [(set (match_operand:SVE_FULL 0 "register_operand" "=w")
	(unspec:SVE_FULL
	  [(match_operand:SVE_FULL 1 "register_operand" "w")
	   (match_operand:SVE_FULL 2 "register_operand" "w")]
	  PERMUTEQ))]
  "TARGET_SVE_F64MM"
  "<perm_insn>\t%0.q, %1.q, %2.q"
)

;; Concatenate two vectors and extract a subvector.  Note that the
;; immediate (third) operand is the lane index not the byte index.
(define_insn "@aarch64_sve_ext<mode>"
  [(set (match_operand:SVE_ALL 0 "register_operand" "=w, ?&w")
	(unspec:SVE_ALL
	  [(match_operand:SVE_ALL 1 "register_operand" "0, w")
	   (match_operand:SVE_ALL 2 "register_operand" "w, w")
	   (match_operand:SI 3 "const_int_operand")]
	  UNSPEC_EXT))]
  "TARGET_SVE
   && IN_RANGE (INTVAL (operands[3]) * <container_bits> / 8, 0, 255)"
  {
    operands[3] = GEN_INT (INTVAL (operands[3]) * <container_bits> / 8);
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

;; Special purpose permute used by the predicate generation instructions.
;; Unlike the normal permute patterns, these instructions operate on VNx16BI
;; regardless of the element size, so that all input and output bits are
;; well-defined.  Operand 3 then indicates the size of the permute.
(define_insn "@aarch64_sve_trn1_conv<mode>"
  [(set (match_operand:VNx16BI 0 "register_operand" "=Upa")
	(unspec:VNx16BI [(match_operand:VNx16BI 1 "register_operand" "Upa")
			 (match_operand:VNx16BI 2 "register_operand" "Upa")
			 (match_operand:PRED_ALL 3 "aarch64_simd_imm_zero")]
			UNSPEC_TRN1_CONV))]
  "TARGET_SVE"
  "trn1\t%0.<PRED_ALL:Vetype>, %1.<PRED_ALL:Vetype>, %2.<PRED_ALL:Vetype>"
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
  [(set (match_operand:SVE_FULL_BHSI 0 "register_operand" "=w")
	(unspec:SVE_FULL_BHSI
	  [(match_operand:<VWIDE> 1 "register_operand" "w")
	   (match_operand:<VWIDE> 2 "register_operand" "w")]
	  UNSPEC_PACK))]
  "TARGET_SVE"
  "uzp1\t%0.<Vetype>, %1.<Vetype>, %2.<Vetype>"
)

;; Integer partial pack packing two partial SVE types into a single full SVE
;; type of the same element type.  Use UZP1 on the wider type, which discards
;; the high part of each wide element.  This allows to concat SVE partial types
;; into a wider vector.
(define_insn "@aarch64_pack_partial<mode>"
  [(set (match_operand:SVE_NO2E 0 "register_operand" "=w")
	(vec_concat:SVE_NO2E
	  (match_operand:<VHALF> 1 "register_operand" "w")
	  (match_operand:<VHALF> 2 "register_operand" "w")))]
  "TARGET_SVE"
  "uzp1\t%0.<Vctype>, %1.<Vctype>, %2.<Vctype>"
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
(define_expand "vec_unpack<su>_<perm_hilo>_<SVE_FULL_BHSI:mode>"
  [(match_operand:<VWIDE> 0 "register_operand")
   (unspec:<VWIDE>
     [(match_operand:SVE_FULL_BHSI 1 "register_operand")] UNPACK)]
  "TARGET_SVE"
  {
    emit_insn ((<hi_lanes_optab>
		? gen_aarch64_sve_<su>unpkhi_<SVE_FULL_BHSI:mode>
		: gen_aarch64_sve_<su>unpklo_<SVE_FULL_BHSI:mode>)
	       (operands[0], operands[1]));
    DONE;
  }
)

(define_insn "@aarch64_sve_<su>unpk<perm_hilo>_<SVE_FULL_BHSI:mode>"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
	(unspec:<VWIDE>
	  [(match_operand:SVE_FULL_BHSI 1 "register_operand" "w")]
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
	   (match_operand:SVE_FULL_F 1 "register_operand")]
	  SVE_COND_FCVTI))]
  "TARGET_SVE"
  {
    operands[2] = aarch64_ptrue_reg (<VPRED>mode);
  }
)

;; Predicated float-to-integer conversion, either to the same width or wider.
(define_insn "@aarch64_sve_<optab>_nontrunc<SVE_FULL_F:mode><SVE_FULL_HSDI:mode>"
  [(set (match_operand:SVE_FULL_HSDI 0 "register_operand")
	(unspec:SVE_FULL_HSDI
	  [(match_operand:<SVE_FULL_HSDI:VPRED> 1 "register_operand")
	   (match_operand:SI 3 "aarch64_sve_gp_strictness")
	   (match_operand:SVE_FULL_F 2 "register_operand")]
	  SVE_COND_FCVTI))]
  "TARGET_SVE && <SVE_FULL_HSDI:elem_bits> >= <SVE_FULL_F:elem_bits>"
  {@ [ cons: =0 , 1   , 2 ; attrs: movprfx ]
     [ w        , Upl , 0 ; *              ] fcvtz<su>\t%0.<SVE_FULL_HSDI:Vetype>, %1/m, %2.<SVE_FULL_F:Vetype>
     [ ?&w      , Upl , w ; yes            ] movprfx\t%0, %2\;fcvtz<su>\t%0.<SVE_FULL_HSDI:Vetype>, %1/m, %2.<SVE_FULL_F:Vetype>
  }
)

;; Predicated narrowing float-to-integer conversion.
(define_insn "@aarch64_sve_<optab>_trunc<VNx2DF_ONLY:mode><VNx4SI_ONLY:mode>"
  [(set (match_operand:VNx4SI_ONLY 0 "register_operand")
	(unspec:VNx4SI_ONLY
	  [(match_operand:VNx2BI 1 "register_operand")
	   (match_operand:SI 3 "aarch64_sve_gp_strictness")
	   (match_operand:VNx2DF_ONLY 2 "register_operand")]
	  SVE_COND_FCVTI))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2 ; attrs: movprfx ]
     [ w        , Upl , 0 ; *              ] fcvtz<su>\t%0.<VNx4SI_ONLY:Vetype>, %1/m, %2.<VNx2DF_ONLY:Vetype>
     [ ?&w      , Upl , w ; yes            ] movprfx\t%0, %2\;fcvtz<su>\t%0.<VNx4SI_ONLY:Vetype>, %1/m, %2.<VNx2DF_ONLY:Vetype>
  }
)

;; Predicated float-to-integer conversion with merging, either to the same
;; width or wider.
(define_expand "@cond_<optab>_nontrunc<SVE_FULL_F:mode><SVE_FULL_HSDI:mode>"
  [(set (match_operand:SVE_FULL_HSDI 0 "register_operand")
	(unspec:SVE_FULL_HSDI
	  [(match_operand:<SVE_FULL_HSDI:VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_HSDI
	     [(match_dup 1)
	      (const_int SVE_STRICT_GP)
	      (match_operand:SVE_FULL_F 2 "register_operand")]
	     SVE_COND_FCVTI)
	   (match_operand:SVE_FULL_HSDI 3 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE && <SVE_FULL_HSDI:elem_bits> >= <SVE_FULL_F:elem_bits>"
)

;; The first alternative doesn't need the earlyclobber, but the only case
;; it would help is the uninteresting one in which operands 2 and 3 are
;; the same register (despite having different modes).  Making all the
;; alternatives earlyclobber makes things more consistent for the
;; register allocator.
(define_insn_and_rewrite "*cond_<optab>_nontrunc<SVE_FULL_F:mode><SVE_FULL_HSDI:mode>_relaxed"
  [(set (match_operand:SVE_FULL_HSDI 0 "register_operand")
	(unspec:SVE_FULL_HSDI
	  [(match_operand:<SVE_FULL_HSDI:VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_HSDI
	     [(match_operand 4)
	      (const_int SVE_RELAXED_GP)
	      (match_operand:SVE_FULL_F 2 "register_operand")]
	     SVE_COND_FCVTI)
	   (match_operand:SVE_FULL_HSDI 3 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE && <SVE_FULL_HSDI:elem_bits> >= <SVE_FULL_F:elem_bits>"
  {@ [ cons: =0 , 1   , 2 , 3  ; attrs: movprfx ]
     [ &w       , Upl , w , 0  ; *              ] fcvtz<su>\t%0.<SVE_FULL_HSDI:Vetype>, %1/m, %2.<SVE_FULL_F:Vetype>
     [ &w       , Upl , w , Dz ; yes            ] movprfx\t%0.<SVE_FULL_HSDI:Vetype>, %1/z, %2.<SVE_FULL_HSDI:Vetype>\;fcvtz<su>\t%0.<SVE_FULL_HSDI:Vetype>, %1/m, %2.<SVE_FULL_F:Vetype>
     [ ?&w      , Upl , w , w  ; yes            ] movprfx\t%0, %3\;fcvtz<su>\t%0.<SVE_FULL_HSDI:Vetype>, %1/m, %2.<SVE_FULL_F:Vetype>
  }
  "&& !rtx_equal_p (operands[1], operands[4])"
  {
    operands[4] = copy_rtx (operands[1]);
  }
)

(define_insn "*cond_<optab>_nontrunc<SVE_FULL_F:mode><SVE_FULL_HSDI:mode>_strict"
  [(set (match_operand:SVE_FULL_HSDI 0 "register_operand")
	(unspec:SVE_FULL_HSDI
	  [(match_operand:<SVE_FULL_HSDI:VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_HSDI
	     [(match_dup 1)
	      (const_int SVE_STRICT_GP)
	      (match_operand:SVE_FULL_F 2 "register_operand")]
	     SVE_COND_FCVTI)
	   (match_operand:SVE_FULL_HSDI 3 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE && <SVE_FULL_HSDI:elem_bits> >= <SVE_FULL_F:elem_bits>"
  {@ [ cons: =0 , 1   , 2 , 3  ; attrs: movprfx ]
     [ &w       , Upl , w , 0  ; *              ] fcvtz<su>\t%0.<SVE_FULL_HSDI:Vetype>, %1/m, %2.<SVE_FULL_F:Vetype>
     [ &w       , Upl , w , Dz ; yes            ] movprfx\t%0.<SVE_FULL_HSDI:Vetype>, %1/z, %2.<SVE_FULL_HSDI:Vetype>\;fcvtz<su>\t%0.<SVE_FULL_HSDI:Vetype>, %1/m, %2.<SVE_FULL_F:Vetype>
     [ ?&w      , Upl , w , w  ; yes            ] movprfx\t%0, %3\;fcvtz<su>\t%0.<SVE_FULL_HSDI:Vetype>, %1/m, %2.<SVE_FULL_F:Vetype>
  }
)

;; Predicated narrowing float-to-integer conversion with merging.
(define_expand "@cond_<optab>_trunc<VNx2DF_ONLY:mode><VNx4SI_ONLY:mode>"
  [(set (match_operand:VNx4SI_ONLY 0 "register_operand")
	(unspec:VNx4SI_ONLY
	  [(match_operand:VNx2BI 1 "register_operand")
	   (unspec:VNx4SI_ONLY
	     [(match_dup 1)
	      (const_int SVE_STRICT_GP)
	      (match_operand:VNx2DF_ONLY 2 "register_operand")]
	     SVE_COND_FCVTI)
	   (match_operand:VNx4SI_ONLY 3 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE"
)

(define_insn "*cond_<optab>_trunc<VNx2DF_ONLY:mode><VNx4SI_ONLY:mode>"
  [(set (match_operand:VNx4SI_ONLY 0 "register_operand")
	(unspec:VNx4SI_ONLY
	  [(match_operand:VNx2BI 1 "register_operand")
	   (unspec:VNx4SI_ONLY
	     [(match_dup 1)
	      (match_operand:SI 4 "aarch64_sve_gp_strictness")
	      (match_operand:VNx2DF_ONLY 2 "register_operand")]
	     SVE_COND_FCVTI)
	   (match_operand:VNx4SI_ONLY 3 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2 , 3  ; attrs: movprfx ]
     [ &w       , Upl , w , 0  ; *              ] fcvtz<su>\t%0.<VNx4SI_ONLY:Vetype>, %1/m, %2.<VNx2DF_ONLY:Vetype>
     [ &w       , Upl , w , Dz ; yes            ] movprfx\t%0.<VNx2DF_ONLY:Vetype>, %1/z, %2.<VNx2DF_ONLY:Vetype>\;fcvtz<su>\t%0.<VNx4SI_ONLY:Vetype>, %1/m, %2.<VNx2DF_ONLY:Vetype>
     [ ?&w      , Upl , w , w  ; yes            ] movprfx\t%0, %3\;fcvtz<su>\t%0.<VNx4SI_ONLY:Vetype>, %1/m, %2.<VNx2DF_ONLY:Vetype>
  }
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
  [(set (match_operand:SVE_FULL_F 0 "register_operand")
	(unspec:SVE_FULL_F
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
(define_insn "@aarch64_sve_<optab>_nonextend<SVE_FULL_HSDI:mode><SVE_FULL_F:mode>"
  [(set (match_operand:SVE_FULL_F 0 "register_operand")
	(unspec:SVE_FULL_F
	  [(match_operand:<SVE_FULL_HSDI:VPRED> 1 "register_operand")
	   (match_operand:SI 3 "aarch64_sve_gp_strictness")
	   (match_operand:SVE_FULL_HSDI 2 "register_operand")]
	  SVE_COND_ICVTF))]
  "TARGET_SVE && <SVE_FULL_HSDI:elem_bits> >= <SVE_FULL_F:elem_bits>"
  {@ [ cons: =0 , 1   , 2 ; attrs: movprfx ]
     [ w        , Upl , 0 ; *              ] <su>cvtf\t%0.<SVE_FULL_F:Vetype>, %1/m, %2.<SVE_FULL_HSDI:Vetype>
     [ ?&w      , Upl , w ; yes            ] movprfx\t%0, %2\;<su>cvtf\t%0.<SVE_FULL_F:Vetype>, %1/m, %2.<SVE_FULL_HSDI:Vetype>
  }
)

;; Predicated widening integer-to-float conversion.
(define_insn "@aarch64_sve_<optab>_extend<VNx4SI_ONLY:mode><VNx2DF_ONLY:mode>"
  [(set (match_operand:VNx2DF_ONLY 0 "register_operand")
	(unspec:VNx2DF_ONLY
	  [(match_operand:VNx2BI 1 "register_operand")
	   (match_operand:SI 3 "aarch64_sve_gp_strictness")
	   (match_operand:VNx4SI_ONLY 2 "register_operand")]
	  SVE_COND_ICVTF))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2 ; attrs: movprfx ]
     [ w        , Upl , 0 ; *              ] <su>cvtf\t%0.<VNx2DF_ONLY:Vetype>, %1/m, %2.<VNx4SI_ONLY:Vetype>
     [ ?&w      , Upl , w ; yes            ] movprfx\t%0, %2\;<su>cvtf\t%0.<VNx2DF_ONLY:Vetype>, %1/m, %2.<VNx4SI_ONLY:Vetype>
  }
)

;; Predicated integer-to-float conversion with merging, either to the same
;; width or narrower.
(define_expand "@cond_<optab>_nonextend<SVE_FULL_HSDI:mode><SVE_FULL_F:mode>"
  [(set (match_operand:SVE_FULL_F 0 "register_operand")
	(unspec:SVE_FULL_F
	  [(match_operand:<SVE_FULL_HSDI:VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_F
	     [(match_dup 1)
	      (const_int SVE_STRICT_GP)
	      (match_operand:SVE_FULL_HSDI 2 "register_operand")]
	     SVE_COND_ICVTF)
	   (match_operand:SVE_FULL_F 3 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE && <SVE_FULL_HSDI:elem_bits> >= <SVE_FULL_F:elem_bits>"
)

;; The first alternative doesn't need the earlyclobber, but the only case
;; it would help is the uninteresting one in which operands 2 and 3 are
;; the same register (despite having different modes).  Making all the
;; alternatives earlyclobber makes things more consistent for the
;; register allocator.
(define_insn_and_rewrite "*cond_<optab>_nonextend<SVE_FULL_HSDI:mode><SVE_FULL_F:mode>_relaxed"
  [(set (match_operand:SVE_FULL_F 0 "register_operand")
	(unspec:SVE_FULL_F
	  [(match_operand:<SVE_FULL_HSDI:VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_F
	     [(match_operand 4)
	      (const_int SVE_RELAXED_GP)
	      (match_operand:SVE_FULL_HSDI 2 "register_operand")]
	     SVE_COND_ICVTF)
	   (match_operand:SVE_FULL_F 3 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE && <SVE_FULL_HSDI:elem_bits> >= <SVE_FULL_F:elem_bits>"
  {@ [ cons: =0 , 1   , 2 , 3  ; attrs: movprfx ]
     [ &w       , Upl , w , 0  ; *              ] <su>cvtf\t%0.<SVE_FULL_F:Vetype>, %1/m, %2.<SVE_FULL_HSDI:Vetype>
     [ &w       , Upl , w , Dz ; yes            ] movprfx\t%0.<SVE_FULL_HSDI:Vetype>, %1/z, %2.<SVE_FULL_HSDI:Vetype>\;<su>cvtf\t%0.<SVE_FULL_F:Vetype>, %1/m, %2.<SVE_FULL_HSDI:Vetype>
     [ ?&w      , Upl , w , w  ; yes            ] movprfx\t%0, %3\;<su>cvtf\t%0.<SVE_FULL_F:Vetype>, %1/m, %2.<SVE_FULL_HSDI:Vetype>
  }
  "&& !rtx_equal_p (operands[1], operands[4])"
  {
    operands[4] = copy_rtx (operands[1]);
  }
)

(define_insn "*cond_<optab>_nonextend<SVE_FULL_HSDI:mode><SVE_FULL_F:mode>_strict"
  [(set (match_operand:SVE_FULL_F 0 "register_operand")
	(unspec:SVE_FULL_F
	  [(match_operand:<SVE_FULL_HSDI:VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_F
	     [(match_dup 1)
	      (const_int SVE_STRICT_GP)
	      (match_operand:SVE_FULL_HSDI 2 "register_operand")]
	     SVE_COND_ICVTF)
	   (match_operand:SVE_FULL_F 3 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE && <SVE_FULL_HSDI:elem_bits> >= <SVE_FULL_F:elem_bits>"
  {@ [ cons: =0 , 1   , 2 , 3  ; attrs: movprfx ]
     [ &w       , Upl , w , 0  ; *              ] <su>cvtf\t%0.<SVE_FULL_F:Vetype>, %1/m, %2.<SVE_FULL_HSDI:Vetype>
     [ &w       , Upl , w , Dz ; yes            ] movprfx\t%0.<SVE_FULL_HSDI:Vetype>, %1/z, %2.<SVE_FULL_HSDI:Vetype>\;<su>cvtf\t%0.<SVE_FULL_F:Vetype>, %1/m, %2.<SVE_FULL_HSDI:Vetype>
     [ ?&w      , Upl , w , w  ; yes            ] movprfx\t%0, %3\;<su>cvtf\t%0.<SVE_FULL_F:Vetype>, %1/m, %2.<SVE_FULL_HSDI:Vetype>
  }
)

;; Predicated widening integer-to-float conversion with merging.
(define_expand "@cond_<optab>_extend<VNx4SI_ONLY:mode><VNx2DF_ONLY:mode>"
  [(set (match_operand:VNx2DF_ONLY 0 "register_operand")
	(unspec:VNx2DF_ONLY
	  [(match_operand:VNx2BI 1 "register_operand")
	   (unspec:VNx2DF_ONLY
	     [(match_dup 1)
	      (const_int SVE_STRICT_GP)
	      (match_operand:VNx4SI_ONLY 2 "register_operand")]
	     SVE_COND_ICVTF)
	   (match_operand:VNx2DF_ONLY 3 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE"
)

(define_insn "*cond_<optab>_extend<VNx4SI_ONLY:mode><VNx2DF_ONLY:mode>"
  [(set (match_operand:VNx2DF_ONLY 0 "register_operand")
	(unspec:VNx2DF_ONLY
	  [(match_operand:VNx2BI 1 "register_operand")
	   (unspec:VNx2DF_ONLY
	     [(match_dup 1)
	      (match_operand:SI 4 "aarch64_sve_gp_strictness")
	      (match_operand:VNx4SI_ONLY 2 "register_operand")]
	     SVE_COND_ICVTF)
	   (match_operand:VNx2DF_ONLY 3 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2 , 3  ; attrs: movprfx ]
     [ w        , Upl , w , 0  ; *              ] <su>cvtf\t%0.<VNx2DF_ONLY:Vetype>, %1/m, %2.<VNx4SI_ONLY:Vetype>
     [ ?&w      , Upl , w , Dz ; yes            ] movprfx\t%0.<VNx2DF_ONLY:Vetype>, %1/z, %2.<VNx2DF_ONLY:Vetype>\;<su>cvtf\t%0.<VNx2DF_ONLY:Vetype>, %1/m, %2.<VNx4SI_ONLY:Vetype>
     [ ?&w      , Upl , w , w  ; yes            ] movprfx\t%0, %3\;<su>cvtf\t%0.<VNx2DF_ONLY:Vetype>, %1/m, %2.<VNx4SI_ONLY:Vetype>
  }
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
	(unspec:SVE_FULL_HSF
	  [(match_dup 3)
	   (const_int SVE_RELAXED_GP)
	   (match_operand:<VWIDE> 1 "register_operand")]
	  UNSPEC_COND_FCVT))
   (set (match_dup 5)
	(unspec:SVE_FULL_HSF
	  [(match_dup 3)
	   (const_int SVE_RELAXED_GP)
	   (match_operand:<VWIDE> 2 "register_operand")]
	  UNSPEC_COND_FCVT))
   (set (match_operand:SVE_FULL_HSF 0 "register_operand")
	(unspec:SVE_FULL_HSF [(match_dup 4) (match_dup 5)] UNSPEC_UZP1))]
  "TARGET_SVE"
  {
    operands[3] = aarch64_ptrue_reg (<VWIDE_PRED>mode);
    operands[4] = gen_reg_rtx (<MODE>mode);
    operands[5] = gen_reg_rtx (<MODE>mode);
  }
)

;; Predicated float-to-float truncation.
(define_insn "@aarch64_sve_<optab>_trunc<SVE_FULL_SDF:mode><SVE_FULL_HSF:mode>"
  [(set (match_operand:SVE_FULL_HSF 0 "register_operand")
	(unspec:SVE_FULL_HSF
	  [(match_operand:<SVE_FULL_SDF:VPRED> 1 "register_operand")
	   (match_operand:SI 3 "aarch64_sve_gp_strictness")
	   (match_operand:SVE_FULL_SDF 2 "register_operand")]
	  SVE_COND_FCVT))]
  "TARGET_SVE && <SVE_FULL_SDF:elem_bits> > <SVE_FULL_HSF:elem_bits>"
  {@ [ cons: =0 , 1   , 2 ; attrs: movprfx ]
     [ w        , Upl , 0 ; *              ] fcvt\t%0.<SVE_FULL_HSF:Vetype>, %1/m, %2.<SVE_FULL_SDF:Vetype>
     [ ?&w      , Upl , w ; yes            ] movprfx\t%0, %2\;fcvt\t%0.<SVE_FULL_HSF:Vetype>, %1/m, %2.<SVE_FULL_SDF:Vetype>
  }
)

;; Predicated float-to-float truncation with merging.
(define_expand "@cond_<optab>_trunc<SVE_FULL_SDF:mode><SVE_FULL_HSF:mode>"
  [(set (match_operand:SVE_FULL_HSF 0 "register_operand")
	(unspec:SVE_FULL_HSF
	  [(match_operand:<SVE_FULL_SDF:VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_HSF
	     [(match_dup 1)
	      (const_int SVE_STRICT_GP)
	      (match_operand:SVE_FULL_SDF 2 "register_operand")]
	     SVE_COND_FCVT)
	   (match_operand:SVE_FULL_HSF 3 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE && <SVE_FULL_SDF:elem_bits> > <SVE_FULL_HSF:elem_bits>"
)

(define_insn "*cond_<optab>_trunc<SVE_FULL_SDF:mode><SVE_FULL_HSF:mode>"
  [(set (match_operand:SVE_FULL_HSF 0 "register_operand")
	(unspec:SVE_FULL_HSF
	  [(match_operand:<SVE_FULL_SDF:VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_HSF
	     [(match_dup 1)
	      (match_operand:SI 4 "aarch64_sve_gp_strictness")
	      (match_operand:SVE_FULL_SDF 2 "register_operand")]
	     SVE_COND_FCVT)
	   (match_operand:SVE_FULL_HSF 3 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE && <SVE_FULL_SDF:elem_bits> > <SVE_FULL_HSF:elem_bits>"
  {@ [ cons: =0 , 1   , 2 , 3  ; attrs: movprfx ]
     [ w        , Upl , w , 0  ; *              ] fcvt\t%0.<SVE_FULL_HSF:Vetype>, %1/m, %2.<SVE_FULL_SDF:Vetype>
     [ ?&w      , Upl , w , Dz ; yes            ] movprfx\t%0.<SVE_FULL_SDF:Vetype>, %1/z, %2.<SVE_FULL_SDF:Vetype>\;fcvt\t%0.<SVE_FULL_HSF:Vetype>, %1/m, %2.<SVE_FULL_SDF:Vetype>
     [ ?&w      , Upl , w , w  ; yes            ] movprfx\t%0, %3\;fcvt\t%0.<SVE_FULL_HSF:Vetype>, %1/m, %2.<SVE_FULL_SDF:Vetype>
  }
)

;; -------------------------------------------------------------------------
;; ---- [FP<-FP] Packs (bfloat16)
;; -------------------------------------------------------------------------
;; Includes:
;; - BFCVT (BF16)
;; - BFCVTNT (BF16)
;; -------------------------------------------------------------------------

;; Predicated BFCVT.
(define_insn "@aarch64_sve_<optab>_trunc<VNx4SF_ONLY:mode><VNx8BF_ONLY:mode>"
  [(set (match_operand:VNx8BF_ONLY 0 "register_operand")
	(unspec:VNx8BF_ONLY
	  [(match_operand:VNx4BI 1 "register_operand")
	   (match_operand:SI 3 "aarch64_sve_gp_strictness")
	   (match_operand:VNx4SF_ONLY 2 "register_operand")]
	  SVE_COND_FCVT))]
  "TARGET_SVE_BF16"
  {@ [ cons: =0 , 1   , 2 ; attrs: movprfx ]
     [ w        , Upl , 0 ; *              ] bfcvt\t%0.h, %1/m, %2.s
     [ ?&w      , Upl , w ; yes            ] movprfx\t%0, %2\;bfcvt\t%0.h, %1/m, %2.s
  }
)

;; Predicated BFCVT with merging.
(define_expand "@cond_<optab>_trunc<VNx4SF_ONLY:mode><VNx8BF_ONLY:mode>"
  [(set (match_operand:VNx8BF_ONLY 0 "register_operand")
	(unspec:VNx8BF_ONLY
	  [(match_operand:VNx4BI 1 "register_operand")
	   (unspec:VNx8BF_ONLY
	     [(match_dup 1)
	      (const_int SVE_STRICT_GP)
	      (match_operand:VNx4SF_ONLY 2 "register_operand")]
	     SVE_COND_FCVT)
	   (match_operand:VNx8BF_ONLY 3 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE_BF16"
)

(define_insn "*cond_<optab>_trunc<VNx4SF_ONLY:mode><VNx8BF_ONLY:mode>"
  [(set (match_operand:VNx8BF_ONLY 0 "register_operand")
	(unspec:VNx8BF_ONLY
	  [(match_operand:VNx4BI 1 "register_operand")
	   (unspec:VNx8BF_ONLY
	     [(match_dup 1)
	      (match_operand:SI 4 "aarch64_sve_gp_strictness")
	      (match_operand:VNx4SF_ONLY 2 "register_operand")]
	     SVE_COND_FCVT)
	   (match_operand:VNx8BF_ONLY 3 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE_BF16"
  {@ [ cons: =0 , 1   , 2 , 3  ; attrs: movprfx ]
     [ w        , Upl , w , 0  ; *              ] bfcvt\t%0.h, %1/m, %2.s
     [ ?&w      , Upl , w , Dz ; yes            ] movprfx\t%0.s, %1/z, %2.s\;bfcvt\t%0.h, %1/m, %2.s
     [ ?&w      , Upl , w , w  ; yes            ] movprfx\t%0, %3\;bfcvt\t%0.h, %1/m, %2.s
  }
)

;; Predicated BFCVTNT.  This doesn't give a natural aarch64_pred_*/cond_*
;; pair because the even elements always have to be supplied for active
;; elements, even if the inactive elements don't matter.
;;
;; This instructions does not take MOVPRFX.
(define_insn "@aarch64_sve_cvtnt<mode>"
  [(set (match_operand:VNx8BF_ONLY 0 "register_operand" "=w")
	(unspec:VNx8BF_ONLY
	  [(match_operand:VNx4BI 2 "register_operand" "Upl")
	   (const_int SVE_STRICT_GP)
	   (match_operand:VNx8BF_ONLY 1 "register_operand" "0")
	   (match_operand:VNx4SF 3 "register_operand" "w")]
	  UNSPEC_COND_FCVTNT))]
  "TARGET_SVE_BF16"
  "bfcvtnt\t%0.h, %2/m, %3.s"
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
   (unspec:SVE_FULL_HSF
     [(match_operand:SVE_FULL_HSF 1 "register_operand")]
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
(define_insn "@aarch64_sve_<optab>_nontrunc<SVE_FULL_HSF:mode><SVE_FULL_SDF:mode>"
  [(set (match_operand:SVE_FULL_SDF 0 "register_operand")
	(unspec:SVE_FULL_SDF
	  [(match_operand:<SVE_FULL_SDF:VPRED> 1 "register_operand")
	   (match_operand:SI 3 "aarch64_sve_gp_strictness")
	   (match_operand:SVE_FULL_HSF 2 "register_operand")]
	  SVE_COND_FCVT))]
  "TARGET_SVE && <SVE_FULL_SDF:elem_bits> > <SVE_FULL_HSF:elem_bits>"
  {@ [ cons: =0 , 1   , 2 ; attrs: movprfx ]
     [ w        , Upl , 0 ; *              ] fcvt\t%0.<SVE_FULL_SDF:Vetype>, %1/m, %2.<SVE_FULL_HSF:Vetype>
     [ ?&w      , Upl , w ; yes            ] movprfx\t%0, %2\;fcvt\t%0.<SVE_FULL_SDF:Vetype>, %1/m, %2.<SVE_FULL_HSF:Vetype>
  }
)

;; Predicated float-to-float extension with merging.
(define_expand "@cond_<optab>_nontrunc<SVE_FULL_HSF:mode><SVE_FULL_SDF:mode>"
  [(set (match_operand:SVE_FULL_SDF 0 "register_operand")
	(unspec:SVE_FULL_SDF
	  [(match_operand:<SVE_FULL_SDF:VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_SDF
	     [(match_dup 1)
	      (const_int SVE_STRICT_GP)
	      (match_operand:SVE_FULL_HSF 2 "register_operand")]
	     SVE_COND_FCVT)
	   (match_operand:SVE_FULL_SDF 3 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE && <SVE_FULL_SDF:elem_bits> > <SVE_FULL_HSF:elem_bits>"
)

(define_insn "*cond_<optab>_nontrunc<SVE_FULL_HSF:mode><SVE_FULL_SDF:mode>"
  [(set (match_operand:SVE_FULL_SDF 0 "register_operand")
	(unspec:SVE_FULL_SDF
	  [(match_operand:<SVE_FULL_SDF:VPRED> 1 "register_operand")
	   (unspec:SVE_FULL_SDF
	     [(match_dup 1)
	      (match_operand:SI 4 "aarch64_sve_gp_strictness")
	      (match_operand:SVE_FULL_HSF 2 "register_operand")]
	     SVE_COND_FCVT)
	   (match_operand:SVE_FULL_SDF 3 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE && <SVE_FULL_SDF:elem_bits> > <SVE_FULL_HSF:elem_bits>"
  {@ [ cons: =0 , 1   , 2 , 3  ; attrs: movprfx ]
     [ w        , Upl , w , 0  ; *              ] fcvt\t%0.<SVE_FULL_SDF:Vetype>, %1/m, %2.<SVE_FULL_HSF:Vetype>
     [ ?&w      , Upl , w , Dz ; yes            ] movprfx\t%0.<SVE_FULL_SDF:Vetype>, %1/z, %2.<SVE_FULL_SDF:Vetype>\;fcvt\t%0.<SVE_FULL_SDF:Vetype>, %1/m, %2.<SVE_FULL_HSF:Vetype>
     [ ?&w      , Upl , w , w  ; yes            ] movprfx\t%0, %3\;fcvt\t%0.<SVE_FULL_SDF:Vetype>, %1/m, %2.<SVE_FULL_HSF:Vetype>
  }
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

(define_insn "@aarch64_sve_punpk<perm_hilo>_<mode>"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=Upa")
	(unspec:<VWIDE> [(match_operand:PRED_BHS 1 "register_operand" "Upa")]
			UNPACK_UNSIGNED))]
  "TARGET_SVE"
  "punpk<perm_hilo>\t%0.h, %1.b"
)

;; =========================================================================
;; == Vector partitioning
;; =========================================================================

;; -------------------------------------------------------------------------
;; ---- [PRED] Unary partitioning
;; -------------------------------------------------------------------------
;; Includes:
;; - BRKA
;; - BRKAS
;; - BRKB
;; - BRKBS
;; -------------------------------------------------------------------------

;; Note that unlike most other instructions that have both merging and
;; zeroing forms, these instructions don't operate elementwise and so
;; don't fit the IFN_COND model.
(define_insn "@aarch64_brk<brk_op>"
  [(set (match_operand:VNx16BI 0 "register_operand")
	(unspec:VNx16BI
	  [(match_operand:VNx16BI 1 "register_operand")
	   (match_operand:VNx16BI 2 "register_operand")
	   (match_operand:VNx16BI 3 "aarch64_simd_reg_or_zero")]
	  SVE_BRK_UNARY))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1   , 2   , 3  ; attrs: pred_clobber ]
     [ &Upa     ,  Upa , Upa , Dz; yes                 ] brk<brk_op>\t%0.b, %1/z, %2.b
     [ ?Upa     ,  0Upa, 0Upa, Dz; yes                 ] ^
     [ Upa      ,  Upa , Upa , Dz; no                  ] ^
     [ &Upa     ,  Upa , Upa , 0 ; yes                 ] brk<brk_op>\t%0.b, %1/m, %2.b
     [ ?Upa     ,  0Upa, 0Upa, 0 ; yes                 ] ^
     [ Upa      ,  Upa , Upa , 0 ; no                  ] ^
  }
)

;; Same, but also producing a flags result.
(define_insn "*aarch64_brk<brk_op>_cc"
  [(set (reg:CC_NZC CC_REGNUM)
	(unspec:CC_NZC
	  [(match_operand:VNx16BI 1 "register_operand")
	   (match_dup 1)
	   (match_operand:SI 4 "aarch64_sve_ptrue_flag")
	   (unspec:VNx16BI
	     [(match_dup 1)
	      (match_operand:VNx16BI 2 "register_operand")
	      (match_operand:VNx16BI 3 "aarch64_simd_imm_zero")]
	     SVE_BRK_UNARY)]
	  UNSPEC_PTEST))
   (set (match_operand:VNx16BI 0 "register_operand")
	(unspec:VNx16BI
	  [(match_dup 1)
	   (match_dup 2)
	   (match_dup 3)]
	  SVE_BRK_UNARY))]
  "TARGET_SVE"
  {@ [ cons: =0, 1   , 2   ; attrs: pred_clobber ]
     [ &Upa    , Upa , Upa ; yes                 ] brk<brk_op>s\t%0.b, %1/z, %2.b
     [ ?Upa    , 0Upa, 0Upa; yes                 ] ^
     [ Upa     , Upa , Upa ; no                  ] ^
  }
)

;; Same, but with only the flags result being interesting.
(define_insn "*aarch64_brk<brk_op>_ptest"
  [(set (reg:CC_NZC CC_REGNUM)
	(unspec:CC_NZC
	  [(match_operand:VNx16BI 1 "register_operand")
	   (match_dup 1)
	   (match_operand:SI 4 "aarch64_sve_ptrue_flag")
	   (unspec:VNx16BI
	     [(match_dup 1)
	      (match_operand:VNx16BI 2 "register_operand")
	      (match_operand:VNx16BI 3 "aarch64_simd_imm_zero")]
	     SVE_BRK_UNARY)]
	  UNSPEC_PTEST))
   (clobber (match_scratch:VNx16BI 0))]
  "TARGET_SVE"
  {@ [ cons: =0, 1   , 2   ; attrs: pred_clobber ]
     [ &Upa    , Upa , Upa ; yes                 ] brk<brk_op>s\t%0.b, %1/z, %2.b
     [ ?Upa    , 0Upa, 0Upa; yes                 ] ^
     [ Upa     , Upa , Upa ; no                  ] ^
  }
)

;; -------------------------------------------------------------------------
;; ---- [PRED] Binary partitioning
;; -------------------------------------------------------------------------
;; Includes:
;; - BRKN
;; - BRKNS
;; - BRKPA
;; - BRKPAS
;; - BRKPB
;; - BRKPBS
;; -------------------------------------------------------------------------

;; Binary BRKs (BRKN, BRKPA, BRKPB).
(define_insn "@aarch64_brk<brk_op>"
  [(set (match_operand:VNx16BI 0 "register_operand")
	(unspec:VNx16BI
	  [(match_operand:VNx16BI 1 "register_operand")
	   (match_operand:VNx16BI 2 "register_operand")
	   (match_operand:VNx16BI 3 "register_operand")]
	  SVE_BRK_BINARY))]
  "TARGET_SVE"
  {@ [ cons: =0,  1  , 2   , 3             ; attrs: pred_clobber ]
     [ &Upa    , Upa , Upa , <brk_reg_con> ; yes                 ] brk<brk_op>\t%0.b, %1/z, %2.b, %<brk_reg_opno>.b
     [ ?Upa    , 0Upa, 0Upa, 0<brk_reg_con>; yes                 ] ^
     [ Upa     , Upa , Upa , <brk_reg_con> ; no                  ] ^
  }
)

;; BRKN, producing both a predicate and a flags result.  Unlike other
;; flag-setting instructions, these flags are always set wrt a ptrue.
(define_insn_and_rewrite "*aarch64_brkn_cc"
  [(set (reg:CC_NZC CC_REGNUM)
	(unspec:CC_NZC
	  [(match_operand:VNx16BI 4)
	   (match_operand:VNx16BI 5)
	   (const_int SVE_KNOWN_PTRUE)
	   (unspec:VNx16BI
	     [(match_operand:VNx16BI 1 "register_operand" "Upa")
	      (match_operand:VNx16BI 2 "register_operand" "Upa")
	      (match_operand:VNx16BI 3 "register_operand" "0")]
	     UNSPEC_BRKN)]
	  UNSPEC_PTEST))
   (set (match_operand:VNx16BI 0 "register_operand" "=Upa")
	(unspec:VNx16BI
	  [(match_dup 1)
	   (match_dup 2)
	   (match_dup 3)]
	  UNSPEC_BRKN))]
  "TARGET_SVE"
  "brkns\t%0.b, %1/z, %2.b, %0.b"
  "&& (operands[4] != CONST0_RTX (VNx16BImode)
       || operands[5] != CONST0_RTX (VNx16BImode))"
  {
    operands[4] = CONST0_RTX (VNx16BImode);
    operands[5] = CONST0_RTX (VNx16BImode);
  }
)

;; Same, but with only the flags result being interesting.
(define_insn_and_rewrite "*aarch64_brkn_ptest"
  [(set (reg:CC_NZC CC_REGNUM)
	(unspec:CC_NZC
	  [(match_operand:VNx16BI 4)
	   (match_operand:VNx16BI 5)
	   (const_int SVE_KNOWN_PTRUE)
	   (unspec:VNx16BI
	     [(match_operand:VNx16BI 1 "register_operand" "Upa")
	      (match_operand:VNx16BI 2 "register_operand" "Upa")
	      (match_operand:VNx16BI 3 "register_operand" "0")]
	     UNSPEC_BRKN)]
	  UNSPEC_PTEST))
   (clobber (match_scratch:VNx16BI 0 "=Upa"))]
  "TARGET_SVE"
  "brkns\t%0.b, %1/z, %2.b, %0.b"
  "&& (operands[4] != CONST0_RTX (VNx16BImode)
       || operands[5] != CONST0_RTX (VNx16BImode))"
  {
    operands[4] = CONST0_RTX (VNx16BImode);
    operands[5] = CONST0_RTX (VNx16BImode);
  }
)

;; BRKPA and BRKPB, producing both a predicate and a flags result.
(define_insn "*aarch64_brk<brk_op>_cc"
  [(set (reg:CC_NZC CC_REGNUM)
	(unspec:CC_NZC
	  [(match_operand:VNx16BI 1 "register_operand")
	   (match_dup 1)
	   (match_operand:SI 4 "aarch64_sve_ptrue_flag")
	   (unspec:VNx16BI
	     [(match_dup 1)
	      (match_operand:VNx16BI 2 "register_operand")
	      (match_operand:VNx16BI 3 "register_operand")]
	     SVE_BRKP)]
	  UNSPEC_PTEST))
   (set (match_operand:VNx16BI 0 "register_operand")
	(unspec:VNx16BI
	  [(match_dup 1)
	   (match_dup 2)
	   (match_dup 3)]
	  SVE_BRKP))]
  "TARGET_SVE"
  {@ [ cons: =0, 1   , 2   , 3   , 4; attrs: pred_clobber ]
     [ &Upa    , Upa , Upa , Upa ,  ; yes                 ] brk<brk_op>s\t%0.b, %1/z, %2.b, %3.b
     [ ?Upa    , 0Upa, 0Upa, 0Upa,  ; yes                 ] ^
     [ Upa     , Upa , Upa , Upa ,  ; no                  ] ^
  }
)

;; Same, but with only the flags result being interesting.
(define_insn "*aarch64_brk<brk_op>_ptest"
  [(set (reg:CC_NZC CC_REGNUM)
	(unspec:CC_NZC
	  [(match_operand:VNx16BI 1 "register_operand")
	   (match_dup 1)
	   (match_operand:SI 4 "aarch64_sve_ptrue_flag")
	   (unspec:VNx16BI
	     [(match_dup 1)
	      (match_operand:VNx16BI 2 "register_operand")
	      (match_operand:VNx16BI 3 "register_operand")]
	     SVE_BRKP)]
	  UNSPEC_PTEST))
   (clobber (match_scratch:VNx16BI 0))]
  "TARGET_SVE"
  {@ [ cons: =0, 1   , 2   , 3   ; attrs: pred_clobber ]
     [ &Upa    , Upa , Upa , Upa ; yes                 ] brk<brk_op>s\t%0.b, %1/z, %2.b, %3.b
     [ ?Upa    , 0Upa, 0Upa, 0Upa; yes                 ] ^
     [ Upa     , Upa , Upa , Upa ; no                  ] ^
  }
)

;; -------------------------------------------------------------------------
;; ---- [PRED] Scalarization
;; -------------------------------------------------------------------------
;; Includes:
;; - PFIRST
;; - PNEXT
;; -------------------------------------------------------------------------

(define_insn "@aarch64_sve_<sve_pred_op><mode>"
  [(set (match_operand:PRED_ALL 0 "register_operand" "=Upa")
	(unspec:PRED_ALL
	  [(match_operand:PRED_ALL 1 "register_operand" "Upa")
	   (match_operand:SI 2 "aarch64_sve_ptrue_flag")
	   (match_operand:PRED_ALL 3 "register_operand" "0")]
	  SVE_PITER))
   (clobber (reg:CC_NZC CC_REGNUM))]
  "TARGET_SVE && <max_elem_bits> >= <elem_bits>"
  "<sve_pred_op>\t%0.<Vetype>, %1, %0.<Vetype>"
)

;; Same, but also producing a flags result.
(define_insn_and_rewrite "*aarch64_sve_<sve_pred_op><mode>_cc"
  [(set (reg:CC_NZC CC_REGNUM)
	(unspec:CC_NZC
	  [(match_operand:VNx16BI 1 "register_operand" "Upa")
	   (match_operand 2)
	   (match_operand:SI 3 "aarch64_sve_ptrue_flag")
	   (unspec:PRED_ALL
	     [(match_operand 4)
	      (match_operand:SI 5 "aarch64_sve_ptrue_flag")
	      (match_operand:PRED_ALL 6 "register_operand" "0")]
	     SVE_PITER)]
	  UNSPEC_PTEST))
   (set (match_operand:PRED_ALL 0 "register_operand" "=Upa")
	(unspec:PRED_ALL
	  [(match_dup 4)
	   (match_dup 5)
	   (match_dup 6)]
	  SVE_PITER))]
  "TARGET_SVE
   && <max_elem_bits> >= <elem_bits>
   && aarch64_sve_same_pred_for_ptest_p (&operands[2], &operands[4])"
  "<sve_pred_op>\t%0.<Vetype>, %1, %0.<Vetype>"
  "&& !rtx_equal_p (operands[2], operands[4])"
  {
    operands[4] = operands[2];
    operands[5] = operands[3];
  }
)

;; Same, but with only the flags result being interesting.
(define_insn_and_rewrite "*aarch64_sve_<sve_pred_op><mode>_ptest"
  [(set (reg:CC_NZC CC_REGNUM)
	(unspec:CC_NZC
	  [(match_operand:VNx16BI 1 "register_operand" "Upa")
	   (match_operand 2)
	   (match_operand:SI 3 "aarch64_sve_ptrue_flag")
	   (unspec:PRED_ALL
	     [(match_operand 4)
	      (match_operand:SI 5 "aarch64_sve_ptrue_flag")
	      (match_operand:PRED_ALL 6 "register_operand" "0")]
	     SVE_PITER)]
	  UNSPEC_PTEST))
   (clobber (match_scratch:PRED_ALL 0 "=Upa"))]
  "TARGET_SVE
   && <max_elem_bits> >= <elem_bits>
   && aarch64_sve_same_pred_for_ptest_p (&operands[2], &operands[4])"
  "<sve_pred_op>\t%0.<Vetype>, %1, %0.<Vetype>"
  "&& !rtx_equal_p (operands[2], operands[4])"
  {
    operands[4] = operands[2];
    operands[5] = operands[3];
  }
)

;; =========================================================================
;; == Counting elements
;; =========================================================================

;; -------------------------------------------------------------------------
;; ---- [INT] Count elements in a pattern (scalar)
;; -------------------------------------------------------------------------
;; Includes:
;; - CNTB
;; - CNTD
;; - CNTH
;; - CNTW
;; -------------------------------------------------------------------------

;; Count the number of elements in an svpattern.  Operand 1 is the pattern,
;; operand 2 is the number of elements that fit in a 128-bit block, and
;; operand 3 is a multiplier in the range [1, 16].
;;
;; Note that this pattern isn't used for SV_ALL (but would work for that too).
(define_insn "aarch64_sve_cnt_pat"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI
	  (unspec:SI [(match_operand:DI 1 "const_int_operand")
		      (match_operand:DI 2 "const_int_operand")
		      (match_operand:DI 3 "const_int_operand")]
		     UNSPEC_SVE_CNT_PAT)))]
  "TARGET_SVE"
  {
    return aarch64_output_sve_cnt_pat_immediate ("cnt", "%x0", operands + 1);
  }
)

;; -------------------------------------------------------------------------
;; ---- [INT] Increment by the number of elements in a pattern (scalar)
;; -------------------------------------------------------------------------
;; Includes:
;; - INC
;; - SQINC
;; - UQINC
;; -------------------------------------------------------------------------

;; Increment a DImode register by the number of elements in an svpattern.
;; See aarch64_sve_cnt_pat for the counting behavior.
(define_insn "@aarch64_sve_<inc_dec><mode>_pat"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ANY_PLUS:DI (zero_extend:DI
		       (unspec:SI [(match_operand:DI 2 "const_int_operand")
				   (match_operand:DI 3 "const_int_operand")
				   (match_operand:DI 4 "const_int_operand")]
				  UNSPEC_SVE_CNT_PAT))
		     (match_operand:DI_ONLY 1 "register_operand" "0")))]
  "TARGET_SVE"
  {
    return aarch64_output_sve_cnt_pat_immediate ("<inc_dec>", "%x0",
						 operands + 2);
  }
)

;; Increment an SImode register by the number of elements in an svpattern
;; using modular arithmetic.  See aarch64_sve_cnt_pat for the counting
;; behavior.
(define_insn "*aarch64_sve_incsi_pat"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (unspec:SI [(match_operand:DI 2 "const_int_operand")
			     (match_operand:DI 3 "const_int_operand")
			     (match_operand:DI 4 "const_int_operand")]
			    UNSPEC_SVE_CNT_PAT)
		 (match_operand:SI 1 "register_operand" "0")))]
  "TARGET_SVE"
  {
    return aarch64_output_sve_cnt_pat_immediate ("inc", "%x0", operands + 2);
  }
)

;; Increment an SImode register by the number of elements in an svpattern
;; using saturating arithmetic, extending the result to 64 bits.
;;
;; See aarch64_sve_cnt_pat for the counting behavior.
(define_insn "@aarch64_sve_<inc_dec><mode>_pat"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(<paired_extend>:DI
	  (SAT_PLUS:SI
	    (unspec:SI [(match_operand:DI 2 "const_int_operand")
			(match_operand:DI 3 "const_int_operand")
			(match_operand:DI 4 "const_int_operand")]
		       UNSPEC_SVE_CNT_PAT)
	    (match_operand:SI_ONLY 1 "register_operand" "0"))))]
  "TARGET_SVE"
  {
    const char *registers = (<CODE> == SS_PLUS ? "%x0, %w0" : "%w0");
    return aarch64_output_sve_cnt_pat_immediate ("<inc_dec>", registers,
						 operands + 2);
  }
)

;; -------------------------------------------------------------------------
;; ---- [INT] Increment by the number of elements in a pattern (vector)
;; -------------------------------------------------------------------------
;; Includes:
;; - INC
;; - SQINC
;; - UQINC
;; -------------------------------------------------------------------------

;; Increment a vector of DIs by the number of elements in an svpattern.
;; See aarch64_sve_cnt_pat for the counting behavior.
(define_insn "@aarch64_sve_<inc_dec><mode>_pat"
  [(set (match_operand:VNx2DI 0 "register_operand" "=w, ?&w")
	(ANY_PLUS:VNx2DI
	  (vec_duplicate:VNx2DI
	    (zero_extend:DI
	      (unspec:SI [(match_operand:DI 2 "const_int_operand")
			  (match_operand:DI 3 "const_int_operand")
			  (match_operand:DI 4 "const_int_operand")]
			 UNSPEC_SVE_CNT_PAT)))
	  (match_operand:VNx2DI_ONLY 1 "register_operand" "0, w")))]
  "TARGET_SVE"
  {
    if (which_alternative == 1)
      output_asm_insn ("movprfx\t%0, %1", operands);
    return aarch64_output_sve_cnt_pat_immediate ("<inc_dec>", "%0.<Vetype>",
						 operands + 2);
  }
  [(set_attr "movprfx" "*,yes")]
)

;; Increment a vector of SIs by the number of elements in an svpattern.
;; See aarch64_sve_cnt_pat for the counting behavior.
(define_insn "@aarch64_sve_<inc_dec><mode>_pat"
  [(set (match_operand:VNx4SI 0 "register_operand" "=w, ?&w")
	(ANY_PLUS:VNx4SI
	  (vec_duplicate:VNx4SI
	    (unspec:SI [(match_operand:DI 2 "const_int_operand")
			(match_operand:DI 3 "const_int_operand")
			(match_operand:DI 4 "const_int_operand")]
		       UNSPEC_SVE_CNT_PAT))
	  (match_operand:VNx4SI_ONLY 1 "register_operand" "0, w")))]
  "TARGET_SVE"
  {
    if (which_alternative == 1)
      output_asm_insn ("movprfx\t%0, %1", operands);
    return aarch64_output_sve_cnt_pat_immediate ("<inc_dec>", "%0.<Vetype>",
						 operands + 2);
  }
  [(set_attr "movprfx" "*,yes")]
)

;; Increment a vector of HIs by the number of elements in an svpattern.
;; See aarch64_sve_cnt_pat for the counting behavior.
(define_expand "@aarch64_sve_<inc_dec><mode>_pat"
  [(set (match_operand:VNx8HI 0 "register_operand")
	(ANY_PLUS:VNx8HI
	  (vec_duplicate:VNx8HI
	    (truncate:HI
	      (unspec:SI [(match_operand:DI 2 "const_int_operand")
			  (match_operand:DI 3 "const_int_operand")
			  (match_operand:DI 4 "const_int_operand")]
			 UNSPEC_SVE_CNT_PAT)))
	  (match_operand:VNx8HI_ONLY 1 "register_operand")))]
  "TARGET_SVE"
)

(define_insn "*aarch64_sve_<inc_dec><mode>_pat"
  [(set (match_operand:VNx8HI 0 "register_operand" "=w, ?&w")
	(ANY_PLUS:VNx8HI
	  (vec_duplicate:VNx8HI
	    (match_operator:HI 5 "subreg_lowpart_operator"
	      [(unspec:SI [(match_operand:DI 2 "const_int_operand")
			   (match_operand:DI 3 "const_int_operand")
			   (match_operand:DI 4 "const_int_operand")]
			  UNSPEC_SVE_CNT_PAT)]))
	  (match_operand:VNx8HI_ONLY 1 "register_operand" "0, w")))]
  "TARGET_SVE"
  {
    if (which_alternative == 1)
      output_asm_insn ("movprfx\t%0, %1", operands);
    return aarch64_output_sve_cnt_pat_immediate ("<inc_dec>", "%0.<Vetype>",
						 operands + 2);
  }
  [(set_attr "movprfx" "*,yes")]
)

;; -------------------------------------------------------------------------
;; ---- [INT] Decrement by the number of elements in a pattern (scalar)
;; -------------------------------------------------------------------------
;; Includes:
;; - DEC
;; - SQDEC
;; - UQDEC
;; -------------------------------------------------------------------------

;; Decrement a DImode register by the number of elements in an svpattern.
;; See aarch64_sve_cnt_pat for the counting behavior.
(define_insn "@aarch64_sve_<inc_dec><mode>_pat"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ANY_MINUS:DI (match_operand:DI_ONLY 1 "register_operand" "0")
		      (zero_extend:DI
			(unspec:SI [(match_operand:DI 2 "const_int_operand")
				    (match_operand:DI 3 "const_int_operand")
				    (match_operand:DI 4 "const_int_operand")]
				   UNSPEC_SVE_CNT_PAT))))]
  "TARGET_SVE"
  {
    return aarch64_output_sve_cnt_pat_immediate ("<inc_dec>", "%x0",
						 operands + 2);
  }
)

;; Decrement an SImode register by the number of elements in an svpattern
;; using modular arithmetic.  See aarch64_sve_cnt_pat for the counting
;; behavior.
(define_insn "*aarch64_sve_decsi_pat"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(minus:SI (match_operand:SI 1 "register_operand" "0")
		  (unspec:SI [(match_operand:DI 2 "const_int_operand")
			      (match_operand:DI 3 "const_int_operand")
			      (match_operand:DI 4 "const_int_operand")]
			     UNSPEC_SVE_CNT_PAT)))]
  "TARGET_SVE"
  {
    return aarch64_output_sve_cnt_pat_immediate ("dec", "%x0", operands + 2);
  }
)

;; Decrement an SImode register by the number of elements in an svpattern
;; using saturating arithmetic, extending the result to 64 bits.
;;
;; See aarch64_sve_cnt_pat for the counting behavior.
(define_insn "@aarch64_sve_<inc_dec><mode>_pat"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(<paired_extend>:DI
	  (SAT_MINUS:SI
	    (match_operand:SI_ONLY 1 "register_operand" "0")
	    (unspec:SI [(match_operand:DI 2 "const_int_operand")
			(match_operand:DI 3 "const_int_operand")
			(match_operand:DI 4 "const_int_operand")]
		       UNSPEC_SVE_CNT_PAT))))]
  "TARGET_SVE"
  {
    const char *registers = (<CODE> == SS_MINUS ? "%x0, %w0" : "%w0");
    return aarch64_output_sve_cnt_pat_immediate ("<inc_dec>", registers,
						 operands + 2);
  }
)

;; -------------------------------------------------------------------------
;; ---- [INT] Decrement by the number of elements in a pattern (vector)
;; -------------------------------------------------------------------------
;; Includes:
;; - DEC
;; - SQDEC
;; - UQDEC
;; -------------------------------------------------------------------------

;; Decrement a vector of DIs by the number of elements in an svpattern.
;; See aarch64_sve_cnt_pat for the counting behavior.
(define_insn "@aarch64_sve_<inc_dec><mode>_pat"
  [(set (match_operand:VNx2DI 0 "register_operand" "=w, ?&w")
	(ANY_MINUS:VNx2DI
	  (match_operand:VNx2DI_ONLY 1 "register_operand" "0, w")
	  (vec_duplicate:VNx2DI
	    (zero_extend:DI
	      (unspec:SI [(match_operand:DI 2 "const_int_operand")
			  (match_operand:DI 3 "const_int_operand")
			  (match_operand:DI 4 "const_int_operand")]
			 UNSPEC_SVE_CNT_PAT)))))]
  "TARGET_SVE"
  {
    if (which_alternative == 1)
      output_asm_insn ("movprfx\t%0, %1", operands);
    return aarch64_output_sve_cnt_pat_immediate ("<inc_dec>", "%0.<Vetype>",
						 operands + 2);
  }
  [(set_attr "movprfx" "*,yes")]
)

;; Decrement a vector of SIs by the number of elements in an svpattern.
;; See aarch64_sve_cnt_pat for the counting behavior.
(define_insn "@aarch64_sve_<inc_dec><mode>_pat"
  [(set (match_operand:VNx4SI 0 "register_operand" "=w, ?&w")
	(ANY_MINUS:VNx4SI
	  (match_operand:VNx4SI_ONLY 1 "register_operand" "0, w")
	  (vec_duplicate:VNx4SI
	    (unspec:SI [(match_operand:DI 2 "const_int_operand")
			(match_operand:DI 3 "const_int_operand")
			(match_operand:DI 4 "const_int_operand")]
		       UNSPEC_SVE_CNT_PAT))))]
  "TARGET_SVE"
  {
    if (which_alternative == 1)
      output_asm_insn ("movprfx\t%0, %1", operands);
    return aarch64_output_sve_cnt_pat_immediate ("<inc_dec>", "%0.<Vetype>",
						 operands + 2);
  }
  [(set_attr "movprfx" "*,yes")]
)

;; Decrement a vector of HIs by the number of elements in an svpattern.
;; See aarch64_sve_cnt_pat for the counting behavior.
(define_expand "@aarch64_sve_<inc_dec><mode>_pat"
  [(set (match_operand:VNx8HI 0 "register_operand")
	(ANY_MINUS:VNx8HI
	  (match_operand:VNx8HI_ONLY 1 "register_operand")
	  (vec_duplicate:VNx8HI
	    (truncate:HI
	      (unspec:SI [(match_operand:DI 2 "const_int_operand")
			  (match_operand:DI 3 "const_int_operand")
			  (match_operand:DI 4 "const_int_operand")]
			 UNSPEC_SVE_CNT_PAT)))))]
  "TARGET_SVE"
)

(define_insn "*aarch64_sve_<inc_dec><mode>_pat"
  [(set (match_operand:VNx8HI 0 "register_operand" "=w, ?&w")
	(ANY_MINUS:VNx8HI
	  (match_operand:VNx8HI_ONLY 1 "register_operand" "0, w")
	  (vec_duplicate:VNx8HI
	    (match_operator:HI 5 "subreg_lowpart_operator"
	      [(unspec:SI [(match_operand:DI 2 "const_int_operand")
			   (match_operand:DI 3 "const_int_operand")
			   (match_operand:DI 4 "const_int_operand")]
			  UNSPEC_SVE_CNT_PAT)]))))]
  "TARGET_SVE"
  {
    if (which_alternative == 1)
      output_asm_insn ("movprfx\t%0, %1", operands);
    return aarch64_output_sve_cnt_pat_immediate ("<inc_dec>", "%0.<Vetype>",
						 operands + 2);
  }
  [(set_attr "movprfx" "*,yes")]
)

;; -------------------------------------------------------------------------
;; ---- [INT] Count elements in a predicate (scalar)
;; -------------------------------------------------------------------------
;; Includes:
;; - CNTP
;; -------------------------------------------------------------------------

;; Count the number of set bits in a predicate.  Operand 3 is true if
;; operand 1 is known to be all-true.
(define_insn "@aarch64_pred_cntp<mode>"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI
	  (unspec:SI [(match_operand:PRED_ALL 1 "register_operand" "Upl")
		      (match_operand:SI 2 "aarch64_sve_ptrue_flag")
		      (match_operand:PRED_ALL 3 "register_operand" "Upa")]
		     UNSPEC_CNTP)))]
  "TARGET_SVE"
  "cntp\t%x0, %1, %3.<Vetype>")

;; -------------------------------------------------------------------------
;; ---- [INT] Increment by the number of elements in a predicate (scalar)
;; -------------------------------------------------------------------------
;; Includes:
;; - INCP
;; - SQINCP
;; - UQINCP
;; -------------------------------------------------------------------------

;; Increment a DImode register by the number of set bits in a predicate.
;; See aarch64_sve_cntp for a description of the operands.
(define_expand "@aarch64_sve_<inc_dec><DI_ONLY:mode><PRED_ALL:mode>_cntp"
  [(set (match_operand:DI 0 "register_operand")
	(ANY_PLUS:DI
	  (zero_extend:DI
	    (unspec:SI [(match_dup 3)
			(const_int SVE_KNOWN_PTRUE)
			(match_operand:PRED_ALL 2 "register_operand")]
		       UNSPEC_CNTP))
	  (match_operand:DI_ONLY 1 "register_operand")))]
  "TARGET_SVE"
  {
    operands[3] = CONSTM1_RTX (<PRED_ALL:MODE>mode);
  }
)

(define_insn_and_rewrite "*aarch64_sve_<inc_dec><DI_ONLY:mode><PRED_ALL:mode>_cntp"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ANY_PLUS:DI
	  (zero_extend:DI
	    (unspec:SI [(match_operand 3)
			(const_int SVE_KNOWN_PTRUE)
			(match_operand:PRED_ALL 2 "register_operand" "Upa")]
		       UNSPEC_CNTP))
	  (match_operand:DI_ONLY 1 "register_operand" "0")))]
  "TARGET_SVE"
  "<inc_dec>p\t%x0, %2.<PRED_ALL:Vetype>"
  "&& !CONSTANT_P (operands[3])"
  {
    operands[3] = CONSTM1_RTX (<PRED_ALL:MODE>mode);
  }
)

;; Increment an SImode register by the number of set bits in a predicate
;; using modular arithmetic.  See aarch64_sve_cntp for a description of
;; the operands.
(define_insn_and_rewrite "*aarch64_incsi<mode>_cntp"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI
	  (unspec:SI [(match_operand 3)
		      (const_int SVE_KNOWN_PTRUE)
		      (match_operand:PRED_ALL 2 "register_operand" "Upa")]
		     UNSPEC_CNTP)
	  (match_operand:SI 1 "register_operand" "0")))]
  "TARGET_SVE"
  "incp\t%x0, %2.<Vetype>"
  "&& !CONSTANT_P (operands[3])"
  {
    operands[3] = CONSTM1_RTX (<MODE>mode);
  }
)

;; Increment an SImode register by the number of set bits in a predicate
;; using saturating arithmetic, extending the result to 64 bits.
;;
;; See aarch64_sve_cntp for a description of the operands.
(define_expand "@aarch64_sve_<inc_dec><SI_ONLY:mode><PRED_ALL:mode>_cntp"
  [(set (match_operand:DI 0 "register_operand")
	(<paired_extend>:DI
	  (SAT_PLUS:SI
	    (unspec:SI [(match_dup 3)
			(const_int SVE_KNOWN_PTRUE)
			(match_operand:PRED_ALL 2 "register_operand")]
		       UNSPEC_CNTP)
	    (match_operand:SI_ONLY 1 "register_operand"))))]
  "TARGET_SVE"
  {
    operands[3] = CONSTM1_RTX (<PRED_ALL:MODE>mode);
  }
)

(define_insn_and_rewrite "*aarch64_sve_<inc_dec><SI_ONLY:mode><PRED_ALL:mode>_cntp"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(<paired_extend>:DI
	  (SAT_PLUS:SI
	    (unspec:SI [(match_operand 3)
			(const_int SVE_KNOWN_PTRUE)
			(match_operand:PRED_ALL 2 "register_operand" "Upa")]
		       UNSPEC_CNTP)
	    (match_operand:SI_ONLY 1 "register_operand" "0"))))]
  "TARGET_SVE"
  {
    if (<CODE> == SS_PLUS)
      return "<inc_dec>p\t%x0, %2.<PRED_ALL:Vetype>, %w0";
    else
      return "<inc_dec>p\t%w0, %2.<PRED_ALL:Vetype>";
  }
  "&& !CONSTANT_P (operands[3])"
  {
    operands[3] = CONSTM1_RTX (<PRED_ALL:MODE>mode);
  }
)

;; -------------------------------------------------------------------------
;; ---- [INT] Increment by the number of elements in a predicate (vector)
;; -------------------------------------------------------------------------
;; Includes:
;; - INCP
;; - SQINCP
;; - UQINCP
;; -------------------------------------------------------------------------

;; Increment a vector of DIs by the number of set bits in a predicate.
;; See aarch64_sve_cntp for a description of the operands.
(define_expand "@aarch64_sve_<inc_dec><mode>_cntp"
  [(set (match_operand:VNx2DI 0 "register_operand")
	(ANY_PLUS:VNx2DI
	  (vec_duplicate:VNx2DI
	    (zero_extend:DI
	      (unspec:SI
		[(match_dup 3)
		 (const_int SVE_KNOWN_PTRUE)
		 (match_operand:<VPRED> 2 "register_operand")]
		UNSPEC_CNTP)))
	  (match_operand:VNx2DI_ONLY 1 "register_operand")))]
  "TARGET_SVE"
  {
    operands[3] = CONSTM1_RTX (<VPRED>mode);
  }
)

(define_insn_and_rewrite "*aarch64_sve_<inc_dec><mode>_cntp"
  [(set (match_operand:VNx2DI 0 "register_operand")
	(ANY_PLUS:VNx2DI
	  (vec_duplicate:VNx2DI
	    (zero_extend:DI
	      (unspec:SI
		[(match_operand 3)
		 (const_int SVE_KNOWN_PTRUE)
		 (match_operand:<VPRED> 2 "register_operand")]
		UNSPEC_CNTP)))
	  (match_operand:VNx2DI_ONLY 1 "register_operand")))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1 , 2   ; attrs: movprfx ]
     [ w        , 0 , Upa ; *              ] <inc_dec>p\t%0.d, %2
     [ ?&w      , w , Upa ; yes            ] movprfx\t%0, %1\;<inc_dec>p\t%0.d, %2
  }
  "&& !CONSTANT_P (operands[3])"
  {
    operands[3] = CONSTM1_RTX (<VPRED>mode);
  }
)

;; Increment a vector of SIs by the number of set bits in a predicate.
;; See aarch64_sve_cntp for a description of the operands.
(define_expand "@aarch64_sve_<inc_dec><mode>_cntp"
  [(set (match_operand:VNx4SI 0 "register_operand")
	(ANY_PLUS:VNx4SI
	  (vec_duplicate:VNx4SI
	    (unspec:SI
	      [(match_dup 3)
	       (const_int SVE_KNOWN_PTRUE)
	       (match_operand:<VPRED> 2 "register_operand")]
	      UNSPEC_CNTP))
	  (match_operand:VNx4SI_ONLY 1 "register_operand")))]
  "TARGET_SVE"
  {
    operands[3] = CONSTM1_RTX (<VPRED>mode);
  }
)

(define_insn_and_rewrite "*aarch64_sve_<inc_dec><mode>_cntp"
  [(set (match_operand:VNx4SI 0 "register_operand")
	(ANY_PLUS:VNx4SI
	  (vec_duplicate:VNx4SI
	    (unspec:SI
	      [(match_operand 3)
	       (const_int SVE_KNOWN_PTRUE)
	       (match_operand:<VPRED> 2 "register_operand")]
	      UNSPEC_CNTP))
	  (match_operand:VNx4SI_ONLY 1 "register_operand")))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1 , 2   ; attrs: movprfx ]
     [ w        , 0 , Upa ; *              ] <inc_dec>p\t%0.s, %2
     [ ?&w      , w , Upa ; yes            ] movprfx\t%0, %1\;<inc_dec>p\t%0.s, %2
  }
  "&& !CONSTANT_P (operands[3])"
  {
    operands[3] = CONSTM1_RTX (<VPRED>mode);
  }
)

;; Increment a vector of HIs by the number of set bits in a predicate.
;; See aarch64_sve_cntp for a description of the operands.
(define_expand "@aarch64_sve_<inc_dec><mode>_cntp"
  [(set (match_operand:VNx8HI 0 "register_operand")
	(ANY_PLUS:VNx8HI
	  (vec_duplicate:VNx8HI
	    (truncate:HI
	      (unspec:SI
		[(match_dup 3)
		 (const_int SVE_KNOWN_PTRUE)
		 (match_operand:<VPRED> 2 "register_operand")]
		UNSPEC_CNTP)))
	  (match_operand:VNx8HI_ONLY 1 "register_operand")))]
  "TARGET_SVE"
  {
    operands[3] = CONSTM1_RTX (<VPRED>mode);
  }
)

(define_insn_and_rewrite "*aarch64_sve_<inc_dec><mode>_cntp"
  [(set (match_operand:VNx8HI 0 "register_operand")
	(ANY_PLUS:VNx8HI
	  (vec_duplicate:VNx8HI
	    (match_operator:HI 3 "subreg_lowpart_operator"
	      [(unspec:SI
		 [(match_operand 4)
		  (const_int SVE_KNOWN_PTRUE)
		  (match_operand:<VPRED> 2 "register_operand")]
		 UNSPEC_CNTP)]))
	  (match_operand:VNx8HI_ONLY 1 "register_operand")))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1 , 2   ; attrs: movprfx ]
     [ w        , 0 , Upa ; *              ] <inc_dec>p\t%0.h, %2
     [ ?&w      , w , Upa ; yes            ] movprfx\t%0, %1\;<inc_dec>p\t%0.h, %2
  }
  "&& !CONSTANT_P (operands[4])"
  {
    operands[4] = CONSTM1_RTX (<VPRED>mode);
  }
)

;; -------------------------------------------------------------------------
;; ---- [INT] Decrement by the number of elements in a predicate (scalar)
;; -------------------------------------------------------------------------
;; Includes:
;; - DECP
;; - SQDECP
;; - UQDECP
;; -------------------------------------------------------------------------

;; Decrement a DImode register by the number of set bits in a predicate.
;; See aarch64_sve_cntp for a description of the operands.
(define_expand "@aarch64_sve_<inc_dec><DI_ONLY:mode><PRED_ALL:mode>_cntp"
  [(set (match_operand:DI 0 "register_operand")
	(ANY_MINUS:DI
	  (match_operand:DI_ONLY 1 "register_operand")
	  (zero_extend:DI
	    (unspec:SI [(match_dup 3)
			(const_int SVE_KNOWN_PTRUE)
			(match_operand:PRED_ALL 2 "register_operand")]
		       UNSPEC_CNTP))))]
  "TARGET_SVE"
  {
    operands[3] = CONSTM1_RTX (<PRED_ALL:MODE>mode);
  }
)

(define_insn_and_rewrite "*aarch64_sve_<inc_dec><DI_ONLY:mode><PRED_ALL:mode>_cntp"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ANY_MINUS:DI
	  (match_operand:DI_ONLY 1 "register_operand" "0")
	  (zero_extend:DI
	    (unspec:SI [(match_operand 3)
			(const_int SVE_KNOWN_PTRUE)
			(match_operand:PRED_ALL 2 "register_operand" "Upa")]
		       UNSPEC_CNTP))))]
  "TARGET_SVE"
  "<inc_dec>p\t%x0, %2.<PRED_ALL:Vetype>"
  "&& !CONSTANT_P (operands[3])"
  {
    operands[3] = CONSTM1_RTX (<PRED_ALL:MODE>mode);
  }
)

;; Decrement an SImode register by the number of set bits in a predicate
;; using modular arithmetic.  See aarch64_sve_cntp for a description of the
;; operands.
(define_insn_and_rewrite "*aarch64_decsi<mode>_cntp"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(minus:SI
	  (match_operand:SI 1 "register_operand" "0")
	  (unspec:SI [(match_operand 3)
		      (const_int SVE_KNOWN_PTRUE)
		      (match_operand:PRED_ALL 2 "register_operand" "Upa")]
		     UNSPEC_CNTP)))]
  "TARGET_SVE"
  "decp\t%x0, %2.<Vetype>"
  "&& !CONSTANT_P (operands[3])"
  {
    operands[3] = CONSTM1_RTX (<MODE>mode);
  }
)

;; Decrement an SImode register by the number of set bits in a predicate
;; using saturating arithmetic, extending the result to 64 bits.
;;
;; See aarch64_sve_cntp for a description of the operands.
(define_expand "@aarch64_sve_<inc_dec><SI_ONLY:mode><PRED_ALL:mode>_cntp"
  [(set (match_operand:DI 0 "register_operand")
	(<paired_extend>:DI
	  (SAT_MINUS:SI
	    (match_operand:SI_ONLY 1 "register_operand")
	    (unspec:SI [(match_dup 3)
			(const_int SVE_KNOWN_PTRUE)
			(match_operand:PRED_ALL 2 "register_operand")]
		       UNSPEC_CNTP))))]
  "TARGET_SVE"
  {
    operands[3] = CONSTM1_RTX (<PRED_ALL:MODE>mode);
  }
)

(define_insn_and_rewrite "*aarch64_sve_<inc_dec><SI_ONLY:mode><PRED_ALL:mode>_cntp"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(<paired_extend>:DI
	  (SAT_MINUS:SI
	    (match_operand:SI_ONLY 1 "register_operand" "0")
	    (unspec:SI [(match_operand 3)
			(const_int SVE_KNOWN_PTRUE)
			(match_operand:PRED_ALL 2 "register_operand" "Upa")]
		       UNSPEC_CNTP))))]
  "TARGET_SVE"
  {
    if (<CODE> == SS_MINUS)
      return "<inc_dec>p\t%x0, %2.<PRED_ALL:Vetype>, %w0";
    else
      return "<inc_dec>p\t%w0, %2.<PRED_ALL:Vetype>";
  }
  "&& !CONSTANT_P (operands[3])"
  {
    operands[3] = CONSTM1_RTX (<PRED_ALL:MODE>mode);
  }
)

;; -------------------------------------------------------------------------
;; ---- [INT] Decrement by the number of elements in a predicate (vector)
;; -------------------------------------------------------------------------
;; Includes:
;; - DECP
;; - SQDECP
;; - UQDECP
;; -------------------------------------------------------------------------

;; Decrement a vector of DIs by the number of set bits in a predicate.
;; See aarch64_sve_cntp for a description of the operands.
(define_expand "@aarch64_sve_<inc_dec><mode>_cntp"
  [(set (match_operand:VNx2DI 0 "register_operand")
	(ANY_MINUS:VNx2DI
	  (match_operand:VNx2DI_ONLY 1 "register_operand")
	  (vec_duplicate:VNx2DI
	    (zero_extend:DI
	      (unspec:SI
		[(match_dup 3)
		 (const_int SVE_KNOWN_PTRUE)
		 (match_operand:<VPRED> 2 "register_operand")]
		UNSPEC_CNTP)))))]
  "TARGET_SVE"
  {
    operands[3] = CONSTM1_RTX (<VPRED>mode);
  }
)

(define_insn_and_rewrite "*aarch64_sve_<inc_dec><mode>_cntp"
  [(set (match_operand:VNx2DI 0 "register_operand")
	(ANY_MINUS:VNx2DI
	  (match_operand:VNx2DI_ONLY 1 "register_operand")
	  (vec_duplicate:VNx2DI
	    (zero_extend:DI
	      (unspec:SI
		[(match_operand 3)
		 (const_int SVE_KNOWN_PTRUE)
		 (match_operand:<VPRED> 2 "register_operand")]
		UNSPEC_CNTP)))))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1 , 2   ; attrs: movprfx ]
     [ w        , 0 , Upa ; *              ] <inc_dec>p\t%0.d, %2
     [ ?&w      , w , Upa ; yes            ] movprfx\t%0, %1\;<inc_dec>p\t%0.d, %2
  }
  "&& !CONSTANT_P (operands[3])"
  {
    operands[3] = CONSTM1_RTX (<VPRED>mode);
  }
)

;; Decrement a vector of SIs by the number of set bits in a predicate.
;; See aarch64_sve_cntp for a description of the operands.
(define_expand "@aarch64_sve_<inc_dec><mode>_cntp"
  [(set (match_operand:VNx4SI 0 "register_operand")
	(ANY_MINUS:VNx4SI
	  (match_operand:VNx4SI_ONLY 1 "register_operand")
	  (vec_duplicate:VNx4SI
	    (unspec:SI
	      [(match_dup 3)
	       (const_int SVE_KNOWN_PTRUE)
	       (match_operand:<VPRED> 2 "register_operand")]
	      UNSPEC_CNTP))))]
  "TARGET_SVE"
  {
    operands[3] = CONSTM1_RTX (<VPRED>mode);
  }
)

(define_insn_and_rewrite "*aarch64_sve_<inc_dec><mode>_cntp"
  [(set (match_operand:VNx4SI 0 "register_operand")
	(ANY_MINUS:VNx4SI
	  (match_operand:VNx4SI_ONLY 1 "register_operand")
	  (vec_duplicate:VNx4SI
	    (unspec:SI
	      [(match_operand 3)
	       (const_int SVE_KNOWN_PTRUE)
	       (match_operand:<VPRED> 2 "register_operand")]
	      UNSPEC_CNTP))))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1 , 2   ; attrs: movprfx ]
     [ w        , 0 , Upa ; *              ] <inc_dec>p\t%0.s, %2
     [ ?&w      , w , Upa ; yes            ] movprfx\t%0, %1\;<inc_dec>p\t%0.s, %2
  }
  "&& !CONSTANT_P (operands[3])"
  {
    operands[3] = CONSTM1_RTX (<VPRED>mode);
  }
)

;; Decrement a vector of HIs by the number of set bits in a predicate.
;; See aarch64_sve_cntp for a description of the operands.
(define_expand "@aarch64_sve_<inc_dec><mode>_cntp"
  [(set (match_operand:VNx8HI 0 "register_operand")
	(ANY_MINUS:VNx8HI
	  (match_operand:VNx8HI_ONLY 1 "register_operand")
	  (vec_duplicate:VNx8HI
	    (truncate:HI
	      (unspec:SI
		[(match_dup 3)
		 (const_int SVE_KNOWN_PTRUE)
		 (match_operand:<VPRED> 2 "register_operand")]
		UNSPEC_CNTP)))))]
  "TARGET_SVE"
  {
    operands[3] = CONSTM1_RTX (<VPRED>mode);
  }
)

(define_insn_and_rewrite "*aarch64_sve_<inc_dec><mode>_cntp"
  [(set (match_operand:VNx8HI 0 "register_operand")
	(ANY_MINUS:VNx8HI
	  (match_operand:VNx8HI_ONLY 1 "register_operand")
	  (vec_duplicate:VNx8HI
	    (match_operator:HI 3 "subreg_lowpart_operator"
	      [(unspec:SI
		 [(match_operand 4)
		  (const_int SVE_KNOWN_PTRUE)
		  (match_operand:<VPRED> 2 "register_operand")]
		 UNSPEC_CNTP)]))))]
  "TARGET_SVE"
  {@ [ cons: =0 , 1 , 2   ; attrs: movprfx ]
     [ w        , 0 , Upa ; *              ] <inc_dec>p\t%0.h, %2
     [ ?&w      , w , Upa ; yes            ] movprfx\t%0, %1\;<inc_dec>p\t%0.h, %2
  }
  "&& !CONSTANT_P (operands[4])"
  {
    operands[4] = CONSTM1_RTX (<VPRED>mode);
  }
)

(define_insn_and_split "@aarch64_sve_get_neonq_<mode>"
  [(set (match_operand:<V128> 0 "register_operand" "=w")
	  (vec_select:<V128>
	    (match_operand:SVE_FULL 1 "register_operand" "w")
	    (match_operand 2 "descending_int_parallel")))]
  "TARGET_SVE
   && BYTES_BIG_ENDIAN
   && known_eq (INTVAL (XVECEXP (operands[2], 0, 0)),
		GET_MODE_NUNITS (<V128>mode) - 1)"
  "#"
  "&& reload_completed"
  [(set (match_dup 0) (match_dup 1))]
  {
    operands[1] = gen_rtx_REG (<V128>mode, REGNO (operands[1]));
  }
)

(define_insn "@aarch64_sve_set_neonq_<mode>"
  [(set (match_operand:SVE_FULL 0 "register_operand" "=w")
      (unspec:SVE_FULL
	[(match_operand:SVE_FULL 1 "register_operand" "w")
	(match_operand:<V128> 2 "register_operand" "w")
	(match_operand:<VPRED> 3 "register_operand" "Upl")]
	UNSPEC_SET_NEONQ))]
  "TARGET_SVE
   && BYTES_BIG_ENDIAN"
  {
    operands[2] = lowpart_subreg (<MODE>mode, operands[2],
                                  GET_MODE (operands[2]));
    return "sel\t%0.<Vetype>, %3, %2.<Vetype>, %1.<Vetype>";
  }
)
