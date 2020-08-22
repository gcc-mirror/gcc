;; GCC machine description for CRIS cpu cores.
;; Copyright (C) 1998-2020 Free Software Foundation, Inc.
;; Contributed by Axis Communications.

;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;; The original PO technology requires these to be ordered by speed,
;; so that assigner will pick the fastest.

;; See files "md.texi" and "rtl.def" for documentation on define_insn,
;; match_*, et. al.

;; There are several instructions that are orthogonal in size, and seems
;; they could be matched by a single pattern without a specified size
;; for the operand that is orthogonal.  However, this did not work on
;; gcc-2.7.2 (and probably not on gcc-2.8.1), relating to that when a
;; constant is substituted into an operand, the actual mode must be
;; deduced from the pattern.  There is reasonable hope that that has been
;; fixed, so FIXME: try again.

;; You will notice that three-operand alternatives ("=r", "r", "!To")
;; are marked with a "!" constraint modifier to avoid being reloaded
;; into.  This is because gcc would otherwise prefer to use the constant
;; pool and its offsettable address instead of reloading to an
;; ("=r", "0", "i") alternative.  Also, the constant-pool support was not
;; only suboptimal but also buggy in 2.7.2, ??? maybe only in 2.6.3.

;; All insns that look like (set (...) (plus (...) (reg:SI 8)))
;; get problems when reloading r8 (frame pointer) to r14 + offs (stack
;; pointer).  Thus the instructions that get into trouble have specific
;; checks against matching frame_pointer_rtx.
;; ??? But it should be re-checked for gcc > 2.7.2
;; FIXME: This changed some time ago (from 2000-03-16) for gcc-2.9x.

(define_c_enum ""
  [
   ;; Stack frame deallocation barrier.
   CRIS_UNSPEC_FRAME_DEALLOC

   ;; Swap all 32 bits of the operand; 31 <=> 0, 30 <=> 1...
   CRIS_UNSPEC_SWAP_BITS
  ])

;; Register numbers.
(define_constants
  [(CRIS_STATIC_CHAIN_REGNUM 7)
   (CRIS_FP_REGNUM 8)
   (CRIS_SP_REGNUM 14)
   (CRIS_ACR_REGNUM 15)
   (CRIS_SRP_REGNUM 16)
   (CRIS_MOF_REGNUM 17)
   (CRIS_AP_REGNUM 18)
   (CRIS_CC0_REGNUM 19)]
)

;; We need an attribute to define whether an instruction can be put in
;; a branch-delay slot or not, and whether it has a delay slot.
;;
;; Branches and return instructions have a delay slot, and cannot
;; themselves be put in a delay slot.  This has changed *for short
;; branches only* between architecture variants, but the possible win
;; is presumed negligible compared to the added complexity of the machine
;; description: one would have to add always-correct infrastructure to
;; distinguish short branches.
;;
;; Whether an instruction can be put in a delay slot depends on the
;; instruction (all short instructions except jumps and branches)
;; and the addressing mode (must not be prefixed or referring to pc).
;; In short, any "slottable" instruction must be 16 bit and not refer
;; to pc, or alter it.
;;
;; The possible values are "yes", "no", "has_slot", and "has_return_slot".
;; Yes/no tells whether the insn is slottable or not.
;; Of special concern is that no RTX_FRAME_RELATED insn must go in that
;; call delay slot, as it's located in the address *after* the call insn,
;; and the unwind machinery doesn't know about delay slots.
;; Has_slot means that the insn is a branch insn (which are
;; not considered slottable since that is generally true).  Having the
;; seemingly illogical value "has_slot" means we do not have to add
;; another attribute just to say that an insn has a delay-slot, since it
;; also infers that it is not slottable.  Better names for the attribute
;; were found to be longer and not add readability to the machine
;; description.
;; Has_return_slot is similar, for the return insn.
;;
;; The default that is defined here for this attribute is "no", not
;; slottable, not having a delay-slot, so there's no need to worry about
;; it being wrong for non-branch and return instructions.
;;  The default could depend on the kind of insn and the addressing
;; mode, but that would need more attributes and hairier, more error
;; prone code.
;;
;;  There is an extra memory constraint, 'Q', which recognizes an indirect
;; register.  The constraints 'Q' and '>' together match all possible
;; memory operands that are slottable.
;;  For other operands, you need to check if it has a valid "slottable"
;; quick-immediate operand, where the particular signedness-variation
;; may match the constraints 'I' or 'J'.), and include it in the
;; constraint pattern for the slottable pattern.  An alternative using
;; only "r" constraints is most often slottable.

(define_attr "slottable" "no,yes,has_slot,has_return_slot"
  (const_string "no"))

;; We also need attributes to sanely determine the condition code
;; state.  This attribute isn't used as-is, just as a template,
;; effectively a dummy except in a substitution setting CRIS_CC0_REGNUM
;; to a specific value.
(define_attr "cc" "none,clobber,normal" (const_string "normal"))

;; The attribute "_enabled" is appended to "cc", forming "cc_enabled" to
;; pick out certain alternatives when generating a useful
;; condition-code-setting.  See the "enabled" attribute.
(define_attr "cc_enabled" "none,clobber,normal" (const_string "normal"))

;; At the moment, this attribute is just used to help bb-reorder do its
;; work; the default 0 doesn't help it.  Many insns have other lengths,
;; though none are shorter.
(define_attr "length" "" (const_int 2))

;; A branch has one delay-slot.  The instruction in the
;; delay-slot is always executed, independent of whether the branch is
;; taken or not.  Note that besides setting "slottable" to "has_slot",
;; there also has to be a "%#" at the end of a "delayed" instruction
;; output pattern (for "jump" this means "ba %l0%#"), so print_operand can
;; catch it and print a "nop" if necessary.  This method was stolen from
;; sparc.md.

(define_delay (eq_attr "slottable" "has_slot")
  [(eq_attr "slottable" "yes") (nil) (nil)])

;; The insn in the return insn slot must not be the
;; return-address-register restore.  FIXME: Use has_slot and express
;; as a parallel with a use of the return-address-register (currently
;; only SRP).  However, this requires an amount of fixing tests for
;; naked RETURN in middle-end.
(define_delay (eq_attr "slottable" "has_return_slot")
  [(and (eq_attr "slottable" "yes")
	(not (match_test "dead_or_set_regno_p (insn, CRIS_SRP_REGNUM)")))
   (nil) (nil)])

(define_attr "enabled" "no,yes"
  (if_then_else
   (eq_attr "cc_enabled" "normal")
   (const_string "yes")
   (const_string "no")))

;; Iterator definitions.

;; For the "usual" pattern size alternatives.
(define_mode_iterator BWD [SI HI QI])
(define_mode_iterator BWDD [DI SI HI QI])

;; To be able to refer to the same mode_attr for both a multi-mode
;; and a mode-specific pattern, we use some singleton iterators.
(define_mode_iterator DI_ [DI])
(define_mode_iterator SI_ [SI])

(define_mode_iterator WD [SI HI])
(define_mode_iterator BW [HI QI])
(define_mode_attr S [(SI "HI") (HI "QI")])
(define_mode_attr s [(SI "hi") (HI "qi")])
(define_mode_attr m [(SI ".d") (HI ".w") (QI ".b")])
(define_mode_attr mm [(SI ".w") (HI ".b")])
(define_mode_attr nbitsm1 [(SI "31") (HI "15") (QI "7")])

;; For the sign_extend+zero_extend variants.
(define_code_iterator szext [sign_extend zero_extend])
(define_code_attr u [(sign_extend "") (zero_extend "u")])
(define_code_attr su [(sign_extend "s") (zero_extend "u")])

;; For extended-operand variants.
(define_code_iterator plusminus [plus minus])
(define_code_attr addsub [(plus "add") (minus "sub")])

;; Similar, other cases also matching bound/umin.
(define_code_iterator plusminusumin [plus minus umin])

;; Ditto, commutative operators (i.e. not minus).
(define_code_iterator plusumin [plus umin])

;; The addsubbo and nd code-attributes form a hack.  We need to output
;; "addu.b", "subu.b" but "bound.b" (no "u"-suffix) which means we'd
;; need to refer to one iterator from the next.  But, that can't be
;; done.  Instead output the "u" for unsigned as the "u" in "bound",
;; i.e. the mnemonic as three parts including the extend-letter, and
;; with an empty third part for "add" and "sub".
(define_code_attr addsubbo [(plus "add") (minus "sub") (umin "bo")])
(define_code_attr nd [(plus "") (minus "") (umin "nd")])

;; For the shift variants.
(define_code_iterator shift [ashiftrt lshiftrt ashift])
(define_code_iterator shiftrt [ashiftrt lshiftrt])
(define_code_attr shlr [(ashiftrt "ashr") (lshiftrt "lshr") (ashift "ashl")])
(define_code_attr slr [(ashiftrt "asr") (lshiftrt "lsr") (ashift "lsl")])

;; Compares, branches, cbranch, cstore.  Conditions gt and le are CC_NZVC.
;; Others start out as CCmode and can degenerate to CC_NZmode.
;; Incidental setters are either CC_NZVCmode or CC_NZmode.  See also
;; cris-modes.def.
(define_mode_iterator NZSET [CC_NZ])
(define_mode_iterator NZUSE [CC CC_NZ CC_NZVC])
(define_mode_iterator NZVCSET [CC CC_NZVC CC_NZ])
(define_mode_iterator NZVCUSE [CC_NZVC])
(define_mode_iterator ZnNNZSET [CC_ZnN CC_NZ])
(define_mode_iterator ZnNNZUSE [CC CC_ZnN CC_NZ CC_NZVC])

;; All conditions.
(define_code_iterator cond [eq ne gtu ltu geu leu gt le lt ge])

;; Just equal and not equal.
(define_code_iterator zcond [eq ne])

;; Conditions that look only at Z and/or N (or can do with that).
(define_code_iterator nzcond [eq ne gtu leu lt ge])

;; The complement of nzcond within cond; conditions that look (also) on V
;; or C.
(define_code_iterator nzvccond [geu ltu gt le])

;; Within nzcond, those that give different opcodes when operands are
;; reversed or that can ignore V or C.  Also, the complement of zcond
;; within nzcond.
(define_code_iterator rnzcond [gtu leu lt ge])

;; CRIS condition mnemonic.
(define_code_attr CC [(eq "eq") (ne "ne") (gt "gt") (gtu "hi") (lt "lt")
		      (ltu "lo") (ge "ge") (geu "hs") (le "le") (leu "ls")])

;; CRIS reverse condition mnemonic.
(define_code_attr rCC [(eq "ne") (ne "eq") (gt "le") (gtu "ls") (lt "ge")
		       (ltu "hs") (ge "lt") (geu "lo") (le "gt") (leu "hi")])

;; Mnemomic for the CRIS condition when V or C can be ignored.
(define_code_attr oCC [(lt "mi") (ge "pl") (gtu "eq") (ltu "ne")])

;; Reverse of oCC.
(define_code_attr roCC [(lt "pl") (ge "mi") (gtu "eq") (ltu "ne")])

;; CC_Z_IN_NOT_N, a.k.a. CC_ZnNmode.
(define_code_attr znnCC [(eq "pl") (ne "mi")])

;;; ...and the reverse
(define_code_attr rznnCC [(eq "mi") (ne "pl")])

;; Required unoptimized CCmode, different for nzcond and nzvccond.
(define_code_attr xCC [(eq "CC") (ne "CC") (gtu "CC") (ltu "CC_NZVC")
		       (geu "CC_NZVC") (leu "CC") (lt "CC") (ge "CC")
		       (gt "CC_NZVC") (le "CC_NZVC")])

;; Substitutions to describe condition-code settings.

(define_subst_attr "setnz" "setnz_subst" "" "_setnz")
(define_subst_attr "ccnz" "setnz_subst" "" "_enabled")
(define_subst_attr "anz" "setnz_subst" "" "*")

(define_subst "setnz_subst"
  [(set (match_operand 0)
	(match_operand 1))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  "reload_completed"
  [(set (reg:CC_NZ CRIS_CC0_REGNUM)
	(compare:CC_NZ (match_dup 1) (const_int 0)))
   (set (match_dup 0) (match_dup 1))])

(define_subst_attr "setnzvc" "setnzvc_subst" "" "_setnzvc")
(define_subst_attr "ccnzvc" "setnzvc_subst" "" "_enabled")
(define_subst_attr "anzvc" "setnzvc_subst" "" "*")

(define_subst "setnzvc_subst"
  [(set (match_operand 0)
	(match_operand 1))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  "reload_completed"
  [(set (reg:CC_NZVC CRIS_CC0_REGNUM)
	(compare:CC_NZVC (match_dup 1) (const_int 0)))
   (set (match_dup 0) (match_dup 1))])

(define_subst_attr "setcc" "setcc_subst" "" "_setcc")
(define_subst_attr "cccc" "setcc_subst" "" "_enabled")
(define_subst_attr "acc" "setcc_subst" "" "*")

(define_subst "setcc_subst"
  [(set (match_operand 0)
	(match_operand 1))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  "reload_completed"
  [(set (reg:CC CRIS_CC0_REGNUM)
	(compare:CC (match_dup 1) (const_int 0)))
   (set (match_dup 0) (match_dup 1))])

;; Operand and operator predicates.

(include "predicates.md")
(include "constraints.md")

;; It seems that the position of the sign-bit and the fact that 0.0 is
;; all 0-bits would make "tstsf" a straight-forward implementation;
;; either "test.d" it for positive/negative or "btstq 30,r" it for
;; zeroness.
;;
;; FIXME: Do that some time.

;; Compare insns.

;; These are used for compare insn, cbranch and cstore.
;; FIXME: Port-local reversing of operands is not done.  Still needed?
;; (It shouldn't be; it should be done as part of register allocation.)
(define_mode_attr sCC_destc
 [(DI "r, r,r,r,r,r,r") (SI "r,r, r,  r,r,r") (HI "r, r,  r,r") (QI "r, r,  r,r")])
(define_mode_attr cmp_op0c
 [(DI "rm,r,r,r,r,r,r") (SI "r,r, rQ>,r,r,m") (HI "r, rQ>,r,m") (QI "r, rQ>,r,m")])
(define_mode_attr cmp_op1c
 [(DI "M,Kc,I,P,n,r,o") (SI "I,rQ>,M, P,g,M") (HI "rQ>,M, g,M") (QI "rQ>,M, g,M")])

;; We could optimize the sizes of the immediate operands for various
;; cases, but that is not worth it because of the very little usage of
;; DImode for anything else but a structure/block-mode.  Just do the
;; obvious stuff for the straight-forward constraint letters.

(define_insn "*cmpdi<NZVCSET:mode>"
  [(set (reg:NZVCSET CRIS_CC0_REGNUM)
	(compare:NZVCSET
	 (match_operand:DI_ 0 "nonimmediate_operand" "<cmp_op0c>")
	 (match_operand:DI_ 1 "general_operand" "<cmp_op1c>")))]
  "reload_completed"
  "@
   test.d %M0\;ax\;test.d %H0
   cmpq %1,%M0\;ax\;cmpq 0,%H0
   cmpq %1,%M0\;ax\;cmpq -1,%H0
   cmp%e1.%z1 %1,%M0\;ax\;cmpq %H1,%H0
   cmp.d %M1,%M0\;ax\;cmp.d %H1,%H0
   cmp.d %M1,%M0\;ax\;cmp.d %H1,%H0
   cmp.d %M1,%M0\;ax\;cmp.d %H1,%H0")

;; Note that compare insns with side effect addressing mode (e.g.):
;;
;; cmp.S [rx=ry+i],rz;
;; cmp.S [%3=%1+%2],%0
;;
;; are *not* usable for gcc since the reloader *does not accept*
;; cc0-changing insns with side-effects other than setting the condition
;; codes.  The reason is that the reload stage *may* cause another insn to
;; be output after the main instruction, in turn invalidating cc0 for the
;; insn using the test.  (This does not apply to the CRIS case, since a
;; reload for output -- move to memory -- does not change the condition
;; code.  Unfortunately we have no way to describe that at the moment.  I
;; think code would improve being in the order of one percent faster.

;; We have cmps and cmpu (compare reg w. sign/zero extended mem).
;; These are mostly useful for compares in SImode, using 8 or 16-bit
;; constants, but sometimes gcc will find its way to use it for other
;; (memory) operands.  Avoid side-effect patterns, though (see above).

(define_insn "*cmp_ext<BW:mode><NZVCSET:mode>"
  [(set (reg:NZVCSET CRIS_CC0_REGNUM)
	(compare:NZVCSET
	 (match_operand:SI 0 "register_operand" "r,r")
	 (match_operator:SI 2 "cris_extend_operator"
			 [(match_operand:BW 1 "memory_operand" "Q>,m")])))]
  "reload_completed"
  "cmp%e2<m> %1,%0"
  [(set_attr "slottable" "yes,no")])

;; The "normal" compare patterns, from SI on.  Special-cases with zero
;; are covered above.

(define_insn "*cmpsi<NZVCSET:mode>"
  [(set (reg:NZVCSET CRIS_CC0_REGNUM)
	(compare:NZVCSET
	 (match_operand:SI_ 0 "nonimmediate_operand" "<cmp_op0c>")
	 (match_operand:SI_ 1 "general_operand" "<cmp_op1c>")))]
  "reload_completed"
  "@
   cmpq %1,%0
   cmp.d %1,%0
   test.d %0
   cmp%e1.%z1 %1,%0
   cmp.d %1,%0
   test.d %0"
  [(set_attr "slottable" "yes,yes,yes,no,no,no")])

(define_insn "*cmp<BW:mode><NZVCSET:mode>"
  [(set (reg:NZVCSET CRIS_CC0_REGNUM)
	(compare:NZVCSET
	 (match_operand:BW 0 "nonimmediate_operand" "<cmp_op0c>")
	 (match_operand:BW 1 "general_operand" "<cmp_op1c>")))]
  "reload_completed"
  "@
   cmp<m> %1,%0
   test<m> %0
   cmp<m> %1,%0
   test<m> %0"
  [(set_attr "slottable" "yes,yes,no,no")])

;; Pattern matching the BTST insn.
;; It is useful for "if (i & val)" constructs, where val is an exact
;; power of 2, or if val + 1 is a power of two, where we check for a bunch
;; of zeros starting at bit 0).

;; SImode.  This mode is the only one needed, since gcc automatically
;; extends subregs for lower-size modes.
(define_insn "*btst<mode>"
  [(set (reg:ZnNNZSET CRIS_CC0_REGNUM)
	(compare:ZnNNZSET
	 (zero_extract:SI
	  (match_operand:SI 0 "nonmemory_operand" "r, r,r, r,r, r,Kp")
	  (match_operand:SI 1 "const_int_operand" "Kc,n,Kc,n,Kc,n,n")
	  (match_operand:SI 2 "nonmemory_operand" "M, M,Kc,n,r, r,r"))
	 (const_int 0)))]
  ;; Either it is a single bit, or consecutive ones starting at 0.
  "reload_completed
   && CONST_INT_P (operands[1])
   && ((operands[1] == const1_rtx && <MODE>mode == CC_ZnNmode)
       || (operands[2] == const0_rtx && <MODE>mode == CC_NZmode))
   && (REG_S_P (operands[0])
       || (operands[1] == const1_rtx
	   && REG_S_P (operands[2])
	   && CONST_INT_P (operands[0])
	   && exact_log2 (INTVAL (operands[0])) >= 0))
   && !TARGET_CCINIT"

;; The next-to-last "&&" condition above should be caught by some kind of
;; canonicalization in gcc, but we can easily help with it here.
;;  It results from expressions of the type
;; "power_of_2_value & (1 << y)".  FIXME: Add testcase.
;;
;; Since there may be codes with tests in on bits (in constant position)
;; beyond the size of a word, handle that by assuming those bits are 0.
;; GCC should handle that, but it's a matter of easily-added belts while
;; having suspenders.

  "@
   btstq (%1-1),%0
   cmpq 0,%0
   btstq %2,%0
   clearf nz
   btst %2,%0
   clearf nz
   cmpq %p0,%2"
 [(set_attr "slottable" "yes")])

;; Move insns.

;; The whole mandatory movdi family is here; expander, "anonymous"
;; recognizer and splitter.  We're forced to have a movdi pattern,
;; although GCC should be able to split it up itself.  Normally it can,
;; but if other insns have DI operands (as is the case here), reload
;; must be able to generate or match a movdi.  many testcases fail at
;; -O3 or -fssa if we don't have this.  FIXME: Fix GCC...  See
;; <URL:http://gcc.gnu.org/ml/gcc-patches/2000-04/msg00104.html>.
;; However, a patch from Richard Kenner (similar to the cause of
;; discussion at the URL above), indicates otherwise.  See
;; <URL:http://gcc.gnu.org/ml/gcc-patches/2000-04/msg00554.html>.
;; The truth has IMO is not been decided yet, so check from time to
;; time by disabling the movdi patterns.

;; To appease testcase gcc.c-torture/execute/920501-2.c (and others) at
;; -O0, we need a movdi as a temporary measure.  Here's how things fail:
;;  A cmpdi RTX needs reloading (global):
;;    (insn 185 326 186 (set (cc0)
;;	    (compare (mem/f:DI (reg/v:SI 22) 0)
;;		(const_int 1 [0x1]))) 4 {cmpdi} (nil)
;;	(nil))
;; Now, reg 22 is reloaded for input address, and the mem is also moved
;; out of the instruction (into a register), since one of the operands
;; must be a register.  Reg 22 is reloaded (into reg 10), and the mem is
;; moved out and synthesized in SImode parts (reg 9, reg 10 - should be ok
;; wrt. overlap).  The bad things happen with the synthesis in
;; emit_move_insn_1; the location where to substitute reg 10 is lost into
;; two new RTX:es, both still having reg 22.  Later on, the left-over reg
;; 22 is recognized to have an equivalent in memory which is substituted
;; straight in, and we end up with an unrecognizable insn:
;;    (insn 325 324 326 (set (reg:SI 9 r9)
;;    	      (mem/f:SI (mem:SI (plus:SI (reg:SI 8 r8)
;;    			  (const_int -84 [0xffffffac])) 0) 0)) -1 (nil)
;;    	  (nil))
;; which is the first part of the reloaded synthesized "movdi".
;;  The right thing would be to add equivalent replacement locations for
;; insn with pseudos that need more reloading.  The question is where.

(define_expand "movdi"
  [(parallel
    [(set (match_operand:DI 0 "nonimmediate_operand")
	  (match_operand:DI 1 "general_operand"))
     (clobber (reg:CC CRIS_CC0_REGNUM))])]
  ""
{
  if (MEM_P (operands[0])
      && operands[1] != const0_rtx
      && can_create_pseudo_p ())
    operands[1] = copy_to_mode_reg (DImode, operands[1]);

  /* Some other ports (as of 2001-09-10 for example mcore and romp) also
     prefer to split up constants early, like this.  The testcase in
     gcc.c-torture/execute/961213-1.c shows that CSE2 gets confused by the
     resulting subreg sets when using the construct from mcore (as of FSF
     CVS, version -r 1.5), and it believes that the high part (the last one
     emitted) is the final value.  */
  if ((CONST_INT_P (operands[1]) || GET_CODE (operands[1]) == CONST_DOUBLE)
      && ! reload_completed
      && ! reload_in_progress)
    {
      rtx insns;
      rtx op0 = operands[0];
      rtx op1 = operands[1];

      start_sequence ();
      emit_move_insn (operand_subword (op0, 0, 1, DImode),
		      operand_subword (op1, 0, 1, DImode));
      emit_move_insn (operand_subword (op0, 1, 1, DImode),
		      operand_subword (op1, 1, 1, DImode));
      insns = get_insns ();
      end_sequence ();

      emit_insn (insns);
      DONE;
    }
})

(define_insn_and_split "*movdi_insn"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=r,rx,m")
	(match_operand:DI 1 "general_operand"	   "rx,g,rxM"))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  "(register_operand (operands[0], DImode)
    || register_operand (operands[1], DImode)
    || operands[1] == const0_rtx)"
  "#"
  "&& reload_completed"
  [(match_dup 2)]
  "operands[2] = cris_split_movdx (operands);")

;; Normal move patterns from SI on.

(define_expand "movsi"
  [(parallel
    [(set
      (match_operand:SI 0 "nonimmediate_operand")
      (match_operand:SI 1 "general_operand"))
     (clobber (reg:CC CRIS_CC0_REGNUM))])]
  ""
{
  /* If the output goes to a MEM, make sure we have zero or a register as
     input.  */
  if (MEM_P (operands[0])
      && ! REG_S_P (operands[1])
      && operands[1] != const0_rtx
      && can_create_pseudo_p ())
    operands[1] = force_reg (SImode, operands[1]);

   /* At post-reload time, we'll get here for e.g. split multi-mode insns
      with a memory destination.  Go directly to the clobber-less variant.
      FIXME: Also applies to special-register source or destination.  */
   if (reload_completed
       && (MEM_P (operands[0]) || operands[1] == const0_rtx))
     {
        emit_insn (gen_rtx_SET (operands[0], operands[1]));
        DONE;
     }
})

;; We provide CC, CC_NZ and CC_NZVC variants, as moves clear V and C
;; and the result is thus usable in a compare against 0.
(define_insn "*movsi_internal<setcc><setnz><setnzvc>"
  [(set
    (match_operand:SI 0 "nonimmediate_operand"
		      "=r,r, r,Q>,r,Q>,g,r,r,g,rQ>,x,  m,x")
    (match_operand:SI 1 "general_operand"
		       "r,Q>,M,M, I,r, M,n,g,r,x,  rQ>,x,gi"))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
    ;; Note that we prefer not to use the S alternative (if for some reason
    ;; it competes with others) above, but g matches S.
  ""
{
  /* Better to have c-switch here; it is worth it to optimize the size of
     move insns.  The alternative would be to try to find more constraint
     letters.  FIXME: Check again.  It seems this could shrink a bit.  */
  switch (which_alternative)
    {
    case 0:
    case 1:
    case 5:
    case 8:
    case 9:
      return "move.d %1,%0";

    case 10:
    case 11:
    case 12:
    case 13:
      return "move %1,%0";

    case 2:
    case 3:
    case 6:
      return "clear.d %0";

      /* Constants -32..31 except 0.  */
    case 4:
      return "moveq %1,%0";

      /* We can win a little on constants -32768..-33, 32..65535.  */
    case 7:
      if (INTVAL (operands[1]) > 0 && INTVAL (operands[1]) < 65536)
	{
	  if (INTVAL (operands[1]) < 256)
	    return "movu.b %1,%0";
	  return "movu.w %1,%0";
	}
      else if (INTVAL (operands[1]) >= -32768 && INTVAL (operands[1]) < 32768)
	{
	  if (INTVAL (operands[1]) >= -128 && INTVAL (operands[1]) < 128)
	    return "movs.b %1,%0";
	  return "movs.w %1,%0";
	}
      return "move.d %1,%0";

    default:
      gcc_unreachable ();
    }
}
  [(set_attr "slottable" "yes,yes,yes,yes,yes,yes,no,no,no,no,yes,yes,no,no")
   (set_attr "cc<cccc><ccnz><ccnzvc>"
	     "*,*,none,none,*,none,none,*,*,none,none,none,none,none")])

;; FIXME: See movsi.

(define_insn "<acc><anz><anzvc>movhi<setcc><setnz><setnzvc>"
  [(set
    (match_operand:HI 0 "nonimmediate_operand" "=r,r, r,Q>,r,Q>,r,r,r,g,g,r,r,x")
    (match_operand:HI 1 "general_operand"	"r,Q>,M,M, I,r, L,O,n,M,r,g,x,r"))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  ""
{
  switch (which_alternative)
    {
    case 0:
    case 1:
    case 5:
    case 10:
    case 11:
      return "move.w %1,%0";
    case 12:
    case 13:
      return "move %1,%0";
    case 2:
    case 3:
    case 9:
      return "clear.w %0";
    case 4:
      return "moveq %1,%0";
    case 6:
    case 8:
      if (INTVAL (operands[1]) < 256 && INTVAL (operands[1]) >= -128)
	{
	  if (INTVAL (operands[1]) > 0)
	    return "movu.b %1,%0";
	  return "movs.b %1,%0";
	}
      return "move.w %1,%0";
    case 7:
      return "movEq %b1,%0";
    default:
      return "BOGUS: %1 to %0";
  }
}
  [(set_attr "slottable" "yes,yes,yes,yes,yes,yes,no,yes,no,no,no,no,yes,yes")
   (set_attr "cc<cccc><ccnz><ccnzvc>" "*,*,none,none,*,none,*,clobber,*,none,none,*,none,none")])

(define_insn "movstricthi"
  [(set
    (strict_low_part
     (match_operand:HI 0 "nonimmediate_operand" "+r,r, r,Q>,Q>,g,r,g"))
    (match_operand:HI 1 "general_operand"	 "r,Q>,M,M, r, M,g,r"))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  ""
  "@
   move.w %1,%0
   move.w %1,%0
   clear.w %0
   clear.w %0
   move.w %1,%0
   clear.w %0
   move.w %1,%0
   move.w %1,%0"
  [(set_attr "slottable" "yes,yes,yes,yes,yes,no,no,no")])

(define_expand "reload_in<mode>"
  [(set (match_operand:BW 2 "register_operand" "=r")
	(match_operand:BW 1 "memory_operand" "m"))
   (set (match_operand:BW 0 "register_operand" "=x")
	(match_dup 2))]
  ""
  "")

(define_expand "reload_out<mode>"
  [(set (match_operand:BW 2 "register_operand" "=&r")
	(match_operand:BW 1 "register_operand" "x"))
   (set (match_operand:BW 0 "memory_operand" "=m")
	(match_dup 2))]
  ""
  "")

(define_insn "<acc><anz><anzvc>movqi<setcc><setnz><setnzvc>"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=r,Q>,r, r,Q>,r,g,g,r,r,r,x")
	(match_operand:QI 1 "general_operand"	    "r,r, Q>,M,M, I,M,r,O,g,x,r"))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  ""
  "@
   move.b %1,%0
   move.b %1,%0
   move.b %1,%0
   clear.b %0
   clear.b %0
   moveq %1,%0
   clear.b %0
   move.b %1,%0
   moveq %b1,%0
   move.b %1,%0
   move %1,%0
   move %1,%0"
  [(set_attr "slottable" "yes,yes,yes,yes,yes,yes,no,no,yes,no,yes,yes")
   (set_attr "cc<cccc><ccnz><ccnzvc>"
	     "*,none,*,none,none,*,none,none,clobber,*,none,none")])

(define_insn "movstrictqi"
  [(set (strict_low_part
	 (match_operand:QI 0 "nonimmediate_operand" "+r,Q>,r, r,Q>,g,g,r"))
	(match_operand:QI 1 "general_operand"	     "r,r, Q>,M,M, M,r,g"))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  ""
  "@
   move.b %1,%0
   move.b %1,%0
   move.b %1,%0
   clear.b %0
   clear.b %0
   clear.b %0
   move.b %1,%0
   move.b %1,%0"
  [(set_attr "slottable" "yes,yes,yes,yes,yes,no,no,no")])

;; The valid "quick" bit-patterns are, except for 0.0, denormalized
;; values REALLY close to 0, and some NaN:s (I think; their exponent is
;; all ones); the worthwhile one is "0.0".
;; It will use clear, so we know ALL types of immediate 0 never change cc.

(define_insn "movsf"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=r,Q>,r, r,Q>,g,g,r,r,x,Q>,m,x, x")
	(match_operand:SF 1 "general_operand"       "r,r, Q>,G,G, G,r,g,x,r,x, x,Q>,g"))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  ""
  "@
   move.d %1,%0
   move.d %1,%0
   move.d %1,%0
   clear.d %0
   clear.d %0
   clear.d %0
   move.d %1,%0
   move.d %1,%0
   move %1,%0
   move %1,%0
   move %1,%0
   move %1,%0
   move %1,%0
   move %1,%0"
  [(set_attr "slottable" "yes,yes,yes,yes,yes,no,no,no,yes,yes,yes,no,yes,no")])

;; Post-reload, for memory destinations, split the clobber-variant and
;; get rid of the clobber.

(define_split ;; "*mov_tomem<mode>_split"
  [(set (match_operand:BWD 0 "memory_operand")
	(match_operand:BWD 1 "nonmemory_operand"))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  "reload_completed"
  [(set (match_dup 0) (match_dup 1))]
  "")

;; Exclude moving special-registers to memory from matching for
;; less-than-SImode, as they are SImode only (or actually, the size of
;; the register, but the ones free for "x" are naturally SImode; see
;; special measures taken for reload).
;; This might be a belt-and-suspenders thing, as a move from special
;; register to memory in less-than-SImode should not have made it here.

(define_mode_attr mov_tomem_enabled
  [(SI "yes,yes,yes,yes,yes,yes")
   (HI "yes,yes,no,yes,yes,no")
   (QI "yes,yes,no,yes,yes,no")])

(define_insn "*mov_tomem<mode>"
  [(set (match_operand:BWD 0 "memory_operand"   "=Q>,Q>,Q>,m,m,m")
	(match_operand:BWD 1 "nonmemory_operand" "M, r, x, M,r,x"))]
  "reload_completed"
  "@
   clear<m> %0
   move<m> %1,%0
   move %1,%0
   clear<m> %0
   move<m> %1,%0
   move %1,%0"
  [(set_attr "slottable" "yes,yes,yes,no,no,no")
   (set_attr "enabled" "<mov_tomem_enabled>")])

(define_split ;; "*mov_fromzero<mode>_split"
  [(set (match_operand:BWD 0 "register_operand") (const_int 0))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  "reload_completed
   && REGNO(operands[0]) <= CRIS_LAST_GENERAL_REGISTER"
  [(set (match_dup 0) (const_int 0))]
  "")

(define_insn "*mov_fromzero<mode>"
  [(set (match_operand:BWD 0 "register_operand" "=r") (const_int 0))]
  "reload_completed"
  "clear<m> %0"
  [(set_attr "slottable" "yes")])

;; Movem patterns.  Primarily for use in function prologue and epilogue.
;; Unfortunately, movem stores R0 in the highest memory location, thus
;; the opposite of the expectation for the standard names "load_multiple"
;; and "store_multiple".

(define_insn "*cris_load_multiple"
  [(match_parallel 0 "cris_load_multiple_op"
		   [(set (match_operand:SI 1 "register_operand" "=r,r")
			 (match_operand:SI 2 "memory_operand" "Q,m"))])]
  ""
  "movem %O0,%o0"
  [(set_attr "cc" "none")
   (set_attr "slottable" "yes,no")
   ;; Not true, but setting the length to 0 causes return sequences (ret
   ;; movem) to have the cost they had when (return) included the movem
   ;; and reduces the performance penalty taken for needing to emit an
   ;; epilogue (in turn copied by bb-reorder) instead of return patterns.
   ;; FIXME: temporary change until all insn lengths are correctly
   ;; described.  FIXME: have better target control over bb-reorder.
   (set_attr "length" "0")])

(define_insn "*cris_store_multiple"
  [(match_parallel 0 "cris_store_multiple_op"
		   [(set (match_operand:SI 2 "memory_operand" "=Q,m")
			 (match_operand:SI 1 "register_operand" "r,r"))])]
  ""
  "movem %o0,%O0"
  [(set_attr "cc" "none")
   (set_attr "slottable" "yes,no")])


;; Sign- and zero-extend insns with standard names.
;;  Those for integer source operand are ordered with the widest source
;; type first.

;; Sign-extend.

(define_insn "extendsidi2"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(sign_extend:DI (match_operand:SI 1 "general_operand" "g")))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  ""
  "move.d %1,%M0\;smi %H0\;neg.d %H0,%H0")

(define_insn "extend<mode>di2"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(sign_extend:DI (match_operand:BW 1 "general_operand" "g")))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  ""
  "movs<m> %1,%M0\;smi %H0\;neg.d %H0,%H0")

(define_insn "<acc><anz><anzvc>extend<mode>si2<setcc><setnz><setnzvc>"
  [(set (match_operand:SI 0 "register_operand" "=r,r,r")
	(sign_extend:SI (match_operand:BW 1 "general_operand" "r,Q>,g")))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  ""
  "movs<m> %1,%0"
  [(set_attr "slottable" "yes,yes,no")])

;; To do a byte->word extension, extend to dword, except that the top half
;; of the register will be clobbered.  FIXME: Perhaps this is not needed.

(define_insn "extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "=r,r,r")
	(sign_extend:HI (match_operand:QI 1 "general_operand" "r,Q>,g")))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  ""
  "movs.b %1,%0"
  [(set_attr "slottable" "yes,yes,no")])


;; Zero-extend.  The DImode ones are synthesized by gcc, so we don't
;; specify them here.

(define_insn "<acc><anz><anzvc>zero_extend<mode>si2<setcc><setnz><setnzvc>"
  [(set (match_operand:SI 0 "register_operand" "=r,r,r")
	(zero_extend:SI
	 (match_operand:BW 1 "nonimmediate_operand" "r,Q>,m")))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  ""
  "movu<m> %1,%0"
  [(set_attr "slottable" "yes,yes,no")])

;; Same comment as sign-extend QImode to HImode above applies.

(define_insn "zero_extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "=r,r,r")
	(zero_extend:HI
	 (match_operand:QI 1 "nonimmediate_operand" "r,Q>,m")))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  ""
  "movu.b %1,%0"
  [(set_attr "slottable" "yes,yes,no")])

;; Add operations, standard names.

;; Note that for the 'P' constraint, the high part can be -1 or 0.  We
;; output the insn through the 'A' output modifier as "adds.w" and "addq",
;; respectively.
(define_expand "adddi3"
  [(parallel
    [(set (match_operand:DI 0 "register_operand")
	  (plus:DI (match_operand:DI 1 "register_operand")
		   (match_operand:DI 2 "general_operand")))
     (clobber (reg:CC CRIS_CC0_REGNUM))])]
  ""
  "")

(define_insn "*adddi3<setnz>"
  [(set (match_operand:DI 0 "register_operand" "=r,r,r,&r,&r")
	(plus:DI (match_operand:DI 1 "register_operand" "%0,0,0,0,r")
		 (match_operand:DI 2 "general_operand" "J,N,P,g,!To")))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  ""
  "@
   addq %2,%M0\;ax\;addq 0,%H0
   subq %n2,%M0\;ax\;subq 0,%H0
   add%e2.%z2 %2,%M0\;ax\;%A2 %H2,%H0
   add.d %M2,%M0\;ax\;add.d %H2,%H0
   add.d %M2,%M1,%M0\;ax\;add.d %H2,%H1,%H0")

(define_expand "add<mode>3"
  [(parallel
    [(set (match_operand:BWD 0 "register_operand")
	  (plus:BWD
	   (match_operand:BWD 1 "register_operand")
	   (match_operand:BWD 2 "general_operand")))
     (clobber (reg:CC CRIS_CC0_REGNUM))])]
  ""
  "")

(define_insn "*addsi3<setnz>"
  [(set (match_operand:SI 0 "register_operand"  "=r,r, r,r,r,r,r,  r")
	(plus:SI
	 (match_operand:SI 1 "register_operand" "%0,0, 0,0,0,0,r,  r")
	 (match_operand:SI 2 "general_operand"   "r,Q>,J,N,n,g,!To,0")))
   (clobber (reg:CC CRIS_CC0_REGNUM))]

;; The last constraint is due to that after reload, the '%' is not
;; honored, and canonicalization doesn't care about keeping the same
;; register as in destination.  This will happen after insn splitting.

 ""
{
  switch (which_alternative)
    {
    case 0:
    case 1:
      return "add.d %2,%0";
    case 2:
      return "addq %2,%0";
    case 3:
      return "subq %n2,%0";
    case 4:
      /* 'Known value', but not in -63..63.
	 Check if addu/subu may be used.  */
      if (INTVAL (operands[2]) > 0)
	{
	  if (INTVAL (operands[2]) < 256)
	    return "addu.b %2,%0";
	  if (INTVAL (operands[2]) < 65536)
	    return "addu.w %2,%0";
	}
      else
	{
	  if (INTVAL (operands[2]) >= -255)
	    return "subu.b %n2,%0";
	  if (INTVAL (operands[2]) >= -65535)
	    return "subu.w %n2,%0";
	}
      return "add.d %2,%0";
    case 5:
      return "add.d %2,%0";
    case 6:
      return "add.d %2,%1,%0";
    case 7:
      return "add.d %1,%0";
    default:
      return "BOGUS addsi %2+%1 to %0";
    }
}
 [(set_attr "slottable" "yes,yes,yes,yes,no,no,no,yes")])

(define_insn "*addhi3<setnz>"
  [(set (match_operand:HI 0 "register_operand"		"=r,r, r,r,r,r")
	(plus:HI (match_operand:HI 1 "register_operand" "%0,0, 0,0,0,r")
		 (match_operand:HI 2 "general_operand"   "r,Q>,J,N,g,!To")))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  ""
  "@
   add.w %2,%0
   add.w %2,%0
   addq %2,%0
   subq %n2,%0
   add.w %2,%0
   add.w %2,%1,%0"
  [(set_attr "slottable" "yes,yes,yes,yes,no,no")
   (set_attr "cc<ccnz>" "normal,normal,clobber,clobber,normal,normal")])

(define_insn "*addqi3<setnz>"
  [(set (match_operand:QI 0 "register_operand"		"=r,r, r,r,r,r,r")
	(plus:QI (match_operand:QI 1 "register_operand" "%0,0, 0,0,0,0,r")
		 (match_operand:QI 2 "general_operand"	 "r,Q>,J,N,O,g,!To")))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  ""
  "@
   add.b %2,%0
   add.b %2,%0
   addq %2,%0
   subq %n2,%0
   subQ -%b2,%0
   add.b %2,%0
   add.b %2,%1,%0"
  [(set_attr "slottable" "yes,yes,yes,yes,yes,no,no")
   (set_attr "cc<ccnz>" "normal,normal,clobber,clobber,clobber,normal,normal")])

;; Subtract.
;;
;; Note that because of insn canonicalization these will *seldom* but
;; rarely be used with a known constant as an operand.

;; Note that for the 'P' constraint, the high part can be -1 or 0.  We
;; output the insn through the 'D' output modifier as "subs.w" and "subq",
;; respectively.
(define_expand "subdi3"
  [(parallel
    [(set (match_operand:DI 0 "register_operand")
	  (minus:DI (match_operand:DI 1 "register_operand")
		    (match_operand:DI 2 "general_operand")))
     (clobber (reg:CC CRIS_CC0_REGNUM))])]
  ""
  "")

(define_insn "*subdi3<setnz>"
  [(set (match_operand:DI 0 "register_operand" "=r,r,r,&r,&r")
	(minus:DI (match_operand:DI 1 "register_operand" "0,0,0,0,r")
		  (match_operand:DI 2 "general_operand" "J,N,P,g,!To")))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  ""
  "@
   subq %2,%M0\;ax\;subq 0,%H0
   addq %n2,%M0\;ax\;addq 0,%H0
   sub%e2.%z2 %2,%M0\;ax\;%D2 %H2,%H0
   sub.d %M2,%M0\;ax\;sub.d %H2,%H0
   sub.d %M2,%M1,%M0\;ax\;sub.d %H2,%H1,%H0")

(define_expand "sub<mode>3"
  [(parallel
    [(set (match_operand:BWD 0 "register_operand")
	  (minus:BWD
	   (match_operand:BWD 1 "register_operand")
	   (match_operand:BWD 2 "general_operand")))
     (clobber (reg:CC CRIS_CC0_REGNUM))])]
  ""
  "")

(define_insn "*subsi3<setnz>"
  [(set (match_operand:SI 0 "register_operand" "=r,r, r,r,r,r,r,r")
	(minus:SI
	 (match_operand:SI 1 "register_operand" "0,0, 0,0,0,0,0,r")
	 (match_operand:SI 2 "general_operand"	"r,Q>,J,N,P,n,g,!To")))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  ""

;; This does not do the optimal: "addu.w 65535,r0" when %2 is negative.
;; But then again, %2 should not be negative.

  "@
   sub.d %2,%0
   sub.d %2,%0
   subq %2,%0
   addq %n2,%0
   sub%e2.%z2 %2,%0
   sub.d %2,%0
   sub.d %2,%0
   sub.d %2,%1,%0"
  [(set_attr "slottable" "yes,yes,yes,yes,no,no,no,no")])

(define_insn "*sub<mode>3<setnz>"
  [(set (match_operand:BW 0 "register_operand"		"=r,r, r,r,r,r")
	(minus:BW (match_operand:BW 1 "register_operand" "0,0, 0,0,0,r")
		  (match_operand:BW 2 "general_operand"  "r,Q>,J,N,g,!To")))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  ""
  "@
   sub<m> %2,%0
   sub<m> %2,%0
   subq %2,%0
   addq %n2,%0
   sub<m> %2,%0
   sub<m> %2,%1,%0"
  [(set_attr "slottable" "yes,yes,yes,yes,no,no")
   (set_attr "cc<ccnz>" "normal,normal,clobber,clobber,normal,normal")])

;; Extend versions (zero/sign) of normal add/sub (no side-effects).

;; QImode to HImode
;; FIXME: GCC should widen.

(define_insn "*<addsub><su>qihi"
  [(set (match_operand:HI 0 "register_operand" "=r,r,r,r")
	(plusminus:HI
	 (match_operand:HI 1 "register_operand" "0,0,0,r")
	 (szext:HI (match_operand:QI 2 "nonimmediate_operand" "r,Q>,m,!To"))))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  "GET_MODE_SIZE (GET_MODE (operands[0])) <= UNITS_PER_WORD
   && (operands[1] != frame_pointer_rtx || <plusminus:CODE> != PLUS)"
  "@
   <addsub><su>.b %2,%0
   <addsub><su>.b %2,%0
   <addsub><su>.b %2,%0
   <addsub><su>.b %2,%1,%0"
  [(set_attr "slottable" "yes,yes,no,no")
   (set_attr "cc" "clobber")])

;; FIXME: bound is actually also <setnzvc>, but is so rarely used in this
;; form that it's not worthwhile to make that distinction.
(define_insn "*<addsubbo><su><nd><mode>si<setnz>"
  [(set (match_operand:SI 0 "register_operand" "=r,r,r,r")
	(plusminusumin:SI
	 (match_operand:SI 1 "register_operand" "0,0,0,r")
	 (szext:SI (match_operand:BW 2 "nonimmediate_operand" "r,Q>,m,!To"))))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  "(<plusminusumin:CODE> != UMIN || <szext:CODE> == ZERO_EXTEND)
   && (operands[1] != frame_pointer_rtx || <plusminusumin:CODE> != PLUS)"
  "@
   <addsubbo><su><nd><m> %2,%0
   <addsubbo><su><nd><m> %2,%0
   <addsubbo><su><nd><m> %2,%0
   <addsubbo><su><nd><m> %2,%1,%0"
  [(set_attr "slottable" "yes,yes,no,no")])

;; We may have swapped operands for add or bound.
;; For commutative operands, these are the canonical forms.

;; QImode to HImode

(define_insn "*add<su>qihi_swap"
  [(set (match_operand:HI 0 "register_operand" "=r,r,r,r")
	(plus:HI
	 (szext:HI (match_operand:QI 2 "nonimmediate_operand" "r,Q>,m,!To"))
	 (match_operand:HI 1 "register_operand" "0,0,0,r")))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  "operands[1] != frame_pointer_rtx"
  "@
   add<su>.b %2,%0
   add<su>.b %2,%0
   add<su>.b %2,%0
   add<su>.b %2,%1,%0"
  [(set_attr "slottable" "yes,yes,no,no")
   (set_attr "cc" "clobber")])

(define_insn "*<addsubbo><su><nd><mode>si<setnz>_swap"
  [(set (match_operand:SI 0 "register_operand" "=r,r,r,r")
	(plusumin:SI
	 (szext:SI (match_operand:BW 2 "nonimmediate_operand" "r,Q>,m,!To"))
	 (match_operand:SI 1 "register_operand" "0,0,0,r")))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  "(<plusumin:CODE> != UMIN || <szext:CODE> == ZERO_EXTEND)
   && operands[1] != frame_pointer_rtx"
  "@
   <addsubbo><su><nd><m> %2,%0
   <addsubbo><su><nd><m> %2,%0
   <addsubbo><su><nd><m> %2,%0
   <addsubbo><su><nd><m> %2,%1,%0"
  [(set_attr "slottable" "yes,yes,no,no")])

;; This is the special case when we use what corresponds to the
;; instruction above in "casesi".  Do *not* change it to use the generic
;; pattern and "REG 15" as pc; I did that and it led to madness and
;; maintenance problems: Instead of (as imagined) recognizing and removing
;; or replacing this pattern with something simpler, other variant
;; patterns were recognized or combined, including some prefix variants
;; where the value in pc is not that of the next instruction (which means
;; this instruction actually *is* special and *should* be marked as such).
;; When switching from the "generic pattern match" approach to this simpler
;; approach, there were insignificant differences in gcc, ipps and
;; product code, somehow due to scratching reload behind the ear or
;; something.  Testcase "gcc" looked .01% slower and 4 bytes bigger;
;; product code became .001% smaller but "looked better".  The testcase
;; "ipps" was just different at register allocation).
;;
;; Assumptions in the jump optimizer forces us to use IF_THEN_ELSE in this
;; pattern with the default-label as the else, with the "if" being
;; index-is-less-than the max number of cases plus one.  The default-label
;; is attached to the end of the case-table at time of output.

(define_insn "*casesi_adds_w"
  [(set (pc)
	(if_then_else
	 (ltu (match_operand:SI 0 "register_operand" "r")
	      (match_operand:SI 1 "const_int_operand" "n"))
	 (plus:SI (sign_extend:SI
		   (mem:HI
		    (plus:SI (mult:SI (match_dup 0) (const_int 2))
			     (pc))))
		  (pc))
	 (label_ref (match_operand 2 "" ""))))
   (use (label_ref (match_operand 3 "" "")))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  "operands[0] != frame_pointer_rtx"
  "adds.w [$pc+%0.w],$pc"
  [(set_attr "cc" "clobber")])

;; Multiply instructions.

;; Sometimes powers of 2 (which are normally canonicalized to a
;; left-shift) appear here, as a result of address reloading.
;; As a special, for values 3 and 5, we can match with an addi, so add those.
;;
;; FIXME: This may be unnecessary now.
;; Explicitly named for convenience of having a gen_... function.

(define_insn "addi_mul"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(mult:SI
	 (match_operand:SI 1 "register_operand" "%0")
	 (match_operand:SI 2 "const_int_operand" "n")))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  "operands[0] != frame_pointer_rtx
   && operands[1] != frame_pointer_rtx
   && CONST_INT_P (operands[2])
   && (INTVAL (operands[2]) == 2
       || INTVAL (operands[2]) == 4 || INTVAL (operands[2]) == 3
       || INTVAL (operands[2]) == 5)"
{
  if (INTVAL (operands[2]) == 2)
    return "lslq 1,%0";
  else if (INTVAL (operands[2]) == 4)
    return "lslq 2,%0";
  else if (INTVAL (operands[2]) == 3)
    return "addi %0.w,%0";
  else if (INTVAL (operands[2]) == 5)
    return "addi %0.d,%0";
  return "BAD: adr_mulsi: %0=%1*%2";
}
[(set_attr "slottable" "yes")
 ;; No flags are changed if this insn is "addi", but it does not seem
 ;; worth the trouble to distinguish that to the lslq cases.
 (set_attr "cc" "clobber")])

;; The addi insn as it is normally used.

(define_insn "*addi"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI
	 (mult:SI (match_operand:SI 2 "register_operand" "r")
		  (match_operand:SI 3 "const_int_operand" "n"))
	 (match_operand:SI 1 "register_operand" "0")))]
  "operands[0] != frame_pointer_rtx
   && operands[1] != frame_pointer_rtx
   && CONST_INT_P (operands[3])
   && (INTVAL (operands[3]) == 1
       || INTVAL (operands[3]) == 2 || INTVAL (operands[3]) == 4)"
  "addi %2%T3,%0"
  [(set_attr "slottable" "yes")
   (set_attr "cc" "none")])

;; This pattern is usually generated after reload, so a '%' is
;; ineffective; use explicit combinations.
(define_insn "*addi_b_<mode>"
  [(set (match_operand:BWD 0 "register_operand" "=r,r")
	(plus:BWD
	 (match_operand:BWD 1 "register_operand" "0,r")
	 (match_operand:BWD 2 "register_operand" "r,0")))]
  ""
  "@
   addi %2.b,%0
   addi %1.b,%0"
  [(set_attr "slottable" "yes")])

;; Strip the dccr clobber from addM3 with register operands, if the
;; next instruction isn't using it.
;; Not clobbering dccr may let cmpelim match a later compare with a
;; previous operation of interest.  This has to run before cmpelim so it
;; can't be a peephole2.  See gcc.target/cris/pr93372-45.c for a
;; test-case.
(define_split ;; "*add<mode>3_addi"
  [(parallel
    [(set (match_operand:BWD 0 "register_operand")
	  (plus:BWD
	   (match_operand:BWD 1 "register_operand")
	   (match_operand:BWD 2 "register_operand")))
     (clobber (reg:CC CRIS_CC0_REGNUM))])]
  "reload_completed"
  [(set (match_dup 0) (plus:BWD (match_dup 1) (match_dup 2)))]
{
  rtx reg = operands[0];
  rtx_insn *i = next_nonnote_nondebug_insn_bb (curr_insn);

  while (i != NULL_RTX && (!INSN_P (i) || DEBUG_INSN_P (i)))
    i = next_nonnote_nondebug_insn_bb (i);

  if (i == NULL_RTX || reg_mentioned_p (reg, i) || BARRIER_P (i))
    FAIL;
})

(define_insn "<u>mul<s><mode>3"
  [(set (match_operand:WD 0 "register_operand" "=r")
	(mult:WD
	 (szext:WD (match_operand:<S> 1 "register_operand" "%0"))
	 (szext:WD (match_operand:<S> 2 "register_operand" "r"))))
   (clobber (match_scratch:SI 3 "=h"))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  "TARGET_HAS_MUL_INSNS"
  "%!mul<su><mm> %2,%0"
  [(set (attr "slottable")
	(if_then_else (match_test "TARGET_MUL_BUG")
		      (const_string "no")
		      (const_string "yes")))
   ;; For umuls.[bwd] it's just N unusable here, but let's be safe.
   ;; For muls.b, this really extends to SImode, so cc should be
   ;; considered clobbered.
   ;; For muls.w, it's just N unusable here, but let's be safe.
   (set_attr "cc" "clobber")])

;; Note that gcc does not make use of such a thing as umulqisi3.  It gets
;; confused and will erroneously use it instead of umulhisi3, failing (at
;; least) gcc.c-torture/execute/arith-rand.c at all optimization levels.
;; Inspection of optab code shows that there must be only one widening
;; multiplication per mode widened to.

(define_insn "mulsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(mult:SI (match_operand:SI 1 "register_operand" "%0")
		 (match_operand:SI 2 "register_operand" "r")))
   (clobber (match_scratch:SI 3 "=h"))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  "TARGET_HAS_MUL_INSNS"
  "%!muls.d %2,%0"
  [(set (attr "slottable")
	(if_then_else (match_test "TARGET_MUL_BUG")
		      (const_string "no")
		      (const_string "yes")))
   ;; Just N unusable here, but let's be safe.
   (set_attr "cc" "clobber")])

;; A few multiply variations.

;; When needed, we can get the high 32 bits from the overflow
;; register.  We don't care to split and optimize these.

(define_insn "<u>mulsidi3"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(mult:DI
	 (szext:DI (match_operand:SI 1 "register_operand" "%0"))
	 (szext:DI (match_operand:SI 2 "register_operand" "r"))))
   (clobber (match_scratch:SI 3 "=h"))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  "TARGET_HAS_MUL_INSNS"
  "%!mul<su>.d %2,%M0\;move $mof,%H0")

;; These two patterns may be expressible by other means, perhaps by making
;; [u]?mulsidi3 a define_expand.

;; Due to register allocation braindamage, the clobber 1,2 alternatives
;; cause a move into the clobbered register *before* the insn, then
;; after the insn, mof is moved too, rather than the clobber assigned
;; the last mof target.  This became apparent when making MOF and SRP
;; visible registers, with the necessary tweak to smulsi3_highpart.
;; Because these patterns are used in division by constants, that damage
;; is visible (ipps regression tests).  Therefore the last two
;; alternatives, "helping" reload to avoid an unnecessary move, but
;; punished by force of one "?".  Check code from "int d (int a) {return
;; a / 1000;}" and unsigned.  FIXME: Comment above was for 3.2, revisit.

(define_insn "<su>mulsi3_highpart"
  [(set (match_operand:SI 0 "register_operand" "=h,h,?r,?r")
	(truncate:SI
	 (lshiftrt:DI
	  (mult:DI
	   (szext:DI (match_operand:SI 1 "register_operand" "r,r,0,r"))
	   (szext:DI (match_operand:SI 2 "register_operand" "r,r,r,0")))
	  (const_int 32))))
   (clobber (match_scratch:SI 3 "=1,2,h,h"))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  "TARGET_HAS_MUL_INSNS"
  "@
   %!mul<su>.d %2,%1
   %!mul<su>.d %1,%2
   %!mul<su>.d %2,%1\;move $mof,%0
   %!mul<su>.d %1,%2\;move $mof,%0"
  [(set_attr "slottable" "yes,yes,no,no")
   (set_attr "cc" "clobber")])

;; Divide and modulus instructions.  CRIS only has a step instruction.

(define_insn "dstep_shift"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(if_then_else:SI
	 (geu:SI (ashift:SI (match_operand:SI 1 "register_operand" "0")
			    (const_int 1))
	      (match_operand:SI 2 "register_operand" "r"))
	 (minus:SI (ashift:SI (match_operand:SI 3 "register_operand" "0")
			(const_int 1))
		   (match_operand:SI 4 "register_operand" "2"))
	 (ashift:SI (match_operand:SI 5 "register_operand" "0")
			(const_int 1))))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  ""
  "dstep %2,%0"
  [(set_attr "slottable" "yes")])

;; Here's a variant with mult instead of ashift.
;;
;; FIXME: This should be investigated.  Which one matches through combination?

(define_insn "dstep_mul"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(if_then_else:SI
	 (geu:SI (mult:SI (match_operand:SI 1 "register_operand" "0")
			  (const_int 2))
	      (match_operand:SI 2 "register_operand" "r"))
	 (minus:SI (mult:SI (match_operand:SI 3 "register_operand" "0")
			    (const_int 2))
		   (match_operand:SI 4 "register_operand" "2"))
	 (mult:SI (match_operand:SI 5 "register_operand" "0")
		  (const_int 2))))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  "operands[0] != frame_pointer_rtx
   && operands[1] != frame_pointer_rtx
   && operands[2] != frame_pointer_rtx
   && operands[3] != frame_pointer_rtx"
  "dstep %2,%0"
  [(set_attr "slottable" "yes")])

;; Logical operators.

;; Bitwise "and".

;; There is no use in defining "anddi3", because gcc can expand this by
;; itself, and make reasonable code without interference.

;; If the first operand is memory or a register and is the same as the
;; second operand, and the third operand is -256 or -65536, we can use
;; CLEAR instead.  Or, if the first operand is a register, and the third
;; operand is 255 or 65535, we can zero_extend.
;; GCC isn't smart enough to recognize these cases (yet), and they seem
;; to be common enough to be worthwhile.
;; FIXME: This should be made obsolete.

(define_expand "andsi3"
  [(parallel
    [(set (match_operand:SI 0 "nonimmediate_operand")
	  (and:SI (match_operand:SI 1 "nonimmediate_operand")
		  (match_operand:SI 2 "general_operand")))
     (clobber (reg:CC CRIS_CC0_REGNUM))])]
  ""
{
  if (! (CONST_INT_P (operands[2])
	 && (((INTVAL (operands[2]) == -256
	       || INTVAL (operands[2]) == -65536)
	      && rtx_equal_p (operands[1], operands[0]))
	     || ((INTVAL (operands[2]) == 255
		  || INTVAL (operands[2]) == 65535)
		 && REG_P (operands[0])))))
    {
      /* Make intermediate steps if operand0 is not a register or
	 operand1 is not a register, and hope that the reload pass will
	 make something useful out of it.  Note that the operands are
	 *not* canonicalized.  For the moment, I chicken out on this,
	 because all or most ports do not describe 'and' with
	 canonicalized operands, and I seem to remember magic in reload,
	 checking that operand1 has constraint '%0', in which case
	 operand0 and operand1 must have similar predicates.
	 FIXME: Investigate.  */
      rtx reg0 = REG_P (operands[0]) ? operands[0] : gen_reg_rtx (SImode);
      rtx reg1 = operands[1];

      if (! REG_P (reg1))
	{
	  emit_move_insn (reg0, reg1);
	  reg1 = reg0;
	}

      cris_emit_insn (gen_rtx_SET (reg0, gen_rtx_AND (SImode, reg1,
						      operands[2])));

      /* Make sure we get the right *final* destination.  */
      if (! REG_P (operands[0]))
	emit_move_insn (operands[0], reg0);

      DONE;
    }
})

;; Some special cases of andsi3.

(define_insn "*andsi_movu"
  [(set (match_operand:SI 0 "register_operand" "=r,r,r")
	(and:SI (match_operand:SI 1 "nonimmediate_operand" "%r,Q,To")
		(match_operand:SI 2 "const_int_operand" "n,n,n")))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  "(INTVAL (operands[2]) == 255 || INTVAL (operands[2]) == 65535)
   && !side_effects_p (operands[1])"
  "movu.%z2 %1,%0"
  [(set_attr "slottable" "yes,yes,no")])

;; FIXME: Remember, this does *not* actually affect condition codes;
;; get rid of the clobber.
(define_insn "*andsi_clear"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=r,r,Q,Q,To,To")
	(and:SI (match_operand:SI 1 "nonimmediate_operand" "%0,0,0,0,0,0")
		(match_operand:SI 2 "const_int_operand" "P,n,P,n,P,n")))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  "(INTVAL (operands[2]) == -65536 || INTVAL (operands[2]) == -256)
   && !side_effects_p (operands[0])"
  "@
   cLear.b %0
   cLear.w %0
   cLear.b %0
   cLear.w %0
   cLear.b %0
   cLear.w %0"
  [(set_attr "slottable" "yes,yes,yes,yes,no,no")
   (set_attr "cc" "none")])

;; This is a catch-all pattern, taking care of everything that was not
;; matched in the insns above.
;;
;; Sidenote: the tightening from "nonimmediate_operand" to
;; "register_operand" for operand 1 actually increased the register
;; pressure (worse code).  That will hopefully change with an
;; improved reload pass.

(define_insn "*expanded_andsi<setcc><setnz><setnzvc>"
  [(set (match_operand:SI 0 "register_operand"	       "=r,r,r, r,r")
	(and:SI (match_operand:SI 1 "register_operand" "%0,0,0, 0,r")
		(match_operand:SI 2 "general_operand"   "I,r,Q>,g,!To")))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  ""
  "@
   andq %2,%0
   and.d %2,%0
   and.d %2,%0
   and.d %2,%0
   and.d %2,%1,%0"
  [(set_attr "slottable" "yes,yes,yes,no,no")])

;; For both QI and HI we may use the quick patterns.  This results in
;; useless condition codes, but that is used rarely enough for it to
;; normally be a win (could check ahead for use of CRIS_CC0_REGNUM, but
;; seems to be more pain than win).

;; FIXME: See note for andsi3

(define_expand "andhi3"
  [(parallel
    [(set (match_operand:HI 0 "nonimmediate_operand")
	(and:HI (match_operand:HI 1 "nonimmediate_operand")
		(match_operand:HI 2 "general_operand")))
     (clobber (reg:CC CRIS_CC0_REGNUM))])]
  ""
{
  if (! (CONST_INT_P (operands[2])
	 && (((INTVAL (operands[2]) == -256
	       || INTVAL (operands[2]) == 65280)
	      && rtx_equal_p (operands[1], operands[0]))
	     || (INTVAL (operands[2]) == 255
		 && REG_P (operands[0])))))
    {
      /* See comment for andsi3.  */
      rtx reg0 = REG_P (operands[0]) ? operands[0] : gen_reg_rtx (HImode);
      rtx reg1 = operands[1];

      if (! REG_P (reg1))
	{
	  emit_move_insn (reg0, reg1);
	  reg1 = reg0;
	}

      cris_emit_insn (gen_rtx_SET (reg0, gen_rtx_AND (HImode, reg1,
						      operands[2])));

      /* Make sure we get the right destination.  */
      if (! REG_P (operands[0]))
	emit_move_insn (operands[0], reg0);

      DONE;
    }
})

;; Some fast andhi3 special cases.

(define_insn "*andhi_movu"
  [(set (match_operand:HI 0 "register_operand" "=r,r,r")
	(and:HI (match_operand:HI 1 "nonimmediate_operand" "r,Q,To")
		(const_int 255)))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  "!side_effects_p (operands[1])"
  "mOvu.b %1,%0"
  [(set_attr "slottable" "yes,yes,no")])

(define_insn "*andhi_clear"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=r,Q,To")
	(and:HI (match_operand:HI 1 "nonimmediate_operand" "0,0,0")
		(const_int -256)))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  "!side_effects_p (operands[0])"
  "cLear.b %0"
  [(set_attr "slottable" "yes,yes,no")
   (set_attr "cc" "none")])

;; Catch-all andhi3 pattern.

(define_insn "*expanded_andhi<setcc><setnz><setnzvc>"
  [(set (match_operand:HI 0 "register_operand"	       "=r,r, r,r, r,r,r,r")
	(and:HI (match_operand:HI 1 "register_operand" "%0,0, 0,0, 0,0,0,r")
		(match_operand:HI 2 "general_operand"   "I,Kc,r,Q>,L,O,g,!To")))
		;; The "Kc" alternative above, is there to match for cmpelim;
		;; it will be dominated by the "I" alternative at other times.
   (clobber (reg:CC CRIS_CC0_REGNUM))]

;; Sidenote: the tightening from "general_operand" to
;; "register_operand" for operand 1 actually increased the register
;; pressure (worse code).  That will hopefully change with an
;; improved reload pass.

  ""
  "@
   andq %2,%0
   andq %2,%0
   and.w %2,%0
   and.w %2,%0
   and.w %2,%0
   anDq %b2,%0
   and.w %2,%0
   and.w %2,%1,%0"
  [(set_attr "slottable" "yes,yes,yes,yes,no,yes,no,no")
   (set_attr "cc<cccc><ccnz><ccnzvc>"
	     "clobber,normal,normal,normal,normal,clobber,normal,normal")])

;; A strict_low_part pattern.

;; Note the use of (match_dup 0) for the first operand of the operation
;; here.  Reload can't handle an operand pair where one is read-write
;; and must match a read, like in:
;; (insn 80 79 81 4
;;  (set (strict_low_part
;;        (subreg:QI (reg/v:SI 0 r0 [orig:36 data ] [36]) 0))
;;       (and:QI
;;        (subreg:QI (reg:SI 15 acr [orig:27 D.7531 ] [27]) 0)
;;        (const_int -64 [0xf..fc0]))) x.c:126 147 {*andqi_lowpart_v32}
;;  (nil))
;; (Note: the example is obsolete.)
;; In theory, it could reload this as a movstrictqi of the register
;; operand at the and:QI to the destination register and change the
;; and:QI operand to the same as the read-write output operand and the
;; result would be recognized, but it doesn't recognize that's a valid
;; reload for a strict_low_part-destination; it just sees a "+" at the
;; destination constraints.  Better than adding complexity to reload is
;; to follow the lead of m68k (see comment that begins with "These insns
;; must use MATCH_DUP") since prehistoric times and make it just a
;; match_dup.  FIXME: a sanity-check in gen* to refuse an insn with
;; input-constraints matching input-output-constraints, e.g. "+r" <- "0".

(define_insn "*andhi_lowpart"
  [(set (strict_low_part
	 (match_operand:HI 0 "register_operand"	       "+r,r,r"))
	(and:HI (match_dup 0)
		(match_operand:HI 1 "general_operand"   "r,Q>,g")))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  ""
  "@
   and.w %1,%0
   and.w %1,%0
   and.w %1,%0"
  [(set_attr "slottable" "yes,yes,no")])

(define_expand "andqi3"
  [(parallel
    [(set (match_operand:QI 0 "register_operand")
	  (and:QI (match_operand:QI 1 "register_operand")
		  (match_operand:QI 2 "general_operand")))
     (clobber (reg:CC CRIS_CC0_REGNUM))])]
  ""
  "")

(define_insn "*andqi3<setcc><setnz><setnzvc>"
  [(set (match_operand:QI 0 "register_operand"	       "=r,r, r,r, r,r,r")
	(and:QI (match_operand:QI 1 "register_operand" "%0,0, 0,0, 0,0,r")
		(match_operand:QI 2 "general_operand"   "I,Kc,r,Q>,O,g,!To")))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  ""
  "@
   andq %2,%0
   andq %2,%0
   and.b %2,%0
   and.b %2,%0
   andQ %b2,%0
   and.b %2,%0
   and.b %2,%1,%0"
  [(set_attr "slottable" "yes,yes,yes,yes,yes,no,no")
   (set_attr "cc<cccc><ccnz><ccnzvc>"
	     "clobber,normal,normal,normal,clobber,normal,normal")])

(define_insn "*andqi_lowpart"
  [(set (strict_low_part
	 (match_operand:QI 0 "register_operand"	       "+r,r,r"))
	(and:QI (match_dup 0)
		(match_operand:QI 1 "general_operand"   "r,Q>,g")))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  ""
  "@
   and.b %1,%0
   and.b %1,%0
   and.b %1,%0"
  [(set_attr "slottable" "yes,yes,no")])

;; Bitwise or.

;; Same comment as anddi3 applies here - no need for such a pattern.

;; It seems there's no need to jump through hoops to get good code such as
;; with andsi3.

(define_expand "ior<mode>3"
  [(parallel
    [(set (match_operand:BWD 0 "register_operand")
	  (ior:BWD (match_operand:BWD 1 "register_operand")
		   (match_operand:BWD 2 "general_operand")))
     (clobber (reg:CC CRIS_CC0_REGNUM))])]
  ""
  "")

(define_insn "*iorsi3<setcc><setnz><setnzvc>"
  [(set (match_operand:SI 0 "register_operand"	       "=r,r,r, r,r,r")
	(ior:SI (match_operand:SI 1 "register_operand" "%0,0,0, 0,0,r")
		(match_operand:SI 2 "general_operand"  "I, r,Q>,n,g,!To")))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  ""
  "@
   orq %2,%0
   or.d %2,%0
   or.d %2,%0
   oR.%s2 %2,%0
   or.d %2,%0
   or.d %2,%1,%0"
  [(set_attr "slottable" "yes,yes,yes,no,no,no")
   (set_attr "cc<cccc><ccnz><ccnzvc>"
	     "normal,normal,normal,clobber,normal,normal")])

(define_insn "*iorhi3<setcc><setnz><setnzvc>"
  [(set (match_operand:HI 0 "register_operand"	       "=r,r,r, r,r,r,r")
	(ior:HI (match_operand:HI 1 "register_operand" "%0,0,0, 0,0,0,r")
		(match_operand:HI 2 "general_operand"   "I,r,Q>,L,O,g,!To")))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  ""
  "@
   orq %2,%0
   or.w %2,%0
   or.w %2,%0
   or.w %2,%0
   oRq %b2,%0
   or.w %2,%0
   or.w %2,%1,%0"
  [(set_attr "slottable" "yes,yes,yes,no,yes,no,no")
   (set_attr "cc<cccc><ccnz><ccnzvc>"
	     "clobber,normal,normal,normal,clobber,normal,normal")])

(define_insn "*iorqi3<setcc><setnz><setnzvc>"
  [(set (match_operand:QI 0 "register_operand"	       "=r,r,r, r,r,r")
	(ior:QI (match_operand:QI 1 "register_operand" "%0,0,0, 0,0,r")
		(match_operand:QI 2 "general_operand"   "I,r,Q>,O,g,!To")))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  ""
  "@
   orq %2,%0
   or.b %2,%0
   or.b %2,%0
   orQ %b2,%0
   or.b %2,%0
   or.b %2,%1,%0"
  [(set_attr "slottable" "yes,yes,yes,yes,no,no")
   (set_attr "cc<cccc><ccnz><ccnzvc>"
	     "clobber,normal,normal,clobber,normal,normal")])

;; Exclusive-or

;; See comment about "anddi3" for xordi3 - no need for such a pattern.
;; FIXME: Do we really need the shorter variants?

(define_insn "<acc><anz><anzvc>xorsi3<setcc><setnz><setnzvc>"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(xor:SI (match_operand:SI 1 "register_operand" "%0")
		(match_operand:SI 2 "register_operand" "r")))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  ""
  "xor %2,%0"
  [(set_attr "slottable" "yes")])

(define_insn "xor<mode>3"
  [(set (match_operand:BW 0 "register_operand" "=r")
	(xor:BW (match_operand:BW 1 "register_operand" "%0")
		(match_operand:BW 2 "register_operand" "r")))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  ""
  "xor %2,%0"
  [(set_attr "slottable" "yes")
   (set_attr "cc" "clobber")])

;; Negation insns.

;; Questionable use, here mostly as a (slightly usable) define_expand
;; example.

(define_expand "negsf2"
  [(parallel
    [(set (match_dup 2) (match_dup 3))
     (clobber (reg:CC CRIS_CC0_REGNUM))])
   (parallel [(set (match_operand:SF 0 "register_operand")
		   (neg:SF (match_operand:SF 1 "register_operand")))
	      (use (match_dup 2))
	      (clobber (reg:CC CRIS_CC0_REGNUM))])]
  ""
{
  operands[2] = gen_reg_rtx (SImode);
  operands[3] = GEN_INT (1 << 31);
})

(define_insn "*expanded_negsf2"
  [(set (match_operand:SF 0 "register_operand" "=r")
	(neg:SF (match_operand:SF 1 "register_operand" "0")))
   (use (match_operand:SI 2 "register_operand" "r"))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  ""
  "xor %2,%0"
  [(set_attr "slottable" "yes")])

;; No "negdi2" although we could make one up that may be faster than
;; the one in libgcc.

(define_insn "<anz>neg<mode>2<setnz>"
  [(set (match_operand:BWD 0 "register_operand" "=r")
	(neg:BWD (match_operand:BWD 1 "register_operand" "r")))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  ""
  "neg<m> %1,%0"
  [(set_attr "slottable" "yes")])

;; One-complements.

;; See comment on anddi3 - no need for a DImode pattern.
;; See also xor comment.

(define_insn "<acc><anz><anzvc>one_cmplsi2<setcc><setnz><setnzvc>"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(not:SI (match_operand:SI 1 "register_operand" "0")))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  ""
  "not %0"
  [(set_attr "slottable" "yes")])

(define_insn "one_cmpl<mode>2"
  [(set (match_operand:BW 0 "register_operand" "=r")
	(not:BW (match_operand:BW 1 "register_operand" "0")))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  ""
  "not %0"
  [(set_attr "slottable" "yes")
   (set_attr "cc" "clobber")])

;; Arithmetic/Logical shift right (and SI left).

(define_insn "<acc><anz><anzvc><shlr>si3<setcc><setnz><setnzvc>"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(shift:SI (match_operand:SI 1 "register_operand" "0")
		  (match_operand:SI 2 "nonmemory_operand" "Kcr")))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  ""
{
  if (REG_S_P (operands[2]))
    return "<slr>.d %2,%0";

  return "<slr>q %2,%0";
}
  [(set_attr "slottable" "yes")])

;; Since gcc gets lost, and forgets to zero-extend the source (or mask
;; the destination) when it changes shifts of lower modes into SImode,
;; it is better to make these expands an anonymous patterns instead of
;; the more correct define_insns.  This occurs when gcc thinks that is
;; is better to widen to SImode and use immediate shift count.

;; FIXME: Is this legacy or still true for gcc >= 2.7.2?

;; FIXME: Can't parametrize sign_extend and zero_extend (before
;; mentioning "shiftrt"), so we need two patterns.
(define_expand "ashr<mode>3"
  [(parallel
    [(set (match_dup 3)
	  (sign_extend:SI (match_operand:BW 1 "nonimmediate_operand")))
     (clobber (reg:CC CRIS_CC0_REGNUM))])
   (parallel
    [(set (match_dup 4)
	  (zero_extend:SI (match_operand:BW 2 "nonimmediate_operand")))
     (clobber (reg:CC CRIS_CC0_REGNUM))])
   (parallel
    [(set (match_dup 5) (ashiftrt:SI (match_dup 3) (match_dup 4)))
     (clobber (reg:CC CRIS_CC0_REGNUM))])
   (parallel
    [(set (match_operand:BW 0 "general_operand")
	  (subreg:BW (match_dup 5) 0))
     (clobber (reg:CC CRIS_CC0_REGNUM))])]
  ""
{
  int i;

  for (i = 3; i < 6; i++)
    operands[i] = gen_reg_rtx (SImode);
})

(define_expand "lshr<mode>3"
  [(parallel
    [(set (match_dup 3)
	  (zero_extend:SI (match_operand:BW 1 "nonimmediate_operand")))
     (clobber (reg:CC CRIS_CC0_REGNUM))])
   (parallel
    [(set (match_dup 4)
	  (zero_extend:SI (match_operand:BW 2 "nonimmediate_operand")))
     (clobber (reg:CC CRIS_CC0_REGNUM))])
   (parallel
    [(set (match_dup 5) (lshiftrt:SI (match_dup 3) (match_dup 4)))
     (clobber (reg:CC CRIS_CC0_REGNUM))])
   (parallel
    [(set (match_operand:BW 0 "general_operand")
	  (subreg:BW (match_dup 5) 0))
     (clobber (reg:CC CRIS_CC0_REGNUM))])]
  ""
{
  int i;

  for (i = 3; i < 6; i++)
    operands[i] = gen_reg_rtx (SImode);
})

(define_insn "*expanded_<shlr><mode>"
  [(set (match_operand:BW 0 "register_operand" "=r")
	(shiftrt:BW (match_operand:BW 1 "register_operand" "0")
		    (match_operand:BW 2 "register_operand" "r")))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  ""
  "<slr><m> %2,%0"
  [(set_attr "slottable" "yes")])

(define_insn "*<shlr><mode>_lowpart"
  [(set (strict_low_part (match_operand:BW 0 "register_operand" "+r"))
	(shiftrt:BW (match_dup 0)
		    (match_operand:BW 1 "register_operand" "r")))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  ""
  "<slr><m> %1,%0"
  [(set_attr "slottable" "yes")])

;; Arithmetic/logical shift left.

;; For narrower modes than SI, we can use lslq although it makes cc
;; unusable.  The win is that we do not have to reload the shift-count
;; into a register.

(define_insn "ashl<mode>3"
  [(set (match_operand:BW 0 "register_operand" "=r,r")
	(ashift:BW (match_operand:BW 1 "register_operand" "0,0")
		   (match_operand:BW 2 "nonmemory_operand" "r,Kc")))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  ""
{
  return
    (CONST_INT_P (operands[2]) && INTVAL (operands[2]) > <nbitsm1>)
    ? "moveq 0,%0"
    : (CONSTANT_P (operands[2])
       ? "lslq %2,%0" : "lsl<m> %2,%0");
}
  [(set_attr "slottable" "yes")
   (set_attr "cc" "*,clobber")])

;; A strict_low_part matcher.

(define_insn "*ashl<mode>_lowpart"
  [(set (strict_low_part (match_operand:BW 0 "register_operand" "+r"))
	(ashift:BW (match_dup 0)
		   (match_operand:HI 1 "register_operand" "r")))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  ""
  "lsl<m> %1,%0"
  [(set_attr "slottable" "yes")])

;; Various strange insns that gcc likes.

;; Fortunately, it is simple to construct an abssf (although it may not
;; be very much used in practice).

(define_insn "abssf2"
  [(set (match_operand:SF 0 "register_operand" "=r")
	(abs:SF (match_operand:SF 1 "register_operand" "0")))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  ""
  "lslq 1,%0\;lsrq 1,%0")

(define_insn "abssi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(abs:SI (match_operand:SI 1 "register_operand" "r")))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  ""
  "abs %1,%0"
  [(set_attr "slottable" "yes")])

;; FIXME: GCC should be able to do these expansions itself.

(define_expand "abs<mode>2"
  [(parallel
    [(set (match_dup 2)
	  (sign_extend:SI (match_operand:BW 1 "general_operand")))
     (clobber (reg:CC CRIS_CC0_REGNUM))])
   (parallel
    [(set (match_dup 3) (abs:SI (match_dup 2)))
     (clobber (reg:CC CRIS_CC0_REGNUM))])
   (parallel
    [(set (match_operand:BW 0 "register_operand")
	  (subreg:BW (match_dup 3) 0))
     (clobber (reg:CC CRIS_CC0_REGNUM))])]
  ""
  "operands[2] = gen_reg_rtx (SImode); operands[3] = gen_reg_rtx (SImode);")

(define_insn "<acc><anz><anzvc>clzsi2<setcc><setnz><setnzvc>"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(clz:SI (match_operand:SI 1 "register_operand" "r")))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  "TARGET_HAS_LZ"
  "lz %1,%0"
  [(set_attr "slottable" "yes")])

(define_insn "<acc><anz><anzvc>bswapsi2<setcc><setnz><setnzvc>"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(bswap:SI (match_operand:SI 1 "register_operand" "0")))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  "TARGET_HAS_SWAP"
  "swapwb %0"
  [(set_attr "slottable" "yes")])

;; This instruction swaps all bits in a register.
;; That means that the most significant bit is put in the place
;; of the least significant bit, and so on.

(define_insn "cris_swap_bits"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI [(match_operand:SI 1 "register_operand" "0")]
		   CRIS_UNSPEC_SWAP_BITS))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  "TARGET_HAS_SWAP"
  "swapwbr %0"
  [(set_attr "slottable" "yes")])

;; Implement ctz using two instructions, one for bit swap and one for clz.
;; Defines a scratch register to avoid clobbering input.

(define_expand "ctzsi2"
  [(parallel
    [(set (match_dup 2)
	  (match_operand:SI 1 "register_operand"))
     (clobber (reg:CC CRIS_CC0_REGNUM))])
   (parallel
    [(set (match_dup 2)
	  (unspec:SI [(match_dup 2)] CRIS_UNSPEC_SWAP_BITS))
     (clobber (reg:CC CRIS_CC0_REGNUM))])
   (parallel
    [(set (match_operand:SI 0 "register_operand")
	  (clz:SI (match_dup 2)))
     (clobber (reg:CC CRIS_CC0_REGNUM))])]
  "TARGET_HAS_LZ && TARGET_HAS_SWAP"
  "operands[2] = gen_reg_rtx (SImode);")

;; Bound-insn.  Defined to be the same as an unsigned minimum, which is an
;; operation supported by gcc.  Used in casesi, but used now and then in
;; normal code too.

(define_expand "uminsi3"
  [(parallel
    [(set (match_operand:SI 0 "register_operand")
	  (umin:SI  (match_operand:SI 1 "register_operand")
		    (match_operand:SI 2 "general_operand")))
     (clobber (reg:CC CRIS_CC0_REGNUM))])]
  ""
  "")

(define_insn "*uminsi3<setcc><setnz><setnzvc>"
  [(set (match_operand:SI 0 "register_operand"		 "=r,r, r,r")
	(umin:SI  (match_operand:SI 1 "register_operand" "%0,0, 0,r")
		  (match_operand:SI 2 "general_operand"   "r,Q>,g,!To")))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  ""
{
  if (CONST_INT_P (operands[2]))
    {
      /* Constant operands are zero-extended, so only 32-bit operands
	 may be negative.  */
      if (INTVAL (operands[2]) >= 0)
	{
	  if (INTVAL (operands[2]) < 256)
	    return "bound.b %2,%0";

	  if (INTVAL (operands[2]) < 65536)
	    return "bound.w %2,%0";
	}
    }
  else if (which_alternative == 3)
    return "bound.d %2,%1,%0";

  return "bound.d %2,%0";
}
 [(set_attr "slottable" "yes,yes,no,no")])

;; Jump and branch insns.

(define_insn "jump"
  [(set (pc)
	(label_ref (match_operand 0 "" "")))]
  ""
  "ba %l0%#"
  [(set_attr "slottable" "has_slot")])

;; Testcase gcc.c-torture/compile/991213-3.c fails if we allow a constant
;; here, since the insn is not recognized as an indirect jump by
;; jmp_uses_reg_or_mem used by computed_jump_p.  Perhaps it is a kludge to
;; change from general_operand to nonimmediate_operand (at least the docs
;; should be changed), but then again the pattern is called indirect_jump.
(define_expand "indirect_jump"
  [(set (pc) (match_operand:SI 0 "nonimmediate_operand"))]
  ""
  "")

(define_insn "*indirect_jump"
  [(set (pc) (match_operand:SI 0 "nonimmediate_operand" "rm"))]
  ""
  "jump %0")

;; Return insn.  Used whenever the epilogue is very simple; if it is only
;; a single ret or jump [sp+].  No allocated stack space or saved
;; registers are allowed.
;; Note that for this pattern, although named, it is ok to check the
;; context of the insn in the test, not only compiler switches.

(define_expand "return"
  [(return)]
  "cris_simple_epilogue ()"
  "cris_expand_return (cris_return_address_on_stack ()); DONE;")

(define_insn "*return_expanded"
  [(return)]
  ""
{
  return cris_return_address_on_stack_for_return ()
    ? "jump [$sp+]" : "ret%#";
}
  [(set (attr "slottable")
 	(if_then_else
 	 (match_test "cris_return_address_on_stack_for_return ()")
 	 (const_string "no")
	 (const_string "has_return_slot")))])

(define_expand "prologue"
  [(const_int 0)]
  ""
  "cris_expand_prologue (); DONE;")

(define_expand "epilogue"
  [(const_int 0)]
  ""
  "cris_expand_epilogue (); DONE;")

;; Conditional branches.

(define_expand "cbranch<mode>4"
  [(parallel
    [(set (pc)
	  (if_then_else
	   (match_operator 0 "ordered_comparison_operator"
	    [(match_operand:BWDD 1 "nonimmediate_operand")
	     (match_operand:BWDD 2 "general_operand")])
	   (label_ref (match_operand 3 ""))
	   (pc)))
     (clobber (reg:CC CRIS_CC0_REGNUM))])]
  ""
  "cris_reduce_compare (&operands[0], &operands[1], &operands[2]);")

(define_insn_and_split "*cbranch<mode><code>4"
  [(set (pc)
	(if_then_else
	 (cond
	  (match_operand:BWDD 0 "nonimmediate_operand" "<cmp_op0c>")
	  (match_operand:BWDD 1 "general_operand" "<cmp_op1c>"))
	 (label_ref (match_operand 2 ""))
	 (pc)))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  ""
  "#"
  "&& reload_completed"
  [(set (reg:<xCC> CRIS_CC0_REGNUM)
	(compare:<xCC> (match_dup 0) (match_dup 1)))
   (set (pc)
	(if_then_else (cond (reg:<xCC> CRIS_CC0_REGNUM) (const_int 0))
		      (label_ref (match_dup 2))
		      (pc)))]
  "")

;; Test a single bit at operand[0] against 0/non-0.
(define_insn_and_split "*cbranch<mode>4_btstrq1_<CC>"
  [(set (pc)
	(if_then_else
	 (zcond
	  (zero_extract:BWD
	   (match_operand:BWD 0 "register_operand" "r,r")
	   (const_int 1)
	   (match_operand:SI 1 "nonmemory_operand" "Kc,r"))
	  (const_int 0))
	 (label_ref (match_operand 2 ""))
	 (pc)))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  ""
  "#"
  "&& reload_completed"
  [(set (reg:CC_ZnN CRIS_CC0_REGNUM)
	(compare:CC_ZnN
	 (zero_extract:SI (match_dup 0) (const_int 1) (match_dup 1))
	 (const_int 0)))
   (set (pc)
	(if_then_else (zcond (reg:CC_ZnN CRIS_CC0_REGNUM) (const_int 0))
		      (label_ref (match_dup 2))
		      (pc)))]
  "")

;; Test a field of bits starting at bit 0 against 0/non-0.
(define_insn_and_split "*cbranch<mode>4_btstqb0_<CC>"
  [(set (pc)
	(if_then_else
	 (zcond
	  (zero_extract:BWD
	   (match_operand:BWD 0 "register_operand" "r")
	   (match_operand 1 "const_int_operand" "Kc")
	   (const_int 0))
	  (const_int 0))
	 (label_ref (match_operand 2 ""))
	 (pc)))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  ""
  "#"
  "&& reload_completed"
  [(set (reg:CC_NZ CRIS_CC0_REGNUM)
	(compare:CC_NZ
	 (zero_extract:SI (match_dup 0) (match_dup 1) (const_int 0))
	 (const_int 0)))
   (set (pc)
	(if_then_else (zcond (reg:CC_NZ CRIS_CC0_REGNUM) (const_int 0))
		      (label_ref (match_dup 2))
		      (pc)))]
  "")


;; We suffer from the same overflow-bit-gets-in-the-way problem as
;; e.g. m68k, so we have to check if overflow bit is set on all "signed"
;; conditions.

(define_insn "*b<zcond:code><mode>"
  [(set (pc)
	(if_then_else (zcond (reg:ZnNNZUSE CRIS_CC0_REGNUM)
			     (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  "reload_completed"
{
  return <MODE>mode == CC_ZnNmode ? "b<znnCC> %l0%#" : "b<CC> %l0%#";
}
  [(set_attr "slottable" "has_slot")])

(define_insn "*b<nzvccond:code><mode>"
  [(set (pc)
	(if_then_else (nzvccond (reg:NZVCUSE CRIS_CC0_REGNUM)
			     (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  "reload_completed"
  "b<CC> %l0%#"
  [(set_attr "slottable" "has_slot")])

(define_insn "*b<rnzcond:code><mode>"
  [(set (pc)
	(if_then_else (rnzcond (reg:NZUSE CRIS_CC0_REGNUM)
			     (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  "reload_completed"
{
  return <MODE>mode == CC_NZmode ? "b<oCC> %l0%#": "b<CC> %l0%#";
}
  [(set_attr "slottable" "has_slot")])

;; Reversed anonymous patterns to the ones above, as mandated.

(define_insn "*b<nzcond:code>_reversed<mode>"
  [(set (pc)
	(if_then_else (nzcond (reg:ZnNNZUSE CRIS_CC0_REGNUM)
			      (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  "reload_completed"
{
  return <MODE>mode == CC_ZnNmode ? "b<rznnCC> %l0%#" : "b<rCC> %l0%#";
}
  [(set_attr "slottable" "has_slot")])

(define_insn "*b<nzvccond:code>_reversed<mode>"
  [(set (pc)
	(if_then_else (nzvccond (reg:NZVCUSE CRIS_CC0_REGNUM)
			     (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  "reload_completed"
  "b<rCC> %l0%#"
  [(set_attr "slottable" "has_slot")])

(define_insn "*b<rnzcond:code>_reversed<mode>"
  [(set (pc)
	(if_then_else (rnzcond (reg:NZUSE CRIS_CC0_REGNUM)
			     (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  "reload_completed"
{
  return <MODE>mode == CC_NZmode ? "b<roCC> %l0%#" : "b<rCC> %l0%#";
}
  [(set_attr "slottable" "has_slot")])

;; Set on condition: sCC.

(define_expand "cstore<mode>4"
  [(parallel
    [(set (match_operand:SI 0 "register_operand")
	  (match_operator:SI 1 "ordered_comparison_operator"
	   [(match_operand:BWDD 2 "nonimmediate_operand")
	    (match_operand:BWDD 3 "general_operand")]))
     (clobber (reg:CC CRIS_CC0_REGNUM))])]
  ""
  "cris_reduce_compare (&operands[1], &operands[2], &operands[3]);")

(define_insn_and_split "*cstore<mode><code>4"
  [(set (match_operand:SI 0 "register_operand" "=<sCC_destc>")
	(cond:SI
	 (match_operand:BWDD 1 "nonimmediate_operand" "<cmp_op0c>")
	 (match_operand:BWDD 2 "general_operand" "<cmp_op1c>")))
   (clobber (reg:CC CRIS_CC0_REGNUM))]
  ""
  "#"
  "&& reload_completed"
  [(set (reg:<xCC> CRIS_CC0_REGNUM)
	(compare:<xCC> (match_dup 1) (match_dup 2)))
   (set (match_dup 0)
	(cond:SI (reg:<xCC> CRIS_CC0_REGNUM) (const_int 0)))]
  "")

;; Like bCC, we have to check the overflow bit for
;; signed conditions.

(define_insn "*s<nzcond:code><mode>"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(nzcond:SI (reg:NZUSE CRIS_CC0_REGNUM) (const_int 0)))]
  "reload_completed"
  "s<CC> %0"
  [(set_attr "slottable" "yes")
   (set_attr "cc" "none")])

(define_insn "*s<rnzcond:code><mode>"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(rnzcond:SI (reg:NZUSE CRIS_CC0_REGNUM) (const_int 0)))]
  "reload_completed"
{
  return <MODE>mode == CC_NZmode ? "s<oCC> %0" : "s<CC> %0";
}
  [(set_attr "slottable" "yes")
   (set_attr "cc" "none")])

(define_insn "*s<nzvccond:code><mode>"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(nzvccond:SI (reg:NZVCUSE CRIS_CC0_REGNUM) (const_int 0)))]
  "reload_completed"
  "s<CC> %0"
  [(set_attr "slottable" "yes")
   (set_attr "cc" "none")])

;; Call insns.

;; We need to make these patterns "expand", since the real operand is
;; hidden in a (mem:QI ) inside operand[0] (call_value: operand[1]),
;; and cannot be checked if it were a "normal" pattern.
;;  Note that "call" and "call_value" are *always* called with a
;; mem-operand for operand 0 and 1 respective.  What happens for combined
;; instructions is a different issue.

(define_expand "call"
  [(parallel [(call (match_operand:SI 0 "indirect_operand")
		    (match_operand 1 "general_operand"))
	      (clobber (reg:SI CRIS_SRP_REGNUM))])]
  ""
{
  operands[1] = const0_rtx;
})

;; Accept operands for operand 0 in order of preference.

(define_insn "*expanded_call"
  [(call (mem:QI (match_operand:SI 0 "general_operand" "r,Q>,g"))
	 (const_int 0))
   (clobber (reg:SI CRIS_SRP_REGNUM))]
  ""
  "jsr %0")

(define_expand "call_value"
  [(parallel [(set (match_operand 0 "")
		   (call (match_operand:SI 1 "indirect_operand")
			 (match_operand 2 "")))
	      (clobber (reg:SI CRIS_SRP_REGNUM))])]
  ""
{
  operands[2] = const0_rtx;
})

;; The validity other than "general" of
;; operand 0 will be checked elsewhere.  Accept operands for operand 1 in
;; order of preference (Q includes r, but r is shorter, faster).
;;  We also accept a PLT symbol.  We output it as [rPIC+sym:GOTPLT] rather
;; than requiring getting rPIC + sym:PLT into a register.

(define_insn "*expanded_call_value"
  [(set (match_operand 0 "nonimmediate_operand" "=g,g,g")
	(call (mem:QI (match_operand:SI 1 "general_operand" "r,Q>,g"))
	      (const_int 0)))
   (clobber (reg:SI CRIS_SRP_REGNUM))]
  ""
  "Jsr %1"
  [(set_attr "cc" "clobber")])

;; Used in debugging.  No use for the direct pattern; unfilled
;; delayed-branches are taken care of by other means.

(define_insn "nop"
  [(const_int 0)]
  ""
  "nop"
  [(set_attr "cc" "none")])

;; Same as the gdb trap breakpoint: would cause a SIGTRAP for
;; cris-linux* and will work in freestanding environments with
;; sufficient framework.
(define_insn "trap"
  [(trap_if (const_int 1) (const_int 8))]
  "TARGET_TRAP_USING_BREAK8"
  "break 8")

;; We need to stop accesses to the stack after the memory is
;; deallocated.  Unfortunately, reorg doesn't look at naked clobbers,
;; e.g. (insn ... (clobber (mem:BLK (stack_pointer_rtx)))) and we don't
;; want to use a naked (unspec_volatile) as that would stop any
;; scheduling in the epilogue.  Hence we model it as a "real" insn that
;; sets the memory in an unspecified manner.  FIXME: Unfortunately it
;; still has the effect of an unspec_volatile.
(define_insn "cris_frame_deallocated_barrier"
  [(set (mem:BLK (reg:SI CRIS_SP_REGNUM))
	(unspec:BLK [(const_int 0)] CRIS_UNSPEC_FRAME_DEALLOC))]
  ""
  ""
  [(set_attr "length" "0")])

;; We expand on casesi so we can use "bound" and "add offset fetched from
;; a table to pc" (adds.w [pc+%0.w],pc).

;; Note: if you change the "parallel" (or add anything after it) in
;; this expansion, you must change the macro ASM_OUTPUT_CASE_END
;; accordingly, to add the default case at the end of the jump-table.

(define_expand "casesi"
  [(parallel
    [(set (match_dup 5) (match_operand:SI 0 "general_operand"))
     (clobber (reg:CC CRIS_CC0_REGNUM))])
   (parallel
    [(set (match_dup 6)
	  (minus:SI (match_dup 5)
		    (match_operand:SI 1 "const_int_operand")))
     (clobber (reg:CC CRIS_CC0_REGNUM))])
   (parallel
    [(set (match_dup 7)
	  (umin:SI (match_dup 6)
		   (match_operand:SI 2 "const_int_operand")))
     (clobber (reg:CC CRIS_CC0_REGNUM))])
   (parallel
    [(set (pc)
	  (if_then_else
	   (ltu (match_dup 7) (match_dup 2))
	   (plus:SI (sign_extend:SI
		     (mem:HI
		      (plus:SI (mult:SI (match_dup 7) (const_int 2))
			       (pc))))
		    (pc))
	   (label_ref (match_operand 4 ""))))
     (use (label_ref (match_operand 3 "")))
     (clobber (reg:CC CRIS_CC0_REGNUM))])]
  ""
{
  operands[2] = plus_constant (SImode, operands[2], 1);
  operands[5] = gen_reg_rtx (SImode);
  operands[6] = gen_reg_rtx (SImode);
  operands[7] = gen_reg_rtx (SImode);
})

(include "sync.md")

;; Various peephole optimizations.
;;
;; Do not add patterns that you do not know will be matched.
;; Please also add a self-contained testcase.

;; We have trouble with and:s and shifts.  Maybe something is broken in
;; gcc?  Or it could just be that bit-field insn expansion is a bit
;; suboptimal when not having extzv insns.  Or combine being over-eager
;; to canonicalize to "and", and ignorant on the benefits of the right
;; mixture of "and" and "zero-extend".

;; Testcase for the following peephole: gcc.target/cris/peep2-movulsr.c

;; Where equivalent and where the "and" argument doesn't fit "andq" but
;; is 16 bits or smaller, replace the "and" with a zero-extend preceding
;; the shift.  A zero-extend is shorter and faster than "and" with a
;; 32-bit argument.

(define_peephole2 ; movulsr
  [(parallel
    [(set (match_operand:SI 0 "register_operand")
	  (lshiftrt:SI (match_dup 0)
		       (match_operand:SI 1 "const_int_operand")))
     (clobber (reg:CC CRIS_CC0_REGNUM))])
   (parallel
    [(set (match_dup 0)
	  (and:SI (match_dup 0)
		  (match_operand 2 "const_int_operand")))
     (clobber (reg:CC CRIS_CC0_REGNUM))])]
  "INTVAL (operands[2]) > 31 && INTVAL (operands[2]) <= 0xffff
   && (((INTVAL (operands[2]) <= 0xff ? 0xff : 0xffff) >> INTVAL (operands[1]))
       == INTVAL (operands[2]))"
  [(parallel
    ;; The zero-extend is expressed as an "and", only because that's easier
    ;; than messing with zero-extend of a subreg.
    [(set (match_dup 0) (and:SI (match_dup 0) (match_dup 3)))
     (clobber (reg:CC CRIS_CC0_REGNUM))])
   (parallel
    [(set (match_dup 0) (lshiftrt:SI (match_dup 0) (match_dup 1)))
     (clobber (reg:CC CRIS_CC0_REGNUM))])]
{
  operands[3]
    = INTVAL (operands[2]) <= 0xff ? GEN_INT (0xff) :  GEN_INT (0xffff);
})

;; Testcase for the following four peepholes: gcc.target/cris/peep2-xsrand.c

(define_peephole2 ; asrandb
  [(parallel
    [(set (match_operand:SI 0 "register_operand")
	  (ashiftrt:SI (match_dup 0)
		       (match_operand:SI 1 "const_int_operand")))
     (clobber (reg:CC CRIS_CC0_REGNUM))])
   (parallel
    [(set (match_dup 0)
	(and:SI (match_dup 0)
		(match_operand 2 "const_int_operand")))
     (clobber (reg:CC CRIS_CC0_REGNUM))])]
  "INTVAL (operands[2]) > 31
   && INTVAL (operands[2]) < 255
   && INTVAL (operands[1]) > 23
   /* Check that the and-operation enables us to use logical-shift.  */
   && (INTVAL (operands[2])
       & ((HOST_WIDE_INT) (HOST_WIDE_INT_M1U
			   << (32 - INTVAL (operands[1]))))) == 0"
  [(parallel
    [(set (match_dup 0) (lshiftrt:SI (match_dup 0) (match_dup 1)))
     (clobber (reg:CC CRIS_CC0_REGNUM))])
   (parallel
    [(set (match_dup 3) (and:QI (match_dup 3) (match_dup 4)))
     (clobber (reg:CC CRIS_CC0_REGNUM))])]
{
  operands[3] = gen_rtx_REG (QImode, REGNO (operands[0]));
  operands[4] = GEN_INT (trunc_int_for_mode (INTVAL (operands[2]), QImode));
})

(define_peephole2 ; asrandw
  [(parallel
    [(set (match_operand:SI 0 "register_operand")
	  (ashiftrt:SI (match_dup 0)
		       (match_operand:SI 1 "const_int_operand")))
     (clobber (reg:CC CRIS_CC0_REGNUM))])
   (parallel
    [(set (match_dup 0)
	(and:SI (match_dup 0) (match_operand 2 "const_int_operand")))
     (clobber (reg:CC CRIS_CC0_REGNUM))])]
  "INTVAL (operands[2]) > 31
   && INTVAL (operands[2]) < 65535
   && INTVAL (operands[2]) != 255
   && INTVAL (operands[1]) > 15
   /* Check that the and-operation enables us to use logical-shift.  */
   && (INTVAL (operands[2])
       & ((HOST_WIDE_INT) (HOST_WIDE_INT_M1U
			   << (32 - INTVAL (operands[1]))))) == 0"
  [(parallel
    [(set (match_dup 0) (lshiftrt:SI (match_dup 0) (match_dup 1)))
     (clobber (reg:CC CRIS_CC0_REGNUM))])
   (parallel
    [(set (match_dup 3) (and:HI (match_dup 3) (match_dup 4)))
     (clobber (reg:CC CRIS_CC0_REGNUM))])]
{
  operands[3] = gen_rtx_REG (HImode, REGNO (operands[0]));
  operands[4] = GEN_INT (trunc_int_for_mode (INTVAL (operands[2]), HImode));
})

(define_peephole2 ; lsrandb
  [(parallel
    [(set (match_operand:SI 0 "register_operand")
	  (lshiftrt:SI (match_dup 0)
		       (match_operand:SI 1 "const_int_operand")))
     (clobber (reg:CC CRIS_CC0_REGNUM))])
   (parallel
    [(set (match_dup 0)
	  (and:SI (match_dup 0) (match_operand 2 "const_int_operand")))
     (clobber (reg:CC CRIS_CC0_REGNUM))])]
  "INTVAL (operands[2]) > 31
   && INTVAL (operands[2]) < 255
   && INTVAL (operands[1]) > 23"
  [(parallel
    [(set (match_dup 0) (lshiftrt:SI (match_dup 0) (match_dup 1)))
     (clobber (reg:CC CRIS_CC0_REGNUM))])
   (parallel
    [(set (match_dup 3) (and:QI (match_dup 3) (match_dup 4)))
     (clobber (reg:CC CRIS_CC0_REGNUM))])]
{
  operands[3] = gen_rtx_REG (QImode, REGNO (operands[0]));
  operands[4] = GEN_INT (trunc_int_for_mode (INTVAL (operands[2]), QImode));
})

(define_peephole2 ; lsrandw
  [(parallel
    [(set (match_operand:SI 0 "register_operand")
	(lshiftrt:SI (match_dup 0)
		     (match_operand:SI 1 "const_int_operand")))
     (clobber (reg:CC CRIS_CC0_REGNUM))])
   (parallel
    [(set (match_dup 0)
	(and:SI (match_dup 0) (match_operand 2 "const_int_operand")))
     (clobber (reg:CC CRIS_CC0_REGNUM))])]
  "INTVAL (operands[2]) > 31 && INTVAL (operands[2]) < 65535
   && INTVAL (operands[2]) != 255
   && INTVAL (operands[1]) > 15"
  [(parallel
    [(set (match_dup 0) (lshiftrt:SI (match_dup 0) (match_dup 1)))
     (clobber (reg:CC CRIS_CC0_REGNUM))])
   (parallel
    [(set (match_dup 3) (and:HI (match_dup 3) (match_dup 4)))
     (clobber (reg:CC CRIS_CC0_REGNUM))])]
{
  operands[3] = gen_rtx_REG (HImode, REGNO (operands[0]));
  operands[4] = GEN_INT (trunc_int_for_mode (INTVAL (operands[2]), HImode));
})

;; There seems to be no other way to make GCC (including 4.8/trunk at
;; r186932) optimally reload an instruction that looks like
;;   and.d reg_or_mem,const_32__65535,other_reg
;; where other_reg is the destination.
;; It should be:
;;   movu.[bw] reg_or_mem,reg_32
;;   and.[bw] trunc_int_for_mode([bw], const_32__65535),reg_32 ;; or andq
;; but it turns into:
;;   move.d reg_or_mem,reg_32
;;   and.d const_32__65535,reg_32
;; Fix it with these two peephole2's.
;; Testcases: gcc.target/cris/peep2-andu1.c gcc.target/cris/peep2-andu2.c

(define_peephole2 ; andu
  [(parallel
    [(set (match_operand:SI 0 "register_operand")
	  (match_operand:SI 1 "nonimmediate_operand"))
     (clobber (reg:CC CRIS_CC0_REGNUM))])
   (parallel
    [(set (match_operand:SI 2 "register_operand")
	  (and:SI (match_dup 0)
		  (match_operand:SI 3 "const_int_operand")))
     (clobber (reg:CC CRIS_CC0_REGNUM))])]
   ;; Since the size of the memory access could be made different here,
   ;; don't do this for a mem-volatile access.
  "REGNO (operands[2]) == REGNO (operands[0])
   && INTVAL (operands[3]) <= 65535 && INTVAL (operands[3]) >= 0
   && !satisfies_constraint_I (operands[3])
   && !side_effects_p (operands[1])
   && (!REG_P (operands[1])
       || REGNO (operands[1]) <= CRIS_LAST_GENERAL_REGISTER)"
  [(parallel
    [(set (match_dup 0) (match_dup 4))
     (clobber (reg:CC CRIS_CC0_REGNUM))])
   (parallel
    [(set (match_dup 5) (match_dup 6))
     (clobber (reg:CC CRIS_CC0_REGNUM))])]
{
  machine_mode zmode = INTVAL (operands[3]) <= 255 ? QImode : HImode;
  machine_mode amode
    = satisfies_constraint_O (operands[3]) ? SImode : zmode;
  rtx op1
    = (REG_S_P (operands[1])
       ? gen_rtx_REG (zmode, REGNO (operands[1]))
       : adjust_address (operands[1], zmode, 0));
  operands[4]
    = gen_rtx_ZERO_EXTEND (SImode, op1);
  operands[5] = gen_rtx_REG (amode, REGNO (operands[0]));
  operands[6]
    = gen_rtx_AND (amode, gen_rtx_REG (amode, REGNO (operands[0])),
		   GEN_INT (trunc_int_for_mode (INTVAL (operands[3]),
						amode == SImode
						? QImode : amode)));
})

;; Since r186861, gcc.target/cris/peep2-andu2.c trigs this pattern, with which
;; we fix up e.g.:
;;  movu.b 254,$r9.
;;  and.d $r10,$r9
;; into:
;;  movu.b $r10,$r9
;;  andq -2,$r9.
;; Only do this for values fitting the quick immediate operand.
(define_peephole2 ; andqu
  [(parallel
    [(set (match_operand:SI 0 "register_operand")
	  (match_operand:SI 1 "const_int_operand"))
     (clobber (reg:CC CRIS_CC0_REGNUM))])
   (parallel
    [(set (match_dup 0)
	  (and:SI (match_dup 0) (match_operand:SI 2 "nonimmediate_operand")))
     (clobber (reg:CC CRIS_CC0_REGNUM))])]
   ;; Since the size of the memory access will be made different here,
   ;; don't do this for a volatile access or a post-incremented address.
  "satisfies_constraint_O (operands[1])
   && !side_effects_p (operands[2])
   && !reg_overlap_mentioned_p (operands[0], operands[2])"
  [(parallel
    [(set (match_dup 0) (match_dup 3))
     (clobber (reg:CC CRIS_CC0_REGNUM))])
   (parallel
    [(set (match_dup 0) (and:SI (match_dup 0) (match_dup 4)))
     (clobber (reg:CC CRIS_CC0_REGNUM))])]
{
  machine_mode zmode = INTVAL (operands[1]) <= 255 ? QImode : HImode;
  rtx op1
    = (REG_S_P (operands[2])
       ? gen_rtx_REG (zmode, REGNO (operands[2]))
       : adjust_address (operands[2], zmode, 0));
  operands[3] = gen_rtx_ZERO_EXTEND (SImode, op1);
  operands[4] = GEN_INT (trunc_int_for_mode (INTVAL (operands[1]), QImode));
})

;; Local variables:
;; mode:emacs-lisp
;; comment-start: ";; "
;; eval: (set-syntax-table (copy-sequence (syntax-table)))
;; eval: (modify-syntax-entry ?[ "(]")
;; eval: (modify-syntax-entry ?] ")[")
;; eval: (modify-syntax-entry ?{ "(}")
;; eval: (modify-syntax-entry ?} "){")
;; eval: (setq indent-tabs-mode t)
;; End:
