;; -*- Mode: Scheme -*-
;; GCC machine description for Ubicom IP2022 Communications Controller.
;; Copyright (C) 2000, 2001, 2002, 2004
;; Free Software Foundation, Inc.
;; Contributed by Red Hat, Inc and Ubicom, Inc.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING.  If not, write to
;; the Free Software Foundation, 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.  */

;; Default all instruction lengths to two bytes (one 16-bit instruction).
;;
(define_attr "length" "" (const_int 2))

;; Define if we can "skip" an insn or not
(define_attr "skip" "no,yes" (const_string "no"))

;; Define an insn clobbers WREG or not
(define_attr "clobberw" "no,yes" (const_string "yes"))

;; Performance Issues:
;;
;; With the IP2k only having one really useful pointer register we have to
;; make most of our instruction patterns only match one offsettable address
;; before addressing becomes strict whereas afterwards of course we can use
;; any register details that have become fixed.  As we've already committed
;; any reloads at this point of course we're a little late so we have to use
;; a number of peephole2 optimizations to remerge viable patterns.  We can
;; do a bit more tidying up in the machine-dependent reorg pass to try and
;; make things better still.  None of this is ideal, but it's *much* better
;; than nothing.

;; Constraints:
;;
;; I - -255..-1 - all other literal values have to be loaded
;; J - 0..7 - valid bit number in a register
;; K - 0..127 - valid offset for addressing mode
;; L - 1..127 - positive count suitable for shift.
;; M - -1 as a literal value
;; N - +1 as a literal value
;; O - zero
;; P - 0..255
;;
;; a - DP or IP registers (general address)
;; f - IP register
;; j - IPL register
;; k - IPH register
;; b - DP register
;; y - DPH register
;; z - DPL register
;; q - SP register
;; c - DP or SP registers (offsettable address)
;; d - non-pointer registers (not SP, DP, IP)
;; u - non-SP registers (everything except SP)
;;
;; R - Indirect through IP - Avoid this except for QI mode, since we
;;     can't access extra bytes.
;; S - Short (stack/dp address). Pointer with 0..127 displacement
;;     Note that 0(SP) has undefined contents due to post-decrement push
;; T - data-section immediate value.  A CONST_INT or SYMBOL_REF into .data

;; Special assembly-language format effectors:
;;
;; ABCD -
;;     Reference up to 4 big-endian registers - %A0 is Rn+0, while %D0 is Rn+3
;; STUVWXYZ -
;;     Reference up to 8 big-endian registers - %S0 is Rn+0, while %Z0 is Rn+7
;;
;; H - High part of 16 bit address or literal %hi8data(v) or %hi8insn(v)
;; L - Low part of 16 bit address or literal  %lo8data(v) or %lo8insn(v)
;; b - print a literal value with no punctuation (typically bit selector)
;; e - print 1 << v ('exponent')
;; n - print negative number
;; x - print 16 bit hex number
;; < - interior stack push; adjust any stack-relative operands accordingly
;; > - interior stack pop; clear adjustment.

;;
;; Basic operations to move data in and out of fr's.  Also extended to
;; cover the loading of w with immediates
;;

(define_insn "*movqi_w_gen"
  [(set (reg:QI 10)
        (match_operand:QI 0 "general_operand" "rSi"))]
  "(ip2k_reorg_split_qimode)"
  "mov\\tw,%0"
  [(set_attr "skip" "yes")])

(define_insn "*movqi_fr_w"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=rS")
        (reg:QI 10))]
  "(ip2k_reorg_split_qimode)"
  "mov\\t%0,w"
  [(set_attr "skip" "yes")
   (set_attr "clobberw" "no")])


;; Handle the cases where we get back to back redundant mov patterns issued.  
;; This of course sounds somewhat absurd but is actually reasonably common
;; because we aren't able to match certain patterns before registers are
;; chosen.  This is particularly true of memory to memory operations where
;; we can't provide patterns that will guarantee to match every time because
;; this would require reloads in the middle of instructions.  If we
;; discover a case that doesn't need a reload of course then this combiner
;; operation will tidy up for us.
;;
;; Warning!  Whilst it would be nice to match operand 0 as a general operand
;; we mustn't do so because this doesn't work with the REG_DEAD check.
;;
(define_peephole2
  [(set (match_operand 0 "ip2k_nonsp_reg_operand" "")
	(match_operand 1 "ip2k_gen_operand" ""))
   (set (match_operand 2 "ip2k_split_dest_operand" "")
        (match_dup 0))]
  "(peep2_reg_dead_p (2, operands[0])
    && ! (REG_P (operands[2]) && REGNO (operands[2]) == REG_SP)
    && (REG_P (operands[2])
        || ip2k_xexp_not_uses_reg_p (operands[2], REGNO (operands[0]),
                                     GET_MODE_SIZE (GET_MODE (operands[0])))))"
  [(set (match_dup 2)
	(match_dup 1))]
  "")

(define_peephole2
  [(set (match_operand 0 "ip2k_nonsp_reg_operand" "")
	(match_operand 1 "immediate_operand" ""))
   (set (match_operand 2 "ip2k_gen_operand" "")
        (match_dup 0))]
  "(peep2_reg_dead_p (2, operands[0])
    && ! (REG_P (operands[2]) && REGNO (operands[2]) == REG_SP)
    && ip2k_xexp_not_uses_reg_p (operands[2], REGNO (operands[0]),
                                 GET_MODE_SIZE (GET_MODE (operands[0]))))"
  [(set (match_dup 2)
	(match_dup 1))]
  "")

;;
;; Move 8-bit integers.
;;

(define_expand "movqi"
  [(set (match_operand:QI 0 "" "")
	(match_operand:QI 1 "" ""))]
  ""
  "")

(define_insn "*pushqi"
  [(set (match_operand:QI 0 "push_operand"   "=<")
	(match_operand:QI 1 "general_operand" "g"))]
  ""
  "push\\t%1"
  [(set_attr "skip" "yes")
   (set_attr "clobberw" "no")])

;; IP isn't offsettable but we can fake this behavior here and win if we would
;; otherwise use DP and require a reload from IP.  This instruction is only
;; matched by peephole2 operations.
;;
(define_insn "*movqi_to_ip_plus_offs"
  [(set (mem:QI (plus:HI (reg:HI 4)
			 (match_operand 0 "const_int_operand" "P,P")))
	(match_operand:QI 1 "general_operand"                 "O,g"))]
  "reload_completed && (INTVAL (operands[0]) < 0x100)"
  "*{
    if (INTVAL (operands[0]) == 1)
      OUT_AS1 (inc, ipl);
    else
      {
        OUT_AS2 (mov, w, %0);
        OUT_AS2 (add, ipl, w);
      }

    switch (which_alternative)
      {
      case 0:
        OUT_AS1 (clr, (IP));
	break;

      case 1:
        OUT_AS1 (push, %1%<);
        OUT_AS1 (pop, (IP)%>);
	break;
      }

    if (!find_regno_note (insn, REG_DEAD, REG_IP))
      {
        if (INTVAL (operands[0]) == 1)
          OUT_AS1 (dec, ipl);
        else
          OUT_AS2 (sub, ipl, w);
      }
    return \"\";
  }")

;; IP isn't offsettable but we can fake this behavior here and win if we would
;; otherwise use DP and require a reload from IP.  This instruction is only
;; matched by peephole2 operations.
;;
(define_insn "*movqi_from_ip_plus_offs"
  [(set (match_operand:QI 0 "nonimmediate_operand"           "=g")
        (mem:QI (plus:HI (reg:HI 4)
			 (match_operand 1 "const_int_operand" "P"))))]
  "reload_completed && (INTVAL (operands[1]) < 0x100)"
  "*{
    if (INTVAL (operands[1]) == 1)
      OUT_AS1 (inc, ipl);
    else
      {
        OUT_AS2 (mov, w, %1);
        OUT_AS2 (add, ipl, w);
      }
    OUT_AS1 (push, (IP)%<);
    OUT_AS1 (pop, %0%>);
    if (!find_regno_note (insn, REG_DEAD, REG_IP)
        && ip2k_xexp_not_uses_reg_p (operands[0], REG_IP, 2))
      {
        if (INTVAL (operands[1]) == 1)
          OUT_AS1 (dec, ipl);
        else
          OUT_AS2 (sub, ipl, w);
      }
    return \"\";
  }")

(define_insn_and_split "*movqi"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=roR,roR,r,  rS,roR")
	(match_operand:QI 1 "general_operand"       "  O, ri,o,rioR,rSi"))]
  ""
  "@
   clr\\t%0
   #
   #
   #
   #"
  "(ip2k_reorg_split_qimode
    && (GET_CODE (operands[1]) != CONST_INT
        || INTVAL (operands[1]) != 0))"
  [(set (reg:QI 10) (match_dup 1))
   (set (match_dup 0) (reg:QI 10))]
  ""
  [(set_attr "skip" "yes,no,no,no,no")
   (set_attr "clobberw" "no,yes,yes,yes,yes")])

(define_peephole2
  [(set (reg:HI 12)
  	(reg:HI 4))
   (set (match_operand:QI 0 "nonimmediate_operand" "")
   	(mem:QI (plus:HI (reg:HI 12)
		         (match_operand 1 "const_int_operand" ""))))]
  "((ip2k_reorg_in_progress || ip2k_reorg_completed)
    && peep2_regno_dead_p (2, REG_DP)
    && ip2k_xexp_not_uses_reg_p (operands[0], REG_DP, 2)
    && (INTVAL (operands[1]) < 0x100))"
  [(set (match_dup 0)
	(mem:QI (plus:HI (reg:HI 4)
			 (match_dup 1))))]
  "")

(define_peephole2
  [(set (reg:HI 12)
  	(reg:HI 4))
   (set (mem:QI (plus:HI (reg:HI 12)
		         (match_operand 0 "const_int_operand" "")))
	(match_operand:QI 1 "general_operand" ""))]
  "((ip2k_reorg_in_progress || ip2k_reorg_completed)
    && peep2_regno_dead_p (2, REG_DP)
    && ip2k_xexp_not_uses_reg_p (operands[0], REG_DP, 2)
    && (INTVAL (operands[0]) < 0x100))"
  [(set (mem:QI (plus:HI (reg:HI 4)
			 (match_dup 0)))
	(match_dup 1))]
  "")

(define_peephole2
  [(set (match_operand:QI 0 "register_operand" "")
   	(mem:QI (plus:HI (reg:HI 4)
			 (match_operand 1 "const_int_operand" ""))))
   (set (match_operand:QI 2 "nonimmediate_operand" "")
        (match_dup 0))]
  "((ip2k_reorg_in_progress || ip2k_reorg_completed)
    && peep2_reg_dead_p (2, operands[0]))"
  [(set (match_dup 2)
	(mem:QI (plus:HI (reg:HI 4)
			 (match_dup 1))))]
  "")

;; We sometimes want to copy a value twice, usually when we copy a value into
;; both a structure slot and into a temporary register.  We can win here
;; because gcc doesn't know about ways of reusing w while we're copying.
;;
(define_insn_and_split "*movqi_twice"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=g")
        (match_operand:QI 1 "general_operand"       "g"))
   (set (match_operand:QI 2 "nonimmediate_operand" "=g")
        (match_dup 1))]
  "ip2k_reorg_merge_qimode"
  "mov\\tw,%1\;mov\\t%0,w\;mov\\t%2,w"
  "(ip2k_reorg_split_qimode)"
  [(set (reg:QI 10) (match_dup 1))
   (set (match_dup 0) (reg:QI 10))
   (set (match_dup 2) (reg:QI 10))]
  "")

;; Don't try to match until we've removed redundant reloads.  Often this
;; simplification will remove the need to do two moves!
;;
(define_peephole2
  [(set (match_operand:QI 0 "nonimmediate_operand" "")
        (match_operand:QI 1 "general_operand" ""))
   (set (match_operand:QI 2 "nonimmediate_operand" "")
        (match_dup 0))]
  "(ip2k_reorg_merge_qimode
    && (GET_CODE (operands[1]) != CONST_INT || INTVAL (operands[1]) != 0))"
  [(parallel [(set (match_dup 0)
  		   (match_dup 1))
	      (set (match_dup 2)
	      	   (match_dup 1))])]
  "")

;; Don't try to match until we've removed redundant reloads.  Often this
;; simplification will remove the need to do two moves!
;;
(define_peephole2
  [(set (match_operand:QI 0 "nonimmediate_operand" "")
        (match_operand:QI 1 "general_operand" ""))
   (set (match_operand:QI 2 "nonimmediate_operand" "")
        (match_dup 1))]
  "(ip2k_reorg_merge_qimode
    && (GET_CODE (operands[1]) != CONST_INT || INTVAL (operands[1]) != 0))"
  [(parallel [(set (match_dup 0)
  		   (match_dup 1))
	      (set (match_dup 2)
	      	   (match_dup 1))])]
  "")

;;
;; Move 16-bit integers.
;;

(define_expand "movhi"
  [(set (match_operand:HI 0 "" "")
	(match_operand:HI 1 "" ""))]
  ""
  "")

(define_insn "*pushhi_ip"
  [(set (match_operand:HI 0 "push_operand"      "=<")
	(mem:HI (reg:HI 4)))]
  "reload_completed"
  "inc\\tipl\;push\\t(IP)\;dec\\tipl\;push\\t(IP)"
  [(set_attr "clobberw" "no")])

(define_insn "*movhi_to_ip"
  [(set (mem:HI (reg:HI 4))
  	(match_operand:HI 0 "general_operand" "O,roi"))]
  "reload_completed"
  "*{
    switch (which_alternative)
      {
      case 0:
        OUT_AS1 (clr, (IP));
	OUT_AS1 (inc, ipl);
	OUT_AS1 (clr, (IP));
        if (!find_regno_note (insn, REG_DEAD, REG_IP))
	  OUT_AS1 (dec, ipl);
	return \"\";

      case 1:
        OUT_AS2 (mov, w, %H0);
	OUT_AS2 (mov, (IP), w);
	OUT_AS2 (mov, w, %L0);
	OUT_AS1 (inc, ipl);
	OUT_AS2 (mov, (IP), w);
        if (!find_regno_note (insn, REG_DEAD, REG_IP))
	  OUT_AS1 (dec, ipl);
	return \"\";
      default:
        abort ();
      }
  }")

(define_insn "*movhi_from_ip"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=f,bqdo")
        (mem:HI (reg:HI 4)))]
  "reload_completed"
  "*{
    switch (which_alternative)
      {
      case 0:
        OUT_AS1 (push, (IP));
	OUT_AS1 (inc, ipl);
	OUT_AS2 (mov, w, (IP));
	OUT_AS2 (mov, ipl, w);
	OUT_AS1 (pop, iph);
	return \"\";

      case 1:
        OUT_AS2 (mov, w, (IP));
	OUT_AS2 (mov, %H0, w);
	OUT_AS1 (inc, ipl);
	OUT_AS2 (mov, w, (IP));
	OUT_AS2 (mov, %L0, w);
        if (!find_regno_note (insn, REG_DEAD, REG_IP))
	  OUT_AS1 (dec, ipl);
	return \"\";
      default:
        abort ();
      }
  }")

(define_insn "*movhi_from_ip_plus_offs"
  [(set (match_operand:HI 0 "nonimmediate_operand"           "=f,bqdo")
        (mem:HI (plus:HI (reg:HI 4)
			 (match_operand 1 "const_int_operand" "P,   P"))))]
  "reload_completed && (INTVAL (operands[1]) < 0x100)"
  "*{
    switch (which_alternative)
      {
      case 0:
        OUT_AS2 (mov, w, %1);
        OUT_AS2 (add, ipl, w);
        OUT_AS1 (push, (IP));
        OUT_AS1 (inc, ipl);
        OUT_AS2 (mov, w, (IP));
        OUT_AS2 (mov, ipl, w);
        OUT_AS1 (pop, iph);
        return \"\";

      case 1:
        if (INTVAL (operands[1]) == 1)
          OUT_AS1 (inc, ipl);
        else
          {
            OUT_AS2 (mov, w, %1);
            OUT_AS2 (add, ipl, w);
          }
        OUT_AS1 (push, (IP)%<);
	OUT_AS1 (pop, %H0%>);
        OUT_AS1 (inc, ipl);
        OUT_AS1 (push, (IP)%<);
        OUT_AS1 (pop, %L0%>);
        if (!find_regno_note (insn, REG_DEAD, REG_IP))
	  {
	    OUT_AS1 (dec, ipl);
            if (INTVAL (operands[1]) == 1)
              OUT_AS1 (dec, ipl);
            else
              OUT_AS2 (sub, ipl, w);
	  }
        return \"\";
      default:
        abort ();
      }
  }")

(define_insn_and_split "*movhi"
  [(set
    (match_operand:HI 0 "ip2k_split_dest_operand" "=<,<,uo,b, uS,uo,uo, q,u")
    (match_operand:HI 1 "general_operand"        "ron,i, n,T,uoi,uS,ui,ui,q"))]
  ""
  "@
   push\\t%L1%<\;push\\t%H1%>
   push\\t%L1%<\;push\\t%H1%>
   mov\\tw,%H1\;mov\\t%H0,w\;mov\\tw,%L1\;mov\\t%L0,w
   loadl\\t%x1\;loadh\\t%x1
   mov\\tw,%H1\;push\\t%L1%<\;pop\\t%L0%>\;mov\\t%H0,w
   mov\\tw,%H1\;push\\t%L1%<\;pop\\t%L0%>\;mov\\t%H0,w
   mov\\tw,%H1\;push\\t%L1%<\;pop\\t%L0%>\;mov\\t%H0,w
   mov\\tw,%H1\;mov\\t%H0,w\;mov\\tw,%L1\;mov\\t%L0,w
   mov\\tw,%H1\;mov\\t%H0,w\;mov\\tw,%L1\;mov\\t%L0,w"
  "(ip2k_reorg_split_himode
    && (GET_CODE (operands[1]) == CONST_INT
        || (push_operand (operands[0], HImode)
	    && GET_CODE (operands[1]) == REG)
	|| (register_operand (operands[0], HImode)
	    && REGNO (operands[0]) >= 0x80
	    && ip2k_gen_operand (operands[1], HImode))))"
  [(set (match_dup 2) (match_dup 3))
   (set (match_dup 4) (match_dup 5))]
  "{
     ip2k_split_words (QImode, HImode, operands);  /* Split into 2=3,4=5 */
  }"
  [(set_attr "clobberw" "no,no,yes,no,yes,yes,yes,yes,yes")])

;; We don't generally use IP for HImode indirections because it's not
;; offsettable, however if we're accessing something that's already pointed
;; to by IP and would otherwise require a reload of DP then we can win by
;; simulating HImode accesses via IP instead.

(define_peephole2
  [(set (reg:HI 12)
  	(reg:HI 4))
   (set (mem:HI (reg:HI 12))
   	(match_operand:HI 0 "general_operand" ""))]
  "((ip2k_reorg_in_progress || ip2k_reorg_completed)
    && ip2k_xexp_not_uses_reg_p (operands[0], REG_DP, 2)
    && peep2_regno_dead_p (2, REG_DP))"
  [(set (mem:HI (reg:HI 4))
  	(match_dup 0))]
  "")

(define_peephole2
  [(set (reg:HI 12)
  	(reg:HI 4))
   (set (match_operand:HI 0 "nonimmediate_operand" "")
   	(mem:HI (reg:HI 12)))]
  "((ip2k_reorg_in_progress || ip2k_reorg_completed)
    && ip2k_xexp_not_uses_reg_p (operands[0], REG_DP, 2)
    && peep2_regno_dead_p (2, REG_DP))"
  [(set (match_dup 0)
	(mem:HI (reg:HI 4)))]
  "")

(define_peephole2
  [(set (reg:HI 12)
  	(reg:HI 4))
   (set (match_operand:HI 0 "nonimmediate_operand" "")
   	(mem:HI (plus:HI (reg:HI 12)
		         (match_operand 1 "const_int_operand" ""))))]
   ;
   ; We only match here if IP and DP both go dead because emulating
   ; offsets in conjunction with IP doesn't win unless IP goes
   ; dead too.
   ;
   "((ip2k_reorg_in_progress || ip2k_reorg_completed)
    && peep2_regno_dead_p (2, REG_DP)
    && peep2_regno_dead_p (2, REG_IP)
    && (INTVAL (operands[1]) < 0x100))"
  [(set (match_dup 0)
	(mem:HI (plus:HI (reg:HI 4)
			 (match_dup 1))))]
  "")

(define_peephole2
  [(set (reg:HI 12)
  	(reg:HI 4))
   (set (reg:HI 4)
   	(mem:HI (plus:HI (reg:HI 12)
		         (match_operand 0 "const_int_operand" ""))))]
  "((ip2k_reorg_in_progress || ip2k_reorg_completed)
    && peep2_regno_dead_p (2, REG_DP)
    && (INTVAL (operands[0]) < 0x100))"
  [(set (reg:HI 4)
	(mem:HI (plus:HI (reg:HI 4)
			 (match_dup 0))))]
  "")

(define_peephole2
  [(set (match_operand:HI 0 "register_operand" "")
   	(mem:HI (reg:HI 4)))
   (set (match_operand:HI 2 "nonimmediate_operand" "")
        (match_dup 0))]
  "((ip2k_reorg_in_progress || ip2k_reorg_completed)
    && peep2_reg_dead_p (2, operands[0]))"
  [(set (match_dup 2)
	(mem:HI (reg:HI 4)))]
  "")

(define_peephole2
  [(set (match_operand:HI 0 "register_operand" "")
   	(mem:HI (plus:HI (reg:HI 4)
		         (match_operand 1 "const_int_operand" ""))))
   (set (match_operand:HI 2 "nonimmediate_operand" "")
        (match_dup 0))]
  "((ip2k_reorg_in_progress || ip2k_reorg_completed)
    && peep2_reg_dead_p (2, operands[0])
    && (INTVAL (operands[1]) < 0x100))"
  [(set (match_dup 2)
	(mem:HI (plus:HI (reg:HI 4)
			 (match_dup 1))))]
  "")

(define_peephole2
  [(set (match_operand:HI 0 "ip2k_nonsp_reg_operand" "")
        (match_operand:HI 1 "ip2k_short_operand" ""))
   (set (reg:HI 12)
        (reg:HI 4))
   (set (mem:HI (reg:HI 12))
        (match_dup 0))]
  "(peep2_reg_dead_p (3, operands[0])
    && ip2k_xexp_not_uses_reg_p (operands[0], REG_DP, 2)
    && peep2_regno_dead_p (3, REG_DP))"
  [(set (mem:HI (reg:HI 4))
        (match_dup 1))]
  "")

;; We sometimes want to copy a value twice, usually when we copy a value into
;; both a structure slot and into a temporary register.  We can win here
;; because gcc doesn't know about ways of reusing w while we're copying.
;;
(define_insn "*movhi_twice"
  [(set (match_operand:HI 0 "ip2k_gen_operand" "=&uS,uS")
        (match_operand:HI 1 "ip2k_gen_operand"   "uS,uS"))
   (set (match_operand:HI 2 "ip2k_gen_operand" "=&uS,uS")
        (match_dup 1))]
  "ip2k_reorg_split_simode"
  "*{
    switch (which_alternative)
      {
      case 0:
        return AS2 (mov, w, %L1) CR_TAB
	       AS2 (mov, %L0, w) CR_TAB
	       AS2 (mov, %L2, w) CR_TAB
	       AS2 (mov, w, %H1) CR_TAB
	       AS2 (mov, %H0, w) CR_TAB
	       AS2 (mov, %H2, w);

      case 1:
        return AS2 (mov, w, %L1) CR_TAB
	       AS1 (push, %H1%<) CR_TAB
	       AS1 (push, %H1%<) CR_TAB
	       AS1 (pop, %H0%>) CR_TAB
	       AS2 (mov, %L0, w) CR_TAB
	       AS1 (pop, %H2%>) CR_TAB
	       AS2 (mov, %L2, w);
      default:
        abort ();
      }
  }")

;; We have to be *very* careful with this one to use predicates that do not
;; allow this to match if there are any register dependencies between the
;; operands.
;; Don't try to match until we've removed redundant reloads.  Often this
;; simplification will remove the need to do two moves!
;;
(define_peephole2
  [(set (match_operand:HI 0 "ip2k_gen_operand" "")
        (match_operand:HI 1 "ip2k_gen_operand" ""))
   (set (match_operand:HI 2 "ip2k_gen_operand" "")
        (match_dup 0))]
  "(ip2k_reorg_split_simode)"
  [(parallel [(set (match_dup 0)
  		   (match_dup 1))
	      (set (match_dup 2)
	      	   (match_dup 1))])]
  "")

;; We have to be *very* careful with this one to use predicates that do not
;; allow this to match if there are any register dependencies between the
;; operands.
;; Don't try to match until we've removed redundant reloads.  Often this
;; simplification will remove the need to do two moves!
;;
(define_peephole2
  [(set (match_operand:HI 0 "ip2k_gen_operand" "")
        (match_operand:HI 1 "ip2k_gen_operand" ""))
   (set (match_operand:HI 2 "ip2k_gen_operand" "")
        (match_dup 1))]
  "(ip2k_reorg_split_simode
    && (!REG_P (operands[0])
        || ip2k_xexp_not_uses_reg_p (operands[1], REGNO (operands[0]), 2)))"
  [(parallel [(set (match_dup 0)
  		   (match_dup 1))
	      (set (match_dup 2)
	      	   (match_dup 1))])]
  "")

;;
;; Move 32-bit integers.
;;

(define_expand "movsi"
  [(set (match_operand:SI 0 "" "")
	(match_operand:SI 1 "" ""))]
  ""
  "")

(define_insn_and_split "*movsi"
  [(set (match_operand:SI 0 "ip2k_split_dest_operand" "=<, ro,  S")
	(match_operand:SI 1 "general_operand"	    "roSi,rSi,roi"))]
  ""
  "#"
  "ip2k_reorg_split_simode"
  [(set (match_dup 2) (match_dup 3))
   (set (match_dup 4) (match_dup 5))]
  "{
     ip2k_split_words (HImode, SImode, operands);  /* Split into 2=3,4=5 */
  }")

;; We sometimes want to copy a value twice, usually when we copy a value into
;; both a structure slot and into a temporary register.  We can win here
;; because gcc doesn't know about ways of reusing w while we're copying.
;;
(define_insn "*movsi_twice"
  [(set (match_operand:SI 0 "ip2k_gen_operand" "=&uS,uS")
        (match_operand:SI 1 "ip2k_gen_operand"   "uS,uS"))
   (set (match_operand:SI 2 "ip2k_gen_operand" "=&uS,uS")
        (match_dup 1))]
  "ip2k_reorg_split_dimode"
  "*{
    switch (which_alternative)
      {
      case 0:
        return AS2 (mov, w, %A1) CR_TAB
	       AS2 (mov, %A0, w) CR_TAB
	       AS2 (mov, %A2, w) CR_TAB
	       AS2 (mov, w, %B1) CR_TAB
	       AS2 (mov, %B0, w) CR_TAB
	       AS2 (mov, %B2, w) CR_TAB
	       AS2 (mov, w, %C1) CR_TAB
	       AS2 (mov, %C0, w) CR_TAB
	       AS2 (mov, %C2, w) CR_TAB
	       AS2 (mov, w, %D1) CR_TAB
	       AS2 (mov, %D0, w) CR_TAB
	       AS2 (mov, %D2, w);

      case 1:
        return AS2 (mov, w, %D1) CR_TAB
               AS1 (push, %C1%<) CR_TAB
	       AS1 (push, %B1%<) CR_TAB
	       AS1 (push, %A1%<) CR_TAB
	       AS1 (push, %C1%<) CR_TAB
	       AS1 (push, %B1%<) CR_TAB
	       AS1 (push, %A1%<) CR_TAB
	       AS1 (pop, %A0%>) CR_TAB
	       AS1 (pop, %B0%>) CR_TAB
	       AS1 (pop, %C0%>) CR_TAB
	       AS2 (mov, %D0, w) CR_TAB
	       AS1 (pop, %A2%>) CR_TAB
	       AS1 (pop, %B2%>) CR_TAB
	       AS1 (pop, %C2%>) CR_TAB
	       AS2 (mov, %D2, w);
      default:
        abort ();
     }
  }")

;; We have to be *very* careful with this one to use predicates that do not 
;; allow this to match if there are any register dependencies between the
;; operands.
;; Don't try to match until we've removed redundant reloads.  Often this
;; simplification will remove the need to do two moves!
;;
(define_peephole2
  [(set (match_operand:SI 0 "ip2k_gen_operand" "")
        (match_operand:SI 1 "ip2k_gen_operand" ""))
   (set (match_operand:SI 2 "ip2k_gen_operand" "")
        (match_dup 0))]
  "(ip2k_reorg_split_dimode
    && (!REG_P (operands[0])
        || (ip2k_xexp_not_uses_reg_p (operands[1], REGNO (operands[0]), 4)
	    && ip2k_xexp_not_uses_reg_p (operands[2], REGNO (operands[0]), 4)))
    && (!REG_P (operands[1])
        || (ip2k_xexp_not_uses_reg_p (operands[0], REGNO (operands[1]), 4)
	    && ip2k_xexp_not_uses_reg_p (operands[2], REGNO (operands[1]), 4)))
    && (!REG_P (operands[2])
        || (ip2k_xexp_not_uses_reg_p (operands[0], REGNO (operands[2]), 4)
	    && ip2k_xexp_not_uses_reg_p (operands[1],
                                         REGNO (operands[2]), 4))))"
  [(parallel [(set (match_dup 0)
  		   (match_dup 1))
	      (set (match_dup 2)
	      	   (match_dup 1))])]
  "")

;; We have to be *very* careful with this one to use predicates that do not
;; allow this to match if there are any register dependencies between the
;; operands.
;; Don't try to match until we've removed redundant reloads.  Often this
;; simplification will remove the need to do two moves!
;;
(define_peephole2
  [(set (match_operand:SI 0 "ip2k_gen_operand" "")
        (match_operand:SI 1 "ip2k_gen_operand" ""))
   (set (match_operand:SI 2 "ip2k_gen_operand" "")
        (match_dup 1))]
  "(ip2k_reorg_split_dimode
    && (!REG_P (operands[0])
        || (ip2k_xexp_not_uses_reg_p (operands[1], REGNO (operands[0]), 4)
	    && ip2k_xexp_not_uses_reg_p (operands[2], REGNO (operands[0]), 4)))
    && (!REG_P (operands[1])
        || (ip2k_xexp_not_uses_reg_p (operands[0], REGNO (operands[1]), 4)
	    && ip2k_xexp_not_uses_reg_p (operands[2], REGNO (operands[1]), 4)))
    && (!REG_P (operands[2])
        || (ip2k_xexp_not_uses_reg_p (operands[0], REGNO (operands[2]), 4)
	    && ip2k_xexp_not_uses_reg_p (operands[1],
                                         REGNO (operands[2]), 4))))"
  [(parallel [(set (match_dup 0)
  		   (match_dup 1))
	      (set (match_dup 2)
	      	   (match_dup 1))])]
  "")

;;
;; Move 64-bit integers.
;;

(define_expand "movdi"
  [(set (match_operand:DI 0 "" "")
	(match_operand:DI 1 "" ""))]
  ""
  "")

(define_insn_and_split "*movdi"
  [(set (match_operand:DI 0 "ip2k_split_dest_operand" "=<, ro,  S")
	(match_operand:DI 1 "general_operand"	    "roSi,rSi,roi"))]
  ""
  "#"
  "ip2k_reorg_split_dimode"
  [(set (match_dup 2) (match_dup 3))
   (set (match_dup 4) (match_dup 5))]
  "{
     ip2k_split_words (SImode, DImode, operands);  /* Split into 2=3,4=5 */
  }")

;;
;; Move 32-bit floating point values.
;;

(define_expand "movsf"
  [(set (match_operand:SF 0 "" "")
	(match_operand:SF 1 "" ""))]
  ""
  "if (GET_CODE (operands[0]) == MEM && GET_CODE (operands[1]) == MEM)
     operands[1] = copy_to_mode_reg (SFmode, operands[1]);
  ")

(define_insn_and_split "*movsf"
  [(set (match_operand:SF 0 "ip2k_split_dest_operand" "=r<, o")
	(match_operand:SF 1 "general_operand"	      "roi,ri"))]
  "(ip2k_short_operand (operands[0], SFmode)
    && ip2k_short_operand (operands[1], SFmode))
   || ! (memory_operand (operands[0], SFmode)
         && memory_operand (operands[1], SFmode))"
  "#"
  "(reload_completed || reload_in_progress)"
  [(set (match_dup 2) (match_dup 3))
   (set (match_dup 4) (match_dup 5))
   (set (match_dup 6) (match_dup 7))
   (set (match_dup 8) (match_dup 9))]
  "{
     /* Split into 2=3,4=5 */
     ip2k_split_words (HImode, SImode, operands);
     /* Split 4=5 into 6=7,8=9 */		
     ip2k_split_words (QImode, HImode, &operands[4]); 
     operands[0] = operands[2];
     operands[1] = operands[3];
     ip2k_split_words (QImode, HImode, operands);
  }")

;;
;; Move 64-bit floating point values.
;;

;;
;; Block move operations.
;;

;; Copy a block of bytes (memcpy()).  We expand the definition to convert
;; our memory operand into a register pointer operand instead.
;;
(define_expand "movmemhi"
  [(use (match_operand:BLK 0 "memory_operand" ""))
   (use (match_operand:BLK 1 "memory_operand" ""))
   (use (match_operand:HI 2 "general_operand" ""))
   (use (match_operand 3 "const_int_operand" ""))]
  ""
  "{
    rtx addr0, addr1, count;

    addr0 = copy_to_mode_reg (Pmode, XEXP (operands[0], 0));
    addr1 = copy_to_mode_reg (Pmode, XEXP (operands[1], 0));

    if (GET_CODE (operands[2]) == CONST_INT)
      count = gen_int_mode (INTVAL (operands[2]) & 0xffff, HImode);
    else
      count = operands[2];

    emit_insn (gen_movmemhi_expanded (addr0, count, addr1));
    DONE;
  }")

;; Block copy instruction.  We handle this by calling one of two functions in
;; libgcc.  The first of these is a special case (faster) routine that handles
;; constant block sizes under 256 bytes.  This one is particularly common
;; because we use it when copying data structures.  The second routine handles
;; the general case where we have either a variable block size or one that is
;; greater than 255 bytes.
;;
(define_insn "movmemhi_expanded"
  [(set
    (mem:BLK
     (match_operand:HI 0 "nonimmediate_operand" "rS,ro,rS, rS, ro, rS"))
    (mem:BLK
     (match_operand:HI 2 "nonimmediate_operand" "ro,rS,rS, ro, rS, rS")))
   (use
    (match_operand:HI 1 "general_operand"	 "P, P, P,rSi,rSi,roi"))]
  ""
  "@
   push\\t%L1%<\;push\\t%L2%<\;push\\t%H2%<\;push\\t%L0%<\;push\\t%H0%>%>%>%>\;page\\t__movmemhi_countqi\;call\\t__movmemhi_countqi
   push\\t%L1%<\;push\\t%L2%<\;push\\t%H2%<\;push\\t%L0%<\;push\\t%H0%>%>%>%>\;page\\t__movmemhi_countqi\;call\\t__movmemhi_countqi
   push\\t%L1%<\;push\\t%L2%<\;push\\t%H2%<\;push\\t%L0%<\;push\\t%H0%>%>%>%>\;page\\t__movmemhi_countqi\;call\\t__movmemhi_countqi
   push\\t%L1%<\;push\\t%H1%<\;push\\t%L2%<\;push\\t%H2%<\;push\\t%L0%<\;push\\t%H0%>%>%>%>%>\;page\\t__movmemhi_counthi\;call\\t__movmemhi_counthi
   push\\t%L1%<\;push\\t%H1%<\;push\\t%L2%<\;push\\t%H2%<\;push\\t%L0%<\;push\\t%H0%>%>%>%>%>\;page\\t__movmemhi_counthi\;call\\t__movmemhi_counthi
   push\\t%L1%<\;push\\t%H1%<\;push\\t%L2%<\;push\\t%H2%<\;push\\t%L0%<\;push\\t%H0%>%>%>%>%>\;page\\t__movmemhi_counthi\;call\\t__movmemhi_counthi")


;; Bit insert
;;
(define_expand "insv"
  [(set (zero_extract:QI (match_operand:QI 0 "nonimmediate_operand" "")
			 (match_operand 1 "immediate_operand" "")  ;size
			 (match_operand 2 "immediate_operand" "")) ;pos
	(match_operand:QI 3 "general_operand" ""))]
  ""
  "{
    if (! CONST_OK_FOR_LETTER_P (INTVAL (operands[1]), 'J')
	|| ! CONST_OK_FOR_LETTER_P (INTVAL (operands[2]), 'J'))
      FAIL;
  }")

(define_insn "*insv"
  [(set (zero_extract:QI
	 (match_operand:QI
	  0 "nonimmediate_operand" "+roR,roR,roR,roR,&roR,&roR,&r")
	 (match_operand
	  1 "immediate_operand"       "N,  N,  J,  J,   N,   J, J") ;sz
	 (match_operand
	  2 "immediate_operand"       "J,  J,  J,  J,   J,   J, J"));pos
	(match_operand:QI
	 3 "general_operand"	     "MN,  O,  M,  O, roR,  rn,oR"))]
  ""
  "*{
    unsigned int pos = INTVAL (operands[2]),
		 siz = INTVAL (operands[1]),
		 mask = (1 << (pos + siz)) - (1 << pos);

    switch (which_alternative)
      {
      case 0:
        return \"setb\\t%0,%b1\";

      case 1:
        return \"clrb\\t%0,%b1\";

      case 2:
        operands[3] = gen_int_mode (mask & 0xff, QImode);
        return AS2 (mov, w, %3) CR_TAB
	       AS2 (or, %0, w);

      case 3:
        operands[3] = gen_int_mode (0xff & ~mask, QImode);
        return AS2 (mov, w, %3) CR_TAB
	       AS2 (and, %0, w);

      case 4:
        return AS2 (clrb, %0,%b2) CR_TAB
	       AS2 (snb, %3, 0) CR_TAB
	       AS2 (setb, %0, %b2);

      case 5:
      case 6:
        {
	  static char buff[256];
	  char *p = buff;

	  /* Clear the destination field */

	  p += sprintf (buff, \"mov\\tw,#$%2.2x\;and\\t%%0,w\;\",
		        0xff & ~mask);

	  if (CONSTANT_P (operands[3]))
	  /* Constant can just be or-ed in.  */
	    {
	      p += sprintf (p, \"mov\\tw,#$%2.2x\;or\\t%%0,w\",
		            (int) (INTVAL (operands[3]) << pos) & mask & 0xff);
	      return buff;
	    }

	  p += sprintf (p, \"mov\\tw,%%3\;\"); /* Value to deposit */

	  /* Shift and mask the value before OR-ing into the destination.  */

          if (pos != 0)
	    p += sprintf (p, \"mulu\\tw,#%d\;\", 1<<pos);

	  p += sprintf (p, \"\;and\\tw,#$%2.2x\;or\\t%%0,w\", mask);
	  return buff;
        }
      default:
        abort ();
      }
  }"
  [(set_attr "skip" "yes,yes,no,no,no,no,no")
   (set_attr "clobberw" "no,no,yes,yes,no,yes,yes")])

;;
;; Add bytes
;;

(define_expand "addqi3"
  [(set (match_operand:QI 0 "nonimmediate_operand" "")
	(plus:QI (match_operand:QI 1 "general_operand" "")
		 (match_operand:QI 2 "general_operand" "")))]
  ""
  "")

(define_insn "*push_addqi3"
  [(set (match_operand:QI 0 "push_operand"                  "=<,<,<")
        (plus:QI (match_operand:QI 1 "nonimmediate_operand" "%g,g,g")
		 (match_operand:QI 2 "general_operand"       "N,M,g")))]
  ""
  "@
   push\\t%1\;inc\\t1(SP)
   push\\t%1\;dec\\t1(SP)
   mov\\tw,%2\;add\\tw,%1\;push\\twreg"
  [(set_attr "clobberw" "no,no,yes")])

(define_insn "*addqi3_w"
  [(set
    (reg:QI 10)
    (plus:QI
     (match_operand:QI 0 "nonimmediate_operand" "%rS, g,rS, g, rS,  g,rS")
     (match_operand:QI 1 "general_operand"        "N, N, M, M,rSi,rSi, g")))]
  "(ip2k_reorg_split_qimode)"
  "@
   inc\\tw,%0
   inc\\tw,%0
   dec\\tw,%0
   dec\\tw,%0
   mov\\tw,%1\;add\\tw,%0
   mov\\tw,%1\;add\\tw,%0
   mov\\tw,%1\;add\\tw,%0"
  [(set_attr "skip" "no,no,no,no,no,no,no")])

(define_insn_and_split "*addqi3"
  [(set (match_operand:QI 0 "nonimmediate_operand"          "=k,k,z,z,djyoR,djyoR,djyoR,djyS, g,rS, g,rS,  g, rS,rS")
	(plus:QI (match_operand:QI 1 "nonimmediate_operand" "%0,0,0,0,    0,    0,    0,   0,rS, g,rS, g, rS,  g,rS")
		 (match_operand:QI 2 "general_operand"       "N,g,N,g,    N,    M,  rSi,   g, N, N, M, M,rSi,rSi, g")))]
  ""
  "@
   incsnz\\t%0\;dec\\tiph
   mov\\tw,%2\;add\\t%0,w
   incsnz\\t%0\;dec\\tdph
   mov\\tw,%2\;add\\t%0,w
   inc\\t%0
   dec\\t%0
   mov\\tw,%2\;add\\t%0,w
   mov\\tw,%2\;add\\t%0,w
   #
   #
   #
   #
   #
   #
   #"
  "(ip2k_reorg_split_qimode
    && ! rtx_equal_p (operands[0], operands[1]))"
  [(set (reg:QI 10)
  	(plus:QI (match_dup 1)
		 (match_dup 2)))
   (set (match_dup 0)
   	(reg:QI 10))]
  ""
  [(set_attr "skip" "no,no,no,no,yes,yes,no,no,no,no,no,no,no,no,no")
   (set_attr
    "clobberw" "no,yes,no,yes,no,no,yes,yes,yes,yes,yes,yes,yes,yes,yes")])

;;
;; Add 16-bit integers.
;;

(define_expand "addhi3"
  [(set (match_operand:HI 0 "nonimmediate_operand" "")
	(plus:HI (match_operand:HI 1 "general_operand" "")
		 (match_operand:HI 2 "general_operand" "")))]
  ""
  "if (rtx_equal_p (operands[1], operands[2]))
     {
       /* It is not impossible to wind up with two constants here.
          If we simply emit the ashl, we'll generate unrecognizable
	  instructions.  */
       if (! nonimmediate_operand (operands[1], HImode))
         operands[1] = copy_to_mode_reg (HImode, operands[1]);
       emit_insn (gen_ashlhi3 (operands[0], operands[1], const1_rtx));
       DONE;
     }
  ")

(define_insn "*push_addhi3" ;		           0 1  2  3   4   5 6 7
  [(set
    (match_operand:HI 0 "push_operand"           "=<,<, <, <,  <,  <,<,<")
    (plus:HI
     (match_operand:HI 1 "nonimmediate_operand" "%uo,q,uo,bf, uo, uS,q,q")
     (match_operand:HI 2 "general_operand"        "N,N, M, P,uSi,uoi,u,n")))]
  ""
  "*{
    switch (which_alternative) {
    case 0:
      return AS1 (push, %L1%<) CR_TAB
      	     AS1 (push, %H1%>) CR_TAB
	     AS1 (incsnz, 2(SP)) CR_TAB
	     AS1 (inc, 1(SP));

    case 1:
      return AS2 (mov, w, %H1) CR_TAB
      	     AS1 (push, %L1) CR_TAB
	     AS1 (push, wreg) CR_TAB
	     AS1 (incsnz, 2(SP)) CR_TAB
	     AS1 (inc, 1(SP));

    case 2:
      return AS1 (push, %L1%<) CR_TAB
      	     AS1 (push, %H1%>) CR_TAB
	     AS2 (mov, w, #-1) CR_TAB
	     AS2 (add, 2(SP), w) CR_TAB
	     AS2 (addc, 1(SP), w);

    case 3:
      OUT_AS2 (mov, w, %L2);
      OUT_AS2 (add, %L1, w);
      OUT_AS1 (push, %L1);
      OUT_AS1 (push, %H1);
      if (!find_regno_note (insn, REG_DEAD, REGNO (operands[1])))
        OUT_AS2 (sub, %L1, w);
      return \"\";

    case 4:
    case 5:
      return AS2 (mov, w, %L2) CR_TAB
      	     AS2 (add, w, %L1) CR_TAB
	     AS1 (push, wreg%<) CR_TAB
	     AS2 (mov, w, %H2) CR_TAB
	     AS2 (addc, w, %H1) CR_TAB
	     AS1 (push, wreg%>);

    case 6:
      return AS2 (mov, w, %H1) CR_TAB
      	     AS1 (push, %L1) CR_TAB
	     AS1 (push, wreg) CR_TAB
	     AS2 (mov, w, %L2) CR_TAB
	     AS2 (add, 2(SP), w) CR_TAB
	     AS2 (mov, w, %H2) CR_TAB
	     AS2 (addc, 1(SP), w);

    case 7:
      {
        operands[3] = GEN_INT (INTVAL (operands[2]) + 2);
	return AS1 (push, %L3) CR_TAB
	       AS1 (push, %H3) CR_TAB
	       AS2 (mov, w, %L1) CR_TAB
	       AS2 (add, 2(SP), w) CR_TAB
	       AS2 (mov, w, %H1) CR_TAB
	       AS2 (addc, 1(SP), w);
      }
    default:
      abort ();
    }
  }"
  [(set_attr "clobberw" "no,yes,yes,yes,yes,yes,yes,yes")])

(define_insn "*push_addhi3_zero_ext" ;		     0    1    2   3
  [(set (match_operand:HI 0 "push_operand"         "=<,   <,   <,  <")
        (plus:HI
	 (zero_extend:HI
	  (match_operand:QI 1 "general_operand" "%roRi,roRi,roRi,rSi"))
	 (match_operand:HI 2 "general_operand"      "N,   P, rSi,roi")))]
  ""
  "@
   inc\\tw,%L2\;push\\twreg\;push\\t#0\;rl\\t1(SP)
   mov\\tw,%L2\;add\\tw,%1\;push\\twreg\;push\\t#0\;rl\\t1(SP)
   mov\\tw,%L2\;add\\tw,%1\;push\\twreg%<\;mov\\tw,%H2\;addc\\tw,$ff\;push\\twreg%>
   mov\\tw,%L2\;add\\tw,%1\;push\\twreg%<\;mov\\tw,%H2\;addc\\tw,$ff\;push\\twreg%>")

(define_insn "*addhi3_imm_zero_ext_w"
  [(set
    (match_operand:HI 0 "nonimmediate_operand"  "=rS,o,a,b,a,a,rS,o,rS,o")
    (plus:HI (zero_extend:HI (reg:QI 10))
	     (match_operand 1 "immediate_operand" "O,O,M,i,P,I, P,P, i,i")))]
  ""
  "@
   mov\\t%L0,w\;clr\\t%H0
   mov\\t%L0,w\;clr\\t%H0
   mov\\t%L0,w\;clr\\t%H0\;dec\\t%L0
   loadh\\t%x1\;loadl\\t%x1\;add\\t%L0,w
   mov\\t%L0,w\;clr\\t%H0\;mov\\tw,%1\;add\\t%L0,w
   mov\\t%L0,w\;clr\\t%H0\;mov\\tw,#%n1\;sub\\t%L0,w
   add\\tw,%L1\;mov\\t%L0,w\;clr\\t%H0\;rl\\t%H0
   add\\tw,%L1\;mov\\t%L0,w\;clr\\t%H0\;rl\\t%H0
   add\\tw,%L1\;mov\\t%L0,w\;clr\\t%H0\;mov\\tw,%H1\;addc\\t%H0,w
   add\\tw,%L1\;mov\\t%L0,w\;clr\\t%H0\;mov\\tw,%H1\;addc\\t%H0,w")

(define_insn_and_split "*addhi3_imm_zero_ext"
  [(set
    (match_operand:HI
     0 "nonimmediate_operand" "=rS, o, rS, o,  a,  b,  a,  a, rS, o, rS, o")
    (plus:HI
     (zero_extend:HI
      (match_operand:QI
       1 "general_operand"   "%roR,rS,roR,rS,roR,roR,roR,roR,roR,rS,roR,rS"))
     (match_operand
      2 "immediate_operand"    " O, O,  N, N,  M,  i,  P,  I,  P, P,  i, i")))]
  ""
  "@
   #
   #
   clr\\t%H0\;incsnz\\tw,%1\;inc\\t%H0\;mov\\t%L0,w
   clr\\t%H0\;incsnz\\tw,%1\;inc\\t%H0\;mov\\t%L0,w
   #
   #
   #
   #
   #
   #
   #
   #"
  "(ip2k_reorg_split_qimode
    && (GET_CODE (operands[1]) != CONST_INT
        || INTVAL (operands[1]) != 1))"
  [(set (reg:QI 10)
  	(match_dup 1))
   (set (match_dup 0)
   	(plus:HI (zero_extend:HI (reg:QI 10))
		 (match_dup 2)))])

(define_insn "*addhi3_immediate" ;			 0  1 2  3 4 5 6  7   8   9  a  b  c   d   e  f
  [(set (match_operand:HI 0 "nonimmediate_operand"     "=a,do,a,do,a,a,a,do,&uo,&uS,bf,bf,bf,&uS,&uo, u")
        (plus:HI (match_operand:HI 1 "general_operand" "%0, 0,0, 0,0,0,0, 0, rS, ro,uo,uo,uo, ro, rS,uo")
	         (match_operand 2 "immediate_operand"   "N, N,M, M,P,I,i, i,  N,  N, M, P, I,  i,  i, i")))]
  ""
  "@
   inc\\t%L0
   incsnz\\t%L0\;inc\\t%H0
   dec\\t%L0
   mov\\tw,#-1\;add\\t%L0,w\;addc\\t%H0,w
   mov\\tw,%2\;add\\t%L0,w
   mov\\tw,#%n2\;sub\\t%L0,w
   mov\\tw,%L2\;add\\t%L0,w\;mov\\tw,%H2\;add\\t%H0,w
   mov\\tw,%L2\;add\\t%L0,w\;mov\\tw,%H2\;addc\\t%H0,w
   mov\\tw,%H1\;mov\\t%H0,w\;incsnz\\tw,%L1\;inc\\t%H0\;mov\\t%L0,w
   mov\\tw,%H1\;mov\\t%H0,w\;incsnz\\tw,%L1\;inc\\t%H0\;mov\\t%L0,w
   mov\\tw,%H1\;push\\t%L1%<\;pop\\t%L0%>\;mov\\t%H0,w\;dec\\t%L0
   mov\\tw,%H1\;push\\t%L1%<\;pop\\t%L0%>\;mov\\t%H0,w\;mov\\tw,%2\;add\\t%L0,w
   mov\\tw,%H1\;push\\t%L1%<\;pop\\t%L0%>\;mov\\t%H0,w\;mov\\tw,#%n2\;sub\\t%L0,w
   mov\\tw,%L2\;add\\tw,%L1\;mov\\t%L0,w\;mov\\tw,%H2\;addc\\tw,%H1\;mov\\t%H0,w
   mov\\tw,%L2\;add\\tw,%L1\;mov\\t%L0,w\;mov\\tw,%H2\;addc\\tw,%H1\;mov\\t%H0,w
   mov\\tw,%L2\;add\\tw,%L1\;push\\twreg%<\;mov\\tw,%H2\;addc\\tw,%H1\;mov\\t%H0,w\;pop\\t%L0%>"
  [(set_attr "skip" "yes,no,yes,no,no,no,no,no,no,no,no,no,no,no,no,no")
   (set_attr "clobberw" "no,no,no,yes,yes,yes,yes,yes,yes,yes,yes,yes,yes,yes,yes,yes")])

(define_insn "*addhi3_nonimmediate" ;			      0  1  2    3 4   5   6  7
  [(set (match_operand:HI 0 "nonimmediate_operand"	  "=&bf,bf,&dS,&do,d,&rS,&rS, o")
	(plus:HI (match_operand:HI 1 "general_operand"      "%0, 0,  0,  0,0, ro, rS,rS")
		 (match_operand:HI 2 "nonimmediate_operand" "ro,uo, ro, rS,r, rS, ro,rS")))]
  ""
  "@
   mov\\tw,%L2\;add\\t%L0,w\;mov\\tw,%H2\;add\\t%H0,w
   mov\\tw,%L2\;push\\t%H2%<\;add\\t%L0,w\;pop\\twreg%>\;add\\t%H0,w
   mov\\tw,%L2\;add\\t%L0,w\;mov\\tw,%H2\;addc\\t%H0,w
   mov\\tw,%L2\;add\\t%L0,w\;mov\\tw,%H2\;addc\\t%H0,w
   mov\\tw,%L2\;push\\t%H2%<\;add\\t%L0,w\;pop\\twreg%>\;addc\\t%H0,w
   mov\\tw,%L2\;add\\tw,%L1\;mov\\t%L0,w\;mov\\tw,%H2\;addc\\tw,%H1\;mov\\t%H0,w
   mov\\tw,%L2\;add\\tw,%L1\;mov\\t%L0,w\;mov\\tw,%H2\;addc\\tw,%H1\;mov\\t%H0,w
   mov\\tw,%L2\;add\\tw,%L1\;mov\\t%L0,w\;mov\\tw,%H2\;addc\\tw,%H1\;mov\\t%H0,w")

(define_insn "*addhi3_nonimm_zero_extend_w"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=a,ro,&ro,&rS,&rS, u")
	(plus:HI
	 (zero_extend:HI (reg:QI 10))
	 (match_operand:HI 1 "nonimmediate_operand" "0, 0, rS, rS, ro,uo")))]
  ""
  "@
   add\\t%L0,w
   add\\t%L0,w\;clr\\twreg\;addc\\t%H0,w
   add\\tw,%L1\;mov\\t%L0,w\;clr\\twreg\;addc\\tw,%H1\;mov\\t%H0,w
   add\\tw,%L1\;mov\\t%L0,w\;clr\\twreg\;addc\\tw,%H1\;mov\\t%H0,w
   add\\tw,%L1\;mov\\t%L0,w\;clr\\twreg\;addc\\tw,%H1\;mov\\t%H0,w
   add\\tw,%L1\;push\\twreg%<\;clr\\twreg\;addc\\tw,%H1\;mov\\t%H0,w\;pop\\t%L0%>"
  [(set_attr "skip" "yes,no,no,no,no,no")
   (set_attr "clobberw" "no,yes,yes,yes,yes,yes")])

(define_insn_and_split "*addhi3_nonimm_zero_extend"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=a, ro,&ro,&rS,&rS, u")
	(plus:HI
	 (zero_extend:HI
	  (match_operand:QI 1 "general_operand"   "roR,roR, rS,roR, rS,rS"))
	 (match_operand:HI 2 "nonimmediate_operand" "0,  0, rS, rS, ro,uo")))]
  ""
  "#"
  "(ip2k_reorg_split_qimode)"
  [(set (reg:QI 10)
  	(match_dup 1))
   (set (match_dup 0)
   	(plus:HI (zero_extend:HI (reg:QI 10))
		 (match_dup 2)))])
  

;;
;; Add 32-bit integers.
;;

(define_expand "addsi3"
  [(set (match_operand:SI 0 "nonimmediate_operand" "")
	(plus:SI (match_operand:SI 1 "general_operand" "")
		 (match_operand:SI 2 "general_operand" "")))]
  ""
  "")

(define_insn "*push_addsi3"
  [(set (match_operand:SI 0 "push_operand"                  "=<,<,  <")
        (plus:SI (match_operand:SI 1 "nonimmediate_operand" "%g,g,  g")
		 (match_operand:SI 2 "general_operand"       "N,M,rSi")))]
  ""
  "*{
    switch (which_alternative) {
    case 0:
      OUT_AS1 (push, %D1%<);
      OUT_AS1 (push, %C1%<);
      OUT_AS1 (push, %B1%<);
      OUT_AS1 (push, %A1%>%>%>);
      OUT_AS1 (incsnz, 4(SP));
      OUT_AS1 (incsz, 3(SP));
      OUT_AS1 (page, 1f);
      OUT_AS1 (jmp, 1f);
      OUT_AS1 (incsnz, 2(SP));
      OUT_AS1 (inc, 1(SP));
      OUT_AS1 (1:,);
      return \"\";

    case 1:
      OUT_AS1 (push, %D1%<);
      OUT_AS1 (push, %C1%<);
      OUT_AS1 (push, %B1%<);
      OUT_AS1 (push, %A1%>%>%>);
      OUT_AS2 (mov, w, #-1);
      OUT_AS2 (add, 4(SP), w);
      OUT_AS2 (addc, 3(SP), w);
      OUT_AS2 (addc, 2(SP), w);
      OUT_AS2 (addc, 1(SP), w);
      return \"\";

    case 2:
      OUT_AS2 (mov, w, %D2);
      OUT_AS2 (add, w, %D1);
      OUT_AS1 (push, wreg%<);
      OUT_AS2 (mov, w, %C2);
      OUT_AS2 (addc, w, %C1);
      OUT_AS1 (push, wreg%<);
      OUT_AS2 (mov, w, %B2);
      OUT_AS2 (addc, w, %B1);
      OUT_AS1 (push, wreg%<);
      OUT_AS2 (mov, w, %A2);
      OUT_AS2 (addc, w, %A1);
      OUT_AS1 (push, wreg%>%>%>);
      return \"\";

    default:
      abort();
    }
  }"
  [(set_attr "clobberw" "no,yes,yes")])

(define_insn "*addsi3" ;				      0  1   2   3   4   5   6
  [(set
    (match_operand:SI 0 "nonimmediate_operand" "=ro,ro, ro, rS,&ro,&rS,&rS")
    (plus:SI
     (match_operand:SI 1 "nonimmediate_operand" "%0, 0,  0,  0, rS, ro, rS")
     (match_operand:SI 2 "general_operand"	 "N, M,rSi,roi,rSi,rSi,roi")))]
  ""
  "@
   incsnz\\t%D0\;incsz\\t%C0\;page\\t1f\;jmp\\t1f\;incsnz\\t%B0\;inc\\t%A0\;1:
   mov\\tw,#-1\;add\\t%D0,w\;addc\\t%C0,w\;addc\\t%B0,w\;addc\\t%A0,w
   mov\\tw,%D2\;add\\t%D0,w\;mov\\tw,%C2\;addc\\t%C0,w\;mov\\tw,%B2\;addc\\t%B0,w\;mov\\tw,%A2\;addc\\t%A0,w
   mov\\tw,%D2\;add\\t%D0,w\;mov\\tw,%C2\;addc\\t%C0,w\;mov\\tw,%B2\;addc\\t%B0,w\;mov\\tw,%A2\;addc\\t%A0,w
   mov\\tw,%D2\;add\\tw,%D1\;mov\\t%D0,w\;mov\\tw,%C2\;addc\\tw,%C1\;mov\\t%C0,w\;mov\\tw,%B2\;addc\\tw,%B1\;mov\\t%B0,w\;mov\\tw,%A2\;addc\\tw,%A1\;mov\\t%A0,w
   mov\\tw,%D2\;add\\tw,%D1\;mov\\t%D0,w\;mov\\tw,%C2\;addc\\tw,%C1\;mov\\t%C0,w\;mov\\tw,%B2\;addc\\tw,%B1\;mov\\t%B0,w\;mov\\tw,%A2\;addc\\tw,%A1\;mov\\t%A0,w
   mov\\tw,%D2\;add\\tw,%D1\;mov\\t%D0,w\;mov\\tw,%C2\;addc\\tw,%C1\;mov\\t%C0,w\;mov\\tw,%B2\;addc\\tw,%B1\;mov\\t%B0,w\;mov\\tw,%A2\;addc\\tw,%A1\;mov\\t%A0,w"
  [(set_attr "clobberw" "no,yes,yes,yes,yes,yes,yes")])

(define_insn "*push_addsi3_zero_extendqi" ;		      0   1
  [(set (match_operand:SI 0 "push_operand"                  "=<,  <")
        (plus:SI (zero_extend:SI
	           (match_operand:QI 1 "general_operand" "%roRi,rSi"))
	         (match_operand:SI 2 "general_operand"     "rSi,roi")))]
  ""
  "@
   mov\\tw,%D2\;add\\tw,%1\;push\\twreg%<\;mov\\tw,%C2\;addc\\tw,$ff\;push\\twreg%<\;mov\\tw,%B2\;addc\\tw,$ff\;push\\twreg%<\;mov\\tw,%A2\;addc\\tw,$ff\;push\\twreg%>%>%>
   mov\\tw,%D2\;add\\tw,%1\;push\\twreg%<\;mov\\tw,%C2\;addc\\tw,$ff\;push\\twreg%<\;mov\\tw,%B2\;addc\\tw,$ff\;push\\twreg%<\;mov\\tw,%A2\;addc\\tw,$ff\;push\\twreg%>%>%>")

(define_insn "*addsi3_zero_extendqi" ;
  [(set (match_operand:SI 0 "nonimmediate_operand"  "=ro, rS,&ro,&rS,&rS")
        (plus:SI
	 (zero_extend:SI
	  (match_operand:QI 1 "nonimmediate_operand" "rS,roR, rS,roR, rS"))
	 (match_operand:SI 2 "general_operand"        "0,  0,rSi,rSi,roi")))]
  ""
  "@
   mov\\tw,%1\;add\\t%D0,w\;clr\\twreg\;addc\\t%C0,w\;addc\\t%B0,w\;addc\\t%A0,w
   mov\\tw,%1\;add\\t%D0,w\;clr\\twreg\;addc\\t%C0,w\;addc\\t%B0,w\;addc\\t%A0,w
   mov\\tw,%1\;add\\tw,%D2\;mov\\t%D0,w\;mov\\tw,%C2\;addc\\tw,$ff\;mov\\t%C0,w\;mov\\tw,%B2\;addc\\tw,$ff\;mov\\t%B0,w\;mov\\tw,%A2\;addc\\tw,$ff\;mov\\t%A0,w
   mov\\tw,%1\;add\\tw,%D2\;mov\\t%D0,w\;mov\\tw,%C2\;addc\\tw,$ff\;mov\\t%C0,w\;mov\\tw,%B2\;addc\\tw,$ff\;mov\\t%B0,w\;mov\\tw,%A2\;addc\\tw,$ff\;mov\\t%A0,w
   mov\\tw,%1\;add\\tw,%D2\;mov\\t%D0,w\;mov\\tw,%C2\;addc\\tw,$ff\;mov\\t%C0,w\;mov\\tw,%B2\;addc\\tw,$ff\;mov\\t%B0,w\;mov\\tw,%A2\;addc\\tw,$ff\;mov\\t%A0,w")

(define_insn "*push_addsi3_zero_extendhi" ;		          0   1
  [(set (match_operand:SI 0 "push_operand"                      "=<,  <")
        (plus:SI (zero_extend:SI
	           (match_operand:HI 1 "nonimmediate_operand" "%roR,rSR"))
	         (match_operand:SI 2 "general_operand"         "rSi,roi")))]
  ""
  "@
   mov\\tw,%D2\;add\\tw,%L1\;push\\twreg%<\;mov\\tw,%C2\;addc\\tw,%H1\;push\\twreg%<\;mov\\tw,%B2\;addc\\tw,$ff\;push\\twreg%<\;mov\\tw,%A2\;addc\\tw,$ff\;push\\twreg%>%>%>
   mov\\tw,%D2\;add\\tw,%L1\;push\\twreg%<\;mov\\tw,%C2\;addc\\tw,%H1\;push\\twreg%<\;mov\\tw,%B2\;addc\\tw,$ff\;push\\twreg%<\;mov\\tw,%A2\;addc\\tw,$ff\;push\\twreg%>%>%>")

(define_insn "*addsi3_zero_extendhi" ;
  [(set (match_operand:SI 0 "nonimmediate_operand"  "=ro,rS,&ro,&rS,&rS")
        (plus:SI
	 (zero_extend:SI
	  (match_operand:HI 1 "nonimmediate_operand" "rS,ro, rS, ro, rS"))
	 (match_operand:SI 2 "general_operand"        "0, 0,rSi,rSi,roi")))]
  ""
  "@
   mov\\tw,%L1\;add\\t%D0,w\;mov\\tw,%H1\;addc\\t%C0,w\;clr\\twreg\;addc\\t%B0,w\;addc\\t%A0,w
   mov\\tw,%L1\;add\\t%D0,w\;mov\\tw,%H1\;addc\\t%C0,w\;clr\\twreg\;addc\\t%B0,w\;addc\\t%A0,w
   mov\\tw,%L1\;add\\tw,%D2\;mov\\t%D0,w\;mov\\tw,%C2\;addc\\tw,%H1\;mov\\t%C0,w\;mov\\tw,%B2\;addc\\tw,$ff\;mov\\t%B0,w\;mov\\tw,%A2\;addc\\tw,$ff\;mov\\t%A0,w
   mov\\tw,%L1\;add\\tw,%D2\;mov\\t%D0,w\;mov\\tw,%C2\;addc\\tw,%H1\;mov\\t%C0,w\;mov\\tw,%B2\;addc\\tw,$ff\;mov\\t%B0,w\;mov\\tw,%A2\;addc\\tw,$ff\;mov\\t%A0,w
   mov\\tw,%L1\;add\\tw,%D2\;mov\\t%D0,w\;mov\\tw,%C2\;addc\\tw,%H1\;mov\\t%C0,w\;mov\\tw,%B2\;addc\\tw,$ff\;mov\\t%B0,w\;mov\\tw,%A2\;addc\\tw,$ff\;mov\\t%A0,w")

;;
;; Add 64-bit integers.
;;

(define_expand "adddi3"
  [(set (match_operand:DI 0 "nonimmediate_operand" "")
	(plus:DI (match_operand:DI 1 "general_operand" "")
		 (match_operand:DI 2 "general_operand" "")))]
  ""
  "")

(define_insn "*push_adddi3"
  [(set (match_operand:DI 0 "push_operand"                  "=<,<,  <")
        (plus:DI (match_operand:DI 1 "nonimmediate_operand" "%g,g,  g")
		 (match_operand:DI 2 "general_operand"       "N,M,rSi")))]
  ""
  "*{
    switch (which_alternative)
      {
        case 0:
          OUT_AS1 (push, %Z1%<);
          OUT_AS1 (push, %Y1%<);
          OUT_AS1 (push, %X1%<);
          OUT_AS1 (push, %W1%<);
          OUT_AS1 (push, %V1%<);
          OUT_AS1 (push, %U1%<);
          OUT_AS1 (push, %T1%<);
          OUT_AS1 (push, %S1%>%>%>%>%>%>%>);
          OUT_AS1 (incsnz, 8(SP));
          OUT_AS1 (incsz, 7(SP));
          OUT_AS1 (page, 1f);
          OUT_AS1 (jmp, 1f);
          OUT_AS1 (incsnz, 6(SP));
          OUT_AS1 (incsz, 5(SP));
          OUT_AS1 (page, 1f);
          OUT_AS1 (jmp, 1f);
          OUT_AS1 (incsnz, 4(SP));
          OUT_AS1 (incsz, 3(SP));
          OUT_AS1 (page, 1f);
          OUT_AS1 (jmp, 1f);
          OUT_AS1 (incsnz, 2(SP));
          OUT_AS1 (inc, 1(SP));
          OUT_AS1 (1:,);
          return \"\";

        case 1:
          OUT_AS1 (push, %Z1%<);
          OUT_AS1 (push, %Y1%<);
          OUT_AS1 (push, %X1%<);
          OUT_AS1 (push, %W1%<);
          OUT_AS1 (push, %V1%<);
          OUT_AS1 (push, %U1%<);
          OUT_AS1 (push, %T1%<);
          OUT_AS1 (push, %S1%>%>%>%>%>%>%>);
          OUT_AS2 (mov, w, #-1);
          OUT_AS2 (add, 8(SP), w);
          OUT_AS2 (addc, 7(SP), w);
          OUT_AS2 (addc, 6(SP), w);
          OUT_AS2 (addc, 5(SP), w);
          OUT_AS2 (addc, 4(SP), w);
          OUT_AS2 (addc, 3(SP), w);
          OUT_AS2 (addc, 2(SP), w);
          OUT_AS2 (addc, 1(SP), w);
          return \"\";

        case 2:
          OUT_AS2 (mov, w, %Z2);
          OUT_AS2 (add, w, %Z1);
          OUT_AS1 (push, wreg%<);
          OUT_AS2 (mov, w, %Y2);
          OUT_AS2 (addc, w, %Y1);
          OUT_AS1 (push, wreg%<);
          OUT_AS2 (mov, w, %X2);
          OUT_AS2 (addc, w, %X1);
          OUT_AS1 (push, wreg%<);
          OUT_AS2 (mov, w, %W2);
          OUT_AS2 (addc, w, %W1);
          OUT_AS1 (push, wreg%<);
          OUT_AS2 (mov, w, %V2);
          OUT_AS2 (addc, w, %V1);
          OUT_AS1 (push, wreg%<);
          OUT_AS2 (mov, w, %U2);
          OUT_AS2 (addc, w, %U1);
          OUT_AS1 (push, wreg%<);
          OUT_AS2 (mov, w, %T2);
          OUT_AS2 (addc, w, %T1);
          OUT_AS1 (push, wreg%<);
          OUT_AS2 (mov, w, %S2);
          OUT_AS2 (addc, w, %S1);
          OUT_AS1 (push, wreg%>%>%>%>%>%>%>);
          return \"\";

        default:
          abort();
      }
  }"
  [(set_attr "clobberw" "no,yes,yes")])

(define_insn "*adddi3" ;			 0  1   2   3   4   5   6
  [(set
    (match_operand:DI 0 "nonimmediate_operand" "=ro,ro, ro, rS,&ro,&rS,&rS")
    (plus:DI
     (match_operand:DI 1 "nonimmediate_operand" "%0, 0,  0,  0, rS, ro, rS")
     (match_operand:DI 2 "general_operand"	 "N, M,rSi,roi,rSi,rSi,roi")))]
  ""
  "*{
    switch (which_alternative)
      {
        case 0:
          OUT_AS1 (incsnz, %Z0);
          OUT_AS1 (incsz, %Y0);
          OUT_AS1 (page, 1f);
          OUT_AS1 (jmp, 1f);
          OUT_AS1 (incsnz, %X0);
          OUT_AS1 (incsz, %W0);
          OUT_AS1 (page, 1f);
          OUT_AS1 (jmp, 1f);
          OUT_AS1 (incsnz, %V0);
          OUT_AS1 (incsz, %U0);
          OUT_AS1 (page, 1f);
          OUT_AS1 (jmp, 1f);
          OUT_AS1 (incsnz, %T0);
          OUT_AS1 (inc, %S0);
          OUT_AS1 (1:, );
          return \"\";

        case 1:
          OUT_AS2 (mov, w, #-1);
          OUT_AS2 (add, %Z0, w);
          OUT_AS2 (addc, %Y0, w);
          OUT_AS2 (addc, %X0, w);
          OUT_AS2 (addc, %W0, w);
          OUT_AS2 (addc, %V0, w);
          OUT_AS2 (addc, %U0, w);
          OUT_AS2 (addc, %T0, w);
          OUT_AS2 (addc, %S0, w);
          return \"\";

        case 2:
        case 3:
          OUT_AS2 (mov, w, %Z2);
          OUT_AS2 (add, %Z0, w);
          OUT_AS2 (mov, w, %Y2);
          OUT_AS2 (addc, %Y0, w);
          OUT_AS2 (mov, w, %X2);
          OUT_AS2 (addc, %X0, w);
          OUT_AS2 (mov, w, %W2);
          OUT_AS2 (addc, %W0, w);
          OUT_AS2 (mov, w, %V2);
          OUT_AS2 (addc, %V0, w);
          OUT_AS2 (mov, w, %U2);
          OUT_AS2 (addc, %U0, w);
          OUT_AS2 (mov, w, %T2);
          OUT_AS2 (addc, %T0, w);
          OUT_AS2 (mov, w, %S2);
          OUT_AS2 (addc, %S0, w);
          return \"\";

        case 4:
        case 5:
        case 6:
          OUT_AS2 (mov, w, %Z2);
          OUT_AS2 (add, w, %Z1);
          OUT_AS2 (mov, %Z0, w);
          OUT_AS2 (mov, w, %Y2);
          OUT_AS2 (addc, w, %Y1);
          OUT_AS2 (mov, %Y0, w);
          OUT_AS2 (mov, w, %X2);
          OUT_AS2 (addc, w, %X1);
          OUT_AS2 (mov, %X0, w);
          OUT_AS2 (mov, w, %W2);
          OUT_AS2 (addc, w, %W1);
          OUT_AS2 (mov, %W0, w);
          OUT_AS2 (mov, w, %V2);
          OUT_AS2 (addc, w, %V1);
          OUT_AS2 (mov, %V0, w);
          OUT_AS2 (mov, w, %U2);
          OUT_AS2 (addc, w, %U1);
          OUT_AS2 (mov, %U0, w);
          OUT_AS2 (mov, w, %T2);
          OUT_AS2 (addc, w, %T1);
          OUT_AS2 (mov, %T0, w);
          OUT_AS2 (mov, w, %S2);
          OUT_AS2 (addc, w, %S1);
          OUT_AS2 (mov, %S0, w);
          return \"\";

        default:
          abort();
      }
  }"
  [(set_attr "clobberw" "no,yes,yes,yes,yes,yes,yes")])

(define_insn "*adddi3_zero_extendqi" ;		       0   1   2   3   4
  [(set (match_operand:DI 0 "nonimmediate_operand"  "=ro, rS,&ro,&rS,&rS")
        (plus:DI
	 (zero_extend:DI
	  (match_operand:QI 1 "nonimmediate_operand" "rS,roR, rS,roR, rS"))
	 (match_operand:DI 2 "general_operand"        "0,  0,rSi,rSi,roi")))]
  ""
  "*{
    switch (which_alternative)
      {
        case 0:
        case 1:
	  OUT_AS2 (mov, w, %1);
	  OUT_AS2 (add, %Z0, w);
	  OUT_AS1 (clr, wreg);
	  OUT_AS2 (addc, %Y0, w);
	  OUT_AS2 (addc, %X0, w);
	  OUT_AS2 (addc, %W0, w);
	  OUT_AS2 (addc, %V0, w);
	  OUT_AS2 (addc, %U0, w);
	  OUT_AS2 (addc, %T0, w);
	  OUT_AS2 (addc, %S0, w);
          return \"\";

        case 2:
        case 3:
        case 4:
	  OUT_AS2 (mov, w, %1);
	  OUT_AS2 (add, w, %Z2);
	  OUT_AS2 (mov, %Z0, w);
	  OUT_AS2 (mov, w, %Y2);
	  OUT_AS2 (addc, w, $ff);
	  OUT_AS2 (mov, %Y0, w);
	  OUT_AS2 (mov, w, %X2);
	  OUT_AS2 (addc, w, $ff);
	  OUT_AS2 (mov, %X0, w);
	  OUT_AS2 (mov, w, %W2);
	  OUT_AS2 (addc, w, $ff);
	  OUT_AS2 (mov, %W0, w);
	  OUT_AS2 (mov, w, %V2);
	  OUT_AS2 (addc, w, $ff);
	  OUT_AS2 (mov, %V0, w);
	  OUT_AS2 (mov, w, %U2);
	  OUT_AS2 (addc, w, $ff);
	  OUT_AS2 (mov, %U0, w);
	  OUT_AS2 (mov, w, %T2);
	  OUT_AS2 (addc, w, $ff);
	  OUT_AS2 (mov, %T0, w);
	  OUT_AS2 (mov, w, %S2);
	  OUT_AS2 (addc, w, $ff);
	  OUT_AS2 (mov, %S0, w);
          return \"\";

        default:
          abort();
      }
  }")

(define_insn "*adddi3_zero_extendhi" ;		      0  1   2   3   4
  [(set (match_operand:DI 0 "nonimmediate_operand"  "=ro,rS,&ro,&rS,&rS")
        (plus:DI
	 (zero_extend:DI
	  (match_operand:HI 1 "nonimmediate_operand" "rS,ro, rS, ro, rS"))
	 (match_operand:DI 2 "general_operand"        "0, 0,rSi,rSi,roi")))]
  ""
  "*{
    switch (which_alternative)
      {
        case 0:
        case 1:
          OUT_AS2 (mov, w, %L1);
	  OUT_AS2 (add, %Z0, w);
	  OUT_AS2 (mov, w, %H1);
	  OUT_AS2 (addc, %Y0, w);
	  OUT_AS1 (clr, wreg);
	  OUT_AS2 (addc, %X0, w);
	  OUT_AS2 (addc, %W0, w);
	  OUT_AS2 (addc, %V0, w);
	  OUT_AS2 (addc, %U0, w);
	  OUT_AS2 (addc, %T0, w);
	  OUT_AS2 (addc, %S0, w);
	  return \"\";

        case 2:
        case 3:
        case 4:
	  OUT_AS2 (mov, w, %L1);
	  OUT_AS2 (add, w, %Z2);
	  OUT_AS2 (mov, %Z0, w);
	  OUT_AS2 (mov, w, %Y2);
	  OUT_AS2 (addc, w, %H1);
	  OUT_AS2 (mov, %Y0, w);
	  OUT_AS2 (mov, w, %X2);
	  OUT_AS2 (addc, w, $ff);
	  OUT_AS2 (mov, %X0, w);
	  OUT_AS2 (mov, w, %W2);
	  OUT_AS2 (addc, w, $ff);
	  OUT_AS2 (mov, %W0, w);
	  OUT_AS2 (mov, w, %V2);
	  OUT_AS2 (addc, w, $ff);
	  OUT_AS2 (mov, %V0, w);
	  OUT_AS2 (mov, w, %U2);
	  OUT_AS2 (addc, w, $ff);
	  OUT_AS2 (mov, %U0, w);
	  OUT_AS2 (mov, w, %T2);
	  OUT_AS2 (addc, w, $ff);
	  OUT_AS2 (mov, %T0, w);
	  OUT_AS2 (mov, w, %S2);
	  OUT_AS2 (addc, w, $ff);
	  OUT_AS2 (mov, %S0, w);
          return \"\";

        default:
          abort();
      }
  }")

(define_insn "*adddi3_zero_extendsi" ;		       0  1   2   3   4
  [(set (match_operand:DI 0 "nonimmediate_operand"  "=ro,rS,&ro,&rS,&rS")
        (plus:DI
	 (zero_extend:DI
	  (match_operand:SI 1 "nonimmediate_operand" "rS,ro, rS, ro, rS"))
	 (match_operand:DI 2 "general_operand"        "0, 0,rSi,rSi,roi")))]
  ""
  "*{
    switch (which_alternative)
      {
        case 0:
        case 1:
          OUT_AS2 (mov, w, %D1);
	  OUT_AS2 (add, %Z0, w);
	  OUT_AS2 (mov, w, %C1);
	  OUT_AS2 (addc, %Y0, w);
	  OUT_AS2 (mov, w, %B1);
	  OUT_AS2 (addc, %X0, w);
	  OUT_AS2 (mov, w, %A1);
	  OUT_AS2 (addc, %W0, w);
	  OUT_AS1 (clr, wreg);
	  OUT_AS2 (addc, %V0, w);
	  OUT_AS2 (addc, %U0, w);
	  OUT_AS2 (addc, %T0, w);
	  OUT_AS2 (addc, %S0, w);
          return \"\";

        case 2:
        case 3:
        case 4:
	  OUT_AS2 (mov, w, %D1);
	  OUT_AS2 (add, w, %Z2);
	  OUT_AS2 (mov, %Z0, w);
	  OUT_AS2 (mov, w, %Y2);
	  OUT_AS2 (addc, w, %C1);
	  OUT_AS2 (mov, %Y0, w);
	  OUT_AS2 (mov, w, %X2);
	  OUT_AS2 (addc, w, %B1);
	  OUT_AS2 (mov, %X0, w);
	  OUT_AS2 (mov, w, %W2);
	  OUT_AS2 (addc, w, %A1);
	  OUT_AS2 (mov, %W0, w);
	  OUT_AS2 (mov, w, %V2);
	  OUT_AS2 (addc, w, $ff);
	  OUT_AS2 (mov, %V0, w);
	  OUT_AS2 (mov, w, %U2);
	  OUT_AS2 (addc, w, $ff);
	  OUT_AS2 (mov, %U0, w);
	  OUT_AS2 (mov, w, %T2);
	  OUT_AS2 (addc, w, $ff);
	  OUT_AS2 (mov, %T0, w);
	  OUT_AS2 (mov, w, %S2);
	  OUT_AS2 (addc, w, $ff);
	  OUT_AS2 (mov, %S0, w);
          return \"\";

        default:
          abort();
      }
  }")

;;
;; Subtract bytes.
;;

(define_expand "subqi3"
  [(set (match_operand:QI 0 "nonimmediate_operand" "")
        (minus:QI (match_operand:QI 1 "general_operand" "")
		  (match_operand:QI 2 "general_operand" "")))]
  ""
  "if (GET_CODE (operands[2]) == CONST_INT)
     {
       emit_insn (gen_addqi3 (operands[0], operands[1],
                              gen_int_mode (-INTVAL (operands[2]), QImode)));
       DONE;
     }
  ")

(define_insn "*push_subqi3"
  [(set (match_operand:QI 0 "push_operand"               "=<,  <")
  	(minus:QI (match_operand:QI 1 "general_operand"   "g,rSn")
		  (match_operand:QI 2 "general_operand" "rSn,  g")))]
  ""
  "@
   push\\t%1%<\;mov\\tw,%2\;sub\\t1(SP),w%>
   push\\t%1%<\;mov\\tw,%2\;sub\\t1(SP),w%>")

(define_insn "*subqi3_w"
  [(set (reg:QI 10)
	(minus:QI (match_operand:QI 0 "general_operand"  "rS,rSi,  g,rSi")
		  (match_operand:QI 1 "general_operand" "rSi, rS,rSi,  g")))]
  "(ip2k_reorg_split_qimode)"
  "@
   mov\\tw,%1\;sub\\tw,%0
   mov\\tw,%1\;sub\\tw,%0
   mov\\tw,%1\;sub\\tw,%0
   mov\\tw,%1\;sub\\tw,%0")

(define_insn_and_split "*subqi3"
  [(set
    (match_operand:QI
     0 "nonimmediate_operand" "=k,k,z,z,djyoR,djyoR,djyS,djyoR,  g,  g, rS, rS")
    (minus:QI
     (match_operand:QI
      1 "general_operand"     "0,0,0,0,    0,    0,   0,    0, rS,rSi,  g,rSi")
     (match_operand:QI
      2 "general_operand"     "M,g,M,g,    M,    N,   g,  rSi,rSi, rS,rSi,  g")))]
  ""
  "@
   incsnz\\t%0\;dec\\tiph
   mov\\tw,%2\;sub\\t%0,w
   incsnz\\t%0\;dec\\tdph
   mov\\tw,%2\;sub\\t%0,w
   inc\\t%0
   dec\\t%0
   mov\\tw,%2\;sub\\t%0,w
   mov\\tw,%2\;sub\\t%0,w
   #
   #
   #
   #"
  "(ip2k_reorg_split_qimode
    && ! rtx_equal_p (operands[0], operands[1]))"
  [(set (reg:QI 10)
  	(minus:QI (match_dup 1)
		  (match_dup 2)))
   (set (match_dup 0)
   	(reg:QI 10))]
  ""
  [(set_attr "skip" "no,no,no,no,yes,yes,no,no,no,no,no,no")
   (set_attr "clobberw" "no,yes,no,yes,no,no,yes,yes,yes,yes,yes,yes")])

;;
;; Subtract 16-bit integers.
;;

(define_expand "subhi3"
  [(set (match_operand:HI 0 "nonimmediate_operand" "")
        (minus:HI (match_operand:HI 1 "general_operand" "")
		  (match_operand:HI 2 "general_operand" "")))]
  ""
  "if (GET_CODE (operands[2]) == CONST_INT)
     {
       emit_insn (gen_addhi3 (operands[0], operands[1],
                              gen_int_mode (-INTVAL (operands[2]), HImode)));
       DONE;
     }
  ")

(define_insn "*push_subhi3"
  [(set (match_operand:HI 0 "push_operand"               "=<,  <")
  	(minus:HI (match_operand:HI 1 "general_operand" "ron,rSn")
		  (match_operand:HI 2 "general_operand" "rSn,ron")))]
  ""
  "@
   push\\t%L1%<\;mov\\tw,%L2\;sub\\t1(SP),w\;push\\t%H1%<\;mov\\tw,%H2\;subc\\t1(SP),w%>%>
   push\\t%L1%<\;mov\\tw,%L2\;sub\\t1(SP),w\;push\\t%H1%<\;mov\\tw,%H2\;subc\\t1(SP),w%>%>")

(define_insn "*subhi3_imm"
  [(set
    (match_operand:HI 0 "nonimmediate_operand"     "=a,a,a,a,do,&r,&ro,&rS")
    (minus:HI (match_operand:HI 1 "general_operand" "0,0,0,0, 0,ro, rS, ro")
	      (match_operand 2 "immediate_operand"  "N,M,P,i, i, O,  i,  i")))]
  ""
  "@
   dec\\t%L0
   inc\\t%L0
   mov\\tw,%2\;sub\\t%L0,w
   mov\\tw,%L2\;sub\\t%L0,w\;mov\\tw,%H2\;sub\\t%H0,w
   mov\\tw,%L2\;sub\\t%L0,w\;mov\\tw,%H2\;subc\\t%H0,w
   mov\\tw,%L1\;mov\\t%L0,w\;mov\\tw,%H1\;mov\\t%H0,w
   mov\\tw,%L2\;sub\\tw,%L1\;mov\\t%L0,w\;mov\\tw,%H2\;subc\\tw,%H1\;mov\\t%H0,w
   mov\\tw,%L2\;sub\\tw,%L1\;mov\\t%L0,w\;mov\\tw,%H2\;subc\\tw,%H1\;mov\\t%H0,w"
  [(set_attr "skip" "yes,yes,no,no,no,no,no,no")
   (set_attr "clobberw" "no,no,yes,yes,yes,yes,yes,yes")])

(define_insn "*subhi3_ximm_zero_extend"
  [(set (match_operand:HI 0 "nonimmediate_operand"            "=ro, rS")
  	(minus:HI (match_operand:HI 1 "immediate_operand"       "i,  i")
		  (zero_extend:HI
		    (match_operand:QI 2 "nonimmediate_operand" "rS,roR"))))]
  ""
  "@
   mov\\tw,%2\;sub\\tw,%L1\;mov\\t%L0,w\;mov\\tw,%H1\;mov\\t%H0,w\;clr\\twreg\;subc\\t%H0,w
   mov\\tw,%2\;sub\\tw,%L1\;mov\\t%L0,w\;mov\\tw,%H1\;mov\\t%H0,w\;clr\\twreg\;subc\\t%H0,w")

(define_insn "*subhi3_ximm"
  [(set (match_operand:HI 0 "nonimmediate_operand"         "=&uo,&ro,&rS")
  	(minus:HI (match_operand:HI 1 "immediate_operand"     "i,  i,  i")
		  (match_operand:HI 2 "nonimmediate_operand"  "0, rS, ro")))]
  ""
  "@
   mov\\tw,%L2\;sub\\tw,%L1\;mov\\t%L0,w\;push\\t%H2%<\;mov\\tw,%H1\;mov\\t%H0,w\;pop\\twreg%>\;subc\\t%H0,w
   mov\\tw,%L2\;sub\\tw,%L1\;mov\\t%L0,w\;mov\\tw,%H1\;mov\\t%H0,w\;mov\\tw,%H2\;subc\\t%H0,w
   mov\\tw,%L2\;sub\\tw,%L1\;mov\\t%L0,w\;mov\\tw,%H1\;mov\\t%H0,w\;mov\\tw,%H2\;subc\\t%H0,w")

(define_insn "*subhi3_nonimm_zero_extend"
  [(set
    (match_operand:HI 0 "nonimmediate_operand" "=a,ro, rS,&ro,&rS,&rS")
    (minus:HI
     (match_operand:HI 1 "nonimmediate_operand" "0, 0,  0, rS, ro, rS")
     (zero_extend:HI
      (match_operand:QI 2 "general_operand"   "roR,rS,roR, rS, rS,roR"))))]
  ""
  "@
   mov\\tw,%2\;sub\\t%L0,w
   mov\\tw,%2\;sub\\t%L0,w\;clr\\twreg\;subc\\t%H0,w
   mov\\tw,%2\;sub\\t%L0,w\;clr\\twreg\;subc\\t%H0,w
   mov\\tw,%2\;sub\\tw,%L1\;mov\\t%L0,w\;clr\\twreg\;subc\\tw,%H1\;mov\\t%H0,w
   mov\\tw,%2\;sub\\tw,%L1\;mov\\t%L0,w\;clr\\twreg\;subc\\tw,%H1\;mov\\t%H0,w
   mov\\tw,%2\;sub\\tw,%L1\;mov\\t%L0,w\;clr\\twreg\;subc\\tw,%H1\;mov\\t%H0,w")

(define_insn "*subhi3_nonimm" ;		          0  1  2   3   4   5  6
  [(set
    (match_operand:HI 0 "nonimmediate_operand"  "=a,dS, o,&rS,&rS,&rS, o")
    (minus:HI
     (match_operand:HI 1 "nonimmediate_operand"  "0, 0, 0, ro, ro, rS,rS")
     (match_operand:HI 2 "nonimmediate_operand" "ro,ro,rS,  0, rS, ro,rS")))]
  ""
  "@
   mov\\tw,%L2\;sub\\t%L0,w\;mov\\tw,%H2\;sub\\t%H0,w
   mov\\tw,%L2\;sub\\t%L0,w\;mov\\tw,%H2\;subc\\t%H0,w
   mov\\tw,%L2\;sub\\t%L0,w\;mov\\tw,%H2\;subc\\t%H0,w
   mov\\tw,%L2\;sub\\tw,%L1\;mov\\t%L0,w\;mov\\tw,%H2\;subc\\tw,%H1\;mov\\t%H0,w
   mov\\tw,%L2\;sub\\tw,%L1\;mov\\t%L0,w\;mov\\tw,%H2\;subc\\tw,%H1\;mov\\t%H0,w
   mov\\tw,%L2\;sub\\tw,%L1\;mov\\t%L0,w\;mov\\tw,%H2\;subc\\tw,%H1\;mov\\t%H0,w
   mov\\tw,%L2\;sub\\tw,%L1\;mov\\t%L0,w\;mov\\tw,%H2\;subc\\tw,%H1\;mov\\t%H0,w")

;;
;; Subtract 32-bit integers.
;;

(define_insn "subsi3" ;	        0  1   2   3   4   5   6   7   8   9   a   b
  [(set
    (match_operand:SI
     0 "nonimmediate_operand" "=ro,ro, ro, rS,&ro,&rS,&ro,&rS,&rS,&ro,&ro,&rS")
    (minus:SI
     (match_operand:SI
      1 "general_operand"    "0, 0,  0,  0,  i, ro, rS, rS, ro, rS,  i,  i")
     (match_operand:SI
      2 "general_operand"    "M, N,rSi,roi,  0,  0,  0,roi,rSi,rSi, rS, ro")))]
  ""
  "*{
    switch (which_alternative) {
    case 0:
      return AS2 (mov, w, #1) CR_TAB
             AS2 (add, %D0, w) CR_TAB
	     AS1 (clr, wreg) CR_TAB
	     AS2 (addc, %C0, w) CR_TAB
	     AS2 (addc, %B0, w) CR_TAB
	     AS2 (addc, %A0, w);
      
    case 1:
      return AS2 (mov, w, #-1) CR_TAB
             AS2 (sub, %D0, w) CR_TAB
	     AS2 (subc, %C0, w) CR_TAB
	     AS2 (subc, %B0, w) CR_TAB
	     AS2 (subc, %A0, w);
      
    case 2:
    case 3:
      return AS2 (mov, w, %D2) CR_TAB
             AS2 (sub, %D0, w) CR_TAB
	     AS2 (mov, w, %C2) CR_TAB
	     AS2 (subc, %C0, w) CR_TAB
	     AS2 (mov, w, %B2) CR_TAB
	     AS2 (subc, %B0, w) CR_TAB
	     AS2 (mov, w, %A2) CR_TAB
	     AS2 (subc, %A0, w);

    case 4:
      return AS2 (mov, w, %D2) CR_TAB
             AS2 (sub, w, %D1) CR_TAB
	     AS2 (mov, %D0, w) CR_TAB
	     AS1 (push, %C2%<) CR_TAB
	     AS2 (mov, w, %C1) CR_TAB
	     AS2 (mov, %C0, w) CR_TAB
	     AS1 (pop, wreg%>) CR_TAB
	     AS2 (subc, %C0, w) CR_TAB
	     AS1 (push, %B2%<) CR_TAB
	     AS2 (mov, w, %B1) CR_TAB
	     AS2 (mov, %B0, w) CR_TAB
	     AS1 (pop, wreg%>) CR_TAB
	     AS2 (subc, %B0, w) CR_TAB
	     AS1 (push, %A2%<) CR_TAB
	     AS2 (mov, w, %A1) CR_TAB
	     AS2 (mov, %A0, w) CR_TAB
	     AS1 (pop, wreg%>) CR_TAB
	     AS2 (subc, %A0, w);

    case 5:
    case 6:
    case 7:
    case 8:
    case 9:
      return AS2 (mov, w, %D2) CR_TAB
             AS2 (sub, w, %D1) CR_TAB
	     AS2 (mov, %D0, w) CR_TAB
	     AS2 (mov, w, %C2) CR_TAB
	     AS2 (subc, w, %C1) CR_TAB
	     AS2 (mov, %C0, w) CR_TAB
	     AS2 (mov, w, %B2) CR_TAB
	     AS2 (subc, w, %B1) CR_TAB
	     AS2 (mov, %B0, w) CR_TAB
	     AS2 (mov, w, %A2) CR_TAB
	     AS2 (subc, w, %A1) CR_TAB
	     AS2 (mov, %A0, w);

    case 10:
    case 11:
      return AS2 (mov, w, %D2) CR_TAB
             AS2 (sub, w, %D1) CR_TAB
	     AS2 (mov, %D0, w) CR_TAB
	     AS2 (mov, w, %C1) CR_TAB
	     AS2 (mov, %C0, w) CR_TAB
	     AS2 (mov, w, %C2) CR_TAB
	     AS2 (subc, %C0, w) CR_TAB
	     AS2 (mov, w, %B1) CR_TAB
	     AS2 (mov, %B0, w) CR_TAB
	     AS2 (mov, w, %B2) CR_TAB
	     AS2 (subc, %B0, w) CR_TAB
	     AS2 (mov, w, %A1) CR_TAB
	     AS2 (mov, %A0, w) CR_TAB
	     AS2 (mov, w, %A2) CR_TAB
	     AS2 (subc, %A0, w);
    default:
      abort ();
    }
  }")

;;
;; Subtract 64-bit integers.
;;

(define_insn "subdi3" ;	        0  1   2   3  4   5   6   7   8   9   a   b
  [(set
    (match_operand:DI
     0 "nonimmediate_operand" "=ro,ro, ro, rS,ro,&rS,&ro,&rS,&rS,&ro,&ro,&rS")
    (minus:DI
     (match_operand:DI
      1 "general_operand"      "0, 0,  0,  0, i, ro, rS, rS, ro, rS,  i,  i")
     (match_operand:DI
      2 "general_operand"     "M, N,rSi,roi, 0,  0,  0,roi,rSi,rSi, rS, ro")))]
  ""
  "*{
    switch (which_alternative) {
    case 0:
      return AS2 (mov, w, #1) CR_TAB
             AS2 (add, %Z0, w) CR_TAB
	     AS1 (clr, wreg) CR_TAB
	     AS2 (addc, %Y0, w) CR_TAB
	     AS2 (addc, %X0, w) CR_TAB
	     AS2 (addc, %W0, w) CR_TAB
	     AS2 (addc, %V0, w) CR_TAB
	     AS2 (addc, %U0, w) CR_TAB
	     AS2 (addc, %T0, w) CR_TAB
	     AS2 (addc, %S0, w);
      
    case 1:
      return AS2 (mov, w, #-1) CR_TAB
             AS2 (sub, %Z0, w) CR_TAB
	     AS2 (subc, %Y0, w) CR_TAB
	     AS2 (subc, %X0, w) CR_TAB
	     AS2 (subc, %W0, w) CR_TAB
	     AS2 (subc, %V0, w) CR_TAB
	     AS2 (subc, %U0, w) CR_TAB
	     AS2 (subc, %T0, w) CR_TAB
	     AS2 (subc, %S0, w);
      
    case 2:
    case 3:
      return AS2 (mov, w, %Z2) CR_TAB
             AS2 (sub, %Z0, w) CR_TAB
	     AS2 (mov, w, %Y2) CR_TAB
	     AS2 (subc, %Y0, w) CR_TAB
	     AS2 (mov, w, %X2) CR_TAB
	     AS2 (subc, %X0, w) CR_TAB
	     AS2 (mov, w, %W2) CR_TAB
	     AS2 (subc, %W0, w) CR_TAB
	     AS2 (mov, w, %V2) CR_TAB
	     AS2 (subc, %V0, w) CR_TAB
	     AS2 (mov, w, %U2) CR_TAB
	     AS2 (subc, %U0, w) CR_TAB
	     AS2 (mov, w, %T2) CR_TAB
	     AS2 (subc, %T0, w) CR_TAB
	     AS2 (mov, w, %S2) CR_TAB
	     AS2 (subc, %S0, w);

    case 4:
      return AS2 (mov, w, %Z2) CR_TAB
             AS2 (sub, w, %Z1) CR_TAB
	     AS2 (mov, %Z0, w) CR_TAB
	     AS1 (push, %Y2%<) CR_TAB
	     AS2 (mov, w, %Y1) CR_TAB
	     AS2 (mov, %Y0, w) CR_TAB
	     AS1 (pop, wreg%>) CR_TAB
	     AS2 (subc, %Y0, w) CR_TAB
	     AS1 (push, %X2%<) CR_TAB
	     AS2 (mov, w, %X1) CR_TAB
	     AS2 (mov, %X0, w) CR_TAB
	     AS1 (pop, wreg%>) CR_TAB
	     AS2 (subc, %X0, w) CR_TAB
	     AS1 (push, %W2%<) CR_TAB
	     AS2 (mov, w, %W1) CR_TAB
	     AS2 (mov, %W0, w) CR_TAB
	     AS1 (pop, wreg%>) CR_TAB
	     AS2 (subc, %W0, w) CR_TAB
	     AS1 (push, %V2%<) CR_TAB
	     AS2 (mov, w, %V1) CR_TAB
	     AS2 (mov, %V0, w) CR_TAB
	     AS1 (pop, wreg%>) CR_TAB
	     AS2 (subc, %V0, w) CR_TAB
	     AS1 (push, %U2%<) CR_TAB
	     AS2 (mov, w, %U1) CR_TAB
	     AS2 (mov, %U0, w) CR_TAB
	     AS1 (pop, wreg%>) CR_TAB
	     AS2 (subc, %U0, w) CR_TAB
	     AS1 (push, %T2%<) CR_TAB
	     AS2 (mov, w, %T1) CR_TAB
	     AS2 (mov, %T0, w) CR_TAB
	     AS1 (pop, wreg%>) CR_TAB
	     AS2 (subc, %T0, w) CR_TAB
	     AS1 (push, %S2%<) CR_TAB
	     AS2 (mov, w, %S1) CR_TAB
	     AS2 (mov, %S0, w) CR_TAB
	     AS1 (pop, wreg%>) CR_TAB
	     AS2 (subc, %S0, w);

    case 5:
    case 6:
    case 7:
    case 8:
    case 9:
      return AS2 (mov, w, %Z2) CR_TAB
             AS2 (sub, w, %Z1) CR_TAB
	     AS2 (mov, %Z0, w) CR_TAB
	     AS2 (mov, w, %Y2) CR_TAB
	     AS2 (subc, w, %Y1) CR_TAB
	     AS2 (mov, %Y0, w) CR_TAB
	     AS2 (mov, w, %X2) CR_TAB
	     AS2 (subc, w, %X1) CR_TAB
	     AS2 (mov, %X0, w) CR_TAB
	     AS2 (mov, w, %W2) CR_TAB
	     AS2 (subc, w, %W1) CR_TAB
	     AS2 (mov, %W0, w) CR_TAB
	     AS2 (mov, w, %V2) CR_TAB
	     AS2 (subc, w, %V1) CR_TAB
	     AS2 (mov, %V0, w) CR_TAB
	     AS2 (mov, w, %U2) CR_TAB
	     AS2 (subc, w, %U1) CR_TAB
	     AS2 (mov, %U0, w) CR_TAB
	     AS2 (mov, w, %T2) CR_TAB
	     AS2 (subc, w, %T1) CR_TAB
	     AS2 (mov, %T0, w) CR_TAB
	     AS2 (mov, w, %S2) CR_TAB
	     AS2 (subc, w, %S1) CR_TAB
	     AS2 (mov, %S0, w);

    case 10:
    case 11:
      return AS2 (mov, w, %Z2) CR_TAB
             AS2 (sub, w, %Z1) CR_TAB
	     AS2 (mov, %Z0, w) CR_TAB
	     AS2 (mov, w, %Y1) CR_TAB
	     AS2 (mov, %Y0, w) CR_TAB
	     AS2 (mov, w, %Y2) CR_TAB
	     AS2 (subc, %Y0, w) CR_TAB
	     AS2 (mov, w, %X1) CR_TAB
	     AS2 (mov, %X0, w) CR_TAB
	     AS2 (mov, w, %X2) CR_TAB
	     AS2 (subc, %X0, w) CR_TAB
	     AS2 (mov, w, %W1) CR_TAB
	     AS2 (mov, %W0, w) CR_TAB
	     AS2 (mov, w, %W2) CR_TAB
	     AS2 (subc, %W0, w) CR_TAB
	     AS2 (mov, w, %V1) CR_TAB
	     AS2 (mov, %V0, w) CR_TAB
	     AS2 (mov, w, %V2) CR_TAB
	     AS2 (subc, %V0, w) CR_TAB
	     AS2 (mov, w, %U1) CR_TAB
	     AS2 (mov, %U0, w) CR_TAB
	     AS2 (mov, w, %U2) CR_TAB
	     AS2 (subc, %U0, w) CR_TAB
	     AS2 (mov, w, %T1) CR_TAB
	     AS2 (mov, %T0, w) CR_TAB
	     AS2 (mov, w, %T2) CR_TAB
	     AS2 (subc, %T0, w) CR_TAB
	     AS2 (mov, w, %S1) CR_TAB
	     AS2 (mov, %S0, w) CR_TAB
	     AS2 (mov, w, %S2) CR_TAB
	     AS2 (subc, %S0, w);
    default:
      abort ();
    }
  }")

;;
;; Bitwise and instructions.
;;

(define_expand "andqi3"
  [(set (match_operand:QI 0 "nonimmediate_operand" "")
  	(and:QI (match_operand:QI 1 "nonimmediate_operand" "")
		(match_operand:QI 2 "general_operand" "")))]
  ""
  "")

(define_insn "*andqi3_bit"
  [(set (match_operand:QI 0 "nonimmediate_operand"       "=roR")
        (and:QI (match_dup 0)
                (match_operand:QI 1 "const_int_operand"     "n")))]
  "(find_one_clear_bit_p (INTVAL (operands[1]) | 0xffffff00UL) != -1)"
  "*{
    operands[2] = GEN_INT (find_one_clear_bit_p (INTVAL (operands[1])
                                                 | 0xffffff00UL));
    return AS2 (clrb, %0, %b2);
  }"
  [(set_attr "skip" "yes")
   (set_attr "clobberw" "no")])

(define_insn "*andqi3_w_fr"
  [(set (reg:QI 10)
        (and:QI (match_operand:QI 0 "general_operand" "roRn")
		(reg:QI 10)))]
  "(ip2k_reorg_split_qimode)"
  "and\\tw,%0"
  [(set_attr "skip" "yes")])

(define_insn_and_split "*andqi3_fr_w"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=roR,rS,roR, rS,rS")
        (and:QI
	 (match_operand:QI 1 "nonimmediate_operand"  "%0, 0, rS,roR,rS")
	 (reg:QI 10)))]
  "(ip2k_reorg_split_qimode)"
  "@
   and\\t%0,w
   and\\t%0,w
   # 
   #
   #"
  "(ip2k_reorg_split_qimode
    && ! rtx_equal_p (operands[0], operands[1]))"
  [(set (reg:QI 10)
   	(and:QI (match_dup 1)
		(reg:QI 10)))
   (set (match_dup 0)
   	(reg:QI 10))]
  ""
  [(set_attr "skip" "yes,yes,no,no,no")
   (set_attr "clobberw" "no,no,yes,yes,yes")])

(define_insn_and_split "*andqi3" ;		      0    1   2   3    4
  [(set (match_operand:QI 0 "nonimmediate_operand" "=roR,  rS,roR, rS,  rS")
        (and:QI
	 (match_operand:QI 1 "nonimmediate_operand"  "%0,   0, rS,roR,  rS")
	 (match_operand:QI 2 "general_operand"      "rSn,roRn,rSn,rSn,roRn")))]
  ""
  "@
   mov\\tw,%2\;and\\t%0,w
   #
   #
   #
   #"
  "(ip2k_reorg_split_qimode
    && (! rtx_equal_p (operands[0], operands[1])
        || GET_CODE (operands[2]) != CONST_INT
        || find_one_clear_bit_p (INTVAL (operands[2]) | 0xffffff00UL) == -1))"
  [(set (reg:QI 10)
  	(match_dup 2))
   (set (match_dup 0)
   	(and:QI (match_dup 1)
		(reg:QI 10)))])

(define_insn_and_split "andhi3" ;		       0   1   2   3   4
  [(set (match_operand:HI 0 "nonimmediate_operand"  "=uo, uS,&dS,&do,&dS")
        (and:HI
	 (match_operand:HI 1 "nonimmediate_operand"  "%0,  0, ro, rS, rS")
	 (match_operand:HI 2 "general_operand"      "rSn,ron,rSn,rSn,ron")))]
  ""
  "#"
  "(ip2k_reorg_split_himode)"
  [(set (match_dup 3)
	(and:QI (match_dup 4)
		(match_dup 5)))
   (set (match_dup 6)
   	(and:QI (match_dup 7)
		(match_dup 8)))]
  "{
    operands[3] = ip2k_get_high_half (operands[0], QImode);
    operands[4] = ip2k_get_high_half (operands[1], QImode);
    operands[5] = ip2k_get_high_half (operands[2], QImode);
    operands[6] = ip2k_get_low_half (operands[0], QImode);
    operands[7] = ip2k_get_low_half (operands[1], QImode);
    operands[8] = ip2k_get_low_half (operands[2], QImode);
  }")

(define_insn_and_split "andsi3" ;		     0   1   2   3   4
  [(set (match_operand:SI 0 "nonimmediate_operand" "=uo, uS,&dS,&do,&dS")
        (and:SI
	 (match_operand:SI 1 "nonimmediate_operand" "%0,  0, ro, rS, rS")
	 (match_operand:SI 2 "general_operand"      "rSn,ron,rSn,rSn,ron")))]
  ""
  "#"
  "(ip2k_reorg_split_simode)"
  [(set (match_dup 3)
	(and:HI (match_dup 4)
		(match_dup 5)))
   (set (match_dup 6)
   	(and:HI (match_dup 7)
		(match_dup 8)))]
  "{
    operands[3] = ip2k_get_high_half (operands[0], HImode);
    operands[4] = ip2k_get_high_half (operands[1], HImode);
    operands[5] = ip2k_get_high_half (operands[2], HImode);
    operands[6] = ip2k_get_low_half (operands[0], HImode);
    operands[7] = ip2k_get_low_half (operands[1], HImode);
    operands[8] = ip2k_get_low_half (operands[2], HImode);
  }")

(define_insn_and_split "anddi3" ;		      0   1   2   3   4
  [(set (match_operand:DI 0 "nonimmediate_operand"  "=uo, uS,&dS,&do,&dS")
        (and:DI
	 (match_operand:DI 1 "nonimmediate_operand"  "%0,  0, ro, rS, rS")
	 (match_operand:DI 2 "general_operand"      "rSn,ron,rSn,rSn,ron")))]
  ""
  "#"
  "(ip2k_reorg_split_dimode)"
  [(set (match_dup 3)
	(and:SI (match_dup 4)
		(match_dup 5)))
   (set (match_dup 6)
   	(and:SI (match_dup 7)
		(match_dup 8)))]
  "{
    operands[3] = ip2k_get_high_half (operands[0], SImode);
    operands[4] = ip2k_get_high_half (operands[1], SImode);
    operands[5] = ip2k_get_high_half (operands[2], SImode);
    operands[6] = ip2k_get_low_half (operands[0], SImode);
    operands[7] = ip2k_get_low_half (operands[1], SImode);
    operands[8] = ip2k_get_low_half (operands[2], SImode);
  }")

;;
;; Bitwise or instructions.
;;

(define_expand "iorqi3"
  [(set (match_operand:QI 0 "nonimmediate_operand" "")
 	(ior:QI (match_operand:QI 1 "nonimmediate_operand" "")
		(match_operand:QI 2 "general_operand" "")))]
  ""
  "")

(define_insn "*iorqi3_bit"
  [(set (match_operand:QI 0 "nonimmediate_operand"       "=roR")
        (ior:QI (match_dup 0)
                (match_operand:QI 1 "const_int_operand"     "n")))]
  "(find_one_set_bit_p (INTVAL (operands[1]) & 0xff) != -1)"
  "*{
    operands[2] = GEN_INT (find_one_set_bit_p (INTVAL (operands[1]) & 0xff));
    return AS2 (setb, %0, %b2);
  }"
  [(set_attr "skip" "yes")
   (set_attr "clobberw" "no")])

(define_insn "*iorqi3" ;			      0    1   2   3    4
  [(set (match_operand:QI 0 "nonimmediate_operand" "=roR,  rS,roR, rS,  rS")
        (ior:QI
	 (match_operand:QI 1 "nonimmediate_operand"  "%0,   0, rS,roR,  rS")
	 (match_operand:QI 2 "general_operand"      "rSi,roRi,rSi,rSi,roRi")))]
  ""
  "@
   mov\\tw,%2\;or\\t%0,w
   mov\\tw,%2\;or\\t%0,w
   mov\\tw,%2\;or\\tw,%1\;mov\\t%0,w
   mov\\tw,%2\;or\\tw,%1\;mov\\t%0,w
   mov\\tw,%2\;or\\tw,%1\;mov\\t%0,w")

(define_insn_and_split "iorhi3" ;		     0   1   2   3   4
  [(set (match_operand:HI 0 "nonimmediate_operand" "=uo, uS,&dS,&do,&dS")
        (ior:HI
	 (match_operand:HI 1 "nonimmediate_operand" "%0,  0, ro, rS, rS")
	 (match_operand:HI 2 "general_operand"     "rSn,ron,rSn,rSn,ron")))]
  ""
  "#"
  "(ip2k_reorg_split_himode)"
  [(set (match_dup 3)
	(ior:QI (match_dup 4)
		(match_dup 5)))
   (set (match_dup 6)
   	(ior:QI (match_dup 7)
		(match_dup 8)))]
  "{
    operands[3] = ip2k_get_high_half (operands[0], QImode);
    operands[4] = ip2k_get_high_half (operands[1], QImode);
    operands[5] = ip2k_get_high_half (operands[2], QImode);
    operands[6] = ip2k_get_low_half (operands[0], QImode);
    operands[7] = ip2k_get_low_half (operands[1], QImode);
    operands[8] = ip2k_get_low_half (operands[2], QImode);
  }")

(define_insn_and_split "iorsi3" ;		      0   1   2   3   4
  [(set (match_operand:SI 0 "nonimmediate_operand"  "=uo, uS,&dS,&do,&dS")
        (ior:SI
	 (match_operand:SI 1 "nonimmediate_operand"  "%0,  0, ro, rS, rS")
	 (match_operand:SI 2 "general_operand"      "rSn,ron,rSn,rSn,ron")))]
  ""
  "#"
  "(ip2k_reorg_split_simode)"
  [(set (match_dup 3)
	(ior:HI (match_dup 4)
		(match_dup 5)))
   (set (match_dup 6)
   	(ior:HI (match_dup 7)
		(match_dup 8)))]
  "{
    operands[3] = ip2k_get_high_half (operands[0], HImode);
    operands[4] = ip2k_get_high_half (operands[1], HImode);
    operands[5] = ip2k_get_high_half (operands[2], HImode);
    operands[6] = ip2k_get_low_half (operands[0], HImode);
    operands[7] = ip2k_get_low_half (operands[1], HImode);
    operands[8] = ip2k_get_low_half (operands[2], HImode);
  }")

(define_insn_and_split "iordi3" ;		      0   1   2   3   4
  [(set (match_operand:DI 0 "nonimmediate_operand"  "=uo, uS,&dS,&do,&dS")
        (ior:DI
	 (match_operand:DI 1 "nonimmediate_operand"  "%0,  0, ro, rS, rS")
	 (match_operand:DI 2 "general_operand"      "rSn,ron,rSn,rSn,ron")))]
  ""
  "#"
  "(ip2k_reorg_split_dimode)"
  [(set (match_dup 3)
	(ior:SI (match_dup 4)
		(match_dup 5)))
   (set (match_dup 6)
   	(ior:SI (match_dup 7)
		(match_dup 8)))]
  "{
    operands[3] = ip2k_get_high_half (operands[0], SImode);
    operands[4] = ip2k_get_high_half (operands[1], SImode);
    operands[5] = ip2k_get_high_half (operands[2], SImode);
    operands[6] = ip2k_get_low_half (operands[0], SImode);
    operands[7] = ip2k_get_low_half (operands[1], SImode);
    operands[8] = ip2k_get_low_half (operands[2], SImode);
  }")

;;
;; Bitwise xor instructions
;;
;; TODO: xor ops can also use "not w, fr"!
;;

(define_insn "xorqi3"
  [(set
    (match_operand:QI 0 "nonimmediate_operand"  "=roR,roR,  rS,roR, rS,  rS")
    (xor:QI (match_operand:QI 1 "general_operand" "%0,  0,   0, rS,roR,  rS")
	    (match_operand:QI 2 "general_operand" "M,rSi,roRi,rSi,rSi,roRi")))]
  ""
  "@
   not\\t%0
   mov\\tw,%2\;xor\\t%0,w
   mov\\tw,%2\;xor\\t%0,w
   mov\\tw,%1\;xor\\tw,%2\;mov\\t%0,w
   mov\\tw,%1\;xor\\tw,%2\;mov\\t%0,w
   mov\\tw,%1\;xor\\tw,%2\;mov\\t%0,w"
  [(set_attr "clobberw" "no,yes,yes,yes,yes,yes")])

(define_insn_and_split "xorhi3" ;		     0   1   2   3   4
  [(set (match_operand:HI 0 "nonimmediate_operand" "=uo, uS,&dS,&do,&dS")
        (xor:HI
	 (match_operand:HI 1 "nonimmediate_operand" "%0,  0, ro, rS, rS")
	 (match_operand:HI 2 "general_operand"     "rSn,ron,rSn,rSn,ron")))]
  ""
  "#"
  "(ip2k_reorg_split_himode)"
  [(set (match_dup 3)
	(xor:QI (match_dup 4)
		(match_dup 5)))
   (set (match_dup 6)
   	(xor:QI (match_dup 7)
		(match_dup 8)))]
  "{
    operands[3] = ip2k_get_high_half (operands[0], QImode);
    operands[4] = ip2k_get_high_half (operands[1], QImode);
    operands[5] = ip2k_get_high_half (operands[2], QImode);
    operands[6] = ip2k_get_low_half (operands[0], QImode);
    operands[7] = ip2k_get_low_half (operands[1], QImode);
    operands[8] = ip2k_get_low_half (operands[2], QImode);
  }")

(define_insn_and_split "xorsi3" ;		     0   1   2   3   4
  [(set (match_operand:SI 0 "nonimmediate_operand" "=uo, uS,&dS,&do,&dS")
        (xor:SI
	 (match_operand:SI 1 "nonimmediate_operand" "%0,  0, ro, rS, rS")
	 (match_operand:SI 2 "general_operand"     "rSn,ron,rSn,rSn,ron")))]
  ""
  "#"
  "(ip2k_reorg_split_simode)"
  [(set (match_dup 3)
	(xor:HI (match_dup 4)
		(match_dup 5)))
   (set (match_dup 6)
   	(xor:HI (match_dup 7)
		(match_dup 8)))]
  "{
    operands[3] = ip2k_get_high_half (operands[0], HImode);
    operands[4] = ip2k_get_high_half (operands[1], HImode);
    operands[5] = ip2k_get_high_half (operands[2], HImode);
    operands[6] = ip2k_get_low_half (operands[0], HImode);
    operands[7] = ip2k_get_low_half (operands[1], HImode);
    operands[8] = ip2k_get_low_half (operands[2], HImode);
  }")

(define_insn_and_split "xordi3" ;		     0   1   2   3   4
  [(set (match_operand:DI 0 "nonimmediate_operand" "=uo, uS,&dS,&do,&dS")
        (xor:DI
	 (match_operand:DI 1 "nonimmediate_operand" "%0,  0, ro, rS, rS")
	 (match_operand:DI 2 "general_operand"     "rSn,ron,rSn,rSn,ron")))]
  ""
  "#"
  "(ip2k_reorg_split_dimode)"
  [(set (match_dup 3)
	(xor:SI (match_dup 4)
		(match_dup 5)))
   (set (match_dup 6)
   	(xor:SI (match_dup 7)
		(match_dup 8)))]
  "{
    operands[3] = ip2k_get_high_half (operands[0], SImode);
    operands[4] = ip2k_get_high_half (operands[1], SImode);
    operands[5] = ip2k_get_high_half (operands[2], SImode);
    operands[6] = ip2k_get_low_half (operands[0], SImode);
    operands[7] = ip2k_get_low_half (operands[1], SImode);
    operands[8] = ip2k_get_low_half (operands[2], SImode);
  }")

;;
;; Multiply instructions.
;;

(define_insn "umulqi3"
  [(set (match_operand:QI 0 "nonimmediate_operand"          "=ro, rS,  rS")
  	(mult:QI (match_operand:QI 1 "nonimmediate_operand" "%rS,roR,  rS")
		 (match_operand:QI 2 "general_operand"      "rSi,rSi,roRi")))]
  ""
  "mov\\tw,%1\;mulu\\tw,%2\;mov\\t%0,w")

(define_insn "mulqihi3"
  [(set (match_operand:HI 0 "nonimmediate_operand"   "=ro, rS,  rS")
	(mult:HI
	 (sign_extend:HI
	  (match_operand:QI 1 "nonimmediate_operand" "%rS,roR,  rS"))
	 (sign_extend:HI
	  (match_operand:QI 2 "general_operand"      "rSi,rSi,roRi"))))]
  ""
  "@
   mov\\tw,%1\;muls\\tw,%2\;mov\\t%L0,w\;mov\\tw,mulh\;mov\\t%H0,w
   mov\\tw,%1\;muls\\tw,%2\;mov\\t%L0,w\;mov\\tw,mulh\;mov\\t%H0,w
   mov\\tw,%1\;muls\\tw,%2\;mov\\t%L0,w\;mov\\tw,mulh\;mov\\t%H0,w")

(define_insn_and_split "umulqihi3"
  [(set (match_operand:HI 0 "nonimmediate_operand"  	    "=ro, rS,  rS")
	(mult:HI (zero_extend:HI
		  (match_operand:QI 1 "nonimmediate_operand" "%rS,roR,  rS"))
		 (zero_extend:HI
		  (match_operand:QI 2 "general_operand"     "rSi,rSi,roRi"))))]
  ""
  "#"
  "ip2k_reorg_split_qimode"
  [(set (match_dup 3)
        (mult:QI (match_dup 1)
		 (match_dup 2)))
   (set (reg:QI 10)
   	(reg:QI 15))
   (set (match_dup 4)
        (reg:QI 10))]
  "{
    operands[3] = ip2k_get_low_half (operands[0], QImode);
    operands[4] = ip2k_get_high_half (operands[0], QImode);
  }")

(define_insn "*mulhi3_by2"
  [(set (match_operand:HI 0 "nonimmediate_operand"         "=ro,&ro,&rS")
        (mult:HI (match_operand:HI 1 "nonimmediate_operand" "0, rS, ro")
		 (zero_extend:HI (const_int 2))))]
  ""
  "@
   clrb\\tSTATUS,0\;rl\\t%L0\;rl\\t%H0
   clrb\\tSTATUS,0\;rl\\tw,%L1\;mov\\t%L0,w\;rl\\tw,%H1\;mov\\t%H0,w
   clrb\\tSTATUS,0\;rl\\tw,%L1\;mov\\t%L0,w\;rl\\tw,%H1\;mov\\t%H0,w"
  [(set_attr "clobberw" "no,yes,yes")])

(define_insn "*mulhi3_byqi"
  [(set (match_operand:HI
	 0 "nonimmediate_operand"  		      "=ro,&ro,&rS, &rS")
	(mult:HI (match_operand:HI
		  1 "nonimmediate_operand"              "0, rS, ro,  rS")
		 (zero_extend:HI (match_operand:QI
				  2 "general_operand" "rSi,rSi,rSi,roRi"))))]
  ""
  "@
   mov\\tw,%L1\;mulu\\tw,%2\;mov\\t%L0,w\;push\\tmulh%<\;mov\\tw,%H1\;mulu\\tw,%2\;pop\\t%H0%>\;add\\t%H0,w
   mov\\tw,%L1\;mulu\\tw,%2\;mov\\t%L0,w\;mov\\tw,mulh\;mov\\t%H0,w\;mov\\tw,%H1\;mulu\\tw,%2\;add\\t%H0,w
   mov\\tw,%L1\;mulu\\tw,%2\;mov\\t%L0,w\;mov\\tw,mulh\;mov\\t%H0,w\;mov\\tw,%H1\;mulu\\tw,%2\;add\\t%H0,w
   mov\\tw,%L1\;mulu\\tw,%2\;mov\\t%L0,w\;mov\\tw,mulh\;mov\\t%H0,w\;mov\\tw,%H1\;mulu\\tw,%2\;add\\t%H0,w")

(define_insn "smulqi_highpart"
  [(set (match_operand:QI 0 "nonimmediate_operand"      "=roR, rS,  rS")
	(truncate:QI
	  (lshiftrt:HI
	    (mult:HI
	     (sign_extend:HI
	      (match_operand:QI 1 "nonimmediate_operand" "%rS,roR,  rS"))
	     (sign_extend:HI
	      (match_operand:QI 2 "general_operand"      "rSi,rSi,roRi")))
	    (const_int 8))))]
  ""
  "@
   mov\\tw,%1\;muls\\tw,%2\;mov\\tw,mulh\;mov %0,w
   mov\\tw,%1\;muls\\tw,%2\;mov\\tw,mulh\;mov %0,w
   mov\\tw,%1\;muls\\tw,%2\;mov\\tw,mulh\;mov %0,w")

(define_insn "umulqi_highpart"
  [(set (match_operand:QI 0 "nonimmediate_operand"      "=roR, rS,  rS")
	(truncate:QI
	  (lshiftrt:HI
	    (mult:HI
	     (zero_extend:HI
	      (match_operand:QI 1 "nonimmediate_operand" "%rS,roR,  rS"))
	     (zero_extend:HI
	      (match_operand:QI 2 "general_operand"      "rSi,rSi,roRi")))
	    (const_int 8))))]
  ""
  "@
   mov\\tw,%1\;mulu\\tw,%2\;mov\\tw,mulh\;mov %0,w
   mov\\tw,%1\;mulu\\tw,%2\;mov\\tw,mulh\;mov %0,w
   mov\\tw,%1\;mulu\\tw,%2\;mov\\tw,mulh\;mov %0,w")

(define_insn "mulhi3"
  [(set (match_operand:HI 0 "nonimmediate_operand"          "=uo, uS, uS")
  	(mult:HI (match_operand:HI 1 "nonimmediate_operand" "%rS, ro, rS")
		 (match_operand:HI 2 "general_operand"      "rSi,rSi,roi")))]
  ""
  "push\\t%L2%<\;push\\t%H2%<\;push\\t%L1%<\;push\\t%H1%>\;page\\t__mulhi3\;call\\t__mulhi3\;pop\\t%H0%>\;pop\\t%L0%>")

;; If we find that we're multiplying by a constant that's less than 256 we
;; can replace a full "mulhi3" with one of the lighter weight variants
;; that multiplies an HImode value by a QImode one.
;;
(define_split
  [(set (match_operand:HI 0 "nonimmediate_operand"         "=ro,rS")
  	(mult:HI (match_operand:HI 1 "nonimmediate_operand" "rS,ro")
		 (match_operand:HI 2 "const_int_operand"     "P, P")))]
  "(INTVAL (operands[2]) < 0x100)"
  [(set (match_dup 0)
  	(mult:HI (match_dup 1)
		 (zero_extend:HI (match_dup 3))))]
  "operands[3] = gen_int_mode (INTVAL (operands[2]), QImode);")

;;
;; Divide/Modulus functions.
;;

(define_expand "udivmodhi4"
  [(parallel [(set (reg:HI 128)
		   (udiv:HI (match_operand:HI 1 "general_operand" "")
			    (match_operand:HI 2 "general_operand" "")))
	      (set (reg:HI 130)
		   (umod:HI (match_dup 1) (match_dup 2)))
	      (clobber (reg:QI 132))
	      (clobber (reg:QI 133))])
   (set (match_operand:HI 0 "general_operand" "") (reg:HI 128))
   (set (match_operand:HI 3 "general_operand" "") (reg:HI 130))]
  ""
  "")

(define_insn "*udivmodhi4_call"
  [(set (reg:HI 128)
	(udiv:HI (match_operand:HI 0 "general_operand" "uSi,uoi")
		 (match_operand:HI 1 "general_operand" "uoi,uSi")))
   (set (reg:HI 130)
	(umod:HI (match_dup 0) (match_dup 1)))
   (clobber (reg:QI 132))
   (clobber (reg:QI 133))]
  ""
  "push\\t%L1%<\;push\\t%H1%<\;push\\t%L0%<\;push\\t%H0%>%>%>\;page\\t__udivmodhi4\;call\\t__udivmodhi4")

(define_expand "divmodhi4"
  [(parallel [(set (reg:HI 128)
		   (div:HI (match_operand:HI 1 "general_operand" "")
			   (match_operand:HI 2 "general_operand" "")))
	      (set (reg:HI 130)
		   (mod:HI (match_dup 1)
		   	   (match_dup 2)))
	      (clobber (reg:QI 132))
	      (clobber (reg:QI 133))
	      (clobber (reg:QI 134))
	      (clobber (reg:QI 135))])
   (set (match_operand:HI 0 "general_operand" "") (reg:HI 128))
   (set (match_operand:HI 3 "general_operand" "") (reg:HI 130))]
  ""
  "")

(define_insn "*divmodhi4_call"
  [(set (reg:HI 128)
	(div:HI (match_operand:HI 0 "general_operand" "uSi,uoi")
		(match_operand:HI 1 "general_operand" "uoi,uSi")))
   (set (reg:HI 130)
	(mod:HI (match_dup 0) (match_dup 1)))
   (clobber (reg:QI 132))
   (clobber (reg:QI 133))
   (clobber (reg:QI 134))
   (clobber (reg:QI 135))]
  ""
  "push\\t%L1%<\;push\\t%H1%<\;push\\t%L0%<\;push\\t%H0%>%>%>\;page\\t__divmodhi4\;call\\t__divmodhi4")

(define_expand "udivmodsi4"
  [(parallel [(set (reg:SI 128)
		   (udiv:SI (match_operand:SI 1 "general_operand" "")
			    (match_operand:SI 2 "general_operand" "")))
	      (set (reg:SI 132)
		   (umod:SI (match_dup 1)
		   	    (match_dup 2)))
	      (clobber (reg:QI 136))
	      (clobber (reg:QI 137))
	      (clobber (reg:QI 138))
	      (clobber (reg:QI 139))])
   (set (match_operand:SI 0 "general_operand" "") (reg:SI 128))
   (set (match_operand:SI 3 "general_operand" "") (reg:SI 132))]
  ""
  "")

(define_insn "*udivmodsi4_call"
  [(set (reg:SI 128)
	(udiv:SI (match_operand:SI 0 "general_operand" "rSi,roi")
		 (match_operand:SI 1 "general_operand" "roi,rSi")))
   (set (reg:SI 132)
	(umod:SI (match_dup 0)
		 (match_dup 1)))
   (clobber (reg:QI 136))
   (clobber (reg:QI 137))
   (clobber (reg:QI 138))
   (clobber (reg:QI 139))]
  ""
  "push\\t%D1%<\;push\\t%C1%<\;push\\t%B1%<\;push\\t%A1%<\;push\\t%D0%<\;push\\t%C0%<\;push\\t%B0%<\;push\\t%A0%>%>%>%>%>%>%>\;page\\t__udivmodsi4\;call\\t__udivmodsi4")

(define_expand "divmodsi4"
  [(parallel [(set (reg:SI 128)
		   (div:SI (match_operand:SI 1 "general_operand" "")
			   (match_operand:SI 2 "general_operand" "")))
	      (set (reg:SI 132)
		   (mod:SI (match_dup 1)
		   	   (match_dup 2)))
	      (clobber (reg:QI 136))
	      (clobber (reg:QI 137))
	      (clobber (reg:QI 138))
	      (clobber (reg:QI 139))
	      (clobber (reg:QI 140))
	      (clobber (reg:QI 141))])
   (set (match_operand:SI 0 "general_operand" "") (reg:SI 128))
   (set (match_operand:SI 3 "general_operand" "") (reg:SI 132))]
  ""
  "")

(define_insn "*divmodsi4_call"
  [(set (reg:SI 128)
	(div:SI (match_operand:SI 0 "general_operand" "rSn,ron")
		(match_operand:SI 1 "general_operand" "ron,rSn")))
   (set (reg:SI 132)
	(mod:SI (match_dup 0)
		(match_dup 1)))
   (clobber (reg:QI 136))
   (clobber (reg:QI 137))
   (clobber (reg:QI 138))
   (clobber (reg:QI 139))
   (clobber (reg:QI 140))
   (clobber (reg:QI 141))]
  ""
  "push\\t%D1%<\;push\\t%C1%<\;push\\t%B1%<\;push\\t%A1%<\;push\\t%D0%<\;push\\t%C0%<\;push\\t%B0%<\;push\\t%A0%>%>%>%>%>%>%>\;page\\t__divmodsi4\;call\\t__divmodsi4")

(define_expand "udivmoddi4"
  [(parallel [(set (reg:DI 128)
		   (udiv:DI (match_operand:DI 1 "general_operand" "")
			    (match_operand:DI 2 "general_operand" "")))
	      (set (reg:DI 136)
		   (umod:DI (match_dup 1)
		   	    (match_dup 2)))
	      (clobber (reg:QI 144))
	      (clobber (reg:QI 145))
	      (clobber (reg:QI 146))
	      (clobber (reg:QI 147))
	      (clobber (reg:QI 148))
	      (clobber (reg:QI 149))
	      (clobber (reg:QI 150))
	      (clobber (reg:QI 151))])
   (set (match_operand:DI 0 "general_operand" "") (reg:DI 128))
   (set (match_operand:DI 3 "general_operand" "") (reg:DI 136))]
  ""
  "")

(define_insn "*udivmoddi4_call"
  [(set (reg:DI 128)
	(udiv:DI (match_operand:DI 0 "general_operand" "rSi,roi")
		 (match_operand:DI 1 "general_operand" "roi,rSi")))
   (set (reg:DI 136)
	(umod:DI (match_dup 0)
		 (match_dup 1)))
   (clobber (reg:QI 144))
   (clobber (reg:QI 145))
   (clobber (reg:QI 146))
   (clobber (reg:QI 147))
   (clobber (reg:QI 148))
   (clobber (reg:QI 149))
   (clobber (reg:QI 150))
   (clobber (reg:QI 151))]
  ""
  "push\\t%Z1%<\;push\\t%Y1%<\;push\\t%X1%<\;push\\t%W1%<\;push\\t%V1%<\;push\\t%U1%<\;push\\t%T1%<\;push\\t%S1%<\;push\\t%Z0%<\;push\\t%Y0%<\;push\\t%X0%<\;push\\t%W0%<\;push\\t%V0%<\;push\\t%U0%<\;push\\t%T0%<\;push\\t%S00%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>\;page\\t__udivmoddi4\;call\\t__udivmoddi4")

(define_expand "divmoddi4"
  [(parallel [(set (reg:DI 128)
		   (div:DI (match_operand:DI 1 "general_operand" "")
			   (match_operand:DI 2 "general_operand" "")))
	      (set (reg:DI 136)
		   (mod:DI (match_dup 1)
		   	   (match_dup 2)))
	      (clobber (reg:QI 144))
	      (clobber (reg:QI 145))
	      (clobber (reg:QI 146))
	      (clobber (reg:QI 147))
	      (clobber (reg:QI 148))
	      (clobber (reg:QI 149))
	      (clobber (reg:QI 150))
	      (clobber (reg:QI 151))])
   (set (match_operand:DI 0 "general_operand" "") (reg:DI 128))
   (set (match_operand:DI 3 "general_operand" "") (reg:DI 136))]
  ""
  "")

(define_insn "*divmoddi4_call"
  [(set (reg:DI 128)
	(div:DI (match_operand:DI 0 "general_operand" "rSn,ron")
		(match_operand:DI 1 "general_operand" "ron,rSn")))
   (set (reg:DI 136)
	(mod:DI (match_dup 0)
		(match_dup 1)))
   (clobber (reg:QI 144))
   (clobber (reg:QI 145))
   (clobber (reg:QI 146))
   (clobber (reg:QI 147))
   (clobber (reg:QI 148))
   (clobber (reg:QI 149))
   (clobber (reg:QI 150))
   (clobber (reg:QI 151))]
  ""
  "push\\t%Z1%<\;push\\t%Y1%<\;push\\t%X1%<\;push\\t%W1%<\;push\\t%V1%<\;push\\t%U1%<\;push\\t%T1%<\;push\\t%S1%<\;push\\t%Z0%<\;push\\t%Y0%<\;push\\t%X0%<\;push\\t%W0%<\;push\\t%V0%<\;push\\t%U0%<\;push\\t%T0%<\;push\\t%S00%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>\;page\\t__divmoddi4\;call\\t__divmoddi4")

;;
;; Arithmetic shift left instructions.
;;

(define_insn "ashlqi3"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=roR,roR, rS,roR, rS")
	(ashift:QI
	 (match_operand:QI 1 "nonimmediate_operand"   "0, rS,roR,  0,  0")
	 (match_operand:QI 2 "general_operand"        "N,  L,  L, rS,roR")))]
  ""
  "@
   clrb	status,0\;rl\\t%0
   mov\\tw,%e2\;mulu\\tw,%1\;mov\\t%0,w
   mov\\tw,%e2\;mulu\\tw,%1\;mov\\t%0,w
   mov\\tw,%2\;snz\;page\\t1f\;jmp\\t1f\;2:clrb\\tstatus,0\;rl\\t%0\;decsz\\twreg\;page\\t2b\;jmp\\t2b\;1:
   mov\\tw,%2\;snz\;page\\t1f\;jmp\\t1f\;2:clrb\\tstatus,0\;rl\\t%0\;decsz\\twreg\;page\\t2b\;jmp\\t2b\;1:"
  [(set_attr "clobberw" "no,yes,yes,yes,yes")])

;; Convert simple fixed-size shift of a zero-extended QImode value into a
;; multiply as our multiplier is much faster.  We also do this so that the
;;  multiply can possibly be merged into a much faster multiply-and-accumulate
;; operation.
;;
(define_split
  [(set (match_operand:HI 0 "nonimmediate_operand"              "=ro, rS")
  	(ashift:HI (zero_extend:HI
	              (match_operand:QI 1 "nonimmediate_operand" "rS,roR"))
		   (match_operand:QI 2 "const_int_operand"        "J,  J")))]
  "(INTVAL (operands[2]) < 8)"
  [(set (match_dup 0)
  	(mult:HI (zero_extend:HI (match_dup 1))
		 (zero_extend:HI (match_dup 3))))]
  "operands[3] = gen_int_mode (1 << INTVAL (operands[2]), QImode);")

(define_insn_and_split "*ashlhi3_by8_zero_extend"
  [(set (match_operand:HI 0 "nonimmediate_operand"              "=ro, rS")
  	(ashift:HI (zero_extend:HI
	              (match_operand:QI 1 "nonimmediate_operand" "rS,roR"))
		   (const_int 8)))]
  ""
  "#"
  "reload_completed"
  [(set (match_dup 2) (match_dup 1))
   (set (match_dup 3) (const_int 0))]
  "{
    operands[2] = ip2k_get_high_half (operands[0], QImode);
    operands[3] = ip2k_get_low_half (operands[0], QImode);
  }")

(define_insn "*ashlhi3_zero_extend" ;				   0   1
  [(set (match_operand:HI 0 "nonimmediate_operand"              "=ro, rS")
  	(ashift:HI (zero_extend:HI
	              (match_operand:QI 1 "nonimmediate_operand" "rS,roR"))
		   (match_operand:QI 2 "const_int_operand"        "n,  n")))]
  ""
  "*{
    if (INTVAL (operands[2]) < 8)
      return AS2 (mov, w, %1) CR_TAB
	     AS2 (mulu, w, %e2) CR_TAB
	     AS2 (mov, %L0, w) CR_TAB
	     AS2 (mov, w, MULH) CR_TAB
	     AS2 (mov, %H0, w);
    else
      {
        operands[3] = GEN_INT (INTVAL (operands[2]) - 8);
        return AS2 (mov, w, %1) CR_TAB
	       AS2 (mulu, w, %e3) CR_TAB
	       AS2 (mov, %H0, w) CR_TAB
	       AS1 (clr, %L0);
      }
  }")

;; Convert simple fixed-size shift of a HImode value into a multiply as
;; our multiplier is much faster.  We also do this so that the multiply can
;; possibly be merged into a much faster multiply-and-accumulate operation.
;;
(define_split
  [(set (match_operand:HI 0 "nonimmediate_operand"           "=ro,rS")
  	(ashift:HI (match_operand:HI 1 "nonimmediate_operand" "rS,ro")
		   (match_operand:QI 2 "const_int_operand"     "J, J")))]
  "(INTVAL (operands[2]) < 8)"
  [(set (match_dup 0)
  	(mult:HI (match_dup 1)
		 (zero_extend:HI (match_dup 3))))]
  "operands[3] = gen_int_mode (1 << INTVAL (operands[2]), QImode);")

(define_insn_and_split "ashlhi3_split"
  [(set (match_operand:HI 0 "nonimmediate_operand"           "=ro,rS")
  	(ashift:HI (match_operand:HI 1 "nonimmediate_operand" "rS,ro")
		   (match_operand:QI 2 "const_int_operand"     "n, n")))]
  "(INTVAL (operands[2]) >= 8)"
  "#"
  "&& ip2k_reorg_split_himode"
  [(set (match_dup 4) (const_int 0))]
  "{
    operands[3] = ip2k_get_high_half (operands[0], QImode);
    operands[4] = ip2k_get_low_half (operands[0], QImode);
    operands[5] = ip2k_get_low_half (operands[1], QImode);

    if (INTVAL (operands[2]) == 8)
      emit_insn (gen_movqi (operands[3], operands[5]));
    else
      {
        operands[6] = gen_int_mode (INTVAL (operands[2]) - 8, QImode);
	emit_insn (gen_ashlqi3 (operands[3], operands[5], operands[6]));
      }
  }")

(define_insn "ashlhi3" ;			      0   1   2  3   4
  [(set (match_operand:HI 0 "nonimmediate_operand" "=ro,&rS,&ro,ro, rS")
        (ashift:HI
	 (match_operand:HI 1 "nonimmediate_operand"  "0, ro, rS, 0,  0")
	 (match_operand:QI 2 "general_operand"       "L,  L,  L,rS,roR")))]
  ""
  "*{
    switch (which_alternative)
      {
      case 0:
        switch (INTVAL (operands[2]))
	  {
          case 1:
            return AS2 (clrb, status, 0) CR_TAB
                   AS1 (rl, %L0) CR_TAB
	           AS1 (rl, %H0);

          case 2:
            return AS2 (clrb, status, 0) CR_TAB
	           AS1 (rl, %L0) CR_TAB
	           AS1 (rl, %H0) CR_TAB
	           AS2 (clrb, status, 0) CR_TAB
	           AS1 (rl, %L0) CR_TAB
	           AS1 (rl, %H0);

          case 3:
          case 4:
          case 5:
          case 6:
            return AS2 (mov, w, %L1) CR_TAB
	           AS2 (mulu, w, %e2) CR_TAB
	           AS2 (mov, %L0, w) CR_TAB
	           AS1 (push, MULH%<) CR_TAB
	           AS2 (mov, w, %H1) CR_TAB
	           AS2 (mulu, w, %e2) CR_TAB
	           AS2 (or, 1(SP), w) CR_TAB
	           AS1 (pop, %H0%>);

          case 7:
            return AS1 (rr, %H0) CR_TAB
	           AS2 (mov, w, %L0) CR_TAB
	           AS1 (clr, %L0) CR_TAB
	           AS2 (mov, %H0, w) CR_TAB
	           AS1 (rr, %H0) CR_TAB
	           AS1 (rr, %L0);

          default:
	    /* Should be caught by a different insn pattern */
	    abort ();
          }

      case 1:
      case 2:
        switch (INTVAL (operands[2]))
	  {
          case 1:
            return AS2 (clrb, status, 0) CR_TAB
	           AS2 (rl, w, %L1) CR_TAB
	           AS2 (mov, %L0, w) CR_TAB
	           AS2 (rl, w, %H1) CR_TAB
	           AS2 (mov, %H0, w);

          case 2:
            return AS2 (clrb, status, 0) CR_TAB
	           AS2 (rl, w, %L1) CR_TAB
	           AS2 (mov, %L0, w) CR_TAB
	           AS2 (rl, w, %H1) CR_TAB
	           AS2 (mov, %H0, w) CR_TAB
	           AS2 (clrb, status, 0) CR_TAB
	           AS1 (rl, %L0) CR_TAB
	           AS1 (rl, %H0);

          case 3:
          case 4:
          case 5:
          case 6:
	    return AS2 (mov, w, %L1) CR_TAB
	           AS2 (mulu, w, %e2) CR_TAB
	           AS2 (mov, %L0, w) CR_TAB
	           AS1 (push, MULH%<) CR_TAB
	           AS2 (mov, w, %H1) CR_TAB
	           AS2 (mulu, w, %e2) CR_TAB
	           AS2 (or, 1(SP), w) CR_TAB
	           AS1 (pop, %H0%>);

          case 7:
            return AS2 (rr, w, %H1) CR_TAB
                   AS2 (mov, w, %L1) CR_TAB
	           AS1 (clr, %L0) CR_TAB
	           AS2 (mov, %H0, w) CR_TAB
	           AS1 (rr, %H0) CR_TAB
	           AS1 (rr, %L0);

          default:
	    /* Should be caught by a different insn pattern */
	    abort ();
          }

      case 3:
      case 4:
        return AS2 (mov, w, %2) CR_TAB
               AS1 (snz,) CR_TAB
	       AS1 (page, 2f) CR_TAB
	       AS1 (jmp, 2f) CR_TAB
	       AS1 (1:,) CR_TAB
	       AS2 (clrb, status, 0) CR_TAB
               AS1 (rl, %L0) CR_TAB
	       AS1 (rl, %H0) CR_TAB
	       AS1 (decsz, wreg) CR_TAB
	       AS1 (page, 1b) CR_TAB
	       AS1 (jmp, 1b) CR_TAB
	       AS1 (2:,);

      default:
        abort();
      }
  }")

(define_insn_and_split "*ashlsi3_by16_zero_extend"
  [(set (match_operand:SI 0 "nonimmediate_operand"              "=ro,rS")
  	(ashift:SI (zero_extend:SI
	              (match_operand:HI 1 "nonimmediate_operand" "rS,ro"))
		   (const_int 16)))]
  ""
  "#"
  "reload_completed"
  [(set (match_dup 2) (match_dup 1))
   (set (match_dup 3) (const_int 0))]
  "{
    operands[2] = ip2k_get_high_half (operands[0], HImode);
    operands[3] = ip2k_get_low_half (operands[0], HImode);
  }")

(define_insn_and_split "ashlsi3_split"
  [(set (match_operand:SI 0 "nonimmediate_operand"          "=ro,&ro,&rS")
  	(ashift:SI (match_operand:SI 1 "nonimmediate_operand" "0, rS, ro")
		   (match_operand:QI 2 "const_int_operand"    "n,  n,  n")))]
  "(INTVAL (operands[2]) >= 16)"
  "#"
  "&& ip2k_reorg_split_simode"
  [(const_int 0)]
  "{
    operands[3] = ip2k_get_high_half (operands[0], HImode);
    operands[4] = ip2k_get_low_half (operands[0], HImode);
    operands[5] = ip2k_get_low_half (operands[1], HImode);

    if (INTVAL (operands[2]) == 16)
      {
        emit_insn (gen_movhi (operands[3], operands[5]));
        emit_insn (gen_movhi (operands[4], const0_rtx));
      }
    else
      {
        operands[6] = GEN_INT (INTVAL (operands[2]) - 16);
	emit_insn (gen_ashlhi3 (operands[3], operands[5], operands[6]));
        emit_insn (gen_movhi (operands[4], const0_rtx));
      }
  }")

(define_insn "ashlsi3"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=ro, rS,ro,&ro,&rS")
	(ashift:SI
	 (match_operand:SI 1 "nonimmediate_operand"  "0,  0, 0, rS, ro")
	 (match_operand:QI 2 "general_operand"       "L,roR,rS,  L,  L")))]
  ""
  "*{
    switch (which_alternative) {
    case 0:
      switch (INTVAL (operands[2])) {
      case 1:
        return AS2 (clrb, status, 0) CR_TAB
      	       AS1 (rl, %D0) CR_TAB
	       AS1 (rl, %C0) CR_TAB
	       AS1 (rl, %B0) CR_TAB
	       AS1 (rl, %A0);

      case 2:
        return AS2 (clrb, status, 0) CR_TAB
      	       AS1 (rl, %D0) CR_TAB
	       AS1 (rl, %C0) CR_TAB
	       AS1 (rl, %B0) CR_TAB
	       AS1 (rl, %A0) CR_TAB
	       AS2 (clrb, status, 0) CR_TAB
      	       AS1 (rl, %D0) CR_TAB
	       AS1 (rl, %C0) CR_TAB
	       AS1 (rl, %B0) CR_TAB
	       AS1 (rl, %A0);

      case 8:
        return AS2 (mov, w, %B0) CR_TAB
	       AS2 (mov, %A0, w) CR_TAB
	       AS2 (mov, w, %C0) CR_TAB
	       AS2 (mov, %B0, w) CR_TAB
	       AS2 (mov, w, %D0) CR_TAB
	       AS2 (mov, %C0, w) CR_TAB
	       AS1 (clr, %D0);

      case 16:
        return AS2 (mov, w, %C0) CR_TAB
	       AS2 (mov, %A0, w) CR_TAB
	       AS2 (mov, w, %D0) CR_TAB
	       AS2 (mov, %B0, w) CR_TAB
	       AS1 (clr, %C0) CR_TAB
	       AS1 (clr, %D0);

      case 23:
        return AS2 (rr, w, %C0)	CR_TAB
	       AS2 (mov, w, %D0) CR_TAB
	       AS2 (mov, %A0, w) CR_TAB
	       AS1 (clr, %B0) CR_TAB
	       AS1 (clr, %C0) CR_TAB
	       AS1 (clr, %D0) CR_TAB
	       AS1 (rr, %A0) CR_TAB
	       AS1 (rr, %B0);

      case 24:
        return AS2 (mov, w, %D0) CR_TAB
	       AS2 (mov, %A0, w) CR_TAB
	       AS1 (clr, %B0) CR_TAB
	       AS1 (clr, %C0) CR_TAB
	       AS1 (clr, %D0);

      case 31:
        return AS2 (rr, w, %D0)	CR_TAB
	       AS1 (clr, %A0) CR_TAB
	       AS1 (clr, %B0) CR_TAB
	       AS1 (clr, %C0) CR_TAB
	       AS1 (clr, %D0) CR_TAB
	       AS1 (rr, %A0);

      default:
        return AS2 (mov, w, %2) CR_TAB
      	       AS1 (1:,) CR_TAB
      	       AS2 (clrb, status, 0) CR_TAB
      	       AS1 (rl, %D0) CR_TAB
	       AS1 (rl, %C0) CR_TAB
	       AS1 (rl, %B0) CR_TAB
	       AS1 (rl, %A0) CR_TAB
	       AS1 (decsz, wreg) CR_TAB
	       AS1 (page, 1b) CR_TAB
	       AS1 (jmp, 1b);
      }

    case 1:
    case 2:
      return AS2 (mov, w, %2) CR_TAB
      	     AS1 (snz,) CR_TAB
	     AS1 (page, 2f) CR_TAB
	     AS1 (jmp, 2f) CR_TAB
      	     AS1 (1:,) CR_TAB
      	     AS2 (clrb, status, 0) CR_TAB
      	     AS1 (rl, %D0) CR_TAB
	     AS1 (rl, %C0) CR_TAB
	     AS1 (rl, %B0) CR_TAB
	     AS1 (rl, %A0) CR_TAB
	     AS1 (decsz, wreg) CR_TAB
	     AS1 (page, 1b) CR_TAB
	     AS1 (jmp, 1b) CR_TAB
	     AS1 (2:,);

    case 3:
    case 4:
      switch (INTVAL (operands[2])) {
      case 1:
        return AS2 (clrb, status, 0) CR_TAB
      	       AS2 (rl, w, %D1) CR_TAB
	       AS2 (mov, %D0, w) CR_TAB
	       AS2 (rl, w, %C1) CR_TAB
	       AS2 (mov, %C0, w) CR_TAB
	       AS2 (rl, w, %B1) CR_TAB
	       AS2 (mov, %B0, w) CR_TAB
	       AS2 (rl, w, %A1) CR_TAB
	       AS2 (mov, %A0, w);

      case 2:
        return AS2 (clrb, status, 0) CR_TAB
      	       AS2 (rl, w, %D1) CR_TAB
	       AS2 (mov, %D0, w) CR_TAB
	       AS2 (rl, w, %C1) CR_TAB
	       AS2 (mov, %C0, w) CR_TAB
	       AS2 (rl, w, %B1) CR_TAB
	       AS2 (mov, %B0, w) CR_TAB
	       AS2 (rl, w, %A1) CR_TAB
	       AS2 (mov, %A0, w) CR_TAB
      	       AS2 (clrb, status, 0) CR_TAB
      	       AS1 (rl, %D0) CR_TAB
	       AS1 (rl, %C0) CR_TAB
	       AS1 (rl, %B0) CR_TAB
	       AS1 (rl, %A0);

      case 8:
        return AS2 (mov, w, %B1) CR_TAB
	       AS2 (mov, %A0, w) CR_TAB
	       AS2 (mov, w, %C1) CR_TAB
	       AS2 (mov, %B0, w) CR_TAB
	       AS2 (mov, w, %D1) CR_TAB
	       AS2 (mov, %C0, w) CR_TAB
	       AS1 (clr, %D0);

      case 16:
        return AS2 (mov, w, %C1) CR_TAB
	       AS2 (mov, %A0, w) CR_TAB
	       AS2 (mov, w, %D1) CR_TAB
	       AS2 (mov, %B0, w) CR_TAB
	       AS1 (clr, %C0) CR_TAB
	       AS1 (clr, %D0);

      case 23:
        return AS2 (rr, w, %C1)	CR_TAB
	       AS2 (mov, w, %D1) CR_TAB
	       AS2 (mov, %A0, w) CR_TAB
	       AS1 (clr, %B0) CR_TAB
	       AS1 (clr, %C0) CR_TAB
	       AS1 (clr, %D0) CR_TAB
	       AS1 (rr, %A0) CR_TAB
	       AS1 (rr, %B0);

      case 24:
        return AS2 (mov, w, %D1) CR_TAB
	       AS2 (mov, %A0, w) CR_TAB
	       AS1 (clr, %B0) CR_TAB
	       AS1 (clr, %C0) CR_TAB
	       AS1 (clr, %D0);

      case 31:
        return AS2 (rr, w, %D1)	CR_TAB
	       AS1 (clr, %A0) CR_TAB
	       AS1 (clr, %B0) CR_TAB
	       AS1 (clr, %C0) CR_TAB
	       AS1 (clr, %D0) CR_TAB
	       AS1 (rr, %A0);

      default:
        return AS2 (mov, w, %A1) CR_TAB
	       AS2 (mov, %A0, w) CR_TAB
	       AS2 (mov, w, %B1) CR_TAB
	       AS2 (mov, %B0, w) CR_TAB
	       AS2 (mov, w, %C1) CR_TAB
	       AS2 (mov, %C0, w) CR_TAB
	       AS2 (mov, w, %D1) CR_TAB
	       AS2 (mov, %D0, w) CR_TAB
               AS2 (mov, w, %2) CR_TAB
      	       AS1 (1:,) CR_TAB
      	       AS2 (clrb, status, 0) CR_TAB
      	       AS1 (rl, %D0) CR_TAB
	       AS1 (rl, %C0) CR_TAB
	       AS1 (rl, %B0) CR_TAB
	       AS1 (rl, %A0) CR_TAB
	       AS1 (decsz, wreg) CR_TAB
	       AS1 (page, 1b) CR_TAB
	       AS1 (jmp, 1b);
      }
    default:
      abort ();
    }
  }")

(define_insn_and_split "ashldi3_split"
  [(set (match_operand:DI 0 "nonimmediate_operand"          "=ro,&ro,&rS")
  	(ashift:DI (match_operand:DI 1 "nonimmediate_operand" "0, rS, ro")
		   (match_operand:QI 2 "const_int_operand"    "n,  n,  n")))]
  "((INTVAL (operands[2]) >= 32) || (INTVAL (operands[2]) == 16))"
  "#"
  "&& ip2k_reorg_split_dimode"
  [(const_int 0)]
  "{
    operands[3] = ip2k_get_high_half (operands[0], SImode);
    operands[4] = ip2k_get_low_half (operands[0], SImode);
    operands[5] = ip2k_get_low_half (operands[1], SImode);

    if (INTVAL (operands[2]) == 16)
      {
        operands[6] = ip2k_get_high_half (operands[1], SImode);
	operands[7] = ip2k_get_high_half (operands[3], HImode);
	operands[8] = ip2k_get_low_half (operands[3], HImode);
	operands[9] = ip2k_get_high_half (operands[4], HImode);
	operands[10] = ip2k_get_low_half (operands[4], HImode);
	operands[11] = ip2k_get_low_half (operands[6], HImode);
	operands[12] = ip2k_get_high_half (operands[5], HImode);
	operands[13] = ip2k_get_low_half (operands[5], HImode);
	emit_insn (gen_movhi (operands[7], operands[11]));
	emit_insn (gen_movhi (operands[8], operands[12]));
	emit_insn (gen_movhi (operands[9], operands[13]));
	emit_insn (gen_movhi (operands[10], const0_rtx));
      }
    else if (INTVAL (operands[2]) == 32)
      {
        emit_insn (gen_movsi (operands[3], operands[5]));
        emit_insn (gen_movsi (operands[4], const0_rtx));
      }
    else
      {
        operands[6] = GEN_INT (INTVAL (operands[2]) - 32);
	emit_insn (gen_ashlsi3 (operands[3], operands[5], operands[6]));
        emit_insn (gen_movsi (operands[4], const0_rtx));
      }
  }")

;;
;; Arithmetic shift right instructions.
;;

(define_expand "ashrqi3"
  [(set (match_operand:QI 0 "nonimmediate_operand" "")
	(ashiftrt:QI (match_operand:QI 1 "nonimmediate_operand" "")
		     (match_operand:QI 2 "general_operand" "")))]
  ""
  "if (operands[2] == const0_rtx)
     {
       emit_move_insn (operands[0], operands[1]);
       DONE;
     }
  ")

(define_insn "*ashrqi3"
  [(set
    (match_operand:QI 0 "nonimmediate_operand" "=roR,roR, rS,roR, rS,roR, rS")
    (ashiftrt:QI
     (match_operand:QI 1 "nonimmediate_operand" "0,  0,  0, rS,roR, rS,roR")
     (match_operand:QI 2 "general_operand"      "N, rS,roR,  N,  N,  L,  L")))]
  ""
  "*{
    switch (which_alternative)
      {
      case 0:
        return AS2 (rl, w, %0) CR_TAB
	       AS1 (rr, %0);

      case 3:
      case 4:
        return AS2 (rl, w, %1) CR_TAB /* dup the sign bit */
	       AS2 (rr, w, %1) CR_TAB
	       AS2 (mov, %0, w);

      case 5:
      case 6:
	/* Do >> by left-shifting partially into MULH. */
	operands[2] = GEN_INT (8 - INTVAL (operands[2]));
	return AS2 (mov, w, %1) CR_TAB
       	       AS2 (muls, w, %e2) CR_TAB
	       AS2 (mov, w, mulh) CR_TAB
	       AS2 (mov, %0, w);

      case 1:
      case 2:
      default:
	return AS2 (mov, w, %2) CR_TAB
	       AS1 (snz,) CR_TAB
	       AS1 (page, 2f) CR_TAB
	       AS1 (jmp, 2f) CR_TAB
	       AS1 (1:,) CR_TAB
	       AS2 (setb, status, 0) CR_TAB
	       AS2 (sb, %0, 7) CR_TAB
	       AS2 (clrb, status, 0) CR_TAB
	       AS1 (rr, %0) CR_TAB
	       AS1 (decsz, wreg) CR_TAB
	       AS1 (page, 1b) CR_TAB
	       AS1 (jmp, 1b) CR_TAB
	       AS1 (2:,);
      }
  }")

(define_insn "ashrhi3" ;		             0   1   2  3   4
  [(set (match_operand:HI 0 "nonimmediate_operand" "=ro,&rS,&ro,ro, rS")
        (ashiftrt:HI
	 (match_operand:HI 1 "nonimmediate_operand"  "0, ro, rS, 0,  0")
	 (match_operand:QI 2 "general_operand"       "L,  L,  L,rS,roR")))]
  ""
  "*{
    switch (which_alternative) {
    case 0:
      switch (INTVAL (operands[2])) {
      case 1:
        return AS2 (rl, w, %H0) CR_TAB
	       AS1 (rr, %H0) CR_TAB
	       AS1 (rr, %L0);

      case 2:
        return AS2 (rl, w, %H0) CR_TAB
	       AS1 (rr, %H0) CR_TAB
	       AS1 (rr, %L0) CR_TAB
	       AS2 (rl, w, %H0) CR_TAB
	       AS1 (rr, %H0) CR_TAB
	       AS1 (rr, %L0);

      case 8:
	return AS2 (mov, w, %H0) CR_TAB
	       AS2 (mov, %L0, w) CR_TAB
	       AS1 (clr, %H0) CR_TAB
	       AS2 (snb, %L0, 7) CR_TAB
	       AS1 (not, %H0);

      default:
	return AS2 (mov, w, %2) CR_TAB
	       AS1 (1:,) CR_TAB
	       AS2 (setb, status, 0) CR_TAB
	       AS2 (sb, %H0, 7) CR_TAB
	       AS2 (clrb, status, 0) CR_TAB
	       AS1 (rr, %H0) CR_TAB
	       AS1 (rr, %L0) CR_TAB
	       AS1 (decsz, wreg) CR_TAB
	       AS1 (page, 1b) CR_TAB
	       AS1 (jmp, 1b);
      }

    case 1:
    case 2:
      switch (INTVAL (operands[2])) {
      case 1:
        return AS2 (rl, w, %H1) CR_TAB
	       AS2 (rr, w, %H1) CR_TAB
	       AS2 (mov, %H0, w) CR_TAB
	       AS2 (rr, w, %L1) CR_TAB
	       AS2 (mov, %L0, w);

      case 2:
        return AS2 (rl, w, %H1) CR_TAB
	       AS2 (rr, w, %H1) CR_TAB
	       AS2 (mov, %H0, w) CR_TAB
	       AS2 (rr, w, %L1) CR_TAB
	       AS2 (mov, %L0, w) CR_TAB
	       AS2 (rl, w, %H0) CR_TAB
	       AS1 (rr, %H0) CR_TAB
	       AS1 (rr, %L0);

      case 8:
	return AS2 (mov, w, %H1) CR_TAB
	       AS2 (mov, %L0, w) CR_TAB
	       AS1 (clr, %H0) CR_TAB
	       AS2 (snb, %L0, 7) CR_TAB
	       AS1 (not, %H0);

      default:
	return AS2 (mov, w, %L1) CR_TAB
	       AS2 (mov, %L0, w) CR_TAB
	       AS2 (mov, w, %H1) CR_TAB
	       AS2 (mov, %H0, w) CR_TAB
	       AS2 (mov, w, %2) CR_TAB
	       AS1 (1:,) CR_TAB
	       AS2 (setb, status, 0) CR_TAB
	       AS2 (sb, %H0, 7) CR_TAB
	       AS2 (clrb, status, 0) CR_TAB
	       AS1 (rr, %H0) CR_TAB
	       AS1 (rr, %L0) CR_TAB
	       AS1 (decsz, wreg) CR_TAB
	       AS1 (page, 1b) CR_TAB
	       AS1 (jmp, 1b);
      }

    case 3:
    case 4:
      return AS2 (mov, w, %2) CR_TAB
             AS1 (snz,) CR_TAB
	     AS1 (page, 2f) CR_TAB
	     AS1 (jmp, 2f) CR_TAB
	     AS1 (1:,) CR_TAB
	     AS2 (setb, status, 0) CR_TAB
	     AS2 (sb, %H0, 7) CR_TAB
	     AS2 (clrb, status, 0) CR_TAB
             AS1 (rr, %H0) CR_TAB
	     AS1 (rr, %L0) CR_TAB
	     AS1 (decsz, wreg) CR_TAB
	     AS1 (page, 1b) CR_TAB
	     AS1 (jmp, 1b) CR_TAB
	     AS1 (2:,);

    default:
      abort();
    }
  }")

(define_insn "ashrsi3"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=ro, rS,ro,&ro,&rS")
	(ashiftrt:SI
	 (match_operand:SI 1 "nonimmediate_operand"  "0,  0, 0, rS, ro")
	 (match_operand:QI 2 "general_operand"       "L,roR,rS,  L,  L")))]
  ""
  "*{
    switch (which_alternative) {
    case 0:
      switch (INTVAL (operands[2])) {
      case 1:
        return AS2 (rl, w, %A0) CR_TAB /* dup the sign bit */
	       AS1 (rr, %A0) CR_TAB
	       AS1 (rr, %B0) CR_TAB
	       AS1 (rr, %C0) CR_TAB
	       AS1 (rr, %D0);

      case 2:
        return AS2 (rl, w, %A0) CR_TAB /* dup the sign bit */
	       AS1 (rr, %A0) CR_TAB
	       AS1 (rr, %B0) CR_TAB
	       AS1 (rr, %C0) CR_TAB
	       AS1 (rr, %D0) CR_TAB
               AS2 (rl, w, %A0) CR_TAB
	       AS1 (rr, %A0) CR_TAB
	       AS1 (rr, %B0) CR_TAB
	       AS1 (rr, %C0) CR_TAB
	       AS1 (rr, %D0);

      case 8:
        return AS2 (mov, w, %C0) CR_TAB
	       AS2 (mov, %D0, w) CR_TAB
	       AS2 (mov, w, %B0) CR_TAB
	       AS2 (mov, %C0, w) CR_TAB
	       AS2 (mov, w, %A0) CR_TAB
	       AS2 (mov, %B0, w) CR_TAB
	       AS1 (clr, %A0) CR_TAB
	       AS2 (snb, %B0, 7) CR_TAB
	       AS1 (not, %A0);

      case 16:
        return AS2 (mov, w, %B0) CR_TAB
	       AS2 (mov, %D0, w) CR_TAB
	       AS2 (mov, w, %A0) CR_TAB
	       AS2 (mov, %C0, w) CR_TAB
	       AS1 (clr, WREG) CR_TAB
	       AS2 (snb, %C0, 7) CR_TAB
	       AS1 (not, WREG) CR_TAB
	       AS2 (mov, %B0, w) CR_TAB
	       AS2 (mov, %A0, w);

      case 23:
        return AS2 (rl, w, %B0)	CR_TAB
	       AS2 (mov, w, %A0) CR_TAB
	       AS2 (mov, %D0, w) CR_TAB
	       AS1 (clr, WREG) CR_TAB
	       AS2 (snb, %D0, 7) CR_TAB
	       AS1 (not, WREG) CR_TAB
	       AS2 (mov, %C0, w) CR_TAB
	       AS2 (mov, %B0, w) CR_TAB
	       AS2 (mov, %A0, w) CR_TAB
	       AS1 (rl, %D0) CR_TAB
	       AS1 (rl, %C0);

      case 24:
        return AS2 (mov, w, %A0) CR_TAB
	       AS2 (mov, %D0, w) CR_TAB
	       AS1 (clr, WREG) CR_TAB
	       AS2 (snb, %D0, 7) CR_TAB
	       AS1 (not, WREG) CR_TAB
	       AS2 (mov, %C0, w) CR_TAB
	       AS2 (mov, %B0, w) CR_TAB
	       AS2 (mov, %A0, w);

      case 31:
        return AS2 (rl, w, %A0)	CR_TAB
	       AS1 (clr, WREG) CR_TAB
	       AS2 (snb, %A0, 7) CR_TAB
	       AS1 (not, WREG) CR_TAB
	       AS2 (mov, %D0, w) CR_TAB
	       AS2 (mov, %C0, w) CR_TAB
	       AS2 (mov, %B0, w) CR_TAB
	       AS2 (mov, %A0, w) CR_TAB
	       AS1 (rl, %D0);

      default:
        return AS2 (mov, w, %2) CR_TAB
      	       AS1 (1:,) CR_TAB
	       AS2 (setb, status, 0) CR_TAB
	       AS2 (sb, %A0, 7) CR_TAB
      	       AS2 (clrb, status, 0) CR_TAB
      	       AS1 (rr, %A0) CR_TAB
	       AS1 (rr, %B0) CR_TAB
	       AS1 (rr, %C0) CR_TAB
	       AS1 (rr, %D0) CR_TAB
	       AS1 (decsz, WREG) CR_TAB
	       AS1 (page, 1b) CR_TAB
	       AS1 (jmp, 1b);
      }

    case 1:
    case 2:
      return AS2 (mov, w, %2) CR_TAB
      	     AS1 (snz,) CR_TAB
	     AS1 (page, 2f) CR_TAB
	     AS1 (jmp, 2f) CR_TAB
      	     AS1 (1:,) CR_TAB
	     AS2 (setb, status, 0) CR_TAB
	     AS2 (sb, %A0, 7) CR_TAB
      	     AS2 (clrb, status, 0) CR_TAB
      	     AS1 (rr, %A0) CR_TAB
	     AS1 (rr, %B0) CR_TAB
	     AS1 (rr, %C0) CR_TAB
	     AS1 (rr, %D0) CR_TAB
	     AS1 (decsz, WREG) CR_TAB
	     AS1 (page, 1b) CR_TAB
	     AS1 (jmp, 1b) CR_TAB
	     AS1 (2:,);

    case 3:
    case 4:
      switch (INTVAL (operands[2])) {
      case 1:
        return AS2 (rl, w, %A1) CR_TAB /* dup the sign bit */
	       AS2 (rr, w, %A1) CR_TAB
	       AS2 (mov, %A0, w) CR_TAB
	       AS2 (rr, w, %B1) CR_TAB
	       AS2 (mov, %B0, w) CR_TAB
	       AS2 (rr, w, %C1) CR_TAB
	       AS2 (mov, %C0, w) CR_TAB
	       AS2 (rr, w, %D1) CR_TAB
	       AS2 (mov, %D0, w);

      case 2:
        return AS2 (rl, w, %A1) CR_TAB /* dup the sign bit */
	       AS2 (rr, w, %A1) CR_TAB
	       AS2 (mov, %A0, w) CR_TAB
	       AS2 (rr, w, %B1) CR_TAB
	       AS2 (mov, %B0, w) CR_TAB
	       AS2 (rr, w, %C1) CR_TAB
	       AS2 (mov, %C0, w) CR_TAB
	       AS2 (rr, w, %D1) CR_TAB
	       AS2 (mov, %D0, w) CR_TAB
	       AS2 (rl, w, %A0) CR_TAB
               AS1 (rr, %A0) CR_TAB
	       AS1 (rr, %B0) CR_TAB
	       AS1 (rr, %C0) CR_TAB
	       AS1 (rr, %D0);

      case 8:
        return AS2 (mov, w, %C1) CR_TAB
	       AS2 (mov, %D0, w) CR_TAB
	       AS2 (mov, w, %B1) CR_TAB
	       AS2 (mov, %C0, w) CR_TAB
	       AS2 (mov, w, %A1) CR_TAB
	       AS2 (mov, %B0, w) CR_TAB
	       AS1 (clr, %A0) CR_TAB
	       AS2 (snb, %B0, 7) CR_TAB
	       AS1 (not, %A0);

      case 16:
        return AS2 (mov, w, %B1) CR_TAB
	       AS2 (mov, %D0, w) CR_TAB
	       AS2 (mov, w, %A1) CR_TAB
	       AS2 (mov, %C0, w) CR_TAB
	       AS1 (clr, WREG) CR_TAB
	       AS2 (snb, %C0, 7) CR_TAB
	       AS1 (not, WREG) CR_TAB
	       AS2 (mov, %B0, w) CR_TAB
	       AS2 (mov, %A0, w);

      case 23:
        return AS2 (rl, w, %B1)	CR_TAB
	       AS2 (mov, w, %A1) CR_TAB
	       AS2 (mov, %D0, w) CR_TAB
	       AS1 (clr, WREG) CR_TAB
	       AS2 (snb, %D0, 7) CR_TAB
	       AS1 (not, WREG) CR_TAB
	       AS2 (mov, %C0, w) CR_TAB
	       AS2 (mov, %B0, w) CR_TAB
	       AS2 (mov, %A0, w) CR_TAB
	       AS1 (rl, %D0) CR_TAB
	       AS1 (rl, %C0);

      case 24:
        return AS2 (mov, w, %A1) CR_TAB
	       AS2 (mov, %D0, w) CR_TAB
	       AS1 (clr, WREG) CR_TAB
	       AS2 (snb, %D0, 7) CR_TAB
	       AS1 (not, WREG) CR_TAB
	       AS2 (mov, %C0, w) CR_TAB
	       AS2 (mov, %B0, w) CR_TAB
	       AS2 (mov, %A0, w);

      case 31:
        return AS2 (rl, w, %A1)	CR_TAB
	       AS1 (clr, WREG) CR_TAB
	       AS2 (snb, %A1, 7) CR_TAB
	       AS1 (not, WREG) CR_TAB
	       AS2 (mov, %D0, w) CR_TAB
	       AS2 (mov, %C0, w) CR_TAB
	       AS2 (mov, %B0, w) CR_TAB
	       AS2 (mov, %A0, w) CR_TAB
	       AS1 (rl, %D0);

      default:
        return AS2 (mov, w, %A1) CR_TAB
	       AS2 (mov, %A0, w) CR_TAB
	       AS2 (mov, w, %B1) CR_TAB
	       AS2 (mov, %B0, w) CR_TAB
	       AS2 (mov, w, %C1) CR_TAB
	       AS2 (mov, %C0, w) CR_TAB
	       AS2 (mov, w, %D1) CR_TAB
	       AS2 (mov, %D0, w) CR_TAB
               AS2 (mov, w, %2) CR_TAB
      	       AS1 (1:,) CR_TAB
	       AS2 (setb, status, 0) CR_TAB
	       AS2 (sb, %A0, 7) CR_TAB
      	       AS2 (clrb, status, 0) CR_TAB
      	       AS1 (rr, %A0) CR_TAB
	       AS1 (rr, %B0) CR_TAB
	       AS1 (rr, %C0) CR_TAB
	       AS1 (rr, %D0) CR_TAB
	       AS1 (decsz, wreg) CR_TAB
	       AS1 (page, 1b) CR_TAB
	       AS1 (jmp, 1b);
      }
    default:
      abort ();
    }
  }")

;;
;; Logical shift right instructions.
;;

(define_insn "lshrqi3"
  [(set (match_operand:QI
	 0 "nonimmediate_operand" "=roR, rS,roR,roR, rS,&roR,roR, rS")
	(lshiftrt:QI
	 (match_operand:QI
	  1 "nonimmediate_operand"   "0,  0,  0, rS,roR,  rS, rS,roR")
	 (match_operand:QI
	  2 "general_operand"        "N,roR, rS,  N,  N,  rS,  L,  L")))]
  ""
  "*{
    switch (which_alternative)
      {
      case 0:
        return AS2 (clrb, status, 0) CR_TAB
	       AS1 (rr, %0);

      case 1:
      case 2:
        return AS2 (mov, w, %2) CR_TAB
	       AS1 (snz,) CR_TAB
	       AS1 (page, 2f) CR_TAB
	       AS1 (jmp, 2f) CR_TAB
	       AS1 (1:,) CR_TAB
	       AS2 (clrb, status, 0) CR_TAB
	       AS1 (rr, %0) CR_TAB
	       AS1 (decsz, wreg) CR_TAB
	       AS1 (page, 1b) CR_TAB
	       AS1 (jmp, 1b) CR_TAB
	       AS1 (2:,);

      case 3:
      case 4:
        return AS2 (clrb, status, 0) CR_TAB
	       AS2 (rr, w, %1) CR_TAB
	       AS2 (mov, %0, w);

      case 5:
        return AS2 (mov, w, %1) CR_TAB
	       AS2 (mov, %0, w) CR_TAB
	       AS2 (mov, w, %2) CR_TAB
	       AS1 (snz,) CR_TAB
	       AS1 (page, 2f) CR_TAB
	       AS1 (jmp, 2f) CR_TAB
	       AS1 (1:,)
	       AS2 (clrb, status, 0) CR_TAB
	       AS1 (rr, %0) CR_TAB
	       AS1 (decsz, wreg) CR_TAB
	       AS1 (page, 1b) CR_TAB
	       AS1 (jmp, 1b) CR_TAB
	       AS1 (2:,);

      case 6:
      case 7:
	/* Do >> by left-shifting partially into MULH. */
	operands[2] = GEN_INT (8 - INTVAL (operands[2]));
	return AS2 (mov, w, %1) CR_TAB
       	       AS2 (mulu, w, %e2) CR_TAB
	       AS2 (mov, w, mulh) CR_TAB
	       AS2 (mov, %0, w);
      default:
        abort ();
      }
  }")

(define_insn_and_split "lshrhi3_split"
  [(set (match_operand:HI 0 "nonimmediate_operand"             "=ro,rS")
  	(lshiftrt:HI (match_operand:HI 1 "nonimmediate_operand" "rS,ro")
		   (match_operand:QI 2 "const_int_operand"       "n, n")))]
  "(INTVAL (operands[2]) >= 8)"
  "#"
  "&& ip2k_reorg_split_himode"
  [(const_int 0)]
  "{
    operands[3] = ip2k_get_high_half (operands[0], QImode);
    operands[4] = ip2k_get_low_half (operands[0], QImode);
    operands[5] = ip2k_get_high_half (operands[1], QImode);

    if (INTVAL (operands[2]) == 8)
      emit_insn (gen_movqi (operands[4], operands[5]));
    else
      {
        operands[6] = GEN_INT (INTVAL (operands[2]) - 8);
	emit_insn (gen_lshrqi3 (operands[4], operands[5], operands[6]));
      }
    emit_insn (gen_movqi (operands[3], const0_rtx));
  }")

(define_insn "lshrhi3" ;			      0   1   2  3   4
  [(set (match_operand:HI 0 "nonimmediate_operand" "=ro,&rS,&ro,ro, rS")
        (lshiftrt:HI
	 (match_operand:HI 1 "nonimmediate_operand" " 0, ro, rS, 0,  0")
	 (match_operand:QI 2 "general_operand"       "L,  L,  L,rS,roR")))]
  ""
  "*{
    switch (which_alternative)
      {
      case 0:
        switch (INTVAL (operands[2]))
	  {
          case 1:
      	    return AS2 (clrb, status, 0) CR_TAB
	           AS1 (rr, %H0) CR_TAB
	           AS1 (rr, %L0);

          case 2:
            return AS2 (clrb, status, 0) CR_TAB
	           AS1 (rr, %H0) CR_TAB
	           AS1 (rr, %L0) CR_TAB
      	           AS2 (clrb, status, 0) CR_TAB
	           AS1 (rr, %H0) CR_TAB
	           AS1 (rr, %L0);

	  case 3:
	  case 4:
	  case 5:
	  case 6:
	  case 7:
	    operands[2] = GEN_INT (8 - INTVAL (operands[2]));
	    return AS2 (mov, w, %L0) CR_TAB
       	           AS2 (mulu, w, %e2) CR_TAB
	           AS2 (mov, w, MULH) CR_TAB
		   AS2 (mov, %L0, w) CR_TAB
		   AS2 (mov, w, %H0) CR_TAB
		   AS2 (mulu, w, %e2) CR_TAB
		   AS2 (or, %L0, w) CR_TAB
		   AS2 (mov, w, MULH) CR_TAB
		   AS2 (mov, %H0, w);

          default:
	    /* Should be caught by a different insn pattern */
	    abort ();
          }

      case 1:
      case 2:
        switch (INTVAL (operands[2]))
	  {
          case 1:
            return AS2 (clrb, status, 0) CR_TAB
	           AS2 (rr, w, %H1) CR_TAB
	           AS2 (mov, %H0, w) CR_TAB
	           AS2 (rr, w, %L1) CR_TAB
	           AS2 (mov, %L0, w);

          case 2:
      	    return AS2 (clrb, status, 0) CR_TAB
	           AS2 (rr, w, %H1) CR_TAB
	           AS2 (mov, %H0, w) CR_TAB
	           AS2 (rr, w, %L1) CR_TAB
	           AS2 (mov, %L0, w) CR_TAB
      	           AS2 (clrb, status, 0) CR_TAB
	           AS1 (rr, %H0) CR_TAB
	           AS1 (rr, %L0);

	  case 3:
	  case 4:
	  case 5:
	  case 6:
	  case 7:
	    operands[2] = GEN_INT (8 - INTVAL (operands[2]));
	    return AS2 (mov, w, %L1) CR_TAB
       	           AS2 (mulu, w, %e2) CR_TAB
	           AS2 (mov, w, MULH) CR_TAB
		   AS2 (mov, %L0, w) CR_TAB
		   AS2 (mov, w, %H1) CR_TAB
		   AS2 (mulu, w, %e2) CR_TAB
		   AS2 (or, %L0, w) CR_TAB
		   AS2 (mov, w, MULH) CR_TAB
		   AS2 (mov, %H0, w);

          default:
	    /* Should be caught by a different insn pattern */
	    abort ();
          }

      case 3:
      case 4:
        return AS2 (mov, w, %2) CR_TAB
               AS1 (snz,) CR_TAB
	       AS1 (page, 2f) CR_TAB
	       AS1 (jmp, 2f) CR_TAB
	       AS1 (1:,) CR_TAB
	       AS2 (clrb, status, 0) CR_TAB
               AS1 (rr, %H0) CR_TAB
	       AS1 (rr, %L0) CR_TAB
	       AS1 (decsz, wreg) CR_TAB
	       AS1 (page, 1b) CR_TAB
	       AS1 (jmp, 1b) CR_TAB
	       AS1 (2:,);

      default:
        abort();
      }
  }")

(define_insn_and_split "lshrsi3_split"
  [(set (match_operand:SI 0 "nonimmediate_operand"            "=ro,&ro,&rS")
  	(lshiftrt:SI (match_operand:SI 1 "nonimmediate_operand" "0, rS, ro")
		   (match_operand:QI 2 "const_int_operand"      "n,  n,  n")))]
  "(INTVAL (operands[2]) >= 16)"
  "#"
  "&& ip2k_reorg_split_simode"
  [(const_int 0)]
  "{
    operands[3] = ip2k_get_high_half (operands[0], HImode);
    operands[4] = ip2k_get_low_half (operands[0], HImode);
    operands[5] = ip2k_get_high_half (operands[1], HImode);

    if (INTVAL (operands[2]) == 16)
      emit_insn (gen_movhi (operands[4], operands[5]));
    else
      {
        operands[6] = GEN_INT (INTVAL (operands[2]) - 16);
	emit_insn (gen_lshrhi3 (operands[4], operands[5], operands[6]));
      }
    emit_insn (gen_movhi (operands[3], const0_rtx));
  }")

;; This occurs frequently in supporting FP among other things,
;; and out-of-line is almost as big as inline, so....
;;
(define_insn "lshrsi3"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=ro, rS,ro,&ro,&rS")
	(lshiftrt:SI
	 (match_operand:SI 1 "nonimmediate_operand"  "0,  0, 0, rS, ro")
	 (match_operand:QI 2 "general_operand"       "L,roR,rS,  L,  L")))]

  ""
  "*{
    switch (which_alternative) {
    case 0:
      switch (INTVAL (operands[2])) {
      case 1:
        return AS2 (clrb, status, 0) CR_TAB
	       AS1 (rr, %A0) CR_TAB
	       AS1 (rr, %B0) CR_TAB
	       AS1 (rr, %C0) CR_TAB
	       AS1 (rr, %D0);

      case 2:
        return AS2 (clrb, status, 0) CR_TAB
	       AS1 (rr, %A0) CR_TAB
	       AS1 (rr, %B0) CR_TAB
	       AS1 (rr, %C0) CR_TAB
	       AS1 (rr, %D0) CR_TAB
               AS2 (clrb, status, 0) CR_TAB
	       AS1 (rr, %A0) CR_TAB
	       AS1 (rr, %B0) CR_TAB
	       AS1 (rr, %C0) CR_TAB
	       AS1 (rr, %D0);

      case 8:
        return AS2 (mov, w, %C0) CR_TAB
	       AS2 (mov, %D0, w) CR_TAB
	       AS2 (mov, w, %B0) CR_TAB
	       AS2 (mov, %C0, w) CR_TAB
	       AS2 (mov, w, %A0) CR_TAB
	       AS2 (mov, %B0, w) CR_TAB
	       AS1 (clr, %A0);

      case 16:
        return AS2 (mov, w, %B0) CR_TAB
	       AS2 (mov, %D0, w) CR_TAB
	       AS2 (mov, w, %A0) CR_TAB
	       AS2 (mov, %C0, w) CR_TAB
	       AS1 (clr, %B0) CR_TAB
	       AS1 (clr, %A0);

      case 23:
        return AS2 (rl, w, %B0)	CR_TAB
	       AS2 (mov, w, %A0) CR_TAB
	       AS2 (mov, %D0, w) CR_TAB
	       AS1 (clr, %C0) CR_TAB
	       AS1 (clr, %B0) CR_TAB
	       AS1 (clr, %A0) CR_TAB
	       AS1 (rl, %D0) CR_TAB
	       AS1 (rl, %C0);

      case 24:
        return AS2 (mov, w, %A0) CR_TAB
	       AS2 (mov, %D0, w) CR_TAB
	       AS1 (clr, %C0) CR_TAB
	       AS1 (clr, %B0) CR_TAB
	       AS1 (clr, %A0);

      case 31:
        return AS2 (rl, w, %A0)	CR_TAB
	       AS1 (clr, %D0) CR_TAB
	       AS1 (clr, %C0) CR_TAB
	       AS1 (clr, %B0) CR_TAB
	       AS1 (clr, %A0) CR_TAB
	       AS1 (rl, %D0);

      default:
        return AS2 (mov, w, %2) CR_TAB
      	       AS1 (1:,) CR_TAB
      	       AS2 (clrb, status, 0) CR_TAB
      	       AS1 (rr, %A0) CR_TAB
	       AS1 (rr, %B0) CR_TAB
	       AS1 (rr, %C0) CR_TAB
	       AS1 (rr, %D0) CR_TAB
	       AS1 (decsz, wreg) CR_TAB
	       AS1 (page, 1b) CR_TAB
	       AS1 (jmp, 1b);
      }

    case 1:
    case 2:
      return AS2 (mov, w, %2) CR_TAB
      	     AS1 (snz,) CR_TAB
	     AS1 (page, 2f) CR_TAB
	     AS1 (jmp, 2f) CR_TAB
      	     AS1 (1:,) CR_TAB
      	     AS2 (clrb, status, 0) CR_TAB
      	     AS1 (rr, %A0) CR_TAB
	     AS1 (rr, %B0) CR_TAB
	     AS1 (rr, %C0) CR_TAB
	     AS1 (rr, %D0) CR_TAB
	     AS1 (decsz, wreg) CR_TAB
	     AS1 (page, 1b) CR_TAB
	     AS1 (jmp, 1b) CR_TAB
	     AS1 (2:,);

    case 3:
    case 4:
      switch (INTVAL (operands[2])) {
      case 1:
        return AS2 (clrb, status, 0) CR_TAB
	       AS2 (rr, w, %A1) CR_TAB
	       AS2 (mov, %A0, w) CR_TAB
	       AS2 (rr, w, %B1) CR_TAB
	       AS2 (mov, %B0, w) CR_TAB
	       AS2 (rr, w, %C1) CR_TAB
	       AS2 (mov, %C0, w) CR_TAB
	       AS2 (rr, w, %D1) CR_TAB
	       AS2 (mov, %D0, w);

      case 2:
        return AS2 (clrb, status, 0) CR_TAB
	       AS2 (rr, w, %A1) CR_TAB
	       AS2 (mov, %A0, w) CR_TAB
	       AS2 (rr, w, %B1) CR_TAB
	       AS2 (mov, %B0, w) CR_TAB
	       AS2 (rr, w, %C1) CR_TAB
	       AS2 (mov, %C0, w) CR_TAB
	       AS2 (rr, w, %D1) CR_TAB
	       AS2 (mov, %D0, w) CR_TAB
      	       AS2 (clrb, status, 0) CR_TAB
      	       AS1 (rr, %A0) CR_TAB
	       AS1 (rr, %B0) CR_TAB
	       AS1 (rr, %C0) CR_TAB
	       AS1 (rr, %D0);

      case 8:
        return AS2 (mov, w, %C1) CR_TAB
	       AS2 (mov, %D0, w) CR_TAB
	       AS2 (mov, w, %B1) CR_TAB
	       AS2 (mov, %C0, w) CR_TAB
	       AS2 (mov, w, %A1) CR_TAB
	       AS2 (mov, %B0, w) CR_TAB
	       AS1 (clr, %A0);

      case 16:
        return AS2 (mov, w, %B1) CR_TAB
	       AS2 (mov, %D0, w) CR_TAB
	       AS2 (mov, w, %A1) CR_TAB
	       AS2 (mov, %C0, w) CR_TAB
	       AS1 (clr, %B0) CR_TAB
	       AS1 (clr, %A0);

      case 23:
        return AS2 (rl, w, %B1)	CR_TAB
	       AS2 (mov, w, %A1) CR_TAB
	       AS2 (mov, %D0, w) CR_TAB
	       AS1 (clr, %C0) CR_TAB
	       AS1 (clr, %B0) CR_TAB
	       AS1 (clr, %A0) CR_TAB
	       AS1 (rl, %D0) CR_TAB
	       AS1 (rl, %C0);

      case 24:
        return AS2 (mov, w, %A1) CR_TAB
	       AS2 (mov, %D0, w) CR_TAB
	       AS1 (clr, %C0) CR_TAB
	       AS1 (clr, %B0) CR_TAB
	       AS1 (clr, %A0);

      case 31:
        return AS2 (rl, w, %A1)	CR_TAB
	       AS1 (clr, %D0) CR_TAB
	       AS1 (clr, %C0) CR_TAB
	       AS1 (clr, %B0) CR_TAB
	       AS1 (clr, %A0) CR_TAB
	       AS1 (rl, %D0);

      default:
        return AS2 (mov, w, %A1) CR_TAB
	       AS2 (mov, %A0, w) CR_TAB
	       AS2 (mov, w, %B1) CR_TAB
	       AS2 (mov, %B0, w) CR_TAB
	       AS2 (mov, w, %C1) CR_TAB
	       AS2 (mov, %C0, w) CR_TAB
	       AS2 (mov, w, %D1) CR_TAB
	       AS2 (mov, %D0, w) CR_TAB
               AS2 (mov, w, %2) CR_TAB
      	       AS1 (1:,) CR_TAB
      	       AS2 (clrb, status, 0) CR_TAB
      	       AS1 (rr, %A0) CR_TAB
	       AS1 (rr, %B0) CR_TAB
	       AS1 (rr, %C0) CR_TAB
	       AS1 (rr, %D0) CR_TAB
	       AS1 (decsz, wreg) CR_TAB
	       AS1 (page, 1b) CR_TAB
	       AS1 (jmp, 1b);
      }
    default:
      abort ();
    }
  }")

(define_insn_and_split "lshrdi3_split"
  [(set (match_operand:DI 0 "nonimmediate_operand"            "=ro,&ro,&rS")
  	(lshiftrt:DI (match_operand:DI 1 "nonimmediate_operand" "0, rS, ro")
		   (match_operand:QI 2 "const_int_operand"      "n,  n,  n")))]
  "((INTVAL (operands[2]) >= 32) || (INTVAL (operands[2]) == 16))"
  "#"
  "&& ip2k_reorg_split_dimode"
  [(const_int 0)]
  "{
    operands[3] = ip2k_get_high_half (operands[0], SImode);
    operands[4] = ip2k_get_low_half (operands[0], SImode);
    operands[5] = ip2k_get_high_half (operands[1], SImode);

    if (INTVAL (operands[2]) == 16)
      {
        operands[6] = ip2k_get_low_half (operands[1], SImode);
	operands[7] = ip2k_get_high_half (operands[3], HImode);
	operands[8] = ip2k_get_low_half (operands[3], HImode);
	operands[9] = ip2k_get_high_half (operands[4], HImode);
	operands[10] = ip2k_get_low_half (operands[4], HImode);
	operands[11] = ip2k_get_high_half (operands[6], HImode);
	operands[12] = ip2k_get_low_half (operands[5], HImode);
	operands[13] = ip2k_get_high_half (operands[5], HImode);
	emit_insn (gen_movhi (operands[10], operands[11]));
	emit_insn (gen_movhi (operands[9], operands[12]));
	emit_insn (gen_movhi (operands[8], operands[13]));
	emit_insn (gen_movhi (operands[7], const0_rtx));
      }
    else if (INTVAL (operands[2]) == 32)
      {
        emit_insn (gen_movsi (operands[4], operands[5]));
        emit_insn (gen_movsi (operands[3], const0_rtx));
      }
    else
      {
        operands[6] = GEN_INT (INTVAL (operands[2]) - 32);
	emit_insn (gen_lshrsi3 (operands[4], operands[5], operands[6]));
        emit_insn (gen_movsi (operands[3], const0_rtx));
      }
  }")

;;
;; Absolute value conversion instructions.
;;

(define_insn "absqi2"
  [(set (match_operand:QI 0 "nonimmediate_operand"	  "=g")
        (abs:QI (match_operand:QI 1 "nonimmediate_operand" "g")))]
  ""
  "mov\\tw,%1\;snb\\twreg,7\;sub\\tw,#0\;mov\\t%0,w")

(define_insn "abssf2"
  [(set (match_operand:SF 0 "nonimmediate_operand"       "=ro")
        (abs:SF (match_operand:SF 1 "nonimmediate_operand" "0")))]
  ""
  "clrb	%A0,7"
  [(set_attr "clobberw" "no")])

;;
;; Negate (X = 0 - Y) instructions.
;;

(define_insn_and_split "negqi2"
  [(set (match_operand:QI 0 "nonimmediate_operand"       "=ro,&ro,&rS")
	(neg:QI (match_operand:QI 1 "nonimmediate_operand" "0, rS, ro")))]
  ""
  "#"
  ""
  [(set (match_dup 0)
	(not:QI (match_dup 1)))
   (set (match_dup 0)
	(plus:QI (match_dup 0)
		 (const_int 1)))]
  "")

(define_insn_and_split "neghi2"
  [(set (match_operand:HI 0 "nonimmediate_operand"       "=ro,&ro,&rS")
	(neg:HI (match_operand:HI 1 "nonimmediate_operand" "0, rS, ro")))]
  ""
  "#"
  ""
  [(set (match_dup 0)
	(not:HI (match_dup 1)))
   (set (match_dup 0)
	(plus:HI (match_dup 0)
		 (const_int 1)))]
  "")

(define_insn_and_split "negsi2"
  [(set (match_operand:SI 0 "nonimmediate_operand"       "=ro,&ro,&rS")
	(neg:SI (match_operand:SI 1 "nonimmediate_operand" "0, rS, ro")))]
  ""
  "#"
  ""
  [(set (match_dup 0)
	(not:SI (match_dup 1)))
   (set (match_dup 0)
	(plus:SI (match_dup 0)
		 (const_int 1)))]
  "")

(define_insn_and_split "negdi2"
  [(set (match_operand:DI 0 "nonimmediate_operand"       "=ro,&ro,&rS")
	(neg:DI (match_operand:DI 1 "nonimmediate_operand" "0, rS, ro")))]
  ""
  "#"
  ""
  [(set (match_dup 0)
	(not:DI (match_dup 1)))
   (set (match_dup 0)
	(plus:DI (match_dup 0)
		 (const_int 1)))]
  "")

;;
;; Bitwise not (one's complement) instructions.
;;

(define_insn "one_cmplqi2"
  [(set (match_operand:QI 0 "nonimmediate_operand"   "=g,roR, rS")
        (not:QI (match_operand:QI 1 "general_operand" "0, rS,roR")))]
  ""
  "@
   not\\t%0
   not\\tw,%1\;mov\\t%0,w
   not\\tw,%1\;mov\\t%0,w"
  [(set_attr "skip" "yes,no,no")
   (set_attr "clobberw" "no,yes,yes")])

(define_insn_and_split "one_cmplhi2"
  [(set (match_operand:HI 0 "nonimmediate_operand"   "=ro,&ro,&rS")
        (not:HI (match_operand:HI 1 "general_operand"  "0, rS, ro")))]
  ""
  "#"
  "(ip2k_reorg_split_himode)"
  [(set (match_dup 3)
	(not:QI (match_dup 4)))
   (set (match_dup 5)
   	(not:QI (match_dup 6)))]
  "{
    operands[3] = ip2k_get_high_half (operands[0], QImode);
    operands[4] = ip2k_get_high_half (operands[1], QImode);
    operands[5] = ip2k_get_low_half (operands[0], QImode);
    operands[6] = ip2k_get_low_half (operands[1], QImode);
  }")

(define_insn_and_split "one_cmplsi2"
  [(set (match_operand:SI 0 "nonimmediate_operand"   "=ro,&ro,&rS")
        (not:SI (match_operand:SI 1 "general_operand"  "0, rS, ro")))]
  ""
  "#"
  "(ip2k_reorg_split_simode)"
  [(set (match_dup 3)
	(not:HI (match_dup 4)))
   (set (match_dup 5)
   	(not:HI (match_dup 6)))]
  "{
    operands[3] = ip2k_get_high_half (operands[0], HImode);
    operands[4] = ip2k_get_high_half (operands[1], HImode);
    operands[5] = ip2k_get_low_half (operands[0], HImode);
    operands[6] = ip2k_get_low_half (operands[1], HImode);
  }")

(define_insn_and_split "one_cmpldi2"
  [(set (match_operand:DI 0 "nonimmediate_operand"   "=ro,&ro,&rS")
        (not:DI (match_operand:DI 1 "general_operand"  "0, rS, ro")))]
  ""
  "#"
  "(ip2k_reorg_split_dimode)"
  [(set (match_dup 3)
	(not:SI (match_dup 4)))
   (set (match_dup 5)
   	(not:SI (match_dup 6)))]
  "{
    operands[3] = ip2k_get_high_half (operands[0], SImode);
    operands[4] = ip2k_get_high_half (operands[1], SImode);
    operands[5] = ip2k_get_low_half (operands[0], SImode);
    operands[6] = ip2k_get_low_half (operands[1], SImode);
  }")

;;
;; Sign extension instructions.
;;

(define_insn "*push_extendqihi2"
  [(set (match_operand:HI 0 "push_operand"                     "=<,<")
        (sign_extend:HI (match_operand:QI 1 "general_operand" "roR,n")))]
  ""
  "@
   push\\t%1%<\;push\\t#0%<\;snb\\t%1,7\;not\\t1(SP)%>%>
   push\\t%L1\;push\\t%H1"
  [(set_attr "clobberw" "no,no")])

(define_insn "extendqihi2"
  [(set (match_operand:HI 0 "nonimmediate_operand"            "=rS,ro,ro")
        (sign_extend:HI (match_operand:QI 1 "general_operand" "roR,rS, n")))]
  ""
  "*{
    switch (which_alternative)
      {
      case 0:
      case 1:
        if (register_operand (operands[0], HImode)
	    && register_operand (operands[1], QImode)
	    && REGNO (operands[0]) == (REGNO (operands[1]) - 1))
	  return AS1 (clr, %H0) CR_TAB
	  	 AS2 (snb, %1, 7) CR_TAB
		 AS1 (not, %H0);
	else
	  return AS2 (mov, w, %1) CR_TAB
	  	 AS2 (mov, %L0, w) CR_TAB
		 AS1 (clr, %H0) CR_TAB
		 AS2 (snb, wreg, 7) CR_TAB
		 AS1 (not, %H0);

      case 2:
	return AS2 (mov, w, %L1) CR_TAB
	       AS2 (mov, %L0, w) CR_TAB
	       AS2 (mov, w, %H1) CR_TAB
	       AS2 (mov, %H0, w);
      default:
        abort ();
      }
  }")

(define_insn "*push_extendhisi2"
  [(set (match_operand:SI 0 "push_operand"                     "=<,<,<")
        (sign_extend:SI (match_operand:HI 1 "general_operand" "roS,n,s")))]
  ""
  "@
   push\\t%L1%<\;push\\t%H1%<\;clr\\twreg\;snb\\t%H1,7\;not\\twreg\;push\\twreg\;push\\twreg%>%>
   push\\t%D1\;push\\t%C1\;push\\t%B1\;push\\t%A1
   push\\t%L1\;push\\t%H1\;push\\t#0\;push\\t#0"
  [(set_attr "clobberw" "yes,no,no")])

(define_insn "extendhisi2"
  [(set (match_operand:SI 0 "nonimmediate_operand"           "=ro,rS,ro,ro")
        (sign_extend:SI (match_operand:HI 1 "general_operand" "rS,ro, n, s")))]
  ""
  "@
   mov\\tw,%L1\;push\\t%H1%<\;pop\\t%C0%>\;mov\\t%D0,w\;clr\\twreg\;snb\\t%C0,7\;not\\twreg\;mov\\t%B0,w\;mov\\t%A0,w
   mov\\tw,%L1\;push\\t%H1%<\;pop\\t%C0%>\;mov\\t%D0,w\;clr\\twreg\;snb\\t%C0,7\;not\\twreg\;mov\\t%B0,w\;mov\\t%A0,w
   mov\\tw,%D1\;mov\\t%D0,w\;mov\\tw,%C1\;mov\\t%C0,w\;mov\\tw,%B1\;mov\\t%B0,w\;mov\\tw,%A1\;mov\\t%A0,w
   mov\\tw,%L1\;push\\t%H1%<\;pop\\t%C0%>\;mov\\t%D0,w\;clr\\t%B0\;clr\\t%A0")

(define_insn "*push_extendqisi2"
  [(set (match_operand:SI 0 "push_operand"                     "=<,<")
        (sign_extend:SI (match_operand:QI 1 "general_operand" "roR,n")))]
  ""
  "@
   push\\t%1%<\;clr\\twreg\;snb\\t%1,7\;not\\twreg\;push\\twreg\;push\\twreg\;push\\twreg%>
   push\\t%D1\;push\\t%C1\;push\\t%B1\;push\\t%A1"
  [(set_attr "clobberw" "yes,no")])

(define_insn "extendqisi2"
  [(set (match_operand:SI 0 "nonimmediate_operand"           "=ro, rS,ro")
        (sign_extend:SI (match_operand:QI 1 "general_operand" "rS,roR, n")))]
  ""
  "@
   mov\\tw,%1\;mov\\t%D0,w\;clr\\twreg\;snb\\t%1,7\;not\\twreg\;mov\\t%C0,w\;mov\\t%B0,w\;mov\\t%A0,w
   mov\\tw,%1\;mov\\t%D0,w\;clr\\twreg\;snb\\t%1,7\;not\\twreg\;mov\\t%C0,w\;mov\\t%B0,w\;mov\\t%A0,w
   mov\\tw,%D1\;mov\\t%D0,w\;mov\\tw,%C1\;mov\\t%C0,w\;mov\\tw,%B1\;mov\\t%B0,w\;mov\\tw,%A1\;mov\\t%A0,w")

;;
;; Zero extension instructions.
;;

(define_insn "*push_zero_extendqihi2"
  [(set (match_operand:HI 0 "push_operand"                      "=<")
        (zero_extend:HI (match_operand:QI 1 "general_operand" "roRi")))]
  ""
  "push\\t%1\;push\\t#0"
  [(set_attr "clobberw" "no")])

(define_insn_and_split "zero_extendqihi2"
  [(set (match_operand:HI 0 "nonimmediate_operand"            "=ro,  rS")
        (zero_extend:HI (match_operand:QI 1 "general_operand" "rSi,roRi")))]
  ""
  "#"
  "ip2k_reorg_completed"
  [(set (match_dup 3) (match_dup 1))
   (set (match_dup 2) (const_int 0))]
  "{
    operands[2] = ip2k_get_high_half (operands[0], QImode);
    operands[3] = ip2k_get_low_half (operands[0], QImode);
  }")

(define_insn "*push_zero_extendhisi2"
  [(set (match_operand:SI 0 "push_operand"                      "=<")
        (zero_extend:SI (match_operand:HI 1 "general_operand" "roSi")))]
  ""
  "push\\t%L1%<\;push\\t%H1%>\;push\\t#0\;push\\t#0")

(define_insn_and_split "zero_extendhisi2"
  [(set (match_operand:SI 0 "nonimmediate_operand"            "=ro, rS")
        (zero_extend:SI (match_operand:HI 1 "general_operand" "rSi,roi")))]
  ""
  "#"
  "ip2k_reorg_completed"
  [(set (match_dup 3) (match_dup 1))
   (set (match_dup 2) (const_int 0))]
  "{
    operands[2] = ip2k_get_high_half (operands[0], HImode);
    operands[3] = ip2k_get_low_half (operands[0], HImode);
  }")

(define_insn "*push_zero_extendqisi2"
  [(set (match_operand:SI 0 "push_operand"                      "=<")
        (zero_extend:SI (match_operand:QI 1 "general_operand" "roRi")))]
  ""
  "push\\t%1\;push\\t#0\;push\\t#0\;push\\t#0"
  [(set_attr "clobberw" "no")])

(define_insn_and_split "zero_extendqisi2"
  [(set (match_operand:SI 0 "nonimmediate_operand"            "=ro,  rS")
        (zero_extend:SI (match_operand:QI 1 "general_operand" "rSi,roRi")))]
  ""
  "#"
  "ip2k_reorg_completed"
  [(set (match_dup 3) (zero_extend:HI (match_dup 1)))
   (set (match_dup 2) (const_int 0))]
  "{
    operands[2] = ip2k_get_high_half (operands[0], HImode);
    operands[3] = ip2k_get_low_half (operands[0], HImode);
  }")

(define_insn "*push_zero_extendsidi2"
  [(set (match_operand:DI 0 "push_operand"                      "=<")
        (zero_extend:DI (match_operand:SI 1 "general_operand" "roSi")))]
  ""
  "push\\t%D1%<\;push\\t%C1%<\;push\\t%B1%<\;push\\t%A1%>%>%>\;push\\t#0\;push\\t#0\;push\\t#0\;push\\t#0")

(define_insn_and_split "zero_extendsidi2"
  [(set (match_operand:DI 0 "nonimmediate_operand"            "=ro, rS")
        (zero_extend:DI (match_operand:SI 1 "general_operand" "rSi,roi")))]
  ""
  "#"
  "ip2k_reorg_completed"
  [(set (match_dup 3) (match_dup 1))
   (set (match_dup 2) (const_int 0))]
  "{
    operands[2] = ip2k_get_high_half (operands[0], SImode);
    operands[3] = ip2k_get_low_half (operands[0], SImode);
  }")

(define_insn "*push_zero_extendhidi2"
  [(set (match_operand:DI 0 "push_operand"                      "=<")
        (zero_extend:DI (match_operand:HI 1 "general_operand" "roSi")))]
  ""
  "push\\t%L1%<\;push\\t%H1%>\;push\\t#0\;push\\t#0\;push\\t#0\;push\\t#0\;push\\t#0\;push\\t#0"
  [(set_attr "clobberw" "no")])

(define_insn_and_split "zero_extendhidi2"
  [(set (match_operand:DI 0 "nonimmediate_operand"            "=ro, rS")
        (zero_extend:DI (match_operand:HI 1 "general_operand" "rSi,roi")))]
  ""
  "#"
  "ip2k_reorg_completed"
  [(set (match_dup 3) (zero_extend:SI (match_dup 1)))
   (set (match_dup 2) (const_int 0))]
  "{
    operands[2] = ip2k_get_high_half (operands[0], SImode);
    operands[3] = ip2k_get_low_half (operands[0], SImode);
  }")

(define_insn "*push_zero_extendqidi2"
  [(set (match_operand:DI 0 "push_operand"                      "=<")
        (zero_extend:DI (match_operand:QI 1 "general_operand" "roRi")))]
  ""
  "push\\t%1\;push\\t#0\;push\\t#0\;push\\t#0\;push\\t#0\;push\\t#0\;push\\t#0\;push\\t#0"
  [(set_attr "clobberw" "no")])

(define_insn_and_split "zero_extendqidi2"
  [(set (match_operand:DI 0 "nonimmediate_operand"            "=ro,  rS")
        (zero_extend:DI (match_operand:QI 1 "general_operand" "rSi,roRi")))]
  ""
  "#"
  "ip2k_reorg_completed"
  [(set (match_dup 3) (zero_extend:SI (match_dup 1)))
   (set (match_dup 2) (const_int 0))]
  "{
    operands[2] = ip2k_get_high_half (operands[0], SImode);
    operands[3] = ip2k_get_low_half (operands[0], SImode);
  }")

;;
;; Truncation instructions.
;;

(define_insn "truncsihi2"
  [(set (match_operand:HI 0 "nonimmediate_operand"         "=rS, ro")
	(truncate:HI (match_operand:SI 1 "general_operand" "roi,rSi")))]
  ""
  "@
   mov\\tw,%D1\;push\\t%C1%<\;pop\\t%H0%>\;mov\\t%L0,w
   mov\\tw,%D1\;push\\t%C1%<\;pop\\t%H0%>\;mov\\t%L0,w")

(define_insn "truncsiqi2"
  [(set (match_operand:QI 0 "nonimmediate_operand"         "=rS, ro")
	(truncate:QI (match_operand:SI 1 "general_operand" "roi,rSi")))]
  ""
  "@
   mov\\tw,%D1\;mov\\t%0,w
   mov\\tw,%D1\;mov\\t%0,w")

(define_insn "trunchiqi2"
  [(set (match_operand:QI 0 "nonimmediate_operand"         "=rS, ro")
	(truncate:QI (match_operand:HI 1 "general_operand" "roi,rSi")))]
  ""
  "@
   mov\\tw,%L1\;mov\\t%0,w
   mov\\tw,%L1\;mov\\t%0,w")

;;
;; Compare with zero (test) instructions.
;;
;; As we don't have a particularly good set of condition codes we simply
;; tagging our comparison operands for use later within our "compare
;; and branch" instructions.
;;

(define_insn "tstqi"
  [(set (cc0)
        (match_operand:QI 0 "nonimmediate_operand" "roR"))]
  ""
  "* return ip2k_set_compare (operands[0], const0_rtx);")

(define_insn "tsthi"
  [(set (cc0)
	(match_operand:HI 0 "nonimmediate_operand" "roS"))]
  ""
  "* return ip2k_set_compare (operands[0], const0_rtx);")

(define_insn "tstsi"
  [(set (cc0)
	(match_operand:SI 0 "nonimmediate_operand" "roS"))]
  ""
  "* return ip2k_set_compare (operands[0], const0_rtx);")

(define_insn "tstdi"
  [(set (cc0)
	(match_operand:DI 0 "nonimmediate_operand" "roS"))]
  ""
  "* return ip2k_set_compare (operands[0], const0_rtx);")

;;
;; General value comparison instructions.
;;
;; As we don't have a particularly good set of condition codes we simply
;; tagging our comparison operands for use later within our "compare
;; and branch" instructions.
;;

(define_insn "cmpqi"
  [(set (cc0)
        (compare (match_operand:QI 0 "nonimmediate_operand" "roR,  rS")
		 (match_operand:QI 1 "general_operand"      "rSn,roRn")))]
  ""
  "* return ip2k_set_compare (operands[0], operands[1]);")

(define_insn "cmphi"
  [(set (cc0)
        (compare (match_operand:HI 0 "nonimmediate_operand" "ro, rS")
		 (match_operand:HI 1 "general_operand"     "rSn,ron")))]
  ""
  "* return ip2k_set_compare (operands[0], operands[1]);")

(define_insn "cmpsi"
  [(set (cc0)
        (compare (match_operand:SI 0 "nonimmediate_operand" "ro, rS")
		 (match_operand:SI 1 "general_operand"     "rSn,ron")))]
  ""
  "* return ip2k_set_compare (operands[0], operands[1]);")

(define_insn "cmpdi"
  [(set (cc0)
  	(compare (match_operand:DI 0 "nonimmediate_operand" "ro, rS")
		 (match_operand:DI 1 "general_operand"     "rSn,ron")))]
  ""
  "* return ip2k_set_compare (operands[0], operands[1]);")

;;
;; Conditional jump instructions.
;;

(define_expand "beq"
  [(set (pc)
        (if_then_else (eq (cc0) (const_int 0))
                      (label_ref (match_operand 0 "" ""))
                      (pc)))]
  ""
  "")

(define_expand "bne"
  [(set (pc)
        (if_then_else (ne (cc0) (const_int 0))
                      (label_ref (match_operand 0 "" ""))
                      (pc)))]
  ""
  "")

(define_expand "bge"
  [(set (pc)
        (if_then_else (ge (cc0) (const_int 0))
                      (label_ref (match_operand 0 "" ""))
                      (pc)))]
  ""
  "")

(define_expand "bgeu"
  [(set (pc)
        (if_then_else (geu (cc0) (const_int 0))
                      (label_ref (match_operand 0 "" ""))
                      (pc)))]
  ""
  "")

(define_expand "blt"
  [(set (pc)
        (if_then_else (lt (cc0) (const_int 0))
                      (label_ref (match_operand 0 "" ""))
                      (pc)))]
  ""
  "")

(define_expand "bltu"
  [(set (pc)
        (if_then_else (ltu (cc0) (const_int 0))
                      (label_ref (match_operand 0 "" ""))
                      (pc)))]
  ""
  "")


(define_expand "ble"
  [(set (pc)
        (if_then_else (le (cc0) (const_int 0))
                      (label_ref (match_operand 0 "" ""))
                      (pc)))]
  ""
  "")

(define_expand "bleu"
  [(set (pc)
        (if_then_else (leu (cc0) (const_int 0))
                      (label_ref (match_operand 0 "" ""))
                      (pc)))]
  ""
  "")

(define_expand "bgt"
  [(set (pc)
        (if_then_else (gt (cc0) (const_int 0))
                      (label_ref (match_operand 0 "" ""))
                      (pc)))]
  ""
  "")

(define_expand "bgtu"
  [(set (pc)
        (if_then_else (gtu (cc0) (const_int 0))
                      (label_ref (match_operand 0 "" ""))
                      (pc)))]
  ""
  "")


;; Implementation of conditional jumps.
;;
;; The assumption is that a previous test or compare instruction will have
;; provided the arguments to be compared to form cc0 and then we perform
;; a compare and branch operation here.
;;
(define_insn "*unsigned_cmp_branch"
  [(set (pc)
        (if_then_else (match_operator 1 "ip2k_unsigned_comparison_operator"
                        [(cc0)
                         (const_int 0)])
                      (label_ref (match_operand 0 "" ""))
                      (pc)))]
  ""
  "* return ip2k_gen_unsigned_comp_branch (insn, GET_CODE (operands[1]),
                                           operands[0]);")

;; Signed branches use Z, N or synthesized V.
;; result is generated as 0 (LT), 1 (EQ), 2 (GT)
;;
(define_insn "*signed_cmp_branch"
  [(set (pc)
        (if_then_else (match_operator 1 "ip2k_signed_comparison_operator"
                        [(cc0)
                         (const_int 0)])
                      (label_ref (match_operand 0 "" ""))
                      (pc)))]
  ""
  "* return ip2k_gen_signed_comp_branch (insn, GET_CODE (operands[1]),
                                         operands[0]);")

;; Reverse branch - reverse our comparison condition so that we can
;; branch in the opposite sense.
;;
(define_insn_and_split "*rvbranch"
  [(set (pc)
        (if_then_else (match_operator 1 "comparison_operator" [(cc0)
                                                               (const_int 0)])
                      (pc)
                      (label_ref (match_operand 0 "" ""))))]
  ""
  "#"
  "reload_completed"
  [(set (pc)
	(if_then_else (match_dup 2)
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  "{
    operands[2] = gen_rtx_fmt_ee (reverse_condition (GET_CODE (operands[1])),
				  GET_MODE (operands[1]),
				  cc0_rtx, const0_rtx);
   }")

;; This is a bit test and jump sequence.
;;
(define_insn "*bit_cmpqi_branch"
  [(set (pc)
        (if_then_else (match_operator 0 "comparison_operator"
			[(zero_extract
			   (match_operand:QI 1 "nonimmediate_operand" "roR")
			   (const_int 1)
			   (match_operand 2 "immediate_operand" "i"))
			 (const_int 0)])
	              (label_ref (match_operand 3 "" ""))
	              (pc)))]
  "(GET_CODE (operands[0]) == EQ || GET_CODE (operands[0]) == NE)"
  "*{
    if (GET_CODE (operands[0]) == EQ)
      OUT_AS2 (sb, %1, %b2);
    else
      OUT_AS2 (snb, %1, %b2);
    return AS1 (page, %3) CR_TAB
           AS1 (jmp, %3);
  }"
  [(set_attr "clobberw" "no")])

;; This is a bit test and jump sequence but for 16-bit operands.  It's pretty
;; certain that there must be a way to do this using a zero_extract operation,
;; but this didn't seem to want to work so we use a bitwise and instead.  This
;; is exactly as efficient but the combiner handles this OK - the implementation
;; here isn't quite as nice though.
;;
(define_insn "*bit_cmphi_branch"
  [(set
    (pc)
    (if_then_else
     (match_operator 0 "comparison_operator"
		     [(and:HI (match_operand:HI 1 "nonimmediate_operand" "roS")
			      (match_operand    2 "const_int_operand"    "n"))
		      (const_int 0)])
     (label_ref (match_operand 3 "" ""))
     (pc)))]
  "((GET_CODE (operands[0]) == EQ || GET_CODE (operands[0]) == NE)
    && find_one_set_bit_p (INTVAL (operands[2])) != -1)"
  "*{
    int bp = find_one_set_bit_p (INTVAL (operands[2]));
    if (INTVAL (operands[2]) >= 8)
      operands[4] = GEN_INT (bp - 8);
    else
      operands[4] = GEN_INT (bp);

    if (GET_CODE (operands[0]) == EQ)
      {
        if (INTVAL (operands[2]) >= 8)
          OUT_AS2 (sb, %H1, %b4);
        else
          OUT_AS2 (sb, %L1, %b4);
      }
    else
      {
        if (INTVAL (operands[2]) >= 8)
          OUT_AS2 (snb, %H1, %b4);
	else
          OUT_AS2 (snb, %L1, %b4);
      }
    return AS1 (page, %3) CR_TAB
           AS1 (jmp, %3);
  }"
  [(set_attr "clobberw" "no")])

;; Add two operands, compare with a third and branch if equal or not-equal.
;;
(define_insn "*add_and_comp_branch"
  [(set
    (pc)
    (if_then_else
     (match_operator 0 "comparison_operator"
		     [(plus:HI
		       (match_operand:HI 1 "nonimmediate_operand" "ro, rS, rS")
		       (match_operand:HI 2 "general_operand"    "rSn,ron,rSn"))
		      (match_operand:HI 3 "general_operand"    "rSn,rSn,ron")])
     (label_ref (match_operand 4 "" ""))
     (pc)))]
  "(GET_CODE (operands[0]) == EQ || GET_CODE (operands[0]) == NE)"
  "*{
    OUT_AS2 (mov, w, %L2);
    OUT_AS2 (add, w, %L1);
    OUT_AS2 (cse, w, %L3);
    if (GET_CODE (operands[0]) == EQ)
      {
        OUT_AS1 (page, 1f);
        OUT_AS1 (jmp, 1f);
      }
    else
      {
        OUT_AS1 (page, %4);
        OUT_AS1 (jmp, %4);
      }
    OUT_AS2 (mov, w, %H2);
    OUT_AS2 (addc, w, %H1);
    if (GET_CODE (operands[0]) == EQ)
      OUT_AS2 (csne, w, %H3);
    else
      OUT_AS2 (cse, w, %H3);
    OUT_AS1 (page, %4);
    OUT_AS1 (jmp, %4);
    return AS1 (1:, );
  }")

;; Unconditional jump
;;
(define_insn "jump"
  [(set (pc)
        (label_ref (match_operand 0 "" "")))]
  ""
  "page\\t%0\;jmp\\t%0"
  [(set_attr "clobberw" "no")])

;; Indirect jump
;;
(define_insn "indirect_jump"
  [(set (pc) (match_operand:HI 0 "nonimmediate_operand" "ro"))]
  ""
  "page\\t1f\;call\\t1f\;1:mov\\tw,%H0\;mov\\tcallh,w\;mov\\tw,%L0\;mov\\tcalll,w\;ret")

;;
;; Function call instructions.
;;

(define_expand "call"
  [(call (match_operand 0 "" "")
	 (match_operand:HI 1 "" ""))]
  ""
  "")

(define_insn "*call"
  [(call (mem:HI (match_operand:HI 0 "general_operand" "i,roS"))
	 (match_operand:HI 1 "" ""))]
  ""
  "@
   page\\t%b0\;call\\t%b0
   push\\t%L0%<\;push\\t%H0%>\;page\\t__indcall\;call\\t__indcall")

(define_expand "call_pop"
  [(parallel [(call (match_operand 0 "" "")
		    (match_operand:HI 1 "" ""))
	      (set (reg:HI 6)
		   (plus:HI (reg:HI 6)
			    (match_operand:HI 3 "immediate_operand" "")))])]
  ""
  "")

(define_insn "*call_pop"
  [(call (mem:HI (match_operand:HI 0 "general_operand" "i,roS"))
	 (match_operand:HI 1 "" ""))
   (set (reg:HI 6)
        (plus:HI (reg:HI 6)
		 (match_operand:HI 2 "immediate_operand" "")))]
  ""
  "@
   page\\t%b0\;call\\t%b0
   push\\t%L0%<\;push\\t%H0%>\;page\\t__indcall\;call\\t__indcall")

;; Undo any splitting of operands that lead to redundant movhi3 instructions.
;;
(define_peephole2
  [(set (match_operand 0 "ip2k_nonsp_reg_operand" "")
        (match_operand 1 "nonimmediate_operand" ""))
   (parallel [(call (mem:HI (match_dup 0))
   		    (match_operand:HI 2 "" ""))
	      (set (reg:HI 6)
	       	   (plus:HI (reg:HI 6)
		    	    (match_operand:HI 3 "immediate_operand" "")))])]
  ""
  [(parallel [(call (mem:HI (match_dup 1))
   		    (match_dup 2))
	      (set (reg:HI 6)
	           (plus:HI (reg:HI 6)
		   	    (match_dup 3)))])]
  "")

(define_expand "call_value"
  [(set (match_operand 0 "" "")
        (call (match_operand 1 "" "")
	      (match_operand:HI 2 "" "")))]
  ""
  "")

(define_insn "*call_value"
  [(set (match_operand 0 "" "")
        (call (mem:HI (match_operand:HI 1 "general_operand" "i,roS"))
	      (match_operand:HI 2 "" "")))]
  ""
  "@
   page\\t%b1\;call\\t%b1
   push\\t%L1%<\;push\\t%H1%>\;page\\t__indcall\;call\\t__indcall")

(define_expand "call_value_pop"
  [(parallel [(set (match_operand 0 "" "")
		   (call (match_operand 1 "" "")
			 (match_operand:HI 2 "" "")))
	      (set (reg:HI 6)
		   (plus:HI (reg:HI 6)
			    (match_operand:HI 4 "immediate_operand" "")))])]
  ""
  "")

(define_insn "*call_value_pop"
  [(set (match_operand 0 "" "")
	(call (mem:HI (match_operand:HI 1 "general_operand" "i,roS"))
	      (match_operand:HI 2 "" "")))
   (set (reg:HI 6)
	(plus:HI (reg:HI 6)
		 (match_operand:HI 3 "immediate_operand" "")))]
  ""
  "@
   page\\t%b1\;call\\t%b1
   push\\t%L1%<\;push\\t%H1%>\;page\\t__indcall\;call\\t__indcall")

;; Undo any splitting of operands that lead to redundant movhi3 instructions.
;;
(define_peephole2
  [(set (match_operand 0 "ip2k_nonsp_reg_operand" "")
        (match_operand 1 "nonimmediate_operand" ""))
   (parallel [(set (match_operand 2 "" "")
   		   (call (mem:HI (match_dup 0))
   		         (match_operand:HI 3 "" "")))
	      (set (reg:HI 6)
	       	   (plus:HI (reg:HI 6)
		    	    (match_operand:HI 4 "immediate_operand" "")))])]
  ""
  [(parallel [(set (match_dup 2)
  		   (call (mem:HI (match_dup 1))
   		         (match_dup 3)))
	      (set (reg:HI 6)
	           (plus:HI (reg:HI 6)
		   	    (match_dup 4)))])]
  "")

;; Nop instruction.
;;
;; We don't really want nops to appear in our code so just insert a comment.
;;
(define_insn "nop"
  [(const_int 0)]
  ""
  "; nop")


;; SEQ instruction
;;
(define_insn "seq"
  [(set (match_operand:QI 0 "register_operand" "=r")
	(eq:QI (cc0) (const_int 0)))]
  ""
  "* return ip2k_gen_sCOND (insn, EQ, operands[0]);")

;; Tweak SEQ if we can adjust the output operand.  Note that we have to do
;; this via a peephole because we need to ensure that any reloads have taken
;; place before we try to do this.  If there's a reload in order to get our
;; actual result operand then this peephole won't match.
;;
(define_peephole
  [(set (match_operand:QI 0 "register_operand" "")
  	(eq:QI (cc0) (const_int 0)))
   (set (reg:QI 10)
        (match_dup 0))
   (set (match_operand:QI 1 "nonimmediate_operand" "")
   	(reg:QI 10))]
  "find_regno_note (insn, REG_DEAD, REGNO (operands[0]))"
  "* return ip2k_gen_sCOND (insn, EQ, operands[1]);")

;; Another peephole match handles the same merge as above but for cases where
;; we're emulating memory accesses via IP and an offset.
;;
(define_peephole
  [(set (match_operand:QI 0 "register_operand" "")
  	(eq:QI (cc0) (const_int 0)))
   (set (reg:QI 10)
        (match_dup 0))
   (set (mem:QI (plus:HI (reg:HI 4)
			 (match_operand:QI 1 "const_int_operand" "")))
   	(reg:QI 10))]
  "(find_regno_note (insn, REG_DEAD, REGNO (operands[0]))
    && (INTVAL (operands[1]) < 0x100))"
  "*{
      if (INTVAL (operands[1]) == 1)
        OUT_AS1 (inc, ipl);
      else
        {
	  OUT_AS2 (mov, w, %1);
	  OUT_AS2 (add, ipl, w);
	}
      ip2k_gen_sCOND (insn, EQ,
                      gen_rtx_MEM (QImode, gen_rtx_REG (HImode, REG_IP)));
      if (find_regno_note (insn, REG_DEAD, REG_IP))
        {
          if (INTVAL (operands[1]) == 1)
            OUT_AS1 (dec, ipl);
          else
            {
	      OUT_AS2 (mov, w, %1);
	      OUT_AS2 (sub, ipl, w);
	    }
        }
      return \"\";
  }")

;; SNE instruction
;;
(define_insn "sne"
  [(set (match_operand:QI 0 "register_operand" "=r")
	(ne:QI (cc0) (const_int 0)))]
  ""
  "* return ip2k_gen_sCOND (insn, NE, operands[0]);")

;; Tweak SNE if we can adjust the output operand.  Note that we have to do
;; this via a peephole because we need to ensure that any reloads have taken
;; place before we try to do this.  If there's a reload in order to get our
;; actual result operand then this peephole won't match.
;;
(define_peephole
  [(set (match_operand:QI 0 "register_operand" "")
  	(ne:QI (cc0) (const_int 0)))
   (set (reg:QI 10)
        (match_dup 0))
   (set (match_operand:QI 1 "nonimmediate_operand" "")
   	(reg:QI 10))]
  "find_regno_note (PREV_INSN (insn), REG_DEAD, REGNO (operands[0]))"
  "* return ip2k_gen_sCOND (insn, NE, operands[1]);")

;; Another peephole match handles the same merge as above but for cases where
;; we're emulating memory accesses via IP and an offset.
;;
(define_peephole
  [(set (match_operand:QI 0 "register_operand" "")
  	(ne:QI (cc0) (const_int 0)))
   (set (reg:QI 10)
        (match_dup 0))
   (set (mem:QI (plus:HI (reg:HI 4)
			 (match_operand:QI 1 "const_int_operand" "")))
   	(reg:QI 10))]
  "(find_regno_note (PREV_INSN (insn), REG_DEAD, REGNO (operands[0]))
    && (INTVAL (operands[1]) < 0x100))"
  "*{
      if (INTVAL (operands[1]) == 1)
        OUT_AS1 (inc, ipl);
      else
        {
	  OUT_AS2 (mov, w, %1);
	  OUT_AS2 (add, ipl, w);
	}
      ip2k_gen_sCOND (insn, NE,
                      gen_rtx_MEM (QImode, gen_rtx_REG (HImode, REG_IP)));
      if (find_regno_note (insn, REG_DEAD, REG_IP))
        {
          if (INTVAL (operands[1]) == 1)
            OUT_AS1 (dec, ipl);
          else
            {
	      OUT_AS2 (mov, w, %1);
	      OUT_AS2 (sub, ipl, w);
	    }
        }
      return \"\";
  }")



;; Case Dispatch Table Support.
;;
;; Called with 5 arguments:
;;
;; 0. case index
;; 1. lower bound (const_int)
;; 2. range (const_int)
;; 3. label before dispatch table
;; 4. out-of-bounds label
;;
;; With the IP2k we actually really want to do a caseqi but that
;; doesn't exist so we cheat and make it look (to the core of gcc)
;; like we're going to do the SImode stuff but then truncate it
;; away when it's no longer looking :-)
;;
(define_expand "casesi"
  [(set (match_dup 5)
        (truncate:QI (match_operand:SI 0 "general_operand" "g")))
   (set (match_dup 5)
        (minus:QI (match_dup 5)
                  (match_operand 1 "const_int_operand" "n")))
   (set (cc0)
        (compare (match_dup 5)
                 (match_operand 2 "const_int_operand" "n")))
   (set (pc)
        (if_then_else (gtu (cc0)
	                   (const_int 0))
                      (label_ref (match_operand 4 "" ""))
                      (pc)))
   (parallel [(set (pc)
		   (plus:HI (pc)
			    (zero_extend:HI (match_dup 5))))
	      (use (label_ref (match_operand 3 "" "")))
	      (use (match_dup 2))])]
  ""
  "{
    operands[5] = gen_reg_rtx (QImode);
  }")

;; There are TWO instructions per dispatch entry (page & jump), so we
;; multiply by two even though our RTL only indicates a simple addition.
;; Subsequent linker relaxation may well restore this back to what the
;; RTL says though!
;;
;; Note that we handle tables with 128 or more entries differently!
;;
(define_insn "*casedispatch"
  [(set (pc)
	(plus:HI (pc) (zero_extend:HI
		       (match_operand:QI 2 "nonimmediate_operand" "roR,roR"))))
   (use (label_ref (match_operand 0 "" "")))
   (use (match_operand 1 "const_int_operand"                        "K,  n"))]
  ""
  "@
   mov\\tw,%2\;add\\tw,wreg\;add\\tpcl,w
   mov\\tw,%2\;push\\t%0%<\;push\\t#0%<\;add\\tw,wreg\;snc\;inc\\t1(SP)\;add\\t2(SP),w\;snc\;inc\\t1(SP)\;page\\t__indcall\;jmp\\t__indcall%>%>")

;; Handle cleaning up the switch statement stuff.  We can eliminate some
;; register moves in some cases.  Note that our pattern is slightly different
;; to the casesi pattern because our minus has become a plus!
;;
;; Note that as of 07-FEB-2002 we must have this pattern as it is because
;; linker relaxation will not work any other way.
;;
(define_peephole
  [(set (reg:QI 10)
        (plus:QI (match_operand 5 "nonimmediate_operand" "rS,rS,rS,rS")
                  (match_operand 1 "const_int_operand"    "M, n, M, n")))
   (set (match_operand:QI 0 "register_operand"           "+r, r, r, r")
        (reg:QI 10))
   (set (cc0)
        (compare (match_dup 0)
                 (match_operand 2 "const_int_operand"     "K, K, n, n")))
   (set (pc)
        (if_then_else (gtu (cc0)
	                   (const_int 0))
                      (label_ref (match_operand 4 "" ""))
                      (pc)))
   (parallel [(set (pc)
		   (plus:HI (pc)
			    (zero_extend:HI (match_dup 0))))
	      (use (label_ref (match_operand 3 "" "")))
	      (use (match_dup 2))])]
  "(INTVAL (operands[1]) != 0
    && find_regno_note (insn, REG_DEAD, REGNO (operands[0])))"
  "*{
    switch (which_alternative)
      {
      case 0:
      case 2:
        OUT_AS2 (dec, w, %5);
	break;

      case 1:
      case 3:
        OUT_AS2 (mov, w, %1);
	OUT_AS2 (add, w, %5);
	break;
      default:
        abort ();
      }

    OUT_AS2 (cmp, w, %2);
    OUT_AS1 (sc, );
    OUT_AS1 (page, %4);
    OUT_AS1 (jmp, %4);

    switch (which_alternative)
      {
      case 0:
      case 1:
	OUT_AS2 (add, w, WREG);
	OUT_AS2 (add, pcl, w);
	return \"\";

      case 2:
      case 3:
	OUT_AS1 (push, %0%<);
	OUT_AS1 (push, #0%<);
	OUT_AS2 (add, w, WREG);
	OUT_AS1 (snc, );
	OUT_AS1 (inc, 1(SP));
	OUT_AS2 (add, 2(SP), w);
	OUT_AS1 (snc, );
	OUT_AS1 (inc, 1(SP));
	OUT_AS1 (page, __indcall);
	OUT_AS1 (jmp, __indcall%>%>);
	return \"\";
      default:
        abort ();
      }
  }")

(define_peephole
  [(set (cc0)
        (compare (match_operand:QI 0 "nonimmediate_operand" "rS,rS")
                 (match_operand 1 "const_int_operand"        "K, n")))
   (set (pc)
        (if_then_else (gtu (cc0)
	                   (const_int 0))
                      (label_ref (match_operand 2 "" ""))
                      (pc)))
   (parallel [(set (pc)
		   (plus:HI (pc)
			    (zero_extend:HI (match_dup 0))))
	      (use (label_ref (match_operand 3 "" "")))
	      (use (match_dup 1))])]
  ""
  "@
   mov\\tw,%0\;cmp\\tw,%1\;sc\;page\\t%2\;jmp\\t%2\;add\\tw,wreg\;add\\tpcl,w
   mov\\tw,%0\;cmp\\tw,%1\;sc\;page\\t%2\;jmp\\t%2\;push\\t%0%<\;push\\t#0%<\;add\\tw,wreg\;snc\;inc\\t1(SP)\;add\\t2(SP),w\;snc\;inc\\t1(SP)\;page\\t__indcall\;jmp\\t__indcall%>%>")

(define_peephole
  [(set (match_operand:HI 0 "nonimmediate_operand" "+roR")
        (plus:HI (match_dup 0)
                 (const_int -1)))
   (set (cc0)
        (compare (match_dup 0)
		 (match_operand 3 "const_int_operand" "n")))
   (set (pc)
	(if_then_else (match_operator 2 "comparison_operator"
	                [(cc0) (const_int 0)])
		      (label_ref (match_operand 1 "" ""))
		      (pc)))]
  "((GET_CODE (operands[2]) == EQ || GET_CODE (operands[2]) == NE)
    && ((INTVAL (operands[3]) == -1) || (INTVAL (operands[3]) == 65535)))"
  "*{
    OUT_AS2 (mov, w, #255);
    OUT_AS2 (add, %L0, w);
    if ((GET_CODE (operands[0]) == REG)
        && ((REGNO (operands[0]) == REG_DP)
	    || (REGNO (operands[0]) == REG_IP)
	    || (REGNO (operands[0]) == REG_SP)))
      {
        OUT_AS2 (add, %H0, w);
      }
    else
      {
        OUT_AS2 (addc, %H0, w);
      }
    if (GET_CODE (operands[2]) == EQ)
      OUT_AS1 (sc, );
    else
      OUT_AS1 (snc, );
    return AS1 (page, %1) CR_TAB
           AS1 (jmp, %1);
  }")

(define_peephole
  [(set (match_operand:QI 0 "nonimmediate_operand" "+rS")
        (plus:QI (match_dup 0)
                 (const_int -1)))
   (set (cc0)
        (match_dup 0))
   (set (pc)
	(if_then_else (match_operator 2 "comparison_operator"
	                [(cc0) (const_int 0)])
		      (label_ref (match_operand 1 "" ""))
		      (pc)))]
  "(GET_CODE (operands[2]) == EQ || GET_CODE (operands[2]) == NE)"
  "*{
    if (GET_CODE (operands[2]) == EQ)
      OUT_AS1 (decsnz, %0);
    else
      OUT_AS1 (decsz, %0);
    return AS1 (page, %1) CR_TAB
           AS1 (jmp, %1);
  }")

;; Handle move and compare-with-zero operations - we can reuse w across
;; the two operations.
;;
(define_peephole
  [(set (reg:QI 10)
        (match_operand:QI 1 "nonimmediate_operand"  "rS"))
   (set (match_operand:QI 0 "nonimmediate_operand" "=rS")
        (reg:QI 10))
   (set (cc0)
        (match_operand:QI 2 "nonimmediate_operand"  "rS"))
   (set (pc)
	(if_then_else (match_operator 3 "comparison_operator"
	                [(cc0) (const_int 0)])
		      (label_ref (match_operand 4 "" ""))
		      (pc)))]
  "((GET_CODE (operands[3]) == EQ || GET_CODE (operands[3]) == NE)
    && (rtx_equal_p (operands[0], operands[2])
        || rtx_equal_p (operands[1], operands[2])))"
  "*{
    OUT_AS2 (mov, w, %1);
    OUT_AS2 (mov, %0, w);
    if (GET_CODE (operands[3]) == EQ)
      OUT_AS1 (snz, );
    else
      OUT_AS1 (sz, );
    return AS1 (page, %4) CR_TAB
           AS1 (jmp, %4);
  }")

;; Handle move and compare-with-zero operations - we can reuse w across
;; the two operations.
;;
(define_peephole
  [(set (reg:QI 10)
  	(match_operand:QI 1 "nonimmediate_operand"  "uS"))
   (set (match_operand:QI 0 "nonimmediate_operand" "+uS")
        (reg:QI 10))
   (set (cc0)
        (match_operand:SI 2 "nonimmediate_operand"  "uS"))
   (set (pc)
	(if_then_else (match_operator 3 "comparison_operator"
	                [(cc0) (const_int 0)])
		      (label_ref (match_operand 4 "" ""))
		      (pc)))]
  "((GET_CODE (operands[3]) == EQ || GET_CODE (operands[3]) == NE)
    && (rtx_equal_p (operands[0],
                     ip2k_get_high_half (ip2k_get_high_half (operands[2],
                                                             HImode), QImode))
        || rtx_equal_p (operands[1],
                        ip2k_get_high_half (ip2k_get_high_half (operands[2],
                                                                HImode),
                                            QImode))))"
  "*{
    OUT_AS2 (mov, w, %1);
    OUT_AS2 (mov, %0, w);
    OUT_AS2 (or, w, %B2);
    OUT_AS2 (or, w, %C2);
    OUT_AS2 (or, w, %D2);
    if (GET_CODE (operands[3]) == EQ)
      OUT_AS1 (snz, );
    else
      OUT_AS1 (sz, );
    return AS1 (page, %4) CR_TAB
           AS1 (jmp, %4);
  }")

;; Handle move and compare-with-zero operations - we can reuse w across
;; the two operations.
;;
(define_peephole
  [(set (reg:QI 10)
  	(match_operand:QI 1 "nonimmediate_operand"  "uS"))
   (set (match_operand:QI 0 "nonimmediate_operand" "+uS")
        (reg:QI 10))
   (set (cc0)
        (match_operand:HI 2 "nonimmediate_operand"  "uS"))
   (set (pc)
	(if_then_else (match_operator 3 "comparison_operator"
	                [(cc0) (const_int 0)])
		      (label_ref (match_operand 4 "" ""))
		      (pc)))]
  "((GET_CODE (operands[3]) == EQ || GET_CODE (operands[3]) == NE)
    && (rtx_equal_p (operands[0], ip2k_get_high_half (operands[2], QImode))
        || rtx_equal_p (operands[1], ip2k_get_high_half (operands[2], QImode))
        || rtx_equal_p (operands[0], ip2k_get_low_half (operands[2], QImode))
        || rtx_equal_p (operands[1], ip2k_get_low_half (operands[2],QImode))))"
  "*{
    OUT_AS2 (mov, w, %1);
    OUT_AS2 (mov, %0, w);
    if (rtx_equal_p (operands[0], ip2k_get_high_half (operands[2], QImode))
        || rtx_equal_p (operands[1], ip2k_get_high_half (operands[2], QImode)))
      OUT_AS2 (or, w, %L2);
    else
      OUT_AS2 (or, w, %H2);
    if (GET_CODE (operands[3]) == EQ)
      OUT_AS1 (snz, );
    else
      OUT_AS1 (sz, );
    return AS1 (page, %4) CR_TAB
           AS1 (jmp, %4);
  }")

;; Handle move and compare-with-zero operations - we can reuse w across
;; the two operations.
;;
(define_peephole
  [(set (match_operand:HI 0 "nonimmediate_operand" "+uo")
        (match_operand:HI 1 "nonimmediate_operand"  "uo"))
   (set (cc0)
        (match_dup 0))
   (set (pc)
	(if_then_else (match_operator 2 "comparison_operator"
	                [(cc0) (const_int 0)])
		      (label_ref (match_operand 3 "" ""))
		      (pc)))]
  "(GET_CODE (operands[2]) == EQ || GET_CODE (operands[2]) == NE)"
  "*{
    OUT_AS2 (mov, w, %H1);
    OUT_AS1 (push, %L1%<);
    OUT_AS1 (pop, %L0%>);
    OUT_AS2 (mov, %H0, w);
    OUT_AS2 (or, w, %L0);
    if (GET_CODE (operands[2]) == EQ)
      OUT_AS1 (snz, );
    else
      OUT_AS1 (sz, );
    return AS1 (page, %3) CR_TAB
           AS1 (jmp, %3);
  }")

;; Handle move and compare-with-zero operations - we can reuse w across
;; the two operations.
;;
(define_peephole
  [(set (match_operand:HI 0 "nonimmediate_operand" "+uo")
        (match_operand:HI 1 "nonimmediate_operand"  "uo"))
   (set (cc0)
        (match_dup 1))
   (set (pc)
	(if_then_else (match_operator 2 "comparison_operator"
	                [(cc0) (const_int 0)])
		      (label_ref (match_operand 3 "" ""))
		      (pc)))]
  "(GET_CODE (operands[2]) == EQ || GET_CODE (operands[2]) == NE)"
  "*{
    OUT_AS2 (mov, w, %H1);
    OUT_AS1 (push, %L1%<);
    OUT_AS1 (pop, %L0%>);
    OUT_AS2 (mov, %H0, w);
    OUT_AS2 (or, w, %L0);
    if (GET_CODE (operands[2]) == EQ)
      OUT_AS1 (snz, );
    else
      OUT_AS1 (sz, );
    return AS1 (page, %3) CR_TAB
           AS1 (jmp, %3);
  }")

(define_peephole
  [(set (match_operand:HI 0 "nonimmediate_operand" "+f,bqdo")
        (mem:HI (reg:HI 4)))
   (set (cc0)
        (match_dup 0))
   (set (pc)
	(if_then_else (match_operator 1 "comparison_operator"
	                [(cc0) (const_int 0)])
		      (label_ref (match_operand 2 "" ""))
		      (pc)))]
  "(GET_CODE (operands[1]) == EQ || GET_CODE (operands[1]) == NE)"
  "*{
    switch (which_alternative)
      {
      case 0:
        OUT_AS1 (push, (IP));
	OUT_AS1 (inc, ipl);
	OUT_AS2 (mov, w, (IP));
	OUT_AS2 (mov, ipl, w);
	OUT_AS1 (pop, iph);
        OUT_AS2 (or, w, iph);
        if (GET_CODE (operands[1]) == EQ)
          OUT_AS1 (snz, );
        else
          OUT_AS1 (sz, );
        return AS1 (page, %2) CR_TAB
               AS1 (jmp, %2);

      case 1:
        OUT_AS2 (mov, w, (IP));
	OUT_AS2 (mov, %H0, w);
	OUT_AS1 (inc, ipl);
	OUT_AS2 (mov, w, (IP));
	OUT_AS2 (mov, %L0, w);
        if (!find_regno_note (insn, REG_DEAD, REG_IP))
	  OUT_AS1 (dec, ipl);
        OUT_AS2 (or, w, %H0);
        if (GET_CODE (operands[1]) == EQ)
          OUT_AS1 (snz, );
        else
          OUT_AS1 (sz, );
        return AS1 (page, %2) CR_TAB
               AS1 (jmp, %2);
      default:
        abort ();
      }
  }")

(define_peephole
  [(set (match_operand:HI 0 "nonimmediate_operand" "+f,bqdo")
        (mem:HI (reg:HI 4)))
   (set (cc0)
        (mem:HI (reg:HI 4)))
   (set (pc)
	(if_then_else (match_operator 1 "comparison_operator"
	                [(cc0) (const_int 0)])
		      (label_ref (match_operand 2 "" ""))
		      (pc)))]
  "(GET_CODE (operands[1]) == EQ || GET_CODE (operands[1]) == NE)"
  "*{
    switch (which_alternative)
      {
      case 0:
        OUT_AS1 (push, (IP));
	OUT_AS1 (inc, ipl);
	OUT_AS2 (mov, w, (IP));
	OUT_AS2 (mov, ipl, w);
	OUT_AS1 (pop, iph);
        OUT_AS2 (or, w, iph);
        if (GET_CODE (operands[1]) == EQ)
          OUT_AS1 (snz, );
        else
          OUT_AS1 (sz, );
        return AS1 (page, %2) CR_TAB
               AS1 (jmp, %2);

      case 1:
        OUT_AS2 (mov, w, (IP));
	OUT_AS2 (mov, %H0, w);
	OUT_AS1 (inc, ipl);
	OUT_AS2 (mov, w, (IP));
	OUT_AS2 (mov, %L0, w);
        if (!find_regno_note (insn, REG_DEAD, REG_IP))
	  OUT_AS1 (dec, ipl);
        OUT_AS2 (or, w, %H0);
        if (GET_CODE (operands[1]) == EQ)
          OUT_AS1 (snz, );
        else
          OUT_AS1 (sz, );
        return AS1 (page, %2) CR_TAB
               AS1 (jmp, %2);
      default:
        abort ();
      }
  }")

;; Handle move-twice and compare-with-zero operations - we can reuse w across
;; the two operations.
;;
(define_peephole
  [(parallel [(set (match_operand:HI 0 "ip2k_gen_operand" "=uS")
                   (match_operand:HI 1 "ip2k_gen_operand"  "uS"))
              (set (match_operand:HI 2 "ip2k_gen_operand" "=uS")
                   (match_dup 1))])
   (set (cc0)
        (match_dup 0))
   (set (pc)
	(if_then_else (match_operator 3 "comparison_operator"
	                [(cc0) (const_int 0)])
		      (label_ref (match_operand 4 "" ""))
		      (pc)))]
  "(GET_CODE (operands[3]) == EQ || GET_CODE (operands[3]) == NE)"
  "*{
    if ((REG_P (operands[0])
         && !(ip2k_xexp_not_uses_reg_p (operands[1], REGNO (operands[0]), 2)
	      && ip2k_xexp_not_uses_reg_p (operands[2],
                                           REGNO (operands[0]), 2)))
        || (REG_P (operands[2])
            && !(ip2k_xexp_not_uses_reg_p (operands[0], REGNO (operands[2]), 2)
	         && ip2k_xexp_not_uses_reg_p (operands[1],
                                              REGNO (operands[2]), 2))))
      {
        OUT_AS2 (mov, w, %L1);
	OUT_AS1 (push, %H1%<);
	OUT_AS1 (push, %H1%<);
	OUT_AS1 (pop, %H0%>);
	OUT_AS2 (mov, %L0, w);
	OUT_AS1 (pop, %H2%>);
	OUT_AS2 (mov, %L2, w);
        OUT_AS2 (or, w, %H2);
        if (GET_CODE (operands[3]) == EQ)
          OUT_AS1 (snz, );
        else
          OUT_AS1 (sz, );
        return AS1 (page, %4) CR_TAB
               AS1 (jmp, %4);
      }
    else
      {
        OUT_AS2 (mov, w, %L1);
	OUT_AS2 (mov, %L0, w);
	OUT_AS2 (mov, %L2, w);
	OUT_AS2 (mov, w, %H1);
	OUT_AS2 (mov, %H0, w);
	OUT_AS2 (mov, %H2, w);
        OUT_AS2 (or, w, %L2);
        if (GET_CODE (operands[3]) == EQ)
          OUT_AS1 (snz, );
        else
          OUT_AS1 (sz, );
        return AS1 (page, %4) CR_TAB
               AS1 (jmp, %4);
      }
  }")

(define_peephole
  [(parallel [(set (match_operand:HI 0 "ip2k_gen_operand" "=uS")
                   (match_operand:HI 1 "ip2k_gen_operand"  "uS"))
              (set (match_operand:HI 2 "ip2k_gen_operand" "=uS")
                   (match_dup 1))])
   (set (cc0)
        (match_dup 2))
   (set (pc)
	(if_then_else (match_operator 3 "comparison_operator"
	                [(cc0) (const_int 0)])
		      (label_ref (match_operand 4 "" ""))
		      (pc)))]
  "(GET_CODE (operands[3]) == EQ || GET_CODE (operands[3]) == NE)"
  "*{
    if ((REG_P (operands[0])
         && !(ip2k_xexp_not_uses_reg_p (operands[1], REGNO (operands[0]), 2)
	      && ip2k_xexp_not_uses_reg_p (operands[2],
                                           REGNO (operands[0]), 2)))
        || (REG_P (operands[2])
            && !(ip2k_xexp_not_uses_reg_p (operands[0], REGNO (operands[2]), 2)
	         && ip2k_xexp_not_uses_reg_p (operands[1],
                                              REGNO (operands[2]), 2))))
      {
        OUT_AS2 (mov, w, %L1);
	OUT_AS1 (push, %H1%<);
	OUT_AS1 (push, %H1%<);
	OUT_AS1 (pop, %H0%>);
	OUT_AS2 (mov, %L0, w);
	OUT_AS1 (pop, %H2%>);
	OUT_AS2 (mov, %L2, w);
        OUT_AS2 (or, w, %H2);
        if (GET_CODE (operands[3]) == EQ)
          OUT_AS1 (snz, );
        else
          OUT_AS1 (sz, );
        return AS1 (page, %4) CR_TAB
               AS1 (jmp, %4);
      }
    else
      {
        OUT_AS2 (mov, w, %L1);
	OUT_AS2 (mov, %L0, w);
	OUT_AS2 (mov, %L2, w);
	OUT_AS2 (mov, w, %H1);
	OUT_AS2 (mov, %H0, w);
	OUT_AS2 (mov, %H2, w);
        OUT_AS2 (or, w, %L2);
        if (GET_CODE (operands[3]) == EQ)
          OUT_AS1 (snz, );
        else
          OUT_AS1 (sz, );
        return AS1 (page, %4) CR_TAB
               AS1 (jmp, %4);
      }
  }")

;; Handle move and compare-with-zero operations - we can reuse w across
;; the two operations.
;;
(define_peephole
  [(set (match_operand:HI 0 "nonimmediate_operand" "+uo")
        (match_operand:HI 1 "nonimmediate_operand"  "uo"))
   (set (cc0)
        (match_operand:SI 2 "nonimmediate_operand"  "uo"))
   (set (pc)
	(if_then_else (match_operator 3 "comparison_operator"
	                [(cc0) (const_int 0)])
		      (label_ref (match_operand 4 "" ""))
		      (pc)))]
  "((GET_CODE (operands[3]) == EQ || GET_CODE (operands[3]) == NE)
    && (rtx_equal_p (operands[0], ip2k_get_high_half (operands[2], HImode))
        || rtx_equal_p (operands[1],
                        ip2k_get_high_half (operands[2], HImode))))"
  "*{
    OUT_AS2 (mov, w, %H1);
    OUT_AS1 (push, %L1%<);
    OUT_AS1 (pop, %L0%>);
    OUT_AS2 (mov, %H0, w);
    OUT_AS2 (or, w, %B0);
    OUT_AS2 (or, w, %C0);
    OUT_AS2 (or, w, %D0);
    if (GET_CODE (operands[3]) == EQ)
      OUT_AS1 (snz, );
    else
      OUT_AS1 (sz, );
    return AS1 (page, %4) CR_TAB
           AS1 (jmp, %4);
  }")

;; Handle bitwise-and and compare-with-zero operations on bytes.
;;
(define_peephole
  [(set (reg:QI 10)
  	(match_operand:QI 2 "general_operand" "        g"))
   (set (reg:QI 10)
        (and:QI (match_operand:QI 1 "general_operand" "g")
		(reg:QI 10)))
   (set (match_operand:QI 0 "register_operand"       "+r")
   	(reg:QI 10))
   (set (cc0)
        (match_dup 0))
   (set (pc)
	(if_then_else (match_operator 3 "comparison_operator"
	                [(cc0) (const_int 0)])
		      (label_ref (match_operand 4 "" ""))
		      (pc)))]
  "(find_regno_note (PREV_INSN (insn), REG_DEAD, REGNO (operands[0]))
    && (GET_CODE (operands[3]) == EQ || GET_CODE (operands[3]) == NE))"
  "*{
    OUT_AS2 (mov, w, %1);
    OUT_AS2 (and, w, %2);
    if (GET_CODE (operands[3]) == EQ)
      OUT_AS1 (snz, );
    else
      OUT_AS1 (sz, );
    return AS1 (page, %4) CR_TAB
           AS1 (jmp, %4);
  }")

;; Handle bitwise-xor and compare-with-zero operations on bytes.
;;
(define_peephole
  [(set (match_operand:QI 0 "register_operand"       "+r")
        (xor:QI (match_operand:QI 1 "general_operand" "g")
		(match_operand:QI 2 "general_operand" "g")))
   (set (cc0)
        (match_dup 0))
   (set (pc)
	(if_then_else (match_operator 3 "comparison_operator"
	                [(cc0) (const_int 0)])
		      (label_ref (match_operand 4 "" ""))
		      (pc)))]
  "(find_regno_note (PREV_INSN (insn), REG_DEAD, REGNO (operands[0]))
    && (GET_CODE (operands[3]) == EQ || GET_CODE (operands[3]) == NE))"
  "*{
    OUT_AS2 (mov, w, %1);
    OUT_AS2 (xor, w, %2);
    if (GET_CODE (operands[3]) == EQ)
      OUT_AS1 (snz, );
    else
      OUT_AS1 (sz, );
    return AS1 (page, %4) CR_TAB
           AS1 (jmp, %4);
  }")

;; Cope with reload's vagaries.
;;

(define_insn "*pushqi_reload_popqi"
  [(set (match_operand:QI 0 "ip2k_nonsp_reg_operand" "=u, u")
        (match_operand:QI 1 "ip2k_short_operand"      "S, S"))
   (set (reg:HI 12)
        (match_operand:HI 2 "general_operand"         "i,ro"))
   (set (match_operand:QI 3 "ip2k_short_operand"     "=S, S")
        (match_dup 0))]
  ""
  "@
   push\\t%1%<\;loadh\\t%x2\;loadl\\t%x2\;pop\\t%3%>
   push\\t%1%<\;mov\\tw,%L2\;push\\t%H2\;pop\\tdph\;mov\\tdpl,w\;pop\\t%3%>"
)

(define_peephole2
  [(set (match_operand:QI 0 "ip2k_nonsp_reg_operand" "")
        (match_operand:QI 1 "ip2k_short_operand" ""))
   (set (reg:HI 12)
        (match_operand:HI 2 "general_operand" ""))
   (set (match_operand:QI 3 "ip2k_short_operand" "")
        (match_dup 0))]
  "(ip2k_reorg_split_himode
    && peep2_reg_dead_p (3, operands[0])
    && ip2k_address_uses_reg_p (operands[1], REG_DP)
    && ip2k_address_uses_reg_p (operands[3], REG_DP)
    && !(ip2k_address_uses_reg_p (operands[2], REG_SP)
         && (GET_CODE (XEXP (operands[2], 0)) == PLUS)
         && (INTVAL (XEXP (XEXP (operands[2], 0), 1)) >= 126))
    && ip2k_xexp_not_uses_reg_p (operands[2], REGNO (operands[0]),
                                 GET_MODE_SIZE (GET_MODE (operands[0]))))"
  [(parallel [(set (match_dup 0)
                   (match_dup 1))
	      (set (reg:HI 12)
 	           (match_dup 2))
	      (set (match_dup 3)
 	           (match_dup 0))])]
  "")

(define_insn "*pushhi_reload_pophi"
  [(set (match_operand:HI 0 "ip2k_nonsp_reg_operand" "=u, u")
        (match_operand:HI 1 "ip2k_short_operand"      "S, S"))
   (set (reg:HI 12)
        (match_operand:HI 2 "general_operand"         "i,ro"))
   (set (match_operand:HI 3 "ip2k_short_operand"     "=S, S")
        (match_dup 0))]
  ""
  "@
   push\\t%L1%<\;push\\t%H1%<\;loadh\\t%x2\;loadl\\t%x2\;pop\\t%H3%>\;pop\\t%L3%>
   push\\t%L1%<\;push\\t%H1%<\;mov\\tw,%L2\;push\\t%H2\;pop\\tdph\;mov\\tdpl,w\;pop\\t%H3%>\;pop\\t%L3%>"
)

(define_peephole2
  [(set (match_operand:HI 0 "ip2k_nonsp_reg_operand" "")
        (match_operand:HI 1 "ip2k_short_operand" ""))
   (set (reg:HI 12)
        (match_operand:HI 2 "general_operand" ""))
   (set (match_operand:HI 3 "ip2k_short_operand" "")
        (match_dup 0))]
  "(ip2k_reorg_split_simode
    && peep2_reg_dead_p (3, operands[0])
    && ip2k_address_uses_reg_p (operands[1], REG_DP)
    && ip2k_address_uses_reg_p (operands[3], REG_DP)
    && !(ip2k_address_uses_reg_p (operands[2], REG_SP)
         && (GET_CODE (XEXP (operands[2], 0)) == PLUS)
         && (INTVAL (XEXP (XEXP (operands[2], 0), 1)) >= 125))
    && ip2k_xexp_not_uses_reg_p (operands[2], REGNO (operands[0]),
                                 GET_MODE_SIZE (GET_MODE (operands[0]))))"
  [(parallel [(set (match_dup 0)
                   (match_dup 1))
	      (set (reg:HI 12)
 	           (match_dup 2))
	      (set (match_dup 3)
 	           (match_dup 0))])]
  "")

(define_insn "*pushsi_reload_popsi"
  [(set (match_operand:SI 0 "ip2k_nonsp_reg_operand" "=u, u")
        (match_operand:SI 1 "ip2k_short_operand"      "S, S"))
   (set (reg:HI 12)
        (match_operand:HI 2 "general_operand"         "i,ro"))
   (set (match_operand:SI 3 "ip2k_short_operand"     "=S, S")
        (match_dup 0))]
  ""
  "@
   push\\t%D1%<\;push\\t%C1%<\;push\\t%B1%<\;push\\t%A1%<\;loadh\\t%x2\;loadl\\t%x2\;pop\\t%A3%>\;pop\\t%B3%>\;pop\\t%C3%>\;pop\\t%D3%>
   push\\t%D1%<\;push\\t%C1%<\;push\\t%B1%<\;push\\t%A1%<\;mov\\tw,%L2\;push\\t%H2\;pop\\tdph\;mov\\tdpl,w\;pop\\t%A3%>\;pop\\t%B3%>\;pop\\t%C3%>\;pop\\t%D3%>"
)

(define_peephole2
  [(set (match_operand:SI 0 "ip2k_nonsp_reg_operand" "")
        (match_operand:SI 1 "ip2k_short_operand" ""))
   (set (reg:HI 12)
        (match_operand:HI 2 "general_operand" ""))
   (set (match_operand:SI 3 "ip2k_short_operand" "")
        (match_dup 0))]
  "(ip2k_reorg_split_dimode
    && peep2_reg_dead_p (3, operands[0])
    && ip2k_address_uses_reg_p (operands[1], REG_DP)
    && ip2k_address_uses_reg_p (operands[3], REG_DP)
    && ! (ip2k_address_uses_reg_p (operands[2], REG_SP)
          && (GET_CODE (XEXP (operands[2], 0)) == PLUS)
          && (INTVAL (XEXP (XEXP (operands[2], 0), 1)) >= 123)))"
  [(parallel [(set (match_dup 0)
                   (match_dup 1))
	      (set (reg:HI 12)
 	           (match_dup 2))
	      (set (match_dup 3)
 	           (match_dup 0))])]
  "")

(define_insn "*pushdi_reload_popdi"
  [(set (match_operand:DI 0 "ip2k_nonsp_reg_operand" "=u, u")
        (match_operand:DI 1 "ip2k_short_operand"      "S, S"))
   (set (reg:HI 12)
        (match_operand:HI 2 "general_operand"         "i,ro"))
   (set (match_operand:DI 3 "ip2k_short_operand"     "=S, S")
        (match_dup 0))]
  ""
  "@
   push\\t%S1%<\;push\\t%T1%<\;push\\t%U1%<\;push\\t%V1%<\;push\\t%W1%<\;push\\t%X1%<\;push\\t%Y1%<\;push\\t%Z1%<\;loadh\\t%x2\;loadl\\t%x2\;pop\\t%Z3%>\;pop\\t%Y3%>\;pop\\t%X3%>\;pop\\t%W3%>\;pop\\t%V3%>\;pop\\t%U3%>\;pop\\t%T3%>\;pop\\t%S3%>
   push\\t%S1%<\;push\\t%T1%<\;push\\t%U1%<\;push\\t%V1%<\;push\\t%W1%<\;push\\t%X1%<\;push\\t%Y1%<\;push\\t%Z1%<\;mov\\tw,%L2\;push\\t%H2\;pop\\tdph\;mov\\tdpl,w\;pop\\t%Z3%>\;pop\\t%Y3%>\;pop\\t%X3%>\;pop\\t%W3%>\;pop\\t%V3%>\;pop\\t%U3%>\;pop\\t%T3%>\;pop\\t%S3%>"
)

(define_peephole2
  [(set (match_operand:DI 0 "ip2k_nonsp_reg_operand" "")
        (match_operand:DI 1 "ip2k_short_operand" ""))
   (set (reg:HI 12)
        (match_operand:HI 2 "general_operand" ""))
   (set (match_operand:DI 3 "ip2k_short_operand" "")
        (match_dup 0))]
  "((ip2k_reorg_in_progress || ip2k_reorg_completed)
    && peep2_reg_dead_p (3, operands[0])
    && ip2k_address_uses_reg_p (operands[1], REG_DP)
    && ip2k_address_uses_reg_p (operands[3], REG_DP)
    && ! (ip2k_address_uses_reg_p (operands[2], REG_SP)
          && (GET_CODE (XEXP (operands[2], 0)) == PLUS)
          && (INTVAL (XEXP (XEXP (operands[2], 0), 1)) >= 119)))"
  [(parallel [(set (match_dup 0)
                   (match_dup 1))
	      (set (reg:HI 12)
 	           (match_dup 2))
	      (set (match_dup 3)
 	           (match_dup 0))])]
  "")

;; FIXME: Disabled because in lshiftrt:SI op1 must match op0
(define_peephole2
  [(set (match_operand 0 "ip2k_nonsp_reg_operand" "")
        (match_operator 3 "ip2k_binary_operator"
			[(match_operand 1 "general_operand" "")
			 (match_operand 2 "general_operand" "")]))
   (set (match_operand 4 "nonimmediate_operand" "")
        (match_dup 0))]
  "0 && (peep2_reg_dead_p (2, operands[0])
         && ip2k_xexp_not_uses_reg_p (operands[4], REGNO (operands[0]),
                                      GET_MODE_SIZE (GET_MODE (operands[0]))))"
  [(set (match_dup 4)
        (match_op_dup 3 [(match_dup 1)
			 (match_dup 2)]))]
  "")

(define_peephole2
  [(set (match_operand 0 "ip2k_nonsp_reg_operand" "")
        (match_operator 3 "ip2k_binary_operator"
			[(zero_extend:HI
			  (match_operand 1 "general_operand" ""))
			 (match_operand 2 "general_operand" "")]))
   (set (match_operand 4 "nonimmediate_operand" "")
        (match_dup 0))]
  "(peep2_reg_dead_p (2, operands[0])
    && ip2k_xexp_not_uses_reg_p (operands[4], REGNO (operands[0]),
                                 GET_MODE_SIZE (GET_MODE (operands[0]))))"
  [(set (match_dup 4)
        (match_op_dup 3 [(zero_extend:HI (match_dup 1))
			 (match_dup 2)]))]
  "")

(define_peephole2
  [(set (match_operand 0 "ip2k_nonsp_reg_operand" "")
        (match_operator 3 "ip2k_binary_operator"
			[(match_operand 1 "general_operand" "")
			 (zero_extend:HI
			  (match_operand 2 "general_operand" ""))]))
   (set (match_operand 4 "nonimmediate_operand" "")
        (match_dup 0))]
  "(peep2_reg_dead_p (2, operands[0])
    && ip2k_xexp_not_uses_reg_p (operands[4], REGNO (operands[0]),
                                 GET_MODE_SIZE (GET_MODE (operands[0]))))"
  [(set (match_dup 4)
        (match_op_dup 3 [(match_dup 1)
			 (zero_extend:HI (match_dup 2))]))]
  "")

(define_peephole2
  [(set (match_operand 0 "ip2k_nonsp_reg_operand" "")
        (match_operator 3 "ip2k_binary_operator"
			[(zero_extend:SI
			  (match_operand 1 "general_operand" ""))
			 (match_operand 2 "general_operand" "")]))
   (set (match_operand 4 "nonimmediate_operand" "")
        (match_dup 0))]
  "(peep2_reg_dead_p (2, operands[0])
    && ip2k_xexp_not_uses_reg_p (operands[4], REGNO (operands[0]),
                                 GET_MODE_SIZE (GET_MODE (operands[0]))))"
  [(set (match_dup 4)
        (match_op_dup 3 [(zero_extend:SI (match_dup 1))
			 (match_dup 2)]))]
  "")

(define_peephole2
  [(set (match_operand 0 "ip2k_nonsp_reg_operand" "")
        (match_operator 3 "ip2k_binary_operator"
			[(match_operand 1 "general_operand" "")
			 (zero_extend:SI
			  (match_operand 2 "general_operand" ""))]))
   (set (match_operand 4 "nonimmediate_operand" "")
        (match_dup 0))]
  "(peep2_reg_dead_p (2, operands[0])
    && ip2k_xexp_not_uses_reg_p (operands[4], REGNO (operands[0]),
                                 GET_MODE_SIZE (GET_MODE (operands[0]))))"
  [(set (match_dup 4)
        (match_op_dup 3 [(match_dup 1)
			 (zero_extend:SI (match_dup 2))]))]
  "")

(define_peephole2
  [(set (match_operand 0 "ip2k_nonsp_reg_operand" "")
        (match_operand 1 "nonimmediate_operand" ""))
   (set (match_operand 2 "nonimmediate_operand" "")
        (match_operator 3 "ip2k_binary_operator"
			[(match_operand 4 "general_operand" "")
			 (match_dup 0)]))]
  "0 && ((peep2_reg_dead_p (2, operands[0])
    || rtx_equal_p (operands[0], operands[2]))
    && ip2k_xexp_not_uses_reg_p (operands[4], REGNO (operands[0]),
                                 GET_MODE_SIZE (GET_MODE (operands[0]))))"
  [(set (match_dup 2)
        (match_op_dup 3 [(match_dup 4)
			 (match_dup 1)]))]
  "")

(define_peephole2
  [(set (match_operand 0 "ip2k_nonsp_reg_operand" "")
        (match_operand 1 "nonimmediate_operand" ""))
   (set (match_operand 2 "nonimmediate_operand" "")
        (match_operator 3 "ip2k_binary_operator"
			[(zero_extend:HI
			  (match_operand 4 "general_operand" ""))
			 (match_dup 0)]))]
  "((peep2_reg_dead_p (2, operands[0])
     || rtx_equal_p (operands[0], operands[2]))
    && ip2k_xexp_not_uses_reg_p (operands[4], REGNO (operands[0]),
                                 GET_MODE_SIZE (GET_MODE (operands[0]))))"
  [(set (match_dup 2)
        (match_op_dup 3 [(zero_extend:HI (match_dup 4))
			 (match_dup 1)]))]
  "")

(define_peephole2
  [(set (match_operand 0 "ip2k_nonsp_reg_operand" "")
        (match_operand 1 "nonimmediate_operand" ""))
   (set (match_operand 2 "nonimmediate_operand" "")
        (match_operator 3 "ip2k_binary_operator"
			[(zero_extend:SI
			  (match_operand 4 "general_operand" ""))
			 (match_dup 0)]))]
  "((peep2_reg_dead_p (2, operands[0])
     || rtx_equal_p (operands[0], operands[2]))
    && ip2k_xexp_not_uses_reg_p (operands[4], REGNO (operands[0]),
                                 GET_MODE_SIZE (GET_MODE (operands[0]))))"
  [(set (match_dup 2)
        (match_op_dup 3 [(zero_extend:SI (match_dup 4))
			 (match_dup 1)]))]
  "")

(define_peephole2
  [(set (match_operand 0 "ip2k_nonsp_reg_operand" "")
        (match_operand 1 "nonimmediate_operand" ""))
   (set (match_operand 2 "nonimmediate_operand" "")
        (match_operator 3 "ip2k_binary_operator"
			[(match_dup 0)
			 (match_operand 4 "general_operand" "")]))]
  "0 && ((peep2_reg_dead_p (2, operands[0])
    || rtx_equal_p (operands[0], operands[2]))
    && ip2k_xexp_not_uses_reg_p (operands[4], REGNO (operands[0]),
                                 GET_MODE_SIZE (GET_MODE (operands[0]))))"
  [(set (match_dup 2)
        (match_op_dup 3 [(match_dup 1)
			 (match_dup 4)]))]
  "")

(define_peephole2
  [(set (match_operand 0 "ip2k_nonsp_reg_operand" "")
        (match_operand 1 "nonimmediate_operand" ""))
   (set (match_operand 2 "nonimmediate_operand" "")
        (match_operator 3 "ip2k_binary_operator"
			[(match_dup 0)
			 (zero_extend:HI
			  (match_operand 4 "general_operand" ""))]))]
  "((peep2_reg_dead_p (2, operands[0])
     || rtx_equal_p (operands[0], operands[2]))
    && ip2k_xexp_not_uses_reg_p (operands[4], REGNO (operands[0]),
                                 GET_MODE_SIZE (GET_MODE (operands[0]))))"
  [(set (match_dup 2)
        (match_op_dup 3 [(match_dup 1)
			 (zero_extend:HI (match_dup 4))]))]
  "")

(define_peephole2
  [(set (match_operand 0 "ip2k_nonsp_reg_operand" "")
        (match_operand 1 "nonimmediate_operand" ""))
   (set (match_operand 2 "nonimmediate_operand" "")
        (match_operator 3 "ip2k_binary_operator"
			[(match_dup 0)
			 (zero_extend:SI
			  (match_operand 4 "general_operand" ""))]))]
  "((peep2_reg_dead_p (2, operands[0])
     || rtx_equal_p (operands[0], operands[2]))
    && ip2k_xexp_not_uses_reg_p (operands[4], REGNO (operands[0]),
                                 GET_MODE_SIZE (GET_MODE (operands[0]))))"
  [(set (match_dup 2)
        (match_op_dup 3 [(match_dup 1)
			 (zero_extend:SI (match_dup 4))]))]
  "")

(define_peephole2
  [(set (match_operand 0 "ip2k_nonsp_reg_operand" "")
        (match_operand 1 "nonimmediate_operand" ""))
   (set (cc0)
        (match_operator 2 "ip2k_binary_operator"
			[(match_operand 3 "general_operand" "")
			 (match_dup 0)]))]
  "0 && (peep2_reg_dead_p (2, operands[0])
    && ip2k_xexp_not_uses_reg_p (operands[3], REGNO (operands[0]),
                                 GET_MODE_SIZE (GET_MODE (operands[0]))))"
  [(set (cc0)
        (match_op_dup 2 [(match_dup 3)
			 (match_dup 1)]))]
  "")

(define_peephole2
  [(set (match_operand 0 "ip2k_nonsp_reg_operand" "")
        (match_operand 1 "nonimmediate_operand" ""))
   (set (cc0)
        (match_operator 2 "ip2k_binary_operator"
			[(zero_extend:HI
			  (match_operand 3 "general_operand" ""))
			 (match_dup 0)]))]
  "(peep2_reg_dead_p (2, operands[0])
    && ip2k_xexp_not_uses_reg_p (operands[3], REGNO (operands[0]),
                                 GET_MODE_SIZE (GET_MODE (operands[0]))))"
  [(set (cc0)
        (match_op_dup 2 [(zero_extend:HI (match_dup 3))
			 (match_dup 1)]))]
  "")

(define_peephole2
  [(set (match_operand 0 "ip2k_nonsp_reg_operand" "")
        (match_operand 1 "nonimmediate_operand" ""))
   (set (cc0)
        (match_operator 2 "ip2k_binary_operator"
			[(zero_extend:SI
			  (match_operand 3 "general_operand" ""))
			 (match_dup 0)]))]
  "(peep2_reg_dead_p (2, operands[0])
    && ip2k_xexp_not_uses_reg_p (operands[3], REGNO (operands[0]),
                                 GET_MODE_SIZE (GET_MODE (operands[0]))))"
  [(set (cc0)
        (match_op_dup 2 [(zero_extend:SI (match_dup 3))
			 (match_dup 1)]))]
  "")

(define_peephole2
  [(set (match_operand 0 "ip2k_nonsp_reg_operand" "")
        (match_operand 1 "nonimmediate_operand" ""))
   (set (cc0)
        (match_operator 2 "ip2k_binary_operator"
			[(match_dup 0)
			 (match_operand 3 "general_operand" "")]))]
  "(peep2_reg_dead_p (2, operands[0])
    && ip2k_xexp_not_uses_reg_p (operands[3], REGNO (operands[0]),
                                 GET_MODE_SIZE (GET_MODE (operands[0]))))"
  [(set (cc0)
        (match_op_dup 2 [(match_dup 1)
			 (match_dup 3)]))]
  "")

(define_peephole2
  [(set (match_operand 0 "ip2k_nonsp_reg_operand" "")
        (match_operand 1 "nonimmediate_operand" ""))
   (set (cc0)
        (match_operator 2 "ip2k_binary_operator"
			[(match_dup 0)
			 (zero_extend:HI
			  (match_operand 3 "general_operand" ""))]))]
  "(peep2_reg_dead_p (2, operands[0])
    && ip2k_xexp_not_uses_reg_p (operands[3], REGNO (operands[0]),
                                 GET_MODE_SIZE (GET_MODE (operands[0]))))"
  [(set (cc0)
        (match_op_dup 2 [(match_dup 1)
			 (zero_extend:HI (match_dup 3))]))]
  "")

(define_peephole2
  [(set (match_operand 0 "ip2k_nonsp_reg_operand" "")
        (match_operand 1 "nonimmediate_operand" ""))
   (set (cc0)
        (match_operator 2 "ip2k_binary_operator"
			[(match_dup 0)
			 (zero_extend:SI
			  (match_operand 3 "general_operand" ""))]))]
  "(peep2_reg_dead_p (2, operands[0])
    && ip2k_xexp_not_uses_reg_p (operands[3], REGNO (operands[0]),
                                 GET_MODE_SIZE (GET_MODE (operands[0]))))"
  [(set (cc0)
        (match_op_dup 2 [(match_dup 1)
			 (zero_extend:SI (match_dup 3))]))]
  "")

(define_peephole2
  [(set (match_operand 0 "ip2k_nonsp_reg_operand" "")
        (match_operator 3 "ip2k_unary_operator"
			[(match_operand 1 "general_operand" "")]))
   (set (match_operand 2 "nonimmediate_operand" "")
        (match_dup 0))]
  "(peep2_reg_dead_p (2, operands[0])
    && ip2k_xexp_not_uses_reg_p (operands[2], REGNO (operands[0]),
                                 GET_MODE_SIZE (GET_MODE (operands[0]))))"
  [(set (match_dup 2)
        (match_op_dup 3 [(match_dup 1)]))]
  "")

(define_peephole2
  [(set (match_operand 0 "ip2k_nonsp_reg_operand" "")
        (match_operand 1 "nonimmediate_operand" ""))
   (set (match_operand 2 "nonimmediate_operand" "")
        (match_operator 3 "ip2k_unary_operator" [(match_dup 0)]))]
  "(peep2_reg_dead_p (2, operands[0])
    && ip2k_xexp_not_uses_reg_p (operands[2], REGNO (operands[0]),
                                 GET_MODE_SIZE (GET_MODE (operands[0]))))"
  [(set (match_dup 2)
        (match_op_dup 3 [(match_dup 1)]))]
  "")

(define_peephole2
  [(set (match_operand 0 "ip2k_nonsp_reg_operand" "")
        (match_operand 1 "nonimmediate_operand" ""))
   (set (cc0)
        (match_dup 0))]
  "peep2_reg_dead_p (2, operands[0])"
  [(set (cc0)
        (match_dup 1))]
  "")

;; Look for places where we can shorten a compare operation.
;;
(define_peephole2
  [(set (match_operand:QI 0 "nonimmediate_operand" "")
        (const_int 0))
   (set (cc0)
        (match_operand:HI 1 "nonimmediate_operand" ""))
   (set (pc)
	(if_then_else (match_operator 2 "comparison_operator"
	                [(cc0) (const_int 0)])
		      (label_ref (match_operand 3 "" ""))
		      (pc)))]
  "(rtx_equal_p (ip2k_get_high_half (operands[1], QImode), operands[0]))"
  [(set (match_dup 0)
        (const_int 0))
   (set (cc0)
        (match_dup 4))
   (set (pc)
	(if_then_else (match_op_dup 2
	                [(cc0) (const_int 0)])
		      (label_ref (match_dup 3))
		      (pc)))]
  "{
    operands[4] = ip2k_get_low_half (operands[1], QImode);
  }")

;; Look for places where we can shorten a compare operation.
;;
(define_peephole2
  [(set (match_operand:QI 0 "nonimmediate_operand" "")
        (const_int 0))
   (set (cc0)
        (compare (match_operand:HI 1 "nonimmediate_operand" "")
	         (match_operand 2 "const_int_operand" "")))
   (set (pc)
	(if_then_else (match_operator 3 "comparison_operator"
	                [(cc0) (const_int 0)])
		      (label_ref (match_operand 4 "" ""))
		      (pc)))]
  "(rtx_equal_p (ip2k_get_high_half (operands[1], QImode), operands[0])
    && (abs (INTVAL (operands[2]) <= 127)))"
  [(set (match_dup 0)
        (const_int 0))
   (set (cc0)
        (compare (match_dup 5)
	         (match_dup 6)))
   (set (pc)
	(if_then_else (match_op_dup 3
	                [(cc0) (const_int 0)])
		      (label_ref (match_dup 4))
		      (pc)))]
  "{
    operands[5] = ip2k_get_low_half (operands[1], QImode);
    operands[6] = gen_int_mode (INTVAL (operands[2]) & 0xff, QImode);
  }")

;; This is one of those cases where gcc just can't untangle our wishes.  We
;; want to add some values but get two copies of the result.  In this instance
;; however, the seconds copy can be made more cheaply by combining things.
;;
(define_peephole
  [(set (match_operand:HI 0 "ip2k_nonsp_reg_operand"       "+&u")
        (plus:HI (match_operand:HI 1 "nonimmediate_operand" "rS")
	         (match_operand:HI 2 "general_operand"     "rSi")))
   (set (match_operand:HI 3 "ip2k_gen_operand"            "=&uS")
        (match_dup 0))]
  "(ip2k_xexp_not_uses_reg_p (operands[1], REGNO (operands[0]),
                              GET_MODE_SIZE (GET_MODE (operands[0])))
    && ip2k_xexp_not_uses_reg_p (operands[2], REGNO (operands[0]),
                                 GET_MODE_SIZE (GET_MODE (operands[0])))
    && ip2k_xexp_not_uses_reg_p (operands[3], REGNO (operands[0]),
                                 GET_MODE_SIZE (GET_MODE (operands[0])))
    && (!REG_P (operands[3])
        || (ip2k_xexp_not_uses_reg_p (operands[1], REGNO (operands[3]),
                                      GET_MODE_SIZE (GET_MODE (operands[3])))
            && ip2k_xexp_not_uses_reg_p (operands[2], REGNO (operands[3]),
                                         GET_MODE_SIZE (GET_MODE (operands[3]))))))"
  "mov\\tw,%L2\;add\\tw,%L1\;mov\\t%L0,w\;mov\\t%L3,w\;mov\\tw,%H2\;addc\\tw,%H1\;mov\\t%H0,w\;mov\\t%H3,w")

(define_peephole
  [(set (match_operand:HI 0 "ip2k_short_operand"           "+&S")
        (plus:HI (match_operand:HI 1 "nonimmediate_operand" "rS")
	         (match_operand:HI 2 "general_operand"     "rSi")))
   (set (match_operand:HI 3 "ip2k_nonsp_reg_operand"       "=&u")
        (match_dup 0))]
  "(ip2k_xexp_not_uses_reg_p (operands[0], REGNO (operands[3]),
                              GET_MODE_SIZE (GET_MODE (operands[3])))
    && ip2k_xexp_not_uses_reg_p (operands[1], REGNO (operands[3]),
                                 GET_MODE_SIZE (GET_MODE (operands[3])))
    && ip2k_xexp_not_uses_reg_p (operands[2], REGNO (operands[3]),
                                 GET_MODE_SIZE (GET_MODE (operands[3])))
    && ! rtx_equal_p (operands[0], operands[1])
    && ! rtx_equal_p (operands[0], operands[2]))"
  "mov\\tw,%L2\;add\\tw,%L1\;mov\\t%L0,w\;mov\\t%L3,w\;mov\\tw,%H2\;addc\\tw,%H1\;mov\\t%H0,w\;mov\\t%H3,w")

;; Some splits zero the MSByte of a word that we then use for shifting.  We
;; can therefore replace full shifts with zero-extended ones.  These are
;; cheaper for us.
;;
(define_peephole2
  [(set (match_operand:QI 0 "register_operand" "")
  	(const_int 0))
   (set (match_operand:HI 1 "nonimmediate_operand" "")
   	(ashift:HI (match_operand:HI 2 "register_operand" "")
		   (match_operand 3 "const_int_operand" "")))]
  "(rtx_equal_p (ip2k_get_high_half (operands[2], QImode), operands[0])
    && peep2_reg_dead_p (2, operands[0]))"
  [(set (match_dup 1)
  	(ashift:HI (zero_extend:HI (match_dup 4))
		   (match_dup 3)))]
  "{
    operands[4] = ip2k_get_low_half (operands[2], QImode);
  }")

(define_peephole2
  [(set (match_operand:QI 0 "register_operand" "")
  	(const_int 0))
   (set (match_operand:HI 1 "nonimmediate_operand" "")
   	(ashift:HI (match_operand:HI 2 "register_operand" "")
		   (match_operand 3 "const_int_operand" "")))]
  "(rtx_equal_p (ip2k_get_high_half (operands[2], QImode), operands[0]))"
  [(set (match_dup 0)
  	(const_int 0))
   (set (match_dup 1)
  	(ashift:HI (zero_extend:HI (match_dup 4))
		   (match_dup 3)))]
  "{
    operands[4] = ip2k_get_low_half (operands[2], QImode);
  }")

;; Some splits zero the MSByte of a word that we then use for multiplying.  We
;; can therefore replace the full multiplies with zero-extended ones.
;; These are cheaper for us.
;;
(define_peephole2
  [(set (match_operand:QI 0 "register_operand" "")
  	(const_int 0))
   (set (match_operand:HI 1 "nonimmediate_operand" "")
   	(mult:HI (match_operand:HI 2 "register_operand" "")
		 (zero_extend:HI
		  (match_operand:QI 3 "const_int_operand" ""))))]
  "(rtx_equal_p (ip2k_get_high_half (operands[2], QImode), operands[0])
    && (peep2_reg_dead_p (2, operands[0])
        || rtx_equal_p (operands[1], operands[2])))"
  [(set (match_dup 1)
  	(mult:HI (zero_extend:HI (match_dup 4))
		 (zero_extend:HI (match_dup 3))))]
  "{
    operands[4] = ip2k_get_low_half (operands[2], QImode);
  }")

(define_peephole2
  [(set (match_operand:QI 0 "register_operand" "")
  	(const_int 0))
   (set (match_operand:HI 1 "nonimmediate_operand" "")
   	(mult:HI (match_operand:HI 2 "register_operand" "")
		 (zero_extend:HI
		  (match_operand:QI 3 "const_int_operand" ""))))]
  "(rtx_equal_p (ip2k_get_high_half (operands[2], QImode), operands[0]))"
  [(set (match_dup 0)
  	(const_int 0))
   (set (match_dup 1)
  	(mult:HI (zero_extend:HI (match_dup 4))
		 (zero_extend:HI (match_dup 3))))]
  "{
    operands[4] = ip2k_get_low_half (operands[2], QImode);
  }")

;; Merge in a redundant move before a zero-extended multiply.
;;
(define_peephole2
  [(set (match_operand:QI 0 "register_operand" "")
  	(match_operand:QI 1 "general_operand" ""))
   (set (match_operand:HI 2 "nonimmediate_operand" "")
   	(mult:HI (zero_extend:HI (match_dup 0))
		 (zero_extend:HI
		  (match_operand:QI 3 "const_int_operand" ""))))]
  "(peep2_reg_dead_p (2, operands[0])
    || rtx_equal_p (ip2k_get_high_half (operands[2], QImode), operands[0])
    || rtx_equal_p (ip2k_get_low_half (operands[2], QImode), operands[0]))"
  [(set (match_dup 2)
  	(mult:HI (zero_extend:HI (match_dup 1))
		 (zero_extend:HI (match_dup 3))))]
  "")

;; Pick up redundant clears followed by adds - these can just become moves.
;;
(define_peephole2
  [(set (match_operand 0 "register_operand" "")
        (const_int 0))
   (set (match_operand 2 "nonimmediate_operand" "")
        (plus (match_dup 0)
	      (match_operand 1 "general_operand" "")))]
  "peep2_reg_dead_p (2, operands[0])"
  [(set (match_dup 2)
  	(match_dup 1))]
  "")

(define_peephole2
  [(set (match_operand 0 "register_operand" "")
        (const_int 0))
   (set (match_dup 0)
        (plus (match_dup 0)
	      (match_operand 1 "general_operand" "")))]
  ""
  [(set (match_dup 0)
  	(match_dup 1))]
  "")

;; Clear up an add followed by a push of the result.  The fact that this 
;; isn't picked up consistently within the combiner suggests a bug somewhere.
;;
(define_peephole2
  [(set (match_operand:HI 0 "register_operand" "")
  	(plus:HI (match_operand:HI 1 "nonimmediate_operand" "")
	         (match_operand:HI 2 "general_operand" "")))
   (set (mem:HI (post_dec:HI (reg:HI 6)))
        (match_dup 0))]
  "peep2_reg_dead_p (2, operands[0])"
  [(set (mem:HI (post_dec:HI (reg:HI 6)))
	(plus:HI (match_dup 1)
		 (match_dup 2)))]
  "")

;; Tidy up stack slot addressing where we've eliminated some registers.
;;   This looks like something strange going on though as gcc-2.97 didn't
;; exhibit this behavior, whereas gcc-3.0.4 does.
;;
(define_peephole2
  [(set (match_operand:HI 0 "register_operand" "")
  	(plus:HI (match_operand:HI 1 "nonimmediate_operand" "")
	         (match_operand 2 "const_int_operand" "")))
   (set (mem:HI (post_dec:HI (reg:HI 6)))
        (plus:HI (match_dup 0)
		 (match_operand 3 "const_int_operand" "")))]
  "peep2_reg_dead_p (2, operands[0])"
  [(set (mem:HI (post_dec:HI (reg:HI 6)))
	(plus:HI (match_dup 1)
		 (match_dup 4)))]
  "{
    operands[4] = gen_int_mode (INTVAL (operands[2]) + INTVAL (operands[3]),
                                HImode);
  }")

;; Match duplicate loads of a symbol ref.  This isn't something that we want to
;; do at the peephole2 stage because more often than not we'll make one of the
;; two loads redundant after we run peephole2.  We catch the remaining cases
;; here though
;;
(define_peephole
  [(set (match_operand:HI 0 "nonimmediate_operand" "+uS")
        (match_operand 1 "ip2k_symbol_ref_operand"   "i"))
   (set (match_operand:HI 2 "nonimmediate_operand" "=uS")
   	(match_dup 1))]
  "((!REG_P (operands[0]) || (REGNO (operands[0]) != REG_DP))
    && (!REG_P (operands[2]) || (REGNO (operands[2]) != REG_DP)))"
  "mov\\tw,%L1\;mov\\t%L0,w\;mov\\t%L2,w\;mov\\tw,%H1\;mov\\t%H0,w\;mov\\t%H2,w")

(define_peephole
  [(set (match_operand:HI 0 "nonimmediate_operand" "+&uS")
        (match_operand 1 "ip2k_symbol_ref_operand"   "i"))
   (set (match_operand:HI 2 "nonimmediate_operand" "=&uS")
   	(match_dup 0))]
  ""
  "*{
    if ((REG_P (operands[0])
         && !(ip2k_xexp_not_uses_reg_p (operands[1], REGNO (operands[0]), 2)
	      && ip2k_xexp_not_uses_reg_p (operands[2],
                                           REGNO (operands[0]), 2)))
        || (REG_P (operands[2])
            && !(ip2k_xexp_not_uses_reg_p (operands[0], REGNO (operands[2]), 2)
	         && ip2k_xexp_not_uses_reg_p (operands[1],
                                              REGNO (operands[2]), 2))))
      {
        return AS2 (mov, w, %L1) CR_TAB
	       AS1 (push, %H1%<) CR_TAB
	       AS1 (push, %H1%<) CR_TAB
	       AS1 (pop, %H0%>) CR_TAB
	       AS2 (mov, %L0, w) CR_TAB
	       AS1 (pop, %H2%>) CR_TAB
	       AS2 (mov, %L2, w);
      }
    else
      {
        return AS2 (mov, w, %L1) CR_TAB
	       AS2 (mov, %L0, w) CR_TAB
	       AS2 (mov, %L2, w) CR_TAB
	       AS2 (mov, w, %H1) CR_TAB
	       AS2 (mov, %H0, w) CR_TAB
	       AS2 (mov, %H2, w);
      }
  }")

;; Handle the common array indexing pattern.
;; This is of the form A = X + (Y * C).
;; We use splits earlier in this file to get our interesting cases into the
;; same form (i.e. zero-extended multiply and add).
;;
(define_insn "*mulacchi"
  [(set (match_operand:HI 3 "nonimmediate_operand"                   "=rS")
        (plus:HI (mult:HI (zero_extend:HI
			   (match_operand:QI 1 "nonimmediate_operand" "rS"))
			  (zero_extend:HI
			   (match_operand:QI 2 "const_int_operand"     "n")))
		 (match_operand:HI 0 "general_operand"                "rSi")))]
  ""
  "*{
    if (immediate_operand (operands[0], HImode)
        && REG_P (operands[3])
	&& (REGNO (operands[3]) == REG_DP)
	&& (INTVAL (operands[2]) == 2))
      return AS2 (mov, w, %1) CR_TAB
      	     AS1 (loadl, %x0) CR_TAB
	     AS1 (loadh, %x0) CR_TAB
	     AS2 (add, dpl, w) CR_TAB
	     AS2 (add, dpl, w);
    else
      return AS2 (mov, w, %1) CR_TAB
             AS2 (mulu, w, %2) CR_TAB
             AS2 (add, w, %L0) CR_TAB
	     AS2 (mov, %L3, w) CR_TAB
	     AS2 (mov, w, %H0) CR_TAB
	     AS2 (addc, w, MULH) CR_TAB
	     AS2 (mov, %H3, w);
  }")

(define_peephole2
  [(set (match_operand:HI 0 "register_operand" "")
	(mult:HI (zero_extend:HI
		   (match_operand:QI 1 "nonimmediate_operand" ""))
		 (zero_extend:HI
		   (match_operand 2 "const_int_operand" ""))))
   (set (match_operand:HI 3 "nonimmediate_operand" "")
        (plus:HI (match_dup 0)
		 (match_operand:HI 4 "general_operand" "")))]
  "(((! REG_P (operands[3]))
     || (ip2k_xexp_not_uses_reg_p (operands[4], REGNO (operands[3]),
                                   GET_MODE_SIZE (GET_MODE (operands[3])))
         && ip2k_xexp_not_uses_reg_p (operands[0], REGNO (operands[3]),
                                      GET_MODE_SIZE (GET_MODE (operands[3])))))
    && peep2_reg_dead_p (2, operands[0]))"
  [(set (match_dup 3)
	(plus:HI (mult:HI (zero_extend:HI
		   	    (match_dup 1))
		   	  (zero_extend:HI
			    (match_dup 2)))
		 (match_dup 4)))]
  "")

(define_insn "*mulhi_and_accumulate"
  [(set (match_operand:HI 0 "nonimmediate_operand"           "=rS")
	(mult:HI (zero_extend:HI
		   (match_operand:QI 1 "nonimmediate_operand" "rS"))
		 (zero_extend:HI
		   (match_operand:QI 2 "const_int_operand"     "n"))))
   (set (match_operand:HI 3 "nonimmediate_operand"           "=rS")
        (plus:HI (match_dup 0)
		 (match_operand:HI 4 "general_operand"      "%rSi")))]
  "((! REG_P (operands[3]))
    || (ip2k_xexp_not_uses_reg_p (operands[4], REGNO (operands[3]),
                                  GET_MODE_SIZE (GET_MODE (operands[3])))
        && ip2k_xexp_not_uses_reg_p (operands[0], REGNO (operands[3]),
                                     GET_MODE_SIZE (GET_MODE (operands[3])))))"
  "*{
    return AS2 (mov, w, %1) CR_TAB
	   AS2 (mulu, w, %2) CR_TAB
	   AS2 (mov, %L0, w) CR_TAB
	   AS2 (add, w, %L4) CR_TAB
	   AS2 (mov, %L3, w) CR_TAB
	   AS2 (mov, w, %H4) CR_TAB
	   AS2 (addc, w, MULH) CR_TAB
	   AS2 (mov, %H3, w) CR_TAB
	   AS2 (mov, w, MULH) CR_TAB
	   AS2 (mov, %H0, w);
  }")

(define_peephole2
  [(set (match_operand:HI 0 "nonimmediate_operand" "")
	(mult:HI (zero_extend:HI
		   (match_operand:QI 1 "nonimmediate_operand" ""))
		 (zero_extend:HI
		   (match_operand 2 "const_int_operand" ""))))
   (set (match_operand:HI 3 "nonimmediate_operand" "")
        (plus:HI (match_dup 0)
		 (match_operand:HI 4 "general_operand" "")))]
  "((! REG_P (operands[3]))
    || (ip2k_xexp_not_uses_reg_p (operands[4], REGNO (operands[3]),
                                  GET_MODE_SIZE (GET_MODE (operands[3])))
        && ip2k_xexp_not_uses_reg_p (operands[0], REGNO (operands[3]),
                                     GET_MODE_SIZE (GET_MODE (operands[3])))))"
  [(parallel [(set (match_dup 0)
  		   (mult:HI (zero_extend:HI
		   	      (match_dup 1))
		   	    (zero_extend:HI
			      (match_dup 2))))
  	      (set (match_dup 3)
	      	   (plus:HI (match_dup 0)
		   	    (match_dup 4)))])]
  "")

;; Handle the common array indexing pattern.
;; This is of the form A = X + (Y * C).
;; We use splits earlier in this file to get our interesting cases into the 
;; same form (i.e. multiply and add).
;;
(define_peephole
  [(set (match_operand:HI 0 "register_operand"              "=r")
	(mult:HI (match_operand:HI 1 "nonimmediate_operand" "rS")
		 (zero_extend:HI
		   (match_operand:QI 2 "const_int_operand"   "n"))))
   (set (match_operand:HI 3 "nonimmediate_operand"         "=rS")
        (plus:HI (match_dup 0)
		 (match_operand:HI 4 "general_operand"    "%rSi")))]
  "((!REG_P (operands[3])
     || (ip2k_xexp_not_uses_reg_p (operands[4], REGNO (operands[3]),
                                   GET_MODE_SIZE (GET_MODE (operands[3])))))
    && find_regno_note (insn, REG_DEAD, REGNO (operands[0])))"
  "*{
    if (immediate_operand (operands[4], HImode)
        && REG_P (operands[3])
	&& (REGNO (operands[3]) == REG_DP)
	&& (INTVAL (operands[2]) == 2)
	&& ip2k_xexp_not_uses_reg_p (operands[1], REG_DP,
                                     GET_MODE_SIZE (HImode)))
      return AS2 (clrb, STATUS, 0) CR_TAB
	     AS1 (loadl, %x4) CR_TAB
	     AS1 (loadh, %x4) CR_TAB
	     AS2 (rl, w, %L1) CR_TAB
	     AS2 (add, dpl, w) CR_TAB
	     AS2 (rl, w, %H1) CR_TAB
	     AS2 (add, dph, w);
    else if (!REG_P (operands[3])
             || (ip2k_xexp_not_uses_reg_p (operands[1], REGNO (operands[3]),
                                           GET_MODE_SIZE (GET_MODE (operands[3])))
                 && ip2k_xexp_not_uses_reg_p (operands[2], REGNO (operands[3]),
		 			       GET_MODE_SIZE (GET_MODE (operands[3])))))
      return AS2 (mov, w, %L1) CR_TAB
      	     AS2 (mulu, w, %2) CR_TAB
	     AS2 (add, w, %L4) CR_TAB
	     AS2 (mov, %L3, w) CR_TAB
	     AS2 (mov, w, %H4) CR_TAB
	     AS2 (addc, w, MULH) CR_TAB
	     AS2 (mov, %H3, w) CR_TAB
	     AS2 (mov, w, %H1) CR_TAB
	     AS2 (mulu, w, %2) CR_TAB
	     AS2 (add, %H3, w);
    else
      return AS2 (mov, w, %L1) CR_TAB
      	     AS2 (mulu, w, %2) CR_TAB
	     AS2 (add, w, %L4) CR_TAB
	     AS1 (push, wreg%<) CR_TAB
	     AS2 (mov, w, %H4) CR_TAB
	     AS2 (addc, w, MULH) CR_TAB
	     AS1 (push, wreg%<) CR_TAB
	     AS2 (mov, w, %H1) CR_TAB
	     AS2 (mulu, w, %2) CR_TAB
	     AS1 (pop, %H3%>) CR_TAB
	     AS1 (pop, %L3%>) CR_TAB
	     AS2 (add, %H3, w);
  }")

;; Handle the more complex variant of the preceding multiply and accumulate
;; variant of the preceding multiply-and-add operation.  This one would 
;; otherwise fail to match because the result never goes dead.
;;
(define_peephole
  [(set (match_operand:HI 0 "nonimmediate_operand"         "=rS")
	(mult:HI (match_operand:HI 1 "nonimmediate_operand" "rS")
		 (zero_extend:HI
		   (match_operand:QI 2 "const_int_operand"   "n"))))
   (set (match_dup 0)
        (plus:HI (match_dup 0)
		 (match_operand:HI 3 "general_operand"    "%rSi")))]
  "(!REG_P (operands[0])
    || (ip2k_xexp_not_uses_reg_p (operands[1], REGNO (operands[0]),
                                  GET_MODE_SIZE (GET_MODE (operands[0])))
        && ip2k_xexp_not_uses_reg_p (operands[2], REGNO (operands[0]),
                                     GET_MODE_SIZE (GET_MODE (operands[0])))
        && ip2k_xexp_not_uses_reg_p (operands[3], REGNO (operands[0]),
                                     GET_MODE_SIZE (GET_MODE (operands[0])))))"
  "*{
    if (immediate_operand (operands[3], HImode)
        && REG_P (operands[0])
        && (REGNO (operands[0]) == REG_DP)
	&& (INTVAL (operands[2]) == 2))
      return AS2 (clrb, STATUS, 0) CR_TAB
	     AS1 (loadl, %x3) CR_TAB
	     AS1 (loadh, %x3) CR_TAB
	     AS2 (rl, w, %L1) CR_TAB
	     AS2 (add, dpl, w) CR_TAB
	     AS2 (rl, w, %H1) CR_TAB
	     AS2 (add, dph, w);
    else
      return AS2 (mov, w, %L1) CR_TAB
      	     AS2 (mulu, w, %2) CR_TAB
	     AS2 (add, w, %L3) CR_TAB
	     AS2 (mov, %L0, w) CR_TAB
	     AS2 (mov, w, %H3) CR_TAB
	     AS2 (addc, w, MULH) CR_TAB
	     AS2 (mov, %H0, w) CR_TAB
	     AS2 (mov, w, %H1) CR_TAB
	     AS2 (mulu, w, %2) CR_TAB
	     AS2 (add, %H0, w);
  }")

;; Handle the a complex variant of the preceding multiply and add
;; operations where the intermediate result is also required.
;;
(define_peephole
  [(set (match_operand:HI 0 "nonimmediate_operand"         "=rS")
	(mult:HI (match_operand:HI 1 "nonimmediate_operand" "rS")
		 (zero_extend:HI
		   (match_operand:QI 2 "const_int_operand"   "n"))))
   (set (match_operand:HI 3 "nonimmediate_operand"         "=rS")
        (plus:HI (match_dup 0)
		 (match_operand:HI 4 "general_operand"    "%rSi")))]
  "((!REG_P (operands[3])
     || (ip2k_xexp_not_uses_reg_p (operands[4], REGNO (operands[3]),
                                   GET_MODE_SIZE (GET_MODE (operands[3])))
         && ip2k_xexp_not_uses_reg_p (operands[0], REGNO (operands[3]),
                                      GET_MODE_SIZE (GET_MODE (operands[3])))
         && ip2k_xexp_not_uses_reg_p (operands[1], REGNO (operands[3]),
                                      GET_MODE_SIZE (GET_MODE (operands[3])))
         && ip2k_xexp_not_uses_reg_p (operands[2], REGNO (operands[3]),
                                      GET_MODE_SIZE (GET_MODE (operands[3])))))
    && (INTVAL (operands[2]) != 2))"
  "* return AS2 (mov, w, %H4) CR_TAB
  	    AS2 (mov, %H3, w) CR_TAB
	    AS2 (mov, w, %L1) CR_TAB
      	    AS2 (mulu, w, %2) CR_TAB
	    AS2 (mov, %L0, w) CR_TAB
	    AS2 (add, w, %L4) CR_TAB
	    AS2 (mov, %L3, w) CR_TAB
	    AS2 (mov, w, MULH) CR_TAB
	    AS2 (mov, %H0, w) CR_TAB
	    AS2 (addc, %H3, w) CR_TAB
	    AS2 (mov, w, %H1) CR_TAB
	    AS2 (mulu, w, %2) CR_TAB
	    AS2 (add, %H3, w) CR_TAB
	    AS2 (add, %H0, w);")

;; Byte swapping!
;;
(define_peephole
  [(set (reg:QI 10)
  	(match_operand:QI 1 "nonimmediate_operand" "rS"))
   (set (match_operand:QI 0 "register_operand"     "=r")
  	(reg:QI 10))
   (set (reg:QI 10)
   	(match_operand:QI 2 "nonimmediate_operand" "rS"))
   (set (match_dup 1)
   	(reg:QI 10))
   (set (reg:QI 10)
   	(match_dup 0))
   (set (match_dup 2)
   	(reg:QI 10))]
  "find_regno_note (PREV_INSN (insn), REG_DEAD, REGNO (operands[0]))"
  "push\\t%1%<\;push\\t%2%<\;pop\\t%1%>\;pop\\t%2%>")


