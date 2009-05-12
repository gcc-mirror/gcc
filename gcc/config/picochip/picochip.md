;; GCC machine description for picochip
;; Copyright (C) 2008, 2009 Free Software Foundation, Inc.
;; Contributed by picoChip Designs Ltd (http://www.picochip.com)
;; Maintained by Daniel Towner (dant@picochip.com) and Hariharan
;; Sandanagobalane (hariharan@picochip.com)
;;
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
;; along with GCC; see the file COPYING3.  If not, see
;; <http://www.gnu.org/licenses/>.

;; -------------------------------------------------------------------------

;; In addition to the normal output operand formats, the following
;; letter formats are also available:
;;
;;  The following can be used for constants, or the constant part of a
;;  memory offset.
;;   Q - Output constant unaltered (byte mode).
;;   M - Alias for Q, which only works with memory operands.
;;   H - Divide constant by 2 (i.e., HImode is 2 bytes)
;;   S - Divide constant by 4 (i.e., SImode is 4 bytes)
;;
;;  The following can be used for two part addresses (i.e., base +
;;  offset or base[offset]).
;;   o - Output offset only.
;;   b - Output base only.
;;
;;  The following are used on SI registers and constants
;;   R - Output register pair (i.e., R[n:m])
;;   L - Output lower word/register
;;   U - Output upper word/register
;;
;;  The following are used on DI mode registers.
;;   X - Output 3rd register
;;   Y - Output 4th register
;;
;;  Miscellaneous
;;   | - Output VLIW separator
;;   r - Output register value of memory operand.
;;   I - Output an opcode (e.g., ADD for plus, LSL for lshift)
;;   i - Output an opcode in symbolic notation (e.g., + for plus)

;; Define the length of an instruction.  Used to allow different types
;; of branches to be used for different branch offsets.  Default to 6
;; bytes, which is the longest possible single instruction.
(define_attr "length" "" (const_int 6))

;; Define some constants which are used in conjuction with branch
;; scheduling.  Branches must be 10-bit signed, which equates to
;; [-512,511]. However, to compensate for the lack of branch alignment
;; these offsets are reduced by a factor of 2.

(define_constants
  [
   (MIN_BRANCH_OFFSET -256)
   (MAX_BRANCH_OFFSET 255)
   (SHORT_BRANCH_LENGTH 6)    ; The size of a schedulable short branch.
   (LONG_BRANCH_LENGTH 16)    ; The size of an expanded JMP?? macro.
   ]
)

;; Define identifiers for various special instructions.  These
;; instructions may then be used in RTL expansions, or builtins.
(define_constants
  [
   ; Special instruction builtins.
   (UNSPEC_SBC             0) ; Sign-bit count
   (UNSPEC_ADDS            1) ; Saturating addition
   (UNSPEC_SUBS            2) ; Saturating subtraction
   (UNSPEC_BREV            3) ; Bit reversal

   ; Special internal instructions (only used by compiler)
   (UNSPEC_COPYSW          5) ; Get status word
   (UNSPEC_ADDC            6) ; Add with carry.

   ; Scalar port communication builtins
   (UNSPEC_PUT             7) ; Communication (put):       port[op0] := op1
   (UNSPEC_GET             8) ; Communication (get):       op0 := get_port[op1]
   (UNSPEC_TESTPORT        9) ; Communication (test):      op0 := testport[op1]

   ; Array port communication builtins.  These all take extra
   ; arguments giving information about the array access being used.
   (UNSPEC_PUT_ARRAY      10) ; Array put
   (UNSPEC_GET_ARRAY      11) ; Array get
   (UNSPEC_TESTPORT_ARRAY 12) ; Array test port

   ;; Array port expansions
   (UNSPEC_CALL_GET_ARRAY 13) ;
   (UNSPEC_CALL_PUT_ARRAY 14) ;
   (UNSPEC_CALL_TESTPORT_ARRAY 15) ;

   ; Array port low-level fn calls
   (UNSPEC_CALL_GET_FN  16)
   (UNSPEC_CALL_TESTPORT_FN  17)

   ; Halt instruction.
   (UNSPEC_HALT 18)

   ; Internal TSTPORT instruction, used to generate a single TSTPORT
   ; instruction for use in the testport branch split.
   (UNSPEC_INTERNAL_TESTPORT        19)
  ]
)

;; Register ID's
(define_constants
  [
   (LINK_REGNUM           12) ; Function link register.
   (CC_REGNUM             17) ; Condition flags.
   (ACC_REGNUM             16) ; Condition flags.
   ]
)

;;============================================================================
;; Predicates and constraints
;;============================================================================

(include "predicates.md")
(include "constraints.md")

;;============================================================================
;; First operand shifting patterns.  These allow certain instructions
;; (e.g., add, and, or, xor, sub) to apply a shift-by-constant to
;; their first operand.
;;
;; Note that only the first operand is matched by the shift, to ensure
;; that non-commutative instructions (like subtract) work
;; properly.  When a commutative instruction, with a shift in the
;; second operand is found, the compiler will reorder the operands to
;; match.
;;============================================================================

(define_insn "*firstOpGenericAshift"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(match_operator:HI 1 "picochip_first_op_shift_operator"
			[(ashift:HI
			  (match_operand:HI 2 "register_operand" "r")
			  (match_operand:HI 3 "picochip_J_operand" "J"))
			 (match_operand:HI 4 "picochip_register_or_immediate_operand" "ri")]))
   (clobber (reg:CC CC_REGNUM))]
  ""
  "%I1.0 [LSL %2,%3],%4,%0\t// %0 := (%2 << %3) %i1 %4"
  [(set_attr "type" "picoAlu")
   ;; A long constant must be used if the operator instruction doesn't
   ;; accept immediates, or if the constant is too big to fit the
   ;; immediate. Note that the following condition is written in the
   ;; way which uses the least number of predicates.
   (set (attr "longConstant")
     (cond [(ior (match_operand 4 "register_operand")
                 (and (match_operand 1 "picochip_first_op_shift_operator_imm")
		      (match_operand 1 "picochip_J_operand")))
              (const_string "false")]
              (const_string "true")))])

;; During combine, ashift gets converted into a multiply, necessitating the following pattern.
;; Note that we do a log_2(imm) to get the actual LSL operand.

(define_insn "*firstOpGenericAshift"
  [(set (match_operand:HI 0 "register_operand" "=r")
        (match_operator:HI 1 "picochip_first_op_shift_operator"
                        [(mult:HI
                          (match_operand:HI 2 "register_operand" "r")
                          (match_operand:HI 3 "power_of_2_imm_operand" "n"))
                         (match_operand:HI 4 "picochip_register_or_immediate_operand" "ri")]))
   (clobber (reg:CC CC_REGNUM))]
  ""
  "%I1.0 [LSL %2,%P3],%4,%0\t// %0 := (%2 << %3) %i1 %4"
  [(set_attr "type" "picoAlu")
   ;; A long constant must be used if the operator instruction doesn't
   ;; accept immediates, or if the constant is too big to fit the
   ;; immediate. Note that the following condition is written in the
   ;; way which uses the least number of predicates.
   (set (attr "longConstant")
     (cond [(ior (match_operand 4 "register_operand")
                 (and (match_operand 1 "picochip_first_op_shift_operator_imm")
                      (match_operand 1 "picochip_J_operand")))
              (const_string "false")]
              (const_string "true")))])

(define_insn "*firstOpGenericAshiftrt"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(match_operator:HI 1 "picochip_first_op_shift_operator"
			[(ashiftrt:HI
			  (match_operand:HI 2 "register_operand" "r")
			  (match_operand:HI 3 "picochip_J_operand" "J"))
			 (match_operand:HI 4 "picochip_register_or_immediate_operand" "ri")]))
   (clobber (reg:CC CC_REGNUM))]
  ""
  "%I1.0 [ASR %2,%3],%4,%0\t// %0 := (%2 >>{arith} %3) %i1 %4"
  [(set_attr "type" "picoAlu")
   ;; A long constant must be used if the operator instruction doesn't
   ;; accept immediates, or if the constant is too big to fit the
   ;; immediate. Note that the following condition is written in the
   ;; way which uses the least number of predicates.
   (set (attr "longConstant")
     (cond [(ior (match_operand 4 "register_operand")
                 (and (match_operand 1 "picochip_first_op_shift_operator_imm")
		      (match_operand 1 "picochip_J_operand")))
              (const_string "false")]
              (const_string "true")))])

(define_insn "*firstOpGenericLshiftrt"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(match_operator:HI 1 "picochip_first_op_shift_operator"
			[(lshiftrt:HI
			  (match_operand:HI 2 "register_operand" "r")
			  (match_operand:HI 3 "picochip_J_operand" "J"))
			 (match_operand:HI 4 "picochip_register_or_immediate_operand" "ri")]))
   (clobber (reg:CC CC_REGNUM))]
  ""
  "%I1.0 [LSR %2,%3],%4,%0\t// %0 := (%2 >> %3) %i1 %4"
  [(set_attr "type" "picoAlu")
   ;; A long constant must be used if the operator instruction doesn't
   ;; accept immediates, or if the constant is too big to fit the
   ;; immediate. Note that the following condition is written in the
   ;; way which uses the least number of predicates.
   (set (attr "longConstant")
     (cond [(ior (match_operand 4 "register_operand")
                 (and (match_operand 1 "picochip_first_op_shift_operator_imm")
		      (match_operand 1 "picochip_J_operand")))
              (const_string "false")]
              (const_string "true")))])

;;===========================================================================
;; Jump instructions.
;;===========================================================================

(define_insn "indirect_jump"
  [(set (pc) (match_operand:HI 0 "register_operand" "r"))]
  ""
  "JR (%0)\t// Indirect_jump to %0 %>"
  [(set_attr "type" "realBranch")
   (set_attr "length" "3")])

(define_insn "jump"
  [(set (pc)
        (label_ref (match_operand 0 "" "")))]
  ""
  "* return picochip_output_jump(insn);"
  [(set (attr "length")
	(if_then_else
	 (and (ge (minus (match_dup 0) (pc)) (const_int MIN_BRANCH_OFFSET))
	      (le (minus (match_dup 0) (pc)) (const_int MAX_BRANCH_OFFSET)))
	 (const_int SHORT_BRANCH_LENGTH)
	 (const_int LONG_BRANCH_LENGTH)))
   (set (attr "type")
	(if_then_else
	 (eq_attr "length" "6")
	 (const_string "realBranch")
	 (const_string "unknown")))])

(define_insn "*fn_return"
  [(return)
   (use (reg:HI LINK_REGNUM))]
  ""
  "JR (R12)\t// Return to caller %>"
  [(set_attr "length" "2")
   (set_attr "type" "realBranch")
   (set_attr "longConstant" "false")])

;; Peephole either 2 LDWs or STWs into LDL/STL.
(define_peephole2
  [(set (match_operand:HI 0 "register_operand" "")
        (match_operand:HI 1 "memory_operand" ""))
   (set (match_operand:HI 2 "register_operand" "")
        (match_operand:HI 3 "memory_operand" ""))]
  "ok_to_peephole_ldw(operands[0],operands[1],operands[2],operands[3])"
  [(set (match_dup 4) (match_dup 5))]
  "{
     operands[4] = gen_min_reg(operands[0],operands[2]);
     operands[5] = gen_SImode_mem(operands[1],operands[3]);
   }")

(define_peephole2
  [(set (match_operand:HI 0 "memory_operand" "")
        (match_operand:HI 1 "register_operand" ""))
   (set (match_operand:HI 2 "memory_operand" "")
        (match_operand:HI 3 "register_operand" ""))]
  "ok_to_peephole_stw(operands[0],operands[1],operands[2],operands[3])"
  [(set (match_dup 4) (match_dup 5))]
  "{
     operands[4] = gen_SImode_mem(operands[0],operands[2]);
     operands[5] = gen_min_reg(operands[1],operands[3]);
   }")


;; We have instructions like add,subtract,ior,and that set condition
;; codes if they are executed on slot 0. If we have
;;    add a = b + c
;;    if (a!=0)
;;    {}
;; We would have RTL sequence like
;;    add.# rb,rc,ra   # will be replaced by slot no, after scheduling
;;    sub.0 ra,0,r15
;;    bnz
;; Instead, we can just do
;;    add.0 rb,rc,ra
;;    bnz

(define_peephole2
  [(parallel [(set (match_operand:HI 0 "register_operand" "")
                   (plus:HI (match_operand:HI 1 "register_operand" "")
                            (match_operand:HI 2 "general_operand" "")))
              (clobber (reg:CC CC_REGNUM))])
   (parallel [(set (pc)
                   (if_then_else
                    (match_operator:CC 3 "picochip_peephole_comparison_operator"
                            [(match_dup 0) (const_int 0)])
                   (label_ref       (match_operand    6 "" ""))
                   (pc)))
              (clobber (reg:CC CC_REGNUM))])]
  ""
  [(parallel [(set (match_dup 0)
                   (plus:HI (match_dup 1) (match_dup 2)))
              (set (reg:CC CC_REGNUM)
                   (match_op_dup 3 [(const_int 0) (const_int 0)]))])
   (parallel [(set (pc)
                   (if_then_else
                    (match_op_dup:HI 3 [(reg:CC CC_REGNUM) (const_int 0)])
                   (label_ref (match_dup 6))
                   (pc)))
              (use (match_dup 7))])]
  "{
     operands[7] = GEN_INT(0);
   }")

(define_peephole2
  [(parallel [(set (match_operand:HI 0 "register_operand" "")
                   (plus:HI (match_operand:HI 1 "register_operand" "")
                     (match_operand:HI 2 "general_operand" "")))
              (clobber (reg:CC CC_REGNUM))])
   (set (reg:CC CC_REGNUM)
         (match_operator:CC 3 "picochip_peephole_comparison_operator"
                   [(match_dup 0) (const_int 0)]))
   (parallel [(set (pc)
                    (if_then_else
                          (match_operator 4 "comparison_operator"
                              [(reg:CC CC_REGNUM) (const_int 0)])
                     (label_ref (match_operand 5 "" ""))
                     (pc)))
               (use (match_operand:HI 6 "const_int_operand" ""))])]
  ""
  [(parallel [(set (match_dup 0)
                   (plus:HI (match_dup 1) (match_dup 2)))
              (set (reg:CC CC_REGNUM)
                   (match_op_dup 3 [(const_int 0) (const_int 0)]))])
   (parallel [(set (pc)
                   (if_then_else (match_op_dup:HI 4 [(reg:CC CC_REGNUM) (const_int 0)])
                    (label_ref (match_dup 5))
                    (pc)))
              (use (match_dup 6))])]
  "{
     operands[7] = GEN_INT(0);
   }")


;; If peephole happens before the cbranch split

(define_peephole2
  [(parallel [(set (match_operand:HI 0 "register_operand" "")
                    (minus:HI (match_operand:HI 1 "general_operand" "")
                              (match_operand:HI 2 "register_operand" "")))
              (clobber (reg:CC CC_REGNUM))])
   (parallel [(set (pc)
                   (if_then_else
                    (match_operator:CC 3 "picochip_peephole_comparison_operator"
                            [(match_dup 0) (const_int 0)])
                     (label_ref       (match_operand    6 "" ""))
                     (pc)))
              (clobber (reg:CC CC_REGNUM))])]
  ""
  [(parallel [(set (match_dup 0)
                   (minus:HI (match_dup 1) (match_dup 2)))
              (set (reg:CC CC_REGNUM)
                   (match_op_dup 3 [(const_int 0) (const_int 0)]))])
   (parallel [(set (pc)
                   (if_then_else
                       (match_op_dup:HI 3 [(reg:CC CC_REGNUM) (const_int 0)])
                        (label_ref (match_dup 6))
                        (pc)))
              (use (match_dup 7))])]
  "{
     operands[7] = GEN_INT(0);
   }")


;; If peephole happens after the cbranch split

(define_peephole2
  [(parallel [(set (match_operand:HI 0 "register_operand" "")
                   (minus:HI (match_operand:HI 1 "general_operand" "")
                             (match_operand:HI 2 "register_operand" "")))
              (clobber (reg:CC CC_REGNUM))])
   (set (reg:CC CC_REGNUM)
         (match_operator:CC 3 "picochip_peephole_comparison_operator"
                 [(match_dup 0) (const_int 0)]))
    (parallel [(set (pc)
                     (if_then_else
                         (match_operator 4 "comparison_operator"
                             [(reg:CC CC_REGNUM) (const_int 0)])
                      (label_ref (match_operand 5 "" ""))
                      (pc)))
                (use (match_operand:HI 6 "const_int_operand" ""))])]
  ""
  [(parallel [(set (match_dup 0)
                   (minus:HI (match_dup 1) (match_dup 2)))
              (set (reg:CC CC_REGNUM)
                   (match_op_dup 3 [(const_int 0) (const_int 0)]))])
   (parallel [(set (pc)
                   (if_then_else (match_op_dup:HI 4 [(reg:CC CC_REGNUM) (const_int 0)])
                                 (label_ref (match_dup 5))
                                 (pc)))
              (use (match_dup 6))])]
  "{
      operands[7] = GEN_INT(0);
   }")

;; If peephole happens before the cbranch split

(define_peephole2
   [(parallel[(set (match_operand:HI 0 "register_operand" "")
                   (and:HI (match_operand:HI 1 "register_operand" "")
                           (match_operand:HI 2 "general_operand" "")))
              (clobber (reg:CC CC_REGNUM))])
   (parallel [(set (pc)
                   (if_then_else
                    (match_operator:CC 3 "picochip_peephole_comparison_operator"
                            [(match_dup 0) (const_int 0)])
                   (label_ref       (match_operand    6 "" ""))
                   (pc)))
              (clobber (reg:CC CC_REGNUM))])]
  ""
  [(parallel [(set (match_dup 0)
                   (and:HI (match_dup 1) (match_dup 2)))
              (set (reg:CC CC_REGNUM)
                   (match_op_dup 3 [(const_int 0) (const_int 0)]))])
   (parallel [(set (pc)
                   (if_then_else
                       (match_op_dup:HI 3 [(reg:CC CC_REGNUM) (const_int 0)])
                                 (label_ref (match_dup 6))
                                 (pc)))
              (use (match_dup 7))])]
  "{
     operands[7] = GEN_INT(0);
   }")

(define_peephole2
   [(parallel[(set (match_operand:HI 0 "register_operand" "")
                   (and:HI (match_operand:HI 1 "register_operand" "")
                           (match_operand:HI 2 "general_operand" "")))
              (clobber (reg:CC CC_REGNUM))])
   (set (reg:CC CC_REGNUM)
        (match_operator:CC 3 "picochip_peephole_comparison_operator"
                    [(match_dup 0) (const_int 0)]))
  (parallel [(set (pc)
                  (if_then_else
                      (match_operator 4 "comparison_operator"
                         [(reg:CC CC_REGNUM) (const_int 0)])
                   (label_ref (match_operand 5 "" ""))
                   (pc)))
              (use (match_operand:HI 6 "const_int_operand" ""))])]
  ""
  [(parallel [(set (match_dup 0)
                   (and:HI (match_dup 1) (match_dup 2)))
              (set (reg:CC CC_REGNUM)
                   (match_op_dup 3 [(const_int 0) (const_int 0)]))])
   (parallel [(set (pc)
                   (if_then_else (match_op_dup:HI 4 [(reg:CC CC_REGNUM) (const_int 0)])
                                 (label_ref (match_dup 5))
                                 (pc)))
              (use (match_dup 6))])]
  "{
      operands[7] = GEN_INT(0);
   }")

;; If peephole happens before the cbranch split

(define_peephole2
   [(parallel[(set (match_operand:HI 0 "register_operand" "")
                   (ior:HI (match_operand:HI 1 "register_operand" "")
                           (match_operand:HI 2 "general_operand" "")))
              (clobber (reg:CC CC_REGNUM))])
   (parallel [(set (pc)
                   (if_then_else
                    (match_operator:CC 3 "picochip_peephole_comparison_operator"
                          [(match_dup 0) (const_int 0)])
                   (label_ref       (match_operand    6 "" ""))
                   (pc)))
              (clobber (reg:CC CC_REGNUM))])]
  ""
  [(parallel [(set (match_dup 0)
                   (ior:HI (match_dup 1) (match_dup 2)))
              (set (reg:CC CC_REGNUM)
                   (match_op_dup 3 [(const_int 0) (const_int 0)]))])
   (parallel [(set (pc)
                   (if_then_else
                       (match_op_dup:HI 3 [(reg:CC CC_REGNUM) (const_int 0)])
                                 (label_ref (match_dup 6))
                                 (pc)))
              (use (match_dup 7))])]
  "{
     operands[7] = GEN_INT(0);
   }")

(define_peephole2
   [(parallel[(set (match_operand:HI 0 "register_operand" "")
                   (ior:HI (match_operand:HI 1 "register_operand" "")
                           (match_operand:HI 2 "general_operand" "")))
              (clobber (reg:CC CC_REGNUM))])
   (set (reg:CC CC_REGNUM)
        (match_operator:CC 3 "picochip_peephole_comparison_operator"
              [(match_dup 0) (const_int 0)]))
  (parallel [(set (pc)
                  (if_then_else
                     (match_operator 4 "comparison_operator"
                        [(reg:CC CC_REGNUM) (const_int 0)])
                   (label_ref (match_operand 5 "" ""))
                   (pc)))
             (use (match_operand:HI 6 "const_int_operand" ""))])]
  ""
  [(parallel [(set (match_dup 0)
                   (ior:HI (match_dup 1) (match_dup 2)))
              (set (reg:CC CC_REGNUM)
                   (match_op_dup 3 [(const_int 0) (const_int 0)]))])
   (parallel [(set (pc)
                   (if_then_else (match_op_dup:HI 4 [(reg:CC CC_REGNUM) (const_int 0)])
                                 (label_ref (match_dup 5))
                                 (pc)))
              (use (match_dup 6))])]
  "{
      operands[7] = GEN_INT(0);
   }")

;; Conditional branch (HI). This is split into separate compare and
;; branch instructions if scheduling is enabled.  The branch
;; instruction is supplied with the type of comparison on which the
;; branch should occur.

(define_insn_and_split "cbranchhi4"
  [(set (pc)
        (if_then_else
            (match_operator:CC 0 "ordered_comparison_operator"
                            [(match_operand:HI 1 "register_operand" "r")
                             (match_operand:HI 2 "picochip_comparison_operand" "ri")])
            (label_ref       (match_operand    3 "" ""))
            (pc)))
   (clobber (reg:CC CC_REGNUM))]
  ""
  "* return picochip_output_cbranch(operands);"
  "reload_completed
   && (picochip_schedule_type != DFA_TYPE_NONE || flag_delayed_branch)"
  [(set (reg:CC CC_REGNUM) (match_dup 0))
   (parallel [(set (pc)
                   (if_then_else (match_op_dup:HI 0 [(reg:CC CC_REGNUM) (const_int 0)])
                                 (label_ref (match_dup 3))
                                 (pc)))
              (use (match_dup 4))])]
  "{
     operands[4] = GEN_INT(GET_CODE(operands[0]));
   }")

;; The only difference between this and the next pattern is that the next pattern
;; might introduce subtracts whose first operand is a constant. This would have to
;; be a longConstant. But, we know that such a situation wouldnt arise for supported
;; comparison operator and hence this pattern assumes that the second constraint combo
;; would still generate a normal instruction.

(define_insn "*supported_compare"
  [(set (reg:CC CC_REGNUM)
        (match_operator:CC 0 "picochip_supported_comparison_operator"
                        [(match_operand:HI 1 "register_operand" "r,r,r")
                         (match_operand:HI 2 "picochip_comparison_operand" "r,J,i")]))]
  ""
  "* return picochip_output_compare(operands);"
  [; Must be picoAlu because it sets the condition flags.
   (set_attr "type" "picoAlu,picoAlu,picoAlu")
   (set_attr "longConstant" "false,false,true")
   (set_attr "length" "2,2,4")
   ])

(define_insn "*compare"
  [(set (reg:CC CC_REGNUM)
        (match_operator:CC 0 "comparison_operator"
                        [(match_operand:HI 1 "register_operand" "r,r,r")
                         (match_operand:HI 2 "picochip_comparison_operand" "r,M,i")]))]
  ""
  "* return picochip_output_compare(operands);"
  [; Must be picoAlu because it sets the condition flags.
   (set_attr "type" "picoAlu,picoAlu,picoAlu")
   (set_attr "longConstant" "false,true,true")
   (set_attr "length" "2,4,4")
   ])

; Match a branch instruction, created from a tstport/cbranch split.
; We use a "use" clause so GCC doesnt try to use this pattern generally.
(define_insn "*branch"
  [(set (pc)
        (if_then_else
            (match_operator 2 "comparison_operator"
                 [(reg:CC CC_REGNUM) (const_int 0)])
                      (label_ref (match_operand 0 "" ""))
                      (pc)))
   (use (match_operand:HI 1 "const_int_operand" ""))]
  ""
  "* return picochip_output_branch(operands, insn);"
  [(set (attr "length")
        (if_then_else
         (and (ge (minus (match_dup 0) (pc)) (const_int MIN_BRANCH_OFFSET))
              (le (minus (match_dup 0) (pc)) (const_int MAX_BRANCH_OFFSET)))
         (const_int SHORT_BRANCH_LENGTH)
         (const_int LONG_BRANCH_LENGTH)))
    (set (attr "type")
        (if_then_else
         (eq_attr "length" "6")
         (const_string "realBranch")
         (const_string "unknown")))])

;; If a movqi is used which accesses memory on a machine which doesn't
;; have byte addressing, synthesise the instruction using word load/store
;; operations. The movqi's that are required during reload phase are
;; handled using reload_inqi/reload_outqi.

(define_expand "movqi"
  [(set (match_operand:QI 0 "nonimmediate_operand" "")
	(match_operand:QI 1 "general_operand" ""))]
  ""
{

     if (!reload_completed &&
         !TARGET_HAS_BYTE_ACCESS &&
         (MEM == GET_CODE(operands[0]) || MEM == GET_CODE(operands[1])))
     {
       rtx address;
       rtx wordAddress;
       rtx const1;
       rtx shiftVal;
       rtx loadedValue;
       rtx addressMask;

       warn_of_byte_access();

       /* Load the constant 1 into a register. */
       const1 = gen_reg_rtx(HImode);
       emit_insn(gen_rtx_SET(HImode, const1, GEN_INT(1)));

       /* Load the address mask with the bitwise complement of 1. */
       addressMask = gen_reg_rtx(HImode);
       emit_insn(gen_rtx_SET(HImode, addressMask, GEN_INT(-2)));

       /* Handle loads first, in case we are dealing with a mem := mem
        * instruction. */
       if (MEM == GET_CODE(operands[1]))
       {
	 /* Loads work as follows. The entire word containing the desired byte
          * is loaded. The bottom bit of the address indicates which
          * byte is required. The desired byte is moved into the most
          * significant byte, and then an arithmetic shift right
          * invoked to achieve sign extension. The desired byte is
          * moved to the MSB by XOR'ing the bottom address bit by 1,
          * multiplying the result by 8, and then shifting left by
          * that amount. Note that shifts only operate on the bottom
          * 4-bits of the source offset, so although the XOR may
          * produce a value which has its upper bits set, only bit 4
          * (i.e., the inverted, shifted bottom address bit) actually
          * gets used.
          */

         /* Ensure the address is in a register. */
         address = gen_reg_rtx(HImode);
         emit_insn(gen_rtx_SET(HImode, address, XEXP(operands[1], 0)));

         /* Compute the word address by masking out the bottom bit. */
         wordAddress = gen_reg_rtx(HImode);
         emit_insn(gen_andhi3(wordAddress, address, addressMask));

         /* Compute the shift value. This is the bottom address bit,
          * inverted, and multiplied by 8. */
         shiftVal = gen_reg_rtx(HImode);
         emit_insn(gen_xorhi3(shiftVal, address, const1));
         emit_insn(gen_ashlhi3(shiftVal, shiftVal, GEN_INT(3)));

         /* Emit the memory load. */
         loadedValue = gen_reg_rtx(HImode);
         emit_insn(gen_rtx_SET(HImode, loadedValue, gen_rtx_MEM(HImode, wordAddress)));

	 /* Shift the desired byte to the most significant byte. */
	 rtx topByteValue = gen_reg_rtx (HImode);
	 emit_insn (gen_ashlhi3 (topByteValue, loadedValue, shiftVal));

         /* Sign extend the top-byte back into the bottom byte. */
	 rtx signExtendedValue = gen_reg_rtx(HImode);
         emit_insn(gen_ashrhi3(signExtendedValue, topByteValue, GEN_INT(8)));

         /* Final extraction of QI mode register. */
        operands[1] = gen_rtx_SUBREG(QImode, signExtendedValue, 0);

       }

       if (MEM == GET_CODE(operands[0]) && GET_CODE(operands[1]) != MEM)
       {
         rtx zeroingByteMask;
         rtx temp;
         rtx tempQiMode;
         rtx tempHiMode;

         /* Get the address. */
         address = gen_reg_rtx(HImode);
         emit_insn(gen_rtx_SET(HImode, address, XEXP(operands[0], 0)));

         /* Compute the word aligned address. */
         wordAddress = gen_reg_rtx(HImode);
         emit_insn(gen_andhi3(wordAddress, address, addressMask));

         /* Compute the shift value. */
         shiftVal = gen_reg_rtx(HImode);
         emit_insn(gen_andhi3(shiftVal, address, const1));
         emit_insn(gen_ashlhi3(shiftVal, shiftVal, GEN_INT(3)));

         /* Emit the memory load. */
         loadedValue = gen_reg_rtx(HImode);
         emit_insn(gen_rtx_SET(HImode, loadedValue, gen_rtx_MEM(HImode, wordAddress)));

         /* Zero out the destination bits by AND'ing with 0xFF00
          * shifted appropriately. */
         zeroingByteMask = gen_reg_rtx(HImode);
         emit_insn(gen_rtx_SET(HImode, zeroingByteMask, GEN_INT(-256)));
         emit_insn(gen_lshrhi3(zeroingByteMask, zeroingByteMask, shiftVal));
         emit_insn(gen_andhi3(loadedValue, loadedValue, zeroingByteMask));

	 /* Grab the incoming QI register, and ensure that the top bits
	  * are zeroed out. This is because the register may be
	  * storing a signed value, in which case the top-bits will be
	  * sign bits. These must be removed to ensure that the
	  * read-modify-write (which uses an OR) doesn't pick up those
	  * bits, instead of the original memory value which is being
	  * modified.
  	  */
         /*if (register_operand(operands[1],QImode))
         {
           tempHiMode = XEXP(operands[1], 0);
         }
         else
         {
           tempHiMode = operands[1];
         }*/
         //tempHiMode = force_reg(QImode, operands[1]);
         tempHiMode = simplify_gen_subreg(HImode, operands[1], QImode, 0);
         temp = gen_reg_rtx(HImode);
	 emit_insn(gen_rtx_SET(HImode, temp, tempHiMode));
         rtx lsbByteMask = gen_reg_rtx (HImode);
	 emit_insn (gen_rtx_SET (HImode, lsbByteMask, GEN_INT (0xFF)));
	 emit_insn (gen_andhi3 (temp, temp, lsbByteMask));

         /* Shift the incoming byte value by the appropriate amount,
          * and OR into the load value. */
         emit_insn(gen_ashlhi3(temp, temp, shiftVal));
         emit_insn(gen_iorhi3(loadedValue, loadedValue, temp));

         /* Rewrite the original assignment, to assign the new value
          * to the word address. */
         operands[0] = gen_rtx_MEM(HImode, wordAddress);
         operands[1] = loadedValue;

       }

     }
})

(define_insn "*movqi_sign_extend"
  [(set (match_operand:HI 0 "register_operand" "=r,r")
	(sign_extend:HI (match_operand:QI 1 "memory_operand" "a,m")))]
  "TARGET_HAS_BYTE_ACCESS"
  "@
     LDB (%a1),%0\t\t// %0 = Mem(%a1)
     LDB %a1,%0\t\t// %0 = Mem(%M1{byte})"
  [(set_attr "type" "mem,mem")
   (set_attr "longConstant" "true,false")
   (set_attr "length" "4,4")])

;; movqi instructions for machines with and without byte access.
(define_insn "*movqi_byte"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=r,r,r,r,r,a,m")
	(match_operand:QI 1 "general_operand" "r,a,m,I,i,r,r"))]
  "TARGET_HAS_BYTE_ACCESS"
  "@
     COPY.%# %1, %0\t// %0 := %1
     LDB (%a1),%0\t\t// %0 = Mem(%a1)
     LDB %a1,%0\t\t// %0 = Mem(%M1{byte})
     COPY.%# %1,%0\t\t// %0 := #%1 (QI) (short constant)
     COPY.%# %1,%0\t\t// %0 := #%1 (QI) (long constant)
     STB %1,(%a0)\t\t// Mem(%a0) := %1
     STB %1,%a0\t\t// Mem(%M0{byte}) := %1"
  [(set_attr "type" "basicAlu,mem,mem,basicAlu,basicAlu,mem,mem")
   (set_attr "longConstant" "false,true,false,false,true,true,false")
   (set_attr "length" "2,4,4,2,4,4,4")])

;; Machines which don't have byte access can copy registers, and load
;; constants, but can't access memory.  The define_expand for movqi
;; should already have rewritten memory accesses using word
;; operations.  The exception is qi reloads, which are handled using
;; the reload_? patterns.
(define_insn "*movqi_nobyte"
  [(set (match_operand:QI 0 "register_operand" "=r,r")
	(match_operand:QI 1 "picochip_register_or_immediate_operand" "r,i"))]
  "!TARGET_HAS_BYTE_ACCESS"
  "@
     COPY.%# %1,%0\t// %0 := %1
     COPY.%# %1,%0\t\t// %0 := #%1 (QI)")

(define_insn "movhi"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=r,r,r,a,m,r,r")
	(match_operand:HI 1 "general_operand" "r,a,m,r,r,I,i"))]
  ""
  "@
    COPY.%# %1,%0\t\t// %0 := %1
    LDW (%a1),%0\t\t// %0 := Mem(%a1)
    LDW %a1,%0\t\t// %0 = Mem(%M1{byte})
    STW %1,(%a0)\t\t// Mem(%a0) := %1
    STW %1,%a0\t\t// Mem(%M0{byte}) := %1
    COPY.%# %1,%0\t// %0 := %1 (short constant)
    COPY.%# %1,%0\t// %0 := %1 (long constant)"
   [(set_attr "type" "basicAlu,mem,mem,mem,mem,basicAlu,basicAlu")
    (set_attr "longConstant" "false,true,false,true,false,false,true")
    (set_attr "length" "2,4,4,4,4,2,4")])

(define_insn "movsi"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=r,r,r,r,a,m")
	(match_operand:SI 1 "general_operand" "r,a,m,i,r,r"))]
  ""
  "@
    // %R0 := %R1 (SI)\n\tCOPY.%# %L1,%L0 %| COPY.1 %U1,%U0
    LDL (%a1),%R0\t\t// %R0 = Mem(%a1)
    LDL %a1,%R0\t\t// %R0 = Mem(%M1{byte})
    // %R0 := #%1 (SI)\n\tCOPY.%# %L1,%L0 %| COPY.%# %U1,%U0
    STL %R1,(%a0)\t\t// Mem(%a0) := %R1
    STL %R1,%a0\t\t// Mem(%M0{byte}) := %R1"
  [(set_attr "type" "unknown,mem,mem,unknown,mem,mem")
   (set_attr "longConstant" "false,true,false,true,false,false")
   (set_attr "length" "4,4,4,6,4,4")])

; Split an SI mode register copy into separate HI mode copies, which
; can be VLIW'd with other instructions.  Only split the instruction
; when VLIW scheduling is enabled.  Splitting the instruction saves
; some code space.
;
; This is predicated in reload_completed.  This ensures that the
; instructions aren't broken up too early which can result in the
; SImode code being converted into inefficient HI mode code.

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
        (match_operand:SI 1 "register_operand" ""))]
  "reload_completed && picochip_schedule_type == DFA_TYPE_SPEED"
  [(set (match_dup 2) (match_dup 3))
   (set (match_dup 4) (match_dup 5))]
  "{
     operands[2] = gen_lowpart (HImode, operands[0]);
     operands[3] = gen_lowpart (HImode, operands[1]);
     operands[4] = gen_highpart (HImode, operands[0]);
     operands[5] = gen_highpart (HImode, operands[1]);
 }")

; SI Mode split for load constant.
(define_split
  [(set (match_operand:SI 0 "register_operand" "")
        (match_operand:SI 1 "const_int_operand" ""))]
  ""
  [(set (match_dup 2) (match_dup 3))
   (set (match_dup 4) (match_dup 5))]
  "{
     operands[2] = gen_lowpart (HImode, operands[0]);
     operands[3] = picochip_get_low_const(operands[1]);
     operands[4] = gen_highpart (HImode, operands[0]);
     operands[5] = picochip_get_high_const(operands[1]);
 }")

(define_insn "movsf"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=r,r,r,m")
	(match_operand:SF 1 "general_operand" "r,m,i,r"))]
  ""
  "@
    // %R0 := %R1 (SF)\n\tCOPY.%# %L1,%L0 %| COPY.1 %U1,%U0
    LDL %a1,%R0\t\t// %R0 :={SF} Mem(%M1{byte})
    // %R0 := #%1 (SF)\n\tCOPY.%# %L1,%L0\n\tCOPY.%# %U1,%U0
    STL %R1,%a0\t\t// Mem(%M0{byte}) :={SF} %R1")

;;===========================================================================
;; NOP
;;===========================================================================

;; No-operation (NOP)
(define_insn "nop"
  [(const_int 0)]
  ""
  "NOP\t// nop"
  [(set_attr "length" "1")])

;;===========================================================================
;; Function Calls.  Define expands are used to ensure that the correct
;; type of pattern is emitted, and then the define_insn's match the
;; pattern using the correct types.
;;
;; Note: The comments output as part of these instructions are detected by
;; the linker. Don't change the comments!
;;===========================================================================

(define_expand "call"
  [(parallel [(call (match_operand:QI 0 "memory_operand" "")
	 (match_operand 1 "const_int_operand" ""))
         (clobber (reg:HI LINK_REGNUM))])]
  ""
  "")

(define_insn "call_for_divmod"
  [(call (match_operand:QI 0 "memory_operand" "")
	 (match_operand 1 "const_int_operand" ""))]
  ""
  "JL (%M0)\t// fn_call %M0%>"
  [(set_attr "length" "4")
   (set_attr "type" "realBranch")
   (set_attr "longConstant" "true")])

(define_insn "*call_using_symbol"
  [(call (mem:QI (match_operand:HI 0 "immediate_operand" "i"))
	 (match_operand 1 "const_int_operand" ""))
         (clobber (reg:HI LINK_REGNUM))]
  ""
  "JL (%M0)\t// fn_call %M0%>"
  [(set_attr "length" "4")
   (set_attr "type" "realBranch")
   (set_attr "longConstant" "true")])

(define_insn "*call_using_register"
  [(call (mem:QI (match_operand:HI 0 "register_operand" "r"))
	 (match_operand 1 "const_int_operand" ""))
         (clobber (reg:HI LINK_REGNUM))]
  ""
  "JL (%r0)\t// fn_call_unknown %r0%>"
  [(set_attr "length" "2")
   (set_attr "type" "realBranch")
   (set_attr "longConstant" "false")])

(define_expand "call_value"
  [(parallel [(set (match_operand:HI       0 "" "")
	(call:HI (match_operand:QI 1 "memory_operand" "g")
	      (match_operand 2 "const_int_operand" "")))
         (clobber (reg:HI LINK_REGNUM))])]
  ""
  "")

(define_insn "*call_value_using_symbol"
  [(set (match_operand:HI 0 "" "")
	(call:HI (mem:QI (match_operand:HI 1 "immediate_operand" "i"))
	      (match_operand 2 "const_int_operand" "")))
         (clobber (reg:HI LINK_REGNUM))]
  ""
  "JL (%M1)\t// fn_call %M1 (value return)%>"
  [(set_attr "length" "4")
   (set_attr "type" "realBranch")
   (set_attr "longConstant" "true")])

(define_insn "*call_value_using_register"
  [(set (match_operand:HI 0 "" "")
	(call:HI (mem:QI (match_operand:HI 1 "register_operand" "r"))
	      (match_operand 2 "const_int_operand" "")))
         (clobber (reg:HI LINK_REGNUM))]
  ""
  "JL (%r1)// fn_call_unknown %r1 (value return)%>"
  [(set_attr "length" "2")
   (set_attr "type" "realBranch")
   (set_attr "longConstant" "false")])

;;===========================================================================
;; Addition
;;===========================================================================

;; Note that the addition of a negative value is transformed into the
;; subtraction of a positive value, so that the add/sub immediate slot
;; can make better use of the 4-bit range.

(define_insn "addhi3"
  [(set (match_operand:HI 0 "register_operand" "=r,r,r,r")
	(plus:HI (match_operand:HI 1 "register_operand" "r,r,r,r")
		 (match_operand:HI 2 "general_operand" "r,M,n,i")))
   (clobber (reg:CC CC_REGNUM))]
  ""
  {  if (CONST_INT == GET_CODE(operands[2]) &&
         INTVAL(operands[2]) > -16 &&
         INTVAL(operands[2]) < 0)
       return "SUB.%# %1,-(%2),%0\t// %0 := %1 + %2 (HI)";
     else
       return "ADD.%# %1,%2,%0\t// %0 := %1 + %2 (HI)";
  }
  [(set_attr "type" "basicAlu,basicAlu,basicAlu,basicAlu")
   (set_attr "longConstant" "false,false,true,true")
   (set_attr "length" "2,2,4,4")]
  )


;; If we peepholed the compare instruction out, we need to make sure the add
;; goes in slot 0. This pattern is just to accomplish that.

(define_insn "addhi3_with_use_clause"
  [(set (match_operand:HI 0 "register_operand" "=r,r,r,r")
        (plus:HI (match_operand:HI 1 "register_operand" "r,r,r,r")
                 (match_operand:HI 2 "general_operand" "r,M,n,i")))
   (set (reg:CC CC_REGNUM)
        (match_operator:CC 3 "picochip_peephole_comparison_operator"
                        [(const_int 0)
                         (const_int 0)]))]
  ""
  {  if (CONST_INT == GET_CODE(operands[2]) &&
         INTVAL(operands[2]) > -16 &&
         INTVAL(operands[2]) < 0)
       return "SUB.0 %1,-(%2),%0\t// %0 := %1 + %2 (HI)";
     else
       return "ADD.0 %1,%2,%0\t// %0 := %1 + %2 (HI)";
  }
  [(set_attr "type" "picoAlu,picoAlu,picoAlu,picoAlu")
   (set_attr "longConstant" "false,false,true,true")
   (set_attr "length" "2,2,4,4")]
  )

;; Match an addition in which the first operand has been shifted
;; (e.g., the comms array functions can emit such instructions).
(define_insn "*addWith1stOpShift"
  [(set (match_operand:HI 0 "register_operand" "=r,r")
	(plus:HI (ashift:HI (match_operand:HI 1 "register_operand" "r,r")
			    (match_operand:HI 2 "const_int_operand" ""))
		 (match_operand:HI 3 "immediate_operand" "I,i")))
   (clobber (reg:CC CC_REGNUM))]
  ""
  "ADD.0 [LSL %1,%2],%3,%0\t// %0 := (%1 << %2) + %3"
  [(set_attr "type" "picoAlu,picoAlu")
   (set_attr "longConstant" "false,true")])

(define_insn_and_split "addsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(plus:SI (match_operand:SI 1 "register_operand" "r,r")
		 (match_operand:SI 2 "general_operand" "r,i")))
   (clobber (reg:CC CC_REGNUM))]
  ""
  "// %0 := %1 + %2 (SI)\n\tADD.0 %L1,%L2,%L0\n\tADDC.0 %U1,%U2,%U0"
  "reload_completed && picochip_schedule_type != DFA_TYPE_NONE"
  [(match_dup 4)
   (match_dup 5)]
  "
{
  rtx op0_high = gen_highpart (HImode, operands[0]);
  rtx op1_high = gen_highpart (HImode, operands[1]);
  rtx op0_low  = gen_lowpart (HImode, operands[0]);
  rtx op1_low  = gen_lowpart (HImode, operands[1]);
  rtx op2_high, op2_low;

  if (CONST_INT == GET_CODE(operands[2]))
  {
    op2_high = picochip_get_high_const(operands[2]);
    op2_low = picochip_get_low_const(operands[2]);
  } else {
    op2_high = gen_highpart (HImode, operands[2]);
    op2_low  = gen_lowpart (HImode, operands[2]);
  }

  operands[4] = gen_add_multi_lower (op0_low, op1_low, op2_low);
  operands[5] = gen_add_multi_upper (op0_high, op1_high, op2_high);

}")

;; Perform the lowest part of a multi-part addition (SI/DI). This sets
;; the flags, so is an picoAlu instruction (we could use a
;; conventional addhi, but the addhi is better off being a treated as
;; a basicAlu instruction, rather than a picoAlu instruction).
(define_insn "add_multi_lower"
  [(set (match_operand:HI 0 "register_operand" "=r,r,r")
	(plus:HI (match_operand:HI 1 "register_operand" "r,r,r")
		 (match_operand:HI 2 "general_operand" "r,M,i")))
   (set (reg:CC CC_REGNUM)
	(compare:CC (plus:HI (match_dup 1)
			     (match_dup 2))
		    (const_int 0)))]
  ""
  {  if (CONST_INT == GET_CODE(operands[2]) &&
         INTVAL(operands[2]) > -16 &&
         INTVAL(operands[2]) < 0)
       return "SUB.%# %1,-(%2),%0\t// %0+carry := %1 + %2 (low multi-part)";
     else
       return "ADD.%# %1,%2,%0\t// %0+carry := %1 + %2 (low multi-part)";
  }
  [(set_attr "type" "picoAlu,picoAlu,picoAlu")
   (set_attr "longConstant" "false,false,true")
   (set_attr "length" "2,2,4")])

;; Perform the central part of a multi-part addition (DI). This uses
;; the CC register, and also sets the CC register, so needs to be
;; placed in the first ALU slot.  Note that the ADDC must
;; use the long constant to represent immediates.
(define_insn "add_multi_mid"
  [(set (match_operand:HI 0 "register_operand" "=r,r")
	(plus:HI (match_operand:HI 1 "register_operand" "r,r")
		 (plus:HI (match_operand:HI 2 "general_operand" "r,i")
			  (reg:CC CC_REGNUM))))
   (set (reg:CC CC_REGNUM)
	(compare:CC (plus:HI (match_dup 1)
			     (match_dup 2))
		    (const_int 0)))]
  ""
  "ADDC.%# %1,%2,%0\t// %0+carry := carry + %1 + %2 (mid multi-part)"
  [(set_attr "type" "picoAlu,picoAlu")
   (set_attr "longConstant" "false,true")
   (set_attr "length" "2,4")])

;; Perform the highest part of a multi-part addition (SI/DI). This
;; uses the CC register, but doesn't require any registers to be set,
;; so may be scheduled in either of the ALU's.  Note that the ADDC must
;; use the long constant to represent immediates.
(define_insn "add_multi_upper"
  [(set (match_operand:HI 0 "register_operand" "=r,r")
	(plus:HI (match_operand:HI 1 "register_operand" "r,r")
		 (plus:HI (match_operand:HI 2 "general_operand" "r,i")
			  (reg:CC CC_REGNUM))))
   (clobber (reg:CC CC_REGNUM))]
  ""
  "ADDC.%# %1,%2,%0\t// %0 := carry + %1 + %2 (high multi-part)"
  [(set_attr "type" "basicAlu,basicAlu")
   (set_attr "longConstant" "false,true")
   (set_attr "length" "2,4")])

;; The lea instruction is a special type of add operation, which looks
;; like a movhi (reg := address). It expands into reg := fp +
;; offset.  Ideally there should be two variants, which take different
;; sized offsets (i.e., using the long constant, or not, as
;; appropriate).  However, the address operand may have arbitrary
;; values added to it later (i.e., the AP will be eliminated, possibly
;; converting a small offset into a long offset), so a long offset is
;; always assumed.

;; Note that the lea can use an addition, and hence may modify the CC
;; register.  This upsets scheduling, so instead the lea is placed in
;; ALU 1 where it cannot modify CC.

(define_insn "*lea_add"
 [(set (match_operand:HI 0 "nonimmediate_operand" "=r")
       (plus:HI (match_operand:HI 1 "register_operand" "r")
		(match_operand:HI 2 "immediate_operand" "i")))]
 ""
 "ADD.1 %1,%2,%0\t// lea (add)")

;; Note that, though this instruction looks similar to movhi pattern,
;; "p" constraint cannot be specified for operands other than 
;; address_operand, hence the extra pattern below.
(define_insn "*lea_move"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=r")
        (match_operand:HI 1 "address_operand" "p"))]
  ""
  {
    if (REG == GET_CODE(operands[1]))
      return "COPY.1 %1,%0\t// %0 := %1 (lea)";
    else
      return "ADD.1 %b1,%o1,%0\t\t// %0 := %b1 + %o1 (lea)";
  }
  [(set_attr "type" "nonCcAlu")
   (set_attr "longConstant" "true")
   (set_attr "length" "4")])


;;===========================================================================
;; Subtraction.  Note that these patterns never take immediate second
;; operands, since those cases are handled by canonicalising the
;; instruction into the addition of a negative costant.
;; But, if the first operand needs to be a negative constant, it
;; is supported here.
;;===========================================================================

(define_insn "subhi3"
  [(set (match_operand:HI 0 "register_operand" "=r,r,r")
	(minus:HI (match_operand:HI 1 "general_operand" "r,I,i")
		  (match_operand:HI 2 "register_operand" "r,r,r")))
   (clobber (reg:CC CC_REGNUM))]
  ""
  "SUB.%# %1,%2,%0 // %0 := %1 - %2 (HI)"
  [(set_attr "type" "basicAlu,basicAlu,basicAlu")
   (set_attr "longConstant" "false,true,true")
   (set_attr "length" "2,4,4")])

;; If we peepholed the compare instruction out, we need to make sure the
;; sub goes in slot 0. This pattern is just to accomplish that.

(define_insn "subhi3_with_use_clause"
  [(set (match_operand:HI 0 "register_operand" "=r,r,r")
	(minus:HI (match_operand:HI 1 "general_operand" "r,I,i")
		  (match_operand:HI 2 "register_operand" "r,r,r")))
   (set (reg:CC CC_REGNUM)
        (match_operator:CC 3 "picochip_peephole_comparison_operator"
                        [(const_int 0)
                         (const_int 0)]))]
  ""
  "SUB.0 %1,%2,%0 // %0 := %1 - %2 (HI)"
  [(set_attr "type" "picoAlu,picoAlu,picoAlu")
   (set_attr "longConstant" "false,true,true")
   (set_attr "length" "2,4,4")])

(define_insn_and_split "subsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(minus:SI (match_operand:SI 1 "general_operand" "r,i")
		  (match_operand:SI 2 "register_operand" "r,r")))
   (clobber (reg:CC CC_REGNUM))]
  ""
  "// %0 := %1 - %2 (SI)\n\tSUB.%# %L1,%L2,%L0\n\tSUBB.%# %U1,%U2,%U0"
  "reload_completed && picochip_schedule_type != DFA_TYPE_NONE"
  [(match_dup 4)
   (match_dup 5)]
  "
{
  rtx op0_high = gen_highpart (HImode, operands[0]);
  rtx op0_low  = gen_lowpart (HImode, operands[0]);
  rtx op2_high = gen_highpart (HImode, operands[2]);
  rtx op2_low = gen_lowpart (HImode, operands[2]);
  rtx op1_high,op1_low;

  if (CONST_INT == GET_CODE(operands[1]))
  {
    op1_high = picochip_get_high_const(operands[1]);
    op1_low = picochip_get_low_const(operands[1]);
  } else {
    op1_high = gen_highpart (HImode, operands[1]);
    op1_low  = gen_lowpart (HImode, operands[1]);
  }


  operands[4] = gen_sub_multi_lower (op0_low, op1_low, op2_low);
  operands[5] = gen_sub_multi_upper (op0_high, op1_high, op2_high);

}")

;; Match the patterns emitted by the multi-part subtraction splitting.
;; This sets the CC register, so it needs to go into slot 0.
(define_insn "sub_multi_lower"
  [(set (match_operand:HI 0 "register_operand" "=r,r")
	(minus:HI (match_operand:HI 1 "general_operand" "r,i")
		  (match_operand:HI 2 "register_operand" "r,r")))
   (set (reg:CC CC_REGNUM)
	(compare:CC (minus:HI (match_dup 1) (match_dup 2))
		    (const_int 0)))]
  ""
  "SUB.%# %1,%2,%0\t// %0+carry := %1 - %2 (lower SI)"
  [(set_attr "type" "picoAlu,picoAlu")
   (set_attr "longConstant" "false,true")
   (set_attr "length" "2,4")])

;; Perform the central part of a multi-part addition (DI). This uses
;; the CC register, and also sets the CC register, so needs to be
;; placed in the first ALU.
(define_insn "sub_multi_mid"
  [(set (match_operand:HI 0 "register_operand" "=r,r")
	(minus:HI (match_operand:HI 1 "general_operand" "r,i")
		  (minus:HI (match_operand:HI 2 "register_operand" "r,r")
			    (reg:CC CC_REGNUM))))
   (set (reg:CC CC_REGNUM)
	(compare:CC (minus:HI (match_dup 1)
			      (match_dup 2))
		    (const_int 0)))]
  ""
  "SUBB.%# %1,%2,%0\t// %0+carry := carry - %1 - %2 (mid multi-part)"
  [(set_attr "type" "picoAlu,picoAlu")
   (set_attr "longConstant" "false,true")
   (set_attr "length" "2,4")])

(define_insn "sub_multi_upper"
  [(set (match_operand:HI 0 "register_operand" "=r,r")
	(minus:HI (match_operand:HI 1 "general_operand" "r,i")
		  (minus:HI (match_operand:HI 2 "register_operand" "r,r")
			    (reg:CC CC_REGNUM))))
   (clobber (reg:CC CC_REGNUM))]
  ""
  "SUBB.%# %1,%2,%0\t// %0 := carry - %1 - %2 (upper SI)"
  [(set_attr "type" "basicAlu,basicAlu")
   (set_attr "longConstant" "false,true")
   (set_attr "length" "2,4")])

;;===========================================================================
;; Multiplication (signed)
;;===========================================================================

(define_insn "multiply_machi"
  [(set (reg:HI ACC_REGNUM)
        (mult:HI (match_operand:HI 0 "register_operand" "r,r")
                 (match_operand:HI 1
                        "picochip_register_or_immediate_operand" "r,i")))]
  "TARGET_HAS_MAC_UNIT"
  "MUL %0,%1,acc0\t// acc0 := %0 * %1 (signed)"
  [(set_attr "length" "3,5")
   (set_attr "type" "mac,mac")
   (set_attr "longConstant" "false,true")])

(define_expand "mulhi3"
  [(set (match_operand:HI 0 "register_operand" "")
	(mult:HI (match_operand:HI 1 "register_operand" "")
		 (match_operand:HI 2 "picochip_register_or_immediate_operand" "")))]
  "TARGET_HAS_MULTIPLY"
  "")

;; Different types of mulhi, depending on the AE type. If the AE has MUL unit,
;; use the following pattern.
(define_insn "*mulhi3_mul"
  [(set (match_operand:HI 0 "register_operand" "=r,r")
	(mult:HI (match_operand:HI 1 "register_operand" "r,r")
		 (match_operand:HI 2
			"picochip_register_or_immediate_operand" "r,i")))]
  "TARGET_HAS_MUL_UNIT"
  "MULL %1,%2,%0 // %0 := %1 * %2 (HI)"
  [(set_attr "length" "3,5")
   (set_attr "type" "mul,mul")
   (set_attr "longConstant" "false,true")])

;; If the AE has MAC unit, instead, use the following pattern.
(define_insn_and_split "*mulhi3_mac"
  [(set (match_operand:HI 0 "register_operand" "=r,r")
	(mult:HI (match_operand:HI 1 "register_operand" "r,r")
		 (match_operand:HI 2
			"picochip_register_or_immediate_operand" "r,i")))]
  "TARGET_HAS_MAC_UNIT"
  "// %0 := %1 * %2\n\tMUL %1,%2,acc0\n\tREADACC acc0,frac,%0"
  "TARGET_HAS_MAC_UNIT && reload_completed"
  [(match_dup 3)
   (match_dup 4)]
  "
{
    rtx const_rtx = GEN_INT(0);
    operands[3] = (gen_multiply_machi(operands[1], operands[2]));
    operands[4] = (gen_movhi_mac(operands[0],const_rtx));
} "
)

(define_insn "umultiply_machisi"
  [(set (reg:SI ACC_REGNUM)
	(mult:SI (zero_extend:SI (match_operand:HI 0 "register_operand" "r"))
		 (zero_extend:SI (match_operand:HI 1 "register_operand" "r"))))]
  "TARGET_HAS_MAC_UNIT"
  "MULUU %0,%1,acc0\t// acc0 := %0 * %1 (unsigned)"
  [(set_attr "length" "3")
   (set_attr "type" "mac")
   (set_attr "longConstant" "false")])

(define_insn "multiply_machisi"
  [(set (reg:SI ACC_REGNUM)
        (mult:SI (sign_extend:SI (match_operand:HI 0 "register_operand" "r,r"))
                 (sign_extend:SI (match_operand:HI 1
                        "picochip_register_or_immediate_operand" "r,i"))))]
  "TARGET_HAS_MAC_UNIT"
  "MUL %0,%1,acc0\t// acc0 := %0 * %1 (signed)"
  [(set_attr "length" "3,5")
   (set_attr "type" "mac,mac")
   (set_attr "longConstant" "false,true")])

;; We want to prevent GCC from thinking ACC is a normal register and using
;; this pattern. We want it to be used only when you use MAC unit 
;; multiplication. Added a "use" clause for that sake.
(define_insn "movsi_mac"
   [(set (match_operand:SI 0 "register_operand" "=r")
        (reg:SI ACC_REGNUM))
    (use (match_operand:SI 1 "const_int_operand" ""))]
  "TARGET_HAS_MAC_UNIT"
  "READACC32 acc0,%R0 \t// %0 := acc0 "
  [(set_attr "length" "3")
   (set_attr "type" "mac")
   (set_attr "longConstant" "false")])

;; We want to prevent GCC from thinking ACC is a normal register and using
;; this pattern. We want it to be used only when you use MAC unit 
;; multiplication. Added a "use" clause for that sake.
(define_insn "movhi_mac"
   [(set (match_operand:HI 0 "register_operand" "=r")
        (reg:HI ACC_REGNUM) )
    (use (match_operand:HI 1 "const_int_operand" ""))]
  "TARGET_HAS_MAC_UNIT"
  "READACC acc0,frac,%0 \t// %0 := acc0 "
  [(set_attr "length" "3")
   (set_attr "type" "mac")
   (set_attr "longConstant" "false")])

;; 16-bit to 32-bit widening signed multiplication.
(define_expand "mulhisi3"
  [(set (match_operand:SI 0 "register_operand" "=&r")
	(mult:SI (sign_extend:SI (match_operand:HI 1 "register_operand" "r"))
		 (sign_extend:SI (match_operand:HI 2 "register_operand" "r"))))]
  "TARGET_HAS_MULTIPLY"
  ""
)

(define_insn_and_split "*mulhisi3_mul"
  [(set (match_operand:SI 0 "register_operand" "=&r")
	(mult:SI (sign_extend:SI (match_operand:HI 1 "register_operand" "r"))
		 (sign_extend:SI (match_operand:HI 2 "register_operand" "r"))))]
  "TARGET_HAS_MUL_UNIT"
  "// %0 := %1 * %2 (HI->SI)\;MULL %1,%2,%L0\;MULH %1,%2,%U0";
  "TARGET_HAS_MUL_UNIT && reload_completed && picochip_schedule_type != DFA_TYPE_NONE"
  [(match_dup 3)
   (match_dup 4)]
  "
{
  rtx op0_high = gen_highpart (HImode, operands[0]);
  rtx op0_low  = gen_lowpart (HImode, operands[0]);
  operands[3] = gen_mulhisi3_mul_lower(op0_low,operands[1],operands[2]);
  operands[4] = gen_mulhisi3_mul_higher(op0_high,operands[1],operands[2]);
}
  "
)

(define_insn "mulhisi3_mul_lower"
  [(set (match_operand:HI 0 "register_operand" "=&r")
	(subreg:HI 
         (mult:SI 
          (sign_extend:SI (match_operand:HI 1 "register_operand" "r"))
	  (sign_extend:SI (match_operand:HI 2 "register_operand" "r"))) 0))]
  "TARGET_HAS_MUL_UNIT"
  "MULL %1,%2,%0"
  [(set_attr "length" "3")
   (set_attr "type" "mul")
   (set_attr "longConstant" "false")])

(define_insn "mulhisi3_mul_higher"
  [(set (match_operand:HI 0 "register_operand" "=&r")
	(subreg:HI 
         (mult:SI 
          (sign_extend:SI (match_operand:HI 1 "register_operand" "r"))
	  (sign_extend:SI (match_operand:HI 2 "register_operand" "r"))) 2))]
  "TARGET_HAS_MUL_UNIT"
  "MULH %1,%2,%0"
  [(set_attr "length" "3")
   (set_attr "type" "mul")
   (set_attr "longConstant" "false")])

(define_insn_and_split "*mulhisi3_mac"
  [(set (match_operand:SI 0 "register_operand" "=&r")
	(mult:SI (sign_extend:SI (match_operand:HI 1 "register_operand" "r"))
		 (sign_extend:SI (match_operand:HI 2 "register_operand" "r"))))]
  "TARGET_HAS_MAC_UNIT"
  "// %0 := %1 * %2 (HI->SI) STAN2\;MUL %1,%2,acc0\;READACC32 acc0,%R0";
  "TARGET_HAS_MAC_UNIT && reload_completed"
  [(match_dup 3)
   (match_dup 4)]
  "
{
    rtx const_rtx = gen_int_mode(0,SImode);
    operands[3] = (gen_multiply_machisi(operands[1], operands[2]));
    operands[4] = (gen_movsi_mac(operands[0],const_rtx));
} "
)
		
;;===========================================================================
;; Widening multiplication (unsigned)
;;===========================================================================

(define_expand "umulhisi3"
  [(set (match_operand:SI 0 "register_operand" "=&r")
	(mult:SI (zero_extend:SI (match_operand:HI 1 "register_operand" "r"))
		 (zero_extend:SI (match_operand:HI 2 "register_operand" "r"))))]
  "TARGET_HAS_MULTIPLY"
  ""
)

(define_insn_and_split "*umulhisi3_mul"
  [(set (match_operand:SI 0 "register_operand" "=&r")
	(mult:SI (zero_extend:SI (match_operand:HI 1 "register_operand" "r"))
		 (zero_extend:SI (match_operand:HI 2 "register_operand" "r"))))]
  "TARGET_HAS_MUL_UNIT"
  "// %0 := %1 * %2 (uHI->uSI Type 1)\;MULUL %1,%2,%L0\n\tMULUH %1,%2,%U0";
  "TARGET_HAS_MUL_UNIT && reload_completed && picochip_schedule_type != DFA_TYPE_NONE"
  [(match_dup 3)
   (match_dup 4)]
  "
{
  rtx op0_high = gen_highpart (HImode, operands[0]);
  rtx op0_low  = gen_lowpart (HImode, operands[0]);
  operands[3] = gen_umulhisi3_mul_lower(op0_low,operands[1],operands[2]);
  operands[4] = gen_umulhisi3_mul_higher(op0_high,operands[1],operands[2]);
}
  "
  )

(define_insn "umulhisi3_mul_lower"
  [(set (match_operand:HI 0 "register_operand" "=&r")
	(subreg:HI 
         (mult:SI 
          (zero_extend:SI (match_operand:HI 1 "register_operand" "r"))
	  (zero_extend:SI (match_operand:HI 2 "register_operand" "r"))) 0))]
  "TARGET_HAS_MUL_UNIT"
  "MULUL %1,%2,%0"
  [(set_attr "length" "3")
   (set_attr "type" "mul")
   (set_attr "longConstant" "false")])

(define_insn "umulhisi3_mul_higher"
  [(set (match_operand:HI 0 "register_operand" "=&r")
	(subreg:HI 
         (mult:SI 
          (zero_extend:SI (match_operand:HI 1 "register_operand" "r"))
	  (zero_extend:SI (match_operand:HI 2 "register_operand" "r"))) 2))]
  "TARGET_HAS_MUL_UNIT"
  "MULUH %1,%2,%0"
  [(set_attr "length" "3")
   (set_attr "type" "mul")
   (set_attr "longConstant" "false")])

(define_insn_and_split "*umulhisi3_mac"
  [(set (match_operand:SI 0 "register_operand" "=&r")
	(mult:SI (zero_extend:SI (match_operand:HI 1 "register_operand" "r"))
		 (zero_extend:SI (match_operand:HI 2 "register_operand" "r"))))]
  "TARGET_HAS_MAC_UNIT"
  "// %0 := %1 * %2 (uHI->uSI Type 3)\;MULUU %1,%2,acc0\;READACC32 acc0,%R0";
  "TARGET_HAS_MAC_UNIT && reload_completed"
  [(match_dup 3)
   (match_dup 4)]
  "
{
    rtx const_rtx = gen_int_mode(0,SImode);
    operands[3] = (gen_umultiply_machisi(operands[1], operands[2]));
    operands[4] = (gen_movsi_mac(operands[0],const_rtx));
} "
)

;;===========================================================================
;; Division (signed)
;;===========================================================================

;; Perform a divmod operation as a function call.  This results in some
;; registers being clobbered (r0-6, r12 - ignore r13,14 as these are
;; known not to be affected).
(define_expand "divmodhi4"
  [
   ; Copy the inputs to r0 and r1.
   (set (reg:HI 0) (match_operand:HI 1 "register_operand" ""))
   (set (reg:HI 1) (match_operand:HI 2 "register_operand" ""))
   ; Make the function call - note that r12 (link) is clobbered. Note also
   ; that an explicit call is generated. This ensures that gcc notices that
   ; any function containing a div/mod is not a leaf function. 
   (parallel [(match_dup 4)
	      (set (reg:HI 0) (div:HI (reg:HI 0) (reg:HI 1)))
              (set (reg:HI 1) (mod:HI (reg:HI 0) (reg:HI 1)))
              (clobber (reg:HI 2))
              (clobber (reg:HI 3))
              (clobber (reg:HI 4))
              (clobber (reg:HI 5))
              (clobber (reg:HI 12))
              (clobber (reg:CC CC_REGNUM))
	      ])
   ; Set the quotient (returned in register 0)
   (set (match_operand:HI 0 "register_operand" "") (reg:HI 0))
   ; Set the remainder (returned in register 1)
   (set (match_operand:HI 3 "register_operand" "") (reg:HI 1))]
  ""
{
  rtx fnName = gen_rtx_SYMBOL_REF (HImode, "_divmodhi4");
  operands[4] = gen_call_for_divmod (gen_rtx_MEM (QImode, fnName), GEN_INT(0));
})

; Match a call to divmodhi4.  As this is a call, the link register
; (r12), and registers r0-5 must be clobbered.  Ignore clobbering of
; r13/4 as these aren't used by the divide function).
(define_insn "*divmodhi4_call"
  [(call (mem:QI (match_operand:HI 0 "immediate_operand" "i"))
	 (match_operand 1 "const_int_operand" ""))
   (set (reg:HI 0) (div:HI (reg:HI 0) (reg:HI 1)))
   (set (reg:HI 1) (mod:HI (reg:HI 0) (reg:HI 1)))
   (clobber (reg:HI 2))
   (clobber (reg:HI 3))
   (clobber (reg:HI 4))
   (clobber (reg:HI 5))
   (clobber (reg:HI 12))
   (clobber (reg:CC CC_REGNUM))
]
  ""
  "JL (%0)\t// call %0%>"
  [(set_attr "length" "4")
   (set_attr "longConstant" "true")
   (set_attr "type" "call")])

;; Perform a udivmod operation as a function call.  This results in some
;; registers being clobbered (r0-6, r12 - ignore r13,14 as these are
;; known not to be affected).
(define_expand "udivmodhi4"
  [
   ; Copy the inputs to r0 and r1.
   (set (reg:HI 0) (match_operand:HI 1 "register_operand" ""))
   (set (reg:HI 1) (match_operand:HI 2 "register_operand" ""))
   ; Make the function call - note that r12 (link) is clobbered. Note also
   ; that an explicit call is generated. This ensures that gcc notices that
   ; any function containing a div/mod is not a leaf function. 
   (parallel [(match_dup 4)
	      (set (reg:HI 0) (udiv:HI (reg:HI 0) (reg:HI 1)))
              (set (reg:HI 1) (umod:HI (reg:HI 0) (reg:HI 1)))
              (clobber (reg:HI 2))
              (clobber (reg:HI 3))
              (clobber (reg:HI 4))
              (clobber (reg:HI 5))
              (clobber (reg:HI 12))
              (clobber (reg:CC CC_REGNUM))
	      ])
   ; Set the quotient (returned in register 0)
   (set (match_operand:HI 0 "register_operand" "") (reg:HI 0))
   ; Set the remainder (returned in register 1)
   (set (match_operand:HI 3 "register_operand" "") (reg:HI 1))]
  ""
{
  rtx fnName = gen_rtx_SYMBOL_REF (HImode, "_udivmodhi4");
  operands[4] = gen_call_for_divmod (gen_rtx_MEM (QImode, fnName), GEN_INT(0));
})

; Match a call to udivmodhi4.  As this is a call, the link register
; (r12), and registers r0-5 must be clobbered.  Ignore clobbering of
; r13/4 as these aren't used by the divide function).
(define_insn "*udivmodhi4_call"
  [(call (mem:QI (match_operand:HI 0 "immediate_operand" "i"))
	 (match_operand 1 "const_int_operand" ""))
   (set (reg:HI 0) (udiv:HI (reg:HI 0) (reg:HI 1)))
   (set (reg:HI 1) (umod:HI (reg:HI 0) (reg:HI 1)))
   (clobber (reg:HI 2))
   (clobber (reg:HI 3))
   (clobber (reg:HI 4))
   (clobber (reg:HI 5))
   (clobber (reg:HI 12))
   (clobber (reg:CC CC_REGNUM))]
  ""
  "JL (%0)\t// call %0%>"
  [(set_attr "length" "4")
   (set_attr "longConstant" "true")
   (set_attr "type" "call")])

(define_expand "udivmodsi4"
  [
   ; Make the function call
   (set (reg:SI 0) (match_operand:SI 1 "register_operand" ""))
   (set (reg:SI 2) (match_operand:SI 2 "register_operand" ""))
   (parallel [
     (match_dup 4)
     (set (reg:SI 4) (udiv:SI (reg:SI 0) (reg:SI 2)))
     (set (reg:SI 6) (umod:SI (reg:SI 0) (reg:SI 2)))
     (clobber (reg:SI 0))
     (clobber (reg:SI 2))
     (clobber (reg:HI 12))
   (clobber (reg:CC CC_REGNUM))])
   (set (match_operand:SI 0 "register_operand" "") (reg:SI 4))
   (set (match_operand:SI 3 "register_operand" "") (reg:SI 6))]
  ""
{
  rtx fnName = gen_rtx_SYMBOL_REF (HImode, "_udivmodsi4");
  operands[4] = gen_call_for_divmod (gen_rtx_MEM (QImode, fnName), GEN_INT(0));
})

(define_insn "*udivmodsi4_call"
  [(call (mem:QI (match_operand:HI 0 "immediate_operand" "i"))
	 (match_operand 1 "const_int_operand" ""))
   (set (reg:SI 4) (udiv:SI (reg:SI 0) (reg:SI 2)))
   (set (reg:SI 6) (umod:SI (reg:SI 0) (reg:SI 2)))
   (clobber (reg:SI 0))
   (clobber (reg:SI 2))
   (clobber (reg:HI 12))
   (clobber (reg:CC CC_REGNUM))]
  ""
  "JL (%0)\t// call %0%>"
  [(set_attr "length" "4")
   (set_attr "longConstant" "true")
   (set_attr "type" "call")])

(define_expand "divmodsi4"
  [
   ; Make the function call
   (set (reg:SI 0) (match_operand:SI 1 "register_operand" ""))
   (set (reg:SI 2) (match_operand:SI 2 "register_operand" ""))
   (parallel [
     (match_dup 4)
     (set (reg:SI 4) (div:SI (reg:SI 0) (reg:SI 2)))
     (set (reg:SI 6) (mod:SI (reg:SI 0) (reg:SI 2)))
     (clobber (reg:SI 0))
     (clobber (reg:SI 2))
     (clobber (reg:HI 12))
     (clobber (reg:CC CC_REGNUM))])
   (set (match_operand:SI 0 "register_operand" "") (reg:SI 4))
   (set (match_operand:SI 3 "register_operand" "") (reg:SI 6))]
  ""
{
  rtx fnName = gen_rtx_SYMBOL_REF (HImode, "_divmodsi4");
  operands[4] = gen_call_for_divmod (gen_rtx_MEM (QImode, fnName), GEN_INT(0));
})

(define_insn "*divmodsi4_call"
  [(call (mem:QI (match_operand:HI 0 "immediate_operand" "i"))
	 (match_operand 1 "const_int_operand" ""))
   (set (reg:SI 4) (div:SI (reg:SI 0) (reg:SI 2)))
   (set (reg:SI 6) (mod:SI (reg:SI 0) (reg:SI 2)))
   (clobber (reg:SI 0))
   (clobber (reg:SI 2))
   (clobber (reg:HI 12))
   (clobber (reg:CC CC_REGNUM))]
  ""
  "JL (%0)\t// call %0%>"
  [(set_attr "length" "4")
   (set_attr "longConstant" "true")
   (set_attr "type" "call")])

;;===========================================================================
;; Bitwise AND.  The QI/SI mode instructions are automatically
;; synthesised from the HI mode instruction.
;;===========================================================================

(define_insn "andhi3"
  [(set (match_operand:HI 0 "register_operand" "=r,r")
	(and:HI (match_operand:HI 1 "register_operand" "r,r")
		(match_operand:HI 2 "general_operand" "r,n")))
   (clobber (reg:CC CC_REGNUM))]
  ""
  "AND.%# %1,%2,%0 // %0 := %1 AND %2 (HI)"
  [(set_attr "type" "basicAlu,basicAlu")
   (set_attr "longConstant" "false,true")
   (set_attr "length" "3,5")])

;; If we peepholed the compare instruction out, we need to make sure the
;; "and" goes in slot 0. This pattern is just to accomplish that.

(define_insn "andhi3_with_use_clause"
  [(set (match_operand:HI 0 "register_operand" "=r,r")
        (and:HI (match_operand:HI 1 "register_operand" "r,r")
                (match_operand:HI 2 "general_operand" "r,n")))
   (set (reg:CC CC_REGNUM)
        (match_operator:CC 3 "picochip_peephole_comparison_operator"
                        [(const_int 0)
                         (const_int 0)]))]
  ""
  "AND.0 %1,%2,%0 // %0 := %1 AND %2 (HI)"
  [(set_attr "type" "picoAlu,picoAlu")
   (set_attr "longConstant" "false,true")
   (set_attr "length" "3,5")])

;;===========================================================================
;; Bitwise inclusive-OR.  The QI mode instruction is automatically
;; synthesised from the HI mode instruction.
;;===========================================================================

(define_insn "iorhi3"
  [(set (match_operand:HI 0 "register_operand" "=r,r")
	(ior:HI (match_operand:HI 1 "register_operand" "r,r")
		(match_operand:HI 2 "register_operand" "r,n")))
   (clobber (reg:CC CC_REGNUM))]
  ""
  "OR.%# %1,%2,%0 // %0 := %1 IOR %2 (HI)"
  [(set_attr "type" "basicAlu,basicAlu")
   (set_attr "longConstant" "false,true")
   (set_attr "length" "3,5")])

(define_insn "iorhi3_with_use_clause"
  [(set (match_operand:HI 0 "register_operand" "=r,r")
        (ior:HI (match_operand:HI 1 "register_operand" "r,r")
                (match_operand:HI 2 "general_operand" "r,n")))
   (set (reg:CC CC_REGNUM)
        (match_operator:CC 3 "picochip_peephole_comparison_operator"
                        [(const_int 0)
                         (const_int 0)]))]
  ""
  "OR.0 %1,%2,%0 // %0 := %1 IOR %2 (HI)"
  [(set_attr "type" "picoAlu,picoAlu")
   (set_attr "longConstant" "false,true")
   (set_attr "length" "3,5")])

;;===========================================================================
;; Bitwise exclusive-OR.  The QI/SI mode instructions are automatically
;; synthesised from the HI mode instruction.
;;===========================================================================

(define_insn "xorhi3"
  [(set (match_operand:HI 0 "register_operand" "=r,r")
	(xor:HI (match_operand:HI 1 "register_operand" "r,r")
		(match_operand:HI 2 "picochip_register_or_immediate_operand" "r,n")))
   (clobber (reg:CC CC_REGNUM))]
  ""
  "XOR.%# %1,%2,%0 // %0 := %1 XOR %2 (HI)"
  [(set_attr "type" "basicAlu,basicAlu")
   (set_attr "longConstant" "false,true")
   (set_attr "length" "3,5")])

;;===========================================================================
;; Arithmetic shift left.
;;===========================================================================

(define_insn "ashlhi3"
  [(set (match_operand:HI 0 "register_operand" "=r,r")
	(ashift:HI (match_operand:HI 1 "register_operand" "r,r")
		(match_operand:HI 2 "general_operand" "r,J")))]
  ""
  "LSL.%# %1,%2,%0 // %0 := %1 << %2"
  [(set_attr "type" "picoAlu,basicAlu")
   (set_attr "length" "3,3")])

;;===========================================================================
;; Arithmetic shift right.
;;===========================================================================

(define_insn "builtin_asri"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(ashiftrt:HI (match_operand:HI 1 "register_operand" "r")
		     (match_operand:HI 2 "immediate_operand" "")))
   (clobber (reg:CC CC_REGNUM))]
  ""
  "ASR.%# %1,%2,%0\t// %0 = %1 >>{arith} %2"
  [(set_attr "type" "basicAlu")
   (set_attr "length" "3")])

;; The picoChip ISA doesn't have a variable arithmetic shift right, so
;; synthesise it.  Shifts by constants are directly supported.

(define_expand "ashrhi3"
  [(match_operand:HI 0 "register_operand" "")
   (match_operand:HI 1 "register_operand" "")
   (match_operand:HI 2 "picochip_register_or_immediate_operand" "")]
  ""
{
  if (GET_CODE(operands[2]) == CONST_INT)
    /* Shift by constant is easy. */
    emit_insn (gen_builtin_asri (operands[0], operands[1], operands[2]));
  else
  {
    /* Synthesise a variable shift. */

    /* Fill a temporary with the sign bits. */
    rtx tmp1 = gen_reg_rtx (HImode);
    emit_insn (gen_builtin_asri (tmp1, operands[1], GEN_INT(15)));

    /* Shift the unsigned value. */
    rtx tmp2 = gen_reg_rtx (HImode);
    emit_insn (gen_lshrhi3 (tmp2, operands[1], operands[2]));

    /* The word of sign bits must be shifted back to the left, to zero
     * out the unwanted lower bits.  The amount to shift left by is (15 -
     * count). Since the shifts are computed modulo 16 (i.e., only the
     * lower 4 bits of the count are used), the shift amount (15 - count)
     * is equivalent to !count. */
    rtx tmp3 = gen_reg_rtx (HImode);
    rtx tmp3_1 = GEN_INT (-1);
    emit_insn (gen_xorhi3 (tmp3, operands[2], tmp3_1));
    rtx tmp4 = gen_reg_rtx (HImode);
    emit_insn (gen_ashlhi3 (tmp4, tmp1, tmp3));

    /* Combine the sign bits with the shifted value. */
    emit_insn (gen_iorhi3 (operands[0], tmp2, tmp4));

  }
  DONE;
})

;;===========================================================================
;; Logical shift right.
;;===========================================================================

(define_insn "lshrhi3"
  [(set (match_operand:HI 0 "register_operand" "=r,r")
	(lshiftrt:HI (match_operand:HI 1 "register_operand" "r,r")
		(match_operand:HI 2 "general_operand" "r,J")))]
  ""
  "LSR.%# %1,%2,%0 // %0 := %1 >> %2"
  [(set_attr "type" "picoAlu,basicAlu")
   (set_attr "length" "3,3")])

;;===========================================================================
;; Negate.
;;===========================================================================

;; Negations are performed by subtracting from the constant 0, which
;; is loaded into a register.  By using a register containing 0, the
;; chances of being able to CSE with another 0 value are increased.

(define_expand "neghi2"
  [(set (match_dup 2) (match_dup 3))
   (parallel [(set (match_operand:HI 0 "register_operand" "=r")
		   (minus:HI (match_dup 2)
			     (match_operand:HI 1 "register_operand" "r")))
	      (clobber (reg:CC CC_REGNUM))])]
  ""
  "operands[2] = gen_reg_rtx(HImode);
   operands[3] = GEN_INT(0x00000000);")

(define_expand "negsi2"
  [(set (match_dup 2) (match_dup 3))
   (parallel [(set (match_operand:SI 0 "register_operand" "=r")
		   (minus:SI (match_dup 2)
			     (match_operand:SI 1 "register_operand" "r")))
	      (clobber (reg:CC CC_REGNUM))])]
  ""
  "operands[2] = gen_reg_rtx(SImode);
   operands[3] = GEN_INT(0x00000000);")

;;===========================================================================
;; Absolute value. Taken from the Hacker's Delight, page 17. The second of the
;; four options given there produces the smallest, fastest code.
;;===========================================================================

(define_insn_and_split "abshi2"
  [(set (match_operand:HI 0 "register_operand" "")
   (abs:HI (match_operand:HI 1 "register_operand" "")))]
 ""
 "#"
 ""
 [(parallel [(set (match_dup 2)
                  (plus:HI (ashiftrt:HI (match_dup 1) (const_int 15))
			   (match_dup 1)))
             (clobber (reg:CC CC_REGNUM))])
  (parallel [(set (match_dup 0)
                  (xor:HI (ashiftrt:HI (match_dup 1) (const_int 15))
			  (match_dup 2)))
             (clobber (reg:CC CC_REGNUM))])]
{
  operands[2] = gen_reg_rtx (HImode);
})

;;===========================================================================
;; Bitwise complement.  Use auto-synthesised variant for SI mode. Though this
;; internally uses xor, the compiler doesnt automatically synthesize it using
;; xor, if this pattern was removed.
;;===========================================================================

(define_insn "one_cmplhi2"
  [(set (match_operand:HI 0 "register_operand" "=r")
        (not:HI (match_operand:HI 1 "register_operand" "0")))
   (clobber (reg:CC CC_REGNUM))]
  ""
  "XOR.%# %1,-1,%0 // %0 := ~%1"
  [(set_attr "type" "basicAlu")
   (set_attr "longConstant" "true")
   (set_attr "length" "5")])

;;===========================================================================
;; Count leading zeros. The special sign-bit-count instruction can be used
;; to help us here.
;;    op1:=clz(op1)
;; The code works by checking to see if the top bit is set. If it is,
;; then there are no leading zeros. If the top bit is cleared, then
;; the SBC instruction is used to determine how many more leading
;; zeros are present, and adding one more for the initial zero.
;;===========================================================================

(define_insn "clzhi2"
  [(set (match_operand:HI 0 "register_operand" "=&r")
	(clz:HI (match_operand:HI 1 "register_operand" "r")))]
  ""
  "// Count leading zeros\;SBC %1,%0\;ASR.0 %1,15,r15 %| ADD.1 %0,1,%0\;COPYNE 0,%0"
  [(set_attr "length" "11")])

;;===========================================================================
;; Count trailing zeros. This can be achieved efficiently by reversing
;; using the bitrev instruction, and then counting the leading zeros as
;; described above.
;;===========================================================================

(define_insn "ctzhi2"
  [(set (match_operand:HI 0 "register_operand" "=&r")
	(ctz:HI (match_operand:HI 1 "register_operand" "r")))]
  ""
  "// Count trailing zeros\;BREV %1,%0\;SBC %0,%0\;AND.0 %1,0x0001,r15 %| ADD.1 %0,1,%0\;COPYNE 0,%0"
  [(set_attr "length" "15")])

;;===========================================================================
;; Find the first set bit, starting from the least significant bit position.
;; This is very similar to the ctz function, except that the bit index is one
;; greater than the number of trailing zeros (i.e., SBC + 2), and the
;; result of ffs on the zero value is defined.
;;===========================================================================

(define_insn "ffshi2"
  [(set (match_operand:HI 0 "register_operand" "=&r")
	(ffs:HI (match_operand:HI 1 "register_operand" "r")))]
  ""
  "// First first bit\;BREV %1,%0\;SBC %0,%0\;AND.0 %1,0x0001,r15 %| ADD.1 %0,2,%0\;COPYNE 1,%0\;SUB.0 %1,0x0000,r15\;COPYEQ 0,%0"
  [(set_attr "length" "20")])

;;===========================================================================
;; Tablejump Instruction.  Jump to an absolute address.
;;===========================================================================

(define_insn "tablejump"
  [(set (pc) (unspec:HI [(match_operand:HI 0 "register_operand" "r")] 1))
   (use (label_ref (match_operand 1 "" "")))
   (clobber (match_dup 0))]
  ""
  "JR (%0)\t // Table jump to %0 %>"
  [(set_attr "length" "2")
   (set_attr "type" "realBranch")])

;; Given the memory address of a QImode value, and a scratch register,
;; store the memory operand into the given output operand.  The scratch
;; operand will not conflict with either of the operands.  The other
;; two operands may conflict with each other.

(define_insn "synthesised_loadqi_unaligned"
  [(set (match_operand:QI 0 "register_operand" "=r")
        (match_operand:QI 1 "memory_operand" "m"))
   (clobber (match_operand:HI 2 "register_operand" "=&r"))
   (clobber (reg:CC CC_REGNUM))]
  ""
  "// Synthesised loadqi %0 = Mem(%1) (Scratch %2)\n\tAND.0 %1,-2,%2\n\tLDW (%2)0,%0 %| AND.0 %1,1,%2\n\tLSL.0 %2,3,%2\n\tSUB.0 8,%2,%2\n\tLSL.0 %0,%2,%0\n\tASR.0 %0,8,%0"
  ; Approximate length only.  Probably a little shorter than this.
  [(set_attr "length" "40")])

;; Given a memory operand whose alignment is known (the HImode aligned
;; base is operand 0, and the number of bits by which to shift is in
;; operand 5),
(define_expand "synthesised_storeqi_aligned"
  [; s1 = mem_op
   (set (match_operand:HI 2 "register_operand" "")
	(match_operand:HI 0 "memory_operand" ""))
   ; s1 = s1 and mask
   (parallel [(set (match_dup 2) (and:HI (match_dup 2) (match_dup 5)))
   (clobber (reg:CC CC_REGNUM))])
   ; s2 = source << bitShift
   (set (match_dup 3)
	(ashift:HI (subreg:HI (match_operand:QI 1 "register_operand" "") 0)
		   (match_operand:HI 4 "const_int_operand" "")))
   ; s1 = s1 or s2
   (parallel [(set (match_dup 2) (ior:HI (match_dup 2) (match_dup 3)))
   (clobber (reg:CC CC_REGNUM))])
   ; mem_op = s1
   (set (match_dup 0) (match_dup 2))]
  "!TARGET_HAS_BYTE_ACCESS"
{
  /* Create the byte mask 0xFF00. */
  operands[5] = gen_int_mode(((~0xFF) >> INTVAL (operands[4])), HImode);
})

;; Reload instructions.  See picochip_secondary_reload for an
;; explanation of why an SI mode register is used as a scratch.  The
;; memory operand must be stored in a register (i.e., it can't be an
;; offset to another register - this would require another scratch
;; register into which the address of the offset could be computed).

(define_expand "reload_inqi"
  [(parallel [(match_operand:QI 0 "register_operand" "=&r")
              (match_operand:QI 1 "memory_operand" "m")
	      (match_operand:SI 2 "register_operand" "=&r")])]
  "!TARGET_HAS_BYTE_ACCESS"
{
  rtx scratch, seq;

  /* Get the scratch register.  Given an SI mode value, we have a
     choice of two HI mode scratch registers, so we can be sure that at
     least one of the scratch registers will be different to the output
     register, operand[0]. */

  if (REGNO (operands[0]) == REGNO (operands[2]))
    scratch = gen_rtx_REG (HImode, REGNO (operands[2]) + 1);
  else
    scratch = gen_rtx_REG (HImode, REGNO (operands[2]));

  /* Ensure that the scratch doesn't overlap either of the other
     two operands - however, the other two may overlap each
     other. */
  gcc_assert (REGNO(scratch) != REGNO(operands[0]));
  gcc_assert (REGNO(scratch) != REGNO(operands[1]));

  gcc_assert (GET_CODE (operands[1]) == MEM);

  if (picochip_word_aligned_memory_reference(XEXP(operands[1], 0)))
  {
    /* Aligned reloads are easy, since they can use word-loads. */
    seq = gen_synthesised_loadqi_aligned(operands[0], operands[1], scratch);
  }
  else
  {
    /* Emit the instruction using a define_insn. */
    seq = gen_synthesised_loadqi_unaligned(operands[0], operands[1], scratch);
  }
  emit_insn (seq);

  DONE;

})

(define_expand "reload_outqi"
  [(parallel [(match_operand 0 "memory_operand" "=m")
	      (match_operand:QI 1 "register_operand" "r")
	      (match_operand:SI 2 "register_operand" "=&r")])]
  "!TARGET_HAS_BYTE_ACCESS"
{
  rtx scratch1 = gen_rtx_REG(HImode, REGNO(operands[2]));
  rtx scratch2 = gen_rtx_REG(HImode, REGNO(operands[2]) + 1);
  rtx seq;

  gcc_assert (GET_CODE (operands[0]) == MEM);

  if (picochip_word_aligned_memory_reference(XEXP(operands[0], 0)))
    {
      rtx alignedAddr, bitShift;

      /* Convert the address of the known alignment into two operands
       * representing the aligned base address, and the number of shift bits
       * required to access the required value. */
      picochip_get_hi_aligned_mem(operands[0], &alignedAddr, &bitShift);

      /* Emit an aligned store of the source, with the given bit offset. */
      seq = gen_synthesised_storeqi_aligned(alignedAddr, operands[1], scratch1, scratch2, bitShift);

    }
  else
    {
      /* This isnt exercised at all. Moreover, with new devices, byte access
         is available in all variants. */
      gcc_unreachable();
    }

  emit_insn (seq);
  DONE;

})

;; Perform a byte load of an alignable memory operand.
; op0 = register to load. op1 = memory operand from which to load
; op2 = op1, aligned to HI, op3 = const bit shift required to extract byte,
; op4 = INTVAL(8 - op3)
(define_expand "synthesised_loadqi_aligned"
  [; Load memory operand into register
   (set (match_operand:HI 2 "register_operand" "=r")
	(match_dup 3))
   ; Shift required byte into top byte of word.
   (set (match_dup 2)
	(ashift:HI (match_dup 2)
		   (match_dup 4)))
   ; Arithmetic shift of byte to sign extend, and move to lowest register.
   (parallel[(set (subreg:HI (match_dup 0) 0)
	(ashiftrt:HI (match_dup 2) 
		     (const_int 8)))
   (clobber (reg:CC CC_REGNUM))])
   (use (match_operand:QI 1 "picochip_alignable_memory_operand" "g"))]
  "!TARGET_HAS_BYTE_ACCESS"
{
  rtx alignedAddr, bitShift;

  /* Convert the address of the known alignment into two operands
   * representing the aligned base address, and the number of shift bits
   * required to access the required value. */
  picochip_get_hi_aligned_mem(operands[1], &alignedAddr, &bitShift);

  operands[3] = alignedAddr;
  operands[4] = GEN_INT(8 - INTVAL(bitShift));
})

;;============================================================================
;; Special instructions.
;;============================================================================

; Count sign-bits.
(define_insn "sbc"
  [(set (match_operand:HI             0 "register_operand" "=r")
	(unspec:HI [(match_operand:HI 1 "register_operand" "r")]
		   UNSPEC_SBC))]
  ""
  "SBC %1,%0\t\t// %0 := SBC(%1)"
  [(set_attr "type" "picoAlu")
   (set_attr "length" "2")])

; Bit reversal.
(define_insn "brev"
  [(set (match_operand:HI             0 "register_operand" "=r")
	(unspec:HI [(match_operand:HI 1 "register_operand" "r")]
		   UNSPEC_BREV))]
  ""
  "BREV %1,%0\t\t// %0 := BREV(%1)"
  [(set_attr "length" "2")
   (set_attr "type" "picoAlu")])

; Byte swap.
(define_insn "bswaphi2"
  [(set (match_operand:HI             0 "register_operand" "=r")
	(bswap:HI (match_operand:HI 1 "register_operand" "r")))]
  ""
  "BYTESWAP %1,%0\t\t// %0 := ByteSwap(%1)"
  [(set_attr "length" "2")
   (set_attr "type" "picoAlu")])

; Read status word.
(define_insn "copysw"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(unspec_volatile:HI [(reg:CC CC_REGNUM)] UNSPEC_COPYSW))]
  ""
  "COPYSW.%# %0\t// %0 := Flags"
  [(set_attr "type" "basicAlu")
   (set_attr "length" "2")])

; Saturating addition.
(define_insn "sataddhi3"
  [(set (match_operand:HI             0 "register_operand" "=r")
	(unspec:HI [(match_operand:HI 1 "register_operand" "r")
		    (match_operand:HI 2 "register_operand" "r")]
		   UNSPEC_ADDS))
   (clobber (reg:CC CC_REGNUM))]
  ""
  "ADDS %1,%2,%0\t// %0 := sat(%1 + %2)"
  [(set_attr "type" "picoAlu")
   (set_attr "length" "3")])

; Saturating subtraction.
(define_insn "satsubhi3"
  [(set (match_operand:HI             0 "register_operand" "=r")
	(unspec:HI [(match_operand:HI 1 "register_operand" "r")
		    (match_operand:HI 2 "register_operand" "r")]
		   UNSPEC_SUBS))
   (clobber (reg:CC CC_REGNUM))]
  ""
  "SUBS %1,%2,%0\t// %0 := sat(%1 - %2)"
  [(set_attr "type" "picoAlu")
   (set_attr "length" "3")])

(define_insn "halt"
  [(unspec_volatile [(match_operand:HI 0 "const_int_operand" "i")]
	UNSPEC_HALT)]
  ""
  "HALT\t// (id %0)"
  [(set_attr "length" "1")
   (set_attr "type" "unknown")])

(define_insn "internal_testport"
  [(set (reg:CC CC_REGNUM)
        (unspec_volatile:CC [(match_operand:HI 0 "const_int_operand" "i")]
           UNSPEC_INTERNAL_TESTPORT))]
  ""
  "TSTPORT %0"
  [(set_attr "length" "2")
   (set_attr "longConstant" "false")
   (set_attr "type" "picoAlu")])

;;============================================================================
;; Communications builtins.
;;
;; Each builtin comes in two forms: a single port version, which maps
;; to a single instruction, and an array port version.  The array port
;; version is treated as a special type of instruction, which is then
;; split into a number of smaller instructions, if the index of the
;; port can't be converted into a constant.  When the RTL split is
;; performed, a function call is emitted, in which the index of the
;; port to use is used to compute the address of the function to call
;; (i.e., each array port is a function in its own right, and the
;; functions are stored as an array which is then indexed to determine
;; the correct function). The communication function port array is
;; created by the linker if and only if it is required (in a
;; collect2-like manner).
;;============================================================================

; Simple scalar get.
(define_insn "commsGet"
  [(set (match_operand:SI             0 "register_operand" "=r")
	(unspec_volatile:SI
	 [(match_operand:HI 1 "immediate_operand" "n")]
	 UNSPEC_GET))]
  ""
  "GET %1,%R0\t// %R0 := PORT(%1)"
  [(set_attr "type" "comms")
   (set_attr "length" "2")])

; Entry point for array get (the actual port index is computed as the
; sum of the index, and the base).
;
; op0 - Destination
; op1 - Requested port index
; op2 - size of port array (constant)
; op3 - base index of port array (constant)

(define_expand "commsArrayGet"
  [(parallel
      [(set (reg:SI 0)
            (unspec_volatile:SI [(match_operand:HI 1 "general_operand" "")
	 	     	 (match_operand:HI 2 "immediate_operand" "")
		     	 (match_operand:HI 3 "immediate_operand" "")]
	 	UNSPEC_CALL_GET_ARRAY))
       (clobber (reg:HI LINK_REGNUM))])
   (set (match_operand:SI 0 "register_operand" "") (reg:SI 0))]
  ""
  "")

;; The actual array get instruction. When the array index is a constant,
;; an exact instruction may be generated. When the index is variable,
;; a call to a special function is generated. This code could be
;; split into individual RTL instructions, but it is so rarely
;; used, that we won't bother.
(define_insn "*commsArrayGetInstruction"
  [(set (reg:SI 0)
        (unspec_volatile:SI [(match_operand:HI 0 "general_operand" "r,i")
	 	     (match_operand:HI 1 "immediate_operand" "")
		     (match_operand:HI 2 "immediate_operand" "")]
	 	UNSPEC_CALL_GET_ARRAY))
   (clobber (reg:HI LINK_REGNUM))]
  ""
{
  return picochip_output_get_array (which_alternative, operands);
})

; Scalar Put instruction.
(define_insn "commsPut"
  [(unspec_volatile [(match_operand:HI 0 "const_int_operand" "")
		     (match_operand:SI 1 "register_operand" "r")]
		    UNSPEC_PUT)]
  ""
  "PUT %R1,%0\t// PORT(%0) := %R1"
  [(set_attr "type" "comms")
   (set_attr "length" "2")])

; Entry point for array put. The operands accepted are:
;   op0 - Value to put
;   op1 - Requested port index
;   op2 - size of port array
;   op3 - base index of port array
; The arguments are marshalled into the fixed registers, so that
; the actual put instruction can expand into a call if necessary
; (e.g., if the index is variable at run-time).

(define_expand "commsArrayPut"
  [(set (reg:SI 0) (match_operand:SI 0 "general_operand" ""))
   (parallel
      [(unspec_volatile [(match_operand:HI 1 "general_operand" "")
	 	     	 (match_operand:HI 2 "immediate_operand" "")
		     	 (match_operand:HI 3 "immediate_operand" "")]
	 	UNSPEC_CALL_PUT_ARRAY)
       (use (reg:SI 0))
       (clobber (reg:HI LINK_REGNUM))])]
  ""
  "")

;; The actual array put instruction. When the array index is a constant,
;; an exact instruction may be generated. When the index is variable,
;; a call to a special function is generated. This code could be
;; split into individual RTL instructions, but it is so rarely
;; used, that we won't bother.
(define_insn "*commsArrayPutInstruction"
  [(unspec_volatile [(match_operand:HI 0 "general_operand" "r,i")
	 	     (match_operand:HI 1 "immediate_operand" "")
		     (match_operand:HI 2 "immediate_operand" "")]
	 	UNSPEC_CALL_PUT_ARRAY)
   (use (reg:SI 0))
   (clobber (reg:HI LINK_REGNUM))]
  ""
{
  return picochip_output_put_array (which_alternative, operands);
})

;; Scalar test port instruction.
(define_insn "commsTestPort"
  [(set (match_operand:HI             0 "register_operand" "=r")
	(unspec_volatile:HI [(match_operand:HI 1 "const_int_operand" "")]
		   UNSPEC_TESTPORT))
   (clobber (reg:CC CC_REGNUM))]
  ""
  "// %0 := TestPort(%1)\;TSTPORT %1\;COPYSW.0 %0\;AND.0 %0,8,%0"
  [(set_attr "length" "9")])

; Entry point for array tstport (the actual port index is computed as the
; sum of the index, and the base).
;
; op0 - Test value.
; op1 - Requested port index
; op2 - size of port array (constant)
; op3 - base index of port array (constant)

(define_expand "commsArrayTestPort"
  [(parallel
      [(set (match_operand:HI 0 "register_operand" "")
            (unspec_volatile:HI [(match_operand:HI 1 "general_operand" "")
	 	              (match_operand:HI 2 "immediate_operand" "")
		     	      (match_operand:HI 3 "immediate_operand" "")]
	 	UNSPEC_CALL_TESTPORT_ARRAY))
       (clobber (reg:HI LINK_REGNUM))])]
  ""
  "")

;; The actual array testport instruction. When the array index is a constant,
;; an exact instruction may be generated. When the index is variable,
;; a call to a special function is generated. This code could be
;; split into individual RTL instructions, but it is so rarely
;; used, that we won't bother.
(define_insn "*commsArrayTestportInstruction"
  [(set (match_operand:HI 0 "register_operand" "=r,r")
        (unspec_volatile:HI [(match_operand:HI 1 "general_operand" "r,i")
	 	     	  (match_operand:HI 2 "immediate_operand" "")
		     	  (match_operand:HI 3 "immediate_operand" "")]
	 	UNSPEC_CALL_TESTPORT_ARRAY))
   (clobber (reg:HI LINK_REGNUM))]
  ""
{
  return picochip_output_testport_array (which_alternative, operands);
})

;; Merge a TSTPORT instruction with the branch to which it
;; relates.  Often the TSTPORT function (generated by a built-in), is
;; used to control conditional execution.  The normal sequence of
;; instructions would be:
;;    TSTPORT p
;;    COPYSW temp
;;    AND temp, 0x0008, temp
;;    SUB temp,0,discard
;;    BEQ label
;; This can be made more efficient by detecting the special case where
;; the result of a TSTPORT is used to branch, to allow the following
;; RTL sequence to be generated instead:
;;    TSTPORT p
;;    BEQ label
;; A big saving in cycles and bytes!

(define_insn_and_split "tstport_branch"
 [(set (pc)
	(if_then_else
	    (match_operator 0 "comparison_operator"
	                    [(unspec_volatile:HI
				[(match_operand:HI 1 "const_int_operand" "")]
					   UNSPEC_TESTPORT)
			     (const_int 0)])
            (label_ref       (match_operand    2 "" ""))
	    (pc)))
   (clobber (reg:CC CC_REGNUM))]
 ""
 "#"
 ""
 [(set (reg:CC CC_REGNUM)
       (unspec_volatile:CC [(match_dup 1)] UNSPEC_INTERNAL_TESTPORT))
  (parallel [(set (pc)
                  (if_then_else
                       (match_op_dup:HI 4 [(reg:CC CC_REGNUM) (const_int 0)])
				(label_ref (match_dup 2))
				(pc)))
	     (use (match_dup 3))])]
 "{
    /* Note that the sense of the branch is reversed, since we are
     * comparing flag != 0. */
    gcc_assert (GET_CODE(operands[0]) == NE || GET_CODE(operands[0]) == EQ);
    operands[4] = gen_rtx_fmt_ee(reverse_condition(GET_CODE(operands[0])),
                  GET_MODE(operands[0]), XEXP(operands[0], 0), XEXP(operands[0], 1));
    operands[3] = GEN_INT (0);
  }")

;;============================================================================
;; Epilogue/Epilogue expansion.
;;============================================================================

(define_expand "prologue"
  [(clobber (const_int 0))]
  ""
{
  picochip_expand_prologue ();
  DONE;
})

(define_expand "epilogue"
  [(use (const_int 0))]
  ""
{
  picochip_expand_epilogue (FALSE);
  DONE;
})

;;============================================================================
;; Trap instruction. This is used to indicate an error. For the
;; picoChip processors this is handled by calling a HALT instruction,
;; which stops the processor.
;;============================================================================

(define_insn "trap"
  [(trap_if (const_int 1) (const_int 6))]
  ""
  "HALT\t// (Trap)"
  [(set_attr "length" "2")])

;;============================================================================
;; Conditional copy instructions.  Only equal/not-equal comparisons are
;; supported.  All other types of comparison remain as branch
;; sequences.
;;============================================================================

;; Define expand seems to consider the resulting two instructions to be
;; independent. It was moving the actual copy instruction further down
;; with a call instruction in between. The call was clobbering the CC
;; and hence the cond_copy was wrong. With a split, it works correctly.
(define_expand "movhicc"
  [(set (reg:CC CC_REGNUM) (match_operand 1 "comparison_operator" ""))
   (parallel [(set (match_operand:HI 0 "register_operand" "=r,r")
                   (if_then_else:HI (match_op_dup:HI 1 [(reg:CC CC_REGNUM) (const_int 0)])
                                 (match_operand:HI 2 "picochip_register_or_immediate_operand" "0,0")
                                 (match_operand:HI 3 "picochip_register_or_immediate_operand" "r,i")))
              (use (match_dup 4))])]
  ""
  {if (!picochip_check_conditional_copy (operands))
     FAIL;
   operands[4] = GEN_INT(GET_CODE(operands[1]));
  })

;; We dont do any checks here. But this pattern is used only when movhicc 
;; was checked. Put a "use" clause to make sure.
(define_insn "*conditional_copy"
  [(set (match_operand:HI 0 "register_operand" "=r,r")
	(if_then_else:HI
            (match_operator:HI 4 "picochip_peephole_comparison_operator"
                 [(reg:CC CC_REGNUM) (const_int 0)])
	 (match_operand:HI 1 "picochip_register_or_immediate_operand" "0,0")
	 (match_operand:HI 2 "picochip_register_or_immediate_operand" "r,i")))
   (use (match_operand:HI 3 "const_int_operand" ""))]
  ""
{

  gcc_assert (GET_CODE(operands[4]) == EQ || GET_CODE(operands[4]) == NE);
  /* Note that the comparison is reversed as the pattern matches
     the *else* part of the if_then_else */
  switch (GET_CODE(operands[4]))
    {
    case EQ: return "COPYNE %2,%0\t// if (NE) %0 := %2";
    case NE: return "COPYEQ %2,%0\t// if (EQ) %0 := %2";
    default:
      gcc_unreachable();
    }
}
  [(set_attr "length" "2")
   (set_attr "type" "picoAlu,picoAlu")
   (set_attr "longConstant" "false,true")])

;;============================================================================
;; Scheduling, including delay slot scheduling.
;;============================================================================

(automata_option "v")
(automata_option "ndfa")

;; Define each VLIW slot as a CPU resource.  Note the three flavours of
;; branch.  `realBranch' is an actual branch instruction.  `macroBranch'
;; is a directive to the assembler, which may expand into multiple
;; instructions.  `call' is an actual branch instruction, but one which
;; sets the link register, and hence can't be scheduled alongside
;; other instructions which set the link register.  When the DFA
;; scheduler is fixed to prevent it scheduling a JL with an R12
;; setting register, the call type branches can be replaced by
;; realBranch types instead.

(define_attr "type"
  "picoAlu,basicAlu,nonCcAlu,mem,call,realBranch,macroBranch,mul,mac,app,comms,unknown"
  (const_string "unknown"))

(define_attr "schedType" "none,space,speed"
  (const (symbol_ref "picochip_schedule_type")))

;; Define whether an instruction uses a long constant.

(define_attr "longConstant"
  "true,false" (const_string "false"))

;; Define three EU slots.
(define_query_cpu_unit "slot0,slot1,slot2")

;; Pull in the pipeline descriptions for speed or space scheduling.
(include "dfa_speed.md")
(include "dfa_space.md")

; Unknown instructions are assumed to take a single cycle, and use all
; slots.  This enables them to actually output a sequence of
; instructions without any limitation.  For the purposes of
; scheduling, unknown instructions are a pain, and should be removed
; completely.  This means that RTL patterns should always be used to
; reduce complex sequences of instructions to individual instructions.
(define_insn_reservation "unknownInsn" 1
  (eq_attr "type" "unknown")
  "(slot0+slot1+slot2)")

; Allow any non-branch instructions to be placed in the branch
; slot. Branch slots are always executed.
(define_delay (eq_attr "type" "realBranch,call")
  [(eq_attr "type" "!realBranch,macroBranch,call,unknown") (nil) (nil)])
