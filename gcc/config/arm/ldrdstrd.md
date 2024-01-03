;; ARM ldrd/strd peephole optimizations.
;;
;; Copyright (C) 2013-2024 Free Software Foundation, Inc.
;;
;; Written by Greta Yorsh <greta.yorsh@arm.com>

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

;; The following peephole optimizations identify consecutive memory
;; accesses, and try to rearrange the operands to enable generation of
;; ldrd/strd.
;;
;; In many cases they behave in the same way that patterns in ldmstm.md behave,
;; but there is extra logic in gen_operands_ldrd_strd to try and ensure the
;; registers used are an (r<N>, r<N + 1>) pair where N is even.

(define_peephole2 ; ldrd
  [(set (match_operand:SI 0 "arm_general_register_operand" "")
	(match_operand:SI 2 "memory_operand" ""))
   (set (match_operand:SI 1 "arm_general_register_operand" "")
	(match_operand:SI 3 "memory_operand" ""))]
  "TARGET_LDRD"
  [(parallel [(set (match_dup 0) (match_dup 2))
	      (set (match_dup 1) (match_dup 3))])]
{
  if (!gen_operands_ldrd_strd (operands, true, false, false))
    FAIL;
})

(define_peephole2 ; strd
  [(set (match_operand:SI 2 "memory_operand" "")
	(match_operand:SI 0 "arm_general_register_operand" ""))
   (set (match_operand:SI 3 "memory_operand" "")
	(match_operand:SI 1 "arm_general_register_operand" ""))]
  "TARGET_LDRD"
  [(parallel [(set (match_dup 2) (match_dup 0))
	      (set (match_dup 3) (match_dup 1))])]
{
  if (!gen_operands_ldrd_strd (operands, false, false, false))
    FAIL;
})

;; The following peepholes reorder registers to enable LDRD/STRD.
(define_peephole2 ; strd of constants
  [(set (match_operand:SI 0 "arm_general_register_operand" "")
	(match_operand:SI 4 "const_int_operand" ""))
   (set (match_operand:SI 2 "memory_operand" "")
	(match_dup 0))
   (set (match_operand:SI 1 "arm_general_register_operand" "")
	(match_operand:SI 5 "const_int_operand" ""))
   (set (match_operand:SI 3 "memory_operand" "")
	(match_dup 1))]
  "TARGET_LDRD"
  [(set (match_dup 0) (match_dup 4))
   (set (match_dup 1) (match_dup 5))
   (parallel [(set (match_dup 2) (match_dup 0))
	      (set (match_dup 3) (match_dup 1))])]
{
  if (!gen_operands_ldrd_strd (operands, false, true, false))
    FAIL;
})

(define_peephole2 ; strd of constants
  [(set (match_operand:SI 0 "arm_general_register_operand" "")
	(match_operand:SI 4 "const_int_operand" ""))
   (set (match_operand:SI 1 "arm_general_register_operand" "")
	(match_operand:SI 5 "const_int_operand" ""))
   (set (match_operand:SI 2 "memory_operand" "")
	(match_dup 0))
   (set (match_operand:SI 3 "memory_operand" "")
	(match_dup 1))]
  "TARGET_LDRD"
  [(set (match_dup 0) (match_dup 4))
   (set (match_dup 1) (match_dup 5))
   (parallel [(set (match_dup 2) (match_dup 0))
	      (set (match_dup 3) (match_dup 1))])]
{
  if (!gen_operands_ldrd_strd (operands, false, true, false))
     FAIL;
})

;; The following two peephole optimizations are only relevant for ARM
;; mode where LDRD/STRD require consecutive registers.

(define_peephole2 ; swap the destination registers of two loads
		  ; before a commutative operation.
  [(set (match_operand:SI 0 "arm_general_register_operand" "")
	(match_operand:SI 2 "memory_operand" ""))
   (set (match_operand:SI 1 "arm_general_register_operand" "")
	(match_operand:SI 3 "memory_operand" ""))
   (set (match_operand:SI 4 "arm_general_register_operand" "")
	(match_operator:SI 5 "commutative_binary_operator"
			   [(match_operand 6 "arm_general_register_operand" "")
			    (match_operand 7 "arm_general_register_operand" "") ]))]
  "TARGET_LDRD && TARGET_ARM
   && (  ((rtx_equal_p(operands[0], operands[6])) && (rtx_equal_p(operands[1], operands[7])))
	||((rtx_equal_p(operands[0], operands[7])) && (rtx_equal_p(operands[1], operands[6]))))
   && (peep2_reg_dead_p (3, operands[0]) || rtx_equal_p (operands[0], operands[4]))
   && (peep2_reg_dead_p (3, operands[1]) || rtx_equal_p (operands[1], operands[4]))"
  [(parallel [(set (match_dup 0) (match_dup 2))
	      (set (match_dup 1) (match_dup 3))])
   (set (match_dup 4) (match_op_dup 5 [(match_dup 6) (match_dup 7)]))]
{
  if (!gen_operands_ldrd_strd (operands, true, false, true))
    FAIL;
})

(define_peephole2 ; swap the destination registers of two loads
		  ; before a commutative operation that sets the flags.
  [(set (match_operand:SI 0 "arm_general_register_operand" "")
	(match_operand:SI 2 "memory_operand" ""))
   (set (match_operand:SI 1 "arm_general_register_operand" "")
	(match_operand:SI 3 "memory_operand" ""))
   (parallel
      [(set (match_operand:SI 4 "arm_general_register_operand" "")
	    (match_operator:SI 5 "commutative_binary_operator"
			       [(match_operand 6 "arm_general_register_operand" "")
				(match_operand 7 "arm_general_register_operand" "") ]))
       (clobber (reg:CC CC_REGNUM))])]
  "TARGET_LDRD && TARGET_ARM
   && (  ((rtx_equal_p(operands[0], operands[6])) && (rtx_equal_p(operands[1], operands[7])))
       ||((rtx_equal_p(operands[0], operands[7])) && (rtx_equal_p(operands[1], operands[6]))))
   && (peep2_reg_dead_p (3, operands[0]) || rtx_equal_p (operands[0], operands[4]))
   && (peep2_reg_dead_p (3, operands[1]) || rtx_equal_p (operands[1], operands[4]))"
  [(parallel [(set (match_dup 0) (match_dup 2))
	      (set (match_dup 1) (match_dup 3))])
   (parallel
      [(set (match_dup 4)
	    (match_op_dup 5 [(match_dup 6) (match_dup 7)]))
       (clobber (reg:CC CC_REGNUM))])]
{
  if (!gen_operands_ldrd_strd (operands, true, false, true))
    FAIL;
})

;; TODO: Handle LDRD/STRD with writeback:
;; (a) memory operands can be POST_INC, POST_DEC, PRE_MODIFY, POST_MODIFY
;; (b) Patterns may be followed by an update of the base address.


;; insns matching the LDRD/STRD patterns that will get created by the above
;; peepholes.
;; We use gen_operands_ldrd_strd() with a modify argument as false so that the
;; operands are not changed.
(define_insn "*arm_ldrd"
  [(parallel [(set (match_operand:SI 0 "s_register_operand" "=r")
		   (match_operand:SI 2 "memory_operand" "m"))
	      (set (match_operand:SI 1 "s_register_operand" "=rk")
		   (match_operand:SI 3 "memory_operand" "m"))])]
  "TARGET_LDRD && TARGET_ARM && reload_completed
  && valid_operands_ldrd_strd (operands, true)"
  {
    rtx op[2];
    op[0] = gen_rtx_REG (DImode, REGNO (operands[0]));
    op[1] = adjust_address (operands[2], DImode, 0);
    return output_move_double (op, true, NULL);
  }
  [(set (attr "length")
	(symbol_ref "arm_count_ldrdstrd_insns (operands, true) * 4"))
   (set (attr "ce_count") (symbol_ref "get_attr_length (insn) / 4"))
   (set_attr "type" "load_8")
   (set_attr "predicable" "yes")]
)

(define_insn "*arm_strd"
  [(parallel [(set (match_operand:SI 2 "memory_operand" "=m")
		   (match_operand:SI 0 "s_register_operand" "r"))
	      (set (match_operand:SI 3 "memory_operand" "=m")
		   (match_operand:SI 1 "s_register_operand" "rk"))])]
  "TARGET_LDRD && TARGET_ARM && reload_completed
  && valid_operands_ldrd_strd (operands, false)"
  {
    rtx op[2];
    op[0] = adjust_address (operands[2], DImode, 0);
    op[1] = gen_rtx_REG (DImode, REGNO (operands[0]));
    return output_move_double (op, true, NULL);
  }
  [(set (attr "length")
	(symbol_ref "arm_count_ldrdstrd_insns (operands, false) * 4"))
   (set (attr "ce_count") (symbol_ref "get_attr_length (insn) / 4"))
   (set_attr "type" "store_8")
   (set_attr "predicable" "yes")]
)
