;; Predicate definitions for CELL SPU
;; Copyright (C) 2006, 2007 Free Software Foundation, Inc.
;;
;; This file is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3 of the License, or (at your option) 
;; any later version.

;; This file is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;; Return 1 if operand is constant zero of its mode
(define_predicate "const_zero_operand"
  (and (match_code "const_int,const,const_double,const_vector")
       (match_test "op == CONST0_RTX (mode)")))

(define_predicate "const_one_operand"
  (and (match_code "const_int,const,const_double,const_vector")
       (match_test "op == CONST1_RTX (mode)")))

(define_predicate "spu_reg_operand"
  (and (match_operand 0 "register_operand")
       (ior (not (match_code "subreg"))
            (match_test "valid_subreg (op)"))))

(define_predicate "spu_nonimm_operand"
  (and (match_operand 0 "nonimmediate_operand")
       (ior (not (match_code "subreg"))
            (match_test "valid_subreg (op)"))))

(define_predicate "spu_nonmem_operand"
  (and (match_operand 0 "nonmemory_operand")
       (ior (not (match_code "subreg"))
            (match_test "valid_subreg (op)"))))

(define_predicate "spu_mov_operand"
  (ior (match_operand 0 "memory_operand")
       (match_operand 0 "spu_nonmem_operand")))

(define_predicate "spu_dest_operand"
  (ior (match_operand 0 "memory_operand")
       (match_operand 0 "spu_reg_operand")))

(define_predicate "call_operand"
  (and (match_code "mem")
       (match_test "(!TARGET_LARGE_MEM && satisfies_constraint_S (op))
		    || (satisfies_constraint_R (op)
			&& REGNO (XEXP (op, 0)) != FRAME_POINTER_REGNUM
			&& REGNO (XEXP (op, 0)) != ARG_POINTER_REGNUM
			&& (REGNO (XEXP (op, 0)) < FIRST_PSEUDO_REGISTER
			    || REGNO (XEXP (op, 0)) > LAST_VIRTUAL_REGISTER))")))

(define_predicate "vec_imm_operand"
  (and (match_code "const_int,const_double,const_vector")
       (match_test "spu_legitimate_constant_p (mode, op)")))

(define_predicate "spu_arith_operand"
  (match_code "reg,subreg,const_int,const_vector")
  {
    if (spu_reg_operand (op, mode))
      return 1;
    if (GET_CODE (op) == CONST_INT || GET_CODE (op) == CONST_VECTOR)
      return arith_immediate_p (op, mode, -0x200, 0x1ff);
    return 0;
  })

(define_predicate "spu_logical_operand"
  (match_code "reg,subreg,const_int,const_double,const_vector")
  {
    if (spu_reg_operand (op, mode))
      return 1;
    if (GET_CODE (op) == CONST_INT || GET_CODE (op) == CONST_DOUBLE
	|| GET_CODE (op) == CONST_VECTOR)
      return logical_immediate_p (op, mode);
    return 0;
  })

(define_predicate "spu_ior_operand"
  (match_code "reg,subreg,const_int,const_double,const_vector")
  {
    if (spu_reg_operand (op, mode))
      return 1;
    if (GET_CODE (op) == CONST_INT || GET_CODE (op) == CONST_DOUBLE
	|| GET_CODE (op) == CONST_VECTOR)
      return logical_immediate_p (op, mode)
	     || iohl_immediate_p (op, mode);
    return 0;
  })

(define_predicate "imm_K_operand"
  (and (match_code "const_int")
       (match_test "arith_immediate_p (op, mode, -0x200, 0x1ff)")))

;; Return 1 if OP is a comparison operation that is valid for a branch insn.
;; We only check the opcode against the mode of the register value here. 
(define_predicate "branch_comparison_operator"
  (and (match_code "eq,ne")
       (ior (match_test "GET_MODE (XEXP (op, 0)) == HImode")
	    (match_test "GET_MODE (XEXP (op, 0)) == SImode"))))

(define_predicate "spu_inv_exp2_operand"
  (and (match_code "const_double,const_vector")
       (and (match_operand 0 "immediate_operand")
	    (match_test "exp2_immediate_p (op, mode, -126, 0)"))))

(define_predicate "spu_exp2_operand"
  (and (match_code "const_double,const_vector")
       (and (match_operand 0 "immediate_operand")
	    (match_test "exp2_immediate_p (op, mode, 0, 127)"))))

(define_predicate "shiftrt_operator"
  (match_code "lshiftrt,ashiftrt"))

(define_predicate "extend_operator"
  (match_code "sign_extend,zero_extend"))

