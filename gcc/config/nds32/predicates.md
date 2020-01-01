;; Predicate definitions of Andes NDS32 cpu for GNU compiler
;; Copyright (C) 2012-2020 Free Software Foundation, Inc.
;; Contributed by Andes Technology Corporation.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

(define_predicate "nds32_equality_comparison_operator"
  (match_code "eq,ne"))

(define_predicate "nds32_greater_less_comparison_operator"
  (match_code "gt,ge,lt,le"))

(define_predicate "nds32_float_comparison_operator"
  (match_code "eq,ne,le,lt,ge,gt,ordered,unordered,ungt,unge,unlt,unle"))

(define_predicate "nds32_movecc_comparison_operator"
  (match_code "eq,ne,le,leu,ge,geu"))

(define_special_predicate "nds32_logical_binary_operator"
  (match_code "and,ior,xor"))

(define_special_predicate "nds32_conditional_call_comparison_operator"
  (match_code "lt,ge"))

(define_special_predicate "nds32_have_33_inst_operator"
  (match_code "mult,and,ior,xor"))

(define_predicate "nds32_symbolic_operand"
  (and (match_code "const,symbol_ref,label_ref")
       (match_test "!(TARGET_ICT_MODEL_LARGE
		      && nds32_indirect_call_referenced_p (op))")))

(define_predicate "nds32_nonunspec_symbolic_operand"
  (and (match_code "const,symbol_ref,label_ref")
       (match_test "!flag_pic && nds32_const_unspec_p (op)
		    && !(TARGET_ICT_MODEL_LARGE
			 && nds32_indirect_call_referenced_p (op))")))

(define_predicate "nds32_reg_constant_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "const_int_operand")))

(define_predicate "nds32_rimm15s_operand"
  (ior (match_operand 0 "register_operand")
       (and (match_operand 0 "const_int_operand")
	    (match_test "satisfies_constraint_Is15 (op)"))))

(define_predicate "nds32_rimm11s_operand"
  (ior (match_operand 0 "register_operand")
       (and (match_operand 0 "const_int_operand")
	    (match_test "satisfies_constraint_Is11 (op)"))))

(define_predicate "nds32_imm_0_1_operand"
  (and (match_operand 0 "const_int_operand")
       (ior (match_test "satisfies_constraint_Iv00 (op)")
	    (match_test "satisfies_constraint_Iv01 (op)"))))

(define_predicate "nds32_imm_1_2_operand"
  (and (match_operand 0 "const_int_operand")
       (ior (match_test "satisfies_constraint_Iv01 (op)")
	    (match_test "satisfies_constraint_Iv02 (op)"))))

(define_predicate "nds32_imm_1_2_4_8_operand"
  (and (match_operand 0 "const_int_operand")
       (ior (ior (match_test "satisfies_constraint_Iv01 (op)")
		 (match_test "satisfies_constraint_Iv02 (op)"))
	    (ior (match_test "satisfies_constraint_Iv04 (op)")
		 (match_test "satisfies_constraint_Iv08 (op)")))))

(define_predicate "nds32_imm2u_operand"
  (and (match_operand 0 "const_int_operand")
       (match_test "satisfies_constraint_Iu02 (op)")))

(define_predicate "nds32_imm4u_operand"
  (and (match_operand 0 "const_int_operand")
       (match_test "satisfies_constraint_Iu04 (op)")))

(define_predicate "nds32_imm5u_operand"
  (and (match_operand 0 "const_int_operand")
       (match_test "satisfies_constraint_Iu05 (op)")))

(define_predicate "nds32_imm6u_operand"
  (and (match_operand 0 "const_int_operand")
       (match_test "satisfies_constraint_Iu06 (op)")))

(define_predicate "nds32_rimm4u_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "nds32_imm4u_operand")))

(define_predicate "nds32_rimm5u_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "nds32_imm5u_operand")))

(define_predicate "nds32_rimm6u_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "nds32_imm6u_operand")))

(define_predicate "nds32_move_operand"
  (and (match_operand 0 "general_operand")
       (not (match_code "high,const,symbol_ref,label_ref")))
{
  /* If the constant op does NOT satisfy Is20 nor Ihig,
     we cannot perform move behavior by a single instruction.  */
  if (CONST_INT_P (op)
      && !satisfies_constraint_Is20 (op)
      && !satisfies_constraint_Ihig (op))
    return false;

  return true;
})

(define_predicate "nds32_vmove_operand"
  (and (match_operand 0 "general_operand")
       (not (match_code "high,const,symbol_ref,label_ref")))
{
  /* If the constant op does NOT satisfy Is20 nor Ihig,
     we cannot perform move behavior by a single instruction.  */
  if (GET_CODE (op) == CONST_VECTOR
      && !satisfies_constraint_CVs2 (op)
      && !satisfies_constraint_CVhi (op))
    return false;

  return true;
})

(define_predicate "nds32_and_operand"
  (match_operand 0 "nds32_reg_constant_operand")
{
  return REG_P (op)
	 || GET_CODE (op) == SUBREG
	 || satisfies_constraint_Izeb (op)
	 || satisfies_constraint_Izeh (op)
	 || satisfies_constraint_Ixls (op)
	 || satisfies_constraint_Ix11 (op)
	 || satisfies_constraint_Ibms (op)
	 || satisfies_constraint_Ifex (op)
	 || satisfies_constraint_Iu15 (op)
	 || satisfies_constraint_Ii15 (op)
	 || satisfies_constraint_Ic15 (op);
})

(define_predicate "nds32_ior_operand"
  (match_operand 0 "nds32_reg_constant_operand")
{
  return REG_P (op)
	 || GET_CODE (op) == SUBREG
	 || satisfies_constraint_Iu15 (op)
	 || satisfies_constraint_Ie15 (op);
})

(define_predicate "nds32_xor_operand"
  (match_operand 0 "nds32_reg_constant_operand")
{
  return REG_P (op)
	 || GET_CODE (op) == SUBREG
	 || satisfies_constraint_Iu15 (op)
	 || satisfies_constraint_It15 (op);
})

(define_predicate "nds32_general_register_operand"
  (match_code "reg,subreg")
{
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);

  return (REG_P (op)
	  && (REGNO (op) >= FIRST_PSEUDO_REGISTER
	      || REGNO (op) <= NDS32_LAST_GPR_REGNUM));
})

(define_predicate "nds32_call_address_operand"
  (ior (match_operand 0 "nds32_symbolic_operand")
       (match_operand 0 "nds32_general_register_operand")))

(define_predicate "nds32_insv_operand"
  (match_code "const_int")
{
  return INTVAL (op) == 0
	 || INTVAL (op) == 8
	 || INTVAL (op) == 16
	 || INTVAL (op) == 24;
})

(define_predicate "nds32_lmw_smw_base_operand"
  (and (match_code "mem")
       (match_test "nds32_valid_smw_lwm_base_p (op)")))

(define_predicate "float_even_register_operand"
  (and (match_code "reg")
       (and (match_test "REGNO (op) >= NDS32_FIRST_FPR_REGNUM")
	    (match_test "REGNO (op) <= NDS32_LAST_FPR_REGNUM")
	    (match_test "(REGNO (op) & 1) == 0"))))

(define_predicate "float_odd_register_operand"
  (and (match_code "reg")
       (and (match_test "REGNO (op) >= NDS32_FIRST_FPR_REGNUM")
	    (match_test "REGNO (op) <= NDS32_LAST_FPR_REGNUM")
	    (match_test "(REGNO (op) & 1) != 0"))))

(define_special_predicate "nds32_load_multiple_operation"
  (match_code "parallel")
{
  /* To verify 'load' operation, pass 'true' for the second argument.
     See the implementation in nds32.c for details.  */
  return nds32_valid_multiple_load_store_p (op, true, false);
})

(define_special_predicate "nds32_load_multiple_and_update_address_operation"
  (match_code "parallel")
{
  /* To verify 'load' operation, pass 'true' for the second argument.
     to verify 'update address' operation, pass 'true' for the third argument
     See the implementation in nds32.c for details.  */
  return nds32_valid_multiple_load_store_p (op, true, true);
})

(define_special_predicate "nds32_store_multiple_operation"
  (match_code "parallel")
{
  /* To verify 'store' operation, pass 'false' for the second argument.
     See the implementation in nds32.c for details.  */
  return nds32_valid_multiple_load_store_p (op, false, false);
})

(define_special_predicate "nds32_store_multiple_and_update_address_operation"
  (match_code "parallel")
{
  /* To verify 'store' operation, pass 'false' for the second argument,
     to verify 'update address' operation, pass 'true' for the third argument
     See the implementation in nds32.c for details.  */
  return nds32_valid_multiple_load_store_p (op, false, true);
})

(define_special_predicate "nds32_stack_push_operation"
  (match_code "parallel")
{
  /* To verify 'push' operation, pass 'true' for the second argument.
     See the implementation in nds32-predicates.c for details.  */
  return nds32_valid_stack_push_pop_p (op, true);
})

(define_special_predicate "nds32_stack_pop_operation"
  (match_code "parallel")
{
  /* To verify 'pop' operation, pass 'false' for the second argument.
     See the implementation in nds32-predicates.c for details.  */
  return nds32_valid_stack_push_pop_p (op, false);
})

;; ------------------------------------------------------------------------
