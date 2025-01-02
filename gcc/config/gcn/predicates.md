;; Predicate definitions for GCN.
;; Copyright (C) 2016-2025 Free Software Foundation, Inc.
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
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.
;; Return true if VALUE can be stored in a sign extended immediate field.

(define_predicate "gcn_conditional_register_operand"
  (match_operand 0 "register_operand")
{
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);

  if (!REG_P (op) || GET_MODE (op) != BImode)
    return 0;

  return REGNO (op) == VCCZ_REG
	 || REGNO (op) == VCC_REG   /* Implied VCCZ.  */
	 || REGNO (op) == SCC_REG
	 || REGNO (op) == EXECZ_REG
	 || REGNO (op) >= FIRST_PSEUDO_REGISTER;
})

(define_predicate "gcn_ssrc_register_operand"
  (match_operand 0 "register_operand")
{
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);

  if (!REG_P (op))
    return false;

  return SSRC_REGNO_P (REGNO (op)) || REGNO (op) >= FIRST_PSEUDO_REGISTER;
})

(define_predicate "gcn_sdst_register_operand"
  (match_operand 0 "register_operand")
{
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);

  if (!REG_P (op))
    return false;

  return SDST_REGNO_P (REGNO (op)) || REGNO (op) >= FIRST_PSEUDO_REGISTER;
})

(define_predicate "gcn_vgpr_register_operand"
  (match_operand 0 "register_operand")
{
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);

  if (!REG_P (op))
    return false;

  return VGPR_REGNO_P (REGNO (op)) || REGNO (op) >= FIRST_PSEUDO_REGISTER;
})

(define_predicate "gcn_avgpr_register_operand"
  (match_operand 0 "register_operand")
  {
    if (GET_CODE (op) == SUBREG)
        op = SUBREG_REG (op);

  if (!REG_P (op))
      return false;

  return AVGPR_REGNO_P (REGNO (op)) || REGNO (op) >= FIRST_PSEUDO_REGISTER;
})

(define_predicate "gcn_avgpr_hard_register_operand"
  (match_operand 0 "register_operand")
    {
        if (GET_CODE (op) == SUBREG)
	        op = SUBREG_REG (op);

  if (!REG_P (op))
        return false;

  return AVGPR_REGNO_P (REGNO (op));
})

(define_predicate "gcn_inline_immediate_operand"
  (match_code "const_int,const_double,const_vector")
{
  return gcn_inline_constant_p (op);
})

(define_predicate "gcn_vop3_operand"
  (ior (match_operand 0 "gcn_inline_immediate_operand")
       (match_operand 0 "register_operand")))

(define_predicate "gcn_vec0_operand"
  (match_code "const_vector")
{
  return CONST_VECTOR_ELT (op, 0) == const0_rtx && gcn_inline_constant_p (op);
})

(define_predicate "gcn_vec1_operand"
  (match_code "const_vector")
{
  return CONST_VECTOR_ELT (op, 0) == const1_rtx && gcn_inline_constant_p (op);
})

(define_predicate "gcn_vec1d_operand"
  (match_code "const_vector")
{
  if (!gcn_inline_constant_p (op))
    return false;

  rtx elem = CONST_VECTOR_ELT (op, 0);
  if (!CONST_DOUBLE_P (elem))
    return false;
  return real_identical (CONST_DOUBLE_REAL_VALUE (elem), &dconst1);
})

(define_predicate "gcn_const1d_operand"
  (match_code "const_double")
{
  return gcn_inline_constant_p (op)
      && real_identical (CONST_DOUBLE_REAL_VALUE (op), &dconst1);
})

(define_predicate "gcn_32bit_immediate_operand"
  (match_code "const_int,const_double,const_vector,symbol_ref,label_ref")
{
  return gcn_constant_p (op);
})

; LRA works smoother when exec values are immediate constants
; prior register allocation.
(define_predicate "gcn_exec_operand"
  (ior (match_operand 0 "register_operand")
       (match_code "const_int")))

(define_predicate "gcn_exec_reg_operand"
  (match_operand 0 "register_operand"))

(define_predicate "gcn_load_operand"
  (ior (match_operand 0 "nonimmediate_operand")
       (match_operand 0 "gcn_32bit_immediate_operand")))

(define_predicate "gcn_alu_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "gcn_32bit_immediate_operand")))

(define_predicate "gcn_ds_memory_operand"
  (and (match_code "mem")
       (and (match_test "AS_ANY_DS_P (MEM_ADDR_SPACE (op))")
	    (match_operand 0 "memory_operand"))))

(define_predicate "gcn_valu_dst_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "gcn_ds_memory_operand")))

(define_predicate "gcn_valu_src0_operand"
  (ior (match_operand 0 "register_operand")
       (ior (match_operand 0 "gcn_32bit_immediate_operand")
	    (match_operand 0 "gcn_ds_memory_operand"))))

(define_predicate "gcn_valu_src1_operand"
  (match_operand 0 "register_operand"))

(define_predicate "gcn_valu_src1com_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "gcn_32bit_immediate_operand")))

(define_predicate "gcn_conditional_operator"
  (match_code "eq,ne"))

(define_predicate "gcn_compare_64bit_operator"
  (match_code "eq,ne"))

(define_predicate "gcn_compare_operator"
  (match_code "eq,ne,gt,ge,lt,le,gtu,geu,ltu,leu"))

(define_predicate "gcn_fp_compare_operator"
  (match_code "eq,ne,gt,ge,lt,le,gtu,geu,ltu,leu,ordered,unordered,uneq,unge,ungt,unle,unlt,ltgt"))

(define_predicate "unary_operator"
  (match_code "not,popcount"))

(define_predicate "binary_operator"
  (match_code "and,ior,xor,ashift,lshiftrt,ashiftrt,smin,smax,umin,umax"))

(define_predicate "gcn_unspec_operand"
  (and (match_code "unspec")
       (match_test "XINT (op, 1) == UNSPEC_VECTOR")))

(define_predicate "general_or_unspec_operand"
  (ior (match_operand 0 "general_operand")
       (and (match_code "unspec")
            (match_test "XINT (op, 1) == UNSPEC_VECTOR"))))

(define_predicate "gcn_register_or_unspec_operand"
  (ior (match_operand 0 "register_operand")
       (and (match_code "unspec")
            (match_test "XINT (op, 1) == UNSPEC_VECTOR"))))

(define_predicate "gcn_alu_or_unspec_operand"
  (ior (match_operand 0 "gcn_alu_operand")
       (and (match_code "unspec")
            (match_test "XINT (op, 1) == UNSPEC_VECTOR"))))

(define_predicate "gcn_register_ds_or_unspec_operand"
  (ior (match_operand 0 "register_operand")
       (ior (match_operand 0 "gcn_ds_memory_operand")
	    (and (match_code "unspec")
              (match_test "XINT (op, 1) == UNSPEC_VECTOR")))))

(define_predicate "ascending_zero_int_parallel"
  (match_code "parallel")
{
  return gcn_stepped_zero_int_parallel_p (op, 1);
})

(define_predicate "maskload_else_operand"
  (match_operand 0 "scratch_operand"))
