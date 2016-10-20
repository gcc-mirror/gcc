;; Predicate definitions for Visium.
;; Copyright (C) 2005-2016 Free Software Foundation, Inc.
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

;; Return true if OP is the constant 0.
(define_predicate "const0_operand"
  (and (match_code "const_int,const_double")
       (match_test "op == CONST0_RTX (mode)")))

;; Return true if OP is a constant in the range 1 .. 31.
(define_predicate "const_shift_operand"
  (and (match_code "const_int")
       (match_test "1 <= INTVAL (op) && INTVAL (op) <= 31")))

;; Return true if OP is either a register or the constant 0.
(define_predicate "reg_or_0_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "const0_operand")))

;; Return true if OP is either a register or a constant in the range 1 .. 31.
(define_predicate "reg_or_shift_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "const_shift_operand")))

;; Return true if OP is either a register or the constant 32.
(define_predicate "reg_or_32_operand"
  (ior (match_operand 0 "register_operand")
       (and (match_code "const_int")
            (match_test "INTVAL (op) == 32"))))

;; Return true if OP is a general register.
(define_predicate "gpc_reg_operand"
  (match_operand 0 "register_operand")
{
  if (GET_CODE (op) == SUBREG)
    {
      op = SUBREG_REG (op);
      if (GET_CODE (op) != REG)
	return 1;
    }

  unsigned int regno = REGNO (op);
  return (regno >= FIRST_PSEUDO_REGISTER
	  || TEST_HARD_REG_BIT (reg_class_contents[GENERAL_REGS], regno));
})

;; Return true if OP is the MDB register.
(define_predicate "mdb_reg_operand"
  (match_operand 0 "register_operand")
{
  unsigned int regno = reg_or_subreg_regno (op);
  return (regno == MDB_REGNUM);
})

;; Return true if OP is the MDC register.
(define_predicate "mdc_reg_operand"
  (match_operand 0 "register_operand")
{
  unsigned int regno = reg_or_subreg_regno (op);
  return (regno == MDC_REGNUM);
})

;; Return true if OP is an rvalue which is not an EAM register.
(define_predicate "non_eam_src_operand"
  (match_operand 0 "general_operand")
{
  unsigned int regno = reg_or_subreg_regno (op);
  return (regno != MDB_REGNUM && regno != MDC_REGNUM);
})

;; Return true if OP is an lvalue which is not an EAM register.
(define_predicate "non_eam_dst_operand"
  (match_operand 0 "nonimmediate_operand")
{
  unsigned int regno = reg_or_subreg_regno (op);
  return (regno != MDB_REGNUM && regno != MDC_REGNUM);
})

;; Return true if OP is a floating-point register.
(define_predicate "fp_reg_operand"
  (match_code "reg")
{
  unsigned int regno = REGNO (op);
  return (regno >= FIRST_PSEUDO_REGISTER || FP_REGISTER_P (regno));
})

;; Return true if OP is a floating-point register or the constant 0.
(define_predicate "fp_reg_or_0_operand"
  (ior (match_operand 0 "fp_reg_operand")
       (match_operand 0 "const0_operand")))

;; Return true if OP can be used as the second operand in a 32-bit or 64-bit
;; add or subtract instruction.  Note that adding a negative constant may be
;; transformed into subtracting a positive constant, and vice versa.
(define_predicate "add_operand"
  (ior (match_operand 0 "gpc_reg_operand")
       (and (match_code "const_int")
            (match_test ("INTVAL (op) >= -65535 && INTVAL (op) <= 65535")))))

;; Return true if OP can be used as the second operand in a 32-bit or 64-bit
;; add or subtract instruction directly, i.e. without the reverse trick.
(define_predicate "real_add_operand"
  (ior (match_operand 0 "gpc_reg_operand")
       (and (match_code "const_int")
            (match_test ("INTVAL (op) >= 0 && INTVAL (op) <= 65535")))))

;; Return true if OP is (or could be) outside the range 0 .. 65535, which is
;; the range of the immediate operands, but accept -1 for NOT.
(define_predicate "large_immediate_operand"
  (ior (match_code "const,label_ref,symbol_ref")
       (and (match_code "const_int")
            (match_test ("INTVAL (op) < -1 || INTVAL (op) > 65535")))))

;; Return true if OP is an equality comparison operator.
(define_predicate "visium_equality_comparison_operator"
  (match_code "eq,ne"))

;; Return true if OP is a valid comparison operator for CCNZmode.
(define_predicate "visium_nz_comparison_operator"
  (match_code "eq,ne,lt,ge"))

;; Return true if OP is a valid comparison operator for CCCmode.
(define_predicate "visium_c_comparison_operator"
  (match_code "eq,ne,ltu,geu"))

;; Return true if OP is a valid comparison operator for CCVmode.
(define_predicate "visium_v_comparison_operator"
  (match_code "eq,ne"))

;; Return true if OP is a valid FP comparison operator.
(define_predicate "visium_fp_comparison_operator"
  (match_code "eq,ne,ordered,unordered,unlt,unle,ungt,unge,lt,le,gt,ge"))

;; Return true if OP is a valid comparison operator for a branch.  This allows
;; the use of MATCH_OPERATOR to recognize all the branch insns.
(define_predicate "visium_branch_operator"
  (match_operand 0 "comparison_operator")
{
  switch (GET_MODE (XEXP (op, 0)))
    {
    case CCmode:
      return ordered_comparison_operator (op, mode);
    case CCNZmode:
      return visium_nz_comparison_operator (op, mode);
    case CCCmode:
      return visium_c_comparison_operator (op, mode);
    case CCVmode:
      return visium_v_comparison_operator (op, mode);
    case CCFPmode:
    case CCFPEmode:
      return visium_fp_comparison_operator (op, mode);
    default:
      return false;
    }
})

;; Return true if OP is a valid comparison operator for an integer cstore.
(define_predicate "visium_int_cstore_operator"
  (match_code "eq,ne,ltu,gtu,leu,geu"))

;; Return true if OP is a valid comparison operator for an FP cstore.
(define_predicate "visium_fp_cstore_operator"
  (match_code "lt,gt,unge,unle"))
