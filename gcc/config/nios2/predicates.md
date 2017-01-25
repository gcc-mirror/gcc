;; Predicate definitions for Altera Nios II.
;; Copyright (C) 2012-2017 Free Software Foundation, Inc.
;; Contributed by Chung-Lin Tang <cltang@codesourcery.com>
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

(define_predicate "const_0_operand"
  (and (match_code "const_int,const_double,const_vector")
       (match_test "op == CONST0_RTX (GET_MODE (op))")))

(define_predicate "reg_or_0_operand"
  (ior (match_operand 0 "const_0_operand")
       (match_operand 0 "register_operand")))

(define_predicate "const_uns_arith_operand"
  (and (match_code "const_int")
       (match_test "SMALL_INT_UNSIGNED (INTVAL (op))")))

(define_predicate "uns_arith_operand"
  (ior (match_operand 0 "const_uns_arith_operand")
       (match_operand 0 "register_operand")))

(define_predicate "const_arith_operand"
  (and (match_code "const_int")
       (match_test "SMALL_INT (INTVAL (op))")))

(define_predicate "arith_operand"
  (ior (match_operand 0 "const_arith_operand")
       (match_operand 0 "register_operand")))

(define_predicate "add_regimm_operand"
  (ior (match_operand 0 "arith_operand")
       (match_test "nios2_unspec_reloc_p (op)")))

(define_predicate "const_logical_operand"
  (and (match_code "const_int")
       (match_test "(INTVAL (op) & 0xffff) == 0
                    || (INTVAL (op) & 0xffff0000) == 0")))

(define_predicate "logical_operand"
  (ior (match_operand 0 "const_logical_operand")
       (match_operand 0 "register_operand")))

(define_predicate "const_and_operand"
  (and (match_code "const_int")
       (match_test "SMALL_INT_UNSIGNED (INTVAL (op))
                    || UPPER16_INT (INTVAL (op))
                    || (TARGET_ARCH_R2 && ANDCLEAR_INT (INTVAL (op)))")))

(define_predicate "and_operand"
  (ior (match_operand 0 "const_and_operand")
       (match_operand 0 "register_operand")))

(define_predicate "const_shift_operand"
  (and (match_code "const_int")
       (match_test "SHIFT_INT (INTVAL (op))")))

(define_predicate "shift_operand"
  (ior (match_operand 0 "const_shift_operand")
       (match_operand 0 "register_operand")))

(define_predicate "call_operand"
  (ior (match_operand 0 "immediate_operand")
       (match_operand 0 "register_operand")))

(define_predicate "rdwrctl_operand"
  (and (match_code "const_int")
       (match_test "RDWRCTL_INT (INTVAL (op))")))

(define_predicate "rdprs_dcache_operand"
  (and (match_code "const_int")
       (if_then_else (match_test "TARGET_ARCH_R2")
                     (match_test "SMALL_INT12 (INTVAL (op))")
                     (match_test "SMALL_INT (INTVAL (op))"))))

(define_predicate "custom_insn_opcode"
  (and (match_code "const_int")
       (match_test "CUSTOM_INSN_OPCODE (INTVAL (op))")))

(define_special_predicate "expandable_comparison_operator"
  (match_operand 0 "ordered_comparison_operator")
{
  return (GET_MODE_CLASS (GET_MODE (XEXP (op, 0))) != MODE_FLOAT
          || nios2_validate_fpu_compare (GET_MODE (XEXP (op, 0)), &op,
                                         &XEXP (op, 0), &XEXP (op, 1),
                                         false));
})

(define_special_predicate "pop_operation"
  (match_code "parallel")
{
  return pop_operation_p (op);
})

(define_special_predicate "ldwm_operation"
  (match_code "parallel")
{
  return ldstwm_operation_p (op, /*load_p=*/true);
})

(define_special_predicate "stwm_operation"
  (match_code "parallel")
{
  return ldstwm_operation_p (op, /*load_p=*/false);
})

(define_predicate "nios2_hard_register_operand"
  (match_code "reg")
{
  return GP_REG_P (REGNO (op));
})

(define_predicate "stack_memory_operand"
  (match_code "mem")
{
  rtx addr = XEXP (op, 0);
  return ((REG_P (addr) && REGNO (addr) == SP_REGNO)
          || (GET_CODE (addr) == PLUS
              && REG_P (XEXP (addr, 0)) && REGNO (XEXP (addr, 0)) == SP_REGNO
              && CONST_INT_P (XEXP (addr, 1))));
})

(define_predicate "ldstio_memory_operand"
  (match_code "mem")
{
  if (TARGET_ARCH_R2)
    {
      rtx addr = XEXP (op, 0);
      if (REG_P (addr))
        return true;
      else if (GET_CODE (addr) == PLUS)
        return (REG_P (XEXP (addr, 0))
                && CONST_INT_P (XEXP (addr, 1))
                && SMALL_INT12 (INTVAL (XEXP (addr, 1))));
      return false;
    }
  return memory_operand (op, mode);
})

(define_predicate "ldstex_memory_operand"
  (match_code "mem")
{
  /* ldex/ldsex/stex/stsex cannot handle memory addresses with offsets.  */
  return GET_CODE (XEXP (op, 0)) == REG;
})
