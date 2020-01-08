;; Predicates of machine description for CR16.
;; Copyright (C) 2012-2020 Free Software Foundation, Inc.
;; Contributed by KPIT Cummins Infosystems Limited.
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

;;  Predicates

;; Predicates for sbit/cbit instructions
;; bit operand used for the generation of bit insn generation
(define_predicate "bit_operand"
  (match_code "mem")
{
  return ((GET_CODE (op) == MEM && OK_FOR_Z (op)));
})

;; Unsigned 4-bits constant int or double value.
(define_predicate "u4bits_operand"
  (match_code "const_int,const_double")
{
  if (GET_CODE (op) == CONST_DOUBLE)
    return cr16_const_double_ok (op);
    return (UNSIGNED_INT_FITS_N_BITS(INTVAL (op), 4)) ? 1 : 0;
})

;; Operand is a constant integer where
;; only one bit is set to 1.
(define_predicate "one_bit_operand"
  (match_code "const_int")
{
  unsigned int val;

  val = INTVAL (op);
  if (mode == QImode) 
    val &= 0xff;
  else if (mode == HImode)
    val &= 0xffff;
  else
    gcc_unreachable();

  if (val != 0)
    return (val & (val - 1)) == 0; /* true if only one bit is set.  */
  else
    return 0;
})

;; Operand is a constant integer where
;; only one bit is set to 0.
(define_predicate "rev_one_bit_operand"
  (match_code "const_int")
{
  unsigned int val;

  val = ~INTVAL (op); /* Invert and use.  */
  if (mode == QImode) 
    val &= 0xff;
  else if (mode == HImode)
    val &= 0xffff;
  else
    gcc_unreachable();

  if (val != 0)
    return (val & (val - 1)) == 0; /* true if only one bit is set.  */
  else
    return 0;
})

;; Predicates for shift instructions
;; Immediate operand predicate for count in shift operations.
;; Immediate shall be 3-bits in case operand to be operated on
;; is a qi mode operand.
(define_predicate "shift_qi_imm_operand"
  (match_code "const_int")
{
  return (UNSIGNED_INT_FITS_N_BITS(INTVAL (op), 3)) ? 1 : 0;
})

;; Immediate shall be 4-bits in case operand to be operated on
;; is a hi mode operand.
(define_predicate "shift_hi_imm_operand"
  (match_code "const_int")
{
  return (UNSIGNED_INT_FITS_N_BITS(INTVAL (op), 4)) ? 1 : 0;
})

;; Immediate shall be 3-bits in case operand to be operated on
;; is a si mode operand.
(define_predicate "shift_si_imm_operand"
  (match_code "const_int")
{
  return (UNSIGNED_INT_FITS_N_BITS(INTVAL (op), 5)) ? 1 : 0;
})

;; Predicates for jump/call instructions
;; Jump immediate cannot be more than 24-bits
(define_predicate "jump_imm_operand"
  (match_code "const_int")
{
  return (UNSIGNED_INT_FITS_N_BITS(INTVAL (op), 24)) ? 1 : 0;
})

;; Call immediate cannot be more than 24-bits
(define_predicate "call_imm_operand"
  (match_operand 0 "immediate_operand")
{
  if (GET_CODE (op) != CONST_INT) return 1;
    return (UNSIGNED_INT_FITS_N_BITS(INTVAL (op), 24)) ? 1 : 0;
})

;; Operand is register or 4-bit immediate operand
(define_predicate "reg_or_u4bits_operand"
  (ior (match_operand 0 "u4bits_operand")
       (match_operand 0 "register_operand")))

;; Operand is a register or symbol reference
(define_predicate "reg_or_sym_operand"
  (ior (match_code "symbol_ref")
       (match_operand 0 "register_operand")))

;; Operand is a non stack pointer register
(define_predicate "nosp_reg_operand"
  (and (match_operand 0 "register_operand")
       (match_test "REGNO (op) != SP_REGNUM")))

(define_predicate "hard_reg_operand"
  (and (match_operand 0 "register_operand")
       (match_test "REGNO (op) <= 15")))

;; Operand is a memory reference and
;; not a push operand.
(define_predicate "store_operand"
  (and (match_operand 0 "memory_operand")
       (not (match_operand 0 "push_operand"))))

;; Helper predicate 
(define_predicate "reg_or_int_operand"
  (ior (match_code "const_int")
       (match_operand 0 "register_operand")))

;;
;;
;; Atithmetic/logical predicates

;; QI Helper
(define_predicate "arith_qi_operand"
   (match_code "const_int")
{
        return (IN_RAN(INTVAL (op), 0, 15) && ((INTVAL (op) != 9) 
                || (INTVAL (op) != 11))) ? 1 : 0 ; 
})

;;QI Reg, subreg(reg) or const_int.
(define_predicate "reg_qi_int_operand"
  (ior (match_operand 0 "arith_qi_operand")
       (match_operand 0 "register_operand")))

;; HI Helper
(define_predicate "arith_hi_operand"
   (match_code "const_int")
{
        return (IN_RAN(INTVAL (op), -32768, 32768) ) ? 1 : 0 ; 
})

;;HI Reg, subreg(reg) or const_int.
(define_predicate "reg_hi_int_operand"
  (ior (match_operand 0 "arith_hi_operand")
       (match_operand 0 "register_operand")))

;;SI Reg, subreg(reg) or const_int.
(define_predicate "reg_si_int_operand"
  (ior (match_operand 0 "const_int_operand")
       (match_operand 0 "register_operand")))

;;
;; Shift predicates

;; QI Helper
(define_predicate "shift_qi_operand"
   (match_code "const_int")
{
        return (IN_RAN(INTVAL (op), 0, 7) ) ? 1 : 0; 
})

;;QI Reg, subreg(reg) or const_int.
(define_predicate "shift_reg_qi_int_operand"
  (ior (match_operand 0 "shift_qi_operand")
       (match_operand 0 "register_operand")))

;; HI Helper
(define_predicate "shift_hi_operand"
   (match_code "const_int")
{
        return (IN_RAN(INTVAL (op), 0, 15) ) ? 1 : 0 ; 
})

;;HI Reg, subreg(reg) or const_int.
(define_predicate "shift_reg_hi_int_operand"
  (ior (match_operand 0 "shift_hi_operand")
       (match_operand 0 "register_operand")))

;; SI Helper
(define_predicate "shift_si_operand"
   (match_code "const_int")
{
        return (IN_RAN(INTVAL (op), 0, 31) ) ? 1 : 0; 
})

;;SI Reg, subreg(reg) or const_int.
(define_predicate "shift_reg_si_int_operand"
  (ior (match_operand 0 "shift_si_operand")
       (match_operand 0 "register_operand")))
