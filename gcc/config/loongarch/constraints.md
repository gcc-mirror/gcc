;; Constraint definitions for LoongArch.
;; Copyright (C) 2021-2025 Free Software Foundation, Inc.
;; Contributed by Loongson Ltd.
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

;; Register constraints

;; "a" <-----unused
;; "b" "A constant call not local address."
;; "c" "A constant call local address."
;; "d" <-----unused
;; "e" JIRL_REGS
;; "f" FP_REGS
;; "g" <-----unused
;; "h" <-----unused
;; "i" "Matches a general integer constant." (Global non-architectural)
;; "j" SIBCALL_REGS
;; "k" "A memory operand whose address is formed by a base register and
;;      (optionally scaled) index register."
;; "l" "A signed 16-bit constant."
;; "m" "A memory operand whose address is formed by a base register and offset
;;      that is suitable for use in instructions with the same addressing mode
;;      as @code{st.w} and @code{ld.w}."
;; "n" "Matches a non-symbolic integer constant." (Global non-architectural)
;; "o" "Matches an offsettable memory reference." (Global non-architectural)
;; "p" "Matches a general address." (Global non-architectural)
;; "q" CSR_REGS
;; "r" GENERAL_REGS (Global non-architectural)
;; "s" "Matches a symbolic integer constant." (Global non-architectural)
;; "t" <-----unused
;; "u" "A signed 52bit constant and low 32-bit is zero (for logic instructions)"
;; "v" "A signed 64-bit constant and low 44-bit is zero (for logic instructions)."
;; "w" "Matches any valid memory."
;; "x" <-----unused
;; "y" <-----unused
;; "z" FCC_REGS
;; "A" <-----unused
;; "B" <-----unused
;; "C" <-----unused
;; "D" <-----unused
;; "E" "Matches a floating-point constant." (Global non-architectural)
;; "F" "Matches a floating-point constant." (Global non-architectural)
;; "G" "Floating-point zero."
;; "H" <-----unused
;; "I" "A signed 12-bit constant (for arithmetic instructions)."
;; "J" "Integer zero."
;; "K" "An unsigned 12-bit constant (for logic instructions)."
;; "L" -
;;     "La"
;;	 "A signed constant in [-4096, 2048) or (2047, 4094]."
;;     "Lb"
;;	 "A signed 32-bit constant and low 16-bit is zero, which can be
;;	  added onto a register with addu16i.d.  It matches nothing if
;;	  the addu16i.d instruction is not available."
;;     "Lc"
;;	 "A signed 64-bit constant can be expressed as Lb + I, but not a
;;	  single Lb or I."
;;     "Ld"
;;	 "A signed 64-bit constant can be expressed as Lb + Lb, but not a
;;	  single Lb."
;;     "Le"
;;	 "A signed 32-bit constant can be expressed as Lb + I, but not a
;;	  single Lb or I."
;; "M" "A constant that cannot be loaded using @code{lui}, @code{addiu}
;;	or @code{ori}."
;; "N" "A constant in the range -65535 to -1 (inclusive)."
;; "O" "A signed 15-bit constant."
;; "P" "A constant in the range 1 to 65535 (inclusive)."
;; "Q" <-----unused
;; "R" "An address that can be used in a non-macro load or store."
;; "S" <-----unused
;; "T" <-----unused
;; "U" <-----unused
;; "V" "Matches a non-offsettable memory reference." (Global non-architectural)
;; "W" <-----unused
;; "X" "Matches anything." (Global non-architectural)
;; "Y" -
;;    "Yd"
;;       "A constant @code{move_operand} that can be safely loaded using
;;	  @code{la}."
;;    "Yx"
;;    "Yy"
;; "Z" -
;;    "ZC"
;;      "A memory operand whose address is formed by a base register and offset
;;       that is suitable for use in instructions with the same addressing mode
;;       as @code{ll.w} and @code{sc.w}."
;;    "ZB"
;;      "An address that is held in a general-purpose register.
;;      The offset is zero"
;;    "ZD"
;;	"An address operand whose address is formed by a base register
;;	 and offset that is suitable for use in instructions with the same
;;	 addressing mode as @code{preld}."
;; "<" "Matches a pre-dec or post-dec operand." (Global non-architectural)
;; ">" "Matches a pre-inc or post-inc operand." (Global non-architectural)

(define_constraint "b"
  "@internal
   A constant call no local address."
  (match_operand 0 "is_const_call_no_local_symbol"))

(define_constraint "c"
  "@internal
   A constant call local address."
  (match_operand 0 "is_const_call_local_symbol"))

(define_register_constraint "e" "JIRL_REGS"
  "@internal")

(define_register_constraint "f" "TARGET_HARD_FLOAT ? FP_REGS : NO_REGS"
  "A floating-point register (if available).")

(define_register_constraint "j" "SIBCALL_REGS"
  "@internal")

(define_memory_constraint "k"
  "A memory operand whose address is formed by a base register and (optionally scaled)
   index register."
  (and (match_code "mem")
       (match_test "loongarch_base_index_address_p (XEXP (op, 0), mode)")))

(define_constraint "l"
"A signed 16-bit constant."
(and (match_code "const_int")
     (match_test "IMM16_OPERAND (ival)")))

(define_memory_constraint "m"
  "A memory operand whose address is formed by a base register and offset
   that is suitable for use in instructions with the same addressing mode
   as @code{st.w} and @code{ld.w}."
  (and (match_code "mem")
       (match_test "loongarch_12bit_offset_address_p (XEXP (op, 0), mode)")))

(define_register_constraint "q" "CSR_REGS"
  "A general-purpose register except for $r0 and $r1 for lcsr.")

(define_constraint "u"
  "A signed 52bit constant and low 32-bit is zero (for logic instructions)."
  (and (match_code "const_int")
       (match_test "LU32I_OPERAND (ival)")))

(define_constraint "v"
  "A signed 64-bit constant and low 44-bit is zero (for logic instructions)."
  (and (match_code "const_int")
       (match_test "LU52I_OPERAND (ival)")))

(define_register_constraint "z" "FCC_REGS"
  "A floating-point condition code register.")

;; Floating-point constraints

(define_constraint "G"
  "Floating-point zero."
  (and (match_code "const_double")
       (match_test "op == CONST0_RTX (mode)")))

;; Integer constraints

(define_constraint "I"
  "A signed 12-bit constant (for arithmetic instructions)."
  (and (match_code "const_int")
       (match_test "IMM12_OPERAND (ival)")))

(define_constraint "J"
  "Integer zero."
  (and (match_code "const_int")
       (match_test "ival == 0")))

(define_constraint "K"
  "An unsigned 12-bit constant (for logic instructions)."
  (and (match_code "const_int")
       (match_test "IMM12_OPERAND_UNSIGNED (ival)")))

(define_constraint "La"
  "A signed constant in [-4096, 2048) or (2047, 4094]."
  (and (match_code "const_int")
       (match_test "DUAL_IMM12_OPERAND (ival)")))

(define_constraint "Lb"
  "A signed 32-bit constant and low 16-bit is zero, which can be added
   onto a register with addu16i.d."
  (and (match_code "const_int")
       (match_test "ADDU16I_OPERAND (ival)")))

(define_constraint "Lc"
  "A signed 64-bit constant can be expressed as Lb + I, but not a single Lb
   or I."
  (and (match_code "const_int")
       (match_test "loongarch_addu16i_imm12_operand_p (ival, DImode)")))

(define_constraint "Ld"
  "A signed 64-bit constant can be expressed as Lb + Lb, but not a single
   Lb."
  (and (match_code "const_int")
       (match_test "DUAL_ADDU16I_OPERAND (ival)")))

(define_constraint "Le"
  "A signed 32-bit constant can be expressed as Lb + I, but not a single Lb
   or I."
  (and (match_code "const_int")
       (match_test "loongarch_addu16i_imm12_operand_p (ival, SImode)")))

(define_constraint "M"
  "A constant that cannot be loaded using @code{lui}, @code{addiu}
   or @code{ori}."
  (and (match_code "const_int")
       (not (match_test "IMM12_OPERAND (ival)"))
       (not (match_test "IMM12_OPERAND_UNSIGNED (ival)"))
       (not (match_test "LU12I_OPERAND (ival)"))))

(define_constraint "N"
  "A constant in the range -65535 to -1 (inclusive)."
  (and (match_code "const_int")
       (match_test "ival >= -0xffff && ival < 0")))

(define_constraint "O"
  "A signed 15-bit constant."
  (and (match_code "const_int")
       (match_test "ival >= -0x4000 && ival < 0x4000")))

(define_constraint "P"
  "A constant in the range 1 to 65535 (inclusive)."
  (and (match_code "const_int")
       (match_test "ival > 0 && ival < 0x10000")))

;; General constraints

(define_memory_constraint "R"
  "An address that can be used in a non-macro load or store."
  (and (match_code "mem")
       (match_test "loongarch_address_insns (XEXP (op, 0), mode, false) == 1")))
(define_constraint "S"
  "@internal
   A constant call address."
  (and (match_operand 0 "call_insn_operand")
       (match_test "CONSTANT_P (op)")))

(define_constraint "YG"
  "@internal
   A vector zero."
  (and (match_code "const_vector")
       (match_test "op == CONST0_RTX (mode)")))

(define_constraint "YA"
  "@internal
   An unsigned 6-bit constant."
  (and (match_code "const_int")
       (match_test "UIMM6_OPERAND (ival)")))

(define_constraint "YB"
  "@internal
   A signed 10-bit constant."
  (and (match_code "const_int")
       (match_test "IMM10_OPERAND (ival)")))

(define_constraint "Yb"
   "@internal"
   (match_operand 0 "qi_mask_operand"))

(define_constraint "Yd"
  "@internal
   A constant @code{move_operand} that can be safely loaded using
   @code{la}."
  (and (match_operand 0 "move_operand")
       (match_test "CONSTANT_P (op)")))

(define_constraint "Yh"
   "@internal"
    (match_operand 0 "hi_mask_operand"))

(define_constraint "Yw"
   "@internal"
    (match_operand 0 "si_mask_operand"))

(define_constraint "Yx"
   "@internal"
   (match_operand 0 "low_bitmask_operand"))

(define_constraint "Yy"
   "@internal"
   (match_operand 0 "ins_zero_bitmask_operand"))

(define_constraint "YI"
  "@internal
   A replicated vector const in which the replicated value is in the range
   [-512,511]."
  (and (match_code "const_vector")
       (match_test "loongarch_const_vector_vrepli (op, mode)")))

(define_constraint "YC"
  "@internal
   A replicated vector const in which the replicated value has a single
   bit set."
  (and (match_code "const_vector")
       (match_test "loongarch_const_vector_bitimm_set_p (op, mode)")))

(define_constraint "YZ"
  "@internal
   A replicated vector const in which the replicated value has a single
   bit clear."
  (and (match_code "const_vector")
       (match_test "loongarch_const_vector_bitimm_clr_p (op, mode)")))

(define_constraint "Unv5"
  "@internal
   A replicated vector const in which the replicated value is in the range
   [-31,0]."
  (and (match_code "const_vector")
       (match_test "loongarch_const_vector_same_int_p (op, mode, -31, 0)")))

(define_constraint "Uuv5"
  "@internal
   A replicated vector const in which the replicated value is in the range
   [0,31]."
  (and (match_code "const_vector")
       (match_test "loongarch_const_vector_same_int_p (op, mode, 0, 31)")))

(define_constraint "Usv5"
  "@internal
   A replicated vector const in which the replicated value is in the range
   [-16,15]."
  (and (match_code "const_vector")
       (match_test "loongarch_const_vector_same_int_p (op, mode, -16, 15)")))

(define_constraint "Urv8"
  "@internal
   A replicated vector const with replicated byte values as well as elements"
  (and (match_code "const_vector")
       (match_test "loongarch_const_vector_same_bytes_p (op, mode)")))

(define_constraint "Uuvx"
  "@internal
   A replicated vector const in which the replicated value is in the unsigned
   range [0,umax]."
  (and (match_code "const_vector")
       (match_test "loongarch_const_vector_same_int_p (op, mode)")))

(define_memory_constraint "ZC"
  "A memory operand whose address is formed by a base register and offset
   that is suitable for use in instructions with the same addressing mode
   as @code{ll.w} and @code{sc.w}."
  (and (match_code "mem")
       (match_test "loongarch_14bit_shifted_offset_address_p (XEXP (op, 0), mode)")))

(define_memory_constraint "ZB"
  "@internal
  An address that is held in a general-purpose register.
  The offset is zero"
  (and (match_code "mem")
       (match_test "REG_P (XEXP (op, 0))")))

(define_address_constraint "ZD"
  "An address operand whose address is formed by a base register
   and offset that is suitable for use in instructions with the same
   addressing mode as @code{preld}."
   (match_test "loongarch_12bit_offset_address_p (op, mode)"))
