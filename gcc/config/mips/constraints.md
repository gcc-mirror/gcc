;; Constraint definitions for MIPS.
;; Copyright (C) 2006-2025 Free Software Foundation, Inc.
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

(define_register_constraint "d" "TARGET_MIPS16 ? M16_REGS : GR_REGS"
  "A general-purpose register.  This is equivalent to @code{r} unless
   generating MIPS16 code, in which case the MIPS16 register set is used.")

(define_register_constraint "t" "T_REG"
  "@internal")

(define_register_constraint "f" "TARGET_HARD_FLOAT ? FP_REGS : NO_REGS"
  "A floating-point register (if available).")

(define_register_constraint "h" "NO_REGS"
  "Formerly the @code{hi} register.  This constraint is no longer supported.")

(define_register_constraint "l" "TARGET_BIG_ENDIAN ? MD1_REG : MD0_REG"
  "The @code{lo} register.  Use this register to store values that are
   no bigger than a word.")

(define_register_constraint "x" "MD_REGS"
  "The concatenated @code{hi} and @code{lo} registers.  Use this register
   to store doubleword values.")

(define_register_constraint "b" "ALL_REGS"
  "@internal")

(define_register_constraint "u" "M16_REGS"
  "@internal")

;; MIPS16 code always calls through a MIPS16 register; see mips_emit_call_insn
;; for details.
(define_register_constraint "c" "TARGET_MIPS16 ? M16_REGS
				 : TARGET_USE_PIC_FN_ADDR_REG ? PIC_FN_ADDR_REG
				 : GR_REGS"
  "A register suitable for use in an indirect jump.  This will always be
   @code{$25} for @option{-mabicalls}.")

(define_register_constraint "e" "LEA_REGS"
  "@internal")

(define_register_constraint "j" "PIC_FN_ADDR_REG"
  "@internal")

;; Don't use this constraint in gcc code!  It runs the risk of
;; introducing a spill failure; see tls_get_tp_<mode>.
(define_register_constraint "v" "V1_REG"
  "Register @code{$3}.  Do not use this constraint in new code;
   it is retained only for compatibility with glibc.")

(define_register_constraint "y" "GR_REGS"
  "Equivalent to @code{r}; retained for backwards compatibility.")

(define_register_constraint "z" "ST_REGS"
  "A floating-point condition code register.")

(define_register_constraint "A" "DSP_ACC_REGS"
  "@internal")

(define_register_constraint "a" "ACC_REGS"
  "@internal")

(define_register_constraint "B" "COP0_REGS"
  "@internal")

(define_register_constraint "C" "COP2_REGS"
  "@internal")

(define_register_constraint "D" "COP3_REGS"
  "@internal")

;; Registers that can be used as the target of multiply-accumulate
;; instructions.  The core MIPS32 ISA provides a hi/lo madd,
;; but the DSP version allows any accumulator target.
(define_register_constraint "ka" "ISA_HAS_DSP_MULT ? ACC_REGS : MD_REGS")

(define_register_constraint "kb" "M16_STORE_REGS"
  "@internal")

(define_constraint "kf"
  "@internal"
  (match_operand 0 "force_to_mem_operand"))

;; This is a normal rather than a register constraint because we can
;; never use the stack pointer as a reload register.
(define_constraint "ks"
  "@internal"
  (and (match_code "reg")
       (match_test "REGNO (op) == STACK_POINTER_REGNUM")))

;; Integer constraints

(define_constraint "I"
  "A signed 16-bit constant (for arithmetic instructions)."
  (and (match_code "const_int")
       (match_test "SMALL_OPERAND (ival)")))

(define_constraint "J"
  "Integer zero."
  (and (match_code "const_int")
       (match_test "ival == 0")))

(define_constraint "K"
  "An unsigned 16-bit constant (for logic instructions)."
  (and (match_code "const_int")
       (match_test "SMALL_OPERAND_UNSIGNED (ival)")))
 
(define_constraint "L"
  "A signed 32-bit constant in which the lower 16 bits are zero.
   Such constants can be loaded using @code{lui}."
  (and (match_code "const_int")
       (match_test "LUI_OPERAND (ival)")))

(define_constraint "M"
  "A constant that cannot be loaded using @code{lui}, @code{addiu}
   or @code{ori}."
  (and (match_code "const_int")
       (not (match_test "SMALL_OPERAND (ival)"))
       (not (match_test "SMALL_OPERAND_UNSIGNED (ival)"))
       (not (match_test "LUI_OPERAND (ival)"))))

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

;; Floating-point constraints

(define_constraint "G"
  "Floating-point zero."
  (and (match_code "const_double")
       (match_test "op == CONST0_RTX (mode)")))

;; General constraints

(define_constraint "Q"
  "@internal"
  (match_operand 0 "const_arith_operand"))

(define_memory_constraint "R"
  "An address that can be used in a non-macro load or store."
  (and (match_code "mem")
       (match_test "mips_address_insns (XEXP (op, 0), mode, false) == 1")))

(define_constraint "S"
  "@internal
   A constant call address."
  (and (match_operand 0 "call_insn_operand")
       (match_test "CONSTANT_P (op)")))

(define_constraint "Udb7"
  "@internal
   A decremented unsigned constant of 7 bits."
  (match_operand 0 "db7_operand"))

(define_constraint "Udb8"
  "@internal
   A decremented unsigned constant of 8 bits."
  (match_operand 0 "db8_operand"))

(define_constraint "Uead"
  "@internal
   A microMIPS encoded ADDIUR2 immediate operand."
  (match_operand 0 "addiur2_operand"))
  
(define_constraint "Uean"
  "@internal
   A microMIPS encoded ANDI operand."
  (match_operand 0 "andi16_operand"))

(define_constraint "Uesp"
  "@internal
   A microMIPS encoded ADDIUSP operand."
  (match_operand 0 "addiusp_operand"))

(define_constraint "Uib3"
  "@internal
   An unsigned, incremented constant of 3 bits."
  (match_operand 0 "ib3_operand"))

(define_constraint "Usb4"
  "@internal
   A signed constant of 4 bits."
  (match_operand 0 "sb4_operand"))

(define_constraint "Usb5"
  "@internal
   A signed constant of 5 bits."
  (match_operand 0 "sb5_operand"))

(define_constraint "Usb8"
  "@internal
   A signed constant of 8 bits."
  (match_operand 0 "sb8_operand"))

(define_constraint "Usd8"
  "@internal
   A signed constant of 8 bits, shifted left three places."
  (match_operand 0 "sd8_operand"))

(define_constraint "Uub8"
  "@internal
   An unsigned constant of 8 bits."
  (match_operand 0 "ub8_operand"))

(define_constraint "Uuw5"
  "@internal
   An unsigned constant of 5 bits, shifted left two places."
  (match_operand 0 "uw5_operand"))

(define_constraint "Uuw6"
  "@internal
   An unsigned constant of 6 bits, shifted left two places."
  (match_operand 0 "uw6_operand"))

(define_constraint "Uuw8"
  "@internal
   An unsigned constant of 8 bits, shifted left two places."
  (match_operand 0 "uw8_operand"))

(define_memory_constraint "W"
  "@internal
   A memory address based on a member of @code{BASE_REG_CLASS}.  This is
   true for all non-mips16 references (although it can sometimes be implicit
   if @samp{!TARGET_EXPLICIT_RELOCS}).  For MIPS16, it excludes stack and
   constant-pool references."
  (and (match_code "mem")
       (match_operand 0 "memory_operand")
       (ior (not (match_test "TARGET_MIPS16"))
	    (and (not (match_operand 0 "stack_operand"))
		 (not (match_test "CONSTANT_P (XEXP (op, 0))"))))))

(define_constraint "YG"
  "@internal
   A vector zero."
  (and (match_code "const_vector")
       (match_test "op == CONST0_RTX (mode)")))

(define_constraint "Yz"
  "@internal"
  (match_operand 0 "bit_clear_operand"))

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
   A constant @code{move_operand} that can be safely loaded into @code{$25}
   using @code{la}."
  (and (match_operand 0 "move_operand")
       (match_test "CONSTANT_P (op)")
       (not (match_test "mips_dangerous_for_la25_p (op)"))))

(define_constraint "Yf"
  "@internal
   A constant @code{move_operand} that cannot be safely loaded into @code{$25}
   using @code{la}."
  (and (match_operand 0 "move_operand")
       (match_test "CONSTANT_P (op)")
       (match_test "mips_dangerous_for_la25_p (op)")))

(define_constraint "Yh"
   "@internal"
    (match_operand 0 "hi_mask_operand"))

(define_constraint "Yw"
   "@internal"
    (match_operand 0 "si_mask_operand"))

(define_constraint "Yx"
   "@internal"
   (match_operand 0 "low_bitmask_operand"))

(define_constraint "YI"
  "@internal
   A replicated vector const in which the replicated value is in the range
   [-512,511]."
  (and (match_code "const_vector")
       (match_test "mips_const_vector_same_int_p (op, mode, -512, 511)")))

(define_constraint "YC"
  "@internal
   A replicated vector const in which the replicated value has a single
   bit set."
  (and (match_code "const_vector")
       (match_test "mips_const_vector_bitimm_set_p (op, mode)")))

(define_constraint "YZ"
  "@internal
   A replicated vector const in which the replicated value has a single
   bit clear."
  (and (match_code "const_vector")
       (match_test "mips_const_vector_bitimm_clr_p (op, mode)")))

(define_constraint "Unv5"
  "@internal
   A replicated vector const in which the replicated value is in the range
   [-31,0]."
  (and (match_code "const_vector")
       (match_test "mips_const_vector_same_int_p (op, mode, -31, 0)")))

(define_constraint "Uuv5"
  "@internal
   A replicated vector const in which the replicated value is in the range
   [0,31]."
  (and (match_code "const_vector")
       (match_test "mips_const_vector_same_int_p (op, mode, 0, 31)")))

(define_constraint "Usv5"
  "@internal
   A replicated vector const in which the replicated value is in the range
   [-16,15]."
  (and (match_code "const_vector")
       (match_test "mips_const_vector_same_int_p (op, mode, -16, 15)")))

(define_constraint "Uuv6"
  "@internal
   A replicated vector const in which the replicated value is in the range
   [0,63]."
  (and (match_code "const_vector")
       (match_test "mips_const_vector_same_int_p (op, mode, 0, 63)")))

(define_constraint "Urv8"
  "@internal
   A replicated vector const with replicated byte values as well as elements"
  (and (match_code "const_vector")
       (match_test "mips_const_vector_same_bytes_p (op, mode)")))

(define_memory_constraint "ZC"
  "A memory operand whose address is formed by a base register and offset
   that is suitable for use in instructions with the same addressing mode
   as @code{ll} and @code{sc}."
  (and (match_code "mem")
       (if_then_else
	 (match_test "TARGET_MICROMIPS")
	 (match_test "umips_12bit_offset_address_p (XEXP (op, 0), mode)")
	 (if_then_else (match_test "ISA_HAS_9BIT_DISPLACEMENT")
	   (match_test "mips_9bit_offset_address_p (XEXP (op, 0), mode)")
	   (match_test "mips_address_insns (XEXP (op, 0), mode, false)")))))

(define_address_constraint "ZD"
  "An address suitable for a @code{prefetch} instruction, or for any other
   instruction with the same addressing mode as @code{prefetch}."
   (if_then_else (match_test "TARGET_MICROMIPS")
		 (match_test "umips_12bit_offset_address_p (op, mode)")
	  (if_then_else (match_test "ISA_HAS_9BIT_DISPLACEMENT")
			(match_test "mips_9bit_offset_address_p (op, mode)")
			(match_test "mips_address_insns (op, mode, false)"))))

(define_memory_constraint "ZR"
 "@internal
  An address valid for loading/storing register exclusive"
 (match_operand 0 "mem_noofs_operand"))

(define_memory_constraint "ZS"
  "@internal
   A microMIPS memory operand for use with the LWSP/SWSP insns."
  (and (match_code "mem")
       (match_operand 0 "lwsp_swsp_operand")))

(define_memory_constraint "ZT"
  "@internal
   A microMIPS memory operand for use with the LW16/SW16 insns."
  (and (match_code "mem")
       (match_operand 0 "lw16_sw16_operand")))

(define_memory_constraint "ZU"
  "@internal
   A microMIPS memory operand for use with the LHU16/SH16 insns."
  (and (match_code "mem")
       (match_operand 0 "lhu16_sh16_operand")))

(define_memory_constraint "ZV"
  "@internal
   A microMIPS memory operand for use with the SB16 insn."
  (and (match_code "mem")
       (match_operand 0 "sb16_operand")))

(define_memory_constraint "ZW"
  "@internal
   A microMIPS memory operand for use with the LBU16 insn."
  (and (match_code "mem")
       (match_operand 0 "lbu16_operand")))

