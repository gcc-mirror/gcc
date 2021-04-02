;; Constraint definitions of Andes NDS32 cpu for GNU compiler
;; Copyright (C) 2012-2021 Free Software Foundation, Inc.
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

;; Check 16.8.7 Defining Machine-Specific Constraints for detail.

;; NO contrains can be prefixed with: E F V X g i m n o p r s
;; Machine-dependent integer: I J K L M N O P
;; Machine-dependent floating: G H


(define_register_constraint "w" "(TARGET_ISA_V3 || TARGET_ISA_V3M) ? LOW_REGS : NO_REGS"
  "LOW register class $r0 ~ $r7 constraint for V3/V3M ISA")

(define_register_constraint "l" "LOW_REGS"
  "LOW register class $r0 ~ $r7")

(define_register_constraint "d" "MIDDLE_REGS"
  "MIDDLE register class $r0 ~ $r11, $r16 ~ $r19")

(define_register_constraint "h" "HIGH_REGS"
  "HIGH register class $r12 ~ $r14, $r20 ~ $r31")


(define_register_constraint "t" "R15_TA_REG"
  "Temporary Assist register $ta (i.e. $r15)")

(define_register_constraint "e" "R8_REG"
  "Function Entry register $r8)")

(define_register_constraint "k" "STACK_REG"
  "Stack register $sp")

(define_register_constraint "v" "R5_REG"
  "Register $r5")

(define_register_constraint "x" "FRAME_POINTER_REG"
  "Frame pointer register $fp")

(define_register_constraint "f"
  "(TARGET_FPU_SINGLE || TARGET_FPU_DOUBLE) ? FP_REGS : NO_REGS"
 "The Floating point registers $fs0 ~ $fs31")

(define_constraint "Iv00"
  "Constant value 0"
  (and (match_code "const_int")
       (match_test "ival == 0")))

(define_constraint "Iv01"
  "Constant value 1"
  (and (match_code "const_int")
       (match_test "ival == 1")))

(define_constraint "Iv02"
  "Constant value 2"
  (and (match_code "const_int")
       (match_test "ival == 2")))

(define_constraint "Iv04"
  "Constant value 4"
  (and (match_code "const_int")
       (match_test "ival == 4")))

(define_constraint "Iv08"
  "Constant value 8"
  (and (match_code "const_int")
       (match_test "ival == 8")))

(define_constraint "Iu01"
  "Unsigned immediate 1-bit value"
  (and (match_code "const_int")
       (match_test "ival == 1 || ival == 0")))

(define_constraint "Iu02"
  "Unsigned immediate 2-bit value"
  (and (match_code "const_int")
       (match_test "ival < (1 << 2) && ival >= 0")))

(define_constraint "Iu03"
  "Unsigned immediate 3-bit value"
  (and (match_code "const_int")
       (match_test "ival < (1 << 3) && ival >= 0")))

(define_constraint "In03"
  "Negative immediate 3-bit value in the range of -7 to 0"
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, -7, 0)")))

(define_constraint "Iu04"
  "Unsigned immediate 4-bit value"
  (and (match_code "const_int")
       (match_test "ival < (1 << 4) && ival >= 0")))

(define_constraint "Is05"
  "Signed immediate 5-bit value"
  (and (match_code "const_int")
       (match_test "ival < (1 << 4) && ival >= -(1 << 4)")))

(define_constraint "Cs05"
  "Signed immediate 5-bit value"
  (and (match_code "const_double")
       (match_test "nds32_const_double_range_ok_p (op, SFmode, -(1 << 4), (1 << 4))")))

(define_constraint "Iu05"
  "Unsigned immediate 5-bit value"
  (and (match_code "const_int")
       (match_test "ival < (1 << 5) && ival >= 0")))

(define_constraint "In05"
  "Negative immediate 5-bit value in the range of -31 to 0"
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, -31, 0)")))

(define_constraint "Iu06"
  "Unsigned immediate 6-bit value"
  (and (match_code "const_int")
       (match_test "ival < (1 << 6) && ival >= 0")))

;; Ip05 is special and dedicated for v3 movpi45 instruction.
;; movpi45 has imm5u field but the range is 16 ~ 47.
(define_constraint "Ip05"
  "Unsigned immediate 5-bit value for movpi45 instruction with range 16-47"
  (and (match_code "const_int")
       (match_test "ival < ((1 << 5) + 16)
		    && ival >= (0 + 16)
		    && (TARGET_ISA_V3 || TARGET_ISA_V3M)")))

(define_constraint "IU06"
  "Unsigned immediate 6-bit value constraint for addri36.sp instruction"
  (and (match_code "const_int")
       (match_test "ival < (1 << 8)
		    && ival >= 0
		    && (ival % 4 == 0)
		    && (TARGET_ISA_V3 || TARGET_ISA_V3M)")))

(define_constraint "Iu08"
  "Unsigned immediate 8-bit value"
  (and (match_code "const_int")
       (match_test "ival < (1 << 8) && ival >= 0")))

(define_constraint "Iu09"
  "Unsigned immediate 9-bit value"
  (and (match_code "const_int")
       (match_test "ival < (1 << 9) && ival >= 0")))


(define_constraint "Is08"
  "Signed immediate 8-bit value"
  (and (match_code "const_int")
       (match_test "ival < (1 << 7) && ival >= -(1 << 7)")))

(define_constraint "Is10"
  "Signed immediate 10-bit value"
  (and (match_code "const_int")
       (match_test "ival < (1 << 9) && ival >= -(1 << 9)")))

(define_constraint "Is11"
  "Signed immediate 11-bit value"
  (and (match_code "const_int")
       (match_test "ival < (1 << 10) && ival >= -(1 << 10)")))

(define_constraint "Is14"
  "Signed immediate 14-bit value"
  (and (match_code "const_int")
       (match_test "ival < (1 << 13) && ival >= -(1 << 13)")))

(define_constraint "Is15"
  "Signed immediate 15-bit value"
  (and (match_code "const_int")
       (match_test "ival < (1 << 14) && ival >= -(1 << 14)")))

(define_constraint "Iu15"
  "Unsigned immediate 15-bit value"
  (and (match_code "const_int")
       (match_test "ival < (1 << 15) && ival >= 0")))


;; Ic15 is special and dedicated for performance extension
;; 'bclr' (single-bit-clear) instruction.
;; It is used in andsi3 pattern and recognized for the immediate
;; which is NOT in the range of imm15u but OK for 'bclr' instruction.
;; (If the immediate value IS in the range of imm15u,
;;  we can directly use 'andi' instruction.)
(define_constraint "Ic15"
  "A constant which is not in the range of imm15u but ok for bclr instruction"
  (and (match_code "const_int")
       (match_test "(ival & 0xffff8000) && nds32_can_use_bclr_p (ival)")))

;; Ie15 is special and dedicated for performance extension
;; 'bset' (single-bit-set) instruction.
;; It is used in iorsi3 pattern and recognized for the immediate
;; which is NOT in the range of imm15u but OK for 'bset' instruction.
;; (If the immediate value IS in the range of imm15u,
;;  we can directly use 'ori' instruction.)
(define_constraint "Ie15"
  "A constant which is not in the range of imm15u but ok for bset instruction"
  (and (match_code "const_int")
       (match_test "(ival & 0xffff8000) && nds32_can_use_bset_p (ival)")))

;; It15 is special and dedicated for performance extension
;; 'btgl' (single-bit-toggle) instruction.
;; It is used in xorsi3 pattern and recognized for the immediate
;; which is NOT in the range of imm15u but OK for 'btgl' instruction.
;; (If the immediate value IS in the range of imm15u,
;;  we can directly use 'xori' instruction.)
(define_constraint "It15"
  "A constant which is not in the range of imm15u but ok for btgl instruction"
  (and (match_code "const_int")
       (match_test "(ival & 0xffff8000) && nds32_can_use_btgl_p (ival)")))


;; Ii15 is special and dedicated for v3 isa
;; 'bitci' (bit-clear-immediate) instruction.
;; It is used in andsi3 pattern and recognized for the immediate whose
;; (~ival) value is in the range of imm15u and OK for 'bitci' instruction.
;; For example, 'andi $r0,$r0,0xfffffffc' can be presented
;  with 'bitci $r0,$r0,3'.
(define_constraint "Ii15"
  "A constant whose compliment value is in the range of imm15u
   and ok for bitci instruction"
  (and (match_code "const_int")
       (match_test "nds32_can_use_bitci_p (ival)")))


(define_constraint "Is16"
  "Signed immediate 16-bit value"
  (and (match_code "const_int")
       (match_test "ival < (1 << 15) && ival >= -(1 << 15)")))

(define_constraint "Is17"
  "Signed immediate 17-bit value"
  (and (match_code "const_int")
       (match_test "ival < (1 << 16) && ival >= -(1 << 16)")))


(define_constraint "Is19"
  "Signed immediate 19-bit value"
  (and (match_code "const_int")
       (match_test "ival < (1 << 18) && ival >= -(1 << 18)")))


(define_constraint "Is20"
  "Signed immediate 20-bit value"
  (and (match_code "const_int")
       (match_test "ival < (1 << 19) && ival >= -(1 << 19)")))

(define_constraint "Cs20"
  "Signed immediate 20-bit value"
  (and (match_code "const_double")
       (match_test "nds32_const_double_range_ok_p (op, SFmode, -(1 << 19), (1 << 19))")))

(define_constraint "Ihig"
  "The immediate value that can be simply set high 20-bit"
  (and (match_code "const_int")
       (match_test "(ival != 0) && ((ival & 0xfff) == 0)")))

(define_constraint "Chig"
  "The immediate value that can be simply set high 20-bit"
  (and (match_code "high")
       (match_test "GET_CODE (XEXP (op, 0)) == CONST_DOUBLE")))

(define_constraint "Izeb"
  "The immediate value 0xff"
  (and (match_code "const_int")
       (match_test "(ival == 0xff)")))

(define_constraint "Izeh"
  "The immediate value 0xffff"
  (and (match_code "const_int")
       (match_test "(ival == 0xffff)")))

(define_constraint "Ixls"
  "The immediate value 0x01"
  (and (match_code "const_int")
       (match_test "TARGET_EXT_PERF && (ival == 0x1)")))

(define_constraint "Ix11"
  "The immediate value 0x7ff"
  (and (match_code "const_int")
       (match_test "TARGET_EXT_PERF && (ival == 0x7ff)")))

(define_constraint "Ibms"
  "The immediate value with power of 2"
  (and (match_code "const_int")
       (match_test "(TARGET_ISA_V3 || TARGET_ISA_V3M)
		    && (IN_RANGE (exact_log2 (ival), 0, 7))")))

(define_constraint "Ifex"
  "The immediate value with power of 2 minus 1"
  (and (match_code "const_int")
       (match_test "(TARGET_ISA_V3 || TARGET_ISA_V3M)
		    && (IN_RANGE (exact_log2 (ival + 1), 1, 8))")))

(define_constraint "CVp5"
  "Unsigned immediate 5-bit value for movpi45 instruction with range 16-47"
  (and (match_code "const_vector")
       (match_test "nds32_valid_CVp5_p (op)")))

(define_constraint "CVs5"
  "Signed immediate 5-bit value"
  (and (match_code "const_vector")
       (match_test "nds32_valid_CVs5_p (op)")))

(define_constraint "CVs2"
  "Signed immediate 20-bit value"
  (and (match_code "const_vector")
       (match_test "nds32_valid_CVs2_p (op)")))

(define_constraint "CVhi"
  "The immediate value that can be simply set high 20-bit"
  (and (match_code "const_vector")
       (match_test "nds32_valid_CVhi_p (op)")))

(define_memory_constraint "U33"
  "Memory constraint for 333 format"
  (and (match_code "mem")
       (match_test "nds32_mem_format (op) == ADDRESS_POST_INC_LO_REG_IMM3U
		    || nds32_mem_format (op) == ADDRESS_POST_MODIFY_LO_REG_IMM3U
		    || nds32_mem_format (op) == ADDRESS_LO_REG_IMM3U")))

(define_memory_constraint "U45"
  "Memory constraint for 45 format"
  (and (match_code "mem")
       (match_test "(nds32_mem_format (op) == ADDRESS_REG)
		    && ((GET_MODE (op) == SImode)
		       || (GET_MODE (op) == SFmode))")))

(define_memory_constraint "Ufe"
  "Memory constraint for fe format"
  (and (match_code "mem")
       (match_test "nds32_mem_format (op) == ADDRESS_R8_IMM7U
		    && (GET_MODE (op) == SImode
			|| GET_MODE (op) == SFmode)")))

(define_memory_constraint "U37"
  "Memory constraint for 37 format"
  (and (match_code "mem")
       (match_test "(nds32_mem_format (op) == ADDRESS_SP_IMM7U
		    || nds32_mem_format (op) == ADDRESS_FP_IMM7U)
		    && (GET_MODE (op) == SImode
			|| GET_MODE (op) == SFmode)")))

(define_memory_constraint "Umw"
  "Memory constraint for lwm/smw"
  (and (match_code "mem")
       (match_test "nds32_valid_smw_lwm_base_p (op)")))

(define_memory_constraint "Da"
  "Memory constraint for non-offset loads/stores"
  (and (match_code "mem")
       (match_test "REG_P (XEXP (op, 0))
		    || (GET_CODE (XEXP (op, 0)) == POST_INC)")))

(define_memory_constraint "Q"
  "Memory constraint for no symbol_ref and const"
  (and (match_code "mem")
       (match_test "(TARGET_FPU_SINGLE || TARGET_FPU_DOUBLE)
		     && nds32_float_mem_operand_p (op)")))

(define_constraint "S"
  "@internal
   A constant call address."
  (match_operand 0 "nds32_symbolic_operand"))

;; ------------------------------------------------------------------------
