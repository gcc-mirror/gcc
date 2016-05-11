;; Constraint definitions for RS6000
;; Copyright (C) 2006-2016 Free Software Foundation, Inc.
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

;; Available constraint letters: e k q t u A B C D S T

;; Register constraints

(define_register_constraint "f" "rs6000_constraints[RS6000_CONSTRAINT_f]"
  "@internal")

(define_register_constraint "d" "rs6000_constraints[RS6000_CONSTRAINT_d]"
  "@internal")

(define_register_constraint "b" "BASE_REGS"
  "@internal")

(define_register_constraint "h" "SPECIAL_REGS"
  "@internal")

(define_register_constraint "c" "CTR_REGS"
  "@internal")

(define_register_constraint "l" "LINK_REGS"
  "@internal")

(define_register_constraint "v" "ALTIVEC_REGS"
  "@internal")

(define_register_constraint "x" "CR0_REGS"
  "@internal")

(define_register_constraint "y" "CR_REGS"
  "@internal")

(define_register_constraint "z" "CA_REGS"
  "@internal")

;; Use w as a prefix to add VSX modes
;; any VSX register
(define_register_constraint "wa" "rs6000_constraints[RS6000_CONSTRAINT_wa]"
  "Any VSX register if the -mvsx option was used or NO_REGS.")

(define_register_constraint "wb" "rs6000_constraints[RS6000_CONSTRAINT_wb]"
  "Altivec register if the -mpower9-dform option was used or NO_REGS.")

;; NOTE: For compatibility, "wc" is reserved to represent individual CR bits.
;; It is currently used for that purpose in LLVM.

(define_register_constraint "wd" "rs6000_constraints[RS6000_CONSTRAINT_wd]"
  "VSX vector register to hold vector double data or NO_REGS.")

(define_register_constraint "we" "rs6000_constraints[RS6000_CONSTRAINT_we]"
  "VSX register if the -mpower9-vector -m64 options were used or NO_REGS.")

(define_register_constraint "wf" "rs6000_constraints[RS6000_CONSTRAINT_wf]"
  "VSX vector register to hold vector float data or NO_REGS.")

(define_register_constraint "wg" "rs6000_constraints[RS6000_CONSTRAINT_wg]"
  "If -mmfpgpr was used, a floating point register or NO_REGS.")

(define_register_constraint "wh" "rs6000_constraints[RS6000_CONSTRAINT_wh]"
  "Floating point register if direct moves are available, or NO_REGS.")

;; At present, DImode is not allowed in the Altivec registers.  If in the
;; future it is allowed, wi/wj can be set to VSX_REGS instead of FLOAT_REGS.
(define_register_constraint "wi" "rs6000_constraints[RS6000_CONSTRAINT_wi]"
  "FP or VSX register to hold 64-bit integers for VSX insns or NO_REGS.")

(define_register_constraint "wj" "rs6000_constraints[RS6000_CONSTRAINT_wj]"
  "FP or VSX register to hold 64-bit integers for direct moves or NO_REGS.")

(define_register_constraint "wk" "rs6000_constraints[RS6000_CONSTRAINT_wk]"
  "FP or VSX register to hold 64-bit doubles for direct moves or NO_REGS.")

(define_register_constraint "wl" "rs6000_constraints[RS6000_CONSTRAINT_wl]"
  "Floating point register if the LFIWAX instruction is enabled or NO_REGS.")

(define_register_constraint "wm" "rs6000_constraints[RS6000_CONSTRAINT_wm]"
  "VSX register if direct move instructions are enabled, or NO_REGS.")

;; NO_REGs register constraint, used to merge mov{sd,sf}, since movsd can use
;; direct move directly, and movsf can't to move between the register sets.
;; There is a mode_attr that resolves to wm for SDmode and wn for SFmode
(define_register_constraint "wn" "NO_REGS" "No register (NO_REGS).")

(define_register_constraint "wo" "rs6000_constraints[RS6000_CONSTRAINT_wo]"
  "VSX register if the -mpower9-vector option was used or NO_REGS.")

(define_register_constraint "wp" "rs6000_constraints[RS6000_CONSTRAINT_wp]"
  "VSX register to use for IEEE 128-bit fp TFmode, or NO_REGS.")

(define_register_constraint "wq" "rs6000_constraints[RS6000_CONSTRAINT_wq]"
  "VSX register to use for IEEE 128-bit fp KFmode, or NO_REGS.")

(define_register_constraint "wr" "rs6000_constraints[RS6000_CONSTRAINT_wr]"
  "General purpose register if 64-bit instructions are enabled or NO_REGS.")

(define_register_constraint "ws" "rs6000_constraints[RS6000_CONSTRAINT_ws]"
  "VSX vector register to hold scalar double values or NO_REGS.")

(define_register_constraint "wt" "rs6000_constraints[RS6000_CONSTRAINT_wt]"
  "VSX vector register to hold 128 bit integer or NO_REGS.")

(define_register_constraint "wu" "rs6000_constraints[RS6000_CONSTRAINT_wu]"
  "Altivec register to use for float/32-bit int loads/stores  or NO_REGS.")

(define_register_constraint "wv" "rs6000_constraints[RS6000_CONSTRAINT_wv]"
  "Altivec register to use for double loads/stores  or NO_REGS.")

(define_register_constraint "ww" "rs6000_constraints[RS6000_CONSTRAINT_ww]"
  "FP or VSX register to perform float operations under -mvsx or NO_REGS.")

(define_register_constraint "wx" "rs6000_constraints[RS6000_CONSTRAINT_wx]"
  "Floating point register if the STFIWX instruction is enabled or NO_REGS.")

(define_register_constraint "wy" "rs6000_constraints[RS6000_CONSTRAINT_wy]"
  "FP or VSX register to perform ISA 2.07 float ops or NO_REGS.")

(define_register_constraint "wz" "rs6000_constraints[RS6000_CONSTRAINT_wz]"
  "Floating point register if the LFIWZX instruction is enabled or NO_REGS.")

(define_constraint "wD"
  "Int constant that is the element number of the 64-bit scalar in a vector."
  (and (match_code "const_int")
       (match_test "TARGET_VSX && (ival == VECTOR_ELEMENT_SCALAR_64BIT)")))

;; Extended fusion store
(define_memory_constraint "wF"
  "Memory operand suitable for power9 fusion load/stores"
  (match_operand 0 "fusion_addis_mem_combo_load"))

;; Fusion gpr load.
(define_memory_constraint "wG"
  "Memory operand suitable for TOC fusion memory references"
  (match_operand 0 "toc_fusion_mem_wrapped"))

(define_constraint "wL"
  "Int constant that is the element number mfvsrld accesses in a vector."
  (and (match_code "const_int")
       (and (match_test "TARGET_DIRECT_MOVE_128")
	    (match_test "(ival == VECTOR_ELEMENT_MFVSRLD_64BIT)"))))

;; ISA 3.0 vector d-form addresses
(define_memory_constraint "wO"
  "Memory operand suitable for the ISA 3.0 vector d-form instructions."
  (match_operand 0 "vsx_quad_dform_memory_operand"))

;; Lq/stq validates the address for load/store quad
(define_memory_constraint "wQ"
  "Memory operand suitable for the load/store quad instructions"
  (match_operand 0 "quad_memory_operand"))

;; Altivec style load/store that ignores the bottom bits of the address
(define_memory_constraint "wZ"
  "Indexed or indirect memory operand, ignoring the bottom 4 bits"
  (match_operand 0 "altivec_indexed_or_indirect_operand"))

;; Integer constraints

(define_constraint "I"
  "A signed 16-bit constant"
  (and (match_code "const_int")
       (match_test "((unsigned HOST_WIDE_INT) ival + 0x8000) < 0x10000")))

(define_constraint "J"
  "high-order 16 bits nonzero"
  (and (match_code "const_int")
       (match_test "(ival & (~ (unsigned HOST_WIDE_INT) 0xffff0000)) == 0")))

(define_constraint "K"
  "low-order 16 bits nonzero"
  (and (match_code "const_int")
       (match_test "(ival & (~ (HOST_WIDE_INT) 0xffff)) == 0")))

(define_constraint "L"
  "signed 16-bit constant shifted left 16 bits"
  (and (match_code "const_int")
       (match_test "((ival & 0xffff) == 0
		      && (ival >> 31 == -1 || ival >> 31 == 0))")))

(define_constraint "M"
  "constant greater than 31"
  (and (match_code "const_int")
       (match_test "ival > 31")))

(define_constraint "N"
  "positive constant that is an exact power of two"
  (and (match_code "const_int")
       (match_test "ival > 0 && exact_log2 (ival) >= 0")))

(define_constraint "O"
  "constant zero"
  (and (match_code "const_int")
       (match_test "ival == 0")))

(define_constraint "P"
  "constant whose negation is signed 16-bit constant"
  (and (match_code "const_int")
       (match_test "((- (unsigned HOST_WIDE_INT) ival) + 0x8000) < 0x10000")))

;; Floating-point constraints

(define_constraint "G"
  "Constant that can be copied into GPR with two insns for DF/DI
   and one for SF."
  (and (match_code "const_double")
       (match_test "num_insns_constant (op, mode)
		    == (mode == SFmode ? 1 : 2)")))

(define_constraint "H"
  "DF/DI constant that takes three insns."
  (and (match_code "const_double")
       (match_test "num_insns_constant (op, mode) == 3")))

;; Memory constraints

(define_memory_constraint "es"
  "A ``stable'' memory operand; that is, one which does not include any
automodification of the base register.  Unlike @samp{m}, this constraint
can be used in @code{asm} statements that might access the operand
several times, or that might not access it at all."
  (and (match_code "mem")
       (match_test "GET_RTX_CLASS (GET_CODE (XEXP (op, 0))) != RTX_AUTOINC")))

(define_memory_constraint "Q"
  "Memory operand that is an offset from a register (it is usually better
to use @samp{m} or @samp{es} in @code{asm} statements)"
  (and (match_code "mem")
       (match_test "GET_CODE (XEXP (op, 0)) == REG")))

(define_memory_constraint "Y"
  "memory operand for 8 byte and 16 byte gpr load/store"
  (and (match_code "mem")
       (match_test "mem_operand_gpr (op, mode)")))

(define_memory_constraint "Z"
  "Memory operand that is an indexed or indirect from a register (it is
usually better to use @samp{m} or @samp{es} in @code{asm} statements)"
  (match_operand 0 "indexed_or_indirect_operand"))

;; Address constraints

(define_address_constraint "a"
  "Indexed or indirect address operand"
  (match_operand 0 "indexed_or_indirect_address"))

(define_constraint "R"
  "AIX TOC entry"
  (match_test "legitimate_constant_pool_address_p (op, QImode, false)"))

;; General constraints

(define_constraint "U"
  "V.4 small data reference"
  (and (match_test "DEFAULT_ABI == ABI_V4")
       (match_test "small_data_operand (op, mode)")))

(define_constraint "W"
  "vector constant that does not require memory"
  (match_operand 0 "easy_vector_constant"))

(define_constraint "j"
  "Zero vector constant"
  (match_test "op == const0_rtx || op == CONST0_RTX (mode)"))
