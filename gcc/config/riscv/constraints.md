;; Constraint definitions for RISC-V target.
;; Copyright (C) 2011-2025 Free Software Foundation, Inc.
;; Contributed by Andrew Waterman (andrew@sifive.com).
;; Based on MIPS target for GNU compiler.
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

(define_register_constraint "f" "TARGET_HARD_FLOAT ? FP_REGS :
  (TARGET_ZFINX ? GR_REGS : NO_REGS)"
  "A floating-point register (if available, reuse GPR as FPR when use zfinx).")

(define_register_constraint "j" "SIBCALL_REGS"
  "@internal")

(define_register_constraint "R" "GR_REGS"
  "Even-odd general purpose register pair."
  "regno % 2 == 0")

;; Avoid using register t0 for JALR's argument, because for some
;; microarchitectures that is a return-address stack hint.
(define_register_constraint "l" "JALR_REGS"
  "@internal")

(define_register_constraint "cr" "RVC_GR_REGS"
  "RVC general purpose register (x8-x15).")

(define_register_constraint "cf" "TARGET_HARD_FLOAT ? RVC_FP_REGS : (TARGET_ZFINX ? RVC_GR_REGS : NO_REGS)"
  "RVC floating-point registers (f8-f15), if available, reuse GPR as FPR when use zfinx.")

;; General constraints

(define_constraint "I"
  "An I-type 12-bit signed immediate."
  (and (match_code "const_int")
       (match_test "SMALL_OPERAND (ival)")))

(define_constraint "J"
  "Integer zero."
  (and (match_code "const_int")
       (match_test "ival == 0")))

(define_constraint "k01"
  "Constant value 1."
  (and (match_code "const_int")
       (match_test "ival == 1")))

(define_constraint "k02"
  "Constant value 2"
  (and (match_code "const_int")
       (match_test "ival == 2")))

(define_constraint "k03"
  "Constant value 3"
  (and (match_code "const_int")
       (match_test "ival == 3")))

(define_constraint "k04"
  "Constant value 4"
  (and (match_code "const_int")
       (match_test "ival == 4")))

(define_constraint "k08"
  "Constant value 8"
  (and (match_code "const_int")
       (match_test "ival == 8")))

(define_constraint "P"
  "A 5-bit signed immediate for vmv.v.i."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, -16, 15)")))

(define_constraint "K"
  "A 5-bit unsigned immediate for CSR access instructions."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, 0, 31)")))

(define_constraint "L"
  "A U-type 20-bit signed immediate."
  (and (match_code "const_int")
       (match_test "LUI_OPERAND (ival)")))

(define_constraint "MiG"
  "const can be represented as sum of any S12 values."
  (and (match_code "const_int")
       (ior (match_test "IN_RANGE (ival,  2048,  4094)")
	    (match_test "IN_RANGE (ival, -4096, -2049)"))))

(define_constraint "Ds3"
  "@internal
   1, 2 or 3 immediate"
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, 1, 3)")))

(define_constraint "DsS"
  "@internal
   31 immediate"
  (and (match_code "const_int")
       (match_test "(ival & 31) == 31")))

(define_constraint "DsD"
  "@internal
   63 immediate"
  (and (match_code "const_int")
       (match_test "(ival & 63) == 63")))

(define_constraint "DbS"
  "@internal"
  (and (match_code "const_int")
       (match_test "SINGLE_BIT_MASK_OPERAND (ival)")))

(define_constraint "DnS"
  "@internal"
  (and (match_code "const_int")
       (match_test "SINGLE_BIT_MASK_OPERAND (~ival)")))

;; Floating-point constant +0.0, used for FCVT-based moves when FMV is
;; not available in RV32.
(define_constraint "G"
  "@internal"
  (and (match_code "const_double")
       (match_test "op == CONST0_RTX (mode)")))

(define_memory_constraint "A"
  "An address that is held in a general-purpose register."
  (and (match_code "mem")
       (match_test "GET_CODE(XEXP(op,0)) == REG")))

(define_constraint "S"
  "A constraint that matches an absolute symbolic address."
  (match_operand 0 "absolute_symbolic_operand"))

(define_constraint "U"
  "@internal
   A PLT-indirect call address."
  (match_operand 0 "plt_symbolic_operand"))

(define_constraint "T"
  "@internal
   A constant @code{move_operand}."
  (and (match_operand 0 "move_operand")
       (match_test "CONSTANT_P (op)")))

;; Zfa constraints.

(define_constraint "zfli"
  "A floating point number that can be loaded using instruction `fli` in zfa."
  (and (match_code "const_double")
       (match_test "TARGET_ZFA && (riscv_float_const_rtx_index_for_fli (op) != -1)")))

(define_register_constraint "zmvf" "(TARGET_ZFA || TARGET_XTHEADFMV) ? FP_REGS : NO_REGS"
  "A floating-point register for ZFA or XTheadFmv.")

(define_register_constraint "zmvr" "(TARGET_ZFA || TARGET_XTHEADFMV) ? GR_REGS : NO_REGS"
  "An integer register for  ZFA or XTheadFmv.")

;; Vector constraints.

(define_register_constraint "vr" "TARGET_VECTOR ? V_REGS : NO_REGS"
  "A vector register (if available).")

(define_register_constraint "vd" "TARGET_VECTOR ? VD_REGS : NO_REGS"
  "A vector register except mask register (if available).")

(define_register_constraint "vm" "TARGET_VECTOR ? VM_REGS : NO_REGS"
  "A vector mask register (if available).")

;; This constraint is used to match instruction "csrr %0, vlenb" which is generated in "mov<mode>".
;; VLENB is a run-time constant which represent the vector register length in bytes.
;; BYTES_PER_RISCV_VECTOR represent runtime invariant of vector register length in bytes.
;; We should only allow the poly equal to BYTES_PER_RISCV_VECTOR.
(define_constraint "vp"
  "POLY_INT"
  (and (match_code "const_poly_int")
       (match_test "known_eq (rtx_to_poly_int64 (op), BYTES_PER_RISCV_VECTOR)")))

(define_constraint "vu"
  "A undefined vector value."
  (and (match_code "unspec")
       (match_test "XINT (op, 1) == UNSPEC_VUNDEF")))

(define_constraint "vi"
  "A vector 5-bit signed immediate."
  (and (match_code "const_vector")
       (match_test "riscv_vector::const_vec_all_same_in_range_p (op, -16, 15)")))

(define_constraint "vj"
  "A vector negated 5-bit signed immediate."
  (and (match_code "const_vector")
       (match_test "riscv_vector::const_vec_all_same_in_range_p (op, -15, 16)")))

(define_constraint "vk"
  "A vector 5-bit unsigned immediate."
  (and (match_code "const_vector")
       (match_test "riscv_vector::const_vec_all_same_in_range_p (op, 0, 31)")))

(define_constraint "vl"
  "A uimm5 for Vector or zero for XTheadVector."
  (and (match_code "const_int")
       (ior (match_test "!TARGET_XTHEADVECTOR && satisfies_constraint_K (op)")
	    (match_test "TARGET_XTHEADVECTOR && satisfies_constraint_J (op)"))))

(define_constraint "Wc0"
  "@internal
 A constraint that matches a vector of immediate all zeros."
 (and (match_code "const_vector")
      (match_test "op == CONST0_RTX (GET_MODE (op))")))

(define_constraint "Wc1"
  "@internal
 A constraint that matches a vector of immediate all ones."
 (and (match_code "const_vector")
      (match_test "op == CONSTM1_RTX (GET_MODE (op))")))

(define_constraint "Wb1"
  "@internal
 A constraint that matches a BOOL vector of {...,0,...0,1}"
 (and (match_code "const_vector")
      (match_test "rtx_equal_p (op, riscv_vector::gen_scalar_move_mask (GET_MODE (op)))")))

(define_memory_constraint "Wdm"
  "Vector duplicate memory operand"
  (and (match_code "mem")
       (match_code "reg" "0")))

;; Vendor ISA extension constraints.

(define_memory_constraint "th_m_mia"
  "@internal
   A MEM with a valid address for th.[l|s]*ia instructions."
  (and (match_code "mem")
       (match_test "th_memidx_legitimate_modify_p (op, true)")))

(define_memory_constraint "th_m_mib"
  "@internal
   A MEM with a valid address for th.[l|s]*ib instructions."
  (and (match_code "mem")
       (match_test "th_memidx_legitimate_modify_p (op, false)")))

(define_memory_constraint "th_m_mir"
  "@internal
   A MEM with a valid address for th.[l|s]*r* instructions."
  (and (match_code "mem")
       (match_test "th_memidx_legitimate_index_p (op, false)")))

(define_memory_constraint "th_m_miu"
  "@internal
   A MEM with a valid address for th.[l|s]*ur* instructions."
  (and (match_code "mem")
       (match_test "th_memidx_legitimate_index_p (op, true)")))

(define_memory_constraint "th_m_noi"
  "@internal
   A MEM with does not match XTheadMemIdx operands."
  (and (match_code "mem")
       (and (match_test "!th_memidx_legitimate_modify_p (op, true)")
	    (and (match_test "!th_memidx_legitimate_modify_p (op, false)")
		 (and (match_test "!th_memidx_legitimate_index_p (op, false)")
		      (match_test "!th_memidx_legitimate_index_p (op, true)"))))))

;; CORE-V Constraints
(define_constraint "CV_alu_pow2"
  "@internal
   Checking for CORE-V ALU clip if ival plus 1 is a power of 2"
  (and (match_code "const_int")
       (and (match_test "IN_RANGE (ival, 0, 1073741823)")
            (match_test "exact_log2 (ival + 1) != -1"))))

(define_constraint "CV_bi_sign5"
  "@internal
   A 5-bit signed immediate for CORE-V Immediate Branch."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, -16, 15)")))

(define_constraint "CV_simd_si6"
  "A 6-bit signed immediate for SIMD."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, -32, 31)")))

(define_constraint "CV_simd_un6"
  "A 6-bit unsigned immediate for SIMD."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, 0, 63)")))

(define_constraint "CV_simd_i01"
  "Shifting immediate for SIMD shufflei1."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, 64, 127)")))

(define_constraint "CV_simd_i02"
  "Shifting immediate for SIMD shufflei2."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, -128, -65)")))

(define_constraint "CV_simd_i03"
  "Shifting immediate for SIMD shufflei3."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, -64, -1)")))

(define_constraint "Q"
  "An address operand that is valid for a prefetch instruction"
  (match_operand 0 "prefetch_operand"))
