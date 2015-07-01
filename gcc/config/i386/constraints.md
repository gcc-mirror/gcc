;; Constraint definitions for IA-32 and x86-64.
;; Copyright (C) 2006-2015 Free Software Foundation, Inc.
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

;;; Unused letters:
;;;           H
;;;           h j               z

;; Integer register constraints.
;; It is not necessary to define 'r' here.
(define_register_constraint "R" "LEGACY_REGS"
 "Legacy register---the eight integer registers available on all
  i386 processors (@code{a}, @code{b}, @code{c}, @code{d},
  @code{si}, @code{di}, @code{bp}, @code{sp}).")

(define_register_constraint "q" "TARGET_64BIT ? GENERAL_REGS : Q_REGS"
 "Any register accessible as @code{@var{r}l}.  In 32-bit mode, @code{a},
  @code{b}, @code{c}, and @code{d}; in 64-bit mode, any integer register.")

(define_register_constraint "Q" "Q_REGS"
 "Any register accessible as @code{@var{r}h}: @code{a}, @code{b},
  @code{c}, and @code{d}.")

(define_register_constraint "l" "INDEX_REGS"
 "@internal Any register that can be used as the index in a base+index
  memory access: that is, any general register except the stack pointer.")

(define_register_constraint "a" "AREG"
 "The @code{a} register.")

(define_register_constraint "b" "BREG"
 "The @code{b} register.")

(define_register_constraint "c" "CREG"
 "The @code{c} register.")

(define_register_constraint "d" "DREG"
 "The @code{d} register.")

(define_register_constraint "S" "SIREG"
 "The @code{si} register.")

(define_register_constraint "D" "DIREG"
 "The @code{di} register.")

(define_register_constraint "A" "AD_REGS"
 "The @code{a} and @code{d} registers, as a pair (for instructions
  that return half the result in one and half in the other).")

(define_register_constraint "U" "CLOBBERED_REGS"
 "The call-clobbered integer registers.")

;; Floating-point register constraints.
(define_register_constraint "f"
 "TARGET_80387 || TARGET_FLOAT_RETURNS_IN_80387 ? FLOAT_REGS : NO_REGS"
 "Any 80387 floating-point (stack) register.")

(define_register_constraint "t"
 "TARGET_80387 || TARGET_FLOAT_RETURNS_IN_80387 ? FP_TOP_REG : NO_REGS"
 "Top of 80387 floating-point stack (@code{%st(0)}).")

(define_register_constraint "u"
 "TARGET_80387 || TARGET_FLOAT_RETURNS_IN_80387 ? FP_SECOND_REG : NO_REGS"
 "Second from top of 80387 floating-point stack (@code{%st(1)}).")

(define_register_constraint "Yk" "TARGET_AVX512F ? MASK_EVEX_REGS : NO_REGS"
"@internal Any mask register that can be used as predicate, i.e. k1-k7.")

(define_register_constraint "k" "TARGET_AVX512F ? MASK_REGS : NO_REGS"
"@internal Any mask register.")

;; Vector registers (also used for plain floating point nowadays).
(define_register_constraint "y" "TARGET_MMX ? MMX_REGS : NO_REGS"
 "Any MMX register.")

(define_register_constraint "x" "TARGET_SSE ? SSE_REGS : NO_REGS"
 "Any SSE register.")

(define_register_constraint "v" "TARGET_SSE ? ALL_SSE_REGS : NO_REGS"
 "Any EVEX encodable SSE register (@code{%xmm0-%xmm31}).")

(define_register_constraint "w" "TARGET_MPX ? BND_REGS : NO_REGS"
 "@internal Any bound register.")

;; We use the Y prefix to denote any number of conditional register sets:
;;  z	First SSE register.
;;  i	SSE2 inter-unit moves to SSE register enabled
;;  j	SSE2 inter-unit moves from SSE register enabled
;;  m	MMX inter-unit moves to MMX register enabled
;;  n	MMX inter-unit moves from MMX register enabled
;;  a	Integer register when zero extensions with AND are disabled
;;  p	Integer register when TARGET_PARTIAL_REG_STALL is disabled
;;  f	x87 register when 80387 floating point arithmetic is enabled
;;  r	SSE regs not requiring REX prefix when prefixes avoidance is enabled
;;	and all SSE regs otherwise

(define_register_constraint "Yz" "TARGET_SSE ? SSE_FIRST_REG : NO_REGS"
 "First SSE register (@code{%xmm0}).")

(define_register_constraint "Yi"
 "TARGET_SSE2 && TARGET_INTER_UNIT_MOVES_TO_VEC ? ALL_SSE_REGS : NO_REGS"
 "@internal Any SSE register, when SSE2 and inter-unit moves to vector registers are enabled.")

(define_register_constraint "Yj"
 "TARGET_SSE2 && TARGET_INTER_UNIT_MOVES_FROM_VEC ? ALL_SSE_REGS : NO_REGS"
 "@internal Any SSE register, when SSE2 and inter-unit moves from vector registers are enabled.")

(define_register_constraint "Ym"
 "TARGET_MMX && TARGET_INTER_UNIT_MOVES_TO_VEC ? MMX_REGS : NO_REGS"
 "@internal Any MMX register, when inter-unit moves to vector registers are enabled.")

(define_register_constraint "Yn"
 "TARGET_MMX && TARGET_INTER_UNIT_MOVES_FROM_VEC ? MMX_REGS : NO_REGS"
 "@internal Any MMX register, when inter-unit moves from vector registers are enabled.")

(define_register_constraint "Yp"
 "TARGET_PARTIAL_REG_STALL ? NO_REGS : GENERAL_REGS"
 "@internal Any integer register when TARGET_PARTIAL_REG_STALL is disabled.")

(define_register_constraint "Ya"
 "TARGET_ZERO_EXTEND_WITH_AND && optimize_function_for_speed_p (cfun)
  ? NO_REGS : GENERAL_REGS"
 "@internal Any integer register when zero extensions with AND are disabled.")

(define_register_constraint "Yf"
 "(ix86_fpmath & FPMATH_387) ? FLOAT_REGS : NO_REGS"
 "@internal Any x87 register when 80387 FP arithmetic is enabled.")

(define_register_constraint "Yr"
 "TARGET_SSE ? (X86_TUNE_AVOID_4BYTE_PREFIXES ? NO_REX_SSE_REGS : ALL_SSE_REGS) : NO_REGS"
 "@internal Lower SSE register when avoiding REX prefix and all SSE registers otherwise.")

;; We use the B prefix to denote any number of internal operands:
;;  f  FLAGS_REG
;;  g  GOT memory operand.
;;  s  Sibcall memory operand, not valid for TARGET_X32
;;  w  Call memory operand, not valid for TARGET_X32
;;  z  Constant call address operand.

(define_constraint "Bf"
  "@internal Flags register operand."
  (match_operand 0 "flags_reg_operand"))

(define_constraint "Bg"
  "@internal GOT memory operand."
  (match_operand 0 "GOT_memory_operand"))

(define_constraint "Bs"
  "@internal Sibcall memory operand."
  (and (not (match_test "TARGET_X32"))
       (match_operand 0 "sibcall_memory_operand")))

(define_constraint "Bw"
  "@internal Call memory operand."
  (and (not (match_test "TARGET_X32"))
       (match_operand 0 "memory_operand")))

(define_constraint "Bz"
  "@internal Constant call address operand."
  (match_operand 0 "constant_call_address_operand"))

;; Integer constant constraints.
(define_constraint "I"
  "Integer constant in the range 0 @dots{} 31, for 32-bit shifts."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, 0, 31)")))

(define_constraint "J"
  "Integer constant in the range 0 @dots{} 63, for 64-bit shifts."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, 0, 63)")))

(define_constraint "K"
  "Signed 8-bit integer constant."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, -128, 127)")))

(define_constraint "L"
  "@code{0xFF}, @code{0xFFFF} or @code{0xFFFFFFFF}
   for AND as a zero-extending move."
  (and (match_code "const_int")
       (match_test "ival == 0xff || ival == 0xffff
		    || ival == (HOST_WIDE_INT) 0xffffffff")))

(define_constraint "M"
  "0, 1, 2, or 3 (shifts for the @code{lea} instruction)."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, 0, 3)")))

(define_constraint "N"
  "Unsigned 8-bit integer constant (for @code{in} and @code{out}
   instructions)."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, 0, 255)")))

(define_constraint "O"
  "@internal Integer constant in the range 0 @dots{} 127, for 128-bit shifts."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, 0, 127)")))

;; Floating-point constant constraints.
;; We allow constants even if TARGET_80387 isn't set, because the
;; stack register converter may need to load 0.0 into the function
;; value register (top of stack).
(define_constraint "G"
  "Standard 80387 floating point constant."
  (and (match_code "const_double")
       (match_test "standard_80387_constant_p (op) > 0")))

;; This can theoretically be any mode's CONST0_RTX.
(define_constraint "C"
  "Standard SSE floating point constant."
  (match_test "standard_sse_constant_p (op)"))

;; Constant-or-symbol-reference constraints.

(define_constraint "e"
  "32-bit signed integer constant, or a symbolic reference known
   to fit that range (for immediate operands in sign-extending x86-64
   instructions)."
  (match_operand 0 "x86_64_immediate_operand"))

;; We use W prefix to denote any number of
;; constant-or-symbol-reference constraints

(define_constraint "We"
  "32-bit signed integer constant, or a symbolic reference known
   to fit that range (for sign-extending conversion operations that
   require non-VOIDmode immediate operands)."
  (and (match_operand 0 "x86_64_immediate_operand")
       (match_test "GET_MODE (op) != VOIDmode")))

(define_constraint "Wz"
  "32-bit unsigned integer constant, or a symbolic reference known
   to fit that range (for zero-extending conversion operations that
   require non-VOIDmode immediate operands)."
  (and (match_operand 0 "x86_64_zext_immediate_operand")
       (match_test "GET_MODE (op) != VOIDmode")))

(define_constraint "Z"
  "32-bit unsigned integer constant, or a symbolic reference known
   to fit that range (for immediate operands in zero-extending x86-64
   instructions)."
  (match_operand 0 "x86_64_zext_immediate_operand"))

;; T prefix is used for different address constraints
;;   v - VSIB address
;;   s - address with no segment register
;;   i - address with no index and no rip
;;   b - address with no base and no rip

(define_address_constraint "Tv"
  "VSIB address operand"
  (match_operand 0 "vsib_address_operand"))

(define_address_constraint "Ts"
  "Address operand without segment register"
  (match_operand 0 "address_no_seg_operand"))

(define_address_constraint "Ti"
  "MPX address operand without index"
  (match_operand 0 "address_mpx_no_index_operand"))

(define_address_constraint "Tb"
  "MPX address operand without base"
  (match_operand 0 "address_mpx_no_base_operand"))
