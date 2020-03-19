;; Constraint definitions for RS6000
;; Copyright (C) 2006-2020 Free Software Foundation, Inc.
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

; Actually defined in common.md:
; (define_register_constraint "r" "GENERAL_REGS"
;   "A general purpose register (GPR), @code{r0}@dots{}@code{r31}.")

(define_register_constraint "b" "BASE_REGS"
  "A base register.  Like @code{r}, but @code{r0} is not allowed, so
   @code{r1}@dots{}@code{r31}.")

(define_register_constraint "f" "rs6000_constraints[RS6000_CONSTRAINT_f]"
  "A floating point register (FPR), @code{f0}@dots{}@code{f31}.")

(define_register_constraint "d" "rs6000_constraints[RS6000_CONSTRAINT_d]"
  "A floating point register.  This is the same as @code{f} nowadays;
   historically @code{f} was for single-precision and @code{d} was for
   double-precision floating point.")

(define_register_constraint "v" "ALTIVEC_REGS"
  "An Altivec vector register (VR), @code{v0}@dots{}@code{v31}.")

(define_register_constraint "wa" "rs6000_constraints[RS6000_CONSTRAINT_wa]"
  "A VSX register (VSR), @code{vs0}@dots{}@code{vs63}.  This is either an
  FPR (@code{vs0}@dots{}@code{vs31} are @code{f0}@dots{}@code{f31}) or a VR
  (@code{vs32}@dots{}@code{vs63} are @code{v0}@dots{}@code{v31}).")

(define_register_constraint "wd" "rs6000_constraints[RS6000_CONSTRAINT_wa]"
  "@internal A compatibility alias for @code{wa}.")
(define_register_constraint "wf" "rs6000_constraints[RS6000_CONSTRAINT_wa]"
  "@internal A compatibility alias for @code{wa}.")
(define_register_constraint "wi" "rs6000_constraints[RS6000_CONSTRAINT_wa]"
  "@internal A compatibility alias for @code{wa}.")
(define_register_constraint "ws" "rs6000_constraints[RS6000_CONSTRAINT_wa]"
  "@internal A compatibility alias for @code{wa}.")
(define_register_constraint "ww" "rs6000_constraints[RS6000_CONSTRAINT_wa]"
  "@internal A compatibility alias for @code{wa}.")

(define_register_constraint "h" "SPECIAL_REGS"
  "@internal A special register (@code{vrsave}, @code{ctr}, or @code{lr}).")

(define_register_constraint "c" "CTR_REGS"
  "The count register, @code{ctr}.")

(define_register_constraint "l" "LINK_REGS"
  "The link register, @code{lr}.")

(define_register_constraint "x" "CR0_REGS"
  "Condition register field 0, @code{cr0}.")

(define_register_constraint "y" "CR_REGS"
  "Any condition register field, @code{cr0}@dots{}@code{cr7}.")

(define_register_constraint "z" "CA_REGS"
  "@internal The carry bit, @code{XER[CA]}.")

;; NOTE: For compatibility, "wc" is reserved to represent individual CR bits.
;; It is currently used for that purpose in LLVM.

(define_register_constraint "we" "rs6000_constraints[RS6000_CONSTRAINT_we]"
  "@internal Like @code{wa}, if @option{-mpower9-vector} and @option{-m64} are
   used; otherwise, @code{NO_REGS}.")

;; NO_REGs register constraint, used to merge mov{sd,sf}, since movsd can use
;; direct move directly, and movsf can't to move between the register sets.
;; There is a mode_attr that resolves to wa for SDmode and wn for SFmode
(define_register_constraint "wn" "NO_REGS"
  "@internal No register (@code{NO_REGS}).")

(define_register_constraint "wr" "rs6000_constraints[RS6000_CONSTRAINT_wr]"
  "@internal Like @code{r}, if @option{-mpowerpc64} is used; otherwise,
   @code{NO_REGS}.")

(define_register_constraint "wx" "rs6000_constraints[RS6000_CONSTRAINT_wx]"
  "@internal Like @code{d}, if @option{-mpowerpc-gfxopt} is used; otherwise,
   @code{NO_REGS}.")

(define_register_constraint "wA" "rs6000_constraints[RS6000_CONSTRAINT_wA]"
  "@internal Like @code{b}, if @option{-mpowerpc64} is used; otherwise,
   @code{NO_REGS}.")

;; wB needs ISA 2.07 VUPKHSW
(define_constraint "wB"
  "@internal Signed 5-bit constant integer that can be loaded into an
   Altivec register."
  (and (match_code "const_int")
       (match_test "TARGET_P8_VECTOR")
       (match_operand 0 "s5bit_cint_operand")))

(define_constraint "wD"
  "@internal Int constant that is the element number of the 64-bit scalar
   in a vector."
  (and (match_code "const_int")
       (match_test "TARGET_VSX && (ival == VECTOR_ELEMENT_SCALAR_64BIT)")))

(define_constraint "wE"
  "@internal Vector constant that can be loaded with the XXSPLTIB instruction."
  (match_test "xxspltib_constant_nosplit (op, mode)"))

;; Extended fusion store
(define_memory_constraint "wF"
  "@internal Memory operand suitable for power8 GPR load fusion."
  (match_operand 0 "fusion_addis_mem_combo_load"))

(define_constraint "wL"
  "@internal Int constant that is the element number mfvsrld accesses in
   a vector."
  (and (match_code "const_int")
       (match_test "TARGET_DIRECT_MOVE_128")
       (match_test "(ival == VECTOR_ELEMENT_MFVSRLD_64BIT)")))

;; Generate the XXORC instruction to set a register to all 1's
(define_constraint "wM"
  "@internal Match vector constant with all 1's if the XXLORC instruction
   is available."
  (and (match_test "TARGET_P8_VECTOR")
       (match_operand 0 "all_ones_constant")))

;; ISA 3.0 vector d-form addresses
(define_memory_constraint "wO"
  "@internal Memory operand suitable for the ISA 3.0 vector d-form instructions."
  (match_operand 0 "vsx_quad_dform_memory_operand"))

;; Lq/stq validates the address for load/store quad
(define_memory_constraint "wQ"
  "@internal Memory operand suitable for the load/store quad instructions."
  (match_operand 0 "quad_memory_operand"))

(define_constraint "wS"
  "@internal Vector constant that can be loaded with XXSPLTIB & sign extension."
  (match_test "xxspltib_constant_split (op, mode)"))

;; ISA 3.0 DS-form instruction that has the bottom 2 bits 0 and no update form.
;; Used by LXSD/STXSD/LXSSP/STXSSP.  In contrast to "Y", the multiple-of-four
;; offset is enforced for 32-bit too.
(define_memory_constraint "wY"
  "@internal A memory operand for a DS-form instruction."
  (and (match_code "mem")
       (not (match_test "update_address_mem (op, mode)"))
       (match_test "mem_operand_ds_form (op, mode)")))

;; Altivec style load/store that ignores the bottom bits of the address
(define_memory_constraint "wZ"
  "@internal An indexed or indirect memory operand, ignoring the bottom 4 bits."
  (match_operand 0 "altivec_indexed_or_indirect_operand"))

;; Integer constraints

(define_constraint "I"
  "A signed 16-bit constant."
  (and (match_code "const_int")
       (match_test "((unsigned HOST_WIDE_INT) ival + 0x8000) < 0x10000")))

(define_constraint "J"
  "An unsigned 16-bit constant shifted left 16 bits (use @code{L} instead
   for @code{SImode} constants)."
  (and (match_code "const_int")
       (match_test "(ival & (~ (unsigned HOST_WIDE_INT) 0xffff0000)) == 0")))

(define_constraint "K"
  "An unsigned 16-bit constant."
  (and (match_code "const_int")
       (match_test "(ival & (~ (HOST_WIDE_INT) 0xffff)) == 0")))

(define_constraint "L"
  "A signed 16-bit constant shifted left 16 bits."
  (and (match_code "const_int")
       (match_test "((ival & 0xffff) == 0
		      && (ival >> 31 == -1 || ival >> 31 == 0))")))

(define_constraint "M"
  "@internal A constant greater than 31."
  (and (match_code "const_int")
       (match_test "ival > 31")))

(define_constraint "N"
  "@internal An exact power of two."
  (and (match_code "const_int")
       (match_test "ival > 0 && exact_log2 (ival) >= 0")))

(define_constraint "O"
  "@internal The integer constant zero."
  (and (match_code "const_int")
       (match_test "ival == 0")))

(define_constraint "P"
  "@internal A constant whose negation is a signed 16-bit constant."
  (and (match_code "const_int")
       (match_test "((- (unsigned HOST_WIDE_INT) ival) + 0x8000) < 0x10000")))

;; 34-bit signed integer constant
(define_constraint "eI"
  "A signed 34-bit integer constant if prefixed instructions are supported."
  (match_operand 0 "cint34_operand"))

;; Floating-point constraints.  These two are defined so that insn
;; length attributes can be calculated exactly.

(define_constraint "G"
  "@internal A floating point constant that can be loaded into a register
   with one instruction per word."
  (and (match_code "const_double")
       (match_test "num_insns_constant (op, mode)
		    == (mode == SFmode || mode == SDmode ? 1 : 2)")))

(define_constraint "H"
  "@internal A floating point constant that can be loaded into a register
   using three instructions."
  (and (match_code "const_double")
       (match_test "num_insns_constant (op, mode) == 3")))

;; Memory constraints

; Actually defined in common.md:
; (define_memory_constraint "m"
;   "A memory operand."

(define_memory_constraint "es"
  "@internal
   A ``stable'' memory operand; that is, one which does not include any
   automodification of the base register.  This used to be useful when
   @code{m} allowed automodification of the base register, but as those
   are now only allowed when @code{<} or @code{>} is used, @code{es} is
   basically the same as @code{m} without @code{<} and @code{>}."
  (and (match_code "mem")
       (match_test "GET_RTX_CLASS (GET_CODE (XEXP (op, 0))) != RTX_AUTOINC")))

(define_memory_constraint "Q"
  "A memory operand addressed by just a base register."
  (and (match_code "mem")
       (match_test "REG_P (XEXP (op, 0))")))

(define_memory_constraint "Y"
  "@internal A memory operand for a DQ-form instruction."
  (and (match_code "mem")
       (match_test "mem_operand_gpr (op, mode)")))

(define_memory_constraint "Z"
  "A memory operand accessed with indexed or indirect addressing."
  (match_operand 0 "indexed_or_indirect_operand"))

;; Address constraints

(define_constraint "R"
  "@internal An AIX TOC entry."
  (match_test "legitimate_constant_pool_address_p (op, QImode, false)"))

(define_address_constraint "a"
  "An indexed or indirect address."
  (match_operand 0 "indexed_or_indirect_address"))

;; General constraints

(define_constraint "U"
  "@internal A V.4 small data reference."
  (and (match_test "DEFAULT_ABI == ABI_V4")
       (match_test "small_data_operand (op, mode)")))

(define_constraint "W"
  "@internal A vector constant that does not require memory."
  (match_operand 0 "easy_vector_constant"))

(define_constraint "j"
  "@internal The zero vector constant."
  (match_test "op == const0_rtx || op == CONST0_RTX (mode)"))
