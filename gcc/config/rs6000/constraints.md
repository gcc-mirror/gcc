;; Constraint definitions for RS6000
;; Copyright (C) 2006-2013 Free Software Foundation, Inc.
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

;; Available constraint letters: "e", "k", "q", "u", "A", "B", "C", "D"

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
;; vector double (V2DF)
(define_register_constraint "wd" "rs6000_constraints[RS6000_CONSTRAINT_wd]"
  "@internal")

;; vector float (V4SF)
(define_register_constraint "wf" "rs6000_constraints[RS6000_CONSTRAINT_wf]"
  "@internal")

;; scalar double (DF)
(define_register_constraint "ws" "rs6000_constraints[RS6000_CONSTRAINT_ws]"
  "@internal")

;; any VSX register
(define_register_constraint "wa" "rs6000_constraints[RS6000_CONSTRAINT_wa]"
  "@internal")

;; Altivec style load/store that ignores the bottom bits of the address
(define_memory_constraint "wZ"
  "Indexed or indirect memory operand, ignoring the bottom 4 bits"
  (match_operand 0 "altivec_indexed_or_indirect_operand"))

;; Integer constraints

(define_constraint "I"
  "A signed 16-bit constant"
  (and (match_code "const_int")
       (match_test "(unsigned HOST_WIDE_INT) (ival + 0x8000) < 0x10000")))

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
       (match_test "(unsigned HOST_WIDE_INT) ((- ival) + 0x8000) < 0x10000")))

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
       (match_operand 0 "mem_operand_gpr")))

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

(define_constraint "S"
  "Constant that can be placed into a 64-bit mask operand"
  (match_operand 0 "mask64_operand"))

(define_constraint "T"
  "Constant that can be placed into a 32-bit mask operand"
  (match_operand 0 "mask_operand"))

(define_constraint "U"
  "V.4 small data reference"
  (and (match_test "DEFAULT_ABI == ABI_V4")
       (match_operand 0 "small_data_operand")))

(define_constraint "t"
  "AND masks that can be performed by two rldic{l,r} insns
   (but excluding those that could match other constraints of anddi3)"
  (and (and (and (match_operand 0 "mask64_2_operand")
		 (match_test "(fixed_regs[CR0_REGNO]
			      || !logical_operand (op, DImode))"))
	    (not (match_operand 0 "mask_operand")))
       (not (match_operand 0 "mask64_operand"))))

(define_constraint "W"
  "vector constant that does not require memory"
  (match_operand 0 "easy_vector_constant"))

(define_constraint "j"
  "Zero vector constant"
  (match_test "op == const0_rtx || op == CONST0_RTX (GET_MODE (op))"))
