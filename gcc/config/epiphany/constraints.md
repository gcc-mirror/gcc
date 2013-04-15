;; Constraint definitions for Adaptiva epiphany
;; Copyright (C) 2007-2013 Free Software Foundation, Inc.
;; Contributed by Embecosm on behalf of Adapteva, Inc.

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;; Integer constraints

(define_constraint "U16"
  "An unsigned 16-bit constant."
  (ior (and (match_code "const_int")
	    (match_test "IMM16 (ival)"))
       (and (match_code "symbol_ref,label_ref,const")
	    (match_test "epiphany_small16 (op)"))))

(define_constraint "K"
  "An unsigned 5-bit constant."
  (and (match_code "const_int")
       (match_test "IMM5 (ival)")))

;; This could also accept symbol_ref, label_ref or const if we introduce
;; a small area and/or attribute that satisfies the 11-bit signed range.
(define_constraint "L"
  "A signed 11-bit constant."
  (and (match_code "const_int")
       (match_test "SIMM11 (ival)")))

(define_constraint "CnL"
  "A negated signed 11-bit constant."
  (and (match_code "const_int")
       (match_test "SIMM11 (-ival)")))

(define_constraint "Cm1"
  "A signed 11-bit constant added to -1"
  (and (match_code "const_int")
       (match_test "SIMM11 (ival+1)")
       (match_test "epiphany_m1reg >= 0")))

(define_constraint "Cl1"
  "Left-shift of -1"
  (and (match_code "const_int")
       (match_test "ival == (ival | ~(ival-1))")
       (match_test "epiphany_m1reg >= 0")))

(define_constraint "Cr1"
  "Right-shift of -1"
  (and (match_code "const_int")
       (match_test "ival == (ival & ~(ival+1))")
       (match_test "epiphany_m1reg >= 0")))

(define_constraint "Cal"
  "Constant for arithmetic/logical operations"
  (match_test "(flag_pic
		? nonsymbolic_immediate_operand (op, VOIDmode)
		: immediate_operand (op, VOIDmode))"))

(define_constraint "Csy"
  "Symbolic constant for call/jump instruction"
  (match_test "symbolic_operand (op, VOIDmode)"))

;; Register constraints
;; proper register constraints define a register class and can thus
;; drive register allocation and reload.  OTOH sometimes we want to
;; avoid just that.

;; The register class usable in short insns.
;; Subject to TARGET_PREFER_SHORT_INSN_REGS.
(define_register_constraint "Rcs" "SHORT_INSN_REGS"
  "short insn register class.")

; The registers that can be used to hold a sibcall call address.
; This must not conflict with any callee-saved registers.
(define_register_constraint "Rsc" "SIBCALL_REGS"
  "sibcall register class")

; The registers that can be used to hold a status value
(define_register_constraint "Rct" "CORE_CONTROL_REGS"
  "Core control register class")

;; The register group usable in short insns.
(define_constraint "Rgs"
  "short insn register group."
  (and (match_code "reg")
       (match_test "REGNO (op) >= FIRST_PSEUDO_REGISTER || REGNO (op) <= 7")))

;; Constant suitable for the addsi3_r pattern.
(define_constraint "Car"
  "addsi3_r constant."
  (and (match_code "const_int")
       (ior (match_test "RTX_OK_FOR_OFFSET_P (SImode, op)")
	    (match_test "RTX_OK_FOR_OFFSET_P (HImode, op)")
	    (match_test "RTX_OK_FOR_OFFSET_P (QImode, op)"))))

;; The return address if it can be replaced with GPR_LR.
(define_constraint "Rra"
  "return address constraint - register variant"
  (and (match_code "unspec")
       (match_test "XINT (op, 1) == UNSPEC_RETURN_ADDR")
       (match_test "!MACHINE_FUNCTION (cfun)->lr_clobbered")))

(define_constraint "Rcc"
  "integer condition code"
  (and (match_code "reg")
       (match_test "REGNO (op) == CC_REGNUM")))

;; The return address, which might be a stack slot.  */
(define_constraint "Sra"
  "return address constraint - memory variant"
  (and (match_code "unspec")
       (match_test "XINT (op, 1) == UNSPEC_RETURN_ADDR")))

(define_constraint "Cfm"
  "control register values to switch fp mode"
  (and (match_code "const")
       (match_test "GET_CODE (XEXP (op, 0)) == UNSPEC")
       (match_test "XINT (XEXP (op, 0), 1) == UNSPEC_FP_MODE")))
