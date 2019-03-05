;; Constraint definitions for Renesas M32R cpu for GNU C compiler
;; Copyright (C) 2007-2019 Free Software Foundation, Inc.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.

;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.


;; The letters I, J, K, L, M, N, O, P in a register constraint string
;; can be used to stand for particular ranges of immediate operands.
;; The letters Q, R, S, T, U are used to segregate specific types of
;; operands, usually memory references, for the target machine.
;;
;; I is used for 8-bit signed immediates.
;; J is used for 16-bit signed immediates.
;; K is used for 16-bit unsigned immediates.
;; L is used for 16-bit immediates left shifted by 16 (sign ???).
;; M is used for 24-bit unsigned immediates.
;; N is used for 8-bit signed immediates for compares
;;   (values in the range -127 to +128).
;; O is used for 5-bit unsigned immediates (shift count).
;; P is used for 16-bit signed immediates for compares
;;     (values in the range -32767 to +32768).
;;
;; Q is for symbolic addresses loadable with ld24.
;; R is for symbolic addresses when ld24 can't be used.
;; S is for stores with pre {inc,dec}rement
;; T is for indirect of a pointer.
;; U is for loads with post increment.
;; W is used for an immediate value of 0.
;;
;; Register constraints

(define_register_constraint "a" "ACCUM_REGS"
  "@internal")

(define_register_constraint "c" "CARRY_REG"
  "@internal")

;; Integer constraints
(define_constraint "I"
  "8-bit signed immediate."
  (and (match_code "const_int")
       (match_test "ival >= -0x80 && ival <= 0x7f")))

(define_constraint "J"
  "16-bit signed immediate."
  (and (match_code "const_int")
       (match_test "ival >= -0x8000 && ival <= 0x7fff")))

(define_constraint "K"
  "16-bit unsigned immediate."
  (and (match_code "const_int")
       (match_test "(unsigned HOST_WIDE_INT) ival <= 0x0000ffff")))

(define_constraint "L"
  "16-bit signed immediate left shifted by 16."
  (and (match_code "const_int")
       (match_test "(ival & 0xffff) == 0")
       (match_test "(ival >> 16) >= -0x8000 && (ival >> 16) <= 0x7fff")))

(define_constraint "M"
  "24-bit unsigned immediate."
  (and (match_code "const_int")
       (match_test "(unsigned HOST_WIDE_INT) ival <= 0x00ffffff")))

(define_constraint "N"
  "8-bit signed immediate for compare."
  (and (match_code "const_int")
       (match_test "ival >= -127 && ival <= 128")))

(define_constraint "O"
  "5-bit unsigned immediate."
  (and (match_code "const_int")
       (match_test "ival >= 0 && ival < 32")))

(define_constraint "P"
  "16-bit signed immediate for compare."
  (and (match_code "const_int")
       (match_test "ival >= -0x7fff && ival <= 0x8000")))

;; Floating-point constraints
(define_constraint "G"
  "Double constant loadable with 2 ldi insns."
  (and (match_code "const_double")
       (match_test "easy_di_const (op)")))

(define_constraint "H"
  "Double constant loadable with movdf."
  (and (match_code "const_double")
       (match_test "easy_df_const (op)")))

;; Extra constraints
(define_constraint "Q"
  "A symbolic address loadable when ld24."
  (ior (and (match_test "TARGET_ADDR24")
	    (match_test "GET_CODE (op) == LABEL_REF"))
       (match_test "addr24_operand (op, VOIDmode)")))

(define_constraint "R"
  "A symbolic address loadable with ld24 can't be used."
  (ior (and (match_test "TARGET_ADDR32")
	    (match_test "GET_CODE (op) == LABEL_REF"))
       (match_test "addr32_operand (op, VOIDmode)")))

(define_constraint "S"
  "A store with pre {inc,dec}rement."
  (and (match_code "mem")
       (match_test "mode == SImode || mode == SFmode")
       (match_code "pre_inc,pre_dec" "0")
       (match_code "reg" "00")
       (match_test "GPR_P (REGNO (XEXP (XEXP (op, 0), 0)))
		    || REGNO (XEXP (XEXP (op, 0), 0)) == ARG_POINTER_REGNUM
		    || ! HARD_REGISTER_P (XEXP (XEXP (op, 0), 0))")))

(define_constraint "T"
  "An indirect of a pointer."
  (and (match_code "mem")
       (match_test "memreg_operand (op, GET_MODE (op))")))

(define_constraint "U"
  "A load with post increment."
  (and (match_code "mem")
       (match_test "mode == SImode || mode == SFmode")
       (match_code "post_inc" "0")
       (match_code "reg" "00")
       (match_test "GPR_P (REGNO (XEXP (XEXP (op, 0), 0)))
		    || REGNO (XEXP (XEXP (op, 0), 0)) == ARG_POINTER_REGNUM
		    || ! HARD_REGISTER_P (XEXP (XEXP (op, 0), 0))")))

(define_constraint "W"
  "zero immediate."
  (and (match_code "const_int")
       (match_test "ival == 0")))

