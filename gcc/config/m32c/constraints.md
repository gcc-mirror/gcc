;; m32c constraints
;; Copyright (C) 2012-2023 Free Software Foundation, Inc.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

(define_register_constraint "Rsp" "SP_REGS"
  "@internal")

(define_register_constraint "Rfb" "FB_REGS"
  "@internal")

(define_register_constraint "Rsb" "SB_REGS"
  "@internal")

(define_register_constraint "Rcr" "TARGET_A16 ? CR_REGS : NO_REGS"
  "@internal")

(define_register_constraint "Rcl" "TARGET_A24 ? CR_REGS : NO_REGS"
  "@internal")

(define_register_constraint "R0w" "R0_REGS"
  "@internal")

(define_register_constraint "R1w" "R1_REGS"
  "@internal")

(define_register_constraint "R2w" "R2_REGS"
  "@internal")

(define_register_constraint "R3w" "R3_REGS"
  "@internal")

(define_register_constraint "R02" "R02_REGS"
  "@internal")

(define_register_constraint "R13" "R13_REGS"
  "@internal")

(define_register_constraint "R03" "R03_REGS"
  "@internal")

(define_register_constraint "Rdi" "DI_REGS"
  "@internal")

(define_register_constraint "Rhl" "HL_REGS"
  "@internal")

(define_register_constraint "R23" "R23_REGS"
  "@internal")

(define_register_constraint "Ra0" "A0_REGS"
  "@internal")

(define_register_constraint "Ra1" "A1_REGS"
  "@internal")

(define_register_constraint "Raa" "A_REGS"
  "@internal")

(define_register_constraint "Raw" "TARGET_A16 ? A_REGS : NO_REGS"
  "@internal")

(define_register_constraint "Ral" "TARGET_A24 ? A_REGS : NO_REGS"
  "@internal")

(define_register_constraint "Rqi" "QI_REGS"
  "@internal")

(define_register_constraint "Rad" "AD_REGS"
  "@internal")

(define_register_constraint "Rsi" "SI_REGS"
  "@internal")

(define_register_constraint "Rhi" "HI_REGS"
  "@internal")

(define_register_constraint "Rhc" "HC_REGS"
  "@internal")

(define_register_constraint "Rra" "RA_REGS"
  "@internal")

(define_register_constraint "Rfl" "FLG_REGS"
  "@internal")

(define_register_constraint "Rmm" "fixed_regs[MEM0_REGNO] ? NO_REGS : MEM_REGS"
  "@internal")

(define_register_constraint "Rpi" "TARGET_A16 ? HI_REGS : RA_REGS"
  "@internal")

;;; For integer constant constraints:
;;; s=signed u=unsigned n=nonzero m=minus l=log2able,
;;; [sun] bits [SUN] bytes, p=pointer size
;;; I[-0-9][0-9] matches that number

(define_constraint "Is3"
  "@internal"
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, -8, 7)")))

(define_constraint "IS1"
  "@internal"
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, -128, 127)")))

(define_constraint "IS2"
  "@internal"
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, -32768, 32767)")))

(define_constraint "IU2"
  "@internal"
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, 0, 65535)")))

(define_constraint "IU3"
  "@internal"
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, 0, 0x00ffffff)")))

(define_constraint "In4"
  "@internal"
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, -8, 8) && ival")))

(define_constraint "In5"
  "@internal"
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, -16, 16) && ival")))

(define_constraint "In6"
  "@internal"
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, -32, 32) && ival")))

(define_constraint "IM2"
  "@internal"
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, -65536, -1)")))

(define_constraint "Ilb"
  "@internal"
  (and (match_code "const_int")
       (match_test "IN_RANGE (exact_log2 (ival), 0, 7)")))

(define_constraint "Imb"
  "@internal"
  (and (match_code "const_int")
       (match_test "IN_RANGE (exact_log2 ((ival ^ 0xff) & 0xff), 0, 7)")))

(define_constraint "ImB"
  "@internal"
  (and (match_code "const_int")
       (match_test "IN_RANGE (exact_log2 ((ival ^ 0xffff) & 0xffff), 0, 7)")))

(define_constraint "Ilw"
  "@internal"
  (and (match_code "const_int")
       (match_test "IN_RANGE (exact_log2 (ival), 0, 15)")))

(define_constraint "Imw"
  "@internal"
  (and (match_code "const_int")
       (match_test "IN_RANGE (exact_log2 ((ival ^ 0xffff) & 0xffff), 0, 15)")))

(define_constraint "I00"
  "@internal"
  (and (match_code "const_int")
       (match_test "ival == 0")))

(define_memory_constraint "SF"
  "@internal"
  (match_test "m32c_matches_constraint_p (op, CONSTRAINT_SF)"))

(define_memory_constraint "Sd"
  "@internal"
  (match_test "m32c_matches_constraint_p (op, CONSTRAINT_Sd)"))

(define_memory_constraint "Sa"
  "@internal"
  (match_test "m32c_matches_constraint_p (op, CONSTRAINT_Sa)"))

(define_memory_constraint "Si"
  "@internal"
  (match_test "m32c_matches_constraint_p (op, CONSTRAINT_Si)"))

(define_memory_constraint "Ss"
  "@internal"
  (match_test "m32c_matches_constraint_p (op, CONSTRAINT_Ss)"))

(define_memory_constraint "Sf"
  "@internal"
  (match_test "m32c_matches_constraint_p (op, CONSTRAINT_Sf)"))

(define_memory_constraint "Sb"
  "@internal"
  (match_test "m32c_matches_constraint_p (op, CONSTRAINT_Sb)"))

(define_memory_constraint "Sp"
  "@internal"
  (match_test "m32c_matches_constraint_p (op, CONSTRAINT_Sp)"))

(define_memory_constraint "S1"
  "@internal"
  (match_test "m32c_matches_constraint_p (op, CONSTRAINT_S1)"))

(define_constraint "Rpa"
  "@internal"
  (match_test "m32c_matches_constraint_p (op, CONSTRAINT_Rpa)"))
