;; Constraint definitions for eBPF.
;; Copyright (C) 2019-2023 Free Software Foundation, Inc.
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

(define_constraint "I"
  "A 32-bit signed immediate."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, -1 - 0x7fffffff, 0x7fffffff)")))

(define_constraint "B"
  "A constant argument for LDDW."
  (match_code "const,symbol_ref,label_ref,const_double,const_int"))

(define_constraint "S"
  "A constant call address."
  (match_code "const,symbol_ref,label_ref,const_int"))

(define_register_constraint "t" "R0"
  "Register r0")

;;
;; Memory constraints.
;;

; Just like 'm' but disallows const_int.
; Used for ldx[b,h,w,dw] and stx[b,h,w,dw] instructions.
(define_memory_constraint "q"
  "Memory reference which is not a constant integer."
  (and (match_code "mem")
       (match_test "GET_CODE(XEXP(op, 0)) != CONST_INT")))
