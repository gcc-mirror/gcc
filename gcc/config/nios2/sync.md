;; Machine Description for Altera Nios II synchronization primitives.
;; Copyright (C) 2014-2016 Free Software Foundation, Inc.
;; Contributed by Mentor Graphics, Inc.
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

(define_int_iterator UNSPECV_LOAD_EXCLUSIVE [UNSPECV_LDEX UNSPECV_LDSEX])
(define_int_attr load_exclusive [(UNSPECV_LDEX  "ldex")
                                 (UNSPECV_LDSEX "ldsex")])
(define_insn "<load_exclusive>"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec_volatile:SI
          [(match_operand:SI 1 "ldstex_memory_operand" "v")]
          UNSPECV_LOAD_EXCLUSIVE))]
  "TARGET_ARCH_R2"
  "<load_exclusive>\\t%0, %A1"
  [(set_attr "type" "ld")])

(define_int_iterator UNSPECV_STORE_EXCLUSIVE [UNSPECV_STEX UNSPECV_STSEX])
(define_int_attr store_exclusive [(UNSPECV_STEX  "stex")
                                  (UNSPECV_STSEX "stsex")])
(define_insn "<store_exclusive>"
  [(set (match_operand:SI 2 "register_operand" "=r")
        (unspec_volatile:SI [(const_int 0)] UNSPECV_STORE_EXCLUSIVE))
   (set (match_operand:SI 0 "ldstex_memory_operand" "=v")
        (unspec_volatile:SI
          [(match_operand:SI 1 "reg_or_0_operand" "rM")]
          UNSPECV_STORE_EXCLUSIVE))]
  "TARGET_ARCH_R2"
  "<store_exclusive>\\t%2, %z1, %A0"
  [(set_attr "type" "st")])
