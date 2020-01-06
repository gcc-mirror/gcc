;; Constraint definitions for Lattice Mico32 architecture.
;; Contributed by Jon Beniston <jon@beniston.com>
;;
;; Copyright (C) 2009-2020 Free Software Foundation, Inc.
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

(define_constraint "J"
  "The value 0."
  (and (match_code "const_int")
       (match_test "ival == 0")))
       
(define_constraint "K"
  "A signed 16-bit immediate in the range -32768 to 32767."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, -32768, 32767)")))

(define_constraint "L"
  "An unsigned 16-bit immediate in the range 0 to 65535."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, 0, 65535)")))

(define_constraint "M"
  "The value 1."
  (and (match_code "const_int")
       (match_test "ival == 1")))

(define_constraint "U"
  "A shifted signed 16-bit constant appropriate for orhi."
  (and (match_code "const_int")
       (match_test "(ival & 0xffff) == 0
		    && (ival >> 31 == -1 || ival >> 31 == 0)")))

(define_constraint "S"
  "A symbol in the small data section."
  (match_operand 0 "no_pic_small_symbol"))

(define_constraint "Y"
  "A high part of a symbol."
  (and (match_code "high")
       (ior (ior (match_code "symbol_ref" "0")
                 (match_code "label_ref" "0"))
            (match_code "const" "0"))))
