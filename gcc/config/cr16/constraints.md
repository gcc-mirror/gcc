;; Predicates of machine description for CR16.
;; Copyright (C) 2012-2021 Free Software Foundation, Inc.
;; Contributed by KPIT Cummins Infosystems Limited.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.  

;; Constraints
;; Register constraints
(define_register_constraint "b" "NOSP_REGS"
  "@no sp registers")

(define_register_constraint "c" "SHORT_REGS"
  "@short registers")

(define_register_constraint "d" "LONG_REGS"
  "@long registers")

;; Integer constraints.
(define_constraint "I"
  "A signed 4-bit immediate."
  (and (match_code "const_int")
       (match_test "SIGNED_INT_FITS_N_BITS (ival, 4)")))

(define_constraint "J"
  "A signed 5-bit immediate."
  (and (match_code "const_int")
       (match_test "SIGNED_INT_FITS_N_BITS (ival, 5)")))

(define_constraint "K"
  "A signed 6-bit immediate."
  (and (match_code "const_int")
       (match_test "SIGNED_INT_FITS_N_BITS (ival, 6)")))

(define_constraint "L"
  "A unsigned 4-bit immediate."
  (and (match_code "const_int")
       (match_test "UNSIGNED_INT_FITS_N_BITS (ival, 4)")))

(define_constraint "M"
  "A unsigned and customized  4-bit immediate."
  (and (match_code "const_int")
       (match_test "(IN_RANGE_P (ival, 0, 15) && ((ival != 9) && (ival != 11)))")))

(define_constraint "N"
  "A signed 16-bit immediate."
  (and (match_code "const_int")
       (match_test "IN_RANGE_P (ival, -32768, 32767)")))

(define_constraint "O"
  "A unsigned 20-bit immediate."
  (and (match_code "const_int")
       (match_test "IN_RANGE_P (ival, 0, 1048575)")))

(define_constraint "Q"
  "A shift QI immediate."
  (and (match_code "const_int")
       (match_test "IN_RANGE_P (ival, 0, 7)")))

(define_constraint "R"
  "A shift HI immediate."
  (and (match_code "const_int")
       (match_test "IN_RANGE_P (ival, 0, 15)")))

(define_constraint "S"
  "A shift SI immediate."
  (and (match_code "const_int")
       (match_test "IN_RANGE_P (ival, 0, 31)")))
