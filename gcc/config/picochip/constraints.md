;; GCC machine description for picochip
;; Copyright (C) 2008-2014 Free Software Foundation, Inc.
;; Contributed by Picochip Ltd (http://www.picochip.com)
;; Maintained by Daniel Towner (dant@picochip.com) and Hariharan
;; Sandanagobalane (hariharan@picochip.com)
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
;; along with GCC; see the file COPYING3.  If not, see
;; <http://www.gnu.org/licenses/>.

(define_constraint "I"
 "4-bits signed value"
 (and (match_code "const_int")
      (match_test " ival >= -8 && ival< 8")))

(define_constraint "J"
 "4-bits unsigned value"
 (and (match_code "const_int")
      (match_test "ival>=0 && ival < 16")))

(define_constraint "K"
 "8-bits signed value"
 (and (match_code "const_int")
      (match_test " ival >= -128 && ival < 128")))

(define_constraint "M"
 "4-bits magnitude"
 (and (match_code "const_int")
      (match_test " abs(ival) < 16")))

(define_constraint "N"
 "10-bits signed value"
 (and (match_code "const_int")
      (match_test "ival >= -512 && ival < 512")))

(define_constraint "O"
 "16-bits signed value"
 (and (match_code "const_int")
      (match_test " ival >= -32768 && ival < 32768 ")))

(define_constraint "a"
 "See if this is an absolute address in memory"
  (and (match_code "mem")
       (match_test "picochip_absolute_memory_operand(op,mode) == 1")))

(define_register_constraint "k" "FRAME_REGS"
  "Frame regs")
(define_register_constraint "f" "PTR_REGS"
  "Pointer regs")
(define_register_constraint "t" "TWIN_REGS"
  "Twin regs")

