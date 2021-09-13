;; MMIX constraints
;; Copyright (C) 2012-2021 Free Software Foundation, Inc.
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
;; <http://www.gnu.org/licenses/>.  */

(define_register_constraint "x" "SYSTEM_REGS"
  "@internal")

(define_register_constraint "y" "REMAINDER_REG"
  "@internal")

(define_register_constraint "z" "HIMULT_REG"
  "@internal")

(define_constraint "I"
  "A 8-bit unsigned integer"
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, 0, 255)")))

(define_constraint "J"
  "A 16-bit unsigned integer."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, 0, 65535)")))

(define_constraint "K"
  "An integer between -255 and 0."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, -255, 0)")))

(define_constraint "L"
  "@internal"
  (and (match_code "const_int")
       (match_test "mmix_shiftable_wyde_value (ival)")))

(define_constraint "M"
  "The value 0."
  (and (match_code "const_int")
       (match_test "ival == 0")))

(define_constraint "N"
  "@internal"
  (and (match_code "const_int")
       (match_test "mmix_shiftable_wyde_value (~ival)")))

(define_constraint "O"
  "The value 3, 5, 9, or 17."
  (and (match_code "const_int")
       (ior (match_test "ival == 3")
	    (match_test "ival == 5")
	    (match_test "ival == 9")
	    (match_test "ival == 17"))))

;; FIXME: M (or G) is redundant.

(define_constraint "G"
  "Floating-point zero."
  (and (match_code "const_double")
       (match_test "op == CONST0_RTX (mode)")))

;; R asks whether x is to be loaded with GETA or something else.  Right
;; now, only a SYMBOL_REF and LABEL_REF can fit for
;; TARGET_BASE_ADDRESSES.
;;
;; Only constant symbolic addresses apply.  With TARGET_BASE_ADDRESSES,
;; we just allow straight LABEL_REF or SYMBOL_REFs with SYMBOL_REF_FLAG
;; set right now; only function addresses and code labels.  If we change
;; to let SYMBOL_REF_FLAG be set on other symbols, we have to check
;; inside CONST expressions.  When TARGET_BASE_ADDRESSES is not in
;; effect, a "raw" constant check together with mmix_constant_address_p
;; is all that's needed; we want all constant addresses to be loaded
;; with GETA then.

(define_constraint "R"
  "@internal"
  (and (not (match_code "const_int,const_double"))
       (match_test "mmix_constant_address_p (op)")
       (ior (match_test "!TARGET_BASE_ADDRESSES")
	    (match_code "label_ref")
	    (and (match_code "symbol_ref")
		 (match_test "SYMBOL_REF_FLAG (op)")))))

;; FIXME: L (or S) is redundant.

(define_constraint "S"
  "@internal"
  (and (match_code "const_int,const_double")
       (match_test "mmix_shiftable_wyde_value (mmix_intval (op))")))

;; FIXME: N (or T) is redundant.

(define_constraint "T"
  "@internal"
  (and (match_code "const_int,const_double")
       (match_test "mmix_shiftable_wyde_value (~mmix_intval (op))")))

(define_address_constraint "U"
  "@internal"
  (match_operand 0 "mmix_address_operand"))

(define_constraint "Yf"
  "@internal"
  (match_operand 0 "frame_pointer_operand"))
