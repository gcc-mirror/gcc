;; Constraints for the DEC VAX port.
;; Copyright (C) 2007-2019 Free Software Foundation, Inc.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; GCC is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.  */

(define_constraint "Z0"
   "Match a CONST_INT of 0"
   (and (match_code "const_int")
	(match_test "ival == 0")))

(define_constraint "U06"
   "unsigned 6 bit value (0..63)"
   (and (match_code "const_int")
	(match_test "ival >= 0 && ival < 64")))

(define_constraint "U08"
   "Unsigned 8 bit value"
   (and (match_code "const_int")
	(match_test "ival >= 0 && ival < 256")))

(define_constraint "U16"
   "Unsigned 16 bit value"
   (and (match_code "const_int")
	(match_test "ival >= 0 && ival < 65536")))

(define_constraint "CN6"
   "negative 6 bit value (-63..-1)"
   (and (match_code "const_int")
	(match_test "ival >= -63 && ival < 0")))

(define_constraint "S08"
   "signed 8 bit value [old]"
   (and (match_code "const_int")
	(match_test "ival >= -128 && ival < 128")))

(define_constraint "S16"
   "signed 16 bit value [old]"
   (and (match_code "const_int")
	(match_test "ival >= -32768 && ival < 32768")))

(define_constraint "I"
   "Match a CONST_INT of 0 [old]"
   (and (match_code "const_int")
	(match_test "satisfies_constraint_Z0 (GEN_INT (ival))")))

(define_constraint "J"
   "unsigned 6 bit value [old]"
   (and (match_code "const_int")
	(match_test "satisfies_constraint_U06 (GEN_INT (ival))")))

(define_constraint "K"
   "signed 8 bit value [old]"
   (and (match_code "const_int")
	(match_test "satisfies_constraint_S08 (GEN_INT (ival))")))

(define_constraint "L"
   "signed 16 bit value [old]"
   (and (match_code "const_int")
	(match_test "satisfies_constraint_S16 (GEN_INT (ival))")))

(define_constraint "M"
   "Unsigned 8 bit value [old]"
   (and (match_code "const_int")
	(match_test "satisfies_constraint_U08 (GEN_INT (ival))")))

(define_constraint "N"
   "Unsigned 16 bit value [old]"
   (and (match_code "const_int")
	(match_test "satisfies_constraint_U16 (GEN_INT (ival))")))

(define_constraint "O"
   "Negative short literals (-63..-1) [old]"
   (and (match_code "const_int")
	(match_test "satisfies_constraint_CN6 (GEN_INT (ival))")))

/* Similar, but for floating constants, and defining letters G and H.  */

(define_constraint "G"
  "Match a floating-point zero"
  (and (match_code "const_double")
       (match_test "op == CONST0_RTX (DFmode) || op == CONST0_RTX (SFmode)")))

/* Optional extra constraints for this machine. */

(define_memory_constraint "Q"
   "operand is a MEM that does not have a mode-dependent address."
   (and (match_code "mem")
	(match_test "!mode_dependent_address_p (XEXP (op, 0),
					        MEM_ADDR_SPACE (op))")))

(define_memory_constraint "B"
    ""
    (and (match_operand:BLK 0 "memory_operand")
	 (not (match_operand:BLK 0 "illegal_blk_memory_operand" ""))))

(define_memory_constraint "R"
    ""
    (and (match_operand:DI 0 "memory_operand")
	 (not (match_operand:DI 0 "illegal_addsub_di_memory_operand" ""))))

(define_constraint "T"
    "@internal satisfies CONSTANT_P and, if pic is enabled, is not a SYMBOL_REF, LABEL_REF, or CONST."
  (and (match_test ("CONSTANT_P (op)"))
       (ior (not (match_code "symbol_ref,label_ref,const"))
	    (match_test "!flag_pic"))))
