;; Constraint definitions for SPU
;; Copyright (C) 2006, 2007 Free Software Foundation, Inc.
;;
;; This file is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3 of the License, or (at your option) 
;; any later version.

;; This file is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.


;; GCC standard constraints:  g, i, m, n, o, p, r, s, E-H, I-P, V, X
;; unused for SPU:  E-H, L, Q, d, e, h, q, t-z

;; For most immediate constraints we have 3 variations to deal with the
;; fact const_int has no mode.  One variation treats const_int as 32 bit,
;; another treats it as 64 bit, and the third sign extends it to 128 bit.

(define_constraint "A"
  "An immediate which can be loaded with the il/ila/ilh/ilhu instructions.  const_int is treated as a 32-bit value."
  (ior (and (match_code "const_int,const_double,const_vector")
	    (match_test "immediate_load_p (op, SImode)"))
       (match_code "symbol_ref,label_ref,high,const")))

(define_constraint "B"
  "An immediate for arithmetic instructions (e.g., ai, ceqi).  const_int is treated as a 32-bit value."
  (and (match_code "const_int,const_double,const_vector")
       (match_test "arith_immediate_p (op, SImode, -0x200, 0x1ff)")))

(define_constraint "C"
  "An immediate for and/xor/or instructions.  const_int is treated as a 32-bit value."
  (and (match_code "const_int,const_double,const_vector")
       (match_test "logical_immediate_p (op, SImode)")))

(define_constraint "D"
  "An immediate for iohl instruction.  const_int is treated as a 32-bit value."
  (and (match_code "const_int,const_double,const_vector")
       (match_test "iohl_immediate_p (op, SImode)")))

(define_constraint "U"
  "An immediate which can be loaded with the il/ila/ilh/ilhu instructions.  const_int is sign extended to 128 bit."
  (and (match_code "const_int,const_double,const_vector")
       (match_test "immediate_load_p (op, TImode)")))

(define_constraint "W"
  "An immediate for shift and rotate instructions.  const_int is treated as a 32-bit value."
  (and (match_code "const_int,const_double,const_vector")
       (match_test "arith_immediate_p (op, SImode, -0x80000000ll, 0x7fffffffll)")))

(define_constraint "Y"
  "An immediate for and/xor/or instructions.  const_int is sign extended as a 128 bit."
  (and (match_code "const_int,const_double,const_vector")
       (match_test "logical_immediate_p (op, TImode)")))

(define_constraint "Z"
  "An immediate for iohl instruction.  const_int is sign extended to 128 bit."
  (and (match_code "const_int,const_double,const_vector")
       (match_test "iohl_immediate_p (op, TImode)")))

(define_constraint "a"
  "An immediate which can be loaded with the il/ila/ilh/ilhu instructions.  const_int is treated as a 64-bit value."
  (and (match_code "const_int")
       (match_test "immediate_load_p (op, DImode)")))

(define_constraint "c"
  "An immediate for and/xor/or instructions.  const_int is treated as a 64-bit value."
  (and (match_code "const_int")
       (match_test "logical_immediate_p (op, DImode)")))

(define_constraint "d"
  "An immediate for iohl instruction.  const_int is treated as a 64-bit value."
  (and (match_code "const_int")
       (match_test "iohl_immediate_p (op, DImode)")))

(define_constraint "f"
  "An immediate which can be loaded with fsmbi."
  (and (match_code "const_int,const_double,const_vector")
       (match_test "fsmbi_const_p (op)")))

(define_constraint "j"
  "An immediate which can be loaded with one of the cbd/chd/cwd/cdd instructions.  const_int is treated as a 32-bit value."
  (and (match_code "const_int,const_double,const_vector")
       (match_test "cpat_const_p (op, SImode)")))

(define_constraint "k"
  "An immediate which can be loaded with one of the cbd/chd/cwd/cdd instructions.  const_int is treated as a 64-bit value."
  (and (match_code "const_int,const_double,const_vector")
       (match_test "cpat_const_p (op, DImode)")))

(define_constraint "l"
  "An immediate which can be loaded with one of the cbd/chd/cwd/cdd instructions."
  (and (match_code "const_double,const_vector")
       (match_test "cpat_const_p (op, TImode)")))


;; Integer constraints

(define_constraint "I"
  "A constant in the range [-64, 63] for shift/rotate instructions."
  (and (match_code "const_int")
       (match_test "ival >= -0x40 && ival <= 0x3f")))

(define_constraint "J"
  "An unsigned 7-bit constant for conversion/nop/channel instructions."
  (and (match_code "const_int")
       (match_test "ival >= 0 && ival <= 0x7f")))

(define_constraint "K"
  "A signed 10-bit constant for most arithmetic instructions."
  (and (match_code "const_int")
       (match_test "ival >= -0x200 && ival <= 0x1ff")))
 
(define_constraint "M"
  "A signed 16-bit immediate for @code{stop}."
  (and (match_code "const_int")
       (match_test "ival >= -0x8000ll && ival <= 0x7fffll")))

(define_constraint "N"
  "An unsigned 16-bit constant for @code{iohl} and @code{fsmbi}."
  (and (match_code "const_int")
       (match_test "ival >= 0 && ival <= 0xffff")))

(define_constraint "O"
  "An unsigned 7-bit constant whose 3 least significant bits are 0."
  (and (match_code "const_int")
       (match_test "(ival & 7) == 0")))

(define_constraint "P"
  "An unsigned 3-bit constant for 16-byte rotates and shifts"
  (and (match_code "const_int")
       (match_test "ival >= 0 && ival <= 7")))


;; Memory constraints

(define_memory_constraint "R"
  "Call operand, reg, for indirect calls"
  (and (match_code "mem")
       (match_test "GET_CODE(XEXP(op, 0)) == REG")))

(define_memory_constraint "S"
  "Call operand, symbol, for relative calls."
  (and (match_code "mem")
       (match_test "!TARGET_LARGE_MEM
		    && ((GET_CODE (XEXP (op, 0)) == SYMBOL_REF
			 || GET_CODE (XEXP (op, 0)) == LABEL_REF))")))

(define_memory_constraint "T"
  "Call operand, const_int, for absolute calls."
  (and (match_code "mem")
       (match_test "GET_CODE (XEXP (op, 0)) == CONST_INT
		    && INTVAL (XEXP (op, 0)) >= 0
		    && INTVAL (XEXP (op, 0)) <= 0x3ffff")))


