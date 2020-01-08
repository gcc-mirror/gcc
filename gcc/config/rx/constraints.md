;; Constraint definitions for Renesas RX.
;; Copyright (C) 2008-2020 Free Software Foundation, Inc.
;; Contributed by Red Hat.
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


(define_constraint "Symbol"
  "@internal Constraint on the type of rtx allowed in call insns"
  (match_test "GET_CODE (op) == SYMBOL_REF")
)


(define_constraint "Int08"
  "@internal A signed or unsigned 8-bit immediate value"
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, (HOST_WIDE_INT_M1U << 8), (1 << 8) - 1)")
  )
)

(define_constraint "Sint08"
  "@internal A signed 8-bit immediate value"
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, (HOST_WIDE_INT_M1U << 7), (1 << 7) - 1)")
  )
)

(define_constraint "Sint16"
  "@internal A signed 16-bit immediate value"
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, (HOST_WIDE_INT_M1U << 15), (1 << 15) - 1)")
  )
)

(define_constraint "Sint24"
  "@internal A signed 24-bit immediate value"
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, (HOST_WIDE_INT_M1U << 23), (1 << 23) - 1)")
  )
)

;; This constraint is used by the SUBSI3 pattern because the
;; RX SUB instruction can only take a 4-bit unsigned integer
;; value.  Also used by the MVTIPL instruction.
(define_constraint "Uint04"
  "@internal An unsigned 4-bit immediate value"
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, 0, 15)")
  )
)

(define_constraint "NEGint4"
  "@internal An signed 4-bit negative immediate value"
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, -15, -1)")
  )
)

;; This is used in arithmetic and logic instructions for
;; a source operand that lies in memory and which satisfies
;; rx_restricted_memory_address().

(define_memory_constraint "Q"
  "A MEM which only uses REG or REG+INT addressing."
  (and (match_code "mem")
       (ior (match_code "reg" "0")
	    (and (match_code "plus" "0")
	         (and (match_code "reg,subreg" "00")
		      (match_code "const_int" "01")
		 )
	    )
       )
  )
)

(define_constraint "Rpid"
  "A MEM to a PID variable"
  (and (match_code "mem")
       (and (match_code "plus" "0")
	    (and (match_code "reg,subreg" "00")
		 (match_code "unspec" "01")
	    )
       )
  )
)

(define_constraint "Rpda"
  "An address to a PID variable"
  (and (match_code "plus" "")
       (and (match_code "reg,subreg" "0")
	    (match_code "unspec" "1")
       )
  )
)

(define_constraint "CALL_OP_SYMBOL_REF"
"constraint for call instructions using symbol ref"
(and (match_test "!TARGET_JSR")
     (match_code "symbol_ref"))
)
