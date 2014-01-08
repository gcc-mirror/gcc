;; Toshiba Media Processor Machine constraints
;; Copyright (C) 2009-2014 Free Software Foundation, Inc.
;; Contributed by Red Hat Inc.
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



(define_register_constraint "a" "SP_REGS"
  "The $sp register.")

(define_register_constraint "b" "TP_REGS"
  "The $tp register.")

(define_register_constraint "c" "CONTROL_REGS"
  "Any control register.")

(define_register_constraint "d" "HILO_REGS"
  "Either the $hi or the $lo register.")

(define_register_constraint "em" "LOADABLE_CR_REGS"
  "Coprocessor registers that can be directly loaded ($c0-$c15).")

(define_register_constraint "ex" "mep_have_copro_copro_moves_p ? CR_REGS : NO_REGS"
  "Coprocessor registers that can be moved to each other.")

(define_register_constraint "er" "mep_have_core_copro_moves_p ? CR_REGS : NO_REGS"
  "Coprocessor registers that can be moved to core registers.")

(define_register_constraint "h" "HI_REGS"
  "The $hi register.")

(define_register_constraint "j" "RPC_REGS"
  "The $rpc register.")

(define_register_constraint "l" "LO_REGS"
  "The $lo register.")

(define_register_constraint "t" "TPREL_REGS"
  "Registers which can be used in $tp-relative addressing.")

(define_register_constraint "v" "GP_REGS"
  "The $gp register.")

(define_register_constraint "x" "CR_REGS"
  "The coprocessor registers.")

(define_register_constraint "y" "CCR_REGS"
  "The coprocessor control registers.")

(define_register_constraint "z" "R0_REGS"
  "The $0 register.")

(define_register_constraint "A" "USER0_REGS"
  "User-defined register set A.")

(define_register_constraint "B" "USER1_REGS"
  "User-defined register set B.")

(define_register_constraint "C" "USER2_REGS"
  "User-defined register set C.")

(define_register_constraint "D" "USER3_REGS"
  "User-defined register set D.")



(define_constraint "I"
  "Offsets for $gp-rel addressing."
  (and (match_code "const_int")
       (match_test "ival >= -32768 && ival < 32768")))

(define_constraint "J"
  "Constants that can be used directly with boolean insns."
  (and (match_code "const_int")
       (match_test "ival >= 0 && ival < 65536")))

(define_constraint "K"
  "Constants that can be moved directly to registers."
  (and (match_code "const_int")
       (match_test "ival >= 0 && ival < 0x01000000")))

(define_constraint "L"
  "Small constants that can be added to registers."
  (and (match_code "const_int")
       (match_test "ival >= -32 && ival < 32")))

(define_constraint "M"
  "Long shift counts."
  (and (match_code "const_int")
       (match_test "ival >= 0 && ival < 32")))

(define_constraint "N"
  "Small constants that can be compared to registers."
  (and (match_code "const_int")
       (match_test "ival >= 0 && ival < 16")))

(define_constraint "O"
  "Constants that can be loaded into the top half of registers."
  (and (match_code "const_int")
       (match_test "!(ival & 0xffff) && ival >= -2147483647-1 && ival <= 2147483647")))

(define_constraint "S"
  "Signed 8-bit immediates."
  (and (match_code "const_int")
       (match_test "ival >= -128 && ival < 127")))



;; This must only be used with mep_call_address_operand() as the predicate.
(define_constraint "R"
  "@internal
Near symbols that can be used as addresses for CALL."
  (not (match_code "reg")))

(define_constraint "T"
  "Symbols encoded for $tp-rel or $gp-rel addressing."
  (ior (ior
	(and (match_code "unspec")
	     (match_code "symbol_ref" "a"))
	(and (match_code "const")
	     (and (match_code "unspec" "0")
		  (match_code "symbol_ref" "0a"))))
       (and (match_code "const")
	    (and (match_code "plus" "0")
		 (and (match_code "unspec" "00")
		      (match_code "symbol_ref" "00a"))))))

(define_constraint "U"
  "Non-constant addresses for loading/saving coprocessor registers."
  (and (match_code "mem")
       (match_test "! CONSTANT_P (XEXP (op, 0))")))

(define_constraint "W"
  "The top half of a symbol's value."
  (and (match_code "high")
       (match_code "symbol_ref" "0")))

(define_constraint "Y"
  "A register indirect address without offset."
  (and (match_code "mem")
       (match_code "reg" "0")))

(define_constraint "Z"
  "Symbolic references to the control bus."
  (and (and (match_code "mem")
	    (match_code "symbol_ref" "0"))
       (match_test "mep_section_tag (op) == 'c'")))
