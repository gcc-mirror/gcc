;; PIC codegen for RISC-V for GNU compiler.
;; Copyright (C) 2011-2017 Free Software Foundation, Inc.
;; Contributed by Andrew Waterman (andrew@sifive.com).

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.


;; Simplify PIC loads to static variables.
;; These should go away once we figure out how to emit auipc discretely.

(define_insn "*local_pic_load_s<mode>"
  [(set (match_operand:ANYI 0 "register_operand" "=r")
	(sign_extend:ANYI (mem:ANYI (match_operand 1 "absolute_symbolic_operand" ""))))]
  "USE_LOAD_ADDRESS_MACRO (operands[1])"
  "<load>\t%0,%1"
  [(set (attr "length") (const_int 8))])

(define_insn "*local_pic_load_u<mode>"
  [(set (match_operand:ZERO_EXTEND_LOAD 0 "register_operand" "=r")
	(zero_extend:ZERO_EXTEND_LOAD (mem:ZERO_EXTEND_LOAD (match_operand 1 "absolute_symbolic_operand" ""))))]
  "USE_LOAD_ADDRESS_MACRO (operands[1])"
  "<load>u\t%0,%1"
  [(set (attr "length") (const_int 8))])

(define_insn "*local_pic_load<mode>"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(mem:ANYF (match_operand 1 "absolute_symbolic_operand" "")))
   (clobber (match_scratch:DI 2 "=r"))]
  "TARGET_HARD_FLOAT && TARGET_64BIT && USE_LOAD_ADDRESS_MACRO (operands[1])"
  "<load>\t%0,%1,%2"
  [(set (attr "length") (const_int 8))])

(define_insn "*local_pic_load<mode>"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(mem:ANYF (match_operand 1 "absolute_symbolic_operand" "")))
   (clobber (match_scratch:SI 2 "=r"))]
  "TARGET_HARD_FLOAT && !TARGET_64BIT && USE_LOAD_ADDRESS_MACRO (operands[1])"
  "<load>\t%0,%1,%2"
  [(set (attr "length") (const_int 8))])

(define_insn "*local_pic_loadu<mode>"
  [(set (match_operand:SUPERQI 0 "register_operand" "=r")
	(zero_extend:SUPERQI (mem:SUBX (match_operand 1 "absolute_symbolic_operand" ""))))]
  "USE_LOAD_ADDRESS_MACRO (operands[1])"
  "<load>u\t%0,%1"
  [(set (attr "length") (const_int 8))])

(define_insn "*local_pic_storedi<mode>"
  [(set (mem:ANYI (match_operand 0 "absolute_symbolic_operand" ""))
	(match_operand:ANYI 1 "reg_or_0_operand" "rJ"))
   (clobber (match_scratch:DI 2 "=&r"))]
  "TARGET_64BIT && USE_LOAD_ADDRESS_MACRO (operands[0])"
  "<store>\t%z1,%0,%2"
  [(set (attr "length") (const_int 8))])

(define_insn "*local_pic_storesi<mode>"
  [(set (mem:ANYI (match_operand 0 "absolute_symbolic_operand" ""))
	(match_operand:ANYI 1 "reg_or_0_operand" "rJ"))
   (clobber (match_scratch:SI 2 "=&r"))]
  "!TARGET_64BIT && USE_LOAD_ADDRESS_MACRO (operands[0])"
  "<store>\t%z1,%0,%2"
  [(set (attr "length") (const_int 8))])

(define_insn "*local_pic_storedi<mode>"
  [(set (mem:ANYF (match_operand 0 "absolute_symbolic_operand" ""))
	(match_operand:ANYF 1 "register_operand" "f"))
   (clobber (match_scratch:DI 2 "=r"))]
  "TARGET_HARD_FLOAT && TARGET_64BIT && USE_LOAD_ADDRESS_MACRO (operands[0])"
  "<store>\t%1,%0,%2"
  [(set (attr "length") (const_int 8))])

(define_insn "*local_pic_storesi<mode>"
  [(set (mem:ANYF (match_operand 0 "absolute_symbolic_operand" ""))
	(match_operand:ANYF 1 "register_operand" "f"))
   (clobber (match_scratch:SI 2 "=r"))]
  "TARGET_HARD_FLOAT && !TARGET_64BIT && USE_LOAD_ADDRESS_MACRO (operands[0])"
  "<store>\t%1,%0,%2"
  [(set (attr "length") (const_int 8))])
