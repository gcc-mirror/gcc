;; PIC codegen for RISC-V for GNU compiler.
;; Copyright (C) 2011-2021 Free Software Foundation, Inc.
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

(define_insn "*local_pic_load<mode>"
  [(set (match_operand:ANYI 0 "register_operand" "=r")
	(mem:ANYI (match_operand 1 "absolute_symbolic_operand" "")))]
  "USE_LOAD_ADDRESS_MACRO (operands[1])"
  "<default_load>\t%0,%1"
  [(set (attr "length") (const_int 8))])

(define_insn "*local_pic_load_s<SUBX:mode>"
  [(set (match_operand:SUPERQI 0 "register_operand" "=r")
	(sign_extend:SUPERQI (mem:SUBX (match_operand 1 "absolute_symbolic_operand" ""))))]
  "USE_LOAD_ADDRESS_MACRO (operands[1])"
  "<SUBX:load>\t%0,%1"
  [(set (attr "length") (const_int 8))])

(define_insn "*local_pic_load_u<SUBX:mode>"
  [(set (match_operand:SUPERQI 0 "register_operand" "=r")
	(zero_extend:SUPERQI (mem:SUBX (match_operand 1 "absolute_symbolic_operand" ""))))]
  "USE_LOAD_ADDRESS_MACRO (operands[1])"
  "<SUBX:load>u\t%0,%1"
  [(set (attr "length") (const_int 8))])

;; We can support ANYF loads into X register if there is no double support
;; or if the target is 64-bit.

(define_insn "*local_pic_load<ANYF:mode>"
  [(set (match_operand:ANYF 0 "register_operand" "=f,*r")
	(mem:ANYF (match_operand 1 "absolute_symbolic_operand" "")))
   (clobber (match_scratch:P 2 "=r,X"))]
  "TARGET_HARD_FLOAT && USE_LOAD_ADDRESS_MACRO (operands[1])
   && (!TARGET_DOUBLE_FLOAT || TARGET_64BIT)"
  "@
   <ANYF:load>\t%0,%1,%2
   <softload>\t%0,%1"
  [(set (attr "length") (const_int 8))])

;; ??? For a 32-bit target with double float, a DF load into a X reg isn't
;; supported.  ld is not valid in that case.  Punt for now.  Maybe add a split
;; for this later.

(define_insn "*local_pic_load_32d<ANYF:mode>"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(mem:ANYF (match_operand 1 "absolute_symbolic_operand" "")))
   (clobber (match_scratch:P 2 "=r"))]
  "TARGET_HARD_FLOAT && USE_LOAD_ADDRESS_MACRO (operands[1])
   && (TARGET_DOUBLE_FLOAT && !TARGET_64BIT)"
  "<ANYF:load>\t%0,%1,%2"
  [(set (attr "length") (const_int 8))])

(define_insn "*local_pic_load_sf<mode>"
  [(set (match_operand:SOFTF 0 "register_operand" "=r")
	(mem:SOFTF (match_operand 1 "absolute_symbolic_operand" "")))]
  "!TARGET_HARD_FLOAT && USE_LOAD_ADDRESS_MACRO (operands[1])"
  "<softload>\t%0,%1"
  [(set (attr "length") (const_int 8))])

;; Simplify PIC stores to static variables.
;; These should go away once we figure out how to emit auipc discretely.

(define_insn "*local_pic_store<ANYI:mode>"
  [(set (mem:ANYI (match_operand 0 "absolute_symbolic_operand" ""))
	(match_operand:ANYI 1 "reg_or_0_operand" "rJ"))
   (clobber (match_scratch:P 2 "=&r"))]
  "USE_LOAD_ADDRESS_MACRO (operands[0])"
  "<ANYI:store>\t%z1,%0,%2"
  [(set (attr "length") (const_int 8))])

(define_insn "*local_pic_store<ANYF:mode>"
  [(set (mem:ANYF (match_operand 0 "absolute_symbolic_operand" ""))
	(match_operand:ANYF 1 "register_operand" "f,*r"))
   (clobber (match_scratch:P 2 "=r,&r"))]
  "TARGET_HARD_FLOAT && USE_LOAD_ADDRESS_MACRO (operands[0])
   && (!TARGET_DOUBLE_FLOAT || TARGET_64BIT)"
  "@
   <ANYF:store>\t%1,%0,%2
   <softstore>\t%1,%0,%2"
  [(set (attr "length") (const_int 8))])

;; ??? For a 32-bit target with double float, a DF store from a X reg isn't
;; supported.  sd is not valid in that case.  Punt for now.  Maybe add a split
;; for this later.

(define_insn "*local_pic_store_32d<ANYF:mode>"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(mem:ANYF (match_operand 1 "absolute_symbolic_operand" "")))
   (clobber (match_scratch:P 2 "=r"))]
  "TARGET_HARD_FLOAT && USE_LOAD_ADDRESS_MACRO (operands[1])
   && (TARGET_DOUBLE_FLOAT && !TARGET_64BIT)"
  "<ANYF:store>\t%1,%0,%2"
  [(set (attr "length") (const_int 8))])

(define_insn "*local_pic_store_sf<SOFTF:mode>"
  [(set (mem:SOFTF (match_operand 0 "absolute_symbolic_operand" ""))
	(match_operand:SOFTF 1 "register_operand" "r"))
   (clobber (match_scratch:P 2 "=&r"))]
  "!TARGET_HARD_FLOAT && USE_LOAD_ADDRESS_MACRO (operands[0])"
  "<softstore>\t%1,%0,%2"
  [(set (attr "length") (const_int 8))])
