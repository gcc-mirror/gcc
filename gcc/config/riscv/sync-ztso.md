;; Machine description for RISC-V atomic operations.
;; Copyright (C) 2011-2025 Free Software Foundation, Inc.
;; Contributed by Andrew Waterman (andrew@sifive.com).
;; Based on MIPS target for GNU compiler.

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

;; Memory barriers.

(define_insn "mem_thread_fence_ztso"
  [(set (match_operand:BLK 0 "" "")
	(unspec:BLK [(match_dup 0)] UNSPEC_MEMORY_BARRIER))
   (match_operand:SI 1 "const_int_operand" "")]  ;; model
  "TARGET_ZTSO"
  {
    enum memmodel model = (enum memmodel) INTVAL (operands[1]);
    model = memmodel_base (model);

    if (model == MEMMODEL_SEQ_CST)
	return "fence\trw,rw";
    else
	gcc_unreachable ();
  }
  [(set_attr "type" "atomic")
   (set (attr "length") (const_int 4))])

;; Atomic memory operations.

(define_insn "atomic_load_ztso<mode>"
  [(set (match_operand:ANYI 0 "register_operand" "=r")
	(unspec_volatile:ANYI
	    [(match_operand:ANYI 1 "memory_operand" "A")
	     (match_operand:SI 2 "const_int_operand")]  ;; model
	 UNSPEC_ATOMIC_LOAD))]
  "TARGET_ZTSO"
  {
    enum memmodel model = (enum memmodel) INTVAL (operands[2]);
    model = memmodel_base (model);

    if (model == MEMMODEL_SEQ_CST)
      return "fence\trw,rw\;"
	     "<load>\t%0,%1";
    else
      return "<load>\t%0,%1";
  }
  [(set_attr "type" "multi")
   (set (attr "length")
	(symbol_ref "(is_mm_seq_cst (memmodel_from_int (INTVAL (operands[2]))) ? 8
		      : 4)"))])


(define_insn "atomic_store_ztso<mode>"
  [(set (match_operand:ANYI 0 "memory_operand" "=A")
	(unspec_volatile:ANYI
	    [(match_operand:ANYI 1 "reg_or_0_operand" "rJ")
	     (match_operand:SI 2 "const_int_operand")]  ;; model
	 UNSPEC_ATOMIC_STORE))]
  "TARGET_ZTSO"
  {
    enum memmodel model = (enum memmodel) INTVAL (operands[2]);
    model = memmodel_base (model);

    if (model == MEMMODEL_SEQ_CST)
      return "<store>\t%z1,%0\;"
	     "fence\trw,rw";
    else
      return "<store>\t%z1,%0";
  }
  [(set_attr "type" "multi")
   (set (attr "length")
	(symbol_ref "(is_mm_seq_cst (memmodel_from_int (INTVAL (operands[2]))) ? 8
		      : 4)"))])
