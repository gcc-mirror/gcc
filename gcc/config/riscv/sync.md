;; Machine description for RISC-V atomic operations.
;; Copyright (C) 2011-2020 Free Software Foundation, Inc.
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

(define_c_enum "unspec" [
  UNSPEC_COMPARE_AND_SWAP
  UNSPEC_SYNC_OLD_OP
  UNSPEC_SYNC_EXCHANGE
  UNSPEC_ATOMIC_STORE
  UNSPEC_MEMORY_BARRIER
])

(define_code_iterator any_atomic [plus ior xor and])
(define_code_attr atomic_optab
  [(plus "add") (ior "or") (xor "xor") (and "and")])

;; Memory barriers.

(define_expand "mem_thread_fence"
  [(match_operand:SI 0 "const_int_operand" "")] ;; model
  ""
{
  if (INTVAL (operands[0]) != MEMMODEL_RELAXED)
    {
      rtx mem = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (Pmode));
      MEM_VOLATILE_P (mem) = 1;
      emit_insn (gen_mem_thread_fence_1 (mem, operands[0]));
    }
  DONE;
})

;; Until the RISC-V memory model (hence its mapping from C++) is finalized,
;; conservatively emit a full FENCE.
(define_insn "mem_thread_fence_1"
  [(set (match_operand:BLK 0 "" "")
	(unspec:BLK [(match_dup 0)] UNSPEC_MEMORY_BARRIER))
   (match_operand:SI 1 "const_int_operand" "")] ;; model
  ""
  "fence\tiorw,iorw")

;; Atomic memory operations.

;; Implement atomic stores with amoswap.  Fall back to fences for atomic loads.
(define_insn "atomic_store<mode>"
  [(set (match_operand:GPR 0 "memory_operand" "=A")
    (unspec_volatile:GPR
      [(match_operand:GPR 1 "reg_or_0_operand" "rJ")
       (match_operand:SI 2 "const_int_operand")]      ;; model
      UNSPEC_ATOMIC_STORE))]
  "TARGET_ATOMIC"
  "%F2amoswap.<amo>%A2 zero,%z1,%0"
  [(set (attr "length") (const_int 8))])

(define_insn "atomic_<atomic_optab><mode>"
  [(set (match_operand:GPR 0 "memory_operand" "+A")
	(unspec_volatile:GPR
	  [(any_atomic:GPR (match_dup 0)
		     (match_operand:GPR 1 "reg_or_0_operand" "rJ"))
	   (match_operand:SI 2 "const_int_operand")] ;; model
	 UNSPEC_SYNC_OLD_OP))]
  "TARGET_ATOMIC"
  "%F2amo<insn>.<amo>%A2 zero,%z1,%0"
  [(set (attr "length") (const_int 8))])

(define_insn "atomic_fetch_<atomic_optab><mode>"
  [(set (match_operand:GPR 0 "register_operand" "=&r")
	(match_operand:GPR 1 "memory_operand" "+A"))
   (set (match_dup 1)
	(unspec_volatile:GPR
	  [(any_atomic:GPR (match_dup 1)
		     (match_operand:GPR 2 "reg_or_0_operand" "rJ"))
	   (match_operand:SI 3 "const_int_operand")] ;; model
	 UNSPEC_SYNC_OLD_OP))]
  "TARGET_ATOMIC"
  "%F3amo<insn>.<amo>%A3 %0,%z2,%1"
  [(set (attr "length") (const_int 8))])

(define_insn "atomic_exchange<mode>"
  [(set (match_operand:GPR 0 "register_operand" "=&r")
	(unspec_volatile:GPR
	  [(match_operand:GPR 1 "memory_operand" "+A")
	   (match_operand:SI 3 "const_int_operand")] ;; model
	  UNSPEC_SYNC_EXCHANGE))
   (set (match_dup 1)
	(match_operand:GPR 2 "register_operand" "0"))]
  "TARGET_ATOMIC"
  "%F3amoswap.<amo>%A3 %0,%z2,%1"
  [(set (attr "length") (const_int 8))])

(define_insn "atomic_cas_value_strong<mode>"
  [(set (match_operand:GPR 0 "register_operand" "=&r")
	(match_operand:GPR 1 "memory_operand" "+A"))
   (set (match_dup 1)
	(unspec_volatile:GPR [(match_operand:GPR 2 "reg_or_0_operand" "rJ")
			      (match_operand:GPR 3 "reg_or_0_operand" "rJ")
			      (match_operand:SI 4 "const_int_operand")  ;; mod_s
			      (match_operand:SI 5 "const_int_operand")] ;; mod_f
	 UNSPEC_COMPARE_AND_SWAP))
   (clobber (match_scratch:GPR 6 "=&r"))]
  "TARGET_ATOMIC"
  "%F5 1: lr.<amo>%A5 %0,%1; bne %0,%z2,1f; sc.<amo>%A4 %6,%z3,%1; bnez %6,1b; 1:"
  [(set (attr "length") (const_int 20))])

(define_expand "atomic_compare_and_swap<mode>"
  [(match_operand:SI 0 "register_operand" "")   ;; bool output
   (match_operand:GPR 1 "register_operand" "")  ;; val output
   (match_operand:GPR 2 "memory_operand" "")    ;; memory
   (match_operand:GPR 3 "reg_or_0_operand" "")  ;; expected value
   (match_operand:GPR 4 "reg_or_0_operand" "")  ;; desired value
   (match_operand:SI 5 "const_int_operand" "")  ;; is_weak
   (match_operand:SI 6 "const_int_operand" "")  ;; mod_s
   (match_operand:SI 7 "const_int_operand" "")] ;; mod_f
  "TARGET_ATOMIC"
{
  emit_insn (gen_atomic_cas_value_strong<mode> (operands[1], operands[2],
						operands[3], operands[4],
						operands[6], operands[7]));

  rtx compare = operands[1];
  if (operands[3] != const0_rtx)
    {
      rtx difference = gen_rtx_MINUS (<MODE>mode, operands[1], operands[3]);
      compare = gen_reg_rtx (<MODE>mode);
      emit_insn (gen_rtx_SET (compare, difference));
    }

  if (word_mode != <MODE>mode)
    {
      rtx reg = gen_reg_rtx (word_mode);
      emit_insn (gen_rtx_SET (reg, gen_rtx_SIGN_EXTEND (word_mode, compare)));
      compare = reg;
    }

  emit_insn (gen_rtx_SET (operands[0], gen_rtx_EQ (SImode, compare, const0_rtx)));
  DONE;
})

(define_expand "atomic_test_and_set"
  [(match_operand:QI 0 "register_operand" "")     ;; bool output
   (match_operand:QI 1 "memory_operand" "+A")    ;; memory
   (match_operand:SI 2 "const_int_operand" "")]   ;; model
  "TARGET_ATOMIC"
{
  /* We have no QImode atomics, so use the address LSBs to form a mask,
     then use an aligned SImode atomic. */
  rtx result = operands[0];
  rtx mem = operands[1];
  rtx model = operands[2];
  rtx addr = force_reg (Pmode, XEXP (mem, 0));

  rtx aligned_addr = gen_reg_rtx (Pmode);
  emit_move_insn (aligned_addr, gen_rtx_AND (Pmode, addr, GEN_INT (-4)));

  rtx aligned_mem = change_address (mem, SImode, aligned_addr);
  set_mem_alias_set (aligned_mem, 0);

  rtx offset = gen_reg_rtx (SImode);
  emit_move_insn (offset, gen_rtx_AND (SImode, gen_lowpart (SImode, addr),
				       GEN_INT (3)));

  rtx tmp = gen_reg_rtx (SImode);
  emit_move_insn (tmp, GEN_INT (1));

  rtx shmt = gen_reg_rtx (SImode);
  emit_move_insn (shmt, gen_rtx_ASHIFT (SImode, offset, GEN_INT (3)));

  rtx word = gen_reg_rtx (SImode);
  emit_move_insn (word, gen_rtx_ASHIFT (SImode, tmp,
					gen_lowpart (QImode, shmt)));

  tmp = gen_reg_rtx (SImode);
  emit_insn (gen_atomic_fetch_orsi (tmp, aligned_mem, word, model));

  emit_move_insn (gen_lowpart (SImode, result),
		  gen_rtx_LSHIFTRT (SImode, tmp,
				    gen_lowpart (QImode, shmt)));
  DONE;
})
