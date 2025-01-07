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

(define_c_enum "unspec" [
  UNSPEC_COMPARE_AND_SWAP
  UNSPEC_COMPARE_AND_SWAP_SUBWORD
  UNSPEC_SYNC_OLD_OP
  UNSPEC_SYNC_OLD_OP_SUBWORD
  UNSPEC_SYNC_OLD_OP_ZABHA
  UNSPEC_SYNC_EXCHANGE
  UNSPEC_SYNC_EXCHANGE_SUBWORD
  UNSPEC_SYNC_EXCHANGE_ZABHA
  UNSPEC_ATOMIC_LOAD
  UNSPEC_ATOMIC_STORE
  UNSPEC_MEMORY_BARRIER
])

;; Memory barriers.

(define_expand "mem_thread_fence"
  [(match_operand:SI 0 "const_int_operand" "")] ;; model
  ""
  {
    enum memmodel model = memmodel_base (INTVAL (operands[0]));

    if (TARGET_ZTSO && model == MEMMODEL_SEQ_CST)
      {
	rtx mem = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (Pmode));
	MEM_VOLATILE_P (mem) = 1;
	emit_insn (gen_mem_thread_fence_ztso (mem, operands[0]));
      }
    else if (!TARGET_ZTSO && model != MEMMODEL_RELAXED)
      {
	rtx mem = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (Pmode));
	MEM_VOLATILE_P (mem) = 1;
	emit_insn (gen_mem_thread_fence_rvwmo (mem, operands[0]));
      }
    DONE;
  })

;; Atomic memory operations.

(define_expand "atomic_load<mode>"
  [(match_operand:ANYI 0 "register_operand")
   (match_operand:ANYI 1 "memory_operand")
   (match_operand:SI 2 "const_int_operand")] ;; model
  ""
  {
    if (TARGET_ZTSO)
      emit_insn (gen_atomic_load_ztso<mode> (operands[0], operands[1],
					     operands[2]));
    else
      emit_insn (gen_atomic_load_rvwmo<mode> (operands[0], operands[1],
					      operands[2]));
    DONE;
  })

(define_expand "atomic_store<mode>"
  [(match_operand:ANYI 0 "memory_operand")
   (match_operand:ANYI 1 "reg_or_0_operand")
   (match_operand:SI 2 "const_int_operand")] ;; model
  ""
  {
    if (TARGET_ZTSO)
      emit_insn (gen_atomic_store_ztso<mode> (operands[0], operands[1],
					      operands[2]));
    else
      emit_insn (gen_atomic_store_rvwmo<mode> (operands[0], operands[1],
					       operands[2]));
    DONE;
  })

;; AMO ops

(define_insn "atomic_<atomic_optab><mode>"
  [(set (match_operand:SHORT 0 "memory_operand" "+A")
	(unspec_volatile:SHORT
	  [(any_atomic:SHORT (match_dup 0)
		     (match_operand:SHORT 1 "reg_or_0_operand" "rJ"))
	   (match_operand:SI 2 "const_int_operand")] ;; model
	 UNSPEC_SYNC_OLD_OP_ZABHA))]
  "TARGET_ZABHA"
  "amo<insn>.<amobh>%A2\tzero,%z1,%0"
  [(set_attr "type" "atomic")
   (set (attr "length") (const_int 4))])

(define_expand "atomic_<atomic_optab><mode>"
  [(any_atomic:GPR (match_operand:GPR 0 "memory_operand")    ;; mem location
		   (match_operand:GPR 1 "reg_or_0_operand")) ;; value for op
   (match_operand:SI 2 "const_int_operand")]		     ;; model
  "TARGET_ZAAMO || TARGET_ZALRSC"
{
  if (TARGET_ZAAMO)
    emit_insn (gen_amo_atomic_<atomic_optab><mode> (operands[0], operands[1],
						    operands[2]));
  else
    emit_insn (gen_lrsc_atomic_<atomic_optab><mode> (operands[0], operands[1],
						     operands[2]));
  DONE;
})

(define_insn "amo_atomic_<atomic_optab><mode>"
  [(set (match_operand:GPR 0 "memory_operand" "+A")
	(unspec_volatile:GPR
	  [(any_atomic:GPR (match_dup 0)
		     (match_operand:GPR 1 "reg_or_0_operand" "rJ"))
	   (match_operand:SI 2 "const_int_operand")] ;; model
	 UNSPEC_SYNC_OLD_OP))]
  "TARGET_ZAAMO"
  "amo<insn>.<amo>%A2\tzero,%z1,%0"
  [(set_attr "type" "atomic")
   (set (attr "length") (const_int 4))])

(define_insn "lrsc_atomic_<atomic_optab><mode>"
  [(set (match_operand:GPR 0 "memory_operand" "+A")
	(unspec_volatile:GPR
	  [(any_atomic:GPR (match_dup 0)
		     (match_operand:GPR 1 "reg_or_0_operand" "rJ"))
	   (match_operand:SI 2 "const_int_operand")] ;; model
	 UNSPEC_SYNC_OLD_OP))
   (clobber (match_scratch:GPR 3 "=&r"))]	     ;; tmp_1
  "!TARGET_ZAAMO && TARGET_ZALRSC"
  {
    return "1:\;"
	   "lr.<amo>%I2\t%3, %0\;"
	   "<insn>\t%3, %3, %1\;"
	   "sc.<amo>%J2\t%3, %3, %0\;"
	   "bnez\t%3, 1b";
  }
  [(set_attr "type" "atomic")
   (set (attr "length") (const_int 16))])

;; AMO fetch ops

(define_expand "atomic_fetch_<atomic_optab><mode>"
  [(match_operand:GPR 0 "register_operand")		     ;; old value at mem
   (any_atomic:GPR (match_operand:GPR 1 "memory_operand")    ;; mem location
		   (match_operand:GPR 2 "reg_or_0_operand")) ;; value for op
   (match_operand:SI 3 "const_int_operand")]		     ;; model
  "TARGET_ZAAMO || TARGET_ZALRSC"
  {
    if (TARGET_ZAAMO)
      emit_insn (gen_amo_atomic_fetch_<atomic_optab><mode> (operands[0], operands[1],
							    operands[2], operands[3]));
    else
      emit_insn (gen_lrsc_atomic_fetch_<atomic_optab><mode> (operands[0], operands[1],
							     operands[2], operands[3]));
    DONE;
  })

(define_insn "amo_atomic_fetch_<atomic_optab><mode>"
  [(set (match_operand:GPR 0 "register_operand" "=&r")
	(match_operand:GPR 1 "memory_operand" "+A"))
   (set (match_dup 1)
	(unspec_volatile:GPR
	  [(any_atomic:GPR (match_dup 1)
		     (match_operand:GPR 2 "reg_or_0_operand" "rJ"))
	   (match_operand:SI 3 "const_int_operand")] ;; model
	 UNSPEC_SYNC_OLD_OP))]
  "TARGET_ZAAMO"
  "amo<insn>.<amo>%A3\t%0,%z2,%1"
  [(set_attr "type" "atomic")
   (set (attr "length") (const_int 4))])

(define_insn "lrsc_atomic_fetch_<atomic_optab><mode>"
  [(set (match_operand:GPR 0 "register_operand" "=&r")
	(match_operand:GPR 1 "memory_operand" "+A"))
   (set (match_dup 1)
	(unspec_volatile:GPR
	  [(any_atomic:GPR (match_dup 1)
		     (match_operand:GPR 2 "reg_or_0_operand" "rJ"))
	   (match_operand:SI 3 "const_int_operand")] ;; model
	 UNSPEC_SYNC_OLD_OP))
   (clobber (match_scratch:GPR 4 "=&r"))]	  ;; tmp_1
  "!TARGET_ZAAMO && TARGET_ZALRSC"
  {
    return "1:\;"
	   "lr.<amo>%I3\t%0, %1\;"
	   "<insn>\t%4, %0, %2\;"
	   "sc.<amo>%J3\t%4, %4, %1\;"
	   "bnez\t%4, 1b";
  }
  [(set_attr "type" "atomic")
   (set (attr "length") (const_int 16))])

(define_insn "subword_atomic_fetch_strong_<atomic_optab>"
  [(set (match_operand:SI 0 "register_operand" "=&r")		   ;; old value at mem
	(match_operand:SI 1 "memory_operand" "+A"))		   ;; mem location
   (set (match_dup 1)
	(unspec_volatile:SI
	  [(any_atomic:SI (match_dup 1)
		     (match_operand:SI 2 "register_operand" "rI")) ;; value for op
	   (match_operand:SI 3 "const_int_operand")]		   ;; model
	 UNSPEC_SYNC_OLD_OP_SUBWORD))
    (match_operand:SI 4 "register_operand" "rI")		   ;; mask
    (match_operand:SI 5 "register_operand" "rI")		   ;; not_mask
    (clobber (match_scratch:SI 6 "=&r"))			   ;; tmp_1
    (clobber (match_scratch:SI 7 "=&r"))]			   ;; tmp_2
  "TARGET_ZALRSC && TARGET_INLINE_SUBWORD_ATOMIC"
  {
    return "1:\;"
	   "lr.w%I3\t%0, %1\;"
	   "<insn>\t%6, %0, %2\;"
	   "and\t%6, %6, %4\;"
	   "and\t%7, %0, %5\;"
	   "or\t%7, %7, %6\;"
	   "sc.w%J3\t%6, %7, %1\;"
	   "bnez\t%6, 1b";
  }
  [(set_attr "type" "multi")
   (set (attr "length") (const_int 28))])

(define_expand "atomic_fetch_nand<mode>"
  [(match_operand:SHORT 0 "register_operand")			      ;; old value at mem
   (not:SHORT (and:SHORT (match_operand:SHORT 1 "memory_operand")     ;; mem location
			 (match_operand:SHORT 2 "reg_or_0_operand"))) ;; value for op
   (match_operand:SI 3 "const_int_operand")]			      ;; model
  "TARGET_ZALRSC && TARGET_INLINE_SUBWORD_ATOMIC"
{
  /* We have no QImode/HImode atomics, so form a mask, then use
     subword_atomic_fetch_strong_nand to implement a LR/SC version of the
     operation.  */

  /* Logic duplicated in gcc/libgcc/config/riscv/atomic.c for use when inlining
     is disabled.  */

  rtx old = gen_reg_rtx (SImode);
  rtx mem = operands[1];
  rtx value = operands[2];
  rtx model = operands[3];
  rtx aligned_mem = gen_reg_rtx (SImode);
  rtx shift = gen_reg_rtx (SImode);
  rtx mask = gen_reg_rtx (SImode);
  rtx not_mask = gen_reg_rtx (SImode);

  riscv_subword_address (mem, &aligned_mem, &shift, &mask, &not_mask);

  rtx shifted_value = gen_reg_rtx (SImode);
  riscv_lshift_subword (<MODE>mode, value, shift, &shifted_value);

  emit_insn (gen_subword_atomic_fetch_strong_nand (old, aligned_mem,
						   shifted_value, model,
						   mask, not_mask));

  emit_move_insn (old, gen_rtx_ASHIFTRT (SImode, old,
					 gen_lowpart (QImode, shift)));

  emit_move_insn (operands[0], gen_lowpart (<MODE>mode, old));

  DONE;
})

(define_insn "subword_atomic_fetch_strong_nand"
  [(set (match_operand:SI 0 "register_operand" "=&r")			  ;; old value at mem
	(match_operand:SI 1 "memory_operand" "+A"))			  ;; mem location
   (set (match_dup 1)
	(unspec_volatile:SI
	  [(not:SI (and:SI (match_dup 1)
			   (match_operand:SI 2 "register_operand" "rI"))) ;; value for op
	   (match_operand:SI 3 "const_int_operand")]			  ;; mask
	 UNSPEC_SYNC_OLD_OP_SUBWORD))
    (match_operand:SI 4 "register_operand" "rI")			  ;; mask
    (match_operand:SI 5 "register_operand" "rI")			  ;; not_mask
    (clobber (match_scratch:SI 6 "=&r"))				  ;; tmp_1
    (clobber (match_scratch:SI 7 "=&r"))]				  ;; tmp_2
  "TARGET_ZALRSC && TARGET_INLINE_SUBWORD_ATOMIC"
  {
    return "1:\;"
	   "lr.w%I3\t%0, %1\;"
	   "and\t%6, %0, %2\;"
	   "not\t%6, %6\;"
	   "and\t%6, %6, %4\;"
	   "and\t%7, %0, %5\;"
	   "or\t%7, %7, %6\;"
	   "sc.w%J3\t%6, %7, %1\;"
	   "bnez\t%6, 1b";
  }
  [(set_attr "type" "multi")
   (set (attr "length") (const_int 32))])

(define_expand "atomic_fetch_<atomic_optab><mode>"
  [(match_operand:SHORT 0 "register_operand")			 ;; old value at mem
   (any_atomic:SHORT (match_operand:SHORT 1 "memory_operand")	 ;; mem location
		     (match_operand:SHORT 2 "reg_or_0_operand")) ;; value for op
   (match_operand:SI 3 "const_int_operand")]			 ;; model
  "(TARGET_ZALRSC && TARGET_INLINE_SUBWORD_ATOMIC) || TARGET_ZABHA"
{
  if (TARGET_ZABHA)
    emit_insn(gen_zabha_atomic_fetch_<atomic_optab><mode> (operands[0], operands[1],
							   operands[2], operands[3]));
  else
    emit_insn(gen_lrsc_atomic_fetch_<atomic_optab><mode> (operands[0], operands[1],
							  operands[2], operands[3]));
  DONE;
})

(define_insn "zabha_atomic_fetch_<atomic_optab><mode>"
  [(set (match_operand:SHORT 0 "register_operand" "=&r")
	(match_operand:SHORT 1 "memory_operand" "+A"))
   (set (match_dup 1)
	(unspec_volatile:SHORT
	  [(any_atomic:SHORT (match_dup 1)
		     (match_operand:SHORT 2 "reg_or_0_operand" "rJ"))
	   (match_operand:SI 3 "const_int_operand")] ;; model
	 UNSPEC_SYNC_OLD_OP_ZABHA))]
   "TARGET_ZABHA"
   "amo<insn>.<amobh>%A3\t%0,%z2,%1"
   [(set_attr "type" "atomic")
    (set (attr "length") (const_int 4))])

(define_expand "lrsc_atomic_fetch_<atomic_optab><mode>"
  [(match_operand:SHORT 0 "register_operand")			 ;; old value at mem
   (any_atomic:SHORT (match_operand:SHORT 1 "memory_operand")	 ;; mem location
		     (match_operand:SHORT 2 "reg_or_0_operand")) ;; value for op
   (match_operand:SI 3 "const_int_operand")]			 ;; model
  "!TARGET_ZABHA && TARGET_ZALRSC && TARGET_INLINE_SUBWORD_ATOMIC"
{
  /* We have no QImode/HImode atomics, so form a mask, then use
     subword_atomic_fetch_strong_<mode> to implement a LR/SC version of the
     operation.  */

  /* Logic duplicated in gcc/libgcc/config/riscv/atomic.c for use when inlining
     is disabled.  */

  rtx old = gen_reg_rtx (SImode);
  rtx mem = operands[1];
  rtx value = operands[2];
  rtx model = operands[3];
  rtx aligned_mem = gen_reg_rtx (SImode);
  rtx shift = gen_reg_rtx (SImode);
  rtx mask = gen_reg_rtx (SImode);
  rtx not_mask = gen_reg_rtx (SImode);

  riscv_subword_address (mem, &aligned_mem, &shift, &mask, &not_mask);

  rtx shifted_value = gen_reg_rtx (SImode);
  riscv_lshift_subword (<MODE>mode, value, shift, &shifted_value);

  emit_insn (gen_subword_atomic_fetch_strong_<atomic_optab> (old, aligned_mem,
							     shifted_value,
							     model, mask,
							     not_mask));

  emit_move_insn (old, gen_rtx_ASHIFTRT (SImode, old,
					 gen_lowpart (QImode, shift)));

  emit_move_insn (operands[0], gen_lowpart (<MODE>mode, old));

  DONE;
})

; Atomic exchange ops

(define_expand "atomic_exchange<mode>"
  [(match_operand:GPR 0 "register_operand")  ;; old value at mem
   (match_operand:GPR 1 "memory_operand")    ;; mem location
   (match_operand:GPR 2 "register_operand")  ;; value for op
   (match_operand:SI 3 "const_int_operand")] ;; model
  "TARGET_ZAAMO || TARGET_ZALRSC"
  {
    if (TARGET_ZAAMO)
      emit_insn (gen_amo_atomic_exchange<mode> (operands[0], operands[1],
					    operands[2], operands[3]));
    else
      emit_insn (gen_lrsc_atomic_exchange<mode> (operands[0], operands[1],
					     operands[2], operands[3]));
    DONE;
  })

(define_insn "amo_atomic_exchange<mode>"
  [(set (match_operand:GPR 0 "register_operand" "=&r")
	(unspec_volatile:GPR
	  [(match_operand:GPR 1 "memory_operand" "+A")
	   (match_operand:SI 3 "const_int_operand")] ;; model
	  UNSPEC_SYNC_EXCHANGE))
   (set (match_dup 1)
	(match_operand:GPR 2 "register_operand" "0"))]
  "TARGET_ZAAMO"
  "amoswap.<amo>%A3\t%0,%z2,%1"
  [(set_attr "type" "atomic")
   (set (attr "length") (const_int 4))])

(define_insn "lrsc_atomic_exchange<mode>"
  [(set (match_operand:GPR 0 "register_operand" "=&r")
	(unspec_volatile:GPR
	  [(match_operand:GPR 1 "memory_operand" "+A")
	   (match_operand:SI 3 "const_int_operand")] ;; model
	  UNSPEC_SYNC_EXCHANGE))
   (set (match_dup 1)
	(match_operand:GPR 2 "register_operand" "0"))
   (clobber (match_scratch:GPR 4 "=&r"))]	  ;; tmp_1
  "!TARGET_ZAAMO && TARGET_ZALRSC"
  {
    return "1:\;"
	   "lr.<amo>%I3\t%4, %1\;"
	   "sc.<amo>%J3\t%0, %0, %1\;"
	   "bnez\t%0, 1b\;"
	   "mv\t%0, %4";
  }
  [(set_attr "type" "atomic")
   (set (attr "length") (const_int 16))])

(define_expand "atomic_exchange<mode>"
  [(match_operand:SHORT 0 "register_operand") ;; old value at mem
   (match_operand:SHORT 1 "memory_operand")   ;; mem location
   (match_operand:SHORT 2 "register_operand") ;; value
   (match_operand:SI 3 "const_int_operand")]  ;; model
  "(TARGET_ZALRSC && TARGET_INLINE_SUBWORD_ATOMIC) || TARGET_ZABHA"
{
 if (TARGET_ZABHA)
    emit_insn(gen_zabha_atomic_exchange<mode>(operands[0], operands[1],
					      operands[2], operands[3]));
 else
    emit_insn(gen_lrsc_atomic_exchange<mode>(operands[0], operands[1],
					     operands[2], operands[3]));
  DONE;
})

(define_insn "zabha_atomic_exchange<mode>"
  [(set (match_operand:SHORT 0 "register_operand" "=&r")
	(unspec_volatile:SHORT
	  [(match_operand:SHORT 1 "memory_operand" "+A")
	   (match_operand:SI 3 "const_int_operand")] ;; model
	  UNSPEC_SYNC_EXCHANGE_ZABHA))
   (set (match_dup 1)
	(match_operand:SHORT 2 "register_operand" "0"))]
  "TARGET_ZABHA"
  "amoswap.<amobh>%A3\t%0,%z2,%1"
  [(set_attr "type" "atomic")
   (set (attr "length") (const_int 4))])

(define_expand "lrsc_atomic_exchange<mode>"
  [(match_operand:SHORT 0 "register_operand") ;; old value at mem
   (match_operand:SHORT 1 "memory_operand")   ;; mem location
   (match_operand:SHORT 2 "register_operand") ;; value
   (match_operand:SI 3 "const_int_operand")]  ;; model
  "!TARGET_ZABHA && TARGET_ZALRSC && TARGET_INLINE_SUBWORD_ATOMIC"
{
  rtx old = gen_reg_rtx (SImode);
  rtx mem = operands[1];
  rtx value = operands[2];
  rtx model = operands[3];
  rtx aligned_mem = gen_reg_rtx (SImode);
  rtx shift = gen_reg_rtx (SImode);
  rtx mask = gen_reg_rtx (SImode);
  rtx not_mask = gen_reg_rtx (SImode);

  riscv_subword_address (mem, &aligned_mem, &shift, &mask, &not_mask);

  rtx shifted_value = gen_reg_rtx (SImode);
  riscv_lshift_subword (<MODE>mode, value, shift, &shifted_value);
  emit_move_insn (shifted_value, gen_rtx_AND (SImode, shifted_value, mask));

  emit_insn (gen_subword_atomic_exchange_strong (old, aligned_mem,
						 shifted_value, model,
						 not_mask));

  emit_move_insn (old, gen_rtx_ASHIFTRT (SImode, old,
					 gen_lowpart (QImode, shift)));

  emit_move_insn (operands[0], gen_lowpart (<MODE>mode, old));
  DONE;
})

(define_insn "subword_atomic_exchange_strong"
  [(set (match_operand:SI 0 "register_operand" "=&r")	 ;; old value at mem
	(match_operand:SI 1 "memory_operand" "+A"))	 ;; mem location
   (set (match_dup 1)
	(unspec_volatile:SI
	  [(match_operand:SI 2 "reg_or_0_operand" "rI")	 ;; value
	   (match_operand:SI 3 "const_int_operand")]	 ;; model
      UNSPEC_SYNC_EXCHANGE_SUBWORD))
    (match_operand:SI 4 "reg_or_0_operand" "rI")	 ;; not_mask
    (clobber (match_scratch:SI 5 "=&r"))]		 ;; tmp_1
  "TARGET_ZALRSC && TARGET_INLINE_SUBWORD_ATOMIC"
  {
    return "1:\;"
	   "lr.w%I3\t%0, %1\;"
	   "and\t%5, %0, %4\;"
	   "or\t%5, %5, %2\;"
	   "sc.w%J3\t%5, %5, %1\;"
	   "bnez\t%5, 1b";
  }
  [(set_attr "type" "multi")
   (set (attr "length") (const_int 20))])

; Atomic CAS ops

(define_insn "zalrsc_atomic_cas_value_strong<mode>"
  [(set (match_operand:GPR 0 "register_operand" "=&r")
	(match_operand:GPR 1 "memory_operand" "+A"))
   (set (match_dup 1)
	(unspec_volatile:GPR [(match_operand:GPR 2 "reg_or_0_operand" "rJ")
			      (match_operand:GPR 3 "reg_or_0_operand" "rJ")
			      (match_operand:SI 4 "const_int_operand")  ;; mod_s
			      (match_operand:SI 5 "const_int_operand")] ;; mod_f
	 UNSPEC_COMPARE_AND_SWAP))
   (clobber (match_scratch:GPR 6 "=&r"))]
  "TARGET_ZALRSC"
  {
    enum memmodel model_success = (enum memmodel) INTVAL (operands[4]);
    enum memmodel model_failure = (enum memmodel) INTVAL (operands[5]);
    /* Find the union of the two memory models so we can satisfy both success
       and failure memory models.  */
    operands[5] = GEN_INT (riscv_union_memmodels (model_success, model_failure));
    return "1:\;"
	   "lr.<amo>%I5\t%0,%1\;"
	   "bne\t%0,%z2,1f\;"
	   "sc.<amo>%J5\t%6,%z3,%1\;"
	   "bnez\t%6,1b\;"
	   "1:";
  }
  [(set_attr "type" "multi")
   (set (attr "length") (const_int 16))])

;; Implement compare_exchange with a conservative leading fence when
;; model_failure is seq_cst.
;; This allows us to be compatible with the ISA manual Table A.6 and Table A.7
;; (A6C and A7).
;; More details: https://github.com/riscv-non-isa/riscv-elf-psabi-doc/issues/444
(define_insn "zacas_atomic_cas_value_strong<mode>"
  [(set (match_operand:GPR 0 "register_operand" "=&r")			    ;; val output
	(match_operand:GPR 1 "memory_operand" "+A"))			    ;; memory
   (set (match_dup 1)
	(unspec_volatile:GPR [(match_operand:GPR 2 "register_operand" "0")  ;; expected val
			      (match_operand:GPR 3 "reg_or_0_operand" "rJ") ;; desired val
			      (match_operand:SI 4 "const_int_operand")	    ;; mod_s
			      (match_operand:SI 5 "const_int_operand")]	    ;; mod_f
	 UNSPEC_COMPARE_AND_SWAP))]
  "TARGET_ZACAS"
  {
    enum memmodel model_success = (enum memmodel) INTVAL (operands[4]);
    enum memmodel model_failure = (enum memmodel) INTVAL (operands[5]);
    /* Find the union of the two memory models so we can satisfy both success
       and failure memory models.  */
    operands[4] = GEN_INT (riscv_union_memmodels (model_success, model_failure));

    if (model_failure == MEMMODEL_SEQ_CST)
      return "fence\trw,rw\;"
	     "amocas.<amo>%A4\t%0,%z3,%1";
    else
      return "amocas.<amo>%A4\t%0,%z3,%1";
  }
  [(set_attr "type" "atomic")
   (set (attr "length")
	(symbol_ref "(is_mm_seq_cst (memmodel_from_int (INTVAL (operands[5]))) ? 8
		      : 4)"))])

(define_expand "atomic_compare_and_swap<mode>"
  [(match_operand:SI 0 "register_operand" "")   ;; bool output
   (match_operand:GPR 1 "register_operand" "")  ;; val output
   (match_operand:GPR 2 "memory_operand" "")    ;; memory
   (match_operand:GPR 3 "register_operand" "")  ;; expected value
   (match_operand:GPR 4 "reg_or_0_operand" "")  ;; desired value
   (match_operand:SI 5 "const_int_operand" "")  ;; is_weak
   (match_operand:SI 6 "const_int_operand" "")  ;; mod_s
   (match_operand:SI 7 "const_int_operand" "")] ;; mod_f
  "TARGET_ZALRSC || TARGET_ZACAS"
{
  if (word_mode != <MODE>mode && operands[3] != const0_rtx)
    {
      /* We don't have SI mode compare on RV64, so we need to make sure expected
	 value is sign-extended.  */
      rtx tmp0 = gen_reg_rtx (word_mode);
      emit_insn (gen_extend_insn (tmp0, operands[3], word_mode, <MODE>mode, 0));
      operands[3] = gen_lowpart (<MODE>mode, tmp0);
    }

  if (TARGET_ZACAS)
    emit_insn (gen_zacas_atomic_cas_value_strong<mode> (operands[1],
							operands[2],
							operands[3],
							operands[4],
							operands[6],
							operands[7]));
  else
    emit_insn (gen_zalrsc_atomic_cas_value_strong<mode> (operands[1],
							 operands[2],
							 operands[3],
							 operands[4],
							 operands[6],
							 operands[7]));

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

;; Implement compare_exchange with a conservative leading fence when
;; model_failure is seq_cst.
;; This allows us to be compatible with the ISA manual Table A.6 and Table A.7
;; (A6C and A7).
;; More details: https://github.com/riscv-non-isa/riscv-elf-psabi-doc/issues/444
(define_insn "zacas_atomic_cas_value_strong<mode>"
  [(set (match_operand:SHORT 0 "register_operand" "=&r")			;; val output
	(match_operand:SHORT 1 "memory_operand" "+A"))				;; memory
   (set (match_dup 1)
	(unspec_volatile:SHORT [(match_operand:SHORT 2 "register_operand" "0")  ;; expected_val
				(match_operand:SHORT 3 "register_operand" "rJ") ;; desired_val
				(match_operand:SI 4 "const_int_operand")	;; mod_s
				(match_operand:SI 5 "const_int_operand")]	;; mod_f
	 UNSPEC_COMPARE_AND_SWAP))]
  "TARGET_ZACAS && TARGET_ZABHA"
  {
    enum memmodel model_success = (enum memmodel) INTVAL (operands[4]);
    enum memmodel model_failure = (enum memmodel) INTVAL (operands[5]);
    /* Find the union of the two memory models so we can satisfy both success
       and failure memory models.  */
    operands[4] = GEN_INT (riscv_union_memmodels (model_success, model_failure));

    if (model_failure == MEMMODEL_SEQ_CST)
      return "fence\trw,rw\;"
	     "amocas.<amobh>%A4\t%0,%z3,%1";
    else
      return "amocas.<amobh>%A4\t%0,%z3,%1";
  }
  [(set_attr "type" "atomic")
   (set (attr "length")
	(symbol_ref "(is_mm_seq_cst (memmodel_from_int (INTVAL (operands[5]))) ? 8
		      : 4)"))])

(define_expand "atomic_compare_and_swap<mode>"
  [(match_operand:SI 0 "register_operand")    ;; bool output
   (match_operand:SHORT 1 "register_operand") ;; val output
   (match_operand:SHORT 2 "memory_operand")   ;; memory
   (match_operand:SHORT 3 "register_operand") ;; expected value
   (match_operand:SHORT 4 "reg_or_0_operand") ;; desired value
   (match_operand:SI 5 "const_int_operand")   ;; is_weak
   (match_operand:SI 6 "const_int_operand")   ;; mod_s
   (match_operand:SI 7 "const_int_operand")]  ;; mod_f
  "(TARGET_ZALRSC && TARGET_INLINE_SUBWORD_ATOMIC) || (TARGET_ZACAS && TARGET_ZABHA)"
{

  if (TARGET_ZACAS && TARGET_ZABHA)
    emit_insn (gen_zacas_atomic_cas_value_strong<mode> (operands[1],
							operands[2],
							operands[3],
							operands[4],
							operands[6],
							operands[7]));
  else
    emit_insn (gen_zalrsc_atomic_cas_value_strong<mode> (operands[1],
							 operands[2],
							 operands[3],
							 operands[4],
							 operands[6],
							 operands[7]));

  rtx val = gen_reg_rtx (SImode);
  if (operands[1] != const0_rtx)
    emit_move_insn (val, gen_rtx_SIGN_EXTEND (SImode, operands[1]));
  else
    emit_move_insn (val, const0_rtx);

  rtx exp = gen_reg_rtx (SImode);
  if (operands[3] != const0_rtx)
    emit_move_insn (exp, gen_rtx_SIGN_EXTEND (SImode, operands[3]));
  else
    emit_move_insn (exp, const0_rtx);

  rtx compare = val;
  if (exp != const0_rtx)
    {
      rtx difference = gen_rtx_MINUS (SImode, val, exp);
      compare = gen_reg_rtx (SImode);
      emit_move_insn (compare, difference);
    }

  if (word_mode != SImode)
    {
      rtx reg = gen_reg_rtx (word_mode);
      emit_move_insn (reg, gen_rtx_SIGN_EXTEND (word_mode, compare));
      compare = reg;
    }

  emit_move_insn (operands[0], gen_rtx_EQ (SImode, compare, const0_rtx));
  DONE;
})

(define_expand "zalrsc_atomic_cas_value_strong<mode>"
  [(match_operand:SHORT 0 "register_operand") ;; val output
   (match_operand:SHORT 1 "memory_operand")   ;; memory
   (match_operand:SHORT 2 "reg_or_0_operand") ;; expected value
   (match_operand:SHORT 3 "reg_or_0_operand") ;; desired value
   (match_operand:SI 4 "const_int_operand")   ;; mod_s
   (match_operand:SI 5 "const_int_operand")   ;; mod_f
   (match_scratch:SHORT 6)]
  "TARGET_ZALRSC && TARGET_INLINE_SUBWORD_ATOMIC"
{
  /* We have no QImode/HImode atomics, so form a mask, then use
     subword_atomic_cas_strong<mode> to implement a LR/SC version of the
     operation.  */

  /* Logic duplicated in gcc/libgcc/config/riscv/atomic.c for use when inlining
     is disabled.  */

  rtx old = gen_reg_rtx (SImode);
  rtx mem = operands[1];
  rtx aligned_mem = gen_reg_rtx (SImode);
  rtx shift = gen_reg_rtx (SImode);
  rtx mask = gen_reg_rtx (SImode);
  rtx not_mask = gen_reg_rtx (SImode);

  riscv_subword_address (mem, &aligned_mem, &shift, &mask, &not_mask);

  rtx o = operands[2];
  rtx n = operands[3];
  rtx shifted_o = gen_reg_rtx (SImode);
  rtx shifted_n = gen_reg_rtx (SImode);

  riscv_lshift_subword (<MODE>mode, o, shift, &shifted_o);
  riscv_lshift_subword (<MODE>mode, n, shift, &shifted_n);

  emit_move_insn (shifted_o, gen_rtx_AND (SImode, shifted_o, mask));
  emit_move_insn (shifted_n, gen_rtx_AND (SImode, shifted_n, mask));

  enum memmodel model_success = (enum memmodel) INTVAL (operands[4]);
  enum memmodel model_failure = (enum memmodel) INTVAL (operands[5]);
  /* Find the union of the two memory models so we can satisfy both success
     and failure memory models.  */
  rtx model = GEN_INT (riscv_union_memmodels (model_success, model_failure));

  emit_insn (gen_subword_atomic_cas_strong (old, aligned_mem,
					    shifted_o, shifted_n,
					    model, mask, not_mask));

  emit_move_insn (old, gen_rtx_ASHIFTRT (SImode, old,
					 gen_lowpart (QImode, shift)));

  emit_move_insn (operands[0], gen_lowpart (<MODE>mode, old));

  DONE;
})

(define_insn "subword_atomic_cas_strong"
  [(set (match_operand:SI 0 "register_operand" "=&r")			   ;; old value at mem
	(match_operand:SI 1 "memory_operand" "+A"))			   ;; mem location
   (set (match_dup 1)
	(unspec_volatile:SI [(match_operand:SI 2 "reg_or_0_operand" "rJ")  ;; expected value
			     (match_operand:SI 3 "reg_or_0_operand" "rJ")] ;; desired value
	 UNSPEC_COMPARE_AND_SWAP_SUBWORD))
	(match_operand:SI 4 "const_int_operand")			   ;; model
	(match_operand:SI 5 "register_operand" "rI")			   ;; mask
	(match_operand:SI 6 "register_operand" "rI")			   ;; not_mask
	(clobber (match_scratch:SI 7 "=&r"))]				   ;; tmp_1
  "TARGET_ZALRSC && TARGET_INLINE_SUBWORD_ATOMIC"
  {
    return "1:\;"
	   "lr.w%I4\t%0, %1\;"
	   "and\t%7, %0, %5\;"
	   "bne\t%7, %z2, 1f\;"
	   "and\t%7, %0, %6\;"
	   "or\t%7, %7, %3\;"
	   "sc.w%J4\t%7, %7, %1\;"
	   "bnez\t%7, 1b\;"
	   "1:";
  }
  [(set_attr "type" "multi")
   (set (attr "length") (const_int 28))])

(define_expand "atomic_test_and_set"
  [(match_operand:QI 0 "register_operand" "")    ;; bool output
   (match_operand:QI 1 "memory_operand" "+A")    ;; memory
   (match_operand:SI 2 "const_int_operand" "")]  ;; model
  "TARGET_ZAAMO || TARGET_ZALRSC"
{
  /* We have no QImode atomics, so use the address LSBs to form a mask,
     then use an aligned SImode atomic.  */
  rtx old = gen_reg_rtx (SImode);
  rtx mem = operands[1];
  rtx model = operands[2];
  rtx set = gen_reg_rtx (QImode);
  rtx aligned_mem = gen_reg_rtx (SImode);
  rtx shift = gen_reg_rtx (SImode);

  /* Unused.  */
  rtx _mask = gen_reg_rtx (SImode);
  rtx _not_mask = gen_reg_rtx (SImode);

  riscv_subword_address (mem, &aligned_mem, &shift, &_mask, &_not_mask);

  emit_move_insn (set, GEN_INT (1));
  rtx shifted_set = gen_reg_rtx (SImode);
  riscv_lshift_subword (QImode, set, shift, &shifted_set);

  if (TARGET_ZAAMO)
    emit_insn (gen_amo_atomic_fetch_orsi (old, aligned_mem, shifted_set, model));
  else if (TARGET_ZALRSC)
    emit_insn (gen_lrsc_atomic_fetch_orsi (old, aligned_mem, shifted_set, model));

  emit_move_insn (old, gen_rtx_ASHIFTRT (SImode, old,
					 gen_lowpart (QImode, shift)));

  emit_move_insn (operands[0], gen_lowpart (QImode, old));

  DONE;
})
