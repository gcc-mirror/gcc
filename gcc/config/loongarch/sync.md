;; Machine description for LoongArch atomic operations.
;; Copyright (C) 2021-2024 Free Software Foundation, Inc.
;; Contributed by Loongson Ltd.
;; Based on MIPS and RISC-V target for GNU compiler.

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
  UNSPEC_COMPARE_AND_SWAP_ADD
  UNSPEC_COMPARE_AND_SWAP_SUB
  UNSPEC_COMPARE_AND_SWAP_AND
  UNSPEC_COMPARE_AND_SWAP_XOR
  UNSPEC_COMPARE_AND_SWAP_OR
  UNSPEC_COMPARE_AND_SWAP_NAND
  UNSPEC_SYNC_OLD_OP
  UNSPEC_SYNC_EXCHANGE
  UNSPEC_ATOMIC_STORE
  UNSPEC_ATOMIC_LOAD
  UNSPEC_MEMORY_BARRIER
])

(define_code_iterator any_atomic [plus ior xor and])
(define_code_attr atomic_optab
  [(plus "add") (ior "or") (xor "xor") (and "and")])

;; This attribute gives the format suffix for atomic memory operations.
(define_mode_attr amo [(QI "b") (HI "h") (SI "w") (DI "d")])

;; <amop> expands to the name of the atomic operand that implements a
;; particular code.
(define_code_attr amop [(ior "or") (xor "xor") (and "and") (plus "add")])

;; Memory barriers.

(define_expand "mem_thread_fence"
  [(match_operand:SI 0 "const_int_operand" "")] ;; model
  ""
{
  rtx mem = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (Pmode));
  MEM_VOLATILE_P (mem) = 1;
  emit_insn (gen_mem_thread_fence_1 (mem, operands[0]));

  DONE;
})

;; DBAR hint encoding for LA664 and later micro-architectures, paraphrased from
;; the Linux patch revealing it [1]:
;;
;; - Bit 4: kind of constraint (0: completion, 1: ordering)
;; - Bit 3: barrier for previous read (0: true, 1: false)
;; - Bit 2: barrier for previous write (0: true, 1: false)
;; - Bit 1: barrier for succeeding read (0: true, 1: false)
;; - Bit 0: barrier for succeeding write (0: true, 1: false)
;;
;; [1]: https://git.kernel.org/torvalds/c/e031a5f3f1ed
;;
;; Implementations without support for the finer-granularity hints simply treat
;; all as the full barrier (DBAR 0), so we can unconditionally start emiting the
;; more precise hints right away.
(define_insn "mem_thread_fence_1"
  [(set (match_operand:BLK 0 "" "")
	(unspec:BLK [(match_dup 0)] UNSPEC_MEMORY_BARRIER))
   (match_operand:SI 1 "const_int_operand" "")] ;; model
  ""
  {
    enum memmodel model = memmodel_base (INTVAL (operands[1]));

    switch (model)
      {
      case MEMMODEL_ACQUIRE:
	return "dbar\t0b10100";
      case MEMMODEL_RELEASE:
	return "dbar\t0b10010";
      case MEMMODEL_ACQ_REL:
      case MEMMODEL_SEQ_CST:
	return "dbar\t0b10000";
      default:
	/* GCC internal: "For the '__ATOMIC_RELAXED' model no instructions
	   need to be issued and this expansion is not invoked."

	   __atomic builtins doc: "Consume is implemented using the
	   stronger acquire memory order because of a deficiency in C++11's
	   semantics."  See PR 59448 and get_memmodel in builtins.cc.

	   Other values should not be returned by memmodel_base.  */
	gcc_unreachable ();
      }
  })

;; Atomic memory operations.

(define_insn "atomic_load<mode>"
  [(set (match_operand:QHWD 0 "register_operand" "=r")
    (unspec_volatile:QHWD
      [(match_operand:QHWD 1 "memory_operand" "+m")
       (match_operand:SI 2 "const_int_operand")]                        ;; model
      UNSPEC_ATOMIC_LOAD))]
  ""
{
  enum memmodel model = memmodel_base (INTVAL (operands[2]));

  switch (model)
    {
    case MEMMODEL_SEQ_CST:
      return "dbar\t0x11\\n\\t"
	     "ld.<size>\t%0,%1\\n\\t"
	     "dbar\t0x14";
    case MEMMODEL_ACQUIRE:
      return "ld.<size>\t%0,%1\\n\\t"
	     "dbar\t0x14";
    case MEMMODEL_RELAXED:
      return ISA_HAS_LD_SEQ_SA ? "ld.<size>\t%0,%1"
			       : "ld.<size>\t%0,%1\\n\\t"
				 "dbar\t0x700";

    default:
      /* The valid memory order variants are __ATOMIC_RELAXED, __ATOMIC_SEQ_CST,
	 __ATOMIC_CONSUME and __ATOMIC_ACQUIRE.
	 The expand_builtin_atomic_store function converts all invalid memmodels
	 to MEMMODEL_SEQ_CST.

	 __atomic builtins doc: "Consume is implemented using the
	 stronger acquire memory order because of a deficiency in C++11's
	 semantics."  See PR 59448 and get_memmodel in builtins.cc.  */
      gcc_unreachable ();
    }
}
  [(set (attr "length") (const_int 12))])

;; Implement atomic stores with amoswap.  Fall back to fences for atomic loads.
(define_insn "atomic_store<mode>"
  [(set (match_operand:QHWD 0 "memory_operand" "+m")
    (unspec_volatile:QHWD
      [(match_operand:QHWD 1 "reg_or_0_operand" "rJ")
       (match_operand:SI 2 "const_int_operand")]      ;; model
      UNSPEC_ATOMIC_STORE))]
  ""
{
  enum memmodel model = memmodel_base (INTVAL (operands[2]));

  switch (model)
    {
    case MEMMODEL_SEQ_CST:
      return "dbar\t0x12\\n\\t"
	     "st.<size>\t%z1,%0\\n\\t"
	     "dbar\t0x18\\n\\t";
    case MEMMODEL_RELEASE:
      return "dbar\t0x12\\n\\t"
	     "st.<size>\t%z1,%0\\n\\t";
    case MEMMODEL_RELAXED:
      return "st.<size>\t%z1,%0";

    default:
      /* The valid memory order variants are __ATOMIC_RELAXED, __ATOMIC_SEQ_CST,
	 and __ATOMIC_RELEASE.
	 The expand_builtin_atomic_store function converts all invalid memmodels
	 to MEMMODEL_SEQ_CST.  */
      gcc_unreachable ();
    }
}
  [(set (attr "length") (const_int 12))])

(define_insn "atomic_<atomic_optab><mode>"
  [(set (match_operand:GPR 0 "memory_operand" "+ZB")
	(unspec_volatile:GPR
	  [(any_atomic:GPR (match_dup 0)
			   (match_operand:GPR 1 "reg_or_0_operand" "rJ"))
	   (match_operand:SI 2 "const_int_operand")] ;; model
	 UNSPEC_SYNC_OLD_OP))]
  ""
  "am<amop>%A2.<amo>\t$zero,%z1,%0"
  [(set (attr "length") (const_int 4))])

(define_insn "atomic_add<mode>"
  [(set (match_operand:SHORT 0 "memory_operand" "+ZB")
	(unspec_volatile:SHORT
	  [(plus:SHORT (match_dup 0)
		       (match_operand:SHORT 1 "reg_or_0_operand" "rJ"))
	   (match_operand:SI 2 "const_int_operand")] ;; model
	 UNSPEC_SYNC_OLD_OP))]
  "ISA_HAS_LAM_BH"
  "amadd%A2.<amo>\t$zero,%z1,%0"
  [(set (attr "length") (const_int 4))])

(define_insn "atomic_fetch_<atomic_optab><mode>"
  [(set (match_operand:GPR 0 "register_operand" "=&r")
	(match_operand:GPR 1 "memory_operand" "+ZB"))
   (set (match_dup 1)
	(unspec_volatile:GPR
	  [(any_atomic:GPR (match_dup 1)
			   (match_operand:GPR 2 "reg_or_0_operand" "rJ"))
	   (match_operand:SI 3 "const_int_operand")] ;; model
	 UNSPEC_SYNC_OLD_OP))]
  ""
  "am<amop>%A3.<amo>\t%0,%z2,%1"
  [(set (attr "length") (const_int 4))])

(define_insn "atomic_exchange<mode>"
  [(set (match_operand:GPR 0 "register_operand" "=&r")
	(unspec_volatile:GPR
	  [(match_operand:GPR 1 "memory_operand" "+ZB")
	   (match_operand:SI 3 "const_int_operand")] ;; model
	  UNSPEC_SYNC_EXCHANGE))
   (set (match_dup 1)
	(match_operand:GPR 2 "register_operand" "r"))]
  ""
  "amswap%A3.<amo>\t%0,%z2,%1"
  [(set (attr "length") (const_int 4))])

(define_insn "atomic_exchange<mode>_short"
  [(set (match_operand:SHORT 0 "register_operand" "=&r")
	(unspec_volatile:SHORT
	  [(match_operand:SHORT 1 "memory_operand" "+ZB")
	   (match_operand:SI 3 "const_int_operand")] ;; model
	  UNSPEC_SYNC_EXCHANGE))
   (set (match_dup 1)
	(match_operand:SHORT 2 "register_operand" "r"))]
  "ISA_HAS_LAM_BH"
  "amswap%A3.<amo>\t%0,%z2,%1"
  [(set (attr "length") (const_int 4))])

(define_insn "atomic_cas_value_strong<mode>"
  [(set (match_operand:GPR 0 "register_operand" "=&r")
	(match_operand:GPR 1 "memory_operand" "+ZC"))
   (set (match_dup 1)
	(unspec_volatile:GPR [(match_operand:GPR 2 "reg_or_0_operand" "rJ")
			      (match_operand:GPR 3 "reg_or_0_operand" "rJ")
			      (match_operand:SI 4 "const_int_operand")]  ;; mod_s
	 UNSPEC_COMPARE_AND_SWAP))
   (clobber (match_scratch:GPR 5 "=&r"))]
  ""
{
  output_asm_insn ("1:", operands);
  output_asm_insn ("ll.<amo>\t%0,%1", operands);

  /* Like the test case atomic-cas-int.C, in loongarch64, O1 and higher, the
     return value of the val_without_const_folding will not be truncated and
     will be passed directly to the function compare_exchange_strong.
     However, the instruction 'bne' does not distinguish between 32-bit and
     64-bit operations.  so if the upper 32 bits of the register are not
     extended by the 32nd bit symbol, then the comparison may not be valid
     here.  This will affect the result of the operation.  */

  if (TARGET_64BIT && REG_P (operands[2])
      && GET_MODE (operands[2]) == SImode)
    {
      output_asm_insn ("addi.w\t%5,%2,0", operands);
      output_asm_insn ("bne\t%0,%5,2f", operands);
    }
  else
    output_asm_insn ("bne\t%0,%z2,2f", operands);

  output_asm_insn ("or%i3\t%5,$zero,%3", operands);
  output_asm_insn ("sc.<amo>\t%5,%1", operands);
  output_asm_insn ("beqz\t%5,1b", operands);
  output_asm_insn ("b\t3f", operands);
  output_asm_insn ("2:", operands);
  output_asm_insn ("%G4", operands);
  output_asm_insn ("3:", operands);

  return "";
}
  [(set (attr "length")
     (if_then_else
	(and (match_test "GET_MODE (operands[2]) == SImode")
	     (match_test "REG_P (operands[2])"))
	(const_int 32)
	(const_int 28)))])

(define_insn "atomic_cas_value_strong<mode>_amcas"
  [(set (match_operand:QHWD 0 "register_operand" "=&r")
	(match_operand:QHWD 1 "memory_operand" "+ZB"))
   (set (match_dup 1)
	(unspec_volatile:QHWD [(match_operand:QHWD 2 "reg_or_0_operand" "rJ")
			       (match_operand:QHWD 3 "reg_or_0_operand" "rJ")
			       (match_operand:SI 4 "const_int_operand")]  ;; mod_s
	 UNSPEC_COMPARE_AND_SWAP))]
  "ISA_HAS_LAMCAS"
  "ori\t%0,%z2,0\n\tamcas%A4.<amo>\t%0,%z3,%1"
  [(set (attr "length") (const_int 8))])

(define_expand "atomic_compare_and_swap<mode>"
  [(match_operand:SI 0 "register_operand" "")   ;; bool output
   (match_operand:GPR 1 "register_operand" "")  ;; val output
   (match_operand:GPR 2 "memory_operand" "")    ;; memory
   (match_operand:GPR 3 "reg_or_0_operand" "")  ;; expected value
   (match_operand:GPR 4 "reg_or_0_operand" "")  ;; desired value
   (match_operand:SI 5 "const_int_operand" "")  ;; is_weak
   (match_operand:SI 6 "const_int_operand" "")  ;; mod_s
   (match_operand:SI 7 "const_int_operand" "")] ;; mod_f
  ""
{
  rtx mod_s, mod_f;

  mod_s = operands[6];
  mod_f = operands[7];

  /* Normally the succ memory model must be stronger than fail, but in the
     unlikely event of fail being ACQUIRE and succ being RELEASE we need to
     promote succ to ACQ_REL so that we don't lose the acquire semantics.  */

  if (is_mm_acquire (memmodel_base (INTVAL (mod_f)))
      && is_mm_release (memmodel_base (INTVAL (mod_s))))
    mod_s = GEN_INT (MEMMODEL_ACQ_REL);

  operands[6] = mod_s;

  if (ISA_HAS_LAMCAS)
    emit_insn (gen_atomic_cas_value_strong<mode>_amcas (operands[1], operands[2],
							 operands[3], operands[4],
							 operands[6]));
  else
    emit_insn (gen_atomic_cas_value_strong<mode> (operands[1], operands[2],
						  operands[3], operands[4],
						  operands[6]));

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

  emit_insn (gen_rtx_SET (operands[0],
			  gen_rtx_EQ (SImode, compare, const0_rtx)));
  DONE;
})

(define_expand "atomic_test_and_set"
  [(match_operand:QI 0 "register_operand" "")     ;; bool output
   (match_operand:QI 1 "memory_operand" "+ZB")    ;; memory
   (match_operand:SI 2 "const_int_operand" "")]   ;; model
  ""
{
  /* We have no QImode atomics, so use the address LSBs to form a mask,
     then use an aligned SImode atomic.  */
  rtx result = operands[0];
  rtx mem = operands[1];
  rtx model = operands[2];
  rtx addr = force_reg (Pmode, XEXP (mem, 0));
  rtx tmp_reg = gen_reg_rtx (Pmode);
  rtx zero_reg = gen_rtx_REG (Pmode, 0);

  rtx aligned_addr = gen_reg_rtx (Pmode);
  emit_move_insn (tmp_reg, gen_rtx_PLUS (Pmode, zero_reg, GEN_INT (-4)));
  emit_move_insn (aligned_addr, gen_rtx_AND (Pmode, addr, tmp_reg));

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
  emit_move_insn (word, gen_rtx_ASHIFT (SImode, tmp, shmt));

  tmp = gen_reg_rtx (SImode);
  emit_insn (gen_atomic_fetch_orsi (tmp, aligned_mem, word, model));

  emit_move_insn (gen_lowpart (SImode, result),
		  gen_rtx_LSHIFTRT (SImode, tmp, shmt));
  DONE;
})

(define_insn "atomic_cas_value_cmp_and_7_<mode>"
  [(set (match_operand:GPR 0 "register_operand" "=&r")
	(match_operand:GPR 1 "memory_operand" "+ZC"))
   (set (match_dup 1)
	(unspec_volatile:GPR [(match_operand:GPR 2 "reg_or_0_operand" "rJ")
			      (match_operand:GPR 3 "reg_or_0_operand" "rJ")
			      (match_operand:GPR 4 "reg_or_0_operand"  "rJ")
			      (match_operand:GPR 5 "reg_or_0_operand"  "rJ")
			      (match_operand:SI 6 "const_int_operand")] ;; model
	 UNSPEC_COMPARE_AND_SWAP))
   (clobber (match_scratch:GPR 7 "=&r"))]
  ""
{
  return "1:\\n\\t"
	 "ll.<amo>\\t%0,%1\\n\\t"
	 "and\\t%7,%0,%2\\n\\t"
	 "bne\\t%7,%z4,2f\\n\\t"
	 "and\\t%7,%0,%z3\\n\\t"
	 "or%i5\\t%7,%7,%5\\n\\t"
	 "sc.<amo>\\t%7,%1\\n\\t"
	 "beq\\t$zero,%7,1b\\n\\t"
	 "b\\t3f\\n\\t"
	 "2:\\n\\t"
	 "%G6\\n\\t"
	 "3:\\n\\t";
}
  [(set (attr "length") (const_int 36))])

(define_expand "atomic_compare_and_swap<mode>"
  [(match_operand:SI 0 "register_operand" "")   ;; bool output
   (match_operand:SHORT 1 "register_operand" "")  ;; val output
   (match_operand:SHORT 2 "memory_operand" "")    ;; memory
   (match_operand:SHORT 3 "reg_or_0_operand" "")  ;; expected value
   (match_operand:SHORT 4 "reg_or_0_operand" "")  ;; desired value
   (match_operand:SI 5 "const_int_operand" "")  ;; is_weak
   (match_operand:SI 6 "const_int_operand" "")  ;; mod_s
   (match_operand:SI 7 "const_int_operand" "")] ;; mod_f
  ""
{
  rtx mod_s, mod_f;

  mod_s = operands[6];
  mod_f = operands[7];

  /* Normally the succ memory model must be stronger than fail, but in the
     unlikely event of fail being ACQUIRE and succ being RELEASE we need to
     promote succ to ACQ_REL so that we don't lose the acquire semantics.  */

  if (is_mm_acquire (memmodel_base (INTVAL (mod_f)))
      && is_mm_release (memmodel_base (INTVAL (mod_s))))
    mod_s = GEN_INT (MEMMODEL_ACQ_REL);

  operands[6] = mod_s;

  if (ISA_HAS_LAMCAS)
    emit_insn (gen_atomic_cas_value_strong<mode>_amcas (operands[1], operands[2],
						       operands[3], operands[4],
						       operands[6]));
  else
    {
      union loongarch_gen_fn_ptrs generator;
      generator.fn_7 = gen_atomic_cas_value_cmp_and_7_si;
      loongarch_expand_atomic_qihi (generator, operands[1], operands[2],
				    operands[3], operands[4], operands[6]);
    }

      rtx compare = operands[1];
      if (operands[3] != const0_rtx)
	{
	  machine_mode mode = GET_MODE (operands[3]);
	  rtx op1 = convert_modes (SImode, mode, operands[1], true);
	  rtx op3 = convert_modes (SImode, mode, operands[3], true);
	  rtx difference = gen_rtx_MINUS (SImode, op1, op3);
	  compare = gen_reg_rtx (SImode);
	  emit_insn (gen_rtx_SET (compare, difference));
	}

      if (word_mode != <MODE>mode)
	{
	  rtx reg = gen_reg_rtx (word_mode);
	  emit_insn (gen_rtx_SET (reg, gen_rtx_SIGN_EXTEND (word_mode, compare)));
	  compare = reg;
	}

      emit_insn (gen_rtx_SET (operands[0],
			      gen_rtx_EQ (SImode, compare, const0_rtx)));
  DONE;
})

(define_insn "atomic_cas_value_add_7_<mode>"
  [(set (match_operand:GPR 0 "register_operand" "=&r")				;; res
	(match_operand:GPR 1 "memory_operand" "+ZC"))
   (set (match_dup 1)
	(unspec_volatile:GPR [(match_operand:GPR 2 "reg_or_0_operand" "rJ")	;; mask
			      (match_operand:GPR 3 "reg_or_0_operand" "rJ")	;; inverted_mask
			      (match_operand:GPR 4 "reg_or_0_operand"  "rJ")	;; old val
			      (match_operand:GPR 5 "reg_or_0_operand"  "rJ")	;; new val
			      (match_operand:SI 6 "const_int_operand")]		;; model
	 UNSPEC_COMPARE_AND_SWAP_ADD))
   (clobber (match_scratch:GPR 7 "=&r"))
   (clobber (match_scratch:GPR 8 "=&r"))]
  ""
{
  return "1:\\n\\t"
	 "ll.<amo>\\t%0,%1\\n\\t"
	 "and\\t%7,%0,%3\\n\\t"
	 "add.w\\t%8,%0,%z5\\n\\t"
	 "and\\t%8,%8,%z2\\n\\t"
	 "or%i8\\t%7,%7,%8\\n\\t"
	 "sc.<amo>\\t%7,%1\\n\\t"
	 "beq\\t$zero,%7,1b";
}

  [(set (attr "length") (const_int 28))])

(define_insn "atomic_cas_value_sub_7_<mode>"
  [(set (match_operand:GPR 0 "register_operand" "=&r")				;; res
	(match_operand:GPR 1 "memory_operand" "+ZC"))
   (set (match_dup 1)
	(unspec_volatile:GPR [(match_operand:GPR 2 "reg_or_0_operand" "rJ")	;; mask
			      (match_operand:GPR 3 "reg_or_0_operand" "rJ")	;; inverted_mask
			      (match_operand:GPR 4 "reg_or_0_operand"  "rJ")	;; old val
			      (match_operand:GPR 5 "reg_or_0_operand"  "rJ")	;; new val
			      (match_operand:SI 6 "const_int_operand")]		;; model
	 UNSPEC_COMPARE_AND_SWAP_SUB))
   (clobber (match_scratch:GPR 7 "=&r"))
   (clobber (match_scratch:GPR 8 "=&r"))]
  ""
{
  return "1:\\n\\t"
	 "ll.<amo>\\t%0,%1\\n\\t"
	 "and\\t%7,%0,%3\\n\\t"
	 "sub.w\\t%8,%0,%z5\\n\\t"
	 "and\\t%8,%8,%z2\\n\\t"
	 "or%i8\\t%7,%7,%8\\n\\t"
	 "sc.<amo>\\t%7,%1\\n\\t"
	 "beq\\t$zero,%7,1b";
}
  [(set (attr "length") (const_int 28))])

(define_insn "atomic_cas_value_and_7_<mode>"
  [(set (match_operand:GPR 0 "register_operand" "=&r")				;; res
	(match_operand:GPR 1 "memory_operand" "+ZC"))
   (set (match_dup 1)
	(unspec_volatile:GPR [(match_operand:GPR 2 "reg_or_0_operand" "rJ")	;; mask
			      (match_operand:GPR 3 "reg_or_0_operand" "rJ")	;; inverted_mask
			      (match_operand:GPR 4 "reg_or_0_operand"  "rJ")	;; old val
			      (match_operand:GPR 5 "reg_or_0_operand"  "rJ")	;; new val
			      (match_operand:SI 6 "const_int_operand")]		;; model
	 UNSPEC_COMPARE_AND_SWAP_AND))
   (clobber (match_scratch:GPR 7 "=&r"))
   (clobber (match_scratch:GPR 8 "=&r"))]
  ""
{
  return "1:\\n\\t"
	 "ll.<amo>\\t%0,%1\\n\\t"
	 "and\\t%7,%0,%3\\n\\t"
	 "and\\t%8,%0,%z5\\n\\t"
	 "and\\t%8,%8,%z2\\n\\t"
	 "or%i8\\t%7,%7,%8\\n\\t"
	 "sc.<amo>\\t%7,%1\\n\\t"
	 "beq\\t$zero,%7,1b";
}
  [(set (attr "length") (const_int 28))])

(define_insn "atomic_cas_value_xor_7_<mode>"
  [(set (match_operand:GPR 0 "register_operand" "=&r")				;; res
	(match_operand:GPR 1 "memory_operand" "+ZC"))
   (set (match_dup 1)
	(unspec_volatile:GPR [(match_operand:GPR 2 "reg_or_0_operand" "rJ")	;; mask
			      (match_operand:GPR 3 "reg_or_0_operand" "rJ")	;; inverted_mask
			      (match_operand:GPR 4 "reg_or_0_operand"  "rJ")	;; old val
			      (match_operand:GPR 5 "reg_or_0_operand"  "rJ")	;; new val
			      (match_operand:SI 6 "const_int_operand")]		;; model
	 UNSPEC_COMPARE_AND_SWAP_XOR))
   (clobber (match_scratch:GPR 7 "=&r"))
   (clobber (match_scratch:GPR 8 "=&r"))]
  ""
{
  return "1:\\n\\t"
	 "ll.<amo>\\t%0,%1\\n\\t"
	 "and\\t%7,%0,%3\\n\\t"
	 "xor\\t%8,%0,%z5\\n\\t"
	 "and\\t%8,%8,%z2\\n\\t"
	 "or%i8\\t%7,%7,%8\\n\\t"
	 "sc.<amo>\\t%7,%1\\n\\t"
	 "beq\\t$zero,%7,1b";
}

  [(set (attr "length") (const_int 28))])

(define_insn "atomic_cas_value_or_7_<mode>"
  [(set (match_operand:GPR 0 "register_operand" "=&r")				;; res
	(match_operand:GPR 1 "memory_operand" "+ZC"))
   (set (match_dup 1)
	(unspec_volatile:GPR [(match_operand:GPR 2 "reg_or_0_operand" "rJ")	;; mask
			      (match_operand:GPR 3 "reg_or_0_operand" "rJ")	;; inverted_mask
			      (match_operand:GPR 4 "reg_or_0_operand"  "rJ")	;; old val
			      (match_operand:GPR 5 "reg_or_0_operand"  "rJ")	;; new val
			      (match_operand:SI 6 "const_int_operand")]		;; model
	 UNSPEC_COMPARE_AND_SWAP_OR))
   (clobber (match_scratch:GPR 7 "=&r"))
   (clobber (match_scratch:GPR 8 "=&r"))]
  ""
{
  return "1:\\n\\t"
	 "ll.<amo>\\t%0,%1\\n\\t"
	 "and\\t%7,%0,%3\\n\\t"
	 "or\\t%8,%0,%z5\\n\\t"
	 "and\\t%8,%8,%z2\\n\\t"
	 "or%i8\\t%7,%7,%8\\n\\t"
	 "sc.<amo>\\t%7,%1\\n\\t"
	 "beq\\t$zero,%7,1b";
}

  [(set (attr "length") (const_int 28))])

(define_insn "atomic_cas_value_nand_7_<mode>"
  [(set (match_operand:GPR 0 "register_operand" "=&r")				;; res
	(match_operand:GPR 1 "memory_operand" "+ZC"))
   (set (match_dup 1)
	(unspec_volatile:GPR [(match_operand:GPR 2 "reg_or_0_operand" "rJ")	;; mask
			      (match_operand:GPR 3 "reg_or_0_operand" "rJ")	;; inverted_mask
			      (match_operand:GPR 4 "reg_or_0_operand"  "rJ")	;; old val
			      (match_operand:GPR 5 "reg_or_0_operand"  "rJ")	;; new val
			      (match_operand:SI 6 "const_int_operand")]		;; model
	 UNSPEC_COMPARE_AND_SWAP_NAND))
   (clobber (match_scratch:GPR 7 "=&r"))
   (clobber (match_scratch:GPR 8 "=&r"))]
  ""
{
  return "1:\\n\\t"
	 "ll.<amo>\\t%0,%1\\n\\t"
	 "and\\t%7,%0,%3\\n\\t"
	 "and\\t%8,%0,%z5\\n\\t"
	 "xor\\t%8,%8,%z2\\n\\t"
	 "or%i8\\t%7,%7,%8\\n\\t"
	 "sc.<amo>\\t%7,%1\\n\\t"
	 "beq\\t$zero,%7,1b";
}
  [(set (attr "length") (const_int 28))])

(define_insn "atomic_cas_value_exchange_7_<mode>"
  [(set (match_operand:GPR 0 "register_operand" "=&r")
	(match_operand:GPR 1 "memory_operand" "+ZC"))
   (set (match_dup 1)
	(unspec_volatile:GPR [(match_operand:GPR 2 "reg_or_0_operand" "rJ")
			      (match_operand:GPR 3 "reg_or_0_operand" "rJ")
			      (match_operand:GPR 4 "reg_or_0_operand" "rJ")
			      (match_operand:GPR 5 "reg_or_0_operand"  "rJ")
			      (match_operand:SI 6 "const_int_operand")] ;; model
	 UNSPEC_SYNC_EXCHANGE))
   (clobber (match_scratch:GPR 7 "=&r"))]
  ""
{
  return "1:\\n\\t"
	 "ll.<amo>\\t%0,%1\\n\\t"
	 "and\\t%7,%0,%z3\\n\\t"
	 "or%i5\\t%7,%7,%5\\n\\t"
	 "sc.<amo>\\t%7,%1\\n\\t"
	 "beqz\\t%7,1b\\n\\t";
}
  [(set (attr "length") (const_int 20))])

(define_expand "atomic_exchange<mode>"
  [(set (match_operand:SHORT 0 "register_operand")
	(unspec_volatile:SHORT
	  [(match_operand:SHORT 1 "memory_operand")
	   (match_operand:SI 3 "const_int_operand")] ;; model
	  UNSPEC_SYNC_EXCHANGE))
   (set (match_dup 1)
	(match_operand:SHORT 2 "register_operand"))]
  ""
{
  if (ISA_HAS_LAM_BH)
    emit_insn (gen_atomic_exchange<mode>_short (operands[0], operands[1], operands[2], operands[3]));
  else
    {
      union loongarch_gen_fn_ptrs generator;
      generator.fn_7 = gen_atomic_cas_value_exchange_7_si;
      loongarch_expand_atomic_qihi (generator, operands[0], operands[1],
				    const0_rtx, operands[2], operands[3]);
    }
  DONE;
})

(define_insn "atomic_fetch_add<mode>_short"
  [(set (match_operand:SHORT 0 "register_operand" "=&r")
	(match_operand:SHORT 1 "memory_operand" "+ZB"))
   (set (match_dup 1)
	(unspec_volatile:SHORT
	  [(plus:SHORT (match_dup 1)
		     (match_operand:SHORT 2 "reg_or_0_operand" "rJ"))
	   (match_operand:SI 3 "const_int_operand")] ;; model
	 UNSPEC_SYNC_OLD_OP))]
  "ISA_HAS_LAM_BH"
  "amadd%A3.<amo>\t%0,%z2,%1"
  [(set (attr "length") (const_int 4))])

(define_expand "atomic_fetch_add<mode>"
  [(set (match_operand:SHORT 0 "register_operand" "=&r")
	(match_operand:SHORT 1 "memory_operand" "+ZB"))
   (set (match_dup 1)
	(unspec_volatile:SHORT
	  [(plus:SHORT (match_dup 1)
		       (match_operand:SHORT 2 "reg_or_0_operand" "rJ"))
	   (match_operand:SI 3 "const_int_operand")] ;; model
	 UNSPEC_SYNC_OLD_OP))]
  ""
{
  if (ISA_HAS_LAM_BH)
    emit_insn (gen_atomic_fetch_add<mode>_short (operands[0], operands[1],
					     operands[2], operands[3]));
  else
    {
      union loongarch_gen_fn_ptrs generator;
      generator.fn_7 = gen_atomic_cas_value_add_7_si;
      loongarch_expand_atomic_qihi (generator, operands[0], operands[1],
				    operands[1], operands[2], operands[3]);
    }
  DONE;
})

(define_expand "atomic_fetch_sub<mode>"
  [(set (match_operand:SHORT 0 "register_operand" "=&r")
	(match_operand:SHORT 1 "memory_operand" "+ZB"))
   (set (match_dup 1)
	(unspec_volatile:SHORT
	  [(minus:SHORT (match_dup 1)
			(match_operand:SHORT 2 "reg_or_0_operand" "rJ"))
	   (match_operand:SI 3 "const_int_operand")] ;; model
	 UNSPEC_SYNC_OLD_OP))]
  ""
{
  union loongarch_gen_fn_ptrs generator;
  generator.fn_7 = gen_atomic_cas_value_sub_7_si;
  loongarch_expand_atomic_qihi (generator, operands[0], operands[1],
				operands[1], operands[2], operands[3]);
  DONE;
})

(define_expand "atomic_fetch_and<mode>"
  [(set (match_operand:SHORT 0 "register_operand" "=&r")
	(match_operand:SHORT 1 "memory_operand" "+ZB"))
   (set (match_dup 1)
	(unspec_volatile:SHORT
	  [(and:SHORT (match_dup 1)
		      (match_operand:SHORT 2 "reg_or_0_operand" "rJ"))
	   (match_operand:SI 3 "const_int_operand")] ;; model
	 UNSPEC_SYNC_OLD_OP))]
  ""
{
  union loongarch_gen_fn_ptrs generator;
  generator.fn_7 = gen_atomic_cas_value_and_7_si;
  loongarch_expand_atomic_qihi (generator, operands[0], operands[1],
				operands[1], operands[2], operands[3]);
  DONE;
})

(define_expand "atomic_fetch_xor<mode>"
  [(set (match_operand:SHORT 0 "register_operand" "=&r")
	(match_operand:SHORT 1 "memory_operand" "+ZB"))
   (set (match_dup 1)
	(unspec_volatile:SHORT
	  [(xor:SHORT (match_dup 1)
		      (match_operand:SHORT 2 "reg_or_0_operand" "rJ"))
	   (match_operand:SI 3 "const_int_operand")] ;; model
	 UNSPEC_SYNC_OLD_OP))]
  ""
{
  union loongarch_gen_fn_ptrs generator;
  generator.fn_7 = gen_atomic_cas_value_xor_7_si;
  loongarch_expand_atomic_qihi (generator, operands[0], operands[1],
				operands[1], operands[2], operands[3]);
  DONE;
})

(define_expand "atomic_fetch_or<mode>"
  [(set (match_operand:SHORT 0 "register_operand" "=&r")
	(match_operand:SHORT 1 "memory_operand" "+ZB"))
   (set (match_dup 1)
	(unspec_volatile:SHORT
	  [(ior:SHORT (match_dup 1)
		      (match_operand:SHORT 2 "reg_or_0_operand" "rJ"))
	   (match_operand:SI 3 "const_int_operand")] ;; model
	 UNSPEC_SYNC_OLD_OP))]
  ""
{
  union loongarch_gen_fn_ptrs generator;
  generator.fn_7 = gen_atomic_cas_value_or_7_si;
  loongarch_expand_atomic_qihi (generator, operands[0], operands[1],
				operands[1], operands[2], operands[3]);
  DONE;
})

(define_expand "atomic_fetch_nand<mode>"
  [(set (match_operand:SHORT 0 "register_operand" "=&r")
	(match_operand:SHORT 1 "memory_operand" "+ZB"))
   (set (match_dup 1)
	(unspec_volatile:SHORT
	  [(not:SHORT (and:SHORT (match_dup 1)
				 (match_operand:SHORT 2 "reg_or_0_operand" "rJ")))
	   (match_operand:SI 3 "const_int_operand")] ;; model
	 UNSPEC_SYNC_OLD_OP))]
  ""
{
  union loongarch_gen_fn_ptrs generator;
  generator.fn_7 = gen_atomic_cas_value_nand_7_si;
  loongarch_expand_atomic_qihi (generator, operands[0], operands[1],
				operands[1], operands[2], operands[3]);
  DONE;
})
