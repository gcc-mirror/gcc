;; GCC machine description for SPARC synchronization instructions.
;; Copyright (C) 2005-2016 Free Software Foundation, Inc.
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

(define_mode_iterator I12MODE [QI HI])
(define_mode_iterator I124MODE [QI HI SI])
(define_mode_iterator I24MODE [HI SI])
(define_mode_iterator I48MODE [SI (DI "TARGET_ARCH64 || TARGET_V8PLUS")])
(define_mode_attr modesuffix [(SI "") (DI "x")])

(define_expand "mem_thread_fence"
  [(match_operand:SI 0 "const_int_operand")]
  "TARGET_V8 || TARGET_V9"
{
  enum memmodel model = (enum memmodel) INTVAL (operands[0]);
  sparc_emit_membar_for_model (model, 3, 3);
  DONE;
})

(define_expand "membar"
  [(set (match_dup 1)
	(unspec:BLK [(match_dup 1) (match_operand:SI 0 "const_int_operand")]
		    UNSPEC_MEMBAR))]
  "TARGET_V8 || TARGET_V9"
{
  operands[1] = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (Pmode));
  MEM_VOLATILE_P (operands[1]) = 1;
})

;; A compiler-only memory barrier.  Generic code, when checking for the
;; existence of various named patterns, uses asm("":::"memory") when we
;; don't need an actual instruction.  Here, it's easiest to pretend that
;; membar 0 is such a barrier.  Further, this gives us a nice hook to 
;; ignore all such barriers on Sparc V7.
(define_insn "*membar_empty"
  [(set (match_operand:BLK 0 "" "")
	(unspec:BLK [(match_dup 0) (match_operand:SI 1 "zero_or_v7_operand")]
		    UNSPEC_MEMBAR))]
  ""
  ""
  [(set_attr "type" "multi")
   (set_attr "length" "0")])

;; For V8, STBAR is exactly membar #StoreStore, by definition.
(define_insn "*membar_storestore"
  [(set (match_operand:BLK 0 "" "")
	(unspec:BLK [(match_dup 0) (const_int 8)] UNSPEC_MEMBAR))]
  "TARGET_V8"
  "stbar"
  [(set_attr "type" "multi")])

;; For LEON3, STB has the effect of membar #StoreLoad.
(define_insn "*membar_storeload_leon3"
  [(set (match_operand:BLK 0 "" "")
	(unspec:BLK [(match_dup 0) (const_int 2)] UNSPEC_MEMBAR))]
  "TARGET_LEON3"
  "stb\t%%g0, [%%sp-1]"
  [(set_attr "type" "store")])

;; For V8, LDSTUB has the effect of membar #StoreLoad.
(define_insn "*membar_storeload"
  [(set (match_operand:BLK 0 "" "")
	(unspec:BLK [(match_dup 0) (const_int 2)] UNSPEC_MEMBAR))]
  "TARGET_V8 && !TARGET_LEON3"
  "ldstub\t[%%sp-1], %%g0"
  [(set_attr "type" "multi")])

;; Put the two together, in combination with the fact that V8 implements PSO
;; as its weakest memory model, means a full barrier.  Match all remaining
;; instances of the membar pattern for Sparc V8.
(define_insn "*membar_v8"
  [(set (match_operand:BLK 0 "" "")
	(unspec:BLK [(match_dup 0) (match_operand:SI 1 "const_int_operand")]
		    UNSPEC_MEMBAR))]
  "TARGET_V8"
  "stbar\n\tldstub\t[%%sp-1], %%g0"
  [(set_attr "type" "multi")
   (set_attr "length" "2")])

;; For V9, we have the full membar instruction.
(define_insn "*membar"
  [(set (match_operand:BLK 0 "" "")
	(unspec:BLK [(match_dup 0) (match_operand:SI 1 "const_int_operand")]
		    UNSPEC_MEMBAR))]
  "TARGET_V9"
  "membar\t%1"
  [(set_attr "type" "multi")])

(define_peephole2
  [(set (match_operand:BLK 0 "" "")
	(unspec:BLK [(match_dup 0) (match_operand:SI 1 "const_int_operand")]
		    UNSPEC_MEMBAR))
   (set (match_operand:BLK 2 "" "")
	(unspec:BLK [(match_dup 2) (match_operand:SI 3 "const_int_operand")]
		    UNSPEC_MEMBAR))]
  ""
  [(set (match_operand:BLK 0 "" "")
	(unspec:BLK [(match_dup 0) (match_dup 1)] UNSPEC_MEMBAR))]
{ operands[1] = GEN_INT (UINTVAL (operands[1]) | UINTVAL (operands[3])); })

(define_expand "atomic_load<mode>"
  [(match_operand:I 0 "register_operand" "")
   (match_operand:I 1 "memory_operand" "")
   (match_operand:SI 2 "const_int_operand" "")]
  ""
{
  enum memmodel model = (enum memmodel) INTVAL (operands[2]);

  sparc_emit_membar_for_model (model, 1, 1);

  if (TARGET_ARCH64 || <MODE>mode != DImode)
    emit_move_insn (operands[0], operands[1]);
  else
    emit_insn (gen_atomic_loaddi_1 (operands[0], operands[1]));

  sparc_emit_membar_for_model (model, 1, 2);
  DONE;
})

(define_insn "atomic_loaddi_1"
  [(set (match_operand:DI 0 "register_operand" "=U,?*f")
	(unspec:DI [(match_operand:DI 1 "memory_operand" "m,m")]
		   UNSPEC_ATOMIC))]
  "!TARGET_ARCH64"
  "ldd\t%1, %0"
  [(set_attr "type" "load,fpload")])

(define_expand "atomic_store<mode>"
  [(match_operand:I 0 "memory_operand" "")
   (match_operand:I 1 "register_operand" "")
   (match_operand:SI 2 "const_int_operand" "")]
  ""
{
  enum memmodel model = (enum memmodel) INTVAL (operands[2]);

  sparc_emit_membar_for_model (model, 2, 1);

  if (TARGET_ARCH64 || <MODE>mode != DImode)
    emit_move_insn (operands[0], operands[1]);
  else
    emit_insn (gen_atomic_storedi_1 (operands[0], operands[1]));

  sparc_emit_membar_for_model (model, 2, 2);
  DONE;
})

(define_insn "atomic_storedi_1"
  [(set (match_operand:DI 0 "memory_operand" "=m,m,m")
	(unspec:DI
	  [(match_operand:DI 1 "register_or_v9_zero_operand" "J,U,?*f")]
	  UNSPEC_ATOMIC))]
  "!TARGET_ARCH64"
  "@
   stx\t%r1, %0
   std\t%1, %0
   std\t%1, %0"
  [(set_attr "type" "store,store,fpstore")
   (set_attr "cpu_feature" "v9,*,*")])

(define_expand "atomic_compare_and_swap<mode>"
  [(match_operand:SI 0 "register_operand" "")		;; bool output
   (match_operand:I 1 "register_operand" "")		;; val output
   (match_operand:I 2 "mem_noofs_operand" "")		;; memory
   (match_operand:I 3 "register_operand" "")		;; expected
   (match_operand:I 4 "register_operand" "")		;; desired
   (match_operand:SI 5 "const_int_operand" "")		;; is_weak
   (match_operand:SI 6 "const_int_operand" "")		;; mod_s
   (match_operand:SI 7 "const_int_operand" "")]		;; mod_f
  "(TARGET_V9 || TARGET_LEON3)
   && (<MODE>mode != DImode || TARGET_ARCH64 || TARGET_V8PLUS)"
{
  sparc_expand_compare_and_swap (operands);
  DONE;
})

(define_expand "atomic_compare_and_swap<mode>_1"
  [(parallel
     [(set (match_operand:I48MODE 0 "register_operand" "")
	   (match_operand:I48MODE 1 "mem_noofs_operand" ""))
      (set (match_dup 1)
	   (unspec_volatile:I48MODE
	     [(match_operand:I48MODE 2 "register_operand" "")
	      (match_operand:I48MODE 3 "register_operand" "")]
	     UNSPECV_CAS))])]
  "TARGET_V9 || TARGET_LEON3"
  "")

(define_insn "*atomic_compare_and_swap<mode>_1"
  [(set (match_operand:I48MODE 0 "register_operand" "=r")
	(match_operand:I48MODE 1 "mem_noofs_operand" "+w"))
   (set (match_dup 1)
	(unspec_volatile:I48MODE
	  [(match_operand:I48MODE 2 "register_operand" "r")
	   (match_operand:I48MODE 3 "register_operand" "0")]
	  UNSPECV_CAS))]
  "TARGET_V9 && (<MODE>mode != DImode || TARGET_ARCH64)"
  "cas<modesuffix>\t%1, %2, %0"
  [(set_attr "type" "multi")])

(define_insn "*atomic_compare_and_swap_leon3_1"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(match_operand:SI 1 "mem_noofs_operand" "+w"))
   (set (match_dup 1)
	(unspec_volatile:SI
	  [(match_operand:SI 2 "register_operand" "r")
	   (match_operand:SI 3 "register_operand" "0")]
	  UNSPECV_CAS))]
  "TARGET_LEON3"
{
  if (TARGET_SV_MODE)
    return "casa\t%1 0xb, %2, %0"; /* ASI for supervisor data space.  */
  else
    return "casa\t%1 0xa, %2, %0"; /* ASI for user data space.  */
}
  [(set_attr "type" "multi")])

(define_insn "*atomic_compare_and_swapdi_v8plus"
  [(set (match_operand:DI 0 "register_operand" "=h")
	(match_operand:DI 1 "mem_noofs_operand" "+w"))
   (set (match_dup 1)
	(unspec_volatile:DI
	  [(match_operand:DI 2 "register_operand" "h")
	   (match_operand:DI 3 "register_operand" "0")]
	  UNSPECV_CAS))]
  "TARGET_V8PLUS"
{
  if (sparc_check_64 (operands[3], insn) <= 0)
    output_asm_insn ("srl\t%L3, 0, %L3", operands);
  output_asm_insn ("sllx\t%H3, 32, %H3", operands);
  output_asm_insn ("or\t%L3, %H3, %L3", operands);
  if (sparc_check_64 (operands[2], insn) <= 0)
    output_asm_insn ("srl\t%L2, 0, %L2", operands);
  output_asm_insn ("sllx\t%H2, 32, %H3", operands);
  output_asm_insn ("or\t%L2, %H3, %H3", operands);
  output_asm_insn ("casx\t%1, %H3, %L3", operands);
  return "srlx\t%L3, 32, %H3";
}
  [(set_attr "type" "multi")
   (set_attr "length" "8")])

(define_expand "atomic_exchangesi"
  [(match_operand:SI 0 "register_operand" "")
   (match_operand:SI 1 "memory_operand" "")
   (match_operand:SI 2 "register_operand" "")
   (match_operand:SI 3 "const_int_operand" "")]
  "(TARGET_V8 || TARGET_V9) && !sparc_fix_ut699"
{
  enum memmodel model = (enum memmodel) INTVAL (operands[3]);

  sparc_emit_membar_for_model (model, 3, 1);
  emit_insn (gen_swapsi (operands[0], operands[1], operands[2]));
  sparc_emit_membar_for_model (model, 3, 2);
  DONE;
})

(define_insn "swapsi"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec_volatile:SI [(match_operand:SI 1 "memory_operand" "+m")]
			    UNSPECV_SWAP))
   (set (match_dup 1)
	(match_operand:SI 2 "register_operand" "0"))]
  "(TARGET_V8 || TARGET_V9) && !sparc_fix_ut699"
  "swap\t%1, %0"
  [(set_attr "type" "multi")])

(define_expand "atomic_test_and_set"
  [(match_operand:QI 0 "register_operand" "")
   (match_operand:QI 1 "memory_operand" "")
   (match_operand:SI 2 "const_int_operand" "")]
  "!sparc_fix_ut699"
{
  enum memmodel model = (enum memmodel) INTVAL (operands[2]);
  rtx ret;

  sparc_emit_membar_for_model (model, 3, 1);
  emit_insn (gen_ldstub (operands[0], operands[1]));
  sparc_emit_membar_for_model (model, 3, 2);

  /* Convert the 0/0xff result we would otherwise have to a boolean.
     I.e. ignore all but bit 0.  */
  ret = expand_simple_binop (QImode, AND, operands[0], const1_rtx,
			     operands[0], true, OPTAB_LIB_WIDEN);
  if (ret != operands[0])
    emit_move_insn (operands[0], ret);

  DONE;
})

(define_insn "ldstub"
  [(set (match_operand:QI 0 "register_operand" "=r")
	(unspec_volatile:QI [(match_operand:QI 1 "memory_operand" "+m")]
			    UNSPECV_LDSTUB))
   (set (match_dup 1) (const_int -1))]
  "!sparc_fix_ut699"
  "ldstub\t%1, %0"
  [(set_attr "type" "multi")])
