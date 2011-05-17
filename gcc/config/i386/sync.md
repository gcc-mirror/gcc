;; GCC machine description for i386 synchronization instructions.
;; Copyright (C) 2005, 2006, 2007, 2008, 2009, 2010, 2011
;; Free Software Foundation, Inc.
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

(define_mode_iterator CASMODE
  [QI HI SI (DI "TARGET_64BIT || TARGET_CMPXCHG8B")
	    (TI "TARGET_64BIT && TARGET_CMPXCHG16B")])
(define_mode_iterator DCASMODE
  [(DI "!TARGET_64BIT && TARGET_CMPXCHG8B && !flag_pic")
   (TI "TARGET_64BIT && TARGET_CMPXCHG16B")])
(define_mode_attr doublemodesuffix [(DI "8") (TI "16")])
(define_mode_attr DCASHMODE [(DI "SI") (TI "DI")])

(define_expand "memory_barrier"
  [(set (match_dup 0)
	(unspec:BLK [(match_dup 0)] UNSPEC_MFENCE))]
  ""
{
  operands[0] = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (Pmode));
  MEM_VOLATILE_P (operands[0]) = 1;

  if (!(TARGET_64BIT || TARGET_SSE2))
    {
      emit_insn (gen_memory_barrier_nosse (operands[0]));
      DONE;
    }
})

(define_insn "memory_barrier_nosse"
  [(set (match_operand:BLK 0 "" "")
	(unspec:BLK [(match_dup 0)] UNSPEC_MFENCE))
   (clobber (reg:CC FLAGS_REG))]
  "!(TARGET_64BIT || TARGET_SSE2)"
  "lock{%;} or{l}\t{$0, (%%esp)|DWORD PTR [esp], 0}"
  [(set_attr "memory" "unknown")])

;; ??? It would be possible to use cmpxchg8b on pentium for DImode
;; changes.  It's complicated because the insn uses ecx:ebx as the
;; new value; note that the registers are reversed from the order
;; that they'd be in with (reg:DI 2 ecx).  Similarly for TImode
;; data in 64-bit mode.

(define_expand "sync_compare_and_swap<mode>"
  [(parallel
    [(set (match_operand:CASMODE 0 "register_operand" "")
	  (match_operand:CASMODE 1 "memory_operand" ""))
     (set (match_dup 1)
	  (unspec_volatile:CASMODE
	    [(match_dup 1)
	     (match_operand:CASMODE 2 "register_operand" "")
	     (match_operand:CASMODE 3 "register_operand" "")]
	    UNSPECV_CMPXCHG))
   (set (reg:CCZ FLAGS_REG)
        (compare:CCZ
          (unspec_volatile:CASMODE
            [(match_dup 1) (match_dup 2) (match_dup 3)] UNSPECV_CMPXCHG)
          (match_dup 2)))])]
  "TARGET_CMPXCHG"
{
  if ((<MODE>mode == DImode && !TARGET_64BIT) || <MODE>mode == TImode)
    {
      enum machine_mode hmode = <MODE>mode == DImode ? SImode : DImode;
      rtx low = simplify_gen_subreg (hmode, operands[3], <MODE>mode, 0);
      rtx high = simplify_gen_subreg (hmode, operands[3], <MODE>mode,
				      GET_MODE_SIZE (hmode));
      low = force_reg (hmode, low);
      high = force_reg (hmode, high);
      if (<MODE>mode == DImode)
	{
	  if (flag_pic && !cmpxchg8b_pic_memory_operand (operands[1], DImode))
	    operands[1] = replace_equiv_address (operands[1],
						 force_reg (Pmode,
							    XEXP (operands[1],
								  0)));
	  emit_insn (gen_sync_double_compare_and_swapdi
		     (operands[0], operands[1], operands[2], low, high));
	}
      else if (<MODE>mode == TImode)
	emit_insn (gen_sync_double_compare_and_swapti
		   (operands[0], operands[1], operands[2], low, high));
      else
	gcc_unreachable ();
      DONE;
    }
})

(define_insn "*sync_compare_and_swap<mode>"
  [(set (match_operand:SWI 0 "register_operand" "=a")
	(match_operand:SWI 1 "memory_operand" "+m"))
   (set (match_dup 1)
	(unspec_volatile:SWI
	  [(match_dup 1)
	   (match_operand:SWI 2 "register_operand" "a")
	   (match_operand:SWI 3 "register_operand" "<r>")]
	  UNSPECV_CMPXCHG))
   (set (reg:CCZ FLAGS_REG)
        (compare:CCZ
          (unspec_volatile:SWI
            [(match_dup 1) (match_dup 2) (match_dup 3)] UNSPECV_CMPXCHG)
          (match_dup 2)))]
  "TARGET_CMPXCHG"
  "lock{%;} cmpxchg{<imodesuffix>}\t{%3, %1|%1, %3}")

(define_insn "sync_double_compare_and_swap<mode>"
  [(set (match_operand:DCASMODE 0 "register_operand" "=A")
	(match_operand:DCASMODE 1 "memory_operand" "+m"))
   (set (match_dup 1)
	(unspec_volatile:DCASMODE
	  [(match_dup 1)
	   (match_operand:DCASMODE 2 "register_operand" "A")
	   (match_operand:<DCASHMODE> 3 "register_operand" "b")
	   (match_operand:<DCASHMODE> 4 "register_operand" "c")]
	  UNSPECV_CMPXCHG))
   (set (reg:CCZ FLAGS_REG)
        (compare:CCZ
          (unspec_volatile:DCASMODE
            [(match_dup 1) (match_dup 2) (match_dup 3) (match_dup 4)]
	    UNSPECV_CMPXCHG)
          (match_dup 2)))]
  ""
  "lock{%;} cmpxchg<doublemodesuffix>b\t%1")

;; Theoretically we'd like to use constraint "r" (any reg) for operand
;; 3, but that includes ecx.  If operand 3 and 4 are the same (like when
;; the input is -1LL) GCC might chose to allocate operand 3 to ecx, like
;; operand 4.  This breaks, as the xchg will move the PIC register contents
;; to %ecx then --> boom.  Operands 3 and 4 really need to be different
;; registers, which in this case means operand 3 must not be ecx.
;; Instead of playing tricks with fake early clobbers or the like we
;; just enumerate all regs possible here, which (as this is !TARGET_64BIT)
;; are just esi and edi.
(define_insn "*sync_double_compare_and_swapdi_pic"
  [(set (match_operand:DI 0 "register_operand" "=A")
	(match_operand:DI 1 "cmpxchg8b_pic_memory_operand" "+m"))
   (set (match_dup 1)
	(unspec_volatile:DI
	  [(match_dup 1)
	   (match_operand:DI 2 "register_operand" "A")
	   (match_operand:SI 3 "register_operand" "SD")
	   (match_operand:SI 4 "register_operand" "c")]
	  UNSPECV_CMPXCHG))
   (set (reg:CCZ FLAGS_REG)
	(compare:CCZ
	  (unspec_volatile:DI
	    [(match_dup 1) (match_dup 2) (match_dup 3) (match_dup 4)]
	    UNSPECV_CMPXCHG)
	  (match_dup 2)))]
  "!TARGET_64BIT && TARGET_CMPXCHG8B && flag_pic"
  "xchg{l}\t%%ebx, %3\;lock{%;} cmpxchg8b\t%1\;xchg{l}\t%%ebx, %3")

;; For operand 2 nonmemory_operand predicate is used instead of
;; register_operand to allow combiner to better optimize atomic
;; additions of constants.
(define_insn "sync_old_add<mode>"
  [(set (match_operand:SWI 0 "register_operand" "=<r>")
	(unspec_volatile:SWI
	  [(match_operand:SWI 1 "memory_operand" "+m")] UNSPECV_XCHG))
   (set (match_dup 1)
	(plus:SWI (match_dup 1)
		  (match_operand:SWI 2 "nonmemory_operand" "0")))
   (clobber (reg:CC FLAGS_REG))]
  "TARGET_XADD"
  "lock{%;} xadd{<imodesuffix>}\t{%0, %1|%1, %0}")

;; Recall that xchg implicitly sets LOCK#, so adding it again wastes space.
(define_insn "sync_lock_test_and_set<mode>"
  [(set (match_operand:SWI 0 "register_operand" "=<r>")
	(unspec_volatile:SWI
	  [(match_operand:SWI 1 "memory_operand" "+m")] UNSPECV_XCHG))
   (set (match_dup 1)
	(match_operand:SWI 2 "register_operand" "0"))]
  ""
  "xchg{<imodesuffix>}\t{%1, %0|%0, %1}")

(define_insn "sync_add<mode>"
  [(set (match_operand:SWI 0 "memory_operand" "+m")
	(unspec_volatile:SWI
	  [(plus:SWI (match_dup 0)
		     (match_operand:SWI 1 "nonmemory_operand" "<r><i>"))]
	  UNSPECV_LOCK))
   (clobber (reg:CC FLAGS_REG))]
  ""
{
  if (TARGET_USE_INCDEC)
    {
      if (operands[1] == const1_rtx)
	return "lock{%;} inc{<imodesuffix>}\t%0";
      if (operands[1] == constm1_rtx)
	return "lock{%;} dec{<imodesuffix>}\t%0";
    }

  if (x86_maybe_negate_const_int (&operands[1], <MODE>mode))
    return "lock{%;} sub{<imodesuffix>}\t{%1, %0|%0, %1}";

  return "lock{%;} add{<imodesuffix>}\t{%1, %0|%0, %1}";
})

(define_insn "sync_sub<mode>"
  [(set (match_operand:SWI 0 "memory_operand" "+m")
	(unspec_volatile:SWI
	  [(minus:SWI (match_dup 0)
		      (match_operand:SWI 1 "nonmemory_operand" "<r><i>"))]
	  UNSPECV_LOCK))
   (clobber (reg:CC FLAGS_REG))]
  ""
{
  if (TARGET_USE_INCDEC)
    {
      if (operands[1] == const1_rtx)
	return "lock{%;} dec{<imodesuffix>}\t%0";
      if (operands[1] == constm1_rtx)
	return "lock{%;} inc{<imodesuffix>}\t%0";
    }

  return "lock{%;} sub{<imodesuffix>}\t{%1, %0|%0, %1}";
})

(define_insn "sync_<code><mode>"
  [(set (match_operand:SWI 0 "memory_operand" "+m")
	(unspec_volatile:SWI
	  [(any_logic:SWI (match_dup 0)
			  (match_operand:SWI 1 "nonmemory_operand" "<r><i>"))]
	  UNSPECV_LOCK))
   (clobber (reg:CC FLAGS_REG))]
  ""
  "lock{%;} <logic>{<imodesuffix>}\t{%1, %0|%0, %1}")
