;; Machine description for PowerPC synchronization instructions.
;; Copyright (C) 2005, 2007, 2008, 2009, 2011
;; Free Software Foundation, Inc.
;; Contributed by Geoffrey Keating.

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.

;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

(define_mode_attr larx [(SI "lwarx") (DI "ldarx")])
(define_mode_attr stcx [(SI "stwcx.") (DI "stdcx.")])

(define_code_iterator FETCHOP [plus minus ior xor and])
(define_code_attr fetchop_name
  [(plus "add") (minus "sub") (ior "or") (xor "xor") (and "and")])
(define_code_attr fetchop_pred
  [(plus "add_operand") (minus "gpc_reg_operand")
   (ior "logical_operand") (xor "logical_operand") (and "and_operand")])

(define_expand "mem_thread_fence"
  [(match_operand:SI 0 "const_int_operand" "")]		;; model
  ""
{
  enum memmodel model = (enum memmodel) INTVAL (operands[0]);
  switch (model)
    {
    case MEMMODEL_RELAXED:
      break;
    case MEMMODEL_CONSUME:
    case MEMMODEL_ACQUIRE:
    case MEMMODEL_RELEASE:
    case MEMMODEL_ACQ_REL:
      emit_insn (gen_lwsync ());
      break;
    case MEMMODEL_SEQ_CST:
      emit_insn (gen_hwsync ());
      break;
    default:
      gcc_unreachable ();
    }
  DONE;
})

(define_expand "hwsync"
  [(set (match_dup 0)
	(unspec:BLK [(match_dup 0)] UNSPEC_SYNC))]
  ""
{
  operands[0] = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (Pmode));
  MEM_VOLATILE_P (operands[0]) = 1;
})

(define_insn "*hwsync"
  [(set (match_operand:BLK 0 "" "")
	(unspec:BLK [(match_dup 0)] UNSPEC_SYNC))]
  ""
  "{dcs|sync}"
  [(set_attr "type" "sync")])

(define_expand "lwsync"
  [(set (match_dup 0)
	(unspec:BLK [(match_dup 0)] UNSPEC_LWSYNC))]
  ""
{
  operands[0] = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (Pmode));
  MEM_VOLATILE_P (operands[0]) = 1;
})

(define_insn "*lwsync"
  [(set (match_operand:BLK 0 "" "")
	(unspec:BLK [(match_dup 0)] UNSPEC_LWSYNC))]
  ""
{
  /* Some AIX assemblers don't accept lwsync, so we use a .long.  */
  if (TARGET_NO_LWSYNC)
    return "sync";
  else if (TARGET_LWSYNC_INSTRUCTION)
    return "lwsync";
  else
    return ".long 0x7c2004ac";
}
  [(set_attr "type" "sync")])

(define_insn "isync"
  [(unspec_volatile:BLK [(const_int 0)] UNSPECV_ISYNC)]
  ""
  "{ics|isync}"
  [(set_attr "type" "isync")])

;; The control dependency used for load dependency described
;; in B.2.3 of the Power ISA 2.06B.
(define_insn "loadsync"
  [(unspec_volatile:BLK [(match_operand 0 "register_operand" "r")]
			UNSPECV_ISYNC)
   (clobber (match_scratch:CC 1 "=y"))]
  ""
  "cmpw %1,%0,%0\;bne- %1,$+4\;isync"
  [(set_attr "type" "isync")
   (set_attr "length" "12")])

(define_expand "atomic_load<mode>"
  [(set (match_operand:INT1 0 "register_operand" "")		;; output
	(match_operand:INT1 1 "memory_operand" ""))		;; memory
   (use (match_operand:SI 2 "const_int_operand" ""))]		;; model
  ""
{
  enum memmodel model = (enum memmodel) INTVAL (operands[2]);

  if (model == MEMMODEL_SEQ_CST)
    emit_insn (gen_hwsync ());

  emit_move_insn (operands[0], operands[1]);

  switch (model)
    {
    case MEMMODEL_RELAXED:
      break;
    case MEMMODEL_CONSUME:
    case MEMMODEL_ACQUIRE:
    case MEMMODEL_SEQ_CST:
      emit_insn (gen_loadsync (operands[0]));
      break;
    default:
      gcc_unreachable ();
    }
  DONE;
})

(define_expand "atomic_store<mode>"
  [(set (match_operand:INT1 0 "memory_operand" "")		;; memory
	(match_operand:INT1 1 "register_operand" ""))		;; input
   (use (match_operand:SI 2 "const_int_operand" ""))]		;; model
  ""
{
  enum memmodel model = (enum memmodel) INTVAL (operands[2]);
  switch (model)
    {
    case MEMMODEL_RELAXED:
      break;
    case MEMMODEL_RELEASE:
      emit_insn (gen_lwsync ());
      break;
    case MEMMODEL_SEQ_CST:
      emit_insn (gen_hwsync ());
      break;
    default:
      gcc_unreachable ();
    }
  emit_move_insn (operands[0], operands[1]);
  DONE;
})

;; ??? Power ISA 2.06B says that there *is* a load-{byte,half}-and-reserve
;; opcode that is "phased-in".  Not implemented as of Power7, so not yet used,
;; but let's prepare the macros anyway.

(define_mode_iterator ATOMIC    [SI (DI "TARGET_64BIT")])

(define_insn "load_locked<mode>"
  [(set (match_operand:ATOMIC 0 "gpc_reg_operand" "=r")
	(unspec_volatile:ATOMIC
         [(match_operand:ATOMIC 1 "memory_operand" "Z")] UNSPECV_LL))]
  "TARGET_POWERPC"
  "<larx> %0,%y1"
  [(set_attr "type" "load_l")])

(define_insn "store_conditional<mode>"
  [(set (match_operand:CC 0 "cc_reg_operand" "=x")
	(unspec_volatile:CC [(const_int 0)] UNSPECV_SC))
   (set (match_operand:ATOMIC 1 "memory_operand" "=Z")
	(match_operand:ATOMIC 2 "gpc_reg_operand" "r"))]
  "TARGET_POWERPC"
  "<stcx> %2,%y1"
  [(set_attr "type" "store_c")])

(define_expand "atomic_compare_and_swap<mode>"
  [(match_operand:SI 0 "gpc_reg_operand" "")		;; bool out
   (match_operand:INT1 1 "gpc_reg_operand" "")		;; val out
   (match_operand:INT1 2 "memory_operand" "")		;; memory
   (match_operand:INT1 3 "reg_or_short_operand" "")	;; expected
   (match_operand:INT1 4 "gpc_reg_operand" "")		;; desired
   (match_operand:SI 5 "const_int_operand" "")		;; is_weak
   (match_operand:SI 6 "const_int_operand" "")		;; model succ
   (match_operand:SI 7 "const_int_operand" "")]		;; model fail
  "TARGET_POWERPC"
{
  rs6000_expand_atomic_compare_and_swap (operands);
  DONE;
})

(define_expand "atomic_exchange<mode>"
  [(match_operand:INT1 0 "gpc_reg_operand" "")		;; output
   (match_operand:INT1 1 "memory_operand" "")		;; memory
   (match_operand:INT1 2 "gpc_reg_operand" "")		;; input
   (match_operand:SI 3 "const_int_operand" "")]		;; model
  "TARGET_POWERPC"
{
  rs6000_expand_atomic_exchange (operands);
  DONE;
})

(define_expand "atomic_<fetchop_name><mode>"
  [(match_operand:INT1 0 "memory_operand" "")		;; memory
   (FETCHOP:INT1 (match_dup 0)
     (match_operand:INT1 1 "<fetchop_pred>" ""))	;; operand
   (match_operand:SI 2 "const_int_operand" "")]		;; model
  "TARGET_POWERPC"
{
  rs6000_expand_atomic_op (<CODE>, operands[0], operands[1],
			   NULL_RTX, NULL_RTX, operands[2]);
  DONE;
})

(define_expand "atomic_nand<mode>"
  [(match_operand:INT1 0 "memory_operand" "")		;; memory
   (match_operand:INT1 1 "gpc_reg_operand" "")		;; operand
   (match_operand:SI 2 "const_int_operand" "")]		;; model
  "TARGET_POWERPC"
{
  rs6000_expand_atomic_op (NOT, operands[0], operands[1],
			   NULL_RTX, NULL_RTX, operands[2]);
  DONE;
})

(define_expand "atomic_fetch_<fetchop_name><mode>"
  [(match_operand:INT1 0 "gpc_reg_operand" "")		;; output
   (match_operand:INT1 1 "memory_operand" "")		;; memory
   (FETCHOP:INT1 (match_dup 1)
     (match_operand:INT1 2 "<fetchop_pred>" ""))	;; operand
   (match_operand:SI 3 "const_int_operand" "")]		;; model
  "TARGET_POWERPC"
{ 
  rs6000_expand_atomic_op (<CODE>, operands[1], operands[2],
			   operands[0], NULL_RTX, operands[3]);
  DONE;
})

(define_expand "atomic_fetch_nand<mode>"
  [(match_operand:INT1 0 "gpc_reg_operand" "")		;; output
   (match_operand:INT1 1 "memory_operand" "")		;; memory
   (match_operand:INT1 2 "gpc_reg_operand" "")		;; operand
   (match_operand:SI 3 "const_int_operand" "")]		;; model
  "TARGET_POWERPC"
{
  rs6000_expand_atomic_op (NOT, operands[1], operands[2],
			   operands[0], NULL_RTX, operands[3]);
  DONE;
})

(define_expand "atomic_<fetchop_name>_fetch<mode>"
  [(match_operand:INT1 0 "gpc_reg_operand" "")		;; output
   (match_operand:INT1 1 "memory_operand" "")		;; memory
   (FETCHOP:INT1 (match_dup 1)
     (match_operand:INT1 2 "<fetchop_pred>" ""))	;; operand
   (match_operand:SI 3 "const_int_operand" "")]		;; model
  "TARGET_POWERPC"
{
  rs6000_expand_atomic_op (<CODE>, operands[1], operands[2],
			   NULL_RTX, operands[0], operands[3]);
  DONE;
})

(define_expand "atomic_nand_fetch<mode>"
  [(match_operand:INT1 0 "gpc_reg_operand" "")		;; output
   (match_operand:INT1 1 "memory_operand" "")		;; memory
   (match_operand:INT1 2 "gpc_reg_operand" "")		;; operand
   (match_operand:SI 3 "const_int_operand" "")]		;; model
  "TARGET_POWERPC"
{
  rs6000_expand_atomic_op (NOT, operands[1], operands[2],
			   NULL_RTX, operands[0], operands[3]);
  DONE;
})
