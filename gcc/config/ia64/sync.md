;; GCC machine description for IA-64 synchronization instructions.
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

;; Conversion to C++11 memory model based on
;; http://www.cl.cam.ac.uk/~pes20/cpp/cpp0xmappings.html

(define_mode_iterator IMODE [QI HI SI DI])
(define_mode_iterator I124MODE [QI HI SI])
(define_mode_iterator I48MODE [SI DI])
(define_mode_attr modesuffix [(QI "1") (HI "2") (SI "4") (DI "8")])

(define_code_iterator FETCHOP [plus minus ior xor and])
(define_code_attr fetchop_name
  [(plus "add") (minus "sub") (ior "or") (xor "xor") (and "and")])

(define_expand "mem_thread_fence"
  [(match_operand:SI 0 "const_int_operand" "")]		;; model
  ""
{
  if (is_mm_seq_cst (memmodel_from_int (INTVAL (operands[0]))))
    emit_insn (gen_memory_barrier ());
  DONE;
})

(define_expand "memory_barrier"
  [(set (match_dup 0)
	(unspec:BLK [(match_dup 0)] UNSPEC_MF))]
  ""
{
  operands[0] = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (Pmode));
  MEM_VOLATILE_P (operands[0]) = 1;
})

(define_insn "*memory_barrier"
  [(set (match_operand:BLK 0 "" "")
	(unspec:BLK [(match_dup 0)] UNSPEC_MF))]
  ""
  "mf"
  [(set_attr "itanium_class" "syst_m")])

(define_expand "atomic_load<mode>"
  [(match_operand:IMODE 0 "gr_register_operand" "")		;; output
   (match_operand:IMODE 1 "memory_operand" "")			;; memory
   (match_operand:SI 2 "const_int_operand" "")]			;; model
  ""
{
  enum memmodel model = memmodel_from_int (INTVAL (operands[2]));

  /* Unless the memory model is relaxed, we want to emit ld.acq, which
     will happen automatically for volatile memories.  */
  gcc_assert (is_mm_relaxed (model) || MEM_VOLATILE_P (operands[1]));
  emit_move_insn (operands[0], operands[1]);
  DONE;
})

(define_expand "atomic_store<mode>"
  [(match_operand:IMODE 0 "memory_operand" "")			;; memory
   (match_operand:IMODE 1 "gr_reg_or_0_operand" "")		;; input
   (match_operand:SI 2 "const_int_operand" "")]			;; model
  ""
{
  enum memmodel model = memmodel_from_int (INTVAL (operands[2]));

  /* Unless the memory model is relaxed, we want to emit st.rel, which
     will happen automatically for volatile memories.  */
  gcc_assert (is_mm_relaxed (model) || MEM_VOLATILE_P (operands[0]));
  emit_move_insn (operands[0], operands[1]);

  /* Sequentially consistent stores need a subsequent MF.  See
     http://www.decadent.org.uk/pipermail/cpp-threads/2008-December/001952.html
     for a discussion of why a MF is needed here, but not for atomic_load.  */
  if (is_mm_seq_cst (model))
    emit_insn (gen_memory_barrier ());
  DONE;
})

(define_expand "atomic_compare_and_swap<mode>"
  [(match_operand:DI 0 "gr_register_operand" "")		;; bool out
   (match_operand:IMODE 1 "gr_register_operand" "")		;; val out
   (match_operand:IMODE 2 "not_postinc_memory_operand" "")	;; memory
   (match_operand:IMODE 3 "gr_register_operand" "")		;; expected
   (match_operand:IMODE 4 "gr_reg_or_0_operand" "")		;; desired
   (match_operand:SI 5 "const_int_operand" "")			;; is_weak
   (match_operand:SI 6 "const_int_operand" "")			;; succ model
   (match_operand:SI 7 "const_int_operand" "")]			;; fail model
  ""
{
  /* No need to distinquish __sync from __atomic, so get base value.  */
  enum memmodel model = memmodel_base (INTVAL (operands[6]));
  rtx ccv = gen_rtx_REG (DImode, AR_CCV_REGNUM);
  rtx dval, eval;

  eval = gen_reg_rtx (DImode);
  convert_move (eval, operands[3], 1);
  emit_move_insn (ccv, eval);

  if (<MODE>mode == DImode)
    dval = operands[1];
  else
    dval = gen_reg_rtx (DImode);

  switch (model)
    {
    case MEMMODEL_RELAXED:
    case MEMMODEL_ACQUIRE:
    case MEMMODEL_CONSUME:
      emit_insn (gen_cmpxchg_acq_<mode> (dval, operands[2], ccv, operands[4]));
      break;
    case MEMMODEL_RELEASE:
      emit_insn (gen_cmpxchg_rel_<mode> (dval, operands[2], ccv, operands[4]));
      break;
    case MEMMODEL_ACQ_REL:
    case MEMMODEL_SEQ_CST:
      emit_insn (gen_cmpxchg_rel_<mode> (dval, operands[2], ccv, operands[4]));
      emit_insn (gen_memory_barrier ());
      break;
    default:
      gcc_unreachable ();
    }

  if (<MODE>mode != DImode)
    emit_move_insn (operands[1], gen_lowpart (<MODE>mode, dval));

  emit_insn (gen_cstoredi4 (operands[0], gen_rtx_EQ (DImode, dval, eval),
			    dval, eval));
  DONE;
})

(define_insn "cmpxchg_acq_<mode>"
  [(set (match_operand:DI 0 "gr_register_operand" "=r")
	(zero_extend:DI
	  (match_operand:I124MODE 1 "not_postinc_memory_operand" "+S")))
   (set (match_dup 1)
        (unspec:I124MODE
	  [(match_dup 1)
	   (match_operand:DI 2 "ar_ccv_reg_operand" "")
	   (match_operand:I124MODE 3 "gr_reg_or_0_operand" "rO")]
	  UNSPEC_CMPXCHG_ACQ))]
  ""
  "cmpxchg<modesuffix>.acq %0 = %1, %r3, %2"
  [(set_attr "itanium_class" "sem")])

(define_insn "cmpxchg_rel_<mode>"
  [(set (match_operand:DI 0 "gr_register_operand" "=r")
	(zero_extend:DI
	  (match_operand:I124MODE 1 "not_postinc_memory_operand" "+S")))
   (set (match_dup 1)
        (unspec:I124MODE
	  [(match_dup 1)
	   (match_operand:DI 2 "ar_ccv_reg_operand" "")
	   (match_operand:I124MODE 3 "gr_reg_or_0_operand" "rO")]
	  UNSPEC_CMPXCHG_REL))]
  ""
  "cmpxchg<modesuffix>.rel %0 = %1, %r3, %2"
  [(set_attr "itanium_class" "sem")])

(define_insn "cmpxchg_acq_di"
  [(set (match_operand:DI 0 "gr_register_operand" "=r")
	(match_operand:DI 1 "not_postinc_memory_operand" "+S"))
   (set (match_dup 1)
        (unspec:DI [(match_dup 1)
		    (match_operand:DI 2 "ar_ccv_reg_operand" "")
		    (match_operand:DI 3 "gr_reg_or_0_operand" "rO")]
		   UNSPEC_CMPXCHG_ACQ))]
  ""
  "cmpxchg8.acq %0 = %1, %r3, %2"
  [(set_attr "itanium_class" "sem")])

(define_insn "cmpxchg_rel_di"
  [(set (match_operand:DI 0 "gr_register_operand" "=r")
	(match_operand:DI 1 "not_postinc_memory_operand" "+S"))
   (set (match_dup 1)
        (unspec:DI [(match_dup 1)
		    (match_operand:DI 2 "ar_ccv_reg_operand" "")
		    (match_operand:DI 3 "gr_reg_or_0_operand" "rO")]
		   UNSPEC_CMPXCHG_REL))]
  ""
  "cmpxchg8.rel %0 = %1, %r3, %2"
  [(set_attr "itanium_class" "sem")])

(define_expand "atomic_exchange<mode>"
  [(match_operand:IMODE 0 "gr_register_operand" "")		;; output
   (match_operand:IMODE 1 "not_postinc_memory_operand" "")	;; memory
   (match_operand:IMODE 2 "gr_reg_or_0_operand" "")		;; input
   (match_operand:SI 3 "const_int_operand" "")]			;; succ model
  ""
{
  /* No need to distinquish __sync from __atomic, so get base value.  */
  enum memmodel model = memmodel_base (INTVAL (operands[3]));

  switch (model)
    {
    case MEMMODEL_RELAXED:
    case MEMMODEL_ACQUIRE:
    case MEMMODEL_CONSUME:
      break;
    case MEMMODEL_RELEASE:
    case MEMMODEL_ACQ_REL:
    case MEMMODEL_SEQ_CST:
      emit_insn (gen_memory_barrier ());
      break;
    default:
      gcc_unreachable ();
    }
  emit_insn (gen_xchg_acq_<mode> (operands[0], operands[1], operands[2]));
  DONE;
})

;; Note that XCHG is always memory model acquire.
(define_insn "xchg_acq_<mode>"
  [(set (match_operand:IMODE 0 "gr_register_operand" "=r")
        (match_operand:IMODE 1 "not_postinc_memory_operand" "+S"))
   (set (match_dup 1)
        (match_operand:IMODE 2 "gr_reg_or_0_operand" "rO"))]
  ""
  "xchg<modesuffix> %0 = %1, %r2"
  [(set_attr "itanium_class" "sem")])

(define_expand "atomic_<fetchop_name><mode>"
  [(set (match_operand:IMODE 0 "memory_operand" "")
	(FETCHOP:IMODE (match_dup 0)
	  (match_operand:IMODE 1 "nonmemory_operand" "")))
   (use (match_operand:SI 2 "const_int_operand" ""))]
  ""
{
  ia64_expand_atomic_op (<CODE>, operands[0], operands[1], NULL, NULL,
			 (enum memmodel) INTVAL (operands[2]));
  DONE;
})

(define_expand "atomic_nand<mode>"
  [(set (match_operand:IMODE 0 "memory_operand" "")
	(not:IMODE
	  (and:IMODE (match_dup 0)
		     (match_operand:IMODE 1 "nonmemory_operand" ""))))
   (use (match_operand:SI 2 "const_int_operand" ""))]
  ""
{
  ia64_expand_atomic_op (NOT, operands[0], operands[1], NULL, NULL,
			 (enum memmodel) INTVAL (operands[2]));
  DONE;
})

(define_expand "atomic_fetch_<fetchop_name><mode>"
  [(set (match_operand:IMODE 0 "gr_register_operand" "")
	(FETCHOP:IMODE 
	  (match_operand:IMODE 1 "memory_operand" "")
	  (match_operand:IMODE 2 "nonmemory_operand" "")))
   (use (match_operand:SI 3 "const_int_operand" ""))]
  ""
{
  ia64_expand_atomic_op (<CODE>, operands[1], operands[2], operands[0], NULL,
			 (enum memmodel) INTVAL (operands[3]));
  DONE;
})

(define_expand "atomic_fetch_nand<mode>"
  [(set (match_operand:IMODE 0 "gr_register_operand" "")
	(not:IMODE 
	  (and:IMODE (match_operand:IMODE 1 "memory_operand" "")
		     (match_operand:IMODE 2 "nonmemory_operand" ""))))
   (use (match_operand:SI 3 "const_int_operand" ""))]
  ""
{
  ia64_expand_atomic_op (NOT, operands[1], operands[2], operands[0], NULL,
			 (enum memmodel) INTVAL (operands[3]));
  DONE;
})

(define_expand "atomic_<fetchop_name>_fetch<mode>"
  [(set (match_operand:IMODE 0 "gr_register_operand" "")
	(FETCHOP:IMODE 
	  (match_operand:IMODE 1 "memory_operand" "")
	  (match_operand:IMODE 2 "nonmemory_operand" "")))
   (use (match_operand:SI 3 "const_int_operand" ""))]
  ""
{
  ia64_expand_atomic_op (<CODE>, operands[1], operands[2], NULL, operands[0],
			 (enum memmodel) INTVAL (operands[3]));
  DONE;
})

(define_expand "atomic_nand_fetch<mode>"
  [(set (match_operand:IMODE 0 "gr_register_operand" "")
	(not:IMODE 
	  (and:IMODE (match_operand:IMODE 1 "memory_operand" "")
		     (match_operand:IMODE 2 "nonmemory_operand" ""))))
   (use (match_operand:SI 3 "const_int_operand" ""))]
  ""
{
  ia64_expand_atomic_op (NOT, operands[1], operands[2], NULL, operands[0],
			 (enum memmodel) INTVAL (operands[3]));
  DONE;
})

(define_insn "fetchadd_acq_<mode>"
  [(set (match_operand:I48MODE 0 "gr_register_operand" "=r")
	(match_operand:I48MODE 1 "not_postinc_memory_operand" "+S"))
   (set (match_dup 1)
	(unspec:I48MODE [(match_dup 1)
			 (match_operand:I48MODE 2 "fetchadd_operand" "n")]
		        UNSPEC_FETCHADD_ACQ))]
  ""
  "fetchadd<modesuffix>.acq %0 = %1, %2"
  [(set_attr "itanium_class" "sem")])

(define_insn "fetchadd_rel_<mode>"
  [(set (match_operand:I48MODE 0 "gr_register_operand" "=r")
	(match_operand:I48MODE 1 "not_postinc_memory_operand" "+S"))
   (set (match_dup 1)
	(unspec:I48MODE [(match_dup 1)
			 (match_operand:I48MODE 2 "fetchadd_operand" "n")]
		        UNSPEC_FETCHADD_REL))]
  ""
  "fetchadd<modesuffix>.rel %0 = %1, %2"
  [(set_attr "itanium_class" "sem")])
