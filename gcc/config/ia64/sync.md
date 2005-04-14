;; GCC machine description for IA-64 synchronization instructions.
;; Copyright (C) 2005
;; Free Software Foundation, Inc.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING.  If not, write to
;; the Free Software Foundation, 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(define_mode_macro I48MODE [SI DI])
(define_mode_attr modesuffix [(SI "4") (DI "8")])


(define_expand "memory_barrier"
  [(set (mem:BLK (match_dup 0))
	(unspec:BLK [(mem:BLK (match_dup 0))] UNSPEC_MF))]
  ""
{
  operands[0] = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (DImode));
  MEM_VOLATILE_P (operands[0]) = 1;
})

(define_insn "*mf_internal"
  [(set (match_operand:BLK 0 "" "")
	(unspec:BLK [(match_operand:BLK 1 "" "")] UNSPEC_MF))]
  ""
  "mf"
  [(set_attr "itanium_class" "syst_m")])

(define_expand "sync_add<mode>"
  [(match_operand:I48MODE 0 "gr_register_operand" "")
   (match_operand:I48MODE 1 "memory_operand" "")
   (match_operand:I48MODE 2 "general_operand" "")]
  ""
{
  if (!fetchadd_operand (operands[2], <MODE>mode))
    FAIL;
  emit_insn (gen_memory_barrier ());
  emit_insn (gen_fetchadd_acq_<mode> (operands[0], operands[1], operands[2]));
  DONE;
})

(define_expand "sync_old_add<mode>"
  [(match_operand:I48MODE 0 "gr_register_operand" "")
   (match_operand:I48MODE 1 "memory_operand" "")
   (match_operand:I48MODE 2 "general_operand" "")]
  ""
{
  if (!fetchadd_operand (operands[2], <MODE>mode))
    FAIL;
  emit_insn (gen_memory_barrier ());
  emit_insn (gen_fetchadd_acq_<mode> (operands[0], operands[1], operands[2]));
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

(define_expand "sync_compare_and_swap<mode>"
  [(match_operand:I48MODE 0 "gr_register_operand" "")
   (match_operand:I48MODE 1 "memory_operand" "")
   (match_operand:I48MODE 2 "gr_register_operand" "")
   (match_operand:I48MODE 3 "gr_register_operand" "")]
  ""
{
  rtx ccv = gen_rtx_REG (DImode, AR_CCV_REGNUM);
  convert_move (ccv, operands[2], 1);
  emit_insn (gen_memory_barrier ());
  emit_insn (gen_cmpxchg_acq_<mode> (operands[0], operands[1],
				     ccv, operands[3]));
  DONE;
})

(define_insn "cmpxchg_acq_<mode>"
  [(set (match_operand:I48MODE 0 "gr_register_operand" "=r")
	(match_operand:I48MODE 1 "not_postinc_memory_operand" "+S"))
   (set (match_dup 1)
        (unspec:I48MODE [(match_dup 1)
			 (match_operand:DI 2 "ar_ccv_reg_operand" "")
			 (match_operand:I48MODE 3 "gr_register_operand" "r")]
			UNSPEC_CMPXCHG_ACQ))]
  ""
  "cmpxchg<modesuffix>.acq %0 = %1, %3, %2"
  [(set_attr "itanium_class" "sem")])

(define_insn "sync_lock_test_and_set<mode>"
  [(set (match_operand:I48MODE 0 "gr_register_operand" "=r")
        (match_operand:I48MODE 1 "not_postinc_memory_operand" "+S"))
   (set (match_dup 1)
        (match_operand:I48MODE 2 "gr_register_operand" "r"))]
  ""
  "xchg<modesuffix> %0 = %1, %2"
  [(set_attr "itanium_class" "sem")])

(define_expand "sync_lock_release<mode>"
  [(set (match_operand:I48MODE 0 "memory_operand" "")
        (const_int 0))]
  ""
{
  gcc_assert (MEM_VOLATILE_P (operands[0]));
})
