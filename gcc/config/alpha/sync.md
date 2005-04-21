;; GCC machine description for Alpha synchronization instructions.
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
(define_mode_attr modesuffix [(SI "l") (DI "q")])

(define_code_macro FETCHOP [plus minus ior xor and])
(define_code_attr fetchop_name
  [(plus "add") (minus "sub") (ior "ior") (xor "xor") (and "and")])
(define_code_attr fetchop_pred
  [(plus "add_operand") (minus "reg_or_8bit_operand")
   (ior "or_operand") (xor "or_operand") (and "and_operand")])
(define_code_attr fetchop_constr
  [(plus "rKL") (minus "rI") (ior "rIN") (xor "rIN") (and "riNHM")])


(define_expand "memory_barrier"
  [(set (mem:BLK (match_dup 0))
	(unspec_volatile:BLK [(mem:BLK (match_dup 0))] UNSPECV_MB))]
  ""
{
  operands[0] = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (DImode));
  MEM_VOLATILE_P (operands[0]) = 1;
})

(define_insn "*mb_internal"
  [(set (match_operand:BLK 0 "" "")
	(unspec_volatile:BLK [(match_operand:BLK 1 "" "")] UNSPECV_MB))]
  ""
  "mb"
  [(set_attr "type" "mb")])

(define_insn "load_locked_<mode>"
  [(set (match_operand:I48MODE 0 "register_operand" "=r")
	(unspec_volatile:I48MODE
	  [(match_operand:I48MODE 1 "memory_operand" "m")]
	  UNSPECV_LL))]
  ""
  "ld<modesuffix>_l %0,%1"
  [(set_attr "type" "ld_l")])

(define_insn "store_conditional_<mode>"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec_volatile:DI [(const_int 0)] UNSPECV_SC))
   (set (match_operand:I48MODE 1 "memory_operand" "=m")
	(match_operand:I48MODE 2 "reg_or_0_operand" "0"))]
  ""
  "st<modesuffix>_c %0,%1"
  [(set_attr "type" "st_c")])

;; The Alpha Architecture Handbook says that it is UNPREDICTABLE whether
;; the lock is cleared by a TAKEN branch.  If we were to honor that, it
;; would mean that we could not expand a ll/sc sequence until after the
;; final basic-block reordering pass.  Fortunately, it appears that no
;; Alpha implementation ever built actually clears the lock on branches,
;; taken or not.

(define_insn_and_split "sync_<fetchop_name><mode>"
  [(set (match_operand:I48MODE 0 "memory_operand" "+m")
	(unspec_volatile:I48MODE
	  [(FETCHOP:I48MODE (match_dup 0)
	     (match_operand:I48MODE 1 "<fetchop_pred>" "<fetchop_constr>"))]
	  UNSPECV_ATOMIC))
   (clobber (match_scratch:I48MODE 2 "=&r"))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
{
  alpha_split_atomic_op (<CODE>, operands[0], operands[1],
			 NULL, NULL, operands[2]);
  DONE;
}
  [(set_attr "type" "multi")])

(define_insn_and_split "sync_nand<mode>"
  [(set (match_operand:I48MODE 0 "memory_operand" "+m")
	(unspec_volatile:I48MODE
	  [(and:I48MODE (not:I48MODE (match_dup 0))
	     (match_operand:I48MODE 1 "register_operand" "r"))]
	  UNSPECV_ATOMIC))
   (clobber (match_scratch:I48MODE 2 "=&r"))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
{
  alpha_split_atomic_op (NOT, operands[0], operands[1],
			 NULL, NULL, operands[2]);
  DONE;
}
  [(set_attr "type" "multi")])

(define_insn_and_split "sync_old_<fetchop_name><mode>"
  [(set (match_operand:I48MODE 0 "register_operand" "=&r")
	(match_operand:I48MODE 1 "memory_operand" "+m"))
   (set (match_dup 1)
	(unspec_volatile:I48MODE
	  [(FETCHOP:I48MODE (match_dup 1)
	     (match_operand:I48MODE 2 "<fetchop_pred>" "<fetchop_constr>"))]
	  UNSPECV_ATOMIC))
   (clobber (match_scratch:I48MODE 3 "=&r"))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
{
  alpha_split_atomic_op (<CODE>, operands[1], operands[2],
			 operands[0], NULL, operands[3]);
  DONE;
}
  [(set_attr "type" "multi")])

(define_insn_and_split "sync_old_nand<mode>"
  [(set (match_operand:I48MODE 0 "register_operand" "=&r")
	(match_operand:I48MODE 1 "memory_operand" "+m"))
   (set (match_dup 1)
	(unspec_volatile:I48MODE
	  [(and:I48MODE (not:I48MODE (match_dup 1))
	     (match_operand:I48MODE 2 "register_operand" "r"))]
	  UNSPECV_ATOMIC))
   (clobber (match_scratch:I48MODE 3 "=&r"))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
{
  alpha_split_atomic_op (NOT, operands[1], operands[2],
			 operands[0], NULL, operands[3]);
  DONE;
}
  [(set_attr "type" "multi")])

(define_insn_and_split "sync_new_<fetchop_name><mode>"
  [(set (match_operand:I48MODE 0 "register_operand" "=&r")
	(FETCHOP:I48MODE 
	  (match_operand:I48MODE 1 "memory_operand" "+m")
	  (match_operand:I48MODE 2 "<fetchop_pred>" "<fetchop_constr>")))
   (set (match_dup 1)
	(unspec_volatile:I48MODE
	  [(FETCHOP:I48MODE (match_dup 1) (match_dup 2))]
	  UNSPECV_ATOMIC))
   (clobber (match_scratch:I48MODE 3 "=&r"))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
{
  alpha_split_atomic_op (<CODE>, operands[1], operands[2],
			 NULL, operands[0], operands[3]);
  DONE;
}
  [(set_attr "type" "multi")])

(define_insn_and_split "sync_new_nand<mode>"
  [(set (match_operand:I48MODE 0 "register_operand" "=&r")
	(and:I48MODE 
	  (not:I48MODE (match_operand:I48MODE 1 "memory_operand" "+m"))
	  (match_operand:I48MODE 2 "register_operand" "r")))
   (set (match_dup 1)
	(unspec_volatile:I48MODE
	  [(and:I48MODE (not:I48MODE (match_dup 1)) (match_dup 2))]
	  UNSPECV_ATOMIC))
   (clobber (match_scratch:I48MODE 3 "=&r"))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
{
  alpha_split_atomic_op (NOT, operands[1], operands[2],
			 NULL, operands[0], operands[3]);
  DONE;
}
  [(set_attr "type" "multi")])

(define_expand "sync_compare_and_swap<mode>"
  [(parallel
     [(set (match_operand:I48MODE 0 "register_operand" "")
	   (match_operand:I48MODE 1 "memory_operand" ""))
      (set (match_dup 1)
	   (unspec_volatile:I48MODE
	     [(match_operand:I48MODE 2 "reg_or_8bit_operand" "")
	      (match_operand:I48MODE 3 "add_operand" "rKL")]
	     UNSPECV_CMPXCHG))
      (clobber (match_scratch:I48MODE 4 "=&r"))])]
  ""
{
  if (<MODE>mode == SImode)
    operands[2] = convert_modes (DImode, SImode, operands[2], 0);
})

(define_insn_and_split "*sync_compare_and_swap<mode>"
  [(set (match_operand:I48MODE 0 "register_operand" "=&r")
	(match_operand:I48MODE 1 "memory_operand" "+m"))
   (set (match_dup 1)
	(unspec_volatile:I48MODE
	  [(match_operand:DI 2 "reg_or_8bit_operand" "rI")
	   (match_operand:I48MODE 3 "add_operand" "rKL")]
	  UNSPECV_CMPXCHG))
   (clobber (match_scratch:I48MODE 4 "=&r"))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
{
  rtx retval, mem, oldval, newval, scratch;
  rtx cond, label1, label2, x;
  rtx very_unlikely = GEN_INT (REG_BR_PROB_BASE / 100 - 1);

  retval = operands[0];
  mem = operands[1];
  oldval = operands[2];
  newval = operands[3];
  scratch = operands[4];
  cond = gen_lowpart (DImode, scratch);

  emit_insn (gen_memory_barrier ());

  label1 = gen_rtx_LABEL_REF (DImode, gen_label_rtx ());
  label2 = gen_rtx_LABEL_REF (DImode, gen_label_rtx ());
  emit_label (XEXP (label1, 0));

  emit_insn (gen_load_locked_<mode> (retval, mem));

  x = gen_lowpart (DImode, retval);
  if (oldval == const0_rtx)
    x = gen_rtx_NE (DImode, x, const0_rtx);
  else
    {
      x = gen_rtx_EQ (DImode, x, oldval);
      emit_insn (gen_rtx_SET (VOIDmode, cond, x));
      x = gen_rtx_EQ (DImode, cond, const0_rtx);
    }
  x = gen_rtx_IF_THEN_ELSE (VOIDmode, x, label2, pc_rtx);
  x = emit_jump_insn (gen_rtx_SET (VOIDmode, pc_rtx, x));
  REG_NOTES (x) = gen_rtx_EXPR_LIST (REG_BR_PROB, very_unlikely, NULL_RTX);
    
  emit_move_insn (scratch, newval);

  emit_insn (gen_store_conditional_<mode> (cond, mem, scratch));

  x = gen_rtx_EQ (DImode, cond, const0_rtx);
  x = gen_rtx_IF_THEN_ELSE (VOIDmode, x, label1, pc_rtx);
  x = emit_jump_insn (gen_rtx_SET (VOIDmode, pc_rtx, x));
  REG_NOTES (x) = gen_rtx_EXPR_LIST (REG_BR_PROB, very_unlikely, NULL_RTX);

  emit_insn (gen_memory_barrier ());
  emit_label (XEXP (label2, 0));
  DONE;
}
  [(set_attr "type" "multi")])

(define_insn_and_split "sync_lock_test_and_set<mode>"
  [(set (match_operand:I48MODE 0 "register_operand" "=&r")
	(match_operand:I48MODE 1 "memory_operand" "+m"))
   (set (match_dup 1)
	(unspec_volatile:I48MODE
	  [(match_operand:I48MODE 2 "add_operand" "rKL")]
	  UNSPECV_XCHG))
   (clobber (match_scratch:I48MODE 3 "=&r"))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
{
  rtx retval, mem, val, scratch;
  rtx cond, label1, x;
  rtx very_unlikely = GEN_INT (REG_BR_PROB_BASE / 100 - 1);

  retval = operands[0];
  mem = operands[1];
  val = operands[2];
  scratch = operands[3];
  cond = gen_lowpart (DImode, scratch);

  emit_insn (gen_memory_barrier ());

  label1 = gen_rtx_LABEL_REF (DImode, gen_label_rtx ());
  emit_label (XEXP (label1, 0));

  emit_insn (gen_load_locked_<mode> (retval, mem));

  emit_move_insn (scratch, val);
  
  emit_insn (gen_store_conditional_<mode> (cond, mem, scratch));

  x = gen_rtx_EQ (DImode, cond, const0_rtx);
  x = gen_rtx_IF_THEN_ELSE (VOIDmode, x, label1, pc_rtx);
  x = emit_jump_insn (gen_rtx_SET (VOIDmode, pc_rtx, x));
  REG_NOTES (x) = gen_rtx_EXPR_LIST (REG_BR_PROB, very_unlikely, NULL_RTX);

  DONE;
}
  [(set_attr "type" "multi")])
