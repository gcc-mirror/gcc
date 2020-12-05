;; builtin definitions for DEC VAX.
;; Copyright (C) 2007-2020 Free Software Foundation, Inc.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; GCC is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

(define_constants
  [
    (VUNSPEC_LOCK 100)		; sync lock operations
  ]
)

(define_mode_attr width [(QI "8") (HI "16") (SI "32")])
(define_mode_attr bb_mem [(QI "m") (HI "Q") (SI "Q")])

(define_int_iterator bit [0 1])
(define_int_attr ccss [(0 "cc") (1 "ss")])

(define_code_iterator any_extend [sign_extend zero_extend])

(define_expand "ffs<mode>2"
  [(set (match_operand:SI 0 "nonimmediate_operand" "")
	(ffs:SI (match_operand:VAXint 1 "general_operand" "")))]
  ""
  "
{
  rtx label = gen_label_rtx ();
  rtx label_ref = gen_rtx_LABEL_REF (VOIDmode, label);
  rtx cond = gen_rtx_NE (VOIDmode, cc0_rtx, const0_rtx);
  rtx target = gen_rtx_IF_THEN_ELSE (VOIDmode, cond, label_ref, pc_rtx);

  emit_insn (gen_ctz<mode>2 (operands[0], operands[1]));
  emit_jump_insn (gen_rtx_SET (pc_rtx, target));
  emit_insn (gen_neg<mode>2 (operands[0], const1_rtx));
  emit_label (label);
  emit_insn (gen_add<mode>3 (operands[0], operands[0], const1_rtx));
  DONE;
}")

(define_insn "ctz<mode>2"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=rQ")
	(ctz:SI (match_operand:VAXint 1 "general_operand" "nrQT")))
   (set (cc0)
	(compare (match_dup 1)
		 (const_int 0)))]
  ""
  "ffs $0,$<width>,%1,%0")

;; Our FFS hardware instruction supports any field width,
;; so handle narrower inputs directly as well.
(define_peephole2
  [(set (match_operand:SI 0 "register_operand")
        (any_extend:SI (match_operand:VAXintQH 1 "general_operand")))
   (parallel
     [(set (match_operand:SI 2 "nonimmediate_operand")
	   (ctz:SI (match_dup 0)))
      (set (cc0)
	   (compare (match_dup 2)
		    (const_int 0)))])]
  "rtx_equal_p (operands[0], operands[2]) || peep2_reg_dead_p (2, operands[0])"
  [(parallel
     [(set (match_dup 2)
	   (ctz:SI (match_dup 1)))
      (set (cc0)
	   (compare (match_dup 1)
		    (const_int 0)))])]
  "")

(define_expand "sync_lock_test_and_set<mode>"
  [(match_operand:VAXint 0 "nonimmediate_operand" "=&g")
   (match_operand:VAXint 1 "memory_operand" "+m")
   (match_operand:VAXint 2 "const_int_operand" "n")]
  ""
  "
{
  rtx label;

  if (operands[2] != const1_rtx)
    FAIL;

  label = gen_label_rtx ();
  emit_move_insn (operands[0], const1_rtx);
  emit_jump_insn (gen_jbbssi<mode> (operands[1], const0_rtx, label,
				    operands[1]));
  emit_move_insn (operands[0], const0_rtx);
  emit_label (label);
  DONE;
}")

(define_expand "sync_lock_release<mode>"
  [(match_operand:VAXint 0 "memory_operand" "+m")
   (match_operand:VAXint 1 "const_int_operand" "n")]
  ""
  "
{
  rtx label;

  if (operands[1] != const0_rtx)
    FAIL;

  label = gen_label_rtx ();
  emit_jump_insn (gen_jbbcci<mode> (operands[0], const0_rtx, label,
				    operands[0]));
  emit_label (label);
  DONE;
}")

(define_insn "jbb<ccss>i<mode>"
  [(unspec_volatile
    [(set (pc)
	  (if_then_else
	    (eq (zero_extract:SI
		  (match_operand:VAXint 0 "any_memory_operand" "<bb_mem>")
		  (const_int 1)
		  (match_operand:SI 1 "general_operand" "nrmT"))
		(const_int bit))
	    (label_ref (match_operand 2 "" ""))
	    (pc)))
     (set (zero_extract:SI (match_operand:VAXint 3 "any_memory_operand" "+0")
			   (const_int 1)
			   (match_dup 1))
	  (const_int bit))]
    VUNSPEC_LOCK)]
  ""
  "jb<ccss>i %1,%0,%l2")
