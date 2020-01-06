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
    (VUNSPEC_LOCK 100)		; sync lock and test
    (VUNSPEC_UNLOCK 101)	; sync lock release
  ]
)

(define_expand "ffssi2"
  [(set (match_operand:SI 0 "nonimmediate_operand" "")
	(ffs:SI (match_operand:SI 1 "general_operand" "")))]
  ""
  "
{
  rtx label = gen_label_rtx ();
  emit_insn (gen_ffssi2_internal (operands[0], operands[1]));
  emit_jump_insn (gen_bne (label));
  emit_insn (gen_negsi2 (operands[0], const1_rtx));
  emit_label (label);
  emit_insn (gen_addsi3 (operands[0], operands[0], const1_rtx));
  DONE;
}")

(define_insn "ffssi2_internal"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=rQ")
	(ffs:SI (match_operand:SI 1 "general_operand" "nrQt")))
   (set (cc0) (match_dup 0))]
  ""
  "ffs $0,$32,%1,%0")

(define_expand "sync_lock_test_and_set<mode>"
  [(set (match_operand:VAXint 0 "nonimmediate_operand" "=&g")
	(unspec:VAXint [(match_operand:VAXint 1 "memory_operand" "+m")
		    (match_operand:VAXint 2 "const_int_operand" "n")
		   ] VUNSPEC_LOCK))]
  ""
  "
{
  rtx label;

  if (operands[2] != const1_rtx)
    FAIL;

  label = gen_label_rtx ();
  emit_move_insn (operands[0], const1_rtx);
  emit_jump_insn (gen_jbbssi<mode> (operands[1], const0_rtx, label, operands[1]));
  emit_move_insn (operands[0], const0_rtx);
  emit_label (label);
  DONE;
}")

(define_insn "jbbssiqi"
  [(parallel
    [(set (pc)
	  (if_then_else
	    (ne (zero_extract:SI (match_operand:QI 0 "memory_operand" "g")
				 (const_int 1)
				 (match_operand:SI 1 "general_operand" "nrm"))
		(const_int 0))
	    (label_ref (match_operand 2 "" ""))
	    (pc)))
     (set (zero_extract:SI (match_operand:QI 3 "memory_operand" "+0")
			   (const_int 1)
			   (match_dup 1))
	  (const_int 1))])]
  ""
  "jbssi %1,%0,%l2")

(define_insn "jbbssihi"
  [(parallel
    [(set (pc)
	  (if_then_else
	    (ne (zero_extract:SI (match_operand:HI 0 "memory_operand" "Q")
				 (const_int 1)
				 (match_operand:SI 1 "general_operand" "nrm"))
		(const_int 0))
	    (label_ref (match_operand 2 "" ""))
	    (pc)))
     (set (zero_extract:SI (match_operand:HI 3 "memory_operand" "+0")
			   (const_int 1)
			   (match_dup 1))
	  (const_int 1))])]
  ""
  "jbssi %1,%0,%l2")

(define_insn "jbbssisi"
  [(parallel
    [(set (pc)
	  (if_then_else
	    (ne (zero_extract:SI (match_operand:SI 0 "memory_operand" "Q")
				 (const_int 1)
				 (match_operand:SI 1 "general_operand" "nrm"))
		(const_int 0))
	    (label_ref (match_operand 2 "" ""))
	    (pc)))
     (set (zero_extract:SI (match_operand:SI 3 "memory_operand" "+0")
			   (const_int 1)
			   (match_dup 1))
	  (const_int 1))])]
  ""
  "jbssi %1,%0,%l2")


(define_expand "sync_lock_release<mode>"
  [(set (match_operand:VAXint 0 "memory_operand" "+m")
	(unspec:VAXint [(match_operand:VAXint 1 "const_int_operand" "n")
		   ] VUNSPEC_UNLOCK))]
  ""
  "
{
  rtx label;
  if (operands[1] != const0_rtx)
    FAIL;
#if 1
  label = gen_label_rtx ();
  emit_jump_insn (gen_jbbcci<mode> (operands[0], const0_rtx, label, operands[0]));
  emit_label (label);
#else
  emit_move_insn (operands[0], const0_rtx);
#endif
  DONE;
}")

(define_insn "jbbcciqi"
  [(parallel
    [(set (pc)
	  (if_then_else
	    (eq (zero_extract:SI (match_operand:QI 0 "memory_operand" "g")
				 (const_int 1)
				 (match_operand:SI 1 "general_operand" "nrm"))
		(const_int 0))
	    (label_ref (match_operand 2 "" ""))
	    (pc)))
     (set (zero_extract:SI (match_operand:QI 3 "memory_operand" "+0")
			   (const_int 1)
			   (match_dup 1))
	  (const_int 0))])]
  ""
  "jbcci %1,%0,%l2")

(define_insn "jbbccihi"
  [(parallel
    [(set (pc)
	  (if_then_else
	    (eq (zero_extract:SI (match_operand:HI 0 "memory_operand" "Q")
				 (const_int 1)
				 (match_operand:SI 1 "general_operand" "nrm"))
		(const_int 0))
	    (label_ref (match_operand 2 "" ""))
	    (pc)))
     (set (zero_extract:SI (match_operand:HI 3 "memory_operand" "+0")
			   (const_int 1)
			   (match_dup 1))
	  (const_int 0))])]
  ""
  "jbcci %1,%0,%l2")

(define_insn "jbbccisi"
  [(parallel
    [(set (pc)
	  (if_then_else
	    (eq (zero_extract:SI (match_operand:SI 0 "memory_operand" "Q")
				 (const_int 1)
				 (match_operand:SI 1 "general_operand" "nrm"))
		(const_int 0))
	    (label_ref (match_operand 2 "" ""))
	    (pc)))
     (set (zero_extract:SI (match_operand:SI 3 "memory_operand" "+0")
			   (const_int 1)
			   (match_dup 1))
	  (const_int 0))])]
  ""
  "jbcci %1,%0,%l2")

