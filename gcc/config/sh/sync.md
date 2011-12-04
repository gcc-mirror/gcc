;; GCC machine description for SH synchronization instructions.
;; Copyright (C) 2011
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

(define_c_enum "unspec" [
  UNSPEC_ATOMIC
])
 
(define_c_enum "unspecv" [
  UNSPECV_CMPXCHG_1
  UNSPECV_CMPXCHG_2
  UNSPECV_CMPXCHG_3
])

(define_mode_iterator I124 [QI HI SI])

(define_mode_attr i124suffix [(QI "b") (HI "w") (SI "l")])
(define_mode_attr i124extend_insn [(QI "exts.b") (HI "exts.w") (SI "mov")])

(define_code_iterator FETCHOP [plus minus ior xor and])
(define_code_attr fetchop_name
  [(plus "add") (minus "sub") (ior "ior") (xor "xor") (and "and")])
(define_code_attr fetchop_insn
  [(plus "add") (minus "sub") (ior "or") (xor "xor") (and "and")])

;; Linux specific atomic patterns for the Renesas / SuperH SH CPUs.
;; Linux kernel for SH3/4 has implemented the support for software
;; atomic sequences.

(define_expand "atomic_compare_and_swap<mode>"
  [(match_operand:QI 0 "register_operand" "")		;; bool success output
   (match_operand:I124 1 "register_operand" "")		;; oldval output
   (match_operand:I124 2 "memory_operand" "")		;; memory
   (match_operand:I124 3 "register_operand" "")		;; expected input
   (match_operand:I124 4 "register_operand" "")		;; newval input
   (match_operand:SI 5 "const_int_operand" "")		;; is_weak
   (match_operand:SI 6 "const_int_operand" "")		;; success model
   (match_operand:SI 7 "const_int_operand" "")]		;; failure model
  "TARGET_SOFT_ATOMIC && !TARGET_SHMEDIA"
{
  rtx addr;

  addr = force_reg (Pmode, XEXP (operands[2], 0));
  emit_insn (gen_atomic_compare_and_swap<mode>_soft
	     (gen_lowpart (SImode, operands[1]), addr, operands[3],
	      operands[4]));
  if (<MODE>mode == QImode)
    emit_insn (gen_zero_extendqisi2 (gen_lowpart (SImode, operands[1]),
				     operands[1]));
  else if (<MODE>mode == HImode)
    emit_insn (gen_zero_extendhisi2 (gen_lowpart (SImode, operands[1]),
				     operands[1]));
  emit_insn (gen_movqi (operands[0], gen_rtx_REG (QImode, T_REG)));
  DONE;
})

(define_insn "atomic_compare_and_swap<mode>_soft"
  [(set (match_operand:SI 0 "register_operand" "=&u")
	(unspec_volatile:SI
	  [(mem:I124 (match_operand:SI 1 "register_operand" "u"))
	   (match_operand:I124 2 "register_operand" "u")
	   (match_operand:I124 3 "register_operand" "u")]
	  UNSPECV_CMPXCHG_1))
   (set (mem:I124 (match_dup 1))
	(unspec_volatile:I124 [(const_int 0)] UNSPECV_CMPXCHG_2))
   (set (reg:QI T_REG)
	(unspec_volatile:QI [(const_int 0)] UNSPECV_CMPXCHG_3))
   (clobber (match_scratch:SI 4 "=&u"))
   (clobber (reg:SI R0_REG))
   (clobber (reg:SI R1_REG))]
  "TARGET_SOFT_ATOMIC && !TARGET_SHMEDIA"
  "*
{
  return \"\\
mova\\t1f, r0\\n\\
\\t<i124extend_insn>\\t%2, %4\\n\\
\\tmov\\tr15, r1\\n\\
\\tmov\\t#(0f-1f), r15\\n\\
0:\\tmov.<i124suffix>\\t@%1, %0\\n\\
\\tcmp/eq\\t%0, %4\\n\\
\\tbf\\t1f\\n\\
\\tmov.<i124suffix>\\t%3, @%1\\n\\
\\t.align\\t2\\n\\
1:\\tmov\tr1, r15\";
}"
  [(set_attr "length" "20")])

(define_expand "atomic_fetch_<fetchop_name><mode>"
  [(set (match_operand:I124 0 "register_operand" "")
	(match_operand:I124 1 "memory_operand" ""))
   (set (match_dup 1)
	(unspec:I124
	  [(FETCHOP:I124 (match_dup 1)
	     (match_operand:I124 2 "register_operand" ""))]
	  UNSPEC_ATOMIC))
   (match_operand:SI 3 "const_int_operand" "")]
  "TARGET_SOFT_ATOMIC && !TARGET_SHMEDIA"
{
  rtx addr;

  addr = force_reg (Pmode, XEXP (operands[1], 0));
  emit_insn (gen_atomic_fetch_<fetchop_name><mode>_soft
	     (operands[0], addr, operands[2]));
  if (<MODE>mode == QImode)
    emit_insn (gen_zero_extendqisi2 (gen_lowpart (SImode, operands[0]),
				     operands[0]));
  else if (<MODE>mode == HImode)
    emit_insn (gen_zero_extendhisi2 (gen_lowpart (SImode, operands[0]),
				     operands[0]));
  DONE;
})

(define_insn "atomic_fetch_<fetchop_name><mode>_soft"
  [(set (match_operand:I124 0 "register_operand" "=&u")
	(mem:I124 (match_operand:SI 1 "register_operand" "u")))
   (set (mem:I124 (match_dup 1))
	(unspec:I124
	  [(FETCHOP:I124 (mem:I124 (match_dup 1))
	     (match_operand:I124 2 "register_operand" "u"))]
	  UNSPEC_ATOMIC))
   (clobber (match_scratch:I124 3 "=&u"))
   (clobber (reg:SI R0_REG))
   (clobber (reg:SI R1_REG))]
  "TARGET_SOFT_ATOMIC && !TARGET_SHMEDIA"
  "*
{
  return \"\\
mova\\t1f, r0\\n\\
\\tmov\\tr15, r1\\n\\
\\tmov\\t#(0f-1f), r15\\n\\
0:\\tmov.<i124suffix>\\t@%1, %0\\n\\
\\tmov\\t%0, %3\\n\\
\\t<fetchop_insn>\\t%2, %3\\n\\
\\tmov.<i124suffix>\\t%3, @%1\\n\\
\\t.align\\t2\\n\\
1:\\tmov\tr1, r15\";
}"
  [(set_attr "length" "18")])

(define_expand "atomic_fetch_nand<mode>"
  [(set (match_operand:I124 0 "register_operand" "")
	(match_operand:I124 1 "memory_operand" ""))
   (set (match_dup 1)
	(unspec:I124
	  [(not:I124 (and:I124 (match_dup 1)
	     (match_operand:I124 2 "register_operand" "")))]
	  UNSPEC_ATOMIC))
   (match_operand:SI 3 "const_int_operand" "")]
  "TARGET_SOFT_ATOMIC && !TARGET_SHMEDIA"
{
  rtx addr;

  addr = force_reg (Pmode, XEXP (operands[1], 0));
  emit_insn (gen_atomic_fetch_nand<mode>_soft
	     (operands[0], addr, operands[2]));
  if (<MODE>mode == QImode)
    emit_insn (gen_zero_extendqisi2 (gen_lowpart (SImode, operands[0]),
				     operands[0]));
  else if (<MODE>mode == HImode)
    emit_insn (gen_zero_extendhisi2 (gen_lowpart (SImode, operands[0]),
				     operands[0]));
  DONE;
})

(define_insn "atomic_fetch_nand<mode>_soft"
  [(set (match_operand:I124 0 "register_operand" "=&u")
	(mem:I124 (match_operand:SI 1 "register_operand" "u")))
   (set (mem:I124 (match_dup 1))
	(unspec:I124
	  [(not:I124 (and:I124 (mem:I124 (match_dup 1))
	     (match_operand:I124 2 "register_operand" "u")))]
	  UNSPEC_ATOMIC))
   (clobber (match_scratch:I124 3 "=&u"))
   (clobber (reg:SI R0_REG))
   (clobber (reg:SI R1_REG))]
  "TARGET_SOFT_ATOMIC && !TARGET_SHMEDIA"
  "*
{
  return \"\\
mova\\t1f, r0\\n\\
\\tmov\\tr15, r1\\n\\
\\tmov\\t#(0f-1f), r15\\n\\
0:\\tmov.<i124suffix>\\t@%1, %0\\n\\
\\tmov\\t%2, %3\\n\\
\\tand\\t%0, %3\\n\\
\\tnot\\t%3, %3\\n\\
\\tmov.<i124suffix>\\t%3, @%1\\n\\
\\t.align\\t2\\n\\
1:\\tmov\tr1, r15\";
}"
  [(set_attr "length" "20")])

(define_expand "atomic_<fetchop_name>_fetch<mode>"
  [(set (match_operand:I124 0 "register_operand" "")
	(FETCHOP:I124
	  (match_operand:I124 1 "memory_operand" "")
	  (match_operand:I124 2 "register_operand" "")))
   (set (match_dup 1)
	(unspec:I124
	  [(FETCHOP:I124 (match_dup 1) (match_dup 2))]
	  UNSPEC_ATOMIC))
   (match_operand:SI 3 "const_int_operand" "")]
  "TARGET_SOFT_ATOMIC && !TARGET_SHMEDIA"
{
  rtx addr;

  addr = force_reg (Pmode, XEXP (operands[1], 0));
  emit_insn (gen_atomic_<fetchop_name>_fetch<mode>_soft
	     (operands[0], addr, operands[2]));
  if (<MODE>mode == QImode)
    emit_insn (gen_zero_extendqisi2 (gen_lowpart (SImode, operands[0]),
				     operands[0]));
  else if (<MODE>mode == HImode)
    emit_insn (gen_zero_extendhisi2 (gen_lowpart (SImode, operands[0]),
				     operands[0]));
  DONE;
})

(define_insn "atomic_<fetchop_name>_fetch<mode>_soft"
  [(set (match_operand:I124 0 "register_operand" "=&u")
	(FETCHOP:I124
	  (mem:I124 (match_operand:SI 1 "register_operand" "u"))
	  (match_operand:I124 2 "register_operand" "u")))
   (set (mem:I124 (match_dup 1))
	(unspec:I124
	  [(FETCHOP:I124 (mem:I124 (match_dup 1)) (match_dup 2))]
	  UNSPEC_ATOMIC))
   (clobber (reg:SI R0_REG))
   (clobber (reg:SI R1_REG))]
  "TARGET_SOFT_ATOMIC && !TARGET_SHMEDIA"
  "*
{
  return \"\\
mova\\t1f, r0\\n\\
\\tmov\\tr15, r1\\n\\
\\tmov\\t#(0f-1f), r15\\n\\
0:\\tmov.<i124suffix>\\t@%1, %0\\n\\
\\t<fetchop_insn>\\t%2, %0\\n\\
\\tmov.<i124suffix>\\t%0, @%1\\n\\
\\t.align\\t2\\n\\
1:\\tmov\tr1, r15\";
}"
  [(set_attr "length" "16")])

(define_expand "atomic_nand_fetch<mode>"
  [(set (match_operand:I124 0 "register_operand" "")
	(not:I124 (and:I124
	  (match_operand:I124 1 "memory_operand" "")
	  (match_operand:I124 2 "register_operand" ""))))
   (set (match_dup 1)
	(unspec:I124
	  [(not:I124 (and:I124 (match_dup 1) (match_dup 2)))]
	  UNSPEC_ATOMIC))
   (match_operand:SI 3 "const_int_operand" "")]
  "TARGET_SOFT_ATOMIC && !TARGET_SHMEDIA"
{
  rtx addr;

  addr = force_reg (Pmode, XEXP (operands[1], 0));
  emit_insn (gen_atomic_nand_fetch<mode>_soft
	     (operands[0], addr, operands[2]));
  if (<MODE>mode == QImode)
    emit_insn (gen_zero_extendqisi2 (gen_lowpart (SImode, operands[0]),
				     operands[0]));
  else if (<MODE>mode == HImode)
    emit_insn (gen_zero_extendhisi2 (gen_lowpart (SImode, operands[0]),
				     operands[0]));
  DONE;
})

(define_insn "atomic_nand_fetch<mode>_soft"
  [(set (match_operand:I124 0 "register_operand" "=&u")
	(not:I124 (and:I124
	  (mem:I124 (match_operand:SI 1 "register_operand" "u"))
	  (match_operand:I124 2 "register_operand" "u"))))
   (set (mem:I124 (match_dup 1))
	(unspec:I124
	  [(not:I124 (and:I124 (mem:I124 (match_dup 1)) (match_dup 2)))]
	  UNSPEC_ATOMIC))
   (clobber (reg:SI R0_REG))
   (clobber (reg:SI R1_REG))]
  "TARGET_SOFT_ATOMIC && !TARGET_SHMEDIA"
  "*
{
  return \"\\
mova\\t1f, r0\\n\\
\\tmov\\tr15, r1\\n\\
\\tmov\\t#(0f-1f), r15\\n\\
0:\\tmov.<i124suffix>\\t@%1, %0\\n\\
\\tand\\t%2, %0\\n\\
\\tnot\\t%0, %0\\n\\
\\tmov.<i124suffix>\\t%0, @%1\\n\\
\\t.align\\t2\\n\\
1:\\tmov\tr1, r15\";
}"
  [(set_attr "length" "18")])
