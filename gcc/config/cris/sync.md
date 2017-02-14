;; GCC machine description for CRIS atomic memory sequences.
;; Copyright (C) 2012-2017 Free Software Foundation, Inc.
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

;; The CRIS atomic support yields code in three flavors, depending on
;; the CPU for which code is generated:
;;
;; - Plain old CRIS v0 (..v8)
;; - CRIS v10 (as used in ETRAX 100 LX)
;; - CRIS v32 (as used in ETRAX FS)
;;
;; The last two alternatives are similar, of LL/SC type.  They may
;; fail for other reasons; an exception, a cache miss or a bus request
;; from other parts of the system.  The difference between them is
;; just in what condition-codes are used to track LL and success or
;; failure for the store.  See the chapter on integral read-write
;; operations, chapter 1.13 in "ETRAX 100LX Programmers Manual",
;; <http://www.axis.com/files/tech_notes/etrax_100lx_prog_man-050519.pdf>
;; and chapter 2.1 in "ETRAX FS Designer's reference",
;; <http://www.axis.com/files/manuals/etrax_fs_des_ref-070821.pdf>.
;; Note that the datum being stored has to be contained fully within a
;; cache-line to be integral.  A failure to store the data integrally
;; will be flagged, but the store may still have happened in part,
;; which translates most usefully into the data having to be
;; "naturally aligned" to work.  Natural alignment is verified in the
;; generated code and will by default cause for unaligned pointers a
;; "break 8" to be executed or optionally a call to abort().  Beware
;; that options -m16bit and -m8bit may cause data to be unaligned
;; where it was otherwise aligned.  Data has a better chance of being
;; aligned if it is declared with e.g. __attribute__ ((__align__ (4))).
;;
;; The "plain old v0..v8 flavor" just assumes there's a single CPU in
;; the system, that no other parts of the system have access to memory
;; used for atomic accesses and since there's no user mode without
;; access to interrupt flags (another assumption), it just turns off
;; interrupts while doing the access.  Here, alignment is neither
;; required nor asserted.

(define_c_enum ""
  [
   CRIS_UNSPEC_ATOMIC_OP
   CRIS_UNSPEC_ATOMIC_SWAP_MEM
   CRIS_UNSPEC_ATOMIC_SWAP_BOOL
  ])

(define_constants [(CRIS_CCR_INTERRUPT_BIT 5)])

;; We use "mult" as a placeholder for "nand" (which does not have a
;; separate binary rtx operation) so we can use an iterator in the
;; define_expand and define_insn and avoid having a separate
;; mostly-identical copy.  You will see the "mult" operator in rtl
;; dumps, but it shouldn't matter as its use has one of its operands
;; inside an unspec_volatile.

(define_code_iterator atomic_op [plus minus ior and xor mult])

(define_code_attr atomic_op_name
 [(plus "add") (minus "sub") (and "and") (ior "or") (xor "xor") (mult "nand")])

;; The operator nonatomic-operand can be memory, constant or register
;; for all but xor.  We can't use memory or addressing modes with
;; side-effects though, so just use registers and literal constants.
(define_code_attr atomic_op_op_cnstr
 [(plus "ri") (minus "ri") (and "ri") (ior "ri") (xor "r") (mult "ri")])

(define_code_attr atomic_op_op_pred
 [(plus "nonmemory_operand") (minus "nonmemory_operand")
  (and "nonmemory_operand") (ior "nonmemory_operand")
  (xor "register_operand") (mult "nonmemory_operand")])

;; Pairs of these are used to insert the "not" after the "and" for nand.
(define_code_attr atomic_op_mnem_pre_op2 ;; Upper-case only to simplify testing.
 [(plus "%P2") (minus "Sub.d %2") (and "And%q2 %2") (ior "Or%q2 %2") (xor "Xor %2")
  (mult "aNd%q2 %2")])

(define_code_attr atomic_op_mnem_post_op3
 [(plus "") (minus "") (and "") (ior "") (xor "") (mult "not %3\;")])

;; For SImode, emit "q" for operands -31..31.
(define_mode_attr qm3 [(SI "%q3") (HI ".w") (QI ".b")])

(define_expand "atomic_fetch_<atomic_op_name><mode>"
  [(match_operand:BWD 0 "register_operand")
   (match_operand:BWD 1 "memory_operand")
   (match_operand:BWD 2 "<atomic_op_op_pred>")
   (match_operand 3)
   (atomic_op:BWD (match_dup 0) (match_dup 1))]
  "<MODE>mode == QImode || !TARGET_ATOMICS_MAY_CALL_LIBFUNCS"
{
  enum memmodel mmodel = (enum memmodel) INTVAL (operands[3]);

  if (<MODE>mode != QImode && TARGET_TRAP_UNALIGNED_ATOMIC)
    cris_emit_trap_for_misalignment (operands[1]);

  if (need_atomic_barrier_p (mmodel, true))
    expand_mem_thread_fence (mmodel);

  emit_insn (gen_cris_atomic_fetch_<atomic_op_name><mode>_1 (operands[0],
							     operands[1],
							     operands[2]));
  if (need_atomic_barrier_p (mmodel, false))
    expand_mem_thread_fence (mmodel);

  DONE;
})

(define_insn "cris_atomic_fetch_<atomic_op_name><mode>_1"
  [(set (match_operand:BWD 1 "memory_operand" "+Q")
	(atomic_op:BWD
	 (unspec_volatile:BWD [(match_dup 1)] CRIS_UNSPEC_ATOMIC_OP)
	 ;; FIXME: improve constants more for plus, minus, and, ior.
	 ;; FIXME: handle memory operands without side-effects.
	 (match_operand:BWD 2 "<atomic_op_op_pred>" "<atomic_op_op_cnstr>")))
   (set (match_operand:BWD 0 "register_operand" "=&r")
	(match_dup 1))
   (clobber (match_scratch:SI 3 "=&r"))]
  "<MODE>mode == QImode || !TARGET_ATOMICS_MAY_CALL_LIBFUNCS"
{
  /* Can't be too sure; better ICE if this happens.  */
  gcc_assert (!reg_overlap_mentioned_p (operands[2], operands[1]));

  if (TARGET_V32)
    return
      "clearf p\n"
      ".Lsync.%=:\;"
      "move<m> %1,%0\;"
      "move.d %0,%3\;"
      "<atomic_op_mnem_pre_op2>,%3\;<atomic_op_mnem_post_op3>"
      "ax\;"
      "move<m> %3,%1\;"
      "bcs .Lsync.%=\;"
      "clearf p";
  else if (cris_cpu_version == 10)
    return
      "clearf\n"
      ".Lsync.%=:\;"
      "move<m> %1,%0\;"
      "move.d %0,%3\;"
      "<atomic_op_mnem_pre_op2>,%3\;<atomic_op_mnem_post_op3>"
      "ax\;"
      "move<m> %3,%1\;"
      "bwf .Lsync.%=\;"
      "clearf";
  else
    {
      /* This one is for CRIS versions without load-locked-store-conditional
	 machinery; assume single-core-non-shared-memory without user
	 mode/supervisor mode distinction, and just disable interrupts
	 while performing the operation.
	 Rather than making this pattern more complex by freeing another
	 register or stack position to save condition codes (the value
	 of the interrupt-enabled bit), we check whether interrupts were
	 enabled before we disabled them and branch to a version
	 with/without afterwards re-enabling them.  */
      rtx ops[5];

      /* We have no available macro to stringify CRIS_CCR_INTERRUPT_BIT.  */
      memcpy (ops, operands, sizeof(ops));
      ops[4] = GEN_INT (CRIS_CCR_INTERRUPT_BIT);

      output_asm_insn ("move $ccr,%3\;"
		       "di\;"
		       "move<m> %1,%0\;"
		       "btstq %4,%3",
		       ops);
      return
	"bmi .Lsync.irqon.%=\;"
	"move.d %0,%3\;"

	"<atomic_op_mnem_pre_op2>,%3\;<atomic_op_mnem_post_op3>"
	"ba .Lsync.irqoff.%=\;"
	"move<m> %3,%1\n"

	".Lsync.irqon.%=:\;"
	"<atomic_op_mnem_pre_op2>,%3\;<atomic_op_mnem_post_op3>"
	"move<m> %3,%1\;"
	"ei\n"
	".Lsync.irqoff.%=:";
    }
})

;; This pattern is more-or-less assumed to always exist if any of the
;; other atomic patterns exist (see e.g.  comment at the
;; can_compare_and_swap_p call in omp-low.c, 4.8 era).  We'd slightly
;; prefer atomic_exchange<mode> over this, but having both would be
;; redundant.
;; FIXME: handle memory without side-effects for operand[3].
(define_expand "atomic_compare_and_swap<mode>"
  [(match_operand:SI 0 "register_operand")
   (match_operand:BWD 1 "register_operand")
   (match_operand:BWD 2 "memory_operand")
   (match_operand:BWD 3 "nonmemory_operand")
   (match_operand:BWD 4 "register_operand")
   (match_operand 5)
   (match_operand 6)
   (match_operand 7)]
  "<MODE>mode == QImode || !TARGET_ATOMICS_MAY_CALL_LIBFUNCS"
{
  enum memmodel mmodel = (enum memmodel) INTVAL (operands[6]);

  if (<MODE>mode != QImode && TARGET_TRAP_UNALIGNED_ATOMIC)
    cris_emit_trap_for_misalignment (operands[2]);

  if (need_atomic_barrier_p (mmodel, true))
    expand_mem_thread_fence (mmodel);

  emit_insn (gen_cris_atomic_compare_and_swap<mode>_1 (operands[0],
						       operands[1],
						       operands[2],
						       operands[3],
						       operands[4]));
  if (need_atomic_barrier_p (mmodel, false))
    expand_mem_thread_fence (mmodel);

  DONE;
})

(define_insn "cris_atomic_compare_and_swap<mode>_1"
  [(set (match_operand:SI 0 "register_operand" "=&r")
	(unspec_volatile:SI
	 [(match_operand:BWD 2 "memory_operand" "+Q")
	  (match_operand:BWD 3 "nonmemory_operand" "ri")]
	 CRIS_UNSPEC_ATOMIC_SWAP_BOOL))
   (set (match_operand:BWD 1 "register_operand" "=&r") (match_dup 2))
   (set (match_dup 2)
	(unspec_volatile:BWD
	 [(match_dup 2)
	  (match_dup 3)
	  (match_operand:BWD 4 "register_operand" "r")]
	 CRIS_UNSPEC_ATOMIC_SWAP_MEM))]
  "<MODE>mode == QImode || !TARGET_ATOMICS_MAY_CALL_LIBFUNCS"
{
  if (TARGET_V32)
    return
      "\n.Lsync.repeat.%=:\;"
      "clearf p\;"
      "move<m> %2,%1\;"
      "cmp<qm3> %3,%1\;"
      "bne .Lsync.after.%=\;"
      "ax\;"

      "move<m> %4,%2\;"
      "bcs .Lsync.repeat.%=\n"
      ".Lsync.after.%=:\;"
      "seq %0";
  else if (cris_cpu_version == 10)
    return
      "\n.Lsync.repeat.%=:\;"
      "clearf\;"
      "move<m> %2,%1\;"
      "cmp<qm3> %3,%1\;"
      "bne .Lsync.after.%=\;"
      "ax\;"

      "move<m> %4,%2\;"
      "bwf .Lsync.repeat.%=\n"
      ".Lsync.after.%=:\;"
      "seq %0";
  else
    {
      /* This one is for CRIS versions without load-locked-store-conditional
	 machinery; assume single-core-non-shared-memory without user
	 mode/supervisor mode distinction, and just disable interrupts
	 while performing the operation.
	 Rather than making this pattern more complex by freeing another
	 register or stack position to save condition codes (the value
	 of the interrupt-enabled bit), we check whether interrupts were
	 enabled before we disabled them and branch to a version
	 with/without afterwards re-enabling them.  */
      rtx ops[4];

      /* We have no available macro to stringify CRIS_CCR_INTERRUPT_BIT.  */
      memcpy (ops, operands, sizeof(ops));
      ops[3] = GEN_INT (CRIS_CCR_INTERRUPT_BIT);

      output_asm_insn ("move $ccr,%0\;"
		       "di\;"
		       "move<m> %2,%1\;"
		       "btstq %3,%0",
		       ops);
      return
	"bmi .Lsync.irqon.%=\;"
	"nop\;"

	"cmp<qm3> %3,%1\;"
	"bne .Lsync.after.%=\;"
	"seq %0\;"
	"ba .Lsync.after.%=\;"
	"move<m> %4,%2\n"

	".Lsync.irqon.%=:\;"
	"cmp<qm3> %3,%1\;"
	"bne .Lsync.after.%=\;"
	"seq %0\;"
	"move<m> %4,%2\;"
	"ei\n"
	".Lsync.after.%=:";
    }
})
