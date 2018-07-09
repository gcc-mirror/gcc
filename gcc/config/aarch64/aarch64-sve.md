;; Machine description for AArch64 SVE.
;; Copyright (C) 2009-2016 Free Software Foundation, Inc.
;; Contributed by ARM Ltd.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; GCC is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;; Note on the handling of big-endian SVE
;; --------------------------------------
;;
;; On big-endian systems, Advanced SIMD mov<mode> patterns act in the
;; same way as movdi or movti would: the first byte of memory goes
;; into the most significant byte of the register and the last byte
;; of memory goes into the least significant byte of the register.
;; This is the most natural ordering for Advanced SIMD and matches
;; the ABI layout for 64-bit and 128-bit vector types.
;;
;; As a result, the order of bytes within the register is what GCC
;; expects for a big-endian target, and subreg offsets therefore work
;; as expected, with the first element in memory having subreg offset 0
;; and the last element in memory having the subreg offset associated
;; with a big-endian lowpart.  However, this ordering also means that
;; GCC's lane numbering does not match the architecture's numbering:
;; GCC always treats the element at the lowest address in memory
;; (subreg offset 0) as element 0, while the architecture treats
;; the least significant end of the register as element 0.
;;
;; The situation for SVE is different.  We want the layout of the
;; SVE register to be same for mov<mode> as it is for maskload<mode>:
;; logically, a mov<mode> load must be indistinguishable from a
;; maskload<mode> whose mask is all true.  We therefore need the
;; register layout to match LD1 rather than LDR.  The ABI layout of
;; SVE types also matches LD1 byte ordering rather than LDR byte ordering.
;;
;; As a result, the architecture lane numbering matches GCC's lane
;; numbering, with element 0 always being the first in memory.
;; However:
;;
;; - Applying a subreg offset to a register does not give the element
;;   that GCC expects: the first element in memory has the subreg offset
;;   associated with a big-endian lowpart while the last element in memory
;;   has subreg offset 0.  We handle this via TARGET_CAN_CHANGE_MODE_CLASS.
;;
;; - We cannot use LDR and STR for spill slots that might be accessed
;;   via subregs, since although the elements have the order GCC expects,
;;   the order of the bytes within the elements is different.  We instead
;;   access spill slots via LD1 and ST1, using secondary reloads to
;;   reserve a predicate register.


;; SVE data moves.
(define_expand "mov<mode>"
  [(set (match_operand:SVE_ALL 0 "nonimmediate_operand")
	(match_operand:SVE_ALL 1 "general_operand"))]
  "TARGET_SVE"
  {
    /* Use the predicated load and store patterns where possible.
       This is required for big-endian targets (see the comment at the
       head of the file) and increases the addressing choices for
       little-endian.  */
    if ((MEM_P (operands[0]) || MEM_P (operands[1]))
        && can_create_pseudo_p ())
      {
	aarch64_expand_sve_mem_move (operands[0], operands[1], <VPRED>mode);
	DONE;
      }

    if (CONSTANT_P (operands[1]))
      {
	aarch64_expand_mov_immediate (operands[0], operands[1],
				      gen_vec_duplicate<mode>);
	DONE;
      }

    /* Optimize subregs on big-endian targets: we can use REV[BHW]
       instead of going through memory.  */
    if (BYTES_BIG_ENDIAN
        && aarch64_maybe_expand_sve_subreg_move (operands[0], operands[1]))
      DONE;
  }
)

;; A pattern for optimizing SUBREGs that have a reinterpreting effect
;; on big-endian targets; see aarch64_maybe_expand_sve_subreg_move
;; for details.  We use a special predicate for operand 2 to reduce
;; the number of patterns.
(define_insn_and_split "*aarch64_sve_mov<mode>_subreg_be"
  [(set (match_operand:SVE_ALL 0 "aarch64_sve_nonimmediate_operand" "=w")
	(unspec:SVE_ALL
          [(match_operand:VNx16BI 1 "register_operand" "Upl")
	   (match_operand 2 "aarch64_any_register_operand" "w")]
	  UNSPEC_REV_SUBREG))]
  "TARGET_SVE && BYTES_BIG_ENDIAN"
  "#"
  "&& reload_completed"
  [(const_int 0)]
  {
    aarch64_split_sve_subreg_move (operands[0], operands[1], operands[2]);
    DONE;
  }
)

;; Unpredicated moves (little-endian).  Only allow memory operations
;; during and after RA; before RA we want the predicated load and
;; store patterns to be used instead.
(define_insn "*aarch64_sve_mov<mode>_le"
  [(set (match_operand:SVE_ALL 0 "aarch64_sve_nonimmediate_operand" "=w, Utr, w, w")
	(match_operand:SVE_ALL 1 "aarch64_sve_general_operand" "Utr, w, w, Dn"))]
  "TARGET_SVE
   && !BYTES_BIG_ENDIAN
   && ((lra_in_progress || reload_completed)
       || (register_operand (operands[0], <MODE>mode)
	   && nonmemory_operand (operands[1], <MODE>mode)))"
  "@
   ldr\t%0, %1
   str\t%1, %0
   mov\t%0.d, %1.d
   * return aarch64_output_sve_mov_immediate (operands[1]);"
)

;; Unpredicated moves (big-endian).  Memory accesses require secondary
;; reloads.
(define_insn "*aarch64_sve_mov<mode>_be"
  [(set (match_operand:SVE_ALL 0 "register_operand" "=w, w")
	(match_operand:SVE_ALL 1 "aarch64_nonmemory_operand" "w, Dn"))]
  "TARGET_SVE && BYTES_BIG_ENDIAN"
  "@
   mov\t%0.d, %1.d
   * return aarch64_output_sve_mov_immediate (operands[1]);"
)

;; Handle big-endian memory reloads.  We use byte PTRUE for all modes
;; to try to encourage reuse.
(define_expand "aarch64_sve_reload_be"
  [(parallel
     [(set (match_operand 0)
           (match_operand 1))
      (clobber (match_operand:VNx16BI 2 "register_operand" "=Upl"))])]
  "TARGET_SVE && BYTES_BIG_ENDIAN"
  {
    /* Create a PTRUE.  */
    emit_move_insn (operands[2], CONSTM1_RTX (VNx16BImode));

    /* Refer to the PTRUE in the appropriate mode for this move.  */
    machine_mode mode = GET_MODE (operands[0]);
    machine_mode pred_mode
      = aarch64_sve_pred_mode (GET_MODE_UNIT_SIZE (mode)).require ();
    rtx pred = gen_lowpart (pred_mode, operands[2]);

    /* Emit a predicated load or store.  */
    aarch64_emit_sve_pred_move (operands[0], pred, operands[1]);
    DONE;
  }
)

;; A predicated load or store for which the predicate is known to be
;; all-true.  Note that this pattern is generated directly by
;; aarch64_emit_sve_pred_move, so changes to this pattern will
;; need changes there as well.
(define_insn "*pred_mov<mode>"
  [(set (match_operand:SVE_ALL 0 "nonimmediate_operand" "=w, m")
	(unspec:SVE_ALL
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl")
	   (match_operand:SVE_ALL 2 "nonimmediate_operand" "m, w")]
	  UNSPEC_MERGE_PTRUE))]
  "TARGET_SVE
   && (register_operand (operands[0], <MODE>mode)
       || register_operand (operands[2], <MODE>mode))"
  "@
   ld1<Vesize>\t%0.<Vetype>, %1/z, %2
   st1<Vesize>\t%2.<Vetype>, %1, %0"
)

(define_expand "movmisalign<mode>"
  [(set (match_operand:SVE_ALL 0 "nonimmediate_operand")
	(match_operand:SVE_ALL 1 "general_operand"))]
  "TARGET_SVE"
  {
    /* Equivalent to a normal move for our purpooses.  */
    emit_move_insn (operands[0], operands[1]);
    DONE;
  }
)

(define_insn "maskload<mode><vpred>"
  [(set (match_operand:SVE_ALL 0 "register_operand" "=w")
	(unspec:SVE_ALL
	  [(match_operand:<VPRED> 2 "register_operand" "Upl")
	   (match_operand:SVE_ALL 1 "memory_operand" "m")]
	  UNSPEC_LD1_SVE))]
  "TARGET_SVE"
  "ld1<Vesize>\t%0.<Vetype>, %2/z, %1"
)

(define_insn "maskstore<mode><vpred>"
  [(set (match_operand:SVE_ALL 0 "memory_operand" "+m")
	(unspec:SVE_ALL [(match_operand:<VPRED> 2 "register_operand" "Upl")
			 (match_operand:SVE_ALL 1 "register_operand" "w")
			 (match_dup 0)]
			UNSPEC_ST1_SVE))]
  "TARGET_SVE"
  "st1<Vesize>\t%1.<Vetype>, %2, %0"
)

;; Unpredicated gather loads.
(define_expand "gather_load<mode>"
  [(set (match_operand:SVE_SD 0 "register_operand")
	(unspec:SVE_SD
	  [(match_dup 5)
	   (match_operand:DI 1 "aarch64_reg_or_zero")
	   (match_operand:<V_INT_EQUIV> 2 "register_operand")
	   (match_operand:DI 3 "const_int_operand")
	   (match_operand:DI 4 "aarch64_gather_scale_operand_<Vesize>")
	   (mem:BLK (scratch))]
	  UNSPEC_LD1_GATHER))]
  "TARGET_SVE"
  {
    operands[5] = force_reg (<VPRED>mode, CONSTM1_RTX (<VPRED>mode));
  }
)

;; Predicated gather loads for 32-bit elements.  Operand 3 is true for
;; unsigned extension and false for signed extension.
(define_insn "mask_gather_load<mode>"
  [(set (match_operand:SVE_S 0 "register_operand" "=w, w, w, w, w")
	(unspec:SVE_S
	  [(match_operand:<VPRED> 5 "register_operand" "Upl, Upl, Upl, Upl, Upl")
	   (match_operand:DI 1 "aarch64_reg_or_zero" "Z, rk, rk, rk, rk")
	   (match_operand:<V_INT_EQUIV> 2 "register_operand" "w, w, w, w, w")
	   (match_operand:DI 3 "const_int_operand" "i, Z, Ui1, Z, Ui1")
	   (match_operand:DI 4 "aarch64_gather_scale_operand_w" "Ui1, Ui1, Ui1, i, i")
	   (mem:BLK (scratch))]
	  UNSPEC_LD1_GATHER))]
  "TARGET_SVE"
  "@
   ld1w\t%0.s, %5/z, [%2.s]
   ld1w\t%0.s, %5/z, [%1, %2.s, sxtw]
   ld1w\t%0.s, %5/z, [%1, %2.s, uxtw]
   ld1w\t%0.s, %5/z, [%1, %2.s, sxtw %p4]
   ld1w\t%0.s, %5/z, [%1, %2.s, uxtw %p4]"
)

;; Predicated gather loads for 64-bit elements.  The value of operand 3
;; doesn't matter in this case.
(define_insn "mask_gather_load<mode>"
  [(set (match_operand:SVE_D 0 "register_operand" "=w, w, w")
	(unspec:SVE_D
	  [(match_operand:<VPRED> 5 "register_operand" "Upl, Upl, Upl")
	   (match_operand:DI 1 "aarch64_reg_or_zero" "Z, rk, rk")
	   (match_operand:<V_INT_EQUIV> 2 "register_operand" "w, w, w")
	   (match_operand:DI 3 "const_int_operand")
	   (match_operand:DI 4 "aarch64_gather_scale_operand_d" "Ui1, Ui1, i")
	   (mem:BLK (scratch))]
	  UNSPEC_LD1_GATHER))]
  "TARGET_SVE"
  "@
   ld1d\t%0.d, %5/z, [%2.d]
   ld1d\t%0.d, %5/z, [%1, %2.d]
   ld1d\t%0.d, %5/z, [%1, %2.d, lsl %p4]"
)

;; Unpredicated scatter store.
(define_expand "scatter_store<mode>"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(match_dup 5)
	   (match_operand:DI 0 "aarch64_reg_or_zero")
	   (match_operand:<V_INT_EQUIV> 1 "register_operand")
	   (match_operand:DI 2 "const_int_operand")
	   (match_operand:DI 3 "aarch64_gather_scale_operand_<Vesize>")
	   (match_operand:SVE_SD 4 "register_operand")]
	  UNSPEC_ST1_SCATTER))]
  "TARGET_SVE"
  {
    operands[5] = force_reg (<VPRED>mode, CONSTM1_RTX (<VPRED>mode));
  }
)

;; Predicated scatter stores for 32-bit elements.  Operand 2 is true for
;; unsigned extension and false for signed extension.
(define_insn "mask_scatter_store<mode>"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(match_operand:<VPRED> 5 "register_operand" "Upl, Upl, Upl, Upl, Upl")
	   (match_operand:DI 0 "aarch64_reg_or_zero" "Z, rk, rk, rk, rk")
	   (match_operand:<V_INT_EQUIV> 1 "register_operand" "w, w, w, w, w")
	   (match_operand:DI 2 "const_int_operand" "i, Z, Ui1, Z, Ui1")
	   (match_operand:DI 3 "aarch64_gather_scale_operand_w" "Ui1, Ui1, Ui1, i, i")
	   (match_operand:SVE_S 4 "register_operand" "w, w, w, w, w")]
	  UNSPEC_ST1_SCATTER))]
  "TARGET_SVE"
  "@
   st1w\t%4.s, %5, [%1.s]
   st1w\t%4.s, %5, [%0, %1.s, sxtw]
   st1w\t%4.s, %5, [%0, %1.s, uxtw]
   st1w\t%4.s, %5, [%0, %1.s, sxtw %p3]
   st1w\t%4.s, %5, [%0, %1.s, uxtw %p3]"
)

;; Predicated scatter stores for 64-bit elements.  The value of operand 2
;; doesn't matter in this case.
(define_insn "mask_scatter_store<mode>"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(match_operand:<VPRED> 5 "register_operand" "Upl, Upl, Upl")
	   (match_operand:DI 0 "aarch64_reg_or_zero" "Z, rk, rk")
	   (match_operand:<V_INT_EQUIV> 1 "register_operand" "w, w, w")
	   (match_operand:DI 2 "const_int_operand")
	   (match_operand:DI 3 "aarch64_gather_scale_operand_d" "Ui1, Ui1, i")
	   (match_operand:SVE_D 4 "register_operand" "w, w, w")]
	  UNSPEC_ST1_SCATTER))]
  "TARGET_SVE"
  "@
   st1d\t%4.d, %5, [%1.d]
   st1d\t%4.d, %5, [%0, %1.d]
   st1d\t%4.d, %5, [%0, %1.d, lsl %p3]"
)

;; SVE structure moves.
(define_expand "mov<mode>"
  [(set (match_operand:SVE_STRUCT 0 "nonimmediate_operand")
	(match_operand:SVE_STRUCT 1 "general_operand"))]
  "TARGET_SVE"
  {
    /* Big-endian loads and stores need to be done via LD1 and ST1;
       see the comment at the head of the file for details.  */
    if ((MEM_P (operands[0]) || MEM_P (operands[1]))
	&& BYTES_BIG_ENDIAN)
      {
	gcc_assert (can_create_pseudo_p ());
	aarch64_expand_sve_mem_move (operands[0], operands[1], <VPRED>mode);
	DONE;
      }

    if (CONSTANT_P (operands[1]))
      {
	aarch64_expand_mov_immediate (operands[0], operands[1]);
	DONE;
      }
  }
)

;; Unpredicated structure moves (little-endian).
(define_insn "*aarch64_sve_mov<mode>_le"
  [(set (match_operand:SVE_STRUCT 0 "aarch64_sve_nonimmediate_operand" "=w, Utr, w, w")
	(match_operand:SVE_STRUCT 1 "aarch64_sve_general_operand" "Utr, w, w, Dn"))]
  "TARGET_SVE && !BYTES_BIG_ENDIAN"
  "#"
  [(set_attr "length" "<insn_length>")]
)

;; Unpredicated structure moves (big-endian).  Memory accesses require
;; secondary reloads.
(define_insn "*aarch64_sve_mov<mode>_le"
  [(set (match_operand:SVE_STRUCT 0 "register_operand" "=w, w")
	(match_operand:SVE_STRUCT 1 "aarch64_nonmemory_operand" "w, Dn"))]
  "TARGET_SVE && BYTES_BIG_ENDIAN"
  "#"
  [(set_attr "length" "<insn_length>")]
)

;; Split unpredicated structure moves into pieces.  This is the same
;; for both big-endian and little-endian code, although it only needs
;; to handle memory operands for little-endian code.
(define_split
  [(set (match_operand:SVE_STRUCT 0 "aarch64_sve_nonimmediate_operand")
	(match_operand:SVE_STRUCT 1 "aarch64_sve_general_operand"))]
  "TARGET_SVE && reload_completed"
  [(const_int 0)]
  {
    rtx dest = operands[0];
    rtx src = operands[1];
    if (REG_P (dest) && REG_P (src))
      aarch64_simd_emit_reg_reg_move (operands, <VSINGLE>mode, <vector_count>);
    else
      for (unsigned int i = 0; i < <vector_count>; ++i)
	{
	  rtx subdest = simplify_gen_subreg (<VSINGLE>mode, dest, <MODE>mode,
					     i * BYTES_PER_SVE_VECTOR);
	  rtx subsrc = simplify_gen_subreg (<VSINGLE>mode, src, <MODE>mode,
					    i * BYTES_PER_SVE_VECTOR);
	  emit_insn (gen_rtx_SET (subdest, subsrc));
	}
    DONE;
  }
)

;; Predicated structure moves.  This works for both endiannesses but in
;; practice is only useful for big-endian.
(define_insn_and_split "pred_mov<mode>"
  [(set (match_operand:SVE_STRUCT 0 "aarch64_sve_struct_nonimmediate_operand" "=w, Utx")
	(unspec:SVE_STRUCT
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl")
	   (match_operand:SVE_STRUCT 2 "aarch64_sve_struct_nonimmediate_operand" "Utx, w")]
	  UNSPEC_MERGE_PTRUE))]
  "TARGET_SVE
   && (register_operand (operands[0], <MODE>mode)
       || register_operand (operands[2], <MODE>mode))"
  "#"
  "&& reload_completed"
  [(const_int 0)]
  {
    for (unsigned int i = 0; i < <vector_count>; ++i)
      {
	rtx subdest = simplify_gen_subreg (<VSINGLE>mode, operands[0],
					   <MODE>mode,
					   i * BYTES_PER_SVE_VECTOR);
	rtx subsrc = simplify_gen_subreg (<VSINGLE>mode, operands[2],
					  <MODE>mode,
					  i * BYTES_PER_SVE_VECTOR);
	aarch64_emit_sve_pred_move (subdest, operands[1], subsrc);
      }
    DONE;
  }
  [(set_attr "length" "<insn_length>")]
)

(define_expand "mov<mode>"
  [(set (match_operand:PRED_ALL 0 "nonimmediate_operand")
	(match_operand:PRED_ALL 1 "general_operand"))]
  "TARGET_SVE"
  {
    if (GET_CODE (operands[0]) == MEM)
      operands[1] = force_reg (<MODE>mode, operands[1]);
  }
)

(define_insn "*aarch64_sve_mov<mode>"
  [(set (match_operand:PRED_ALL 0 "nonimmediate_operand" "=Upa, m, Upa, Upa, Upa")
	(match_operand:PRED_ALL 1 "general_operand" "Upa, Upa, m, Dz, Dm"))]
  "TARGET_SVE
   && (register_operand (operands[0], <MODE>mode)
       || register_operand (operands[1], <MODE>mode))"
  "@
   mov\t%0.b, %1.b
   str\t%1, %0
   ldr\t%0, %1
   pfalse\t%0.b
   * return aarch64_output_ptrue (<MODE>mode, '<Vetype>');"
)

;; Handle extractions from a predicate by converting to an integer vector
;; and extracting from there.
(define_expand "vec_extract<vpred><Vel>"
  [(match_operand:<VEL> 0 "register_operand")
   (match_operand:<VPRED> 1 "register_operand")
   (match_operand:SI 2 "nonmemory_operand")
   ;; Dummy operand to which we can attach the iterator.
   (reg:SVE_I V0_REGNUM)]
  "TARGET_SVE"
  {
    rtx tmp = gen_reg_rtx (<MODE>mode);
    emit_insn (gen_aarch64_sve_dup<mode>_const (tmp, operands[1],
						CONST1_RTX (<MODE>mode),
						CONST0_RTX (<MODE>mode)));
    emit_insn (gen_vec_extract<mode><Vel> (operands[0], tmp, operands[2]));
    DONE;
  }
)

(define_expand "vec_extract<mode><Vel>"
  [(set (match_operand:<VEL> 0 "register_operand")
	(vec_select:<VEL>
	  (match_operand:SVE_ALL 1 "register_operand")
	  (parallel [(match_operand:SI 2 "nonmemory_operand")])))]
  "TARGET_SVE"
  {
    poly_int64 val;
    if (poly_int_rtx_p (operands[2], &val)
	&& known_eq (val, GET_MODE_NUNITS (<MODE>mode) - 1))
      {
	/* The last element can be extracted with a LASTB and a false
	   predicate.  */
	rtx sel = force_reg (<VPRED>mode, CONST0_RTX (<VPRED>mode));
	emit_insn (gen_extract_last_<mode> (operands[0], sel, operands[1]));
	DONE;
      }
    if (!CONST_INT_P (operands[2]))
      {
	/* Create an index with operand[2] as the base and -1 as the step.
	   It will then be zero for the element we care about.  */
	rtx index = gen_lowpart (<VEL_INT>mode, operands[2]);
	index = force_reg (<VEL_INT>mode, index);
	rtx series = gen_reg_rtx (<V_INT_EQUIV>mode);
	emit_insn (gen_vec_series<v_int_equiv> (series, index, constm1_rtx));

	/* Get a predicate that is true for only that element.  */
	rtx zero = CONST0_RTX (<V_INT_EQUIV>mode);
	rtx cmp = gen_rtx_EQ (<V_INT_EQUIV>mode, series, zero);
	rtx sel = gen_reg_rtx (<VPRED>mode);
	emit_insn (gen_vec_cmp<v_int_equiv><vpred> (sel, cmp, series, zero));

	/* Select the element using LASTB.  */
	emit_insn (gen_extract_last_<mode> (operands[0], sel, operands[1]));
	DONE;
      }
  }
)

;; Extract element zero.  This is a special case because we want to force
;; the registers to be the same for the second alternative, and then
;; split the instruction into nothing after RA.
(define_insn_and_split "*vec_extract<mode><Vel>_0"
  [(set (match_operand:<VEL> 0 "aarch64_simd_nonimmediate_operand" "=r, w, Utv")
	(vec_select:<VEL>
	  (match_operand:SVE_ALL 1 "register_operand" "w, 0, w")
	  (parallel [(const_int 0)])))]
  "TARGET_SVE"
  {
    operands[1] = gen_rtx_REG (<V128>mode, REGNO (operands[1]));
    switch (which_alternative)
      {
	case 0:
	  return "umov\\t%<vwcore>0, %1.<Vetype>[0]";
	case 1:
	  return "#";
	case 2:
	  return "st1\\t{%1.<Vetype>}[0], %0";
	default:
	  gcc_unreachable ();
      }
  }
  "&& reload_completed
   && REG_P (operands[0])
   && REGNO (operands[0]) == REGNO (operands[1])"
  [(const_int 0)]
  {
    emit_note (NOTE_INSN_DELETED);
    DONE;
  }
  [(set_attr "type" "neon_to_gp_q, untyped, neon_store1_one_lane_q")]
)

;; Extract an element from the Advanced SIMD portion of the register.
;; We don't just reuse the aarch64-simd.md pattern because we don't
;; want any change in lane number on big-endian targets.
(define_insn "*vec_extract<mode><Vel>_v128"
  [(set (match_operand:<VEL> 0 "aarch64_simd_nonimmediate_operand" "=r, w, Utv")
	(vec_select:<VEL>
	  (match_operand:SVE_ALL 1 "register_operand" "w, w, w")
	  (parallel [(match_operand:SI 2 "const_int_operand")])))]
  "TARGET_SVE
   && IN_RANGE (INTVAL (operands[2]) * GET_MODE_SIZE (<VEL>mode), 1, 15)"
  {
    operands[1] = gen_rtx_REG (<V128>mode, REGNO (operands[1]));
    switch (which_alternative)
      {
	case 0:
	  return "umov\\t%<vwcore>0, %1.<Vetype>[%2]";
	case 1:
	  return "dup\\t%<Vetype>0, %1.<Vetype>[%2]";
	case 2:
	  return "st1\\t{%1.<Vetype>}[%2], %0";
	default:
	  gcc_unreachable ();
      }
  }
  [(set_attr "type" "neon_to_gp_q, neon_dup_q, neon_store1_one_lane_q")]
)

;; Extract an element in the range of DUP.  This pattern allows the
;; source and destination to be different.
(define_insn "*vec_extract<mode><Vel>_dup"
  [(set (match_operand:<VEL> 0 "register_operand" "=w")
	(vec_select:<VEL>
	  (match_operand:SVE_ALL 1 "register_operand" "w")
	  (parallel [(match_operand:SI 2 "const_int_operand")])))]
  "TARGET_SVE
   && IN_RANGE (INTVAL (operands[2]) * GET_MODE_SIZE (<VEL>mode), 16, 63)"
  {
    operands[0] = gen_rtx_REG (<MODE>mode, REGNO (operands[0]));
    return "dup\t%0.<Vetype>, %1.<Vetype>[%2]";
  }
)

;; Extract an element outside the range of DUP.  This pattern requires the
;; source and destination to be the same.
(define_insn "*vec_extract<mode><Vel>_ext"
  [(set (match_operand:<VEL> 0 "register_operand" "=w")
	(vec_select:<VEL>
	  (match_operand:SVE_ALL 1 "register_operand" "0")
	  (parallel [(match_operand:SI 2 "const_int_operand")])))]
  "TARGET_SVE && INTVAL (operands[2]) * GET_MODE_SIZE (<VEL>mode) >= 64"
  {
    operands[0] = gen_rtx_REG (<MODE>mode, REGNO (operands[0]));
    operands[2] = GEN_INT (INTVAL (operands[2]) * GET_MODE_SIZE (<VEL>mode));
    return "ext\t%0.b, %0.b, %0.b, #%2";
  }
)

;; Extract the last active element of operand 1 into operand 0.
;; If no elements are active, extract the last inactive element instead.
(define_insn "extract_last_<mode>"
  [(set (match_operand:<VEL> 0 "register_operand" "=r, w")
	(unspec:<VEL>
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl")
	   (match_operand:SVE_ALL 2 "register_operand" "w, w")]
	  UNSPEC_LASTB))]
  "TARGET_SVE"
  "@
   lastb\t%<vwcore>0, %1, %2.<Vetype>
   lastb\t%<Vetype>0, %1, %2.<Vetype>"
)

(define_expand "vec_duplicate<mode>"
  [(parallel
    [(set (match_operand:SVE_ALL 0 "register_operand")
	  (vec_duplicate:SVE_ALL
	    (match_operand:<VEL> 1 "aarch64_sve_dup_operand")))
     (clobber (scratch:<VPRED>))])]
  "TARGET_SVE"
  {
    if (MEM_P (operands[1]))
      {
	rtx ptrue = force_reg (<VPRED>mode, CONSTM1_RTX (<VPRED>mode));
	emit_insn (gen_sve_ld1r<mode> (operands[0], ptrue, operands[1],
				       CONST0_RTX (<MODE>mode)));
	DONE;
      }
  }
)

;; Accept memory operands for the benefit of combine, and also in case
;; the scalar input gets spilled to memory during RA.  We want to split
;; the load at the first opportunity in order to allow the PTRUE to be
;; optimized with surrounding code.
(define_insn_and_split "*vec_duplicate<mode>_reg"
  [(set (match_operand:SVE_ALL 0 "register_operand" "=w, w, w")
	(vec_duplicate:SVE_ALL
	  (match_operand:<VEL> 1 "aarch64_sve_dup_operand" "r, w, Uty")))
   (clobber (match_scratch:<VPRED> 2 "=X, X, Upl"))]
  "TARGET_SVE"
  "@
   mov\t%0.<Vetype>, %<vwcore>1
   mov\t%0.<Vetype>, %<Vetype>1
   #"
  "&& MEM_P (operands[1])"
  [(const_int 0)]
  {
    if (GET_CODE (operands[2]) == SCRATCH)
      operands[2] = gen_reg_rtx (<VPRED>mode);
    emit_move_insn (operands[2], CONSTM1_RTX (<VPRED>mode));
    emit_insn (gen_sve_ld1r<mode> (operands[0], operands[2], operands[1],
				   CONST0_RTX (<MODE>mode)));
    DONE;
  }
  [(set_attr "length" "4,4,8")]
)

;; This is used for vec_duplicate<mode>s from memory, but can also
;; be used by combine to optimize selects of a a vec_duplicate<mode>
;; with zero.
(define_insn "sve_ld1r<mode>"
  [(set (match_operand:SVE_ALL 0 "register_operand" "=w")
	(unspec:SVE_ALL
	  [(match_operand:<VPRED> 1 "register_operand" "Upl")
	   (vec_duplicate:SVE_ALL
	     (match_operand:<VEL> 2 "aarch64_sve_ld1r_operand" "Uty"))
	   (match_operand:SVE_ALL 3 "aarch64_simd_imm_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  "ld1r<Vesize>\t%0.<Vetype>, %1/z, %2"
)

;; Load 128 bits from memory and duplicate to fill a vector.  Since there
;; are so few operations on 128-bit "elements", we don't define a VNx1TI
;; and simply use vectors of bytes instead.
(define_insn "*sve_ld1rq<Vesize>"
  [(set (match_operand:SVE_ALL 0 "register_operand" "=w")
	(unspec:SVE_ALL
	  [(match_operand:<VPRED> 1 "register_operand" "Upl")
	   (match_operand:TI 2 "aarch64_sve_ld1r_operand" "Uty")]
	  UNSPEC_LD1RQ))]
  "TARGET_SVE"
  "ld1rq<Vesize>\t%0.<Vetype>, %1/z, %2"
)

;; Implement a predicate broadcast by shifting the low bit of the scalar
;; input into the top bit and using a WHILELO.  An alternative would be to
;; duplicate the input and do a compare with zero.
(define_expand "vec_duplicate<mode>"
  [(set (match_operand:PRED_ALL 0 "register_operand")
	(vec_duplicate:PRED_ALL (match_operand 1 "register_operand")))]
  "TARGET_SVE"
  {
    rtx tmp = gen_reg_rtx (DImode);
    rtx op1 = gen_lowpart (DImode, operands[1]);
    emit_insn (gen_ashldi3 (tmp, op1, gen_int_mode (63, DImode)));
    emit_insn (gen_while_ultdi<mode> (operands[0], const0_rtx, tmp));
    DONE;
  }
)

(define_insn "vec_series<mode>"
  [(set (match_operand:SVE_I 0 "register_operand" "=w, w, w")
	(vec_series:SVE_I
	  (match_operand:<VEL> 1 "aarch64_sve_index_operand" "Usi, r, r")
	  (match_operand:<VEL> 2 "aarch64_sve_index_operand" "r, Usi, r")))]
  "TARGET_SVE"
  "@
   index\t%0.<Vetype>, #%1, %<vw>2
   index\t%0.<Vetype>, %<vw>1, #%2
   index\t%0.<Vetype>, %<vw>1, %<vw>2"
)

;; Optimize {x, x, x, x, ...} + {0, n, 2*n, 3*n, ...} if n is in range
;; of an INDEX instruction.
(define_insn "*vec_series<mode>_plus"
  [(set (match_operand:SVE_I 0 "register_operand" "=w")
	(plus:SVE_I
	  (vec_duplicate:SVE_I
	    (match_operand:<VEL> 1 "register_operand" "r"))
	  (match_operand:SVE_I 2 "immediate_operand")))]
  "TARGET_SVE && aarch64_check_zero_based_sve_index_immediate (operands[2])"
  {
    operands[2] = aarch64_check_zero_based_sve_index_immediate (operands[2]);
    return "index\t%0.<Vetype>, %<vw>1, #%2";
  }
)

;; Unpredicated LD[234].
(define_expand "vec_load_lanes<mode><vsingle>"
  [(set (match_operand:SVE_STRUCT 0 "register_operand")
	(unspec:SVE_STRUCT
	  [(match_dup 2)
	   (match_operand:SVE_STRUCT 1 "memory_operand")]
	  UNSPEC_LDN))]
  "TARGET_SVE"
  {
    operands[2] = force_reg (<VPRED>mode, CONSTM1_RTX (<VPRED>mode));
  }
)

;; Predicated LD[234].
(define_insn "vec_mask_load_lanes<mode><vsingle>"
  [(set (match_operand:SVE_STRUCT 0 "register_operand" "=w")
	(unspec:SVE_STRUCT
	  [(match_operand:<VPRED> 2 "register_operand" "Upl")
	   (match_operand:SVE_STRUCT 1 "memory_operand" "m")]
	  UNSPEC_LDN))]
  "TARGET_SVE"
  "ld<vector_count><Vesize>\t%0, %2/z, %1"
)

;; Unpredicated ST[234].  This is always a full update, so the dependence
;; on the old value of the memory location (via (match_dup 0)) is redundant.
;; There doesn't seem to be any obvious benefit to treating the all-true
;; case differently though.  In particular, it's very unlikely that we'll
;; only find out during RTL that a store_lanes is dead.
(define_expand "vec_store_lanes<mode><vsingle>"
  [(set (match_operand:SVE_STRUCT 0 "memory_operand")
	(unspec:SVE_STRUCT
	  [(match_dup 2)
	   (match_operand:SVE_STRUCT 1 "register_operand")
	   (match_dup 0)]
	  UNSPEC_STN))]
  "TARGET_SVE"
  {
    operands[2] = force_reg (<VPRED>mode, CONSTM1_RTX (<VPRED>mode));
  }
)

;; Predicated ST[234].
(define_insn "vec_mask_store_lanes<mode><vsingle>"
  [(set (match_operand:SVE_STRUCT 0 "memory_operand" "+m")
	(unspec:SVE_STRUCT
	  [(match_operand:<VPRED> 2 "register_operand" "Upl")
	   (match_operand:SVE_STRUCT 1 "register_operand" "w")
	   (match_dup 0)]
	  UNSPEC_STN))]
  "TARGET_SVE"
  "st<vector_count><Vesize>\t%1, %2, %0"
)

(define_expand "vec_perm<mode>"
  [(match_operand:SVE_ALL 0 "register_operand")
   (match_operand:SVE_ALL 1 "register_operand")
   (match_operand:SVE_ALL 2 "register_operand")
   (match_operand:<V_INT_EQUIV> 3 "aarch64_sve_vec_perm_operand")]
  "TARGET_SVE && GET_MODE_NUNITS (<MODE>mode).is_constant ()"
  {
    aarch64_expand_sve_vec_perm (operands[0], operands[1],
				 operands[2], operands[3]);
    DONE;
  }
)

(define_insn "*aarch64_sve_tbl<mode>"
  [(set (match_operand:SVE_ALL 0 "register_operand" "=w")
	(unspec:SVE_ALL
	  [(match_operand:SVE_ALL 1 "register_operand" "w")
	   (match_operand:<V_INT_EQUIV> 2 "register_operand" "w")]
	  UNSPEC_TBL))]
  "TARGET_SVE"
  "tbl\t%0.<Vetype>, %1.<Vetype>, %2.<Vetype>"
)

(define_insn "*aarch64_sve_<perm_insn><perm_hilo><mode>"
  [(set (match_operand:PRED_ALL 0 "register_operand" "=Upa")
	(unspec:PRED_ALL [(match_operand:PRED_ALL 1 "register_operand" "Upa")
			  (match_operand:PRED_ALL 2 "register_operand" "Upa")]
			 PERMUTE))]
  "TARGET_SVE"
  "<perm_insn><perm_hilo>\t%0.<Vetype>, %1.<Vetype>, %2.<Vetype>"
)

(define_insn "aarch64_sve_<perm_insn><perm_hilo><mode>"
  [(set (match_operand:SVE_ALL 0 "register_operand" "=w")
	(unspec:SVE_ALL [(match_operand:SVE_ALL 1 "register_operand" "w")
			 (match_operand:SVE_ALL 2 "register_operand" "w")]
			PERMUTE))]
  "TARGET_SVE"
  "<perm_insn><perm_hilo>\t%0.<Vetype>, %1.<Vetype>, %2.<Vetype>"
)

(define_insn "*aarch64_sve_rev64<mode>"
  [(set (match_operand:SVE_BHS 0 "register_operand" "=w")
	(unspec:SVE_BHS
	  [(match_operand:VNx2BI 1 "register_operand" "Upl")
	   (unspec:SVE_BHS [(match_operand:SVE_BHS 2 "register_operand" "w")]
			   UNSPEC_REV64)]
	  UNSPEC_MERGE_PTRUE))]
  "TARGET_SVE"
  "rev<Vesize>\t%0.d, %1/m, %2.d"
)

(define_insn "*aarch64_sve_rev32<mode>"
  [(set (match_operand:SVE_BH 0 "register_operand" "=w")
	(unspec:SVE_BH
	  [(match_operand:VNx4BI 1 "register_operand" "Upl")
	   (unspec:SVE_BH [(match_operand:SVE_BH 2 "register_operand" "w")]
			  UNSPEC_REV32)]
	  UNSPEC_MERGE_PTRUE))]
  "TARGET_SVE"
  "rev<Vesize>\t%0.s, %1/m, %2.s"
)

(define_insn "*aarch64_sve_rev16vnx16qi"
  [(set (match_operand:VNx16QI 0 "register_operand" "=w")
	(unspec:VNx16QI
	  [(match_operand:VNx8BI 1 "register_operand" "Upl")
	   (unspec:VNx16QI [(match_operand:VNx16QI 2 "register_operand" "w")]
			   UNSPEC_REV16)]
	  UNSPEC_MERGE_PTRUE))]
  "TARGET_SVE"
  "revb\t%0.h, %1/m, %2.h"
)

(define_insn "*aarch64_sve_rev<mode>"
  [(set (match_operand:SVE_ALL 0 "register_operand" "=w")
	(unspec:SVE_ALL [(match_operand:SVE_ALL 1 "register_operand" "w")]
			UNSPEC_REV))]
  "TARGET_SVE"
  "rev\t%0.<Vetype>, %1.<Vetype>")

(define_insn "*aarch64_sve_dup_lane<mode>"
  [(set (match_operand:SVE_ALL 0 "register_operand" "=w")
	(vec_duplicate:SVE_ALL
	  (vec_select:<VEL>
	    (match_operand:SVE_ALL 1 "register_operand" "w")
	    (parallel [(match_operand:SI 2 "const_int_operand")]))))]
  "TARGET_SVE
   && IN_RANGE (INTVAL (operands[2]) * GET_MODE_SIZE (<VEL>mode), 0, 63)"
  "dup\t%0.<Vetype>, %1.<Vetype>[%2]"
)

;; Note that the immediate (third) operand is the lane index not
;; the byte index.
(define_insn "*aarch64_sve_ext<mode>"
  [(set (match_operand:SVE_ALL 0 "register_operand" "=w")
	(unspec:SVE_ALL [(match_operand:SVE_ALL 1 "register_operand" "0")
			 (match_operand:SVE_ALL 2 "register_operand" "w")
			 (match_operand:SI 3 "const_int_operand")]
			UNSPEC_EXT))]
  "TARGET_SVE
   && IN_RANGE (INTVAL (operands[3]) * GET_MODE_SIZE (<VEL>mode), 0, 255)"
  {
    operands[3] = GEN_INT (INTVAL (operands[3]) * GET_MODE_SIZE (<VEL>mode));
    return "ext\\t%0.b, %0.b, %2.b, #%3";
  }
)

(define_insn "add<mode>3"
  [(set (match_operand:SVE_I 0 "register_operand" "=w, w, w, w")
	(plus:SVE_I
	  (match_operand:SVE_I 1 "register_operand" "%0, 0, 0, w")
	  (match_operand:SVE_I 2 "aarch64_sve_add_operand" "vsa, vsn, vsi, w")))]
  "TARGET_SVE"
  "@
   add\t%0.<Vetype>, %0.<Vetype>, #%D2
   sub\t%0.<Vetype>, %0.<Vetype>, #%N2
   * return aarch64_output_sve_inc_dec_immediate (\"%0.<Vetype>\", operands[2]);
   add\t%0.<Vetype>, %1.<Vetype>, %2.<Vetype>"
)

(define_insn "sub<mode>3"
  [(set (match_operand:SVE_I 0 "register_operand" "=w, w")
	(minus:SVE_I
	  (match_operand:SVE_I 1 "aarch64_sve_arith_operand" "w, vsa")
	  (match_operand:SVE_I 2 "register_operand" "w, 0")))]
  "TARGET_SVE"
  "@
   sub\t%0.<Vetype>, %1.<Vetype>, %2.<Vetype>
   subr\t%0.<Vetype>, %0.<Vetype>, #%D1"
)

;; Unpredicated multiplication.
(define_expand "mul<mode>3"
  [(set (match_operand:SVE_I 0 "register_operand")
	(unspec:SVE_I
	  [(match_dup 3)
	   (mult:SVE_I
	     (match_operand:SVE_I 1 "register_operand")
	     (match_operand:SVE_I 2 "aarch64_sve_mul_operand"))]
	  UNSPEC_MERGE_PTRUE))]
  "TARGET_SVE"
  {
    operands[3] = force_reg (<VPRED>mode, CONSTM1_RTX (<VPRED>mode));
  }
)

;; Multiplication predicated with a PTRUE.  We don't actually need the
;; predicate for the first alternative, but using Upa or X isn't likely
;; to gain much and would make the instruction seem less uniform to the
;; register allocator.
(define_insn "*mul<mode>3"
  [(set (match_operand:SVE_I 0 "register_operand" "=w, w, ?&w")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl, Upl")
	   (mult:SVE_I
	     (match_operand:SVE_I 2 "register_operand" "%0, 0, w")
	     (match_operand:SVE_I 3 "aarch64_sve_mul_operand" "vsm, w, w"))]
	  UNSPEC_MERGE_PTRUE))]
  "TARGET_SVE"
  "@
   mul\t%0.<Vetype>, %0.<Vetype>, #%3
   mul\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
   movprfx\t%0, %2\;mul\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>"
  [(set_attr "movprfx" "*,*,yes")]
)

(define_insn "*madd<mode>"
  [(set (match_operand:SVE_I 0 "register_operand" "=w, w, ?&w")
	(plus:SVE_I
	  (unspec:SVE_I
	    [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl, Upl")
	     (mult:SVE_I (match_operand:SVE_I 2 "register_operand" "%0, w, w")
			 (match_operand:SVE_I 3 "register_operand" "w, w, w"))]
	    UNSPEC_MERGE_PTRUE)
	  (match_operand:SVE_I 4 "register_operand" "w, 0, w")))]
  "TARGET_SVE"
  "@
   mad\t%0.<Vetype>, %1/m, %3.<Vetype>, %4.<Vetype>
   mla\t%0.<Vetype>, %1/m, %2.<Vetype>, %3.<Vetype>
   movprfx\t%0, %4\;mla\t%0.<Vetype>, %1/m, %2.<Vetype>, %3.<Vetype>"
  [(set_attr "movprfx" "*,*,yes")]
)

(define_insn "*msub<mode>3"
  [(set (match_operand:SVE_I 0 "register_operand" "=w, w, ?&w")
	(minus:SVE_I
	  (match_operand:SVE_I 4 "register_operand" "w, 0, w")
	  (unspec:SVE_I
	    [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl, Upl")
	     (mult:SVE_I (match_operand:SVE_I 2 "register_operand" "%0, w, w")
			 (match_operand:SVE_I 3 "register_operand" "w, w, w"))]
	    UNSPEC_MERGE_PTRUE)))]
  "TARGET_SVE"
  "@
   msb\t%0.<Vetype>, %1/m, %3.<Vetype>, %4.<Vetype>
   mls\t%0.<Vetype>, %1/m, %2.<Vetype>, %3.<Vetype>
   movprfx\t%0, %4\;mls\t%0.<Vetype>, %1/m, %2.<Vetype>, %3.<Vetype>"
  [(set_attr "movprfx" "*,*,yes")]
)

;; Unpredicated highpart multiplication.
(define_expand "<su>mul<mode>3_highpart"
  [(set (match_operand:SVE_I 0 "register_operand")
	(unspec:SVE_I
	  [(match_dup 3)
	   (unspec:SVE_I [(match_operand:SVE_I 1 "register_operand")
			  (match_operand:SVE_I 2 "register_operand")]
			 MUL_HIGHPART)]
	  UNSPEC_MERGE_PTRUE))]
  "TARGET_SVE"
  {
    operands[3] = force_reg (<VPRED>mode, CONSTM1_RTX (<VPRED>mode));
  }
)

;; Predicated highpart multiplication.
(define_insn "*<su>mul<mode>3_highpart"
  [(set (match_operand:SVE_I 0 "register_operand" "=w, ?&w")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl")
	   (unspec:SVE_I [(match_operand:SVE_I 2 "register_operand" "%0, w")
			  (match_operand:SVE_I 3 "register_operand" "w, w")]
			 MUL_HIGHPART)]
	  UNSPEC_MERGE_PTRUE))]
  "TARGET_SVE"
  "@
   <su>mulh\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
   movprfx\t%0, %2\;<su>mulh\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>"
  [(set_attr "movprfx" "*,yes")]
)

;; Unpredicated division.
(define_expand "<optab><mode>3"
  [(set (match_operand:SVE_SDI 0 "register_operand")
	(unspec:SVE_SDI
	  [(match_dup 3)
	   (SVE_INT_BINARY_SD:SVE_SDI
	     (match_operand:SVE_SDI 1 "register_operand")
	     (match_operand:SVE_SDI 2 "register_operand"))]
	  UNSPEC_MERGE_PTRUE))]
  "TARGET_SVE"
  {
    operands[3] = force_reg (<VPRED>mode, CONSTM1_RTX (<VPRED>mode));
  }
)

;; Division predicated with a PTRUE.
(define_insn "*<optab><mode>3"
  [(set (match_operand:SVE_SDI 0 "register_operand" "=w, w, ?&w")
	(unspec:SVE_SDI
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl, Upl")
	   (SVE_INT_BINARY_SD:SVE_SDI
	     (match_operand:SVE_SDI 2 "register_operand" "0, w, w")
	     (match_operand:SVE_SDI 3 "aarch64_sve_mul_operand" "w, 0, w"))]
	  UNSPEC_MERGE_PTRUE))]
  "TARGET_SVE"
  "@
   <sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
   <sve_int_op>r\t%0.<Vetype>, %1/m, %0.<Vetype>, %2.<Vetype>
   movprfx\t%0, %2\;<sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>"
  [(set_attr "movprfx" "*,*,yes")]
)

;; Unpredicated NEG, NOT and POPCOUNT.
(define_expand "<optab><mode>2"
  [(set (match_operand:SVE_I 0 "register_operand")
	(unspec:SVE_I
	  [(match_dup 2)
	   (SVE_INT_UNARY:SVE_I (match_operand:SVE_I 1 "register_operand"))]
	  UNSPEC_MERGE_PTRUE))]
  "TARGET_SVE"
  {
    operands[2] = force_reg (<VPRED>mode, CONSTM1_RTX (<VPRED>mode));
  }
)

;; NEG, NOT and POPCOUNT predicated with a PTRUE.
(define_insn "*<optab><mode>2"
  [(set (match_operand:SVE_I 0 "register_operand" "=w")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand" "Upl")
	   (SVE_INT_UNARY:SVE_I
	     (match_operand:SVE_I 2 "register_operand" "w"))]
	  UNSPEC_MERGE_PTRUE))]
  "TARGET_SVE"
  "<sve_int_op>\t%0.<Vetype>, %1/m, %2.<Vetype>"
)

;; Vector AND, ORR and XOR.
(define_insn "<optab><mode>3"
  [(set (match_operand:SVE_I 0 "register_operand" "=w, w")
	(LOGICAL:SVE_I
	  (match_operand:SVE_I 1 "register_operand" "%0, w")
	  (match_operand:SVE_I 2 "aarch64_sve_logical_operand" "vsl, w")))]
  "TARGET_SVE"
  "@
   <logical>\t%0.<Vetype>, %0.<Vetype>, #%C2
   <logical>\t%0.d, %1.d, %2.d"
)

;; Vector AND, ORR and XOR on floating-point modes.  We avoid subregs
;; by providing this, but we need to use UNSPECs since rtx logical ops
;; aren't defined for floating-point modes.
(define_insn "*<optab><mode>3"
  [(set (match_operand:SVE_F 0 "register_operand" "=w")
	(unspec:SVE_F [(match_operand:SVE_F 1 "register_operand" "w")
		       (match_operand:SVE_F 2 "register_operand" "w")]
		      LOGICALF))]
  "TARGET_SVE"
  "<logicalf_op>\t%0.d, %1.d, %2.d"
)

;; REG_EQUAL notes on "not<mode>3" should ensure that we can generate
;; this pattern even though the NOT instruction itself is predicated.
(define_insn "bic<mode>3"
  [(set (match_operand:SVE_I 0 "register_operand" "=w")
	(and:SVE_I
	  (not:SVE_I (match_operand:SVE_I 1 "register_operand" "w"))
	  (match_operand:SVE_I 2 "register_operand" "w")))]
  "TARGET_SVE"
  "bic\t%0.d, %2.d, %1.d"
)

;; Predicate AND.  We can reuse one of the inputs as the GP.
(define_insn "and<mode>3"
  [(set (match_operand:PRED_ALL 0 "register_operand" "=Upa")
	(and:PRED_ALL (match_operand:PRED_ALL 1 "register_operand" "Upa")
		      (match_operand:PRED_ALL 2 "register_operand" "Upa")))]
  "TARGET_SVE"
  "and\t%0.b, %1/z, %1.b, %2.b"
)

;; Unpredicated predicate ORR and XOR.
(define_expand "<optab><mode>3"
  [(set (match_operand:PRED_ALL 0 "register_operand")
	(and:PRED_ALL
	  (LOGICAL_OR:PRED_ALL
	    (match_operand:PRED_ALL 1 "register_operand")
	    (match_operand:PRED_ALL 2 "register_operand"))
	  (match_dup 3)))]
  "TARGET_SVE"
  {
    operands[3] = force_reg (<MODE>mode, CONSTM1_RTX (<MODE>mode));
  }
)

;; Predicated predicate ORR and XOR.
(define_insn "pred_<optab><mode>3"
  [(set (match_operand:PRED_ALL 0 "register_operand" "=Upa")
	(and:PRED_ALL
	  (LOGICAL:PRED_ALL
	    (match_operand:PRED_ALL 2 "register_operand" "Upa")
	    (match_operand:PRED_ALL 3 "register_operand" "Upa"))
	  (match_operand:PRED_ALL 1 "register_operand" "Upa")))]
  "TARGET_SVE"
  "<logical>\t%0.b, %1/z, %2.b, %3.b"
)

;; Perform a logical operation on operands 2 and 3, using operand 1 as
;; the GP (which is known to be a PTRUE).  Store the result in operand 0
;; and set the flags in the same way as for PTEST.  The (and ...) in the
;; UNSPEC_PTEST_PTRUE is logically redundant, but means that the tested
;; value is structurally equivalent to rhs of the second set.
(define_insn "*<optab><mode>3_cc"
  [(set (reg:CC CC_REGNUM)
	(compare:CC
	  (unspec:SI [(match_operand:PRED_ALL 1 "register_operand" "Upa")
		      (and:PRED_ALL
			(LOGICAL:PRED_ALL
			  (match_operand:PRED_ALL 2 "register_operand" "Upa")
			  (match_operand:PRED_ALL 3 "register_operand" "Upa"))
			(match_dup 1))]
		     UNSPEC_PTEST_PTRUE)
	  (const_int 0)))
   (set (match_operand:PRED_ALL 0 "register_operand" "=Upa")
	(and:PRED_ALL (LOGICAL:PRED_ALL (match_dup 2) (match_dup 3))
		      (match_dup 1)))]
  "TARGET_SVE"
  "<logical>s\t%0.b, %1/z, %2.b, %3.b"
)

;; Unpredicated predicate inverse.
(define_expand "one_cmpl<mode>2"
  [(set (match_operand:PRED_ALL 0 "register_operand")
	(and:PRED_ALL
	  (not:PRED_ALL (match_operand:PRED_ALL 1 "register_operand"))
	  (match_dup 2)))]
  "TARGET_SVE"
  {
    operands[2] = force_reg (<MODE>mode, CONSTM1_RTX (<MODE>mode));
  }
)

;; Predicated predicate inverse.
(define_insn "*one_cmpl<mode>3"
  [(set (match_operand:PRED_ALL 0 "register_operand" "=Upa")
	(and:PRED_ALL
	  (not:PRED_ALL (match_operand:PRED_ALL 2 "register_operand" "Upa"))
	  (match_operand:PRED_ALL 1 "register_operand" "Upa")))]
  "TARGET_SVE"
  "not\t%0.b, %1/z, %2.b"
)

;; Predicated predicate BIC and ORN.
(define_insn "*<nlogical><mode>3"
  [(set (match_operand:PRED_ALL 0 "register_operand" "=Upa")
	(and:PRED_ALL
	  (NLOGICAL:PRED_ALL
	    (not:PRED_ALL (match_operand:PRED_ALL 2 "register_operand" "Upa"))
	    (match_operand:PRED_ALL 3 "register_operand" "Upa"))
	  (match_operand:PRED_ALL 1 "register_operand" "Upa")))]
  "TARGET_SVE"
  "<nlogical>\t%0.b, %1/z, %3.b, %2.b"
)

;; Predicated predicate NAND and NOR.
(define_insn "*<logical_nn><mode>3"
  [(set (match_operand:PRED_ALL 0 "register_operand" "=Upa")
	(and:PRED_ALL
	  (NLOGICAL:PRED_ALL
	    (not:PRED_ALL (match_operand:PRED_ALL 2 "register_operand" "Upa"))
	    (not:PRED_ALL (match_operand:PRED_ALL 3 "register_operand" "Upa")))
	  (match_operand:PRED_ALL 1 "register_operand" "Upa")))]
  "TARGET_SVE"
  "<logical_nn>\t%0.b, %1/z, %2.b, %3.b"
)

;; Unpredicated LSL, LSR and ASR by a vector.
(define_expand "v<optab><mode>3"
  [(set (match_operand:SVE_I 0 "register_operand")
	(unspec:SVE_I
	  [(match_dup 3)
	   (ASHIFT:SVE_I
	     (match_operand:SVE_I 1 "register_operand")
	     (match_operand:SVE_I 2 "aarch64_sve_<lr>shift_operand"))]
	  UNSPEC_MERGE_PTRUE))]
  "TARGET_SVE"
  {
    operands[3] = force_reg (<VPRED>mode, CONSTM1_RTX (<VPRED>mode));
  }
)

;; LSL, LSR and ASR by a vector, predicated with a PTRUE.  We don't
;; actually need the predicate for the first alternative, but using Upa
;; or X isn't likely to gain much and would make the instruction seem
;; less uniform to the register allocator.
(define_insn "*v<optab><mode>3"
  [(set (match_operand:SVE_I 0 "register_operand" "=w, w, ?&w")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl, Upl")
	   (ASHIFT:SVE_I
	     (match_operand:SVE_I 2 "register_operand" "w, 0, w")
	     (match_operand:SVE_I 3 "aarch64_sve_<lr>shift_operand" "D<lr>, w, w"))]
	  UNSPEC_MERGE_PTRUE))]
  "TARGET_SVE"
  "@
   <shift>\t%0.<Vetype>, %2.<Vetype>, #%3
   <shift>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
   movprfx\t%0, %2\;<shift>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>"
  [(set_attr "movprfx" "*,*,yes")]
)

;; LSL, LSR and ASR by a scalar, which expands into one of the vector
;; shifts above.
(define_expand "<ASHIFT:optab><mode>3"
  [(set (match_operand:SVE_I 0 "register_operand")
	(ASHIFT:SVE_I (match_operand:SVE_I 1 "register_operand")
		      (match_operand:<VEL> 2 "general_operand")))]
  "TARGET_SVE"
  {
    rtx amount;
    if (CONST_INT_P (operands[2]))
      {
	amount = gen_const_vec_duplicate (<MODE>mode, operands[2]);
	if (!aarch64_sve_<lr>shift_operand (operands[2], <MODE>mode))
	  amount = force_reg (<MODE>mode, amount);
      }
    else
      {
	amount = gen_reg_rtx (<MODE>mode);
	emit_insn (gen_vec_duplicate<mode> (amount,
					    convert_to_mode (<VEL>mode,
							     operands[2], 0)));
      }
    emit_insn (gen_v<optab><mode>3 (operands[0], operands[1], amount));
    DONE;
  }
)

;; Test all bits of operand 1.  Operand 0 is a GP that is known to hold PTRUE.
;;
;; Using UNSPEC_PTEST_PTRUE allows combine patterns to assume that the GP
;; is a PTRUE even if the optimizers haven't yet been able to propagate
;; the constant.  We would use a separate unspec code for PTESTs involving
;; GPs that might not be PTRUEs.
(define_insn "ptest_ptrue<mode>"
  [(set (reg:CC CC_REGNUM)
	(compare:CC
	  (unspec:SI [(match_operand:PRED_ALL 0 "register_operand" "Upa")
		      (match_operand:PRED_ALL 1 "register_operand" "Upa")]
		     UNSPEC_PTEST_PTRUE)
	  (const_int 0)))]
  "TARGET_SVE"
  "ptest\t%0, %1.b"
)

;; Set element I of the result if operand1 + J < operand2 for all J in [0, I].
;; with the comparison being unsigned.
(define_insn "while_ult<GPI:mode><PRED_ALL:mode>"
  [(set (match_operand:PRED_ALL 0 "register_operand" "=Upa")
	(unspec:PRED_ALL [(match_operand:GPI 1 "aarch64_reg_or_zero" "rZ")
			  (match_operand:GPI 2 "aarch64_reg_or_zero" "rZ")]
			 UNSPEC_WHILE_LO))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_SVE"
  "whilelo\t%0.<PRED_ALL:Vetype>, %<w>1, %<w>2"
)

;; WHILELO sets the flags in the same way as a PTEST with a PTRUE GP.
;; Handle the case in which both results are useful.  The GP operand
;; to the PTEST isn't needed, so we allow it to be anything.
(define_insn_and_split "while_ult<GPI:mode><PRED_ALL:mode>_cc"
  [(set (reg:CC CC_REGNUM)
	(compare:CC
	  (unspec:SI [(match_operand:PRED_ALL 1)
		      (unspec:PRED_ALL
			[(match_operand:GPI 2 "aarch64_reg_or_zero" "rZ")
			 (match_operand:GPI 3 "aarch64_reg_or_zero" "rZ")]
			UNSPEC_WHILE_LO)]
		     UNSPEC_PTEST_PTRUE)
	  (const_int 0)))
   (set (match_operand:PRED_ALL 0 "register_operand" "=Upa")
	(unspec:PRED_ALL [(match_dup 2)
			  (match_dup 3)]
			 UNSPEC_WHILE_LO))]
  "TARGET_SVE"
  "whilelo\t%0.<PRED_ALL:Vetype>, %<w>2, %<w>3"
  ;; Force the compiler to drop the unused predicate operand, so that we
  ;; don't have an unnecessary PTRUE.
  "&& !CONSTANT_P (operands[1])"
  [(const_int 0)]
  {
    emit_insn (gen_while_ult<GPI:mode><PRED_ALL:mode>_cc
	       (operands[0], CONSTM1_RTX (<MODE>mode),
		operands[2], operands[3]));
    DONE;
  }
)

;; Integer comparisons predicated with a PTRUE.
(define_insn "*cmp<cmp_op><mode>"
  [(set (match_operand:<VPRED> 0 "register_operand" "=Upa, Upa")
	(unspec:<VPRED>
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl")
	   (SVE_INT_CMP:<VPRED>
	     (match_operand:SVE_I 2 "register_operand" "w, w")
	     (match_operand:SVE_I 3 "aarch64_sve_cmp_<sve_imm_con>_operand" "<sve_imm_con>, w"))]
	  UNSPEC_MERGE_PTRUE))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_SVE"
  "@
   cmp<cmp_op>\t%0.<Vetype>, %1/z, %2.<Vetype>, #%3
   cmp<cmp_op>\t%0.<Vetype>, %1/z, %2.<Vetype>, %3.<Vetype>"
)

;; Integer comparisons predicated with a PTRUE in which only the flags result
;; is interesting.
(define_insn "*cmp<cmp_op><mode>_ptest"
  [(set (reg:CC CC_REGNUM)
	(compare:CC
	  (unspec:SI
	    [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl")
	     (unspec:<VPRED>
	       [(match_dup 1)
		(SVE_INT_CMP:<VPRED>
		  (match_operand:SVE_I 2 "register_operand" "w, w")
		  (match_operand:SVE_I 3 "aarch64_sve_cmp_<sve_imm_con>_operand" "<sve_imm_con>, w"))]
	       UNSPEC_MERGE_PTRUE)]
	    UNSPEC_PTEST_PTRUE)
	  (const_int 0)))
   (clobber (match_scratch:<VPRED> 0 "=Upa, Upa"))]
  "TARGET_SVE"
  "@
   cmp<cmp_op>\t%0.<Vetype>, %1/z, %2.<Vetype>, #%3
   cmp<cmp_op>\t%0.<Vetype>, %1/z, %2.<Vetype>, %3.<Vetype>"
)

;; Integer comparisons predicated with a PTRUE in which both the flag and
;; predicate results are interesting.
(define_insn "*cmp<cmp_op><mode>_cc"
  [(set (reg:CC CC_REGNUM)
	(compare:CC
	  (unspec:SI
	    [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl")
	     (unspec:<VPRED>
	       [(match_dup 1)
		(SVE_INT_CMP:<VPRED>
		  (match_operand:SVE_I 2 "register_operand" "w, w")
		  (match_operand:SVE_I 3 "aarch64_sve_cmp_<sve_imm_con>_operand" "<sve_imm_con>, w"))]
	       UNSPEC_MERGE_PTRUE)]
	    UNSPEC_PTEST_PTRUE)
	  (const_int 0)))
   (set (match_operand:<VPRED> 0 "register_operand" "=Upa, Upa")
	(unspec:<VPRED>
	  [(match_dup 1)
	   (SVE_INT_CMP:<VPRED>
	     (match_dup 2)
	     (match_dup 3))]
	  UNSPEC_MERGE_PTRUE))]
  "TARGET_SVE"
  "@
   cmp<cmp_op>\t%0.<Vetype>, %1/z, %2.<Vetype>, #%3
   cmp<cmp_op>\t%0.<Vetype>, %1/z, %2.<Vetype>, %3.<Vetype>"
)

;; Predicated integer comparisons, formed by combining a PTRUE-predicated
;; comparison with an AND.  Split the instruction into its preferred form
;; (below) at the earliest opportunity, in order to get rid of the
;; redundant operand 1.
(define_insn_and_split "*pred_cmp<cmp_op><mode>_combine"
  [(set (match_operand:<VPRED> 0 "register_operand" "=Upa, Upa")
       (and:<VPRED>
         (unspec:<VPRED>
           [(match_operand:<VPRED> 1)
            (SVE_INT_CMP:<VPRED>
              (match_operand:SVE_I 2 "register_operand" "w, w")
              (match_operand:SVE_I 3 "aarch64_sve_cmp_<sve_imm_con>_operand" "<sve_imm_con>, w"))]
           UNSPEC_MERGE_PTRUE)
         (match_operand:<VPRED> 4 "register_operand" "Upl, Upl")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_SVE"
  "#"
  "&& 1"
  [(parallel
     [(set (match_dup 0)
          (and:<VPRED>
            (SVE_INT_CMP:<VPRED>
              (match_dup 2)
              (match_dup 3))
            (match_dup 4)))
      (clobber (reg:CC CC_REGNUM))])]
)

;; Predicated integer comparisons.
(define_insn "*pred_cmp<cmp_op><mode>"
  [(set (match_operand:<VPRED> 0 "register_operand" "=Upa, Upa")
	(and:<VPRED>
	  (SVE_INT_CMP:<VPRED>
	    (match_operand:SVE_I 2 "register_operand" "w, w")
	    (match_operand:SVE_I 3 "aarch64_sve_cmp_<sve_imm_con>_operand" "<sve_imm_con>, w"))
	  (match_operand:<VPRED> 1 "register_operand" "Upl, Upl")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_SVE"
  "@
   cmp<cmp_op>\t%0.<Vetype>, %1/z, %2.<Vetype>, #%3
   cmp<cmp_op>\t%0.<Vetype>, %1/z, %2.<Vetype>, %3.<Vetype>"
)

;; Floating-point comparisons predicated with a PTRUE.
(define_insn "*fcm<cmp_op><mode>"
  [(set (match_operand:<VPRED> 0 "register_operand" "=Upa, Upa")
	(unspec:<VPRED>
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl")
	   (SVE_FP_CMP:<VPRED>
	     (match_operand:SVE_F 2 "register_operand" "w, w")
	     (match_operand:SVE_F 3 "aarch64_simd_reg_or_zero" "Dz, w"))]
	  UNSPEC_MERGE_PTRUE))]
  "TARGET_SVE"
  "@
   fcm<cmp_op>\t%0.<Vetype>, %1/z, %2.<Vetype>, #0.0
   fcm<cmp_op>\t%0.<Vetype>, %1/z, %2.<Vetype>, %3.<Vetype>"
)

(define_insn "*fcmuo<mode>"
  [(set (match_operand:<VPRED> 0 "register_operand" "=Upa")
	(unspec:<VPRED>
	  [(match_operand:<VPRED> 1 "register_operand" "Upl")
	   (unordered:<VPRED>
	     (match_operand:SVE_F 2 "register_operand" "w")
	     (match_operand:SVE_F 3 "register_operand" "w"))]
	  UNSPEC_MERGE_PTRUE))]
  "TARGET_SVE"
  "fcmuo\t%0.<Vetype>, %1/z, %2.<Vetype>, %3.<Vetype>"
)

;; Floating-point comparisons predicated on a PTRUE, with the results ANDed
;; with another predicate P.  This does not have the same trapping behavior
;; as predicating the comparison itself on P, but it's a legitimate fold,
;; since we can drop any potentially-trapping operations whose results
;; are not needed.
;;
;; Split the instruction into its preferred form (below) at the earliest
;; opportunity, in order to get rid of the redundant operand 1.
(define_insn_and_split "*fcm<cmp_op><mode>_and_combine"
  [(set (match_operand:<VPRED> 0 "register_operand" "=Upa, Upa")
	(and:<VPRED>
	  (unspec:<VPRED>
	    [(match_operand:<VPRED> 1)
	     (SVE_FP_CMP
	       (match_operand:SVE_F 2 "register_operand" "w, w")
	       (match_operand:SVE_F 3 "aarch64_simd_reg_or_zero" "Dz, w"))]
	    UNSPEC_MERGE_PTRUE)
	  (match_operand:<VPRED> 4 "register_operand" "Upl, Upl")))]
  "TARGET_SVE"
  "#"
  "&& 1"
  [(set (match_dup 0)
	(and:<VPRED>
	  (SVE_FP_CMP:<VPRED>
	    (match_dup 2)
	    (match_dup 3))
	  (match_dup 4)))]
)

(define_insn_and_split "*fcmuo<mode>_and_combine"
  [(set (match_operand:<VPRED> 0 "register_operand" "=Upa")
	(and:<VPRED>
	  (unspec:<VPRED>
	    [(match_operand:<VPRED> 1)
	     (unordered
	       (match_operand:SVE_F 2 "register_operand" "w")
	       (match_operand:SVE_F 3 "register_operand" "w"))]
	    UNSPEC_MERGE_PTRUE)
	  (match_operand:<VPRED> 4 "register_operand" "Upl")))]
  "TARGET_SVE"
  "#"
  "&& 1"
  [(set (match_dup 0)
	(and:<VPRED>
	  (unordered:<VPRED>
	    (match_dup 2)
	    (match_dup 3))
	  (match_dup 4)))]
)

;; Unpredicated floating-point comparisons, with the results ANDed
;; with another predicate.  This is a valid fold for the same reasons
;; as above.
(define_insn "*fcm<cmp_op><mode>_and"
  [(set (match_operand:<VPRED> 0 "register_operand" "=Upa, Upa")
	(and:<VPRED>
	  (SVE_FP_CMP:<VPRED>
	    (match_operand:SVE_F 2 "register_operand" "w, w")
	    (match_operand:SVE_F 3 "aarch64_simd_reg_or_zero" "Dz, w"))
	  (match_operand:<VPRED> 1 "register_operand" "Upl, Upl")))]
  "TARGET_SVE"
  "@
   fcm<cmp_op>\t%0.<Vetype>, %1/z, %2.<Vetype>, #0.0
   fcm<cmp_op>\t%0.<Vetype>, %1/z, %2.<Vetype>, %3.<Vetype>"
)

(define_insn "*fcmuo<mode>_and"
  [(set (match_operand:<VPRED> 0 "register_operand" "=Upa")
	(and:<VPRED>
	  (unordered:<VPRED>
	    (match_operand:SVE_F 2 "register_operand" "w")
	    (match_operand:SVE_F 3 "register_operand" "w"))
	  (match_operand:<VPRED> 1 "register_operand" "Upl")))]
  "TARGET_SVE"
  "fcmuo\t%0.<Vetype>, %1/z, %2.<Vetype>, %3.<Vetype>"
)

;; Predicated floating-point comparisons.  We don't need a version
;; of this for unordered comparisons.
(define_insn "*pred_fcm<cmp_op><mode>"
  [(set (match_operand:<VPRED> 0 "register_operand" "=Upa, Upa")
	(unspec:<VPRED>
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl")
	   (match_operand:SVE_F 2 "register_operand" "w, w")
	   (match_operand:SVE_F 3 "aarch64_simd_reg_or_zero" "Dz, w")]
	  SVE_COND_FP_CMP))]
  "TARGET_SVE"
  "@
   fcm<cmp_op>\t%0.<Vetype>, %1/z, %2.<Vetype>, #0.0
   fcm<cmp_op>\t%0.<Vetype>, %1/z, %2.<Vetype>, %3.<Vetype>"
)

;; vcond_mask operand order: true, false, mask
;; UNSPEC_SEL operand order: mask, true, false (as for VEC_COND_EXPR)
;; SEL operand order:        mask, true, false
(define_insn "vcond_mask_<mode><vpred>"
  [(set (match_operand:SVE_ALL 0 "register_operand" "=w")
	(unspec:SVE_ALL
	  [(match_operand:<VPRED> 3 "register_operand" "Upa")
	   (match_operand:SVE_ALL 1 "register_operand" "w")
	   (match_operand:SVE_ALL 2 "register_operand" "w")]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  "sel\t%0.<Vetype>, %3, %1.<Vetype>, %2.<Vetype>"
)

;; Selects between a duplicated immediate and zero.
(define_insn "aarch64_sve_dup<mode>_const"
  [(set (match_operand:SVE_I 0 "register_operand" "=w")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand" "Upl")
	   (match_operand:SVE_I 2 "aarch64_sve_dup_immediate")
	   (match_operand:SVE_I 3 "aarch64_simd_imm_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  "mov\t%0.<Vetype>, %1/z, #%2"
)

;; Integer (signed) vcond.  Don't enforce an immediate range here, since it
;; depends on the comparison; leave it to aarch64_expand_sve_vcond instead.
(define_expand "vcond<mode><v_int_equiv>"
  [(set (match_operand:SVE_ALL 0 "register_operand")
	(if_then_else:SVE_ALL
	  (match_operator 3 "comparison_operator"
	    [(match_operand:<V_INT_EQUIV> 4 "register_operand")
	     (match_operand:<V_INT_EQUIV> 5 "nonmemory_operand")])
	  (match_operand:SVE_ALL 1 "register_operand")
	  (match_operand:SVE_ALL 2 "register_operand")))]
  "TARGET_SVE"
  {
    aarch64_expand_sve_vcond (<MODE>mode, <V_INT_EQUIV>mode, operands);
    DONE;
  }
)

;; Integer vcondu.  Don't enforce an immediate range here, since it
;; depends on the comparison; leave it to aarch64_expand_sve_vcond instead.
(define_expand "vcondu<mode><v_int_equiv>"
  [(set (match_operand:SVE_ALL 0 "register_operand")
	(if_then_else:SVE_ALL
	  (match_operator 3 "comparison_operator"
	    [(match_operand:<V_INT_EQUIV> 4 "register_operand")
	     (match_operand:<V_INT_EQUIV> 5 "nonmemory_operand")])
	  (match_operand:SVE_ALL 1 "register_operand")
	  (match_operand:SVE_ALL 2 "register_operand")))]
  "TARGET_SVE"
  {
    aarch64_expand_sve_vcond (<MODE>mode, <V_INT_EQUIV>mode, operands);
    DONE;
  }
)

;; Floating-point vcond.  All comparisons except FCMUO allow a zero
;; operand; aarch64_expand_sve_vcond handles the case of an FCMUO
;; with zero.
(define_expand "vcond<mode><v_fp_equiv>"
  [(set (match_operand:SVE_SD 0 "register_operand")
	(if_then_else:SVE_SD
	  (match_operator 3 "comparison_operator"
	    [(match_operand:<V_FP_EQUIV> 4 "register_operand")
	     (match_operand:<V_FP_EQUIV> 5 "aarch64_simd_reg_or_zero")])
	  (match_operand:SVE_SD 1 "register_operand")
	  (match_operand:SVE_SD 2 "register_operand")))]
  "TARGET_SVE"
  {
    aarch64_expand_sve_vcond (<MODE>mode, <V_FP_EQUIV>mode, operands);
    DONE;
  }
)

;; Signed integer comparisons.  Don't enforce an immediate range here, since
;; it depends on the comparison; leave it to aarch64_expand_sve_vec_cmp_int
;; instead.
(define_expand "vec_cmp<mode><vpred>"
  [(parallel
    [(set (match_operand:<VPRED> 0 "register_operand")
	  (match_operator:<VPRED> 1 "comparison_operator"
	    [(match_operand:SVE_I 2 "register_operand")
	     (match_operand:SVE_I 3 "nonmemory_operand")]))
     (clobber (reg:CC CC_REGNUM))])]
  "TARGET_SVE"
  {
    aarch64_expand_sve_vec_cmp_int (operands[0], GET_CODE (operands[1]),
				    operands[2], operands[3]);
    DONE;
  }
)

;; Unsigned integer comparisons.  Don't enforce an immediate range here, since
;; it depends on the comparison; leave it to aarch64_expand_sve_vec_cmp_int
;; instead.
(define_expand "vec_cmpu<mode><vpred>"
  [(parallel
    [(set (match_operand:<VPRED> 0 "register_operand")
	  (match_operator:<VPRED> 1 "comparison_operator"
	    [(match_operand:SVE_I 2 "register_operand")
	     (match_operand:SVE_I 3 "nonmemory_operand")]))
     (clobber (reg:CC CC_REGNUM))])]
  "TARGET_SVE"
  {
    aarch64_expand_sve_vec_cmp_int (operands[0], GET_CODE (operands[1]),
				    operands[2], operands[3]);
    DONE;
  }
)

;; Floating-point comparisons.  All comparisons except FCMUO allow a zero
;; operand; aarch64_expand_sve_vec_cmp_float handles the case of an FCMUO
;; with zero.
(define_expand "vec_cmp<mode><vpred>"
  [(set (match_operand:<VPRED> 0 "register_operand")
	(match_operator:<VPRED> 1 "comparison_operator"
	  [(match_operand:SVE_F 2 "register_operand")
	   (match_operand:SVE_F 3 "aarch64_simd_reg_or_zero")]))]
  "TARGET_SVE"
  {
    aarch64_expand_sve_vec_cmp_float (operands[0], GET_CODE (operands[1]),
				      operands[2], operands[3], false);
    DONE;
  }
)

;; Branch based on predicate equality or inequality.
(define_expand "cbranch<mode>4"
  [(set (pc)
	(if_then_else
	  (match_operator 0 "aarch64_equality_operator"
	    [(match_operand:PRED_ALL 1 "register_operand")
	     (match_operand:PRED_ALL 2 "aarch64_simd_reg_or_zero")])
	  (label_ref (match_operand 3 ""))
	  (pc)))]
  ""
  {
    rtx ptrue = force_reg (<MODE>mode, CONSTM1_RTX (<MODE>mode));
    rtx pred;
    if (operands[2] == CONST0_RTX (<MODE>mode))
      pred = operands[1];
    else
      {
	pred = gen_reg_rtx (<MODE>mode);
	emit_insn (gen_pred_xor<mode>3 (pred, ptrue, operands[1],
					operands[2]));
      }
    emit_insn (gen_ptest_ptrue<mode> (ptrue, pred));
    operands[1] = gen_rtx_REG (CCmode, CC_REGNUM);
    operands[2] = const0_rtx;
  }
)

;; Unpredicated integer MIN/MAX.
(define_expand "<su><maxmin><mode>3"
  [(set (match_operand:SVE_I 0 "register_operand")
	(unspec:SVE_I
	  [(match_dup 3)
	   (MAXMIN:SVE_I (match_operand:SVE_I 1 "register_operand")
			 (match_operand:SVE_I 2 "register_operand"))]
	  UNSPEC_MERGE_PTRUE))]
  "TARGET_SVE"
  {
    operands[3] = force_reg (<VPRED>mode, CONSTM1_RTX (<VPRED>mode));
  }
)

;; Integer MIN/MAX predicated with a PTRUE.
(define_insn "*<su><maxmin><mode>3"
  [(set (match_operand:SVE_I 0 "register_operand" "=w, ?&w")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl")
	   (MAXMIN:SVE_I (match_operand:SVE_I 2 "register_operand" "%0, w")
			 (match_operand:SVE_I 3 "register_operand" "w, w"))]
	  UNSPEC_MERGE_PTRUE))]
  "TARGET_SVE"
  "@
   <su><maxmin>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
   movprfx\t%0, %2\;<su><maxmin>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>"
  [(set_attr "movprfx" "*,yes")]
)

;; Unpredicated floating-point MIN/MAX.
(define_expand "<su><maxmin><mode>3"
  [(set (match_operand:SVE_F 0 "register_operand")
	(unspec:SVE_F
	  [(match_dup 3)
	   (FMAXMIN:SVE_F (match_operand:SVE_F 1 "register_operand")
			  (match_operand:SVE_F 2 "register_operand"))]
	  UNSPEC_MERGE_PTRUE))]
  "TARGET_SVE"
  {
    operands[3] = force_reg (<VPRED>mode, CONSTM1_RTX (<VPRED>mode));
  }
)

;; Floating-point MIN/MAX predicated with a PTRUE.
(define_insn "*<su><maxmin><mode>3"
  [(set (match_operand:SVE_F 0 "register_operand" "=w, ?&w")
	(unspec:SVE_F
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl")
	   (FMAXMIN:SVE_F (match_operand:SVE_F 2 "register_operand" "%0, w")
			  (match_operand:SVE_F 3 "register_operand" "w, w"))]
	  UNSPEC_MERGE_PTRUE))]
  "TARGET_SVE"
  "@
   f<maxmin>nm\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
   movprfx\t%0, %2\;f<maxmin>nm\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>"
  [(set_attr "movprfx" "*,yes")]
)

;; Unpredicated fmin/fmax.
(define_expand "<maxmin_uns><mode>3"
  [(set (match_operand:SVE_F 0 "register_operand")
	(unspec:SVE_F
	  [(match_dup 3)
	   (unspec:SVE_F [(match_operand:SVE_F 1 "register_operand")
			  (match_operand:SVE_F 2 "register_operand")]
			 FMAXMIN_UNS)]
	  UNSPEC_MERGE_PTRUE))]
  "TARGET_SVE"
  {
    operands[3] = force_reg (<VPRED>mode, CONSTM1_RTX (<VPRED>mode));
  }
)

;; fmin/fmax predicated with a PTRUE.
(define_insn "*<maxmin_uns><mode>3"
  [(set (match_operand:SVE_F 0 "register_operand" "=w, ?&w")
	(unspec:SVE_F
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl")
	   (unspec:SVE_F [(match_operand:SVE_F 2 "register_operand" "%0, w")
			  (match_operand:SVE_F 3 "register_operand" "w, w")]
			 FMAXMIN_UNS)]
	  UNSPEC_MERGE_PTRUE))]
  "TARGET_SVE"
  "@
   <maxmin_uns_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
   movprfx\t%0, %2\;<maxmin_uns_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>"
  [(set_attr "movprfx" "*,yes")]
)

;; Predicated integer operations with select.
(define_expand "cond_<optab><mode>"
  [(set (match_operand:SVE_I 0 "register_operand")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand")
	   (SVE_INT_BINARY:SVE_I
	     (match_operand:SVE_I 2 "register_operand")
	     (match_operand:SVE_I 3 "register_operand"))
	   (match_operand:SVE_I 4 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE"
)

(define_expand "cond_<optab><mode>"
  [(set (match_operand:SVE_SDI 0 "register_operand")
	(unspec:SVE_SDI
	  [(match_operand:<VPRED> 1 "register_operand")
	   (SVE_INT_BINARY_SD:SVE_SDI
	     (match_operand:SVE_SDI 2 "register_operand")
	     (match_operand:SVE_SDI 3 "register_operand"))
	   (match_operand:SVE_SDI 4 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE"
)

;; Predicated integer operations with select matching the output operand.
(define_insn "*cond_<optab><mode>_0"
  [(set (match_operand:SVE_I 0 "register_operand" "+w, w, ?&w")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl, Upl")
	   (SVE_INT_BINARY:SVE_I
	     (match_operand:SVE_I 2 "register_operand" "0, w, w")
	     (match_operand:SVE_I 3 "register_operand" "w, 0, w"))
	   (match_dup 0)]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  "@
   <sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
   <sve_int_op_rev>\t%0.<Vetype>, %1/m, %0.<Vetype>, %2.<Vetype>
   movprfx\t%0, %1/m, %2\;<sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>"
  [(set_attr "movprfx" "*,*,yes")]
)

(define_insn "*cond_<optab><mode>_0"
  [(set (match_operand:SVE_SDI 0 "register_operand" "+w, w, ?&w")
	(unspec:SVE_SDI
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl, Upl")
	   (SVE_INT_BINARY_SD:SVE_SDI
	     (match_operand:SVE_SDI 2 "register_operand" "0, w, w")
	     (match_operand:SVE_SDI 3 "register_operand" "w, 0, w"))
	   (match_dup 0)]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  "@
   <sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
   <sve_int_op_rev>\t%0.<Vetype>, %1/m, %0.<Vetype>, %2.<Vetype>
   movprfx\t%0, %1/m, %2\;<sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>"
  [(set_attr "movprfx" "*,*,yes")]
)

;; Predicated integer operations with select matching the first operand.
(define_insn "*cond_<optab><mode>_2"
  [(set (match_operand:SVE_I 0 "register_operand" "=w, ?&w")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl")
	   (SVE_INT_BINARY:SVE_I
	     (match_operand:SVE_I 2 "register_operand" "0, w")
	     (match_operand:SVE_I 3 "register_operand" "w, w"))
	   (match_dup 2)]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  "@
   <sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
   movprfx\t%0, %2\;<sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>"
  [(set_attr "movprfx" "*,yes")]
)

(define_insn "*cond_<optab><mode>_2"
  [(set (match_operand:SVE_SDI 0 "register_operand" "=w, ?&w")
	(unspec:SVE_SDI
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl")
	   (SVE_INT_BINARY_SD:SVE_SDI
	     (match_operand:SVE_SDI 2 "register_operand" "0, w")
	     (match_operand:SVE_SDI 3 "register_operand" "w, w"))
	   (match_dup 2)]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  "@
   <sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
   movprfx\t%0, %2\;<sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>"
  [(set_attr "movprfx" "*,yes")]
)

;; Predicated integer operations with select matching the second operand.
(define_insn "*cond_<optab><mode>_3"
  [(set (match_operand:SVE_I 0 "register_operand" "=w, ?&w")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl")
	   (SVE_INT_BINARY:SVE_I
	     (match_operand:SVE_I 2 "register_operand" "w, w")
	     (match_operand:SVE_I 3 "register_operand" "0, w"))
	   (match_dup 3)]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  "@
   <sve_int_op_rev>\t%0.<Vetype>, %1/m, %0.<Vetype>, %2.<Vetype>
   movprfx\t%0, %3\;<sve_int_op_rev>\t%0.<Vetype>, %1/m, %0.<Vetype>, %2.<Vetype>"
  [(set_attr "movprfx" "*,yes")]
)

(define_insn "*cond_<optab><mode>_3"
  [(set (match_operand:SVE_SDI 0 "register_operand" "=w, ?&w")
	(unspec:SVE_SDI
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl")
	   (SVE_INT_BINARY_SD:SVE_SDI
	     (match_operand:SVE_SDI 2 "register_operand" "w, w")
	     (match_operand:SVE_SDI 3 "register_operand" "0, w"))
	   (match_dup 3)]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  "@
   <sve_int_op_rev>\t%0.<Vetype>, %1/m, %0.<Vetype>, %2.<Vetype>
   movprfx\t%0, %3\;<sve_int_op_rev>\t%0.<Vetype>, %1/m, %0.<Vetype>, %2.<Vetype>"
  [(set_attr "movprfx" "*,yes")]
)

;; Predicated integer operations with select matching zero.
(define_insn "*cond_<optab><mode>_z"
  [(set (match_operand:SVE_I 0 "register_operand" "=&w")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand" "Upl")
	   (SVE_INT_BINARY:SVE_I
	     (match_operand:SVE_I 2 "register_operand" "w")
	     (match_operand:SVE_I 3 "register_operand" "w"))
	   (match_operand:SVE_I 4 "aarch64_simd_imm_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  "movprfx\t%0.<Vetype>, %1/z, %2.<Vetype>\;<sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>"
  [(set_attr "movprfx" "yes")]
)

(define_insn "*cond_<optab><mode>_z"
  [(set (match_operand:SVE_SDI 0 "register_operand" "=&w")
	(unspec:SVE_SDI
	  [(match_operand:<VPRED> 1 "register_operand" "Upl")
	   (SVE_INT_BINARY_SD:SVE_SDI
	     (match_operand:SVE_SDI 2 "register_operand" "w")
	     (match_operand:SVE_SDI 3 "register_operand" "w"))
	   (match_operand:SVE_SDI 4 "aarch64_simd_imm_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  "movprfx\t%0.<Vetype>, %1/z, %2.<Vetype>\;<sve_int_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>"
  [(set_attr "movprfx" "yes")]
)

;; Synthetic predications with select unmatched.
(define_insn "*cond_<optab><mode>_any"
  [(set (match_operand:SVE_I 0 "register_operand" "=&w")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand" "Upl")
	   (SVE_INT_BINARY:SVE_I
	     (match_operand:SVE_I 2 "register_operand" "w")
	     (match_operand:SVE_I 3 "register_operand" "w"))
	   (match_operand:SVE_I 4 "register_operand"   "w")]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  "#"
)

(define_insn "*cond_<optab><mode>_any"
  [(set (match_operand:SVE_SDI 0 "register_operand" "=&w")
	(unspec:SVE_SDI
	  [(match_operand:<VPRED> 1 "register_operand" "Upl")
	   (SVE_INT_BINARY_SD:SVE_I
	     (match_operand:SVE_SDI 2 "register_operand" "w")
	     (match_operand:SVE_SDI 3 "register_operand" "w"))
	   (match_operand:SVE_SDI 4 "register_operand"   "w")]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  "#"
)

(define_split
  [(set (match_operand:SVE_I 0 "register_operand")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand")
	   (match_operator:SVE_I 5 "aarch64_sve_any_binary_operator"
	     [(match_operand:SVE_I 2 "register_operand")
	      (match_operand:SVE_I 3 "register_operand")])
	   (match_operand:SVE_I 4 "register_operand")]
	  UNSPEC_SEL))]
  "TARGET_SVE && reload_completed
   && !(rtx_equal_p (operands[0], operands[4])
        || rtx_equal_p (operands[2], operands[4])
        || rtx_equal_p (operands[3], operands[4]))"
  ; Not matchable by any one insn or movprfx insn.  We need a separate select.
  [(set (match_dup 0)
	(unspec:SVE_I [(match_dup 1) (match_dup 2) (match_dup 4)]
                      UNSPEC_SEL))
   (set (match_dup 0)
	(unspec:SVE_I
	  [(match_dup 1)
	   (match_op_dup 5 [(match_dup 0) (match_dup 3)])
           (match_dup 0)]
	  UNSPEC_SEL))]
)

;; Set operand 0 to the last active element in operand 3, or to tied
;; operand 1 if no elements are active.
(define_insn "fold_extract_last_<mode>"
  [(set (match_operand:<VEL> 0 "register_operand" "=r, w")
	(unspec:<VEL>
	  [(match_operand:<VEL> 1 "register_operand" "0, 0")
	   (match_operand:<VPRED> 2 "register_operand" "Upl, Upl")
	   (match_operand:SVE_ALL 3 "register_operand" "w, w")]
	  UNSPEC_CLASTB))]
  "TARGET_SVE"
  "@
   clastb\t%<vwcore>0, %2, %<vwcore>0, %3.<Vetype>
   clastb\t%<vw>0, %2, %<vw>0, %3.<Vetype>"
)

;; Unpredicated integer add reduction.
(define_expand "reduc_plus_scal_<mode>"
  [(set (match_operand:<VEL> 0 "register_operand")
	(unspec:<VEL> [(match_dup 2)
		       (match_operand:SVE_I 1 "register_operand")]
		      UNSPEC_ADDV))]
  "TARGET_SVE"
  {
    operands[2] = force_reg (<VPRED>mode, CONSTM1_RTX (<VPRED>mode));
  }
)

;; Predicated integer add reduction.  The result is always 64-bits.
(define_insn "*reduc_plus_scal_<mode>"
  [(set (match_operand:<VEL> 0 "register_operand" "=w")
	(unspec:<VEL> [(match_operand:<VPRED> 1 "register_operand" "Upl")
		       (match_operand:SVE_I 2 "register_operand" "w")]
		      UNSPEC_ADDV))]
  "TARGET_SVE"
  "uaddv\t%d0, %1, %2.<Vetype>"
)

;; Unpredicated floating-point add reduction.
(define_expand "reduc_plus_scal_<mode>"
  [(set (match_operand:<VEL> 0 "register_operand")
	(unspec:<VEL> [(match_dup 2)
		       (match_operand:SVE_F 1 "register_operand")]
		      UNSPEC_FADDV))]
  "TARGET_SVE"
  {
    operands[2] = force_reg (<VPRED>mode, CONSTM1_RTX (<VPRED>mode));
  }
)

;; Predicated floating-point add reduction.
(define_insn "*reduc_plus_scal_<mode>"
  [(set (match_operand:<VEL> 0 "register_operand" "=w")
	(unspec:<VEL> [(match_operand:<VPRED> 1 "register_operand" "Upl")
		       (match_operand:SVE_F 2 "register_operand" "w")]
		      UNSPEC_FADDV))]
  "TARGET_SVE"
  "faddv\t%<Vetype>0, %1, %2.<Vetype>"
)

;; Unpredicated integer MIN/MAX reduction.
(define_expand "reduc_<maxmin_uns>_scal_<mode>"
  [(set (match_operand:<VEL> 0 "register_operand")
	(unspec:<VEL> [(match_dup 2)
		       (match_operand:SVE_I 1 "register_operand")]
		      MAXMINV))]
  "TARGET_SVE"
  {
    operands[2] = force_reg (<VPRED>mode, CONSTM1_RTX (<VPRED>mode));
  }
)

;; Predicated integer MIN/MAX reduction.
(define_insn "*reduc_<maxmin_uns>_scal_<mode>"
  [(set (match_operand:<VEL> 0 "register_operand" "=w")
	(unspec:<VEL> [(match_operand:<VPRED> 1 "register_operand" "Upl")
		       (match_operand:SVE_I 2 "register_operand" "w")]
		      MAXMINV))]
  "TARGET_SVE"
  "<maxmin_uns_op>v\t%<Vetype>0, %1, %2.<Vetype>"
)

;; Unpredicated floating-point MIN/MAX reduction.
(define_expand "reduc_<maxmin_uns>_scal_<mode>"
  [(set (match_operand:<VEL> 0 "register_operand")
	(unspec:<VEL> [(match_dup 2)
		       (match_operand:SVE_F 1 "register_operand")]
		      FMAXMINV))]
  "TARGET_SVE"
  {
    operands[2] = force_reg (<VPRED>mode, CONSTM1_RTX (<VPRED>mode));
  }
)

;; Predicated floating-point MIN/MAX reduction.
(define_insn "*reduc_<maxmin_uns>_scal_<mode>"
  [(set (match_operand:<VEL> 0 "register_operand" "=w")
	(unspec:<VEL> [(match_operand:<VPRED> 1 "register_operand" "Upl")
		       (match_operand:SVE_F 2 "register_operand" "w")]
		      FMAXMINV))]
  "TARGET_SVE"
  "<maxmin_uns_op>v\t%<Vetype>0, %1, %2.<Vetype>"
)

(define_expand "reduc_<optab>_scal_<mode>"
  [(set (match_operand:<VEL> 0 "register_operand")
	(unspec:<VEL> [(match_dup 2)
		       (match_operand:SVE_I 1 "register_operand")]
		      BITWISEV))]
  "TARGET_SVE"
  {
    operands[2] = force_reg (<VPRED>mode, CONSTM1_RTX (<VPRED>mode));
  }
)

(define_insn "*reduc_<optab>_scal_<mode>"
  [(set (match_operand:<VEL> 0 "register_operand" "=w")
	(unspec:<VEL> [(match_operand:<VPRED> 1 "register_operand" "Upl")
		       (match_operand:SVE_I 2 "register_operand" "w")]
		      BITWISEV))]
  "TARGET_SVE"
  "<bit_reduc_op>\t%<Vetype>0, %1, %2.<Vetype>"
)

;; Unpredicated in-order FP reductions.
(define_expand "fold_left_plus_<mode>"
  [(set (match_operand:<VEL> 0 "register_operand")
	(unspec:<VEL> [(match_dup 3)
		       (match_operand:<VEL> 1 "register_operand")
		       (match_operand:SVE_F 2 "register_operand")]
		      UNSPEC_FADDA))]
  "TARGET_SVE"
  {
    operands[3] = force_reg (<VPRED>mode, CONSTM1_RTX (<VPRED>mode));
  }
)

;; In-order FP reductions predicated with PTRUE.
(define_insn "*fold_left_plus_<mode>"
  [(set (match_operand:<VEL> 0 "register_operand" "=w")
	(unspec:<VEL> [(match_operand:<VPRED> 1 "register_operand" "Upl")
		       (match_operand:<VEL> 2 "register_operand" "0")
		       (match_operand:SVE_F 3 "register_operand" "w")]
		      UNSPEC_FADDA))]
  "TARGET_SVE"
  "fadda\t%<Vetype>0, %1, %<Vetype>0, %3.<Vetype>"
)

;; Predicated form of the above in-order reduction.
(define_insn "*pred_fold_left_plus_<mode>"
  [(set (match_operand:<VEL> 0 "register_operand" "=w")
	(unspec:<VEL>
	  [(match_operand:<VEL> 1 "register_operand" "0")
	   (unspec:SVE_F
	     [(match_operand:<VPRED> 2 "register_operand" "Upl")
	      (match_operand:SVE_F 3 "register_operand" "w")
	      (match_operand:SVE_F 4 "aarch64_simd_imm_zero")]
	     UNSPEC_SEL)]
	  UNSPEC_FADDA))]
  "TARGET_SVE"
  "fadda\t%<Vetype>0, %2, %<Vetype>0, %3.<Vetype>"
)

;; Unpredicated floating-point addition.
(define_expand "add<mode>3"
  [(set (match_operand:SVE_F 0 "register_operand")
	(unspec:SVE_F
	  [(match_dup 3)
	   (plus:SVE_F
	     (match_operand:SVE_F 1 "register_operand")
	     (match_operand:SVE_F 2 "aarch64_sve_float_arith_with_sub_operand"))]
	  UNSPEC_MERGE_PTRUE))]
  "TARGET_SVE"
  {
    operands[3] = force_reg (<VPRED>mode, CONSTM1_RTX (<VPRED>mode));
  }
)

;; Floating-point addition predicated with a PTRUE.
(define_insn "*add<mode>3"
  [(set (match_operand:SVE_F 0 "register_operand" "=w, w, w")
	(unspec:SVE_F
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl, Upl")
	   (plus:SVE_F
	      (match_operand:SVE_F 2 "register_operand" "%0, 0, w")
	      (match_operand:SVE_F 3 "aarch64_sve_float_arith_with_sub_operand" "vsA, vsN, w"))]
	  UNSPEC_MERGE_PTRUE))]
  "TARGET_SVE"
  "@
   fadd\t%0.<Vetype>, %1/m, %0.<Vetype>, #%3
   fsub\t%0.<Vetype>, %1/m, %0.<Vetype>, #%N3
   fadd\t%0.<Vetype>, %2.<Vetype>, %3.<Vetype>"
)

;; Unpredicated floating-point subtraction.
(define_expand "sub<mode>3"
  [(set (match_operand:SVE_F 0 "register_operand")
	(unspec:SVE_F
	  [(match_dup 3)
	   (minus:SVE_F
	     (match_operand:SVE_F 1 "aarch64_sve_float_arith_operand")
	     (match_operand:SVE_F 2 "register_operand"))]
	  UNSPEC_MERGE_PTRUE))]
  "TARGET_SVE"
  {
    operands[3] = force_reg (<VPRED>mode, CONSTM1_RTX (<VPRED>mode));
  }
)

;; Floating-point subtraction predicated with a PTRUE.
(define_insn "*sub<mode>3"
  [(set (match_operand:SVE_F 0 "register_operand" "=w, w, w, w")
	(unspec:SVE_F
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl, Upl, Upl")
	   (minus:SVE_F
	     (match_operand:SVE_F 2 "aarch64_sve_float_arith_operand" "0, 0, vsA, w")
	     (match_operand:SVE_F 3 "aarch64_sve_float_arith_with_sub_operand" "vsA, vsN, 0, w"))]
	  UNSPEC_MERGE_PTRUE))]
  "TARGET_SVE
   && (register_operand (operands[2], <MODE>mode)
       || register_operand (operands[3], <MODE>mode))"
  "@
   fsub\t%0.<Vetype>, %1/m, %0.<Vetype>, #%3
   fadd\t%0.<Vetype>, %1/m, %0.<Vetype>, #%N3
   fsubr\t%0.<Vetype>, %1/m, %0.<Vetype>, #%2
   fsub\t%0.<Vetype>, %2.<Vetype>, %3.<Vetype>"
)

;; Unpredicated floating-point multiplication.
(define_expand "mul<mode>3"
  [(set (match_operand:SVE_F 0 "register_operand")
	(unspec:SVE_F
	  [(match_dup 3)
	   (mult:SVE_F
	     (match_operand:SVE_F 1 "register_operand")
	     (match_operand:SVE_F 2 "aarch64_sve_float_mul_operand"))]
	  UNSPEC_MERGE_PTRUE))]
  "TARGET_SVE"
  {
    operands[3] = force_reg (<VPRED>mode, CONSTM1_RTX (<VPRED>mode));
  }
)

;; Floating-point multiplication predicated with a PTRUE.
(define_insn "*mul<mode>3"
  [(set (match_operand:SVE_F 0 "register_operand" "=w, w")
	(unspec:SVE_F
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl")
	   (mult:SVE_F
	     (match_operand:SVE_F 2 "register_operand" "%0, w")
	     (match_operand:SVE_F 3 "aarch64_sve_float_mul_operand" "vsM, w"))]
	  UNSPEC_MERGE_PTRUE))]
  "TARGET_SVE"
  "@
   fmul\t%0.<Vetype>, %1/m, %0.<Vetype>, #%3
   fmul\t%0.<Vetype>, %2.<Vetype>, %3.<Vetype>"
)

;; Unpredicated fma (%0 = (%1 * %2) + %3).
(define_expand "fma<mode>4"
  [(set (match_operand:SVE_F 0 "register_operand")
	(unspec:SVE_F
	  [(match_dup 4)
	   (fma:SVE_F (match_operand:SVE_F 1 "register_operand")
		      (match_operand:SVE_F 2 "register_operand")
		      (match_operand:SVE_F 3 "register_operand"))]
	  UNSPEC_MERGE_PTRUE))]
  "TARGET_SVE"
  {
    operands[4] = force_reg (<VPRED>mode, CONSTM1_RTX (<VPRED>mode));
  }
)

;; fma predicated with a PTRUE.
(define_insn "*fma<mode>4"
  [(set (match_operand:SVE_F 0 "register_operand" "=w, w, ?&w")
	(unspec:SVE_F
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl, Upl")
	   (fma:SVE_F (match_operand:SVE_F 3 "register_operand" "%0, w, w")
		      (match_operand:SVE_F 4 "register_operand" "w, w, w")
		      (match_operand:SVE_F 2 "register_operand" "w, 0, w"))]
	  UNSPEC_MERGE_PTRUE))]
  "TARGET_SVE"
  "@
   fmad\t%0.<Vetype>, %1/m, %4.<Vetype>, %2.<Vetype>
   fmla\t%0.<Vetype>, %1/m, %3.<Vetype>, %4.<Vetype>
   movprfx\t%0, %2\;fmla\t%0.<Vetype>, %1/m, %3.<Vetype>, %4.<Vetype>"
  [(set_attr "movprfx" "*,*,yes")]
)

;; Unpredicated fnma (%0 = (-%1 * %2) + %3).
(define_expand "fnma<mode>4"
  [(set (match_operand:SVE_F 0 "register_operand")
	(unspec:SVE_F
	  [(match_dup 4)
	   (fma:SVE_F (neg:SVE_F
			(match_operand:SVE_F 1 "register_operand"))
		      (match_operand:SVE_F 2 "register_operand")
		      (match_operand:SVE_F 3 "register_operand"))]
	  UNSPEC_MERGE_PTRUE))]
  "TARGET_SVE"
  {
    operands[4] = force_reg (<VPRED>mode, CONSTM1_RTX (<VPRED>mode));
  }
)

;; fnma predicated with a PTRUE.
(define_insn "*fnma<mode>4"
  [(set (match_operand:SVE_F 0 "register_operand" "=w, w, ?&w")
	(unspec:SVE_F
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl, Upl")
	   (fma:SVE_F (neg:SVE_F
			(match_operand:SVE_F 3 "register_operand" "%0, w, w"))
		      (match_operand:SVE_F 4 "register_operand" "w, w, w")
		      (match_operand:SVE_F 2 "register_operand" "w, 0, w"))]
	  UNSPEC_MERGE_PTRUE))]
  "TARGET_SVE"
  "@
   fmsb\t%0.<Vetype>, %1/m, %4.<Vetype>, %2.<Vetype>
   fmls\t%0.<Vetype>, %1/m, %3.<Vetype>, %4.<Vetype>
   movprfx\t%0, %2\;fmls\t%0.<Vetype>, %1/m, %3.<Vetype>, %4.<Vetype>"
  [(set_attr "movprfx" "*,*,yes")]
)

;; Unpredicated fms (%0 = (%1 * %2) - %3).
(define_expand "fms<mode>4"
  [(set (match_operand:SVE_F 0 "register_operand")
	(unspec:SVE_F
	  [(match_dup 4)
	   (fma:SVE_F (match_operand:SVE_F 1 "register_operand")
		      (match_operand:SVE_F 2 "register_operand")
		      (neg:SVE_F
			(match_operand:SVE_F 3 "register_operand")))]
	  UNSPEC_MERGE_PTRUE))]
  "TARGET_SVE"
  {
    operands[4] = force_reg (<VPRED>mode, CONSTM1_RTX (<VPRED>mode));
  }
)

;; fms predicated with a PTRUE.
(define_insn "*fms<mode>4"
  [(set (match_operand:SVE_F 0 "register_operand" "=w, w, ?&w")
	(unspec:SVE_F
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl, Upl")
	   (fma:SVE_F (match_operand:SVE_F 3 "register_operand" "%0, w, w")
		      (match_operand:SVE_F 4 "register_operand" "w, w, w")
		      (neg:SVE_F
			(match_operand:SVE_F 2 "register_operand" "w, 0, w")))]
	  UNSPEC_MERGE_PTRUE))]
  "TARGET_SVE"
  "@
   fnmsb\t%0.<Vetype>, %1/m, %4.<Vetype>, %2.<Vetype>
   fnmls\t%0.<Vetype>, %1/m, %3.<Vetype>, %4.<Vetype>
   movprfx\t%0, %2\;fnmls\t%0.<Vetype>, %1/m, %3.<Vetype>, %4.<Vetype>"
  [(set_attr "movprfx" "*,*,yes")]
)

;; Unpredicated fnms (%0 = (-%1 * %2) - %3).
(define_expand "fnms<mode>4"
  [(set (match_operand:SVE_F 0 "register_operand")
	(unspec:SVE_F
	  [(match_dup 4)
	   (fma:SVE_F (neg:SVE_F
			(match_operand:SVE_F 1 "register_operand"))
		      (match_operand:SVE_F 2 "register_operand")
		      (neg:SVE_F
			(match_operand:SVE_F 3 "register_operand")))]
	  UNSPEC_MERGE_PTRUE))]
  "TARGET_SVE"
  {
    operands[4] = force_reg (<VPRED>mode, CONSTM1_RTX (<VPRED>mode));
  }
)

;; fnms predicated with a PTRUE.
(define_insn "*fnms<mode>4"
  [(set (match_operand:SVE_F 0 "register_operand" "=w, w, ?&w")
	(unspec:SVE_F
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl, Upl")
	   (fma:SVE_F (neg:SVE_F
			(match_operand:SVE_F 3 "register_operand" "%0, w, w"))
		      (match_operand:SVE_F 4 "register_operand" "w, w, w")
		      (neg:SVE_F
			(match_operand:SVE_F 2 "register_operand" "w, 0, w")))]
	  UNSPEC_MERGE_PTRUE))]
  "TARGET_SVE"
  "@
   fnmad\t%0.<Vetype>, %1/m, %4.<Vetype>, %2.<Vetype>
   fnmla\t%0.<Vetype>, %1/m, %3.<Vetype>, %4.<Vetype>
   movprfx\t%0, %2\;fnmla\t%0.<Vetype>, %1/m, %3.<Vetype>, %4.<Vetype>"
  [(set_attr "movprfx" "*,*,yes")]
)

;; Unpredicated floating-point division.
(define_expand "div<mode>3"
  [(set (match_operand:SVE_F 0 "register_operand")
	(unspec:SVE_F
	  [(match_dup 3)
	   (div:SVE_F (match_operand:SVE_F 1 "register_operand")
		      (match_operand:SVE_F 2 "register_operand"))]
	  UNSPEC_MERGE_PTRUE))]
  "TARGET_SVE"
  {
    operands[3] = force_reg (<VPRED>mode, CONSTM1_RTX (<VPRED>mode));
  }
)

;; Floating-point division predicated with a PTRUE.
(define_insn "*div<mode>3"
  [(set (match_operand:SVE_F 0 "register_operand" "=w, w, ?&w")
	(unspec:SVE_F
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl, Upl")
	   (div:SVE_F (match_operand:SVE_F 2 "register_operand" "0, w, w")
		      (match_operand:SVE_F 3 "register_operand" "w, 0, w"))]
	  UNSPEC_MERGE_PTRUE))]
  "TARGET_SVE"
  "@
   fdiv\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
   fdivr\t%0.<Vetype>, %1/m, %0.<Vetype>, %2.<Vetype>
   movprfx\t%0, %2\;fdiv\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>"
  [(set_attr "movprfx" "*,*,yes")]
)

;; Unpredicated FNEG, FABS and FSQRT.
(define_expand "<optab><mode>2"
  [(set (match_operand:SVE_F 0 "register_operand")
	(unspec:SVE_F
	  [(match_dup 2)
	   (SVE_FP_UNARY:SVE_F (match_operand:SVE_F 1 "register_operand"))]
	  UNSPEC_MERGE_PTRUE))]
  "TARGET_SVE"
  {
    operands[2] = force_reg (<VPRED>mode, CONSTM1_RTX (<VPRED>mode));
  }
)

;; FNEG, FABS and FSQRT predicated with a PTRUE.
(define_insn "*<optab><mode>2"
  [(set (match_operand:SVE_F 0 "register_operand" "=w")
	(unspec:SVE_F
	  [(match_operand:<VPRED> 1 "register_operand" "Upl")
	   (SVE_FP_UNARY:SVE_F (match_operand:SVE_F 2 "register_operand" "w"))]
	  UNSPEC_MERGE_PTRUE))]
  "TARGET_SVE"
  "<sve_fp_op>\t%0.<Vetype>, %1/m, %2.<Vetype>"
)

;; Unpredicated FRINTy.
(define_expand "<frint_pattern><mode>2"
  [(set (match_operand:SVE_F 0 "register_operand")
	(unspec:SVE_F
	  [(match_dup 2)
	   (unspec:SVE_F [(match_operand:SVE_F 1 "register_operand")]
			 FRINT)]
	  UNSPEC_MERGE_PTRUE))]
  "TARGET_SVE"
  {
    operands[2] = force_reg (<VPRED>mode, CONSTM1_RTX (<VPRED>mode));
  }
)

;; FRINTy predicated with a PTRUE.
(define_insn "*<frint_pattern><mode>2"
  [(set (match_operand:SVE_F 0 "register_operand" "=w")
	(unspec:SVE_F
	  [(match_operand:<VPRED> 1 "register_operand" "Upl")
	   (unspec:SVE_F [(match_operand:SVE_F 2 "register_operand" "w")]
			 FRINT)]
	  UNSPEC_MERGE_PTRUE))]
  "TARGET_SVE"
  "frint<frint_suffix>\t%0.<Vetype>, %1/m, %2.<Vetype>"
)

;; Unpredicated conversion of floats to integers of the same size (HF to HI,
;; SF to SI or DF to DI).
(define_expand "<fix_trunc_optab><mode><v_int_equiv>2"
  [(set (match_operand:<V_INT_EQUIV> 0 "register_operand")
	(unspec:<V_INT_EQUIV>
	  [(match_dup 2)
	   (FIXUORS:<V_INT_EQUIV>
	     (match_operand:SVE_F 1 "register_operand"))]
	  UNSPEC_MERGE_PTRUE))]
  "TARGET_SVE"
  {
    operands[2] = force_reg (<VPRED>mode, CONSTM1_RTX (<VPRED>mode));
  }
)

;; Conversion of SF to DI, SI or HI, predicated with a PTRUE.
(define_insn "*<fix_trunc_optab>v16hsf<mode>2"
  [(set (match_operand:SVE_HSDI 0 "register_operand" "=w")
	(unspec:SVE_HSDI
	  [(match_operand:<VPRED> 1 "register_operand" "Upl")
	   (FIXUORS:SVE_HSDI
	     (match_operand:VNx8HF 2 "register_operand" "w"))]
	  UNSPEC_MERGE_PTRUE))]
  "TARGET_SVE"
  "fcvtz<su>\t%0.<Vetype>, %1/m, %2.h"
)

;; Conversion of SF to DI or SI, predicated with a PTRUE.
(define_insn "*<fix_trunc_optab>vnx4sf<mode>2"
  [(set (match_operand:SVE_SDI 0 "register_operand" "=w")
	(unspec:SVE_SDI
	  [(match_operand:<VPRED> 1 "register_operand" "Upl")
	   (FIXUORS:SVE_SDI
	     (match_operand:VNx4SF 2 "register_operand" "w"))]
	  UNSPEC_MERGE_PTRUE))]
  "TARGET_SVE"
  "fcvtz<su>\t%0.<Vetype>, %1/m, %2.s"
)

;; Conversion of DF to DI or SI, predicated with a PTRUE.
(define_insn "*<fix_trunc_optab>vnx2df<mode>2"
  [(set (match_operand:SVE_SDI 0 "register_operand" "=w")
	(unspec:SVE_SDI
	  [(match_operand:VNx2BI 1 "register_operand" "Upl")
	   (FIXUORS:SVE_SDI
	     (match_operand:VNx2DF 2 "register_operand" "w"))]
	  UNSPEC_MERGE_PTRUE))]
  "TARGET_SVE"
  "fcvtz<su>\t%0.<Vetype>, %1/m, %2.d"
)

;; Unpredicated conversion of integers to floats of the same size
;; (HI to HF, SI to SF or DI to DF).
(define_expand "<optab><v_int_equiv><mode>2"
  [(set (match_operand:SVE_F 0 "register_operand")
	(unspec:SVE_F
	  [(match_dup 2)
	   (FLOATUORS:SVE_F
	     (match_operand:<V_INT_EQUIV> 1 "register_operand"))]
	  UNSPEC_MERGE_PTRUE))]
  "TARGET_SVE"
  {
    operands[2] = force_reg (<VPRED>mode, CONSTM1_RTX (<VPRED>mode));
  }
)

;; Conversion of DI, SI or HI to the same number of HFs, predicated
;; with a PTRUE.
(define_insn "*<optab><mode>vnx8hf2"
  [(set (match_operand:VNx8HF 0 "register_operand" "=w")
	(unspec:VNx8HF
	  [(match_operand:<VPRED> 1 "register_operand" "Upl")
	   (FLOATUORS:VNx8HF
	     (match_operand:SVE_HSDI 2 "register_operand" "w"))]
	  UNSPEC_MERGE_PTRUE))]
  "TARGET_SVE"
  "<su_optab>cvtf\t%0.h, %1/m, %2.<Vetype>"
)

;; Conversion of DI or SI to the same number of SFs, predicated with a PTRUE.
(define_insn "*<optab><mode>vnx4sf2"
  [(set (match_operand:VNx4SF 0 "register_operand" "=w")
	(unspec:VNx4SF
	  [(match_operand:<VPRED> 1 "register_operand" "Upl")
	   (FLOATUORS:VNx4SF
	     (match_operand:SVE_SDI 2 "register_operand" "w"))]
	  UNSPEC_MERGE_PTRUE))]
  "TARGET_SVE"
  "<su_optab>cvtf\t%0.s, %1/m, %2.<Vetype>"
)

;; Conversion of DI or SI to DF, predicated with a PTRUE.
(define_insn "aarch64_sve_<optab><mode>vnx2df2"
  [(set (match_operand:VNx2DF 0 "register_operand" "=w")
	(unspec:VNx2DF
	  [(match_operand:VNx2BI 1 "register_operand" "Upl")
	   (FLOATUORS:VNx2DF
	     (match_operand:SVE_SDI 2 "register_operand" "w"))]
	  UNSPEC_MERGE_PTRUE))]
  "TARGET_SVE"
  "<su_optab>cvtf\t%0.d, %1/m, %2.<Vetype>"
)

;; Conversion of DFs to the same number of SFs, or SFs to the same number
;; of HFs.
(define_insn "*trunc<Vwide><mode>2"
  [(set (match_operand:SVE_HSF 0 "register_operand" "=w")
	(unspec:SVE_HSF
	  [(match_operand:<VWIDE_PRED> 1 "register_operand" "Upl")
	   (unspec:SVE_HSF
	     [(match_operand:<VWIDE> 2 "register_operand" "w")]
	     UNSPEC_FLOAT_CONVERT)]
	  UNSPEC_MERGE_PTRUE))]
  "TARGET_SVE"
  "fcvt\t%0.<Vetype>, %1/m, %2.<Vewtype>"
)

;; Conversion of SFs to the same number of DFs, or HFs to the same number
;; of SFs.
(define_insn "aarch64_sve_extend<mode><Vwide>2"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
	(unspec:<VWIDE>
	  [(match_operand:<VWIDE_PRED> 1 "register_operand" "Upl")
	   (unspec:<VWIDE>
	     [(match_operand:SVE_HSF 2 "register_operand" "w")]
	     UNSPEC_FLOAT_CONVERT)]
	  UNSPEC_MERGE_PTRUE))]
  "TARGET_SVE"
  "fcvt\t%0.<Vewtype>, %1/m, %2.<Vetype>"
)

;; Unpack the low or high half of a predicate, where "high" refers to
;; the low-numbered lanes for big-endian and the high-numbered lanes
;; for little-endian.
(define_expand "vec_unpack<su>_<perm_hilo>_<mode>"
  [(match_operand:<VWIDE> 0 "register_operand")
   (unspec:<VWIDE> [(match_operand:PRED_BHS 1 "register_operand")]
		   UNPACK)]
  "TARGET_SVE"
  {
    emit_insn ((<hi_lanes_optab>
		? gen_aarch64_sve_punpkhi_<PRED_BHS:mode>
		: gen_aarch64_sve_punpklo_<PRED_BHS:mode>)
	       (operands[0], operands[1]));
    DONE;
  }
)

;; PUNPKHI and PUNPKLO.
(define_insn "aarch64_sve_punpk<perm_hilo>_<mode>"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=Upa")
	(unspec:<VWIDE> [(match_operand:PRED_BHS 1 "register_operand" "Upa")]
			UNPACK_UNSIGNED))]
  "TARGET_SVE"
  "punpk<perm_hilo>\t%0.h, %1.b"
)

;; Unpack the low or high half of a vector, where "high" refers to
;; the low-numbered lanes for big-endian and the high-numbered lanes
;; for little-endian.
(define_expand "vec_unpack<su>_<perm_hilo>_<SVE_BHSI:mode>"
  [(match_operand:<VWIDE> 0 "register_operand")
   (unspec:<VWIDE> [(match_operand:SVE_BHSI 1 "register_operand")] UNPACK)]
  "TARGET_SVE"
  {
    emit_insn ((<hi_lanes_optab>
		? gen_aarch64_sve_<su>unpkhi_<SVE_BHSI:mode>
		: gen_aarch64_sve_<su>unpklo_<SVE_BHSI:mode>)
	       (operands[0], operands[1]));
    DONE;
  }
)

;; SUNPKHI, UUNPKHI, SUNPKLO and UUNPKLO.
(define_insn "aarch64_sve_<su>unpk<perm_hilo>_<SVE_BHSI:mode>"
  [(set (match_operand:<VWIDE> 0 "register_operand" "=w")
	(unspec:<VWIDE> [(match_operand:SVE_BHSI 1 "register_operand" "w")]
			UNPACK))]
  "TARGET_SVE"
  "<su>unpk<perm_hilo>\t%0.<Vewtype>, %1.<Vetype>"
)

;; Unpack one half of a VNx4SF to VNx2DF, or one half of a VNx8HF to VNx4SF.
;; First unpack the source without conversion, then float-convert the
;; unpacked source.
(define_expand "vec_unpacks_<perm_hilo>_<mode>"
  [(match_operand:<VWIDE> 0 "register_operand")
   (unspec:SVE_HSF [(match_operand:SVE_HSF 1 "register_operand")]
		   UNPACK_UNSIGNED)]
  "TARGET_SVE"
  {
    /* Use ZIP to do the unpack, since we don't care about the upper halves
       and since it has the nice property of not needing any subregs.
       If using UUNPK* turns out to be preferable, we could model it as
       a ZIP whose first operand is zero.  */
    rtx temp = gen_reg_rtx (<MODE>mode);
    emit_insn ((<hi_lanes_optab>
		? gen_aarch64_sve_zip2<mode>
		: gen_aarch64_sve_zip1<mode>)
		(temp, operands[1], operands[1]));
    rtx ptrue = force_reg (<VWIDE_PRED>mode, CONSTM1_RTX (<VWIDE_PRED>mode));
    emit_insn (gen_aarch64_sve_extend<mode><Vwide>2 (operands[0],
						     ptrue, temp));
    DONE;
  }
)

;; Unpack one half of a VNx4SI to VNx2DF.  First unpack from VNx4SI
;; to VNx2DI, reinterpret the VNx2DI as a VNx4SI, then convert the
;; unpacked VNx4SI to VNx2DF.
(define_expand "vec_unpack<su_optab>_float_<perm_hilo>_vnx4si"
  [(match_operand:VNx2DF 0 "register_operand")
   (FLOATUORS:VNx2DF
     (unspec:VNx2DI [(match_operand:VNx4SI 1 "register_operand")]
		    UNPACK_UNSIGNED))]
  "TARGET_SVE"
  {
    /* Use ZIP to do the unpack, since we don't care about the upper halves
       and since it has the nice property of not needing any subregs.
       If using UUNPK* turns out to be preferable, we could model it as
       a ZIP whose first operand is zero.  */
    rtx temp = gen_reg_rtx (VNx4SImode);
    emit_insn ((<hi_lanes_optab>
	        ? gen_aarch64_sve_zip2vnx4si
	        : gen_aarch64_sve_zip1vnx4si)
	       (temp, operands[1], operands[1]));
    rtx ptrue = force_reg (VNx2BImode, CONSTM1_RTX (VNx2BImode));
    emit_insn (gen_aarch64_sve_<FLOATUORS:optab>vnx4sivnx2df2 (operands[0],
							       ptrue, temp));
    DONE;
  }
)

;; Predicate pack.  Use UZP1 on the narrower type, which discards
;; the high part of each wide element.
(define_insn "vec_pack_trunc_<Vwide>"
  [(set (match_operand:PRED_BHS 0 "register_operand" "=Upa")
	(unspec:PRED_BHS
	  [(match_operand:<VWIDE> 1 "register_operand" "Upa")
	   (match_operand:<VWIDE> 2 "register_operand" "Upa")]
	  UNSPEC_PACK))]
  "TARGET_SVE"
  "uzp1\t%0.<Vetype>, %1.<Vetype>, %2.<Vetype>"
)

;; Integer pack.  Use UZP1 on the narrower type, which discards
;; the high part of each wide element.
(define_insn "vec_pack_trunc_<Vwide>"
  [(set (match_operand:SVE_BHSI 0 "register_operand" "=w")
	(unspec:SVE_BHSI
	  [(match_operand:<VWIDE> 1 "register_operand" "w")
	   (match_operand:<VWIDE> 2 "register_operand" "w")]
	  UNSPEC_PACK))]
  "TARGET_SVE"
  "uzp1\t%0.<Vetype>, %1.<Vetype>, %2.<Vetype>"
)

;; Convert two vectors of DF to SF, or two vectors of SF to HF, and pack
;; the results into a single vector.
(define_expand "vec_pack_trunc_<Vwide>"
  [(set (match_dup 4)
	(unspec:SVE_HSF
	  [(match_dup 3)
	   (unspec:SVE_HSF [(match_operand:<VWIDE> 1 "register_operand")]
			   UNSPEC_FLOAT_CONVERT)]
	  UNSPEC_MERGE_PTRUE))
   (set (match_dup 5)
	(unspec:SVE_HSF
	  [(match_dup 3)
	   (unspec:SVE_HSF [(match_operand:<VWIDE> 2 "register_operand")]
			   UNSPEC_FLOAT_CONVERT)]
	  UNSPEC_MERGE_PTRUE))
   (set (match_operand:SVE_HSF 0 "register_operand")
	(unspec:SVE_HSF [(match_dup 4) (match_dup 5)] UNSPEC_UZP1))]
  "TARGET_SVE"
  {
    operands[3] = force_reg (<VWIDE_PRED>mode, CONSTM1_RTX (<VWIDE_PRED>mode));
    operands[4] = gen_reg_rtx (<MODE>mode);
    operands[5] = gen_reg_rtx (<MODE>mode);
  }
)

;; Convert two vectors of DF to SI and pack the results into a single vector.
(define_expand "vec_pack_<su>fix_trunc_vnx2df"
  [(set (match_dup 4)
	(unspec:VNx4SI
	  [(match_dup 3)
	   (FIXUORS:VNx4SI (match_operand:VNx2DF 1 "register_operand"))]
	  UNSPEC_MERGE_PTRUE))
   (set (match_dup 5)
	(unspec:VNx4SI
	  [(match_dup 3)
	   (FIXUORS:VNx4SI (match_operand:VNx2DF 2 "register_operand"))]
	  UNSPEC_MERGE_PTRUE))
   (set (match_operand:VNx4SI 0 "register_operand")
	(unspec:VNx4SI [(match_dup 4) (match_dup 5)] UNSPEC_UZP1))]
  "TARGET_SVE"
  {
    operands[3] = force_reg (VNx2BImode, CONSTM1_RTX (VNx2BImode));
    operands[4] = gen_reg_rtx (VNx4SImode);
    operands[5] = gen_reg_rtx (VNx4SImode);
  }
)

;; Predicated floating-point operations with select.
(define_expand "cond_<optab><mode>"
  [(set (match_operand:SVE_F 0 "register_operand")
	(unspec:SVE_F
	  [(match_operand:<VPRED> 1 "register_operand")
	   (unspec:SVE_F
	     [(match_operand:SVE_F 2 "register_operand")
	      (match_operand:SVE_F 3 "register_operand")]
	     SVE_COND_FP_BINARY)
	   (match_operand:SVE_F 4 "aarch64_simd_reg_or_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE"
)

;; Predicated floating-point operations with select matching output.
(define_insn "*cond_<optab><mode>_0"
  [(set (match_operand:SVE_F 0 "register_operand" "+w, w, ?&w")
	(unspec:SVE_F
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl, Upl")
	   (unspec:SVE_F
	     [(match_operand:SVE_F 2 "register_operand" "0, w, w")
	      (match_operand:SVE_F 3 "register_operand" "w, 0, w")]
	     SVE_COND_FP_BINARY)
	   (match_dup 0)]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  "@
   <sve_fp_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
   <sve_fp_op_rev>\t%0.<Vetype>, %1/m, %0.<Vetype>, %2.<Vetype>
   movprfx\t%0, %1/m, %2\;<sve_fp_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>"
  [(set_attr "movprfx" "*,*,yes")]
)

;; Predicated floating-point operations with select matching first operand.
(define_insn "*cond_<optab><mode>_2"
  [(set (match_operand:SVE_F 0 "register_operand" "=w, ?&w")
	(unspec:SVE_F
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl")
	   (unspec:SVE_F
	     [(match_operand:SVE_F 2 "register_operand" "0, w")
	      (match_operand:SVE_F 3 "register_operand" "w, w")]
	     SVE_COND_FP_BINARY)
	   (match_dup 2)]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  "@
   <sve_fp_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
   movprfx\t%0, %2\;<sve_fp_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>"
  [(set_attr "movprfx" "*,yes")]
)

;; Predicated floating-point operations with select matching second operand.
(define_insn "*cond_<optab><mode>_3"
  [(set (match_operand:SVE_F 0 "register_operand" "=w, ?&w")
	(unspec:SVE_F
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl")
	   (unspec:SVE_F
	     [(match_operand:SVE_F 2 "register_operand" "w, w")
	      (match_operand:SVE_F 3 "register_operand" "0, w")]
	     SVE_COND_FP_BINARY)
	   (match_dup 3)]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  "@
   <sve_fp_op_rev>\t%0.<Vetype>, %1/m, %0.<Vetype>, %2.<Vetype>
   movprfx\t%0, %3\;<sve_fp_op_rev>\t%0.<Vetype>, %1/m, %0.<Vetype>, %2.<Vetype>"
  [(set_attr "movprfx" "*,yes")]
)

;; Predicated floating-point operations with select matching zero.
(define_insn "*cond_<optab><mode>_z"
  [(set (match_operand:SVE_F 0 "register_operand" "=&w")
	(unspec:SVE_F
	  [(match_operand:<VPRED> 1 "register_operand" "Upl")
	   (unspec:SVE_F
	     [(match_operand:SVE_F 2 "register_operand" "w")
	      (match_operand:SVE_F 3 "register_operand" "w")]
	     SVE_COND_FP_BINARY)
	   (match_operand:SVE_F 4 "aarch64_simd_imm_zero")]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  "movprfx\t%0.<Vetype>, %1/z, %2.<Vetype>\;<sve_fp_op>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>"
  [(set_attr "movprfx" "yes")]
)

;; Synthetic predication of floating-point operations with select unmatched.
(define_insn_and_split "*cond_<optab><mode>_any"
  [(set (match_operand:SVE_F 0 "register_operand" "=&w")
	(unspec:SVE_F
	  [(match_operand:<VPRED> 1 "register_operand" "Upl")
	   (unspec:SVE_F
	     [(match_operand:SVE_F 2 "register_operand" "w")
	      (match_operand:SVE_F 3 "register_operand" "w")]
	     SVE_COND_FP_BINARY)
	   (match_operand:SVE_F 4 "register_operand" "w")]
	  UNSPEC_SEL))]
  "TARGET_SVE"
  "#"
  "&& reload_completed
   && !(rtx_equal_p (operands[0], operands[4])
        || rtx_equal_p (operands[2], operands[4])
        || rtx_equal_p (operands[3], operands[4]))"
  ; Not matchable by any one insn or movprfx insn.  We need a separate select.
  [(set (match_dup 0)
	(unspec:SVE_F [(match_dup 1) (match_dup 2) (match_dup 4)] UNSPEC_SEL))
   (set (match_dup 0)
	(unspec:SVE_F
	  [(match_dup 1)
	   (unspec:SVE_F [(match_dup 0) (match_dup 3)] SVE_COND_FP_BINARY)
           (match_dup 0)]
	  UNSPEC_SEL))]
)

;; Shift an SVE vector left and insert a scalar into element 0.
(define_insn "vec_shl_insert_<mode>"
  [(set (match_operand:SVE_ALL 0 "register_operand" "=w, w")
	(unspec:SVE_ALL
	  [(match_operand:SVE_ALL 1 "register_operand" "0, 0")
	   (match_operand:<VEL> 2 "register_operand" "rZ, w")]
	  UNSPEC_INSR))]
  "TARGET_SVE"
  "@
   insr\t%0.<Vetype>, %<vwcore>2
   insr\t%0.<Vetype>, %<Vetype>2"
)
