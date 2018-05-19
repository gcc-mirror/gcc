;; Machine description of Andes NDS32 cpu for GNU compiler
;; Copyright (C) 2012-2018 Free Software Foundation, Inc.
;; Contributed by Andes Technology Corporation.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

(define_expand "mov<mode>"
  [(set (match_operand:VQIHI 0 "general_operand" "")
	(match_operand:VQIHI 1 "general_operand" ""))]
  "NDS32_EXT_DSP_P ()"
{
  /* Need to force register if mem <- !reg.  */
  if (MEM_P (operands[0]) && !REG_P (operands[1]))
    operands[1] = force_reg (<MODE>mode, operands[1]);

  /* If operands[1] is a large constant and cannot be performed
     by a single instruction, we need to split it.  */
  if (GET_CODE (operands[1]) == CONST_VECTOR
      && !satisfies_constraint_CVs2 (operands[1])
      && !satisfies_constraint_CVhi (operands[1]))
    {
      HOST_WIDE_INT ival = const_vector_to_hwint (operands[1]);
      rtx tmp_rtx;

      tmp_rtx = can_create_pseudo_p ()
		? gen_reg_rtx (SImode)
		: simplify_gen_subreg (SImode, operands[0], <MODE>mode, 0);

      emit_move_insn (tmp_rtx, gen_int_mode (ival, SImode));
      convert_move (operands[0], tmp_rtx, false);
      DONE;
    }

  if (REG_P (operands[0]) && SYMBOLIC_CONST_P (operands[1]))
    {
      if (nds32_tls_referenced_p (operands [1]))
	{
	  nds32_expand_tls_move (operands);
	  DONE;
	}
      else if (flag_pic)
	{
	  nds32_expand_pic_move (operands);
	  DONE;
	}
    }
})

(define_insn "*mov<mode>"
  [(set (match_operand:VQIHI 0 "nonimmediate_operand" "=r, r,$U45,$U33,$U37,$U45, m,$  l,$  l,$  l,$  d,  d, r,$   d,    r,    r,    r, *f, *f,  r, *f,  Q")
	(match_operand:VQIHI 1 "nds32_vmove_operand"  " r, r,   l,   l,   l,   d, r, U45, U33, U37, U45,Ufe, m, CVp5, CVs5, CVs2, CVhi, *f,  r, *f,  Q, *f"))]
  "NDS32_EXT_DSP_P ()
   && (register_operand(operands[0], <MODE>mode)
       || register_operand(operands[1], <MODE>mode))"
{
  switch (which_alternative)
    {
    case 0:
      return "mov55\t%0, %1";
    case 1:
      return "ori\t%0, %1, 0";
    case 2:
    case 3:
    case 4:
    case 5:
      return nds32_output_16bit_store (operands, <byte>);
    case 6:
      return nds32_output_32bit_store (operands, <byte>);
    case 7:
    case 8:
    case 9:
    case 10:
    case 11:
      return nds32_output_16bit_load (operands, <byte>);
    case 12:
      return nds32_output_32bit_load (operands, <byte>);
    case 13:
      return "movpi45\t%0, %1";
    case 14:
      return "movi55\t%0, %1";
    case 15:
      return "movi\t%0, %1";
    case 16:
      return "sethi\t%0, hi20(%1)";
    case 17:
      if (TARGET_FPU_SINGLE)
	return "fcpyss\t%0, %1, %1";
      else
	return "#";
    case 18:
      return "fmtsr\t%1, %0";
    case 19:
      return "fmfsr\t%0, %1";
    case 20:
      return nds32_output_float_load (operands);
    case 21:
      return nds32_output_float_store (operands);
    default:
      gcc_unreachable ();
    }
}
  [(set_attr "type"    "alu,alu,store,store,store,store,store,load,load,load,load,load,load,alu,alu,alu,alu,fcpy,fmtsr,fmfsr,fload,fstore")
   (set_attr "length"  "  2,  4,    2,    2,    2,    2,    4,   2,   2,   2,   2,   2,   4,  2,  2,  4,  4,   4,    4,    4,    4,     4")
   (set_attr "feature" " v1, v1,   v1,   v1,   v1,   v1,   v1,  v1,  v1,  v1,  v1, v3m,  v1, v1, v1, v1, v1, fpu,  fpu,  fpu,  fpu,   fpu")])

(define_expand "movv2si"
  [(set (match_operand:V2SI 0 "general_operand" "")
	(match_operand:V2SI 1 "general_operand" ""))]
  "NDS32_EXT_DSP_P ()"
{
  /* Need to force register if mem <- !reg.  */
  if (MEM_P (operands[0]) && !REG_P (operands[1]))
    operands[1] = force_reg (V2SImode, operands[1]);
})

(define_insn "*movv2si"
  [(set (match_operand:V2SI 0 "nonimmediate_operand" "=r, r,  r, r, Da, m, f, Q, f, r, f")
	(match_operand:V2SI 1 "general_operand"      " r, i, Da, m,  r, r, Q, f, f, f, r"))]
  "NDS32_EXT_DSP_P ()
   && (register_operand(operands[0], V2SImode)
       || register_operand(operands[1], V2SImode))"
{
  switch (which_alternative)
    {
    case 0:
      return "movd44\t%0, %1";
    case 1:
      /* reg <- const_int, we ask gcc to split instruction.  */
      return "#";
    case 2:
      /* The memory format is (mem (reg)),
	 we can generate 'lmw.bi' instruction.  */
      return nds32_output_double (operands, true);
    case 3:
      /* We haven't 64-bit load instruction,
	 we split this pattern to two SImode pattern.  */
      return "#";
    case 4:
      /* The memory format is (mem (reg)),
	 we can generate 'smw.bi' instruction.  */
      return nds32_output_double (operands, false);
    case 5:
      /* We haven't 64-bit store instruction,
	 we split this pattern to two SImode pattern.  */
      return "#";
    case 6:
      return nds32_output_float_load (operands);
    case 7:
      return nds32_output_float_store (operands);
    case 8:
      return "fcpysd\t%0, %1, %1";
    case 9:
      return "fmfdr\t%0, %1";
    case 10:
      return "fmtdr\t%1, %0";
    default:
      gcc_unreachable ();
    }
}
  [(set_attr "type"    "alu,alu,load,load,store,store,unknown,unknown,unknown,unknown,unknown")
   (set_attr_alternative "length"
     [
       ;; Alternative 0
       (if_then_else (match_test "!TARGET_16_BIT")
		     (const_int 4)
		     (const_int 2))
       ;; Alternative 1
       (const_int 16)
       ;; Alternative 2
       (const_int 4)
       ;; Alternative 3
       (const_int 8)
       ;; Alternative 4
       (const_int 4)
       ;; Alternative 5
       (const_int 8)
       ;; Alternative 6
       (const_int 4)
       ;; Alternative 7
       (const_int 4)
       ;; Alternative 8
       (const_int 4)
       ;; Alternative 9
       (const_int 4)
       ;; Alternative 10
       (const_int 4)
     ])
   (set_attr "feature" " v1, v1,  v1,  v1,   v1,   v1,    fpu,    fpu,    fpu,    fpu,    fpu")])

(define_expand "movmisalign<mode>"
  [(set (match_operand:VQIHI 0 "general_operand" "")
	(match_operand:VQIHI 1 "general_operand" ""))]
  "NDS32_EXT_DSP_P ()"
{
  rtx addr;
  if (MEM_P (operands[0]) && !REG_P (operands[1]))
    operands[1] = force_reg (<MODE>mode, operands[1]);

  if (MEM_P (operands[0]))
    {
      addr = force_reg (Pmode, XEXP (operands[0], 0));
      emit_insn (gen_unaligned_store<mode> (addr, operands[1]));
    }
  else
    {
      addr = force_reg (Pmode, XEXP (operands[1], 0));
      emit_insn (gen_unaligned_load<mode> (operands[0], addr));
    }
  DONE;
})

(define_expand "unaligned_load<mode>"
  [(set (match_operand:VQIHI 0 "register_operand" "=r")
	(unspec:VQIHI [(mem:VQIHI (match_operand:SI 1 "register_operand" "r"))] UNSPEC_UALOAD_W))]
  "NDS32_EXT_DSP_P ()"
{
  if (TARGET_ISA_V3M)
    nds32_expand_unaligned_load (operands, <MODE>mode);
  else
    emit_insn (gen_unaligned_load_w<mode> (operands[0], gen_rtx_MEM (<MODE>mode, operands[1])));
  DONE;
})

(define_insn "unaligned_load_w<mode>"
  [(set (match_operand:VQIHI 0 "register_operand"                          "=  r")
	(unspec:VQIHI [(match_operand:VQIHI 1 "nds32_lmw_smw_base_operand" " Umw")] UNSPEC_UALOAD_W))]
  "NDS32_EXT_DSP_P ()"
{
  return nds32_output_lmw_single_word (operands);
}
  [(set_attr "type"   "load")
   (set_attr "length"    "4")]
)

(define_expand "unaligned_store<mode>"
  [(set (mem:VQIHI (match_operand:SI 0 "register_operand" "r"))
	(unspec:VQIHI [(match_operand:VQIHI 1 "register_operand" "r")] UNSPEC_UASTORE_W))]
  "NDS32_EXT_DSP_P ()"
{
  if (TARGET_ISA_V3M)
    nds32_expand_unaligned_store (operands, <MODE>mode);
  else
    emit_insn (gen_unaligned_store_w<mode> (gen_rtx_MEM (<MODE>mode, operands[0]), operands[1]));
  DONE;
})

(define_insn "unaligned_store_w<mode>"
  [(set (match_operand:VQIHI 0 "nds32_lmw_smw_base_operand"      "=Umw")
	(unspec:VQIHI [(match_operand:VQIHI 1 "register_operand" "   r")] UNSPEC_UASTORE_W))]
  "NDS32_EXT_DSP_P ()"
{
  return nds32_output_smw_single_word (operands);
}
  [(set_attr "type"   "store")
   (set_attr "length"     "4")]
)

(define_insn "<uk>add<mode>3"
  [(set (match_operand:VQIHI 0 "register_operand"                 "=r")
	(all_plus:VQIHI (match_operand:VQIHI 1 "register_operand" " r")
			(match_operand:VQIHI 2 "register_operand" " r")))]
  "NDS32_EXT_DSP_P ()"
  "<uk>add<bits> %0, %1, %2"
  [(set_attr "type"    "dalu")
   (set_attr "length"  "4")
   (set_attr "feature" "v1")])

(define_insn "<uk>adddi3"
  [(set (match_operand:DI 0 "register_operand"              "=r")
	(all_plus:DI (match_operand:DI 1 "register_operand" " r")
		     (match_operand:DI 2 "register_operand" " r")))]
  "NDS32_EXT_DSP_P ()"
  "<uk>add64 %0, %1, %2"
  [(set_attr "type"    "dalu64")
   (set_attr "length"  "4")
   (set_attr "feature" "v1")])

(define_insn "raddv4qi3"
  [(set (match_operand:V4QI 0 "register_operand"                  "=r")
	(truncate:V4QI
	  (ashiftrt:V4HI
	    (plus:V4HI (sign_extend:V4HI (match_operand:V4QI 1 "register_operand" " r"))
		       (sign_extend:V4HI (match_operand:V4QI 2 "register_operand" " r")))
	    (const_int 1))))]
  "NDS32_EXT_DSP_P ()"
  "radd8\t%0, %1, %2"
  [(set_attr "type"    "dalu")
   (set_attr "length"  "4")
   (set_attr "feature" "v1")])


(define_insn "uraddv4qi3"
  [(set (match_operand:V4QI 0 "register_operand"                  "=r")
	(truncate:V4QI
	  (lshiftrt:V4HI
	    (plus:V4HI (zero_extend:V4HI (match_operand:V4QI 1 "register_operand" " r"))
		       (zero_extend:V4HI (match_operand:V4QI 2 "register_operand" " r")))
	    (const_int 1))))]
  "NDS32_EXT_DSP_P ()"
  "uradd8\t%0, %1, %2"
  [(set_attr "type"    "dalu")
   (set_attr "length"  "4")
   (set_attr "feature" "v1")])

(define_insn "raddv2hi3"
  [(set (match_operand:V2HI 0 "register_operand"                                  "=r")
	(truncate:V2HI
	  (ashiftrt:V2SI
	    (plus:V2SI (sign_extend:V2SI (match_operand:V2HI 1 "register_operand" " r"))
		       (sign_extend:V2SI (match_operand:V2HI 2 "register_operand" " r")))
	    (const_int 1))))]
  "NDS32_EXT_DSP_P ()"
  "radd16\t%0, %1, %2"
  [(set_attr "type"    "dalu")
   (set_attr "length"  "4")
   (set_attr "feature" "v1")])

(define_insn "uraddv2hi3"
  [(set (match_operand:V2HI 0 "register_operand"                                  "=r")
	(truncate:V2HI
	  (lshiftrt:V2SI
	    (plus:V2SI (zero_extend:V2SI (match_operand:V2HI 1 "register_operand" " r"))
		       (zero_extend:V2SI (match_operand:V2HI 2 "register_operand" " r")))
	    (const_int 1))))]
  "NDS32_EXT_DSP_P ()"
  "uradd16\t%0, %1, %2"
  [(set_attr "type"    "dalu")
   (set_attr "length"  "4")
   (set_attr "feature" "v1")])

(define_insn "radddi3"
  [(set (match_operand:DI 0 "register_operand"            "=r")
	(truncate:DI
	  (ashiftrt:TI
	    (plus:TI (sign_extend:TI (match_operand:DI 1 "register_operand" " r"))
		     (sign_extend:TI (match_operand:DI 2 "register_operand" " r")))
	  (const_int 1))))]
  "NDS32_EXT_DSP_P ()"
  "radd64\t%0, %1, %2"
  [(set_attr "type"    "dalu64")
   (set_attr "length"  "4")
   (set_attr "feature" "v1")])


(define_insn "uradddi3"
  [(set (match_operand:DI 0 "register_operand"            "=r")
	(truncate:DI
	  (lshiftrt:TI
	    (plus:TI (zero_extend:TI (match_operand:DI 1 "register_operand" " r"))
		     (zero_extend:TI (match_operand:DI 2 "register_operand" " r")))
	  (const_int 1))))]
  "NDS32_EXT_DSP_P ()"
  "uradd64\t%0, %1, %2"
  [(set_attr "type"    "dalu64")
   (set_attr "length"  "4")
   (set_attr "feature" "v1")])

(define_insn "<uk>sub<mode>3"
  [(set (match_operand:VQIHI 0 "register_operand"                  "=r")
	(all_minus:VQIHI (match_operand:VQIHI 1 "register_operand" " r")
			 (match_operand:VQIHI 2 "register_operand" " r")))]
  "NDS32_EXT_DSP_P ()"
  "<uk>sub<bits> %0, %1, %2"
  [(set_attr "type"    "dalu")
   (set_attr "length"  "4")
   (set_attr "feature" "v1")])

(define_insn "<uk>subdi3"
  [(set (match_operand:DI 0 "register_operand"               "=r")
	(all_minus:DI (match_operand:DI 1 "register_operand" " r")
		      (match_operand:DI 2 "register_operand" " r")))]
  "NDS32_EXT_DSP_P ()"
  "<uk>sub64 %0, %1, %2"
  [(set_attr "type"    "dalu64")
   (set_attr "length"  "4")
   (set_attr "feature" "v1")])

(define_insn "rsubv4qi3"
  [(set (match_operand:V4QI 0 "register_operand"                                   "=r")
	(truncate:V4QI
	  (ashiftrt:V4HI
	    (minus:V4HI (sign_extend:V4HI (match_operand:V4QI 1 "register_operand" " r"))
			(sign_extend:V4HI (match_operand:V4QI 2 "register_operand" " r")))
	    (const_int 1))))]
  "NDS32_EXT_DSP_P ()"
  "rsub8\t%0, %1, %2"
  [(set_attr "type"    "dalu")
   (set_attr "length"  "4")])

(define_insn "ursubv4qi3"
  [(set (match_operand:V4QI 0 "register_operand"                                   "=r")
	(truncate:V4QI
	  (lshiftrt:V4HI
	    (minus:V4HI (zero_extend:V4HI (match_operand:V4QI 1 "register_operand" " r"))
			(zero_extend:V4HI (match_operand:V4QI 2 "register_operand" " r")))
	    (const_int 1))))]
  "NDS32_EXT_DSP_P ()"
  "ursub8\t%0, %1, %2"
  [(set_attr "type"    "dalu")
   (set_attr "length"  "4")])

(define_insn "rsubv2hi3"
  [(set (match_operand:V2HI 0 "register_operand"                                   "=r")
	(truncate:V2HI
	  (ashiftrt:V2SI
	    (minus:V2SI (sign_extend:V2SI (match_operand:V2HI 1 "register_operand" " r"))
			(sign_extend:V2SI (match_operand:V2HI 2 "register_operand" " r")))
	    (const_int 1))))]
  "NDS32_EXT_DSP_P ()"
  "rsub16\t%0, %1, %2"
  [(set_attr "type"    "dalu")
   (set_attr "length"  "4")])

(define_insn "ursubv2hi3"
  [(set (match_operand:V2HI 0 "register_operand"                                   "=r")
	(truncate:V2HI
	  (lshiftrt:V2SI
	    (minus:V2SI (zero_extend:V2SI (match_operand:V2HI 1 "register_operand" " r"))
			(zero_extend:V2SI (match_operand:V2HI 2 "register_operand" " r")))
	    (const_int 1))))]
  "NDS32_EXT_DSP_P ()"
  "ursub16\t%0, %1, %2"
  [(set_attr "type"    "dalu")
   (set_attr "length"  "4")])

(define_insn "rsubdi3"
  [(set (match_operand:DI 0 "register_operand"                   "=r")
	(truncate:DI
	  (ashiftrt:TI
	    (minus:TI (sign_extend:TI (match_operand:DI 1 "register_operand" " r"))
		      (sign_extend:TI (match_operand:DI 2 "register_operand" " r")))
	  (const_int 1))))]
  "NDS32_EXT_DSP_P ()"
  "rsub64\t%0, %1, %2"
  [(set_attr "type"    "dalu64")
   (set_attr "length"  "4")])


(define_insn "ursubdi3"
  [(set (match_operand:DI 0 "register_operand"                   "=r")
	(truncate:DI
	  (lshiftrt:TI
	    (minus:TI (zero_extend:TI (match_operand:DI 1 "register_operand" " r"))
		      (zero_extend:TI (match_operand:DI 2 "register_operand" " r")))
	  (const_int 1))))]
  "NDS32_EXT_DSP_P ()"
  "ursub64\t%0, %1, %2"
  [(set_attr "type"    "dalu64")
   (set_attr "length"  "4")])

(define_expand "cras16_1"
  [(match_operand:V2HI 0 "register_operand" "")
   (match_operand:V2HI 1 "register_operand" "")
   (match_operand:V2HI 2 "register_operand" "")]
  "NDS32_EXT_DSP_P ()"
{
  if (TARGET_BIG_ENDIAN)
    emit_insn (gen_cras16_1_be (operands[0], operands[1], operands[2]));
  else
    emit_insn (gen_cras16_1_le (operands[0], operands[1], operands[2]));
  DONE;
})

(define_insn "cras16_1_le"
  [(set (match_operand:V2HI 0 "register_operand"         "=r")
	(vec_merge:V2HI
	  (vec_duplicate:V2HI
	    (minus:HI
	      (vec_select:HI
		(match_operand:V2HI 1 "register_operand" " r")
		(parallel [(const_int 0)]))
	      (vec_select:HI
		(match_operand:V2HI 2 "register_operand" " r")
		(parallel [(const_int 1)]))))
	  (vec_duplicate:V2HI
	    (plus:HI
	      (vec_select:HI
		(match_dup 2)
		(parallel [(const_int 0)]))
	      (vec_select:HI
		(match_dup 1)
		(parallel [(const_int 1)]))))
	  (const_int 1)))]
  "NDS32_EXT_DSP_P () && !TARGET_BIG_ENDIAN"
  "cras16\t%0, %1, %2"
  [(set_attr "type" "dalu")]
)

(define_insn "cras16_1_be"
  [(set (match_operand:V2HI 0 "register_operand"         "=r")
	(vec_merge:V2HI
	  (vec_duplicate:V2HI
	    (minus:HI
	      (vec_select:HI
		(match_operand:V2HI 1 "register_operand" " r")
		(parallel [(const_int 1)]))
	      (vec_select:HI
		(match_operand:V2HI 2 "register_operand" " r")
		(parallel [(const_int 0)]))))
	  (vec_duplicate:V2HI
	    (plus:HI
	      (vec_select:HI
		(match_dup 2)
		(parallel [(const_int 1)]))
	      (vec_select:HI
		(match_dup 1)
		(parallel [(const_int 0)]))))
	  (const_int 2)))]
  "NDS32_EXT_DSP_P () && TARGET_BIG_ENDIAN"
  "cras16\t%0, %1, %2"
  [(set_attr "type" "dalu")]
)

(define_expand "kcras16_1"
  [(match_operand:V2HI 0 "register_operand" "")
   (match_operand:V2HI 1 "register_operand" "")
   (match_operand:V2HI 2 "register_operand" "")]
  "NDS32_EXT_DSP_P ()"
{
  if (TARGET_BIG_ENDIAN)
    emit_insn (gen_kcras16_1_be (operands[0], operands[1], operands[2]));
  else
    emit_insn (gen_kcras16_1_le (operands[0], operands[1], operands[2]));
  DONE;
})

(define_insn "kcras16_1_le"
  [(set (match_operand:V2HI 0 "register_operand"         "=r")
	(vec_merge:V2HI
	  (vec_duplicate:V2HI
	    (ss_minus:HI
	      (vec_select:HI
		(match_operand:V2HI 1 "register_operand" " r")
		(parallel [(const_int 0)]))
	      (vec_select:HI
		(match_operand:V2HI 2 "register_operand" " r")
		(parallel [(const_int 1)]))))
	  (vec_duplicate:V2HI
	    (ss_plus:HI
	      (vec_select:HI
		(match_dup 2)
		(parallel [(const_int 0)]))
	      (vec_select:HI
		(match_dup 1)
		(parallel [(const_int 1)]))))
	  (const_int 1)))]
  "NDS32_EXT_DSP_P () && !TARGET_BIG_ENDIAN"
  "kcras16\t%0, %1, %2"
  [(set_attr "type" "dalu")]
)

(define_insn "kcras16_1_be"
  [(set (match_operand:V2HI 0 "register_operand"         "=r")
	(vec_merge:V2HI
	  (vec_duplicate:V2HI
	    (ss_minus:HI
	      (vec_select:HI
		(match_operand:V2HI 1 "register_operand" " r")
		(parallel [(const_int 1)]))
	      (vec_select:HI
		(match_operand:V2HI 2 "register_operand" " r")
		(parallel [(const_int 0)]))))
	  (vec_duplicate:V2HI
	    (ss_plus:HI
	      (vec_select:HI
		(match_dup 2)
		(parallel [(const_int 1)]))
	      (vec_select:HI
		(match_dup 1)
		(parallel [(const_int 0)]))))
	  (const_int 2)))]
  "NDS32_EXT_DSP_P () && TARGET_BIG_ENDIAN"
  "kcras16\t%0, %1, %2"
  [(set_attr "type" "dalu")]
)

(define_expand "ukcras16_1"
  [(match_operand:V2HI 0 "register_operand" "")
   (match_operand:V2HI 1 "register_operand" "")
   (match_operand:V2HI 2 "register_operand" "")]
  "NDS32_EXT_DSP_P ()"
{
  if (TARGET_BIG_ENDIAN)
    emit_insn (gen_ukcras16_1_be (operands[0], operands[1], operands[2]));
  else
    emit_insn (gen_ukcras16_1_le (operands[0], operands[1], operands[2]));
  DONE;
})

(define_insn "ukcras16_1_le"
  [(set (match_operand:V2HI 0 "register_operand"         "=r")
	(vec_merge:V2HI
	  (vec_duplicate:V2HI
	    (us_minus:HI
	      (vec_select:HI
		(match_operand:V2HI 1 "register_operand" " r")
		(parallel [(const_int 0)]))
	      (vec_select:HI
		(match_operand:V2HI 2 "register_operand" " r")
		(parallel [(const_int 1)]))))
	  (vec_duplicate:V2HI
	    (us_plus:HI
	      (vec_select:HI
		(match_dup 2)
		(parallel [(const_int 0)]))
	      (vec_select:HI
		(match_dup 1)
		(parallel [(const_int 1)]))))
	  (const_int 1)))]
  "NDS32_EXT_DSP_P () && !TARGET_BIG_ENDIAN"
  "ukcras16\t%0, %1, %2"
  [(set_attr "type" "dalu")]
)

(define_insn "ukcras16_1_be"
  [(set (match_operand:V2HI 0 "register_operand"         "=r")
	(vec_merge:V2HI
	  (vec_duplicate:V2HI
	    (us_minus:HI
	      (vec_select:HI
		(match_operand:V2HI 1 "register_operand" " r")
		(parallel [(const_int 1)]))
	      (vec_select:HI
		(match_operand:V2HI 2 "register_operand" " r")
		(parallel [(const_int 0)]))))
	  (vec_duplicate:V2HI
	    (us_plus:HI
	      (vec_select:HI
		(match_dup 2)
		(parallel [(const_int 1)]))
	      (vec_select:HI
		(match_dup 1)
		(parallel [(const_int 0)]))))
	  (const_int 2)))]
  "NDS32_EXT_DSP_P () && TARGET_BIG_ENDIAN"
  "ukcras16\t%0, %1, %2"
  [(set_attr "type" "dalu")]
)

(define_expand "crsa16_1"
  [(match_operand:V2HI 0 "register_operand" "")
   (match_operand:V2HI 1 "register_operand" "")
   (match_operand:V2HI 2 "register_operand" "")]
  "NDS32_EXT_DSP_P ()"
{
  if (TARGET_BIG_ENDIAN)
    emit_insn (gen_crsa16_1_be (operands[0], operands[1], operands[2]));
  else
    emit_insn (gen_crsa16_1_le (operands[0], operands[1], operands[2]));
  DONE;
})

(define_insn "crsa16_1_le"
  [(set (match_operand:V2HI 0 "register_operand"         "=r")
	(vec_merge:V2HI
	  (vec_duplicate:V2HI
	    (minus:HI
	      (vec_select:HI
		(match_operand:V2HI 1 "register_operand" " r")
		(parallel [(const_int 1)]))
	      (vec_select:HI
		(match_operand:V2HI 2 "register_operand" " r")
		(parallel [(const_int 0)]))))
	  (vec_duplicate:V2HI
	    (plus:HI
	      (vec_select:HI
		(match_dup 1)
		(parallel [(const_int 0)]))
	      (vec_select:HI
		(match_dup 2)
		(parallel [(const_int 1)]))))
	  (const_int 2)))]
  "NDS32_EXT_DSP_P () && !TARGET_BIG_ENDIAN"
  "crsa16\t%0, %1, %2"
  [(set_attr "type" "dalu")]
)

(define_insn "crsa16_1_be"
  [(set (match_operand:V2HI 0 "register_operand"         "=r")
	(vec_merge:V2HI
	  (vec_duplicate:V2HI
	    (minus:HI
	      (vec_select:HI
		(match_operand:V2HI 1 "register_operand" " r")
		(parallel [(const_int 0)]))
	      (vec_select:HI
		(match_operand:V2HI 2 "register_operand" " r")
		(parallel [(const_int 1)]))))
	  (vec_duplicate:V2HI
	    (plus:HI
	      (vec_select:HI
		(match_dup 1)
		(parallel [(const_int 1)]))
	      (vec_select:HI
		(match_dup 2)
		(parallel [(const_int 0)]))))
	  (const_int 1)))]
  "NDS32_EXT_DSP_P () && TARGET_BIG_ENDIAN"
  "crsa16\t%0, %1, %2"
  [(set_attr "type" "dalu")]
)

(define_expand "kcrsa16_1"
  [(match_operand:V2HI 0 "register_operand" "")
   (match_operand:V2HI 1 "register_operand" "")
   (match_operand:V2HI 2 "register_operand" "")]
  "NDS32_EXT_DSP_P ()"
{
  if (TARGET_BIG_ENDIAN)
    emit_insn (gen_kcrsa16_1_be (operands[0], operands[1], operands[2]));
  else
    emit_insn (gen_kcrsa16_1_le (operands[0], operands[1], operands[2]));
  DONE;
})

(define_insn "kcrsa16_1_le"
  [(set (match_operand:V2HI 0 "register_operand"         "=r")
	(vec_merge:V2HI
	  (vec_duplicate:V2HI
	    (ss_minus:HI
	      (vec_select:HI
		(match_operand:V2HI 1 "register_operand" " r")
		(parallel [(const_int 1)]))
	      (vec_select:HI
		(match_operand:V2HI 2 "register_operand" " r")
		(parallel [(const_int 0)]))))
	  (vec_duplicate:V2HI
	    (ss_plus:HI
	      (vec_select:HI
		(match_dup 1)
		(parallel [(const_int 0)]))
	      (vec_select:HI
		(match_dup 2)
		(parallel [(const_int 1)]))))
	  (const_int 2)))]
  "NDS32_EXT_DSP_P () && !TARGET_BIG_ENDIAN"
  "kcrsa16\t%0, %1, %2"
  [(set_attr "type" "dalu")]
)

(define_insn "kcrsa16_1_be"
  [(set (match_operand:V2HI 0 "register_operand"         "=r")
	(vec_merge:V2HI
	  (vec_duplicate:V2HI
	    (ss_minus:HI
	      (vec_select:HI
		(match_operand:V2HI 1 "register_operand" " r")
		(parallel [(const_int 0)]))
	      (vec_select:HI
		(match_operand:V2HI 2 "register_operand" " r")
		(parallel [(const_int 1)]))))
	  (vec_duplicate:V2HI
	    (ss_plus:HI
	      (vec_select:HI
		(match_dup 1)
		(parallel [(const_int 1)]))
	      (vec_select:HI
		(match_dup 2)
		(parallel [(const_int 0)]))))
	  (const_int 1)))]
  "NDS32_EXT_DSP_P () && TARGET_BIG_ENDIAN"
  "kcrsa16\t%0, %1, %2"
  [(set_attr "type" "dalu")]
)

(define_expand "ukcrsa16_1"
  [(match_operand:V2HI 0 "register_operand" "")
   (match_operand:V2HI 1 "register_operand" "")
   (match_operand:V2HI 2 "register_operand" "")]
  "NDS32_EXT_DSP_P ()"
{
  if (TARGET_BIG_ENDIAN)
    emit_insn (gen_ukcrsa16_1_be (operands[0], operands[1], operands[2]));
  else
    emit_insn (gen_ukcrsa16_1_le (operands[0], operands[1], operands[2]));
  DONE;
})

(define_insn "ukcrsa16_1_le"
  [(set (match_operand:V2HI 0 "register_operand"         "=r")
	(vec_merge:V2HI
	  (vec_duplicate:V2HI
	    (us_minus:HI
	      (vec_select:HI
		(match_operand:V2HI 1 "register_operand" " r")
		(parallel [(const_int 1)]))
	      (vec_select:HI
		(match_operand:V2HI 2 "register_operand" " r")
		(parallel [(const_int 0)]))))
	  (vec_duplicate:V2HI
	    (us_plus:HI
	      (vec_select:HI
		(match_dup 1)
		(parallel [(const_int 0)]))
	      (vec_select:HI
		(match_dup 2)
		(parallel [(const_int 1)]))))
	  (const_int 2)))]
  "NDS32_EXT_DSP_P () && !TARGET_BIG_ENDIAN"
  "ukcrsa16\t%0, %1, %2"
  [(set_attr "type" "dalu")]
)

(define_insn "ukcrsa16_1_be"
  [(set (match_operand:V2HI 0 "register_operand"         "=r")
	(vec_merge:V2HI
	  (vec_duplicate:V2HI
	    (us_minus:HI
	      (vec_select:HI
		(match_operand:V2HI 1 "register_operand" " r")
		(parallel [(const_int 0)]))
	      (vec_select:HI
		(match_operand:V2HI 2 "register_operand" " r")
		(parallel [(const_int 1)]))))
	  (vec_duplicate:V2HI
	    (us_plus:HI
	      (vec_select:HI
		(match_dup 1)
		(parallel [(const_int 1)]))
	      (vec_select:HI
		(match_dup 2)
		(parallel [(const_int 0)]))))
	  (const_int 1)))]
  "NDS32_EXT_DSP_P () && TARGET_BIG_ENDIAN"
  "ukcrsa16\t%0, %1, %2"
  [(set_attr "type" "dalu")]
)

(define_expand "rcras16_1"
  [(match_operand:V2HI 0 "register_operand" "")
   (match_operand:V2HI 1 "register_operand" "")
   (match_operand:V2HI 2 "register_operand" "")]
  "NDS32_EXT_DSP_P ()"
{
  if (TARGET_BIG_ENDIAN)
    emit_insn (gen_rcras16_1_be (operands[0], operands[1], operands[2]));
  else
    emit_insn (gen_rcras16_1_le (operands[0], operands[1], operands[2]));
  DONE;
})

(define_insn "rcras16_1_le"
  [(set (match_operand:V2HI 0 "register_operand"           "=r")
	(vec_merge:V2HI
	  (vec_duplicate:V2HI
	    (truncate:HI
	      (ashiftrt:SI
		(minus:SI
		  (sign_extend:SI
		    (vec_select:HI
		      (match_operand:V2HI 1 "register_operand" " r")
		      (parallel [(const_int 0)])))
		  (sign_extend:SI
		    (vec_select:HI
		      (match_operand:V2HI 2 "register_operand" " r")
		      (parallel [(const_int 1)]))))
		(const_int 1))))
	  (vec_duplicate:V2HI
	    (truncate:HI
	      (ashiftrt:SI
		(plus:SI
		  (sign_extend:SI
		    (vec_select:HI
		      (match_dup 2)
		      (parallel [(const_int 0)])))
		  (sign_extend:SI
		    (vec_select:HI
		      (match_dup 1)
		      (parallel [(const_int 1)]))))
		(const_int 1))))
	  (const_int 1)))]
  "NDS32_EXT_DSP_P () && !TARGET_BIG_ENDIAN"
  "rcras16\t%0, %1, %2"
  [(set_attr "type" "dalu")]
)

(define_insn "rcras16_1_be"
  [(set (match_operand:V2HI 0 "register_operand"           "=r")
	(vec_merge:V2HI
	  (vec_duplicate:V2HI
	    (truncate:HI
	      (ashiftrt:SI
		(minus:SI
		  (sign_extend:SI
		    (vec_select:HI
		      (match_operand:V2HI 1 "register_operand" " r")
		      (parallel [(const_int 1)])))
		  (sign_extend:SI
		    (vec_select:HI
		      (match_operand:V2HI 2 "register_operand" " r")
		      (parallel [(const_int 0)]))))
		(const_int 1))))
	  (vec_duplicate:V2HI
	    (truncate:HI
	      (ashiftrt:SI
		(plus:SI
		  (sign_extend:SI
		    (vec_select:HI
		      (match_dup 2)
		      (parallel [(const_int 1)])))
		  (sign_extend:SI
		    (vec_select:HI
		      (match_dup 1)
		      (parallel [(const_int 0)]))))
		(const_int 1))))
	  (const_int 2)))]
  "NDS32_EXT_DSP_P () && TARGET_BIG_ENDIAN"
  "rcras16\t%0, %1, %2"
  [(set_attr "type" "dalu")]
)

(define_expand "urcras16_1"
  [(match_operand:V2HI 0 "register_operand" "")
   (match_operand:V2HI 1 "register_operand" "")
   (match_operand:V2HI 2 "register_operand" "")]
  "NDS32_EXT_DSP_P ()"
{
  if (TARGET_BIG_ENDIAN)
    emit_insn (gen_urcras16_1_be (operands[0], operands[1], operands[2]));
  else
    emit_insn (gen_urcras16_1_le (operands[0], operands[1], operands[2]));
  DONE;
})

(define_insn "urcras16_1_le"
  [(set (match_operand:V2HI 0 "register_operand"           "=r")
	(vec_merge:V2HI
	  (vec_duplicate:V2HI
	    (truncate:HI
	      (lshiftrt:SI
		(minus:SI
		  (zero_extend:SI
		    (vec_select:HI
		      (match_operand:V2HI 1 "register_operand" " r")
		      (parallel [(const_int 0)])))
		  (zero_extend:SI
		    (vec_select:HI
		      (match_operand:V2HI 2 "register_operand" " r")
		      (parallel [(const_int 1)]))))
		(const_int 1))))
	  (vec_duplicate:V2HI
	    (truncate:HI
	      (lshiftrt:SI
		(plus:SI
		  (zero_extend:SI
		    (vec_select:HI
		      (match_dup 2)
		      (parallel [(const_int 0)])))
		  (zero_extend:SI
		    (vec_select:HI
		      (match_dup 1)
		      (parallel [(const_int 1)]))))
		(const_int 1))))
	  (const_int 1)))]
  "NDS32_EXT_DSP_P () && !TARGET_BIG_ENDIAN"
  "urcras16\t%0, %1, %2"
  [(set_attr "type" "dalu")]
)

(define_insn "urcras16_1_be"
  [(set (match_operand:V2HI 0 "register_operand"           "=r")
	(vec_merge:V2HI
	  (vec_duplicate:V2HI
	    (truncate:HI
	      (lshiftrt:SI
		(minus:SI
		  (zero_extend:SI
		    (vec_select:HI
		      (match_operand:V2HI 1 "register_operand" " r")
		      (parallel [(const_int 1)])))
		  (zero_extend:SI
		    (vec_select:HI
		      (match_operand:V2HI 2 "register_operand" " r")
		      (parallel [(const_int 0)]))))
		(const_int 1))))
	  (vec_duplicate:V2HI
	    (truncate:HI
	      (lshiftrt:SI
		(plus:SI
		  (zero_extend:SI
		    (vec_select:HI
		      (match_dup 2)
		      (parallel [(const_int 1)])))
		  (zero_extend:SI
		    (vec_select:HI
		      (match_dup 1)
		      (parallel [(const_int 0)]))))
		(const_int 1))))
	  (const_int 2)))]
  "NDS32_EXT_DSP_P () && TARGET_BIG_ENDIAN"
  "urcras16\t%0, %1, %2"
  [(set_attr "type" "dalu")]
)

(define_expand "rcrsa16_1"
  [(match_operand:V2HI 0 "register_operand" "")
   (match_operand:V2HI 1 "register_operand" "")
   (match_operand:V2HI 2 "register_operand" "")]
  "NDS32_EXT_DSP_P ()"
{
  if (TARGET_BIG_ENDIAN)
    emit_insn (gen_rcrsa16_1_be (operands[0], operands[1], operands[2]));
  else
    emit_insn (gen_rcrsa16_1_le (operands[0], operands[1], operands[2]));
  DONE;
})

(define_insn "rcrsa16_1_le"
  [(set (match_operand:V2HI 0 "register_operand"           "=r")
	(vec_merge:V2HI
	  (vec_duplicate:V2HI
	    (truncate:HI
	      (ashiftrt:SI
	        (minus:SI
		  (sign_extend:SI
		    (vec_select:HI
		      (match_operand:V2HI 1 "register_operand" " r")
		      (parallel [(const_int 1)])))
		  (sign_extend:SI
		    (vec_select:HI
		      (match_operand:V2HI 2 "register_operand" " r")
		      (parallel [(const_int 0)]))))
		(const_int 1))))
	  (vec_duplicate:V2HI
	    (truncate:HI
	      (ashiftrt:SI
		(plus:SI
		  (sign_extend:SI
		    (vec_select:HI
		      (match_dup 1)
		      (parallel [(const_int 0)])))
		  (sign_extend:SI
		    (vec_select:HI
		      (match_dup 2)
		      (parallel [(const_int 1)]))))
		(const_int 1))))
	  (const_int 2)))]
  "NDS32_EXT_DSP_P () && !TARGET_BIG_ENDIAN"
  "rcrsa16\t%0, %1, %2"
  [(set_attr "type" "dalu")]
)

(define_insn "rcrsa16_1_be"
  [(set (match_operand:V2HI 0 "register_operand"           "=r")
	(vec_merge:V2HI
	  (vec_duplicate:V2HI
	    (truncate:HI
	      (ashiftrt:SI
	        (minus:SI
		  (sign_extend:SI
		    (vec_select:HI
		      (match_operand:V2HI 1 "register_operand" " r")
		      (parallel [(const_int 0)])))
		  (sign_extend:SI
		    (vec_select:HI
		      (match_operand:V2HI 2 "register_operand" " r")
		      (parallel [(const_int 1)]))))
		(const_int 1))))
	  (vec_duplicate:V2HI
	    (truncate:HI
	      (ashiftrt:SI
		(plus:SI
		  (sign_extend:SI
		    (vec_select:HI
		      (match_dup 1)
		      (parallel [(const_int 1)])))
		  (sign_extend:SI
		    (vec_select:HI
		      (match_dup 2)
		      (parallel [(const_int 0)]))))
		(const_int 1))))
	  (const_int 1)))]
  "NDS32_EXT_DSP_P () && TARGET_BIG_ENDIAN"
  "rcrsa16\t%0, %1, %2"
  [(set_attr "type" "dalu")]
)

(define_expand "urcrsa16_1"
  [(match_operand:V2HI 0 "register_operand" "")
   (match_operand:V2HI 1 "register_operand" "")
   (match_operand:V2HI 2 "register_operand" "")]
  "NDS32_EXT_DSP_P ()"
{
  if (TARGET_BIG_ENDIAN)
    emit_insn (gen_urcrsa16_1_be (operands[0], operands[1], operands[2]));
  else
    emit_insn (gen_urcrsa16_1_le (operands[0], operands[1], operands[2]));
  DONE;
})

(define_insn "urcrsa16_1_le"
  [(set (match_operand:V2HI 0 "register_operand"           "=r")
	(vec_merge:V2HI
	  (vec_duplicate:V2HI
	    (truncate:HI
	      (lshiftrt:SI
	        (minus:SI
		  (zero_extend:SI
		    (vec_select:HI
		      (match_operand:V2HI 1 "register_operand" " r")
		      (parallel [(const_int 1)])))
		  (zero_extend:SI
		    (vec_select:HI
		      (match_operand:V2HI 2 "register_operand" " r")
		      (parallel [(const_int 0)]))))
		(const_int 1))))
	  (vec_duplicate:V2HI
	    (truncate:HI
	      (lshiftrt:SI
		(plus:SI
		  (zero_extend:SI
		    (vec_select:HI
		      (match_dup 1)
		      (parallel [(const_int 0)])))
		  (zero_extend:SI
		    (vec_select:HI
		      (match_dup 2)
		      (parallel [(const_int 1)]))))
		(const_int 1))))
	  (const_int 2)))]
  "NDS32_EXT_DSP_P () && !TARGET_BIG_ENDIAN"
  "urcrsa16\t%0, %1, %2"
  [(set_attr "type" "dalu")]
)

(define_insn "urcrsa16_1_be"
  [(set (match_operand:V2HI 0 "register_operand"           "=r")
	(vec_merge:V2HI
	  (vec_duplicate:V2HI
	    (truncate:HI
	      (lshiftrt:SI
	        (minus:SI
		  (zero_extend:SI
		    (vec_select:HI
		      (match_operand:V2HI 1 "register_operand" " r")
		      (parallel [(const_int 0)])))
		  (zero_extend:SI
		    (vec_select:HI
		      (match_operand:V2HI 2 "register_operand" " r")
		      (parallel [(const_int 1)]))))
		(const_int 1))))
	  (vec_duplicate:V2HI
	    (truncate:HI
	      (lshiftrt:SI
		(plus:SI
		  (zero_extend:SI
		    (vec_select:HI
		      (match_dup 1)
		      (parallel [(const_int 1)])))
		  (zero_extend:SI
		    (vec_select:HI
		      (match_dup 2)
		      (parallel [(const_int 0)]))))
		(const_int 1))))
	  (const_int 1)))]
  "NDS32_EXT_DSP_P () && TARGET_BIG_ENDIAN"
  "urcrsa16\t%0, %1, %2"
  [(set_attr "type" "dalu")]
)

(define_expand "<shift>v2hi3"
  [(set (match_operand:V2HI 0 "register_operand"                  "")
	(shifts:V2HI (match_operand:V2HI 1 "register_operand"     "")
		     (match_operand:SI   2 "nds32_rimm4u_operand" "")))]
  "NDS32_EXT_DSP_P ()"
{
  if (operands[2] == const0_rtx)
    {
      emit_move_insn (operands[0], operands[1]);
      DONE;
    }
})

(define_insn "*ashlv2hi3"
  [(set (match_operand:V2HI 0 "register_operand"                "=   r, r")
	(ashift:V2HI (match_operand:V2HI 1 "register_operand"   "    r, r")
		     (match_operand:SI 2 "nds32_rimm4u_operand" " Iu04, r")))]
  "NDS32_EXT_DSP_P ()"
  "@
   slli16\t%0, %1, %2
   sll16\t%0, %1, %2"
  [(set_attr "type"   "dalu,dalu")
   (set_attr "length" "   4,   4")])

(define_insn "kslli16"
  [(set (match_operand:V2HI 0 "register_operand"                   "=   r, r")
	(ss_ashift:V2HI (match_operand:V2HI 1 "register_operand"   "    r, r")
			(match_operand:SI 2 "nds32_rimm4u_operand" " Iu04, r")))]
  "NDS32_EXT_DSP_P ()"
  "@
   kslli16\t%0, %1, %2
   ksll16\t%0, %1, %2"
  [(set_attr "type"   "dalu,dalu")
   (set_attr "length" "   4,   4")])

(define_insn "*ashrv2hi3"
  [(set (match_operand:V2HI 0 "register_operand"                  "=   r, r")
	(ashiftrt:V2HI (match_operand:V2HI 1 "register_operand"   "    r, r")
		       (match_operand:SI 2 "nds32_rimm4u_operand" " Iu04, r")))]
  "NDS32_EXT_DSP_P ()"
  "@
   srai16\t%0, %1, %2
   sra16\t%0, %1, %2"
  [(set_attr "type"   "dalu,dalu")
   (set_attr "length" "   4,   4")])

(define_insn "sra16_round"
  [(set (match_operand:V2HI 0 "register_operand"                                "=   r, r")
	(unspec:V2HI [(ashiftrt:V2HI (match_operand:V2HI 1 "register_operand"   "    r, r")
				     (match_operand:SI 2 "nds32_rimm4u_operand" " Iu04, r"))]
		     UNSPEC_ROUND))]
  "NDS32_EXT_DSP_P ()"
  "@
   srai16.u\t%0, %1, %2
   sra16.u\t%0, %1, %2"
  [(set_attr "type"   "daluround,daluround")
   (set_attr "length" "         4,       4")])

(define_insn "*lshrv2hi3"
  [(set (match_operand:V2HI 0 "register_operand"                  "=   r, r")
	(lshiftrt:V2HI (match_operand:V2HI 1 "register_operand"   "    r, r")
		       (match_operand:SI 2 "nds32_rimm4u_operand" " Iu04, r")))]
  "NDS32_EXT_DSP_P ()"
  "@
   srli16\t%0, %1, %2
   srl16\t%0, %1, %2"
  [(set_attr "type"   "dalu,dalu")
   (set_attr "length" "   4,   4")])

(define_insn "srl16_round"
  [(set (match_operand:V2HI 0 "register_operand"                                "=   r, r")
	(unspec:V2HI [(lshiftrt:V2HI (match_operand:V2HI 1 "register_operand"   "    r, r")
				     (match_operand:SI 2 "nds32_rimm4u_operand" " Iu04, r"))]
		     UNSPEC_ROUND))]
  "NDS32_EXT_DSP_P ()"
  "@
   srli16.u\t%0, %1, %2
   srl16.u\t%0, %1, %2"
  [(set_attr "type"   "daluround,daluround")
   (set_attr "length" "        4,        4")])

(define_insn "kslra16"
  [(set (match_operand:V2HI 0 "register_operand"                  "=r")
	(if_then_else:V2HI
	  (lt:SI (match_operand:SI 2 "register_operand"           " r")
		 (const_int 0))
	  (ashiftrt:V2HI (match_operand:V2HI 1 "register_operand" " r")
			 (neg:SI (match_dup 2)))
	  (ashift:V2HI (match_dup 1)
		       (match_dup 2))))]
  "NDS32_EXT_DSP_P ()"
  "kslra16\t%0, %1, %2"
  [(set_attr "type"    "dalu")
   (set_attr "length"  "4")])

(define_insn "kslra16_round"
  [(set (match_operand:V2HI 0 "register_operand"                  "=r")
	(if_then_else:V2HI
	  (lt:SI (match_operand:SI 2 "register_operand"           " r")
		 (const_int 0))
	  (unspec:V2HI [(ashiftrt:V2HI (match_operand:V2HI 1 "register_operand" " r")
				       (neg:SI (match_dup 2)))]
		       UNSPEC_ROUND)
	  (ashift:V2HI (match_dup 1)
		       (match_dup 2))))]
  "NDS32_EXT_DSP_P ()"
  "kslra16.u\t%0, %1, %2"
  [(set_attr "type"    "daluround")
   (set_attr "length"  "4")])

(define_insn "cmpeq<bits>"
  [(set (match_operand:SI 0 "register_operand"                       "=r")
	(unspec:SI [(eq:SI (match_operand:VQIHI 1 "register_operand" " r")
			   (match_operand:VQIHI 2 "register_operand" " r"))]
		   UNSPEC_VEC_COMPARE))]
  "NDS32_EXT_DSP_P ()"
  "cmpeq<bits>\t%0, %1, %2"
  [(set_attr "type"    "dcmp")
   (set_attr "length"  "4")])

(define_insn "scmplt<bits>"
  [(set (match_operand:SI 0 "register_operand"                       "=r")
	(unspec:SI [(lt:SI (match_operand:VQIHI 1 "register_operand" " r")
			   (match_operand:VQIHI 2 "register_operand" " r"))]
		   UNSPEC_VEC_COMPARE))]
  "NDS32_EXT_DSP_P ()"
  "scmplt<bits>\t%0, %1, %2"
  [(set_attr "type"    "dcmp")
   (set_attr "length"  "4")])

(define_insn "scmple<bits>"
  [(set (match_operand:SI 0 "register_operand"                       "=r")
	(unspec:SI [(le:SI (match_operand:VQIHI 1 "register_operand" " r")
			   (match_operand:VQIHI 2 "register_operand" " r"))]
		   UNSPEC_VEC_COMPARE))]
  "NDS32_EXT_DSP_P ()"
  "scmple<bits>\t%0, %1, %2"
  [(set_attr "type"    "dcmp")
   (set_attr "length"  "4")])

(define_insn "ucmplt<bits>"
  [(set (match_operand:SI 0 "register_operand"                        "=r")
	(unspec:SI [(ltu:SI (match_operand:VQIHI 1 "register_operand" " r")
			    (match_operand:VQIHI 2 "register_operand" " r"))]
		   UNSPEC_VEC_COMPARE))]
  "NDS32_EXT_DSP_P ()"
  "ucmplt<bits>\t%0, %1, %2"
  [(set_attr "type"    "dcmp")
   (set_attr "length"  "4")])

(define_insn "ucmple<bits>"
  [(set (match_operand:SI 0 "register_operand"                        "=r")
	(unspec:SI [(leu:SI (match_operand:VQIHI 1 "register_operand" " r")
			    (match_operand:VQIHI 2 "register_operand" " r"))]
		   UNSPEC_VEC_COMPARE))]
  "NDS32_EXT_DSP_P ()"
  "ucmple<bits>\t%0, %1, %2"
  [(set_attr "type"    "dcmp")
   (set_attr "length"  "4")])

(define_insn "sclip16"
  [(set (match_operand:V2HI 0 "register_operand"                "=   r")
	(unspec:V2HI [(match_operand:V2HI 1 "register_operand"  "    r")
		      (match_operand:SI 2 "nds32_imm4u_operand" " Iu04")]
		     UNSPEC_CLIPS))]
  "NDS32_EXT_DSP_P ()"
  "sclip16\t%0, %1, %2"
  [(set_attr "type"    "dclip")
   (set_attr "length"  "4")])

(define_insn "uclip16"
  [(set (match_operand:V2HI 0 "register_operand"                "=   r")
	(unspec:V2HI [(match_operand:V2HI 1 "register_operand"  "    r")
		      (match_operand:SI 2 "nds32_imm4u_operand" " Iu04")]
		     UNSPEC_CLIP))]
  "NDS32_EXT_DSP_P ()"
  "uclip16\t%0, %1, %2"
  [(set_attr "type"    "dclip")
   (set_attr "length"  "4")])

(define_insn "khm16"
  [(set (match_operand:V2HI 0 "register_operand"                "=r")
	(unspec:V2HI [(match_operand:V2HI 1 "register_operand"  " r")
		      (match_operand:V2HI 2 "register_operand" "  r")]
		     UNSPEC_KHM))]
  "NDS32_EXT_DSP_P ()"
  "khm16\t%0, %1, %2"
  [(set_attr "type"    "dmul")
   (set_attr "length"  "4")])

(define_insn "khmx16"
  [(set (match_operand:V2HI 0 "register_operand"                "=r")
	(unspec:V2HI [(match_operand:V2HI 1 "register_operand"  " r")
		      (match_operand:V2HI 2 "register_operand" "  r")]
		     UNSPEC_KHMX))]
  "NDS32_EXT_DSP_P ()"
  "khmx16\t%0, %1, %2"
  [(set_attr "type"    "dmul")
   (set_attr "length"  "4")])

(define_expand "vec_setv4qi"
  [(match_operand:V4QI 0 "register_operand" "")
   (match_operand:QI 1 "register_operand" "")
   (match_operand:SI 2 "immediate_operand" "")]
  "NDS32_EXT_DSP_P ()"
{
  HOST_WIDE_INT pos = INTVAL (operands[2]);
  if (pos > 4)
    gcc_unreachable ();
  HOST_WIDE_INT elem = (HOST_WIDE_INT) 1 << pos;
  emit_insn (gen_vec_setv4qi_internal (operands[0], operands[1],
				       operands[0], GEN_INT (elem)));
  DONE;
})

(define_expand "insb"
  [(match_operand:V4QI 0 "register_operand" "")
   (match_operand:V4QI 1 "register_operand" "")
   (match_operand:SI 2 "register_operand" "")
   (match_operand:SI 3 "const_int_operand" "")]
  "NDS32_EXT_DSP_P ()"
{
  if (INTVAL (operands[3]) > 3 || INTVAL (operands[3]) < 0)
    gcc_unreachable ();

  rtx src = gen_reg_rtx (QImode);

  convert_move (src, operands[2], false);

  HOST_WIDE_INT selector_index;
  /* Big endian need reverse index. */
  if (TARGET_BIG_ENDIAN)
    selector_index = 4 - INTVAL (operands[3]) - 1;
  else
    selector_index = INTVAL (operands[3]);
  rtx selector = gen_int_mode (1 << selector_index, SImode);
  emit_insn (gen_vec_setv4qi_internal (operands[0], src,
				       operands[1], selector));
  DONE;
})

(define_expand "insvsi"
  [(set (zero_extract:SI (match_operand:SI 0 "register_operand" "")
			 (match_operand:SI 1 "const_int_operand" "")
			 (match_operand:SI 2 "nds32_insv_operand" ""))
	(match_operand:SI 3 "register_operand" ""))]
  "NDS32_EXT_DSP_P ()"
{
  if (INTVAL (operands[1]) != 8)
    FAIL;
}
  [(set_attr "type"    "dinsb")
   (set_attr "length"  "4")])


(define_insn "insvsi_internal"
  [(set (zero_extract:SI (match_operand:SI 0 "register_operand"   "+r")
			 (const_int 8)
			 (match_operand:SI 1 "nds32_insv_operand"  "i"))
	(match_operand:SI 2                  "register_operand"    "r"))]
  "NDS32_EXT_DSP_P ()"
  "insb\t%0, %2, %v1"
  [(set_attr "type"    "dinsb")
   (set_attr "length"  "4")])

(define_insn "insvsiqi_internal"
  [(set (zero_extract:SI (match_operand:SI 0 "register_operand"   "+r")
			 (const_int 8)
			 (match_operand:SI 1 "nds32_insv_operand"  "i"))
	(zero_extend:SI (match_operand:QI 2 "register_operand"    "r")))]
  "NDS32_EXT_DSP_P ()"
  "insb\t%0, %2, %v1"
  [(set_attr "type"    "dinsb")
   (set_attr "length"  "4")])

;; Intermedium pattern for synthetize insvsiqi_internal
;; v0 = ((v1 & 0xff) << 8)
(define_insn_and_split "and0xff_s8"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(and:SI (ashift:SI (match_operand:SI 1 "register_operand" "r")
			   (const_int 8))
		(const_int 65280)))]
  "NDS32_EXT_DSP_P () && !reload_completed"
  "#"
  "NDS32_EXT_DSP_P () && !reload_completed"
  [(const_int 1)]
{
  rtx tmp = gen_reg_rtx (SImode);
  emit_insn (gen_ashlsi3 (tmp, operands[1], gen_int_mode (8, SImode)));
  emit_insn (gen_andsi3 (operands[0], tmp, gen_int_mode (0xffff, SImode)));
  DONE;
})

;; v0 = (v1 & 0xff00ffff) | ((v2 << 16) | 0xff0000)
(define_insn_and_split "insbsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ior:SI (and:SI (match_operand:SI 1 "register_operand" "0")
			(const_int -16711681))
		(and:SI (ashift:SI (match_operand:SI 2 "register_operand" "r")
				   (const_int 16))
			(const_int 16711680))))]
  "NDS32_EXT_DSP_P () && !reload_completed"
  "#"
  "NDS32_EXT_DSP_P () && !reload_completed"
  [(const_int 1)]
{
  rtx tmp = gen_reg_rtx (SImode);
  emit_move_insn (tmp, operands[1]);
  emit_insn (gen_insvsi_internal (tmp, gen_int_mode(16, SImode), operands[2]));
  emit_move_insn (operands[0], tmp);
  DONE;
})

;; v0 = (v1 & 0xff00ffff) | v2
(define_insn_and_split "ior_and0xff00ffff_reg"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ior:SI (and:SI (match_operand:SI 1 "register_operand" "r")
			(const_int -16711681))
		(match_operand:SI 2 "register_operand" "r")))]
  "NDS32_EXT_DSP_P () && !reload_completed"
  "#"
  "NDS32_EXT_DSP_P () && !reload_completed"
  [(const_int 1)]
{
  rtx tmp = gen_reg_rtx (SImode);
  emit_insn (gen_andsi3 (tmp, operands[1], gen_int_mode (0xff00ffff, SImode)));
  emit_insn (gen_iorsi3 (operands[0], tmp, operands[2]));
  DONE;
})

(define_insn "vec_setv4qi_internal"
  [(set (match_operand:V4QI 0 "register_operand"          "=   r,    r,    r,    r")
	(vec_merge:V4QI
	  (vec_duplicate:V4QI
	    (match_operand:QI 1 "register_operand"        "    r,    r,    r,    r"))
	  (match_operand:V4QI 2 "register_operand"        "    0,    0,    0,    0")
	  (match_operand:SI 3 "nds32_imm_1_2_4_8_operand" " Iv01, Iv02, Iv04, Iv08")))]
  "NDS32_EXT_DSP_P ()"
{
  if (TARGET_BIG_ENDIAN)
    {
       const char *pats[] = { "insb\t%0, %1, 3",
			      "insb\t%0, %1, 2",
			      "insb\t%0, %1, 1",
			      "insb\t%0, %1, 0" };
      return pats[which_alternative];
    }
  else
    {
       const char *pats[] = { "insb\t%0, %1, 0",
			      "insb\t%0, %1, 1",
			      "insb\t%0, %1, 2",
			      "insb\t%0, %1, 3" };
      return pats[which_alternative];
    }
}
  [(set_attr "type"    "dinsb")
   (set_attr "length"  "4")])

(define_insn "vec_setv4qi_internal_vec"
  [(set (match_operand:V4QI 0 "register_operand"          "=   r,    r,    r,    r")
	(vec_merge:V4QI
	  (vec_duplicate:V4QI
	    (vec_select:QI
	      (match_operand:V4QI 1 "register_operand"    "    r,    r,    r,    r")
	      (parallel [(const_int 0)])))
	  (match_operand:V4QI 2 "register_operand"        "    0,    0,    0,    0")
	  (match_operand:SI 3 "nds32_imm_1_2_4_8_operand" " Iv01, Iv02, Iv04, Iv08")))]
  "NDS32_EXT_DSP_P () && !TARGET_BIG_ENDIAN"
  "@
   insb\t%0, %1, 0
   insb\t%0, %1, 1
   insb\t%0, %1, 2
   insb\t%0, %1, 3"
  [(set_attr "type"    "dinsb")
   (set_attr "length"  "4")])

(define_insn "vec_mergev4qi_and_cv0_1"
  [(set (match_operand:V4QI 0 "register_operand"       "=$l,r")
	(vec_merge:V4QI
	  (vec_duplicate:V4QI
	    (vec_select:QI
	      (match_operand:V4QI 1 "register_operand" "  l,r")
	      (parallel [(const_int 0)])))
	  (const_vector:V4QI [
	    (const_int 0)
	    (const_int 0)
	    (const_int 0)
	    (const_int 0)])
	  (const_int 1)))]
  "NDS32_EXT_DSP_P () && !TARGET_BIG_ENDIAN"
  "@
   zeb33\t%0, %1
   zeb\t%0, %1"
  [(set_attr "type"    "alu,alu")
   (set_attr "length"  "  2,  4")])

(define_insn "vec_mergev4qi_and_cv0_2"
  [(set (match_operand:V4QI 0 "register_operand"       "=$l,r")
	(vec_merge:V4QI
	  (const_vector:V4QI [
	    (const_int 0)
	    (const_int 0)
	    (const_int 0)
	    (const_int 0)])
	  (vec_duplicate:V4QI
	    (vec_select:QI
	      (match_operand:V4QI 1 "register_operand" "  l,r")
	      (parallel [(const_int 0)])))
	  (const_int 2)))]
  "NDS32_EXT_DSP_P () && !TARGET_BIG_ENDIAN"
  "@
   zeb33\t%0, %1
   zeb\t%0, %1"
  [(set_attr "type"    "alu,alu")
   (set_attr "length"  "  2,  4")])

(define_insn "vec_mergeqi_and_cv0_1"
  [(set (match_operand:V4QI 0 "register_operand"                     "=$l,r")
	(vec_merge:V4QI
	  (vec_duplicate:V4QI (match_operand:QI 1 "register_operand" "  l,r"))
	  (const_vector:V4QI [
	    (const_int 0)
	    (const_int 0)
	    (const_int 0)
	    (const_int 0)])
	  (const_int 1)))]
  "NDS32_EXT_DSP_P () && !TARGET_BIG_ENDIAN"
  "@
   zeb33\t%0, %1
   zeb\t%0, %1"
  [(set_attr "type"    "alu,alu")
   (set_attr "length"  "  2,  4")])

(define_insn "vec_mergeqi_and_cv0_2"
  [(set (match_operand:V4QI 0 "register_operand"                     "=$l,r")
	(vec_merge:V4QI
	  (const_vector:V4QI [
	    (const_int 0)
	    (const_int 0)
	    (const_int 0)
	    (const_int 0)])
	  (vec_duplicate:V4QI (match_operand:QI 1 "register_operand" "  l,r"))
	  (const_int 2)))]
  "NDS32_EXT_DSP_P () && !TARGET_BIG_ENDIAN"
  "@
   zeb33\t%0, %1
   zeb\t%0, %1"
  [(set_attr "type"    "alu,alu")
   (set_attr "length"  "  2,  4")])

(define_expand "vec_setv2hi"
  [(match_operand:V2HI 0 "register_operand" "")
   (match_operand:HI 1 "register_operand" "")
   (match_operand:SI 2 "immediate_operand" "")]
  "NDS32_EXT_DSP_P ()"
{
  HOST_WIDE_INT pos = INTVAL (operands[2]);
  if (pos > 2)
    gcc_unreachable ();
  HOST_WIDE_INT elem = (HOST_WIDE_INT) 1 << pos;
  emit_insn (gen_vec_setv2hi_internal (operands[0], operands[1],
				       operands[0], GEN_INT (elem)));
  DONE;
})

(define_insn "vec_setv2hi_internal"
  [(set (match_operand:V2HI 0 "register_operand"      "=   r,    r")
	(vec_merge:V2HI
	  (vec_duplicate:V2HI
	    (match_operand:HI 1 "register_operand"    "    r,    r"))
	  (match_operand:V2HI 2 "register_operand"    "    r,    r")
	  (match_operand:SI 3 "nds32_imm_1_2_operand" " Iv01, Iv02")))]
  "NDS32_EXT_DSP_P ()"
{
  if (TARGET_BIG_ENDIAN)
    {
      const char *pats[] = { "pkbb16\t%0, %1, %2",
			     "pktb16\t%0, %2, %1" };
      return pats[which_alternative];
    }
  else
    {
      const char *pats[] = { "pktb16\t%0, %2, %1",
			     "pkbb16\t%0, %1, %2" };
      return pats[which_alternative];
    }
}
  [(set_attr "type"    "dpack")
   (set_attr "length"  "4")])

(define_insn "vec_mergev2hi_and_cv0_1"
  [(set (match_operand:V2HI 0 "register_operand"       "=$l,r")
	(vec_merge:V2HI
	  (vec_duplicate:V2HI
	    (vec_select:HI
	      (match_operand:V2HI 1 "register_operand" "  l,r")
	      (parallel [(const_int 0)])))
	  (const_vector:V2HI [
	    (const_int 0)
	    (const_int 0)])
	  (const_int 1)))]
  "NDS32_EXT_DSP_P () && !TARGET_BIG_ENDIAN"
  "@
   zeh33\t%0, %1
   zeh\t%0, %1"
  [(set_attr "type"    "alu,alu")
   (set_attr "length"  "  2,  4")])

(define_insn "vec_mergev2hi_and_cv0_2"
  [(set (match_operand:V2HI 0 "register_operand"       "=$l,r")
	(vec_merge:V2HI
	  (const_vector:V2HI [
	    (const_int 0)
	    (const_int 0)])
	  (vec_duplicate:V2HI
	    (vec_select:HI
	      (match_operand:V2HI 1 "register_operand" "  l,r")
	      (parallel [(const_int 0)])))
	  (const_int 2)))]
  "NDS32_EXT_DSP_P () && !TARGET_BIG_ENDIAN"
  "@
   zeh33\t%0, %1
   zeh\t%0, %1"
  [(set_attr "type"    "alu,alu")
   (set_attr "length"  "  2,  4")])

(define_insn "vec_mergehi_and_cv0_1"
  [(set (match_operand:V2HI 0 "register_operand"                     "=$l,r")
	(vec_merge:V2HI
	  (vec_duplicate:V2HI (match_operand:HI 1 "register_operand" "  l,r"))
	  (const_vector:V2HI [
	    (const_int 0)
	    (const_int 0)])
	  (const_int 1)))]
  "NDS32_EXT_DSP_P () && !TARGET_BIG_ENDIAN"
  "@
   zeh33\t%0, %1
   zeh\t%0, %1"
  [(set_attr "type"    "alu,alu")
   (set_attr "length"  "  2,  4")])

(define_insn "vec_mergehi_and_cv0_2"
  [(set (match_operand:V2HI 0 "register_operand"                     "=$l,r")
	(vec_merge:V2HI
	  (const_vector:V2HI [
	    (const_int 0)
	    (const_int 0)])
	  (vec_duplicate:V2HI (match_operand:HI 1 "register_operand" "  l,r"))
	  (const_int 2)))]
  "NDS32_EXT_DSP_P () && !TARGET_BIG_ENDIAN"
  "@
   zeh33\t%0, %1
   zeh\t%0, %1"
  [(set_attr "type"    "alu,alu")
   (set_attr "length"  "  2,  4")])

(define_expand "pkbb"
  [(match_operand:V2HI 0 "register_operand")
   (match_operand:V2HI 1 "register_operand")
   (match_operand:V2HI 2 "register_operand")]
  "NDS32_EXT_DSP_P ()"
{
  if (TARGET_BIG_ENDIAN)
    {
      emit_insn (gen_vec_mergevv (operands[0], operands[1], operands[2],
				  GEN_INT (1), GEN_INT (1), GEN_INT (1)));
    }
  else
    {
      emit_insn (gen_vec_mergevv (operands[0], operands[1], operands[2],
				  GEN_INT (2), GEN_INT (0), GEN_INT (0)));
    }
  DONE;
})

(define_insn "pkbbsi_1"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ior:SI (and:SI (match_operand:SI 1 "register_operand" "r")
			(const_int 65535))
		(ashift:SI (match_operand:SI 2 "register_operand" "r")
			   (const_int 16))))]
  "NDS32_EXT_DSP_P ()"
  "pkbb16\t%0, %2, %1"
  [(set_attr "type"    "dpack")
   (set_attr "length"  "4")])

(define_insn "pkbbsi_2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ior:SI	(ashift:SI (match_operand:SI 2 "register_operand" "r")
			   (const_int 16))
		(and:SI (match_operand:SI 1 "register_operand" "r")
			(const_int 65535))))]
  "NDS32_EXT_DSP_P ()"
  "pkbb16\t%0, %2, %1"
  [(set_attr "type"    "dpack")
   (set_attr "length"  "4")])

(define_insn "pkbbsi_3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ior:SI (zero_extend:SI	(match_operand:HI 1 "register_operand" "r"))
		(ashift:SI (match_operand:SI 2 "register_operand" "r")
			   (const_int 16))))]
  "NDS32_EXT_DSP_P ()"
  "pkbb16\t%0, %2, %1"
  [(set_attr "type"    "dpack")
   (set_attr "length"  "4")])

(define_insn "pkbbsi_4"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ior:SI	(ashift:SI (match_operand:SI 2 "register_operand" "r")
			   (const_int 16))
		(zero_extend:SI (match_operand:HI 1 "register_operand" "r"))))]
  "NDS32_EXT_DSP_P ()"
  "pkbb16\t%0, %2, %1"
  [(set_attr "type"    "dpack")
   (set_attr "length"  "4")])

;; v0 = (v1 & 0xffff0000) | (v2 & 0xffff)
(define_insn "pktbsi_1"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ior:SI (and:SI (match_operand:SI 1 "register_operand" "r")
			(const_int -65536))
		(zero_extend:SI (match_operand:HI 2 "register_operand" "r"))))]
  "NDS32_EXT_DSP_P ()"
  "pktb16\t%0, %1, %2"
  [(set_attr "type"    "dpack")
   (set_attr "length"  "4")])

(define_insn "pktbsi_2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ior:SI (and:SI (match_operand:SI 1 "register_operand" "r")
			(const_int -65536))
		(and:SI (match_operand:SI 2 "register_operand" "r")
			(const_int 65535))))]
  "NDS32_EXT_DSP_P ()"
  "pktb16\t%0, %1, %2"
  [(set_attr "type"    "alu")
   (set_attr "length"  "4")])

(define_insn "pktbsi_3"
  [(set (zero_extract:SI (match_operand:SI 0 "register_operand" "+r")
			 (const_int 16 )
			 (const_int 0))
	(match_operand:SI 1 "register_operand"                  " r"))]
  "NDS32_EXT_DSP_P ()"
  "pktb16\t%0, %0, %1"
  [(set_attr "type"    "dpack")
   (set_attr "length"  "4")])

(define_insn "pktbsi_4"
  [(set (zero_extract:SI (match_operand:SI 0 "register_operand" "+r")
			 (const_int 16 )
			 (const_int 0))
	(zero_extend:SI (match_operand:HI 1 "register_operand"  " r")))]
  "NDS32_EXT_DSP_P ()"
  "pktb16\t%0, %0, %1"
  [(set_attr "type"    "dpack")
   (set_attr "length"  "4")])

(define_insn "pkttsi"
  [(set (match_operand:SI 0 "register_operand"                      "=r")
	(ior:SI (and:SI (match_operand:SI 1 "register_operand"      " r")
			(const_int -65536))
		(lshiftrt:SI (match_operand:SI 2 "register_operand" " r")
			     (const_int 16))))]
  "NDS32_EXT_DSP_P ()"
  "pktt16\t%0, %1, %2"
  [(set_attr "type"    "dpack")
   (set_attr "length"  "4")])

(define_expand "pkbt"
  [(match_operand:V2HI 0 "register_operand")
   (match_operand:V2HI 1 "register_operand")
   (match_operand:V2HI 2 "register_operand")]
  "NDS32_EXT_DSP_P ()"
{
  if (TARGET_BIG_ENDIAN)
    {
      emit_insn (gen_vec_mergevv (operands[0], operands[1], operands[2],
				  GEN_INT (1), GEN_INT (1), GEN_INT (0)));
    }
  else
    {
      emit_insn (gen_vec_mergevv (operands[0], operands[1], operands[2],
				  GEN_INT (2), GEN_INT (0), GEN_INT (1)));
    }
  DONE;
})

(define_expand "pktt"
  [(match_operand:V2HI 0 "register_operand")
   (match_operand:V2HI 1 "register_operand")
   (match_operand:V2HI 2 "register_operand")]
  "NDS32_EXT_DSP_P ()"
{
  if (TARGET_BIG_ENDIAN)
    {
      emit_insn (gen_vec_mergevv (operands[0], operands[1], operands[2],
				  GEN_INT (1), GEN_INT (0), GEN_INT (0)));
    }
  else
    {
      emit_insn (gen_vec_mergevv (operands[0], operands[1], operands[2],
				  GEN_INT (2), GEN_INT (1), GEN_INT (1)));
    }
  DONE;
})

(define_expand "pktb"
  [(match_operand:V2HI 0 "register_operand")
   (match_operand:V2HI 1 "register_operand")
   (match_operand:V2HI 2 "register_operand")]
  "NDS32_EXT_DSP_P ()"
{
  if (TARGET_BIG_ENDIAN)
    {
      emit_insn (gen_vec_mergevv (operands[0], operands[1], operands[2],
				  GEN_INT (1), GEN_INT (0), GEN_INT (1)));
    }
  else
    {
      emit_insn (gen_vec_mergevv (operands[0], operands[1], operands[2],
				  GEN_INT (2), GEN_INT (1), GEN_INT (0)));
    }
  DONE;
})

(define_insn "vec_mergerr"
  [(set (match_operand:V2HI 0 "register_operand"      "=   r,    r")
	(vec_merge:V2HI
	  (vec_duplicate:V2HI
	    (match_operand:HI 1 "register_operand"    "    r,    r"))
	  (vec_duplicate:V2HI
	    (match_operand:HI 2 "register_operand"    "    r,    r"))
	  (match_operand:SI 3 "nds32_imm_1_2_operand" " Iv01, Iv02")))]
  "NDS32_EXT_DSP_P () && !TARGET_BIG_ENDIAN"
  "@
   pkbb16\t%0, %2, %1
   pkbb16\t%0, %1, %2"
  [(set_attr "type"    "dpack")
   (set_attr "length"  "4")])


(define_insn "vec_merge"
  [(set (match_operand:V2HI 0 "register_operand"      "=   r,    r")
	(vec_merge:V2HI
	  (match_operand:V2HI 1 "register_operand"    "    r,    r")
	  (match_operand:V2HI 2 "register_operand"    "    r,    r")
	  (match_operand:SI 3 "nds32_imm_1_2_operand" " Iv01, Iv02")))]
  "NDS32_EXT_DSP_P ()"
{
  if (TARGET_BIG_ENDIAN)
    {
      const char *pats[] = { "pktb16\t%0, %1, %2",
			     "pktb16\t%0, %2, %1" };
      return pats[which_alternative];
    }
  else
    {
      const char *pats[] = { "pktb16\t%0, %2, %1",
			     "pktb16\t%0, %1, %2" };
      return pats[which_alternative];
    }
}
  [(set_attr "type"    "dpack")
   (set_attr "length"  "4")])

(define_insn "vec_mergerv"
  [(set (match_operand:V2HI 0 "register_operand"                     "=   r,    r,    r,    r")
	(vec_merge:V2HI
	  (vec_duplicate:V2HI
	    (match_operand:HI 1 "register_operand"                   "    r,    r,    r,    r"))
	  (vec_duplicate:V2HI
	    (vec_select:HI
	      (match_operand:V2HI 2 "register_operand"               "    r,    r,    r,    r")
	      (parallel [(match_operand:SI 4 "nds32_imm_0_1_operand" " Iv00, Iv01, Iv00, Iv01")])))
	  (match_operand:SI 3 "nds32_imm_1_2_operand"                " Iv01, Iv01, Iv02, Iv02")))]
  "NDS32_EXT_DSP_P () && !TARGET_BIG_ENDIAN"
  "@
   pkbb16\t%0, %2, %1
   pktb16\t%0, %2, %1
   pkbb16\t%0, %1, %2
   pkbt16\t%0, %1, %2"
  [(set_attr "type"    "dpack")
   (set_attr "length"  "4")])

(define_insn "vec_mergevr"
  [(set (match_operand:V2HI 0 "register_operand"                      "=   r,    r,    r,    r")
	(vec_merge:V2HI
	  (vec_duplicate:V2HI
	    (vec_select:HI
	      (match_operand:V2HI 1 "register_operand"                "    r,    r,    r,    r")
	       (parallel [(match_operand:SI 4 "nds32_imm_0_1_operand" " Iv00, Iv01, Iv00, Iv01")])))
	  (vec_duplicate:V2HI
	    (match_operand:HI 2 "register_operand"                    "    r,    r,    r,    r"))
	  (match_operand:SI 3 "nds32_imm_1_2_operand"                 " Iv01, Iv01, Iv02, Iv02")))]
  "NDS32_EXT_DSP_P () && !TARGET_BIG_ENDIAN"
  "@
   pkbb16\t%0, %2, %1
   pkbt16\t%0, %2, %1
   pkbb16\t%0, %1, %2
   pktb16\t%0, %1, %2"
  [(set_attr "type"    "dpack")
   (set_attr "length"  "4")])

(define_insn "vec_mergevv"
  [(set (match_operand:V2HI 0 "register_operand"                     "=   r,    r,    r,    r,    r,    r,    r,    r")
	(vec_merge:V2HI
	  (vec_duplicate:V2HI
	    (vec_select:HI
	      (match_operand:V2HI 1 "register_operand"               "    r,    r,    r,    r,    r,    r,    r,    r")
	      (parallel [(match_operand:SI 4 "nds32_imm_0_1_operand" " Iv00, Iv00, Iv01, Iv01, Iv00, Iv00, Iv01, Iv01")])))
	  (vec_duplicate:V2HI
	    (vec_select:HI
	      (match_operand:V2HI 2 "register_operand"               "    r,    r,    r,    r,    r,    r,    r,    r")
	      (parallel [(match_operand:SI 5 "nds32_imm_0_1_operand" " Iv00, Iv01, Iv01, Iv00, Iv00, Iv01, Iv01, Iv00")])))
	  (match_operand:SI 3 "nds32_imm_1_2_operand"                " Iv01, Iv01, Iv01, Iv01, Iv02, Iv02, Iv02, Iv02")))]
  "NDS32_EXT_DSP_P ()"
{
  if (TARGET_BIG_ENDIAN)
    {
      const char *pats[] = { "pktt16\t%0, %1, %2",
			     "pktb16\t%0, %1, %2",
			     "pkbb16\t%0, %1, %2",
			     "pkbt16\t%0, %1, %2",
			     "pktt16\t%0, %2, %1",
			     "pkbt16\t%0, %2, %1",
			     "pkbb16\t%0, %2, %1",
			     "pktb16\t%0, %2, %1" };
      return pats[which_alternative];
    }
  else
    {
      const char *pats[] = { "pkbb16\t%0, %2, %1",
			     "pktb16\t%0, %2, %1",
			     "pktt16\t%0, %2, %1",
			     "pkbt16\t%0, %2, %1",
			     "pkbb16\t%0, %1, %2",
			     "pkbt16\t%0, %1, %2",
			     "pktt16\t%0, %1, %2",
			     "pktb16\t%0, %1, %2" };
      return pats[which_alternative];
    }
}
  [(set_attr "type"    "dpack")
   (set_attr "length"  "4")])

(define_expand "vec_extractv4qi"
  [(set (match_operand:QI 0 "register_operand" "")
	(vec_select:QI
	  (match_operand:V4QI 1          "nonimmediate_operand" "")
	  (parallel [(match_operand:SI 2 "const_int_operand" "")])))]
  "NDS32_EXT_DSP_P () && !TARGET_BIG_ENDIAN"
{
  if (INTVAL (operands[2]) != 0
      && INTVAL (operands[2]) != 1
      && INTVAL (operands[2]) != 2
      && INTVAL (operands[2]) != 3)
    gcc_unreachable ();

  if (INTVAL (operands[2]) != 0 && MEM_P (operands[0]))
    FAIL;
})

(define_insn "vec_extractv4qi0"
  [(set (match_operand:QI 0 "register_operand"         "=l,r,r")
	(vec_select:QI
	  (match_operand:V4QI 1 "nonimmediate_operand" " l,r,m")
	  (parallel [(const_int 0)])))]
  "NDS32_EXT_DSP_P () && !TARGET_BIG_ENDIAN"
{
  switch (which_alternative)
    {
    case 0:
      return "zeb33\t%0, %1";
    case 1:
      return "zeb\t%0, %1";
    case 2:
      return nds32_output_32bit_load (operands, 1);
    default:
      gcc_unreachable ();
    }
}
  [(set_attr "type"    "alu")
   (set_attr "length"  "4")])

(define_insn "vec_extractv4qi0_ze"
  [(set (match_operand:SI 0 "register_operand"         "=l,r,r")
	(zero_extend:SI
	  (vec_select:QI
	    (match_operand:V4QI 1 "nonimmediate_operand" " l,r,m")
	    (parallel [(const_int 0)]))))]
  "NDS32_EXT_DSP_P () && !TARGET_BIG_ENDIAN"
{
  switch (which_alternative)
    {
    case 0:
      return "zeb33\t%0, %1";
    case 1:
      return "zeb\t%0, %1";
    case 2:
      return nds32_output_32bit_load (operands, 1);
    default:
      gcc_unreachable ();
    }
}
  [(set_attr "type"    "alu")
   (set_attr "length"  "4")])

(define_insn "vec_extractv4qi0_se"
  [(set (match_operand:SI 0 "register_operand"         "=l,r,r")
	(sign_extend:SI
	  (vec_select:QI
	    (match_operand:V4QI 1 "nonimmediate_operand" " l,r,m")
	    (parallel [(const_int 0)]))))]
  "NDS32_EXT_DSP_P () && !TARGET_BIG_ENDIAN"
{
  switch (which_alternative)
    {
    case 0:
      return "seb33\t%0, %1";
    case 1:
      return "seb\t%0, %1";
    case 2:
      return nds32_output_32bit_load_s (operands, 1);
    default:
      gcc_unreachable ();
    }
}
  [(set_attr "type"    "alu")
   (set_attr "length"  "4")])

(define_insn_and_split "vec_extractv4qi1"
  [(set (match_operand:QI 0 "register_operand" "=r")
	(vec_select:QI
	  (match_operand:V4QI 1 "register_operand" " r")
	  (parallel [(const_int 1)])))]
  "NDS32_EXT_DSP_P () && !reload_completed && !TARGET_BIG_ENDIAN"
  "#"
  "NDS32_EXT_DSP_P () && !reload_completed"
  [(const_int 1)]
{
  rtx tmp = gen_reg_rtx (V4QImode);
  emit_insn (gen_rotrv4qi_1 (tmp, operands[1]));
  emit_insn (gen_vec_extractv4qi0 (operands[0], tmp));
  DONE;
}
  [(set_attr "type"    "alu")
   (set_attr "length"  "4")])

(define_insn_and_split "vec_extractv4qi2"
  [(set (match_operand:QI 0 "register_operand" "=r")
	(vec_select:QI
	  (match_operand:V4QI 1 "register_operand" " r")
	  (parallel [(const_int 2)])))]
  "NDS32_EXT_DSP_P () && !reload_completed && !TARGET_BIG_ENDIAN"
  "#"
  "NDS32_EXT_DSP_P () && !reload_completed"
  [(const_int 1)]
{
  rtx tmp = gen_reg_rtx (V4QImode);
  emit_insn (gen_rotrv4qi_2 (tmp, operands[1]));
  emit_insn (gen_vec_extractv4qi0 (operands[0], tmp));
  DONE;
}
  [(set_attr "type"    "alu")
   (set_attr "length"  "4")])

(define_insn_and_split "vec_extractv4qi3"
  [(set (match_operand:QI 0 "register_operand" "=r")
	(vec_select:QI
	  (match_operand:V4QI 1 "register_operand" " r")
	  (parallel [(const_int 3)])))]
  "NDS32_EXT_DSP_P () && !reload_completed && !TARGET_BIG_ENDIAN"
  "#"
  "NDS32_EXT_DSP_P () && !reload_completed"
  [(const_int 1)]
{
  rtx tmp = gen_reg_rtx (V4QImode);
  emit_insn (gen_rotrv4qi_3 (tmp, operands[1]));
  emit_insn (gen_vec_extractv4qi0 (operands[0], tmp));
  DONE;
}
  [(set_attr "type"    "alu")
   (set_attr "length"  "4")])

(define_insn "vec_extractv4qi3_se"
  [(set (match_operand:SI 0 "register_operand"       "=$d,r")
	(sign_extend:SI
	  (vec_select:QI
	    (match_operand:V4QI 1 "register_operand" "  0,r")
	    (parallel [(const_int 3)]))))]
  "NDS32_EXT_DSP_P () && !TARGET_BIG_ENDIAN"
  "@
   srai45\t%0, 24
   srai\t%0, %1, 24"
  [(set_attr "type"    "alu,alu")
   (set_attr "length"  "  2,  4")])

(define_insn "vec_extractv4qi3_ze"
  [(set (match_operand:SI 0 "register_operand"       "=$d,r")
	(zero_extend:SI
	  (vec_select:QI
	    (match_operand:V4QI 1 "register_operand" "  0,r")
	    (parallel [(const_int 3)]))))]
  "NDS32_EXT_DSP_P () && !TARGET_BIG_ENDIAN"
  "@
   srli45\t%0, 24
   srli\t%0, %1, 24"
  [(set_attr "type"    "alu,alu")
   (set_attr "length"  "  2,  4")])

(define_insn_and_split "vec_extractv4qihi0"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(sign_extend:HI
	  (vec_select:QI
	    (match_operand:V4QI 1 "register_operand" " r")
	    (parallel [(const_int 0)]))))]
  "NDS32_EXT_DSP_P () && !reload_completed && !TARGET_BIG_ENDIAN"
  "#"
  "NDS32_EXT_DSP_P () && !reload_completed"
  [(const_int 1)]
{
  rtx tmp = gen_reg_rtx (QImode);
  emit_insn (gen_vec_extractv4qi0 (tmp, operands[1]));
  emit_insn (gen_extendqihi2 (operands[0], tmp));
  DONE;
}
  [(set_attr "type"    "alu")
   (set_attr "length"  "4")])

(define_insn_and_split "vec_extractv4qihi1"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(sign_extend:HI
	  (vec_select:QI
	    (match_operand:V4QI 1 "register_operand" " r")
	    (parallel [(const_int 1)]))))]
  "NDS32_EXT_DSP_P () && !reload_completed && !TARGET_BIG_ENDIAN"
  "#"
  "NDS32_EXT_DSP_P () && !reload_completed"
  [(const_int 1)]
{
  rtx tmp = gen_reg_rtx (QImode);
  emit_insn (gen_vec_extractv4qi1 (tmp, operands[1]));
  emit_insn (gen_extendqihi2 (operands[0], tmp));
  DONE;
}
  [(set_attr "type"    "alu")
   (set_attr "length"  "4")])

(define_insn_and_split "vec_extractv4qihi2"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(sign_extend:HI
	  (vec_select:QI
	    (match_operand:V4QI 1 "register_operand" " r")
	    (parallel [(const_int 2)]))))]
  "NDS32_EXT_DSP_P () && !reload_completed && !TARGET_BIG_ENDIAN"
  "#"
  "NDS32_EXT_DSP_P () && !reload_completed"
  [(const_int 1)]
{
  rtx tmp = gen_reg_rtx (QImode);
  emit_insn (gen_vec_extractv4qi2 (tmp, operands[1]));
  emit_insn (gen_extendqihi2 (operands[0], tmp));
  DONE;
}
  [(set_attr "type"    "alu")
   (set_attr "length"  "4")])

(define_insn_and_split "vec_extractv4qihi3"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(sign_extend:HI
	  (vec_select:QI
	    (match_operand:V4QI 1 "register_operand" " r")
	    (parallel [(const_int 3)]))))]
  "NDS32_EXT_DSP_P () && !reload_completed && !TARGET_BIG_ENDIAN"
  "#"
  "NDS32_EXT_DSP_P () && !reload_completed"
  [(const_int 1)]
{
  rtx tmp = gen_reg_rtx (QImode);
  emit_insn (gen_vec_extractv4qi3 (tmp, operands[1]));
  emit_insn (gen_extendqihi2 (operands[0], tmp));
  DONE;
}
  [(set_attr "type"    "alu")
   (set_attr "length"  "4")])

(define_expand "vec_extractv2hi"
  [(set (match_operand:HI 0 "register_operand" "")
	(vec_select:HI
	  (match_operand:V2HI 1          "nonimmediate_operand" "")
	  (parallel [(match_operand:SI 2 "const_int_operand" "")])))]
  "NDS32_EXT_DSP_P ()"
{
  if (INTVAL (operands[2]) != 0
      && INTVAL (operands[2]) != 1)
    gcc_unreachable ();

  if (INTVAL (operands[2]) != 0 && MEM_P (operands[0]))
    FAIL;
})

(define_insn "vec_extractv2hi0"
  [(set (match_operand:HI 0 "register_operand"         "=$l,r,r")
	(vec_select:HI
	  (match_operand:V2HI 1 "nonimmediate_operand" "  l,r,m")
	  (parallel [(const_int 0)])))]
  "NDS32_EXT_DSP_P () && !TARGET_BIG_ENDIAN"
{
  switch (which_alternative)
    {
    case 0:
      return "seh33\t%0, %1";
    case 1:
      return "seh\t%0, %1";
    case 2:
      return nds32_output_32bit_load_s (operands, 2);

    default:
      gcc_unreachable ();
    }
}
  [(set_attr "type"    "alu,alu,load")
   (set_attr "length"  "  2,  4,   4")])

(define_insn "vec_extractv2hi0_ze"
  [(set (match_operand:SI 0 "register_operand"         "=$l, r,$  l, *r")
        (zero_extend:SI
	  (vec_select:HI
	    (match_operand:V2HI 1 "nonimmediate_operand" "  l, r, U33,  m")
	    (parallel [(const_int 0)]))))]
  "NDS32_EXT_DSP_P () && !TARGET_BIG_ENDIAN"
{
  switch (which_alternative)
    {
    case 0:
      return "zeh33\t%0, %1";
    case 1:
      return "zeh\t%0, %1";
    case 2:
      return nds32_output_16bit_load (operands, 2);
    case 3:
      return nds32_output_32bit_load (operands, 2);

    default:
      gcc_unreachable ();
    }
}
  [(set_attr "type"   "alu,alu,load,load")
   (set_attr "length" "  2,  4,   2,   4")])

(define_insn "vec_extractv2hi0_se"
  [(set (match_operand:SI 0 "register_operand"         "=$l, r, r")
        (sign_extend:SI
	  (vec_select:HI
	    (match_operand:V2HI 1 "nonimmediate_operand" "  l,r,m")
	    (parallel [(const_int 0)]))))]
  "NDS32_EXT_DSP_P () && !TARGET_BIG_ENDIAN"
{
  switch (which_alternative)
    {
    case 0:
      return "seh33\t%0, %1";
    case 1:
      return "seh\t%0, %1";
    case 2:
      return nds32_output_32bit_load_s (operands, 2);

    default:
      gcc_unreachable ();
    }
}
  [(set_attr "type"   "alu,alu,load")
   (set_attr "length" "  2,  4,   4")])

(define_insn "vec_extractv2hi0_be"
  [(set (match_operand:HI 0 "register_operand"     "=$d,r")
	(vec_select:HI
	  (match_operand:V2HI 1 "register_operand" "  0,r")
	  (parallel [(const_int 0)])))]
  "NDS32_EXT_DSP_P () && TARGET_BIG_ENDIAN"
  "@
   srai45\t%0, 16
   srai\t%0, %1, 16"
  [(set_attr "type"    "alu,alu")
   (set_attr "length"  "  2,  4")])

(define_insn "vec_extractv2hi1"
  [(set (match_operand:HI 0 "register_operand"     "=$d,r")
	(vec_select:HI
	  (match_operand:V2HI 1 "register_operand" "  0,r")
	  (parallel [(const_int 1)])))]
  "NDS32_EXT_DSP_P () && !TARGET_BIG_ENDIAN"
  "@
   srai45\t%0, 16
   srai\t%0, %1, 16"
  [(set_attr "type"    "alu,alu")
   (set_attr "length"  "  2,  4")])

(define_insn "vec_extractv2hi1_se"
  [(set (match_operand:SI 0 "register_operand"     "=$d,r")
	(sign_extend:SI
	  (vec_select:HI
	    (match_operand:V2HI 1 "register_operand" "  0,r")
	    (parallel [(const_int 1)]))))]
  "NDS32_EXT_DSP_P () && !TARGET_BIG_ENDIAN"
  "@
   srai45\t%0, 16
   srai\t%0, %1, 16"
  [(set_attr "type"    "alu,alu")
   (set_attr "length"  "  2,  4")])

(define_insn "vec_extractv2hi1_ze"
  [(set (match_operand:SI 0 "register_operand"     "=$d,r")
	(zero_extend:SI
	  (vec_select:HI
	    (match_operand:V2HI 1 "register_operand" "  0,r")
	    (parallel [(const_int 1)]))))]
  "NDS32_EXT_DSP_P () && !TARGET_BIG_ENDIAN"
  "@
   srli45\t%0, 16
   srli\t%0, %1, 16"
  [(set_attr "type"    "alu,alu")
   (set_attr "length"  "  2,  4")])

(define_insn "vec_extractv2hi1_be"
  [(set (match_operand:HI 0 "register_operand"         "=$l,r,r")
	(vec_select:HI
	  (match_operand:V2HI 1 "nonimmediate_operand" "  l,r,m")
	  (parallel [(const_int 1)])))]
  "NDS32_EXT_DSP_P () && TARGET_BIG_ENDIAN"
{
  switch (which_alternative)
    {
    case 0:
      return "seh33\t%0, %1";
    case 1:
      return "seh\t%0, %1";
    case 2:
      return nds32_output_32bit_load_s (operands, 2);

    default:
      gcc_unreachable ();
    }
}
  [(set_attr "type"    "alu,alu,load")
   (set_attr "length"  "  2,  4,   4")])

(define_insn "<su>mul16"
  [(set (match_operand:V2SI 0 "register_operand"                         "=r")
	(mult:V2SI (extend:V2SI (match_operand:V2HI 1 "register_operand" "%r"))
		   (extend:V2SI (match_operand:V2HI 2 "register_operand" " r"))))]
  "NDS32_EXT_DSP_P ()"
  "<su>mul16\t%0, %1, %2"
  [(set_attr "type"   "dmul")
   (set_attr "length"   "4")])

(define_insn "<su>mulx16"
  [(set (match_operand:V2SI 0 "register_operand"         "=r")
	(vec_merge:V2SI
	  (vec_duplicate:V2SI
	    (mult:SI
	      (extend:SI
		(vec_select:HI
		  (match_operand:V2HI 1 "register_operand" " r")
		  (parallel [(const_int 0)])))
	      (extend:SI
		(vec_select:HI
		  (match_operand:V2HI 2 "register_operand" " r")
		  (parallel [(const_int 1)])))))
	  (vec_duplicate:V2SI
	    (mult:SI
	      (extend:SI
		(vec_select:HI
		  (match_dup 1)
		  (parallel [(const_int 1)])))
	      (extend:SI
		(vec_select:HI
		  (match_dup 2)
		  (parallel [(const_int 0)])))))
	  (const_int 1)))]
  "NDS32_EXT_DSP_P ()"
  "<su>mulx16\t%0, %1, %2"
  [(set_attr "type"    "dmul")
   (set_attr "length"   "4")])

(define_insn "rotrv2hi_1"
  [(set (match_operand:V2HI 0 "register_operand"    "=r")
	(vec_select:V2HI
	   (match_operand:V2HI 1 "register_operand" " r")
	   (parallel [(const_int 1) (const_int 0)])))]
  "NDS32_EXT_DSP_P () && !TARGET_BIG_ENDIAN"
  "rotri\t%0, %1, 16"
  [(set_attr "type"   "alu")
   (set_attr "length"  "4")])

(define_insn "rotrv2hi_1_be"
  [(set (match_operand:V2HI 0 "register_operand"    "=r")
	(vec_select:V2HI
	   (match_operand:V2HI 1 "register_operand" " r")
	   (parallel [(const_int 0) (const_int 1)])))]
  "NDS32_EXT_DSP_P () && TARGET_BIG_ENDIAN"
  "rotri\t%0, %1, 16"
  [(set_attr "type"   "alu")
   (set_attr "length"  "4")])

(define_insn "rotrv4qi_1"
  [(set (match_operand:V4QI 0 "register_operand"    "=r")
	(vec_select:V4QI
	   (match_operand:V4QI 1 "register_operand" " r")
	   (parallel [(const_int 1) (const_int 2) (const_int 3) (const_int 0)])))]
  "NDS32_EXT_DSP_P () && !TARGET_BIG_ENDIAN"
  "rotri\t%0, %1, 8"
  [(set_attr "type"   "alu")
   (set_attr "length"  "4")])

(define_insn "rotrv4qi_1_be"
  [(set (match_operand:V4QI 0 "register_operand"    "=r")
	(vec_select:V4QI
	   (match_operand:V4QI 1 "register_operand" " r")
	   (parallel [(const_int 2) (const_int 1) (const_int 0) (const_int 3)])))]
  "NDS32_EXT_DSP_P () && TARGET_BIG_ENDIAN"
  "rotri\t%0, %1, 8"
  [(set_attr "type"   "alu")
   (set_attr "length"  "4")])

(define_insn "rotrv4qi_2"
  [(set (match_operand:V4QI 0 "register_operand"    "=r")
	(vec_select:V4QI
	   (match_operand:V4QI 1 "register_operand" " r")
	   (parallel [(const_int 2) (const_int 3) (const_int 0) (const_int 1)])))]
  "NDS32_EXT_DSP_P () && !TARGET_BIG_ENDIAN"
  "rotri\t%0, %1, 16"
  [(set_attr "type"   "alu")
   (set_attr "length"  "4")])

(define_insn "rotrv4qi_2_be"
  [(set (match_operand:V4QI 0 "register_operand"    "=r")
	(vec_select:V4QI
	   (match_operand:V4QI 1 "register_operand" " r")
	   (parallel [(const_int 1) (const_int 0) (const_int 3) (const_int 2)])))]
  "NDS32_EXT_DSP_P () && TARGET_BIG_ENDIAN"
  "rotri\t%0, %1, 16"
  [(set_attr "type"   "alu")
   (set_attr "length"  "4")])

(define_insn "rotrv4qi_3"
  [(set (match_operand:V4QI 0 "register_operand"    "=r")
	(vec_select:V4QI
	   (match_operand:V4QI 1 "register_operand" " r")
	   (parallel [(const_int 3) (const_int 0) (const_int 1) (const_int 2)])))]
  "NDS32_EXT_DSP_P () && !TARGET_BIG_ENDIAN"
  "rotri\t%0, %1, 24"
  [(set_attr "type"   "alu")
   (set_attr "length"  "4")])

(define_insn "rotrv4qi_3_be"
  [(set (match_operand:V4QI 0 "register_operand"    "=r")
	(vec_select:V4QI
	   (match_operand:V4QI 1 "register_operand" " r")
	   (parallel [(const_int 0) (const_int 3) (const_int 2) (const_int 1)])))]
  "NDS32_EXT_DSP_P () && TARGET_BIG_ENDIAN"
  "rotri\t%0, %1, 24"
  [(set_attr "type"   "alu")
   (set_attr "length"  "4")])

(define_insn "v4qi_dup_10"
  [(set (match_operand:V4QI 0 "register_operand"    "=r")
	(vec_select:V4QI
	   (match_operand:V4QI 1 "register_operand" " r")
	   (parallel [(const_int 0) (const_int 1) (const_int 0) (const_int 1)])))]
  "NDS32_EXT_DSP_P () && !TARGET_BIG_ENDIAN"
  "pkbb\t%0, %1, %1"
  [(set_attr "type"    "dpack")
   (set_attr "length"  "4")])

(define_insn "v4qi_dup_32"
  [(set (match_operand:V4QI 0 "register_operand"    "=r")
	(vec_select:V4QI
	   (match_operand:V4QI 1 "register_operand" " r")
	   (parallel [(const_int 2) (const_int 3) (const_int 2) (const_int 3)])))]
  "NDS32_EXT_DSP_P () && !TARGET_BIG_ENDIAN"
  "pktt\t%0, %1, %1"
  [(set_attr "type"    "dpack")
   (set_attr "length"  "4")])

(define_expand "vec_unpacks_lo_v4qi"
  [(match_operand:V2HI 0 "register_operand" "=r")
   (match_operand:V4QI 1 "register_operand" " r")]
  "NDS32_EXT_DSP_P () && !TARGET_BIG_ENDIAN"
{
  emit_insn (gen_sunpkd810 (operands[0], operands[1]));
  DONE;
})

(define_expand "sunpkd810"
  [(match_operand:V2HI 0 "register_operand")
   (match_operand:V4QI 1 "register_operand")]
  "NDS32_EXT_DSP_P ()"
{
  if (TARGET_BIG_ENDIAN)
    emit_insn (gen_sunpkd810_imp_be (operands[0], operands[1]));
  else
    emit_insn (gen_sunpkd810_imp (operands[0], operands[1]));
  DONE;
})

(define_insn "<zs>unpkd810_imp"
  [(set (match_operand:V2HI 0 "register_operand"                     "=r")
	(vec_merge:V2HI
	  (vec_duplicate:V2HI
	    (extend:HI
	      (vec_select:QI
		(match_operand:V4QI 1 "register_operand"             " r")
		(parallel [(const_int 1)]))))
	  (vec_duplicate:V2HI
	    (extend:HI
	      (vec_select:QI
		(match_dup 1)
		(parallel [(const_int 0)]))))
	  (const_int 2)))]
  "NDS32_EXT_DSP_P () && !TARGET_BIG_ENDIAN"
  "<zs>unpkd810\t%0, %1"
  [(set_attr "type"    "dpack")
   (set_attr "length"  "4")])

(define_insn "<zs>unpkd810_imp_inv"
  [(set (match_operand:V2HI 0 "register_operand"                     "=r")
	(vec_merge:V2HI
	  (vec_duplicate:V2HI
	    (extend:HI
	      (vec_select:QI
		(match_operand:V4QI 1 "register_operand"             " r")
		(parallel [(const_int 0)]))))
	  (vec_duplicate:V2HI
	    (extend:HI
	      (vec_select:QI
		(match_dup 1)
		(parallel [(const_int 1)]))))
	  (const_int 1)))]
  "NDS32_EXT_DSP_P () && !TARGET_BIG_ENDIAN"
  "<zs>unpkd810\t%0, %1"
  [(set_attr "type"    "dpack")
   (set_attr "length"  "4")])

(define_insn "<zs>unpkd810_imp_be"
  [(set (match_operand:V2HI 0 "register_operand"                     "=r")
	(vec_merge:V2HI
	  (vec_duplicate:V2HI
	    (extend:HI
	      (vec_select:QI
		(match_operand:V4QI 1 "register_operand"             " r")
		(parallel [(const_int 2)]))))
	  (vec_duplicate:V2HI
	    (extend:HI
	      (vec_select:QI
		(match_dup 1)
		(parallel [(const_int 3)]))))
	  (const_int 1)))]
  "NDS32_EXT_DSP_P () && TARGET_BIG_ENDIAN"
  "<zs>unpkd810\t%0, %1"
  [(set_attr "type"    "dpack")
   (set_attr "length"  "4")])

(define_insn "<zs>unpkd810_imp_inv_be"
  [(set (match_operand:V2HI 0 "register_operand"                     "=r")
	(vec_merge:V2HI
	  (vec_duplicate:V2HI
	    (extend:HI
	      (vec_select:QI
		(match_operand:V4QI 1 "register_operand"             " r")
		(parallel [(const_int 3)]))))
	  (vec_duplicate:V2HI
	    (extend:HI
	      (vec_select:QI
		(match_dup 1)
		(parallel [(const_int 2)]))))
	  (const_int 2)))]
  "NDS32_EXT_DSP_P () && TARGET_BIG_ENDIAN"
  "<zs>unpkd810\t%0, %1"
  [(set_attr "type"    "dpack")
   (set_attr "length"  "4")])

(define_expand "sunpkd820"
  [(match_operand:V2HI 0 "register_operand")
   (match_operand:V4QI 1 "register_operand")]
  "NDS32_EXT_DSP_P ()"
{
  if (TARGET_BIG_ENDIAN)
    emit_insn (gen_sunpkd820_imp_be (operands[0], operands[1]));
  else
    emit_insn (gen_sunpkd820_imp (operands[0], operands[1]));
  DONE;
})

(define_insn "<zs>unpkd820_imp"
  [(set (match_operand:V2HI 0 "register_operand"                     "=r")
	(vec_merge:V2HI
	  (vec_duplicate:V2HI
	    (extend:HI
	      (vec_select:QI
		(match_operand:V4QI 1 "register_operand"             " r")
		(parallel [(const_int 2)]))))
	  (vec_duplicate:V2HI
	    (extend:HI
	      (vec_select:QI
		(match_dup 1)
		(parallel [(const_int 0)]))))
	  (const_int 2)))]
  "NDS32_EXT_DSP_P () && !TARGET_BIG_ENDIAN"
  "<zs>unpkd820\t%0, %1"
  [(set_attr "type"    "dpack")
   (set_attr "length"  "4")])

(define_insn "<zs>unpkd820_imp_inv"
  [(set (match_operand:V2HI 0 "register_operand"                     "=r")
	(vec_merge:V2HI
	  (vec_duplicate:V2HI
	    (extend:HI
	      (vec_select:QI
		(match_operand:V4QI 1 "register_operand"             " r")
		(parallel [(const_int 0)]))))
	  (vec_duplicate:V2HI
	    (extend:HI
	      (vec_select:QI
		(match_dup 1)
		(parallel [(const_int 2)]))))
	  (const_int 1)))]
  "NDS32_EXT_DSP_P () && !TARGET_BIG_ENDIAN"
  "<zs>unpkd820\t%0, %1"
  [(set_attr "type"    "dpack")
   (set_attr "length"  "4")])

(define_insn "<zs>unpkd820_imp_be"
  [(set (match_operand:V2HI 0 "register_operand"                     "=r")
	(vec_merge:V2HI
	  (vec_duplicate:V2HI
	    (extend:HI
	      (vec_select:QI
		(match_operand:V4QI 1 "register_operand"             " r")
		(parallel [(const_int 1)]))))
	  (vec_duplicate:V2HI
	    (extend:HI
	      (vec_select:QI
		(match_dup 1)
		(parallel [(const_int 3)]))))
	  (const_int 1)))]
  "NDS32_EXT_DSP_P () && TARGET_BIG_ENDIAN"
  "<zs>unpkd820\t%0, %1"
  [(set_attr "type"    "dpack")
   (set_attr "length"  "4")])

(define_insn "<zs>unpkd820_imp_inv_be"
  [(set (match_operand:V2HI 0 "register_operand"                     "=r")
	(vec_merge:V2HI
	  (vec_duplicate:V2HI
	    (extend:HI
	      (vec_select:QI
		(match_operand:V4QI 1 "register_operand"             " r")
		(parallel [(const_int 3)]))))
	  (vec_duplicate:V2HI
	    (extend:HI
	      (vec_select:QI
		(match_dup 1)
		(parallel [(const_int 1)]))))
	  (const_int 2)))]
  "NDS32_EXT_DSP_P () && TARGET_BIG_ENDIAN"
  "<zs>unpkd820\t%0, %1"
  [(set_attr "type"    "dpack")
   (set_attr "length"  "4")])

(define_expand "sunpkd830"
  [(match_operand:V2HI 0 "register_operand")
   (match_operand:V4QI 1 "register_operand")]
  "NDS32_EXT_DSP_P ()"
{
  if (TARGET_BIG_ENDIAN)
    emit_insn (gen_sunpkd830_imp_be (operands[0], operands[1]));
  else
    emit_insn (gen_sunpkd830_imp (operands[0], operands[1]));
  DONE;
})

(define_insn "<zs>unpkd830_imp"
  [(set (match_operand:V2HI 0 "register_operand"                     "=r")
	(vec_merge:V2HI
	  (vec_duplicate:V2HI
	    (extend:HI
	      (vec_select:QI
		(match_operand:V4QI 1 "register_operand"             " r")
		(parallel [(const_int 3)]))))
	  (vec_duplicate:V2HI
	    (extend:HI
	      (vec_select:QI
		(match_dup 1)
		(parallel [(const_int 0)]))))
	  (const_int 2)))]
  "NDS32_EXT_DSP_P () && !TARGET_BIG_ENDIAN"
  "<zs>unpkd830\t%0, %1"
  [(set_attr "type"    "dpack")
   (set_attr "length"  "4")])

(define_insn "<zs>unpkd830_imp_inv"
  [(set (match_operand:V2HI 0 "register_operand"                     "=r")
	(vec_merge:V2HI
	  (vec_duplicate:V2HI
	    (extend:HI
	      (vec_select:QI
		(match_operand:V4QI 1 "register_operand"             " r")
		(parallel [(const_int 0)]))))
	  (vec_duplicate:V2HI
	    (extend:HI
	      (vec_select:QI
		(match_dup 1)
		(parallel [(const_int 3)]))))
	  (const_int 1)))]
  "NDS32_EXT_DSP_P () && !TARGET_BIG_ENDIAN"
  "<zs>unpkd830\t%0, %1"
  [(set_attr "type"    "dpack")
   (set_attr "length"  "4")])

(define_insn "<zs>unpkd830_imp_be"
  [(set (match_operand:V2HI 0 "register_operand"                     "=r")
	(vec_merge:V2HI
	  (vec_duplicate:V2HI
	    (extend:HI
	      (vec_select:QI
		(match_operand:V4QI 1 "register_operand"             " r")
		(parallel [(const_int 0)]))))
	  (vec_duplicate:V2HI
	    (extend:HI
	      (vec_select:QI
		(match_dup 1)
		(parallel [(const_int 3)]))))
	  (const_int 1)))]
  "NDS32_EXT_DSP_P () && TARGET_BIG_ENDIAN"
  "<zs>unpkd830\t%0, %1"
  [(set_attr "type"    "dpack")
   (set_attr "length"  "4")])

(define_insn "<zs>unpkd830_imp_inv_be"
  [(set (match_operand:V2HI 0 "register_operand"                     "=r")
	(vec_merge:V2HI
	  (vec_duplicate:V2HI
	    (extend:HI
	      (vec_select:QI
		(match_operand:V4QI 1 "register_operand"             " r")
		(parallel [(const_int 3)]))))
	  (vec_duplicate:V2HI
	    (extend:HI
	      (vec_select:QI
		(match_dup 1)
		(parallel [(const_int 0)]))))
	  (const_int 2)))]
  "NDS32_EXT_DSP_P () && TARGET_BIG_ENDIAN"
  "<zs>unpkd830\t%0, %1"
  [(set_attr "type"    "dpack")
   (set_attr "length"  "4")])

(define_expand "sunpkd831"
  [(match_operand:V2HI 0 "register_operand")
   (match_operand:V4QI 1 "register_operand")]
  "NDS32_EXT_DSP_P ()"
{
  if (TARGET_BIG_ENDIAN)
    emit_insn (gen_sunpkd831_imp_be (operands[0], operands[1]));
  else
    emit_insn (gen_sunpkd831_imp (operands[0], operands[1]));
  DONE;
})

(define_insn "<zs>unpkd831_imp"
  [(set (match_operand:V2HI 0 "register_operand"                     "=r")
	(vec_merge:V2HI
	  (vec_duplicate:V2HI
	    (extend:HI
	      (vec_select:QI
		(match_operand:V4QI 1 "register_operand"             " r")
		(parallel [(const_int 3)]))))
	  (vec_duplicate:V2HI
	    (extend:HI
	      (vec_select:QI
		(match_dup 1)
		(parallel [(const_int 1)]))))
	  (const_int 2)))]
  "NDS32_EXT_DSP_P () && !TARGET_BIG_ENDIAN"
  "<zs>unpkd831\t%0, %1"
  [(set_attr "type"    "dpack")
   (set_attr "length"  "4")])

(define_insn "<zs>unpkd831_imp_inv"
  [(set (match_operand:V2HI 0 "register_operand"                     "=r")
	(vec_merge:V2HI
	  (vec_duplicate:V2HI
	    (extend:HI
	      (vec_select:QI
		(match_operand:V4QI 1 "register_operand"             " r")
		(parallel [(const_int 1)]))))
	  (vec_duplicate:V2HI
	    (extend:HI
	      (vec_select:QI
		(match_dup 1)
		(parallel [(const_int 3)]))))
	  (const_int 1)))]
  "NDS32_EXT_DSP_P () && !TARGET_BIG_ENDIAN"
  "<zs>unpkd831\t%0, %1"
  [(set_attr "type"    "dpack")
   (set_attr "length"  "4")])

(define_insn "<zs>unpkd831_imp_be"
  [(set (match_operand:V2HI 0 "register_operand"                     "=r")
	(vec_merge:V2HI
	  (vec_duplicate:V2HI
	    (extend:HI
	      (vec_select:QI
		(match_operand:V4QI 1 "register_operand"             " r")
		(parallel [(const_int 0)]))))
	  (vec_duplicate:V2HI
	    (extend:HI
	      (vec_select:QI
		(match_dup 1)
		(parallel [(const_int 2)]))))
	  (const_int 1)))]
  "NDS32_EXT_DSP_P () && TARGET_BIG_ENDIAN"
  "<zs>unpkd831\t%0, %1"
  [(set_attr "type"    "dpack")
   (set_attr "length"  "4")])

(define_insn "<zs>unpkd831_imp_inv_be"
  [(set (match_operand:V2HI 0 "register_operand"                     "=r")
	(vec_merge:V2HI
	  (vec_duplicate:V2HI
	    (extend:HI
	      (vec_select:QI
		(match_operand:V4QI 1 "register_operand"             " r")
		(parallel [(const_int 2)]))))
	  (vec_duplicate:V2HI
	    (extend:HI
	      (vec_select:QI
		(match_dup 1)
		(parallel [(const_int 0)]))))
	  (const_int 2)))]
  "NDS32_EXT_DSP_P () && TARGET_BIG_ENDIAN"
  "<zs>unpkd831\t%0, %1"
  [(set_attr "type"    "dpack")
   (set_attr "length"  "4")])

(define_expand "zunpkd810"
  [(match_operand:V2HI 0 "register_operand")
   (match_operand:V4QI 1 "register_operand")]
  "NDS32_EXT_DSP_P ()"
{
  if (TARGET_BIG_ENDIAN)
    emit_insn (gen_zunpkd810_imp_be (operands[0], operands[1]));
  else
    emit_insn (gen_zunpkd810_imp (operands[0], operands[1]));
  DONE;
})

(define_expand "zunpkd820"
  [(match_operand:V2HI 0 "register_operand")
   (match_operand:V4QI 1 "register_operand")]
  "NDS32_EXT_DSP_P ()"
{
  if (TARGET_BIG_ENDIAN)
    emit_insn (gen_zunpkd820_imp_be (operands[0], operands[1]));
  else
    emit_insn (gen_zunpkd820_imp (operands[0], operands[1]));
  DONE;
})

(define_expand "zunpkd830"
  [(match_operand:V2HI 0 "register_operand")
   (match_operand:V4QI 1 "register_operand")]
  "NDS32_EXT_DSP_P ()"
{
  if (TARGET_BIG_ENDIAN)
    emit_insn (gen_zunpkd830_imp_be (operands[0], operands[1]));
  else
    emit_insn (gen_zunpkd830_imp (operands[0], operands[1]));
  DONE;
})

(define_expand "zunpkd831"
  [(match_operand:V2HI 0 "register_operand")
   (match_operand:V4QI 1 "register_operand")]
  "NDS32_EXT_DSP_P ()"
{
  if (TARGET_BIG_ENDIAN)
    emit_insn (gen_zunpkd831_imp_be (operands[0], operands[1]));
  else
    emit_insn (gen_zunpkd831_imp (operands[0], operands[1]));
  DONE;
})

(define_expand "smbb"
  [(match_operand:SI 0 "register_operand" "")
   (match_operand:V2HI 1 "register_operand" "")
   (match_operand:V2HI 2 "register_operand" "")]
  "NDS32_EXT_DSP_P ()"
{
  if (TARGET_BIG_ENDIAN)
    emit_insn (gen_mulhisi3v (operands[0], operands[1], operands[2],
			      GEN_INT (1), GEN_INT (1)));
  else
    emit_insn (gen_mulhisi3v (operands[0], operands[1], operands[2],
			      GEN_INT (0), GEN_INT (0)));
  DONE;
})

(define_expand "smbt"
  [(match_operand:SI 0 "register_operand" "")
   (match_operand:V2HI 1 "register_operand" "")
   (match_operand:V2HI 2 "register_operand" "")]
  "NDS32_EXT_DSP_P ()"
{
  if (TARGET_BIG_ENDIAN)
    emit_insn (gen_mulhisi3v (operands[0], operands[1], operands[2],
			      GEN_INT (1), GEN_INT (0)));
  else
    emit_insn (gen_mulhisi3v (operands[0], operands[1], operands[2],
			      GEN_INT (0), GEN_INT (1)));
  DONE;
})

(define_expand "smtt"
  [(match_operand:SI 0 "register_operand" "")
   (match_operand:V2HI 1 "register_operand" "")
   (match_operand:V2HI 2 "register_operand" "")]
  "NDS32_EXT_DSP_P ()"
{
  if (TARGET_BIG_ENDIAN)
    emit_insn (gen_mulhisi3v (operands[0], operands[1], operands[2],
			      GEN_INT (0), GEN_INT (0)));
  else
    emit_insn (gen_mulhisi3v (operands[0], operands[1], operands[2],
			      GEN_INT (1), GEN_INT (1)));
  DONE;
})

(define_insn "mulhisi3v"
  [(set (match_operand:SI 0 "register_operand"                         "=   r,    r,    r,    r")
	(mult:SI
	  (sign_extend:SI
	     (vec_select:HI
	       (match_operand:V2HI 1 "register_operand"                "    r,    r,    r,    r")
	       (parallel [(match_operand:SI 3 "nds32_imm_0_1_operand"  " Iv00, Iv00, Iv01, Iv01")])))
	  (sign_extend:SI (vec_select:HI
	       (match_operand:V2HI 2 "register_operand"                "    r,    r,    r,    r")
	       (parallel [(match_operand:SI 4 "nds32_imm_0_1_operand"  " Iv00, Iv01, Iv01, Iv00")])))))]
  "NDS32_EXT_DSP_P ()"
{
  if (TARGET_BIG_ENDIAN)
    {
      const char *pats[] = { "smtt\t%0, %1, %2",
			     "smbt\t%0, %2, %1",
			     "smbb\t%0, %1, %2",
			     "smbt\t%0, %1, %2" };
      return pats[which_alternative];
    }
  else
    {
      const char *pats[] = { "smbb\t%0, %1, %2",
			     "smbt\t%0, %1, %2",
			     "smtt\t%0, %1, %2",
			     "smbt\t%0, %2, %1" };
      return pats[which_alternative];
    }
}
  [(set_attr "type"     "dmul")
   (set_attr "length"   "4")])

(define_expand "kmabb"
  [(match_operand:SI 0 "register_operand" "")
   (match_operand:SI 1 "register_operand" "")
   (match_operand:V2HI 2 "register_operand" "")
   (match_operand:V2HI 3 "register_operand" "")]
  "NDS32_EXT_DSP_P ()"
{
  if (TARGET_BIG_ENDIAN)
    emit_insn (gen_kma_internal (operands[0], operands[2], operands[3],
				 GEN_INT (1), GEN_INT (1),
				 operands[1]));
  else
    emit_insn (gen_kma_internal (operands[0], operands[2], operands[3],
				 GEN_INT (0), GEN_INT (0),
				 operands[1]));
  DONE;
})

(define_expand "kmabt"
  [(match_operand:SI 0 "register_operand" "")
   (match_operand:SI 1 "register_operand" "")
   (match_operand:V2HI 2 "register_operand" "")
   (match_operand:V2HI 3 "register_operand" "")]
  "NDS32_EXT_DSP_P ()"
{
  if (TARGET_BIG_ENDIAN)
    emit_insn (gen_kma_internal (operands[0], operands[2], operands[3],
				 GEN_INT (1), GEN_INT (0),
				 operands[1]));
  else
    emit_insn (gen_kma_internal (operands[0], operands[2], operands[3],
				 GEN_INT (0), GEN_INT (1),
				 operands[1]));
  DONE;
})

(define_expand "kmatt"
  [(match_operand:SI 0 "register_operand" "")
   (match_operand:SI 1 "register_operand" "")
   (match_operand:V2HI 2 "register_operand" "")
   (match_operand:V2HI 3 "register_operand" "")]
  "NDS32_EXT_DSP_P ()"
{
  if (TARGET_BIG_ENDIAN)
    emit_insn (gen_kma_internal (operands[0], operands[2], operands[3],
				 GEN_INT (0), GEN_INT (0),
				 operands[1]));
  else
    emit_insn (gen_kma_internal (operands[0], operands[2], operands[3],
				 GEN_INT (1), GEN_INT (1),
				 operands[1]));
  DONE;
})

(define_insn "kma_internal"
  [(set (match_operand:SI 0 "register_operand"                          "=    r,    r,    r,    r")
	(ss_plus:SI
	  (mult:SI
	    (sign_extend:SI
	      (vec_select:HI
		(match_operand:V2HI 1 "register_operand"                "    r,    r,    r,    r")
	        (parallel [(match_operand:SI 3 "nds32_imm_0_1_operand"  " Iv00, Iv00, Iv01, Iv01")])))
	    (sign_extend:SI
	      (vec_select:HI
	        (match_operand:V2HI 2 "register_operand"                "    r,    r,    r,    r")
	        (parallel [(match_operand:SI 4 "nds32_imm_0_1_operand"  " Iv00, Iv01, Iv01, Iv00")]))))
	  (match_operand:SI 5 "register_operand"                        "     0,    0,    0,    0")))]
  "NDS32_EXT_DSP_P ()"
{
  if (TARGET_BIG_ENDIAN)
    {
      const char *pats[] = { "kmatt\t%0, %1, %2",
			     "kmabt\t%0, %2, %1",
			     "kmabb\t%0, %1, %2",
			     "kmabt\t%0, %1, %2" };
      return pats[which_alternative];
    }
  else
    {
      const char *pats[] = { "kmabb\t%0, %1, %2",
			     "kmabt\t%0, %1, %2",
			     "kmatt\t%0, %1, %2",
			     "kmabt\t%0, %2, %1" };
      return pats[which_alternative];
    }
}
  [(set_attr "type"    "dmac")
   (set_attr "length"   "4")])

(define_expand "smds"
  [(match_operand:SI 0 "register_operand" "")
   (match_operand:V2HI 1 "register_operand" "")
   (match_operand:V2HI 2 "register_operand" "")]
  "NDS32_EXT_DSP_P ()"
{
  if (TARGET_BIG_ENDIAN)
    emit_insn (gen_smds_be (operands[0], operands[1], operands[2]));
  else
    emit_insn (gen_smds_le (operands[0], operands[1], operands[2]));
  DONE;
})

(define_expand "smds_le"
  [(set (match_operand:SI 0 "register_operand"                         "=r")
	(minus:SI
	  (mult:SI
	    (sign_extend:SI (vec_select:HI
			      (match_operand:V2HI 1 "register_operand" " r")
			      (parallel [(const_int 1)])))
	    (sign_extend:SI (vec_select:HI
			      (match_operand:V2HI 2 "register_operand" " r")
			      (parallel [(const_int 1)]))))
	  (mult:SI
	    (sign_extend:SI (vec_select:HI
			      (match_dup 1)
			      (parallel [(const_int 0)])))
	    (sign_extend:SI (vec_select:HI
			      (match_dup 2)
			      (parallel [(const_int 0)]))))))]
  "NDS32_EXT_DSP_P ()"
{
})

(define_expand "smds_be"
  [(set (match_operand:SI 0 "register_operand"                         "=r")
	(minus:SI
	  (mult:SI
	    (sign_extend:SI (vec_select:HI
			      (match_operand:V2HI 1 "register_operand" " r")
			      (parallel [(const_int 0)])))
	    (sign_extend:SI (vec_select:HI
			      (match_operand:V2HI 2 "register_operand" " r")
			      (parallel [(const_int 0)]))))
	  (mult:SI
	    (sign_extend:SI (vec_select:HI
			      (match_dup 1)
			      (parallel [(const_int 1)])))
	    (sign_extend:SI (vec_select:HI
			      (match_dup 2)
			      (parallel [(const_int 1)]))))))]
  "NDS32_EXT_DSP_P ()"
{
})

(define_expand "smdrs"
  [(match_operand:SI 0 "register_operand" "")
   (match_operand:V2HI 1 "register_operand" "")
   (match_operand:V2HI 2 "register_operand" "")]
  "NDS32_EXT_DSP_P ()"
{
  if (TARGET_BIG_ENDIAN)
    emit_insn (gen_smdrs_be (operands[0], operands[1], operands[2]));
  else
    emit_insn (gen_smdrs_le (operands[0], operands[1], operands[2]));
  DONE;
})

(define_expand "smdrs_le"
  [(set (match_operand:SI 0 "register_operand"                         "=r")
	(minus:SI
	  (mult:SI
	    (sign_extend:SI (vec_select:HI
			      (match_operand:V2HI 1 "register_operand" " r")
			      (parallel [(const_int 0)])))
	    (sign_extend:SI (vec_select:HI
			      (match_operand:V2HI 2 "register_operand" " r")
			      (parallel [(const_int 0)]))))
	  (mult:SI
	    (sign_extend:SI (vec_select:HI
			      (match_dup 1)
			      (parallel [(const_int 1)])))
	    (sign_extend:SI (vec_select:HI
			      (match_dup 2)
			      (parallel [(const_int 1)]))))))]
  "NDS32_EXT_DSP_P ()"
{
})

(define_expand "smdrs_be"
  [(set (match_operand:SI 0 "register_operand"                         "=r")
	(minus:SI
	  (mult:SI
	    (sign_extend:SI (vec_select:HI
			      (match_operand:V2HI 1 "register_operand" " r")
			      (parallel [(const_int 1)])))
	    (sign_extend:SI (vec_select:HI
			      (match_operand:V2HI 2 "register_operand" " r")
			      (parallel [(const_int 1)]))))
	  (mult:SI
	    (sign_extend:SI (vec_select:HI
			      (match_dup 1)
			      (parallel [(const_int 0)])))
	    (sign_extend:SI (vec_select:HI
			      (match_dup 2)
			      (parallel [(const_int 0)]))))))]
  "NDS32_EXT_DSP_P ()"
{
})

(define_expand "smxdsv"
  [(match_operand:SI 0 "register_operand" "")
   (match_operand:V2HI 1 "register_operand" "")
   (match_operand:V2HI 2 "register_operand" "")]
  "NDS32_EXT_DSP_P ()"
{
  if (TARGET_BIG_ENDIAN)
    emit_insn (gen_smxdsv_be (operands[0], operands[1], operands[2]));
  else
    emit_insn (gen_smxdsv_le (operands[0], operands[1], operands[2]));
  DONE;
})


(define_expand "smxdsv_le"
  [(set (match_operand:SI 0 "register_operand"                         "=r")
	(minus:SI
	  (mult:SI
	    (sign_extend:SI (vec_select:HI
			      (match_operand:V2HI 1 "register_operand" " r")
			      (parallel [(const_int 1)])))
	    (sign_extend:SI (vec_select:HI
			      (match_operand:V2HI 2 "register_operand" " r")
			      (parallel [(const_int 0)]))))
	  (mult:SI
	    (sign_extend:SI (vec_select:HI
			      (match_dup 1)
			      (parallel [(const_int 0)])))
	    (sign_extend:SI (vec_select:HI
			      (match_dup 2)
			      (parallel [(const_int 1)]))))))]
  "NDS32_EXT_DSP_P ()"
{
})

(define_expand "smxdsv_be"
  [(set (match_operand:SI 0 "register_operand"                         "=r")
	(minus:SI
	  (mult:SI
	    (sign_extend:SI (vec_select:HI
			      (match_operand:V2HI 1 "register_operand" " r")
			      (parallel [(const_int 0)])))
	    (sign_extend:SI (vec_select:HI
			      (match_operand:V2HI 2 "register_operand" " r")
			      (parallel [(const_int 1)]))))
	  (mult:SI
	    (sign_extend:SI (vec_select:HI
			      (match_dup 1)
			      (parallel [(const_int 1)])))
	    (sign_extend:SI (vec_select:HI
			      (match_dup 2)
			      (parallel [(const_int 0)]))))))]
  "NDS32_EXT_DSP_P ()"
{
})

(define_insn "smal1"
  [(set (match_operand:DI 0 "register_operand"             "=r")
	(plus:DI (match_operand:DI 1 "register_operand"    " r")
	  (sign_extend:DI
	    (mult:SI
	      (sign_extend:SI
		(vec_select:HI
		  (match_operand:V2HI 2 "register_operand" " r")
		  (parallel [(const_int 0)])))
	      (sign_extend:SI
		(vec_select:HI
		  (match_dup 2)
		  (parallel [(const_int 1)])))))))]
  "NDS32_EXT_DSP_P ()"
  "smal\t%0, %1, %2"
  [(set_attr "type"     "dmac")
   (set_attr "length"   "4")])

(define_insn "smal2"
  [(set (match_operand:DI 0 "register_operand"           "=r")
	(plus:DI (match_operand:DI 1 "register_operand"  " r")
	  (mult:DI
	    (sign_extend:DI
	      (vec_select:HI
		(match_operand:V2HI 2 "register_operand" " r")
		(parallel [(const_int 0)])))
	    (sign_extend:DI
	      (vec_select:HI
		(match_dup 2)
		(parallel [(const_int 1)]))))))]
  "NDS32_EXT_DSP_P ()"
  "smal\t%0, %1, %2"
  [(set_attr "type"     "dmac")
   (set_attr "length"   "4")])

(define_insn "smal3"
  [(set (match_operand:DI 0 "register_operand"             "=r")
	(plus:DI (match_operand:DI 1 "register_operand"    " r")
	  (sign_extend:DI
	    (mult:SI
	      (sign_extend:SI
		(vec_select:HI
		  (match_operand:V2HI 2 "register_operand" " r")
		  (parallel [(const_int 1)])))
	      (sign_extend:SI
		(vec_select:HI
		  (match_dup 2)
		  (parallel [(const_int 0)])))))))]
  "NDS32_EXT_DSP_P ()"
  "smal\t%0, %1, %2"
  [(set_attr "type"     "dmac")
   (set_attr "length"   "4")])

(define_insn "smal4"
  [(set (match_operand:DI 0 "register_operand"           "=r")
	(plus:DI (match_operand:DI 1 "register_operand"  " r")
	  (mult:DI
	    (sign_extend:DI
	      (vec_select:HI
		(match_operand:V2HI 2 "register_operand" " r")
		(parallel [(const_int 1)])))
	    (sign_extend:DI
	      (vec_select:HI
		(match_dup 2)
		(parallel [(const_int 0)]))))))]
  "NDS32_EXT_DSP_P ()"
  "smal\t%0, %1, %2"
  [(set_attr "type"     "dmac")
   (set_attr "length"   "4")])

(define_insn "smal5"
  [(set (match_operand:DI 0 "register_operand"             "=r")
	(plus:DI
	  (sign_extend:DI
	    (mult:SI
	      (sign_extend:SI
		(vec_select:HI
		  (match_operand:V2HI 2 "register_operand" " r")
		  (parallel [(const_int 0)])))
	      (sign_extend:SI
		(vec_select:HI
		  (match_dup 2)
		  (parallel [(const_int 1)])))))
	  (match_operand:DI 1 "register_operand"           " r")))]
  "NDS32_EXT_DSP_P ()"
  "smal\t%0, %1, %2"
  [(set_attr "type"     "dmac")
   (set_attr "length"   "4")])

(define_insn "smal6"
  [(set (match_operand:DI 0 "register_operand"           "=r")
	(plus:DI
	  (mult:DI
	    (sign_extend:DI
	      (vec_select:HI
		(match_operand:V2HI 2 "register_operand" " r")
		(parallel [(const_int 0)])))
	    (sign_extend:DI
	      (vec_select:HI
		(match_dup 2)
		(parallel [(const_int 1)]))))
	  (match_operand:DI 1 "register_operand"         " r")))]
  "NDS32_EXT_DSP_P ()"
  "smal\t%0, %1, %2"
  [(set_attr "type"     "dmac")
   (set_attr "length"   "4")])

(define_insn "smal7"
  [(set (match_operand:DI 0 "register_operand"             "=r")
	(plus:DI
	  (sign_extend:DI
	    (mult:SI
	      (sign_extend:SI
		(vec_select:HI
		  (match_operand:V2HI 2 "register_operand" " r")
		  (parallel [(const_int 1)])))
	      (sign_extend:SI
		(vec_select:HI
		  (match_dup 2)
		  (parallel [(const_int 0)])))))
	  (match_operand:DI 1 "register_operand"           " r")))]
  "NDS32_EXT_DSP_P ()"
  "smal\t%0, %1, %2"
  [(set_attr "type"    "dmac")
   (set_attr "length"   "4")])

(define_insn "smal8"
  [(set (match_operand:DI 0 "register_operand"           "=r")
	(plus:DI
	  (mult:DI
	    (sign_extend:DI
	      (vec_select:HI
		(match_operand:V2HI 2 "register_operand" " r")
		(parallel [(const_int 1)])))
	    (sign_extend:DI
	      (vec_select:HI
		(match_dup 2)
		(parallel [(const_int 0)]))))
	  (match_operand:DI 1 "register_operand"         " r")))]
  "NDS32_EXT_DSP_P ()"
  "smal\t%0, %1, %2"
  [(set_attr "type"     "dmac")
   (set_attr "length"   "4")])

;; We need this dummy pattern for smal
(define_insn_and_split "extendsidi2"
  [(set (match_operand:DI 0 "register_operand" "")
	(sign_extend:DI (match_operand:SI 1 "nds32_move_operand" "")))]
  "NDS32_EXT_DSP_P ()"
  "#"
  "NDS32_EXT_DSP_P ()"
  [(const_int 0)]
{
  rtx high_part_dst, low_part_dst;

  low_part_dst = nds32_di_low_part_subreg (operands[0]);
  high_part_dst = nds32_di_high_part_subreg (operands[0]);

  emit_move_insn (low_part_dst, operands[1]);
  emit_insn (gen_ashrsi3 (high_part_dst, low_part_dst, GEN_INT (31)));
  DONE;
}
  [(set_attr "type"   "alu")
   (set_attr "length"   "4")])

;; We need this dummy pattern for usmar64/usmsr64
(define_insn_and_split "zero_extendsidi2"
  [(set (match_operand:DI 0 "register_operand" "")
	(zero_extend:DI (match_operand:SI 1 "nds32_move_operand" "")))]
  "NDS32_EXT_DSP_P ()"
  "#"
  "NDS32_EXT_DSP_P ()"
  [(const_int 0)]
{
  rtx high_part_dst, low_part_dst;

  low_part_dst = nds32_di_low_part_subreg (operands[0]);
  high_part_dst = nds32_di_high_part_subreg (operands[0]);

  emit_move_insn (low_part_dst, operands[1]);
  emit_move_insn (high_part_dst, const0_rtx);
  DONE;
}
  [(set_attr "type"   "alu")
   (set_attr "length"   "4")])

(define_insn_and_split "extendhidi2"
  [(set (match_operand:DI 0 "register_operand" "")
	(sign_extend:DI (match_operand:HI 1 "nonimmediate_operand" "")))]
  "NDS32_EXT_DSP_P ()"
  "#"
  "NDS32_EXT_DSP_P ()"
  [(const_int 0)]
{
  rtx high_part_dst, low_part_dst;

  low_part_dst = nds32_di_low_part_subreg (operands[0]);
  high_part_dst = nds32_di_high_part_subreg (operands[0]);


  emit_insn (gen_extendhisi2 (low_part_dst, operands[1]));
  emit_insn (gen_ashrsi3 (high_part_dst, low_part_dst, GEN_INT (31)));
  DONE;
}
  [(set_attr "type"   "alu")
   (set_attr "length"   "4")])

(define_insn "extendqihi2"
  [(set (match_operand:HI 0 "register_operand"                 "=r")
	(sign_extend:HI (match_operand:QI 1 "register_operand" " r")))]
  "NDS32_EXT_DSP_P ()"
  "sunpkd820\t%0, %1"
  [(set_attr "type"       "dpack")
   (set_attr "length"     "4")])

(define_insn "smulsi3_highpart"
  [(set (match_operand:SI 0 "register_operand"                       "=r")
	(truncate:SI
	  (lshiftrt:DI
	    (mult:DI
	      (sign_extend:DI (match_operand:SI 1 "register_operand" " r"))
	      (sign_extend:DI (match_operand:SI 2 "register_operand" " r")))
	    (const_int 32))))]
  "NDS32_EXT_DSP_P ()"
  "smmul\t%0, %1, %2"
  [(set_attr "type"     "dmul")
   (set_attr "length"   "4")])

(define_insn "smmul_round"
  [(set (match_operand:SI 0 "register_operand"                       "=r")
	(truncate:SI
	  (lshiftrt:DI
	    (unspec:DI [(mult:DI
			  (sign_extend:DI (match_operand:SI 1 "register_operand" " r"))
			  (sign_extend:DI (match_operand:SI 2 "register_operand" " r")))]
		       UNSPEC_ROUND)
	    (const_int 32))))]
  "NDS32_EXT_DSP_P ()"
  "smmul.u\t%0, %1, %2"
  [(set_attr "type"     "dmul")
   (set_attr "length"   "4")])

(define_insn "kmmac"
  [(set (match_operand:SI 0 "register_operand"                         "=r")
	(ss_plus:SI (match_operand:SI 1 "register_operand"             " 0")
	  (truncate:SI
	    (lshiftrt:DI
	      (mult:DI
		(sign_extend:DI (match_operand:SI 2 "register_operand" " r"))
		(sign_extend:DI (match_operand:SI 3 "register_operand" " r")))
	      (const_int 32)))))]
  "NDS32_EXT_DSP_P ()"
  "kmmac\t%0, %2, %3"
  [(set_attr "type"     "dmac")
   (set_attr "length"   "4")])

(define_insn "kmmac_round"
  [(set (match_operand:SI 0 "register_operand"                                     "=r")
	(ss_plus:SI (match_operand:SI 1 "register_operand"                         " 0")
	  (truncate:SI
	    (lshiftrt:DI
	      (unspec:DI [(mult:DI
			    (sign_extend:DI (match_operand:SI 2 "register_operand" " r"))
			    (sign_extend:DI (match_operand:SI 3 "register_operand" " r")))]
			 UNSPEC_ROUND)
	      (const_int 32)))))]
  "NDS32_EXT_DSP_P ()"
  "kmmac.u\t%0, %2, %3"
  [(set_attr "type"     "dmac")
   (set_attr "length"   "4")])

(define_insn "kmmsb"
  [(set (match_operand:SI 0 "register_operand"                         "=r")
	(ss_minus:SI (match_operand:SI 1 "register_operand"            " 0")
	  (truncate:SI
	    (lshiftrt:DI
	      (mult:DI
		(sign_extend:DI (match_operand:SI 2 "register_operand" " r"))
		(sign_extend:DI (match_operand:SI 3 "register_operand" " r")))
	      (const_int 32)))))]
  "NDS32_EXT_DSP_P ()"
  "kmmsb\t%0, %2, %3"
  [(set_attr "type"     "dmac")
   (set_attr "length"   "4")])

(define_insn "kmmsb_round"
  [(set (match_operand:SI 0 "register_operand"                                     "=r")
	(ss_minus:SI (match_operand:SI 1 "register_operand"                        " 0")
	  (truncate:SI
	    (lshiftrt:DI
	      (unspec:DI [(mult:DI
			    (sign_extend:DI (match_operand:SI 2 "register_operand" " r"))
			    (sign_extend:DI (match_operand:SI 3 "register_operand" " r")))]
			 UNSPEC_ROUND)
	      (const_int 32)))))]
  "NDS32_EXT_DSP_P ()"
  "kmmsb.u\t%0, %2, %3"
  [(set_attr "type"     "dmac")
   (set_attr "length"   "4")])

(define_insn "kwmmul"
  [(set (match_operand:SI 0 "register_operand"                       "=r")
	(truncate:SI
	  (lshiftrt:DI
	    (ss_mult:DI
	      (mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand" " r")) (const_int 2))
	      (mult:DI (sign_extend:DI (match_operand:SI 2 "register_operand" " r")) (const_int 2)))
	    (const_int 32))))]
  "NDS32_EXT_DSP_P ()"
  "kwmmul\t%0, %1, %2"
  [(set_attr "type"     "dmul")
   (set_attr "length"   "4")])

(define_insn "kwmmul_round"
  [(set (match_operand:SI 0 "register_operand"                       "=r")
	(truncate:SI
	  (lshiftrt:DI
	    (unspec:DI [
	      (ss_mult:DI
		(mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand" " r")) (const_int 2))
		(mult:DI (sign_extend:DI (match_operand:SI 2 "register_operand" " r")) (const_int 2)))]
	      UNSPEC_ROUND)
	    (const_int 32))))]
  "NDS32_EXT_DSP_P ()"
  "kwmmul.u\t%0, %1, %2"
  [(set_attr "type"     "dmul")
   (set_attr "length"   "4")])

(define_expand "smmwb"
  [(match_operand:SI 0 "register_operand" "")
   (match_operand:SI 1 "register_operand" "")
   (match_operand:V2HI 2 "register_operand" "")]
  "NDS32_EXT_DSP_P ()"
{
  if (TARGET_BIG_ENDIAN)
    emit_insn (gen_smulhisi3_highpart_1 (operands[0], operands[1], operands[2], GEN_INT (1)));
  else
    emit_insn (gen_smulhisi3_highpart_1 (operands[0], operands[1], operands[2], GEN_INT (0)));
  DONE;
})

(define_expand "smmwt"
  [(match_operand:SI 0 "register_operand" "")
   (match_operand:SI 1 "register_operand" "")
   (match_operand:V2HI 2 "register_operand" "")]
  "NDS32_EXT_DSP_P ()"
{
  if (TARGET_BIG_ENDIAN)
    emit_insn (gen_smulhisi3_highpart_1 (operands[0], operands[1], operands[2], GEN_INT (0)));
  else
    emit_insn (gen_smulhisi3_highpart_1 (operands[0], operands[1], operands[2], GEN_INT (1)));
  DONE;
})

(define_insn "smulhisi3_highpart_1"
  [(set (match_operand:SI 0 "register_operand"                           "=   r,    r")
	(truncate:SI
	  (lshiftrt:DI
	    (mult:DI
	      (sign_extend:DI (match_operand:SI 1 "register_operand"     "    r,    r"))
	      (sign_extend:DI
	        (vec_select:HI
		  (match_operand:V2HI 2 "register_operand"               "    r,    r")
		  (parallel [(match_operand:SI 3 "nds32_imm_0_1_operand" " Iv00, Iv01")]))))
	    (const_int 16))))]
  "NDS32_EXT_DSP_P ()"
{
  if (TARGET_BIG_ENDIAN)
    {
      const char *pats[] = { "smmwt\t%0, %1, %2",
			     "smmwb\t%0, %1, %2" };
      return pats[which_alternative];
    }
  else
    {
      const char *pats[] = { "smmwb\t%0, %1, %2",
			     "smmwt\t%0, %1, %2" };
      return pats[which_alternative];
    }
}
  [(set_attr "type"     "dmul")
   (set_attr "length"   "4")])

(define_insn "smulhisi3_highpart_2"
  [(set (match_operand:SI 0 "register_operand"                           "=   r,    r")
	(truncate:SI
	  (lshiftrt:DI
	    (mult:DI
	      (sign_extend:DI
	        (vec_select:HI
		  (match_operand:V2HI 1 "register_operand"               "    r,    r")
		  (parallel [(match_operand:SI 3 "nds32_imm_0_1_operand" " Iv00, Iv01")])))
	      (sign_extend:DI (match_operand:SI 2 "register_operand"     "    r,    r")))
	    (const_int 16))))]
  "NDS32_EXT_DSP_P ()"
{
  if (TARGET_BIG_ENDIAN)
    {
      const char *pats[] = { "smmwt\t%0, %1, %2",
			     "smmwb\t%0, %1, %2" };
      return pats[which_alternative];
    }
  else
    {
      const char *pats[] = { "smmwb\t%0, %1, %2",
			     "smmwt\t%0, %1, %2" };
      return pats[which_alternative];
    }
}
  [(set_attr "type"     "dmul")
   (set_attr "length"   "4")])

(define_expand "smmwb_round"
  [(match_operand:SI 0 "register_operand" "")
   (match_operand:SI 1 "register_operand" "")
   (match_operand:V2HI 2 "register_operand" "")]
  "NDS32_EXT_DSP_P ()"
{
  if (TARGET_BIG_ENDIAN)
    emit_insn (gen_smmw_round_internal (operands[0], operands[1], operands[2], GEN_INT (1)));
  else
    emit_insn (gen_smmw_round_internal (operands[0], operands[1], operands[2], GEN_INT (0)));
  DONE;
})

(define_expand "smmwt_round"
  [(match_operand:SI 0 "register_operand" "")
   (match_operand:SI 1 "register_operand" "")
   (match_operand:V2HI 2 "register_operand" "")]
  "NDS32_EXT_DSP_P ()"
{
  if (TARGET_BIG_ENDIAN)
    emit_insn (gen_smmw_round_internal (operands[0], operands[1], operands[2], GEN_INT (0)));
  else
    emit_insn (gen_smmw_round_internal (operands[0], operands[1], operands[2], GEN_INT (1)));
  DONE;
})

(define_insn "smmw_round_internal"
  [(set (match_operand:SI 0 "register_operand"                           "=   r,    r")
	(truncate:SI
	  (lshiftrt:DI
	    (unspec:DI
	      [(mult:DI
		 (sign_extend:DI (match_operand:SI 1 "register_operand"     "    r,    r"))
		 (sign_extend:DI
		   (vec_select:HI
		     (match_operand:V2HI 2 "register_operand"               "    r,    r")
		     (parallel [(match_operand:SI 3 "nds32_imm_0_1_operand" " Iv00, Iv01")]))))]
	      UNSPEC_ROUND)
	    (const_int 16))))]
  "NDS32_EXT_DSP_P ()"
{
  if (TARGET_BIG_ENDIAN)
    {
      const char *pats[] = { "smmwt.u\t%0, %1, %2",
			     "smmwb.u\t%0, %1, %2" };
      return pats[which_alternative];
    }
  else
    {
      const char *pats[] = { "smmwb.u\t%0, %1, %2",
			     "smmwt.u\t%0, %1, %2" };
      return pats[which_alternative];
    }
}
  [(set_attr "type"     "dmul")
   (set_attr "length"   "4")])

(define_expand "kmmawb"
  [(match_operand:SI 0 "register_operand" "")
   (match_operand:SI 1 "register_operand" "")
   (match_operand:SI 2 "register_operand" "")
   (match_operand:V2HI 3 "register_operand" "")]
  "NDS32_EXT_DSP_P ()"
{
  if (TARGET_BIG_ENDIAN)
    emit_insn (gen_kmmaw_internal (operands[0], operands[2], operands[3], GEN_INT (1), operands[1]));
  else
    emit_insn (gen_kmmaw_internal (operands[0], operands[2], operands[3], GEN_INT (0), operands[1]));
  DONE;
})

(define_expand "kmmawt"
  [(match_operand:SI 0 "register_operand" "")
   (match_operand:SI 1 "register_operand" "")
   (match_operand:SI 2 "register_operand" "")
   (match_operand:V2HI 3 "register_operand" "")]
  "NDS32_EXT_DSP_P ()"
{
  if (TARGET_BIG_ENDIAN)
    emit_insn (gen_kmmaw_internal (operands[0], operands[2], operands[3], GEN_INT (0), operands[1]));
  else
    emit_insn (gen_kmmaw_internal (operands[0], operands[2], operands[3], GEN_INT (1), operands[1]));
  DONE;
})

(define_insn "kmmaw_internal"
  [(set (match_operand:SI 0 "register_operand"                               "=   r,    r")
	(ss_plus:SI
	  (match_operand:SI 4 "register_operand"                             "    0,    0")
	  (truncate:SI
	    (lshiftrt:DI
	      (mult:DI
		(sign_extend:DI (match_operand:SI 1 "register_operand"       "    r,    r"))
		  (sign_extend:DI
		    (vec_select:HI
		      (match_operand:V2HI 2 "register_operand"               "    r,    r")
		      (parallel [(match_operand:SI 3 "nds32_imm_0_1_operand" " Iv00, Iv01")]))))
	      (const_int 16)))))]
  "NDS32_EXT_DSP_P ()"
{
  if (TARGET_BIG_ENDIAN)
    {
      const char *pats[] = { "kmmawt\t%0, %1, %2",
			     "kmmawb\t%0, %1, %2" };
      return pats[which_alternative];
    }
  else
    {
      const char *pats[] = { "kmmawb\t%0, %1, %2",
			     "kmmawt\t%0, %1, %2" };
      return pats[which_alternative];
    }
}
  [(set_attr "type"     "dmac")
   (set_attr "length"   "4")])

(define_expand "kmmawb_round"
  [(match_operand:SI 0 "register_operand" "")
   (match_operand:SI 1 "register_operand" "")
   (match_operand:SI 2 "register_operand" "")
   (match_operand:V2HI 3 "register_operand" "")]
  "NDS32_EXT_DSP_P ()"
{
  if (TARGET_BIG_ENDIAN)
    emit_insn (gen_kmmaw_round_internal (operands[0], operands[2], operands[3], GEN_INT (1), operands[1]));
  else
    emit_insn (gen_kmmaw_round_internal (operands[0], operands[2], operands[3], GEN_INT (0), operands[1]));
  DONE;
}
  [(set_attr "type"   "alu")
   (set_attr "length"   "4")])

(define_expand "kmmawt_round"
  [(match_operand:SI 0 "register_operand" "")
   (match_operand:SI 1 "register_operand" "")
   (match_operand:SI 2 "register_operand" "")
   (match_operand:V2HI 3 "register_operand" "")]
  "NDS32_EXT_DSP_P ()"
{
  if (TARGET_BIG_ENDIAN)
    emit_insn (gen_kmmaw_round_internal (operands[0], operands[2], operands[3], GEN_INT (0), operands[1]));
  else
    emit_insn (gen_kmmaw_round_internal (operands[0], operands[2], operands[3], GEN_INT (1), operands[1]));
  DONE;
}
  [(set_attr "type"     "dmac")
   (set_attr "length"   "4")])


(define_insn "kmmaw_round_internal"
  [(set (match_operand:SI 0 "register_operand"                                "=   r,    r")
	(ss_plus:SI
	  (match_operand:SI 4 "register_operand"                              "    0,    0")
	  (truncate:SI
	    (lshiftrt:DI
	      (unspec:DI
		[(mult:DI
		   (sign_extend:DI (match_operand:SI 1 "register_operand"     "    r,    r"))
		   (sign_extend:DI
		     (vec_select:HI
		       (match_operand:V2HI 2 "register_operand"               "    r,    r")
		       (parallel [(match_operand:SI 3 "nds32_imm_0_1_operand" " Iv00, Iv01")]))))]
		UNSPEC_ROUND)
	      (const_int 16)))))]
  "NDS32_EXT_DSP_P ()"
{
  if (TARGET_BIG_ENDIAN)
    {
      const char *pats[] = { "kmmawt.u\t%0, %1, %2",
			     "kmmawb.u\t%0, %1, %2" };
      return pats[which_alternative];
    }
  else
    {
      const char *pats[] = { "kmmawb.u\t%0, %1, %2",
			     "kmmawt.u\t%0, %1, %2" };
      return pats[which_alternative];
    }
}
  [(set_attr "type"     "dmac")
   (set_attr "length"   "4")])

(define_expand "smalbb"
  [(match_operand:DI 0 "register_operand" "")
   (match_operand:DI 1 "register_operand" "")
   (match_operand:V2HI 2 "register_operand" "")
   (match_operand:V2HI 3 "register_operand" "")]
  "NDS32_EXT_DSP_P ()"
{
  if (TARGET_BIG_ENDIAN)
    emit_insn (gen_smaddhidi (operands[0], operands[2],
			      operands[3], operands[1],
			      GEN_INT (1), GEN_INT (1)));
  else
    emit_insn (gen_smaddhidi (operands[0], operands[2],
			      operands[3], operands[1],
			      GEN_INT (0), GEN_INT (0)));
  DONE;
})

(define_expand "smalbt"
  [(match_operand:DI 0 "register_operand" "")
   (match_operand:DI 1 "register_operand" "")
   (match_operand:V2HI 2 "register_operand" "")
   (match_operand:V2HI 3 "register_operand" "")]
  "NDS32_EXT_DSP_P ()"
{
  if (TARGET_BIG_ENDIAN)
    emit_insn (gen_smaddhidi (operands[0], operands[2],
			      operands[3], operands[1],
			      GEN_INT (1), GEN_INT (0)));
  else
    emit_insn (gen_smaddhidi (operands[0], operands[2],
			      operands[3], operands[1],
			      GEN_INT (0), GEN_INT (1)));
  DONE;
})

(define_expand "smaltt"
  [(match_operand:DI 0 "register_operand" "")
   (match_operand:DI 1 "register_operand" "")
   (match_operand:V2HI 2 "register_operand" "")
   (match_operand:V2HI 3 "register_operand" "")]
  "NDS32_EXT_DSP_P ()"
{
  if (TARGET_BIG_ENDIAN)
    emit_insn (gen_smaddhidi (operands[0], operands[2],
			      operands[3], operands[1],
			      GEN_INT (0), GEN_INT (0)));
  else
    emit_insn (gen_smaddhidi (operands[0], operands[2],
			      operands[3], operands[1],
			      GEN_INT (1), GEN_INT (1)));
  DONE;
})

(define_insn "smaddhidi"
  [(set (match_operand:DI 0 "register_operand"                         "=   r,    r,    r,    r")
	(plus:DI
	  (match_operand:DI 3 "register_operand"                       "    0,    0,    0,    0")
	  (mult:DI
	    (sign_extend:DI
	      (vec_select:HI
		(match_operand:V2HI 1 "register_operand"               "    r,    r,    r,    r")
		(parallel [(match_operand:SI 4 "nds32_imm_0_1_operand" " Iv00, Iv00, Iv01, Iv01")])))
	    (sign_extend:DI
	      (vec_select:HI
		(match_operand:V2HI 2 "register_operand"               "    r,    r,    r,    r")
		(parallel [(match_operand:SI 5 "nds32_imm_0_1_operand" " Iv00, Iv01, Iv01, Iv00")]))))))]
  "NDS32_EXT_DSP_P ()"
{
  if (TARGET_BIG_ENDIAN)
    {
      const char *pats[] = { "smaltt\t%0, %1, %2",
			     "smalbt\t%0, %2, %1",
			     "smalbb\t%0, %1, %2",
			     "smalbt\t%0, %1, %2" };
      return pats[which_alternative];
    }
  else
    {
      const char *pats[] = { "smalbb\t%0, %1, %2",
			     "smalbt\t%0, %1, %2",
			     "smaltt\t%0, %1, %2",
			     "smalbt\t%0, %2, %1" };
      return pats[which_alternative];
    }
}
  [(set_attr "type"     "dmac")
   (set_attr "length"   "4")])

(define_insn "smaddhidi2"
  [(set (match_operand:DI 0 "register_operand"                         "=   r,    r,    r,    r")
	(plus:DI
	  (mult:DI
	    (sign_extend:DI
	      (vec_select:HI
		(match_operand:V2HI 1 "register_operand"               "    r,    r,    r,    r")
		(parallel [(match_operand:SI 4 "nds32_imm_0_1_operand" " Iv00, Iv00, Iv01, Iv01")])))
	    (sign_extend:DI
	      (vec_select:HI
		(match_operand:V2HI 2 "register_operand"               "    r,    r,    r,    r")
		(parallel [(match_operand:SI 5 "nds32_imm_0_1_operand" " Iv00, Iv01, Iv01, Iv00")]))))
	  (match_operand:DI 3 "register_operand"                       "    0,    0,    0,    0")))]
  "NDS32_EXT_DSP_P ()"
{
  if (TARGET_BIG_ENDIAN)
    {
      const char *pats[] = { "smaltt\t%0, %1, %2",
			     "smalbt\t%0, %2, %1",
			     "smalbb\t%0, %1, %2",
			     "smalbt\t%0, %1, %2" };
      return pats[which_alternative];
    }
  else
    {
      const char *pats[] = { "smalbb\t%0, %1, %2",
			     "smalbt\t%0, %1, %2",
			     "smaltt\t%0, %1, %2",
			     "smalbt\t%0, %2, %1" };
      return pats[which_alternative];
    }
}
  [(set_attr "type"     "dmac")
   (set_attr "length"   "4")])

(define_expand "smalda1"
  [(match_operand:DI 0 "register_operand" "")
   (match_operand:DI 1 "register_operand" "")
   (match_operand:V2HI 2 "register_operand" " r")
   (match_operand:V2HI 3 "register_operand" " r")]
  "NDS32_EXT_DSP_P ()"
{
  if (TARGET_BIG_ENDIAN)
    emit_insn (gen_smalda1_be (operands[0], operands[1], operands[2], operands[3]));
  else
    emit_insn (gen_smalda1_le (operands[0], operands[1], operands[2], operands[3]));
  DONE;
})

(define_expand "smalds1"
  [(match_operand:DI 0 "register_operand" "")
   (match_operand:DI 1 "register_operand" "")
   (match_operand:V2HI 2 "register_operand" " r")
   (match_operand:V2HI 3 "register_operand" " r")]
  "NDS32_EXT_DSP_P ()"
{
  if (TARGET_BIG_ENDIAN)
    emit_insn (gen_smalds1_be (operands[0], operands[1], operands[2], operands[3]));
  else
    emit_insn (gen_smalds1_le (operands[0], operands[1], operands[2], operands[3]));
  DONE;
})

(define_insn "smalda1_le"
  [(set (match_operand:DI 0 "register_operand"                             "=r")
	(plus:DI
	  (match_operand:DI 1 "register_operand"                           " 0")
	  (sign_extend:DI
	    (plus:SI
	      (mult:SI
		(sign_extend:SI (vec_select:HI
				  (match_operand:V2HI 2 "register_operand" " r")
				  (parallel [(const_int 1)])))
		(sign_extend:SI (vec_select:HI
				  (match_operand:V2HI 3 "register_operand" " r")
				  (parallel [(const_int 1)]))))
	      (mult:SI
		(sign_extend:SI (vec_select:HI
				  (match_dup 2)
				  (parallel [(const_int 0)])))
		(sign_extend:SI (vec_select:HI
				  (match_dup 3)
				  (parallel [(const_int 0)]))))))))]
  "NDS32_EXT_DSP_P () && !TARGET_BIG_ENDIAN"
  "smalda\t%0, %2, %3"
  [(set_attr "type"    "dmac")
   (set_attr "length"   "4")])

(define_insn "smalds1_le"
  [(set (match_operand:DI 0 "register_operand"                             "=r")
	(plus:DI
	  (match_operand:DI 1 "register_operand"                           " 0")
	  (sign_extend:DI
	    (minus:SI
	      (mult:SI
		(sign_extend:SI (vec_select:HI
				  (match_operand:V2HI 2 "register_operand" " r")
				  (parallel [(const_int 1)])))
		(sign_extend:SI (vec_select:HI
				  (match_operand:V2HI 3 "register_operand" " r")
				  (parallel [(const_int 1)]))))
	      (mult:SI
		(sign_extend:SI (vec_select:HI
				  (match_dup 2)
				  (parallel [(const_int 0)])))
		(sign_extend:SI (vec_select:HI
				  (match_dup 3)
				  (parallel [(const_int 0)]))))))))]
  "NDS32_EXT_DSP_P () && !TARGET_BIG_ENDIAN"
  "smalds\t%0, %2, %3"
  [(set_attr "type"     "dmac")
   (set_attr "length"   "4")])

(define_insn "smalda1_be"
  [(set (match_operand:DI 0 "register_operand"                             "=r")
	(plus:DI
	  (match_operand:DI 1 "register_operand"                           " 0")
	  (sign_extend:DI
	    (plus:SI
	      (mult:SI
		(sign_extend:SI (vec_select:HI
				  (match_operand:V2HI 2 "register_operand" " r")
				  (parallel [(const_int 0)])))
		(sign_extend:SI (vec_select:HI
				  (match_operand:V2HI 3 "register_operand" " r")
				  (parallel [(const_int 0)]))))
	      (mult:SI
		(sign_extend:SI (vec_select:HI
				  (match_dup 2)
				  (parallel [(const_int 1)])))
		(sign_extend:SI (vec_select:HI
				  (match_dup 3)
				  (parallel [(const_int 1)]))))))))]
  "NDS32_EXT_DSP_P () && TARGET_BIG_ENDIAN"
  "smalda\t%0, %2, %3"
  [(set_attr "type"     "dmac")
   (set_attr "length"   "4")])

(define_insn "smalds1_be"
  [(set (match_operand:DI 0 "register_operand"                             "=r")
	(plus:DI
	  (match_operand:DI 1 "register_operand"                           " 0")
	  (sign_extend:DI
	    (minus:SI
	      (mult:SI
		(sign_extend:SI (vec_select:HI
				  (match_operand:V2HI 2 "register_operand" " r")
				  (parallel [(const_int 0)])))
		(sign_extend:SI (vec_select:HI
				  (match_operand:V2HI 3 "register_operand" " r")
				  (parallel [(const_int 0)]))))
	      (mult:SI
		(sign_extend:SI (vec_select:HI
				  (match_dup 2)
				  (parallel [(const_int 1)])))
		(sign_extend:SI (vec_select:HI
				  (match_dup 3)
				  (parallel [(const_int 1)]))))))))]
  "NDS32_EXT_DSP_P () && TARGET_BIG_ENDIAN"
  "smalds\t%0, %2, %3"
  [(set_attr "type"     "dmac")
   (set_attr "length"   "4")])

(define_expand "smaldrs3"
  [(match_operand:DI 0 "register_operand" "")
   (match_operand:DI 1 "register_operand" "")
   (match_operand:V2HI 2 "register_operand" " r")
   (match_operand:V2HI 3 "register_operand" " r")]
  "NDS32_EXT_DSP_P ()"
{
  if (TARGET_BIG_ENDIAN)
    emit_insn (gen_smaldrs3_be (operands[0], operands[1], operands[2], operands[3]));
  else
    emit_insn (gen_smaldrs3_le (operands[0], operands[1], operands[2], operands[3]));
  DONE;
})

(define_insn "smaldrs3_le"
  [(set (match_operand:DI 0 "register_operand"                             "=r")
	(plus:DI
	  (match_operand:DI 1 "register_operand"                           " 0")
	  (sign_extend:DI
	    (minus:SI
	      (mult:SI
		(sign_extend:SI (vec_select:HI
				  (match_operand:V2HI 2 "register_operand" " r")
				  (parallel [(const_int 0)])))
		(sign_extend:SI (vec_select:HI
				  (match_operand:V2HI 3 "register_operand" " r")
				  (parallel [(const_int 0)]))))
	      (mult:SI
		(sign_extend:SI (vec_select:HI
				  (match_dup 2)
				  (parallel [(const_int 1)])))
		(sign_extend:SI (vec_select:HI
				  (match_dup 3)
				  (parallel [(const_int 1)]))))))))]
  "NDS32_EXT_DSP_P () && !TARGET_BIG_ENDIAN"
  "smaldrs\t%0, %2, %3"
  [(set_attr "type"     "dmac")
   (set_attr "length"   "4")])

(define_insn "smaldrs3_be"
  [(set (match_operand:DI 0 "register_operand"                             "=r")
	(plus:DI
	  (match_operand:DI 1 "register_operand"                           " 0")
	  (sign_extend:DI
	    (minus:SI
	      (mult:SI
		(sign_extend:SI (vec_select:HI
				  (match_operand:V2HI 2 "register_operand" " r")
				  (parallel [(const_int 1)])))
		(sign_extend:SI (vec_select:HI
				  (match_operand:V2HI 3 "register_operand" " r")
				  (parallel [(const_int 1)]))))
	      (mult:SI
		(sign_extend:SI (vec_select:HI
				  (match_dup 2)
				  (parallel [(const_int 0)])))
		(sign_extend:SI (vec_select:HI
				  (match_dup 3)
				  (parallel [(const_int 0)]))))))))]
  "NDS32_EXT_DSP_P () && TARGET_BIG_ENDIAN"
  "smaldrs\t%0, %2, %3"
  [(set_attr "type"     "dmac")
   (set_attr "length"   "4")])

(define_expand "smalxda1"
  [(match_operand:DI 0 "register_operand" "")
   (match_operand:DI 1 "register_operand" "")
   (match_operand:V2HI 2 "register_operand" " r")
   (match_operand:V2HI 3 "register_operand" " r")]
  "NDS32_EXT_DSP_P ()"
{
  if (TARGET_BIG_ENDIAN)
    emit_insn (gen_smalxda1_be (operands[0], operands[1], operands[2], operands[3]));
  else
    emit_insn (gen_smalxda1_le (operands[0], operands[1], operands[2], operands[3]));
  DONE;
})

(define_expand "smalxds1"
  [(match_operand:DI 0 "register_operand" "")
   (match_operand:DI 1 "register_operand" "")
   (match_operand:V2HI 2 "register_operand" " r")
   (match_operand:V2HI 3 "register_operand" " r")]
  "NDS32_EXT_DSP_P ()"
{
  if (TARGET_BIG_ENDIAN)
    emit_insn (gen_smalxds1_be (operands[0], operands[1], operands[2], operands[3]));
  else
    emit_insn (gen_smalxds1_le (operands[0], operands[1], operands[2], operands[3]));
  DONE;
})

(define_insn "smalxd<add_sub>1_le"
  [(set (match_operand:DI 0 "register_operand"                             "=r")
	(plus:DI
	  (match_operand:DI 1 "register_operand"                           " 0")
	  (sign_extend:DI
	    (plus_minus:SI
	      (mult:SI
		(sign_extend:SI (vec_select:HI
				  (match_operand:V2HI 2 "register_operand" " r")
				  (parallel [(const_int 1)])))
		(sign_extend:SI (vec_select:HI
				  (match_operand:V2HI 3 "register_operand" " r")
				  (parallel [(const_int 0)]))))
	      (mult:SI
		(sign_extend:SI (vec_select:HI
				  (match_dup 2)
				  (parallel [(const_int 0)])))
		(sign_extend:SI (vec_select:HI
				  (match_dup 3)
				  (parallel [(const_int 1)]))))))))]
  "NDS32_EXT_DSP_P () && !TARGET_BIG_ENDIAN"
  "smalxd<add_sub>\t%0, %2, %3"
  [(set_attr "type"     "dmac")
   (set_attr "length"   "4")])


(define_insn "smalxd<add_sub>1_be"
  [(set (match_operand:DI 0 "register_operand"                             "=r")
	(plus:DI
	  (match_operand:DI 1 "register_operand"                           " 0")
	  (sign_extend:DI
	    (plus_minus:SI
	      (mult:SI
		(sign_extend:SI (vec_select:HI
				  (match_operand:V2HI 2 "register_operand" " r")
				  (parallel [(const_int 0)])))
		(sign_extend:SI (vec_select:HI
				  (match_operand:V2HI 3 "register_operand" " r")
				  (parallel [(const_int 1)]))))
	      (mult:SI
		(sign_extend:SI (vec_select:HI
				  (match_dup 2)
				  (parallel [(const_int 1)])))
		(sign_extend:SI (vec_select:HI
				  (match_dup 3)
				  (parallel [(const_int 0)]))))))))]
  "NDS32_EXT_DSP_P () && TARGET_BIG_ENDIAN"
  "smalxd<add_sub>\t%0, %2, %3"
  [(set_attr "type"     "dmac")
   (set_attr "length"   "4")])

(define_insn "smslda1"
  [(set (match_operand:DI 0 "register_operand"                             "=r")
	(minus:DI
	  (minus:DI
	    (match_operand:DI 1 "register_operand"                           " 0")
	    (sign_extend:DI
	      (mult:SI
		(sign_extend:SI (vec_select:HI
				  (match_operand:V2HI 2 "register_operand" " r")
				  (parallel [(const_int 1)])))
		(sign_extend:SI (vec_select:HI
				  (match_operand:V2HI 3 "register_operand" " r")
				  (parallel [(const_int 1)]))))))
	  (sign_extend:DI
	    (mult:SI
	      (sign_extend:SI (vec_select:HI
				(match_dup 2)
				(parallel [(const_int 0)])))
	      (sign_extend:SI (vec_select:HI
				(match_dup 3)
				(parallel [(const_int 0)])))))))]
  "NDS32_EXT_DSP_P ()"
  "smslda\t%0, %2, %3"
  [(set_attr "type"     "dmac")
   (set_attr "length"   "4")])

(define_insn "smslxda1"
  [(set (match_operand:DI 0 "register_operand"                             "=r")
	(minus:DI
	  (minus:DI
	    (match_operand:DI 1 "register_operand"                           " 0")
	      (sign_extend:DI
		(mult:SI
		  (sign_extend:SI (vec_select:HI
				    (match_operand:V2HI 2 "register_operand" " r")
				    (parallel [(const_int 1)])))
		  (sign_extend:SI (vec_select:HI
				    (match_operand:V2HI 3 "register_operand" " r")
				    (parallel [(const_int 0)]))))))
	  (sign_extend:DI
	    (mult:SI
	      (sign_extend:SI (vec_select:HI
				(match_dup 2)
				(parallel [(const_int 0)])))
	      (sign_extend:SI (vec_select:HI
				(match_dup 3)
				(parallel [(const_int 1)])))))))]
  "NDS32_EXT_DSP_P ()"
  "smslxda\t%0, %2, %3"
  [(set_attr "type"     "dmac")
   (set_attr "length"   "4")])

;; mada for synthetize smalda
(define_insn_and_split "mada1"
  [(set (match_operand:SI 0 "register_operand"                          "=r")
	(plus:SI
	  (mult:SI
	    (sign_extend:SI (vec_select:HI
			      (match_operand:V2HI 1 "register_operand" "r")
			      (parallel [(match_operand:SI 3 "nds32_imm_0_1_operand" " Iu01")])))
	    (sign_extend:SI (vec_select:HI
			      (match_operand:V2HI 2 "register_operand" "r")
			      (parallel [(match_operand:SI 4 "nds32_imm_0_1_operand" " Iu01")]))))
	  (mult:SI
	    (sign_extend:SI (vec_select:HI
			      (match_dup 1)
			      (parallel [(match_operand:SI 5 "nds32_imm_0_1_operand" " Iu01")])))
	    (sign_extend:SI (vec_select:HI
			      (match_dup 2)
			      (parallel [(match_operand:SI 6 "nds32_imm_0_1_operand" " Iu01")]))))))]
  "NDS32_EXT_DSP_P () && !reload_completed"
  "#"
  "NDS32_EXT_DSP_P () && !reload_completed"
  [(const_int 1)]
{
  rtx result0 = gen_reg_rtx (SImode);
  rtx result1 = gen_reg_rtx (SImode);
  emit_insn (gen_mulhisi3v (result0, operands[1], operands[2],
			    operands[3], operands[4]));
  emit_insn (gen_mulhisi3v (result1, operands[1], operands[2],
			    operands[5], operands[6]));
  emit_insn (gen_addsi3 (operands[0], result0, result1));
  DONE;
})

(define_insn_and_split "mada2"
  [(set (match_operand:SI 0 "register_operand"                          "=r")
	(plus:SI
	  (mult:SI
	    (sign_extend:SI (vec_select:HI
			      (match_operand:V2HI 1 "register_operand" "r")
			      (parallel [(match_operand:SI 3 "nds32_imm_0_1_operand" " Iu01")])))
	    (sign_extend:SI (vec_select:HI
			      (match_operand:V2HI 2 "register_operand" "r")
			      (parallel [(match_operand:SI 4 "nds32_imm_0_1_operand" " Iu01")]))))
	  (mult:SI
	    (sign_extend:SI (vec_select:HI
			      (match_dup 2)
			      (parallel [(match_operand:SI 5 "nds32_imm_0_1_operand" " Iu01")])))
	    (sign_extend:SI (vec_select:HI
			      (match_dup 1)
			      (parallel [(match_operand:SI 6 "nds32_imm_0_1_operand" " Iu01")]))))))]
  "NDS32_EXT_DSP_P () && !reload_completed"
  "#"
  "NDS32_EXT_DSP_P () && !reload_completed"
  [(const_int 1)]
{
  rtx result0 = gen_reg_rtx (SImode);
  rtx result1 = gen_reg_rtx (SImode);
  emit_insn (gen_mulhisi3v (result0, operands[1], operands[2],
			    operands[3], operands[4]));
  emit_insn (gen_mulhisi3v (result1, operands[1], operands[2],
			    operands[6], operands[5]));
  emit_insn (gen_addsi3 (operands[0], result0, result1));
  DONE;
})

;; sms for synthetize smalds
(define_insn_and_split "sms1"
  [(set (match_operand:SI 0 "register_operand"                                       "=   r")
	(minus:SI
	  (mult:SI
	    (sign_extend:SI (vec_select:HI
			      (match_operand:V2HI 1 "register_operand"               "    r")
			      (parallel [(match_operand:SI 3 "nds32_imm_0_1_operand" " Iu01")])))
	    (sign_extend:SI (vec_select:HI
			      (match_operand:V2HI 2 "register_operand"               "    r")
			      (parallel [(match_operand:SI 4 "nds32_imm_0_1_operand" " Iu01")]))))
	  (mult:SI
	    (sign_extend:SI (vec_select:HI
			      (match_dup 1)
			      (parallel [(match_operand:SI 5 "nds32_imm_0_1_operand" " Iu01")])))
	    (sign_extend:SI (vec_select:HI
			      (match_dup 2)
			      (parallel [(match_operand:SI 6 "nds32_imm_0_1_operand" " Iu01")]))))))]
  "NDS32_EXT_DSP_P ()
   && (!reload_completed
       || !nds32_need_split_sms_p (operands[3], operands[4],
				   operands[5], operands[6]))"

{
  return nds32_output_sms (operands[3], operands[4],
			   operands[5], operands[6]);
}
  "NDS32_EXT_DSP_P ()
   && !reload_completed
   && nds32_need_split_sms_p (operands[3], operands[4],
			      operands[5], operands[6])"
  [(const_int 1)]
{
  nds32_split_sms (operands[0], operands[1], operands[2],
		   operands[3], operands[4],
		   operands[5], operands[6]);
  DONE;
}
  [(set_attr "type"     "dmac")
   (set_attr "length"   "4")])

(define_insn_and_split "sms2"
  [(set (match_operand:SI 0 "register_operand"                                       "=   r")
	(minus:SI
	  (mult:SI
	    (sign_extend:SI (vec_select:HI
			      (match_operand:V2HI 1 "register_operand"               "    r")
			      (parallel [(match_operand:SI 3 "nds32_imm_0_1_operand" " Iu01")])))
	    (sign_extend:SI (vec_select:HI
			      (match_operand:V2HI 2 "register_operand"               "    r")
			      (parallel [(match_operand:SI 4 "nds32_imm_0_1_operand" " Iu01")]))))
	  (mult:SI
	    (sign_extend:SI (vec_select:HI
			      (match_dup 2)
			      (parallel [(match_operand:SI 5 "nds32_imm_0_1_operand" " Iu01")])))
	    (sign_extend:SI (vec_select:HI
			      (match_dup 1)
			      (parallel [(match_operand:SI 6 "nds32_imm_0_1_operand" " Iu01")]))))))]
  "NDS32_EXT_DSP_P ()
   && (!reload_completed
       || !nds32_need_split_sms_p (operands[3], operands[4],
				   operands[6], operands[5]))"
{
  return nds32_output_sms (operands[3], operands[4],
			   operands[6], operands[5]);
}
  "NDS32_EXT_DSP_P ()
   && !reload_completed
   && nds32_need_split_sms_p (operands[3], operands[4],
			      operands[6], operands[5])"
  [(const_int 1)]
{
  nds32_split_sms (operands[0], operands[1], operands[2],
		   operands[3], operands[4],
		   operands[6], operands[5]);
  DONE;
}
  [(set_attr "type"     "dmac")
   (set_attr "length"   "4")])

(define_insn "kmda"
  [(set (match_operand:SI 0 "register_operand"                          "=r")
	(ss_plus:SI
	  (mult:SI
	    (sign_extend:SI (vec_select:HI
			      (match_operand:V2HI 1 "register_operand" "r")
			      (parallel [(const_int 1)])))
	    (sign_extend:SI (vec_select:HI
			      (match_operand:V2HI 2 "register_operand" "r")
			      (parallel [(const_int 1)]))))
	  (mult:SI
	    (sign_extend:SI (vec_select:HI
			      (match_dup 1)
			      (parallel [(const_int 0)])))
	    (sign_extend:SI (vec_select:HI
			      (match_dup 2)
			      (parallel [(const_int 0)]))))))]
  "NDS32_EXT_DSP_P ()"
  "kmda\t%0, %1, %2"
  [(set_attr "type"     "dmac")
   (set_attr "length"   "4")])

(define_insn "kmxda"
  [(set (match_operand:SI 0 "register_operand"                          "=r")
	(ss_plus:SI
	  (mult:SI
	    (sign_extend:SI (vec_select:HI
			      (match_operand:V2HI 1 "register_operand" "r")
			      (parallel [(const_int 1)])))
	    (sign_extend:SI (vec_select:HI
			      (match_operand:V2HI 2 "register_operand" "r")
			      (parallel [(const_int 0)]))))
	  (mult:SI
	    (sign_extend:SI (vec_select:HI
			      (match_dup 1)
			      (parallel [(const_int 0)])))
	    (sign_extend:SI (vec_select:HI
			      (match_dup 2)
			      (parallel [(const_int 1)]))))))]
  "NDS32_EXT_DSP_P ()"
  "kmxda\t%0, %1, %2"
  [(set_attr "type"     "dmac")
   (set_attr "length"   "4")])

(define_insn "kmada"
  [(set (match_operand:SI 0 "register_operand"                           "=r")
	(ss_plus:SI
	  (match_operand:SI 1 "register_operand"                         " 0")
	  (ss_plus:SI
	    (mult:SI
	      (sign_extend:SI (vec_select:HI
				(match_operand:V2HI 2 "register_operand" " r")
				(parallel [(const_int 1)])))
	      (sign_extend:SI (vec_select:HI
				(match_operand:V2HI 3 "register_operand" " r")
				(parallel [(const_int 1)]))))
	    (mult:SI
	      (sign_extend:SI (vec_select:HI
				(match_dup 2)
				(parallel [(const_int 0)])))
	      (sign_extend:SI (vec_select:HI
				(match_dup 3)
				(parallel [(const_int 0)])))))))]
  "NDS32_EXT_DSP_P ()"
  "kmada\t%0, %2, %3"
  [(set_attr "type"     "dmac")
   (set_attr "length"   "4")])

(define_insn "kmada2"
  [(set (match_operand:SI 0 "register_operand"                           "=r")
	(ss_plus:SI
	  (match_operand:SI 1 "register_operand"                         " 0")
	  (ss_plus:SI
	    (mult:SI
	      (sign_extend:SI (vec_select:HI
				(match_operand:V2HI 2 "register_operand" " r")
				(parallel [(const_int 0)])))
	      (sign_extend:SI (vec_select:HI
				(match_operand:V2HI 3 "register_operand" " r")
				(parallel [(const_int 0)]))))
	    (mult:SI
	      (sign_extend:SI (vec_select:HI
				(match_dup 2)
				(parallel [(const_int 1)])))
	      (sign_extend:SI (vec_select:HI
				(match_dup 3)
				(parallel [(const_int 1)])))))))]
  "NDS32_EXT_DSP_P ()"
  "kmada\t%0, %2, %3"
  [(set_attr "type"     "dmac")
   (set_attr "length"   "4")])

(define_insn "kmaxda"
  [(set (match_operand:SI 0 "register_operand"                           "=r")
	(ss_plus:SI
	  (match_operand:SI 1 "register_operand"                         " 0")
	  (ss_plus:SI
	    (mult:SI
	      (sign_extend:SI (vec_select:HI
				(match_operand:V2HI 2 "register_operand" " r")
				(parallel [(const_int 1)])))
	      (sign_extend:SI (vec_select:HI
				(match_operand:V2HI 3 "register_operand" " r")
				(parallel [(const_int 0)]))))
	    (mult:SI
	      (sign_extend:SI (vec_select:HI
				(match_dup 2)
				(parallel [(const_int 0)])))
	      (sign_extend:SI (vec_select:HI
				(match_dup 3)
				(parallel [(const_int 1)])))))))]
  "NDS32_EXT_DSP_P ()"
  "kmaxda\t%0, %2, %3"
  [(set_attr "type"     "dmac")
   (set_attr "length"   "4")])

(define_insn "kmads"
  [(set (match_operand:SI 0 "register_operand"                           "=r")
	(ss_plus:SI
	  (match_operand:SI 1 "register_operand"                         " 0")
	  (ss_minus:SI
	    (mult:SI
	      (sign_extend:SI (vec_select:HI
				(match_operand:V2HI 2 "register_operand" " r")
				(parallel [(const_int 1)])))
	      (sign_extend:SI (vec_select:HI
				(match_operand:V2HI 3 "register_operand" " r")
				(parallel [(const_int 1)]))))
	    (mult:SI
	      (sign_extend:SI (vec_select:HI
				(match_dup 2)
				(parallel [(const_int 0)])))
	      (sign_extend:SI (vec_select:HI
				(match_dup 3)
				(parallel [(const_int 0)])))))))]
  "NDS32_EXT_DSP_P ()"
  "kmads\t%0, %2, %3"
  [(set_attr "type"     "dmac")
   (set_attr "length"   "4")])

(define_insn "kmadrs"
  [(set (match_operand:SI 0 "register_operand"                           "=r")
	(ss_plus:SI
	  (match_operand:SI 1 "register_operand"                         " 0")
	  (ss_minus:SI
	    (mult:SI
	      (sign_extend:SI (vec_select:HI
				(match_operand:V2HI 2 "register_operand" " r")
				(parallel [(const_int 0)])))
	      (sign_extend:SI (vec_select:HI
				(match_operand:V2HI 3 "register_operand" " r")
				(parallel [(const_int 0)]))))
	    (mult:SI
	      (sign_extend:SI (vec_select:HI
				(match_dup 2)
				(parallel [(const_int 1)])))
	      (sign_extend:SI (vec_select:HI
				(match_dup 3)
				(parallel [(const_int 1)])))))))]
  "NDS32_EXT_DSP_P ()"
  "kmadrs\t%0, %2, %3"
  [(set_attr "type"     "dmac")
   (set_attr "length"   "4")])

(define_insn "kmaxds"
  [(set (match_operand:SI 0 "register_operand"                           "=r")
	(ss_plus:SI
	  (match_operand:SI 1 "register_operand"                         " 0")
	  (ss_minus:SI
	    (mult:SI
	      (sign_extend:SI (vec_select:HI
				(match_operand:V2HI 2 "register_operand" " r")
				(parallel [(const_int 1)])))
	      (sign_extend:SI (vec_select:HI
				(match_operand:V2HI 3 "register_operand" " r")
				(parallel [(const_int 0)]))))
	    (mult:SI
	      (sign_extend:SI (vec_select:HI
				(match_dup 2)
				(parallel [(const_int 0)])))
	      (sign_extend:SI (vec_select:HI
				(match_dup 3)
				(parallel [(const_int 1)])))))))]
  "NDS32_EXT_DSP_P ()"
  "kmaxds\t%0, %2, %3"
  [(set_attr "type"     "dmac")
   (set_attr "length"   "4")])

(define_insn "kmsda"
  [(set (match_operand:SI 0 "register_operand"                           "=r")
	(ss_minus:SI
	  (match_operand:SI 1 "register_operand"                         " 0")
	  (ss_minus:SI
	    (mult:SI
	      (sign_extend:SI (vec_select:HI
				(match_operand:V2HI 2 "register_operand" " r")
				(parallel [(const_int 1)])))
	      (sign_extend:SI (vec_select:HI
				(match_operand:V2HI 3 "register_operand" " r")
				(parallel [(const_int 1)]))))
	    (mult:SI
	      (sign_extend:SI (vec_select:HI
				(match_dup 2)
				(parallel [(const_int 0)])))
	      (sign_extend:SI (vec_select:HI
				(match_dup 3)
				(parallel [(const_int 0)])))))))]
  "NDS32_EXT_DSP_P ()"
  "kmsda\t%0, %2, %3"
  [(set_attr "type"     "dmac")
   (set_attr "length"   "4")])

(define_insn "kmsxda"
  [(set (match_operand:SI 0 "register_operand"                           "=r")
	(ss_minus:SI
	  (match_operand:SI 1 "register_operand"                         " 0")
	  (ss_minus:SI
	    (mult:SI
	      (sign_extend:SI (vec_select:HI
				(match_operand:V2HI 2 "register_operand" " r")
				(parallel [(const_int 1)])))
	      (sign_extend:SI (vec_select:HI
				(match_operand:V2HI 3 "register_operand" " r")
				(parallel [(const_int 0)]))))
	    (mult:SI
	      (sign_extend:SI (vec_select:HI
				(match_dup 2)
				(parallel [(const_int 0)])))
	      (sign_extend:SI (vec_select:HI
				(match_dup 3)
				(parallel [(const_int 1)])))))))]
  "NDS32_EXT_DSP_P ()"
  "kmsxda\t%0, %2, %3"
  [(set_attr "type"     "dmac")
   (set_attr "length"   "4")])

;; smax[8|16] and umax[8|16]
(define_insn "<opcode><mode>3"
  [(set (match_operand:VQIHI 0 "register_operand"               "=r")
	(sumax:VQIHI (match_operand:VQIHI 1 "register_operand" " r")
		     (match_operand:VQIHI 2 "register_operand" " r")))]
  "NDS32_EXT_DSP_P ()"
  "<opcode><bits>\t%0, %1, %2"
  [(set_attr "type"   "dalu")
   (set_attr "length" "4")])

;; smin[8|16] and umin[8|16]
(define_insn "<opcode><mode>3"
  [(set (match_operand:VQIHI 0 "register_operand"              "=r")
	(sumin:VQIHI (match_operand:VQIHI 1 "register_operand" " r")
		     (match_operand:VQIHI 2 "register_operand" " r")))]
  "NDS32_EXT_DSP_P ()"
  "<opcode><bits>\t%0, %1, %2"
  [(set_attr "type"   "dalu")
   (set_attr "length" "4")])

(define_insn "<opcode><mode>3_bb"
  [(set (match_operand:<VELT> 0 "register_operand"                    "=r")
	(sumin_max:<VELT> (vec_select:<VELT>
			    (match_operand:VQIHI 1 "register_operand" " r")
			    (parallel [(const_int 0)]))
			  (vec_select:<VELT>
			    (match_operand:VQIHI 2 "register_operand" " r")
			    (parallel [(const_int 0)]))))]
  "NDS32_EXT_DSP_P () && !TARGET_BIG_ENDIAN"
  "<opcode><bits>\t%0, %1, %2"
  [(set_attr "type"   "dalu")
   (set_attr "length" "4")])

(define_insn_and_split "<opcode><mode>3_tt"
  [(set (match_operand:<VELT> 0 "register_operand"                    "=r")
	(sumin_max:<VELT> (vec_select:<VELT>
			    (match_operand:VQIHI 1 "register_operand" " r")
			    (parallel [(const_int 1)]))
			  (vec_select:<VELT>
			    (match_operand:VQIHI 2 "register_operand" " r")
			    (parallel [(const_int 1)]))))]
  "NDS32_EXT_DSP_P () && !reload_completed && !TARGET_BIG_ENDIAN"
  "#"
  "NDS32_EXT_DSP_P () && !reload_completed"
  [(const_int 0)]
{
  rtx tmp = gen_reg_rtx (<MODE>mode);
  emit_insn (gen_<opcode><mode>3 (tmp, operands[1], operands[2]));
  emit_insn (gen_rotr<mode>_1 (tmp, tmp));
  emit_move_insn (operands[0], simplify_gen_subreg (<VELT>mode, tmp, <MODE>mode, 0));
  DONE;
}
  [(set_attr "type"   "dalu")
   (set_attr "length" "4")])

(define_insn_and_split "<opcode>v4qi3_22"
  [(set (match_operand:QI 0 "register_operand"                   "=r")
	(sumin_max:QI (vec_select:QI
			(match_operand:V4QI 1 "register_operand" " r")
			(parallel [(const_int 2)]))
		      (vec_select:QI
			(match_operand:V4QI 2 "register_operand" " r")
			(parallel [(const_int 2)]))))]
  "NDS32_EXT_DSP_P () && !reload_completed && !TARGET_BIG_ENDIAN"
  "#"
  "NDS32_EXT_DSP_P () && !reload_completed"
  [(const_int 0)]
{
  rtx tmp = gen_reg_rtx (V4QImode);
  emit_insn (gen_<opcode>v4qi3 (tmp, operands[1], operands[2]));
  emit_insn (gen_rotrv4qi_2 (tmp, tmp));
  emit_move_insn (operands[0], simplify_gen_subreg (QImode, tmp, V4QImode, 0));
  DONE;
}
  [(set_attr "type"   "dalu")
   (set_attr "length" "4")])

(define_insn_and_split "<opcode>v4qi3_33"
  [(set (match_operand:QI 0 "register_operand"                   "=r")
	(sumin_max:QI (vec_select:QI
			(match_operand:V4QI 1 "register_operand" " r")
			(parallel [(const_int 3)]))
		      (vec_select:QI
			(match_operand:V4QI 2 "register_operand" " r")
			(parallel [(const_int 3)]))))]
  "NDS32_EXT_DSP_P () && !reload_completed && !TARGET_BIG_ENDIAN"
  "#"
  "NDS32_EXT_DSP_P () && !reload_completed"
  [(const_int 0)]
{
  rtx tmp = gen_reg_rtx (V4QImode);
  emit_insn (gen_<opcode>v4qi3 (tmp, operands[1], operands[2]));
  emit_insn (gen_rotrv4qi_3 (tmp, tmp));
  emit_move_insn (operands[0], simplify_gen_subreg (QImode, tmp, V4QImode, 0));
  DONE;
}
  [(set_attr "type"   "dalu")
   (set_attr "length" "4")])

(define_insn_and_split "<opcode>v2hi3_bbtt"
  [(set (match_operand:V2HI 0 "register_operand"                         "=r")
	(vec_merge:V2HI
	  (vec_duplicate:V2HI
	    (sumin_max:HI (vec_select:HI
			    (match_operand:V2HI 1 "register_operand" " r")
			    (parallel [(const_int 1)]))
			  (vec_select:HI
			    (match_operand:V2HI 2 "register_operand" " r")
			    (parallel [(const_int 1)]))))
	  (vec_duplicate:V2HI
	    (sumin_max:HI (vec_select:HI
			    (match_dup:V2HI 1)
			    (parallel [(const_int 0)]))
			  (vec_select:HI
			    (match_dup:V2HI 2)
			    (parallel [(const_int 0)]))))
	  (const_int 2)))]
  "NDS32_EXT_DSP_P () && !TARGET_BIG_ENDIAN"
  "#"
  "NDS32_EXT_DSP_P ()"
  [(const_int 0)]
{
  emit_insn (gen_<opcode>v2hi3 (operands[0], operands[1], operands[2]));
  DONE;
}
  [(set_attr "type"   "dalu")
   (set_attr "length" "4")])

(define_expand "abs<mode>2"
  [(set (match_operand:VQIHI 0 "register_operand"                "=r")
	(ss_abs:VQIHI (match_operand:VQIHI 1 "register_operand" " r")))]
  "NDS32_EXT_DSP_P () && TARGET_HW_ABS && !flag_wrapv"
{
})

(define_insn "kabs<mode>2"
  [(set (match_operand:VQIHI 0 "register_operand"                "=r")
	(ss_abs:VQIHI (match_operand:VQIHI 1 "register_operand" " r")))]
  "NDS32_EXT_DSP_P ()"
  "kabs<bits>\t%0, %1"
  [(set_attr "type"   "dalu")
   (set_attr "length" "4")])

(define_insn "<su>mar64_1"
  [(set (match_operand:DI 0 "register_operand"       "=r")
	(plus:DI
	  (match_operand:DI 1 "register_operand"     " 0")
	  (mult:DI
	    (extend:DI
	      (match_operand:SI 2 "register_operand" " r"))
	    (extend:DI
	      (match_operand:SI 3 "register_operand" " r")))))]
  "NDS32_EXT_DSP_P ()"
  "<su>mar64\t%0, %2, %3"
  [(set_attr "type"     "dmac")
   (set_attr "length"   "4")])

(define_insn "<su>mar64_2"
  [(set (match_operand:DI 0 "register_operand"       "=r")
	(plus:DI
	  (mult:DI
	    (extend:DI
	      (match_operand:SI 2 "register_operand" " r"))
	    (extend:DI
	      (match_operand:SI 3 "register_operand" " r")))
	  (match_operand:DI 1 "register_operand"     " 0")))]
  "NDS32_EXT_DSP_P ()"
  "<su>mar64\t%0, %2, %3"
  [(set_attr "type"     "dmac")
   (set_attr "length"   "4")])

(define_insn "<su>mar64_3"
  [(set (match_operand:DI 0 "register_operand"       "=r")
	(plus:DI
	  (match_operand:DI 1 "register_operand"     " 0")
	  (extend:DI
	    (mult:SI
	      (match_operand:SI 2 "register_operand" " r")
	      (match_operand:SI 3 "register_operand" " r")))))]
  "NDS32_EXT_DSP_P ()"
  "<su>mar64\t%0, %2, %3"
  [(set_attr "type"     "dmac")
   (set_attr "length"   "4")])

(define_insn "<su>mar64_4"
  [(set (match_operand:DI 0 "register_operand"       "=r")
	(plus:DI
	  (extend:DI
	  (mult:SI
	      (match_operand:SI 2 "register_operand" " r")
	      (match_operand:SI 3 "register_operand" " r")))
	  (match_operand:DI 1 "register_operand"     " 0")))]
  "NDS32_EXT_DSP_P ()"
  "<su>mar64\t%0, %2, %3"
  [(set_attr "type"     "dmac")
   (set_attr "length"   "4")])

(define_insn "<su>msr64"
  [(set (match_operand:DI 0 "register_operand"       "=r")
	(minus:DI
	  (match_operand:DI 1 "register_operand"     " 0")
	  (mult:DI
	    (extend:DI
	      (match_operand:SI 2 "register_operand" " r"))
	    (extend:DI
	      (match_operand:SI 3 "register_operand" " r")))))]
  "NDS32_EXT_DSP_P ()"
  "<su>msr64\t%0, %2, %3"
  [(set_attr "type"     "dmac")
   (set_attr "length"   "4")])

(define_insn "<su>msr64_2"
  [(set (match_operand:DI 0 "register_operand"       "=r")
	(minus:DI
	  (match_operand:DI 1 "register_operand"     " 0")
	  (extend:DI
	    (mult:SI
	      (match_operand:SI 2 "register_operand" " r")
	      (match_operand:SI 3 "register_operand" " r")))))]
  "NDS32_EXT_DSP_P ()"
  "<su>msr64\t%0, %2, %3"
  [(set_attr "type"     "dmac")
   (set_attr "length"   "4")])

;; kmar64, kmsr64, ukmar64 and ukmsr64
(define_insn "kmar64_1"
  [(set (match_operand:DI 0 "register_operand"       "=r")
	(ss_plus:DI
	  (match_operand:DI 1 "register_operand"     " 0")
	  (mult:DI
	    (sign_extend:DI
	      (match_operand:SI 2 "register_operand" " r"))
	    (sign_extend:DI
	      (match_operand:SI 3 "register_operand" " r")))))]
  "NDS32_EXT_DSP_P ()"
  "kmar64\t%0, %2, %3"
  [(set_attr "type"     "dmac")
   (set_attr "length"   "4")])

(define_insn "kmar64_2"
  [(set (match_operand:DI 0 "register_operand"       "=r")
	(ss_plus:DI
	  (mult:DI
	    (sign_extend:DI
	      (match_operand:SI 2 "register_operand" " r"))
	    (sign_extend:DI
	      (match_operand:SI 3 "register_operand" " r")))
	  (match_operand:DI 1 "register_operand"     " 0")))]
  "NDS32_EXT_DSP_P ()"
  "kmar64\t%0, %2, %3"
  [(set_attr "type"     "dmac")
   (set_attr "length"   "4")])

(define_insn "kmsr64"
  [(set (match_operand:DI 0 "register_operand"       "=r")
	(ss_minus:DI
	  (match_operand:DI 1 "register_operand"     " 0")
	  (mult:DI
	    (sign_extend:DI
	      (match_operand:SI 2 "register_operand" " r"))
	    (sign_extend:DI
	      (match_operand:SI 3 "register_operand" " r")))))]
  "NDS32_EXT_DSP_P ()"
  "kmsr64\t%0, %2, %3"
  [(set_attr "type"     "dmac")
   (set_attr "length"   "4")])

(define_insn "ukmar64_1"
  [(set (match_operand:DI 0 "register_operand"       "=r")
	(us_plus:DI
	  (match_operand:DI 1 "register_operand"     " 0")
	  (mult:DI
	    (zero_extend:DI
	      (match_operand:SI 2 "register_operand" " r"))
	    (zero_extend:DI
	      (match_operand:SI 3 "register_operand" " r")))))]
  "NDS32_EXT_DSP_P ()"
  "ukmar64\t%0, %2, %3"
  [(set_attr "type"     "dmac")
   (set_attr "length"   "4")])

(define_insn "ukmar64_2"
  [(set (match_operand:DI 0 "register_operand"       "=r")
	(us_plus:DI
	  (mult:DI
	    (zero_extend:DI
	      (match_operand:SI 2 "register_operand" " r"))
	    (zero_extend:DI
	      (match_operand:SI 3 "register_operand" " r")))
	  (match_operand:DI 1 "register_operand"     " 0")))]
  "NDS32_EXT_DSP_P ()"
  "ukmar64\t%0, %2, %3"
  [(set_attr "type"     "dmac")
   (set_attr "length"   "4")])

(define_insn "ukmsr64"
  [(set (match_operand:DI 0 "register_operand"       "=r")
	(us_minus:DI
	  (match_operand:DI 1 "register_operand"     " 0")
	  (mult:DI
	    (zero_extend:DI
	      (match_operand:SI 2 "register_operand" " r"))
	    (zero_extend:DI
	      (match_operand:SI 3 "register_operand" " r")))))]
  "NDS32_EXT_DSP_P ()"
  "ukmsr64\t%0, %2, %3"
  [(set_attr "type"     "dmac")
   (set_attr "length"   "4")])

(define_insn "bpick1"
  [(set (match_operand:SI 0 "register_operand"       "=r")
	  (ior:SI
	    (and:SI
	      (match_operand:SI 1 "register_operand" " r")
	      (match_operand:SI 3 "register_operand" " r"))
	    (and:SI
	      (match_operand:SI 2 "register_operand" " r")
	      (not:SI (match_dup 3)))))]
  "NDS32_EXT_DSP_P ()"
  "bpick\t%0, %1, %2, %3"
  [(set_attr "type"     "dbpick")
   (set_attr "length"   "4")])

(define_insn "bpick2"
  [(set (match_operand:SI 0 "register_operand"       "=r")
	  (ior:SI
	    (and:SI
	      (match_operand:SI 1 "register_operand" " r")
	      (match_operand:SI 2 "register_operand" " r"))
	    (and:SI
	      (not:SI (match_dup 2))
	      (match_operand:SI 3 "register_operand" " r"))))]
  "NDS32_EXT_DSP_P ()"
  "bpick\t%0, %1, %3, %2"
  [(set_attr "type"     "dbpick")
   (set_attr "length"   "4")])

(define_insn "bpick3"
  [(set (match_operand:SI 0 "register_operand"       "=r")
	  (ior:SI
	    (and:SI
	      (match_operand:SI 1 "register_operand" " r")
	      (match_operand:SI 2 "register_operand" " r"))
	    (and:SI
	      (match_operand:SI 3 "register_operand" " r")
	      (not:SI (match_dup 1)))))]
  "NDS32_EXT_DSP_P ()"
  "bpick\t%0, %2, %3, %1"
  [(set_attr "type"     "dbpick")
   (set_attr "length"   "4")])

(define_insn "bpick4"
  [(set (match_operand:SI 0 "register_operand"       "=r")
	  (ior:SI
	    (and:SI
	      (match_operand:SI 1 "register_operand" " r")
	      (match_operand:SI 2 "register_operand" " r"))
	    (and:SI
	      (not:SI (match_dup 1))
	      (match_operand:SI 3 "register_operand" " r"))))]
  "NDS32_EXT_DSP_P ()"
  "bpick\t%0, %2, %3, %1"
  [(set_attr "type"     "dbpick")
   (set_attr "length"   "4")])

(define_insn "bpick5"
  [(set (match_operand:SI 0 "register_operand"               "=r")
	  (ior:SI
	    (and:SI
	      (match_operand:SI 1 "register_operand"         " r")
	      (not:SI (match_operand:SI 2 "register_operand" " r")))
	    (and:SI
	      (match_operand:SI 3 "register_operand"         " r")
	      (match_dup 2))))]
  "NDS32_EXT_DSP_P ()"
  "bpick\t%0, %3, %1, %2"
  [(set_attr "type"     "dbpick")
   (set_attr "length"   "4")])

(define_insn "bpick6"
  [(set (match_operand:SI 0 "register_operand"               "=r")
	  (ior:SI
	    (and:SI
	      (not:SI (match_operand:SI 1 "register_operand" " r"))
	      (match_operand:SI 2 "register_operand"         " r"))
	    (and:SI
	      (match_operand:SI 3 "register_operand" " r")
	      (match_dup 1))))]
  "NDS32_EXT_DSP_P ()"
  "bpick\t%0, %3, %2, %1"
  [(set_attr "type"     "dbpick")
   (set_attr "length"   "4")])

(define_insn "bpick7"
  [(set (match_operand:SI 0 "register_operand"               "=r")
	  (ior:SI
	    (and:SI
	      (match_operand:SI 1 "register_operand"         " r")
	      (not:SI (match_operand:SI 2 "register_operand" " r")))
	    (and:SI
	      (match_dup 2)
	      (match_operand:SI 3 "register_operand"         " r"))))]
  "NDS32_EXT_DSP_P ()"
  "bpick\t%0, %3, %1, %2"
  [(set_attr "type"     "dbpick")
   (set_attr "length"   "4")])

(define_insn "bpick8"
  [(set (match_operand:SI 0 "register_operand"               "=r")
	  (ior:SI
	    (and:SI
	      (not:SI (match_operand:SI 1 "register_operand" " r"))
	      (match_operand:SI 2 "register_operand"         " r"))
	    (and:SI
	      (match_dup 1)
	      (match_operand:SI 3 "register_operand"         " r"))))]
  "NDS32_EXT_DSP_P ()"
  "bpick\t%0, %3, %2, %1"
  [(set_attr "type"     "dbpick")
   (set_attr "length"   "4")])

(define_insn "sraiu"
  [(set (match_operand:SI 0 "register_operand"                              "=   r, r")
	(unspec:SI [(ashiftrt:SI (match_operand:SI 1 "register_operand"     "    r, r")
				 (match_operand:SI 2 "nds32_rimm5u_operand" " Iu05, r"))]
		    UNSPEC_ROUND))]
  "NDS32_EXT_DSP_P ()"
  "@
   srai.u\t%0, %1, %2
   sra.u\t%0, %1, %2"
  [(set_attr "type"   "daluround")
   (set_attr "length" "4")])

(define_insn "kssl"
  [(set (match_operand:SI 0 "register_operand"                   "=   r, r")
	(ss_ashift:SI (match_operand:SI 1 "register_operand"     "    r, r")
		      (match_operand:SI 2 "nds32_rimm5u_operand" " Iu05, r")))]
  "NDS32_EXT_DSP_P ()"
  "@
   kslli\t%0, %1, %2
   ksll\t%0, %1, %2"
  [(set_attr "type"   "dalu")
   (set_attr "length" "4")])

(define_insn "kslraw_round"
  [(set (match_operand:SI 0 "register_operand"                  "=r")
	(if_then_else:SI
	  (lt:SI (match_operand:SI 2 "register_operand"        " r")
		 (const_int 0))
	  (unspec:SI [(ashiftrt:SI (match_operand:SI 1 "register_operand" " r")
				   (neg:SI (match_dup 2)))]
		     UNSPEC_ROUND)
	  (ss_ashift:SI (match_dup 1)
			(match_dup 2))))]
  "NDS32_EXT_DSP_P ()"
  "kslraw.u\t%0, %1, %2"
  [(set_attr "type"    "daluround")
   (set_attr "length"  "4")])

(define_insn_and_split "<shift>di3"
  [(set (match_operand:DI 0 "register_operand" "")
	(shift_rotate:DI (match_operand:DI 1 "register_operand" "")
			 (match_operand:SI 2 "nds32_rimm6u_operand" "")))]
  "NDS32_EXT_DSP_P () && !reload_completed"
  "#"
  "NDS32_EXT_DSP_P () && !reload_completed"
  [(const_int 0)]
{
  if (REGNO (operands[0]) == REGNO (operands[1]))
    {
      rtx tmp = gen_reg_rtx (DImode);
      nds32_split_<code>di3 (tmp, operands[1], operands[2]);
      emit_move_insn (operands[0], tmp);
    }
  else
    nds32_split_<code>di3 (operands[0], operands[1], operands[2]);
  DONE;
})

(define_insn "sclip32"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI [(match_operand:SI 1 "register_operand" "r")
		    (match_operand:SI 2 "immediate_operand" "i")] UNSPEC_CLIPS_OV))]
  "NDS32_EXT_DSP_P ()"
  "sclip32\t%0, %1, %2"
  [(set_attr "type"   "dclip")
   (set_attr "length" "4")]
)

(define_insn "uclip32"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI [(match_operand:SI 1 "register_operand" "r")
		    (match_operand:SI 2 "immediate_operand" "i")] UNSPEC_CLIP_OV))]
  "NDS32_EXT_DSP_P ()"
  "uclip32\t%0, %1, %2"
  [(set_attr "type"   "dclip")
   (set_attr "length" "4")]
)

(define_insn "bitrev"
  [(set (match_operand:SI 0 "register_operand"                 "=r,    r")
	(unspec:SI [(match_operand:SI 1 "register_operand"     " r,    r")
		    (match_operand:SI 2 "nds32_rimm5u_operand" " r, Iu05")]
		   UNSPEC_BITREV))]
  ""
  "@
   bitrev\t%0, %1, %2
   bitrevi\t%0, %1, %2"
  [(set_attr "type"   "dalu")
   (set_attr "length" "4")]
)

;; wext, wexti
(define_insn "<su>wext"
  [(set (match_operand:SI 0 "register_operand"                "=r,   r")
	(truncate:SI
	  (shiftrt:DI
	    (match_operand:DI 1 "register_operand"            " r,   r")
	    (match_operand:SI 2 "nds32_rimm5u_operand"        " r,Iu05"))))]
  "NDS32_EXT_DSP_P ()"
  "@
   wext\t%0, %1, %2
   wexti\t%0, %1, %2"
  [(set_attr "type"     "dwext")
   (set_attr "length"   "4")])

;; 32-bit add/sub instruction: raddw and rsubw.
(define_insn "r<opcode>si3"
  [(set (match_operand:SI 0 "register_operand"                       "=r")
	(truncate:SI
	  (ashiftrt:DI
	    (plus_minus:DI
	      (sign_extend:DI (match_operand:SI 1 "register_operand" " r"))
	      (sign_extend:DI (match_operand:SI 2 "register_operand" " r")))
	    (const_int 1))))]
  "NDS32_EXT_DSP_P ()"
  "r<opcode>w\t%0, %1, %2"
  [(set_attr "type"    "dalu")
   (set_attr "length"  "4")])

;; 32-bit add/sub instruction: uraddw and ursubw.
(define_insn "ur<opcode>si3"
  [(set (match_operand:SI 0 "register_operand"                       "=r")
	(truncate:SI
	  (lshiftrt:DI
	    (plus_minus:DI
	      (zero_extend:DI (match_operand:SI 1 "register_operand" " r"))
	      (zero_extend:DI (match_operand:SI 2 "register_operand" " r")))
	    (const_int 1))))]
  "NDS32_EXT_DSP_P ()"
  "ur<opcode>w\t%0, %1, %2"
  [(set_attr "type"    "dalu")
   (set_attr "length"  "4")])
