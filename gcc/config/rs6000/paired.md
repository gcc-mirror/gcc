;; PowerPC paired single and double hummer description
;; Copyright (C) 2007-2016 Free Software Foundation, Inc.
;; Contributed by David Edelsohn <edelsohn@gnu.org> and Revital Eres
;; <eres@il.ibm.com>

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.

;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

(define_c_enum "unspec"
  [UNSPEC_INTERHI_V2SF
   UNSPEC_INTERLO_V2SF
   UNSPEC_EXTEVEN_V2SF
   UNSPEC_EXTODD_V2SF
  ])

(define_insn "paired_negv2sf2"
  [(set (match_operand:V2SF 0 "gpc_reg_operand" "=f")
	(neg:V2SF (match_operand:V2SF 1 "gpc_reg_operand" "f")))]
  "TARGET_PAIRED_FLOAT"
  "ps_neg %0,%1"
  [(set_attr "type" "fp")])

(define_insn "sqrtv2sf2"
  [(set (match_operand:V2SF 0 "gpc_reg_operand" "=f")
	(sqrt:V2SF (match_operand:V2SF 1 "gpc_reg_operand" "f")))]
  "TARGET_PAIRED_FLOAT"
  "ps_rsqrte %0,%1"
  [(set_attr "type" "fp")])

(define_insn "paired_absv2sf2"
  [(set (match_operand:V2SF 0 "gpc_reg_operand" "=f")
	(abs:V2SF (match_operand:V2SF 1 "gpc_reg_operand" "f")))]
  "TARGET_PAIRED_FLOAT"
  "ps_abs %0,%1"
  [(set_attr "type" "fp")])

(define_insn "nabsv2sf2"
  [(set (match_operand:V2SF 0 "gpc_reg_operand" "=f")
	(neg:V2SF (abs:V2SF (match_operand:V2SF 1 "gpc_reg_operand" "f"))))]
  "TARGET_PAIRED_FLOAT"
  "ps_nabs %0,%1"
  [(set_attr "type" "fp")])

(define_insn "paired_addv2sf3"
  [(set (match_operand:V2SF 0 "gpc_reg_operand" "=f")
	(plus:V2SF (match_operand:V2SF 1 "gpc_reg_operand" "%f")
		   (match_operand:V2SF 2 "gpc_reg_operand" "f")))]
  "TARGET_PAIRED_FLOAT"
  "ps_add %0,%1,%2"
  [(set_attr "type" "fp")])

(define_insn "paired_subv2sf3"
  [(set (match_operand:V2SF 0 "gpc_reg_operand" "=f")
        (minus:V2SF (match_operand:V2SF 1 "gpc_reg_operand" "f")
                    (match_operand:V2SF 2 "gpc_reg_operand" "f")))]
  "TARGET_PAIRED_FLOAT"
  "ps_sub %0,%1,%2"
  [(set_attr "type" "fp")])

(define_insn "paired_mulv2sf3"
  [(set (match_operand:V2SF 0 "gpc_reg_operand" "=f")
	(mult:V2SF (match_operand:V2SF 1 "gpc_reg_operand" "%f")
		   (match_operand:V2SF 2 "gpc_reg_operand" "f")))]
  "TARGET_PAIRED_FLOAT"
  "ps_mul %0,%1,%2"
  [(set_attr "type" "fp")])

(define_insn "resv2sf2"
  [(set (match_operand:V2SF 0 "gpc_reg_operand" "=f")
	(unspec:V2SF [(match_operand:V2SF 1 "gpc_reg_operand" "f")] UNSPEC_FRES))]
  "TARGET_PAIRED_FLOAT && flag_finite_math_only"
  "ps_res %0,%1"
  [(set_attr "type" "fp")])

(define_insn "paired_divv2sf3"
  [(set (match_operand:V2SF 0 "gpc_reg_operand" "=f")
	(div:V2SF (match_operand:V2SF 1 "gpc_reg_operand" "f")
		  (match_operand:V2SF 2 "gpc_reg_operand" "f")))]
  "TARGET_PAIRED_FLOAT"
  "ps_div %0,%1,%2"
  [(set_attr "type" "sdiv")])

(define_insn "paired_madds0"
 [(set (match_operand:V2SF 0 "gpc_reg_operand" "=f")
       (vec_concat:V2SF
	 (fma:SF
           (vec_select:SF (match_operand:V2SF 1 "gpc_reg_operand" "f")
			  (parallel [(const_int 0)]))
	   (vec_select:SF (match_operand:V2SF 2 "gpc_reg_operand" "f")
                          (parallel [(const_int 0)]))
	   (vec_select:SF (match_operand:V2SF 3 "gpc_reg_operand" "f")
                          (parallel [(const_int 0)])))
	 (fma:SF
	   (vec_select:SF (match_dup 1)
                          (parallel [(const_int 1)]))
	   (vec_select:SF (match_dup 2)
                          (parallel [(const_int 0)]))
	   (vec_select:SF (match_dup 3)
                          (parallel [(const_int 1)])))))]
  "TARGET_PAIRED_FLOAT"
  "ps_madds0 %0,%1,%2,%3"
  [(set_attr "type" "fp")])

(define_insn "paired_madds1"
 [(set (match_operand:V2SF 0 "gpc_reg_operand" "=f")
       (vec_concat:V2SF
         (fma:SF
	   (vec_select:SF (match_operand:V2SF 1 "gpc_reg_operand" "f")
                          (parallel [(const_int 0)]))
           (vec_select:SF (match_operand:V2SF 2 "gpc_reg_operand" "f")
                          (parallel [(const_int 1)]))
           (vec_select:SF (match_operand:V2SF 3 "gpc_reg_operand" "f")
                          (parallel [(const_int 0)])))
	 (fma:SF
	   (vec_select:SF (match_dup 1)
                          (parallel [(const_int 1)]))
           (vec_select:SF (match_dup 2)
                          (parallel [(const_int 1)]))
           (vec_select:SF (match_dup 3)
                          (parallel [(const_int 1)])))))]
  "TARGET_PAIRED_FLOAT"
  "ps_madds1 %0,%1,%2,%3"
  [(set_attr "type" "fp")])

(define_insn "*paired_madd"
  [(set (match_operand:V2SF 0 "gpc_reg_operand" "=f")
	(fma:V2SF
	  (match_operand:V2SF 1 "gpc_reg_operand" "f")
	  (match_operand:V2SF 2 "gpc_reg_operand" "f")
	  (match_operand:V2SF 3 "gpc_reg_operand" "f")))]
  "TARGET_PAIRED_FLOAT"
  "ps_madd %0,%1,%2,%3"
  [(set_attr "type" "fp")]) 

(define_insn "*paired_msub"
  [(set (match_operand:V2SF 0 "gpc_reg_operand" "=f")
	(fma:V2SF
	  (match_operand:V2SF 1 "gpc_reg_operand" "f")
	  (match_operand:V2SF 2 "gpc_reg_operand" "f")
	  (neg:V2SF (match_operand:V2SF 3 "gpc_reg_operand" "f"))))]
  "TARGET_PAIRED_FLOAT"
  "ps_msub %0,%1,%2,%3"
  [(set_attr "type" "fp")])

(define_insn "*paired_nmadd"
  [(set (match_operand:V2SF 0 "gpc_reg_operand" "=f")
	(neg:V2SF
	  (fma:V2SF
	    (match_operand:V2SF 1 "gpc_reg_operand" "f")
	    (match_operand:V2SF 2 "gpc_reg_operand" "f")
	    (match_operand:V2SF 3 "gpc_reg_operand" "f"))))]
  "TARGET_PAIRED_FLOAT"
  "ps_nmadd %0,%1,%2,%3"
  [(set_attr "type" "fp")])

(define_insn "*paired_nmsub"
  [(set (match_operand:V2SF 0 "gpc_reg_operand" "=f")
	(neg:V2SF
	  (fma:V2SF
	    (match_operand:V2SF 1 "gpc_reg_operand" "f")
	    (match_operand:V2SF 2 "gpc_reg_operand" "f")
	    (neg:V2SF (match_operand:V2SF 3 "gpc_reg_operand" "f")))))]
  "TARGET_PAIRED_FLOAT"
  "ps_nmsub %0,%1,%2,%3"
  [(set_attr "type" "dmul")])

(define_insn "selv2sf4"
  [(set (match_operand:V2SF 0 "gpc_reg_operand" "=f")
	(vec_concat:V2SF
	 (if_then_else:SF (ge (vec_select:SF (match_operand:V2SF 1 "gpc_reg_operand" "f")
					     (parallel [(const_int 0)]))
			      (match_operand:SF 4 "zero_fp_constant" "F"))
			  (vec_select:SF (match_operand:V2SF 2 "gpc_reg_operand" "f")
					 (parallel [(const_int 0)]))
			  (vec_select:SF (match_operand:V2SF 3 "gpc_reg_operand" "f")
					 (parallel [(const_int 0)])))
	 (if_then_else:SF (ge (vec_select:SF (match_dup 1)
					     (parallel [(const_int 1)]))
			      (match_dup 4))
			  (vec_select:SF (match_dup 2)
					 (parallel [(const_int 1)]))
			  (vec_select:SF (match_dup 3)
					 (parallel [(const_int 1)])))))]

  "TARGET_PAIRED_FLOAT"
  "ps_sel %0,%1,%2,%3"
  [(set_attr "type" "fp")])

(define_insn "*movv2sf_paired"
  [(set (match_operand:V2SF 0 "nonimmediate_operand" "=Z,f,f,Y,r,r,f")
		 (match_operand:V2SF 1 "input_operand" "f,Z,f,r,Y,r,W"))]
  "TARGET_PAIRED_FLOAT
   && (register_operand (operands[0], V2SFmode) 
       || register_operand (operands[1], V2SFmode))"
{
  switch (which_alternative)
    {
    case 0: return "psq_stx %1,%y0,0,0";
    case 1: return "psq_lx %0,%y1,0,0";
    case 2: return "ps_mr %0,%1";
    case 3: return "#";
    case 4: return "#";
    case 5: return "#";
    case 6: return "#"; 
    default: gcc_unreachable ();
    }
}
  [(set_attr "type" "fpstore,fpload,fp,*,*,*,*")])

(define_insn "paired_stx"
  [(set (match_operand:V2SF 0 "memory_operand" "=Z")
        (match_operand:V2SF 1 "gpc_reg_operand" "f"))]
  "TARGET_PAIRED_FLOAT"
  "psq_stx %1,%y0,0,0"
  [(set_attr "type" "fpstore")])

(define_insn "paired_lx"
  [(set (match_operand:V2SF 0 "gpc_reg_operand" "=f")
        (match_operand:V2SF 1 "memory_operand" "Z"))]
  "TARGET_PAIRED_FLOAT"
  "psq_lx %0,%y1,0,0"
  [(set_attr "type" "fpload")])


(define_split
  [(set (match_operand:V2SF 0 "nonimmediate_operand" "")
        (match_operand:V2SF 1 "input_operand" ""))]
  "TARGET_PAIRED_FLOAT && reload_completed
   && gpr_or_gpr_p (operands[0], operands[1])"
  [(pc)]
  {
  rs6000_split_multireg_move (operands[0], operands[1]); DONE;
  })

(define_insn "paired_cmpu0"
  [(set (match_operand:CCFP 0 "cc_reg_operand" "=y")
	(compare:CCFP (vec_select:SF
		       (match_operand:V2SF 1 "gpc_reg_operand" "f")
		       (parallel [(const_int 0)]))
		      (vec_select:SF
		       (match_operand:V2SF 2 "gpc_reg_operand" "f")
		       (parallel [(const_int 0)]))))]
  "TARGET_PAIRED_FLOAT"
  "ps_cmpu0 %0,%1,%2"
  [(set_attr "type" "fpcompare")])

(define_insn "paired_cmpu1"
  [(set (match_operand:CCFP 0 "cc_reg_operand" "=y")
	(compare:CCFP (vec_select:SF
		       (match_operand:V2SF 1 "gpc_reg_operand" "f")
		       (parallel [(const_int 1)]))
		      (vec_select:SF
		       (match_operand:V2SF 2 "gpc_reg_operand" "f")
		       (parallel [(const_int 1)]))))]
  "TARGET_PAIRED_FLOAT"
  "ps_cmpu1 %0,%1,%2"
  [(set_attr "type" "fpcompare")])

(define_insn "paired_merge00"
  [(set (match_operand:V2SF 0 "gpc_reg_operand" "=f")
	(vec_select:V2SF
	  (vec_concat:V4SF
	    (match_operand:V2SF 1 "gpc_reg_operand" "f")
	    (match_operand:V2SF 2 "gpc_reg_operand" "f"))
	  (parallel [(const_int 0) (const_int 2)])))]
  "TARGET_PAIRED_FLOAT"
  "ps_merge00 %0, %1, %2"
  [(set_attr "type" "fp")])

(define_insn "paired_merge01"
  [(set (match_operand:V2SF 0 "gpc_reg_operand" "=f")
	(vec_select:V2SF
	  (vec_concat:V4SF
	    (match_operand:V2SF 1 "gpc_reg_operand" "f")
	    (match_operand:V2SF 2 "gpc_reg_operand" "f"))
	  (parallel [(const_int 0) (const_int 3)])))]
  "TARGET_PAIRED_FLOAT"
  "ps_merge01 %0, %1, %2"
  [(set_attr "type" "fp")])

(define_insn "paired_merge10"
  [(set (match_operand:V2SF 0 "gpc_reg_operand" "=f")
	(vec_select:V2SF
	  (vec_concat:V4SF
	    (match_operand:V2SF 1 "gpc_reg_operand" "f")
	    (match_operand:V2SF 2 "gpc_reg_operand" "f"))
	  (parallel [(const_int 1) (const_int 2)])))]
  "TARGET_PAIRED_FLOAT"
  "ps_merge10 %0, %1, %2"
  [(set_attr "type" "fp")])

(define_insn "paired_merge11"
  [(set (match_operand:V2SF 0 "gpc_reg_operand" "=f")
	(vec_select:V2SF
	  (vec_concat:V4SF
	    (match_operand:V2SF 1 "gpc_reg_operand" "f")
	    (match_operand:V2SF 2 "gpc_reg_operand" "f"))
	  (parallel [(const_int 1) (const_int 3)])))]
  "TARGET_PAIRED_FLOAT"
  "ps_merge11 %0, %1, %2"
  [(set_attr "type" "fp")])

(define_expand "vec_perm_constv2sf"
  [(match_operand:V2SF 0 "gpc_reg_operand" "")
   (match_operand:V2SF 1 "gpc_reg_operand" "")
   (match_operand:V2SF 2 "gpc_reg_operand" "")
   (match_operand:V2SI 3 "" "")]
  "TARGET_PAIRED_FLOAT"
{
  if (rs6000_expand_vec_perm_const (operands))
    DONE;
  else
    FAIL;
})

(define_insn "paired_sum0"
  [(set (match_operand:V2SF 0 "gpc_reg_operand" "=f")
	(vec_concat:V2SF (plus:SF (vec_select:SF
				   (match_operand:V2SF 1 "gpc_reg_operand" "f")
				   (parallel [(const_int 0)]))
				  (vec_select:SF
				   (match_operand:V2SF 2 "gpc_reg_operand" "f")
				   (parallel [(const_int 1)])))
			 (vec_select:SF
			  (match_operand:V2SF 3 "gpc_reg_operand" "f")
			  (parallel [(const_int 1)]))))]
  "TARGET_PAIRED_FLOAT"
  "ps_sum0 %0,%1,%2,%3"
  [(set_attr "type" "fp")])

(define_insn "paired_sum1"
  [(set (match_operand:V2SF 0 "gpc_reg_operand" "=f")
	(vec_concat:V2SF (vec_select:SF
			  (match_operand:V2SF 2 "gpc_reg_operand" "f")
			  (parallel [(const_int 1)]))
			 (plus:SF (vec_select:SF
				   (match_operand:V2SF 1 "gpc_reg_operand" "f")
				   (parallel [(const_int 0)]))
				  (vec_select:SF
				   (match_operand:V2SF 3 "gpc_reg_operand" "f")
				   (parallel [(const_int 1)])))))]
  "TARGET_PAIRED_FLOAT"
  "ps_sum1 %0,%1,%2,%3"
  [(set_attr "type" "fp")])

(define_insn "paired_muls0"
  [(set (match_operand:V2SF 0 "gpc_reg_operand" "=f")
	(mult:V2SF (match_operand:V2SF 2 "gpc_reg_operand" "f")
		   (vec_duplicate:V2SF
		    (vec_select:SF (match_operand:V2SF 1 "gpc_reg_operand" "f")
				   (parallel [(const_int 0)])))))]
  "TARGET_PAIRED_FLOAT"
  "ps_muls0 %0, %1, %2"
  [(set_attr "type" "fp")])


(define_insn "paired_muls1"
  [(set (match_operand:V2SF 0 "gpc_reg_operand" "=f")
	(mult:V2SF (match_operand:V2SF 2 "gpc_reg_operand" "f")
		   (vec_duplicate:V2SF
		    (vec_select:SF (match_operand:V2SF 1 "gpc_reg_operand" "f")
				   (parallel [(const_int 1)])))))]
  "TARGET_PAIRED_FLOAT"
  "ps_muls1 %0, %1, %2"
  [(set_attr "type" "fp")])

(define_expand "vec_initv2sf"
  [(match_operand:V2SF 0 "gpc_reg_operand" "=f")
   (match_operand 1 "" "")]
  "TARGET_PAIRED_FLOAT"
{
  paired_expand_vector_init (operands[0], operands[1]);
  DONE;
})

(define_insn "*vconcatsf"
  [(set (match_operand:V2SF 0 "gpc_reg_operand" "=f")
        (vec_concat:V2SF
         (match_operand:SF 1 "gpc_reg_operand" "f")
         (match_operand:SF 2 "gpc_reg_operand" "f")))]
  "TARGET_PAIRED_FLOAT"
  "ps_merge00 %0, %1, %2"
  [(set_attr "type" "fp")])

(define_expand "sminv2sf3"
  [(set (match_operand:V2SF 0 "gpc_reg_operand" "=f")
        (smin:V2SF (match_operand:V2SF 1 "gpc_reg_operand" "f")
                   (match_operand:V2SF 2 "gpc_reg_operand" "f")))]
  "TARGET_PAIRED_FLOAT"
{
  rtx tmp = gen_reg_rtx (V2SFmode);

  emit_insn (gen_subv2sf3 (tmp, operands[1], operands[2]));
  emit_insn (gen_selv2sf4 (operands[0], tmp, operands[2], operands[1], CONST0_RTX (SFmode)));
  DONE;
})

(define_expand "smaxv2sf3"
  [(set (match_operand:V2SF 0 "gpc_reg_operand" "=f")
        (smax:V2SF (match_operand:V2SF 1 "gpc_reg_operand" "f")
                   (match_operand:V2SF 2 "gpc_reg_operand" "f")))]
  "TARGET_PAIRED_FLOAT"
{
  rtx tmp = gen_reg_rtx (V2SFmode);

  emit_insn (gen_subv2sf3 (tmp, operands[1], operands[2]));
  emit_insn (gen_selv2sf4 (operands[0], tmp, operands[1], operands[2], CONST0_RTX (SFmode)));
  DONE;
})

(define_expand "reduc_smax_scal_v2sf"
  [(match_operand:SF 0 "gpc_reg_operand" "=f")
   (match_operand:V2SF 1 "gpc_reg_operand" "f")]
  "TARGET_PAIRED_FLOAT"
{
  rtx tmp_swap = gen_reg_rtx (V2SFmode);
  rtx tmp = gen_reg_rtx (V2SFmode);
  rtx vec_res = gen_reg_rtx (V2SFmode);
  rtx di_res = gen_reg_rtx (DImode);

  emit_insn (gen_paired_merge10 (tmp_swap, operands[1], operands[1]));
  emit_insn (gen_subv2sf3 (tmp, operands[1], tmp_swap));
  emit_insn (gen_selv2sf4 (vec_res, tmp, operands[1], tmp_swap,
			   CONST0_RTX (SFmode)));
  emit_move_insn (di_res, simplify_gen_subreg (DImode, vec_res, V2SFmode, 0));
  emit_move_insn (operands[0], simplify_gen_subreg (SFmode, di_res, DImode,
						    BYTES_BIG_ENDIAN ? 4 : 0));

  DONE;
})

(define_expand "reduc_smin_scal_v2sf"
  [(match_operand:SF 0 "gpc_reg_operand" "=f")
   (match_operand:V2SF 1 "gpc_reg_operand" "f")]
  "TARGET_PAIRED_FLOAT"
{
  rtx tmp_swap = gen_reg_rtx (V2SFmode);
  rtx tmp = gen_reg_rtx (V2SFmode);
  rtx vec_res = gen_reg_rtx (V2SFmode);
  rtx di_res = gen_reg_rtx (DImode);

  emit_insn (gen_paired_merge10 (tmp_swap, operands[1], operands[1]));
  emit_insn (gen_subv2sf3 (tmp, operands[1], tmp_swap));
  emit_insn (gen_selv2sf4 (vec_res, tmp, tmp_swap, operands[1],
			   CONST0_RTX (SFmode)));
  emit_move_insn (di_res, simplify_gen_subreg (DImode, vec_res, V2SFmode, 0));
  emit_move_insn (operands[0], simplify_gen_subreg (SFmode, di_res, DImode,
						    BYTES_BIG_ENDIAN ? 4 : 0));

  DONE;
})

(define_expand "reduc_plus_scal_v2sf"
  [(set (match_operand:SF 0 "gpc_reg_operand" "=f")
        (match_operand:V2SF 1 "gpc_reg_operand" "f"))]
  "TARGET_PAIRED_FLOAT"
{
  rtx vec_res = gen_reg_rtx (V2SFmode);
  rtx di_res = gen_reg_rtx (DImode);

  emit_insn (gen_paired_sum1 (vec_res, operands[1], operands[1], operands[1]));
  emit_move_insn (di_res, simplify_gen_subreg (DImode, vec_res, V2SFmode, 0));
  emit_move_insn (operands[0], simplify_gen_subreg (SFmode, di_res, DImode,
						    BYTES_BIG_ENDIAN ? 4 : 0));
  DONE;
})

(define_expand "movmisalignv2sf"
  [(set (match_operand:V2SF 0 "nonimmediate_operand" "")
        (match_operand:V2SF 1 "any_operand" ""))]
  "TARGET_PAIRED_FLOAT"
{
  paired_expand_vector_move (operands);
  DONE;
})

(define_expand "vcondv2sfv2sf"
  [(set (match_operand:V2SF 0 "gpc_reg_operand" "=f")
        (if_then_else:V2SF
         (match_operator 3 "gpc_reg_operand"
                         [(match_operand:V2SF 4 "gpc_reg_operand" "f")
                          (match_operand:V2SF 5 "gpc_reg_operand" "f")])
         (match_operand:V2SF 1 "gpc_reg_operand" "f")
         (match_operand:V2SF 2 "gpc_reg_operand" "f")))]
  "TARGET_PAIRED_FLOAT && flag_unsafe_math_optimizations"
{
  if (paired_emit_vector_cond_expr (operands[0], operands[1], operands[2],
                                    operands[3], operands[4], operands[5]))
    DONE;
  else
    FAIL;
})
