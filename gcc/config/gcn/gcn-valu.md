;; Copyright (C) 2016-2020 Free Software Foundation, Inc.

;; This file is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3 of the License, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;; {{{ Vector iterators

; Vector modes for specific types
; (This will make more sense when there are multiple vector sizes)
(define_mode_iterator V_QI
		      [V64QI])
(define_mode_iterator V_HI
		      [V64HI])
(define_mode_iterator V_HF
		      [V64HF])
(define_mode_iterator V_SI
		      [V64SI])
(define_mode_iterator V_SF
		      [V64SF])
(define_mode_iterator V_DI
		      [V64DI])
(define_mode_iterator V_DF
		      [V64DF])

; Vector modes for sub-dword modes
(define_mode_iterator V_QIHI
		      [V64QI V64HI])

; Vector modes for one vector register
(define_mode_iterator V_1REG
		      [V64QI V64HI V64SI V64HF V64SF])

(define_mode_iterator V_INT_1REG
		      [V64QI V64HI V64SI])
(define_mode_iterator V_INT_1REG_ALT
		      [V64QI V64HI V64SI])
(define_mode_iterator V_FP_1REG
		      [V64HF V64SF])

; Vector modes for two vector registers
(define_mode_iterator V_2REG
		      [V64DI V64DF])

; Vector modes with native support
(define_mode_iterator V_noQI
		      [V64HI V64HF V64SI V64SF V64DI V64DF])
(define_mode_iterator V_noHI
		      [V64HF V64SI V64SF V64DI V64DF])

(define_mode_iterator V_INT_noQI
		      [V64HI V64SI V64DI])

; All of above
(define_mode_iterator V_ALL
		      [V64QI V64HI V64HF V64SI V64SF V64DI V64DF])
(define_mode_iterator V_ALL_ALT
		      [V64QI V64HI V64HF V64SI V64SF V64DI V64DF])

(define_mode_iterator V_INT
		      [V64QI V64HI V64SI V64DI])
(define_mode_iterator V_FP
		      [V64HF V64SF V64DF])

(define_mode_attr scalar_mode
  [(V64QI "qi") (V64HI "hi") (V64SI "si")
   (V64HF "hf") (V64SF "sf") (V64DI "di") (V64DF "df")])

(define_mode_attr SCALAR_MODE
  [(V64QI "QI") (V64HI "HI") (V64SI "SI")
   (V64HF "HF") (V64SF "SF") (V64DI "DI") (V64DF "DF")])

(define_mode_attr vnsi
  [(V64QI "v64si") (V64HI "v64si") (V64HF "v64si") (V64SI "v64si")
   (V64SF "v64si") (V64DI "v64si") (V64DF "v64si")])

(define_mode_attr VnSI
  [(V64QI "V64SI") (V64HI "V64SI") (V64HF "V64SI") (V64SI "V64SI")
   (V64SF "V64SI") (V64DI "V64SI") (V64DF "V64SI")])

(define_mode_attr vndi
  [(V64QI "v64di") (V64HI "v64di") (V64HF "v64di") (V64SI "v64di")
   (V64SF "v64di") (V64DI "v64di") (V64DF "v64di")])

(define_mode_attr VnDI
  [(V64QI "V64DI") (V64HI "V64DI") (V64HF "V64DI") (V64SI "V64DI")
   (V64SF "V64DI") (V64DI "V64DI") (V64DF "V64DI")])

(define_mode_attr sdwa [(V64QI "BYTE_0") (V64HI "WORD_0") (V64SI "DWORD")])

;; }}}
;; {{{ Substitutions

(define_subst_attr "exec" "vec_merge"
		   "" "_exec")
(define_subst_attr "exec_clobber" "vec_merge_with_clobber"
		   "" "_exec")
(define_subst_attr "exec_vcc" "vec_merge_with_vcc"
		   "" "_exec")
(define_subst_attr "exec_scatter" "scatter_store"
		   "" "_exec")

(define_subst "vec_merge"
  [(set (match_operand:V_ALL 0)
	(match_operand:V_ALL 1))]
  ""
  [(set (match_dup 0)
	(vec_merge:V_ALL
	  (match_dup 1)
	  (match_operand:V_ALL 3 "gcn_register_or_unspec_operand" "U0")
	  (match_operand:DI 4 "gcn_exec_reg_operand" "e")))])

(define_subst "vec_merge_with_clobber"
  [(set (match_operand:V_ALL 0)
	(match_operand:V_ALL 1))
   (clobber (match_operand 2))]
  ""
  [(set (match_dup 0)
	(vec_merge:V_ALL
	  (match_dup 1)
	  (match_operand:V_ALL 3 "gcn_register_or_unspec_operand" "U0")
	  (match_operand:DI 4 "gcn_exec_reg_operand" "e")))
   (clobber (match_dup 2))])

(define_subst "vec_merge_with_vcc"
  [(set (match_operand:V_ALL 0)
	(match_operand:V_ALL 1))
   (set (match_operand:DI 2)
	(match_operand:DI 3))]
  ""
  [(parallel
     [(set (match_dup 0)
	   (vec_merge:V_ALL
	     (match_dup 1)
	     (match_operand:V_ALL 4 "gcn_register_or_unspec_operand" "U0")
	     (match_operand:DI 5 "gcn_exec_reg_operand" "e")))
      (set (match_dup 2)
	   (and:DI (match_dup 3)
		   (reg:DI EXEC_REG)))])])

(define_subst "scatter_store"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(match_operand 0)
	   (match_operand 1)
	   (match_operand 2)
	   (match_operand 3)]
	  UNSPEC_SCATTER))]
  ""
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(match_dup 0)
	   (match_dup 1)
	   (match_dup 2)
	   (match_dup 3)
	   (match_operand:DI 4 "gcn_exec_reg_operand" "e")]
	  UNSPEC_SCATTER))])

;; }}}
;; {{{ Vector moves

; This is the entry point for all vector register moves.  Memory accesses can
; come this way also, but will more usually use the reload_in/out,
; gather/scatter, maskload/store, etc.

(define_expand "mov<mode>"
  [(set (match_operand:V_ALL 0 "nonimmediate_operand")
	(match_operand:V_ALL 1 "general_operand"))]
  ""
  {
    if (MEM_P (operands[0]) && !lra_in_progress && !reload_completed)
      {
	operands[1] = force_reg (<MODE>mode, operands[1]);
	rtx scratch = gen_rtx_SCRATCH (<VnDI>mode);
	rtx a = gen_rtx_CONST_INT (VOIDmode, MEM_ADDR_SPACE (operands[0]));
	rtx v = gen_rtx_CONST_INT (VOIDmode, MEM_VOLATILE_P (operands[0]));
	rtx expr = gcn_expand_scalar_to_vector_address (<MODE>mode, NULL,
							operands[0],
							scratch);
	emit_insn (gen_scatter<mode>_expr (expr, operands[1], a, v));
	DONE;
      }
    else if (MEM_P (operands[1]) && !lra_in_progress && !reload_completed)
      {
	rtx scratch = gen_rtx_SCRATCH (<VnDI>mode);
	rtx a = gen_rtx_CONST_INT (VOIDmode, MEM_ADDR_SPACE (operands[1]));
	rtx v = gen_rtx_CONST_INT (VOIDmode, MEM_VOLATILE_P (operands[1]));
	rtx expr = gcn_expand_scalar_to_vector_address (<MODE>mode, NULL,
							operands[1],
							scratch);
	emit_insn (gen_gather<mode>_expr (operands[0], expr, a, v));
	DONE;
      }
    else if ((MEM_P (operands[0]) || MEM_P (operands[1])))
      {
        gcc_assert (!reload_completed);
	rtx scratch = gen_reg_rtx (<VnDI>mode);
	emit_insn (gen_mov<mode>_sgprbase (operands[0], operands[1], scratch));
	DONE;
      }
  })

; A pseudo instruction that helps LRA use the "U0" constraint.

(define_insn "mov<mode>_unspec"
  [(set (match_operand:V_ALL 0 "nonimmediate_operand" "=v")
	(match_operand:V_ALL 1 "gcn_unspec_operand"   " U"))]
  ""
  ""
  [(set_attr "type" "unknown")
   (set_attr "length" "0")])

(define_insn "*mov<mode>"
  [(set (match_operand:V_1REG 0 "nonimmediate_operand" "=v,v")
	(match_operand:V_1REG 1 "general_operand"      "vA,B"))]
  ""
  "v_mov_b32\t%0, %1"
  [(set_attr "type" "vop1,vop1")
   (set_attr "length" "4,8")])

(define_insn "mov<mode>_exec"
  [(set (match_operand:V_1REG 0 "nonimmediate_operand"	 "=v, v, v, v, v, m")
	(vec_merge:V_1REG
	  (match_operand:V_1REG 1 "general_operand"	 "vA, B, v,vA, m, v")
	  (match_operand:V_1REG 2 "gcn_alu_or_unspec_operand"
							 "U0,U0,vA,vA,U0,U0")
	  (match_operand:DI 3 "register_operand"	 " e, e,cV,Sv, e, e")))
   (clobber (match_scratch:<VnDI> 4			 "=X, X, X, X,&v,&v"))]
  "!MEM_P (operands[0]) || REG_P (operands[1])"
  "@
   v_mov_b32\t%0, %1
   v_mov_b32\t%0, %1
   v_cndmask_b32\t%0, %2, %1, vcc
   v_cndmask_b32\t%0, %2, %1, %3
   #
   #"
  [(set_attr "type" "vop1,vop1,vop2,vop3a,*,*")
   (set_attr "length" "4,8,4,8,16,16")])

; This variant does not accept an unspec, but does permit MEM
; read/modify/write which is necessary for maskstore.

;(define_insn "*mov<mode>_exec_match"
;  [(set (match_operand:V_1REG 0 "nonimmediate_operand" "=v,v, v, m")
;	(vec_merge:V_1REG
;	  (match_operand:V_1REG 1 "general_operand"	"vA,B, m, v")
;	  (match_dup 0)
;	  (match_operand:DI 2 "gcn_exec_reg_operand"	" e,e, e, e")))
;   (clobber (match_scratch:<VnDI> 3			"=X,X,&v,&v"))]
;  "!MEM_P (operands[0]) || REG_P (operands[1])"
;  "@
;  v_mov_b32\t%0, %1
;  v_mov_b32\t%0, %1
;  #
;  #"
;  [(set_attr "type" "vop1,vop1,*,*")
;   (set_attr "length" "4,8,16,16")])

(define_insn "*mov<mode>"
  [(set (match_operand:V_2REG 0 "nonimmediate_operand" "=v")
	(match_operand:V_2REG 1 "general_operand"      "vDB"))]
  ""
  {
    if (!REG_P (operands[1]) || REGNO (operands[0]) <= REGNO (operands[1]))
      return "v_mov_b32\t%L0, %L1\;v_mov_b32\t%H0, %H1";
    else
      return "v_mov_b32\t%H0, %H1\;v_mov_b32\t%L0, %L1";
  }
  [(set_attr "type" "vmult")
   (set_attr "length" "16")])

(define_insn "mov<mode>_exec"
  [(set (match_operand:V_2REG 0 "nonimmediate_operand" "= v,   v,   v, v, m")
	(vec_merge:V_2REG
	  (match_operand:V_2REG 1 "general_operand"    "vDB,  v0,  v0, m, v")
	  (match_operand:V_2REG 2 "gcn_alu_or_unspec_operand"
						       " U0,vDA0,vDA0,U0,U0")
	  (match_operand:DI 3 "register_operand"       "  e,  cV,  Sv, e, e")))
   (clobber (match_scratch:<VnDI> 4		       "= X,   X,   X,&v,&v"))]
  "!MEM_P (operands[0]) || REG_P (operands[1])"
  {
    if (!REG_P (operands[1]) || REGNO (operands[0]) <= REGNO (operands[1]))
      switch (which_alternative)
	{
	case 0:
	  return "v_mov_b32\t%L0, %L1\;v_mov_b32\t%H0, %H1";
	case 1:
	  return "v_cndmask_b32\t%L0, %L2, %L1, vcc\;"
		 "v_cndmask_b32\t%H0, %H2, %H1, vcc";
	case 2:
	  return "v_cndmask_b32\t%L0, %L2, %L1, %3\;"
		 "v_cndmask_b32\t%H0, %H2, %H1, %3";
	}
    else
      switch (which_alternative)
	{
	case 0:
	  return "v_mov_b32\t%H0, %H1\;v_mov_b32\t%L0, %L1";
	case 1:
	  return "v_cndmask_b32\t%H0, %H2, %H1, vcc\;"
		 "v_cndmask_b32\t%L0, %L2, %L1, vcc";
	case 2:
	  return "v_cndmask_b32\t%H0, %H2, %H1, %3\;"
		 "v_cndmask_b32\t%L0, %L2, %L1, %3";
	}

    return "#";
  }
  [(set_attr "type" "vmult,vmult,vmult,*,*")
   (set_attr "length" "16,16,16,16,16")])

; This variant does not accept an unspec, but does permit MEM
; read/modify/write which is necessary for maskstore.

;(define_insn "*mov<mode>_exec_match"
;  [(set (match_operand:V_2REG 0 "nonimmediate_operand" "=v, v, m")
;	(vec_merge:V_2REG
;	  (match_operand:V_2REG 1 "general_operand"	"vDB, m, v")
;	  (match_dup 0)
;	  (match_operand:DI 2 "gcn_exec_reg_operand"	" e, e, e")))
;   (clobber (match_scratch:<VnDI> 3			"=X,&v,&v"))]
;  "!MEM_P (operands[0]) || REG_P (operands[1])"
;  "@
;   * if (!REG_P (operands[1]) || REGNO (operands[0]) <= REGNO (operands[1])) \
;       return \"v_mov_b32\t%L0, %L1\;v_mov_b32\t%H0, %H1\"; \
;     else \
;       return \"v_mov_b32\t%H0, %H1\;v_mov_b32\t%L0, %L1\";
;   #
;   #"
;  [(set_attr "type" "vmult,*,*")
;   (set_attr "length" "16,16,16")])

; A SGPR-base load looks like:
;   <load> v, Sv
;
; There's no hardware instruction that corresponds to this, but vector base
; addresses are placed in an SGPR because it is easier to add to a vector.
; We also have a temporary vT, and the vector v1 holding numbered lanes.
;
; Rewrite as:
;   vT = v1 << log2(element-size)
;   vT += Sv
;   flat_load v, vT

(define_insn "mov<mode>_sgprbase"
  [(set (match_operand:V_1REG 0 "nonimmediate_operand" "= v, v, v, m")
	(unspec:V_1REG
	  [(match_operand:V_1REG 1 "general_operand"   " vA,vB, m, v")]
	  UNSPEC_SGPRBASE))
   (clobber (match_operand:<VnDI> 2 "register_operand"  "=&v,&v,&v,&v"))]
  "lra_in_progress || reload_completed"
  "@
   v_mov_b32\t%0, %1
   v_mov_b32\t%0, %1
   #
   #"
  [(set_attr "type" "vop1,vop1,*,*")
   (set_attr "length" "4,8,12,12")])

(define_insn "mov<mode>_sgprbase"
  [(set (match_operand:V_2REG 0 "nonimmediate_operand" "= v, v, m")
	(unspec:V_2REG
	  [(match_operand:V_2REG 1 "general_operand"   "vDB, m, v")]
	  UNSPEC_SGPRBASE))
   (clobber (match_operand:<VnDI> 2 "register_operand"  "=&v,&v,&v"))]
  "lra_in_progress || reload_completed"
  "@
   * if (!REG_P (operands[1]) || REGNO (operands[0]) <= REGNO (operands[1])) \
       return \"v_mov_b32\t%L0, %L1\;v_mov_b32\t%H0, %H1\"; \
     else \
       return \"v_mov_b32\t%H0, %H1\;v_mov_b32\t%L0, %L1\";
   #
   #"
  [(set_attr "type" "vmult,*,*")
   (set_attr "length" "8,12,12")])

; reload_in was once a standard name, but here it's only referenced by
; gcn_secondary_reload.  It allows a reload with a scratch register.

(define_expand "reload_in<mode>"
  [(set (match_operand:V_ALL 0 "register_operand"     "= v")
	(match_operand:V_ALL 1 "memory_operand"	      "  m"))
   (clobber (match_operand:<VnDI> 2 "register_operand" "=&v"))]
  ""
  {
    emit_insn (gen_mov<mode>_sgprbase (operands[0], operands[1], operands[2]));
    DONE;
  })

; reload_out is similar to reload_in, above.

(define_expand "reload_out<mode>"
  [(set (match_operand:V_ALL 0 "memory_operand"	      "= m")
	(match_operand:V_ALL 1 "register_operand"     "  v"))
   (clobber (match_operand:<VnDI> 2 "register_operand" "=&v"))]
  ""
  {
    emit_insn (gen_mov<mode>_sgprbase (operands[0], operands[1], operands[2]));
    DONE;
  })

; Expand scalar addresses into gather/scatter patterns

(define_split
  [(set (match_operand:V_ALL 0 "memory_operand")
	(unspec:V_ALL
	  [(match_operand:V_ALL 1 "general_operand")]
	  UNSPEC_SGPRBASE))
   (clobber (match_scratch:<VnDI> 2))]
  ""
  [(set (mem:BLK (scratch))
	(unspec:BLK [(match_dup 5) (match_dup 1) (match_dup 6) (match_dup 7)]
		    UNSPEC_SCATTER))]
  {
    operands[5] = gcn_expand_scalar_to_vector_address (<MODE>mode, NULL,
						       operands[0],
						       operands[2]);
    operands[6] = gen_rtx_CONST_INT (VOIDmode, MEM_ADDR_SPACE (operands[0]));
    operands[7] = gen_rtx_CONST_INT (VOIDmode, MEM_VOLATILE_P (operands[0]));
  })

(define_split
  [(set (match_operand:V_ALL 0 "memory_operand")
	(vec_merge:V_ALL
	  (match_operand:V_ALL 1 "general_operand")
	  (match_operand:V_ALL 2 "")
	  (match_operand:DI 3 "gcn_exec_reg_operand")))
   (clobber (match_scratch:<VnDI> 4))]
  ""
  [(set (mem:BLK (scratch))
	(unspec:BLK [(match_dup 5) (match_dup 1)
		     (match_dup 6) (match_dup 7) (match_dup 3)]
		    UNSPEC_SCATTER))]
  {
    operands[5] = gcn_expand_scalar_to_vector_address (<MODE>mode,
						       operands[3],
						       operands[0],
						       operands[4]);
    operands[6] = gen_rtx_CONST_INT (VOIDmode, MEM_ADDR_SPACE (operands[0]));
    operands[7] = gen_rtx_CONST_INT (VOIDmode, MEM_VOLATILE_P (operands[0]));
  })

(define_split
  [(set (match_operand:V_ALL 0 "nonimmediate_operand")
	(unspec:V_ALL
	  [(match_operand:V_ALL 1 "memory_operand")]
	  UNSPEC_SGPRBASE))
   (clobber (match_scratch:<VnDI> 2))]
  ""
  [(set (match_dup 0)
	(unspec:V_ALL [(match_dup 5) (match_dup 6) (match_dup 7)
		       (mem:BLK (scratch))]
		      UNSPEC_GATHER))]
  {
    operands[5] = gcn_expand_scalar_to_vector_address (<MODE>mode, NULL,
						       operands[1],
						       operands[2]);
    operands[6] = gen_rtx_CONST_INT (VOIDmode, MEM_ADDR_SPACE (operands[1]));
    operands[7] = gen_rtx_CONST_INT (VOIDmode, MEM_VOLATILE_P (operands[1]));
  })

(define_split
  [(set (match_operand:V_ALL 0 "nonimmediate_operand")
	(vec_merge:V_ALL
	  (match_operand:V_ALL 1 "memory_operand")
	  (match_operand:V_ALL 2 "")
	  (match_operand:DI 3 "gcn_exec_reg_operand")))
   (clobber (match_scratch:<VnDI> 4))]
  ""
  [(set (match_dup 0)
	(vec_merge:V_ALL
	  (unspec:V_ALL [(match_dup 5) (match_dup 6) (match_dup 7)
			 (mem:BLK (scratch))]
			 UNSPEC_GATHER)
	  (match_dup 2)
	  (match_dup 3)))]
  {
    operands[5] = gcn_expand_scalar_to_vector_address (<MODE>mode,
						       operands[3],
						       operands[1],
						       operands[4]);
    operands[6] = gen_rtx_CONST_INT (VOIDmode, MEM_ADDR_SPACE (operands[1]));
    operands[7] = gen_rtx_CONST_INT (VOIDmode, MEM_VOLATILE_P (operands[1]));
  })

; TODO: Add zero/sign extending variants.

;; }}}
;; {{{ Lane moves

; v_writelane and v_readlane work regardless of exec flags.
; We allow source to be scratch.
;
; FIXME these should take A immediates

(define_insn "*vec_set<mode>"
  [(set (match_operand:V_1REG 0 "register_operand"		   "= v")
	(vec_merge:V_1REG
	  (vec_duplicate:V_1REG
	    (match_operand:<SCALAR_MODE> 1 "register_operand"	   " Sv"))
	  (match_operand:V_1REG 3 "gcn_register_or_unspec_operand" " U0")
	  (ashift (const_int 1)
		  (match_operand:SI 2 "gcn_alu_operand"		   "SvB"))))]
  ""
  "v_writelane_b32 %0, %1, %2"
  [(set_attr "type" "vop3a")
   (set_attr "length" "8")
   (set_attr "exec" "none")
   (set_attr "laneselect" "yes")])

; FIXME: 64bit operations really should be splitters, but I am not sure how
; to represent vertical subregs.
(define_insn "*vec_set<mode>"
  [(set (match_operand:V_2REG 0 "register_operand"		   "= v")
	(vec_merge:V_2REG
	  (vec_duplicate:V_2REG
	    (match_operand:<SCALAR_MODE> 1 "register_operand"	   " Sv"))
	  (match_operand:V_2REG 3 "gcn_register_or_unspec_operand" " U0")
	  (ashift (const_int 1)
		  (match_operand:SI 2 "gcn_alu_operand"		   "SvB"))))]
  ""
  "v_writelane_b32 %L0, %L1, %2\;v_writelane_b32 %H0, %H1, %2"
  [(set_attr "type" "vmult")
   (set_attr "length" "16")
   (set_attr "exec" "none")
   (set_attr "laneselect" "yes")])

(define_expand "vec_set<mode>"
  [(set (match_operand:V_ALL 0 "register_operand")
	(vec_merge:V_ALL
	  (vec_duplicate:V_ALL
	    (match_operand:<SCALAR_MODE> 1 "register_operand"))
	  (match_dup 0)
	  (ashift (const_int 1) (match_operand:SI 2 "gcn_alu_operand"))))]
  "")

(define_insn "*vec_set<mode>_1"
  [(set (match_operand:V_1REG 0 "register_operand"		   "=v")
	(vec_merge:V_1REG
	  (vec_duplicate:V_1REG
	    (match_operand:<SCALAR_MODE> 1 "register_operand"	   "Sv"))
	  (match_operand:V_1REG 3 "gcn_register_or_unspec_operand" "U0")
	  (match_operand:SI 2 "const_int_operand"		   " i")))]
  "((unsigned) exact_log2 (INTVAL (operands[2])) < GET_MODE_NUNITS (<MODE>mode))"
  {
    operands[2] = GEN_INT (exact_log2 (INTVAL (operands[2])));
    return "v_writelane_b32 %0, %1, %2";
  }
  [(set_attr "type" "vop3a")
   (set_attr "length" "8")
   (set_attr "exec" "none")
   (set_attr "laneselect" "yes")])

(define_insn "*vec_set<mode>_1"
  [(set (match_operand:V_2REG 0 "register_operand"		   "=v")
	(vec_merge:V_2REG
	  (vec_duplicate:V_2REG
	    (match_operand:<SCALAR_MODE> 1 "register_operand"	   "Sv"))
	  (match_operand:V_2REG 3 "gcn_register_or_unspec_operand" "U0")
	  (match_operand:SI 2 "const_int_operand"		   " i")))]
  "((unsigned) exact_log2 (INTVAL (operands[2])) < GET_MODE_NUNITS (<MODE>mode))"
  {
    operands[2] = GEN_INT (exact_log2 (INTVAL (operands[2])));
    return "v_writelane_b32 %L0, %L1, %2\;v_writelane_b32 %H0, %H1, %2";
  }
  [(set_attr "type" "vmult")
   (set_attr "length" "16")
   (set_attr "exec" "none")
   (set_attr "laneselect" "yes")])

(define_insn "vec_duplicate<mode><exec>"
  [(set (match_operand:V_1REG 0 "register_operand"	   "=v")
	(vec_duplicate:V_1REG
	  (match_operand:<SCALAR_MODE> 1 "gcn_alu_operand" "SvB")))]
  ""
  "v_mov_b32\t%0, %1"
  [(set_attr "type" "vop3a")
   (set_attr "length" "8")])

(define_insn "vec_duplicate<mode><exec>"
  [(set (match_operand:V_2REG 0 "register_operand"	   "=  v")
	(vec_duplicate:V_2REG
	  (match_operand:<SCALAR_MODE> 1 "gcn_alu_operand" "SvDB")))]
  ""
  "v_mov_b32\t%L0, %L1\;v_mov_b32\t%H0, %H1"
  [(set_attr "type" "vop3a")
   (set_attr "length" "16")])

(define_insn "vec_extract<mode><scalar_mode>"
  [(set (match_operand:<SCALAR_MODE> 0 "register_operand"  "=Sg")
	(vec_select:<SCALAR_MODE>
	  (match_operand:V_1REG 1 "register_operand"	   "  v")
	  (parallel [(match_operand:SI 2 "gcn_alu_operand" "SvB")])))]
  ""
  "v_readlane_b32 %0, %1, %2"
  [(set_attr "type" "vop3a")
   (set_attr "length" "8")
   (set_attr "exec" "none")
   (set_attr "laneselect" "yes")])

(define_insn "vec_extract<mode><scalar_mode>"
  [(set (match_operand:<SCALAR_MODE> 0 "register_operand"  "=&Sg")
	(vec_select:<SCALAR_MODE>
	  (match_operand:V_2REG 1 "register_operand"	   "   v")
	  (parallel [(match_operand:SI 2 "gcn_alu_operand" " SvB")])))]
  ""
  "v_readlane_b32 %L0, %L1, %2\;v_readlane_b32 %H0, %H1, %2"
  [(set_attr "type" "vmult")
   (set_attr "length" "16")
   (set_attr "exec" "none")
   (set_attr "laneselect" "yes")])

(define_expand "extract_last_<mode>"
  [(match_operand:<SCALAR_MODE> 0 "register_operand")
   (match_operand:DI 1 "gcn_alu_operand")
   (match_operand:V_ALL 2 "register_operand")]
  "can_create_pseudo_p ()"
  {
    rtx dst = operands[0];
    rtx mask = operands[1];
    rtx vect = operands[2];
    rtx tmpreg = gen_reg_rtx (SImode);

    emit_insn (gen_clzdi2 (tmpreg, mask));
    emit_insn (gen_subsi3 (tmpreg, GEN_INT (63), tmpreg));
    emit_insn (gen_vec_extract<mode><scalar_mode> (dst, vect, tmpreg));
    DONE;
  })

(define_expand "fold_extract_last_<mode>"
  [(match_operand:<SCALAR_MODE> 0 "register_operand")
   (match_operand:<SCALAR_MODE> 1 "gcn_alu_operand")
   (match_operand:DI 2 "gcn_alu_operand")
   (match_operand:V_ALL 3 "register_operand")]
  "can_create_pseudo_p ()"
  {
    rtx dst = operands[0];
    rtx default_value = operands[1];
    rtx mask = operands[2];
    rtx vect = operands[3];
    rtx else_label = gen_label_rtx ();
    rtx end_label = gen_label_rtx ();

    rtx cond = gen_rtx_EQ (VOIDmode, mask, const0_rtx);
    emit_jump_insn (gen_cbranchdi4 (cond, mask, const0_rtx, else_label));
    emit_insn (gen_extract_last_<mode> (dst, mask, vect));
    emit_jump_insn (gen_jump (end_label));
    emit_barrier ();
    emit_label (else_label);
    emit_move_insn (dst, default_value);
    emit_label (end_label);
    DONE;
  })

(define_expand "vec_init<mode><scalar_mode>"
  [(match_operand:V_ALL 0 "register_operand")
   (match_operand 1)]
  ""
  {
    gcn_expand_vector_init (operands[0], operands[1]);
    DONE;
  })

;; }}}
;; {{{ Scatter / Gather

;; GCN does not have an instruction for loading a vector from contiguous
;; memory so *all* loads and stores are eventually converted to scatter
;; or gather.
;;
;; GCC does not permit MEM to hold vectors of addresses, so we must use an
;; unspec.  The unspec formats are as follows:
;;
;;     (unspec:V??
;;	 [(<address expression>)
;;	  (<addr_space_t>)
;;	  (<use_glc>)
;;	  (mem:BLK (scratch))]
;;	 UNSPEC_GATHER)
;;
;;     (unspec:BLK
;;	  [(<address expression>)
;;	   (<source register>)
;;	   (<addr_space_t>)
;;	   (<use_glc>)
;;	   (<exec>)]
;;	  UNSPEC_SCATTER)
;;
;; - Loads are expected to be wrapped in a vec_merge, so do not need <exec>.
;; - The mem:BLK does not contain any real information, but indicates that an
;;   unknown memory read is taking place.  Stores are expected to use a similar
;;   mem:BLK outside the unspec.
;; - The address space and glc (volatile) fields are there to replace the
;;   fields normally found in a MEM.
;; - Multiple forms of address expression are supported, below.

(define_expand "gather_load<mode><vnsi>"
  [(match_operand:V_ALL 0 "register_operand")
   (match_operand:DI 1 "register_operand")
   (match_operand:<VnSI> 2 "register_operand")
   (match_operand 3 "immediate_operand")
   (match_operand:SI 4 "gcn_alu_operand")]
  ""
  {
    rtx addr = gcn_expand_scaled_offsets (DEFAULT_ADDR_SPACE, operands[1],
					  operands[2], operands[4],
					  INTVAL (operands[3]), NULL);

    if (GET_MODE (addr) == <VnDI>mode)
      emit_insn (gen_gather<mode>_insn_1offset (operands[0], addr, const0_rtx,
						const0_rtx, const0_rtx));
    else
      emit_insn (gen_gather<mode>_insn_2offsets (operands[0], operands[1],
						 addr, const0_rtx, const0_rtx,
						 const0_rtx));
    DONE;
  })

; Allow any address expression
(define_expand "gather<mode>_expr<exec>"
  [(set (match_operand:V_ALL 0 "register_operand")
	(unspec:V_ALL
	  [(match_operand 1 "")
	   (match_operand 2 "immediate_operand")
	   (match_operand 3 "immediate_operand")
	   (mem:BLK (scratch))]
	  UNSPEC_GATHER))]
    ""
    {})

(define_insn "gather<mode>_insn_1offset<exec>"
  [(set (match_operand:V_ALL 0 "register_operand"		   "=v")
	(unspec:V_ALL
	  [(plus:<VnDI> (match_operand:<VnDI> 1 "register_operand" " v")
			(vec_duplicate:<VnDI>
			  (match_operand 2 "immediate_operand"	   " n")))
	   (match_operand 3 "immediate_operand"			   " n")
	   (match_operand 4 "immediate_operand"			   " n")
	   (mem:BLK (scratch))]
	  UNSPEC_GATHER))]
  "(AS_FLAT_P (INTVAL (operands[3]))
    && ((TARGET_GCN3 && INTVAL(operands[2]) == 0)
	|| ((unsigned HOST_WIDE_INT)INTVAL(operands[2]) < 0x1000)))
    || (AS_GLOBAL_P (INTVAL (operands[3]))
	&& (((unsigned HOST_WIDE_INT)INTVAL(operands[2]) + 0x1000) < 0x2000))"
  {
    addr_space_t as = INTVAL (operands[3]);
    const char *glc = INTVAL (operands[4]) ? " glc" : "";

    static char buf[200];
    if (AS_FLAT_P (as))
      {
	if (TARGET_GCN5_PLUS)
	  sprintf (buf, "flat_load%%o0\t%%0, %%1 offset:%%2%s\;s_waitcnt\t0",
		   glc);
	else
	  sprintf (buf, "flat_load%%o0\t%%0, %%1%s\;s_waitcnt\t0", glc);
      }
    else if (AS_GLOBAL_P (as))
      sprintf (buf, "global_load%%o0\t%%0, %%1, off offset:%%2%s\;"
	       "s_waitcnt\tvmcnt(0)", glc);
    else
      gcc_unreachable ();

    return buf;
  }
  [(set_attr "type" "flat")
   (set_attr "length" "12")])

(define_insn "gather<mode>_insn_1offset_ds<exec>"
  [(set (match_operand:V_ALL 0 "register_operand"		   "=v")
	(unspec:V_ALL
	  [(plus:<VnSI> (match_operand:<VnSI> 1 "register_operand" " v")
			(vec_duplicate:<VnSI>
			  (match_operand 2 "immediate_operand"	   " n")))
	   (match_operand 3 "immediate_operand"			   " n")
	   (match_operand 4 "immediate_operand"			   " n")
	   (mem:BLK (scratch))]
	  UNSPEC_GATHER))]
  "(AS_ANY_DS_P (INTVAL (operands[3]))
    && ((unsigned HOST_WIDE_INT)INTVAL(operands[2]) < 0x10000))"
  {
    addr_space_t as = INTVAL (operands[3]);
    static char buf[200];
    sprintf (buf, "ds_read%%b0\t%%0, %%1 offset:%%2%s\;s_waitcnt\tlgkmcnt(0)",
	     (AS_GDS_P (as) ? " gds" : ""));
    return buf;
  }
  [(set_attr "type" "ds")
   (set_attr "length" "12")])

(define_insn "gather<mode>_insn_2offsets<exec>"
  [(set (match_operand:V_ALL 0 "register_operand"			"=v")
	(unspec:V_ALL
	  [(plus:<VnDI>
	     (plus:<VnDI>
	       (vec_duplicate:<VnDI>
		 (match_operand:DI 1 "register_operand"			"Sv"))
	       (sign_extend:<VnDI>
		 (match_operand:<VnSI> 2 "register_operand"		" v")))
	     (vec_duplicate:<VnDI> (match_operand 3 "immediate_operand" " n")))
	   (match_operand 4 "immediate_operand"				" n")
	   (match_operand 5 "immediate_operand"				" n")
	   (mem:BLK (scratch))]
	  UNSPEC_GATHER))]
  "(AS_GLOBAL_P (INTVAL (operands[4]))
    && (((unsigned HOST_WIDE_INT)INTVAL(operands[3]) + 0x1000) < 0x2000))"
  {
    addr_space_t as = INTVAL (operands[4]);
    const char *glc = INTVAL (operands[5]) ? " glc" : "";

    static char buf[200];
    if (AS_GLOBAL_P (as))
      {
	/* Work around assembler bug in which a 64-bit register is expected,
	but a 32-bit value would be correct.  */
	int reg = REGNO (operands[2]) - FIRST_VGPR_REG;
	sprintf (buf, "global_load%%o0\t%%0, v[%d:%d], %%1 offset:%%3%s\;"
		      "s_waitcnt\tvmcnt(0)", reg, reg + 1, glc);
      }
    else
      gcc_unreachable ();
      
    return buf;
  }
  [(set_attr "type" "flat")
   (set_attr "length" "12")])

(define_expand "scatter_store<mode><vnsi>"
  [(match_operand:DI 0 "register_operand")
   (match_operand:<VnSI> 1 "register_operand")
   (match_operand 2 "immediate_operand")
   (match_operand:SI 3 "gcn_alu_operand")
   (match_operand:V_ALL 4 "register_operand")]
  ""
  {
    rtx addr = gcn_expand_scaled_offsets (DEFAULT_ADDR_SPACE, operands[0],
					  operands[1], operands[3],
					  INTVAL (operands[2]), NULL);

    if (GET_MODE (addr) == <VnDI>mode)
      emit_insn (gen_scatter<mode>_insn_1offset (addr, const0_rtx, operands[4],
						 const0_rtx, const0_rtx));
    else
      emit_insn (gen_scatter<mode>_insn_2offsets (operands[0], addr,
						  const0_rtx, operands[4],
						  const0_rtx, const0_rtx));
    DONE;
  })

; Allow any address expression
(define_expand "scatter<mode>_expr<exec_scatter>"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(match_operand:<VnDI> 0 "")
	   (match_operand:V_ALL 1 "register_operand")
	   (match_operand 2 "immediate_operand")
	   (match_operand 3 "immediate_operand")]
	  UNSPEC_SCATTER))]
  ""
  {})

(define_insn "scatter<mode>_insn_1offset<exec_scatter>"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(plus:<VnDI> (match_operand:<VnDI> 0 "register_operand" "v")
			(vec_duplicate:<VnDI>
			  (match_operand 1 "immediate_operand"	   "n")))
	   (match_operand:V_ALL 2 "register_operand"		   "v")
	   (match_operand 3 "immediate_operand"			   "n")
	   (match_operand 4 "immediate_operand"			   "n")]
	  UNSPEC_SCATTER))]
  "(AS_FLAT_P (INTVAL (operands[3]))
    && (INTVAL(operands[1]) == 0
	|| (TARGET_GCN5_PLUS
	    && (unsigned HOST_WIDE_INT)INTVAL(operands[1]) < 0x1000)))
    || (AS_GLOBAL_P (INTVAL (operands[3]))
	&& (((unsigned HOST_WIDE_INT)INTVAL(operands[1]) + 0x1000) < 0x2000))"
  {
    addr_space_t as = INTVAL (operands[3]);
    const char *glc = INTVAL (operands[4]) ? " glc" : "";

    static char buf[200];
    if (AS_FLAT_P (as))
      {
	if (TARGET_GCN5_PLUS)
	  sprintf (buf, "flat_store%%s2\t%%0, %%2 offset:%%1%s", glc);
	else
	  sprintf (buf, "flat_store%%s2\t%%0, %%2%s", glc);
      }
    else if (AS_GLOBAL_P (as))
      sprintf (buf, "global_store%%s2\t%%0, %%2, off offset:%%1%s", glc);
    else
      gcc_unreachable ();

    return buf;
  }
  [(set_attr "type" "flat")
   (set_attr "length" "12")])

(define_insn "scatter<mode>_insn_1offset_ds<exec_scatter>"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(plus:<VnSI> (match_operand:<VnSI> 0 "register_operand" "v")
			(vec_duplicate:<VnSI>
			  (match_operand 1 "immediate_operand"	   "n")))
	   (match_operand:V_ALL 2 "register_operand"		   "v")
	   (match_operand 3 "immediate_operand"			   "n")
	   (match_operand 4 "immediate_operand"			   "n")]
	  UNSPEC_SCATTER))]
  "(AS_ANY_DS_P (INTVAL (operands[3]))
    && ((unsigned HOST_WIDE_INT)INTVAL(operands[1]) < 0x10000))"
  {
    addr_space_t as = INTVAL (operands[3]);
    static char buf[200];
    sprintf (buf, "ds_write%%b2\t%%0, %%2 offset:%%1%s",
	     (AS_GDS_P (as) ? " gds" : ""));
    return buf;
  }
  [(set_attr "type" "ds")
   (set_attr "length" "12")])

(define_insn "scatter<mode>_insn_2offsets<exec_scatter>"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(plus:<VnDI>
	     (plus:<VnDI>
	       (vec_duplicate:<VnDI>
		 (match_operand:DI 0 "register_operand"			"Sv"))
	       (sign_extend:<VnDI>
		 (match_operand:<VnSI> 1 "register_operand"		" v")))
	     (vec_duplicate:<VnDI> (match_operand 2 "immediate_operand" " n")))
	   (match_operand:V_ALL 3 "register_operand"			" v")
	   (match_operand 4 "immediate_operand"				" n")
	   (match_operand 5 "immediate_operand"				" n")]
	  UNSPEC_SCATTER))]
  "(AS_GLOBAL_P (INTVAL (operands[4]))
    && (((unsigned HOST_WIDE_INT)INTVAL(operands[2]) + 0x1000) < 0x2000))"
  {
    addr_space_t as = INTVAL (operands[4]);
    const char *glc = INTVAL (operands[5]) ? " glc" : "";

    static char buf[200];
    if (AS_GLOBAL_P (as))
      {
	/* Work around assembler bug in which a 64-bit register is expected,
	but a 32-bit value would be correct.  */
	int reg = REGNO (operands[1]) - FIRST_VGPR_REG;
	sprintf (buf, "global_store%%s3\tv[%d:%d], %%3, %%0 offset:%%2%s",
		 reg, reg + 1, glc);
      }
    else
      gcc_unreachable ();

    return buf;
  }
  [(set_attr "type" "flat")
   (set_attr "length" "12")])

;; }}}
;; {{{ Permutations

(define_insn "ds_bpermute<mode>"
  [(set (match_operand:V_1REG 0 "register_operand"    "=v")
	(unspec:V_1REG
	  [(match_operand:V_1REG 2 "register_operand" " v")
	   (match_operand:<VnSI> 1 "register_operand" " v")
	   (match_operand:DI 3 "gcn_exec_reg_operand" " e")]
	  UNSPEC_BPERMUTE))]
  ""
  "ds_bpermute_b32\t%0, %1, %2\;s_waitcnt\tlgkmcnt(0)"
  [(set_attr "type" "vop2")
   (set_attr "length" "12")])

(define_insn_and_split "ds_bpermute<mode>"
  [(set (match_operand:V_2REG 0 "register_operand"    "=&v")
	(unspec:V_2REG
	  [(match_operand:V_2REG 2 "register_operand" " v0")
	   (match_operand:<VnSI> 1 "register_operand" "  v")
	   (match_operand:DI 3 "gcn_exec_reg_operand" "  e")]
	  UNSPEC_BPERMUTE))]
  ""
  "#"
  "reload_completed"
  [(set (match_dup 4) (unspec:<VnSI>
			[(match_dup 6) (match_dup 1) (match_dup 3)]
			UNSPEC_BPERMUTE))
   (set (match_dup 5) (unspec:<VnSI>
			[(match_dup 7) (match_dup 1) (match_dup 3)]
			UNSPEC_BPERMUTE))]
  {
    operands[4] = gcn_operand_part (<MODE>mode, operands[0], 0);
    operands[5] = gcn_operand_part (<MODE>mode, operands[0], 1);
    operands[6] = gcn_operand_part (<MODE>mode, operands[2], 0);
    operands[7] = gcn_operand_part (<MODE>mode, operands[2], 1);
  }
  [(set_attr "type" "vmult")
   (set_attr "length" "24")])

(define_insn "@dpp_move<mode>"
  [(set (match_operand:V_noHI 0 "register_operand"    "=v")
	(unspec:V_noHI
	  [(match_operand:V_noHI 1 "register_operand" " v")
	   (match_operand:SI 2 "const_int_operand"    " n")]
	  UNSPEC_MOV_DPP_SHR))]
  ""
  {
    return gcn_expand_dpp_shr_insn (<MODE>mode, "v_mov_b32",
				    UNSPEC_MOV_DPP_SHR, INTVAL (operands[2]));
  }
  [(set_attr "type" "vop_dpp")
   (set_attr "length" "16")])

;; }}}
;; {{{ ALU special case: add/sub

(define_insn "add<mode>3<exec_clobber>"
  [(set (match_operand:V_INT_1REG 0 "register_operand"   "=  v")
	(plus:V_INT_1REG
	  (match_operand:V_INT_1REG 1 "register_operand" "%  v")
	  (match_operand:V_INT_1REG 2 "gcn_alu_operand"  "vSvB")))
   (clobber (reg:DI VCC_REG))]
  ""
  "v_add%^_u32\t%0, vcc, %2, %1"
  [(set_attr "type" "vop2")
   (set_attr "length" "8")])

(define_insn "add<mode>3_dup<exec_clobber>"
  [(set (match_operand:V_INT_1REG 0 "register_operand"	     "= v")
	(plus:V_INT_1REG
	  (vec_duplicate:V_INT_1REG
	    (match_operand:<SCALAR_MODE> 2 "gcn_alu_operand" "SvB"))
	  (match_operand:V_INT_1REG 1 "register_operand"     "  v")))
   (clobber (reg:DI VCC_REG))]
  ""
  "v_add%^_u32\t%0, vcc, %2, %1"
  [(set_attr "type" "vop2")
   (set_attr "length" "8")])

(define_insn "add<mode>3_vcc<exec_vcc>"
  [(set (match_operand:V_SI 0 "register_operand"   "=  v,   v")
	(plus:V_SI
	  (match_operand:V_SI 1 "register_operand" "%  v,   v")
	  (match_operand:V_SI 2 "gcn_alu_operand"  "vSvB,vSvB")))
   (set (match_operand:DI 3 "register_operand"	   "= cV,  Sg")
	(ltu:DI (plus:V_SI (match_dup 1) (match_dup 2))
		(match_dup 1)))]
  ""
  "v_add%^_u32\t%0, %3, %2, %1"
  [(set_attr "type" "vop2,vop3b")
   (set_attr "length" "8")])

; This pattern only changes the VCC bits when the corresponding lane is
; enabled, so the set must be described as an ior.

(define_insn "add<mode>3_vcc_dup<exec_vcc>"
  [(set (match_operand:V_SI 0 "register_operand"   "= v,  v")
	(plus:V_SI
	  (vec_duplicate:V_SI
	    (match_operand:SI 1 "gcn_alu_operand"  "SvB,SvB"))
	  (match_operand:V_SI 2 "register_operand" "  v,  v")))
   (set (match_operand:DI 3 "register_operand"	   "=cV, Sg")
	(ltu:DI (plus:V_SI (vec_duplicate:V_SI (match_dup 2))
			   (match_dup 1))
		(vec_duplicate:V_SI (match_dup 2))))]
  ""
  "v_add%^_u32\t%0, %3, %2, %1"
  [(set_attr "type" "vop2,vop3b")
   (set_attr "length" "8,8")])

; v_addc does not accept an SGPR because the VCC read already counts as an
; SGPR use and the number of SGPR operands is limited to 1.  It does not
; accept "B" immediate constants due to a related bus conflict.

(define_insn "addc<mode>3<exec_vcc>"
  [(set (match_operand:V_SI 0 "register_operand"     "=v,   v")
	(plus:V_SI
	  (plus:V_SI
	    (vec_merge:V_SI
	      (vec_duplicate:V_SI (const_int 1))
	      (vec_duplicate:V_SI (const_int 0))
	      (match_operand:DI 3 "register_operand" " cV,cVSv"))
	    (match_operand:V_SI 1 "gcn_alu_operand"  "% v,  vA"))
	  (match_operand:V_SI 2 "gcn_alu_operand"    " vA,  vA")))
   (set (match_operand:DI 4 "register_operand"	     "=cV,cVSg")
	(ior:DI (ltu:DI (plus:V_SI
			  (plus:V_SI
			    (vec_merge:V_SI
			      (vec_duplicate:V_SI (const_int 1))
			      (vec_duplicate:V_SI (const_int 0))
			      (match_dup 3))
			    (match_dup 1))
			  (match_dup 2))
			(match_dup 2))
		(ltu:DI (plus:V_SI
			  (vec_merge:V_SI
			    (vec_duplicate:V_SI (const_int 1))
			    (vec_duplicate:V_SI (const_int 0))
			    (match_dup 3))
			  (match_dup 1))
			(match_dup 1))))]
  ""
  "v_addc%^_u32\t%0, %4, %2, %1, %3"
  [(set_attr "type" "vop2,vop3b")
   (set_attr "length" "4,8")])

(define_insn "sub<mode>3<exec_clobber>"
  [(set (match_operand:V_INT_1REG 0 "register_operand"  "=  v,   v")
	(minus:V_INT_1REG
	  (match_operand:V_INT_1REG 1 "gcn_alu_operand" "vSvB,   v")
	  (match_operand:V_INT_1REG 2 "gcn_alu_operand" "   v,vSvB")))
   (clobber (reg:DI VCC_REG))]
  ""
  "@
   v_sub%^_u32\t%0, vcc, %1, %2
   v_subrev%^_u32\t%0, vcc, %2, %1"
  [(set_attr "type" "vop2")
   (set_attr "length" "8,8")])

(define_insn "sub<mode>3_vcc<exec_vcc>"
  [(set (match_operand:V_SI 0 "register_operand"  "=  v,   v,   v,   v")
	(minus:V_SI
	  (match_operand:V_SI 1 "gcn_alu_operand" "vSvB,vSvB,   v,   v")
	  (match_operand:V_SI 2 "gcn_alu_operand" "   v,   v,vSvB,vSvB")))
   (set (match_operand:DI 3 "register_operand"	  "= cV,  Sg,  cV,  Sg")
	(gtu:DI (minus:V_SI (match_dup 1) (match_dup 2))
		(match_dup 1)))]
  ""
  "@
   v_sub%^_u32\t%0, %3, %1, %2
   v_sub%^_u32\t%0, %3, %1, %2
   v_subrev%^_u32\t%0, %3, %2, %1
   v_subrev%^_u32\t%0, %3, %2, %1"
  [(set_attr "type" "vop2,vop3b,vop2,vop3b")
   (set_attr "length" "8")])

; v_subb does not accept an SGPR because the VCC read already counts as an
; SGPR use and the number of SGPR operands is limited to 1.  It does not
; accept "B" immediate constants due to a related bus conflict.

(define_insn "subc<mode>3<exec_vcc>"
  [(set (match_operand:V_SI 0 "register_operand"    "= v, v, v, v")
	(minus:V_SI
	  (minus:V_SI
	    (vec_merge:V_SI
	      (vec_duplicate:V_SI (const_int 1))
	      (vec_duplicate:V_SI (const_int 0))
	      (match_operand:DI 3 "gcn_alu_operand" " cV,cVSv,cV,cVSv"))
	    (match_operand:V_SI 1 "gcn_alu_operand" " vA,  vA, v,  vA"))
	  (match_operand:V_SI 2 "gcn_alu_operand"   "  v,  vA,vA,  vA")))
   (set (match_operand:DI 4 "register_operand"	    "=cV,cVSg,cV,cVSg")
	(ior:DI (gtu:DI (minus:V_SI (minus:V_SI
				      (vec_merge:V_SI
					(vec_duplicate:V_SI (const_int 1))
					(vec_duplicate:V_SI (const_int 0))
					(match_dup 3))
				       (match_dup 1))
				     (match_dup 2))
			(match_dup 2))
		(ltu:DI (minus:V_SI (vec_merge:V_SI
				      (vec_duplicate:V_SI (const_int 1))
				      (vec_duplicate:V_SI (const_int 0))
				      (match_dup 3))
				    (match_dup 1))
			(match_dup 1))))]
  ""
  "@
   v_subb%^_u32\t%0, %4, %1, %2, %3
   v_subb%^_u32\t%0, %4, %1, %2, %3
   v_subbrev%^_u32\t%0, %4, %2, %1, %3
   v_subbrev%^_u32\t%0, %4, %2, %1, %3"
  [(set_attr "type" "vop2,vop3b,vop2,vop3b")
   (set_attr "length" "4,8,4,8")])

(define_insn_and_split "add<mode>3"
  [(set (match_operand:V_DI 0 "register_operand"   "=  v")
	(plus:V_DI
	  (match_operand:V_DI 1 "register_operand" "%vDb")
	  (match_operand:V_DI 2 "gcn_alu_operand"  " vDb")))
   (clobber (reg:DI VCC_REG))]
  ""
  "#"
  "gcn_can_split_p  (<MODE>mode, operands[0])
   && gcn_can_split_p (<MODE>mode, operands[1])
   && gcn_can_split_p (<MODE>mode, operands[2])"
  [(const_int 0)]
  {
    rtx vcc = gen_rtx_REG (DImode, VCC_REG);
    emit_insn (gen_add<vnsi>3_vcc
		(gcn_operand_part (<MODE>mode, operands[0], 0),
		 gcn_operand_part (<MODE>mode, operands[1], 0),
		 gcn_operand_part (<MODE>mode, operands[2], 0),
		 vcc));
    emit_insn (gen_addc<vnsi>3
		(gcn_operand_part (<MODE>mode, operands[0], 1),
		 gcn_operand_part (<MODE>mode, operands[1], 1),
		 gcn_operand_part (<MODE>mode, operands[2], 1),
		 vcc, vcc));
    DONE;
  }
  [(set_attr "type" "vmult")
   (set_attr "length" "8")])

(define_insn_and_split "add<mode>3_exec"
  [(set (match_operand:V_DI 0 "register_operand"		 "=  v")
	(vec_merge:V_DI
	  (plus:V_DI
	    (match_operand:V_DI 1 "register_operand"		 "%vDb")
	    (match_operand:V_DI 2 "gcn_alu_operand"		 " vDb"))
	  (match_operand:V_DI 3 "gcn_register_or_unspec_operand" "  U0")
	  (match_operand:DI 4 "gcn_exec_reg_operand"		 "   e")))
   (clobber (reg:DI VCC_REG))]
  ""
  "#"
  "gcn_can_split_p  (<MODE>mode, operands[0])
   && gcn_can_split_p (<MODE>mode, operands[1])
   && gcn_can_split_p (<MODE>mode, operands[2])
   && gcn_can_split_p (<MODE>mode, operands[4])"
  [(const_int 0)]
  {
    rtx vcc = gen_rtx_REG (DImode, VCC_REG);
    emit_insn (gen_add<vnsi>3_vcc_exec
		(gcn_operand_part (<MODE>mode, operands[0], 0),
		 gcn_operand_part (<MODE>mode, operands[1], 0),
		 gcn_operand_part (<MODE>mode, operands[2], 0),
		 vcc,
		 gcn_operand_part (<MODE>mode, operands[3], 0),
		 operands[4]));
    emit_insn (gen_addc<vnsi>3_exec
		(gcn_operand_part (<MODE>mode, operands[0], 1),
		 gcn_operand_part (<MODE>mode, operands[1], 1),
		 gcn_operand_part (<MODE>mode, operands[2], 1),
		 vcc, vcc,
		 gcn_operand_part (<MODE>mode, operands[3], 1),
		 operands[4]));
    DONE;
  }
  [(set_attr "type" "vmult")
   (set_attr "length" "8")])

(define_insn_and_split "sub<mode>3"
  [(set (match_operand:V_DI 0 "register_operand"  "= v,  v")
	(minus:V_DI                                        
	  (match_operand:V_DI 1 "gcn_alu_operand" "vDb,  v")
	  (match_operand:V_DI 2 "gcn_alu_operand" "  v,vDb")))
   (clobber (reg:DI VCC_REG))]
  ""
  "#"
  "gcn_can_split_p  (<MODE>mode, operands[0])
   && gcn_can_split_p (<MODE>mode, operands[1])
   && gcn_can_split_p (<MODE>mode, operands[2])"
  [(const_int 0)]
  {
    rtx vcc = gen_rtx_REG (DImode, VCC_REG);
    emit_insn (gen_sub<vnsi>3_vcc
		(gcn_operand_part (<MODE>mode, operands[0], 0),
		 gcn_operand_part (<MODE>mode, operands[1], 0),
		 gcn_operand_part (<MODE>mode, operands[2], 0),
		 vcc));
    emit_insn (gen_subc<vnsi>3
		(gcn_operand_part (<MODE>mode, operands[0], 1),
		 gcn_operand_part (<MODE>mode, operands[1], 1),
		 gcn_operand_part (<MODE>mode, operands[2], 1),
		 vcc, vcc));
    DONE;
  }
  [(set_attr "type" "vmult")
   (set_attr "length" "8")])

(define_insn_and_split "sub<mode>3_exec"
  [(set (match_operand:V_DI 0 "register_operand"		 "=  v,   v")
	(vec_merge:V_DI                                                         
	  (minus:V_DI                                                           
	    (match_operand:V_DI 1 "gcn_alu_operand"		 "vSvB,   v")
	    (match_operand:V_DI 2 "gcn_alu_operand"		 "   v,vSvB"))
	  (match_operand:V_DI 3 "gcn_register_or_unspec_operand" " U0,  U0")
	  (match_operand:DI 4 "gcn_exec_reg_operand"		 "   e,   e")))
   (clobber (reg:DI VCC_REG))]
  "register_operand (operands[1], VOIDmode)
   || register_operand (operands[2], VOIDmode)"
  "#"
  "gcn_can_split_p  (<MODE>mode, operands[0])
   && gcn_can_split_p (<MODE>mode, operands[1])
   && gcn_can_split_p (<MODE>mode, operands[2])
   && gcn_can_split_p (<MODE>mode, operands[3])"
  [(const_int 0)]
  {
    rtx vcc = gen_rtx_REG (DImode, VCC_REG);
    emit_insn (gen_sub<vnsi>3_vcc_exec
		(gcn_operand_part (<MODE>mode, operands[0], 0),
		 gcn_operand_part (<MODE>mode, operands[1], 0),
		 gcn_operand_part (<MODE>mode, operands[2], 0),
		 vcc,
		 gcn_operand_part (<MODE>mode, operands[3], 0),
		 operands[4]));
    emit_insn (gen_subc<vnsi>3_exec
		(gcn_operand_part (<MODE>mode, operands[0], 1),
		 gcn_operand_part (<MODE>mode, operands[1], 1),
		 gcn_operand_part (<MODE>mode, operands[2], 1),
		 vcc, vcc,
		 gcn_operand_part (<MODE>mode, operands[3], 1),
		 operands[4]));
    DONE;
  }
  [(set_attr "type" "vmult")
   (set_attr "length" "8")])

(define_insn_and_split "add<mode>3_zext"
  [(set (match_operand:V_DI 0 "register_operand"      "= v,  v")
	(plus:V_DI
	  (zero_extend:V_DI
	    (match_operand:<VnSI> 1 "gcn_alu_operand" " vA, vB"))
	  (match_operand:V_DI 2 "gcn_alu_operand"     "vDb,vDA")))
   (clobber (reg:DI VCC_REG))]
  ""
  "#"
  "gcn_can_split_p  (<MODE>mode, operands[0])
   && gcn_can_split_p (<MODE>mode, operands[2])"
  [(const_int 0)]
  {
    rtx vcc = gen_rtx_REG (DImode, VCC_REG);
    emit_insn (gen_add<vnsi>3_vcc
		(gcn_operand_part (<MODE>mode, operands[0], 0),
		 operands[1],
		 gcn_operand_part (<MODE>mode, operands[2], 0),
		 vcc));
    emit_insn (gen_addc<vnsi>3
		(gcn_operand_part (<MODE>mode, operands[0], 1),
		 gcn_operand_part (<MODE>mode, operands[2], 1),
		 const0_rtx, vcc, vcc));
    DONE;
  }
  [(set_attr "type" "vmult")
   (set_attr "length" "8")])

(define_insn_and_split "add<mode>3_zext_exec"
  [(set (match_operand:V_DI 0 "register_operand"		 "= v,  v")
	(vec_merge:V_DI
	  (plus:V_DI
	    (zero_extend:V_DI
	      (match_operand:<VnSI> 1 "gcn_alu_operand"		 " vA, vB"))
	    (match_operand:V_DI 2 "gcn_alu_operand"		 "vDb,vDA"))
	  (match_operand:V_DI 3 "gcn_register_or_unspec_operand" " U0, U0")
	  (match_operand:DI 4 "gcn_exec_reg_operand"		 "  e,  e")))
   (clobber (reg:DI VCC_REG))]
  ""
  "#"
  "gcn_can_split_p  (<MODE>mode, operands[0])
   && gcn_can_split_p (<MODE>mode, operands[2])
   && gcn_can_split_p (<MODE>mode, operands[3])"
  [(const_int 0)]
  {
    rtx vcc = gen_rtx_REG (DImode, VCC_REG);
    emit_insn (gen_add<vnsi>3_vcc_exec
		(gcn_operand_part (<MODE>mode, operands[0], 0),
		 operands[1],
		 gcn_operand_part (<MODE>mode, operands[2], 0),
		 vcc,
		 gcn_operand_part (<MODE>mode, operands[3], 0),
		 operands[4]));
    emit_insn (gen_addc<vnsi>3_exec
		(gcn_operand_part (<MODE>mode, operands[0], 1),
		 gcn_operand_part (<MODE>mode, operands[2], 1),
		 const0_rtx, vcc, vcc,
		 gcn_operand_part (<MODE>mode, operands[3], 1),
		 operands[4]));
    DONE;
  }
  [(set_attr "type" "vmult")
   (set_attr "length" "8")])

(define_insn_and_split "add<mode>3_vcc_zext_dup"
  [(set (match_operand:V_DI 0 "register_operand"    "=    v,    v")
	(plus:V_DI
	  (zero_extend:V_DI
	    (vec_duplicate:<VnSI>
	      (match_operand:SI 1 "gcn_alu_operand" "   BSv,  ASv")))
	  (match_operand:V_DI 2 "gcn_alu_operand"   "   vDA,  vDb")))
   (set (match_operand:DI 3 "register_operand"	    "=&SgcV,&SgcV")
	(ltu:DI (plus:V_DI 
		  (zero_extend:V_DI (vec_duplicate:<VnSI> (match_dup 1)))
		  (match_dup 2))
		(match_dup 1)))]
  ""
  "#"
  "gcn_can_split_p  (<MODE>mode, operands[0])
   && gcn_can_split_p (<MODE>mode, operands[2])"
  [(const_int 0)]
  {
    emit_insn (gen_add<vnsi>3_vcc_dup
		(gcn_operand_part (<MODE>mode, operands[0], 0),
		 gcn_operand_part (DImode, operands[1], 0),
		 gcn_operand_part (<MODE>mode, operands[2], 0),
		 operands[3]));
    emit_insn (gen_addc<vnsi>3
		(gcn_operand_part (<MODE>mode, operands[0], 1),
		 gcn_operand_part (<MODE>mode, operands[2], 1),
		 const0_rtx, operands[3], operands[3]));
    DONE;
  }
  [(set_attr "type" "vmult")
   (set_attr "length" "8")])

(define_expand "add<mode>3_zext_dup"
  [(match_operand:V_DI 0 "register_operand")
   (match_operand:SI 1 "gcn_alu_operand")
   (match_operand:V_DI 2 "gcn_alu_operand")]
  ""
  {
    rtx vcc = gen_rtx_REG (DImode, VCC_REG);
    emit_insn (gen_add<mode>3_vcc_zext_dup (operands[0], operands[1],
					    operands[2], vcc));
    DONE;
  })

(define_insn_and_split "add<mode>3_vcc_zext_dup_exec"
  [(set (match_operand:V_DI 0 "register_operand"	      "=    v,    v")
	(vec_merge:V_DI
	  (plus:V_DI
	    (zero_extend:V_DI
	      (vec_duplicate:<VnSI>
		(match_operand:SI 1 "gcn_alu_operand"	      "   ASv,  BSv")))
	    (match_operand:V_DI 2 "gcn_alu_operand"	      "   vDb,  vDA"))
	  (match_operand:V_DI 4 "gcn_register_or_unspec_operand" " U0,   U0")
	  (match_operand:DI 5 "gcn_exec_reg_operand"	      "     e,    e")))
   (set (match_operand:DI 3 "register_operand"		      "=&SgcV,&SgcV")
	(and:DI
	  (ltu:DI (plus:V_DI 
		    (zero_extend:V_DI (vec_duplicate:<VnSI> (match_dup 1)))
		    (match_dup 2))
		  (match_dup 1))
	  (match_dup 5)))]
  ""
  "#"
  "gcn_can_split_p  (<MODE>mode, operands[0])
   && gcn_can_split_p (<MODE>mode, operands[2])
   && gcn_can_split_p (<MODE>mode, operands[4])"
  [(const_int 0)]
  {
    emit_insn (gen_add<vnsi>3_vcc_dup_exec
		(gcn_operand_part (<MODE>mode, operands[0], 0),
		 gcn_operand_part (DImode, operands[1], 0),
		 gcn_operand_part (<MODE>mode, operands[2], 0),
		 operands[3],
		 gcn_operand_part (<MODE>mode, operands[4], 0),
		 operands[5]));
    emit_insn (gen_addc<vnsi>3_exec
		(gcn_operand_part (<MODE>mode, operands[0], 1),
		 gcn_operand_part (<MODE>mode, operands[2], 1),
		 const0_rtx, operands[3], operands[3],
		 gcn_operand_part (<MODE>mode, operands[4], 1),
		 operands[5]));
    DONE;
  }
  [(set_attr "type" "vmult")
   (set_attr "length" "8")])

(define_expand "add<mode>3_zext_dup_exec"
  [(match_operand:V_DI 0 "register_operand")
   (match_operand:SI 1 "gcn_alu_operand")
   (match_operand:V_DI 2 "gcn_alu_operand")
   (match_operand:V_DI 3 "gcn_register_or_unspec_operand")
   (match_operand:DI 4 "gcn_exec_reg_operand")]
  ""
  {
    rtx vcc = gen_rtx_REG (DImode, VCC_REG);
    emit_insn (gen_add<mode>3_vcc_zext_dup_exec (operands[0], operands[1],
						 operands[2], vcc, operands[3],
						 operands[4]));
    DONE;
  })

(define_insn_and_split "add<mode>3_vcc_zext_dup2"
  [(set (match_operand:V_DI 0 "register_operand"		   "=    v")
	(plus:V_DI
	  (zero_extend:V_DI (match_operand:<VnSI> 1 "gcn_alu_operand" " vA"))
	  (vec_duplicate:V_DI (match_operand:DI 2 "gcn_alu_operand" " DbSv"))))
   (set (match_operand:DI 3 "register_operand"			   "=&SgcV")
	(ltu:DI (plus:V_DI 
		  (zero_extend:V_DI (match_dup 1))
		  (vec_duplicate:V_DI (match_dup 2)))
		(match_dup 1)))]
  ""
  "#"
  "gcn_can_split_p (<MODE>mode, operands[0])"
  [(const_int 0)]
  {
    emit_insn (gen_add<vnsi>3_vcc_dup
		(gcn_operand_part (<MODE>mode, operands[0], 0),
		 gcn_operand_part (DImode, operands[2], 0),
		 operands[1],
		 operands[3]));
    rtx dsthi = gcn_operand_part (<MODE>mode, operands[0], 1);
    emit_insn (gen_vec_duplicate<vnsi>
		(dsthi, gcn_operand_part (DImode, operands[2], 1)));
    emit_insn (gen_addc<vnsi>3 (dsthi, dsthi, const0_rtx, operands[3],
				operands[3]));
    DONE;
  }
  [(set_attr "type" "vmult")
   (set_attr "length" "8")])

(define_expand "add<mode>3_zext_dup2"
  [(match_operand:V_DI 0 "register_operand")
   (match_operand:<VnSI> 1 "gcn_alu_operand")
   (match_operand:DI 2 "gcn_alu_operand")]
  ""
  {
    rtx vcc = gen_rtx_REG (DImode, VCC_REG);
    emit_insn (gen_add<mode>3_vcc_zext_dup2 (operands[0], operands[1],
					     operands[2], vcc));
    DONE;
  })

(define_insn_and_split "add<mode>3_vcc_zext_dup2_exec"
  [(set (match_operand:V_DI 0 "register_operand"		    "=    v")
	(vec_merge:V_DI
	  (plus:V_DI
	    (zero_extend:V_DI (match_operand:<VnSI> 1 "gcn_alu_operand" "vA"))
	    (vec_duplicate:V_DI (match_operand:DI 2 "gcn_alu_operand"  "BSv")))
	  (match_operand:V_DI 4 "gcn_register_or_unspec_operand"    "    U0")
	  (match_operand:DI 5 "gcn_exec_reg_operand"		    "     e")))
   (set (match_operand:DI 3 "register_operand"			    "=&SgcV")
	(and:DI
	  (ltu:DI (plus:V_DI 
		    (zero_extend:V_DI (match_dup 1))
		    (vec_duplicate:V_DI (match_dup 2)))
		  (match_dup 1))
	  (match_dup 5)))]
  ""
  "#"
  "gcn_can_split_p  (<MODE>mode, operands[0])
   && gcn_can_split_p (<MODE>mode, operands[4])"
  [(const_int 0)]
  {
    emit_insn (gen_add<vnsi>3_vcc_dup_exec
		(gcn_operand_part (<MODE>mode, operands[0], 0),
		 gcn_operand_part (DImode, operands[2], 0),
		 operands[1],
		 operands[3],
		 gcn_operand_part (<MODE>mode, operands[4], 0),
		 operands[5]));
    rtx dsthi = gcn_operand_part (<MODE>mode, operands[0], 1);
    emit_insn (gen_vec_duplicate<vnsi>_exec
		(dsthi, gcn_operand_part (DImode, operands[2], 1),
		 gcn_operand_part (<MODE>mode, operands[4], 1),
		 operands[5]));
    emit_insn (gen_addc<vnsi>3_exec
		(dsthi, dsthi, const0_rtx, operands[3], operands[3],
		 gcn_operand_part (<MODE>mode, operands[4], 1),
		 operands[5]));
    DONE;
  }
  [(set_attr "type" "vmult")
   (set_attr "length" "8")])

(define_expand "add<mode>3_zext_dup2_exec"
  [(match_operand:V_DI 0 "register_operand")
   (match_operand:<VnSI> 1 "gcn_alu_operand")
   (match_operand:DI 2 "gcn_alu_operand")
   (match_operand:V_DI 3 "gcn_register_or_unspec_operand")
   (match_operand:DI 4 "gcn_exec_reg_operand")]
  ""
  {
    rtx vcc = gen_rtx_REG (DImode, VCC_REG);
    emit_insn (gen_add<mode>3_vcc_zext_dup2_exec (operands[0], operands[1],
						  operands[2], vcc,
						  operands[3], operands[4]));
    DONE;
  })

(define_insn_and_split "add<mode>3_sext_dup2"
  [(set (match_operand:V_DI 0 "register_operand"		      "= v")
	(plus:V_DI
	  (sign_extend:V_DI (match_operand:<VnSI> 1 "gcn_alu_operand" " vA"))
	  (vec_duplicate:V_DI (match_operand:DI 2 "gcn_alu_operand"   "BSv"))))
   (clobber (match_scratch:<VnSI> 3				      "=&v"))
   (clobber (reg:DI VCC_REG))]
  ""
  "#"
  "gcn_can_split_p  (<MODE>mode, operands[0])"
  [(const_int 0)]
  {
    rtx vcc = gen_rtx_REG (DImode, VCC_REG);
    emit_insn (gen_ashr<vnsi>3 (operands[3], operands[1], GEN_INT (31)));
    emit_insn (gen_add<vnsi>3_vcc_dup
		(gcn_operand_part (<MODE>mode, operands[0], 0),
		 gcn_operand_part (DImode, operands[2], 0),
		 operands[1],
		 vcc));
    rtx dsthi = gcn_operand_part (<MODE>mode, operands[0], 1);
    emit_insn (gen_vec_duplicate<vnsi>
		(dsthi, gcn_operand_part (DImode, operands[2], 1)));
    emit_insn (gen_addc<vnsi>3 (dsthi, dsthi, operands[3], vcc, vcc));
    DONE;
  }
  [(set_attr "type" "vmult")
   (set_attr "length" "8")])

(define_insn_and_split "add<mode>3_sext_dup2_exec"
  [(set (match_operand:V_DI 0 "register_operand"		       "= v")
	(vec_merge:V_DI
	  (plus:V_DI
	    (sign_extend:V_DI (match_operand:<VnSI> 1 "gcn_alu_operand" "vA"))
	    (vec_duplicate:V_DI (match_operand:DI 2 "gcn_alu_operand"  "BSv")))
	  (match_operand:V_DI 3 "gcn_register_or_unspec_operand"       " U0")
	  (match_operand:DI 4 "gcn_exec_reg_operand"		       "  e")))
   (clobber (match_scratch:<VnSI> 5				       "=&v"))
   (clobber (reg:DI VCC_REG))]
  ""
  "#"
  "gcn_can_split_p  (<MODE>mode, operands[0])
   && gcn_can_split_p (<MODE>mode, operands[3])"
  [(const_int 0)]
  {
    rtx vcc = gen_rtx_REG (DImode, VCC_REG);
    emit_insn (gen_ashr<vnsi>3_exec (operands[5], operands[1], GEN_INT (31),
				     gcn_gen_undef (<VnSI>mode), operands[4]));
    emit_insn (gen_add<vnsi>3_vcc_dup_exec
		(gcn_operand_part (<MODE>mode, operands[0], 0),
		 gcn_operand_part (DImode, operands[2], 0),
		 operands[1],
		 vcc,
		 gcn_operand_part (<MODE>mode, operands[3], 0),
		 operands[4]));
    rtx dsthi = gcn_operand_part (<MODE>mode, operands[0], 1);
    emit_insn (gen_vec_duplicate<vnsi>_exec
		(dsthi, gcn_operand_part (DImode, operands[2], 1),
		gcn_operand_part (<MODE>mode, operands[3], 1),
		operands[4]));
    emit_insn (gen_addc<vnsi>3_exec
		(dsthi, dsthi, operands[5], vcc, vcc,
		 gcn_operand_part (<MODE>mode, operands[3], 1),
		 operands[4]));
    DONE;
  }
  [(set_attr "type" "vmult")
   (set_attr "length" "8")])

;; }}}
;; {{{ DS memory ALU: add/sub

(define_mode_iterator DS_ARITH_MODE [V64SI V64SF V64DI])
(define_mode_iterator DS_ARITH_SCALAR_MODE [SI SF DI])

;; FIXME: the vector patterns probably need RD expanded to a vector of
;;        addresses.  For now, the only way a vector can get into LDS is
;;        if the user puts it there manually.
;;
;; FIXME: the scalar patterns are probably fine in themselves, but need to be
;;        checked to see if anything can ever use them.

(define_insn "add<mode>3_ds<exec>"
  [(set (match_operand:DS_ARITH_MODE 0 "gcn_ds_memory_operand"	 "=RD")
	(plus:DS_ARITH_MODE
	  (match_operand:DS_ARITH_MODE 1 "gcn_ds_memory_operand" "%RD")
	  (match_operand:DS_ARITH_MODE 2 "register_operand"	 "  v")))]
  "rtx_equal_p (operands[0], operands[1])"
  "ds_add%u0\t%A0, %2%O0"
  [(set_attr "type" "ds")
   (set_attr "length" "8")])

(define_insn "add<mode>3_ds_scalar"
  [(set (match_operand:DS_ARITH_SCALAR_MODE 0 "gcn_ds_memory_operand" "=RD")
	(plus:DS_ARITH_SCALAR_MODE
	  (match_operand:DS_ARITH_SCALAR_MODE 1 "gcn_ds_memory_operand"
								      "%RD")
	  (match_operand:DS_ARITH_SCALAR_MODE 2 "register_operand"    "  v")))]
  "rtx_equal_p (operands[0], operands[1])"
  "ds_add%u0\t%A0, %2%O0"
  [(set_attr "type" "ds")
   (set_attr "length" "8")])

(define_insn "sub<mode>3_ds<exec>"
  [(set (match_operand:DS_ARITH_MODE 0 "gcn_ds_memory_operand"	 "=RD")
	(minus:DS_ARITH_MODE
	  (match_operand:DS_ARITH_MODE 1 "gcn_ds_memory_operand" " RD")
	  (match_operand:DS_ARITH_MODE 2 "register_operand"	 "  v")))]
  "rtx_equal_p (operands[0], operands[1])"
  "ds_sub%u0\t%A0, %2%O0"
  [(set_attr "type" "ds")
   (set_attr "length" "8")])

(define_insn "sub<mode>3_ds_scalar"
  [(set (match_operand:DS_ARITH_SCALAR_MODE 0 "gcn_ds_memory_operand" "=RD")
	(minus:DS_ARITH_SCALAR_MODE
	  (match_operand:DS_ARITH_SCALAR_MODE 1 "gcn_ds_memory_operand"
								      " RD")
	  (match_operand:DS_ARITH_SCALAR_MODE 2 "register_operand"    "  v")))]
  "rtx_equal_p (operands[0], operands[1])"
  "ds_sub%u0\t%A0, %2%O0"
  [(set_attr "type" "ds")
   (set_attr "length" "8")])

(define_insn "subr<mode>3_ds<exec>"
  [(set (match_operand:DS_ARITH_MODE 0 "gcn_ds_memory_operand"	 "=RD")
	(minus:DS_ARITH_MODE
	  (match_operand:DS_ARITH_MODE 2 "register_operand"	 "  v")
	  (match_operand:DS_ARITH_MODE 1 "gcn_ds_memory_operand" " RD")))]
  "rtx_equal_p (operands[0], operands[1])"
  "ds_rsub%u0\t%A0, %2%O0"
  [(set_attr "type" "ds")
   (set_attr "length" "8")])

(define_insn "subr<mode>3_ds_scalar"
  [(set (match_operand:DS_ARITH_SCALAR_MODE 0 "gcn_ds_memory_operand" "=RD")
	(minus:DS_ARITH_SCALAR_MODE
	  (match_operand:DS_ARITH_SCALAR_MODE 2 "register_operand"    "  v")
	  (match_operand:DS_ARITH_SCALAR_MODE 1 "gcn_ds_memory_operand" 
								      " RD")))]
  "rtx_equal_p (operands[0], operands[1])"
  "ds_rsub%u0\t%A0, %2%O0"
  [(set_attr "type" "ds")
   (set_attr "length" "8")])

;; }}}
;; {{{ ALU special case: mult

(define_insn "<su>mul<mode>3_highpart<exec>"
  [(set (match_operand:V_SI 0 "register_operand"        "=  v")
	(truncate:V_SI
	  (lshiftrt:<VnDI>
	    (mult:<VnDI>
	      (any_extend:<VnDI>
		(match_operand:V_SI 1 "gcn_alu_operand" "  %v"))
	      (any_extend:<VnDI>
		(match_operand:V_SI 2 "gcn_alu_operand" "vSvA")))
	    (const_int 32))))]
  ""
  "v_mul_hi<sgnsuffix>0\t%0, %2, %1"
  [(set_attr "type" "vop3a")
   (set_attr "length" "8")])

(define_insn "mul<mode>3<exec>"
  [(set (match_operand:V_INT_1REG 0 "register_operand"  "=   v")
	(mult:V_INT_1REG
	  (match_operand:V_INT_1REG 1 "gcn_alu_operand" "%vSvA")
	  (match_operand:V_INT_1REG 2 "gcn_alu_operand" " vSvA")))]
  ""
  "v_mul_lo_u32\t%0, %1, %2"
  [(set_attr "type" "vop3a")
   (set_attr "length" "8")])

(define_insn "mul<mode>3_dup<exec>"
  [(set (match_operand:V_INT_1REG 0 "register_operand"	     "=   v")
	(mult:V_INT_1REG
	  (match_operand:V_INT_1REG 1 "gcn_alu_operand"	     "%vSvA")
	  (vec_duplicate:V_INT_1REG
	    (match_operand:<SCALAR_MODE> 2 "gcn_alu_operand" "  SvA"))))]
  ""
  "v_mul_lo_u32\t%0, %1, %2"
  [(set_attr "type" "vop3a")
   (set_attr "length" "8")])

(define_insn_and_split "mul<mode>3"
  [(set (match_operand:V_DI 0 "register_operand"  "=&v")
	(mult:V_DI
	  (match_operand:V_DI 1 "gcn_alu_operand" "% v")
	  (match_operand:V_DI 2 "gcn_alu_operand" "vDA")))
   (clobber (match_scratch:<VnSI> 3		  "=&v"))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
  {
    rtx out_lo = gcn_operand_part (<MODE>mode, operands[0], 0);
    rtx out_hi = gcn_operand_part (<MODE>mode, operands[0], 1);
    rtx left_lo = gcn_operand_part (<MODE>mode, operands[1], 0);
    rtx left_hi = gcn_operand_part (<MODE>mode, operands[1], 1);
    rtx right_lo = gcn_operand_part (<MODE>mode, operands[2], 0);
    rtx right_hi = gcn_operand_part (<MODE>mode, operands[2], 1);
    rtx tmp = operands[3];

    emit_insn (gen_mul<vnsi>3 (out_lo, left_lo, right_lo));
    emit_insn (gen_umul<vnsi>3_highpart (out_hi, left_lo, right_lo));
    emit_insn (gen_mul<vnsi>3 (tmp, left_hi, right_lo));
    emit_insn (gen_add<vnsi>3 (out_hi, out_hi, tmp));
    emit_insn (gen_mul<vnsi>3 (tmp, left_lo, right_hi));
    emit_insn (gen_add<vnsi>3 (out_hi, out_hi, tmp));
    emit_insn (gen_mul<vnsi>3 (tmp, left_hi, right_hi));
    emit_insn (gen_add<vnsi>3 (out_hi, out_hi, tmp));
    DONE;
  })

(define_insn_and_split "mul<mode>3_exec"
  [(set (match_operand:V_DI 0 "register_operand"		 "=&v")
	(vec_merge:V_DI
	  (mult:V_DI
	    (match_operand:V_DI 1 "gcn_alu_operand"		 "% v")
	    (match_operand:V_DI 2 "gcn_alu_operand"		 "vDA"))
	  (match_operand:V_DI 3 "gcn_register_or_unspec_operand" " U0")
	  (match_operand:DI 4 "gcn_exec_reg_operand"		 "  e")))
   (clobber (match_scratch:<VnSI> 5				 "=&v"))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
  {
    rtx out_lo = gcn_operand_part (<MODE>mode, operands[0], 0);
    rtx out_hi = gcn_operand_part (<MODE>mode, operands[0], 1);
    rtx left_lo = gcn_operand_part (<MODE>mode, operands[1], 0);
    rtx left_hi = gcn_operand_part (<MODE>mode, operands[1], 1);
    rtx right_lo = gcn_operand_part (<MODE>mode, operands[2], 0);
    rtx right_hi = gcn_operand_part (<MODE>mode, operands[2], 1);
    rtx exec = operands[4];
    rtx tmp = operands[5];

    rtx old_lo, old_hi;
    if (GET_CODE (operands[3]) == UNSPEC)
      {
	old_lo = old_hi = gcn_gen_undef (<VnSI>mode);
      }
    else
      {
	old_lo = gcn_operand_part (<MODE>mode, operands[3], 0);
	old_hi = gcn_operand_part (<MODE>mode, operands[3], 1);
      }

    rtx undef = gcn_gen_undef (<VnSI>mode);

    emit_insn (gen_mul<vnsi>3_exec (out_lo, left_lo, right_lo, old_lo, exec));
    emit_insn (gen_umul<vnsi>3_highpart_exec (out_hi, left_lo, right_lo,
					      old_hi, exec));
    emit_insn (gen_mul<vnsi>3_exec (tmp, left_hi, right_lo, undef, exec));
    emit_insn (gen_add<vnsi>3_exec (out_hi, out_hi, tmp, out_hi, exec));
    emit_insn (gen_mul<vnsi>3_exec (tmp, left_lo, right_hi, undef, exec));
    emit_insn (gen_add<vnsi>3_exec (out_hi, out_hi, tmp, out_hi, exec));
    emit_insn (gen_mul<vnsi>3_exec (tmp, left_hi, right_hi, undef, exec));
    emit_insn (gen_add<vnsi>3_exec (out_hi, out_hi, tmp, out_hi, exec));
    DONE;
  })

(define_insn_and_split "mul<mode>3_zext"
  [(set (match_operand:V_DI 0 "register_operand"      "=&v")
	(mult:V_DI
	  (zero_extend:V_DI
	    (match_operand:<VnSI> 1 "gcn_alu_operand" "  v"))
	  (match_operand:V_DI 2 "gcn_alu_operand"     "vDA")))
   (clobber (match_scratch:<VnSI> 3		      "=&v"))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
  {
    rtx out_lo = gcn_operand_part (<MODE>mode, operands[0], 0);
    rtx out_hi = gcn_operand_part (<MODE>mode, operands[0], 1);
    rtx left = operands[1];
    rtx right_lo = gcn_operand_part (<MODE>mode, operands[2], 0);
    rtx right_hi = gcn_operand_part (<MODE>mode, operands[2], 1);
    rtx tmp = operands[3];

    emit_insn (gen_mul<vnsi>3 (out_lo, left, right_lo));
    emit_insn (gen_umul<vnsi>3_highpart (out_hi, left, right_lo));
    emit_insn (gen_mul<vnsi>3 (tmp, left, right_hi));
    emit_insn (gen_add<vnsi>3 (out_hi, out_hi, tmp));
    DONE;
  })

(define_insn_and_split "mul<mode>3_zext_exec"
  [(set (match_operand:V_DI 0 "register_operand"		 "=&v")
	(vec_merge:V_DI
	  (mult:V_DI
	    (zero_extend:V_DI
	      (match_operand:<VnSI> 1 "gcn_alu_operand"		 "  v"))
	    (match_operand:V_DI 2 "gcn_alu_operand"		 "vDA"))
	  (match_operand:V_DI 3 "gcn_register_or_unspec_operand" " U0")
	  (match_operand:DI 4 "gcn_exec_reg_operand"		 "  e")))
   (clobber (match_scratch:<VnSI> 5				 "=&v"))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
  {
    rtx out_lo = gcn_operand_part (<MODE>mode, operands[0], 0);
    rtx out_hi = gcn_operand_part (<MODE>mode, operands[0], 1);
    rtx left = operands[1];
    rtx right_lo = gcn_operand_part (<MODE>mode, operands[2], 0);
    rtx right_hi = gcn_operand_part (<MODE>mode, operands[2], 1);
    rtx exec = operands[4];
    rtx tmp = operands[5];

    rtx old_lo, old_hi;
    if (GET_CODE (operands[3]) == UNSPEC)
      {
	old_lo = old_hi = gcn_gen_undef (<VnSI>mode);
      }
    else
      {
	old_lo = gcn_operand_part (<MODE>mode, operands[3], 0);
	old_hi = gcn_operand_part (<MODE>mode, operands[3], 1);
      }

    rtx undef = gcn_gen_undef (<VnSI>mode);

    emit_insn (gen_mul<vnsi>3_exec (out_lo, left, right_lo, old_lo, exec));
    emit_insn (gen_umul<vnsi>3_highpart_exec (out_hi, left, right_lo,
					      old_hi, exec));
    emit_insn (gen_mul<vnsi>3_exec (tmp, left, right_hi, undef, exec));
    emit_insn (gen_add<vnsi>3_exec (out_hi, out_hi, tmp, out_hi, exec));
    DONE;
  })

(define_insn_and_split "mul<mode>3_zext_dup2"
  [(set (match_operand:V_DI 0 "register_operand"      "= &v")
	(mult:V_DI
	  (zero_extend:V_DI
	    (match_operand:<VnSI> 1 "gcn_alu_operand" "   v"))
	  (vec_duplicate:V_DI
	    (match_operand:DI 2 "gcn_alu_operand"     "SvDA"))))
   (clobber (match_scratch:<VnSI> 3		      "= &v"))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
  {
    rtx out_lo = gcn_operand_part (<MODE>mode, operands[0], 0);
    rtx out_hi = gcn_operand_part (<MODE>mode, operands[0], 1);
    rtx left = operands[1];
    rtx right_lo = gcn_operand_part (<MODE>mode, operands[2], 0);
    rtx right_hi = gcn_operand_part (<MODE>mode, operands[2], 1);
    rtx tmp = operands[3];

    emit_insn (gen_mul<vnsi>3 (out_lo, left, right_lo));
    emit_insn (gen_umul<vnsi>3_highpart (out_hi, left, right_lo));
    emit_insn (gen_mul<vnsi>3 (tmp, left, right_hi));
    emit_insn (gen_add<vnsi>3 (out_hi, out_hi, tmp));
    DONE;
  })

(define_insn_and_split "mul<mode>3_zext_dup2_exec"
  [(set (match_operand:V_DI 0 "register_operand"		 "= &v")
	(vec_merge:V_DI
	  (mult:V_DI
	    (zero_extend:V_DI
	      (match_operand:<VnSI> 1 "gcn_alu_operand"		 "   v"))
	    (vec_duplicate:V_DI
	      (match_operand:DI 2 "gcn_alu_operand"		 "SvDA")))
	  (match_operand:V_DI 3 "gcn_register_or_unspec_operand" "  U0")
	  (match_operand:DI 4 "gcn_exec_reg_operand"		 "   e")))
   (clobber (match_scratch:<VnSI> 5				 "= &v"))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
  {
    rtx out_lo = gcn_operand_part (<MODE>mode, operands[0], 0);
    rtx out_hi = gcn_operand_part (<MODE>mode, operands[0], 1);
    rtx left = operands[1];
    rtx right_lo = gcn_operand_part (<MODE>mode, operands[2], 0);
    rtx right_hi = gcn_operand_part (<MODE>mode, operands[2], 1);
    rtx exec = operands[4];
    rtx tmp = operands[5];

    rtx old_lo, old_hi;
    if (GET_CODE (operands[3]) == UNSPEC)
      {
	old_lo = old_hi = gcn_gen_undef (<VnSI>mode);
      }
    else
      {
	old_lo = gcn_operand_part (<MODE>mode, operands[3], 0);
	old_hi = gcn_operand_part (<MODE>mode, operands[3], 1);
      }

    rtx undef = gcn_gen_undef (<VnSI>mode);

    emit_insn (gen_mul<vnsi>3_exec (out_lo, left, right_lo, old_lo, exec));
    emit_insn (gen_umul<vnsi>3_highpart_exec (out_hi, left, right_lo,
					      old_hi, exec));
    emit_insn (gen_mul<vnsi>3_exec (tmp, left, right_hi, undef, exec));
    emit_insn (gen_add<vnsi>3_exec (out_hi, out_hi, tmp, out_hi, exec));
    DONE;
  })

;; }}}
;; {{{ ALU generic case

(define_code_iterator bitop [and ior xor])
(define_code_iterator shiftop [ashift lshiftrt ashiftrt])
(define_code_iterator minmaxop [smin smax umin umax])

(define_insn "<expander><mode>2<exec>"
  [(set (match_operand:V_INT_1REG 0 "gcn_valu_dst_operand"    "=  v")
	(bitunop:V_INT_1REG
	  (match_operand:V_INT_1REG 1 "gcn_valu_src0_operand" "vSvB")))]
  ""
  "v_<mnemonic>0\t%0, %1"
  [(set_attr "type" "vop1")
   (set_attr "length" "8")])

(define_insn "<expander><mode>3<exec>"
  [(set (match_operand:V_INT_1REG 0 "gcn_valu_dst_operand"	 "=  v,RD")
	(bitop:V_INT_1REG
	  (match_operand:V_INT_1REG 1 "gcn_valu_src0_operand"	 "%  v, 0")
	  (match_operand:V_INT_1REG 2 "gcn_valu_src1com_operand" "vSvB, v")))]
  ""
  "@
   v_<mnemonic>0\t%0, %2, %1
   ds_<mnemonic>0\t%A0, %2%O0"
  [(set_attr "type" "vop2,ds")
   (set_attr "length" "8,8")])

(define_insn_and_split "<expander><mode>3"
  [(set (match_operand:V_DI 0 "gcn_valu_dst_operand"	   "=  v,RD")
	(bitop:V_DI
	  (match_operand:V_DI 1 "gcn_valu_src0_operand"    "%  v,RD")
	  (match_operand:V_DI 2 "gcn_valu_src1com_operand" "vSvB, v")))]
  ""
  "@
   #
   ds_<mnemonic>0\t%A0, %2%O0"
  "(reload_completed && !gcn_ds_memory_operand (operands[0], <MODE>mode))"
  [(set (match_dup 3)
	(bitop:<VnSI> (match_dup 5) (match_dup 7)))
   (set (match_dup 4)
	(bitop:<VnSI> (match_dup 6) (match_dup 8)))]
  {
    operands[3] = gcn_operand_part (<MODE>mode, operands[0], 0);
    operands[4] = gcn_operand_part (<MODE>mode, operands[0], 1);
    operands[5] = gcn_operand_part (<MODE>mode, operands[1], 0);
    operands[6] = gcn_operand_part (<MODE>mode, operands[1], 1);
    operands[7] = gcn_operand_part (<MODE>mode, operands[2], 0);
    operands[8] = gcn_operand_part (<MODE>mode, operands[2], 1);
  }
  [(set_attr "type" "vmult,ds")
   (set_attr "length" "16,8")])

(define_insn_and_split "<expander><mode>3_exec"
  [(set (match_operand:V_DI 0 "gcn_valu_dst_operand"		  "=  v,RD")
	(vec_merge:V_DI
	  (bitop:V_DI
	    (match_operand:V_DI 1 "gcn_valu_src0_operand"	  "%  v,RD")
	    (match_operand:V_DI 2 "gcn_valu_src1com_operand"	  "vSvB, v"))
	  (match_operand:V_DI 3 "gcn_register_ds_or_unspec_operand" "U0,U0")
	  (match_operand:DI 4 "gcn_exec_reg_operand"		  "   e, e")))]
  "!memory_operand (operands[0], VOIDmode)
   || (rtx_equal_p (operands[0], operands[1])
       && register_operand (operands[2], VOIDmode))"
  "@
   #
   ds_<mnemonic>0\t%A0, %2%O0"
  "(reload_completed && !gcn_ds_memory_operand (operands[0], <MODE>mode))"
  [(set (match_dup 5)
	(vec_merge:<VnSI>
	  (bitop:<VnSI> (match_dup 7) (match_dup 9))
	  (match_dup 11)
	  (match_dup 4)))
   (set (match_dup 6)
	(vec_merge:<VnSI>
	  (bitop:<VnSI> (match_dup 8) (match_dup 10))
	  (match_dup 12)
	  (match_dup 4)))]
  {
    operands[5] = gcn_operand_part (<MODE>mode, operands[0], 0);
    operands[6] = gcn_operand_part (<MODE>mode, operands[0], 1);
    operands[7] = gcn_operand_part (<MODE>mode, operands[1], 0);
    operands[8] = gcn_operand_part (<MODE>mode, operands[1], 1);
    operands[9] = gcn_operand_part (<MODE>mode, operands[2], 0);
    operands[10] = gcn_operand_part (<MODE>mode, operands[2], 1);
    operands[11] = gcn_operand_part (<MODE>mode, operands[3], 0);
    operands[12] = gcn_operand_part (<MODE>mode, operands[3], 1);
  }
  [(set_attr "type" "vmult,ds")
   (set_attr "length" "16,8")])

(define_expand "<expander><mode>3"
  [(set (match_operand:V_QIHI 0 "register_operand"  "= v")
	(shiftop:V_QIHI
	  (match_operand:V_QIHI 1 "gcn_alu_operand" "  v")
	  (vec_duplicate:V_QIHI
	    (match_operand:SI 2 "gcn_alu_operand"   "SvB"))))]
  ""
  {
    enum {ashift, lshiftrt, ashiftrt};
    bool unsignedp = (<code> == lshiftrt);
    rtx insi1 = gen_reg_rtx (<VnSI>mode);
    rtx insi2 = gen_reg_rtx (SImode);
    rtx outsi = gen_reg_rtx (<VnSI>mode);

    convert_move (insi1, operands[1], unsignedp);
    convert_move (insi2, operands[2], unsignedp);
    emit_insn (gen_<expander><vnsi>3 (outsi, insi1, insi2));
    convert_move (operands[0], outsi, unsignedp);
    DONE;
  })

(define_insn "<expander><mode>3<exec>"
  [(set (match_operand:V_SI 0 "register_operand"  "= v")
	(shiftop:V_SI
	  (match_operand:V_SI 1 "gcn_alu_operand" "  v")
	  (vec_duplicate:V_SI
	    (match_operand:SI 2 "gcn_alu_operand"  "SvB"))))]
  ""
  "v_<revmnemonic>0\t%0, %2, %1"
  [(set_attr "type" "vop2")
   (set_attr "length" "8")])

(define_expand "v<expander><mode>3"
  [(set (match_operand:V_QIHI 0 "register_operand"  "=v")
	(shiftop:V_QIHI
	  (match_operand:V_QIHI 1 "gcn_alu_operand" " v")
	  (match_operand:V_QIHI 2 "gcn_alu_operand" "vB")))]
  ""
  {
    enum {ashift, lshiftrt, ashiftrt};
    bool unsignedp = (<code> == lshiftrt);
    rtx insi1 = gen_reg_rtx (<VnSI>mode);
    rtx insi2 = gen_reg_rtx (<VnSI>mode);
    rtx outsi = gen_reg_rtx (<VnSI>mode);

    convert_move (insi1, operands[1], unsignedp);
    convert_move (insi2, operands[2], unsignedp);
    emit_insn (gen_v<expander><vnsi>3 (outsi, insi1, insi2));
    convert_move (operands[0], outsi, unsignedp);
    DONE;
  })

(define_insn "v<expander><mode>3<exec>"
  [(set (match_operand:V_SI 0 "register_operand"  "=v")
	(shiftop:V_SI
	  (match_operand:V_SI 1 "gcn_alu_operand" " v")
	  (match_operand:V_SI 2 "gcn_alu_operand" "vB")))]
  ""
  "v_<revmnemonic>0\t%0, %2, %1"
  [(set_attr "type" "vop2")
   (set_attr "length" "8")])

(define_expand "<expander><mode>3"
  [(set (match_operand:V_QIHI 0 "gcn_valu_dst_operand")
	(minmaxop:V_QIHI
	  (match_operand:V_QIHI 1 "gcn_valu_src0_operand")
	  (match_operand:V_QIHI 2 "gcn_valu_src1com_operand")))]
  ""
  {
    enum {smin, umin, smax, umax};
    bool unsignedp = (<code> == umax || <code> == umin);
    rtx insi1 = gen_reg_rtx (<VnSI>mode);
    rtx insi2 = gen_reg_rtx (<VnSI>mode);
    rtx outsi = gen_reg_rtx (<VnSI>mode);

    convert_move (insi1, operands[1], unsignedp);
    convert_move (insi2, operands[2], unsignedp);
    emit_insn (gen_<code><vnsi>3 (outsi, insi1, insi2));
    convert_move (operands[0], outsi, unsignedp);
    DONE;
  })

(define_insn "<expander><vnsi>3<exec>"
  [(set (match_operand:V_SI 0 "gcn_valu_dst_operand"	   "=  v,RD")
	(minmaxop:V_SI
	  (match_operand:V_SI 1 "gcn_valu_src0_operand"    "%  v, 0")
	  (match_operand:V_SI 2 "gcn_valu_src1com_operand" "vSvB, v")))]
  ""
  "@
   v_<mnemonic>0\t%0, %2, %1
   ds_<mnemonic>0\t%A0, %2%O0"
  [(set_attr "type" "vop2,ds")
   (set_attr "length" "8,8")])

;; }}}
;; {{{ FP binops - special cases

; GCN does not directly provide a DFmode subtract instruction, so we do it by
; adding the negated second operand to the first.

(define_insn "sub<mode>3<exec>"
  [(set (match_operand:V_DF 0 "register_operand"  "=  v,   v")
	(minus:V_DF
	  (match_operand:V_DF 1 "gcn_alu_operand" "vSvB,   v")
	  (match_operand:V_DF 2 "gcn_alu_operand" "   v,vSvB")))]
  ""
  "@
   v_add_f64\t%0, %1, -%2
   v_add_f64\t%0, -%2, %1"
  [(set_attr "type" "vop3a")
   (set_attr "length" "8,8")])

(define_insn "subdf"
  [(set (match_operand:DF 0 "register_operand"  "=  v,   v")
	(minus:DF
	  (match_operand:DF 1 "gcn_alu_operand" "vSvB,   v")
	  (match_operand:DF 2 "gcn_alu_operand" "   v,vSvB")))]
  ""
  "@
   v_add_f64\t%0, %1, -%2
   v_add_f64\t%0, -%2, %1"
  [(set_attr "type" "vop3a")
   (set_attr "length" "8,8")])

;; }}}
;; {{{ FP binops - generic

(define_code_iterator comm_fp [plus mult smin smax])
(define_code_iterator nocomm_fp [minus])
(define_code_iterator all_fp [plus mult minus smin smax])

(define_insn "<expander><mode>3<exec>"
  [(set (match_operand:V_FP 0 "register_operand"  "=  v")
	(comm_fp:V_FP
	  (match_operand:V_FP 1 "gcn_alu_operand" "%  v")
	  (match_operand:V_FP 2 "gcn_alu_operand" "vSvB")))]
  ""
  "v_<mnemonic>0\t%0, %2, %1"
  [(set_attr "type" "vop2")
   (set_attr "length" "8")])

(define_insn "<expander><mode>3"
  [(set (match_operand:FP 0 "gcn_valu_dst_operand"    "=  v,  RL")
	(comm_fp:FP
	  (match_operand:FP 1 "gcn_valu_src0_operand" "%  v,   0")
	  (match_operand:FP 2 "gcn_valu_src1_operand" "vSvB,vSvB")))]
  ""
  "@
  v_<mnemonic>0\t%0, %2, %1
  v_<mnemonic>0\t%0, %1%O0"
  [(set_attr "type" "vop2,ds")
   (set_attr "length" "8")])

(define_insn "<expander><mode>3<exec>"
  [(set (match_operand:V_FP_1REG 0 "register_operand"  "=  v,   v")
	(nocomm_fp:V_FP_1REG
	  (match_operand:V_FP_1REG 1 "gcn_alu_operand" "vSvB,   v")
	  (match_operand:V_FP_1REG 2 "gcn_alu_operand" "   v,vSvB")))]
  ""
  "@
   v_<mnemonic>0\t%0, %1, %2
   v_<revmnemonic>0\t%0, %2, %1"
  [(set_attr "type" "vop2")
   (set_attr "length" "8,8")])

(define_insn "<expander><mode>3"
  [(set (match_operand:FP_1REG 0 "register_operand"  "=  v,   v")
	(nocomm_fp:FP_1REG
	  (match_operand:FP_1REG 1 "gcn_alu_operand" "vSvB,   v")
	  (match_operand:FP_1REG 2 "gcn_alu_operand" "   v,vSvB")))]
  ""
  "@
   v_<mnemonic>0\t%0, %1, %2
   v_<revmnemonic>0\t%0, %2, %1"
  [(set_attr "type" "vop2")
   (set_attr "length" "8,8")])

;; }}}
;; {{{ FP unops

(define_insn "abs<mode>2"
  [(set (match_operand:FP 0 "register_operand"		 "=v")
	(abs:FP (match_operand:FP 1 "register_operand" " v")))]
  ""
  "v_add%i0\t%0, 0, |%1|"
  [(set_attr "type" "vop3a")
   (set_attr "length" "8")])

(define_insn "abs<mode>2<exec>"
  [(set (match_operand:V_FP 0 "register_operand"   "=v")
	(abs:V_FP
	  (match_operand:V_FP 1 "register_operand" " v")))]
  ""
  "v_add%i0\t%0, 0, |%1|"
  [(set_attr "type" "vop3a")
   (set_attr "length" "8")])

(define_insn "neg<mode>2<exec>"
  [(set (match_operand:V_FP 0 "register_operand"   "=v")
	(neg:V_FP
	  (match_operand:V_FP 1 "register_operand" " v")))]
  ""
  "v_add%i0\t%0, 0, -%1"
  [(set_attr "type" "vop3a")
   (set_attr "length" "8")])

(define_insn "sqrt<mode>2<exec>"
  [(set (match_operand:V_FP 0 "register_operand"  "=  v")
	(sqrt:V_FP
	  (match_operand:V_FP 1 "gcn_alu_operand" "vSvB")))]
  "flag_unsafe_math_optimizations"
  "v_sqrt%i0\t%0, %1"
  [(set_attr "type" "vop1")
   (set_attr "length" "8")])

(define_insn "sqrt<mode>2"
  [(set (match_operand:FP 0 "register_operand"  "=  v")
	(sqrt:FP
	  (match_operand:FP 1 "gcn_alu_operand" "vSvB")))]
  "flag_unsafe_math_optimizations"
  "v_sqrt%i0\t%0, %1"
  [(set_attr "type" "vop1")
   (set_attr "length" "8")])

;; }}}
;; {{{ FP fused multiply and add

(define_insn "fma<mode>4<exec>"
  [(set (match_operand:V_FP 0 "register_operand"  "=  v,   v")
	(fma:V_FP
	  (match_operand:V_FP 1 "gcn_alu_operand" "% vA,  vA")
	  (match_operand:V_FP 2 "gcn_alu_operand" "  vA,vSvA")
	  (match_operand:V_FP 3 "gcn_alu_operand" "vSvA,  vA")))]
  ""
  "v_fma%i0\t%0, %1, %2, %3"
  [(set_attr "type" "vop3a")
   (set_attr "length" "8")])

(define_insn "fma<mode>4_negop2<exec>"
  [(set (match_operand:V_FP 0 "register_operand"    "=  v,   v,   v")
	(fma:V_FP
	  (match_operand:V_FP 1 "gcn_alu_operand"   "  vA,  vA,vSvA")
	  (neg:V_FP
	    (match_operand:V_FP 2 "gcn_alu_operand" "  vA,vSvA,  vA"))
	  (match_operand:V_FP 3 "gcn_alu_operand"   "vSvA,  vA,  vA")))]
  ""
  "v_fma%i0\t%0, %1, -%2, %3"
  [(set_attr "type" "vop3a")
   (set_attr "length" "8")])

(define_insn "fma<mode>4"
  [(set (match_operand:FP 0 "register_operand"  "=  v,   v")
	(fma:FP
	  (match_operand:FP 1 "gcn_alu_operand" "% vA,  vA")
	  (match_operand:FP 2 "gcn_alu_operand" "  vA,vSvA")
	  (match_operand:FP 3 "gcn_alu_operand" "vSvA,  vA")))]
  ""
  "v_fma%i0\t%0, %1, %2, %3"
  [(set_attr "type" "vop3a")
   (set_attr "length" "8")])

(define_insn "fma<mode>4_negop2"
  [(set (match_operand:FP 0 "register_operand"    "=  v,   v,   v")
	(fma:FP
	  (match_operand:FP 1 "gcn_alu_operand"   "  vA,  vA,vSvA")
	  (neg:FP
	    (match_operand:FP 2 "gcn_alu_operand" "  vA,vSvA,  vA"))
	  (match_operand:FP 3 "gcn_alu_operand"   "vSvA,  vA,  vA")))]
  ""
  "v_fma%i0\t%0, %1, -%2, %3"
  [(set_attr "type" "vop3a")
   (set_attr "length" "8")])

;; }}}
;; {{{ FP division

(define_insn "recip<mode>2<exec>"
  [(set (match_operand:V_FP 0 "register_operand"  "=  v")
	(div:V_FP
	  (vec_duplicate:V_FP (float:<SCALAR_MODE> (const_int 1)))
	  (match_operand:V_FP 1 "gcn_alu_operand" "vSvB")))]
  ""
  "v_rcp%i0\t%0, %1"
  [(set_attr "type" "vop1")
   (set_attr "length" "8")])

(define_insn "recip<mode>2"
  [(set (match_operand:FP 0 "register_operand"	 "=  v")
	(div:FP
	  (float:FP (const_int 1))
	  (match_operand:FP 1 "gcn_alu_operand"	 "vSvB")))]
  ""
  "v_rcp%i0\t%0, %1"
  [(set_attr "type" "vop1")
   (set_attr "length" "8")])

;; Do division via a = b * 1/c
;; The v_rcp_* instructions are not sufficiently accurate on their own,
;; so we use 2 v_fma_* instructions to do one round of Newton-Raphson
;; which the ISA manual says is enough to improve the reciprocal accuracy.
;;
;; FIXME: This does not handle denormals, NaNs, division-by-zero etc.

(define_expand "div<mode>3"
  [(match_operand:V_FP 0 "gcn_valu_dst_operand")
   (match_operand:V_FP 1 "gcn_valu_src0_operand")
   (match_operand:V_FP 2 "gcn_valu_src0_operand")]
  "flag_reciprocal_math"
  {
    rtx two = gcn_vec_constant (<MODE>mode,
		  const_double_from_real_value (dconst2, <SCALAR_MODE>mode));
    rtx initrcp = gen_reg_rtx (<MODE>mode);
    rtx fma = gen_reg_rtx (<MODE>mode);
    rtx rcp;

    bool is_rcp = (GET_CODE (operands[1]) == CONST_VECTOR
		   && real_identical
		        (CONST_DOUBLE_REAL_VALUE
			  (CONST_VECTOR_ELT (operands[1], 0)), &dconstm1));

    if (is_rcp)
      rcp = operands[0];
    else
      rcp = gen_reg_rtx (<MODE>mode);

    emit_insn (gen_recip<mode>2 (initrcp, operands[2]));
    emit_insn (gen_fma<mode>4_negop2 (fma, initrcp, operands[2], two));
    emit_insn (gen_mul<mode>3 (rcp, initrcp, fma));

    if (!is_rcp)
      emit_insn (gen_mul<mode>3 (operands[0], operands[1], rcp));

    DONE;
  })

(define_expand "div<mode>3"
  [(match_operand:FP 0 "gcn_valu_dst_operand")
   (match_operand:FP 1 "gcn_valu_src0_operand")
   (match_operand:FP 2 "gcn_valu_src0_operand")]
  "flag_reciprocal_math"
  {
    rtx two = const_double_from_real_value (dconst2, <MODE>mode);
    rtx initrcp = gen_reg_rtx (<MODE>mode);
    rtx fma = gen_reg_rtx (<MODE>mode);
    rtx rcp;

    bool is_rcp = (GET_CODE (operands[1]) == CONST_DOUBLE
		   && real_identical (CONST_DOUBLE_REAL_VALUE (operands[1]),
				      &dconstm1));

    if (is_rcp)
      rcp = operands[0];
    else
      rcp = gen_reg_rtx (<MODE>mode);

    emit_insn (gen_recip<mode>2 (initrcp, operands[2]));
    emit_insn (gen_fma<mode>4_negop2 (fma, initrcp, operands[2], two));
    emit_insn (gen_mul<mode>3 (rcp, initrcp, fma));

    if (!is_rcp)
      emit_insn (gen_mul<mode>3 (operands[0], operands[1], rcp));

    DONE;
  })

;; }}}
;; {{{ Int/FP conversions

(define_mode_iterator CVT_FROM_MODE [HI SI HF SF DF])
(define_mode_iterator CVT_TO_MODE [HI SI HF SF DF])

(define_mode_iterator VCVT_MODE [V64HI V64SI V64HF V64SF V64DF])
(define_mode_iterator VCVT_FMODE [V64HF V64SF V64DF])
(define_mode_iterator VCVT_IMODE [V64HI V64SI])

(define_code_iterator cvt_op [fix unsigned_fix
			      float unsigned_float
			      float_extend float_truncate])
(define_code_attr cvt_name [(fix "fix_trunc") (unsigned_fix "fixuns_trunc")
			    (float "float") (unsigned_float "floatuns")
			    (float_extend "extend") (float_truncate "trunc")])
(define_code_attr cvt_operands [(fix "%i0%i1") (unsigned_fix "%u0%i1")
				(float "%i0%i1") (unsigned_float "%i0%u1")
				(float_extend "%i0%i1")
				(float_truncate "%i0%i1")])

(define_insn "<cvt_name><CVT_FROM_MODE:mode><CVT_TO_MODE:mode>2"
  [(set (match_operand:CVT_TO_MODE 0 "register_operand"	   "=  v")
	(cvt_op:CVT_TO_MODE
	  (match_operand:CVT_FROM_MODE 1 "gcn_alu_operand" "vSvB")))]
  "gcn_valid_cvt_p (<CVT_FROM_MODE:MODE>mode, <CVT_TO_MODE:MODE>mode,
		    <cvt_name>_cvt)"
  "v_cvt<cvt_operands>\t%0, %1"
  [(set_attr "type" "vop1")
   (set_attr "length" "8")])

(define_insn "<cvt_name><VCVT_MODE:mode><VCVT_FMODE:mode>2<exec>"
  [(set (match_operand:VCVT_FMODE 0 "register_operand" "=  v")
	(cvt_op:VCVT_FMODE
	  (match_operand:VCVT_MODE 1 "gcn_alu_operand" "vSvB")))]
  "gcn_valid_cvt_p (<VCVT_MODE:MODE>mode, <VCVT_FMODE:MODE>mode,
		    <cvt_name>_cvt)"
  "v_cvt<cvt_operands>\t%0, %1"
  [(set_attr "type" "vop1")
   (set_attr "length" "8")])

(define_insn "<cvt_name><VCVT_FMODE:mode><VCVT_IMODE:mode>2<exec>"
  [(set (match_operand:VCVT_IMODE 0 "register_operand"  "=  v")
	(cvt_op:VCVT_IMODE
	  (match_operand:VCVT_FMODE 1 "gcn_alu_operand" "vSvB")))]
  "gcn_valid_cvt_p (<VCVT_FMODE:MODE>mode, <VCVT_IMODE:MODE>mode,
		    <cvt_name>_cvt)"
  "v_cvt<cvt_operands>\t%0, %1"
  [(set_attr "type" "vop1")
   (set_attr "length" "8")])

;; }}}
;; {{{ Int/int conversions

(define_code_iterator zero_convert [truncate zero_extend])
(define_code_attr convop [
	(sign_extend "extend")
	(zero_extend "zero_extend")
	(truncate "trunc")])

(define_insn "<convop><V_INT_1REG_ALT:mode><V_INT_1REG:mode>2<exec>"
  [(set (match_operand:V_INT_1REG 0 "register_operand"      "=v")
        (zero_convert:V_INT_1REG
	  (match_operand:V_INT_1REG_ALT 1 "gcn_alu_operand" " v")))]
  ""
  "v_mov_b32_sdwa\t%0, %1 dst_sel:<V_INT_1REG:sdwa> dst_unused:UNUSED_PAD src0_sel:<V_INT_1REG_ALT:sdwa>"
  [(set_attr "type" "vop_sdwa")
   (set_attr "length" "8")])

(define_insn "extend<V_INT_1REG_ALT:mode><V_INT_1REG:mode>2<exec>"
  [(set (match_operand:V_INT_1REG 0 "register_operand"	    "=v")
        (sign_extend:V_INT_1REG
	  (match_operand:V_INT_1REG_ALT 1 "gcn_alu_operand" " v")))]
  ""
  "v_mov_b32_sdwa\t%0, sext(%1) src0_sel:<V_INT_1REG_ALT:sdwa>"
  [(set_attr "type" "vop_sdwa")
   (set_attr "length" "8")])

;; GCC can already do these for scalar types, but not for vector types.
;; Unfortunately you can't just do SUBREG on a vector to select the low part,
;; so there must be a few tricks here.

(define_insn_and_split "trunc<vndi><mode>2"
  [(set (match_operand:V_INT_1REG 0 "register_operand" "=v")
	(truncate:V_INT_1REG
	  (match_operand:<VnDI> 1 "gcn_alu_operand"     " v")))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
  {
    rtx inlo = gcn_operand_part (<VnDI>mode, operands[1], 0);
    rtx out = operands[0];

    if (<MODE>mode != <VnSI>mode)
      emit_insn (gen_trunc<vnsi><mode>2 (out, inlo));
    else
      emit_move_insn (out, inlo);
  }
  [(set_attr "type" "vop2")
   (set_attr "length" "4")])

(define_insn_and_split "trunc<vndi><mode>2_exec"
  [(set (match_operand:V_INT_1REG 0 "register_operand"		  "=v")
	(vec_merge:V_INT_1REG
	  (truncate:V_INT_1REG
	    (match_operand:<VnDI> 1 "gcn_alu_operand"		  " v"))
	  (match_operand:V_INT_1REG 2 "gcn_alu_or_unspec_operand" "U0")
	  (match_operand:DI 3 "gcn_exec_operand"		  " e")))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
  {
    rtx out = operands[0];
    rtx inlo = gcn_operand_part (<VnDI>mode, operands[1], 0);
    rtx merge = operands[2];
    rtx exec = operands[3];

    if (<MODE>mode != <VnSI>mode)
      emit_insn (gen_trunc<vnsi><mode>2_exec (out, inlo, merge, exec));
    else
      emit_insn (gen_mov<mode>_exec (out, inlo, merge, exec));
  }
  [(set_attr "type" "vop2")
   (set_attr "length" "4")])

(define_insn_and_split "<convop><mode><vndi>2"
  [(set (match_operand:<VnDI> 0 "register_operand"	"=v")
	(any_extend:<VnDI>
	  (match_operand:V_INT_1REG 1 "gcn_alu_operand" " v")))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
  {
    rtx outlo = gcn_operand_part (<VnDI>mode, operands[0], 0);
    rtx outhi = gcn_operand_part (<VnDI>mode, operands[0], 1);
    rtx in = operands[1];
      
    if (<MODE>mode != <VnSI>mode)
      emit_insn (gen_<convop><mode><vnsi>2 (outlo, in));
    else
      emit_move_insn (outlo, in);
    if ('<su>' == 's')
      emit_insn (gen_ashr<vnsi>3 (outhi, outlo, GEN_INT (31)));
    else
      emit_insn (gen_vec_duplicate<vnsi> (outhi, const0_rtx));
  }
  [(set_attr "type" "mult")
   (set_attr "length" "12")])

(define_insn_and_split "<convop><mode><vndi>2_exec"
  [(set (match_operand:<VnDI> 0 "register_operand"	     "=v")
	(vec_merge:<VnDI>
	  (any_extend:<VnDI>
	    (match_operand:V_INT_1REG 1 "gcn_alu_operand"    " v"))
	  (match_operand:<VnDI> 2 "gcn_alu_or_unspec_operand" "U0")
	  (match_operand:DI 3 "gcn_exec_operand"	     " e")))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
  {
    rtx outlo = gcn_operand_part (<VnDI>mode, operands[0], 0);
    rtx outhi = gcn_operand_part (<VnDI>mode, operands[0], 1);
    rtx in = operands[1];
    rtx mergelo = gcn_operand_part (<VnDI>mode, operands[2], 0);
    rtx mergehi = gcn_operand_part (<VnDI>mode, operands[2], 1);
    rtx exec = operands[3];
      
    if (<MODE>mode != <VnSI>mode)
      emit_insn (gen_<convop><mode><vnsi>2_exec (outlo, in, mergelo, exec));
    else
      emit_insn (gen_mov<mode>_exec (outlo, in, mergelo, exec));
    if ('<su>' == 's')
      emit_insn (gen_ashr<vnsi>3_exec (outhi, outlo, GEN_INT (31), mergehi,
				       exec));
    else
      emit_insn (gen_vec_duplicate<vnsi>_exec (outhi, const0_rtx, mergehi,
					       exec));
  }
  [(set_attr "type" "mult")
   (set_attr "length" "12")])

;; }}}
;; {{{ Vector comparison/merge

(define_insn "vec_cmp<mode>di"
  [(set (match_operand:DI 0 "register_operand"	      "=cV,cV,  e, e,Sg,Sg")
	(match_operator:DI 1 "gcn_fp_compare_operator"
	  [(match_operand:V_noQI 2 "gcn_alu_operand"  "vSv, B,vSv, B, v,vA")
	   (match_operand:V_noQI 3 "gcn_vop3_operand" "  v, v,  v, v,vA, v")]))
   (clobber (match_scratch:DI 4			      "= X, X, cV,cV, X, X"))]
  ""
  "@
   v_cmp%E1\tvcc, %2, %3
   v_cmp%E1\tvcc, %2, %3
   v_cmpx%E1\tvcc, %2, %3
   v_cmpx%E1\tvcc, %2, %3
   v_cmp%E1\t%0, %2, %3
   v_cmp%E1\t%0, %2, %3"
  [(set_attr "type" "vopc,vopc,vopc,vopc,vop3a,vop3a")
   (set_attr "length" "4,8,4,8,8,8")])

(define_expand "vec_cmpu<mode>di"
  [(match_operand:DI 0 "register_operand")
   (match_operator 1 "gcn_compare_operator"
     [(match_operand:V_INT_noQI 2 "gcn_alu_operand")
      (match_operand:V_INT_noQI 3 "gcn_vop3_operand")])]
  ""
  {
    /* Unsigned comparisons use the same patterns as signed comparisons,
       except that they use unsigned operators (e.g. LTU vs LT).
       The '%E1' directive then does the Right Thing.  */
    emit_insn (gen_vec_cmp<mode>di (operands[0], operands[1], operands[2],
				    operands[3]));
    DONE;
  })

; There's no instruction for 8-bit vector comparison, so we need to extend.
(define_expand "vec_cmp<u><mode>di"
  [(match_operand:DI 0 "register_operand")
   (match_operator 1 "gcn_compare_operator"
     [(any_extend:<VnSI> (match_operand:V_QI 2 "gcn_alu_operand"))
      (any_extend:<VnSI> (match_operand:V_QI 3 "gcn_vop3_operand"))])]
  "can_create_pseudo_p ()"
  {
    rtx sitmp1 = gen_reg_rtx (<VnSI>mode);
    rtx sitmp2 = gen_reg_rtx (<VnSI>mode);

    emit_insn (gen_<expander><mode><vnsi>2 (sitmp1, operands[2]));
    emit_insn (gen_<expander><mode><vnsi>2 (sitmp2, operands[3]));
    emit_insn (gen_vec_cmp<vnsi>di (operands[0], operands[1], sitmp1, sitmp2));
    DONE;
  })

(define_insn "vec_cmp<mode>di_exec"
  [(set (match_operand:DI 0 "register_operand"	       "=cV,cV,  e, e,Sg,Sg")
	(and:DI
	  (match_operator 1 "gcn_fp_compare_operator"
	    [(match_operand:V_noQI 2 "gcn_alu_operand" "vSv, B,vSv, B, v,vA")
	     (match_operand:V_noQI 3 "gcn_vop3_operand" " v, v,  v, v,vA, v")])
	  (match_operand:DI 4 "gcn_exec_reg_operand"   "  e, e,  e, e, e, e")))
   (clobber (match_scratch:DI 5			       "= X, X, cV,cV, X, X"))]
  ""
  "@
   v_cmp%E1\tvcc, %2, %3
   v_cmp%E1\tvcc, %2, %3
   v_cmpx%E1\tvcc, %2, %3
   v_cmpx%E1\tvcc, %2, %3
   v_cmp%E1\t%0, %2, %3
   v_cmp%E1\t%0, %2, %3"
  [(set_attr "type" "vopc,vopc,vopc,vopc,vop3a,vop3a")
   (set_attr "length" "4,8,4,8,8,8")])

(define_expand "vec_cmpu<mode>di_exec"
  [(match_operand:DI 0 "register_operand")
   (match_operator 1 "gcn_compare_operator"
     [(match_operand:V_INT_noQI 2 "gcn_alu_operand")
      (match_operand:V_INT_noQI 3 "gcn_vop3_operand")])
   (match_operand:DI 4 "gcn_exec_reg_operand")]
  ""
  {
    /* Unsigned comparisons use the same patterns as signed comparisons,
       except that they use unsigned operators (e.g. LTU vs LT).
       The '%E1' directive then does the Right Thing.  */
    emit_insn (gen_vec_cmpu<mode>di_exec (operands[0], operands[1],
					  operands[2], operands[3],
					  operands[4]));
    DONE;
  })

(define_expand "vec_cmp<u><mode>di_exec"
  [(match_operand:DI 0 "register_operand")
   (match_operator 1 "gcn_compare_operator"
     [(any_extend:<VnSI> (match_operand:V_QI 2 "gcn_alu_operand"))
      (any_extend:<VnSI> (match_operand:V_QI 3 "gcn_vop3_operand"))])
   (match_operand:DI 4 "gcn_exec_reg_operand")]
  "can_create_pseudo_p ()"
  {
    rtx sitmp1 = gen_reg_rtx (<VnSI>mode);
    rtx sitmp2 = gen_reg_rtx (<VnSI>mode);

    emit_insn (gen_<expander><mode><vnsi>2_exec (sitmp1, operands[2],
						 operands[2], operands[4]));
    emit_insn (gen_<expander><mode><vnsi>2_exec (sitmp2, operands[3],
						 operands[3], operands[4]));
    emit_insn (gen_vec_cmp<vnsi>di_exec (operands[0], operands[1], sitmp1,
					 sitmp2, operands[4]));
    DONE;
  })

(define_insn "vec_cmp<mode>di_dup"
  [(set (match_operand:DI 0 "register_operand"		   "=cV,cV, e,e,Sg")
	(match_operator:DI 1 "gcn_fp_compare_operator"
	  [(vec_duplicate:V_noQI
	     (match_operand:<SCALAR_MODE> 2 "gcn_alu_operand"
							   " Sv, B,Sv,B, A"))
	   (match_operand:V_noQI 3 "gcn_vop3_operand"	   "  v, v, v,v, v")]))
   (clobber (match_scratch:DI 4				   "= X,X,cV,cV, X"))]
  ""
  "@
   v_cmp%E1\tvcc, %2, %3
   v_cmp%E1\tvcc, %2, %3
   v_cmpx%E1\tvcc, %2, %3
   v_cmpx%E1\tvcc, %2, %3
   v_cmp%E1\t%0, %2, %3"
  [(set_attr "type" "vopc,vopc,vopc,vopc,vop3a")
   (set_attr "length" "4,8,4,8,8")])

(define_insn "vec_cmp<mode>di_dup_exec"
  [(set (match_operand:DI 0 "register_operand"		    "=cV,cV, e,e,Sg")
	(and:DI
	  (match_operator 1 "gcn_fp_compare_operator"
	    [(vec_duplicate:V_noQI
	       (match_operand:<SCALAR_MODE> 2 "gcn_alu_operand"
							    " Sv, B,Sv,B, A"))
	     (match_operand:V_noQI 3 "gcn_vop3_operand"	    "  v, v, v,v, v")])
	  (match_operand:DI 4 "gcn_exec_reg_operand"	    "  e, e, e,e, e")))
   (clobber (match_scratch:DI 5				    "= X,X,cV,cV, X"))]
  ""
  "@
   v_cmp%E1\tvcc, %2, %3
   v_cmp%E1\tvcc, %2, %3
   v_cmpx%E1\tvcc, %2, %3
   v_cmpx%E1\tvcc, %2, %3
   v_cmp%E1\t%0, %2, %3"
  [(set_attr "type" "vopc,vopc,vopc,vopc,vop3a")
   (set_attr "length" "4,8,4,8,8")])

(define_expand "vcond_mask_<mode>di"
  [(parallel
    [(set (match_operand:V_ALL 0   "register_operand" "")
	  (vec_merge:V_ALL
	    (match_operand:V_ALL 1 "gcn_vop3_operand" "")
	    (match_operand:V_ALL 2 "gcn_alu_operand" "")
	    (match_operand:DI 3		     "register_operand" "")))
     (clobber (scratch:<VnDI>))])]
  ""
  "")

(define_expand "vcond<V_ALL:mode><V_ALL_ALT:mode>"
  [(match_operand:V_ALL 0 "register_operand")
   (match_operand:V_ALL 1 "gcn_vop3_operand")
   (match_operand:V_ALL 2 "gcn_alu_operand")
   (match_operator 3 "gcn_fp_compare_operator"
     [(match_operand:V_ALL_ALT 4 "gcn_alu_operand")
      (match_operand:V_ALL_ALT 5 "gcn_vop3_operand")])]
  ""
  {
    rtx tmp = gen_reg_rtx (DImode);
    emit_insn (gen_vec_cmp<V_ALL_ALT:mode>di
	       (tmp, operands[3], operands[4], operands[5]));
    emit_insn (gen_vcond_mask_<V_ALL:mode>di
	       (operands[0], operands[1], operands[2], tmp));
    DONE;
  })

(define_expand "vcond<V_ALL:mode><V_ALL_ALT:mode>_exec"
  [(match_operand:V_ALL 0 "register_operand")
   (match_operand:V_ALL 1 "gcn_vop3_operand")
   (match_operand:V_ALL 2 "gcn_alu_operand")
   (match_operator 3 "gcn_fp_compare_operator"
     [(match_operand:V_ALL_ALT 4 "gcn_alu_operand")
      (match_operand:V_ALL_ALT 5 "gcn_vop3_operand")])
   (match_operand:DI 6 "gcn_exec_reg_operand" "e")]
  ""
  {
    rtx tmp = gen_reg_rtx (DImode);
    emit_insn (gen_vec_cmp<V_ALL_ALT:mode>di_exec
	       (tmp, operands[3], operands[4], operands[5], operands[6]));
    emit_insn (gen_vcond_mask_<V_ALL:mode>di
	       (operands[0], operands[1], operands[2], tmp));
    DONE;
  })

(define_expand "vcondu<V_ALL:mode><V_INT:mode>"
  [(match_operand:V_ALL 0 "register_operand")
   (match_operand:V_ALL 1 "gcn_vop3_operand")
   (match_operand:V_ALL 2 "gcn_alu_operand")
   (match_operator 3 "gcn_fp_compare_operator"
     [(match_operand:V_INT 4 "gcn_alu_operand")
      (match_operand:V_INT 5 "gcn_vop3_operand")])]
  ""
  {
    rtx tmp = gen_reg_rtx (DImode);
    emit_insn (gen_vec_cmpu<V_INT:mode>di
	       (tmp, operands[3], operands[4], operands[5]));
    emit_insn (gen_vcond_mask_<V_ALL:mode>di
	       (operands[0], operands[1], operands[2], tmp));
    DONE;
  })

(define_expand "vcondu<V_ALL:mode><V_INT:mode>_exec"
  [(match_operand:V_ALL 0 "register_operand")
   (match_operand:V_ALL 1 "gcn_vop3_operand")
   (match_operand:V_ALL 2 "gcn_alu_operand")
   (match_operator 3 "gcn_fp_compare_operator"
     [(match_operand:V_INT 4 "gcn_alu_operand")
      (match_operand:V_INT 5 "gcn_vop3_operand")])
   (match_operand:DI 6 "gcn_exec_reg_operand" "e")]
  ""
  {
    rtx tmp = gen_reg_rtx (DImode);
    emit_insn (gen_vec_cmpu<V_INT:mode>di_exec
	       (tmp, operands[3], operands[4], operands[5], operands[6]));
    emit_insn (gen_vcond_mask_<V_ALL:mode>di
	       (operands[0], operands[1], operands[2], tmp));
    DONE;
  })

;; }}}
;; {{{ Fully masked loop support

(define_expand "while_ultsidi"
  [(match_operand:DI 0 "register_operand")
   (match_operand:SI 1 "")
   (match_operand:SI 2 "")]
  ""
  {
    if (GET_CODE (operands[1]) != CONST_INT
	|| GET_CODE (operands[2]) != CONST_INT)
      {
	rtx _0_1_2_3 = gen_rtx_REG (V64SImode, VGPR_REGNO (1));
	rtx tmp = _0_1_2_3;
	if (GET_CODE (operands[1]) != CONST_INT
	    || INTVAL (operands[1]) != 0)
	  {
	    tmp = gen_reg_rtx (V64SImode);
	    emit_insn (gen_addv64si3_dup (tmp, _0_1_2_3, operands[1]));
	  }
	emit_insn (gen_vec_cmpv64sidi_dup (operands[0],
					   gen_rtx_GT (VOIDmode, 0, 0),
					   operands[2], tmp));
      }
    else
      {
	HOST_WIDE_INT diff = INTVAL (operands[2]) - INTVAL (operands[1]);
	HOST_WIDE_INT mask = (diff >= 64 ? -1
			      : ~((unsigned HOST_WIDE_INT)-1 << diff));
	emit_move_insn (operands[0], gen_rtx_CONST_INT (VOIDmode, mask));
      }
    DONE;
  })

(define_expand "maskload<mode>di"
  [(match_operand:V_ALL 0 "register_operand")
   (match_operand:V_ALL 1 "memory_operand")
   (match_operand 2 "")]
  ""
  {
    rtx exec = force_reg (DImode, operands[2]);
    rtx addr = gcn_expand_scalar_to_vector_address
		(<MODE>mode, exec, operands[1], gen_rtx_SCRATCH (<VnDI>mode));
    rtx as = gen_rtx_CONST_INT (VOIDmode, MEM_ADDR_SPACE (operands[1]));
    rtx v = gen_rtx_CONST_INT (VOIDmode, MEM_VOLATILE_P (operands[1]));

    /* Masked lanes are required to hold zero.  */
    emit_move_insn (operands[0], gcn_vec_constant (<MODE>mode, 0));

    emit_insn (gen_gather<mode>_expr_exec (operands[0], addr, as, v,
					   operands[0], exec));
    DONE;
  })

(define_expand "maskstore<mode>di"
  [(match_operand:V_ALL 0 "memory_operand")
   (match_operand:V_ALL 1 "register_operand")
   (match_operand 2 "")]
  ""
  {
    rtx exec = force_reg (DImode, operands[2]);
    rtx addr = gcn_expand_scalar_to_vector_address
		(<MODE>mode, exec, operands[0], gen_rtx_SCRATCH (<VnDI>mode));
    rtx as = gen_rtx_CONST_INT (VOIDmode, MEM_ADDR_SPACE (operands[0]));
    rtx v = gen_rtx_CONST_INT (VOIDmode, MEM_VOLATILE_P (operands[0]));
    emit_insn (gen_scatter<mode>_expr_exec (addr, operands[1], as, v, exec));
    DONE;
  })

(define_expand "mask_gather_load<mode><vnsi>"
  [(match_operand:V_ALL 0 "register_operand")
   (match_operand:DI 1 "register_operand")
   (match_operand:<VnSI> 2 "register_operand")
   (match_operand 3 "immediate_operand")
   (match_operand:SI 4 "gcn_alu_operand")
   (match_operand:DI 5 "")]
  ""
  {
    rtx exec = force_reg (DImode, operands[5]);

    rtx addr = gcn_expand_scaled_offsets (DEFAULT_ADDR_SPACE, operands[1],
					  operands[2], operands[4],
					  INTVAL (operands[3]), exec);

    /* Masked lanes are required to hold zero.  */
    emit_move_insn (operands[0], gcn_vec_constant (<MODE>mode, 0));

    if (GET_MODE (addr) == <VnDI>mode)
      emit_insn (gen_gather<mode>_insn_1offset_exec (operands[0], addr,
						     const0_rtx, const0_rtx,
						     const0_rtx, operands[0],
						     exec));
    else
      emit_insn (gen_gather<mode>_insn_2offsets_exec (operands[0], operands[1],
						      addr, const0_rtx,
						      const0_rtx, const0_rtx,
						      operands[0], exec));
    DONE;
  })

(define_expand "mask_scatter_store<mode><vnsi>"
  [(match_operand:DI 0 "register_operand")
   (match_operand:<VnSI> 1 "register_operand")
   (match_operand 2 "immediate_operand")
   (match_operand:SI 3 "gcn_alu_operand")
   (match_operand:V_ALL 4 "register_operand")
   (match_operand:DI 5 "")]
  ""
  {
    rtx exec = force_reg (DImode, operands[5]);

    rtx addr = gcn_expand_scaled_offsets (DEFAULT_ADDR_SPACE, operands[0],
					  operands[1], operands[3],
					  INTVAL (operands[2]), exec);

    if (GET_MODE (addr) == <VnDI>mode)
      emit_insn (gen_scatter<mode>_insn_1offset_exec (addr, const0_rtx,
						      operands[4], const0_rtx,
						      const0_rtx,
						      exec));
    else
      emit_insn (gen_scatter<mode>_insn_2offsets_exec (operands[0], addr,
						       const0_rtx, operands[4],
						       const0_rtx, const0_rtx,
						       exec));
    DONE;
  })

(define_code_iterator cond_op [plus minus mult])

(define_expand "cond_<expander><mode>"
  [(match_operand:V_ALL 0 "register_operand")
   (match_operand:DI 1 "register_operand")
   (cond_op:V_ALL
     (match_operand:V_ALL 2 "gcn_alu_operand")
     (match_operand:V_ALL 3 "gcn_alu_operand"))
   (match_operand:V_ALL 4 "register_operand")]
  ""
  {
    operands[1] = force_reg (DImode, operands[1]);
    operands[2] = force_reg (<MODE>mode, operands[2]);

    emit_insn (gen_<expander><mode>3_exec (operands[0], operands[2],
					   operands[3], operands[4],
					   operands[1]));
    DONE;
  })

;; TODO smin umin smax umax
(define_code_iterator cond_bitop [and ior xor])

(define_expand "cond_<expander><mode>"
  [(match_operand:V_INT 0 "register_operand")
   (match_operand:DI 1 "register_operand")
   (cond_bitop:V_INT
     (match_operand:V_INT 2 "gcn_alu_operand")
     (match_operand:V_INT 3 "gcn_alu_operand"))
   (match_operand:V_INT 4 "register_operand")]
  ""
  {
    operands[1] = force_reg (DImode, operands[1]);
    operands[2] = force_reg (<MODE>mode, operands[2]);

    emit_insn (gen_<expander><mode>3_exec (operands[0], operands[2],
					   operands[3], operands[4],
					   operands[1]));
    DONE;
  })

;; }}}
;; {{{ Vector reductions

(define_int_iterator REDUC_UNSPEC [UNSPEC_SMIN_DPP_SHR UNSPEC_SMAX_DPP_SHR
				   UNSPEC_UMIN_DPP_SHR UNSPEC_UMAX_DPP_SHR
				   UNSPEC_PLUS_DPP_SHR
				   UNSPEC_AND_DPP_SHR
				   UNSPEC_IOR_DPP_SHR UNSPEC_XOR_DPP_SHR])

(define_int_iterator REDUC_2REG_UNSPEC [UNSPEC_PLUS_DPP_SHR
					UNSPEC_AND_DPP_SHR
					UNSPEC_IOR_DPP_SHR UNSPEC_XOR_DPP_SHR])

; FIXME: Isn't there a better way of doing this?
(define_int_attr reduc_unspec [(UNSPEC_SMIN_DPP_SHR "UNSPEC_SMIN_DPP_SHR")
			       (UNSPEC_SMAX_DPP_SHR "UNSPEC_SMAX_DPP_SHR")
			       (UNSPEC_UMIN_DPP_SHR "UNSPEC_UMIN_DPP_SHR")
			       (UNSPEC_UMAX_DPP_SHR "UNSPEC_UMAX_DPP_SHR")
			       (UNSPEC_PLUS_DPP_SHR "UNSPEC_PLUS_DPP_SHR")
			       (UNSPEC_AND_DPP_SHR "UNSPEC_AND_DPP_SHR")
			       (UNSPEC_IOR_DPP_SHR "UNSPEC_IOR_DPP_SHR")
			       (UNSPEC_XOR_DPP_SHR "UNSPEC_XOR_DPP_SHR")])

(define_int_attr reduc_op [(UNSPEC_SMIN_DPP_SHR "smin")
			   (UNSPEC_SMAX_DPP_SHR "smax")
			   (UNSPEC_UMIN_DPP_SHR "umin")
			   (UNSPEC_UMAX_DPP_SHR "umax")
			   (UNSPEC_PLUS_DPP_SHR "plus")
			   (UNSPEC_AND_DPP_SHR "and")
			   (UNSPEC_IOR_DPP_SHR "ior")
			   (UNSPEC_XOR_DPP_SHR "xor")])

(define_int_attr reduc_insn [(UNSPEC_SMIN_DPP_SHR "v_min%i0")
			     (UNSPEC_SMAX_DPP_SHR "v_max%i0")
			     (UNSPEC_UMIN_DPP_SHR "v_min%u0")
			     (UNSPEC_UMAX_DPP_SHR "v_max%u0")
			     (UNSPEC_PLUS_DPP_SHR "v_add%U0")
			     (UNSPEC_AND_DPP_SHR  "v_and%B0")
			     (UNSPEC_IOR_DPP_SHR  "v_or%B0")
			     (UNSPEC_XOR_DPP_SHR  "v_xor%B0")])

(define_expand "reduc_<reduc_op>_scal_<mode>"
  [(set (match_operand:<SCALAR_MODE> 0 "register_operand")
	(unspec:<SCALAR_MODE>
	  [(match_operand:V_ALL 1 "register_operand")]
	  REDUC_UNSPEC))]
  ""
  {
    rtx tmp = gcn_expand_reduc_scalar (<MODE>mode, operands[1],
				       <reduc_unspec>);

    /* The result of the reduction is in lane 63 of tmp.  */
    emit_insn (gen_mov_from_lane63_<mode> (operands[0], tmp));

    DONE;
  })

;; Warning: This "-ffast-math" implementation converts in-order reductions
;;          into associative reductions. It's also used where OpenMP or
;;          OpenACC paralellization has already broken the in-order semantics.
(define_expand "fold_left_plus_<mode>"
 [(match_operand:<SCALAR_MODE> 0 "register_operand")
  (match_operand:<SCALAR_MODE> 1 "gcn_alu_operand")
  (match_operand:V_FP 2 "gcn_alu_operand")]
  "can_create_pseudo_p ()
   && (flag_openacc || flag_openmp
       || flag_associative_math)"
  {
    rtx dest = operands[0];
    rtx scalar = operands[1];
    rtx vector = operands[2];
    rtx tmp = gen_reg_rtx (<SCALAR_MODE>mode);

    emit_insn (gen_reduc_plus_scal_<mode> (tmp, vector));
    emit_insn (gen_add<scalar_mode>3 (dest, scalar, tmp));
     DONE;
   })

(define_insn "*<reduc_op>_dpp_shr_<mode>"
  [(set (match_operand:V_1REG 0 "register_operand"   "=v")
	(unspec:V_1REG
	  [(match_operand:V_1REG 1 "register_operand" "v")
	   (match_operand:V_1REG 2 "register_operand" "v")
	   (match_operand:SI 3 "const_int_operand"    "n")]
	  REDUC_UNSPEC))]
  ; GCN3 requires a carry out, GCN5 not
  "!(TARGET_GCN3 && SCALAR_INT_MODE_P (<SCALAR_MODE>mode)
     && <reduc_unspec> == UNSPEC_PLUS_DPP_SHR)"
  {
    return gcn_expand_dpp_shr_insn (<MODE>mode, "<reduc_insn>",
				    <reduc_unspec>, INTVAL (operands[3]));
  }
  [(set_attr "type" "vop_dpp")
   (set_attr "length" "8")])

(define_insn_and_split "*<reduc_op>_dpp_shr_<mode>"
  [(set (match_operand:V_DI 0 "register_operand"    "=v")
	(unspec:V_DI
	  [(match_operand:V_DI 1 "register_operand" "v")
	   (match_operand:V_DI 2 "register_operand" "v")
	   (match_operand:SI 3 "const_int_operand"  "n")]
	  REDUC_2REG_UNSPEC))]
  ""
  "#"
  "reload_completed"
  [(set (match_dup 4)
	(unspec:<VnSI>
	  [(match_dup 6) (match_dup 8) (match_dup 3)] REDUC_2REG_UNSPEC))
   (set (match_dup 5)
	(unspec:<VnSI>
	  [(match_dup 7) (match_dup 9) (match_dup 3)] REDUC_2REG_UNSPEC))]
  {
    operands[4] = gcn_operand_part (<MODE>mode, operands[0], 0);
    operands[5] = gcn_operand_part (<MODE>mode, operands[0], 1);
    operands[6] = gcn_operand_part (<MODE>mode, operands[1], 0);
    operands[7] = gcn_operand_part (<MODE>mode, operands[1], 1);
    operands[8] = gcn_operand_part (<MODE>mode, operands[2], 0);
    operands[9] = gcn_operand_part (<MODE>mode, operands[2], 1);
  }
  [(set_attr "type" "vmult")
   (set_attr "length" "16")])

; Special cases for addition.

(define_insn "*plus_carry_dpp_shr_<mode>"
  [(set (match_operand:V_INT_1REG 0 "register_operand"   "=v")
	(unspec:V_INT_1REG
	  [(match_operand:V_INT_1REG 1 "register_operand" "v")
	   (match_operand:V_INT_1REG 2 "register_operand" "v")
	   (match_operand:SI 3 "const_int_operand"	  "n")]
	  UNSPEC_PLUS_CARRY_DPP_SHR))
   (clobber (reg:DI VCC_REG))]
  ""
  {
    return gcn_expand_dpp_shr_insn (<VnSI>mode, "v_add%^_u32",
				    UNSPEC_PLUS_CARRY_DPP_SHR,
				    INTVAL (operands[3]));
  }
  [(set_attr "type" "vop_dpp")
   (set_attr "length" "8")])

(define_insn "*plus_carry_in_dpp_shr_<mode>"
  [(set (match_operand:V_SI 0 "register_operand"    "=v")
	(unspec:V_SI
	  [(match_operand:V_SI 1 "register_operand" "v")
	   (match_operand:V_SI 2 "register_operand" "v")
	   (match_operand:SI 3 "const_int_operand"  "n")
	   (match_operand:DI 4 "register_operand"   "cV")]
	  UNSPEC_PLUS_CARRY_IN_DPP_SHR))
   (clobber (reg:DI VCC_REG))]
  ""
  {
    return gcn_expand_dpp_shr_insn (<MODE>mode, "v_addc%^_u32",
				    UNSPEC_PLUS_CARRY_IN_DPP_SHR,
				    INTVAL (operands[3]));
  }
  [(set_attr "type" "vop_dpp")
   (set_attr "length" "8")])

(define_insn_and_split "*plus_carry_dpp_shr_<mode>"
  [(set (match_operand:V_DI 0 "register_operand"    "=v")
	(unspec:V_DI
	  [(match_operand:V_DI 1 "register_operand" "v")
	   (match_operand:V_DI 2 "register_operand" "v")
	   (match_operand:SI 3 "const_int_operand"  "n")]
	  UNSPEC_PLUS_CARRY_DPP_SHR))
   (clobber (reg:DI VCC_REG))]
  ""
  "#"
  "reload_completed"
  [(parallel [(set (match_dup 4)
		(unspec:<VnSI>
		  [(match_dup 6) (match_dup 8) (match_dup 3)]
		  UNSPEC_PLUS_CARRY_DPP_SHR))
	      (clobber (reg:DI VCC_REG))])
   (parallel [(set (match_dup 5)
		(unspec:<VnSI>
		  [(match_dup 7) (match_dup 9) (match_dup 3) (reg:DI VCC_REG)]
		  UNSPEC_PLUS_CARRY_IN_DPP_SHR))
	      (clobber (reg:DI VCC_REG))])]
  {
    operands[4] = gcn_operand_part (<MODE>mode, operands[0], 0);
    operands[5] = gcn_operand_part (<MODE>mode, operands[0], 1);
    operands[6] = gcn_operand_part (<MODE>mode, operands[1], 0);
    operands[7] = gcn_operand_part (<MODE>mode, operands[1], 1);
    operands[8] = gcn_operand_part (<MODE>mode, operands[2], 0);
    operands[9] = gcn_operand_part (<MODE>mode, operands[2], 1);
  }
  [(set_attr "type" "vmult")
   (set_attr "length" "16")])

; Instructions to move a scalar value from lane 63 of a vector register.
(define_insn "mov_from_lane63_<mode>"
  [(set (match_operand:<SCALAR_MODE> 0 "register_operand" "=Sg,v")
	(unspec:<SCALAR_MODE>
	  [(match_operand:V_1REG 1 "register_operand"	  "  v,v")]
	  UNSPEC_MOV_FROM_LANE63))]
  ""
  "@
   v_readlane_b32\t%0, %1, 63
   v_mov_b32\t%0, %1 wave_ror:1"
  [(set_attr "type" "vop3a,vop_dpp")
   (set_attr "exec" "none,*")
   (set_attr "length" "8")])

(define_insn "mov_from_lane63_<mode>"
  [(set (match_operand:<SCALAR_MODE> 0 "register_operand" "=Sg,v")
	(unspec:<SCALAR_MODE>
	  [(match_operand:V_2REG 1 "register_operand"	  "  v,v")]
	  UNSPEC_MOV_FROM_LANE63))]
  ""
  "@
   v_readlane_b32\t%L0, %L1, 63\;v_readlane_b32\t%H0, %H1, 63
   * if (REGNO (operands[0]) <= REGNO (operands[1]))	\
       return \"v_mov_b32\t%L0, %L1 wave_ror:1\;\"	\
	      \"v_mov_b32\t%H0, %H1 wave_ror:1\";	\
     else						\
       return \"v_mov_b32\t%H0, %H1 wave_ror:1\;\"	\
	      \"v_mov_b32\t%L0, %L1 wave_ror:1\";"
  [(set_attr "type" "vop3a,vop_dpp")
   (set_attr "exec" "none,*")
   (set_attr "length" "8")])

;; }}}
;; {{{ Miscellaneous

(define_expand "vec_series<mode>"
  [(match_operand:V_SI 0 "register_operand")
   (match_operand:SI 1 "gcn_alu_operand")
   (match_operand:SI 2 "gcn_alu_operand")]
  ""
  {
    rtx tmp = gen_reg_rtx (<MODE>mode);
    rtx v1 = gen_rtx_REG (<MODE>mode, VGPR_REGNO (1));

    emit_insn (gen_mul<mode>3_dup (tmp, v1, operands[2]));
    emit_insn (gen_add<mode>3_dup (operands[0], tmp, operands[1]));
    DONE;
  })

(define_expand "vec_series<mode>"
  [(match_operand:V_DI 0 "register_operand")
   (match_operand:DI 1 "gcn_alu_operand")
   (match_operand:DI 2 "gcn_alu_operand")]
  ""
  {
    rtx tmp = gen_reg_rtx (<MODE>mode);
    rtx v1 = gen_rtx_REG (<VnSI>mode, VGPR_REGNO (1));
    rtx op1vec = gen_reg_rtx (<MODE>mode);

    emit_insn (gen_mul<mode>3_zext_dup2 (tmp, v1, operands[2]));
    emit_insn (gen_vec_duplicate<mode> (op1vec, operands[1]));
    emit_insn (gen_add<mode>3 (operands[0], tmp, op1vec));
    DONE;
  })

;; }}}
