(define_c_enum "unspec" [
  UNSPEC_TH_VLB
  UNSPEC_TH_VLBU
  UNSPEC_TH_VLH
  UNSPEC_TH_VLHU
  UNSPEC_TH_VLW
  UNSPEC_TH_VLWU

  UNSPEC_TH_VLSB
  UNSPEC_TH_VLSBU
  UNSPEC_TH_VLSH
  UNSPEC_TH_VLSHU
  UNSPEC_TH_VLSW
  UNSPEC_TH_VLSWU

  UNSPEC_TH_VLXB
  UNSPEC_TH_VLXBU
  UNSPEC_TH_VLXH
  UNSPEC_TH_VLXHU
  UNSPEC_TH_VLXW
  UNSPEC_TH_VLXWU

  UNSPEC_TH_VSUXB
  UNSPEC_TH_VSUXH
  UNSPEC_TH_VSUXW

  UNSPEC_TH_VWLDST
])

(define_int_iterator UNSPEC_TH_VLMEM_OP [
  UNSPEC_TH_VLB UNSPEC_TH_VLBU
  UNSPEC_TH_VLH UNSPEC_TH_VLHU
  UNSPEC_TH_VLW UNSPEC_TH_VLWU
])

(define_int_iterator UNSPEC_TH_VLSMEM_OP [
  UNSPEC_TH_VLSB UNSPEC_TH_VLSBU
  UNSPEC_TH_VLSH UNSPEC_TH_VLSHU
  UNSPEC_TH_VLSW UNSPEC_TH_VLSWU
])

(define_int_iterator UNSPEC_TH_VLXMEM_OP [
  UNSPEC_TH_VLXB UNSPEC_TH_VLXBU
  UNSPEC_TH_VLXH UNSPEC_TH_VLXHU
  UNSPEC_TH_VLXW UNSPEC_TH_VLXWU
])

(define_int_attr vlmem_op_attr [
  (UNSPEC_TH_VLB "b") (UNSPEC_TH_VLBU "bu")
  (UNSPEC_TH_VLH "h") (UNSPEC_TH_VLHU "hu")
  (UNSPEC_TH_VLW "w") (UNSPEC_TH_VLWU "wu")
  (UNSPEC_TH_VLSB "b") (UNSPEC_TH_VLSBU "bu")
  (UNSPEC_TH_VLSH "h") (UNSPEC_TH_VLSHU "hu")
  (UNSPEC_TH_VLSW "w") (UNSPEC_TH_VLSWU "wu")
  (UNSPEC_TH_VLXB "b") (UNSPEC_TH_VLXBU "bu")
  (UNSPEC_TH_VLXH "h") (UNSPEC_TH_VLXHU "hu")
  (UNSPEC_TH_VLXW "w") (UNSPEC_TH_VLXWU "wu")
  (UNSPEC_TH_VSUXB "b")
  (UNSPEC_TH_VSUXH "h")
  (UNSPEC_TH_VSUXW "w")
])

(define_int_attr vlmem_order_attr [
  (UNSPEC_TH_VLXB "")
  (UNSPEC_TH_VLXH "")
  (UNSPEC_TH_VLXW "")
  (UNSPEC_TH_VSUXB "u")
  (UNSPEC_TH_VSUXH "u")
  (UNSPEC_TH_VSUXW "u")
])

(define_int_iterator UNSPEC_TH_VSMEM_OP [
  UNSPEC_TH_VLB
  UNSPEC_TH_VLH
  UNSPEC_TH_VLW
])

(define_int_iterator UNSPEC_TH_VSSMEM_OP [
  UNSPEC_TH_VLSB
  UNSPEC_TH_VLSH
  UNSPEC_TH_VLSW
])

(define_int_iterator UNSPEC_TH_VSXMEM_OP [
  UNSPEC_TH_VLXB
  UNSPEC_TH_VLXH
  UNSPEC_TH_VLXW
  UNSPEC_TH_VSUXB
  UNSPEC_TH_VSUXH
  UNSPEC_TH_VSUXW
])

(define_mode_iterator V_VLS_VT [V VLS VT])
(define_mode_iterator V_VB_VLS_VT [V VB VLS VT])

(define_split
  [(set (match_operand:V_VB_VLS_VT 0 "reg_or_mem_operand")
	(match_operand:V_VB_VLS_VT 1 "reg_or_mem_operand"))]
  "TARGET_XTHEADVECTOR"
  [(const_int 0)]
  {
    emit_insn (gen_pred_th_whole_mov (<MODE>mode, operands[0], operands[1],
				      RVV_VLMAX, GEN_INT(riscv_vector::VLMAX)));
    DONE;
  })

(define_insn_and_split "@pred_th_whole_mov<mode>"
  [(set (match_operand:V_VLS_VT 0 "reg_or_mem_operand"  "=vr,vr, m")
	(unspec:V_VLS_VT
	  [(match_operand:V_VLS_VT 1 "reg_or_mem_operand" " vr, m,vr")
	   (match_operand 2 "vector_length_operand"   "rvl,rvl,rvl")
	   (match_operand 3 "const_1_operand"         "  i, i, i")
	   (reg:SI VL_REGNUM)
	   (reg:SI VTYPE_REGNUM)]
	UNSPEC_TH_VWLDST))]
  "TARGET_XTHEADVECTOR"
  "@
   vmv.v.v\t%0,%1
   vle.v\t%0,%1
   vse.v\t%1,%0"
  "&& REG_P (operands[0]) && REG_P (operands[1])
   && REGNO (operands[0]) == REGNO (operands[1])"
  [(const_int 0)]
  ""
  [(set_attr "type" "vimov,vlds,vlds")
   (set_attr "mode" "<MODE>")
   (set (attr "ta") (symbol_ref "riscv_vector::TAIL_UNDISTURBED"))
   (set (attr "ma") (symbol_ref "riscv_vector::MASK_UNDISTURBED"))
   (set (attr "avl_type_idx") (const_int 3))
   (set_attr "vl_op_idx" "2")])

(define_insn_and_split "@pred_th_whole_mov<mode>"
  [(set (match_operand:VB 0 "reg_or_mem_operand"  "=vr,vr, m")
	(unspec:VB
	  [(match_operand:VB 1 "reg_or_mem_operand" " vr, m,vr")
	   (match_operand 2 "vector_length_operand"   "rvl,rvl,rvl")
	   (match_operand 3 "const_1_operand"         "  i, i, i")
	   (reg:SI VL_REGNUM)
	   (reg:SI VTYPE_REGNUM)]
	UNSPEC_TH_VWLDST))]
  "TARGET_XTHEADVECTOR"
  "@
   vmv.v.v\t%0,%1
   vle.v\t%0,%1
   vse.v\t%1,%0"
  "&& REG_P (operands[0]) && REG_P (operands[1])
   && REGNO (operands[0]) == REGNO (operands[1])"
  [(const_int 0)]
  ""
  [(set_attr "type" "vimov,vlds,vlds")
   (set_attr "mode" "<MODE>")
   (set (attr "ta") (symbol_ref "riscv_vector::TAIL_UNDISTURBED"))
   (set (attr "ma") (symbol_ref "riscv_vector::MASK_UNDISTURBED"))
   (set (attr "avl_type_idx") (const_int 3))
   (set_attr "vl_op_idx" "2")
   (set (attr "sew") (const_int 8))
   (set (attr "vlmul") (symbol_ref "riscv_vector::LMUL_1"))])

(define_insn_and_split "*pred_th_mov<mode>"
  [(set (match_operand:VB_VLS 0 "nonimmediate_operand"               "=vr,   m,  vr,  vr,  vr")
	(if_then_else:VB_VLS
	  (unspec:VB_VLS
	    [(match_operand:VB_VLS 1 "vector_all_trues_mask_operand" "Wc1, Wc1, Wc1, Wc1, Wc1")
	     (match_operand 4 "vector_length_operand"            "rvl, rvl, rvl, rvl, rvl")
	     (match_operand 5 "const_int_operand"                "  i,   i,   i,   i,   i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
	  (match_operand:VB_VLS 3 "vector_move_operand"              "  m,  vr,  vr, Wc0, Wc1")
	  (match_operand:VB_VLS 2 "vector_undef_operand"             " vu,  vu,  vu,  vu,  vu")))]
  "TARGET_XTHEADVECTOR"
  "@
   #
   #
   vmcpy.m\t%0,%3
   vmclr.m\t%0
   vmset.m\t%0"
  "&& !reload_completed"
  [(const_int 0)]
  {
    if ((MEM_P (operands[0]) || MEM_P (operands[3]))
        || (REG_P (operands[0]) && REG_P (operands[3])
	    && INTVAL (operands[5]) == riscv_vector::VLMAX))
      {
	emit_move_insn (operands[0], operands[3]);
	DONE;
      }
    FAIL;
  }
  [(set_attr "type" "vldm,vstm,vmalu,vmalu,vmalu")
   (set_attr "mode" "<MODE>")])

(define_expand "@pred_mov_width<vlmem_op_attr><mode>"
  [(set (match_operand:V_VLS 0 "nonimmediate_operand")
    (if_then_else:V_VLS
      (unspec:<VM>
	[(match_operand:<VM> 1 "vector_mask_operand")
	 (match_operand 4 "vector_length_operand")
	 (match_operand 5 "const_int_operand")
	 (match_operand 6 "const_int_operand")
	 (match_operand 7 "const_int_operand")
	 (reg:SI VL_REGNUM)
	 (reg:SI VTYPE_REGNUM)] UNSPEC_TH_VLMEM_OP)
      (match_operand:V_VLS 3 "vector_move_operand")
      (match_operand:V_VLS 2 "vector_merge_operand")))]
  "TARGET_XTHEADVECTOR"
  {})

(define_insn_and_split "*pred_mov_width<vlmem_op_attr><mode>"
  [(set (match_operand:V_VLS 0 "nonimmediate_operand"	    "=vr,    vr,    vd,     m,    vr,    vr")
    (if_then_else:V_VLS
      (unspec:<VM>
	[(match_operand:<VM> 1 "vector_mask_operand"	   "vmWc1,   Wc1,    vm, vmWc1,   Wc1,   Wc1")
	 (match_operand 4 "vector_length_operand"	      "  rvl,   rvl,   rvl,   rvl,   rvl,   rvl")
	 (match_operand 5 "const_int_operand"		  "    i,     i,     i,     i,     i,     i")
	 (match_operand 6 "const_int_operand"		  "    i,     i,     i,     i,     i,     i")
	 (match_operand 7 "const_int_operand"		  "    i,     i,     i,     i,     i,     i")
	 (reg:SI VL_REGNUM)
	 (reg:SI VTYPE_REGNUM)] UNSPEC_TH_VLMEM_OP)
      (match_operand:V_VLS 3 "reg_or_mem_operand"	      "    m,     m,     m,    vr,    vr,    vr")
      (match_operand:V_VLS 2 "vector_merge_operand"	    "    0,    vu,    vu,    vu,    vu,     0")))]
  "(TARGET_XTHEADVECTOR
    && (register_operand (operands[0], <MODE>mode)
	|| register_operand (operands[3], <MODE>mode)))"
  "@
   vl<vlmem_op_attr>.v\t%0,%3%p1
   vl<vlmem_op_attr>.v\t%0,%3
   vl<vlmem_op_attr>.v\t%0,%3,%1.t
   vs<vlmem_op_attr>.v\t%3,%0%p1
   vmv.v.v\t%0,%3
   vmv.v.v\t%0,%3"
  "&& riscv_vector::whole_reg_to_reg_move_p (operands, <MODE>mode, 7)"
  [(set (match_dup 0) (match_dup 3))]
  ""
  [(set_attr "type" "vlde,vlde,vlde,vste,vimov,vimov")
   (set_attr "mode" "<MODE>")])

(define_insn "@pred_store_width<vlmem_op_attr><mode>"
  [(set (match_operand:VI 0 "memory_operand"		 "+m")
	(if_then_else:VI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" "vmWc1")
	     (match_operand 3 "vector_length_operand"    "  rvl")
	     (match_operand 4 "const_int_operand"	"    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_TH_VSMEM_OP)
	  (match_operand:VI 2 "register_operand"	 "    vr")
	  (match_dup 0)))]
  "TARGET_XTHEADVECTOR"
  "vs<vlmem_op_attr>.v\t%2,%0%p1"
  [(set_attr "type" "vste")
   (set_attr "mode" "<MODE>")
   (set (attr "avl_type_idx") (const_int 4))
   (set_attr "vl_op_idx" "3")])

(define_insn "@pred_strided_load_width<vlmem_op_attr><mode>"
  [(set (match_operand:VI 0 "register_operand"	      "=vr,    vr,    vd")
	(if_then_else:VI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" "vmWc1,   Wc1,    vm")
	     (match_operand 5 "vector_length_operand"    "  rvl,   rvl,   rvl")
	     (match_operand 6 "const_int_operand"	"    i,     i,     i")
	     (match_operand 7 "const_int_operand"	"    i,     i,     i")
	     (match_operand 8 "const_int_operand"	"    i,     i,     i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_TH_VLSMEM_OP)
	  (unspec:VI
	    [(match_operand:VI 3 "memory_operand"	 "    m,     m,     m")
	     (match_operand 4 "pmode_reg_or_0_operand"   "   rJ,    rJ,    rJ")] UNSPEC_TH_VLSMEM_OP)
	  (match_operand:VI 2 "vector_merge_operand"      "    0,    vu,    vu")))]
  "TARGET_XTHEADVECTOR"
  "vls<vlmem_op_attr>.v\t%0,%3,%z4%p1"
  [(set_attr "type" "vlds")
   (set_attr "mode" "<MODE>")])

(define_insn "@pred_strided_store_width<vlmem_op_attr><mode>"
  [(set (match_operand:VI 0 "memory_operand"		 "+m")
	(if_then_else:VI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand" "vmWc1")
	     (match_operand 4 "vector_length_operand"    "  rvl")
	     (match_operand 5 "const_int_operand"	"    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_TH_VSSMEM_OP)
	  (unspec:VI
	    [(match_operand 2 "pmode_reg_or_0_operand"   "   rJ")
	     (match_operand:VI 3 "register_operand"       "   vr")] UNSPEC_TH_VSSMEM_OP)
	  (match_dup 0)))]
  "TARGET_XTHEADVECTOR"
  "vss<vlmem_op_attr>.v\t%3,%0,%z2%p1"
  [(set_attr "type" "vsts")
   (set_attr "mode" "<MODE>")
   (set (attr "avl_type_idx") (const_int 5))])

(define_insn "@pred_indexed_load_width<vlmem_op_attr><mode>"
  [(set (match_operand:VI 0 "register_operand"	     "=vd, vr,vd, vr")
	(if_then_else:VI
	  (unspec:<VM>
	    [(match_operand:<VM> 1 "vector_mask_operand"  " vm,Wc1,vm,Wc1")
	     (match_operand 5 "vector_length_operand"     "rvl,rvl,rvl,rvl")
	     (match_operand 6 "const_int_operand"	 "  i,  i, i,  i")
	     (match_operand 7 "const_int_operand"	 "  i,  i, i,  i")
	     (match_operand 8 "const_int_operand"	 "  i,  i, i,  i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_TH_VLXMEM_OP)
	  (unspec:VI
	    [(match_operand 3 "pmode_reg_or_0_operand"    " rJ, rJ,rJ, rJ")
	     (mem:BLK (scratch))
	     (match_operand:VI 4 "register_operand" " vr, vr,vr, vr")] UNSPEC_TH_VLXMEM_OP)
	  (match_operand:VI 2 "vector_merge_operand"       " vu, vu, 0,  0")))]
  "TARGET_XTHEADVECTOR"
  "vlx<vlmem_op_attr>.v\t%0,(%z3),%4%p1"
  [(set_attr "type" "vldux")
   (set_attr "mode" "<MODE>")])

(define_insn "@pred_indexed_<vlmem_order_attr>store_width<vlmem_op_attr><mode>"
  [(set (mem:BLK (scratch))
	(unspec:BLK
	  [(unspec:<VM>
	    [(match_operand:<VM> 0 "vector_mask_operand" "vmWc1")
	     (match_operand 4 "vector_length_operand"    "  rvl")
	     (match_operand 5 "const_int_operand"	"    i")
	     (reg:SI VL_REGNUM)
	     (reg:SI VTYPE_REGNUM)] UNSPEC_TH_VSXMEM_OP)
	   (match_operand 1 "pmode_reg_or_0_operand"      "  rJ")
	   (match_operand:VI 2 "register_operand" "  vr")
	   (match_operand:VI 3 "register_operand"  "  vr")] UNSPEC_TH_VSXMEM_OP))]
  "TARGET_XTHEADVECTOR"
  "vs<vlmem_order_attr>x<vlmem_op_attr>.v\t%3,(%z1),%2%p0"
  [(set_attr "type" "vstux")
   (set_attr "mode" "<MODE>")])

(define_expand "@pred_th_extract<mode>"
  [(set (match_operand:<VEL> 0 "register_operand")
	(unspec:<VEL>
	  [(vec_select:<VEL>
	     (match_operand:V_VLSI 1 "register_operand")
	     (parallel [(match_operand:DI 2 "register_operand" "r")]))
	   (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE))]
  "TARGET_XTHEADVECTOR"
{})

(define_insn "*pred_th_extract<mode>"
  [(set (match_operand:<VEL> 0 "register_operand"   "=r")
  (unspec:<VEL>
    [(vec_select:<VEL>
       (match_operand:V_VLSI 1 "register_operand" "vr")
       (parallel [(match_operand:DI 2 "register_operand" "r")]))
     (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE))]
  "TARGET_XTHEADVECTOR"
  "vext.x.v\t%0,%1,%2"
  [(set_attr "type" "vimovvx")
   (set_attr "mode" "<MODE>")])
