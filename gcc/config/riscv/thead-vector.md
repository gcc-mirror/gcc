(define_c_enum "unspec" [
  UNSPEC_TH_VWLDST
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
	   (match_operand 2 "vector_length_operand"   " rK, rK, rK")
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
	   (match_operand 2 "vector_length_operand"   " rK, rK, rK")
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
	     (match_operand 4 "vector_length_operand"            " rK,  rK,  rK,  rK,  rK")
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
