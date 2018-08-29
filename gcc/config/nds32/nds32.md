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

;; See file "rtl.def" for documentation on define_insn, match_*, et. al.

;; Include predicates definition.
(include "predicates.md")

;; Include constraints definition.
(include "constraints.md")

;; Include iterators definition.
(include "iterators.md")

;; Include pipelines definition.
(include "pipelines.md")


;; Include constants definition.
(include "constants.md")


;; Include intrinsic functions definition.
(include "nds32-intrinsic.md")

;; Include block move for nds32 multiple load/store behavior.
(include "nds32-multiple.md")

;; Include DImode/DFmode operations.
(include "nds32-doubleword.md")

;; Include floating-point patterns.
(include "nds32-fpu.md")

;; Include peephole patterns.
(include "nds32-peephole2.md")


;; ------------------------------------------------------------------------

;; CPU pipeline model.
(define_attr "pipeline_model" "n7,n8,e8,n9,n10,graywolf,n13,simple"
  (const
    (cond [(match_test "nds32_cpu_option == CPU_N7")  (const_string "n7")
	   (match_test "nds32_cpu_option == CPU_E8")  (const_string "e8")
	   (match_test "nds32_cpu_option == CPU_N6 || nds32_cpu_option == CPU_N8")  (const_string "n8")
	   (match_test "nds32_cpu_option == CPU_N9")  (const_string "n9")
	   (match_test "nds32_cpu_option == CPU_N10") (const_string "n10")
	   (match_test "nds32_cpu_option == CPU_GRAYWOLF") (const_string "graywolf")
	   (match_test "nds32_cpu_option == CPU_N12") (const_string "n13")
	   (match_test "nds32_cpu_option == CPU_N13") (const_string "n13")
	   (match_test "nds32_cpu_option == CPU_SIMPLE") (const_string "simple")]
	  (const_string "n9"))))

;; Insn type, it is used to default other attribute values.
(define_attr "type"
  "unknown,load,store,load_multiple,store_multiple,alu,alu_shift,pbsad,pbsada,mul,mac,div,branch,mmu,misc,\
   falu,fmuls,fmuld,fmacs,fmacd,fdivs,fdivd,fsqrts,fsqrtd,fcmp,fabs,fcpy,fcmov,fmfsr,fmfdr,fmtsr,fmtdr,fload,fstore,\
   dalu,dalu64,daluround,dcmp,dclip,dmul,dmac,dinsb,dpack,dbpick,dwext"
  (const_string "unknown"))

;; Insn sub-type
(define_attr "subtype"
  "simple,shift,saturation"
  (const_string "simple"))

;; Length, in bytes, default is 4-bytes.
(define_attr "length" "" (const_int 4))

;; Indicate the amount of micro instructions.
(define_attr "combo"
  "0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25"
  (const_string "1"))

;; Insn in which feature set, it is used to enable/disable insn alternatives.
;; v1  : Baseline Instructions
;; v2  : Baseline Version 2 Instructions
;; v3m : Baseline Version 3m Instructions
;; v3  : Baseline Version 3 Instructions
;; pe1 : Performance Extension Instructions
;; pe2 : Performance Extension Version 2 Instructions
;; se  : String Extension instructions
(define_attr "feature"
  "v1,v2,v3m,v3,pe1,pe2,se,fpu"
  (const_string "v1"))

;; Enabled, which is used to enable/disable insn alternatives.
;; Note that we use length and TARGET_16_BIT here as criteria.
;; If the instruction pattern already check TARGET_16_BIT to determine
;; the length by itself, its enabled attribute should be customized to
;; avoid the conflict between length attribute and this default setting.
(define_attr "enabled" "no,yes"
  (if_then_else
    (and (eq_attr "length" "2")
	 (match_test "!TARGET_16_BIT"))
    (const_string "no")
    (cond [(eq_attr "feature" "v1")   (const_string "yes")
	   (eq_attr "feature" "v2")   (if_then_else (match_test "TARGET_ISA_V2 || TARGET_ISA_V3 || TARGET_ISA_V3M")
						    (const_string "yes")
						    (const_string "no"))
	   (eq_attr "feature" "v3")   (if_then_else (match_test "TARGET_ISA_V3")
						    (const_string "yes")
						    (const_string "no"))
	   (eq_attr "feature" "v3m")  (if_then_else (match_test "TARGET_ISA_V3 || TARGET_ISA_V3M")
						    (const_string "yes")
						    (const_string "no"))
	   (eq_attr "feature" "pe1")  (if_then_else (match_test "TARGET_EXT_PERF")
						    (const_string "yes")
						    (const_string "no"))
	   (eq_attr "feature" "pe2")  (if_then_else (match_test "TARGET_EXT_PERF2")
						    (const_string "yes")
						    (const_string "no"))
	   (eq_attr "feature" "se")   (if_then_else (match_test "TARGET_EXT_STRING")
						    (const_string "yes")
						    (const_string "no"))
	   (eq_attr "feature" "fpu")  (if_then_else (match_test "TARGET_FPU_SINGLE || TARGET_FPU_DOUBLE")
						    (const_string "yes")
						    (const_string "no"))]
	   (const_string "yes"))))


;; ----------------------------------------------------------------------------

(include "nds32-dspext.md")

;; Move instructions.

;; For QImode and HImode, the immediate value can be fit in imm20s.
;; So there is no need to split rtx for QI and HI patterns.

(define_expand "mov<mode>"
  [(set (match_operand:QIHI 0 "general_operand" "")
	(match_operand:QIHI 1 "general_operand" ""))]
  ""
{
  /* Need to force register if mem <- !reg.  */
  if (MEM_P (operands[0]) && !REG_P (operands[1]))
    operands[1] = force_reg (<MODE>mode, operands[1]);

  if (MEM_P (operands[1]) && optimize > 0)
    {
      rtx reg = gen_reg_rtx (SImode);

      emit_insn (gen_zero_extend<mode>si2 (reg, operands[1]));
      operands[1] = gen_lowpart (<MODE>mode, reg);
    }
})

(define_expand "movmisalign<mode>"
  [(set (match_operand:SIDI 0 "general_operand" "")
	(match_operand:SIDI 1 "general_operand" ""))]
  ""
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

(define_expand "movsi"
  [(set (match_operand:SI 0 "general_operand" "")
	(match_operand:SI 1 "general_operand" ""))]
  ""
{
  /* Need to force register if mem <- !reg.  */
  if (MEM_P (operands[0]) && !REG_P (operands[1]))
    operands[1] = force_reg (SImode, operands[1]);

  /* If operands[1] is a large constant and cannot be performed
     by a single instruction, we need to split it.  */
  if (CONST_INT_P (operands[1])
      && !satisfies_constraint_Is20 (operands[1])
      && !satisfies_constraint_Ihig (operands[1]))
    {
      rtx high20_rtx;
      HOST_WIDE_INT low12_int;
      rtx tmp_rtx;

      tmp_rtx = can_create_pseudo_p () ? gen_reg_rtx (SImode) : operands[0];

      high20_rtx = gen_int_mode ((INTVAL (operands[1]) >> 12) << 12, SImode);
      low12_int = INTVAL (operands[1]) & 0xfff;

      emit_move_insn (tmp_rtx, high20_rtx);
      emit_move_insn (operands[0], plus_constant (SImode,
						  tmp_rtx,
						  low12_int));
      DONE;
    }

  if ((REG_P (operands[0]) || GET_CODE (operands[0]) == SUBREG)
       && SYMBOLIC_CONST_P (operands[1]))
    {
      if (TARGET_ICT_MODEL_LARGE
	  && nds32_indirect_call_referenced_p (operands[1]))
	{
	  nds32_expand_ict_move (operands);
	  DONE;
	}
      else if (nds32_tls_referenced_p (operands [1]))
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
  [(set (match_operand:QIHISI 0 "nonimmediate_operand" "=r, r,U45,U33,U37,U45, m,  l,  l,  l,  d,  d, r,   d,    r,    r,    r, *f, *f,  r, *f,  Q")
	(match_operand:QIHISI 1 "nds32_move_operand"   " r, r,  l,  l,  l,  d, r,U45,U33,U37,U45,Ufe, m,Ip05, Is05, Is20, Ihig, *f,  r, *f,  Q, *f"))]
  "register_operand(operands[0], <MODE>mode)
   || register_operand(operands[1], <MODE>mode)"
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


;; We use nds32_symbolic_operand to limit that only CONST/SYMBOL_REF/LABEL_REF
;; are able to match such instruction template.
(define_insn "move_addr"
  [(set (match_operand:SI 0 "nds32_general_register_operand"   "=l, r")
	(match_operand:SI 1 "nds32_nonunspec_symbolic_operand" " i, i"))]
  ""
  "la\t%0, %1"
  [(set_attr "type"  "alu")
   (set_attr "length"  "8")])


(define_insn "sethi"
  [(set (match_operand:SI 0 "register_operand"                "=r")
	(high:SI (match_operand:SI 1 "nds32_symbolic_operand" " i")))]
  ""
  "sethi\t%0, hi20(%1)"
  [(set_attr "type" "alu")
   (set_attr "length" "4")])


(define_insn "lo_sum"
  [(set (match_operand:SI 0 "register_operand"                  "=r")
	(lo_sum:SI (match_operand:SI 1 "register_operand"       " r")
		   (match_operand:SI 2 "nds32_symbolic_operand" " i")))]
  ""
  "ori\t%0, %1, lo12(%2)"
  [(set_attr "type" "alu")
   (set_attr "length" "4")])


;; ----------------------------------------------------------------------------

;; Zero extension instructions.

(define_insn "zero_extend<mode>si2"
  [(set (match_operand:SI 0 "register_operand"                       "=l, r,   l, *r")
	(zero_extend:SI (match_operand:QIHI 1 "nonimmediate_operand" " l, r, U33,  m")))]
  ""
{
  switch (which_alternative)
    {
    case 0:
      return "ze<size>33\t%0, %1";
    case 1:
      return "ze<size>\t%0, %1";
    case 2:
      return nds32_output_16bit_load (operands, <byte>);
    case 3:
      return nds32_output_32bit_load (operands, <byte>);

    default:
      gcc_unreachable ();
    }
}
  [(set_attr "type"   "alu,alu,load,load")
   (set_attr "length" "  2,  4,   2,   4")])


;; Sign extension instructions.

(define_insn "extend<mode>si2"
  [(set (match_operand:SI 0 "register_operand"                       "=l, r, r")
	(sign_extend:SI (match_operand:QIHI 1 "nonimmediate_operand" " l, r, m")))]
  ""
{
  switch (which_alternative)
    {
    case 0:
      return "se<size>33\t%0, %1";
    case 1:
      return "se<size>\t%0, %1";
    case 2:
      return nds32_output_32bit_load_s (operands, <byte>);

    default:
      gcc_unreachable ();
    }
}
  [(set_attr "type"   "alu,alu,load")
   (set_attr "length" "  2,  4,   4")])


;; ----------------------------------------------------------------------------
(define_expand "extv"
  [(set (match_operand 0 "register_operand" "")
        (sign_extract (match_operand 1 "nonimmediate_operand" "")
                      (match_operand 2 "const_int_operand" "")
                      (match_operand 3 "const_int_operand" "")))]
  ""
{
  enum nds32_expand_result_type result = nds32_expand_extv (operands);
  switch (result)
    {
    case EXPAND_DONE:
      DONE;
      break;
    case EXPAND_FAIL:
      FAIL;
      break;
    case EXPAND_CREATE_TEMPLATE:
      break;
    default:
      gcc_unreachable ();
    }
})

(define_expand "insv"
  [(set (zero_extract (match_operand 0 "nonimmediate_operand" "")
                      (match_operand 1 "const_int_operand" "")
                      (match_operand 2 "const_int_operand" ""))
        (match_operand 3 "register_operand" ""))]
  ""
{
  enum nds32_expand_result_type result = nds32_expand_insv (operands);
  switch (result)
    {
    case EXPAND_DONE:
      DONE;
      break;
    case EXPAND_FAIL:
      FAIL;
      break;
    case EXPAND_CREATE_TEMPLATE:
      break;
    default:
      gcc_unreachable ();
    }
})

;; Arithmetic instructions.

(define_insn "addsi3"
  [(set (match_operand:SI 0 "register_operand"               "=   d,   l,   d,   l, d, l,   k,   l,    r, r")
	(plus:SI (match_operand:SI 1 "register_operand"      "%   0,   l,   0,   l, 0, l,   0,   k,    r, r")
		 (match_operand:SI 2 "nds32_rimm15s_operand" " In05,In03,Iu05,Iu03, r, l,Is10,IU06, Is15, r")))]
  ""
{
  switch (which_alternative)
    {
    case 0:
      /* addi Rt4,Rt4,-x  ==>  subi45 Rt4,x
	 where 0 <= x <= 31 */
      operands[2] = gen_int_mode (-INTVAL (operands[2]), SImode);
      return "subi45\t%0, %2";
    case 1:
      /* addi Rt3,Ra3,-x  ==>  subi333 Rt3,Ra3,x
	 where 0 <= x <= 7 */
      operands[2] = gen_int_mode (-INTVAL (operands[2]), SImode);
      return "subi333\t%0, %1, %2";
    case 2:
      return "addi45\t%0, %2";
    case 3:
      return "addi333\t%0, %1, %2";
    case 4:
      return "add45\t%0, %2";
    case 5:
      return "add333\t%0, %1, %2";
    case 6:
      return "addi10.sp\t%2";
    case 7:
      return "addri36.sp\t%0, %2";
    case 8:
      return "addi\t%0, %1, %2";
    case 9:
      return "add\t%0, %1, %2";

    default:
      gcc_unreachable ();
    }
}
  [(set_attr "type"    "alu,alu,alu,alu,alu,alu,alu,alu,alu,alu")
   (set_attr "length"  "  2,  2,  2,  2,  2,  2,  2,  2,  4,  4")
   (set_attr "feature" " v1, v1, v1, v1, v1, v1, v2, v1, v1, v1")])

(define_insn "subsi3"
  [(set (match_operand:SI 0 "register_operand"                "=d, l,    r, r")
	(minus:SI (match_operand:SI 1 "nds32_rimm15s_operand" " 0, l, Is15, r")
		  (match_operand:SI 2 "register_operand"      " r, l,    r, r")))]
  ""
  "@
   sub45\t%0, %2
   sub333\t%0, %1, %2
   subri\t%0, %2, %1
   sub\t%0, %1, %2"
  [(set_attr "type"   "alu,alu,alu,alu")
   (set_attr "length" "  2,  2,  4,  4")])


;; GCC intends to simplify (plus (ashift ...) (reg))
;; into (plus (mult ...) (reg)), so our matching pattern takes 'mult'
;; and needs to ensure it is exact_log2 value.
(define_insn "*add_slli"
  [(set (match_operand:SI 0 "register_operand"                    "=r")
	(plus:SI (mult:SI (match_operand:SI 1 "register_operand"  " r")
			  (match_operand:SI 2 "immediate_operand" " i"))
		 (match_operand:SI 3 "register_operand"           " r")))]
  "TARGET_ISA_V3 && optimize_size
   && (exact_log2 (INTVAL (operands[2])) != -1)
   && (exact_log2 (INTVAL (operands[2])) <= 31)"
{
  /* Get floor_log2 of the immediate value
     so that we can generate 'add_slli' instruction.  */
  operands[2] = GEN_INT (floor_log2 (INTVAL (operands[2])));

  return "add_slli\t%0, %3, %1, %2";
}
  [(set_attr "type" "alu_shift")
   (set_attr "combo"        "2")
   (set_attr "length"       "4")])

(define_insn "*add_srli"
  [(set (match_operand:SI 0 "register_operand"                          "=   r")
	(plus:SI (lshiftrt:SI (match_operand:SI 1 "register_operand"    "    r")
			      (match_operand:SI 2 "nds32_imm5u_operand" " Iu05"))
		 (match_operand:SI 3 "register_operand"                 "    r")))]
  "TARGET_ISA_V3 && optimize_size"
  "add_srli\t%0, %3, %1, %2"
  [(set_attr "type" "alu_shift")
   (set_attr "combo"        "2")
   (set_attr "length"       "4")])


;; GCC intends to simplify (minus (reg) (ashift ...))
;; into (minus (reg) (mult ...)), so our matching pattern takes 'mult'
;; and needs to ensure it is exact_log2 value.
(define_insn "*sub_slli"
  [(set (match_operand:SI 0 "register_operand"                     "=r")
	(minus:SI (match_operand:SI 1 "register_operand"           " r")
		  (mult:SI (match_operand:SI 2 "register_operand"  " r")
			   (match_operand:SI 3 "immediate_operand" " i"))))]
  "TARGET_ISA_V3 && optimize_size
   && (exact_log2 (INTVAL (operands[3])) != -1)
   && (exact_log2 (INTVAL (operands[3])) <= 31)"
{
  /* Get floor_log2 of the immediate value
     so that we can generate 'sub_slli' instruction.  */
  operands[3] = GEN_INT (floor_log2 (INTVAL (operands[3])));

  return "sub_slli\t%0, %1, %2, %3";
}
  [(set_attr "type" "alu_shift")
   (set_attr "combo"        "2")
   (set_attr "length"       "4")])

(define_insn "*sub_srli"
  [(set (match_operand:SI 0 "register_operand"                           "=   r")
	(minus:SI (match_operand:SI 1 "register_operand"                 "    r")
		  (lshiftrt:SI (match_operand:SI 2 "register_operand"    "    r")
			       (match_operand:SI 3 "nds32_imm5u_operand" " Iu05"))))]
  "TARGET_ISA_V3 && optimize_size"
  "sub_srli\t%0, %1, %2, %3"
  [(set_attr "type" "alu_shift")
   (set_attr "combo"        "2")
   (set_attr "length"       "4")])


;; Multiplication instructions.

(define_insn "mulsi3"
  [(set (match_operand:SI 0 "register_operand"          "=w, r")
	(mult:SI (match_operand:SI 1 "register_operand" "%0, r")
		 (match_operand:SI 2 "register_operand" " w, r")))]
  ""
  "@
   mul33\t%0, %2
   mul\t%0, %1, %2"
  [(set_attr "type"    "mul,mul")
   (set_attr "length"  "  2,  4")
   (set_attr "feature" "v3m, v1")])

(define_insn "mulsidi3"
  [(set (match_operand:DI 0 "register_operand"                          "=r")
	(mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand" " r"))
		 (sign_extend:DI (match_operand:SI 2 "register_operand" " r"))))]
  "TARGET_ISA_V2 || TARGET_ISA_V3"
  "mulsr64\t%0, %1, %2"
  [(set_attr "type"   "mul")
   (set_attr "length"   "4")])

(define_insn "umulsidi3"
  [(set (match_operand:DI 0 "register_operand"                          "=r")
	(mult:DI (zero_extend:DI (match_operand:SI 1 "register_operand" " r"))
		 (zero_extend:DI (match_operand:SI 2 "register_operand" " r"))))]
  "TARGET_ISA_V2 || TARGET_ISA_V3"
  "mulr64\t%0, %1, %2"
  [(set_attr "type"   "mul")
   (set_attr "length"   "4")])


;; Multiply-accumulate instructions.

(define_insn "*maddr32_0"
  [(set (match_operand:SI 0 "register_operand"                   "=r")
	(plus:SI (match_operand:SI 3 "register_operand"          " 0")
		 (mult:SI (match_operand:SI 1 "register_operand" " r")
			  (match_operand:SI 2 "register_operand" " r"))))]
  ""
  "maddr32\t%0, %1, %2"
  [(set_attr "type"   "mac")
   (set_attr "length"   "4")])

(define_insn "*maddr32_1"
  [(set (match_operand:SI 0 "register_operand"                   "=r")
	(plus:SI (mult:SI (match_operand:SI 1 "register_operand" " r")
			  (match_operand:SI 2 "register_operand" " r"))
		 (match_operand:SI 3 "register_operand"          " 0")))]
  ""
  "maddr32\t%0, %1, %2"
  [(set_attr "type"   "mac")
   (set_attr "length"   "4")])

(define_insn "*msubr32"
  [(set (match_operand:SI 0 "register_operand"                    "=r")
	(minus:SI (match_operand:SI 3 "register_operand"          " 0")
		  (mult:SI (match_operand:SI 1 "register_operand" " r")
			   (match_operand:SI 2 "register_operand" " r"))))]
  ""
  "msubr32\t%0, %1, %2"
  [(set_attr "type"   "mac")
   (set_attr "length"   "4")])


;; Div Instructions.

(define_insn "divmodsi4"
  [(set (match_operand:SI 0 "register_operand"         "=r")
	(div:SI (match_operand:SI 1 "register_operand" " r")
		(match_operand:SI 2 "register_operand" " r")))
   (set (match_operand:SI 3 "register_operand"         "=r")
	(mod:SI (match_dup 1) (match_dup 2)))]
  ""
  "divsr\t%0, %3, %1, %2"
  [(set_attr "type"   "div")
   (set_attr "length"   "4")])

(define_insn "udivmodsi4"
  [(set (match_operand:SI 0 "register_operand"          "=r")
	(udiv:SI (match_operand:SI 1 "register_operand" " r")
		 (match_operand:SI 2 "register_operand"  " r")))
   (set (match_operand:SI 3 "register_operand"          "=r")
	(umod:SI (match_dup 1) (match_dup 2)))]
  ""
  "divr\t%0, %3, %1, %2"
  [(set_attr "type"   "div")
   (set_attr "length"   "4")])

;; divsr/divr will keep quotient only when quotient and remainder is the same
;; register in our ISA spec, it's can reduce 1 register presure if we don't
;; want remainder.
(define_insn "divsi4"
  [(set (match_operand:SI 0 "register_operand"         "=r")
	(div:SI (match_operand:SI 1 "register_operand" " r")
		(match_operand:SI 2 "register_operand" " r")))]
  ""
  "divsr\t%0, %0, %1, %2"
  [(set_attr "type"   "div")
   (set_attr "length"   "4")])

(define_insn "udivsi4"
  [(set (match_operand:SI 0 "register_operand"          "=r")
	(udiv:SI (match_operand:SI 1 "register_operand" " r")
		 (match_operand:SI 2 "register_operand"  " r")))]
  ""
  "divr\t%0, %0, %1, %2"
  [(set_attr "type"   "div")
   (set_attr "length"   "4")])

;; ----------------------------------------------------------------------------

;; Boolean instructions.
;; Note: We define the DImode versions in nds32-doubleword.md.

;; ----------------------------------------------------------------------------
;; 'AND' operation
;; ----------------------------------------------------------------------------

(define_insn "bitc"
  [(set (match_operand:SI 0 "register_operand"                 "=r")
	(and:SI (not:SI (match_operand:SI 1 "register_operand" " r"))
		(match_operand:SI 2 "register_operand"         " r")))]
  "TARGET_ISA_V3"
  "bitc\t%0, %2, %1"
  [(set_attr "type" "alu")
   (set_attr "length" "4")]
)

(define_expand "andsi3"
  [(set (match_operand:SI 0 "register_operand" "")
	(and:SI (match_operand:SI 1 "register_operand" "")
		(match_operand:SI 2 "nds32_reg_constant_operand" "")))]
  ""
{
  if (CONST_INT_P (operands[2])
      && !nds32_and_operand (operands[2], SImode))
    {
      nds32_expand_constant (SImode, INTVAL (operands[2]),
			     operands[0], operands[1]);
      DONE;
    }
})

(define_insn "*andsi3"
  [(set (match_operand:SI 0 "register_operand"          "=l, r,   l,   l,   l,   l,   l,   l,    r,   r,     r,    r,    r")
	(and:SI (match_operand:SI 1 "register_operand"  "%0, r,   l,   l,   l,   l,   0,   0,    r,   r,     r,    r,    r")
		(match_operand:SI 2 "nds32_and_operand" " l, r,Izeb,Izeh,Ixls,Ix11,Ibms,Ifex, Izeb, Izeh, Iu15, Ii15, Ic15")))]
  ""
{
  HOST_WIDE_INT mask = INTVAL (operands[2]);

  /* 16-bit andi instructions:
     andi Rt3,Ra3,0xff   -> zeb33  Rt3,Ra3
     andi Rt3,Ra3,0xffff -> zeh33  Rt3,Ra3
     andi Rt3,Ra3,0x01   -> xlsb33 Rt3,Ra3
     andi Rt3,Ra3,0x7ff  -> x11b33 Rt3,Ra3
     andi Rt3,Rt3,2^imm3u          -> bmski33 Rt3,imm3u
     andi Rt3,Rt3,(2^(imm3u+1))-1  -> fexti33 Rt3,imm3u.  */

  switch (which_alternative)
    {
    case 0:
      return "and33\t%0, %2";
    case 1:
      return "and\t%0, %1, %2";
    case 2:
      return "zeb33\t%0, %1";
    case 3:
      return "zeh33\t%0, %1";
    case 4:
      return "xlsb33\t%0, %1";
    case 5:
      return "x11b33\t%0, %1";
    case 6:
      return "bmski33\t%0, %B2";
    case 7:
      operands[2] = GEN_INT (floor_log2 (mask + 1) - 1);
      return "fexti33\t%0, %2";
    case 8:
      return "zeb\t%0, %1";
    case 9:
      return "zeh\t%0, %1";
    case 10:
      return "andi\t%0, %1, %2";
    case 11:
      operands[2] = GEN_INT (~mask);
      return "bitci\t%0, %1, %2";
    case 12:
      return "bclr\t%0, %1, %b2";

    default:
      gcc_unreachable ();
    }
}
  [(set_attr "type"    "alu,alu,alu,alu,alu,alu,alu,alu,alu,alu,alu,alu,alu")
   (set_attr "length"  "  2,  4,  2,  2,  2,  2,  2,  2,  4,  4,  4,  4,  4")
   (set_attr "feature" "v3m, v1, v1, v1, v1, v1,v3m,v3m, v1, v1, v1, v3,pe1")])

(define_insn "*and_slli"
  [(set (match_operand:SI 0 "register_operand"                        "=   r")
	(and:SI (ashift:SI (match_operand:SI 1 "register_operand"     "    r")
			    (match_operand:SI 2 "nds32_imm5u_operand" " Iu05"))
		(match_operand:SI 3 "register_operand"                "    r")))]
  "TARGET_ISA_V3 && optimize_size"
  "and_slli\t%0, %3, %1, %2"
  [(set_attr "type" "alu_shift")
   (set_attr "length"       "4")])

(define_insn "*and_srli"
  [(set (match_operand:SI 0 "register_operand"                         "=   r")
	(and:SI (lshiftrt:SI (match_operand:SI 1 "register_operand"    "    r")
			     (match_operand:SI 2 "nds32_imm5u_operand" " Iu05"))
		(match_operand:SI 3 "register_operand"                 "    r")))]
  "TARGET_ISA_V3 && optimize_size"
  "and_srli\t%0, %3, %1, %2"
  [(set_attr "type" "alu_shift")
   (set_attr "length"       "4")])


;; ----------------------------------------------------------------------------
;; 'OR' operation
;; ----------------------------------------------------------------------------

;; For V3/V3M ISA, we have 'or33' instruction.
;; So we can identify 'or Rt3,Rt3,Ra3' case and set its length to be 2.

(define_expand "iorsi3"
  [(set (match_operand:SI 0 "register_operand"         "")
	(ior:SI (match_operand:SI 1 "register_operand" "")
		(match_operand:SI 2 "general_operand"  "")))]
  ""
{
  if (!nds32_ior_operand (operands[2], SImode))
    operands[2] = force_reg (SImode, operands[2]);
})

(define_insn "*iorsi3"
  [(set (match_operand:SI 0 "register_operand"          "=l, r,    r,    r")
	(ior:SI (match_operand:SI 1 "register_operand"  "%0, r,    r,    r")
		(match_operand:SI 2 "nds32_ior_operand" " l, r, Iu15, Ie15")))]
  ""
  "@
   or33\t%0, %2
   or\t%0, %1, %2
   ori\t%0, %1, %2
   bset\t%0, %1, %B2"
  [(set_attr "type"    "alu,alu,alu,alu")
   (set_attr "length"  "  2,  4,  4,  4")
   (set_attr "feature" "v3m, v1, v1,pe1")])

(define_insn "*or_slli"
  [(set (match_operand:SI 0 "register_operand"                       "=   r")
	(ior:SI (ashift:SI (match_operand:SI 1 "register_operand"    "    r")
			   (match_operand:SI 2 "nds32_imm5u_operand" " Iu05"))
		(match_operand:SI 3 "register_operand"               "    r")))]
  "TARGET_ISA_V3 && optimize_size"
  "or_slli\t%0, %3, %1, %2"
  [(set_attr "type" "alu_shift")
   (set_attr "length"       "4")])

(define_insn "*or_srli"
  [(set (match_operand:SI 0 "register_operand"                         "=   r")
	(ior:SI (lshiftrt:SI (match_operand:SI 1 "register_operand"    "    r")
			     (match_operand:SI 2 "nds32_imm5u_operand" " Iu05"))
		(match_operand:SI 3 "register_operand"                 "    r")))]
  "TARGET_ISA_V3 && optimize_size"
  "or_srli\t%0, %3, %1, %2"
  [(set_attr "type" "alu_shift")
   (set_attr "length"       "4")])


;; ----------------------------------------------------------------------------
;; 'XOR' operation
;; ----------------------------------------------------------------------------

;; For V3/V3M ISA, we have 'xor33' instruction.
;; So we can identify 'xor Rt3,Rt3,Ra3' case and set its length to be 2.

(define_expand "xorsi3"
  [(set (match_operand:SI 0 "register_operand"         "")
	(xor:SI (match_operand:SI 1 "register_operand" "")
		(match_operand:SI 2 "general_operand"  "")))]
  ""
{
  if (!nds32_xor_operand (operands[2], SImode))
    operands[2] = force_reg (SImode, operands[2]);
})

(define_insn "*xorsi3"
  [(set (match_operand:SI 0 "register_operand"          "=l, r,    r,    r")
	(xor:SI (match_operand:SI 1 "register_operand"  "%0, r,    r,    r")
		(match_operand:SI 2 "nds32_xor_operand" " l, r, Iu15, It15")))]
  ""
  "@
   xor33\t%0, %2
   xor\t%0, %1, %2
   xori\t%0, %1, %2
   btgl\t%0, %1, %B2"
  [(set_attr "type"    "alu,alu,alu,alu")
   (set_attr "length"  "  2,  4,  4,  4")
   (set_attr "feature" "v3m, v1, v1,pe1")])

(define_insn "*xor_slli"
  [(set (match_operand:SI 0 "register_operand"                     "=   r")
	(xor:SI (ashift:SI (match_operand:SI 1 "register_operand"  "    r")
			   (match_operand:SI 2 "nds32_imm5u_operand" " Iu05"))
		(match_operand:SI 3 "register_operand"             "    r")))]
  "TARGET_ISA_V3 && optimize_size"
  "xor_slli\t%0, %3, %1, %2"
  [(set_attr "type" "alu_shift")
   (set_attr "length"       "4")])

(define_insn "*xor_srli"
  [(set (match_operand:SI 0 "register_operand"                         "=   r")
	(xor:SI (lshiftrt:SI (match_operand:SI 1 "register_operand"    "    r")
			     (match_operand:SI 2 "nds32_imm5u_operand" " Iu05"))
		(match_operand:SI 3 "register_operand"                 "    r")))]
  "TARGET_ISA_V3 && optimize_size"
  "xor_srli\t%0, %3, %1, %2"
  [(set_attr "type" "alu_shift")
   (set_attr "length"       "4")])

;; Rotate Right Instructions.

(define_insn "*rotrsi3"
  [(set (match_operand:SI 0 "register_operand"                    "=   r, r")
	  (rotatert:SI (match_operand:SI 1 "register_operand"     "    r, r")
		       (match_operand:SI 2 "nds32_rimm5u_operand" " Iu05, r")))]
  ""
  "@
   rotri\t%0, %1, %2
   rotr\t%0, %1, %2"
  [(set_attr "type"    "  alu,  alu")
   (set_attr "subtype" "shift,shift")
   (set_attr "length"  "    4,    4")])


;; ----------------------------------------------------------------------------
;; 'NEG' operation
;; ----------------------------------------------------------------------------

;; For V3/V3M ISA, we have 'neg33' instruction.
;; So we can identify 'xor Rt3,Ra3' case and set its length to be 2.
;; And for V2 ISA, there is NO 'neg33' instruction.
;; The only option is to use 'subri A,B,0' (its semantic is 'A = 0 - B').
(define_insn "negsi2"
  [(set (match_operand:SI 0 "register_operand"         "=l, r")
	(neg:SI (match_operand:SI 1 "register_operand" " l, r")))]
  ""
  "@
   neg33\t%0, %1
   subri\t%0, %1, 0"
  [(set_attr "type"    "alu,alu")
   (set_attr "length"  "  2,  4")
   (set_attr "feature" "v3m, v1")])

(define_expand "negsf2"
  [(set (match_operand:SF 0 "register_operand" "")
	(neg:SF (match_operand:SF 1 "register_operand" "")))]
  ""
{
  if (!TARGET_FPU_SINGLE && !TARGET_EXT_PERF)
    {
      rtx new_dst = simplify_gen_subreg (SImode, operands[0], SFmode, 0);
      rtx new_src = simplify_gen_subreg (SImode, operands[1], SFmode, 0);

      emit_insn (gen_xorsi3 (new_dst,
			     new_src,
			     gen_int_mode (0x80000000, SImode)));

      DONE;
    }
})

(define_expand "negdf2"
  [(set (match_operand:DF 0 "register_operand" "")
	(neg:DF (match_operand:DF 1 "register_operand" "")))]
  ""
{
})

(define_insn_and_split "soft_negdf2"
  [(set (match_operand:DF 0 "register_operand" "")
	(neg:DF (match_operand:DF 1 "register_operand" "")))]
  "!TARGET_FPU_DOUBLE"
  "#"
  "!TARGET_FPU_DOUBLE"
  [(const_int 1)]
{
    rtx src = operands[1];
    rtx dst = operands[0];
    rtx ori_dst = operands[0];

    bool need_extra_move_for_dst_p;
    /* FPU register can't change mode to SI directly, so we need create a
       tmp register to handle it, and FPU register can't do `xor` or btgl.  */
    if (HARD_REGISTER_P (src)
	&& TEST_HARD_REG_BIT (reg_class_contents[FP_REGS], REGNO (src)))
      {
	rtx tmp = gen_reg_rtx (DFmode);
	emit_move_insn (tmp, src);
	src = tmp;
      }

    if (HARD_REGISTER_P (dst)
	&& TEST_HARD_REG_BIT (reg_class_contents[FP_REGS], REGNO (dst)))
      {
	need_extra_move_for_dst_p = true;
	rtx tmp = gen_reg_rtx (DFmode);
	dst = tmp;
      }

    rtx dst_high_part = simplify_gen_subreg (
			  SImode, dst,
			  DFmode, subreg_highpart_offset (SImode, DFmode));
    rtx dst_low_part = simplify_gen_subreg (
			  SImode, dst,
			  DFmode, subreg_lowpart_offset (SImode, DFmode));
    rtx src_high_part = simplify_gen_subreg (
			  SImode, src,
			  DFmode, subreg_highpart_offset (SImode, DFmode));
    rtx src_low_part = simplify_gen_subreg (
			  SImode, src,
			  DFmode, subreg_lowpart_offset (SImode, DFmode));

    emit_insn (gen_xorsi3 (dst_high_part,
			   src_high_part,
			   gen_int_mode (0x80000000, SImode)));
    emit_move_insn (dst_low_part, src_low_part);

    if (need_extra_move_for_dst_p)
      emit_move_insn (ori_dst, dst);

    DONE;
})


;; ----------------------------------------------------------------------------
;; 'ONE_COMPLIMENT' operation
;; ----------------------------------------------------------------------------

;; For V3/V3M ISA, we have 'not33' instruction.
;; So we can identify 'not Rt3,Ra3' case and set its length to be 2.
(define_insn "one_cmplsi2"
  [(set (match_operand:SI 0 "register_operand"         "=w, r")
	(not:SI (match_operand:SI 1 "register_operand" " w, r")))]
  ""
  "@
   not33\t%0, %1
   nor\t%0, %1, %1"
  [(set_attr "type"    "alu,alu")
   (set_attr "length"  "  2,  4")
   (set_attr "feature" "v3m, v1")])


;; ----------------------------------------------------------------------------

;; Shift instructions.

(define_expand "<shift>si3"
  [(set (match_operand:SI 0 "register_operand"                      "")
	(shift_rotate:SI (match_operand:SI 1 "register_operand"     "")
			 (match_operand:SI 2 "nds32_rimm5u_operand" "")))]
  ""
{
  if (operands[2] == const0_rtx)
    {
      emit_move_insn (operands[0], operands[1]);
      DONE;
    }
})

(define_insn "*ashlsi3"
  [(set (match_operand:SI 0 "register_operand"                "=   l,    r, r")
	(ashift:SI (match_operand:SI 1 "register_operand"     "    l,    r, r")
		   (match_operand:SI 2 "nds32_rimm5u_operand" " Iu03, Iu05, r")))]
  ""
  "@
   slli333\t%0, %1, %2
   slli\t%0, %1, %2
   sll\t%0, %1, %2"
  [(set_attr "type"    "  alu,  alu,  alu")
   (set_attr "subtype" "shift,shift,shift")
   (set_attr "length"  "    2,    4,    4")])

(define_insn "*ashrsi3"
  [(set (match_operand:SI 0 "register_operand"                  "=   d,    r, r")
	(ashiftrt:SI (match_operand:SI 1 "register_operand"     "    0,    r, r")
		     (match_operand:SI 2 "nds32_rimm5u_operand" " Iu05, Iu05, r")))]
  ""
  "@
   srai45\t%0, %2
   srai\t%0, %1, %2
   sra\t%0, %1, %2"
  [(set_attr "type"    "  alu,  alu,  alu")
   (set_attr "subtype" "shift,shift,shift")
   (set_attr "length"  "    2,    4,    4")])

(define_insn "*lshrsi3"
  [(set (match_operand:SI 0 "register_operand"                  "=   d,    r, r")
	(lshiftrt:SI (match_operand:SI 1 "register_operand"     "    0,    r, r")
		     (match_operand:SI 2 "nds32_rimm5u_operand" " Iu05, Iu05, r")))]
  ""
  "@
   srli45\t%0, %2
   srli\t%0, %1, %2
   srl\t%0, %1, %2"
  [(set_attr "type"    "  alu,  alu,  alu")
   (set_attr "subtype" "shift,shift,shift")
   (set_attr "length"  "    2,    4,    4")])


;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;; Conditional Move patterns
;; ----------------------------------------------------------------------------

(define_expand "mov<mode>cc"
  [(set (match_operand:QIHISI 0 "register_operand" "")
	(if_then_else:QIHISI (match_operand 1 "nds32_movecc_comparison_operator" "")
			 (match_operand:QIHISI 2 "register_operand" "")
			 (match_operand:QIHISI 3 "register_operand" "")))]
  "TARGET_CMOV && !optimize_size"
{
  enum nds32_expand_result_type result = nds32_expand_movcc (operands);
  switch (result)
    {
    case EXPAND_DONE:
      DONE;
      break;
    case EXPAND_FAIL:
      FAIL;
      break;
    case EXPAND_CREATE_TEMPLATE:
      break;
    default:
      gcc_unreachable ();
    }
})

(define_insn "cmovz<mode>"
  [(set (match_operand:QIHISI 0 "register_operand"                      "=r, r")
	(if_then_else:QIHISI (eq (match_operand:SI 1 "register_operand" " r, r")
			     (const_int 0))
			 (match_operand:QIHISI 2 "register_operand"     " r, 0")
			 (match_operand:QIHISI 3 "register_operand"     " 0, r")))]
  "TARGET_CMOV"
  "@
   cmovz\t%0, %2, %1
   cmovn\t%0, %3, %1"
  [(set_attr "type"  "alu")
   (set_attr "length"  "4")])

(define_insn "cmovn<mode>"
  [(set (match_operand:QIHISI 0 "register_operand"                      "=r, r")
	(if_then_else:QIHISI (ne (match_operand:SI 1 "register_operand" " r, r")
			     (const_int 0))
			 (match_operand:QIHISI 2 "register_operand"     " r, 0")
			 (match_operand:QIHISI 3 "register_operand"     " 0, r")))]
  "TARGET_CMOV"
  "@
   cmovn\t%0, %2, %1
   cmovz\t%0, %3, %1"
  [(set_attr "type"  "alu")
   (set_attr "length"  "4")])

;; A hotfix to help RTL combiner to merge a cmovn insn and a zero_extend insn.
;; It should be removed once after we change the expansion form of the cmovn.
(define_insn "*cmovn_simplified_<mode>"
  [(set (match_operand:QIHISI 0 "register_operand" "=r")
	(if_then_else:QIHISI (match_operand:SI 1 "register_operand" "r")
			 (match_operand:QIHISI 2 "register_operand" "r")
			 (match_operand:QIHISI 3 "register_operand" "0")))]
  ""
  "cmovn\t%0, %2, %1"
  [(set_attr "type" "alu")])

;; ----------------------------------------------------------------------------
;; Conditional Branch patterns
;; ----------------------------------------------------------------------------

(define_expand "cbranchsi4"
  [(set (pc)
	(if_then_else (match_operator 0 "comparison_operator"
			[(match_operand:SI 1 "register_operand"           "")
			 (match_operand:SI 2 "nds32_reg_constant_operand" "")])
		      (label_ref (match_operand 3 "" ""))
		      (pc)))]
  ""
{
  enum nds32_expand_result_type result = nds32_expand_cbranch (operands);
  switch (result)
    {
    case EXPAND_DONE:
      DONE;
      break;
    case EXPAND_FAIL:
      FAIL;
      break;
    case EXPAND_CREATE_TEMPLATE:
      break;
    default:
      gcc_unreachable ();
    }
})


(define_insn "cbranchsi4_equality_zero"
  [(set (pc)
	(if_then_else (match_operator 0 "nds32_equality_comparison_operator"
			[(match_operand:SI 1 "register_operand"  "t,l, r")
			 (const_int 0)])
		      (label_ref (match_operand 2 "" ""))
		      (pc)))]
  ""
{
  return nds32_output_cbranchsi4_equality_zero (insn, operands);
}
  [(set_attr "type" "branch")
   (set_attr_alternative "enabled"
     [
       ;; Alternative 0
       (if_then_else (match_test "TARGET_16_BIT")
		     (const_string "yes")
		     (const_string "no"))
       ;; Alternative 1
       (if_then_else (match_test "TARGET_16_BIT")
		     (const_string "yes")
		     (const_string "no"))
       ;; Alternative 2
       (const_string "yes")
     ])
   (set_attr_alternative "length"
     [
       ;; Alternative 0
       (if_then_else (match_test "!CROSSING_JUMP_P (insn)")
		     (if_then_else (and (ge (minus (match_dup 2) (pc)) (const_int -250))
					(le (minus (match_dup 2) (pc)) (const_int  250)))
				   (if_then_else (match_test "TARGET_16_BIT")
						 (const_int 2)
						 (const_int 4))
				   (if_then_else (and (ge (minus (match_dup 2) (pc)) (const_int -65500))
						      (le (minus (match_dup 2) (pc)) (const_int  65500)))
						 (const_int 4)
						 (if_then_else (match_test "TARGET_16_BIT")
							       (const_int 8)
							       (const_int 10))))
		     (const_int 10))
       ;; Alternative 1
       (if_then_else (match_test "!CROSSING_JUMP_P (insn)")
		     (if_then_else (and (ge (minus (match_dup 2) (pc)) (const_int -250))
					(le (minus (match_dup 2) (pc)) (const_int  250)))
				   (if_then_else (match_test "TARGET_16_BIT")
						 (const_int 2)
						 (const_int 4))
				   (if_then_else (and (ge (minus (match_dup 2) (pc)) (const_int -65500))
						      (le (minus (match_dup 2) (pc)) (const_int  65500)))
						 (const_int 4)
						 (if_then_else (match_test "TARGET_16_BIT")
							       (const_int 8)
							       (const_int 10))))
		     (const_int 10))
       ;; Alternative 2
       (if_then_else (match_test "!CROSSING_JUMP_P (insn)")
		     (if_then_else (and (ge (minus (match_dup 2) (pc)) (const_int -65500))
					(le (minus (match_dup 2) (pc)) (const_int  65500)))
				   (const_int 4)
				   (const_int 10))
		     (const_int 10))
     ])])


;; This pattern is dedicated to V2 ISA,
;; because V2 DOES NOT HAVE beqc/bnec instruction.
(define_insn "cbranchsi4_equality_reg"
  [(set (pc)
	(if_then_else (match_operator 0 "nds32_equality_comparison_operator"
			[(match_operand:SI 1 "register_operand" "v, r")
			 (match_operand:SI 2 "register_operand" "l, r")])
		      (label_ref (match_operand 3 "" ""))
		      (pc)))]
  "TARGET_ISA_V2"
{
  return nds32_output_cbranchsi4_equality_reg (insn, operands);
}
  [(set_attr "type"   "branch")
   (set_attr_alternative "enabled"
     [
       ;; Alternative 0
       (if_then_else (match_test "TARGET_16_BIT")
		     (const_string "yes")
		     (const_string "no"))
       ;; Alternative 1
       (const_string "yes")
     ])
   (set_attr_alternative "length"
     [
       ;; Alternative 0
       (if_then_else (match_test "!CROSSING_JUMP_P (insn)")
		     (if_then_else (and (ge (minus (match_dup 3) (pc)) (const_int -250))
					(le (minus (match_dup 3) (pc)) (const_int  250)))
				   (const_int 2)
				   (if_then_else (and (ge (minus (match_dup 3) (pc))
							  (const_int -16350))
						      (le (minus (match_dup 3) (pc))
							  (const_int  16350)))
						 (const_int 4)
						 (const_int 8)))
		     (const_int 8))
       ;; Alternative 1
       (if_then_else (match_test "!CROSSING_JUMP_P (insn)")
		     (if_then_else (and (ge (minus (match_dup 3) (pc)) (const_int -16350))
					(le (minus (match_dup 3) (pc)) (const_int  16350)))
				   (const_int 4)
				   (const_int 10))
		     (const_int 10))
     ])])


;; This pattern is dedicated to V3/V3M,
;; because V3/V3M DO HAVE beqc/bnec instruction.
(define_insn "cbranchsi4_equality_reg_or_const_int"
  [(set (pc)
	(if_then_else (match_operator 0 "nds32_equality_comparison_operator"
			[(match_operand:SI 1 "register_operand"      "v, r,    r")
			 (match_operand:SI 2 "nds32_rimm11s_operand" "l, r, Is11")])
		      (label_ref (match_operand 3 "" ""))
		      (pc)))]
  "TARGET_ISA_V3 || TARGET_ISA_V3M"
{
  return nds32_output_cbranchsi4_equality_reg_or_const_int (insn, operands);
}
  [(set_attr "type"   "branch")
   (set_attr_alternative "enabled"
     [
       ;; Alternative 0
       (if_then_else (match_test "TARGET_16_BIT")
		     (const_string "yes")
		     (const_string "no"))
       ;; Alternative 1
       (const_string "yes")
       ;; Alternative 2
       (const_string "yes")
     ])
   (set_attr_alternative "length"
     [
       ;; Alternative 0
       (if_then_else (match_test "!CROSSING_JUMP_P (insn)")
		     (if_then_else (and (ge (minus (match_dup 3) (pc)) (const_int -250))
					(le (minus (match_dup 3) (pc)) (const_int  250)))
				   (const_int 2)
				   (if_then_else (and (ge (minus (match_dup 3) (pc))
							  (const_int -16350))
						      (le (minus (match_dup 3) (pc))
							  (const_int  16350)))
						 (const_int 4)
						 (const_int 8)))
		    (const_int 8))
       ;; Alternative 1
       (if_then_else (match_test "!CROSSING_JUMP_P (insn)")
		     (if_then_else (and (ge (minus (match_dup 3) (pc)) (const_int -16350))
					(le (minus (match_dup 3) (pc)) (const_int  16350)))
				   (const_int 4)
				   (const_int 10))
		    (const_int 10))
       ;; Alternative 2
       (if_then_else (match_test "!CROSSING_JUMP_P (insn)")
		     (if_then_else (and (ge (minus (match_dup 3) (pc)) (const_int -250))
					(le (minus (match_dup 3) (pc)) (const_int  250)))
				   (const_int 4)
				   (const_int 10))
		    (const_int 10))
     ])])


(define_insn "*cbranchsi4_greater_less_zero"
  [(set (pc)
	(if_then_else (match_operator 0 "nds32_greater_less_comparison_operator"
			[(match_operand:SI 1 "register_operand" "r")
			 (const_int 0)])
		      (label_ref (match_operand 2 "" ""))
		      (pc)))]
  ""
{
  return nds32_output_cbranchsi4_greater_less_zero (insn, operands);
}
  [(set_attr "type"   "branch")
   (set (attr "length")
	(if_then_else (match_test "!CROSSING_JUMP_P (insn)")
		      (if_then_else (and (ge (minus (match_dup 2) (pc)) (const_int -65500))
					 (le (minus (match_dup 2) (pc)) (const_int  65500)))
				    (const_int 4)
				    (const_int 10))
		      (const_int 10)))])


(define_expand "cstoresi4"
  [(set (match_operand:SI 0 "register_operand" "")
	(match_operator:SI 1 "comparison_operator"
	  [(match_operand:SI 2 "register_operand" "")
	   (match_operand:SI 3 "nonmemory_operand" "")]))]
  ""
{
  enum nds32_expand_result_type result = nds32_expand_cstore (operands);
  switch (result)
    {
    case EXPAND_DONE:
      DONE;
      break;
    case EXPAND_FAIL:
      FAIL;
      break;
    case EXPAND_CREATE_TEMPLATE:
      break;
    default:
      gcc_unreachable ();
    }
})


(define_expand "slts_compare"
  [(set (match_operand:SI 0 "register_operand"       "")
	(lt:SI (match_operand:SI 1 "general_operand" "")
	       (match_operand:SI 2 "general_operand" "")))]
  ""
{
  if (!REG_P (operands[1]))
    operands[1] = force_reg (SImode, operands[1]);

  if (!REG_P (operands[2]) && !satisfies_constraint_Is15 (operands[2]))
    operands[2] = force_reg (SImode, operands[2]);
})

(define_insn "slts_compare_impl"
  [(set (match_operand:SI 0 "register_operand"             "=t,   t, r,    r")
	(lt:SI (match_operand:SI 1 "register_operand"      " d,   d, r,    r")
	       (match_operand:SI 2 "nds32_rimm15s_operand" " r,Iu05, r, Is15")))]
  ""
  "@
   slts45\t%1, %2
   sltsi45\t%1, %2
   slts\t%0, %1, %2
   sltsi\t%0, %1, %2"
  [(set_attr "type"   "alu,    alu,    alu,    alu")
   (set_attr "length" "  2,      2,      4,      4")])

(define_insn "slt_eq0"
  [(set (match_operand:SI 0 "register_operand"        "=t, r")
	(eq:SI (match_operand:SI 1 "register_operand" " d, r")
	       (const_int 0)))]
  ""
  "@
   slti45\t%1, 1
   slti\t%0, %1, 1"
  [(set_attr "type"   "alu, alu")
   (set_attr "length" "  2,   4")])

(define_expand "slt_compare"
  [(set (match_operand:SI 0 "register_operand"        "")
	(ltu:SI (match_operand:SI 1 "general_operand" "")
		(match_operand:SI 2 "general_operand" "")))]
  ""
{
  if (!REG_P (operands[1]))
    operands[1] = force_reg (SImode, operands[1]);

  if (!REG_P (operands[2]) && !satisfies_constraint_Is15 (operands[2]))
    operands[2] = force_reg (SImode, operands[2]);
})

(define_insn "slt_compare_impl"
  [(set (match_operand:SI 0 "register_operand"              "=t,    t, r,    r")
	(ltu:SI (match_operand:SI 1 "register_operand"      " d,    d, r,    r")
		(match_operand:SI 2 "nds32_rimm15s_operand" " r, Iu05, r, Is15")))]
  ""
  "@
   slt45\t%1, %2
   slti45\t%1, %2
   slt\t%0, %1, %2
   slti\t%0, %1, %2"
  [(set_attr "type"   "alu,    alu,    alu,    alu")
   (set_attr "length" "  2,      2,      4,      4")])


;; ----------------------------------------------------------------------------

;; Unconditional and other jump instructions.

(define_insn "jump"
  [(set (pc) (label_ref (match_operand 0 "" "")))]
  ""
{
  /* This unconditional jump has two forms:
       32-bit instruction => j   imm24s << 1
       16-bit instruction => j8  imm8s << 1

     For 32-bit case,
     we assume it is always reachable.
     For 16-bit case,
     it must satisfy { 255 >= (label - pc) >= -256 } condition.
     However, since the $pc for nds32 is at the beginning of the instruction,
     we should leave some length space for current insn.
     So we use range -250 ~ 250.  */
  switch (get_attr_length (insn))
    {
    case 2:
      return "j8\t%0";
    case 4:
      return "j\t%0";
    default:
      gcc_unreachable ();
    }
}
  [(set_attr "type" "branch")
   (set_attr "enabled" "yes")
   (set (attr "length")
	(if_then_else (match_test "!CROSSING_JUMP_P (insn)")
		      (if_then_else (and (ge (minus (match_dup 0) (pc)) (const_int -250))
					 (le (minus (match_dup 0) (pc)) (const_int  250)))
				    (if_then_else (match_test "TARGET_16_BIT")
						  (const_int 2)
						  (const_int 4))
				    (const_int 4))
		      (const_int 4)))])

(define_insn "indirect_jump"
  [(set (pc) (match_operand:SI 0 "register_operand" "r, r"))]
  ""
  "@
   jr5\t%0
   jr\t%0"
  [(set_attr "type"   "branch,branch")
   (set_attr "length" "     2,     4")])

;; Subroutine call instruction returning no value.
;;   operands[0]: It should be a mem RTX whose address is
;;                the address of the function.
;;   operands[1]: It is the number of bytes of arguments pushed as a const_int.
;;   operands[2]: It is the number of registers used as operands.

(define_expand "call"
  [(parallel [(call (match_operand 0 "memory_operand" "")
		    (match_operand 1))
	      (clobber (reg:SI LP_REGNUM))
	      (clobber (reg:SI TA_REGNUM))])]
  ""
  {
    rtx insn;
    rtx sym = XEXP (operands[0], 0);

    if (TARGET_ICT_MODEL_LARGE
	&& nds32_indirect_call_referenced_p (sym))
      {
	rtx reg = gen_reg_rtx (Pmode);
	emit_move_insn (reg, sym);
	operands[0] = gen_const_mem (Pmode, reg);
      }

    if (flag_pic)
      {
	insn = emit_call_insn (gen_call_internal
			       (XEXP (operands[0], 0), GEN_INT (0)));
	use_reg (&CALL_INSN_FUNCTION_USAGE (insn), pic_offset_table_rtx);
	DONE;
      }
  }
)

(define_insn "call_internal"
  [(parallel [(call (mem (match_operand:SI 0 "nds32_call_address_operand" "r, S"))
		    (match_operand 1))
	      (clobber (reg:SI LP_REGNUM))
	      (clobber (reg:SI TA_REGNUM))])]
  ""
{
  rtx_insn *next_insn = next_active_insn (insn);
  bool align_p = (!(next_insn && get_attr_length (next_insn) == 2))
		 && NDS32_ALIGN_P ();
  switch (which_alternative)
    {
    case 0:
      if (TARGET_16_BIT)
	{
	  if (align_p)
	    return "jral5\t%0\;.align 2";
	  else
	    return "jral5\t%0";
	}
      else
	{
	  if (align_p)
	    return "jral\t%0\;.align 2";
	  else
	    return "jral\t%0";
	}
    case 1:
      return nds32_output_call (insn, operands, operands[0],
				"bal\t%0", "jal\t%0", align_p);
    default:
      gcc_unreachable ();
    }
}
  [(set_attr "enabled" "yes")
   (set_attr "type" "branch")
   (set_attr_alternative "length"
     [
       ;; Alternative 0
       (if_then_else (match_test "TARGET_16_BIT")
		     (const_int 2)
		     (const_int 4))
       ;; Alternative 1
       (if_then_else (match_test "flag_pic")
		     (const_int 16)
		     (if_then_else (match_test "nds32_long_call_p (operands[0])")
				   (const_int 12)
				   (const_int 4)))
     ])]
)

;; Subroutine call instruction returning a value.
;;   operands[0]: It is the hard regiser in which the value is returned.
;;   The rest three operands are the same as the
;;   three operands of the 'call' instruction.
;;   (but with numbers increased by one)

(define_expand "call_value"
  [(parallel [(set (match_operand 0)
		   (call (match_operand 1 "memory_operand" "")
		         (match_operand 2)))
	      (clobber (reg:SI LP_REGNUM))
	      (clobber (reg:SI TA_REGNUM))])]
  ""
  {
    rtx insn;
    rtx sym = XEXP (operands[1], 0);

    if (TARGET_ICT_MODEL_LARGE
	&& nds32_indirect_call_referenced_p (sym))
      {
	rtx reg = gen_reg_rtx (Pmode);
	emit_move_insn (reg, sym);
	operands[1] = gen_const_mem (Pmode, reg);
      }

    if (flag_pic)
      {
	insn =
	  emit_call_insn (gen_call_value_internal
			  (operands[0], XEXP (operands[1], 0), GEN_INT (0)));
	use_reg (&CALL_INSN_FUNCTION_USAGE (insn), pic_offset_table_rtx);
	DONE;
      }
  }
)

(define_insn "call_value_internal"
  [(parallel [(set (match_operand 0)
		   (call (mem (match_operand:SI 1 "nds32_call_address_operand" "r, S"))
		         (match_operand 2)))
	      (clobber (reg:SI LP_REGNUM))
	      (clobber (reg:SI TA_REGNUM))])]
  ""
{
  rtx_insn *next_insn = next_active_insn (insn);
  bool align_p = (!(next_insn && get_attr_length (next_insn) == 2))
		 && NDS32_ALIGN_P ();
  switch (which_alternative)
    {
    case 0:
      if (TARGET_16_BIT)
	{
	  if (align_p)
	    return "jral5\t%1\;.align 2";
	  else
	    return "jral5\t%1";
	}
      else
	{
	  if (align_p)
	    return "jral\t%1\;.align 2";
	  else
	    return "jral\t%1";
	}
    case 1:
      return nds32_output_call (insn, operands, operands[1],
				"bal\t%1", "jal\t%1", align_p);
    default:
      gcc_unreachable ();
    }
}
  [(set_attr "enabled" "yes")
   (set_attr "type" "branch")
   (set_attr_alternative "length"
     [
       ;; Alternative 0
       (if_then_else (match_test "TARGET_16_BIT")
		     (const_int 2)
		     (const_int 4))
       ;; Alternative 1
       (if_then_else (match_test "flag_pic")
		     (const_int 16)
		     (if_then_else (match_test "nds32_long_call_p (operands[1])")
				   (const_int 12)
				   (const_int 4)))
     ])]
)

;; Call subroutine returning any type.

(define_expand "untyped_call"
  [(parallel [(call (match_operand 0 "" "")
		    (const_int 0))
	      (match_operand 1 "" "")
	      (match_operand 2 "" "")])]
  ""
{
  int i;

  emit_call_insn (gen_call (operands[0], const0_rtx));

  for (i = 0; i < XVECLEN (operands[2], 0); i++)
    {
      rtx set = XVECEXP (operands[2], 0, i);
      emit_move_insn (SET_DEST (set), SET_SRC (set));
    }

  /* The optimizer does not know that the call sets the function value
     registers we stored in the result block.  We avoid problems by
     claiming that all hard registers are used and clobbered at this
     point.  */
  emit_insn (gen_blockage ());
  DONE;
})

;; ----------------------------------------------------------------------------

;; The sibcall patterns.

;; sibcall
;; sibcall_internal

(define_expand "sibcall"
  [(parallel [(call (match_operand 0 "memory_operand" "")
		    (const_int 0))
	      (clobber (reg:SI TA_REGNUM))
	      (return)])]
  ""
{
    rtx sym = XEXP (operands[0], 0);

    if (TARGET_ICT_MODEL_LARGE
	&& nds32_indirect_call_referenced_p (sym))
      {
	rtx reg = gen_reg_rtx (Pmode);
	emit_move_insn (reg, sym);
	operands[0] = gen_const_mem (Pmode, reg);
      }
})

(define_insn "sibcall_internal"
  [(parallel [(call (mem (match_operand:SI 0 "nds32_call_address_operand" "r, S"))
		    (match_operand 1))
	      (clobber (reg:SI TA_REGNUM))
	      (return)])]
  ""
{
  switch (which_alternative)
    {
    case 0:
      if (TARGET_16_BIT)
	return "jr5\t%0";
      else
	return "jr\t%0";
    case 1:
      if (nds32_long_call_p (operands[0]))
	return "b\t%0";
      else
	return "j\t%0";
    default:
      gcc_unreachable ();
    }
}
  [(set_attr "enabled" "yes")
   (set_attr "type" "branch")
   (set_attr_alternative "length"
     [
       ;; Alternative 0
       (if_then_else (match_test "TARGET_16_BIT")
		     (const_int 2)
		     (const_int 4))
       ;; Alternative 1
       (if_then_else (match_test "flag_pic")
		     (const_int 16)
		     (if_then_else (match_test "nds32_long_call_p (operands[0])")
				   (const_int 12)
				   (const_int 4)))
     ])]
)

;; sibcall_value
;; sibcall_value_internal
;; sibcall_value_immediate

(define_expand "sibcall_value"
  [(parallel [(set (match_operand 0)
		   (call (match_operand 1 "memory_operand" "")
			 (const_int 0)))
	      (clobber (reg:SI TA_REGNUM))
	      (return)])]
  ""
{
    rtx sym = XEXP (operands[1], 0);

    if (TARGET_ICT_MODEL_LARGE
	&& nds32_indirect_call_referenced_p (sym))
      {
	rtx reg = gen_reg_rtx (Pmode);
	emit_move_insn (reg, sym);
	operands[1] = gen_const_mem (Pmode, reg);
      }
})

(define_insn "sibcall_value_internal"
  [(parallel [(set (match_operand 0)
		   (call (mem (match_operand:SI 1 "nds32_call_address_operand" "r, S"))
			 (match_operand 2)))
	      (clobber (reg:SI TA_REGNUM))
	      (return)])]
  ""
{
  switch (which_alternative)
    {
    case 0:
      if (TARGET_16_BIT)
	return "jr5\t%1";
      else
	return "jr\t%1";
    case 1:
      if (nds32_long_call_p (operands[1]))
	return "b\t%1";
      else
	return "j\t%1";
    default:
      gcc_unreachable ();
    }
}
  [(set_attr "enabled" "yes")
   (set_attr "type" "branch")
   (set_attr_alternative "length"
     [
       ;; Alternative 0
       (if_then_else (match_test "TARGET_16_BIT")
		     (const_int 2)
		     (const_int 4))
       ;; Alternative 1
       (if_then_else (match_test "flag_pic")
		     (const_int 16)
		     (if_then_else (match_test "nds32_long_call_p (operands[1])")
				   (const_int 12)
				   (const_int 4)))
     ])]
)

;; ----------------------------------------------------------------------------

;; prologue and epilogue.

(define_expand "prologue" [(const_int 0)]
  ""
{
  /* Note that only under V3/V3M ISA, we could use v3push prologue.
     In addition, we need to check if v3push is indeed available.  */
  if (NDS32_V3PUSH_AVAILABLE_P)
    nds32_expand_prologue_v3push ();
  else
    nds32_expand_prologue ();

  /* If cfun->machine->fp_as_gp_p is true, we can generate special
     directive to guide linker doing fp-as-gp optimization.
     However, for a naked function, which means
     it should not have prologue/epilogue,
     using fp-as-gp still requires saving $fp by push/pop behavior and
     there is no benefit to use fp-as-gp on such small function.
     So we need to make sure this function is NOT naked as well.  */
  if (cfun->machine->fp_as_gp_p && !cfun->machine->naked_p)
    emit_insn (gen_omit_fp_begin (gen_rtx_REG (SImode, FP_REGNUM)));

  DONE;
})

(define_expand "epilogue" [(const_int 0)]
  ""
{
  /* If cfun->machine->fp_as_gp_p is true, we can generate special
     directive to guide linker doing fp-as-gp optimization.
     However, for a naked function, which means
     it should not have prologue/epilogue,
     using fp-as-gp still requires saving $fp by push/pop behavior and
     there is no benefit to use fp-as-gp on such small function.
     So we need to make sure this function is NOT naked as well.  */
  if (cfun->machine->fp_as_gp_p && !cfun->machine->naked_p)
    emit_insn (gen_omit_fp_end (gen_rtx_REG (SImode, FP_REGNUM)));

  /* Note that only under V3/V3M ISA, we could use v3pop epilogue.
     In addition, we need to check if v3push is indeed available.  */
  if (NDS32_V3PUSH_AVAILABLE_P)
    nds32_expand_epilogue_v3pop (false);
  else
    nds32_expand_epilogue (false);
  DONE;
})

(define_expand "sibcall_epilogue" [(const_int 0)]
  ""
{
  /* Pass true to indicate that this is sibcall epilogue and
     exit from a function without the final branch back to the
     calling function.  */
  nds32_expand_epilogue (true);

  DONE;
})


;; nop instruction.

(define_insn "nop"
  [(const_int 0)]
  ""
{
  if (TARGET_16_BIT)
    return "nop16";
  else
    return "nop";
}
  [(set_attr "type" "misc")
   (set_attr "enabled" "yes")
   (set (attr "length")
	(if_then_else (match_test "TARGET_16_BIT")
		      (const_int 2)
		      (const_int 4)))])


;; ----------------------------------------------------------------------------
;; Stack push/pop operations
;; ----------------------------------------------------------------------------

;; The pattern for stack push.
;; Both stack_push_multiple and stack_v3push use the following pattern.
;; So we need to use TARGET_V3PUSH to determine the instruction length.
(define_insn "*stack_push"
  [(match_parallel 0 "nds32_stack_push_operation"
     [(set (mem:SI (plus:SI (reg:SI SP_REGNUM)
			    (match_operand:SI 1 "const_int_operand" "")))
	   (match_operand:SI 2 "register_operand" ""))
     ])]
  ""
{
  return nds32_output_stack_push (operands[0]);
}
  [(set_attr "type" "store_multiple")
   (set_attr "combo" "12")
   (set_attr "enabled" "yes")
   (set (attr "length")
	(if_then_else (match_test "NDS32_V3PUSH_AVAILABLE_P")
		      (const_int 2)
		      (const_int 4)))])


;; The pattern for stack pop.
;; Both stack_pop_multiple and stack_v3pop use the following pattern.
;; So we need to use TARGET_V3PUSH to determine the instruction length.
(define_insn "*stack_pop"
  [(match_parallel 0 "nds32_stack_pop_operation"
     [(set (match_operand:SI 1 "register_operand" "")
	   (mem:SI (reg:SI SP_REGNUM)))
     ])]
  ""
{
  return nds32_output_stack_pop (operands[0]);
}
  [(set_attr "type" "load_multiple")
   (set_attr "combo" "12")
   (set_attr "enabled" "yes")
   (set (attr "length")
	(if_then_else (match_test "NDS32_V3PUSH_AVAILABLE_P")
		      (const_int 2)
		      (const_int 4)))])


;; ----------------------------------------------------------------------------
;; Return operation patterns
;; ----------------------------------------------------------------------------

;; Use this pattern to expand a return instruction
;; with simple_return rtx if no epilogue is required.
(define_expand "return"
  [(parallel [(return)
              (clobber (reg:SI FP_REGNUM))])]
  "nds32_can_use_return_insn ()"
{
  /* Emit as the simple return.  */
  if (!cfun->machine->fp_as_gp_p
      && cfun->machine->naked_p
      && (cfun->machine->va_args_size == 0))
    {
      emit_jump_insn (gen_return_internal ());
      DONE;
    }
})

;; This pattern is expanded only by the shrink-wrapping optimization
;; on paths where the function prologue has not been executed.
;; However, such optimization may reorder the prologue/epilogue blocks
;; together with basic blocks within function body.
;; So we must disable this pattern if we have already decided
;; to perform fp_as_gp optimization, which requires prologue to be
;; first block and epilogue to be last block.
(define_expand "simple_return"
  [(simple_return)]
  "!cfun->machine->fp_as_gp_p"
  ""
)

(define_insn "*nds32_return"
  [(parallel [(return)
   (clobber (reg:SI FP_REGNUM))])]
  ""
{
  return nds32_output_return ();
}
  [(set_attr "type" "branch")
   (set_attr "enabled" "yes")
   (set_attr "length" "4")])

(define_insn "return_internal"
  [(simple_return)]
  ""
{
  if (nds32_isr_function_critical_p (current_function_decl))
    return "iret";

  if (TARGET_16_BIT)
    return "ret5";
  else
    return "ret";
}
  [(set_attr "type" "branch")
   (set_attr "enabled" "yes")
   (set (attr "length")
	(if_then_else (match_test "nds32_isr_function_critical_p (current_function_decl)")
		      (const_int 4)
		      (if_then_else (match_test "TARGET_16_BIT")
				    (const_int 2)
				    (const_int 4))))])


;; ----------------------------------------------------------------------------
;; Jump Table patterns
;; ----------------------------------------------------------------------------
;; Need to implement ASM_OUTPUT_ADDR_VEC_ELT (for normal jump table)
;; or ASM_OUTPUT_ADDR_DIFF_ELT (for pc relative jump table) as well.
;;
;; operands[0]: The index to dispatch on.
;; operands[1]: The lower bound for indices in the table.
;; operands[2]: The total range of indices int the table.
;;              i.e. The largest index minus the smallest one.
;; operands[3]: A label that precedes the table itself.
;; operands[4]: A label to jump to if the index has a value outside the bounds.
;;
;; We need to create following sequences for jump table code generation:
;;   A) k <-- (plus (operands[0]) (-operands[1]))
;;   B) if (gtu k operands[2]) then goto operands[4]
;;   C) t <-- operands[3]
;;   D) z <-- (mem (plus (k << 0 or 1 or 2) t))
;;   E) z <-- t + z (NOTE: This is only required for pc relative jump table.)
;;   F) jump to target with register t or z
;;
;; The steps C, D, E, and F are performed by casesi_internal pattern.
(define_expand "casesi"
  [(match_operand:SI 0 "register_operand"  "r") ; index to jump on
   (match_operand:SI 1 "immediate_operand" "i") ; lower bound
   (match_operand:SI 2 "immediate_operand" "i") ; total range
   (match_operand:SI 3 "" "")                   ; table label
   (match_operand:SI 4 "" "")]                  ; Out of range label
  ""
{
  rtx add_tmp;
  rtx reg, test;
  rtx tmp_reg;

  /* Step A: "k <-- (plus (operands[0]) (-operands[1]))".  */
  if (operands[1] != const0_rtx)
    {
      reg = gen_reg_rtx (SImode);
      add_tmp = gen_int_mode (-INTVAL (operands[1]), SImode);

      /* If the integer value is not in the range of imm15s,
	 we need to force register first because our addsi3 pattern
	 only accept nds32_rimm15s_operand predicate.  */
      add_tmp = force_reg (SImode, add_tmp);

      emit_insn (gen_addsi3 (reg, operands[0], add_tmp));
      operands[0] = reg;
    }

  /* Step B: "if (gtu k operands[2]) then goto operands[4]".  */
  test = gen_rtx_GTU (VOIDmode, operands[0], operands[2]);
  emit_jump_insn (gen_cbranchsi4 (test, operands[0], operands[2],
				  operands[4]));

  tmp_reg = gen_reg_rtx (SImode);
  /* Step C, D, E, and F, using another temporary register tmp_reg.  */
  if (flag_pic)
    emit_use (pic_offset_table_rtx);

  emit_jump_insn (gen_casesi_internal (operands[0],
				       operands[3],
				       tmp_reg));
  DONE;
})

;; We are receiving operands from casesi pattern:
;;
;; operands[0]: The index that have been substracted with lower bound.
;; operands[1]: A label that precedes the table itself.
;; operands[2]: A temporary register to retrieve value in table.
;;
;; We need to perform steps C, D, E, and F:
;;
;;   C) t <-- operands[1]
;;   D) z <-- (mem (plus (operands[0] << m) t))
;;            m is 2 for normal jump table.
;;            m is 0, 1, or 2 for pc relative jump table based on diff size.
;;   E) t <-- z + t (NOTE: This is only required for pc relative jump table.)
;;   F) Jump to target with register t or z.
;;
;; The USE in this pattern is needed to tell flow analysis that this is
;; a CASESI insn.  It has no other purpose.
(define_insn "casesi_internal"
  [(parallel [(set (pc)
		   (mem:SI (plus:SI (mult:SI (match_operand:SI 0 "register_operand" "r")
					     (const_int 4))
				    (label_ref (match_operand 1 "" "")))))
	      (use (label_ref (match_dup 1)))
	      (clobber (match_operand:SI 2 "register_operand" "=r"))
	      (clobber (reg:SI TA_REGNUM))])]
  ""
{
  if (CASE_VECTOR_PC_RELATIVE)
    return nds32_output_casesi_pc_relative (operands);
  else
    return nds32_output_casesi (operands);
}
  [(set_attr "type" "branch")
   (set (attr "length")
	(if_then_else (match_test "flag_pic")
		      (const_int 28)
		      (const_int 20)))])

;; ----------------------------------------------------------------------------

;; Performance Extension

; If -fwrapv option is issued, GCC expects there will be
; signed overflow situation.  So the ABS(INT_MIN) is still INT_MIN
; (e.g. ABS(0x80000000)=0x80000000).
; However, the hardware ABS instruction of nds32 target
; always performs saturation: abs 0x80000000 -> 0x7fffffff.
; So that we can only enable abssi2 pattern if flag_wrapv is NOT presented.
(define_insn "abssi2"
  [(set (match_operand:SI 0 "register_operand"         "=r")
	(abs:SI (match_operand:SI 1 "register_operand" " r")))]
  "TARGET_EXT_PERF && TARGET_HW_ABS && !flag_wrapv"
  "abs\t%0, %1"
  [(set_attr "type" "alu")
   (set_attr "length" "4")])

(define_insn "clzsi2"
  [(set (match_operand:SI 0 "register_operand"         "=r")
	(clz:SI (match_operand:SI 1 "register_operand" " r")))]
  "TARGET_EXT_PERF"
  "clz\t%0, %1"
  [(set_attr "type" "alu")
   (set_attr "length" "4")])

(define_insn "smaxsi3"
  [(set (match_operand:SI 0 "register_operand"          "=r")
	(smax:SI (match_operand:SI 1 "register_operand" " r")
		 (match_operand:SI 2 "register_operand" " r")))]
  "TARGET_EXT_PERF"
  "max\t%0, %1, %2"
  [(set_attr "type" "alu")
   (set_attr "length" "4")])

(define_insn "sminsi3"
  [(set (match_operand:SI 0 "register_operand"          "=r")
	(smin:SI (match_operand:SI 1 "register_operand" " r")
		 (match_operand:SI 2 "register_operand" " r")))]
  "TARGET_EXT_PERF"
  "min\t%0, %1, %2"
  [(set_attr "type" "alu")
   (set_attr "length" "4")])

(define_insn "btst"
  [(set (match_operand:SI 0 "register_operand"                     "=   r")
	(zero_extract:SI (match_operand:SI 1 "register_operand"    "    r")
			 (const_int 1)
			 (match_operand:SI 2 "nds32_imm5u_operand" " Iu05")))]
  "TARGET_EXT_PERF"
  "btst\t%0, %1, %2"
  [(set_attr "type" "alu")
   (set_attr "length" "4")])

(define_insn "ave"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(truncate:SI
	  (ashiftrt:DI
	    (plus:DI
	      (plus:DI
		(sign_extend:DI (match_operand:SI 1 "register_operand" "r"))
		(sign_extend:DI (match_operand:SI 2 "register_operand" "r")))
	      (const_int 1))
	  (const_int 1))))]
  "TARGET_EXT_PERF"
  "ave\t%0, %1, %2"
  [(set_attr "type" "alu")
   (set_attr "length" "4")])

;; ----------------------------------------------------------------------------

;; Pseudo NOPs

(define_insn "relax_group"
  [(unspec_volatile [(match_operand:SI 0 "immediate_operand" "i")] UNSPEC_VOLATILE_RELAX_GROUP)]
  ""
  ".relax_hint %0"
  [(set_attr "length" "0")]
)

;; Output .omit_fp_begin for fp-as-gp optimization.
;; Also we have to set $fp register.
(define_insn "omit_fp_begin"
  [(set (match_operand:SI 0 "register_operand" "=x")
	(unspec_volatile:SI [(const_int 0)] UNSPEC_VOLATILE_OMIT_FP_BEGIN))]
  ""
  "! -----\;.omit_fp_begin\;la\t$fp,_FP_BASE_\;! -----"
  [(set_attr "length" "8")]
)

;; Output .omit_fp_end for fp-as-gp optimization.
;; Claim that we have to use $fp register.
(define_insn "omit_fp_end"
  [(unspec_volatile:SI [(match_operand:SI 0 "register_operand" "x")] UNSPEC_VOLATILE_OMIT_FP_END)]
  ""
  "! -----\;.omit_fp_end\;! -----"
  [(set_attr "length" "0")]
)

(define_insn "pop25return"
  [(return)
   (unspec_volatile:SI [(reg:SI LP_REGNUM)] UNSPEC_VOLATILE_POP25_RETURN)]
  ""
  "! return for pop 25"
  [(set_attr "length" "0")]
)

;; Add pc
(define_insn "add_pc"
  [(set (match_operand:SI 0 "register_operand"          "=r")
	(plus:SI (match_operand:SI 1 "register_operand"  "0")
		 (pc)))]
  "TARGET_LINUX_ABI || flag_pic"
  "add5.pc\t%0"
  [(set_attr "type"    "alu")
   (set_attr "length"    "4")]
)

(define_expand "bswapsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(bswap:SI (match_operand:SI 1 "register_operand" "r")))]
  ""
{
  emit_insn (gen_unspec_wsbh (operands[0], operands[1]));
  emit_insn (gen_rotrsi3 (operands[0], operands[0], GEN_INT (16)));
  DONE;
})

(define_insn "bswaphi2"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(bswap:HI (match_operand:HI 1 "register_operand" "r")))]
  ""
  "wsbh\t%0, %1"
  [(set_attr "type"    "alu")
   (set_attr "length"    "4")]
)

;; ----------------------------------------------------------------------------

;; Patterns for exception handling

(define_expand "eh_return"
  [(use (match_operand 0 "general_operand"))]
  ""
{
  emit_insn (gen_nds32_eh_return (operands[0]));
  DONE;
})

(define_insn_and_split "nds32_eh_return"
  [(unspec_volatile [(match_operand:SI 0 "register_operand" "r")] UNSPEC_VOLATILE_EH_RETURN)]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
{
  rtx place;
  rtx addr;

  /* The operands[0] is the handler address.  We need to assign it
     to return address rtx so that we can jump to exception handler
     when returning from current function.  */

  if (cfun->machine->lp_size == 0)
    {
      /* If $lp is not saved in the stack frame, we can take $lp directly.  */
      place = gen_rtx_REG (SImode, LP_REGNUM);
    }
  else
    {
      /* Otherwise, we need to locate the stack slot of return address.
	 The return address is generally saved in [$fp-4] location.
	 However, DSE (dead store elimination) does not detect an alias
	 between [$fp-x] and [$sp+y].  This can result in a store to save
	 $lp introduced by builtin_eh_return() being incorrectly deleted
	 if it is based on $fp.  The solution we take here is to compute
	 the offset relative to stack pointer and then use $sp to access
	 location so that the alias can be detected.
	 FIXME: What if the immediate value "offset" is too large to be
	        fit in a single addi instruction?  */
      HOST_WIDE_INT offset;

      offset = (cfun->machine->fp_size
		+ cfun->machine->gp_size
		+ cfun->machine->lp_size
		+ cfun->machine->callee_saved_gpr_regs_size
		+ cfun->machine->callee_saved_area_gpr_padding_bytes
		+ cfun->machine->callee_saved_fpr_regs_size
		+ cfun->machine->eh_return_data_regs_size
		+ cfun->machine->local_size
		+ cfun->machine->out_args_size);

      addr = plus_constant (Pmode, stack_pointer_rtx, offset - 4);
      place = gen_frame_mem (SImode, addr);
    }

  emit_move_insn (place, operands[0]);
  DONE;
})

;; ----------------------------------------------------------------------------

;; Patterns for TLS.
;; The following two tls patterns don't be expanded directly because the
;; intermediate value may be spilled into the stack.  As a result, it is
;; hard to analyze the define-use chain in the relax_opt pass.


;; There is a unspec operand to record RELAX_GROUP number because each
;; emitted instruction need a relax_hint above it.
(define_insn "tls_desc"
  [(set (reg:SI 0)
	(call (unspec_volatile:SI [(match_operand:SI 0 "nds32_symbolic_operand" "i")] UNSPEC_TLS_DESC)
	      (const_int 1)))
   (use (unspec [(match_operand:SI 1 "immediate_operand" "i")] UNSPEC_VOLATILE_RELAX_GROUP))
   (use (reg:SI GP_REGNUM))
   (clobber (reg:SI LP_REGNUM))
   (clobber (reg:SI TA_REGNUM))]
  ""
  {
    return nds32_output_tls_desc (operands);
  }
  [(set_attr "length" "20")
   (set_attr "type" "branch")]
)

;; There is a unspec operand to record RELAX_GROUP number because each
;; emitted instruction need a relax_hint above it.
(define_insn "tls_ie"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI [(match_operand:SI 1 "nds32_symbolic_operand" "i")] UNSPEC_TLS_IE))
   (use (unspec [(match_operand:SI 2 "immediate_operand" "i")] UNSPEC_VOLATILE_RELAX_GROUP))
   (use (reg:SI GP_REGNUM))]
  ""
  {
    return nds32_output_tls_ie (operands);
  }
  [(set (attr "length") (if_then_else (match_test "flag_pic")
				      (const_int 12)
				      (const_int 8)))
   (set_attr "type" "misc")]
)

;; The pattern is for some relaxation groups that have to keep addsi3 in 32-bit mode.
(define_insn "addsi3_32bit"
  [(set (match_operand:SI 0 "register_operand"             "=r")
	(unspec:SI [(match_operand:SI 1 "register_operand" "%r")
		    (match_operand:SI 2 "register_operand" " r")] UNSPEC_ADD32))]
  ""
  "add\t%0, %1, %2";
  [(set_attr "type"    "alu")
   (set_attr "length"  "4")
   (set_attr "feature" "v1")])

;; ----------------------------------------------------------------------------
