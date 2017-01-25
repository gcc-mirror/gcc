;; Machine description of Andes NDS32 cpu for GNU compiler
;; Copyright (C) 2012-2017 Free Software Foundation, Inc.
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

;; Include peephole patterns.
(include "nds32-peephole2.md")


;; Insn type, it is used to default other attribute values.
(define_attr "type"
  "unknown,move,load,store,alu,compare,branch,call,misc"
  (const_string "unknown"))


;; Length, in bytes, default is 4-bytes.
(define_attr "length" "" (const_int 4))


;; Enabled, which is used to enable/disable insn alternatives.
;; Note that we use length and TARGET_16_BIT here as criteria.
;; If the instruction pattern already check TARGET_16_BIT to
;; determine the length by itself, its enabled attribute should be
;; always 1 to avoid the conflict with the settings here.
(define_attr "enabled" ""
  (cond [(and (eq_attr "length" "2")
	      (match_test "!TARGET_16_BIT"))
	 (const_int 0)]
	(const_int 1)))


;; ----------------------------------------------------------------------------


;; Move instructions.

;; For QImode and HImode, the immediate value can be fit in imm20s.
;; So there is no need to split rtx for QI and HI patterns.

(define_expand "movqi"
  [(set (match_operand:QI 0 "general_operand" "")
	(match_operand:QI 1 "general_operand" ""))]
  ""
{
  /* Need to force register if mem <- !reg.  */
  if (MEM_P (operands[0]) && !REG_P (operands[1]))
    operands[1] = force_reg (QImode, operands[1]);
})

(define_expand "movhi"
  [(set (match_operand:HI 0 "general_operand" "")
	(match_operand:HI 1 "general_operand" ""))]
  ""
{
  /* Need to force register if mem <- !reg.  */
  if (MEM_P (operands[0]) && !REG_P (operands[1]))
    operands[1] = force_reg (HImode, operands[1]);
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
})

(define_insn "*mov<mode>"
  [(set (match_operand:QIHISI 0 "nonimmediate_operand" "=r, r, U45, U33, U37, U45, m,   l,   l,   l,   d, r,    d,    r,    r,    r")
	(match_operand:QIHISI 1 "nds32_move_operand"   " r, r,   l,   l,   l,   d, r, U45, U33, U37, U45, m, Ip05, Is05, Is20, Ihig"))]
  ""
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
      return nds32_output_16bit_load (operands, <byte>);
    case 11:
      return nds32_output_32bit_load (operands, <byte>);
    case 12:
      return "movpi45\t%0, %1";
    case 13:
      return "movi55\t%0, %1";
    case 14:
      return "movi\t%0, %1";
    case 15:
      return "sethi\t%0, hi20(%1)";
    default:
      gcc_unreachable ();
    }
}
  [(set_attr "type"   "alu,alu,store,store,store,store,store,load,load,load,load,load,alu,alu,alu,alu")
   (set_attr "length" "  2,  4,    2,    2,    2,    2,    4,   2,   2,   2,   2,   4,  2,  2,  4,  4")])


;; We use nds32_symbolic_operand to limit that only CONST/SYMBOL_REF/LABEL_REF
;; are able to match such instruction template.
(define_insn "*move_addr"
  [(set (match_operand:SI 0 "register_operand"       "=l, r")
	(match_operand:SI 1 "nds32_symbolic_operand" " i, i"))]
  ""
  "la\t%0, %1"
  [(set_attr "type" "move")
   (set_attr "length"  "8")])


(define_insn "*sethi"
  [(set (match_operand:SI 0 "register_operand"                "=r")
	(high:SI (match_operand:SI 1 "nds32_symbolic_operand" " i")))]
  ""
  "sethi\t%0, hi20(%1)"
  [(set_attr "type" "alu")
   (set_attr "length" "4")])


(define_insn "*lo_sum"
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

;; Arithmetic instructions.

(define_insn "add<mode>3"
  [(set (match_operand:QIHISI 0 "register_operand"                   "=   d,    l,    d,    l,  d, l,    k,    l,    r, r")
	(plus:QIHISI (match_operand:QIHISI 1 "register_operand"      "%   0,    l,    0,    l,  0, l,    0,    k,    r, r")
		     (match_operand:QIHISI 2 "nds32_rimm15s_operand" " In05, In03, Iu05, Iu03,  r, l, Is10, Iu06, Is15, r")))]
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
  [(set_attr "type"   "alu,alu,alu,alu,alu,alu,alu,alu,alu,alu")
   (set_attr "length" "  2,  2,  2,  2,  2,  2,  2,  2,  4,  4")])

(define_insn "sub<mode>3"
  [(set (match_operand:QIHISI 0 "register_operand"                    "=d, l,    r, r")
	(minus:QIHISI (match_operand:QIHISI 1 "nds32_rimm15s_operand" " 0, l, Is15, r")
		      (match_operand:QIHISI 2 "register_operand"      " r, l,    r, r")))]
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
  "TARGET_ISA_V3
   && (exact_log2 (INTVAL (operands[2])) != -1)
   && (exact_log2 (INTVAL (operands[2])) <= 31)"
{
  /* Get floor_log2 of the immediate value
     so that we can generate 'add_slli' instruction.  */
  operands[2] = GEN_INT (floor_log2 (INTVAL (operands[2])));

  return "add_slli\t%0, %3, %1, %2";
}
  [(set_attr "type" "alu")
   (set_attr "length" "4")])

(define_insn "*add_srli"
  [(set (match_operand:SI 0 "register_operand"                        "=   r")
	(plus:SI (lshiftrt:SI (match_operand:SI 1 "register_operand"  "    r")
			      (match_operand:SI 2 "immediate_operand" " Iu05"))
		 (match_operand:SI 3 "register_operand"               "    r")))]
  "TARGET_ISA_V3"
  "add_srli\t%0, %3, %1, %2"
  [(set_attr "type" "alu")
   (set_attr "length" "4")])


;; GCC intends to simplify (minus (reg) (ashift ...))
;; into (minus (reg) (mult ...)), so our matching pattern takes 'mult'
;; and needs to ensure it is exact_log2 value.
(define_insn "*sub_slli"
  [(set (match_operand:SI 0 "register_operand"                     "=r")
	(minus:SI (match_operand:SI 1 "register_operand"           " r")
		  (mult:SI (match_operand:SI 2 "register_operand"  " r")
			   (match_operand:SI 3 "immediate_operand" " i"))))]
  "TARGET_ISA_V3
   && (exact_log2 (INTVAL (operands[3])) != -1)
   && (exact_log2 (INTVAL (operands[3])) <= 31)"
{
  /* Get floor_log2 of the immediate value
     so that we can generate 'sub_slli' instruction.  */
  operands[3] = GEN_INT (floor_log2 (INTVAL (operands[3])));

  return "sub_slli\t%0, %1, %2, %3";
}
  [(set_attr "type" "alu")
   (set_attr "length" "4")])

(define_insn "*sub_srli"
  [(set (match_operand:SI 0 "register_operand"                         "=   r")
	(minus:SI (match_operand:SI 1 "register_operand"               "    r")
		  (lshiftrt:SI (match_operand:SI 2 "register_operand"  "    r")
			       (match_operand:SI 3 "immediate_operand" " Iu05"))))]
  "TARGET_ISA_V3"
  "sub_srli\t%0, %1, %2, %3"
  [(set_attr "type" "alu")
   (set_attr "length" "4")])


;; Multiplication instructions.

(define_insn "mulsi3"
  [(set (match_operand:SI 0 "register_operand"          "=w, r")
	(mult:SI (match_operand:SI 1 "register_operand" "%0, r")
		 (match_operand:SI 2 "register_operand" " w, r")))]
  ""
  "@
  mul33\t%0, %2
  mul\t%0, %1, %2"
  [(set_attr "type"   "alu,alu")
   (set_attr "length" "  2,  4")])

(define_insn "mulsidi3"
  [(set (match_operand:DI 0 "register_operand"                          "=r")
	(mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand" " r"))
		 (sign_extend:DI (match_operand:SI 2 "register_operand" " r"))))]
  "TARGET_ISA_V2 || TARGET_ISA_V3"
  "mulsr64\t%0, %1, %2"
  [(set_attr "type"   "alu")
   (set_attr "length"   "4")])

(define_insn "umulsidi3"
  [(set (match_operand:DI 0 "register_operand"                          "=r")
	(mult:DI (zero_extend:DI (match_operand:SI 1 "register_operand" " r"))
		 (zero_extend:DI (match_operand:SI 2 "register_operand" " r"))))]
  "TARGET_ISA_V2 || TARGET_ISA_V3"
  "mulr64\t%0, %1, %2"
  [(set_attr "type"   "alu")
   (set_attr "length"   "4")])


;; Multiply-accumulate instructions.

(define_insn "*maddr32_0"
  [(set (match_operand:SI 0 "register_operand"                   "=r")
        (plus:SI (match_operand:SI 3 "register_operand"          " 0")
                 (mult:SI (match_operand:SI 1 "register_operand" " r")
                          (match_operand:SI 2 "register_operand" " r"))))]
  ""
  "maddr32\t%0, %1, %2"
  [(set_attr "type"   "alu")
   (set_attr "length"   "4")])

(define_insn "*maddr32_1"
  [(set (match_operand:SI 0 "register_operand"                   "=r")
        (plus:SI (mult:SI (match_operand:SI 1 "register_operand" " r")
                          (match_operand:SI 2 "register_operand" " r"))
                 (match_operand:SI 3 "register_operand"          " 0")))]
  ""
  "maddr32\t%0, %1, %2"
  [(set_attr "type"   "alu")
   (set_attr "length"   "4")])

(define_insn "*msubr32"
  [(set (match_operand:SI 0 "register_operand"                    "=r")
        (minus:SI (match_operand:SI 3 "register_operand"          " 0")
                  (mult:SI (match_operand:SI 1 "register_operand" " r")
                           (match_operand:SI 2 "register_operand" " r"))))]
  ""
  "msubr32\t%0, %1, %2"
  [(set_attr "type"   "alu")
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
  [(set_attr "type"   "alu")
   (set_attr "length"   "4")])

(define_insn "udivmodsi4"
  [(set (match_operand:SI 0 "register_operand"          "=r")
        (udiv:SI (match_operand:SI 1 "register_operand" " r")
                (match_operand:SI 2 "register_operand"  " r")))
   (set (match_operand:SI 3 "register_operand"          "=r")
        (umod:SI (match_dup 1) (match_dup 2)))]
  ""
  "divr\t%0, %3, %1, %2"
  [(set_attr "type"   "alu")
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

(define_insn "andsi3"
  [(set (match_operand:SI 0 "register_operand"         "=w, r,    l,    l,    l,    l,    l,    l,    r,   r,     r,    r,    r")
	(and:SI (match_operand:SI 1 "register_operand" "%0, r,    l,    l,    l,    l,    0,    0,    r,   r,     r,    r,    r")
		(match_operand:SI 2 "general_operand"  " w, r, Izeb, Izeh, Ixls, Ix11, Ibms, Ifex, Izeb, Izeh, Iu15, Ii15, Ic15")))]
  ""
{
  HOST_WIDE_INT mask = INTVAL (operands[2]);
  int zero_position;

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
      operands[2] = GEN_INT (floor_log2 (mask));
      return "bmski33\t%0, %2";
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
      /* If we reach this alternative,
         it must pass the nds32_can_use_bclr_p() test,
         so that we can guarantee there is only one 0-bit
         within the immediate value.  */
      for (zero_position = 31; zero_position >= 0; zero_position--)
	{
	  if ((INTVAL (operands[2]) & (1 << zero_position)) == 0)
	    {
	      /* Found the 0-bit position.  */
	      operands[2] = GEN_INT (zero_position);
	      break;
	    }
	}
      return "bclr\t%0, %1, %2";

    default:
      gcc_unreachable ();
    }
}
  [(set_attr "type"   "alu,alu,alu,alu,alu,alu,alu,alu,alu,alu,alu,alu,alu")
   (set_attr "length" "  2,  4,  2,  2,  2,  2,  2,  2,  4,  4,  4,  4,  4")])

(define_insn "*and_slli"
  [(set (match_operand:SI 0 "register_operand"                      "=   r")
	(and:SI (ashift:SI (match_operand:SI 1 "register_operand"   "    r")
			    (match_operand:SI 2 "immediate_operand" " Iu05"))
		(match_operand:SI 3 "register_operand"              "    r")))]
  "TARGET_ISA_V3"
  "and_slli\t%0, %3, %1, %2"
  [(set_attr "type" "alu")
   (set_attr "length" "4")])

(define_insn "*and_srli"
  [(set (match_operand:SI 0 "register_operand"                       "=   r")
	(and:SI (lshiftrt:SI (match_operand:SI 1 "register_operand"  "    r")
			     (match_operand:SI 2 "immediate_operand" " Iu05"))
		(match_operand:SI 3 "register_operand"               "    r")))]
  "TARGET_ISA_V3"
  "and_srli\t%0, %3, %1, %2"
  [(set_attr "type" "alu")
   (set_attr "length" "4")])


;; ----------------------------------------------------------------------------
;; 'OR' operation
;; ----------------------------------------------------------------------------

;; For V3/V3M ISA, we have 'or33' instruction.
;; So we can identify 'or Rt3,Rt3,Ra3' case and set its length to be 2.
(define_insn "iorsi3"
  [(set (match_operand:SI 0 "register_operand"         "=w, r,    r,    r")
	(ior:SI (match_operand:SI 1 "register_operand" "%0, r,    r,    r")
		(match_operand:SI 2 "general_operand"  " w, r, Iu15, Ie15")))]
  ""
{
  int one_position;

  switch (which_alternative)
    {
    case 0:
      return "or33\t%0, %2";
    case 1:
      return "or\t%0, %1, %2";
    case 2:
      return "ori\t%0, %1, %2";
    case 3:
      /* If we reach this alternative,
         it must pass the nds32_can_use_bset_p() test,
         so that we can guarantee there is only one 1-bit
         within the immediate value.  */
      /* Use exact_log2() to search the 1-bit position.  */
      one_position = exact_log2 (INTVAL (operands[2]));
      operands[2] = GEN_INT (one_position);
      return "bset\t%0, %1, %2";

    default:
      gcc_unreachable ();
    }
}
  [(set_attr "type"   "alu,alu,alu,alu")
   (set_attr "length" "  2,  4,  4,  4")])

(define_insn "*or_slli"
  [(set (match_operand:SI 0 "register_operand"                     "=   r")
	(ior:SI (ashift:SI (match_operand:SI 1 "register_operand"  "    r")
			   (match_operand:SI 2 "immediate_operand" " Iu05"))
		(match_operand:SI 3 "register_operand"             "    r")))]
  "TARGET_ISA_V3"
  "or_slli\t%0, %3, %1, %2"
  [(set_attr "type" "alu")
   (set_attr "length" "4")])

(define_insn "*or_srli"
  [(set (match_operand:SI 0 "register_operand"                       "=   r")
	(ior:SI (lshiftrt:SI (match_operand:SI 1 "register_operand"  "    r")
			     (match_operand:SI 2 "immediate_operand" " Iu05"))
		(match_operand:SI 3 "register_operand"               "    r")))]
  "TARGET_ISA_V3"
  "or_srli\t%0, %3, %1, %2"
  [(set_attr "type" "alu")
   (set_attr "length" "4")])


;; ----------------------------------------------------------------------------
;; 'XOR' operation
;; ----------------------------------------------------------------------------

;; For V3/V3M ISA, we have 'xor33' instruction.
;; So we can identify 'xor Rt3,Rt3,Ra3' case and set its length to be 2.
(define_insn "xorsi3"
  [(set (match_operand:SI 0 "register_operand"         "=w, r,    r,    r")
	(xor:SI (match_operand:SI 1 "register_operand" "%0, r,    r,    r")
		(match_operand:SI 2 "general_operand"  " w, r, Iu15, It15")))]
  ""
{
  int one_position;

  switch (which_alternative)
    {
    case 0:
      return "xor33\t%0, %2";
    case 1:
      return "xor\t%0, %1, %2";
    case 2:
      return "xori\t%0, %1, %2";
    case 3:
      /* If we reach this alternative,
         it must pass the nds32_can_use_btgl_p() test,
         so that we can guarantee there is only one 1-bit
         within the immediate value.  */
      /* Use exact_log2() to search the 1-bit position.  */
      one_position = exact_log2 (INTVAL (operands[2]));
      operands[2] = GEN_INT (one_position);
      return "btgl\t%0, %1, %2";

    default:
      gcc_unreachable ();
    }
}
  [(set_attr "type"   "alu,alu,alu,alu")
   (set_attr "length" "  2,  4,  4,  4")])

(define_insn "*xor_slli"
  [(set (match_operand:SI 0 "register_operand"                     "=   r")
	(xor:SI (ashift:SI (match_operand:SI 1 "register_operand"  "    r")
			   (match_operand:SI 2 "immediate_operand" " Iu05"))
		(match_operand:SI 3 "register_operand"             "    r")))]
  "TARGET_ISA_V3"
  "xor_slli\t%0, %3, %1, %2"
  [(set_attr "type" "alu")
   (set_attr "length" "4")])

(define_insn "*xor_srli"
  [(set (match_operand:SI 0 "register_operand"                       "=   r")
	(xor:SI (lshiftrt:SI (match_operand:SI 1 "register_operand"  "    r")
			     (match_operand:SI 2 "immediate_operand" " Iu05"))
		(match_operand:SI 3 "register_operand"               "    r")))]
  "TARGET_ISA_V3"
  "xor_srli\t%0, %3, %1, %2"
  [(set_attr "type" "alu")
   (set_attr "length" "4")])

;; Rotate Right Instructions.

(define_insn "rotrsi3"
  [(set (match_operand:SI 0 "register_operand"                 "=   r, r")
	  (rotatert:SI (match_operand:SI 1 "register_operand"  "    r, r")
		       (match_operand:SI 2 "nonmemory_operand" " Iu05, r")))]
  ""
  "@
  rotri\t%0, %1, %2
  rotr\t%0, %1, %2"
  [(set_attr "type"   "alu,alu")
   (set_attr "length" "  4,  4")])


;; ----------------------------------------------------------------------------
;; 'NEG' operation
;; ----------------------------------------------------------------------------

;; For V3/V3M ISA, we have 'neg33' instruction.
;; So we can identify 'xor Rt3,Ra3' case and set its length to be 2.
;; And for V2 ISA, there is NO 'neg33' instruction.
;; The only option is to use 'subri A,B,0' (its semantic is 'A = 0 - B').
(define_insn "negsi2"
  [(set (match_operand:SI 0 "register_operand"         "=w, r")
	(neg:SI (match_operand:SI 1 "register_operand" " w, r")))]
  ""
  "@
   neg33\t%0, %1
   subri\t%0, %1, 0"
  [(set_attr "type"   "alu,alu")
   (set_attr "length" "  2,  4")])


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
  [(set_attr "type"   "alu,alu")
   (set_attr "length" "  2,  4")])


;; ----------------------------------------------------------------------------

;; Shift instructions.

(define_insn "ashlsi3"
  [(set (match_operand:SI 0 "register_operand"             "=   l,    r, r")
	(ashift:SI (match_operand:SI 1 "register_operand"  "    l,    r, r")
		   (match_operand:SI 2 "nonmemory_operand" " Iu03, Iu05, r")))]
  ""
  "@
  slli333\t%0, %1, %2
  slli\t%0, %1, %2
  sll\t%0, %1, %2"
  [(set_attr "type"   "alu,alu,alu")
   (set_attr "length" "  2,  4,  4")])

(define_insn "ashrsi3"
  [(set (match_operand:SI 0 "register_operand"               "=   d,    r, r")
	(ashiftrt:SI (match_operand:SI 1 "register_operand"  "    0,    r, r")
		     (match_operand:SI 2 "nonmemory_operand" " Iu05, Iu05, r")))]
  ""
  "@
  srai45\t%0, %2
  srai\t%0, %1, %2
  sra\t%0, %1, %2"
  [(set_attr "type"   "alu,alu,alu")
   (set_attr "length" "  2,  4,  4")])

(define_insn "lshrsi3"
  [(set (match_operand:SI 0 "register_operand"               "=   d,    r, r")
	(lshiftrt:SI (match_operand:SI 1 "register_operand"  "    0,    r, r")
		     (match_operand:SI 2 "nonmemory_operand" " Iu05, Iu05, r")))]
  ""
  "@
  srli45\t%0, %2
  srli\t%0, %1, %2
  srl\t%0, %1, %2"
  [(set_attr "type"   "alu,alu,alu")
   (set_attr "length" "  2,  4,  4")])


;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;; Conditional Move patterns
;; ----------------------------------------------------------------------------

(define_expand "movsicc"
  [(set (match_operand:SI 0 "register_operand" "")
	(if_then_else:SI (match_operand 1 "comparison_operator" "")
			 (match_operand:SI 2 "register_operand" "")
			 (match_operand:SI 3 "register_operand" "")))]
  "TARGET_CMOV"
{
  if ((GET_CODE (operands[1]) == EQ || GET_CODE (operands[1]) == NE)
      && GET_MODE (XEXP (operands[1], 0)) == SImode
      && XEXP (operands[1], 1) == const0_rtx)
    {
      /* If the operands[1] rtx is already (eq X 0) or (ne X 0),
         we have gcc generate original template rtx.  */
      goto create_template;
    }
  else
    {
      /* Since there is only 'slt'(Set when Less Than) instruction for
         comparison in Andes ISA, the major strategy we use here is to
         convert conditional move into 'LT + EQ' or 'LT + NE' rtx combination.
         We design constraints properly so that the reload phase will assist
         to make one source operand to use same register as result operand.
         Then we can use cmovz/cmovn to catch the other source operand
         which has different register.  */
      enum rtx_code code = GET_CODE (operands[1]);
      enum rtx_code new_code = code;
      rtx cmp_op0 = XEXP (operands[1], 0);
      rtx cmp_op1 = XEXP (operands[1], 1);
      rtx tmp;
      int reverse = 0;

      /* Main Goal: Use 'LT + EQ' or 'LT + NE' to target "then" part
         Strategy : Reverse condition and swap comparison operands

         For example:

             a <= b ? P : Q   (LE or LEU)
         --> a >  b ? Q : P   (reverse condition)
         --> b <  a ? Q : P   (swap comparison operands to achieve 'LT/LTU')

             a >= b ? P : Q   (GE or GEU)
         --> a <  b ? Q : P   (reverse condition to achieve 'LT/LTU')

             a <  b ? P : Q   (LT or LTU)
         --> (NO NEED TO CHANGE, it is already 'LT/LTU')

             a >  b ? P : Q   (GT or GTU)
         --> b <  a ? P : Q   (swap comparison operands to achieve 'LT/LTU') */
      switch (code)
	{
	case NE:
	  /*   (a != b ? P : Q)
	     can be expressed as
	       (a == b ? Q : P)
	     so, fall through to reverse condition */
	case GE: case GEU: case LE: case LEU:
	  new_code = reverse_condition (code);
	  reverse = 1;
	  break;
	case EQ: case GT: case GTU: case LT: case LTU:
	  /* no need to reverse condition */
	  break;
	default:
	  FAIL;
	}

      /* For '>' comparison operator, we swap operands
         so that we can have 'LT/LTU' operator.  */
      if (new_code == GT || new_code == GTU)
	{
	  tmp     = cmp_op0;
	  cmp_op0 = cmp_op1;
	  cmp_op1 = tmp;

	  new_code = swap_condition (new_code);
	}

      /* Use a temporary register to store slt/slts result.  */
      tmp = gen_reg_rtx (SImode);

      /* Split EQ and NE because we don't have direct comparison of EQ and NE.
         If we don't split it, the conditional move transformation will fail
         when producing (SET A (EQ B C)) or (SET A (NE B C)).  */
      if (new_code == EQ)
	{
	  emit_insn (gen_xorsi3 (tmp, cmp_op0, cmp_op1));
	  emit_insn (gen_slt_compare (tmp, tmp, GEN_INT (1)));
	}
      else if (new_code == NE)
	{
	  emit_insn (gen_xorsi3 (tmp, cmp_op0, cmp_op1));
	  emit_insn (gen_slt_compare (tmp, GEN_INT (0), tmp));
        }
      else
	/* This emit_insn will create corresponding 'slt/slts' insturction.  */
	emit_insn (gen_rtx_SET (tmp, gen_rtx_fmt_ee (new_code, SImode,
						     cmp_op0, cmp_op1)));

      /* Change comparison semantic into (eq X 0) or (ne X 0) behavior
         so that cmovz or cmovn will be matched later.

         For reverse condition cases, we want to create a semantic that:
           (eq X 0) --> pick up "else" part
         For normal cases, we want to create a semantic that:
           (ne X 0) --> pick up "then" part

         Later we will have cmovz/cmovn instruction pattern to
         match corresponding behavior and output instruction.  */
      operands[1] = gen_rtx_fmt_ee (reverse ? EQ : NE,
				    VOIDmode, tmp, const0_rtx);
    }

create_template:
  do {} while(0); /* dummy line */
})

(define_insn "cmovz"
  [(set (match_operand:SI 0 "register_operand"                      "=r, r")
        (if_then_else:SI (eq (match_operand:SI 1 "register_operand" " r, r")
			     (const_int 0))
			 (match_operand:SI 2 "register_operand"     " r, 0")
			 (match_operand:SI 3 "register_operand"     " 0, r")))]
  "TARGET_CMOV"
  "@
   cmovz\t%0, %2, %1
   cmovn\t%0, %3, %1"
  [(set_attr "type" "move")
   (set_attr "length"  "4")])

(define_insn "cmovn"
  [(set (match_operand:SI 0 "register_operand"                      "=r, r")
	(if_then_else:SI (ne (match_operand:SI 1 "register_operand" " r, r")
			     (const_int 0))
			 (match_operand:SI 2 "register_operand"     " r, 0")
			 (match_operand:SI 3 "register_operand"     " 0, r")))]
  "TARGET_CMOV"
  "@
   cmovn\t%0, %2, %1
   cmovz\t%0, %3, %1"
  [(set_attr "type" "move")
   (set_attr "length"  "4")])


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
  rtx tmp_reg;
  enum rtx_code code;

  code = GET_CODE (operands[0]);

  /* If operands[2] is (const_int 0),
     we can use beqz,bnez,bgtz,bgez,bltz,or blez instructions.
     So we have gcc generate original template rtx.  */
  if (GET_CODE (operands[2]) == CONST_INT)
    if (INTVAL (operands[2]) == 0)
      if ((code != GTU)
	  && (code != GEU)
	  && (code != LTU)
	  && (code != LEU))
	goto create_template;

  /* For other comparison, NDS32 ISA only has slt (Set-on-Less-Than)
     behavior for the comparison, we might need to generate other
     rtx patterns to achieve same semantic.  */
  switch (code)
    {
    case GT:
    case GTU:
      if (GET_CODE (operands[2]) == CONST_INT)
	{
	  /* GT  reg_A, const_int  =>  !(LT  reg_A, const_int + 1) */
	  tmp_reg = gen_rtx_REG (SImode, TA_REGNUM);

	  /* We want to plus 1 into the integer value
	     of operands[2] to create 'slt' instruction.
	     This caculation is performed on the host machine,
	     which may be 64-bit integer.
	     So the meaning of caculation result may be
	     different from the 32-bit nds32 target.

	     For example:
	       0x7fffffff + 0x1 -> 0x80000000,
	       this value is POSITIVE on 64-bit machine,
	       but the expected value on 32-bit nds32 target
	       should be NEGATIVE value.

	     Hence, instead of using GEN_INT(), we use gen_int_mode() to
	     explicitly create SImode constant rtx.  */
	  operands[2] = gen_int_mode (INTVAL (operands[2]) + 1, SImode);

	  if (code == GT)
	    {
	      /* GT, use slts instruction */
	      emit_insn (gen_slts_compare (tmp_reg, operands[1], operands[2]));
	    }
	  else
	    {
	      /* GTU, use slt instruction */
	      emit_insn (gen_slt_compare  (tmp_reg, operands[1], operands[2]));
	    }

	  PUT_CODE (operands[0], EQ);
	  operands[1] = tmp_reg;
	  operands[2] = const0_rtx;
	  emit_insn (gen_cbranchsi4 (operands[0], operands[1],
				     operands[2], operands[3]));

	  DONE;
	}
      else
	{
	  /* GT  reg_A, reg_B  =>  LT  reg_B, reg_A */
	  tmp_reg = gen_rtx_REG (SImode, TA_REGNUM);

	  if (code == GT)
	    {
	      /* GT, use slts instruction */
	      emit_insn (gen_slts_compare (tmp_reg, operands[2], operands[1]));
	    }
	  else
	    {
	      /* GTU, use slt instruction */
	      emit_insn (gen_slt_compare  (tmp_reg, operands[2], operands[1]));
	    }

	  PUT_CODE (operands[0], NE);
	  operands[1] = tmp_reg;
	  operands[2] = const0_rtx;
	  emit_insn (gen_cbranchsi4 (operands[0], operands[1],
				     operands[2], operands[3]));

	  DONE;
	}

    case GE:
    case GEU:
      /* GE  reg_A, reg_B      =>  !(LT  reg_A, reg_B) */
      /* GE  reg_A, const_int  =>  !(LT  reg_A, const_int) */
      tmp_reg = gen_rtx_REG (SImode, TA_REGNUM);

      if (code == GE)
	{
	  /* GE, use slts instruction */
	  emit_insn (gen_slts_compare (tmp_reg, operands[1], operands[2]));
	}
      else
	{
	  /* GEU, use slt instruction */
	  emit_insn (gen_slt_compare  (tmp_reg, operands[1], operands[2]));
	}

      PUT_CODE (operands[0], EQ);
      operands[1] = tmp_reg;
      operands[2] = const0_rtx;
      emit_insn (gen_cbranchsi4 (operands[0], operands[1],
				 operands[2], operands[3]));

      DONE;

    case LT:
    case LTU:
      /* LT  reg_A, reg_B      =>  LT  reg_A, reg_B */
      /* LT  reg_A, const_int  =>  LT  reg_A, const_int */
      tmp_reg = gen_rtx_REG (SImode, TA_REGNUM);

      if (code == LT)
	{
	  /* LT, use slts instruction */
	  emit_insn (gen_slts_compare (tmp_reg, operands[1], operands[2]));
	}
      else
	{
	  /* LTU, use slt instruction */
	  emit_insn (gen_slt_compare  (tmp_reg, operands[1], operands[2]));
	}

      PUT_CODE (operands[0], NE);
      operands[1] = tmp_reg;
      operands[2] = const0_rtx;
      emit_insn (gen_cbranchsi4 (operands[0], operands[1],
				 operands[2], operands[3]));

      DONE;

    case LE:
    case LEU:
      if (GET_CODE (operands[2]) == CONST_INT)
	{
	  /* LE  reg_A, const_int  =>  LT  reg_A, const_int + 1 */
	  tmp_reg = gen_rtx_REG (SImode, TA_REGNUM);

	  /* Note that (le:SI X INT_MAX) is not the same as (lt:SI X INT_MIN).
	     We better have an assert here in case GCC does not properly
	     optimize it away.  The INT_MAX here is 0x7fffffff for target.  */
	  gcc_assert (code != LE || INTVAL (operands[2]) != 0x7fffffff);
	  operands[2] = gen_int_mode (INTVAL (operands[2]) + 1, SImode);

	  if (code == LE)
	    {
	      /* LE, use slts instruction */
	      emit_insn (gen_slts_compare (tmp_reg, operands[1], operands[2]));
	    }
	  else
	    {
	      /* LEU, use slt instruction */
	      emit_insn (gen_slt_compare  (tmp_reg, operands[1], operands[2]));
	    }

	  PUT_CODE (operands[0], NE);
	  operands[1] = tmp_reg;
	  operands[2] = const0_rtx;
	  emit_insn (gen_cbranchsi4 (operands[0], operands[1],
				     operands[2], operands[3]));

	  DONE;
	}
      else
	{
	  /* LE  reg_A, reg_B  =>  !(LT  reg_B, reg_A) */
	  tmp_reg = gen_rtx_REG (SImode, TA_REGNUM);

	  if (code == LE)
	    {
	      /* LE, use slts instruction */
	      emit_insn (gen_slts_compare (tmp_reg, operands[2], operands[1]));
	    }
	  else
	    {
	      /* LEU, use slt instruction */
	      emit_insn (gen_slt_compare  (tmp_reg, operands[2], operands[1]));
	    }

	  PUT_CODE (operands[0], EQ);
	  operands[1] = tmp_reg;
	  operands[2] = const0_rtx;
	  emit_insn (gen_cbranchsi4 (operands[0], operands[1],
				     operands[2], operands[3]));

	  DONE;
	}

    case EQ:
    case NE:
      /* NDS32 ISA has various form for eq/ne behavior no matter
         what kind of the operand is.
         So just generate original template rtx.  */
      goto create_template;

    default:
      FAIL;
    }

create_template:
  do {} while(0); /* dummy line */
})


(define_insn "*cbranchsi4_equality_zero"
  [(set (pc)
	(if_then_else (match_operator 0 "nds32_equality_comparison_operator"
			[(match_operand:SI 1 "register_operand"  "t, l, r")
			 (const_int 0)])
		      (label_ref (match_operand 2 "" ""))
		      (pc)))]
  ""
{
  enum rtx_code code;

  code = GET_CODE (operands[0]);

  /* This zero-comparison conditional branch has two forms:
       32-bit instruction =>          beqz/bnez           imm16s << 1
       16-bit instruction => beqzs8/bnezs8/beqz38/bnez38  imm8s << 1

     For 32-bit case,
     we assume it is always reachable. (but check range -65500 ~ 65500)

     For 16-bit case,
     it must satisfy { 255 >= (label - pc) >= -256 } condition.
     However, since the $pc for nds32 is at the beginning of the instruction,
     we should leave some length space for current insn.
     So we use range -250 ~ 250.  */

  switch (get_attr_length (insn))
    {
    case 2:
      if (which_alternative == 0)
	{
	  /* constraint: t */
	  return (code == EQ) ? "beqzs8\t%2" : "bnezs8\t%2";
	}
      else if (which_alternative == 1)
	{
	  /* constraint: l */
	  return (code == EQ) ? "beqz38\t%1, %2" : "bnez38\t%1, %2";
	}
      else
	{
	  /* constraint: r */
	  /* For which_alternative==2, it should not be here.  */
	  gcc_unreachable ();
	}
    case 4:
      /* including constraints: t, l, and r */
      return (code == EQ) ? "beqz\t%1, %2" : "bnez\t%1, %2";
    case 6:
      if (which_alternative == 0)
	{
	  /* constraint: t */
	  if (code == EQ)
	    {
	      /*    beqzs8  .L0
	          =>
	            bnezs8  .LCB0
	            j  .L0
	          .LCB0:
	       */
	      return "bnezs8\t.LCB%=\;j\t%2\n.LCB%=:";
	    }
	  else
	    {
	      /*    bnezs8  .L0
	          =>
	            beqzs8  .LCB0
	            j  .L0
	          .LCB0:
	       */
	      return "beqzs8\t.LCB%=\;j\t%2\n.LCB%=:";
	    }
	}
      else if (which_alternative == 1)
	{
	  /* constraint: l */
	  if (code == EQ)
	    {
	      /*    beqz38  $r0, .L0
	          =>
	            bnez38  $r0, .LCB0
	            j  .L0
	          .LCB0:
	       */
	      return "bnez38\t%1, .LCB%=\;j\t%2\n.LCB%=:";
	    }
	  else
	    {
	      /*    bnez38  $r0, .L0
	          =>
	            beqz38  $r0, .LCB0
	            j  .L0
	          .LCB0:
	       */
	      return "beqz38\t%1, .LCB%=\;j\t%2\n.LCB%=:";
	    }
	}
      else
	{
	  /* constraint: r */
	  /* For which_alternative==2, it should not be here.  */
	  gcc_unreachable ();
	}
    case 8:
      /* constraint: t, l, r.  */
      if (code == EQ)
	{
	  /*    beqz  $r8, .L0
	      =>
	        bnez  $r8, .LCB0
	        j  .L0
	      .LCB0:
	   */
	  return "bnez\t%1, .LCB%=\;j\t%2\n.LCB%=:";
	}
      else
	{
	  /*    bnez  $r8, .L0
	      =>
	        beqz  $r8, .LCB0
	        j  .L0
	      .LCB0:
	   */
	  return "beqz\t%1, .LCB%=\;j\t%2\n.LCB%=:";
	}
    default:
      gcc_unreachable ();
    }
}
  [(set_attr "type" "branch")
   (set_attr "enabled" "1")
   (set_attr_alternative "length"
     [
       ;; Alternative 0
       (if_then_else (and (ge (minus (match_dup 2) (pc)) (const_int -250))
			  (le (minus (match_dup 2) (pc)) (const_int  250)))
		     (if_then_else (match_test "TARGET_16_BIT")
				   (const_int 2)
				   (const_int 4))
		     (if_then_else (and (ge (minus (match_dup 2) (pc)) (const_int -65500))
					(le (minus (match_dup 2) (pc)) (const_int  65500)))
				   (const_int 4)
				   (if_then_else (match_test "TARGET_16_BIT")
						 (const_int 6)
						 (const_int 8))))
       ;; Alternative 1
       (if_then_else (and (ge (minus (match_dup 2) (pc)) (const_int -250))
			  (le (minus (match_dup 2) (pc)) (const_int  250)))
		     (if_then_else (match_test "TARGET_16_BIT")
				   (const_int 2)
				   (const_int 4))
		     (if_then_else (and (ge (minus (match_dup 2) (pc)) (const_int -65500))
					(le (minus (match_dup 2) (pc)) (const_int  65500)))
				   (const_int 4)
				   (if_then_else (match_test "TARGET_16_BIT")
						 (const_int 6)
						 (const_int 8))))
       ;; Alternative 2
       (if_then_else (and (ge (minus (match_dup 2) (pc)) (const_int -65500))
			  (le (minus (match_dup 2) (pc)) (const_int  65500)))
		     (const_int 4)
		     (const_int 8))
     ])])


;; This pattern is dedicated to V2 ISA,
;; because V2 DOES NOT HAVE beqc/bnec instruction.
(define_insn "*cbranchsi4_equality_reg"
  [(set (pc)
	(if_then_else (match_operator 0 "nds32_equality_comparison_operator"
			[(match_operand:SI 1 "register_operand"           "r")
			 (match_operand:SI 2 "nds32_reg_constant_operand" "r")])
		      (label_ref (match_operand 3 "" ""))
		      (pc)))]
  "TARGET_ISA_V2"
{
  enum rtx_code code;

  code = GET_CODE (operands[0]);

  /* This register-comparison conditional branch has one form:
       32-bit instruction =>          beq/bne           imm14s << 1

     For 32-bit case,
     we assume it is always reachable. (but check range -16350 ~ 16350).  */

  switch (code)
    {
    case EQ:
      /* r, r */
      switch (get_attr_length (insn))
	{
	case 4:
	  return "beq\t%1, %2, %3";
	case 8:
	  /*    beq  $r0, $r1, .L0
	      =>
	        bne  $r0, $r1, .LCB0
	        j  .L0
	      .LCB0:
	   */
	  return "bne\t%1, %2, .LCB%=\;j\t%3\n.LCB%=:";
	default:
	  gcc_unreachable ();
	}

    case NE:
      /* r, r */
      switch (get_attr_length (insn))
	{
	case 4:
	  return "bne\t%1, %2, %3";
	case 8:
	  /*    bne  $r0, $r1, .L0
	      =>
	        beq  $r0, $r1, .LCB0
	        j  .L0
	      .LCB0:
	   */
	  return "beq\t%1, %2, .LCB%=\;j\t%3\n.LCB%=:";
	default:
	  gcc_unreachable ();
	}

    default:
      gcc_unreachable ();
    }
}
  [(set_attr "type"   "branch")
   (set (attr "length")
	(if_then_else (and (ge (minus (match_dup 3) (pc)) (const_int -16350))
			   (le (minus (match_dup 3) (pc)) (const_int  16350)))
		      (const_int 4)
		      (const_int 8)))])


;; This pattern is dedicated to V3/V3M,
;; because V3/V3M DO HAVE beqc/bnec instruction.
(define_insn "*cbranchsi4_equality_reg_or_const_int"
  [(set (pc)
	(if_then_else (match_operator 0 "nds32_equality_comparison_operator"
			[(match_operand:SI 1 "register_operand"           "r,    r")
			 (match_operand:SI 2 "nds32_reg_constant_operand" "r, Is11")])
		      (label_ref (match_operand 3 "" ""))
		      (pc)))]
  "TARGET_ISA_V3 || TARGET_ISA_V3M"
{
  enum rtx_code code;

  code = GET_CODE (operands[0]);

  /* This register-comparison conditional branch has one form:
       32-bit instruction =>          beq/bne           imm14s << 1
       32-bit instruction =>         beqc/bnec          imm8s << 1

     For 32-bit case, we assume it is always reachable.
     (but check range -16350 ~ 16350 and -250 ~ 250).  */

  switch (code)
    {
    case EQ:
      if (which_alternative == 0)
	{
	  /* r, r */
	  switch (get_attr_length (insn))
	    {
	    case 4:
	      return "beq\t%1, %2, %3";
	    case 8:
	      /*    beq  $r0, $r1, .L0
	          =>
	            bne  $r0, $r1, .LCB0
	            j  .L0
	          .LCB0:
	       */
	      return "bne\t%1, %2, .LCB%=\;j\t%3\n.LCB%=:";
	    default:
	      gcc_unreachable ();
	    }
	}
      else
	{
	  /* r, Is11 */
	  switch (get_attr_length (insn))
	    {
	    case 4:
	      return "beqc\t%1, %2, %3";
	    case 8:
	      /*    beqc  $r0, constant, .L0
	          =>
	            bnec  $r0, constant, .LCB0
	            j  .L0
	          .LCB0:
	       */
	      return "bnec\t%1, %2, .LCB%=\;j\t%3\n.LCB%=:";
	    default:
	      gcc_unreachable ();
	    }
	}
    case NE:
      if (which_alternative == 0)
	{
	  /* r, r */
	  switch (get_attr_length (insn))
	    {
	    case 4:
	      return "bne\t%1, %2, %3";
	    case 8:
	      /*    bne  $r0, $r1, .L0
	          =>
	            beq  $r0, $r1, .LCB0
	            j  .L0
	          .LCB0:
	       */
	      return "beq\t%1, %2, .LCB%=\;j\t%3\n.LCB%=:";
	    default:
	      gcc_unreachable ();
	    }
	}
      else
	{
	  /* r, Is11 */
	  switch (get_attr_length (insn))
	    {
	    case 4:
	      return "bnec\t%1, %2, %3";
	    case 8:
	      /*    bnec  $r0, constant, .L0
	          =>
	            beqc  $r0, constant, .LCB0
	            j  .L0
	          .LCB0:
	       */
	      return "beqc\t%1, %2, .LCB%=\;j\t%3\n.LCB%=:";
	    default:
	      gcc_unreachable ();
	    }
	}
    default:
      gcc_unreachable ();
    }
}
  [(set_attr "type"   "branch")
   (set_attr_alternative "length"
     [
       ;; Alternative 0
       (if_then_else (and (ge (minus (match_dup 3) (pc)) (const_int -16350))
			  (le (minus (match_dup 3) (pc)) (const_int  16350)))
		     (const_int 4)
		     (const_int 8))
       ;; Alternative 1
       (if_then_else (and (ge (minus (match_dup 3) (pc)) (const_int -250))
			  (le (minus (match_dup 3) (pc)) (const_int  250)))
		     (const_int 4)
		     (const_int 8))
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
  enum rtx_code code;

  code = GET_CODE (operands[0]);

  /* This zero-greater-less-comparison conditional branch has one form:
       32-bit instruction =>      bgtz/bgez/bltz/blez     imm16s << 1

     For 32-bit case, we assume it is always reachable.
     (but check range -65500 ~ 65500).  */

  if (get_attr_length (insn) == 8)
    {
      /* The branch target is too far to simply use one
         bgtz/bgez/bltz/blez instruction.
         We need to reverse condition and use 'j' to jump to the target.  */
      switch (code)
	{
	case GT:
	  /*   bgtz  $r8, .L0
	     =>
	       blez  $r8, .LCB0
	       j  .L0
	     .LCB0:
	   */
	  return "blez\t%1, .LCB%=\;j\t%2\n.LCB%=:";
	case GE:
	  /*   bgez  $r8, .L0
	     =>
	       bltz  $r8, .LCB0
	       j  .L0
	     .LCB0:
	   */
	  return "bltz\t%1, .LCB%=\;j\t%2\n.LCB%=:";
	case LT:
	  /*   bltz  $r8, .L0
	     =>
	       bgez  $r8, .LCB0
	       j  .L0
	     .LCB0:
	   */
	  return "bgez\t%1, .LCB%=\;j\t%2\n.LCB%=:";
	case LE:
	  /*   blez  $r8, .L0
	     =>
	       bgtz  $r8, .LCB0
	       j  .L0
	     .LCB0:
	   */
	  return "bgtz\t%1, .LCB%=\;j\t%2\n.LCB%=:";
	default:
	  gcc_unreachable ();
	}
    }

  switch (code)
    {
    case GT:
      return "bgtz\t%1, %2";
    case GE:
      return "bgez\t%1, %2";
    case LT:
      return "bltz\t%1, %2";
    case LE:
      return "blez\t%1, %2";
    default:
      gcc_unreachable ();
    }
}
  [(set_attr "type"   "branch")
   (set (attr "length")
        (if_then_else (and (ge (minus (match_dup 2) (pc)) (const_int -65500))
			   (le (minus (match_dup 2) (pc)) (const_int  65500)))
		      (const_int 4)
		      (const_int 8)))])


(define_expand "cstoresi4"
  [(set (match_operand:SI 0 "register_operand" "")
	(match_operator:SI 1 "comparison_operator"
	  [(match_operand:SI 2 "register_operand" "")
	   (match_operand:SI 3 "nonmemory_operand" "")]))]
  ""
{
  rtx tmp_reg;
  enum rtx_code code;

  code = GET_CODE (operands[1]);

  switch (code)
    {
    case EQ:
      if (GET_CODE (operands[3]) == CONST_INT)
	{
	  /* reg_R = (reg_A == const_int_B)
	     --> addi reg_C, reg_A, -const_int_B
	         slti reg_R, reg_C, const_int_1 */
	  tmp_reg = gen_reg_rtx (SImode);
	  operands[3] = gen_int_mode (-INTVAL (operands[3]), SImode);
	  /* If the integer value is not in the range of imm15s,
	     we need to force register first because our addsi3 pattern
	     only accept nds32_rimm15s_operand predicate.  */
	  if (!satisfies_constraint_Is15 (operands[3]))
	    operands[3] = force_reg (SImode, operands[3]);
	  emit_insn (gen_addsi3 (tmp_reg, operands[2], operands[3]));
	  emit_insn (gen_slt_compare (operands[0], tmp_reg, const1_rtx));

	  DONE;
	}
      else
	{
	  /* reg_R = (reg_A == reg_B)
	     --> xor  reg_C, reg_A, reg_B
	         slti reg_R, reg_C, const_int_1 */
	  tmp_reg = gen_reg_rtx (SImode);
	  emit_insn (gen_xorsi3 (tmp_reg, operands[2], operands[3]));
	  emit_insn (gen_slt_compare (operands[0], tmp_reg, const1_rtx));

	  DONE;
	}

    case NE:
      if (GET_CODE (operands[3]) == CONST_INT)
	{
	  /* reg_R = (reg_A != const_int_B)
	     --> addi reg_C, reg_A, -const_int_B
	         slti reg_R, const_int_0, reg_C */
	  tmp_reg = gen_reg_rtx (SImode);
	  operands[3] = gen_int_mode (-INTVAL (operands[3]), SImode);
	  /* If the integer value is not in the range of imm15s,
	     we need to force register first because our addsi3 pattern
	     only accept nds32_rimm15s_operand predicate.  */
	  if (!satisfies_constraint_Is15 (operands[3]))
	    operands[3] = force_reg (SImode, operands[3]);
	  emit_insn (gen_addsi3 (tmp_reg, operands[2], operands[3]));
	  emit_insn (gen_slt_compare (operands[0], const0_rtx, tmp_reg));

	  DONE;
	}
      else
	{
	  /* reg_R = (reg_A != reg_B)
	     --> xor  reg_C, reg_A, reg_B
	         slti reg_R, const_int_0, reg_C */
	  tmp_reg = gen_reg_rtx (SImode);
	  emit_insn (gen_xorsi3 (tmp_reg, operands[2], operands[3]));
	  emit_insn (gen_slt_compare (operands[0], const0_rtx, tmp_reg));

	  DONE;
	}

    case GT:
    case GTU:
      /* reg_R = (reg_A > reg_B)       --> slt reg_R, reg_B, reg_A */
      /* reg_R = (reg_A > const_int_B) --> slt reg_R, const_int_B, reg_A */
      if (code == GT)
	{
	  /* GT, use slts instruction */
	  emit_insn (gen_slts_compare (operands[0], operands[3], operands[2]));
	}
      else
	{
	  /* GTU, use slt instruction */
	  emit_insn (gen_slt_compare  (operands[0], operands[3], operands[2]));
	}

      DONE;

    case GE:
    case GEU:
      if (GET_CODE (operands[3]) == CONST_INT)
	{
	  /* reg_R = (reg_A >= const_int_B)
	     --> movi reg_C, const_int_B - 1
	         slt  reg_R, reg_C, reg_A */
	  tmp_reg = gen_reg_rtx (SImode);

	  emit_insn (gen_movsi (tmp_reg,
				gen_int_mode (INTVAL (operands[3]) - 1,
					      SImode)));
	  if (code == GE)
	    {
	      /* GE, use slts instruction */
	      emit_insn (gen_slts_compare (operands[0], tmp_reg, operands[2]));
	    }
	  else
	    {
	      /* GEU, use slt instruction */
	      emit_insn (gen_slt_compare  (operands[0], tmp_reg, operands[2]));
	    }

	  DONE;
	}
      else
	{
	  /* reg_R = (reg_A >= reg_B)
	     --> slt  reg_R, reg_A, reg_B
	         xori reg_R, reg_R, const_int_1 */
	  if (code == GE)
	    {
	      /* GE, use slts instruction */
	      emit_insn (gen_slts_compare (operands[0],
					   operands[2], operands[3]));
	    }
	  else
	    {
	      /* GEU, use slt instruction */
	      emit_insn (gen_slt_compare  (operands[0],
					   operands[2], operands[3]));
	    }

	  /* perform 'not' behavior */
	  emit_insn (gen_xorsi3 (operands[0], operands[0], const1_rtx));

	  DONE;
	}

    case LT:
    case LTU:
      /* reg_R = (reg_A < reg_B)       --> slt reg_R, reg_A, reg_B */
      /* reg_R = (reg_A < const_int_B) --> slt reg_R, reg_A, const_int_B */
      if (code == LT)
	{
	  /* LT, use slts instruction */
	  emit_insn (gen_slts_compare (operands[0], operands[2], operands[3]));
	}
      else
	{
	  /* LTU, use slt instruction */
	  emit_insn (gen_slt_compare  (operands[0], operands[2], operands[3]));
	}

      DONE;

    case LE:
    case LEU:
      if (GET_CODE (operands[3]) == CONST_INT)
	{
	  /* reg_R = (reg_A <= const_int_B)
	     --> movi reg_C, const_int_B + 1
	         slt  reg_R, reg_A, reg_C */
	  tmp_reg = gen_reg_rtx (SImode);

	  emit_insn (gen_movsi (tmp_reg,
				gen_int_mode (INTVAL (operands[3]) + 1,
						      SImode)));
	  if (code == LE)
	    {
	      /* LE, use slts instruction */
	      emit_insn (gen_slts_compare (operands[0], operands[2], tmp_reg));
	    }
	  else
	    {
	      /* LEU, use slt instruction */
	      emit_insn (gen_slt_compare  (operands[0], operands[2], tmp_reg));
	    }

	  DONE;
	}
      else
	{
	  /* reg_R = (reg_A <= reg_B) --> slt  reg_R, reg_B, reg_A
	                                  xori reg_R, reg_R, const_int_1 */
	  if (code == LE)
	    {
	      /* LE, use slts instruction */
	      emit_insn (gen_slts_compare (operands[0],
					   operands[3], operands[2]));
	    }
	  else
	    {
	      /* LEU, use slt instruction */
	      emit_insn (gen_slt_compare  (operands[0],
					   operands[3], operands[2]));
	    }

	  /* perform 'not' behavior */
	  emit_insn (gen_xorsi3 (operands[0], operands[0], const1_rtx));

	  DONE;
	}


    default:
      gcc_unreachable ();
    }
})


(define_insn "slts_compare"
  [(set (match_operand:SI 0 "register_operand"         "=t,    t, r,    r")
	(lt:SI (match_operand:SI 1 "nonmemory_operand" " d,    d, r,    r")
	       (match_operand:SI 2 "nonmemory_operand" " r, Iu05, r, Is15")))]
  ""
  "@
   slts45\t%1, %2
   sltsi45\t%1, %2
   slts\t%0, %1, %2
   sltsi\t%0, %1, %2"
  [(set_attr "type"   "compare,compare,compare,compare")
   (set_attr "length" "      2,      2,      4,      4")])

(define_insn "slt_compare"
  [(set (match_operand:SI 0 "register_operand"          "=t,    t, r,    r")
	(ltu:SI (match_operand:SI 1 "nonmemory_operand" " d,    d, r,    r")
		(match_operand:SI 2 "nonmemory_operand" " r, Iu05, r, Is15")))]
  ""
  "@
   slt45\t%1, %2
   slti45\t%1, %2
   slt\t%0, %1, %2
   slti\t%0, %1, %2"
  [(set_attr "type"   "compare,compare,compare,compare")
   (set_attr "length" "      2,      2,      4,      4")])


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
   (set_attr "enabled" "1")
   (set (attr "length")
	(if_then_else (and (ge (minus (match_dup 0) (pc)) (const_int -250))
			   (le (minus (match_dup 0) (pc)) (const_int  250)))
		      (if_then_else (match_test "TARGET_16_BIT")
				    (const_int 2)
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
  ""
)

(define_insn "*call_register"
  [(parallel [(call (mem (match_operand:SI 0 "register_operand" "r, r"))
		    (match_operand 1))
	      (clobber (reg:SI LP_REGNUM))
	      (clobber (reg:SI TA_REGNUM))])]
  ""
  "@
  jral5\t%0
  jral\t%0"
  [(set_attr "type"   "branch,branch")
   (set_attr "length" "     2,     4")])

(define_insn "*call_immediate"
  [(parallel [(call (mem (match_operand:SI 0 "immediate_operand" "i"))
		    (match_operand 1))
	      (clobber (reg:SI LP_REGNUM))
	      (clobber (reg:SI TA_REGNUM))])]
  ""
{
  if (TARGET_CMODEL_LARGE)
    return "bal\t%0";
  else
    return "jal\t%0";
}
  [(set_attr "type"   "branch")
   (set (attr "length")
	(if_then_else (match_test "TARGET_CMODEL_LARGE")
		      (const_int 12)
		      (const_int 4)))])


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
  ""
)

(define_insn "*call_value_register"
  [(parallel [(set (match_operand 0)
		   (call (mem (match_operand:SI 1 "register_operand" "r, r"))
		         (match_operand 2)))
	      (clobber (reg:SI LP_REGNUM))
	      (clobber (reg:SI TA_REGNUM))])]
  ""
  "@
  jral5\t%1
  jral\t%1"
  [(set_attr "type"   "branch,branch")
   (set_attr "length" "     2,     4")])

(define_insn "*call_value_immediate"
  [(parallel [(set (match_operand 0)
		   (call (mem (match_operand:SI 1 "immediate_operand" "i"))
			 (match_operand 2)))
	      (clobber (reg:SI LP_REGNUM))
	      (clobber (reg:SI TA_REGNUM))])]
  ""
{
  if (TARGET_CMODEL_LARGE)
    return "bal\t%1";
  else
    return "jal\t%1";
}
  [(set_attr "type"   "branch")
   (set (attr "length")
	(if_then_else (match_test "TARGET_CMODEL_LARGE")
		      (const_int 12)
		      (const_int 4)))])


;; ----------------------------------------------------------------------------

;; The sibcall patterns.

;; sibcall
;; sibcall_register
;; sibcall_immediate

(define_expand "sibcall"
  [(parallel [(call (match_operand 0 "memory_operand" "")
		    (const_int 0))
	      (clobber (reg:SI TA_REGNUM))
	      (return)])]
  ""
  ""
)

(define_insn "*sibcall_register"
  [(parallel [(call (mem (match_operand:SI 0 "register_operand" "r, r"))
		    (match_operand 1))
	      (clobber (reg:SI TA_REGNUM))
	      (return)])]
  ""
  "@
   jr5\t%0
   jr\t%0"
  [(set_attr "type"   "branch,branch")
   (set_attr "length" "     2,     4")])

(define_insn "*sibcall_immediate"
  [(parallel [(call (mem (match_operand:SI 0 "immediate_operand" "i"))
		    (match_operand 1))
	      (clobber (reg:SI TA_REGNUM))
	      (return)])]
  ""
{
  if (TARGET_CMODEL_LARGE)
    return "b\t%0";
  else
    return "j\t%0";
}
  [(set_attr "type"   "branch")
   (set (attr "length")
	(if_then_else (match_test "TARGET_CMODEL_LARGE")
		      (const_int 12)
		      (const_int 4)))])

;; sibcall_value
;; sibcall_value_register
;; sibcall_value_immediate

(define_expand "sibcall_value"
  [(parallel [(set (match_operand 0)
		   (call (match_operand 1 "memory_operand" "")
			 (const_int 0)))
	      (clobber (reg:SI TA_REGNUM))
	      (return)])]
  ""
  ""
)

(define_insn "*sibcall_value_register"
  [(parallel [(set (match_operand 0)
		   (call (mem (match_operand:SI 1 "register_operand" "r, r"))
			 (match_operand 2)))
	      (clobber (reg:SI TA_REGNUM))
	      (return)])]
  ""
  "@
   jr5\t%1
   jr\t%1"
  [(set_attr "type"   "branch,branch")
   (set_attr "length" "     2,     4")])

(define_insn "*sibcall_value_immediate"
  [(parallel [(set (match_operand 0)
		   (call (mem (match_operand:SI 1 "immediate_operand" "i"))
			 (match_operand 2)))
	      (clobber (reg:SI TA_REGNUM))
	      (return)])]
  ""
{
  if (TARGET_CMODEL_LARGE)
    return "b\t%1";
  else
    return "j\t%1";
}
  [(set_attr "type"   "branch")
   (set (attr "length")
	(if_then_else (match_test "TARGET_CMODEL_LARGE")
		      (const_int 12)
		      (const_int 4)))])


;; ----------------------------------------------------------------------------

;; prologue and epilogue.

(define_expand "prologue" [(const_int 0)]
  ""
{
  /* Note that only under V3/V3M ISA, we could use v3push prologue.
     In addition, we do not want to use v3push for isr function
     and variadic function.  */
  if (TARGET_V3PUSH
      && !nds32_isr_function_p (current_function_decl)
      && (cfun->machine->va_args_size == 0))
    nds32_expand_prologue_v3push ();
  else
    nds32_expand_prologue ();
  DONE;
})

(define_expand "epilogue" [(const_int 0)]
  ""
{
  /* Note that only under V3/V3M ISA, we could use v3pop epilogue.
     In addition, we do not want to use v3pop for isr function
     and variadic function.  */
  if (TARGET_V3PUSH
      && !nds32_isr_function_p (current_function_decl)
      && (cfun->machine->va_args_size == 0))
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
  if (TARGET_V3PUSH && !nds32_isr_function_p (current_function_decl))
    nds32_expand_epilogue_v3pop (true);
  else
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
   (set_attr "enabled" "1")
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
  [(set_attr "type" "misc")
   (set_attr "enabled" "1")
   (set (attr "length")
	(if_then_else (match_test "TARGET_V3PUSH
				   && !nds32_isr_function_p (cfun->decl)
				   && (cfun->machine->va_args_size == 0)")
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
  [(set_attr "type" "misc")
   (set_attr "enabled" "1")
   (set (attr "length")
	(if_then_else (match_test "TARGET_V3PUSH
				   && !nds32_isr_function_p (cfun->decl)
				   && (cfun->machine->va_args_size == 0)")
		      (const_int 2)
		      (const_int 4)))])


;; ----------------------------------------------------------------------------
;; Return operation patterns
;; ----------------------------------------------------------------------------

;; Use this pattern to expand a return instruction
;; with simple_return rtx if no epilogue is required.
(define_expand "return"
  [(simple_return)]
  "nds32_can_use_return_insn ()"
  ""
)

;; This pattern is expanded only by the shrink-wrapping optimization
;; on paths where the function prologue has not been executed.
(define_expand "simple_return"
  [(simple_return)]
  ""
  ""
)

(define_insn "return_internal"
  [(simple_return)]
  ""
{
  if (TARGET_16_BIT)
    return "ret5";
  else
    return "ret";
}
  [(set_attr "type" "branch")
   (set_attr "enabled" "1")
   (set (attr "length")
	(if_then_else (match_test "TARGET_16_BIT")
		      (const_int 2)
		      (const_int 4)))])


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

  /* Step C, D, E, and F, using another temporary register.  */
  rtx tmp = gen_reg_rtx (SImode);
  emit_jump_insn (gen_casesi_internal (operands[0], operands[3], tmp));
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
  [(set_attr "length" "20")
   (set_attr "type" "alu")])

;; ----------------------------------------------------------------------------

;; Performance Extension

(define_insn "clzsi2"
  [(set (match_operand:SI 0 "register_operand"         "=r")
	(clz:SI (match_operand:SI 1 "register_operand" " r")))]
  "TARGET_PERF_EXT"
  "clz\t%0, %1"
  [(set_attr "type" "alu")
   (set_attr "length" "4")])

(define_insn "smaxsi3"
  [(set (match_operand:SI 0 "register_operand"          "=r")
	(smax:SI (match_operand:SI 1 "register_operand" " r")
		 (match_operand:SI 2 "register_operand" " r")))]
  "TARGET_PERF_EXT"
  "max\t%0, %1, %2"
  [(set_attr "type" "alu")
   (set_attr "length" "4")])

(define_insn "sminsi3"
  [(set (match_operand:SI 0 "register_operand"          "=r")
	(smin:SI (match_operand:SI 1 "register_operand" " r")
		 (match_operand:SI 2 "register_operand" " r")))]
  "TARGET_PERF_EXT"
  "min\t%0, %1, %2"
  [(set_attr "type" "alu")
   (set_attr "length" "4")])

(define_insn "*btst"
  [(set (match_operand:SI 0 "register_operand"                   "=   r")
	(zero_extract:SI (match_operand:SI 1 "register_operand"  "    r")
			 (const_int 1)
			 (match_operand:SI 2 "immediate_operand" " Iu05")))]
  "TARGET_PERF_EXT"
  "btst\t%0, %1, %2"
  [(set_attr "type" "alu")
   (set_attr "length" "4")])

;; ----------------------------------------------------------------------------

;; Pseudo NOPs

(define_insn "pop25return"
  [(return)
   (unspec_volatile:SI [(reg:SI LP_REGNUM)] UNSPEC_VOLATILE_POP25_RETURN)]
  ""
  "! return for pop 25"
  [(set_attr "length" "0")]
)

;; ----------------------------------------------------------------------------
