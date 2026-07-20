;; Matrix-Multiply Assist (MMA) patterns.
;; Copyright (C) 2020-2026 Free Software Foundation, Inc.
;; Contributed by Peter Bergner <bergner@linux.ibm.com> and
;;		  Michael Meissner <meissner@linux.ibm.com>
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

;; The MMA patterns use the multi-register XOmode and OOmode opaque
;; modes to implement the target specific __vector_quad and
;; __vector_pair types that the MMA built-in functions reference.  We
;; use OPAQUE_MODE to prevent anything from trying to open them up.

(define_constants [(MAX_MMA_OPERANDS 7)])

;; Constants for creating unspecs

(define_c_enum "unspec"
  [UNSPEC_VSX_ASSEMBLE
   UNSPEC_MMA_EXTRACT
   UNSPEC_MMA_PMXVBF16GER2
   UNSPEC_MMA_PMXVBF16GER2NN
   UNSPEC_MMA_PMXVBF16GER2NP
   UNSPEC_MMA_PMXVBF16GER2PN
   UNSPEC_MMA_PMXVBF16GER2PP
   UNSPEC_MMA_PMXVF16GER2
   UNSPEC_MMA_PMXVF16GER2NN
   UNSPEC_MMA_PMXVF16GER2NP
   UNSPEC_MMA_PMXVF16GER2PN
   UNSPEC_MMA_PMXVF16GER2PP
   UNSPEC_MMA_PMXVF32GER
   UNSPEC_MMA_PMXVF32GERNN
   UNSPEC_MMA_PMXVF32GERNP
   UNSPEC_MMA_PMXVF32GERPN
   UNSPEC_MMA_PMXVF32GERPP
   UNSPEC_MMA_PMXVF64GER
   UNSPEC_MMA_PMXVF64GERNN
   UNSPEC_MMA_PMXVF64GERNP
   UNSPEC_MMA_PMXVF64GERPN
   UNSPEC_MMA_PMXVF64GERPP
   UNSPEC_MMA_PMXVI16GER2
   UNSPEC_MMA_PMXVI16GER2PP
   UNSPEC_MMA_PMXVI16GER2S
   UNSPEC_MMA_PMXVI16GER2SPP
   UNSPEC_MMA_PMXVI4GER8
   UNSPEC_MMA_PMXVI4GER8PP
   UNSPEC_MMA_PMXVI8GER4
   UNSPEC_MMA_PMXVI8GER4PP
   UNSPEC_MMA_PMXVI8GER4SPP
   UNSPEC_MMA_XVBF16GER2
   UNSPEC_MMA_XVBF16GER2NN
   UNSPEC_MMA_XVBF16GER2NP
   UNSPEC_MMA_XVBF16GER2PN
   UNSPEC_MMA_XVBF16GER2PP
   UNSPEC_MMA_XVF16GER2
   UNSPEC_MMA_XVF16GER2NN
   UNSPEC_MMA_XVF16GER2NP
   UNSPEC_MMA_XVF16GER2PN
   UNSPEC_MMA_XVF16GER2PP
   UNSPEC_MMA_XVF32GER
   UNSPEC_MMA_XVF32GERNN
   UNSPEC_MMA_XVF32GERNP
   UNSPEC_MMA_XVF32GERPN
   UNSPEC_MMA_XVF32GERPP
   UNSPEC_MMA_XVF64GER
   UNSPEC_MMA_XVF64GERNN
   UNSPEC_MMA_XVF64GERNP
   UNSPEC_MMA_XVF64GERPN
   UNSPEC_MMA_XVF64GERPP
   UNSPEC_MMA_XVI16GER2
   UNSPEC_MMA_XVI16GER2PP
   UNSPEC_MMA_XVI16GER2S
   UNSPEC_MMA_XVI16GER2SPP
   UNSPEC_MMA_XVI4GER8
   UNSPEC_MMA_XVI4GER8PP
   UNSPEC_MMA_XVI8GER4
   UNSPEC_MMA_XVI8GER4PP
   UNSPEC_MMA_XVI8GER4SPP
   UNSPEC_MMA_XXMFACC
   UNSPEC_MMA_XXMTACC
   UNSPEC_DMF_INSERT512
   UNSPEC_DMF_EXTRACT512
   UNSPEC_DMF_INSERT1024
   UNSPEC_DMF_RELOAD_FROM_MEMORY
   UNSPEC_DMF_RELOAD_TO_MEMORY
  ])

(define_c_enum "unspecv"
  [UNSPECV_MMA_ASSEMBLE
   UNSPECV_MMA_XXSETACCZ
  ])

;; MMA instructions with 1 accumulator argument
(define_int_iterator MMA_ACC		[UNSPEC_MMA_XXMFACC
					 UNSPEC_MMA_XXMTACC])

;; MMA instructions with 2 vector arguments
(define_int_iterator MMA_VV		[UNSPEC_MMA_XVI4GER8
					 UNSPEC_MMA_XVI8GER4
					 UNSPEC_MMA_XVI16GER2
					 UNSPEC_MMA_XVI16GER2S
					 UNSPEC_MMA_XVF16GER2
					 UNSPEC_MMA_XVBF16GER2
					 UNSPEC_MMA_XVF32GER])

;; MMA instructions with 1 accumulator and 2 vector arguments
(define_int_iterator MMA_AVV		[UNSPEC_MMA_XVI4GER8PP
					 UNSPEC_MMA_XVI8GER4PP
					 UNSPEC_MMA_XVI8GER4SPP
					 UNSPEC_MMA_XVI16GER2PP
					 UNSPEC_MMA_XVI16GER2SPP
					 UNSPEC_MMA_XVF16GER2PP
					 UNSPEC_MMA_XVF16GER2PN
					 UNSPEC_MMA_XVF16GER2NP
					 UNSPEC_MMA_XVF16GER2NN
					 UNSPEC_MMA_XVBF16GER2PP
					 UNSPEC_MMA_XVBF16GER2PN
					 UNSPEC_MMA_XVBF16GER2NP
					 UNSPEC_MMA_XVBF16GER2NN
					 UNSPEC_MMA_XVF32GERPP
					 UNSPEC_MMA_XVF32GERPN
					 UNSPEC_MMA_XVF32GERNP
					 UNSPEC_MMA_XVF32GERNN])

;; MMA instructions with 1 vector pair and 1 vector arguments
(define_int_iterator MMA_PV		[UNSPEC_MMA_XVF64GER])

;; MMA instructions with 1 accumulator, 1 vector pair and 1 vector arguments
(define_int_iterator MMA_APV		[UNSPEC_MMA_XVF64GERPP
					 UNSPEC_MMA_XVF64GERPN
					 UNSPEC_MMA_XVF64GERNP
					 UNSPEC_MMA_XVF64GERNN])

;; MMA instructions with 2 vector, 2 4-bit and 1 8-bit arguments
(define_int_iterator MMA_VVI4I4I8	[UNSPEC_MMA_PMXVI4GER8])

;; MMA instructions with 1 accumulator, 2 vector, 2 4-bit and 1 8-bit arguments
(define_int_iterator MMA_AVVI4I4I8	[UNSPEC_MMA_PMXVI4GER8PP])

;; MMA instructions with 2 vector, 2 4-bit and 1 2-bit arguments
(define_int_iterator MMA_VVI4I4I2	[UNSPEC_MMA_PMXVI16GER2
					 UNSPEC_MMA_PMXVI16GER2S
					 UNSPEC_MMA_PMXVF16GER2
					 UNSPEC_MMA_PMXVBF16GER2])

;; MMA instructions with 1 accumulator, 2 vector, 2 4-bit and 1 2-bit arguments
(define_int_iterator MMA_AVVI4I4I2	[UNSPEC_MMA_PMXVI16GER2PP
					 UNSPEC_MMA_PMXVI16GER2SPP
					 UNSPEC_MMA_PMXVF16GER2PP
					 UNSPEC_MMA_PMXVF16GER2PN
					 UNSPEC_MMA_PMXVF16GER2NP
					 UNSPEC_MMA_PMXVF16GER2NN
					 UNSPEC_MMA_PMXVBF16GER2PP
					 UNSPEC_MMA_PMXVBF16GER2PN
					 UNSPEC_MMA_PMXVBF16GER2NP
					 UNSPEC_MMA_PMXVBF16GER2NN])

;; MMA instructions with 2 vector and 2 4-bit arguments
(define_int_iterator MMA_VVI4I4		[UNSPEC_MMA_PMXVF32GER])

;; MMA instructions with 1 accumulator, 2 vector and 2 4-bit arguments
(define_int_iterator MMA_AVVI4I4	[UNSPEC_MMA_PMXVF32GERPP
					 UNSPEC_MMA_PMXVF32GERPN
					 UNSPEC_MMA_PMXVF32GERNP
					 UNSPEC_MMA_PMXVF32GERNN])

;; MMA instructions with 2 vector, 1 4-bit and 1 2-bit arguments
(define_int_iterator MMA_PVI4I2		[UNSPEC_MMA_PMXVF64GER])

;; MMA instructions with 1 accumulator, 2 vector, 1 4-bit and 1 2-bit arguments
(define_int_iterator MMA_APVI4I2	[UNSPEC_MMA_PMXVF64GERPP
					 UNSPEC_MMA_PMXVF64GERPN
					 UNSPEC_MMA_PMXVF64GERNP
					 UNSPEC_MMA_PMXVF64GERNN])

;; MMA instructions with 2 vector and 3 4-bit arguments
(define_int_iterator MMA_VVI4I4I4	[UNSPEC_MMA_PMXVI8GER4])

;; MMA instructions with 1 accumulator, 2 vector and 3 4-bit arguments
(define_int_iterator MMA_AVVI4I4I4	[UNSPEC_MMA_PMXVI8GER4PP
					 UNSPEC_MMA_PMXVI8GER4SPP])

(define_int_attr acc		[(UNSPEC_MMA_XXMFACC		"xxmfacc")
				 (UNSPEC_MMA_XXMTACC		"xxmtacc")])

(define_int_attr vv		[(UNSPEC_MMA_XVI4GER8		"xvi4ger8")
				 (UNSPEC_MMA_XVI8GER4		"xvi8ger4")
				 (UNSPEC_MMA_XVI16GER2		"xvi16ger2")
				 (UNSPEC_MMA_XVI16GER2S		"xvi16ger2s")
				 (UNSPEC_MMA_XVF16GER2		"xvf16ger2")
				 (UNSPEC_MMA_XVBF16GER2		"xvbf16ger2")
				 (UNSPEC_MMA_XVF32GER		"xvf32ger")])

(define_int_attr avv		[(UNSPEC_MMA_XVI4GER8PP		"xvi4ger8pp")
				 (UNSPEC_MMA_XVI8GER4PP		"xvi8ger4pp")
				 (UNSPEC_MMA_XVI8GER4SPP	"xvi8ger4spp")
				 (UNSPEC_MMA_XVI16GER2PP	"xvi16ger2pp")
				 (UNSPEC_MMA_XVI16GER2SPP	"xvi16ger2spp")
				 (UNSPEC_MMA_XVF16GER2PP	"xvf16ger2pp")
				 (UNSPEC_MMA_XVF16GER2PN	"xvf16ger2pn")
				 (UNSPEC_MMA_XVF16GER2NP	"xvf16ger2np")
				 (UNSPEC_MMA_XVF16GER2NN	"xvf16ger2nn")
				 (UNSPEC_MMA_XVBF16GER2PP	"xvbf16ger2pp")
				 (UNSPEC_MMA_XVBF16GER2PN	"xvbf16ger2pn")
				 (UNSPEC_MMA_XVBF16GER2NP	"xvbf16ger2np")
				 (UNSPEC_MMA_XVBF16GER2NN	"xvbf16ger2nn")
				 (UNSPEC_MMA_XVF32GERPP		"xvf32gerpp")
				 (UNSPEC_MMA_XVF32GERPN		"xvf32gerpn")
				 (UNSPEC_MMA_XVF32GERNP		"xvf32gernp")
				 (UNSPEC_MMA_XVF32GERNN		"xvf32gernn")])

(define_int_attr pv		[(UNSPEC_MMA_XVF64GER		"xvf64ger")])

(define_int_attr apv		[(UNSPEC_MMA_XVF64GERPP		"xvf64gerpp")
				 (UNSPEC_MMA_XVF64GERPN		"xvf64gerpn")
				 (UNSPEC_MMA_XVF64GERNP		"xvf64gernp")
				 (UNSPEC_MMA_XVF64GERNN		"xvf64gernn")])

(define_int_attr vvi4i4i8	[(UNSPEC_MMA_PMXVI4GER8		"pmxvi4ger8")])

(define_int_attr avvi4i4i8	[(UNSPEC_MMA_PMXVI4GER8PP	"pmxvi4ger8pp")])

(define_int_attr vvi4i4i2	[(UNSPEC_MMA_PMXVI16GER2	"pmxvi16ger2")
				 (UNSPEC_MMA_PMXVI16GER2S	"pmxvi16ger2s")
				 (UNSPEC_MMA_PMXVF16GER2	"pmxvf16ger2")
				 (UNSPEC_MMA_PMXVBF16GER2	"pmxvbf16ger2")])

(define_int_attr avvi4i4i2	[(UNSPEC_MMA_PMXVI16GER2PP	"pmxvi16ger2pp")
				 (UNSPEC_MMA_PMXVI16GER2SPP	"pmxvi16ger2spp")
				 (UNSPEC_MMA_PMXVF16GER2PP	"pmxvf16ger2pp")
				 (UNSPEC_MMA_PMXVF16GER2PN	"pmxvf16ger2pn")
				 (UNSPEC_MMA_PMXVF16GER2NP	"pmxvf16ger2np")
				 (UNSPEC_MMA_PMXVF16GER2NN	"pmxvf16ger2nn")
				 (UNSPEC_MMA_PMXVBF16GER2PP	"pmxvbf16ger2pp")
				 (UNSPEC_MMA_PMXVBF16GER2PN	"pmxvbf16ger2pn")
				 (UNSPEC_MMA_PMXVBF16GER2NP	"pmxvbf16ger2np")
				 (UNSPEC_MMA_PMXVBF16GER2NN	"pmxvbf16ger2nn")])

(define_int_attr vvi4i4		[(UNSPEC_MMA_PMXVF32GER		"pmxvf32ger")])

(define_int_attr avvi4i4	[(UNSPEC_MMA_PMXVF32GERPP	"pmxvf32gerpp")
				 (UNSPEC_MMA_PMXVF32GERPN	"pmxvf32gerpn")
				 (UNSPEC_MMA_PMXVF32GERNP	"pmxvf32gernp")
				 (UNSPEC_MMA_PMXVF32GERNN	"pmxvf32gernn")])

(define_int_attr pvi4i2		[(UNSPEC_MMA_PMXVF64GER		"pmxvf64ger")])

(define_int_attr apvi4i2	[(UNSPEC_MMA_PMXVF64GERPP	"pmxvf64gerpp")
				 (UNSPEC_MMA_PMXVF64GERPN	"pmxvf64gerpn")
				 (UNSPEC_MMA_PMXVF64GERNP	"pmxvf64gernp")
				 (UNSPEC_MMA_PMXVF64GERNN	"pmxvf64gernn")])

(define_int_attr vvi4i4i4	[(UNSPEC_MMA_PMXVI8GER4		"pmxvi8ger4")])

(define_int_attr avvi4i4i4	[(UNSPEC_MMA_PMXVI8GER4PP	"pmxvi8ger4pp")
				 (UNSPEC_MMA_PMXVI8GER4SPP	"pmxvi8ger4spp")])


;; Vector pair support.  OOmode can only live in VSRs.
(define_expand "movoo"
  [(set (match_operand:OO 0 "nonimmediate_operand")
	(match_operand:OO 1 "input_operand"))]
  ""
{
  if (TARGET_MMA || TARGET_DMF)
    {
      rs6000_emit_move (operands[0], operands[1], OOmode);
      DONE;
    }
  else if (currently_expanding_to_rtl && seen_error ())
    {
      /* PR103353 shows we may want to continue to expand the __builtin_vsx_lxvp
	 built-in function, even if we have already emitted error messages about
	 some missing required conditions.  As shown in that PR, without one
	 explicit mov optab on OOmode provided, it would call emit_move_insn
	 recursively.  So we allow this pattern to be generated when we are
	 expanding to RTL and have seen errors.  It would not cause further ICEs
	 as the compilation would stop soon after expanding.  */
    }
  else if (rs6000_opaque_type_invalid_use_p (currently_expanding_gimple_stmt))
    ;
  else
    /* Catch unexpected cases.  */
    gcc_assert (false);
})

(define_insn_and_split "*movoo"
  [(set (match_operand:OO 0 "nonimmediate_operand" "=wa,ZwO,wa")
	(match_operand:OO 1 "input_operand" "ZwO,wa,wa"))]
  "(TARGET_MMA || TARGET_DMF)
   && (gpc_reg_operand (operands[0], OOmode)
       || gpc_reg_operand (operands[1], OOmode))"
  "@
   lxvp%X1 %x0,%1
   stxvp%X0 %x1,%0
   #"
  "&& reload_completed
   && (!MEM_P (operands[0]) && !MEM_P (operands[1]))"
  [(const_int 0)]
{
  rs6000_split_multireg_move (operands[0], operands[1]);
  DONE;
}
  [(set_attr "type" "vecload,vecstore,veclogical")
   (set_attr "size" "256")
   (set_attr "length" "*,*,8")])


;; Vector quad support.  XOmode can only live in FPRs.
(define_expand "movxo"
  [(set (match_operand:XO 0 "nonimmediate_operand")
	(match_operand:XO 1 "input_operand"))]
  ""
{
  if (TARGET_MMA)
    {
      rs6000_emit_move (operands[0], operands[1], XOmode);
      DONE;
    }
  else if (currently_expanding_to_rtl && seen_error ())
    {
      /* PR103353 shows we may want to continue to expand the __builtin_vsx_lxvp
	 built-in function, even if we have already emitted error messages about
	 some missing required conditions.  So do the same handlings for XOmode
	 as OOmode here.  */
    }
  else if (rs6000_opaque_type_invalid_use_p (currently_expanding_gimple_stmt))
    ;
  else
    /* Catch unexpected cases.  */
    gcc_assert (false);
})

(define_insn_and_split "*movxo_nodmf"
  [(set (match_operand:XO 0 "nonimmediate_operand" "=wD,ZwO,wD")
	(match_operand:XO 1 "input_operand" "ZwO,wD,wD"))]
  "TARGET_MMA && !TARGET_DMF
   && (gpc_reg_operand (operands[0], XOmode)
       || gpc_reg_operand (operands[1], XOmode))"
  "@
   #
   #
   #"
  "&& reload_completed"
  [(const_int 0)]
{
  rs6000_split_multireg_move (operands[0], operands[1]);
  DONE;
}
  [(set_attr "type" "vecload,vecstore,veclogical")
   (set_attr "length" "*,*,16")
   (set_attr "max_prefixed_insns" "2,2,*")])

(define_insn_and_split "*movxo_dmf"
  [(set (match_operand:XO 0 "nonimmediate_operand" "=wa,ZwO,wa,wD,wD,wa")
        (match_operand:XO 1 "input_operand"        "ZwO,wa, wa,wa,wD,wD"))]
  "TARGET_DMF
   && (gpc_reg_operand (operands[0], XOmode)
       || gpc_reg_operand (operands[1], XOmode))"
  "@
   #
   #
   #
   dmxxinstdmr512 %0,%x1,%W1,0
   dmmr %0,%1
   dmxxextfdmr512 %x0,%W0,%1,0"
  "&& reload_completed
   && !dmr_register_operand (operands[0], XOmode)
   && !dmr_register_operand (operands[1], XOmode)"
  [(const_int 0)]
{
  rs6000_split_multireg_move (operands[0], operands[1]);
  DONE;
}
  [(set_attr "type" "vecload,vecstore,veclogical,dmf,dmf,dmf")
   (set_attr "length" "*,*,16,*,*,*")
   (set_attr "max_prefixed_insns" "2,2,*,*,*,*")])

(define_expand "vsx_assemble_pair"
  [(match_operand:OO 0 "vsx_register_operand")
   (match_operand:V16QI 1 "mma_assemble_input_operand")
   (match_operand:V16QI 2 "mma_assemble_input_operand")]
  "TARGET_MMA || TARGET_DMF"
{
  rtx src = gen_rtx_UNSPEC (OOmode,
			    gen_rtvec (2, operands[1], operands[2]),
			    UNSPEC_VSX_ASSEMBLE);
  emit_move_insn (operands[0], src);
  DONE;
})

;; We cannot update the two output registers atomically, so mark the output
;; as an early clobber so we don't accidentally clobber the input operands.  */

(define_insn_and_split "*vsx_assemble_pair"
  [(set (match_operand:OO 0 "vsx_register_operand" "=&wa")
	(unspec:OO [(match_operand:V16QI 1 "mma_assemble_input_operand" "mwa")
		    (match_operand:V16QI 2 "mma_assemble_input_operand" "mwa")]
		   UNSPEC_VSX_ASSEMBLE))]
  "TARGET_MMA || TARGET_DMF"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  rtx src = gen_rtx_UNSPEC (OOmode,
			    gen_rtvec (2, operands[1], operands[2]),
			    UNSPEC_VSX_ASSEMBLE);
  rs6000_split_multireg_move (operands[0], src);
  DONE;
})

(define_expand "vsx_disassemble_pair"
  [(match_operand:V16QI 0 "mma_disassemble_output_operand")
   (match_operand:OO 1 "vsx_register_operand")
   (match_operand 2 "const_0_to_1_operand")]
  "TARGET_MMA || TARGET_DMF"
{
  rtx src;
  int regoff = INTVAL (operands[2]);
  src = gen_rtx_UNSPEC (V16QImode,
                        gen_rtvec (2, operands[1], GEN_INT (regoff)),
                        UNSPEC_MMA_EXTRACT);
  emit_move_insn (operands[0], src);
  DONE;
})

(define_insn_and_split "*vsx_disassemble_pair"
  [(set (match_operand:V16QI 0 "mma_disassemble_output_operand" "=mwa")
       (unspec:V16QI [(match_operand:OO 1 "vsx_register_operand" "wa")
                      (match_operand 2 "const_0_to_1_operand")]
                      UNSPEC_MMA_EXTRACT))]
  "(TARGET_MMA || TARGET_DMF)
   && vsx_register_operand (operands[1], OOmode)"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  int reg = REGNO (operands[1]);
  int regoff = INTVAL (operands[2]);
  rtx src = gen_rtx_REG (V16QImode, reg + regoff);
  emit_move_insn (operands[0], src);
  DONE;
})

(define_insn "dm_insert512"
  [(set (match_operand:XO 0 "dmr_register_operand" "=wD")
	(unspec:XO [(match_operand:OO 1 "vsx_register_operand" "wa")
		    (match_operand:OO 2 "vsx_register_operand" "wa")
		    (match_operand 3 "const_0_to_1_operand")]
		   UNSPEC_DMF_INSERT512))]
  "TARGET_DMF"
  "dmxxinstdmr512 %0,%x1,%x2,%3"
  [(set_attr "type" "dmf")])

;; Move from VSX registers to DMR registers via two insert 512 bit
;; instructions.
(define_insn "dm_insert1024"
  [(set (match_operand:TDO 0 "dmr_register_operand" "=wD")
	(unspec:TDO [(match_operand:OO 1 "vsx_register_operand" "wa")
		     (match_operand:OO 2 "vsx_register_operand" "wa")
		     (match_operand:OO 3 "vsx_register_operand" "wa")
		     (match_operand:OO 4 "vsx_register_operand" "wa")]
		    UNSPEC_DMF_INSERT1024))]
  "TARGET_DMF"
  "dmxxinstdmr512 %0,%x1,%x2,0\n\tdmxxinstdmr512 %0,%x3,%x4,1"
  [(set_attr "type" "dmf")])

(define_insn "dm_extract512"
  [(set (match_operand:XO 0 "vsx_register_operand" "=wa")
        (unspec:XO [(match_operand:TDO 1 "dmr_register_operand" "wD")
		    (match_operand 2 "const_0_to_1_operand" "n")]
		   UNSPEC_DMF_EXTRACT512))]
  "TARGET_DMF"
  "dmxxextfdmr512 %x0,%W0,%1,%2"
  [(set_attr "type" "dmf")])

;; TDO (1024-bit dense-math) move expander.
(define_expand "movtdo"
  [(set (match_operand:TDO 0 "nonimmediate_operand")
        (match_operand:TDO 1 "input_operand"))]
  "TARGET_DMF"
{
  rs6000_emit_move (operands[0], operands[1], TDOmode);
  DONE;
})

(define_insn_and_split "*movtdo"
  [(set (match_operand:TDO 0 "nonimmediate_operand" "=wa,m,wa,wD,wa,wD")
        (match_operand:TDO 1 "input_operand"         "m,wa,wa,wD,wD,wa"))]
  "TARGET_DMF
   && (gpc_reg_operand (operands[0], TDOmode)
       || gpc_reg_operand (operands[1], TDOmode))"
{
  if (which_alternative == 3)
    return "dmmr %0,%1";
  else
    return "#";
}
"reload_completed
 && (!dmr_register_operand (operands[0], TDOmode)
     || !dmr_register_operand (operands[1], TDOmode))"
  [(const_int 0)]
{
  rtx dst = operands[0];
  rtx src = operands[1];

  /* Memory-involving moves (alt 0/1) and wa<-wa VSX moves (alt 2)
     both go through the generic multiregister splitter.  */
  if (!REG_P (dst) || !REG_P (src)
      || (VSX_REGNO_P (REGNO (dst)) && VSX_REGNO_P (REGNO (src))))
    {
      rs6000_split_multireg_move (dst, src);
      DONE;
    }

  unsigned dst_regno = REGNO (dst);
  unsigned src_regno = REGNO (src);
  bool dst_is_dmr = DMR_REGNO_P (dst_regno);
  bool src_is_dmr = DMR_REGNO_P (src_regno);
  bool dst_is_vsx = VSX_REGNO_P (dst_regno);
  bool src_is_vsx = VSX_REGNO_P (src_regno);

  /* wD <- wD: already a dmmr move, nothing to split.  */
  if (dst_is_dmr && src_is_dmr)
    DONE;

  /* wD <- wa  */
  if (dst_is_dmr && src_is_vsx)
    {
      rtx chunk0 = gen_rtx_REG (OOmode, src_regno);
      rtx chunk1 = gen_rtx_REG (OOmode, src_regno + 2);
      rtx chunk2 = gen_rtx_REG (OOmode, src_regno + 4);
      rtx chunk3 = gen_rtx_REG (OOmode, src_regno + 6);

      emit_insn (gen_dm_insert1024 (dst, chunk0, chunk1, chunk2, chunk3));
      DONE;
    }

  /* wa <- wD  */
  if (dst_is_vsx && src_is_dmr)
    {
      rtx chunk0 = gen_rtx_REG (XOmode, dst_regno);
      rtx chunk1 = gen_rtx_REG (XOmode, dst_regno + 4);
      emit_insn (gen_dm_extract512 (chunk0, src, const0_rtx));
      emit_insn (gen_dm_extract512 (chunk1, src, const1_rtx));
      DONE;
    }

  gcc_unreachable ();
})


(define_insn_and_split "reload_tdo_load"
  [(set (match_operand:TDO 0 "dmr_register_operand" "=wD")
        (unspec:TDO [(match_operand:TDO 1 "memory_operand" "m")]
                    UNSPEC_DMF_RELOAD_FROM_MEMORY))
   (clobber (match_operand:OO 2 "vsx_register_operand" "=wa"))
   (clobber (match_operand:OO 3 "vsx_register_operand" "=wa"))
   (clobber (match_operand:OO 4 "vsx_register_operand" "=wa"))
   (clobber (match_operand:OO 5 "vsx_register_operand" "=wa"))]
  "TARGET_DMF"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  rtx dest = operands[0];
  rtx src = operands[1];
  rtx pair0 = operands[2];
  rtx pair1 = operands[3];
  rtx pair2 = operands[4];
  rtx pair3 = operands[5];

  if (BYTES_BIG_ENDIAN)
    {
      emit_move_insn (pair0, adjust_address (src, OOmode, 0));
      emit_move_insn (pair1, adjust_address (src, OOmode, 32));
      emit_move_insn (pair2, adjust_address (src, OOmode, 64));
      emit_move_insn (pair3, adjust_address (src, OOmode, 96));
    }
  else
    {
      emit_move_insn (pair3, adjust_address (src, OOmode, 0));
      emit_move_insn (pair2, adjust_address (src, OOmode, 32));
      emit_move_insn (pair1, adjust_address (src, OOmode, 64));
      emit_move_insn (pair0, adjust_address (src, OOmode, 96));
    }
  emit_insn (gen_dm_insert1024 (dest, pair0, pair1, pair2, pair3));
  DONE;
}
  [(set_attr "max_prefixed_insns" "4")])


;; Reload dense math register to memory
(define_insn_and_split "reload_tdo_store"
  [(set (match_operand:TDO 0 "memory_operand" "=m")
        (unspec:TDO [(match_operand:TDO 1 "dmr_register_operand" "wD")]
                    UNSPEC_DMF_RELOAD_TO_MEMORY))
   (clobber (match_operand:XO 2 "vsx_register_operand" "=wa"))]
  "TARGET_DMF"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  rtx dest = operands[0];
  rtx src = operands[1];
  rtx tmp_vsx_512 = operands[2];
  rtx high_mem = adjust_address (dest, XOmode, BYTES_BIG_ENDIAN ? 0 : 64);
  rtx low_mem = adjust_address (dest, XOmode, BYTES_BIG_ENDIAN ? 64 : 0);

  emit_insn (gen_dm_extract512 (tmp_vsx_512, src, const0_rtx));
  emit_move_insn (high_mem, tmp_vsx_512);

  emit_insn (gen_dm_extract512 (tmp_vsx_512, src, const1_rtx));
  emit_move_insn (low_mem, tmp_vsx_512);
  DONE;
}
  [(set_attr "max_prefixed_insns" "4")])


(define_expand "mma_assemble_acc"
  [(match_operand:XO 0 "accumulator_operand")
   (match_operand:V16QI 1 "mma_assemble_input_operand")
   (match_operand:V16QI 2 "mma_assemble_input_operand")
   (match_operand:V16QI 3 "mma_assemble_input_operand")
   (match_operand:V16QI 4 "mma_assemble_input_operand")]
  "TARGET_MMA || TARGET_DMF"
{
  if (TARGET_DMF)
    {
      rtx vp0 = gen_reg_rtx (OOmode);
      rtx vp1 = gen_reg_rtx (OOmode);
      emit_insn (gen_vsx_assemble_pair (vp0, operands[1], operands[2]));
      emit_insn (gen_vsx_assemble_pair (vp1, operands[3], operands[4]));
      emit_insn (gen_dm_insert512 (operands[0], vp0, vp1, const0_rtx));
    }
  else
    {
      rtx src = gen_rtx_UNSPEC_VOLATILE (XOmode,
					 gen_rtvec (4, operands[1], operands[2],
						    operands[3], operands[4]),
					 UNSPECV_MMA_ASSEMBLE);
      emit_move_insn (operands[0], src);
    }
  DONE;
})

;; We cannot update the four output registers atomically, so mark the output
;; as an early clobber so we don't accidentally clobber the input operands.  */

(define_insn_and_split "*mma_assemble_acc"
  [(set (match_operand:XO 0 "accumulator_operand" "=&wD")
	(unspec_volatile:XO
	  [(match_operand:V16QI 1 "mma_assemble_input_operand" "mwa")
	   (match_operand:V16QI 2 "mma_assemble_input_operand" "mwa")
	   (match_operand:V16QI 3 "mma_assemble_input_operand" "mwa")
	   (match_operand:V16QI 4 "mma_assemble_input_operand" "mwa")]
	  UNSPECV_MMA_ASSEMBLE))]
  "TARGET_MMA
   && accumulator_operand (operands[0], XOmode)"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  rtx src = gen_rtx_UNSPEC_VOLATILE (XOmode,
			    	     gen_rtvec (4, operands[1], operands[2],
				       		operands[3], operands[4]),
			    	     UNSPECV_MMA_ASSEMBLE);
  rs6000_split_multireg_move (operands[0], src);
  DONE;
})

(define_expand "mma_disassemble_acc"
  [(match_operand:V16QI 0 "mma_disassemble_output_operand")
   (match_operand:XO 1 "accumulator_operand")
   (match_operand 2 "const_0_to_3_operand")]
  "TARGET_MMA"
{
  rtx src;
  int regoff = INTVAL (operands[2]);
  src = gen_rtx_UNSPEC (V16QImode,
                        gen_rtvec (2, operands[1], GEN_INT (regoff)),
                        UNSPEC_MMA_EXTRACT);
  emit_move_insn (operands[0], src);
  DONE;
})

(define_insn_and_split "*mma_disassemble_acc"
  [(set (match_operand:V16QI 0 "mma_disassemble_output_operand" "=mwa")
       (unspec:V16QI [(match_operand:XO 1 "fpr_reg_operand" "d")
                      (match_operand 2 "const_0_to_3_operand")]
                      UNSPEC_MMA_EXTRACT))]
  "TARGET_MMA
   && fpr_reg_operand (operands[1], XOmode)"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  int reg = REGNO (operands[1]);
  int regoff = INTVAL (operands[2]);
  rtx src = gen_rtx_REG (V16QImode, reg + regoff);
  emit_move_insn (operands[0], src);
  DONE;
})

;; xxmtacc/xxmfacc prime/deprime an accumulator that lives in 4 adjacent
;; FPRs -- they reformat that shared FPR/accumulator storage in place.  On
;; TARGET_DMF, DMRs are a register file entirely separate from the FPRs,
;; so there is no such format to convert and these are a nop.  This expand
;; only exists so __builtin_mma_xxmfacc/xxmtacc (which always resolve to
;; this pattern) correctly do nothing on TARGET_DMF instead of failing
;; to match any insn.
(define_expand "mma_<acc>"
  [(set (match_operand:XO 0 "accumulator_operand")
	(unspec:XO [(match_operand:XO 1 "accumulator_operand")]
		   MMA_ACC))]
  "TARGET_MMA || TARGET_DMF"
{
  if (TARGET_DMF)
    {
      emit_move_insn (operands[0], operands[1]);
      DONE;
    }
})


;; MMA instructions that do not use their accumulators as an input, still
;; must not allow their vector operands to overlap the registers used by
;; the accumulator.  We enforce this by marking the output as early clobber.

(define_insn "*mma_<acc>"
  [(set (match_operand:XO 0 "accumulator_operand" "=&wD")
	(unspec:XO [(match_operand:XO 1 "accumulator_operand" "0")]
		    MMA_ACC))]
  "TARGET_MMA && !TARGET_DMF"
  "<acc> %A0"
  [(set_attr "type" "mma")])

;; We can't have integer constants in XOmode so we wrap this in an
;; UNSPEC_VOLATILE.

(define_insn "mma_xxsetaccz"
  [(set (match_operand:XO 0 "fpr_reg_operand" "=d")
	(unspec_volatile:XO [(const_int 0)]
			    UNSPECV_MMA_XXSETACCZ))]
  "TARGET_MMA"
  "xxsetaccz %A0"
  [(set_attr "type" "mma")])

(define_insn "mma_<vv>"
  [(set (match_operand:XO 0 "fpr_reg_operand" "=&d,&d")
	(unspec:XO [(match_operand:V16QI 1 "vsx_register_operand" "v,?wa")
		    (match_operand:V16QI 2 "vsx_register_operand" "v,?wa")]
		    MMA_VV))]
  "TARGET_MMA"
  "<vv> %A0,%x1,%x2"
  [(set_attr "type" "mma")])

(define_insn "mma_<avv>"
  [(set (match_operand:XO 0 "fpr_reg_operand" "=&d,&d")
	(unspec:XO [(match_operand:XO 1 "fpr_reg_operand" "0,0")
		    (match_operand:V16QI 2 "vsx_register_operand" "v,?wa")
		    (match_operand:V16QI 3 "vsx_register_operand" "v,?wa")]
		    MMA_AVV))]
  "TARGET_MMA"
  "<avv> %A0,%x2,%x3"
  [(set_attr "type" "mma")])

(define_insn "mma_<pv>"
  [(set (match_operand:XO 0 "fpr_reg_operand" "=&d,&d")
	(unspec:XO [(match_operand:OO 1 "vsx_register_operand" "v,?wa")
		    (match_operand:V16QI 2 "vsx_register_operand" "v,?wa")]
		    MMA_PV))]
  "TARGET_MMA"
  "<pv> %A0,%x1,%x2"
  [(set_attr "type" "mma")])

(define_insn "mma_<apv>"
  [(set (match_operand:XO 0 "fpr_reg_operand" "=&d,&d")
	(unspec:XO [(match_operand:XO 1 "fpr_reg_operand" "0,0")
		    (match_operand:OO 2 "vsx_register_operand" "v,?wa")
		    (match_operand:V16QI 3 "vsx_register_operand" "v,?wa")]
		    MMA_APV))]
  "TARGET_MMA"
  "<apv> %A0,%x2,%x3"
  [(set_attr "type" "mma")])

(define_insn "mma_<vvi4i4i8>"
  [(set (match_operand:XO 0 "fpr_reg_operand" "=&d,&d")
	(unspec:XO [(match_operand:V16QI 1 "vsx_register_operand" "v,?wa")
		    (match_operand:V16QI 2 "vsx_register_operand" "v,?wa")
		    (match_operand:SI 3 "const_0_to_15_operand" "n,n")
		    (match_operand:SI 4 "const_0_to_15_operand" "n,n")
		    (match_operand:SI 5 "u8bit_cint_operand" "n,n")]
		    MMA_VVI4I4I8))]
  "TARGET_MMA"
  "<vvi4i4i8> %A0,%x1,%x2,%3,%4,%5"
  [(set_attr "type" "mma")
   (set_attr "prefixed" "yes")])

(define_insn "mma_<avvi4i4i8>"
  [(set (match_operand:XO 0 "fpr_reg_operand" "=&d,&d")
	(unspec:XO [(match_operand:XO 1 "fpr_reg_operand" "0,0")
		    (match_operand:V16QI 2 "vsx_register_operand" "v,?wa")
		    (match_operand:V16QI 3 "vsx_register_operand" "v,?wa")
		    (match_operand:SI 4 "const_0_to_15_operand" "n,n")
		    (match_operand:SI 5 "const_0_to_15_operand" "n,n")
		    (match_operand:SI 6 "u8bit_cint_operand" "n,n")]
		    MMA_AVVI4I4I8))]
  "TARGET_MMA"
  "<avvi4i4i8> %A0,%x2,%x3,%4,%5,%6"
  [(set_attr "type" "mma")
   (set_attr "prefixed" "yes")])

(define_insn "mma_<vvi4i4i2>"
  [(set (match_operand:XO 0 "fpr_reg_operand" "=&d,&d")
	(unspec:XO [(match_operand:V16QI 1 "vsx_register_operand" "v,?wa")
		    (match_operand:V16QI 2 "vsx_register_operand" "v,?wa")
		    (match_operand:SI 3 "const_0_to_15_operand" "n,n")
		    (match_operand:SI 4 "const_0_to_15_operand" "n,n")
		    (match_operand:SI 5 "const_0_to_3_operand" "n,n")]
		    MMA_VVI4I4I2))]
  "TARGET_MMA"
  "<vvi4i4i2> %A0,%x1,%x2,%3,%4,%5"
  [(set_attr "type" "mma")
   (set_attr "prefixed" "yes")])

(define_insn "mma_<avvi4i4i2>"
  [(set (match_operand:XO 0 "fpr_reg_operand" "=&d,&d")
	(unspec:XO [(match_operand:XO 1 "fpr_reg_operand" "0,0")
		    (match_operand:V16QI 2 "vsx_register_operand" "v,?wa")
		    (match_operand:V16QI 3 "vsx_register_operand" "v,?wa")
		    (match_operand:SI 4 "const_0_to_15_operand" "n,n")
		    (match_operand:SI 5 "const_0_to_15_operand" "n,n")
		    (match_operand:SI 6 "const_0_to_3_operand" "n,n")]
		    MMA_AVVI4I4I2))]
  "TARGET_MMA"
  "<avvi4i4i2> %A0,%x2,%x3,%4,%5,%6"
  [(set_attr "type" "mma")
   (set_attr "prefixed" "yes")])

(define_insn "mma_<vvi4i4>"
  [(set (match_operand:XO 0 "fpr_reg_operand" "=&d,&d")
	(unspec:XO [(match_operand:V16QI 1 "vsx_register_operand" "v,?wa")
		    (match_operand:V16QI 2 "vsx_register_operand" "v,?wa")
		    (match_operand:SI 3 "const_0_to_15_operand" "n,n")
		    (match_operand:SI 4 "const_0_to_15_operand" "n,n")]
		    MMA_VVI4I4))]
  "TARGET_MMA"
  "<vvi4i4> %A0,%x1,%x2,%3,%4"
  [(set_attr "type" "mma")
   (set_attr "prefixed" "yes")])

(define_insn "mma_<avvi4i4>"
  [(set (match_operand:XO 0 "fpr_reg_operand" "=&d,&d")
	(unspec:XO [(match_operand:XO 1 "fpr_reg_operand" "0,0")
		    (match_operand:V16QI 2 "vsx_register_operand" "v,?wa")
		    (match_operand:V16QI 3 "vsx_register_operand" "v,?wa")
		    (match_operand:SI 4 "const_0_to_15_operand" "n,n")
		    (match_operand:SI 5 "const_0_to_15_operand" "n,n")]
		    MMA_AVVI4I4))]
  "TARGET_MMA"
  "<avvi4i4> %A0,%x2,%x3,%4,%5"
  [(set_attr "type" "mma")
   (set_attr "prefixed" "yes")])

(define_insn "mma_<pvi4i2>"
  [(set (match_operand:XO 0 "fpr_reg_operand" "=&d,&d")
	(unspec:XO [(match_operand:OO 1 "vsx_register_operand" "v,?wa")
		    (match_operand:V16QI 2 "vsx_register_operand" "v,?wa")
		    (match_operand:SI 3 "const_0_to_15_operand" "n,n")
		    (match_operand:SI 4 "const_0_to_3_operand" "n,n")]
		    MMA_PVI4I2))]
  "TARGET_MMA"
  "<pvi4i2> %A0,%x1,%x2,%3,%4"
  [(set_attr "type" "mma")
   (set_attr "prefixed" "yes")])

(define_insn "mma_<apvi4i2>"
  [(set (match_operand:XO 0 "fpr_reg_operand" "=&d,&d")
	(unspec:XO [(match_operand:XO 1 "fpr_reg_operand" "0,0")
		    (match_operand:OO 2 "vsx_register_operand" "v,?wa")
		    (match_operand:V16QI 3 "vsx_register_operand" "v,?wa")
		    (match_operand:SI 4 "const_0_to_15_operand" "n,n")
		    (match_operand:SI 5 "const_0_to_3_operand" "n,n")]
		    MMA_APVI4I2))]
  "TARGET_MMA"
  "<apvi4i2> %A0,%x2,%x3,%4,%5"
  [(set_attr "type" "mma")
   (set_attr "prefixed" "yes")])

(define_insn "mma_<vvi4i4i4>"
  [(set (match_operand:XO 0 "fpr_reg_operand" "=&d,&d")
	(unspec:XO [(match_operand:V16QI 1 "vsx_register_operand" "v,?wa")
		    (match_operand:V16QI 2 "vsx_register_operand" "v,?wa")
		    (match_operand:SI 3 "const_0_to_15_operand" "n,n")
		    (match_operand:SI 4 "const_0_to_15_operand" "n,n")
		    (match_operand:SI 5 "const_0_to_15_operand" "n,n")]
		    MMA_VVI4I4I4))]
  "TARGET_MMA"
  "<vvi4i4i4> %A0,%x1,%x2,%3,%4,%5"
  [(set_attr "type" "mma")
   (set_attr "prefixed" "yes")])

(define_insn "mma_<avvi4i4i4>"
  [(set (match_operand:XO 0 "fpr_reg_operand" "=&d,&d")
	(unspec:XO [(match_operand:XO 1 "fpr_reg_operand" "0,0")
		    (match_operand:V16QI 2 "vsx_register_operand" "v,?wa")
		    (match_operand:V16QI 3 "vsx_register_operand" "v,?wa")
		    (match_operand:SI 4 "const_0_to_15_operand" "n,n")
		    (match_operand:SI 5 "const_0_to_15_operand" "n,n")
		    (match_operand:SI 6 "const_0_to_15_operand" "n,n")]
		    MMA_AVVI4I4I4))]
  "TARGET_MMA"
  "<avvi4i4i4> %A0,%x2,%x3,%4,%5,%6"
  [(set_attr "type" "mma")
   (set_attr "prefixed" "yes")])
