;; Matrix-Multiply Assist (MMA) patterns.
;; Copyright (C) 2020 Free Software Foundation, Inc.
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

;; The MMA patterns use the multi-register PXImode and POImode partial
;; integer modes to implement the target specific __vector_quad and
;; __vector_pair types that the MMA built-in functions reference.
;; To use these modes, we must define XImode and OImode move patterns
;; so the independent parts of the compiler can use our large partial
;; integer modes.  However, if we enable the XImode and OImode move
;; patterns, then the compiler will attempt to use them and this can
;; cause byte swapping issues on litte-endian systems.  We don't need
;; the XImode and OImode move patterns for actual code generation,
;; therefore, we define the XImode and OImode move patterns, but we
;; disable their use with a "false" condition flag.

(define_constants [(MAX_MMA_OPERANDS 7)])

;; Constants for creating unspecs

(define_c_enum "unspec"
  [UNSPEC_MMA_ASSEMBLE_ACC
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


;; Define a disabled OImode move pattern, so we can use POImode.
(define_expand "movoi"
  [(set (match_operand:OI 0 "nonimmediate_operand")
	(match_operand:OI 1 "input_operand"))]
  "0"
{
  gcc_unreachable ();
})

;; Vector pair support.  POImode can only live in VSRs.
(define_expand "movpoi"
  [(set (match_operand:POI 0 "nonimmediate_operand")
	(match_operand:POI 1 "input_operand"))]
  "TARGET_MMA"
{
  rs6000_emit_move (operands[0], operands[1], POImode);
  DONE;
})

(define_insn_and_split "*movpoi"
  [(set (match_operand:POI 0 "nonimmediate_operand" "=wa,m,wa")
	(match_operand:POI 1 "input_operand" "m,wa,wa"))]
  "TARGET_MMA
   && (gpc_reg_operand (operands[0], POImode)
       || gpc_reg_operand (operands[1], POImode))"
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
   (set_attr "length" "*,*,8")])


;; Define a disabled XImode move pattern, so we can use PXImode.
(define_expand "movxi"
  [(set (match_operand:XI 0 "nonimmediate_operand")
	(match_operand:XI 1 "input_operand"))]
  "0"
{
  gcc_unreachable ();
})

;; Vector quad support.  PXImode can only live in FPRs.
(define_expand "movpxi"
  [(set (match_operand:PXI 0 "nonimmediate_operand")
	(match_operand:PXI 1 "input_operand"))]
  "TARGET_MMA"
{
  rs6000_emit_move (operands[0], operands[1], PXImode);
  DONE;
})

(define_insn_and_split "*movpxi"
  [(set (match_operand:PXI 0 "nonimmediate_operand" "=d,m,d,d")
	(match_operand:PXI 1 "input_operand" "m,d,d,O"))]
  "TARGET_MMA
   && (gpc_reg_operand (operands[0], PXImode)
       || gpc_reg_operand (operands[1], PXImode))"
  "@
   #
   #
   #
   xxsetaccz %A0"
  "&& reload_completed
   && !(fpr_reg_operand (operands[0], PXImode) && operands[1] == const0_rtx)"
  [(const_int 0)]
{
  rs6000_split_multireg_move (operands[0], operands[1]);
  DONE;
}
  [(set_attr "type" "vecload,vecstore,veclogical,mma")
   (set_attr "length" "8,8,16,*")
   (set_attr "max_prefixed_insns" "2,2,*,*")])

(define_expand "vsx_assemble_pair"
  [(match_operand:POI 0 "vsx_register_operand")
   (match_operand:V16QI 1 "input_operand")
   (match_operand:V16QI 2 "input_operand")]
  "TARGET_MMA"
{
  rtx dst;

  /* Let the compiler know the code below fully defines our output value.  */
  emit_clobber (operands[0]);

  dst = simplify_gen_subreg (V16QImode, operands[0], POImode, 0);
  emit_move_insn (dst, operands[1]);
  dst = simplify_gen_subreg (V16QImode, operands[0], POImode, 16);
  emit_move_insn (dst, operands[2]);
  DONE;
})

(define_expand "mma_assemble_acc"
  [(match_operand:PXI 0 "fpr_reg_operand")
   (match_operand:V16QI 1 "input_operand")
   (match_operand:V16QI 2 "input_operand")
   (match_operand:V16QI 3 "input_operand")
   (match_operand:V16QI 4 "input_operand")]
  "TARGET_MMA"
{
  rtx src = gen_rtx_UNSPEC (PXImode,
			    gen_rtvec (4, operands[1], operands[2],
				       operands[3], operands[4]),
			    UNSPEC_MMA_ASSEMBLE_ACC);
  emit_move_insn (operands[0], src);
  DONE;
})

(define_insn_and_split "*mma_assemble_acc"
  [(set (match_operand:PXI 0 "fpr_reg_operand" "=d")
	(unspec:PXI [(match_operand:V16QI 1 "mma_assemble_input_operand" "mwa")
		     (match_operand:V16QI 2 "mma_assemble_input_operand" "mwa")
		     (match_operand:V16QI 3 "mma_assemble_input_operand" "mwa")
		     (match_operand:V16QI 4 "mma_assemble_input_operand" "mwa")]
		     UNSPEC_MMA_ASSEMBLE_ACC))]
  "TARGET_MMA
   && fpr_reg_operand (operands[0], PXImode)"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  rtx src = gen_rtx_UNSPEC (PXImode,
			    gen_rtvec (4, operands[1], operands[2],
				       operands[3], operands[4]),
			    UNSPEC_MMA_ASSEMBLE_ACC);
  rs6000_split_multireg_move (operands[0], src);
  DONE;
})

;; MMA instructions that do not use their accumulators as an input, still
;; must not allow their vector operands to overlap the registers used by
;; the accumulator.  We enforce this by marking the output as early clobber.

(define_insn "mma_<acc>"
  [(set (match_operand:PXI 0 "fpr_reg_operand" "=&d")
	(unspec:PXI [(match_operand:PXI 1 "fpr_reg_operand" "0")]
		    MMA_ACC))]
  "TARGET_MMA"
  "<acc> %A0"
  [(set_attr "type" "mma")])

(define_expand "mma_xxsetaccz"
  [(set (match_operand:PXI 0 "fpr_reg_operand")
	(const_int 0))]
  "TARGET_MMA"
{
  emit_insn (gen_movpxi (operands[0], const0_rtx));
  DONE;
})

(define_insn "mma_<vv>"
  [(set (match_operand:PXI 0 "fpr_reg_operand" "=&d")
	(unspec:PXI [(match_operand:V16QI 1 "vsx_register_operand" "wa")
		     (match_operand:V16QI 2 "vsx_register_operand" "wa")]
		     MMA_VV))]
  "TARGET_MMA"
  "<vv> %A0,%x1,%x2"
  [(set_attr "type" "mma")])

(define_insn "mma_<avv>"
  [(set (match_operand:PXI 0 "fpr_reg_operand" "=&d")
	(unspec:PXI [(match_operand:PXI 1 "fpr_reg_operand" "0")
		     (match_operand:V16QI 2 "vsx_register_operand" "wa")
		     (match_operand:V16QI 3 "vsx_register_operand" "wa")]
		     MMA_AVV))]
  "TARGET_MMA"
  "<avv> %A0,%x2,%x3"
  [(set_attr "type" "mma")])

(define_insn "mma_<pv>"
  [(set (match_operand:PXI 0 "fpr_reg_operand" "=&d")
	(unspec:PXI [(match_operand:POI 1 "vsx_register_operand" "wa")
		     (match_operand:V16QI 2 "vsx_register_operand" "wa")]
		     MMA_PV))]
  "TARGET_MMA"
  "<pv> %A0,%x1,%x2"
  [(set_attr "type" "mma")])

(define_insn "mma_<apv>"
  [(set (match_operand:PXI 0 "fpr_reg_operand" "=&d")
	(unspec:PXI [(match_operand:PXI 1 "fpr_reg_operand" "0")
		     (match_operand:POI 2 "vsx_register_operand" "wa")
		     (match_operand:V16QI 3 "vsx_register_operand" "wa")]
		     MMA_APV))]
  "TARGET_MMA"
  "<apv> %A0,%x2,%x3"
  [(set_attr "type" "mma")])

(define_insn "mma_<vvi4i4i8>"
  [(set (match_operand:PXI 0 "fpr_reg_operand" "=&d")
	(unspec:PXI [(match_operand:V16QI 1 "vsx_register_operand" "wa")
		     (match_operand:V16QI 2 "vsx_register_operand" "wa")
		     (match_operand:SI 3 "const_0_to_15_operand" "n")
		     (match_operand:SI 4 "const_0_to_15_operand" "n")
		     (match_operand:SI 5 "u8bit_cint_operand" "n")]
		     MMA_VVI4I4I8))]
  "TARGET_MMA"
  "<vvi4i4i8> %A0,%x1,%x2,%3,%4,%5"
  [(set_attr "type" "mma")
   (set_attr "length" "8")])

(define_insn "mma_<avvi4i4i8>"
  [(set (match_operand:PXI 0 "fpr_reg_operand" "=&d")
	(unspec:PXI [(match_operand:PXI 1 "fpr_reg_operand" "0")
		     (match_operand:V16QI 2 "vsx_register_operand" "wa")
		     (match_operand:V16QI 3 "vsx_register_operand" "wa")
		     (match_operand:SI 4 "const_0_to_15_operand" "n")
		     (match_operand:SI 5 "const_0_to_15_operand" "n")
		     (match_operand:SI 6 "u8bit_cint_operand" "n")]
		     MMA_AVVI4I4I8))]
  "TARGET_MMA"
  "<avvi4i4i8> %A0,%x2,%x3,%4,%5,%6"
  [(set_attr "type" "mma")
   (set_attr "length" "8")])

(define_insn "mma_<vvi4i4i2>"
  [(set (match_operand:PXI 0 "fpr_reg_operand" "=&d")
	(unspec:PXI [(match_operand:V16QI 1 "vsx_register_operand" "wa")
		     (match_operand:V16QI 2 "vsx_register_operand" "wa")
		     (match_operand:SI 3 "const_0_to_15_operand" "n")
		     (match_operand:SI 4 "const_0_to_15_operand" "n")
		     (match_operand:SI 5 "const_0_to_3_operand" "n")]
		     MMA_VVI4I4I2))]
  "TARGET_MMA"
  "<vvi4i4i2> %A0,%x1,%x2,%3,%4,%5"
  [(set_attr "type" "mma")
   (set_attr "length" "8")])

(define_insn "mma_<avvi4i4i2>"
  [(set (match_operand:PXI 0 "fpr_reg_operand" "=&d")
	(unspec:PXI [(match_operand:PXI 1 "fpr_reg_operand" "0")
		     (match_operand:V16QI 2 "vsx_register_operand" "wa")
		     (match_operand:V16QI 3 "vsx_register_operand" "wa")
		     (match_operand:SI 4 "const_0_to_15_operand" "n")
		     (match_operand:SI 5 "const_0_to_15_operand" "n")
		     (match_operand:SI 6 "const_0_to_3_operand" "n")]
		     MMA_AVVI4I4I2))]
  "TARGET_MMA"
  "<avvi4i4i2> %A0,%x2,%x3,%4,%5,%6"
  [(set_attr "type" "mma")
   (set_attr "length" "8")])

(define_insn "mma_<vvi4i4>"
  [(set (match_operand:PXI 0 "fpr_reg_operand" "=&d")
	(unspec:PXI [(match_operand:V16QI 1 "vsx_register_operand" "wa")
		     (match_operand:V16QI 2 "vsx_register_operand" "wa")
		     (match_operand:SI 3 "const_0_to_15_operand" "n")
		     (match_operand:SI 4 "const_0_to_15_operand" "n")]
		     MMA_VVI4I4))]
  "TARGET_MMA"
  "<vvi4i4> %A0,%x1,%x2,%3,%4"
  [(set_attr "type" "mma")
   (set_attr "length" "8")])

(define_insn "mma_<avvi4i4>"
  [(set (match_operand:PXI 0 "fpr_reg_operand" "=&d")
	(unspec:PXI [(match_operand:PXI 1 "fpr_reg_operand" "0")
		     (match_operand:V16QI 2 "vsx_register_operand" "wa")
		     (match_operand:V16QI 3 "vsx_register_operand" "wa")
		     (match_operand:SI 4 "const_0_to_15_operand" "n")
		     (match_operand:SI 5 "const_0_to_15_operand" "n")]
		     MMA_AVVI4I4))]
  "TARGET_MMA"
  "<avvi4i4> %A0,%x2,%x3,%4,%5"
  [(set_attr "type" "mma")
   (set_attr "length" "8")])

(define_insn "mma_<pvi4i2>"
  [(set (match_operand:PXI 0 "fpr_reg_operand" "=&d")
	(unspec:PXI [(match_operand:POI 1 "vsx_register_operand" "wa")
		     (match_operand:V16QI 2 "vsx_register_operand" "wa")
		     (match_operand:SI 3 "const_0_to_15_operand" "n")
		     (match_operand:SI 4 "const_0_to_3_operand" "n")]
		     MMA_PVI4I2))]
  "TARGET_MMA"
  "<pvi4i2> %A0,%x1,%x2,%3,%4"
  [(set_attr "type" "mma")
   (set_attr "length" "8")])

(define_insn "mma_<apvi4i2>"
  [(set (match_operand:PXI 0 "fpr_reg_operand" "=&d")
	(unspec:PXI [(match_operand:PXI 1 "fpr_reg_operand" "0")
		     (match_operand:POI 2 "vsx_register_operand" "wa")
		     (match_operand:V16QI 3 "vsx_register_operand" "wa")
		     (match_operand:SI 4 "const_0_to_15_operand" "n")
		     (match_operand:SI 5 "const_0_to_3_operand" "n")]
		     MMA_APVI4I2))]
  "TARGET_MMA"
  "<apvi4i2> %A0,%x2,%x3,%4,%5"
  [(set_attr "type" "mma")
   (set_attr "length" "8")])

(define_insn "mma_<vvi4i4i4>"
  [(set (match_operand:PXI 0 "fpr_reg_operand" "=&d")
	(unspec:PXI [(match_operand:V16QI 1 "vsx_register_operand" "wa")
		     (match_operand:V16QI 2 "vsx_register_operand" "wa")
		     (match_operand:SI 3 "const_0_to_15_operand" "n")
		     (match_operand:SI 4 "const_0_to_15_operand" "n")
		     (match_operand:SI 5 "const_0_to_15_operand" "n")]
		     MMA_VVI4I4I4))]
  "TARGET_MMA"
  "<vvi4i4i4> %A0,%x1,%x2,%3,%4,%5"
  [(set_attr "type" "mma")
   (set_attr "length" "8")])

(define_insn "mma_<avvi4i4i4>"
  [(set (match_operand:PXI 0 "fpr_reg_operand" "=&d")
	(unspec:PXI [(match_operand:PXI 1 "fpr_reg_operand" "0")
		     (match_operand:V16QI 2 "vsx_register_operand" "wa")
		     (match_operand:V16QI 3 "vsx_register_operand" "wa")
		     (match_operand:SI 4 "const_0_to_15_operand" "n")
		     (match_operand:SI 5 "const_0_to_15_operand" "n")
		     (match_operand:SI 6 "const_0_to_15_operand" "n")]
		     MMA_AVVI4I4I4))]
  "TARGET_MMA"
  "<avvi4i4i4> %A0,%x2,%x3,%4,%5,%6"
  [(set_attr "type" "mma")
   (set_attr "length" "8")])
