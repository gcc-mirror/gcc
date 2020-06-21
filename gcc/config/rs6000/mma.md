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
  [(set (match_operand:PXI 0 "nonimmediate_operand" "=d,m,d")
	(match_operand:PXI 1 "input_operand" "m,d,d"))]
  "TARGET_MMA
   && (gpc_reg_operand (operands[0], PXImode)
       || gpc_reg_operand (operands[1], PXImode))"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  rs6000_split_multireg_move (operands[0], operands[1]);
  DONE;
}
  [(set_attr "type" "vecload,vecstore,veclogical")
   (set_attr "length" "8,8,16")
   (set_attr "max_prefixed_insns" "2,2,*")])
