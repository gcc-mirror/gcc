;; Machine Descriptions for R8C/M16C/M32C
;; Copyright (C) 2005
;; Free Software Foundation, Inc.
;; Contributed by Red Hat.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.
;;
;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING.  If not, write to the Free
;; Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

; conditionals - cmp, jcc, setcc, etc.

; Until support for relaxing is supported in gas, we must assume that
; short labels won't reach, so we must use long labels.
; Unfortunately, there aren't any conditional jumps with long labels,
; so instead we invert the conditional and jump around a regular jump.

; Note that we can, at some point in the future, add code to omit the
; "cmp" portion of the insn if the preceding insn happened to set the
; right flags already.  For example, a mov followed by a "cmp *,0" is
; redundant; the move already set the Z flag.

(define_insn "cbranchqi4"
  [(set (pc) (if_then_else
	      (match_operator 0 "m32c_cmp_operator"
			      [(match_operand:QI 1 "mrai_operand" "RqiSd,RqiSd,?Rmm,?Rmm")
			       (match_operand:QI 2 "mrai_operand" "iRqiSd,?Rmm,iRqiSd,?Rmm")])
              (label_ref (match_operand 3 "" ""))
	      (pc)))]
  ""
  "cmp.b\t%2,%1\n\tj%C0\t1f\n\tjmp.a\t%l3\n1:"
;  "cmp.b\t%2,%1\n\tj%c0\t%l3"
  [(set_attr "flags" "oszc,oszc,oszc,oszc")]
  )

(define_insn "cbranchhi4"
  [(set (pc) (if_then_else
	      (match_operator 0 "m32c_cmp_operator"
			      [(match_operand:HI 1 "mrai_operand" "Rhi,?Sd,Rhi,?Sd,?Rmm,?Rmm")
			       (match_operand:HI 2 "mrai_operand" "iRhiSd,iRhiSd,?Rmm,?Rmm,iRhiSd,?Rmm")])
              (label_ref (match_operand 3 "" ""))
	      (pc)))]
  ""
  "cmp.w\t%2,%1\n\tj%C0\t1f\n\tjmp.a\t%l3\n1:"
;  "cmp.w\t%2,%1\n\tj%c0\t%l3"
  [(set_attr "flags" "oszc,oszc,oszc,oszc,oszc,oszc")]
  )

(define_insn "cbranchpsi4"
  [(set (pc) (if_then_else
	      (match_operator 0 "m32c_cmp_operator"
			      [(match_operand:PSI 1 "mrai_operand" "RsiSd,RsiSd,?Rmm,?Rmm")
			       (match_operand:PSI 2 "mrai_operand" "iRsiSd,?Rmm,iRsiSd,?Rmm")])
              (label_ref (match_operand 3 "" ""))
	      (pc)))]
  "TARGET_A24"
  "cmp.l\t%2,%1\n\tj%C0\t1f\n\tjmp.a\t%l3\n1:"
;  "cmp.l\t%2,%1\n\tj%c0\t%l3"
  [(set_attr "flags" "oszc,oszc,oszc,oszc")]
  )
