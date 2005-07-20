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

;; Bit-wise operations (and, ior, xor, shift)

(define_insn "andqi3"
  [(set (match_operand:QI 0 "mra_operand" "=RhlSd,RhlSd,??Rmm,??Rmm")
	(and:QI (match_operand:QI 1 "mra_operand" "%0,0,0,0")
		(match_operand:QI 2 "mrai_operand" "iRhlSd,?Rmm,iRhlSd,?Rmm")))]
  ""
  "and.b\t%x2,%0"
  [(set_attr "flags" "sz,sz,sz,sz")]
  )

(define_insn "andhi3"
  [(set (match_operand:HI 0 "mra_operand" "=RhiSd,??Rmm,RhiSd,??Rmm")
	(and:HI (match_operand:HI 1 "mra_operand" "%0,0,0,0")
		(match_operand:HI 2 "mrai_operand" "iRhiSd,?Rmm,?Rmm,iRhiSd")))]
  ""
  "and.w\t%X2,%0"
  [(set_attr "flags" "sz,sz,sz,sz")]
  )

(define_insn "iorqi3"
  [(set (match_operand:QI 0 "mra_operand" "=RqiSd,??Rmm,RqiSd,??Rmm")
	(ior:QI (match_operand:QI 1 "mra_operand" "%0,0,0,0")
		(match_operand:QI 2 "mrai_operand" "iRhlSd,iRhlSd,?Rmm,?Rmm")))]
  ""
  "or.b\t%x2,%0"
  [(set_attr "flags" "sz,sz,sz,sz")]
  )

(define_insn "iorhi3"
  [(set (match_operand:HI 0 "mra_operand" "=RhiSd,RhiSd,??Rmm,??Rmm")
	(ior:HI (match_operand:HI 1 "mra_operand" "%0,0,0,0")
		(match_operand:HI 2 "mrai_operand" "iRhiSd,?Rmm,iRhiSd,?Rmm")))]
  ""
  "or.w\t%X2,%0"
  [(set_attr "flags" "sz,sz,sz,sz")]
  )

(define_insn "xorqi3"
  [(set (match_operand:QI 0 "mra_operand" "=RhlSd,RhlSd,??Rmm,??Rmm")
	(xor:QI (match_operand:QI 1 "mra_operand" "%0,0,0,0")
		(match_operand:QI 2 "mrai_operand" "iRhlSd,?Rmm,iRhlSd,?Rmm")))]
  ""
  "xor.b\t%x2,%0"
  [(set_attr "flags" "sz,sz,sz,sz")]
  )

(define_insn "xorhi3"
  [(set (match_operand:HI 0 "mra_operand" "=RhiSd,RhiSd,??Rmm,??Rmm")
	(xor:HI (match_operand:HI 1 "mra_operand" "%0,0,0,0")
		(match_operand:HI 2 "mrai_operand" "iRhiSd,?Rmm,iRhiSd,?Rmm")))]
  ""
  "xor.w\t%X2,%0"
  [(set_attr "flags" "sz,sz,sz,sz")]
  )

(define_insn "one_cmplqi2"
  [(set (match_operand:QI 0 "mra_operand" "=RhlSd,??Rmm")
	(not:QI (match_operand:QI 1 "mra_operand" "0,0")))]
  ""
  "not.b\t%0"
  [(set_attr "flags" "sz,sz")]
  )

(define_insn "one_cmplhi2"
  [(set (match_operand:HI 0 "mra_operand" "=RhiSd,??Rmm")
	(not:HI (match_operand:HI 1 "mra_operand" "0,0")))]
  ""
  "not.w\t%0"
  [(set_attr "flags" "sz,sz")]
  )
