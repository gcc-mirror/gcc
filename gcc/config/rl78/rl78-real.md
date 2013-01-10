;;  Machine Description for Renesas RL78 processors
;;  Copyright (C) 2011-2013 Free Software Foundation, Inc.
;;  Contributed by Red Hat.

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;; The insns in this file correspond to the actual opcodes the RL78
;; can issue with real registers.  All insns in here should be
;; conditional on rl78_real_insns_ok() returning true, and should
;; allow virtual registers in their predicates - the reorg pass that
;; allocates physical registers uses the constraints to select
;; registers, but insns with virtual registers MUST match one of these
;; patterns - other than the constraints - so that the operand info is
;; properly set up for the alloc pass.

;;---------- Moving ------------------------

(define_insn "movqi_es"
  [(set (reg:QI ES_REG)
	(match_operand:QI 0 "register_operand" "a"))]
  ""
  "mov\tes, %0"
)

(define_insn "movqi_cs"
  [(set (reg:QI CS_REG)
	(match_operand:QI 0 "register_operand" "a"))]
  ""
  "mov\tcs, %0"
)

(define_insn "*movqi_real"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=g,RaxbcWab,RaxbcWab,a,                          bcx,R, WabWd2WhlWh1WhbWbcWs1v, bcx")
	(match_operand    1 "general_operand"      "0,K,        M,       RInt8sJvWabWdeWd2WhlWh1WhbWbcWs1,Wab,aInt8J,a,                      R"))]
  "rl78_real_insns_ok ()"
  "@
   ; mov\t%0, %1
   oneb\t%0
   clrb\t%0
   mov\t%0, %1
   mov\t%0, %1
   mov\t%0, %1
   mov\t%0, %1
   mov\t%0, %S1"
)

(define_insn "*movhi_real"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=g,AB,AB,RSv,A,BDTvSWabWd2WdeWhlWh1WbcWs1, BDT,ABDT,v")
	(match_operand:HI 1 "general_operand"      " 0,K, M, i,  BDTvSWabWd2WdeWh1WhlWbcWs1,A, BDT,vS,  ABDT"))]
  "rl78_real_insns_ok ()"
  "@
   ; movw\t%0, %1
   onew\t%0
   clrw\t%0
   movw\t%0, %1
   movw\t%0, %1
   movw\t%0, %1
   movw\t%0, %S1
   movw\t%0, %1
   movw\t%0, %1"
)

;;---------- Conversions ------------------------

(define_insn "*zero_extendqihi2_real"
  [(set (match_operand:HI                 0 "nonimmediate_operand" "=rv,A")
	(zero_extend:HI (match_operand:QI 1 "general_operand" "0,a")))]
  "rl78_real_insns_ok ()"
  "@
   mov\t%Q0, #0
   mov\tx, a \;mov\ta, #0"
  )

(define_insn "*extendqihi2_real"
  [(set (match_operand:HI                 0 "nonimmediate_operand" "=A,A")
	(sign_extend:HI (match_operand:QI 1 "general_operand" "x,a")))]
  "rl78_real_insns_ok ()"
  "@
   shlw\t%0, 8 \;sarw\t%0, 8
   sarw\t%0, 8"
  )

;;---------- Arithmetic ------------------------

(define_insn "*addqi3_real"
  [(set (match_operand:QI          0 "nonimmediate_operand"  "=rvWabWhlWh1,rvWabWhlWh1,a,*bcdehl")
	(plus:QI (match_operand:QI 1 "general_operand"  "%0,0,0,0")
		 (match_operand:QI 2 "general_operand" "K,L,RWhlWh1i,a")))
   ]
  "rl78_real_insns_ok ()"
  "@
    inc\t%0
    dec\t%0
    add\t%0, %2
    add\t%0, %2"
)

(define_insn "*addhi3_real"
  [(set (match_operand:HI          0 "nonimmediate_operand"  "=vABDTWh1Wab,vABDTWh1Wab,v,v,A,S,S,A")
	(plus:HI (match_operand:HI 1 "general_operand"  "%0,0,0,0,0,0,0,S")
		 (match_operand:HI 2 "general_operand" "K,L,N,O,RWh1WhlWabiv,Int8,J,Ri")))
   ]
  "rl78_real_insns_ok ()"
  "@
   incw\t%0
   decw\t%0
   incw\t%0 \;incw\t%0
   decw\t%0 \;decw\t%0
   addw\t%0, %p2
   addw\t%0, %2
   subw\t%0, %m2
   movw\t%0, %1 \;addw\t%0, %2"
)

(define_insn "*addqihi3a_real"
  [(set (match_operand:HI          0 "register_operand"  "=r")
	(plus:HI (zero_extend:HI (match_operand:QI 1 "register_operand"  "%r"))
		 (match_operand:HI 2 "register_operand" "r")))
   ]
  "rl78_real_insns_ok ()"
  "add\t%q0, %q1 \;addc\t%Q0, #0"
)

(define_insn "*subqi3_real"
  [(set (match_operand:QI           0 "nonimmediate_operand"  "=a,R,v")
	(minus:QI (match_operand:QI 1 "general_operand"  "0,0,0")
		  (match_operand:QI 2 "general_operand" "RiWabWhbWh1Whl,a,i")))
   ]
  "rl78_real_insns_ok ()"
  "sub\t%0, %2"
)

(define_insn "*subhi3_real"
  [(set (match_operand:HI           0 "nonimmediate_operand"  "=A,S")
	(minus:HI (match_operand:HI 1 "general_operand"  "0,0")
		  (match_operand:HI 2 "general_operand" "iBDTWabWh1v,i")))
   ]
  "rl78_real_insns_ok ()"
  "subw\t%0, %2"
)

(define_insn "*umulhi3_shift_real"
  [(set (match_operand:HI 0 "register_operand" "=A,A")
        (mult:HI (match_operand:HI 1 "rl78_nonfar_operand" "0,0")
                 (match_operand:HI 2 "rl78_24_operand" "N,i")))]
  "rl78_real_insns_ok ()"
  "@
   shlw\t%0, 1
   shlw\t%0, 2"
)

(define_insn "*umulqihi3_real"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=A")
        (mult:HI (zero_extend:HI (match_operand:QI 1 "general_operand" "%a"))
                 (zero_extend:HI (match_operand:QI 2 "general_operand" "x"))))]
  "rl78_real_insns_ok ()"
  "mulu\t%2"
)

(define_insn "*andqi3_real"
  [(set (match_operand:QI         0 "nonimmediate_operand"  "=A,R,v")
	(and:QI (match_operand:QI 1 "general_operand"       "%0,0,0")
		(match_operand:QI 2 "general_operand"       "iRvWabWhbWh1Whl,A,i")))
   ]
  "rl78_real_insns_ok ()"
  "and\t%0, %2"
)

(define_insn "*iorqi3_real"
  [(set (match_operand:QI         0 "nonimmediate_operand"  "=A,R,v")
	(ior:QI (match_operand:QI 1 "general_operand"       "%0,0,0")
		(match_operand:QI 2 "general_operand"       "iRvWabWhbWh1Whl,A,i")))
   ]
  "rl78_real_insns_ok ()"
  "or\t%0, %2"
)

(define_insn "*xorqi3_real"
  [(set (match_operand:QI         0 "nonimmediate_operand"  "=A,R,v")
	(xor:QI (match_operand:QI 1 "general_operand"       "%0,0,0")
		(match_operand    2 "general_operand"       "iRvWabWhbWh1Whl,A,i")))
   ]
  "rl78_real_insns_ok ()"
  "xor\t%0, %2"
)

;;---------- Shifts ------------------------

(define_insn "*ashlqi3_real"
  [(set (match_operand:QI            0 "nonimmediate_operand"  "=abc,a,a")
	(ashift:QI (match_operand:QI 1 "general_operand"  "0,0,0")
		   (match_operand:QI 2 "general_operand" "Int3,bc,dehl")))
   ]
  "rl78_real_insns_ok ()"
  "@
   shl\t%0, %u2
   cmp0 %2\; bz $2f\; 1: shl\t%0, 1 \;dec %2 \;bnz $1b\;2:
   inc %2\;dec %2\;bz $2f\;1: shl\t%0, 1 \;dec %2 \;bnz $1b\;2:"
)

(define_insn "*ashlhi3_real"
  [(set (match_operand:HI            0 "nonimmediate_operand"  "=AB,A,A")
	(ashift:HI (match_operand:HI 1 "general_operand"  "0,0,0")
		   (match_operand:QI 2 "general_operand" "P,bc,dehl")))
   ]
  "rl78_real_insns_ok ()"
  "@
   shlw\t%0, %u2
   cmp0 %2\; bz $2f\; 1: shlw\t%0, 1 \;dec %2 \;bnz $1b\;2:
   inc %2\;dec %2\;bz $2f\;1: shlw\t%0, 1 \;dec %2 \;bnz $1b\;2:"
)

;;----------

(define_insn "*ashrqi3_real"
  [(set (match_operand:QI              0 "nonimmediate_operand"  "=abc,a,a")
	(ashiftrt:QI (match_operand:QI 1 "general_operand"  "0,0,0")
		     (match_operand:QI 2 "general_operand" "Int3,bc,dehl")))
   ]
  "rl78_real_insns_ok ()"
  "@
   sar\t%0, %u2
   cmp0 %2\; bz $2f\; 1: sar\t%0, 1 \;dec %2 \;bnz $1b\;2:
   inc %2\;dec %2\;bz $2f\;1: sar\t%0, 1\;dec %2 \;bnz $1b\;2:"
)

(define_insn "*ashrhi3_real"
  [(set (match_operand:HI              0 "nonimmediate_operand"  "=AB,A,A")
	(ashiftrt:HI (match_operand:HI 1 "general_operand"  "0,0,0")
		     (match_operand:QI 2 "general_operand" "P,bc,dehl")))
   ]
  "rl78_real_insns_ok ()"
  "@
   sarw\t%0, %u2
   cmp0 %2\; bz $2f\; 1: sarw\t%0, 1 \;dec %2 \;bnz $1b\;2:
   inc %2\;dec %2\;bz $2f\;1: sarw\t%0, 1\;dec %2\;bnz $1b\;2:"
)

;;----------

(define_insn "*lshrqi3_real"
  [(set (match_operand:QI              0 "nonimmediate_operand"  "=abc,a,a")
	(lshiftrt:QI (match_operand:QI 1 "general_operand"  "0,0,0")
		     (match_operand:QI 2 "general_operand" "Int3,bc,dehl")))
   ]
  "rl78_real_insns_ok ()"
  "@
   shr\t%0, %u2
   cmp0 %2\; bz $2f\; 1: shr\t%0, 1 \;dec %2 \;bnz $1b\;2:
   inc %2\;dec %2\;bz $2f\;1: shr\t%0, 1\;dec %2\;bnz $1b\;2:"
)

(define_insn "*lshrhi3_real"
  [(set (match_operand:HI              0 "nonimmediate_operand"  "=AB,A,A")
	(lshiftrt:HI (match_operand:HI 1 "general_operand"  "0,0,0")
		     (match_operand:QI 2 "general_operand" "P,bc,dehl")))
   ]
  "rl78_real_insns_ok ()"
  "@
   shrw\t%0, %u2
   cmp0 %2\; bz $2f\; 1: shrw\t%0, 1 \;dec %2 \;bnz $1b\;2:
   inc %2\;dec %2\;bz $2f\;1: shrw\t%0, 1\;dec %2\;bnz $1b\;2:"
)

;;---------- Branching ------------------------

(define_insn "*indirect_jump_real"
  [(set (pc)
	(match_operand:HI 0 "nonimmediate_operand" "A"))]
  "rl78_real_insns_ok ()"
  "br\t%0"
)

(define_insn "jump"
  [(set (pc)
	(label_ref (match_operand 0 "" "")))]
  ""
  ;; $rel8, $!rel16, !abs16, !!abs20
  "br\t!!%0"
)

(define_insn "*call_real"
  [(call (match_operand:HI 0 "memory_operand" "Wab,Wca")
	 (match_operand 1 "" ""))]
  "rl78_real_insns_ok ()"
  "@
   call\t!!%A0
   call\t%A0"
  )

(define_insn "*call_value_real"
  [(set (match_operand 0 "register_operand" "=v,v")
	(call (match_operand:HI 1 "memory_operand" "Wab,Wca")
	      (match_operand 2 "" "")))]
  "rl78_real_insns_ok ()"
  "@
   call\t!!%A1
   call\t%A1"
  )

(define_insn "*cbranchqi4_real"
  [(set (pc) (if_then_else
	      (match_operator 0 "rl78_cmp_operator_real"
			      [(match_operand:QI 1 "general_operand" "Wabvaxbc,a,          v,bcdehl")
			       (match_operand:QI 2 "general_operand" "M,       irWhlWh1Whb,i,a")])
              (label_ref (match_operand 3 "" ""))
	      (pc)))]
  "rl78_real_insns_ok ()"
  "@
   cmp0\t%1 \;sk%c0 \;br\t!!%3
   cmp\t%1, %2 \;sk%c0 \;br\t!!%3
   cmp\t%1, %2 \;sk%c0 \;br\t!!%3
   cmp\t%1, %2 \;sk%c0 \;br\t!!%3"
  )

(define_insn "*cbranchhi4_real"
  [(set (pc) (if_then_else
	      (match_operator 0 "rl78_cmp_operator_real"
			      [(match_operand:HI 1 "general_operand" "A")
			       (match_operand:HI 2 "general_operand" "iBDTWhlWh1")])
              (label_ref (match_operand 3 "" ""))
	      (pc)))]
  "rl78_real_insns_ok ()"
  "cmpw\t%1, %2 \;sk%c0 \;br\t!!%3"
  )
