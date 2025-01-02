;;  Machine Description for Renesas RL78 processors
;;  Copyright (C) 2011-2025 Free Software Foundation, Inc.
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

;; This attribute reflects how the insn alters the Z flag,
;; based upon the value of the it's output.  The default is NO
;; for no change, but other possibilities are UPDATE_Z if it changes
;; the Z flag and CLOBBER if the state of the flag is indeterminate.
;; The CY and AC flags are not set in the same way as the Z flag, so
;; their values are not tracked.
(define_attr "update_Z" "no,update_Z,clobber" (const_string "no"))

;;---------- Moving ------------------------

(define_insn "movqi_to_es"
  [(set (reg:QI ES_REG)
	(match_operand:QI 0 "register_operand" "a"))]
  ""
  "mov\tes, %0"
)

(define_insn "movqi_from_es"
  [(set (match_operand:QI 0 "register_operand" "=a")
	(reg:QI ES_REG))]
  ""
  "mov\t%0, es"
)

(define_insn "movqi_cs"
  [(set (reg:QI CS_REG)
	(match_operand:QI 0 "register_operand" "a"))]
  ""
  "mov\tcs, %0"
)

(define_insn "*movqi_real"
  [(set (match_operand:QI 0 "rl78_nonimmediate_operand" "=Rv,RaxbcWab,RaxbcWab,a,                               bcx,R,     WabWd2WhlWh1WhbWbcWs1v, bcx,WsaWsf")
	(match_operand    1 "rl78_general_operand"      "0,K,        M,       RInt8sJvWabWdeWd2WhlWh1WhbWbcWs1,Wab,aInt8J,a,                      R,  i"))]
  "rl78_real_insns_ok ()"
  "@
   ; mov\t%0, %1
   oneb\t%0
   clrb\t%0
   mov\t%0, %1
   mov\t%0, %1
   mov\t%0, %1
   mov\t%0, %1
   mov\t%0, %S1
   mov\t%0, %1"
)

(define_insn "*movhi_real"
  [(set (match_operand:HI 0 "rl78_nonimmediate_operand" "=Rv,AB,AB,RSv,A,BDTvSWabWd2WdeWhlWh1WbcWs1, BDT,ABDT,v")
	(match_operand:HI 1 "rl78_general_operand"      " 0,K, M, i,  BDTvSWabWd2WdeWh1WhlWbcWs1,A, BDT,vS,  ABDT"))]
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

(define_insn "*bswaphi2_real"
  [(set (match_operand:HI           0 "rl78_nonfar_nonimm_operand" "=A,A")
        (bswap:HI (match_operand:HI 1 "general_operand"  "0,viU")))]
  "rl78_real_insns_ok ()"
  "@
   xch\ta, x
   movw\tax, %1\n\txch\ta, x"
)

;;---------- Conversions ------------------------

(define_insn "*zero_extendqihi2_real"
  [(set (match_operand:HI                 0 "nonimmediate_operand" "=Rv,A")
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
  [(set (match_operand:QI          0 "rl78_nonimmediate_operand"  "=RvWabWhlWh1Wsa,RvWabWhlWh1Wsa,a,*bcdehl,Wsa")
	(plus:QI (match_operand:QI 1 "rl78_general_operand"  "%0,0,0,0,0")
		 (match_operand:QI 2 "rl78_general_operand" "K,L,RWhlWh1Wabi,a,i")))
   ]
  "rl78_real_insns_ok ()"
  "@
    inc\t%p0
    dec\t%p0
    add\t%0, %2
    add\t%0, %2
    add\t%0, %2"
  [(set (attr "update_Z") (const_string "update_Z"))]
)

(define_insn "*addhi3_real"
  [(set (match_operand:HI          0 "rl78_nonimmediate_operand"  "=vABDTWhlWh1WabWsa,vABDTWhlWh1WabWsa,v,v,A,S,S,A")
	(plus:HI (match_operand:HI 1 "rl78_general_operand"  "%0,0,0,0,0,0,0,S")
		 (match_operand:HI 2 "" "K,L,N,O,RWh1WhlWabiv,Int8Qs8,J,Ri")))
   ]
  "rl78_real_insns_ok ()"
  "@
   incw\t%p0
   decw\t%p0
   incw\t%0 \;incw\t%0
   decw\t%0 \;decw\t%0
   addw\t%0, %p2
   addw\t%0, %2
   subw\t%0, %m2
   movw\t%0, %1 \;addw\t%0, %2"
  [(set_attr "update_Z" "*,*,*,*,update_Z,update_Z,update_Z,update_Z")]
)

(define_insn "*addqihi3a_real"
  [(set (match_operand:HI                          0 "register_operand" "=R")
	(plus:HI (zero_extend:HI (match_operand:QI 1 "register_operand"  "R"))
		 (match_operand:HI                 2 "register_operand"  "0")))
   ]
  "rl78_real_insns_ok ()"
  "add\t%q0, %q1 \;addc\t%Q0, #0"
  [(set (attr "update_Z") (const_string "update_Z"))]
)

(define_insn "*subqi3_real"
  [(set (match_operand:QI           0 "nonimmediate_operand"  "=a,R,v")
	(minus:QI (match_operand:QI 1 "general_operand"  "0,0,0")
		  (match_operand:QI 2 "rl78_general_operand" "RiWabWhbWh1Whl,a,i")))
   ]
  "rl78_real_insns_ok ()"
  "sub\t%0, %2"
  [(set (attr "update_Z") (const_string "update_Z"))]
)

(define_insn "*subhi3_real"
  [(set (match_operand:HI           0 "nonimmediate_operand"  "=A,S")
	(minus:HI (match_operand:HI 1 "general_operand"  "0,0")
		  (match_operand:HI 2 "rl78_general_operand" "iBDTWabWh1v,i")))
   ]
  "rl78_real_insns_ok ()"
  "subw\t%0, %2"
  [(set (attr "update_Z") (const_string "update_Z"))]
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
  [(set (match_operand:QI         0 "rl78_nonimmediate_operand"  "=WsfWsaWhlWab,A,R,vWsa")
	(and:QI (match_operand:QI 1 "rl78_general_operand"       "%0,0,0,0")
		(match_operand:QI 2 "rl78_general_operand"       "IBqi,iRvWabWhbWh1Whl,A,i")))
   ]
  "rl78_real_insns_ok ()"
  "@
   clr1\t%0.%B2
   and\t%0, %2
   and\t%0, %2
   and\t%0, %2"
  [(set_attr "update_Z" "*,update_Z,update_Z,update_Z")]
)

(define_insn "*iorqi3_real"
  [(set (match_operand:QI         0 "rl78_nonimmediate_operand"  "=WsfWsaWhlWab,A,R,vWsa")
	(ior:QI (match_operand:QI 1 "rl78_general_operand"       "%0,0,0,0")
		(match_operand:QI 2 "rl78_general_operand"       "Ibqi,iRvWabWhbWh1Whl,A,i")))
   ]
  "rl78_real_insns_ok ()"
  "@
   set1\t%0.%B2
   or\t%0, %2
   or\t%0, %2
   or\t%0, %2"
  [(set_attr "update_Z" "*,update_Z,update_Z,update_Z")]
)

(define_insn "*xorqi3_real"
  [(set (match_operand:QI         0 "rl78_nonimmediate_operand"  "=A,R,vWsa")
	(xor:QI (match_operand:QI 1 "rl78_general_operand"       "%0,0,0")
		(match_operand    2 "rl78_general_operand"       "iRvWabWhbWh1Whl,A,i")))
   ]
  "rl78_real_insns_ok ()"
  "xor\t%0, %2"
  [(set (attr "update_Z") (const_string "update_Z"))]
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
  [(set_attr "update_Z" "*,clobber,clobber")]
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
  [(set_attr "update_Z" "*,clobber,clobber")]
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
  [(set_attr "update_Z" "*,clobber,clobber")]
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
  [(set_attr "update_Z" "*,clobber,clobber")]
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
  [(set_attr "update_Z" "*,clobber,clobber")]
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
  [(set_attr "update_Z" "*,clobber,clobber")]
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
  [(set (attr "update_Z") (const_string "clobber"))]
  )

;; Peephole to match:
;;
;;	(set (reg1) (reg2))
;;	(call (mem (reg1)))
;;
;;  and replace it with:
;;
;;	(call (mem (reg2)))

(define_peephole2
  [(set (match_operand:HI 0 "register_operand") (match_operand:HI 1 "register_operand"))
   (call (mem:HI (match_dup 0))(const_int 0))
  ]
  "peep2_regno_dead_p (2, REGNO (operands[0]))
   && REGNO (operands[1]) < 8"
  [(call (mem:HI (match_dup 1))(const_int 0))
  ]
)

(define_insn "*call_value_real"
  [(set (match_operand 0 "register_operand" "=v,v")
	(call (match_operand:HI 1 "memory_operand" "Wab,Wca")
	      (match_operand 2 "" "")))]
  "rl78_real_insns_ok ()"
  "@
   call\t!!%A1
   call\t%A1"
  [(set (attr "update_Z") (const_string "clobber"))]
  )

;; Peephole to match:
;;
;;	(set (reg1) (reg2))
;;	(set (reg3) (call (mem (reg1))))
;;
;;  and replace it with:
;;
;;	(set (reg3) (call (mem (reg2))))

(define_peephole2
  [(set (match_operand:HI 0 "register_operand") (match_operand:HI 1 "register_operand"))
   (set (match_operand:HI 2 "register_operand") (call (mem:HI (match_dup 0))(const_int 0)))
  ]
  "peep2_regno_dead_p (2, REGNO (operands[0]))
   && REGNO (operands[1]) < 8"
  [(set (match_dup 2) (call (mem:HI (match_dup 1))(const_int 0)))
  ]
)

(define_insn "*cbranchqi4_real_signed"
  [(set (pc) (if_then_else
	      (match_operator 0 "rl78_cmp_operator_signed"
			      [(match_operand:QI 1 "general_operand" "A,A,A,A,Wsa")
			       (match_operand:QI 2 "general_operand" "M,ISqi,i,v,i")])
              (label_ref (match_operand 3 "" ""))
	      (pc)))]
  "rl78_real_insns_ok ()"
  {
    gcc_assert (GET_CODE (operands[0]) != EQ && GET_CODE (operands[0]) != NE);

    switch (which_alternative)
    {
    case 0: return "cmp0\t%1\; xor1\tCY, %1.7\; sk%C0\; br\t!!%3";
    case 1: return "cmp\t%1, %2\; xor1\tCY, %1.7\; not1\tCY\; sk%C0\; br\t!!%3";
    case 4:
    case 2: return "cmp\t%1, %2\; xor1\tCY, %1.7\; sk%C0\; br\t!!%3";
    case 3: return "cmp\t%1, %2\; xor1\tCY, %1.7\; xor1\tCY, %2.7\; sk%C0\; br\t!!%3";
    default: gcc_unreachable ();
    }
  }   
  [(set (attr "update_Z") (const_string "clobber"))] ;; FIXME: flags are set based on %1 vs %2
  )

(define_insn "*cbranchqi4_real"
  [(set (pc) (if_then_else
	      (match_operator 0 "rl78_cmp_operator_real"
			      [(match_operand:QI 1 "rl78_general_operand" "Wabvaxbc,a,              vWsaWab,bcdehl")
			       (match_operand:QI 2 "rl78_general_operand" "M,       iRvWabWhlWh1Whb,i,a")])
              (label_ref (match_operand 3 "" ""))
	      (pc)))]
  "rl78_real_insns_ok ()"
  {
    if (which_alternative == 0)
      {
        if (rl78_flags_already_set (operands[0], operands[1]))
          return "sk%C0\; br\t!!%3\; # zero-comparison eliminated";
	else
	  return "cmp0\t%1\; sk%C0\; br\t!!%3";
      }
    return "cmp\t%1, %2\; sk%C0\; br\t!!%3";
  }
  [(set (attr "update_Z") (const_string "clobber"))] ;; FIXME: alt 0: flags are set based on %1 vs %2
  )

(define_insn "*cbranchhi4_real_signed"
  [(set (pc) (if_then_else
	      (match_operator 0 "rl78_cmp_operator_signed"
			      [(match_operand:HI 1 "general_operand" "A,A,A,vR")
			       (match_operand:HI 2 "general_operand" "IShi,i,v,1")])
              (label_ref (match_operand 3))
	      (pc)))]
  "rl78_real_insns_ok ()"
  "@
   cmpw\t%1, %2\; xor1\tCY, %Q1.7\; not1\tCY\; sk%C0\; br\t!!%3
   cmpw\t%1, %2\; xor1\tCY, %Q1.7\; sk%C0\; br\t!!%3
   cmpw\t%1, %2\; xor1\tCY, %Q1.7\; xor1\tCY, %Q2.7\; sk%C0\; br\t!!%3
   %z0\t!!%3"
  [(set_attr "update_Z" "clobber,clobber,clobber,*")]
  )

(define_insn "cbranchhi4_real"
  [(set (pc) (if_then_else
	      (match_operator                    0 "rl78_cmp_operator_real"
			      [(match_operand:HI 1 "general_operand" "A,A,vR")
			       (match_operand:HI 2 "rl78_general_operand" "M,iBDTvWabWhlWh1,1")])
              (label_ref (match_operand          3 "" ""))
	      (pc)))]
  "rl78_real_insns_ok ()"
  {
    switch (which_alternative)
      {
      case 0:
        if (rl78_flags_already_set (operands[0], operands[1]))
	  return "sk%C0\; br\t!!%3\; # cmpw eliminated";
	/* else fall through.  */
      case 1:
	return "cmpw\t%1, %2\; sk%C0\; br\t!!%3";
      case 2:
        return "%z0\t!!%3";
      default:
        gcc_unreachable ();
      }
  }
  [(set (attr "update_Z") (const_string "clobber"))] ;; FIXME: Z might be set based on %1 vs %2
  )

(define_insn "cbranchhi4_real_inverted"  
  [(set (pc) (if_then_else
	      (match_operator                    0 "rl78_cmp_operator_real"
			      [(match_operand:HI 1 "general_operand" "A,A")
			       (match_operand:HI 2 "rl78_general_operand" "M,iBDTvWabWhlWh1")])
	      (pc)
              (label_ref (match_operand          3 "" ""))))]
  "rl78_real_insns_ok ()"
  {
    if (which_alternative == 0 && rl78_flags_already_set (operands[0], operands[1]))
      return "sk%C0\; br\t!!%3\; # inverted cmpw eliminated";
    else
      return "cmpw\t%1, %2\; sk%C0\; br\t!!%3";
  }
  [(set (attr "update_Z") (const_string "clobber"))] ;; FIXME: flags are set based on %1 vs %2
  )

(define_insn "*cbranchsi4_real_lt"
  [(set (pc) (if_then_else
	      (lt (match_operand:SI 0 "rl78_general_operand" "U,vWabWhlWh1")
		  (const_int 0))
              (label_ref (match_operand 1 "" ""))
	      (pc)))
   (clobber (reg:HI AX_REG))
   ]
  "rl78_real_insns_ok ()"
  "@
   mov\ta, %E0\; mov1\tCY, a.7\; sknc\; br\t!!%1
   mov1\tCY, %E0.7\; sknc\; br\t!!%1"
  )

(define_insn "*cbranchsi4_real_ge"
  [(set (pc) (if_then_else
	      (ge (match_operand:SI 0 "rl78_general_operand" "U,vWabWhlWh1")
		  (const_int 0))
              (label_ref (match_operand 1 "" ""))
	      (pc)))
   (clobber (reg:HI AX_REG))
   ]
  "rl78_real_insns_ok ()"
  "@
   mov\ta, %E0\; mov1\tCY, a.7\; skc\; br\t!!%1
   mov1\tCY, %E0.7\; skc\; br\t!!%1"
  )

(define_insn "*cbranchsi4_real_signed"
  [(set (pc) (if_then_else
	      (match_operator 0 "rl78_cmp_operator_signed"
			      [(match_operand:SI 1 "general_operand"   "vU,vU,vU,i,i")
			       (match_operand:SI 2 "nonmemory_operand" "ISsi,i,v,S,v")])
              (label_ref (match_operand 3 "" ""))
	      (pc)))
   (clobber (reg:HI AX_REG))
   ]
  "rl78_real_insns_ok ()"
  "@
   movw\tax, %H1\; cmpw\tax, %H2\; xor1\tCY, a.7\; not1\tCY\; movw\tax, %h1\; sknz\; cmpw\tax, %h2\; sk%C0\; br\t!!%3
   movw\tax, %H1\; cmpw\tax, %H2\; xor1\tCY, a.7\; movw\tax, %h1\; sknz\; cmpw\tax, %h2\; sk%C0\; br\t!!%3
   movw\tax, %H1\; cmpw\tax, %H2\; xor1\tCY, a.7\; xor1\tCY, %E2.7\; movw\tax, %h1\; sknz\; cmpw\tax, %h2\; sk%C0\; br\t!!%3
   movw\tax, %H1\; cmpw\tax, %H2\; xor1\tCY, a.7\; not1\tCY\; movw\tax, %h1\; sknz\; cmpw\tax, %h2\; sk%0\; br\t!!%3
   movw\tax, %H1\; cmpw\tax, %H2\; xor1\tCY, a.7\; movw\tax, %h1\; sknz\; cmpw\tax, %h2\; sk%0\; br\t!!%3"
  [(set (attr "update_Z") (const_string "clobber"))]
  )

(define_insn "*cbranchsi4_real"
  [(set (pc) (if_then_else
	      (match_operator 0 "rl78_cmp_operator_real"
			      [(match_operand:SI 1 "general_operand" "vUi")
			       (match_operand:SI 2 "general_operand" "iWhlWh1v")])
              (label_ref (match_operand 3 "" ""))
	      (pc)))
   (clobber (reg:HI AX_REG))
   ]
  "rl78_real_insns_ok ()"
  "movw\tax, %H1\; cmpw\tax, %H2\; movw\tax, %h1\; sknz\; cmpw\tax, %h2\; sk%C0\; br\t!!%3"
  [(set (attr "update_Z") (const_string "clobber"))]
  )

;; Peephole to match:
;;
;;     (set (mem (sp)) (ax))
;;     (set (ax) (mem (sp)))
;; or:
;;     (set (mem (plus (sp) (const)) (ax))
;;     (set (ax) (mem (plus (sp) (const))))
;;
;; which can be generated as the last instruction of the conversion
;; of one virtual insn into a real insn and the first instruction of
;; the conversion of the following virtual insn.

(define_peephole2
  [(set (match_operand:HI 0 "rl78_stack_based_mem")
	(reg:HI AX_REG))
   (set (reg:HI AX_REG)
	(match_dup 0))]
  ""
  [(set (match_dup 0) (reg:HI AX_REG))]
  )

;; Bit test and branch insns.

;; NOTE: These patterns will work for bits in other places, not just A.

(define_insn "bf"
  [(set (pc)
	(if_then_else (eq (and (reg:QI A_REG)
			       (match_operand 0 "immediate_operand" "n"))
			  (const_int 0))
		      (label_ref (match_operand 1 "" ""))
		      (pc)))]
  ""
  "bt\tA.%B0, $1f\n\tbr !!%1\n\t1:"
  [(set (attr "update_Z") (const_string "clobber"))]
)

(define_insn "bt"
  [(set (pc)
	(if_then_else (ne (and (reg:QI A_REG)
			       (match_operand 0 "immediate_operand" "n"))
			  (const_int 0))
		      (label_ref (match_operand 1 "" ""))
		      (pc)))]
  ""
  "bf\tA.%B0, $1f\n\tbr !!%1\n\t1:"
  [(set (attr "update_Z") (const_string "clobber"))]
)

;; NOTE: These peepholes are fragile.  They rely upon GCC generating
;; a specific sequence on insns, based upon examination of test code.
;; Improvements to GCC or using code other than the test code can result
;; in the peephole not matching and the optimization being missed.

(define_peephole2
  [(set (match_operand:QI 0 "register_operand") (reg:QI A_REG))
   (set (match_dup 0) (and:QI (match_dup 0) (match_operand 1 "immediate_operand")))
   (set (pc) (if_then_else (eq (match_dup 0) (const_int 0))
			   (label_ref (match_operand 2 ""))
			   (pc)))]
  "peep2_regno_dead_p (3, REGNO (operands[0]))
   && exact_log2 (INTVAL (operands[1])) >= 0"
  [(set (pc) (if_then_else (eq (and (reg:QI A_REG) (match_dup 1)) (const_int 0))
			   (label_ref (match_dup 2))
			   (pc)))]
  )

(define_peephole2
  [(set (match_operand:QI 0 "register_operand") (reg:QI A_REG))
   (set (match_dup 0) (and:QI (match_dup 0) (match_operand 1 "immediate_operand")))
   (set (pc) (if_then_else (ne (match_dup 0) (const_int 0))
			   (label_ref (match_operand 2 ""))
			   (pc)))]
  "peep2_regno_dead_p (3, REGNO (operands[0]))
   && exact_log2 (INTVAL (operands[1])) >= 0"
  [(set (pc) (if_then_else (ne (and (reg:QI A_REG) (match_dup 1)) (const_int 0))
			   (label_ref (match_dup 2))
			   (pc)))]
  )

;; Eliminate needless register copies.
(define_peephole2
  [(set (match_operand:HI 0 "register_operand") (match_operand:HI 1 "register_operand"))
   (set (match_operand:HI 2 "register_operand") (match_dup 0))]
  "peep2_regno_dead_p (2, REGNO (operands[0]))
   && (REGNO (operands[1]) < 8 || REGNO (operands[2]) < 8)"
  [(set (match_dup 2) (match_dup 1))]
  )

;; Eliminate needless register copying when performing bit manipulations.
(define_peephole2
  [(set (match_operand:QI 0 "register_operand") (reg:QI A_REG))
   (set (match_dup 0) (ior:QI (match_dup 0) (match_operand 1 "immediate_operand")))
   (set (reg:QI A_REG) (match_dup 0))]
  "peep2_regno_dead_p (3, REGNO (operands[0]))"
  [(set (reg:QI A_REG) (ior:QI (reg:QI A_REG) (match_dup 1)))]
  )

(define_peephole2
  [(set (match_operand:QI 0 "register_operand") (reg:QI A_REG))
   (set (match_dup 0) (xor:QI (match_dup 0) (match_operand 1 "immediate_operand")))
   (set (reg:QI A_REG) (match_dup 0))]
  "peep2_regno_dead_p (3, REGNO (operands[0]))"
  [(set (reg:QI A_REG) (xor:QI (reg:QI A_REG) (match_dup 1)))]
  )

(define_peephole2
  [(set (match_operand:QI 0 "register_operand") (reg:QI A_REG))
   (set (match_dup 0) (and:QI (match_dup 0) (match_operand 1 "immediate_operand")))
   (set (reg:QI A_REG) (match_dup 0))]
  "peep2_regno_dead_p (3, REGNO (operands[0]))"
  [(set (reg:QI A_REG) (and:QI (reg:QI A_REG) (match_dup 1)))]
  )

(define_insn "*negandhi3_real"
  [(set (match_operand:HI                 0 "register_operand"  "=A")
	(and:HI (neg:HI (match_operand:HI 1 "register_operand"  "0"))
		(match_operand:HI         2 "immediate_operand" "n")))
   ]
  "rl78_real_insns_ok ()"
  "xor a, #0xff @ xch a, x @ xor a, #0xff @ xch a, x @ addw ax, #1 @ and a, %Q2 @ xch a, x @ and a, %q2 @ xch a, x"
  [(set (attr "update_Z") (const_string "clobber"))]
)
