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

;; In this MD file, we define those insn patterns that involve
;; registers, where such registers are virtual until allocated to a
;; physical register.  All of these insns need to be conditional on
;; rl78_virt_insns_ok () being true.

;; This tells the physical register allocator what method to use to
;; allocate registers.  Basically, this defines the template of the
;; instruction - op1 is of the form "a = op(b)", op2 is "a = b op c"
;; etc.

(define_attr "valloc" "op1,op2,ro1,cmp,umul,macax"
  (const_string "op2"))

;;---------- Moving ------------------------

(define_insn "*movqi_virt"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=vY,v,Wfr")
	(match_operand    1 "general_operand" "vInt8JY,Wfr,vInt8J"))]
  "rl78_virt_insns_ok ()"
  "v.mov %0, %1"
  [(set_attr "valloc" "op1")]
)

(define_insn "*movhi_virt"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=vYS,v,Wfr")
	(match_operand:HI 1 "general_operand" "viYS,Wfr,v"))]
  "rl78_virt_insns_ok ()"
  "v.movw %0, %1"
  [(set_attr "valloc" "op1")]
)

;;---------- Conversions ------------------------

(define_insn "*zero_extendqihi2_virt"
  [(set (match_operand:HI                 0 "rl78_nonfar_nonimm_operand" "=vm")
	(zero_extend:HI (match_operand:QI 1 "general_operand" "vim")))]
  "rl78_virt_insns_ok ()"
  "v.zero_extend\t%0, %1"
  [(set_attr "valloc" "op1")]
  )

(define_insn "*extendqihi2_virt"
  [(set (match_operand:HI                 0 "rl78_nonfar_nonimm_operand" "=vm")
	(sign_extend:HI (match_operand:QI 1 "general_operand" "vim")))]
  "rl78_virt_insns_ok ()"
  "v.sign_extend\t%0, %1"
  [(set_attr "valloc" "op1")]
  )

;;---------- Arithmetic ------------------------

(define_insn "*add<mode>3_virt"
  [(set (match_operand:QHI           0 "rl78_nonfar_nonimm_operand" "=vY,S")
	(plus:QHI (match_operand:QHI 1 "rl78_nonfar_operand" "viY,0")
		  (match_operand:QHI 2 "general_operand" "vim,i")))
   ]
  "rl78_virt_insns_ok ()"
  "v.add\t%0, %1, %2"
)

(define_insn "*sub<mode>3_virt"
  [(set (match_operand:QHI            0 "rl78_nonfar_nonimm_operand" "=vm,S")
	(minus:QHI (match_operand:QHI 1 "rl78_nonfar_operand" "vim,0")
		   (match_operand:QHI 2 "general_operand" "vim,i")))
   ]
  "rl78_virt_insns_ok ()"
  "v.sub\t%0, %1, %2"
)

(define_insn "*umulhi3_shift_virt"
  [(set (match_operand:HI 0 "register_operand" "=vm")
        (mult:HI (match_operand:HI 1 "rl78_nonfar_operand" "%vim")
                 (match_operand:HI 2 "rl78_24_operand" "Ni")))]
  "rl78_virt_insns_ok ()"
  "v.mulu\t%0, %1, %2"
  [(set_attr "valloc" "umul")]
)

(define_insn "*umulqihi3_virt"
  [(set (match_operand:HI 0 "register_operand" "=vm")
        (mult:HI (zero_extend:HI (match_operand:QI 1 "rl78_nonfar_operand" "%vim"))
                 (zero_extend:HI (match_operand:QI 2 "general_operand" "vim"))))]
  "rl78_virt_insns_ok ()"
  "v.mulu\t%0, %2"
  [(set_attr "valloc" "umul")]
)

(define_insn "*andqi3_virt"
  [(set (match_operand:QI         0 "rl78_nonfar_nonimm_operand" "=vm")
	(and:QI (match_operand:QI 1 "rl78_nonfar_operand" "vim")
		(match_operand:QI 2 "general_operand" "vim")))
   ]
  "rl78_virt_insns_ok ()"
  "v.and\t%0, %1, %2"
)

(define_insn "*iorqi3_virt"
  [(set (match_operand:QI         0 "rl78_nonfar_nonimm_operand" "=vm")
	(ior:QI (match_operand:QI 1 "rl78_nonfar_operand" "vim")
		(match_operand:QI 2 "general_operand" "vim")))
   ]
  "rl78_virt_insns_ok ()"
  "v.or\t%0, %1, %2"
)

(define_insn "*xor3_virt"
  [(set (match_operand:QI         0 "rl78_nonfar_nonimm_operand" "=v,vm,m")
	(xor:QI (match_operand:QI 1 "rl78_nonfar_operand" "%0,vm,vm")
		(match_operand    2 "general_operand" "i,vm,vim")))
   ]
  "rl78_virt_insns_ok ()"
  "v.xor\t%0, %1, %2"
)

;;---------- Shifts ------------------------

(define_insn "*ashl<mode>3_virt"
  [(set (match_operand:QHI             0 "rl78_nonfar_nonimm_operand" "=vm")
	(ashift:QHI (match_operand:QHI 1 "rl78_nonfar_operand" "vim")
		    (match_operand:QI  2 "general_operand" "vim")))
   ]
  "rl78_virt_insns_ok ()"
  "v.shl\t%0, %1, %2"
)

(define_insn "*ashr<mode>3_virt"
  [(set (match_operand:QHI               0 "rl78_nonfar_nonimm_operand" "=vm")
	(ashiftrt:QHI (match_operand:QHI 1 "rl78_nonfar_operand" "vim")
		      (match_operand:QI  2 "general_operand" "vim")))
   ]
  "rl78_virt_insns_ok ()"
  "v.sar\t%0, %1, %2"
)

(define_insn "*lshr<mode>3_virt"
  [(set (match_operand:QHI               0 "rl78_nonfar_nonimm_operand" "=vm")
	(lshiftrt:QHI (match_operand:QHI 1 "rl78_nonfar_operand" "vim")
		      (match_operand:QI  2 "general_operand" "vim")))
   ]
  "rl78_virt_insns_ok ()"
  "v.shr\t%0, %1, %2"
)

;; really a macro
(define_insn "*ashrsi3_virt"
  [(set (match_operand:SI               0 "register_operand" "=v,v,v")
	(ashiftrt:SI (match_operand:SI  1 "register_operand" "0,v,0")
		      (match_operand:SI 2 "immediate_operand" "M,K,i")))
   ]
  ""
  "@
   ; ashrsi %0, 0
   movw\tax,%H1\;sarw\tax,1\;movw\t%H0,ax\;mov\ta,%Q1\;rorc\ta,1\;mov\t%Q0,a\;mov\ta,%q1\;rorc\ta,1\;mov\t%q0,a
   mov\tb,%2\;1:\;movw\tax,%H1\;sarw\tax,1\;movw\t%H0,ax\;mov\ta,%Q1\;rorc\ta,1\;mov\t%Q0,a\;mov\ta,%q1\;rorc\ta,1\;mov\t%q0,a\;dec\tb\;bnz $1b"
  [(set_attr "valloc" "macax")]
)

;;---------- Branching ------------------------

(define_insn "*indirect_jump_virt"
  [(set (pc)
	(match_operand:HI 0 "nonimmediate_operand" "vm"))]
  "rl78_virt_insns_ok ()"
  "v.br\t%0"
  [(set_attr "valloc" "ro1")]
)

(define_insn "*call_virt"
  [(call (match_operand:HI 0 "memory_operand" "Wab,Wcv")
	 (match_operand 1 "" ""))]
  "rl78_virt_insns_ok ()"
  "v.call\t%0"
  [(set_attr "valloc" "ro1")]
  )

(define_insn "*call_value_virt"
  [(set (match_operand 0 "register_operand" "=v,v")
	(call (match_operand:HI 1 "memory_operand" "Wab,Wcv")
	      (match_operand 2 "" "")))]
  "rl78_virt_insns_ok ()"
  "v.call\t%1"
  [(set_attr "valloc" "op1")]
  )

(define_insn "*cbranchqi4_virt"
  [(set (pc) (if_then_else
	      (match_operator 0 "rl78_cmp_operator_real"
			      [(match_operand:QI 1 "general_operand" "vim")
			       (match_operand:QI 2 "general_operand" "vim")])
              (label_ref (match_operand 3 "" ""))
	      (pc)))]
  "rl78_virt_insns_ok ()"
  "v.cmp\t%1, %2\\n\tv.b%c0\t%3"
  [(set_attr "valloc" "cmp")]
  )

(define_insn "*cbranchhi4_virt"
  [(set (pc) (if_then_else
	      (match_operator 0 "rl78_cmp_operator_real"
			      [(match_operand:HI 1 "general_operand" "vim")
			       (match_operand:HI 2 "general_operand" "vim")])
              (label_ref (match_operand 3 "" ""))
	      (pc)))]
  "rl78_virt_insns_ok ()"
  "v.cmpw\t%1, %2\\n\tv.b%c0\t%3"
  [(set_attr "valloc" "cmp")]
  )

;;---------- Peepholes ------------------------

(define_peephole2
  [(set (match_operand:QI 0 "" "")
	(match_operand:QI 1 "" ""))
   (set (match_operand:QI 2 "" "")
	(match_operand:QI 3 "" ""))]
  "rl78_peep_movhi_p (operands)"
  [(set (match_dup 4)
	(match_dup 5))]
  "rl78_setup_peep_movhi (operands);"
  )

(define_peephole2
  [(set (reg:QI A_REG)
	(match_operand:QI 1 "" ""))
   (set (match_operand:QI 0 "" "")
	(reg:QI A_REG))
   (set (reg:QI A_REG)
	(match_operand:QI 3 "" ""))
   (set (match_operand:QI 2 "" "")
	(reg:QI A_REG))
   ]
  "rl78_peep_movhi_p (operands)"
  [(set (reg:HI AX_REG)
	(match_dup 5))
   (set (match_dup 4)
	(reg:HI AX_REG))
   ]
  "rl78_setup_peep_movhi (operands);"
  )
