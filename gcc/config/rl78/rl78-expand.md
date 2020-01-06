;;  Machine Description for Renesas RL78 processors
;;  Copyright (C) 2011-2020 Free Software Foundation, Inc.
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

;;---------- Moving ------------------------

(define_expand "movqi"
  [(set (match_operand:QI 0 "nonimmediate_operand")
	(match_operand:QI 1 "general_operand"))]
  ""
  {
    if (MEM_P (operands[0]) && MEM_P (operands[1]))
      operands[1] = copy_to_mode_reg (QImode, operands[1]);
    if (rl78_far_p (operands[0]) && rl78_far_p (operands[1]))
      operands[1] = copy_to_mode_reg (QImode, operands[1]);

    /* GCC can generate (SUBREG (SYMBOL_REF)) when it has to store a symbol
       into a bitfield, or a packed ordinary field.  We can handle this
       provided that the destination is a register.  If not, then load the
       source into a register first.  */
    if (GET_CODE (operands[1]) == SUBREG
        && GET_CODE (XEXP (operands[1], 0)) == SYMBOL_REF
	&& ! REG_P (operands[0]))
	operands[1] = copy_to_mode_reg (QImode, operands[1]);

    /* Similarly for (SUBREG (CONST (PLUS (SYMBOL_REF)))).
       cf. g++.dg/abi/packed.C.  */
    if (GET_CODE (operands[1]) == SUBREG
	&& GET_CODE (XEXP (operands[1], 0)) == CONST
        && GET_CODE (XEXP (XEXP (operands[1], 0), 0)) == PLUS
        && GET_CODE (XEXP (XEXP (XEXP (operands[1], 0), 0), 0)) == SYMBOL_REF
	&& ! REG_P (operands[0]))
	operands[1] = copy_to_mode_reg (QImode, operands[1]);

    if (CONST_INT_P (operands[1]) && ! IN_RANGE (INTVAL (operands[1]), (HOST_WIDE_INT_M1U << 8) + 1, (1 << 8) - 1))
      FAIL;
  }
)

(define_expand "movhi"
  [(set (match_operand:HI 0 "nonimmediate_operand")
	(match_operand:HI 1 "general_operand"))]
  ""
  {
    if (MEM_P (operands[0]) && MEM_P (operands[1]))
      operands[1] = copy_to_mode_reg (HImode, operands[1]);
    if (rl78_far_p (operands[0]) && rl78_far_p (operands[1]))
      operands[1] = copy_to_mode_reg (HImode, operands[1]);

    /* FIXME: Not sure how GCC can generate (SUBREG (SYMBOL_REF)),
       but it does.  Since this makes no sense, reject it here.  */
    if (GET_CODE (operands[1]) == SUBREG
        && GET_CODE (XEXP (operands[1], 0)) == SYMBOL_REF)
      FAIL;
    /* Similarly for (SUBREG (CONST (PLUS (SYMBOL_REF)))).  */
    if (GET_CODE (operands[1]) == SUBREG
	&& GET_CODE (XEXP (operands[1], 0)) == CONST
        && GET_CODE (XEXP (XEXP (operands[1], 0), 0)) == PLUS
        && GET_CODE (XEXP (XEXP (XEXP (operands[1], 0), 0), 0)) == SYMBOL_REF)
      FAIL;
  }
)

(define_insn_and_split "movsi"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=vYS,v,Wfr")
	(match_operand:SI 1 "general_operand" "viYS,Wfr,v"))]
  ""
  "#"
  ""
  [(set (match_operand:HI 2 "nonimmediate_operand")
	(match_operand:HI 4 "general_operand"))
   (set (match_operand:HI 3 "nonimmediate_operand")
	(match_operand:HI 5 "general_operand"))]
  "rl78_split_movsi (operands, SImode);"
  [(set_attr "valloc" "op1")]
)

(define_insn_and_split "movsf"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=vYS,v,Wfr")
	(match_operand:SF 1 "general_operand" "viYS,Wfr,v"))]
  ""
  "#"
  ""
  [(set (match_operand:HI 2 "nonimmediate_operand")
	(match_operand:HI 4 "general_operand"))
   (set (match_operand:HI 3 "nonimmediate_operand")
	(match_operand:HI 5 "general_operand"))]
  "rl78_split_movsi (operands, SFmode);"
  [(set_attr "valloc" "op1")]
)

(define_expand "bswaphi2"
  [(set (match_operand:HI           0 "nonimmediate_operand")
        (bswap:HI (match_operand:HI 1 "general_operand")))]
  ""
  "if (rl78_force_nonfar_2 (operands, gen_bswaphi2))
     DONE;"
)

;;---------- Conversions ------------------------

(define_expand "zero_extendqihi2"
  [(set (match_operand:HI                 0 "nonimmediate_operand")
	(zero_extend:HI (match_operand:QI 1 "general_operand")))]
  ""
  "if (rl78_force_nonfar_2 (operands, gen_zero_extendqihi2))
     DONE;"
  )

(define_expand "extendqihi2"
  [(set (match_operand:HI                 0 "nonimmediate_operand")
	(sign_extend:HI (match_operand:QI 1 "general_operand")))]
  ""
  "if (rl78_force_nonfar_2 (operands, gen_extendqihi2))
     DONE;"
  )

;;---------- Arithmetic ------------------------

(define_expand "add<mode>3"
  [(set (match_operand:QHI           0 "nonimmediate_operand")
	(plus:QHI (match_operand:QHI 1 "general_operand")
		  (match_operand:QHI 2 "general_operand")))
   ]
  ""
  "if (rl78_force_nonfar_3 (operands, gen_add<mode>3))
     DONE;"
)

(define_expand "sub<mode>3"
  [(set (match_operand:QHI            0 "nonimmediate_operand")
	(minus:QHI (match_operand:QHI 1 "general_operand")
		   (match_operand:QHI 2 "general_operand")))
   ]
  ""
  "if (rl78_force_nonfar_3 (operands, gen_sub<mode>3))
     DONE;"
)

(define_expand "neg<mode>2"
  [(set (match_operand:QHI            0 "nonimmediate_operand")
	(minus:QHI (const_int 0)
		   (match_operand:QHI 1 "general_operand")))
   ]
  ""
  "if (rl78_force_nonfar_2 (operands, gen_neg<mode>2))
     DONE;"
)

(define_expand "umulqihi3"
  [(set (match_operand:HI 0 "register_operand")
        (mult:HI (zero_extend:HI (match_operand:QI 1 "register_operand"))
                 (zero_extend:HI (match_operand:QI 2 "register_operand"))))]
  ""
  ""
)

(define_expand "andqi3"
  [(set (match_operand:QI         0 "rl78_nonimmediate_operand")
	(and:QI (match_operand:QI 1 "rl78_general_operand")
		(match_operand:QI 2 "rl78_general_operand")))
   ]
  ""
  "if (rl78_force_nonfar_3 (operands, gen_andqi3))
     DONE;"
)

(define_expand "iorqi3"
  [(set (match_operand:QI         0 "rl78_nonimmediate_operand")
	(ior:QI (match_operand:QI 1 "rl78_general_operand")
		(match_operand:QI 2 "rl78_general_operand")))
   ]
  ""
  "if (rl78_force_nonfar_3 (operands, gen_iorqi3))
     DONE;"
)

(define_expand "xorqi3"
  [(set (match_operand:QI         0 "rl78_nonimmediate_operand")
	(xor:QI (match_operand:QI 1 "rl78_general_operand")
		(match_operand:QI 2 "rl78_general_operand")))
   ]
  ""
  "if (rl78_force_nonfar_3 (operands, gen_xorqi3))
     DONE;"
)

(define_expand "one_cmplqi2"
  [(set (match_operand:QI         0 "nonimmediate_operand")
	(xor:QI (match_operand:QI 1 "general_operand")
		(const_int -1)))
   ]
  ""
  "if (rl78_force_nonfar_2 (operands, gen_one_cmplqi2))
     DONE;"
)

;;---------- Shifts ------------------------

(define_expand "ashl<mode>3"
  [(set (match_operand:QHI             0 "nonimmediate_operand")
	(ashift:QHI (match_operand:QHI 1 "general_operand")
		    (match_operand:QI  2 "general_operand")))
   ]
  ""
  "if (rl78_force_nonfar_3 (operands, gen_ashl<mode>3))
     DONE;"
)

(define_expand "ashr<mode>3"
  [(set (match_operand:QHI               0 "nonimmediate_operand")
	(ashiftrt:QHI (match_operand:QHI 1 "general_operand")
		      (match_operand:QI  2 "general_operand")))
   ]
  ""
  "if (rl78_force_nonfar_3 (operands, gen_ashr<mode>3))
     DONE;"
)

(define_expand "lshr<mode>3"
  [(set (match_operand:QHI               0 "nonimmediate_operand")
	(lshiftrt:QHI (match_operand:QHI 1 "general_operand")
		      (match_operand:QI  2 "general_operand")))
   ]
  ""
  "if (rl78_force_nonfar_3 (operands, gen_lshr<mode>3))
     DONE;"
)

(define_expand "ashrsi3"
  [(parallel [(set (match_operand:SI               0 "nonimmediate_operand")
		   (ashiftrt:SI (match_operand:SI  1 "nonimmediate_operand")
				(match_operand:SI  2 "nonmemory_operand")))
	      (clobber (reg:HI X_REG))])
   ]
  ""
  ""
)

(define_expand "lshrsi3"
  [(parallel [(set (match_operand:SI               0 "nonimmediate_operand")
		   (lshiftrt:SI (match_operand:SI  1 "nonimmediate_operand")
				(match_operand:SI  2 "nonmemory_operand")))
	      (clobber (reg:HI X_REG))])
   ]
  ""
  ""
)

(define_expand "ashlsi3"
  [(parallel [(set (match_operand:SI            0 "nonimmediate_operand")
		   (ashift:SI (match_operand:SI 1 "nonimmediate_operand")
			      (match_operand:SI 2 "nonmemory_operand")))
	      (clobber (reg:HI X_REG))])
   ]
  ""
  ""
)

;;---------- Branching ------------------------

(define_expand "indirect_jump"
  [(set (pc)
	(match_operand:HI 0 "nonimmediate_operand"))]
  ""
  ""
)

(define_expand "call"
  [(call (match_operand:HI 0 "memory_operand")
	 (match_operand 1 ""))]
  ""
  ""
)

(define_expand "call_value"
  [(set (match_operand          0 "register_operand")
	(call (match_operand:HI 1 "memory_operand")
	      (match_operand    2 "")))]
  ""
  ""
)

(define_expand "cbranchqi4"
  [(set (pc) (if_then_else
	      (match_operator                    0 "rl78_cmp_operator"
			      [(match_operand:QI 1 "general_operand")
			       (match_operand:QI 2 "general_operand")])
              (label_ref (match_operand 3 "" ""))
	      (pc)))]
  ""
  "rl78_expand_compare (operands);"
)

(define_expand "cbranchhi4"
  [(set (pc) (if_then_else
	      (match_operator                    0 "rl78_cmp_operator"
			      [(match_operand:HI 1 "general_operand")
			       (match_operand:HI 2 "general_operand")])
              (label_ref (match_operand 3 "" ""))
	      (pc)))]
  ""
  "rl78_expand_compare (operands);"
)

(define_expand "cbranchsi4"
  [(parallel [(set (pc) (if_then_else
			 (match_operator 0 "rl78_cmp_operator"
					 [(match_operand:SI 1 "general_operand")
					  (match_operand:SI 2 "nonmemory_operand")])
			 (label_ref (match_operand 3 "" ""))
			 (pc)))
	      (clobber (reg:HI AX_REG))
	      ])]
  "1"
  "rl78_expand_compare (operands);"
)
