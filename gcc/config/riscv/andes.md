;; Machine description for Andes vendor extensions
;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

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

(define_c_enum "unspec" [
  ;; XANDESPERF string
  UNSPEC_NDS_FFB
  UNSPEC_NDS_FFZMISM
  UNSPEC_NDS_FFMISM
  UNSPEC_NDS_FLMISM
])

;; AndesPerf
;;  ....................
;;
;;      BRANCH OPERATION
;;
;;  ....................
;;

(define_insn "*nds_branch_imms7<mode>"
  [(set (pc)
	(if_then_else (any_eq (match_operand:X 1 "register_operand"        "r")
			      (match_operand:X 2 "ads_branch_bimm_operand" "Ou07"))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  "TARGET_XANDESPERF"
{
  if (get_attr_length (insn) == 12)
    return "nds.b<optab>c\t%1,%2,1f; jump\t%l0,ra; 1:";

  return "nds.b<optab>c\t%1,%2,%l0";
}
  [(set_attr "type" "branch")
   (set_attr "mode" "none")])

(define_insn "*nds_branch_on_bit<X:mode>"
  [(set (pc)
	(if_then_else (any_eq (zero_extract:X (match_operand:X 1 "register_operand" "r")
			      (const_int 1)
			      (match_operand 2 "ads_branch_bbcs_operand"))
		      (const_int 0))
		      (label_ref (match_operand 0))
		      (pc)))
	(clobber (match_scratch:X 3 "=&r"))]
   "TARGET_XANDESPERF"
{
  if (get_attr_length (insn) == 12)
    return "nds.bb<any_eq:cs>\t%1,%2,%1f; jump\t%l0,ra; 1:";

  return "nds.bb<any_eq:cs>\t%1,%2,%l0";
}
  [(set_attr "type" "branch")
   (set_attr "mode" "none")])

;;
;;  ....................
;;
;;      EXTENSION OPERATION
;;
;;  ....................
;;

(define_insn "*nds_zero_extendsidi2_internal"
  [(set (match_operand:DI     0 "register_operand"     "=r,r")
	(zero_extend:DI
	    (match_operand:SI 1 "nonimmediate_operand" " r,m")))]
  "TARGET_64BIT && TARGET_XANDESPERF"
  "@
   nds.bfoz\t%0,%1,31,0
   lwu\t%0,%1"
  [(set_attr "move_type" "arith,load")
   (set_attr "type" "arith,load")
   (set_attr "mode" "DI")])

(define_insn "*nds_zero_extendhi<GPR:mode>2"
  [(set (match_operand:GPR    0 "register_operand"     "=r,r")
	(zero_extend:GPR
	  (match_operand:HI 1 "nonimmediate_operand" " r,m")))]
  "TARGET_XANDESPERF"
  "@
   nds.bfoz\t%0,%1,15,0
   lhu\t%0,%1"
  [(set_attr "move_type" "arith,load")
   (set_attr "type" "arith,load")
   (set_attr "mode" "<GPR:MODE>, HI")])

(define_insn "*nds_extendsidi2_internal"
  [(set (match_operand:DI     0 "register_operand"     "=r,r")
	(sign_extend:DI
	  (match_operand:SI 1 "nonimmediate_operand" " r,m")))]
  "TARGET_64BIT && TARGET_XANDESPERF"
  "@
   nds.bfos\t%0,%1,31,0
   lw\t%0,%1"
  [(set_attr "move_type" "arith,load")
   (set_attr "type" "arith,load")
   (set_attr "mode" "DI")])

(define_insn "*nds_extend<SHORT:mode><SUPERQI:mode>2"
  [(set (match_operand:SUPERQI   0 "register_operand"     "=r,r")
	(sign_extend:SUPERQI
	  (match_operand:SHORT 1 "nonimmediate_operand" " r,m")))]
  "TARGET_XANDESPERF"
  "@
   nds.bfos\t%0,%1,<SHORT:sh_limit>,0
   l<SHORT:size>\t%0,%1"
  [(set_attr "move_type" "arith,load")
   (set_attr "type" "arith,load")
   (set_attr "mode" "<SHORT:MODE>")])

;;
;;  ....................
;;
;;      BIT FIELD OPERATION
;;
;;  ....................
;;

;; BFO[SZ]: msb >= lsb: Extract sequence tail bits.
(define_insn "*nds_bfo_<sz>extra<mode>4"
  [(set (match_operand:GPR 0 "register_operand"                          "=r")
	(any_extract:GPR (match_operand:GPR 1 "register_operand"         " r")
			  (match_operand 2 "ads_extract_size_imm_<mode>" " n")
			  (match_operand 3 "const_int<sh_bit>_operand"   " n")))]
  "TARGET_XANDESPERF
   && IN_RANGE (INTVAL (operands[2]) + INTVAL (operands[3]),
		1, GET_MODE_BITSIZE (<MODE>mode))"
  {
    operands[2] = GEN_INT (INTVAL (operands[2]) + INTVAL (operands[3]) - 1);
    return "nds.bfo<sz>\t%0,%1,%2,%3";
  }
  [(set_attr "type" "shift")
   (set_attr "mode" "<GPR:MODE>")])

;; BFOZ: msb >= lsb: Mask sequence bits.
(define_insn "*nds_bfoz_and<mode>3"
  [(set (match_operand:GPR 0 "register_operand"          "=r")
	(and:GPR (match_operand:GPR 1 "register_operand" " r")
		 (match_operand:GPR 2 "ads_imm_extract_operand"  " ads_Bext")))]
  "TARGET_XANDESPERF"
  {
    operands[2] = GEN_INT (__builtin_popcountll (INTVAL (operands[2])) - 1);
    return "nds.bfoz\t%0,%1,%2,0";
  }
  [(set_attr "type" "shift")
   (set_attr "mode" "<GPR:MODE>")])

;; BFOZ: msb >= lsb: Extract sequence bits.
(define_insn "*nds_zero_extend<GPR:mode>_lshr<SHORT:mode>"
  [(set (match_operand:GPR 0 "register_operand"                                    "=r")
	(zero_extend:GPR (lshiftrt:SHORT (match_operand:SHORT 1 "register_operand" " r")
			 (match_operand 2 "const_int_operand"                      " n"))))]
  "TARGET_XANDESPERF
   && UINTVAL (operands[2]) < GET_MODE_BITSIZE (<SHORT:MODE>mode)"
  "nds.bfoz\t%0, %1, <SHORT:sh_limit>, %2"
  [(set_attr "type" "shift")
   (set_attr "mode" "<GPR:MODE>")])

;; BFOS: msb >= lsb
(define_insn "*nds_extend<GPR:mode>_ashr<SHORT:mode>"
  [(set (match_operand:GPR 0 "register_operand"                                    "=r")
	(sign_extend:GPR (ashiftrt:SHORT (match_operand:SHORT 1 "register_operand" " r")
					 (match_operand 2 "const_int_operand"      " n"))))]
  "TARGET_XANDESPERF
   && UINTVAL (operands[2]) < GET_MODE_BITSIZE (<SHORT:MODE>mode)"
  "nds.bfos\t%0, %1, <SHORT:sh_limit>, %2"
  [(set_attr "type" "shift")
   (set_attr "mode" "<GPR:MODE>")])

;; BFO[SZ]: msb < lsb: The pattern doesn't match zero_extract when
;; exact_log2 (Y + 1) < 0 of (and (ashift) Y).
(define_insn "*nds_bfoz<mode>4"
  [(set (match_operand:GPR 0 "register_operand"                           "=r")
	(and:GPR (ashift:GPR (match_operand:GPR 1 "register_operand"      " r")
			     (match_operand 2 "const_int<sh_bit>_operand" " n"))
		 (match_operand 3 "const_int_operand"                     " i")))]
  "TARGET_XANDESPERF
   && (UINTVAL (operands[2]) != 0)
   && (exact_log2 ((UINTVAL (operands[3]) >> UINTVAL (operands[2])) + 1) > 1)
   && ((UINTVAL (operands[3]) & ((1 << UINTVAL (operands[2])) - 1)) == 0)"
  {
    operands[3] =
      GEN_INT (exact_log2 ((UINTVAL (operands[3]) >> UINTVAL (operands[2])) + 1)
	       + UINTVAL (operands[2]) - 1) ;
    return "nds.bfoz\t%0,%1,%2,%3";
  }
  [(set_attr "type" "shift")
   (set_attr "mode" "<GPR:MODE>")])

;; BFOZ: msb = 0.
(define_insn "*nds_bfoz0<mode>4"
  [(set (match_operand:GPR 0 "register_operand"                           "=r")
	(and:GPR (ashift:GPR (match_operand:GPR 1 "register_operand"      " r")
			     (match_operand 2 "const_int<sh_bit>_operand" " n"))
		 (match_operand 3 "const_int_operand"                     " i")))]
  "TARGET_XANDESPERF
   && (UINTVAL (operands[2]) != 0)
   && (exact_log2 ((UINTVAL (operands[3]) >> UINTVAL (operands[2])) + 1) == 1)
   && ((UINTVAL (operands[3]) & ((1 << UINTVAL (operands[2])) - 1)) == 0)"
  {
    return "nds.bfoz\t%0,%1,0,%2";
  }
  [(set_attr "type" "shift")
   (set_attr "mode" "<GPR:MODE>")])

;; BFO: msb = 0.
(define_insn "*nds_bfos0<mode>4"
  [(set (match_operand:GPR 0 "register_operand"                                 "=r")
	(ashift:GPR (any_extract:GPR (match_operand:GPR 1 "register_operand"    " r")
				     (const_int 1)
				     (const_int 0))
		    (match_operand 2 "const_int<sh_bit>_operand"                " n")))]
  "TARGET_XANDESPERF"
  {
    return "nds.bfo<sz>\t%0,%1,0,%2";
  }
  [(set_attr "type" "shift")
   (set_attr "mode" "<GPR:MODE>")])

;; BFO: msb < lsb.
(define_insn "*nds_bfos_<sz>extra_<mode>4"
  [(set (match_operand:GPR 0 "register_operand"                                     "=r")
	(ashift:GPR (any_extract:GPR (match_operand:GPR 1 "register_operand"        " r")
				     (match_operand 2 "ads_extract_size_imm_<mode>" " n")
				     (const_int 0))
		    (match_operand 3 "const_int<sh_bit>_operand"                    " n")))]
  "TARGET_XANDESPERF
   && UINTVAL (operands[2]) != 1"
  {
    operands[2] =  GEN_INT (UINTVAL (operands[2]) + UINTVAL (operands[3]) - 1);
    return "nds.bfo<sz>\t%0,%1,%3,%2";
  }
  [(set_attr "type" "shift")
   (set_attr "mode" "<GPR:MODE>")])

;; BFO: msb < lsb
(define_insn "*nds_<optab><ANYLE32:mode>_shft_<GPR:mode>"
  [(set (match_operand:GPR 0 "register_operand"               "=r")
	(ashift:GPR (any_extend:GPR
		    (match_operand:ANYLE32 1 "register_operand" " r"))
		    (match_operand 2 "const_int_operand"      " n")))]
  "TARGET_XANDESPERF
   && (UINTVAL (operands[2]) < <ANYLE32:sizen>)
   && ((INTVAL (operands[2]) + <ANYLE32:sizen>) <= <GPR:sizen>)"
{
  operands[3] = GEN_INT (<ANYLE32:sizen> + INTVAL (operands[2]) - 1);
  return "nds.bfo<sz>\t%0, %1, %2, %3";
}
  [(set_attr "type" "shift")])

;; BFO: msb < lsb
(define_insn "*nds_<optab><GPR:mode>_ashl<ANYLE32:mode>"
  [(set (match_operand:GPR 0 "register_operand"                 "=r")
	(any_extend:GPR
	(ashift:ANYLE32 (match_operand:ANYLE32 1 "register_operand" " r")
		      (match_operand 2 "const_int_operand"      " n"))))]
  "TARGET_XANDESPERF
   && UINTVAL (operands[2]) < ((<ANYLE32:sizen>) - 1)"
{
  operands[3] = GEN_INT (<ANYLE32:sizen> - 1);
  return "nds.bfo<sz>\t%0, %1, %2, %3";
}
  [(set_attr "type" "shift")
   (set_attr "mode" "<GPR:MODE>")])

;;
;;  ....................
;;
;;	LOAD ADDRESS
;;
;;  ....................
;;

(define_insn "lea_h<mode>"
  [(set (match_operand:P                   0 "register_operand" "=r")
	(plus:P (ashift:P (match_operand:P 1 "register_operand" " r")
			  (const_int 1))
		(match_operand:P           2 "register_operand" " r")))]
  "TARGET_XANDESPERF"
  { return "nds.lea.h\t%0,%2,%1"; }
  [(set_attr "type" "arith")
   (set_attr "mode" "<MODE>")])

(define_insn "lea_w<mode>"
  [(set (match_operand:P                   0 "register_operand" "=r")
	(plus:P (ashift:P (match_operand:P 1 "register_operand" " r")
			  (const_int 2))
		(match_operand:P           2 "register_operand" " r")))]
  "TARGET_XANDESPERF"
  { return "nds.lea.w\t%0,%2,%1"; }
  [(set_attr "type" "arith")
   (set_attr "mode" "<MODE>")])

(define_insn "lea_d<mode>"
  [(set (match_operand:P                   0 "register_operand" "=r")
	(plus:P (ashift:P (match_operand:P 1 "register_operand" " r")
			  (const_int 3))
		(match_operand:P           2 "register_operand" " r")))]
  "TARGET_XANDESPERF"
  { return "nds.lea.d\t%0,%2,%1"; }
  [(set_attr "type" "arith")
   (set_attr "mode" "<MODE>")])

(define_insn "lea_b_ze"
  [(set (match_operand:DI 0 "register_operand"                          "=r")
	(plus:DI (zero_extend:DI (match_operand:SI 1 "register_operand" " r"))
		 (match_operand:DI 2 "register_operand"                 " r")))]
  "TARGET_64BIT && TARGET_XANDESPERF"
  { return "nds.lea.b.ze\t%0,%2,%1"; }
  [(set_attr "type" "arith")
   (set_attr "mode" "DI")])

(define_insn "lea_h_ze"
  [(set (match_operand:DI 0 "register_operand"                                     "=r")
	(plus:DI (ashift:DI (zero_extend:DI (match_operand:SI 1 "register_operand" " r"))
			    (const_int 1))
		 (match_operand:DI 2 "register_operand"                            " r")))]
  "TARGET_64BIT && TARGET_XANDESPERF"
  { return "nds.lea.h.ze\t%0,%2,%1"; }
  [(set_attr "type" "arith")
   (set_attr "mode" "DI")])

(define_insn "lea_w_ze"
  [(set (match_operand:DI 0 "register_operand"                                     "=r")
	(plus:DI (ashift:DI (zero_extend:DI (match_operand:SI 1 "register_operand" " r"))
			    (const_int 2))
		 (match_operand:DI 2 "register_operand"                            " r")))]
  "TARGET_64BIT && TARGET_XANDESPERF"
  { return "nds.lea.w.ze\t%0,%2,%1"; }
  [(set_attr "type" "arith")
   (set_attr "mode" "DI")])

(define_insn "lea_d_ze"
  [(set (match_operand:DI 0 "register_operand"                                     "=r")
	(plus:DI (ashift:DI (zero_extend:DI (match_operand:SI 1 "register_operand" " r"))
			    (const_int 3))
		 (match_operand:DI 2 "register_operand"                            " r")))]
  "TARGET_64BIT && TARGET_XANDESPERF"
  { return "nds.lea.d.ze\t%0,%2,%1"; }
  [(set_attr "type" "arith")
   (set_attr "mode" "DI")])

(define_insn "lea_andim_ashift"
  [(set (match_operand:DI 0 "register_operand"				   "=r")
	(plus:DI (and:DI (ashift:DI (match_operand:DI 1 "register_operand" " r")
				    (match_operand 2 "const_int_operand"   " i"))
			 (match_operand 3 "const_int_operand"		   " i"))
	(match_operand:DI 4 "register_operand"				   " r")))]
  "TARGET_64BIT && TARGET_XANDESPERF
   && IN_RANGE (UINTVAL (operands[2]), 0, 3)
   && exact_log2 ((INTVAL (operands[3]) >> INTVAL (operands[2])) + 1) == 32
   && (INTVAL (operands[3]) & ((1 << INTVAL (operands[2])) - 1)) == 0"
  {
  switch (UINTVAL (operands[2]))
    {
    case 0:
      return "nds.lea.b.ze %0, %4, %1";
    case 1:
      return "nds.lea.h.ze %0, %4, %1";
    case 2:
      return "nds.lea.w.ze %0, %4, %1";
    case 3:
      return "nds.lea.d.ze %0, %4, %1";
    default:
      gcc_unreachable ();
    }
  }
  [(set_attr "type" "arith")
   (set_attr "mode" "DI")])

;;
;;  ....................
;;
;;    String Extension
;;
;;  ....................
;;

(define_insn "riscv_nds_ffb<mode>"
  [(set (match_operand:GPR 0 "register_operand" "=r")
	(unspec:GPR [(match_operand:GPR 1 "reg_or_0_operand" "rJ")
		     (match_operand:GPR 2 "nonmemory_operand" "rJ")] UNSPEC_NDS_FFB))]
  ""
  "nds.ffb\t%0, %z1, %z2"
  [(set_attr "mode" "<MODE>")
   (set_attr "type" "arith")])

(define_insn "riscv_nds_ffzmism<mode>"
  [(set (match_operand:GPR 0 "register_operand" "=r")
	(unspec:GPR [(match_operand:GPR 1 "reg_or_0_operand" "rJ")
		     (match_operand:GPR 2 "reg_or_0_operand" "rJ")] UNSPEC_NDS_FFZMISM))]
  ""
  "nds.ffzmism\t%0, %z1, %z2"
  [(set_attr "mode" "<MODE>")
   (set_attr "type" "arith")])

(define_insn "riscv_nds_ffmism<mode>"
  [(set (match_operand:GPR 0 "register_operand" "=r")
	(unspec:GPR [(match_operand:GPR 1 "reg_or_0_operand" "rJ")
		     (match_operand:GPR 2 "reg_or_0_operand" "rJ")] UNSPEC_NDS_FFMISM))]
  ""
  "nds.ffmism\t%0, %z1, %z2"
  [(set_attr "mode" "<MODE>")
   (set_attr "type" "arith")])

(define_insn "riscv_nds_flmism<mode>"
  [(set (match_operand:GPR 0 "register_operand" "=r")
	(unspec:GPR [(match_operand:GPR 1 "reg_or_0_operand" "rJ")
		     (match_operand:GPR 2 "reg_or_0_operand" "rJ")] UNSPEC_NDS_FLMISM))]
  ""
  "nds.flmism\t%0, %z1, %z2"
  [(set_attr "mode" "<MODE>")
   (set_attr "type" "arith")])
