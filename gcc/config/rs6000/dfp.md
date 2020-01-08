;; Decimal Floating Point (DFP) patterns.
;; Copyright (C) 2007-2020 Free Software Foundation, Inc.
;; Contributed by Ben Elliston (bje@au.ibm.com) and Peter Bergner
;; (bergner@vnet.ibm.com).

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.

;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;;
;; UNSPEC usage
;;

(define_c_enum "unspec"
  [UNSPEC_MOVSD_LOAD
   UNSPEC_MOVSD_STORE
  ])

; Either of the two decimal modes.
(define_mode_iterator DDTD [DD TD])

(define_mode_attr q [(DD "") (TD "q")])


(define_insn "movsd_store"
  [(set (match_operand:DD 0 "nonimmediate_operand" "=m")
	(unspec:DD [(match_operand:SD 1 "input_operand" "d")]
		   UNSPEC_MOVSD_STORE))]
  "(gpc_reg_operand (operands[0], DDmode)
   || gpc_reg_operand (operands[1], SDmode))
   && TARGET_HARD_FLOAT"
  "stfd%U0%X0 %1,%0"
  [(set_attr "type" "fpstore")])

(define_insn "movsd_load"
  [(set (match_operand:SD 0 "nonimmediate_operand" "=f")
	(unspec:SD [(match_operand:DD 1 "input_operand" "m")]
		   UNSPEC_MOVSD_LOAD))]
  "(gpc_reg_operand (operands[0], SDmode)
   || gpc_reg_operand (operands[1], DDmode))
   && TARGET_HARD_FLOAT"
  "lfd%U1%X1 %0,%1"
  [(set_attr "type" "fpload")])

;; Hardware support for decimal floating point operations.

(define_insn "extendsddd2"
  [(set (match_operand:DD 0 "gpc_reg_operand" "=d")
	(float_extend:DD (match_operand:SD 1 "gpc_reg_operand" "f")))]
  "TARGET_DFP"
  "dctdp %0,%1"
  [(set_attr "type" "dfp")])

(define_expand "extendsdtd2"
  [(set (match_operand:TD 0 "gpc_reg_operand" "=d")
	(float_extend:TD (match_operand:SD 1 "gpc_reg_operand" "d")))]
  "TARGET_DFP"
{
  rtx tmp = gen_reg_rtx (DDmode);
  emit_insn (gen_extendsddd2 (tmp, operands[1]));
  emit_insn (gen_extendddtd2 (operands[0], tmp));
  DONE;
})

(define_insn "truncddsd2"
  [(set (match_operand:SD 0 "gpc_reg_operand" "=f")
	(float_truncate:SD (match_operand:DD 1 "gpc_reg_operand" "d")))]
  "TARGET_DFP"
  "drsp %0,%1"
  [(set_attr "type" "dfp")])

(define_insn "negdd2"
  [(set (match_operand:DD 0 "gpc_reg_operand" "=d")
	(neg:DD (match_operand:DD 1 "gpc_reg_operand" "d")))]
  "TARGET_HARD_FLOAT"
  "fneg %0,%1"
  [(set_attr "type" "fpsimple")])

(define_insn "absdd2"
  [(set (match_operand:DD 0 "gpc_reg_operand" "=d")
	(abs:DD (match_operand:DD 1 "gpc_reg_operand" "d")))]
  "TARGET_HARD_FLOAT"
  "fabs %0,%1"
  [(set_attr "type" "fpsimple")])

(define_insn "*nabsdd2_fpr"
  [(set (match_operand:DD 0 "gpc_reg_operand" "=d")
	(neg:DD (abs:DD (match_operand:DD 1 "gpc_reg_operand" "d"))))]
  "TARGET_HARD_FLOAT"
  "fnabs %0,%1"
  [(set_attr "type" "fpsimple")])

(define_insn "negtd2"
  [(set (match_operand:TD 0 "gpc_reg_operand" "=d,d")
	(neg:TD (match_operand:TD 1 "gpc_reg_operand" "0,d")))]
  "TARGET_HARD_FLOAT"
  "@
   fneg %0,%1
   fneg %0,%1\;fmr %L0,%L1"
  [(set_attr "type" "fpsimple")
   (set_attr "length" "4,8")])

(define_insn "abstd2"
  [(set (match_operand:TD 0 "gpc_reg_operand" "=d,d")
	(abs:TD (match_operand:TD 1 "gpc_reg_operand" "0,d")))]
  "TARGET_HARD_FLOAT"
  "@
   fabs %0,%1
   fabs %0,%1\;fmr %L0,%L1"
  [(set_attr "type" "fpsimple")
   (set_attr "length" "4,8")])

(define_insn "*nabstd2_fpr"
  [(set (match_operand:TD 0 "gpc_reg_operand" "=d,d")
	(neg:TD (abs:TD (match_operand:TD 1 "gpc_reg_operand" "0,d"))))]
  "TARGET_HARD_FLOAT"
  "@
   fnabs %0,%1
   fnabs %0,%1\;fmr %L0,%L1"
  [(set_attr "type" "fpsimple")
   (set_attr "length" "4,8")])

;; Hardware support for decimal floating point operations.

(define_insn "extendddtd2"
  [(set (match_operand:TD 0 "gpc_reg_operand" "=d")
	(float_extend:TD (match_operand:DD 1 "gpc_reg_operand" "d")))]
  "TARGET_DFP"
  "dctqpq %0,%1"
  [(set_attr "type" "dfp")])

;; The result of drdpq is an even/odd register pair with the converted
;; value in the even register and zero in the odd register.
;; FIXME: Avoid the register move by using a reload constraint to ensure
;; that the result is the first of the pair receiving the result of drdpq.

(define_insn "trunctddd2"
  [(set (match_operand:DD 0 "gpc_reg_operand" "=d")
	(float_truncate:DD (match_operand:TD 1 "gpc_reg_operand" "d")))
   (clobber (match_scratch:TD 2 "=d"))]
  "TARGET_DFP"
  "drdpq %2,%1\;fmr %0,%2"
  [(set_attr "type" "dfp")
   (set_attr "length" "8")])

(define_insn "add<mode>3"
  [(set (match_operand:DDTD 0 "gpc_reg_operand" "=d")
	(plus:DDTD (match_operand:DDTD 1 "gpc_reg_operand" "%d")
		   (match_operand:DDTD 2 "gpc_reg_operand" "d")))]
  "TARGET_DFP"
  "dadd<q> %0,%1,%2"
  [(set_attr "type" "dfp")])

(define_insn "sub<mode>3"
  [(set (match_operand:DDTD 0 "gpc_reg_operand" "=d")
	(minus:DDTD (match_operand:DDTD 1 "gpc_reg_operand" "d")
		    (match_operand:DDTD 2 "gpc_reg_operand" "d")))]
  "TARGET_DFP"
  "dsub<q> %0,%1,%2"
  [(set_attr "type" "dfp")])

(define_insn "mul<mode>3"
  [(set (match_operand:DDTD 0 "gpc_reg_operand" "=d")
	(mult:DDTD (match_operand:DDTD 1 "gpc_reg_operand" "%d")
		   (match_operand:DDTD 2 "gpc_reg_operand" "d")))]
  "TARGET_DFP"
  "dmul<q> %0,%1,%2"
  [(set_attr "type" "dfp")])

(define_insn "div<mode>3"
  [(set (match_operand:DDTD 0 "gpc_reg_operand" "=d")
	(div:DDTD (match_operand:DDTD 1 "gpc_reg_operand" "d")
		  (match_operand:DDTD 2 "gpc_reg_operand" "d")))]
  "TARGET_DFP"
  "ddiv<q> %0,%1,%2"
  [(set_attr "type" "dfp")])

(define_insn "*cmp<mode>_internal1"
  [(set (match_operand:CCFP 0 "cc_reg_operand" "=y")
	(compare:CCFP (match_operand:DDTD 1 "gpc_reg_operand" "d")
		      (match_operand:DDTD 2 "gpc_reg_operand" "d")))]
  "TARGET_DFP"
  "dcmpu<q> %0,%1,%2"
  [(set_attr "type" "dfp")])

(define_insn "floatdidd2"
  [(set (match_operand:DD 0 "gpc_reg_operand" "=d")
	(float:DD (match_operand:DI 1 "gpc_reg_operand" "d")))]
  "TARGET_DFP && TARGET_POPCNTD"
  "dcffix %0,%1"
  [(set_attr "type" "dfp")])

(define_insn "floatditd2"
  [(set (match_operand:TD 0 "gpc_reg_operand" "=d")
	(float:TD (match_operand:DI 1 "gpc_reg_operand" "d")))]
  "TARGET_DFP"
  "dcffixq %0,%1"
  [(set_attr "type" "dfp")])

;; Convert a decimal64/128 to a decimal64/128 whose value is an integer.
;; This is the first stage of converting it to an integer type.

(define_insn "ftrunc<mode>2"
  [(set (match_operand:DDTD 0 "gpc_reg_operand" "=d")
	(fix:DDTD (match_operand:DDTD 1 "gpc_reg_operand" "d")))]
  "TARGET_DFP"
  "drintn<q>. 0,%0,%1,1"
  [(set_attr "type" "dfp")])

;; Convert a decimal64/128 whose value is an integer to an actual integer.
;; This is the second stage of converting decimal float to integer type.

(define_insn "fix<mode>di2"
  [(set (match_operand:DI 0 "gpc_reg_operand" "=d")
	(fix:DI (match_operand:DDTD 1 "gpc_reg_operand" "d")))]
  "TARGET_DFP"
  "dctfix<q> %0,%1"
  [(set_attr "type" "dfp")])

;; Decimal builtin support

(define_c_enum "unspec"
  [UNSPEC_DDEDPD
   UNSPEC_DENBCD
   UNSPEC_DXEX
   UNSPEC_DIEX
   UNSPEC_DSCLI
   UNSPEC_DTSTSFI
   UNSPEC_DSCRI])

(define_code_iterator DFP_TEST [eq lt gt unordered])

(define_insn "dfp_ddedpd_<mode>"
  [(set (match_operand:DDTD 0 "gpc_reg_operand" "=d")
	(unspec:DDTD [(match_operand:QI 1 "const_0_to_3_operand" "i")
		      (match_operand:DDTD 2 "gpc_reg_operand" "d")]
		     UNSPEC_DDEDPD))]
  "TARGET_DFP"
  "ddedpd<q> %1,%0,%2"
  [(set_attr "type" "dfp")])

(define_insn "dfp_denbcd_<mode>"
  [(set (match_operand:DDTD 0 "gpc_reg_operand" "=d")
	(unspec:DDTD [(match_operand:QI 1 "const_0_to_1_operand" "i")
		      (match_operand:DDTD 2 "gpc_reg_operand" "d")]
		     UNSPEC_DENBCD))]
  "TARGET_DFP"
  "denbcd<q> %1,%0,%2"
  [(set_attr "type" "dfp")])

(define_insn "dfp_dxex_<mode>"
  [(set (match_operand:DI 0 "gpc_reg_operand" "=d")
	(unspec:DI [(match_operand:DDTD 1 "gpc_reg_operand" "d")]
		   UNSPEC_DXEX))]
  "TARGET_DFP"
  "dxex<q> %0,%1"
  [(set_attr "type" "dfp")])

(define_insn "dfp_diex_<mode>"
  [(set (match_operand:DDTD 0 "gpc_reg_operand" "=d")
	(unspec:DDTD [(match_operand:DI 1 "gpc_reg_operand" "d")
		      (match_operand:DDTD 2 "gpc_reg_operand" "d")]
		     UNSPEC_DXEX))]
  "TARGET_DFP"
  "diex<q> %0,%1,%2"
  [(set_attr "type" "dfp")])

(define_expand "dfptstsfi_<code>_<mode>"
  [(set (match_dup 3)
	(compare:CCFP (unspec:DDTD [(match_operand:SI 1 "const_int_operand")
				    (match_operand:DDTD 2 "gpc_reg_operand")]
				   UNSPEC_DTSTSFI)
		      (const_int 0)))
   (set (match_operand:SI 0 "register_operand")
	(DFP_TEST:SI (match_dup 3)
		     (const_int 0)))
  ]
  "TARGET_P9_MISC"
{
  if (<CODE> == UNORDERED && !HONOR_NANS (<MODE>mode))
    {
      emit_move_insn (operands[0], const0_rtx);
      DONE;
    }

  operands[3] = gen_reg_rtx (CCFPmode);
})

(define_insn "*dfp_sgnfcnc_<mode>"
  [(set (match_operand:CCFP 0 "" "=y")
	(compare:CCFP
	 (unspec:DDTD [(match_operand:SI 1 "const_int_operand" "n")
		       (match_operand:DDTD 2 "gpc_reg_operand" "d")]
		      UNSPEC_DTSTSFI)
	 (match_operand:SI 3 "zero_constant" "j")))]
  "TARGET_P9_MISC"
{
  /* If immediate operand is greater than 63, it will behave as if
     the value had been 63.  The code generator does not support
     immediate operand values greater than 63.  */
  if (!(IN_RANGE (INTVAL (operands[1]), 0, 63)))
    operands[1] = GEN_INT (63);
  return "dtstsfi<q> %0,%1,%2";
}
  [(set_attr "type" "fp")])

(define_insn "dfp_dscli_<mode>"
  [(set (match_operand:DDTD 0 "gpc_reg_operand" "=d")
	(unspec:DDTD [(match_operand:DDTD 1 "gpc_reg_operand" "d")
		      (match_operand:QI 2 "immediate_operand" "i")]
		     UNSPEC_DSCLI))]
  "TARGET_DFP"
  "dscli<q> %0,%1,%2"
  [(set_attr "type" "dfp")])

(define_insn "dfp_dscri_<mode>"
  [(set (match_operand:DDTD 0 "gpc_reg_operand" "=d")
	(unspec:DDTD [(match_operand:DDTD 1 "gpc_reg_operand" "d")
		      (match_operand:QI 2 "immediate_operand" "i")]
		     UNSPEC_DSCRI))]
  "TARGET_DFP"
  "dscri<q> %0,%1,%2"
  [(set_attr "type" "dfp")])
