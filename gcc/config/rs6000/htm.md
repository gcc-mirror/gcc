;; Hardware Transactional Memory (HTM) patterns.
;; Copyright (C) 2013-2014 Free Software Foundation, Inc.
;; Contributed by Peter Bergner <bergner@vnet.ibm.com>.

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

(define_constants
  [(TFHAR_SPR		128)
   (TFIAR_SPR		129)
   (TEXASR_SPR		130)
   (TEXASRU_SPR		131)
   (MAX_HTM_OPERANDS	4)
  ])

;;
;; UNSPEC_VOLATILE usage
;;

(define_c_enum "unspecv"
  [UNSPECV_HTM_TABORT
   UNSPECV_HTM_TABORTDC
   UNSPECV_HTM_TABORTDCI
   UNSPECV_HTM_TABORTWC
   UNSPECV_HTM_TABORTWCI
   UNSPECV_HTM_TBEGIN
   UNSPECV_HTM_TCHECK
   UNSPECV_HTM_TEND
   UNSPECV_HTM_TRECHKPT
   UNSPECV_HTM_TRECLAIM
   UNSPECV_HTM_TSR
   UNSPECV_HTM_MFSPR
   UNSPECV_HTM_MTSPR
  ])


(define_expand "tabort"
  [(set (match_dup 2)
	(unspec_volatile:CC [(match_operand:SI 1 "int_reg_operand" "")]
			    UNSPECV_HTM_TABORT))
   (set (match_dup 3)
	(eq:SI (match_dup 2)
	       (const_int 0)))
   (set (match_operand:SI 0 "int_reg_operand" "")
	(minus:SI (const_int 1) (match_dup 3)))]
  "TARGET_HTM"
{
  operands[2] = gen_rtx_REG (CCmode, CR0_REGNO);
  operands[3] = gen_reg_rtx (SImode);
})

(define_insn "*tabort_internal"
  [(set (match_operand:CC 1 "cc_reg_operand" "=x")
	(unspec_volatile:CC [(match_operand:SI 0 "int_reg_operand" "r")]
			    UNSPECV_HTM_TABORT))]
  "TARGET_HTM"
  "tabort. %0"
  [(set_attr "type" "htm")
   (set_attr "length" "4")])

(define_expand "tabortdc"
  [(set (match_dup 4)
	(unspec_volatile:CC [(match_operand 1 "u5bit_cint_operand" "n")
			     (match_operand:SI 2 "gpc_reg_operand" "r")
			     (match_operand:SI 3 "gpc_reg_operand" "r")]
			    UNSPECV_HTM_TABORTDC))
   (set (match_dup 5)
	(eq:SI (match_dup 4)
	       (const_int 0)))
   (set (match_operand:SI 0 "int_reg_operand" "")
	(minus:SI (const_int 1) (match_dup 5)))]
  "TARGET_HTM"
{
  operands[4] = gen_rtx_REG (CCmode, CR0_REGNO);
  operands[5] = gen_reg_rtx (SImode);
})

(define_insn "*tabortdc_internal"
  [(set (match_operand:CC 3 "cc_reg_operand" "=x")
	(unspec_volatile:CC [(match_operand 0 "u5bit_cint_operand" "n")
			     (match_operand:SI 1 "gpc_reg_operand" "r")
			     (match_operand:SI 2 "gpc_reg_operand" "r")]
			    UNSPECV_HTM_TABORTDC))]
  "TARGET_HTM"
  "tabortdc. %0,%1,%2"
  [(set_attr "type" "htm")
   (set_attr "length" "4")])

(define_expand "tabortdci"
  [(set (match_dup 4)
	(unspec_volatile:CC [(match_operand 1 "u5bit_cint_operand" "n")
			     (match_operand:SI 2 "gpc_reg_operand" "r")
			     (match_operand 3 "s5bit_cint_operand" "n")]
			    UNSPECV_HTM_TABORTDCI))
   (set (match_dup 5)
	(eq:SI (match_dup 4)
	       (const_int 0)))
   (set (match_operand:SI 0 "int_reg_operand" "")
	(minus:SI (const_int 1) (match_dup 5)))]
  "TARGET_HTM"
{
  operands[4] = gen_rtx_REG (CCmode, CR0_REGNO);
  operands[5] = gen_reg_rtx (SImode);
})

(define_insn "*tabortdci_internal"
  [(set (match_operand:CC 3 "cc_reg_operand" "=x")
	(unspec_volatile:CC [(match_operand 0 "u5bit_cint_operand" "n")
			     (match_operand:SI 1 "gpc_reg_operand" "r")
			     (match_operand 2 "s5bit_cint_operand" "n")]
			    UNSPECV_HTM_TABORTDCI))]
  "TARGET_HTM"
  "tabortdci. %0,%1,%2"
  [(set_attr "type" "htm")
   (set_attr "length" "4")])

(define_expand "tabortwc"
  [(set (match_dup 4)
	(unspec_volatile:CC [(match_operand 1 "u5bit_cint_operand" "n")
			     (match_operand:SI 2 "gpc_reg_operand" "r")
			     (match_operand:SI 3 "gpc_reg_operand" "r")]
			    UNSPECV_HTM_TABORTWC))
   (set (match_dup 5)
	(eq:SI (match_dup 4)
	       (const_int 0)))
   (set (match_operand:SI 0 "int_reg_operand" "")
	(minus:SI (const_int 1) (match_dup 5)))]
  "TARGET_HTM"
{
  operands[4] = gen_rtx_REG (CCmode, CR0_REGNO);
  operands[5] = gen_reg_rtx (SImode);
})

(define_insn "*tabortwc_internal"
  [(set (match_operand:CC 3 "cc_reg_operand" "=x")
	(unspec_volatile:CC [(match_operand 0 "u5bit_cint_operand" "n")
			     (match_operand:SI 1 "gpc_reg_operand" "r")
			     (match_operand:SI 2 "gpc_reg_operand" "r")]
			    UNSPECV_HTM_TABORTWC))]
  "TARGET_HTM"
  "tabortwc. %0,%1,%2"
  [(set_attr "type" "htm")
   (set_attr "length" "4")])

(define_expand "tabortwci"
  [(set (match_dup 4)
	(unspec_volatile:CC [(match_operand 1 "u5bit_cint_operand" "n")
			     (match_operand:SI 2 "gpc_reg_operand" "r")
			     (match_operand 3 "s5bit_cint_operand" "n")]
			    UNSPECV_HTM_TABORTWCI))
   (set (match_dup 5)
	(eq:SI (match_dup 4)
	       (const_int 0)))
   (set (match_operand:SI 0 "int_reg_operand" "")
	(minus:SI (const_int 1) (match_dup 5)))]
  "TARGET_HTM"
{
  operands[4] = gen_rtx_REG (CCmode, CR0_REGNO);
  operands[5] = gen_reg_rtx (SImode);
})

(define_expand "ttest"
  [(set (match_dup 1)
	(unspec_volatile:CC [(const_int 0)
			     (reg:SI 0)
			     (const_int 0)]
			    UNSPECV_HTM_TABORTWCI))
   (set (subreg:CC (match_dup 2) 0) (match_dup 1))
   (set (match_dup 3) (lshiftrt:SI (match_dup 2) (const_int 28)))
   (parallel [(set (match_operand:SI 0 "int_reg_operand" "")
		   (and:SI (match_dup 3) (const_int 15)))
              (clobber (scratch:CC))])]
  "TARGET_HTM"
{
  operands[1] = gen_rtx_REG (CCmode, CR0_REGNO);
  operands[2] = gen_reg_rtx (SImode);
  operands[3] = gen_reg_rtx (SImode);
})

(define_insn "*tabortwci_internal"
  [(set (match_operand:CC 3 "cc_reg_operand" "=x")
	(unspec_volatile:CC [(match_operand 0 "u5bit_cint_operand" "n")
			     (match_operand:SI 1 "gpc_reg_operand" "r")
			     (match_operand 2 "s5bit_cint_operand" "n")]
			    UNSPECV_HTM_TABORTWCI))]
  "TARGET_HTM"
  "tabortwci. %0,%1,%2"
  [(set_attr "type" "htm")
   (set_attr "length" "4")])

(define_expand "tbegin"
  [(set (match_dup 2)
	(unspec_volatile:CC [(match_operand 1 "const_0_to_1_operand" "n")]
			    UNSPECV_HTM_TBEGIN))
   (set (match_dup 3)
	(eq:SI (match_dup 2)
	       (const_int 0)))
   (set (match_operand:SI 0 "int_reg_operand" "")
	(minus:SI (const_int 1) (match_dup 3)))]
  "TARGET_HTM"
{
  operands[2] = gen_rtx_REG (CCmode, CR0_REGNO);
  operands[3] = gen_reg_rtx (SImode);
})

(define_insn "*tbegin_internal"
  [(set (match_operand:CC 1 "cc_reg_operand" "=x")
	(unspec_volatile:CC [(match_operand 0 "const_0_to_1_operand" "n")]
			    UNSPECV_HTM_TBEGIN))]
  "TARGET_HTM"
  "tbegin. %0"
  [(set_attr "type" "htm")
   (set_attr "length" "4")])

(define_expand "tcheck"
  [(set (match_dup 2)
	(unspec_volatile:CC [(match_operand 1 "u3bit_cint_operand" "n")]
			    UNSPECV_HTM_TCHECK))
   (set (match_dup 3)
	(eq:SI (match_dup 2)
	       (const_int 0)))
   (set (match_operand:SI 0 "int_reg_operand" "")
	(minus:SI (const_int 1) (match_dup 3)))]
  "TARGET_HTM"
{
  operands[2] = gen_rtx_REG (CCmode, CR0_REGNO);
  operands[3] = gen_reg_rtx (SImode);
})

(define_insn "*tcheck_internal"
  [(set (match_operand:CC 1 "cc_reg_operand" "=x")
	(unspec_volatile:CC [(match_operand 0 "u3bit_cint_operand" "n")]
			    UNSPECV_HTM_TCHECK))]
  "TARGET_HTM"
  "tcheck. %0"
  [(set_attr "type" "htm")
   (set_attr "length" "4")])

(define_expand "tend"
  [(set (match_dup 2)
	(unspec_volatile:CC [(match_operand 1 "const_0_to_1_operand" "n")]
			    UNSPECV_HTM_TEND))
   (set (match_dup 3)
	(eq:SI (match_dup 2)
	       (const_int 0)))
   (set (match_operand:SI 0 "int_reg_operand" "")
	(minus:SI (const_int 1) (match_dup 3)))]
  "TARGET_HTM"
{
  operands[2] = gen_rtx_REG (CCmode, CR0_REGNO);
  operands[3] = gen_reg_rtx (SImode);
})

(define_insn "*tend_internal"
  [(set (match_operand:CC 1 "cc_reg_operand" "=x")
	(unspec_volatile:CC [(match_operand 0 "const_0_to_1_operand" "n")]
			    UNSPECV_HTM_TEND))]
  "TARGET_HTM"
  "tend. %0"
  [(set_attr "type" "htm")
   (set_attr "length" "4")])

(define_expand "trechkpt"
  [(set (match_dup 1)
	(unspec_volatile:CC [(const_int 0)]
			    UNSPECV_HTM_TRECHKPT))
   (set (match_dup 2)
	(eq:SI (match_dup 1)
	       (const_int 0)))
   (set (match_operand:SI 0 "int_reg_operand" "")
	(minus:SI (const_int 1) (match_dup 2)))]
  "TARGET_HTM"
{
  operands[1] = gen_rtx_REG (CCmode, CR0_REGNO);
  operands[2] = gen_reg_rtx (SImode);
})

(define_insn "*trechkpt_internal"
  [(set (match_operand:CC 0 "cc_reg_operand" "=x")
	(unspec_volatile:CC [(const_int 0)]
			    UNSPECV_HTM_TRECHKPT))]
  "TARGET_HTM"
  "trechkpt."
  [(set_attr "type" "htm")
   (set_attr "length" "4")])

(define_expand "treclaim"
  [(set (match_dup 2)
	(unspec_volatile:CC [(match_operand:SI 1 "gpc_reg_operand" "r")]
			    UNSPECV_HTM_TRECLAIM))
   (set (match_dup 3)
	(eq:SI (match_dup 2)
	       (const_int 0)))
   (set (match_operand:SI 0 "int_reg_operand" "")
	(minus:SI (const_int 1) (match_dup 3)))]
  "TARGET_HTM"
{
  operands[2] = gen_rtx_REG (CCmode, CR0_REGNO);
  operands[3] = gen_reg_rtx (SImode);
})

(define_insn "*treclaim_internal"
  [(set (match_operand:CC 1 "cc_reg_operand" "=x")
	(unspec_volatile:CC [(match_operand:SI 0 "gpc_reg_operand" "r")]
			    UNSPECV_HTM_TRECLAIM))]
  "TARGET_HTM"
  "treclaim. %0"
  [(set_attr "type" "htm")
   (set_attr "length" "4")])

(define_expand "tsr"
  [(set (match_dup 2)
	(unspec_volatile:CC [(match_operand 1 "const_0_to_1_operand" "n")]
			    UNSPECV_HTM_TSR))
   (set (match_dup 3)
	(eq:SI (match_dup 2)
	       (const_int 0)))
   (set (match_operand:SI 0 "int_reg_operand" "")
	(minus:SI (const_int 1) (match_dup 3)))]
  "TARGET_HTM"
{
  operands[2] = gen_rtx_REG (CCmode, CR0_REGNO);
  operands[3] = gen_reg_rtx (SImode);
})

(define_insn "*tsr_internal"
  [(set (match_operand:CC 1 "cc_reg_operand" "=x")
	(unspec_volatile:CC [(match_operand 0 "const_0_to_1_operand" "n")]
			    UNSPECV_HTM_TSR))]
  "TARGET_HTM"
  "tsr. %0"
  [(set_attr "type" "htm")
   (set_attr "length" "4")])

(define_insn "htm_mfspr_<mode>"
  [(set (match_operand:P 0 "gpc_reg_operand" "=r")
        (unspec_volatile:P [(match_operand 1 "u10bit_cint_operand" "n")
			    (match_operand:P 2 "htm_spr_reg_operand" "")]
			   UNSPECV_HTM_MFSPR))]
  "TARGET_HTM"
  "mfspr %0,%1";
  [(set_attr "type" "htm")
   (set_attr "length" "4")])

(define_insn "htm_mtspr_<mode>"
  [(set (match_operand:P 2 "htm_spr_reg_operand" "")
        (unspec_volatile:P [(match_operand:P 0 "gpc_reg_operand" "r")
			    (match_operand 1 "u10bit_cint_operand" "n")]
                           UNSPECV_HTM_MTSPR))]
  "TARGET_HTM"
  "mtspr %1,%0";
  [(set_attr "type" "htm")
   (set_attr "length" "4")])
