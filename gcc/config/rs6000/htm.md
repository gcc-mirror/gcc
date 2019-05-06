;; Hardware Transactional Memory (HTM) patterns.
;; Copyright (C) 2013-2019 Free Software Foundation, Inc.
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
;; UNSPEC usage
;;

(define_c_enum "unspec"
  [UNSPEC_HTM_FENCE
  ])

;;
;; UNSPEC_VOLATILE usage
;;

(define_c_enum "unspecv"
  [UNSPECV_HTM_TABORT
   UNSPECV_HTM_TABORTXC
   UNSPECV_HTM_TABORTXCI
   UNSPECV_HTM_TBEGIN
   UNSPECV_HTM_TCHECK
   UNSPECV_HTM_TEND
   UNSPECV_HTM_TRECHKPT
   UNSPECV_HTM_TRECLAIM
   UNSPECV_HTM_TSR
   UNSPECV_HTM_TTEST
   UNSPECV_HTM_MFSPR
   UNSPECV_HTM_MTSPR
  ])

(define_expand "tabort"
  [(parallel
     [(set (match_operand:CC 1 "cc_reg_operand" "=x")
	   (unspec_volatile:CC [(match_operand:SI 0 "base_reg_operand" "b")]
			       UNSPECV_HTM_TABORT))
      (set (match_dup 2) (unspec:BLK [(match_dup 2)] UNSPEC_HTM_FENCE))])]
  "TARGET_HTM"
{
  operands[2] = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (Pmode));
  MEM_VOLATILE_P (operands[2]) = 1;
})

(define_insn "*tabort"
  [(set (match_operand:CC 1 "cc_reg_operand" "=x")
	(unspec_volatile:CC [(match_operand:SI 0 "base_reg_operand" "b")]
			    UNSPECV_HTM_TABORT))
   (set (match_operand:BLK 2) (unspec:BLK [(match_dup 2)] UNSPEC_HTM_FENCE))]
  "TARGET_HTM"
  "tabort. %0"
  [(set_attr "type" "htmsimple")])

(define_expand "tabort<wd>c"
  [(parallel
     [(set (match_operand:CC 3 "cc_reg_operand" "=x")
	   (unspec_volatile:CC [(match_operand 0 "u5bit_cint_operand" "n")
				(match_operand:GPR 1 "gpc_reg_operand" "r")
				(match_operand:GPR 2 "gpc_reg_operand" "r")]
			       UNSPECV_HTM_TABORTXC))
      (set (match_dup 4) (unspec:BLK [(match_dup 4)] UNSPEC_HTM_FENCE))])]
  "TARGET_HTM"
{
  operands[4] = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (Pmode));
  MEM_VOLATILE_P (operands[4]) = 1;
})

(define_insn "*tabort<wd>c"
  [(set (match_operand:CC 3 "cc_reg_operand" "=x")
	(unspec_volatile:CC [(match_operand 0 "u5bit_cint_operand" "n")
			     (match_operand:GPR 1 "gpc_reg_operand" "r")
			     (match_operand:GPR 2 "gpc_reg_operand" "r")]
			    UNSPECV_HTM_TABORTXC))
   (set (match_operand:BLK 4) (unspec:BLK [(match_dup 4)] UNSPEC_HTM_FENCE))]
  "TARGET_HTM"
  "tabort<wd>c. %0,%1,%2"
  [(set_attr "type" "htmsimple")])

(define_expand "tabort<wd>ci"
  [(parallel
     [(set (match_operand:CC 3 "cc_reg_operand" "=x")
	   (unspec_volatile:CC [(match_operand 0 "u5bit_cint_operand" "n")
				(match_operand:GPR 1 "gpc_reg_operand" "r")
				(match_operand 2 "s5bit_cint_operand" "n")]
			       UNSPECV_HTM_TABORTXCI))
      (set (match_dup 4) (unspec:BLK [(match_dup 4)] UNSPEC_HTM_FENCE))])]
  "TARGET_HTM"
{
  operands[4] = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (Pmode));
  MEM_VOLATILE_P (operands[4]) = 1;
})

(define_insn "*tabort<wd>ci"
  [(set (match_operand:CC 3 "cc_reg_operand" "=x")
	(unspec_volatile:CC [(match_operand 0 "u5bit_cint_operand" "n")
			     (match_operand:GPR 1 "gpc_reg_operand" "r")
			     (match_operand 2 "s5bit_cint_operand" "n")]
			    UNSPECV_HTM_TABORTXCI))
   (set (match_operand:BLK 4) (unspec:BLK [(match_dup 4)] UNSPEC_HTM_FENCE))]
  "TARGET_HTM"
  "tabort<wd>ci. %0,%1,%2"
  [(set_attr "type" "htmsimple")])

(define_expand "tbegin"
  [(parallel
     [(set (match_operand:CC 1 "cc_reg_operand" "=x")
	   (unspec_volatile:CC [(match_operand 0 "const_0_to_1_operand" "n")]
			       UNSPECV_HTM_TBEGIN))
      (set (match_dup 2) (unspec:BLK [(match_dup 2)] UNSPEC_HTM_FENCE))])]
  "TARGET_HTM"
{
  operands[2] = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (Pmode));
  MEM_VOLATILE_P (operands[2]) = 1;
})

(define_insn "*tbegin"
  [(set (match_operand:CC 1 "cc_reg_operand" "=x")
	(unspec_volatile:CC [(match_operand 0 "const_0_to_1_operand" "n")]
			    UNSPECV_HTM_TBEGIN))
   (set (match_operand:BLK 2) (unspec:BLK [(match_dup 2)] UNSPEC_HTM_FENCE))]
  "TARGET_HTM"
  "tbegin. %0"
  [(set_attr "type" "htm")])

(define_expand "tcheck"
  [(parallel
     [(set (match_operand:CC 0 "cc_reg_operand" "=y")
	   (unspec_volatile:CC [(const_int 0)] UNSPECV_HTM_TCHECK))
      (set (match_dup 1) (unspec:BLK [(match_dup 1)] UNSPEC_HTM_FENCE))])]
  "TARGET_HTM"
{
  operands[1] = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (Pmode));
  MEM_VOLATILE_P (operands[1]) = 1;
})

(define_insn "*tcheck"
  [(set (match_operand:CC 0 "cc_reg_operand" "=y")
	(unspec_volatile:CC [(const_int 0)] UNSPECV_HTM_TCHECK))
   (set (match_operand:BLK 1) (unspec:BLK [(match_dup 1)] UNSPEC_HTM_FENCE))]
  "TARGET_HTM"
  "tcheck %0"
  [(set_attr "type" "htm")])

(define_expand "tend"
  [(parallel
     [(set (match_operand:CC 1 "cc_reg_operand" "=x")
	   (unspec_volatile:CC [(match_operand 0 "const_0_to_1_operand" "n")]
			       UNSPECV_HTM_TEND))
      (set (match_dup 2) (unspec:BLK [(match_dup 2)] UNSPEC_HTM_FENCE))])]
  "TARGET_HTM"
{
  operands[2] = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (Pmode));
  MEM_VOLATILE_P (operands[2]) = 1;
})

(define_insn "*tend"
  [(set (match_operand:CC 1 "cc_reg_operand" "=x")
	(unspec_volatile:CC [(match_operand 0 "const_0_to_1_operand" "n")]
			    UNSPECV_HTM_TEND))
   (set (match_operand:BLK 2) (unspec:BLK [(match_dup 2)] UNSPEC_HTM_FENCE))]
  "TARGET_HTM"
  "tend. %0"
  [(set_attr "type" "htm")])

(define_expand "trechkpt"
  [(parallel
     [(set (match_operand:CC 0 "cc_reg_operand" "=x")
	   (unspec_volatile:CC [(const_int 0)] UNSPECV_HTM_TRECHKPT))
      (set (match_dup 1) (unspec:BLK [(match_dup 1)] UNSPEC_HTM_FENCE))])]
  "TARGET_HTM"
{
  operands[1] = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (Pmode));
  MEM_VOLATILE_P (operands[1]) = 1;
})

(define_insn "*trechkpt"
  [(set (match_operand:CC 0 "cc_reg_operand" "=x")
	(unspec_volatile:CC [(const_int 0)] UNSPECV_HTM_TRECHKPT))
   (set (match_operand:BLK 1) (unspec:BLK [(match_dup 1)] UNSPEC_HTM_FENCE))]
  "TARGET_HTM"
  "trechkpt."
  [(set_attr "type" "htmsimple")])

(define_expand "treclaim"
  [(parallel
     [(set (match_operand:CC 1 "cc_reg_operand" "=x")
	   (unspec_volatile:CC [(match_operand:SI 0 "gpc_reg_operand" "r")]
			       UNSPECV_HTM_TRECLAIM))
      (set (match_dup 2) (unspec:BLK [(match_dup 2)] UNSPEC_HTM_FENCE))])]
  "TARGET_HTM"
{
  operands[2] = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (Pmode));
  MEM_VOLATILE_P (operands[2]) = 1;
})

(define_insn "*treclaim"
  [(set (match_operand:CC 1 "cc_reg_operand" "=x")
	(unspec_volatile:CC [(match_operand:SI 0 "gpc_reg_operand" "r")]
			    UNSPECV_HTM_TRECLAIM))
   (set (match_operand:BLK 2) (unspec:BLK [(match_dup 2)] UNSPEC_HTM_FENCE))]
  "TARGET_HTM"
  "treclaim. %0"
  [(set_attr "type" "htmsimple")])

(define_expand "tsr"
  [(parallel
     [(set (match_operand:CC 1 "cc_reg_operand" "=x")
	   (unspec_volatile:CC [(match_operand 0 "const_0_to_1_operand" "n")]
			       UNSPECV_HTM_TSR))
      (set (match_dup 2) (unspec:BLK [(match_dup 2)] UNSPEC_HTM_FENCE))])]
  "TARGET_HTM"
{
  operands[2] = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (Pmode));
  MEM_VOLATILE_P (operands[2]) = 1;
})

(define_insn "*tsr"
  [(set (match_operand:CC 1 "cc_reg_operand" "=x")
	(unspec_volatile:CC [(match_operand 0 "const_0_to_1_operand" "n")]
			    UNSPECV_HTM_TSR))
   (set (match_operand:BLK 2) (unspec:BLK [(match_dup 2)] UNSPEC_HTM_FENCE))]
  "TARGET_HTM"
  "tsr. %0"
  [(set_attr "type" "htmsimple")])

(define_expand "ttest"
  [(parallel
     [(set (match_operand:CC 0 "cc_reg_operand" "=x")
	   (unspec_volatile:CC [(const_int 0)] UNSPECV_HTM_TTEST))
      (set (match_dup 1) (unspec:BLK [(match_dup 1)] UNSPEC_HTM_FENCE))])]
  "TARGET_HTM"
{
  operands[1] = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (Pmode));
  MEM_VOLATILE_P (operands[1]) = 1;
})

(define_insn "*ttest"
  [(set (match_operand:CC 0 "cc_reg_operand" "=x")
	(unspec_volatile:CC [(const_int 0)] UNSPECV_HTM_TTEST))
   (set (match_operand:BLK 1) (unspec:BLK [(match_dup 1)] UNSPEC_HTM_FENCE))]
  "TARGET_HTM"
  "tabortwci. 0,1,0"
  [(set_attr "type" "htmsimple")])

(define_insn "htm_mfspr_<mode>"
  [(set (match_operand:GPR 0 "gpc_reg_operand" "=r")
        (unspec_volatile:GPR [(match_operand 1 "u10bit_cint_operand" "n")]
			     UNSPECV_HTM_MFSPR))]
  "TARGET_HTM"
  "mfspr %0,%1";
  [(set_attr "type" "htm")])

(define_insn "htm_mtspr_<mode>"
  [(unspec_volatile [(match_operand:GPR 0 "gpc_reg_operand" "r")
		     (match_operand 1 "u10bit_cint_operand" "n")]
		    UNSPECV_HTM_MTSPR)]
  "TARGET_HTM"
  "mtspr %1,%0";
  [(set_attr "type" "htm")])
