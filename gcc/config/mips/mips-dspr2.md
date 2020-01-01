;; Copyright (C) 2007-2020 Free Software Foundation, Inc.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.
;;
; MIPS DSP ASE REV 2 Revision 0.02 11/24/2006

(define_c_enum "unspec" [
  UNSPEC_ABSQ_S_QB
  UNSPEC_ADDU_PH
  UNSPEC_ADDU_S_PH
  UNSPEC_ADDUH_QB
  UNSPEC_ADDUH_R_QB
  UNSPEC_APPEND
  UNSPEC_BALIGN
  UNSPEC_CMPGDU_EQ_QB
  UNSPEC_CMPGDU_LT_QB
  UNSPEC_CMPGDU_LE_QB
  UNSPEC_DPA_W_PH
  UNSPEC_DPS_W_PH
  UNSPEC_MADD
  UNSPEC_MADDU
  UNSPEC_MSUB
  UNSPEC_MSUBU
  UNSPEC_MUL_PH
  UNSPEC_MUL_S_PH
  UNSPEC_MULQ_RS_W
  UNSPEC_MULQ_S_PH
  UNSPEC_MULQ_S_W
  UNSPEC_MULSA_W_PH
  UNSPEC_MULT
  UNSPEC_MULTU
  UNSPEC_PRECR_QB_PH
  UNSPEC_PRECR_SRA_PH_W
  UNSPEC_PRECR_SRA_R_PH_W
  UNSPEC_PREPEND
  UNSPEC_SHRA_QB
  UNSPEC_SHRA_R_QB
  UNSPEC_SHRL_PH
  UNSPEC_SUBU_PH
  UNSPEC_SUBU_S_PH
  UNSPEC_SUBUH_QB
  UNSPEC_SUBUH_R_QB
  UNSPEC_ADDQH_PH
  UNSPEC_ADDQH_R_PH
  UNSPEC_ADDQH_W
  UNSPEC_ADDQH_R_W
  UNSPEC_SUBQH_PH
  UNSPEC_SUBQH_R_PH
  UNSPEC_SUBQH_W
  UNSPEC_SUBQH_R_W
  UNSPEC_DPAX_W_PH
  UNSPEC_DPSX_W_PH
  UNSPEC_DPAQX_S_W_PH
  UNSPEC_DPAQX_SA_W_PH
  UNSPEC_DPSQX_S_W_PH
  UNSPEC_DPSQX_SA_W_PH
])

(define_insn "mips_absq_s_qb"
  [(set (match_operand:V4QI 0 "register_operand" "=d")
	(unspec:V4QI [(match_operand:V4QI 1 "reg_or_0_operand" "dYG")]
		     UNSPEC_ABSQ_S_QB))
   (set (reg:CCDSP CCDSP_OU_REGNUM)
	(unspec:CCDSP [(match_dup 1)] UNSPEC_ABSQ_S_QB))]
  "ISA_HAS_DSPR2"
  "absq_s.qb\t%0,%z1"
  [(set_attr "type"	"dspalusat")
   (set_attr "mode"	"SI")])

(define_insn "mips_addu_ph"
  [(set (match_operand:V2HI 0 "register_operand" "=d")
	(plus:V2HI (match_operand:V2HI 1 "reg_or_0_operand" "dYG")
		   (match_operand:V2HI 2 "reg_or_0_operand" "dYG")))
   (set (reg:CCDSP CCDSP_OU_REGNUM)
	(unspec:CCDSP [(match_dup 1) (match_dup 2)] UNSPEC_ADDU_PH))]
  "ISA_HAS_DSPR2"
  "addu.ph\t%0,%z1,%z2"
  [(set_attr "type"	"dspalu")
   (set_attr "mode"	"SI")])

(define_insn "mips_addu_s_ph"
  [(set (match_operand:V2HI 0 "register_operand" "=d")
	(unspec:V2HI [(match_operand:V2HI 1 "reg_or_0_operand" "dYG")
		      (match_operand:V2HI 2 "reg_or_0_operand" "dYG")]
		     UNSPEC_ADDU_S_PH))
   (set (reg:CCDSP CCDSP_OU_REGNUM)
	(unspec:CCDSP [(match_dup 1) (match_dup 2)] UNSPEC_ADDU_S_PH))]
  "ISA_HAS_DSPR2"
  "addu_s.ph\t%0,%z1,%z2"
  [(set_attr "type"	"dspalusat")
   (set_attr "mode"	"SI")])

(define_insn "mips_adduh_qb"
  [(set (match_operand:V4QI 0 "register_operand" "=d")
	(unspec:V4QI [(match_operand:V4QI 1 "reg_or_0_operand" "dYG")
		      (match_operand:V4QI 2 "reg_or_0_operand" "dYG")]
		     UNSPEC_ADDUH_QB))]
  "ISA_HAS_DSPR2"
  "adduh.qb\t%0,%z1,%z2"
  [(set_attr "type"	"dspalu")
   (set_attr "mode"	"SI")])

(define_insn "mips_adduh_r_qb"
  [(set (match_operand:V4QI 0 "register_operand" "=d")
	(unspec:V4QI [(match_operand:V4QI 1 "reg_or_0_operand" "dYG")
		      (match_operand:V4QI 2 "reg_or_0_operand" "dYG")]
		     UNSPEC_ADDUH_R_QB))]
  "ISA_HAS_DSPR2"
  "adduh_r.qb\t%0,%z1,%z2"
  [(set_attr "type"	"dspalusat")
   (set_attr "mode"	"SI")])

(define_insn "mips_append"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(unspec:SI [(match_operand:SI 1 "register_operand" "0")
		    (match_operand:SI 2 "reg_or_0_operand" "dJ")
		    (match_operand:SI 3 "const_int_operand" "n")]
		   UNSPEC_APPEND))]
  "ISA_HAS_DSPR2"
{
  if (INTVAL (operands[3]) & ~(unsigned HOST_WIDE_INT) 31)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 31);
  return "append\t%0,%z2,%3";
}
  [(set_attr "type"	"dspalu")
   (set_attr "mode"	"SI")])

(define_insn "mips_balign"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(unspec:SI [(match_operand:SI 1 "register_operand" "0")
		    (match_operand:SI 2 "reg_or_0_operand" "dJ")
		    (match_operand:SI 3 "const_int_operand" "n")]
		   UNSPEC_BALIGN))]
  "ISA_HAS_DSPR2"
{
  if (INTVAL (operands[3]) & ~(unsigned HOST_WIDE_INT) 3)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 3);
  return "balign\t%0,%z2,%3";
}
  [(set_attr "type"	"dspalu")
   (set_attr "mode"	"SI")])

(define_insn "mips_cmpgdu_eq_qb"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(unspec:SI [(match_operand:V4QI 1 "reg_or_0_operand" "dYG")
		    (match_operand:V4QI 2 "reg_or_0_operand" "dYG")]
		   UNSPEC_CMPGDU_EQ_QB))
   (set (reg:CCDSP CCDSP_CC_REGNUM)
	(unspec:CCDSP [(match_dup 1) (match_dup 2)
		       (reg:CCDSP CCDSP_CC_REGNUM)]
		      UNSPEC_CMPGDU_EQ_QB))]
  "ISA_HAS_DSPR2"
  "cmpgdu.eq.qb\t%0,%z1,%z2"
  [(set_attr "type"	"dspalu")
   (set_attr "mode"	"SI")])

(define_insn "mips_cmpgdu_lt_qb"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(unspec:SI [(match_operand:V4QI 1 "reg_or_0_operand" "dYG")
		    (match_operand:V4QI 2 "reg_or_0_operand" "dYG")]
		   UNSPEC_CMPGDU_LT_QB))
   (set (reg:CCDSP CCDSP_CC_REGNUM)
	(unspec:CCDSP [(match_dup 1) (match_dup 2)
		       (reg:CCDSP CCDSP_CC_REGNUM)]
		      UNSPEC_CMPGDU_LT_QB))]
  "ISA_HAS_DSPR2"
  "cmpgdu.lt.qb\t%0,%z1,%z2"
  [(set_attr "type"	"dspalu")
   (set_attr "mode"	"SI")])

(define_insn "mips_cmpgdu_le_qb"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(unspec:SI [(match_operand:V4QI 1 "reg_or_0_operand" "dYG")
		    (match_operand:V4QI 2 "reg_or_0_operand" "dYG")]
		   UNSPEC_CMPGDU_LE_QB))
   (set (reg:CCDSP CCDSP_CC_REGNUM)
	(unspec:CCDSP [(match_dup 1) (match_dup 2)
		       (reg:CCDSP CCDSP_CC_REGNUM)]
		      UNSPEC_CMPGDU_LE_QB))]
  "ISA_HAS_DSPR2"
  "cmpgdu.le.qb\t%0,%z1,%z2"
  [(set_attr "type"	"dspalu")
   (set_attr "mode"	"SI")])

(define_insn "mips_dpa_w_ph"
  [(set (match_operand:DI 0 "register_operand" "=a")
	(unspec:DI [(match_operand:DI 1 "register_operand" "0")
		    (match_operand:V2HI 2 "reg_or_0_operand" "dYG")
		    (match_operand:V2HI 3 "reg_or_0_operand" "dYG")]
		   UNSPEC_DPA_W_PH))]
  "ISA_HAS_DSPR2 && !TARGET_64BIT"
  "dpa.w.ph\t%q0,%z2,%z3"
  [(set_attr "type"	"dspmac")
   (set_attr "accum_in" "1")
   (set_attr "mode"	"SI")])

(define_insn "mips_dps_w_ph"
  [(set (match_operand:DI 0 "register_operand" "=a")
	(unspec:DI [(match_operand:DI 1 "register_operand" "0")
		    (match_operand:V2HI 2 "reg_or_0_operand" "dYG")
		    (match_operand:V2HI 3 "reg_or_0_operand" "dYG")]
		   UNSPEC_DPS_W_PH))]
  "ISA_HAS_DSPR2 && !TARGET_64BIT"
  "dps.w.ph\t%q0,%z2,%z3"
  [(set_attr "type"	"dspmac")
   (set_attr "accum_in" "1")
   (set_attr "mode"	"SI")])

(define_insn "mulv2hi3"
  [(set (match_operand:V2HI 0 "register_operand" "=d")
	(mult:V2HI (match_operand:V2HI 1 "register_operand" "d")
		   (match_operand:V2HI 2 "register_operand" "d")))
   (set (reg:CCDSP CCDSP_OU_REGNUM)
	(unspec:CCDSP [(match_dup 1) (match_dup 2)] UNSPEC_MUL_PH))
   (clobber (match_scratch:DI 3 "=x"))]
  "ISA_HAS_DSPR2"
  "mul.ph\t%0,%1,%2"
  [(set_attr "type"	"imul3")
   (set_attr "mode"	"SI")])

(define_insn "mips_mul_s_ph"
  [(set (match_operand:V2HI 0 "register_operand" "=d")
	(unspec:V2HI [(match_operand:V2HI 1 "reg_or_0_operand" "dYG")
		      (match_operand:V2HI 2 "reg_or_0_operand" "dYG")]
		     UNSPEC_MUL_S_PH))
   (set (reg:CCDSP CCDSP_OU_REGNUM)
	(unspec:CCDSP [(match_dup 1) (match_dup 2)] UNSPEC_MUL_S_PH))
   (clobber (match_scratch:DI 3 "=x"))]
  "ISA_HAS_DSPR2"
  "mul_s.ph\t%0,%z1,%z2"
  [(set_attr "type"	"imul3")
   (set_attr "mode"	"SI")])

(define_insn "mips_mulq_rs_w"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(unspec:SI [(match_operand:SI 1 "reg_or_0_operand" "dJ")
		    (match_operand:SI 2 "reg_or_0_operand" "dJ")]
		   UNSPEC_MULQ_RS_W))
   (set (reg:CCDSP CCDSP_OU_REGNUM)
	(unspec:CCDSP [(match_dup 1) (match_dup 2)] UNSPEC_MULQ_RS_W))
   (clobber (match_scratch:DI 3 "=x"))]
  "ISA_HAS_DSPR2"
  "mulq_rs.w\t%0,%z1,%z2"
  [(set_attr "type"	"imul3")
   (set_attr "mode"	"SI")])

(define_insn "mips_mulq_s_ph"
  [(set (match_operand:V2HI 0 "register_operand" "=d")
	(unspec:V2HI [(match_operand:V2HI 1 "reg_or_0_operand" "dYG")
		      (match_operand:V2HI 2 "reg_or_0_operand" "dYG")]
		     UNSPEC_MULQ_S_PH))
   (set (reg:CCDSP CCDSP_OU_REGNUM)
	(unspec:CCDSP [(match_dup 1) (match_dup 2)] UNSPEC_MULQ_S_PH))
   (clobber (match_scratch:DI 3 "=x"))]
  "ISA_HAS_DSPR2"
  "mulq_s.ph\t%0,%z1,%z2"
  [(set_attr "type"	"imul3")
   (set_attr "mode"	"SI")])

(define_insn "mips_mulq_s_w"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(unspec:SI [(match_operand:SI 1 "reg_or_0_operand" "dJ")
		    (match_operand:SI 2 "reg_or_0_operand" "dJ")]
		   UNSPEC_MULQ_S_W))
   (set (reg:CCDSP CCDSP_OU_REGNUM)
	(unspec:CCDSP [(match_dup 1) (match_dup 2)] UNSPEC_MULQ_S_W))
   (clobber (match_scratch:DI 3 "=x"))]
  "ISA_HAS_DSPR2"
  "mulq_s.w\t%0,%z1,%z2"
  [(set_attr "type"	"imul3")
   (set_attr "mode"	"SI")])

(define_insn "mips_mulsa_w_ph"
  [(set (match_operand:DI 0 "register_operand" "=a")
	(unspec:DI [(match_operand:DI 1 "register_operand" "0")
		    (match_operand:V2HI 2 "reg_or_0_operand" "dYG")
		    (match_operand:V2HI 3 "reg_or_0_operand" "dYG")]
		   UNSPEC_MULSA_W_PH))]
  "ISA_HAS_DSPR2 && !TARGET_64BIT"
  "mulsa.w.ph\t%q0,%z2,%z3"
  [(set_attr "type"	"dspmac")
   (set_attr "accum_in" "1")
   (set_attr "mode"	"SI")])

(define_insn "mips_precr_qb_ph"
  [(set (match_operand:V4QI 0 "register_operand" "=d")
	(unspec:V4QI [(match_operand:V2HI 1 "reg_or_0_operand" "dYG")
		      (match_operand:V2HI 2 "reg_or_0_operand" "dYG")]
		     UNSPEC_PRECR_QB_PH))]
  "ISA_HAS_DSPR2"
  "precr.qb.ph\t%0,%z1,%z2"
  [(set_attr "type"	"dspalu")
   (set_attr "mode"	"SI")])

(define_insn "mips_precr_sra_ph_w"
  [(set (match_operand:V2HI 0 "register_operand" "=d")
	(unspec:V2HI [(match_operand:SI 1 "register_operand" "0")
		      (match_operand:SI 2 "reg_or_0_operand" "dJ")
		      (match_operand:SI 3 "const_int_operand" "n")]
		     UNSPEC_PRECR_SRA_PH_W))]
  "ISA_HAS_DSPR2"
{
  if (INTVAL (operands[3]) & ~(unsigned HOST_WIDE_INT) 31)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 31);
  return "precr_sra.ph.w\t%0,%z2,%3";
}
  [(set_attr "type"	"dspalu")
   (set_attr "mode"	"SI")])

(define_insn "mips_precr_sra_r_ph_w"
  [(set (match_operand:V2HI 0 "register_operand" "=d")
	(unspec:V2HI [(match_operand:SI 1 "register_operand" "0")
		      (match_operand:SI 2 "reg_or_0_operand" "dJ")
		      (match_operand:SI 3 "const_int_operand" "n")]
		     UNSPEC_PRECR_SRA_R_PH_W))]
  "ISA_HAS_DSPR2"
{
  if (INTVAL (operands[3]) & ~(unsigned HOST_WIDE_INT) 31)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 31);
  return "precr_sra_r.ph.w\t%0,%z2,%3";
}
  [(set_attr "type"	"dspalu")
   (set_attr "mode"	"SI")])

(define_insn "mips_prepend"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(unspec:SI [(match_operand:SI 1 "register_operand" "0")
		    (match_operand:SI 2 "reg_or_0_operand" "dJ")
		    (match_operand:SI 3 "const_int_operand" "n")]
		   UNSPEC_PREPEND))]
  "ISA_HAS_DSPR2"
{
  if (INTVAL (operands[3]) & ~(unsigned HOST_WIDE_INT) 31)
    operands[3] = GEN_INT (INTVAL (operands[3]) & 31);
  return "prepend\t%0,%z2,%3";
}
  [(set_attr "type"	"dspalu")
   (set_attr "mode"	"SI")])

(define_insn "mips_shra_qb"
  [(set (match_operand:V4QI 0 "register_operand" "=d,d")
	(unspec:V4QI [(match_operand:V4QI 1 "reg_or_0_operand" "dYG,dYG")
		      (match_operand:SI 2 "arith_operand" "I,d")]
		     UNSPEC_SHRA_QB))]
  "ISA_HAS_DSPR2"
{
  if (which_alternative == 0)
    {
      if (INTVAL (operands[2]) & ~(unsigned HOST_WIDE_INT) 7)
	operands[2] = GEN_INT (INTVAL (operands[2]) & 7);
      return "shra.qb\t%0,%z1,%2";
    }
  return "shrav.qb\t%0,%z1,%2";
}
  [(set_attr "type"	"dspalu")
   (set_attr "mode"	"SI")])


(define_insn "mips_shra_r_qb"
  [(set (match_operand:V4QI 0 "register_operand" "=d,d")
	(unspec:V4QI [(match_operand:V4QI 1 "reg_or_0_operand" "dYG,dYG")
		      (match_operand:SI 2 "arith_operand" "I,d")]
		     UNSPEC_SHRA_R_QB))]
  "ISA_HAS_DSPR2"
{
  if (which_alternative == 0)
    {
      if (INTVAL (operands[2]) & ~(unsigned HOST_WIDE_INT) 7)
	operands[2] = GEN_INT (INTVAL (operands[2]) & 7);
      return "shra_r.qb\t%0,%z1,%2";
    }
  return "shrav_r.qb\t%0,%z1,%2";
}
  [(set_attr "type"	"dspalu")
   (set_attr "mode"	"SI")])

(define_insn "mips_shrl_ph"
  [(set (match_operand:V2HI 0 "register_operand" "=d,d")
	(unspec:V2HI [(match_operand:V2HI 1 "reg_or_0_operand" "dYG,dYG")
		      (match_operand:SI 2 "arith_operand" "I,d")]
		     UNSPEC_SHRL_PH))]
  "ISA_HAS_DSPR2"
{
  if (which_alternative == 0)
    {
      if (INTVAL (operands[2]) & ~(unsigned HOST_WIDE_INT) 15)
	operands[2] = GEN_INT (INTVAL (operands[2]) & 15);
      return "shrl.ph\t%0,%z1,%2";
    }
  return "shrlv.ph\t%0,%z1,%2";
}
  [(set_attr "type"	"dspalu")
   (set_attr "mode"	"SI")])

(define_insn "mips_subu_ph"
  [(set (match_operand:V2HI 0 "register_operand" "=d")
	(unspec:V2HI [(match_operand:V2HI 1 "reg_or_0_operand" "dYG")
		      (match_operand:V2HI 2 "reg_or_0_operand" "dYG")]
		     UNSPEC_SUBU_PH))
   (set (reg:CCDSP CCDSP_OU_REGNUM)
	(unspec:CCDSP [(match_dup 1) (match_dup 2)] UNSPEC_SUBU_PH))]
  "ISA_HAS_DSPR2"
  "subu.ph\t%0,%z1,%z2"
  [(set_attr "type"	"dspalu")
   (set_attr "mode"	"SI")])

(define_insn "mips_subu_s_ph"
  [(set (match_operand:V2HI 0 "register_operand" "=d")
	(unspec:V2HI [(match_operand:V2HI 1 "reg_or_0_operand" "dYG")
		      (match_operand:V2HI 2 "reg_or_0_operand" "dYG")]
		     UNSPEC_SUBU_S_PH))
   (set (reg:CCDSP CCDSP_OU_REGNUM)
	(unspec:CCDSP [(match_dup 1) (match_dup 2)] UNSPEC_SUBU_S_PH))]
  "ISA_HAS_DSPR2"
  "subu_s.ph\t%0,%z1,%z2"
  [(set_attr "type"	"dspalusat")
   (set_attr "mode"	"SI")])

(define_insn "mips_subuh_qb"
  [(set (match_operand:V4QI 0 "register_operand" "=d")
	(unspec:V4QI [(match_operand:V4QI 1 "reg_or_0_operand" "dYG")
		      (match_operand:V4QI 2 "reg_or_0_operand" "dYG")]
		     UNSPEC_SUBUH_QB))]
  "ISA_HAS_DSPR2"
  "subuh.qb\t%0,%z1,%z2"
  [(set_attr "type"	"dspalu")
   (set_attr "mode"	"SI")])

(define_insn "mips_subuh_r_qb"
  [(set (match_operand:V4QI 0 "register_operand" "=d")
	(unspec:V4QI [(match_operand:V4QI 1 "reg_or_0_operand" "dYG")
		      (match_operand:V4QI 2 "reg_or_0_operand" "dYG")]
		     UNSPEC_SUBUH_R_QB))]
  "ISA_HAS_DSPR2"
  "subuh_r.qb\t%0,%z1,%z2"
  [(set_attr "type"	"dspalu")
   (set_attr "mode"	"SI")])

(define_insn "mips_addqh_ph"
  [(set (match_operand:V2HI 0 "register_operand" "=d")
	(unspec:V2HI [(match_operand:V2HI 1 "reg_or_0_operand" "dYG")
		      (match_operand:V2HI 2 "reg_or_0_operand" "dYG")]
		     UNSPEC_ADDQH_PH))]
  "ISA_HAS_DSPR2"
  "addqh.ph\t%0,%z1,%z2"
  [(set_attr "type"	"dspalu")
   (set_attr "mode"	"SI")])

(define_insn "mips_addqh_r_ph"
  [(set (match_operand:V2HI 0 "register_operand" "=d")
	(unspec:V2HI [(match_operand:V2HI 1 "reg_or_0_operand" "dYG")
		      (match_operand:V2HI 2 "reg_or_0_operand" "dYG")]
		     UNSPEC_ADDQH_R_PH))]
  "ISA_HAS_DSPR2"
  "addqh_r.ph\t%0,%z1,%z2"
  [(set_attr "type"	"dspalu")
   (set_attr "mode"	"SI")])

(define_insn "mips_addqh_w"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(unspec:SI [(match_operand:SI 1 "reg_or_0_operand" "dJ")
		    (match_operand:SI 2 "reg_or_0_operand" "dJ")]
		   UNSPEC_ADDQH_W))]
  "ISA_HAS_DSPR2"
  "addqh.w\t%0,%z1,%z2"
  [(set_attr "type"	"dspalu")
   (set_attr "mode"	"SI")])

(define_insn "mips_addqh_r_w"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(unspec:SI [(match_operand:SI 1 "reg_or_0_operand" "dJ")
		    (match_operand:SI 2 "reg_or_0_operand" "dJ")]
		   UNSPEC_ADDQH_R_W))]
  "ISA_HAS_DSPR2"
  "addqh_r.w\t%0,%z1,%z2"
  [(set_attr "type"	"dspalu")
   (set_attr "mode"	"SI")])

(define_insn "mips_subqh_ph"
  [(set (match_operand:V2HI 0 "register_operand" "=d")
	(unspec:V2HI [(match_operand:V2HI 1 "reg_or_0_operand" "dYG")
		      (match_operand:V2HI 2 "reg_or_0_operand" "dYG")]
		     UNSPEC_SUBQH_PH))]
  "ISA_HAS_DSPR2"
  "subqh.ph\t%0,%z1,%z2"
  [(set_attr "type"	"dspalu")
   (set_attr "mode"	"SI")])

(define_insn "mips_subqh_r_ph"
  [(set (match_operand:V2HI 0 "register_operand" "=d")
	(unspec:V2HI [(match_operand:V2HI 1 "reg_or_0_operand" "dYG")
		      (match_operand:V2HI 2 "reg_or_0_operand" "dYG")]
		     UNSPEC_SUBQH_R_PH))]
  "ISA_HAS_DSPR2"
  "subqh_r.ph\t%0,%z1,%z2"
  [(set_attr "type"	"dspalu")
   (set_attr "mode"	"SI")])

(define_insn "mips_subqh_w"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(unspec:SI [(match_operand:SI 1 "reg_or_0_operand" "dJ")
		    (match_operand:SI 2 "reg_or_0_operand" "dJ")]
		   UNSPEC_SUBQH_W))]
  "ISA_HAS_DSPR2"
  "subqh.w\t%0,%z1,%z2"
  [(set_attr "type"	"dspalu")
   (set_attr "mode"	"SI")])

(define_insn "mips_subqh_r_w"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(unspec:SI [(match_operand:SI 1 "reg_or_0_operand" "dJ")
		    (match_operand:SI 2 "reg_or_0_operand" "dJ")]
		   UNSPEC_SUBQH_R_W))]
  "ISA_HAS_DSPR2"
  "subqh_r.w\t%0,%z1,%z2"
  [(set_attr "type"	"dspalu")
   (set_attr "mode"	"SI")])

(define_insn "mips_dpax_w_ph"
  [(set (match_operand:DI 0 "register_operand" "=a")
	(unspec:DI [(match_operand:DI 1 "register_operand" "0")
		    (match_operand:V2HI 2 "reg_or_0_operand" "dYG")
		    (match_operand:V2HI 3 "reg_or_0_operand" "dYG")]
		   UNSPEC_DPAX_W_PH))]
  "ISA_HAS_DSPR2 && !TARGET_64BIT"
  "dpax.w.ph\t%q0,%z2,%z3"
  [(set_attr "type"	"dspmac")
   (set_attr "accum_in" "1")
   (set_attr "mode"	"SI")])

(define_insn "mips_dpsx_w_ph"
  [(set (match_operand:DI 0 "register_operand" "=a")
	(unspec:DI [(match_operand:DI 1 "register_operand" "0")
		    (match_operand:V2HI 2 "reg_or_0_operand" "dYG")
		    (match_operand:V2HI 3 "reg_or_0_operand" "dYG")]
		   UNSPEC_DPSX_W_PH))]
  "ISA_HAS_DSPR2 && !TARGET_64BIT"
  "dpsx.w.ph\t%q0,%z2,%z3"
  [(set_attr "type"	"dspmac")
   (set_attr "accum_in" "1")
   (set_attr "mode"	"SI")])

(define_insn "mips_dpaqx_s_w_ph"
  [(set (match_operand:DI 0 "register_operand" "=a")
	(unspec:DI [(match_operand:DI 1 "register_operand" "0")
		    (match_operand:V2HI 2 "reg_or_0_operand" "dYG")
		    (match_operand:V2HI 3 "reg_or_0_operand" "dYG")]
		   UNSPEC_DPAQX_S_W_PH))
   (set (reg:CCDSP CCDSP_OU_REGNUM)
	(unspec:CCDSP [(match_dup 1) (match_dup 2) (match_dup 3)]
		      UNSPEC_DPAQX_S_W_PH))]
  "ISA_HAS_DSPR2 && !TARGET_64BIT"
  "dpaqx_s.w.ph\t%q0,%z2,%z3"
  [(set_attr "type"	"dspmac")
   (set_attr "accum_in" "1")
   (set_attr "mode"	"SI")])

(define_insn "mips_dpaqx_sa_w_ph"
  [(set (match_operand:DI 0 "register_operand" "=a")
	(unspec:DI [(match_operand:DI 1 "register_operand" "0")
		    (match_operand:V2HI 2 "reg_or_0_operand" "dYG")
		    (match_operand:V2HI 3 "reg_or_0_operand" "dYG")]
		   UNSPEC_DPAQX_SA_W_PH))
   (set (reg:CCDSP CCDSP_OU_REGNUM)
	(unspec:CCDSP [(match_dup 1) (match_dup 2) (match_dup 3)]
		      UNSPEC_DPAQX_SA_W_PH))]
  "ISA_HAS_DSPR2 && !TARGET_64BIT"
  "dpaqx_sa.w.ph\t%q0,%z2,%z3"
  [(set_attr "type"	"dspmacsat")
   (set_attr "accum_in" "1")
   (set_attr "mode"	"SI")])

(define_insn "mips_dpsqx_s_w_ph"
  [(set (match_operand:DI 0 "register_operand" "=a")
	(unspec:DI [(match_operand:DI 1 "register_operand" "0")
		    (match_operand:V2HI 2 "reg_or_0_operand" "dYG")
		    (match_operand:V2HI 3 "reg_or_0_operand" "dYG")]
		   UNSPEC_DPSQX_S_W_PH))
   (set (reg:CCDSP CCDSP_OU_REGNUM)
	(unspec:CCDSP [(match_dup 1) (match_dup 2) (match_dup 3)]
		      UNSPEC_DPSQX_S_W_PH))]
  "ISA_HAS_DSPR2 && !TARGET_64BIT"
  "dpsqx_s.w.ph\t%q0,%z2,%z3"
  [(set_attr "type"	"dspmac")
   (set_attr "accum_in" "1")
   (set_attr "mode"	"SI")])

(define_insn "mips_dpsqx_sa_w_ph"
  [(set (match_operand:DI 0 "register_operand" "=a")
	(unspec:DI [(match_operand:DI 1 "register_operand" "0")
		    (match_operand:V2HI 2 "reg_or_0_operand" "dYG")
		    (match_operand:V2HI 3 "reg_or_0_operand" "dYG")]
		   UNSPEC_DPSQX_SA_W_PH))
   (set (reg:CCDSP CCDSP_OU_REGNUM)
	(unspec:CCDSP [(match_dup 1) (match_dup 2) (match_dup 3)]
		      UNSPEC_DPSQX_SA_W_PH))]
  "ISA_HAS_DSPR2 && !TARGET_64BIT"
  "dpsqx_sa.w.ph\t%q0,%z2,%z3"
  [(set_attr "type"	"dspmacsat")
   (set_attr "accum_in" "1")
   (set_attr "mode"	"SI")])
