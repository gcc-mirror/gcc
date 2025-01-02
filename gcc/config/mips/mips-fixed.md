;; Copyright (C) 2007-2025 Free Software Foundation, Inc.
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
;; This file contains MIPS instructions that support fixed-point operations.

;; All supported fixed-point modes
(define_mode_iterator FIXED [(QQ "") (HQ "") (SQ "") (DQ "TARGET_64BIT")
			     (UQQ "") (UHQ "") (USQ "") (UDQ "TARGET_64BIT")
			     (HA "") (SA "") (DA "TARGET_64BIT")
			     (UHA "") (USA "") (UDA "TARGET_64BIT")])

;; For signed add/sub with saturation
(define_mode_iterator ADDSUB [(HQ "") (SQ "") (HA "") (SA "") (V2HQ "")
			      (V2HA "")])
(define_mode_attr addsubfmt [(HQ "ph") (SQ "w") (HA "ph") (SA "w")
			     (V2HQ "ph") (V2HA "ph")])

;; For unsigned add/sub with saturation
(define_mode_iterator UADDSUB [(UQQ "ISA_HAS_DSP") (UHQ "ISA_HAS_DSPR2")
			       (UHA "ISA_HAS_DSPR2") (V4UQQ "ISA_HAS_DSP")
			       (V2UHQ "ISA_HAS_DSPR2") (V2UHA "ISA_HAS_DSPR2")])
(define_mode_attr uaddsubfmt [(UQQ "qb") (UHQ "ph") (UHA "ph")
			      (V4UQQ "qb") (V2UHQ "ph") (V2UHA "ph")])

;; For signed multiplication with saturation
(define_mode_iterator MULQ [(V2HQ "ISA_HAS_DSP") (HQ "ISA_HAS_DSP")
			    (SQ "ISA_HAS_DSPR2")])
(define_mode_attr mulqfmt [(V2HQ "ph") (HQ "ph") (SQ "w")])

(define_insn "add<mode>3"
  [(set (match_operand:FIXED 0 "register_operand" "=d")
	(plus:FIXED (match_operand:FIXED 1 "register_operand" "d")
		    (match_operand:FIXED 2 "register_operand" "d")))]
  ""
  "<d>addu\t%0,%1,%2"
  [(set_attr "type" "arith")
   (set_attr "mode" "<IMODE>")])

(define_insn "usadd<mode>3"
  [(set (match_operand:UADDSUB 0 "register_operand" "=d")
	(us_plus:UADDSUB (match_operand:UADDSUB 1 "register_operand" "d")
			 (match_operand:UADDSUB 2 "register_operand" "d")))
   (set (reg:CCDSP CCDSP_OU_REGNUM)
	(unspec:CCDSP [(match_dup 1) (match_dup 2)] UNSPEC_ADDQ_S))]
  ""
  "addu_s.<uaddsubfmt>\t%0,%1,%2"
  [(set_attr "type" "arith")
   (set_attr "mode" "<IMODE>")])

(define_insn "ssadd<mode>3"
  [(set (match_operand:ADDSUB 0 "register_operand" "=d")
	(ss_plus:ADDSUB (match_operand:ADDSUB 1 "register_operand" "d")
			(match_operand:ADDSUB 2 "register_operand" "d")))
   (set (reg:CCDSP CCDSP_OU_REGNUM)
	(unspec:CCDSP [(match_dup 1) (match_dup 2)] UNSPEC_ADDQ_S))]
  "ISA_HAS_DSP"
  "addq_s.<addsubfmt>\t%0,%1,%2"
  [(set_attr "type" "arith")
   (set_attr "mode" "<IMODE>")])

(define_insn "sub<mode>3"
  [(set (match_operand:FIXED 0 "register_operand" "=d")
        (minus:FIXED (match_operand:FIXED 1 "register_operand" "d")
		     (match_operand:FIXED 2 "register_operand" "d")))]
  ""
  "<d>subu\t%0,%1,%2"
  [(set_attr "type" "arith")
   (set_attr "mode" "<IMODE>")])

(define_insn "ussub<mode>3"
  [(set (match_operand:UADDSUB 0 "register_operand" "=d")
	(us_minus:UADDSUB (match_operand:UADDSUB 1 "register_operand" "d")
			  (match_operand:UADDSUB 2 "register_operand" "d")))
   (set (reg:CCDSP CCDSP_OU_REGNUM)
	(unspec:CCDSP [(match_dup 1) (match_dup 2)] UNSPEC_SUBQ_S))]
  ""
  "subu_s.<uaddsubfmt>\t%0,%1,%2"
  [(set_attr "type" "arith")
   (set_attr "mode" "<IMODE>")])

(define_insn "sssub<mode>3"
  [(set (match_operand:ADDSUB 0 "register_operand" "=d")
	(ss_minus:ADDSUB (match_operand:ADDSUB 1 "register_operand" "d")
			 (match_operand:ADDSUB 2 "register_operand" "d")))
   (set (reg:CCDSP CCDSP_OU_REGNUM)
	(unspec:CCDSP [(match_dup 1) (match_dup 2)] UNSPEC_SUBQ_S))]
  "ISA_HAS_DSP"
  "subq_s.<addsubfmt>\t%0,%1,%2"
  [(set_attr "type" "arith")
   (set_attr "mode" "<IMODE>")])

(define_insn "ssmul<mode>3"
  [(set (match_operand:MULQ 0 "register_operand" "=d")
	(ss_mult:MULQ (match_operand:MULQ 1 "register_operand" "d")
		      (match_operand:MULQ 2 "register_operand" "d")))
   (set (reg:CCDSP CCDSP_OU_REGNUM)
        (unspec:CCDSP [(match_dup 1) (match_dup 2)] UNSPEC_MULQ_RS_PH))
   (clobber (match_scratch:DI 3 "=x"))]
  ""
  "mulq_rs.<mulqfmt>\t%0,%1,%2"
  [(set_attr "type"     "imul3")
   (set_attr "mode"     "<IMODE>")])

(define_insn "ssmaddsqdq4"
  [(set (match_operand:DQ 0 "register_operand" "=a")
	(ss_plus:DQ
	(ss_mult:DQ (sat_fract:DQ (match_operand:SQ 1
				   "register_operand" "d"))
                    (sat_fract:DQ (match_operand:SQ 2
				   "register_operand" "d")))
        (match_operand:DQ 3 "register_operand" "0")))
   (set (reg:CCDSP CCDSP_OU_REGNUM)
	(unspec:CCDSP [(match_dup 1) (match_dup 2) (match_dup 3)]
		      UNSPEC_DPAQ_SA_L_W))]
  "ISA_HAS_DSP && !TARGET_64BIT"
  "dpaq_sa.l.w\t%q0,%1,%2"
  [(set_attr "type" "imadd")
   (set_attr "mode" "SI")])

(define_insn "ssmsubsqdq4"
  [(set (match_operand:DQ 0 "register_operand" "=a")
	(ss_minus:DQ
	 (match_operand:DQ 3 "register_operand" "0")
         (ss_mult:DQ (sat_fract:DQ (match_operand:SQ 1
				    "register_operand" "d"))
                     (sat_fract:DQ (match_operand:SQ 2
				    "register_operand" "d")))))
   (set (reg:CCDSP CCDSP_OU_REGNUM)
	(unspec:CCDSP [(match_dup 1) (match_dup 2) (match_dup 3)]
		      UNSPEC_DPSQ_SA_L_W))]
  "ISA_HAS_DSP && !TARGET_64BIT"
  "dpsq_sa.l.w\t%q0,%1,%2"
  [(set_attr "type" "imadd")
   (set_attr "mode" "SI")])
