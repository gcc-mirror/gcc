;; Machine description for AArch64 SME.
;; Copyright (C) 2023 Free Software Foundation, Inc.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; GCC is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;; The file is organised into the following sections (search for the full
;; line):
;;
;; == State management
;; ---- Test current state
;; ---- PSTATE.SM management

;; =========================================================================
;; == State management
;; =========================================================================
;;
;; Many of the instructions in this section are only valid when SME is
;; present.  However, they don't have a TARGET_SME condition since
;; (a) they are only emitted under direct control of aarch64 code and
;; (b) they are sometimes used conditionally, particularly in streaming-
;; compatible code.
;;
;; =========================================================================

;; -------------------------------------------------------------------------
;; ---- Test current state
;; -------------------------------------------------------------------------

(define_c_enum "unspec" [
  UNSPEC_OLD_VG_SAVED
  UNSPEC_UPDATE_VG
  UNSPEC_GET_SME_STATE
  UNSPEC_READ_SVCR
])

;; A marker instruction to say that the old value of the DWARF VG register
;; has been saved to the stack, for CFI purposes.  Operand 0 is the old
;; value of the register and operand 1 is the save slot.
(define_insn "aarch64_old_vg_saved"
  [(set (reg:DI VG_REGNUM)
	(unspec:DI [(match_operand 0)
		    (match_operand 1)] UNSPEC_OLD_VG_SAVED))]
  ""
  ""
  [(set_attr "type" "no_insn")]
)

;; A marker to indicate places where a call temporarily changes VG.
(define_insn "aarch64_update_vg"
  [(set (reg:DI VG_REGNUM)
	(unspec:DI [(reg:DI VG_REGNUM)] UNSPEC_UPDATE_VG))]
  ""
  ""
  [(set_attr "type" "no_insn")]
)

(define_insn "aarch64_get_sme_state"
  [(set (reg:TI R0_REGNUM)
	(unspec_volatile:TI [(const_int 0)] UNSPEC_GET_SME_STATE))
   (clobber (reg:DI R16_REGNUM))
   (clobber (reg:DI R17_REGNUM))
   (clobber (reg:DI R18_REGNUM))
   (clobber (reg:DI R30_REGNUM))
   (clobber (reg:CC CC_REGNUM))]
  ""
  "bl\t__arm_sme_state"
)

(define_insn "aarch64_read_svcr"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(unspec_volatile:DI [(const_int 0)] UNSPEC_READ_SVCR))]
  ""
  "mrs\t%0, svcr"
)

;; -------------------------------------------------------------------------
;; ---- PSTATE.SM management
;; -------------------------------------------------------------------------
;; Includes:
;; - SMSTART SM
;; - SMSTOP SM
;; -------------------------------------------------------------------------

(define_c_enum "unspec" [
  UNSPEC_SMSTART_SM
  UNSPEC_SMSTOP_SM
])

;; Turn on streaming mode.  This clobbers all SVE state.
;;
;; Depend on VG_REGNUM to ensure that the VG save slot has already been
;; initialized.
(define_insn "aarch64_smstart_sm"
  [(unspec_volatile [(const_int 0)] UNSPEC_SMSTART_SM)
   (use (reg:DI VG_REGNUM))
   (clobber (reg:V4x16QI V0_REGNUM))
   (clobber (reg:V4x16QI V4_REGNUM))
   (clobber (reg:V4x16QI V8_REGNUM))
   (clobber (reg:V4x16QI V12_REGNUM))
   (clobber (reg:V4x16QI V16_REGNUM))
   (clobber (reg:V4x16QI V20_REGNUM))
   (clobber (reg:V4x16QI V24_REGNUM))
   (clobber (reg:V4x16QI V28_REGNUM))
   (clobber (reg:VNx16BI P0_REGNUM))
   (clobber (reg:VNx16BI P1_REGNUM))
   (clobber (reg:VNx16BI P2_REGNUM))
   (clobber (reg:VNx16BI P3_REGNUM))
   (clobber (reg:VNx16BI P4_REGNUM))
   (clobber (reg:VNx16BI P5_REGNUM))
   (clobber (reg:VNx16BI P6_REGNUM))
   (clobber (reg:VNx16BI P7_REGNUM))
   (clobber (reg:VNx16BI P8_REGNUM))
   (clobber (reg:VNx16BI P9_REGNUM))
   (clobber (reg:VNx16BI P10_REGNUM))
   (clobber (reg:VNx16BI P11_REGNUM))
   (clobber (reg:VNx16BI P12_REGNUM))
   (clobber (reg:VNx16BI P13_REGNUM))
   (clobber (reg:VNx16BI P14_REGNUM))
   (clobber (reg:VNx16BI P15_REGNUM))]
  ""
  "smstart\tsm"
)

;; Turn off streaming mode.  This clobbers all SVE state.
;;
;; Depend on VG_REGNUM to ensure that the VG save slot has already been
;; initialized.
(define_insn "aarch64_smstop_sm"
  [(unspec_volatile [(const_int 0)] UNSPEC_SMSTOP_SM)
   (use (reg:DI VG_REGNUM))
   (clobber (reg:V4x16QI V0_REGNUM))
   (clobber (reg:V4x16QI V4_REGNUM))
   (clobber (reg:V4x16QI V8_REGNUM))
   (clobber (reg:V4x16QI V12_REGNUM))
   (clobber (reg:V4x16QI V16_REGNUM))
   (clobber (reg:V4x16QI V20_REGNUM))
   (clobber (reg:V4x16QI V24_REGNUM))
   (clobber (reg:V4x16QI V28_REGNUM))
   (clobber (reg:VNx16BI P0_REGNUM))
   (clobber (reg:VNx16BI P1_REGNUM))
   (clobber (reg:VNx16BI P2_REGNUM))
   (clobber (reg:VNx16BI P3_REGNUM))
   (clobber (reg:VNx16BI P4_REGNUM))
   (clobber (reg:VNx16BI P5_REGNUM))
   (clobber (reg:VNx16BI P6_REGNUM))
   (clobber (reg:VNx16BI P7_REGNUM))
   (clobber (reg:VNx16BI P8_REGNUM))
   (clobber (reg:VNx16BI P9_REGNUM))
   (clobber (reg:VNx16BI P10_REGNUM))
   (clobber (reg:VNx16BI P11_REGNUM))
   (clobber (reg:VNx16BI P12_REGNUM))
   (clobber (reg:VNx16BI P13_REGNUM))
   (clobber (reg:VNx16BI P14_REGNUM))
   (clobber (reg:VNx16BI P15_REGNUM))]
  ""
  "smstop\tsm"
)
