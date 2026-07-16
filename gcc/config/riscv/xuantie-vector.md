;; Machine description for XuanTie vector extensions.
;; Copyright (C) 2026 Free Software Foundation, Inc.
;; Contributed by Alibaba DAMO Academy XuanTie Team.

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
  ; XXTVarith
  UNSPEC_XT_VGMULXOR
  UNSPEC_XT_VILO
  UNSPEC_XT_VILE
  UNSPEC_XT_VCRCFOLDR
  UNSPEC_XT_VCRCFOLDN
])

;; XXTVarith
(define_int_iterator UNSPEC_XT_VGMULXOR_IT [
  UNSPEC_XT_VGMULXOR
])

(define_int_iterator UNSPEC_XT_VIL_IT [
  UNSPEC_XT_VILO
  UNSPEC_XT_VILE
])

(define_int_iterator UNSPEC_XT_VCRCFOLD_IT [
  UNSPEC_XT_VCRCFOLDR
  UNSPEC_XT_VCRCFOLDN
])

(define_int_attr xxtvarith_ins_name [
  (UNSPEC_XT_VGMULXOR "vgmulxor")
  (UNSPEC_XT_VILO "vilo") (UNSPEC_XT_VILE "vile")
  (UNSPEC_XT_VCRCFOLDR "vcrcfoldr") (UNSPEC_XT_VCRCFOLDN "vcrcfoldn")
])

(define_mode_iterator VI_Q [
  RVVM8QI RVVM4QI RVVM2QI RVVM1QI RVVMF2QI RVVMF4QI
  (RVVMF8QI "TARGET_MIN_VLEN > 32")
])

(define_insn "@xt_pred_<xxtvarith_ins_name><mode>"
  [(set (match_operand:VI_Q 0 "register_operand"    "=vr")
     (if_then_else:VI_Q
       (unspec:<VM>
	 [(match_operand 4 "vector_length_operand" " rK")
	  (match_operand 5 "const_int_operand"     "  i")
	  (match_operand 6 "const_int_operand"     "  i")
	  (reg:SI VL_REGNUM)
	  (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
       (unspec:VI_Q
	 [(match_operand:VI_Q 1 "register_operand" " 0")
	  (match_operand:VI_Q 2 "register_operand" "vr")
	  (match_operand:VI_Q 3 "register_operand" "vr")] UNSPEC_XT_VGMULXOR_IT)
       (match_dup 1)))]
  "TARGET_VECTOR && TARGET_XXTVARITH"
  "xt.<xxtvarith_ins_name>.vv\t%0,%2,%3"
  [(set_attr "type" "xt_vgmulxor")
   (set_attr "mode" "<MODE>")])

(define_insn "@xt_pred_<xxtvarith_ins_name><mode>"
  [(set (match_operand:V_VLS 0 "register_operand"	   "=vr, vr")
    (if_then_else:V_VLS
      (unspec:<VM>
	[(match_operand 4 "vector_length_operand"    " rK, rK")
	(match_operand 5 "const_int_operand"	" i,  i")
	(match_operand 6 "const_int_operand"	" i,  i")
	(reg:SI VL_REGNUM)
	(reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
      (unspec:V_VLS
	[(match_operand:V_VLS 2 "register_operand" "vr,vr")
	 (match_operand:V_VLS 3 "register_operand" "vr,vr")] UNSPEC_XT_VIL_IT)
      (match_operand:V_VLS 1 "vector_merge_operand"     "vu,0")))]
  "TARGET_VECTOR && TARGET_XXTVARITH"
  "xt.<xxtvarith_ins_name>.vv\t%0,%2,%3"
  [(set_attr "type" "xt_<xxtvarith_ins_name>")
   (set_attr "mode" "<MODE>")])

(define_insn "@xt_pred_<xxtvarith_ins_name><mode>"
  [(set (match_operand:VI_D 0 "register_operand"     "=vr")
     (if_then_else:VI_D
       (unspec:<VM>
	 [(match_operand 4 "vector_length_operand"     "rK")
	  (match_operand 5 "const_int_operand"	 " i")
	  (match_operand 6 "const_int_operand"	 " i")
	  (reg:SI VL_REGNUM)
	  (reg:SI VTYPE_REGNUM)] UNSPEC_VPREDICATE)
       (unspec:VI_D
	  [(match_operand:VI_D 1 "register_operand" " 0")
	   (match_operand:VI_D 2 "register_operand" "vr")
	   (match_operand:VI_D 3 "register_operand" "vr")]
	  UNSPEC_XT_VCRCFOLD_IT)
       (match_dup 1)))]
  "TARGET_VECTOR && TARGET_XXTVARITH"
  "xt.<xxtvarith_ins_name>.vv\t%0,%2,%3"
  [(set_attr "type" "xt_vcrcfold")
   (set_attr "mode" "<MODE>")])
