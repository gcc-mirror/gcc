;; Machine description for AArch64 SVE2.
;; Copyright (C) 2019 Free Software Foundation, Inc.
;; Contributed by ARM Ltd.
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

;; Integer average (floor).
(define_expand "<u>avg<mode>3_floor"
  [(set (match_operand:SVE_I 0 "register_operand")
	(unspec:SVE_I
	  [(match_dup 3)
	   (unspec:SVE_I [(match_operand:SVE_I 1 "register_operand")
			  (match_operand:SVE_I 2 "register_operand")]
			 HADD)]
	  UNSPEC_MERGE_PTRUE))]
  "TARGET_SVE2"
  {
    operands[3] = force_reg (<VPRED>mode, CONSTM1_RTX (<VPRED>mode));
  }
)

;; Integer average (rounding).
(define_expand "<u>avg<mode>3_ceil"
  [(set (match_operand:SVE_I 0 "register_operand")
	(unspec:SVE_I
	  [(match_dup 3)
	   (unspec:SVE_I [(match_operand:SVE_I 1 "register_operand")
			  (match_operand:SVE_I 2 "register_operand")]
			 RHADD)]
	  UNSPEC_MERGE_PTRUE))]
  "TARGET_SVE2"
  {
    operands[3] = force_reg (<VPRED>mode, CONSTM1_RTX (<VPRED>mode));
  }
)

;; Predicated halving addsub.
(define_insn "*<sur>h<addsub><mode>"
  [(set (match_operand:SVE_I 0 "register_operand" "=w, ?&w")
	(unspec:SVE_I
	  [(match_operand:<VPRED> 1 "register_operand" "Upl, Upl")
	   (unspec:SVE_I [(match_operand:SVE_I 2 "register_operand" "%0, w")
			  (match_operand:SVE_I 3 "register_operand" "w, w")]
			 HADDSUB)]
	  UNSPEC_MERGE_PTRUE))]
  "TARGET_SVE2"
  "@
   <sur>h<addsub>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>
   movprfx\t%0, %2\;<sur>h<addsub>\t%0.<Vetype>, %1/m, %0.<Vetype>, %3.<Vetype>"
  [(set_attr "movprfx" "*,yes")]
)