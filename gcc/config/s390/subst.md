;;- Machine description for GNU compiler -- S/390 / zSeries version.
;;  Subst patterns.
;;  Copyright (C) 2016-2020 Free Software Foundation, Inc.
;;  Contributed by Andreas Krebbel (Andreas.Krebbel@de.ibm.com)

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later
;; version.

;; GCC is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

(define_code_iterator SUBST [rotate ashift lshiftrt ashiftrt])
(define_mode_iterator DSI_VI [SI DI V2QI V4QI V8QI V16QI V2HI V4HI V8HI V2SI V4SI V2DI])

; This adds an explicit CC reg set to an operation while keeping the
; set for the operation result as well.
(define_subst "setcc_subst"
  [(set (match_operand:DSI 0 ""           "")
        (match_operand:DSI 1 "" ""))
   (clobber (reg:CC CC_REGNUM))]
  "s390_match_ccmode(insn, CCSmode)"
  [(set (reg CC_REGNUM)
	(compare (match_dup 1) (const_int 0)))
   (set (match_dup 0) (match_dup 1))])

; Use this in the insn name.
(define_subst_attr "setcc" "setcc_subst" "" "_cc")

; This adds an explicit CC reg set to an operation while dropping the
; result of the operation.
(define_subst "cconly_subst"
  [(set (match_operand:DSI 0 ""           "")
        (match_operand:DSI 1 "" ""))
   (clobber (reg:CC CC_REGNUM))]
  "s390_match_ccmode(insn, CCSmode)"
  [(set (reg CC_REGNUM)
	(compare (match_dup 1) (const_int 0)))
   (clobber (match_scratch:DSI 0 "=d,d"))])

(define_subst_attr "cconly" "cconly_subst" "" "_cconly")


; Does transformations to switch between patterns unsing risbg +
; clobber CC (z10) and risbgn without clobber (zEC12).
(define_subst "clobbercc_or_nocc_subst"
  [(set (match_operand 0 "" "") (match_operand 1 "" ""))]
  ""
  [(set (match_dup 0) (match_dup 1))
   (clobber (reg:CC CC_REGNUM))])

; Use this in the insn name to add the target suffix.
(define_subst_attr "clobbercc_or_nocc" "clobbercc_or_nocc_subst"
  "_nocc" "_clobbercc")

; Use this in the condition.
(define_subst_attr "z10_or_zEC12_cond" "clobbercc_or_nocc_subst"
  "TARGET_ZEC12" "TARGET_Z10 && ! TARGET_ZEC12")

; Use this instead of the risbg instruction.
(define_subst_attr "risbg_n" "clobbercc_or_nocc_subst"
  "risbgn" "risbg")
