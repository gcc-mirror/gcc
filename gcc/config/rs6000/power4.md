;; Scheduling description for IBM Power4 and PowerPC 970 processors.
;;   Copyright (C) 2003 Free Software Foundation, Inc.
;;
;; This file is part of GNU CC.
;;
;; GNU CC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; GNU CC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU CC; see the file COPYING.  If not, write to
;; the Free Software Foundation, 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; Sources: IBM Red Book and White Paper on POWER4

;; The POWER4 has 2 iu, 2 fpu, 2 lsu per engine (2 engines per chip).
;; Instructions that update more than one register get broken into two
;; (split) or more internal ops.  The chip can issue up to 5
;; internal ops per cycle.

(define_automaton "power4iu,power4lsu,power4fpu,power4misc,power4vec,power4disp")

(define_cpu_unit "iu1_power4,iu2_power4" "power4iu")
(define_cpu_unit "lsu1_power4,lsu2_power4" "power4lsu")
(define_cpu_unit "fpu1_power4,fpu2_power4" "power4fpu")
(define_cpu_unit "bpu_power4,cru_power4" "power4misc")
(define_cpu_unit "vec_power4,vecperm_power4" "power4vec")
(define_cpu_unit "du1_power4,du2_power4,du3_power4,du4_power4,du5_power4"
		 "power4disp")

(define_reservation "q1_power4" "du1_power4|du4_power4")
(define_reservation "q2_power4" "du2_power4|du3_power4")

(define_reservation "lsq_power4" "((du1_power4|du4_power4),lsu1_power4)\
				 |((du2_power4|du3_power4),lsu2_power4)")

(define_reservation "lsuq_power4"
		    "((du1_power4+du2_power4),lsu1_power4+iu2_power4)\
		    |((du2_power4+du3_power4),lsu2_power4+iu2_power4)\
		    |((du3_power4+du4_power4),lsu2_power4+iu1_power4)")
;;;		    |((du2_power4+du3_power4),lsu2_power4,iu2_power4)

(define_reservation "lsuxq_power4"
		    "(du1_power4+du2_power4+du3_power4+du4_power4),\
		     iu1_power4,(lsu2_power4+iu2_power4)")

(define_reservation "iq_power4" "((du1_power4|du4_power4),iu1_power4)\
				|((du2_power4|du3_power4),iu2_power4)")

(define_reservation "fpq_power4" "((du1_power4|du4_power4),fpu1_power4)\
				 |((du2_power4|du3_power4),fpu2_power4)")

(define_reservation "vq_power4"
		    "(du1_power4|du2_power4|du3_power4|du4_power4),vec_power4")
(define_reservation "vpq_power4"
		    "(du1_power4|du2_power4|du3_power4|du4_power4),\
		     vecperm_power4")


; Dispatch slots are allocated in order conforming to program order.
(absence_set "du1_power4" "du2_power4,du3_power4,du4_power4,du5_power4")
(absence_set "du2_power4" "du3_power4,du4_power4,du5_power4")
(absence_set "du3_power4" "du4_power4,du5_power4")
(absence_set "du4_power4" "du5_power4")


; Load/store
(define_insn_reservation "power4-load" 3
  (and (eq_attr "type" "load")
       (eq_attr "cpu" "power4"))
  "lsq_power4")

(define_insn_reservation "power4-load-ext" 5
  (and (eq_attr "type" "load_ext")
       (eq_attr "cpu" "power4"))
  "((du1_power4+du2_power4),lsu1_power4,nothing,nothing,iu2_power4)\
  |((du2_power4+du3_power4),lsu2_power4,nothing,nothing,iu2_power4)\
  |((du3_power4+du4_power4),lsu2_power4,nothing,nothing,iu1_power4)")

(define_insn_reservation "power4-load-ext-update" 5
  (and (eq_attr "type" "load_ext_u")
       (eq_attr "cpu" "power4"))
  "(du1_power4+du2_power4+du3_power4+du4_power4),\
   (lsu1_power4+iu2_power4),nothing,nothing,iu2_power4")

(define_insn_reservation "power4-load-ext-update-indexed" 5
  (and (eq_attr "type" "load_ext_ux")
       (eq_attr "cpu" "power4"))
  "(du1_power4+du2_power4+du3_power4+du4_power4),\
   iu1_power4,(lsu2_power4+iu1_power4),nothing,nothing,iu2_power4")

(define_insn_reservation "power4-load-update-indexed" 3
  (and (eq_attr "type" "load_ux")
       (eq_attr "cpu" "power4"))
  "lsuxq_power4")

(define_insn_reservation "power4-load-update" 3
  (and (eq_attr "type" "load_u")
       (eq_attr "cpu" "power4"))
  "lsuq_power4")

(define_insn_reservation "power4-fpload" 5
  (and (eq_attr "type" "fpload")
       (eq_attr "cpu" "power4"))
  "lsq_power4")

(define_insn_reservation "power4-fpload-update" 5
  (and (eq_attr "type" "fpload_u")
       (eq_attr "cpu" "power4"))
  "lsuq_power4")

(define_insn_reservation "power4-fpload-update-indexed" 5
  (and (eq_attr "type" "fpload_ux")
       (eq_attr "cpu" "power4"))
  "lsuxq_power4")

(define_insn_reservation "power4-vecload" 5
  (and (eq_attr "type" "vecload")
       (eq_attr "cpu" "power4"))
  "lsq_power4")

(define_insn_reservation "power4-store" 1
  (and (eq_attr "type" "store")
       (eq_attr "cpu" "power4"))
  "((du1_power4|du4_power4),lsu1_power4,iu2_power4)\
  |((du2_power4|du3_power4),lsu2_power4,iu1_power4)")

(define_insn_reservation "power4-store-update" 1
  (and (eq_attr "type" "store_u")
       (eq_attr "cpu" "power4"))
  "lsuq_power4")

(define_insn_reservation "power4-store-update-indexed" 1
  (and (eq_attr "type" "store_ux")
       (eq_attr "cpu" "power4"))
  "lsuxq_power4")

(define_insn_reservation "power4-fpstore" 1
  (and (eq_attr "type" "fpstore")
       (eq_attr "cpu" "power4"))
  "((du1_power4|du4_power4),lsu1_power4,fpu1_power4)\
  |((du2_power4|du3_power4),lsu2_power4,fpu2_power4)")

(define_insn_reservation "power4-fpstore-update" 1
  (and (eq_attr "type" "fpstore_u")
       (eq_attr "cpu" "power4"))
  "((du1_power4+du2_power4),(fpu1_power4+iu2_power4),lsu1_power4)\
  |((du2_power4+du3_power4),(fpu2_power4+iu2_power4),lsu2_power4)\
  |((du3_power4+du4_power4),(fpu2_power4+iu1_power4),lsu2_power4)")
;;;((du2_power4+du3_power4),fpu2_power4,(iu2_power4+lsu2_power4))

(define_insn_reservation "power4-fpstore-update-indexed" 1
  (and (eq_attr "type" "fpstore_ux")
       (eq_attr "cpu" "power4"))
  "(du1_power4+du2_power4+du3_power4+du4_power4),
   iu1_power4,fpu2_power4,(iu2_power4+lsu2_power4)")

(define_insn_reservation "power4-vecstore" 1
  (and (eq_attr "type" "vecstore")
       (eq_attr "cpu" "power4"))
  "((du1_power4|du4_power4),lsu1_power4,vec_power4)\
  |((du2_power4|du3_power4),lsu2_power4,vec_power4)")


; Integer latency is 2 cycles
(define_insn_reservation "power4-integer" 2
  (and (eq_attr "type" "integer")
       (eq_attr "cpu" "power4"))
  "iq_power4")

(define_insn_reservation "power4-cmp" 3
  (and (eq_attr "type" "cmp,fast_compare")
       (eq_attr "cpu" "power4"))
  "iq_power4")

(define_insn_reservation "power4-compare" 4
  (and (eq_attr "type" "compare,delayed_compare")
       (eq_attr "cpu" "power4"))
  "((du1_power4+du2_power4),iu1_power4,iu2_power4)\
  |((du2_power4+du3_power4),iu2_power4,iu2_power4)\
  |((du3_power4+du4_power4),iu2_power4,iu1_power4)")

(define_insn_reservation "power4-imul" 7
  (and (eq_attr "type" "imul,lmul")
       (eq_attr "cpu" "power4"))
  "(q1_power4,iu1_power4*6)|(q2_power4,iu2_power4*6)")

(define_insn_reservation "power4-imul2" 5
  (and (eq_attr "type" "imul2")
       (eq_attr "cpu" "power4"))
  "(q1_power4,iu1_power4*4)|(q2_power4,iu2_power4*4)")

(define_insn_reservation "power4-imul3" 4
  (and (eq_attr "type" "imul3")
       (eq_attr "cpu" "power4"))
  "(q1_power4,iu1_power4*3)|(q2_power4,iu2_power4*3)")

; SPR move only executes in first IU.
; Integer division only executes in second IU.
(define_insn_reservation "power4-idiv" 36
  (and (eq_attr "type" "idiv")
       (eq_attr "cpu" "power4"))
  "(du1_power4+du2_power4),iu2_power4*35")

(define_insn_reservation "power4-ldiv" 68
  (and (eq_attr "type" "ldiv")
       (eq_attr "cpu" "power4"))
  "(du1_power4+du2_power4),iu2_power4*67")


(define_insn_reservation "power4-mtjmpr" 3
  (and (eq_attr "type" "mtjmpr")
       (eq_attr "cpu" "power4"))
  "du1_power4,bpu_power4")


; Branches take dispatch Slot 4.  The presence_sets prevent other insn from
; grabbing previous dispatch slots once this is assigned.
(define_insn_reservation "power4-branch" 2
  (and (eq_attr "type" "jmpreg,branch")
       (eq_attr "cpu" "power4"))
  "du5_power4,bpu_power4")


; Condition Register logical ops are split if non-destructive (RT != RB)
(define_insn_reservation "power4-crlogical" 2
  (and (eq_attr "type" "cr_logical")
       (eq_attr "cpu" "power4"))
  "du1_power4,cru_power4")

(define_insn_reservation "power4-delayedcr" 4
  (and (eq_attr "type" "delayed_cr")
       (eq_attr "cpu" "power4"))
  "(du1_power4+du2_power4),cru_power4,cru_power4")

; 4 mfcrf (each 3 cyc, 1/cyc) + 3 fxu
(define_insn_reservation "power4-mfcr" 6
  (and (eq_attr "type" "mfcr")
       (eq_attr "cpu" "power4"))
  "(du1_power4+du2_power4+du3_power4+du4_power4),\
   (du1_power4+du2_power4+du3_power4+du4_power4+cru_power4),\
    cru_power4,cru_power4,cru_power4")

; mtcrf (1 field)
(define_insn_reservation "power4-mtcr" 4
  (and (eq_attr "type" "mtcr")
       (eq_attr "cpu" "power4"))
  "du1_power4,iu1_power4")

; Basic FP latency is 6 cycles
(define_insn_reservation "power4-fp" 6
  (and (eq_attr "type" "fp,dmul")
       (eq_attr "cpu" "power4"))
  "fpq_power4")

(define_insn_reservation "power4-fpcompare" 5
  (and (eq_attr "type" "fpcompare")
       (eq_attr "cpu" "power4"))
  "fpq_power4")

(define_insn_reservation "power4-sdiv" 33
  (and (eq_attr "type" "sdiv,ddiv")
       (eq_attr "cpu" "power4"))
  "(q1_power4,fpu1_power4*28)|(q2_power4,fpu2_power4*28)")

(define_insn_reservation "power4-sqrt" 40
  (and (eq_attr "type" "ssqrt,dsqrt")
       (eq_attr "cpu" "power4"))
  "(q1_power4,fpu1_power4*35)|(q2_power4,fpu2_power4*35)")


; VMX
(define_insn_reservation "power4-vec" 2
  (and (eq_attr "type" "vecsimple,veccomplex")
       (eq_attr "cpu" "power4"))
  "vq_power4")

; vecfp compare
(define_insn_reservation "power4-veccmp" 8
  (and (eq_attr "type" "veccmp")
       (eq_attr "cpu" "power4"))
  "vq_power4")

(define_insn_reservation "power4-vecfloat" 8
  (and (eq_attr "type" "vecfloat")
       (eq_attr "cpu" "power4"))
  "vq_power4")

(define_insn_reservation "power4-vecperm" 2
  (and (eq_attr "type" "vecperm")
       (eq_attr "cpu" "power4"))
  "vpq_power4")

(define_bypass 4 "power4-vecload" "power4-vecperm")
(define_bypass 5 "power4-vec"
		 "power4-branch,power4-crlogical,power4-delayedcr,power4-mfcr")
(define_bypass 3 "power4-vec,power4-vecfloat" "power4-vecperm")
(define_bypass 3 "power4-vecperm" "power4-vec,power4-vecfloat")
