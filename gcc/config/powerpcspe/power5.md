;; Scheduling description for IBM POWER5 processor.
;;   Copyright (C) 2003-2017 Free Software Foundation, Inc.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;; Sources: IBM Red Book and White Paper on POWER5

;; The POWER5 has 2 iu, 2 fpu, 2 lsu per engine (2 engines per chip).
;; Instructions that update more than one register get broken into two
;; (split) or more internal ops.  The chip can issue up to 5
;; internal ops per cycle.

(define_automaton "power5iu,power5fpu,power5misc")

(define_cpu_unit "iu1_power5,iu2_power5" "power5iu")
(define_cpu_unit "lsu1_power5,lsu2_power5" "power5misc")
(define_cpu_unit "fpu1_power5,fpu2_power5" "power5fpu")
(define_cpu_unit "bpu_power5,cru_power5" "power5misc")
(define_cpu_unit "du1_power5,du2_power5,du3_power5,du4_power5,du5_power5"
		 "power5misc")

(define_reservation "lsq_power5"
		    "(du1_power5,lsu1_power5)\
		    |(du2_power5,lsu2_power5)\
		    |(du3_power5,lsu2_power5)\
		    |(du4_power5,lsu1_power5)")

(define_reservation "iq_power5"
		    "(du1_power5|du2_power5|du3_power5|du4_power5),\
                     (iu1_power5|iu2_power5)")

(define_reservation "fpq_power5"
		    "(du1_power5|du2_power5|du3_power5|du4_power5),\
                     (fpu1_power5|fpu2_power5)")

; Dispatch slots are allocated in order conforming to program order.
(absence_set "du1_power5" "du2_power5,du3_power5,du4_power5,du5_power5")
(absence_set "du2_power5" "du3_power5,du4_power5,du5_power5")
(absence_set "du3_power5" "du4_power5,du5_power5")
(absence_set "du4_power5" "du5_power5")


; Load/store
(define_insn_reservation "power5-load" 4 ; 3
  (and (eq_attr "type" "load")
       (eq_attr "sign_extend" "no")
       (eq_attr "update" "no")
       (eq_attr "cpu" "power5"))
  "lsq_power5")

(define_insn_reservation "power5-load-ext" 5
  (and (eq_attr "type" "load")
       (eq_attr "sign_extend" "yes")
       (eq_attr "update" "no")
       (eq_attr "cpu" "power5"))
  "du1_power5+du2_power5,lsu1_power5,nothing,nothing,iu2_power5")

(define_insn_reservation "power5-load-ext-update" 5
  (and (eq_attr "type" "load")
       (eq_attr "sign_extend" "yes")
       (eq_attr "update" "yes")
       (eq_attr "indexed" "no")
       (eq_attr "cpu" "power5"))
  "du1_power5+du2_power5+du3_power5+du4_power5,\
   lsu1_power5+iu2_power5,nothing,nothing,iu2_power5")

(define_insn_reservation "power5-load-ext-update-indexed" 5
  (and (eq_attr "type" "load")
       (eq_attr "sign_extend" "yes")
       (eq_attr "update" "yes")
       (eq_attr "indexed" "yes")
       (eq_attr "cpu" "power5"))
  "du1_power5+du2_power5+du3_power5+du4_power5,\
   iu1_power5,lsu2_power5+iu1_power5,nothing,nothing,iu2_power5")

(define_insn_reservation "power5-load-update-indexed" 3
  (and (eq_attr "type" "load")
       (eq_attr "sign_extend" "no")
       (eq_attr "update" "yes")
       (eq_attr "indexed" "yes")
       (eq_attr "cpu" "power5"))
  "du1_power5+du2_power5+du3_power5+du4_power5,\
   iu1_power5,lsu2_power5+iu2_power5")

(define_insn_reservation "power5-load-update" 4 ; 3
  (and (eq_attr "type" "load")
       (eq_attr "sign_extend" "no")
       (eq_attr "update" "yes")
       (eq_attr "indexed" "no")
       (eq_attr "cpu" "power5"))
  "du1_power5+du2_power5,lsu1_power5+iu2_power5")

(define_insn_reservation "power5-fpload" 6 ; 5
  (and (eq_attr "type" "fpload")
       (eq_attr "update" "no")
       (eq_attr "cpu" "power5"))
  "lsq_power5")

(define_insn_reservation "power5-fpload-update" 6 ; 5
  (and (eq_attr "type" "fpload")
       (eq_attr "update" "yes")
       (eq_attr "cpu" "power5"))
  "du1_power5+du2_power5,lsu1_power5+iu2_power5")

(define_insn_reservation "power5-store" 12
  (and (eq_attr "type" "store")
       (eq_attr "update" "no")
       (eq_attr "cpu" "power5"))
  "((du1_power5,lsu1_power5)\
    |(du2_power5,lsu2_power5)\
    |(du3_power5,lsu2_power5)\
    |(du4_power5,lsu1_power5)),\
    (iu1_power5|iu2_power5)")

(define_insn_reservation "power5-store-update" 12
  (and (eq_attr "type" "store")
       (eq_attr "update" "yes")
       (eq_attr "indexed" "no")
       (eq_attr "cpu" "power5"))
  "du1_power5+du2_power5,lsu1_power5+iu2_power5,iu1_power5")

(define_insn_reservation "power5-store-update-indexed" 12
  (and (eq_attr "type" "store")
       (eq_attr "update" "yes")
       (eq_attr "indexed" "yes")
       (eq_attr "cpu" "power5"))
   "du1_power5+du2_power5+du3_power5+du4_power5,\
    iu1_power5,lsu2_power5+iu2_power5,iu2_power5")

(define_insn_reservation "power5-fpstore" 12
  (and (eq_attr "type" "fpstore")
       (eq_attr "update" "no")
       (eq_attr "cpu" "power5"))
  "((du1_power5,lsu1_power5)\
    |(du2_power5,lsu2_power5)\
    |(du3_power5,lsu2_power5)\
    |(du4_power5,lsu1_power5)),\
    (fpu1_power5|fpu2_power5)")

(define_insn_reservation "power5-fpstore-update" 12
  (and (eq_attr "type" "fpstore")
       (eq_attr "update" "yes")
       (eq_attr "cpu" "power5"))
  "du1_power5+du2_power5,lsu1_power5+iu2_power5,fpu1_power5")

(define_insn_reservation "power5-llsc" 11
  (and (eq_attr "type" "load_l,store_c,sync")
       (eq_attr "cpu" "power5"))
  "du1_power5+du2_power5+du3_power5+du4_power5,\
  lsu1_power5")


; Integer latency is 2 cycles
(define_insn_reservation "power5-integer" 2
  (and (ior (eq_attr "type" "integer,trap,cntlz,isel,popcnt")
	    (and (eq_attr "type" "add,logical,shift,exts")
		 (eq_attr "dot" "no"))
	    (and (eq_attr "type" "insert")
		 (eq_attr "size" "64")))
       (eq_attr "cpu" "power5"))
  "iq_power5")

(define_insn_reservation "power5-two" 2
  (and (eq_attr "type" "two")
       (eq_attr "cpu" "power5"))
  "((du1_power5+du2_power5)\
    |(du2_power5+du3_power5)\
    |(du3_power5+du4_power5)\
    |(du4_power5+du1_power5)),\
    ((iu1_power5,nothing,iu2_power5)\
     |(iu2_power5,nothing,iu2_power5)\
     |(iu2_power5,nothing,iu1_power5)\
     |(iu1_power5,nothing,iu1_power5))")

(define_insn_reservation "power5-three" 2
  (and (eq_attr "type" "three")
       (eq_attr "cpu" "power5"))
  "(du1_power5+du2_power5+du3_power5|du2_power5+du3_power5+du4_power5\
    |du3_power5+du4_power5+du1_power5|du4_power5+du1_power5+du2_power5),\
   ((iu1_power5,nothing,iu2_power5,nothing,iu2_power5)\
    |(iu2_power5,nothing,iu2_power5,nothing,iu1_power5)\
    |(iu2_power5,nothing,iu1_power5,nothing,iu1_power5)\
    |(iu1_power5,nothing,iu2_power5,nothing,iu2_power5))")

(define_insn_reservation "power5-insert" 4
  (and (eq_attr "type" "insert")
       (eq_attr "size" "32")
       (eq_attr "cpu" "power5"))
  "du1_power5+du2_power5,iu1_power5,nothing,iu2_power5")

(define_insn_reservation "power5-cmp" 3
  (and (ior (eq_attr "type" "cmp")
	    (and (eq_attr "type" "add,logical")
		 (eq_attr "dot" "yes")))
       (eq_attr "cpu" "power5"))
  "iq_power5")

(define_insn_reservation "power5-compare" 2
  (and (eq_attr "type" "shift,exts")
       (eq_attr "dot" "yes")
       (eq_attr "cpu" "power5"))
  "du1_power5+du2_power5,iu1_power5,iu2_power5")

(define_bypass 4 "power5-compare" "power5-branch,power5-crlogical,power5-delayedcr,power5-mfcr,power5-mfcrf")

(define_insn_reservation "power5-lmul-cmp" 7
  (and (eq_attr "type" "mul")
       (eq_attr "dot" "yes")
       (eq_attr "size" "64")
       (eq_attr "cpu" "power5"))
  "du1_power5+du2_power5,iu1_power5*6,iu2_power5")

(define_bypass 10 "power5-lmul-cmp" "power5-branch,power5-crlogical,power5-delayedcr,power5-mfcr,power5-mfcrf")

(define_insn_reservation "power5-imul-cmp" 5
  (and (eq_attr "type" "mul")
       (eq_attr "dot" "yes")
       (eq_attr "size" "32")
       (eq_attr "cpu" "power5"))
  "du1_power5+du2_power5,iu1_power5*4,iu2_power5")

(define_bypass 8 "power5-imul-cmp" "power5-branch,power5-crlogical,power5-delayedcr,power5-mfcr,power5-mfcrf")

(define_insn_reservation "power5-lmul" 7
  (and (eq_attr "type" "mul")
       (eq_attr "dot" "no")
       (eq_attr "size" "64")
       (eq_attr "cpu" "power5"))
  "(du1_power5|du2_power5|du3_power5|du4_power5),(iu1_power5*6|iu2_power5*6)")

(define_insn_reservation "power5-imul" 5
  (and (eq_attr "type" "mul")
       (eq_attr "dot" "no")
       (eq_attr "size" "32")
       (eq_attr "cpu" "power5"))
  "(du1_power5|du2_power5|du3_power5|du4_power5),(iu1_power5*4|iu2_power5*4)")

(define_insn_reservation "power5-imul3" 4
  (and (eq_attr "type" "mul")
       (eq_attr "size" "8,16")
       (eq_attr "cpu" "power5"))
  "(du1_power5|du2_power5|du3_power5|du4_power5),(iu1_power5*3|iu2_power5*3)")


; SPR move only executes in first IU.
; Integer division only executes in second IU.
(define_insn_reservation "power5-idiv" 36
  (and (eq_attr "type" "div")
       (eq_attr "size" "32")
       (eq_attr "cpu" "power5"))
  "du1_power5+du2_power5,iu2_power5*35")

(define_insn_reservation "power5-ldiv" 68
  (and (eq_attr "type" "div")
       (eq_attr "size" "64")
       (eq_attr "cpu" "power5"))
  "du1_power5+du2_power5,iu2_power5*67")


(define_insn_reservation "power5-mtjmpr" 3
  (and (eq_attr "type" "mtjmpr,mfjmpr")
       (eq_attr "cpu" "power5"))
  "du1_power5,bpu_power5")


; Branches take dispatch Slot 4.  The presence_sets prevent other insn from
; grabbing previous dispatch slots once this is assigned.
(define_insn_reservation "power5-branch" 2
  (and (eq_attr "type" "jmpreg,branch")
       (eq_attr "cpu" "power5"))
  "(du5_power5\
   |du4_power5+du5_power5\
   |du3_power5+du4_power5+du5_power5\
   |du2_power5+du3_power5+du4_power5+du5_power5\
   |du1_power5+du2_power5+du3_power5+du4_power5+du5_power5),bpu_power5")


; Condition Register logical ops are split if non-destructive (RT != RB)
(define_insn_reservation "power5-crlogical" 2
  (and (eq_attr "type" "cr_logical")
       (eq_attr "cpu" "power5"))
  "du1_power5,cru_power5")

(define_insn_reservation "power5-delayedcr" 4
  (and (eq_attr "type" "delayed_cr")
       (eq_attr "cpu" "power5"))
  "du1_power5+du2_power5,cru_power5,cru_power5")

; 4 mfcrf (each 3 cyc, 1/cyc) + 3 fxu
(define_insn_reservation "power5-mfcr" 6
  (and (eq_attr "type" "mfcr")
       (eq_attr "cpu" "power5"))
  "du1_power5+du2_power5+du3_power5+du4_power5,\
   du1_power5+du2_power5+du3_power5+du4_power5+cru_power5,\
   cru_power5,cru_power5,cru_power5")

; mfcrf (1 field)
(define_insn_reservation "power5-mfcrf" 3
  (and (eq_attr "type" "mfcrf")
       (eq_attr "cpu" "power5"))
  "du1_power5,cru_power5")

; mtcrf (1 field)
(define_insn_reservation "power5-mtcr" 4
  (and (eq_attr "type" "mtcr")
       (eq_attr "cpu" "power5"))
  "du1_power5,iu1_power5")

; Basic FP latency is 6 cycles
(define_insn_reservation "power5-fp" 6
  (and (eq_attr "type" "fp,fpsimple,dmul")
       (eq_attr "cpu" "power5"))
  "fpq_power5")

(define_insn_reservation "power5-fpcompare" 5
  (and (eq_attr "type" "fpcompare")
       (eq_attr "cpu" "power5"))
  "fpq_power5")

(define_insn_reservation "power5-sdiv" 33
  (and (eq_attr "type" "sdiv,ddiv")
       (eq_attr "cpu" "power5"))
  "(du1_power5|du2_power5|du3_power5|du4_power5),\
   (fpu1_power5*28|fpu2_power5*28)")

(define_insn_reservation "power5-sqrt" 40
  (and (eq_attr "type" "ssqrt,dsqrt")
       (eq_attr "cpu" "power5"))
  "(du1_power5|du2_power5|du3_power5|du4_power5),\
   (fpu1_power5*35|fpu2_power5*35)")

(define_insn_reservation "power5-isync" 2 
  (and (eq_attr "type" "isync")
       (eq_attr "cpu" "power5"))
  "du1_power5+du2_power5+du3_power5+du4_power5,\
  lsu1_power5")

