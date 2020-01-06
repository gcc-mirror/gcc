;; Scheduling description for IBM POWER8 processor.
;; Copyright (C) 2013-2020 Free Software Foundation, Inc.
;;
;; Contributed by Pat Haugen (pthaugen@us.ibm.com).

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

(define_automaton "power8fxu,power8lsu,power8vsu,power8misc")

(define_cpu_unit "fxu0_power8,fxu1_power8" "power8fxu")
(define_cpu_unit "lu0_power8,lu1_power8" "power8lsu")
(define_cpu_unit "lsu0_power8,lsu1_power8" "power8lsu")
(define_cpu_unit "vsu0_power8,vsu1_power8" "power8vsu")
(define_cpu_unit "bpu_power8,cru_power8" "power8misc")
(define_cpu_unit "du0_power8,du1_power8,du2_power8,du3_power8,du4_power8,\
		  du5_power8,du6_power8"  "power8misc")


; Dispatch group reservations
(define_reservation "DU_any_power8"
		    "du0_power8|du1_power8|du2_power8|du3_power8|du4_power8|\
		     du5_power8")

; 2-way Cracked instructions go in slots 0-1
;   (can also have a second in slots 3-4 if insns are adjacent)
(define_reservation "DU_cracked_power8"
		    "du0_power8+du1_power8")

; Insns that are first in group
(define_reservation "DU_first_power8"
		    "du0_power8")

; Insns that are first and last in group
(define_reservation "DU_both_power8"
		    "du0_power8+du1_power8+du2_power8+du3_power8+du4_power8+\
		     du5_power8+du6_power8")

; Dispatch slots are allocated in order conforming to program order.
(absence_set "du0_power8" "du1_power8,du2_power8,du3_power8,du4_power8,\
	      du5_power8,du6_power8")
(absence_set "du1_power8" "du2_power8,du3_power8,du4_power8,du5_power8,\
	      du6_power8")
(absence_set "du2_power8" "du3_power8,du4_power8,du5_power8,du6_power8")
(absence_set "du3_power8" "du4_power8,du5_power8,du6_power8")
(absence_set "du4_power8" "du5_power8,du6_power8")
(absence_set "du5_power8" "du6_power8")


; Execution unit reservations
(define_reservation "FXU_power8"
                    "fxu0_power8|fxu1_power8")

(define_reservation "LU_power8"
                    "lu0_power8|lu1_power8")

(define_reservation "LSU_power8"
                    "lsu0_power8|lsu1_power8")

(define_reservation "LU_or_LSU_power8"
                    "lu0_power8|lu1_power8|lsu0_power8|lsu1_power8")

(define_reservation "VSU_power8"
                    "vsu0_power8|vsu1_power8")


; LS Unit
(define_insn_reservation "power8-load" 3
  (and (eq_attr "type" "load")
       (eq_attr "sign_extend" "no")
       (eq_attr "update" "no")
       (eq_attr "cpu" "power8"))
  "DU_any_power8,LU_or_LSU_power8")

(define_insn_reservation "power8-load-update" 3
  (and (eq_attr "type" "load")
       (eq_attr "sign_extend" "no")
       (eq_attr "update" "yes")
       (eq_attr "cpu" "power8"))
  "DU_cracked_power8,LU_or_LSU_power8+FXU_power8")

(define_insn_reservation "power8-load-ext" 3
  (and (eq_attr "type" "load")
       (eq_attr "sign_extend" "yes")
       (eq_attr "update" "no")
       (eq_attr "cpu" "power8"))
  "DU_cracked_power8,LU_or_LSU_power8,FXU_power8")

(define_insn_reservation "power8-load-ext-update" 3
  (and (eq_attr "type" "load")
       (eq_attr "sign_extend" "yes")
       (eq_attr "update" "yes")
       (eq_attr "cpu" "power8"))
  "DU_both_power8,LU_or_LSU_power8+FXU_power8,FXU_power8")

(define_insn_reservation "power8-fpload" 5
  (and (ior (eq_attr "type" "vecload")
	    (and (eq_attr "type" "fpload")
		 (eq_attr "update" "no")))
       (eq_attr "cpu" "power8"))
  "DU_any_power8,LU_power8")

(define_insn_reservation "power8-fpload-update" 5
  (and (eq_attr "type" "fpload")
       (eq_attr "update" "yes")
       (eq_attr "cpu" "power8"))
  "DU_cracked_power8,LU_power8+FXU_power8")

(define_insn_reservation "power8-store" 5 ; store-forwarding latency
  (and (eq_attr "type" "store")
       (not (and (eq_attr "update" "yes")
		 (eq_attr "indexed" "yes")))
       (eq_attr "cpu" "power8"))
  "DU_any_power8,LSU_power8+LU_power8")

(define_insn_reservation "power8-store-update-indexed" 5
  (and (eq_attr "type" "store")
       (eq_attr "update" "yes")
       (eq_attr "indexed" "yes")
       (eq_attr "cpu" "power8"))
  "DU_cracked_power8,LSU_power8+LU_power8")

(define_insn_reservation "power8-fpstore" 5
  (and (eq_attr "type" "fpstore")
       (eq_attr "update" "no")
       (eq_attr "cpu" "power8"))
  "DU_any_power8,LSU_power8+VSU_power8")

(define_insn_reservation "power8-fpstore-update" 5
  (and (eq_attr "type" "fpstore")
       (eq_attr "update" "yes")
       (eq_attr "cpu" "power8"))
  "DU_any_power8,LSU_power8+VSU_power8")

(define_insn_reservation "power8-vecstore" 5
  (and (eq_attr "type" "vecstore")
       (eq_attr "cpu" "power8"))
  "DU_cracked_power8,LSU_power8+VSU_power8")

(define_insn_reservation "power8-larx" 3
  (and (eq_attr "type" "load_l")
       (eq_attr "cpu" "power8"))
  "DU_both_power8,LU_or_LSU_power8")

(define_insn_reservation "power8-stcx" 10
  (and (eq_attr "type" "store_c")
       (eq_attr "cpu" "power8"))
  "DU_both_power8,LSU_power8+LU_power8")

(define_insn_reservation "power8-sync" 1
  (and (eq_attr "type" "sync,isync")
       (eq_attr "cpu" "power8"))
  "DU_both_power8,LSU_power8")


; FX Unit
(define_insn_reservation "power8-1cyc" 1
  (and (ior (eq_attr "type" "integer,insert,trap,isel")
	    (and (eq_attr "type" "add,logical,shift,exts")
		 (eq_attr "dot" "no")))
       (eq_attr "cpu" "power8"))
  "DU_any_power8,FXU_power8")

; Extra cycle to LU/LSU
(define_bypass 2 "power8-1cyc"
		 "power8-load*,power8-fpload*,power8-store*,power8-fpstore*,\
		  power8-vecstore,power8-larx,power8-stcx")
;		 "power8-load,power8-load-update,power8-load-ext,\
;		  power8-load-ext-update,power8-fpload,power8-fpload-update,\
;		  power8-store,power8-store-update,power8-store-update-indexed,\
;		  power8-fpstore,power8-fpstore-update,power8-vecstore,\
;		  power8-larx,power8-stcx")

(define_insn_reservation "power8-2cyc" 2
  (and (eq_attr "type" "cntlz,popcnt")
       (eq_attr "cpu" "power8"))
  "DU_any_power8,FXU_power8")

(define_insn_reservation "power8-two" 2
  (and (eq_attr "type" "two")
       (eq_attr "cpu" "power8"))
  "DU_any_power8+DU_any_power8,FXU_power8,FXU_power8")

(define_insn_reservation "power8-three" 3
  (and (eq_attr "type" "three")
       (eq_attr "cpu" "power8"))
  "DU_any_power8+DU_any_power8+DU_any_power8,FXU_power8,FXU_power8,FXU_power8")

; cmp - Normal compare insns
(define_insn_reservation "power8-cmp" 2
  (and (eq_attr "type" "cmp")
       (eq_attr "cpu" "power8"))
  "DU_any_power8,FXU_power8")

; add/logical with dot : add./and./nor./etc
(define_insn_reservation "power8-fast-compare" 2
  (and (eq_attr "type" "add,logical")
       (eq_attr "dot" "yes")
       (eq_attr "cpu" "power8"))
  "DU_any_power8,FXU_power8")

; exts/shift with dot : rldicl./exts./rlwinm./slwi./rlwnm./slw./etc
(define_insn_reservation "power8-compare" 2
  (and (eq_attr "type" "shift,exts")
       (eq_attr "dot" "yes")
       (eq_attr "cpu" "power8"))
  "DU_cracked_power8,FXU_power8,FXU_power8")

; Extra cycle to LU/LSU
(define_bypass 3 "power8-fast-compare,power8-compare"
		 "power8-load*,power8-fpload*,power8-store*,power8-fpstore*,\
		  power8-vecstore,power8-larx,power8-stcx")

; 5 cycle CR latency 
(define_bypass 5 "power8-fast-compare,power8-compare"
		 "power8-crlogical,power8-mfcr,power8-mfcrf,power8-branch")

(define_insn_reservation "power8-mul" 4
  (and (eq_attr "type" "mul")
       (eq_attr "dot" "no")
       (eq_attr "cpu" "power8"))
  "DU_any_power8,FXU_power8")

(define_insn_reservation "power8-mul-compare" 4
  (and (eq_attr "type" "mul")
       (eq_attr "dot" "yes")
       (eq_attr "cpu" "power8"))
  "DU_cracked_power8,FXU_power8")

; Extra cycle to LU/LSU
(define_bypass 5 "power8-mul,power8-mul-compare"
		 "power8-load*,power8-fpload*,power8-store*,power8-fpstore*,\
		  power8-vecstore,power8-larx,power8-stcx")

; 7 cycle CR latency 
(define_bypass 7 "power8-mul,power8-mul-compare"
		 "power8-crlogical,power8-mfcr,power8-mfcrf,power8-branch")

; FXU divides are not pipelined
(define_insn_reservation "power8-idiv" 37
  (and (eq_attr "type" "div")
       (eq_attr "size" "32")
       (eq_attr "cpu" "power8"))
  "DU_any_power8,fxu0_power8*37|fxu1_power8*37")

(define_insn_reservation "power8-ldiv" 68
  (and (eq_attr "type" "div")
       (eq_attr "size" "64")
       (eq_attr "cpu" "power8"))
  "DU_any_power8,fxu0_power8*68|fxu1_power8*68")

(define_insn_reservation "power8-mtjmpr" 5
  (and (eq_attr "type" "mtjmpr")
       (eq_attr "cpu" "power8"))
  "DU_first_power8,FXU_power8")

; Should differentiate between 1 cr field and > 1 since mtocrf is not microcode
(define_insn_reservation "power8-mtcr" 3
  (and (eq_attr "type" "mtcr")
       (eq_attr "cpu" "power8"))
  "DU_both_power8,FXU_power8")


; CR Unit
(define_insn_reservation "power8-mfjmpr" 5
  (and (eq_attr "type" "mfjmpr")
       (eq_attr "cpu" "power8"))
  "DU_first_power8,cru_power8+FXU_power8")

(define_insn_reservation "power8-crlogical" 3
  (and (eq_attr "type" "cr_logical")
       (eq_attr "cpu" "power8"))
  "DU_first_power8,cru_power8")

(define_insn_reservation "power8-mfcr" 5
  (and (eq_attr "type" "mfcr")
       (eq_attr "cpu" "power8"))
  "DU_both_power8,cru_power8")

(define_insn_reservation "power8-mfcrf" 3
  (and (eq_attr "type" "mfcrf")
       (eq_attr "cpu" "power8"))
  "DU_first_power8,cru_power8")


; BR Unit
; Branches take dispatch slot 7, but reserve any remaining prior slots to
; prevent other insns from grabbing them once this is assigned.
(define_insn_reservation "power8-branch" 3
  (and (eq_attr "type" "jmpreg,branch")
       (eq_attr "cpu" "power8"))
  "(du6_power8\
   |du5_power8+du6_power8\
   |du4_power8+du5_power8+du6_power8\
   |du3_power8+du4_power8+du5_power8+du6_power8\
   |du2_power8+du3_power8+du4_power8+du5_power8+du6_power8\
   |du1_power8+du2_power8+du3_power8+du4_power8+du5_power8+du6_power8\
   |du0_power8+du1_power8+du2_power8+du3_power8+du4_power8+du5_power8+\
    du6_power8),bpu_power8")

; Branch updating LR/CTR feeding mf[lr|ctr]
(define_bypass 4 "power8-branch" "power8-mfjmpr")


; VS Unit (includes FP/VSX/VMX/DFP/Crypto)
(define_insn_reservation "power8-fp" 6
  (and (eq_attr "type" "fp,fpsimple,dmul,dfp")
       (eq_attr "cpu" "power8"))
  "DU_any_power8,VSU_power8")

; Additional 3 cycles for any CR result
(define_bypass 9 "power8-fp" "power8-crlogical,power8-mfcr*,power8-branch")

(define_insn_reservation "power8-fpcompare" 8
  (and (eq_attr "type" "fpcompare")
       (eq_attr "cpu" "power8"))
  "DU_any_power8,VSU_power8")

(define_insn_reservation "power8-sdiv" 27
  (and (eq_attr "type" "sdiv")
       (eq_attr "cpu" "power8"))
  "DU_any_power8,VSU_power8")

(define_insn_reservation "power8-ddiv" 33
  (and (eq_attr "type" "ddiv")
       (eq_attr "cpu" "power8"))
  "DU_any_power8,VSU_power8")

(define_insn_reservation "power8-sqrt" 32
  (and (eq_attr "type" "ssqrt")
       (eq_attr "cpu" "power8"))
  "DU_any_power8,VSU_power8")

(define_insn_reservation "power8-dsqrt" 44
  (and (eq_attr "type" "dsqrt")
       (eq_attr "cpu" "power8"))
  "DU_any_power8,VSU_power8")

(define_insn_reservation "power8-vecsimple" 2
  (and (eq_attr "type" "vecperm,vecsimple,veclogical,vecmove,veccmp,
			veccmpfx")
       (eq_attr "cpu" "power8"))
  "DU_any_power8,VSU_power8")

(define_insn_reservation "power8-vecnormal" 6
  (and (eq_attr "type" "vecfloat,vecdouble")
       (eq_attr "cpu" "power8"))
  "DU_any_power8,VSU_power8")

(define_bypass 7 "power8-vecnormal"
		 "power8-vecsimple,power8-veccomplex,power8-fpstore*,\
		  power8-vecstore")

(define_insn_reservation "power8-veccomplex" 7
  (and (eq_attr "type" "veccomplex")
       (eq_attr "cpu" "power8"))
  "DU_any_power8,VSU_power8")

(define_insn_reservation "power8-vecfdiv" 25
  (and (eq_attr "type" "vecfdiv")
       (eq_attr "cpu" "power8"))
  "DU_any_power8,VSU_power8")

(define_insn_reservation "power8-vecdiv" 31
  (and (eq_attr "type" "vecdiv")
       (eq_attr "cpu" "power8"))
  "DU_any_power8,VSU_power8")

(define_insn_reservation "power8-mffgpr" 5
  (and (eq_attr "type" "mffgpr")
       (eq_attr "cpu" "power8"))
  "DU_any_power8,VSU_power8")

(define_insn_reservation "power8-mftgpr" 6
  (and (eq_attr "type" "mftgpr")
       (eq_attr "cpu" "power8"))
  "DU_any_power8,VSU_power8")

(define_insn_reservation "power8-crypto" 7
  (and (eq_attr "type" "crypto")
       (eq_attr "cpu" "power8"))
  "DU_any_power8,VSU_power8")

