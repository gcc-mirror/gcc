;; Scheduling description for the IBM POWER10 processor.
;; Copyright (C) 2020-2024 Free Software Foundation, Inc.
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

; For Power10 we model (and try to pack) the in-order decode/dispatch groups
; which consist of 8 instructions max.  We do not try to model the details of
; the out-of-order issue queues and how insns flow to the various execution
; units except for the simple representation of the issue limitation of at
; most 4 insns to the execution units/2 insns to the load units/2 insns to
; the store units.
(define_automaton "power10dispatch,power10issue")

; Decode/dispatch slots
(define_cpu_unit "du0_power10,du1_power10,du2_power10,du3_power10,
		  du4_power10,du5_power10,du6_power10,du7_power10" "power10dispatch")

; Four execution units
(define_cpu_unit "exu0_power10,exu1_power10,exu2_power10,exu3_power10"
		 "power10issue")
; Two load units and two store units
(define_cpu_unit "lu0_power10,lu1_power10" "power10issue")
(define_cpu_unit "stu0_power10,stu1_power10" "power10issue")


; Dispatch slots are allocated in order conforming to program order.
(absence_set "du0_power10" "du1_power10,du2_power10,du3_power10,du4_power10,\
              du5_power10,du6_power10,du7_power10")
(absence_set "du1_power10" "du2_power10,du3_power10,du4_power10,du5_power10,\
              du6_power10,du7_power10")
(absence_set "du2_power10" "du3_power10,du4_power10,du5_power10,du6_power10,\
	      du7_power10")
(absence_set "du3_power10" "du4_power10,du5_power10,du6_power10,du7_power10")
(absence_set "du4_power10" "du5_power10,du6_power10,du7_power10")
(absence_set "du5_power10" "du6_power10,du7_power10")
(absence_set "du6_power10" "du7_power10")


; Dispatch port reservations
;
; Power10 can dispatch a maximum of 8 iops per cycle. With a maximum of
; 4 VSU/2 Load/2 Store per cycle.

; Any dispatch slot
(define_reservation "DU_any_power10"
		    "du0_power10|du1_power10|du2_power10|du3_power10|
		     du4_power10|du5_power10|du6_power10|du7_power10")

; Even slot, actually takes even/odd slots
(define_reservation "DU_even_power10"
		    "du0_power10+du1_power10|du2_power10+du3_power10|
		     du4_power10+du5_power10|du6_power10+du7_power10")

; 4-way cracked (consumes whole decode/dispatch cycle)
(define_reservation "DU_all_power10"
		    "du0_power10+du1_power10+du2_power10+du3_power10+
		     du4_power10+du5_power10+du6_power10+du7_power10")


; Execution unit reservations
(define_reservation "LU_power10"
		    "lu0_power10|lu1_power10")

(define_reservation "STU_power10"
		    "stu0_power10|stu1_power10")

; Certain simple fixed-point insns can execute in the Store-agen pipe
(define_reservation "SXU_power10"
		    "stu0_power10|stu1_power10")

(define_reservation "EXU_power10"
		    "exu0_power10|exu1_power10|exu2_power10|exu3_power10")

(define_reservation "EXU_super_power10"
		    "exu0_power10+exu1_power10|exu2_power10+exu3_power10")


; Load Unit
(define_insn_reservation "power10-load" 4
  (and (eq_attr "type" "load")
       (eq_attr "update" "no")
       (eq_attr "size" "!128")
       (eq_attr "prefixed" "no")
       (eq_attr "cpu" "power10"))
  "DU_any_power10,LU_power10")

(define_insn_reservation "power10-fused-load" 4
  (and (eq_attr "type" "fused_load_cmpi,fused_addis_load,fused_load_load")
       (eq_attr "cpu" "power10"))
  "DU_even_power10,LU_power10")

(define_insn_reservation "power10-prefixed-load" 4
  (and (eq_attr "type" "load")
       (eq_attr "update" "no")
       (eq_attr "size" "!128")
       (eq_attr "prefixed" "yes")
       (eq_attr "cpu" "power10"))
  "DU_even_power10,LU_power10")

(define_insn_reservation "power10-load-update" 4
  (and (eq_attr "type" "load")
       (eq_attr "update" "yes")
       (eq_attr "cpu" "power10"))
  "DU_even_power10,LU_power10+SXU_power10")

(define_insn_reservation "power10-fpload-double" 4
  (and (eq_attr "type" "fpload")
       (eq_attr "update" "no")
       (eq_attr "size" "64")
       (eq_attr "prefixed" "no")
       (eq_attr "cpu" "power10"))
  "DU_any_power10,LU_power10")

(define_insn_reservation "power10-prefixed-fpload-double" 4
  (and (eq_attr "type" "fpload")
       (eq_attr "update" "no")
       (eq_attr "size" "64")
       (eq_attr "prefixed" "yes")
       (eq_attr "cpu" "power10"))
  "DU_even_power10,LU_power10")

(define_insn_reservation "power10-fpload-update-double" 4
  (and (eq_attr "type" "fpload")
       (eq_attr "update" "yes")
       (eq_attr "size" "64")
       (eq_attr "cpu" "power10"))
  "DU_even_power10,LU_power10+SXU_power10")

; SFmode loads are cracked and have additional 3 cycles over DFmode
; Prefixed forms behave the same
(define_insn_reservation "power10-fpload-single" 7
  (and (eq_attr "type" "fpload")
       (eq_attr "update" "no")
       (eq_attr "size" "32")
       (eq_attr "cpu" "power10"))
  "DU_even_power10,LU_power10")

(define_insn_reservation "power10-fpload-update-single" 7
  (and (eq_attr "type" "fpload")
       (eq_attr "update" "yes")
       (eq_attr "size" "32")
       (eq_attr "cpu" "power10"))
  "DU_even_power10,LU_power10+SXU_power10")

(define_insn_reservation "power10-vecload" 4
  (and (eq_attr "type" "vecload")
       (eq_attr "size" "!256")
       (eq_attr "cpu" "power10"))
  "DU_any_power10,LU_power10")

; lxvp
(define_insn_reservation "power10-vecload-pair" 4
  (and (eq_attr "type" "vecload")
       (eq_attr "size" "256")
       (eq_attr "cpu" "power10"))
  "DU_even_power10,LU_power10+SXU_power10")

; Store Unit
(define_insn_reservation "power10-store" 0
  (and (eq_attr "type" "store,fpstore,vecstore")
       (eq_attr "update" "no")
       (eq_attr "prefixed" "no")
       (eq_attr "size" "!128")
       (eq_attr "size" "!256")
       (eq_attr "cpu" "power10"))
  "DU_any_power10,STU_power10")

(define_insn_reservation "power10-fused-store" 0
  (and (eq_attr "type" "fused_store_store")
       (eq_attr "cpu" "power10"))
  "DU_even_power10,STU_power10")

(define_insn_reservation "power10-prefixed-store" 0
  (and (eq_attr "type" "store,fpstore,vecstore")
       (eq_attr "prefixed" "yes")
       (eq_attr "size" "!128")
       (eq_attr "size" "!256")
       (eq_attr "cpu" "power10"))
  "DU_even_power10,STU_power10")

; Update forms have 2 cycle latency for updated addr reg
(define_insn_reservation "power10-store-update" 2
  (and (eq_attr "type" "store,fpstore")
       (eq_attr "update" "yes")
       (eq_attr "cpu" "power10"))
  "DU_any_power10,STU_power10")

; stxvp
(define_insn_reservation "power10-vecstore-pair" 0
  (and (eq_attr "type" "vecstore")
       (eq_attr "size" "256")
       (eq_attr "cpu" "power10"))
  "DU_even_power10,stu0_power10+stu1_power10")

(define_insn_reservation "power10-larx" 4
  (and (eq_attr "type" "load_l")
       (eq_attr "size" "!128")
       (eq_attr "cpu" "power10"))
  "DU_any_power10,LU_power10")

; All load quad forms
(define_insn_reservation "power10-lq" 4
  (and (eq_attr "type" "load,load_l")
       (eq_attr "size" "128")
       (eq_attr "cpu" "power10"))
  "DU_even_power10,LU_power10+SXU_power10")

(define_insn_reservation "power10-stcx" 0
  (and (eq_attr "type" "store_c")
       (eq_attr "size" "!128")
       (eq_attr "cpu" "power10"))
  "DU_any_power10,STU_power10")

; All store quad forms
(define_insn_reservation "power10-stq" 0
  (and (eq_attr "type" "store,store_c")
       (eq_attr "size" "128")
       (eq_attr "cpu" "power10"))
  "DU_even_power10,stu0_power10+stu1_power10")

(define_insn_reservation "power10-sync" 1
  (and (eq_attr "type" "sync,isync")
       (eq_attr "cpu" "power10"))
  "DU_even_power10,STU_power10")


; VSU Execution Unit

; Fixed point ops

; Most ALU insns are simple 2 cycle, including record form
(define_insn_reservation "power10-alu" 2
  (and (eq_attr "type" "add,exts,integer,logical,isel")
       (eq_attr "prefixed" "no")
       (eq_attr "cpu" "power10"))
  "DU_any_power10,EXU_power10")
; 4 cycle CR latency
(define_bypass 4 "power10-alu"
		 "power10-crlogical,power10-mfcr,power10-mfcrf")

(define_insn_reservation "power10-fused_alu" 2
  (and (eq_attr "type" "fused_arith_logical,fused_cmp_isel,fused_carry")
       (eq_attr "cpu" "power10"))
  "DU_even_power10,EXU_power10")

; paddi
(define_insn_reservation "power10-paddi" 2
  (and (eq_attr "type" "add")
       (eq_attr "prefixed" "yes")
       (eq_attr "cpu" "power10"))
  "DU_even_power10,EXU_power10")

; Rotate/shift (non-record form)
(define_insn_reservation "power10-rot" 2
  (and (eq_attr "type" "insert,shift")
       (eq_attr "dot" "no")
       (eq_attr "cpu" "power10"))
  "DU_any_power10,EXU_power10")

; Record form rotate/shift
(define_insn_reservation "power10-rot-compare" 3
  (and (eq_attr "type" "insert,shift")
       (eq_attr "dot" "yes")
       (eq_attr "cpu" "power10"))
  "DU_any_power10,EXU_power10")
; 5 cycle CR latency
(define_bypass 5 "power10-rot-compare"
		 "power10-crlogical,power10-mfcr,power10-mfcrf")

(define_insn_reservation "power10-alu2" 3
  (and (eq_attr "type" "cntlz,popcnt,trap")
       (eq_attr "cpu" "power10"))
  "DU_any_power10,EXU_power10")
; 5 cycle CR latency
(define_bypass 5 "power10-alu2"
		 "power10-crlogical,power10-mfcr,power10-mfcrf")

(define_insn_reservation "power10-cmp" 2
  (and (eq_attr "type" "cmp")
       (eq_attr "cpu" "power10"))
  "DU_any_power10,EXU_power10")

; Treat 'two' and 'three' types as 2 or 3 way cracked
(define_insn_reservation "power10-two" 4
  (and (eq_attr "type" "two")
       (eq_attr "cpu" "power10"))
  "DU_even_power10,EXU_power10")

(define_insn_reservation "power10-three" 6
  (and (eq_attr "type" "three")
       (eq_attr "cpu" "power10"))
  "DU_all_power10,EXU_power10")

(define_insn_reservation "power10-mul" 5
  (and (eq_attr "type" "mul")
       (eq_attr "dot" "no")
       (eq_attr "cpu" "power10"))
  "DU_any_power10,EXU_power10")
; 4 cycle MUL->MUL latency
(define_bypass 4 "power10-mul"
		 "power10-mul,power10-mul-compare")

(define_insn_reservation "power10-mul-compare" 5
  (and (eq_attr "type" "mul")
       (eq_attr "dot" "yes")
       (eq_attr "cpu" "power10"))
  "DU_even_power10,EXU_power10")
; 4 cycle MUL->MUL latency
(define_bypass 4 "power10-mul-compare"
		 "power10-mul,power10-mul-compare")
; 7 cycle CR latency
(define_bypass 7 "power10-mul-compare"
		 "power10-crlogical,power10-mfcr,power10-mfcrf")

(define_insn_reservation "power10-div" 12
  (and (eq_attr "type" "div")
       (eq_attr "dot" "no")
       (eq_attr "cpu" "power10"))
  "DU_any_power10,EXU_power10")

(define_insn_reservation "power10-div-compare" 12
  (and (eq_attr "type" "div")
       (eq_attr "dot" "yes")
       (eq_attr "cpu" "power10"))
  "DU_even_power10,EXU_power10")
; 14 cycle CR latency
(define_bypass 14 "power10-div-compare"
		 "power10-crlogical,power10-mfcr,power10-mfcrf")

(define_insn_reservation "power10-crlogical" 2
  (and (eq_attr "type" "cr_logical")
       (eq_attr "cpu" "power10"))
  "DU_any_power10,EXU_power10")

(define_insn_reservation "power10-mfcrf" 2
  (and (eq_attr "type" "mfcrf")
       (eq_attr "cpu" "power10"))
  "DU_any_power10,EXU_power10")

(define_insn_reservation "power10-mfcr" 3
  (and (eq_attr "type" "mfcr")
       (eq_attr "cpu" "power10"))
  "DU_even_power10,EXU_power10")

; Should differentiate between 1 cr field and > 1 since target of > 1 cr
; is cracked
(define_insn_reservation "power10-mtcr" 3
  (and (eq_attr "type" "mtcr")
       (eq_attr "cpu" "power10"))
  "DU_any_power10,EXU_power10")

(define_insn_reservation "power10-mtjmpr" 3
  (and (eq_attr "type" "mtjmpr")
       (eq_attr "cpu" "power10"))
  "DU_any_power10,EXU_power10")

(define_insn_reservation "power10-mfjmpr" 2
  (and (eq_attr "type" "mfjmpr")
       (eq_attr "cpu" "power10"))
  "DU_any_power10,EXU_power10")


; Floating point/Vector ops

(define_insn_reservation "power10-fpsimple" 3
  (and (eq_attr "type" "fpsimple")
       (eq_attr "cpu" "power10"))
  "DU_any_power10,EXU_power10")

(define_insn_reservation "power10-fp" 5
  (and (eq_attr "type" "fp,dmul")
       (eq_attr "cpu" "power10"))
  "DU_any_power10,EXU_power10")

(define_insn_reservation "power10-fpcompare" 3
  (and (eq_attr "type" "fpcompare")
       (eq_attr "cpu" "power10"))
  "DU_any_power10,EXU_power10")

(define_insn_reservation "power10-sdiv" 22
  (and (eq_attr "type" "sdiv")
       (eq_attr "cpu" "power10"))
  "DU_any_power10,EXU_power10")

(define_insn_reservation "power10-ddiv" 27
  (and (eq_attr "type" "ddiv")
       (eq_attr "cpu" "power10"))
  "DU_any_power10,EXU_power10")

(define_insn_reservation "power10-sqrt" 26
  (and (eq_attr "type" "ssqrt")
       (eq_attr "cpu" "power10"))
  "DU_any_power10,EXU_power10")

(define_insn_reservation "power10-dsqrt" 36
  (and (eq_attr "type" "dsqrt")
       (eq_attr "cpu" "power10"))
  "DU_any_power10,EXU_power10")

(define_insn_reservation "power10-vec-2cyc" 2
  (and (eq_attr "type" "vecmove,veclogical,vecexts,veccmpfx")
       (eq_attr "cpu" "power10"))
  "DU_any_power10,EXU_power10")

(define_insn_reservation "power10-fused-vec" 2
  (and (eq_attr "type" "fused_vector")
       (eq_attr "cpu" "power10"))
  "DU_even_power10,EXU_power10")

(define_insn_reservation "power10-veccmp" 3
  (and (eq_attr "type" "veccmp")
       (eq_attr "cpu" "power10"))
  "DU_any_power10,EXU_power10")

(define_insn_reservation "power10-vecsimple" 2
  (and (eq_attr "type" "vecsimple")
       (eq_attr "cpu" "power10"))
  "DU_any_power10,EXU_power10")

(define_insn_reservation "power10-vecnormal" 5
  (and (eq_attr "type" "vecfloat,vecdouble")
       (eq_attr "size" "!128")
       (eq_attr "cpu" "power10"))
  "DU_any_power10,EXU_power10")

(define_insn_reservation "power10-qp" 12
  (and (eq_attr "type" "vecfloat,vecdouble")
       (eq_attr "size" "128")
       (eq_attr "cpu" "power10"))
  "DU_any_power10,EXU_power10")

(define_insn_reservation "power10-vecperm" 3
  (and (eq_attr "type" "vecperm")
       (eq_attr "prefixed" "no")
       (eq_attr "dot" "no")
       (eq_attr "cpu" "power10"))
  "DU_any_power10,EXU_power10")

(define_insn_reservation "power10-vecperm-compare" 3
  (and (eq_attr "type" "vecperm")
       (eq_attr "dot" "yes")
       (eq_attr "cpu" "power10"))
  "DU_even_power10,EXU_power10")

(define_insn_reservation "power10-prefixed-vecperm" 3
  (and (eq_attr "type" "vecperm")
       (eq_attr "prefixed" "yes")
       (eq_attr "cpu" "power10"))
  "DU_even_power10,EXU_power10")

(define_insn_reservation "power10-veccomplex" 6
  (and (eq_attr "type" "veccomplex")
       (eq_attr "cpu" "power10"))
  "DU_any_power10,EXU_power10")

(define_insn_reservation "power10-vecfdiv" 24
  (and (eq_attr "type" "vecfdiv")
       (eq_attr "cpu" "power10"))
  "DU_any_power10,EXU_power10")

(define_insn_reservation "power10-vecdiv" 27
  (and (eq_attr "type" "vecdiv")
       (eq_attr "size" "!128")
       (eq_attr "cpu" "power10"))
  "DU_any_power10,EXU_power10")

(define_insn_reservation "power10-qpdiv" 56
  (and (eq_attr "type" "vecdiv")
       (eq_attr "size" "128")
       (eq_attr "cpu" "power10"))
  "DU_any_power10,EXU_power10")

(define_insn_reservation "power10-qpmul" 24
  (and (eq_attr "type" "qmul")
       (eq_attr "size" "128")
       (eq_attr "cpu" "power10"))
  "DU_any_power10,EXU_power10")

(define_insn_reservation "power10-mtvsr" 2
  (and (eq_attr "type" "mtvsr")
       (eq_attr "cpu" "power10"))
  "DU_any_power10,EXU_power10")

(define_insn_reservation "power10-mfvsr" 2
  (and (eq_attr "type" "mfvsr")
       (eq_attr "cpu" "power10"))
  "DU_any_power10,EXU_power10")


; Branch
; Branch is 2 cycles, grouped with STU for issue
(define_insn_reservation "power10-branch" 2
  (and (eq_attr "type" "jmpreg,branch")
       (eq_attr "cpu" "power10"))
  "DU_any_power10,STU_power10")

(define_insn_reservation "power10-fused-branch" 3
  (and (eq_attr "type" "fused_mtbc")
       (eq_attr "cpu" "power10"))
  "DU_even_power10,STU_power10")


; Crypto
(define_insn_reservation "power10-crypto" 4
  (and (eq_attr "type" "crypto")
       (eq_attr "cpu" "power10"))
  "DU_any_power10,EXU_power10")


; HTM
(define_insn_reservation "power10-htm" 2
  (and (eq_attr "type" "htmsimple,htm")
       (eq_attr "cpu" "power10"))
  "DU_any_power10,EXU_power10")


; DFP
; Use the minimum 12 cycle latency for all DFP insns
(define_insn_reservation "power10-dfp" 12
  (and (eq_attr "type" "dfp")
       (eq_attr "size" "!128")
       (eq_attr "cpu" "power10"))
  "DU_any_power10,EXU_power10")

(define_insn_reservation "power10-dfpq" 12
  (and (eq_attr "type" "dfp")
       (eq_attr "size" "128")
       (eq_attr "cpu" "power10"))
  "DU_even_power10,EXU_power10")

; MMA
(define_insn_reservation "power10-mma" 9
  (and (eq_attr "type" "mma")
       (eq_attr "prefixed" "no")
       (eq_attr "cpu" "power10"))
  "DU_any_power10,EXU_super_power10")

(define_insn_reservation "power10-prefixed-mma" 9
  (and (eq_attr "type" "mma")
       (eq_attr "prefixed" "yes")
       (eq_attr "cpu" "power10"))
  "DU_even_power10,EXU_super_power10")
; 4 cycle MMA->MMA latency
(define_bypass 4 "power10-mma,power10-prefixed-mma"
		 "power10-mma,power10-prefixed-mma")


