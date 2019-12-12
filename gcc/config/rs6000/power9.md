;; Scheduling description for IBM POWER9 processor.
;; Copyright (C) 2016-2019 Free Software Foundation, Inc.
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

(define_automaton "power9dsp,power9lsu,power9vsu,power9fpdiv,power9misc")

(define_cpu_unit "lsu0_power9,lsu1_power9,lsu2_power9,lsu3_power9" "power9lsu")
(define_cpu_unit "vsu0_power9,vsu1_power9,vsu2_power9,vsu3_power9" "power9vsu")
; Two vector permute units, part of vsu
(define_cpu_unit "prm0_power9,prm1_power9" "power9vsu")
; Two fixed point divide units, not pipelined
(define_cpu_unit "fx_div0_power9,fx_div1_power9" "power9misc")
(define_cpu_unit "bru_power9,cryptu_power9,dfu_power9" "power9misc")
; Create a false unit for use by non-pipelined FP div/sqrt
(define_cpu_unit "fp_div0_power9,fp_div1_power9,fp_div2_power9,fp_div3_power9"
		 "power9fpdiv")


(define_cpu_unit "x0_power9,x1_power9,xa0_power9,xa1_power9,
		  x2_power9,x3_power9,xb0_power9,xb1_power9,
		  br0_power9,br1_power9" "power9dsp")


; Dispatch port reservations
;
; Power9 can dispatch a maximum of 6 iops per cycle with the following
; general restrictions (other restrictions also apply):
;   1) At most 2 iops per execution slice
;   2) At most 2 iops to the branch unit
; Note that insn position in a dispatch group of 6 insns does not infer which
; execution slice the insn is routed to.  The units are used to infer the
; conflicts that exist (i.e. an 'even' requirement will preclude dispatch
; with 2 insns with 'superslice' requirement).

; The xa0/xa1 units really represent the 3rd dispatch port for a superslice but
; are listed as separate units to allow those insns that preclude its use to
; still be scheduled two to a superslice while reserving the 3rd slot.  The
; same applies for xb0/xb1.
(define_reservation "DU_xa_power9" "xa0_power9+xa1_power9")
(define_reservation "DU_xb_power9" "xb0_power9+xb1_power9")

; Any execution slice dispatch
(define_reservation "DU_any_power9"
		    "x0_power9|x1_power9|DU_xa_power9|x2_power9|x3_power9|
		     DU_xb_power9")

; Even slice, actually takes even/odd slots
(define_reservation "DU_even_power9" "x0_power9+x1_power9|x2_power9+x3_power9")

; Slice plus 3rd slot
(define_reservation "DU_slice_3_power9"
		    "x0_power9+xa0_power9|x1_power9+xa1_power9|
		     x2_power9+xb0_power9|x3_power9+xb1_power9")

; Superslice
(define_reservation "DU_super_power9"
		    "x0_power9+x1_power9|x2_power9+x3_power9")

; 2-way cracked
(define_reservation "DU_C2_power9" "x0_power9+x1_power9|
				    x1_power9+DU_xa_power9|
				    x1_power9+x2_power9|
				    DU_xa_power9+x2_power9|
				    x2_power9+x3_power9|
				    x3_power9+DU_xb_power9")

; 2-way cracked plus 3rd slot
(define_reservation "DU_C2_3_power9" "x0_power9+x1_power9+xa0_power9|
				      x1_power9+x2_power9+xa1_power9|
				      x2_power9+x3_power9+xb0_power9")

; 3-way cracked (consumes whole decode/dispatch cycle)
(define_reservation "DU_C3_power9"
		    "x0_power9+x1_power9+xa0_power9+xa1_power9+x2_power9+
		     x3_power9+xb0_power9+xb1_power9+br0_power9+br1_power9")

; Branch ports
(define_reservation "DU_branch_power9" "br0_power9|br1_power9")


; Execution unit reservations
(define_reservation "LSU_power9"
		    "lsu0_power9|lsu1_power9|lsu2_power9|lsu3_power9")

(define_reservation "LSU_pair_power9"
		    "lsu0_power9+lsu1_power9|lsu1_power9+lsu2_power9|
		     lsu2_power9+lsu3_power9|lsu3_power9+lsu0_power9")

(define_reservation "VSU_power9"
		    "vsu0_power9|vsu1_power9|vsu2_power9|vsu3_power9")

(define_reservation "VSU_super_power9"
		    "vsu0_power9+vsu1_power9|vsu2_power9+vsu3_power9")

(define_reservation "VSU_PRM_power9" "prm0_power9|prm1_power9")

; Define the reservation to be used by FP div/sqrt which allows other insns
; to be issued to the VSU, but blocks other div/sqrt for a number of cycles.
; Note that the number of cycles blocked varies depending on insn, but we
; just use the same number for all in order to keep the number of DFA states
; reasonable.
(define_reservation "FP_DIV_power9"
		    "fp_div0_power9*8|fp_div1_power9*8|fp_div2_power9*8|
		     fp_div3_power9*8")
(define_reservation "VEC_DIV_power9"
		    "fp_div0_power9*8+fp_div1_power9*8|
		     fp_div2_power9*8+fp_div3_power9*8")


; LS Unit
(define_insn_reservation "power9-load" 4
  (and (eq_attr "type" "load")
       (eq_attr "sign_extend" "no")
       (eq_attr "update" "no")
       (eq_attr "cpu" "power9"))
  "DU_any_power9,LSU_power9")

(define_insn_reservation "power9-load-update" 4
  (and (eq_attr "type" "load")
       (eq_attr "sign_extend" "no")
       (eq_attr "update" "yes")
       (eq_attr "cpu" "power9"))
  "DU_C2_power9,LSU_power9+VSU_power9")

(define_insn_reservation "power9-load-ext" 6
  (and (eq_attr "type" "load")
       (eq_attr "sign_extend" "yes")
       (eq_attr "update" "no")
       (eq_attr "cpu" "power9"))
  "DU_C2_power9,LSU_power9")

(define_insn_reservation "power9-load-ext-update" 6
  (and (eq_attr "type" "load")
       (eq_attr "sign_extend" "yes")
       (eq_attr "update" "yes")
       (eq_attr "cpu" "power9"))
  "DU_C3_power9,LSU_power9+VSU_power9")

(define_insn_reservation "power9-fpload-double" 4
  (and (eq_attr "type" "fpload")
       (eq_attr "update" "no")
       (eq_attr "size" "64")
       (eq_attr "cpu" "power9"))
  "DU_slice_3_power9,LSU_power9")

(define_insn_reservation "power9-fpload-update-double" 4
  (and (eq_attr "type" "fpload")
       (eq_attr "update" "yes")
       (eq_attr "size" "64")
       (eq_attr "cpu" "power9"))
  "DU_C2_3_power9,LSU_power9+VSU_power9")

; SFmode loads are cracked and have additional 2 cycles over DFmode
(define_insn_reservation "power9-fpload-single" 6
  (and (eq_attr "type" "fpload")
       (eq_attr "update" "no")
       (eq_attr "size" "32")
       (eq_attr "cpu" "power9"))
  "DU_C2_3_power9,LSU_power9")

(define_insn_reservation "power9-fpload-update-single" 6
  (and (eq_attr "type" "fpload")
       (eq_attr "update" "yes")
       (eq_attr "size" "32")
       (eq_attr "cpu" "power9"))
  "DU_C3_power9,LSU_power9+VSU_power9")

(define_insn_reservation "power9-vecload" 5
  (and (eq_attr "type" "vecload")
       (eq_attr "cpu" "power9"))
  "DU_any_power9,LSU_pair_power9")

; Store data can issue 2 cycles after AGEN issue, 3 cycles for vector store
(define_insn_reservation "power9-store" 0
  (and (eq_attr "type" "store")
       (eq_attr "update" "no")
       (eq_attr "indexed" "no")
       (eq_attr "cpu" "power9"))
  "DU_slice_3_power9,LSU_power9")

(define_insn_reservation "power9-store-indexed" 0
  (and (eq_attr "type" "store")
       (eq_attr "update" "no")
       (eq_attr "indexed" "yes")
       (eq_attr "cpu" "power9"))
  "DU_slice_3_power9,LSU_power9")

; Update forms have 2 cycle latency for updated addr reg
(define_insn_reservation "power9-store-update" 2
  (and (eq_attr "type" "store")
       (eq_attr "update" "yes")
       (eq_attr "indexed" "no")
       (eq_attr "cpu" "power9"))
  "DU_C2_3_power9,LSU_power9+VSU_power9")

; Update forms have 2 cycle latency for updated addr reg
(define_insn_reservation "power9-store-update-indexed" 2
  (and (eq_attr "type" "store")
       (eq_attr "update" "yes")
       (eq_attr "indexed" "yes")
       (eq_attr "cpu" "power9"))
  "DU_C2_3_power9,LSU_power9+VSU_power9")

(define_insn_reservation "power9-fpstore" 0
  (and (eq_attr "type" "fpstore")
       (eq_attr "update" "no")
       (eq_attr "cpu" "power9"))
  "DU_slice_3_power9,LSU_power9")

; Update forms have 2 cycle latency for updated addr reg
(define_insn_reservation "power9-fpstore-update" 2
  (and (eq_attr "type" "fpstore")
       (eq_attr "update" "yes")
       (eq_attr "cpu" "power9"))
  "DU_C2_3_power9,LSU_power9+VSU_power9")

(define_insn_reservation "power9-vecstore" 0
  (and (eq_attr "type" "vecstore")
       (eq_attr "cpu" "power9"))
  "DU_super_power9,LSU_pair_power9")

; Store forwarding latency is 6
(define_bypass 6 "power9-*store*" "power9-*load*")

(define_insn_reservation "power9-larx" 4
  (and (eq_attr "type" "load_l")
       (eq_attr "cpu" "power9"))
  "DU_any_power9,LSU_power9")

(define_insn_reservation "power9-stcx" 2
  (and (eq_attr "type" "store_c")
       (eq_attr "cpu" "power9"))
  "DU_C2_3_power9,LSU_power9+VSU_power9")

(define_insn_reservation "power9-sync" 4
  (and (eq_attr "type" "sync,isync")
       (eq_attr "cpu" "power9"))
  "DU_any_power9,LSU_power9")


; VSU Execution Unit

; Fixed point ops

; Most ALU insns are simple 2 cycle, including record form
(define_insn_reservation "power9-alu" 2
  (and (eq_attr "type" "add,exts,integer,logical,isel")
       (eq_attr "cpu" "power9"))
  "DU_any_power9,VSU_power9")
; 5 cycle CR latency
(define_bypass 5 "power9-alu"
		 "power9-crlogical,power9-mfcr,power9-mfcrf")

; Rotate/shift prevent use of third slot
(define_insn_reservation "power9-rot" 2
  (and (eq_attr "type" "insert,shift")
       (eq_attr "dot" "no")
       (eq_attr "cpu" "power9"))
  "DU_slice_3_power9,VSU_power9")

; Record form rotate/shift are cracked
(define_insn_reservation "power9-cracked-alu" 2
  (and (eq_attr "type" "insert,shift")
       (eq_attr "dot" "yes")
       (eq_attr "cpu" "power9"))
  "DU_C2_3_power9,VSU_power9")
; 7 cycle CR latency
(define_bypass 7 "power9-cracked-alu"
		 "power9-crlogical,power9-mfcr,power9-mfcrf")

(define_insn_reservation "power9-alu2" 3
  (and (eq_attr "type" "cntlz,popcnt,trap")
       (eq_attr "cpu" "power9"))
  "DU_any_power9,VSU_power9")
; 6 cycle CR latency
(define_bypass 6 "power9-alu2"
		 "power9-crlogical,power9-mfcr,power9-mfcrf")

(define_insn_reservation "power9-cmp" 2
  (and (eq_attr "type" "cmp")
       (eq_attr "cpu" "power9"))
  "DU_any_power9,VSU_power9")


; Treat 'two' and 'three' types as 2 or 3 way cracked
(define_insn_reservation "power9-two" 4
  (and (eq_attr "type" "two")
       (eq_attr "cpu" "power9"))
  "DU_C2_power9,VSU_power9")

(define_insn_reservation "power9-three" 6
  (and (eq_attr "type" "three")
       (eq_attr "cpu" "power9"))
  "DU_C3_power9,VSU_power9")

(define_insn_reservation "power9-mul" 5
  (and (eq_attr "type" "mul")
       (eq_attr "dot" "no")
       (eq_attr "cpu" "power9"))
  "DU_slice_3_power9,VSU_power9")

(define_insn_reservation "power9-mul-compare" 5
  (and (eq_attr "type" "mul")
       (eq_attr "dot" "yes")
       (eq_attr "cpu" "power9"))
  "DU_C2_3_power9,VSU_power9")
; 10 cycle CR latency
(define_bypass 10 "power9-mul-compare"
		 "power9-crlogical,power9-mfcr,power9-mfcrf")

; Fixed point divides reserve the divide units for a minimum of 8 cycles
(define_insn_reservation "power9-idiv" 16
  (and (eq_attr "type" "div")
       (eq_attr "size" "32")
       (eq_attr "cpu" "power9"))
  "DU_even_power9,fx_div0_power9*8|fx_div1_power9*8")

(define_insn_reservation "power9-ldiv" 24
  (and (eq_attr "type" "div")
       (eq_attr "size" "64")
       (eq_attr "cpu" "power9"))
  "DU_even_power9,fx_div0_power9*8|fx_div1_power9*8")

(define_insn_reservation "power9-crlogical" 2
  (and (eq_attr "type" "cr_logical")
       (eq_attr "cpu" "power9"))
  "DU_any_power9,VSU_power9")

(define_insn_reservation "power9-mfcrf" 2
  (and (eq_attr "type" "mfcrf")
       (eq_attr "cpu" "power9"))
  "DU_any_power9,VSU_power9")

(define_insn_reservation "power9-mfcr" 6
  (and (eq_attr "type" "mfcr")
       (eq_attr "cpu" "power9"))
  "DU_C3_power9,VSU_power9")

; Should differentiate between 1 cr field and > 1 since target of > 1 cr
; is cracked
(define_insn_reservation "power9-mtcr" 2
  (and (eq_attr "type" "mtcr")
       (eq_attr "cpu" "power9"))
  "DU_any_power9,VSU_power9")

; Move to LR/CTR are executed in VSU
(define_insn_reservation "power9-mtjmpr" 5
  (and (eq_attr "type" "mtjmpr")
       (eq_attr "cpu" "power9"))
  "DU_any_power9,VSU_power9")

; Floating point/Vector ops
(define_insn_reservation "power9-fpsimple" 2
  (and (eq_attr "type" "fpsimple")
       (eq_attr "cpu" "power9"))
  "DU_slice_3_power9,VSU_power9")

(define_insn_reservation "power9-fp" 5
  (and (eq_attr "type" "fp,dmul")
       (eq_attr "cpu" "power9"))
  "DU_slice_3_power9,VSU_power9")

(define_insn_reservation "power9-fpcompare" 3
  (and (eq_attr "type" "fpcompare")
       (eq_attr "cpu" "power9"))
  "DU_slice_3_power9,VSU_power9")

; FP div/sqrt are executed in the VSU slices.  They are not pipelined wrt other
; div/sqrt insns, but for the most part do not block pipelined ops.
(define_insn_reservation "power9-sdiv" 22
  (and (eq_attr "type" "sdiv")
       (eq_attr "cpu" "power9"))
  "DU_slice_3_power9,VSU_power9,FP_DIV_power9")

(define_insn_reservation "power9-ddiv" 27
  (and (eq_attr "type" "ddiv")
       (eq_attr "cpu" "power9"))
  "DU_slice_3_power9,VSU_power9,FP_DIV_power9")

(define_insn_reservation "power9-sqrt" 26
  (and (eq_attr "type" "ssqrt")
       (eq_attr "cpu" "power9"))
  "DU_slice_3_power9,VSU_power9,FP_DIV_power9")

(define_insn_reservation "power9-dsqrt" 36
  (and (eq_attr "type" "dsqrt")
       (eq_attr "cpu" "power9"))
  "DU_slice_3_power9,VSU_power9,FP_DIV_power9")

(define_insn_reservation "power9-vec-2cyc" 2
  (and (eq_attr "type" "vecmove,veclogical,vecexts,veccmpfx")
       (eq_attr "cpu" "power9"))
  "DU_super_power9,VSU_super_power9")

(define_insn_reservation "power9-veccmp" 3
  (and (eq_attr "type" "veccmp")
       (eq_attr "cpu" "power9"))
  "DU_super_power9,VSU_super_power9")

(define_insn_reservation "power9-vecsimple" 3
  (and (eq_attr "type" "vecsimple")
       (eq_attr "cpu" "power9"))
  "DU_super_power9,VSU_super_power9")

(define_insn_reservation "power9-vecnormal" 7
  (and (eq_attr "type" "vecfloat,vecdouble")
       (eq_attr "size" "!128")
       (eq_attr "cpu" "power9"))
  "DU_super_power9,VSU_super_power9")

; Quad-precision FP ops, execute in DFU
(define_insn_reservation "power9-qp" 12
  (and (eq_attr "type" "vecfloat,vecdouble")
       (eq_attr "size" "128")
       (eq_attr "cpu" "power9"))
  "DU_super_power9,dfu_power9")

(define_insn_reservation "power9-vecperm" 3
  (and (eq_attr "type" "vecperm")
       (eq_attr "cpu" "power9"))
  "DU_super_power9,VSU_PRM_power9")

(define_insn_reservation "power9-veccomplex" 7
  (and (eq_attr "type" "veccomplex")
       (eq_attr "cpu" "power9"))
  "DU_super_power9,VSU_super_power9")

(define_insn_reservation "power9-vecfdiv" 24
  (and (eq_attr "type" "vecfdiv")
       (eq_attr "cpu" "power9"))
  "DU_super_power9,VSU_super_power9,VEC_DIV_power9")

(define_insn_reservation "power9-vecdiv" 27
  (and (eq_attr "type" "vecdiv")
       (eq_attr "size" "!128")
       (eq_attr "cpu" "power9"))
  "DU_super_power9,VSU_super_power9,VEC_DIV_power9")

; Use 8 for DFU reservation on QP div/mul to limit DFA state size
(define_insn_reservation "power9-qpdiv" 56
  (and (eq_attr "type" "vecdiv")
       (eq_attr "size" "128")
       (eq_attr "cpu" "power9"))
  "DU_super_power9,dfu_power9*8")

(define_insn_reservation "power9-qpmul" 24
  (and (eq_attr "type" "qmul")
       (eq_attr "size" "128")
       (eq_attr "cpu" "power9"))
  "DU_super_power9,dfu_power9*8")

(define_insn_reservation "power9-mffgpr" 2
  (and (eq_attr "type" "mffgpr")
       (eq_attr "cpu" "power9"))
  "DU_slice_3_power9,VSU_power9")

(define_insn_reservation "power9-mftgpr" 2
  (and (eq_attr "type" "mftgpr")
       (eq_attr "cpu" "power9"))
  "DU_slice_3_power9,VSU_power9")


; Branch Unit
; Move from LR/CTR are executed in BRU but consume a writeback port from an
; execution slice.
(define_insn_reservation "power9-mfjmpr" 6
  (and (eq_attr "type" "mfjmpr")
       (eq_attr "cpu" "power9"))
  "DU_branch_power9,bru_power9+VSU_power9")

; Branch is 2 cycles
(define_insn_reservation "power9-branch" 2
  (and (eq_attr "type" "jmpreg,branch")
       (eq_attr "cpu" "power9"))
  "DU_branch_power9,bru_power9")


; Crypto Unit
(define_insn_reservation "power9-crypto" 6
  (and (eq_attr "type" "crypto")
       (eq_attr "cpu" "power9"))
  "DU_super_power9,cryptu_power9")


; HTM Unit
(define_insn_reservation "power9-htm" 4
  (and (eq_attr "type" "htm")
       (eq_attr "cpu" "power9"))
  "DU_C2_power9,LSU_power9")

(define_insn_reservation "power9-htm-simple" 2
  (and (eq_attr "type" "htmsimple")
       (eq_attr "cpu" "power9"))
  "DU_any_power9,VSU_power9")


; DFP Unit
(define_insn_reservation "power9-dfp" 12
  (and (eq_attr "type" "dfp")
       (eq_attr "cpu" "power9"))
  "DU_even_power9,dfu_power9")

