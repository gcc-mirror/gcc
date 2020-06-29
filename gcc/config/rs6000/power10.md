;; Scheduling description for IBM POWER10 processor.
;; Copyright (C) 2016-2020 Free Software Foundation, Inc.
;;
;; This is a clone of power9.md.  It is intended to be a placeholder until a
;; real scheduler model can be contributed.
;; The original power9.md was contributed by Pat Haugen (pthaugen@us.ibm.com).

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

;; This file was cloned from power9.md, it does not (yet) describe the actual
;; POWER10 processor.

(define_automaton "power10dsp,power10lsu,power10vsu,power10fpdiv,power10misc")

(define_cpu_unit "lsu0_power10,lsu1_power10,lsu2_power10,lsu3_power10" "power10lsu")
(define_cpu_unit "vsu0_power10,vsu1_power10,vsu2_power10,vsu3_power10" "power10vsu")
; Two vector permute units, part of vsu
(define_cpu_unit "prm0_power10,prm1_power10" "power10vsu")
; Two fixed point divide units, not pipelined
(define_cpu_unit "fx_div0_power10,fx_div1_power10" "power10misc")
(define_cpu_unit "bru_power10,cryptu_power10,dfu_power10" "power10misc")
; Create a false unit for use by non-pipelined FP div/sqrt
(define_cpu_unit "fp_div0_power10,fp_div1_power10,fp_div2_power10,fp_div3_power10"
		 "power10fpdiv")


(define_cpu_unit "x0_power10,x1_power10,xa0_power10,xa1_power10,
		  x2_power10,x3_power10,xb0_power10,xb1_power10,
		  br0_power10,br1_power10" "power10dsp")


; Dispatch port reservations
;
; The processor can dispatch a maximum of 6 iops per cycle with the following
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
(define_reservation "DU_xa_power10" "xa0_power10+xa1_power10")
(define_reservation "DU_xb_power10" "xb0_power10+xb1_power10")

; Any execution slice dispatch
(define_reservation "DU_any_power10"
		    "x0_power10|x1_power10|DU_xa_power10|x2_power10|x3_power10|
		     DU_xb_power10")

; Even slice, actually takes even/odd slots
(define_reservation "DU_even_power10" "x0_power10+x1_power10|x2_power10+x3_power10")

; Slice plus 3rd slot
(define_reservation "DU_slice_3_power10"
		    "x0_power10+xa0_power10|x1_power10+xa1_power10|
		     x2_power10+xb0_power10|x3_power10+xb1_power10")

; Superslice
(define_reservation "DU_super_power10"
		    "x0_power10+x1_power10|x2_power10+x3_power10")

; 2-way cracked
(define_reservation "DU_C2_power10" "x0_power10+x1_power10|
				    x1_power10+DU_xa_power10|
				    x1_power10+x2_power10|
				    DU_xa_power10+x2_power10|
				    x2_power10+x3_power10|
				    x3_power10+DU_xb_power10")

; 2-way cracked plus 3rd slot
(define_reservation "DU_C2_3_power10" "x0_power10+x1_power10+xa0_power10|
				      x1_power10+x2_power10+xa1_power10|
				      x2_power10+x3_power10+xb0_power10")

; 3-way cracked (consumes whole decode/dispatch cycle)
(define_reservation "DU_C3_power10"
		    "x0_power10+x1_power10+xa0_power10+xa1_power10+x2_power10+
		     x3_power10+xb0_power10+xb1_power10+br0_power10+br1_power10")

; Branch ports
(define_reservation "DU_branch_power10" "br0_power10|br1_power10")


; Execution unit reservations
(define_reservation "LSU_power10"
		    "lsu0_power10|lsu1_power10|lsu2_power10|lsu3_power10")

(define_reservation "LSU_pair_power10"
		    "lsu0_power10+lsu1_power10|lsu1_power10+lsu2_power10|
		     lsu2_power10+lsu3_power10|lsu3_power10+lsu0_power10")

(define_reservation "VSU_power10"
		    "vsu0_power10|vsu1_power10|vsu2_power10|vsu3_power10")

(define_reservation "VSU_super_power10"
		    "vsu0_power10+vsu1_power10|vsu2_power10+vsu3_power10")

(define_reservation "VSU_PRM_power10" "prm0_power10|prm1_power10")

; Define the reservation to be used by FP div/sqrt which allows other insns
; to be issued to the VSU, but blocks other div/sqrt for a number of cycles.
; Note that the number of cycles blocked varies depending on insn, but we
; just use the same number for all in order to keep the number of DFA states
; reasonable.
(define_reservation "FP_DIV_power10"
		    "fp_div0_power10*8|fp_div1_power10*8|fp_div2_power10*8|
		     fp_div3_power10*8")
(define_reservation "VEC_DIV_power10"
		    "fp_div0_power10*8+fp_div1_power10*8|
		     fp_div2_power10*8+fp_div3_power10*8")


; LS Unit
(define_insn_reservation "power10-load" 4
  (and (eq_attr "type" "load")
       (eq_attr "sign_extend" "no")
       (eq_attr "update" "no")
       (eq_attr "cpu" "power10"))
  "DU_any_power10,LSU_power10")

(define_insn_reservation "power10-load-update" 4
  (and (eq_attr "type" "load")
       (eq_attr "sign_extend" "no")
       (eq_attr "update" "yes")
       (eq_attr "cpu" "power10"))
  "DU_C2_power10,LSU_power10+VSU_power10")

(define_insn_reservation "power10-load-ext" 6
  (and (eq_attr "type" "load")
       (eq_attr "sign_extend" "yes")
       (eq_attr "update" "no")
       (eq_attr "cpu" "power10"))
  "DU_C2_power10,LSU_power10")

(define_insn_reservation "power10-load-ext-update" 6
  (and (eq_attr "type" "load")
       (eq_attr "sign_extend" "yes")
       (eq_attr "update" "yes")
       (eq_attr "cpu" "power10"))
  "DU_C3_power10,LSU_power10+VSU_power10")

(define_insn_reservation "power10-fpload-double" 4
  (and (eq_attr "type" "fpload")
       (eq_attr "update" "no")
       (eq_attr "size" "64")
       (eq_attr "cpu" "power10"))
  "DU_slice_3_power10,LSU_power10")

(define_insn_reservation "power10-fpload-update-double" 4
  (and (eq_attr "type" "fpload")
       (eq_attr "update" "yes")
       (eq_attr "size" "64")
       (eq_attr "cpu" "power10"))
  "DU_C2_3_power10,LSU_power10+VSU_power10")

; SFmode loads are cracked and have additional 2 cycles over DFmode
(define_insn_reservation "power10-fpload-single" 6
  (and (eq_attr "type" "fpload")
       (eq_attr "update" "no")
       (eq_attr "size" "32")
       (eq_attr "cpu" "power10"))
  "DU_C2_3_power10,LSU_power10")

(define_insn_reservation "power10-fpload-update-single" 6
  (and (eq_attr "type" "fpload")
       (eq_attr "update" "yes")
       (eq_attr "size" "32")
       (eq_attr "cpu" "power10"))
  "DU_C3_power10,LSU_power10+VSU_power10")

(define_insn_reservation "power10-vecload" 5
  (and (eq_attr "type" "vecload")
       (eq_attr "cpu" "power10"))
  "DU_any_power10,LSU_pair_power10")

; Store data can issue 2 cycles after AGEN issue, 3 cycles for vector store
(define_insn_reservation "power10-store" 0
  (and (eq_attr "type" "store")
       (eq_attr "update" "no")
       (eq_attr "indexed" "no")
       (eq_attr "cpu" "power10"))
  "DU_slice_3_power10,LSU_power10")

(define_insn_reservation "power10-store-indexed" 0
  (and (eq_attr "type" "store")
       (eq_attr "update" "no")
       (eq_attr "indexed" "yes")
       (eq_attr "cpu" "power10"))
  "DU_slice_3_power10,LSU_power10")

; Update forms have 2 cycle latency for updated addr reg
(define_insn_reservation "power10-store-update" 2
  (and (eq_attr "type" "store")
       (eq_attr "update" "yes")
       (eq_attr "indexed" "no")
       (eq_attr "cpu" "power10"))
  "DU_C2_3_power10,LSU_power10+VSU_power10")

; Update forms have 2 cycle latency for updated addr reg
(define_insn_reservation "power10-store-update-indexed" 2
  (and (eq_attr "type" "store")
       (eq_attr "update" "yes")
       (eq_attr "indexed" "yes")
       (eq_attr "cpu" "power10"))
  "DU_C2_3_power10,LSU_power10+VSU_power10")

(define_insn_reservation "power10-fpstore" 0
  (and (eq_attr "type" "fpstore")
       (eq_attr "update" "no")
       (eq_attr "cpu" "power10"))
  "DU_slice_3_power10,LSU_power10")

; Update forms have 2 cycle latency for updated addr reg
(define_insn_reservation "power10-fpstore-update" 2
  (and (eq_attr "type" "fpstore")
       (eq_attr "update" "yes")
       (eq_attr "cpu" "power10"))
  "DU_C2_3_power10,LSU_power10+VSU_power10")

(define_insn_reservation "power10-vecstore" 0
  (and (eq_attr "type" "vecstore")
       (eq_attr "cpu" "power10"))
  "DU_super_power10,LSU_pair_power10")

(define_insn_reservation "power10-larx" 4
  (and (eq_attr "type" "load_l")
       (eq_attr "cpu" "power10"))
  "DU_any_power10,LSU_power10")

(define_insn_reservation "power10-stcx" 2
  (and (eq_attr "type" "store_c")
       (eq_attr "cpu" "power10"))
  "DU_C2_3_power10,LSU_power10+VSU_power10")

(define_insn_reservation "power10-sync" 4
  (and (eq_attr "type" "sync,isync")
       (eq_attr "cpu" "power10"))
  "DU_any_power10,LSU_power10")


; VSU Execution Unit

; Fixed point ops

; Most ALU insns are simple 2 cycle, including record form
(define_insn_reservation "power10-alu" 2
  (and (eq_attr "type" "add,exts,integer,logical,isel")
       (eq_attr "cpu" "power10"))
  "DU_any_power10,VSU_power10")
; 5 cycle CR latency
(define_bypass 5 "power10-alu"
		 "power10-crlogical,power10-mfcr,power10-mfcrf")

; Rotate/shift prevent use of third slot
(define_insn_reservation "power10-rot" 2
  (and (eq_attr "type" "insert,shift")
       (eq_attr "dot" "no")
       (eq_attr "cpu" "power10"))
  "DU_slice_3_power10,VSU_power10")

; Record form rotate/shift are cracked
(define_insn_reservation "power10-cracked-alu" 2
  (and (eq_attr "type" "insert,shift")
       (eq_attr "dot" "yes")
       (eq_attr "cpu" "power10"))
  "DU_C2_3_power10,VSU_power10")
; 7 cycle CR latency
(define_bypass 7 "power10-cracked-alu"
		 "power10-crlogical,power10-mfcr,power10-mfcrf")

(define_insn_reservation "power10-alu2" 3
  (and (eq_attr "type" "cntlz,popcnt,trap")
       (eq_attr "cpu" "power10"))
  "DU_any_power10,VSU_power10")
; 6 cycle CR latency
(define_bypass 6 "power10-alu2"
		 "power10-crlogical,power10-mfcr,power10-mfcrf")

(define_insn_reservation "power10-cmp" 2
  (and (eq_attr "type" "cmp")
       (eq_attr "cpu" "power10"))
  "DU_any_power10,VSU_power10")


; Treat 'two' and 'three' types as 2 or 3 way cracked
(define_insn_reservation "power10-two" 4
  (and (eq_attr "type" "two")
       (eq_attr "cpu" "power10"))
  "DU_C2_power10,VSU_power10")

(define_insn_reservation "power10-three" 6
  (and (eq_attr "type" "three")
       (eq_attr "cpu" "power10"))
  "DU_C3_power10,VSU_power10")

(define_insn_reservation "power10-mul" 5
  (and (eq_attr "type" "mul")
       (eq_attr "dot" "no")
       (eq_attr "cpu" "power10"))
  "DU_slice_3_power10,VSU_power10")

(define_insn_reservation "power10-mul-compare" 5
  (and (eq_attr "type" "mul")
       (eq_attr "dot" "yes")
       (eq_attr "cpu" "power10"))
  "DU_C2_3_power10,VSU_power10")
; 10 cycle CR latency
(define_bypass 10 "power10-mul-compare"
		 "power10-crlogical,power10-mfcr,power10-mfcrf")

; Fixed point divides reserve the divide units for a minimum of 8 cycles
(define_insn_reservation "power10-idiv" 16
  (and (eq_attr "type" "div")
       (eq_attr "size" "32")
       (eq_attr "cpu" "power10"))
  "DU_even_power10,fx_div0_power10*8|fx_div1_power10*8")

(define_insn_reservation "power10-ldiv" 24
  (and (eq_attr "type" "div")
       (eq_attr "size" "64")
       (eq_attr "cpu" "power10"))
  "DU_even_power10,fx_div0_power10*8|fx_div1_power10*8")

(define_insn_reservation "power10-crlogical" 2
  (and (eq_attr "type" "cr_logical")
       (eq_attr "cpu" "power10"))
  "DU_any_power10,VSU_power10")

(define_insn_reservation "power10-mfcrf" 2
  (and (eq_attr "type" "mfcrf")
       (eq_attr "cpu" "power10"))
  "DU_any_power10,VSU_power10")

(define_insn_reservation "power10-mfcr" 6
  (and (eq_attr "type" "mfcr")
       (eq_attr "cpu" "power10"))
  "DU_C3_power10,VSU_power10")

; Should differentiate between 1 cr field and > 1 since target of > 1 cr
; is cracked
(define_insn_reservation "power10-mtcr" 2
  (and (eq_attr "type" "mtcr")
       (eq_attr "cpu" "power10"))
  "DU_any_power10,VSU_power10")

; Move to LR/CTR are executed in VSU
(define_insn_reservation "power10-mtjmpr" 5
  (and (eq_attr "type" "mtjmpr")
       (eq_attr "cpu" "power10"))
  "DU_any_power10,VSU_power10")

; Floating point/Vector ops
(define_insn_reservation "power10-fpsimple" 2
  (and (eq_attr "type" "fpsimple")
       (eq_attr "cpu" "power10"))
  "DU_slice_3_power10,VSU_power10")

(define_insn_reservation "power10-fp" 5
  (and (eq_attr "type" "fp,dmul")
       (eq_attr "cpu" "power10"))
  "DU_slice_3_power10,VSU_power10")

(define_insn_reservation "power10-fpcompare" 3
  (and (eq_attr "type" "fpcompare")
       (eq_attr "cpu" "power10"))
  "DU_slice_3_power10,VSU_power10")

; FP div/sqrt are executed in the VSU slices.  They are not pipelined wrt other
; div/sqrt insns, but for the most part do not block pipelined ops.
(define_insn_reservation "power10-sdiv" 22
  (and (eq_attr "type" "sdiv")
       (eq_attr "cpu" "power10"))
  "DU_slice_3_power10,VSU_power10,FP_DIV_power10")

(define_insn_reservation "power10-ddiv" 27
  (and (eq_attr "type" "ddiv")
       (eq_attr "cpu" "power10"))
  "DU_slice_3_power10,VSU_power10,FP_DIV_power10")

(define_insn_reservation "power10-sqrt" 26
  (and (eq_attr "type" "ssqrt")
       (eq_attr "cpu" "power10"))
  "DU_slice_3_power10,VSU_power10,FP_DIV_power10")

(define_insn_reservation "power10-dsqrt" 36
  (and (eq_attr "type" "dsqrt")
       (eq_attr "cpu" "power10"))
  "DU_slice_3_power10,VSU_power10,FP_DIV_power10")

(define_insn_reservation "power10-vec-2cyc" 2
  (and (eq_attr "type" "vecmove,veclogical,vecexts,veccmpfx")
       (eq_attr "cpu" "power10"))
  "DU_super_power10,VSU_super_power10")

(define_insn_reservation "power10-veccmp" 3
  (and (eq_attr "type" "veccmp")
       (eq_attr "cpu" "power10"))
  "DU_super_power10,VSU_super_power10")

(define_insn_reservation "power10-vecsimple" 3
  (and (eq_attr "type" "vecsimple")
       (eq_attr "cpu" "power10"))
  "DU_super_power10,VSU_super_power10")

(define_insn_reservation "power10-vecnormal" 7
  (and (eq_attr "type" "vecfloat,vecdouble")
       (eq_attr "size" "!128")
       (eq_attr "cpu" "power10"))
  "DU_super_power10,VSU_super_power10")

; Quad-precision FP ops, execute in DFU
(define_insn_reservation "power10-qp" 12
  (and (eq_attr "type" "vecfloat,vecdouble")
       (eq_attr "size" "128")
       (eq_attr "cpu" "power10"))
  "DU_super_power10,dfu_power10")

(define_insn_reservation "power10-vecperm" 3
  (and (eq_attr "type" "vecperm")
       (eq_attr "cpu" "power10"))
  "DU_super_power10,VSU_PRM_power10")

(define_insn_reservation "power10-veccomplex" 7
  (and (eq_attr "type" "veccomplex")
       (eq_attr "cpu" "power10"))
  "DU_super_power10,VSU_super_power10")

(define_insn_reservation "power10-vecfdiv" 24
  (and (eq_attr "type" "vecfdiv")
       (eq_attr "cpu" "power10"))
  "DU_super_power10,VSU_super_power10,VEC_DIV_power10")

(define_insn_reservation "power10-vecdiv" 27
  (and (eq_attr "type" "vecdiv")
       (eq_attr "size" "!128")
       (eq_attr "cpu" "power10"))
  "DU_super_power10,VSU_super_power10,VEC_DIV_power10")

; Use 8 for DFU reservation on QP div/mul to limit DFA state size
(define_insn_reservation "power10-qpdiv" 56
  (and (eq_attr "type" "vecdiv")
       (eq_attr "size" "128")
       (eq_attr "cpu" "power10"))
  "DU_super_power10,dfu_power10*8")

(define_insn_reservation "power10-qpmul" 24
  (and (eq_attr "type" "qmul")
       (eq_attr "size" "128")
       (eq_attr "cpu" "power10"))
  "DU_super_power10,dfu_power10*8")

(define_insn_reservation "power10-mffgpr" 2
  (and (eq_attr "type" "mffgpr")
       (eq_attr "cpu" "power10"))
  "DU_slice_3_power10,VSU_power10")

(define_insn_reservation "power10-mftgpr" 2
  (and (eq_attr "type" "mftgpr")
       (eq_attr "cpu" "power10"))
  "DU_slice_3_power10,VSU_power10")


; Branch Unit
; Move from LR/CTR are executed in BRU but consume a writeback port from an
; execution slice.
(define_insn_reservation "power10-mfjmpr" 6
  (and (eq_attr "type" "mfjmpr")
       (eq_attr "cpu" "power10"))
  "DU_branch_power10,bru_power10+VSU_power10")

; Branch is 2 cycles
(define_insn_reservation "power10-branch" 2
  (and (eq_attr "type" "jmpreg,branch")
       (eq_attr "cpu" "power10"))
  "DU_branch_power10,bru_power10")


; Crypto Unit
(define_insn_reservation "power10-crypto" 6
  (and (eq_attr "type" "crypto")
       (eq_attr "cpu" "power10"))
  "DU_super_power10,cryptu_power10")


; HTM Unit
(define_insn_reservation "power10-htm" 4
  (and (eq_attr "type" "htm")
       (eq_attr "cpu" "power10"))
  "DU_C2_power10,LSU_power10")

(define_insn_reservation "power10-htm-simple" 2
  (and (eq_attr "type" "htmsimple")
       (eq_attr "cpu" "power10"))
  "DU_any_power10,VSU_power10")


; DFP Unit
(define_insn_reservation "power10-dfp" 12
  (and (eq_attr "type" "dfp")
       (eq_attr "cpu" "power10"))
  "DU_even_power10,dfu_power10")

