;; Scheduling description for a future IBM processor.
;; Copyright (C) 2016-2019 Free Software Foundation, Inc.
;;
;; This is a clone of power9.md.  It is intended to be a placeholder until a
;; real scheduler module can be contributed.
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
;;
;; This file was cloned from power9.md.  In the future, we will have future
;; specific optimizations here.

(define_automaton "futuredsp,futurelsu,futurevsu,futurefpdiv,futuremisc")

(define_cpu_unit "lsu0_future,lsu1_future,lsu2_future,lsu3_future" "futurelsu")
(define_cpu_unit "vsu0_future,vsu1_future,vsu2_future,vsu3_future" "futurevsu")
; Two vector permute units, part of vsu
(define_cpu_unit "prm0_future,prm1_future" "futurevsu")
; Two fixed point divide units, not pipelined
(define_cpu_unit "fx_div0_future,fx_div1_future" "futuremisc")
(define_cpu_unit "bru_future,cryptu_future,dfu_future" "futuremisc")
; Create a false unit for use by non-pipelined FP div/sqrt
(define_cpu_unit "fp_div0_future,fp_div1_future,fp_div2_future,fp_div3_future"
		 "futurefpdiv")


(define_cpu_unit "x0_future,x1_future,xa0_future,xa1_future,
		  x2_future,x3_future,xb0_future,xb1_future,
		  br0_future,br1_future" "futuredsp")


; Dispatch port reservations
;
; Future can dispatch a maximum of 6 iops per cycle with the following
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
(define_reservation "DU_xa_future" "xa0_future+xa1_future")
(define_reservation "DU_xb_future" "xb0_future+xb1_future")

; Any execution slice dispatch
(define_reservation "DU_any_future"
		    "x0_future|x1_future|DU_xa_future|x2_future|x3_future|
		     DU_xb_future")

; Even slice, actually takes even/odd slots
(define_reservation "DU_even_future" "x0_future+x1_future|x2_future+x3_future")

; Slice plus 3rd slot
(define_reservation "DU_slice_3_future"
		    "x0_future+xa0_future|x1_future+xa1_future|
		     x2_future+xb0_future|x3_future+xb1_future")

; Superslice
(define_reservation "DU_super_future"
		    "x0_future+x1_future|x2_future+x3_future")

; 2-way cracked
(define_reservation "DU_C2_future" "x0_future+x1_future|
				    x1_future+DU_xa_future|
				    x1_future+x2_future|
				    DU_xa_future+x2_future|
				    x2_future+x3_future|
				    x3_future+DU_xb_future")

; 2-way cracked plus 3rd slot
(define_reservation "DU_C2_3_future" "x0_future+x1_future+xa0_future|
				      x1_future+x2_future+xa1_future|
				      x2_future+x3_future+xb0_future")

; 3-way cracked (consumes whole decode/dispatch cycle)
(define_reservation "DU_C3_future"
		    "x0_future+x1_future+xa0_future+xa1_future+x2_future+
		     x3_future+xb0_future+xb1_future+br0_future+br1_future")

; Branch ports
(define_reservation "DU_branch_future" "br0_future|br1_future")


; Execution unit reservations
(define_reservation "LSU_future"
		    "lsu0_future|lsu1_future|lsu2_future|lsu3_future")

(define_reservation "LSU_pair_future"
		    "lsu0_future+lsu1_future|lsu1_future+lsu2_future|
		     lsu2_future+lsu3_future|lsu3_future+lsu0_future")

(define_reservation "VSU_future"
		    "vsu0_future|vsu1_future|vsu2_future|vsu3_future")

(define_reservation "VSU_super_future"
		    "vsu0_future+vsu1_future|vsu2_future+vsu3_future")

(define_reservation "VSU_PRM_future" "prm0_future|prm1_future")

; Define the reservation to be used by FP div/sqrt which allows other insns
; to be issued to the VSU, but blocks other div/sqrt for a number of cycles.
; Note that the number of cycles blocked varies depending on insn, but we
; just use the same number for all in order to keep the number of DFA states
; reasonable.
(define_reservation "FP_DIV_future"
		    "fp_div0_future*8|fp_div1_future*8|fp_div2_future*8|
		     fp_div3_future*8")
(define_reservation "VEC_DIV_future"
		    "fp_div0_future*8+fp_div1_future*8|
		     fp_div2_future*8+fp_div3_future*8")


; LS Unit
(define_insn_reservation "future-load" 4
  (and (eq_attr "type" "load")
       (eq_attr "sign_extend" "no")
       (eq_attr "update" "no")
       (eq_attr "cpu" "future"))
  "DU_any_future,LSU_future")

(define_insn_reservation "future-load-update" 4
  (and (eq_attr "type" "load")
       (eq_attr "sign_extend" "no")
       (eq_attr "update" "yes")
       (eq_attr "cpu" "future"))
  "DU_C2_future,LSU_future+VSU_future")

(define_insn_reservation "future-load-ext" 6
  (and (eq_attr "type" "load")
       (eq_attr "sign_extend" "yes")
       (eq_attr "update" "no")
       (eq_attr "cpu" "future"))
  "DU_C2_future,LSU_future")

(define_insn_reservation "future-load-ext-update" 6
  (and (eq_attr "type" "load")
       (eq_attr "sign_extend" "yes")
       (eq_attr "update" "yes")
       (eq_attr "cpu" "future"))
  "DU_C3_future,LSU_future+VSU_future")

(define_insn_reservation "future-fpload-double" 4
  (and (eq_attr "type" "fpload")
       (eq_attr "update" "no")
       (eq_attr "size" "64")
       (eq_attr "cpu" "future"))
  "DU_slice_3_future,LSU_future")

(define_insn_reservation "future-fpload-update-double" 4
  (and (eq_attr "type" "fpload")
       (eq_attr "update" "yes")
       (eq_attr "size" "64")
       (eq_attr "cpu" "future"))
  "DU_C2_3_future,LSU_future+VSU_future")

; SFmode loads are cracked and have additional 2 cycles over DFmode
(define_insn_reservation "future-fpload-single" 6
  (and (eq_attr "type" "fpload")
       (eq_attr "update" "no")
       (eq_attr "size" "32")
       (eq_attr "cpu" "future"))
  "DU_C2_3_future,LSU_future")

(define_insn_reservation "future-fpload-update-single" 6
  (and (eq_attr "type" "fpload")
       (eq_attr "update" "yes")
       (eq_attr "size" "32")
       (eq_attr "cpu" "future"))
  "DU_C3_future,LSU_future+VSU_future")

(define_insn_reservation "future-vecload" 5
  (and (eq_attr "type" "vecload")
       (eq_attr "cpu" "future"))
  "DU_any_future,LSU_pair_future")

; Store data can issue 2 cycles after AGEN issue, 3 cycles for vector store
(define_insn_reservation "future-store" 0
  (and (eq_attr "type" "store")
       (eq_attr "update" "no")
       (eq_attr "indexed" "no")
       (eq_attr "cpu" "future"))
  "DU_slice_3_future,LSU_future")

(define_insn_reservation "future-store-indexed" 0
  (and (eq_attr "type" "store")
       (eq_attr "update" "no")
       (eq_attr "indexed" "yes")
       (eq_attr "cpu" "future"))
  "DU_slice_3_future,LSU_future")

; Update forms have 2 cycle latency for updated addr reg
(define_insn_reservation "future-store-update" 2
  (and (eq_attr "type" "store")
       (eq_attr "update" "yes")
       (eq_attr "indexed" "no")
       (eq_attr "cpu" "future"))
  "DU_C2_3_future,LSU_future+VSU_future")

; Update forms have 2 cycle latency for updated addr reg
(define_insn_reservation "future-store-update-indexed" 2
  (and (eq_attr "type" "store")
       (eq_attr "update" "yes")
       (eq_attr "indexed" "yes")
       (eq_attr "cpu" "future"))
  "DU_C2_3_future,LSU_future+VSU_future")

(define_insn_reservation "future-fpstore" 0
  (and (eq_attr "type" "fpstore")
       (eq_attr "update" "no")
       (eq_attr "cpu" "future"))
  "DU_slice_3_future,LSU_future")

; Update forms have 2 cycle latency for updated addr reg
(define_insn_reservation "future-fpstore-update" 2
  (and (eq_attr "type" "fpstore")
       (eq_attr "update" "yes")
       (eq_attr "cpu" "future"))
  "DU_C2_3_future,LSU_future+VSU_future")

(define_insn_reservation "future-vecstore" 0
  (and (eq_attr "type" "vecstore")
       (eq_attr "cpu" "future"))
  "DU_super_future,LSU_pair_future")

(define_insn_reservation "future-larx" 4
  (and (eq_attr "type" "load_l")
       (eq_attr "cpu" "future"))
  "DU_any_future,LSU_future")

(define_insn_reservation "future-stcx" 2
  (and (eq_attr "type" "store_c")
       (eq_attr "cpu" "future"))
  "DU_C2_3_future,LSU_future+VSU_future")

(define_insn_reservation "future-sync" 4
  (and (eq_attr "type" "sync,isync")
       (eq_attr "cpu" "future"))
  "DU_any_future,LSU_future")


; VSU Execution Unit

; Fixed point ops

; Most ALU insns are simple 2 cycle, including record form
(define_insn_reservation "future-alu" 2
  (and (eq_attr "type" "add,exts,integer,logical,isel")
       (eq_attr "cpu" "future"))
  "DU_any_future,VSU_future")
; 5 cycle CR latency
(define_bypass 5 "future-alu"
		 "future-crlogical,future-mfcr,future-mfcrf")

; Rotate/shift prevent use of third slot
(define_insn_reservation "future-rot" 2
  (and (eq_attr "type" "insert,shift")
       (eq_attr "dot" "no")
       (eq_attr "cpu" "future"))
  "DU_slice_3_future,VSU_future")

; Record form rotate/shift are cracked
(define_insn_reservation "future-cracked-alu" 2
  (and (eq_attr "type" "insert,shift")
       (eq_attr "dot" "yes")
       (eq_attr "cpu" "future"))
  "DU_C2_3_future,VSU_future")
; 7 cycle CR latency
(define_bypass 7 "future-cracked-alu"
		 "future-crlogical,future-mfcr,future-mfcrf")

(define_insn_reservation "future-alu2" 3
  (and (eq_attr "type" "cntlz,popcnt,trap")
       (eq_attr "cpu" "future"))
  "DU_any_future,VSU_future")
; 6 cycle CR latency
(define_bypass 6 "future-alu2"
		 "future-crlogical,future-mfcr,future-mfcrf")

(define_insn_reservation "future-cmp" 2
  (and (eq_attr "type" "cmp")
       (eq_attr "cpu" "future"))
  "DU_any_future,VSU_future")


; Treat 'two' and 'three' types as 2 or 3 way cracked
(define_insn_reservation "future-two" 4
  (and (eq_attr "type" "two")
       (eq_attr "cpu" "future"))
  "DU_C2_future,VSU_future")

(define_insn_reservation "future-three" 6
  (and (eq_attr "type" "three")
       (eq_attr "cpu" "future"))
  "DU_C3_future,VSU_future")

(define_insn_reservation "future-mul" 5
  (and (eq_attr "type" "mul")
       (eq_attr "dot" "no")
       (eq_attr "cpu" "future"))
  "DU_slice_3_future,VSU_future")

(define_insn_reservation "future-mul-compare" 5
  (and (eq_attr "type" "mul")
       (eq_attr "dot" "yes")
       (eq_attr "cpu" "future"))
  "DU_C2_3_future,VSU_future")
; 10 cycle CR latency
(define_bypass 10 "future-mul-compare"
		 "future-crlogical,future-mfcr,future-mfcrf")

; Fixed point divides reserve the divide units for a minimum of 8 cycles
(define_insn_reservation "future-idiv" 16
  (and (eq_attr "type" "div")
       (eq_attr "size" "32")
       (eq_attr "cpu" "future"))
  "DU_even_future,fx_div0_future*8|fx_div1_future*8")

(define_insn_reservation "future-ldiv" 24
  (and (eq_attr "type" "div")
       (eq_attr "size" "64")
       (eq_attr "cpu" "future"))
  "DU_even_future,fx_div0_future*8|fx_div1_future*8")

(define_insn_reservation "future-crlogical" 2
  (and (eq_attr "type" "cr_logical")
       (eq_attr "cpu" "future"))
  "DU_any_future,VSU_future")

(define_insn_reservation "future-mfcrf" 2
  (and (eq_attr "type" "mfcrf")
       (eq_attr "cpu" "future"))
  "DU_any_future,VSU_future")

(define_insn_reservation "future-mfcr" 6
  (and (eq_attr "type" "mfcr")
       (eq_attr "cpu" "future"))
  "DU_C3_future,VSU_future")

; Should differentiate between 1 cr field and > 1 since target of > 1 cr
; is cracked
(define_insn_reservation "future-mtcr" 2
  (and (eq_attr "type" "mtcr")
       (eq_attr "cpu" "future"))
  "DU_any_future,VSU_future")

; Move to LR/CTR are executed in VSU
(define_insn_reservation "future-mtjmpr" 5
  (and (eq_attr "type" "mtjmpr")
       (eq_attr "cpu" "future"))
  "DU_any_future,VSU_future")

; Floating point/Vector ops
(define_insn_reservation "future-fpsimple" 2
  (and (eq_attr "type" "fpsimple")
       (eq_attr "cpu" "future"))
  "DU_slice_3_future,VSU_future")

(define_insn_reservation "future-fp" 5
  (and (eq_attr "type" "fp,dmul")
       (eq_attr "cpu" "future"))
  "DU_slice_3_future,VSU_future")

(define_insn_reservation "future-fpcompare" 3
  (and (eq_attr "type" "fpcompare")
       (eq_attr "cpu" "future"))
  "DU_slice_3_future,VSU_future")

; FP div/sqrt are executed in the VSU slices.  They are not pipelined wrt other
; div/sqrt insns, but for the most part do not block pipelined ops.
(define_insn_reservation "future-sdiv" 22
  (and (eq_attr "type" "sdiv")
       (eq_attr "cpu" "future"))
  "DU_slice_3_future,VSU_future,FP_DIV_future")

(define_insn_reservation "future-ddiv" 27
  (and (eq_attr "type" "ddiv")
       (eq_attr "cpu" "future"))
  "DU_slice_3_future,VSU_future,FP_DIV_future")

(define_insn_reservation "future-sqrt" 26
  (and (eq_attr "type" "ssqrt")
       (eq_attr "cpu" "future"))
  "DU_slice_3_future,VSU_future,FP_DIV_future")

(define_insn_reservation "future-dsqrt" 36
  (and (eq_attr "type" "dsqrt")
       (eq_attr "cpu" "future"))
  "DU_slice_3_future,VSU_future,FP_DIV_future")

(define_insn_reservation "future-vec-2cyc" 2
  (and (eq_attr "type" "vecmove,veclogical,vecexts,veccmpfx")
       (eq_attr "cpu" "future"))
  "DU_super_future,VSU_super_future")

(define_insn_reservation "future-veccmp" 3
  (and (eq_attr "type" "veccmp")
       (eq_attr "cpu" "future"))
  "DU_super_future,VSU_super_future")

(define_insn_reservation "future-vecsimple" 3
  (and (eq_attr "type" "vecsimple")
       (eq_attr "cpu" "future"))
  "DU_super_future,VSU_super_future")

(define_insn_reservation "future-vecnormal" 7
  (and (eq_attr "type" "vecfloat,vecdouble")
       (eq_attr "size" "!128")
       (eq_attr "cpu" "future"))
  "DU_super_future,VSU_super_future")

; Quad-precision FP ops, execute in DFU
(define_insn_reservation "future-qp" 12
  (and (eq_attr "type" "vecfloat,vecdouble")
       (eq_attr "size" "128")
       (eq_attr "cpu" "future"))
  "DU_super_future,dfu_future")

(define_insn_reservation "future-vecperm" 3
  (and (eq_attr "type" "vecperm")
       (eq_attr "cpu" "future"))
  "DU_super_future,VSU_PRM_future")

(define_insn_reservation "future-veccomplex" 7
  (and (eq_attr "type" "veccomplex")
       (eq_attr "cpu" "future"))
  "DU_super_future,VSU_super_future")

(define_insn_reservation "future-vecfdiv" 24
  (and (eq_attr "type" "vecfdiv")
       (eq_attr "cpu" "future"))
  "DU_super_future,VSU_super_future,VEC_DIV_future")

(define_insn_reservation "future-vecdiv" 27
  (and (eq_attr "type" "vecdiv")
       (eq_attr "size" "!128")
       (eq_attr "cpu" "future"))
  "DU_super_future,VSU_super_future,VEC_DIV_future")

; Use 8 for DFU reservation on QP div/mul to limit DFA state size
(define_insn_reservation "future-qpdiv" 56
  (and (eq_attr "type" "vecdiv")
       (eq_attr "size" "128")
       (eq_attr "cpu" "future"))
  "DU_super_future,dfu_future*8")

(define_insn_reservation "future-qpmul" 24
  (and (eq_attr "type" "qmul")
       (eq_attr "size" "128")
       (eq_attr "cpu" "future"))
  "DU_super_future,dfu_future*8")

(define_insn_reservation "future-mffgpr" 2
  (and (eq_attr "type" "mffgpr")
       (eq_attr "cpu" "future"))
  "DU_slice_3_future,VSU_future")

(define_insn_reservation "future-mftgpr" 2
  (and (eq_attr "type" "mftgpr")
       (eq_attr "cpu" "future"))
  "DU_slice_3_future,VSU_future")


; Branch Unit
; Move from LR/CTR are executed in BRU but consume a writeback port from an
; execution slice.
(define_insn_reservation "future-mfjmpr" 6
  (and (eq_attr "type" "mfjmpr")
       (eq_attr "cpu" "future"))
  "DU_branch_future,bru_future+VSU_future")

; Branch is 2 cycles
(define_insn_reservation "future-branch" 2
  (and (eq_attr "type" "jmpreg,branch")
       (eq_attr "cpu" "future"))
  "DU_branch_future,bru_future")


; Crypto Unit
(define_insn_reservation "future-crypto" 6
  (and (eq_attr "type" "crypto")
       (eq_attr "cpu" "future"))
  "DU_super_future,cryptu_future")


; HTM Unit
(define_insn_reservation "future-htm" 4
  (and (eq_attr "type" "htm")
       (eq_attr "cpu" "future"))
  "DU_C2_future,LSU_future")

(define_insn_reservation "future-htm-simple" 2
  (and (eq_attr "type" "htmsimple")
       (eq_attr "cpu" "future"))
  "DU_any_future,VSU_future")


; DFP Unit
(define_insn_reservation "future-dfp" 12
  (and (eq_attr "type" "dfp")
       (eq_attr "cpu" "future"))
  "DU_even_future,dfu_future")

