;; Scheduling description for cell processor.
;; Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2009
;; Free Software Foundation, Inc.
;; Contributed by Sony Computer Entertainment, Inc.,


;; This file is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3 of the License, or (at your option) 
;; any later version.

;; This file is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;; Sources: BE BOOK4 (/sfs/enc/doc/PPU_BookIV_DD3.0_latest.pdf)

;; BE Architecture *DD3.0 and DD3.1*
;; This file simulate PPU processor unit backend of pipeline, maualP24. 
;; manual P27, stall and flush points
;; IU, XU, VSU, dispatcher decodes and dispatch 2 insns per cycle in program
;;  order, the grouped address are aligned by 8
;; This file only simulate one thread situation
;; XU executes all fixed point insns(3 units, a simple alu, a complex unit,
;;   and load/store unit)
;; VSU executes all scalar floating points insn(a float unit),
;;   VMX insns(VMX unit, 4 sub units, simple, permute, complex, floating point)

;; Dual issue combination

;;	FXU	LSU	BR 	        VMX	               VMX
;;                             (sx,cx,vsu_fp,fp_arith)    (perm,vsu_ls,fp_ls)
;;FXU	X
;;LSU		X               	X               	X	
;;BR			X
;;VMX(sx,cx,vsu_fp,fp_arth)		X
;;VMX(perm,vsu_ls, fp_ls)					X
;;    X are illegal combination.

;; Dual issue exceptions:
;;(1) nop-pipelined FXU instr in slot 0 
;;(2) non-pipelined FPU inst in slot 0
;; CSI instr(contex-synchronizing insn)
;; Microcode insn

;; BRU unit: bru(none register stall), bru_cr(cr register stall)
;; VSU unit: vus(vmx simple), vup(vmx permute), vuc(vmx complex),
;;  vuf(vmx float), fpu(floats). fpu_div is hypothetical, it is for
;;  nonpipelined simulation
;; micr insns will stall at least 7 cycles to get the first instr from ROM,
;;  micro instructions are not dual issued. 

;; slot0 is older than slot1
;; non-pipelined insn need to be in slot1 to avoid 1cycle stall

;; There different stall point
;; IB2, only stall one thread if stall here, so try to stall here as much as
;; we can 
;; condition(1) insert nop, OR and ORI instruction form 
;; condition(2) flush happens, in case of: RAW, WAW, D-ERAT miss, or
;;   CR0-access while stdcx, or stwcx
;; IS2 stall ;; Page91 for details
;; VQ8 stall
;; IS2 stall can be activated by VQ8 stall and trying to issue a vsu instr to
;;  the vsu issue queue

;;(define_automaton "cellxu")

;;(define_cpu_unit "fxu_cell,lsu_cell,bru_cell,vsu1_cell,vsu2_cell" "cellxu")

;; ndfa
(define_automaton "cellxu,cellvsu,cellbru,cell_mis")

(define_cpu_unit "fxu_cell,lsu_cell" "cellxu")
(define_cpu_unit "bru_cell" "cellbru")
(define_cpu_unit "vsu1_cell,vsu2_cell" "cellvsu")

(define_cpu_unit "slot0,slot1" "cell_mis")

(absence_set "slot0" "slot1")

(define_reservation "nonpipeline" "fxu_cell+lsu_cell+vsu1_cell+vsu2_cell")
(define_reservation "slot01" "slot0|slot1")


;; Load/store
;; lmw, lswi, lswx are only generated for optimize for space, MC,
;;   these instr are not simulated
(define_insn_reservation "cell-load" 2
  (and (eq_attr "type" "load")
       (eq_attr "cpu" "cell"))
  "slot01,lsu_cell")

;; ldux, ldu, lbzux, lbzu, hardware breaks it down to two instrs,
;;  if with 32bytes alignment, CMC
(define_insn_reservation "cell-load-ux" 2
  (and (eq_attr "type" "load_ux,load_u")
       (eq_attr "cpu" "cell"))
  "slot01,fxu_cell+lsu_cell")

;; lha, lhax, lhau, lhaux, lwa, lwax, lwaux, MC, latency unknown
;;   11/7, 11/8, 11/12
(define_insn_reservation "cell-load-ext" 2
  (and (eq_attr "type" "load_ext,load_ext_u,load_ext_ux")
       (eq_attr "cpu" "cell")) 
  "slot01,fxu_cell+lsu_cell")

;;lfs,lfsx,lfd,lfdx, 1 cycle
(define_insn_reservation "cell-fpload" 1
  (and (eq_attr "type" "fpload")
       (eq_attr "cpu" "cell"))
  "vsu2_cell+lsu_cell+slot01")

;; lfsu,lfsux,lfdu,lfdux 1cycle(fpr) 2 cycle(gpr)
(define_insn_reservation "cell-fpload-update" 1
  (and (eq_attr "type" "fpload,fpload_u,fpload_ux")
       (eq_attr "cpu" "cell"))
  "fxu_cell+vsu2_cell+lsu_cell+slot01")

(define_insn_reservation "cell-vecload" 2
  (and (eq_attr "type" "vecload")
       (eq_attr "cpu" "cell"))
  "slot01,vsu2_cell+lsu_cell")

;;st? stw(MC)
(define_insn_reservation "cell-store" 1
  (and (eq_attr "type" "store")
       (eq_attr "cpu" "cell"))
  "lsu_cell+slot01")

;;stdux, stdu, (hardware breaks into store and add) 2 for update reg
(define_insn_reservation "cell-store-update" 1
  (and (eq_attr "type" "store_ux,store_u")
       (eq_attr "cpu" "cell"))
  "fxu_cell+lsu_cell+slot01")

(define_insn_reservation "cell-fpstore" 1
  (and (eq_attr "type" "fpstore")
       (eq_attr "cpu" "cell"))
  "vsu2_cell+lsu_cell+slot01")

(define_insn_reservation "cell-fpstore-update" 1
  (and (eq_attr "type" "fpstore_ux,fpstore_u")
       (eq_attr "cpu" "cell"))
  "vsu2_cell+fxu_cell+lsu_cell+slot01")

(define_insn_reservation "cell-vecstore" 1
  (and (eq_attr "type" "vecstore")
       (eq_attr "cpu" "cell"))
  "vsu2_cell+lsu_cell+slot01")

;; Integer latency is 2 cycles
(define_insn_reservation "cell-integer" 2
  (and (eq_attr "type" "integer,insert_dword,shift,trap,\
			var_shift_rotate,cntlz,exts,isel")
       (eq_attr "cpu" "cell"))
  "slot01,fxu_cell")

;; Two integer latency is 4 cycles
(define_insn_reservation "cell-two" 4
  (and (eq_attr "type" "two")
       (eq_attr "cpu" "cell"))
  "slot01,fxu_cell,fxu_cell*2")

;; Three integer latency is 6 cycles
(define_insn_reservation "cell-three" 6
  (and (eq_attr "type" "three")
       (eq_attr "cpu" "cell"))
  "slot01,fxu_cell,fxu_cell*4")

;; rlwimi, alter cr0  
(define_insn_reservation "cell-insert" 2
  (and (eq_attr "type" "insert_word")
       (eq_attr "cpu" "cell"))
 "slot01,fxu_cell")

;; cmpi, cmpli, cmpla, add, addo, sub, subo, alter cr0 
(define_insn_reservation "cell-cmp" 1
  (and (eq_attr "type" "cmp")
       (eq_attr "cpu" "cell"))
  "fxu_cell+slot01")

;; add, addo, sub, subo, alter cr0, rldcli, rlwinm 
(define_insn_reservation "cell-fast-cmp" 2
  (and (and (eq_attr "type" "fast_compare,delayed_compare,compare,\
			    var_delayed_compare")
            (eq_attr "cpu" "cell"))
        (eq_attr "cell_micro" "not"))
  "slot01,fxu_cell")

(define_insn_reservation "cell-cmp-microcoded" 9
  (and (and (eq_attr "type" "fast_compare,delayed_compare,compare,\
			    var_delayed_compare")
            (eq_attr "cpu" "cell"))
        (eq_attr "cell_micro" "always"))
  "slot0+slot1,fxu_cell,fxu_cell*7")

;; mulld
(define_insn_reservation "cell-lmul" 15
  (and (eq_attr "type" "lmul")
       (eq_attr "cpu" "cell"))
  "slot1,nonpipeline,nonpipeline*13")

;; mulld. is microcoded
(define_insn_reservation "cell-lmul-cmp" 22
  (and (eq_attr "type" "lmul_compare")
       (eq_attr "cpu" "cell"))
  "slot0+slot1,nonpipeline,nonpipeline*20")

;; mulli, 6 cycles
(define_insn_reservation "cell-imul23" 6
  (and (eq_attr "type" "imul2,imul3")
       (eq_attr "cpu" "cell"))
  "slot1,nonpipeline,nonpipeline*4")

;; mullw, 9
(define_insn_reservation "cell-imul" 9
  (and (eq_attr "type" "imul")
       (eq_attr "cpu" "cell"))
  "slot1,nonpipeline,nonpipeline*7")
 
;; divide
(define_insn_reservation "cell-idiv" 32
  (and (eq_attr "type" "idiv")
       (eq_attr "cpu" "cell"))
  "slot1,nonpipeline,nonpipeline*30")

(define_insn_reservation "cell-ldiv" 64
  (and (eq_attr "type" "ldiv")
       (eq_attr "cpu" "cell"))
  "slot1,nonpipeline,nonpipeline*62")

;;mflr and mfctr are pipelined
(define_insn_reservation "cell-mfjmpr" 1
  (and (eq_attr "type" "mfjmpr")
       (eq_attr "cpu" "cell"))
  "slot01+bru_cell")

;;mtlr and mtctr,
;;mtspr fully pipelined 
(define_insn_reservation "cell-mtjmpr" 1
 (and (eq_attr "type" "mtjmpr")
       (eq_attr "cpu" "cell"))
  "bru_cell+slot01")

;; Branches
;; b, ba, bl, bla, unconditional branch always predicts correctly n/a latency
;; bcctr, bcctrl, latency 2, actually adjust by be to 4
(define_insn_reservation "cell-branch" 1
  (and (eq_attr "type" "branch")
       (eq_attr "cpu" "cell"))
  "bru_cell+slot1")

(define_insn_reservation "cell-branchreg" 1
  (and (eq_attr "type" "jmpreg")
       (eq_attr "cpu" "cell"))
  "bru_cell+slot1")

;; cr hazard
;; page 90, special cases for CR hazard, only one instr can access cr per cycle
;; if insn reads CR following a stwcx, pipeline stall till stwcx finish
(define_insn_reservation "cell-crlogical" 1
  (and (eq_attr "type" "cr_logical,delayed_cr")
       (eq_attr "cpu" "cell"))
  "bru_cell+slot01")

;; mfcrf and mfcr is about 34 cycles and nonpipelined
(define_insn_reservation "cell-mfcr" 34
  (and (eq_attr "type" "mfcrf,mfcr")
       (eq_attr "cpu" "cell"))
   "slot1,nonpipeline,nonpipeline*32")

;; mtcrf (1 field)
(define_insn_reservation "cell-mtcrf" 1
  (and (eq_attr "type" "mtcr")
       (eq_attr "cpu" "cell"))
  "fxu_cell+slot01")

; Basic FP latency is 10 cycles, thoughput is 1/cycle
(define_insn_reservation "cell-fp" 10
  (and (eq_attr "type" "fp,dmul")
       (eq_attr "cpu" "cell"))
  "slot01,vsu1_cell,vsu1_cell*8")

(define_insn_reservation "cell-fpcompare" 1
  (and (eq_attr "type" "fpcompare")
       (eq_attr "cpu" "cell"))
  "vsu1_cell+slot01")

;; sdiv thoughput 1/74, not pipelined but only in the FPU
(define_insn_reservation "cell-sdiv" 74
  (and (eq_attr "type" "sdiv,ddiv")
       (eq_attr "cpu" "cell"))
  "slot1,nonpipeline,nonpipeline*72")

;; fsqrt thoughput 1/84, not pipelined but only in the FPU
(define_insn_reservation "cell-sqrt" 84
  (and (eq_attr "type" "ssqrt,dsqrt")
       (eq_attr "cpu" "cell"))
  "slot1,nonpipeline,nonpipeline*82")

; VMX
(define_insn_reservation "cell-vecsimple" 4
  (and (eq_attr "type" "vecsimple")
       (eq_attr "cpu" "cell"))
  "slot01,vsu1_cell,vsu1_cell*2")

;; mult, div, madd
(define_insn_reservation "cell-veccomplex" 10
  (and (eq_attr "type" "veccomplex")
       (eq_attr "cpu" "cell"))
  "slot01,vsu1_cell,vsu1_cell*8")

;; TODO: add support for recording instructions
(define_insn_reservation "cell-veccmp" 4
  (and (eq_attr "type" "veccmp")
       (eq_attr "cpu" "cell"))
  "slot01,vsu1_cell,vsu1_cell*2")

(define_insn_reservation "cell-vecfloat" 12
  (and (eq_attr "type" "vecfloat")
       (eq_attr "cpu" "cell"))
  "slot01,vsu1_cell,vsu1_cell*10")

(define_insn_reservation "cell-vecperm" 4
  (and (eq_attr "type" "vecperm")
       (eq_attr "cpu" "cell"))
  "slot01,vsu2_cell,vsu2_cell*2")

;; New for 4.2, syncs

(define_insn_reservation "cell-sync" 11
  (and (eq_attr "type" "sync")
       (eq_attr "cpu" "cell"))
  "slot01,lsu_cell,lsu_cell*9")

(define_insn_reservation "cell-isync" 11
  (and (eq_attr "type" "isync")
       (eq_attr "cpu" "cell"))
  "slot01,lsu_cell,lsu_cell*9")

(define_insn_reservation "cell-load_l" 11
  (and (eq_attr "type" "load_l")
       (eq_attr "cpu" "cell"))
  "slot01,lsu_cell,lsu_cell*9")

(define_insn_reservation "cell-store_c" 11
  (and (eq_attr "type" "store_c")
       (eq_attr "cpu" "cell"))
  "slot01,lsu_cell,lsu_cell*9")

;; RAW register dependency

;; addi r3, r3, 1
;; lw r4,offset(r3)
;; there are 5 cycle deplay for r3 bypassing
;; there are 5 cycle delay for a dependent load after a load
(define_bypass 5 "cell-integer" "cell-load")
(define_bypass 5 "cell-integer" "cell-load-ext")
(define_bypass 5 "cell-load,cell-load-ext" "cell-load,cell-load-ext")

;; there is a 6 cycle delay after a fp compare until you can use the cr.
(define_bypass 6 "cell-fpcompare" "cell-branch,cell-branchreg,cell-mfcr,cell-crlogical")

;; VXU float RAW
(define_bypass 11 "cell-vecfloat" "cell-vecfloat")

;; VXU and FPU
(define_bypass 6 "cell-veccomplex" "cell-vecsimple")
;;(define_bypass 6 "cell-veccompare" "cell-branch,cell-branchreg")
(define_bypass 3 "cell-vecfloat" "cell-veccomplex")
; this is not correct, 
;;  this is a stall in general and not dependent on result
(define_bypass 13 "cell-vecstore" "cell-fpstore")
; this is not correct, this can never be true, not dependent on result
(define_bypass 7 "cell-fp" "cell-fpload")
;; vsu1 should avoid writing to the same target register as vsu2 insn
;;   within 12 cycles. 

;; WAW hazard

;; the target of VSU estimate should not be reused within 10 dispatch groups
;; the target of VSU float should not be reused within 8 dispatch groups
;; the target of VSU complex should not be reused within 5 dispatch groups
;; FP LOAD should not reuse an FPU Arithmetic target with 6 dispatch gropus

;; mtctr-bcctr/bcctrl, branch target ctr register shadow update at
;;  ex4 stage(10 cycles)
(define_bypass 10 "cell-mtjmpr" "cell-branchreg")

;;Things are not simulated:
;; update instruction, update address gpr are not simulated
;; vrefp, vrsqrtefp have latency(14), currently simulated as 12 cycle float
;;  insns

