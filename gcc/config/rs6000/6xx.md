;; Scheduling description for PowerPC 604, PowerPC 604e, PowerPC 620,
;; and PowerPC 630 processors.
;;   Copyright (C) 2003-2024 Free Software Foundation, Inc.
;;
;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.

;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

(define_automaton "ppc6xx,ppc6xxfp,ppc6xxfp2")
(define_cpu_unit "iu1_6xx,iu2_6xx,mciu_6xx" "ppc6xx")
(define_cpu_unit "fpu_6xx" "ppc6xxfp")
(define_cpu_unit "fpu1_6xx,fpu2_6xx" "ppc6xxfp2")
(define_cpu_unit "lsu_6xx,bpu_6xx,cru_6xx" "ppc6xx")

;; PPC604  32-bit 2xSCIU, MCIU, LSU, FPU, BPU
;; PPC604e  32-bit 2xSCIU, MCIU, LSU, FPU, BPU, CRU
;; MCIU used for imul/idiv and moves from/to spr
;; LSU 2 stage pipelined
;; FPU 3 stage pipelined
;; Max issue 4 insns/clock cycle

;; PPC604e is PPC604 with larger caches and a CRU.  In the 604
;; the CR logical operations are handled in the BPU.
;; In the 604e, the CRU shares bus with BPU so only one condition
;; register or branch insn can be issued per clock.  Not modelled.

;; PPC620  64-bit 2xSCIU, MCIU, LSU, FPU, BPU, CRU
;; PPC630 64-bit 2xSCIU, MCIU, LSU, 2xFPU, BPU, CRU
;; Max issue 4 insns/clock cycle
;; Out-of-order execution, in-order completion

;; No following instruction can dispatch in the same cycle as a branch
;; instruction.  Not modelled.  This is no problem if RCSP is not
;; enabled since the scheduler stops a schedule when it gets to a branch.

;; Four insns can be dispatched per cycle.

(define_insn_reservation "ppc604-load" 2
  (and (eq_attr "type" "load")
       (eq_attr "cpu" "ppc604,ppc604e,ppc620,ppc630"))
  "lsu_6xx")

(define_insn_reservation "ppc604-fpload" 3
  (and (eq_attr "type" "fpload")
       (eq_attr "cpu" "ppc604,ppc604e,ppc620,ppc630"))
  "lsu_6xx")

(define_insn_reservation "ppc604-store" 3
  (and (eq_attr "type" "store,fpstore")
       (eq_attr "cpu" "ppc604,ppc604e,ppc620,ppc630"))
  "lsu_6xx")

(define_insn_reservation "ppc604-llsc" 3
  (and (eq_attr "type" "load_l,store_c")
       (eq_attr "cpu" "ppc604,ppc604e"))
  "lsu_6xx")
  
(define_insn_reservation "ppc630-llsc" 4
  (and (eq_attr "type" "load_l,store_c")
       (eq_attr "cpu" "ppc620,ppc630"))
  "lsu_6xx")
  
(define_insn_reservation "ppc604-integer" 1
  (and (ior (eq_attr "type" "integer,insert,trap,cntlz,isel")
	    (and (eq_attr "type" "add,logical,shift,exts")
		 (eq_attr "dot" "no")))
       (eq_attr "cpu" "ppc604,ppc604e,ppc620,ppc630"))
  "iu1_6xx|iu2_6xx")

(define_insn_reservation "ppc604-two" 1
  (and (eq_attr "type" "two")
       (eq_attr "cpu" "ppc604,ppc604e,ppc620,ppc630"))
  "iu1_6xx|iu2_6xx,iu1_6xx|iu2_6xx")

(define_insn_reservation "ppc604-three" 1
  (and (eq_attr "type" "three")
       (eq_attr "cpu" "ppc604,ppc604e,ppc620,ppc630"))
  "iu1_6xx|iu2_6xx,iu1_6xx|iu2_6xx,iu1_6xx|iu2_6xx")

(define_insn_reservation "ppc604-imul" 4
  (and (eq_attr "type" "mul")
       (eq_attr "cpu" "ppc604"))
  "mciu_6xx*2")

(define_insn_reservation "ppc604e-imul" 2
  (and (eq_attr "type" "mul")
       (eq_attr "cpu" "ppc604e"))
  "mciu_6xx")

(define_insn_reservation "ppc620-imul" 5
  (and (eq_attr "type" "mul")
       (eq_attr "size" "32")
       (eq_attr "cpu" "ppc620,ppc630"))
  "mciu_6xx*3")

(define_insn_reservation "ppc620-imul2" 4
  (and (eq_attr "type" "mul")
       (eq_attr "size" "16")
       (eq_attr "cpu" "ppc620,ppc630"))
  "mciu_6xx*3")

(define_insn_reservation "ppc620-imul3" 3
  (and (eq_attr "type" "mul")
       (eq_attr "size" "8")
       (eq_attr "cpu" "ppc620,ppc630"))
  "mciu_6xx*3")

(define_insn_reservation "ppc620-lmul" 7
  (and (eq_attr "type" "mul")
       (eq_attr "size" "64")
       (eq_attr "cpu" "ppc620,ppc630"))
  "mciu_6xx*5")

(define_insn_reservation "ppc604-idiv" 20
  (and (eq_attr "type" "div")
       (eq_attr "cpu" "ppc604,ppc604e"))
  "mciu_6xx*19")

(define_insn_reservation "ppc620-idiv" 37
  (and (eq_attr "type" "div")
       (eq_attr "size" "32")
       (eq_attr "cpu" "ppc620"))
  "mciu_6xx*36")

(define_insn_reservation "ppc630-idiv" 21
  (and (eq_attr "type" "div")
       (eq_attr "size" "32")
       (eq_attr "cpu" "ppc630"))
  "mciu_6xx*20")

(define_insn_reservation "ppc620-ldiv" 37
  (and (eq_attr "type" "div")
       (eq_attr "size" "64")
       (eq_attr "cpu" "ppc620,ppc630"))
  "mciu_6xx*36")

(define_insn_reservation "ppc604-compare" 3
  (and (ior (eq_attr "type" "cmp")
	    (and (eq_attr "type" "add,logical,shift,exts")
		 (eq_attr "dot" "yes")))
       (eq_attr "cpu" "ppc604,ppc604e,ppc620,ppc630"))
  "(iu1_6xx|iu2_6xx)")

; FPU PPC604{,e},PPC620
(define_insn_reservation "ppc604-fpcompare" 5
  (and (eq_attr "type" "fpcompare")
       (eq_attr "cpu" "ppc604,ppc604e,ppc620"))
  "fpu_6xx")

(define_insn_reservation "ppc604-fp" 3
  (and (eq_attr "type" "fp,fpsimple")
       (eq_attr "cpu" "ppc604,ppc604e,ppc620"))
  "fpu_6xx")

(define_insn_reservation "ppc604-dmul" 3
  (and (eq_attr "type" "dmul")
       (eq_attr "cpu" "ppc604,ppc604e,ppc620"))
  "fpu_6xx")

; Divides are not pipelined
(define_insn_reservation "ppc604-sdiv" 18
  (and (eq_attr "type" "sdiv")
       (eq_attr "cpu" "ppc604,ppc604e,ppc620"))
  "fpu_6xx*18")

(define_insn_reservation "ppc604-ddiv" 32
  (and (eq_attr "type" "ddiv")
       (eq_attr "cpu" "ppc604,ppc604e,ppc620"))
  "fpu_6xx*32")

(define_insn_reservation "ppc620-ssqrt" 31
  (and (eq_attr "type" "ssqrt")
       (eq_attr "cpu" "ppc620"))
  "fpu_6xx*31")

(define_insn_reservation "ppc620-dsqrt" 31
  (and (eq_attr "type" "dsqrt")
       (eq_attr "cpu" "ppc620"))
  "fpu_6xx*31")


; 2xFPU PPC630
(define_insn_reservation "ppc630-fpcompare" 5
  (and (eq_attr "type" "fpcompare")
       (eq_attr "cpu" "ppc630"))
  "fpu1_6xx|fpu2_6xx")

(define_insn_reservation "ppc630-fp" 3
  (and (eq_attr "type" "fp,dmul")
       (eq_attr "cpu" "ppc630"))
  "fpu1_6xx|fpu2_6xx")

(define_insn_reservation "ppc630-sdiv" 17
  (and (eq_attr "type" "sdiv")
       (eq_attr "cpu" "ppc630"))
  "fpu1_6xx*17|fpu2_6xx*17")

(define_insn_reservation "ppc630-ddiv" 21
  (and (eq_attr "type" "ddiv")
       (eq_attr "cpu" "ppc630"))
  "fpu1_6xx*21|fpu2_6xx*21")

(define_insn_reservation "ppc630-ssqrt" 18
  (and (eq_attr "type" "ssqrt")
       (eq_attr "cpu" "ppc630"))
  "fpu1_6xx*18|fpu2_6xx*18")

(define_insn_reservation "ppc630-dsqrt" 25
  (and (eq_attr "type" "dsqrt")
       (eq_attr "cpu" "ppc630"))
  "fpu1_6xx*25|fpu2_6xx*25")

(define_insn_reservation "ppc604-mfcr" 3
  (and (eq_attr "type" "mfcr")
       (eq_attr "cpu" "ppc604,ppc604e,ppc620,ppc630"))
  "mciu_6xx")

(define_insn_reservation "ppc604-mtcr" 2
  (and (eq_attr "type" "mtcr")
       (eq_attr "cpu" "ppc604,ppc604e,ppc620,ppc630"))
  "iu1_6xx|iu2_6xx")

(define_insn_reservation "ppc604-crlogical" 2
  (and (eq_attr "type" "cr_logical")
       (eq_attr "cpu" "ppc604"))
  "bpu_6xx")

(define_insn_reservation "ppc604e-crlogical" 2
  (and (eq_attr "type" "cr_logical")
       (eq_attr "cpu" "ppc604e,ppc620,ppc630"))
  "cru_6xx")

(define_insn_reservation "ppc604-mtjmpr" 2
  (and (eq_attr "type" "mtjmpr")
       (eq_attr "cpu" "ppc604,ppc604e,ppc620,ppc630"))
  "mciu_6xx")

(define_insn_reservation "ppc604-mfjmpr" 3
  (and (eq_attr "type" "mfjmpr")
       (eq_attr "cpu" "ppc604,ppc604e,ppc620"))
  "mciu_6xx")

(define_insn_reservation "ppc630-mfjmpr" 2
  (and (eq_attr "type" "mfjmpr")
       (eq_attr "cpu" "ppc630"))
  "mciu_6xx")

(define_insn_reservation "ppc604-jmpreg" 1
  (and (eq_attr "type" "jmpreg,branch")
       (eq_attr "cpu" "ppc604,ppc604e,ppc620,ppc630"))
  "bpu_6xx")

(define_insn_reservation "ppc604-isync" 0
  (and (eq_attr "type" "isync")
       (eq_attr "cpu" "ppc604,ppc604e"))
  "bpu_6xx")
  
(define_insn_reservation "ppc630-isync" 6
  (and (eq_attr "type" "isync")
       (eq_attr "cpu" "ppc620,ppc630"))
  "bpu_6xx")
  
(define_insn_reservation "ppc604-sync" 35
  (and (eq_attr "type" "sync")
       (eq_attr "cpu" "ppc604,ppc604e"))
  "lsu_6xx")
  
(define_insn_reservation "ppc630-sync" 26
  (and (eq_attr "type" "sync")
       (eq_attr "cpu" "ppc620,ppc630"))
  "lsu_6xx")
  
