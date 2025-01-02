;; Goldmont(GLM) Scheduling
;; Copyright (C) 2018-2025 Free Software Foundation, Inc.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.
;;
;; Goldmont has 3 out-of-order IEC, 2 out-of--order FEC and out-of-order MEC.


(define_automaton "glm")

;;  EU: Execution Unit
;;  Goldmont has 3 clusters - IEC, FPC, MEC

;;  IEC has three execution ports - IEC-0, IEC-1 and IEC-2.
;;  FPC has two execution ports - FPC-0 and FPC-1.
;;  MEC has two execution ports - MEC-0 (load) and MEC-1 (store0.
(define_cpu_unit "glm-iec-0,glm-iec-1,glm-iec-2" "glm")
(define_cpu_unit "glm-fec-0,glm-fec-1,glm-load,glm-store" "glm")

;; Some EUs have duplicated copied and can be accessed via either ports 0, 1 or 2.
(define_reservation "glm-iec-any" "(glm-iec-0 | glm-iec-1 | glm-iec-2)")
(define_reservation "glm-iec-any-load" "(glm-iec-0|glm-iec-1|glm-iec-2)+glm-load")
(define_reservation "glm-iec-any-store" "(glm-iec-0|glm-iec-1|glm-iec-2)+glm-store")
(define_reservation "glm-iec-any-both" "(glm-iec-0 | glm-iec-1 | glm-iec-2) + glm-load + glm-store")
(define_reservation "glm-fec-all" "(glm-fec-0 + glm-fec-1)")
(define_reservation "glm-all" "(glm-iec-0+glm-iec-1+glm-iec-2)+(glm-fec-0+glm-fec-1)+(glm-load+glm-store)")
(define_reservation "glm-int-0" "glm-iec-0")
(define_reservation "glm-int-0-load" "glm-iec-0 + glm-load")
(define_reservation "glm-int-0-both" "glm-iec-0 + glm-load + glm-store")
(define_reservation "glm-int-1" "glm-iec-1")
(define_reservation "glm-int-1-mem" "glm-iec-1 + glm-load")
(define_reservation "glm-int-2" "glm-iec-2")
(define_reservation "glm-int-2-mem" "glm-iec-2 + glm-load")
(define_reservation "glm-fp-0" "glm-fec-0")
(define_reservation "glm-fec-any" "(glm-fec-0 | glm-fec-1)")

;;; fmul insn can have 4 or 5 cycles latency for scalar and vector types.
(define_reservation "glm-fmul-4c" "glm-fec-0, nothing*3")
(define_reservation "glm-fmul-4c-mem" "glm-fec-0+glm-load, nothing*3")
(define_reservation "glm-fmul-5c" "glm-fec-0, nothing*4")

;;; fadd has 3 cycles latency.
(define_reservation "glm-fadd-3c" "glm-fec-1, nothing*2")
(define_reservation "glm-fadd-3c-mem" "glm-fec-1+glm-load, nothing*2")

;;; imul insn has 3 cycles latency for SI operands
(define_reservation "glm-imul-32" "glm-iec-1, nothing*2")
(define_reservation "glm-imul-mem-32"
		    "(glm-iec-1+glm-load), nothing*2")
;;; imul has 5 cycles latency for DI operands with 1/2 tput
(define_reservation "glm-imul-64"
		    "glm-iec-1, glm-iec-1, nothing*3")
(define_reservation "glm-imul-mem-64"
		    "glm-iec-1+glm-load, glm-iec-1, nothing*3")


(define_insn_reservation  "glm_other" 9
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "other")
	    (eq_attr "atom_unit" "!jeu")))
  "glm-all*9")

;; return has type "other" with atom_unit "jeu"
(define_insn_reservation  "glm_other_2" 1
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "other")
	    (eq_attr "atom_unit" "jeu")))
  "glm-all")

(define_insn_reservation  "glm_multi" 9
  (and (eq_attr "cpu" "glm")
       (eq_attr "type" "multi"))
  "glm-all*9")

;; Normal alu insns without carry
(define_insn_reservation  "glm_alu" 1
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "alu")
	    (and (eq_attr "memory" "none")
		 (eq_attr "use_carry" "0"))))
  "glm-iec-any")

;; Normal alu insns without carry, but use MEC.
(define_insn_reservation  "glm_alu_load" 1
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "alu")
	    (and (eq_attr "memory" "load")
		 (eq_attr "use_carry" "0"))))
  "glm-iec-any-load")

(define_insn_reservation  "glm_alu_mem" 1
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "alu")
	    (and (eq_attr "memory" "both")
		 (eq_attr "use_carry" "0"))))
  "glm-iec-any-both")


;; Alu insn consuming CF, such as add/sbb
(define_insn_reservation  "glm_alu_carry" 2
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "alu")
	    (and (eq_attr "memory" "none")
		 (eq_attr "use_carry" "1"))))
  "glm-int-2, nothing")

;; Alu insn consuming CF, such as add/sbb
(define_insn_reservation  "glm_alu_carry_mem" 2
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "alu")
	    (and (eq_attr "memory" "!none")
		(eq_attr "use_carry" "1"))))
  "glm-int-2-mem, nothing")

(define_insn_reservation  "glm_alu1" 1
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "alu1")
	    (eq_attr "memory" "none") (eq_attr "prefix_0f" "0")))
  "glm-int-1")

;; bsf and bsf insn
(define_insn_reservation  "glm_alu1_1" 10
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "alu1")
	    (eq_attr "memory" "none") (eq_attr "prefix_0f" "1")))
  "glm-int-1*8,nothing*2")

(define_insn_reservation  "glm_alu1_mem" 1
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "alu1")
	    (eq_attr "memory" "!none")))
  "glm-int-1-mem")

(define_insn_reservation  "glm_negnot" 1
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "negnot")
	    (eq_attr "memory" "none")))
  "glm-iec-any")

(define_insn_reservation  "glm_negnot_mem" 1
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "negnot")
	    (eq_attr "memory" "!none")))
  "glm-iec-any-both")

(define_insn_reservation  "glm_imov" 1
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "imov")
	    (eq_attr "memory" "none")))
  "glm-iec-any")

(define_insn_reservation  "glm_imov_load" 2
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "imov")
	    (eq_attr "memory" "load")))
  "glm-iec-any-load,nothing")

(define_insn_reservation  "glm_imov_store" 1
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "imov")
	    (eq_attr "memory" "store")))
  "glm-iec-any-store")

;; 16<-16, 32<-32
(define_insn_reservation  "glm_imovx" 1
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "imovx")
	    (and (eq_attr "memory" "none")
		 (ior (and (match_operand:HI 0 "register_operand")
			   (match_operand:HI 1 "general_operand"))
		      (and (match_operand:SI 0 "register_operand")
			   (match_operand:SI 1 "general_operand"))))))
  "glm-iec-any")

;; 16<-16, 32<-32, mem
(define_insn_reservation  "glm_imovx_mem" 1
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "imovx")
	    (and (eq_attr "memory" "!none")
		 (ior (and (match_operand:HI 0 "register_operand")
			   (match_operand:HI 1 "general_operand"))
		      (and (match_operand:SI 0 "register_operand")
			   (match_operand:SI 1 "general_operand"))))))
  "glm-iec-any-load")


;; 32<-16, 32<-8, 64<-16, 64<-8, 64<-32, 8<-8
(define_insn_reservation  "glm_imovx_2" 1
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "imovx")
	    (and (eq_attr "memory" "none")
		 (ior (match_operand:QI 0 "register_operand")
		      (ior (and (match_operand:SI 0 "register_operand")
				(not (match_operand:SI 1 "general_operand")))
			   (match_operand:DI 0 "register_operand"))))))
  "glm-iec-any")

;; 32<-16, 32<-8, 64<-16, 64<-8, 64<-32, 8<-8, mem
(define_insn_reservation  "glm_imovx_2_load" 2
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "imovx")
	    (and (eq_attr "memory" "load")
		 (ior (match_operand:QI 0 "register_operand")
		      (ior (and (match_operand:SI 0 "register_operand")
				(not (match_operand:SI 1 "general_operand")))
			   (match_operand:DI 0 "register_operand"))))))
  "glm-iec-any-load,nothing")

(define_insn_reservation  "glm_imovx_2_mem" 1
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "imovx")
	    (and (eq_attr "memory" "!none")
		 (ior (match_operand:QI 0 "register_operand")
		      (ior (and (match_operand:SI 0 "register_operand")
				(not (match_operand:SI 1 "general_operand")))
			   (match_operand:DI 0 "register_operand"))))))
  "glm-iec-any-both")


;; 16<-8
(define_insn_reservation  "glm_imovx_3" 3
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "imovx")
	    (and (match_operand:HI 0 "register_operand")
		 (match_operand:QI 1 "general_operand"))))
  "glm-int-0, nothing*2")

(define_insn_reservation  "glm_lea" 1
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "lea")
	    (eq_attr "mode" "!HI")))
  "glm-iec-any")

;; lea 16bit address is complex insn
(define_insn_reservation  "glm_lea_2" 2
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "lea")
	    (eq_attr "mode" "HI")))
  "glm-all*2")

(define_insn_reservation  "glm_incdec" 1
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "incdec")
	    (eq_attr "memory" "none")))
  "glm-int-0")

(define_insn_reservation  "glm_incdec_mem" 3
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "incdec")
	    (eq_attr "memory" "!none")))
  "glm-int-0-both, nothing*2")

;; simple shift instruction use SHIFT eu, none memory
(define_insn_reservation  "glm_ishift" 1
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "ishift")
	    (and (eq_attr "memory" "none") (eq_attr "prefix_0f" "0"))))
  "glm-int-0")

;; simple shift instruction use SHIFT eu, memory
(define_insn_reservation  "glm_ishift_mem" 2
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "ishift")
	    (and (eq_attr "memory" "!none") (eq_attr "prefix_0f" "0"))))
  "glm-int-0-both,nothing")

;; DF shift (prefixed with 0f) is complex insn with latency of 4 cycles
(define_insn_reservation  "glm_ishift_3" 4
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "ishift")
	    (eq_attr "prefix_0f" "1")))
  "glm-all*4")

(define_insn_reservation  "glm_ishift1" 1
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "ishift1")
	    (eq_attr "memory" "none")))
  "glm-int-0")

(define_insn_reservation  "glm_ishift1_mem" 2
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "ishift1")
	    (eq_attr "memory" "!none")))
  "glm-int-0-both,nothing")

(define_insn_reservation  "glm_rotate" 1
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "rotate")
	    (eq_attr "memory" "none")))
  "glm-int-0")

(define_insn_reservation  "glm_rotate_mem" 2
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "rotate")
	    (eq_attr "memory" "!none")))
  "glm-int-0-both,nothing")

(define_insn_reservation  "glm_imul" 3
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "imul")
	    (and (eq_attr "memory" "none") (eq_attr "mode" "SI"))))
  "glm-imul-32")

(define_insn_reservation  "glm_imul_load" 3
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "imul")
	    (and (eq_attr "memory" "!none") (eq_attr "mode" "SI"))))
  "glm-imul-mem-32")


;; latency set to 5 as common 64x64 imul with 1/2 tput
(define_insn_reservation  "glm_imul64" 5
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "imul")
	    (and (eq_attr "memory" "none") (eq_attr "mode" "!SI"))))
  "glm-imul-64")

(define_insn_reservation  "glm_imul64-load" 5
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "imul")
	    (and (eq_attr "memory" "!none") (eq_attr "mode" "!SI"))))
  "glm-imul-mem-64")

(define_insn_reservation  "glm_idiv" 25
  (and (eq_attr "cpu" "glm")
       (eq_attr "type" "idiv"))
  "glm-all*16, nothing*9")

(define_insn_reservation  "glm_icmp" 1
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "icmp")
	    (eq_attr "memory" "none")))
  "glm-int-0")

(define_insn_reservation  "glm_icmp_mem" 2
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "icmp")
	    (eq_attr "memory" "!none")))
  "glm-int-0-load,nothing")

(define_insn_reservation  "glm_test" 1
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "test")
	    (eq_attr "memory" "none")))
  "glm-int-0")

(define_insn_reservation  "glm_test_mem" 2
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "test")
	    (eq_attr "memory" "!none")))
  "glm-int-0-load,nothing")

(define_insn_reservation  "glm_ibr" 1
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "ibr")
	    (eq_attr "memory" "!load")))
  "glm-int-1")

;; complex if jump target is from address
(define_insn_reservation  "glm_ibr_2" 2
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "ibr")
	    (eq_attr "memory" "load")))
  "glm-all*2")

(define_insn_reservation  "glm_setcc" 1
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "setcc")
	    (eq_attr "memory" "!store")))
  "glm-iec-any")

;; 2 cycles complex if target is in memory
(define_insn_reservation  "glm_setcc_2" 2
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "setcc")
	    (eq_attr "memory" "store")))
  "glm-all*2")

(define_insn_reservation  "glm_icmov" 2
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "icmov")
	    (eq_attr "memory" "none")))
  "glm-iec-any, nothing")

(define_insn_reservation  "glm_icmov_mem" 2
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "icmov")
	    (eq_attr "memory" "!none")))
  "glm-int-0-load, nothing")

;; UCODE if segreg, ignored
(define_insn_reservation  "glm_push" 2
  (and (eq_attr "cpu" "glm")
       (eq_attr "type" "push"))
  "(glm-int-1+glm-int-2)*2")

;; pop r64 is 1 cycle. UCODE if segreg, ignored
(define_insn_reservation  "glm_pop" 1
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "pop")
	    (eq_attr "mode" "DI")))
  "glm-int-1+glm-int-2")

;; pop non-r64 is 2 cycles. UCODE if segreg, ignored
(define_insn_reservation  "glm_pop_2" 2
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "pop")
	    (eq_attr "mode" "!DI")))
  "(glm-int-1+glm-int-2)*2")

;; UCODE if segreg, ignored
(define_insn_reservation  "glm_call" 1
  (and (eq_attr "cpu" "glm")
       (eq_attr "type" "call,callv"))
  "(glm-int-0+glm-int-1)")

(define_insn_reservation  "glm_leave" 3
  (and (eq_attr "cpu" "glm")
       (eq_attr "type" "leave"))
  "glm-all*3")

(define_insn_reservation  "glm_str" 3
  (and (eq_attr "cpu" "glm")
       (eq_attr "type" "str"))
  "glm-all*3")

(define_insn_reservation  "glm_sselog" 1
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "sselog")
	    (eq_attr "memory" "none")))
  "glm-fec-all")

(define_insn_reservation  "glm_sselog_mem" 1
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "sselog")
	    (eq_attr "memory" "!none")))
  "glm-fec-all+glm-load")

(define_insn_reservation  "glm_sselog1" 1
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "sselog1")
	    (eq_attr "memory" "none")))
  "glm-fp-0")

(define_insn_reservation  "glm_sselog1_mem" 1
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "sselog1")
	    (eq_attr "memory" "!none")))
  "glm-fp-0+glm-load")

;; not pmad, not psad
(define_insn_reservation  "glm_sseiadd" 3
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "sseiadd")
	    (and (not (match_operand:V2DI 0 "register_operand"))
		 (and (eq_attr "atom_unit" "!simul")
		      (eq_attr "atom_unit" "!complex")))))
  "glm-fadd-3c")

;; pmad, psad and 64
(define_insn_reservation  "glm_sseiadd_2" 4
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "sseiadd")
	    (and (not (match_operand:V2DI 0 "register_operand"))
		 (and (eq_attr "atom_unit" "simul")
		      (eq_attr "mode" "DI,TI")))))
  "glm-fmul-4c")

;; if paddq(64 bit op), phadd/phsub
(define_insn_reservation  "glm_sseiadd_3" 5
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "sseiadd")
	    (ior (match_operand:V2DI 0 "register_operand")
		 (eq_attr "atom_unit" "complex"))))
  "glm-fmul-5c")

;; if immediate op.
(define_insn_reservation  "glm_sseishft" 2
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "sseishft")
	    (match_operand 2 "immediate_operand")))
  "glm-fp-0, nothing")

(define_insn_reservation  "glm_sseimul" 4
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "sseimul")
	    (eq_attr "memory" "none")))
  "glm-fmul-4c")

(define_insn_reservation  "glm_sseimul_load" 4
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "sseimul")
	    (eq_attr "memory" "!none")))
  "glm-fmul-4c-mem")


;; rcpss or rsqrtss
(define_insn_reservation  "glm_sse" 4
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "sse")
	    (and (eq_attr "atom_sse_attr" "rcp") (eq_attr "mode" "SF"))))
  "glm-fmul-4c")

;; movshdup, movsldup. Suggest to type sseishft
(define_insn_reservation  "glm_sse_2" 1
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "sse")
	    (eq_attr "atom_sse_attr" "movdup")))
  "glm-fec-any")

;; lfence
(define_insn_reservation  "glm_sse_3" 1
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "sse")
	    (eq_attr "atom_sse_attr" "lfence")))
  "glm-fec-any")

;; sfence,clflush,mfence, prefetch
(define_insn_reservation  "glm_sse_4" 1
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "sse")
	    (ior (eq_attr "atom_sse_attr" "fence")
		 (eq_attr "atom_sse_attr" "prefetch"))))
  "glm-fp-0")

;; rcpps, rsqrtss, sqrt, ldmxcsr
(define_insn_reservation  "glm_sse_5" 9
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "sse")
	    (ior (ior (eq_attr "atom_sse_attr" "sqrt")
		      (eq_attr "atom_sse_attr" "mxcsr"))
		 (and (eq_attr "atom_sse_attr" "rcp")
		      (eq_attr "mode" "V4SF")))))
  "glm-fec-all*6, nothing*3")

;; xmm->xmm
(define_insn_reservation  "glm_ssemov" 1
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "ssemov")
	    (and (match_operand 0 "register_operand" "xy")
		 (match_operand 1 "register_operand" "xy"))))
  "glm-fec-any")

;; reg->xmm
(define_insn_reservation  "glm_ssemov_2" 1
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "ssemov")
	    (and (match_operand 0 "register_operand" "xy")
		 (match_operand 1 "register_operand" "r"))))
  "glm-fp-0")

;; xmm->reg
(define_insn_reservation  "glm_ssemov_3" 3
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "ssemov")
	    (and (match_operand 0 "register_operand" "r")
		 (match_operand 1 "register_operand" "xy"))))
  "glm-fp-0, nothing*2")

;; mov mem
(define_insn_reservation  "glm_ssemov_load" 2
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "ssemov")
	    (eq_attr "memory" "load")))
  "glm-fec-any+glm-load,nothing")

(define_insn_reservation  "glm_ssemov_store" 1
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "ssemov")
	    (eq_attr "memory" "store")))
  "glm-fec-any+glm-store")

;; no memory simple
(define_insn_reservation  "glm_sseadd" 3
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "sseadd")
	    (eq_attr "memory" "none")))
  "glm-fadd-3c")

;; memory simple
(define_insn_reservation  "glm_sseadd_mem" 3
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "sseadd")
	    (eq_attr "memory" "!none")))
  "glm-fadd-3c-mem")

;; Except dppd/dpps
(define_insn_reservation  "glm_ssemul" 4
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "ssemul")
	    (eq_attr "memory" "none")))
  "glm-fmul-4c")

(define_insn_reservation  "glm_ssemul_mem" 4
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "ssemul")
	    (eq_attr "memory" "!none")))
  "glm-fmul-4c-mem")

(define_insn_reservation  "glm_ssecmp" 1
  (and (eq_attr "cpu" "glm")
       (eq_attr "type" "ssecmp"))
  "glm-fec-any")

(define_insn_reservation  "glm_ssecomi" 1
  (and (eq_attr "cpu" "glm")
       (eq_attr "type" "ssecomi"))
  "glm-fp-0")

;; no memory and cvtpi2ps, cvtps2pi, cvttps2pi
(define_insn_reservation  "glm_ssecvt" 4
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "ssecvt")
	    (ior (and (match_operand:V2SI 0 "register_operand")
		      (match_operand:V4SF 1 "register_operand"))
		 (and (match_operand:V4SF 0 "register_operand")
		      (match_operand:V2SI 1 "register_operand")))))
  "glm-fp-0, nothing*3")

;; memory and cvtpi2ps, cvtps2pi, cvttps2pi
(define_insn_reservation  "glm_ssecvt_mem" 4
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "ssecvt")
	    (ior (and (match_operand:V2SI 0 "register_operand")
		      (match_operand:V4SF 1 "memory_operand"))
		 (and (match_operand:V4SF 0 "register_operand")
		      (match_operand:V2SI 1 "memory_operand")))))
  "glm-fp-0+glm-load, nothing*3")

;; memory and cvtsi2sd
(define_insn_reservation  "glm_sseicvt" 1
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "sseicvt")
	    (and (match_operand:V2DF 0 "register_operand")
		 (match_operand:SI 1 "nonimmediate_operand"))))
  "glm-fp-0")

;; otherwise. 8 cycles average for cvtsd2si
(define_insn_reservation  "glm_sseicvt_2" 4
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "sseicvt")
	    (not (and (match_operand:V2DF 0 "register_operand")
		      (match_operand:SI 1 "memory_operand")))))
  "glm-fp-0, nothing*3")

(define_insn_reservation  "glm_ssediv" 13
  (and (eq_attr "cpu" "glm")
       (eq_attr "type" "ssediv"))
  "glm-fec-all*12, nothing")

;; simple for fmov
(define_insn_reservation  "glm_fmov" 1
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "fmov")
	    (eq_attr "memory" "none")))
  "glm-fec-any")

;; simple for fmov
(define_insn_reservation  "glm_fmov_load" 3
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "fmov")
	    (eq_attr "memory" "load")))
  "glm-fec-any+glm-load, nothing*2")

(define_insn_reservation  "glm_fmov_store" 1
  (and (eq_attr "cpu" "glm")
       (and (eq_attr "type" "fmov")
	    (eq_attr "memory" "store")))
  "glm-fec-any+glm-store")

;; Define bypass here

;; There will be 0 cycle stall from cmp/test to jcc

;; There will be 1 cycle stall from flag producer to cmov and adc/sbb
(define_bypass 2 "glm_icmp, glm_test, glm_alu, glm_alu_carry,
		  glm_alu1, glm_negnot, glm_incdec, glm_ishift,
		  glm_ishift1, glm_rotate"
		 "glm_icmov, glm_alu_carry")

;; lea to shift source stall is 1 cycle
(define_bypass 2 "glm_lea"
		 "glm_ishift, glm_ishift1, glm_rotate"
		 "!ix86_dep_by_shift_count")

;; non-lea to shift count stall is 1 cycle
(define_bypass 2 "glm_alu_carry,
		  glm_alu,glm_alu1,glm_negnot,glm_imov,glm_imovx,
		  glm_incdec,glm_ishift,glm_ishift1,glm_rotate,
		  glm_setcc, glm_icmov, glm_pop, glm_imov_store,
		  glm_alu_mem, glm_alu_carry_mem, glm_alu1_mem,
		  glm_alu_load, glm_imovx_mem, glm_imovx_2_mem,
		  glm_imov_load, glm_icmov_mem, glm_fmov_load, glm_fmov_store"
		 "glm_ishift, glm_ishift1, glm_rotate,
		  glm_ishift_mem, glm_ishift1_mem,
		  glm_rotate_mem"
		 "ix86_dep_by_shift_count")
