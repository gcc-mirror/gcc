;; Slivermont(SLM) Scheduling
;; Copyright (C) 2009-2020 Free Software Foundation, Inc.
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
;; Silvermont has 2 out-of-order IEC, 2 in-order FEC and 1 in-order MEC.


(define_automaton "slm")

;;  EU: Execution Unit
;;  Silvermont EUs are connected by port 0 or port 1.

;;  SLM has two ports: port 0 and port 1 connecting to all execution units
(define_cpu_unit "slm-port-0,slm-port-1" "slm")

(define_cpu_unit "slm-ieu-0, slm-ieu-1,
                  slm-imul, slm-feu-0, slm-feu-1"
                  "slm")

(define_reservation "slm-all-ieu" "(slm-ieu-0 + slm-ieu-1 + slm-imul)")
(define_reservation "slm-all-feu" "(slm-feu-0 + slm-feu-1)")
(define_reservation "slm-all-eu" "(slm-all-ieu + slm-all-feu)")
(define_reservation "slm-fp-0" "(slm-port-0 + slm-feu-0)")

;; Some EUs have duplicated copied and can be accessed via either
;; port 0 or port 1
;; (define_reservation "slm-port-either" "(slm-port-0 | slm-port-1)"
(define_reservation "slm-port-dual" "(slm-port-0 + slm-port-1)")

;;; fmul insn can have 4 or 5 cycles latency
(define_reservation "slm-fmul-5c"
                    "(slm-port-0 + slm-feu-0), slm-feu-0, nothing*3")
(define_reservation "slm-fmul-4c" "(slm-port-0 + slm-feu-0), nothing*3")

;;; fadd can has 3 cycles latency depends on instruction forms
(define_reservation "slm-fadd-3c" "(slm-port-1 + slm-feu-1), nothing*2")
(define_reservation "slm-fadd-4c"
                    "(slm-port-1 + slm-feu-1), slm-feu-1, nothing*2")

;;; imul insn has 3 cycles latency for SI operands
(define_reservation "slm-imul-32"
                    "(slm-port-1 + slm-imul), nothing*2")
(define_reservation "slm-imul-mem-32"
                    "(slm-port-1 + slm-imul + slm-port-0), nothing*2")
;;; imul has 4 cycles latency for DI operands with 1/2 tput
(define_reservation "slm-imul-64"
                    "(slm-port-1 + slm-imul), slm-imul, nothing*2")

;;; dual-execution instructions can have 1,2,4,5 cycles latency depends on
;;; instruction forms
(define_reservation "slm-dual-1c" "(slm-port-dual + slm-all-eu)")
(define_reservation "slm-dual-2c"
                    "(slm-port-dual + slm-all-eu, nothing)")

;;; Most of simple ALU instructions have 1 cycle latency. Some of them
;;; issue in port 0, some in port 0 and some in either port.
(define_reservation "slm-simple-0" "(slm-port-0 + slm-ieu-0)")
(define_reservation "slm-simple-1" "(slm-port-1 + slm-ieu-1)")
(define_reservation "slm-simple-either" "(slm-simple-0 | slm-simple-1)")

;;; Complex macro-instruction has variants of latency, and uses both ports.
(define_reservation "slm-complex" "(slm-port-dual + slm-all-eu)")

(define_insn_reservation  "slm_other" 9
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "other")
            (eq_attr "atom_unit" "!jeu")))
  "slm-complex, slm-all-eu*8")

;; return has type "other" with atom_unit "jeu"
(define_insn_reservation  "slm_other_2" 1
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "other")
            (eq_attr "atom_unit" "jeu")))
  "slm-dual-1c")

(define_insn_reservation  "slm_multi" 9
  (and (eq_attr "cpu" "slm")
       (eq_attr "type" "multi"))
  "slm-complex, slm-all-eu*8")

;; Normal alu insns without carry
(define_insn_reservation  "slm_alu" 1
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "alu")
            (and (eq_attr "memory" "none")
                 (eq_attr "use_carry" "0"))))
  "slm-simple-either")

;; Normal alu insns without carry, but use MEC.
(define_insn_reservation  "slm_alu_mem" 1
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "alu")
            (and (eq_attr "memory" "!none")
                 (eq_attr "use_carry" "0"))))
  "slm-simple-either")

;; Alu insn consuming CF, such as add/sbb
(define_insn_reservation  "slm_alu_carry" 2
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "alu")
            (and (eq_attr "memory" "none")
                 (eq_attr "use_carry" "1"))))
  "slm-simple-either, nothing")

;; Alu insn consuming CF, such as add/sbb
(define_insn_reservation  "slm_alu_carry_mem" 2
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "alu")
            (and (eq_attr "memory" "!none")
                (eq_attr "use_carry" "1"))))
  "slm-simple-either, nothing")

(define_insn_reservation  "slm_alu1" 1
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "alu1")
            (eq_attr "memory" "none") (eq_attr "prefix_0f" "0")))
  "slm-simple-either")

;; bsf and bsf insn
(define_insn_reservation  "slm_alu1_1" 10
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "alu1")
            (eq_attr "memory" "none") (eq_attr "prefix_0f" "1")))
  "slm-simple-1, slm-ieu-1*9")

(define_insn_reservation  "slm_alu1_mem" 1
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "alu1")
            (eq_attr "memory" "!none")))
  "slm-simple-either")

(define_insn_reservation  "slm_negnot" 1
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "negnot")
            (eq_attr "memory" "none")))
  "slm-simple-either")

(define_insn_reservation  "slm_negnot_mem" 1
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "negnot")
            (eq_attr "memory" "!none")))
  "slm-simple-either")

(define_insn_reservation  "slm_imov" 1
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "imov")
            (eq_attr "memory" "none")))
  "slm-simple-either")

(define_insn_reservation  "slm_imov_mem" 1
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "imov")
            (eq_attr "memory" "!none")))
  "slm-simple-0")

;; 16<-16, 32<-32
(define_insn_reservation  "slm_imovx" 1
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "imovx")
            (and (eq_attr "memory" "none")
                 (ior (and (match_operand:HI 0 "register_operand")
                           (match_operand:HI 1 "general_operand"))
                      (and (match_operand:SI 0 "register_operand")
                           (match_operand:SI 1 "general_operand"))))))
  "slm-simple-either")

;; 16<-16, 32<-32, mem
(define_insn_reservation  "slm_imovx_mem" 1
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "imovx")
            (and (eq_attr "memory" "!none")
                 (ior (and (match_operand:HI 0 "register_operand")
                           (match_operand:HI 1 "general_operand"))
                      (and (match_operand:SI 0 "register_operand")
                           (match_operand:SI 1 "general_operand"))))))
  "slm-simple-either")

;; 32<-16, 32<-8, 64<-16, 64<-8, 64<-32, 8<-8
(define_insn_reservation  "slm_imovx_2" 1
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "imovx")
            (and (eq_attr "memory" "none")
                 (ior (match_operand:QI 0 "register_operand")
                      (ior (and (match_operand:SI 0 "register_operand")
                                (not (match_operand:SI 1 "general_operand")))
                           (match_operand:DI 0 "register_operand"))))))
  "slm-simple-either")

;; 32<-16, 32<-8, 64<-16, 64<-8, 64<-32, 8<-8, mem
(define_insn_reservation  "slm_imovx_2_mem" 1
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "imovx")
            (and (eq_attr "memory" "!none")
                 (ior (match_operand:QI 0 "register_operand")
                      (ior (and (match_operand:SI 0 "register_operand")
                                (not (match_operand:SI 1 "general_operand")))
                           (match_operand:DI 0 "register_operand"))))))
  "slm-simple-0")

;; 16<-8
(define_insn_reservation  "slm_imovx_3" 3
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "imovx")
            (and (match_operand:HI 0 "register_operand")
                 (match_operand:QI 1 "general_operand"))))
  "slm-simple-0, nothing*2")

(define_insn_reservation  "slm_lea" 1
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "lea")
            (eq_attr "mode" "!HI")))
  "slm-simple-either")

;; lea 16bit address is complex insn
(define_insn_reservation  "slm_lea_2" 2
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "lea")
            (eq_attr "mode" "HI")))
  "slm-complex, slm-all-eu")

(define_insn_reservation  "slm_incdec" 1
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "incdec")
            (eq_attr "memory" "none")))
  "slm-simple-0")

(define_insn_reservation  "slm_incdec_mem" 3
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "incdec")
            (eq_attr "memory" "!none")))
  "slm-simple-0, nothing*2")

;; simple shift instruction use SHIFT eu, none memory
(define_insn_reservation  "slm_ishift" 1
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "ishift")
            (and (eq_attr "memory" "none") (eq_attr "prefix_0f" "0"))))
  "slm-simple-0")

;; simple shift instruction use SHIFT eu, memory
(define_insn_reservation  "slm_ishift_mem" 1
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "ishift")
            (and (eq_attr "memory" "!none") (eq_attr "prefix_0f" "0"))))
  "slm-simple-0")

;; DF shift (prefixed with 0f) is complex insn with latency of 4 cycles
(define_insn_reservation  "slm_ishift_3" 4
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "ishift")
            (eq_attr "prefix_0f" "1")))
  "slm-complex, slm-all-eu*3")

(define_insn_reservation  "slm_ishift1" 1
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "ishift1")
            (eq_attr "memory" "none")))
  "slm-simple-0")

(define_insn_reservation  "slm_ishift1_mem" 1
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "ishift1")
            (eq_attr "memory" "!none")))
  "slm-simple-0")

(define_insn_reservation  "slm_rotate" 1
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "rotate")
            (eq_attr "memory" "none")))
  "slm-simple-0")

(define_insn_reservation  "slm_rotate_mem" 1
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "rotate")
            (eq_attr "memory" "!none")))
  "slm-simple-0")

(define_insn_reservation  "slm_rotate1" 1
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "rotate1")
            (eq_attr "memory" "none")))
  "slm-simple-0")

(define_insn_reservation  "slm_rotate1_mem" 1
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "rotate1")
            (eq_attr "memory" "!none")))
  "slm-simple-0")

(define_insn_reservation  "slm_imul" 3
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "imul")
            (and (eq_attr "memory" "none") (eq_attr "mode" "SI"))))
  "slm-imul-32")

(define_insn_reservation  "slm_imul_mem" 3
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "imul")
            (and (eq_attr "memory" "!none") (eq_attr "mode" "SI"))))
  "slm-imul-mem-32")

;; latency set to 4 as common 64x64 imul with 1/2 tput
(define_insn_reservation  "slm_imul_3" 4
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "imul")
            (eq_attr "mode" "!SI")))
  "slm-imul-64")

(define_insn_reservation  "slm_idiv" 33
  (and (eq_attr "cpu" "slm")
       (eq_attr "type" "idiv"))
  "slm-complex, slm-all-eu*16, nothing*16")

(define_insn_reservation  "slm_icmp" 1
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "icmp")
            (eq_attr "memory" "none")))
  "slm-simple-either")

(define_insn_reservation  "slm_icmp_mem" 1
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "icmp")
            (eq_attr "memory" "!none")))
  "slm-simple-either")

(define_insn_reservation  "slm_test" 1
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "test")
            (eq_attr "memory" "none")))
  "slm-simple-either")

(define_insn_reservation  "slm_test_mem" 1
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "test")
            (eq_attr "memory" "!none")))
  "slm-simple-either")

(define_insn_reservation  "slm_ibr" 1
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "ibr")
            (eq_attr "memory" "!load")))
  "slm-simple-1")

;; complex if jump target is from address
(define_insn_reservation  "slm_ibr_2" 2
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "ibr")
            (eq_attr "memory" "load")))
  "slm-complex, slm-all-eu")

(define_insn_reservation  "slm_setcc" 1
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "setcc")
            (eq_attr "memory" "!store")))
  "slm-simple-either")

;; 2 cycles complex if target is in memory
(define_insn_reservation  "slm_setcc_2" 2
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "setcc")
            (eq_attr "memory" "store")))
  "slm-complex, slm-all-eu")

(define_insn_reservation  "slm_icmov" 2
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "icmov")
            (eq_attr "memory" "none")))
  "slm-simple-either, nothing")

(define_insn_reservation  "slm_icmov_mem" 2
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "icmov")
            (eq_attr "memory" "!none")))
  "slm-simple-0, nothing")

;; UCODE if segreg, ignored
(define_insn_reservation  "slm_push" 2
  (and (eq_attr "cpu" "slm")
       (eq_attr "type" "push"))
  "slm-dual-2c")

;; pop r64 is 1 cycle. UCODE if segreg, ignored
(define_insn_reservation  "slm_pop" 1
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "pop")
            (eq_attr "mode" "DI")))
  "slm-dual-1c")

;; pop non-r64 is 2 cycles. UCODE if segreg, ignored
(define_insn_reservation  "slm_pop_2" 2
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "pop")
            (eq_attr "mode" "!DI")))
  "slm-dual-2c")

;; UCODE if segreg, ignored
(define_insn_reservation  "slm_call" 1
  (and (eq_attr "cpu" "slm")
       (eq_attr "type" "call"))
  "slm-dual-1c")

(define_insn_reservation  "slm_callv" 1
  (and (eq_attr "cpu" "slm")
       (eq_attr "type" "callv"))
  "slm-dual-1c")

(define_insn_reservation  "slm_leave" 3
  (and (eq_attr "cpu" "slm")
       (eq_attr "type" "leave"))
  "slm-complex, slm-all-eu*2")

(define_insn_reservation  "slm_str" 3
  (and (eq_attr "cpu" "slm")
       (eq_attr "type" "str"))
  "slm-complex, slm-all-eu*2")

(define_insn_reservation  "slm_sselog" 1
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "sselog")
            (eq_attr "memory" "none")))
  "slm-simple-either")

(define_insn_reservation  "slm_sselog_mem" 1
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "sselog")
            (eq_attr "memory" "!none")))
  "slm-simple-either")

(define_insn_reservation  "slm_sselog1" 1
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "sselog1")
            (eq_attr "memory" "none")))
  "slm-simple-0")

(define_insn_reservation  "slm_sselog1_mem" 1
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "sselog1")
            (eq_attr "memory" "!none")))
  "slm-simple-0")

;; not pmad, not psad
(define_insn_reservation  "slm_sseiadd" 1
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "sseiadd")
            (and (not (match_operand:V2DI 0 "register_operand"))
                 (and (eq_attr "atom_unit" "!simul")
                      (eq_attr "atom_unit" "!complex")))))
  "slm-simple-either")

;; pmad, psad and 64
(define_insn_reservation  "slm_sseiadd_2" 4
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "sseiadd")
            (and (not (match_operand:V2DI 0 "register_operand"))
                 (and (eq_attr "atom_unit" "simul" )
                      (eq_attr "mode" "DI")))))
  "slm-fmul-4c")

;; pmad, psad and 128
(define_insn_reservation  "slm_sseiadd_3" 5
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "sseiadd")
            (and (not (match_operand:V2DI 0 "register_operand"))
                 (and (eq_attr "atom_unit" "simul" )
                      (eq_attr "mode" "TI")))))
  "slm-fmul-5c")

;; if paddq(64 bit op), phadd/phsub
(define_insn_reservation  "slm_sseiadd_4" 4
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "sseiadd")
            (ior (match_operand:V2DI 0 "register_operand")
                 (eq_attr "atom_unit" "complex"))))
  "slm-fadd-4c")

;; if immediate op.
(define_insn_reservation  "slm_sseishft" 1
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "sseishft")
            (and (eq_attr "atom_unit" "!sishuf")
                 (match_operand 2 "immediate_operand"))))
  "slm-simple-either")

;; if palignr or psrldq
(define_insn_reservation  "slm_sseishft_2" 1
  (and (eq_attr "cpu" "slm")
       (ior (eq_attr "type" "sseishft1")
	    (and (eq_attr "type" "sseishft")
		 (and (eq_attr "atom_unit" "sishuf")
		      (match_operand 2 "immediate_operand")))))
  "slm-simple-0")

;; if reg/mem op
(define_insn_reservation  "slm_sseishft_3" 2
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "sseishft")
            (not (match_operand 2 "immediate_operand"))))
  "slm-complex, slm-all-eu")

(define_insn_reservation  "slm_sseimul" 5
  (and (eq_attr "cpu" "slm")
       (eq_attr "type" "sseimul"))
  "slm-fmul-5c")

;; rcpss or rsqrtss
(define_insn_reservation  "slm_sse" 4
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "sse")
            (and (eq_attr "atom_sse_attr" "rcp") (eq_attr "mode" "SF"))))
  "slm-fmul-4c")

;; movshdup, movsldup. Suggest to type sseishft
(define_insn_reservation  "slm_sse_2" 1
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "sse")
            (eq_attr "atom_sse_attr" "movdup")))
  "slm-simple-0")

;; lfence
(define_insn_reservation  "slm_sse_3" 1
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "sse")
            (eq_attr "atom_sse_attr" "lfence")))
  "slm-simple-either")

;; sfence,clflush,mfence, prefetch
(define_insn_reservation  "slm_sse_4" 1
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "sse")
            (ior (eq_attr "atom_sse_attr" "fence")
                 (eq_attr "atom_sse_attr" "prefetch"))))
  "slm-simple-0")

;; rcpps, rsqrtss, sqrt, ldmxcsr
(define_insn_reservation  "slm_sse_5" 9
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "sse")
            (ior (ior (eq_attr "atom_sse_attr" "sqrt")
                      (eq_attr "atom_sse_attr" "mxcsr"))
                 (and (eq_attr "atom_sse_attr" "rcp")
                      (eq_attr "mode" "V4SF")))))
  "slm-complex, slm-all-eu*7, nothing")

;; xmm->xmm
(define_insn_reservation  "slm_ssemov" 1
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "ssemov")
            (and (match_operand 0 "register_operand" "xy")
                 (match_operand 1 "register_operand" "xy"))))
  "slm-simple-either")

;; reg->xmm
(define_insn_reservation  "slm_ssemov_2" 1
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "ssemov")
            (and (match_operand 0 "register_operand" "xy")
                 (match_operand 1 "register_operand" "r"))))
  "slm-simple-0")

;; xmm->reg
(define_insn_reservation  "slm_ssemov_3" 3
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "ssemov")
            (and (match_operand 0 "register_operand" "r")
                 (match_operand 1 "register_operand" "xy"))))
  "slm-simple-0, nothing*2")

;; mov mem
(define_insn_reservation  "slm_ssemov_4" 1
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "ssemov")
            (and (eq_attr "movu" "0") (eq_attr "memory" "!none"))))
  "slm-simple-0")

;; movu mem
(define_insn_reservation  "slm_ssemov_5" 2
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "ssemov")
            (ior (eq_attr "movu" "1") (eq_attr "memory" "!none"))))
  "slm-simple-0, nothing")

;; no memory simple
(define_insn_reservation  "slm_sseadd" 3
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "sseadd")
            (and (eq_attr "memory" "none")
                 (and (eq_attr "mode" "!V2DF")
                      (eq_attr "atom_unit" "!complex")))))
  "slm-fadd-3c")

;; memory simple
(define_insn_reservation  "slm_sseadd_mem" 3
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "sseadd")
            (and (eq_attr "memory" "!none")
                 (and (eq_attr "mode" "!V2DF")
                      (eq_attr "atom_unit" "!complex")))))
  "slm-fadd-3c")

;; maxps, minps, *pd, hadd, hsub
(define_insn_reservation  "slm_sseadd_3" 4
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "sseadd")
            (ior (eq_attr "mode" "V2DF") (eq_attr "atom_unit" "complex"))))
  "slm-fadd-4c")

;; Except dppd/dpps
(define_insn_reservation  "slm_ssemul" 5
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "ssemul")
            (eq_attr "mode" "!SF")))
  "slm-fmul-5c")

;; Except dppd/dpps, 4 cycle if mulss
(define_insn_reservation  "slm_ssemul_2" 4
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "ssemul")
            (eq_attr "mode" "SF")))
  "slm-fmul-4c")

(define_insn_reservation  "slm_ssecmp" 1
  (and (eq_attr "cpu" "slm")
       (eq_attr "type" "ssecmp"))
  "slm-simple-either")

(define_insn_reservation  "slm_ssecomi" 1
  (and (eq_attr "cpu" "slm")
       (eq_attr "type" "ssecomi"))
  "slm-simple-0")

;; no memory and cvtpi2ps, cvtps2pi, cvttps2pi
(define_insn_reservation  "slm_ssecvt" 5
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "ssecvt")
            (ior (and (match_operand:V2SI 0 "register_operand")
                      (match_operand:V4SF 1 "register_operand"))
                 (and (match_operand:V4SF 0 "register_operand")
                      (match_operand:V2SI 1 "register_operand")))))
  "slm-fp-0, slm-feu-0, nothing*3")

;; memory and cvtpi2ps, cvtps2pi, cvttps2pi
(define_insn_reservation  "slm_ssecvt_mem" 5
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "ssecvt")
            (ior (and (match_operand:V2SI 0 "register_operand")
                      (match_operand:V4SF 1 "memory_operand"))
                 (and (match_operand:V4SF 0 "register_operand")
                      (match_operand:V2SI 1 "memory_operand")))))
"slm-fp-0, slm-feu-0, nothing*3")

;; cvtpd2pi, cvtpi2pd
(define_insn_reservation  "slm_ssecvt_1" 2
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "ssecvt")
            (ior (and (match_operand:V2DF 0 "register_operand")
                      (match_operand:V2SI 1 "register_operand"))
                 (and (match_operand:V2SI 0 "register_operand")
                      (match_operand:V2DF 1 "register_operand")))))
  "slm-fp-0, slm-feu-0")

;; memory and cvtpd2pi, cvtpi2pd
(define_insn_reservation  "slm_ssecvt_1_mem" 2
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "ssecvt")
            (ior (and (match_operand:V2DF 0 "register_operand")
                      (match_operand:V2SI 1 "memory_operand"))
                 (and (match_operand:V2SI 0 "register_operand")
                      (match_operand:V2DF 1 "memory_operand")))))
  "slm-fp-0, slm-feu-0")

;; otherwise. 4 cycles average for cvtss2sd
(define_insn_reservation  "slm_ssecvt_3" 4
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "ssecvt")
            (not (ior (and (match_operand:V2SI 0 "register_operand")
                           (match_operand:V4SF 1 "nonimmediate_operand"))
                      (and (match_operand:V4SF 0 "register_operand")
                           (match_operand:V2SI 1 "nonimmediate_operand"))))))
  "slm-fp-0, nothing*3")

;; memory and cvtsi2sd
(define_insn_reservation  "slm_sseicvt" 1
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "sseicvt")
            (and (match_operand:V2DF 0 "register_operand")
                 (match_operand:SI 1 "nonimmediate_operand"))))
  "slm-fp-0")

;; otherwise. 8 cycles average for cvtsd2si
(define_insn_reservation  "slm_sseicvt_2" 4
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "sseicvt")
            (not (and (match_operand:V2DF 0 "register_operand")
                      (match_operand:SI 1 "memory_operand")))))
  "slm-fp-0, nothing*3")

(define_insn_reservation  "slm_ssediv" 13
  (and (eq_attr "cpu" "slm")
       (eq_attr "type" "ssediv"))
  "slm-fp-0, slm-feu-0*10, nothing*2")

;; simple for fmov
(define_insn_reservation  "slm_fmov" 1
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "fmov")
            (eq_attr "memory" "none")))
  "slm-simple-either")

;; simple for fmov
(define_insn_reservation  "slm_fmov_mem" 1
  (and (eq_attr "cpu" "slm")
       (and (eq_attr "type" "fmov")
            (eq_attr "memory" "!none")))
  "slm-simple-either")

;; Define bypass here

;; There will be 0 cycle stall from cmp/test to jcc

;; There will be 1 cycle stall from flag producer to cmov and adc/sbb
(define_bypass 2 "slm_icmp, slm_test, slm_alu, slm_alu_carry,
                  slm_alu1, slm_negnot, slm_incdec, slm_ishift,
                  slm_ishift1, slm_rotate, slm_rotate1"
                 "slm_icmov, slm_alu_carry")

;; lea to shift source stall is 1 cycle
(define_bypass 2 "slm_lea"
                 "slm_ishift, slm_ishift1, slm_rotate, slm_rotate1"
                 "!ix86_dep_by_shift_count")

;; non-lea to shift count stall is 1 cycle
(define_bypass 2 "slm_alu_carry,
                  slm_alu,slm_alu1,slm_negnot,slm_imov,slm_imovx,
                  slm_incdec,slm_ishift,slm_ishift1,slm_rotate,
                  slm_rotate1, slm_setcc, slm_icmov, slm_pop,
                  slm_alu_mem, slm_alu_carry_mem, slm_alu1_mem,
                  slm_imovx_mem, slm_imovx_2_mem,
                  slm_imov_mem, slm_icmov_mem, slm_fmov_mem"
                 "slm_ishift, slm_ishift1, slm_rotate, slm_rotate1,
                  slm_ishift_mem, slm_ishift1_mem,
                  slm_rotate_mem, slm_rotate1_mem"
                 "ix86_dep_by_shift_count")
