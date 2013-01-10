;; Atom Scheduling
;; Copyright (C) 2009-2013 Free Software Foundation, Inc.
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
;; Atom is an in-order core with two integer pipelines.


(define_attr "atom_unit" "sishuf,simul,jeu,complex,other" 
  (const_string "other"))

(define_attr "atom_sse_attr" "rcp,movdup,lfence,fence,prefetch,sqrt,mxcsr,other"
  (const_string "other"))

(define_automaton "atom")

;;  Atom has two ports: port 0 and port 1 connecting to all execution units
(define_cpu_unit "atom-port-0,atom-port-1" "atom")

;;  EU: Execution Unit
;;  Atom EUs are connected by port 0 or port 1. 

(define_cpu_unit "atom-eu-0, atom-eu-1,
                  atom-imul-1, atom-imul-2, atom-imul-3, atom-imul-4"
                  "atom")

;; Some EUs have duplicated copied and can be accessed via either
;; port 0 or port 1
;; (define_reservation "atom-port-either" "(atom-port-0 | atom-port-1)")

;;; Some instructions is dual-pipe execution, need both ports
;;; Complex multi-op macro-instructoins need both ports and all EUs
(define_reservation "atom-port-dual" "(atom-port-0 + atom-port-1)")
(define_reservation "atom-all-eu" "(atom-eu-0 + atom-eu-1 + 
                                    atom-imul-1 + atom-imul-2 + atom-imul-3 +
                                    atom-imul-4)")

;;; Most of simple instructions have 1 cycle latency. Some of them
;;; issue in port 0, some in port 0 and some in either port.
(define_reservation "atom-simple-0" "(atom-port-0 + atom-eu-0)")
(define_reservation "atom-simple-1" "(atom-port-1 + atom-eu-1)")
(define_reservation "atom-simple-either" "(atom-simple-0 | atom-simple-1)")

;;; Some insn issues in port 0 with 3 cycle latency and 1 cycle tput
(define_reservation "atom-eu-0-3-1" "(atom-port-0 + atom-eu-0, nothing*2)")

;;; fmul insn can have 4 or 5 cycles latency
(define_reservation "atom-fmul-5c" "(atom-port-0 + atom-eu-0), nothing*4")
(define_reservation "atom-fmul-4c" "(atom-port-0 + atom-eu-0), nothing*3")

;;; fadd can has 5 cycles latency depends on instruction forms
(define_reservation "atom-fadd-5c" "(atom-port-1 + atom-eu-1), nothing*5")

;;; imul insn has 5 cycles latency
(define_reservation "atom-imul-32" 
                    "atom-imul-1, atom-imul-2, atom-imul-3, atom-imul-4, 
                     atom-port-0")
;;; imul instruction excludes other non-FP instructions.
(exclusion_set "atom-eu-0, atom-eu-1" 
               "atom-imul-1, atom-imul-2, atom-imul-3, atom-imul-4")

;;; dual-execution instructions can have 1,2,4,5 cycles latency depends on 
;;; instruction forms
(define_reservation "atom-dual-1c" "(atom-port-dual + atom-eu-0 + atom-eu-1)")
(define_reservation "atom-dual-2c"
                    "(atom-port-dual + atom-eu-0 + atom-eu-1, nothing)")
(define_reservation "atom-dual-5c"
                    "(atom-port-dual + atom-eu-0 + atom-eu-1, nothing*4)")

;;; Complex macro-instruction has variants of latency, and uses both ports.
(define_reservation "atom-complex" "(atom-port-dual + atom-all-eu)")

(define_insn_reservation  "atom_other" 9
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "other")
            (eq_attr "atom_unit" "!jeu")))
  "atom-complex, atom-all-eu*8")

;; return has type "other" with atom_unit "jeu"
(define_insn_reservation  "atom_other_2" 1
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "other")
            (eq_attr "atom_unit" "jeu")))
  "atom-dual-1c")

(define_insn_reservation  "atom_multi" 9
  (and (eq_attr "cpu" "atom")
       (eq_attr "type" "multi"))
  "atom-complex, atom-all-eu*8")

;; Normal alu insns without carry
(define_insn_reservation  "atom_alu" 1
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "alu")
            (and (eq_attr "memory" "none")
                 (eq_attr "use_carry" "0"))))
  "atom-simple-either")

;; Normal alu insns without carry
(define_insn_reservation  "atom_alu_mem" 1
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "alu")
            (and (eq_attr "memory" "!none")
                 (eq_attr "use_carry" "0"))))
  "atom-simple-either")

;; Alu insn consuming CF, such as add/sbb
(define_insn_reservation  "atom_alu_carry" 1
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "alu")
            (and (eq_attr "memory" "none")
                 (eq_attr "use_carry" "1"))))
  "atom-simple-either")

;; Alu insn consuming CF, such as add/sbb
(define_insn_reservation  "atom_alu_carry_mem" 1
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "alu")
            (and (eq_attr "memory" "!none")
                (eq_attr "use_carry" "1"))))
  "atom-simple-either")

(define_insn_reservation  "atom_alu1" 1
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "alu1")
            (eq_attr "memory" "none")))
  "atom-simple-either")

(define_insn_reservation  "atom_alu1_mem" 1
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "alu1")
            (eq_attr "memory" "!none")))
  "atom-simple-either")

(define_insn_reservation  "atom_negnot" 1
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "negnot")
            (eq_attr "memory" "none")))
  "atom-simple-either")

(define_insn_reservation  "atom_negnot_mem" 1
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "negnot")
            (eq_attr "memory" "!none")))
  "atom-simple-either")

(define_insn_reservation  "atom_imov" 1
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "imov")
            (eq_attr "memory" "none")))
  "atom-simple-either")

(define_insn_reservation  "atom_imov_mem" 1
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "imov")
            (eq_attr "memory" "!none")))
  "atom-simple-either")

;; 16<-16, 32<-32
(define_insn_reservation  "atom_imovx" 1
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "imovx")
            (and (eq_attr "memory" "none")
                 (ior (and (match_operand:HI 0 "register_operand")
                           (match_operand:HI 1 "general_operand"))
                      (and (match_operand:SI 0 "register_operand")
                           (match_operand:SI 1 "general_operand"))))))
  "atom-simple-either")

;; 16<-16, 32<-32, mem
(define_insn_reservation  "atom_imovx_mem" 1
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "imovx")
            (and (eq_attr "memory" "!none")
                 (ior (and (match_operand:HI 0 "register_operand")
                           (match_operand:HI 1 "general_operand"))
                      (and (match_operand:SI 0 "register_operand")
                           (match_operand:SI 1 "general_operand"))))))
  "atom-simple-either")

;; 32<-16, 32<-8, 64<-16, 64<-8, 64<-32, 8<-8
(define_insn_reservation  "atom_imovx_2" 1
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "imovx")
            (and (eq_attr "memory" "none")
                 (ior (match_operand:QI 0 "register_operand")
                      (ior (and (match_operand:SI 0 "register_operand")
                                (not (match_operand:SI 1 "general_operand")))
                           (match_operand:DI 0 "register_operand"))))))
  "atom-simple-0")

;; 32<-16, 32<-8, 64<-16, 64<-8, 64<-32, 8<-8, mem
(define_insn_reservation  "atom_imovx_2_mem" 1
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "imovx")
            (and (eq_attr "memory" "!none")
                 (ior (match_operand:QI 0 "register_operand")
                      (ior (and (match_operand:SI 0 "register_operand")
                                (not (match_operand:SI 1 "general_operand")))
                           (match_operand:DI 0 "register_operand"))))))
  "atom-simple-0")

;; 16<-8
(define_insn_reservation  "atom_imovx_3" 3
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "imovx")
            (and (match_operand:HI 0 "register_operand")
                 (match_operand:QI 1 "general_operand"))))
  "atom-complex, atom-all-eu*2")

(define_insn_reservation  "atom_lea" 1
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "lea")
            (eq_attr "mode" "!HI")))
  "atom-simple-either")

;; lea 16bit address is complex insn
(define_insn_reservation  "atom_lea_2" 2
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "lea")
            (eq_attr "mode" "HI")))
  "atom-complex, atom-all-eu")

(define_insn_reservation  "atom_incdec" 1
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "incdec")
            (eq_attr "memory" "none")))
  "atom-simple-either")

(define_insn_reservation  "atom_incdec_mem" 1
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "incdec")
            (eq_attr "memory" "!none")))
  "atom-simple-either")

;; simple shift instruction use SHIFT eu, none memory
(define_insn_reservation  "atom_ishift" 1
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "ishift")
            (and (eq_attr "memory" "none") (eq_attr "prefix_0f" "0"))))
  "atom-simple-0")

;; simple shift instruction use SHIFT eu, memory
(define_insn_reservation  "atom_ishift_mem" 1
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "ishift")
            (and (eq_attr "memory" "!none") (eq_attr "prefix_0f" "0"))))
  "atom-simple-0")

;; DF shift (prefixed with 0f) is complex insn with latency of 7 cycles
(define_insn_reservation  "atom_ishift_3" 7
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "ishift")
            (eq_attr "prefix_0f" "1")))
  "atom-complex, atom-all-eu*6")

(define_insn_reservation  "atom_ishift1" 1
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "ishift1")
            (eq_attr "memory" "none")))
  "atom-simple-0")

(define_insn_reservation  "atom_ishift1_mem" 1
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "ishift1")
            (eq_attr "memory" "!none")))
  "atom-simple-0")

(define_insn_reservation  "atom_rotate" 1
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "rotate")
            (eq_attr "memory" "none")))
  "atom-simple-0")

(define_insn_reservation  "atom_rotate_mem" 1
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "rotate")
            (eq_attr "memory" "!none")))
  "atom-simple-0")

(define_insn_reservation  "atom_rotate1" 1
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "rotate1")
            (eq_attr "memory" "none")))
  "atom-simple-0")

(define_insn_reservation  "atom_rotate1_mem" 1
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "rotate1")
            (eq_attr "memory" "!none")))
  "atom-simple-0")

(define_insn_reservation  "atom_imul" 5
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "imul")
            (and (eq_attr "memory" "none") (eq_attr "mode" "SI"))))
  "atom-imul-32")

(define_insn_reservation  "atom_imul_mem" 5
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "imul")
            (and (eq_attr "memory" "!none") (eq_attr "mode" "SI"))))
  "atom-imul-32")

;; latency set to 10 as common 64x64 imul
(define_insn_reservation  "atom_imul_3" 10
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "imul")
            (eq_attr "mode" "!SI")))
  "atom-complex, atom-all-eu*9")

(define_insn_reservation  "atom_idiv" 65
  (and (eq_attr "cpu" "atom")
       (eq_attr "type" "idiv"))
  "atom-complex, atom-all-eu*32, nothing*32")

(define_insn_reservation  "atom_icmp" 1
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "icmp")
            (eq_attr "memory" "none")))
  "atom-simple-either")

(define_insn_reservation  "atom_icmp_mem" 1
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "icmp")
            (eq_attr "memory" "!none")))
  "atom-simple-either")

(define_insn_reservation  "atom_test" 1
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "test")
            (eq_attr "memory" "none")))
  "atom-simple-either")

(define_insn_reservation  "atom_test_mem" 1
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "test")
            (eq_attr "memory" "!none")))
  "atom-simple-either")

(define_insn_reservation  "atom_ibr" 1
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "ibr")
            (eq_attr "memory" "!load")))
  "atom-simple-1")

;; complex if jump target is from address
(define_insn_reservation  "atom_ibr_2" 2
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "ibr")
            (eq_attr "memory" "load")))
  "atom-complex, atom-all-eu")

(define_insn_reservation  "atom_setcc" 1
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "setcc")
            (eq_attr "memory" "!store")))
  "atom-simple-either")

;; 2 cycles complex if target is in memory
(define_insn_reservation  "atom_setcc_2" 2
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "setcc")
            (eq_attr "memory" "store")))
  "atom-complex, atom-all-eu")

(define_insn_reservation  "atom_icmov" 1
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "icmov")
            (eq_attr "memory" "none")))
  "atom-simple-either")

(define_insn_reservation  "atom_icmov_mem" 1
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "icmov")
            (eq_attr "memory" "!none")))
  "atom-simple-either")

;; UCODE if segreg, ignored
(define_insn_reservation  "atom_push" 2
  (and (eq_attr "cpu" "atom")
       (eq_attr "type" "push"))
  "atom-dual-2c")

;; pop r64 is 1 cycle. UCODE if segreg, ignored
(define_insn_reservation  "atom_pop" 1
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "pop")
            (eq_attr "mode" "DI")))
  "atom-dual-1c")

;; pop non-r64 is 2 cycles. UCODE if segreg, ignored
(define_insn_reservation  "atom_pop_2" 2
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "pop")
            (eq_attr "mode" "!DI")))
  "atom-dual-2c")

;; UCODE if segreg, ignored
(define_insn_reservation  "atom_call" 1
  (and (eq_attr "cpu" "atom")
       (eq_attr "type" "call"))
  "atom-dual-1c")

(define_insn_reservation  "atom_callv" 1
  (and (eq_attr "cpu" "atom")
       (eq_attr "type" "callv"))
  "atom-dual-1c")

(define_insn_reservation  "atom_leave" 3
  (and (eq_attr "cpu" "atom")
       (eq_attr "type" "leave"))
  "atom-complex, atom-all-eu*2")

(define_insn_reservation  "atom_str" 3
  (and (eq_attr "cpu" "atom")
       (eq_attr "type" "str"))
  "atom-complex, atom-all-eu*2")

(define_insn_reservation  "atom_sselog" 1
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "sselog,sseshuf")
            (eq_attr "memory" "none")))
  "atom-simple-either")

(define_insn_reservation  "atom_sselog_mem" 1
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "sselog,sseshuf")
            (eq_attr "memory" "!none")))
  "atom-simple-either")

(define_insn_reservation  "atom_sselog1" 1
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "sselog1,sseshuf1")
            (eq_attr "memory" "none")))
  "atom-simple-0")

(define_insn_reservation  "atom_sselog1_mem" 1
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "sselog1,sseshuf1")
            (eq_attr "memory" "!none")))
  "atom-simple-0")

;; not pmad, not psad
(define_insn_reservation  "atom_sseiadd" 1
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "sseiadd")
            (and (not (match_operand:V2DI 0 "register_operand"))
                 (and (eq_attr "atom_unit" "!simul")
                      (eq_attr "atom_unit" "!complex")))))
  "atom-simple-either")

;; pmad, psad and 64
(define_insn_reservation  "atom_sseiadd_2" 4
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "sseiadd")
            (and (not (match_operand:V2DI 0 "register_operand"))
                 (and (eq_attr "atom_unit" "simul" )
                      (eq_attr "mode" "DI")))))
  "atom-fmul-4c")

;; pmad, psad and 128
(define_insn_reservation  "atom_sseiadd_3" 5
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "sseiadd")
            (and (not (match_operand:V2DI 0 "register_operand"))
                 (and (eq_attr "atom_unit" "simul" )
                      (eq_attr "mode" "TI")))))
  "atom-fmul-5c")

;; if paddq(64 bit op), phadd/phsub
(define_insn_reservation  "atom_sseiadd_4" 6
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "sseiadd")
            (ior (match_operand:V2DI 0 "register_operand")
                 (eq_attr "atom_unit" "complex"))))
  "atom-complex, atom-all-eu*5")

;; if immediate op. 
(define_insn_reservation  "atom_sseishft" 1
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "sseishft")
            (and (eq_attr "atom_unit" "!sishuf")
                 (match_operand 2 "immediate_operand"))))
  "atom-simple-either")

;; if palignr or psrldq
(define_insn_reservation  "atom_sseishft_2" 1
  (and (eq_attr "cpu" "atom")
       (ior (eq_attr "type" "sseishft1")
	    (and (eq_attr "type" "sseishft")
		 (and (eq_attr "atom_unit" "sishuf")
		      (match_operand 2 "immediate_operand")))))
  "atom-simple-0")

;; if reg/mem op
(define_insn_reservation  "atom_sseishft_3" 2
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "sseishft")
            (not (match_operand 2 "immediate_operand"))))
  "atom-complex, atom-all-eu")

(define_insn_reservation  "atom_sseimul" 1
  (and (eq_attr "cpu" "atom")
       (eq_attr "type" "sseimul"))
  "atom-simple-0")

;; rcpss or rsqrtss
(define_insn_reservation  "atom_sse" 4
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "sse")
            (and (eq_attr "atom_sse_attr" "rcp") (eq_attr "mode" "SF"))))
  "atom-fmul-4c")

;; movshdup, movsldup. Suggest to type sseishft
(define_insn_reservation  "atom_sse_2" 1
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "sse")
            (eq_attr "atom_sse_attr" "movdup")))
  "atom-simple-0")

;; lfence
(define_insn_reservation  "atom_sse_3" 1
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "sse")
            (eq_attr "atom_sse_attr" "lfence")))
  "atom-simple-either")

;; sfence,clflush,mfence, prefetch
(define_insn_reservation  "atom_sse_4" 1
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "sse")
            (eq_attr "atom_sse_attr" "fence,prefetch")))
  "atom-simple-0")

;; rcpps, rsqrtss, sqrt, ldmxcsr
(define_insn_reservation  "atom_sse_5" 7
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "sse")
            (ior (eq_attr "atom_sse_attr" "sqrt,mxcsr")
                 (and (eq_attr "atom_sse_attr" "rcp")
                      (eq_attr "mode" "V4SF")))))
  "atom-complex, atom-all-eu*6")

;; xmm->xmm
(define_insn_reservation  "atom_ssemov" 1
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "ssemov")
            (and (match_operand 0 "register_operand" "xy") (match_operand 1 "register_operand" "xy"))))
  "atom-simple-either")

;; reg->xmm
(define_insn_reservation  "atom_ssemov_2" 1
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "ssemov")
            (and (match_operand 0 "register_operand" "xy") (match_operand 1 "register_operand" "r"))))
  "atom-simple-0")

;; xmm->reg
(define_insn_reservation  "atom_ssemov_3" 3
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "ssemov")
            (and (match_operand 0 "register_operand" "r") (match_operand 1 "register_operand" "xy"))))
  "atom-eu-0-3-1")

;; mov mem
(define_insn_reservation  "atom_ssemov_4" 1
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "ssemov")
            (and (eq_attr "movu" "0") (eq_attr "memory" "!none"))))
  "atom-simple-0")

;; movu mem
(define_insn_reservation  "atom_ssemov_5" 2
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "ssemov")
            (ior (eq_attr "movu" "1") (eq_attr "memory" "!none"))))
  "atom-complex, atom-all-eu")

;; no memory simple
(define_insn_reservation  "atom_sseadd" 5
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "sseadd,sseadd1")
            (and (eq_attr "memory" "none")
                 (and (eq_attr "mode" "!V2DF")
                      (eq_attr "atom_unit" "!complex")))))
  "atom-fadd-5c")

;; memory simple
(define_insn_reservation  "atom_sseadd_mem" 5
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "sseadd,sseadd1")
            (and (eq_attr "memory" "!none")
                 (and (eq_attr "mode" "!V2DF")
                      (eq_attr "atom_unit" "!complex")))))
  "atom-dual-5c")

;; maxps, minps, *pd, hadd, hsub
(define_insn_reservation  "atom_sseadd_3" 8
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "sseadd,sseadd1")
            (ior (eq_attr "mode" "V2DF") (eq_attr "atom_unit" "complex"))))
  "atom-complex, atom-all-eu*7")

;; Except dppd/dpps
(define_insn_reservation  "atom_ssemul" 5
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "ssemul")
            (eq_attr "mode" "!SF")))
  "atom-fmul-5c")

;; Except dppd/dpps, 4 cycle if mulss
(define_insn_reservation  "atom_ssemul_2" 4
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "ssemul")
            (eq_attr "mode" "SF")))
  "atom-fmul-4c")

(define_insn_reservation  "atom_ssecmp" 1
  (and (eq_attr "cpu" "atom")
       (eq_attr "type" "ssecmp"))
  "atom-simple-either")

(define_insn_reservation  "atom_ssecomi" 10
  (and (eq_attr "cpu" "atom")
       (eq_attr "type" "ssecomi"))
  "atom-complex, atom-all-eu*9")

;; no memory and cvtpi2ps, cvtps2pi, cvttps2pi
(define_insn_reservation  "atom_ssecvt" 5
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "ssecvt")
            (ior (and (match_operand:V2SI 0 "register_operand")
                      (match_operand:V4SF 1 "register_operand"))
                 (and (match_operand:V4SF 0 "register_operand")
                      (match_operand:V2SI 1 "register_operand")))))
  "atom-fadd-5c")

;; memory and cvtpi2ps, cvtps2pi, cvttps2pi
(define_insn_reservation  "atom_ssecvt_2" 5
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "ssecvt")
            (ior (and (match_operand:V2SI 0 "register_operand")
                      (match_operand:V4SF 1 "memory_operand"))
                 (and (match_operand:V4SF 0 "register_operand")
                      (match_operand:V2SI 1 "memory_operand")))))
  "atom-dual-5c")

;; otherwise. 7 cycles average for cvtss2sd
(define_insn_reservation  "atom_ssecvt_3" 7
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "ssecvt")
            (not (ior (and (match_operand:V2SI 0 "register_operand")
                           (match_operand:V4SF 1 "nonimmediate_operand"))
                      (and (match_operand:V4SF 0 "register_operand")
                           (match_operand:V2SI 1 "nonimmediate_operand"))))))
  "atom-complex, atom-all-eu*6")

;; memory and cvtsi2sd
(define_insn_reservation  "atom_sseicvt" 5
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "sseicvt")
            (and (match_operand:V2DF 0 "register_operand")
                 (match_operand:SI 1 "memory_operand"))))
  "atom-dual-5c")

;; otherwise. 8 cycles average for cvtsd2si
(define_insn_reservation  "atom_sseicvt_2" 8
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "sseicvt")
            (not (and (match_operand:V2DF 0 "register_operand")
                      (match_operand:SI 1 "memory_operand")))))
  "atom-complex, atom-all-eu*7")

(define_insn_reservation  "atom_ssediv" 62
  (and (eq_attr "cpu" "atom")
       (eq_attr "type" "ssediv"))
  "atom-complex, atom-all-eu*12, nothing*49")

;; simple for fmov
(define_insn_reservation  "atom_fmov" 1
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "fmov")
            (eq_attr "memory" "none")))
  "atom-simple-either")

;; simple for fmov
(define_insn_reservation  "atom_fmov_mem" 1
  (and (eq_attr "cpu" "atom")
       (and (eq_attr "type" "fmov")
            (eq_attr "memory" "!none")))
  "atom-simple-either")

;; Define bypass here

;; There will be no stall from lea to non-mem EX insns
(define_bypass 0 "atom_lea"
                 "atom_alu_carry,
                  atom_alu,atom_alu1,atom_negnot,atom_imov,atom_imovx,
                  atom_incdec, atom_setcc, atom_icmov, atom_pop")

(define_bypass 0 "atom_lea"
                 "atom_alu_mem, atom_alu_carry_mem, atom_alu1_mem,
                  atom_imovx_mem, atom_imovx_2_mem,
                  atom_imov_mem, atom_icmov_mem, atom_fmov_mem"
                 "!ix86_agi_dependent")

;; There will be 3 cycles stall from EX insns to AGAN insns LEA
(define_bypass 4 "atom_alu_carry,
                  atom_alu,atom_alu1,atom_negnot,atom_imov,atom_imovx,
                  atom_incdec,atom_ishift,atom_ishift1,atom_rotate,
                  atom_rotate1, atom_setcc, atom_icmov, atom_pop,
                  atom_alu_mem, atom_alu_carry_mem, atom_alu1_mem,
                  atom_imovx_mem, atom_imovx_2_mem,
                  atom_imov_mem, atom_icmov_mem, atom_fmov_mem"
                 "atom_lea")

;; There will be 3 cycles stall from EX insns to insns need addr calculation
(define_bypass 4 "atom_alu_carry,
                  atom_alu,atom_alu1,atom_negnot,atom_imov,atom_imovx,
                  atom_incdec,atom_ishift,atom_ishift1,atom_rotate,
                  atom_rotate1, atom_setcc, atom_icmov, atom_pop,
                  atom_imovx_mem, atom_imovx_2_mem,
                  atom_alu_mem, atom_alu_carry_mem, atom_alu1_mem,
                  atom_imov_mem, atom_icmov_mem, atom_fmov_mem"
                 "atom_alu_mem, atom_alu_carry_mem, atom_alu1_mem,
                  atom_negnot_mem, atom_imov_mem, atom_incdec_mem,
                  atom_imovx_mem, atom_imovx_2_mem,
                  atom_imul_mem, atom_icmp_mem,
                  atom_test_mem, atom_icmov_mem, atom_sselog_mem,
                  atom_sselog1_mem, atom_fmov_mem, atom_sseadd_mem,
                  atom_ishift_mem, atom_ishift1_mem, 
                  atom_rotate_mem, atom_rotate1_mem"
                  "ix86_agi_dependent")

;; Stall from imul to lea is 8 cycles.
(define_bypass 9 "atom_imul, atom_imul_mem" "atom_lea")

;; Stall from imul to memory address is 8 cycles.
(define_bypass 9 "atom_imul, atom_imul_mem" 
                 "atom_alu_mem, atom_alu_carry_mem, atom_alu1_mem,
                  atom_negnot_mem, atom_imov_mem, atom_incdec_mem,
                  atom_ishift_mem, atom_ishift1_mem, atom_rotate_mem,
                  atom_rotate1_mem, atom_imul_mem, atom_icmp_mem,
                  atom_test_mem, atom_icmov_mem, atom_sselog_mem,
                  atom_sselog1_mem, atom_fmov_mem, atom_sseadd_mem"
                  "ix86_agi_dependent")

;; There will be 0 cycle stall from cmp/test to jcc

;; There will be 1 cycle stall from flag producer to cmov and adc/sbb
(define_bypass 2 "atom_icmp, atom_test, atom_alu, atom_alu_carry,
                  atom_alu1, atom_negnot, atom_incdec, atom_ishift,
                  atom_ishift1, atom_rotate, atom_rotate1"
                 "atom_icmov, atom_alu_carry")

;; lea to shift count stall is 2 cycles
(define_bypass 3 "atom_lea"
                 "atom_ishift, atom_ishift1, atom_rotate, atom_rotate1,
                  atom_ishift_mem, atom_ishift1_mem, 
                  atom_rotate_mem, atom_rotate1_mem"
                 "ix86_dep_by_shift_count")

;; lea to shift source stall is 1 cycle
(define_bypass 2 "atom_lea"
                 "atom_ishift, atom_ishift1, atom_rotate, atom_rotate1"
                 "!ix86_dep_by_shift_count")

;; non-lea to shift count stall is 1 cycle
(define_bypass 2 "atom_alu_carry,
                  atom_alu,atom_alu1,atom_negnot,atom_imov,atom_imovx,
                  atom_incdec,atom_ishift,atom_ishift1,atom_rotate,
                  atom_rotate1, atom_setcc, atom_icmov, atom_pop,
                  atom_alu_mem, atom_alu_carry_mem, atom_alu1_mem,
                  atom_imovx_mem, atom_imovx_2_mem,
                  atom_imov_mem, atom_icmov_mem, atom_fmov_mem"
                 "atom_ishift, atom_ishift1, atom_rotate, atom_rotate1,
                  atom_ishift_mem, atom_ishift1_mem, 
                  atom_rotate_mem, atom_rotate1_mem"
                 "ix86_dep_by_shift_count")
