;; Copyright (C) 2012-2025 Free Software Foundation, Inc.
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

;; ZHAOXIN yongfeng processor Scheduling
;; Modeling automatons for yongfeng decoders, integer execution pipes,
;; FP execution pipes, AGU pipes, and dividers.
(define_automaton "yongfeng_decoder,yongfeng_ieu,yongfeng_fp,yongfeng_agu,yongfeng_idiv,yongfeng_fdiv")

;; The rules for the decoder are simple:
;;  - an instruction with 1 uop can be decoded by any of the four
;;    decoders in one cycle.
;;  - an instruction with 2 uops can be decoded by decoder 0 or decoder 1
;;    or decoder 2 but still in only one cycle.
;;  - a complex (microcode) instruction can only be decoded by
;;    decoder 0, and this takes an unspecified number of cycles.
;;
;; The goal is to schedule such that we have a few-one-two uops sequence
;; in each cycle, to decode as many instructions per cycle as possible.
(define_cpu_unit "yf_decoder0" "yongfeng_decoder")
(define_cpu_unit "yf_decoder1" "yongfeng_decoder")
(define_cpu_unit "yf_decoder2" "yongfeng_decoder")
(define_cpu_unit "yf_decoder3" "yongfeng_decoder")

;; We first wish to find an instruction for yf_decoder0, so exclude
;; other decoders from being reserved until yf_decoder0 is
;; reserved
(presence_set "yf_decoder1" "yf_decoder0")
(presence_set "yf_decoder2" "yf_decoder0")
(presence_set "yf_decoder3" "yf_decoder0")

;; Most instructions can be decoded on any of the three decoders.
(define_reservation "yf_decodern" "yf_decoder0|yf_decoder1|yf_decoder2|yf_decoder3")
(define_reservation "yf_decoder012" "yf_decoder0|yf_decoder1|yf_decoder2")

;; The out-of-order core has ten pipelines. Port 0,1,2,3 are integer execution
;; pipelines, port 4, 5 are responsible for address calculation, load and store,
;; port 6,7,8,9 are FP pipelines.
(define_cpu_unit "yf_p0,yf_p1,yf_p2,yf_p3" "yongfeng_ieu")
(define_cpu_unit "yf_p4,yf_p5" "yongfeng_agu")
(define_cpu_unit "yf_p6,yf_p7,yf_p8,yf_p9" "yongfeng_fp")

(define_cpu_unit "yf_idiv" "yongfeng_idiv")
(define_cpu_unit "yf_fdiv" "yongfeng_fdiv")

(define_reservation "yf_ieu" "yf_p0|yf_p1|yf_p2|yf_p3")
(define_reservation "yf_p01" "yf_p0|yf_p1")
(define_reservation "yf_agu" "yf_p4|yf_p5")
(define_reservation "yf_feu" "yf_p6|yf_p7|yf_p8|yf_p9")

;; Only the irregular instructions have to be modeled here.

;; Complex instruction.
(define_insn_reservation "yongfeng_complex_insn" 6
			 (and (eq_attr "cpu" "yongfeng")
			      (eq_attr "type" "other,multi,str"))
			 "yf_decoder0")

;; Call instruction.
(define_insn_reservation "yongfeng_call" 3
			 (and (eq_attr "cpu" "yongfeng")
			      (eq_attr "type" "call,callv"))
			 "yf_decoder012,yf_agu,yf_ieu*3")
;; Push and pop.
(define_insn_reservation "yongfeng_push_reg" 1
			 (and (eq_attr "cpu" "yongfeng")
			      (and (eq_attr "memory" "store")
				   (eq_attr "type" "push")))
			 "yf_decodern,yf_agu,yf_ieu")

(define_insn_reservation "yongfeng_push_mem" 4
			 (and (eq_attr "cpu" "yongfeng")
			      (and (eq_attr "memory" "both")
				   (eq_attr "type" "push")))
			 "yf_decoder012,yf_agu,yf_ieu")

(define_insn_reservation "yongfeng_pop_reg" 4
			 (and (eq_attr "cpu" "yongfeng")
			      (and (eq_attr "memory" "load")
				   (eq_attr "type" "pop")))
			 "yf_decoder012,yf_p01,yf_agu")

(define_insn_reservation "yongfeng_pop_mem" 4
			 (and (eq_attr "cpu" "yongfeng")
			      (and (eq_attr "memory" "both")
				   (eq_attr "type" "pop")))
			 "yf_decoder0,yf_agu,yf_ieu")

(define_insn_reservation "yongfeng_leave" 3
			 (and (eq_attr "cpu" "yongfeng")
			      (eq_attr "type" "leave"))
			 "yf_decoder0,yf_agu,yf_p01*3")

;; MOV - integer moves.
(define_insn_reservation "yongfeng_imov" 1
			 (and (eq_attr "cpu" "yongfeng")
			      (and (eq_attr "memory" "none")
				        (eq_attr "type" "imov,imovx")))
			 "yf_decodern,yf_ieu")

(define_insn_reservation "yongfeng_imov_load" 4
			 (and (eq_attr "cpu" "yongfeng")
			      (and (eq_attr "memory" "load")
				   (eq_attr "type" "imov")))
			 "yf_decodern,yf_agu")

(define_insn_reservation "yongfeng_imovx_load" 4
			 (and (eq_attr "cpu" "yongfeng")
			      (and (eq_attr "memory" "load")
				   (eq_attr "type" "imovx")))
			 "yf_decoder012,yf_agu,yf_ieu|yf_ieu")

(define_insn_reservation "yongfeng_imov_store" 1
			 (and (eq_attr "cpu" "yongfeng")
			      (and (eq_attr "memory" "store")
				   (eq_attr "type" "imov")))
			 "yf_decodern,yf_agu,yf_ieu")

(define_insn_reservation "yongfeng_int_insn" 1
			 (and (eq_attr "cpu" "yongfeng")
			      (and (eq_attr "memory" "none,unknown")
				   (eq_attr "type" "alu,alu1,icmov,icmp,test,lea,ishift1,rotate,rotate1,setcc,incdec")))
			 "yf_decodern,yf_ieu")

(define_insn_reservation "yongfeng_int_insn_load" 5
			 (and (eq_attr "cpu" "yongfeng")
			      (and (eq_attr "memory" "load")
				   (eq_attr "type" "alu,alu1,icmov,icmp,test,ishift1,rotate,rotate1,setcc")))
			 "yf_decoder012,yf_agu,yf_ieu")

(define_insn_reservation "yongfeng_int_insn_store" 1
			 (and (eq_attr "cpu" "yongfeng")
			      (and (eq_attr "memory" "store")
				   (eq_attr "type" "alu,alu1,icmp,test,ishift1,rotate,rotate1,setcc")))
			 "yf_decoder012,yf_agu,yf_ieu")

(define_insn_reservation "yongfeng_int_insn_both" 5
			 (and (eq_attr "cpu" "yongfeng")
			      (and (eq_attr "memory" "both")
				   (eq_attr "type" "alu,alu1,icmp,test,ishift1,rotate,rotate1,setcc,incdec")))
			 "yf_decoder012,yf_agu,yf_ieu")

(define_insn_reservation "yongfeng_shift_HI" 5
			 (and (eq_attr "cpu" "yongfeng")
			      (and (eq_attr "memory" "none,unknown")
				   (and (eq_attr "mode" "HI")
			                (eq_attr "type" "ishift"))))
			 "yf_decoder0,yf_ieu")

(define_insn_reservation "yongfeng_shift_SIDI" 2
			 (and (eq_attr "cpu" "yongfeng")
			      (and (eq_attr "memory" "none,unknown")
				   (and (eq_attr "mode" "SI,DI")
			                (eq_attr "type" "ishift"))))
			 "yf_decoder0,yf_ieu")

(define_insn_reservation "yongfeng_shift_HI_mem" 9
			 (and (eq_attr "cpu" "yongfeng")
			      (and (eq_attr "memory" "!none")
				   (and (eq_attr "mode" "HI")
			                (eq_attr "type" "ishift"))))
			 "yf_decoder0,yf_agu,yf_ieu")

(define_insn_reservation "yongfeng_shift_SIDI_mem" 6
			 (and (eq_attr "cpu" "yongfeng")
			      (and (eq_attr "memory" "!none")
				   (and (eq_attr "mode" "SI,DI")
			                (eq_attr "type" "ishift"))))
			 "yf_decoder0,yf_agu,yf_ieu")

(define_insn_reservation "yongfeng_negnot_QIHI" 2
			 (and (eq_attr "cpu" "yongfeng")
			      (and (eq_attr "memory" "none,unknown")
				   (and (eq_attr "mode" "QI,HI")
			                (eq_attr "type" "negnot"))))
			 "yf_decoder012,yf_ieu|yf_ieu")

(define_insn_reservation "yongfeng_negnot_SIDI" 1
			 (and (eq_attr "cpu" "yongfeng")
			      (and (eq_attr "memory" "none,unknown")
				   (and (eq_attr "mode" "SI,DI")
			                (eq_attr "type" "negnot"))))
			 "yf_decodern,yf_ieu")

(define_insn_reservation "yongfeng_negnot_QIHI_mem" 6
			 (and (eq_attr "cpu" "yongfeng")
			      (and (eq_attr "memory" "!none")
				   (and (eq_attr "mode" "QI,HI")
			                (eq_attr "type" "negnot"))))
			 "yf_decoder012,yf_agu,yf_ieu")

(define_insn_reservation "yongfeng_negnot_SIDI_mem" 5
			 (and (eq_attr "cpu" "yongfeng")
			      (and (eq_attr "memory" "!none")
				   (and (eq_attr "mode" "SI,DI")
			                (eq_attr "type" "negnot"))))
			 "yf_decoder012,yf_agu,yf_ieu")

;; branch instruction
(define_insn_reservation "yongfeng_branch" 3
			 (and (eq_attr "cpu" "yongfeng")
			      (and (eq_attr "memory" "none")
				   (eq_attr "type" "ibr")))
			 "yf_decodern,yf_p2*3")

(define_insn_reservation "yongfeng_branch_mem" 7
			 (and (eq_attr "cpu" "yongfeng")
			      (and (eq_attr "memory" "!none")
				   (eq_attr "type" "ibr")))
			 "yf_decodern,yf_agu,yf_p2")

;; Integer Multiplication instructions.

(define_insn_reservation "yongfeng_imul_QI" 2
			 (and (eq_attr "cpu" "yongfeng")
			      (and (eq_attr "memory" "none")
				   (and (eq_attr "mode" "QI")
					(eq_attr "type" "imul"))))
			 "yf_decodern,yf_ieu|yf_ieu")

(define_insn_reservation "yongfeng_imul_HI" 3
			 (and (eq_attr "cpu" "yongfeng")
			      (and (eq_attr "memory" "none")
				   (and (eq_attr "mode" "HI")
					(eq_attr "type" "imul"))))
			 "yf_decoder0,yf_ieu")

(define_insn_reservation "yongfeng_imul_SIDI" 2
			 (and (eq_attr "cpu" "yongfeng")
			      (and (eq_attr "memory" "none")
				   (and (eq_attr "mode" "SI,DI")
					(eq_attr "type" "imul"))))
			 "yf_decoder0,yf_ieu|yf_ieu")

(define_insn_reservation "yongfeng_imul_QI_mem" 6
			 (and (eq_attr "cpu" "yongfeng")
			      (and (eq_attr "memory" "!none")
				   (and (eq_attr "mode" "QI")
				         (eq_attr "type" "imul"))))
			 "yf_decoder012,yf_agu,yf_ieu")

(define_insn_reservation "yongfeng_imul_SIDI_mem" 6
			 (and (eq_attr "cpu" "yongfeng")
			      (and (eq_attr "memory" "!none")
				   (and (eq_attr "mode" "SI,DI")
				         (eq_attr "type" "imul"))))
			 "yf_decoder0,yf_agu,yf_ieu")

(define_insn_reservation "yongfeng_imul_HI_mem" 7
			 (and (eq_attr "cpu" "yongfeng")
			      (and (eq_attr "memory" "!none")
				   (and (eq_attr "mode" "HI")
				         (eq_attr "type" "imul"))))
			 "yf_decoder0,yf_agu,yf_ieu")

;; Integer Division instructions.

(define_insn_reservation "yongfeng_idiv_DI" 41
			 (and (eq_attr "cpu" "yongfeng")
			      (and (eq_attr "memory" "none")
				   (and (eq_attr "mode" "DI")
					(eq_attr "type" "idiv"))))
			 "yf_decoder0,yf_ieu,yf_feu,yf_idiv*41")

(define_insn_reservation "yongfeng_idiv_HI" 9
			 (and (eq_attr "cpu" "yongfeng")
			      (and (eq_attr "memory" "none")
				   (and (eq_attr "mode" "HI")
					(eq_attr "type" "idiv"))))
			 "yf_decoder0,yf_ieu,yf_feu,yf_idiv*3")

(define_insn_reservation "yongfeng_idiv_QISI" 8
			 (and (eq_attr "cpu" "yongfeng")
			      (and (eq_attr "memory" "none")
				   (and (eq_attr "mode" "QI,SI")
					(eq_attr "type" "idiv"))))
			 "yf_decoder0,yf_ieu,yf_feu,yf_idiv*3")


(define_insn_reservation "yongfeng_idiv_mem_DI" 45
			 (and (eq_attr "cpu" "yongfeng")
			      (and (eq_attr "memory" "load")
				   (and (eq_attr "mode" "DI")
					(eq_attr "type" "idiv"))))
			 "yf_decoder0,yf_agu,yf_ieu,yf_feu,yf_idiv*41")

(define_insn_reservation "yongfeng_idiv_HI_mem" 13
			 (and (eq_attr "cpu" "yongfeng")
			      (and (eq_attr "memory" "load")
				   (and (eq_attr "mode" "HI")
					(eq_attr "type" "idiv"))))
			 "yf_decoder0,yf_agu,yf_ieu,yf_feu,yf_idiv*3")


(define_insn_reservation "yongfeng_idiv_QISI_mem" 12
			 (and (eq_attr "cpu" "yongfeng")
			      (and (eq_attr "memory" "load")
				   (and (eq_attr "mode" "QI,SI")
					(eq_attr "type" "idiv"))))
			 "yf_decoder0,yf_agu,yf_ieu,yf_feu,yf_idiv*3")

;; MMX,SSE,AVX,AVX2 instructions
;; sse moves

(define_insn_reservation "yongfeng_sse_mov" 1
			 (and (eq_attr "cpu" "yongfeng")
                              (and (eq_attr "mode" "SF,DF,V4SF,V2DF,TI")
			           (and (eq_attr "memory" "none")
				        (eq_attr "type" "ssemov"))))
			 "yf_decodern,yf_feu")

(define_insn_reservation "yongfeng_sse_mov_store" 1
			 (and (eq_attr "cpu" "yongfeng")
                              (and (eq_attr "mode" "SF,DF,V4SF,V2DF,TI")
			           (and (eq_attr "memory" "store")
				        (eq_attr "type" "ssemov"))))
			 "yf_decodern,yf_agu,yf_feu")

(define_insn_reservation "yongfeng_sse_mov_load" 5
			 (and (eq_attr "cpu" "yongfeng")
                              (and (eq_attr "mode" "SF,DF,V4SF,V2DF,TI")
			           (and (eq_attr "memory" "load")
				        (eq_attr "type" "ssemov"))))
			 "yf_decodern,yf_agu,yf_feu")

(define_insn_reservation "yongfeng_avx256_mov" 1
			 (and (eq_attr "cpu" "yongfeng")
                              (and (eq_attr "mode" "V8SF,V4DF,OI")
			           (and (eq_attr "memory" "none")
				        (eq_attr "type" "ssemov"))))
			 "yf_decoder012,yf_feu")

(define_insn_reservation "yongfeng_avx256_mov_store" 1
			 (and (eq_attr "cpu" "yongfeng")
                              (and (eq_attr "mode" "V8SF,V4DF,OI")
			           (and (eq_attr "memory" "store")
				        (eq_attr "type" "ssemov"))))
			 "yf_decoder012,yf_agu,yf_feu")

(define_insn_reservation "yongfeng_avx256_mov_load" 6
			 (and (eq_attr "cpu" "yongfeng")
                              (and (eq_attr "mode" "V8SF,V4DF,OI")
			           (and (eq_attr "memory" "load")
				        (eq_attr "type" "ssemov"))))
			 "yf_decoder012,yf_agu,yf_feu")

;;sse general instructions
(define_insn_reservation "yongfeng_sse_insns" 3
			 (and (eq_attr "cpu" "yongfeng")
                              (and (eq_attr "mode" "SF,DF,V4SF,V2DF,TI")
			           (and (eq_attr "memory" "none")
				        (eq_attr "type" "sseadd,sseadd1,ssemul,ssecmp"))))
			 "yf_decodern,yf_feu|yf_feu")

(define_insn_reservation "yongfeng_sse_insns_load" 7
			 (and (eq_attr "cpu" "yongfeng")
                              (and (eq_attr "mode" "SF,DF,V4SF,V2DF,TI")
			           (and (eq_attr "memory" "load")
				        (eq_attr "type" "sseadd,sseadd1,ssemul,ssecmp"))))
			 "yf_decodern,yf_agu,yf_feu")

(define_insn_reservation "yongfeng_avx256_insns" 3
			 (and (eq_attr "cpu" "yongfeng")
                              (and (eq_attr "mode" "V8SF,V4DF,OI")
			           (and (eq_attr "memory" "none")
				        (eq_attr "type" "sseadd,sseadd1,ssemul,ssecmp"))))
			 "yf_decoder012,yf_feu")

(define_insn_reservation "yongfeng_avx256_insns_load" 8
			 (and (eq_attr "cpu" "yongfeng")
                              (and (eq_attr "mode" "V8SF,V4DF,OI")
			           (and (eq_attr "memory" "load")
				        (eq_attr "type" "sseadd,sseadd1,ssemul,ssecmp"))))
			 "yf_decoder012,yf_agu,yf_feu")

(define_insn_reservation "yongfeng_sse_iadd" 1
			 (and (eq_attr "cpu" "yongfeng")
                              (and (eq_attr "mode" "DI,TI")
			           (and (eq_attr "memory" "none")
				        (eq_attr "type" "sseiadd"))))
			 "yf_decodern,yf_feu")

(define_insn_reservation "yongfeng_sse_iadd_load" 5
			 (and (eq_attr "cpu" "yongfeng")
                              (and (eq_attr "mode" "DI,TI")
			           (and (eq_attr "memory" "load")
				        (eq_attr "type" "sseiadd"))))
			 "yf_decodern,yf_agu,yf_feu")

(define_insn_reservation "yongfeng_avx256_iadd" 1
			 (and (eq_attr "cpu" "yongfeng")
                              (and (eq_attr "mode" "OI")
			           (and (eq_attr "memory" "none")
				        (eq_attr "type" "sseiadd"))))
			 "yf_decoder012,yf_feu")

(define_insn_reservation "yongfeng_avx256_iadd_load" 6
			 (and (eq_attr "cpu" "yongfeng")
                              (and (eq_attr "mode" "OI")
			           (and (eq_attr "memory" "load")
				        (eq_attr "type" "sseiadd"))))
			 "yf_decoder0,yf_agu,yf_feu")

(define_insn_reservation "yongfeng_sse_iadd1" 2
			 (and (eq_attr "cpu" "yongfeng")
			           (and (eq_attr "memory" "none")
				        (eq_attr "type" "sseiadd1")))
			 "yf_decoder0,yf_feu")

(define_insn_reservation "yongfeng_sse_iadd1_load" 6
			 (and (eq_attr "cpu" "yongfeng")
			           (and (eq_attr "memory" "load")
				        (eq_attr "type" "sseiadd1")))
			 "yf_decoder0,yf_agu,yf_feu")

;;sse imul
(define_insn_reservation "yongfeng_sse_imul" 2
			 (and (eq_attr "cpu" "yongfeng")
                              (and (eq_attr "mode" "DI,TI")
			           (and (eq_attr "memory" "none")
				        (eq_attr "type" "sseimul"))))
			 "yf_decodern,yf_feu")

(define_insn_reservation "yongfeng_sse_imul_load" 6
			 (and (eq_attr "cpu" "yongfeng")
                              (and (eq_attr "mode" "DI,TI")
			           (and (eq_attr "memory" "load")
				        (eq_attr "type" "sseimul"))))
			 "yf_decoder012,yf_agu,yf_feu")

(define_insn_reservation "yongfeng_avx256_imul" 2
			 (and (eq_attr "cpu" "yongfeng")
                              (and (eq_attr "mode" "OI")
			           (and (eq_attr "memory" "none")
				        (eq_attr "type" "sseimul"))))
			 "yf_decoder012,yf_feu")

(define_insn_reservation "yongfeng_avx256_imul_load" 7
			 (and (eq_attr "cpu" "yongfeng")
                              (and (eq_attr "mode" "OI")
			           (and (eq_attr "memory" "load")
				        (eq_attr "type" "sseimul"))))
			 "yf_decoder0,yf_agu,yf_feu")

;; sse FMA
(define_insn_reservation "yongfeng_sse_fma" 5
			 (and (eq_attr "cpu" "yongfeng")
                              (and (eq_attr "mode" "SF,DF,V4SF,V2DF")
			           (and (eq_attr "memory" "none")
				        (eq_attr "type" "ssemuladd"))))
			 "yf_decodern,yf_feu")

(define_insn_reservation "yongfeng_sse_fma_load" 9
			 (and (eq_attr "cpu" "yongfeng")
                              (and (eq_attr "mode" "SF,DF,V4SF,V2DF")
			           (and (eq_attr "memory" "load")
				        (eq_attr "type" "ssemuladd"))))
			 "yf_decoder012,yf_agu,yf_feu")

(define_insn_reservation "yongfeng_avx256_fma" 5
			 (and (eq_attr "cpu" "yongfeng")
                              (and (eq_attr "mode" "V8SF,V4DF")
			           (and (eq_attr "memory" "none")
				        (eq_attr "type" "ssemuladd"))))
			 "yf_decoder012,yf_feu")

(define_insn_reservation "yongfeng_avx256_fma_load" 10
			 (and (eq_attr "cpu" "yongfeng")
                              (and (eq_attr "mode" "V8SF,V4DF")
			           (and (eq_attr "memory" "load")
				        (eq_attr "type" "ssemuladd"))))
			 "yf_decoder0,yf_agu,yf_feu")
;; sse div
(define_insn_reservation "yongfeng_ssediv_s" 10
			 (and (eq_attr "cpu" "yongfeng")
                              (and (eq_attr "mode" "SF,V4SF")
			           (and (eq_attr "memory" "none")
				        (eq_attr "type" "ssediv"))))
			 "yf_decodern,yf_fdiv*2")

(define_insn_reservation "yongfeng_ssediv_s_load" 14
			 (and (eq_attr "cpu" "yongfeng")
                              (and (eq_attr "mode" "SF,V4SF")
			           (and (eq_attr "memory" "load")
				        (eq_attr "type" "ssediv"))))
			 "yf_decodern,yf_agu,yf_fdiv*2")

(define_insn_reservation "yongfeng_ssediv_d" 14
			 (and (eq_attr "cpu" "yongfeng")
                              (and (eq_attr "mode" "DF,V2DF")
			           (and (eq_attr "memory" "none")
				        (eq_attr "type" "ssediv"))))
			 "yf_decodern,yf_fdiv*3")

(define_insn_reservation "yongfeng_ssediv_d_load" 18
			 (and (eq_attr "cpu" "yongfeng")
                              (and (eq_attr "mode" "DF,V2DF")
			           (and (eq_attr "memory" "load")
				        (eq_attr "type" "ssediv"))))
			 "yf_decodern,yf_agu,yf_fdiv*3")

(define_insn_reservation "yongfeng_ssediv_avx256_s" 10
			 (and (eq_attr "cpu" "yongfeng")
                              (and (eq_attr "mode" "V8SF")
			           (and (eq_attr "memory" "none")
				        (eq_attr "type" "ssediv"))))
			 "yf_decoder012,yf_fdiv*10")

(define_insn_reservation "yongfeng_ssediv_avx256_s_load" 15
			 (and (eq_attr "cpu" "yongfeng")
                              (and (eq_attr "mode" "V8SF")
			           (and (eq_attr "memory" "none")
				        (eq_attr "type" "ssediv"))))
			 "yf_decoder012,yf_agu,yf_fdiv*10")

(define_insn_reservation "yongfeng_ssediv_avx256_d" 14
			 (and (eq_attr "cpu" "yongfeng")
                              (and (eq_attr "mode" "V4DF")
			           (and (eq_attr "memory" "none")
				        (eq_attr "type" "ssediv"))))
			 "yf_decoder012,yf_fdiv*14")

(define_insn_reservation "yongfeng_ssediv_avx256_d_load" 19
			 (and (eq_attr "cpu" "yongfeng")
                              (and (eq_attr "mode" "V4DF")
			           (and (eq_attr "memory" "load")
				        (eq_attr "type" "ssediv"))))
			 "yf_decoder012,yf_fdiv*14")

;;sse logical and shuffle instructions
(define_insn_reservation "yongfeng_avx256_log_shuf" 1
			 (and (eq_attr "cpu" "yongfeng")
                              (and (eq_attr "mode" "V8SF,V4DF,OI")
			           (and (eq_attr "memory" "none")
				        (eq_attr "type" "sselog,sselog1,sseshuf,sseshuf1"))))
			 "yf_decoder012,yf_feu")

(define_insn_reservation "yongfeng_avx256_log_shuf_load" 6
			 (and (eq_attr "cpu" "yongfeng")
                              (and (eq_attr "mode" "V8SF,V4DF,OI")
			           (and (eq_attr "memory" "load")
				        (eq_attr "type" "sselog,sselog1,sseshuf,sseshuf1"))))
			 "yf_decoder012,yf_agu,yf_feu")

(define_insn_reservation "yongfeng_sse_log_shuf" 1
			 (and (eq_attr "cpu" "yongfeng")
			      (and (eq_attr "memory" "none")
				   (eq_attr "type" "sselog,sselog1,sseshuf,sseshuf1")))
			 "yf_decodern,yf_feu")

(define_insn_reservation "yongfeng_sse_log_shuf_load" 5
			 (and (eq_attr "cpu" "yongfeng")
			      (and (eq_attr "memory" "load")
				   (eq_attr "type" "sselog,sselog1,sseshuf,sseshuf1")))
			 "yf_decodern,yf_agu,yf_feu")
;;sse shift

(define_insn_reservation "yongfeng_avx256_shift" 1
			 (and (eq_attr "cpu" "yongfeng")
                              (and (eq_attr "mode" "V8SF,V4DF,OI")
			           (and (eq_attr "memory" "none")
				        (eq_attr "type" "sseishft,sseishft1"))))
			 "yf_decoder012,yf_feu")

(define_insn_reservation "yongfeng_avx256_shift_load" 6
			 (and (eq_attr "cpu" "yongfeng")
                              (and (eq_attr "mode" "V8SF,V4DF,OI")
			           (and (eq_attr "memory" "load")
				        (eq_attr "type" "sseishft,sseishft1"))))
			 "yf_decoder0,yf_agu,yf_feu")

(define_insn_reservation "yongfeng_sse_shift" 1
			 (and (eq_attr "cpu" "yongfeng")
			      (and (eq_attr "memory" "none")
				   (eq_attr "type" "sseishft,sseishft1")))
			 "yf_decodern,yf_feu")

(define_insn_reservation "yongfeng_sse_shift_load" 5
			 (and (eq_attr "cpu" "yongfeng")
			      (and (eq_attr "memory" "load")
				   (eq_attr "type" "sseishft,sseishft1")))
			 "yf_decodern,yf_agu,yf_feu")
;;sse comi
(define_insn_reservation "yongfeng_avx256_test" 4
			 (and (eq_attr "cpu" "yongfeng")
                              (and (eq_attr "mode" "V8SF,V4DF,OI")
                                   (and (eq_attr "prefix_extra" "1")
			                (and (eq_attr "memory" "none")
				             (eq_attr "type" "ssecomi")))))
			 "yf_decoder012,yf_ieu*3")

(define_insn_reservation "yongfeng_avx256_test_load" 9
			 (and (eq_attr "cpu" "yongfeng")
                              (and (eq_attr "mode" "V8SF,V4DF,OI")
                                   (and (eq_attr "prefix_extra" "1")
			                (and (eq_attr "memory" "load")
				             (eq_attr "type" "ssecomi")))))
			 "yf_decoder012,yf_agu,yf_ieu,yf_p6*3")

(define_insn_reservation "yongfeng_sse_test" 3
			 (and (eq_attr "cpu" "yongfeng")
                              (and (eq_attr "prefix_extra" "1")
			           (and (eq_attr "memory" "none")
				        (eq_attr "type" "ssecomi"))))
			 "yf_decodern,yf_feu|yf_feu")

(define_insn_reservation "yongfeng_sse_test_load" 7
			 (and (eq_attr "cpu" "yongfeng")
                              (and (eq_attr "prefix_extra" "1")
			           (and (eq_attr "memory" "load")
				        (eq_attr "type" "ssecomi"))))
			 "yf_decodern,yf_agu,yf_feu")

(define_insn_reservation "yongfeng_sse_comi" 1
			 (and (eq_attr "cpu" "yongfeng")
                              (and (eq_attr "prefix_extra" "0")
			           (and (eq_attr "memory" "none")
				        (eq_attr "type" "ssecomi"))))
			 "yf_decodern,yf_feu|yf_feu")

(define_insn_reservation "yongfeng_sse_comi_load" 4
			 (and (eq_attr "cpu" "yongfeng")
                              (and (eq_attr "prefix_extra" "0")
			           (and (eq_attr "memory" "load")
				        (eq_attr "type" "ssecomi"))))
			 "yf_decodern,yf_agu,yf_feu")

;;sse conversion
(define_insn_reservation "yongfeng_avx_cvt_ps" 4
			 (and (eq_attr "cpu" "yongfeng")
                              (and (eq_attr "mode" "V4SF")
			           (and (eq_attr "memory" "none")
				        (eq_attr "type" "ssecvt"))))
			 "yf_decoder0,yf_feu")

(define_insn_reservation "yongfeng_avx_cvt_ps_load" 8
			 (and (eq_attr "cpu" "yongfeng")
                              (and (eq_attr "mode" "V4SF")
			           (and (eq_attr "memory" "load")
				        (eq_attr "type" "ssecvt"))))
			 "yf_decoder0,yf_agu,yf_feu")

(define_insn_reservation "yongfeng_avx_cvt_pd" 3
			 (and (eq_attr "cpu" "yongfeng")
                              (and (eq_attr "mode" "V4DF")
			           (and (eq_attr "memory" "none")
				        (eq_attr "type" "ssecvt"))))
			 "yf_decoder0,yf_feu")

(define_insn_reservation "yongfeng_avx_cvt_pd_load" 7
			 (and (eq_attr "cpu" "yongfeng")
                              (and (eq_attr "mode" "V4DF")
			           (and (eq_attr "memory" "load")
				        (eq_attr "type" "ssecvt"))))
			 "yf_decoder0,yf_agu,yf_feu")

(define_insn_reservation "yongfeng_sse_cvt" 3
			 (and (eq_attr "cpu" "yongfeng")
			           (and (eq_attr "memory" "none")
				        (eq_attr "type" "ssecvt")))
			 "yf_decodern,yf_feu|yf_feu")

(define_insn_reservation "yongfeng_sse_cvt_load" 7
			 (and (eq_attr "cpu" "yongfeng")
			           (and (eq_attr "memory" "load")
				        (eq_attr "type" "ssecvt")))
			 "yf_decoder012,yf_agu,yf_feu")

(define_insn_reservation "yongfeng_sse_icvt" 3
			 (and (eq_attr "cpu" "yongfeng")
			           (and (eq_attr "memory" "none")
				        (eq_attr "type" "sseicvt")))
			 "yf_decodern,yf_feu|yf_feu")

(define_insn_reservation "yongfeng_sse_icvt_load" 7
			 (and (eq_attr "cpu" "yongfeng")
			           (and (eq_attr "memory" "load")
				        (eq_attr "type" "sseicvt")))
			 "yf_decoder012,yf_agu,yf_feu")

(define_insn_reservation "yongfeng_sse_icvt_SI" 1
			 (and (eq_attr "cpu" "yongfeng")
                              (and (eq_attr "mode" "SI")
			           (and (eq_attr "memory" "none")
				        (eq_attr "type" "sseicvt"))))
			 "yf_decoder012,yf_feu")

(define_insn_reservation "yongfeng_sse_icvt_SI_load" 5
			 (and (eq_attr "cpu" "yongfeng")
                              (and (eq_attr "mode" "SI")
			           (and (eq_attr "memory" "load")
				        (eq_attr "type" "sseicvt"))))
			 "yf_decoder012,yf_agu,yf_feu")

(define_insn_reservation "yongfeng_sse_icvt_DI" 2
			 (and (eq_attr "cpu" "yongfeng")
                              (and (eq_attr "mode" "DI")
			           (and (eq_attr "memory" "none")
				        (eq_attr "type" "sseicvt"))))
			 "yf_decoder0,yf_feu")

(define_insn_reservation "yongfeng_sse_icvt_DI_load" 6
			 (and (eq_attr "cpu" "yongfeng")
                              (and (eq_attr "mode" "DI")
			           (and (eq_attr "memory" "load")
				        (eq_attr "type" "sseicvt"))))
			 "yf_decoder0,yf_agu,yf_feu")
;; MMX
(define_insn_reservation "yongfeng_mmx_move" 1
			 (and (eq_attr "cpu" "yongfeng")
			      (and (eq_attr "memory" "none")
				   (eq_attr "type" "mmxmov")))
			 "yf_decodern,yf_p0")

(define_insn_reservation "yongfeng_mmx_move_load" 5
			 (and (eq_attr "cpu" "yongfeng")
			      (and (eq_attr "memory" "load")
				   (eq_attr "type" "mmxmov")))
			 "yf_decodern,yf_agu,yf_p0")

(define_insn_reservation "yongfeng_mmx_move_store" 1
			 (and (eq_attr "cpu" "yongfeng")
			      (and (eq_attr "memory" "store")
				   (eq_attr "type" "mmxmov")))
			 "yf_decodern,yf_agu,yf_p0")

(define_insn_reservation "yongfeng_mmx_mul" 2
			 (and (eq_attr "cpu" "yongfeng")
			      (and (eq_attr "memory" "none")
				   (eq_attr "type" "mmxmul")))
			 "yf_decodern,yf_feu")

(define_insn_reservation "yongfeng_mmx_mul_load" 6
			 (and (eq_attr "cpu" "yongfeng")
			      (and (eq_attr "memory" "load")
				   (eq_attr "type" "mmxmul")))
			 "yf_decoder012,yf_agu,yf_feu")

;; MMX general instructions
(define_insn_reservation "yongfeng_mmx_insns" 1
			 (and (eq_attr "cpu" "yongfeng")
			      (and (eq_attr "memory" "none")
				   (eq_attr "type" "mmxadd,mmxshft,mmxcmp,mmx,mmxcvt")))
			 "yf_decodern,yf_feu|yf_feu")

(define_insn_reservation "yongfeng_mmx_insns_load" 5
			 (and (eq_attr "cpu" "yongfeng")
			      (and (eq_attr "memory" "load")
				   (eq_attr "type" "mmxadd,mmxshft,mmxcmp,mmx,mmxcvt")))
			 "yf_decodern,yf_agu,yf_feu|yf_feu")

(define_insn_reservation "yongfeng_mmx_insns_store" 1
			 (and (eq_attr "cpu" "yongfeng")
			      (and (eq_attr "memory" "store")
				   (eq_attr "type" "mmxadd,mmxshft,mmxcmp,mmx,mmxcvt")))
			 "yf_decodern,yf_agu,yf_feu")

;; x87 floating point operations.

(define_insn_reservation "yongfeng_fxch" 1
			 (and (eq_attr "cpu" "yongfeng")
			      (eq_attr "type" "fxch"))
			 "yf_decodern,yf_p0|yf_p1")

(define_insn_reservation "yongfeng_fcmov_sgn" 1
			 (and (eq_attr "cpu" "yongfeng")
			      (eq_attr "type" "fcmov,fsgn"))
			 "yf_decodern,yf_p0|yf_p1,yf_feu")

(define_insn_reservation "yongfeng_fcmp" 1
			 (and (eq_attr "cpu" "yongfeng")
                              (and (eq_attr "memory" "none")
			           (eq_attr "type" "fcmp")))
			 "yf_decodern,yf_feu")

(define_insn_reservation "yongfeng_fcmp_load" 5
			 (and (eq_attr "cpu" "yongfeng")
                              (and (eq_attr "memory" "load")
			           (eq_attr "type" "fcmp")))
			 "yf_decodern,yf_agu,yf_feu")

(define_insn_reservation "yongfeng_fmov" 1
			 (and (eq_attr "cpu" "yongfeng")
                              (and (eq_attr "memory" "none")
			           (eq_attr "type" "fmov")))
			 "yf_decodern,yf_feu")

(define_insn_reservation "yongfeng_fmov_store" 1
			 (and (eq_attr "cpu" "yongfeng")
                              (and (eq_attr "memory" "store")
			           (eq_attr "type" "fmov")))
			 "yf_decoder0,yf_agu,yf_feu")

(define_insn_reservation "yongfeng_fmov_load" 5
			 (and (eq_attr "cpu" "yongfeng")
                              (and (eq_attr "memory" "load")
			           (eq_attr "type" "fmov")))
			 "yf_decoder0,yf_agu,yf_feu")

(define_insn_reservation "yongfeng_fistp" 5
			 (and (eq_attr "cpu" "yongfeng")
			      (eq_attr "type" "fistp,fisttp"))
			 "yf_decoder012,yf_agu,yf_feu")

(define_insn_reservation "yongfeng_fop_mul" 3
			 (and (eq_attr "cpu" "yongfeng")
			      (and (eq_attr "memory" "none,unknown")
				   (eq_attr "type" "fop,fmul")))
			 "yf_decodern,yf_feu")

(define_insn_reservation "yongfeng_fop_mul_load" 7
			 (and (eq_attr "cpu" "yongfeng")
			      (and (eq_attr "memory" "load,both")
				   (eq_attr "type" "fop,fmul")))
			 "yf_decoder012,yf_agu,yf_feu")

(define_insn_reservation "yf_fop_store" 3
			 (and (eq_attr "cpu" "yongfeng")
			      (and (eq_attr "memory" "store")
				   (eq_attr "type" "fop")))
			 "yf_decodern,yf_agu,yf_feu")

(define_insn_reservation "yongfeng_fdiv_fpspc" 14
			 (and (eq_attr "cpu" "yongfeng")
			      (and (eq_attr "memory" "none")
				   (eq_attr "type" "fdiv,fpspc")))
			 "yf_decodern,yf_fdiv*7")

(define_insn_reservation "yongfeng_fdiv_fpspc_load" 18
			 (and (eq_attr "cpu" "yongfeng")
			      (and (eq_attr "memory" "load")
				   (eq_attr "type" "fdiv,fpspc")))
			 "yf_decoder012,yf_agu,yf_fdiv*7")
