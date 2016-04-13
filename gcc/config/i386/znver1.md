;; Copyright (C) 2012-2016 Free Software Foundation, Inc.
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

(define_attr "znver1_decode" "direct,vector,double"
  (const_string "direct"))

;; AMD znver1 Scheduling
;; Modeling automatons for zen decoders, integer execution pipes,
;; AGU pipes and floating point execution units.
(define_automaton "znver1, znver1_ieu, znver1_fp, znver1_agu")

;; Decoders unit has 4 decoders and all of them can decode fast path
;; and vector type instructions.
(define_cpu_unit "znver1-decode0" "znver1")
(define_cpu_unit "znver1-decode1" "znver1")
(define_cpu_unit "znver1-decode2" "znver1")
(define_cpu_unit "znver1-decode3" "znver1")

;; Currently blocking all decoders for vector path instructions as
;; they are dispatched separetely as microcode sequence.
;; Fix me: Need to revisit this.
(define_reservation "znver1-vector" "znver1-decode0+znver1-decode1+znver1-decode2+znver1-decode3")

;; Direct instructions can be issued to any of the four decoders.
(define_reservation "znver1-direct" "znver1-decode0|znver1-decode1|znver1-decode2|znver1-decode3")

;; Fix me: Need to revisit this later to simulate fast path double behavior.
(define_reservation "znver1-double" "znver1-direct")


;; Integer unit 4 ALU pipes.
(define_cpu_unit "znver1-ieu0" "znver1_ieu")
(define_cpu_unit "znver1-ieu1" "znver1_ieu")
(define_cpu_unit "znver1-ieu2" "znver1_ieu")
(define_cpu_unit "znver1-ieu3" "znver1_ieu")
(define_reservation "znver1-ieu" "znver1-ieu0|znver1-ieu1|znver1-ieu2|znver1-ieu3")

;; 2 AGU pipes.
(define_cpu_unit "znver1-agu0" "znver1_agu")
(define_cpu_unit "znver1-agu1" "znver1_agu")
(define_reservation "znver1-agu-reserve" "znver1-agu0|znver1-agu1")

(define_reservation "znver1-load" "znver1-agu-reserve")
(define_reservation "znver1-store" "znver1-agu-reserve")

;; vectorpath (microcoded) instructions are single issue instructions.
;; So, they occupy all the integer units.
(define_reservation "znver1-ivector" "znver1-ieu0+znver1-ieu1
				      +znver1-ieu2+znver1-ieu3
				      +znver1-agu0+znver1-agu1")

;; Floating point unit 4 FP pipes.
(define_cpu_unit "znver1-fp0" "znver1_fp")
(define_cpu_unit "znver1-fp1" "znver1_fp")
(define_cpu_unit "znver1-fp2" "znver1_fp")
(define_cpu_unit "znver1-fp3" "znver1_fp")

(define_reservation "znver1-fpu" "znver1-fp0|znver1-fp1|znver1-fp2|znver1-fp3")

(define_reservation "znver1-fvector" "znver1-fp0+znver1-fp1
				      +znver1-fp2+znver1-fp3
				      +znver1-agu0+znver1-agu1")

;; Call instruction
(define_insn_reservation "znver1_call" 1
			 (and (eq_attr "cpu" "znver1")
			      (eq_attr "type" "call,callv"))
			 "znver1-double,znver1-store,znver1-ieu0|znver1-ieu3")

;; General instructions
(define_insn_reservation "znver1_push" 1
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "type" "push")
				   (eq_attr "memory" "none,unknown")))
			 "znver1-direct,znver1-store")

(define_insn_reservation "znver1_push_store" 1
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "type" "push")
				   (eq_attr "memory" "store")))
			 "znver1-direct,znver1-store")

(define_insn_reservation "znver1_push_both" 5
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "type" "push")
				   (eq_attr "memory" "both")))
			 "znver1-direct,znver1-load,znver1-store")

(define_insn_reservation "znver1_pop" 4
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "type" "pop")
				   (eq_attr "memory" "load")))
			 "znver1-direct,znver1-load")

(define_insn_reservation "znver1_pop_mem" 4
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "type" "pop")
				   (eq_attr "memory" "both")))
			 "znver1-direct,znver1-load,znver1-store")

;; Leave
(define_insn_reservation "znver1_leave" 1
			 (and (eq_attr "cpu" "znver1")
			      (eq_attr "type" "leave"))
			 "znver1-double,znver1-ieu, znver1-store")

;; Integer Instructions or General intructions
;; Multiplications
;; Reg operands
(define_insn_reservation "znver1_imul" 3
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "type" "imul")
				   (eq_attr "memory" "none")))
			 "znver1-direct,znver1-ieu1")

(define_insn_reservation "znver1_imul_mem" 7
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "type" "imul")
				   (eq_attr "memory" "!none")))
			 "znver1-direct,znver1-load, znver1-ieu1")

;; Divisions
;; Reg operands
(define_insn_reservation "znver1_idiv_DI" 41
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "type" "idiv")
				   (and (eq_attr "mode" "DI")
					(eq_attr "memory" "none"))))
			 "znver1-double,znver1-ieu2*41")

(define_insn_reservation "znver1_idiv_SI" 25
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "type" "idiv")
				   (and (eq_attr "mode" "SI")
					(eq_attr "memory" "none"))))
			 "znver1-double,znver1-ieu2*25")

(define_insn_reservation "znver1_idiv_HI" 17
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "type" "idiv")
				   (and (eq_attr "mode" "HI")
					(eq_attr "memory" "none"))))
			 "znver1-double,znver1-ieu2*17")

(define_insn_reservation "znver1_idiv_QI" 12
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "type" "idiv")
				   (and (eq_attr "mode" "QI")
					(eq_attr "memory" "none"))))
			 "znver1-direct,znver1-ieu2*12")

;; Mem operands
(define_insn_reservation "znver1_idiv_mem_DI" 45
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "type" "idiv")
				   (and (eq_attr "mode" "DI")
					(eq_attr "memory" "none"))))
			 "znver1-double,znver1-load,znver1-ieu2*41")

(define_insn_reservation "znver1_idiv_mem_SI" 29
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "type" "idiv")
				   (and (eq_attr "mode" "SI")
					(eq_attr "memory" "none"))))
			 "znver1-double,znver1-load,znver1-ieu2*25")

(define_insn_reservation "znver1_idiv_mem_HI" 21
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "type" "idiv")
				   (and (eq_attr "mode" "HI")
					(eq_attr "memory" "none"))))
			 "znver1-double,znver1-load,znver1-ieu2*17")

(define_insn_reservation "znver1_idiv_mem_QI" 16
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "type" "idiv")
				   (and (eq_attr "mode" "QI")
					(eq_attr "memory" "none"))))
			 "znver1-direct,znver1-load,znver1-ieu2*12")

;; STR ISHIFT which are micro coded.
;; Fix me: Latency need to be rechecked.
(define_insn_reservation "znver1_str_ishift" 6
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "type" "str,ishift")
				   (eq_attr "memory" "both,store")))
			 "znver1-vector,znver1-ivector")
;; MOV - integer moves
(define_insn_reservation "znver1_load_imov_double" 2
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "znver1_decode" "double")
				   (and (eq_attr "type" "imovx")
					(eq_attr "memory" "none"))))
			 "znver1-double,znver1-ieu")

(define_insn_reservation "znver1_load_imov_direct" 1
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "type" "imov,imovx")
				   (eq_attr "memory" "none")))
			 "znver1-direct,znver1-ieu")

(define_insn_reservation "znver1_load_imov_double_store" 2
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "znver1_decode" "double")
				   (and (eq_attr "type" "imovx")
					(eq_attr "memory" "store"))))
			 "znver1-double,znver1-ieu,znver1-store")

(define_insn_reservation "znver1_load_imov_direct_store" 1
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "type" "imov,imovx")
				   (eq_attr "memory" "store")))
				   "znver1-direct,znver1-ieu,znver1-store")

(define_insn_reservation "znver1_load_imov_double_load" 6
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "znver1_decode" "double")
				   (and (eq_attr "type" "imovx")
					(eq_attr "memory" "load"))))
			 "znver1-double,znver1-load,znver1-ieu")

(define_insn_reservation "znver1_load_imov_direct_load" 5
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "type" "imov,imovx")
				   (eq_attr "memory" "load")))
			 "znver1-direct,znver1-load,znver1-ieu")

;; INTEGER/GENERAL instructions
;; register/imm operands only: ALU, ICMP, NEG, NOT, ROTATE, ISHIFT, TEST
(define_insn_reservation "znver1_insn" 1
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "type" "alu,icmp,negnot,rotate,rotate1,ishift,ishift1,test,setcc,incdec,icmov")
				   (eq_attr "memory" "none,unknown")))
			 "znver1-direct,znver1-ieu")

(define_insn_reservation "znver1_insn_load" 5
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "type" "alu,icmp,negnot,rotate,rotate1,ishift,ishift1,test,setcc,incdec,icmov")
				   (eq_attr "memory" "load")))
			 "znver1-direct,znver1-load,znver1-ieu")

(define_insn_reservation "znver1_insn_store" 1
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "type" "alu,icmp,negnot,rotate,rotate1,ishift1,test,setcc,incdec")
				   (eq_attr "memory" "store")))
			 "znver1-direct,znver1-ieu,znver1-store")

(define_insn_reservation "znver1_insn_both" 5
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "type" "alu,icmp,negnot,rotate,rotate1,ishift1,test,setcc,incdec")
				   (eq_attr "memory" "both")))
			 "znver1-direct,znver1-load,znver1-ieu,znver1-store")

;; Fix me: Other vector type insns keeping latency 6 as of now.
(define_insn_reservation "znver1_ieu_vector" 6
			 (and (eq_attr "cpu" "znver1")
			      (eq_attr "type" "other,str,multi"))
			 "znver1-vector,znver1-ivector")

;; ALU1 register operands.
(define_insn_reservation "znver1_alu1_vector" 3
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "znver1_decode" "vector")
				   (and (eq_attr "type" "alu1")
					(eq_attr "memory" "none,unknown"))))
			 "znver1-vector,znver1-ivector")

(define_insn_reservation "znver1_alu1_double" 2
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "znver1_decode" "double")
				   (and (eq_attr "type" "alu1")
					(eq_attr "memory" "none,unknown"))))
			 "znver1-double,znver1-ieu")

(define_insn_reservation "znver1_alu1_direct" 1
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "znver1_decode" "direct")
				   (and (eq_attr "type" "alu1")
					(eq_attr "memory" "none,unknown"))))
			 "znver1-direct,znver1-ieu")

;; Branches : Fix me need to model conditional branches.
(define_insn_reservation "znver1_branch" 1
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "type" "ibr")
				   (eq_attr "memory" "none")))
			  "znver1-direct")

;; Indirect branches check latencies.
(define_insn_reservation "znver1_indirect_branch_mem" 6
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "type" "ibr")
				   (eq_attr "memory" "load")))
			 "znver1-vector,znver1-ivector")

;; LEA executes in ALU units with 1 cycle latency.
(define_insn_reservation "znver1_lea" 1
			 (and (eq_attr "cpu" "znver1")
			      (eq_attr "type" "lea"))
			 "znver1-direct,znver1-ieu")

;; Other integer instrucions
(define_insn_reservation "znver1_idirect" 1
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "unit" "integer,unknown")
				   (eq_attr "memory" "none,unknown")))
			 "znver1-direct,znver1-ieu")

;;  Floating point
(define_insn_reservation "znver1_fp_cmov" 6
			 (and (eq_attr "cpu" "znver1")
			      (eq_attr "type" "fcmov"))
			 "znver1-vector,znver1-fvector")

(define_insn_reservation "znver1_fp_mov_direct_load" 8 
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "znver1_decode" "direct")
				   (and (eq_attr "type" "fmov")
					(eq_attr "memory" "load"))))
			 "znver1-direct,znver1-load,znver1-fp3|znver1-fp1")

(define_insn_reservation "znver1_fp_mov_direct_store" 5
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "znver1_decode" "direct")
				   (and (eq_attr "type" "fmov")
					(eq_attr "memory" "store"))))
			 "znver1-direct,znver1-fp2|znver1-fp3,znver1-store")

(define_insn_reservation "znver1_fp_mov_double" 4
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "znver1_decode" "double")
				   (and (eq_attr "type" "fmov")
					(eq_attr "memory" "none"))))
			 "znver1-double,znver1-fp3")

(define_insn_reservation "znver1_fp_mov_double_load" 12
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "znver1_decode" "double")
				   (and (eq_attr "type" "fmov")
					(eq_attr "memory" "load"))))
			 "znver1-double,znver1-load,znver1-fp3")

(define_insn_reservation "znver1_fp_mov_direct" 1
			 (and (eq_attr "cpu" "znver1")
			      (eq_attr "type" "fmov"))
			 "znver1-direct,znver1-fp3")

(define_insn_reservation "znver1_fp_spc_direct" 5
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "type" "fpspc")
				   (eq_attr "memory" "store")))
			 "znver1-direct,znver1-fp3,znver1-fp2")

(define_insn_reservation "znver1_fp_insn_vector" 6
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "znver1_decode" "vector")
				   (eq_attr "type" "fpspc,mmxcvt,sselog1,ssemul,ssemov")))
			 "znver1-vector,znver1-fvector")

;; FABS
(define_insn_reservation "znver1_fp_fsgn" 1
			 (and (eq_attr "cpu" "znver1")
			      (eq_attr "type" "fsgn"))
			 "znver1-direct,znver1-fp3")

(define_insn_reservation "znver1_fp_fcmp" 2
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "memory" "none")
				   (and (eq_attr "znver1_decode" "double")
					(eq_attr "type" "fcmp"))))
			 "znver1-double,znver1-fp0,znver1-fp2")

(define_insn_reservation "znver1_fp_fcmp_load" 9
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "memory" "none")
				   (and (eq_attr "znver1_decode" "double")
					(eq_attr "type" "fcmp"))))
			 "znver1-double,znver1-load, znver1-fp0,znver1-fp2")

;;FADD FSUB FMUL
(define_insn_reservation "znver1_fp_op_mul" 5
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "type" "fop,fmul")
				   (eq_attr "memory" "none")))
			 "znver1-direct,znver1-fp0*5")

(define_insn_reservation "znver1_fp_op_mul_load" 12 
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "type" "fop,fmul")
				   (eq_attr "memory" "load")))
			 "znver1-direct,znver1-load,znver1-fp0*5")

(define_insn_reservation "znver1_fp_op_imul_load" 16
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "type" "fop,fmul")
				   (and (eq_attr "fp_int_src" "true")
					(eq_attr "memory" "load"))))
			"znver1-double,znver1-load,znver1-fp3,znver1-fp0")

(define_insn_reservation "znver1_fp_op_div" 15
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "type" "fdiv")
				   (eq_attr "memory" "none")))
			 "znver1-direct,znver1-fp3*15")

(define_insn_reservation "znver1_fp_op_div_load" 22
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "type" "fdiv")
				   (eq_attr "memory" "load")))
			 "znver1-direct,znver1-load,znver1-fp3*15")

(define_insn_reservation "znver1_fp_op_idiv_load" 27
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "type" "fdiv")
				   (and (eq_attr "fp_int_src" "true")
					(eq_attr "memory" "load"))))
			 "znver1-double,znver1-load,znver1-fp3*19")

;; MMX, SSE, SSEn.n, AVX, AVX2 instructions
(define_insn_reservation "znver1_fp_insn" 1
			 (and (eq_attr "cpu" "znver1")
			      (eq_attr "type" "mmx"))
			 "znver1-direct,znver1-fpu")

(define_insn_reservation "znver1_mmx_add" 1
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "type" "mmxadd")
				   (eq_attr "memory" "none")))
			 "znver1-direct,znver1-fp0|znver1-fp1|znver1-fp3")

(define_insn_reservation "znver1_mmx_add_load" 8
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "type" "mmxadd")
				   (eq_attr "memory" "load")))
			 "znver1-direct,znver1-load,znver1-fp0|znver1-fp1|znver1-fp3")

(define_insn_reservation "znver1_mmx_cmp" 1
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "type" "mmxcmp")
				   (eq_attr "memory" "none")))
			 "znver1-direct,znver1-fp0|znver1-fp3")

(define_insn_reservation "znver1_mmx_cmp_load" 8
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "type" "mmxcmp")
				   (eq_attr "memory" "load")))
			 "znver1-direct,znver1-load,znver1-fp0|znver1-fp3")

(define_insn_reservation "znver1_mmx_cvt_pck_shuf" 1
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "type" "mmxcvt,sseshuf,sseshuf1")
				   (eq_attr "memory" "none")))
			 "znver1-direct,znver1-fp1|znver1-fp2")

(define_insn_reservation "znver1_mmx_cvt_pck_shuf_load" 8
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "type" "mmxcvt,sseshuf,sseshuf1")
				   (eq_attr "memory" "load")))
			 "znver1-direct,znver1-load,znver1-fp1|znver1-fp2")

(define_insn_reservation "znver1_mmx_shift_move" 1
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "type" "mmxshft,mmxmov")
				   (eq_attr "memory" "none")))
 			 "znver1-direct,znver1-fp2")

(define_insn_reservation "znver1_mmx_shift_move_load" 8
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "type" "mmxshft,mmxmov")
				   (eq_attr "memory" "load")))
			 "znver1-direct,znver1-load,znver1-fp2")

(define_insn_reservation "znver1_mmx_move_store" 1
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "type" "mmxshft,mmxmov")
				   (eq_attr "memory" "store,both")))
			  "znver1-direct,znver1-fp2,znver1-store")

(define_insn_reservation "znver1_mmx_mul" 3
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "type" "mmxmul")
				   (eq_attr "memory" "none")))
			  "znver1-direct,znver1-fp0*3")

(define_insn_reservation "znver1_mmx_load" 10
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "type" "mmxmul")
				   (eq_attr "memory" "load")))
			 "znver1-direct,znver1-load,znver1-fp0*3")

(define_insn_reservation "znver1_avx256_log" 1
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "mode" "V8SF,V4DF,OI")
 				   (and (eq_attr "type" "sselog")
					(eq_attr "memory" "none"))))
			 "znver1-double,znver1-fpu")

(define_insn_reservation "znver1_avx256_log_load" 8
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "mode" "V8SF,V4DF,OI")
				   (and (eq_attr "type" "sselog")
					(eq_attr "memory" "load"))))
			 "znver1-double,znver1-load,znver1-fpu")

(define_insn_reservation "znver1_sse_log" 1
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "type" "sselog")
				   (eq_attr "memory" "none")))
			 "znver1-direct,znver1-fpu")

(define_insn_reservation "znver1_sse_log_load" 8
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "type" "sselog")
				   (eq_attr "memory" "load")))
			 "znver1-direct,znver1-load,znver1-fpu")

(define_insn_reservation "znver1_avx256_log1" 1
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "mode" "V8SF,V4DF,OI")
				   (and (eq_attr "type" "sselog1")
					(eq_attr "memory" "none"))))
			 "znver1-double,znver1-fp1|znver1-fp2")

(define_insn_reservation "znver1_avx256_log1_load" 8
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "mode" "V8SF,V4DF,OI")
				   (and (eq_attr "type" "sselog1")
					(eq_attr "memory" "!none"))))
			 "znver1-double,znver1-load,znver1-fp1|znver1-fp2")

(define_insn_reservation "znver1_sse_log1" 1
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "type" "sselog1")
				   (eq_attr "memory" "none")))
			 "znver1-direct,znver1-fp1|znver1-fp2")

(define_insn_reservation "znver1_sse_log1_load" 8
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "type" "sselog1")
				   (eq_attr "memory" "!none")))
			 "znver1-direct,znver1-load,znver1-fp1|znver1-fp2")

(define_insn_reservation "znver1_sse_comi" 1
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "mode" "SF,DF,V4SF,V2DF")
				   (and (eq_attr "prefix" "!vex")
					(and (eq_attr "prefix_extra" "0")
					     (and (eq_attr "type" "ssecomi")
						  (eq_attr "memory" "none"))))))
			 "znver1-direct,znver1-fp0|znver1-fp1")

(define_insn_reservation "znver1_sse_comi_load" 8
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "mode" "SF,DF,V4SF,V2DF")
				   (and (eq_attr "prefix" "!vex")
					(and (eq_attr "prefix_extra" "0")
					     (and (eq_attr "type" "ssecomi")
						  (eq_attr "memory" "load"))))))
			 "znver1-direct,znver1-load,znver1-fp0|znver1-fp1")

(define_insn_reservation "znver1_sse_comi_double" 2
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "mode" "V4SF,V2DF,TI")
				   (and (eq_attr "prefix" "vex")
					(and (eq_attr "prefix_extra" "0")
					     (and (eq_attr "type" "ssecomi")
						  (eq_attr "memory" "none"))))))
			 "znver1-double,znver1-fp0|znver1-fp1")

(define_insn_reservation "znver1_sse_comi_double_load" 10
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "mode" "V4SF,V2DF,TI")
				   (and (eq_attr "prefix" "vex")
					(and (eq_attr "prefix_extra" "0")
					     (and (eq_attr "type" "ssecomi")
						  (eq_attr "memory" "load"))))))
			 "znver1-double,znver1-load,znver1-fp0|znver1-fp1")

(define_insn_reservation "znver1_sse_test" 1
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "mode" "SF,DF,V4SF,V2DF,TI")
				   (and (eq_attr "prefix_extra" "1")
					(and (eq_attr "type" "ssecomi")
					     (eq_attr "memory" "none")))))
			 "znver1-direct,znver1-fp1|znver1-fp2")

(define_insn_reservation "znver1_sse_test_load" 8
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "mode" "SF,DF,V4SF,V2DF,TI")
				   (and (eq_attr "prefix_extra" "1")
					(and (eq_attr "type" "ssecomi")
					     (eq_attr "memory" "load")))))
			 "znver1-direct,znver1-load,znver1-fp1|znver1-fp2")

;; SSE moves
;; Fix me:  Need to revist this again some of the moves may be restricted
;; to some fpu pipes.
(define_insn_reservation "znver1_sse_mov" 2
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "mode" "SI")
				   (and (eq_attr "isa" "avx")
					(and (eq_attr "type" "ssemov")
					     (eq_attr "memory" "none")))))
			 "znver1-direct,znver1-ieu0")

(define_insn_reservation "znver1_avx_mov" 2
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "mode" "TI")
				   (and (eq_attr "isa" "avx")
					(and (eq_attr "type" "ssemov")
					     (and (match_operand:SI 1 "register_operand")
						  (eq_attr "memory" "none"))))))
			 "znver1-direct,znver1-ieu2")

(define_insn_reservation "znver1_sseavx_mov" 1
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "mode" "SF,DF,V4SF,V2DF,TI")
				   (and (eq_attr "type" "ssemov")
					(eq_attr "memory" "none"))))
			 "znver1-direct,znver1-fpu")

(define_insn_reservation "znver1_sseavx_mov_store" 1
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "mode" "SF,DF,V4SF,V2DF,TI")
				   (and (eq_attr "type" "ssemov")
					(eq_attr "memory" "store"))))
			"znver1-direct,znver1-fpu,znver1-store")

(define_insn_reservation "znver1_sseavx_mov_load" 8
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "mode" "SF,DF,V4SF,V2DF,TI")
				   (and (eq_attr "type" "ssemov")
					(eq_attr "memory" "load"))))
			 "znver1-direct,znver1-load,znver1-fpu")

(define_insn_reservation "znver1_avx256_mov" 1
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "mode" "V8SF,V4DF,OI")
				   (and (eq_attr "type" "ssemov")
					(eq_attr "memory" "none"))))
			 "znver1-double,znver1-fpu")

(define_insn_reservation "znver1_avx256_mov_store" 1
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "mode" "V8SF,V4DF,OI")
				   (and (eq_attr "type" "ssemov")
					(eq_attr "memory" "store"))))
			 "znver1-double,znver1-fpu,znver1-store")

(define_insn_reservation "znver1_avx256_mov_load" 8
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "mode" "V8SF,V4DF,OI")
				   (and (eq_attr "type" "ssemov")
					(eq_attr "memory" "load"))))
			 "znver1-double,znver1-load,znver1-fpu")

;; SSE add
(define_insn_reservation "znver1_sseavx_add" 3
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "mode" "SF,DF,V4SF,V2DF,TI")
				   (and (eq_attr "type" "sseadd")
					(eq_attr "memory" "none"))))
			 "znver1-direct,znver1-fp2|znver1-fp3")

(define_insn_reservation "znver1_sseavx_add_load" 10
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "mode" "SF,DF,V4SF,V2DF,TI")
				   (and (eq_attr "type" "sseadd")
					(eq_attr "memory" "load"))))
			 "znver1-direct,znver1-load,znver1-fp2|znver1-fp3")

(define_insn_reservation "znver1_avx256_add" 3
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "mode" "V8SF,V4DF,OI")
				   (and (eq_attr "type" "sseadd")
					(eq_attr "memory" "none"))))
			 "znver1-double,znver1-fp2|znver1-fp3")

(define_insn_reservation "znver1_avx256_add_load" 10
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "mode" "V8SF,V4DF,OI")
				   (and (eq_attr "type" "sseadd")
					(eq_attr "memory" "load"))))
			 "znver1-double,znver1-load,znver1-fp2|znver1-fp3")

(define_insn_reservation "znver1_sseavx_fma" 5
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "mode" "SF,DF,V4SF,V2DF")
				   (and (eq_attr "type" "ssemuladd")
					(eq_attr "memory" "none"))))
			 "znver1-direct,znver1-fp0|znver1-fp1")

(define_insn_reservation "znver1_sseavx_fma_load" 12
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "mode" "SF,DF,V4SF,V2DF")
				   (and (eq_attr "type" "ssemuladd")
					(eq_attr "memory" "load"))))
			"znver1-direct,znver1-load,znver1-fp0|znver1-fp1")

(define_insn_reservation "znver1_avx256_fma" 5
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "mode" "V8SF,V4DF")
				   (and (eq_attr "type" "ssemuladd")
					(eq_attr "memory" "none"))))
			 "znver1-double,znver1-fp0|znver1-fp1")

(define_insn_reservation "znver1_avx256_fma_load" 12
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "mode" "V8SF,V4DF")
				   (and (eq_attr "type" "ssemuladd")
					(eq_attr "memory" "load"))))
			 "znver1-double,znver1-load,znver1-fp0|znver1-fp1")

(define_insn_reservation "znver1_sseavx_iadd" 1
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "mode" "DI,TI")
				   (and (eq_attr "type" "sseiadd")
					(eq_attr "memory" "none"))))
			 "znver1-direct,znver1-fp0|znver1-fp1|znver1-fp3")

(define_insn_reservation "znver1_sseavx_iadd_load" 8
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "mode" "DI,TI")
				   (and (eq_attr "type" "sseiadd")
					(eq_attr "memory" "load"))))
			 "znver1-direct,znver1-load,znver1-fp0|znver1-fp1|znver1-fp3")

(define_insn_reservation "znver1_avx256_iadd" 1
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "mode" "OI")
				   (and (eq_attr "type" "sseiadd")
					(eq_attr "memory" "none"))))
			 "znver1-double,znver1-fp0|znver1-fp1|znver1-fp3")

(define_insn_reservation "znver1_avx256_iadd_load" 8
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "mode" "OI")
				   (and (eq_attr "type" "sseiadd")
					(eq_attr "memory" "load"))))
			 "znver1-double,znver1-load,znver1-fp0|znver1-fp1|znver1-fp3")

;; SSE conversions.
(define_insn_reservation "znver1_ssecvtsf_si_load" 12
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "mode" "SI")
				   (and (eq_attr "type" "sseicvt")
					(and (match_operand:SF 1 "memory_operand")
					     (eq_attr "memory" "load")))))
			 "znver1-double,znver1-load,znver1-fp3,znver1-ieu0")

(define_insn_reservation "znver1_ssecvtdf_si" 5
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "mode" "SI")
				   (and (match_operand:DF 1 "register_operand")
					(and (eq_attr "type" "sseicvt")
					     (eq_attr "memory" "none")))))
			 "znver1-double,znver1-fp3,znver1-ieu0")

(define_insn_reservation "znver1_ssecvtdf_si_load" 12
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "mode" "SI")
				   (and (eq_attr "type" "sseicvt")
					(and (match_operand:DF 1 "memory_operand")
					     (eq_attr "memory" "load")))))
			 "znver1-double,znver1-load,znver1-fp3,znver1-ieu0")

;; All other used ssecvt fp3 pipes
;; Check: Need to revisit this again.
;; Some SSE converts may use different pipe combinations.
(define_insn_reservation "znver1_ssecvt" 4
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "type" "ssecvt")
				   (eq_attr "memory" "none")))
			 "znver1-direct,znver1-fp3")

(define_insn_reservation "znver1_ssecvt_load" 11
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "type" "ssecvt")
				   (eq_attr "memory" "load")))
			 "znver1-direct,znver1-load,znver1-fp3")

;; SSE div
(define_insn_reservation "znver1_ssediv_ss_ps" 10
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "mode" "V4SF,SF")
				   (and (eq_attr "type" "ssediv")
					(eq_attr "memory" "none"))))
			 "znver1-direct,znver1-fp3*10")

(define_insn_reservation "znver1_ssediv_ss_ps_load" 17
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "mode" "V4SF,SF")
				   (and (eq_attr "type" "ssediv")
			 		(eq_attr "memory" "load"))))
			 "znver1-direct,znver1-load,znver1-fp3*10")

(define_insn_reservation "znver1_ssediv_sd_pd" 13
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "mode" "V2DF,DF")
				   (and (eq_attr "type" "ssediv")
					(eq_attr "memory" "none"))))
			 "znver1-direct,znver1-fp3*13")

(define_insn_reservation "znver1_ssediv_sd_pd_load" 20
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "mode" "V2DF,DF")
				   (and (eq_attr "type" "ssediv")
					(eq_attr "memory" "load"))))
			 "znver1-direct,znver1-load,znver1-fp3*13")

(define_insn_reservation "znver1_ssediv_avx256_ps" 12
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "mode" "V8SF")
				   (and (eq_attr "memory" "none")
					(eq_attr "type" "ssediv"))))
			 "znver1-double,znver1-fp3*12")

(define_insn_reservation "znver1_ssediv_avx256_ps_load" 19
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "mode" "V8SF")
				   (and (eq_attr "type" "ssediv")
					(eq_attr "memory" "load"))))
			 "znver1-double,znver1-load,znver1-fp3*12")

(define_insn_reservation "znver1_ssediv_avx256_pd" 15
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "mode" "V4DF")
				   (and (eq_attr "type" "ssediv")
					(eq_attr "memory" "none"))))
			 "znver1-double,znver1-fp3*15")

(define_insn_reservation "znver1_ssediv_avx256_pd_load" 22 
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "mode" "V4DF")
				   (and (eq_attr "type" "ssediv")
					(eq_attr "memory" "load"))))
			 "znver1-double,znver1-load,znver1-fp3*15")
;; SSE MUL
(define_insn_reservation "znver1_ssemul_ss_ps" 3
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "mode" "V4SF,SF")
				   (and (eq_attr "type" "ssemul")
					(eq_attr "memory" "none"))))
			 "znver1-direct,(znver1-fp0|znver1-fp1)*3")

(define_insn_reservation "znver1_ssemul_ss_ps_load" 10 
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "mode" "V4SF,SF")
				   (and (eq_attr "type" "ssemul")
					(eq_attr "memory" "load"))))
			 "znver1-direct,znver1-load,(znver1-fp0|znver1-fp1)*3")

(define_insn_reservation "znver1_ssemul_avx256_ps" 3
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "mode" "V8SF")
				   (and (eq_attr "type" "ssemul")
					(eq_attr "memory" "none"))))
			 "znver1-double,(znver1-fp0|znver1-fp1)*3")

(define_insn_reservation "znver1_ssemul_avx256_ps_load" 10
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "mode" "V8SF")
				   (and (eq_attr "type" "ssemul")
					(eq_attr "memory" "load"))))
			 "znver1-double,znver1-load,(znver1-fp0|znver1-fp1)*3")

(define_insn_reservation "znver1_ssemul_sd_pd" 4
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "mode" "V2DF,DF")
				   (and (eq_attr "type" "ssemul")
					(eq_attr "memory" "none"))))
			 "znver1-direct,(znver1-fp0|znver1-fp1)*4")

(define_insn_reservation "znver1_ssemul_sd_pd_load" 11
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "mode" "V2DF,DF")
				   (and (eq_attr "type" "ssemul")
					(eq_attr "memory" "load"))))
			 "znver1-direct,znver1-load,(znver1-fp0|znver1-fp1)*4")

(define_insn_reservation "znver1_ssemul_avx256_pd" 5
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "mode" "V4DF")
				   (and (eq_attr "mode" "V4DF")
					(and (eq_attr "type" "ssemul")
					     (eq_attr "memory" "none")))))
			 "znver1-double,(znver1-fp0|znver1-fp1)*4")

(define_insn_reservation "znver1_ssemul_avx256_pd_load" 12
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "mode" "V4DF")
				   (and (eq_attr "type" "ssemul")
					(eq_attr "memory" "load"))))
			 "znver1-double,znver1-load,(znver1-fp0|znver1-fp1)*4")

;;SSE imul
(define_insn_reservation "znver1_sseimul" 3
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "mode" "TI")
				   (and (eq_attr "type" "sseimul")
					(eq_attr "memory" "none"))))
			 "znver1-direct,znver1-fp0*3")

(define_insn_reservation "znver1_sseimul_avx256" 4
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "mode" "OI")
				   (and (eq_attr "type" "sseimul")
					(eq_attr "memory" "none"))))
			 "znver1-double,znver1-fp0*4")

(define_insn_reservation "znver1_sseimul_load" 10
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "mode" "TI")
				   (and (eq_attr "type" "sseimul")
					(eq_attr "memory" "load"))))
			 "znver1-direct,znver1-load,znver1-fp0*3")

(define_insn_reservation "znver1_sseimul_avx256_load" 11
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "mode" "OI")
				   (and (eq_attr "type" "sseimul")
					(eq_attr "memory" "load"))))
			 "znver1-double,znver1-load,znver1-fp0*4")

(define_insn_reservation "znver1_sseimul_di" 3 
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "mode" "DI")
				   (and (eq_attr "memory" "none")
					(eq_attr "type" "sseimul"))))
			 "znver1-direct,znver1-fp0*3")

(define_insn_reservation "znver1_sseimul_load_di" 10 
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "mode" "DI")
				   (and (eq_attr "type" "sseimul")
					(eq_attr "memory" "load"))))
			 "znver1-direct,znver1-load,znver1-fp0*3")

;; SSE compares
(define_insn_reservation "znver1_sse_cmp" 1
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "mode" "SF,DF,V4SF,V2DF")
				   (and (eq_attr "type" "ssecmp")
					(eq_attr "memory" "none"))))
			 "znver1-direct,znver1-fp0|znver1-fp1")

(define_insn_reservation "znver1_sse_cmp_load" 8
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "mode" "SF,DF,V4SF,V2DF")
				   (and (eq_attr "type" "ssecmp")
					(eq_attr "memory" "load"))))
			 "znver1-direct,znver1-load,znver1-fp0|znver1-fp1")

(define_insn_reservation "znver1_sse_cmp_avx256" 1
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "mode" "V8SF,V4DF")
				   (and (eq_attr "type" "ssecmp")
					(eq_attr "memory" "none"))))
			"znver1-double,znver1-fp0|znver1-fp1")

(define_insn_reservation "znver1_sse_cmp_avx256_load" 8
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "mode" "V8SF,V4DF")
				   (and (eq_attr "type" "ssecmp")
					(eq_attr "memory" "load"))))
			 "znver1-double,znver1-load,znver1-fp0|znver1-fp1")

(define_insn_reservation "znver1_sse_icmp" 1
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "mode" "QI,HI,SI,DI,TI")
				   (and (eq_attr "type" "ssecmp")
					(eq_attr "memory" "none"))))
			 "znver1-direct,znver1-fp0|znver1-fp3")

(define_insn_reservation "znver1_sse_icmp_load" 8
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "mode" "QI,HI,SI,DI,TI")
				   (and (eq_attr "type" "ssecmp")
					(eq_attr "memory" "load"))))
			 "znver1-direct,znver1-load,znver1-fp0|znver1-fp3")

(define_insn_reservation "znver1_sse_icmp_avx256" 1
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "mode" "OI")
				   (and (eq_attr "type" "ssecmp")
					(eq_attr "memory" "none"))))
			 "znver1-double,znver1-fp0|znver1-fp3")

(define_insn_reservation "znver1_sse_icmp_avx256_load" 8
			 (and (eq_attr "cpu" "znver1")
			      (and (eq_attr "mode" "OI")
				   (and (eq_attr "type" "ssecmp")
					(eq_attr "memory" "load"))))
			 "znver1-double,znver1-load,znver1-fp0|znver1-fp3")
