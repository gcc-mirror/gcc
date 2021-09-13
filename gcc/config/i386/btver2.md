;; Copyright (C) 2012-2021 Free Software Foundation, Inc.
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

;; AMD btver2 scheduling

;; Instructions decoded are that are classifed as direct (fast path single),
;; double (fast path double) and vector instructions.
;; Direct instrucions are decoded and convereted into 1 cop
;; Double instrucions are decoded and converetd into 2 cops
;; Vector instrucions are microcoded and they generated converted to 
;; 3 or more cops.

(define_attr "btver2_decode" "direct,vector,double"
  (const_string "direct"))

(define_attr "btver2_sse_attr" "other,rcp,sqrt,maxmin"
  (const_string "other"))

(define_automaton "btver2,btver2_int,btver2_agu,btver2_fp")

;; Decoder decodes up to two insns (2 fastpath singles) or 
;;(2 fastpath doubles) or combination of both at a cycle.
;; In case of vector (microded) instruction decoder decodes only one insn
;; at a cycle .To model that we have 2 "decoder" units.

(define_cpu_unit "btver2-decode0" "btver2")
(define_cpu_unit "btver2-decode1" "btver2")

;; "me" unit converts the decoded insn into cops.
;; It can generate upto 2 cops from two fast path singles in cycle x+1, 
;; to model we have two "mes". In case of fast path double it converts
;; them to 2 cops in cycle x+1. Vector instructions are modelled to block
;; all decoder units. 

(define_cpu_unit "me0" "btver2")
(define_cpu_unit "me1" "btver2")

(define_reservation "btver2-direct" "(btver2-decode0|btver2-decode1),(me0|me1)")

(define_reservation "btver2-double" "(btver2-decode0|btver2-decode1),(me0+me1)")

(define_reservation "btver2-vector" "(btver2-decode0+btver2-decode1),(me0+me1)")

;; Integer operations 
;; There are 2 ALU pipes 

(define_cpu_unit "btver2-ieu0" "btver2_int")
(define_cpu_unit "btver2-ieu1" "btver2_int")

;; There are 2 AGU pipes one for load and one for store.

(define_cpu_unit "btver2-load"  "btver2_agu")
(define_cpu_unit "btver2-store" "btver2_agu")

;; ALU operations can take place in ALU pipe0 or pipe1. 
(define_reservation "btver2-alu" "(btver2-ieu0|btver2-ieu1)")

;; MUL and DIV operations can take place in to ALU pipe1.
(define_reservation "btver2-mul" "btver2-ieu1")
(define_reservation "btver2-div" "btver2-ieu1")

;; vectorpath (microcoded) instructions are single issue instructions.
;; So, they occupy all the integer units.
(define_reservation "btver2-ivector" "btver2-ieu0+btver2-ieu1+
                                      btver2-load+btver2-store")

;;Floating point pipes.
(define_cpu_unit "btver2-fp0" "btver2_fp")
(define_cpu_unit "btver2-fp1" "btver2_fp")

(define_reservation "btver2-fpa" "btver2-fp0")
(define_reservation "btver2-vimul" "btver2-fp0")
(define_reservation "btver2-valu" "btver2-fp0|btver2-fp1")
(define_reservation "btver2-stc" "btver2-fp1")
(define_reservation "btver2-fpm" "btver2-fp1")

;; vectorpath (microcoded) instructions are single issue instructions.
;; So, they occupy all the fp units.
(define_reservation "btver2-fvector" "btver2-fp0+btver2-fp1+
                                      btver2-load+btver2-store")

;; Call instruction
(define_insn_reservation "btver2_call" 2
			 (and (eq_attr "cpu" "btver2")
			      (eq_attr "type" "call,callv"))
			 "btver2-double,btver2-load")

;; General instructions
;;

(define_insn_reservation "btver2_push_mem" 4
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "memory" "load")
				   (eq_attr "type" "push")))
			 "btver2-direct,btver2-load,btver2-alu")

(define_insn_reservation "btver2_push" 1
			 (and (eq_attr "cpu" "btver2")
			      (eq_attr "type" "push"))
			 "btver2-direct,btver2-alu")

(define_insn_reservation "btver2_pop_mem" 4
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "memory" "load")
				   (eq_attr "type" "pop")))
			 "btver2-direct,btver2-load,btver2-alu")

(define_insn_reservation "btver2_pop" 1
			 (and (eq_attr "cpu" "btver2")
			      (eq_attr "type" "pop"))
			 "btver2-direct,btver2-alu")

(define_insn_reservation "btver2_leave" 3
			 (and (eq_attr "cpu" "btver2")
			      (eq_attr "type" "leave"))
			 "btver2-double,btver2-alu")

(define_insn_reservation "btver2_lea" 1
			 (and (eq_attr "cpu" "btver2")
			      (eq_attr "type" "lea"))
			 "btver2-direct,btver2-alu")

;; Integer  
(define_insn_reservation "btver2_imul_DI" 6
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "type" "imul")
				   (and (eq_attr "mode" "DI")
					(eq_attr "memory" "none,unknown"))))
			 "btver2-direct,btver2-mul*4")

(define_insn_reservation "btver2_imul" 3
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "type" "imul")
				   (eq_attr "memory" "none,unknown")))
			 "btver2-direct,btver2-mul")

(define_insn_reservation "btver2_imul_mem_DI" 9
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "type" "imul")
				   (and (eq_attr "mode" "DI")
					(eq_attr "memory" "load,both"))))
			 "btver2-direct,btver2-load,btver2-mul*4")

(define_insn_reservation "btver2_imul_mem" 6
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "type" "imul")
				   (eq_attr "memory" "load,both")))
			 "btver2-direct,btver2-load,btver2-mul")

(define_insn_reservation "btver2_idiv_DI" 41
			    (and (eq_attr "cpu" "btver2")
				 (and (eq_attr "type" "idiv")
				      (and (eq_attr "mode" "DI")
					   (eq_attr "memory" "none,unknown"))))
			 "btver2-double,btver2-div")

(define_insn_reservation "btver2_idiv_mem_DI" 44
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "type" "idiv")
				   (and (eq_attr "mode" "DI")
					(eq_attr "memory" "load"))))
			 "btver2-double,btver2-load,btver2-div")

(define_insn_reservation "btver2_idiv_SI" 25
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "type" "idiv")
				   (and (eq_attr "mode" "SI")
					(eq_attr "memory" "none,unknown"))))
			 "btver2-double,btver2-div*25")

(define_insn_reservation "btver2_idiv_mem_SI" 28
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "type" "idiv")
				   (and (eq_attr "mode" "SI")
					(eq_attr "memory" "load"))))
			 "btver2-double,btver2-load,btver2-div*25")

(define_insn_reservation "btver2_idiv_HI" 17
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "type" "idiv")
				   (and (eq_attr "mode" "HI")
					(eq_attr "memory" "none,unknown"))))
			 "btver2-double,btver2-div*17")

(define_insn_reservation "btver2_idiv_mem_HI" 20
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "type" "idiv")
				   (and (eq_attr "mode" "HI")
					(eq_attr "memory" "load"))))
			 "btver2-double,btver2-load,btver2-div*17")

(define_insn_reservation "btver2_idiv_QI" 12
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "type" "idiv")
				   (and (eq_attr "mode" "SI")
					(eq_attr "memory" "none,unknown"))))
			 "btver2-direct,btver2-div*12")

(define_insn_reservation "btver2_idiv_mem_QI" 15
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "type" "idiv")
				   (and (eq_attr "mode" "SI")
					(eq_attr "memory" "load"))))
			 "btver2-direct,btver2-load,btver2-div*12")

(define_insn_reservation "btver2_str" 7
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "type" "str")
				   (eq_attr "memory" "load,both,store")))
			 "btver2-vector,btver2-ivector")

(define_insn_reservation "btver2_idirect_loadmov" 4
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "type" "imov")
				   (eq_attr "memory" "load")))
			 "btver2-direct,btver2-load,btver2-alu")

(define_insn_reservation "btver2_idirect_load" 4
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "bdver1_decode" "direct")
				   (and (eq_attr "unit" "integer,unknown")
					(eq_attr "memory" "load"))))
			 "btver2-direct,btver2-load,btver2-alu")

(define_insn_reservation "btver2_idirect_movstore" 4
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "type" "imov")
				   (eq_attr "memory" "store")))
			 "btver2-direct,btver2-alu,btver2-store")

(define_insn_reservation "btver2_idirect_both" 4
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "bdver1_decode" "direct")
				   (and (eq_attr "unit" "integer,unknown")
					(eq_attr "memory" "both"))))
			 "btver2-direct,btver2-load,btver2-alu,btver2-store")

(define_insn_reservation "btver2_idirect_store" 4
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "bdver1_decode" "direct")
				   (and (eq_attr "unit" "integer,unknown")
					(eq_attr "memory" "store"))))
			 "btver2-direct,btver2-alu,btver2-store")

;; Other integer instrucions 
(define_insn_reservation "btver2_idirect" 1
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "btver2_decode" "direct")
				   (and (eq_attr "unit" "integer,unknown")
					(eq_attr "memory" "none,unknown"))))
			 "btver2-direct,btver2-alu")

;; Floating point instructions 
(define_insn_reservation "btver2_fldxf" 19
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "type" "fmov")
				   (and (eq_attr "memory" "load")
					(eq_attr "mode" "XF"))))
			 "btver2-vector,btver2-load,btver2-fvector*5")

(define_insn_reservation "btver2_fld" 11
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "type" "fmov")
				   (eq_attr "memory" "load")))
			 "btver2-direct,btver2-load,(btver2-fp0|btver2-fp1)")

(define_insn_reservation "btver2_fstxf" 24
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "type" "fmov")
				   (and (eq_attr "memory" "both")
					(eq_attr "mode" "XF"))))
			 "btver2-vector,btver2-fvector*9,btver2-store")

(define_insn_reservation "btver2_fst" 11
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "type" "fmov")
				   (eq_attr "memory" "store,both")))
			 "btver2-direct,btver2-fp1,btver2-store")

(define_insn_reservation "btver2_fist" 9
			 (and (eq_attr "cpu" "btver2")
			      (eq_attr "type" "fistp,fisttp"))
			 "btver2-direct,btver2-load,btver2-fp1")

(define_insn_reservation "btver2_fmov" 2
			 (and (eq_attr "cpu" "btver2")
			      (eq_attr "type" "fmov"))
			 "btver2-direct,(btver2-fp0|btver2-fp1)")

(define_insn_reservation "btver2_fadd_load" 8
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "type" "fop")
				   (eq_attr "memory" "load")))
			 "btver2-direct,btver2-load,btver2-fp0")

(define_insn_reservation "btver2_fadd" 3
			 (and (eq_attr "cpu" "btver2")
			      (eq_attr "type" "fop"))
			 "btver2-direct,btver2-fp0")

(define_insn_reservation "btver2_fmul_load" 10
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "type" "fmul")
				   (eq_attr "memory" "load")))
			 "btver2-direct,btver2-load,btver2-fp1*3")

(define_insn_reservation "btver2_fmul" 5
			 (and (eq_attr "cpu" "btver2")
			      (eq_attr "type" "fmul"))
			 "btver2-direct,(btver2-fp1*3)")

(define_insn_reservation "btver2_fsgn" 2
			 (and (eq_attr "cpu" "btver2")
			      (eq_attr "type" "fsgn"))
			 "btver2-direct,btver2-fp1*2")

(define_insn_reservation "btver2_fdiv_load" 24
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "type" "fdiv")
				   (eq_attr "memory" "load")))
			 "btver2-direct,btver2-load,btver2-fp1*19")

(define_insn_reservation "btver2_fdiv" 19
			 (and (eq_attr "cpu" "btver2")
			      (eq_attr "type" "fdiv"))
			 "btver2-direct,btver2-fp1*19")

(define_insn_reservation "btver2_fcmov_load" 12
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "type" "fcmov")
				   (eq_attr "memory" "load")))
			 "btver2-vector,btver2-load,(btver2-fp0|btver2-fp1)*7")

(define_insn_reservation "btver2_fcmov" 7
			 (and (eq_attr "cpu" "btver2")
			      (eq_attr "type" "fcmov"))
			 "btver2-vector,(btver2-fp0|btver2-fp1)*7")

(define_insn_reservation "btver2_fcomi_load" 7
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "type" "fcmp")
				   (and (eq_attr "bdver1_decode" "double")
					(eq_attr "memory" "load"))))
			 "btver2-direct,btver2-load,btver2-fp0*2")

(define_insn_reservation "btver2_fcomi" 2
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "bdver1_decode" "double")
				   (eq_attr "type" "fcmp")))
			 "btver2-direct, btver2-fp0*2")

(define_insn_reservation "btver2_fcom_load" 6
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "type" "fcmp")
				   (eq_attr "memory" "load")))
			 "btver2-direct,btver2-load,btver2-fp0")

(define_insn_reservation "btver2_fcom" 1
			 (and (eq_attr "cpu" "btver2")
			      (eq_attr "type" "fcmp"))
			  "btver2-direct,btver2-fp0")

(define_insn_reservation "btver2_fxch" 1
			 (and (eq_attr "cpu" "btver2")
			      (eq_attr "type" "fxch"))
			 "btver2-direct,btver2-fp1")

;; SSE AVX maxmin,rcp,sqrt
(define_insn_reservation "btver2_sse_maxmin" 2
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "V8SF,V4DF,V2DF,V4SF,SF,DF")
				   (and (eq_attr "memory" "none,unknown")
					(and (eq_attr "btver2_sse_attr" "maxmin")
					     (eq_attr "type" "sse,sseadd")))))
			 "btver2-direct,btver2-fpa")

(define_insn_reservation "btver2_sse_maxmin_mem" 7 
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "V8SF,V4DF,V2DF,V4SF,SF,DF")
				   (and (eq_attr "memory" "load")
					(and (eq_attr "btver2_sse_attr" "maxmin")
					     (eq_attr "type" "sse,sseadd")))))
			 "btver2-direct,btver2-load,btver2-fpa")

(define_insn_reservation "btver2_sse_rcp" 2
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "V4SF,SF")
				   (and (eq_attr "memory" "none,unknown")
					(and (eq_attr "btver2_sse_attr" "rcp")
					     (eq_attr "type" "sse")))))
			 "btver2-direct,btver2-fpm")

(define_insn_reservation "btver2_sse_rcp_mem" 7
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "V4SF,SF")
				   (and (eq_attr "memory" "load")
					(and (eq_attr "btver2_sse_attr" "rcp")
					     (eq_attr "type" "sse")))))
			 "btver2-direct,btver2-load,btver2-fpm")

(define_insn_reservation "btver2_avx_rcp" 2
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "V8SF")
				   (and (eq_attr "memory" "none,unknown")
					(and (eq_attr "btver2_sse_attr" "rcp")
					     (eq_attr "type" "sse")))))
			 "btver2-double,btver2-fpm*2")

(define_insn_reservation "btver2_avx_rcp_mem" 7 
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "V8SF")
				   (and (eq_attr "memory" "load")
					(and (eq_attr "btver2_sse_attr" "rcp")
					     (eq_attr "type" "sse")))))
			 "btver2-double,btver2-load,btver2-fpm*2")

(define_insn_reservation "btver2_sse_sqrt_v4sf" 21
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "V4SF")
				   (and (eq_attr "memory" "none,unknown")
					(and (eq_attr "btver2_sse_attr" "sqrt")
					     (eq_attr "type" "sse")))))
			 "btver2-direct,btver2-fpm*21")

(define_insn_reservation "btver2_sse_sqrt_v4sf_mem" 26
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "V4SF")
				   (and (eq_attr "memory" "load")
					(and (eq_attr "btver2_sse_attr" "sqrt")
					     (eq_attr "type" "sse")))))
			 "btver2-direct,btver2-load,btver2-fpm*21")

(define_insn_reservation "btver2_sse_sqrt_v4df" 54
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "V4DF")
				   (and (eq_attr "memory" "none,unknown")
					(and (eq_attr "btver2_sse_attr" "sqrt")
					     (eq_attr "type" "sse")))))
			 "btver2-double,btver2-fpm*54")

(define_insn_reservation "btver2_sse_sqrt_v4df_mem" 59
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "V4DF")
				   (and (eq_attr "memory" "load")
					(and (eq_attr "btver2_sse_attr" "sqrt")
					     (eq_attr "type" "sse")))))
			 "btver2-double,btver2-load,btver2-fpm*54")

(define_insn_reservation "btver2_sse_sqrt_sf" 16
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "SF")
				   (and (eq_attr "memory" "none,unknown")
					(and (eq_attr "btver2_sse_attr" "sqrt")
					     (eq_attr "type" "sse")))))
			 "btver2-direct,btver2-fpm*16")

(define_insn_reservation "btver2_sse_sqrt_sf_mem" 21
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "SF")
				   (and (eq_attr "memory" "load")
					(and (eq_attr "btver2_sse_attr" "sqrt")
					     (eq_attr "type" "sse")))))
			 "btver2-direct,btver2-load,btver2-fpm*16")

(define_insn_reservation "btver2_sse_sqrt_df" 27
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "V2DF,DF")
				   (and (eq_attr "memory" "none,unknown")
					(and (eq_attr "btver2_sse_attr" "sqrt")
					     (eq_attr "type" "sse")))))
			 "btver2-direct,btver2-fpm*27")

(define_insn_reservation "btver2_sse_sqrt_df_mem" 32
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "V2DF,DF")
				   (and (eq_attr "memory" "load")
					(and (eq_attr "btver2_sse_attr" "sqrt")
					     (eq_attr "type" "sse")))))
			 "btver2-direct,btver2-load,btver2-fpm*27")

(define_insn_reservation "btver2_sse_sqrt_v8sf" 42
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "V8SF")
				   (and (eq_attr "memory" "none,unknown")
					(and (eq_attr "btver2_sse_attr" "sqrt")
					     (eq_attr "type" "sse")))))
			 "btver2-double,btver2-fpm*42")

(define_insn_reservation "btver2_sse_sqrt_v8sf_mem" 42
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "V8SF")
				   (and (eq_attr "memory" "load")
					(and (eq_attr "btver2_sse_attr" "sqrt")
					     (eq_attr "type" "sse")))))
			 "btver2-double,btver2-load,btver2-fpm*42")

;; Bitmanipulation instrucions BMI LZCNT POPCNT 
(define_insn_reservation "btver2_bmi_reg_direct"   1
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "btver2_decode" "direct")
				   (and (eq_attr "memory" "none")
					(eq_attr "type" "bitmanip"))))
			 "btver2-direct,btver2-alu")

(define_insn_reservation "btver2_bmi_mem_direct" 4
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "btver2_decode" "direct")
				   (and (eq_attr "memory" "load")
					(eq_attr "type" "bitmanip"))))
			 "btver2-direct,btver2-load,btver2-alu")

(define_insn_reservation "btver2_bmi_reg_double"  2
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "btver2_decode" "double")
				   (and (eq_attr "memory" "none")
					(eq_attr "type" "bitmanip,alu1"))))
			 "btver2-double,btver2-alu")

(define_insn_reservation "btver2_bmi_double_store"  5
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "memory" "store")
				   (and (eq_attr "btver2_decode" "double")
					(eq_attr "type" "bitmanip,alu1"))))
			 "btver2-double,btver2-alu,btver2-store")

(define_insn_reservation "btver2_bmi_double_load" 4 
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "btver2_decode" "double")
				   (and (eq_attr "memory" "load")
					(eq_attr "type" "bitmanip,alu1"))))
			 "btver2-double,btver2-load,btver2-alu")

;; F16C converts
(define_insn_reservation "btver2_ssecvt_load_direct" 8
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "V8SF,V4SF")
				   (and (eq_attr "memory" "load")
					(and (eq_attr "btver2_decode" "direct")
					     (eq_attr "type" "ssecvt")))))
			 "btver2-direct,btver2-load,btver2-stc")

(define_insn_reservation "btver2_ssecvt_store_direct" 8
			 (and (eq_attr "cpu" "btver2")
			     (and (eq_attr "mode" "V8SF,V4SF")
				  (and (eq_attr "memory" "store")
					(and (eq_attr "btver2_decode" "direct")
					     (eq_attr "type" "ssecvt")))))
			 "btver2-direct,btver2-stc,btver2-store")

(define_insn_reservation "btver2_ssecvt_reg_direct" 3
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "V8SF,V4SF")
				   (and (eq_attr "btver2_decode" "direct")
					(eq_attr "type" "ssecvt"))))
			 "btver2-direct,btver2-stc")

(define_insn_reservation "btver2_ssecvt_load_double" 8
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "V8SF,V4SF")
				  (and (eq_attr "memory" "load")
					(and (eq_attr "btver2_decode" "double")
					     (eq_attr "type" "ssecvt")))))
			 "btver2-double,btver2-load,btver2-stc*2")

(define_insn_reservation "btver2_ssecvt_reg_double" 3
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "V8SF,V4SF")
				   (and (eq_attr "btver2_decode" "double")
					(eq_attr "type" "ssecvt"))))
			 "btver2-double,btver2-stc*2")

(define_insn_reservation "btver2_ssecvt_store_vector" 11
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "V8SF,V4SF")
				   (and (eq_attr "memory" "store")
					(and (eq_attr "btver2_decode" "vector")
					     (eq_attr "type" "ssecvt")))))
			 "btver2-vector,btver2-stc,(btver2-fpa|btver2-fpm),btver2-store")

(define_insn_reservation "btver2_ssecvt_reg_vector" 6
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "V8SF,V4SF")
				   (and (eq_attr "btver2_decode" "vector")
					(eq_attr "type" "ssecvt"))))
			 "btver2-vector,btver2-stc,(btver2-fpa|btver2-fpm)")

;; avx256 adds
(define_insn_reservation "btver2_avx_add_load_256" 8
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "V4DF,V8SF")
				   (and (eq_attr "memory" "load")
					(eq_attr "type" "sseadd,sseadd1"))))
			 "btver2-double,btver2-load,btver2-fpa")

(define_insn_reservation "btver2_avx_add_reg_256" 3
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "V4DF,V8SF")
				   (and (eq_attr "memory" "none,unknown")
					(eq_attr "type" "sseadd,sseadd1"))))
			 "btver2-double,btver2-fpa")

;; avx256 logs 
(define_insn_reservation "btver2_avx_load_log" 6
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "V4DF,V8SF")
				   (and (eq_attr "memory" "load")
					(and (eq_attr "btver2_decode" "!vector")
					     (eq_attr "type" "sselog,sselog1")))))
			 "btver2-double,btver2-load,(btver2-fpa|btver2-fpm)")

(define_insn_reservation "btver2_avx_reg_log" 1
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "V4DF,V8SF")
				   (and (eq_attr "memory" "none,unknown")
					(and (eq_attr "btver2_decode" "!vector")
					     (eq_attr "type" "sselog,sselog1")))))
			 "btver2-double,(btver2-fpa|btver2-fpm)")

;; avx256 sse

(define_insn_reservation "btver2_avx_load_sse" 6
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "V4DF,V8SF")
				   (and (eq_attr "memory" "load")
					(and (eq_attr "btver2_decode" "!vector")
					     (eq_attr "type" "sse")))))
			 "btver2-double,btver2-load,(btver2-fpa|btver2-fpm)")

(define_insn_reservation "btver2_avx_reg_sse" 1
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "V4DF,V8SF")
				   (and (eq_attr "memory" "none,unknown")
					(and (eq_attr "btver2_decode" "!vector")
					     (eq_attr "type" "sse")))))
			 "btver2-double,(btver2-fpa|btver2-fpm)")

;; avx256 moves
(define_insn_reservation "btver2_avx_load_int_mov" 6
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "OI")
				   (and (eq_attr "memory" "load")
					(eq_attr "type" "ssemov"))))
			 "btver2-double,btver2-load,btver2-valu")

(define_insn_reservation "btver2_avx_store_int_mov" 6
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "OI")
				   (and (eq_attr "memory" "store")
					(eq_attr "type" "ssemov"))))
			 "btver2-double,btver2-valu,btver2-store")

(define_insn_reservation "btver2_avx_int_mov" 1 
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "OI")
				   (and (eq_attr "memory" "none,unknown")
					(eq_attr "type" "ssemov"))))
			 "btver2-double,btver2-valu")

(define_insn_reservation "btver2_avx_load_from_vectors" 6
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "V8SF,V4DF")
				   (and (ior ( match_operand:V4SF 1 "memory_operand")
					     ( match_operand:V2DF 1 "memory_operand"))
					(and (eq_attr "memory" "load")
					     (eq_attr "type" "ssemov")))))
			 "btver2-double,btver2-load,(btver2-fpa|btver2-fpm)")

(define_insn_reservation "btver2_avx_loads_from_scalar" 6
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "V8SF,V4DF")
				   (and (ior ( match_operand:SF 1 "memory_operand")
					     ( match_operand:DF 1 "memory_operand"))
					(and (eq_attr "memory" "load")
					     (eq_attr "type" "ssemov")))))
			 "btver2-double,btver2-load,(btver2-fpa|btver2-fpm)*2")

(define_insn_reservation "btver2_avx_store_move" 6
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "V4DF,V8SF")
				   (and (eq_attr "memory" "store")
					(and (eq_attr "btver2_decode" "!vector")
					     (eq_attr "type" "ssemov")))))
			 "btver2-double,(btver2-fpa|btver2-fpm),btver2-store")

(define_insn_reservation "btver2_avx_load_move" 6
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "V4DF,V8SF")
				   (and (eq_attr "memory" "load")
					(and (eq_attr "btver2_decode" "!vector")
					     (eq_attr "type" "ssemov")))))
			 "btver2-double,btver2-load,(btver2-fpa|btver2-fpm)")

(define_insn_reservation "btver2_avx_reg_move" 1
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "V4DF,V8SF")
				   (and (eq_attr "memory" "none,unknown")
					(and (eq_attr "btver2_decode" "!vector")
					     (eq_attr "type" "ssemov")))))
			 "btver2-double,(btver2-fpa|btver2-fpm)")
;; avx256 cmps
(define_insn_reservation "btver2_avx_load_cmp" 7
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "V4DF,V8SF")
				   (and (eq_attr "memory" "load")
					(eq_attr "type" "ssecmp"))))
			 "btver2-double,btver2-load,(btver2-fpa|btver2-fpm)*2")

(define_insn_reservation "btver2_avx_cmp" 2
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "V4DF,V8SF")
				   (and (eq_attr "memory" "none,unknown")
					(eq_attr "type" "ssecmp"))))
			 "btver2-double,(btver2-fpa|btver2-fpm)*2")

;; ssecvts 256 
(define_insn_reservation "btver2_ssecvt_256_load" 8
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "V4DF,OI")
				   (and (eq_attr "memory" "none,unknown")
					(and (eq_attr "btver2_decode" "!vector")
					     (eq_attr "type" "ssecvt")))))
			 "btver2-double,btver2-load,btver2-stc*2")

(define_insn_reservation "btver2_ssecvt_256" 3 
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "V4DF,OI")
				   (and (eq_attr "memory" "none,unknown")
					(and (eq_attr "btver2_decode" "!vector")
					     (eq_attr "type" "ssecvt")))))
			 "btver2-double,btver2-stc*2")

(define_insn_reservation "btver2_ssecvt_256_vector_load" 11
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "V4DF,OI")
				   (and (eq_attr "memory" "none,unknown")
					(and (eq_attr "btver2_decode" "vector")
					     (eq_attr "type" "ssecvt")))))
			 "btver2-vector,btver2-load,btver2-stc*2,(btver2-fpa|btver2-fpm)")

(define_insn_reservation "btver2_ssecvt_256_vector" 6
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "V4DF,OI")
				   (and (eq_attr "memory" "none,unknown")
					(and (eq_attr "btver2_decode" "vector")
					     (eq_attr "type" "ssecvt")))))
			 "btver2-vector,btver2-stc*2,(btver2-fpa|btver2-fpm)")

;; avx256 divides
(define_insn_reservation "btver2_avx_load_div" 43
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "V4DF,V8SF")
				   (and (eq_attr "memory" "load")
					(and (eq_attr "btver2_decode" "!vector")
					     (eq_attr "type" "ssediv")))))
			 "btver2-double,btver2-load,btver2-fpm*38")

(define_insn_reservation "btver2_avx_div" 38
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "V4DF,V8SF")
				   (and (eq_attr "memory" "none,unknown")
					(and (eq_attr "btver2_decode" "!vector")
					     (eq_attr "type" "ssediv")))))
			 "btver2-double,btver2-fpm*38")

;; avx256  multiply

(define_insn_reservation "btver2_avx_mul_load_pd" 9
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "V4DF")
				   (and (eq_attr "memory" "load")
					(and (eq_attr "btver2_decode" "!vector")
					     (eq_attr "type" "ssemul")))))
			"btver2-double,btver2-load,btver2-fpm*4")

(define_insn_reservation "btver2_avx_mul_load_ps" 7
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "V8SF")
				   (and (eq_attr "memory" "load")
					(and (eq_attr "btver2_decode" "!vector")
					     (eq_attr "type" "ssemul")))))
			 "btver2-double,btver2-load,btver2-fpm*2")


(define_insn_reservation "btver2_avx_mul_256_pd" 4
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "V4DF")
				   (and (eq_attr "memory" "none,unknown")
					(and (eq_attr "btver2_decode" "!vector")
					     (eq_attr "type" "ssemul")))))
			 "btver2-double,btver2-fpm*4")

(define_insn_reservation "btver2_avx_mul_256_ps" 2	
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "V8SF")
				   (and (eq_attr "memory" "none,unknown")
					(and (eq_attr "btver2_decode" "!vector")
					     (eq_attr "type" "ssemul")))))
			 "btver2-double,btver2-fpm*2")

(define_insn_reservation "btver2_avx_dpps_load_ps" 17
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "V8SF")
				   (and (eq_attr "memory" "load")
					(and (eq_attr "btver2_decode" "vector")
					     (eq_attr "type" "ssemul")))))
			 "btver2-vector,btver2-fpm*6,btver2-fpa*6")

(define_insn_reservation "btver2_avx_dpps_ps" 12
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "V8SF")
				   (and (eq_attr "memory" "none,unknown")
					(and (eq_attr "btver2_decode" "vector")
					     (eq_attr "type" "ssemul")))))
			 "btver2-vector,btver2-fpm*6,btver2-fpa*6")

;; AES/CLMUL

(define_insn_reservation "btver2_aes_double" 3
			 (and (eq_attr "cpu" "btver2")
			      (and (match_operand:V2DI 0 "register_operand")
				   (and (eq_attr "memory" "none,unknown")
					(and (eq_attr "btver2_decode" "double")
					     (eq_attr "type" "sselog1")))))
			 "btver2-double,btver2-valu,btver2-vimul")

(define_insn_reservation "btver2_aes_direct" 2
			 (and (eq_attr "cpu" "btver2")
			      (and (match_operand:V2DI 0 "register_operand")
				   (and (eq_attr "memory" "none,unknown")
					(and (eq_attr "btver2_decode" "direct")
					     (eq_attr "type" "sselog1")))))
			 "btver2-direct,btver2-vimul")

;; AVX128 SSE4* SSSE3 SSE3* SSE2 SSE instructions 

(define_insn_reservation "btver2_sseint_load_direct" 6
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "TI")
				   (and (eq_attr "memory" "load")
					(and (eq_attr "btver2_decode" "direct")
					     (eq_attr "type" "sse,ssecmp,sseiadd")))))
			 "btver2-direct,btver2-load,btver2-valu")

(define_insn_reservation "btver2_sseint_direct" 1
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "TI")
				   (and (eq_attr "memory" "none,unknown")
					(and (eq_attr "btver2_decode" "direct")
					     (eq_attr "type" "sse,ssecmp,sseiadd")))))
			 "btver2-direct,btver2-valu")

(define_insn_reservation "btver2_sselog_direct" 1
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "V2DF,V4SF")
				   (and (eq_attr "memory" "none,unknown")
					(and (eq_attr "btver2_decode" "direct")
					     (eq_attr "type" "sse,sselog")))))
			 "btver2-direct,(btver2-fpa|btver2-fpm)")

(define_insn_reservation "btver2_sselog_load_direct" 6
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "V2DF,V4SF")
				   (and (eq_attr "memory" "load")
					(and (eq_attr "btver2_decode" "direct")
					     (eq_attr "type" "sse,sselog")))))
			 "btver2-direct,btver2-load,(btver2-fpa|btver2-fpm)")

(define_insn_reservation "btver2_intext_reg_128" 3
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "SF,QI,SI,HI,SI")
				   (and (eq_attr "memory" "none,unknown")
					(and (eq_attr "btver2_decode" "direct")
					     (eq_attr "type" "sselog")))))
			 "btver2-direct,btver2-fpa")

(define_insn_reservation "btver2_sse_mov_direct" 1
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "V2DF,V4SF")
				   (and (eq_attr "memory" "none,unknown")
					(and (eq_attr "btver2_decode" "direct")
					     (eq_attr "type" "ssemov")))))
			 "btver2-direct,(btver2-fpa|btver2-fpm)")

(define_insn_reservation "btver2_sse_mov_vector" 2 
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "V2DF,V4SF")
				   (and (eq_attr "memory" "none,unknown")
					(and (eq_attr "btver2_decode" "vector")
					     (eq_attr "type" "ssemov")))))
			 "btver2-vector,(btver2-fpa|btver2-fpm)*2")

(define_insn_reservation "btver2_ssecomi_load_128" 8
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "TI")
				   (and (eq_attr "memory" "load")
					(and (eq_attr "btver2_decode" "!vector")
					     (eq_attr "type" "ssecomi")))))
			 "btver2-direct,btver2-load,btver2-fpa")

(define_insn_reservation "btver2_ssecomi_reg_128" 3
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "TI")
				   (and (eq_attr "memory" "none,unknown")
					(and (eq_attr "btver2_decode" "!vector")
					     (eq_attr "type" "ssecomi")))))
			 "btver2-direct,btver2-fpa")

(define_insn_reservation "btver2_ssemul_load_v2df" 14
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "V2DF")
				   (and (eq_attr "memory" "load")
					(and (eq_attr "btver2_decode" "vector")
					     (eq_attr "type" "ssemul")))))
			 "btver2-vector,btver2-load,btver2-fpm*2,btver2-fpa")

(define_insn_reservation "btver2_ssemul_reg_v2df" 9
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "V2DF")
				   (and (eq_attr "memory" "none,unknown")
					(and (eq_attr "btver2_decode" "vector")
					     (eq_attr "type" "ssemul")))))
			 "btver2-vector,btver2-fpm*2,btver2-fpa")

(define_insn_reservation "btver2_ssemul_load_v4sf" 16
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "V4SF")
				   (and (eq_attr "memory" "none,unknown")
					(and (eq_attr "btver2_decode" "vector")
					     (eq_attr "type" "ssemul")))))
			"btver2-vector,btver2-load,btver2-fpm*3,btver2-fpa*2")

(define_insn_reservation "btver2_ssemul_reg_v4sf" 11
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "V4SF")
				   (and (eq_attr "memory" "none,unknown")
					(and (eq_attr "btver2_decode" "vector")
					     (eq_attr "type" "ssemul")))))
			 "btver2-vector,btver2-fpm*3,btver2-fpa*2")

(define_insn_reservation "btver2_sse_store_vectmov" 8
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "memory" "load")
				   (and (eq_attr "btver2_decode" "vector")
					(eq_attr "type" "ssemov"))))
			"btver2-vector,btver2-valu*3,btver2-store")

(define_insn_reservation "btver2_sse_load_vectmov" 8
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "memory" "load")
				   (and (eq_attr "btver2_decode" "vector")
					(eq_attr "type" "ssemov"))))
			 "btver2-vector,btver2-load,btver2-valu*3")

(define_insn_reservation "btver2_sse_vectmov" 3
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "memory" "none,unknown")
				   (and (eq_attr "btver2_decode" "vector")
					(eq_attr "type" "ssemov"))))
			 "btver2-vector,btver2-valu*3")


(define_insn_reservation "btver2_sseimul" 2 
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "memory" "none,unknown")
				   (and (eq_attr "btver2_decode" "direct")
					(eq_attr "type" "sseimul"))))
			 "btver2-direct,btver2-vimul")

(define_insn_reservation "btver2_sseimul_load" 7
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "memory" "load")
				   (and (eq_attr "btver2_decode" "direct")
					(eq_attr "type" "sseimul"))))
			 "btver2-direct,btver2-load,btver2-vimul")

(define_insn_reservation "btver2_sseimul_load_vect" 9
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "memory" "load")
				   (and (eq_attr "btver2_decode" "vector")
					(eq_attr "type" "sseimul"))))
			 "btver2-vector,btver2-load,btver2-vimul*2,btver2-valu")

(define_insn_reservation "btver2_sseimul_vect" 4
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "memory" "none,unknown")
				   (and (eq_attr "btver2_decode" "vector")
					(eq_attr "type" "sseimul"))))
			 "btver2-vector,btver2-vimul*2,btver2-valu")

(define_insn_reservation "btver2_sseins" 3
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "memory" "none,unknown")
				   (eq_attr "type" "sseins")))
			 "btver2-vector,btver2-valu*3")

(define_insn_reservation "btver2_sseishft_load" 6
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "memory" "load")
				   (and (eq_attr "btver2_decode" "direct")
					(eq_attr "type" "sseishft"))))
			 "btver2-direct,btver2-load,btver2-valu")

(define_insn_reservation "btver2_sseishft_direct" 1
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "memory" "none,unknown")
				   (and (eq_attr "btver2_decode" "direct") 
					(eq_attr "type" "sseishft"))))
			 "btver2-direct,btver2-valu")

(define_insn_reservation "btver2_sselog1_load" 6
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode"  "!V8SF,!V4DF")
				   (and (eq_attr "memory" "load")
					(and (eq_attr "btver2_decode" "direct")
					     (eq_attr "type" "sselog1")))))
			 "btver2-direct,btver2-load,btver2-valu")

(define_insn_reservation "btver2_sselog1_direct" 1
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode"  "!V8SF,!V4DF")
				   (and (eq_attr "memory" "none,unknown")
					(and (eq_attr "btver2_decode" "direct")
					     (eq_attr "type" "sselog1")))))
			 "btver2-direct,btver2-valu")

(define_insn_reservation "btver2_sselog1_vector_load" 7
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "memory" "load")
				   (and (eq_attr "btver2_decode" "vector")
					(eq_attr "type" "sselog1"))))
			 "btver2-vector,btver2-valu*2")

(define_insn_reservation "btver2_sselog1_vector" 2
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "memory" "none,unknown")
				   (and (eq_attr "btver2_decode" "vector")
					(eq_attr "type" "sselog1"))))
			 "btver2-vector,btver2-valu*2")

(define_insn_reservation "btver2_sseadd_load" 8
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "V4SF,V2DF")
				   (and (eq_attr "memory" "load")
					(and (eq_attr "btver2_decode" "direct")
					     (eq_attr "type" "sseadd,sseadd1")))))
			 "btver2-direct,btver2-load,btver2-fpa")

(define_insn_reservation "btver2_sseadd_reg" 3
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "V4SF,V2DF")
				   (and (eq_attr "memory" "none,unknown")
					(and (eq_attr "btver2_decode" "direct")
					     (eq_attr "type" "sseadd,sseadd1")))))
			 "btver2-direct,btver2-fpa")

;;SSE2 SSEint SSEfp SSE

(define_insn_reservation "btver2_sseint_to_scalar_move_with_load" 8
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "SI,DI")
				   (and (eq_attr "memory" "load")
					(and (eq_attr "btver2_decode" "direct")
					     (eq_attr "type" "ssemov")))))
			 "btver2-direct,btver2-load,btver2-fpa")

(define_insn_reservation "btver2_sseint_to_scalar_move_with_store" 8
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "SI,DI")
				   (and (eq_attr "memory" "store")
					(and (eq_attr "btver2_decode" "direct")
					     (eq_attr "type" "ssemov")))))
			 "btver2-direct,btver2-fpa,btver2-store")


(define_insn_reservation "btver2_scalar_to_sseint_move_with_load" 11
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "TI")
				   (and (ior ( match_operand:SI 1 "memory_operand")
					     ( match_operand:DI 1 "memory_operand"))
					(eq_attr "type" "ssemov"))))
			 "btver2-direct,btver2-load,btver2-stc,btver2-valu")

(define_insn_reservation "btver2_sseint_to_scalar" 3
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "SI,DI")
				   (and (eq_attr "memory" "none,unknown")
					(and (eq_attr "btver2_decode" "direct")
					     (eq_attr "type" "ssemov")))))
			 "btver2-direct,btver2-fpa")

(define_insn_reservation "btver2_scalar_to_sseint" 6
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "TI")
				   (and (ior ( match_operand:SI 1 "register_operand")
					     ( match_operand:DI 1 "register_operand"))
					(eq_attr "type" "ssemov"))))
			    "btver2-direct,btver2-stc,btver2-valu")

(define_insn_reservation "btver2_sse_int_load" 6
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "TI")
				   (and (eq_attr "memory" "load")
					(and (eq_attr "btver2_decode" "direct")
					     (eq_attr "type" "ssemov,sselog,sseishft1")))))
			 "btver2-direct,btver2-load,btver2-valu")

(define_insn_reservation "btver2_sse_int_direct" 1
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "TI")
				   (and (eq_attr "memory" "none,unknown")
					(and (eq_attr "btver2_decode" "direct") 
					     (eq_attr "type" "ssemov,sselog,sseishft1")))))
			 "btver2-direct,btver2-valu")

(define_insn_reservation "btver2_sse_int_cvt_load" 6
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "DI")
				   (and (eq_attr "memory" "load")
					(and (eq_attr "btver2_decode" "direct")
					     (eq_attr "type" "sseicvt")))))
			 "btver2-direct,btver2-load,btver2-valu")

(define_insn_reservation "btver2_sse_int_cvt" 1
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "DI")
				   (and (eq_attr "memory" "none,unknown")
					(and (eq_attr "btver2_decode" "direct") 
					     (eq_attr "type" "sseicvt")))))
			 "btver2-direct,btver2-valu")

(define_insn_reservation "btver2_sse_int_32_move" 3
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "SI,DI")
				   (and (eq_attr "memory" "none,unknown")
					(and (eq_attr "btver2_decode" "direct")
					     (eq_attr "type" "ssemov")))))
			 "btver2-direct,btver2-fpa")

(define_insn_reservation "btver2_int_32_sse_move" 6
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "TI")
				   (and (ior ( match_operand:SI 1 "register_operand")
					     ( match_operand:DI 1 "register_operand"))
					(eq_attr "type" "ssemov"))))
			 "btver2-direct,btver2-stc,btver2-valu")

(define_insn_reservation "btver2_sse2cvt_load_direct" 8
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "TI,V4SF,V2DF,DI")
				   (and (eq_attr "memory" "load") 
					(and (eq_attr "btver2_decode" "direct")
					     (eq_attr "type" "ssecvt")))))
			 "btver2-direct,btver2-load,btver2-stc")

(define_insn_reservation "btver2_sse2cvt_reg_direct" 3
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "TI,V4SF,V2DF,DI") 
				   (and (eq_attr "btver2_decode" "direct")
					(eq_attr "type" "ssecvt"))))
			 "btver2-direct,btver2-stc")

(define_insn_reservation "btver2_sseicvt_load_si" 11
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "SI")
				   (and (eq_attr "memory" "load")
					(and (eq_attr "btver2_decode" "double")
					     (eq_attr "type" "sseicvt")))))
			 "btver2-double,btver2-load,btver2-stc,btver2-fpa")

(define_insn_reservation "btver2_sseicvt_si" 6
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "SI")
				   (and (eq_attr "btver2_decode" "double")
					(eq_attr "type" "sseicvt"))))
			 "btver2-double,btver2-stc,btver2-fpa")

(define_insn_reservation "btver2_ssecvt_load_df" 11
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "DF")
				   (and (eq_attr "memory" "load")
					(and (eq_attr "btver2_decode" "double")
					     (eq_attr "type" "ssecvt")))))
			 "btver2-double,btver2-load,btver2-stc*2")


(define_insn_reservation "btver2_ssecvt_df" 6
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "DF")
				   (and (eq_attr "btver2_decode" "double")
					(eq_attr "type" "ssecvt"))))
			 "btver2-double,btver2-stc*2")

(define_insn_reservation "btver2_ssecvt_load_sf" 12
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "SF")
				   (and (eq_attr "memory" "load")
					(and (eq_attr "btver2_decode" "double")
					     (eq_attr "type" "ssecvt")))))
			 "btver2-double,btver2-load,btver2-stc*2")

(define_insn_reservation "btver2_ssecvt_sf" 7
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "SF")
				   (and (eq_attr "btver2_decode" "double")
					(eq_attr "type" "ssecvt"))))
			 "btver2-double,btver2-stc*2")

(define_insn_reservation "btver2_sseicvt_load_df" 14
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "DF,SF")
				   (and (eq_attr "memory" "load")
					(and (eq_attr "btver2_decode" "double")
					     (eq_attr "type" "sseicvt")))))
			 "btver2-double,btver2-load,btver2-stc")
;;st,ld-stc
(define_insn_reservation "btver2_sseicvt_df" 9
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "DF,SF")
				   (and (eq_attr "btver2_decode" "double")
					(eq_attr "type" "sseicvt"))))
			 "btver2-double,btver2-stc")


(define_insn_reservation "btver2_scalar_sse_load_add" 8
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "DF,SF")
				   (and (eq_attr "memory" "load")
					(and (eq_attr "btver2_decode" "direct")
					     (eq_attr "type" "sseadd")))))
			 "btver2-direct,btver2-load,btver2-fpa")

(define_insn_reservation "btver2_scalar_sse_add" 3
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "DF,SF")
				   (and (eq_attr "memory" "none,unknown")
					(and (eq_attr "btver2_decode" "direct")
					     (eq_attr "type" "sseadd")))))
			 "btver2-direct,btver2-fpa")

(define_insn_reservation "btver2_int_sse_cmp_load" 7
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "V2DF,V4SF,DF,SF")
				   (and (eq_attr "memory" "load")
					(and (eq_attr "btver2_decode" "direct")
					     (eq_attr "type" "ssecmp")))))
			 "btver2-direct,btver2-load,btver2-fpa")

(define_insn_reservation "btver2_int_sse_cmp" 2
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "V2DF,V4SF,DF,SF")
				   (and (eq_attr "memory" "none,unknown")
					(and (eq_attr "btver2_decode" "direct")
					     (eq_attr "type" "ssecmp")))))
			 "btver2-direct,btver2-fpa")

(define_insn_reservation "btver2_int_sse_comsi_load" 7
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "DF,SF")
				   (and (eq_attr "memory" "load")
					(and (eq_attr "btver2_decode" "direct")
					     (eq_attr "type" "ssecomi")))))
			 "btver2-direct,btver2-fpa")

(define_insn_reservation "btver2_int_sse_comsi" 2
			 (and (eq_attr "cpu" "btver2")
			     (and (eq_attr "mode" "DF,SF")
				   (and (eq_attr "memory" "none,unknown")
					(and (eq_attr "btver2_decode" "direct")
					     (eq_attr "type" "ssecomi")))))
			 "btver2-direct,btver2-fpa")

(define_insn_reservation "btver2_ssemmx_mov_load_default" 6
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "memory" "load")
				   (and (eq_attr "btver2_decode" "direct")
					(eq_attr "type" "ssemov,mmxmov"))))
			 "btver2-direct,btver2-load,(btver2-fpa|btver2-fpm)")

(define_insn_reservation "btver2_ssemmx_mov_store_default" 6
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "memory" "store,both")
				   (and (eq_attr "btver2_decode" "direct")
					(eq_attr "type" "ssemov,mmxmov"))))
			 "btver2-direct,(btver2-fpa|btver2-fpm),btver2-store")

(define_insn_reservation "btver2_sse_mov_default" 1
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "memory" "none,unknown")
				   (and (eq_attr "btver2_decode" "direct")
					(eq_attr "type" "ssemov,mmxmov"))))
			 "btver2-direct,(btver2-fpa|btver2-fpm)")

(define_insn_reservation "btver2_sse_shuf_double" 2
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "memory" "none,unknown")
				   (and (eq_attr "mode" "V4DF,V8SF")     
					(eq_attr "type" "sseshuf"))))
			 "btver2-double,(btver2-fpa|btver2-fpm)")

(define_insn_reservation "btver2_sse_shuf_direct" 1
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "memory" "none,unknown")
				   (and (eq_attr "mode" "V2DF,V4SF")
					(eq_attr "type" "sseshuf,sseshuf1"))))
			 "btver2-direct,(btver2-fpa|btver2-fpm)")

(define_insn_reservation "btver2_sse_shuf_double_load" 7
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "memory" "load")
				   (and (eq_attr "mode" "V4DF,V8SF")
					(eq_attr "type" "sseshuf"))))
			 "btver2-double,btver2-load,(btver2-fpa|btver2-fpm)")

(define_insn_reservation "btver2_sse_shuf_direct_load" 6
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "memory" "load")
				   (and (eq_attr "mode" "V2DF,V4SF")
					(eq_attr "type" "sseshuf,sseshuf1"))))
			 "btver2-direct,btver2-load,(btver2-fpa|btver2-fpm)")

(define_insn_reservation "btver2_sse_div" 19
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "V2DF,DF,V4SF")
				   (and (eq_attr "memory" "none,unknown")
					(eq_attr "type" "ssediv"))))
			 "btver2-direct,btver2-fpm*19")

(define_insn_reservation "btver2_sse_div_sf" 14
			 (and (eq_attr "cpu" "btver2")
			     (and (eq_attr "mode" "SF")
				   (and (eq_attr "memory" "none,unknown")
					(eq_attr "type" "ssediv"))))
			 "btver2-direct,btver2-fpm*14")

(define_insn_reservation "btver2_sse_mul" 4
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "V2DF,DF,V4SF,SF")
				   (and (eq_attr "memory" "none,unknown")
					(eq_attr "type" "ssemul"))))
			 "btver2-direct,btver2-fpm*2")

(define_insn_reservation "btver2_sse_mul_sf" 2
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "V2DF,DF,V4SF,SF")
				   (and (eq_attr "memory" "none,unknown")
					(eq_attr "type" "ssemul"))))
			 "btver2-direct,btver2-fpm")

(define_insn_reservation "btver2_sse_div_load" 24
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "V2DF,DF,V4SF")
				   (and (eq_attr "memory" "load")
					(eq_attr "type" "ssediv"))))
			 "btver2-direct,btver2-load,btver2-fpm*19")

(define_insn_reservation "btver2_sse_div_sf_load" 19
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "SF")
				   (and (eq_attr "memory" "load")
					(eq_attr "type" "ssediv"))))
			 "btver2-direct,btver2-load,btver2-fpm*14")

(define_insn_reservation "btver2_sse_mul_load" 9
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "V2DF,DF,V4SF,SF")
				   (and (eq_attr "memory" "load")
					(eq_attr "type" "ssemul"))))
			 "btver2-direct,btver2-load,btver2-fpm*2")

(define_insn_reservation "btver2_sse_mul_sf_load" 7
			 (and (eq_attr "cpu" "btver2")
			      (and (eq_attr "mode" "V2DF,DF,V4SF,SF")
				   (and (eq_attr "memory" "load")
					(eq_attr "type" "ssemul"))))
			 "btver2-direct,btver2-load,btver2-fpm")

