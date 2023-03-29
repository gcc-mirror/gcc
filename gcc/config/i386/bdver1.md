;; Copyright (C) 2010-2023 Free Software Foundation, Inc.
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
;; AMD bdver1 Scheduling
;;
;; The bdver1 contains four pipelined FP units, two integer units and
;; two address generation units.
;;
;; The predecode logic is determining boundaries of instructions in the 64
;; byte cache line.  So the cache line straddling problem of K6 might be issue
;; here as well, but it is not noted in the documentation.
;;
;; Three DirectPath instructions decoders and only one VectorPath decoder
;; is available.  They can decode three DirectPath instructions or one
;; VectorPath instruction per cycle.
;;
;; The load/store queue unit is not attached to the schedulers but
;; communicates with all the execution units separately instead.


(define_attr "bdver1_decode" "direct,vector,double"
  (const_string "direct"))

(define_automaton "bdver1,bdver1_ieu,bdver1_load,bdver1_fp,bdver1_agu")

(define_cpu_unit "bdver1-decode0" "bdver1")
(define_cpu_unit "bdver1-decode1" "bdver1")
(define_cpu_unit "bdver1-decode2" "bdver1")
(define_cpu_unit "bdver1-decodev" "bdver1")

;; Model the fact that double decoded instruction may take 2 cycles
;; to decode when decoder2 and decoder0 in next cycle
;; is used (this is needed to allow throughput of 1.5 double decoded
;; instructions per cycle).
;;
;; In order to avoid dependence between reservation of decoder
;; and other units, we model decoder as two stage fully pipelined unit
;; and only double decoded instruction may occupy unit in the first cycle.
;; With this scheme however two double instructions can be issued cycle0.
;;
;; Avoid this by using presence set requiring decoder0 to be allocated
;; too.  Vector decoded instructions then can't be issued when modeled
;; as consuming decoder0+decoder1+decoder2.
;; We solve that by specialized vector decoder unit and exclusion set.
(presence_set "bdver1-decode2" "bdver1-decode0")
(exclusion_set "bdver1-decodev" "bdver1-decode0,bdver1-decode1,bdver1-decode2")

(define_reservation "bdver1-vector" "nothing,bdver1-decodev")
(define_reservation "bdver1-direct1" "nothing,bdver1-decode1")
(define_reservation "bdver1-direct" "nothing,
				     (bdver1-decode0 | bdver1-decode1
				     | bdver1-decode2)")
;; Double instructions behaves like two direct instructions.
(define_reservation "bdver1-double" "((bdver1-decode2,bdver1-decode0)
				     | (nothing,(bdver1-decode0 + bdver1-decode1))
				     | (nothing,(bdver1-decode1 + bdver1-decode2)))")


(define_cpu_unit "bdver1-ieu0" "bdver1_ieu")
(define_cpu_unit "bdver1-ieu1" "bdver1_ieu")
(define_reservation "bdver1-ieu" "(bdver1-ieu0 | bdver1-ieu1)")

(define_cpu_unit "bdver1-agu0" "bdver1_agu")
(define_cpu_unit "bdver1-agu1" "bdver1_agu")
(define_reservation "bdver1-agu" "(bdver1-agu0 | bdver1-agu1)")

(define_cpu_unit "bdver1-load0" "bdver1_load")
(define_cpu_unit "bdver1-load1" "bdver1_load")
(define_reservation "bdver1-load" "bdver1-agu,
				   (bdver1-load0 | bdver1-load1),nothing")
;; 128bit SSE instructions issue two loads at once.
(define_reservation "bdver1-load2" "bdver1-agu,
				   (bdver1-load0 + bdver1-load1),nothing")

(define_reservation "bdver1-store" "(bdver1-load0 | bdver1-load1)")
;; 128bit SSE instructions issue two stores at once.
(define_reservation "bdver1-store2" "(bdver1-load0 + bdver1-load1)")

;; vectorpath (microcoded) instructions are single issue instructions.
;; So, they occupy all the integer units.
(define_reservation "bdver1-ivector" "bdver1-ieu0+bdver1-ieu1+
                                      bdver1-agu0+bdver1-agu1+
                                      bdver1-load0+bdver1-load1")

;; The FP operations start to execute at stage 12 in the pipeline, while
;; integer operations start to execute at stage 9 for athlon and 11 for K8
;; Compensate the difference for athlon because it results in significantly
;; smaller automata.
;; NOTE: the above information was just copied from athlon.md, and was not
;; actually verified for bdver1.
(define_reservation "bdver1-fpsched" "nothing,nothing,nothing")
;; The floating point loads.
(define_reservation "bdver1-fpload" "(bdver1-fpsched + bdver1-load)")
(define_reservation "bdver1-fpload2" "(bdver1-fpsched + bdver1-load2)")

;; Four FP units.
(define_cpu_unit "bdver1-ffma0" "bdver1_fp")
(define_cpu_unit "bdver1-ffma1" "bdver1_fp")
(define_cpu_unit "bdver1-fmal0" "bdver1_fp")
(define_cpu_unit "bdver1-fmal1" "bdver1_fp")

(define_reservation "bdver1-ffma"     "(bdver1-ffma0 | bdver1-ffma1)")
(define_reservation "bdver1-fcvt"     "bdver1-ffma0")
(define_reservation "bdver1-fmma"     "bdver1-ffma0")
(define_reservation "bdver1-fxbar"    "bdver1-ffma1")
(define_reservation "bdver1-fmal"     "(bdver1-fmal0 | bdver1-fmal1)")
(define_reservation "bdver1-fsto"     "bdver1-fmal1")

;; Vector operations usually consume many of pipes.
(define_reservation "bdver1-fvector"  "(bdver1-ffma0 + bdver1-ffma1
					+ bdver1-fmal0 + bdver1-fmal1)")

;; Jump instructions are executed in the branch unit completely transparent to us.
(define_insn_reservation "bdver1_call" 0
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (eq_attr "type" "call,callv"))
			 "bdver1-double,bdver1-agu")
;; PUSH mem is double path.
(define_insn_reservation "bdver1_push" 1
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (eq_attr "type" "push"))
			 "bdver1-direct,bdver1-agu,bdver1-store")
;; POP r16/mem are double path.
(define_insn_reservation "bdver1_pop" 1
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (eq_attr "type" "pop"))
			 "bdver1-direct,bdver1-ivector")
;; LEAVE no latency info so far, assume same with amdfam10.
(define_insn_reservation "bdver1_leave" 3
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (eq_attr "type" "leave"))
			 "bdver1-vector,bdver1-ivector")
;; LEA executes in AGU unit with 1 cycle latency on BDVER1.
(define_insn_reservation "bdver1_lea" 1
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (eq_attr "type" "lea"))
			 "bdver1-direct,bdver1-agu")

;; MUL executes in special multiplier unit attached to IEU1.
(define_insn_reservation "bdver1_imul_DI" 6
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "imul")
				   (and (eq_attr "mode" "DI")
					(eq_attr "memory" "none,unknown"))))
			 "bdver1-direct1,bdver1-ieu1")
(define_insn_reservation "bdver1_imul" 4
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "imul")
				   (eq_attr "memory" "none,unknown")))
			 "bdver1-direct1,bdver1-ieu1")
(define_insn_reservation "bdver1_imul_mem_DI" 10
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "imul")
				   (and (eq_attr "mode" "DI")
					(eq_attr "memory" "load,both"))))
                         "bdver1-direct1,bdver1-load,bdver1-ieu1")
(define_insn_reservation "bdver1_imul_mem" 8
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "imul")
				   (eq_attr "memory" "load,both")))
			 "bdver1-direct1,bdver1-load,bdver1-ieu1")

;; IDIV cannot execute in parallel with other instructions.  Dealing with it
;; as with short latency vector instruction is good approximation avoiding
;; scheduler from trying too hard to can hide it's latency by overlap with
;; other instructions.
;; ??? Experiments show that the IDIV can overlap with roughly 6 cycles
;; of the other code.
(define_insn_reservation "bdver1_idiv" 6
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "idiv")
				   (eq_attr "memory" "none,unknown")))
			 "bdver1-vector,(bdver1-ieu0*6+(bdver1-fpsched,bdver1-fvector))")

(define_insn_reservation "bdver1_idiv_mem" 10
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "idiv")
				   (eq_attr "memory" "load,both")))
			 "bdver1-vector,((bdver1-load,bdver1-ieu0*6)+(bdver1-fpsched,bdver1-fvector))")

;; The parallelism of string instructions is not documented.  Model it same way
;; as IDIV to create smaller automata.  This probably does not matter much.
;; Using the same heuristics for bdver1 as amdfam10 and K8 with IDIV.
(define_insn_reservation "bdver1_str" 6
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "str")
				   (eq_attr "memory" "load,both,store")))
			 "bdver1-vector,bdver1-load,bdver1-ieu0*6")

;; Integer instructions.
(define_insn_reservation "bdver1_idirect" 1
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "bdver1_decode" "direct")
				   (and (eq_attr "unit" "integer,unknown")
					(eq_attr "memory" "none,unknown"))))
			 "bdver1-direct,bdver1-ieu")
(define_insn_reservation "bdver1_ivector" 2
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "bdver1_decode" "vector")
				   (and (eq_attr "unit" "integer,unknown")
					(eq_attr "memory" "none,unknown"))))
			 "bdver1-vector,bdver1-ieu,bdver1-ieu")
(define_insn_reservation "bdver1_idirect_loadmov" 4
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "imov")
				   (eq_attr "memory" "load")))
			 "bdver1-direct,bdver1-load")
(define_insn_reservation "bdver1_idirect_load" 5
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "bdver1_decode" "direct")
				   (and (eq_attr "unit" "integer,unknown")
					(eq_attr "memory" "load"))))
			 "bdver1-direct,bdver1-load,bdver1-ieu")
(define_insn_reservation "bdver1_ivector_load" 6
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "bdver1_decode" "vector")
				   (and (eq_attr "unit" "integer,unknown")
					(eq_attr "memory" "load"))))
			 "bdver1-vector,bdver1-load,bdver1-ieu,bdver1-ieu")
(define_insn_reservation "bdver1_idirect_movstore" 4
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "imov")
				   (eq_attr "memory" "store")))
			 "bdver1-direct,bdver1-agu,bdver1-store")
(define_insn_reservation "bdver1_idirect_both" 4
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "bdver1_decode" "direct")
				   (and (eq_attr "unit" "integer,unknown")
					(eq_attr "memory" "both"))))
			 "bdver1-direct,bdver1-load,
			  bdver1-ieu,bdver1-store,
			  bdver1-store")
(define_insn_reservation "bdver1_ivector_both" 5
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "bdver1_decode" "vector")
				   (and (eq_attr "unit" "integer,unknown")
					(eq_attr "memory" "both"))))
			 "bdver1-vector,bdver1-load,
			  bdver1-ieu,
			  bdver1-ieu,
			  bdver1-store")
(define_insn_reservation "bdver1_idirect_store" 4
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "bdver1_decode" "direct")
				   (and (eq_attr "unit" "integer,unknown")
					(eq_attr "memory" "store"))))
			 "bdver1-direct,(bdver1-ieu+bdver1-agu),
			  bdver1-store")
(define_insn_reservation "bdver1_ivector_store" 5
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "bdver1_decode" "vector")
				   (and (eq_attr "unit" "integer,unknown")
					(eq_attr "memory" "store"))))
			 "bdver1-vector,(bdver1-ieu+bdver1-agu),bdver1-ieu,
			  bdver1-store")

;; BDVER1 floating point units.
(define_insn_reservation "bdver1_fldxf" 13
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "fmov")
				   (and (eq_attr "memory" "load")
					(eq_attr "mode" "XF"))))
			 "bdver1-vector,bdver1-fpload2,bdver1-fvector*9")
(define_insn_reservation "bdver1_fld" 5
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "fmov")
				   (eq_attr "memory" "load")))
			 "bdver1-direct,bdver1-fpload,bdver1-ffma")
(define_insn_reservation "bdver1_fstxf" 8
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "fmov")
				   (and (eq_attr "memory" "store,both")
					(eq_attr "mode" "XF"))))
			 "bdver1-vector,(bdver1-fpsched+bdver1-agu),(bdver1-store2+(bdver1-fvector*6))")
(define_insn_reservation "bdver1_fst" 2
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "fmov")
				   (eq_attr "memory" "store,both")))
			 "bdver1-double,(bdver1-fpsched+bdver1-agu),(bdver1-fsto+bdver1-store)")
(define_insn_reservation "bdver1_fist" 2
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (eq_attr "type" "fistp,fisttp"))
			 "bdver1-double,(bdver1-fpsched+bdver1-agu),(bdver1-fsto+bdver1-store)")
(define_insn_reservation "bdver1_fmov_bdver1" 2
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (eq_attr "type" "fmov"))
			 "bdver1-direct,bdver1-fpsched,bdver1-ffma")
(define_insn_reservation "bdver1_fadd_load" 10
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "fop")
				   (eq_attr "memory" "load")))
			 "bdver1-direct,bdver1-fpload,bdver1-ffma")
(define_insn_reservation "bdver1_fadd" 6
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (eq_attr "type" "fop"))
			 "bdver1-direct,bdver1-fpsched,bdver1-ffma")
(define_insn_reservation "bdver1_fmul_load" 10
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "fmul")
				   (eq_attr "memory" "load")))
			 "bdver1-double,bdver1-fpload,bdver1-ffma")
(define_insn_reservation "bdver1_fmul" 6
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (eq_attr "type" "fmul"))
			 "bdver1-direct,bdver1-fpsched,bdver1-ffma")
(define_insn_reservation "bdver1_fsgn" 2
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (eq_attr "type" "fsgn"))
			 "bdver1-direct,bdver1-fpsched,bdver1-ffma")
(define_insn_reservation "bdver1_fdiv_load" 46
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "fdiv")
				   (eq_attr "memory" "load")))
			 "bdver1-direct,bdver1-fpload,bdver1-ffma")
(define_insn_reservation "bdver1_fdiv" 42
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (eq_attr "type" "fdiv"))
			 "bdver1-direct,bdver1-fpsched,bdver1-ffma")
(define_insn_reservation "bdver1_fpspc_load" 103
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "fpspc")
				   (eq_attr "memory" "load")))
			 "bdver1-vector,bdver1-fpload,bdver1-fvector")
(define_insn_reservation "bdver1_fpspc" 100
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "fpspc")
				   (eq_attr "memory" "load")))
			 "bdver1-vector,bdver1-fpload,bdver1-fvector")
(define_insn_reservation "bdver1_fcmov_load" 17
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "fcmov")
				   (eq_attr "memory" "load")))
			 "bdver1-vector,bdver1-fpload,bdver1-fvector")
(define_insn_reservation "bdver1_fcmov" 15
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (eq_attr "type" "fcmov"))
			 "bdver1-vector,bdver1-fpsched,bdver1-fvector")
(define_insn_reservation "bdver1_fcomi_load" 6
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "fcmp")
				   (and (eq_attr "bdver1_decode" "double")
					(eq_attr "memory" "load"))))
			 "bdver1-double,bdver1-fpload,(bdver1-ffma | bdver1-fsto)")
(define_insn_reservation "bdver1_fcomi" 2
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "bdver1_decode" "double")
				   (eq_attr "type" "fcmp")))
			 "bdver1-double,bdver1-fpsched,(bdver1-ffma | bdver1-fsto)")
(define_insn_reservation "bdver1_fcom_load" 6
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "fcmp")
				   (eq_attr "memory" "load")))
			 "bdver1-direct,bdver1-fpload,bdver1-ffma")
(define_insn_reservation "bdver1_fcom" 2
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (eq_attr "type" "fcmp"))
			 "bdver1-direct,bdver1-fpsched,bdver1-ffma")
(define_insn_reservation "bdver1_fxch" 2
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (eq_attr "type" "fxch"))
			 "bdver1-direct,bdver1-fpsched,bdver1-ffma")

;; SSE loads.
(define_insn_reservation "bdver1_ssevector_avx128_unaligned_load" 4
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "ssemov")
				   (and (eq_attr "prefix" "vex")
					(and (eq_attr "movu" "1")
					     (and (eq_attr "mode" "V4SF,V2DF")
						  (eq_attr "memory" "load"))))))
			 "bdver1-direct,bdver1-fpload")
(define_insn_reservation "bdver1_ssevector_avx256_unaligned_load" 5
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "ssemov")
				   (and (eq_attr "movu" "1")
				        (and (eq_attr "mode" "V8SF,V4DF")
				             (eq_attr "memory" "load")))))
			 "bdver1-double,bdver1-fpload")
(define_insn_reservation "bdver1_ssevector_sse128_unaligned_load" 4
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "ssemov")
				   (and (eq_attr "movu" "1")
				        (and (eq_attr "mode" "V4SF,V2DF")
				             (eq_attr "memory" "load")))))
			 "bdver1-direct,bdver1-fpload,bdver1-fmal")
(define_insn_reservation "bdver1_ssevector_avx128_load" 4
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "ssemov")
				   (and (eq_attr "prefix" "vex")
				        (and (eq_attr "mode" "V4SF,V2DF,TI")
				             (eq_attr "memory" "load")))))
			 "bdver1-direct,bdver1-fpload,bdver1-fmal")
(define_insn_reservation "bdver1_ssevector_avx256_load" 5
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "ssemov")
				   (and (eq_attr "mode" "V8SF,V4DF,OI")
				        (eq_attr "memory" "load"))))
			 "bdver1-double,bdver1-fpload,bdver1-fmal")
(define_insn_reservation "bdver1_ssevector_sse128_load" 4
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "ssemov")
				   (and (eq_attr "mode" "V4SF,V2DF,TI")
				        (eq_attr "memory" "load"))))
			 "bdver1-direct,bdver1-fpload")
(define_insn_reservation "bdver1_ssescalar_movq_load" 4
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "ssemov")
				   (and (eq_attr "mode" "DI")
				        (eq_attr "memory" "load"))))
			 "bdver1-direct,bdver1-fpload,bdver1-fmal")
(define_insn_reservation "bdver1_ssescalar_vmovss_load" 4
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "ssemov")
				   (and (eq_attr "prefix" "vex")
				        (and (eq_attr "mode" "SF")
				             (eq_attr "memory" "load")))))
			 "bdver1-direct,bdver1-fpload")
(define_insn_reservation "bdver1_ssescalar_sse128_load" 4
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "ssemov")
				   (and (eq_attr "mode" "SF,DF")
				        (eq_attr "memory" "load"))))
			 "bdver1-direct,bdver1-fpload, bdver1-ffma")
(define_insn_reservation "bdver1_mmxsse_load" 4
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "mmxmov,ssemov")
				   (eq_attr "memory" "load")))
			 "bdver1-direct,bdver1-fpload, bdver1-fmal")

;; SSE stores.
(define_insn_reservation "bdver1_sse_store_avx256" 5
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "ssemov")
				   (and (eq_attr "mode" "V8SF,V4DF,OI")
					(eq_attr "memory" "store,both"))))
			 "bdver1-double,(bdver1-fpsched+bdver1-agu),((bdver1-fsto+bdver1-store)*2)")
(define_insn_reservation "bdver1_sse_store" 4
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "ssemov")
				   (and (eq_attr "mode" "V4SF,V2DF,TI")
					(eq_attr "memory" "store,both"))))
			 "bdver1-direct,(bdver1-fpsched+bdver1-agu),((bdver1-fsto+bdver1-store)*2)")
(define_insn_reservation "bdver1_mmxsse_store_short" 4
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "mmxmov,ssemov")
				   (eq_attr "memory" "store,both")))
			 "bdver1-direct,(bdver1-fpsched+bdver1-agu),(bdver1-fsto+bdver1-store)")

;; Register moves.
(define_insn_reservation "bdver1_ssevector_avx256" 3
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "ssemov")
				   (and (eq_attr "mode" "V8SF,V4DF,OI")
					(eq_attr "memory" "none"))))
			 "bdver1-double,bdver1-fpsched,bdver1-fmal")
(define_insn_reservation "bdver1_movss_movsd" 2
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "ssemov")
				   (and (eq_attr "mode" "SF,DF")
                                        (eq_attr "memory" "none"))))
			 "bdver1-direct,bdver1-fpsched,bdver1-ffma")
(define_insn_reservation "bdver1_mmxssemov" 2
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "mmxmov,ssemov")
				   (eq_attr "memory" "none")))
			 "bdver1-direct,bdver1-fpsched,bdver1-fmal")
;; SSE logs.
(define_insn_reservation "bdver1_sselog_load_256" 7
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "sselog,sselog1,sseshuf,sseshuf1")
				   (and (eq_attr "mode" "V8SF")
				   (eq_attr "memory" "load"))))
			 "bdver1-double,bdver1-fpload,bdver1-fmal")
(define_insn_reservation "bdver1_sselog_256" 3
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "sselog,sselog1,sseshuf,sseshuf1")
                                   (eq_attr "mode" "V8SF")))
			 "bdver1-double,bdver1-fpsched,bdver1-fmal")
(define_insn_reservation "bdver1_sselog_load" 6
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "sselog,sselog1,sseshuf,sseshuf1")
				   (eq_attr "memory" "load")))
			 "bdver1-direct,bdver1-fpload,bdver1-fxbar")
(define_insn_reservation "bdver1_sselog" 2
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (eq_attr "type" "sselog,sselog1,sseshuf,sseshuf1"))
			 "bdver1-direct,bdver1-fpsched,bdver1-fxbar")

;; PCMP actually executes in FMAL.
(define_insn_reservation "bdver1_ssecmp_load" 6
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "ssecmp")
				   (eq_attr "memory" "load")))
			 "bdver1-direct,bdver1-fpload,bdver1-ffma")
(define_insn_reservation "bdver1_ssecmp" 2
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (eq_attr "type" "ssecmp"))
			 "bdver1-direct,bdver1-fpsched,bdver1-ffma")
(define_insn_reservation "bdver1_ssecomi_load" 6
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "ssecomi")
				   (eq_attr "memory" "load")))
			 "bdver1-double,bdver1-fpload,(bdver1-ffma | bdver1-fsto)")
(define_insn_reservation "bdver1_ssecomi" 2
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (eq_attr "type" "ssecomi"))
			 "bdver1-double,bdver1-fpsched,(bdver1-ffma | bdver1-fsto)")

;; Conversions behaves very irregularly and the scheduling is critical here.
;; Take each instruction separately.

;; 256 bit conversion.
(define_insn_reservation "bdver1_vcvtX2Y_avx256_load" 8
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "ssecvt")
				   (and (eq_attr "memory" "load")
					(ior (ior (match_operand:V4DF 0 "register_operand")
					          (ior (match_operand:V8SF 0 "register_operand")
						       (match_operand:V8SI 0 "register_operand")))
					     (ior (match_operand:V4DF 1 "nonimmediate_operand")
						  (ior (match_operand:V8SF 1 "nonimmediate_operand")
						       (match_operand:V8SI 1 "nonimmediate_operand")))))))
			 "bdver1-vector,bdver1-fpload,bdver1-fvector")
(define_insn_reservation "bdver1_vcvtX2Y_avx256" 4
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "ssecvt")
				   (and (eq_attr "memory" "none")
					(ior (ior (match_operand:V4DF 0 "register_operand")
					          (ior (match_operand:V8SF 0 "register_operand")
						       (match_operand:V8SI 0 "register_operand")))
					     (ior (match_operand:V4DF 1 "nonimmediate_operand")
						  (ior (match_operand:V8SF 1 "nonimmediate_operand")
						       (match_operand:V8SI 1 "nonimmediate_operand")))))))
			 "bdver1-vector,bdver1-fpsched,bdver1-fvector")
;; CVTSS2SD, CVTSD2SS.
(define_insn_reservation "bdver1_ssecvt_cvtss2sd_load" 8
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "ssecvt")
				   (and (eq_attr "mode" "SF,DF")
					(eq_attr "memory" "load"))))
			 "bdver1-direct,bdver1-fpload,bdver1-fcvt")
(define_insn_reservation "bdver1_ssecvt_cvtss2sd" 4
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "ssecvt")
				   (and (eq_attr "mode" "SF,DF")
					(eq_attr "memory" "none"))))
			 "bdver1-direct,bdver1-fpsched,bdver1-fcvt")
;; CVTSI2SD, CVTSI2SS, CVTSI2SDQ, CVTSI2SSQ.
(define_insn_reservation "bdver1_sseicvt_cvtsi2sd_load" 8
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "sseicvt")
				   (and (eq_attr "mode" "SF,DF")
					(eq_attr "memory" "load"))))
			 "bdver1-direct,bdver1-fpload,bdver1-fcvt")
(define_insn_reservation "bdver1_sseicvt_cvtsi2sd" 4
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "sseicvt")
				   (and (eq_attr "mode" "SF,DF")
					(eq_attr "memory" "none"))))
			 "bdver1-double,bdver1-fpsched,(nothing | bdver1-fcvt)")
;; CVTPD2PS.
(define_insn_reservation "bdver1_ssecvt_cvtpd2ps_load" 8
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "ssecvt")
				   (and (eq_attr "memory" "load")
                                        (and (match_operand:V4SF 0 "register_operand")
					     (match_operand:V2DF 1 "nonimmediate_operand")))))
			 "bdver1-double,bdver1-fpload,(bdver1-fxbar | bdver1-fcvt)")
(define_insn_reservation "bdver1_ssecvt_cvtpd2ps" 4
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "ssecvt")
				   (and (eq_attr "memory" "none")
                                        (and (match_operand:V4SF 0 "register_operand")
					     (match_operand:V2DF 1 "nonimmediate_operand")))))
			 "bdver1-double,bdver1-fpsched,(bdver1-fxbar | bdver1-fcvt)")
;; CVTPI2PS, CVTDQ2PS.
(define_insn_reservation "bdver1_ssecvt_cvtdq2ps_load" 8
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "ssecvt")
				   (and (eq_attr "memory" "load")
                                        (and (match_operand:V4SF 0 "register_operand")
					     (ior (match_operand:V2SI 1 "nonimmediate_operand")
					          (match_operand:V4SI 1 "nonimmediate_operand"))))))
			 "bdver1-direct,bdver1-fpload,bdver1-fcvt")
(define_insn_reservation "bdver1_ssecvt_cvtdq2ps" 4
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "ssecvt")
				   (and (eq_attr "memory" "none")
                                        (and (match_operand:V4SF 0 "register_operand")
					     (ior (match_operand:V2SI 1 "nonimmediate_operand")
					          (match_operand:V4SI 1 "nonimmediate_operand"))))))
			 "bdver1-direct,bdver1-fpsched,bdver1-fcvt")
;; CVTDQ2PD.
(define_insn_reservation "bdver1_ssecvt_cvtdq2pd_load" 8
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "ssecvt")
				   (and (eq_attr "memory" "load")
                                        (and (match_operand:V2DF 0 "register_operand")
					     (match_operand:V4SI 1 "nonimmediate_operand")))))
			 "bdver1-double,bdver1-fpload,(bdver1-fxbar | bdver1-fcvt)")
(define_insn_reservation "bdver1_ssecvt_cvtdq2pd" 4
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "ssecvt")
				   (and (eq_attr "memory" "none")
                                        (and (match_operand:V2DF 0 "register_operand")
					     (match_operand:V4SI 1 "nonimmediate_operand")))))
			 "bdver1-double,bdver1-fpsched,(bdver1-fxbar | bdver1-fcvt)")
;; CVTPS2PD, CVTPI2PD.
(define_insn_reservation "bdver1_ssecvt_cvtps2pd_load" 6
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "ssecvt")
				   (and (eq_attr "memory" "load")
                                        (and (match_operand:V2DF 0 "register_operand")
					     (ior (match_operand:V2SI 1 "nonimmediate_operand")
					          (match_operand:V4SF 1 "nonimmediate_operand"))))))
			 "bdver1-double,bdver1-fpload,(bdver1-fxbar | bdver1-fcvt)")
(define_insn_reservation "bdver1_ssecvt_cvtps2pd" 2
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "ssecvt")
				   (and (eq_attr "memory" "load")
                                        (and (match_operand:V2DF 0 "register_operand")
					     (ior (match_operand:V2SI 1 "nonimmediate_operand")
					          (match_operand:V4SF 1 "nonimmediate_operand"))))))
			 "bdver1-double,bdver1-fpsched,(bdver1-fxbar | bdver1-fcvt)")
;; CVTSD2SI, CVTSD2SIQ, CVTSS2SI, CVTSS2SIQ, CVTTSD2SI, CVTTSD2SIQ, CVTTSS2SI, CVTTSS2SIQ.
(define_insn_reservation "bdver1_ssecvt_cvtsX2si_load" 8
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "sseicvt")
				   (and (eq_attr "mode" "SI,DI")
					(eq_attr "memory" "load"))))
			 "bdver1-double,bdver1-fpload,(bdver1-fcvt | bdver1-fsto)")
(define_insn_reservation "bdver1_ssecvt_cvtsX2si" 4
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "sseicvt")
				   (and (eq_attr "mode" "SI,DI")
					(eq_attr "memory" "none"))))
			 "bdver1-double,bdver1-fpsched,(bdver1-fcvt | bdver1-fsto)")
;; CVTPD2PI, CVTTPD2PI.
(define_insn_reservation "bdver1_ssecvt_cvtpd2pi_load" 8
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "ssecvt")
				   (and (eq_attr "memory" "load")
				        (and (match_operand:V2DF 1 "nonimmediate_operand")
					     (match_operand:V2SI 0 "register_operand")))))
			 "bdver1-double,bdver1-fpload,(bdver1-fcvt | bdver1-fxbar)")
(define_insn_reservation "bdver1_ssecvt_cvtpd2pi" 4
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "ssecvt")
				   (and (eq_attr "memory" "none")
				        (and (match_operand:V2DF 1 "nonimmediate_operand")
					     (match_operand:V2SI 0 "register_operand")))))
			 "bdver1-double,bdver1-fpsched,(bdver1-fcvt | bdver1-fxbar)")
;; CVTPD2DQ, CVTTPD2DQ.
(define_insn_reservation "bdver1_ssecvt_cvtpd2dq_load" 6
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "ssecvt")
				   (and (eq_attr "memory" "load")
				        (and (match_operand:V2DF 1 "nonimmediate_operand")
					     (match_operand:V4SI 0 "register_operand")))))
			 "bdver1-double,bdver1-fpload,(bdver1-fcvt | bdver1-fxbar)")
(define_insn_reservation "bdver1_ssecvt_cvtpd2dq" 2
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "ssecvt")
				   (and (eq_attr "memory" "none")
				        (and (match_operand:V2DF 1 "nonimmediate_operand")
					     (match_operand:V4SI 0 "register_operand")))))
			 "bdver1-double,bdver1-fpsched,(bdver1-fcvt | bdver1-fxbar)")
;; CVTPS2PI, CVTTPS2PI, CVTPS2DQ, CVTTPS2DQ.
(define_insn_reservation "bdver1_ssecvt_cvtps2pi_load" 8
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "ssecvt")
                                   (and (eq_attr "memory" "load")
				        (and (match_operand:V4SF 1 "nonimmediate_operand")
				             (ior (match_operand: V2SI 0 "register_operand")
						  (match_operand: V4SI 0 "register_operand"))))))
			 "bdver1-direct,bdver1-fpload,bdver1-fcvt")
(define_insn_reservation "bdver1_ssecvt_cvtps2pi" 4
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "ssecvt")
				   (and (eq_attr "memory" "none")
				        (and (match_operand:V4SF 1 "nonimmediate_operand")
				             (ior (match_operand: V2SI 0 "register_operand")
						  (match_operand: V4SI 0 "register_operand"))))))
			 "bdver1-direct,bdver1-fpsched,bdver1-fcvt")

;; SSE MUL, ADD, and MULADD.
(define_insn_reservation "bdver1_ssemuladd_load_256" 11
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "ssemul,sseadd,sseadd1,ssemuladd")
				   (and (eq_attr "mode" "V8SF,V4DF")
					(eq_attr "memory" "load"))))
			 "bdver1-double,bdver1-fpload,bdver1-ffma")
(define_insn_reservation "bdver1_ssemuladd_256" 7
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "ssemul,sseadd,sseadd1,ssemuladd")
				   (and (eq_attr "mode" "V8SF,V4DF")
					(eq_attr "memory" "none"))))
			 "bdver1-double,bdver1-fpsched,bdver1-ffma")
(define_insn_reservation "bdver1_ssemuladd_load" 10
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "ssemul,sseadd,sseadd1,ssemuladd")
				   (eq_attr "memory" "load")))
			 "bdver1-direct,bdver1-fpload,bdver1-ffma")
(define_insn_reservation "bdver1_ssemuladd" 6
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "ssemul,sseadd,sseadd1,ssemuladd")
				   (eq_attr "memory" "none")))
			 "bdver1-direct,bdver1-fpsched,bdver1-ffma")
(define_insn_reservation "bdver1_sseimul_load" 8
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "sseimul")
				   (eq_attr "memory" "load")))
			 "bdver1-direct,bdver1-fpload,bdver1-fmma")
(define_insn_reservation "bdver1_sseimul" 4
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "sseimul")
				   (eq_attr "memory" "none")))
			 "bdver1-direct,bdver1-fpsched,bdver1-fmma")
(define_insn_reservation "bdver1_sseiadd_load" 6
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "sseiadd")
				   (eq_attr "memory" "load")))
			 "bdver1-direct,bdver1-fpload,bdver1-fmal")
(define_insn_reservation "bdver1_sseiadd" 2
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "sseiadd")
				   (eq_attr "memory" "none")))
			 "bdver1-direct,bdver1-fpsched,bdver1-fmal")

;; SSE DIV: no throughput information (assume same as amdfam10).
(define_insn_reservation "bdver1_ssediv_double_load_256" 31
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "ssediv")
				   (and (eq_attr "mode" "V4DF")
				        (eq_attr "memory" "load"))))
			 "bdver1-double,bdver1-fpload,(bdver1-ffma0*17 | bdver1-ffma1*17)")
(define_insn_reservation "bdver1_ssediv_double_256" 27
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "ssediv")
				   (and (eq_attr "mode" "V4DF")
				        (eq_attr "memory" "none"))))
			 "bdver1-double,bdver1-fpsched,(bdver1-ffma0*17 | bdver1-ffma1*17)")
(define_insn_reservation "bdver1_ssediv_single_load_256" 28
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "ssediv")
				   (and (eq_attr "mode" "V8SF")
				        (eq_attr "memory" "load"))))
			 "bdver1-double,bdver1-fpload,(bdver1-ffma0*17 | bdver1-ffma1*17)")
(define_insn_reservation "bdver1_ssediv_single_256" 24
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "ssediv")
				   (and (eq_attr "mode" "V8SF")
				        (eq_attr "memory" "none"))))
			 "bdver1-double,bdver1-fpsched,(bdver1-ffma0*17 | bdver1-ffma1*17)")
(define_insn_reservation "bdver1_ssediv_double_load" 31
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "ssediv")
				   (and (eq_attr "mode" "DF,V2DF")
					(eq_attr "memory" "load"))))
			 "bdver1-direct,bdver1-fpload,(bdver1-ffma0*17 | bdver1-ffma1*17)")
(define_insn_reservation "bdver1_ssediv_double" 27
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "ssediv")
				   (and (eq_attr "mode" "DF,V2DF")
					(eq_attr "memory" "none"))))
			 "bdver1-direct,bdver1-fpsched,(bdver1-ffma0*17 | bdver1-ffma1*17)")
(define_insn_reservation "bdver1_ssediv_single_load" 28
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "ssediv")
				   (and (eq_attr "mode" "SF,V4SF")
					(eq_attr "memory" "load"))))
			 "bdver1-direct,bdver1-fpload,(bdver1-ffma0*17 | bdver1-ffma1*17)")
(define_insn_reservation "bdver1_ssediv_single" 24
			 (and (eq_attr "cpu" "bdver1,bdver2")
			      (and (eq_attr "type" "ssediv")
				   (and (eq_attr "mode" "SF,V4SF")
					(eq_attr "memory" "none"))))
			 "bdver1-direct,bdver1-fpsched,(bdver1-ffma0*17 | bdver1-ffma1*17)")

(define_insn_reservation "bdver1_sseins" 3
                         (and (eq_attr "cpu" "bdver1,bdver2")
                              (and (eq_attr "type" "sseins")
                                   (eq_attr "mode" "TI")))
                         "bdver1-direct,bdver1-fpsched,bdver1-fxbar")

