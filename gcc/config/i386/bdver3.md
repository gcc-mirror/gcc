;; Copyright (C) 2012-2024 Free Software Foundation, Inc.
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
;; AMD bdver3 and bdver4 Scheduling
;;
;; The bdver3 and bdver4 contains three pipelined FP units and two integer
;; units. ;; Fetching and decoding logic is different from previous fam15 
;; processors. Fetching is done every two cycles rather than every cycle 
;; and two decode units are available. The decode units therefore decode
;; four instructions in two cycles.
;;
;; The load/store queue unit is not attached to the schedulers but
;; communicates with all the execution units separately instead.
;;
;; bdver3 and bdver4 belong to fam15 processors. We use the same insn 
;; attribute that was used for bdver1 decoding scheme.

(define_automaton "bdver3,bdver3_ieu,bdver3_load,bdver3_fp,bdver3_agu")

(define_cpu_unit "bdver3-decode0" "bdver3")
(define_cpu_unit "bdver3-decode1" "bdver3")
(define_cpu_unit "bdver3-decode2" "bdver3")
(define_cpu_unit "bdver3-decode3" "bdver3")

;; Double decoded instructions take two cycles whereas
;; direct instructions take one cycle.
;; Vectorpath instructions are single issue instructions.
;; So, we engage all units vector instructions.
(define_reservation "bdver3-vector" "bdver3-decode0+bdver3-decode1+bdver3-decode2+bdver3-decode3")

;; Direct instructions can be issued to any of the four decoders
(define_reservation "bdver3-direct" "(bdver3-decode0|bdver3-decode1|bdver3-decode2|bdver3-decode3)")

;; Double instructions take two cycles to decode.
(define_reservation "bdver3-double" "(bdver3-decode0,bdver3-decode0)|
               (bdver3-decode1,bdver3-decode1)| (bdver3-decode2,bdver3-decode2)|
               (bdver3-decode3,bdver3-decode3)")

(define_cpu_unit "bdver3-ieu0" "bdver3_ieu")
(define_cpu_unit "bdver3-ieu1" "bdver3_ieu")
(define_reservation "bdver3-ieu" "(bdver3-ieu0|bdver3-ieu1)")

(define_cpu_unit "bdver3-agu0" "bdver3_agu")
(define_cpu_unit "bdver3-agu1" "bdver3_agu")
(define_reservation "bdver3-agu" "(bdver3-agu0|bdver3-agu1)")

(define_cpu_unit "bdver3-load0" "bdver3_load")
(define_cpu_unit "bdver3-load1" "bdver3_load")
(define_reservation "bdver3-load" "bdver3-agu,
				   (bdver3-load0|bdver3-load1),nothing")
;; 128bit SSE instructions issue two loads at once.
(define_reservation "bdver3-load2" "bdver3-agu,
				   (bdver3-load0+bdver3-load1),nothing")

(define_reservation "bdver3-store" "(bdver3-load0 | bdver3-load1)")
;; 128bit SSE instructions issue two stores at once.
(define_reservation "bdver3-store2" "(bdver3-load0+bdver3-load1)")

;; vectorpath (microcoded) instructions are single issue instructions.
;; So, they occupy all the integer units.
(define_reservation "bdver3-ivector" "bdver3-ieu0+bdver3-ieu1+
                                      bdver3-agu0+bdver3-agu1+
                                      bdver3-load0+bdver3-load1")

(define_reservation "bdver3-fpsched" "nothing,nothing,nothing")

;; The floating point loads.
(define_reservation "bdver3-fpload" "(bdver3-fpsched + bdver3-load)")
(define_reservation "bdver3-fpload2" "(bdver3-fpsched + bdver3-load2)")

;; Three FP units.
(define_cpu_unit "bdver3-ffma0" "bdver3_fp")
(define_cpu_unit "bdver3-ffma1" "bdver3_fp")
(define_cpu_unit "bdver3-fpsto" "bdver3_fp")

(define_reservation "bdver3-fvector" "bdver3-ffma0+bdver3-ffma1+
                                      bdver3-fpsto+bdver3-load0+
                                      bdver3-load1")

(define_reservation "bdver3-ffma"     "(bdver3-ffma0 | bdver3-ffma1)")
(define_reservation "bdver3-fcvt"     "bdver3-ffma0")
(define_reservation "bdver3-fmma"     "bdver3-ffma0")
(define_reservation "bdver3-fxbar"    "bdver3-ffma1")
(define_reservation "bdver3-fmal"     "(bdver3-ffma0 | bdver3-fpsto)")
(define_reservation "bdver3-fsto"     "bdver3-fpsto")
(define_reservation "bdver3-fpshuf"    "bdver3-fpsto")

;; Jump instructions are executed in the branch unit completely transparent to us.
(define_insn_reservation "bdver3_call" 2
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (eq_attr "type" "call,callv"))
			 "bdver3-double,(bdver3-agu | bdver3-ieu),nothing")
;; PUSH mem is double path.
(define_insn_reservation "bdver3_push" 1
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (eq_attr "type" "push"))
			 "bdver3-direct,bdver3-ieu,bdver3-store")
;; POP r16/mem are double path.
(define_insn_reservation "bdver3_pop" 1
                         (and (eq_attr "cpu" "bdver3,bdver4")
                              (eq_attr "type" "pop"))
                         "bdver3-direct,bdver3-ivector")
;; LEAVE no latency info so far, assume same with amdfam10.
(define_insn_reservation "bdver3_leave" 3
                         (and (eq_attr "cpu" "bdver3,bdver4")
                              (eq_attr "type" "leave"))
                         "bdver3-vector,bdver3-ivector")
;; LEA executes in AGU unit with 1 cycle latency on BDVER3.
(define_insn_reservation "bdver3_lea" 1
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (eq_attr "type" "lea"))
			 "bdver3-direct,bdver3-ieu")
;; MUL executes in special multiplier unit attached to IEU1.
(define_insn_reservation "bdver3_imul_DI" 6
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "imul")
				   (and (eq_attr "mode" "DI")
					(eq_attr "memory" "none,unknown"))))
			 "bdver3-direct,bdver3-ieu1")
(define_insn_reservation "bdver3_imul" 4
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "imul")
				   (eq_attr "memory" "none,unknown")))
			 "bdver3-direct,bdver3-ieu1")
(define_insn_reservation "bdver3_imul_mem_DI" 10
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "imul")
				   (and (eq_attr "mode" "DI")
					(eq_attr "memory" "load,both"))))
			 "bdver3-direct,bdver3-load,bdver3-ieu1")
(define_insn_reservation "bdver3_imul_mem" 8
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "imul")
				   (eq_attr "memory" "load,both")))
			 "bdver3-direct,bdver3-load,bdver3-ieu1")

(define_insn_reservation "bdver3_str" 6
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "str")
				   (eq_attr "memory" "load,both,store")))
			 "bdver3-vector,bdver3-load,bdver3-ivector")

;; Integer instructions.
(define_insn_reservation "bdver3_idirect" 1
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "bdver1_decode" "direct")
				   (and (eq_attr "unit" "integer,unknown")
					(eq_attr "memory" "none,unknown"))))
			 "bdver3-direct,(bdver3-ieu|bdver3-agu)")
(define_insn_reservation "bdver3_ivector" 2
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "bdver1_decode" "vector")
				   (and (eq_attr "unit" "integer,unknown")
					(eq_attr "memory" "none,unknown"))))
			 "bdver3-vector,bdver3-ivector")
(define_insn_reservation "bdver3_idirect_loadmov" 4
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "imov")
				   (eq_attr "memory" "load")))
			 "bdver3-direct,bdver3-load")
(define_insn_reservation "bdver3_idirect_load" 5
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "bdver1_decode" "direct")
				   (and (eq_attr "unit" "integer,unknown")
					(eq_attr "memory" "load"))))
			 "bdver3-direct,bdver3-load,bdver3-ieu")
(define_insn_reservation "bdver3_idirect_movstore" 5
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "imov")
				   (eq_attr "memory" "store")))
			 "bdver3-direct,bdver3-ieu,bdver3-store")
(define_insn_reservation "bdver3_idirect_both" 4
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "bdver1_decode" "direct")
				   (and (eq_attr "unit" "integer,unknown")
					(eq_attr "memory" "both"))))
			 "bdver3-direct,bdver3-load,
			  bdver3-ieu,bdver3-store,
			  bdver3-store")
(define_insn_reservation "bdver3_idirect_store" 4
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "bdver1_decode" "direct")
				   (and (eq_attr "unit" "integer,unknown")
					(eq_attr "memory" "store"))))
			 "bdver3-direct,(bdver3-ieu+bdver3-agu),
			  bdver3-store")
;; BDVER3 floating point units.
(define_insn_reservation "bdver3_fldxf" 13
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "fmov")
				   (and (eq_attr "memory" "load")
					(eq_attr "mode" "XF"))))
			 "bdver3-vector,bdver3-fpload2,bdver3-fvector*9")
(define_insn_reservation "bdver3_fld" 2
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "fmov")
				   (eq_attr "memory" "load")))
			 "bdver3-direct,bdver3-fpload,bdver3-ffma")
(define_insn_reservation "bdver3_fstxf" 4
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "fmov")
				   (and (eq_attr "memory" "store,both")
					(eq_attr "mode" "XF"))))
			 "bdver3-vector,(bdver3-fpsched+bdver3-agu),(bdver3-store2+(bdver3-fvector*6))")
(define_insn_reservation "bdver3_fst" 2
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "fmov")
				   (eq_attr "memory" "store,both")))
			 "bdver3-double,(bdver3-fpsched),(bdver3-fsto+bdver3-store)")
(define_insn_reservation "bdver3_fist" 2
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (eq_attr "type" "fistp,fisttp"))
			 "bdver3-double,(bdver3-fpsched),(bdver3-fsto+bdver3-store)")
(define_insn_reservation "bdver3_fmov_bdver3" 2
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (eq_attr "type" "fmov"))
			 "bdver3-direct,bdver3-fpsched,bdver3-ffma")
(define_insn_reservation "bdver3_fadd_load" 10
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "fop")
				   (eq_attr "memory" "load")))
			 "bdver3-direct,bdver3-fpload,bdver3-ffma")
(define_insn_reservation "bdver3_fadd" 6
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (eq_attr "type" "fop"))
			 "bdver3-direct,bdver3-fpsched,bdver3-ffma")
(define_insn_reservation "bdver3_fmul_load" 6
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "fmul")
				   (eq_attr "memory" "load")))
			 "bdver3-double,bdver3-fpload,bdver3-ffma")
(define_insn_reservation "bdver3_fmul" 6
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (eq_attr "type" "fmul"))
			 "bdver3-direct,bdver3-fpsched,bdver3-ffma")
(define_insn_reservation "bdver3_fsgn" 2
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (eq_attr "type" "fsgn"))
			 "bdver3-direct,bdver3-fpsched,bdver3-ffma")
(define_insn_reservation "bdver3_fdiv_load" 42
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "fdiv")
				   (eq_attr "memory" "load")))
			 "bdver3-direct,bdver3-fpload,bdver3-ffma")
(define_insn_reservation "bdver3_fdiv" 42
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (eq_attr "type" "fdiv"))
			 "bdver3-direct,bdver3-fpsched,bdver3-ffma")
(define_insn_reservation "bdver3_fpspc_load" 143
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "fpspc")
				   (eq_attr "memory" "load")))
			 "bdver3-vector,bdver3-fpload,bdver3-fvector")
(define_insn_reservation "bdver3_fcmov_load" 17
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "fcmov")
				   (eq_attr "memory" "load")))
			 "bdver3-vector,bdver3-fpload,bdver3-fvector")
(define_insn_reservation "bdver3_fcmov" 15
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (eq_attr "type" "fcmov"))
			 "bdver3-vector,bdver3-fpsched,bdver3-fvector")
(define_insn_reservation "bdver3_fcomi_load" 6
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "fcmp")
				   (and (eq_attr "bdver1_decode" "double")
					(eq_attr "memory" "load"))))
			 "bdver3-double,bdver3-fpload,(bdver3-ffma | bdver3-fsto)")
(define_insn_reservation "bdver3_fcomi" 2
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "bdver1_decode" "double")
				   (eq_attr "type" "fcmp")))
			 "bdver3-double,bdver3-fpsched,(bdver3-ffma | bdver3-fsto)")
(define_insn_reservation "bdver3_fcom_load" 6
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "fcmp")
				   (eq_attr "memory" "load")))
			 "bdver3-direct,bdver3-fpload,bdver3-ffma")
(define_insn_reservation "bdver3_fcom" 2
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (eq_attr "type" "fcmp"))
			 "bdver3-direct,bdver3-fpsched,bdver3-ffma")
(define_insn_reservation "bdver3_fxch" 2
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (eq_attr "type" "fxch"))
			 "bdver3-direct,bdver3-fpsched,bdver3-ffma")

;; SSE loads.
(define_insn_reservation "bdver3_ssevector_avx128_unaligned_load" 4
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "ssemov")
				   (and (eq_attr "prefix" "vex")
					(and (eq_attr "movu" "1")
					     (and (eq_attr "mode" "V4SF,V2DF")
						  (eq_attr "memory" "load"))))))
			 "bdver3-direct,bdver3-fpload")
(define_insn_reservation "bdver3_ssevector_avx256_unaligned_load" 5
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "ssemov")
				   (and (eq_attr "movu" "1")
				        (and (eq_attr "mode" "V8SF,V4DF")
				             (eq_attr "memory" "load")))))
			 "bdver3-double,bdver3-fpload")
(define_insn_reservation "bdver3_ssevector_sse128_unaligned_load" 4
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "ssemov")
				   (and (eq_attr "movu" "1")
				        (and (eq_attr "mode" "V4SF,V2DF")
				             (eq_attr "memory" "load")))))
			 "bdver3-direct,bdver3-fpload,bdver3-fmal")
(define_insn_reservation "bdver3_ssevector_avx128_load" 4
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "ssemov")
				   (and (eq_attr "prefix" "vex")
				        (and (eq_attr "mode" "V4SF,V2DF,TI")
				             (eq_attr "memory" "load")))))
			 "bdver3-direct,bdver3-fpload,bdver3-fmal")
(define_insn_reservation "bdver3_ssevector_avx256_load" 5
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "ssemov")
				   (and (eq_attr "mode" "V8SF,V4DF,OI")
				        (eq_attr "memory" "load"))))
			 "bdver3-double,bdver3-fpload,bdver3-fmal")
(define_insn_reservation "bdver3_ssevector_sse128_load" 4
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "ssemov")
				   (and (eq_attr "mode" "V4SF,V2DF,TI")
				        (eq_attr "memory" "load"))))
			 "bdver3-direct,bdver3-fpload")
(define_insn_reservation "bdver3_ssescalar_movq_load" 4
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "ssemov")
				   (and (eq_attr "mode" "DI")
				        (eq_attr "memory" "load"))))
			 "bdver3-direct,bdver3-fpload,bdver3-fmal")
(define_insn_reservation "bdver3_ssescalar_vmovss_load" 4
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "ssemov")
				   (and (eq_attr "prefix" "vex")
				        (and (eq_attr "mode" "SF")
				             (eq_attr "memory" "load")))))
			 "bdver3-direct,bdver3-fpload")
(define_insn_reservation "bdver3_ssescalar_sse128_load" 4
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "ssemov")
				   (and (eq_attr "mode" "SF,DF")
				        (eq_attr "memory" "load"))))
			 "bdver3-direct,bdver3-fpload, bdver3-ffma")
(define_insn_reservation "bdver3_mmxsse_load" 4
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "mmxmov,ssemov")
				   (eq_attr "memory" "load")))
			 "bdver3-direct,bdver3-fpload, bdver3-fmal")

;; SSE stores.
(define_insn_reservation "bdver3_sse_store_avx256" 5
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "ssemov")
				   (and (eq_attr "mode" "V8SF,V4DF,OI")
					(eq_attr "memory" "store,both"))))
			 "bdver3-double,bdver3-fpsched,((bdver3-fsto+bdver3-store)*2)")
(define_insn_reservation "bdver3_sse_store" 4
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "ssemov")
				   (and (eq_attr "mode" "V4SF,V2DF,TI")
					(eq_attr "memory" "store,both"))))
			 "bdver3-direct,bdver3-fpsched,((bdver3-fsto+bdver3-store)*2)")
(define_insn_reservation "bdver3_mmxsse_store_short" 4
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "mmxmov,ssemov")
				   (eq_attr "memory" "store,both")))
			 "bdver3-direct,bdver3-fpsched,(bdver3-fsto+bdver3-store)")

;; Register moves.
(define_insn_reservation "bdver3_ssevector_avx256" 3
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "ssemov")
				   (and (eq_attr "mode" "V8SF,V4DF,OI")
					(eq_attr "memory" "none"))))
			 "bdver3-double,bdver3-fpsched,bdver3-fmal")
(define_insn_reservation "bdver3_movss_movsd" 2
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "ssemov")
				   (and (eq_attr "mode" "SF,DF")
                                        (eq_attr "memory" "none"))))
			 "bdver3-direct,bdver3-fpsched,bdver3-ffma")
(define_insn_reservation "bdver3_mmxssemov" 2
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "mmxmov,ssemov")
				   (eq_attr "memory" "none")))
			 "bdver3-direct,bdver3-fpsched,bdver3-fmal")
;; SSE logs.
(define_insn_reservation "bdver3_sselog_load_256" 7
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "sselog,sselog1")
				   (and (eq_attr "mode" "V8SF")
				   (eq_attr "memory" "load"))))
			 "bdver3-double,bdver3-fpload,bdver3-fmal")
(define_insn_reservation "bdver3_sselog_256" 3
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "sselog,sselog1")
                                   (eq_attr "mode" "V8SF")))
			 "bdver3-double,bdver3-fpsched,bdver3-fmal")
(define_insn_reservation "bdver3_sselog_load" 6
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "sselog,sselog1")
				   (eq_attr "memory" "load")))
			 "bdver3-direct,bdver3-fpload,bdver3-fxbar")
(define_insn_reservation "bdver3_sselog" 2
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (eq_attr "type" "sselog,sselog1"))
			 "bdver3-direct,bdver3-fpsched,bdver3-fxbar")

;; SSE Shuffles
(define_insn_reservation "bdver3_sseshuf_load_256" 7
                         (and (eq_attr "cpu" "bdver3,bdver4")
                              (and (eq_attr "type" "sseshuf,sseshuf1")
                                   (and (eq_attr "mode" "V8SF")
                                   (eq_attr "memory" "load"))))
                         "bdver3-double,bdver3-fpload,bdver3-fpshuf")
(define_insn_reservation "bdver3_sseshuf_load" 6
                         (and (eq_attr "cpu" "bdver3,bdver4")
                              (and (eq_attr "type" "sseshuf,sseshuf1")
                                   (eq_attr "memory" "load")))
                         "bdver3-direct,bdver3-fpload,bdver3-fpshuf")

(define_insn_reservation "bdver3_sseshuf_256" 3
                         (and (eq_attr "cpu" "bdver3,bdver4")
                              (and (eq_attr "type" "sseshuf")
                                   (eq_attr "mode" "V8SF")))
                         "bdver3-double,bdver3-fpsched,bdver3-fpshuf")
(define_insn_reservation "bdver3_sseshuf" 2
                         (and (eq_attr "cpu" "bdver3,bdver4")
                              (eq_attr "type" "sseshuf,sseshuf1"))
                         "bdver3-direct,bdver3-fpsched,bdver3-fpshuf")

;; PCMP actually executes in FMAL.
(define_insn_reservation "bdver3_ssecmp_load" 6
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "ssecmp")
				   (eq_attr "memory" "load")))
			 "bdver3-direct,bdver3-fpload,bdver3-ffma")
(define_insn_reservation "bdver3_ssecmp" 2
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (eq_attr "type" "ssecmp"))
			 "bdver3-direct,bdver3-fpsched,bdver3-ffma")
(define_insn_reservation "bdver3_ssecomi_load" 6
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "ssecomi")
				   (eq_attr "memory" "load")))
			 "bdver3-double,bdver3-fpload,(bdver3-ffma | bdver3-fsto)")
(define_insn_reservation "bdver3_ssecomi" 2
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (eq_attr "type" "ssecomi"))
			 "bdver3-double,bdver3-fpsched,(bdver3-ffma | bdver3-fsto)")

;; Conversions behaves very irregularly and the scheduling is critical here.
;; Take each instruction separately.

;; 256 bit conversion.
(define_insn_reservation "bdver3_vcvtX2Y_avx256_load" 8
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "ssecvt")
				   (and (eq_attr "memory" "load")
					(ior (ior (match_operand:V4DF 0 "register_operand")
					          (ior (match_operand:V8SF 0 "register_operand")
						       (match_operand:V8SI 0 "register_operand")))
					     (ior (match_operand:V4DF 1 "nonimmediate_operand")
						  (ior (match_operand:V8SF 1 "nonimmediate_operand")
						       (match_operand:V8SI 1 "nonimmediate_operand")))))))
			 "bdver3-vector,bdver3-fpload,bdver3-fvector")
(define_insn_reservation "bdver3_vcvtX2Y_avx256" 4
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "ssecvt")
				   (and (eq_attr "memory" "none")
					(ior (ior (match_operand:V4DF 0 "register_operand")
					          (ior (match_operand:V8SF 0 "register_operand")
						       (match_operand:V8SI 0 "register_operand")))
					     (ior (match_operand:V4DF 1 "nonimmediate_operand")
						  (ior (match_operand:V8SF 1 "nonimmediate_operand")
						       (match_operand:V8SI 1 "nonimmediate_operand")))))))
			 "bdver3-vector,bdver3-fpsched,bdver3-fvector")
;; CVTSS2SD, CVTSD2SS.
(define_insn_reservation "bdver3_ssecvt_cvtss2sd_load" 8
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "ssecvt")
				   (and (eq_attr "mode" "SF,DF")
					(eq_attr "memory" "load"))))
			 "bdver3-direct,bdver3-fpload,bdver3-fcvt")
(define_insn_reservation "bdver3_ssecvt_cvtss2sd" 4
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "ssecvt")
				   (and (eq_attr "mode" "SF,DF")
					(eq_attr "memory" "none"))))
			 "bdver3-direct,bdver3-fpsched,bdver3-fcvt")
;; CVTSI2SD, CVTSI2SS, CVTSI2SDQ, CVTSI2SSQ.
(define_insn_reservation "bdver3_sseicvt_cvtsi2sd_load" 8
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "sseicvt")
				   (and (eq_attr "mode" "SF,DF")
					(eq_attr "memory" "load"))))
			 "bdver3-direct,bdver3-fpload,bdver3-fcvt")
(define_insn_reservation "bdver3_sseicvt_cvtsi2sd" 4
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "sseicvt")
				   (and (eq_attr "mode" "SF,DF")
					(eq_attr "memory" "none"))))
			 "bdver3-double,bdver3-fpsched,(nothing | bdver3-fcvt)")
;; CVTPD2PS.
(define_insn_reservation "bdver3_ssecvt_cvtpd2ps_load" 8
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "ssecvt")
				   (and (eq_attr "memory" "load")
                                        (and (match_operand:V4SF 0 "register_operand")
					     (match_operand:V2DF 1 "nonimmediate_operand")))))
			 "bdver3-double,bdver3-fpload,(bdver3-fxbar | bdver3-fcvt)")
(define_insn_reservation "bdver3_ssecvt_cvtpd2ps" 4
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "ssecvt")
				   (and (eq_attr "memory" "none")
                                        (and (match_operand:V4SF 0 "register_operand")
					     (match_operand:V2DF 1 "nonimmediate_operand")))))
			 "bdver3-double,bdver3-fpsched,(bdver3-fxbar | bdver3-fcvt)")
;; CVTPI2PS, CVTDQ2PS.
(define_insn_reservation "bdver3_ssecvt_cvtdq2ps_load" 8
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "ssecvt")
				   (and (eq_attr "memory" "load")
                                        (and (match_operand:V4SF 0 "register_operand")
					     (ior (match_operand:V2SI 1 "nonimmediate_operand")
					          (match_operand:V4SI 1 "nonimmediate_operand"))))))
			 "bdver3-direct,bdver3-fpload,bdver3-fcvt")
(define_insn_reservation "bdver3_ssecvt_cvtdq2ps" 4
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "ssecvt")
				   (and (eq_attr "memory" "none")
                                        (and (match_operand:V4SF 0 "register_operand")
					     (ior (match_operand:V2SI 1 "nonimmediate_operand")
					          (match_operand:V4SI 1 "nonimmediate_operand"))))))
			 "bdver3-direct,bdver3-fpsched,bdver3-fcvt")
;; CVTDQ2PD.
(define_insn_reservation "bdver3_ssecvt_cvtdq2pd_load" 8
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "ssecvt")
				   (and (eq_attr "memory" "load")
                                        (and (match_operand:V2DF 0 "register_operand")
					     (match_operand:V4SI 1 "nonimmediate_operand")))))
			 "bdver3-double,bdver3-fpload,(bdver3-fxbar | bdver3-fcvt)")
(define_insn_reservation "bdver3_ssecvt_cvtdq2pd" 4
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "ssecvt")
				   (and (eq_attr "memory" "none")
                                        (and (match_operand:V2DF 0 "register_operand")
					     (match_operand:V4SI 1 "nonimmediate_operand")))))
			 "bdver3-double,bdver3-fpsched,(bdver3-fxbar | bdver3-fcvt)")
;; CVTPS2PD, CVTPI2PD.
(define_insn_reservation "bdver3_ssecvt_cvtps2pd_load" 6
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "ssecvt")
				   (and (eq_attr "memory" "load")
                                        (and (match_operand:V2DF 0 "register_operand")
					     (ior (match_operand:V2SI 1 "nonimmediate_operand")
					          (match_operand:V4SF 1 "nonimmediate_operand"))))))
			 "bdver3-double,bdver3-fpload,(bdver3-fxbar | bdver3-fcvt)")
(define_insn_reservation "bdver3_ssecvt_cvtps2pd" 2
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "ssecvt")
				   (and (eq_attr "memory" "load")
                                        (and (match_operand:V2DF 0 "register_operand")
					     (ior (match_operand:V2SI 1 "nonimmediate_operand")
					          (match_operand:V4SF 1 "nonimmediate_operand"))))))
			 "bdver3-double,bdver3-fpsched,(bdver3-fxbar | bdver3-fcvt)")
;; CVTSD2SI, CVTSD2SIQ, CVTSS2SI, CVTSS2SIQ, CVTTSD2SI, CVTTSD2SIQ, CVTTSS2SI, CVTTSS2SIQ.
(define_insn_reservation "bdver3_ssecvt_cvtsX2si_load" 8
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "sseicvt")
				   (and (eq_attr "mode" "SI,DI")
					(eq_attr "memory" "load"))))
			 "bdver3-double,bdver3-fpload,(bdver3-fcvt | bdver3-fsto)")
(define_insn_reservation "bdver3_ssecvt_cvtsX2si" 4
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "sseicvt")
				   (and (eq_attr "mode" "SI,DI")
					(eq_attr "memory" "none"))))
			 "bdver3-double,bdver3-fpsched,(bdver3-fcvt | bdver3-fsto)")
;; CVTPD2PI, CVTTPD2PI.
(define_insn_reservation "bdver3_ssecvt_cvtpd2pi_load" 8
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "ssecvt")
				   (and (eq_attr "memory" "load")
				        (and (match_operand:V2DF 1 "nonimmediate_operand")
					     (match_operand:V2SI 0 "register_operand")))))
			 "bdver3-double,bdver3-fpload,(bdver3-fcvt | bdver3-fxbar)")
(define_insn_reservation "bdver3_ssecvt_cvtpd2pi" 4
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "ssecvt")
				   (and (eq_attr "memory" "none")
				        (and (match_operand:V2DF 1 "nonimmediate_operand")
					     (match_operand:V2SI 0 "register_operand")))))
			 "bdver3-double,bdver3-fpsched,(bdver3-fcvt | bdver3-fxbar)")
;; CVTPD2DQ, CVTTPD2DQ.
(define_insn_reservation "bdver3_ssecvt_cvtpd2dq_load" 6
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "ssecvt")
				   (and (eq_attr "memory" "load")
				        (and (match_operand:V2DF 1 "nonimmediate_operand")
					     (match_operand:V4SI 0 "register_operand")))))
			 "bdver3-double,bdver3-fpload,(bdver3-fcvt | bdver3-fxbar)")
(define_insn_reservation "bdver3_ssecvt_cvtpd2dq" 2
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "ssecvt")
				   (and (eq_attr "memory" "none")
				        (and (match_operand:V2DF 1 "nonimmediate_operand")
					     (match_operand:V4SI 0 "register_operand")))))
			 "bdver3-double,bdver3-fpsched,(bdver3-fcvt | bdver3-fxbar)")
;; CVTPS2PI, CVTTPS2PI, CVTPS2DQ, CVTTPS2DQ.
(define_insn_reservation "bdver3_ssecvt_cvtps2pi_load" 8
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "ssecvt")
                                   (and (eq_attr "memory" "load")
				        (and (match_operand:V4SF 1 "nonimmediate_operand")
				             (ior (match_operand: V2SI 0 "register_operand")
						  (match_operand: V4SI 0 "register_operand"))))))
			 "bdver3-direct,bdver3-fpload,bdver3-fcvt")
(define_insn_reservation "bdver3_ssecvt_cvtps2pi" 4
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "ssecvt")
				   (and (eq_attr "memory" "none")
				        (and (match_operand:V4SF 1 "nonimmediate_operand")
				             (ior (match_operand: V2SI 0 "register_operand")
						  (match_operand: V4SI 0 "register_operand"))))))
			 "bdver3-direct,bdver3-fpsched,bdver3-fcvt")

;; SSE MUL, ADD, and MULADD.
(define_insn_reservation "bdver3_ssemuladd_load_256" 11
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "ssemul,sseadd,sseadd1,ssemuladd")
				   (and (eq_attr "mode" "V8SF,V4DF")
					(eq_attr "memory" "load"))))
			 "bdver3-double,bdver3-fpload,bdver3-ffma")
(define_insn_reservation "bdver3_ssemuladd_256" 7
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "ssemul,sseadd,sseadd1,ssemuladd")
				   (and (eq_attr "mode" "V8SF,V4DF")
					(eq_attr "memory" "none"))))
			 "bdver3-double,bdver3-fpsched,bdver3-ffma")
(define_insn_reservation "bdver3_ssemuladd_load" 10
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "ssemul,sseadd,sseadd1,ssemuladd")
				   (eq_attr "memory" "load")))
			 "bdver3-direct,bdver3-fpload,bdver3-ffma")
(define_insn_reservation "bdver3_ssemuladd" 6
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "ssemul,sseadd,sseadd1,ssemuladd")
				   (eq_attr "memory" "none")))
			 "bdver3-direct,bdver3-fpsched,bdver3-ffma")
(define_insn_reservation "bdver3_sseimul_load" 8
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "sseimul")
				   (eq_attr "memory" "load")))
			 "bdver3-direct,bdver3-fpload,bdver3-fmma")
(define_insn_reservation "bdver3_sseimul" 4
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "sseimul")
				   (eq_attr "memory" "none")))
			 "bdver3-direct,bdver3-fpsched,bdver3-fmma")
(define_insn_reservation "bdver3_sseiadd_load" 6
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "sseiadd")
				   (eq_attr "memory" "load")))
			 "bdver3-direct,bdver3-fpload,bdver3-fmal")
(define_insn_reservation "bdver3_sseiadd" 2
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "sseiadd")
				   (eq_attr "memory" "none")))
			 "bdver3-direct,bdver3-fpsched,bdver3-fmal")

;; SSE DIV: no throughput information (assume same as amdfam10).
(define_insn_reservation "bdver3_ssediv_double_load_256" 27
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "ssediv")
				   (and (eq_attr "mode" "V4DF")
				        (eq_attr "memory" "load"))))
			 "bdver3-double,bdver3-fpload,(bdver3-ffma0*17 | bdver3-ffma1*17)")
(define_insn_reservation "bdver3_ssediv_double_256" 27
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "ssediv")
				   (and (eq_attr "mode" "V4DF")
				        (eq_attr "memory" "none"))))
			 "bdver3-double,bdver3-fpsched,(bdver3-ffma0*17 | bdver3-ffma1*17)")
(define_insn_reservation "bdver3_ssediv_single_load_256" 27
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "ssediv")
				   (and (eq_attr "mode" "V8SF")
				        (eq_attr "memory" "load"))))
			 "bdver3-double,bdver3-fpload,(bdver3-ffma0*17 | bdver3-ffma1*17)")
(define_insn_reservation "bdver3_ssediv_single_256" 24
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "ssediv")
				   (and (eq_attr "mode" "V8SF")
				        (eq_attr "memory" "none"))))
			 "bdver3-double,bdver3-fpsched,(bdver3-ffma0*17 | bdver3-ffma1*17)")
(define_insn_reservation "bdver3_ssediv_double_load" 27
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "ssediv")
				   (and (eq_attr "mode" "DF,V2DF")
					(eq_attr "memory" "load"))))
			 "bdver3-direct,bdver3-fpload,(bdver3-ffma0*17 | bdver3-ffma1*17)")
(define_insn_reservation "bdver3_ssediv_double" 27
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "ssediv")
				   (and (eq_attr "mode" "DF,V2DF")
					(eq_attr "memory" "none"))))
			 "bdver3-direct,bdver3-fpsched,(bdver3-ffma0*17 | bdver3-ffma1*17)")
(define_insn_reservation "bdver3_ssediv_single_load" 27 
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "ssediv")
				   (and (eq_attr "mode" "SF,V4SF")
					(eq_attr "memory" "load"))))
			 "bdver3-direct,bdver3-fpload,(bdver3-ffma0*17 | bdver3-ffma1*17)")
(define_insn_reservation "bdver3_ssediv_single" 24
			 (and (eq_attr "cpu" "bdver3,bdver4")
			      (and (eq_attr "type" "ssediv")
				   (and (eq_attr "mode" "SF,V4SF")
					(eq_attr "memory" "none"))))
			 "bdver3-direct,bdver3-fpsched,(bdver3-ffma0*17 | bdver3-ffma1*17)")

(define_insn_reservation "bdver3_sseins" 3
                         (and (eq_attr "cpu" "bdver3,bdver4")
                              (and (eq_attr "type" "sseins")
                                   (eq_attr "mode" "TI")))
                         "bdver3-direct,bdver3-fpsched,bdver3-fxbar")

