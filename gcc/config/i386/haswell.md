;; Scheduling for Haswell and derived processors.
;; Copyright (C) 2004-2023 Free Software Foundation, Inc.
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
;; <http://www.gnu.org/licenses/>.  */

;; The scheduling description in this file is based on core2.md.
;; The major difference from the CORE2 pipeline is that HASWELL has
;; two MU for load and one MU for store.
(define_automaton "haswell_decoder,haswell_core,haswell_idiv,haswell_fdiv,haswell_ssediv,haswell_load,haswell_store")

;; The CPU domain, used for HASWELL bypass latencies
(define_attr "hsw_domain" "int,float,simd"
  (cond [(eq_attr "type" "fmov,fop,fsgn,fmul,fdiv,fpspc,fcmov,fcmp,fxch,fistp,fisttp,frndint")
	   (const_string "float")
	 (eq_attr "type" "sselog,sselog1,sseiadd,sseiadd1,sseishft,sseishft1,sseimul,
			  sse,ssemov,sseadd,sseadd1,ssemul,ssecmp,ssecomi,ssecvt,
			  ssecvt1,sseicvt,ssediv,sseins,ssemuladd,sse4arg")
	   (cond [(eq_attr "mode" "V4DF,V8SF,V2DF,V4SF,SF,DF")
		    (const_string "float")
		  (eq_attr "mode" "SI")
		    (const_string "int")]
		  (const_string "simd"))
	 (eq_attr "type" "mmx,mmxmov,mmxadd,mmxmul,mmxcmp,mmxcvt,mmxshft")
	   (const_string "simd")]
	(const_string "int")))

(define_cpu_unit "hsw_decoder0" "haswell_decoder")
(define_cpu_unit "hsw_decoder1" "haswell_decoder")
(define_cpu_unit "hsw_decoder2" "haswell_decoder")
(define_cpu_unit "hsw_decoder3" "haswell_decoder")

;; We first wish to find an instruction for hsw_decoder0, so exclude
;; other hsw_decoders from being reserved until hsw_decoder0 is
;; reserved.
(presence_set "hsw_decoder1" "hsw_decoder0")
(presence_set "hsw_decoder2" "hsw_decoder0")
(presence_set "hsw_decoder3" "hsw_decoder0")

;; Most instructions can be decoded on any of the three decoders.
(define_reservation "hsw_decodern" "(hsw_decoder0|hsw_decoder1|hsw_decoder2|hsw_decoder3)")

;; The out-of-order core has eight pipelines.  These are similar to the
;; Pentium Pro's five pipelines.  Port 2,3 are responsible for memory loads,
;; port 7 for store address calculations, port 4 for memory stores, and
;; ports 0, 1, 5 and 6 for everything else.

(define_cpu_unit "hsw_p0,hsw_p1,hsw_p5,hsw_p6" "haswell_core")
(define_cpu_unit "hsw_p2,hsw_p3" "haswell_load")
(define_cpu_unit "hsw_p4,hsw_p7" "haswell_store")
(define_cpu_unit "hsw_idiv" "haswell_idiv")
(define_cpu_unit "hsw_fdiv" "haswell_fdiv")
(define_cpu_unit "hsw_ssediv" "haswell_ssediv")

(define_reservation "hsw_p0156" "hsw_p0|hsw_p1|hsw_p5|hsw_p6")
(define_reservation "hsw_p0p1p5p6" "hsw_p0+hsw_p1+hsw_p5+hsw_p6")
(define_reservation "hsw_p23" "hsw_p2|hsw_p3")
(define_reservation "hsw_p4p7" "hsw_p4+hsw_p7")
(define_reservation "hsw_p237" "hsw_p2|hsw_p3|hsw_p7")
(define_reservation "hsw_p015" "hsw_p0|hsw_p1|hsw_p5")
(define_reservation "hsw_p01" "hsw_p0|hsw_p1")

(define_insn_reservation "hsw_complex_insn" 6
			 (and (eq_attr "cpu" "generic,haswell")
			      (eq_attr "type" "other,multi,str"))
			 "hsw_decoder0")

(define_insn_reservation "hsw_call" 1
			 (and (eq_attr "cpu" "generic,haswell")
			      (eq_attr "type" "call,callv"))
			 "hsw_decoder0")

;; imov with memory operands does not use the integer units.
;; imovx always decodes to one uop, and also doesn't use the integer
;; units if it has memory operands.
(define_insn_reservation "hsw_imov" 1
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "none")
				   (eq_attr "type" "imov,imovx")))
			 "hsw_decodern,hsw_p0156")

(define_insn_reservation "hsw_imov_load" 2
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "load")
				   (eq_attr "type" "imov,imovx")))
			 "hsw_decodern,hsw_p23")

(define_insn_reservation "hsw_imov_store" 3
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "store")
				   (eq_attr "type" "imov")))
			 "hsw_decodern,hsw_p4+(hsw_p2|hsw_p3|hsw_p7)")

(define_insn_reservation "hsw_icmov" 2
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "none")
				   (eq_attr "type" "icmov")))
			 "hsw_decodern,hsw_p0156,hsw_p0156")

(define_insn_reservation "hsw_icmov_load" 2
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "load")
				   (eq_attr "type" "icmov")))
			 "hsw_decodern,hsw_p23+hsw_p0156,hsw_p0156")

(define_insn_reservation "hsw_push_reg" 3
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "store")
				   (eq_attr "type" "push")))
			 "hsw_decodern,hsw_p4+hsw_p237")

(define_insn_reservation "hsw_push_mem" 3
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "both")
				   (eq_attr "type" "push")))
			 "hsw_decodern,hsw_p4+hsw_p237,hsw_p237")

;; Consider lea latency as having 2 components.
(define_insn_reservation "hsw_lea" 1
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "none")
				   (eq_attr "type" "lea")))
			 "hsw_decodern,hsw_p1|hsw_p5")

(define_insn_reservation "hsw_shift_rotate" 1
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "none")
				   (eq_attr "type" "ishift,ishift1,rotate,rotate1")))
			 "hsw_decodern,hsw_p0|hsw_p6")

(define_insn_reservation "hsw_shift_rotate_mem" 1
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "!none")
				   (eq_attr "type" "ishift,ishift1,rotate,rotate1")))
			 "hsw_decodern,(hsw_p0|hsw_p6)+hsw_p237+hsw_p4")

(define_insn_reservation "hsw_branch" 1
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "none")
				   (eq_attr "type" "ibr")))
			 "hsw_decodern,hsw_p6")

(define_insn_reservation "hsw_indirect_branch" 2
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "!none")
				   (eq_attr "type" "ibr")))
			 "hsw_decoder0,hsw_p23+hsw_p6")

(define_insn_reservation "hsw_leave" 4
			 (and (eq_attr "cpu" "generic,haswell")
			      (eq_attr "type" "leave"))
			 "hsw_decoder0,hsw_p23+hsw_p0156,hsw_p0156")

;; imul and imulx with two/three operands only execute on port 1.
(define_insn_reservation "hsw_imul" 3
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "none")
				   (eq_attr "type" "imul")))
			 "hsw_decodern,hsw_p1")

(define_insn_reservation "hsw_imul_mem" 3
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "!none")
				   (eq_attr "type" "imul")))
			 "hsw_decodern,hsw_p23+hsw_p1")

(define_insn_reservation "hsw_imulx" 4
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "none")
				   (eq_attr "type" "imulx")))
			 "hsw_decodern,hsw_p0156,hsw_p0156")

(define_insn_reservation "hsw_imulx_mem" 4
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "!none")
				   (eq_attr "type" "imulx")))
			 "hsw_decodern,hsw_p23+hsw_p0156,(hsw_p0|hsw_p6|hsw_p6)")


;; div and idiv are very similar, so we model them the same.
;; Use the same latency for all QI,HI and SI modes.
(define_insn_reservation "hsw_idiv" 23
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "none")
				   (eq_attr "type" "idiv")))
			 "hsw_decoder0,(hsw_p0p1p5p6+hsw_idiv)*9")

(define_insn_reservation "hsw_idiv_load" 23
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "load")
					(eq_attr "type" "idiv")))
			 "hsw_decoder0,hsw_p23+hsw_p0+hsw_idiv,(hsw_p0p1p5p6+hsw_idiv)*9")

;; x87 floating point operations.

(define_insn_reservation "hsw_fxch" 0
			 (and (eq_attr "cpu" "generic,haswell")
			      (eq_attr "type" "fxch"))
			 "hsw_decodern")

(define_insn_reservation "hsw_fop" 3
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "none,unknown")
				   (eq_attr "type" "fop")))
			 "hsw_decodern,hsw_p1")

(define_insn_reservation "hsw_fop_load" 5
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "load")
				   (eq_attr "type" "fop")))
			 "hsw_decodern,hsw_p23+hsw_p1,hsw_p1")

(define_insn_reservation "hsw_fop_store" 3
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "store")
				   (eq_attr "type" "fop")))
			 "hsw_decodern,hsw_p0,hsw_p0,hsw_p0+hsw_p4+hsw_p3")

(define_insn_reservation "hsw_fop_both" 5
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "both")
				   (eq_attr "type" "fop")))
			 "hsw_decodern,hsw_p2+hsw_p0,hsw_p0+hsw_p4+hsw_p3")

(define_insn_reservation "hsw_fsgn" 1
			 (and (eq_attr "cpu" "generic,haswell")
			      (eq_attr "type" "fsgn"))
			 "hsw_decodern,hsw_p0")

(define_insn_reservation "hsw_fistp" 7
			 (and (eq_attr "cpu" "generic,haswell")
			      (eq_attr "type" "fistp"))
			 "hsw_decoder0,hsw_p1+hsw_p4+hsw_p23")

(define_insn_reservation "hsw_fcmov" 2
			 (and (eq_attr "cpu" "generic,haswell")
			      (eq_attr "type" "fcmov"))
			 "hsw_decoder0,hsw_p0+hsw_p5,hsw_p0")

(define_insn_reservation "hsw_fcmp" 1
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "none")
				   (eq_attr "type" "fcmp")))
			 "hsw_decodern,hsw_p1")

(define_insn_reservation "hsw_fcmp_load" 1
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "load")
				   (eq_attr "type" "fcmp")))
			 "hsw_decodern,hsw_p23+hsw_p1")

(define_insn_reservation "hsw_fmov" 1
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "none")
				   (eq_attr "type" "fmov")))
			 "hsw_decodern,hsw_p01")

(define_insn_reservation "hsw_fmov_load" 3
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "load")
				   (and (eq_attr "mode" "!XF")
					(eq_attr "type" "fmov"))))
			 "hsw_decodern,hsw_p23")

(define_insn_reservation "hsw_fmov_XF_load" 3
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "load")
				   (and (eq_attr "mode" "XF")
					(eq_attr "type" "fmov"))))
			 "hsw_decodern,(hsw_p23+hsw_p0)*2")

(define_insn_reservation "hsw_fmov_store" 1
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "store")
				   (and (eq_attr "mode" "!XF")
					(eq_attr "type" "fmov"))))
			 "hsw_decodern,hsw_p4p7")

(define_insn_reservation "hsw_fmov_XF_store" 3
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "store")
				   (and (eq_attr "mode" "XF")
					(eq_attr "type" "fmov"))))
			 "hsw_decodern,hsw_p4p7,hsw_p4p7")

(define_insn_reservation "hsw_fmul" 4
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "none")
				   (eq_attr "type" "fmul")))
			 "hsw_decodern,hsw_p01")

(define_insn_reservation "hsw_fmul_load" 4
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "load")
				   (eq_attr "type" "fmul")))
			 "hsw_decodern,hsw_p23+hsw_p01")

;; fdiv latencies depend on the mode of the operands.  XFmode gives
;; a latency of 38 cycles, DFmode gives 32, and SFmode gives latency 18.
;; Division by a power of 2 takes only 9 cycles, but we cannot model
;; that.  Throughput is equal to latency - 1, which we model using the
;; hsw_div automaton.
(define_insn_reservation "hsw_fdiv_SF" 18
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "none")
				   (and (eq_attr "mode" "SF")
					(eq_attr "type" "fdiv,fpspc"))))
			 "hsw_decodern,hsw_p0+hsw_fdiv,hsw_fdiv*16")

(define_insn_reservation "hsw_fdiv_SF_load" 19
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "load")
				   (and (eq_attr "mode" "SF")
					(eq_attr "type" "fdiv,fpspc"))))
			 "hsw_decodern,hsw_p23+hsw_p0+hsw_fdiv,hsw_fdiv*16")

(define_insn_reservation "hsw_fdiv_DF" 32
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "none")
				   (and (eq_attr "mode" "DF")
					(eq_attr "type" "fdiv,fpspc"))))
			 "hsw_decodern,hsw_p0+hsw_fdiv,hsw_fdiv*30")

(define_insn_reservation "hsw_fdiv_DF_load" 33
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "load")
				   (and (eq_attr "mode" "DF")
					(eq_attr "type" "fdiv,fpspc"))))
			 "hsw_decodern,hsw_p23+hsw_p0+hsw_fdiv,hsw_fdiv*30")

(define_insn_reservation "hsw_fdiv_XF" 38
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "none")
				   (and (eq_attr "mode" "XF")
					(eq_attr "type" "fdiv,fpspc"))))
			 "hsw_decodern,hsw_p0+hsw_fdiv,hsw_fdiv*36")

(define_insn_reservation "hsw_fdiv_XF_load" 39
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "load")
				   (and (eq_attr "mode" "XF")
					(eq_attr "type" "fdiv,fpspc"))))
			 "hsw_decodern,hsw_p2+hsw_p0+hsw_fdiv,hsw_fdiv*36")

;; MMX instructions.

(define_insn_reservation "hsw_mmx_add" 1
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "none")
				   (eq_attr "type" "mmxadd,sseiadd")))
			 "hsw_decodern,hsw_p1|hsw_p5")

(define_insn_reservation "hsw_mmx_add_load" 2
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "load")
				   (eq_attr "type" "mmxadd,sseiadd")))
			 "hsw_decodern,hsw_p23+(hsw_p1|hsw_p5)")

(define_insn_reservation "hsw_mmx_shft" 1
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "none")
				   (eq_attr "type" "mmxshft")))
			 "hsw_decodern,hsw_p0")

(define_insn_reservation "hsw_mmx_shft_load" 2
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "load")
				   (eq_attr "type" "mmxshft")))
			 "hsw_decodern,hsw_p23+hsw_p0")

(define_insn_reservation "hsw_mmx_sse_shft" 1
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "none")
				   (and (eq_attr "type" "sseishft")
					(eq_attr "length_immediate" "!0"))))
			 "hsw_decodern,hsw_p01")

(define_insn_reservation "hsw_mmx_sse_shft_load" 2
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "load")
				   (and (eq_attr "type" "sseishft")
					(eq_attr "length_immediate" "!0"))))
			 "hsw_decodern,hsw_p01+hsw_p23")

(define_insn_reservation "hsw_mmx_sse_shft1" 2
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "none")
				   (and (eq_attr "type" "sseishft")
					(eq_attr "length_immediate" "0"))))
			 "hsw_decodern,hsw_p01")

(define_insn_reservation "hsw_mmx_sse_shft1_load" 3
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "load")
				   (and (eq_attr "type" "sseishft")
					(eq_attr "length_immediate" "0"))))
			 "hsw_decodern,hsw_p01+hsw_p23")

(define_insn_reservation "hsw_mmx_mul" 5
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "none")
				   (eq_attr "type" "mmxmul,sseimul")))
			 "hsw_decodern,hsw_p01")

(define_insn_reservation "hsw_mmx_mul_load" 5
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "none")
				   (eq_attr "type" "mmxmul,sseimul")))
			 "hsw_decodern,hsw_p23+hsw_p01")

(define_insn_reservation "hsw_sse_mmxcvt" 4
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "mode" "DI")
				   (eq_attr "type" "mmxcvt")))
			 "hsw_decodern,hsw_p1")

;; (define_insn_reservation "hsw_sse_mmxshft" 2
;;			 (and (eq_attr "cpu" "generic,haswell")
;;			      (and (eq_attr "mode" "TI")
;;				   (eq_attr "type" "mmxshft")))
;;			 "hsw_decodern,hsw_p01")

;; The sfence instruction.
(define_insn_reservation "hsw_sse_sfence" 2
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "unknown")
				   (eq_attr "type" "sse")))
			 "hsw_decoder0,hsw_p23+hsw_p4")

(define_insn_reservation "hsw_sse_SFDF" 3
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "mode" "SF,DF")
				   (eq_attr "type" "sse")))
			 "hsw_decodern,hsw_p01")

(define_insn_reservation "hsw_sse_V4SF" 4
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "mode" "V4SF")
				   (eq_attr "type" "sse")))
			 "hsw_decodern,hsw_p01")

(define_insn_reservation "hsw_sse_V8SF" 4
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "mode" "V8SF,V4DF")
				   (eq_attr "type" "sse")))
			 "hsw_decodern,hsw_p01")

(define_insn_reservation "hsw_sse_addcmp" 3
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "none")
				   (eq_attr "type" "sseadd1,ssecmp,ssecomi")))
			 "hsw_decodern,hsw_p01")

(define_insn_reservation "hsw_sse_addcmp_load" 3
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "load")
				   (eq_attr "type" "sseadd1,ssecmp,ssecomi")))
			 "hsw_decodern,hsw_p23+hsw_p01")

(define_insn_reservation "hsw_sse_logic" 1
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "none")
				   (eq_attr "type" "sselog,sselog1")))
			 "hsw_decodern,hsw_p015")

(define_insn_reservation "hsw_sse_logic_load" 2
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "load")
				   (eq_attr "type" "sselog,sselog1")))
			 "hsw_decodern,hsw_p015+hsw_p23")

(define_insn_reservation "hsw_sse_add" 3
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "none")
				   (eq_attr "type" "sseadd")))
			"hsw_decodern,hsw_p1|hsw_p5")

(define_insn_reservation "hsw_sse_add_load" 3
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "load")
				   (eq_attr "type" "sseadd")))
			"hsw_decodern,(hsw_p1|hsw_p5)+hsw_p23")

(define_insn_reservation "hsw_sse_mul" 5
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "none")
				   (eq_attr "type" "ssemul")))
			"hsw_decodern,hsw_p0")

(define_insn_reservation "hsw_sse_mul_load" 5
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "load")
				   (eq_attr "type" "ssemul")))
			"hsw_decodern,hsw_p0+hsw_p23")
;; Use skylake pipeline.
(define_insn_reservation "hsw_sse_muladd" 5
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "none")
					(eq_attr "type" "ssemuladd")))
			"hsw_decodern,hsw_p01")

(define_insn_reservation "hsw_sse_muladd_load" 5
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "load")
					(eq_attr "type" "ssemuladd")))
			"hsw_decodern,hsw_p01+hsw_p23")

(define_insn_reservation "hsw_sse_div_SF" 18
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "none")
				   (and (eq_attr "mode" "SF,V4SF,V8SF")
					(eq_attr "type" "ssediv"))))
			 "hsw_decodern,hsw_p0,hsw_ssediv*14")

(define_insn_reservation "hsw_sse_div_SF_load" 18
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "none")
				   (and (eq_attr "mode" "SF,V4SF,V8SF")
					(eq_attr "type" "ssediv"))))
			 "hsw_decodern,(hsw_p23+hsw_p0),hsw_ssediv*14")

(define_insn_reservation "hsw_sse_div_DF" 28
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "none")
				   (and (eq_attr "mode" "DF,V2DF,V4DF")
					(eq_attr "type" "ssediv"))))
			 "hsw_decodern,hsw_p0,hsw_ssediv*20")

(define_insn_reservation "hsw_sse_div_DF_load" 28
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "none")
				   (and (eq_attr "mode" "DF,V2DF,V4DF")
					(eq_attr "type" "ssediv"))))
			 "hsw_decodern,(hsw_p23+hsw_p0),hsw_ssediv*20")

(define_insn_reservation "hsw_sse_icvt" 4
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "none")
				   (eq_attr "type" "sseicvt")))
			 "hsw_decodern,hsw_p1")

(define_insn_reservation "hsw_sse_icvt_load" 4
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "!none")
				   (eq_attr "type" "sseicvt")))
			 "hsw_decodern,hsw_p23+hsw_p1")


(define_insn_reservation "hsw_sse_icvt_SI" 3
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "none")
				   (and (eq_attr "mode" "SI")
					(eq_attr "type" "sseicvt"))))
			 "hsw_decodern,hsw_p1")

(define_insn_reservation "hsw_sse_icvt_SI_load" 3
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "!none")
				   (and (eq_attr "mode" "SI")
					(eq_attr "type" "sseicvt"))))
			 "hsw_decodern,hsw_p23+hsw_p1")

(define_insn_reservation "hsw_sse_mov" 1
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "none")
				   (eq_attr "type" "ssemov")))
			 "hsw_decodern,hsw_p015")

(define_insn_reservation "hsw_sse_mov_load" 2
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "load")
				   (eq_attr "type" "ssemov")))
			 "hsw_decodern,hsw_p23")

(define_insn_reservation "hsw_sse_mov_store" 1
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "store")
				   (eq_attr "type" "ssemov")))
			 "hsw_decodern,hsw_p4p7")

(define_insn_reservation "hsw_insn" 1
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "none,unknown")
				   (eq_attr "type" "alu,alu1,negnot,incdec,icmp,test,setcc,sseishft1,mmx,mmxcmp")))
			 "hsw_decodern,hsw_p0156")

(define_insn_reservation "hsw_insn_load" 1
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "load")
				   (eq_attr "type" "alu,alu1,negnot,incdec,icmp,test,setcc,pop,sseishft1,mmx,mmxcmp")))
			 "hsw_decodern,hsw_p23+hsw_p0156")

(define_insn_reservation "hsw_insn_store" 1
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "store")
				   (eq_attr "type" "alu,alu1,negnot,incdec,icmp,test,setcc,sseishft1,mmx,mmxcmp")))
			 "hsw_decodern,hsw_p0156+hsw_p4p7")

;; read-modify-store instructions produce 4 uops so they have to be
;; decoded on hsw_decoder0 as well.
(define_insn_reservation "hsw_insn_both" 4
			 (and (eq_attr "cpu" "generic,haswell")
			      (and (eq_attr "memory" "both")
				   (eq_attr "type" "alu,alu1,negnot,incdec,icmp,test,setcc,pop,sseishft1,mmx,mmxcmp")))
			 "hsw_decodern,hsw_p23+hsw_p0156+hsw_p4p7")
