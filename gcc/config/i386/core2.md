;; Scheduling for Core 2 and derived processors.
;; Copyright (C) 2004-2013 Free Software Foundation, Inc.
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

;; The scheduling description in this file is based on the one in ppro.md,
;; with additional information obtained from
;;
;;    "How to optimize for the Pentium family of microprocessors",
;;    by Agner Fog, PhD.
;;
;; The major difference from the P6 pipeline is one extra decoder, and
;; one extra execute unit.  Due to micro-op fusion, many insns no longer
;; need to be decoded in decoder 0, but can be handled by all of them.

;; The core2_idiv, core2_fdiv and core2_ssediv automata are used to
;; model issue latencies of idiv, fdiv and ssediv type insns.
(define_automaton "core2_decoder,core2_core,core2_idiv,core2_fdiv,core2_ssediv,core2_load,core2_store")

;; The CPU domain, used for Core i7 bypass latencies
(define_attr "i7_domain" "int,float,simd"
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

;; As for the Pentium Pro,
;;  - an instruction with 1 uop can be decoded by any of the three
;;    decoders in one cycle.
;;  - an instruction with 1 to 4 uops can be decoded only by decoder 0
;;    but still in only one cycle.
;;  - a complex (microcode) instruction can also only be decoded by
;;    decoder 0, and this takes an unspecified number of cycles.
;;
;; The goal is to schedule such that we have a few-one-one uops sequence
;; in each cycle, to decode as many instructions per cycle as possible.
(define_cpu_unit "c2_decoder0" "core2_decoder")
(define_cpu_unit "c2_decoder1" "core2_decoder")
(define_cpu_unit "c2_decoder2" "core2_decoder")
(define_cpu_unit "c2_decoder3" "core2_decoder")

;; We first wish to find an instruction for c2_decoder0, so exclude
;; c2_decoder1 and c2_decoder2 from being reserved until c2_decoder 0 is
;; reserved.
(presence_set "c2_decoder1" "c2_decoder0")
(presence_set "c2_decoder2" "c2_decoder0")
(presence_set "c2_decoder3" "c2_decoder0")

;; Most instructions can be decoded on any of the three decoders.
(define_reservation "c2_decodern" "(c2_decoder0|c2_decoder1|c2_decoder2|c2_decoder3)")

;; The out-of-order core has six pipelines.  These are similar to the
;; Pentium Pro's five pipelines.  Port 2 is responsible for memory loads,
;; port 3 for store address calculations, port 4 for memory stores, and
;; ports 0, 1 and 5 for everything else.

(define_cpu_unit "c2_p0,c2_p1,c2_p5" "core2_core")
(define_cpu_unit "c2_p2" "core2_load")
(define_cpu_unit "c2_p3,c2_p4" "core2_store")
(define_cpu_unit "c2_idiv" "core2_idiv")
(define_cpu_unit "c2_fdiv" "core2_fdiv")
(define_cpu_unit "c2_ssediv" "core2_ssediv")

;; Only the irregular instructions have to be modeled here.  A load
;; increases the latency by 2 or 3, or by nothing if the manual gives
;; a latency already.  Store latencies are not accounted for.
;;
;; The simple instructions follow a very regular pattern of 1 uop per
;; reg-reg operation, 1 uop per load on port 2. and 2 uops per store
;; on port 4 and port 3.  These instructions are modelled at the bottom
;; of this file.
;;
;; For microcoded instructions we don't know how many uops are produced.
;; These instructions are the "complex" ones in the Intel manuals.  All
;; we _do_ know is that they typically produce four or more uops, so
;; they can only be decoded on c2_decoder0.  Modelling their latencies
;; doesn't make sense because we don't know how these instructions are
;; executed in the core.  So we just model that they can only be decoded
;; on decoder 0, and say that it takes a little while before the result
;; is available.
(define_insn_reservation "c2_complex_insn" 6
			 (and (eq_attr "cpu" "core2,nehalem")
			      (eq_attr "type" "other,multi,str"))
			 "c2_decoder0")

(define_insn_reservation "c2_call" 1
			 (and (eq_attr "cpu" "core2,nehalem")
			      (eq_attr "type" "call,callv"))
			 "c2_decoder0")

;; imov with memory operands does not use the integer units.
;; imovx always decodes to one uop, and also doesn't use the integer
;; units if it has memory operands.
(define_insn_reservation "c2_imov" 1
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "none")
				   (eq_attr "type" "imov,imovx")))
			 "c2_decodern,(c2_p0|c2_p1|c2_p5)")

(define_insn_reservation "c2_imov_load" 4
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "load")
				   (eq_attr "type" "imov,imovx")))
			 "c2_decodern,c2_p2")

(define_insn_reservation "c2_imov_store" 1
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "store")
				   (eq_attr "type" "imov")))
			 "c2_decodern,c2_p4+c2_p3")

(define_insn_reservation "c2_icmov" 2
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "none")
				   (eq_attr "type" "icmov")))
			 "c2_decoder0,(c2_p0|c2_p1|c2_p5)*2")

(define_insn_reservation "c2_icmov_load" 2
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "load")
				   (eq_attr "type" "icmov")))
			 "c2_decoder0,c2_p2,(c2_p0|c2_p1|c2_p5)*2")

(define_insn_reservation "c2_push_reg" 1
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "store")
				   (eq_attr "type" "push")))
			 "c2_decodern,c2_p4+c2_p3")

(define_insn_reservation "c2_push_mem" 1
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "both")
				   (eq_attr "type" "push")))
			 "c2_decoder0,c2_p2,c2_p4+c2_p3")

;; lea executes on port 0 with latency one and throughput 1.
(define_insn_reservation "c2_lea" 1
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "none")
				   (eq_attr "type" "lea")))
			 "c2_decodern,c2_p0")

;; Shift and rotate decode as two uops which can go to port 0 or 5.
;; The load and store units need to be reserved when memory operands
;; are involved.
(define_insn_reservation "c2_shift_rotate" 1
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "none")
				   (eq_attr "type" "ishift,ishift1,rotate,rotate1")))
			 "c2_decodern,(c2_p0|c2_p5)")

(define_insn_reservation "c2_shift_rotate_mem" 4
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "!none")
				   (eq_attr "type" "ishift,ishift1,rotate,rotate1")))
			 "c2_decoder0,c2_p2,(c2_p0|c2_p5),c2_p4+c2_p3")

;; See comments in ppro.md for the corresponding reservation.
(define_insn_reservation "c2_branch" 1
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "none")
				   (eq_attr "type" "ibr")))
			 "c2_decodern,c2_p5")

;; ??? Indirect branches probably have worse latency than this.
(define_insn_reservation "c2_indirect_branch" 6
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "!none")
				   (eq_attr "type" "ibr")))
			 "c2_decoder0,c2_p2+c2_p5")

(define_insn_reservation "c2_leave" 4
			 (and (eq_attr "cpu" "core2,nehalem")
			      (eq_attr "type" "leave"))
			 "c2_decoder0,c2_p2+(c2_p0|c2_p1),(c2_p0|c2_p1)")

;; mul and imul with two/three operands only execute on port 1 for HImode
;; and SImode, port 0 for DImode.
(define_insn_reservation "c2_imul_hisi" 3
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "none")
				   (and (eq_attr "mode" "HI,SI")
					(eq_attr "type" "imul"))))
			 "c2_decodern,c2_p1")

(define_insn_reservation "c2_imul_hisi_mem" 3
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "!none")
				   (and (eq_attr "mode" "HI,SI")
					(eq_attr "type" "imul"))))
			 "c2_decoder0,c2_p2+c2_p1")

(define_insn_reservation "c2_imul_di" 5
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "none")
				   (and (eq_attr "mode" "DI")
					(eq_attr "type" "imul"))))
			 "c2_decodern,c2_p0")

(define_insn_reservation "c2_imul_di_mem" 5
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "!none")
				   (and (eq_attr "mode" "DI")
					(eq_attr "type" "imul"))))
			 "c2_decoder0,c2_p2+c2_p0")

;; div and idiv are very similar, so we model them the same.
;; QI, HI, and SI have issue latency 12, 21, and 37, respectively.
;; These issue latencies are modelled via the c2_div automaton.
(define_insn_reservation "c2_idiv_QI" 19
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "none")
				   (and (eq_attr "mode" "QI")
					(eq_attr "type" "idiv"))))
			 "c2_decoder0,(c2_p0+c2_idiv)*2,(c2_p0|c2_p1)+c2_idiv,c2_idiv*9")

(define_insn_reservation "c2_idiv_QI_load" 19
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "load")
				   (and (eq_attr "mode" "QI")
					(eq_attr "type" "idiv"))))
			 "c2_decoder0,c2_p2+c2_p0+c2_idiv,c2_p0+c2_idiv,(c2_p0|c2_p1)+c2_idiv,c2_idiv*9")

(define_insn_reservation "c2_idiv_HI" 23
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "none")
				   (and (eq_attr "mode" "HI")
					(eq_attr "type" "idiv"))))
			 "c2_decoder0,(c2_p0+c2_idiv)*3,(c2_p0|c2_p1)+c2_idiv,c2_idiv*17")

(define_insn_reservation "c2_idiv_HI_load" 23
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "load")
				   (and (eq_attr "mode" "HI")
					(eq_attr "type" "idiv"))))
			 "c2_decoder0,c2_p2+c2_p0+c2_idiv,c2_p0+c2_idiv,(c2_p0|c2_p1)+c2_idiv,c2_idiv*18")

(define_insn_reservation "c2_idiv_SI" 39
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "none")
				   (and (eq_attr "mode" "SI")
					(eq_attr "type" "idiv"))))
			 "c2_decoder0,(c2_p0+c2_idiv)*3,(c2_p0|c2_p1)+c2_idiv,c2_idiv*33")

(define_insn_reservation "c2_idiv_SI_load" 39
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "load")
				   (and (eq_attr "mode" "SI")
					(eq_attr "type" "idiv"))))
			 "c2_decoder0,c2_p2+c2_p0+c2_idiv,c2_p0+c2_idiv,(c2_p0|c2_p1)+c2_idiv,c2_idiv*34")

;; x87 floating point operations.

(define_insn_reservation "c2_fxch" 0
			 (and (eq_attr "cpu" "core2,nehalem")
			      (eq_attr "type" "fxch"))
			 "c2_decodern")

(define_insn_reservation "c2_fop" 3
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "none,unknown")
				   (eq_attr "type" "fop")))
			 "c2_decodern,c2_p1")

(define_insn_reservation "c2_fop_load" 5
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "load")
				   (eq_attr "type" "fop")))
			 "c2_decoder0,c2_p2+c2_p1,c2_p1")

(define_insn_reservation "c2_fop_store" 3
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "store")
				   (eq_attr "type" "fop")))
			 "c2_decoder0,c2_p0,c2_p0,c2_p0+c2_p4+c2_p3")

(define_insn_reservation "c2_fop_both" 5
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "both")
				   (eq_attr "type" "fop")))
			 "c2_decoder0,c2_p2+c2_p0,c2_p0+c2_p4+c2_p3")

(define_insn_reservation "c2_fsgn" 1
			 (and (eq_attr "cpu" "core2,nehalem")
			      (eq_attr "type" "fsgn"))
			 "c2_decodern,c2_p0")

(define_insn_reservation "c2_fistp" 5
			 (and (eq_attr "cpu" "core2,nehalem")
			      (eq_attr "type" "fistp"))
			 "c2_decoder0,c2_p0*2,c2_p4+c2_p3")

(define_insn_reservation "c2_fcmov" 2
			 (and (eq_attr "cpu" "core2,nehalem")
			      (eq_attr "type" "fcmov"))
			 "c2_decoder0,c2_p0*2")

(define_insn_reservation "c2_fcmp" 1
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "none")
				   (eq_attr "type" "fcmp")))
			 "c2_decodern,c2_p1")

(define_insn_reservation "c2_fcmp_load" 4
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "load")
				   (eq_attr "type" "fcmp")))
			 "c2_decoder0,c2_p2+c2_p1")

(define_insn_reservation "c2_fmov" 1
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "none")
				   (eq_attr "type" "fmov")))
			 "c2_decodern,c2_p0")

(define_insn_reservation "c2_fmov_load" 1
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "load")
				   (and (eq_attr "mode" "!XF")
					(eq_attr "type" "fmov"))))
			 "c2_decodern,c2_p2")

(define_insn_reservation "c2_fmov_XF_load" 3
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "load")
				   (and (eq_attr "mode" "XF")
					(eq_attr "type" "fmov"))))
			 "c2_decoder0,(c2_p2+c2_p0)*2")

(define_insn_reservation "c2_fmov_store" 1
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "store")
				   (and (eq_attr "mode" "!XF")
					(eq_attr "type" "fmov"))))
			 "c2_decodern,c2_p3+c2_p4")

(define_insn_reservation "c2_fmov_XF_store" 3
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "store")
				   (and (eq_attr "mode" "XF")
					(eq_attr "type" "fmov"))))
			 "c2_decoder0,(c2_p3+c2_p4),(c2_p3+c2_p4)")

;; fmul executes on port 0 with latency 5.  It has issue latency 2,
;; but we don't model this.
(define_insn_reservation "c2_fmul" 5
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "none")
				   (eq_attr "type" "fmul")))
			 "c2_decoder0,c2_p0*2")

(define_insn_reservation "c2_fmul_load" 6
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "load")
				   (eq_attr "type" "fmul")))
			 "c2_decoder0,c2_p2+c2_p0,c2_p0")

;; fdiv latencies depend on the mode of the operands.  XFmode gives
;; a latency of 38 cycles, DFmode gives 32, and SFmode gives latency 18.
;; Division by a power of 2 takes only 9 cycles, but we cannot model
;; that.  Throughput is equal to latency - 1, which we model using the
;; c2_div automaton.
(define_insn_reservation "c2_fdiv_SF" 18
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "none")
				   (and (eq_attr "mode" "SF")
					(eq_attr "type" "fdiv,fpspc"))))
			 "c2_decodern,c2_p0+c2_fdiv,c2_fdiv*16")

(define_insn_reservation "c2_fdiv_SF_load" 19
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "load")
				   (and (eq_attr "mode" "SF")
					(eq_attr "type" "fdiv,fpspc"))))
			 "c2_decoder0,c2_p2+c2_p0+c2_fdiv,c2_fdiv*16")

(define_insn_reservation "c2_fdiv_DF" 32
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "none")
				   (and (eq_attr "mode" "DF")
					(eq_attr "type" "fdiv,fpspc"))))
			 "c2_decodern,c2_p0+c2_fdiv,c2_fdiv*30")

(define_insn_reservation "c2_fdiv_DF_load" 33
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "load")
				   (and (eq_attr "mode" "DF")
					(eq_attr "type" "fdiv,fpspc"))))
			 "c2_decoder0,c2_p2+c2_p0+c2_fdiv,c2_fdiv*30")

(define_insn_reservation "c2_fdiv_XF" 38
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "none")
				   (and (eq_attr "mode" "XF")
					(eq_attr "type" "fdiv,fpspc"))))
			 "c2_decodern,c2_p0+c2_fdiv,c2_fdiv*36")

(define_insn_reservation "c2_fdiv_XF_load" 39
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "load")
				   (and (eq_attr "mode" "XF")
					(eq_attr "type" "fdiv,fpspc"))))
			 "c2_decoder0,c2_p2+c2_p0+c2_fdiv,c2_fdiv*36")

;; MMX instructions.

(define_insn_reservation "c2_mmx_add" 1
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "none")
				   (eq_attr "type" "mmxadd,sseiadd")))
			 "c2_decodern,c2_p0|c2_p5")

(define_insn_reservation "c2_mmx_add_load" 2
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "load")
				   (eq_attr "type" "mmxadd,sseiadd")))
			 "c2_decodern,c2_p2+c2_p0|c2_p5")

(define_insn_reservation "c2_mmx_shft" 1
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "none")
				   (eq_attr "type" "mmxshft")))
			 "c2_decodern,c2_p0|c2_p5")

(define_insn_reservation "c2_mmx_shft_load" 2
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "load")
				   (eq_attr "type" "mmxshft")))
			 "c2_decoder0,c2_p2+c2_p1")

(define_insn_reservation "c2_mmx_sse_shft" 1
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "none")
				   (and (eq_attr "type" "sseishft")
					(eq_attr "length_immediate" "!0"))))
			 "c2_decodern,c2_p1")

(define_insn_reservation "c2_mmx_sse_shft_load" 2
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "load")
				   (and (eq_attr "type" "sseishft")
					(eq_attr "length_immediate" "!0"))))
			 "c2_decodern,c2_p1")

(define_insn_reservation "c2_mmx_sse_shft1" 2
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "none")
				   (and (eq_attr "type" "sseishft")
					(eq_attr "length_immediate" "0"))))
			 "c2_decodern,c2_p1")

(define_insn_reservation "c2_mmx_sse_shft1_load" 3
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "load")
				   (and (eq_attr "type" "sseishft")
					(eq_attr "length_immediate" "0"))))
			 "c2_decodern,c2_p1")

(define_insn_reservation "c2_mmx_mul" 3
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "none")
				   (eq_attr "type" "mmxmul,sseimul")))
			 "c2_decodern,c2_p1")

(define_insn_reservation "c2_mmx_mul_load" 3
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "none")
				   (eq_attr "type" "mmxmul,sseimul")))
			 "c2_decoder0,c2_p2+c2_p1")

(define_insn_reservation "c2_sse_mmxcvt" 4
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "mode" "DI")
				   (eq_attr "type" "mmxcvt")))
			 "c2_decodern,c2_p1")

;; FIXME: These are Pentium III only, but we cannot tell here if
;; we're generating code for PentiumPro/Pentium II or Pentium III
;; (define_insn_reservation "c2_sse_mmxshft" 2
;;			 (and (eq_attr "cpu" "core2,nehalem")
;;			      (and (eq_attr "mode" "TI")
;;				   (eq_attr "type" "mmxshft")))
;;			 "c2_decodern,c2_p0")

;; The sfence instruction.
(define_insn_reservation "c2_sse_sfence" 3
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "unknown")
				   (eq_attr "type" "sse")))
			 "c2_decoder0,c2_p4+c2_p3")

;; FIXME: This reservation is all wrong when we're scheduling sqrtss.
(define_insn_reservation "c2_sse_SFDF" 3
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "mode" "SF,DF")
				   (eq_attr "type" "sse")))
			 "c2_decodern,c2_p0")

(define_insn_reservation "c2_sse_V4SF" 4
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "mode" "V4SF")
				   (eq_attr "type" "sse")))
			 "c2_decoder0,c2_p1*2")

(define_insn_reservation "c2_sse_addcmp" 3
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "none")
				   (eq_attr "type" "sseadd,sseadd1,ssecmp,ssecomi")))
			 "c2_decodern,c2_p1")

(define_insn_reservation "c2_sse_addcmp_load" 3
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "load")
				   (eq_attr "type" "sseadd,sseadd1,ssecmp,ssecomi")))
			 "c2_decodern,c2_p2+c2_p1")

(define_insn_reservation "c2_sse_mul_SF" 4
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "none")
				   (and (eq_attr "mode" "SF,V4SF")
					(eq_attr "type" "ssemul"))))
			"c2_decodern,c2_p0")

(define_insn_reservation "c2_sse_mul_SF_load" 4
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "load")
				   (and (eq_attr "mode" "SF,V4SF")
					(eq_attr "type" "ssemul"))))
			"c2_decodern,c2_p2+c2_p0")

(define_insn_reservation "c2_sse_mul_DF" 5
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "none")
				   (and (eq_attr "mode" "DF,V2DF")
					(eq_attr "type" "ssemul"))))
			"c2_decodern,c2_p0")

(define_insn_reservation "c2_sse_mul_DF_load" 5
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "load")
				   (and (eq_attr "mode" "DF,V2DF")
					(eq_attr "type" "ssemul"))))
			"c2_decodern,c2_p2+c2_p0")

(define_insn_reservation "c2_sse_div_SF" 18
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "none")
				   (and (eq_attr "mode" "SF,V4SF")
					(eq_attr "type" "ssediv"))))
			 "c2_decodern,c2_p0,c2_ssediv*17")

(define_insn_reservation "c2_sse_div_SF_load" 18
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "none")
				   (and (eq_attr "mode" "SF,V4SF")
					(eq_attr "type" "ssediv"))))
			 "c2_decodern,(c2_p2+c2_p0),c2_ssediv*17")

(define_insn_reservation "c2_sse_div_DF" 32
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "none")
				   (and (eq_attr "mode" "DF,V2DF")
					(eq_attr "type" "ssediv"))))
			 "c2_decodern,c2_p0,c2_ssediv*31")

(define_insn_reservation "c2_sse_div_DF_load" 32
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "none")
				   (and (eq_attr "mode" "DF,V2DF")
					(eq_attr "type" "ssediv"))))
			 "c2_decodern,(c2_p2+c2_p0),c2_ssediv*31")

;; FIXME: these have limited throughput
(define_insn_reservation "c2_sse_icvt_SF" 4
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "none")
				   (and (eq_attr "mode" "SF")
					(eq_attr "type" "sseicvt"))))
			 "c2_decodern,c2_p1")

(define_insn_reservation "c2_sse_icvt_SF_load" 4
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "!none")
				   (and (eq_attr "mode" "SF")
					(eq_attr "type" "sseicvt"))))
			 "c2_decodern,c2_p2+c2_p1")

(define_insn_reservation "c2_sse_icvt_DF" 4
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "none")
				   (and (eq_attr "mode" "DF")
					(eq_attr "type" "sseicvt"))))
			 "c2_decoder0,c2_p0+c2_p1")

(define_insn_reservation "c2_sse_icvt_DF_load" 4
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "!none")
				   (and (eq_attr "mode" "DF")
					(eq_attr "type" "sseicvt"))))
			 "c2_decoder0,(c2_p2+c2_p1)")

(define_insn_reservation "c2_sse_icvt_SI" 3
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "none")
				   (and (eq_attr "mode" "SI")
					(eq_attr "type" "sseicvt"))))
			 "c2_decodern,c2_p1")

(define_insn_reservation "c2_sse_icvt_SI_load" 3
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "!none")
				   (and (eq_attr "mode" "SI")
					(eq_attr "type" "sseicvt"))))
			 "c2_decodern,(c2_p2+c2_p1)")

(define_insn_reservation "c2_sse_mov" 1
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "none")
				   (eq_attr "type" "ssemov")))
			 "c2_decodern,(c2_p0|c2_p1|c2_p5)")

(define_insn_reservation "c2_sse_mov_load" 2
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "load")
				   (eq_attr "type" "ssemov")))
			 "c2_decodern,c2_p2")

(define_insn_reservation "c2_sse_mov_store" 1
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "store")
				   (eq_attr "type" "ssemov")))
			 "c2_decodern,c2_p4+c2_p3")

;; All other instructions are modelled as simple instructions.
;; We have already modelled all i387 floating point instructions, so all
;; other instructions execute on either port 0, 1 or 5.  This includes
;; the ALU units, and the MMX units.
;;
;; reg-reg instructions produce 1 uop so they can be decoded on any of
;; the three decoders.  Loads benefit from micro-op fusion and can be
;; treated in the same way.
(define_insn_reservation "c2_insn" 1
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "none,unknown")
				   (eq_attr "type" "alu,alu1,negnot,incdec,icmp,test,setcc,sseishft1,mmx,mmxcmp")))
			 "c2_decodern,(c2_p0|c2_p1|c2_p5)")

(define_insn_reservation "c2_insn_load" 4
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "load")
				   (eq_attr "type" "alu,alu1,negnot,incdec,icmp,test,setcc,pop,sseishft1,mmx,mmxcmp")))
			 "c2_decodern,c2_p2,(c2_p0|c2_p1|c2_p5)")

;; register-memory instructions have three uops,  so they have to be
;; decoded on c2_decoder0.
(define_insn_reservation "c2_insn_store" 1
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "store")
				   (eq_attr "type" "alu,alu1,negnot,incdec,icmp,test,setcc,sseishft1,mmx,mmxcmp")))
			 "c2_decoder0,(c2_p0|c2_p1|c2_p5),c2_p4+c2_p3")

;; read-modify-store instructions produce 4 uops so they have to be
;; decoded on c2_decoder0 as well.
(define_insn_reservation "c2_insn_both" 4
			 (and (eq_attr "cpu" "core2,nehalem")
			      (and (eq_attr "memory" "both")
				   (eq_attr "type" "alu,alu1,negnot,incdec,icmp,test,setcc,pop,sseishft1,mmx,mmxcmp")))
			 "c2_decoder0,c2_p2,(c2_p0|c2_p1|c2_p5),c2_p4+c2_p3")
