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

;; Scheduling for ZHAOXIN lujiazui processor.

;; Modeling automatons for decoders, execution pipes, AGU pipes, and divider.
(define_automaton "lujiazui_decoder,lujiazui_core,lujiazui_agu,lujiazui_div")

;; The rules for the decoder are simple:
;;  - an instruction with 1 uop can be decoded by any of the three
;;    decoders in one cycle.
;;  - an instruction with 2 uops can be decoded by decoder 0 or decoder 1
;;    but still in only one cycle.
;;  - a complex (microcode) instruction can only be decoded by
;;    decoder 0, and this takes an unspecified number of cycles.
;;
;; The goal is to schedule such that we have a few-one-two uops sequence
;; in each cycle, to decode as many instructions per cycle as possible.
(define_cpu_unit "lua_decoder0" "lujiazui_decoder")
(define_cpu_unit "lua_decoder1" "lujiazui_decoder")
(define_cpu_unit "lua_decoder2" "lujiazui_decoder")

;; We first wish to find an instruction for lua_decoder0, so exclude
;; lua_decoder1 and lua_decoder2 from being reserved until
;; lua_decoder0 is reserved, and also exclude lua_decoder2
;; from being reserved until lua_decoder1 is reserved.
(presence_set "lua_decoder1" "lua_decoder0")
(presence_set "lua_decoder2" "lua_decoder0")
(presence_set "lua_decoder2" "lua_decoder1")

;; Most instructions can be decoded on any of the three decoders.
(define_reservation "lua_decodern" "lua_decoder0|lua_decoder1|lua_decoder2")
(define_reservation "lua_decoder01" "lua_decoder0|lua_decoder1")

;; The out-of-order core has six pipelines.
;; Port 4, 5 are responsible for address calculations, load or store.
;; Port 0, 1, 2, 3 for everything else.

(define_cpu_unit "lua_p0,lua_p1,lua_p2,lua_p3" "lujiazui_core")
(define_cpu_unit "lua_p4,lua_p5" "lujiazui_agu")

(define_cpu_unit "lua_div" "lujiazui_div")

(define_reservation "lua_p03" "lua_p0|lua_p3")
(define_reservation "lua_p12" "lua_p1|lua_p2")
(define_reservation "lua_p1p2" "lua_p1+lua_p2")
(define_reservation "lua_p45" "lua_p4|lua_p5")
(define_reservation "lua_p4p5" "lua_p4+lua_p5")
(define_reservation "lua_p0p1p2p3" "lua_p0+lua_p1+lua_p2+lua_p3")

;; Only the irregular instructions have to be modeled here.

;; Complex instruction.
(define_insn_reservation "lua_complex_insn" 6
			 (and (eq_attr "cpu" "lujiazui")
			      (eq_attr "type" "other,multi,str"))
			 "lua_decoder0")

;; Call instruction.
(define_insn_reservation "lua_call" 1
			 (and (eq_attr "cpu" "lujiazui")
			      (eq_attr "type" "call,callv"))
			 "lua_decoder0,lua_p45,lua_p1")

;; MOV - integer moves.
(define_insn_reservation "lua_imov" 1
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "none")
				   (eq_attr "type" "imov,imovx")))
			 "lua_decodern,lua_p12")

(define_insn_reservation "lua_imov_load" 4
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "load")
				   (eq_attr "type" "imov,imovx")))
			 "lua_decoder01,lua_p45")

(define_insn_reservation "lua_imov_store" 1
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "store")
				   (eq_attr "type" "imov")))
			 "lua_decodern,lua_p12+lua_p45")

(define_insn_reservation "lua_icmov" 1
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "none,unknown")
				   (eq_attr "type" "icmov")))
			 "lua_decodern,lua_p2")

(define_insn_reservation "lua_icmov_load" 5
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "load")
				   (eq_attr "type" "icmov")))
			 "lua_decoder01,lua_p45,lua_p2")

;; Push and pop.
(define_insn_reservation "lua_push_reg" 1
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "store")
				   (eq_attr "type" "push")))
			 "lua_decodern,lua_p12+lua_p45")

(define_insn_reservation "lua_push_mem" 4
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "both")
				   (eq_attr "type" "push")))
			 "lua_decoder01,lua_p45,lua_p12+lua_p45")

(define_insn_reservation "lua_pop_reg" 4
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "load")
				   (eq_attr "type" "pop")))
			 "lua_decoder01,lua_p45")

(define_insn_reservation "lua_pop_mem" 4
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "both")
				   (eq_attr "type" "pop")))
			 "lua_decoder0,lua_p45,lua_p12+lua_p45")

(define_insn_reservation "lua_lea" 1
			 (and (eq_attr "cpu" "lujiazui")
				  (eq_attr "type" "lea"))
			 "lua_decodern,lua_p45")

(define_insn_reservation "lua_shift_rotate" 1
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "none,unknown")
			 (eq_attr "type" "ishift,ishift1,rotate,rotate1")))
			 "lua_decodern,lua_p2")

(define_insn_reservation "lua_shift_rotate_load" 5
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "load")
			 (eq_attr "type" "ishift,ishift1,rotate,rotate1")))
			 "lua_decoder01,lua_p45,lua_p2")

(define_insn_reservation "lua_shift_rotate_store" 1
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "store")
			 (eq_attr "type" "ishift,ishift1,rotate,rotate1")))
			 "lua_decoder01,lua_p2,lua_p45")

(define_insn_reservation "lua_shift_rotate_both" 5
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "both")
			 (eq_attr "type" "ishift,ishift1,rotate,rotate1")))
			 "lua_decoder0,lua_p45,lua_p2,lua_p45")

(define_insn_reservation "lua_branch" 1
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "none")
				   (eq_attr "type" "ibr")))
			 "lua_decodern,lua_p1")

(define_insn_reservation "lua_indirect_branch_load" 5
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "load")
				   (eq_attr "type" "ibr")))
			 "lua_decodern,lua_p45,lua_p1")

(define_insn_reservation "lua_leave" 4
			 (and (eq_attr "cpu" "lujiazui")
			      (eq_attr "type" "leave"))
			 "lua_decoder0,lua_p45+lua_p12,lua_p12")

;; Multiplication instructions.

(define_insn_reservation "lua_imul_qi" 2
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "none")
				   (and (eq_attr "mode" "QI")
					(eq_attr "type" "imul,imulx"))))
			 "lua_decodern,lua_p1p2")

(define_insn_reservation "lua_imul_qi_mem" 6
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "!none")
				   (and (eq_attr "mode" "QI")
					(eq_attr "type" "imul,imulx"))))
			 "lua_decoder01,lua_p1p2+lua_p45")

(define_insn_reservation "lua_imul_hisi" 3
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "none")
				   (and (eq_attr "mode" "HI,SI")
					(eq_attr "type" "imul,imulx"))))
			 "lua_decoder0,lua_p1p2")

(define_insn_reservation "lua_imul_hisi_mem" 7
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "!none")
				   (and (eq_attr "mode" "HI,SI")
					(eq_attr "type" "imul,imulx"))))
			 "lua_decoder0,lua_p1p2+lua_p45")

(define_insn_reservation "lua_imul_di" 12
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "none")
				   (and (eq_attr "mode" "DI")
					(eq_attr "type" "imul,imulx"))))
			 "lua_decoder0,lua_p0p1p2p3")

(define_insn_reservation "lua_imul_di_mem" 16
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "!none")
				   (and (eq_attr "mode" "DI")
					(eq_attr "type" "imul,imulx"))))
			 "lua_decoder0,lua_p0p1p2p3+lua_p45")

;; Division instructions.

(define_insn_reservation "lua_idiv_qi" 21
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "none")
				   (and (eq_attr "mode" "QI")
					(eq_attr "type" "idiv"))))
			 "lua_decoder0,lua_p0p1p2p3,lua_div*21")

(define_insn_reservation "lua_idiv_qi_load" 25
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "load")
				   (and (eq_attr "mode" "QI")
					(eq_attr "type" "idiv"))))
			 "lua_decoder0,lua_p45,lua_p0p1p2p3,lua_div*21")

(define_insn_reservation "lua_idiv_hi" 22
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "none")
				   (and (eq_attr "mode" "HI")
					(eq_attr "type" "idiv"))))
			 "lua_decoder0,lua_p0p1p2p3,lua_div*22")

(define_insn_reservation "lua_idiv_hi_load" 26
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "load")
				   (and (eq_attr "mode" "HI")
					(eq_attr "type" "idiv"))))
			 "lua_decoder0,lua_p45,lua_p0p1p2p3,lua_div*22")

(define_insn_reservation "lua_idiv_si" 20
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "none")
				   (and (eq_attr "mode" "SI")
					(eq_attr "type" "idiv"))))
			 "lua_decoder0,lua_p0p1p2p3,lua_div*20")

(define_insn_reservation "lua_idiv_si_load" 24
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "load")
				   (and (eq_attr "mode" "SI")
					(eq_attr "type" "idiv"))))
			 "lua_decoder0,lua_p45,lua_p0p1p2p3,lua_div*20")

(define_insn_reservation "lua_idiv_di" 150
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "none")
				   (and (eq_attr "mode" "DI")
					(eq_attr "type" "idiv"))))
			 "lua_decoder0,lua_p0p1p2p3,lua_div*150")

(define_insn_reservation "lua_idiv_di_load" 154
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "load")
				   (and (eq_attr "mode" "DI")
					(eq_attr "type" "idiv"))))
			 "lua_decoder0,lua_p45,lua_p0p1p2p3,lua_div*150")

;; x87 floating point operations.

(define_insn_reservation "lua_fxch" 1
			 (and (eq_attr "cpu" "lujiazui")
			      (eq_attr "type" "fxch"))
			 "lua_decodern,lua_p1")

(define_insn_reservation "lua_fop" 3
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "none,unknown")
				   (eq_attr "type" "fop")))
			 "lua_decodern,lua_p0")

(define_insn_reservation "lua_fop_load" 7
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "load")
				   (eq_attr "type" "fop")))
			 "lua_decoder01,lua_p45,lua_p0")

(define_insn_reservation "lua_fop_store" 3
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "store")
				   (eq_attr "type" "fop")))
			 "lua_decodern,lua_p0,lua_p45")

(define_insn_reservation "lua_fop_both" 7
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "both")
				   (eq_attr "type" "fop")))
			 "lua_decoder0,lua_p45,lua_p0,lua_p45")

(define_insn_reservation "lua_fsgn" 1
			 (and (eq_attr "cpu" "lujiazui")
			      (eq_attr "type" "fsgn"))
			 "lua_decodern,lua_p0")

(define_insn_reservation "lua_fistp" 1
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "none")
				   (eq_attr "type" "fistp")))
			 "lua_decodern,lua_p0")

(define_insn_reservation "lua_fistp_mem" 4
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "!none")
				   (eq_attr "type" "fistp")))
			 "lua_decoder0,lua_p0+lua_p45")

(define_insn_reservation "lua_fcmov" 3
			 (and (eq_attr "cpu" "lujiazui")
			      (eq_attr "type" "fcmov"))
			 "lua_decodern,lua_p0")

(define_insn_reservation "lua_fcmp" 1
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "none")
				   (eq_attr "type" "fcmp")))
			 "lua_decodern,lua_p0")

(define_insn_reservation "lua_fcmp_load" 5
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "load")
				   (eq_attr "type" "fcmp")))
			 "lua_decoder01,lua_p45,lua_p0")

(define_insn_reservation "lua_fmov" 1
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "none")
				   (eq_attr "type" "fmov")))
			 "lua_decodern,lua_p0")

(define_insn_reservation "lua_fmov_load" 4
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "load")
				   (and (eq_attr "mode" "!XF")
					(eq_attr "type" "fmov"))))
			 "lua_decoder01,lua_p45,lua_p0")

(define_insn_reservation "lua_fmov_XF_load" 3
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "load")
				   (and (eq_attr "mode" "XF")
					(eq_attr "type" "fmov"))))
			 "lua_decoder0,lua_p45,lua_p0")

(define_insn_reservation "lua_fmov_store" 1
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "store")
				   (and (eq_attr "mode" "!XF")
					(eq_attr "type" "fmov"))))
			 "lua_decoder0,lua_p0,lua_p45")

(define_insn_reservation "lua_fmov_XF_store" 1
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "store")
				   (and (eq_attr "mode" "XF")
					(eq_attr "type" "fmov"))))
			 "lua_decoder0,lua_p0,lua_p45")

(define_insn_reservation "lua_fmul" 4
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "none")
				   (eq_attr "type" "fmul")))
			 "lua_decodern,lua_p3")

(define_insn_reservation "lua_fmul_load" 8
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "fp_int_src" "false")
				   (and (eq_attr "memory" "load")
					(eq_attr "type" "fmul"))))
			 "lua_decoder01,lua_p45,lua_p3")

(define_insn_reservation "lua_fimul_load" 8
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "fp_int_src" "true")
				   (and (eq_attr "memory" "load")
					(eq_attr "type" "fmul"))))
			 "lua_decoder0,lua_p45,lua_p3")

;; fdiv instructions.

(define_insn_reservation "lua_fdiv_SF" 15
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "none")
				   (and (eq_attr "mode" "SF")
				    (eq_attr "type" "fdiv,fpspc"))))
			 "lua_decodern,lua_p0,lua_div*15")

(define_insn_reservation "lua_fdiv_SF_load" 19
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "load")
				   (and (eq_attr "mode" "SF")
				    (eq_attr "type" "fdiv,fpspc"))))
			 "lua_decoder01,lua_p45,lua_p0,lua_div*15")

(define_insn_reservation "lua_fdiv_DF" 18
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "none")
				   (and (eq_attr "mode" "DF")
				    (eq_attr "type" "fdiv,fpspc"))))
			 "lua_decodern,lua_p0,lua_div*18")

(define_insn_reservation "lua_fdiv_DF_load" 22
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "load")
				   (and (eq_attr "mode" "DF")
				    (eq_attr "type" "fdiv,fpspc"))))
			 "lua_decoder01,lua_p45,lua_p0,lua_div*18")

(define_insn_reservation "lua_fdiv_XF" 22
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "none")
				   (and (eq_attr "mode" "XF")
				    (eq_attr "type" "fdiv,fpspc"))))
			 "lua_decoder0,lua_p0,lua_div*22")

(define_insn_reservation "lua_fdiv_XF_load" 26
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "load")
				   (and (eq_attr "mode" "XF")
				    (eq_attr "type" "fdiv,fpspc"))))
			 "lua_decoder0,lua_p45,lua_p0,lua_div*22")

;; MMX instructions.

(define_insn_reservation "lua_mmx_sse_add_shft" 1
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "none")
			 (eq_attr "type" "mmxadd,sseiadd,mmxshft,sseishft")))
			 "lua_decodern,lua_p0")

(define_insn_reservation "lua_mmx_sse_add_shft_load" 5
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "load")
			 (eq_attr "type" "mmxadd,sseiadd,mmxshft,sseishft")))
			 "lua_decoder01,lua_p45,lua_p0")

(define_insn_reservation "lua_mmx_sse_add_shft_store" 1
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "store")
			 (eq_attr "type" "mmxadd,sseiadd,mmxshft,sseishft")))
			 "lua_decodern,lua_p0,lua_p45")

(define_insn_reservation "lua_mmx_mul" 5
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "none")
				   (eq_attr "type" "mmxmul,sseimul")))
			 "lua_decodern,lua_p3")

(define_insn_reservation "lua_mmx_mul_load" 9
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "load")
				   (eq_attr "type" "mmxmul,sseimul")))
			 "lua_decoder01,lua_p45,lua_p3")

(define_insn_reservation "lua_mmxcvt" 1
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "none")
				   (eq_attr "type" "mmxcvt")))
			 "lua_decodern,lua_p03")

(define_insn_reservation "lua_mmxcvt_load" 5
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "load")
				   (eq_attr "type" "mmxcvt")))
			 "lua_decoder01,lua_p45,lua_p03")

;; The sfence instruction.
(define_insn_reservation "lua_sse_sfence" 13
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "unknown")
				   (eq_attr "type" "sse")))
			 "lua_decoder0,lua_p45")

(define_insn_reservation "lua_sse_SFDF" 5
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "mode" "SF,DF")
				   (eq_attr "type" "sse")))
			 "lua_decodern,lua_p0")

(define_insn_reservation "lua_sse_V4SF" 13
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "mode" "V4SF")
				   (eq_attr "type" "sse")))
			 "lua_decodern,lua_p0")

(define_insn_reservation "lua_sse_V8SF" 19
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "mode" "V8SF,V4DF")
				   (eq_attr "type" "sse")))
			 "lua_decodern,lua_p0")

(define_insn_reservation "lua_sse_add1" 5
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "none")
				   (eq_attr "type" "sseadd1")))
			 "lua_decoder0,lua_p0")

(define_insn_reservation "lua_sse_add1_load" 8
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "load")
				   (eq_attr "type" "sseadd1")))
			 "lua_decoder0,lua_p45,lua_p0")

(define_insn_reservation "lua_sse_cmp" 3
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "none")
				   (eq_attr "type" "ssecmp,ssecomi")))
			 "lua_decodern,lua_p0")

(define_insn_reservation "lua_sse_cmp_load" 7
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "load")
				   (eq_attr "type" "ssecmp,ssecomi")))
			 "lua_decoder01,lua_p45,lua_p0")

(define_insn_reservation "lua_sse_logic" 1
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "none")
				   (eq_attr "type" "sselog,sselog1")))
			 "lua_decodern,lua_p03")

(define_insn_reservation "lua_sse_logic_load" 5
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "load")
				   (eq_attr "type" "sselog,sselog1")))
			 "lua_decoder01,lua_p45,lua_p03")

(define_insn_reservation "lua_sse_add" 3
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "none")
				   (eq_attr "type" "sseadd")))
			 "lua_decodern,lua_p0")

(define_insn_reservation "lua_sse_add_load" 7
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "load")
				   (eq_attr "type" "sseadd")))
			 "lua_decoder01,lua_p45,lua_p0")

(define_insn_reservation "lua_ssemul_ss_ps" 3
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "none")
				   (and (eq_attr "mode" "SF,V4SF,V8SF")
					(eq_attr "type" "ssemul"))))
			 "lua_decodern,lua_p3")

(define_insn_reservation "lua_ssemul_ss_ps_load" 7
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "load")
				   (and (eq_attr "mode" "SF,V4SF,V8SF")
					(eq_attr "type" "ssemul"))))
			 "lua_decoder01,lua_p45,lua_p3")

(define_insn_reservation "lua_ssemul_sd_pd" 4
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "none")
				   (and (eq_attr "mode" "DF,V2DF,V4DF")
					(eq_attr "type" "ssemul"))))
			 "lua_decodern,lua_p3")

(define_insn_reservation "lua_ssemul_sd_pd_load" 8
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "load")
				   (and (eq_attr "mode" "DF,V2DF,V4DF")
					(eq_attr "type" "ssemul"))))
			 "lua_decoder01,lua_p45,lua_p3")

(define_insn_reservation "lua_ssediv_SF" 13
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "none")
				   (and (eq_attr "mode" "SF")
					(eq_attr "type" "ssediv"))))
			 "lua_decodern,lua_p0,lua_div*13")

(define_insn_reservation "lua_ssediv_load_SF" 17
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "load")
				   (and (eq_attr "mode" "SF")
					(eq_attr "type" "ssediv"))))
			 "lua_decoder01,lua_p45,lua_p0,lua_div*13")

(define_insn_reservation "lua_ssediv_V4SF" 23
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "none")
				   (and (eq_attr "mode" "V4SF")
					(eq_attr "type" "ssediv"))))
			 "lua_decodern,lua_p0,lua_div*23")

(define_insn_reservation "lua_ssediv_load_V4SF" 27
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "load")
				   (and (eq_attr "mode" "V4SF")
					(eq_attr "type" "ssediv"))))
			 "lua_decoder01,lua_p45,lua_p0,lua_div*23")

(define_insn_reservation "lua_ssediv_V8SF" 47
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "none")
				   (and (eq_attr "mode" "V8SF")
					(eq_attr "type" "ssediv"))))
			 "lua_decoder0,lua_p0,lua_div*47")

(define_insn_reservation "lua_ssediv_load_V8SF" 51
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "load")
				   (and (eq_attr "mode" "V8SF")
					(eq_attr "type" "ssediv"))))
			 "lua_decoder0,lua_p45,lua_p0,lua_div*47")

(define_insn_reservation "lua_ssediv_SD" 17
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "none")
				   (and (eq_attr "mode" "DF")
					(eq_attr "type" "ssediv"))))
			 "lua_decodern,lua_p0,lua_div*17")

(define_insn_reservation "lua_ssediv_load_SD" 21
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "load")
				   (and (eq_attr "mode" "DF")
					(eq_attr "type" "ssediv"))))
			 "lua_decoder01,lua_p45,lua_p0,lua_div*17")

(define_insn_reservation "lua_ssediv_V2DF" 30
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "none")
				   (and (eq_attr "mode" "V2DF")
					(eq_attr "type" "ssediv"))))
			 "lua_decodern,lua_p0,lua_div*30")

(define_insn_reservation "lua_ssediv_load_V2DF" 34
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "load")
				   (and (eq_attr "mode" "V2DF")
					(eq_attr "type" "ssediv"))))
			 "lua_decoder01,lua_p45,lua_p0,lua_div*30")

(define_insn_reservation "lua_ssediv_V4DF" 56
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "none")
				   (and (eq_attr "mode" "V4DF")
					(eq_attr "type" "ssediv"))))
			 "lua_decoder0,lua_p0,lua_div*56")

(define_insn_reservation "lua_ssediv_load_V4DF" 60
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "load")
				   (and (eq_attr "mode" "V4DF")
					(eq_attr "type" "ssediv"))))
			 "lua_decoder0,lua_p4p5,lua_p0,lua_div*56")


(define_insn_reservation "lua_sseicvt_si" 2
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "none")
				   (and (eq_attr "mode" "SI")
			 (and (match_operand:SF 1 "memory_operand")
					(eq_attr "type" "sseicvt")))))
			 "lua_decoder01,lua_p0")

(define_insn_reservation "lua_sseicvt_si_load" 6
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "load")
				   (and (eq_attr "mode" "SI")
			 (and (match_operand:SF 1 "memory_operand")
					(eq_attr "type" "sseicvt")))))
			 "lua_decoder0,lua_p45,lua_p0")

(define_insn_reservation "lua_sseicvtdf_si" 3
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "none")
				   (and (eq_attr "mode" "SI")
			 (and (match_operand:DF 1 "memory_operand")
					(eq_attr "type" "sseicvt")))))
			 "lua_decodern,lua_p0")

(define_insn_reservation "lua_sseicvtdf_si_load" 7
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "load")
				   (and (eq_attr "mode" "SI")
			 (and (match_operand:DF 1 "memory_operand")
					(eq_attr "type" "sseicvt")))))
			 "lua_decoder01,lua_p45,lua_p0")

(define_insn_reservation "lua_ssecvt" 6
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "none")
				   (eq_attr "type" "ssecvt")))
			 "lua_decoder01,lua_p03")

(define_insn_reservation "lua_ssecvt_load" 10
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "load")
				   (eq_attr "type" "ssecvt")))
			 "lua_decoder0,lua_p45,lua_p03")

(define_insn_reservation "lua_sse_mov" 1
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "none")
				   (eq_attr "type" "ssemov")))
			 "lua_decodern,lua_p03")

(define_insn_reservation "lua_sse_mov_load" 5
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "load")
				   (eq_attr "type" "ssemov")))
			 "lua_decoder01,lua_p45,lua_p03")

(define_insn_reservation "lua_sse_mov_store" 1
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "store")
				   (eq_attr "type" "ssemov")))
			 "lua_decoder01,lua_p0,lua_p45")

(define_insn_reservation "lua_insn_alu" 1
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "none,unknown")
				   (eq_attr "type" "alu")))
			 "lua_decodern,lua_p12")

(define_insn_reservation "lua_insn_alu_load" 5
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "load")
				   (eq_attr "type" "alu")))
			 "lua_decoder01,lua_p45,lua_p12")

(define_insn_reservation "lua_insn_alu_store" 1
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "store")
				   (eq_attr "type" "alu")))
			 "lua_decoder01,lua_p12,lua_p45")

(define_insn_reservation "lua_insn_alu_both" 5
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "both")
				   (eq_attr "type" "alu")))
			 "lua_decoder0,lua_p45,lua_p12,lua_p45")

(define_insn_reservation "lua_insn_alu1" 1
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "none,unknown")
				   (eq_attr "type" "alu1")))
			 "lua_decodern,lua_p12")

(define_insn_reservation "lua_insn_alu1_load" 5
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "load")
				   (eq_attr "type" "alu1")))
			 "lua_decoder01,lua_p45,lua_p12")

(define_insn_reservation "lua_insn_alu1_store" 1
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "store")
				   (eq_attr "type" "alu1")))
			 "lua_decoder01,lua_p12,lua_p45")

(define_insn_reservation "lua_insn_alu1_both" 5
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "both")
				   (eq_attr "type" "alu1")))
			 "lua_decoder0,lua_p45,lua_p12,lua_p45")

(define_insn_reservation "lua_insn_negnot_incdec" 1
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "none")
				   (eq_attr "type" "negnot,incdec")))
			 "lua_decodern,lua_p12")

(define_insn_reservation "lua_insn_negnot_setcc" 1
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "none")
				   (eq_attr "type" "setcc")))
			 "lua_decodern,lua_p2")

(define_insn_reservation "lua_insn_negnot_setcc_mem" 5
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "!none")
				   (eq_attr "type" "negnot,setcc")))
			 "lua_decoder01,lua_p45,lua_p2,lua_p45")

(define_insn_reservation "lua_insn_incdec_mem" 5
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "!none")
				   (eq_attr "type" "incdec")))
			 "lua_decoder0,lua_p45,lua_p12,lua_p45")

(define_insn_reservation "lua_insn_icmptest" 1
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "none,unknown")
				   (eq_attr "type" "icmp,test")))
			 "lua_decodern,lua_p12")

(define_insn_reservation "lua_insn_icmptest_load" 5
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "load")
				   (eq_attr "type" "icmp,test")))
			 "lua_decoder01,lua_p45,lua_p12")

(define_insn_reservation "lua_insn_icmptest_store" 1
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "store")
				   (eq_attr "type" "icmp,test")))
			 "lua_decoder01,lua_p12,lua_p45")

(define_insn_reservation "lua_insn_icmptest_both" 5
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "both")
				   (eq_attr "type" "icmp,test")))
			 "lua_decoder0,lua_p45,lua_p12,lua_p45")

(define_insn_reservation "lua_insn_sseishft1_mmx" 1
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "none")
				   (eq_attr "type" "sseishft1,mmx,mmxcmp")))
			 "lua_decodern,lua_p03")

(define_insn_reservation "lua_insn_sseishft1_mmx_mem" 5
			 (and (eq_attr "cpu" "lujiazui")
			      (and (eq_attr "memory" "load")
				   (eq_attr "type" "sseishft1,mmx,mmxcmp")))
			 "lua_decoder01,lua_p45,lua_p03")
