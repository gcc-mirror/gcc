;; Toshiba Media Processor Machine description template
;; Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2009 Free
;; Software Foundation, Inc.
;; Contributed by Red Hat Inc
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.  */



;; Constraints:
;;
;;  a   $sp
;;  b   $tp
;;  c   control regs
;;  h   $hi ($23)
;;  l   $lo ($24)
;;  d   $hi/$lo pair (DImode)
;;  j   $rpc ($22)
;;  r   $0..$15
;;  t   $0..$7
;;  v   $gp
;;  x	$c0..$c31
;;  ex  coprocessor registers that can be moved to other coprocessor registers
;;  er  coprocessor registers that can be moved to and from core registers
;;  em  coprocessor registers that can be moves to and from memory
;;  y	$ccr0..$ccr31
;;  z   $0
;;
;;  I   sign imm16	mov/add
;;  J   zero imm16	mov/add
;;  K   zero imm24	mov
;;  L   sign imm6	add
;;  M   zero imm5	slt,shifts
;;  N   zero imm4	bCC
;;  O   high imm16	mov
;;
;;  R   near symbol
;;  S   sign imm8	mov
;;  T   tp or gp relative symbol
;;  U   non-absolute memory
;;  W   %hi(sym)
;;  Y   (Rn)
;;  Z   Control Bus Symbol
;;
;; Modifiers:
;;
;;  b   print unique bit in mask
;;  B   print bits required for value (for clip)
;;  h	print decimal >> 16.
;;  I   print decimal, with hex comment if more than 8 bits
;;  J   print unsigned hex
;;  L   print set, clr or not (for bitops)
;;  P	print memory as a post-inc with no increment
;;  U   print bits required for value (for clipu)
;;  x   print unsigned decimal or hex, depending on where set bits are

(define_constants [
		   (REGSAVE_CONTROL_TEMP 11)
		   (FP_REGNO 8)
		   (TP_REGNO 13)
		   (GP_REGNO 14)
		   (SP_REGNO 15)
		   (PSW_REGNO 16)
		   (LP_REGNO 17)
		   (SAR_REGNO 18)
		   (RPB_REGNO 20)
		   (RPE_REGNO 21)
		   (RPC_REGNO 22)
		   (HI_REGNO 23)
		   (LO_REGNO 24)
		   (CBCR_REGNO 81)
		   ])

(define_constants [
		   (UNS_BLOCKAGE 0)
		   (UNS_TPREL 2)
		   (UNS_GPREL 3)
		   (UNS_REPEAT_BEG 4)
		   (UNS_REPEAT_END 5)
		   (UNS_EH_EPILOGUE 6)
		   (UNS_EREPEAT_BEG 7)
		   (UNS_EREPEAT_END 8)
		   (UNS_BB_TRACE_RET 9)
		   (UNS_DISABLE_INT 10)
		   (UNS_ENABLE_INT 11)
		   (UNS_RETI 12)
		  ])

;; This attribute determines the VLIW packing mechanism.  The IVC2
;; coprocessor has two pipelines (P0 and P1), and a MeP+IVC2 can issue
;; up to three insns at a time.  Most IVC2 insns can run on either
;; pipeline, however, scheduling some insns on P0 precludes packing a
;; core insn with it, and only 16-bit core insns can pack with any P0
;; insn.
(define_attr "vliw" "basic,ivc2"
  (const (symbol_ref "TARGET_IVC2")))

;; This attribute describes the kind of memory operand present in the
;; instruction.  This is used to compute the length of the insn based
;; on the addressing mode used.
(define_attr "memop" "none,core0,core1,cop0,cop1"
  (const_string "none"))

(define_attr "intrinsic" "none,cmov,cmov1,cmov2,cmovc1,cmovc2,cmovh1,cmovh2"
  (const_string "none"))

;; This attribute describes how the instruction may be bundled in a
;; VLIW instruction.  Type MULTI is assumed to use both slots.
(define_attr "slot" "core,cop,multi"
  (cond [(eq_attr "intrinsic" "!none")
	   (const_string "cop")]
	(const_string "core")))

;; This attribute describes the latency of the opcode (ready delay).
;; The 0 is used to indicate "unspecified".  An instruction that
;; completes immediately with no potential stalls would have a value
;; of 1, a one cycle stall would be 2, etc.
(define_attr "latency" ""
  (const_int 0))

(define_attr "shiftop" "none,operand2"
  (const_string "none"))

;; This attribute describes the size of the instruction in bytes.
;; This *must* be exact unless the pattern is SLOT_MULTI, as this
;; is used by the VLIW bundling code.
(define_attr "length" ""
  (cond [(eq_attr "memop" "core0")
	   (symbol_ref "mep_core_address_length (insn, 0)")
	 (eq_attr "memop" "core1")
	   (symbol_ref "mep_core_address_length (insn, 1)")
	 (eq_attr "memop" "cop0")
	   (symbol_ref "mep_cop_address_length (insn, 0)")
	 (eq_attr "memop" "cop1")
	   (symbol_ref "mep_cop_address_length (insn, 1)")
         ]
	 ; Catch patterns that don't define the length properly.
         (symbol_ref "(abort (), 0)")))

;; This attribute describes a pipeline hazard seen in the insn.
(define_attr "stall" "none,int2,ssarb,load,store,ldc,stc,ldcb,stcb,ssrab,fsft,ret,advck,mul,mulr,div"
  (cond [(and (eq_attr "shiftop" "operand2")
	      (not (match_operand:SI 2 "mep_single_shift_operand" "")))
	 (const_string "int2")]
	(const_string "none")))

(define_attr "may_trap" "no,yes"
  (const_string "no"))

;; Describe a user's asm statement.
(define_asm_attributes
  [(set_attr "length" "4")
   (set_attr "slot" "multi")])

;; Each IVC2 instruction uses one of these two pipelines.  P0S insns
;; use P0; C3 insns use P1.
(define_automaton "mep_ivc2")
(define_cpu_unit "ivc2_core,ivc2_p0,ivc2_p1" "mep_ivc2")

;; Each core or IVC2 instruction is bundled into one of these slots.
;; Supported bundlings:
;; 
;; Core mode:
;;
;;  C1	[-----core-----]
;;  C2	[-------------core-------------]
;;  C3	[--------------c3--------------]
;;
;; VLIW mode:
;;
;;  V1	[-----core-----][--------p0s-------][------------p1------------]
;;  V2  [-------------core-------------]xxxx[------------p1------------]
;;  V3	1111[--p0--]0111[--------p0--------][------------p1------------]

(define_attr "slots" "core,c3,p0,p0_p0s,p0_p1,p0s,p0s_p1,p1" (const_string "core"))

(define_cpu_unit "ivc2_slot_c16,ivc2_slot_c32,ivc2_slot_c3,ivc2_slot_p0s,ivc2_slot_p0,ivc2_slot_p1" "mep_ivc2")

(define_insn_reservation "ivc2_insn_core16" 1
  (and (eq_attr "vliw" "ivc2")
       (and (eq (symbol_ref "get_attr_length(insn)") (const_int 2))
	    (and (eq_attr "intrinsic" "none")
		 (eq_attr "slot" "!cop"))))
  "ivc2_core+ivc2_slot_c16")

(define_insn_reservation "ivc2_insn_core32" 1
  (and (eq_attr "vliw" "ivc2")
       (and (eq (symbol_ref "get_attr_length(insn)") (const_int 4))
	    (and (eq_attr "intrinsic" "none")
		 (eq_attr "slot" "!cop"))))
  "ivc2_core+ivc2_slot_c32")

;; These shouldn't happen when in VLIW mode.
(define_insn_reservation "ivc2_insn_c3" 1
  (and (eq_attr "vliw" "ivc2")
       (eq_attr "slots" "c3"))
  "ivc2_p1+ivc2_slot_c3")

(define_insn_reservation "ivc2_insn_p0" 1
  (and (eq_attr "vliw" "ivc2")
       (eq_attr "slots" "p0"))
  "ivc2_p0+ivc2_slot_p0")

(define_insn_reservation "ivc2_insn_p0_p0s" 1
  (and (eq_attr "vliw" "ivc2")
       (eq_attr "slots" "p0_p0s"))
  "ivc2_p0+ivc2_slot_p0|ivc2_p0+ivc2_slot_p0s")

(define_insn_reservation "ivc2_insn_p0_p1" 1
  (and (eq_attr "vliw" "ivc2")
       (eq_attr "slots" "p0_p1"))
  "ivc2_p0+ivc2_slot_p0|ivc2_p1+ivc2_slot_p1")

(define_insn_reservation "ivc2_insn_p0s" 1
  (and (eq_attr "vliw" "ivc2")
       (eq_attr "slots" "p0s"))
  "ivc2_p0+ivc2_slot_p0s")

(define_insn_reservation "ivc2_insn_p0s_p1" 1
  (and (eq_attr "vliw" "ivc2")
       (eq_attr "slots" "p0s_p1"))
  "ivc2_p0+ivc2_slot_p0s|ivc2_p1+ivc2_slot_p1")

(define_insn_reservation "ivc2_insn_p1" 1
  (and (eq_attr "vliw" "ivc2")
       (eq_attr "slots" "p1"))
  "ivc2_p1+ivc2_slot_p1")

;; these run in C3 also, but when we're doing VLIW scheduling, they
;; only run in P0.
(define_insn_reservation "ivc2_insn_cmov" 1
  (and (eq_attr "vliw" "ivc2")
       (eq_attr "intrinsic" "!none"))
  "ivc2_p0+ivc2_slot_p0")


(exclusion_set "ivc2_slot_c32"
	       "ivc2_slot_p0,ivc2_slot_p0s")
(exclusion_set "ivc2_slot_p0"
	       "ivc2_slot_p0s")
(exclusion_set "ivc2_slot_c16"
	       "ivc2_slot_p0")
(exclusion_set "ivc2_slot_c16"
	       "ivc2_slot_c32")

;; Non-IVC2 scheduling.
(define_automaton "mep")
(define_cpu_unit "core,cop" "mep")

;; Latencies are the time between one insn entering the second pipeline
;; stage (E2, LD, A2 or V2) and the next instruction entering the same
;; stage.  When an instruction assigns to general registers, the default
;; latencies are for when the next instruction receives the register
;; through bypass 1.

;; Arithmetic instructions that execute in a single stage.
(define_insn_reservation "h1_int1" 2
  (and (eq_attr "slot" "!cop")
       (eq_attr "stall" "none"))
  "core")
(define_bypass 1 "h1_int1" "h1_int1,h1_ssarb")
(define_bypass 1 "h1_int1" "h1_store" "mep_store_data_bypass_p")

;; $sar can be read by an immediately following fsft or ldc.
(define_insn_reservation "h1_ssarb" 1
  (eq_attr "stall" "ssarb")
  "core")

;; Arithmetic instructions that execute in two stages.
(define_insn_reservation "h1_int2" 2
  (eq_attr "stall" "int2,fsft")
  "core")
(define_bypass 1 "h1_int2" "h1_int1,h1_ssarb")
(define_bypass 1 "h1_int2" "h1_store" "mep_store_data_bypass_p")

(define_insn_reservation "h1_load" 4
  (eq_attr "stall" "load")
  "core")
(define_bypass 3 "h1_load" "h1_int1,h1_ssarb")
(define_bypass 3 "h1_load" "h1_store" "mep_store_data_bypass_p")

(define_insn_reservation "h1_store" 1
  (eq_attr "stall" "store")
  "core")

(define_insn_reservation "h1_ipipe_ldc" 2
  (and (eq_attr "stall" "ldc")
       (ne (symbol_ref "mep_ipipe_ldc_p(insn)") (const_int 0)))
  "core")
(define_bypass 1 "h1_ipipe_ldc" "h1_int1,h1_ssarb")
(define_bypass 1 "h1_ipipe_ldc" "h1_store" "mep_store_data_bypass_p")

(define_insn_reservation "h1_apipe_ldc" 2
  (and (eq_attr "stall" "ldc")
       (eq (symbol_ref "mep_ipipe_ldc_p(insn)") (const_int 0)))
  "core")

;; 2 is correct for stc->ret and stc->fsft.  The most important remaining
;; case is stc->madd, which induces no stall.
(define_insn_reservation "h1_stc" 2
  (eq_attr "stall" "stc")
  "core")
(define_bypass 1 "h1_stc" "h1_mul")

;; ??? Parameterised latency.
(define_insn_reservation "h1_ldcb" 5
  (eq_attr "stall" "ldcb")
  "core")

(define_insn_reservation "h1_stcb" 1
  (eq_attr "stall" "stcb")
  "core")

(define_insn_reservation "h1_advck" 6
  (eq_attr "stall" "advck")
  "core")

(define_insn_reservation "h1_mul" 5
  (eq_attr "stall" "mul,mulr")
  "core")
(define_bypass 4 "h1_mul" "h1_int1,h1_ssarb")
(define_bypass 4 "h1_mul" "h1_store" "mep_store_data_bypass_p")
(define_bypass 1 "h1_mul" "h1_mul" "mep_mul_hilo_bypass_p")

(define_insn_reservation "h1_div" 36
  (eq_attr "stall" "div")
  "core")

(define_insn_reservation "h1_cop" 1
  (eq_attr "slot" "cop")
  "cop")

(include "predicates.md")
(include "constraints.md")
(include "intrinsics.md")

;; ::::::::::::::::::::
;; ::
;; :: Moves
;; ::
;; ::::::::::::::::::::

(define_expand "movqi"
  [(set (match_operand:QI 0 "general_operand" "")
	(match_operand:QI 1 "general_operand" ""))]
  ""
  "
{
  if (mep_expand_mov (operands, QImode))
    DONE;
}")

;; The Idea here is to prefer the 16-bit tp-relative load, but to fall back
;; to the general 32-bit load rather than do silly things with spill regs.
(define_insn "*movqi_tprel_load"
  [(set (match_operand:QI 0 "mep_tprel_operand" "=t,*r")
	(mem:QI (plus:SI (match_operand:SI 1 "mep_tp_operand" "b,*r")
			 (const:SI (unspec:SI [(match_operand:SI 2
						"symbolic_operand" "s,s")]
					      UNS_TPREL)))))]
  ""
  "lb\\t%0, %%tpoff(%2)(%1)"
  [(set_attr "length" "2,4")
   (set_attr "stall" "load")])

(define_insn "*movqi_tprel_store"
  [(set (mem:QI (plus:SI (match_operand:SI 0 "mep_tp_operand" "b,*r")
			 (const:SI (unspec:SI [(match_operand:SI 1
						"symbolic_operand" "s,s")]
					      UNS_TPREL))))
	(match_operand:QI 2 "mep_tprel_operand" "t,*r"))]
  ""
  "sb\\t%2, %%tpoff(%1)(%0)"
  [(set_attr "length" "2,4")
   (set_attr "stall" "store")])

(define_insn "*movqi_internal"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=r,r, r,m,r,c,r,y,r,er,ex,em,Y")
	(match_operand:QI 1 "general_operand" " r,n,rm,r,c,r,y,r,er,r,ex,Y,em"))]
  "mep_mov_ok (operands, QImode)"
  "@
   mov\\t%0, %1
   mov\\t%0, %1
   lb\\t%0, %1
   sb\\t%1, %0
   ldc\\t%0, %1
   stc\\t%1, %0
   cmovc\\t%0, %1
   cmovc\\t%0, %1
   cmov\\t%0, %1
   cmov\\t%0, %1
   %<\\t%0, %M1
   lbcpa\\t%0, %P1
   sbcpa\\t%1, %P0"
  [(set_attr "length" "2,2,*,*,2,2,4,4,4,4,*,4,4")
   (set_attr "intrinsic" "*,*,*,*,*,*,cmovc2,cmovc1,cmov2,cmov1,cmov,*,*")
   (set_attr "stall"  "*,*,load,store,ldc,stc,*,*,*,*,*,load,store")
   (set_attr "memop"  "*,*,core1,core0,*,*,*,*,*,*,*,*,*")])

(define_expand "movhi"
  [(set (match_operand:HI 0 "general_operand" "")
	(match_operand:HI 1 "general_operand" ""))]
  ""
  "
{
  if (mep_expand_mov (operands, HImode))
    DONE;
}")

(define_insn "*movhi_tprel_load"
  [(set (match_operand:HI 0 "mep_tprel_operand" "=t,*r")
	(mem:HI (plus:SI (match_operand:SI 1 "mep_tp_operand" "b,*r")
			 (const:SI (unspec:SI [(match_operand:SI 2
						"symbolic_operand" "s,s")]
					      UNS_TPREL)))))]
  ""
  "lh\\t%0, %%tpoff(%2)(%1)"
  [(set_attr "length" "2,4")
   (set_attr "stall" "load")])

(define_insn "*movhi_tprel_store"
  [(set (mem:HI (plus:SI (match_operand:SI 0 "mep_tp_operand" "b,*r")
			 (const:SI (unspec:SI [(match_operand:SI 1
						"symbolic_operand" "s,s")]
					      UNS_TPREL))))
	(match_operand:HI 2 "mep_tprel_operand" "t,*r"))]
  ""
  "sh\\t%2, %%tpoff(%1)(%0)"
  [(set_attr "length" "2,4")
   (set_attr "stall" "store")])

(define_insn "*movhi_internal"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=r,r,r,r,m,r,c,r,y,r,er,ex,em,Y")
	(match_operand:HI 1 "general_operand" " r,S,n,m,r,c,r,y,r,er,r,ex,Y,em"))]
  "mep_mov_ok (operands, HImode)"
  "@
   mov\\t%0, %1
   mov\\t%0, %I1
   mov\\t%0, %I1
   lh\\t%0, %1
   sh\\t%1, %0
   ldc\\t%0, %1
   stc\\t%1, %0
   cmovc\\t%0, %1
   cmovc\\t%0, %1
   cmov\\t%0, %1
   cmov\\t%0, %1
   %<\\t%0, %M1
   lhcpa\\t%0, %P1
   shcpa\\t%1, %P0"
  [(set_attr "length" "2,2,4,*,*,2,2,4,4,4,4,*,4,4")
   (set_attr "intrinsic" "*,*,*,*,*,*,*,cmovc2,cmovc1,cmov2,cmov1,cmov,*,*")
   (set_attr "stall"  "*,*,*,load,store,ldc,stc,*,*,*,*,*,load,store")
   (set_attr "memop"  "*,*,*,core1,core0,*,*,*,*,*,*,*,*,*")])

(define_expand "movsi"
  [(set (match_operand:SI 0 "nonimmediate_operand" "")
	(match_operand:SI 1 "general_operand" ""))]
  ""
  "
{
  if (mep_expand_mov (operands, SImode))
    DONE;
}")

(define_insn "*movsi_tprel_load"
  [(set (match_operand:SI 0 "mep_tprel_operand" "=t,*r")
	(mem:SI (plus:SI (match_operand:SI 1 "mep_tp_operand" "b,*r")
			 (const:SI (unspec:SI [(match_operand:SI 2
						"symbolic_operand" "s,s")]
					      UNS_TPREL)))))]
  ""
  "lw\\t%0, %%tpoff(%2)(%1)"
  [(set_attr "length" "2,4")
   (set_attr "stall" "load")])

(define_insn "*movsi_tprel_store"
  [(set (mem:SI (plus:SI (match_operand:SI 0 "mep_tp_operand" "b,*r")
			 (const:SI (unspec:SI [(match_operand:SI 1
						"symbolic_operand" "s,s")]
					      UNS_TPREL))))
	(match_operand:SI 2 "mep_tprel_operand" "t,*r"))]
  ""
  "sw\\t%2, %%tpoff(%1)(%0)"
  [(set_attr "length" "2,4")
   (set_attr "stall" "store")])

(define_insn "movsi_topsym_s"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(high:SI (match_operand:SI 1 "symbolic_operand" "s")))]
  ""
  "movh\\t%0, %%hi(%1)"
  [(set_attr "length" "4")])

(define_insn "movsi_botsym_s"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(lo_sum:SI (match_operand:SI 1 "register_operand" "0")
		   (match_operand:SI 2 "symbolic_operand" "s")))]
  ""
  "add3\\t%0, %1, %%lo(%2)"
  [(set_attr "length" "4")])



(define_insn "cmovh_getsub"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(subreg:SI (match_operand:DI 1 "register_operand" "er") 4))]
  "0 && TARGET_64BIT_CR_REGS"
  "cmovh\\t%0, %1"
  [(set_attr "intrinsic" "cmovh2")
   (set_attr "length" "4")])

(define_insn "*movsi_internal"
  [(set (match_operand:SI 0 "mep_movdest_operand"
	    "=r,r,r,r,r, t,t,r,r,r,Z,m,r,c,r,y,r, er,ex,em,U ")
	(match_operand:SI 1 "general_operand"
	    " r,S,I,J,OW,K,s,i,Z,m,r,r,c,r,y,r,er,r, ex,U, em"))]
  "mep_mov_ok (operands, SImode)"
  "@
   mov\\t%0, %1
   mov\\t%0, %I1
   mov\\t%0, %I1
   movu\\t%0, %J1
   movh\\t%0, %h1
   movu\\t%0, %x1
   movu\\t%0, %1
   #
   ldcb\\t%0, %1
   lw\\t%0, %1
   stcb\\t%1, %0
   sw\\t%1, %0
   ldc\\t%0, %1
   stc\\t%1, %0
   cmovc\\t%0, %1
   cmovc\\t%0, %1
   cmov\\t%0, %1
   cmov\\t%0, %1
   %<\\t%0, %M1
   lwcp\\t%0, %1
   swcp\\t%1, %0"
  [(set_attr "length" "2,2,4,4,4,4,4,*,4,*,4,*,2,2,4,4,4,4,4,*,*")
   (set_attr "intrinsic" "*,*,*,*,*,*,*,*,*,*,*,*,*,*,cmovc2,cmovc1,cmov2,cmov1,cmov,*,*")
   (set_attr "stall"  "*,*,*,*,*,*,*,*,ldcb,load,stcb,store,ldc,stc,*,*,*,*,*,load,store")
   (set_attr "memop"  "*,*,*,*,*,*,*,*,*,core1,*,core0,*,*,*,*,*,*,*,cop1,cop0")
   (set_attr "slot"   "*,*,*,*,*,*,*,multi,*,*,*,*,*,*,*,*,*,*,*,*,*")])

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
        (match_operand:SI 1 "const_int_operand" ""))]
  "mep_split_mov (operands, 0)"
  [(set (match_dup 0) (match_dup 2))
   (set (match_dup 0) (ior:SI (match_dup 0) (match_dup 3)))]
  "
{
  HOST_WIDE_INT value;
  int lo, hi;

  value = INTVAL (operands[1]);

  lo = value & 0xffff;
  hi = trunc_int_for_mode (value & 0xffff0000, SImode);

  operands[2] = GEN_INT (hi);
  operands[3] = GEN_INT (lo);
}")

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
        (match_operand:SI 1 "immediate_operand" ""))]
  "mep_split_mov (operands, 1)"
  [(set (match_dup 0) (high:SI (match_dup 1)))
   (set (match_dup 0) (lo_sum:SI (match_dup 0) (match_dup 1)))]
  "")

;; ??? What purpose do these two serve that high+lo_sum do not?
(define_insn "movsi_topsym_u"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(and:SI (match_operand:SI 1 "symbolic_operand" "s")
		(const_int -65536)))]
  ""
  "movh\\t%0, %%uhi(%1)"
  [(set_attr "length" "4")])

(define_insn "movsi_botsym_u"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ior:SI (match_operand:SI 1 "register_operand" "0")
		(and:SI (match_operand:SI 2 "symbolic_operand" "s")
			(const_int 65535))))]
  ""
  "or3\\t%0, %1, %%lo(%2)"
  [(set_attr "length" "4")])

(define_expand "movdi"
  [(set (match_operand:DI 0 "" "")
	(match_operand:DI 1 "" ""))]
  ""
  "
{
  if (mep_expand_mov (operands, DImode))
    DONE;
}")

(define_insn "*movdi_internal_32"
  [(set (match_operand:DI 0 "mep_movdest_operand" "= r,m,r,c,r,er,ex,em,U")
	(match_operand:DI 1 "general_operand"     "rim,r,c,r,er,r,ex,U,em"))]
  "TARGET_32BIT_CR_REGS && mep_mov_ok (operands, DImode)"
  "#"
  [(set_attr "slot" "multi")])

(define_insn "*movdi_internal_64"
  [(set (match_operand:DI 0 "mep_movdest_operand" "=r,r,m,r,c,r,er,ex,em,U")
	(match_operand:DI 1 "general_operand"     "r,im,r,c,r,er,r,ex,U,em"))]
  "TARGET_64BIT_CR_REGS && mep_mov_ok (operands, DImode)"
  "@
   #
   #
   #
   #
   #
   #
   #
   %<\\t%0, %M1
   lmcp\\t%0, %1
   smcp\\t%1, %0"
  [(set_attr "slot"  "multi,multi,multi,multi,multi,multi,multi,*,*,*")
   (set_attr "intrinsic" "*,*,*,*,*,*,*,cmov,*,*")
   (set_attr "memop" "*,*,*,*,*,*,*,cop0,cop1,cop0")
   (set_attr "stall" "*,*,*,*,*,*,*,*,load,store")])

(define_insn "*movdi_cop_postinc"
  [(parallel [(set (match_operand:DI 0 "register_operand" "=em")
		   (mem:DI (reg:SI SP_REGNO)))
	      (set (reg:SI SP_REGNO)
		   (plus:SI (reg:SI SP_REGNO)
			    (const_int 8)))
	      ]
	     )]
  "TARGET_COP"
  "lmcpi\\t%0,($sp+)"
  [(set_attr "length" "2")])

(define_insn "*movdi_cop_postinc"
  [(parallel [(set (match_operand:DI 0 "register_operand" "=em")
		   (mem:DI (match_operand:SI 2 "register_operand" "r")))
	      (set (match_operand:SI 1 "register_operand" "=0")
		   (plus:SI (match_operand:SI 3 "register_operand" "0")
			    (const_int 8)))
	      ]
	     )]
  "TARGET_COP"
  "lmcpi\\t%0,(%1+)"
  [(set_attr "length" "2")])

(define_insn "*cmovh_set"
  [(set (zero_extract:SI (match_operand:DI 0 "register_operand" "+er")
			 (const_int 32)
			 (const_int 32))
	(match_operand:SI 1 "register_operand" "r"))]
  "TARGET_64BIT_CR_REGS"
  "cmovh\\t%0, %1"
  [(set_attr "intrinsic" "cmovh1")
   (set_attr "length" "4")])

(define_insn "cmovh_get"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(zero_extract:SI (match_operand:DI 1 "register_operand" "er")
			 (const_int 32)
			 (const_int 32)))]
  "TARGET_64BIT_CR_REGS"
  "cmovh\\t%0, %1"
  [(set_attr "intrinsic" "cmovh2")
   (set_attr "length" "4")])

(define_split
  [(set (match_operand:DI 0 "mep_movdest_operand" "")
        (match_operand:DI 1 "general_operand" ""))]
  "reload_completed && mep_multi_slot (insn)"
  [(set (match_dup 2) (match_dup 3))
   (set (match_dup 4) (match_dup 5))]
  "mep_split_wide_move (operands, DImode);")

;; Floating Point Moves

(define_expand "movsf"
  [(set (match_operand:SF 0 "nonimmediate_operand" "")
	(match_operand:SF 1 "general_operand" ""))]
  ""
  "
{
  if (mep_expand_mov (operands, SFmode))
    DONE;
}")

(define_insn "*movsf_tprel_load"
  [(set (match_operand:SF 0 "mep_tprel_operand" "=t,*r")
	(mem:SF (plus:SI (match_operand:SI 1 "mep_tp_operand" "b,*r")
			 (const:SI (unspec:SI [(match_operand:SI 2
						"symbolic_operand" "s,s")]
					      UNS_TPREL)))))]
  ""
  "lw\\t%0, %%tpoff(%2)(%1)"
  [(set_attr "length" "2,4")
   (set_attr "stall" "load")])

(define_insn "*movsf_tprel_store"
  [(set (mem:SF (plus:SI (match_operand:SI 0 "mep_tp_operand" "b,*r")
			 (const:SI (unspec:SI [(match_operand:SI 1
						"symbolic_operand" "s,s")]
					      UNS_TPREL))))
	(match_operand:SF 2 "mep_tprel_operand" "t,*r"))]
  ""
  "sw\\t%2, %%tpoff(%1)(%0)"
  [(set_attr "length" "2,4")
   (set_attr "stall" "store")])

(define_insn "*movsf_internal"
  [(set (match_operand:SF 0 "mep_movdest_operand"
	    "=r,r,r,r,Z,m,r,c,r,y,r,er,ex,em,U")
	(match_operand:SF 1 "general_operand"
	    " r,F,Z,m,r,r,c,r,y,r,er,r,ex,U,em"))]
  "mep_mov_ok (operands, SFmode)"
  "@
   mov\\t%0, %1
   #
   ldcb\\t%0, %1
   lw\\t%0, %1
   stcb\\t%1, %0
   sw\\t%1, %0
   ldc\\t%0, %1
   stc\\t%1, %0
   cmovc\\t%0, %1
   cmovc\\t%0, %1
   cmov\\t%0, %1
   cmov\\t%0, %1
   %<\\t%0, %M1
   lwcp\\t%0, %1
   swcp\\t%1, %0"
  [(set_attr "length" "2,*,2,*,2,*,2,2,*,*,4,4,*,*,*")
   (set_attr "intrinsic" "*,*,*,*,*,*,*,*,cmovc2,cmovc1,cmov2,cmov1,cmov,*,*")
   (set_attr "stall"  "*,*,ldcb,load,stcb,store,ldc,stc,*,*,*,*,*,load,store")
   (set_attr "memop"  "*,*,*,core1,*,core0,*,*,*,*,*,*,*,cop1,cop0")])

(define_split
  [(set (match_operand:SF 0 "register_operand" "")
        (match_operand:SF 1 "const_double_operand" ""))]
  "reload_completed"
  [(const_int 0)]
  "
{
  REAL_VALUE_TYPE rv;
  HOST_WIDE_INT value;
  HOST_WIDE_INT lo, hi;
  rtx out;

  REAL_VALUE_FROM_CONST_DOUBLE (rv, operands[1]);
  REAL_VALUE_TO_TARGET_SINGLE (rv, value);

  lo = value & 0xffff;
  hi = trunc_int_for_mode (value & 0xffff0000, SImode);

  out = gen_rtx_REG (SImode, REGNO (operands[0]));
  emit_move_insn (out, GEN_INT (hi));
  if (lo != 0)
    emit_insn (gen_iorsi3 (out, out, GEN_INT (lo)));
  DONE;
}")

(define_expand "movdf"
  [(set (match_operand:DF 0 "" "")
	(match_operand:DF 1 "" ""))]
  ""
  "
{
  if (mep_expand_mov (operands, DFmode))
    DONE;
}")

(define_insn "*movdf_internal_32"
  [(set (match_operand:DF 0 "mep_movdest_operand" "= r,m,r,c,r,er,ex,em,U")
	(match_operand:DF 1 "general_operand"     "rFm,r,c,r,er,r,ex,U,em"))]
  "TARGET_32BIT_CR_REGS && mep_mov_ok (operands, DFmode)"
  "#"
  [(set_attr "slot" "multi")])

(define_insn "*movdf_internal_64"
  [(set (match_operand:DF 0 "mep_movdest_operand" "= r,m,r,c,r,er,ex,em,U")
	(match_operand:DF 1 "general_operand"     "rFm,r,c,r,er,r,ex,U,em"))]
  "TARGET_64BIT_CR_REGS && mep_mov_ok (operands, DFmode)"
  "@
   #
   #
   #
   #
   #
   #
   %<\\t%0, %M1
   lmcp\\t%0, %1
   smcp\\t%1, %0"
  [(set_attr "slot"  "multi,multi,multi,multi,multi,multi,*,*,*")
   (set_attr "intrinsic" "*,*,*,*,*,*,cmov,*,*")
   (set_attr "memop" "*,*,*,*,*,*,*,cop1,cop0")
   (set_attr "stall" "*,*,*,*,*,*,*,load,store")])

(define_split
  [(set (match_operand:DF 0 "mep_movdest_operand" "")
        (match_operand:DF 1 "general_operand" ""))]
  "reload_completed && mep_multi_slot (insn)"
  [(set (match_dup 2) (match_dup 3))
   (set (match_dup 4) (match_dup 5))]
  "mep_split_wide_move (operands, DFmode);")


(define_insn "*lbcpa"
  [(set (match_operand:SI 0 "register_operand" "=em")
	(sign_extend:SI (mem:QI (match_operand:SI 2 "register_operand" "1"))))
   (set (match_operand:SI 1 "register_operand" "=r")
	(plus:SI (match_dup 2)
		 (match_operand:SI 3 "cgen_h_sint_8a1_immediate" "")))]
  "TARGET_COP && reload_completed"
  "lbcpa\t%0, (%1+), %3"
  [(set_attr "length" "4")
   (set_attr "stall" "load")])

(define_insn "*sbcpa"
  [(set (mem:QI (match_operand:SI 1 "register_operand" "0"))
	(match_operand:QI 2 "register_operand" "em"))
   (set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (match_dup 1)
		 (match_operand:SI 3 "cgen_h_sint_8a1_immediate" "")))]
  "TARGET_COP && reload_completed"
  "sbcpa\t%2, (%0+), %3"
  [(set_attr "length" "4")
   (set_attr "stall" "store")])

(define_insn "*lhcpa"
  [(set (match_operand:SI 0 "register_operand" "=em")
	(sign_extend:SI (mem:HI (match_operand:SI 2 "register_operand" "1"))))
   (set (match_operand:SI 1 "register_operand" "=r")
	(plus:SI (match_dup 2)
		 (match_operand:SI 3 "cgen_h_sint_7a2_immediate" "")))]
  "TARGET_COP && reload_completed"
  "lhcpa\t%0, (%1+), %3"
  [(set_attr "length" "4")
   (set_attr "stall" "load")])

(define_insn "*shcpa"
  [(set (mem:HI (match_operand:SI 1 "register_operand" "0"))
	(match_operand:HI 2 "register_operand" "em"))
   (set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (match_dup 1)
		 (match_operand:SI 3 "cgen_h_sint_7a2_immediate" "")))]
  "TARGET_COP && reload_completed"
  "shcpa\t%2, (%0+), %3"
  [(set_attr "length" "4")
   (set_attr "stall" "store")])

(define_insn "*lwcpi"
  [(set (match_operand:SI 0 "register_operand" "=em")
	(mem:SI (match_operand:SI 2 "register_operand" "1")))
   (set (match_operand:SI 1 "register_operand" "=r")
	(plus:SI (match_dup 2)
		 (const_int 4)))]
  "TARGET_COP && reload_completed"
  "lwcpi\t%0, (%1+)"
  [(set_attr "length" "2")
   (set_attr "stall" "load")])

(define_insn "*lwcpa"
  [(set (match_operand:SI 0 "register_operand" "=em")
	(mem:SI (match_operand:SI 2 "register_operand" "1")))
   (set (match_operand:SI 1 "register_operand" "=r")
	(plus:SI (match_dup 2)
		 (match_operand:SI 3 "cgen_h_sint_6a4_immediate" "")))]
  "TARGET_COP && reload_completed"
  "lwcpa\t%0, (%1+), %3"
  [(set_attr "length" "4")
   (set_attr "stall" "load")])

(define_insn "*swcpi"
  [(set (mem:SI (match_operand:SI 1 "register_operand" "0"))
	(match_operand:SI 2 "register_operand" "em"))
   (set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (match_dup 1)
		 (const_int 4)))]
  "TARGET_COP && reload_completed"
  "swcpi\t%2, (%0+)"
  [(set_attr "length" "2")
   (set_attr "stall" "store")])

(define_insn "*swcpa"
  [(set (mem:SI (match_operand:SI 1 "register_operand" "0"))
	(match_operand:SI 2 "register_operand" "em"))
   (set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (match_dup 1)
		 (match_operand:SI 3 "cgen_h_sint_6a4_immediate" "")))]
  "TARGET_COP && reload_completed"
  "swcpa\t%2, (%0+), %3"
  [(set_attr "length" "4")
   (set_attr "stall" "store")])

(define_peephole2
  [(set (match_operand:SI 0 "register_operand" "")
	(plus:SI (match_dup 0)
		 (match_operand:SI 1 "cgen_h_sint_8a1_immediate" "")))]
  "TARGET_COP && mep_use_post_modify_p (insn, operands[0], operands[1])"
  [(const_int 0)]
{
  emit_note (NOTE_INSN_DELETED);
  DONE;
})

;; ::::::::::::::::::::
;; ::
;; :: Reloads
;; ::
;; ::::::::::::::::::::

(define_expand "reload_insi"
  [(set (match_operand:SI 0 "mep_reload_operand" "")
        (match_operand:SI 1 "mep_reload_operand" "r"))
   (clobber (match_operand:SI 2 "register_operand" "=&r"))]
  ""
  "
{
  mep_expand_reload (operands, SImode);
  DONE;
}")

(define_expand "reload_outsi"
  [(set (match_operand:SI 0 "mep_reload_operand" "=r")
        (match_operand:SI 1 "mep_reload_operand" ""))
   (clobber (match_operand:SI 2 "register_operand" "=&r"))]
  ""
  "
{
  mep_expand_reload (operands, SImode);
  DONE;
}")


;; ::::::::::::::::::::
;; ::
;; :: Conversions
;; ::
;; ::::::::::::::::::::

(define_insn "extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=r,r,em")
	(sign_extend:SI
	  (match_operand:QI 1 "nonimmediate_operand" "0,m,Y")))]
  ""
  "@
   extb\\t%0
   lb\\t%0, %1
   lbcpa\\t%0, %P1"
  [(set_attr "length" "2,*,*")
   (set_attr "stall"  "*,load,load")
   (set_attr "memop"  "*,core1,cop1")])

(define_insn "extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "=r,r,em")
	(sign_extend:SI
	  (match_operand:HI 1 "nonimmediate_operand" "0,m,Y")))]
  ""
  "@
   exth\\t%0
   lh\\t%0, %1
   lhcpa\\t%0, %P1"
  [(set_attr "length" "2,*,*")
   (set_attr "stall"  "*,load,load")
   (set_attr "memop"  "*,core1,cop1")])

(define_insn "zero_extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=r,r,r")
	(zero_extend:SI
	  (match_operand:QI 1 "nonimmediate_operand" "0,r,m")))]
  ""
  "@
   extub\\t%0
   and3\\t%0, %1, 255
   lbu\\t%0, %1"
  [(set_attr "length" "2,4,*")
   (set_attr "stall" "*,*,load")
   (set_attr "memop"  "*,*,core1")])

(define_insn "zero_extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "=r,r,r")
	(zero_extend:SI
	  (match_operand:HI 1 "nonimmediate_operand" "0,r,m")))]
  ""
  "@
   extuh\\t%0
   and3\\t%0, %1, 65535
   lhu\\t%0, %1"
  [(set_attr "length" "2,4,*")
   (set_attr "stall" "*,*,load")
   (set_attr "memop"  "*,*,core1")])

;; ::::::::::::::::::::
;; ::
;; :: 32 bit Integer arithmetic
;; ::
;; ::::::::::::::::::::

(define_insn "addsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r,r")
	(plus:SI (match_operand:SI 1 "register_operand" "%r,0,r")
		 (match_operand:SI 2 "mep_add_operand" "r,L,IT")))]
  ""
  "@
   add3\\t%0, %1, %2
   add\\t%0, %2
   add3\\t%0, %1, %I2"
  [(set (attr "length")
	(if_then_else (eq_attr "alternative" "2")
	  (if_then_else (and (match_operand:SI 1 "mep_sp_operand" "")
			     (match_operand:SI 2 "mep_imm7a4_operand" ""))
	    (const_int 2)
	    (const_int 4))
	  (const_int 2)))])

;; The intention here is to combine the 16-bit add with the 16-bit
;; move to create a 32-bit add.  It's the same size, but takes one
;; less machine cycle.  It will happen to match a 32-bit add with a
;; 16-bit move also, but gcc shouldn't be doing that ;)
(define_peephole2
  [(set (match_operand:SI 0 "register_operand" "")
	(plus:SI (match_operand:SI 1 "register_operand" "")
		 (match_operand:SI 2 "immediate_operand" "")))
   (set (match_operand:SI 3 "register_operand" "")
	(match_operand:SI 4 "register_operand" ""))]
  "REGNO (operands[0]) == REGNO (operands[1])
   && REGNO (operands[0]) == REGNO (operands[4])
   && GR_REGNO_P (REGNO (operands[3]))
   && dead_or_set_p (peep2_next_insn (1), operands[4])"
  [(set (match_dup 3)
	(plus:SI (match_dup 1)
		 (match_dup 2)))]
  "")

(define_insn "subsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(minus:SI (match_operand:SI 1 "register_operand" "0")
		  (match_operand:SI 2 "register_operand" "r")))]
  ""
  "sub\\t%0, %2"
  [(set_attr "length" "2")])

(define_expand "mulsi3"
  [(set (match_operand:SI 0 "register_operand" "")
        (mult:SI (match_operand:SI 1 "register_operand" "")
                 (match_operand:SI 2 "register_operand" "")))]
  "TARGET_OPT_MULT || TARGET_COPRO_MULT"
{
  emit_insn (gen_mulsi3_1 (operands[0], operands[1], operands[2]));
  DONE;
})

;; Generated by mep_reuse_lo_p when no GPR destination is needed.
(define_insn "mulsi3_lo"
  [(set (match_operand:SI 0 "mep_lo_operand" "=l")
	(mult:SI (match_operand:SI 1 "register_operand" "r")
		 (match_operand:SI 2 "register_operand" "r")))
   (clobber (match_scratch:SI 3 "=h"))]
  "TARGET_OPT_MULT && reload_completed"
  "mul\\t%1, %2"
  [(set_attr "length" "2")
   (set_attr "stall" "mul")])

;; Generated by mep_reuse_lo_p when both destinations of a mulr
;; are needed.
(define_insn "mulsi3r"
  [(set (match_operand:SI 0 "mep_lo_operand" "=l")
	(mult:SI (match_operand:SI 2 "register_operand" "1")
		 (match_operand:SI 3 "register_operand" "r")))
   (set (match_operand:SI 1 "register_operand" "=r")
	(mult:SI (match_dup 2)
		 (match_dup 3)))
   (clobber (match_scratch:SI 4 "=h"))]
  "TARGET_OPT_MULT && reload_completed"
  "mulr\\t%2, %3"
  [(set_attr "length" "2")
   (set_attr "stall" "mulr")])

(define_insn "mulsi3_1"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(mult:SI (match_operand:SI 1 "register_operand" "%0")
		 (match_operand:SI 2 "register_operand" "r")))
   (clobber (match_scratch:SI 3 "=l"))
   (clobber (match_scratch:SI 4 "=h"))]
  "TARGET_OPT_MULT"
  "mulr\\t%1, %2"
  [(set_attr "length" "2")
   (set_attr "stall" "mulr")])

(define_expand "mulsidi3"
  [(set (match_operand:DI 0 "register_operand" "")
	(mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand" ""))
		 (sign_extend:DI (match_operand:SI 2 "register_operand" ""))))]
  "TARGET_OPT_MULT"
  "
{
  rtx hi = gen_reg_rtx (SImode);
  rtx lo = gen_reg_rtx (SImode);

  emit_insn (gen_mulsidi3_i (hi, lo, operands[1], operands[2]));
  emit_move_insn (gen_lowpart (SImode, operands[0]), lo);
  emit_move_insn (gen_highpart (SImode, operands[0]), hi);
  DONE;
}")

(define_insn "mulsidi3_i"
  [(set (match_operand:SI 0 "mep_hi_operand" "=h")
	(truncate:SI
	 (lshiftrt:DI
	  (mult:DI (sign_extend:DI
		    (match_operand:SI 2 "register_operand" "r"))
		   (sign_extend:DI
		    (match_operand:SI 3 "register_operand" "r")))
	  (const_int 32))))
   (set (match_operand:SI 1 "mep_lo_operand" "=l")
	(mult:SI (match_dup 2)
		 (match_dup 3)))]
  "TARGET_OPT_MULT"
  "mul\\t%2, %3"
  [(set_attr "length" "2")
   (set_attr "stall" "mul")])

(define_insn "smulsi3_highpart"
  [(set (match_operand:SI 0 "mep_hi_operand" "=h")
	(truncate:SI
	 (lshiftrt:DI
	  (mult:DI (sign_extend:DI
		    (match_operand:SI 1 "register_operand" "r"))
		   (sign_extend:DI
		    (match_operand:SI 2 "register_operand" "r")))
	  (const_int 32))))
   (clobber (reg:SI LO_REGNO))]
  "TARGET_OPT_MULT"
  "mul\\t%1, %2"
  [(set_attr "length" "2")
   (set_attr "stall" "mul")])

(define_expand "umulsidi3"
  [(set (match_operand:DI 0 "mep_hi_operand" "")
	(mult:DI (zero_extend:DI (match_operand:SI 1 "register_operand" ""))
		 (zero_extend:DI (match_operand:SI 2 "register_operand" ""))))]
  "TARGET_OPT_MULT"
  "
{
  rtx hi = gen_reg_rtx (SImode);
  rtx lo = gen_reg_rtx (SImode);

  emit_insn (gen_umulsidi3_i (hi, lo, operands[1], operands[2]));
  emit_move_insn (gen_lowpart (SImode, operands[0]), lo);
  emit_move_insn (gen_highpart (SImode, operands[0]), hi);
  DONE;
}")

(define_insn "umulsidi3_i"
  [(set (match_operand:SI 0 "mep_hi_operand" "=h")
	(truncate:SI
	 (lshiftrt:DI
	  (mult:DI (zero_extend:DI
		    (match_operand:SI 2 "register_operand" "r"))
		   (zero_extend:DI
		    (match_operand:SI 3 "register_operand" "r")))
	  (const_int 32))))
   (set (match_operand:SI 1 "mep_lo_operand" "=l")
	(mult:SI (match_dup 2)
		 (match_dup 3)))]
  "TARGET_OPT_MULT"
  "mulu\\t%2, %3"
  [(set_attr "length" "2")
   (set_attr "stall" "mul")])

(define_insn "umulsi3_highpart"
  [(set (match_operand:SI 0 "mep_hi_operand" "=h")
	(truncate:SI
	 (lshiftrt:DI
	  (mult:DI (zero_extend:DI
		    (match_operand:SI 1 "register_operand" "r"))
		   (zero_extend:DI
		    (match_operand:SI 2 "register_operand" "r")))
	  (const_int 32))))
   (clobber (reg:SI LO_REGNO))]
  "TARGET_OPT_MULT"
  "mulu %1, %2"
  [(set_attr "length" "2")
   (set_attr "stall" "mul")])

;; These two don't currently match because we don't have an adddi3 pattern.
(define_insn "*smultdi_and_add"
  [(set (match_operand:DI 0 "mep_hi_operand" "=d")
	(plus:DI (mult:DI (zero_extend:DI
			   (match_operand:SI 1 "register_operand" "r"))
			  (zero_extend:DI
			   (match_operand:SI 2 "register_operand" "r")))
		 (match_operand:DI 3 "mep_hi_operand" "0")))]
  "TARGET_OPT_MULT && TARGET_BIG_ENDIAN"
  "maddu\\t%1, %2"
  [(set_attr "length" "4")
   (set_attr "stall" "mul")])

(define_insn "*umultdi_and_add"
  [(set (match_operand:DI 0 "mep_hi_operand" "=d")
	(plus:DI (mult:DI (sign_extend:DI
			   (match_operand:SI 1 "register_operand" "r"))
			  (sign_extend:DI
			   (match_operand:SI 2 "register_operand" "r")))
		 (match_operand:DI 3 "mep_hi_operand" "0")))]
  "TARGET_OPT_MULT && TARGET_BIG_ENDIAN"
  "madd\\t%1, %2"
  [(set_attr "length" "4")
   (set_attr "stall" "mul")])

;; A pattern for 'r1 = r2 * r3 + r4'.  There are three possible
;; implementations:
;;
;;    (1) 'mulr;add3'.  This is usually the best choice if the instruction
;;	  is not part of a natural multiply-accumulate chain.  It has the
;;	  same latency as 'stc;maddr' but doesn't tie up $lo for as long.
;;
;;    (2) 'madd'.  This is the best choice if the instruction is in the
;;	  middle of a natural multiply-accumulate chain.  r4 will already
;;	  be in $lo and r1 will also be needed in $lo.
;;
;;    (3) 'maddr'.  This is the best choice if the instruction is at the
;;	  end of a natural multiply-accumulate chain.  r4 will be in $lo
;;	  but r1 will be needed in a GPR.
;;
;; In theory, we could put all the alternatives into a single pattern and
;; leave the register allocator to choose between them.  However, this can
;; sometimes produce poor results in practice.
;;
;; This pattern therefore describes a general GPR-to-GPR operation that
;; has a slight preference for cases in which operands 0 and 1 are tied.
;; After reload, we try to rewrite the patterns using peephole2s (if
;; enabled), falling back on define_splits if that fails.  See also
;; mep_reuse_lo_p.
(define_insn "maddsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(plus:SI (mult:SI (match_operand:SI 1 "register_operand" "%0,r")
			  (match_operand:SI 2 "register_operand" "r,r"))
		 (match_operand:SI 3 "register_operand" "r,r")))
   (clobber (match_scratch:SI 4 "=l,l"))
   (clobber (match_scratch:SI 5 "=h,h"))]
  "TARGET_OPT_MULT"
  "#"
  [(set_attr "length" "8")
   (set_attr "stall" "mulr")])

;; Implement maddsi3s using maddr if operand 3 is already available in $lo.
(define_peephole2
  [(parallel
	[(set (match_operand:SI 0 "register_operand" "")
	      (plus:SI (mult:SI (match_operand:SI 1 "register_operand" "")
				(match_operand:SI 2 "register_operand" ""))
		       (match_operand:SI 3 "register_operand" "")))
	 (clobber (match_scratch:SI 4 ""))
	 (clobber (match_scratch:SI 5 ""))])]
  "TARGET_OPT_MULT
   && reload_completed
   && mep_reuse_lo_p (operands[4], operands[3], insn,
		      !rtx_equal_p (operands[1], operands[3])
		      && !rtx_equal_p (operands[2], operands[3])
		      && (rtx_equal_p (operands[0], operands[3])
			  || peep2_reg_dead_p (1, operands[3])))"
  [(parallel
	[(set (match_dup 4)
	      (plus:SI (mult:SI (match_dup 0)
			        (match_dup 2))
		       (match_dup 4)))
	 (set (match_dup 0)
	      (plus:SI (mult:SI (match_dup 0)
				(match_dup 2))
		       (match_dup 4)))
	 (clobber (match_dup 5))])]
  "operands[2] = mep_mulr_source (0, operands[0], operands[1], operands[2]);")

;; This splitter implements maddsi3 as "mulr;add3".  It only works if
;; operands 0 and 3 are distinct, since operand 0 is clobbered before
;; operand 3 is used.
(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(plus:SI (mult:SI (match_operand:SI 1 "register_operand" "")
			  (match_operand:SI 2 "register_operand" ""))
		 (match_operand:SI 3 "register_operand" "")))
   (clobber (match_scratch:SI 4 ""))
   (clobber (match_scratch:SI 5 ""))]
  "TARGET_OPT_MULT
   && reload_completed
   && !rtx_equal_p (operands[0], operands[3])"
  [(parallel [(set (match_dup 0)
		   (mult:SI (match_dup 0)
			    (match_dup 2)))
	      (clobber (match_dup 4))
	      (clobber (match_dup 5))])
   (set (match_dup 0)
	(plus:SI (match_dup 0)
		 (match_dup 3)))]
  "operands[2] = mep_mulr_source (0, operands[0], operands[1], operands[2]);")

;; This is the fallback splitter for maddsi3.  It moves operand 3 into
;; $lo and then uses maddr.
(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(plus:SI (mult:SI (match_operand:SI 1 "register_operand" "")
			  (match_operand:SI 2 "register_operand" ""))
		 (match_operand:SI 3 "register_operand" "")))
   (clobber (match_scratch:SI 4 ""))
   (clobber (match_scratch:SI 5 ""))]
  "TARGET_OPT_MULT
   && reload_completed"
  [(parallel [(set (match_dup 4)
		   (plus:SI (mult:SI (match_dup 0)
				     (match_dup 2))
			    (match_dup 4)))
	      (set (match_dup 0)
		   (plus:SI (mult:SI (match_dup 0)
				     (match_dup 2))
			    (match_dup 4)))
	      (clobber (match_dup 5))])]
{
  emit_move_insn (operands[4], operands[3]);
  operands[2] = mep_mulr_source (0, operands[0], operands[1], operands[2]);
})

;; Remove unnecessary stcs to $lo.  This cleans up the moves generated
;; by earlier calls to mep_reuse_lo_p.
(define_peephole2
  [(set (match_operand:SI 0 "mep_lo_operand" "")
	(match_operand:SI 1 "register_operand" ""))]
  "TARGET_OPT_MULT
   && mep_reuse_lo_p (operands[0], operands[1], insn,
		      peep2_reg_dead_p (1, operands[1]))"
  [(const_int 0)]
{
  emit_note (NOTE_INSN_DELETED);
  DONE;
})

(define_insn "maddsi3_lo"
  [(set (match_operand:SI 0 "mep_lo_operand" "=l")
	(plus:SI (mult:SI (match_operand:SI 1 "register_operand" "r")
			  (match_operand:SI 2 "register_operand" "r"))
		 (match_operand:SI 3 "mep_lo_operand" "0")))
   (clobber (match_scratch:SI 4 "=h"))]
  "TARGET_OPT_MULT && reload_completed"
  "madd\\t%1, %2"
  [(set_attr "length" "4")
   (set_attr "stall" "mul")])

(define_insn "maddsi3r"
  [(set (match_operand:SI 0 "mep_lo_operand" "=l")
	(plus:SI (mult:SI (match_operand:SI 2 "register_operand" "1")
			  (match_operand:SI 3 "register_operand" "r"))
		 (match_operand:SI 4 "register_operand" "0")))
   (set (match_operand:SI 1 "register_operand" "=r")
	(plus:SI (mult:SI (match_dup 2)
			  (match_dup 3))
		 (match_dup 4)))
   (clobber (match_scratch:SI 5 "=h"))]
  "TARGET_OPT_MULT && reload_completed"
  "maddr\\t%2, %3"
  [(set_attr "length" "4")
   (set_attr "stall" "mulr")])

(define_insn "*shift_1_or_2_and_add"
  [(set (match_operand:SI 0 "mep_r0_operand" "=z")
	(plus:SI (mult:SI (match_operand:SI 1 "register_operand" "r")
			  (match_operand:SI 2 "mep_slad_operand" "n"))
		 (match_operand:SI 3 "register_operand" "r")))]
  ""
  "sl%b2ad3\\t%0, %1, %3"
  [(set_attr "length" "2")
   (set_attr "stall" "int2")])

(define_insn "divmodsi4"
  [(set (match_operand:SI 0 "mep_lo_operand" "=l")
	(div:SI (match_operand:SI 1 "register_operand" "r")
		(match_operand:SI 2 "register_operand" "r")))
   (set (match_operand:SI 3 "mep_hi_operand" "=h")
	(mod:SI (match_dup 1)
		(match_dup 2)))]
  "TARGET_OPT_DIV"
  "div\\t%1, %2"
  [(set_attr "length" "2")
   (set_attr "stall" "div")
   (set_attr "may_trap" "yes")])

(define_insn "udivmodsi4"
  [(set (match_operand:SI 0 "mep_lo_operand" "=l")
	(udiv:SI (match_operand:SI 1 "register_operand" "r")
		 (match_operand:SI 2 "register_operand" "r")))
   (set (match_operand:SI 3 "mep_hi_operand" "=h")
	(umod:SI (match_dup 1)
		(match_dup 2)))]
  "TARGET_OPT_DIV"
  "divu\\t%1, %2"
  [(set_attr "length" "2")
   (set_attr "stall" "div")
   (set_attr "may_trap" "yes")])

(define_insn "negsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(neg:SI (match_operand:SI 1 "register_operand" "r")))]
  ""
  "neg\\t%0, %1"
  [(set_attr "length" "2")])

;; We have "absolute difference between two regs" which isn't quite
;; what gcc is expecting.
(define_expand "abssi2"
  [(set (match_dup 2) (const_int 0))
   (set (match_operand:SI 0 "register_operand" "")
	(abs:SI (minus:SI (match_operand:SI 1 "register_operand" "")
			  (match_dup 2))
		))]
  "TARGET_OPT_ABSDIFF"
  "operands[2] = gen_reg_rtx (SImode);")

(define_insn "*absdiff"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(abs:SI (minus:SI (match_operand:SI 1 "register_operand" "0")
			  (match_operand:SI 2 "register_operand" "r"))))]
  "TARGET_OPT_ABSDIFF"
  "abs\\t%0, %2"
  [(set_attr "length" "4")])

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(abs:SI (plus:SI (match_operand:SI 1 "register_operand" "")
			 (match_operand:SI 2 "immediate_operand" ""))))
   (clobber (match_operand:SI 3 "register_operand" ""))]
  "!reload_completed"
  [(set (match_dup 3)
	(match_dup 4))
   (set (match_operand:SI 0 "register_operand" "")
	(abs:SI (minus:SI (match_operand:SI 1 "register_operand" "")
			  (match_dup 3))))]
  "operands[4] = GEN_INT (-INTVAL (operands[2]));")

(define_insn "sminsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(smin:SI (match_operand:SI 1 "register_operand" "0")
		 (match_operand:SI 2 "nonmemory_operand" "r")))]
  "TARGET_OPT_MINMAX"
  "min\\t%0, %2"
  [(set_attr "length" "4")])

(define_insn "smaxsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(smax:SI (match_operand:SI 1 "register_operand" "0")
		 (match_operand:SI 2 "nonmemory_operand" "r")))]
  "TARGET_OPT_MINMAX"
  "max\\t%0, %2"
  [(set_attr "length" "4")])

(define_insn "uminsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(umin:SI (match_operand:SI 1 "register_operand" "0")
		 (match_operand:SI 2 "nonmemory_operand" "r")))]
  "TARGET_OPT_MINMAX"
  "minu\\t%0, %2"
  [(set_attr "length" "4")])

(define_insn "umaxsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(umax:SI (match_operand:SI 1 "register_operand" "0")
		 (match_operand:SI 2 "nonmemory_operand" "r")))]
  "TARGET_OPT_MINMAX"
  "maxu\\t%0, %2"
  [(set_attr "length" "4")])

;; Average:  a = (b+c+1)>>1
(define_insn "*averagesi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ashiftrt:SI (plus:SI (plus:SI
				(match_operand:SI 1 "register_operand" "0")
				(match_operand:SI 2 "register_operand" "r"))
			      (const_int 1))
		     (const_int 1)))]
  "TARGET_OPT_AVERAGE"
  "ave\\t%0, %2"
  [(set_attr "length" "4")])

;; clip support

(define_insn "clip_maxmin"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(smax:SI (smin:SI (match_operand:SI 1 "register_operand" "0")
			  (match_operand:SI 2 "immediate_operand" "n"))
		 (match_operand:SI 3 "immediate_operand" "n")))]
  "mep_allow_clip (operands[2], operands[3], 1)"
  "clip\\t%0, %B2"
  [(set_attr "length" "4")])

(define_insn "clip_minmax"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(smin:SI (smax:SI (match_operand:SI 1 "register_operand" "0")
			  (match_operand:SI 2 "immediate_operand" "n"))
		 (match_operand:SI 3 "immediate_operand" "n")))]
  "mep_allow_clip (operands[3], operands[2], 1)"
  "clip\\t%0, %B3"
  [(set_attr "length" "4")])

(define_insn "clipu_maxmin"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(smax:SI (smin:SI (match_operand:SI 1 "register_operand" "0")
			  (match_operand:SI 2 "immediate_operand" "n"))
		 (match_operand:SI 3 "immediate_operand" "n")))]
  "mep_allow_clip (operands[2], operands[3], 0)"
  "clipu\\t%0, %U2"
  [(set_attr "length" "4")])

(define_insn "clipu_minmax"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(smin:SI (smax:SI (match_operand:SI 1 "register_operand" "0")
			  (match_operand:SI 2 "immediate_operand" "n"))
		 (match_operand:SI 3 "immediate_operand" "n")))]
  "mep_allow_clip (operands[3], operands[2], 0)"
  "clipu\\t%0, %U3"
  [(set_attr "length" "4")])

;; ::::::::::::::::::::
;; ::
;; :: 32 bit Integer Shifts and Rotates
;; ::
;; ::::::::::::::::::::

(define_insn "ashlsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,z")
	(ashift:SI (match_operand:SI 1 "register_operand" "0,r")
		   (match_operand:SI 2 "nonmemory_operand" "rM,M")))]
  ""
  "@
   sll\\t%0, %2
   sll3\\t%0, %1, %2"
  [(set_attr "length" "2,2")
   (set_attr "shiftop" "operand2")])

(define_insn "ashrsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ashiftrt:SI (match_operand:SI 1 "register_operand" "0")
		     (match_operand:SI 2 "nonmemory_operand" "rM")))]
  ""
  "sra\\t%0, %2"
  [(set_attr "length" "2")
   (set_attr "shiftop" "operand2")])

(define_insn "lshrsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(lshiftrt:SI (match_operand:SI 1 "register_operand" "0")
		     (match_operand:SI 2 "nonmemory_operand" "rM")))]
  ""
  "srl\\t%0, %2"
  [(set_attr "length" "2")
   (set_attr "shiftop" "operand2")])

;; ::::::::::::::::::::
;; ::
;; :: 32 Bit Integer Logical operations
;; ::
;; ::::::::::::::::::::

(define_insn "andsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(and:SI (match_operand:SI 1 "register_operand" "%0,r")
		(match_operand:SI 2 "nonmemory_operand" "r,J")))]
  ""
  "@
   and\\t%0, %2
   and3\\t%0, %1, %J2"
  [(set_attr "length" "2,4")])

(define_insn "iorsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(ior:SI (match_operand:SI 1 "register_operand" "%0,r")
		(match_operand:SI 2 "nonmemory_operand" "r,J")))]
  ""
  "@
   or\\t%0, %2
   or3\\t%0, %1, %J2"
  [(set_attr "length" "2,4")])

(define_insn "xorsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(xor:SI (match_operand:SI 1 "register_operand" "%0,r")
		(match_operand:SI 2 "nonmemory_operand" "r,J")))]
  ""
  "@
   xor\\t%0, %2
   xor3\\t%0, %1, %J2"
  [(set_attr "length" "2,4")])

(define_expand "one_cmplsi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(not:SI (match_operand:SI 1 "register_operand" "")))]
  ""
  "operands[2] = operands[1];
   ")

;; No separate insn for this; use NOR
(define_insn "*one_cmplsi3_internal"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(not:SI (match_operand:SI 1 "register_operand" "0")))]
  ""
  "nor\\t%0, %0"
  [(set_attr "length" "2")])

;; ::::::::::::::::::::
;; ::
;; :: Bit Manipulation
;; ::
;; ::::::::::::::::::::

(define_insn "*bitop_be"
  [(set (match_operand:QI 0 "mep_Y_operand" "=Y")
	(subreg:QI (match_operator:SI 3 "mep_bit_operator"
			[(subreg:SI (match_operand:QI 1 "mep_Y_operand" "0") 0)
			 (match_operand 2 "immediate_operand" "n")])
		   3)
	)]
  "TARGET_BIG_ENDIAN && TARGET_OPT_BITOPS
   && rtx_equal_p (operands[0], operands[1])"
  "b%L3m\\t%0, %b2"
  [(set_attr "length" "2")])

(define_insn "*bitop_le"
  [(set (match_operand:QI 0 "mep_Y_operand" "=Y")
	(subreg:QI (match_operator:SI 3 "mep_bit_operator"
			[(subreg:SI (match_operand:QI 1 "mep_Y_operand" "0") 0)
			 (match_operand 2 "immediate_operand" "n")])
		   0)
	)]
  "!TARGET_BIG_ENDIAN && TARGET_OPT_BITOPS
   && rtx_equal_p (operands[0], operands[1])"
  "b%L3m\\t%0, %b2"
  [(set_attr "length" "2")])

(define_insn "btstm"
  [(set (match_operand:SI 0 "mep_r0_operand" "=z")
	(and:SI (subreg:SI (match_operand:QI 1 "mep_Y_operand" "Y") 0)
		(match_operand 2 "immediate_operand" "n"))
	)]
  "TARGET_OPT_BITOPS && mep_bit_position_p (operands[2], 1)"
  "btstm\\t%0, %1, %b2"
  [(set_attr "length" "2")])

(define_insn "tas"
  [(parallel [(set (match_operand:SI 0 "mep_r0_operand" "=z")
		   (zero_extend:SI (match_operand:QI 1 "mep_Y_operand" "+Y")))
	      (set (match_dup 1)
		   (const_int 1))
	      ]
	     )]
  "TARGET_OPT_BITOPS"
  "tas\\t%0, %1"
  [(set_attr "length" "2")])

(define_peephole2
  [(set (match_operand:SI 0 "mep_r0_operand" "")
	(zero_extend:SI (match_operand:QI 1 "mep_Y_operand" "")))
   (set (match_operand:QI 2 "register_operand" "")
	(const_int 1))
   (set (match_dup 1)
	(match_dup 2))
   ]
  "TARGET_OPT_BITOPS"
  [(parallel [(set (match_dup 0)
		   (zero_extend:SI (match_dup 1)))
	      (set (match_dup 1)
		   (const_int 1))
	      ])]
  "")

(define_peephole2
  [(set (match_operand:SI 0 "mep_r0_operand" "")
	(sign_extend:SI (match_operand:QI 1 "mep_Y_operand" "")))
   (set (match_operand:QI 2 "register_operand" "")
	(const_int 1))
   (set (match_dup 1)
	(match_dup 2))
   ]
  "TARGET_OPT_BITOPS"
  [(parallel [(set (match_dup 0)
		   (zero_extend:SI (match_dup 1)))
	      (set (match_dup 1)
		   (const_int 1))
	      ])
   (set (match_dup 0)
	(sign_extend:SI (match_dup 3)))]
  "operands[3] = gen_lowpart (QImode, operands[0]);")


;; ::::::::::::::::::::
;; ::
;; :: Conditional branches and stores
;; ::
;; ::::::::::::::::::::

(define_expand "cbranchsi4"
  [(set (pc)
	(if_then_else (match_operator 0 "ordered_comparison_operator"
				      [(match_operand:SI 1 "register_operand" "")
				       (match_operand:SI 2 "nonmemory_operand" "")])
		      (label_ref (match_operand 3 "" ""))
		      (pc)))]
  ""
  "emit_jump_insn (gen_branch_true (operands[3],
			       mep_expand_cbranch (operands)));
   DONE;")
  
(define_expand "branch_true"
  [(set (pc)
	(if_then_else (match_operand 1 "" "")
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "")
  
(define_expand "cstoresi4"
  [(set (match_operand:SI 0 "register_operand" "")
	(match_operator:SI 1 "ordered_comparison_operator"
			   [(match_operand:SI 2 "register_operand" "")
			    (match_operand:SI 3 "nonmemory_operand" "")]))]
  ""
  "if (mep_expand_setcc (operands)) DONE; else FAIL;")

;; ------------------------------------------------------------

(define_insn "*slt"
  [(set (match_operand:SI 0 "register_operand" "=z,z,r")
	(lt:SI (match_operand:SI 1 "register_operand" "r,r,r")
	    (match_operand:SI 2 "nonmemory_operand" "r,M,I")))]
  ""
  "slt3\\t%0, %1, %2"
  [(set_attr "length" "2,2,4")])

(define_insn "*sltu"
  [(set (match_operand:SI 0 "register_operand" "=z,z,r")
	(ltu:SI (match_operand:SI 1 "register_operand" "r,r,r")
	     (match_operand:SI 2 "nonmemory_operand" "r,M,J")))]
  ""
  "sltu3\\t%0, %1, %2"
  [(set_attr "length" "2,2,4")])

(define_insn "*bcpeq_true"
  [(set (pc)
	(if_then_else (eq:SI (reg:SI CBCR_REGNO)
			     (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "bcpeq\t0, %l0"
  [(set_attr "length" "4")])

(define_insn "*bcpeq_false"
  [(set (pc)
	(if_then_else (eq:SI (reg:SI CBCR_REGNO)
			     (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "bcpne\t0, %l0"
  [(set_attr "length" "4")])

(define_insn "*bcpne_true"
  [(set (pc)
	(if_then_else (ne:SI (reg:SI CBCR_REGNO)
			     (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "bcpne\t0, %l0"
  [(set_attr "length" "4")])

(define_insn "*bcpne_false"
  [(set (pc)
	(if_then_else (ne:SI (reg:SI CBCR_REGNO)
			     (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "bcpeq\t0, %l0"
  [(set_attr "length" "4")])

;; ??? The lengths here aren't correct, since no attempt it made to
;; find "beqz" in the 256-byte range.  However, this should not affect
;; bundling, since we never run core branches in parallel.

(define_insn "mep_beq_true"
  [(set (pc)
	(if_then_else (eq (match_operand:SI 0 "register_operand" "r")
			  (match_operand:SI 1 "mep_reg_or_imm4_operand" "rN"))
		      (label_ref (match_operand 2 "" ""))
		      (pc)))]
  ""
  "* return mep_emit_cbranch (operands, 0);"
  [(set_attr "length" "4")]  )

(define_insn "*beq_false"
  [(set (pc)
	(if_then_else (eq (match_operand:SI 0 "register_operand" "r")
			  (match_operand:SI 1 "mep_reg_or_imm4_operand" "rN"))
		      (pc)
		      (label_ref (match_operand 2 "" ""))))]
  ""
  "* return mep_emit_cbranch (operands, 1);"
  [(set_attr "length" "4")])

(define_insn "mep_bne_true"
  [(set (pc)
	(if_then_else (ne (match_operand:SI 0 "register_operand" "r")
			  (match_operand:SI 1 "mep_reg_or_imm4_operand" "rN"))
		      (label_ref (match_operand 2 "" ""))
		      (pc)))]
  ""
  "* return mep_emit_cbranch (operands, 1); "
  [(set_attr "length" "4")])

(define_insn "*bne_false"
  [(set (pc)
	(if_then_else (ne (match_operand:SI 0 "register_operand" "r")
			  (match_operand:SI 1 "mep_reg_or_imm4_operand" "rN"))
		      (pc)
		      (label_ref (match_operand 2 "" ""))))]
  ""
  "* return mep_emit_cbranch (operands, 0); "
  [(set_attr "length" "4")])

(define_insn "mep_blti"
  [(set (pc)
	(if_then_else (lt (match_operand:SI 0 "register_operand" "r")
			  (match_operand:SI 1 "mep_imm4_operand" "N"))
		      (label_ref (match_operand 2 "" ""))
		      (pc)))]
  ""
  "blti\\t%0, %1, %l2"
  [(set_attr "length" "4")])

(define_insn "*bgei"
  [(set (pc)
	(if_then_else (ge (match_operand:SI 0 "register_operand" "r")
			  (match_operand:SI 1 "mep_imm4_operand" "N"))
		      (label_ref (match_operand 2 "" ""))
		      (pc)))]
  ""
  "bgei\\t%0, %1, %l2"
  [(set_attr "length" "4")])

;; ::::::::::::::::::::
;; ::
;; :: Call and branch instructions
;; ::
;; ::::::::::::::::::::

(define_expand "call"
  [(parallel [(call (match_operand:QI 0 "" "")
		    (match_operand:SI 1 "" ""))
	      (use (match_operand:SI 2 "" ""))
	      (clobber (reg:SI REGSAVE_CONTROL_TEMP))
	      ])]
  ""
  "
{
  mep_expand_call (operands, 0);
  DONE;
}")

(define_insn "call_internal"
  [(call (mem (match_operand:SI 0 "mep_call_address_operand" "R,r"))
	 (match_operand:SI 1 "" ""))
   (use (match_operand:SI 2 "const_int_operand" ""))
   (use (match_operand:SI 3 "mep_tp_operand" "b,b"))
   (use (match_operand:SI 4 "mep_gp_operand" "v,v"))
   (clobber (reg:SI LP_REGNO))
   (clobber (reg:SI REGSAVE_CONTROL_TEMP))
  ]
  ""
{
  static char const pattern[2][2][8] = 
  {
    { "bsrv\t%0", "jsrv\t%0" },
    { "bsr\t%0", "jsr\t%0" }
  };

  return pattern[mep_vliw_mode_match (operands[2])][which_alternative];
}
  [(set_attr "length" "4,2")])

(define_expand "sibcall"
  [(parallel [(call (match_operand:QI 0 "" "")
		    (match_operand:SI 1 "" ""))
	      (use (match_operand:SI 2 "" ""))
	      (use (reg:SI LP_REGNO))
	      (clobber (reg:SI REGSAVE_CONTROL_TEMP))
	      ])]
  ""
  "")

(define_insn "*sibcall_internal"
  [(call (mem (match_operand:SI 0 "mep_nearsym_operand" "s"))
	 (match_operand:SI 1 "" ""))
   (use (match_operand:SI 2 "const_int_operand" ""))
   (use (reg:SI LP_REGNO))
   (clobber (reg:SI REGSAVE_CONTROL_TEMP))
  ]
  "SIBLING_CALL_P (insn)"
{
  if (mep_vliw_jmp_match (operands[2]))
    return "jmp\t%0";
  else if (mep_vliw_mode_match (operands[2]))
    return
        "movu	$0, %0\n\
	jmp	$0";
  else
    return
	"ldc	$12, $lp\n\
	movh	$11, %%hi(%0)\n\
	xor3	$12, $12, 1\n\
	add3	$11, $11, %%lo(%0+1)\n\
	stc	$12, $lp\n\
	jmp	$11";
}
  [(set_attr "length" "48")
   (set_attr "slot" "multi")])

(define_expand "call_value"
  [(parallel [(set (match_operand 0 "" "")
		   (call (match_operand:QI 1 "" "")
		         (match_operand:SI 2 "" "")))
	      (use (match_operand:SI 3 "" ""))
	      (clobber (reg:SI REGSAVE_CONTROL_TEMP))
	      ])]
  ""
  "
{
  mep_expand_call (operands, 1);
  DONE;
}")

(define_insn "call_value_internal"
  [(set (match_operand 0 "register_operand" "=rx,rx")
	(call (mem:SI (match_operand:SI 1 "mep_call_address_operand" "R,r"))
	      (match_operand:SI 2 "" "")))
   (use (match_operand:SI 3 "const_int_operand" ""))
   (use (match_operand:SI 4 "mep_tp_operand" "b,b"))
   (use (match_operand:SI 5 "mep_gp_operand" "v,v"))
   (clobber (reg:SI LP_REGNO))
   (clobber (reg:SI REGSAVE_CONTROL_TEMP))
  ]
  ""
{
  static char const pattern[2][2][8] = 
  {
    { "bsrv\t%1", "jsrv\t%1" },
    { "bsr\t%1", "jsr\t%1" }
  };

  return pattern[mep_vliw_mode_match (operands[3])][which_alternative];
}
  [(set_attr "length" "4,2")])

(define_expand "sibcall_value"
  [(parallel [(set (match_operand 0 "" "")
		   (call (match_operand:QI 1 "" "")
		         (match_operand:SI 2 "" "")))
	      (use (match_operand:SI 3 "" ""))
	      (use (reg:SI LP_REGNO))
	      (clobber (reg:SI REGSAVE_CONTROL_TEMP))
	      ])]
  ""
  "")

(define_insn "*sibcall_value_internal"
  [(set (match_operand 0 "register_operand" "=rx")
	(call (mem (match_operand:SI 1 "mep_nearsym_operand" "s"))
	      (match_operand:SI 2 "" "")))
   (use (match_operand:SI 3 "const_int_operand" ""))
   (use (reg:SI LP_REGNO))
   (clobber (reg:SI REGSAVE_CONTROL_TEMP))
  ]
  "SIBLING_CALL_P (insn)"
{
  if (mep_vliw_jmp_match (operands[3]))
    return "jmp\t%1";
  else if (mep_vliw_mode_match (operands[3]))
    return
        "movu	$0, %1\n\
	jmp	$0";
  else
    return
	"ldc	$12, $lp\n\
	movh	$11, %%hi(%1)\n\
	xor3	$12, $12, 1\n\
	add3	$11, $11, %%lo(%1+1)\n\
	stc	$12, $lp\n\
	jmp	$11";
}
  [(set_attr "length" "48")
   (set_attr "slot" "multi")])

(define_insn "return_internal"
  [(return)
   (use (match_operand:SI 0 "register_operand" ""))]
  ""
  "* return (REGNO (operands[0]) == LP_REGNO) ? \"ret\" : \"jmp\\t%0\";"
  [(set_attr "length" "2")
   (set_attr "stall" "ret")])

(define_insn "eh_return_internal"
  [(return)
   (use (reg:SI 10))
   (use (reg:SI 11))
   (use (reg:SI LP_REGNO))
   (clobber (reg:SI REGSAVE_CONTROL_TEMP))
  ]
  ""
  "ret"
  [(set_attr "length" "2")
   (set_attr "stall" "ret")])

;; The assembler replaces short jumps with long jumps as needed.
(define_insn "jump"
  [(set (pc) (label_ref (match_operand 0 "" "")))]
  ""
  "bra\\t%l0"
  [(set_attr "length" "4")])

(define_insn "indirect_jump"
  [(set (pc) (match_operand:SI 0 "register_operand" "r"))]
  ""
  "jmp\\t%0"
  [(set_attr "length" "2")])

(define_insn "tablejump"
  [(set (pc) (match_operand:SI 0 "register_operand" "r"))
   (use (label_ref (match_operand 1 "" "")))]
  ""
  "jmp\\t%0"
  [(set_attr "length" "2")])


;; ::::::::::::::::::::
;; ::
;; :: Low Overhead Looping
;; ::
;; ::::::::::::::::::::

;; This insn is volatile because we'd like it to stay in its original
;; position, just before the loop header.  If it stays there, we might
;; be able to convert it into a "repeat" insn.
(define_insn "doloop_begin_internal"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec_volatile:SI
	 [(match_operand:SI 1 "register_operand" "0")
	  (match_operand 2 "const_int_operand" "")] UNS_REPEAT_BEG))]
  ""
  { gcc_unreachable (); }
  [(set_attr "length" "4")])

(define_expand "doloop_begin"
  [(use (match_operand 0 "register_operand" ""))
   (use (match_operand:QI 1 "const_int_operand" ""))
   (use (match_operand:QI 2 "const_int_operand" ""))
   (use (match_operand:QI 3 "const_int_operand" ""))]
  "!profile_arc_flag && TARGET_OPT_REPEAT"
  "if (INTVAL (operands[3]) > 1)
     FAIL;
   mep_emit_doloop (operands, 0);
   DONE;
  ")

(define_insn "doloop_end_internal"
  [(set (pc)
	(if_then_else (ne (match_operand:SI 0 "nonimmediate_operand" "+r,cxy,*m")
			  (const_int 0))
		      (label_ref (match_operand 1 "" ""))
		      (pc)))
   (set (match_dup 0)
	(plus:SI (match_dup 0)
		 (const_int -1)))
   (unspec [(match_operand 2 "const_int_operand" "")] UNS_REPEAT_END)
   (clobber (match_scratch:SI 3 "=X,&r,&r"))]
  ""
  { gcc_unreachable (); }
  ;; Worst case length:
  ;;
  ;;      lw <op3>,<op0>	4
  ;;      add <op3>,-1		2
  ;;      sw <op3>,<op0>	4
  ;;      jmp <op1>		4
  ;; 1f:
  [(set_attr "length" "14")
   (set_attr "slot" "multi")])

(define_expand "doloop_end"
  [(use (match_operand 0 "nonimmediate_operand" ""))
   (use (match_operand:QI 1 "const_int_operand" ""))
   (use (match_operand:QI 2 "const_int_operand" ""))
   (use (match_operand:QI 3 "const_int_operand" ""))
   (use (label_ref (match_operand 4 "" "")))]
  "!profile_arc_flag && TARGET_OPT_REPEAT"
  "if (INTVAL (operands[3]) > 1)
     FAIL;
   if (GET_CODE (operands[0]) == REG && GET_MODE (operands[0]) != SImode)
     FAIL;
   mep_emit_doloop (operands, 1);
   DONE;
  ")

(define_insn "repeat"
  [(set (reg:SI RPC_REGNO)
	(unspec:SI [(match_operand:SI 0 "mep_r0_15_operand" "r")
		    (match_operand:SI 1 "" "")]
		   UNS_REPEAT_BEG))]
  ""
  "repeat\\t%0,%l1"
  [(set_attr "length" "4")])

(define_insn "repeat_end"
  [(unspec [(const_int 0)] UNS_REPEAT_END)]
  ""
  "# repeat end"
  [(set_attr "length" "0")])

(define_insn "erepeat"
  [(unspec [(match_operand 0 "" "")] UNS_EREPEAT_BEG)]
  ""
  "erepeat\\t%l0"
  [(set_attr "length" "4")])

(define_insn "erepeat_end"
  [(unspec [(const_int 0)] UNS_EREPEAT_END)]
  ""
  "# erepeat end"
  [(set_attr "length" "0")
   (set_attr "slot" "multi")])


;; ::::::::::::::::::::
;; ::
;; :: Prologue and Epilogue instructions
;; ::
;; ::::::::::::::::::::

(define_expand "prologue"
  [(const_int 1)]
  ""
  "
{
  mep_expand_prologue ();
  DONE;
}")

(define_expand "epilogue"
  [(return)]
  ""
  "
{
  mep_expand_epilogue ();
  DONE;
}")

(define_expand "eh_return"
  [(use (match_operand:SI 0 "register_operand" "r"))]
  ""
  "
{
  mep_expand_eh_return (operands);
  DONE;
}")

(define_insn_and_split "eh_epilogue"
  [(unspec [(match_operand:SI 0 "register_operand" "r")] UNS_EH_EPILOGUE)
   (use (reg:SI LP_REGNO))]
  ""
  "#"
  "epilogue_completed"
  [(const_int 1)]
  "mep_emit_eh_epilogue (operands); DONE;"
  [(set_attr "slot" "multi")])

(define_expand "sibcall_epilogue"
  [(const_int 0)]
  ""
  "
{
  mep_expand_sibcall_epilogue ();
  DONE;
}")

(define_insn "mep_bb_trace_ret"
  [(unspec_volatile [(const_int 0)] UNS_BB_TRACE_RET)]
  ""
  "* return mep_emit_bb_trace_ret ();"
  [(set_attr "slot" "multi")])

(define_insn "mep_disable_int"
  [(unspec_volatile [(const_int 0)] UNS_DISABLE_INT)]
  ""
  "di"
  [(set_attr "length" "2")])

(define_insn "mep_enable_int"
  [(unspec_volatile [(const_int 0)] UNS_ENABLE_INT)]
  ""
  "ei"
  [(set_attr "length" "2")])

(define_insn "mep_reti"
  [(return)
   (unspec_volatile [(const_int 0)] UNS_RETI)]
  ""
  "reti"
  [(set_attr "length" "2")])

;; ::::::::::::::::::::
;; ::
;; :: Miscellaneous instructions
;; ::
;; ::::::::::::::::::::

(define_insn "nop"
  [(const_int 0)]
  ""
  "nop"
  [(set_attr "length" "2")])

(define_insn "nop32"
  [(const_int 1)]
  ""
  "or3\\t$0, $0, 0"
  [(set_attr "length" "4")])

(define_insn "blockage"
  [(unspec_volatile [(const_int 0)] UNS_BLOCKAGE)]
  ""
  ""
  [(set_attr "length" "0")
   (set_attr "slot" "multi")])


(define_insn "djmark"
  [(unspec_volatile [(const_int 0)] 999)]
  ""
  "# dj"
  [(set_attr "length" "0")
   (set_attr "slot" "multi")])

