;; Copyright (C) 2006 Free Software Foundation, Inc.

;; This file is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2 of the License, or (at your option) 
;; any later version.

;; This file is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file; see the file COPYING.  If not, write to the Free
;; Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;- See file "rtl.def" for documentation on define_insn, match_*, et. al.


;; Define an insn type attribute.  This is used in function unit delay
;; computations.
;; multi0 is a multiple insn rtl whose first insn is in pipe0
;; multi1 is a multiple insn rtl whose first insn is in pipe1
(define_attr "type" "fx2,shuf,fx3,load,store,br,spr,lnop,nop,fxb,fp6,fp7,fpd,iprefetch,multi0,multi1,hbr,convert"
  (const_string "fx2"))

;; Length (in bytes).
(define_attr "length" ""
		(const_int 4))

;; Processor type -- this attribute must exactly match the processor_type
;; enumeration in spu.h.

(define_attr "cpu" "spu"
  (const (symbol_ref "spu_cpu_attr")))

; (define_function_unit NAME MULTIPLICITY SIMULTANEITY
;			TEST READY-DELAY ISSUE-DELAY [CONFLICT-LIST])

(define_cpu_unit "pipe0,pipe1,fp,ls")

(define_insn_reservation "NOP" 1 (eq_attr "type" "nop")
    "pipe0")

(define_insn_reservation "FX2" 2 (eq_attr "type" "fx2")
    "pipe0, nothing")

(define_insn_reservation "FX3" 4 (eq_attr "type" "fx3,fxb")
    "pipe0, nothing*3")

(define_insn_reservation "FP6" 6 (eq_attr "type" "fp6")
    "pipe0 + fp, nothing*5")

(define_insn_reservation "FP7" 7 (eq_attr "type" "fp7")
    "pipe0, fp, nothing*5")

;; The behaviour of the double precision is that both pipes stall
;; for 6 cycles and the the rest of the operation pipelines for
;; 7 cycles.  The simplest way to model this is to simply ignore
;; the 6 cyle stall.
(define_insn_reservation "FPD" 7 (eq_attr "type" "fpd")
    "pipe0 + pipe1, fp, nothing*5")

(define_insn_reservation "LNOP" 1 (eq_attr "type" "lnop")
    "pipe1")

(define_insn_reservation "STORE" 1 (eq_attr "type" "store")
    "pipe1 + ls")

(define_insn_reservation "IPREFETCH" 1 (eq_attr "type" "iprefetch")
    "pipe1 + ls")

(define_insn_reservation "SHUF" 4 (eq_attr "type" "shuf,br,spr")
    "pipe1, nothing*3")

(define_insn_reservation "LOAD" 6 (eq_attr "type" "load")
    "pipe1 + ls, nothing*5")

(define_insn_reservation "HBR" 18 (eq_attr "type" "hbr")
    "pipe1, nothing*15")

(define_insn_reservation "MULTI0" 4 (eq_attr "type" "multi0")
    "pipe0+pipe1, nothing*3")

(define_insn_reservation "MULTI1" 4 (eq_attr "type" "multi1")
    "pipe1, nothing*3")

(define_insn_reservation "CONVERT" 0 (eq_attr "type" "convert")
    "nothing")

;; Force pipe0 to occur before pipe 1 in a cycle.
(absence_set "pipe0" "pipe1")


(define_constants [
 (UNSPEC_BLOCKAGE	0)
 (UNSPEC_IPREFETCH	1)
 (UNSPEC_FREST		2)
 (UNSPEC_FRSQEST	3)
 (UNSPEC_FI		4)
 (UNSPEC_EXTEND_CMP	5)
 (UNSPEC_CG		6)
 (UNSPEC_CGX		7)
 (UNSPEC_ADDX		8)
 (UNSPEC_BG		9)
 (UNSPEC_BGX		10)
 (UNSPEC_SFX		11)
 (UNSPEC_FSM		12)
 (UNSPEC_HBR		13)
 (UNSPEC_LNOP		14)
 (UNSPEC_NOP		15)
 (UNSPEC_CONVERT	16)
 (UNSPEC_SELB		17)
 (UNSPEC_SHUFB		18)
 (UNSPEC_CPAT		19)
 (UNSPEC_SYNC		20)
 (UNSPEC_CNTB		21)
 (UNSPEC_SUMB		22)
 (UNSPEC_FSMB           23)
 (UNSPEC_FSMH           24)
 (UNSPEC_GBB            25)
 (UNSPEC_GBH            26)
 (UNSPEC_GB             27)
 (UNSPEC_AVGB           28)
 (UNSPEC_ABSDB          29)
 (UNSPEC_ORX            30)
 (UNSPEC_HEQ            31)
 (UNSPEC_HGT            32)
 (UNSPEC_HLGT           33)
 (UNSPEC_CSFLT          34)
 (UNSPEC_CFLTS          35)
 (UNSPEC_CUFLT          36)
 (UNSPEC_CFLTU          37)
 (UNSPEC_STOP           38)
 (UNSPEC_STOPD          39)
 (UNSPEC_IDISABLE       40)
 (UNSPEC_IENABLE        41)
 (UNSPEC_FSCRRD         42)
 (UNSPEC_FSCRWR         43)
 (UNSPEC_MFSPR          44)
 (UNSPEC_MTSPR          45)
 (UNSPEC_RDCH           46)
 (UNSPEC_RCHCNT         47)
 (UNSPEC_WRCH           48)
 (UNSPEC_SPU_REALIGN_LOAD 49)
 (UNSPEC_SPU_MASK_FOR_LOAD 50)
])

(include "predicates.md")
(include "constraints.md")


;; Mode macros

(define_mode_macro ALL [QI V16QI
			HI V8HI
			SI V4SI
			DI V2DI
			TI
                        SF V4SF
                        DF V2DF])

; Everything except DI and TI which are handled separately because
; they need different constraints to correctly test VOIDmode constants
(define_mode_macro MOV [QI V16QI
			HI V8HI
			SI V4SI
			V2DI
                        SF V4SF
                        DF V2DF])

(define_mode_macro DTI  [DI TI])

(define_mode_macro VINT [QI V16QI
			 HI V8HI
			 SI V4SI
			 DI V2DI
			 TI])

(define_mode_macro VQHSI [QI V16QI
			  HI V8HI
			  SI V4SI])

(define_mode_macro VHSI [HI V8HI
			 SI V4SI])

(define_mode_macro VSDF [SF V4SF
                         DF V2DF])

(define_mode_macro VSI [SI V4SI])
(define_mode_macro VDI [DI V2DI])
(define_mode_macro VSF [SF V4SF])
(define_mode_macro VDF [DF V2DF])

(define_mode_attr bh  [(QI "b")  (V16QI "b")
		       (HI "h")  (V8HI "h")
		       (SI "")   (V4SI "")])

(define_mode_attr d   [(SF "")   (V4SF "")
                       (DF "d")  (V2DF "d")])
(define_mode_attr d6  [(SF "6")  (V4SF "6")
                       (DF "d")  (V2DF "d")])
(define_mode_attr f2i [(SF "SI") (V4SF "V4SI")
                       (DF "DI") (V2DF "V2DI")])

;; Used for carry and borrow instructions.
(define_mode_macro CBOP  [SI DI V4SI V2DI])

;; Used in vec_set and vec_extract
(define_mode_macro V [V2DI V4SI V8HI V16QI V2DF V4SF])
(define_mode_attr inner  [(V16QI "QI")
			  (V8HI  "HI")
			  (V4SI  "SI")
			  (V2DI  "DI")
			  (V4SF  "SF")
			  (V2DF  "DF")])
(define_mode_attr vmult  [(V16QI "1")
			  (V8HI  "2")
			  (V4SI  "4")
			  (V2DI  "8")
			  (V4SF  "4")
			  (V2DF  "8")])
(define_mode_attr voff   [(V16QI "13")
			  (V8HI  "14")
			  (V4SI  "0")
			  (V2DI  "0")
			  (V4SF  "0")
			  (V2DF  "0")])


;; mov

(define_expand "mov<mode>"
  [(set (match_operand:ALL 0 "spu_nonimm_operand" "=r,r,r,m")
	(match_operand:ALL 1 "general_operand" "r,i,m,r"))]
  ""
  {
    if (spu_expand_mov(operands, <MODE>mode))
      DONE;
  })

(define_split 
  [(set (match_operand 0 "spu_reg_operand")
	(match_operand 1 "immediate_operand"))]

  ""
  [(set (match_dup 0)
	(high (match_dup 1)))
   (set (match_dup 0)
	(lo_sum (match_dup 0)
	        (match_dup 1)))]
  {
    if (spu_split_immediate (operands))
      DONE;
    FAIL;
  })

(define_insn "pic"
  [(set (match_operand:SI 0 "spu_reg_operand" "=r")
	(match_operand:SI 1 "immediate_operand" "s"))
   (use (const_int 0))]
  "flag_pic"
  "ila\t%0,%%pic(%1)")

;; Whenever a function generates the 'pic' pattern above we need to
;; load the pic_offset_table register.
;; GCC doesn't deal well with labels in the middle of a block so we
;; hardcode the offsets in the asm here.
(define_insn "load_pic_offset"
  [(set (match_operand:SI 0 "spu_reg_operand" "=r")
	(unspec:SI [(const_int 0)] 0))
   (set (match_operand:SI 1 "spu_reg_operand" "=r")
	(unspec:SI [(const_int 0)] 0))]
  "flag_pic"
  "ila\t%1,.+8\;brsl\t%0,4"
  [(set_attr "length" "8")
   (set_attr "type" "multi0")])


;; move internal

(define_insn "_mov<mode>"
  [(set (match_operand:MOV 0 "spu_nonimm_operand" "=r,r,r,r,r,m")
	(match_operand:MOV 1 "spu_mov_operand" "r,A,f,j,m,r"))]
  "spu_valid_move (operands)"
  "@
   ori\t%0,%1,0
   il%s1\t%0,%S1
   fsmbi\t%0,%S1
   c%s1d\t%0,%S1($sp)
   lq%p1\t%0,%1
   stq%p0\t%1,%0"
  [(set_attr "type" "fx2,fx2,shuf,shuf,load,store")])

(define_insn "high"
  [(set (match_operand:SI 0 "spu_reg_operand" "=r")
	(high:SI (match_operand:SI 1 "immediate_operand" "i")))]
  ""
  "ilhu\t%0,%1@h")

(define_insn "low"
  [(set (match_operand:SI 0 "spu_reg_operand" "=r")
	(lo_sum:SI (match_operand:SI 1 "spu_reg_operand" "0")
		   (match_operand:SI 2 "immediate_operand" "i")))]
  ""
  "iohl\t%0,%2@l")

(define_insn "_movdi"
  [(set (match_operand:DI 0 "spu_nonimm_operand" "=r,r,r,r,r,m")
	(match_operand:DI 1 "spu_mov_operand" "r,a,f,k,m,r"))]
  "spu_valid_move (operands)"
  "@
   ori\t%0,%1,0
   il%d1\t%0,%D1
   fsmbi\t%0,%D1
   c%d1d\t%0,%D1($sp)
   lq%p1\t%0,%1
   stq%p0\t%1,%0"
  [(set_attr "type" "fx2,fx2,shuf,shuf,load,store")])

(define_insn "_movti"
  [(set (match_operand:TI 0 "spu_nonimm_operand" "=r,r,r,r,r,m")
	(match_operand:TI 1 "spu_mov_operand" "r,U,f,l,m,r"))]
  "spu_valid_move (operands)"
  "@
   ori\t%0,%1,0
   il%t1\t%0,%T1
   fsmbi\t%0,%T1
   c%t1d\t%0,%T1($sp)
   lq%p1\t%0,%1
   stq%p0\t%1,%0"
  [(set_attr "type" "fx2,fx2,shuf,shuf,load,store")])

(define_insn_and_split "load"
  [(set (match_operand 0 "spu_reg_operand" "=r")
	(match_operand 1 "memory_operand" "m"))
   (clobber (match_operand:TI 2 "spu_reg_operand" "=&r"))
   (clobber (match_operand:SI 3 "spu_reg_operand" "=&r"))]
  "GET_MODE(operands[0]) == GET_MODE(operands[1])"
  "#"
  ""
  [(set (match_dup 0)
	(match_dup 1))]
  { spu_split_load(operands); DONE; })

(define_insn_and_split "store"
  [(set (match_operand 0 "memory_operand" "=m")
	(match_operand 1 "spu_reg_operand" "r"))
   (clobber (match_operand:TI 2 "spu_reg_operand" "=&r"))
   (clobber (match_operand:TI 3 "spu_reg_operand" "=&r"))]
  "GET_MODE(operands[0]) == GET_MODE(operands[1])"
  "#"
  ""
  [(set (match_dup 0)
	(match_dup 1))]
  { spu_split_store(operands); DONE; })

;; Operand 3 is the number of bytes. 1:b 2:h 4:w 8:d

(define_expand "cpat"
  [(set (match_operand:TI 0 "spu_reg_operand" "=r,r")
	(unspec:TI [(match_operand:SI 1 "spu_reg_operand" "r,r")
		    (match_operand:SI 2 "spu_nonmem_operand" "r,n")
		    (match_operand:SI 3 "immediate_operand" "i,i")] UNSPEC_CPAT))]
  ""
  {
    rtx x = gen_cpat_const (operands);
    if (x)
      {
        emit_move_insn (operands[0], x);
        DONE;
      }
  })

(define_insn "_cpat"
  [(set (match_operand:TI 0 "spu_reg_operand" "=r,r")
	(unspec:TI [(match_operand:SI 1 "spu_reg_operand" "r,r")
		    (match_operand:SI 2 "spu_nonmem_operand" "r,n")
		    (match_operand:SI 3 "immediate_operand" "i,i")] UNSPEC_CPAT))]
  ""
  "@
   c%M3x\t%0,%1,%2
   c%M3d\t%0,%C2(%1)"
  [(set_attr "type" "shuf")])

(define_split
  [(set (match_operand:TI 0 "spu_reg_operand")
	(unspec:TI [(match_operand:SI 1 "spu_nonmem_operand")
		    (match_operand:SI 2 "immediate_operand")
		    (match_operand:SI 3 "immediate_operand")] UNSPEC_CPAT))]
  ""
  [(set (match_dup:TI 0)
        (match_dup:TI 4))]
  {
    operands[4] = gen_cpat_const (operands);
    if (!operands[4])
      FAIL;
  })

;; extend

(define_insn "extendqihi2"
  [(set (match_operand:HI 0 "spu_reg_operand" "=r")
	(sign_extend:HI (match_operand:QI 1 "spu_reg_operand" "r")))]
  ""
  "xsbh\t%0,%1")

(define_insn "extendhisi2"
  [(set (match_operand:SI 0 "spu_reg_operand" "=r")
	(sign_extend:SI (match_operand:HI 1 "spu_reg_operand" "r")))]
  ""
  "xshw\t%0,%1")

(define_expand "extendsidi2"
  [(set (match_dup:DI 2)
	(zero_extend:DI (match_operand:SI 1 "spu_reg_operand" "")))
   (set (match_operand:DI 0 "spu_reg_operand" "")
	(sign_extend:DI (vec_select:SI (match_dup:V2SI 3)
				       (parallel [(const_int 1)]))))]
  ""
  {
    operands[2] = gen_reg_rtx (DImode);
    operands[3] = spu_gen_subreg (V2SImode, operands[2]);
  })

(define_insn "xswd"
  [(set (match_operand:DI 0 "spu_reg_operand" "=r")
	(sign_extend:DI
	  (vec_select:SI
	    (match_operand:V2SI 1 "spu_reg_operand" "r")
	    (parallel [(const_int 1) ]))))]
  ""
  "xswd\t%0,%1");

(define_expand "extendqiti2"
  [(set (match_operand:TI 0 "register_operand" "")
	(sign_extend:TI (match_operand:QI 1 "register_operand" "")))]
  ""
  "spu_expand_sign_extend(operands);
   DONE;")

(define_expand "extendhiti2"
  [(set (match_operand:TI 0 "register_operand" "")
	(sign_extend:TI (match_operand:HI 1 "register_operand" "")))]
  ""
  "spu_expand_sign_extend(operands);
   DONE;")

(define_expand "extendsiti2"
  [(set (match_operand:TI 0 "register_operand" "")
	(sign_extend:TI (match_operand:SI 1 "register_operand" "")))]
  ""
  "spu_expand_sign_extend(operands);
   DONE;")

(define_expand "extendditi2"
  [(set (match_operand:TI 0 "register_operand" "")
	(sign_extend:TI (match_operand:DI 1 "register_operand" "")))]
  ""
  "spu_expand_sign_extend(operands);
   DONE;")


;; zero_extend

(define_insn "zero_extendqihi2"
  [(set (match_operand:HI 0 "spu_reg_operand" "=r")
	(zero_extend:HI (match_operand:QI 1 "spu_reg_operand" "r")))]
  ""
  "andi\t%0,%1,0x00ff")

(define_insn "zero_extendqisi2"
  [(set (match_operand:SI 0 "spu_reg_operand" "=r")
	(zero_extend:SI (match_operand:QI 1 "spu_reg_operand" "r")))]
  ""
  "andi\t%0,%1,0x00ff")

(define_expand "zero_extendhisi2"
  [(set (match_operand:SI 0 "spu_reg_operand" "=r")
	(zero_extend:SI (match_operand:HI 1 "spu_reg_operand" "r")))
   (clobber (match_scratch:SI 2 "=&r"))]
  ""
  {
    rtx mask = gen_reg_rtx (SImode);
    rtx op1 = simplify_gen_subreg (SImode, operands[1], HImode, 0);
    emit_move_insn (mask, GEN_INT (0xffff));
    emit_insn (gen_andsi3(operands[0], op1, mask));
    DONE;
  })
  
(define_insn "zero_extendsidi2"
  [(set (match_operand:DI 0 "spu_reg_operand" "=r")
	(zero_extend:DI (match_operand:SI 1 "spu_reg_operand" "r")))]
  ""
  "rotqmbyi\t%0,%1,-4"
  [(set_attr "type" "shuf")])

(define_insn "zero_extendsiti2"
  [(set (match_operand:TI 0 "spu_reg_operand" "=r")
	(zero_extend:TI (match_operand:SI 1 "spu_reg_operand" "r")))]
  ""
  "rotqmbyi\t%0,%1,-12"
  [(set_attr "type" "shuf")])

(define_insn "zero_extendditi2"
  [(set (match_operand:TI 0 "spu_reg_operand" "=r")
	(zero_extend:TI (match_operand:DI 1 "spu_reg_operand" "r")))]
  ""
  "rotqmbyi\t%0,%1,-8"
  [(set_attr "type" "shuf")])


;; trunc

(define_insn "truncdiqi2"
  [(set (match_operand:QI 0 "spu_reg_operand" "=r")
	(truncate:QI (match_operand:DI 1 "spu_reg_operand" "r")))]
  ""
  "shlqbyi\t%0,%1,4"
  [(set_attr "type" "shuf")])

(define_insn "truncdihi2"
  [(set (match_operand:HI 0 "spu_reg_operand" "=r")
	(truncate:HI (match_operand:DI 1 "spu_reg_operand" "r")))]
  ""
  "shlqbyi\t%0,%1,4"
  [(set_attr "type" "shuf")])

(define_insn "truncdisi2"
  [(set (match_operand:SI 0 "spu_reg_operand" "=r")
	(truncate:SI (match_operand:DI 1 "spu_reg_operand" "r")))]
  ""
  "shlqbyi\t%0,%1,4"
  [(set_attr "type" "shuf")])

(define_insn "trunctiqi2"
  [(set (match_operand:QI 0 "spu_reg_operand" "=r")
	(truncate:QI (match_operand:TI 1 "spu_reg_operand" "r")))]
  ""
  "shlqbyi\t%0,%1,12"
  [(set_attr "type" "shuf")])

(define_insn "trunctihi2"
  [(set (match_operand:HI 0 "spu_reg_operand" "=r")
	(truncate:HI (match_operand:TI 1 "spu_reg_operand" "r")))]
  ""
  "shlqbyi\t%0,%1,12"
  [(set_attr "type" "shuf")])

(define_insn "trunctisi2"
  [(set (match_operand:SI 0 "spu_reg_operand" "=r")
	(truncate:SI (match_operand:TI 1 "spu_reg_operand" "r")))]
  ""
  "shlqbyi\t%0,%1,12"
  [(set_attr "type" "shuf")])

(define_insn "trunctidi2"
  [(set (match_operand:DI 0 "spu_reg_operand" "=r")
	(truncate:DI (match_operand:TI 1 "spu_reg_operand" "r")))]
  ""
  "shlqbyi\t%0,%1,8"
  [(set_attr "type" "shuf")])


;; float conversions

(define_insn "floatsisf2"
  [(set (match_operand:SF 0 "spu_reg_operand" "=r")
	(float:SF (match_operand:SI 1 "spu_reg_operand" "r")))]
  ""
  "csflt\t%0,%1,0"
  [(set_attr "type" "fp7")])

(define_insn "floatv4siv4sf2"
  [(set (match_operand:V4SF 0 "spu_reg_operand" "=r")
	(float:V4SF (match_operand:V4SI 1 "spu_reg_operand" "r")))]
  ""
  "csflt\t%0,%1,0"
  [(set_attr "type" "fp7")])

(define_insn "fix_truncsfsi2"
  [(set (match_operand:SI 0 "spu_reg_operand" "=r")
	(fix:SI (match_operand:SF 1 "spu_reg_operand" "r")))]
  ""
  "cflts\t%0,%1,0"
  [(set_attr "type" "fp7")])

(define_insn "fix_truncv4sfv4si2"
  [(set (match_operand:V4SI 0 "spu_reg_operand" "=r")
	(fix:V4SI (match_operand:V4SF 1 "spu_reg_operand" "r")))]
  ""
  "cflts\t%0,%1,0"
  [(set_attr "type" "fp7")])

(define_insn "floatunssisf2"
  [(set (match_operand:SF 0 "spu_reg_operand" "=r")
	(unsigned_float:SF (match_operand:SI 1 "spu_reg_operand" "r")))]
  ""
  "cuflt\t%0,%1,0"
  [(set_attr "type" "fp7")])

(define_insn "floatunsv4siv4sf2"
  [(set (match_operand:V4SF 0 "spu_reg_operand" "=r")
	(unsigned_float:V4SF (match_operand:V4SI 1 "spu_reg_operand" "r")))]
  ""
  "cuflt\t%0,%1,0"
  [(set_attr "type" "fp7")])

(define_insn "fixuns_truncsfsi2"
  [(set (match_operand:SI 0 "spu_reg_operand" "=r")
	(unsigned_fix:SI (match_operand:SF 1 "spu_reg_operand" "r")))]
  ""
  "cfltu\t%0,%1,0"
  [(set_attr "type" "fp7")])

(define_insn "fixuns_truncv4sfv4si2"
  [(set (match_operand:V4SI 0 "spu_reg_operand" "=r")
	(unsigned_fix:V4SI (match_operand:V4SF 1 "spu_reg_operand" "r")))]
  ""
  "cfltu\t%0,%1,0"
  [(set_attr "type" "fp7")])

(define_insn "extendsfdf2"
  [(set (match_operand:DF 0 "spu_reg_operand" "=r")
	(float_extend:DF (match_operand:SF 1 "spu_reg_operand" "r")))]
  ""
  "fesd\t%0,%1"
  [(set_attr "type" "fpd")])

(define_insn "truncdfsf2"
  [(set (match_operand:SF 0 "spu_reg_operand" "=r")
	(float_truncate:SF (match_operand:DF 1 "spu_reg_operand" "r")))]
  ""
  "frds\t%0,%1"
  [(set_attr "type" "fpd")])

;; Do (double)(operands[1]+0x80000000u)-(double)0x80000000
(define_expand "floatsidf2"
  [(set (match_operand:DF 0 "register_operand" "")
	(float:DF (match_operand:SI 1 "register_operand" "")))]
  ""
  {
    rtx value, insns;
    rtx c0 = gen_reg_rtx (SImode);
    rtx c1 = gen_reg_rtx (DFmode);
    rtx r0 = gen_reg_rtx (SImode);
    rtx r1 = gen_reg_rtx (DFmode);

    emit_move_insn (c0, GEN_INT (-0x80000000ll));
    emit_move_insn (c1, spu_float_const ("2147483648", DFmode));

    emit_insn (gen_xorsi3 (r0, operands[1], c0));

    start_sequence ();
    value =
      emit_library_call_value (ufloat_optab->handlers[DFmode][SImode].libfunc,
			       NULL_RTX, LCT_NORMAL, DFmode, 1, r0, SImode);
    insns = get_insns ();
    end_sequence ();
    emit_libcall_block (insns, r1, value,
			gen_rtx_UNSIGNED_FLOAT (DFmode, r0));

    emit_insn (gen_subdf3 (operands[0], r1, c1));
    DONE;
  })

(define_expand "floatdidf2"
  [(set (match_operand:DF 0 "register_operand" "")
	(float:DF (match_operand:DI 1 "register_operand" "")))]
  ""
  {
    rtx value, insns;
    rtx c0 = gen_reg_rtx (DImode);
    rtx r0 = gen_reg_rtx (DImode);
    rtx r1 = gen_reg_rtx (DFmode);
    rtx r2 = gen_reg_rtx (DImode);
    rtx setneg = gen_reg_rtx (DImode);
    rtx isneg = gen_reg_rtx (SImode);
    rtx neg = gen_reg_rtx (DImode);
    rtx mask = gen_reg_rtx (DImode);

    emit_move_insn (c0, GEN_INT (0x8000000000000000ull));

    emit_insn (gen_negdi2 (neg, operands[1]));
    emit_insn (gen_cgt_di_m1 (isneg, operands[1]));
    emit_insn (gen_extend_compare (mask, isneg));
    emit_insn (gen_selb (r0, neg, operands[1], mask));
    emit_insn (gen_andc_di (setneg, c0, mask));


    start_sequence ();
    value =
      emit_library_call_value (ufloat_optab->handlers[DFmode][DImode].libfunc,
			       NULL_RTX, LCT_NORMAL, DFmode, 1, r0, DImode);
    insns = get_insns ();
    end_sequence ();
    emit_libcall_block (insns, r1, value,
			gen_rtx_UNSIGNED_FLOAT (DFmode, r0));

    emit_insn (gen_iordi3 (r2, gen_rtx_SUBREG (DImode, r1, 0), setneg));
    emit_move_insn (operands[0], gen_rtx_SUBREG (DFmode, r2, 0));
    DONE;
  })

;; add

(define_expand "addv16qi3"
  [(set (match_operand:V16QI 0 "spu_reg_operand" "=r")
	(plus:V16QI (match_operand:V16QI 1 "spu_reg_operand" "r")
		    (match_operand:V16QI 2 "spu_reg_operand" "r")))]
  ""
  "{
    rtx res_short = simplify_gen_subreg (V8HImode, operands[0], V16QImode, 0);
    rtx lhs_short = simplify_gen_subreg (V8HImode, operands[1], V16QImode, 0);
    rtx rhs_short = simplify_gen_subreg (V8HImode, operands[2], V16QImode, 0);
    rtx rhs_and = gen_reg_rtx (V8HImode);
    rtx hi_char = gen_reg_rtx (V8HImode);
    rtx lo_char = gen_reg_rtx (V8HImode);
    rtx mask = gen_reg_rtx (V8HImode);

    emit_move_insn (mask, spu_const (V8HImode, 0x00ff));
    emit_insn (gen_andv8hi3 (rhs_and, rhs_short, spu_const (V8HImode, 0xff00)));
    emit_insn (gen_addv8hi3 (hi_char, lhs_short, rhs_and));
    emit_insn (gen_addv8hi3 (lo_char, lhs_short, rhs_short));
    emit_insn (gen_selb (res_short, hi_char, lo_char, mask));
    DONE;
   }")

(define_insn "add<mode>3"
  [(set (match_operand:VHSI 0 "spu_reg_operand" "=r,r")
	(plus:VHSI (match_operand:VHSI 1 "spu_reg_operand" "r,r")
		   (match_operand:VHSI 2 "spu_arith_operand" "r,B")))]
  ""
  "@
  a<bh>\t%0,%1,%2
  a<bh>i\t%0,%1,%2")

(define_expand "add<mode>3"
  [(set (match_dup:VDI 3) 
	(unspec:VDI [(match_operand:VDI 1 "spu_reg_operand" "")
		     (match_operand:VDI 2 "spu_reg_operand" "")] UNSPEC_CG))
   (set (match_dup:VDI 5)
	(unspec:VDI [(match_dup 3)
		     (match_dup 3)
		     (match_dup:TI 4)] UNSPEC_SHUFB))
   (set (match_operand:VDI 0 "spu_reg_operand" "") 
	(unspec:VDI [(match_dup 1)
		     (match_dup 2)
		     (match_dup 5)] UNSPEC_ADDX))]
  ""
  {
    unsigned char pat[16] = {
      0x04, 0x05, 0x06, 0x07,
      0x80, 0x80, 0x80, 0x80,
      0x0c, 0x0d, 0x0e, 0x0f,
      0x80, 0x80, 0x80, 0x80
    };
    operands[3] = gen_reg_rtx (<MODE>mode);
    operands[4] = gen_reg_rtx (TImode);
    operands[5] = gen_reg_rtx (<MODE>mode);
    emit_move_insn (operands[4], array_to_constant (TImode, pat));
  })

(define_insn "cg_<mode>"
  [(set (match_operand:CBOP 0 "spu_reg_operand" "=r")
	(unspec:CBOP [(match_operand 1 "spu_reg_operand" "r")
		      (match_operand 2 "spu_reg_operand" "r")] UNSPEC_CG))]
  "operands"
  "cg\t%0,%1,%2")

(define_insn "cgx_<mode>"
  [(set (match_operand:CBOP 0 "spu_reg_operand" "=r")
	(unspec:CBOP [(match_operand 1 "spu_reg_operand" "r")
		      (match_operand 2 "spu_reg_operand" "r")
		      (match_operand 3 "spu_reg_operand" "0")] UNSPEC_CGX))]
  "operands"
  "cgx\t%0,%1,%2")

(define_insn "addx_<mode>"
  [(set (match_operand:CBOP 0 "spu_reg_operand" "=r")
	(unspec:CBOP [(match_operand 1 "spu_reg_operand" "r")
		      (match_operand 2 "spu_reg_operand" "r")
		      (match_operand 3 "spu_reg_operand" "0")] UNSPEC_ADDX))]
  "operands"
  "addx\t%0,%1,%2")


;; This is not the most efficient implementation of addti3.
;; We include this here because 1) the compiler needs it to be
;; defined as the word size is 128-bit and 2) sometimes gcc
;; substitutes an add for a constant left-shift. 2) is unlikely
;; because we also give addti3 a high cost. In case gcc does
;; generate TImode add, here is the code to do it.
;; operand 2 is a nonmemory because the compiler requires it.
(define_insn "addti3"
  [(set (match_operand:TI 0 "spu_reg_operand" "=&r")
	(plus:TI (match_operand:TI 1 "spu_reg_operand" "r")
		 (match_operand:TI 2 "spu_nonmem_operand" "r")))
   (clobber (match_scratch:TI 3 "=&r"))]
  ""
  "cg\t%3,%1,%2\n\\
   shlqbyi\t%3,%3,4\n\\
   cgx\t%3,%1,%2\n\\
   shlqbyi\t%3,%3,4\n\\
   cgx\t%3,%1,%2\n\\
   shlqbyi\t%0,%3,4\n\\
   addx\t%0,%1,%2"
  [(set_attr "type" "multi0")
   (set_attr "length" "28")])

(define_insn "add<mode>3"
  [(set (match_operand:VSF 0 "spu_reg_operand" "=r")
	(plus:VSF (match_operand:VSF 1 "spu_reg_operand" "r")
		  (match_operand:VSF 2 "spu_reg_operand" "r")))]
  ""
  "fa\t%0,%1,%2"
  [(set_attr "type" "fp6")])

(define_insn "add<mode>3"
  [(set (match_operand:VDF 0 "spu_reg_operand" "=r")
	(plus:VDF (match_operand:VDF 1 "spu_reg_operand" "r")
		  (match_operand:VDF 2 "spu_reg_operand" "r")))]
  ""
  "dfa\t%0,%1,%2"
  [(set_attr "type" "fpd")])


;; sub

(define_expand "subv16qi3"
  [(set (match_operand:V16QI 0 "spu_reg_operand" "=r")
	(minus:V16QI (match_operand:V16QI 1 "spu_reg_operand" "r")
		     (match_operand:V16QI 2 "spu_reg_operand" "r")))]
  ""
  "{
    rtx res_short = simplify_gen_subreg (V8HImode, operands[0], V16QImode, 0);
    rtx lhs_short = simplify_gen_subreg (V8HImode, operands[1], V16QImode, 0);
    rtx rhs_short = simplify_gen_subreg (V8HImode, operands[2], V16QImode, 0);
    rtx rhs_and = gen_reg_rtx (V8HImode);
    rtx hi_char = gen_reg_rtx (V8HImode);
    rtx lo_char = gen_reg_rtx (V8HImode);
    rtx mask = gen_reg_rtx (V8HImode);

    emit_move_insn (mask, spu_const (V8HImode, 0x00ff));
    emit_insn (gen_andv8hi3 (rhs_and, rhs_short, spu_const (V8HImode, 0xff00)));
    emit_insn (gen_subv8hi3 (hi_char, lhs_short, rhs_and));
    emit_insn (gen_subv8hi3 (lo_char, lhs_short, rhs_short));
    emit_insn (gen_selb (res_short, hi_char, lo_char, mask));
    DONE;
   }")

(define_insn "sub<mode>3"
  [(set (match_operand:VHSI 0 "spu_reg_operand" "=r,r")
	(minus:VHSI (match_operand:VHSI 1 "spu_arith_operand" "r,B")
		    (match_operand:VHSI 2 "spu_reg_operand" "r,r")))]
  ""
  "@
  sf<bh>\t%0,%2,%1
  sf<bh>i\t%0,%2,%1")

(define_expand "sub<mode>3"
  [(set (match_dup:VDI 3) 
	(unspec:VDI [(match_operand:VDI 1 "spu_reg_operand" "")
		     (match_operand:VDI 2 "spu_reg_operand" "")] UNSPEC_BG))
   (set (match_dup:VDI 5)
	(unspec:VDI [(match_dup 3)
		     (match_dup 3)
		     (match_dup:TI 4)] UNSPEC_SHUFB))
   (set (match_operand:VDI 0 "spu_reg_operand" "") 
	(unspec:VDI [(match_dup 1)
		     (match_dup 2)
		     (match_dup 5)] UNSPEC_SFX))]
  ""
  {
    unsigned char pat[16] = {
      0x04, 0x05, 0x06, 0x07,
      0xc0, 0xc0, 0xc0, 0xc0,
      0x0c, 0x0d, 0x0e, 0x0f,
      0xc0, 0xc0, 0xc0, 0xc0
    };
    operands[3] = gen_reg_rtx (<MODE>mode);
    operands[4] = gen_reg_rtx (TImode);
    operands[5] = gen_reg_rtx (<MODE>mode);
    emit_move_insn (operands[4], array_to_constant (TImode, pat));
  })

(define_insn "bg_<mode>"
  [(set (match_operand:CBOP 0 "spu_reg_operand" "=r")
	(unspec:CBOP [(match_operand 1 "spu_reg_operand" "r")
		      (match_operand 2 "spu_reg_operand" "r")] UNSPEC_BG))]
  "operands"
  "bg\t%0,%2,%1")

(define_insn "bgx_<mode>"
  [(set (match_operand:CBOP 0 "spu_reg_operand" "=r")
	(unspec:CBOP [(match_operand 1 "spu_reg_operand" "r")
		      (match_operand 2 "spu_reg_operand" "r")
		      (match_operand 3 "spu_reg_operand" "0")] UNSPEC_BGX))]
  "operands"
  "bgx\t%0,%2,%1")

(define_insn "sfx_<mode>"
  [(set (match_operand:CBOP 0 "spu_reg_operand" "=r")
	(unspec:CBOP [(match_operand 1 "spu_reg_operand" "r")
		      (match_operand 2 "spu_reg_operand" "r")
		      (match_operand 3 "spu_reg_operand" "0")] UNSPEC_SFX))]
  "operands"
  "sfx\t%0,%2,%1")

(define_insn "subti3"
  [(set (match_operand:TI 0 "spu_reg_operand" "=r")
	(minus:TI (match_operand:TI 1 "spu_reg_operand" "r")
		  (match_operand:TI 2 "spu_reg_operand" "r")))
   (clobber (match_scratch:TI 3 "=&r"))
   (clobber (match_scratch:TI 4 "=&r"))
   (clobber (match_scratch:TI 5 "=&r"))]
  ""
  "bg\t%3,%1,%2\n\\
   sf\t%4,%2,%1\n\\
   shlqbyi\t%5,%3,4\n\\
   bg\t%3,%4,%5\n\\
   sf\t%4,%5,%4\n\\
   shlqbyi\t%5,%3,4\n\\
   bg\t%3,%4,%5\n\\
   shlqbyi\t%0,%3,4\n\\
   sfx\t%0,%5,%4"
  [(set_attr "type" "multi0")
   (set_attr "length" "36")])

(define_insn "sub<mode>3"
  [(set (match_operand:VSF 0 "spu_reg_operand" "=r")
	(minus:VSF (match_operand:VSF 1 "spu_reg_operand" "r")
		   (match_operand:VSF 2 "spu_reg_operand" "r")))]
  ""
  "fs\t%0,%1,%2"
  [(set_attr "type" "fp6")])

(define_insn "sub<mode>3"
  [(set (match_operand:VDF 0 "spu_reg_operand" "=r")
	(minus:VDF (match_operand:VDF 1 "spu_reg_operand" "r")
		   (match_operand:VDF 2 "spu_reg_operand" "r")))]
  ""
  "dfs\t%0,%1,%2"
  [(set_attr "type" "fpd")])


;; neg

(define_expand "negv16qi2"
  [(set (match_operand:V16QI 0 "spu_reg_operand" "=r")
	(neg:V16QI (match_operand:V16QI 1 "spu_reg_operand" "r")))]
  ""
  "{
    rtx zero = gen_reg_rtx (V16QImode);
    emit_move_insn (zero, CONST0_RTX (V16QImode));
    emit_insn (gen_subv16qi3 (operands[0], zero, operands[1]));
    DONE;
   }")

(define_insn "neg<mode>2"
  [(set (match_operand:VHSI 0 "spu_reg_operand" "=r")
	(neg:VHSI (match_operand:VHSI 1 "spu_reg_operand" "r")))]
  ""
  "sf<bh>i\t%0,%1,0")

(define_expand "negdi2"
  [(set (match_operand:DI 0 "spu_reg_operand" "")
	(neg:DI (match_operand:DI 1 "spu_reg_operand" "")))]
  ""
  {
    rtx zero = gen_reg_rtx(DImode);
    emit_move_insn(zero, GEN_INT(0));
    emit_insn(gen_subdi3(operands[0], zero, operands[1]));
    DONE;
  })

(define_expand "negti2"
  [(set (match_operand:TI 0 "spu_reg_operand" "")
	(neg:TI (match_operand:TI 1 "spu_reg_operand" "")))]
  ""
  {
    rtx zero = gen_reg_rtx(TImode);
    emit_move_insn(zero, GEN_INT(0));
    emit_insn(gen_subti3(operands[0], zero, operands[1]));
    DONE;
  })

(define_expand "neg<mode>2"
  [(parallel
    [(set (match_operand:VSF 0 "spu_reg_operand" "")
	  (neg:VSF (match_operand:VSF 1 "spu_reg_operand" "")))
     (use (match_dup 2))])]
  ""
  "operands[2] = gen_reg_rtx (<f2i>mode);
   emit_move_insn (operands[2], spu_const (<f2i>mode, -0x80000000ull));")

(define_expand "neg<mode>2"
  [(parallel
    [(set (match_operand:VDF 0 "spu_reg_operand" "")
	  (neg:VDF (match_operand:VDF 1 "spu_reg_operand" "")))
     (use (match_dup 2))])]
  ""
  "operands[2] = gen_reg_rtx (<f2i>mode);
   emit_move_insn (operands[2], spu_const (<f2i>mode, -0x8000000000000000ull));")

(define_insn_and_split "_neg<mode>2"
  [(set (match_operand:VSDF 0 "spu_reg_operand" "=r")
	(neg:VSDF (match_operand:VSDF 1 "spu_reg_operand" "r")))
   (use (match_operand:<f2i> 2 "spu_reg_operand" "r"))]
  ""
  "#"
  ""
  [(set (match_dup:<f2i> 3)
	(xor:<f2i> (match_dup:<f2i> 4)
		   (match_dup:<f2i> 2)))]
  {
    operands[3] = spu_gen_subreg (<f2i>mode, operands[0]);
    operands[4] = spu_gen_subreg (<f2i>mode, operands[1]);
  })


;; abs

(define_expand "abs<mode>2"
  [(parallel
    [(set (match_operand:VSF 0 "spu_reg_operand" "")
	  (abs:VSF (match_operand:VSF 1 "spu_reg_operand" "")))
     (use (match_dup 2))])]
  ""
  "operands[2] = gen_reg_rtx (<f2i>mode);
   emit_move_insn (operands[2], spu_const (<f2i>mode, 0x7fffffffull));")

(define_expand "abs<mode>2"
  [(parallel
    [(set (match_operand:VDF 0 "spu_reg_operand" "")
	  (abs:VDF (match_operand:VDF 1 "spu_reg_operand" "")))
     (use (match_dup 2))])]
  ""
  "operands[2] = gen_reg_rtx (<f2i>mode);
   emit_move_insn (operands[2], spu_const (<f2i>mode, 0x7fffffffffffffffull));")

(define_insn_and_split "_abs<mode>2"
  [(set (match_operand:VSDF 0 "spu_reg_operand" "=r")
	(abs:VSDF (match_operand:VSDF 1 "spu_reg_operand" "r")))
   (use (match_operand:<f2i> 2 "spu_reg_operand" "r"))]
  ""
  "#"
  ""
  [(set (match_dup:<f2i> 3)
	(and:<f2i> (match_dup:<f2i> 4)
		   (match_dup:<f2i> 2)))]
  {
    operands[3] = spu_gen_subreg (<f2i>mode, operands[0]);
    operands[4] = spu_gen_subreg (<f2i>mode, operands[1]);
  })


;; mul

(define_insn "mulhi3"
  [(set (match_operand:HI 0 "spu_reg_operand" "=r,r")
	(mult:HI (match_operand:HI 1 "spu_reg_operand" "r,r")
		 (match_operand:HI 2 "spu_arith_operand" "r,B")))]
  ""
  "@
  mpy\t%0,%1,%2
  mpyi\t%0,%1,%2"
  [(set_attr "type" "fp7")])

(define_expand "mulv8hi3"
  [(set (match_operand:V8HI 0 "spu_reg_operand" "")
	(mult:V8HI (match_operand:V8HI 1 "spu_reg_operand" "")
		   (match_operand:V8HI 2 "spu_reg_operand" "")))]
  ""
  "{
    rtx result = simplify_gen_subreg (V4SImode, operands[0], V8HImode, 0);
    rtx low = gen_reg_rtx (V4SImode);
    rtx high = gen_reg_rtx (V4SImode);
    rtx shift = gen_reg_rtx (V4SImode);
    rtx mask = gen_reg_rtx (V4SImode);

    emit_move_insn (mask, spu_const (V4SImode, 0x0000ffff));
    emit_insn (gen_spu_mpyhh (high, operands[1], operands[2]));
    emit_insn (gen_spu_mpy (low, operands[1], operands[2]));
    emit_insn (gen_ashlv4si3 (shift, high, spu_const(V4SImode, 16)));
    emit_insn (gen_selb (result, shift, low, mask));
    DONE;
   }")

(define_expand "mul<mode>3"
  [(parallel
    [(set (match_operand:VSI 0 "spu_reg_operand" "")
	  (mult:VSI (match_operand:VSI 1 "spu_reg_operand" "")
		    (match_operand:VSI 2 "spu_reg_operand" "")))
     (clobber (match_dup:VSI 3))
     (clobber (match_dup:VSI 4))
     (clobber (match_dup:VSI 5))
     (clobber (match_dup:VSI 6))])]
  ""
  {
    operands[3] = gen_reg_rtx(<MODE>mode);
    operands[4] = gen_reg_rtx(<MODE>mode);
    operands[5] = gen_reg_rtx(<MODE>mode);
    operands[6] = gen_reg_rtx(<MODE>mode);
  })

(define_insn_and_split "_mulsi3"
  [(set (match_operand:SI 0 "spu_reg_operand" "=r")
	(mult:SI (match_operand:SI 1 "spu_reg_operand" "r")
		 (match_operand:SI 2 "spu_arith_operand" "rK")))
   (clobber (match_operand:SI 3 "spu_reg_operand" "=&r"))
   (clobber (match_operand:SI 4 "spu_reg_operand" "=&r"))
   (clobber (match_operand:SI 5 "spu_reg_operand" "=&r"))
   (clobber (match_operand:SI 6 "spu_reg_operand" "=&r"))]
  ""
  "#"
  ""
  [(set (match_dup:SI 0)
	(mult:SI (match_dup:SI 1)
		 (match_dup:SI 2)))]
  {
    HOST_WIDE_INT val = 0;
    rtx a = operands[3];
    rtx b = operands[4];
    rtx c = operands[5];
    rtx d = operands[6];
    if (GET_CODE(operands[2]) == CONST_INT)
      {
	val = INTVAL(operands[2]);
	emit_move_insn(d, operands[2]);
	operands[2] = d;
      }
    if (val && (val & 0xffff) == 0)
      {
	emit_insn(gen_mpyh_si(operands[0], operands[2], operands[1]));
      }
    else if (val > 0 && val < 0x10000)
      {
	rtx cst = satisfies_constraint_K (GEN_INT (val)) ? GEN_INT(val) : d;
	emit_insn(gen_mpyh_si(a, operands[1], operands[2]));
	emit_insn(gen_mpyu_si(c, operands[1], cst));
	emit_insn(gen_addsi3(operands[0], a, c));
      }
    else
      {
	emit_insn(gen_mpyh_si(a, operands[1], operands[2]));
	emit_insn(gen_mpyh_si(b, operands[2], operands[1]));
	emit_insn(gen_mpyu_si(c, operands[1], operands[2]));
	emit_insn(gen_addsi3(d, a, b));
	emit_insn(gen_addsi3(operands[0], d, c));
      }
    DONE;
   })

(define_insn_and_split "_mulv4si3"
  [(set (match_operand:V4SI 0 "spu_reg_operand" "=r")
	(mult:V4SI (match_operand:V4SI 1 "spu_reg_operand" "r")
		   (match_operand:V4SI 2 "spu_reg_operand" "r")))
   (clobber (match_operand:V4SI 3 "spu_reg_operand" "=&r"))
   (clobber (match_operand:V4SI 4 "spu_reg_operand" "=&r"))
   (clobber (match_operand:V4SI 5 "spu_reg_operand" "=&r"))
   (clobber (match_operand:V4SI 6 "spu_reg_operand" "=&r"))]
  ""
  "#"
  ""
  [(set (match_dup:V4SI 0)
	(mult:V4SI (match_dup:V4SI 1)
		   (match_dup:V4SI 2)))]
  {
    rtx a = operands[3];
    rtx b = operands[4];
    rtx c = operands[5];
    rtx d = operands[6];
    rtx op1 = simplify_gen_subreg (V8HImode, operands[1], V4SImode, 0);
    rtx op2 = simplify_gen_subreg (V8HImode, operands[2], V4SImode, 0);
    emit_insn(gen_spu_mpyh(a, op1, op2));
    emit_insn(gen_spu_mpyh(b, op2, op1));
    emit_insn(gen_spu_mpyu(c, op1, op2));
    emit_insn(gen_addv4si3(d, a, b));
    emit_insn(gen_addv4si3(operands[0], d, c));
    DONE;
   })

(define_insn "mulhisi3"
  [(set (match_operand:SI 0 "spu_reg_operand" "=r")
	(mult:SI (sign_extend:SI (match_operand:HI 1 "spu_reg_operand" "r"))
		 (sign_extend:SI (match_operand:HI 2 "spu_reg_operand" "r"))))]
  ""
  "mpy\t%0,%1,%2"
  [(set_attr "type" "fp7")])

(define_insn "mulhisi3_imm"
  [(set (match_operand:SI 0 "spu_reg_operand" "=r")
	(mult:SI (sign_extend:SI (match_operand:HI 1 "spu_reg_operand" "r"))
		 (match_operand:SI 2 "immediate_operand" "K")))]
  ""
  "mpyi\t%0,%1,%2"
  [(set_attr "type" "fp7")])

(define_insn "umulhisi3"
  [(set (match_operand:SI 0 "spu_reg_operand" "=r")
	(mult:SI (zero_extend:SI (match_operand:HI 1 "spu_reg_operand" "r"))
		 (zero_extend:SI (match_operand:HI 2 "spu_reg_operand" "r"))))]
  ""
  "mpyu\t%0,%1,%2"
  [(set_attr "type" "fp7")])

(define_insn "umulhisi3_imm"
  [(set (match_operand:SI 0 "spu_reg_operand" "=r")
	(mult:SI (zero_extend:SI (match_operand:HI 1 "spu_reg_operand" "r"))
		 (and:SI (match_operand:SI 2 "immediate_operand" "K") (const_int 65535))))]
  ""
  "mpyui\t%0,%1,%2"
  [(set_attr "type" "fp7")])

(define_insn "mpyu_si"
  [(set (match_operand:SI 0 "spu_reg_operand" "=r,r")
	(mult:SI (and:SI (match_operand:SI 1 "spu_reg_operand" "r,r")
			 (const_int 65535))
		 (and:SI (match_operand:SI 2 "spu_arith_operand" "r,K")
			 (const_int 65535))))]
  ""
  "@
   mpyu\t%0,%1,%2
   mpyui\t%0,%1,%2"
  [(set_attr "type" "fp7")])

;; This isn't always profitable to use.  Consider r = a * b + c * d.
;; It's faster to do the multiplies in parallel then add them.  If we
;; merge a multiply and add it prevents the multiplies from happening in
;; parallel.
(define_insn "mpya_si"
  [(set (match_operand:SI 0 "spu_reg_operand" "=r")
	(plus:SI (mult:SI (sign_extend:SI (match_operand:HI 1 "spu_reg_operand" "r"))
			  (sign_extend:SI (match_operand:HI 2 "spu_reg_operand" "r")))
		 (match_operand:SI 3 "spu_reg_operand" "r")))]
  "0"
  "mpya\t%0,%1,%2,%3"
  [(set_attr "type" "fp7")])

(define_insn "mpyh_si"
  [(set (match_operand:SI 0 "spu_reg_operand" "=r")
	(mult:SI (and:SI (match_operand:SI 1 "spu_reg_operand" "r")
			 (const_int -65536))
	         (and:SI (match_operand:SI 2 "spu_reg_operand" "r")
			 (const_int 65535))))]
  ""
  "mpyh\t%0,%1,%2"
  [(set_attr "type" "fp7")])

(define_insn "mpys_si"
  [(set (match_operand:SI 0 "spu_reg_operand" "=r")
	(ashiftrt:SI
	    (mult:SI (sign_extend:SI (match_operand:HI 1 "spu_reg_operand" "r"))
		     (sign_extend:SI (match_operand:HI 2 "spu_reg_operand" "r")))
	    (const_int 16)))]
  ""
  "mpys\t%0,%1,%2"
  [(set_attr "type" "fp7")])

(define_insn "mpyhh_si"
  [(set (match_operand:SI 0 "spu_reg_operand" "=r")
	(mult:SI (ashiftrt:SI (match_operand:SI 1 "spu_reg_operand" "r")
			      (const_int 16))
		 (ashiftrt:SI (match_operand:SI 2 "spu_reg_operand" "r")
			      (const_int 16))))]
  ""
  "mpyhh\t%0,%1,%2"
  [(set_attr "type" "fp7")])

(define_insn "mpyhhu_si"
  [(set (match_operand:SI 0 "spu_reg_operand" "=r")
	(mult:SI (lshiftrt:SI (match_operand:SI 1 "spu_reg_operand" "r")
			      (const_int 16))
		 (lshiftrt:SI (match_operand:SI 2 "spu_reg_operand" "r")
			      (const_int 16))))]
  ""
  "mpyhhu\t%0,%1,%2"
  [(set_attr "type" "fp7")])

(define_insn "mpyhha_si" 
  [(set (match_operand:SI 0 "spu_reg_operand" "=r")
	(plus:SI (mult:SI (ashiftrt:SI (match_operand:SI 1 "spu_reg_operand" "r")
				       (const_int 16))
			  (ashiftrt:SI (match_operand:SI 2 "spu_reg_operand" "r")
				       (const_int 16)))
		 (match_operand:SI 3 "spu_reg_operand" "0")))]
  "0"
  "mpyhha\t%0,%1,%2"
  [(set_attr "type" "fp7")])

(define_insn "mul<mode>3"
  [(set (match_operand:VSDF 0 "spu_reg_operand" "=r")
	(mult:VSDF (match_operand:VSDF 1 "spu_reg_operand" "r")
		   (match_operand:VSDF 2 "spu_reg_operand" "r")))]
  ""
  "<d>fm\t%0,%1,%2"
  [(set_attr "type" "fp<d6>")])

(define_insn "fma_<mode>"
  [(set (match_operand:VSF 0 "spu_reg_operand" "=r")
	(plus:VSF (mult:VSF (match_operand:VSF 1 "spu_reg_operand" "r")
			      (match_operand:VSF 2 "spu_reg_operand" "r"))
		   (match_operand:VSF 3 "spu_reg_operand" "r")))]
  ""
  "fma\t%0,%1,%2,%3"
  [(set_attr "type"	"fp6")])

(define_insn "fnms_<mode>"
  [(set (match_operand:VSF 0 "spu_reg_operand" "=r")
	(minus:VSF (match_operand:VSF 3 "spu_reg_operand" "r")
		    (mult:VSF (match_operand:VSF 1 "spu_reg_operand" "r")
			       (match_operand:VSF 2 "spu_reg_operand" "r"))))]
  ""
  "fnms\t%0,%1,%2,%3"
  [(set_attr "type" "fp6")])

(define_insn "fms_<mode>"
  [(set (match_operand:VSF 0 "spu_reg_operand" "=r")
	(minus:VSF (mult:VSF (match_operand:VSF 1 "spu_reg_operand" "r")
			       (match_operand:VSF 2 "spu_reg_operand" "r"))
		    (match_operand:VSF 3 "spu_reg_operand" "r")))]
  ""
  "fms\t%0,%1,%2,%3"
  [(set_attr "type" "fp6")])

(define_insn "fma_<mode>"
  [(set (match_operand:VDF 0 "spu_reg_operand" "=r")
	(plus:VDF (mult:VDF (match_operand:VDF 1 "spu_reg_operand" "r")
			    (match_operand:VDF 2 "spu_reg_operand" "r"))
		  (match_operand:VDF 3 "spu_reg_operand" "0")))]
  ""
  "dfma\t%0,%1,%2"
  [(set_attr "type"	"fpd")])

(define_insn "fnma_<mode>"
  [(set (match_operand:VDF 0 "spu_reg_operand" "=r")
	(neg:VDF (plus:VDF (mult:VDF (match_operand:VDF 1 "spu_reg_operand" "r")
				     (match_operand:VDF 2 "spu_reg_operand" "r"))
			   (match_operand:VDF 3 "spu_reg_operand" "0"))))]
  ""
  "dfnma\t%0,%1,%2"
  [(set_attr "type"	"fpd")])

(define_insn "fnms_<mode>"
  [(set (match_operand:VDF 0 "spu_reg_operand" "=r")
	(minus:VDF (match_operand:VDF 3 "spu_reg_operand" "0")
		   (mult:VDF (match_operand:VDF 1 "spu_reg_operand" "r")
			     (match_operand:VDF 2 "spu_reg_operand" "r"))))]
  ""
  "dfnms\t%0,%1,%2"
  [(set_attr "type" "fpd")])

(define_insn "fms_<mode>"
  [(set (match_operand:VDF 0 "spu_reg_operand" "=r")
	(minus:VDF (mult:VDF (match_operand:VDF 1 "spu_reg_operand" "r")
			     (match_operand:VDF 2 "spu_reg_operand" "r"))
		   (match_operand:VDF 3 "spu_reg_operand" "0")))]
  ""
  "dfms\t%0,%1,%2"
  [(set_attr "type" "fpd")])


;; mul highpart, used for divide by constant optimizations.

(define_expand "smulsi3_highpart"
  [(set (match_operand:SI 0 "register_operand" "")
	(truncate:SI
	  (ashiftrt:DI
	    (mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand" ""))
	             (sign_extend:DI (match_operand:SI 2 "register_operand" "")))
	    (const_int 32))))]
  ""
  {
    rtx t0 = gen_reg_rtx (SImode);
    rtx t1 = gen_reg_rtx (SImode);
    rtx t2 = gen_reg_rtx (SImode);
    rtx t3 = gen_reg_rtx (SImode);
    rtx t4 = gen_reg_rtx (SImode);
    rtx t5 = gen_reg_rtx (SImode);
    rtx t6 = gen_reg_rtx (SImode);
    rtx t7 = gen_reg_rtx (SImode);
    rtx t8 = gen_reg_rtx (SImode);
    rtx t9 = gen_reg_rtx (SImode);
    rtx t11 = gen_reg_rtx (SImode);
    rtx t12 = gen_reg_rtx (SImode);
    rtx t14 = gen_reg_rtx (SImode);
    rtx t15 = gen_reg_rtx (HImode);
    rtx t16 = gen_reg_rtx (HImode);
    rtx t17 = gen_reg_rtx (HImode);
    rtx t18 = gen_reg_rtx (HImode);
    rtx t19 = gen_reg_rtx (SImode);
    rtx t20 = gen_reg_rtx (SImode);
    rtx t21 = gen_reg_rtx (SImode);
    rtx op1_hi = gen_rtx_SUBREG (HImode, operands[1], 2);
    rtx op2_hi = gen_rtx_SUBREG (HImode, operands[2], 2);
    rtx t0_hi = gen_rtx_SUBREG (HImode, t0, 2);
    rtx t1_hi = gen_rtx_SUBREG (HImode, t1, 2);

    emit_insn (gen_lshrsi3 (t0, operands[1], GEN_INT (16)));
    emit_insn (gen_lshrsi3 (t1, operands[2], GEN_INT (16)));
    emit_insn (gen_umulhisi3 (t2, op1_hi, op2_hi));
    emit_insn (gen_mpyh_si (t3, operands[1], operands[2]));
    emit_insn (gen_mpyh_si (t4, operands[2], operands[1]));
    emit_insn (gen_mpyhh_si (t5, operands[1], operands[2]));
    emit_insn (gen_mpys_si (t6, t0_hi, op2_hi));
    emit_insn (gen_mpys_si (t7, t1_hi, op1_hi));

    /* Gen carry bits (in t9 and t11). */
    emit_insn (gen_addsi3 (t8, t2, t3));
    emit_insn (gen_cg_si (t9, t2, t3));
    emit_insn (gen_cg_si (t11, t8, t4));

    /* Gen high 32 bits in operand[0].  Correct for mpys. */
    emit_insn (gen_addx_si (t12, t5, t6, t9));
    emit_insn (gen_addx_si (t14, t12, t7, t11));

    /* mpys treats both operands as signed when we really want it to treat
       the first operand as signed and the second operand as unsigned.
       The code below corrects for that difference.  */
    emit_insn (gen_cgt_hi (t15, op1_hi, GEN_INT (-1)));
    emit_insn (gen_cgt_hi (t16, op2_hi, GEN_INT (-1)));
    emit_insn (gen_andc_hi (t17, t1_hi, t15));
    emit_insn (gen_andc_hi (t18, t0_hi, t16));
    emit_insn (gen_extendhisi2 (t19, t17));
    emit_insn (gen_extendhisi2 (t20, t18));
    emit_insn (gen_addsi3 (t21, t19, t20));
    emit_insn (gen_addsi3 (operands[0], t14, t21));
    DONE;
  })

(define_expand "umulsi3_highpart"
  [(set (match_operand:SI 0 "register_operand" "")
	(truncate:SI
	  (ashiftrt:DI
	    (mult:DI (zero_extend:DI (match_operand:SI 1 "register_operand" ""))
	             (zero_extend:DI (match_operand:SI 2 "register_operand" "")))
	    (const_int 32))))]
  ""
  
  {
    rtx t0 = gen_reg_rtx (SImode);
    rtx t1 = gen_reg_rtx (SImode);
    rtx t2 = gen_reg_rtx (SImode);
    rtx t3 = gen_reg_rtx (SImode);
    rtx t4 = gen_reg_rtx (SImode);
    rtx t5 = gen_reg_rtx (SImode);
    rtx t6 = gen_reg_rtx (SImode);
    rtx t7 = gen_reg_rtx (SImode);
    rtx t8 = gen_reg_rtx (SImode);
    rtx t9 = gen_reg_rtx (SImode);
    rtx t10 = gen_reg_rtx (SImode);
    rtx t12 = gen_reg_rtx (SImode);
    rtx t13 = gen_reg_rtx (SImode);
    rtx t14 = gen_reg_rtx (SImode);
    rtx op1_hi = gen_rtx_SUBREG (HImode, operands[1], 2);
    rtx op2_hi = gen_rtx_SUBREG (HImode, operands[2], 2);
    rtx t0_hi = gen_rtx_SUBREG (HImode, t0, 2);

    emit_insn (gen_rotlsi3 (t0, operands[2], GEN_INT (16)));
    emit_insn (gen_umulhisi3 (t1, op1_hi, op2_hi));
    emit_insn (gen_umulhisi3 (t2, op1_hi, t0_hi));
    emit_insn (gen_mpyhhu_si (t3, operands[1], t0));
    emit_insn (gen_mpyhhu_si (t4, operands[1], operands[2]));
    emit_insn (gen_ashlsi3 (t5, t2, GEN_INT (16)));
    emit_insn (gen_ashlsi3 (t6, t3, GEN_INT (16)));
    emit_insn (gen_lshrsi3 (t7, t2, GEN_INT (16)));
    emit_insn (gen_lshrsi3 (t8, t3, GEN_INT (16)));

    /* Gen carry bits (in t10 and t12). */
    emit_insn (gen_addsi3 (t9, t1, t5));
    emit_insn (gen_cg_si (t10, t1, t5));
    emit_insn (gen_cg_si (t12, t9, t6));

    /* Gen high 32 bits in operand[0]. */
    emit_insn (gen_addx_si (t13, t4, t7, t10));
    emit_insn (gen_addx_si (t14, t13, t8, t12));
    emit_insn (gen_movsi (operands[0], t14));

    DONE;
  })

;; div

;; Not necessarily the best implementation of divide but faster then
;; the default that gcc provides because this is inlined and it uses
;; clz.
(define_insn "divmodsi4"
      [(set (match_operand:SI 0 "spu_reg_operand" "=&r")
	    (div:SI (match_operand:SI 1 "spu_reg_operand" "r")
		    (match_operand:SI 2 "spu_reg_operand" "r")))
       (set (match_operand:SI 3 "spu_reg_operand" "=&r")
	    (mod:SI (match_dup 1)
		    (match_dup 2)))
       (clobber (match_scratch:SI 4 "=&r"))
       (clobber (match_scratch:SI 5 "=&r"))
       (clobber (match_scratch:SI 6 "=&r"))
       (clobber (match_scratch:SI 7 "=&r"))
       (clobber (match_scratch:SI 8 "=&r"))
       (clobber (match_scratch:SI 9 "=&r"))
       (clobber (match_scratch:SI 10 "=&r"))
       (clobber (match_scratch:SI 11 "=&r"))
       (clobber (match_scratch:SI 12 "=&r"))
       (clobber (reg:SI 130))]
  ""
  "heqi	%2,0\\n\\
	hbrr	3f,1f\\n\\
	sfi	%8,%1,0\\n\\
	sfi	%9,%2,0\\n\\
	cgti	%10,%1,-1\\n\\
	cgti	%11,%2,-1\\n\\
	selb	%8,%8,%1,%10\\n\\
	selb	%9,%9,%2,%11\\n\\
	clz	%4,%8\\n\\
	clz	%7,%9\\n\\
	il	%5,1\\n\\
	fsmbi	%0,0\\n\\
	sf	%7,%4,%7\\n\\
	shlqbyi	%3,%8,0\\n\\
	xor	%11,%10,%11\\n\\
	shl	%5,%5,%7\\n\\
	shl	%4,%9,%7\\n\\
	lnop	\\n\\
1:	or	%12,%0,%5\\n\\
	rotqmbii	%5,%5,-1\\n\\
	clgt	%6,%4,%3\\n\\
	lnop	\\n\\
	sf	%7,%4,%3\\n\\
	rotqmbii	%4,%4,-1\\n\\
	selb	%0,%12,%0,%6\\n\\
	lnop	\\n\\
	selb	%3,%7,%3,%6\\n\\
3:	brnz	%5,1b\\n\\
2:	sfi	%8,%3,0\\n\\
	sfi	%9,%0,0\\n\\
	selb	%3,%8,%3,%10\\n\\
	selb	%0,%0,%9,%11"
  [(set_attr "type" "multi0")
   (set_attr "length" "128")])

(define_insn "udivmodsi4"
      [(set (match_operand:SI 0 "spu_reg_operand" "=&r")
	    (udiv:SI (match_operand:SI 1 "spu_reg_operand" "r")
		     (match_operand:SI 2 "spu_reg_operand" "r")))
       (set (match_operand:SI 3 "spu_reg_operand" "=&r")
	    (umod:SI (match_dup 1)
		     (match_dup 2)))
       (clobber (match_scratch:SI 4 "=&r"))
       (clobber (match_scratch:SI 5 "=&r"))
       (clobber (match_scratch:SI 6 "=&r"))
       (clobber (match_scratch:SI 7 "=&r"))
       (clobber (match_scratch:SI 8 "=&r"))
       (clobber (reg:SI 130))]
  ""
  "heqi	%2,0\\n\\
	hbrr	3f,1f\\n\\
	clz	%7,%2\\n\\
	clz	%4,%1\\n\\
	il	%5,1\\n\\
	fsmbi	%0,0\\n\\
	sf	%7,%4,%7\\n\\
	ori	%3,%1,0\\n\\
	shl	%5,%5,%7\\n\\
	shl	%4,%2,%7\\n\\
1:	or	%8,%0,%5\\n\\
	rotqmbii	%5,%5,-1\\n\\
	clgt	%6,%4,%3\\n\\
	lnop	\\n\\
	sf	%7,%4,%3\\n\\
	rotqmbii	%4,%4,-1\\n\\
	selb	%0,%8,%0,%6\\n\\
	lnop	\\n\\
	selb	%3,%7,%3,%6\\n\\
3:	brnz	%5,1b\\n\\
2:"
  [(set_attr "type" "multi0")
   (set_attr "length" "80")])

(define_insn_and_split "div<mode>3"
  [(set (match_operand:VSF 0 "spu_reg_operand" "=r")
	(div:VSF (match_operand:VSF 1 "spu_reg_operand" "r")
		 (match_operand:VSF 2 "spu_reg_operand" "r")))
   (clobber (match_scratch:VSF 3 "=&r"))
   (clobber (match_scratch:VSF 4 "=&r"))]
  ""
  "#"
  "reload_completed"
  [(set (match_dup:VSF 0)
	(div:VSF (match_dup:VSF 1)
		 (match_dup:VSF 2)))
   (clobber (match_dup:VSF 3))
   (clobber (match_dup:VSF 4))]
  {
    emit_insn(gen_frest_<mode>(operands[3], operands[2]));
    emit_insn(gen_fi_<mode>(operands[3], operands[2], operands[3]));
    emit_insn(gen_mul<mode>3(operands[4], operands[1], operands[3]));
    emit_insn(gen_fnms_<mode>(operands[0], operands[4], operands[2], operands[1]));
    emit_insn(gen_fma_<mode>(operands[0], operands[0], operands[3], operands[4]));
    DONE;
  })


;; sqrt

(define_insn_and_split "sqrtsf2"
  [(set (match_operand:SF 0 "spu_reg_operand" "=r")
	(sqrt:SF (match_operand:SF 1 "spu_reg_operand" "r")))
   (clobber (match_scratch:SF 2 "=&r"))
   (clobber (match_scratch:SF 3 "=&r"))
   (clobber (match_scratch:SF 4 "=&r"))
   (clobber (match_scratch:SF 5 "=&r"))]
  ""
  "#"
  "reload_completed"
  [(set (match_dup:SF 0)
	(sqrt:SF (match_dup:SF 1)))
   (clobber (match_dup:SF 2))
   (clobber (match_dup:SF 3))
   (clobber (match_dup:SF 4))
   (clobber (match_dup:SF 5))]
  {
    emit_move_insn (operands[3],spu_float_const(\"0.5\",SFmode));
    emit_move_insn (operands[4],spu_float_const(\"1.00000011920928955078125\",SFmode));
    emit_insn(gen_frsqest_sf(operands[2],operands[1]));
    emit_insn(gen_fi_sf(operands[2],operands[1],operands[2]));
    emit_insn(gen_mulsf3(operands[5],operands[2],operands[1]));
    emit_insn(gen_mulsf3(operands[3],operands[5],operands[3]));
    emit_insn(gen_fnms_sf(operands[4],operands[2],operands[5],operands[4]));
    emit_insn(gen_fma_sf(operands[0],operands[4],operands[3],operands[5]));
    DONE;
  })

(define_insn "frest_<mode>"
  [(set (match_operand:VSF 0 "spu_reg_operand" "=r")
	(unspec:VSF [(match_operand:VSF 1 "spu_reg_operand" "r")] UNSPEC_FREST))]
  ""
  "frest\t%0,%1"
  [(set_attr "type" "shuf")])

(define_insn "frsqest_<mode>"
  [(set (match_operand:VSF 0 "spu_reg_operand" "=r")
	(unspec:VSF [(match_operand:VSF 1 "spu_reg_operand" "r")] UNSPEC_FRSQEST))]
  ""
  "frsqest\t%0,%1"
  [(set_attr "type" "shuf")])

(define_insn "fi_<mode>"
  [(set (match_operand:VSF 0 "spu_reg_operand" "=r")
	(unspec:VSF [(match_operand:VSF 1 "spu_reg_operand" "r")
		    (match_operand:VSF 2 "spu_reg_operand" "r")] UNSPEC_FI))]
  ""
  "fi\t%0,%1,%2"
  [(set_attr "type" "fp7")])


;; and

(define_insn "and<mode>3"
  [(set (match_operand:MOV 0 "spu_reg_operand" "=r,r")
	(and:MOV (match_operand:MOV 1 "spu_reg_operand" "r,r")
		 (match_operand:MOV 2 "spu_logical_operand" "r,C")))]
  ""
  "@
  and\t%0,%1,%2
  and%j2i\t%0,%1,%J2")

(define_insn "anddi3"
  [(set (match_operand:DI 0 "spu_reg_operand" "=r,r")
	(and:DI (match_operand:DI 1 "spu_reg_operand" "r,r")
		(match_operand:DI 2 "spu_logical_operand" "r,c")))]
  ""
  "@
  and\t%0,%1,%2
  and%k2i\t%0,%1,%K2")

(define_insn "andti3"
  [(set (match_operand:TI 0 "spu_reg_operand" "=r,r")
	(and:TI (match_operand:TI 1 "spu_reg_operand" "r,r")
		(match_operand:TI 2 "spu_logical_operand" "r,Y")))]
  ""
  "@
  and\t%0,%1,%2
  and%m2i\t%0,%1,%L2")

(define_insn "andc_<mode>"
  [(set (match_operand:ALL 0 "spu_reg_operand" "=r")
	(and:ALL (not:ALL (match_operand:ALL 2 "spu_reg_operand" "r"))
	         (match_operand:ALL 1 "spu_reg_operand" "r")))]
  ""
  "andc\t%0,%1,%2")

(define_insn "nand_<mode>"
  [(set (match_operand:ALL 0 "spu_reg_operand" "=r")
	(not:ALL (and:ALL (match_operand:ALL 2 "spu_reg_operand" "r")
			  (match_operand:ALL 1 "spu_reg_operand" "r"))))]
  ""
  "nand\t%0,%1,%2")


;; ior

(define_insn "ior<mode>3"
  [(set (match_operand:MOV 0 "spu_reg_operand" "=r,r,r")
	(ior:MOV (match_operand:MOV 1 "spu_reg_operand" "r,r,0")
		 (match_operand:MOV 2 "spu_ior_operand" "r,C,D")))]
  ""
  "@
  or\t%0,%1,%2
  or%j2i\t%0,%1,%J2
  iohl\t%0,%J2")

(define_insn "iordi3"
  [(set (match_operand:DI 0 "spu_reg_operand" "=r,r,r")
	(ior:DI (match_operand:DI 1 "spu_reg_operand" "r,r,0")
		(match_operand:DI 2 "spu_ior_operand" "r,c,d")))]
  ""
  "@
  or\t%0,%1,%2
  or%k2i\t%0,%1,%K2
  iohl\t%0,%K2")

(define_insn "iorti3"
  [(set (match_operand:TI 0 "spu_reg_operand" "=r,r,r")
	(ior:TI (match_operand:TI 1 "spu_reg_operand" "r,r,0")
		(match_operand:TI 2 "spu_ior_operand" "r,Y,Z")))]
  ""
  "@
  or\t%0,%1,%2
  or%m2i\t%0,%1,%L2
  iohl\t%0,%L2")

(define_insn "orc_<mode>"
  [(set (match_operand:ALL 0 "spu_reg_operand" "=r")
	(ior:ALL (not:ALL (match_operand:ALL 2 "spu_reg_operand" "r"))
	         (match_operand:ALL 1 "spu_reg_operand" "r")))]
  ""
  "orc\t%0,%1,%2")

(define_insn "nor_<mode>"
  [(set (match_operand:ALL 0 "spu_reg_operand" "=r")
	(not:ALL (ior:ALL (match_operand:ALL 1 "spu_reg_operand" "r")
			  (match_operand:ALL 2 "spu_reg_operand" "r"))))]
  ""
  "nor\t%0,%1,%2")

;; xor

(define_insn "xor<mode>3"
  [(set (match_operand:MOV 0 "spu_reg_operand" "=r,r")
	(xor:MOV (match_operand:MOV 1 "spu_reg_operand" "r,r")
		 (match_operand:MOV 2 "spu_logical_operand" "r,B")))]
  ""
  "@
  xor\t%0,%1,%2
  xor%j2i\t%0,%1,%S2")

(define_insn "xordi3"
  [(set (match_operand:DI 0 "spu_reg_operand" "=r,r")
	(xor:DI (match_operand:DI 1 "spu_reg_operand" "r,r")
		(match_operand:DI 2 "spu_logical_operand" "r,c")))]
  ""
  "@
  xor\t%0,%1,%2
  xor%k2i\t%0,%1,%K2")

(define_insn "xorti3"
  [(set (match_operand:TI 0 "spu_reg_operand" "=r,r")
	(xor:TI (match_operand:TI 1 "spu_reg_operand" "r,r")
		(match_operand:TI 2 "spu_logical_operand" "r,Y")))]
  ""
  "@
  xor\t%0,%1,%2
  xor%m2i\t%0,%1,%L2")

(define_insn "eqv_<mode>"
  [(set (match_operand:ALL 0 "spu_reg_operand" "=r")
	(not:ALL (xor:ALL (match_operand:ALL 1 "spu_reg_operand" "r")
			  (match_operand:ALL 2 "spu_reg_operand" "r"))))]
  ""
  "eqv\t%0,%1,%2")

;; one_cmpl

(define_insn "one_cmpl<mode>2"
  [(set (match_operand:ALL 0 "spu_reg_operand" "=r")
	(not:ALL (match_operand:ALL 1 "spu_reg_operand" "r")))]
  ""
  "nor\t%0,%1,%1")


;; selb

(define_expand "selb"
  [(set (match_operand 0 "spu_reg_operand" "")
  	(unspec [(match_operand 1 "spu_reg_operand" "")
		 (match_operand 2 "spu_reg_operand" "")
		 (match_operand 3 "spu_reg_operand" "")] UNSPEC_SELB))]
  ""
  {
    rtx s = gen__selb (operands[0], operands[1], operands[2], operands[3]);
    PUT_MODE (SET_SRC (s), GET_MODE (operands[0]));
    emit_insn (s);
    DONE;
  })

;; This could be defined as a combination of logical operations, but at
;; one time it caused a crash due to recursive expansion of rtl during CSE.
(define_insn "_selb"
  [(set (match_operand 0 "spu_reg_operand" "=r")
  	(unspec [(match_operand 1 "spu_reg_operand" "r")
		 (match_operand 2 "spu_reg_operand" "r")
		 (match_operand 3 "spu_reg_operand" "r")] UNSPEC_SELB))]
  "GET_MODE(operands[0]) == GET_MODE(operands[1]) 
   && GET_MODE(operands[1]) == GET_MODE(operands[2])"
  "selb\t%0,%1,%2,%3")


;; Misc. byte/bit operations
;; clz/ctz/ffs/popcount/parity
;; cntb/sumb

(define_insn "clz<mode>2"
  [(set (match_operand:VSI 0 "spu_reg_operand" "=r")
	(clz:VSI (match_operand:VSI 1 "spu_reg_operand" "r")))]
  ""
  "clz\t%0,%1")

(define_expand "ctz<mode>2"
  [(set (match_dup 2)
	(neg:VSI (match_operand:VSI 1 "spu_reg_operand" "")))
   (set (match_dup 3) (and:VSI (match_dup 1)
			       (match_dup 2)))
   (set (match_dup 4) (clz:VSI (match_dup 3)))
   (set (match_operand:VSI 0 "spu_reg_operand" "")
	(minus:VSI (match_dup 5) (match_dup 4)))]
  ""
  {
     operands[2] = gen_reg_rtx (<MODE>mode);
     operands[3] = gen_reg_rtx (<MODE>mode);
     operands[4] = gen_reg_rtx (<MODE>mode);
     operands[5] = spu_const(<MODE>mode, 31);
  })

(define_expand "ffs<mode>2"
  [(set (match_dup 2)
	(neg:VSI (match_operand:VSI 1 "spu_reg_operand" "")))
   (set (match_dup 3) (and:VSI (match_dup 1)
			       (match_dup 2)))
   (set (match_dup 4) (clz:VSI (match_dup 3)))
   (set (match_operand:VSI 0 "spu_reg_operand" "")
	(minus:VSI (match_dup 5) (match_dup 4)))]
  ""
  {
     operands[2] = gen_reg_rtx (<MODE>mode);
     operands[3] = gen_reg_rtx (<MODE>mode);
     operands[4] = gen_reg_rtx (<MODE>mode);
     operands[5] = spu_const(<MODE>mode, 32);
  })

(define_expand "popcountsi2"
  [(set (match_dup 2)
	(unspec:SI [(match_operand:SI 1 "spu_reg_operand" "")]
		     UNSPEC_CNTB))
   (set (match_dup 3)
	(unspec:HI [(match_dup 2)] UNSPEC_SUMB))
   (set (match_operand:SI 0 "spu_reg_operand" "")
	(sign_extend:SI (match_dup 3)))]
  ""
  {
    operands[2] = gen_reg_rtx (SImode);
    operands[3] = gen_reg_rtx (HImode);
  })

(define_expand "paritysi2"
  [(set (match_operand:SI 0 "spu_reg_operand" "")
	(parity:SI (match_operand:SI 1 "spu_reg_operand" "")))]
  ""
  {
    operands[2] = gen_reg_rtx (SImode);
    emit_insn (gen_popcountsi2(operands[2], operands[1]));
    emit_insn (gen_andsi3(operands[0], operands[2], GEN_INT (1)));
    DONE;
  })

(define_insn "cntb_si"
  [(set (match_operand:SI 0 "spu_reg_operand" "=r")
        (unspec:SI [(match_operand:SI 1 "spu_reg_operand" "r")]
                   UNSPEC_CNTB))]
  ""
  "cntb\t%0,%1"
  [(set_attr "type" "fxb")])

(define_insn "cntb_v16qi"
  [(set (match_operand:V16QI 0 "spu_reg_operand" "=r")
        (unspec:V16QI [(match_operand:V16QI 1 "spu_reg_operand" "r")]
                      UNSPEC_CNTB))]
  ""
  "cntb\t%0,%1"
  [(set_attr "type" "fxb")])

(define_insn "sumb_si"
  [(set (match_operand:HI 0 "spu_reg_operand" "=r")
        (unspec:HI [(match_operand:SI 1 "spu_reg_operand" "r")] UNSPEC_SUMB))]
  ""
  "sumb\t%0,%1,%1"
  [(set_attr "type" "fxb")])


;; ashl

(define_insn "ashl<mode>3"
  [(set (match_operand:VHSI 0 "spu_reg_operand" "=r,r")
	(ashift:VHSI (match_operand:VHSI 1 "spu_reg_operand" "r,r")
		     (match_operand:VHSI 2 "spu_shift_operand" "r,W")))]
  ""
  "@
  shl<bh>\t%0,%1,%2
  shl<bh>i\t%0,%1,%2"
  [(set_attr "type" "fx3")])

(define_insn_and_split "ashldi3"
  [(set (match_operand:DI 0 "spu_reg_operand" "=r,r")
	(ashift:DI (match_operand:DI 1 "spu_reg_operand" "r,r")
	           (match_operand:SI 2 "spu_shift_operand" "r,I")))
   (clobber (match_scratch:SI 3 "=&r,X"))]
  ""
  "#"
  "reload_completed"
  [(set (match_dup:DI 0)
	(ashift:DI (match_dup:DI 1)
	           (match_dup:SI 2)))]
  {
    rtx op0 = gen_rtx_REG (TImode, REGNO (operands[0]));
    rtx op1 = gen_rtx_REG (TImode, REGNO (operands[1]));
    rtx op2 = operands[2];
    rtx op3 = operands[3];

    if (GET_CODE (operands[2]) == REG)
      {
	emit_insn (gen_addsi3 (op3, op2, GEN_INT (64)));
	emit_insn (gen_rotlti3 (op0, op1, GEN_INT (64)));
	emit_insn (gen_shlqbybi_ti (op0, op0, op3));
	emit_insn (gen_shlqbi_ti (op0, op0, op3));
      }
    else
      {
	HOST_WIDE_INT val = INTVAL (operands[2]);
	emit_insn (gen_rotlti3 (op0, op1, GEN_INT (64)));
	emit_insn (gen_shlqby_ti (op0, op0, GEN_INT (val / 8 + 8)));
	if (val % 8)
	  emit_insn (gen_shlqbi_ti (op0, op0, GEN_INT (val % 8)));
      }
    DONE;
  })

(define_expand "ashlti3"
  [(parallel [(set (match_operand:TI 0 "spu_reg_operand" "")
		   (ashift:TI (match_operand:TI 1 "spu_reg_operand" "")
			      (match_operand:SI 2 "spu_nonmem_operand" "")))
	      (clobber (match_dup:TI 3))])]
  ""
  "if (GET_CODE (operands[2]) == CONST_INT)
    {
      emit_insn(gen_ashlti3_imm(operands[0], operands[1], operands[2]));
      DONE;
    }
   operands[3] = gen_reg_rtx (TImode);")

(define_insn_and_split "ashlti3_imm"
  [(set (match_operand:TI 0 "spu_reg_operand" "=r,r")
	(ashift:TI (match_operand:TI 1 "spu_reg_operand" "r,r")
		   (match_operand:SI 2 "immediate_operand" "O,P")))]
  ""
  "@
   shlqbyi\t%0,%1,%2/8
   shlqbii\t%0,%1,%2"
  "!satisfies_constraint_O (operands[2]) && !satisfies_constraint_P (operands[2])"
  [(set (match_dup:TI 0)
	(ashift:TI (match_dup:TI 1)
		   (match_dup:SI 3)))
   (set (match_dup:TI 0)
	(ashift:TI (match_dup:TI 0)
		   (match_dup:SI 4)))]
  {
    HOST_WIDE_INT val = INTVAL(operands[2]);
    operands[3] = GEN_INT (val&7);
    operands[4] = GEN_INT (val&0x78);
  }
  [(set_attr "type" "shuf,shuf")])

(define_insn_and_split "ashlti3_reg"
  [(set (match_operand:TI 0 "spu_reg_operand" "=r")
	(ashift:TI (match_operand:TI 1 "spu_reg_operand" "r")
		   (match_operand:SI 2 "spu_reg_operand" "r")))
   (clobber (match_operand:TI 3 "spu_reg_operand" "=&r"))]
  ""
  "#"
  ""
  [(set (match_dup:TI 3)
	(ashift:TI (match_dup:TI 1)
		   (and:SI (match_dup:SI 2)
			   (const_int 7))))
   (set (match_dup:TI 0)
	(ashift:TI (match_dup:TI 3)
		   (and:SI (match_dup:SI 2)
			   (const_int -8))))]
  "")

(define_insn "shlqbybi_ti"
  [(set (match_operand:TI 0 "spu_reg_operand" "=r,r")
	(ashift:TI (match_operand:TI 1 "spu_reg_operand" "r,r")
		   (and:SI (match_operand:SI 2 "spu_nonmem_operand" "r,I")
			   (const_int -8))))]
  ""
  "@
   shlqbybi\t%0,%1,%2
   shlqbyi\t%0,%1,%2/8"
  [(set_attr "type" "shuf,shuf")])

(define_insn "shlqbi_ti"
  [(set (match_operand:TI 0 "spu_reg_operand" "=r,r")
	(ashift:TI (match_operand:TI 1 "spu_reg_operand" "r,r")
		   (and:SI (match_operand:SI 2 "spu_nonmem_operand" "r,I")
			   (const_int 7))))]
  ""
  "@
   shlqbi\t%0,%1,%2
   shlqbii\t%0,%1,%2"
  [(set_attr "type" "shuf,shuf")])

(define_insn "shlqby_ti"
  [(set (match_operand:TI 0 "spu_reg_operand" "=r,r")
	(ashift:TI (match_operand:TI 1 "spu_reg_operand" "r,r")
		   (mult:SI (match_operand:SI 2 "spu_nonmem_operand" "r,I")
			    (const_int 8))))]
  ""
  "@
   shlqby\t%0,%1,%2
   shlqbyi\t%0,%1,%2"
  [(set_attr "type" "shuf,shuf")])


;; lshr

(define_insn_and_split "lshr<mode>3"
  [(set (match_operand:VHSI 0 "spu_reg_operand" "=r,r")
	(lshiftrt:VHSI (match_operand:VHSI 1 "spu_reg_operand" "r,r")
		       (match_operand:VHSI 2 "spu_shift_operand" "r,W")))
   (clobber (match_scratch:VHSI 3 "=&r,X"))]
  ""
  "@
   #
   rot<bh>mi\t%0,%1,%N2"
  "reload_completed && GET_CODE (operands[2]) == REG"
  [(set (match_dup:VHSI 3)
	(neg:VHSI (match_dup:VHSI 2)))
   (set (match_dup:VHSI 0)
	(lshiftrt:VHSI (match_dup:VHSI 1)
		       (neg:VHSI (match_dup:VHSI 3))))]
  ""
  [(set_attr "type" "*,fx3")])
  

(define_insn "rotm_<mode>"
  [(set (match_operand:VHSI 0 "spu_reg_operand" "=r,r")
	(lshiftrt:VHSI (match_operand:VHSI 1 "spu_reg_operand" "r,r")
		       (neg:VHSI (match_operand:VHSI 2 "spu_shift_operand" "r,W"))))]
  ""
  "@
   rot<bh>m\t%0,%1,%2
   rot<bh>mi\t%0,%1,%2"
  [(set_attr "type" "fx3")])
 
(define_expand "lshr<mode>3"
  [(parallel [(set (match_operand:DTI 0 "spu_reg_operand" "")
		   (lshiftrt:DTI (match_operand:DTI 1 "spu_reg_operand" "")
			         (match_operand:SI 2 "spu_nonmem_operand" "")))
	      (clobber (match_dup:DTI 3))
	      (clobber (match_dup:SI 4))
	      (clobber (match_dup:SI 5))])]
  ""
  "if (GET_CODE (operands[2]) == CONST_INT)
    {
      emit_insn(gen_lshr<mode>3_imm(operands[0], operands[1], operands[2]));
      DONE;
    }
   operands[3] = gen_reg_rtx (<MODE>mode);
   operands[4] = gen_reg_rtx (SImode);
   operands[5] = gen_reg_rtx (SImode);")

(define_insn_and_split "lshr<mode>3_imm"
  [(set (match_operand:DTI 0 "spu_reg_operand" "=r,r")
	(lshiftrt:DTI (match_operand:DTI 1 "spu_reg_operand" "r,r")
		      (match_operand:SI 2 "immediate_operand" "O,P")))]
  ""
  "@
   rotqmbyi\t%0,%1,%N2/8
   rotqmbii\t%0,%1,%N2"
  "!satisfies_constraint_O (operands[2]) && !satisfies_constraint_P (operands[2])"
  [(set (match_dup:DTI 0)
	(lshiftrt:DTI (match_dup:DTI 1)
		      (match_dup:SI 4)))
   (set (match_dup:DTI 0)
	(lshiftrt:DTI (match_dup:DTI 0)
		      (match_dup:SI 5)))]
  {
    HOST_WIDE_INT val = INTVAL(operands[2]);
    operands[4] = GEN_INT (val&7);
    operands[5] = GEN_INT (val&0x78);
  }
  [(set_attr "type" "shuf,shuf")])

(define_insn_and_split "lshr<mode>3_reg"
  [(set (match_operand:DTI 0 "spu_reg_operand" "=r")
	(lshiftrt:DTI (match_operand:DTI 1 "spu_reg_operand" "r")
		      (match_operand:SI 2 "spu_reg_operand" "r")))
   (clobber (match_operand:DTI 3 "spu_reg_operand" "=&r"))
   (clobber (match_operand:SI 4 "spu_reg_operand" "=&r"))
   (clobber (match_operand:SI 5 "spu_reg_operand" "=&r"))]
  ""
  "#"
  ""
  [(set (match_dup:DTI 3)
	(lshiftrt:DTI (match_dup:DTI 1)
		     (and:SI (neg:SI (match_dup:SI 4))
			     (const_int 7))))
   (set (match_dup:DTI 0)
	(lshiftrt:DTI (match_dup:DTI 3)
		     (and:SI (neg:SI (match_dup:SI 5))
			     (const_int -8))))]
  {
    emit_insn(gen_subsi3(operands[4], GEN_INT(0), operands[2]));
    emit_insn(gen_subsi3(operands[5], GEN_INT(7), operands[2]));
  })

(define_insn "rotqmbybi_<mode>"
  [(set (match_operand:DTI 0 "spu_reg_operand" "=r,r")
	(lshiftrt:DTI (match_operand:DTI 1 "spu_reg_operand" "r,r")
		      (and:SI (neg:SI (match_operand:SI 2 "spu_nonmem_operand" "r,I"))
			      (const_int -8))))]
  ""
  "@
   rotqmbybi\t%0,%1,%2
   rotqmbyi\t%0,%1,%2/8"
  [(set_attr "type" "shuf")])

(define_insn "rotqmbi_<mode>"
  [(set (match_operand:DTI 0 "spu_reg_operand" "=r,r")
	(lshiftrt:DTI (match_operand:DTI 1 "spu_reg_operand" "r,r")
		      (and:SI (neg:SI (match_operand:SI 2 "spu_nonmem_operand" "r,I"))
			      (const_int 7))))]
  ""
  "@
   rotqmbi\t%0,%1,%2
   rotqmbii\t%0,%1,%2"
  [(set_attr "type" "shuf")])

(define_insn "rotqmby_<mode>"
  [(set (match_operand:DTI 0 "spu_reg_operand" "=r,r")
	(lshiftrt:DTI (match_operand:DTI 1 "spu_reg_operand" "r,r")
		      (mult:SI (neg:SI (match_operand:SI 2 "spu_nonmem_operand" "r,I"))
			       (const_int 8))))]
  ""
  "@
   rotqmby\t%0,%1,%2
   rotqmbyi\t%0,%1,%2"
  [(set_attr "type" "shuf")])


;; ashr

(define_insn_and_split "ashr<mode>3"
  [(set (match_operand:VHSI 0 "spu_reg_operand" "=r,r")
	(ashiftrt:VHSI (match_operand:VHSI 1 "spu_reg_operand" "r,r")
		       (match_operand:VHSI 2 "spu_shift_operand" "r,W")))
   (clobber (match_scratch:VHSI 3 "=&r,X"))]
  ""
  "@
   #
   rotma<bh>i\t%0,%1,%N2"
  "reload_completed && GET_CODE (operands[2]) == REG"
  [(set (match_dup:VHSI 3)
	(neg:VHSI (match_dup:VHSI 2)))
   (set (match_dup:VHSI 0)
	(ashiftrt:VHSI (match_dup:VHSI 1)
		       (neg:VHSI (match_dup:VHSI 3))))]
  ""
  [(set_attr "type" "*,fx3")])
  

(define_insn "rotma_<mode>"
  [(set (match_operand:VHSI 0 "spu_reg_operand" "=r,r")
	(ashiftrt:VHSI (match_operand:VHSI 1 "spu_reg_operand" "r,r")
		       (neg:VHSI (match_operand:VHSI 2 "spu_shift_operand" "r,W"))))]
  ""
  "@
   rotma<bh>\t%0,%1,%2
   rotma<bh>i\t%0,%1,%2"
  [(set_attr "type" "fx3")])
 
(define_insn_and_split "ashrdi3"
  [(set (match_operand:DI 0 "spu_reg_operand" "=r,r")
        (ashiftrt:DI (match_operand:DI 1 "spu_reg_operand" "r,r")
                     (match_operand:SI 2 "spu_nonmem_operand" "r,I")))
   (clobber (match_scratch:TI 3 "=&r,&r"))
   (clobber (match_scratch:TI 4 "=&r,&r"))
   (clobber (match_scratch:SI 5 "=&r,&r"))]
  ""
  "#"
  "reload_completed"
  [(set (match_dup:DI 0)
        (ashiftrt:DI (match_dup:DI 1)
                     (match_dup:SI 2)))]
  {
    rtx op0 = gen_rtx_REG (TImode, REGNO (operands[0]));
    rtx op0v = gen_rtx_REG (V4SImode, REGNO (op0));
    rtx op1 = gen_rtx_REG (TImode, REGNO (operands[1]));
    rtx op1s = gen_rtx_REG (SImode, REGNO (op1));
    rtx op2 = operands[2];
    rtx op3 = operands[3];
    rtx op4 = operands[4];
    rtx op5 = operands[5];

    if (GET_CODE (op2) == CONST_INT && INTVAL (op2) >= 63)
      {
	rtx op0s = gen_rtx_REG (SImode, REGNO (op0));
	emit_insn (gen_ashrsi3 (op0s, op1s, GEN_INT (32)));
	emit_insn (gen_spu_fsm (op0v, op0s));
      }
    else if (GET_CODE (op2) == CONST_INT && INTVAL (op2) >= 32)
      {
	rtx op0d = gen_rtx_REG (V2DImode, REGNO (op0));
	HOST_WIDE_INT val = INTVAL (op2);
	emit_insn (gen_lshrti3 (op0, op1, GEN_INT (32)));
	emit_insn (gen_spu_xswd (op0d, op0v));
        if (val > 32)
	  emit_insn (gen_ashrv4si3 (op0v, op0v, spu_const (V4SImode, val - 32)));
      }
    else
      {
	rtx op3v = gen_rtx_REG (V4SImode, REGNO (op3));
	unsigned char arr[16] = {
	  0xff, 0xff, 0xff, 0xff,
	  0xff, 0xff, 0xff, 0xff,
	  0x00, 0x00, 0x00, 0x00,
	  0x00, 0x00, 0x00, 0x00
	};

	emit_insn (gen_ashrsi3 (op5, op1s, GEN_INT (31)));
	emit_move_insn (op4, array_to_constant (TImode, arr));
	emit_insn (gen_spu_fsm (op3v, op5));

	if (GET_CODE (operands[2]) == REG)
	  {
	    emit_insn (gen_selb (op4, op3, op1, op4));
	    emit_insn (gen_negsi2 (op5, op2));
	    emit_insn (gen_rotqbybi_ti (op0, op4, op5));
	    emit_insn (gen_rotqbi_ti (op0, op0, op5));
	  }
	else
	  {
	    HOST_WIDE_INT val = -INTVAL (op2);
	    emit_insn (gen_selb (op0, op3, op1, op4));
	    if ((val - 7) / 8)
	      emit_insn (gen_rotqby_ti (op0, op0, GEN_INT ((val - 7) / 8)));
	    if (val % 8)
	      emit_insn (gen_rotqbi_ti (op0, op0, GEN_INT (val % 8)));
	  }
      }
    DONE;
  })


(define_expand "ashrti3"
  [(set (match_operand:TI 0 "spu_reg_operand" "")
	(ashiftrt:TI (match_operand:TI 1 "spu_reg_operand" "")
		     (match_operand:SI 2 "spu_nonmem_operand" "")))]
  ""
  {
    rtx sign_shift = gen_reg_rtx (SImode);
    rtx sign_mask = gen_reg_rtx (TImode);
    rtx sign_mask_v4si = gen_rtx_SUBREG (V4SImode, sign_mask, 0);
    rtx op1_v4si = spu_gen_subreg (V4SImode, operands[1]);
    rtx t = gen_reg_rtx (TImode);
    emit_insn (gen_subsi3 (sign_shift, GEN_INT (128), force_reg (SImode, operands[2])));
    emit_insn (gen_ashrv4si3 (sign_mask_v4si, op1_v4si, spu_const (V4SImode, 31)));
    emit_insn (gen_fsm_ti (sign_mask, sign_mask));
    emit_insn (gen_ashlti3 (sign_mask, sign_mask, sign_shift));
    emit_insn (gen_lshrti3 (t, operands[1], operands[2]));
    emit_insn (gen_iorti3 (operands[0], t, sign_mask));
    DONE;
  })

;; fsm is used after rotam to replicate the sign across the whole register.
(define_insn "fsm_ti"
  [(set (match_operand:TI 0 "spu_reg_operand" "=r")
	(unspec:TI [(match_operand:TI 1 "spu_reg_operand" "r")] UNSPEC_FSM))]
  ""
  "fsm\t%0,%1"
  [(set_attr "type" "shuf")])


;; rotl

(define_insn "rotl<mode>3"
  [(set (match_operand:VHSI 0 "spu_reg_operand" "=r,r")
	(rotate:VHSI (match_operand:VHSI 1 "spu_reg_operand" "r,r")
		     (match_operand:VHSI 2 "spu_shift_operand" "r,W")))]
  ""
  "@
  rot<bh>\t%0,%1,%2
  rot<bh>i\t%0,%1,%2"
  [(set_attr "type" "fx3")])

(define_insn "rotlti3"
  [(set (match_operand:TI 0 "spu_reg_operand" "=&r,r,r,r")
	(rotate:TI (match_operand:TI 1 "spu_reg_operand" "r,r,r,r")
		   (match_operand:SI 2 "spu_nonmem_operand" "r,O,P,I")))]
  ""
  "@
  rotqbybi\t%0,%1,%2\;rotqbi\t%0,%0,%2
  rotqbyi\t%0,%1,%2/8
  rotqbii\t%0,%1,%2
  rotqbyi\t%0,%1,%2/8\;rotqbii\t%0,%0,%2%%8"
  [(set_attr "length" "8,4,4,8")
   (set_attr "type" "multi1,shuf,shuf,multi1")])

(define_insn "rotqbybi_ti"
  [(set (match_operand:TI 0 "spu_reg_operand" "=r,r")
	(rotate:TI (match_operand:TI 1 "spu_reg_operand" "r,r")
		   (and:SI (match_operand:SI 2 "spu_nonmem_operand" "r,I")
			   (const_int -8))))]
  ""
  "@
  rotqbybi\t%0,%1,%2
  rotqbyi\t%0,%1,%2/8"
  [(set_attr "type" "shuf,shuf")])

(define_insn "rotqby_ti"
  [(set (match_operand:TI 0 "spu_reg_operand" "=r,r")
	(rotate:TI (match_operand:TI 1 "spu_reg_operand" "r,r")
		   (mult:SI (match_operand:SI 2 "spu_nonmem_operand" "r,I")
			    (const_int 8))))]
  ""
  "@
  rotqby\t%0,%1,%2
  rotqbyi\t%0,%1,%2"
  [(set_attr "type" "shuf,shuf")])

(define_insn "rotqbi_ti"
  [(set (match_operand:TI 0 "spu_reg_operand" "=r,r")
	(rotate:TI (match_operand:TI 1 "spu_reg_operand" "r,r")
		   (and:SI (match_operand:SI 2 "spu_nonmem_operand" "r,I")
			   (const_int 7))))]
  ""
  "@
  rotqbi\t%0,%1,%2
  rotqbii\t%0,%1,%2%%8"
  [(set_attr "type" "shuf,shuf")])


;; struct extract/insert
;; We have to handle mem's because GCC will generate invalid SUBREG's
;; if it handles them.  We generate better code anyway.

(define_expand "extv"
  [(set (match_operand 0 "register_operand" "")
	(sign_extract (match_operand 1 "register_operand" "")
		      (match_operand:SI 2 "const_int_operand" "")
		      (match_operand:SI 3 "const_int_operand" "")))]
  ""
  { spu_expand_extv(operands, 0); DONE; })

(define_expand "extzv"
  [(set (match_operand 0 "register_operand" "")
	(zero_extract (match_operand 1 "register_operand" "")
			 (match_operand:SI 2 "const_int_operand" "")
			 (match_operand:SI 3 "const_int_operand" "")))]
  ""
  { spu_expand_extv(operands, 1); DONE; })

(define_expand "insv"
  [(set (zero_extract (match_operand 0 "register_operand" "")
		      (match_operand:SI 1 "const_int_operand" "")
		      (match_operand:SI 2 "const_int_operand" ""))
	(match_operand 3 "nonmemory_operand" ""))]
  ""
  { spu_expand_insv(operands); DONE; })


;; String/block move insn.
;; Argument 0 is the destination
;; Argument 1 is the source
;; Argument 2 is the length
;; Argument 3 is the alignment

(define_expand "movstrsi"
  [(parallel [(set (match_operand:BLK 0 "" "")
		   (match_operand:BLK 1 "" ""))
	      (use (match_operand:SI 2 "" ""))
	      (use (match_operand:SI 3 "" ""))])]
  ""
  "
  {
    if (spu_expand_block_move (operands))
      DONE;
    else
      FAIL;
  }")


;; jump

(define_insn "indirect_jump"
  [(set (pc) (match_operand:SI 0 "spu_reg_operand" "r"))]
  ""
  "bi\t%0"
  [(set_attr "type" "br")])

(define_insn "jump"
  [(set (pc)
	(label_ref (match_operand 0 "" "")))]
  ""
  "br\t%0"
  [(set_attr "type" "br")])


;; return

;; This will be used for leaf functions, that don't save any regs and
;; don't have locals on stack, maybe... that is for functions that
;; don't change $sp and don't need to save $lr. 
(define_expand "return"
    [(return)]
  "direct_return()"
  "")

;; used in spu_expand_epilogue to generate return from a function and
;; explicitly set use of $lr.

(define_insn "_return"
  [(return)]
  ""
  "bi\t$lr"
  [(set_attr "type" "br")])



;; ceq

(define_insn "ceq_<mode>"
  [(set (match_operand:VQHSI 0 "spu_reg_operand" "=r,r")
	(eq:VQHSI (match_operand:VQHSI 1 "spu_reg_operand" "r,r")
	         (match_operand:VQHSI 2 "spu_arith_operand" "r,B")))]
  ""
  "@
  ceq<bh>\t%0,%1,%2
  ceq<bh>i\t%0,%1,%2")

(define_insn_and_split "ceq_di"
  [(set (match_operand:SI 0 "spu_reg_operand" "=r")
        (eq:SI (match_operand:DI 1 "spu_reg_operand" "r")
	       (match_operand:DI 2 "spu_reg_operand" "r")))]
  ""
  "#"
  "reload_completed"
  [(set (match_dup:SI 0)
        (eq:SI (match_dup:DI 1)
	       (match_dup:DI 2)))]
  {
    rtx op0 = gen_rtx_REG (V4SImode, REGNO (operands[0]));
    rtx op1 = gen_rtx_REG (V4SImode, REGNO (operands[1]));
    rtx op2 = gen_rtx_REG (V4SImode, REGNO (operands[2]));
    emit_insn (gen_ceq_v4si (op0, op1, op2));
    emit_insn (gen_spu_gb (op0, op0));
    emit_insn (gen_cgt_si (operands[0], operands[0], GEN_INT (11)));
    DONE;
  })


;; We provide the TI compares for completeness and because some parts of
;; gcc/libgcc use them, even though user code might never see it.
(define_insn "ceq_ti"
  [(set (match_operand:SI 0 "spu_reg_operand" "=r")
	(eq:SI (match_operand:TI 1 "spu_reg_operand" "r")
	       (match_operand:TI 2 "spu_reg_operand" "r")))]
  ""
  "ceq\t%0,%1,%2\;gb\t%0,%0\;ceqi\t%0,%0,15"
  [(set_attr "type" "multi0")
   (set_attr "length" "12")])

(define_insn "ceq_<mode>"
  [(set (match_operand:<f2i> 0 "spu_reg_operand" "=r")
	(eq:<f2i> (match_operand:VSF 1 "spu_reg_operand" "r")
		  (match_operand:VSF 2 "spu_reg_operand" "r")))]
  ""
  "fceq\t%0,%1,%2")

(define_insn "cmeq_<mode>"
  [(set (match_operand:<f2i> 0 "spu_reg_operand" "=r")
	(eq:<f2i> (abs:VSF (match_operand:VSF 1 "spu_reg_operand" "r"))
	          (abs:VSF (match_operand:VSF 2 "spu_reg_operand" "r"))))]
  ""
  "fcmeq\t%0,%1,%2")

(define_insn "ceq_vec"
  [(set (match_operand:SI 0 "spu_reg_operand" "=r")
	(eq:SI (match_operand 1 "spu_reg_operand" "r")
	       (match_operand 2 "spu_reg_operand" "r")))]
  "VECTOR_MODE_P(GET_MODE(operands[1]))
   && GET_MODE(operands[1]) == GET_MODE(operands[2])"
  "ceq\t%0,%1,%2\;gb\t%0,%0\;ceqi\t%0,%0,15"
  [(set_attr "length" "12")])


;; cgt

(define_insn "cgt_<mode>"
  [(set (match_operand:VQHSI 0 "spu_reg_operand" "=r,r")
	(gt:VQHSI (match_operand:VQHSI 1 "spu_reg_operand" "r,r")
	          (match_operand:VQHSI 2 "spu_arith_operand" "r,B")))]
  ""
  "@
  cgt<bh>\t%0,%1,%2
  cgt<bh>i\t%0,%1,%2")

(define_insn "cgt_di_m1" 
  [(set (match_operand:SI 0 "spu_reg_operand" "=r")
	(gt:SI (match_operand:DI 1 "spu_reg_operand" "r")
	       (const_int -1)))]
  ""
  "cgti\t%0,%1,-1")

(define_insn_and_split "cgt_di" 
  [(set (match_operand:SI 0 "spu_reg_operand" "=r")
	(gt:SI (match_operand:DI 1 "spu_reg_operand" "r")
	       (match_operand:DI 2 "spu_reg_operand" "r")))
   (clobber (match_scratch:V4SI 3 "=&r"))
   (clobber (match_scratch:V4SI 4 "=&r"))
   (clobber (match_scratch:V4SI 5 "=&r"))]
  ""
  "#"
  "reload_completed"
  [(set (match_dup:SI 0)
        (gt:SI (match_dup:DI 1)
	       (match_dup:DI 2)))]
  {
    rtx op0 = gen_rtx_REG (V4SImode, REGNO (operands[0]));
    rtx op1 = gen_rtx_REG (V4SImode, REGNO (operands[1]));
    rtx op2 = gen_rtx_REG (V4SImode, REGNO (operands[2]));
    rtx op3 = operands[3];
    rtx op4 = operands[4];
    rtx op5 = operands[5];
    rtx op3d = gen_rtx_REG (V2DImode, REGNO (operands[3]));
    emit_insn (gen_clgt_v4si (op3, op1, op2));
    emit_insn (gen_ceq_v4si (op4, op1, op2));
    emit_insn (gen_cgt_v4si (op5, op1, op2));
    emit_insn (gen_spu_xswd (op3d, op3));
    emit_insn (gen_selb (op0, op5, op3, op4));
    DONE;
  })

(define_insn "cgt_ti"
  [(set (match_operand:SI 0 "spu_reg_operand" "=r")
	(gt:SI (match_operand:TI 1 "spu_reg_operand" "r")
	       (match_operand:TI 2 "spu_reg_operand" "r")))
   (clobber (match_scratch:V4SI 3 "=&r"))
   (clobber (match_scratch:V4SI 4 "=&r"))
   (clobber (match_scratch:V4SI 5 "=&r"))]
  ""
  "clgt\t%4,%1,%2\;\
ceq\t%3,%1,%2\;\
cgt\t%5,%1,%2\;\
shlqbyi\t%0,%4,4\;\
selb\t%0,%4,%0,%3\;\
shlqbyi\t%0,%0,4\;\
selb\t%0,%4,%0,%3\;\
shlqbyi\t%0,%0,4\;\
selb\t%0,%5,%0,%3"
  [(set_attr "type" "multi0")
   (set_attr "length" "36")])

(define_insn "cgt_<mode>"
  [(set (match_operand:<f2i> 0 "spu_reg_operand" "=r")
	(gt:<f2i> (match_operand:VSF 1 "spu_reg_operand" "r")
		  (match_operand:VSF 2 "spu_reg_operand" "r")))]
  ""
  "fcgt\t%0,%1,%2")

(define_insn "cmgt_<mode>"
  [(set (match_operand:<f2i> 0 "spu_reg_operand" "=r")
	(gt:<f2i> (abs:VSF (match_operand:VSF 1 "spu_reg_operand" "r"))
		  (abs:VSF (match_operand:VSF 2 "spu_reg_operand" "r"))))]
  ""
  "fcmgt\t%0,%1,%2")


;; clgt

(define_insn "clgt_<mode>"
  [(set (match_operand:VQHSI 0 "spu_reg_operand" "=r,r")
	(gtu:VQHSI (match_operand:VQHSI 1 "spu_reg_operand" "r,r")
		   (match_operand:VQHSI 2 "spu_arith_operand" "r,B")))]
  ""
  "@
  clgt<bh>\t%0,%1,%2
  clgt<bh>i\t%0,%1,%2")

(define_insn_and_split "clgt_di" 
  [(set (match_operand:SI 0 "spu_reg_operand" "=r")
	(gtu:SI (match_operand:DI 1 "spu_reg_operand" "r")
	        (match_operand:DI 2 "spu_reg_operand" "r")))
   (clobber (match_scratch:V4SI 3 "=&r"))
   (clobber (match_scratch:V4SI 4 "=&r"))
   (clobber (match_scratch:V4SI 5 "=&r"))]
  ""
  "#"
  "reload_completed"
  [(set (match_dup:SI 0)
        (gtu:SI (match_dup:DI 1)
	        (match_dup:DI 2)))]
  {
    rtx op0 = gen_rtx_REG (V4SImode, REGNO (operands[0]));
    rtx op1 = gen_rtx_REG (V4SImode, REGNO (operands[1]));
    rtx op2 = gen_rtx_REG (V4SImode, REGNO (operands[2]));
    rtx op3 = operands[3];
    rtx op4 = operands[4];
    rtx op5 = operands[5];
    rtx op5d = gen_rtx_REG (V2DImode, REGNO (operands[5]));
    emit_insn (gen_clgt_v4si (op3, op1, op2));
    emit_insn (gen_ceq_v4si (op4, op1, op2));
    emit_insn (gen_spu_xswd (op5d, op3));
    emit_insn (gen_selb (op0, op3, op5, op4));
    DONE;
  })

(define_insn "clgt_ti"
  [(set (match_operand:SI 0 "spu_reg_operand" "=r")
	(gtu:SI (match_operand:TI 1 "spu_reg_operand" "r")
	       (match_operand:TI 2 "spu_reg_operand" "r")))
   (clobber (match_scratch:V4SI 3 "=&r"))
   (clobber (match_scratch:V4SI 4 "=&r"))]
  ""
  "ceq\t%3,%1,%2\;\
clgt\t%4,%1,%2\;\
shlqbyi\t%0,%4,4\;\
selb\t%0,%4,%0,%3\;\
shlqbyi\t%0,%0,4\;\
selb\t%0,%4,%0,%3\;\
shlqbyi\t%0,%0,4\;\
selb\t%0,%4,%0,%3"
  [(set_attr "type" "multi0")
   (set_attr "length" "32")])


;; branches

(define_insn ""
  [(set (pc)
	(if_then_else (match_operator 1 "branch_comparison_operator"
				      [(match_operand 2
						      "spu_reg_operand" "r")
				       (const_int 0)])
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "br%b2%b1z\t%2,%0"
  [(set_attr "type" "br")])

(define_insn ""
  [(set (pc)
	(if_then_else (match_operator 0 "branch_comparison_operator"
				      [(match_operand 1
						      "spu_reg_operand" "r")
				       (const_int 0)])
		      (return)
		      (pc)))]
  "direct_return ()"
  "bi%b1%b0z\t%1,$lr"
  [(set_attr "type" "br")])

(define_insn ""
  [(set (pc)
	(if_then_else (match_operator 1 "branch_comparison_operator"
				      [(match_operand 2
						      "spu_reg_operand" "r")
				       (const_int 0)])
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "br%b2%b1z\t%2,%0"
  [(set_attr "type" "br")])

(define_insn ""
  [(set (pc)
	(if_then_else (match_operator 0 "branch_comparison_operator"
				      [(match_operand 1
						      "spu_reg_operand" "r")
				       (const_int 0)])
		      (pc)
		      (return)))]
  "direct_return ()"
  "bi%b1%b0z\t%1,$lr"
  [(set_attr "type" "br")])


;; Compare insns are next.  Note that the spu has two types of compares,
;; signed & unsigned, and one type of branch.
;;
;; Start with the DEFINE_EXPANDs to generate the rtl for compares, scc
;; insns, and branches.  We store the operands of compares until we see
;; how it is used.

(define_expand "cmp<mode>"
  [(set (cc0)
	(compare (match_operand:VQHSI 0 "spu_reg_operand" "")
  		 (match_operand:VQHSI 1 "spu_nonmem_operand" "")))]
  ""
  {
    spu_compare_op0 = operands[0];
    spu_compare_op1 = operands[1];
    DONE;
  })

(define_expand "cmp<mode>"
  [(set (cc0)
	(compare (match_operand:DTI 0 "spu_reg_operand" "")
  		 (match_operand:DTI 1 "spu_reg_operand" "")))]
  ""
  {
    spu_compare_op0 = operands[0];
    spu_compare_op1 = operands[1];
    DONE;
  })

(define_expand "cmp<mode>"
  [(set (cc0)
	(compare (match_operand:VSF 0 "spu_reg_operand" "")
  		 (match_operand:VSF 1 "spu_reg_operand" "")))]
  ""
  {
    spu_compare_op0 = operands[0];
    spu_compare_op1 = operands[1];
    DONE;
  })


;; branch on condition

(define_expand "beq"
  [(use (match_operand 0 "" ""))]
  ""
  { spu_emit_branch_or_set (0, EQ, operands); DONE; })

(define_expand "bne"
  [(use (match_operand 0 "" ""))]
  ""
  { spu_emit_branch_or_set (0, NE, operands); DONE; })

(define_expand "bge"
  [(use (match_operand 0 "" ""))]
  ""
  { spu_emit_branch_or_set (0, GE, operands); DONE; })

(define_expand "bgt"
  [(use (match_operand 0 "" ""))]
  ""
  { spu_emit_branch_or_set (0, GT, operands); DONE; })

(define_expand "ble"
  [(use (match_operand 0 "" ""))]
  ""
  { spu_emit_branch_or_set (0, LE, operands); DONE; })

(define_expand "blt"
  [(use (match_operand 0 "" ""))]
  ""
  { spu_emit_branch_or_set (0, LT, operands); DONE; })

(define_expand "bgeu"
  [(use (match_operand 0 "" ""))]
  ""
  { spu_emit_branch_or_set (0, GEU, operands); DONE; })

(define_expand "bgtu"
  [(use (match_operand 0 "" ""))]
  ""
  { spu_emit_branch_or_set (0, GTU, operands); DONE; })

(define_expand "bleu"
  [(use (match_operand 0 "" ""))]
  ""
  { spu_emit_branch_or_set (0, LEU, operands); DONE; })

(define_expand "bltu"
  [(use (match_operand 0 "" ""))]
  ""
  { spu_emit_branch_or_set (0, LTU, operands); DONE; })


;; set on condition

(define_expand "seq"
  [(clobber (match_operand:SI 0 "spu_reg_operand" ""))]
  ""
  { spu_emit_branch_or_set (1, EQ, operands); DONE; })

(define_expand "sne"
  [(clobber (match_operand:SI 0 "spu_reg_operand" ""))]
  ""
  { spu_emit_branch_or_set (1, NE, operands); DONE; })

(define_expand "sgt"
  [(clobber (match_operand:SI 0 "spu_reg_operand" ""))]
  ""
  { spu_emit_branch_or_set (1, GT, operands); DONE; })

(define_expand "slt"
  [(clobber (match_operand:SI 0 "spu_reg_operand" ""))]
  ""
  { spu_emit_branch_or_set (1, LT, operands); DONE; })

(define_expand "sge"
  [(clobber (match_operand:SI 0 "spu_reg_operand" ""))]
  ""
  { spu_emit_branch_or_set (1, GE, operands); DONE; })

(define_expand "sle"
  [(clobber (match_operand:SI 0 "spu_reg_operand" ""))]
  ""
  { spu_emit_branch_or_set (1, LE, operands); DONE; })

(define_expand "sgtu"
  [(clobber (match_operand:SI 0 "spu_reg_operand" ""))]
  ""
  { spu_emit_branch_or_set (1, GTU, operands); DONE; })

(define_expand "sltu"
  [(clobber (match_operand:SI 0 "spu_reg_operand" ""))]
  ""
  { spu_emit_branch_or_set (1, LTU, operands); DONE; })

(define_expand "sgeu"
  [(clobber (match_operand:SI 0 "spu_reg_operand" ""))]
  ""
  { spu_emit_branch_or_set (1, GEU, operands); DONE; })

(define_expand "sleu"
  [(clobber (match_operand:SI 0 "spu_reg_operand" ""))]
  ""
  { spu_emit_branch_or_set (1, LEU, operands); DONE; })


;; conditional move

;; Define this first one so HAVE_conditional_move is defined.
(define_insn "movcc_dummy"
  [(set (match_operand 0 "" "")
       (if_then_else (match_operand 1 "" "")
		     (match_operand 2 "" "")
		     (match_operand 3 "" "")))]
  "!operands[0]"
  "")

(define_expand "mov<mode>cc"
  [(set (match_operand:ALL 0 "spu_reg_operand" "")
	(if_then_else:ALL (match_operand 1 "comparison_operator" "")
		      (match_operand:ALL 2 "spu_reg_operand" "")
		      (match_operand:ALL 3 "spu_reg_operand" "")))]
  ""
  {
    spu_emit_branch_or_set(2, GET_CODE(operands[1]), operands);
    DONE;
  })

;; This pattern is used when the result of a compare is not large
;; enough to use in a selb when expanding conditional moves.
(define_expand "extend_compare"
  [(set (match_operand 0 "spu_reg_operand" "=r")
	(unspec [(match_operand 1 "spu_reg_operand" "r")] UNSPEC_EXTEND_CMP))]
  ""
  {
    emit_insn (gen_rtx_SET (VOIDmode, operands[0],
			    gen_rtx_UNSPEC (GET_MODE (operands[0]),
			                    gen_rtvec (1, operands[1]),
					    UNSPEC_EXTEND_CMP)));
    DONE;
  })

(define_insn "extend_compare<mode>"
  [(set (match_operand:ALL 0 "spu_reg_operand" "=r")
	(unspec:ALL [(match_operand 1 "spu_reg_operand" "r")] UNSPEC_EXTEND_CMP))]
  "operands"
  "fsm\t%0,%1"
  [(set_attr "type" "shuf")])


;; case

;; operand 0 is index
;; operand 1 is the minimum bound
;; operand 2 is the maximum bound - minimum bound + 1
;; operand 3 is CODE_LABEL for the table;
;; operand 4 is the CODE_LABEL to go to if index out of range.
(define_expand "casesi"
  [(match_operand:SI 0 "spu_reg_operand" "")
   (match_operand:SI 1 "immediate_operand" "")
   (match_operand:SI 2 "immediate_operand" "")
   (match_operand 3 "" "")
   (match_operand 4 "" "")]
  ""
  {
    rtx table = gen_reg_rtx (SImode);
    rtx index = gen_reg_rtx (SImode);
    rtx sindex = gen_reg_rtx (SImode);
    rtx addr = gen_reg_rtx (Pmode);

    emit_move_insn (table, gen_rtx_LABEL_REF (SImode, operands[3]));

    emit_insn (gen_subsi3(index, operands[0], force_reg(SImode, operands[1])));
    emit_insn (gen_ashlsi3(sindex, index, GEN_INT (2)));
    emit_move_insn (addr, gen_rtx_MEM (SImode,
				       gen_rtx_PLUS (SImode, table, sindex)));
    if (flag_pic)
      emit_insn (gen_addsi3 (addr, addr, table));

    emit_cmp_and_jump_insns (index, operands[2], GTU, NULL_RTX, SImode, 1, operands[4]);
    emit_jump_insn (gen_tablejump (addr, operands[3]));
    DONE;
  })

(define_insn "tablejump"
  [(set (pc) (match_operand:SI 0 "spu_reg_operand" "r"))
   (use (label_ref (match_operand 1 "" "")))]
  ""
  "bi\t%0"
  [(set_attr "type" "br")])


;; call

;; Note that operand 1 is total size of args, in bytes,
;; and what the call insn wants is the number of words.
(define_expand "sibcall"
  [(parallel
    [(call (match_operand:QI 0 "call_operand" "")
	   (match_operand:QI 1 "" ""))
     (use (reg:SI 0))])]
  ""
  {
    if (! call_operand (operands[0], QImode))
      XEXP (operands[0], 0) = copy_to_mode_reg (Pmode, XEXP (operands[0], 0));
  })

(define_insn "_sibcall"
  [(parallel
    [(call (match_operand:QI 0 "call_operand" "R,S")
	   (match_operand:QI 1 "" "i,i"))
     (use (reg:SI 0))])]
  "SIBLING_CALL_P(insn)"
  "@
   bi\t%i0
   br\t%0"
   [(set_attr "type" "br,br")])

(define_expand "sibcall_value"
  [(parallel
    [(set (match_operand 0 "" "")
	  (call (match_operand:QI 1 "call_operand" "")
		(match_operand:QI 2 "" "")))
     (use (reg:SI 0))])]
  ""
  {
    if (! call_operand (operands[1], QImode))
      XEXP (operands[1], 0) = copy_to_mode_reg (Pmode, XEXP (operands[1], 0));
  })

(define_insn "_sibcall_value"
  [(parallel
    [(set (match_operand 0 "" "")
	  (call (match_operand:QI 1 "call_operand" "R,S")
		(match_operand:QI 2 "" "i,i")))
     (use (reg:SI 0))])]
  "SIBLING_CALL_P(insn)"
  "@
   bi\t%i1
   br\t%1"
   [(set_attr "type" "br,br")])

;; Note that operand 1 is total size of args, in bytes,
;; and what the call insn wants is the number of words.
(define_expand "call"
  [(parallel
    [(call (match_operand:QI 0 "call_operand" "")
	   (match_operand:QI 1 "" ""))
     (clobber (reg:SI 0))
     (clobber (reg:SI 130))])]
  ""
  {
    if (! call_operand (operands[0], QImode))
      XEXP (operands[0], 0) = copy_to_mode_reg (Pmode, XEXP (operands[0], 0));
  })

(define_insn "_call"
  [(parallel
    [(call (match_operand:QI 0 "call_operand" "R,S,T")
	   (match_operand:QI 1 "" "i,i,i"))
     (clobber (reg:SI 0))
     (clobber (reg:SI 130))])]
  ""
  "@
   bisl\t$lr,%i0
   brsl\t$lr,%0
   brasl\t$lr,%0"
   [(set_attr "type" "br")])

(define_expand "call_value"
  [(parallel
    [(set (match_operand 0 "" "")
	  (call (match_operand:QI 1 "call_operand" "")
		(match_operand:QI 2 "" "")))
     (clobber (reg:SI 0))
     (clobber (reg:SI 130))])]
  ""
  {
    if (! call_operand (operands[1], QImode))
      XEXP (operands[1], 0) = copy_to_mode_reg (Pmode, XEXP (operands[1], 0));
  })

(define_insn "_call_value"
  [(parallel
    [(set (match_operand 0 "" "")
	  (call (match_operand:QI 1 "call_operand" "R,S,T")
		(match_operand:QI 2 "" "i,i,i")))
     (clobber (reg:SI 0))
     (clobber (reg:SI 130))])]
  ""
  "@
   bisl\t$lr,%i1
   brsl\t$lr,%1
   brasl\t$lr,%1"
   [(set_attr "type" "br")])

(define_expand "untyped_call"
  [(parallel [(call (match_operand 0 "" "")
		    (const_int 0))
	      (match_operand 1 "" "")
	      (match_operand 2 "" "")])]
  ""
  {
    int i;
    rtx reg = gen_rtx_REG (TImode, 3);

    /* We need to use call_value so the return value registers don't get
     * clobbered. */
    emit_call_insn (gen_call_value (reg, operands[0], const0_rtx));

    for (i = 0; i < XVECLEN (operands[2], 0); i++)
      {
	rtx set = XVECEXP (operands[2], 0, i);
	emit_move_insn (SET_DEST (set), SET_SRC (set));
      }

    /* The optimizer does not know that the call sets the function value
       registers we stored in the result block.  We avoid problems by
       claiming that all hard registers are used and clobbered at this
       point.  */
    emit_insn (gen_blockage ());

    DONE;
  })


;; Patterns used for splitting and combining.


;; Function prologue and epilogue.

(define_expand "prologue"
  [(const_int 1)]
  ""
  { spu_expand_prologue (); DONE; })

;; "blockage" is only emited in epilogue.  This is what it took to
;; make "basic block reordering" work with the insns sequence
;; generated by the spu_expand_epilogue (taken from mips.md)

(define_insn "blockage"
  [(unspec_volatile [(const_int 0)] UNSPEC_BLOCKAGE)]
  ""
  ""
  [(set_attr "length" "0")])

(define_expand "epilogue"
  [(const_int 2)]
  ""
  { spu_expand_epilogue (false); DONE; })

(define_expand "sibcall_epilogue"
  [(const_int 2)]
  ""
  { spu_expand_epilogue (true); DONE; })


;; stack manipulations

;; An insn to allocate new stack space for dynamic use (e.g., alloca).
;; We move the back-chain and decrement the stack pointer.
(define_expand "allocate_stack"
  [(set (match_operand 0 "spu_reg_operand" "")
	(minus (reg 1) (match_operand 1 "spu_nonmem_operand" "")))
   (set (reg 1)
	(minus (reg 1) (match_dup 1)))]
  ""
  "spu_allocate_stack (operands[0], operands[1]); DONE;")

;; These patterns say how to save and restore the stack pointer.  We need not
;; save the stack pointer at function or block level since we are careful to
;; preserve the backchain.  Doing nothing at block level means the stack space
;; is allocated until the end of the function.  This is currently safe to do
;; because gcc uses the frame pointer for the whole function, so the worst that
;; happens is wasted stack space.  That could be bad if a VLA is declared in a
;; loop, because new space will be allocated every iteration, but users can
;; work around that case.  Ideally we could detect when we are in a loop and
;; generate the more complicated code in that case.
;;
;; For nonlocal gotos, we must save both the stack pointer and its
;; backchain and restore both.  Note that in the nonlocal case, the
;; save area is a memory location.

(define_expand "save_stack_function"
  [(match_operand 0 "general_operand" "")
   (match_operand 1 "general_operand" "")]
  ""
  "DONE;")

(define_expand "restore_stack_function"
  [(match_operand 0 "general_operand" "")
   (match_operand 1 "general_operand" "")]
  ""
  "DONE;")

(define_expand "save_stack_block"
  [(match_operand 0 "general_operand" "")
   (match_operand 1 "general_operand" "")]
  ""
  "DONE; ")

(define_expand "restore_stack_block"
  [(use (match_operand 0 "spu_reg_operand" ""))
   (set (match_dup 2) (match_dup 3))
   (set (match_dup 0) (match_operand 1 "spu_reg_operand" ""))
   (set (match_dup 3) (match_dup 2))]
  ""
  "DONE;")

(define_expand "save_stack_nonlocal"
  [(match_operand 0 "memory_operand" "")
   (match_operand 1 "spu_reg_operand" "")]
  ""
  "
  {
    rtx temp = gen_reg_rtx (Pmode);

    /* Copy the backchain to the first word, sp to the second.  We need to
       save the back chain because __builtin_apply appears to clobber it. */
    emit_move_insn (temp, gen_rtx_MEM (Pmode, operands[1]));
    emit_move_insn (adjust_address_nv (operands[0], SImode, 0), temp);
    emit_move_insn (adjust_address_nv (operands[0], SImode, 4), operands[1]);
    DONE;
  }")

(define_expand "restore_stack_nonlocal"
  [(match_operand 0 "spu_reg_operand" "")
   (match_operand 1 "memory_operand" "")]
  ""
  "
  {
    spu_restore_stack_nonlocal(operands[0], operands[1]);
    DONE;
  }")


;; vector patterns

;; Vector initialization
(define_expand "vec_init<mode>"
  [(match_operand:V 0 "register_operand" "")
   (match_operand 1 "" "")]
  ""
  {
    spu_expand_vector_init (operands[0], operands[1]);
    DONE;
  })

(define_expand "vec_set<mode>"
  [(use (match_operand:SI 2 "spu_nonmem_operand" ""))
   (set (match_dup:TI 3)
        (unspec:TI [(match_dup:SI 4)
		    (match_dup:SI 5)
		    (match_dup:SI 6)] UNSPEC_CPAT))
   (set (match_operand:V 0 "spu_reg_operand" "")
	(unspec:V [(match_operand:<inner> 1 "spu_reg_operand" "")
		   (match_dup:V 0)
		   (match_dup:TI 3)] UNSPEC_SHUFB))]
  ""
  {
    HOST_WIDE_INT size = GET_MODE_SIZE (<inner>mode);
    rtx offset = GEN_INT (INTVAL (operands[2]) * size);
    operands[3] = gen_reg_rtx (TImode);
    operands[4] = stack_pointer_rtx;
    operands[5] = offset;
    operands[6] = GEN_INT (size);
  })

(define_expand "vec_extract<mode>"
  [(set (match_operand:<inner> 0 "spu_reg_operand" "=r")
	(vec_select:<inner> (match_operand:V 1 "spu_reg_operand" "r")
			    (parallel [(match_operand 2 "const_int_operand" "i")])))]
  ""
  {
    if ((INTVAL (operands[2]) * <vmult> + <voff>) % 16 == 0)
      {
	emit_insn (gen_spu_convert (operands[0], operands[1]));
	DONE;
      }
  })

(define_insn "_vec_extract<mode>"
  [(set (match_operand:<inner> 0 "spu_reg_operand" "=r")
	(vec_select:<inner> (match_operand:V 1 "spu_reg_operand" "r")
			    (parallel [(match_operand 2 "const_int_operand" "i")])))]
  ""
  "rotqbyi\t%0,%1,(%2*<vmult>+<voff>)%%16"
  [(set_attr "type" "shuf")])


;; misc

(define_expand "shufb"
  [(set (match_operand 0 "spu_reg_operand" "")
	(unspec [(match_operand 1 "spu_reg_operand" "")
		 (match_operand 2 "spu_reg_operand" "")
		 (match_operand:TI 3 "spu_reg_operand" "")] UNSPEC_SHUFB))]
  ""
  {
    rtx s = gen__shufb (operands[0], operands[1], operands[2], operands[3]);
    PUT_MODE (SET_SRC (s), GET_MODE (operands[0]));
    emit_insn (s);
    DONE;
  })

(define_insn "_shufb"
  [(set (match_operand 0 "spu_reg_operand" "=r")
	(unspec [(match_operand 1 "spu_reg_operand" "r")
		 (match_operand 2 "spu_reg_operand" "r")
		 (match_operand:TI 3 "spu_reg_operand" "r")] UNSPEC_SHUFB))]
  "operands"
  "shufb\t%0,%1,%2,%3"
  [(set_attr "type" "shuf")])

(define_insn "nop"
  [(unspec_volatile [(const_int 0)] UNSPEC_NOP)]
  ""
  "nop"
  [(set_attr "type" "nop")])

(define_insn "nopn"
  [(unspec_volatile [(match_operand:SI 0 "immediate_operand" "K")] UNSPEC_NOP)]
  ""
  "nop\t%0"
  [(set_attr "type" "nop")])

(define_insn "lnop"
  [(unspec_volatile [(const_int 0)] UNSPEC_LNOP)]
  ""
  "lnop"
  [(set_attr "type" "lnop")])

(define_insn "iprefetch"
  [(unspec [(const_int 0)] UNSPEC_IPREFETCH)]
  ""
  "hbrp"
  [(set_attr "type" "iprefetch")])

(define_insn "hbr"
  [(set (reg:SI 130)
	(unspec:SI [(match_operand:SI 0 "immediate_operand" "s,s,s")
		    (match_operand:SI 1 "nonmemory_operand" "r,s,i")] UNSPEC_HBR))
   (unspec [(const_int 0)] UNSPEC_HBR)]
  ""
  "@
   hbr\t%0,%1
   hbrr\t%0,%1
   hbra\t%0,%1"
  [(set_attr "type" "hbr")])

(define_insn "sync"
  [(unspec_volatile [(const_int 0)] UNSPEC_SYNC)
   (clobber (mem:BLK (scratch)))]
  ""
  "sync"
  [(set_attr "type" "br")])

(define_insn "syncc"
  [(unspec_volatile [(const_int 1)] UNSPEC_SYNC)
   (clobber (mem:BLK (scratch)))]
  ""
  "syncc"
  [(set_attr "type" "br")])

(define_insn "dsync"
  [(unspec_volatile [(const_int 2)] UNSPEC_SYNC)
   (clobber (mem:BLK (scratch)))]
  ""
  "dsync"
  [(set_attr "type" "br")])


;; convert between any two modes, avoiding any GCC assumptions
(define_expand "spu_convert"
  [(set (match_operand 0 "spu_reg_operand" "")
	(unspec [(match_operand 1 "spu_reg_operand" "")] UNSPEC_CONVERT))]
  ""
  {
    rtx c = gen__spu_convert (operands[0], operands[1]);
    PUT_MODE (SET_SRC (c), GET_MODE (operands[0]));
    emit_insn (c);
    DONE;
  })

(define_insn "_spu_convert"
  [(set (match_operand 0 "spu_reg_operand" "=r")
	(unspec [(match_operand 1 "spu_reg_operand" "0")] UNSPEC_CONVERT))]
  "operands"
  ""
  [(set_attr "type" "convert")
   (set_attr "length" "0")])

(define_peephole2
  [(set (match_operand 0 "spu_reg_operand")
	(unspec [(match_operand 1 "spu_reg_operand")] UNSPEC_CONVERT))]
  ""
  [(use (const_int 0))]
  "")


;;
(include "spu-builtins.md")

  
(define_expand "smaxv4sf3"
  [(set (match_operand:V4SF 0 "register_operand" "=r")
        (smax:V4SF (match_operand:V4SF 1 "register_operand" "r")
                 (match_operand:V4SF 2 "register_operand" "r")))]
  ""
  "
{
  rtx mask = gen_reg_rtx (V4SImode);

  emit_insn (gen_cgt_v4sf (mask, operands[1], operands[2]));
  emit_insn (gen_selb (operands[0], operands[2], operands[1], mask));
  DONE;
}") 

(define_expand "sminv4sf3"
  [(set (match_operand:V4SF 0 "register_operand" "=r")
        (smax:V4SF (match_operand:V4SF 1 "register_operand" "r")
                 (match_operand:V4SF 2 "register_operand" "r")))]
  ""
  "
{
  rtx mask = gen_reg_rtx (V4SImode);

  emit_insn (gen_cgt_v4sf (mask, operands[1], operands[2]));
  emit_insn (gen_selb (operands[0], operands[1], operands[2], mask));
  DONE;
}") 

(define_expand "vec_widen_umult_hi_v8hi"
  [(set (match_operand:V4SI 0 "register_operand"   "=r")
        (mult:V4SI
          (zero_extend:V4SI
            (vec_select:V4HI
              (match_operand:V8HI 1 "register_operand" "r")
              (parallel [(const_int 0)(const_int 1)(const_int 2)(const_int 3)])))
          (zero_extend:V4SI
            (vec_select:V4HI
              (match_operand:V8HI 2 "register_operand" "r")
              (parallel [(const_int 0)(const_int 1)(const_int 2)(const_int 3)])))))]
  ""
  "
{
  rtx ve = gen_reg_rtx (V4SImode);
  rtx vo = gen_reg_rtx (V4SImode);
  rtx mask = gen_reg_rtx (TImode);
  unsigned char arr[16] = {
    0x00, 0x01, 0x02, 0x03, 0x10, 0x11, 0x12, 0x13, 
    0x04, 0x05, 0x06, 0x07, 0x14, 0x15, 0x16, 0x17};
  
  emit_move_insn (mask, array_to_constant (TImode, arr));
  emit_insn (gen_spu_mpyhhu (ve, operands[1], operands[2]));
  emit_insn (gen_spu_mpyu (vo, operands[1], operands[2]));
  emit_insn (gen_shufb (operands[0], ve, vo, mask));
  DONE;
}")

(define_expand "vec_widen_umult_lo_v8hi"
  [(set (match_operand:V4SI 0 "register_operand"   "=r")
        (mult:V4SI
          (zero_extend:V4SI
            (vec_select:V4HI
              (match_operand:V8HI 1 "register_operand" "r")
              (parallel [(const_int 4)(const_int 5)(const_int 6)(const_int 7)])))
          (zero_extend:V4SI
            (vec_select:V4HI
              (match_operand:V8HI 2 "register_operand" "r")
              (parallel [(const_int 4)(const_int 5)(const_int 6)(const_int 7)])))))]
  ""
  "
{
  rtx ve = gen_reg_rtx (V4SImode);
  rtx vo = gen_reg_rtx (V4SImode);
  rtx mask = gen_reg_rtx (TImode);
  unsigned char arr[16] = {
    0x08, 0x09, 0x0A, 0x0B, 0x18, 0x19, 0x1A, 0x1B, 
    0x0C, 0x0D, 0x0E, 0x0F, 0x1C, 0x1D, 0x1E, 0x1F};

  emit_move_insn (mask, array_to_constant (TImode, arr));
  emit_insn (gen_spu_mpyhhu (ve, operands[1], operands[2]));
  emit_insn (gen_spu_mpyu (vo, operands[1], operands[2]));
  emit_insn (gen_shufb (operands[0], ve, vo, mask));
  DONE;
}")

(define_expand "vec_widen_smult_hi_v8hi"
  [(set (match_operand:V4SI 0 "register_operand"   "=r")
        (mult:V4SI
          (sign_extend:V4SI
            (vec_select:V4HI
              (match_operand:V8HI 1 "register_operand" "r")
              (parallel [(const_int 0)(const_int 1)(const_int 2)(const_int 3)])))
          (sign_extend:V4SI
            (vec_select:V4HI
              (match_operand:V8HI 2 "register_operand" "r")
              (parallel [(const_int 0)(const_int 1)(const_int 2)(const_int 3)])))))]
  ""
  "
{
  rtx ve = gen_reg_rtx (V4SImode);
  rtx vo = gen_reg_rtx (V4SImode);
  rtx mask = gen_reg_rtx (TImode);
  unsigned char arr[16] = {
    0x00, 0x01, 0x02, 0x03, 0x10, 0x11, 0x12, 0x13, 
    0x04, 0x05, 0x06, 0x07, 0x14, 0x15, 0x16, 0x17};
  
  emit_move_insn (mask, array_to_constant (TImode, arr));
  emit_insn (gen_spu_mpyhh (ve, operands[1], operands[2]));
  emit_insn (gen_spu_mpy (vo, operands[1], operands[2]));
  emit_insn (gen_shufb (operands[0], ve, vo, mask));
  DONE;
}")

(define_expand "vec_widen_smult_lo_v8hi"
  [(set (match_operand:V4SI 0 "register_operand"   "=r")
        (mult:V4SI
          (sign_extend:V4SI
            (vec_select:V4HI
              (match_operand:V8HI 1 "register_operand" "r")
              (parallel [(const_int 4)(const_int 5)(const_int 6)(const_int 7)])))
          (sign_extend:V4SI
            (vec_select:V4HI
              (match_operand:V8HI 2 "register_operand" "r")
              (parallel [(const_int 4)(const_int 5)(const_int 6)(const_int 7)])))))]
  ""
  "
{
  rtx ve = gen_reg_rtx (V4SImode);
  rtx vo = gen_reg_rtx (V4SImode);
  rtx mask = gen_reg_rtx (TImode);
  unsigned char arr[16] = {
    0x08, 0x09, 0x0A, 0x0B, 0x18, 0x19, 0x1A, 0x1B, 
    0x0C, 0x0D, 0x0E, 0x0F, 0x1C, 0x1D, 0x1E, 0x1F};

  emit_move_insn (mask, array_to_constant (TImode, arr));
  emit_insn (gen_spu_mpyhh (ve, operands[1], operands[2]));
  emit_insn (gen_spu_mpy (vo, operands[1], operands[2]));
  emit_insn (gen_shufb (operands[0], ve, vo, mask));
  DONE;
}")

(define_expand "vec_realign_load_<mode>"
  [(set (match_operand:ALL 0 "register_operand" "=r")
	(unspec:ALL [(match_operand:ALL 1 "register_operand" "r")
		     (match_operand:ALL 2 "register_operand" "r")
		     (match_operand:TI 3 "register_operand" "r")] UNSPEC_SPU_REALIGN_LOAD))]
  ""
  "
{
  emit_insn (gen_shufb (operands[0], operands[1], operands[2], operands[3])); 
  DONE;
}")

(define_expand "spu_lvsr"
  [(set (match_operand:V16QI 0 "register_operand" "")
        (unspec:V16QI [(match_operand 1 "memory_operand" "")] UNSPEC_SPU_MASK_FOR_LOAD))]
  ""
  "
{ 
  rtx addr;
  rtx offset = gen_reg_rtx (V8HImode);
  rtx addr_bits = gen_reg_rtx (SImode);
  rtx addr_bits_vec = gen_reg_rtx (V8HImode);
  rtx splatqi = gen_reg_rtx (TImode);
  rtx result = gen_reg_rtx (V8HImode);
  unsigned char arr[16] = {
    0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 
    0x18, 0x19, 0x1A, 0x1B, 0x1C, 0x1D, 0x1E, 0x1F};
  unsigned char arr2[16] = {
    0x03, 0x03, 0x03, 0x03, 0x03, 0x03, 0x03, 0x03, 
    0x03, 0x03, 0x03, 0x03, 0x03, 0x03, 0x03, 0x03};

  emit_move_insn (offset, array_to_constant (V8HImode, arr));
  emit_move_insn (splatqi, array_to_constant (TImode, arr2));

  gcc_assert (GET_CODE (operands[1]) == MEM);
  addr = force_reg (Pmode, XEXP (operands[1], 0));
  emit_insn (gen_andsi3 (addr_bits, addr, GEN_INT (0xF))); 
  emit_insn (gen_shufb (addr_bits_vec, addr_bits, addr_bits, splatqi));

  /* offset - (addr & 0xF) 
     It is safe to use a single sfh, because each byte of offset is > 15 and
     each byte of addr is <= 15. */
  emit_insn (gen_subv8hi3 (result, offset, addr_bits_vec));

  result = simplify_gen_subreg (V16QImode, result, V8HImode, 0);
  emit_move_insn (operands[0], result);

  DONE;
}")
