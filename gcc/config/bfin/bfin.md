;;- Machine description for Blackfin for GNU compiler
;;  Copyright (C) 2005-2025 Free Software Foundation, Inc.
;;  Contributed by Analog Devices.

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.

;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

; operand punctuation marks:
;
;     X -- integer value printed as log2
;     Y -- integer value printed as log2(~value) - for bitclear
;     h -- print half word register, low part
;     d -- print half word register, high part
;     D -- print operand as dregs pairs
;     w -- print operand as accumulator register word (a0w, a1w)
;     H -- high part of double mode operand
;     T -- byte register representation Oct. 02 2001

; constant operand classes
;
;     J   2**N       5bit imm scaled
;     Ks7 -64 .. 63  signed 7bit imm
;     Ku5 0..31      unsigned 5bit imm
;     Ks4 -8 .. 7    signed 4bit imm
;     Ks3 -4 .. 3    signed 3bit imm
;     Ku3 0 .. 7     unsigned 3bit imm
;     Pn  0, 1, 2    constants 0, 1 or 2, corresponding to n
;
; register operands
;     d  (r0..r7)
;     a  (p0..p5,fp,sp)
;     e  (a0, a1)
;     b  (i0..i3)
;     f  (m0..m3)
;     v  (b0..b3)
;     c  (i0..i3,m0..m3) CIRCREGS
;     C  (CC)            CCREGS
;     t  (lt0,lt1)
;     k  (lc0,lc1)
;     u  (lb0,lb1)
;

;; Define constants for hard registers.

(define_constants
  [(REG_R0 0)
   (REG_R1 1)
   (REG_R2 2)
   (REG_R3 3)
   (REG_R4 4)
   (REG_R5 5)
   (REG_R6 6)
   (REG_R7 7)

   (REG_P0 8)
   (REG_P1 9)
   (REG_P2 10)
   (REG_P3 11)
   (REG_P4 12)
   (REG_P5 13)
   (REG_P6 14)
   (REG_P7 15)

   (REG_SP 14)
   (REG_FP 15)

   (REG_I0 16)
   (REG_I1 17)
   (REG_I2 18)
   (REG_I3 19)

   (REG_B0 20)
   (REG_B1 21)
   (REG_B2 22)
   (REG_B3 23)

   (REG_L0 24)
   (REG_L1 25)
   (REG_L2 26)
   (REG_L3 27)

   (REG_M0 28)
   (REG_M1 29)
   (REG_M2 30)
   (REG_M3 31)

   (REG_A0 32)
   (REG_A1 33)

   (REG_CC 34)
   (REG_RETS 35)
   (REG_RETI 36)
   (REG_RETX 37)
   (REG_RETN 38)
   (REG_RETE 39)

   (REG_ASTAT 40)
   (REG_SEQSTAT 41)
   (REG_USP 42)

   (REG_ARGP 43)

   (REG_LT0 44)
   (REG_LT1 45)
   (REG_LC0 46)
   (REG_LC1 47)
   (REG_LB0 48)
   (REG_LB1 49)])

;; Constants used in UNSPECs and UNSPEC_VOLATILEs.

(define_constants
  [(UNSPEC_CBRANCH_TAKEN 0)
   (UNSPEC_CBRANCH_NOPS 1)
   (UNSPEC_RETURN 2)
   (UNSPEC_MOVE_PIC 3)
   (UNSPEC_LIBRARY_OFFSET 4)
   (UNSPEC_PUSH_MULTIPLE 5)
   ;; Multiply or MAC with extra CONST_INT operand specifying the macflag
   (UNSPEC_MUL_WITH_FLAG 6)
   (UNSPEC_MAC_WITH_FLAG 7)
   (UNSPEC_MOVE_FDPIC 8)
   (UNSPEC_FUNCDESC_GOT17M4 9)
   (UNSPEC_LSETUP_END 10)
   ;; Distinguish a 32-bit version of an insn from a 16-bit version.
   (UNSPEC_32BIT 11)
   (UNSPEC_NOP 12)
   (UNSPEC_ATOMIC 13)])

(define_constants
  [(UNSPEC_VOLATILE_CSYNC 1)
   (UNSPEC_VOLATILE_SSYNC 2)
   (UNSPEC_VOLATILE_LOAD_FUNCDESC 3)
   (UNSPEC_VOLATILE_STORE_EH_HANDLER 4)
   (UNSPEC_VOLATILE_DUMMY 5)
   (UNSPEC_VOLATILE_STALL 6)])

(define_constants
  [(MACFLAG_NONE 0)
   (MACFLAG_T 1)
   (MACFLAG_FU 2)
   (MACFLAG_TFU 3)
   (MACFLAG_IS 4)
   (MACFLAG_IU 5)
   (MACFLAG_W32 6)
   (MACFLAG_M 7)
   (MACFLAG_IS_M 8)
   (MACFLAG_S2RND 9)
   (MACFLAG_ISS2 10)
   (MACFLAG_IH 11)])

(define_attr "type"
  "move,movcc,mvi,mcld,mcst,dsp32,dsp32shiftimm,mult,alu0,shft,brcc,br,call,misc,sync,compare,dummy,stall"
  (const_string "misc"))

(define_attr "addrtype" "32bit,preg,spreg,ireg"
  (cond [(and (eq_attr "type" "mcld")
	      (and (match_operand 0 "dp_register_operand" "")
		   (match_operand 1 "mem_p_address_operand" "")))
	   (const_string "preg")
	 (and (eq_attr "type" "mcld")
	      (and (match_operand 0 "dp_register_operand" "")
		   (match_operand 1 "mem_spfp_address_operand" "")))
	   (const_string "spreg")
	 (and (eq_attr "type" "mcld")
	      (and (match_operand 0 "dp_register_operand" "")
		   (match_operand 1 "mem_i_address_operand" "")))
	   (const_string "ireg")
	 (and (eq_attr "type" "mcst")
	      (and (match_operand 1 "dp_register_operand" "")
		   (match_operand 0 "mem_p_address_operand" "")))
	   (const_string "preg")
	 (and (eq_attr "type" "mcst")
	      (and (match_operand 1 "dp_register_operand" "")
		   (match_operand 0 "mem_spfp_address_operand" "")))
	   (const_string "spreg")
	 (and (eq_attr "type" "mcst")
	      (and (match_operand 1 "dp_register_operand" "")
		   (match_operand 0 "mem_i_address_operand" "")))
	   (const_string "ireg")]
	(const_string "32bit")))

(define_attr "storereg" "preg,other"
  (cond [(and (eq_attr "type" "mcst")
	      (match_operand 1 "p_register_operand" ""))
	   (const_string "preg")]
	(const_string "other")))

;; Scheduling definitions

(define_automaton "bfin")

(define_cpu_unit "slot0" "bfin")
(define_cpu_unit "slot1" "bfin")
(define_cpu_unit "slot2" "bfin")

;; Three units used to enforce parallel issue restrictions:
;; only one of the 16-bit slots can use a P register in an address,
;; and only one them can be a store.
(define_cpu_unit "store" "bfin")
(define_cpu_unit "pregs" "bfin")

;; A dummy unit used to delay scheduling of loads after a conditional
;; branch.
(define_cpu_unit "load" "bfin")

;; A logical unit used to work around anomaly 05000074.
(define_cpu_unit "anomaly_05000074" "bfin")

(define_reservation "core" "slot0+slot1+slot2")

(define_insn_reservation "alu" 1
  (eq_attr "type" "move,movcc,mvi,alu0,shft,brcc,br,call,misc,sync,compare")
  "core")

(define_insn_reservation "imul" 3
  (eq_attr "type" "mult")
  "core*3")

(define_insn_reservation "dsp32" 1
  (eq_attr "type" "dsp32")
  "slot0")

(define_insn_reservation "dsp32shiftimm" 1
  (and (eq_attr "type" "dsp32shiftimm")
       (not (match_test "ENABLE_WA_05000074")))
  "slot0")

(define_insn_reservation "dsp32shiftimm_anomaly_05000074" 1
  (and (eq_attr "type" "dsp32shiftimm")
       (match_test "ENABLE_WA_05000074"))
  "slot0+anomaly_05000074")

(define_insn_reservation "load32" 1
  (and (not (eq_attr "seq_insns" "multi"))
       (and (eq_attr "type" "mcld") (eq_attr "addrtype" "32bit")))
  "core+load")

(define_insn_reservation "loadp" 1
  (and (not (eq_attr "seq_insns" "multi"))
       (and (eq_attr "type" "mcld") (eq_attr "addrtype" "preg")))
  "slot1+pregs+load")

(define_insn_reservation "loadsp" 1
  (and (not (eq_attr "seq_insns" "multi"))
       (and (eq_attr "type" "mcld") (eq_attr "addrtype" "spreg")))
  "slot1+pregs")

(define_insn_reservation "loadi" 1
  (and (not (eq_attr "seq_insns" "multi"))
       (and (eq_attr "type" "mcld") (eq_attr "addrtype" "ireg")))
  "(slot1|slot2)+load")

(define_insn_reservation "store32" 1
  (and (not (eq_attr "seq_insns" "multi"))
       (and (eq_attr "type" "mcst") (eq_attr "addrtype" "32bit")))
  "core")

(define_insn_reservation "storep" 1
  (and (and (not (eq_attr "seq_insns" "multi"))
	    (and (eq_attr "type" "mcst")
		 (ior (eq_attr "addrtype" "preg")
		      (eq_attr "addrtype" "spreg"))))
       (ior (not (match_test "ENABLE_WA_05000074"))
	    (eq_attr "storereg" "other")))
  "slot1+pregs+store")

(define_insn_reservation "storep_anomaly_05000074" 1
  (and (and (not (eq_attr "seq_insns" "multi"))
	    (and (eq_attr "type" "mcst")
		 (ior (eq_attr "addrtype" "preg")
		      (eq_attr "addrtype" "spreg"))))
       (and (match_test "ENABLE_WA_05000074")
	    (eq_attr "storereg" "preg")))
  "slot1+anomaly_05000074+pregs+store")

(define_insn_reservation "storei" 1
  (and (and (not (eq_attr "seq_insns" "multi"))
	    (and (eq_attr "type" "mcst") (eq_attr "addrtype" "ireg")))
       (ior (not (match_test "ENABLE_WA_05000074"))
	    (eq_attr "storereg" "other")))
  "(slot1|slot2)+store")

(define_insn_reservation "storei_anomaly_05000074" 1
  (and (and (not (eq_attr "seq_insns" "multi"))
	    (and (eq_attr "type" "mcst") (eq_attr "addrtype" "ireg")))
       (and (match_test "ENABLE_WA_05000074")
	    (eq_attr "storereg" "preg")))
  "((slot1+anomaly_05000074)|slot2)+store")

(define_insn_reservation "multi" 2
  (eq_attr "seq_insns" "multi")
  "core")

(define_insn_reservation "load_stall1" 1
  (and (eq_attr "type" "stall")
       (match_operand 0 "const1_operand" ""))
  "core+load*2")

(define_insn_reservation "load_stall3" 1
  (and (eq_attr "type" "stall")
       (match_operand 0 "const3_operand" ""))
  "core+load*4")

(absence_set "slot0" "slot1,slot2")
(absence_set "slot1" "slot2")

;; Make sure genautomata knows about the maximum latency that can be produced
;; by the adjust_cost function.
(define_insn_reservation "dummy" 5
  (eq_attr "type" "dummy")
  "core")

;; Operand and operator predicates

(include "predicates.md")
(include "constraints.md")

;;; FRIO branches have been optimized for code density
;;; this comes at a slight cost of complexity when
;;; a compiler needs to generate branches in the general
;;; case.  In order to generate the correct branching
;;; mechanisms the compiler needs keep track of instruction
;;; lengths.  The follow table describes how to count instructions
;;; for the FRIO architecture.
;;;
;;; unconditional br are 12-bit imm pcrelative branches *2
;;; conditional   br are 10-bit imm pcrelative branches *2
;;; brcc 10-bit:
;;;   1024 10-bit imm *2 is 2048 (-1024..1022)
;;; br 12-bit  :
;;;   4096 12-bit imm *2 is 8192 (-4096..4094)
;;; NOTE : For brcc we generate instructions such as
;;;   if cc jmp; jump.[sl] offset
;;;   offset of jump.[sl] is from the jump instruction but
;;;     gcc calculates length from the if cc jmp instruction
;;;     furthermore gcc takes the end address of the branch instruction
;;;     as (pc) for a forward branch
;;;     hence our range is (-4094, 4092) instead of (-4096, 4094) for a br
;;;
;;; The way the (pc) rtx works in these calculations is somewhat odd;
;;; for backward branches it's the address of the current instruction,
;;; for forward branches it's the previously known address of the following
;;; instruction - we have to take this into account by reducing the range
;;; for a forward branch.

;; Lengths for type "mvi" insns are always defined by the instructions
;; themselves.
(define_attr "length" ""
  (cond [(eq_attr "type" "mcld")
         (if_then_else (match_operand 1 "effective_address_32bit_p" "")
                       (const_int 4) (const_int 2))

	 (eq_attr "type" "mcst")
	 (if_then_else (match_operand 0 "effective_address_32bit_p" "")
		       (const_int 4) (const_int 2))

	 (eq_attr "type" "move") (const_int 2)

	 (eq_attr "type" "dsp32") (const_int 4)
	 (eq_attr "type" "dsp32shiftimm") (const_int 4)
	 (eq_attr "type" "call")  (const_int 4)

         (eq_attr "type" "br")
  	 (if_then_else (and
	                  (le (minus (match_dup 0) (pc)) (const_int 4092))
	                  (ge (minus (match_dup 0) (pc)) (const_int -4096)))
        	  (const_int 2)
                  (const_int 4))

         (eq_attr "type" "brcc")
	 (cond [(and
	            (le (minus (match_dup 3) (pc)) (const_int 1020))
	            (ge (minus (match_dup 3) (pc)) (const_int -1024)))
		  (const_int 2)
		(and
	            (le (minus (match_dup 3) (pc)) (const_int 4092))
	            (ge (minus (match_dup 3) (pc)) (const_int -4094)))
		  (const_int 4)]
	       (const_int 6))
        ]

	(const_int 2)))

;; Classify the insns into those that are one instruction and those that
;; are more than one in sequence.
(define_attr "seq_insns" "single,multi"
  (const_string "single"))

;; Describe a user's asm statement.
(define_asm_attributes
  [(set_attr "type" "misc")
   (set_attr "seq_insns" "multi")
   (set_attr "length" "4")])

;; Conditional moves

(define_mode_iterator CCMOV [QI HI SI])

(define_expand "mov<mode>cc"
  [(set (match_operand:CCMOV 0 "register_operand" "")
        (if_then_else:CCMOV (match_operand 1 "comparison_operator" "")
			    (match_operand:CCMOV 2 "register_operand" "")
			    (match_operand:CCMOV 3 "register_operand" "")))]
  ""
{
  operands[1] = bfin_gen_compare (operands[1], <MODE>mode);
})

(define_insn "*mov<mode>cc_insn1"
  [(set (match_operand:CCMOV 0 "register_operand" "=da,da,da")
        (if_then_else:CCMOV
	    (eq:BI (match_operand:BI 3 "register_operand" "C,C,C")
		(const_int 0))
	    (match_operand:CCMOV 1 "register_operand" "da,0,da")
	    (match_operand:CCMOV 2 "register_operand" "0,da,da")))]
  ""
  "@
    if !cc %0 = %1;
    if cc %0 = %2;
    if !cc %0 = %1; if cc %0 = %2;"
  [(set_attr "length" "2,2,4")
   (set_attr "type" "movcc")
   (set_attr "seq_insns" "*,*,multi")])

(define_insn "*mov<mode>cc_insn2"
  [(set (match_operand:CCMOV 0 "register_operand" "=da,da,da")
        (if_then_else:CCMOV
	    (ne:BI (match_operand:BI 3 "register_operand" "C,C,C")
		(const_int 0))
	    (match_operand:CCMOV 1 "register_operand" "0,da,da")
	    (match_operand:CCMOV 2 "register_operand" "da,0,da")))]
  ""
  "@
   if !cc %0 = %2;
   if cc %0 = %1;
   if cc %0 = %1; if !cc %0 = %2;"
  [(set_attr "length" "2,2,4")
   (set_attr "type" "movcc")
   (set_attr "seq_insns" "*,*,multi")])

;; Insns to load HIGH and LO_SUM

(define_insn "movsi_high"
  [(set (match_operand:SI 0 "register_operand" "=x")
	(high:SI (match_operand:SI 1 "immediate_operand" "i")))]
  "reload_completed"
  "%d0 = %d1;"
  [(set_attr "type" "mvi")
   (set_attr "length" "4")])

(define_insn "movstricthi_high"
  [(set (match_operand:SI 0 "register_operand" "+x")
	(ior:SI (and:SI (match_dup 0) (const_int 65535))
		(match_operand:SI 1 "immediate_operand" "i")))]
  "reload_completed"
  "%d0 = %d1;"
  [(set_attr "type" "mvi")
   (set_attr "length" "4")])

(define_insn "movsi_low"
  [(set (match_operand:SI 0 "register_operand" "=x")
	(lo_sum:SI (match_operand:SI 1 "register_operand" "0")
		   (match_operand:SI 2 "immediate_operand" "i")))]
  "reload_completed"
  "%h0 = %h2;"
  [(set_attr "type" "mvi")
   (set_attr "length" "4")])

(define_insn "movsi_high_pic"
  [(set (match_operand:SI 0 "register_operand" "=x")
	(high:SI (unspec:SI [(match_operand:SI 1 "" "")]
			    UNSPEC_MOVE_PIC)))]
  ""
  "%d0 = %1@GOT_LOW;"
  [(set_attr "type" "mvi")
   (set_attr "length" "4")])

(define_insn "movsi_low_pic"
  [(set (match_operand:SI 0 "register_operand" "=x")
	(lo_sum:SI (match_operand:SI 1 "register_operand" "0")
		   (unspec:SI [(match_operand:SI 2 "" "")]
			      UNSPEC_MOVE_PIC)))]
  ""
  "%h0 = %h2@GOT_HIGH;"
  [(set_attr "type" "mvi")
   (set_attr "length" "4")])

;;; Move instructions

(define_insn_and_split "movdi_insn"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=x,mx,r")
	(match_operand:DI 1 "general_operand" "iFx,r,mx"))]
  "GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) == REG"
  "#"
  "reload_completed"
  [(set (match_dup 2) (match_dup 3))
   (set (match_dup 4) (match_dup 5))]
{
  rtx lo_half[2], hi_half[2];
  split_di (operands, 2, lo_half, hi_half);

  if (reg_overlap_mentioned_p (lo_half[0], hi_half[1]))
    {
      operands[2] = hi_half[0];
      operands[3] = hi_half[1];
      operands[4] = lo_half[0];
      operands[5] = lo_half[1];
    }
  else
    {
      operands[2] = lo_half[0];
      operands[3] = lo_half[1];
      operands[4] = hi_half[0];
      operands[5] = hi_half[1];
    }
})

(define_insn "movbi"
  [(set (match_operand:BI 0 "nonimmediate_operand" "=x,x,d,md,C,d,C,P1")
        (match_operand:BI 1 "general_operand" "x,xKs3,md,d,d,C,P0,P1"))]

  ""
  "@
   %0 = %1;
   %0 = %1 (X);
   %0 = B %1 (Z)%!
   B %0 = %1;
   CC = %1;
   %0 = CC;
   CC = R0 < R0;
   CC = R0 == R0;"
  [(set_attr "type" "move,mvi,mcld,mcst,compare,compare,compare,compare")
   (set_attr "length" "2,2,*,*,2,2,2,2")
   (set_attr "seq_insns" "*,*,*,*,*,*,*,*")])

(define_insn "movpdi"
  [(set (match_operand:PDI 0 "nonimmediate_operand" "=e,<,e")
        (match_operand:PDI 1 "general_operand" " e,e,>"))]
  ""
  "@
   %0 = %1;
   %0 = %x1; %0 = %w1;
   %w0 = %1; %x0 = %1;"
  [(set_attr "type" "move,mcst,mcld")
   (set_attr "length" "4,*,*")
   (set_attr "seq_insns" "*,multi,multi")])

(define_insn "load_accumulator"
  [(set (match_operand:PDI 0 "register_operand" "=e")
        (sign_extend:PDI (match_operand:SI 1 "register_operand" "d")))]
  ""
  "%0 = %1;"
  [(set_attr "type" "move")])

(define_insn_and_split "load_accumulator_pair"
  [(set (match_operand:V2PDI 0 "register_operand" "=e")
        (sign_extend:V2PDI (vec_concat:V2SI
			    (match_operand:SI 1 "register_operand" "d")
			    (match_operand:SI 2 "register_operand" "d"))))]
  ""
  "#"
  "reload_completed"
  [(set (match_dup 3) (sign_extend:PDI (match_dup 1)))
   (set (match_dup 4) (sign_extend:PDI (match_dup 2)))]
{
  operands[3] = gen_rtx_REG (PDImode, REGNO (operands[0]));
  operands[4] = gen_rtx_REG (PDImode, REGNO (operands[0]) + 1);
})

(define_insn "*pushsi_insn"
  [(set (mem:SI (pre_dec:SI (reg:SI REG_SP)))
        (match_operand:SI 0 "register_operand" "xy"))]
  ""
  "[--SP] = %0;"
  [(set_attr "type" "mcst")
   (set_attr "addrtype" "32bit")
   (set_attr "length" "2")])

(define_insn "*popsi_insn"
  [(set (match_operand:SI 0 "register_operand" "=d,xy")
        (mem:SI (post_inc:SI (reg:SI REG_SP))))]
  ""
  "%0 = [SP++]%!"
  [(set_attr "type" "mcld")
   (set_attr "addrtype" "preg,32bit")
   (set_attr "length" "2")])

;; The first alternative is used to make reload choose a limited register
;; class when faced with a movsi_insn that had its input operand replaced
;; with a PLUS.  We generally require fewer secondary reloads this way.

(define_insn "*movsi_insn"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=da,x,da,y,da,x,x,x,da,mr")
	(match_operand:SI 1 "general_operand" "da,x,y,da,xKs7,xKsh,xKuh,ix,mr,da"))]
  "GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) == REG"
 "@
   %0 = %1;
   %0 = %1;
   %0 = %1;
   %0 = %1;
   %0 = %1 (X);
   %0 = %1 (X);
   %0 = %1 (Z);
   #
   %0 = %1%!
   %0 = %1%!"
  [(set_attr "type" "move,move,move,move,mvi,mvi,mvi,*,mcld,mcst")
   (set_attr "length" "2,2,2,2,2,4,4,*,*,*")])

(define_insn "*movsi_insn32"
  [(set (match_operand:SI 0 "register_operand" "=d,d")
	(unspec:SI [(match_operand:SI 1 "nonmemory_operand" "d,P0")] UNSPEC_32BIT))]
  ""
 "@
   %0 = ROT %1 BY 0%!
   %0 = %0 -|- %0%!"
  [(set_attr "type" "dsp32shiftimm,dsp32")])

(define_split
  [(set (match_operand:SI 0 "d_register_operand" "")
	(const_int 0))]
  "splitting_for_sched && !optimize_size"
  [(set (match_dup 0) (unspec:SI [(const_int 0)] UNSPEC_32BIT))])

(define_split
  [(set (match_operand:SI 0 "d_register_operand" "")
	(match_operand:SI 1 "d_register_operand" ""))]
  "splitting_for_sched && !optimize_size"
  [(set (match_dup 0) (unspec:SI [(match_dup 1)] UNSPEC_32BIT))])

(define_insn_and_split "*movv2hi_insn"
  [(set (match_operand:V2HI 0 "nonimmediate_operand" "=da,da,d,dm")
        (match_operand:V2HI 1 "general_operand" "i,di,md,d"))]

  "GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) == REG"
  "@
   #
   %0 = %1;
   %0 = %1%!
   %0 = %1%!"
  "reload_completed && GET_CODE (operands[1]) == CONST_VECTOR"
  [(set (match_dup 0) (high:SI (match_dup 2)))
   (set (match_dup 0) (lo_sum:SI (match_dup 0) (match_dup 3)))]
{
  HOST_WIDE_INT intval = INTVAL (XVECEXP (operands[1], 0, 1)) << 16;
  intval |= INTVAL (XVECEXP (operands[1], 0, 0)) & 0xFFFF;

  operands[0] = gen_rtx_REG (SImode, REGNO (operands[0]));
  operands[2] = operands[3] = GEN_INT (trunc_int_for_mode (intval, SImode));
}
  [(set_attr "type" "move,move,mcld,mcst")
   (set_attr "length" "2,2,*,*")])

(define_insn "*movhi_insn"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=x,da,x,d,mr")
        (match_operand:HI 1 "general_operand" "x,xKs7,xKsh,mr,d"))]
  "GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) == REG"
{
  static const char *templates[] = {
    "%0 = %1;",
    "%0 = %1 (X);",
    "%0 = %1 (X);",
    "%0 = W %1 (X)%!",
    "W %0 = %1%!",
    "%h0 = W %1%!",
    "W %0 = %h1%!"
  };
  int alt = which_alternative;
  rtx mem = (MEM_P (operands[0]) ? operands[0]
	     : MEM_P (operands[1]) ? operands[1] : NULL_RTX);
  if (mem && bfin_dsp_memref_p (mem))
    alt += 2;
  return templates[alt];
}
  [(set_attr "type" "move,mvi,mvi,mcld,mcst")
   (set_attr "length" "2,2,4,*,*")])

(define_insn "*movqi_insn"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=x,da,x,d,mr")
        (match_operand:QI 1 "general_operand" "x,xKs7,xKsh,mr,d"))]
  "GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) == REG"
  "@
   %0 = %1;
   %0 = %1 (X);
   %0 = %1 (X);
   %0 = B %1 (X)%!
   B %0 = %1%!"
  [(set_attr "type" "move,mvi,mvi,mcld,mcst")
   (set_attr "length" "2,2,4,*,*")])

(define_insn "*movsf_insn"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=x,x,da,mr")
        (match_operand:SF 1 "general_operand" "x,Fx,mr,da"))]
  "GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) == REG"
  "@
   %0 = %1;
   #
   %0 = %1%!
   %0 = %1%!"
  [(set_attr "type" "move,*,mcld,mcst")])

(define_insn_and_split "movdf_insn"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=x,mx,r")
	(match_operand:DF 1 "general_operand" "iFx,r,mx"))]
  "GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) == REG"
  "#"
  "reload_completed"
  [(set (match_dup 2) (match_dup 3))
   (set (match_dup 4) (match_dup 5))]
{
  rtx lo_half[2], hi_half[2];
  split_di (operands, 2, lo_half, hi_half);

  if (reg_overlap_mentioned_p (lo_half[0], hi_half[1]))
    {
      operands[2] = hi_half[0];
      operands[3] = hi_half[1];
      operands[4] = lo_half[0];
      operands[5] = lo_half[1];
    }
  else
    {
      operands[2] = lo_half[0];
      operands[3] = lo_half[1];
      operands[4] = hi_half[0];
      operands[5] = hi_half[1];
    }
})

;; Storing halfwords.
(define_insn "*movsi_insv"
  [(set (zero_extract:SI (match_operand 0 "register_operand" "+d,x")
			 (const_int 16)
			 (const_int 16))
	(match_operand:SI 1 "nonmemory_operand" "d,n"))]
  ""
  "@
   %d0 = %h1 << 0%!
   %d0 = %1;"
  [(set_attr "type" "dsp32shiftimm,mvi")
   (set_attr "length" "*,4")])

(define_expand "insv"
  [(set (zero_extract:SI (match_operand:SI 0 "register_operand" "")
			 (match_operand:SI 1 "immediate_operand" "")
			 (match_operand:SI 2 "immediate_operand" ""))
        (match_operand:SI 3 "nonmemory_operand" ""))]
  ""
{
  if (INTVAL (operands[1]) != 16 || INTVAL (operands[2]) != 16)
    FAIL;

  /* From mips.md: insert_bit_field doesn't verify that our source
     matches the predicate, so check it again here.  */
  if (! register_operand (operands[0], VOIDmode))
    FAIL;
})

;; This is the main "hook" for PIC code.  When generating
;; PIC, movsi is responsible for determining when the source address
;; needs PIC relocation and appropriately calling legitimize_pic_address
;; to perform the actual relocation.

(define_expand "movsi"
  [(set (match_operand:SI 0 "nonimmediate_operand" "")
	(match_operand:SI 1 "general_operand" ""))]
  ""
{
  if (expand_move (operands, SImode))
    DONE;
})

(define_expand "movv2hi"
  [(set (match_operand:V2HI 0 "nonimmediate_operand" "")
	(match_operand:V2HI 1 "general_operand" ""))]
  ""
  "expand_move (operands, V2HImode);")

(define_expand "movdi"
  [(set (match_operand:DI 0 "nonimmediate_operand" "")
	(match_operand:DI 1 "general_operand" ""))]
  ""
  "expand_move (operands, DImode);")

(define_expand "movsf"
 [(set (match_operand:SF 0 "nonimmediate_operand" "")
       (match_operand:SF 1 "general_operand" ""))]
  ""
  "expand_move (operands, SFmode);")

(define_expand "movdf"
 [(set (match_operand:DF 0 "nonimmediate_operand" "")
       (match_operand:DF 1 "general_operand" ""))]
  ""
  "expand_move (operands, DFmode);")

(define_expand "movhi"
  [(set (match_operand:HI 0 "nonimmediate_operand" "")
	(match_operand:HI 1 "general_operand" ""))]
  ""
  "expand_move (operands, HImode);")

(define_expand "movqi"
  [(set (match_operand:QI 0 "nonimmediate_operand" "")
	(match_operand:QI 1 "general_operand" ""))]
  ""
  " expand_move (operands, QImode); ")

;; Some define_splits to break up SI/SFmode loads of immediate constants.

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(match_operand:SI 1 "symbolic_or_const_operand" ""))]
  "reload_completed
   /* Always split symbolic operands; split integer constants that are
      too large for a single instruction.  */
   && (GET_CODE (operands[1]) != CONST_INT
       || (INTVAL (operands[1]) < -32768
 	   || INTVAL (operands[1]) >= 65536
	   || (INTVAL (operands[1]) >= 32768 && PREG_P (operands[0]))))"
  [(set (match_dup 0) (high:SI (match_dup 1)))
   (set (match_dup 0) (lo_sum:SI (match_dup 0) (match_dup 1)))]
{
  if (GET_CODE (operands[1]) == CONST_INT
      && split_load_immediate (operands))
    DONE;
  /* ??? Do something about TARGET_LOW_64K.  */
})

(define_split
  [(set (match_operand:SF 0 "register_operand" "")
	(match_operand:SF 1 "immediate_operand" ""))]
  "reload_completed"
  [(set (match_dup 2) (high:SI (match_dup 3)))
   (set (match_dup 2) (lo_sum:SI (match_dup 2) (match_dup 3)))]
{
  long values;

  gcc_assert (GET_CODE (operands[1]) == CONST_DOUBLE);

  REAL_VALUE_TO_TARGET_SINGLE (*CONST_DOUBLE_REAL_VALUE (operands[1]), values);

  operands[2] = gen_rtx_REG (SImode, true_regnum (operands[0]));
  operands[3] = GEN_INT (trunc_int_for_mode (values, SImode));
  if (values >= -32768 && values < 65536)
    {
      emit_move_insn (operands[2], operands[3]);
      DONE;
    }
  if (split_load_immediate (operands + 2))
    DONE;
})

;; Sadly, this can't be a proper named movstrict pattern, since the compiler
;; expects to be able to use registers for operand 1.
;; Note that the asm instruction is defined by the manual to take an unsigned
;; constant, but it doesn't matter to the assembler, and the compiler only
;; deals with sign-extended constants.  Hence "Ksh".
(define_insn "movstricthi_1"
  [(set (strict_low_part (match_operand:HI 0 "register_operand" "+x"))
	(match_operand:HI 1 "immediate_operand" "Ksh"))]
  ""
  "%h0 = %1;"
  [(set_attr "type" "mvi")
   (set_attr "length" "4")])

;; Sign and zero extensions

(define_insn_and_split "extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "=d, d")
	(sign_extend:SI (match_operand:HI 1 "nonimmediate_operand" "d, m")))]
  ""
  "@
   %0 = %h1 (X);
   %0 = W %h1 (X)%!"
  "reload_completed && bfin_dsp_memref_p (operands[1])"
  [(set (match_dup 2) (match_dup 1))
   (set (match_dup 0) (sign_extend:SI (match_dup 2)))]
{
  operands[2] = gen_lowpart (HImode, operands[0]);
}
  [(set_attr "type" "alu0,mcld")])

(define_insn_and_split "zero_extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "=d, d")
	(zero_extend:SI (match_operand:HI 1 "nonimmediate_operand" "d, m")))]
  ""
  "@
   %0 = %h1 (Z);
   %0 = W %h1 (Z)%!"
  "reload_completed && bfin_dsp_memref_p (operands[1])"
  [(set (match_dup 2) (match_dup 1))
   (set (match_dup 0) (zero_extend:SI (match_dup 2)))]
{
  operands[2] = gen_lowpart (HImode, operands[0]);
}
  [(set_attr "type" "alu0,mcld")])

(define_insn "zero_extendbisi2"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(zero_extend:SI (match_operand:BI 1 "nonimmediate_operand" "C")))]
  ""
  "%0 = %1;"
  [(set_attr "type" "compare")])

(define_insn "extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "=d, d")
	(sign_extend:HI (match_operand:QI 1 "nonimmediate_operand" "m, d")))]
  ""
  "@
   %0 = B %1 (X)%!
   %0 = %T1 (X);"
  [(set_attr "type" "mcld,alu0")])

(define_insn "extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=d, d")
	(sign_extend:SI (match_operand:QI 1 "nonimmediate_operand" "m, d")))]
  ""
  "@
   %0 = B %1 (X)%!
   %0 = %T1 (X);"
  [(set_attr "type" "mcld,alu0")])


(define_insn "zero_extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "=d, d")
	(zero_extend:HI (match_operand:QI 1 "nonimmediate_operand" "m, d")))]
  ""
  "@
   %0 = B %1 (Z)%!
   %0 = %T1 (Z);"
  [(set_attr "type" "mcld,alu0")])


(define_insn "zero_extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=d, d")
	(zero_extend:SI (match_operand:QI 1 "nonimmediate_operand" "m, d")))]
  ""
  "@
   %0 = B %1 (Z)%!
   %0 = %T1 (Z);"
  [(set_attr "type" "mcld,alu0")])

;; DImode logical operations

(define_code_iterator any_logical [and ior xor])
(define_code_attr optab [(and "and")
			 (ior "ior")
			 (xor "xor")])
(define_code_attr op [(and "&")
		      (ior "|")
		      (xor "^")])
(define_code_attr high_result [(and "0")
			       (ior "%H1")
			       (xor "%H1")])

;; Keep this pattern around to avoid generating NO_CONFLICT blocks.
(define_expand "<optab>di3"
  [(set (match_operand:DI 0 "register_operand" "=d")
        (any_logical:DI (match_operand:DI 1 "register_operand" "0")
			(match_operand:DI 2 "general_operand" "d")))]
  ""
{
  rtx hi_half[3], lo_half[3];
  enum insn_code icode = CODE_FOR_<optab>si3;
  if (!reg_overlap_mentioned_p (operands[0], operands[1])
      && !reg_overlap_mentioned_p (operands[0], operands[2]))
    emit_clobber (operands[0]);
  split_di (operands, 3, lo_half, hi_half);
  if (!(*insn_data[icode].operand[2].predicate) (lo_half[2], SImode))
    lo_half[2] = force_reg (SImode, lo_half[2]);
  emit_insn (GEN_FCN (icode) (lo_half[0], lo_half[1], lo_half[2]));
  if (!(*insn_data[icode].operand[2].predicate) (hi_half[2], SImode))
    hi_half[2] = force_reg (SImode, hi_half[2]);
  emit_insn (GEN_FCN (icode) (hi_half[0], hi_half[1], hi_half[2]));
  DONE;
})

(define_insn "zero_extendqidi2"
  [(set (match_operand:DI 0 "register_operand" "=d")
        (zero_extend:DI (match_operand:QI 1 "register_operand" "d")))]
  ""
  "%0 = %T1 (Z);\\n\\t%H0 = 0;"
  [(set_attr "length" "4")
   (set_attr "seq_insns" "multi")])

(define_insn "zero_extendhidi2"
  [(set (match_operand:DI 0 "register_operand" "=d")
        (zero_extend:DI (match_operand:HI 1 "register_operand" "d")))]
  ""
  "%0 = %h1 (Z);\\n\\t%H0 = 0;"
  [(set_attr "length" "4")
   (set_attr "seq_insns" "multi")])

(define_insn_and_split "extendsidi2"
  [(set (match_operand:DI 0 "register_operand" "=d")
        (sign_extend:DI (match_operand:SI 1 "register_operand" "d")))]
  ""
  "#"
  "reload_completed"
  [(set (match_dup 3) (match_dup 1))
   (set (match_dup 3) (ashiftrt:SI (match_dup 3) (const_int 31)))]
{
  split_di (operands, 1, operands + 2, operands + 3);
  if (REGNO (operands[0]) != REGNO (operands[1]))
    emit_move_insn (operands[2], operands[1]);
})

(define_insn_and_split "extendqidi2"
  [(set (match_operand:DI 0 "register_operand" "=d")
        (sign_extend:DI (match_operand:QI 1 "register_operand" "d")))]
  ""
  "#"
  "reload_completed"
  [(set (match_dup 2) (sign_extend:SI (match_dup 1)))
   (set (match_dup 3) (sign_extend:SI (match_dup 1)))
   (set (match_dup 3) (ashiftrt:SI (match_dup 3) (const_int 31)))]
{
  split_di (operands, 1, operands + 2, operands + 3);
})

(define_insn_and_split "extendhidi2"
  [(set (match_operand:DI 0 "register_operand" "=d")
        (sign_extend:DI (match_operand:HI 1 "register_operand" "d")))]
  ""
  "#"
  "reload_completed"
  [(set (match_dup 2) (sign_extend:SI (match_dup 1)))
   (set (match_dup 3) (sign_extend:SI (match_dup 1)))
   (set (match_dup 3) (ashiftrt:SI (match_dup 3) (const_int 31)))]
{
  split_di (operands, 1, operands + 2, operands + 3);
})

;; DImode arithmetic operations

(define_insn "add_with_carry"
  [(set (match_operand:SI 0 "register_operand" "=d,d")
        (plus:SI (match_operand:SI 1 "register_operand" "%0,d")
                 (match_operand:SI 2 "nonmemory_operand" "Ks7,d")))
   (set (match_operand:BI 3 "register_operand" "=C,C")
	(ltu:BI (not:SI (match_dup 1)) (match_dup 2)))]
  ""
  "@
   %0 += %2; cc = ac0;
   %0 = %1 + %2; cc = ac0;"
  [(set_attr "type" "alu0")
   (set_attr "length" "4")
   (set_attr "seq_insns" "multi")])

(define_insn "sub_with_carry"
  [(set (match_operand:SI 0 "register_operand" "=d")
        (minus:SI (match_operand:SI 1 "register_operand" "%d")
		  (match_operand:SI 2 "nonmemory_operand" "d")))
   (set (match_operand:BI 3 "register_operand" "=C")
	(leu:BI (match_dup 2) (match_dup 1)))]
  ""
  "%0 = %1 - %2; cc = ac0;"
  [(set_attr "type" "alu0")
   (set_attr "length" "4")
   (set_attr "seq_insns" "multi")])

(define_expand "adddi3"
  [(set (match_operand:DI 0 "register_operand" "")
        (plus:DI (match_operand:DI 1 "register_operand" "")
                 (match_operand:DI 2 "nonmemory_operand" "")))
   (clobber (match_scratch:SI 3 ""))
   (clobber (reg:CC 34))]
  ""
{
  rtx xops[8];
  xops[0] = gen_lowpart (SImode, operands[0]);
  xops[1] = simplify_gen_subreg (SImode, operands[0], DImode, 4);
  xops[2] = gen_lowpart (SImode, operands[1]);
  xops[3] = simplify_gen_subreg (SImode, operands[1], DImode, 4);
  xops[4] = gen_lowpart (SImode, operands[2]);
  xops[5] = simplify_gen_subreg (SImode, operands[2], DImode, 4);
  xops[6] = gen_reg_rtx (SImode);
  xops[7] = gen_rtx_REG (BImode, REG_CC);
  if (!register_operand (xops[4], SImode)
      && (GET_CODE (xops[4]) != CONST_INT
          || !satisfies_constraint_Ks7 (xops[4])))
    xops[4] = force_reg (SImode, xops[4]);
  if (!reg_overlap_mentioned_p (operands[0], operands[1])
      && !reg_overlap_mentioned_p (operands[0], operands[2]))
    emit_clobber (operands[0]);
  emit_insn (gen_add_with_carry (xops[0], xops[2], xops[4], xops[7]));
  emit_insn (gen_movbisi (xops[6], xops[7]));
  if (!register_operand (xops[5], SImode)
      && (GET_CODE (xops[5]) != CONST_INT
          || !satisfies_constraint_Ks7 (xops[5])))
    xops[5] = force_reg (SImode, xops[5]);
  if (xops[5] != const0_rtx)
    emit_insn (gen_addsi3 (xops[1], xops[3], xops[5]));
  else
    emit_move_insn (xops[1], xops[3]);
  emit_insn (gen_addsi3 (xops[1], xops[1], xops[6]));
  DONE;
})

(define_expand "subdi3"
  [(set (match_operand:DI 0 "register_operand" "")
        (minus:DI (match_operand:DI 1 "register_operand" "")
                  (match_operand:DI 2 "register_operand" "")))
   (clobber (reg:CC 34))]
  ""
{
  rtx xops[8];
  xops[0] = gen_lowpart (SImode, operands[0]);
  xops[1] = simplify_gen_subreg (SImode, operands[0], DImode, 4);
  xops[2] = gen_lowpart (SImode, operands[1]);
  xops[3] = simplify_gen_subreg (SImode, operands[1], DImode, 4);
  xops[4] = gen_lowpart (SImode, operands[2]);
  xops[5] = simplify_gen_subreg (SImode, operands[2], DImode, 4);
  xops[6] = gen_reg_rtx (SImode);
  xops[7] = gen_rtx_REG (BImode, REG_CC);
  if (!reg_overlap_mentioned_p (operands[0], operands[1])
      && !reg_overlap_mentioned_p (operands[0], operands[2]))
    emit_clobber (operands[0]);
  emit_insn (gen_sub_with_carry (xops[0], xops[2], xops[4], xops[7]));
  emit_insn (gen_notbi (xops[7], xops[7]));
  emit_insn (gen_movbisi (xops[6], xops[7]));
  emit_insn (gen_subsi3 (xops[1], xops[3], xops[5]));
  emit_insn (gen_subsi3 (xops[1], xops[1], xops[6]));
  DONE;
})

;; Combined shift/add instructions

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=a,d")
	(ashift:SI (plus:SI (match_operand:SI 1 "register_operand" "%0,0")
		            (match_operand:SI 2 "register_operand" "a,d"))
		   (match_operand:SI 3 "pos_scale_operand" "P1P2,P1P2")))]
  ""
  "%0 = (%0 + %2) << %3;" /* "shadd %0,%2,%3;" */
  [(set_attr "type" "alu0")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=a")
	(plus:SI (match_operand:SI 1 "register_operand" "a")
		 (mult:SI (match_operand:SI 2 "register_operand" "a")
			  (match_operand:SI 3 "scale_by_operand" "i"))))]
  ""
  "%0 = %1 + (%2 << %X3);"
  [(set_attr "type" "alu0")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=a")
	(plus:SI (match_operand:SI 1 "register_operand" "a")
		 (ashift:SI (match_operand:SI 2 "register_operand" "a")
			    (match_operand:SI 3 "pos_scale_operand" "i"))))]
  ""
  "%0 = %1 + (%2 << %3);"
  [(set_attr "type" "alu0")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=a")
	(plus:SI (mult:SI (match_operand:SI 1 "register_operand" "a")
			  (match_operand:SI 2 "scale_by_operand" "i"))
		 (match_operand:SI 3 "register_operand" "a")))]
  ""
  "%0 = %3 + (%1 << %X2);"
  [(set_attr "type" "alu0")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=a")
	(plus:SI (ashift:SI (match_operand:SI 1 "register_operand" "a")
			    (match_operand:SI 2 "pos_scale_operand" "i"))
		 (match_operand:SI 3 "register_operand" "a")))]
  ""
  "%0 = %3 + (%1 << %2);"
  [(set_attr "type" "alu0")])

(define_insn "mulhisi3"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(mult:SI (sign_extend:SI (match_operand:HI 1 "register_operand" "%d"))
		 (sign_extend:SI (match_operand:HI 2 "register_operand" "d"))))]
  ""
  "%0 = %h1 * %h2 (IS)%!"
  [(set_attr "type" "dsp32")])

(define_insn "umulhisi3"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(mult:SI (zero_extend:SI (match_operand:HI 1 "register_operand" "%d"))
		 (zero_extend:SI (match_operand:HI 2 "register_operand" "d"))))]
  ""
  "%0 = %h1 * %h2 (FU)%!"
  [(set_attr "type" "dsp32")])

(define_insn "usmulhisi3"
  [(set (match_operand:SI 0 "register_operand" "=W")
	(mult:SI (zero_extend:SI (match_operand:HI 1 "register_operand" "W"))
		 (sign_extend:SI (match_operand:HI 2 "register_operand" "W"))))]
  ""
  "%0 = %h2 * %h1 (IS,M)%!"
  [(set_attr "type" "dsp32")])

;; The alternative involving IREGS requires that the corresponding L register
;; is zero.

(define_insn "addsi3"
  [(set (match_operand:SI 0 "register_operand" "=ad,a,d,b")
       (plus:SI (match_operand:SI 1 "register_operand" "%0, a,d,0")
                (match_operand:SI 2 "reg_or_7bit_operand" "Ks7, a,d,fP2P4")))]
  ""
  "@
   %0 += %2;
   %0 = %1 + %2;
   %0 = %1 + %2;
   %0 += %2;"
  [(set_attr "type" "alu0")
   (set_attr "length" "2,2,2,2")])

(define_insn "ssaddsi3"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(ss_plus:SI (match_operand:SI 1 "register_operand" "d")
		    (match_operand:SI 2 "register_operand" "d")))]
  ""
  "%0 = %1 + %2 (S)%!"
  [(set_attr "type" "dsp32")])

(define_insn "subsi3"
  [(set (match_operand:SI 0 "register_operand" "=da,d,a")
	(minus:SI (match_operand:SI 1 "register_operand" "0,d,0")
		  (match_operand:SI 2 "reg_or_neg7bit_operand" "KN7,d,a")))]
  ""
{
  static const char *const strings_subsi3[] = {
    "%0 += -%2;",
    "%0 = %1 - %2;",
    "%0 -= %2;",
  };

  if (CONSTANT_P (operands[2]) && INTVAL (operands[2]) < 0) {
     rtx tmp_op = operands[2];
     operands[2] = GEN_INT (-INTVAL (operands[2]));
     output_asm_insn ("%0 += %2;", operands);
     operands[2] = tmp_op;
     return "";
  }

  return strings_subsi3[which_alternative];
}
  [(set_attr "type" "alu0")])

(define_insn "sssubsi3"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(ss_minus:SI (match_operand:SI 1 "register_operand" "d")
		     (match_operand:SI 2 "register_operand" "d")))]
  ""
  "%0 = %1 - %2 (S)%!"
  [(set_attr "type" "dsp32")])

;; Accumulator addition

(define_insn "addpdi3"
  [(set (match_operand:PDI 0 "register_operand" "=A")
        (ss_plus:PDI (match_operand:PDI 1 "register_operand" "%0")
		     (match_operand:PDI 2 "nonmemory_operand" "B")))]
  ""
  "A0 += A1%!"
  [(set_attr "type" "dsp32")])

(define_insn "sum_of_accumulators"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(ss_truncate:SI
	 (ss_plus:PDI (match_operand:PDI 2 "register_operand" "1")
		      (match_operand:PDI 3 "register_operand" "B"))))
   (set (match_operand:PDI 1 "register_operand" "=A")
	 (ss_plus:PDI (match_dup 2) (match_dup 3)))]
  ""
  "%0 = (A0 += A1)%!"
  [(set_attr "type" "dsp32")])

(define_insn "us_truncpdisi2"
  [(set (match_operand:SI 0 "register_operand" "=D,W")
	(us_truncate:SI (match_operand:PDI 1 "register_operand" "A,B")))]
  ""
  "%0 = %1 (FU)%!"
  [(set_attr "type" "dsp32")])

;; Bit test instructions

(define_insn "*not_bittst"
 [(set (match_operand:BI 0 "register_operand" "=C")
       (eq:BI (zero_extract:SI (match_operand:SI 1 "register_operand" "d")
			       (const_int 1)
			       (match_operand:SI 2 "immediate_operand" "Ku5"))
	      (const_int 0)))]
 ""
 "cc = !BITTST (%1,%2);"
  [(set_attr "type" "alu0")])

(define_insn "*bittst"
 [(set (match_operand:BI 0 "register_operand" "=C")
       (ne:BI (zero_extract:SI (match_operand:SI 1 "register_operand" "d")
			       (const_int 1)
			       (match_operand:SI 2 "immediate_operand" "Ku5"))
		(const_int 0)))]
 ""
 "cc = BITTST (%1,%2);"
  [(set_attr "type" "alu0")])

(define_insn_and_split "*bit_extract"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(zero_extract:SI (match_operand:SI 1 "register_operand" "d")
			 (const_int 1)
			 (match_operand:SI 2 "immediate_operand" "Ku5")))
   (clobber (reg:BI REG_CC))]
  ""
  "#"
  ""
  [(set (reg:BI REG_CC)
	(ne:BI (zero_extract:SI (match_dup 1) (const_int 1) (match_dup 2))
	       (const_int 0)))
   (set (match_dup 0)
	(ne:SI (reg:BI REG_CC) (const_int 0)))])

(define_insn_and_split "*not_bit_extract"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(zero_extract:SI (not:SI (match_operand:SI 1 "register_operand" "d"))
			 (const_int 1)
			 (match_operand:SI 2 "immediate_operand" "Ku5")))
   (clobber (reg:BI REG_CC))]
  ""
  "#"
  ""
  [(set (reg:BI REG_CC)
	(eq:BI (zero_extract:SI (match_dup 1) (const_int 1) (match_dup 2))
	       (const_int 0)))
   (set (match_dup 0)
	(ne:SI (reg:BI REG_CC) (const_int 0)))])

(define_insn "*andsi_insn"
  [(set (match_operand:SI 0 "register_operand" "=d,d,d,d")
	(and:SI (match_operand:SI 1 "register_operand" "%0,d,d,d")
		(match_operand:SI 2 "rhs_andsi3_operand" "L,M1,M2,d")))]
  ""
  "@
   BITCLR (%0,%Y2);
   %0 = %T1 (Z);
   %0 = %h1 (Z);
   %0 = %1 & %2;"
  [(set_attr "type" "alu0")])

(define_expand "andsi3"
  [(set (match_operand:SI 0 "register_operand" "")
	(and:SI (match_operand:SI 1 "register_operand" "")
		(match_operand:SI 2 "general_operand" "")))]
  ""
{
  if (highbits_operand (operands[2], SImode))
    {
      operands[2] = GEN_INT (exact_log2 (-INTVAL (operands[2])));
      emit_insn (gen_ashrsi3 (operands[0], operands[1], operands[2]));
      emit_insn (gen_ashlsi3 (operands[0], operands[0], operands[2]));
      DONE;
    }
  if (! rhs_andsi3_operand (operands[2], SImode))
    operands[2] = force_reg (SImode, operands[2]);
})

(define_insn "iorsi3"
  [(set (match_operand:SI 0 "register_operand" "=d,d")
	(ior:SI (match_operand:SI 1 "register_operand" "%0,d")
		(match_operand:SI 2 "regorlog2_operand" "J,d")))]
  ""
  "@
   BITSET (%0, %X2);
   %0 = %1 | %2;"
  [(set_attr "type" "alu0")])

(define_insn "xorsi3"
  [(set (match_operand:SI 0 "register_operand" "=d,d")
	(xor:SI (match_operand:SI 1 "register_operand" "%0,d")
		  (match_operand:SI 2 "regorlog2_operand" "J,d")))]
  ""
  "@
   BITTGL (%0, %X2);
   %0 = %1 ^ %2;"
  [(set_attr "type" "alu0")])

(define_insn "ones"
  [(set (match_operand:HI 0 "register_operand" "=d")
	(truncate:HI
	 (popcount:SI (match_operand:SI 1 "register_operand" "d"))))]
  ""
  "%h0 = ONES %1;"
  [(set_attr "type" "alu0")
   (set_attr "length" "4")])

(define_expand "popcountsi2"
  [(set (match_dup 2)
	(truncate:HI (popcount:SI (match_operand:SI 1 "register_operand" ""))))
   (set (match_operand:SI 0 "register_operand")
	(zero_extend:SI (match_dup 2)))]
  ""
{
  operands[2] = gen_reg_rtx (HImode);
})

(define_expand "popcounthi2"
  [(set (match_dup 2)
	(zero_extend:SI (match_operand:HI 1 "register_operand" "")))
   (set (match_operand:HI 0 "register_operand") 
	(truncate:HI (popcount:SI (match_dup 2))))]
  ""
{
  operands[2] = gen_reg_rtx (SImode);
})

(define_insn "smaxsi3"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(smax:SI (match_operand:SI 1 "register_operand" "d")
		 (match_operand:SI 2 "register_operand" "d")))]
  ""
  "%0 = max(%1,%2)%!"
  [(set_attr "type" "dsp32")])

(define_insn "sminsi3"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(smin:SI (match_operand:SI 1 "register_operand" "d")
		 (match_operand:SI 2 "register_operand" "d")))]
  ""
  "%0 = min(%1,%2)%!"
  [(set_attr "type" "dsp32")])

(define_insn "abssi2"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(abs:SI (match_operand:SI 1 "register_operand" "d")))]
  ""
  "%0 = abs %1%!"
  [(set_attr "type" "dsp32")])

(define_insn "ssabssi2"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(ss_abs:SI (match_operand:SI 1 "register_operand" "d")))]
  ""
  "%0 = abs %1%!"
  [(set_attr "type" "dsp32")])

(define_insn "negsi2"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(neg:SI (match_operand:SI 1 "register_operand" "d")))]
  ""
  "%0 = -%1;"
  [(set_attr "type" "alu0")])

(define_insn "ssnegsi2"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(ss_neg:SI (match_operand:SI 1 "register_operand" "d")))]
  ""
  "%0 = -%1 (S)%!"
  [(set_attr "type" "dsp32")])

(define_insn "one_cmplsi2"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(not:SI (match_operand:SI 1 "register_operand" "d")))]
  ""
  "%0 = ~%1;"
  [(set_attr "type" "alu0")])

(define_expand "clrsbsi2"
  [(set (match_dup 2)
	(truncate:HI (clrsb:SI (match_operand:SI 1 "register_operand" "d"))))
   (set (match_operand:SI 0 "register_operand")
	(zero_extend:SI (match_dup 2)))]
  ""
{
  operands[2] = gen_reg_rtx (HImode);
})

(define_insn "signbitssi2"
  [(set (match_operand:HI 0 "register_operand" "=d")
	(truncate:HI (clrsb:SI (match_operand:SI 1 "register_operand" "d"))))]
  ""
  "%h0 = signbits %1%!"
  [(set_attr "type" "dsp32")])

(define_insn "ssroundsi2"
  [(set (match_operand:HI 0 "register_operand" "=d")
	(truncate:HI
	 (lshiftrt:SI (ss_plus:SI (match_operand:SI 1 "register_operand" "d")
				  (const_int 32768))
		      (const_int 16))))]
  ""
  "%h0 = %1 (RND)%!"
  [(set_attr "type" "dsp32")])

(define_insn "smaxhi3"
  [(set (match_operand:HI 0 "register_operand" "=d")
	(smax:HI (match_operand:HI 1 "register_operand" "d")
		 (match_operand:HI 2 "register_operand" "d")))]
  ""
  "%0 = max(%1,%2) (V)%!"
  [(set_attr "type" "dsp32")])

(define_insn "sminhi3"
  [(set (match_operand:HI 0 "register_operand" "=d")
	(smin:HI (match_operand:HI 1 "register_operand" "d")
		 (match_operand:HI 2 "register_operand" "d")))]
  ""
  "%0 = min(%1,%2) (V)%!"
  [(set_attr "type" "dsp32")])

(define_insn "abshi2"
  [(set (match_operand:HI 0 "register_operand" "=d")
	(abs:HI (match_operand:HI 1 "register_operand" "d")))]
  ""
  "%0 = abs %1 (V)%!"
  [(set_attr "type" "dsp32")])

(define_insn "neghi2"
  [(set (match_operand:HI 0 "register_operand" "=d")
	(neg:HI (match_operand:HI 1 "register_operand" "d")))]
  ""
  "%0 = -%1;"
  [(set_attr "type" "alu0")])

(define_insn "ssneghi2"
  [(set (match_operand:HI 0 "register_operand" "=d")
	(ss_neg:HI (match_operand:HI 1 "register_operand" "d")))]
  ""
  "%0 = -%1 (V)%!"
  [(set_attr "type" "dsp32")])

(define_insn "clrsbhi2"
  [(set (match_operand:HI 0 "register_operand" "=d")
	(clrsb:HI (match_operand:HI 1 "register_operand" "d")))]
  ""
  "%h0 = signbits %h1%!"
  [(set_attr "type" "dsp32")])

(define_insn "mulsi3"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(mult:SI (match_operand:SI 1 "register_operand" "%0")
		 (match_operand:SI 2 "register_operand" "d")))]
  ""
  "%0 *= %2;"
  [(set_attr "type" "mult")])

(define_expand "umulsi3_highpart"
  [(parallel
    [(set (match_operand:SI 0 "register_operand" "")
	  (truncate:SI
	   (lshiftrt:DI
	    (mult:DI (zero_extend:DI
		      (match_operand:SI 1 "nonimmediate_operand" ""))
		     (zero_extend:DI
		      (match_operand:SI 2 "register_operand" "")))
	    (const_int 32))))
     (clobber (reg:PDI REG_A0))
     (clobber (reg:PDI REG_A1))])]
  ""
{
  if (!optimize_size)
    {
      rtx a1reg = gen_rtx_REG (PDImode, REG_A1);
      rtx a0reg = gen_rtx_REG (PDImode, REG_A0);
      emit_insn (gen_flag_macinit1hi (a1reg,
				      gen_lowpart (HImode, operands[1]),
				      gen_lowpart (HImode, operands[2]),
				      GEN_INT (MACFLAG_FU)));
      emit_insn (gen_lshrpdi3 (a1reg, a1reg, GEN_INT (16)));
      emit_insn (gen_flag_mul_macv2hi_parts_acconly (a0reg, a1reg,
						     gen_lowpart (V2HImode, operands[1]),
						     gen_lowpart (V2HImode, operands[2]),
						     const1_rtx, const1_rtx,
						     const1_rtx, const0_rtx, a1reg,
						     const0_rtx, GEN_INT (MACFLAG_FU),
						     GEN_INT (MACFLAG_FU)));
      emit_insn (gen_flag_machi_parts_acconly (a1reg,
					       gen_lowpart (V2HImode, operands[2]),
					       gen_lowpart (V2HImode, operands[1]),
					       const1_rtx, const0_rtx,
					       a1reg, const0_rtx, GEN_INT (MACFLAG_FU)));
      emit_insn (gen_lshrpdi3 (a1reg, a1reg, GEN_INT (16)));
      emit_insn (gen_addpdi3 (a0reg, a0reg, a1reg));
      emit_insn (gen_us_truncpdisi2 (operands[0], a0reg));
    }
  else
    {
      rtx umulsi3_highpart_libfunc
	= init_one_libfunc ("__umulsi3_highpart");

      emit_library_call_value (umulsi3_highpart_libfunc,
			       operands[0], LCT_NORMAL, SImode,
			       operands[1], SImode, operands[2], SImode);
    }
  DONE;
})

(define_expand "smulsi3_highpart"
  [(parallel
    [(set (match_operand:SI 0 "register_operand" "")
	  (truncate:SI
	   (lshiftrt:DI
	    (mult:DI (sign_extend:DI
		      (match_operand:SI 1 "nonimmediate_operand" ""))
		     (sign_extend:DI
		      (match_operand:SI 2 "register_operand" "")))
	    (const_int 32))))
     (clobber (reg:PDI REG_A0))
     (clobber (reg:PDI REG_A1))])]
  ""
{
  if (!optimize_size)
    {
      rtx a1reg = gen_rtx_REG (PDImode, REG_A1);
      rtx a0reg = gen_rtx_REG (PDImode, REG_A0);
      emit_insn (gen_flag_macinit1hi (a1reg,
				      gen_lowpart (HImode, operands[1]),
				      gen_lowpart (HImode, operands[2]),
				      GEN_INT (MACFLAG_FU)));
      emit_insn (gen_lshrpdi3 (a1reg, a1reg, GEN_INT (16)));
      emit_insn (gen_flag_mul_macv2hi_parts_acconly (a0reg, a1reg,
						     gen_lowpart (V2HImode, operands[1]),
						     gen_lowpart (V2HImode, operands[2]),
						     const1_rtx, const1_rtx,
						     const1_rtx, const0_rtx, a1reg,
						     const0_rtx, GEN_INT (MACFLAG_IS),
						     GEN_INT (MACFLAG_IS_M)));
      emit_insn (gen_flag_machi_parts_acconly (a1reg,
					       gen_lowpart (V2HImode, operands[2]),
					       gen_lowpart (V2HImode, operands[1]),
					       const1_rtx, const0_rtx,
					       a1reg, const0_rtx, GEN_INT (MACFLAG_IS_M)));
      emit_insn (gen_ashrpdi3 (a1reg, a1reg, GEN_INT (16)));
      emit_insn (gen_sum_of_accumulators (operands[0], a0reg, a0reg, a1reg));
    }
  else
    {
      rtx smulsi3_highpart_libfunc
	= init_one_libfunc ("__smulsi3_highpart");

      emit_library_call_value (smulsi3_highpart_libfunc,
			       operands[0], LCT_NORMAL, SImode,
			       operands[1], SImode, operands[2], SImode);
    }
  DONE;
})

(define_expand "ashlsi3"
  [(set (match_operand:SI 0 "register_operand" "")
        (ashift:SI (match_operand:SI 1 "register_operand" "")
                   (match_operand:SI 2 "nonmemory_operand" "")))]
  ""
{
 if (GET_CODE (operands[2]) == CONST_INT
     && ((unsigned HOST_WIDE_INT) INTVAL (operands[2])) > 31)
   {
     emit_insn (gen_movsi (operands[0], const0_rtx));
     DONE;
   }
})

(define_insn_and_split "*ashlsi3_insn"
  [(set (match_operand:SI 0 "register_operand" "=d,d,a,a,a")
	(ashift:SI (match_operand:SI 1 "register_operand" "0,d,a,a,a")
		   (match_operand:SI 2 "nonmemory_operand" "dKu5,Ku5,P1,P2,?P3P4")))]
  ""
  "@
   %0 <<= %2;
   %0 = %1 << %2%!
   %0 = %1 + %1;
   %0 = %1 << %2;
   #"
  "PREG_P (operands[0]) && INTVAL (operands[2]) > 2"
  [(set (match_dup 0) (ashift:SI (match_dup 1) (const_int 2)))
   (set (match_dup 0) (ashift:SI (match_dup 0) (match_dup 3)))]
  "operands[3] = GEN_INT (INTVAL (operands[2]) - 2);"
  [(set_attr "type" "shft,dsp32shiftimm,shft,shft,*")])

(define_insn "ashrsi3"
  [(set (match_operand:SI 0 "register_operand" "=d,d")
	(ashiftrt:SI (match_operand:SI 1 "register_operand" "0,d")
		     (match_operand:SI 2 "nonmemory_operand" "dKu5,Ku5")))]
  ""
  "@
   %0 >>>= %2;
   %0 = %1 >>> %2%!"
  [(set_attr "type" "shft,dsp32shiftimm")])

(define_insn "rotl16"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(rotate:SI (match_operand:SI 1 "register_operand" "d")
		   (const_int 16)))]
  ""
  "%0 = PACK (%h1, %d1)%!"
  [(set_attr "type" "dsp32")])

(define_expand "rotlsi3"
  [(set (match_operand:SI 0 "register_operand" "")
	(rotate:SI (match_operand:SI 1 "register_operand" "")
		   (match_operand:SI 2 "const_int_operand" "")))]
  ""
{
  if (GET_CODE (operands[2]) != CONST_INT || INTVAL (operands[2]) != 16)
    FAIL;
})

(define_expand "rotrsi3"
  [(set (match_operand:SI 0 "register_operand" "")
	(rotatert:SI (match_operand:SI 1 "register_operand" "")
		     (match_operand:SI 2 "const_int_operand" "")))]
  ""
{
  if (GET_CODE (operands[2]) != CONST_INT || INTVAL (operands[2]) != 16)
    FAIL;
  emit_insn (gen_rotl16 (operands[0], operands[1]));
  DONE;
})


(define_insn "ror_one"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(ior:SI (lshiftrt:SI (match_operand:SI 1 "register_operand" "d") (const_int 1))
		(ashift:SI (zero_extend:SI (reg:BI REG_CC)) (const_int 31))))
   (set (reg:BI REG_CC)
	(zero_extract:BI (match_dup 1) (const_int 1) (const_int 0)))]
  ""
  "%0 = ROT %1 BY -1%!"
  [(set_attr "type" "dsp32shiftimm")])

(define_insn "rol_one"
  [(set (match_operand:SI 0 "register_operand" "+d")
	(ior:SI (ashift:SI (match_operand:SI 1 "register_operand" "d") (const_int 1))
		(zero_extend:SI (reg:BI REG_CC))))
   (set (reg:BI REG_CC)
	(zero_extract:BI (match_dup 1) (const_int 1) (const_int 31)))]
  ""
  "%0 = ROT %1 BY 1%!"
  [(set_attr "type" "dsp32shiftimm")])

(define_expand "lshrdi3"
  [(set (match_operand:DI 0 "register_operand" "")
	(lshiftrt:DI (match_operand:DI 1 "register_operand" "")
		     (match_operand:DI 2 "general_operand" "")))]
  ""
{
  rtx lo_half[2], hi_half[2];
      
  if (operands[2] != const1_rtx)
    FAIL;
  if (! rtx_equal_p (operands[0], operands[1]))
    emit_move_insn (operands[0], operands[1]);

  split_di (operands, 2, lo_half, hi_half);

  emit_move_insn (bfin_cc_rtx, const0_rtx);
  emit_insn (gen_ror_one (hi_half[0], hi_half[0]));
  emit_insn (gen_ror_one (lo_half[0], lo_half[0]));
  DONE;
})

(define_expand "ashrdi3"
  [(set (match_operand:DI 0 "register_operand" "")
	(ashiftrt:DI (match_operand:DI 1 "register_operand" "")
		     (match_operand:DI 2 "general_operand" "")))]
  ""
{
  rtx lo_half[2], hi_half[2];
      
  if (operands[2] != const1_rtx)
    FAIL;
  if (! rtx_equal_p (operands[0], operands[1]))
    emit_move_insn (operands[0], operands[1]);

  split_di (operands, 2, lo_half, hi_half);

  emit_insn (gen_compare_lt (gen_rtx_REG (BImode, REG_CC),
			     hi_half[1], const0_rtx));
  emit_insn (gen_ror_one (hi_half[0], hi_half[0]));
  emit_insn (gen_ror_one (lo_half[0], lo_half[0]));
  DONE;
})

(define_expand "ashldi3"
  [(set (match_operand:DI 0 "register_operand" "")
	(ashift:DI (match_operand:DI 1 "register_operand" "")
		   (match_operand:DI 2 "general_operand" "")))]
  ""
{
  rtx lo_half[2], hi_half[2];
      
  if (operands[2] != const1_rtx)
    FAIL;
  if (! rtx_equal_p (operands[0], operands[1]))
    emit_move_insn (operands[0], operands[1]);

  split_di (operands, 2, lo_half, hi_half);

  emit_move_insn (bfin_cc_rtx, const0_rtx);
  emit_insn (gen_rol_one (lo_half[0], lo_half[0]));
  emit_insn (gen_rol_one (hi_half[0], hi_half[0]));
  DONE;
})

(define_insn "lshrsi3"
  [(set (match_operand:SI 0 "register_operand" "=d,d,a")
	(lshiftrt:SI (match_operand:SI 1 "register_operand" "0,d,a")
		     (match_operand:SI 2 "nonmemory_operand" "dKu5,Ku5,P1P2")))]
  ""
  "@
   %0 >>= %2;
   %0 = %1 >> %2%!
   %0 = %1 >> %2;"
  [(set_attr "type" "shft,dsp32shiftimm,shft")])

(define_insn "lshrpdi3"
  [(set (match_operand:PDI 0 "register_operand" "=e")
	(lshiftrt:PDI (match_operand:PDI 1 "register_operand" "0")
		      (match_operand:SI 2 "nonmemory_operand" "Ku5")))]
  ""
  "%0 = %1 >> %2%!"
  [(set_attr "type" "dsp32shiftimm")])

(define_insn "ashrpdi3"
  [(set (match_operand:PDI 0 "register_operand" "=e")
	(ashiftrt:PDI (match_operand:PDI 1 "register_operand" "0")
		      (match_operand:SI 2 "nonmemory_operand" "Ku5")))]
  ""
  "%0 = %1 >>> %2%!"
  [(set_attr "type" "dsp32shiftimm")])

;; A pattern to reload the equivalent of
;;   (set (Dreg) (plus (FP) (large_constant)))
;; or
;;   (set (dagreg) (plus (FP) (arbitrary_constant))) 
;; using a scratch register
(define_expand "reload_insi"
  [(parallel [(set (match_operand:SI 0 "register_operand" "=w")
                   (match_operand:SI 1 "fp_plus_const_operand" ""))
              (clobber (match_operand:SI 2 "register_operand" "=&a"))])]
  ""
{
  rtx fp_op = XEXP (operands[1], 0);
  rtx const_op = XEXP (operands[1], 1);
  rtx primary = operands[0];
  rtx scratch = operands[2];

  emit_move_insn (scratch, const_op);
  emit_insn (gen_addsi3 (scratch, scratch, fp_op));
  emit_move_insn (primary, scratch);
  DONE;
})

(define_mode_iterator AREG [PDI V2PDI])

(define_insn "reload_in<mode>"
  [(set (match_operand:AREG 0 "register_operand" "=e")
	(match_operand:AREG 1 "memory_operand" "m"))
   (clobber (match_operand:SI 2 "register_operand" "=d"))]
  ""
{
  rtx xops[4];
  xops[0] = operands[0];
  xops[1] = operands[2];
  split_di (operands + 1, 1, xops + 2, xops + 3);
  output_asm_insn ("%1 = %2;", xops);
  output_asm_insn ("%w0 = %1;", xops);
  output_asm_insn ("%1 = %3;", xops);
  output_asm_insn ("%x0 = %1;", xops);
  return "";
}
 [(set_attr "seq_insns" "multi")
  (set_attr "type" "mcld")
  (set_attr "length" "12")])

(define_insn "reload_out<mode>"
  [(set (match_operand:AREG 0 "memory_operand" "=m")
	(match_operand:AREG 1 "register_operand" "e"))
   (clobber (match_operand:SI 2 "register_operand" "=d"))]
  ""
{
  rtx xops[4];
  xops[0] = operands[1];
  xops[1] = operands[2];
  split_di (operands, 1, xops + 2, xops + 3);
  output_asm_insn ("%1 = %w0;", xops);
  output_asm_insn ("%2 = %1;", xops);
  output_asm_insn ("%1 = %x0;", xops);
  output_asm_insn ("%3 = %1;", xops);
  return "";
}
 [(set_attr "seq_insns" "multi")
  (set_attr "type" "mcld")
  (set_attr "length" "12")])

;; Jump instructions

(define_insn "jump"
  [(set (pc)
	(label_ref (match_operand 0 "" "")))]
  ""
{
  if (get_attr_length (insn) == 2)
    return "jump.s %0;";
  else
    return "jump.l %0;";
}
  [(set_attr "type" "br")])

(define_insn "indirect_jump"
  [(set (pc)
	(match_operand:SI 0 "register_operand" "a"))]
  ""
  "jump (%0);"
  [(set_attr "type" "misc")])

(define_expand "tablejump"
  [(parallel [(set (pc) (match_operand:SI 0 "register_operand" "a"))
              (use (label_ref (match_operand 1 "" "")))])]
  ""
{
  /* In PIC mode, the table entries are stored PC relative.
     Convert the relative address to an absolute address.  */
  if (flag_pic)
    {
      rtx op1 = gen_rtx_LABEL_REF (Pmode, operands[1]);

      operands[0] = expand_simple_binop (Pmode, PLUS, operands[0],
					 op1, NULL_RTX, 0, OPTAB_DIRECT);
    }
})

(define_insn "*tablejump_internal"
  [(set (pc) (match_operand:SI 0 "register_operand" "a"))
   (use (label_ref (match_operand 1 "" "")))]
  ""
  "jump (%0);"
  [(set_attr "type" "misc")])

;;  Hardware loop

; operand 0 is the loop count pseudo register
; operand 1 is the label to jump to at the top of the loop
(define_expand "doloop_end"
  [(parallel [(set (pc) (if_then_else
			  (ne (match_operand:SI 0 "" "")
			      (const_int 1))
			  (label_ref (match_operand 1 "" ""))
			  (pc)))
	      (set (match_dup 0)
		   (plus:SI (match_dup 0)
			    (const_int -1)))
	      (unspec [(const_int 0)] UNSPEC_LSETUP_END)
	      (clobber (match_dup 2))
	      (clobber (reg:BI REG_CC))])] ; match_scratch
  ""
{
  /* The loop optimizer doesn't check the predicates... */
  if (GET_MODE (operands[0]) != SImode)
    FAIL;
  bfin_hardware_loop ();
  operands[2] = gen_rtx_SCRATCH (SImode);
})

(define_insn "loop_end"
  [(set (pc)
	(if_then_else (ne (match_operand:SI 2 "nonimmediate_operand" "0,0,0")
			  (const_int 1))
		      (label_ref (match_operand 1 "" ""))
		      (pc)))
   (set (match_operand:SI 0 "nonimmediate_operand" "=a*d,*b*v*f,m")
	(plus (match_dup 2)
	      (const_int -1)))
   (unspec [(const_int 0)] UNSPEC_LSETUP_END)
   (clobber (match_scratch:SI 3 "=X,&r,&r"))
   (clobber (reg:BI REG_CC))]
  ""
  "@
   /* loop end %0 %l1 */
   #
   #"
  [(set_attr "length" "6,10,14")])

(define_split
  [(set (pc)
	(if_then_else (ne (match_operand:SI 0 "nondp_reg_or_memory_operand")
			  (const_int 1))
		      (label_ref (match_operand 1 ""))
		      (pc)))
   (set (match_dup 0)
	(plus (match_dup 0)
	      (const_int -1)))
   (unspec [(const_int 0)] UNSPEC_LSETUP_END)
   (clobber (match_scratch:SI 2))
   (clobber (reg:BI REG_CC))]
  "memory_operand (operands[0], SImode) || splitting_loops"
  [(set (match_dup 2) (match_dup 0))
   (set (match_dup 2) (plus:SI (match_dup 2) (const_int -1)))
   (set (match_dup 0) (match_dup 2))
   (set (reg:BI REG_CC) (eq:BI (match_dup 2) (const_int 0)))
   (set (pc)
	(if_then_else (eq (reg:BI REG_CC)
			  (const_int 0))
		      (label_ref (match_dup 1))
		      (pc)))]
  "")

(define_insn "lsetup_with_autoinit"
  [(set (match_operand:SI 0 "lt_register_operand" "=t")
	(label_ref (match_operand 1 "" "")))
   (set (match_operand:SI 2 "lb_register_operand" "=u")
	(label_ref (match_operand 3 "" "")))
   (set (match_operand:SI 4 "lc_register_operand" "=k")
	(match_operand:SI 5 "register_operand" "a"))]
  ""
  "LSETUP (%1, %3) %4 = %5;"
  [(set_attr "length" "4")])

(define_insn "lsetup_without_autoinit"
  [(set (match_operand:SI 0 "lt_register_operand" "=t")
	(label_ref (match_operand 1 "" "")))
   (set (match_operand:SI 2 "lb_register_operand" "=u")
	(label_ref (match_operand 3 "" "")))
   (use (match_operand:SI 4 "lc_register_operand" "k"))]
  ""
  "LSETUP (%1, %3) %4;"
  [(set_attr "length" "4")])

;;  Call instructions..

;; The explicit MEM inside the UNSPEC prevents the compiler from moving
;; the load before a branch after a NULL test, or before a store that
;; initializes a function descriptor.

(define_insn_and_split "load_funcdescsi"
  [(set (match_operand:SI 0 "register_operand" "=a")
	(unspec_volatile:SI [(mem:SI (match_operand:SI 1 "address_operand" "p"))]
			    UNSPEC_VOLATILE_LOAD_FUNCDESC))]
  ""
  "#"
  "reload_completed"
  [(set (match_dup 0) (mem:SI (match_dup 1)))])

(define_expand "call"
  [(parallel [(call (match_operand:SI 0 "" "")
		    (match_operand 1 "" ""))
	      (use (match_operand 2 "" ""))])]
  ""
{
  bfin_expand_call (NULL_RTX, operands[0], operands[1], operands[2], 0);
  DONE;
})

(define_expand "sibcall"
  [(parallel [(call (match_operand:SI 0 "" "")
		    (match_operand 1 "" ""))
	      (use (match_operand 2 "" ""))
	      (return)])]
  ""
{
  bfin_expand_call (NULL_RTX, operands[0], operands[1], operands[2], 1);
  DONE;
})

(define_expand "call_value"
  [(parallel [(set (match_operand 0 "register_operand" "")
		   (call (match_operand:SI 1 "" "")
			 (match_operand 2 "" "")))
	      (use (match_operand 3 "" ""))])]
  ""
{
  bfin_expand_call (operands[0], operands[1], operands[2], operands[3], 0);
  DONE;
})

(define_expand "sibcall_value"
  [(parallel [(set (match_operand 0 "register_operand" "")
		   (call (match_operand:SI 1 "" "")
			 (match_operand 2 "" "")))
	      (use (match_operand 3 "" ""))
	      (return)])]
  ""
{
  bfin_expand_call (operands[0], operands[1], operands[2], operands[3], 1);
  DONE;
})

(define_insn "*call_symbol_fdpic"
  [(call (mem:SI (match_operand:SI 0 "symbol_ref_operand" "Q"))
	 (match_operand 1 "general_operand" "g"))
   (use (match_operand:SI 2 "register_operand" "Z"))
   (use (match_operand 3 "" ""))
   (clobber (reg:SI REG_RETS))]
  "! SIBLING_CALL_P (insn)
   && GET_CODE (operands[0]) == SYMBOL_REF
   && !bfin_longcall_p (operands[0], INTVAL (operands[3]))"
  "call %0;"
  [(set_attr "type" "call")
   (set_attr "length" "4")])

(define_insn "*sibcall_symbol_fdpic"
  [(call (mem:SI (match_operand:SI 0 "symbol_ref_operand" "Q"))
	 (match_operand 1 "general_operand" "g"))
   (use (match_operand:SI 2 "register_operand" "Z"))
   (use (match_operand 3 "" ""))
   (return)]
  "SIBLING_CALL_P (insn)
   && GET_CODE (operands[0]) == SYMBOL_REF
   && !bfin_longcall_p (operands[0], INTVAL (operands[3]))"
  "jump.l %0;"
  [(set_attr "type" "br")
   (set_attr "length" "4")])

(define_insn "*call_value_symbol_fdpic"
  [(set (match_operand 0 "register_operand" "=d")
        (call (mem:SI (match_operand:SI 1 "symbol_ref_operand" "Q"))
	      (match_operand 2 "general_operand" "g")))
   (use (match_operand:SI 3 "register_operand" "Z"))
   (use (match_operand 4 "" ""))
   (clobber (reg:SI REG_RETS))]
  "! SIBLING_CALL_P (insn)
   && GET_CODE (operands[1]) == SYMBOL_REF
   && !bfin_longcall_p (operands[1], INTVAL (operands[4]))"
  "call %1;"
  [(set_attr "type" "call")
   (set_attr "length" "4")])

(define_insn "*sibcall_value_symbol_fdpic"
  [(set (match_operand 0 "register_operand" "=d")
         (call (mem:SI (match_operand:SI 1 "symbol_ref_operand" "Q"))
	       (match_operand 2 "general_operand" "g")))
   (use (match_operand:SI 3 "register_operand" "Z"))
   (use (match_operand 4 "" ""))
   (return)]
  "SIBLING_CALL_P (insn)
   && GET_CODE (operands[1]) == SYMBOL_REF
   && !bfin_longcall_p (operands[1], INTVAL (operands[4]))"
  "jump.l %1;"
  [(set_attr "type" "br")
   (set_attr "length" "4")])

(define_insn "*call_insn_fdpic"
  [(call (mem:SI (match_operand:SI 0 "register_no_elim_operand" "Y"))
	 (match_operand 1 "general_operand" "g"))
   (use (match_operand:SI 2 "register_operand" "Z"))
   (use (match_operand 3 "" ""))
   (clobber (reg:SI REG_RETS))]
  "! SIBLING_CALL_P (insn)"
  "call (%0);"
  [(set_attr "type" "call")
   (set_attr "length" "2")])

(define_insn "*sibcall_insn_fdpic"
  [(call (mem:SI (match_operand:SI 0 "register_no_elim_operand" "Y"))
	 (match_operand 1 "general_operand" "g"))
   (use (match_operand:SI 2 "register_operand" "Z"))
   (use (match_operand 3 "" ""))
   (return)]
  "SIBLING_CALL_P (insn)"
  "jump (%0);"
  [(set_attr "type" "br")
   (set_attr "length" "2")])

(define_insn "*call_value_insn_fdpic"
  [(set (match_operand 0 "register_operand" "=d")
        (call (mem:SI (match_operand:SI 1 "register_no_elim_operand" "Y"))
	      (match_operand 2 "general_operand" "g")))
   (use (match_operand:SI 3 "register_operand" "Z"))
   (use (match_operand 4 "" ""))
   (clobber (reg:SI REG_RETS))]
  "! SIBLING_CALL_P (insn)"
  "call (%1);"
  [(set_attr "type" "call")
   (set_attr "length" "2")])

(define_insn "*sibcall_value_insn_fdpic"
  [(set (match_operand 0 "register_operand" "=d")
         (call (mem:SI (match_operand:SI 1 "register_no_elim_operand" "Y"))
	       (match_operand 2 "general_operand" "g")))
   (use (match_operand:SI 3 "register_operand" "Z"))
   (use (match_operand 4 "" ""))
   (return)]
  "SIBLING_CALL_P (insn)"
  "jump (%1);"
  [(set_attr "type" "br")
   (set_attr "length" "2")])

(define_insn "*call_symbol"
  [(call (mem:SI (match_operand:SI 0 "symbol_ref_operand" "Q"))
	 (match_operand 1 "general_operand" "g"))
   (use (match_operand 2 "" ""))
   (clobber (reg:SI REG_RETS))]
  "! SIBLING_CALL_P (insn)
   && (!TARGET_ID_SHARED_LIBRARY || TARGET_LEAF_ID_SHARED_LIBRARY)
   && GET_CODE (operands[0]) == SYMBOL_REF
   && !bfin_longcall_p (operands[0], INTVAL (operands[2]))"
  "call %0;"
  [(set_attr "type" "call")
   (set_attr "length" "4")])

(define_insn "*sibcall_symbol"
  [(call (mem:SI (match_operand:SI 0 "symbol_ref_operand" "Q"))
	 (match_operand 1 "general_operand" "g"))
   (use (match_operand 2 "" ""))
   (return)]
  "SIBLING_CALL_P (insn)
   && (!TARGET_ID_SHARED_LIBRARY || TARGET_LEAF_ID_SHARED_LIBRARY)
   && GET_CODE (operands[0]) == SYMBOL_REF
   && !bfin_longcall_p (operands[0], INTVAL (operands[2]))"
  "jump.l %0;"
  [(set_attr "type" "br")
   (set_attr "length" "4")])

(define_insn "*call_value_symbol"
  [(set (match_operand 0 "register_operand" "=d")
        (call (mem:SI (match_operand:SI 1 "symbol_ref_operand" "Q"))
	      (match_operand 2 "general_operand" "g")))
   (use (match_operand 3 "" ""))
   (clobber (reg:SI REG_RETS))]
  "! SIBLING_CALL_P (insn)
   && (!TARGET_ID_SHARED_LIBRARY || TARGET_LEAF_ID_SHARED_LIBRARY)
   && GET_CODE (operands[1]) == SYMBOL_REF
   && !bfin_longcall_p (operands[1], INTVAL (operands[3]))"
  "call %1;"
  [(set_attr "type" "call")
   (set_attr "length" "4")])

(define_insn "*sibcall_value_symbol"
  [(set (match_operand 0 "register_operand" "=d")
         (call (mem:SI (match_operand:SI 1 "symbol_ref_operand" "Q"))
	       (match_operand 2 "general_operand" "g")))
   (use (match_operand 3 "" ""))
   (return)]
  "SIBLING_CALL_P (insn)
   && (!TARGET_ID_SHARED_LIBRARY || TARGET_LEAF_ID_SHARED_LIBRARY)
   && GET_CODE (operands[1]) == SYMBOL_REF
   && !bfin_longcall_p (operands[1], INTVAL (operands[3]))"
  "jump.l %1;"
  [(set_attr "type" "br")
   (set_attr "length" "4")])

(define_insn "*call_insn"
  [(call (mem:SI (match_operand:SI 0 "register_no_elim_operand" "a"))
	 (match_operand 1 "general_operand" "g"))
   (use (match_operand 2 "" ""))
   (clobber (reg:SI REG_RETS))]
  "! SIBLING_CALL_P (insn)"
  "call (%0);"
  [(set_attr "type" "call")
   (set_attr "length" "2")])

(define_insn "*sibcall_insn"
  [(call (mem:SI (match_operand:SI 0 "register_no_elim_operand" "z"))
	 (match_operand 1 "general_operand" "g"))
   (use (match_operand 2 "" ""))
   (return)]
  "SIBLING_CALL_P (insn)"
  "jump (%0);"
  [(set_attr "type" "br")
   (set_attr "length" "2")])

(define_insn "*call_value_insn"
  [(set (match_operand 0 "register_operand" "=d")
        (call (mem:SI (match_operand:SI 1 "register_no_elim_operand" "a"))
	      (match_operand 2 "general_operand" "g")))
   (use (match_operand 3 "" ""))
   (clobber (reg:SI REG_RETS))]
  "! SIBLING_CALL_P (insn)"
  "call (%1);"
  [(set_attr "type" "call")
   (set_attr "length" "2")])

(define_insn "*sibcall_value_insn"
  [(set (match_operand 0 "register_operand" "=d")
         (call (mem:SI (match_operand:SI 1 "register_no_elim_operand" "z"))
	       (match_operand 2 "general_operand" "g")))
   (use (match_operand 3 "" ""))
   (return)]
  "SIBLING_CALL_P (insn)"
  "jump (%1);"
  [(set_attr "type" "br")
   (set_attr "length" "2")])

;; Block move patterns

;; We cheat.  This copies one more word than operand 2 indicates.

(define_insn "rep_movsi"
  [(set (match_operand:SI 0 "register_operand" "=&a")
        (plus:SI (plus:SI (match_operand:SI 3 "register_operand" "0")
			  (ashift:SI (match_operand:SI 2 "register_operand" "a")
				     (const_int 2)))
		 (const_int 4)))
   (set (match_operand:SI 1 "register_operand" "=&b")
        (plus:SI (plus:SI (match_operand:SI 4 "register_operand" "1")
			  (ashift:SI (match_dup 2) (const_int 2)))
		 (const_int 4)))
   (set (mem:BLK (match_dup 3))
	(mem:BLK (match_dup 4)))
   (use (match_dup 2))
   (clobber (match_scratch:HI 5 "=&d"))
   (clobber (reg:SI REG_LT1))
   (clobber (reg:SI REG_LC1))
   (clobber (reg:SI REG_LB1))]
  ""
  "%5 = [%4++]; lsetup (1f, 1f) LC1 = %2; 1: MNOP || [%3++] = %5 || %5 = [%4++]; [%3++] = %5;"
  [(set_attr "type" "misc")
   (set_attr "length" "16")
   (set_attr "seq_insns" "multi")])

(define_insn "rep_movhi"
  [(set (match_operand:SI 0 "register_operand" "=&a")
        (plus:SI (plus:SI (match_operand:SI 3 "register_operand" "0")
			  (ashift:SI (match_operand:SI 2 "register_operand" "a")
				     (const_int 1)))
		 (const_int 2)))
   (set (match_operand:SI 1 "register_operand" "=&b")
        (plus:SI (plus:SI (match_operand:SI 4 "register_operand" "1")
			  (ashift:SI (match_dup 2) (const_int 1)))
		 (const_int 2)))
   (set (mem:BLK (match_dup 3))
	(mem:BLK (match_dup 4)))
   (use (match_dup 2))
   (clobber (match_scratch:HI 5 "=&d"))
   (clobber (reg:SI REG_LT1))
   (clobber (reg:SI REG_LC1))
   (clobber (reg:SI REG_LB1))]
  ""
  "%h5 = W[%4++]; lsetup (1f, 1f) LC1 = %2; 1: MNOP || W [%3++] = %5 || %h5 = W [%4++]; W [%3++] = %5;"
  [(set_attr "type" "misc")
   (set_attr "length" "16")
   (set_attr "seq_insns" "multi")])

(define_expand "cpymemsi"
  [(match_operand:BLK 0 "general_operand" "")
   (match_operand:BLK 1 "general_operand" "")
   (match_operand:SI 2 "const_int_operand" "")
   (match_operand:SI 3 "const_int_operand" "")]
  ""
{
  if (bfin_expand_cpymem (operands[0], operands[1], operands[2], operands[3]))
    DONE;
  FAIL;
})

;; Conditional branch patterns
;; The Blackfin has only few condition codes: eq, lt, lte, ltu, leu

(define_insn "compare_eq"
  [(set (match_operand:BI 0 "register_operand" "=C,C")
        (eq:BI (match_operand:SI 1 "register_operand" "d,a")
               (match_operand:SI 2 "reg_or_const_int_operand" "dKs3,aKs3")))]
  ""
  "cc =%1==%2;"
  [(set_attr "type" "compare")])

(define_insn "compare_ne"
  [(set (match_operand:BI 0 "register_operand" "=C,C")
        (ne:BI (match_operand:SI 1 "register_operand" "d,a")
               (match_operand:SI 2 "reg_or_const_int_operand" "dKs3,aKs3")))]
  "0"
  "cc =%1!=%2;"
  [(set_attr "type" "compare")])

(define_insn "compare_lt"
  [(set (match_operand:BI 0 "register_operand" "=C,C")
        (lt:BI (match_operand:SI 1 "register_operand" "d,a")
               (match_operand:SI 2 "reg_or_const_int_operand" "dKs3,aKs3")))]
  ""
  "cc =%1<%2;"
  [(set_attr "type" "compare")])

(define_insn "compare_le"
  [(set (match_operand:BI 0 "register_operand" "=C,C")
        (le:BI (match_operand:SI 1 "register_operand" "d,a")
               (match_operand:SI 2 "reg_or_const_int_operand" "dKs3,aKs3")))]
  ""
  "cc =%1<=%2;"
  [(set_attr "type" "compare")])

(define_insn "compare_leu"
  [(set (match_operand:BI 0 "register_operand" "=C,C")
        (leu:BI (match_operand:SI 1 "register_operand" "d,a")
                (match_operand:SI 2 "reg_or_const_int_operand" "dKu3,aKu3")))]
  ""
  "cc =%1<=%2 (iu);"
  [(set_attr "type" "compare")])

(define_insn "compare_ltu"
  [(set (match_operand:BI 0 "register_operand" "=C,C")
        (ltu:BI (match_operand:SI 1 "register_operand" "d,a")
                (match_operand:SI 2 "reg_or_const_int_operand" "dKu3,aKu3")))]
  ""
  "cc =%1<%2 (iu);"
  [(set_attr "type" "compare")])

;; Same as above, but and CC with the overflow bit generated by the first
;; multiplication.
(define_insn "flag_mul_macv2hi_parts_acconly_andcc0"
  [(set (match_operand:PDI 0 "register_operand" "=B,e,e")
	(unspec:PDI [(vec_select:HI
		      (match_operand:V2HI 2 "register_operand" "d,d,d")
		      (parallel [(match_operand 4 "const01_operand" "P0P1,P0P1,P0P1")]))
		     (vec_select:HI
		      (match_operand:V2HI 3 "register_operand" "d,d,d")
		      (parallel [(match_operand 6 "const01_operand" "P0P1,P0P1,P0P1")]))
		     (match_operand 10 "const_int_operand" "PB,PA,PA")]
		    UNSPEC_MUL_WITH_FLAG))
   (set (match_operand:PDI 1 "register_operand" "=B,e,e")
	(unspec:PDI [(vec_select:HI
		      (match_dup 2)
		      (parallel [(match_operand 5 "const01_operand" "P0P1,P0P1,P0P1")]))
		     (vec_select:HI
		      (match_dup 3)
		      (parallel [(match_operand 7 "const01_operand" "P0P1,P0P1,P0P1")]))
		     (match_operand:PDI 8 "register_operand" "1,1,1")
		     (match_operand 9 "const01_operand" "P0P1,P0P1,P0P1")
		     (match_operand 11 "const_int_operand" "PA,PB,PA")]
		    UNSPEC_MAC_WITH_FLAG))
   (set (reg:BI REG_CC)
	(and:BI (reg:BI REG_CC)
		(unspec:BI [(vec_select:HI (match_dup 2) (parallel [(match_dup 4)]))
			    (vec_select:HI (match_dup 3) (parallel [(match_dup 6)]))
			    (match_dup 10)]
			   UNSPEC_MUL_WITH_FLAG)))]
  "MACFLAGS_MATCH_P (INTVAL (operands[10]), INTVAL (operands[11]))"
{
  rtx xops[6];
  const char *templates[] = {
    "%0 = %h2 * %h3, %1 %b4 %h2 * %h3 %M5;\n\tCC &= %v0;",
    "%0 = %d2 * %h3, %1 %b4 %h2 * %h3 %M5;\n\tCC &= %v0;",
    "%0 = %h2 * %h3, %1 %b4 %d2 * %h3 %M5;\n\tCC &= %v0;",
    "%0 = %d2 * %h3, %1 %b4 %d2 * %h3 %M5;\n\tCC &= %v0;",
    "%0 = %h2 * %d3, %1 %b4 %h2 * %h3 %M5;\n\tCC &= %v0;",
    "%0 = %d2 * %d3, %1 %b4 %h2 * %h3 %M5;\n\tCC &= %v0;",
    "%0 = %h2 * %d3, %1 %b4 %d2 * %h3 %M5;\n\tCC &= %v0;",
    "%0 = %d2 * %d3, %1 %b4 %d2 * %h3 %M5;\n\tCC &= %v0;",
    "%0 = %h2 * %h3, %1 %b4 %h2 * %d3 %M5;\n\tCC &= %v0;",
    "%0 = %d2 * %h3, %1 %b4 %h2 * %d3 %M5;\n\tCC &= %v0;",
    "%0 = %h2 * %h3, %1 %b4 %d2 * %d3 %M5;\n\tCC &= %v0;",
    "%0 = %d2 * %h3, %1 %b4 %d2 * %d3 %M5;\n\tCC &= %v0;",
    "%0 = %h2 * %d3, %1 %b4 %h2 * %d3 %M5;\n\tCC &= %v0;",
    "%0 = %d2 * %d3, %1 %b4 %h2 * %d3 %M5;\n\tCC &= %v0;",
    "%0 = %h2 * %d3, %1 %b4 %d2 * %d3 %M5;\n\tCC &= %v0;",
    "%0 = %d2 * %d3, %1 %b4 %d2 * %d3 %M5;\n\tCC &= %v0;" };
  int alt = (INTVAL (operands[4]) + (INTVAL (operands[5]) << 1)
	     + (INTVAL (operands[6]) << 2)  + (INTVAL (operands[7]) << 3));
  xops[0] = operands[0];
  xops[1] = operands[1];
  xops[2] = operands[2];
  xops[3] = operands[3];
  xops[4] = operands[9];
  xops[5] = which_alternative == 0 ? operands[10] : operands[11];
  output_asm_insn (templates[alt], xops);
  return "";
}
  [(set_attr "type" "misc")
   (set_attr "length" "6")
   (set_attr "seq_insns" "multi")])

(define_expand "cbranchsi4"
  [(set (pc)
        (if_then_else (match_operator 0 "ordered_comparison_operator"
                       [(match_operand:SI 1 "register_operand" "")
                        (match_operand:SI 2 "reg_or_const_int_operand" "")])
                   (label_ref (match_operand 3 "" ""))
                   (pc)))]
  ""
{
  rtx bi_compare = bfin_gen_compare (operands[0], SImode);
  emit_jump_insn (gen_cbranchbi4 (bi_compare, bfin_cc_rtx, CONST0_RTX (BImode),
				  operands[3]));
  DONE;
})

(define_insn "cbranchbi4"
  [(set (pc)
	(if_then_else
	 (match_operator 0 "bfin_bimode_comparison_operator"
			 [(match_operand:BI 1 "register_operand" "C")
			  (match_operand:BI 2 "immediate_operand" "P0")])
	 (label_ref (match_operand 3 "" ""))
	 (pc)))]
  ""
{
  asm_conditional_branch (insn, operands, 0, 0);
  return "";
}
  [(set_attr "type" "brcc")])

;; Special cbranch patterns to deal with the speculative load problem - see
;; bfin_reorg for details.

(define_insn "cbranch_predicted_taken"
  [(set (pc)
	(if_then_else
	 (match_operator 0 "bfin_bimode_comparison_operator"
			 [(match_operand:BI 1 "register_operand" "C")
			  (match_operand:BI 2 "immediate_operand" "P0")])
	 (label_ref (match_operand 3 "" ""))
	 (pc)))
   (unspec [(const_int 0)] UNSPEC_CBRANCH_TAKEN)]
  ""
{
  asm_conditional_branch (insn, operands, 0, 1);
  return "";
}
  [(set_attr "type" "brcc")])

(define_insn "cbranch_with_nops"
  [(set (pc)
	(if_then_else
	 (match_operator 0 "bfin_bimode_comparison_operator"
			 [(match_operand:BI 1 "register_operand" "C")
			  (match_operand:BI 2 "immediate_operand" "P0")])
	 (label_ref (match_operand 3 "" ""))
	 (pc)))
   (unspec [(match_operand 4 "immediate_operand" "")] UNSPEC_CBRANCH_NOPS)]
  "reload_completed"
{
  asm_conditional_branch (insn, operands, INTVAL (operands[4]), 0);
  return "";
}
  [(set_attr "type" "brcc")
   (set_attr "length" "8")])

;; setcc insns.

(define_expand "cstorebi4"
  [(set (match_dup 4)
        (match_operator:BI 1 "bfin_bimode_comparison_operator"
                       [(match_operand:BI 2 "register_operand" "")
                        (match_operand:BI 3 "reg_or_const_int_operand" "")]))
   (set (match_operand:SI 0 "register_operand" "")
       (ne:SI (match_dup 4) (const_int 0)))]
  ""
{
  /* It could be expanded as a movbisi instruction, but the portable
     alternative produces better code.  */
  if (GET_CODE (operands[1]) == NE)
    FAIL;

  operands[4] = bfin_cc_rtx;
})

(define_expand "cstoresi4"
  [(set (match_operand:SI 0 "register_operand")
        (match_operator:SI 1 "ordered_comparison_operator"
                       [(match_operand:SI 2 "register_operand" "")
                        (match_operand:SI 3 "reg_or_const_int_operand" "")]))]
  ""
{
  rtx bi_compare, test;

  if (!bfin_direct_comparison_operator (operands[1], SImode))
    {
      if (!register_operand (operands[3], SImode)
	  || GET_CODE (operands[1]) == NE)
	FAIL;
      test = gen_rtx_fmt_ee (swap_condition (GET_CODE (operands[1])),
			     SImode, operands[3], operands[2]);
    }
  else
    test = operands[1];

  bi_compare = bfin_gen_compare (test, SImode);
  gcc_assert (GET_CODE (bi_compare) == NE);
  emit_insn (gen_movbisi (operands[0], bfin_cc_rtx));
  DONE;
})

(define_insn "nop"
  [(const_int 0)]
  ""
  "nop;")

;; A nop which stays there when emitted.
(define_insn "forced_nop"
  [(unspec [(const_int 0)] UNSPEC_NOP)]
  ""
  "nop;")

(define_insn "mnop"
  [(unspec [(const_int 0)] UNSPEC_32BIT)]
  ""
  "mnop%!"
  [(set_attr "type" "dsp32")])

;;;;;;;;;;;;;;;;;;;;   CC2dreg   ;;;;;;;;;;;;;;;;;;;;;;;;;
(define_insn "movsibi"
  [(set (match_operand:BI 0 "register_operand" "=C")
	(ne:BI (match_operand:SI 1 "register_operand" "d")
	       (const_int 0)))]
  ""
  "CC = %1;"
  [(set_attr "length" "2")])

(define_insn_and_split "movbisi"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(ne:SI (match_operand:BI 1 "register_operand" "C")
	       (const_int 0)))]
  ""
  "#"
  ""
  [(set (match_operand:SI 0 "register_operand" "")
	(zero_extend:SI (match_operand:BI 1 "register_operand" "")))]
  "")

(define_insn "notbi"
  [(set (match_operand:BI 0 "register_operand" "=C")
	(eq:BI (match_operand:BI 1 "register_operand" " 0")
	       (const_int 0)))]
  ""
  "%0 = ! %0;"    /*  NOT CC;"  */
  [(set_attr "type" "compare")])

;; Vector and DSP insns

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=d")
	(ior:SI (ashift:SI (match_operand:SI 1 "register_operand" "d")
			   (const_int 24))
		(lshiftrt:SI (match_operand:SI 2 "register_operand" "d")
			     (const_int 8))))]
  ""
  "%0 = ALIGN8(%1, %2)%!"
  [(set_attr "type" "dsp32")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=d")
	(ior:SI (ashift:SI (match_operand:SI 1 "register_operand" "d")
			   (const_int 16))
		(lshiftrt:SI (match_operand:SI 2 "register_operand" "d")
			     (const_int 16))))]
  ""
  "%0 = ALIGN16(%1, %2)%!"
  [(set_attr "type" "dsp32")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=d")
	(ior:SI (ashift:SI (match_operand:SI 1 "register_operand" "d")
			   (const_int 8))
		(lshiftrt:SI (match_operand:SI 2 "register_operand" "d")
			     (const_int 24))))]
  ""
  "%0 = ALIGN24(%1, %2)%!"
  [(set_attr "type" "dsp32")])

;; Prologue and epilogue.

(define_expand "prologue"
  [(const_int 1)]
  ""
  "bfin_expand_prologue (); DONE;")

(define_expand "epilogue"
  [(const_int 1)]
  ""
  "bfin_expand_epilogue (1, 0, 0); DONE;")

(define_expand "sibcall_epilogue"
  [(const_int 1)]
  ""
  "bfin_expand_epilogue (0, 0, 1); DONE;")

(define_expand "eh_return"
  [(use (match_operand:SI 0 "register_operand" ""))]
  ""
{
  emit_insn (gen_eh_store_handler (EH_RETURN_HANDLER_RTX, operands[0]));
  emit_jump_insn (gen_eh_return_internal ());
  emit_barrier ();
  DONE;
})

(define_insn "eh_store_handler"
  [(unspec_volatile [(match_operand:SI 1 "register_operand" "da")]
		    UNSPEC_VOLATILE_STORE_EH_HANDLER)
   (clobber (match_operand:SI 0 "memory_operand" "=m"))]
  ""
  "%0 = %1%!"
  [(set_attr "type" "mcst")])

(define_insn_and_split "eh_return_internal"
  [(eh_return)]
  ""
  "#"
  "epilogue_completed"
  [(const_int 1)]
  "bfin_expand_epilogue (1, 1, 0); DONE;")

(define_insn "link"
  [(set (mem:SI (plus:SI (reg:SI REG_SP) (const_int -4))) (reg:SI REG_RETS))
   (set (mem:SI (plus:SI (reg:SI REG_SP) (const_int -8))) (reg:SI REG_FP))
   (set (reg:SI REG_FP)
	(plus:SI (reg:SI REG_SP) (const_int -8)))
   (set (reg:SI REG_SP)
	(plus:SI (reg:SI REG_SP) (match_operand:SI 0 "immediate_operand" "i")))]
  ""
  "LINK %Z0;"
  [(set_attr "length" "4")])

(define_insn "unlink"
  [(set (reg:SI REG_FP) (mem:SI (reg:SI REG_FP)))
   (set (reg:SI REG_RETS) (mem:SI (plus:SI (reg:SI REG_FP) (const_int 4))))
   (set (reg:SI REG_SP) (plus:SI (reg:SI REG_FP) (const_int 8)))]
  ""
  "UNLINK;"
  [(set_attr "length" "4")])

;; This pattern is slightly clumsy.  The stack adjust must be the final SET in
;; the pattern, otherwise dwarf2out becomes very confused about which reg goes
;; where on the stack, since it goes through all elements of the parallel in
;; sequence.
(define_insn "push_multiple"
  [(match_parallel 0 "push_multiple_operation"
    [(unspec [(match_operand:SI 1 "immediate_operand" "i")] UNSPEC_PUSH_MULTIPLE)])]
  ""
{
  output_push_multiple (insn, operands);
  return "";
})

(define_insn "pop_multiple"
  [(match_parallel 0 "pop_multiple_operation"
    [(set (reg:SI REG_SP)
	  (plus:SI (reg:SI REG_SP) (match_operand:SI 1 "immediate_operand" "i")))])]
  ""
{
  output_pop_multiple (insn, operands);
  return "";
})

(define_insn "return_internal"
  [(return)
   (use (match_operand 0 "register_operand" ""))]
  "reload_completed"
{
  switch (REGNO (operands[0]))
    {
    case REG_RETX:
      return "rtx;";
    case REG_RETN:
      return "rtn;";
    case REG_RETI:
      return "rti;";
    case REG_RETS:
      return "rts;";
    }
  gcc_unreachable ();
})

;; When used at a location where CC contains 1, causes a speculative load
;; that is later cancelled.  This is used for certain workarounds in
;; interrupt handler prologues.
(define_insn "dummy_load"
  [(unspec_volatile [(match_operand 0 "register_operand" "a")
		     (match_operand 1 "register_operand" "C")]
		    UNSPEC_VOLATILE_DUMMY)]
  ""
  "if cc jump 4;\n\tr7 = [%0];"
 [(set_attr "type" "misc")
  (set_attr "length" "4")
  (set_attr "seq_insns" "multi")])

;; A placeholder insn inserted before the final scheduling pass.  It is used
;; to improve scheduling of loads when workarounds for speculative loads are
;; needed, by not placing them in the first few cycles after a conditional
;; branch.
(define_insn "stall"
  [(unspec_volatile [(match_operand 0 "const_int_operand" "P1P3")]
		    UNSPEC_VOLATILE_STALL)]
  ""
  ""
  [(set_attr "type" "stall")])

(define_insn "csync"
  [(unspec_volatile [(const_int 0)] UNSPEC_VOLATILE_CSYNC)]
  ""
  "csync;"
  [(set_attr "type" "sync")])

(define_insn "ssync"
  [(unspec_volatile [(const_int 0)] UNSPEC_VOLATILE_SSYNC)]
  ""
  "ssync;"
  [(set_attr "type" "sync")])

(define_insn "trap"
  [(trap_if (const_int 1) (const_int 3))]
  ""
  "excpt 3;"
  [(set_attr "type" "misc")
   (set_attr "length" "2")])

(define_insn "trapifcc"
  [(trap_if (reg:BI REG_CC) (const_int 3))]
  ""
  "if !cc jump 4 (bp); excpt 3;"
  [(set_attr "type" "misc")
   (set_attr "length" "4")
   (set_attr "seq_insns" "multi")])

;;; Vector instructions

;; First, all sorts of move variants

(define_insn "movhiv2hi_low"
  [(set (match_operand:V2HI 0 "register_operand" "=d")
	(vec_concat:V2HI
	 (match_operand:HI 2 "register_operand" "d")
	 (vec_select:HI (match_operand:V2HI 1 "register_operand" "0")
			(parallel [(const_int 1)]))))]
  ""
  "%h0 = %h2 << 0%!"
  [(set_attr "type" "dsp32shiftimm")])

(define_insn "movhiv2hi_high"
  [(set (match_operand:V2HI 0 "register_operand" "=d")
	(vec_concat:V2HI
	 (vec_select:HI (match_operand:V2HI 1 "register_operand" "0")
			(parallel [(const_int 0)]))
	 (match_operand:HI 2 "register_operand" "d")))]
  ""
  "%d0 = %h2 << 0%!"
  [(set_attr "type" "dsp32shiftimm")])

;; No earlyclobber on alternative two since our sequence ought to be safe.
;; The order of operands is intentional to match the VDSP builtin (high word
;; is passed first).
(define_insn_and_split "composev2hi"
  [(set (match_operand:V2HI 0 "register_operand" "=d,d")
	(vec_concat:V2HI (match_operand:HI 2 "register_operand" "0,d")
			 (match_operand:HI 1 "register_operand" "d,d")))]
  ""
  "@
   %d0 = %h1 << 0%!
   #"
  "reload_completed"
  [(set (match_dup 0)
	(vec_concat:V2HI
	 (vec_select:HI (match_dup 0) (parallel [(const_int 0)]))
	 (match_dup 1)))
   (set (match_dup 0)
	(vec_concat:V2HI
	 (match_dup 2)
	 (vec_select:HI (match_dup 0) (parallel [(const_int 1)]))))]
  ""
  [(set_attr "type" "dsp32shiftimm")])

; Like composev2hi, but operating on elements of V2HI vectors.
; Useful on its own, and as a combiner bridge for the multiply and
; mac patterns.
(define_insn "packv2hi"
  [(set (match_operand:V2HI 0 "register_operand" "=d,d,d,d,d,d,d,d")
	(vec_concat:V2HI (vec_select:HI
			  (match_operand:V2HI 1 "register_operand" "0,0,d,d,d,d,d,d")
			  (parallel [(match_operand 3 "const01_operand" "P0,P0,P0,P1,P0,P1,P0,P1")]))
			 (vec_select:HI
			  (match_operand:V2HI 2 "register_operand" "d,d,0,0,d,d,d,d")
			  (parallel [(match_operand 4 "const01_operand" "P0,P1,P1,P1,P0,P0,P1,P1")]))))]
  ""
  "@
   %d0 = %h2 << 0%!
   %d0 = %d2 << 0%!
   %h0 = %h1 << 0%!
   %h0 = %d1 << 0%!
   %0 = PACK (%h2,%h1)%!
   %0 = PACK (%h2,%d1)%!
   %0 = PACK (%d2,%h1)%!
   %0 = PACK (%d2,%d1)%!"
  [(set_attr "type" "dsp32shiftimm,dsp32shiftimm,dsp32shiftimm,dsp32shiftimm,dsp32,dsp32,dsp32,dsp32")])

(define_insn "movv2hi_hi"
  [(set (match_operand:HI 0 "register_operand" "=d,d,d")
	(vec_select:HI (match_operand:V2HI 1 "register_operand" "0,d,d")
		       (parallel [(match_operand 2 "const01_operand" "P0,P0,P1")])))]
  ""
  "@
   /* optimized out */
   %h0 = %h1 << 0%!
   %h0 = %d1 << 0%!"
  [(set_attr "type" "dsp32shiftimm")])

(define_expand "movv2hi_hi_low"
  [(set (match_operand:HI 0 "register_operand" "")
	(vec_select:HI (match_operand:V2HI 1 "register_operand" "")
		       (parallel [(const_int 0)])))]
  ""
  "")

(define_expand "movv2hi_hi_high"
  [(set (match_operand:HI 0 "register_operand" "")
	(vec_select:HI (match_operand:V2HI 1 "register_operand" "")
		       (parallel [(const_int 1)])))]
  ""
  "")

;; Unusual arithmetic operations on 16-bit registers.

(define_code_iterator sp_or_sm [ss_plus ss_minus])
(define_code_attr spm_string [(ss_plus "+") (ss_minus "-")])
(define_code_attr spm_name [(ss_plus "add") (ss_minus "sub")])

(define_insn "ss<spm_name>hi3"
  [(set (match_operand:HI 0 "register_operand" "=d")
	(sp_or_sm:HI (match_operand:HI 1 "register_operand" "d")
		    (match_operand:HI 2 "register_operand" "d")))]
  ""
  "%h0 = %h1 <spm_string>  %h2 (S)%!"
  [(set_attr "type" "dsp32")])

(define_insn "ss<spm_name>hi3_parts"
  [(set (match_operand:HI 0 "register_operand" "=d")
	(sp_or_sm:HI (vec_select:HI
		      (match_operand:V2HI 1 "register_operand" "d")
		      (parallel [(match_operand 3 "const01_operand" "P0P1")]))
		     (vec_select:HI
		      (match_operand:V2HI 2 "register_operand" "d")
		      (parallel [(match_operand 4 "const01_operand" "P0P1")]))))]
   ""
{
  const char *templates[] = {
    "%h0 = %h1 <spm_string> %h2 (S)%!",
    "%h0 = %d1 <spm_string> %h2 (S)%!",
    "%h0 = %h1 <spm_string> %d2 (S)%!",
    "%h0 = %d1 <spm_string> %d2 (S)%!" };
  int alt = INTVAL (operands[3]) + (INTVAL (operands[4]) << 1);
  return templates[alt];
}
  [(set_attr "type" "dsp32")])

(define_insn "ss<spm_name>hi3_low_parts"
  [(set (match_operand:V2HI 0 "register_operand" "=d")
	(vec_concat:V2HI
	 (vec_select:HI (match_operand:V2HI 1 "register_operand" "0")
			(parallel [(const_int 0)]))
	 (sp_or_sm:HI (vec_select:HI
		       (match_operand:V2HI 2 "register_operand" "d")
		       (parallel [(match_operand 4 "const01_operand" "P0P1")]))
		      (vec_select:HI
		       (match_operand:V2HI 3 "register_operand" "d")
		       (parallel [(match_operand 5 "const01_operand" "P0P1")])))))]
   ""
{
  const char *templates[] = {
    "%h0 = %h2 <spm_string> %h3 (S)%!",
    "%h0 = %d2 <spm_string> %h3 (S)%!",
    "%h0 = %h2 <spm_string> %d3 (S)%!",
    "%h0 = %d2 <spm_string> %d3 (S)%!" };
  int alt = INTVAL (operands[4]) + (INTVAL (operands[5]) << 1);
  return templates[alt];
}
  [(set_attr "type" "dsp32")])

(define_insn "ss<spm_name>hi3_high_parts"
  [(set (match_operand:V2HI 0 "register_operand" "=d")
	(vec_concat:V2HI
	 (sp_or_sm:HI (vec_select:HI
		       (match_operand:V2HI 2 "register_operand" "d")
		       (parallel [(match_operand 4 "const01_operand" "P0P1")]))
		      (vec_select:HI
		       (match_operand:V2HI 3 "register_operand" "d")
		       (parallel [(match_operand 5 "const01_operand" "P0P1")])))
	 (vec_select:HI (match_operand:V2HI 1 "register_operand" "0")
			(parallel [(const_int 1)]))))]
   ""
{
  const char *templates[] = {
    "%d0 = %h2 <spm_string> %h3 (S)%!",
    "%d0 = %d2 <spm_string> %h3 (S)%!",
    "%d0 = %h2 <spm_string> %d3 (S)%!",
    "%d0 = %d2 <spm_string> %d3 (S)%!" };
  int alt = INTVAL (operands[4]) + (INTVAL (operands[5]) << 1);
  return templates[alt];
}
  [(set_attr "type" "dsp32")])

;; V2HI vector insns

(define_insn "addv2hi3"
  [(set (match_operand:V2HI 0 "register_operand" "=d")
	(plus:V2HI (match_operand:V2HI 1 "register_operand" "d")
		   (match_operand:V2HI 2 "register_operand" "d")))]
  ""
  "%0 = %1 +|+ %2%!"
  [(set_attr "type" "dsp32")])

(define_insn "ssaddv2hi3"
  [(set (match_operand:V2HI 0 "register_operand" "=d")
	(ss_plus:V2HI (match_operand:V2HI 1 "register_operand" "d")
		      (match_operand:V2HI 2 "register_operand" "d")))]
  ""
  "%0 = %1 +|+ %2 (S)%!"
  [(set_attr "type" "dsp32")])

(define_insn "subv2hi3"
  [(set (match_operand:V2HI 0 "register_operand" "=d")
	(minus:V2HI (match_operand:V2HI 1 "register_operand" "d")
		   (match_operand:V2HI 2 "register_operand" "d")))]
  ""
  "%0 = %1 -|- %2%!"
  [(set_attr "type" "dsp32")])

(define_insn "sssubv2hi3"
  [(set (match_operand:V2HI 0 "register_operand" "=d")
	(ss_minus:V2HI (match_operand:V2HI 1 "register_operand" "d")
		       (match_operand:V2HI 2 "register_operand" "d")))]
  ""
  "%0 = %1 -|- %2 (S)%!"
  [(set_attr "type" "dsp32")])

(define_insn "addsubv2hi3"
  [(set (match_operand:V2HI 0 "register_operand" "=d")
	(vec_concat:V2HI
	 (minus:HI (vec_select:HI (match_operand:V2HI 1 "register_operand" "d")
				  (parallel [(const_int 0)]))
		   (vec_select:HI (match_operand:V2HI 2 "register_operand" "d")
				  (parallel [(const_int 0)])))
	 (plus:HI (vec_select:HI (match_dup 1) (parallel [(const_int 1)]))
		  (vec_select:HI (match_dup 2) (parallel [(const_int 1)])))))]
  ""
  "%0 = %1 +|- %2%!"
  [(set_attr "type" "dsp32")])

(define_insn "subaddv2hi3"
  [(set (match_operand:V2HI 0 "register_operand" "=d")
	(vec_concat:V2HI
	 (plus:HI (vec_select:HI (match_operand:V2HI 1 "register_operand" "d")
				 (parallel [(const_int 0)]))
		  (vec_select:HI (match_operand:V2HI 2 "register_operand" "d")
				 (parallel [(const_int 0)])))
	 (minus:HI (vec_select:HI (match_dup 1) (parallel [(const_int 1)]))
		   (vec_select:HI (match_dup 2) (parallel [(const_int 1)])))))]
  ""
  "%0 = %1 -|+ %2%!"
  [(set_attr "type" "dsp32")])

(define_insn "ssaddsubv2hi3"
  [(set (match_operand:V2HI 0 "register_operand" "=d")
	(vec_concat:V2HI
	 (ss_minus:HI (vec_select:HI (match_operand:V2HI 1 "register_operand" "d")
				     (parallel [(const_int 0)]))
		      (vec_select:HI (match_operand:V2HI 2 "register_operand" "d")
				     (parallel [(const_int 0)])))
	 (ss_plus:HI (vec_select:HI (match_dup 1) (parallel [(const_int 1)]))
		     (vec_select:HI (match_dup 2) (parallel [(const_int 1)])))))]
  ""
  "%0 = %1 +|- %2 (S)%!"
  [(set_attr "type" "dsp32")])

(define_insn "sssubaddv2hi3"
  [(set (match_operand:V2HI 0 "register_operand" "=d")
	(vec_concat:V2HI
	 (ss_plus:HI (vec_select:HI (match_operand:V2HI 1 "register_operand" "d")
				    (parallel [(const_int 0)]))
		     (vec_select:HI (match_operand:V2HI 2 "register_operand" "d")
				    (parallel [(const_int 0)])))
	 (ss_minus:HI (vec_select:HI (match_dup 1) (parallel [(const_int 1)]))
		      (vec_select:HI (match_dup 2) (parallel [(const_int 1)])))))]
  ""
  "%0 = %1 -|+ %2 (S)%!"
  [(set_attr "type" "dsp32")])

(define_insn "sublohiv2hi3"
  [(set (match_operand:HI 0 "register_operand" "=d")
	(minus:HI (vec_select:HI (match_operand:V2HI 1 "register_operand" "d")
				 (parallel [(const_int 1)]))
		  (vec_select:HI (match_operand:V2HI 2 "register_operand" "d")
				 (parallel [(const_int 0)]))))]
  ""
  "%h0 = %d1 - %h2%!"
  [(set_attr "type" "dsp32")])

(define_insn "subhilov2hi3"
  [(set (match_operand:HI 0 "register_operand" "=d")
	(minus:HI (vec_select:HI (match_operand:V2HI 1 "register_operand" "d")
				 (parallel [(const_int 0)]))
		  (vec_select:HI (match_operand:V2HI 2 "register_operand" "d")
				 (parallel [(const_int 1)]))))]
  ""
  "%h0 = %h1 - %d2%!"
  [(set_attr "type" "dsp32")])

(define_insn "sssublohiv2hi3"
  [(set (match_operand:HI 0 "register_operand" "=d")
	(ss_minus:HI (vec_select:HI (match_operand:V2HI 1 "register_operand" "d")
				    (parallel [(const_int 1)]))
		     (vec_select:HI (match_operand:V2HI 2 "register_operand" "d")
				    (parallel [(const_int 0)]))))]
  ""
  "%h0 = %d1 - %h2 (S)%!"
  [(set_attr "type" "dsp32")])

(define_insn "sssubhilov2hi3"
  [(set (match_operand:HI 0 "register_operand" "=d")
	(ss_minus:HI (vec_select:HI (match_operand:V2HI 1 "register_operand" "d")
				    (parallel [(const_int 0)]))
		     (vec_select:HI (match_operand:V2HI 2 "register_operand" "d")
				    (parallel [(const_int 1)]))))]
  ""
  "%h0 = %h1 - %d2 (S)%!"
  [(set_attr "type" "dsp32")])

(define_insn "addlohiv2hi3"
  [(set (match_operand:HI 0 "register_operand" "=d")
	(plus:HI (vec_select:HI (match_operand:V2HI 1 "register_operand" "d")
				(parallel [(const_int 1)]))
		 (vec_select:HI (match_operand:V2HI 2 "register_operand" "d")
				(parallel [(const_int 0)]))))]
  ""
  "%h0 = %d1 + %h2%!"
  [(set_attr "type" "dsp32")])

(define_insn "addhilov2hi3"
  [(set (match_operand:HI 0 "register_operand" "=d")
	(plus:HI (vec_select:HI (match_operand:V2HI 1 "register_operand" "d")
				(parallel [(const_int 0)]))
		 (vec_select:HI (match_operand:V2HI 2 "register_operand" "d")
				(parallel [(const_int 1)]))))]
  ""
  "%h0 = %h1 + %d2%!"
  [(set_attr "type" "dsp32")])

(define_insn "ssaddlohiv2hi3"
  [(set (match_operand:HI 0 "register_operand" "=d")
	(ss_plus:HI (vec_select:HI (match_operand:V2HI 1 "register_operand" "d")
				   (parallel [(const_int 1)]))
		    (vec_select:HI (match_operand:V2HI 2 "register_operand" "d")
				   (parallel [(const_int 0)]))))]
  ""
  "%h0 = %d1 + %h2 (S)%!"
  [(set_attr "type" "dsp32")])

(define_insn "ssaddhilov2hi3"
  [(set (match_operand:HI 0 "register_operand" "=d")
	(ss_plus:HI (vec_select:HI (match_operand:V2HI 1 "register_operand" "d")
				   (parallel [(const_int 0)]))
		    (vec_select:HI (match_operand:V2HI 2 "register_operand" "d")
				   (parallel [(const_int 1)]))))]
  ""
  "%h0 = %h1 + %d2 (S)%!"
  [(set_attr "type" "dsp32")])

(define_insn "sminv2hi3"
  [(set (match_operand:V2HI 0 "register_operand" "=d")
	(smin:V2HI (match_operand:V2HI 1 "register_operand" "d")
		   (match_operand:V2HI 2 "register_operand" "d")))]
  ""
  "%0 = MIN (%1, %2) (V)%!"
  [(set_attr "type" "dsp32")])

(define_insn "smaxv2hi3"
  [(set (match_operand:V2HI 0 "register_operand" "=d")
	(smax:V2HI (match_operand:V2HI 1 "register_operand" "d")
		   (match_operand:V2HI 2 "register_operand" "d")))]
  ""
  "%0 = MAX (%1, %2) (V)%!"
  [(set_attr "type" "dsp32")])

;; Multiplications.

;; The Blackfin allows a lot of different options, and we need many patterns to
;; cover most of the hardware's abilities.
;; There are a few simple patterns using MULT rtx codes, but most of them use
;; an unspec with a const_int operand that determines which flag to use in the
;; instruction.
;; There are variants for single and parallel multiplications.
;; There are variants which just use 16-bit lowparts as inputs, and variants
;; which allow the user to choose just which halves to use as input values.
;; There are variants which set D registers, variants which set accumulators,
;; variants which set both, some of them optionally using the accumulators as
;; inputs for multiply-accumulate operations.

(define_insn "flag_mulhi"
  [(set (match_operand:HI 0 "register_operand" "=d")
	(unspec:HI [(match_operand:HI 1 "register_operand" "d")
		    (match_operand:HI 2 "register_operand" "d")
		    (match_operand 3 "const_int_operand" "n")]
		   UNSPEC_MUL_WITH_FLAG))]
  ""
  "%h0 = %h1 * %h2 %M3%!"
  [(set_attr "type" "dsp32")])

(define_insn "flag_mulhi_parts"
  [(set (match_operand:HI 0 "register_operand" "=d")
	(unspec:HI [(vec_select:HI
		     (match_operand:V2HI 1 "register_operand" "d")
		     (parallel [(match_operand 3 "const01_operand" "P0P1")]))
		    (vec_select:HI
		     (match_operand:V2HI 2 "register_operand" "d")
		     (parallel [(match_operand 4 "const01_operand" "P0P1")]))
		    (match_operand 5 "const_int_operand" "n")]
		   UNSPEC_MUL_WITH_FLAG))]
  ""
{
  const char *templates[] = {
    "%h0 = %h1 * %h2 %M5%!",
    "%h0 = %d1 * %h2 %M5%!",
    "%h0 = %h1 * %d2 %M5%!",
    "%h0 = %d1 * %d2 %M5%!" };
  int alt = INTVAL (operands[3]) + (INTVAL (operands[4]) << 1);
  return templates[alt];
}
  [(set_attr "type" "dsp32")])

(define_insn "flag_mulhisi"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(unspec:SI [(match_operand:HI 1 "register_operand" "d")
		    (match_operand:HI 2 "register_operand" "d")
		    (match_operand 3 "const_int_operand" "n")]
		   UNSPEC_MUL_WITH_FLAG))]
  ""
  "%0 = %h1 * %h2 %M3%!"
  [(set_attr "type" "dsp32")])

(define_insn "flag_mulhisi_parts"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(unspec:SI [(vec_select:HI
		     (match_operand:V2HI 1 "register_operand" "d")
		     (parallel [(match_operand 3 "const01_operand" "P0P1")]))
		    (vec_select:HI
		     (match_operand:V2HI 2 "register_operand" "d")
		     (parallel [(match_operand 4 "const01_operand" "P0P1")]))
		    (match_operand 5 "const_int_operand" "n")]
		   UNSPEC_MUL_WITH_FLAG))]
  ""
{
  const char *templates[] = {
    "%0 = %h1 * %h2 %M5%!",
    "%0 = %d1 * %h2 %M5%!",
    "%0 = %h1 * %d2 %M5%!",
    "%0 = %d1 * %d2 %M5%!" };
  int alt = INTVAL (operands[3]) + (INTVAL (operands[4]) << 1);
  return templates[alt];
}
  [(set_attr "type" "dsp32")])

;; Three alternatives here to cover all possible allocations:
;; 0. mac flag is usable only for accumulator 1 - use A1 and odd DREG
;; 1. mac flag is usable for accumulator 0 - use A0 and even DREG
;; 2. mac flag is usable in any accumulator - use A1 and odd DREG
;; Other patterns which don't have a DREG destination can collapse cases
;; 1 and 2 into one.
(define_insn "flag_machi"
  [(set (match_operand:HI 0 "register_operand" "=W,D,W")
	(unspec:HI [(match_operand:HI 2 "register_operand" "d,d,d")
		    (match_operand:HI 3 "register_operand" "d,d,d")
		    (match_operand 4 "register_operand" "1,1,1")
		    (match_operand 5 "const01_operand" "P0P1,P0P1,P0P1")
		    (match_operand 6 "const_int_operand" "PB,PA,PA")]
		   UNSPEC_MAC_WITH_FLAG))
   (set (match_operand:PDI 1 "register_operand" "=B,A,B")
	(unspec:PDI [(match_dup 1) (match_dup 2) (match_dup 3)
		     (match_dup 4) (match_dup 5)]
		    UNSPEC_MAC_WITH_FLAG))]
  ""
  "%h0 = (%1 %b5 %h2 * %h3) %M6%!"
  [(set_attr "type" "dsp32")])

(define_insn "flag_machi_acconly"
  [(set (match_operand:PDI 0 "register_operand" "=B,e")
	(unspec:PDI [(match_operand:HI 1 "register_operand" "d,d")
		     (match_operand:HI 2 "register_operand" "d,d")
		     (match_operand 3 "register_operand" "0,0")
		     (match_operand 4 "const01_operand" "P0P1,P0P1")
		     (match_operand 5 "const_int_operand" "PB,PA")]
		    UNSPEC_MAC_WITH_FLAG))]
  ""
  "%0 %b4 %h1 * %h2 %M5%!"
  [(set_attr "type" "dsp32")])

(define_insn "flag_machi_parts_acconly"
  [(set (match_operand:PDI 0 "register_operand" "=B,e")
	(unspec:PDI [(vec_select:HI
		      (match_operand:V2HI 1 "register_operand" "d,d")
		      (parallel [(match_operand 3 "const01_operand" "P0P1,P0P1")]))
		     (vec_select:HI
		      (match_operand:V2HI 2 "register_operand" "d,d")
		      (parallel [(match_operand 4 "const01_operand" "P0P1,P0P1")]))
		     (match_operand:PDI 5 "register_operand" "0,0")
		     (match_operand 6 "const01_operand" "P0P1,P0P1")
		     (match_operand 7 "const_int_operand" "PB,PA")]
		    UNSPEC_MAC_WITH_FLAG))]
  ""
{
  const char *templates[] = {
    "%0 %b6 %h1 * %h2 %M7%!",
    "%0 %b6 %d1 * %h2 %M7%!",
    "%0 %b6 %h1 * %d2 %M7%!",
    "%0 %b6 %d1 * %d2 %M7%!"
  };
  int alt = INTVAL (operands[3]) + (INTVAL (operands[4]) << 1);
  return templates[alt];
}
  [(set_attr "type" "dsp32")])

(define_insn "flag_macinithi"
  [(set (match_operand:HI 0 "register_operand" "=W,D,W")
	(unspec:HI [(match_operand:HI 1 "register_operand" "d,d,d")
		    (match_operand:HI 2 "register_operand" "d,d,d")
		    (match_operand 3 "const_int_operand" "PB,PA,PA")]
		   UNSPEC_MAC_WITH_FLAG))
   (set (match_operand:PDI 4 "register_operand" "=B,A,B")
	(unspec:PDI [(match_dup 1) (match_dup 2) (match_dup 3)]
		    UNSPEC_MAC_WITH_FLAG))]
  ""
  "%h0 = (%4 = %h1 * %h2) %M3%!"
  [(set_attr "type" "dsp32")])

(define_insn "flag_macinit1hi"
  [(set (match_operand:PDI 0 "register_operand" "=B,e")
	(unspec:PDI [(match_operand:HI 1 "register_operand" "d,d")
		     (match_operand:HI 2 "register_operand" "d,d")
		     (match_operand 3 "const_int_operand" "PB,PA")]
		    UNSPEC_MAC_WITH_FLAG))]
  ""
  "%0 = %h1 * %h2 %M3%!"
  [(set_attr "type" "dsp32")])

(define_insn "mulv2hi3"
  [(set (match_operand:V2HI 0 "register_operand" "=d")
	(mult:V2HI (match_operand:V2HI 1 "register_operand" "d")
		   (match_operand:V2HI 2 "register_operand" "d")))]
  ""
  "%h0 = %h1 * %h2, %d0 = %d1 * %d2 (IS)%!"
  [(set_attr "type" "dsp32")])

(define_insn "flag_mulv2hi"
  [(set (match_operand:V2HI 0 "register_operand" "=d")
	(unspec:V2HI [(match_operand:V2HI 1 "register_operand" "d")
		      (match_operand:V2HI 2 "register_operand" "d")
		      (match_operand 3 "const_int_operand" "n")]
		     UNSPEC_MUL_WITH_FLAG))]
  ""
  "%h0 = %h1 * %h2, %d0 = %d1 * %d2 %M3%!"
  [(set_attr "type" "dsp32")])

(define_insn "flag_mulv2hi_parts"
  [(set (match_operand:V2HI 0 "register_operand" "=d")
	(unspec:V2HI [(vec_concat:V2HI
		       (vec_select:HI
			(match_operand:V2HI 1 "register_operand" "d")
			(parallel [(match_operand 3 "const01_operand" "P0P1")]))
		       (vec_select:HI
			(match_dup 1)
			(parallel [(match_operand 4 "const01_operand" "P0P1")])))
		      (vec_concat:V2HI
		       (vec_select:HI (match_operand:V2HI 2 "register_operand" "d")
			(parallel [(match_operand 5 "const01_operand" "P0P1")]))
		       (vec_select:HI (match_dup 2)
			(parallel [(match_operand 6 "const01_operand" "P0P1")])))
		      (match_operand 7 "const_int_operand" "n")]
		     UNSPEC_MUL_WITH_FLAG))]
  ""
{
  const char *templates[] = {
    "%h0 = %h1 * %h2, %d0 = %h1 * %h2 %M7%!",
    "%h0 = %d1 * %h2, %d0 = %h1 * %h2 %M7%!",
    "%h0 = %h1 * %h2, %d0 = %d1 * %h2 %M7%!",
    "%h0 = %d1 * %h2, %d0 = %d1 * %h2 %M7%!",
    "%h0 = %h1 * %d2, %d0 = %h1 * %h2 %M7%!",
    "%h0 = %d1 * %d2, %d0 = %h1 * %h2 %M7%!",
    "%h0 = %h1 * %d2, %d0 = %d1 * %h2 %M7%!",
    "%h0 = %d1 * %d2, %d0 = %d1 * %h2 %M7%!",
    "%h0 = %h1 * %h2, %d0 = %h1 * %d2 %M7%!",
    "%h0 = %d1 * %h2, %d0 = %h1 * %d2 %M7%!",
    "%h0 = %h1 * %h2, %d0 = %d1 * %d2 %M7%!",
    "%h0 = %d1 * %h2, %d0 = %d1 * %d2 %M7%!",
    "%h0 = %h1 * %d2, %d0 = %h1 * %d2 %M7%!",
    "%h0 = %d1 * %d2, %d0 = %h1 * %d2 %M7%!",
    "%h0 = %h1 * %d2, %d0 = %d1 * %d2 %M7%!",
    "%h0 = %d1 * %d2, %d0 = %d1 * %d2 %M7%!" };
  int alt = (INTVAL (operands[3]) + (INTVAL (operands[4]) << 1)
	     + (INTVAL (operands[5]) << 2)  + (INTVAL (operands[6]) << 3));
  return templates[alt];
}
  [(set_attr "type" "dsp32")])

;; A slightly complicated pattern.
;; Operand 0 is the halfword output; operand 11 is the accumulator output
;; Halfword inputs are operands 1 and 2; operands 3, 4, 5 and 6 specify which
;; parts of these 2x16 bit registers to use.
;; Operand 7 is the accumulator input.
;; Operands 8/9 specify whether low/high parts are mac (0) or msu (1)
;; Operand 10 is the macflag to be used.
(define_insn "flag_macv2hi_parts"
  [(set (match_operand:V2HI 0 "register_operand" "=d")
	(unspec:V2HI [(vec_concat:V2HI
		       (vec_select:HI
			(match_operand:V2HI 1 "register_operand" "d")
			(parallel [(match_operand 3 "const01_operand" "P0P1")]))
		       (vec_select:HI
			(match_dup 1)
			(parallel [(match_operand 4 "const01_operand" "P0P1")])))
		      (vec_concat:V2HI
		       (vec_select:HI (match_operand:V2HI 2 "register_operand" "d")
			(parallel [(match_operand 5 "const01_operand" "P0P1")]))
		       (vec_select:HI (match_dup 2)
			(parallel [(match_operand 6 "const01_operand" "P0P1")])))
		      (match_operand:V2PDI 7 "register_operand" "e")
		      (match_operand 8 "const01_operand" "P0P1")
		      (match_operand 9 "const01_operand" "P0P1")
		      (match_operand 10 "const_int_operand" "n")]
		     UNSPEC_MAC_WITH_FLAG))
   (set (match_operand:V2PDI 11 "register_operand" "=e")
	(unspec:V2PDI [(vec_concat:V2HI
			(vec_select:HI (match_dup 1) (parallel [(match_dup 3)]))
			(vec_select:HI (match_dup 1) (parallel [(match_dup 4)])))
		       (vec_concat:V2HI
			(vec_select:HI (match_dup 2) (parallel [(match_dup 5)]))
			(vec_select:HI (match_dup 2) (parallel [(match_dup 5)])))
		       (match_dup 7) (match_dup 8) (match_dup 9) (match_dup 10)]
		      UNSPEC_MAC_WITH_FLAG))]
  ""
{
  const char *templates[] = {
    "%h0 = (A0 %b8 %h1 * %h2), %d0 = (A1 %b9 %h1 * %h2) %M10%!",
    "%h0 = (A0 %b8 %d1 * %h2), %d0 = (A1 %b9 %h1 * %h2) %M10%!",
    "%h0 = (A0 %b8 %h1 * %h2), %d0 = (A1 %b9 %d1 * %h2) %M10%!",
    "%h0 = (A0 %b8 %d1 * %h2), %d0 = (A1 %b9 %d1 * %h2) %M10%!",
    "%h0 = (A0 %b8 %h1 * %d2), %d0 = (A1 %b9 %h1 * %h2) %M10%!",
    "%h0 = (A0 %b8 %d1 * %d2), %d0 = (A1 %b9 %h1 * %h2) %M10%!",
    "%h0 = (A0 %b8 %h1 * %d2), %d0 = (A1 %b9 %d1 * %h2) %M10%!",
    "%h0 = (A0 %b8 %d1 * %d2), %d0 = (A1 %b9 %d1 * %h2) %M10%!",
    "%h0 = (A0 %b8 %h1 * %h2), %d0 = (A1 %b9 %h1 * %d2) %M10%!",
    "%h0 = (A0 %b8 %d1 * %h2), %d0 = (A1 %b9 %h1 * %d2) %M10%!",
    "%h0 = (A0 %b8 %h1 * %h2), %d0 = (A1 %b9 %d1 * %d2) %M10%!",
    "%h0 = (A0 %b8 %d1 * %h2), %d0 = (A1 %b9 %d1 * %d2) %M10%!",
    "%h0 = (A0 %b8 %h1 * %d2), %d0 = (A1 %b9 %h1 * %d2) %M10%!",
    "%h0 = (A0 %b8 %d1 * %d2), %d0 = (A1 %b9 %h1 * %d2) %M10%!",
    "%h0 = (A0 %b8 %h1 * %d2), %d0 = (A1 %b9 %d1 * %d2) %M10%!",
    "%h0 = (A0 %b8 %d1 * %d2), %d0 = (A1 %b9 %d1 * %d2) %M10%!" };
  int alt = (INTVAL (operands[3]) + (INTVAL (operands[4]) << 1)
	     + (INTVAL (operands[5]) << 2)  + (INTVAL (operands[6]) << 3));
  return templates[alt];
}
  [(set_attr "type" "dsp32")])

(define_insn "flag_macv2hi_parts_acconly"
  [(set (match_operand:V2PDI 0 "register_operand" "=e")
	(unspec:V2PDI [(vec_concat:V2HI
			(vec_select:HI
			 (match_operand:V2HI 1 "register_operand" "d")
			 (parallel [(match_operand 3 "const01_operand" "P0P1")]))
			(vec_select:HI
			 (match_dup 1)
			 (parallel [(match_operand 4 "const01_operand" "P0P1")])))
		       (vec_concat:V2HI
			(vec_select:HI (match_operand:V2HI 2 "register_operand" "d")
				       (parallel [(match_operand 5 "const01_operand" "P0P1")]))
			(vec_select:HI (match_dup 2)
				       (parallel [(match_operand 6 "const01_operand" "P0P1")])))
		       (match_operand:V2PDI 7 "register_operand" "e")
		       (match_operand 8 "const01_operand" "P0P1")
		       (match_operand 9 "const01_operand" "P0P1")
		       (match_operand 10 "const_int_operand" "n")]
		      UNSPEC_MAC_WITH_FLAG))]
  ""
{
  const char *templates[] = {
    "A0 %b8 %h1 * %h2, A1 %b9 %h1 * %h2 %M10%!",
    "A0 %b8 %d1 * %h2, A1 %b9 %h1 * %h2 %M10%!",
    "A0 %b8 %h1 * %h2, A1 %b9 %d1 * %h2 %M10%!",
    "A0 %b8 %d1 * %h2, A1 %b9 %d1 * %h2 %M10%!",
    "A0 %b8 %h1 * %d2, A1 %b9 %h1 * %h2 %M10%!",
    "A0 %b8 %d1 * %d2, A1 %b9 %h1 * %h2 %M10%!",
    "A0 %b8 %h1 * %d2, A1 %b9 %d1 * %h2 %M10%!",
    "A0 %b8 %d1 * %d2, A1 %b9 %d1 * %h2 %M10%!",
    "A0 %b8 %h1 * %h2, A1 %b9 %h1 * %d2 %M10%!",
    "A0 %b8 %d1 * %h2, A1 %b9 %h1 * %d2 %M10%!",
    "A0 %b8 %h1 * %h2, A1 %b9 %d1 * %d2 %M10%!",
    "A0 %b8 %d1 * %h2, A1 %b9 %d1 * %d2 %M10%!",
    "A0 %b8 %h1 * %d2, A1 %b9 %h1 * %d2 %M10%!",
    "A0 %b8 %d1 * %d2, A1 %b9 %h1 * %d2 %M10%!",
    "A0 %b8 %h1 * %d2, A1 %b9 %d1 * %d2 %M10%!",
    "A0 %b8 %d1 * %d2, A1 %b9 %d1 * %d2 %M10%!" };
  int alt = (INTVAL (operands[3]) + (INTVAL (operands[4]) << 1)
	     + (INTVAL (operands[5]) << 2)  + (INTVAL (operands[6]) << 3));
  return templates[alt];
}
  [(set_attr "type" "dsp32")])

;; Same as above, but initializing the accumulators and therefore a couple fewer
;; necessary operands.
(define_insn "flag_macinitv2hi_parts"
  [(set (match_operand:V2HI 0 "register_operand" "=d")
	(unspec:V2HI [(vec_concat:V2HI
		       (vec_select:HI
			(match_operand:V2HI 1 "register_operand" "d")
			(parallel [(match_operand 3 "const01_operand" "P0P1")]))
		       (vec_select:HI
			(match_dup 1)
			(parallel [(match_operand 4 "const01_operand" "P0P1")])))
		      (vec_concat:V2HI
		       (vec_select:HI (match_operand:V2HI 2 "register_operand" "d")
			(parallel [(match_operand 5 "const01_operand" "P0P1")]))
		       (vec_select:HI (match_dup 2)
			(parallel [(match_operand 6 "const01_operand" "P0P1")])))
		      (match_operand 7 "const_int_operand" "n")]
		     UNSPEC_MAC_WITH_FLAG))
   (set (match_operand:V2PDI 8 "register_operand" "=e")
	(unspec:V2PDI [(vec_concat:V2HI
			(vec_select:HI (match_dup 1) (parallel [(match_dup 3)]))
			(vec_select:HI (match_dup 1) (parallel [(match_dup 4)])))
		       (vec_concat:V2HI
			(vec_select:HI (match_dup 2) (parallel [(match_dup 5)]))
			(vec_select:HI (match_dup 2) (parallel [(match_dup 5)])))
		       (match_dup 7)]
		      UNSPEC_MAC_WITH_FLAG))]
  ""
{
  const char *templates[] = {
    "%h0 = (A0 = %h1 * %h2), %d0 = (A1 = %h1 * %h2) %M7%!",
    "%h0 = (A0 = %d1 * %h2), %d0 = (A1 = %h1 * %h2) %M7%!",
    "%h0 = (A0 = %h1 * %h2), %d0 = (A1 = %d1 * %h2) %M7%!",
    "%h0 = (A0 = %d1 * %h2), %d0 = (A1 = %d1 * %h2) %M7%!",
    "%h0 = (A0 = %h1 * %d2), %d0 = (A1 = %h1 * %h2) %M7%!",
    "%h0 = (A0 = %d1 * %d2), %d0 = (A1 = %h1 * %h2) %M7%!",
    "%h0 = (A0 = %h1 * %d2), %d0 = (A1 = %d1 * %h2) %M7%!",
    "%h0 = (A0 = %d1 * %d2), %d0 = (A1 = %d1 * %h2) %M7%!",
    "%h0 = (A0 = %h1 * %h2), %d0 = (A1 = %h1 * %d2) %M7%!",
    "%h0 = (A0 = %d1 * %h2), %d0 = (A1 = %h1 * %d2) %M7%!",
    "%h0 = (A0 = %h1 * %h2), %d0 = (A1 = %d1 * %d2) %M7%!",
    "%h0 = (A0 = %d1 * %h2), %d0 = (A1 = %d1 * %d2) %M7%!",
    "%h0 = (A0 = %h1 * %d2), %d0 = (A1 = %h1 * %d2) %M7%!",
    "%h0 = (A0 = %d1 * %d2), %d0 = (A1 = %h1 * %d2) %M7%!",
    "%h0 = (A0 = %h1 * %d2), %d0 = (A1 = %d1 * %d2) %M7%!",
    "%h0 = (A0 = %d1 * %d2), %d0 = (A1 = %d1 * %d2) %M7%!" };
  int alt = (INTVAL (operands[3]) + (INTVAL (operands[4]) << 1)
	     + (INTVAL (operands[5]) << 2)  + (INTVAL (operands[6]) << 3));
  return templates[alt];
}
  [(set_attr "type" "dsp32")])

(define_insn "flag_macinit1v2hi_parts"
  [(set (match_operand:V2PDI 0 "register_operand" "=e")
	(unspec:V2PDI [(vec_concat:V2HI
		       (vec_select:HI
			(match_operand:V2HI 1 "register_operand" "d")
			(parallel [(match_operand 3 "const01_operand" "P0P1")]))
		       (vec_select:HI
			(match_dup 1)
			(parallel [(match_operand 4 "const01_operand" "P0P1")])))
		      (vec_concat:V2HI
		       (vec_select:HI (match_operand:V2HI 2 "register_operand" "d")
			(parallel [(match_operand 5 "const01_operand" "P0P1")]))
		       (vec_select:HI (match_dup 2)
			(parallel [(match_operand 6 "const01_operand" "P0P1")])))
		      (match_operand 7 "const_int_operand" "n")]
		     UNSPEC_MAC_WITH_FLAG))]
  ""
{
  const char *templates[] = {
    "A0 = %h1 * %h2, A1 = %h1 * %h2 %M7%!",
    "A0 = %d1 * %h2, A1 = %h1 * %h2 %M7%!",
    "A0 = %h1 * %h2, A1 = %d1 * %h2 %M7%!",
    "A0 = %d1 * %h2, A1 = %d1 * %h2 %M7%!",
    "A0 = %h1 * %d2, A1 = %h1 * %h2 %M7%!",
    "A0 = %d1 * %d2, A1 = %h1 * %h2 %M7%!",
    "A0 = %h1 * %d2, A1 = %d1 * %h2 %M7%!",
    "A0 = %d1 * %d2, A1 = %d1 * %h2 %M7%!",
    "A0 = %h1 * %h2, A1 = %h1 * %d2 %M7%!",
    "A0 = %d1 * %h2, A1 = %h1 * %d2 %M7%!",
    "A0 = %h1 * %h2, A1 = %d1 * %d2 %M7%!",
    "A0 = %d1 * %h2, A1 = %d1 * %d2 %M7%!",
    "A0 = %h1 * %d2, A1 = %h1 * %d2 %M7%!",
    "A0 = %d1 * %d2, A1 = %h1 * %d2 %M7%!",
    "A0 = %h1 * %d2, A1 = %d1 * %d2 %M7%!",
    "A0 = %d1 * %d2, A1 = %d1 * %d2 %M7%!" };
  int alt = (INTVAL (operands[3]) + (INTVAL (operands[4]) << 1)
	     + (INTVAL (operands[5]) << 2)  + (INTVAL (operands[6]) << 3));
  return templates[alt];
}
  [(set_attr "type" "dsp32")])

;; A mixture of multiply and multiply-accumulate for when we only want to
;; initialize one part.
(define_insn "flag_mul_macv2hi_parts_acconly"
  [(set (match_operand:PDI 0 "register_operand" "=B,e,e")
	(unspec:PDI [(vec_select:HI
		      (match_operand:V2HI 2 "register_operand" "d,d,d")
		      (parallel [(match_operand 4 "const01_operand" "P0P1,P0P1,P0P1")]))
		     (vec_select:HI
		      (match_operand:V2HI 3 "register_operand" "d,d,d")
		      (parallel [(match_operand 6 "const01_operand" "P0P1,P0P1,P0P1")]))
		     (match_operand 10 "const_int_operand" "PB,PA,PA")]
		    UNSPEC_MUL_WITH_FLAG))
   (set (match_operand:PDI 1 "register_operand" "=B,e,e")
	(unspec:PDI [(vec_select:HI
		      (match_dup 2)
		      (parallel [(match_operand 5 "const01_operand" "P0P1,P0P1,P0P1")]))
		     (vec_select:HI
		      (match_dup 3)
		      (parallel [(match_operand 7 "const01_operand" "P0P1,P0P1,P0P1")]))
		     (match_operand:PDI 8 "register_operand" "1,1,1")
		     (match_operand 9 "const01_operand" "P0P1,P0P1,P0P1")
		     (match_operand 11 "const_int_operand" "PA,PB,PA")]
		    UNSPEC_MAC_WITH_FLAG))]
  "MACFLAGS_MATCH_P (INTVAL (operands[10]), INTVAL (operands[11]))"
{
  rtx xops[6];
  const char *templates[] = {
    "%0 = %h2 * %h3, %1 %b4 %h2 * %h3 %M5%!",
    "%0 = %d2 * %h3, %1 %b4 %h2 * %h3 %M5%!",
    "%0 = %h2 * %h3, %1 %b4 %d2 * %h3 %M5%!",
    "%0 = %d2 * %h3, %1 %b4 %d2 * %h3 %M5%!",
    "%0 = %h2 * %d3, %1 %b4 %h2 * %h3 %M5%!",
    "%0 = %d2 * %d3, %1 %b4 %h2 * %h3 %M5%!",
    "%0 = %h2 * %d3, %1 %b4 %d2 * %h3 %M5%!",
    "%0 = %d2 * %d3, %1 %b4 %d2 * %h3 %M5%!",
    "%0 = %h2 * %h3, %1 %b4 %h2 * %d3 %M5%!",
    "%0 = %d2 * %h3, %1 %b4 %h2 * %d3 %M5%!",
    "%0 = %h2 * %h3, %1 %b4 %d2 * %d3 %M5%!",
    "%0 = %d2 * %h3, %1 %b4 %d2 * %d3 %M5%!",
    "%0 = %h2 * %d3, %1 %b4 %h2 * %d3 %M5%!",
    "%0 = %d2 * %d3, %1 %b4 %h2 * %d3 %M5%!",
    "%0 = %h2 * %d3, %1 %b4 %d2 * %d3 %M5%!",
    "%0 = %d2 * %d3, %1 %b4 %d2 * %d3 %M5%!" };
  int alt = (INTVAL (operands[4]) + (INTVAL (operands[5]) << 1)
	     + (INTVAL (operands[6]) << 2)  + (INTVAL (operands[7]) << 3));
  xops[0] = operands[0];
  xops[1] = operands[1];
  xops[2] = operands[2];
  xops[3] = operands[3];
  xops[4] = operands[9];
  xops[5] = which_alternative == 0 ? operands[10] : operands[11];
  output_asm_insn (templates[alt], xops);
  return "";
}
  [(set_attr "type" "dsp32")])


(define_code_iterator s_or_u [sign_extend zero_extend])
(define_code_attr su_optab [(sign_extend "mul")
			    (zero_extend "umul")])
(define_code_attr su_modifier [(sign_extend "IS")
			       (zero_extend "FU")])

(define_insn "<su_optab>hisi_ll"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(mult:SI (s_or_u:SI
		  (vec_select:HI (match_operand:V2HI 1 "register_operand" "%d")
				 (parallel [(const_int 0)])))
		 (s_or_u:SI
		  (vec_select:HI (match_operand:V2HI 2 "register_operand" "d")
				 (parallel [(const_int 0)])))))]
  ""
  "%0 = %h1 * %h2 (<su_modifier>)%!"
  [(set_attr "type" "dsp32")])

(define_insn "<su_optab>hisi_lh"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(mult:SI (s_or_u:SI
		  (vec_select:HI (match_operand:V2HI 1 "register_operand" "d")
				 (parallel [(const_int 0)])))
		 (s_or_u:SI
		  (vec_select:HI (match_operand:V2HI 2 "register_operand" "d")
				 (parallel [(const_int 1)])))))]
  ""
  "%0 = %h1 * %d2 (<su_modifier>)%!"
  [(set_attr "type" "dsp32")])

(define_insn "<su_optab>hisi_hl"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(mult:SI (s_or_u:SI
		  (vec_select:HI (match_operand:V2HI 1 "register_operand" "d")
				 (parallel [(const_int 1)])))
		 (s_or_u:SI
		  (vec_select:HI (match_operand:V2HI 2 "register_operand" "d")
				 (parallel [(const_int 0)])))))]
  ""
  "%0 = %d1 * %h2 (<su_modifier>)%!"
  [(set_attr "type" "dsp32")])

(define_insn "<su_optab>hisi_hh"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(mult:SI (s_or_u:SI
		  (vec_select:HI (match_operand:V2HI 1 "register_operand" "%d")
				 (parallel [(const_int 1)])))
		 (s_or_u:SI
		  (vec_select:HI (match_operand:V2HI 2 "register_operand" "d")
				 (parallel [(const_int 1)])))))]
  ""
  "%0 = %d1 * %d2 (<su_modifier>)%!"
  [(set_attr "type" "dsp32")])

;; Additional variants for signed * unsigned multiply.

(define_insn "usmulhisi_ull"
  [(set (match_operand:SI 0 "register_operand" "=W")
	(mult:SI (zero_extend:SI
		  (vec_select:HI (match_operand:V2HI 1 "register_operand" "%d")
				 (parallel [(const_int 0)])))
		 (sign_extend:SI
		  (vec_select:HI (match_operand:V2HI 2 "register_operand" "d")
				 (parallel [(const_int 0)])))))]
  ""
  "%0 = %h2 * %h1 (IS,M)%!"
  [(set_attr "type" "dsp32")])

(define_insn "usmulhisi_ulh"
  [(set (match_operand:SI 0 "register_operand" "=W")
	(mult:SI (zero_extend:SI
		  (vec_select:HI (match_operand:V2HI 1 "register_operand" "d")
				 (parallel [(const_int 0)])))
		 (sign_extend:SI
		  (vec_select:HI (match_operand:V2HI 2 "register_operand" "d")
				 (parallel [(const_int 1)])))))]
  ""
  "%0 = %d2 * %h1 (IS,M)%!"
  [(set_attr "type" "dsp32")])

(define_insn "usmulhisi_uhl"
  [(set (match_operand:SI 0 "register_operand" "=W")
	(mult:SI (zero_extend:SI
		  (vec_select:HI (match_operand:V2HI 1 "register_operand" "d")
				 (parallel [(const_int 1)])))
		 (sign_extend:SI
		  (vec_select:HI (match_operand:V2HI 2 "register_operand" "d")
				 (parallel [(const_int 0)])))))]
  ""
  "%0 = %h2 * %d1 (IS,M)%!"
  [(set_attr "type" "dsp32")])

(define_insn "usmulhisi_uhh"
  [(set (match_operand:SI 0 "register_operand" "=W")
	(mult:SI (zero_extend:SI
		  (vec_select:HI (match_operand:V2HI 1 "register_operand" "%d")
				 (parallel [(const_int 1)])))
		 (sign_extend:SI
		  (vec_select:HI (match_operand:V2HI 2 "register_operand" "d")
				 (parallel [(const_int 1)])))))]
  ""
  "%0 = %d2 * %d1 (IS,M)%!"
  [(set_attr "type" "dsp32")])

;; Parallel versions of these operations.  First, normal signed or unsigned
;; multiplies.

(define_insn "<su_optab>hisi_ll_lh"
  [(set (match_operand:SI 0 "register_operand" "=q0,q2,q4,q6")
	(mult:SI (s_or_u:SI
		  (vec_select:HI (match_operand:V2HI 1 "register_operand" "d,d,d,d")
				 (parallel [(const_int 0)])))
		 (s_or_u:SI
		  (vec_select:HI (match_operand:V2HI 2 "register_operand" "d,d,d,d")
				 (parallel [(const_int 0)])))))
   (set (match_operand:SI 3 "register_operand" "=q1,q3,q5,q7")
	(mult:SI (s_or_u:SI
		  (vec_select:HI (match_dup 1) (parallel [(const_int 0)])))
		 (s_or_u:SI
		  (vec_select:HI (match_dup 2) (parallel [(const_int 1)])))))]
  ""
  "%0 = %h1 * %h2, %3 = %h1 * %d2 (<su_modifier>)%!"
  [(set_attr "type" "dsp32")])

(define_insn "<su_optab>hisi_ll_hl"
  [(set (match_operand:SI 0 "register_operand" "=q0,q2,q4,q6")
	(mult:SI (s_or_u:SI
		  (vec_select:HI (match_operand:V2HI 1 "register_operand" "d,d,d,d")
				 (parallel [(const_int 0)])))
		 (s_or_u:SI
		  (vec_select:HI (match_operand:V2HI 2 "register_operand" "d,d,d,d")
				 (parallel [(const_int 0)])))))
   (set (match_operand:SI 3 "register_operand" "=q1,q3,q5,q7")
	(mult:SI (s_or_u:SI
		  (vec_select:HI (match_dup 1) (parallel [(const_int 1)])))
		 (s_or_u:SI
		  (vec_select:HI (match_dup 2) (parallel [(const_int 0)])))))]
  ""
  "%0 = %h1 * %h2, %3 = %d1 * %h2 (<su_modifier>)%!"
  [(set_attr "type" "dsp32")])

(define_insn "<su_optab>hisi_ll_hh"
  [(set (match_operand:SI 0 "register_operand" "=q0,q2,q4,q6")
	(mult:SI (s_or_u:SI
		  (vec_select:HI (match_operand:V2HI 1 "register_operand" "d,d,d,d")
				 (parallel [(const_int 0)])))
		 (s_or_u:SI
		  (vec_select:HI (match_operand:V2HI 2 "register_operand" "d,d,d,d")
				 (parallel [(const_int 0)])))))
   (set (match_operand:SI 3 "register_operand" "=q1,q3,q5,q7")
	(mult:SI (s_or_u:SI
		  (vec_select:HI (match_dup 1) (parallel [(const_int 1)])))
		 (s_or_u:SI
		  (vec_select:HI (match_dup 2) (parallel [(const_int 1)])))))]
  ""
  "%0 = %h1 * %h2, %3 = %d1 * %d2 (<su_modifier>)%!"
  [(set_attr "type" "dsp32")])

(define_insn "<su_optab>hisi_lh_hl"
  [(set (match_operand:SI 0 "register_operand" "=q0,q2,q4,q6")
	(mult:SI (s_or_u:SI
		  (vec_select:HI (match_operand:V2HI 1 "register_operand" "d,d,d,d")
				 (parallel [(const_int 0)])))
		 (s_or_u:SI
		  (vec_select:HI (match_operand:V2HI 2 "register_operand" "d,d,d,d")
				 (parallel [(const_int 1)])))))
   (set (match_operand:SI 3 "register_operand" "=q1,q3,q5,q7")
	(mult:SI (s_or_u:SI
		  (vec_select:HI (match_dup 1) (parallel [(const_int 1)])))
		 (s_or_u:SI
		  (vec_select:HI (match_dup 2) (parallel [(const_int 0)])))))]
  ""
  "%0 = %h1 * %d2, %3 = %d1 * %h2 (<su_modifier>)%!"
  [(set_attr "type" "dsp32")])

(define_insn "<su_optab>hisi_lh_hh"
  [(set (match_operand:SI 0 "register_operand" "=q0,q2,q4,q6")
	(mult:SI (s_or_u:SI
		  (vec_select:HI (match_operand:V2HI 1 "register_operand" "d,d,d,d")
				 (parallel [(const_int 0)])))
		 (s_or_u:SI
		  (vec_select:HI (match_operand:V2HI 2 "register_operand" "d,d,d,d")
				 (parallel [(const_int 1)])))))
   (set (match_operand:SI 3 "register_operand" "=q1,q3,q5,q7")
	(mult:SI (s_or_u:SI
		  (vec_select:HI (match_dup 1) (parallel [(const_int 1)])))
		 (s_or_u:SI
		  (vec_select:HI (match_dup 2) (parallel [(const_int 1)])))))]
  ""
  "%0 = %h1 * %d2, %3 = %d1 * %d2 (<su_modifier>)%!"
  [(set_attr "type" "dsp32")])

(define_insn "<su_optab>hisi_hl_hh"
  [(set (match_operand:SI 0 "register_operand" "=q0,q2,q4,q6")
	(mult:SI (s_or_u:SI
		  (vec_select:HI (match_operand:V2HI 1 "register_operand" "d,d,d,d")
				 (parallel [(const_int 1)])))
		 (s_or_u:SI
		  (vec_select:HI (match_operand:V2HI 2 "register_operand" "d,d,d,d")
				 (parallel [(const_int 0)])))))
   (set (match_operand:SI 3 "register_operand" "=q1,q3,q5,q7")
	(mult:SI (s_or_u:SI
		  (vec_select:HI (match_dup 1) (parallel [(const_int 1)])))
		 (s_or_u:SI
		  (vec_select:HI (match_dup 2) (parallel [(const_int 1)])))))]
  ""
  "%0 = %d1 * %h2, %3 = %d1 * %d2 (<su_modifier>)%!"
  [(set_attr "type" "dsp32")])

;; Special signed * unsigned variants.

(define_insn "usmulhisi_ll_lul"
  [(set (match_operand:SI 0 "register_operand" "=q0,q2,q4,q6")
	(mult:SI (sign_extend:SI
		  (vec_select:HI (match_operand:V2HI 1 "register_operand" "d,d,d,d")
				 (parallel [(const_int 0)])))
		 (sign_extend:SI
		  (vec_select:HI (match_operand:V2HI 2 "register_operand" "d,d,d,d")
				 (parallel [(const_int 0)])))))
   (set (match_operand:SI 3 "register_operand" "=q1,q3,q5,q7")
	(mult:SI (sign_extend:SI
		  (vec_select:HI (match_dup 1) (parallel [(const_int 0)])))
		 (zero_extend:SI
		  (vec_select:HI (match_dup 2) (parallel [(const_int 0)])))))]
  ""
  "%0 = %h1 * %h2, %3 = %h1 * %h2 (IS,M)%!"
  [(set_attr "type" "dsp32")])

(define_insn "usmulhisi_ll_luh"
  [(set (match_operand:SI 0 "register_operand" "=q0,q2,q4,q6")
	(mult:SI (sign_extend:SI
		  (vec_select:HI (match_operand:V2HI 1 "register_operand" "d,d,d,d")
				 (parallel [(const_int 0)])))
		 (sign_extend:SI
		  (vec_select:HI (match_operand:V2HI 2 "register_operand" "d,d,d,d")
				 (parallel [(const_int 0)])))))
   (set (match_operand:SI 3 "register_operand" "=q1,q3,q5,q7")
	(mult:SI (sign_extend:SI
		  (vec_select:HI (match_dup 1) (parallel [(const_int 0)])))
		 (zero_extend:SI
		  (vec_select:HI (match_dup 2) (parallel [(const_int 1)])))))]
  ""
  "%0 = %h1 * %h2, %3 = %h1 * %d2 (IS,M)%!"
  [(set_attr "type" "dsp32")])

(define_insn "usmulhisi_ll_hul"
  [(set (match_operand:SI 0 "register_operand" "=q0,q2,q4,q6")
	(mult:SI (sign_extend:SI
		  (vec_select:HI (match_operand:V2HI 1 "register_operand" "d,d,d,d")
				 (parallel [(const_int 0)])))
		 (sign_extend:SI
		  (vec_select:HI (match_operand:V2HI 2 "register_operand" "d,d,d,d")
				 (parallel [(const_int 0)])))))
   (set (match_operand:SI 3 "register_operand" "=q1,q3,q5,q7")
	(mult:SI (sign_extend:SI
		  (vec_select:HI (match_dup 1) (parallel [(const_int 1)])))
		 (zero_extend:SI
		  (vec_select:HI (match_dup 2) (parallel [(const_int 0)])))))]
  ""
  "%0 = %h1 * %h2, %3 = %d1 * %h2 (IS,M)%!"
  [(set_attr "type" "dsp32")])

(define_insn "usmulhisi_ll_huh"
  [(set (match_operand:SI 0 "register_operand" "=q0,q2,q4,q6")
	(mult:SI (sign_extend:SI
		  (vec_select:HI (match_operand:V2HI 1 "register_operand" "d,d,d,d")
				 (parallel [(const_int 0)])))
		 (sign_extend:SI
		  (vec_select:HI (match_operand:V2HI 2 "register_operand" "d,d,d,d")
				 (parallel [(const_int 0)])))))
   (set (match_operand:SI 3 "register_operand" "=q1,q3,q5,q7")
	(mult:SI (sign_extend:SI
		  (vec_select:HI (match_dup 1) (parallel [(const_int 1)])))
		 (zero_extend:SI
		  (vec_select:HI (match_dup 2) (parallel [(const_int 1)])))))]
  ""
  "%0 = %h1 * %h2, %3 = %d1 * %d2 (IS,M)%!"
  [(set_attr "type" "dsp32")])

(define_insn "usmulhisi_lh_lul"
  [(set (match_operand:SI 0 "register_operand" "=q0,q2,q4,q6")
	(mult:SI (sign_extend:SI
		  (vec_select:HI (match_operand:V2HI 1 "register_operand" "d,d,d,d")
				 (parallel [(const_int 0)])))
		 (sign_extend:SI
		  (vec_select:HI (match_operand:V2HI 2 "register_operand" "d,d,d,d")
				 (parallel [(const_int 1)])))))
   (set (match_operand:SI 3 "register_operand" "=q1,q3,q5,q7")
	(mult:SI (sign_extend:SI
		  (vec_select:HI (match_dup 1) (parallel [(const_int 0)])))
		 (zero_extend:SI
		  (vec_select:HI (match_dup 2) (parallel [(const_int 0)])))))]
  ""
  "%0 = %h1 * %d2, %3 = %h1 * %h2 (IS,M)%!"
  [(set_attr "type" "dsp32")])

(define_insn "usmulhisi_lh_luh"
  [(set (match_operand:SI 0 "register_operand" "=q0,q2,q4,q6")
	(mult:SI (sign_extend:SI
		  (vec_select:HI (match_operand:V2HI 1 "register_operand" "d,d,d,d")
				 (parallel [(const_int 0)])))
		 (sign_extend:SI
		  (vec_select:HI (match_operand:V2HI 2 "register_operand" "d,d,d,d")
				 (parallel [(const_int 1)])))))
   (set (match_operand:SI 3 "register_operand" "=q1,q3,q5,q7")
	(mult:SI (sign_extend:SI
		  (vec_select:HI (match_dup 1) (parallel [(const_int 0)])))
		 (zero_extend:SI
		  (vec_select:HI (match_dup 2) (parallel [(const_int 1)])))))]
  ""
  "%0 = %h1 * %d2, %3 = %h1 * %d2 (IS,M)%!"
  [(set_attr "type" "dsp32")])

(define_insn "usmulhisi_lh_hul"
  [(set (match_operand:SI 0 "register_operand" "=q0,q2,q4,q6")
	(mult:SI (sign_extend:SI
		  (vec_select:HI (match_operand:V2HI 1 "register_operand" "d,d,d,d")
				 (parallel [(const_int 0)])))
		 (sign_extend:SI
		  (vec_select:HI (match_operand:V2HI 2 "register_operand" "d,d,d,d")
				 (parallel [(const_int 1)])))))
   (set (match_operand:SI 3 "register_operand" "=q1,q3,q5,q7")
	(mult:SI (sign_extend:SI
		  (vec_select:HI (match_dup 1) (parallel [(const_int 1)])))
		 (zero_extend:SI
		  (vec_select:HI (match_dup 2) (parallel [(const_int 0)])))))]
  ""
  "%0 = %h1 * %d2, %3 = %d1 * %h2 (IS,M)%!"
  [(set_attr "type" "dsp32")])

(define_insn "usmulhisi_lh_huh"
  [(set (match_operand:SI 0 "register_operand" "=q0,q2,q4,q6")
	(mult:SI (sign_extend:SI
		  (vec_select:HI (match_operand:V2HI 1 "register_operand" "d,d,d,d")
				 (parallel [(const_int 0)])))
		 (sign_extend:SI
		  (vec_select:HI (match_operand:V2HI 2 "register_operand" "d,d,d,d")
				 (parallel [(const_int 1)])))))
   (set (match_operand:SI 3 "register_operand" "=q1,q3,q5,q7")
	(mult:SI (sign_extend:SI
		  (vec_select:HI (match_dup 1) (parallel [(const_int 1)])))
		 (zero_extend:SI
		  (vec_select:HI (match_dup 2) (parallel [(const_int 1)])))))]
  ""
  "%0 = %h1 * %d2, %3 = %d1 * %d2 (IS,M)%!"
  [(set_attr "type" "dsp32")])

(define_insn "usmulhisi_hl_lul"
  [(set (match_operand:SI 0 "register_operand" "=q0,q2,q4,q6")
	(mult:SI (sign_extend:SI
		  (vec_select:HI (match_operand:V2HI 1 "register_operand" "d,d,d,d")
				 (parallel [(const_int 1)])))
		 (sign_extend:SI
		  (vec_select:HI (match_operand:V2HI 2 "register_operand" "d,d,d,d")
				 (parallel [(const_int 0)])))))
   (set (match_operand:SI 3 "register_operand" "=q1,q3,q5,q7")
	(mult:SI (sign_extend:SI
		  (vec_select:HI (match_dup 1) (parallel [(const_int 0)])))
		 (zero_extend:SI
		  (vec_select:HI (match_dup 2) (parallel [(const_int 0)])))))]
  ""
  "%0 = %d1 * %h2, %3 = %h1 * %h2 (IS,M)%!"
  [(set_attr "type" "dsp32")])

(define_insn "usmulhisi_hl_luh"
  [(set (match_operand:SI 0 "register_operand" "=q0,q2,q4,q6")
	(mult:SI (sign_extend:SI
		  (vec_select:HI (match_operand:V2HI 1 "register_operand" "d,d,d,d")
				 (parallel [(const_int 1)])))
		 (sign_extend:SI
		  (vec_select:HI (match_operand:V2HI 2 "register_operand" "d,d,d,d")
				 (parallel [(const_int 0)])))))
   (set (match_operand:SI 3 "register_operand" "=q1,q3,q5,q7")
	(mult:SI (sign_extend:SI
		  (vec_select:HI (match_dup 1) (parallel [(const_int 0)])))
		 (zero_extend:SI
		  (vec_select:HI (match_dup 2) (parallel [(const_int 1)])))))]
  ""
  "%0 = %d1 * %h2, %3 = %h1 * %d2 (IS,M)%!"
  [(set_attr "type" "dsp32")])

(define_insn "usmulhisi_hl_hul"
  [(set (match_operand:SI 0 "register_operand" "=q0,q2,q4,q6")
	(mult:SI (sign_extend:SI
		  (vec_select:HI (match_operand:V2HI 1 "register_operand" "d,d,d,d")
				 (parallel [(const_int 1)])))
		 (sign_extend:SI
		  (vec_select:HI (match_operand:V2HI 2 "register_operand" "d,d,d,d")
				 (parallel [(const_int 0)])))))
   (set (match_operand:SI 3 "register_operand" "=q1,q3,q5,q7")
	(mult:SI (sign_extend:SI
		  (vec_select:HI (match_dup 1) (parallel [(const_int 1)])))
		 (zero_extend:SI
		  (vec_select:HI (match_dup 2) (parallel [(const_int 0)])))))]
  ""
  "%0 = %d1 * %h2, %3 = %d1 * %h2 (IS,M)%!"
  [(set_attr "type" "dsp32")])

(define_insn "usmulhisi_hl_huh"
  [(set (match_operand:SI 0 "register_operand" "=q0,q2,q4,q6")
	(mult:SI (sign_extend:SI
		  (vec_select:HI (match_operand:V2HI 1 "register_operand" "d,d,d,d")
				 (parallel [(const_int 1)])))
		 (sign_extend:SI
		  (vec_select:HI (match_operand:V2HI 2 "register_operand" "d,d,d,d")
				 (parallel [(const_int 0)])))))
   (set (match_operand:SI 3 "register_operand" "=q1,q3,q5,q7")
	(mult:SI (sign_extend:SI
		  (vec_select:HI (match_dup 1) (parallel [(const_int 1)])))
		 (zero_extend:SI
		  (vec_select:HI (match_dup 2) (parallel [(const_int 1)])))))]
  ""
  "%0 = %d1 * %h2, %3 = %d1 * %d2 (IS,M)%!"
  [(set_attr "type" "dsp32")])

(define_insn "usmulhisi_hh_lul"
  [(set (match_operand:SI 0 "register_operand" "=q0,q2,q4,q6")
	(mult:SI (sign_extend:SI
		  (vec_select:HI (match_operand:V2HI 1 "register_operand" "d,d,d,d")
				 (parallel [(const_int 1)])))
		 (sign_extend:SI
		  (vec_select:HI (match_operand:V2HI 2 "register_operand" "d,d,d,d")
				 (parallel [(const_int 1)])))))
   (set (match_operand:SI 3 "register_operand" "=q1,q3,q5,q7")
	(mult:SI (sign_extend:SI
		  (vec_select:HI (match_dup 1) (parallel [(const_int 0)])))
		 (zero_extend:SI
		  (vec_select:HI (match_dup 2) (parallel [(const_int 0)])))))]
  ""
  "%0 = %d1 * %d2, %3 = %h1 * %h2 (IS,M)%!"
  [(set_attr "type" "dsp32")])

(define_insn "usmulhisi_hh_luh"
  [(set (match_operand:SI 0 "register_operand" "=q0,q2,q4,q6")
	(mult:SI (sign_extend:SI
		  (vec_select:HI (match_operand:V2HI 1 "register_operand" "d,d,d,d")
				 (parallel [(const_int 1)])))
		 (sign_extend:SI
		  (vec_select:HI (match_operand:V2HI 2 "register_operand" "d,d,d,d")
				 (parallel [(const_int 1)])))))
   (set (match_operand:SI 3 "register_operand" "=q1,q3,q5,q7")
	(mult:SI (sign_extend:SI
		  (vec_select:HI (match_dup 1) (parallel [(const_int 0)])))
		 (zero_extend:SI
		  (vec_select:HI (match_dup 2) (parallel [(const_int 1)])))))]
  ""
  "%0 = %d1 * %d2, %3 = %h1 * %d2 (IS,M)%!"
  [(set_attr "type" "dsp32")])

(define_insn "usmulhisi_hh_hul"
  [(set (match_operand:SI 0 "register_operand" "=q0,q2,q4,q6")
	(mult:SI (sign_extend:SI
		  (vec_select:HI (match_operand:V2HI 1 "register_operand" "d,d,d,d")
				 (parallel [(const_int 1)])))
		 (sign_extend:SI
		  (vec_select:HI (match_operand:V2HI 2 "register_operand" "d,d,d,d")
				 (parallel [(const_int 1)])))))
   (set (match_operand:SI 3 "register_operand" "=q1,q3,q5,q7")
	(mult:SI (sign_extend:SI
		  (vec_select:HI (match_dup 1) (parallel [(const_int 1)])))
		 (zero_extend:SI
		  (vec_select:HI (match_dup 2) (parallel [(const_int 0)])))))]
  ""
  "%0 = %d1 * %d2, %3 = %d1 * %h2 (IS,M)%!"
  [(set_attr "type" "dsp32")])

(define_insn "usmulhisi_hh_huh"
  [(set (match_operand:SI 0 "register_operand" "=q0,q2,q4,q6")
	(mult:SI (sign_extend:SI
		  (vec_select:HI (match_operand:V2HI 1 "register_operand" "d,d,d,d")
				 (parallel [(const_int 1)])))
		 (sign_extend:SI
		  (vec_select:HI (match_operand:V2HI 2 "register_operand" "d,d,d,d")
				 (parallel [(const_int 1)])))))
   (set (match_operand:SI 3 "register_operand" "=q1,q3,q5,q7")
	(mult:SI (sign_extend:SI
		  (vec_select:HI (match_dup 1) (parallel [(const_int 1)])))
		 (zero_extend:SI
		  (vec_select:HI (match_dup 2) (parallel [(const_int 1)])))))]
  ""
  "%0 = %d1 * %d2, %3 = %d1 * %d2 (IS,M)%!"
  [(set_attr "type" "dsp32")])

;; Vector neg/abs.

(define_insn "ssnegv2hi2"
  [(set (match_operand:V2HI 0 "register_operand" "=d")
	(ss_neg:V2HI (match_operand:V2HI 1 "register_operand" "d")))]
  ""
  "%0 = - %1 (V)%!"
  [(set_attr "type" "dsp32")])

(define_insn "ssabsv2hi2"
  [(set (match_operand:V2HI 0 "register_operand" "=d")
	(ss_abs:V2HI (match_operand:V2HI 1 "register_operand" "d")))]
  ""
  "%0 = ABS %1 (V)%!"
  [(set_attr "type" "dsp32")])

;; Shifts.

(define_insn "ssashiftv2hi3"
  [(set (match_operand:V2HI 0 "register_operand" "=d,d,d")
	(if_then_else:V2HI
	 (lt (match_operand:HI 2 "vec_shift_operand" "d,Ku4,Ks4") (const_int 0))
	 (ashiftrt:V2HI (match_operand:V2HI 1 "register_operand" "d,d,d")
			(match_dup 2))
	 (ss_ashift:V2HI (match_dup 1) (match_dup 2))))]
  ""
  "@
   %0 = ASHIFT %1 BY %h2 (V, S)%!
   %0 = %1 << %2 (V,S)%!
   %0 = %1 >>> %N2 (V,S)%!"
  [(set_attr "type" "dsp32,dsp32shiftimm,dsp32shiftimm")])

(define_insn "ssashifthi3"
  [(set (match_operand:HI 0 "register_operand" "=d,d,d")
	(if_then_else:HI
	 (lt (match_operand:HI 2 "vec_shift_operand" "d,Ku4,Ks4") (const_int 0))
	 (ashiftrt:HI (match_operand:HI 1 "register_operand" "d,d,d")
		      (match_dup 2))
	 (ss_ashift:HI (match_dup 1) (match_dup 2))))]
  ""
  "@
   %0 = ASHIFT %1 BY %h2 (V, S)%!
   %0 = %1 << %2 (V,S)%!
   %0 = %1 >>> %N2 (V,S)%!"
  [(set_attr "type" "dsp32,dsp32shiftimm,dsp32shiftimm")])

(define_insn "ssashiftsi3"
  [(set (match_operand:SI 0 "register_operand" "=d,d,d")
	(if_then_else:SI
	 (lt (match_operand:HI 2 "reg_or_const_int_operand" "d,Ku5,Ks5") (const_int 0))
	 (ashiftrt:SI (match_operand:HI 1 "register_operand" "d,d,d")
		      (match_dup 2))
	 (ss_ashift:SI (match_dup 1) (match_dup 2))))]
  ""
  "@
   %0 = ASHIFT %1 BY %h2 (S)%!
   %0 = %1 << %2 (S)%!
   %0 = %1 >>> %N2 (S)%!"
  [(set_attr "type" "dsp32,dsp32shiftimm,dsp32shiftimm")])

(define_insn "lshiftv2hi3"
  [(set (match_operand:V2HI 0 "register_operand" "=d,d,d")
	(if_then_else:V2HI
	 (lt (match_operand:HI 2 "vec_shift_operand" "d,Ku4,Ks4") (const_int 0))
	 (lshiftrt:V2HI (match_operand:V2HI 1 "register_operand" "d,d,d")
			(match_dup 2))
	 (ashift:V2HI (match_dup 1) (match_dup 2))))]
  ""
  "@
   %0 = LSHIFT %1 BY %h2 (V)%!
   %0 = %1 << %2 (V)%!
   %0 = %1 >> %N2 (V)%!"
  [(set_attr "type" "dsp32,dsp32shiftimm,dsp32shiftimm")])

(define_insn "lshifthi3"
  [(set (match_operand:HI 0 "register_operand" "=d,d,d")
	(if_then_else:HI
	 (lt (match_operand:HI 2 "vec_shift_operand" "d,Ku4,Ks4") (const_int 0))
	 (lshiftrt:HI (match_operand:HI 1 "register_operand" "d,d,d")
		      (match_dup 2))
	 (ashift:HI (match_dup 1) (match_dup 2))))]
  ""
  "@
   %0 = LSHIFT %1 BY %h2 (V)%!
   %0 = %1 << %2 (V)%!
   %0 = %1 >> %N2 (V)%!"
  [(set_attr "type" "dsp32,dsp32shiftimm,dsp32shiftimm")])

;; Load without alignment exception (masking off low bits)

(define_insn "loadbytes"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(mem:SI (and:SI (match_operand:SI 1 "register_operand" "b")
			(const_int -4))))]
  ""
  "DISALGNEXCPT || %0 = [%1];"
  [(set_attr "type" "mcld")
   (set_attr "length" "8")])

(include "sync.md")
