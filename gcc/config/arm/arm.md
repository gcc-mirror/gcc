;;- Machine description for ARM for GNU compiler
;;  Copyright (C) 1991, 93-98, 1999 Free Software Foundation, Inc.
;;  Contributed by Pieter `Tiggr' Schoenmakers (rcpieter@win.tue.nl)
;;  and Martin Simmons (@harleqn.co.uk).
;;  More major hacks by Richard Earnshaw (rearnsha@arm.com).

;; This file is part of GNU CC.

;; GNU CC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU CC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU CC; see the file COPYING.  If not, write to
;; the Free Software Foundation, 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;- See file "rtl.def" for documentation on define_insn, match_*, et. al.

;; There are patterns in this file to support XFmode arithmetic.
;; Unfortunately RISC iX doesn't work well with these so they are disabled.
;; (See arm.h)

;; UNSPEC Usage:
;; 0 `sin' operation: operand 0 is the result, operand 1 the parameter,
;;   the mode is MODE_FLOAT
;; 1 `cos' operation: operand 0 is the result, operand 1 the parameter,
;;   the mode is MODE_FLOAT
;; 2 `push multiple' operation: operand 0 is the first register.  Subsequent
;;   registers are in parallel (use...) expressions.
;; 3 A symbol that has been treated properly for pic usage, that is, we
;;   will add the pic_register value to it before trying to dereference it.
;; Note: sin and cos are no-longer used.

;; Attributes

; PROG_MODE attribute is used to determine whether condition codes are
; clobbered by a call insn: they are if in prog32 mode.  This is controlled
; by the -mapcs-{32,26} flag, and possibly the -mcpu=... option.
(define_attr "prog_mode" "prog26,prog32" (const (symbol_ref "arm_prog_mode")))

(define_attr "is_strongarm" "no,yes" (const (symbol_ref "arm_is_strong")))

; Floating Point Unit.  If we only have floating point emulation, then there
; is no point in scheduling the floating point insns.  (Well, for best
; performance we should try and group them together).

(define_attr "fpu" "fpa,fpe2,fpe3" (const (symbol_ref "arm_fpu_attr")))

; LENGTH of an instruction (in bytes)
(define_attr "length" "" (const_int 4))

; An assembler sequence may clobber the condition codes without us knowing
(define_asm_attributes
 [(set_attr "conds" "clob")
  (set_attr "length" "4")])

; TYPE attribute is used to detect floating point instructions which, if
; running on a co-processor can run in parallel with other, basic instructions
; If write-buffer scheduling is enabled then it can also be used in the
; scheduling of writes.

; Classification of each insn
; normal	any data instruction that doesn't hit memory or fp regs
; mult		a multiply instruction
; block		blockage insn, this blocks all functional units
; float		a floating point arithmetic operation (subject to expansion)
; fdivx		XFmode floating point division
; fdivd		DFmode floating point division
; fdivs		SFmode floating point division
; fmul		Floating point multiply
; ffmul		Fast floating point multiply
; farith	Floating point arithmetic (4 cycle)
; ffarith	Fast floating point arithmetic (2 cycle)
; float_em	a floating point arithmetic operation that is normally emulated
;		even on a machine with an fpa.
; f_load	a floating point load from memory
; f_store	a floating point store to memory
; f_mem_r	a transfer of a floating point register to a real reg via mem
; r_mem_f	the reverse of f_mem_r
; f_2_r		fast transfer float to arm (no memory needed)
; r_2_f		fast transfer arm to float
; call		a subroutine call
; load		any load from memory
; store1	store 1 word to memory from arm registers
; store2	store 2 words
; store3	store 3 words
; store4	store 4 words
;
(define_attr "type"
	"normal,mult,block,float,fdivx,fdivd,fdivs,fmul,ffmul,farith,ffarith,float_em,f_load,f_store,f_mem_r,r_mem_f,f_2_r,r_2_f,call,load,store1,store2,store3,store4" 
	(const_string "normal"))

; Load scheduling, set from the arm_ld_sched variable
; initialised by arm_override_options() 
(define_attr "ldsched" "no,yes" (const (symbol_ref "arm_ld_sched")))

; condition codes: this one is used by final_prescan_insn to speed up
; conditionalizing instructions.  It saves having to scan the rtl to see if
; it uses or alters the condition codes.

; USE means that the condition codes are used by the insn in the process of
; outputting code, this means (at present) that we can't use the insn in
; inlined branches

; SET means that the purpose of the insn is to set the condition codes in a
; well defined manner.

; CLOB means that the condition codes are altered in an undefined manner, if
; they are altered at all

; JUMP_CLOB is used when the conditions are not defined if a branch is taken,
; but are if the branch wasn't taken; the effect is to limit the branch
; elimination scanning.

; NOCOND means that the condition codes are neither altered nor affect the
; output of this insn

(define_attr "conds" "use,set,clob,jump_clob,nocond"
	(if_then_else (eq_attr "type" "call")
	 (if_then_else (eq_attr "prog_mode" "prog32")
	  (const_string "clob") (const_string "nocond"))
	 (const_string "nocond")))

; Only model the write buffer for ARM6 and ARM7.  Earlier processors don't
; have one.  Later ones, such as StrongARM, have write-back caches, so don't
; suffer blockages enough to warrent modelling this (and it can adversely
; affect the schedule).
(define_attr "model_wbuf" "no,yes" (const (symbol_ref "arm_is_6_or_7")))

(define_attr "write_conflict" "no,yes"
  (if_then_else (eq_attr "type"
		 "block,float_em,f_load,f_store,f_mem_r,r_mem_f,call,load")
		(const_string "yes")
		(const_string "no")))

(define_attr "core_cycles" "single,multi"
  (if_then_else (eq_attr "type"
		 "normal,float,fdivx,fdivd,fdivs,fmul,ffmul,farith,ffarith")
		(const_string "single")
	        (const_string "multi")))

; The write buffer on some of the arm6 processors is hard to model exactly.
; There is room in the buffer for up to two addresses and up to eight words
; of memory, but the two needn't be split evenly.  When writing the two
; addresses are fully pipelined.  However, a read from memory that is not
; currently in the cache will block until the writes have completed.
; It is normally the case that FCLK and MCLK will be in the ratio 2:1, so
; writes will take 2 FCLK cycles per word, if FCLK and MCLK are asynchronous
; (they aren't allowed to be at present) then there is a startup cost of 1MCLK
; cycle to add as well.

;; (define_function_unit {name} {num-units} {n-users} {test}
;;                       {ready-delay} {issue-delay} [{conflict-list}])
(define_function_unit "fpa" 1 0 (and (eq_attr "fpu" "fpa")
				     (eq_attr "type" "fdivx")) 71 69)

(define_function_unit "fpa" 1 0 (and (eq_attr "fpu" "fpa")
				     (eq_attr "type" "fdivd")) 59 57)

(define_function_unit "fpa" 1 0 (and (eq_attr "fpu" "fpa")
				     (eq_attr "type" "fdivs")) 31 29)

(define_function_unit "fpa" 1 0 (and (eq_attr "fpu" "fpa")
				     (eq_attr "type" "fmul")) 9 7)

(define_function_unit "fpa" 1 0 (and (eq_attr "fpu" "fpa")
				     (eq_attr "type" "ffmul")) 6 4)

(define_function_unit "fpa" 1 0 (and (eq_attr "fpu" "fpa")
				     (eq_attr "type" "farith")) 4 2)

(define_function_unit "fpa" 1 0 (and (eq_attr "fpu" "fpa")
				     (eq_attr "type" "ffarith")) 2 2)

(define_function_unit "fpa" 1 0 (and (eq_attr "fpu" "fpa")
				     (eq_attr "type" "r_2_f")) 5 3)

(define_function_unit "fpa" 1 0 (and (eq_attr "fpu" "fpa")
				     (eq_attr "type" "f_2_r")) 1 2)

;; The fpa10 doesn't really have a memory read unit, but it can start to
;; speculatively execute the instruction in the pipeline, provided the data
;; is already loaded, so pretend reads have a delay of 2 (and that the
;; pipeline is infinite.

(define_function_unit "fpa_mem" 1 0 (and (eq_attr "fpu" "fpa")
					 (eq_attr "type" "f_load")) 3 1)

;;--------------------------------------------------------------------
;; Write buffer
;;--------------------------------------------------------------------
;; Strictly we should model a 4-deep write buffer for ARM7xx based chips
(define_function_unit "write_buf" 1 2
  (and (eq_attr "model_wbuf" "yes")
       (eq_attr "type" "store1,r_mem_f")) 5 3)
(define_function_unit "write_buf" 1 2 
  (and (eq_attr "model_wbuf" "yes")
       (eq_attr "type" "store2")) 7 4)
(define_function_unit "write_buf" 1 2
  (and (eq_attr "model_wbuf" "yes")
       (eq_attr "type" "store3")) 9 5)
(define_function_unit "write_buf" 1 2
  (and (eq_attr "model_wbuf" "yes")
       (eq_attr "type" "store4")) 11 6)

;;--------------------------------------------------------------------
;; Write blockage unit
;;--------------------------------------------------------------------
;; The write_blockage unit models (partially), the fact that reads will stall
;; until the write buffer empties.
;; The f_mem_r and r_mem_f could also block, but they are to the stack,
;; so we don't model them here
(define_function_unit "write_blockage" 1 0 (and (eq_attr "model_wbuf" "yes")
						(eq_attr "type" "store1")) 5 5
	[(eq_attr "write_conflict" "yes")])
(define_function_unit "write_blockage" 1 0 (and (eq_attr "model_wbuf" "yes")
						(eq_attr "type" "store2")) 7 7
	[(eq_attr "write_conflict" "yes")])
(define_function_unit "write_blockage" 1 0 (and (eq_attr "model_wbuf" "yes")
						(eq_attr "type" "store3")) 9 9
	[(eq_attr "write_conflict" "yes")])
(define_function_unit "write_blockage" 1 0
	(and (eq_attr "model_wbuf" "yes") (eq_attr "type" "store4")) 11 11
	[(eq_attr "write_conflict" "yes")])
(define_function_unit "write_blockage" 1 0
	(and (eq_attr "model_wbuf" "yes")
	     (eq_attr "write_conflict" "yes")) 1 1)

;;--------------------------------------------------------------------
;; Core unit
;;--------------------------------------------------------------------
;; Everything must spend at least one cycle in the core unit
(define_function_unit "core" 1 0
  (and (eq_attr "ldsched" "yes") (eq_attr "type" "store1")) 1 1)

(define_function_unit "core" 1 0
  (and (eq_attr "ldsched" "yes") (eq_attr "type" "load")) 2 1)

(define_function_unit "core" 1 0
  (and (eq_attr "ldsched" "!yes") (eq_attr "type" "load,store1")) 2 2)

(define_function_unit "core" 1 0
  (and (eq_attr "fpu" "fpa") (eq_attr "type" "f_load")) 3 3)

(define_function_unit "core" 1 0
  (and (eq_attr "fpu" "fpa") (eq_attr "type" "f_store")) 4 4)

(define_function_unit "core" 1 0
  (and (eq_attr "fpu" "fpa") (eq_attr "type" "r_mem_f")) 6 6)

(define_function_unit "core" 1 0
  (and (eq_attr "fpu" "fpa") (eq_attr "type" "f_mem_r")) 7 7)

(define_function_unit "core" 1 0
  (and (eq_attr "ldsched" "no") (eq_attr "type" "mult")) 16 16)

(define_function_unit "core" 1 0
  (and (and (eq_attr "ldsched" "yes") (eq_attr "is_strongarm" "no"))
       (eq_attr "type" "mult")) 4 4)

(define_function_unit "core" 1 0
  (and (and (eq_attr "ldsched" "yes") (eq_attr "is_strongarm" "yes"))
       (eq_attr "type" "mult")) 3 2)

(define_function_unit "core" 1 0 (eq_attr "type" "store2") 3 3)

(define_function_unit "core" 1 0 (eq_attr "type" "store3") 4 4)

(define_function_unit "core" 1 0 (eq_attr "type" "store4") 5 5)

;; Note: For DImode insns, there is normally no reason why operands should
;; not be in the same register, what we don't want is for something being
;; written to partially overlap something that is an input.

;; Addition insns.

(define_insn "adddi3"
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(plus:DI (match_operand:DI 1 "s_register_operand" "%0,0")
		 (match_operand:DI 2 "s_register_operand" "r,0")))
   (clobber (reg:CC 24))]
  ""
  "adds\\t%Q0, %Q1, %Q2\;adc\\t%R0, %R1, %R2"
[(set_attr "conds" "clob")
 (set_attr "length" "8")])

(define_insn "*adddi_sesidi_di"
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(plus:DI (sign_extend:DI
		  (match_operand:SI 2 "s_register_operand" "r,r"))
		 (match_operand:DI 1 "s_register_operand" "r,0")))
   (clobber (reg:CC 24))]
  ""
  "adds\\t%Q0, %Q1, %2\;adc\\t%R0, %R1, %2, asr #31"
[(set_attr "conds" "clob")
 (set_attr "length" "8")])

(define_insn "*adddi_zesidi_di"
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(plus:DI (zero_extend:DI
		  (match_operand:SI 2 "s_register_operand" "r,r"))
		 (match_operand:DI 1 "s_register_operand" "r,0")))
   (clobber (reg:CC 24))]
  ""
  "adds\\t%Q0, %Q1, %2\;adc\\t%R0, %R1, #0"
[(set_attr "conds" "clob")
 (set_attr "length" "8")])

(define_expand "addsi3"
  [(set (match_operand:SI 0 "s_register_operand" "")
	(plus:SI (match_operand:SI 1 "s_register_operand" "")
		 (match_operand:SI 2 "reg_or_int_operand" "")))]
  ""
  "
  if (GET_CODE (operands[2]) == CONST_INT)
    {
      arm_split_constant (PLUS, SImode, INTVAL (operands[2]), operands[0],
			  operands[1],
			  (reload_in_progress || reload_completed ? 0
			   : preserve_subexpressions_p ()));
      DONE;
    }
")

(define_split
  [(set (match_operand:SI 0 "s_register_operand" "")
	(plus:SI (match_operand:SI 1 "s_register_operand" "")
		 (match_operand:SI 2 "const_int_operand" "")))]
  "! (const_ok_for_arm (INTVAL (operands[2]))
      || const_ok_for_arm (-INTVAL (operands[2])))"
  [(clobber (const_int 0))]
  "
  arm_split_constant (PLUS, SImode, INTVAL (operands[2]), operands[0],
		      operands[1], 0);
  DONE;
")

(define_insn "*addsi3_insn"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r,r")
	(plus:SI (match_operand:SI 1 "s_register_operand" "r,r,r")
		 (match_operand:SI 2 "reg_or_int_operand" "rI,L,?n")))]
  ""
  "@
   add%?\\t%0, %1, %2
   sub%?\\t%0, %1, #%n2
   #"
[(set_attr "length" "4,4,16")])

(define_insn "*addsi3_compare0"
  [(set (reg:CC_NOOV 24)
	(compare:CC_NOOV
	 (plus:SI (match_operand:SI 1 "s_register_operand" "r,r")
		  (match_operand:SI 2 "arm_add_operand" "rI,L"))
	 (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=r,r")
	(plus:SI (match_dup 1) (match_dup 2)))]
  ""
  "@
   add%?s\\t%0, %1, %2
   sub%?s\\t%0, %1, #%n2"
[(set_attr "conds" "set")])

(define_insn "*addsi3_compare0_scratch"
  [(set (reg:CC_NOOV 24)
	(compare:CC_NOOV
	 (plus:SI (match_operand:SI 0 "s_register_operand" "r,r")
		  (match_operand:SI 1 "arm_add_operand" "rI,L"))
	 (const_int 0)))]
  ""
  "@
   cmn%?\\t%0, %1
   cmp%?\\t%0, #%n1"
[(set_attr "conds" "set")])

;; The next four insns work because they compare the result with one of
;; the operands, and we know that the use of the condition code is
;; either GEU or LTU, so we can use the carry flag from the addition
;; instead of doing the compare a second time.
(define_insn "*addsi3_compare_op1"
  [(set (reg:CC_C 24)
	(compare:CC_C
	 (plus:SI (match_operand:SI 1 "s_register_operand" "r,r")
		  (match_operand:SI 2 "arm_add_operand" "rI,L"))
	 (match_dup 1)))
   (set (match_operand:SI 0 "s_register_operand" "=r,r")
	(plus:SI (match_dup 1) (match_dup 2)))]
  ""
  "@
   add%?s\\t%0, %1, %2
   sub%?s\\t%0, %1, #%n2"
[(set_attr "conds" "set")])

(define_insn "*addsi3_compare_op2"
  [(set (reg:CC_C 24)
	(compare:CC_C
	 (plus:SI (match_operand:SI 1 "s_register_operand" "r,r")
		  (match_operand:SI 2 "arm_add_operand" "rI,L"))
	 (match_dup 2)))
   (set (match_operand:SI 0 "s_register_operand" "=r,r")
	(plus:SI (match_dup 1) (match_dup 2)))]
  ""
  "@
   add%?s\\t%0, %1, %2
   sub%?s\\t%0, %1, #%n2"
[(set_attr "conds" "set")])

(define_insn "*compare_addsi2_op0"
  [(set (reg:CC_C 24)
	(compare:CC_C
	 (plus:SI (match_operand:SI 0 "s_register_operand" "r,r")
		  (match_operand:SI 1 "arm_add_operand" "rI,L"))
	 (match_dup 0)))]
  ""
  "@
   cmn%?\\t%0, %1
   cmp%?\\t%0, #%n1"
[(set_attr "conds" "set")])

(define_insn "*compare_addsi2_op1"
  [(set (reg:CC_C 24)
	(compare:CC_C
	 (plus:SI (match_operand:SI 0 "s_register_operand" "r,r")
		  (match_operand:SI 1 "arm_add_operand" "rI,L"))
	 (match_dup 1)))]
  ""
  "@
   cmn%?\\t%0, %1
   cmp%?\\t%0, #%n1"
[(set_attr "conds" "set")])

(define_insn "*addsi3_carryin"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(plus:SI (ltu:SI (reg:CC_C 24) (const_int 0))
		 (plus:SI (match_operand:SI 1 "s_register_operand" "r")
			  (match_operand:SI 2 "arm_rhs_operand" "rI"))))]
  ""
  "adc%?\\t%0, %1, %2"
[(set_attr "conds" "use")])

(define_insn "*addsi3_carryin_alt1"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(plus:SI (plus:SI (match_operand:SI 1 "s_register_operand" "r")
			  (match_operand:SI 2 "arm_rhs_operand" "rI"))
		 (ltu:SI (reg:CC_C 24) (const_int 0))))]
  ""
  "adc%?\\t%0, %1, %2"
[(set_attr "conds" "use")])

(define_insn "*addsi3_carryin_alt2"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(plus:SI (plus:SI (ltu:SI (reg:CC_C 24) (const_int 0))
			  (match_operand:SI 1 "s_register_operand" "r"))
		 (match_operand:SI 2 "arm_rhs_operand" "rI")))]
  ""
  "adc%?\\t%0, %1, %2"
[(set_attr "conds" "use")])

(define_insn "*addsi3_carryin_alt3"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(plus:SI (plus:SI (ltu:SI (reg:CC_C 24) (const_int 0))
			  (match_operand:SI 2 "arm_rhs_operand" "rI"))
		 (match_operand:SI 1 "s_register_operand" "r")))]
  ""
  "adc%?\\t%0, %1, %2"
[(set_attr "conds" "use")])

(define_insn "incscc"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
        (plus:SI (match_operator:SI 2 "comparison_operator"
                    [(match_operand 3 "cc_register" "") (const_int 0)])
                 (match_operand:SI 1 "s_register_operand" "0,?r")))]
  ""
  "@
  add%d2\\t%0, %1, #1
  mov%D2\\t%0, %1\;add%d2\\t%0, %1, #1"
[(set_attr "conds" "use")
 (set_attr "length" "4,8")])

; If a constant is too big to fit in a single instruction then the constant
; will be pre-loaded into a register taking at least two insns, we might be
; able to merge it with an add, but it depends on the exact value.

(define_split
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(plus:SI (match_operand:SI 1 "s_register_operand" "r")
		 (match_operand:SI 2 "const_int_operand" "n")))]
  "!(const_ok_for_arm (INTVAL (operands[2]))
     || const_ok_for_arm (-INTVAL (operands[2])))"
  [(set (match_dup 0) (plus:SI (match_dup 1) (match_dup 2)))
   (set (match_dup 0) (plus:SI (match_dup 0) (match_dup 3)))]
  "
{
  unsigned int val = (unsigned) INTVAL (operands[2]);
  int i;
  unsigned int temp;

  /* this code is similar to the approach followed in movsi, but it must
     generate exactly two insns */

  for (i = 30; i >= 0; i -= 2)
    {
      if (val & (3 << i))
	{
	  i -= 6;
	  if (i < 0) i = 0;
	  if (const_ok_for_arm (temp = (val & ~(255 << i))))
	    {
	      val &= 255 << i;
	      break;
	    }
	  /* we might be able to do this as (larger number - small number) */
	  temp = ((val >> i) & 255) + 1;
	  if (temp > 255 && i < 24)
	    {
	      i += 2;
	      temp = ((val >> i) & 255) + 1;
	    }
	  if (const_ok_for_arm ((temp << i) - val))
	    {
	      i = temp << i;
	      temp = (unsigned) - (int) (i - val);
	      val = i;
	      break;
	    }
	  FAIL;
	}
    }
  /* if we got here, we have found a way of doing it in two instructions.
     the two constants are in val and temp */
  operands[2] = GEN_INT ((int)val);
  operands[3] = GEN_INT ((int)temp);
}
")

(define_insn "addsf3"
  [(set (match_operand:SF 0 "s_register_operand" "=f,f")
	(plus:SF (match_operand:SF 1 "s_register_operand" "f,f")
		 (match_operand:SF 2 "fpu_add_operand" "fG,H")))]
  "TARGET_HARD_FLOAT"
  "@
   adf%?s\\t%0, %1, %2
   suf%?s\\t%0, %1, #%N2"
[(set_attr "type" "farith")])

(define_insn "adddf3"
  [(set (match_operand:DF 0 "s_register_operand" "=f,f")
	(plus:DF (match_operand:DF 1 "s_register_operand" "f,f")
		 (match_operand:DF 2 "fpu_add_operand" "fG,H")))]
  "TARGET_HARD_FLOAT"
  "@
   adf%?d\\t%0, %1, %2
   suf%?d\\t%0, %1, #%N2"
[(set_attr "type" "farith")])

(define_insn "*adddf_df_esfdf"
  [(set (match_operand:DF 0 "s_register_operand" "=f,f")
	(plus:DF (float_extend:DF
		  (match_operand:SF 1 "s_register_operand" "f,f"))
		 (match_operand:DF 2 "fpu_add_operand" "fG,H")))]
  "TARGET_HARD_FLOAT"
  "@
   adf%?d\\t%0, %1, %2
   suf%?d\\t%0, %1, #%N2"
[(set_attr "type" "farith")])

(define_insn "*adddf_df_esfdf"
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(plus:DF (match_operand:DF 1 "s_register_operand" "f")
		 (float_extend:DF
		  (match_operand:SF 2 "s_register_operand" "f"))))]
  "TARGET_HARD_FLOAT"
  "adf%?d\\t%0, %1, %2"
[(set_attr "type" "farith")])

(define_insn "*adddf_esfdf_esfdf"
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(plus:DF (float_extend:DF 
		  (match_operand:SF 1 "s_register_operand" "f"))
		 (float_extend:DF
		  (match_operand:SF 2 "s_register_operand" "f"))))]
  "TARGET_HARD_FLOAT"
  "adf%?d\\t%0, %1, %2"
[(set_attr "type" "farith")])

(define_insn "addxf3"
  [(set (match_operand:XF 0 "s_register_operand" "=f,f")
	(plus:XF (match_operand:XF 1 "s_register_operand" "f,f")
		 (match_operand:XF 2 "fpu_add_operand" "fG,H")))]
  "ENABLE_XF_PATTERNS && TARGET_HARD_FLOAT"
  "@
   adf%?e\\t%0, %1, %2
   suf%?e\\t%0, %1, #%N2"
[(set_attr "type" "farith")])

(define_insn "subdi3"
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r,&r")
	(minus:DI (match_operand:DI 1 "s_register_operand" "0,r,0")
		  (match_operand:DI 2 "s_register_operand" "r,0,0")))
   (clobber (reg:CC 24))]
  ""
  "subs\\t%Q0, %Q1, %Q2\;sbc\\t%R0, %R1, %R2"
[(set_attr "conds" "clob")
 (set_attr "length" "8")])

(define_insn "*subdi_di_zesidi"
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(minus:DI (match_operand:DI 1 "s_register_operand" "?r,0")
		  (zero_extend:DI
		   (match_operand:SI 2 "s_register_operand" "r,r"))))
   (clobber (reg:CC 24))]
  ""
  "subs\\t%Q0, %Q1, %2\;sbc\\t%R0, %R1, #0"
[(set_attr "conds" "clob")
 (set_attr "length" "8")])

(define_insn "*subdi_di_sesidi"
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(minus:DI (match_operand:DI 1 "s_register_operand" "r,0")
		  (sign_extend:DI
		   (match_operand:SI 2 "s_register_operand" "r,r"))))
   (clobber (reg:CC 24))]
  ""
  "subs\\t%Q0, %Q1, %2\;sbc\\t%R0, %R1, %2, asr #31"
[(set_attr "conds" "clob")
 (set_attr "length" "8")])

(define_insn "*subdi_zesidi_di"
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(minus:DI (zero_extend:DI
		   (match_operand:SI 2 "s_register_operand" "r,r"))
		  (match_operand:DI 1 "s_register_operand" "?r,0")))
   (clobber (reg:CC 24))]
  ""
  "rsbs\\t%Q0, %Q1, %2\;rsc\\t%R0, %R1, #0"
[(set_attr "conds" "clob")
 (set_attr "length" "8")])

(define_insn "*subdi_sesidi_di"
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(minus:DI (sign_extend:DI
		   (match_operand:SI 2 "s_register_operand" "r,r"))
		  (match_operand:DI 1 "s_register_operand" "?r,0")))
   (clobber (reg:CC 24))]
  ""
  "rsbs\\t%Q0, %Q1, %2\;rsc\\t%R0, %R1, %2, asr #31"
[(set_attr "conds" "clob")
 (set_attr "length" "8")])

(define_insn "*subdi_zesidi_zesidi"
  [(set (match_operand:DI 0 "s_register_operand" "=r")
	(minus:DI (zero_extend:DI
		   (match_operand:SI 1 "s_register_operand" "r"))
		  (zero_extend:DI
		   (match_operand:SI 2 "s_register_operand" "r"))))
   (clobber (reg:CC 24))]
  ""
  "subs\\t%Q0, %1, %2\;rsc\\t%R0, %1, %1"
[(set_attr "conds" "clob")
 (set_attr "length" "8")])

(define_expand "subsi3"
  [(set (match_operand:SI 0 "s_register_operand" "")
	(minus:SI (match_operand:SI 1 "reg_or_int_operand" "")
		  (match_operand:SI 2 "s_register_operand" "")))]
  ""
  "
  if (GET_CODE (operands[1]) == CONST_INT)
    {
      arm_split_constant (MINUS, SImode, INTVAL (operands[1]), operands[0],
			  operands[2],
			  (reload_in_progress || reload_completed ? 0
			   : preserve_subexpressions_p ()));
      DONE;
    }
")

(define_insn "*subsi3_insn"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(minus:SI (match_operand:SI 1 "reg_or_int_operand" "rI,?n")
		  (match_operand:SI 2 "s_register_operand" "r,r")))]
  ""
  "@
   rsb%?\\t%0, %2, %1
   #"
[(set_attr "length" "4,16")])

(define_split
  [(set (match_operand:SI 0 "s_register_operand" "")
	(minus:SI (match_operand:SI 1 "const_int_operand" "")
		  (match_operand:SI 2 "s_register_operand" "")))]
  "! const_ok_for_arm (INTVAL (operands[1]))"
  [(clobber (const_int 0))]
  "
  arm_split_constant (MINUS, SImode, INTVAL (operands[1]), operands[0],
		      operands[2], 0);
  DONE;
")

(define_insn "*subsi3_compare0"
  [(set (reg:CC_NOOV 24)
	(compare:CC_NOOV (minus:SI (match_operand:SI 1 "arm_rhs_operand" "r,I")
				 (match_operand:SI 2 "arm_rhs_operand" "rI,r"))
			 (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=r,r")
	(minus:SI (match_dup 1) (match_dup 2)))]
  ""
  "@
   sub%?s\\t%0, %1, %2
   rsb%?s\\t%0, %2, %1"
[(set_attr "conds" "set")])

(define_insn "decscc"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
        (minus:SI (match_operand:SI 1 "s_register_operand" "0,?r")
		  (match_operator:SI 2 "comparison_operator"
                   [(match_operand 3 "cc_register" "") (const_int 0)])))]
  ""
  "@
  sub%d2\\t%0, %1, #1
  mov%D2\\t%0, %1\;sub%d2\\t%0, %1, #1"
[(set_attr "conds" "use")
 (set_attr "length" "*,8")])

(define_insn "subsf3"
  [(set (match_operand:SF 0 "s_register_operand" "=f,f")
	(minus:SF (match_operand:SF 1 "fpu_rhs_operand" "f,G")
		  (match_operand:SF 2 "fpu_rhs_operand" "fG,f")))]
  "TARGET_HARD_FLOAT"
  "@
   suf%?s\\t%0, %1, %2
   rsf%?s\\t%0, %2, %1"
[(set_attr "type" "farith")])

(define_insn "subdf3"
  [(set (match_operand:DF 0 "s_register_operand" "=f,f")
	(minus:DF (match_operand:DF 1 "fpu_rhs_operand" "f,G")
		  (match_operand:DF 2 "fpu_rhs_operand" "fG,f")))]
  "TARGET_HARD_FLOAT"
  "@
   suf%?d\\t%0, %1, %2
   rsf%?d\\t%0, %2, %1"
[(set_attr "type" "farith")])

(define_insn "*subdf_esfdf_df"
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(minus:DF (float_extend:DF
		   (match_operand:SF 1 "s_register_operand" "f"))
		  (match_operand:DF 2 "fpu_rhs_operand" "fG")))]
  "TARGET_HARD_FLOAT"
  "suf%?d\\t%0, %1, %2"
[(set_attr "type" "farith")])

(define_insn "*subdf_df_esfdf"
  [(set (match_operand:DF 0 "s_register_operand" "=f,f")
	(minus:DF (match_operand:DF 1 "fpu_rhs_operand" "f,G")
		  (float_extend:DF
		   (match_operand:SF 2 "s_register_operand" "f,f"))))]
  "TARGET_HARD_FLOAT"
  "@
   suf%?d\\t%0, %1, %2
   rsf%?d\\t%0, %2, %1"
[(set_attr "type" "farith")])

(define_insn "*subdf_esfdf_esfdf"
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(minus:DF (float_extend:DF
		   (match_operand:SF 1 "s_register_operand" "f"))
		  (float_extend:DF
		   (match_operand:SF 2 "s_register_operand" "f"))))]
  "TARGET_HARD_FLOAT"
  "suf%?d\\t%0, %1, %2"
[(set_attr "type" "farith")])

(define_insn "subxf3"
  [(set (match_operand:XF 0 "s_register_operand" "=f,f")
	(minus:XF (match_operand:XF 1 "fpu_rhs_operand" "f,G")
		  (match_operand:XF 2 "fpu_rhs_operand" "fG,f")))]
  "ENABLE_XF_PATTERNS && TARGET_HARD_FLOAT"
  "@
   suf%?e\\t%0, %1, %2
   rsf%?e\\t%0, %2, %1"
[(set_attr "type" "farith")])

;; Multiplication insns

;; Use `&' and then `0' to prevent the operands 0 and 1 being the same
(define_insn "mulsi3"
  [(set (match_operand:SI 0 "s_register_operand" "=&r,&r")
	(mult:SI (match_operand:SI 2 "s_register_operand" "r,r")
		 (match_operand:SI 1 "s_register_operand" "%?r,0")))]
  ""
  "mul%?\\t%0, %2, %1"
[(set_attr "type" "mult")])

(define_insn "*mulsi3_compare0"
  [(set (reg:CC_NOOV 24)
	(compare:CC_NOOV (mult:SI
			  (match_operand:SI 2 "s_register_operand" "r,r")
			  (match_operand:SI 1 "s_register_operand" "%?r,0"))
			 (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=&r,&r")
	(mult:SI (match_dup 2) (match_dup 1)))]
  ""
  "mul%?s\\t%0, %2, %1"
[(set_attr "conds" "set")
 (set_attr "type" "mult")])

(define_insn "*mulsi_compare0_scratch"
  [(set (reg:CC_NOOV 24)
	(compare:CC_NOOV (mult:SI
			  (match_operand:SI 2 "s_register_operand" "r,r")
			  (match_operand:SI 1 "s_register_operand" "%?r,0"))
			 (const_int 0)))
   (clobber (match_scratch:SI 0 "=&r,&r"))]
  ""
  "mul%?s\\t%0, %2, %1"
[(set_attr "conds" "set")
 (set_attr "type" "mult")])

;; Unnamed templates to match MLA instruction.

(define_insn "*mulsi3addsi"
  [(set (match_operand:SI 0 "s_register_operand" "=&r,&r,&r,&r")
	(plus:SI
	  (mult:SI (match_operand:SI 2 "s_register_operand" "r,r,r,r")
		   (match_operand:SI 1 "s_register_operand" "%r,0,r,0"))
	  (match_operand:SI 3 "s_register_operand" "?r,r,0,0")))]
  ""
  "mla%?\\t%0, %2, %1, %3"
[(set_attr "type" "mult")])

(define_insn "*mulsi3addsi_compare0"
  [(set (reg:CC_NOOV 24)
	(compare:CC_NOOV (plus:SI
			  (mult:SI
			   (match_operand:SI 2 "s_register_operand" "r,r,r,r")
			   (match_operand:SI 1 "s_register_operand" "%r,0,r,0"))
			  (match_operand:SI 3 "s_register_operand" "?r,r,0,0"))
			 (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=&r,&r,&r,&r")
	(plus:SI (mult:SI (match_dup 2) (match_dup 1))
		 (match_dup 3)))]
  ""
  "mla%?s\\t%0, %2, %1, %3"
[(set_attr "conds" "set")
 (set_attr "type" "mult")])

(define_insn "*mulsi3addsi_compare0_scratch"
  [(set (reg:CC_NOOV 24)
	(compare:CC_NOOV (plus:SI
			  (mult:SI
			   (match_operand:SI 2 "s_register_operand" "r,r,r,r")
			   (match_operand:SI 1 "s_register_operand" "%r,0,r,0"))
			  (match_operand:SI 3 "s_register_operand" "?r,r,0,0"))
			 (const_int 0)))
   (clobber (match_scratch:SI 0 "=&r,&r,&r,&r"))]
  ""
  "mla%?s\\t%0, %2, %1, %3"
[(set_attr "conds" "set")
 (set_attr "type" "mult")])

(define_insn "mulsidi3"
  [(set (match_operand:DI 0 "s_register_operand" "=&r")
      (mult:DI (sign_extend:DI
                (match_operand:SI 1 "s_register_operand" "%r"))
               (sign_extend:DI
               (match_operand:SI 2 "s_register_operand" "r"))))]
  "arm_fast_multiply"
  "smull%?\\t%Q0, %R0, %1, %2"
[(set_attr "type" "mult")])

(define_insn "umulsidi3"
  [(set (match_operand:DI 0 "s_register_operand" "=&r")
      (mult:DI (zero_extend:DI
                (match_operand:SI 1 "s_register_operand" "%r"))
               (zero_extend:DI
                (match_operand:SI 2 "s_register_operand" "r"))))]
  "arm_fast_multiply"
  "umull%?\\t%Q0, %R0, %1, %2"
[(set_attr "type" "mult")])

(define_insn "smulsi3_highpart"
  [(set (match_operand:SI 0 "s_register_operand" "=&r,&r")
	(truncate:SI
	 (lshiftrt:DI
	  (mult:DI (sign_extend:DI
		    (match_operand:SI 1 "s_register_operand" "%r,0"))
		   (sign_extend:DI
		    (match_operand:SI 2 "s_register_operand" "r,r")))
	  (const_int 32))))
   (clobber (match_scratch:SI 3 "=&r,&r"))]
  "arm_fast_multiply"
  "smull%?\\t%3, %0, %2, %1"
[(set_attr "type" "mult")])

(define_insn "umulsi3_highpart"
  [(set (match_operand:SI 0 "s_register_operand" "=&r,&r")
	(truncate:SI
	 (lshiftrt:DI
	  (mult:DI (zero_extend:DI
		    (match_operand:SI 1 "s_register_operand" "%r,0"))
		   (zero_extend:DI
		    (match_operand:SI 2 "s_register_operand" "r,r")))
	  (const_int 32))))
   (clobber (match_scratch:SI 3 "=&r,&r"))]
  "arm_fast_multiply"
  "umull%?\\t%3, %0, %2, %1"
[(set_attr "type" "mult")])

(define_insn "mulsf3"
  [(set (match_operand:SF 0 "s_register_operand" "=f")
	(mult:SF (match_operand:SF 1 "s_register_operand" "f")
		 (match_operand:SF 2 "fpu_rhs_operand" "fG")))]
  "TARGET_HARD_FLOAT"
  "fml%?s\\t%0, %1, %2"
[(set_attr "type" "ffmul")])

(define_insn "muldf3"
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(mult:DF (match_operand:DF 1 "s_register_operand" "f")
		 (match_operand:DF 2 "fpu_rhs_operand" "fG")))]
  "TARGET_HARD_FLOAT"
  "muf%?d\\t%0, %1, %2"
[(set_attr "type" "fmul")])

(define_insn "*muldf_esfdf_df"
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(mult:DF (float_extend:DF
		  (match_operand:SF 1 "s_register_operand" "f"))
		 (match_operand:DF 2 "fpu_rhs_operand" "fG")))]
  "TARGET_HARD_FLOAT"
  "muf%?d\\t%0, %1, %2"
[(set_attr "type" "fmul")])

(define_insn "*muldf_df_esfdf"
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(mult:DF (match_operand:DF 1 "s_register_operand" "f")
		 (float_extend:DF
		  (match_operand:SF 2 "s_register_operand" "f"))))]
  "TARGET_HARD_FLOAT"
  "muf%?d\\t%0, %1, %2"
[(set_attr "type" "fmul")])

(define_insn "*muldf_esfdf_esfdf"
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(mult:DF (float_extend:DF
		  (match_operand:SF 1 "s_register_operand" "f"))
		 (float_extend:DF
		  (match_operand:SF 2 "s_register_operand" "f"))))]
  "TARGET_HARD_FLOAT"
  "muf%?d\\t%0, %1, %2"
[(set_attr "type" "fmul")])

(define_insn "mulxf3"
  [(set (match_operand:XF 0 "s_register_operand" "=f")
	(mult:XF (match_operand:XF 1 "s_register_operand" "f")
		 (match_operand:XF 2 "fpu_rhs_operand" "fG")))]
  "ENABLE_XF_PATTERNS && TARGET_HARD_FLOAT"
  "muf%?e\\t%0, %1, %2"
[(set_attr "type" "fmul")])

;; Division insns

(define_insn "divsf3"
  [(set (match_operand:SF 0 "s_register_operand" "=f,f")
	(div:SF (match_operand:SF 1 "fpu_rhs_operand" "f,G")
		(match_operand:SF 2 "fpu_rhs_operand" "fG,f")))]
  "TARGET_HARD_FLOAT"
  "@
   fdv%?s\\t%0, %1, %2
   frd%?s\\t%0, %2, %1"
[(set_attr "type" "fdivs")])

(define_insn "divdf3"
  [(set (match_operand:DF 0 "s_register_operand" "=f,f")
	(div:DF (match_operand:DF 1 "fpu_rhs_operand" "f,G")
		(match_operand:DF 2 "fpu_rhs_operand" "fG,f")))]
  "TARGET_HARD_FLOAT"
  "@
   dvf%?d\\t%0, %1, %2
   rdf%?d\\t%0, %2, %1"
[(set_attr "type" "fdivd")])

(define_insn "*divdf_esfdf_df"
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(div:DF (float_extend:DF
		 (match_operand:SF 1 "s_register_operand" "f"))
		(match_operand:DF 2 "fpu_rhs_operand" "fG")))]
  "TARGET_HARD_FLOAT"
  "dvf%?d\\t%0, %1, %2"
[(set_attr "type" "fdivd")])

(define_insn "*divdf_df_esfdf"
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(div:DF (match_operand:DF 1 "fpu_rhs_operand" "fG")
		(float_extend:DF
		 (match_operand:SF 2 "s_register_operand" "f"))))]
  "TARGET_HARD_FLOAT"
  "rdf%?d\\t%0, %2, %1"
[(set_attr "type" "fdivd")])

(define_insn "*divdf_esfdf_esfdf"
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(div:DF (float_extend:DF
		 (match_operand:SF 1 "s_register_operand" "f"))
		(float_extend:DF
		 (match_operand:SF 2 "s_register_operand" "f"))))]
  "TARGET_HARD_FLOAT"
  "dvf%?d\\t%0, %1, %2"
[(set_attr "type" "fdivd")])

(define_insn "divxf3"
  [(set (match_operand:XF 0 "s_register_operand" "=f,f")
	(div:XF (match_operand:XF 1 "fpu_rhs_operand" "f,G")
		(match_operand:XF 2 "fpu_rhs_operand" "fG,f")))]
  "ENABLE_XF_PATTERNS && TARGET_HARD_FLOAT"
  "@
   dvf%?e\\t%0, %1, %2
   rdf%?e\\t%0, %2, %1"
[(set_attr "type" "fdivx")])

;; Modulo insns

(define_insn "modsf3"
  [(set (match_operand:SF 0 "s_register_operand" "=f")
	(mod:SF (match_operand:SF 1 "s_register_operand" "f")
		(match_operand:SF 2 "fpu_rhs_operand" "fG")))]
  "TARGET_HARD_FLOAT"
  "rmf%?s\\t%0, %1, %2"
[(set_attr "type" "fdivs")])

(define_insn "moddf3"
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(mod:DF (match_operand:DF 1 "s_register_operand" "f")
		(match_operand:DF 2 "fpu_rhs_operand" "fG")))]
  "TARGET_HARD_FLOAT"
  "rmf%?d\\t%0, %1, %2"
[(set_attr "type" "fdivd")])

(define_insn "*moddf_esfdf_df"
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(mod:DF (float_extend:DF
		 (match_operand:SF 1 "s_register_operand" "f"))
		(match_operand:DF 2 "fpu_rhs_operand" "fG")))]
  "TARGET_HARD_FLOAT"
  "rmf%?d\\t%0, %1, %2"
[(set_attr "type" "fdivd")])

(define_insn "*moddf_df_esfdf"
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(mod:DF (match_operand:DF 1 "s_register_operand" "f")
		(float_extend:DF
		 (match_operand:SF 2 "s_register_operand" "f"))))]
  "TARGET_HARD_FLOAT"
  "rmf%?d\\t%0, %1, %2"
[(set_attr "type" "fdivd")])

(define_insn "*moddf_esfdf_esfdf"
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(mod:DF (float_extend:DF
		 (match_operand:SF 1 "s_register_operand" "f"))
		(float_extend:DF
		 (match_operand:SF 2 "s_register_operand" "f"))))]
  "TARGET_HARD_FLOAT"
  "rmf%?d\\t%0, %1, %2"
[(set_attr "type" "fdivd")])

(define_insn "modxf3"
  [(set (match_operand:XF 0 "s_register_operand" "=f")
	(mod:XF (match_operand:XF 1 "s_register_operand" "f")
		(match_operand:XF 2 "fpu_rhs_operand" "fG")))]
  "ENABLE_XF_PATTERNS && TARGET_HARD_FLOAT"
  "rmf%?e\\t%0, %1, %2"
[(set_attr "type" "fdivx")])

;; Boolean and,ior,xor insns

(define_insn "anddi3"
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(and:DI (match_operand:DI 1 "s_register_operand" "%0,0")
		(match_operand:DI 2 "s_register_operand" "r,0")))]
  ""
  "and%?\\t%Q0, %Q1, %Q2\;and%?\\t%R0, %R1, %R2"
[(set_attr "length" "8")])

(define_insn "*anddi_zesidi_di"
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(and:DI (zero_extend:DI
		 (match_operand:SI 2 "s_register_operand" "r,r"))
		(match_operand:DI 1 "s_register_operand" "?r,0")))]
  ""
  "and%?\\t%Q0, %Q1, %2\;mov%?\\t%R0, #0"
[(set_attr "length" "8")])

(define_insn "*anddi_sesdi_di"
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(and:DI (sign_extend:DI
		 (match_operand:SI 2 "s_register_operand" "r,r"))
		(match_operand:DI 1 "s_register_operand" "?r,0")))]
  ""
  "and%?\\t%Q0, %Q1, %2\;and%?\\t%R0, %R1, %2, asr #31"
[(set_attr "length" "8")])

(define_expand "andsi3"
  [(set (match_operand:SI 0 "s_register_operand" "")
	(and:SI (match_operand:SI 1 "s_register_operand" "")
		(match_operand:SI 2 "reg_or_int_operand" "")))]
  ""
  "
  if (GET_CODE (operands[2]) == CONST_INT)
    {
      arm_split_constant (AND, SImode, INTVAL (operands[2]), operands[0],
			  operands[1],
			  (reload_in_progress || reload_completed
			   ? 0 : preserve_subexpressions_p ()));
      DONE;
    }
")

(define_insn "*andsi3_insn"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r,r")
	(and:SI (match_operand:SI 1 "s_register_operand" "r,r,r")
		(match_operand:SI 2 "reg_or_int_operand" "rI,K,?n")))]
  ""
  "@
   and%?\\t%0, %1, %2
   bic%?\\t%0, %1, #%B2
   #"
[(set_attr "length" "4,4,16")])

(define_split
  [(set (match_operand:SI 0 "s_register_operand" "")
	(and:SI (match_operand:SI 1 "s_register_operand" "")
		(match_operand:SI 2 "const_int_operand" "")))]
  "! (const_ok_for_arm (INTVAL (operands[2]))
      || const_ok_for_arm (~ INTVAL (operands[2])))"
  [(clobber (const_int 0))]
  "
  arm_split_constant  (AND, SImode, INTVAL (operands[2]), operands[0],
		       operands[1], 0);
  DONE;
")

(define_insn "*andsi3_compare0"
  [(set (reg:CC_NOOV 24)
	(compare:CC_NOOV
	 (and:SI (match_operand:SI 1 "s_register_operand" "r,r")
		 (match_operand:SI 2 "arm_not_operand" "rI,K"))
	 (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=r,r")
	(and:SI (match_dup 1) (match_dup 2)))]
  ""
  "@
   and%?s\\t%0, %1, %2
   bic%?s\\t%0, %1, #%B2"
[(set_attr "conds" "set")])

(define_insn "*andsi3_compare0_scratch"
  [(set (reg:CC_NOOV 24)
	(compare:CC_NOOV
	 (and:SI (match_operand:SI 0 "s_register_operand" "r,r")
		 (match_operand:SI 1 "arm_not_operand" "rI,K"))
	 (const_int 0)))
   (clobber (match_scratch:SI 3 "=X,r"))]
  ""
  "@
   tst%?\\t%0, %1
   bic%?s\\t%3, %0, #%B1"
[(set_attr "conds" "set")])

(define_insn "*zeroextractsi_compare0_scratch"
  [(set (reg:CC_NOOV 24)
	(compare:CC_NOOV (zero_extract:SI
			  (match_operand:SI 0 "s_register_operand" "r")
		 	  (match_operand 1 "const_int_operand" "n")
			  (match_operand 2 "const_int_operand" "n"))
			 (const_int 0)))]
  "INTVAL (operands[2]) >= 0 && INTVAL (operands[2]) < 32
   && INTVAL (operands[1]) > 0 
   && INTVAL (operands[1]) + (INTVAL (operands[2]) & 1) <= 8
   && INTVAL (operands[1]) + INTVAL (operands[2]) <= 32"
  "*
  operands[1] = GEN_INT (((1 << INTVAL (operands[1])) - 1)
			 << INTVAL (operands[2]));
  output_asm_insn (\"tst%?\\t%0, %1\", operands);
  return \"\";
"
[(set_attr "conds" "set")])

(define_insn "*ne_zeroextractsi"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(ne:SI (zero_extract:SI
		(match_operand:SI 1 "s_register_operand" "r")
		(match_operand:SI 2 "const_int_operand" "n")
		(match_operand:SI 3 "const_int_operand" "n"))
	       (const_int 0)))]
  "INTVAL (operands[3]) >= 0 && INTVAL (operands[3]) < 32
   && INTVAL (operands[2]) > 0 
   && INTVAL (operands[2]) + (INTVAL (operands[3]) & 1) <= 8
   && INTVAL (operands[2]) + INTVAL (operands[3]) <= 32"
  "*
  operands[2] = GEN_INT (((1 << INTVAL (operands[2])) - 1)
			 << INTVAL (operands[3]));
  output_asm_insn (\"ands\\t%0, %1, %2\", operands);
  return \"movne\\t%0, #1\";
"
[(set_attr "conds" "clob")
 (set_attr "length" "8")])

;;; ??? This pattern is bogus.  If operand3 has bits outside the range
;;; represented by the bitfield, then this will produce incorrect results.
;;; Somewhere, the value needs to be truncated.  On targets like the m68k,
;;; which have a real bitfield insert instruction, the truncation happens
;;; in the bitfield insert instruction itself.  Since arm does not have a
;;; bitfield insert instruction, we would have to emit code here to truncate
;;; the value before we insert.  This loses some of the advantage of having
;;; this insv pattern, so this pattern needs to be reevalutated.

(define_expand "insv"
  [(set (zero_extract:SI (match_operand:SI 0 "s_register_operand" "")
                         (match_operand:SI 1 "general_operand" "")
                         (match_operand:SI 2 "general_operand" ""))
        (match_operand:SI 3 "nonmemory_operand" ""))]
  ""
  "
{
  int start_bit = INTVAL (operands[2]);
  int width = INTVAL (operands[1]);
  HOST_WIDE_INT mask = (((HOST_WIDE_INT)1) << width) - 1;
  rtx target, subtarget;
  
  target = operands[0];
  /* Avoid using a subreg as a subtarget, and avoid writing a paradoxical 
     subreg as the final target.  */
  if (GET_CODE (target) == SUBREG)
    {
      subtarget = gen_reg_rtx (SImode);
      if (GET_MODE_SIZE (GET_MODE (SUBREG_REG (target)))
	  < GET_MODE_SIZE (SImode))
        target = SUBREG_REG (target);
    }
  else
    subtarget = target;    
    
  if (GET_CODE (operands[3]) == CONST_INT)
    {
      /* Since we are inserting a known constant, we may be able to
	 reduce the number of bits that we have to clear so that
	 the mask becomes simple.  */
      /* ??? This code does not check to see if the new mask is actually
	 simpler.  It may not be.  */
      rtx op1 = gen_reg_rtx (SImode);
      /* ??? Truncate operand3 to fit in the bitfield.  See comment before
	 start of this pattern.  */
      HOST_WIDE_INT op3_value = mask & INTVAL (operands[3]);
      HOST_WIDE_INT mask2 = ((mask & ~op3_value) << start_bit);

      emit_insn (gen_andsi3 (op1, operands[0], GEN_INT (~mask2)));
      emit_insn (gen_iorsi3 (subtarget, op1,
			     GEN_INT (op3_value << start_bit)));
    }
  else if (start_bit == 0
	   && ! (const_ok_for_arm (mask)
		 || const_ok_for_arm (~mask)))
    {
      /* A Trick, since we are setting the bottom bits in the word,
	 we can shift operand[3] up, operand[0] down, OR them together
	 and rotate the result back again.  This takes 3 insns, and
	 the third might be mergable into another op.  */
      /* The shift up copes with the possibility that operand[3] is
         wider than the bitfield.  */
      rtx op0 = gen_reg_rtx (SImode);
      rtx op1 = gen_reg_rtx (SImode);

      emit_insn (gen_ashlsi3 (op0, operands[3], GEN_INT (32 - width)));
      emit_insn (gen_iorsi3 (op1, gen_rtx_LSHIFTRT (SImode, operands[0],
							    operands[1]),
			     op0));
      emit_insn (gen_rotlsi3 (subtarget, op1, operands[1]));
    }
  else if ((width + start_bit == 32)
	   && ! (const_ok_for_arm (mask)
		 || const_ok_for_arm (~mask)))
    {
      /* Similar trick, but slightly less efficient.  */

      rtx op0 = gen_reg_rtx (SImode);
      rtx op1 = gen_reg_rtx (SImode);

      emit_insn (gen_ashlsi3 (op0, operands[3], GEN_INT (32 - width)));
      emit_insn (gen_ashlsi3 (op1, operands[0], operands[1]));
      emit_insn (gen_iorsi3 (subtarget,
			     gen_rtx_LSHIFTRT (SImode, op1, operands[1]),
			     op0));
    }
  else
    {
      rtx op0 = GEN_INT (mask);
      rtx op1 = gen_reg_rtx (SImode);
      rtx op2 = gen_reg_rtx (SImode);

      if (! (const_ok_for_arm (mask) || const_ok_for_arm (~mask)))
	{
	  rtx tmp = gen_reg_rtx (SImode);

	  emit_insn (gen_movsi (tmp, op0));
	  op0 = tmp;
	}

      /* Mask out any bits in operand[3] that are not needed.  */
      emit_insn (gen_andsi3 (op1, operands[3], op0));

      if (GET_CODE (op0) == CONST_INT
	  && (const_ok_for_arm (mask << start_bit)
	      || const_ok_for_arm (~ (mask << start_bit))))
	{
	  op0 = GEN_INT (~(mask << start_bit));
	  emit_insn (gen_andsi3 (op2, operands[0], op0));
	}
      else
	{
	  if (GET_CODE (op0) == CONST_INT)
	    {
	      rtx tmp = gen_reg_rtx (SImode);

	      emit_insn (gen_movsi (tmp, op0));
	      op0 = tmp;
	    }

	  if (start_bit != 0)
	    op0 = gen_rtx_ASHIFT (SImode, op0, operands[2]);
	    
	  emit_insn (gen_andsi_notsi_si (op2, operands[0], op0));
	}

      if (start_bit != 0)
	op1 = gen_rtx_ASHIFT (SImode, op1, operands[2]);

      emit_insn (gen_iorsi3 (subtarget, op1, op2));
    }

  if (subtarget != target)
    {
      /* If TARGET is still a SUBREG, then it must be wider than a word,
	 so we must be careful only to set the subword we were asked to. */
      if (GET_CODE (target) == SUBREG)
	emit_move_insn (target, subtarget);
      else
	emit_move_insn (target, gen_lowpart (GET_MODE (target), subtarget));
    }

  DONE;
}
")

;; constants for op 2 will never be given to these patterns.
(define_insn "*anddi_notdi_di"
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(and:DI (not:DI (match_operand:DI 2 "s_register_operand" "r,0"))
		(match_operand:DI 1 "s_register_operand" "0,r")))]
  ""
  "bic%?\\t%Q0, %Q1, %Q2\;bic%?\\t%R0, %R1, %R2"
[(set_attr "length" "8")])
  
(define_insn "*anddi_notzesidi_di"
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(and:DI (not:DI (zero_extend:DI
			 (match_operand:SI 2 "s_register_operand" "r,r")))
		(match_operand:DI 1 "s_register_operand" "0,?r")))]
  ""
  "@
   bic%?\\t%Q0, %Q1, %2
   bic%?\\t%Q0, %Q1, %2\;mov%?\\t%R0, %R1"
[(set_attr "length" "4,8")])
  
(define_insn "*anddi_notsesidi_di"
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(and:DI (not:DI (sign_extend:DI
			 (match_operand:SI 2 "s_register_operand" "r,r")))
		(match_operand:DI 1 "s_register_operand" "?r,0")))]
  ""
  "bic%?\\t%Q0, %Q1, %2\;bic%?\\t%R0, %R1, %2, asr #31"
[(set_attr "length" "8")])
  
(define_insn "andsi_notsi_si"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(and:SI (not:SI (match_operand:SI 2 "s_register_operand" "r"))
		(match_operand:SI 1 "s_register_operand" "r")))]
  ""
  "bic%?\\t%0, %1, %2")

(define_insn "andsi_not_shiftsi_si"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(and:SI (not:SI (match_operator:SI 4 "shift_operator"
			 [(match_operand:SI 2 "s_register_operand" "r")
			  (match_operand:SI 3 "arm_rhs_operand" "rM")]))
		(match_operand:SI 1 "s_register_operand" "r")))]
  ""
  "bic%?\\t%0, %1, %2%S4")

(define_insn "*andsi_notsi_si_compare0"
  [(set (reg:CC_NOOV 24)
	(compare:CC_NOOV
	 (and:SI (not:SI (match_operand:SI 2 "s_register_operand" "r"))
		 (match_operand:SI 1 "s_register_operand" "r"))
	 (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(and:SI (not:SI (match_dup 2)) (match_dup 1)))]
  ""
  "bic%?s\\t%0, %1, %2"
[(set_attr "conds" "set")])

(define_insn "*andsi_notsi_si_compare0_scratch"
  [(set (reg:CC_NOOV 24)
	(compare:CC_NOOV
	 (and:SI (not:SI (match_operand:SI 2 "s_register_operand" "r"))
		 (match_operand:SI 1 "s_register_operand" "r"))
	 (const_int 0)))
   (clobber (match_scratch:SI 0 "=r"))]
  ""
  "bic%?s\\t%0, %1, %2"
[(set_attr "conds" "set")])

(define_insn "iordi3"
  [(set (match_operand:DI 0 "s_register_operand" "=&r")
	(ior:DI (match_operand:DI 1 "s_register_operand" "%0")
		(match_operand:DI 2 "s_register_operand" "r")))]
  ""
  "orr%?\\t%Q0, %Q1, %Q2\;orr%?\\t%R0, %R1, %R2"
[(set_attr "length" "8")])

(define_insn "*iordi_zesidi_di"
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(ior:DI (zero_extend:DI
		 (match_operand:SI 2 "s_register_operand" "r,r"))
		(match_operand:DI 1 "s_register_operand" "0,?r")))]
  ""
  "@
   orr%?\\t%Q0, %Q1, %2
   orr%?\\t%Q0, %Q1, %2\;mov%?\\t%R0, %R1"
[(set_attr "length" "4,8")])

(define_insn "*iordi_sesidi_di"
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(ior:DI (sign_extend:DI
		 (match_operand:SI 2 "s_register_operand" "r,r"))
		(match_operand:DI 1 "s_register_operand" "?r,0")))]
  ""
  "orr%?\\t%Q0, %Q1, %2\;orr%?\\t%R0, %R1, %2, asr #31"
[(set_attr "length" "8")])

(define_expand "iorsi3"
  [(set (match_operand:SI 0 "s_register_operand" "")
	(ior:SI (match_operand:SI 1 "s_register_operand" "")
		(match_operand:SI 2 "reg_or_int_operand" "")))]
  ""
  "
  if (GET_CODE (operands[2]) == CONST_INT)
    {
      arm_split_constant (IOR, SImode, INTVAL (operands[2]), operands[0],
			  operands[1],
			  (reload_in_progress || reload_completed
			   ? 0 : preserve_subexpressions_p ()));
      DONE;
    }
")

(define_insn "*iorsi3_insn"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(ior:SI (match_operand:SI 1 "s_register_operand" "r,r")
		(match_operand:SI 2 "reg_or_int_operand" "rI,?n")))]
  ""
  "@
   orr%?\\t%0, %1, %2
   #"
[(set_attr "length" "4,16")])

(define_split
  [(set (match_operand:SI 0 "s_register_operand" "")
	(ior:SI (match_operand:SI 1 "s_register_operand" "")
		(match_operand:SI 2 "const_int_operand" "")))]
  "! const_ok_for_arm (INTVAL (operands[2]))"
  [(clobber (const_int 0))]
  "
  arm_split_constant (IOR, SImode, INTVAL (operands[2]), operands[0],
		      operands[1], 0);
  DONE;
")
  
(define_insn "*iorsi3_compare0"
  [(set (reg:CC_NOOV 24)
	(compare:CC_NOOV (ior:SI (match_operand:SI 1 "s_register_operand" "%r")
				 (match_operand:SI 2 "arm_rhs_operand" "rI"))
			 (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(ior:SI (match_dup 1) (match_dup 2)))]
  ""
  "orr%?s\\t%0, %1, %2"
[(set_attr "conds" "set")])

(define_insn "*iorsi3_compare0_scratch"
  [(set (reg:CC_NOOV 24)
	(compare:CC_NOOV (ior:SI (match_operand:SI 1 "s_register_operand" "%r")
				 (match_operand:SI 2 "arm_rhs_operand" "rI"))
			 (const_int 0)))
   (clobber (match_scratch:SI 0 "=r"))]
  ""
  "orr%?s\\t%0, %1, %2"
[(set_attr "conds" "set")])

(define_insn "xordi3"
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(xor:DI (match_operand:DI 1 "s_register_operand" "%0,0")
		(match_operand:DI 2 "s_register_operand" "r,0")))]
  ""
  "eor%?\\t%Q0, %Q1, %Q2\;eor%?\\t%R0, %R1, %R2"
[(set_attr "length" "8")])

(define_insn "*xordi_zesidi_di"
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(xor:DI (zero_extend:DI
		 (match_operand:SI 2 "s_register_operand" "r,r"))
		(match_operand:DI 1 "s_register_operand" "0,?r")))]
  ""
  "@
   eor%?\\t%Q0, %Q1, %2
   eor%?\\t%Q0, %Q1, %2\;mov%?\\t%R0, %R1"
[(set_attr "length" "4,8")])

(define_insn "*xordi_sesidi_di"
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(xor:DI (sign_extend:DI
		 (match_operand:SI 2 "s_register_operand" "r,r"))
		(match_operand:DI 1 "s_register_operand" "?r,0")))]
  ""
  "eor%?\\t%Q0, %Q1, %2\;eor%?\\t%R0, %R1, %2, asr #31"
[(set_attr "length" "8")])

(define_insn "xorsi3"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(xor:SI (match_operand:SI 1 "s_register_operand" "r")
		(match_operand:SI 2 "arm_rhs_operand" "rI")))]
  ""
  "eor%?\\t%0, %1, %2")

(define_insn "*xorsi3_compare0"
  [(set (reg:CC_NOOV 24)
	(compare:CC_NOOV (xor:SI (match_operand:SI 1 "s_register_operand" "r")
				 (match_operand:SI 2 "arm_rhs_operand" "rI"))
			 (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(xor:SI (match_dup 1) (match_dup 2)))]
  ""
  "eor%?s\\t%0, %1, %2"
[(set_attr "conds" "set")])

(define_insn "*xorsi3_compare0_scratch"
  [(set (reg:CC_NOOV 24)
	(compare:CC_NOOV (xor:SI (match_operand:SI 0 "s_register_operand" "r")
				 (match_operand:SI 1 "arm_rhs_operand" "rI"))
			 (const_int 0)))]
  ""
  "teq%?\\t%0, %1"
[(set_attr "conds" "set")])

;; by splitting (IOR (AND (NOT A) (NOT B)) C) as D = AND (IOR A B) (NOT C), 
;; (NOT D) we can sometimes merge the final NOT into one of the following
;; insns

(define_split
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(ior:SI (and:SI (not:SI (match_operand:SI 1 "s_register_operand" "r"))
			(not:SI (match_operand:SI 2 "arm_rhs_operand" "rI")))
		(match_operand:SI 3 "arm_rhs_operand" "rI")))
   (clobber (match_operand:SI 4 "s_register_operand" "=r"))]
  ""
  [(set (match_dup 4) (and:SI (ior:SI (match_dup 1) (match_dup 2))
			      (not:SI (match_dup 3))))
   (set (match_dup 0) (not:SI (match_dup 4)))]
  ""
)

(define_insn "*andsi_iorsi3_notsi"
  [(set (match_operand:SI 0 "s_register_operand" "=&r,&r,&r")
	(and:SI (ior:SI (match_operand:SI 1 "s_register_operand" "r,r,0")
			(match_operand:SI 2 "arm_rhs_operand" "rI,0,rI"))
		(not:SI (match_operand:SI 3 "arm_rhs_operand" "rI,rI,rI"))))]
  ""
  "orr%?\\t%0, %1, %2\;bic%?\\t%0, %0, %3"
[(set_attr "length" "8")])



;; Minimum and maximum insns

(define_insn "smaxsi3"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r,r")
	(smax:SI (match_operand:SI 1 "s_register_operand" "0,r,?r")
		 (match_operand:SI 2 "arm_rhs_operand" "rI,0,rI")))
   (clobber (reg:CC 24))]
  ""
  "@
   cmp\\t%1, %2\;movlt\\t%0, %2
   cmp\\t%1, %2\;movge\\t%0, %1
   cmp\\t%1, %2\;movge\\t%0, %1\;movlt\\t%0, %2"
[(set_attr "conds" "clob")
 (set_attr "length" "8,8,12")])

(define_insn "sminsi3"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r,r")
	(smin:SI (match_operand:SI 1 "s_register_operand" "0,r,?r")
		 (match_operand:SI 2 "arm_rhs_operand" "rI,0,rI")))
   (clobber (reg:CC 24))]
  ""
  "@
   cmp\\t%1, %2\;movge\\t%0, %2
   cmp\\t%1, %2\;movlt\\t%0, %1
   cmp\\t%1, %2\;movlt\\t%0, %1\;movge\\t%0, %2"
[(set_attr "conds" "clob")
 (set_attr "length" "8,8,12")])

(define_insn "umaxsi3"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r,r")
	(umax:SI (match_operand:SI 1 "s_register_operand" "0,r,?r")
		 (match_operand:SI 2 "arm_rhs_operand" "rI,0,rI")))
   (clobber (reg:CC 24))]
  ""
  "@
   cmp\\t%1, %2\;movcc\\t%0, %2
   cmp\\t%1, %2\;movcs\\t%0, %1
   cmp\\t%1, %2\;movcs\\t%0, %1\;movcc\\t%0, %2"
[(set_attr "conds" "clob")
 (set_attr "length" "8,8,12")])

(define_insn "uminsi3"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r,r")
	(umin:SI (match_operand:SI 1 "s_register_operand" "0,r,?r")
		 (match_operand:SI 2 "arm_rhs_operand" "rI,0,rI")))
   (clobber (reg:CC 24))]
  ""
  "@
   cmp\\t%1, %2\;movcs\\t%0, %2
   cmp\\t%1, %2\;movcc\\t%0, %1
   cmp\\t%1, %2\;movcc\\t%0, %1\;movcs\\t%0, %2"
[(set_attr "conds" "clob")
 (set_attr "length" "8,8,12")])

(define_insn "*store_minmaxsi"
  [(set (match_operand:SI 0 "memory_operand" "=m")
	(match_operator:SI 3 "minmax_operator"
	 [(match_operand:SI 1 "s_register_operand" "r")
	  (match_operand:SI 2 "s_register_operand" "r")]))
   (clobber (reg:CC 24))]
  ""
  "*
  operands[3] = gen_rtx (minmax_code (operands[3]), SImode, operands[1],
			 operands[2]);
  output_asm_insn (\"cmp\\t%1, %2\", operands);
  output_asm_insn (\"str%d3\\t%1, %0\", operands);
  output_asm_insn (\"str%D3\\t%2, %0\", operands);
  return \"\";
"
[(set_attr "conds" "clob")
 (set_attr "length" "12")
 (set_attr "type" "store1")])

; Reject the frame pointer in operand[1], since reloading this after
; it has been eliminated can cause carnage.
(define_insn "*minmax_arithsi"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(match_operator:SI 4 "shiftable_operator"
	 [(match_operator:SI 5 "minmax_operator"
	   [(match_operand:SI 2 "s_register_operand" "r,r")
	    (match_operand:SI 3 "arm_rhs_operand" "rI,rI")])
	  (match_operand:SI 1 "s_register_operand" "0,?r")]))
   (clobber (reg:CC 24))]
  "GET_CODE (operands[1]) != REG
   || (REGNO(operands[1]) != FRAME_POINTER_REGNUM
       && REGNO(operands[1]) != ARG_POINTER_REGNUM)"
  "*
{
  enum rtx_code code = GET_CODE (operands[4]);

  operands[5] = gen_rtx (minmax_code (operands[5]), SImode, operands[2],
			 operands[3]);
  output_asm_insn (\"cmp\\t%2, %3\", operands);
  output_asm_insn (\"%i4%d5\\t%0, %1, %2\", operands);
  if (which_alternative != 0 || operands[3] != const0_rtx
      || (code != PLUS && code != MINUS && code != IOR && code != XOR))
    output_asm_insn (\"%i4%D5\\t%0, %1, %3\", operands);
  return \"\";
}
"
[(set_attr "conds" "clob")
 (set_attr "length" "12")])


;; Shift and rotation insns

(define_expand "ashlsi3"
  [(set (match_operand:SI 0 "s_register_operand" "")
	(ashift:SI (match_operand:SI 1 "s_register_operand" "")
		   (match_operand:SI 2 "arm_rhs_operand" "")))]
  ""
  "
  if (GET_CODE (operands[2]) == CONST_INT
      && ((unsigned HOST_WIDE_INT) INTVAL (operands[2])) > 31)
    {
      emit_insn (gen_movsi (operands[0], const0_rtx));
      DONE;
    }
")

(define_expand "ashrsi3"
  [(set (match_operand:SI 0 "s_register_operand" "")
	(ashiftrt:SI (match_operand:SI 1 "s_register_operand" "")
		     (match_operand:SI 2 "arm_rhs_operand" "")))]
  ""
  "
  if (GET_CODE (operands[2]) == CONST_INT
      && ((unsigned HOST_WIDE_INT) INTVAL (operands[2])) > 31)
    operands[2] = GEN_INT (31);
")

(define_expand "lshrsi3"
  [(set (match_operand:SI 0 "s_register_operand" "")
	(lshiftrt:SI (match_operand:SI 1 "s_register_operand" "")
		     (match_operand:SI 2 "arm_rhs_operand" "")))]
  ""
  "
  if (GET_CODE (operands[2]) == CONST_INT
      && ((unsigned HOST_WIDE_INT) INTVAL (operands[2])) > 31)
    {
      emit_insn (gen_movsi (operands[0], const0_rtx));
      DONE;
    }
")

(define_expand "rotlsi3"
  [(set (match_operand:SI 0 "s_register_operand" "")
	(rotatert:SI (match_operand:SI 1 "s_register_operand" "")
		     (match_operand:SI 2 "reg_or_int_operand" "")))]
  ""
  "
  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT ((32 - INTVAL (operands[2])) % 32);
  else
    {
      rtx reg = gen_reg_rtx (SImode);
      emit_insn (gen_subsi3 (reg, GEN_INT (32), operands[2]));
      operands[2] = reg;
    }
")

(define_expand "rotrsi3"
  [(set (match_operand:SI 0 "s_register_operand" "")
	(rotatert:SI (match_operand:SI 1 "s_register_operand" "")
		     (match_operand:SI 2 "arm_rhs_operand" "")))]
  ""
  "
  if (GET_CODE (operands[2]) == CONST_INT
      && ((unsigned HOST_WIDE_INT) INTVAL (operands[2])) > 31)
    operands[2] = GEN_INT (INTVAL (operands[2]) % 32);
")

(define_insn "*shiftsi3"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(match_operator:SI 3 "shift_operator"
	 [(match_operand:SI 1 "s_register_operand" "r")
	  (match_operand:SI 2 "reg_or_int_operand" "rM")]))]
  ""
  "mov%?\\t%0, %1%S3")

(define_insn "*shiftsi3_compare0"
  [(set (reg:CC_NOOV 24)
	(compare:CC_NOOV (match_operator:SI 3 "shift_operator"
			  [(match_operand:SI 1 "s_register_operand" "r")
			   (match_operand:SI 2 "arm_rhs_operand" "rM")])
			 (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(match_op_dup 3 [(match_dup 1) (match_dup 2)]))]
  ""
  "mov%?s\\t%0, %1%S3"
[(set_attr "conds" "set")])

(define_insn "*shiftsi3_compare0_scratch"
  [(set (reg:CC_NOOV 24)
	(compare:CC_NOOV (match_operator:SI 3 "shift_operator"
			  [(match_operand:SI 1 "s_register_operand" "r")
			   (match_operand:SI 2 "arm_rhs_operand" "rM")])
			 (const_int 0)))
   (clobber (match_scratch:SI 0 "=r"))]
  ""
  "mov%?s\\t%0, %1%S3"
[(set_attr "conds" "set")])

(define_insn "*notsi_shiftsi"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(not:SI (match_operator:SI 3 "shift_operator"
		 [(match_operand:SI 1 "s_register_operand" "r")
		  (match_operand:SI 2 "arm_rhs_operand" "rM")])))]
  ""
  "mvn%?\\t%0, %1%S3")

(define_insn "*notsi_shiftsi_compare0"
  [(set (reg:CC_NOOV 24)
	(compare:CC_NOOV (not:SI (match_operator:SI 3 "shift_operator"
			  [(match_operand:SI 1 "s_register_operand" "r")
			   (match_operand:SI 2 "arm_rhs_operand" "rM")]))
			 (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(not:SI (match_op_dup 3 [(match_dup 1) (match_dup 2)])))]
  ""
  "mvn%?s\\t%0, %1%S3"
[(set_attr "conds" "set")])

(define_insn "*not_shiftsi_compare0_scratch"
  [(set (reg:CC_NOOV 24)
	(compare:CC_NOOV (not:SI (match_operator:SI 3 "shift_operator"
			  [(match_operand:SI 1 "s_register_operand" "r")
			   (match_operand:SI 2 "arm_rhs_operand" "rM")]))
			 (const_int 0)))
   (clobber (match_scratch:SI 0 "=r"))]
  ""
  "mvn%?s\\t%0, %1%S3"
[(set_attr "conds" "set")])


;; Unary arithmetic insns

(define_insn "negdi2"
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(neg:DI (match_operand:DI 1 "s_register_operand" "?r,0")))]
  ""
  "rsbs\\t%Q0, %Q1, #0\;rsc\\t%R0, %R1, #0"
[(set_attr "conds" "clob")
 (set_attr "length" "8")])

(define_insn "negsi2"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(neg:SI (match_operand:SI 1 "s_register_operand" "r")))]
  ""
  "rsb%?\\t%0, %1, #0")

(define_insn "negsf2"
  [(set (match_operand:SF 0 "s_register_operand" "=f")
	(neg:SF (match_operand:SF 1 "s_register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "mnf%?s\\t%0, %1"
[(set_attr "type" "ffarith")])

(define_insn "negdf2"
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(neg:DF (match_operand:DF 1 "s_register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "mnf%?d\\t%0, %1"
[(set_attr "type" "ffarith")])

(define_insn "*negdf_esfdf"
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(neg:DF (float_extend:DF
		 (match_operand:SF 1 "s_register_operand" "f"))))]
  "TARGET_HARD_FLOAT"
  "mnf%?d\\t%0, %1"
[(set_attr "type" "ffarith")])

(define_insn "negxf2"
  [(set (match_operand:XF 0 "s_register_operand" "=f")
	(neg:XF (match_operand:XF 1 "s_register_operand" "f")))]
  "ENABLE_XF_PATTERNS && TARGET_HARD_FLOAT"
  "mnf%?e\\t%0, %1"
[(set_attr "type" "ffarith")])

;; abssi2 doesn't really clobber the condition codes if a different register
;; is being set.  To keep things simple, assume during rtl manipulations that
;; it does, but tell the final scan operator the truth.  Similarly for
;; (neg (abs...))

(define_insn "abssi2"
  [(set (match_operand:SI 0 "s_register_operand" "=r,&r")
	(abs:SI (match_operand:SI 1 "s_register_operand" "0,r")))
   (clobber (reg:CC 24))]
  ""
  "@
   cmp\\t%0, #0\;rsblt\\t%0, %0, #0
   eor%?\\t%0, %1, %1, asr #31\;sub%?\\t%0, %0, %1, asr #31"
[(set_attr "conds" "clob,*")
 (set_attr "length" "8")])

(define_insn "*neg_abssi2"
  [(set (match_operand:SI 0 "s_register_operand" "=r,&r")
	(neg:SI (abs:SI (match_operand:SI 1 "s_register_operand" "0,r"))))
   (clobber (reg:CC 24))]
  ""
  "@
   cmp\\t%0, #0\;rsbgt\\t%0, %0, #0
   eor%?\\t%0, %1, %1, asr #31\;rsb%?\\t%0, %0, %1, asr #31"
[(set_attr "conds" "clob,*")
 (set_attr "length" "8")])

(define_insn "abssf2"
  [(set (match_operand:SF 0 "s_register_operand" "=f")
	 (abs:SF (match_operand:SF 1 "s_register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "abs%?s\\t%0, %1"
[(set_attr "type" "ffarith")])

(define_insn "absdf2"
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(abs:DF (match_operand:DF 1 "s_register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "abs%?d\\t%0, %1"
[(set_attr "type" "ffarith")])

(define_insn "*absdf_esfdf"
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(abs:DF (float_extend:DF
		 (match_operand:SF 1 "s_register_operand" "f"))))]
  "TARGET_HARD_FLOAT"
  "abs%?d\\t%0, %1"
[(set_attr "type" "ffarith")])

(define_insn "absxf2"
  [(set (match_operand:XF 0 "s_register_operand" "=f")
	(abs:XF (match_operand:XF 1 "s_register_operand" "f")))]
  "ENABLE_XF_PATTERNS && TARGET_HARD_FLOAT"
  "abs%?e\\t%0, %1"
[(set_attr "type" "ffarith")])

(define_insn "sqrtsf2"
  [(set (match_operand:SF 0 "s_register_operand" "=f")
	(sqrt:SF (match_operand:SF 1 "s_register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "sqt%?s\\t%0, %1"
[(set_attr "type" "float_em")])

(define_insn "sqrtdf2"
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(sqrt:DF (match_operand:DF 1 "s_register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "sqt%?d\\t%0, %1"
[(set_attr "type" "float_em")])

(define_insn "*sqrtdf_esfdf"
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(sqrt:DF (float_extend:DF
		  (match_operand:SF 1 "s_register_operand" "f"))))]
  "TARGET_HARD_FLOAT"
  "sqt%?d\\t%0, %1"
[(set_attr "type" "float_em")])

(define_insn "sqrtxf2"
  [(set (match_operand:XF 0 "s_register_operand" "=f")
	(sqrt:XF (match_operand:XF 1 "s_register_operand" "f")))]
  "ENABLE_XF_PATTERNS && TARGET_HARD_FLOAT"
  "sqt%?e\\t%0, %1"
[(set_attr "type" "float_em")])

;; SIN COS TAN and family are always emulated, so it's probably better
;; to always call a library function.
;(define_insn "sinsf2"
;  [(set (match_operand:SF 0 "s_register_operand" "=f")
;	(unspec:SF [(match_operand:SF 1 "s_register_operand" "f")] 0))]
;  "TARGET_HARD_FLOAT"
;  "sin%?s\\t%0, %1"
;[(set_attr "type" "float_em")])
;
;(define_insn "sindf2"
;  [(set (match_operand:DF 0 "s_register_operand" "=f")
;	(unspec:DF [(match_operand:DF 1 "s_register_operand" "f")] 0))]
;  "TARGET_HARD_FLOAT"
;  "sin%?d\\t%0, %1"
;[(set_attr "type" "float_em")])
;
;(define_insn "*sindf_esfdf"
;  [(set (match_operand:DF 0 "s_register_operand" "=f")
;	(unspec:DF [(float_extend:DF
;		     (match_operand:SF 1 "s_register_operand" "f"))] 0))]
;  "TARGET_HARD_FLOAT"
;  "sin%?d\\t%0, %1"
;[(set_attr "type" "float_em")])
;
;(define_insn "sinxf2"
;  [(set (match_operand:XF 0 "s_register_operand" "=f")
;	(unspec:XF [(match_operand:XF 1 "s_register_operand" "f")] 0))]
;  "ENABLE_XF_PATTERNS && TARGET_HARD_FLOAT"
;  "sin%?e\\t%0, %1"
;[(set_attr "type" "float_em")])
;
;(define_insn "cossf2"
;  [(set (match_operand:SF 0 "s_register_operand" "=f")
;	(unspec:SF [(match_operand:SF 1 "s_register_operand" "f")] 1))]
;  "TARGET_HARD_FLOAT"
;  "cos%?s\\t%0, %1"
;[(set_attr "type" "float_em")])
;
;(define_insn "cosdf2"
;  [(set (match_operand:DF 0 "s_register_operand" "=f")
;	(unspec:DF [(match_operand:DF 1 "s_register_operand" "f")] 1))]
;  "TARGET_HARD_FLOAT"
;  "cos%?d\\t%0, %1"
;[(set_attr "type" "float_em")])
;
;(define_insn "*cosdf_esfdf"
;  [(set (match_operand:DF 0 "s_register_operand" "=f")
;	(unspec:DF [(float_extend:DF
;		     (match_operand:SF 1 "s_register_operand" "f"))] 1))]
;  "TARGET_HARD_FLOAT"
;  "cos%?d\\t%0, %1"
;[(set_attr "type" "float_em")])
;
;(define_insn "cosxf2"
;  [(set (match_operand:XF 0 "s_register_operand" "=f")
;	(unspec:XF [(match_operand:XF 1 "s_register_operand" "f")] 1))]
;  "ENABLE_XF_PATTERNS && TARGET_HARD_FLOAT"
;  "cos%?e\\t%0, %1"
;[(set_attr "type" "float_em")])

(define_insn "one_cmpldi2"
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(not:DI (match_operand:DI 1 "s_register_operand" "?r,0")))]
  ""
  "mvn%?\\t%Q0, %Q1\;mvn%?\\t%R0, %R1"
[(set_attr "length" "8")])

(define_insn "one_cmplsi2"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(not:SI (match_operand:SI 1 "s_register_operand" "r")))]
  ""
  "mvn%?\\t%0, %1")

(define_insn "*notsi_compare0"
  [(set (reg:CC_NOOV 24)
	(compare:CC_NOOV (not:SI (match_operand:SI 1 "s_register_operand" "r"))
			 (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(not:SI (match_dup 1)))]
  ""
  "mvn%?s\\t%0, %1"
[(set_attr "conds" "set")])

(define_insn "*notsi_compare0_scratch"
  [(set (reg:CC_NOOV 24)
	(compare:CC_NOOV (not:SI (match_operand:SI 1 "s_register_operand" "r"))
			 (const_int 0)))
   (clobber (match_scratch:SI 0 "=r"))]
  ""
  "mvn%?s\\t%0, %1"
[(set_attr "conds" "set")])

;; Fixed <--> Floating conversion insns

(define_insn "floatsisf2"
  [(set (match_operand:SF 0 "s_register_operand" "=f")
	(float:SF (match_operand:SI 1 "s_register_operand" "r")))]
  "TARGET_HARD_FLOAT"
  "flt%?s\\t%0, %1"
[(set_attr "type" "r_2_f")])

(define_insn "floatsidf2"
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(float:DF (match_operand:SI 1 "s_register_operand" "r")))]
  "TARGET_HARD_FLOAT"
  "flt%?d\\t%0, %1"
[(set_attr "type" "r_2_f")])

(define_insn "floatsixf2"
  [(set (match_operand:XF 0 "s_register_operand" "=f")
	(float:XF (match_operand:SI 1 "s_register_operand" "r")))]
  "ENABLE_XF_PATTERNS && TARGET_HARD_FLOAT"
  "flt%?e\\t%0, %1"
[(set_attr "type" "r_2_f")])

(define_insn "fix_truncsfsi2"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(fix:SI (match_operand:SF 1 "s_register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "fix%?z\\t%0, %1"
[(set_attr "type" "f_2_r")])

(define_insn "fix_truncdfsi2"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(fix:SI (match_operand:DF 1 "s_register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "fix%?z\\t%0, %1"
[(set_attr "type" "f_2_r")])

(define_insn "fix_truncxfsi2"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(fix:SI (match_operand:XF 1 "s_register_operand" "f")))]
  "ENABLE_XF_PATTERNS && TARGET_HARD_FLOAT"
  "fix%?z\\t%0, %1"
[(set_attr "type" "f_2_r")])

;; Truncation insns

(define_insn "truncdfsf2"
  [(set (match_operand:SF 0 "s_register_operand" "=f")
	(float_truncate:SF
	 (match_operand:DF 1 "s_register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "mvf%?s\\t%0, %1"
[(set_attr "type" "ffarith")])

(define_insn "truncxfsf2"
  [(set (match_operand:SF 0 "s_register_operand" "=f")
	(float_truncate:SF
	 (match_operand:XF 1 "s_register_operand" "f")))]
  "ENABLE_XF_PATTERNS && TARGET_HARD_FLOAT"
  "mvf%?s\\t%0, %1"
[(set_attr "type" "ffarith")])

(define_insn "truncxfdf2"
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(float_truncate:DF
	 (match_operand:XF 1 "s_register_operand" "f")))]
  "ENABLE_XF_PATTERNS && TARGET_HARD_FLOAT"
  "mvf%?d\\t%0, %1"
[(set_attr "type" "ffarith")])

;; Zero and sign extension instructions.

(define_insn "zero_extendsidi2"
  [(set (match_operand:DI 0 "s_register_operand" "=r")
        (zero_extend:DI (match_operand:SI 1 "s_register_operand" "r")))]
  ""
  "*
  if (REGNO (operands[1]) != REGNO (operands[0]) + (WORDS_BIG_ENDIAN ? 1 : 0))
    output_asm_insn (\"mov%?\\t%Q0, %1\", operands);
  return \"mov%?\\t%R0, #0\";
"
[(set_attr "length" "8")])

(define_insn "zero_extendqidi2"
  [(set (match_operand:DI 0 "s_register_operand" "=r")
	(zero_extend:DI (match_operand:QI 1 "s_register_operand" "r")))]
  ""
  "and%?\\t%Q0, %1, #255\;mov%?\\t%R0, #0"
[(set_attr "length" "8")
 (set_attr "type" "*")])

(define_insn "extendsidi2"
  [(set (match_operand:DI 0 "s_register_operand" "=r")
        (sign_extend:DI (match_operand:SI 1 "s_register_operand" "r")))]
  ""
  "*
  if (REGNO (operands[1]) != REGNO (operands[0]) + (WORDS_BIG_ENDIAN ? 1 : 0))
    output_asm_insn (\"mov%?\\t%Q0, %1\", operands);
  return \"mov%?\\t%R0, %Q0, asr #31\";
"
[(set_attr "length" "8")])

(define_expand "zero_extendhisi2"
  [(set (match_dup 2) (ashift:SI (match_operand:HI 1 "nonimmediate_operand" "")
				 (const_int 16)))
   (set (match_operand:SI 0 "s_register_operand" "")
	(lshiftrt:SI (match_dup 2) (const_int 16)))]
  ""
  "
{
  if (arm_arch4 && GET_CODE (operands[1]) == MEM)
    {
     /* Note: We do not have to worry about TARGET_SHORT_BY_BYTES
	here because the insn below will generate an LDRH instruction
	rather than an LDR instruction, so we cannot get an unaligned
	word access.  */
      emit_insn (gen_rtx_SET (VOIDmode, operands[0],
			      gen_rtx_ZERO_EXTEND (SImode, operands[1])));
      DONE;
    }
  if (TARGET_SHORT_BY_BYTES && GET_CODE (operands[1]) == MEM)
    {
      emit_insn (gen_movhi_bytes (operands[0], operands[1]));
      DONE;
    }
  if (! s_register_operand (operands[1], HImode))
    operands[1] = copy_to_mode_reg (HImode, operands[1]);
  operands[1] = gen_lowpart (SImode, operands[1]);
  operands[2] = gen_reg_rtx (SImode); 
}")

(define_insn "*zero_extendhisi_insn"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(zero_extend:SI (match_operand:HI 1 "memory_operand" "m")))]
  "arm_arch4"
  "ldr%?h\\t%0, %1"
[(set_attr "type" "load")])

(define_split
  [(set (match_operand:SI 0 "s_register_operand" "")
	(zero_extend:SI (match_operand:HI 1 "alignable_memory_operand" "")))
   (clobber (match_operand:SI 2 "s_register_operand" ""))]
  "! arm_arch4"
  [(set (match_dup 2) (match_dup 1))
   (set (match_dup 0) (lshiftrt:SI (match_dup 2) (const_int 16)))]
  "
{
  if ((operands[1] = gen_rotated_half_load (operands[1])) == NULL)
    FAIL;
}")

(define_split
  [(set (match_operand:SI 0 "s_register_operand" "")
	(match_operator:SI 3 "shiftable_operator"
	 [(zero_extend:SI (match_operand:HI 1 "alignable_memory_operand" ""))
	  (match_operand:SI 4 "s_register_operand" "")]))
   (clobber (match_operand:SI 2 "s_register_operand" ""))]
  "! arm_arch4"
  [(set (match_dup 2) (match_dup 1))
   (set (match_dup 0)
	(match_op_dup 3
	 [(lshiftrt:SI (match_dup 2) (const_int 16)) (match_dup 4)]))]
  "
{
  if ((operands[1] = gen_rotated_half_load (operands[1])) == NULL)
    FAIL;
}")

(define_expand "zero_extendqisi2"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(zero_extend:SI
	 (match_operand:QI 1 "nonimmediate_operand" "r,m")))]
  ""
  "
  if (GET_CODE (operands[1]) != MEM)
    {
      emit_insn (gen_andsi3 (operands[0], gen_lowpart (SImode, operands[1]),
			     GEN_INT (255)));
      DONE;
    }
")

(define_insn "*load_extendqisi"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(zero_extend:SI (match_operand:QI 1 "memory_operand" "m")))]
  ""
  "ldr%?b\\t%0, %1\\t%@ zero_extendqisi2"
[(set_attr "type" "load")])

(define_split
  [(set (match_operand:SI 0 "s_register_operand" "")
	(zero_extend:SI (subreg:QI (match_operand:SI 1 "" "") 0)))
   (clobber (match_operand:SI 2 "s_register_operand" ""))]
  "GET_CODE (operands[1]) != MEM"
  [(set (match_dup 2) (match_dup 1))
   (set (match_dup 0) (and:SI (match_dup 2) (const_int 255)))]
  "")

(define_insn "*compareqi_eq0"
  [(set (reg:CC_Z 24)
	(compare:CC_Z (match_operand:QI 0 "s_register_operand" "r")
			 (const_int 0)))]
  ""
  "tst\\t%0, #255"
[(set_attr "conds" "set")])

(define_expand "extendhisi2"
  [(set (match_dup 2)
	(ashift:SI (match_operand:HI 1 "nonimmediate_operand" "")
		   (const_int 16)))
   (set (match_operand:SI 0 "s_register_operand" "")
	(ashiftrt:SI (match_dup 2)
		     (const_int 16)))]
  ""
  "
{
  if (arm_arch4 && GET_CODE (operands[1]) == MEM)
    {
     /* Note: We do not have to worry about TARGET_SHORT_BY_BYTES
	here because the insn below will generate an LDRH instruction
	rather than an LDR instruction, so we cannot get an unaligned
	word access.  */
      emit_insn (gen_rtx_SET (VOIDmode, operands[0],
		 gen_rtx_SIGN_EXTEND (SImode, operands[1])));
      DONE;
    }

  if (TARGET_SHORT_BY_BYTES && GET_CODE (operands[1]) == MEM)
    {
      emit_insn (gen_extendhisi2_mem (operands[0], operands[1]));
      DONE;
    }
  if (! s_register_operand (operands[1], HImode))
    operands[1] = copy_to_mode_reg (HImode, operands[1]);
  operands[1] = gen_lowpart (SImode, operands[1]);
  operands[2] = gen_reg_rtx (SImode);
}")

(define_expand "extendhisi2_mem"
  [(set (match_dup 2) (zero_extend:SI (match_operand:HI 1 "" "")))
   (set (match_dup 3)
	(zero_extend:SI (match_dup 7)))
   (set (match_dup 6) (ashift:SI (match_dup 4) (const_int 24)))
   (set (match_operand:SI 0 "" "")
	(ior:SI (ashiftrt:SI (match_dup 6) (const_int 16)) (match_dup 5)))]
  ""
  "
{
  rtx mem1, mem2;
  rtx addr = copy_to_mode_reg (SImode, XEXP (operands[1], 0));

  mem1 = gen_rtx_MEM (QImode, addr);
  MEM_COPY_ATTRIBUTES (mem1, operands[1]);
  RTX_UNCHANGING_P (mem1) = RTX_UNCHANGING_P (operands[1]);
  mem2 = gen_rtx_MEM (QImode, plus_constant (addr, 1));
  MEM_COPY_ATTRIBUTES (mem2, operands[1]);
  RTX_UNCHANGING_P (mem2) = RTX_UNCHANGING_P (operands[1]);
  operands[0] = gen_lowpart (SImode, operands[0]);
  operands[1] = mem1;
  operands[2] = gen_reg_rtx (SImode);
  operands[3] = gen_reg_rtx (SImode);
  operands[6] = gen_reg_rtx (SImode);
  operands[7] = mem2;

  if (BYTES_BIG_ENDIAN)
    {
      operands[4] = operands[2];
      operands[5] = operands[3];
    }
  else
    {
      operands[4] = operands[3];
      operands[5] = operands[2];
    }
}
")

(define_insn "*extendhisi_insn"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(sign_extend:SI (match_operand:HI 1 "memory_operand" "m")))]
  "arm_arch4"
  "ldr%?sh\\t%0, %1"
[(set_attr "type" "load")])

(define_split
  [(set (match_operand:SI 0 "s_register_operand" "")
	(sign_extend:SI (match_operand:HI 1 "alignable_memory_operand" "")))
   (clobber (match_operand:SI 2 "s_register_operand" ""))]
  "! arm_arch4"
  [(set (match_dup 2) (match_dup 1))
   (set (match_dup 0) (ashiftrt:SI (match_dup 2) (const_int 16)))]
  "
{
  if ((operands[1] = gen_rotated_half_load (operands[1])) == NULL)
    FAIL;
}")

(define_split
  [(set (match_operand:SI 0 "s_register_operand" "")
	(match_operator:SI 3 "shiftable_operator"
	 [(sign_extend:SI (match_operand:HI 1 "alignable_memory_operand" ""))
	  (match_operand:SI 4 "s_register_operand" "")]))
   (clobber (match_operand:SI 2 "s_register_operand" ""))]
  "! arm_arch4"
  [(set (match_dup 2) (match_dup 1))
   (set (match_dup 0)
	(match_op_dup 3
	 [(ashiftrt:SI (match_dup 2) (const_int 16)) (match_dup 4)]))]
  "
{
  if ((operands[1] = gen_rotated_half_load (operands[1])) == NULL)
    FAIL;
}")

(define_expand "extendqihi2"
  [(set (match_dup 2)
	(ashift:SI (match_operand:QI 1 "general_operand" "")
		   (const_int 24)))
   (set (match_operand:HI 0 "s_register_operand" "")
	(ashiftrt:SI (match_dup 2)
		     (const_int 24)))]
  ""
  "
{
  if (arm_arch4 && GET_CODE (operands[1]) == MEM)
    {
      emit_insn (gen_rtx_SET (VOIDmode,
			      operands[0],
			      gen_rtx_SIGN_EXTEND (HImode, operands[1])));
      DONE;
    }
  if (! s_register_operand (operands[1], QImode))
    operands[1] = copy_to_mode_reg (QImode, operands[1]);
  operands[0] = gen_lowpart (SImode, operands[0]);
  operands[1] = gen_lowpart (SImode, operands[1]);
  operands[2] = gen_reg_rtx (SImode);
}")

; Rather than restricting all byte accesses to memory addresses that ldrsb
; can handle, we fix up the ones that ldrsb can't grok with a split.
(define_insn "*extendqihi_insn"
  [(set (match_operand:HI 0 "s_register_operand" "=r")
	(sign_extend:HI (match_operand:QI 1 "memory_operand" "m")))]
  "arm_arch4"
  "*
  /* If the address is invalid, this will split the instruction into two. */
  if (bad_signed_byte_operand (operands[1], QImode))
    return \"#\";
  return \"ldr%?sb\\t%0, %1\";
"
[(set_attr "type" "load")
 (set_attr "length" "8")])

(define_split
  [(set (match_operand:HI 0 "s_register_operand" "")
	(sign_extend:HI (match_operand:QI 1 "bad_signed_byte_operand" "")))]
  "arm_arch4 && reload_completed"
  [(set (match_dup 3) (match_dup 1))
   (set (match_dup 0) (sign_extend:HI (match_dup 2)))]
  "
  {
    HOST_WIDE_INT offset;

    operands[3] = gen_rtx_REG (SImode, REGNO (operands[0]));
    operands[2] = gen_rtx_MEM (QImode, operands[3]);
    MEM_COPY_ATTRIBUTES (operands[2], operands[1]);
    RTX_UNCHANGING_P (operands[2]) = RTX_UNCHANGING_P (operands[1]);
    operands[1] = XEXP (operands[1], 0);
    if (GET_CODE (operands[1]) == PLUS
	&& GET_CODE (XEXP (operands[1], 1)) == CONST_INT
	&& ! (const_ok_for_arm (offset = INTVAL (XEXP (operands[1], 1)))
	      || const_ok_for_arm (-offset)))
      {
	HOST_WIDE_INT low = (offset > 0
			     ? (offset & 0xff) : -((-offset) & 0xff));
	XEXP (operands[2], 0) = plus_constant (operands[3], low);
	operands[1] = plus_constant (XEXP (operands[1], 0), offset - low);
      }
    /* Ensure the sum is in correct canonical form */
    else if (GET_CODE (operands[1]) == PLUS
	     && GET_CODE (XEXP (operands[1], 1)) != CONST_INT
	     && ! s_register_operand (XEXP (operands[1], 1), VOIDmode))
      operands[1] = gen_rtx_PLUS (GET_MODE (operands[1]),
					   XEXP (operands[1], 1),
					   XEXP (operands[1], 0));
  }
")

(define_expand "extendqisi2"
  [(set (match_dup 2)
	(ashift:SI (match_operand:QI 1 "general_operand" "")
		   (const_int 24)))
   (set (match_operand:SI 0 "s_register_operand" "")
	(ashiftrt:SI (match_dup 2)
		     (const_int 24)))]
  ""
  "
{
  if (arm_arch4 && GET_CODE (operands[1]) == MEM)
    {
      emit_insn (gen_rtx_SET (VOIDmode,
			      operands[0],
			      gen_rtx_SIGN_EXTEND (SImode, operands[1])));
      DONE;
    }
  if (! s_register_operand (operands[1], QImode))
    operands[1] = copy_to_mode_reg (QImode, operands[1]);
  operands[1] = gen_lowpart (SImode, operands[1]);
  operands[2] = gen_reg_rtx (SImode);
}")

; Rather than restricting all byte accesses to memory addresses that ldrsb
; can handle, we fix up the ones that ldrsb can't grok with a split.
(define_insn "*extendqisi_insn"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(sign_extend:SI (match_operand:QI 1 "memory_operand" "m")))]
  "arm_arch4"
  "*
  /* If the address is invalid, this will split the instruction into two. */
  if (bad_signed_byte_operand (operands[1], QImode))
    return \"#\";
  return \"ldr%?sb\\t%0, %1\";
"
[(set_attr "type" "load")
 (set_attr "length" "8")])

(define_split
  [(set (match_operand:SI 0 "s_register_operand" "")
	(sign_extend:SI (match_operand:QI 1 "bad_signed_byte_operand" "")))]
  "arm_arch4 && reload_completed"
  [(set (match_dup 0) (match_dup 1))
   (set (match_dup 0) (sign_extend:SI (match_dup 2)))]
  "
  {
    HOST_WIDE_INT offset;

    operands[2] = gen_rtx_MEM (QImode, operands[0]);
    MEM_COPY_ATTRIBUTES (operands[2], operands[1]);
    RTX_UNCHANGING_P (operands[2]) = RTX_UNCHANGING_P (operands[1]);
    operands[1] = XEXP (operands[1], 0);
    if (GET_CODE (operands[1]) == PLUS
	&& GET_CODE (XEXP (operands[1], 1)) == CONST_INT
	&& ! (const_ok_for_arm (offset = INTVAL (XEXP (operands[1], 1)))
	      || const_ok_for_arm (-offset)))
      {
	HOST_WIDE_INT low = (offset > 0
			     ? (offset & 0xff) : -((-offset) & 0xff));
	XEXP (operands[2], 0) = plus_constant (operands[0], low);
	operands[1] = plus_constant (XEXP (operands[1], 0), offset - low);
      }
    /* Ensure the sum is in correct canonical form */
    else if (GET_CODE (operands[1]) == PLUS
	     && GET_CODE (XEXP (operands[1], 1)) != CONST_INT
	     && ! s_register_operand (XEXP (operands[1], 1), VOIDmode))
      operands[1] = gen_rtx_PLUS (GET_MODE (operands[1]),
					   XEXP (operands[1], 1),
					   XEXP (operands[1], 0));
  }
")

(define_insn "extendsfdf2"
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(float_extend:DF (match_operand:SF 1 "s_register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "mvf%?d\\t%0, %1"
[(set_attr "type" "ffarith")])

(define_insn "extendsfxf2"
  [(set (match_operand:XF 0 "s_register_operand" "=f")
	(float_extend:XF (match_operand:SF 1 "s_register_operand" "f")))]
  "ENABLE_XF_PATTERNS && TARGET_HARD_FLOAT"
  "mvf%?e\\t%0, %1"
[(set_attr "type" "ffarith")])

(define_insn "extenddfxf2"
  [(set (match_operand:XF 0 "s_register_operand" "=f")
	(float_extend:XF (match_operand:DF 1 "s_register_operand" "f")))]
  "ENABLE_XF_PATTERNS && TARGET_HARD_FLOAT"
  "mvf%?e\\t%0, %1"
[(set_attr "type" "ffarith")])


;; Move insns (including loads and stores)

;; XXX Just some ideas about movti.
;; I don't think these are a good idea on the arm, there just aren't enough
;; registers
;;(define_expand "loadti"
;;  [(set (match_operand:TI 0 "s_register_operand" "")
;;	(mem:TI (match_operand:SI 1 "address_operand" "")))]
;;  "" "")

;;(define_expand "storeti"
;;  [(set (mem:TI (match_operand:TI 0 "address_operand" ""))
;;	(match_operand:TI 1 "s_register_operand" ""))]
;;  "" "")

;;(define_expand "movti"
;;  [(set (match_operand:TI 0 "general_operand" "")
;;	(match_operand:TI 1 "general_operand" ""))]
;;  ""
;;  "
;;{
;;  rtx insn;
;;
;;  if (GET_CODE (operands[0]) == MEM && GET_CODE (operands[1]) == MEM)
;;    operands[1] = copy_to_reg (operands[1]);
;;  if (GET_CODE (operands[0]) == MEM)
;;    insn = gen_storeti (XEXP (operands[0], 0), operands[1]);
;;  else if (GET_CODE (operands[1]) == MEM)
;;    insn = gen_loadti (operands[0], XEXP (operands[1], 0));
;;  else
;;    FAIL;
;;
;;  emit_insn (insn);
;;  DONE;
;;}")

;; Recognise garbage generated above.

;;(define_insn ""
;;  [(set (match_operand:TI 0 "general_operand" "=r,r,r,<,>,m")
;;	(match_operand:TI 1 "general_operand" "<,>,m,r,r,r"))]
;;  ""
;;  "*
;;  {
;;    register mem = (which_alternative < 3);
;;    register char *template;
;;
;;    operands[mem] = XEXP (operands[mem], 0);
;;    switch (which_alternative)
;;      {
;;      case 0: template = \"ldmdb\\t%1!, %M0\"; break;
;;      case 1: template = \"ldmia\\t%1!, %M0\"; break;
;;      case 2: template = \"ldmia\\t%1, %M0\"; break;
;;      case 3: template = \"stmdb\\t%0!, %M1\"; break;
;;      case 4: template = \"stmia\\t%0!, %M1\"; break;
;;      case 5: template = \"stmia\\t%0, %M1\"; break;
;;      }
;;    output_asm_insn (template, operands);
;;    return \"\";
;;  }")


(define_insn "movdi"
  [(set (match_operand:DI 0 "di_operand" "=r,r,o<>")
	(match_operand:DI 1 "di_operand" "rIK,mi,r"))]
  ""
  "*
  return (output_move_double (operands));
"
[(set_attr "length" "8,8,8")
 (set_attr "type" "*,load,store2")])

(define_expand "movsi"
  [(set (match_operand:SI 0 "general_operand" "")
        (match_operand:SI 1 "general_operand" ""))]
  ""
  "
  /* Everything except mem = const or mem = mem can be done easily */
  if (GET_CODE (operands[0]) == MEM)
    operands[1] = force_reg (SImode, operands[1]);
  if (GET_CODE (operands[1]) == CONST_INT
      && !(const_ok_for_arm (INTVAL (operands[1]))
           || const_ok_for_arm (~INTVAL (operands[1]))))
    {
      arm_split_constant (SET, SImode, INTVAL (operands[1]), operands[0],
			  NULL_RTX,
			  (reload_in_progress || reload_completed ? 0
			   : preserve_subexpressions_p ()));
      DONE;
    }
  if (CONSTANT_P (operands[1]) && flag_pic)
    operands[1] = legitimize_pic_address (operands[1], SImode,
					  ((reload_in_progress
					    || reload_completed)
					   ? operands[0] : 0));
")

(define_insn "*movsi_insn"
  [(set (match_operand:SI 0 "general_operand" "=r,r,r,m")
	(match_operand:SI 1 "general_operand" "rI,K,mi,r"))]
  "register_operand (operands[0], SImode)
   || register_operand (operands[1], SImode)"
  "@
   mov%?\\t%0, %1
   mvn%?\\t%0, #%B1
   ldr%?\\t%0, %1
   str%?\\t%1, %0"
[(set_attr "type" "*,*,load,store1")])

(define_split
  [(set (match_operand:SI 0 "s_register_operand" "")
	(match_operand:SI 1 "const_int_operand" ""))]
  "! (const_ok_for_arm (INTVAL (operands[1]))
      || const_ok_for_arm (~INTVAL (operands[1])))"
  [(clobber (const_int 0))]
  "
  arm_split_constant (SET, SImode, INTVAL (operands[1]), operands[0],
		      NULL_RTX, 0);
  DONE;
")

(define_expand "movaddr"
  [(set (match_operand:SI 0 "s_register_operand" "")
	(match_operand:DI 1 "address_operand" ""))]
  ""
  "")

(define_insn "*movaddr_insn"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(match_operand:DI 1 "address_operand" "p"))]
  "reload_completed
   && (GET_CODE (operands[1]) == LABEL_REF
       || (GET_CODE (operands[1]) == CONST
	   && GET_CODE (XEXP (operands[1], 0)) == PLUS
	   && GET_CODE (XEXP (XEXP (operands[1], 0), 0)) == LABEL_REF
	   && GET_CODE (XEXP (XEXP (operands[1], 0), 1)) == CONST_INT))"
  "adr%?\\t%0, %a1")

/* When generating pic, we need to load the symbol offset into a register.
   So that the optimizer does not confuse this with a normal symbol load
   we use an unspec.  The offset will be loaded from a constant pool entry,
   since that is the only type of relocation we can use.  */

(define_insn "pic_load_addr"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(unspec:SI [(match_operand 1 "" "")] 3))]
  "flag_pic"
  "ldr%?\\t%0, %a1"
 [(set_attr "type" "load")])

;; This variant is used for AOF assembly, since it needs to mention the
;; pic register in the rtl.
(define_expand "pic_load_addr_based"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(unspec:SI [(match_operand 1 "" "") (match_dup 2)] 3))]
  "flag_pic"
  "operands[2] = pic_offset_table_rtx;")

(define_insn "*pic_load_addr_based_insn"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(unspec:SI [(match_operand 1 "" "")
		    (match_operand 2 "s_register_operand" "r")] 3))]
  "flag_pic && operands[2] == pic_offset_table_rtx"
  "*
#ifdef AOF_ASSEMBLER
  operands[1] = aof_pic_entry (operands[1]);
#endif
  output_asm_insn (\"ldr%?\\t%0, %a1\", operands);
  return \"\";
" [(set_attr "type" "load")])

(define_insn "pic_add_dot_plus_eight"
  [(set (match_operand 0 "register_operand" "+r")
	(plus:SI (match_dup 0) (const (plus:SI (pc) (const_int 8)))))
   (use (label_ref (match_operand 1 "" "")))]
  "flag_pic"
  "*
  ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, \"L\",
			     CODE_LABEL_NUMBER (operands[1]));
  return \"add%?\\t%0, %|pc, %0\";
")

;; If copying one reg to another we can set the condition codes according to
;; its value.  Such a move is common after a return from subroutine and the
;; result is being tested against zero.

(define_insn "*movsi_compare0"
  [(set (reg:CC 24) (compare:CC (match_operand:SI 1 "s_register_operand" "0,r")
			        (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=r,r") (match_dup 1))]
  ""
  "@
   cmp%?\\t%0, #0
   sub%?s\\t%0, %1, #0"
[(set_attr "conds" "set")])

;; Subroutine to store a half word from a register into memory.
;; Operand 0 is the source register (HImode)
;; Operand 1 is the destination address in a register (SImode)

;; In both this routine and the next, we must be careful not to spill
;; a memory address of reg+large_const into a separate PLUS insn, since this
;; can generate unrecognizable rtl.

(define_expand "storehi"
  [;; store the low byte
   (set (match_operand 1 "" "") (match_dup 3))
   ;; extract the high byte
   (set (match_dup 2)
	(ashiftrt:SI (match_operand 0 "" "") (const_int 8)))
   ;; store the high byte
   (set (match_dup 4) (subreg:QI (match_dup 2) 0))]	;explicit subreg safe
  ""
  "
{
  rtx addr = XEXP (operands[1], 0);
  enum rtx_code code = GET_CODE (addr);

  if ((code == PLUS && GET_CODE (XEXP (addr, 1)) != CONST_INT)
      || code == MINUS)
    addr = force_reg (SImode, addr);

  operands[4] = change_address (operands[1], QImode, plus_constant (addr, 1));
  operands[1] = change_address (operands[1], QImode, NULL_RTX);
  operands[3] = gen_lowpart (QImode, operands[0]);
  operands[0] = gen_lowpart (SImode, operands[0]);
  operands[2] = gen_reg_rtx (SImode); 
}
")

(define_expand "storehi_bigend"
  [(set (match_dup 4) (match_dup 3))
   (set (match_dup 2)
	(ashiftrt:SI (match_operand 0 "" "") (const_int 8)))
   (set (match_operand 1 "" "")	(subreg:QI (match_dup 2) 0))]
  ""
  "
{
  rtx addr = XEXP (operands[1], 0);
  enum rtx_code code = GET_CODE (addr);

  if ((code == PLUS && GET_CODE (XEXP (addr, 1)) != CONST_INT)
      || code == MINUS)
    addr = force_reg (SImode, addr);

  operands[4] = change_address (operands[1], QImode, plus_constant (addr, 1));
  operands[1] = change_address (operands[1], QImode, NULL_RTX);
  operands[3] = gen_lowpart (QImode, operands[0]);
  operands[0] = gen_lowpart (SImode, operands[0]);
  operands[2] = gen_reg_rtx (SImode);
}
")

;; Subroutine to store a half word integer constant into memory.
(define_expand "storeinthi"
  [(set (match_operand 0 "" "")
	(subreg:QI (match_operand 1 "" "") 0))
   (set (match_dup 3) (subreg:QI (match_dup 2) 0))]
  ""
  "
{
  HOST_WIDE_INT value = INTVAL (operands[1]);
  rtx addr = XEXP (operands[0], 0);
  enum rtx_code code = GET_CODE (addr);

  if ((code == PLUS && GET_CODE (XEXP (addr, 1)) != CONST_INT)
      || code == MINUS)
    addr = force_reg (SImode, addr);

  operands[1] = gen_reg_rtx (SImode);
  if (BYTES_BIG_ENDIAN)
    {
      emit_insn (gen_movsi (operands[1], GEN_INT ((value >> 8) & 255)));
      if ((value & 255) == ((value >> 8) & 255))
	operands[2] = operands[1];
      else
	{
	  operands[2] = gen_reg_rtx (SImode);
	  emit_insn (gen_movsi (operands[2], GEN_INT (value & 255)));
	}
    }
  else
    {
      emit_insn (gen_movsi (operands[1], GEN_INT (value & 255)));
      if ((value & 255) == ((value >> 8) & 255))
	operands[2] = operands[1];
      else
	{
	  operands[2] = gen_reg_rtx (SImode);
	  emit_insn (gen_movsi (operands[2], GEN_INT ((value >> 8) & 255)));
	}
    }

  operands[3] = change_address (operands[0], QImode, plus_constant (addr, 1));
  operands[0] = change_address (operands[0], QImode, NULL_RTX);
}
")

(define_expand "storehi_single_op"
  [(set (match_operand:HI 0 "memory_operand" "")
	(match_operand:HI 1 "general_operand" ""))]
  "arm_arch4"
  "
  if (! s_register_operand (operands[1], HImode))
    operands[1] = copy_to_mode_reg (HImode, operands[1]);
")

(define_expand "movhi"
  [(set (match_operand:HI 0 "general_operand" "")
	(match_operand:HI 1 "general_operand" ""))]
  ""
  "
{
  if (! (reload_in_progress || reload_completed))
    {
      if (GET_CODE (operands[0]) == MEM)
	{
	  if (arm_arch4)
	    {
	      emit_insn (gen_storehi_single_op (operands[0], operands[1]));
	      DONE;
	    }
	  if (GET_CODE (operands[1]) == CONST_INT)
	    emit_insn (gen_storeinthi (operands[0], operands[1]));
	  else
	    {
	      if (GET_CODE (operands[1]) == MEM)
		operands[1] = force_reg (HImode, operands[1]);
	      if (BYTES_BIG_ENDIAN)
		emit_insn (gen_storehi_bigend (operands[1], operands[0]));
	      else
		emit_insn (gen_storehi (operands[1], operands[0]));
	    }
	  DONE;
	}
      /* Sign extend a constant, and keep it in an SImode reg.  */
      else if (GET_CODE (operands[1]) == CONST_INT)
	{
	  rtx reg = gen_reg_rtx (SImode);
	  HOST_WIDE_INT val = INTVAL (operands[1]) & 0xffff;

	  /* If the constant is already valid, leave it alone.  */
	  if (! const_ok_for_arm (val))
	    {
	      /* If setting all the top bits will make the constant 
		 loadable in a single instruction, then set them.  
		 Otherwise, sign extend the number.  */

	      if (const_ok_for_arm (~ (val | ~0xffff)))
		val |= ~0xffff;
	      else if (val & 0x8000)
		val |= ~0xffff;
	    }

	  emit_insn (gen_movsi (reg, GEN_INT (val)));
	  operands[1] = gen_rtx_SUBREG (HImode, reg, 0);
	}
      else if (! arm_arch4)
	{
	 /* Note: We do not have to worry about TARGET_SHORT_BY_BYTES
	    for v4 and up architectures because LDRH instructions will
	    be used to access the HI values, and these cannot generate
	    unaligned word access faults in the MMU.  */
	  if (GET_CODE (operands[1]) == MEM)
	    {
	      if (TARGET_SHORT_BY_BYTES)
		{
		  rtx base;
		  rtx offset = const0_rtx;
		  rtx reg = gen_reg_rtx (SImode);

		  if ((GET_CODE (base = XEXP (operands[1], 0)) == REG
		       || (GET_CODE (base) == PLUS
			   && GET_CODE (offset = XEXP (base, 1)) == CONST_INT
                           && ((INTVAL(offset) & 1) != 1)
			   && GET_CODE (base = XEXP (base, 0)) == REG))
		      && REGNO_POINTER_ALIGN (REGNO (base)) >= 4)
		    {
		      HOST_WIDE_INT new_offset = INTVAL (offset) & ~3;
		      rtx new;

		      new = gen_rtx_MEM (SImode,
					 plus_constant (base, new_offset));
	              MEM_COPY_ATTRIBUTES (new, operands[1]);
		      RTX_UNCHANGING_P (new) = RTX_UNCHANGING_P (operands[1]);
		      emit_insn (gen_movsi (reg, new));
		      if (((INTVAL (offset) & 2) != 0)
			  ^ (BYTES_BIG_ENDIAN ? 1 : 0))
			{
			  rtx reg2 = gen_reg_rtx (SImode);

			  emit_insn (gen_lshrsi3 (reg2, reg, GEN_INT (16)));
			  reg = reg2;
			}
		    }
		  else
		    emit_insn (gen_movhi_bytes (reg, operands[1]));

		  operands[1] = gen_lowpart (HImode, reg);
		}
	      else if (BYTES_BIG_ENDIAN)
		{
		  rtx base;
		  rtx offset = const0_rtx;

		  if ((GET_CODE (base = XEXP (operands[1], 0)) == REG
		       || (GET_CODE (base) == PLUS
			   && GET_CODE (offset = XEXP (base, 1)) == CONST_INT
			   && GET_CODE (base = XEXP (base, 0)) == REG))
		      && REGNO_POINTER_ALIGN (REGNO (base)) >= 4)
		    {
		      rtx reg = gen_reg_rtx (SImode);
		      rtx new;

		      if ((INTVAL (offset) & 2) == 2)
			{
			  HOST_WIDE_INT new_offset = INTVAL (offset) ^ 2;
			  new = gen_rtx_MEM (SImode,
					     plus_constant (base, new_offset));
                          MEM_COPY_ATTRIBUTES (new, operands[1]);
			  RTX_UNCHANGING_P (new) = RTX_UNCHANGING_P (operands[1]);
			  emit_insn (gen_movsi (reg, new));
			}
		      else
			{
			  new = gen_rtx_MEM (SImode, XEXP (operands[1], 0));
	                  MEM_COPY_ATTRIBUTES (new, operands[1]);
			  RTX_UNCHANGING_P (new)
			    = RTX_UNCHANGING_P (operands[1]);
			  emit_insn (gen_rotated_loadsi (reg, new));
			}

		      operands[1] = gen_lowpart (HImode, reg);
		    }
		  else
		    {
		      emit_insn (gen_movhi_bigend (operands[0], operands[1]));
		      DONE;
		    }
		}
	    }
	}
    }
  /* Handle loading a large integer during reload */
  else if (GET_CODE (operands[1]) == CONST_INT
	   && ! const_ok_for_arm (INTVAL (operands[1]))
	   && ! const_ok_for_arm (~INTVAL (operands[1])))
    {
      /* Writing a constant to memory needs a scratch, which should
	 be handled with SECONDARY_RELOADs.  */
      if (GET_CODE (operands[0]) != REG)
	abort ();

      operands[0] = gen_rtx_SUBREG (SImode, operands[0], 0);
      emit_insn (gen_movsi (operands[0], operands[1]));
      DONE;
    }
}
")

(define_insn "rotated_loadsi"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(rotate:SI (match_operand:SI 1 "offsettable_memory_operand" "o")
		   (const_int 16)))]
  "! TARGET_SHORT_BY_BYTES"
  "*
{
  rtx ops[2];

  ops[0] = operands[0];
  ops[1] = gen_rtx_MEM (SImode, plus_constant (XEXP (operands[1], 0), 2));
  output_asm_insn (\"ldr%?\\t%0, %1\\t%@ load-rotate\", ops);
  return \"\";
}"
[(set_attr "type" "load")])

(define_expand "movhi_bytes"
  [(set (match_dup 2) (zero_extend:SI (match_operand:HI 1 "" "")))
   (set (match_dup 3)
	(zero_extend:SI (match_dup 6)))
   (set (match_operand:SI 0 "" "")
	 (ior:SI (ashift:SI (match_dup 4) (const_int 8)) (match_dup 5)))]
  ""
  "
{
  rtx mem1, mem2;
  rtx addr = copy_to_mode_reg (SImode, XEXP (operands[1], 0));

  mem1 = gen_rtx_MEM (QImode, addr);
  MEM_COPY_ATTRIBUTES (mem1, operands[1]);
  RTX_UNCHANGING_P (mem1) = RTX_UNCHANGING_P (operands[1]);
  mem2 = gen_rtx_MEM (QImode, plus_constant (addr, 1));
  MEM_COPY_ATTRIBUTES (mem2, operands[1]);
  RTX_UNCHANGING_P (mem2) = RTX_UNCHANGING_P (operands[1]);
  operands[0] = gen_lowpart (SImode, operands[0]);
  operands[1] = mem1;
  operands[2] = gen_reg_rtx (SImode);
  operands[3] = gen_reg_rtx (SImode);
  operands[6] = mem2;

  if (BYTES_BIG_ENDIAN)
    {
      operands[4] = operands[2];
      operands[5] = operands[3];
    }
  else
    {
      operands[4] = operands[3];
      operands[5] = operands[2];
    }
}
")

(define_expand "movhi_bigend"
  [(set (match_dup 2)
	(rotate:SI (subreg:SI (match_operand:HI 1 "memory_operand" "") 0)
		   (const_int 16)))
   (set (match_dup 3)
	(ashiftrt:SI (match_dup 2) (const_int 16)))
   (set (match_operand:HI 0 "s_register_operand" "")
	(subreg:HI (match_dup 3) 0))]
  ""
  "
  operands[2] = gen_reg_rtx (SImode);
  operands[3] = gen_reg_rtx (SImode);
")

;; Pattern to recognise insn generated default case above
(define_insn "*movhi_insn_arch4"
  [(set (match_operand:HI 0 "general_operand" "=r,r,r,m")
	(match_operand:HI 1 "general_operand"  "rI,K,m,r"))]
  "arm_arch4
   && (GET_CODE (operands[1]) != CONST_INT
       || const_ok_for_arm (INTVAL (operands[1]))
       || const_ok_for_arm (~INTVAL (operands[1])))"
  "@
   mov%?\\t%0, %1\\t%@ movhi
   mvn%?\\t%0, #%B1\\t%@ movhi
   ldr%?h\\t%0, %1\\t%@ movhi
   str%?h\\t%1, %0\\t%@ movhi"
[(set_attr "type" "*,*,load,store1")])

(define_insn "*movhi_insn_littleend"
  [(set (match_operand:HI 0 "s_register_operand" "=r,r,r")
	(match_operand:HI 1 "general_operand"  "rI,K,m"))]
  "! arm_arch4
   && ! BYTES_BIG_ENDIAN
   && ! TARGET_SHORT_BY_BYTES
   && (GET_CODE (operands[1]) != CONST_INT
       || const_ok_for_arm (INTVAL (operands[1]))
       || const_ok_for_arm (~INTVAL (operands[1])))"
  "@
   mov%?\\t%0, %1\\t%@ movhi
   mvn%?\\t%0, #%B1\\t%@ movhi
   ldr%?\\t%0, %1\\t%@ movhi"
[(set_attr "type" "*,*,load")])

(define_insn "*movhi_insn_bigend"
  [(set (match_operand:HI 0 "s_register_operand" "=r,r,r")
	(match_operand:HI 1 "general_operand"  "rI,K,m"))]
  "! arm_arch4
   && BYTES_BIG_ENDIAN
   && ! TARGET_SHORT_BY_BYTES
   && (GET_CODE (operands[1]) != CONST_INT
       || const_ok_for_arm (INTVAL (operands[1]))
       || const_ok_for_arm (~INTVAL (operands[1])))"
  "@
   mov%?\\t%0, %1\\t%@ movhi
   mvn%?\\t%0, #%B1\\t%@ movhi
   ldr%?\\t%0, %1\\t%@ movhi_bigend\;mov%?\\t%0, %0, asr #16"
[(set_attr "type" "*,*,load")
 (set_attr "length" "4,4,8")])

(define_insn "*loadhi_si_bigend"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(rotate:SI (subreg:SI (match_operand:HI 1 "memory_operand" "m") 0)
		   (const_int 16)))]
  "BYTES_BIG_ENDIAN
   && ! TARGET_SHORT_BY_BYTES"
  "ldr%?\\t%0, %1\\t%@ movhi_bigend"
[(set_attr "type" "load")])

(define_insn "*movhi_bytes"
  [(set (match_operand:HI 0 "s_register_operand" "=r,r")
	(match_operand:HI 1 "arm_rhs_operand"  "rI,K"))]
  "TARGET_SHORT_BY_BYTES"
  "@
   mov%?\\t%0, %1\\t%@ movhi
   mvn%?\\t%0, #%B1\\t%@ movhi")

;; We use a DImode scratch because we may occasionally need an additional
;; temporary if the address isn't offsettable -- push_reload doesn't seem
;; to take any notice of the "o" constraints on reload_memory_operand operand.
(define_expand "reload_outhi"
  [(parallel [(match_operand:HI 0 "reload_memory_operand" "=o")
	      (match_operand:HI 1 "s_register_operand" "r")
	      (match_operand:DI 2 "s_register_operand" "=&r")])]
  ""
  "
  arm_reload_out_hi (operands);
  DONE;
")

(define_expand "reload_inhi"
  [(parallel [(match_operand:HI 0 "s_register_operand" "=r")
	      (match_operand:HI 1 "reload_memory_operand" "o")
	      (match_operand:DI 2 "s_register_operand" "=&r")])]
  "TARGET_SHORT_BY_BYTES"
  "
  arm_reload_in_hi (operands);
  DONE;
")

(define_expand "movqi"
  [(set (match_operand:QI 0 "general_operand" "")
        (match_operand:QI 1 "general_operand" ""))]
  ""
  "
  /* Everything except mem = const or mem = mem can be done easily */

  if (!(reload_in_progress || reload_completed))
    {
      if (GET_CODE (operands[1]) == CONST_INT)
	{
	  rtx reg = gen_reg_rtx (SImode);

	  emit_insn (gen_movsi (reg, operands[1]));
	  operands[1] = gen_rtx_SUBREG (QImode, reg, 0);
	}
      if (GET_CODE (operands[0]) == MEM)
	operands[1] = force_reg (QImode, operands[1]);
    }
")


(define_insn "*movqi_insn"
  [(set (match_operand:QI 0 "general_operand" "=r,r,r,m")
	(match_operand:QI 1 "general_operand" "rI,K,m,r"))]
  "register_operand (operands[0], QImode)
   || register_operand (operands[1], QImode)"
  "@
   mov%?\\t%0, %1
   mvn%?\\t%0, #%B1
   ldr%?b\\t%0, %1
   str%?b\\t%1, %0"
[(set_attr "type" "*,*,load,store1")])

(define_expand "movsf"
  [(set (match_operand:SF 0 "general_operand" "")
	(match_operand:SF 1 "general_operand" ""))]
  ""
  "
  if (GET_CODE (operands[0]) == MEM)
    operands[1] = force_reg (SFmode, operands[1]);
")

(define_insn "*movsf_hard_insn"
  [(set (match_operand:SF 0 "general_operand" "=f,f,f,m,f,r,r,r,m")
	(match_operand:SF 1 "general_operand" "fG,H,mE,f,r,f,r,mE,r"))]
  "TARGET_HARD_FLOAT
   && (GET_CODE (operands[0]) != MEM || register_operand (operands[1], SFmode))"
  "@
   mvf%?s\\t%0, %1
   mnf%?s\\t%0, #%N1
   ldf%?s\\t%0, %1
   stf%?s\\t%1, %0
   str%?\\t%1, [%|sp, #-4]!\;ldf%?s\\t%0, [%|sp], #4
   stf%?s\\t%1, [%|sp, #-4]!\;ldr%?\\t%0, [%|sp], #4
   mov%?\\t%0, %1
   ldr%?\\t%0, %1\\t%@ float
   str%?\\t%1, %0\\t%@ float"
[(set_attr "length" "4,4,4,4,8,8,4,4,4")
 (set_attr "type"
	 "ffarith,ffarith,f_load,f_store,r_mem_f,f_mem_r,*,load,store1")])

;; Exactly the same as above, except that all `f' cases are deleted.
;; This is necessary to prevent reload from ever trying to use a `f' reg
;; when -msoft-float.

(define_insn "*movsf_soft_insn"
  [(set (match_operand:SF 0 "general_operand" "=r,r,m")
	(match_operand:SF 1 "general_operand" "r,mE,r"))]
  "TARGET_SOFT_FLOAT
   && (GET_CODE (operands[0]) != MEM || register_operand (operands[1], SFmode))"
  "@
   mov%?\\t%0, %1
   ldr%?\\t%0, %1\\t%@ float
   str%?\\t%1, %0\\t%@ float"
[(set_attr "length" "4,4,4")
 (set_attr "type" "*,load,store1")])

(define_expand "movdf"
  [(set (match_operand:DF 0 "general_operand" "")
	(match_operand:DF 1 "general_operand" ""))]
  ""
  "
  if (GET_CODE (operands[0]) == MEM)
    operands[1] = force_reg (DFmode, operands[1]);
")

;; Reloading a df mode value stored in integer regs to memory can require a
;; scratch reg.
(define_expand "reload_outdf"
  [(match_operand:DF 0 "reload_memory_operand" "=o")
   (match_operand:DF 1 "s_register_operand" "r")
   (match_operand:SI 2 "s_register_operand" "=&r")]
  ""
  "
{
  enum rtx_code code = GET_CODE (XEXP (operands[0], 0));

  if (code == REG)
    operands[2] = XEXP (operands[0], 0);
  else if (code == POST_INC || code == PRE_DEC)
    {
      operands[0] = gen_rtx_SUBREG (DImode, operands[0], 0);
      operands[1] = gen_rtx_SUBREG (DImode, operands[1], 0);
      emit_insn (gen_movdi (operands[0], operands[1]));
      DONE;
    }
  else if (code == PRE_INC)
    {
      rtx reg = XEXP (XEXP (operands[0], 0), 0);
      emit_insn (gen_addsi3 (reg, reg, GEN_INT (8)));
      operands[2] = reg;
    }
  else if (code == POST_DEC)
    operands[2] = XEXP (XEXP (operands[0], 0), 0);
  else
    emit_insn (gen_addsi3 (operands[2], XEXP (XEXP (operands[0], 0), 0),
			   XEXP (XEXP (operands[0], 0), 1)));

  emit_insn (gen_rtx_SET (VOIDmode, gen_rtx_MEM (DFmode, operands[2]),
				    operands[1]));

  if (code == POST_DEC)
    emit_insn (gen_addsi3 (operands[2], operands[2], GEN_INT (-8)));

  DONE;
}
")

(define_insn "*movdf_hard_insn"
  [(set (match_operand:DF 0 "general_operand" "=r,Q,r,m,r,f,f,f,m,!f,!r")
	(match_operand:DF 1 "general_operand" "Q,r,r,r,mF,fG,H,mF,f,r,f"))]
  "TARGET_HARD_FLOAT
   && (GET_CODE (operands[0]) != MEM
       || register_operand (operands[1], DFmode))"
  "*
{
  switch (which_alternative)
    {
    default:
    case 0: return \"ldm%?ia\\t%m1, %M0\\t%@ double\";
    case 1: return \"stm%?ia\\t%m0, %M1\\t%@ double\";
    case 2: case 3: case 4: return output_move_double (operands);
    case 5: return \"mvf%?d\\t%0, %1\";
    case 6: return \"mnf%?d\\t%0, #%N1\";
    case 7: return \"ldf%?d\\t%0, %1\";
    case 8: return \"stf%?d\\t%1, %0\";
    case 9: return output_mov_double_fpu_from_arm (operands);
    case 10: return output_mov_double_arm_from_fpu (operands);
    }
}
"
[(set_attr "length" "4,4,8,8,8,4,4,4,4,8,8")
 (set_attr "type"
"load,store2,*,store2,load,ffarith,ffarith,f_load,f_store,r_mem_f,f_mem_r")])

;; Software floating point version.  This is essentially the same as movdi.
;; Do not use `f' as a constraint to prevent reload from ever trying to use
;; an `f' reg.

(define_insn "*movdf_soft_insn"
  [(set (match_operand:DF 0 "soft_df_operand" "=r,r,m")
	(match_operand:DF 1 "soft_df_operand" "r,mF,r"))]
  "TARGET_SOFT_FLOAT"
  "* return output_move_double (operands);"
[(set_attr "length" "8,8,8")
 (set_attr "type" "*,load,store2")])

(define_expand "movxf"
  [(set (match_operand:XF 0 "general_operand" "")
	(match_operand:XF 1 "general_operand" ""))]
  "ENABLE_XF_PATTERNS && TARGET_HARD_FLOAT"
  "")

;; Even when the XFmode patterns aren't enabled, we enable this after
;; reloading so that we can push floating point registers in the prologue.

(define_insn "*movxf_hard_insn"
  [(set (match_operand:XF 0 "general_operand" "=f,f,f,m,f,r,r")
	(match_operand:XF 1 "general_operand" "fG,H,m,f,r,f,r"))]
  "TARGET_HARD_FLOAT && (ENABLE_XF_PATTERNS || reload_completed)"
  "*
  switch (which_alternative)
    {
    default:
    case 0: return \"mvf%?e\\t%0, %1\";
    case 1: return \"mnf%?e\\t%0, #%N1\";
    case 2: return \"ldf%?e\\t%0, %1\";
    case 3: return \"stf%?e\\t%1, %0\";
    case 4: return output_mov_long_double_fpu_from_arm (operands);
    case 5: return output_mov_long_double_arm_from_fpu (operands);
    case 6: return output_mov_long_double_arm_from_arm (operands);
    }
"
[(set_attr "length" "4,4,4,4,8,8,12")
 (set_attr "type" "ffarith,ffarith,f_load,f_store,r_mem_f,f_mem_r,*")])


;; load- and store-multiple insns
;; The arm can load/store any set of registers, provided that they are in
;; ascending order; but that is beyond GCC so stick with what it knows.

(define_expand "load_multiple"
  [(match_par_dup 3 [(set (match_operand:SI 0 "" "")
                          (match_operand:SI 1 "" ""))
                     (use (match_operand:SI 2 "" ""))])]
  ""
  "
  /* Support only fixed point registers */
  if (GET_CODE (operands[2]) != CONST_INT
      || INTVAL (operands[2]) > 14
      || INTVAL (operands[2]) < 2
      || GET_CODE (operands[1]) != MEM
      || GET_CODE (operands[0]) != REG
      || REGNO (operands[0]) > 14
      || REGNO (operands[0]) + INTVAL (operands[2]) > 15)
    FAIL;

  operands[3]
    = arm_gen_load_multiple (REGNO (operands[0]), INTVAL (operands[2]),
			     force_reg (SImode, XEXP (operands[1], 0)),
			     TRUE, FALSE, RTX_UNCHANGING_P(operands[1]),
			     MEM_IN_STRUCT_P(operands[1]),
	                     MEM_SCALAR_P (operands[1]));
")

;; Load multiple with write-back

(define_insn "*ldmsi_postinc"
  [(match_parallel 0 "load_multiple_operation"
    [(set (match_operand:SI 1 "s_register_operand" "+r")
	  (plus:SI (match_dup 1)
		   (match_operand:SI 2 "const_int_operand" "n")))
     (set (match_operand:SI 3 "s_register_operand" "=r")
	  (mem:SI (match_dup 1)))])]
  "(INTVAL (operands[2])  == 4 * (XVECLEN (operands[0], 0) - 2))"
  "*
{
  rtx ops[3];
  int count = XVECLEN (operands[0], 0);

  ops[0] = XEXP (SET_SRC (XVECEXP (operands[0], 0, 0)), 0);
  ops[1] = SET_DEST (XVECEXP (operands[0], 0, 1));
  ops[2] = SET_DEST (XVECEXP (operands[0], 0, count - 2));

  output_asm_insn (\"ldm%?ia\\t%0!, {%1-%2}\\t%@ load multiple\", ops);
  return \"\";
}
"
[(set_attr "type" "load")])

;; Ordinary load multiple

(define_insn "*ldmsi"
  [(match_parallel 0 "load_multiple_operation"
    [(set (match_operand:SI 1 "s_register_operand" "=r")
	  (mem:SI (match_operand:SI 2 "s_register_operand" "r")))])]
  ""
  "*
{
  rtx ops[3];
  int count = XVECLEN (operands[0], 0);

  ops[0] = XEXP (SET_SRC (XVECEXP (operands[0], 0, 0)), 0);
  ops[1] = SET_DEST (XVECEXP (operands[0], 0, 0));
  ops[2] = SET_DEST (XVECEXP (operands[0], 0, count - 1));

  output_asm_insn (\"ldm%?ia\\t%0, {%1-%2}\\t%@ load multiple\", ops);
  return \"\";
}
"
[(set_attr "type" "load")])

(define_expand "store_multiple"
  [(match_par_dup 3 [(set (match_operand:SI 0 "" "")
                          (match_operand:SI 1 "" ""))
                     (use (match_operand:SI 2 "" ""))])]
  ""
  "
  /* Support only fixed point registers */
  if (GET_CODE (operands[2]) != CONST_INT
      || INTVAL (operands[2]) > 14
      || INTVAL (operands[2]) < 2
      || GET_CODE (operands[1]) != REG
      || GET_CODE (operands[0]) != MEM
      || REGNO (operands[1]) > 14
      || REGNO (operands[1]) + INTVAL (operands[2]) > 15)
    FAIL;

  operands[3]
    = arm_gen_store_multiple (REGNO (operands[1]), INTVAL (operands[2]),
			      force_reg (SImode, XEXP (operands[0], 0)),
			      TRUE, FALSE, RTX_UNCHANGING_P (operands[0]),
			      MEM_IN_STRUCT_P(operands[0]), 
	                      MEM_SCALAR_P (operands[0]));
")

;; Store multiple with write-back

(define_insn "*stmsi_postinc"
  [(match_parallel 0 "store_multiple_operation"
    [(set (match_operand:SI 1 "s_register_operand" "+r")
	  (plus:SI (match_dup 1)
		   (match_operand:SI 2 "const_int_operand" "n")))
     (set (mem:SI (match_dup 1))
	  (match_operand:SI 3 "s_register_operand" "r"))])]
  "(INTVAL (operands[2]) == 4 * (XVECLEN (operands[0], 0) - 2))"
  "*
{
  rtx ops[3];
  int count = XVECLEN (operands[0], 0);

  ops[0] = XEXP (SET_SRC (XVECEXP (operands[0], 0, 0)), 0);
  ops[1] = SET_SRC (XVECEXP (operands[0], 0, 1));
  ops[2] = SET_SRC (XVECEXP (operands[0], 0, count - 2));

  output_asm_insn (\"stm%?ia\\t%0!, {%1-%2}\\t%@ str multiple\", ops);
  return \"\";
}
"
[(set (attr "type")
      (cond [(eq (symbol_ref "XVECLEN (operands[0],0)") (const_int 4))
		(const_string "store2")
	     (eq (symbol_ref "XVECLEN (operands[0],0)") (const_int 5))
		(const_string "store3")]
	  (const_string "store4")))])

;; Ordinary store multiple

(define_insn "*stmsi"
  [(match_parallel 0 "store_multiple_operation"
    [(set (mem:SI (match_operand:SI 2 "s_register_operand" "r"))
	  (match_operand:SI 1 "s_register_operand" "r"))])]
  ""
  "*
{
  rtx ops[3];
  int count = XVECLEN (operands[0], 0);

  ops[0] = XEXP (SET_DEST (XVECEXP (operands[0], 0, 0)), 0);
  ops[1] = SET_SRC (XVECEXP (operands[0], 0, 0));
  ops[2] = SET_SRC (XVECEXP (operands[0], 0, count - 1));

  output_asm_insn (\"stm%?ia\\t%0, {%1-%2}\\t%@ str multiple\", ops);
  return \"\";
}
"
[(set (attr "type")
      (cond [(eq (symbol_ref "XVECLEN (operands[0],0)") (const_int 3))
		(const_string "store2")
	     (eq (symbol_ref "XVECLEN (operands[0],0)") (const_int 4))
		(const_string "store3")]
	  (const_string "store4")))])

;; Move a block of memory if it is word aligned and MORE than 2 words long.
;; We could let this apply for blocks of less than this, but it clobbers so
;; many registers that there is then probably a better way.

(define_expand "movstrqi"
  [(match_operand:BLK 0 "general_operand" "")
   (match_operand:BLK 1 "general_operand" "")
   (match_operand:SI 2 "const_int_operand" "")
   (match_operand:SI 3 "const_int_operand" "")]
  ""
  "
  if (arm_gen_movstrqi (operands))
    DONE;
  FAIL;
")


;; Comparison and test insns

(define_expand "cmpsi"
  [(match_operand:SI 0 "s_register_operand" "")
   (match_operand:SI 1 "arm_add_operand" "")]
  ""
  "
{
  arm_compare_op0 = operands[0];
  arm_compare_op1 = operands[1];
  DONE;
}
")

(define_expand "cmpsf"
  [(match_operand:SF 0 "s_register_operand" "")
   (match_operand:SF 1 "fpu_rhs_operand" "")]
  "TARGET_HARD_FLOAT"
  "
{
  arm_compare_op0 = operands[0];
  arm_compare_op1 = operands[1];
  DONE;
}
")

(define_expand "cmpdf"
  [(match_operand:DF 0 "s_register_operand" "")
   (match_operand:DF 1 "fpu_rhs_operand" "")]
  "TARGET_HARD_FLOAT"
  "
{
  arm_compare_op0 = operands[0];
  arm_compare_op1 = operands[1];
  DONE;
}
")

(define_expand "cmpxf"
  [(match_operand:XF 0 "s_register_operand" "")
   (match_operand:XF 1 "fpu_rhs_operand" "")]
  "ENABLE_XF_PATTERNS && TARGET_HARD_FLOAT"
  "
{
  arm_compare_op0 = operands[0];
  arm_compare_op1 = operands[1];
  DONE;
}
")

(define_insn "*cmpsi_insn"
  [(set (reg:CC 24)
	(compare:CC (match_operand:SI 0 "s_register_operand" "r,r")
		    (match_operand:SI 1 "arm_add_operand" "rI,L")))]
  ""
  "@
   cmp%?\\t%0, %1
   cmn%?\\t%0, #%n1"
[(set_attr "conds" "set")])

(define_insn "*cmpsi_shiftsi"
  [(set (reg:CC 24)
	(compare:CC (match_operand:SI 0 "s_register_operand" "r")
		    (match_operator:SI 3 "shift_operator"
		     [(match_operand:SI 1 "s_register_operand" "r")
		      (match_operand:SI 2 "arm_rhs_operand" "rM")])))]
  ""
  "cmp%?\\t%0, %1%S3"
[(set_attr "conds" "set")])

(define_insn "*cmpsi_shiftsi_swp"
  [(set (reg:CC_SWP 24)
	(compare:CC_SWP (match_operator:SI 3 "shift_operator"
			 [(match_operand:SI 1 "s_register_operand" "r")
			  (match_operand:SI 2 "reg_or_int_operand" "rM")])
			(match_operand:SI 0 "s_register_operand" "r")))]
  ""
  "cmp%?\\t%0, %1%S3"
[(set_attr "conds" "set")])

(define_insn "*cmpsi_neg_shiftsi"
  [(set (reg:CC 24)
	(compare:CC (match_operand:SI 0 "s_register_operand" "r")
		    (neg:SI (match_operator:SI 3 "shift_operator"
			     [(match_operand:SI 1 "s_register_operand" "r")
			      (match_operand:SI 2 "arm_rhs_operand" "rM")]))))]
  ""
  "cmn%?\\t%0, %1%S3"
[(set_attr "conds" "set")])

(define_insn "*cmpsf_insn"
  [(set (reg:CCFP 24)
	(compare:CCFP (match_operand:SF 0 "s_register_operand" "f,f")
		      (match_operand:SF 1 "fpu_add_operand" "fG,H")))]
  "TARGET_HARD_FLOAT"
  "@
   cmf%?\\t%0, %1
   cnf%?\\t%0, #%N1"
[(set_attr "conds" "set")
 (set_attr "type" "f_2_r")])

(define_insn "*cmpdf_insn"
  [(set (reg:CCFP 24)
	(compare:CCFP (match_operand:DF 0 "s_register_operand" "f,f")
		      (match_operand:DF 1 "fpu_add_operand" "fG,H")))]
  "TARGET_HARD_FLOAT"
  "@
   cmf%?\\t%0, %1
   cnf%?\\t%0, #%N1"
[(set_attr "conds" "set")
 (set_attr "type" "f_2_r")])

(define_insn "*cmpesfdf_df"
  [(set (reg:CCFP 24)
	(compare:CCFP (float_extend:DF
		       (match_operand:SF 0 "s_register_operand" "f,f"))
		      (match_operand:DF 1 "fpu_add_operand" "fG,H")))]
  "TARGET_HARD_FLOAT"
  "@
   cmf%?\\t%0, %1
   cnf%?\\t%0, #%N1"
[(set_attr "conds" "set")
 (set_attr "type" "f_2_r")])

(define_insn "*cmpdf_esfdf"
  [(set (reg:CCFP 24)
	(compare:CCFP (match_operand:DF 0 "s_register_operand" "f")
		      (float_extend:DF
		       (match_operand:SF 1 "s_register_operand" "f"))))]
  "TARGET_HARD_FLOAT"
  "cmf%?\\t%0, %1"
[(set_attr "conds" "set")
 (set_attr "type" "f_2_r")])

(define_insn "*cmpxf_insn"
  [(set (reg:CCFP 24)
	(compare:CCFP (match_operand:XF 0 "s_register_operand" "f,f")
		      (match_operand:XF 1 "fpu_add_operand" "fG,H")))]
  "ENABLE_XF_PATTERNS && TARGET_HARD_FLOAT"
  "@
   cmf%?\\t%0, %1
   cnf%?\\t%0, #%N1"
[(set_attr "conds" "set")
 (set_attr "type" "f_2_r")])

(define_insn "*cmpsf_trap"
  [(set (reg:CCFPE 24)
	(compare:CCFPE (match_operand:SF 0 "s_register_operand" "f,f")
		       (match_operand:SF 1 "fpu_add_operand" "fG,H")))]
  "TARGET_HARD_FLOAT"
  "@
   cmf%?e\\t%0, %1
   cnf%?e\\t%0, #%N1"
[(set_attr "conds" "set")
 (set_attr "type" "f_2_r")])

(define_insn "*cmpdf_trap"
  [(set (reg:CCFPE 24)
	(compare:CCFPE (match_operand:DF 0 "s_register_operand" "f,f")
		       (match_operand:DF 1 "fpu_add_operand" "fG,H")))]
  "TARGET_HARD_FLOAT"
  "@
   cmf%?e\\t%0, %1
   cnf%?e\\t%0, #%N1"
[(set_attr "conds" "set")
 (set_attr "type" "f_2_r")])

(define_insn "*cmp_esfdf_df_trap"
  [(set (reg:CCFPE 24)
	(compare:CCFPE (float_extend:DF
			(match_operand:SF 0 "s_register_operand" "f,f"))
		       (match_operand:DF 1 "fpu_add_operand" "fG,H")))]
  "TARGET_HARD_FLOAT"
  "@
   cmf%?e\\t%0, %1
   cnf%?e\\t%0, #%N1"
[(set_attr "conds" "set")
 (set_attr "type" "f_2_r")])

(define_insn "*cmp_df_esfdf_trap"
  [(set (reg:CCFPE 24)
	(compare:CCFPE (match_operand:DF 0 "s_register_operand" "f")
		       (float_extend:DF
			(match_operand:SF 1 "s_register_operand" "f"))))]
  "TARGET_HARD_FLOAT"
  "cmf%?e\\t%0, %1"
[(set_attr "conds" "set")
 (set_attr "type" "f_2_r")])

(define_insn "*cmpxf_trap"
  [(set (reg:CCFPE 24)
	(compare:CCFPE (match_operand:XF 0 "s_register_operand" "f,f")
		       (match_operand:XF 1 "fpu_add_operand" "fG,H")))]
  "ENABLE_XF_PATTERNS && TARGET_HARD_FLOAT"
  "@
   cmf%?e\\t%0, %1
   cnf%?e\\t%0, #%N1"
[(set_attr "conds" "set")
 (set_attr "type" "f_2_r")])

; This insn allows redundant compares to be removed by cse, nothing should
; ever appear in the output file since (set (reg x) (reg x)) is a no-op that
; is deleted later on. The match_dup will match the mode here, so that
; mode changes of the condition codes aren't lost by this even though we don't
; specify what they are.

(define_insn "*deleted_compare"
  [(set (match_operand 0 "cc_register" "") (match_dup 0))]
  ""
  "\\t%@ deleted compare"
[(set_attr "conds" "set")
 (set_attr "length" "0")])


;; Conditional branch insns

(define_expand "beq"
  [(set (pc)
	(if_then_else (eq (match_dup 1) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{
  operands[1] = gen_compare_reg (EQ, arm_compare_op0, arm_compare_op1);
}
")

(define_expand "bne"
  [(set (pc)
	(if_then_else (ne (match_dup 1) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{
  operands[1] = gen_compare_reg (NE, arm_compare_op0, arm_compare_op1);
}
")

(define_expand "bgt"
  [(set (pc)
	(if_then_else (gt (match_dup 1) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{
  operands[1] = gen_compare_reg (GT, arm_compare_op0, arm_compare_op1);
}
")

(define_expand "ble"
  [(set (pc)
	(if_then_else (le (match_dup 1) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{
  operands[1] = gen_compare_reg (LE, arm_compare_op0, arm_compare_op1);
}
")

(define_expand "bge"
  [(set (pc)
	(if_then_else (ge (match_dup 1) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{
  operands[1] = gen_compare_reg (GE, arm_compare_op0, arm_compare_op1);
}
")

(define_expand "blt"
  [(set (pc)
	(if_then_else (lt (match_dup 1) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{
  operands[1] = gen_compare_reg (LT, arm_compare_op0, arm_compare_op1);
}
")

(define_expand "bgtu"
  [(set (pc)
	(if_then_else (gtu (match_dup 1) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{
  operands[1] = gen_compare_reg (GTU, arm_compare_op0, arm_compare_op1);
}
")

(define_expand "bleu"
  [(set (pc)
	(if_then_else (leu (match_dup 1) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{
  operands[1] = gen_compare_reg (LEU, arm_compare_op0, arm_compare_op1);
}
")

(define_expand "bgeu"
  [(set (pc)
	(if_then_else (geu (match_dup 1) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{
  operands[1] = gen_compare_reg (GEU, arm_compare_op0, arm_compare_op1);
}
")

(define_expand "bltu"
  [(set (pc)
	(if_then_else (ltu (match_dup 1) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{
  operands[1] = gen_compare_reg (LTU, arm_compare_op0, arm_compare_op1);
}
")

;; patterns to match conditional branch insns

(define_insn "*condbranch"
  [(set (pc)
	(if_then_else (match_operator 1 "comparison_operator"
		       [(match_operand 2 "cc_register" "") (const_int 0)])
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "*
{
  extern int arm_ccfsm_state;

  if (arm_ccfsm_state == 1 || arm_ccfsm_state == 2)
  {
    arm_ccfsm_state += 2;
    return \"\";
  }
  return \"b%d1\\t%l0\";
}"
[(set_attr "conds" "use")])

(define_insn "*condbranch_reversed"
  [(set (pc)
	(if_then_else (match_operator 1 "comparison_operator"
		       [(match_operand 2 "cc_register" "") (const_int 0)])
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "*
{
  extern int arm_ccfsm_state;

  if (arm_ccfsm_state == 1 || arm_ccfsm_state == 2)
  {
    arm_ccfsm_state += 2;
    return \"\";
  }
  return \"b%D1\\t%l0\";
}"
[(set_attr "conds" "use")])


; scc insns

(define_expand "seq"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(eq:SI (match_dup 1) (const_int 0)))]
  ""
  "
{
  operands[1] = gen_compare_reg (EQ, arm_compare_op0, arm_compare_op1);
}
")

(define_expand "sne"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(ne:SI (match_dup 1) (const_int 0)))]
  ""
  "
{
  operands[1] = gen_compare_reg (NE, arm_compare_op0, arm_compare_op1);
}
")

(define_expand "sgt"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(gt:SI (match_dup 1) (const_int 0)))]
  ""
  "
{
  operands[1] = gen_compare_reg (GT, arm_compare_op0, arm_compare_op1);
}
")

(define_expand "sle"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(le:SI (match_dup 1) (const_int 0)))]
  ""
  "
{
  operands[1] = gen_compare_reg (LE, arm_compare_op0, arm_compare_op1);
}
")

(define_expand "sge"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(ge:SI (match_dup 1) (const_int 0)))]
  ""
  "
{
  operands[1] = gen_compare_reg (GE, arm_compare_op0, arm_compare_op1);
}
")

(define_expand "slt"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(lt:SI (match_dup 1) (const_int 0)))]
  ""
  "
{
  operands[1] = gen_compare_reg (LT, arm_compare_op0, arm_compare_op1);
}
")

(define_expand "sgtu"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(gtu:SI (match_dup 1) (const_int 0)))]
  ""
  "
{
  operands[1] = gen_compare_reg (GTU, arm_compare_op0, arm_compare_op1);
}
")

(define_expand "sleu"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(leu:SI (match_dup 1) (const_int 0)))]
  ""
  "
{
  operands[1] = gen_compare_reg (LEU, arm_compare_op0, arm_compare_op1);
}
")

(define_expand "sgeu"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(geu:SI (match_dup 1) (const_int 0)))]
  ""
  "
{
  operands[1] = gen_compare_reg (GEU, arm_compare_op0, arm_compare_op1);
}
")

(define_expand "sltu"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(ltu:SI (match_dup 1) (const_int 0)))]
  ""
  "
{
  operands[1] = gen_compare_reg (LTU, arm_compare_op0, arm_compare_op1);
}
")

(define_insn "*mov_scc"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(match_operator:SI 1 "comparison_operator"
	 [(match_operand 2 "cc_register" "") (const_int 0)]))]
  ""
  "mov%D1\\t%0, #0\;mov%d1\\t%0, #1"
[(set_attr "conds" "use")
 (set_attr "length" "8")])

(define_insn "*mov_negscc"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(neg:SI (match_operator:SI 1 "comparison_operator"
		 [(match_operand 2 "cc_register" "") (const_int 0)])))]
  ""
  "mov%D1\\t%0, #0\;mvn%d1\\t%0, #0"
[(set_attr "conds" "use")
 (set_attr "length" "8")])

(define_insn "*mov_notscc"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(not:SI (match_operator:SI 1 "comparison_operator"
		 [(match_operand 2 "cc_register" "") (const_int 0)])))]
  ""
  "mov%D1\\t%0, #0\;mvn%d1\\t%0, #1"
[(set_attr "conds" "use")
 (set_attr "length" "8")])


;; Conditional move insns

(define_expand "movsicc"
  [(set (match_operand:SI 0 "s_register_operand" "")
	(if_then_else:SI (match_operand 1 "comparison_operator" "")
			 (match_operand:SI 2 "arm_not_operand" "")
			 (match_operand:SI 3 "arm_not_operand" "")))]
  ""
  "
{
  enum rtx_code code = GET_CODE (operands[1]);
  rtx ccreg = gen_compare_reg (code, arm_compare_op0, arm_compare_op1);

  operands[1] = gen_rtx (code, VOIDmode, ccreg, const0_rtx);
}")

(define_expand "movsfcc"
  [(set (match_operand:SF 0 "s_register_operand" "")
	(if_then_else:SF (match_operand 1 "comparison_operator" "")
			 (match_operand:SF 2 "s_register_operand" "")
			 (match_operand:SF 3 "nonmemory_operand" "")))]
  ""
  "
{
  enum rtx_code code = GET_CODE (operands[1]);
  rtx ccreg;

  /* When compiling for SOFT_FLOAT, ensure both arms are in registers. 
     Otherwise, ensure it is a valid FP add operand */
  if ((! TARGET_HARD_FLOAT)
      || (! fpu_add_operand (operands[3], SFmode)))
    operands[3] = force_reg (SFmode, operands[3]);

  ccreg = gen_compare_reg (code, arm_compare_op0, arm_compare_op1);

  operands[1] = gen_rtx (code, VOIDmode, ccreg, const0_rtx);
}")

(define_expand "movdfcc"
  [(set (match_operand:DF 0 "s_register_operand" "")
	(if_then_else:DF (match_operand 1 "comparison_operator" "")
			 (match_operand:DF 2 "s_register_operand" "")
			 (match_operand:DF 3 "fpu_add_operand" "")))]
  "TARGET_HARD_FLOAT"
  "
{
  enum rtx_code code = GET_CODE (operands[1]);
  rtx ccreg = gen_compare_reg (code, arm_compare_op0, arm_compare_op1);

  operands[1] = gen_rtx (code, VOIDmode, ccreg, const0_rtx);
}")

(define_insn "*movsicc_insn"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r,r,r,r,r,r,r")
	(if_then_else:SI
	 (match_operator 3 "comparison_operator"
	  [(match_operand 4 "cc_register" "") (const_int 0)])
	 (match_operand:SI 1 "arm_not_operand" "0,0,rI,K,rI,rI,K,K")
	 (match_operand:SI 2 "arm_not_operand" "rI,K,0,0,rI,K,rI,K")))]
  ""
  "@
   mov%D3\\t%0, %2
   mvn%D3\\t%0, #%B2
   mov%d3\\t%0, %1
   mvn%d3\\t%0, #%B1
   mov%d3\\t%0, %1\;mov%D3\\t%0, %2
   mov%d3\\t%0, %1\;mvn%D3\\t%0, #%B2
   mvn%d3\\t%0, #%B1\;mov%D3\\t%0, %2
   mvn%d3\\t%0, #%B1\;mvn%D3\\t%0, #%B2"
  [(set_attr "length" "4,4,4,4,8,8,8,8")
   (set_attr "conds" "use")])

(define_insn "*movsfcc_hard_insn"
  [(set (match_operand:SF 0 "s_register_operand" "=f,f,f,f,f,f,f,f")
	(if_then_else:SF
	 (match_operator 3 "comparison_operator" 
	  [(match_operand 4 "cc_register" "") (const_int 0)])
	 (match_operand:SF 1 "fpu_add_operand" "0,0,fG,H,fG,fG,H,H")
	 (match_operand:SF 2 "fpu_add_operand" "fG,H,0,0,fG,H,fG,H")))]
  "TARGET_HARD_FLOAT"
  "@
   mvf%D3s\\t%0, %2
   mnf%D3s\\t%0, #%N2
   mvf%d3s\\t%0, %1
   mnf%d3s\\t%0, #%N1
   mvf%d3s\\t%0, %1\;mvf%D3s\\t%0, %2
   mvf%d3s\\t%0, %1\;mnf%D3s\\t%0, #%N2
   mnf%d3s\\t%0, #%N1\;mvf%D3s\\t%0, %2
   mnf%d3s\\t%0, #%N1\;mnf%D3s\\t%0, #%N2"
  [(set_attr "length" "4,4,4,4,8,8,8,8")
   (set_attr "type" "ffarith")
   (set_attr "conds" "use")])

(define_insn "*movsfcc_soft_insn"
  [(set (match_operand:SF 0 "s_register_operand" "=r,r")
	(if_then_else:SF (match_operator 3 "comparison_operator"
			  [(match_operand 4 "cc_register" "") (const_int 0)])
			 (match_operand:SF 1 "s_register_operand" "0,r")
			 (match_operand:SF 2 "s_register_operand" "r,0")))]
  "TARGET_SOFT_FLOAT"
  "@
   mov%D3\\t%0, %2
   mov%d3\\t%0, %1"
  [(set_attr "conds" "use")])

(define_insn "*movdfcc_insn"
  [(set (match_operand:DF 0 "s_register_operand" "=f,f,f,f,f,f,f,f")
	(if_then_else:DF
	 (match_operator 3 "comparison_operator"
	  [(match_operand 4 "cc_register" "") (const_int 0)])
	 (match_operand:DF 1 "fpu_add_operand" "0,0,fG,H,fG,fG,H,H")
	 (match_operand:DF 2 "fpu_add_operand" "fG,H,0,0,fG,H,fG,H")))]
  "TARGET_HARD_FLOAT"
  "@
   mvf%D3d\\t%0, %2
   mnf%D3d\\t%0, #%N2
   mvf%d3d\\t%0, %1
   mnf%d3d\\t%0, #%N1
   mvf%d3d\\t%0, %1\;mvf%D3d\\t%0, %2
   mvf%d3d\\t%0, %1\;mnf%D3d\\t%0, #%N2
   mnf%d3d\\t%0, #%N1\;mvf%D3d\\t%0, %2
   mnf%d3d\\t%0, #%N1\;mnf%D3d\\t%0, #%N2"
  [(set_attr "length" "4,4,4,4,8,8,8,8")
   (set_attr "type" "ffarith")
   (set_attr "conds" "use")])

;; Jump and linkage insns

(define_insn "jump"
  [(set (pc)
	(label_ref (match_operand 0 "" "")))]
  ""
  "*
{
  extern int arm_ccfsm_state;

  if (arm_ccfsm_state == 1 || arm_ccfsm_state == 2)
    {
      arm_ccfsm_state += 2;
      return \"\";
    }
  return \"b%?\\t%l0\";
}")

(define_expand "call"
  [(parallel [(call (match_operand 0 "memory_operand" "")
	            (match_operand 1 "general_operand" ""))
	      (clobber (reg:SI 14))])]
  ""
  "")

(define_insn "*call_reg"
  [(call (mem:SI (match_operand:SI 0 "s_register_operand" "r"))
         (match_operand 1 "" "g"))
   (clobber (reg:SI 14))]
  ""
  "*
  return output_call (operands);
"
;; length is worst case, normally it is only two
[(set_attr "length" "12")
 (set_attr "type" "call")])

(define_insn "*call_mem"
  [(call (mem:SI (match_operand 0 "memory_operand" "m"))
	 (match_operand 1 "general_operand" "g"))
   (clobber (reg:SI 14))]
  ""
  "*
  return output_call_mem (operands);
"
[(set_attr "length" "12")
 (set_attr "type" "call")])

(define_expand "call_value"
  [(parallel [(set (match_operand 0 "" "=rf")
	           (call (match_operand 1 "memory_operand" "m")
		         (match_operand 2 "general_operand" "g")))
	      (clobber (reg:SI 14))])]
  ""
  "")

(define_insn "*call_value_reg"
  [(set (match_operand 0 "" "=rf")
        (call (mem:SI (match_operand:SI 1 "s_register_operand" "r"))
	      (match_operand 2 "general_operand" "g")))
   (clobber (reg:SI 14))]
  ""
  "*
  return output_call (&operands[1]);
"
[(set_attr "length" "12")
 (set_attr "type" "call")])

(define_insn "*call_value_mem"
  [(set (match_operand 0 "" "=rf")
	(call (mem:SI (match_operand 1 "memory_operand" "m"))
	(match_operand 2 "general_operand" "g")))
   (clobber (reg:SI 14))]
  "! CONSTANT_ADDRESS_P (XEXP (operands[1], 0))"
  "*
  return output_call_mem (&operands[1]);
"
[(set_attr "length" "12")
 (set_attr "type" "call")])

;; Allow calls to SYMBOL_REFs specially as they are not valid general addresses
;; The 'a' causes the operand to be treated as an address, i.e. no '#' output.

(define_insn "*call_symbol"
  [(call (mem:SI (match_operand:SI 0 "" "X"))
	 (match_operand:SI 1 "general_operand" "g"))
   (clobber (reg:SI 14))]
  "GET_CODE (operands[0]) == SYMBOL_REF"
  "*
  {
    return NEED_PLT_GOT ? \"bl%?\\t%a0(PLT)\" : \"bl%?\\t%a0\";
  }"
[(set_attr "type" "call")])

(define_insn "*call_value_symbol"
  [(set (match_operand 0 "s_register_operand" "=rf")
	(call (mem:SI (match_operand:SI 1 "" "X"))
	(match_operand:SI 2 "general_operand" "g")))
   (clobber (reg:SI 14))]
  "GET_CODE(operands[1]) == SYMBOL_REF"
  "*
  {
    return NEED_PLT_GOT ? \"bl%?\\t%a1(PLT)\" : \"bl%?\\t%a1\";
  }"
[(set_attr "type" "call")])

;; Often the return insn will be the same as loading from memory, so set attr
(define_insn "return"
  [(return)]
  "USE_RETURN_INSN(FALSE)"
  "*
{
  extern int arm_ccfsm_state;

  if (arm_ccfsm_state == 2)
  {
    arm_ccfsm_state += 2;
    return \"\";
  }
  return output_return_instruction (NULL, TRUE, FALSE);
}"
[(set_attr "type" "load")])

(define_insn "*cond_return"
  [(set (pc)
        (if_then_else (match_operator 0 "comparison_operator"
		       [(match_operand 1 "cc_register" "") (const_int 0)])
                      (return)
                      (pc)))]
  "USE_RETURN_INSN(TRUE)"
  "*
{
  extern int arm_ccfsm_state;

  if (arm_ccfsm_state == 2)
  {
    arm_ccfsm_state += 2;
    return \"\";
  }
  return output_return_instruction (operands[0], TRUE, FALSE);
}"
[(set_attr "conds" "use")
 (set_attr "type" "load")])

(define_insn "*cond_return_inverted"
  [(set (pc)
        (if_then_else (match_operator 0 "comparison_operator"
		       [(match_operand 1 "cc_register" "") (const_int 0)])
                      (pc)
		      (return)))]
  "USE_RETURN_INSN(TRUE)"
  "*
{
  extern int arm_ccfsm_state;

  if (arm_ccfsm_state == 2)
  {
    arm_ccfsm_state += 2;
    return \"\";
  }
  return output_return_instruction (operands[0], TRUE, TRUE);
}"
[(set_attr "conds" "use")
 (set_attr "type" "load")])

;; Call subroutine returning any type.

(define_expand "untyped_call"
  [(parallel [(call (match_operand 0 "" "")
		    (const_int 0))
	      (match_operand 1 "" "")
	      (match_operand 2 "" "")])]
  ""
  "
{
  int i;

  emit_call_insn (gen_call (operands[0], const0_rtx, NULL, const0_rtx));

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
}")

;; UNSPEC_VOLATILE is considered to use and clobber all hard registers and
;; all of memory.  This blocks insns from being moved across this point.

(define_insn "blockage"
  [(unspec_volatile [(const_int 0)] 0)]
  ""
  ""
[(set_attr "length" "0")
 (set_attr "type" "block")])

(define_expand "casesi"
  [(match_operand:SI 0 "s_register_operand" "")	; index to jump on
   (match_operand:SI 1 "const_int_operand" "")	; lower bound
   (match_operand:SI 2 "const_int_operand" "")	; total range
   (match_operand:SI 3 "" "")			; table label
   (match_operand:SI 4 "" "")]			; Out of range label
  ""
  "
{
  rtx reg;
  if (operands[1] != const0_rtx)
    {
      reg = gen_reg_rtx (SImode);
      emit_insn (gen_addsi3 (reg, operands[0],
			     GEN_INT (-INTVAL (operands[1]))));
      operands[0] = reg;
    }

  if (! const_ok_for_arm (INTVAL (operands[2])))
    operands[2] = force_reg (SImode, operands[2]);

  emit_jump_insn (gen_casesi_internal (operands[0], operands[2], operands[3],
				       operands[4]));
  DONE;
}")

;; The USE in this pattern is needed to tell flow analysis that this is
;; a CASESI insn.  It has no other purpose.
(define_insn "casesi_internal"
  [(parallel [(set (pc)
	       (if_then_else
		(leu (match_operand:SI 0 "s_register_operand" "r")
		     (match_operand:SI 1 "arm_rhs_operand" "rI"))
		(mem:SI (plus:SI (mult:SI (match_dup 0) (const_int 4))
				 (label_ref (match_operand 2 "" ""))))
		(label_ref (match_operand 3 "" ""))))
	      (use (label_ref (match_dup 2)))])]
  ""
  "*
  if (flag_pic)
    return \"cmp\\t%0, %1\;addls\\t%|pc, %|pc, %0, asl #2\;b\\t%l3\";
  return \"cmp\\t%0, %1\;ldrls\\t%|pc, [%|pc, %0, asl #2]\;b\\t%l3\";
"
[(set_attr "conds" "clob")
 (set_attr "length" "12")])

(define_insn "indirect_jump"
  [(set (pc)
	(match_operand:SI 0 "s_register_operand" "r"))]
  ""
  "mov%?\\t%|pc, %0\\t%@ indirect jump")

(define_insn "*load_indirect_jump"
  [(set (pc)
	(match_operand:SI 0 "memory_operand" "m"))]
  ""
  "ldr%?\\t%|pc, %0\\t%@ indirect jump"
[(set_attr "type" "load")])

;; Misc insns

(define_insn "nop"
  [(const_int 0)]
  ""
  "mov%?\\t%|r0, %|r0\\t%@ nop")

;; Patterns to allow combination of arithmetic, cond code and shifts

(define_insn "*arith_shiftsi"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
        (match_operator:SI 1 "shiftable_operator"
          [(match_operator:SI 3 "shift_operator"
             [(match_operand:SI 4 "s_register_operand" "r")
              (match_operand:SI 5 "reg_or_int_operand" "rI")])
           (match_operand:SI 2 "s_register_operand" "r")]))]
  ""
  "%i1%?\\t%0, %2, %4%S3")

(define_insn "*arith_shiftsi_compare0"
  [(set (reg:CC_NOOV 24)
        (compare:CC_NOOV (match_operator:SI 1 "shiftable_operator"
		          [(match_operator:SI 3 "shift_operator"
		            [(match_operand:SI 4 "s_register_operand" "r")
		             (match_operand:SI 5 "reg_or_int_operand" "rI")])
		           (match_operand:SI 2 "s_register_operand" "r")])
			 (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(match_op_dup 1 [(match_op_dup 3 [(match_dup 4) (match_dup 5)])
			 (match_dup 2)]))]
  ""
  "%i1%?s\\t%0, %2, %4%S3"
[(set_attr "conds" "set")])

(define_insn "*arith_shiftsi_compare0_scratch"
  [(set (reg:CC_NOOV 24)
        (compare:CC_NOOV (match_operator:SI 1 "shiftable_operator"
		          [(match_operator:SI 3 "shift_operator"
		            [(match_operand:SI 4 "s_register_operand" "r")
		             (match_operand:SI 5 "reg_or_int_operand" "rI")])
		           (match_operand:SI 2 "s_register_operand" "r")])
			 (const_int 0)))
   (clobber (match_scratch:SI 0 "=r"))]
  ""
  "%i1%?s\\t%0, %2, %4%S3"
[(set_attr "conds" "set")])

(define_insn "*sub_shiftsi"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(minus:SI (match_operand:SI 1 "s_register_operand" "r")
		  (match_operator:SI 2 "shift_operator"
		   [(match_operand:SI 3 "s_register_operand" "r")
		    (match_operand:SI 4 "reg_or_int_operand" "rM")])))]
  ""
  "sub%?\\t%0, %1, %3%S2")

(define_insn "*sub_shiftsi_compare0"
  [(set (reg:CC_NOOV 24)
	(compare:CC_NOOV
	 (minus:SI (match_operand:SI 1 "s_register_operand" "r")
		   (match_operator:SI 2 "shift_operator"
		    [(match_operand:SI 3 "s_register_operand" "r")
		     (match_operand:SI 4 "reg_or_int_operand" "rM")]))
	 (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(minus:SI (match_dup 1) (match_op_dup 2 [(match_dup 3)
						 (match_dup 4)])))]
  ""
  "sub%?s\\t%0, %1, %3%S2"
[(set_attr "conds" "set")])

(define_insn "*sub_shiftsi_compare0_scratch"
  [(set (reg:CC_NOOV 24)
	(compare:CC_NOOV
	 (minus:SI (match_operand:SI 1 "s_register_operand" "r")
		   (match_operator:SI 2 "shift_operator"
		    [(match_operand:SI 3 "s_register_operand" "r")
		     (match_operand:SI 4 "reg_or_int_operand" "rM")]))
	 (const_int 0)))
   (clobber (match_scratch:SI 0 "=r"))]
  ""
  "sub%?s\\t%0, %1, %3%S2"
[(set_attr "conds" "set")])

;; These variants of the above insns can occur if the first operand is the
;; frame pointer and we eliminate that.  This is a kludge, but there doesn't
;; seem to be a way around it.  Most of the predicates have to be null
;; because the format can be generated part way through reload, so
;; if we don't match it as soon as it becomes available, reload doesn't know
;; how to reload pseudos that haven't got hard registers; the constraints will
;; sort everything out.

(define_insn "*reload_mulsi3"
  [(set (match_operand:SI 0 "" "=&r")
	(plus:SI (plus:SI (match_operator:SI 5 "shift_operator"
			   [(match_operand:SI 3 "" "r")
			    (match_operand:SI 4 "" "rM")])
			  (match_operand:SI 2 "" "r"))
		 (match_operand:SI 1 "const_int_operand" "n")))]
  "reload_in_progress"
  "*
  output_asm_insn (\"add%?\\t%0, %2, %3%S5\", operands);
  operands[2] = operands[1];
  operands[1] = operands[0];
  return output_add_immediate (operands);
"
; we have no idea how long the add_immediate is, it could be up to 4.
[(set_attr "length" "20")])

(define_insn "*reload_mulsi_compare0"
  [(set (reg:CC_NOOV 24)
	(compare:CC_NOOV (plus:SI
			  (plus:SI 
			   (match_operator:SI 5 "shift_operator"
			    [(match_operand:SI 3 "" "r")
			     (match_operand:SI 4 "" "rM")])
			   (match_operand:SI 1 "" "r"))
			  (match_operand:SI 2 "const_int_operand" "n"))
			 (const_int 0)))
   (set (match_operand:SI 0 "" "=&r")
	(plus:SI (plus:SI (match_op_dup 5 [(match_dup 3) (match_dup 4)])
			  (match_dup 1))
		 (match_dup 2)))]
  "reload_in_progress"
  "*
  output_add_immediate (operands);
  return \"add%?s\\t%0, %0, %3%S5\";
"
[(set_attr "conds" "set")
 (set_attr "length" "20")])

(define_insn "*reload_mulsi_compare0_scratch"
  [(set (reg:CC_NOOV 24)
	(compare:CC_NOOV (plus:SI
			  (plus:SI 
			   (match_operator:SI 5 "shift_operator"
			    [(match_operand:SI 3 "" "r")
			     (match_operand:SI 4 "" "rM")])
			   (match_operand:SI 1 "" "r"))
			  (match_operand:SI 2 "const_int_operand" "n"))
			 (const_int 0)))
   (clobber (match_scratch:SI 0 "=&r"))]
  "reload_in_progress"
  "*
  output_add_immediate (operands);
  return \"add%?s\\t%0, %0, %3%S5\";
"
[(set_attr "conds" "set")
 (set_attr "length" "20")])

;; These are similar, but are needed when the mla pattern contains the
;; eliminated register as operand 3.

(define_insn "*reload_muladdsi"
  [(set (match_operand:SI 0 "" "=&r,&r")
	(plus:SI (plus:SI (mult:SI (match_operand:SI 1 "" "%0,r")
				   (match_operand:SI 2 "" "r,r"))
			  (match_operand:SI 3 "" "r,r"))
		 (match_operand:SI 4 "const_int_operand" "n,n")))]
  "reload_in_progress"
  "*
  output_asm_insn (\"mla%?\\t%0, %2, %1, %3\", operands);
  operands[2] = operands[4];
  operands[1] = operands[0];
  return output_add_immediate (operands);
"
[(set_attr "length" "20")
 (set_attr "type" "mult")])

(define_insn "*reload_muladdsi_compare0"
  [(set (reg:CC_NOOV 24)
	(compare:CC_NOOV (plus:SI (plus:SI (mult:SI
					    (match_operand:SI 3 "" "r")
					    (match_operand:SI 4 "" "r"))
					   (match_operand:SI 1 "" "r"))
				  (match_operand:SI 2 "const_int_operand" "n"))
			 (const_int 0)))
   (set (match_operand:SI 0 "" "=&r")
	(plus:SI (plus:SI (mult:SI (match_dup 3) (match_dup 4)) (match_dup 1))
		 (match_dup 2)))]
  "reload_in_progress"
  "*
  output_add_immediate (operands);
  output_asm_insn (\"mla%?s\\t%0, %3, %4, %0\", operands);
  return \"\";
"
[(set_attr "length" "20")
 (set_attr "conds" "set")
 (set_attr "type" "mult")])

(define_insn "*reload_muladdsi_compare0_scratch"
  [(set (reg:CC_NOOV 24)
	(compare:CC_NOOV (plus:SI (plus:SI (mult:SI
					    (match_operand:SI 3 "" "r")
					    (match_operand:SI 4 "" "r"))
					   (match_operand:SI 1 "" "r"))
				  (match_operand:SI 2 "const_int_operand" "n"))
			 (const_int 0)))
   (clobber (match_scratch:SI 0 "=&r"))]
  "reload_in_progress"
  "*
  output_add_immediate (operands);
  return \"mla%?s\\t%0, %3, %4, %0\";
"
[(set_attr "length" "20")
 (set_attr "conds" "set")
 (set_attr "type" "mult")])



(define_insn "*and_scc"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(and:SI (match_operator 1 "comparison_operator"
		 [(match_operand 3 "cc_register" "") (const_int 0)])
		(match_operand:SI 2 "s_register_operand" "r")))]
  ""
  "mov%D1\\t%0, #0\;and%d1\\t%0, %2, #1"
[(set_attr "conds" "use")
 (set_attr "length" "8")])

(define_insn "*ior_scc"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(ior:SI (match_operator 2 "comparison_operator"
		 [(match_operand 3 "cc_register" "") (const_int 0)])
		(match_operand:SI 1 "s_register_operand" "0,?r")))]
  ""
  "@
   orr%d2\\t%0, %1, #1
   mov%D2\\t%0, %1\;orr%d2\\t%0, %1, #1"
[(set_attr "conds" "use")
 (set_attr "length" "4,8")])

(define_insn "*compare_scc"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(match_operator 1 "comparison_operator"
	 [(match_operand:SI 2 "s_register_operand" "r,r")
	  (match_operand:SI 3 "arm_add_operand" "rI,L")]))
   (clobber (reg:CC 24))]
  ""
  "*
  if (GET_CODE (operands[1]) == LT && operands[3] == const0_rtx)
    return \"mov\\t%0, %2, lsr #31\";

  if (GET_CODE (operands[1]) == GE && operands[3] == const0_rtx)
    return \"mvn\\t%0, %2\;mov\\t%0, %0, lsr #31\";

  if (GET_CODE (operands[1]) == NE)
    {
      if (which_alternative == 1)
	return \"adds\\t%0, %2, #%n3\;movne\\t%0, #1\";
      return \"subs\\t%0, %2, %3\;movne\\t%0, #1\";
    }
  if (which_alternative == 1)
    output_asm_insn (\"cmn\\t%2, #%n3\", operands);
  else
    output_asm_insn (\"cmp\\t%2, %3\", operands);
  return \"mov%D1\\t%0, #0\;mov%d1\\t%0, #1\";
"
[(set_attr "conds" "clob")
 (set_attr "length" "12")])

(define_insn "*cond_move"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r,r")
	(if_then_else:SI (match_operator 3 "equality_operator"
			  [(match_operator 4 "comparison_operator"
			    [(match_operand 5 "cc_register" "") (const_int 0)])
			   (const_int 0)])
			 (match_operand:SI 1 "arm_rhs_operand" "0,rI,?rI")
			 (match_operand:SI 2 "arm_rhs_operand" "rI,0,rI")))]
  ""
  "*
  if (GET_CODE (operands[3]) == NE)
    {
      if (which_alternative != 1)
	output_asm_insn (\"mov%D4\\t%0, %2\", operands);
      if (which_alternative != 0)
	output_asm_insn (\"mov%d4\\t%0, %1\", operands);
      return \"\";
    }
  if (which_alternative != 0)
    output_asm_insn (\"mov%D4\\t%0, %1\", operands);
  if (which_alternative != 1)
    output_asm_insn (\"mov%d4\\t%0, %2\", operands);
  return \"\";
"
[(set_attr "conds" "use")
 (set_attr "length" "4,4,8")])

(define_insn "*cond_arith"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
        (match_operator:SI 5 "shiftable_operator" 
	 [(match_operator:SI 4 "comparison_operator"
           [(match_operand:SI 2 "s_register_operand" "r,r")
	    (match_operand:SI 3 "arm_rhs_operand" "rI,rI")])
          (match_operand:SI 1 "s_register_operand" "0,?r")]))
   (clobber (reg:CC 24))]
  ""
  "*
  if (GET_CODE (operands[4]) == LT && operands[3] == const0_rtx)
    return \"%i5\\t%0, %1, %2, lsr #31\";

  output_asm_insn (\"cmp\\t%2, %3\", operands);
  if (GET_CODE (operands[5]) == AND)
    output_asm_insn (\"mov%D4\\t%0, #0\", operands);
  else if (GET_CODE (operands[5]) == MINUS)
    output_asm_insn (\"rsb%D4\\t%0, %1, #0\", operands);
  else if (which_alternative != 0)
    output_asm_insn (\"mov%D4\\t%0, %1\", operands);
  return \"%i5%d4\\t%0, %1, #1\";
"
[(set_attr "conds" "clob")
 (set_attr "length" "12")])

(define_insn "*cond_sub"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
        (minus:SI (match_operand:SI 1 "s_register_operand" "0,?r")
		  (match_operator:SI 4 "comparison_operator"
                   [(match_operand:SI 2 "s_register_operand" "r,r")
		    (match_operand:SI 3 "arm_rhs_operand" "rI,rI")])))
   (clobber (reg:CC 24))]
  ""
  "*
  output_asm_insn (\"cmp\\t%2, %3\", operands);
  if (which_alternative != 0)
    output_asm_insn (\"mov%D4\\t%0, %1\", operands);
  return \"sub%d4\\t%0, %1, #1\";
"
[(set_attr "conds" "clob")
 (set_attr "length" "8,12")])

(define_insn "*cmp_ite0"
  [(set (match_operand 6 "dominant_cc_register" "")
	(compare
	 (if_then_else:SI
	  (match_operator 4 "comparison_operator"
	   [(match_operand:SI 0 "s_register_operand" "r,r,r,r")
	    (match_operand:SI 1 "arm_add_operand" "rI,L,rI,L")])
	  (match_operator:SI 5 "comparison_operator"
	   [(match_operand:SI 2 "s_register_operand" "r,r,r,r")
	    (match_operand:SI 3 "arm_add_operand" "rI,rI,L,L")])
	  (const_int 0))
	 (const_int 0)))]
  ""
  "*
{
  char* opcodes[4][2] =
  {
    {\"cmp\\t%2, %3\;cmp%d5\\t%0, %1\",\"cmp\\t%0, %1\;cmp%d4\\t%2, %3\"},
    {\"cmp\\t%2, %3\;cmn%d5\\t%0, #%n1\", \"cmn\\t%0, #%n1\;cmp%d4\\t%2, %3\"},
    {\"cmn\\t%2, #%n3\;cmp%d5\\t%0, %1\", \"cmp\\t%0, %1\;cmn%d4\\t%2, #%n3\"},
    {\"cmn\\t%2, #%n3\;cmn%d5\\t%0, #%n1\",
     \"cmn\\t%0, #%n1\;cmn%d4\\t%2, #%n3\"}
  };
  int swap =
    comparison_dominates_p (GET_CODE (operands[5]), GET_CODE (operands[4]));

  return opcodes[which_alternative][swap];
}
"
[(set_attr "conds" "set")
 (set_attr "length" "8")])

(define_insn "*cmp_ite1"
  [(set (match_operand 6 "dominant_cc_register" "")
	(compare
	 (if_then_else:SI
	  (match_operator 4 "comparison_operator"
	   [(match_operand:SI 0 "s_register_operand" "r,r,r,r")
	    (match_operand:SI 1 "arm_add_operand" "rI,L,rI,L")])
	  (match_operator:SI 5 "comparison_operator"
	   [(match_operand:SI 2 "s_register_operand" "r,r,r,r")
	    (match_operand:SI 3 "arm_add_operand" "rI,rI,L,L")])
	  (const_int 1))
	 (const_int 0)))]
  ""
  "*
{
  char* opcodes[4][2] =
  {
    {\"cmp\\t%0, %1\;cmp%d4\\t%2, %3\", \"cmp\\t%2, %3\;cmp%D5\\t%0, %1\"},
    {\"cmn\\t%0, #%n1\;cmp%d4\\t%2, %3\", \"cmp\\t%2, %3\;cmn%D5\\t%0, #%n1\"},
    {\"cmp\\t%0, %1\;cmn%d4\\t%2, #%n3\", \"cmn\\t%2, #%n3\;cmp%D5\\t%0, %1\"},
    {\"cmn\\t%0, #%n1\;cmn%d4\\t%2, #%n3\",
     \"cmn\\t%2, #%n3\;cmn%D5\\t%0, #%n1\"}
  };
  int swap =
    comparison_dominates_p (GET_CODE (operands[5]),
			    reverse_condition (GET_CODE (operands[4])));

  return opcodes[which_alternative][swap];
}
"
[(set_attr "conds" "set")
 (set_attr "length" "8")])

(define_insn "*negscc"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(neg:SI (match_operator 3 "comparison_operator"
		 [(match_operand:SI 1 "s_register_operand" "r")
		  (match_operand:SI 2 "arm_rhs_operand" "rI")])))
   (clobber (reg:CC 24))]
  ""
  "*
  if (GET_CODE (operands[3]) == LT && operands[3] == const0_rtx)
    return \"mov\\t%0, %1, asr #31\";

  if (GET_CODE (operands[3]) == NE)
    return \"subs\\t%0, %1, %2\;mvnne\\t%0, #0\";

  if (GET_CODE (operands[3]) == GT)
    return \"subs\\t%0, %1, %2\;mvnne\\t%0, %0, asr #31\";

  output_asm_insn (\"cmp\\t%1, %2\", operands);
  output_asm_insn (\"mov%D3\\t%0, #0\", operands);
  return \"mvn%d3\\t%0, #0\";
"
[(set_attr "conds" "clob")
 (set_attr "length" "12")])

(define_insn "movcond"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r,r")
	(if_then_else:SI
	 (match_operator 5 "comparison_operator"
	  [(match_operand:SI 3 "s_register_operand" "r,r,r")
	   (match_operand:SI 4 "arm_add_operand" "rIL,rIL,rIL")])
	 (match_operand:SI 1 "arm_rhs_operand" "0,rI,?rI")
	 (match_operand:SI 2 "arm_rhs_operand" "rI,0,rI")))
   (clobber (reg:CC 24))]
  ""
  "*
  if (GET_CODE (operands[5]) == LT
      && (operands[4] == const0_rtx))
    {
      if (which_alternative != 1 && GET_CODE (operands[1]) == REG)
	{
	  if (operands[2] == const0_rtx)
	    return \"and\\t%0, %1, %3, asr #31\";
	  return \"ands\\t%0, %1, %3, asr #32\;movcc\\t%0, %2\";
	}
      else if (which_alternative != 0 && GET_CODE (operands[2]) == REG)
	{
	  if (operands[1] == const0_rtx)
	    return \"bic\\t%0, %2, %3, asr #31\";
	  return \"bics\\t%0, %2, %3, asr #32\;movcs\\t%0, %1\";
	}
      /* The only case that falls through to here is when both ops 1 & 2
	 are constants */
    }

  if (GET_CODE (operands[5]) == GE
      && (operands[4] == const0_rtx))
    {
      if (which_alternative != 1 && GET_CODE (operands[1]) == REG)
	{
	  if (operands[2] == const0_rtx)
	    return \"bic\\t%0, %1, %3, asr #31\";
	  return \"bics\\t%0, %1, %3, asr #32\;movcs\\t%0, %2\";
	}
      else if (which_alternative != 0 && GET_CODE (operands[2]) == REG)
	{
	  if (operands[1] == const0_rtx)
	    return \"and\\t%0, %2, %3, asr #31\";
	  return \"ands\\t%0, %2, %3, asr #32\;movcc\\t%0, %1\";
	}
      /* The only case that falls through to here is when both ops 1 & 2
	 are constants */
    }
  if (GET_CODE (operands[4]) == CONST_INT
      && !const_ok_for_arm (INTVAL (operands[4])))
    output_asm_insn (\"cmn\\t%3, #%n4\", operands);
  else
    output_asm_insn (\"cmp\\t%3, %4\", operands);
  if (which_alternative != 0)
    output_asm_insn (\"mov%d5\\t%0, %1\", operands);
  if (which_alternative != 1)
    output_asm_insn (\"mov%D5\\t%0, %2\", operands);
  return \"\";
"
[(set_attr "conds" "clob")
 (set_attr "length" "8,8,12")])

(define_insn "*ifcompare_plus_move"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(if_then_else:SI (match_operator 6 "comparison_operator"
			  [(match_operand:SI 4 "s_register_operand" "r,r")
			   (match_operand:SI 5 "arm_add_operand" "rIL,rIL")])
			 (plus:SI
			  (match_operand:SI 2 "s_register_operand" "r,r")
			  (match_operand:SI 3 "arm_add_operand" "rIL,rIL"))
			 (match_operand:SI 1 "arm_rhs_operand" "0,?rI")))
   (clobber (reg:CC 24))]
  ""
  "#"
[(set_attr "conds" "clob")
 (set_attr "length" "8,12")])

(define_insn "*if_plus_move"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r,r,r")
	(if_then_else:SI
	 (match_operator 4 "comparison_operator"
	  [(match_operand 5 "cc_register" "") (const_int 0)])
	 (plus:SI
	  (match_operand:SI 2 "s_register_operand" "r,r,r,r")
	  (match_operand:SI 3 "arm_add_operand" "rI,L,rI,L"))
	 (match_operand:SI 1 "arm_rhs_operand" "0,0,?rI,?rI")))]
  ""
  "@
   add%d4\\t%0, %2, %3
   sub%d4\\t%0, %2, #%n3
   add%d4\\t%0, %2, %3\;mov%D4\\t%0, %1
   sub%d4\\t%0, %2, #%n3\;mov%D4\\t%0, %1"
[(set_attr "conds" "use")
 (set_attr "length" "4,4,8,8")
 (set_attr "type" "*,*,*,*")])

(define_insn "*ifcompare_move_plus"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(if_then_else:SI (match_operator 6 "comparison_operator"
			  [(match_operand:SI 4 "s_register_operand" "r,r")
			   (match_operand:SI 5 "arm_add_operand" "rIL,rIL")])
			 (match_operand:SI 1 "arm_rhs_operand" "0,?rI")
			 (plus:SI
			  (match_operand:SI 2 "s_register_operand" "r,r")
			  (match_operand:SI 3 "arm_add_operand" "rIL,rIL"))))
   (clobber (reg:CC 24))]
  ""
  "#"
[(set_attr "conds" "clob")
 (set_attr "length" "8,12")])

(define_insn "*if_move_plus"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r,r,r")
	(if_then_else:SI
	 (match_operator 4 "comparison_operator"
	  [(match_operand 5 "cc_register" "") (const_int 0)])
	 (match_operand:SI 1 "arm_rhs_operand" "0,0,?rI,?rI")
	 (plus:SI
	  (match_operand:SI 2 "s_register_operand" "r,r,r,r")
	  (match_operand:SI 3 "arm_add_operand" "rI,L,rI,L"))))]
  ""
  "@
   add%D4\\t%0, %2, %3
   sub%D4\\t%0, %2, #%n3
   add%D4\\t%0, %2, %3\;mov%d4\\t%0, %1
   sub%D4\\t%0, %2, #%n3\;mov%d4\\t%0, %1"
[(set_attr "conds" "use")
 (set_attr "length" "4,4,8,8")
 (set_attr "type" "*,*,*,*")])

(define_insn "*ifcompare_arith_arith"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(if_then_else:SI (match_operator 9 "comparison_operator"
			  [(match_operand:SI 5 "s_register_operand" "r")
			   (match_operand:SI 6 "arm_add_operand" "rIL")])
			 (match_operator:SI 8 "shiftable_operator"
			  [(match_operand:SI 1 "s_register_operand" "r")
			   (match_operand:SI 2 "arm_rhs_operand" "rI")])
			 (match_operator:SI 7 "shiftable_operator"
			  [(match_operand:SI 3 "s_register_operand" "r")
			   (match_operand:SI 4 "arm_rhs_operand" "rI")])))
   (clobber (reg:CC 24))]
  ""
  "#"
[(set_attr "conds" "clob")
 (set_attr "length" "12")])

(define_insn "*if_arith_arith"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(if_then_else:SI (match_operator 5 "comparison_operator"
			  [(match_operand 8 "cc_register" "") (const_int 0)])
			 (match_operator:SI 6 "shiftable_operator"
			  [(match_operand:SI 1 "s_register_operand" "r")
			   (match_operand:SI 2 "arm_rhs_operand" "rI")])
			 (match_operator:SI 7 "shiftable_operator"
			  [(match_operand:SI 3 "s_register_operand" "r")
			   (match_operand:SI 4 "arm_rhs_operand" "rI")])))]
  ""
  "%I6%d5\\t%0, %1, %2\;%I7%D5\\t%0, %3, %4"
[(set_attr "conds" "use")
 (set_attr "length" "8")])

(define_insn "*ifcompare_arith_move"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(if_then_else:SI (match_operator 6 "comparison_operator"
			  [(match_operand:SI 2 "s_register_operand" "r,r")
			   (match_operand:SI 3 "arm_add_operand" "rIL,rIL")])
			 (match_operator:SI 7 "shiftable_operator"
			  [(match_operand:SI 4 "s_register_operand" "r,r")
			   (match_operand:SI 5 "arm_rhs_operand" "rI,rI")])
			 (match_operand:SI 1 "arm_rhs_operand" "0,?rI")))
   (clobber (reg:CC 24))]
  ""
  "*
  /* If we have an operation where (op x 0) is the identity operation and
     the conditional operator is LT or GE and we are comparing against zero and
     everything is in registers then we can do this in two instructions */
  if (operands[3] == const0_rtx
      && GET_CODE (operands[7]) != AND
      && GET_CODE (operands[5]) == REG
      && GET_CODE (operands[1]) == REG 
      && REGNO (operands[1]) == REGNO (operands[4])
      && REGNO (operands[4]) != REGNO (operands[0]))
    {
      if (GET_CODE (operands[6]) == LT)
	return \"and\\t%0, %5, %2, asr #31\;%I7\\t%0, %4, %0\";
      else if (GET_CODE (operands[6]) == GE)
	return \"bic\\t%0, %5, %2, asr #31\;%I7\\t%0, %4, %0\";
    }
  if (GET_CODE (operands[3]) == CONST_INT
      && !const_ok_for_arm (INTVAL (operands[3])))
    output_asm_insn (\"cmn\\t%2, #%n3\", operands);
  else
    output_asm_insn (\"cmp\\t%2, %3\", operands);
  output_asm_insn (\"%I7%d6\\t%0, %4, %5\", operands);
  if (which_alternative != 0)
    return \"mov%D6\\t%0, %1\";
  return \"\";
"
[(set_attr "conds" "clob")
 (set_attr "length" "8,12")])

(define_insn "*if_arith_move"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(if_then_else:SI (match_operator 4 "comparison_operator"
			  [(match_operand 6 "cc_register" "") (const_int 0)])
			 (match_operator:SI 5 "shiftable_operator"
			  [(match_operand:SI 2 "s_register_operand" "r,r")
			   (match_operand:SI 3 "arm_rhs_operand" "rI,rI")])
			 (match_operand:SI 1 "arm_rhs_operand" "0,?rI")))]
  ""
  "@
   %I5%d4\\t%0, %2, %3
   %I5%d4\\t%0, %2, %3\;mov%D4\\t%0, %1"
[(set_attr "conds" "use")
 (set_attr "length" "4,8")
 (set_attr "type" "*,*")])

(define_insn "*ifcompare_move_arith"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(if_then_else:SI (match_operator 6 "comparison_operator"
			  [(match_operand:SI 4 "s_register_operand" "r,r")
			   (match_operand:SI 5 "arm_add_operand" "rIL,rIL")])
			 (match_operand:SI 1 "arm_rhs_operand" "0,?rI")
			 (match_operator:SI 7 "shiftable_operator"
			  [(match_operand:SI 2 "s_register_operand" "r,r")
			   (match_operand:SI 3 "arm_rhs_operand" "rI,rI")])))
   (clobber (reg:CC 24))]
  ""
  "*
  /* If we have an operation where (op x 0) is the identity operation and
     the conditional operator is LT or GE and we are comparing against zero and
     everything is in registers then we can do this in two instructions */
  if (operands[5] == const0_rtx
      && GET_CODE (operands[7]) != AND
      && GET_CODE (operands[3]) == REG
      && GET_CODE (operands[1]) == REG 
      && REGNO (operands[1]) == REGNO (operands[2])
      && REGNO (operands[2]) != REGNO (operands[0]))
    {
      if (GET_CODE (operands[6]) == GE)
	return \"and\\t%0, %3, %4, asr #31\;%I7\\t%0, %2, %0\";
      else if (GET_CODE (operands[6]) == LT)
	return \"bic\\t%0, %3, %4, asr #31\;%I7\\t%0, %2, %0\";
    }

  if (GET_CODE (operands[5]) == CONST_INT
      && !const_ok_for_arm (INTVAL (operands[5])))
    output_asm_insn (\"cmn\\t%4, #%n5\", operands);
  else
    output_asm_insn (\"cmp\\t%4, %5\", operands);

  if (which_alternative != 0)
    output_asm_insn (\"mov%d6\\t%0, %1\", operands);
  return \"%I7%D6\\t%0, %2, %3\";
"
[(set_attr "conds" "clob")
 (set_attr "length" "8,12")])

(define_insn "*if_move_arith"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(if_then_else:SI
	 (match_operator 4 "comparison_operator"
	  [(match_operand 6 "cc_register" "") (const_int 0)])
	 (match_operand:SI 1 "arm_rhs_operand" "0,?rI")
	 (match_operator:SI 5 "shiftable_operator"
	  [(match_operand:SI 2 "s_register_operand" "r,r")
	   (match_operand:SI 3 "arm_rhs_operand" "rI,rI")])))]
  ""
  "@
   %I5%D4\\t%0, %2, %3
   %I5%D4\\t%0, %2, %3\;mov%d4\\t%0, %1"
[(set_attr "conds" "use")
 (set_attr "length" "4,8")
 (set_attr "type" "*,*")])

(define_insn "*ifcompare_move_not"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(if_then_else:SI
	 (match_operator 5 "comparison_operator"
	  [(match_operand:SI 3 "s_register_operand" "r,r")
	   (match_operand:SI 4 "arm_add_operand" "rIL,rIL")])
	 (match_operand:SI 1 "arm_not_operand" "0,?rIK")
	 (not:SI
	  (match_operand:SI 2 "s_register_operand" "r,r"))))
   (clobber (reg:CC 24))]
  ""
  "#"
[(set_attr "conds" "clob")
 (set_attr "length" "8,12")])

(define_insn "*if_move_not"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r,r")
	(if_then_else:SI
	 (match_operator 4 "comparison_operator"
	  [(match_operand 3 "cc_register" "") (const_int 0)])
	 (match_operand:SI 1 "arm_not_operand" "0,?rI,K")
	 (not:SI (match_operand:SI 2 "s_register_operand" "r,r,r"))))]
  ""
  "@
   mvn%D4\\t%0, %2
   mov%d4\\t%0, %1\;mvn%D4\\t%0, %2
   mvn%d4\\t%0, #%B1\;mvn%D4\\t%0, %2"
[(set_attr "conds" "use")
 (set_attr "length" "4,8,8")])

(define_insn "*ifcompare_not_move"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(if_then_else:SI 
	 (match_operator 5 "comparison_operator"
	  [(match_operand:SI 3 "s_register_operand" "r,r")
	   (match_operand:SI 4 "arm_add_operand" "rIL,rIL")])
	 (not:SI
	  (match_operand:SI 2 "s_register_operand" "r,r"))
	 (match_operand:SI 1 "arm_not_operand" "0,?rIK")))
   (clobber (reg:CC 24))]
  ""
  "#"
[(set_attr "conds" "clob")
 (set_attr "length" "8,12")])

(define_insn "*if_not_move"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r,r")
	(if_then_else:SI
	 (match_operator 4 "comparison_operator"
	  [(match_operand 3 "cc_register" "") (const_int 0)])
	 (not:SI (match_operand:SI 2 "s_register_operand" "r,r,r"))
	 (match_operand:SI 1 "arm_not_operand" "0,?rI,K")))]
  ""
  "@
   mvn%d4\\t%0, %2
   mov%D4\\t%0, %1\;mvn%d4\\t%0, %2
   mvn%D4\\t%0, #%B1\;mvn%d4\\t%0, %2"
[(set_attr "conds" "use")
 (set_attr "length" "4,8,8")])

(define_insn "*ifcompare_shift_move"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(if_then_else:SI
	 (match_operator 6 "comparison_operator"
	  [(match_operand:SI 4 "s_register_operand" "r,r")
	   (match_operand:SI 5 "arm_add_operand" "rIL,rIL")])
	 (match_operator:SI 7 "shift_operator"
	  [(match_operand:SI 2 "s_register_operand" "r,r")
	   (match_operand:SI 3 "arm_rhs_operand" "rM,rM")])
	 (match_operand:SI 1 "arm_not_operand" "0,?rIK")))
   (clobber (reg:CC 24))]
  ""
  "#"
[(set_attr "conds" "clob")
 (set_attr "length" "8,12")])

(define_insn "*if_shift_move"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r,r")
	(if_then_else:SI
	 (match_operator 5 "comparison_operator"
	  [(match_operand 6 "cc_register" "") (const_int 0)])
	 (match_operator:SI 4 "shift_operator"
	  [(match_operand:SI 2 "s_register_operand" "r,r,r")
	   (match_operand:SI 3 "arm_rhs_operand" "rM,rM,rM")])
	 (match_operand:SI 1 "arm_not_operand" "0,?rI,K")))]
  ""
  "@
   mov%d5\\t%0, %2%S4
   mov%D5\\t%0, %1\;mov%d5\\t%0, %2%S4
   mvn%D5\\t%0, #%B1\;mov%d5\\t%0, %2%S4"
[(set_attr "conds" "use")
 (set_attr "length" "4,8,8")])

(define_insn "*ifcompare_move_shift"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(if_then_else:SI
	 (match_operator 6 "comparison_operator"
	  [(match_operand:SI 4 "s_register_operand" "r,r")
	   (match_operand:SI 5 "arm_add_operand" "rIL,rIL")])
	 (match_operand:SI 1 "arm_not_operand" "0,?rIK")
	 (match_operator:SI 7 "shift_operator"
	  [(match_operand:SI 2 "s_register_operand" "r,r")
	   (match_operand:SI 3 "arm_rhs_operand" "rM,rM")])))
   (clobber (reg:CC 24))]
  ""
  "#"
[(set_attr "conds" "clob")
 (set_attr "length" "8,12")])

(define_insn "*if_move_shift"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r,r")
	(if_then_else:SI
	 (match_operator 5 "comparison_operator"
	  [(match_operand 6 "cc_register" "") (const_int 0)])
	 (match_operand:SI 1 "arm_not_operand" "0,?rI,K")
	 (match_operator:SI 4 "shift_operator"
	  [(match_operand:SI 2 "s_register_operand" "r,r,r")
	   (match_operand:SI 3 "arm_rhs_operand" "rM,rM,rM")])))]
  ""
  "@
   mov%D5\\t%0, %2%S4
   mov%d5\\t%0, %1\;mov%D5\\t%0, %2%S4
   mvn%d5\\t%0, #%B1\;mov%D5\\t%0, %2%S4"
[(set_attr "conds" "use")
 (set_attr "length" "4,8,8")])

(define_insn "*ifcompare_shift_shift"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(if_then_else:SI
	 (match_operator 7 "comparison_operator"
	  [(match_operand:SI 5 "s_register_operand" "r")
	   (match_operand:SI 6 "arm_add_operand" "rIL")])
	 (match_operator:SI 8 "shift_operator"
	  [(match_operand:SI 1 "s_register_operand" "r")
	   (match_operand:SI 2 "arm_rhs_operand" "rM")])
	 (match_operator:SI 9 "shift_operator"
	  [(match_operand:SI 3 "s_register_operand" "r")
	   (match_operand:SI 4 "arm_rhs_operand" "rM")])))
   (clobber (reg:CC 24))]
  ""
  "#"
[(set_attr "conds" "clob")
 (set_attr "length" "12")])

(define_insn "*if_shift_shift"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(if_then_else:SI
	 (match_operator 5 "comparison_operator"
	  [(match_operand 8 "cc_register" "") (const_int 0)])
	 (match_operator:SI 6 "shift_operator"
	  [(match_operand:SI 1 "s_register_operand" "r")
	   (match_operand:SI 2 "arm_rhs_operand" "rM")])
	 (match_operator:SI 7 "shift_operator"
	  [(match_operand:SI 3 "s_register_operand" "r")
	   (match_operand:SI 4 "arm_rhs_operand" "rM")])))]
  ""
  "mov%d5\\t%0, %1%S6\;mov%D5\\t%0, %3%S7"
[(set_attr "conds" "use")
 (set_attr "length" "8")])

(define_insn "*ifcompare_not_arith"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(if_then_else:SI
	 (match_operator 6 "comparison_operator"
	  [(match_operand:SI 4 "s_register_operand" "r")
	   (match_operand:SI 5 "arm_add_operand" "rIL")])
	 (not:SI (match_operand:SI 1 "s_register_operand" "r"))
	 (match_operator:SI 7 "shiftable_operator"
	  [(match_operand:SI 2 "s_register_operand" "r")
	   (match_operand:SI 3 "arm_rhs_operand" "rI")])))
   (clobber (reg:CC 24))]
  ""
  "#"
[(set_attr "conds" "clob")
 (set_attr "length" "12")])

(define_insn "*if_not_arith"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(if_then_else:SI
	 (match_operator 5 "comparison_operator"
	  [(match_operand 4 "cc_register" "") (const_int 0)])
	 (not:SI (match_operand:SI 1 "s_register_operand" "r"))
	 (match_operator:SI 6 "shiftable_operator"
	  [(match_operand:SI 2 "s_register_operand" "r")
	   (match_operand:SI 3 "arm_rhs_operand" "rI")])))]
  ""
  "mvn%d5\\t%0, %1\;%I6%D5\\t%0, %2, %3"
[(set_attr "conds" "use")
 (set_attr "length" "8")])

(define_insn "*ifcompare_arith_not"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(if_then_else:SI
	 (match_operator 6 "comparison_operator"
	  [(match_operand:SI 4 "s_register_operand" "r")
	   (match_operand:SI 5 "arm_add_operand" "rIL")])
	 (match_operator:SI 7 "shiftable_operator"
	  [(match_operand:SI 2 "s_register_operand" "r")
	   (match_operand:SI 3 "arm_rhs_operand" "rI")])
	 (not:SI (match_operand:SI 1 "s_register_operand" "r"))))
   (clobber (reg:CC 24))]
  ""
  "#"
[(set_attr "conds" "clob")
 (set_attr "length" "12")])

(define_insn "*if_arith_not"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(if_then_else:SI
	 (match_operator 5 "comparison_operator"
	  [(match_operand 4 "cc_register" "") (const_int 0)])
	 (match_operator:SI 6 "shiftable_operator"
	  [(match_operand:SI 2 "s_register_operand" "r")
	   (match_operand:SI 3 "arm_rhs_operand" "rI")])
	 (not:SI (match_operand:SI 1 "s_register_operand" "r"))))]
  ""
  "mvn%D5\\t%0, %1\;%I6%d5\\t%0, %2, %3"
[(set_attr "conds" "use")
 (set_attr "length" "8")])

(define_insn "*ifcompare_neg_move"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(if_then_else:SI
	 (match_operator 5 "comparison_operator"
	  [(match_operand:SI 3 "s_register_operand" "r,r")
	   (match_operand:SI 4 "arm_add_operand" "rIL,rIL")])
	 (neg:SI (match_operand:SI 2 "s_register_operand" "r,r"))
	 (match_operand:SI 1 "arm_not_operand" "0,?rIK")))
   (clobber (reg:CC 24))]
  ""
  "#"
[(set_attr "conds" "clob")
 (set_attr "length" "8,12")])

(define_insn "*if_neg_move"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r,r")
	(if_then_else:SI
	 (match_operator 4 "comparison_operator"
	  [(match_operand 3 "cc_register" "") (const_int 0)])
	 (neg:SI (match_operand:SI 2 "s_register_operand" "r,r,r"))
	 (match_operand:SI 1 "arm_not_operand" "0,?rI,K")))]
  ""
  "@
   rsb%d4\\t%0, %2, #0
   mov%D4\\t%0, %1\;rsb%d4\\t%0, %2, #0
   mvn%D4\\t%0, #%B1\;rsb%d4\\t%0, %2, #0"
[(set_attr "conds" "use")
 (set_attr "length" "4,8,8")])

(define_insn "*ifcompare_move_neg"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(if_then_else:SI
	 (match_operator 5 "comparison_operator"
	  [(match_operand:SI 3 "s_register_operand" "r,r")
	   (match_operand:SI 4 "arm_add_operand" "rIL,rIL")])
	 (match_operand:SI 1 "arm_not_operand" "0,?rIK")
	 (neg:SI (match_operand:SI 2 "s_register_operand" "r,r"))))
   (clobber (reg:CC 24))]
  ""
  "#"
[(set_attr "conds" "clob")
 (set_attr "length" "8,12")])

(define_insn "*if_move_neg"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r,r")
	(if_then_else:SI
	 (match_operator 4 "comparison_operator"
	  [(match_operand 3 "cc_register" "") (const_int 0)])
	 (match_operand:SI 1 "arm_not_operand" "0,?rI,K")
	 (neg:SI (match_operand:SI 2 "s_register_operand" "r,r,r"))))]
  ""
  "@
   rsb%D4\\t%0, %2, #0
   mov%d4\\t%0, %1\;rsb%D4\\t%0, %2, #0
   mvn%d4\\t%0, #%B1\;rsb%D4\\t%0, %2, #0"
[(set_attr "conds" "use")
 (set_attr "length" "4,8,8")])

(define_insn "*arith_adjacentmem"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(match_operator:SI 1 "shiftable_operator"
	 [(match_operand:SI 2 "memory_operand" "m")
	  (match_operand:SI 3 "memory_operand" "m")]))
   (clobber (match_scratch:SI 4 "=r"))]
  "adjacent_mem_locations (operands[2], operands[3])"
  "*
{
  rtx ldm[3];
  rtx arith[4];
  int val1 = 0, val2 = 0;

  if (REGNO (operands[0]) > REGNO (operands[4]))
    {
      ldm[1] = operands[4];
      ldm[2] = operands[0];
    }
  else
    {
      ldm[1] = operands[0];
      ldm[2] = operands[4];
    }
  if (GET_CODE (XEXP (operands[2], 0)) != REG)
    val1 = INTVAL (XEXP (XEXP (operands[2], 0), 1));
  if (GET_CODE (XEXP (operands[3], 0)) != REG)
    val2 = INTVAL (XEXP (XEXP (operands[3], 0), 1));
  arith[0] = operands[0];
  arith[3] = operands[1];
  if (val1 < val2)
    {
      arith[1] = ldm[1];
      arith[2] = ldm[2];
    }
  else
    {
      arith[1] = ldm[2];
      arith[2] = ldm[1];
    }
  if (val1 && val2)
    {
      rtx ops[3];
      ldm[0] = ops[0] = operands[4];
      ops[1] = XEXP (XEXP (operands[2], 0), 0);
      ops[2] = XEXP (XEXP (operands[2], 0), 1);
      output_add_immediate (ops);
      if (val1 < val2)
	output_asm_insn (\"ldm%?ia\\t%0, {%1, %2}\", ldm);
      else
	output_asm_insn (\"ldm%?da\\t%0, {%1, %2}\", ldm);
    }
  else if (val1)
    {
      ldm[0] = XEXP (operands[3], 0);
      if (val1 < val2)
	output_asm_insn (\"ldm%?da\\t%0, {%1, %2}\", ldm);
      else
	output_asm_insn (\"ldm%?ia\\t%0, {%1, %2}\", ldm);
    }
  else
    {
      ldm[0] = XEXP (operands[2], 0);
      if (val1 < val2)
	output_asm_insn (\"ldm%?ia\\t%0, {%1, %2}\", ldm);
      else
	output_asm_insn (\"ldm%?da\\t%0, {%1, %2}\", ldm);
    }
  output_asm_insn (\"%I3%?\\t%0, %1, %2\", arith);
  return \"\";
}
"
[(set_attr "length" "12")
 (set_attr "type" "load")])

;; the arm can support extended pre-inc instructions

;; In all these cases, we use operands 0 and 1 for the register being
;; incremented because those are the operands that local-alloc will
;; tie and these are the pair most likely to be tieable (and the ones
;; that will benefit the most).

;; We reject the frame pointer if it occurs anywhere in these patterns since
;; elimination will cause too many headaches.

(define_insn "*strqi_preinc"
  [(set (mem:QI (plus:SI (match_operand:SI 1 "s_register_operand" "%0")
			 (match_operand:SI 2 "index_operand" "rJ")))
	(match_operand:QI 3 "s_register_operand" "r"))
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(plus:SI (match_dup 1) (match_dup 2)))]
  "REGNO (operands[0]) != FRAME_POINTER_REGNUM
   && REGNO (operands[1]) != FRAME_POINTER_REGNUM
   && (GET_CODE (operands[2]) != REG
       || REGNO (operands[2]) != FRAME_POINTER_REGNUM)"
  "str%?b\\t%3, [%0, %2]!"
[(set_attr "type" "store1")])

(define_insn "*strqi_predec"
  [(set (mem:QI (minus:SI (match_operand:SI 1 "s_register_operand" "0")
			  (match_operand:SI 2 "s_register_operand" "r")))
	(match_operand:QI 3 "s_register_operand" "r"))
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(minus:SI (match_dup 1) (match_dup 2)))]
  "REGNO (operands[0]) != FRAME_POINTER_REGNUM
   && REGNO (operands[1]) != FRAME_POINTER_REGNUM
   && (GET_CODE (operands[2]) != REG
       || REGNO (operands[2]) != FRAME_POINTER_REGNUM)"
  "str%?b\\t%3, [%0, -%2]!"
[(set_attr "type" "store1")])

(define_insn "*loadqi_preinc"
  [(set (match_operand:QI 3 "s_register_operand" "=r")
	(mem:QI (plus:SI (match_operand:SI 1 "s_register_operand" "%0")
			 (match_operand:SI 2 "index_operand" "rJ"))))
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(plus:SI (match_dup 1) (match_dup 2)))]
  "REGNO (operands[0]) != FRAME_POINTER_REGNUM
   && REGNO (operands[1]) != FRAME_POINTER_REGNUM
   && (GET_CODE (operands[2]) != REG
       || REGNO (operands[2]) != FRAME_POINTER_REGNUM)"
  "ldr%?b\\t%3, [%0, %2]!"
[(set_attr "type" "load")])

(define_insn "*loadqi_predec"
  [(set (match_operand:QI 3 "s_register_operand" "=r")
	(mem:QI (minus:SI (match_operand:SI 1 "s_register_operand" "0")
			  (match_operand:SI 2 "s_register_operand" "r"))))
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(minus:SI (match_dup 1) (match_dup 2)))]
  "REGNO (operands[0]) != FRAME_POINTER_REGNUM
   && REGNO (operands[1]) != FRAME_POINTER_REGNUM
   && (GET_CODE (operands[2]) != REG
       || REGNO (operands[2]) != FRAME_POINTER_REGNUM)"
  "ldr%?b\\t%3, [%0, -%2]!"
[(set_attr "type" "load")])

(define_insn "*loadqisi_preinc"
  [(set (match_operand:SI 3 "s_register_operand" "=r")
	(zero_extend:SI
	 (mem:QI (plus:SI (match_operand:SI 1 "s_register_operand" "%0")
			  (match_operand:SI 2 "index_operand" "rJ")))))
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(plus:SI (match_dup 1) (match_dup 2)))]
  "REGNO (operands[0]) != FRAME_POINTER_REGNUM
   && REGNO (operands[1]) != FRAME_POINTER_REGNUM
   && (GET_CODE (operands[2]) != REG
       || REGNO (operands[2]) != FRAME_POINTER_REGNUM)"
  "ldr%?b\\t%3, [%0, %2]!\\t%@ z_extendqisi"
[(set_attr "type" "load")])

(define_insn "*loadqisi_predec"
  [(set (match_operand:SI 3 "s_register_operand" "=r")
	(zero_extend:SI
	 (mem:QI (minus:SI (match_operand:SI 1 "s_register_operand" "0")
			   (match_operand:SI 2 "s_register_operand" "r")))))
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(minus:SI (match_dup 1) (match_dup 2)))]
  "REGNO (operands[0]) != FRAME_POINTER_REGNUM
   && REGNO (operands[1]) != FRAME_POINTER_REGNUM
   && (GET_CODE (operands[2]) != REG
       || REGNO (operands[2]) != FRAME_POINTER_REGNUM)"
  "ldr%?b\\t%3, [%0, -%2]!\\t%@ z_extendqisi"
[(set_attr "type" "load")])

(define_insn "*strsi_preinc"
  [(set (mem:SI (plus:SI (match_operand:SI 1 "s_register_operand" "%0")
			 (match_operand:SI 2 "index_operand" "rJ")))
	(match_operand:SI 3 "s_register_operand" "r"))
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(plus:SI (match_dup 1) (match_dup 2)))]
  "REGNO (operands[0]) != FRAME_POINTER_REGNUM
   && REGNO (operands[1]) != FRAME_POINTER_REGNUM
   && (GET_CODE (operands[2]) != REG
       || REGNO (operands[2]) != FRAME_POINTER_REGNUM)"
  "str%?\\t%3, [%0, %2]!"
[(set_attr "type" "store1")])

(define_insn "*strqi_predec"
  [(set (mem:SI (minus:SI (match_operand:SI 1 "s_register_operand" "0")
			  (match_operand:SI 2 "s_register_operand" "r")))
	(match_operand:SI 3 "s_register_operand" "r"))
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(minus:SI (match_dup 1) (match_dup 2)))]
  "REGNO (operands[0]) != FRAME_POINTER_REGNUM
   && REGNO (operands[1]) != FRAME_POINTER_REGNUM
   && (GET_CODE (operands[2]) != REG
       || REGNO (operands[2]) != FRAME_POINTER_REGNUM)"
  "str%?\\t%3, [%0, -%2]!"
[(set_attr "type" "store1")])

(define_insn "*loadsi_preinc"
  [(set (match_operand:SI 3 "s_register_operand" "=r")
	(mem:SI (plus:SI (match_operand:SI 1 "s_register_operand" "%0")
			 (match_operand:SI 2 "index_operand" "rJ"))))
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(plus:SI (match_dup 1) (match_dup 2)))]
  "REGNO (operands[0]) != FRAME_POINTER_REGNUM
   && REGNO (operands[1]) != FRAME_POINTER_REGNUM
   && (GET_CODE (operands[2]) != REG
       || REGNO (operands[2]) != FRAME_POINTER_REGNUM)"
  "ldr%?\\t%3, [%0, %2]!"
[(set_attr "type" "load")])

(define_insn "*loadsi_predec"
  [(set (match_operand:SI 3 "s_register_operand" "=r")
	(mem:SI (minus:SI (match_operand:SI 1 "s_register_operand" "0")
			  (match_operand:SI 2 "s_register_operand" "r"))))
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(minus:SI (match_dup 1) (match_dup 2)))]
  "REGNO (operands[0]) != FRAME_POINTER_REGNUM
   && REGNO (operands[1]) != FRAME_POINTER_REGNUM
   && (GET_CODE (operands[2]) != REG
       || REGNO (operands[2]) != FRAME_POINTER_REGNUM)"
  "ldr%?\\t%3, [%0, -%2]!"
[(set_attr "type" "load")])

(define_insn "*loadhi_preinc"
  [(set (match_operand:HI 3 "s_register_operand" "=r")
	(mem:HI (plus:SI (match_operand:SI 1 "s_register_operand" "%0")
			 (match_operand:SI 2 "index_operand" "rJ"))))
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(plus:SI (match_dup 1) (match_dup 2)))]
  "(! BYTES_BIG_ENDIAN)
   && ! TARGET_SHORT_BY_BYTES
   && REGNO (operands[0]) != FRAME_POINTER_REGNUM
   && REGNO (operands[1]) != FRAME_POINTER_REGNUM
   && (GET_CODE (operands[2]) != REG
       || REGNO (operands[2]) != FRAME_POINTER_REGNUM)"
  "ldr%?\\t%3, [%0, %2]!\\t%@ loadhi"
[(set_attr "type" "load")])

(define_insn "*loadhi_predec"
  [(set (match_operand:HI 3 "s_register_operand" "=r")
	(mem:HI (minus:SI (match_operand:SI 1 "s_register_operand" "0")
			  (match_operand:SI 2 "s_register_operand" "r"))))
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(minus:SI (match_dup 1) (match_dup 2)))]
  "(!BYTES_BIG_ENDIAN)
   && ! TARGET_SHORT_BY_BYTES
   && REGNO (operands[0]) != FRAME_POINTER_REGNUM
   && REGNO (operands[1]) != FRAME_POINTER_REGNUM
   && (GET_CODE (operands[2]) != REG
       || REGNO (operands[2]) != FRAME_POINTER_REGNUM)"
  "ldr%?\\t%3, [%0, -%2]!\\t%@ loadhi"
[(set_attr "type" "load")])

(define_insn "*strqi_shiftpreinc"
  [(set (mem:QI (plus:SI (match_operator:SI 2 "shift_operator"
			  [(match_operand:SI 3 "s_register_operand" "r")
			   (match_operand:SI 4 "const_shift_operand" "n")])
			 (match_operand:SI 1 "s_register_operand" "0")))
	(match_operand:QI 5 "s_register_operand" "r"))
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(plus:SI (match_op_dup 2 [(match_dup 3)	(match_dup 4)])
		 (match_dup 1)))]
  "REGNO (operands[0]) != FRAME_POINTER_REGNUM
   && REGNO (operands[1]) != FRAME_POINTER_REGNUM
   && REGNO (operands[3]) != FRAME_POINTER_REGNUM"
  "str%?b\\t%5, [%0, %3%S2]!"
[(set_attr "type" "store1")])

(define_insn "*strqi_shiftpredec"
  [(set (mem:QI (minus:SI (match_operand:SI 1 "s_register_operand" "0")
			  (match_operator:SI 2 "shift_operator"
			   [(match_operand:SI 3 "s_register_operand" "r")
			    (match_operand:SI 4 "const_shift_operand" "n")])))
	(match_operand:QI 5 "s_register_operand" "r"))
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(minus:SI (match_dup 1) (match_op_dup 2 [(match_dup 3)
						 (match_dup 4)])))]
  "REGNO (operands[0]) != FRAME_POINTER_REGNUM
   && REGNO (operands[1]) != FRAME_POINTER_REGNUM
   && REGNO (operands[3]) != FRAME_POINTER_REGNUM"
  "str%?b\\t%5, [%0, -%3%S2]!"
[(set_attr "type" "store1")])

(define_insn "*loadqi_shiftpreinc"
  [(set (match_operand:QI 5 "s_register_operand" "=r")
	(mem:QI (plus:SI (match_operator:SI 2 "shift_operator"
			  [(match_operand:SI 3 "s_register_operand" "r")
			   (match_operand:SI 4 "const_shift_operand" "n")])
			 (match_operand:SI 1 "s_register_operand" "0"))))
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(plus:SI (match_op_dup 2 [(match_dup 3)	(match_dup 4)])
		 (match_dup 1)))]
  "REGNO (operands[0]) != FRAME_POINTER_REGNUM
   && REGNO (operands[1]) != FRAME_POINTER_REGNUM
   && REGNO (operands[3]) != FRAME_POINTER_REGNUM"
  "ldr%?b\\t%5, [%0, %3%S2]!"
[(set_attr "type" "load")])

(define_insn "*loadqi_shiftpredec"
  [(set (match_operand:QI 5 "s_register_operand" "=r")
	(mem:QI (minus:SI (match_operand:SI 1 "s_register_operand" "0")
			  (match_operator:SI 2 "shift_operator"
			   [(match_operand:SI 3 "s_register_operand" "r")
			    (match_operand:SI 4 "const_shift_operand" "n")]))))
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(minus:SI (match_dup 1) (match_op_dup 2 [(match_dup 3)
						 (match_dup 4)])))]
  "REGNO (operands[0]) != FRAME_POINTER_REGNUM
   && REGNO (operands[1]) != FRAME_POINTER_REGNUM
   && REGNO (operands[3]) != FRAME_POINTER_REGNUM"
  "ldr%?b\\t%5, [%0, -%3%S2]!"
[(set_attr "type" "load")])

(define_insn "*strsi_shiftpreinc"
  [(set (mem:SI (plus:SI (match_operator:SI 2 "shift_operator"
			  [(match_operand:SI 3 "s_register_operand" "r")
			   (match_operand:SI 4 "const_shift_operand" "n")])
			 (match_operand:SI 1 "s_register_operand" "0")))
	(match_operand:SI 5 "s_register_operand" "r"))
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(plus:SI (match_op_dup 2 [(match_dup 3)	(match_dup 4)])
		 (match_dup 1)))]
  "REGNO (operands[0]) != FRAME_POINTER_REGNUM
   && REGNO (operands[1]) != FRAME_POINTER_REGNUM
   && REGNO (operands[3]) != FRAME_POINTER_REGNUM"
  "str%?\\t%5, [%0, %3%S2]!"
[(set_attr "type" "store1")])

(define_insn "*strsi_shiftpredec"
  [(set (mem:SI (minus:SI (match_operand:SI 1 "s_register_operand" "0")
			  (match_operator:SI 2 "shift_operator"
			   [(match_operand:SI 3 "s_register_operand" "r")
			    (match_operand:SI 4 "const_shift_operand" "n")])))
	(match_operand:SI 5 "s_register_operand" "r"))
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(minus:SI (match_dup 1) (match_op_dup 2 [(match_dup 3)
						 (match_dup 4)])))]
  "REGNO (operands[0]) != FRAME_POINTER_REGNUM
   && REGNO (operands[1]) != FRAME_POINTER_REGNUM
   && REGNO (operands[3]) != FRAME_POINTER_REGNUM"
  "str%?\\t%5, [%0, -%3%S2]!"
[(set_attr "type" "store1")])

(define_insn "*loadqi_shiftpreinc"
  [(set (match_operand:SI 5 "s_register_operand" "=r")
	(mem:SI (plus:SI (match_operator:SI 2 "shift_operator"
			  [(match_operand:SI 3 "s_register_operand" "r")
			   (match_operand:SI 4 "const_shift_operand" "n")])
			 (match_operand:SI 1 "s_register_operand" "0"))))
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(plus:SI (match_op_dup 2 [(match_dup 3) (match_dup 4)])
		 (match_dup 1)))]
  "REGNO (operands[0]) != FRAME_POINTER_REGNUM
   && REGNO (operands[1]) != FRAME_POINTER_REGNUM
   && REGNO (operands[3]) != FRAME_POINTER_REGNUM"
  "ldr%?\\t%5, [%0, %3%S2]!"
[(set_attr "type" "load")])

(define_insn "*loadqi_shiftpredec"
  [(set (match_operand:SI 5 "s_register_operand" "=r")
	(mem:SI (minus:SI (match_operand:SI 1 "s_register_operand" "0")
			  (match_operator:SI 2 "shift_operator"
			   [(match_operand:SI 3 "s_register_operand" "r")
			    (match_operand:SI 4 "const_shift_operand" "n")]))))
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(minus:SI (match_dup 1) (match_op_dup 2 [(match_dup 3)
						 (match_dup 4)])))]
  "REGNO (operands[0]) != FRAME_POINTER_REGNUM
   && REGNO (operands[1]) != FRAME_POINTER_REGNUM
   && REGNO (operands[3]) != FRAME_POINTER_REGNUM"
  "ldr%?\\t%5, [%0, -%3%S2]!"
[(set_attr "type" "load")])

(define_insn "*loadhi_shiftpreinc"
  [(set (match_operand:HI 5 "s_register_operand" "=r")
	(mem:HI (plus:SI (match_operator:SI 2 "shift_operator"
			  [(match_operand:SI 3 "s_register_operand" "r")
			   (match_operand:SI 4 "const_shift_operand" "n")])
			 (match_operand:SI 1 "s_register_operand" "0"))))
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(plus:SI (match_op_dup 2 [(match_dup 3)	(match_dup 4)])
		 (match_dup 1)))]
  "(! BYTES_BIG_ENDIAN)
   && ! TARGET_SHORT_BY_BYTES
   && REGNO (operands[0]) != FRAME_POINTER_REGNUM
   && REGNO (operands[1]) != FRAME_POINTER_REGNUM
   && REGNO (operands[3]) != FRAME_POINTER_REGNUM"
  "ldr%?\\t%5, [%0, %3%S2]!\\t%@ loadhi"
[(set_attr "type" "load")])

(define_insn "*loadhi_shiftpredec"
  [(set (match_operand:HI 5 "s_register_operand" "=r")
	(mem:HI (minus:SI (match_operand:SI 1 "s_register_operand" "0")
			  (match_operator:SI 2 "shift_operator"
			   [(match_operand:SI 3 "s_register_operand" "r")
			    (match_operand:SI 4 "const_shift_operand" "n")]))))
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(minus:SI (match_dup 1) (match_op_dup 2 [(match_dup 3)
						 (match_dup 4)])))]
  "(! BYTES_BIG_ENDIAN)
   && ! TARGET_SHORT_BY_BYTES
   && REGNO (operands[0]) != FRAME_POINTER_REGNUM
   && REGNO (operands[1]) != FRAME_POINTER_REGNUM
   && REGNO (operands[3]) != FRAME_POINTER_REGNUM"
  "ldr%?\\t%5, [%0, -%3%S2]!\\t%@ loadhi"
[(set_attr "type" "load")])

; It can also support extended post-inc expressions, but combine doesn't
; try these....
; It doesn't seem worth adding peepholes for anything but the most common
; cases since, unlike combine, the increment must immediately follow the load
; for this pattern to match.
; When loading we must watch to see that the base register isn't trampled by
; the load.  In such cases this isn't a post-inc expression.

(define_peephole
  [(set (mem:QI (match_operand:SI 0 "s_register_operand" "+r"))
	(match_operand:QI 2 "s_register_operand" "r"))
   (set (match_dup 0)
	(plus:SI (match_dup 0) (match_operand:SI 1 "index_operand" "rJ")))]
  ""
  "str%?b\\t%2, [%0], %1")

(define_peephole
  [(set (match_operand:QI 0 "s_register_operand" "=r")
	(mem:QI (match_operand:SI 1 "s_register_operand" "+r")))
   (set (match_dup 1)
	(plus:SI (match_dup 1) (match_operand:SI 2 "index_operand" "rJ")))]
  "REGNO(operands[0]) != REGNO(operands[1])
   && (GET_CODE (operands[2]) != REG
       || REGNO(operands[0]) != REGNO (operands[2]))"
  "ldr%?b\\t%0, [%1], %2")

(define_peephole
  [(set (mem:SI (match_operand:SI 0 "s_register_operand" "+r"))
	(match_operand:SI 2 "s_register_operand" "r"))
   (set (match_dup 0)
	(plus:SI (match_dup 0) (match_operand:SI 1 "index_operand" "rJ")))]
  ""
  "str%?\\t%2, [%0], %1")

(define_peephole
  [(set (match_operand:HI 0 "s_register_operand" "=r")
	(mem:HI (match_operand:SI 1 "s_register_operand" "+r")))
   (set (match_dup 1)
	(plus:SI (match_dup 1) (match_operand:SI 2 "index_operand" "rJ")))]
  "(! BYTES_BIG_ENDIAN)
   && ! TARGET_SHORT_BY_BYTES
   && REGNO(operands[0]) != REGNO(operands[1])
   && (GET_CODE (operands[2]) != REG
       || REGNO(operands[0]) != REGNO (operands[2]))"
  "ldr%?\\t%0, [%1], %2\\t%@ loadhi")

(define_peephole
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(mem:SI (match_operand:SI 1 "s_register_operand" "+r")))
   (set (match_dup 1)
	(plus:SI (match_dup 1) (match_operand:SI 2 "index_operand" "rJ")))]
  "REGNO(operands[0]) != REGNO(operands[1])
   && (GET_CODE (operands[2]) != REG
       || REGNO(operands[0]) != REGNO (operands[2]))"
  "ldr%?\\t%0, [%1], %2")

(define_peephole
  [(set (mem:QI (plus:SI (match_operand:SI 0 "s_register_operand" "+r")
			 (match_operand:SI 1 "index_operand" "rJ")))
	(match_operand:QI 2 "s_register_operand" "r"))
   (set (match_dup 0) (plus:SI (match_dup 0) (match_dup 1)))]
  ""
  "str%?b\\t%2, [%0, %1]!")

(define_peephole
  [(set (mem:QI (plus:SI (match_operator:SI 4 "shift_operator"
			  [(match_operand:SI 0 "s_register_operand" "r")
			   (match_operand:SI 1 "const_int_operand" "n")])
			 (match_operand:SI 2 "s_register_operand" "+r")))
	(match_operand:QI 3 "s_register_operand" "r"))
   (set (match_dup 2) (plus:SI (match_op_dup 4 [(match_dup 0) (match_dup 1)])
			       (match_dup 2)))]
  ""
  "str%?b\\t%3, [%2, %0%S4]!")

; This pattern is never tried by combine, so do it as a peephole

(define_peephole
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(match_operand:SI 1 "s_register_operand" "r"))
   (set (reg:CC 24)
	(compare:CC (match_dup 1) (const_int 0)))]
  ""
  "sub%?s\\t%0, %1, #0"
[(set_attr "conds" "set")])

; Peepholes to spot possible load- and store-multiples, if the ordering is
; reversed, check that the memory references aren't volatile.

(define_peephole
  [(set (match_operand:SI 0 "s_register_operand" "=r")
        (match_operand:SI 4 "memory_operand" "m"))
   (set (match_operand:SI 1 "s_register_operand" "=r")
        (match_operand:SI 5 "memory_operand" "m"))
   (set (match_operand:SI 2 "s_register_operand" "=r")
        (match_operand:SI 6 "memory_operand" "m"))
   (set (match_operand:SI 3 "s_register_operand" "=r")
        (match_operand:SI 7 "memory_operand" "m"))]
  "load_multiple_sequence (operands, 4, NULL, NULL, NULL)"
  "*
  return emit_ldm_seq (operands, 4);
")

(define_peephole
  [(set (match_operand:SI 0 "s_register_operand" "=r")
        (match_operand:SI 3 "memory_operand" "m"))
   (set (match_operand:SI 1 "s_register_operand" "=r")
        (match_operand:SI 4 "memory_operand" "m"))
   (set (match_operand:SI 2 "s_register_operand" "=r")
        (match_operand:SI 5 "memory_operand" "m"))]
  "load_multiple_sequence (operands, 3, NULL, NULL, NULL)"
  "*
  return emit_ldm_seq (operands, 3);
")

(define_peephole
  [(set (match_operand:SI 0 "s_register_operand" "=r")
        (match_operand:SI 2 "memory_operand" "m"))
   (set (match_operand:SI 1 "s_register_operand" "=r")
        (match_operand:SI 3 "memory_operand" "m"))]
  "load_multiple_sequence (operands, 2, NULL, NULL, NULL)"
  "*
  return emit_ldm_seq (operands, 2);
")

(define_peephole
  [(set (match_operand:SI 4 "memory_operand" "=m")
        (match_operand:SI 0 "s_register_operand" "r"))
   (set (match_operand:SI 5 "memory_operand" "=m")
        (match_operand:SI 1 "s_register_operand" "r"))
   (set (match_operand:SI 6 "memory_operand" "=m")
        (match_operand:SI 2 "s_register_operand" "r"))
   (set (match_operand:SI 7 "memory_operand" "=m")
        (match_operand:SI 3 "s_register_operand" "r"))]
  "store_multiple_sequence (operands, 4, NULL, NULL, NULL)"
  "*
  return emit_stm_seq (operands, 4);
")

(define_peephole
  [(set (match_operand:SI 3 "memory_operand" "=m")
        (match_operand:SI 0 "s_register_operand" "r"))
   (set (match_operand:SI 4 "memory_operand" "=m")
        (match_operand:SI 1 "s_register_operand" "r"))
   (set (match_operand:SI 5 "memory_operand" "=m")
        (match_operand:SI 2 "s_register_operand" "r"))]
  "store_multiple_sequence (operands, 3, NULL, NULL, NULL)"
  "*
  return emit_stm_seq (operands, 3);
")

(define_peephole
  [(set (match_operand:SI 2 "memory_operand" "=m")
        (match_operand:SI 0 "s_register_operand" "r"))
   (set (match_operand:SI 3 "memory_operand" "=m")
        (match_operand:SI 1 "s_register_operand" "r"))]
  "store_multiple_sequence (operands, 2, NULL, NULL, NULL)"
  "*
  return emit_stm_seq (operands, 2);
")

;; A call followed by return can be replaced by restoring the regs and
;; jumping to the subroutine, provided we aren't passing the address of
;; any of our local variables.  If we call alloca then this is unsafe
;; since restoring the frame frees the memory, which is not what we want.
;; Sometimes the return might have been targeted by the final prescan:
;; if so then emit a proper return insn as well.
;; Unfortunately, if the frame pointer is required, we don't know if the
;; current function has any implicit stack pointer adjustments that will 
;; be restored by the return: we can't therefore do a tail call.
;; Another unfortunate that we can't handle is if current_function_args_size
;; is non-zero: in this case elimination of the argument pointer assumed
;; that lr was pushed onto the stack, so eliminating upsets the offset
;; calculations.

(define_peephole
  [(parallel [(call (mem:SI (match_operand:SI 0 "" "X"))
			  (match_operand:SI 1 "general_operand" "g"))
		    (clobber (reg:SI 14))])
   (return)]
  "(GET_CODE (operands[0]) == SYMBOL_REF && USE_RETURN_INSN(FALSE)
    && !get_frame_size () && !current_function_calls_alloca
    && !frame_pointer_needed && !current_function_args_size)"
  "*
{
  extern rtx arm_target_insn;
  extern int arm_ccfsm_state;

  if (arm_ccfsm_state && arm_target_insn && INSN_DELETED_P (arm_target_insn))
  {
    arm_current_cc = ARM_INVERSE_CONDITION_CODE (arm_current_cc);
    output_return_instruction (NULL, TRUE, FALSE);
    arm_ccfsm_state = 0;
    arm_target_insn = NULL;
  }

  output_return_instruction (NULL, FALSE, FALSE);
  return NEED_PLT_GOT ? \"b%?\\t%a0(PLT)\" : \"b%?\\t%a0\";
}"
[(set_attr "type" "call")
 (set_attr "length" "8")])

(define_peephole
  [(parallel [(set (match_operand 0 "s_register_operand" "=rf")
		   (call (mem:SI (match_operand:SI 1 "" "X"))
			 (match_operand:SI 2 "general_operand" "g")))
	      (clobber (reg:SI 14))])
   (return)]
  "(GET_CODE (operands[1]) == SYMBOL_REF && USE_RETURN_INSN(FALSE)
    && !get_frame_size () && !current_function_calls_alloca
    && !frame_pointer_needed && !current_function_args_size)"
  "*
{
  extern rtx arm_target_insn;
  extern int arm_ccfsm_state;

  if (arm_ccfsm_state && arm_target_insn && INSN_DELETED_P (arm_target_insn))
  {
    arm_current_cc = ARM_INVERSE_CONDITION_CODE (arm_current_cc);
    output_return_instruction (NULL, TRUE, FALSE);
    arm_ccfsm_state = 0;
    arm_target_insn = NULL;
  }

  output_return_instruction (NULL, FALSE, FALSE);
  return NEED_PLT_GOT ? \"b%?\\t%a1(PLT)\" : \"b%?\\t%a1\";
}"
[(set_attr "type" "call")
 (set_attr "length" "8")])

;; As above but when this function is not void, we must be returning the
;; result of the called subroutine.

(define_peephole
  [(parallel [(set (match_operand 0 "s_register_operand" "=rf")
		   (call (mem:SI (match_operand:SI 1 "" "X"))
			 (match_operand:SI 2 "general_operand" "g")))
	      (clobber (reg:SI 14))])
   (use (match_dup 0))
   (return)]
  "(GET_CODE (operands[1]) == SYMBOL_REF && USE_RETURN_INSN(FALSE)
    && !get_frame_size () && !current_function_calls_alloca
    && !frame_pointer_needed && !current_function_args_size)"
  "*
{
  extern rtx arm_target_insn;
  extern int arm_ccfsm_state;

  if (arm_ccfsm_state && arm_target_insn && INSN_DELETED_P (arm_target_insn))
  {
    arm_current_cc = ARM_INVERSE_CONDITION_CODE (arm_current_cc);
    output_return_instruction (NULL, TRUE, FALSE);
    arm_ccfsm_state = 0;
    arm_target_insn = NULL;
  }

  output_return_instruction (NULL, FALSE, FALSE);
  return \"b%?\\t%a1\";
}"
[(set_attr "type" "call")
 (set_attr "length" "8")])

(define_split
  [(set (match_operand:SI 0 "s_register_operand" "")
	(and:SI (ge:SI (match_operand:SI 1 "s_register_operand" "")
		       (const_int 0))
		(neg:SI (match_operator:SI 2 "comparison_operator"
			 [(match_operand:SI 3 "s_register_operand" "")
			  (match_operand:SI 4 "arm_rhs_operand" "")]))))
   (clobber (match_operand:SI 5 "s_register_operand" ""))]
  ""
  [(set (match_dup 5) (not:SI (ashiftrt:SI (match_dup 1) (const_int 31))))
   (set (match_dup 0) (and:SI (match_op_dup 2 [(match_dup 3) (match_dup 4)])
			      (match_dup 5)))]
  "")

;; This split can be used because CC_Z mode implies that the following
;; branch will be an equality, or an unsigned inequality, so the sign
;; extension is not needed.

(define_split
  [(set (reg:CC_Z 24)
	(compare:CC_Z
	 (ashift:SI (subreg:SI (match_operand:QI 0 "memory_operand" "") 0)
		    (const_int 24))
	 (match_operand 1 "const_int_operand" "")))
   (clobber (match_scratch:SI 2 ""))]
  "((unsigned HOST_WIDE_INT) INTVAL (operands[1]))
   == (((unsigned HOST_WIDE_INT) INTVAL (operands[1])) >> 24) << 24"
  [(set (match_dup 2) (zero_extend:SI (match_dup 0)))
   (set (reg:CC 24) (compare:CC (match_dup 2) (match_dup 1)))]
  "
  operands[1] = GEN_INT (((unsigned long) INTVAL (operands[1])) >> 24);
")

(define_expand "prologue"
  [(clobber (const_int 0))]
  ""
  "
  arm_expand_prologue ();
  DONE;
")

;; This split is only used during output to reduce the number of patterns
;; that need assembler instructions adding to them.  We allowed the setting
;; of the conditions to be implicit during rtl generation so that
;; the conditional compare patterns would work.  However this conflicts to
;; some extent with the conditional data operations, so we have to split them
;; up again here.

(define_split
  [(set (match_operand:SI 0 "s_register_operand" "")
	(if_then_else:SI (match_operator 1 "comparison_operator"
			  [(match_operand 2 "" "") (match_operand 3 "" "")])
			 (match_operand 4 "" "")
			 (match_operand 5 "" "")))
   (clobber (reg:CC 24))]
  "reload_completed"
  [(set (match_dup 6) (match_dup 7))
   (set (match_dup 0) 
	(if_then_else:SI (match_op_dup 1 [(match_dup 6) (const_int 0)])
			 (match_dup 4)
			 (match_dup 5)))]
  "
{
  enum machine_mode mode = SELECT_CC_MODE (GET_CODE (operands[1]), operands[2],
					   operands[3]);

  operands[6] = gen_rtx_REG (mode, 24);
  operands[7] = gen_rtx_COMPARE (mode, operands[2], operands[3]);
}
")

;; The next two patterns occur when an AND operation is followed by a
;; scc insn sequence 

(define_insn "*sign_extract_onebit"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(sign_extract:SI (match_operand:SI 1 "s_register_operand" "r")
			 (const_int 1)
			 (match_operand:SI 2 "const_int_operand" "n")))]
  ""
  "*
  operands[2] = GEN_INT (1 << INTVAL (operands[2]));
  output_asm_insn (\"ands\\t%0, %1, %2\", operands);
  return \"mvnne\\t%0, #0\";
"
[(set_attr "conds" "clob")
 (set_attr "length" "8")])

(define_insn "*not_signextract_onebit"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(not:SI
	 (sign_extract:SI (match_operand:SI 1 "s_register_operand" "r")
			  (const_int 1)
			  (match_operand:SI 2 "const_int_operand" "n"))))]
  ""
  "*
  operands[2] = GEN_INT (1 << INTVAL (operands[2]));
  output_asm_insn (\"tst\\t%1, %2\", operands);
  output_asm_insn (\"mvneq\\t%0, #0\", operands);
  return \"movne\\t%0, #0\";
"
[(set_attr "conds" "clob")
 (set_attr "length" "12")])

;; Push multiple registers to the stack.  The first register is in the
;; unspec part of the insn; subsequent registers are in parallel (use ...)
;; expressions.
(define_insn "*push_multi"
  [(match_parallel 2 "multi_register_push"
    [(set (match_operand:BLK 0 "memory_operand" "=m")
	  (unspec:BLK [(match_operand:SI 1 "s_register_operand" "r")] 2))])]
  ""
  "*
{
  char pattern[100];
  int i;
  extern int lr_save_eliminated;

  if (lr_save_eliminated)
    {
      if (XVECLEN (operands[2], 0) > 1)
	abort ();
      return \"\";
    }
  strcpy (pattern, \"stmfd\\t%m0!, {%1\");
  for (i = 1; i < XVECLEN (operands[2], 0); i++)
    {
      strcat (pattern, \", %|\");
      strcat (pattern, reg_names[REGNO (XEXP (XVECEXP (operands[2], 0, i),
					      0))]);
    }
  strcat (pattern, \"}\");
  output_asm_insn (pattern, operands);
  return \"\";
}"
[(set_attr "type" "store4")])

;; Similarly for the floating point registers
(define_insn "*push_fp_multi"
  [(match_parallel 2 "multi_register_push"
    [(set (match_operand:BLK 0 "memory_operand" "=m")
	  (unspec:BLK [(match_operand:XF 1 "f_register_operand" "f")] 2))])]
  ""
  "*
{
  char pattern[100];

  sprintf (pattern, \"sfmfd\\t%%1, %d, [%%m0]!\", XVECLEN (operands[2], 0));
  output_asm_insn (pattern, operands);
  return \"\";
}"
[(set_attr "type" "f_store")])

;; Special patterns for dealing with the constant pool

(define_insn "consttable_4"
  [(unspec_volatile [(match_operand 0 "" "")] 2)]
  ""
  "*
{
  making_const_table = TRUE;
  switch (GET_MODE_CLASS (GET_MODE (operands[0])))
    {
    case MODE_FLOAT:
    {
      union real_extract u;
      bcopy ((char *) &CONST_DOUBLE_LOW (operands[0]), (char *) &u, sizeof u);
      assemble_real (u.d, GET_MODE (operands[0]));
      break;
    }
    default:
      assemble_integer (operands[0], 4, 1);
      break;
    }
  return \"\";
}"
[(set_attr "length" "4")])

(define_insn "consttable_8"
  [(unspec_volatile [(match_operand 0 "" "")] 3)]
  ""
  "*
{
  making_const_table = TRUE;
  switch (GET_MODE_CLASS (GET_MODE (operands[0])))
    {
    case MODE_FLOAT:
    {
      union real_extract u;
      bcopy ((char *) &CONST_DOUBLE_LOW (operands[0]), (char *) &u, sizeof u);
      assemble_real (u.d, GET_MODE (operands[0]));
      break;
    }
    default:
      assemble_integer (operands[0], 8, 1);
      break;
    }
  return \"\";
}"
[(set_attr "length" "8")])

(define_insn "consttable_end"
  [(unspec_volatile [(const_int 0)] 4)]
  ""
  "*
  making_const_table = FALSE;
  return \"\";
")

(define_insn "align_4"
  [(unspec_volatile [(const_int 0)] 5)]
  ""
  "*
  assemble_align (32);
  return \"\";
")
