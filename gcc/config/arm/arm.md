;;- Machine description for Advanced RISC Machines' ARM for GNU compiler
;;  Copyright (C) 1991, 1993, 1994 Free Software Foundation, Inc.
;;  Contributed by Pieter `Tiggr' Schoenmakers (rcpieter@win.tue.nl)
;;             and Martin Simmons (@harleqn.co.uk).
;;  More major hacks by Richard Earnshaw (rwe11@cl.cam.ac.uk)

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
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

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

;; Attributes

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

; NOCOND means that the condition codes are niether altered nor affect the
; output of this insn

(define_attr "conds" "use,set,clob,jump_clob,nocond"
	(const_string "nocond"))

; CPU attribute is used to determine whether condition codes are clobbered
; by a call insn: on the arm6 they are if in 32-bit addressing mode; on the
; arm2 and arm3 the condition codes are restored by the return.

(define_attr "cpu" "arm2,arm3,arm6" (const (symbol_ref "arm_cpu_attr")))

; Floating Point Unit.  If we only have floating point emulation, then there
; is no point in scheduling the floating point insns.  (Well, for best
; performance we should try and group them together).

(define_attr "fpu" "fpa,fpe" (const (symbol_ref "arm_fpu_attr")))

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
	"normal,block,float,fdivx,fdivd,fdivs,fmul,ffmul,farith,ffarith,float_em,f_load,f_store,f_mem_r,r_mem_f,f_2_r,r_2_f,call,load,store1,store2,store3,store4" 
	(const_string "normal"))

(define_attr "write_conflict" "no,yes"
  (if_then_else (eq_attr "type"
		 "block,float_em,f_load,f_store,f_mem_r,r_mem_f,call,load")
		(const_string "yes")
		(const_string "no")))

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

(define_function_unit "write_buf" 1 2 (eq_attr "type" "store1") 3 3
	[(eq_attr "write_conflict" "yes")])
(define_function_unit "write_buf" 1 2 (eq_attr "type" "store2") 5 5
	[(eq_attr "write_conflict" "yes")])
(define_function_unit "write_buf" 1 2 (eq_attr "type" "store3") 7 7
	[(eq_attr "write_conflict" "yes")])
(define_function_unit "write_buf" 1 2 (eq_attr "type" "store4") 9 9
	[(eq_attr "write_conflict" "yes")])
(define_function_unit "write_buf" 1 2 (eq_attr "type" "r_mem_f") 3 3
	[(eq_attr "write_conflict" "yes")])

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
  "adds\\t%0, %1, %2\;adc\\t%R0, %R1, %R2"
[(set_attr "conds" "clob")
 (set_attr "length" "8")])

(define_insn ""
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(plus:DI (sign_extend:DI
		  (match_operand:SI 1 "s_register_operand" "r,r"))
		 (match_operand:DI 2 "s_register_operand" "r,0")))
   (clobber (reg:CC 24))]
  ""
  "adds\\t%0, %2, %1\;adc\\t%R0, %R2, %1, asr #31"
[(set_attr "conds" "clob")
 (set_attr "length" "8")])

(define_insn ""
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(plus:DI (zero_extend:DI
		  (match_operand:SI 1 "s_register_operand" "r,r"))
		 (match_operand:DI 2 "s_register_operand" "r,0")))
   (clobber (reg:CC 24))]
  ""
  "adds\\t%0, %2, %1\;adc\\t%R0, %R2, #0"
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

(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=r,r,r")
	(plus:SI (match_operand:SI 1 "s_register_operand" "r,r,r")
		 (match_operand:SI 2 "reg_or_int_operand" "rI,L,?n")))]
  ""
  "@
   add%?\\t%0, %1, %2
   sub%?\\t%0, %1, #%n2
   #"
[(set_attr "length" "4,4,16")])

(define_insn ""
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

(define_insn ""
  [(set (reg:CC 24)
	(compare:CC (match_operand:SI 1 "s_register_operand" "r,r")
		    (neg:SI (match_operand:SI 2 "arm_add_operand" "rI,L"))))
   (set (match_operand:SI 0 "s_register_operand" "=r,r")
	(plus:SI (match_dup 1) (match_dup 2)))]
  ""
  "@
   add%?s\\t%0, %1, %2
   sub%?s\\t%0, %1, #%n2"
[(set_attr "conds" "set")])

(define_insn "incscc"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
        (plus:SI (match_operator:SI 2 "comparison_operator"
                    [(reg 24) (const_int 0)])
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
		 (match_operand:SI 2 "immediate_operand" "n")))]
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
  ""
  "@
   adf%?s\\t%0, %1, %2
   suf%?s\\t%0, %1, #%N2"
[(set_attr "type" "farith")])

(define_insn "adddf3"
  [(set (match_operand:DF 0 "s_register_operand" "=f,f")
	(plus:DF (match_operand:DF 1 "s_register_operand" "f,f")
		 (match_operand:DF 2 "fpu_add_operand" "fG,H")))]
  ""
  "@
   adf%?d\\t%0, %1, %2
   suf%?d\\t%0, %1, #%N2"
[(set_attr "type" "farith")])

(define_insn ""
  [(set (match_operand:DF 0 "s_register_operand" "=f,f")
	(plus:DF (float_extend:DF
		  (match_operand:SF 1 "s_register_operand" "f,f"))
		 (match_operand:DF 2 "fpu_add_operand" "fG,H")))]
  ""
  "@
   adf%?d\\t%0, %1, %2
   suf%?d\\t%0, %1, #%N2"
[(set_attr "type" "farith")])

(define_insn ""
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(plus:DF (match_operand:DF 1 "s_register_operand" "f")
		 (float_extend:DF
		  (match_operand:SF 2 "s_register_operand" "f"))))]
  ""
  "adf%?d\\t%0, %1, %2"
[(set_attr "type" "farith")])

(define_insn ""
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(plus:DF (float_extend:DF 
		  (match_operand:SF 1 "s_register_operand" "f"))
		 (float_extend:DF
		  (match_operand:SF 2 "s_register_operand" "f"))))]
  ""
  "adf%?d\\t%0, %1, %2"
[(set_attr "type" "farith")])

(define_insn "addxf3"
  [(set (match_operand:XF 0 "s_register_operand" "=f,f")
	(plus:XF (match_operand:XF 1 "s_register_operand" "f,f")
		 (match_operand:XF 2 "fpu_add_operand" "fG,H")))]
  "ENABLE_XF_PATTERNS"
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
  "subs\\t%0, %1, %2\;sbc\\t%R0, %R1, %R2"
[(set_attr "conds" "clob")
 (set_attr "length" "8")])

(define_insn ""
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(minus:DI (match_operand:DI 1 "s_register_operand" "?r,0")
		  (zero_extend:DI
		   (match_operand:SI 2 "s_register_operand" "r,r"))))
   (clobber (reg:CC 24))]
  ""
  "subs\\t%0, %1, %2\;sbc\\t%R0, %R1, #0"
[(set_attr "conds" "clob")
 (set_attr "length" "8")])

(define_insn ""
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(minus:DI (match_operand:DI 1 "s_register_operand" "r,0")
		  (sign_extend:DI
		   (match_operand:SI 2 "s_register_operand" "r,r"))))
   (clobber (reg:CC 24))]
  ""
  "subs\\t%0, %1, %2\;sbc\\t%R0, %R1, %2, asr #31"
[(set_attr "conds" "clob")
 (set_attr "length" "8")])

(define_insn ""
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(minus:DI (zero_extend:DI
		   (match_operand:SI 2 "s_register_operand" "r,r"))
		  (match_operand:DI 1 "s_register_operand" "?r,0")))
   (clobber (reg:CC 24))]
  ""
  "rsbs\\t%0, %1, %2\;rsc\\t%R0, %R1, #0"
[(set_attr "conds" "clob")
 (set_attr "length" "8")])

(define_insn ""
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(minus:DI (sign_extend:DI
		   (match_operand:SI 2 "s_register_operand" "r,r"))
		  (match_operand:DI 1 "s_register_operand" "?r,0")))
   (clobber (reg:CC 24))]
  ""
  "rsbs\\t%0, %1, %2\;rsc\\t%R0, %R1, %2, asr #31"
[(set_attr "conds" "clob")
 (set_attr "length" "8")])

(define_insn ""
  [(set (match_operand:DI 0 "s_register_operand" "=r")
	(minus:DI (zero_extend:DI
		   (match_operand:SI 1 "s_register_operand" "r"))
		  (zero_extend:DI
		   (match_operand:SI 2 "s_register_operand" "r"))))
   (clobber (reg:CC 24))]
  ""
  "subs\\t%0, %1, %2\;rsc\\t%R0, %1, %1"
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

(define_insn ""
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

(define_insn ""
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
                   [(reg 24) (const_int 0)])))]
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
  ""
  "@
   suf%?s\\t%0, %1, %2
   rsf%?s\\t%0, %2, %1"
[(set_attr "type" "farith")])

(define_insn "subdf3"
  [(set (match_operand:DF 0 "s_register_operand" "=f,f")
	(minus:DF (match_operand:DF 1 "fpu_rhs_operand" "f,G")
		  (match_operand:DF 2 "fpu_rhs_operand" "fG,f")))]
  ""
  "@
   suf%?d\\t%0, %1, %2
   rsf%?d\\t%0, %2, %1"
[(set_attr "type" "farith")])

(define_insn ""
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(minus:DF (float_extend:DF
		   (match_operand:SF 1 "s_register_operand" "f"))
		  (match_operand:DF 2 "fpu_rhs_operand" "fG")))]
  ""
  "suf%?d\\t%0, %1, %2"
[(set_attr "type" "farith")])

(define_insn ""
  [(set (match_operand:DF 0 "s_register_operand" "=f,f")
	(minus:DF (match_operand:DF 1 "fpu_rhs_operand" "f,G")
		  (float_extend:DF
		   (match_operand:SF 2 "s_register_operand" "f,f"))))]
  ""
  "@
   suf%?d\\t%0, %1, %2
   rsf%?d\\t%0, %2, %1"
[(set_attr "type" "farith")])

(define_insn ""
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(minus:DF (float_extend:DF
		   (match_operand:SF 1 "s_register_operand" "f"))
		  (float_extend:DF
		   (match_operand:SF 2 "s_register_operand" "f"))))]
  ""
  "suf%?d\\t%0, %1, %2"
[(set_attr "type" "farith")])

(define_insn "subxf3"
  [(set (match_operand:XF 0 "s_register_operand" "=f,f")
	(minus:XF (match_operand:XF 1 "fpu_rhs_operand" "f,G")
		  (match_operand:XF 2 "fpu_rhs_operand" "fG,f")))]
  "ENABLE_XF_PATTERNS"
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
  "mul%?\\t%0, %2, %1")

(define_insn ""
  [(set (reg:CC_NOOV 24)
	(compare:CC_NOOV (mult:SI
			  (match_operand:SI 2 "s_register_operand" "r,r")
			  (match_operand:SI 1 "s_register_operand" "%?r,0"))
			 (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=&r,&r")
	(mult:SI (match_dup 2) (match_dup 1)))]
  ""
  "mul%?s\\t%0, %2, %1"
[(set_attr "conds" "set")])

(define_insn ""
  [(set (reg:CC_NOOV 24)
	(compare:CC_NOOV (mult:SI
			  (match_operand:SI 2 "s_register_operand" "r,r")
			  (match_operand:SI 1 "s_register_operand" "%?r,0"))
			 (const_int 0)))
   (clobber (match_scratch:SI 0 "=&r,&r"))]
  ""
  "mul%?s\\t%0, %2, %1"
[(set_attr "conds" "set")])

;; Unnamed templates to match MLA instruction.

(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=&r,&r,&r,&r")
	(plus:SI
	  (mult:SI (match_operand:SI 2 "s_register_operand" "r,r,r,r")
		   (match_operand:SI 1 "s_register_operand" "%r,0,r,0"))
	  (match_operand:SI 3 "s_register_operand" "?r,r,0,0")))]
  ""
  "mla%?\\t%0, %2, %1, %3")

(define_insn ""
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
[(set_attr "conds" "set")])

(define_insn ""
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
[(set_attr "conds" "set")])

(define_insn "mulsf3"
  [(set (match_operand:SF 0 "s_register_operand" "=f")
	(mult:SF (match_operand:SF 1 "s_register_operand" "f")
		 (match_operand:SF 2 "fpu_rhs_operand" "fG")))]
  ""
  "fml%?s\\t%0, %1, %2"
[(set_attr "type" "ffmul")])

(define_insn "muldf3"
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(mult:DF (match_operand:DF 1 "s_register_operand" "f")
		 (match_operand:DF 2 "fpu_rhs_operand" "fG")))]
  ""
  "muf%?d\\t%0, %1, %2"
[(set_attr "type" "fmul")])

(define_insn ""
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(mult:DF (float_extend:DF
		  (match_operand:SF 1 "s_register_operand" "f"))
		 (match_operand:DF 2 "fpu_rhs_operand" "fG")))]
  ""
  "muf%?d\\t%0, %1, %2"
[(set_attr "type" "fmul")])

(define_insn ""
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(mult:DF (match_operand:DF 1 "s_register_operand" "f")
		 (float_extend:DF
		  (match_operand:SF 2 "s_register_operand" "f"))))]
  ""
  "muf%?d\\t%0, %1, %2"
[(set_attr "type" "fmul")])

(define_insn ""
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(mult:DF (float_extend:DF
		  (match_operand:SF 1 "s_register_operand" "f"))
		 (float_extend:DF
		  (match_operand:SF 2 "s_register_operand" "f"))))]
  ""
  "muf%?d\\t%0, %1, %2"
[(set_attr "type" "fmul")])

(define_insn "mulxf3"
  [(set (match_operand:XF 0 "s_register_operand" "=f")
	(mult:XF (match_operand:XF 1 "s_register_operand" "f")
		 (match_operand:XF 2 "fpu_rhs_operand" "fG")))]
  "ENABLE_XF_PATTERNS"
  "muf%?e\\t%0, %1, %2"
[(set_attr "type" "fmul")])

;; Division insns

(define_insn "divsf3"
  [(set (match_operand:SF 0 "s_register_operand" "=f,f")
	(div:SF (match_operand:SF 1 "fpu_rhs_operand" "f,G")
		(match_operand:SF 2 "fpu_rhs_operand" "fG,f")))]
  ""
  "@
   fdv%?s\\t%0, %1, %2
   frd%?s\\t%0, %2, %1"
[(set_attr "type" "fdivs")])

(define_insn "divdf3"
  [(set (match_operand:DF 0 "s_register_operand" "=f,f")
	(div:DF (match_operand:DF 1 "fpu_rhs_operand" "f,G")
		(match_operand:DF 2 "fpu_rhs_operand" "fG,f")))]
  ""
  "@
   dvf%?d\\t%0, %1, %2
   rdf%?d\\t%0, %2, %1"
[(set_attr "type" "fdivd")])

(define_insn ""
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(div:DF (float_extend:DF
		 (match_operand:SF 1 "s_register_operand" "f"))
		(match_operand:DF 2 "fpu_rhs_operand" "fG")))]
  ""
  "dvf%?d\\t%0, %1, %2"
[(set_attr "type" "fdivd")])

(define_insn ""
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(div:DF (match_operand:DF 1 "fpu_rhs_operand" "fG")
		(float_extend:DF
		 (match_operand:SF 2 "s_register_operand" "f"))))]
  ""
  "rdf%?d\\t%0, %2, %1"
[(set_attr "type" "fdivd")])

(define_insn ""
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(div:DF (float_extend:DF
		 (match_operand:SF 1 "s_register_operand" "f"))
		(float_extend:DF
		 (match_operand:SF 2 "s_register_operand" "f"))))]
  ""
  "dvf%?d\\t%0, %1, %2"
[(set_attr "type" "fdivd")])

(define_insn "divxf3"
  [(set (match_operand:XF 0 "s_register_operand" "=f,f")
	(div:XF (match_operand:XF 1 "fpu_rhs_operand" "f,G")
		(match_operand:XF 2 "fpu_rhs_operand" "fG,f")))]
  "ENABLE_XF_PATTERNS"
  "@
   dvf%?e\\t%0, %1, %2
   rdf%?e\\t%0, %2, %1"
[(set_attr "type" "fdivx")])

;; Modulo insns

(define_insn "modsf3"
  [(set (match_operand:SF 0 "s_register_operand" "=f")
	(mod:SF (match_operand:SF 1 "s_register_operand" "f")
		(match_operand:SF 2 "fpu_rhs_operand" "fG")))]
  ""
  "rmf%?s\\t%0, %1, %2"
[(set_attr "type" "fdivs")])

(define_insn "moddf3"
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(mod:DF (match_operand:DF 1 "s_register_operand" "f")
		(match_operand:DF 2 "fpu_rhs_operand" "fG")))]
  ""
  "rmf%?d\\t%0, %1, %2"
[(set_attr "type" "fdivd")])

(define_insn ""
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(mod:DF (float_extend:DF
		 (match_operand:SF 1 "s_register_operand" "f"))
		(match_operand:DF 2 "fpu_rhs_operand" "fG")))]
  ""
  "rmf%?d\\t%0, %1, %2"
[(set_attr "type" "fdivd")])

(define_insn ""
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(mod:DF (match_operand:DF 1 "s_register_operand" "f")
		(float_extend:DF
		 (match_operand:SF 2 "s_register_operand" "f"))))]
  ""
  "rmf%?d\\t%0, %1, %2"
[(set_attr "type" "fdivd")])

(define_insn ""
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(mod:DF (float_extend:DF
		 (match_operand:SF 1 "s_register_operand" "f"))
		(float_extend:DF
		 (match_operand:SF 2 "s_register_operand" "f"))))]
  ""
  "rmf%?d\\t%0, %1, %2"
[(set_attr "type" "fdivd")])

(define_insn "modxf3"
  [(set (match_operand:XF 0 "s_register_operand" "=f")
	(mod:XF (match_operand:XF 1 "s_register_operand" "f")
		(match_operand:XF 2 "fpu_rhs_operand" "fG")))]
  "ENABLE_XF_PATTERNS"
  "rmf%?e\\t%0, %1, %2"
[(set_attr "type" "fdivx")])

;; Boolean and,ior,xor insns

(define_insn "anddi3"
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(and:DI (match_operand:DI 1 "s_register_operand" "%0,0")
		(match_operand:DI 2 "s_register_operand" "r,0")))]
  ""
  "and%?\\t%0, %1, %2\;and%?\\t%R0, %R1, %R2"
[(set_attr "length" "8")])

(define_insn ""
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(and:DI (zero_extend:DI
		 (match_operand:SI 2 "s_register_operand" "r,r"))
		(match_operand:DI 1 "s_register_operand" "?r,0")))]
  ""
  "and%?\\t%0, %1, %2\;mov%?\\t%R0, #0"
[(set_attr "length" "8")])

(define_insn ""
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(and:DI (sign_extend:DI
		 (match_operand:SI 2 "s_register_operand" "r,r"))
		(match_operand:DI 1 "s_register_operand" "?r,0")))]
  ""
  "and%?\\t%0, %1, %2\;and%?\\t%R0, %R1, %2, asr #31"
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

(define_insn ""
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

(define_insn ""
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

(define_insn ""
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

(define_insn ""
  [(set (reg:CC_NOOV 24)
	(compare:CC_NOOV (zero_extract:SI
			  (match_operand:SI 0 "s_register_operand" "r")
		 	  (match_operand:SI 1 "immediate_operand" "n")
			  (match_operand:SI 2 "immediate_operand" "n"))
			 (const_int 0)))]
  "INTVAL (operands[2]) >= 0 && INTVAL (operands[2]) < 32
   && INTVAL (operands[1]) > 0 
   && INTVAL (operands[1]) + (INTVAL (operands[2]) & 1) <= 8
   && INTVAL (operands[1]) + INTVAL (operands[2]) <= 32"
  "*
{
  unsigned int mask = 0;
  int cnt = INTVAL (operands[1]);
  
  while (cnt--)
    mask = (mask << 1) | 1;
  operands[1] = GEN_INT (mask << INTVAL (operands[2]));
  output_asm_insn (\"tst%?\\t%0, %1\", operands);
  return \"\";
}
"
[(set_attr "conds" "set")])

(define_insn ""
  [(set (reg:CC_NOOV 24)
	(compare:CC_NOOV (zero_extract:SI
			  (match_operand:QI 0 "memory_operand" "m")
		 	  (match_operand 1 "immediate_operand" "n")
			  (match_operand 2 "immediate_operand" "n"))
			 (const_int 0)))
   (clobber (match_scratch:QI 3 "=r"))]
  "INTVAL (operands[2]) >= 0 && INTVAL (operands[2]) < 8
   && INTVAL (operands[1]) > 0 && INTVAL (operands[1]) <= 8"
  "*
{
  unsigned int mask = 0;
  int cnt = INTVAL (operands[1]);
  
  while (cnt--)
    mask = (mask << 1) | 1;
  operands[1] = GEN_INT (mask << INTVAL (operands[2]));
  output_asm_insn (\"ldr%?b\\t%3, %0\", operands);
  output_asm_insn (\"tst%?\\t%3, %1\", operands);
  return \"\";
}
"
[(set_attr "conds" "set")
 (set_attr "length" "8")])

;; constants for op 2 will never be given to these patterns.
(define_insn ""
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(and:DI (not:DI (match_operand:DI 2 "s_register_operand" "r,0"))
		(match_operand:DI 1 "s_register_operand" "0,r")))]
  ""
  "bic%?\\t%0, %1, %2\;bic%?\\t%R0, %R1, %R2"
[(set_attr "length" "8")])
  
(define_insn ""
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(and:DI (not:DI (zero_extend:DI
			 (match_operand:SI 2 "s_register_operand" "r,r")))
		(match_operand:DI 1 "s_register_operand" "0,?r")))]
  ""
  "@
   bic%?\\t%0, %1, %2
   bic%?\\t%0, %1, %2\;mov%?\\t%R0, %R1"
[(set_attr "length" "4,8")])
  
(define_insn ""
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(and:DI (not:DI (sign_extend:DI
			 (match_operand:SI 2 "s_register_operand" "r,r")))
		(match_operand:DI 1 "s_register_operand" "?r,0")))]
  ""
  "bic%?\\t%0, %1, %2\;bic%?\\t%R0, %R1, %2, asr #31"
[(set_attr "length" "8")])
  
(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(and:SI (not:SI (match_operand:SI 2 "s_register_operand" "r"))
		(match_operand:SI 1 "s_register_operand" "r")))]
  ""
  "bic%?\\t%0, %1, %2")

(define_insn ""
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

(define_insn ""
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
  "orr%?\\t%0, %1, %2\;orr%?\\t%R0, %R1, %R2"
[(set_attr "length" "8")])

(define_insn ""
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(ior:DI (zero_extend:DI
		 (match_operand:SI 2 "s_register_operand" "r,r"))
		(match_operand:DI 1 "s_register_operand" "0,?r")))]
  ""
  "@
   orr%?\\t%0, %1, %2
   orr%?\\t%0, %1, %2\;mov%?\\t%R0, %R1"
[(set_attr "length" "4,8")])

(define_insn ""
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(ior:DI (sign_extend:DI
		 (match_operand:SI 2 "s_register_operand" "r,r"))
		(match_operand:DI 1 "s_register_operand" "?r,0")))]
  ""
  "orr%?\\t%0, %1, %2\;orr%?\\t%R0, %R1, %2, asr #31"
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

(define_insn ""
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
  
(define_insn ""
  [(set (reg:CC_NOOV 24)
	(compare:CC_NOOV (ior:SI (match_operand:SI 1 "s_register_operand" "%r")
				 (match_operand:SI 2 "arm_rhs_operand" "rI"))
			 (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(ior:SI (match_dup 1) (match_dup 2)))]
  ""
  "orr%?s\\t%0, %1, %2"
[(set_attr "conds" "set")])

(define_insn ""
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
  "eor%?\\t%0, %1, %2\;eor%?\\t%R0, %R1, %R2"
[(set_attr "length" "8")])

(define_insn ""
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(xor:DI (zero_extend:DI
		 (match_operand:SI 2 "s_register_operand" "r,r"))
		(match_operand:DI 1 "s_register_operand" "0,?r")))]
  ""
  "@
   eor%?\\t%0, %1, %2
   eor%?\\t%0, %1, %2\;mov%?\\t%R0, %R1"
[(set_attr "length" "4,8")])

(define_insn ""
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(xor:DI (sign_extend:DI
		 (match_operand:SI 2 "s_register_operand" "r,r"))
		(match_operand:DI 1 "s_register_operand" "?r,0")))]
  ""
  "eor%?\\t%0, %1, %2\;eor%?\\t%R0, %R1, %2, asr #31"
[(set_attr "length" "8")])

(define_insn "xorsi3"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(xor:SI (match_operand:SI 1 "s_register_operand" "r")
		(match_operand:SI 2 "arm_rhs_operand" "rI")))]
  ""
  "eor%?\\t%0, %1, %2")

(define_insn ""
  [(set (reg:CC_NOOV 24)
	(compare:CC_NOOV (xor:SI (match_operand:SI 1 "s_register_operand" "r")
				 (match_operand:SI 2 "arm_rhs_operand" "rI"))
			 (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(xor:SI (match_dup 1) (match_dup 2)))]
  ""
  "eor%?s\\t%0, %1, %2"
[(set_attr "conds" "set")])

(define_insn ""
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

(define_insn ""
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

(define_insn ""
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

(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(match_operator:SI 4 "shiftable_operator"
	 [(match_operator:SI 5 "minmax_operator"
	   [(match_operand:SI 2 "s_register_operand" "r,r")
	    (match_operand:SI 3 "arm_rhs_operand" "rI,rI")])
	  (match_operand:SI 1 "s_register_operand" "0,?r")]))
   (clobber (reg:CC 24))]
  ""
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

(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(match_operator:SI 3 "shift_operator"
	 [(match_operand:SI 1 "s_register_operand" "r")
	  (match_operand:SI 2 "reg_or_int_operand" "rM")]))]
  ""
  "mov%?\\t%0, %1%S3")

(define_insn ""
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

(define_insn ""
  [(set (reg:CC_NOOV 24)
	(compare:CC_NOOV (match_operator:SI 3 "shift_operator"
			  [(match_operand:SI 1 "s_register_operand" "r")
			   (match_operand:SI 2 "arm_rhs_operand" "rM")])
			 (const_int 0)))
   (clobber (match_scratch:SI 0 "=r"))]
  ""
  "mov%?s\\t%0, %1%S3"
[(set_attr "conds" "set")])

(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(not:SI (match_operator:SI 3 "shift_operator"
		 [(match_operand:SI 1 "s_register_operand" "r")
		  (match_operand:SI 2 "arm_rhs_operand" "rM")])))]
  ""
  "mvn%?\\t%0, %1%S3")

(define_insn ""
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

(define_insn ""
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
  "rsbs\\t%0, %1, #0\;rsc\\t%R0, %R1, #0"
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
  ""
  "mnf%?s\\t%0, %1"
[(set_attr "type" "ffarith")])

(define_insn "negdf2"
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(neg:DF (match_operand:DF 1 "s_register_operand" "f")))]
  ""
  "mnf%?d\\t%0, %1"
[(set_attr "type" "ffarith")])

(define_insn ""
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(neg:DF (float_extend:DF
		 (match_operand:SF 1 "s_register_operand" "f"))))]
  ""
  "mnf%?d\\t%0, %1"
[(set_attr "type" "ffarith")])

(define_insn "negxf2"
  [(set (match_operand:XF 0 "s_register_operand" "=f")
	(neg:XF (match_operand:XF 1 "s_register_operand" "f")))]
  "ENABLE_XF_PATTERNS"
  "mnf%?e\\t%0, %1"
[(set_attr "type" "ffarith")])

;; abssi2 doesn't really clobber the condition codes if a different register
;; is being set.  To keep things simple, assume during rtl manipulations that
;; it does, but tell the final scan operator the truth.  Similarly for
;; (neg (abs...))

(define_insn "abssi2"
  [(set (match_operand:SI 0 "s_register_operand" "=r,&r")
	(abs:SI (match_operand:SI 1 "s_register_operand" "0,r")))
   (clobber (reg 24))]
  ""
  "@
   cmp\\t%0, #0\;rsblt\\t%0, %0, #0
   eor%?\\t%0, %1, %1, asr #31\;sub%?\\t%0, %0, %1, asr #31"
[(set_attr "conds" "clob,*")
 (set_attr "length" "8")])

(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=r,&r")
	(neg:SI (abs:SI (match_operand:SI 1 "s_register_operand" "0,r"))))
   (clobber (reg 24))]
  ""
  "@
   cmp\\t%0, #0\;rsbgt\\t%0, %0, #0
   eor%?\\t%0, %1, %1, asr #31\;rsb%?\\t%0, %0, %1, asr #31"
[(set_attr "conds" "clob,*")
 (set_attr "length" "8")])

(define_insn "abssf2"
  [(set (match_operand:SF 0 "s_register_operand" "=f")
	 (abs:SF (match_operand:SF 1 "s_register_operand" "f")))]
  ""
  "abs%?s\\t%0, %1"
[(set_attr "type" "ffarith")])

(define_insn "absdf2"
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(abs:DF (match_operand:DF 1 "s_register_operand" "f")))]
  ""
  "abs%?d\\t%0, %1"
[(set_attr "type" "ffarith")])

(define_insn ""
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(abs:DF (float_extend:DF
		 (match_operand:SF 1 "s_register_operand" "f"))))]
  ""
  "abs%?d\\t%0, %1"
[(set_attr "type" "ffarith")])

(define_insn "absxf2"
  [(set (match_operand:XF 0 "s_register_operand" "=f")
	(abs:XF (match_operand:XF 1 "s_register_operand" "f")))]
  "ENABLE_XF_PATTERNS"
  "abs%?e\\t%0, %1"
[(set_attr "type" "ffarith")])

(define_insn "sqrtsf2"
  [(set (match_operand:SF 0 "s_register_operand" "=f")
	(sqrt:SF (match_operand:SF 1 "s_register_operand" "f")))]
  ""
  "sqt%?s\\t%0, %1"
[(set_attr "type" "float_em")])

(define_insn "sqrtdf2"
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(sqrt:DF (match_operand:DF 1 "s_register_operand" "f")))]
  ""
  "sqt%?d\\t%0, %1"
[(set_attr "type" "float_em")])

(define_insn ""
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(sqrt:DF (float_extend:DF
		  (match_operand:SF 1 "s_register_operand" "f"))))]
  ""
  "sqt%?d\\t%0, %1"
[(set_attr "type" "float_em")])

(define_insn "sqrtxf2"
  [(set (match_operand:XF 0 "s_register_operand" "=f")
	(sqrt:XF (match_operand:XF 1 "s_register_operand" "f")))]
  "ENABLE_XF_PATTERNS"
  "sqt%?e\\t%0, %1"
[(set_attr "type" "float_em")])

(define_insn "sinsf2"
  [(set (match_operand:SF 0 "s_register_operand" "=f")
	(unspec:SF [(match_operand:SF 1 "s_register_operand" "f")] 0))]
  ""
  "sin%?s\\t%0, %1"
[(set_attr "type" "float_em")])

(define_insn "sindf2"
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(unspec:DF [(match_operand:DF 1 "s_register_operand" "f")] 0))]
  ""
  "sin%?d\\t%0, %1"
[(set_attr "type" "float_em")])

(define_insn ""
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(unspec:DF [(float_extend:DF
		     (match_operand:SF 1 "s_register_operand" "f"))] 0))]
  ""
  "sin%?d\\t%0, %1"
[(set_attr "type" "float_em")])

(define_insn "sinxf2"
  [(set (match_operand:XF 0 "s_register_operand" "=f")
	(unspec:XF [(match_operand:XF 1 "s_register_operand" "f")] 0))]
  "ENABLE_XF_PATTERNS"
  "sin%?e\\t%0, %1"
[(set_attr "type" "float_em")])

(define_insn "cossf2"
  [(set (match_operand:SF 0 "s_register_operand" "=f")
	(unspec:SF [(match_operand:SF 1 "s_register_operand" "f")] 1))]
  ""
  "cos%?s\\t%0, %1"
[(set_attr "type" "float_em")])

(define_insn "cosdf2"
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(unspec:DF [(match_operand:DF 1 "s_register_operand" "f")] 1))]
  ""
  "cos%?d\\t%0, %1"
[(set_attr "type" "float_em")])

(define_insn ""
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(unspec:DF [(float_extend:DF
		     (match_operand:SF 1 "s_register_operand" "f"))] 1))]
  ""
  "cos%?d\\t%0, %1"
[(set_attr "type" "float_em")])

(define_insn "cosxf2"
  [(set (match_operand:XF 0 "s_register_operand" "=f")
	(unspec:XF [(match_operand:XF 1 "s_register_operand" "f")] 1))]
  "ENABLE_XF_PATTERNS"
  "cos%?e\\t%0, %1"
[(set_attr "type" "float_em")])

(define_insn "one_cmpldi2"
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(not:DI (match_operand:DI 1 "s_register_operand" "?r,0")))]
  ""
  "mvn%?\\t%0, %1\;mvn%?\\t%R0, %R1"
[(set_attr "length" "8")])

(define_insn "one_cmplsi2"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(not:SI (match_operand:SI 1 "s_register_operand" "r")))]
  ""
  "mvn%?\\t%0, %1")

(define_insn ""
  [(set (reg:CC_NOOV 24)
	(compare:CC_NOOV (not:SI (match_operand:SI 1 "s_register_operand" "r"))
			 (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(not:SI (match_dup 1)))]
  ""
  "mvn%?s\\t%0, %1"
[(set_attr "conds" "set")])

(define_insn ""
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
  ""
  "flt%?s\\t%0, %1"
[(set_attr "type" "r_2_f")])

(define_insn "floatsidf2"
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(float:DF (match_operand:SI 1 "s_register_operand" "r")))]
  ""
  "flt%?d\\t%0, %1"
[(set_attr "type" "r_2_f")])

(define_insn "floatsixf2"
  [(set (match_operand:XF 0 "s_register_operand" "=f")
	(float:XF (match_operand:SI 1 "s_register_operand" "r")))]
  "ENABLE_XF_PATTERNS"
  "flt%?e\\t%0, %1"
[(set_attr "type" "r_2_f")])

(define_insn "fix_truncsfsi2"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(fix:SI (match_operand:SF 1 "s_register_operand" "f")))]
  ""
  "fix%?z\\t%0, %1"
[(set_attr "type" "f_2_r")])

(define_insn "fix_truncdfsi2"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(fix:SI (match_operand:DF 1 "s_register_operand" "f")))]
  ""
  "fix%?z\\t%0, %1"
[(set_attr "type" "f_2_r")])

(define_insn "fix_truncxfsi2"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(fix:SI (match_operand:XF 1 "s_register_operand" "f")))]
  "ENABLE_XF_PATTERNS"
  "fix%?z\\t%0, %1"
[(set_attr "type" "f_2_r")])

;; Truncation insns

(define_insn "truncdfsf2"
  [(set (match_operand:SF 0 "s_register_operand" "=f")
	(float_truncate:SF
	 (match_operand:DF 1 "s_register_operand" "f")))]
  ""
  "mvf%?s\\t%0, %1"
[(set_attr "type" "ffarith")])

(define_insn "truncxfsf2"
  [(set (match_operand:SF 0 "s_register_operand" "=f")
	(float_truncate:SF
	 (match_operand:XF 1 "s_register_operand" "f")))]
  "ENABLE_XF_PATTERNS"
  "mvf%?s\\t%0, %1"
[(set_attr "type" "ffarith")])

(define_insn "truncxfdf2"
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(float_truncate:DF
	 (match_operand:XF 1 "s_register_operand" "f")))]
  "ENABLE_XF_PATTERNS"
  "mvf%?d\\t%0, %1"
[(set_attr "type" "ffarith")])

;; Zero and sign extension instructions.

(define_insn "zero_extendsidi2"
  [(set (match_operand:DI 0 "s_register_operand" "=r")
        (zero_extend:DI (match_operand:SI 1 "s_register_operand" "r")))]
  ""
  "*
  if (REGNO (operands[1]) != REGNO (operands[0]))
    output_asm_insn (\"mov%?\\t%0, %1\", operands);
  return \"mov%?\\t%R0, #0\";
"
[(set_attr "length" "8")])

(define_insn "zero_extendqidi2"
  [(set (match_operand:DI 0 "s_register_operand" "=r,r")
	(zero_extend:DI (match_operand:QI 1 "nonimmediate_operand" "r,m")))]
  ""
  "@
   and%?\\t%0, %1, #255\;mov%?\\t%R0, #0
   ldr%?b\\t%0, %1\;mov%?\\t%R0, #0"
[(set_attr "length" "8")
 (set_attr "type" "*,load")])

(define_insn "extendsidi2"
  [(set (match_operand:DI 0 "s_register_operand" "=r")
        (sign_extend:DI (match_operand:SI 1 "s_register_operand" "r")))]
  ""
  "*
  if (REGNO (operands[1]) != REGNO (operands[0]))
    output_asm_insn (\"mov%?\\t%0, %1\", operands);
  return \"mov%?\\t%R0, %0, asr #31\";
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

(define_insn ""
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

(define_insn ""
  [(set (reg:CC_NOOV 24)
	(compare:CC_NOOV (match_operand:QI 0 "s_register_operand" "r")
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
  [(set (match_dup 2) (zero_extend:SI (mem:QI (match_operand:HI 1 "" ""))))
   (set (match_dup 3)
	(zero_extend:SI (mem:QI (plus:SI (match_dup 1) (const_int 1)))))
   (set (match_dup 6) (ashift:SI (match_dup 4) (const_int 24)))
   (set (match_operand:SI 0 "" "")
	(ior:SI (ashiftrt:SI (match_dup 6) (const_int 16)) (match_dup 5)))]
  ""
  "
  operands[0] = gen_lowpart (SImode, operands[0]);
  operands[1] = copy_to_mode_reg (SImode, XEXP (operands[1], 0));
  operands[2] = gen_reg_rtx (SImode);
  operands[3] = gen_reg_rtx (SImode);
  operands[6] = gen_reg_rtx (SImode);

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
")

(define_expand "extendqihi2"
  [(set (match_dup 2)
	(ashift:SI (match_operand:QI 1 "s_register_operand" "")
		   (const_int 24)))
   (set (match_operand:HI 0 "s_register_operand" "")
	(ashiftrt:SI (match_dup 2)
		     (const_int 24)))]
  ""
  "
{ operands[0] = gen_lowpart (SImode, operands[0]);
  operands[1] = gen_lowpart (SImode, operands[1]);
  operands[2] = gen_reg_rtx (SImode); }")

(define_expand "extendqisi2"
  [(set (match_dup 2)
	(ashift:SI (match_operand:QI 1 "s_register_operand" "")
		   (const_int 24)))
   (set (match_operand:SI 0 "s_register_operand" "")
	(ashiftrt:SI (match_dup 2)
		     (const_int 24)))]
  ""
  "
{ operands[1] = gen_lowpart (SImode, operands[1]);
  operands[2] = gen_reg_rtx (SImode); }")

(define_insn "extendsfdf2"
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(float_extend:DF (match_operand:SF 1 "s_register_operand" "f")))]
  ""
  "mvf%?d\\t%0, %1"
[(set_attr "type" "ffarith")])

(define_insn "extendsfxf2"
  [(set (match_operand:XF 0 "s_register_operand" "=f")
	(float_extend:XF (match_operand:SF 1 "s_register_operand" "f")))]
  "ENABLE_XF_PATTERNS"
  "mvf%?e\\t%0, %1"
[(set_attr "type" "ffarith")])

(define_insn "extenddfxf2"
  [(set (match_operand:XF 0 "s_register_operand" "=f")
	(float_extend:XF (match_operand:DF 1 "s_register_operand" "f")))]
  "ENABLE_XF_PATTERNS"
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
  [(set (match_operand:DI 0 "di_operand" "=r,r,r,o<>,r")
	(match_operand:DI 1 "di_operand" "rIK,n,o<>,r,F"))]
  ""
  "*
  return (output_move_double (operands));
"
[(set_attr "length" "8,32,8,8,32")
 (set_attr "type" "*,*,load,store2,*")])

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
")

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=r,r,r,r,m,r,r")
	(match_operand:SI 1 "general_operand"  "R,m,K,rI,r,S,?n"))]
  "(register_operand (operands[0], SImode)
    && (GET_CODE (operands[1]) != SYMBOL_REF
	|| CONSTANT_ADDRESS_P (operands[1])))
   || register_operand (operands[1], SImode)"
  "*
  switch (which_alternative)
    {
    case 0:
      /* NB Calling get_attr_length may cause the insn to be re-extracted... */
      if (get_attr_length (insn) == 8)
	{
	  /* ... so modify the operands here.  */
	  operands[1] = XEXP (operands[1], 0);
	  output_asm_insn (\"sub%?\\t%0, %|pc, #(8 + . - %a1) & ~4095\",
			   operands);
	  output_asm_insn (\"ldr%?\\t%0, [%0, #- ((4 + . - %a1) & 4095)]\",
			   operands);
	}
      else
	{
	  /* ... and here.  */
	  operands[1] = XEXP (operands[1], 0);
	  output_asm_insn (\"ldr%?\\t%0, [%|pc, %1 - . - 8]\", operands);
	}
      return \"\";

    case 1:
      if (GET_CODE (XEXP (operands[1], 0)) == SYMBOL_REF
	  &&  CONSTANT_POOL_ADDRESS_P (XEXP (operands[1], 0)))
	abort ();
      return \"ldr%?\\t%0, %1\";

    case 3:
      return \"mov%?\\t%0, %1\";
    case 2:
      return \"mvn%?\\t%0, #%B1\";
    case 4:
      return \"str%?\\t%1, %0\";
    case 5:
      return output_load_symbol (insn, operands);
    case 6:
      return \"#\";
    }
"
[(set (attr "length")
      (cond [(eq_attr "alternative" "0")
             (if_then_else
              (gt (minus 
                   (pc)
                   (symbol_ref "const_pool_offset (XEXP (operands[1], 0))"))
                  (const_int 4087))
              (const_int 8)
              (const_int 4))
             (ior (eq_attr "alternative" "5")
		  (eq_attr "alternative" "6")) (const_int 16)]
            (const_int 4)))
 (set_attr "type" "load,load,*,*,store1,*,*")])

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

;; If copying one reg to another we can set the condition codes according to
;; its value.  Such a move is common after a return from subroutine and the
;; result is being tested against zero.

(define_insn ""
  [(set (reg:CC 24) (compare (match_operand:SI 1 "s_register_operand" "0,r")
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
;; a memory address of reg+large_const into a seperate PLUS insn, since this
;; can generate unrecognizable rtl.

(define_expand "storehi"
  [;; store the low byte
   (set (mem:QI (match_operand:SI 1 "" "")) (match_dup 3))
   ;; extract the high byte
   (set (match_dup 2)
	(ashiftrt:SI (match_operand 0 "" "") (const_int 8)))
   ;; store the high byte
   (set (mem:QI (match_dup 4))
	(subreg:QI (match_dup 2) 0))]	;explicit subreg safe
  ""
  "
{
  enum rtx_code code = GET_CODE (operands[1]);

  if ((code == PLUS || code == MINUS)
      && (GET_CODE (XEXP (operands[1], 1)) == REG
	  || GET_CODE (XEXP (operands[1], 0)) != REG))
    operands[1] = force_reg (SImode, operands[1]);
  operands[4] = plus_constant (operands[1], 1);
  operands[3] = gen_lowpart (QImode, operands[0]);
  operands[0] = gen_lowpart (SImode, operands[0]);
  operands[2] = gen_reg_rtx (SImode); 
}
")

(define_expand "storehi_bigend"
  [(set (mem:QI (match_dup 4)) (match_dup 3))
   (set (match_dup 2)
	(ashiftrt:SI (match_operand 0 "" "") (const_int 8)))
   (set (mem:QI (match_operand 1 "" ""))
	(subreg:QI (match_dup 2) 0))]
  ""
  "
{
  enum rtx_code code = GET_CODE (operands[1]);
  if ((code == PLUS || code == MINUS)
      && (GET_CODE (XEXP (operands[1], 1)) == REG
	  || GET_CODE (XEXP (operands[1], 0)) != REG))
    operands[1] = force_reg (SImode, operands[1]);

  operands[4] = plus_constant (operands[1], 1);
  operands[3] = gen_lowpart (QImode, operands[0]);
  operands[0] = gen_lowpart (SImode, operands[0]);
  operands[2] = gen_reg_rtx (SImode);
}
")

;; Subroutine to store a half word integer constant into memory.
(define_expand "storeinthi"
  [(set (mem:QI (match_operand:SI 0 "" ""))
	(subreg:QI (match_operand 1 "" "") 0))
   (set (mem:QI (match_dup 3)) (subreg:QI (match_dup 2) 0))]
  ""
  "
{
  HOST_WIDE_INT value = INTVAL (operands[1]);
  enum rtx_code code = GET_CODE (operands[0]);

  if ((code == PLUS || code == MINUS)
      && (GET_CODE (XEXP (operands[0], 1)) == REG
	  || GET_CODE (XEXP (operands[0], 0)) != REG))
  operands[0] = force_reg (SImode, operands[0]);

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

  operands[3] = plus_constant (operands[0], 1);
}
")

(define_expand "movhi"
  [(set (match_operand:HI 0 "general_operand" "")
	(match_operand:HI 1 "general_operand" ""))]
  ""
  "
{
  rtx insn;

  if (! (reload_in_progress || reload_completed))
    {
      if (GET_CODE (operands[0]) == MEM)
	{
	  if (GET_CODE (operands[1]) == CONST_INT)
	    emit_insn (gen_storeinthi (XEXP (operands[0], 0), operands[1]));
	  else
	    {
	      if (GET_CODE (operands[1]) == MEM)
		operands[1] = force_reg (HImode, operands[1]);
	      if (BYTES_BIG_ENDIAN)
		emit_insn (gen_storehi_bigend (operands[1],
					       XEXP (operands[0], 0)));
	      else
		emit_insn (gen_storehi (operands[1], XEXP (operands[0], 0)));
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
	  operands[1] = gen_rtx (SUBREG, HImode, reg, 0);
	}
      else if (TARGET_SHORT_BY_BYTES && GET_CODE (operands[1]) == MEM)
        {
	  rtx reg = gen_reg_rtx (SImode);
	  emit_insn (gen_movhi_bytes (reg, operands[1]));
	  operands[1] = gen_lowpart (HImode, reg);
	}
      else if (BYTES_BIG_ENDIAN && GET_CODE (operands[1]) == MEM)
	{
	  emit_insn (gen_movhi_bigend (operands[0], operands[1]));
	  DONE;
	}
    }
}
")

(define_expand "movhi_bytes"
  [(set (match_dup 2) (zero_extend:SI (mem:QI (match_operand:HI 1 "" ""))))
   (set (match_dup 3)
	(zero_extend:SI (mem:QI (plus:SI (match_dup 1) (const_int 1)))))
   (set (match_operand:SI 0 "" "")
	 (ior:SI (ashift:SI (match_dup 4) (const_int 8)) (match_dup 5)))]
  ""
  "
  operands[0] = gen_lowpart (SImode, operands[0]);
  operands[1] = copy_to_mode_reg (SImode, XEXP (operands[1], 0));
  operands[2] = gen_reg_rtx (SImode);
  operands[3] = gen_reg_rtx (SImode);

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

(define_insn ""
  [(set (match_operand:HI 0 "general_operand" "=r,r,r")
	(match_operand:HI 1 "general_operand"  "rI,K,m"))]
  "! BYTES_BIG_ENDIAN
   && ! TARGET_SHORT_BY_BYTES
   && (GET_CODE (operands[1]) != CONST_INT
       || const_ok_for_arm (INTVAL (operands[1]))
       || const_ok_for_arm (~INTVAL (operands[1])))"
  "@
   mov%?\\t%0, %1\\t%@ movhi
   mvn%?\\t%0, #%B1\\t%@ movhi
   ldr%?\\t%0, %1\\t%@ movhi"
[(set_attr "type" "*,*,load")])

(define_insn ""
  [(set (match_operand:HI 0 "s_register_operand" "=r,r,r")
	(match_operand:HI 1 "general_operand"  "rI,K,m"))]
  "BYTES_BIG_ENDIAN
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

(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(rotate:SI (subreg:SI (match_operand:HI 1 "memory_operand" "m") 0)
		   (const_int 16)))]
  "BYTES_BIG_ENDIAN
   && ! TARGET_SHORT_BY_BYTES"
  "ldr%?\\t%0, %1\\t%@ movhi_bigend"
[(set_attr "type" "load")])

(define_insn ""
  [(set (match_operand:HI 0 "s_register_operand" "=r,r")
	(match_operand:HI 1 "arm_rhs_operand"  "rI,K"))]
  "TARGET_SHORT_BY_BYTES"
  "@
   mov%?\\t%0, %1\\t%@ movhi
   mvn%?\\t%0, #%B1\\t%@ movhi")


(define_expand "reload_outhi"
  [(parallel [(match_operand:HI 0 "reload_memory_operand" "=o")
	      (match_operand:HI 1 "s_register_operand" "r")
	      (match_operand:SI 2 "s_register_operand" "=&r")])]
  ""
  "
  arm_reload_out_hi (operands);
  DONE;
")

(define_expand "reload_inhi"
  [(parallel [(match_operand:HI 0 "s_register_operand" "=r")
	      (match_operand:HI 1 "reload_memory_operand" "o")
	      (match_operand:SI 2 "s_register_operand" "=&r")])]
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
	  operands[1] = gen_rtx (SUBREG, QImode, reg, 0);
	}
      if (GET_CODE (operands[0]) == MEM)
	operands[1] = force_reg (QImode, operands[1]);
    }
")


(define_insn ""
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
  if (GET_CODE (operands[1]) == CONST_DOUBLE
      && ((GET_CODE (operands[0]) == REG
	   && REGNO (operands[0]) < 16)
	  || ! (const_double_rtx_ok_for_fpu (operands[1])
		|| neg_const_double_rtx_ok_for_fpu (operands[1]))))
    {
      extern int optimize;
      rtx mem = force_const_mem (SFmode, operands[1]);
      rtx addr;

      if (reload_in_progress || reload_completed)
	addr = gen_rtx (REG, SImode, REGNO (operands[0]));
      else
        addr = gen_reg_rtx (SImode);
      if (optimize == 0)
	{
	  rtx ptr = force_const_mem (SImode, XEXP (mem, 0));
	  emit_insn (gen_movsi (addr, ptr));
	}
      else
	emit_insn (gen_movsi (addr, XEXP (mem, 0)));
      operands[1] = gen_rtx (MEM, SFmode, addr);
    }
  if (GET_CODE (operands[0]) == MEM)
    operands[1] = force_reg (SFmode, operands[1]);
")

(define_insn ""
  [(set (match_operand:SF 0 "general_operand" "=f,f,f,m,f,r,r,r,m")
	(match_operand:SF 1 "general_operand" "fG,H,m,f,r,f,r,m,r"))]
  "GET_CODE (operands[0]) != MEM || register_operand (operands[1], SFmode)"
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

(define_expand "movdf"
  [(set (match_operand:DF 0 "general_operand" "")
	(match_operand:DF 1 "general_operand" ""))]
  ""
  "
  if (GET_CODE (operands[1]) == CONST_DOUBLE
      && ((GET_CODE (operands[0]) == REG
	   && REGNO (operands[0]) < 16)
	  || ! (const_double_rtx_ok_for_fpu (operands[1])
		|| neg_const_double_rtx_ok_for_fpu (operands[1]))))
    {
      extern int optimize;
      rtx mem = force_const_mem (DFmode, operands[1]);
      rtx addr;

      if (reload_in_progress || reload_completed)
	addr = gen_rtx (REG, SImode, REGNO (operands[0]));
      else
	addr = gen_reg_rtx (SImode);
      if (optimize == 0)
	{
	  rtx ptr = force_const_mem (SImode, XEXP (mem, 0));
	  emit_insn (gen_movsi (addr, ptr));
	}
      else
	emit_insn (gen_movsi (addr, XEXP (mem, 0)));
      operands[1] = gen_rtx (MEM, DFmode, addr);
    }
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
  if (GET_CODE (XEXP (operands[0], 0)) == REG)
    operands[2] = XEXP (operands[0], 0);
  else
    emit_insn (gen_addsi3 (operands[2], XEXP (XEXP (operands[0], 0), 0),
			   XEXP (XEXP (operands[0], 0), 1)));
  emit_insn (gen_rtx (SET, VOIDmode, gen_rtx (MEM, DFmode, operands[2]),
		      operands[1]));
  DONE;
")

(define_insn ""
  [(set (match_operand:DF 0 "general_operand" "=r,Q#m,r,f,f,f,f,m,!f,!r,r")
	(match_operand:DF 1 "general_operand" 
	 	"Q,r,?o,?f,!G,!H,m,f,r,f,??r"))]
  "GET_CODE (operands[0]) != MEM || register_operand (operands[1], DFmode)"
  "*
{
  rtx ops[3];

  switch (which_alternative)
    {
    case 0:
      return \"ldm%?ia\\t%m1, {%0, %R0}\\t%@ double\";

    case 1:
      return \"stm%?ia\\t%m0, {%1, %R1}\\t%@ double\";

    case 2:
      ops[0] = operands[0];
      ops[1] = XEXP (XEXP (operands[1], 0), 0);
      ops[2] = XEXP (XEXP (operands[1], 0), 1);
      if (!INTVAL (ops[2]) || const_ok_for_arm (INTVAL (ops[2])))
	output_asm_insn (\"add%?\\t%0, %1, %2\", ops);
      else
	output_asm_insn (\"sub%?\\t%0, %1, #%n2\", ops);
      return \"ldm%?ia\\t%0, {%0, %R0}\\t%@ double\";

    case 3:
    case 4:
      return \"mvf%?d\\t%0, %1\";

    case 5: return \"mnf%?d\\t%0, #%N1\";
    case 6: return \"ldf%?d\\t%0, %1\";
    case 7: return \"stf%?d\\t%1, %0\";
    case 8: return output_mov_double_fpu_from_arm (operands);
    case 9: return output_mov_double_arm_from_fpu (operands);
    case 10: return output_move_double (operands);
    }
}
"
[(set_attr "length" "4,4,8,4,4,4,4,4,8,8,8")
 (set_attr "type" 
"load,store2,load,ffarith,ffarith,ffarith,f_load,f_store,r_mem_f,f_mem_r,*")])

(define_expand "movxf"
  [(set (match_operand:XF 0 "general_operand" "")
	(match_operand:XF 1 "general_operand" ""))]
  "ENABLE_XF_PATTERNS"
  "")

;; Even when the XFmode patterns aren't enabled, we enable this after
;; reloading so that we can push floating point registers in the prologue.

(define_insn ""
  [(set (match_operand:XF 0 "general_operand" "=f,f,f,m,f,r,r")
	(match_operand:XF 1 "general_operand" "fG,H,m,f,r,f,r"))]
  "ENABLE_XF_PATTERNS || reload_completed"
  "*
  switch (which_alternative)
    {
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
                                     TRUE, FALSE);
")

;; Load multiple with write-back

(define_insn ""
  [(match_parallel 0 "load_multiple_operation"
                   [(set (match_operand:SI 1 "s_register_operand" "+r")
                         (plus:SI (match_dup 1)
                                  (match_operand:SI 2 "immediate_operand" "n")))
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

(define_insn ""
  [(match_parallel 0 "load_multiple_operation"
                   [(set (match_operand:SI 1 "s_register_operand" "=r")
                         (match_operand:SI 2 "indirect_operand" "Q"))])]
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
                                     TRUE, FALSE);
")

;; Store multiple with write-back

(define_insn ""
  [(match_parallel 0 "store_multiple_operation"
                   [(set (match_operand:SI 1 "s_register_operand" "+r")
                         (plus:SI (match_dup 1)
                                  (match_operand:SI 2 "immediate_operand" "n")))
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

(define_insn ""
  [(match_parallel 0 "store_multiple_operation"
                   [(set (match_operand:SI 2 "indirect_operand" "=Q")
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
  [(set (reg:CC 24)
	(compare:CC (match_operand:SI 0 "s_register_operand" "")
   		    (match_operand:SI 1 "arm_add_operand" "")))]
  ""
  "
{
  arm_compare_op0 = operands[0];
  arm_compare_op1 = operands[1];
  arm_compare_fp = 0;
  DONE;
}
")

(define_expand "cmpsf"
  [(set (reg:CC 24)
	(compare:CC (match_operand:SF 0 "s_register_operand" "")
		    (match_operand:SF 1 "fpu_rhs_operand" "")))]
  ""
  "
{
  arm_compare_op0 = operands[0];
  arm_compare_op1 = operands[1];
  arm_compare_fp = 1;
  DONE;
}
")

(define_expand "cmpdf"
  [(set (reg:CC 24)
	(compare:CC (match_operand:DF 0 "s_register_operand" "")
		    (match_operand:DF 1 "fpu_rhs_operand" "")))]
  ""
  "
{
  arm_compare_op0 = operands[0];
  arm_compare_op1 = operands[1];
  arm_compare_fp = 1;
  DONE;
}
")

(define_expand "cmpxf"
  [(set (reg:CC 24)
	(compare:CC (match_operand:XF 0 "s_register_operand" "")
		    (match_operand:XF 1 "fpu_rhs_operand" "")))]
  "ENABLE_XF_PATTERNS"
  "
{
  arm_compare_op0 = operands[0];
  arm_compare_op1 = operands[1];
  arm_compare_fp = 1;
  DONE;
}
")

(define_insn ""
  [(set (match_operand 0 "cc_register" "")
	(compare (match_operand:SI 1 "s_register_operand" "r,r")
		 (match_operand:SI 2 "arm_add_operand" "rI,L")))]
  ""
  "@
   cmp%?\\t%1, %2
   cmn%?\\t%1, #%n2"
[(set_attr "conds" "set")])

(define_insn ""
  [(set (match_operand 0 "cc_register" "")
	(compare (match_operand:SI 1 "s_register_operand" "r")
		 (neg:SI (match_operand:SI 2 "s_register_operand" "r"))))]
  ""
  "cmn%?\\t%1, %2"
[(set_attr "conds" "set")])

(define_insn ""
  [(set (match_operand 0 "cc_register" "")
	(compare (match_operand:SI 1 "s_register_operand" "r")
		 (match_operator:SI 2 "shift_operator"
		  [(match_operand:SI 3 "s_register_operand" "r")
		   (match_operand:SI 4 "arm_rhs_operand" "rM")])))]
  ""
  "cmp%?\\t%1, %3%S2"
[(set_attr "conds" "set")])

(define_insn ""
  [(set (match_operand 0 "cc_register" "")
	(compare (match_operand:SI 1 "s_register_operand" "r")
		 (neg:SI (match_operator:SI 2 "shift_operator"
			  [(match_operand:SI 3 "s_register_operand" "r")
			   (match_operand:SI 4 "arm_rhs_operand" "rM")]))))]
  ""
  "cmn%?\\t%1, %3%S2"
[(set_attr "conds" "set")])

(define_insn ""
  [(set (reg:CCFP 24)
	(compare:CCFP (match_operand:SF 0 "s_register_operand" "f,f")
		      (match_operand:SF 1 "fpu_add_operand" "fG,H")))]
  ""
  "@
   cmf%?\\t%0, %1
   cnf%?\\t%0, #%N1"
[(set_attr "conds" "set")
 (set_attr "type" "f_2_r")])

(define_insn ""
  [(set (reg:CCFP 24)
	(compare:CCFP (match_operand:DF 0 "s_register_operand" "f,f")
		      (match_operand:DF 1 "fpu_add_operand" "fG,H")))]
  ""
  "@
   cmf%?\\t%0, %1
   cnf%?\\t%0, #%N1"
[(set_attr "conds" "set")
 (set_attr "type" "f_2_r")])

(define_insn ""
  [(set (reg:CCFP 24)
	(compare:CCFP (float_extend:DF
		       (match_operand:SF 0 "s_register_operand" "f,f"))
		      (match_operand:DF 1 "fpu_add_operand" "fG,H")))]
  ""
  "@
   cmf%?\\t%0, %1
   cnf%?\\t%0, #%N1"
[(set_attr "conds" "set")
 (set_attr "type" "f_2_r")])

(define_insn ""
  [(set (reg:CCFP 24)
	(compare:CCFP (match_operand:DF 0 "s_register_operand" "f")
		      (float_extend:DF
		       (match_operand:SF 1 "s_register_operand" "f"))))]
  ""
  "cmf%?\\t%0, %1"
[(set_attr "conds" "set")
 (set_attr "type" "f_2_r")])

(define_insn ""
  [(set (reg:CCFP 24)
	(compare:CCFP (match_operand:XF 0 "s_register_operand" "f,f")
		      (match_operand:XF 1 "fpu_add_operand" "fG,H")))]
  "ENABLE_XF_PATTERNS"
  "@
   cmf%?\\t%0, %1
   cnf%?\\t%0, #%N1"
[(set_attr "conds" "set")
 (set_attr "type" "f_2_r")])

(define_insn ""
  [(set (reg:CCFPE 24)
	(compare:CCFPE (match_operand:SF 0 "s_register_operand" "f,f")
		       (match_operand:SF 1 "fpu_add_operand" "fG,H")))]
  ""
  "@
   cmf%?e\\t%0, %1
   cnf%?e\\t%0, #%N1"
[(set_attr "conds" "set")
 (set_attr "type" "f_2_r")])

(define_insn ""
  [(set (reg:CCFPE 24)
	(compare:CCFPE (match_operand:DF 0 "s_register_operand" "f,f")
		       (match_operand:DF 1 "fpu_add_operand" "fG,H")))]
  ""
  "@
   cmf%?e\\t%0, %1
   cnf%?e\\t%0, #%N1"
[(set_attr "conds" "set")
 (set_attr "type" "f_2_r")])

(define_insn ""
  [(set (reg:CCFPE 24)
	(compare:CCFPE (float_extend:DF
			(match_operand:SF 0 "s_register_operand" "f,f"))
		       (match_operand:DF 1 "fpu_add_operand" "fG,H")))]
  ""
  "@
   cmf%?e\\t%0, %1
   cnf%?e\\t%0, #%N1"
[(set_attr "conds" "set")
 (set_attr "type" "f_2_r")])

(define_insn ""
  [(set (reg:CCFPE 24)
	(compare:CCFPE (match_operand:DF 0 "s_register_operand" "f")
		       (float_extend:DF
			(match_operand:SF 1 "s_register_operand" "f"))))]
  ""
  "cmf%?e\\t%0, %1"
[(set_attr "conds" "set")
 (set_attr "type" "f_2_r")])

(define_insn ""
  [(set (reg:CCFPE 24)
	(compare:CCFPE (match_operand:XF 0 "s_register_operand" "f,f")
		       (match_operand:XF 1 "fpu_add_operand" "fG,H")))]
  "ENABLE_XF_PATTERNS"
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

(define_insn ""
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
  operands[1] = gen_compare_reg (EQ, arm_compare_op0, arm_compare_op1,
				 arm_compare_fp);
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
  operands[1] = gen_compare_reg (NE, arm_compare_op0, arm_compare_op1,
				 arm_compare_fp);
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
  operands[1] = gen_compare_reg (GT, arm_compare_op0, arm_compare_op1,
				 arm_compare_fp);
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
  operands[1] = gen_compare_reg (LE, arm_compare_op0, arm_compare_op1,
				 arm_compare_fp);
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
  operands[1] = gen_compare_reg (GE, arm_compare_op0, arm_compare_op1,
				 arm_compare_fp);
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
  operands[1] = gen_compare_reg (LT, arm_compare_op0, arm_compare_op1,
				 arm_compare_fp);
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
  operands[1] = gen_compare_reg (GTU, arm_compare_op0, arm_compare_op1,
				 arm_compare_fp);
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
  operands[1] = gen_compare_reg (LEU, arm_compare_op0, arm_compare_op1,
				 arm_compare_fp);
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
  operands[1] = gen_compare_reg (GEU, arm_compare_op0, arm_compare_op1,
				 arm_compare_fp);
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
  operands[1] = gen_compare_reg (LTU, arm_compare_op0, arm_compare_op1,
				 arm_compare_fp);
}
")

;; patterns to match conditional branch insns

(define_insn ""
  [(set (pc)
	(if_then_else (match_operator 1 "comparison_operator"
					[(reg 24) (const_int 0)])
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

(define_insn ""
  [(set (pc)
	(if_then_else (match_operator 1 "comparison_operator"
					[(reg 24) (const_int 0)])
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  "REVERSIBLE_CC_MODE (GET_MODE (XEXP (operands[1], 0)))"
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
  operands[1] = gen_compare_reg (EQ, arm_compare_op0, arm_compare_op1,
				 arm_compare_fp);
}
")

(define_expand "sne"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(ne:SI (match_dup 1) (const_int 0)))]
  ""
  "
{
  operands[1] = gen_compare_reg (NE, arm_compare_op0, arm_compare_op1,
				 arm_compare_fp);
}
")

(define_expand "sgt"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(gt:SI (match_dup 1) (const_int 0)))]
  ""
  "
{
  operands[1] = gen_compare_reg (GT, arm_compare_op0, arm_compare_op1,
				 arm_compare_fp);
}
")

(define_expand "sle"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(le:SI (match_dup 1) (const_int 0)))]
  ""
  "
{
  operands[1] = gen_compare_reg (LE, arm_compare_op0, arm_compare_op1,
				 arm_compare_fp);
}
")

(define_expand "sge"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(ge:SI (match_dup 1) (const_int 0)))]
  ""
  "
{
  operands[1] = gen_compare_reg (GE, arm_compare_op0, arm_compare_op1,
				 arm_compare_fp);
}
")

(define_expand "slt"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(lt:SI (match_dup 1) (const_int 0)))]
  ""
  "
{
  operands[1] = gen_compare_reg (LT, arm_compare_op0, arm_compare_op1,
				 arm_compare_fp);
}
")

(define_expand "sgtu"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(gtu:SI (match_dup 1) (const_int 0)))]
  ""
  "
{
  operands[1] = gen_compare_reg (GTU, arm_compare_op0, arm_compare_op1,
				 arm_compare_fp);
}
")

(define_expand "sleu"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(leu:SI (match_dup 1) (const_int 0)))]
  ""
  "
{
  operands[1] = gen_compare_reg (LEU, arm_compare_op0, arm_compare_op1,
				 arm_compare_fp);
}
")

(define_expand "sgeu"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(geu:SI (match_dup 1) (const_int 0)))]
  ""
  "
{
  operands[1] = gen_compare_reg (GEU, arm_compare_op0, arm_compare_op1,
				 arm_compare_fp);
}
")

(define_expand "sltu"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(ltu:SI (match_dup 1) (const_int 0)))]
  ""
  "
{
  operands[1] = gen_compare_reg (LTU, arm_compare_op0, arm_compare_op1,
				 arm_compare_fp);
}
")

(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(match_operator:SI 1 "comparison_operator" [(reg 24) (const_int 0)]))]
  ""
  "mov%D1\\t%0, #0\;mov%d1\\t%0, #1"
[(set_attr "conds" "use")
 (set_attr "length" "8")])

(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(neg:SI (match_operator:SI 1 "comparison_operator"
		 [(reg 24) (const_int 0)])))]
  ""
  "mov%D1\\t%0, #0\;mvn%d1\\t%0, #0"
[(set_attr "conds" "use")
 (set_attr "length" "8")])

(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(not:SI (match_operator:SI 1 "comparison_operator"
		 [(reg 24) (const_int 0)])))]
  ""
  "mov%D1\\t%0, #0\;mvn%d1\\t%0, #1"
[(set_attr "conds" "use")
 (set_attr "length" "8")])


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

(define_insn ""
  [(call (mem:SI (match_operand:SI 0 "s_register_operand" "r"))
         (match_operand 1 "" "g"))
   (clobber (reg:SI 14))]
  ""
  "*
  return output_call (operands);
"
[(set (attr "conds")
      (if_then_else (eq_attr "cpu" "arm6")
		    (const_string "clob")
		    (const_string "nocond")))
;; length is worst case, normally it is only two
 (set_attr "length" "12")
 (set_attr "type" "call")])

(define_insn ""
  [(call (mem:SI (match_operand 0 "memory_operand" "m"))
	 (match_operand 1 "general_operand" "g"))
   (clobber (reg:SI 14))]
  ""
  "*
  return output_call_mem (operands);
"
[(set (attr "conds")
      (if_then_else (eq_attr "cpu" "arm6")
		    (const_string "clob")
		    (const_string "nocond")))
 (set_attr "length" "12")
 (set_attr "type" "call")])

(define_expand "call_value"
  [(parallel [(set (match_operand 0 "" "=rf")
	           (call (match_operand 1 "memory_operand" "m")
		         (match_operand 2 "general_operand" "g")))
	      (clobber (reg:SI 14))])]
  ""
  "")

(define_insn ""
  [(set (match_operand 0 "" "=rf")
        (call (mem:SI (match_operand:SI 1 "s_register_operand" "r"))
	      (match_operand 2 "general_operand" "g")))
   (clobber (reg:SI 14))]
  ""
  "*
  return output_call (&operands[1]);
"
[(set (attr "conds")
      (if_then_else (eq_attr "cpu" "arm6")
		    (const_string "clob")
		    (const_string "nocond")))
 (set_attr "length" "12")
 (set_attr "type" "call")])

(define_insn ""
  [(set (match_operand 0 "" "=rf")
	(call (mem:SI (match_operand 1 "memory_operand" "m"))
	(match_operand 2 "general_operand" "g")))
   (clobber (reg:SI 14))]
  "! CONSTANT_ADDRESS_P (XEXP (operands[1], 0))"
  "*
  return output_call_mem (&operands[1]);
"
[(set (attr "conds")
      (if_then_else (eq_attr "cpu" "arm6")
		    (const_string "clob")
		    (const_string "nocond")))
 (set_attr "length" "12")
 (set_attr "type" "call")])

;; Allow calls to SYMBOL_REFs specially as they are not valid general addresses
;; The 'a' causes the operand to be treated as an address, i.e. no '#' output.

(define_insn ""
  [(call (mem:SI (match_operand:SI 0 "" "i"))
	 (match_operand:SI 1 "general_operand" "g"))
   (clobber (reg:SI 14))]
  "GET_CODE (operands[0]) == SYMBOL_REF"
  "bl%?\\t%a0"
[(set (attr "conds")
      (if_then_else (eq_attr "cpu" "arm6")
		    (const_string "clob")
		    (const_string "nocond")))
 (set_attr "type" "call")])

(define_insn ""
  [(set (match_operand 0 "s_register_operand" "=rf")
	(call (mem:SI (match_operand:SI 1 "" "i"))
	(match_operand:SI 2 "general_operand" "g")))
   (clobber (reg:SI 14))]
  "GET_CODE(operands[1]) == SYMBOL_REF"
  "bl%?\\t%a1"
[(set (attr "conds")
      (if_then_else (eq_attr "cpu" "arm6")
		    (const_string "clob")
		    (const_string "nocond")))
 (set_attr "type" "call")])

;; Often the return insn will be the same as loading from memory, so set attr
(define_insn "return"
  [(return)]
  "USE_RETURN_INSN"
  "*
{
  extern int arm_ccfsm_state;

  if (arm_ccfsm_state == 2)
  {
    arm_ccfsm_state += 2;
    return \"\";
  }
  return output_return_instruction (NULL, TRUE);
}"
[(set_attr "type" "load")])

(define_insn ""
  [(set (pc)
        (if_then_else (match_operator 0 "comparison_operator"
		       [(reg 24) (const_int 0)])
                      (return)
                      (pc)))]
  "USE_RETURN_INSN"
  "*
{
  extern int arm_ccfsm_state;

  if (arm_ccfsm_state == 2)
  {
    arm_ccfsm_state += 2;
    return \"\";
  }
  return output_return_instruction (operands[0], TRUE);
}"
[(set_attr "conds" "use")
 (set_attr "type" "load")])

(define_insn ""
  [(set (pc)
        (if_then_else (match_operator 0 "comparison_operator"
		       [(reg 24) (const_int 0)])
                      (pc)
		      (return)))]
  "USE_RETURN_INSN"
  "*
{
  extern int arm_ccfsm_state;

  if (arm_ccfsm_state == 2)
  {
    arm_ccfsm_state += 2;
    return \"\";
  }
  return output_return_instruction 
	(gen_rtx (reverse_condition (GET_CODE (operands[0])),
		  GET_MODE (operands[0]), XEXP (operands[0], 0),
		  XEXP (operands[0], 1)),
	 TRUE);
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

(define_insn "tablejump"
  [(set (pc)
	(match_operand:SI 0 "s_register_operand" "r"))
   (use (label_ref (match_operand 1 "" "")))]
  ""
  "mov%?\\t%|pc, %0\\t%@ table jump, label %l1")

(define_insn ""
  [(set (pc)
	(match_operand:SI 0 "memory_operand" "m"))
   (use (label_ref (match_operand 1 "" "")))]
  ""
  "ldr%?\\t%|pc, %0\\t%@ table jump, label %l1"
[(set_attr "type" "load")])

(define_insn "indirect_jump"
  [(set (pc)
	(match_operand:SI 0 "s_register_operand" "r"))]
  ""
  "mov%?\\t%|pc, %0\\t%@ indirect jump")

(define_insn ""
  [(set (pc)
	(match_operand:SI 0 "memory_operand" "m"))]
  ""
  "ldr%?\\t%|pc, %0\\t%@ indirect jump"
[(set_attr "type" "load")])

;; Misc insns

(define_insn "nop"
  [(const_int 0)]
  ""
  "mov%?\\tr0, r0\\t%@ nop")

;; Patterns to allow combination of arithmetic, cond code and shifts

(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=r")
        (match_operator:SI 1 "shiftable_operator"
          [(match_operator:SI 3 "shift_operator"
             [(match_operand:SI 4 "s_register_operand" "r")
              (match_operand:SI 5 "reg_or_int_operand" "rI")])
           (match_operand:SI 2 "s_register_operand" "r")]))]
  ""
  "%i1%?\\t%0, %2, %4%S3")

(define_insn ""
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

(define_insn ""
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

(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(minus:SI (match_operand:SI 1 "s_register_operand" "r")
		  (match_operator:SI 2 "shift_operator"
		   [(match_operand:SI 3 "s_register_operand" "r")
		    (match_operand:SI 4 "reg_or_int_operand" "rM")])))]
  ""
  "sub%?\\t%0, %1, %3%S2")

(define_insn ""
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

(define_insn ""
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

(define_insn ""
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

(define_insn ""
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

(define_insn ""
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

(define_insn ""
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
[(set_attr "length" "20")])

(define_insn ""
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
 (set_attr "conds" "set")])

(define_insn ""
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
 (set_attr "conds" "set")])




(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(and:SI (match_operator 1 "comparison_operator"
		 [(match_operand 3 "reversible_cc_register" "") (const_int 0)])
		(match_operand:SI 2 "s_register_operand" "r")))]
  ""
  "mov%D1\\t%0, #0\;and%d1\\t%0, %2, #1"
[(set_attr "conds" "use")
 (set_attr "length" "8")])

(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(ior:SI (match_operator 2 "comparison_operator"
		 [(reg 24) (const_int 0)])
		(match_operand:SI 1 "s_register_operand" "0,?r")))]
  ""
  "@
   orr%d2\\t%0, %1, #1
   mov%D2\\t%0, %1\;orr%d2\\t%0, %1, #1"
[(set_attr "conds" "use")
 (set_attr "length" "4,8")])

(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(match_operator 1 "comparison_operator"
	 [(match_operand:SI 2 "s_register_operand" "r,r")
	  (match_operand:SI 3 "arm_add_operand" "rI,L")]))
   (clobber (reg 24))]
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

(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=&r")
	(ior:SI (match_operator 1 "comparison_operator"
		 [(match_operand:SI 2 "s_register_operand" "r")
		  (match_operand:SI 3 "arm_rhs_operand" "rI")])
		(match_operator 4 "comparison_operator"
		 [(match_operand:SI 5 "s_register_operand" "r")
		  (match_operand:SI 6 "arm_rhs_operand" "rI")])))
   (clobber (reg 24))]
  ""
  "*
{
  int dominant = comparison_dominates_p (GET_CODE (operands[4]),
					 GET_CODE (operands[1]));

  output_asm_insn (dominant ? \"cmp\\t%5, %6\" : \"cmp\\t%2, %3\",
		   operands);
  output_asm_insn (\"mov\\t%0, #0\", operands);
  if (GET_CODE (operands[1]) == GET_CODE (operands[4])
      || comparison_dominates_p (GET_CODE (operands[1]),
				 GET_CODE (operands[4]))
      || dominant)
    output_asm_insn (dominant ? \"cmp%D4\\t%2, %3\" : \"cmp%D1\\t%5,%6\",
		     operands);
  else
    output_asm_insn (\"mov%d1\\t%0, #1\;cmp\\t%5, %6\", operands);
  return dominant ? \"mov%d1\\t%0, #1\" : \"mov%d4\\t%0, #1\";
}
"
[(set_attr "conds" "clob")
; worst case length
 (set_attr "length" "20")])

(define_split
  [(set (pc)
	(if_then_else
	 (match_operator 5 "equality_operator"
	  [(ior:SI (match_operator 6 "comparison_operator"
		    [(match_operand:SI 0 "s_register_operand" "")
		     (match_operand:SI 1 "arm_add_operand" "")])
		   (match_operator 7 "comparison_operator"
		    [(match_operand:SI 2 "s_register_operand" "")
		     (match_operand:SI 3 "arm_add_operand" "")]))
	  (const_int 0)])
	 (label_ref (match_operand 4 "" ""))
	 (pc)))
   (clobber (reg 24))]
  "(GET_CODE (operands[6]) == GET_CODE (operands[7])
    || comparison_dominates_p (GET_CODE (operands[6]), GET_CODE (operands[7]))
    || comparison_dominates_p (GET_CODE (operands[7]), GET_CODE (operands[6])))"
  [(set (reg:CC 24)
	(compare:CC (ior:CC (match_op_dup 6
			     [(match_dup 0) (match_dup 1)])
			    (match_op_dup 7
			     [(match_dup 2) (match_dup 3)]))
		    (const_int 0)))
   (set (pc)
        (if_then_else (match_op_dup 5 [(reg:CC 24) (const_int 0)])
		      (label_ref (match_dup 4))
		      (pc)))]
  "
{
  enum rtx_code code = comparison_dominates_p (GET_CODE (operands[6]),
					       GET_CODE (operands[7]))
		       ? GET_CODE (operands[7]) : GET_CODE (operands[6]);

  if (GET_CODE (operands[5]) == NE)
    operands[5] = gen_rtx (code, CCmode,
			   XEXP (operands[5], 0), XEXP (operands[5], 1));
  else
    operands[5] = gen_rtx (reverse_condition (code), CCmode,
			   XEXP (operands[5], 0), XEXP (operands[5], 1));
}
")

;; Don't match these patterns if we can use a conditional compare, since they
;; tell the final prescan branch elimator code that full branch inlining
;; can't be done.

(define_insn ""
  [(set (pc)
	(if_then_else
	 (ne (ior:SI (match_operator 5 "comparison_operator"
		      [(match_operand:SI 0 "s_register_operand" "r,r,r,r")
		       (match_operand:SI 1 "arm_add_operand" "rI,L,rI,L")])
		     (match_operator 6 "comparison_operator"
		      [(match_operand:SI 2 "s_register_operand" "r,r,r,r")
		       (match_operand:SI 3 "arm_rhs_operand" "rI,rI,L,L")]))
	     (const_int 0))
	 (label_ref (match_operand 4 "" ""))
	 (pc)))
   (clobber (reg 24))]
  "!(GET_CODE (operands[5]) == GET_CODE (operands[6])
     || comparison_dominates_p (GET_CODE (operands[5]), GET_CODE (operands[6]))
     || comparison_dominates_p (GET_CODE (operands[6]), GET_CODE (operands[5])))"
  "*
{
  extern int arm_ccfsm_state;

  if (which_alternative & 1)
    output_asm_insn (\"cmn\\t%0, #%n1\;b%d5\\t%l4\", operands);
  else
    output_asm_insn (\"cmp\\t%0, %1\;b%d5\\t%l4\", operands);

  if (which_alternative >= 2)
    output_asm_insn (\"cmn\\t%2, #%n3\", operands);
  else
    output_asm_insn (\"cmp\\t%2, %3\", operands);

  if (arm_ccfsm_state == 1 || arm_ccfsm_state == 2)
  {
    arm_ccfsm_state += 2;
    return \"\";
  }
  return \"b%d6\\t%l4\";
}"
[(set_attr "conds" "jump_clob")
 (set_attr "length" "16")])

(define_insn ""
  [(set (reg:CC 24)
	(compare:CC
	 (ior:CC (match_operator 4 "comparison_operator"
		  [(match_operand:SI 0 "s_register_operand" "r,r,r,r")
		   (match_operand:SI 1 "arm_add_operand" "rI,L,rI,L")])
		 (match_operator 5 "comparison_operator"
		  [(match_operand:SI 2 "s_register_operand" "r,r,r,r")
		   (match_operand:SI 3 "arm_add_operand" "rI,rI,L,L")]))
	 (const_int 0)))]
  "(GET_CODE (operands[4]) == GET_CODE (operands[5])
    || comparison_dominates_p (GET_CODE (operands[4]), GET_CODE (operands[5]))
    || comparison_dominates_p (GET_CODE (operands[5]), GET_CODE (operands[4])))"
  "*
  if (comparison_dominates_p (GET_CODE (operands[5]), GET_CODE (operands[4])))
    {
      if (which_alternative >= 2)
	output_asm_insn (\"cmn\\t%2, #%n3\", operands);
      else
	output_asm_insn (\"cmp\\t%2, %3\", operands);

      if (which_alternative & 1)
	return \"cmn%D5\\t%0, #%n1\";
      return \"cmp%D5\\t%0, %1\";
    }

  if (which_alternative & 1)
    output_asm_insn (\"cmn\\t%0, #%n1\", operands);
  else
    output_asm_insn (\"cmp\\t%0, %1\", operands);

  if (which_alternative >= 2)
    return \"cmn%D4\\t%2, #%n3\";
  return \"cmp%D4\\t%2, %3\";
"
[(set_attr "conds" "set")
 (set_attr "length" "8")])

(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=r,r,r")
	(if_then_else (match_operator 3 "equality_operator"
		       [(match_operator 4 "comparison_operator"
			 [(reg 24) (const_int 0)])
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

(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
        (match_operator:SI 5 "shiftable_operator" 
	 [(match_operator:SI 4 "comparison_operator"
           [(match_operand:SI 2 "s_register_operand" "r,r")
	    (match_operand:SI 3 "arm_rhs_operand" "rI,rI")])
          (match_operand:SI 1 "s_register_operand" "0,?r")]))
   (clobber (reg 24))]
  ""
  "*
  if (GET_CODE (operands[4]) == LT && operands[3] == const0_rtx)
    return \"%i5\\t%0, %1, %2, lsr #31\";

  output_asm_insn (\"cmp\\t%2, %3\", operands);
  if (GET_CODE (operands[5]) == AND)
    output_asm_insn (\"mov%D4\\t%0, #0\", operands);
  else if (which_alternative != 0)
    output_asm_insn (\"mov%D4\\t%0, %1\", operands);
  return \"%i5%d4\\t%0, %1, #1\";
"
[(set_attr "conds" "clob")
 (set_attr "length" "12")])

(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
        (minus:SI (match_operand:SI 1 "s_register_operand" "0,?r")
		  (match_operator:SI 4 "comparison_operator"
                   [(match_operand:SI 2 "s_register_operand" "r,r")
		    (match_operand:SI 3 "arm_rhs_operand" "rI,rI")])))
   (clobber (reg 24))]
  ""
  "*
  output_asm_insn (\"cmp\\t%2, %3\", operands);
  if (which_alternative != 0)
    output_asm_insn (\"mov%D4\\t%0, %1\", operands);
  return \"sub%d4\\t%0, %1, #1\";
"
[(set_attr "conds" "clob")
 (set_attr "length" "8,12")])

(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=&r")
	(and:SI (match_operator 1 "comparison_operator"
		 [(match_operand:SI 2 "s_register_operand" "r")
		  (match_operand:SI 3 "arm_rhs_operand" "rI")])
		(match_operator 4 "comparison_operator"
		 [(match_operand:SI 5 "s_register_operand" "r")
		  (match_operand:SI 6 "arm_rhs_operand" "rI")])))
   (clobber (reg 24))]
  ""
  "*
{
  int dominant =
	comparison_dominates_p (reverse_condition (GET_CODE (operands[1])),
				reverse_condition (GET_CODE (operands[4])))
	? 1 
	: comparison_dominates_p (reverse_condition (GET_CODE (operands[4])),
				  reverse_condition (GET_CODE (operands[1])))
	? 2 : 0;
  output_asm_insn (dominant == 2 ? \"cmp\\t%5, %6\" : \"cmp\\t%2, %3\",
		       operands);
  output_asm_insn (\"mov\\t%0, #1\", operands);
  if (GET_CODE (operands[1]) == GET_CODE (operands[4]) || dominant)
    {
      output_asm_insn (dominant == 2 ? \"cmp%d4\\t%2, %3\"
			   : \"cmp%d1\\t%5, %6\", operands);
    }
  else
    {
      output_asm_insn (\"mov%D1\\t%0, #0\", operands);
      output_asm_insn (\"cmp\\t%5, %6\", operands);
    }
  return dominant == 2 ? \"mov%D1\\t%0, #0\" : \"mov%D4\\t%0, #0\";
}
"
[(set_attr "conds" "clob")
 (set_attr "length" "20")])

(define_split
  [(set (pc)
	(if_then_else (match_operator 1 "equality_operator"
		       [(and:SI (match_operator 2 "comparison_operator"
				 [(match_operand:SI 3 "s_register_operand" "")
				  (match_operand:SI 4 "arm_add_operand" "")])
				(match_operator 0 "comparison_operator"
				 [(match_operand:SI 5 "s_register_operand" "")
				  (match_operand:SI 6 "arm_add_operand" "")]))
			(const_int 0)])
		      (label_ref (match_operand 7 "" ""))
		      (pc)))
   (clobber (reg 24))]
  "(GET_CODE (operands[2]) == GET_CODE (operands[0])
    || comparison_dominates_p (reverse_condition (GET_CODE (operands[2])),
			       reverse_condition (GET_CODE (operands[0])))
    || comparison_dominates_p (reverse_condition (GET_CODE (operands[0])),
			       reverse_condition (GET_CODE (operands[2]))))"
  [(set (reg:CC 24)
	(compare:CC (ior:CC (match_op_dup 2
			     [(match_dup 3) (match_dup 4)])
			    (match_op_dup 0
			     [(match_dup 5) (match_dup 6)]))
		    (const_int 0)))
   (set (pc)
        (if_then_else (match_op_dup 1 [(reg:CC 24) (const_int 0)])
		      (label_ref (match_dup 7))
		      (pc)))]
  "
{
  /* Use DeMorgans law to convert this into an IOR of the inverse conditions 
     This is safe since we only do it for integer comparisons. */
  enum rtx_code code = 
	comparison_dominates_p (reverse_condition (GET_CODE (operands[2])),
				reverse_condition (GET_CODE (operands[0])))
	? GET_CODE (operands[0]) : GET_CODE (operands[2]);

  operands[2] = gen_rtx (reverse_condition (GET_CODE (operands[2])),
			 GET_MODE (operands[2]), operands[3], operands[4]);
  operands[0] = gen_rtx (reverse_condition (GET_CODE (operands[0])),
			 GET_MODE (operands[0]), operands[5], operands[6]);
  if (GET_CODE (operands[1]) == NE)
    operands[1] = gen_rtx (code, CCmode,
			   XEXP (operands[1], 0), XEXP (operands[1], 1));
  else
    operands[1] = gen_rtx (reverse_condition (code), CCmode,
			   XEXP (operands[1], 0), XEXP (operands[1], 1));
}
")

;; Don't match these patterns if we can use a conditional compare, since they
;; tell the final prescan branch elimator code that full branch inlining
;; can't be done.

(define_insn ""
  [(set (pc)
	(if_then_else
	 (eq (and:SI (match_operator 1 "comparison_operator"
		      [(match_operand:SI 2 "s_register_operand" "r,r,r,r")
		       (match_operand:SI 3 "arm_add_operand" "rI,L,rI,L")])
		     (match_operator 4 "comparison_operator"
		      [(match_operand:SI 5 "s_register_operand" "r,r,r,r")
		       (match_operand:SI 6 "arm_rhs_operand" "rI,rI,L,L")]))
	     (const_int 0))
	 (label_ref (match_operand 0 "" ""))
	 (pc)))
   (clobber (reg 24))]
  "!(GET_CODE (operands[1]) == GET_CODE (operands[4])
     || comparison_dominates_p (reverse_condition (GET_CODE (operands[1])),
			        reverse_condition (GET_CODE (operands[4])))
     || comparison_dominates_p (reverse_condition (GET_CODE (operands[4])),
			        reverse_condition (GET_CODE (operands[1]))))"
  "*
{
  extern int arm_ccfsm_state;

  if (which_alternative & 1)
    output_asm_insn (\"cmn\\t%2, #%n3\;b%D1\\t%l0\", operands);
  else
    output_asm_insn (\"cmp\\t%2, %3\;b%D1\\t%l0\", operands);

  if (which_alternative >= 2)
    output_asm_insn (\"cmn\\t%5, #%n6\", operands);
  else
    output_asm_insn (\"cmp\\t%5, %6\", operands);

  if (arm_ccfsm_state == 1 || arm_ccfsm_state == 2)
  {
    arm_ccfsm_state += 2;
    return \"\";
  }
  return \"b%D4\\t%l0\";
}"
[(set_attr "conds" "jump_clob")
 (set_attr "length" "16")])

(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(neg:SI (match_operator 3 "comparison_operator"
		 [(match_operand:SI 1 "s_register_operand" "r")
		  (match_operand:SI 2 "arm_rhs_operand" "rI")])))
   (clobber (reg 24))]
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
   (clobber (reg 24))]
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

(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(if_then_else:SI (match_operator 9 "comparison_operator"
			  [(match_operand:SI 5 "s_register_operand" "r,r")
			   (match_operand:SI 6 "arm_add_operand" "rI,L")])
			 (match_operator:SI 8 "shiftable_operator"
			  [(match_operand:SI 1 "s_register_operand" "r,r")
			   (match_operand:SI 2 "arm_rhs_operand" "rI,rI")])
			 (match_operator:SI 7 "shiftable_operator"
			  [(match_operand:SI 3 "s_register_operand" "r,r")
			   (match_operand:SI 4 "arm_rhs_operand" "rI,rI")])))
   (clobber (reg 24))]
  ""
  "@
   cmp\\t%5, %6\;%I8%d9\\t%0, %1, %2\;%I7%D9\\t%0, %3, %4
   cmn\\t%5, #%n6\;%I8%d9\\t%0, %1, %2\;%I7%D9\\t%0, %3, %4"
[(set_attr "conds" "clob")
 (set_attr "length" "12")])

(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(if_then_else:SI (match_operator 6 "comparison_operator"
			  [(match_operand:SI 2 "s_register_operand" "r,r")
			   (match_operand:SI 3 "arm_add_operand" "rIL,rIL")])
			 (match_operator:SI 7 "shiftable_operator"
			  [(match_operand:SI 4 "s_register_operand" "r,r")
			   (match_operand:SI 5 "arm_rhs_operand" "rI,rI")])
			 (match_operand:SI 1 "arm_rhsm_operand" "0,?rIm")))
   (clobber (reg 24))]
  ""
  "*
  /* If we have an operation where (op x 0) is the identity operation and
     the condtional operator is LT or GE and we are comparing against zero and
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
    {
      if (GET_CODE (operands[1]) == MEM)
	return \"ldr%D6\\t%0, %1\";
      else
	return \"mov%D6\\t%0, %1\";
    }
  return \"\";
"
[(set_attr "conds" "clob")
 (set_attr "length" "8,12")])

(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(if_then_else:SI (match_operator 6 "comparison_operator"
			  [(match_operand:SI 4 "s_register_operand" "r,r")
			   (match_operand:SI 5 "arm_add_operand" "rIL,rIL")])
			 (match_operand:SI 1 "arm_rhsm_operand" "0,?rIm")
			 (match_operator:SI 7 "shiftable_operator"
			  [(match_operand:SI 2 "s_register_operand" "r,r")
			   (match_operand:SI 3 "arm_rhs_operand" "rI,rI")])))
   (clobber (reg 24))]
  ""
  "*
  /* If we have an operation where (op x 0) is the identity operation and
     the condtional operator is LT or GE and we are comparing against zero and
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
    {
      if (GET_CODE (operands[1]) == MEM)
	output_asm_insn (\"ldr%d6\\t%0, %1\", operands);
      else
	output_asm_insn (\"mov%d6\\t%0, %1\", operands);
    }
  return \"%I7%D6\\t%0, %2, %3\";
"
[(set_attr "conds" "clob")
 (set_attr "length" "8,12")])

(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(if_then_else:SI (match_operator 6 "comparison_operator"
			  [(match_operand:SI 4 "s_register_operand" "r,r")
			   (match_operand:SI 5 "arm_add_operand" "rIL,rIL")])
			 (plus:SI
			  (match_operand:SI 2 "s_register_operand" "r,r")
			  (match_operand:SI 3 "arm_add_operand" "rL,rL"))
			 (match_operand:SI 1 "arm_rhsm_operand" "0,?rIm")))
   (clobber (reg 24))]
  ""
  "*
{
  if (GET_CODE (operands[5]) == CONST_INT
      && !const_ok_for_arm (INTVAL (operands[5])))
    output_asm_insn (\"cmn\\t%4, #%n5\", operands);
  else
    output_asm_insn (\"cmp\\t%4, %5\", operands);
  if (GET_CODE (operands[3]) == CONST_INT
      && !const_ok_for_arm (INTVAL (operands[3])))
    output_asm_insn (\"sub%d6\\t%0, %2, #%n3\", operands);
  else
    output_asm_insn (\"add%d6\\t%0, %2, %3\", operands);
  if (which_alternative != 0)
    {
      if (GET_CODE (operands[1]) == MEM)
	output_asm_insn (\"ldr%D6\\t%0, %1\", operands);
      else
	output_asm_insn (\"mov%D6\\t%0, %1\", operands);
    }
  return \"\";
}
"
[(set_attr "conds" "clob")
 (set_attr "length" "8,12")])

(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(if_then_else:SI (match_operator 6 "comparison_operator"
			  [(match_operand:SI 4 "s_register_operand" "r,r")
			   (match_operand:SI 5 "arm_add_operand" "rIL,rIL")])
			 (match_operand:SI 1 "arm_rhsm_operand" "0,?rIm")
			 (plus:SI
			  (match_operand:SI 2 "s_register_operand" "r,r")
			  (match_operand:SI 3 "arm_add_operand" "rIL,rIL"))))
   (clobber (reg 24))]
  ""
  "*
{
  if (GET_CODE (operands[5]) == CONST_INT
      && !const_ok_for_arm (INTVAL (operands[5])))
    output_asm_insn (\"cmn\\t%4, #%n5\", operands);
  else
    output_asm_insn (\"cmp\\t%4, %5\", operands);
  if (GET_CODE (operands[3]) == CONST_INT
      && !const_ok_for_arm (INTVAL (operands[3])))
    output_asm_insn (\"sub%D6\\t%0, %2, #%n3\", operands);
  else
    output_asm_insn (\"add%D6\\t%0, %2, %3\", operands);
  if (which_alternative != 0)
    {
      if (GET_CODE (operands[6]) == MEM)
	output_asm_insn (\"ldr%d6\\t%0, %1\", operands);
      else
	output_asm_insn (\"mov%d6\\t%0, %1\", operands);
    }
  return \"\";
}
"
[(set_attr "conds" "clob")
 (set_attr "length" "8,12")])

(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(if_then_else:SI (match_operator 5 "comparison_operator"
			  [(match_operand:SI 3 "s_register_operand" "r,r")
			   (match_operand:SI 4 "arm_add_operand" "rIL,rIL")])
			 (match_operand:SI 1 "arm_rhs_operand" "0,?rI")
			 (not:SI
			  (match_operand:SI 2 "s_register_operand" "r,r"))))
   (clobber (reg 24))]
  ""
  "#"
[(set_attr "conds" "clob")
 (set_attr "length" "8,12")])

(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=r,r,r,r")
	(if_then_else:SI 
	 (match_operator 5 "comparison_operator"
	  [(match_operand:SI 3 "s_register_operand" "r,r,r,r")
	   (match_operand:SI 4 "arm_add_operand" "rI,L,rI,L")])
	 (not:SI
	  (match_operand:SI 2 "s_register_operand" "r,r,r,r"))
	 (match_operand:SI 1 "arm_rhs_operand" "0,0,?rI,?rI")))
   (clobber (reg 24))]
  ""
  "@
   cmp\\t%3, %4\;mvn%d5\\t%0, %2
   cmn\\t%3, #%n4\;mvn%d5\\t%0, %2
   cmp\\t%3, %4\;mov%D5\\t%0, %1\;mvn%d5\\t%0, %2
   cmn\\t%3, #%n4\;mov%D5\\t%0, %1\;mvn%d5\\t%0, %2"
[(set_attr "conds" "clob")
 (set_attr "length" "8,8,12,12")])

(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=r,r,r,r")
	(if_then_else:SI
	 (match_operator 6 "comparison_operator"
	  [(match_operand:SI 4 "s_register_operand" "r,r,r,r")
	   (match_operand:SI 5 "arm_add_operand" "rI,L,rI,L")])
	 (match_operator:SI 7 "shift_operator"
	  [(match_operand:SI 2 "s_register_operand" "r,r,r,r")
	   (match_operand:SI 3 "arm_rhs_operand" "rM,rM,rM,rM")])
	 (match_operand:SI 1 "arm_rhs_operand" "0,0,?rI,?rI")))
   (clobber (reg 24))]
  ""
  "@
   cmp\\t%4, %5\;mov%d6\\t%0, %2%S7
   cmn\\t%4, #%n5\;mov%d6\\t%0, %2%S7
   cmp\\t%4, %5\;mov%D6\\t%0, %1\;mov%d6\\t%0, %2%S7
   cmn\\t%4, #%n5\;mov%D6\\t%0, %1\;mov%d6\\t%0, %2%S7"
[(set_attr "conds" "clob")
 (set_attr "length" "8,8,12,12")])

(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=r,r,r,r")
	(if_then_else:SI
	 (match_operator 6 "comparison_operator"
	  [(match_operand:SI 4 "s_register_operand" "r,r,r,r")
	   (match_operand:SI 5 "arm_add_operand" "rI,L,rI,L")])
	 (match_operand:SI 1 "arm_rhs_operand" "0,0,?rI,?rI")
	 (match_operator:SI 7 "shift_operator"
	  [(match_operand:SI 2 "s_register_operand" "r,r,r,r")
	   (match_operand:SI 3 "arm_rhs_operand" "rM,rM,rM,rM")])))
   (clobber (reg 24))]
  ""
  "@
   cmp\\t%4, %5\;mov%D6\\t%0, %2%S7
   cmn\\t%4, #%n5\;mov%D6\\t%0, %2%S7
   cmp\\t%4, %5\;mov%d6\\t%0, %1\;mov%D6\\t%0, %2%S7
   cmn\\t%4, #%n5\;mov%d6\\t%0, %1\;mov%D6\\t%0, %2%S7"
[(set_attr "conds" "clob")
 (set_attr "length" "8,8,12,12")])

(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(if_then_else:SI
	 (match_operator 7 "comparison_operator"
	  [(match_operand:SI 5 "s_register_operand" "r,r")
	   (match_operand:SI 6 "arm_add_operand" "rI,L")])
	 (match_operator:SI 8 "shift_operator"
	  [(match_operand:SI 1 "s_register_operand" "r,r")
	   (match_operand:SI 2 "arm_rhs_operand" "rM,rM")])
	 (match_operator:SI 9 "shift_operator"
	  [(match_operand:SI 3 "s_register_operand" "r,r")
	   (match_operand:SI 4 "arm_rhs_operand" "rI,rI")])))
   (clobber (reg 24))]
  ""
  "@
   cmp\\t%5, %6\;mov%d7\\t%0, %1%S8\;mov%D7\\t%0, %3%S9
   cmn\\t%5, #%n6\;mov%d7\\t%0, %1%S8\;mov%D7\\t%0, %3%S9"
[(set_attr "conds" "clob")
 (set_attr "length" "12")])

(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(if_then_else:SI
	 (match_operator 6 "comparison_operator"
	  [(match_operand:SI 4 "s_register_operand" "r,r")
	   (match_operand:SI 5 "arm_add_operand" "rI,L")])
	 (not:SI (match_operand:SI 1 "s_register_operand" "r,r"))
	 (match_operator:SI 7 "shiftable_operator"
	  [(match_operand:SI 2 "s_register_operand" "r,r")
	   (match_operand:SI 3 "arm_rhs_operand" "rI,rI")])))
   (clobber (reg 24))]
  ""
  "@
   cmp\\t%4, %5\;mvn%d6\\t%0, %1\;%I7%D6\\t%0, %2, %3
   cmn\\t%4, #%n5\;mvn%d6\\t%0, %1\;%I7%D6\\t%0, %2, %3"
[(set_attr "conds" "clob")
 (set_attr "length" "12")])

(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(if_then_else:SI
	 (match_operator 6 "comparison_operator"
	  [(match_operand:SI 4 "s_register_operand" "r,r")
	   (match_operand:SI 5 "arm_add_operand" "rI,L")])
	 (match_operator:SI 7 "shiftable_operator"
	  [(match_operand:SI 2 "s_register_operand" "r,r")
	   (match_operand:SI 3 "arm_rhs_operand" "rI,rI")])
	 (not:SI (match_operand:SI 1 "s_register_operand" "r,r"))))
   (clobber (reg 24))]
  ""
  "@
   cmp\\t%4, %5\;mvn%D6\\t%0, %1\;%I7%d6\\t%0, %2, %3
   cmn\\t%4, #%n5\;mvn%D6\\t%0, %1\;%I7%d6\\t%0, %2, %3"
[(set_attr "conds" "clob")
 (set_attr "length" "12")])

(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=r,r,r,r")
	(if_then_else:SI
	 (match_operator 5 "comparison_operator"
	  [(match_operand:SI 3 "s_register_operand" "r,r,r,r")
	   (match_operand:SI 4 "arm_add_operand" "rI,L,rI,L")])
	 (neg:SI (match_operand:SI 2 "s_register_operand" "r,r,r,r"))
	 (match_operand:SI 1 "arm_rhs_operand" "0,0,?rI,?rI")))
   (clobber (reg:CC 24))]
  ""
  "@
   cmp\\t%3, %4\;rsb%d5\\t%0, %2, #0
   cmn\\t%3, #%n4\;rsb%d5\\t%0, %2, #0
   cmp\\t%3, %4\;mov%D5\\t%0, %1\;rsb%d5\\t%0, %2, #0
   cmn\\t%3, #%n4\;mov%D5\\t%0, %1\;rsb%d5\\t%0, %2, #0"
[(set_attr "conds" "clob")
 (set_attr "length" "8,8,12,12")])

(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=r,r,r,r")
	(if_then_else:SI
	 (match_operator 5 "comparison_operator"
	  [(match_operand:SI 3 "s_register_operand" "r,r,r,r")
	   (match_operand:SI 4 "arm_add_operand" "rI,L,rI,L")])
	 (match_operand:SI 1 "arm_rhs_operand" "0,0,?rI,?rI")
	 (neg:SI (match_operand:SI 2 "s_register_operand" "r,r,r,r"))))
   (clobber (reg:CC 24))]
  ""
  "@
   cmp\\t%3, %4\;rsb%D5\\t%0, %2, #0
   cmn\\t%3, #%n4\;rsb%D5\\t%0, %2, #0
   cmp\\t%3, %4\;mov%d5\\t%0, %1\;rsb%D5\\t%0, %2, #0
   cmn\\t%3, #%n4\;mov%d5\\t%0, %1\;rsb%D5\\t%0, %2, #0"
[(set_attr "conds" "clob")
 (set_attr "length" "8,8,12,12")])

(define_insn ""
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

(define_insn ""
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

(define_insn ""
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

(define_insn ""
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

(define_insn ""
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

(define_insn ""
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

(define_insn ""
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

(define_insn ""
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

(define_insn ""
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

(define_insn ""
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

(define_insn ""
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

(define_insn ""
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

(define_insn ""
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

(define_insn ""
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

(define_insn ""
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

(define_insn ""
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

(define_insn ""
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

(define_insn ""
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

(define_insn ""
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

(define_insn ""
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

(define_insn ""
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

(define_insn ""
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

(define_insn ""
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
   (set (match_operand 2 "cc_register" "")
	(compare (match_dup 1) (const_int 0)))]
  ""
  "sub%?s\\t%0, %1, #0"
[(set_attr "conds" "set")])

; Peepholes to spot possible load- and store-multiples, if the ordering is
; reversed, check that the memory references aren't volatile.

(define_peephole
  [(set (match_operand:SI 0 "s_register_operand" "=r")
        (mem:SI (plus:SI (match_operand:SI 1 "s_register_operand" "r")
                         (const_int 12))))
   (set (match_operand:SI 2 "s_register_operand" "=r")
        (mem:SI (plus:SI (match_dup 1) (const_int 8))))
   (set (match_operand:SI 3 "s_register_operand" "=r")
        (mem:SI (plus:SI (match_dup 1) (const_int 4))))
   (set (match_operand:SI 4 "s_register_operand" "=r")
        (mem:SI (match_dup 1)))]
  "REGNO (operands[0]) > REGNO (operands[2])
   && REGNO (operands[2]) > REGNO (operands[3])
   && REGNO (operands[3]) > REGNO (operands[4])
   && !(REGNO (operands[1]) == REGNO (operands[0])
       || REGNO (operands[1]) == REGNO (operands[2])
       || REGNO (operands[1]) == REGNO (operands[3])
       || REGNO (operands[1]) == REGNO (operands[4]))
   && !MEM_VOLATILE_P (SET_SRC (PATTERN (insn)))
   && !MEM_VOLATILE_P (SET_SRC (PATTERN (prev_nonnote_insn (insn))))
   && !MEM_VOLATILE_P (SET_SRC (PATTERN (prev_nonnote_insn
					 (prev_nonnote_insn (insn)))))
   && !MEM_VOLATILE_P (SET_SRC (PATTERN (prev_nonnote_insn
					 (prev_nonnote_insn 
					  (prev_nonnote_insn (insn))))))"
  "ldm%?ia\\t%1, {%4, %3, %2, %0}\\t%@ phole ldm")

(define_peephole
  [(set (match_operand:SI 0 "s_register_operand" "=r")
        (mem:SI (plus:SI (match_operand:SI 1 "s_register_operand" "r")
                         (const_int 8))))
   (set (match_operand:SI 2 "s_register_operand" "=r")
        (mem:SI (plus:SI (match_dup 1) (const_int 4))))
   (set (match_operand:SI 3 "s_register_operand" "=r")
        (mem:SI (match_dup 1)))]
  "REGNO (operands[0]) >  REGNO (operands[2])
   && REGNO (operands[2]) > REGNO (operands[3])
   && !(REGNO (operands[1]) == REGNO (operands[0])
       || REGNO (operands[1]) == REGNO (operands[2])
       || REGNO (operands[1]) == REGNO (operands[3]))
   && !MEM_VOLATILE_P (SET_SRC (PATTERN (insn)))
   && !MEM_VOLATILE_P (SET_SRC (PATTERN (prev_nonnote_insn (insn))))
   && !MEM_VOLATILE_P (SET_SRC (PATTERN (prev_nonnote_insn
					 (prev_nonnote_insn (insn)))))"
  "ldm%?ia\\t%1, {%3, %2, %0}\\t%@ phole ldm")

(define_peephole
  [(set (match_operand:SI 0 "s_register_operand" "=r")
        (mem:SI (plus:SI (match_operand:SI 1 "s_register_operand" "r")
                         (const_int 4))))
   (set (match_operand:SI 2 "s_register_operand" "=r")
        (mem:SI (match_dup 1)))]
  "REGNO (operands[0]) > REGNO (operands[2])
   && !(REGNO (operands[1]) == REGNO (operands[0])
       || REGNO (operands[1]) == REGNO (operands[2]))
   && !MEM_VOLATILE_P (SET_SRC (PATTERN (insn)))
   && !MEM_VOLATILE_P (SET_SRC (PATTERN (prev_nonnote_insn (insn))))"
  "ldm%?ia\\t%1, {%2, %0}\\t%@ phole ldm")

(define_peephole
  [(set (mem:SI (plus:SI (match_operand:SI 1 "s_register_operand" "r")
                         (const_int 12)))
        (match_operand:SI 0 "s_register_operand" "r"))
   (set (mem:SI (plus:SI (match_dup 1) (const_int 8)))
        (match_operand:SI 2 "s_register_operand" "r"))
   (set (mem:SI (plus:SI (match_dup 1) (const_int 4)))
        (match_operand:SI 3 "s_register_operand" "r"))
   (set (mem:SI (match_dup 1))
        (match_operand:SI 4 "s_register_operand" "r"))]
  "REGNO (operands[0]) >  REGNO (operands[2])
   && REGNO (operands[2]) > REGNO (operands[3])
   && REGNO (operands[3]) > REGNO (operands[4])
   && !MEM_VOLATILE_P (SET_DEST (PATTERN (insn)))
   && !MEM_VOLATILE_P (SET_DEST (PATTERN (prev_nonnote_insn (insn))))
   && !MEM_VOLATILE_P (SET_DEST (PATTERN (prev_nonnote_insn
					  (prev_nonnote_insn (insn)))))
   && !MEM_VOLATILE_P (SET_DEST (PATTERN (prev_nonnote_insn
					  (prev_nonnote_insn 
					   (prev_nonnote_insn (insn))))))"
  "stm%?ia\\t%1, {%4, %3, %2, %0}\\t%@ phole stm")

(define_peephole
  [(set (mem:SI (plus:SI (match_operand:SI 1 "s_register_operand" "r")
                         (const_int 8)))
        (match_operand:SI 0 "s_register_operand" "r"))
   (set (mem:SI (plus:SI (match_dup 1) (const_int 4)))
        (match_operand:SI 2 "s_register_operand" "r"))
   (set (mem:SI (match_dup 1))
        (match_operand:SI 3 "s_register_operand" "r"))]
  "REGNO (operands[0]) >  REGNO (operands[2])
   && REGNO (operands[2]) > REGNO (operands[3])
   && !MEM_VOLATILE_P (SET_DEST (PATTERN (insn)))
   && !MEM_VOLATILE_P (SET_DEST (PATTERN (prev_nonnote_insn (insn))))
   && !MEM_VOLATILE_P (SET_DEST (PATTERN (prev_nonnote_insn
					  (prev_nonnote_insn (insn)))))"
  "stm%?ia\\t%1, {%3, %2, %0}\\t%@ phole stm")

(define_peephole
  [(set (mem:SI (plus:SI (match_operand:SI 1 "s_register_operand" "r")
                         (const_int 4)))
        (match_operand:SI 0 "s_register_operand" "r"))
   (set (mem:SI (match_dup 1))
        (match_operand:SI 2 "s_register_operand" "r"))]
  "REGNO (operands[0]) >  REGNO (operands[2])
   && !MEM_VOLATILE_P (SET_DEST (PATTERN (insn)))
   && !MEM_VOLATILE_P (SET_DEST (PATTERN (prev_nonnote_insn (insn))))"
  "stm%?ia\\t%1, {%2, %0}\\t%@ phole stm")

;; A call followed by return can be replaced by restoring the regs and
;; jumping to the subroutine, provided we aren't passing the address of
;; any of our local variables.  If we call alloca then this is unsafe
;; since restoring the frame frees the memory, which is not what we want.
;; Sometimes the return might have been targeted by the final prescan:
;; if so then emit a propper return insn as well.
;; Unfortunately, if the frame pointer is required, we don't know if the
;; current function has any implicit stack pointer adjustments that will 
;; be restored by the return: we can't therefore do a tail call.
;; Another unfortunate that we can't handle is if current_function_args_size
;; is non-zero: in this case elimination of the argument pointer assumed
;; that lr was pushed onto the stack, so eliminating upsets the offset
;; calculations.

(define_peephole
  [(parallel [(call (mem:SI (match_operand:SI 0 "" "i"))
			  (match_operand:SI 1 "general_operand" "g"))
		    (clobber (reg:SI 14))])
   (return)]
  "(GET_CODE (operands[0]) == SYMBOL_REF && USE_RETURN_INSN
    && !get_frame_size () && !current_function_calls_alloca
    && !frame_pointer_needed && !current_function_args_size)"
  "*
{
  extern rtx arm_target_insn;
  extern int arm_ccfsm_state, arm_current_cc;

  if (arm_ccfsm_state && arm_target_insn && INSN_DELETED_P (arm_target_insn))
  {
    arm_current_cc ^= 1;
    output_return_instruction (NULL, TRUE);
    arm_ccfsm_state = 0;
    arm_target_insn = NULL;
  }

  output_return_instruction (NULL, FALSE);
  return \"b%?\\t%a0\";
}"
[(set (attr "conds")
      (if_then_else (eq_attr "cpu" "arm6")
		    (const_string "clob")
		    (const_string "nocond")))
 (set_attr "length" "8")])

(define_peephole
  [(parallel [(set (match_operand 0 "s_register_operand" "=rf")
		   (call (mem:SI (match_operand:SI 1 "" "i"))
			 (match_operand:SI 2 "general_operand" "g")))
	      (clobber (reg:SI 14))])
   (return)]
  "(GET_CODE (operands[1]) == SYMBOL_REF && USE_RETURN_INSN
    && !get_frame_size () && !current_function_calls_alloca
    && !frame_pointer_needed && !current_function_args_size)"
  "*
{
  extern rtx arm_target_insn;
  extern int arm_ccfsm_state, arm_current_cc;

  if (arm_ccfsm_state && arm_target_insn && INSN_DELETED_P (arm_target_insn))
  {
    arm_current_cc ^= 1;
    output_return_instruction (NULL, TRUE);
    arm_ccfsm_state = 0;
    arm_target_insn = NULL;
  }

  output_return_instruction (NULL, FALSE);
  return \"b%?\\t%a1\";
}"
[(set (attr "conds")
      (if_then_else (eq_attr "cpu" "arm6")
		    (const_string "clob")
		    (const_string "nocond")))
 (set_attr "length" "8")])

;; As above but when this function is not void, we must be returning the
;; result of the called subroutine.

(define_peephole
  [(parallel [(set (match_operand 0 "s_register_operand" "=rf")
		   (call (mem:SI (match_operand:SI 1 "" "i"))
			 (match_operand:SI 2 "general_operand" "g")))
	      (clobber (reg:SI 14))])
   (use (match_dup 0))
   (return)]
  "(GET_CODE (operands[1]) == SYMBOL_REF && USE_RETURN_INSN
    && !get_frame_size () && !current_function_calls_alloca
    && !frame_pointer_needed && !current_function_args_size)"
  "*
{
  extern rtx arm_target_insn;
  extern int arm_ccfsm_state, arm_current_cc;

  if (arm_ccfsm_state && arm_target_insn && INSN_DELETED_P (arm_target_insn))
  {
    arm_current_cc ^= 1;
    output_return_instruction (NULL, TRUE);
    arm_ccfsm_state = 0;
    arm_target_insn = NULL;
  }

  output_return_instruction (NULL, FALSE);
  return \"b%?\\t%a1\";
}"
[(set (attr "conds")
      (if_then_else (eq_attr "cpu" "arm6")
		    (const_string "clob")
		    (const_string "nocond")))
 (set_attr "length" "8")])

;; If calling a subroutine and then jumping back to somewhere else, but not
;; too far away, then we can set the link register with the branch address
;; and jump direct to the subroutine.  On return from the subroutine
;; execution continues at the branch; this avoids a prefetch stall.
;; We use the length attribute (via short_branch ()) to establish whether or
;; not this is possible, this is the same asthe sparc does.

(define_peephole
  [(parallel[(call (mem:SI (match_operand:SI 0 "" "i"))
                   (match_operand:SI 1 "general_operand" "g"))
             (clobber (reg:SI 14))])
   (set (pc)
        (label_ref (match_operand 2 "" "")))]
  "0 && GET_CODE (operands[0]) == SYMBOL_REF 
   && short_branch (INSN_UID (insn), INSN_UID (operands[2]))
   && arm_insn_not_targeted (insn)"
  "*
{
  int backward = arm_backwards_branch (INSN_UID (insn),
				       INSN_UID (operands[2]));

#if 0
  /* Putting this in means that TARGET_6 code will ONLY run on an arm6 or
   * above, leaving it out means that the code will still run on an arm 2 or 3
   */
  if (TARGET_6)
    {
      if (backward)
	output_asm_insn (\"sub%?\\t%|lr, %|pc, #(8 + . -%l2)\", operands);
      else
	output_asm_insn (\"add%?\\t%|lr, %|pc, #(%l2 - . -8)\", operands);
    }
  else
#endif
    {
      output_asm_insn (\"mov%?\\t%|lr, %|pc\\t%@ protect cc\", operands);
      if (backward)
	output_asm_insn (\"sub%?\\t%|lr, %|lr, #(4 + . -%l2)\", operands);
      else
	output_asm_insn (\"add%?\\t%|lr, %|lr, #(%l2 - . -4)\", operands);
    }
  return \"b%?\\t%a0\";
}"
[(set (attr "conds")
      (if_then_else (eq_attr "cpu" "arm6")
		    (const_string "clob")
		    (const_string "nocond")))
 (set (attr "length")
      (if_then_else (eq_attr "cpu" "arm6")
		    (const_int 8)
		    (const_int 12)))])

(define_peephole
  [(parallel[(set (match_operand:SI 0 "s_register_operand" "=r")
		  (call (mem:SI (match_operand:SI 1 "" "i"))
                        (match_operand:SI 2 "general_operand" "g")))
             (clobber (reg:SI 14))])
   (set (pc)
        (label_ref (match_operand 3 "" "")))]
  "0 && GET_CODE (operands[0]) == SYMBOL_REF
   && short_branch (INSN_UID (insn), INSN_UID (operands[3]))
   && arm_insn_not_targeted (insn)"
  "*
{
  int backward = arm_backwards_branch (INSN_UID (insn),
				       INSN_UID (operands[3]));

#if 0
  /* Putting this in means that TARGET_6 code will ONLY run on an arm6 or
   * above, leaving it out means that the code will still run on an arm 2 or 3
   */
  if (TARGET_6)
    {
      if (backward)
	output_asm_insn (\"sub%?\\t%|lr, %|pc, #(8 + . -%l3)\", operands);
      else
	output_asm_insn (\"add%?\\t%|lr, %|pc, #(%l3 - . -8)\", operands);
    }
  else
#endif
    {
      output_asm_insn (\"mov%?\\t%|lr, %|pc\\t%@ protect cc\", operands);
      if (backward)
	output_asm_insn (\"sub%?\\t%|lr, %|lr, #(4 + . -%l3)\", operands);
      else
	output_asm_insn (\"add%?\\t%|lr, %|lr, #(%l3 - . -4)\", operands);
    }
  return \"b%?\\t%a1\";
}"
[(set (attr "conds")
      (if_then_else (eq_attr "cpu" "arm6")
		    (const_string "clob")
		    (const_string "nocond")))
 (set (attr "length")
      (if_then_else (eq_attr "cpu" "arm6")
		    (const_int 8)
		    (const_int 12)))])

(define_split
  [(set (pc)
	(if_then_else (match_operator 0 "comparison_operator"
		       [(match_operator:SI 1 "shift_operator"
			 [(match_operand:SI 2 "s_register_operand" "r")
			  (match_operand:SI 3 "reg_or_int_operand" "rM")])
			(match_operand:SI 4 "s_register_operand" "r")])
		      (label_ref (match_operand 5 "" ""))
		      (pc)))
   (clobber (reg 24))]
  ""
  [(set (reg:CC 24)
	(compare:CC (match_dup 4)
		    (match_op_dup 1 [(match_dup 2) (match_dup 3)])))
   (set (pc)
	(if_then_else (match_op_dup 0 [(reg 24) (const_int 0)])
		      (label_ref (match_dup 5))
		      (pc)))]
  "
  operands[0] = gen_rtx (swap_condition (GET_CODE (operands[0])), VOIDmode,
			 operands[1], operands[2]);
")

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

;; This pattern can be used because cc_noov mode implies that the following
;; branch will be an equality (EQ or NE), so the sign extension is not
;; needed.  Combine doesn't eliminate these because by the time it sees the
;; branch it no-longer knows that the data came from memory.

(define_insn ""
  [(set (reg:CC_NOOV 24)
	(compare:CC_NOOV
	 (ashift:SI (subreg:SI (match_operand:QI 0 "memory_operand" "m") 0)
		    (const_int 24))
	 (match_operand 1 "immediate_operand" "I")))
   (clobber (match_scratch:SI 2 "=r"))]
  "((unsigned long) INTVAL (operands[1]))
   == (((unsigned long) INTVAL (operands[1])) >> 24) << 24"
  "*
  operands[1] = GEN_INT (((unsigned long) INTVAL (operands[1])) >> 24);
  output_asm_insn (\"ldr%?b\\t%2, %0\", operands);
  output_asm_insn (\"cmp%?\\t%2, %1\", operands);
  return \"\";
"
[(set_attr "conds" "set")
 (set_attr "length" "8")
 (set_attr "type" "load")])

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
;; some extend with the conditional data operations, so we have to split them
;; up again here.

(define_split
  [(set (match_operand:SI 0 "s_register_operand" "")
	(if_then_else:SI (match_operator 1 "comparison_operator"
			  [(match_operand 2 "" "") (match_operand 3 "" "")])
			 (match_operand 4 "" "")
			 (match_operand 5 "" "")))
   (clobber (reg 24))]
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

  operands[6] = gen_rtx (REG, mode, 24);
  operands[7] = gen_rtx (COMPARE, mode, operands[2], operands[3]);
}
")


(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(if_then_else:SI (match_operator 4 "comparison_operator"
			  [(match_operand 3 "reversible_cc_register" "")
			   (const_int 0)])
			 (match_operand:SI 1 "arm_rhs_operand" "0,?rI")
			 (not:SI
			  (match_operand:SI 2 "s_register_operand" "r,r"))))]
  ""
  "@
   mvn%D4\\t%0, %2
   mov%d4\\t%0, %1\;mvn%D4\\t%0, %2"
[(set_attr "conds" "use")
 (set_attr "length" "4,8")])

;; The next two patterns occur when an AND operation is followed by a
;; scc insn sequence 

(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(sign_extract:SI (match_operand:SI 1 "s_register_operand" "r")
			 (const_int 1)
			 (match_operand:SI 2 "immediate_operand" "n")))]
  ""
  "*
  operands[2] = GEN_INT (1 << INTVAL (operands[2]));
  output_asm_insn (\"ands\\t%0, %1, %2\", operands);
  return \"mvnne\\t%0, #0\";
"
[(set_attr "conds" "clob")
 (set_attr "length" "8")])

(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(not:SI
	 (sign_extract:SI (match_operand:SI 1 "s_register_operand" "r")
			  (const_int 1)
			  (match_operand:SI 2 "immediate_operand" "n"))))]
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
(define_insn ""
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
  strcpy (pattern, \"stmfd\\t%m0!, {%|%1\");
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
