;;- Machine description Acorn RISC Machine for GNU compiler
;;  Copyright (C) 1991, 1993 Free Software Foundation, Inc.
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

;; Every template must be output by arm_output_asm_insn, since this keeps
;; track of the offset of labels within the text segment.  This is needed to
;; to be able to (correctly) output instructions for loading a value from a
;; function's constant pool, since different instructions are needed when the
;; constant pool is more than 4095 bytes away from the PC.

;; There are patterns in this file to support XFmode arithmetic.
;; Unfortunately RISCiX doesn't work well with these so they are disabled.
;; (See arm.h)

;; UNSPEC Usage:
;; 0 `sin' operation: operand 0 is the result, operand 1 the parameter,
;;   the mode is MODE_FLOAT
;; 1 `cos' operation: operand 0 is the result, operand 1 the parameter,
;;   the mode is MODE_FLOAT

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

; LENGTH, all instructions are 4 bytes
(define_attr "length" "" (const_int 1))

; An assembler sequence may clobber the condition codes without us knowing
(define_asm_attributes
 [(set_attr "conds" "clob")
  (set_attr "length" "1")])

; TYPE attribute is used to detect floating point instructions which, if
; running on a co-processor can run in parallel with other, basic instructions
; If write-buffer scheduling is enabled then it can also be used in the
; scheduling of writes.

; Classification of each insn
; normal	any data instruction that doesn't hit memory or fp regs
; block		blockage insn, this blocks all functional units
; float		a floating point arithmetic operation (subject to expansion)
; float_em	a floating point arithmetic operation that is normally emulated
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
	"normal,block,float,float_em,f_load,f_store,f_mem_r,r_mem_f,f_2_r,r_2_f,call,load,store1,store2,store3,store4" 
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
;; This is not well tuned, but I don't have all the details.
(define_function_unit "fpa" 1 1 (eq_attr "type" "float") 5 0)

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
  "*
  arm_output_asm_insn (\"adds\\t%0, %1, %2\", operands);
  return (arm_output_asm_insn (\"adc\\t%R0, %R1, %R2\", operands));
"
[(set_attr "conds" "clob")
 (set_attr "length" "2")])

(define_insn ""
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(plus:DI (sign_extend:DI
		  (match_operand:SI 1 "s_register_operand" "r,r"))
		 (match_operand:DI 2 "s_register_operand" "r,0")))
   (clobber (reg:CC 24))]
  ""
  "*
  arm_output_asm_insn (\"adds\\t%0, %2, %1\", operands);
  return (arm_output_asm_insn (\"adc\\t%R0, %R2, %1, asr #31\", operands));
"
[(set_attr "conds" "clob")
 (set_attr "length" "2")])

(define_insn ""
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(plus:DI (zero_extend:DI
		  (match_operand:SI 1 "s_register_operand" "r,r"))
		 (match_operand:DI 2 "s_register_operand" "r,0")))
   (clobber (reg:CC 24))]
  ""
  "*
  arm_output_asm_insn (\"adds\\t%0, %2, %1\", operands);
  return (arm_output_asm_insn (\"adc\\t%R0, %R2, #0\", operands));
"
[(set_attr "conds" "clob")
 (set_attr "length" "2")])

(define_insn "addsi3"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(plus:SI (match_operand:SI 1 "s_register_operand" "r")
		 (match_operand:SI 2 "arm_add_operand" "rL")))]
  ""
  "*
  if (GET_CODE (operands[2]) == CONST_INT
      && !const_ok_for_arm (INTVAL (operands[2])))
    {
      operands[2] = gen_rtx (CONST_INT, VOIDmode, -INTVAL (operands[2]));
      return arm_output_asm_insn (\"sub\\t%0, %1, %2\", operands);
    }
  return arm_output_asm_insn (\"add\\t%0, %1, %2\", operands);
")

(define_insn ""
  [(set (reg:CC_NOOV 24)
	(compare:CC_NOOV (plus:SI (match_operand:SI 1 "s_register_operand" "r")
		 		  (match_operand:SI 2 "arm_add_operand" "rL"))
			 (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(plus:SI (match_dup 1) (match_dup 2)))]
  ""
  "*
  if (GET_CODE (operands[2]) == CONST_INT
      && !const_ok_for_arm (INTVAL (operands[2])))
    {
      operands[2] = gen_rtx (CONST_INT, VOIDmode, -INTVAL (operands[2]));
      return arm_output_asm_insn (\"subs\\t%0, %1, %2\", operands);
    }
  return (arm_output_asm_insn (\"adds\\t%0, %1, %2\", operands));
"
[(set_attr "conds" "set")])

(define_insn ""
  [(set (reg:CC 24)
	(compare:CC (match_operand:SI 1 "s_register_operand" "r")
		    (neg:SI (match_operand:SI 2 "arm_add_operand" "rL"))))
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(plus:SI (match_dup 1) (match_dup 2)))]
  ""
  "*
  if (GET_CODE (operands[2]) == CONST_INT
      && !const_ok_for_arm (INTVAL (operands[2])))
    {
      operands[2] = gen_rtx (CONST_INT, VOIDmode, -INTVAL (operands[2]));
      return arm_output_asm_insn (\"subs\\t%0, %1, %2\", operands);
    }
  return (arm_output_asm_insn (\"adds\\t%0, %1, %2\", operands));
"
[(set_attr "conds" "set")])

(define_insn "incscc"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
        (plus:SI (match_operator:SI 2 "comparison_operator"
                    [(reg 24) (const_int 0)])
                 (match_operand:SI 1 "s_register_operand" "0,?r")))]
  ""
  "*
  if (which_alternative == 1)
    arm_output_asm_insn (\"mov%D2\\t%0, %1\", operands);
  return arm_output_asm_insn (\"add%d2\\t%0, %1, #1\", operands);
"
[(set_attr "conds" "use")
 (set_attr "length" "*,2")])

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
  "*
{
  REAL_VALUE_TYPE r;

  switch (which_alternative)
    {
    case 0:
      return arm_output_asm_insn (\"adfs\\t%0, %1, %2\", operands);
    case 1:
      REAL_VALUE_FROM_CONST_DOUBLE (r, operands[2]);
      r = REAL_VALUE_NEGATE (r);
      operands[2] = CONST_DOUBLE_FROM_REAL_VALUE (r, GET_MODE (operands[2]));
      return arm_output_asm_insn (\"sufs\\t%0, %1, %2\", operands);
    }
}
"
[(set_attr "type" "float")])

(define_insn "adddf3"
  [(set (match_operand:DF 0 "s_register_operand" "=f,f")
	(plus:DF (match_operand:DF 1 "s_register_operand" "f,f")
		 (match_operand:DF 2 "fpu_add_operand" "fG,H")))]
  ""
  "*
{
  REAL_VALUE_TYPE r;

  switch (which_alternative)
    {
    case 0:
      return (arm_output_asm_insn (\"adfd\\t%0, %1, %2\", operands));
    case 1:
      REAL_VALUE_FROM_CONST_DOUBLE (r, operands[2]);
      r = REAL_VALUE_NEGATE (r);
      operands[2] = CONST_DOUBLE_FROM_REAL_VALUE (r, GET_MODE (operands[2]));
      return arm_output_asm_insn (\"sufd\\t%0, %1, %2\", operands);
    }
}
"
[(set_attr "type" "float")])

(define_insn ""
  [(set (match_operand:DF 0 "s_register_operand" "=f,f")
	(plus:DF (float_extend:DF
		  (match_operand:SF 1 "s_register_operand" "f,f"))
		 (match_operand:DF 2 "fpu_add_operand" "fG,H")))]
  ""
  "*
{
  REAL_VALUE_TYPE r;

  switch (which_alternative)
    {
    case 0:
      return (arm_output_asm_insn (\"adfd\\t%0, %1, %2\", operands));
    case 1:
      REAL_VALUE_FROM_CONST_DOUBLE (r, operands[2]);
      r = REAL_VALUE_NEGATE (r);
      operands[2] = CONST_DOUBLE_FROM_REAL_VALUE (r, GET_MODE (operands[2]));
      return arm_output_asm_insn (\"sufd\\t%0, %1, %2\", operands);
    }
}
"
[(set_attr "type" "float")])

(define_insn ""
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(plus:DF (match_operand:DF 1 "s_register_operand" "f")
		 (float_extend:DF
		  (match_operand:SF 2 "s_register_operand" "f"))))]
  ""
  "*
  return (arm_output_asm_insn (\"adfd\\t%0, %1, %2\", operands));
"
[(set_attr "type" "float")])

(define_insn ""
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(plus:DF (float_extend:DF 
		  (match_operand:SF 1 "s_register_operand" "f"))
		 (float_extend:DF
		  (match_operand:SF 2 "s_register_operand" "f"))))]
  ""
  "*
  return (arm_output_asm_insn (\"adfd\\t%0, %1, %2\", operands));
"
[(set_attr "type" "float")])

(define_insn "addxf3"
  [(set (match_operand:XF 0 "s_register_operand" "=f,f")
	(plus:XF (match_operand:XF 1 "s_register_operand" "f,f")
		 (match_operand:XF 2 "fpu_add_operand" "fG,H")))]
  "ENABLE_XF_PATTERNS"
  "*
{
  REAL_VALUE_TYPE r;

  switch (which_alternative)
    {
    case 0:
      return (arm_output_asm_insn (\"adfe\\t%0, %1, %2\", operands));
    case 1:
      REAL_VALUE_FROM_CONST_DOUBLE (r, operands[2]);
      r = REAL_VALUE_NEGATE (r);
      operands[2] = CONST_DOUBLE_FROM_REAL_VALUE (r, GET_MODE (operands[2]));
      return arm_output_asm_insn (\"sufe\\t%0, %1, %2\", operands);
    }
}
"
[(set_attr "type" "float")])

(define_insn "subdi3"
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r,&r")
	(minus:DI (match_operand:DI 1 "s_register_operand" "0,r,0")
		  (match_operand:DI 2 "s_register_operand" "r,0,0")))
   (clobber (reg:CC 24))]
  ""
  "*
  arm_output_asm_insn (\"subs\\t%0, %1, %2\", operands);
  return (arm_output_asm_insn (\"sbc\\t%R0, %R1, %R2\", operands));
"
[(set_attr "conds" "clob")
 (set_attr "length" "2")])

(define_insn ""
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(minus:DI (match_operand:DI 1 "s_register_operand" "?r,0")
		  (zero_extend:DI
		   (match_operand:SI 2 "s_register_operand" "r,r"))))
   (clobber (reg:CC 24))]
  ""
  "*
  arm_output_asm_insn (\"subs\\t%0, %1, %2\", operands);
  return (arm_output_asm_insn (\"sbc\\t%R0, %R1, #0\", operands));
"
[(set_attr "conds" "clob")
 (set_attr "length" "2")])

(define_insn ""
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(minus:DI (match_operand:DI 1 "s_register_operand" "r,0")
		  (sign_extend:DI
		   (match_operand:SI 2 "s_register_operand" "r,r"))))
   (clobber (reg:CC 24))]
  ""
  "*
  arm_output_asm_insn (\"subs\\t%0, %1, %2\", operands);
  return (arm_output_asm_insn (\"sbc\\t%R0, %R1, %2, asr #31\", operands));
"
[(set_attr "conds" "clob")
 (set_attr "length" "2")])

(define_insn ""
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(minus:DI (zero_extend:DI
		   (match_operand:SI 2 "s_register_operand" "r,r"))
		  (match_operand:DI 1 "s_register_operand" "?r,0")))
   (clobber (reg:CC 24))]
  ""
  "*
  arm_output_asm_insn (\"rsbs\\t%0, %1, %2\", operands);
  return (arm_output_asm_insn (\"rsc\\t%R0, %R1, #0\", operands));
"
[(set_attr "conds" "clob")
 (set_attr "length" "2")])

(define_insn ""
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(minus:DI (sign_extend:DI
		   (match_operand:SI 2 "s_register_operand" "r,r"))
		  (match_operand:DI 1 "s_register_operand" "?r,0")))
   (clobber (reg:CC 24))]
  ""
  "*
  arm_output_asm_insn (\"rsbs\\t%0, %1, %2\", operands);
  return (arm_output_asm_insn (\"rsc\\t%R0, %R1, %2, asr #31\", operands));
"
[(set_attr "conds" "clob")
 (set_attr "length" "2")])

(define_insn ""
  [(set (match_operand:DI 0 "s_register_operand" "=r")
	(minus:DI (zero_extend:DI
		   (match_operand:SI 1 "s_register_operand" "r"))
		  (zero_extend:DI
		   (match_operand:SI 2 "s_register_operand" "r"))))
   (clobber (reg:CC 24))]
  ""
  "*
  arm_output_asm_insn (\"subs\\t%0, %1, %2\", operands);
  return (arm_output_asm_insn (\"rsc\\t%R0, %1, %1 @ extend carry\",
	  operands));
"
[(set_attr "conds" "clob")
 (set_attr "length" "2")])

(define_insn "subsi3"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(minus:SI (match_operand:SI 1 "arm_rhs_operand" "r,I")
		  (match_operand:SI 2 "arm_rhs_operand" "rI,r")))]
  ""
  "*
  switch (which_alternative)
    {
    case 0:
      return (arm_output_asm_insn (\"sub\\t%0, %1, %2\", operands));
    case 1:
      return (arm_output_asm_insn (\"rsb\\t%0, %2, %1\", operands));
    }
")

(define_insn ""
  [(set (reg:CC_NOOV 24)
	(compare:CC_NOOV (minus:SI (match_operand:SI 1 "arm_rhs_operand" "r,I")
				 (match_operand:SI 2 "arm_rhs_operand" "rI,r"))
			 (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=r,r")
	(minus:SI (match_dup 1) (match_dup 2)))]
  ""
  "*
  switch (which_alternative)
    {
    case 0:
      return arm_output_asm_insn (\"subs\\t%0, %1, %2\", operands);
    case 1:
      return arm_output_asm_insn (\"rsbs\\t%0, %2, %1\", operands);
    }
"
[(set_attr "conds" "set")])

(define_insn "decscc"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
        (minus:SI (match_operand:SI 1 "s_register_operand" "0,?r")
		  (match_operator:SI 2 "comparison_operator"
                   [(reg 24) (const_int 0)])))]
  ""
  "*
  if (which_alternative == 1)
    arm_output_asm_insn (\"mov%D2\\t%0, %1\", operands);
  return arm_output_asm_insn (\"sub%d2\\t%0, %1, #1\", operands);
"
[(set_attr "conds" "use")
 (set_attr "length" "*,2")])

(define_insn "subsf3"
  [(set (match_operand:SF 0 "s_register_operand" "=f,f")
	(minus:SF (match_operand:SF 1 "fpu_rhs_operand" "f,G")
		  (match_operand:SF 2 "fpu_rhs_operand" "fG,f")))]
  ""
  "*
  switch (which_alternative)
    {
    case 0:
      return (arm_output_asm_insn (\"sufs\\t%0, %1, %2\", operands));
    case 1:
      return (arm_output_asm_insn (\"rsfs\\t%0, %2, %1\", operands));
    }
"
[(set_attr "type" "float")])

(define_insn "subdf3"
  [(set (match_operand:DF 0 "s_register_operand" "=f,f")
	(minus:DF (match_operand:DF 1 "fpu_rhs_operand" "f,G")
		  (match_operand:DF 2 "fpu_rhs_operand" "fG,f")))]
  ""
  "*
  switch (which_alternative)
    {
    case 0:
      return (arm_output_asm_insn (\"sufd\\t%0, %1, %2\", operands));
    case 1:
      return (arm_output_asm_insn (\"rsfd\\t%0, %2, %1\", operands));
    }
"
[(set_attr "type" "float")])

(define_insn ""
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(minus:DF (float_extend:DF
		   (match_operand:SF 1 "s_register_operand" "f"))
		  (match_operand:DF 2 "fpu_rhs_operand" "fG")))]
  ""
  "*
  return arm_output_asm_insn (\"sufd\\t%0, %1, %2\", operands);
"
[(set_attr "type" "float")])

(define_insn ""
  [(set (match_operand:DF 0 "s_register_operand" "=f,f")
	(minus:DF (match_operand:DF 1 "fpu_rhs_operand" "f,G")
		  (float_extend:DF
		   (match_operand:SF 2 "s_register_operand" "f,f"))))]
  ""
  "*
  switch (which_alternative)
    {
    case 0:
      return (arm_output_asm_insn (\"sufd\\t%0, %1, %2\", operands));
    case 1:
      return (arm_output_asm_insn (\"rsfd\\t%0, %2, %1\", operands));
    }
"
[(set_attr "type" "float")])

(define_insn ""
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(minus:DF (float_extend:DF
		   (match_operand:SF 1 "s_register_operand" "f"))
		  (float_extend:DF
		   (match_operand:SF 2 "s_register_operand" "f"))))]
  ""
  "*
  return arm_output_asm_insn (\"sufd\\t%0, %1, %2\", operands);
"
[(set_attr "type" "float")])

(define_insn "subxf3"
  [(set (match_operand:XF 0 "s_register_operand" "=f,f")
	(minus:XF (match_operand:XF 1 "fpu_rhs_operand" "f,G")
		  (match_operand:XF 2 "fpu_rhs_operand" "fG,f")))]
  "ENABLE_XF_PATTERNS"
  "*
  switch (which_alternative)
    {
    case 0:
      return (arm_output_asm_insn (\"sufe\\t%0, %1, %2\", operands));
    case 1:
      return (arm_output_asm_insn (\"rsfe\\t%0, %2, %1\", operands));
    }
"
[(set_attr "type" "float")])

;; Multiplication insns

;; Use `&' and then `0' to prevent the operands 0 and 1 being the same
(define_insn "mulsi3"
  [(set (match_operand:SI 0 "s_register_operand" "=&r,&r")
	(mult:SI (match_operand:SI 2 "s_register_operand" "r,r")
		 (match_operand:SI 1 "s_register_operand" "%?r,0")))]
  ""
  "*
  return (arm_output_asm_insn (\"mul\\t%0, %2, %1\", operands));
")

(define_insn ""
  [(set (reg:CC_NOOV 24)
	(compare:CC_NOOV (mult:SI
			  (match_operand:SI 2 "s_register_operand" "r,r")
			  (match_operand:SI 1 "s_register_operand" "%?r,0"))
			 (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=&r,&r")
	(mult:SI (match_dup 2) (match_dup 1)))]
  ""
  "*
  return (arm_output_asm_insn (\"muls\\t%0, %2, %1\", operands));
"
[(set_attr "conds" "set")])

(define_insn ""
  [(set (reg:CC_NOOV 24)
	(compare:CC_NOOV (mult:SI
			  (match_operand:SI 2 "s_register_operand" "r,r")
			  (match_operand:SI 1 "s_register_operand" "%?r,0"))
			 (const_int 0)))
   (clobber (match_scratch:SI 0 "=&r,&r"))]
  ""
  "*
  return (arm_output_asm_insn (\"muls\\t%0, %2, %1\", operands));
"
[(set_attr "conds" "set")])

;; Unnamed templates to match MLA instruction.

(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=&r,&r,&r,&r")
	(plus:SI
	  (mult:SI (match_operand:SI 2 "s_register_operand" "r,r,r,r")
		   (match_operand:SI 1 "s_register_operand" "%r,0,r,0"))
	  (match_operand:SI 3 "s_register_operand" "?r,r,0,0")))]
  ""
  "*
  return (arm_output_asm_insn (\"mla\\t%0, %2, %1, %3\", operands));
")

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
  "*
  return (arm_output_asm_insn (\"mlas\\t%0, %2, %1, %3\", operands));
"
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
  "*
  return (arm_output_asm_insn (\"mlas\\t%0, %2, %1, %3\", operands));
"
[(set_attr "conds" "set")])

(define_insn "mulsf3"
  [(set (match_operand:SF 0 "s_register_operand" "=f")
	(mult:SF (match_operand:SF 1 "s_register_operand" "f")
		 (match_operand:SF 2 "fpu_rhs_operand" "fG")))]
  ""
  "*
  return (arm_output_asm_insn (\"fmls\\t%0, %1, %2\", operands));
"
[(set_attr "type" "float")])

(define_insn "muldf3"
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(mult:DF (match_operand:DF 1 "s_register_operand" "f")
		 (match_operand:DF 2 "fpu_rhs_operand" "fG")))]
  ""
  "*
  return (arm_output_asm_insn (\"mufd\\t%0, %1, %2\", operands));
"
[(set_attr "type" "float")])

(define_insn ""
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(mult:DF (float_extend:DF
		  (match_operand:SF 1 "s_register_operand" "f"))
		 (match_operand:DF 2 "fpu_rhs_operand" "fG")))]
  ""
  "*
  return (arm_output_asm_insn (\"mufd\\t%0, %1, %2\", operands));
"
[(set_attr "type" "float")])

(define_insn ""
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(mult:DF (match_operand:DF 1 "s_register_operand" "f")
		 (float_extend:DF
		  (match_operand:SF 2 "s_register_operand" "f"))))]
  ""
  "*
  return (arm_output_asm_insn (\"mufd\\t%0, %1, %2\", operands));
"
[(set_attr "type" "float")])

(define_insn ""
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(mult:DF (float_extend:DF
		  (match_operand:SF 1 "s_register_operand" "f"))
		 (float_extend:DF
		  (match_operand:SF 2 "s_register_operand" "f"))))]
  ""
  "*
  return (arm_output_asm_insn (\"mufd\\t%0, %1, %2\", operands));
"
[(set_attr "type" "float")])

(define_insn "mulxf3"
  [(set (match_operand:XF 0 "s_register_operand" "=f")
	(mult:XF (match_operand:XF 1 "s_register_operand" "f")
		 (match_operand:XF 2 "fpu_rhs_operand" "fG")))]
  "ENABLE_XF_PATTERNS"
  "*
  return (arm_output_asm_insn (\"mufe\\t%0, %1, %2\", operands));
"
[(set_attr "type" "float")])

;; Division insns

(define_insn "divsf3"
  [(set (match_operand:SF 0 "s_register_operand" "=f,f")
	(div:SF (match_operand:SF 1 "fpu_rhs_operand" "f,G")
		(match_operand:SF 2 "fpu_rhs_operand" "fG,f")))]
  ""
  "*
  switch (which_alternative)
    {
    case 0:
      return (arm_output_asm_insn (\"fdvs\\t%0, %1, %2\", operands));
    case 1:
      return (arm_output_asm_insn (\"frds\\t%0, %2, %1\", operands));
    }
"
[(set_attr "type" "float")])

(define_insn "divdf3"
  [(set (match_operand:DF 0 "s_register_operand" "=f,f")
	(div:DF (match_operand:DF 1 "fpu_rhs_operand" "f,G")
		(match_operand:DF 2 "fpu_rhs_operand" "fG,f")))]
  ""
  "*
  switch (which_alternative)
    {
    case 0:
      return (arm_output_asm_insn (\"dvfd\\t%0, %1, %2\", operands));
    case 1:
      return (arm_output_asm_insn (\"rdfd\\t%0, %2, %1\", operands));
    }
"
[(set_attr "type" "float")])

(define_insn ""
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(div:DF (float_extend:DF
		 (match_operand:SF 1 "s_register_operand" "f"))
		(match_operand:DF 2 "fpu_rhs_operand" "fG")))]
  ""
  "*
  return (arm_output_asm_insn (\"dvfd\\t%0, %1, %2\", operands));
"
[(set_attr "type" "float")])

(define_insn ""
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(div:DF (match_operand:DF 1 "fpu_rhs_operand" "fG")
		(float_extend:DF
		 (match_operand:SF 2 "s_register_operand" "f"))))]
  ""
  "*
  return (arm_output_asm_insn (\"rdfd\\t%0, %2, %1\", operands));
"
[(set_attr "type" "float")])

(define_insn ""
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(div:DF (float_extend:DF
		 (match_operand:SF 1 "s_register_operand" "f"))
		(float_extend:DF
		 (match_operand:SF 2 "s_register_operand" "f"))))]
  ""
  "*
  return (arm_output_asm_insn (\"dvfd\\t%0, %1, %2\", operands));
"
[(set_attr "type" "float")])

(define_insn "divxf3"
  [(set (match_operand:XF 0 "s_register_operand" "=f,f")
	(div:XF (match_operand:XF 1 "fpu_rhs_operand" "f,G")
		(match_operand:XF 2 "fpu_rhs_operand" "fG,f")))]
  "ENABLE_XF_PATTERNS"
  "*
  switch (which_alternative)
    {
    case 0:
      return (arm_output_asm_insn (\"dvfe\\t%0, %1, %2\", operands));
    case 1:
      return (arm_output_asm_insn (\"rdfe\\t%0, %2, %1\", operands));
    }
"
[(set_attr "type" "float")])

;; Modulo insns

(define_insn "modsf3"
  [(set (match_operand:SF 0 "s_register_operand" "=f")
	(mod:SF (match_operand:SF 1 "s_register_operand" "f")
		(match_operand:SF 2 "fpu_rhs_operand" "fG")))]
  ""
  "*
  return (arm_output_asm_insn (\"rmfs\\t%0, %1, %2\", operands));
"
[(set_attr "type" "float")])

(define_insn "moddf3"
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(mod:DF (match_operand:DF 1 "s_register_operand" "f")
		(match_operand:DF 2 "fpu_rhs_operand" "fG")))]
  ""
  "*
  return (arm_output_asm_insn (\"rmfd\\t%0, %1, %2\", operands));
"
[(set_attr "type" "float")])

(define_insn ""
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(mod:DF (float_extend:DF
		 (match_operand:SF 1 "s_register_operand" "f"))
		(match_operand:DF 2 "fpu_rhs_operand" "fG")))]
  ""
  "*
  return (arm_output_asm_insn (\"rmfd\\t%0, %1, %2\", operands));
"
[(set_attr "type" "float")])

(define_insn ""
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(mod:DF (match_operand:DF 1 "s_register_operand" "f")
		(float_extend:DF
		 (match_operand:SF 2 "s_register_operand" "f"))))]
  ""
  "*
  return (arm_output_asm_insn (\"rmfd\\t%0, %1, %2\", operands));
"
[(set_attr "type" "float")])

(define_insn ""
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(mod:DF (float_extend:DF
		 (match_operand:SF 1 "s_register_operand" "f"))
		(float_extend:DF
		 (match_operand:SF 2 "s_register_operand" "f"))))]
  ""
  "*
  return (arm_output_asm_insn (\"rmfd\\t%0, %1, %2\", operands));
"
[(set_attr "type" "float")])

(define_insn "modxf3"
  [(set (match_operand:XF 0 "s_register_operand" "=f")
	(mod:XF (match_operand:XF 1 "s_register_operand" "f")
		(match_operand:XF 2 "fpu_rhs_operand" "fG")))]
  "ENABLE_XF_PATTERNS"
  "*
  return (arm_output_asm_insn (\"rmfe\\t%0, %1, %2\", operands));
"
[(set_attr "type" "float")])

;; Boolean and,ior,xor insns

(define_insn "anddi3"
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(and:DI (match_operand:DI 1 "s_register_operand" "%0,0")
		(match_operand:DI 2 "s_register_operand" "r,0")))]
  ""
  "*
  arm_output_asm_insn (\"and\\t%0, %1, %2\", operands);
  return (arm_output_asm_insn (\"and\\t%R0, %R1, %R2\", operands));
"
[(set_attr "length" "2")])

(define_insn ""
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(and:DI (zero_extend:DI
		 (match_operand:SI 2 "s_register_operand" "r,r"))
		(match_operand:DI 1 "s_register_operand" "?r,0")))]
  ""
  "*
  arm_output_asm_insn (\"and\\t%0, %1, %2\", operands);
  return arm_output_asm_insn (\"mov\\t%R0, #0\", operands);
"
[(set_attr "length" "2")])

(define_insn ""
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(and:DI (sign_extend:DI
		 (match_operand:SI 2 "s_register_operand" "r,r"))
		(match_operand:DI 1 "s_register_operand" "?r,0")))]
  ""
  "*
  arm_output_asm_insn (\"and\\t%0, %1, %2\", operands);
  return arm_output_asm_insn (\"and\\t%R0, %R1, %2, asr #31\", operands);
"
[(set_attr "length" "2")])

(define_insn "andsi3"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(and:SI (match_operand:SI 1 "s_register_operand" "r")
		(match_operand:SI 2 "arm_not_operand" "rK")))]
  ""
  "*
  if (GET_CODE (operands[2]) == CONST_INT
      && !const_ok_for_arm (INTVAL (operands[2])))
    {
      operands[2] = gen_rtx (CONST_INT, VOIDmode, ~INTVAL (operands[2]));
      return arm_output_asm_insn (\"bic\\t%0, %1, %2\", operands);
    }
  return arm_output_asm_insn (\"and\\t%0, %1, %2\", operands);
")

(define_insn ""
  [(set (reg:CC_NOOV 24)
	(compare:CC_NOOV (and:SI (match_operand:SI 1 "s_register_operand" "r")
		 		 (match_operand:SI 2 "arm_not_operand" "rK"))
			 (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(and:SI (match_dup 1) (match_dup 2)))]
  ""
  "*
  if (GET_CODE (operands[2]) == CONST_INT
      && !const_ok_for_arm (INTVAL (operands[2])))
    {
      operands[2] = gen_rtx (CONST_INT, VOIDmode, ~INTVAL (operands[2]));
      return arm_output_asm_insn (\"bics\\t%0, %1, %2\", operands);
    }
  return arm_output_asm_insn (\"ands\\t%0, %1, %2\", operands);
"
[(set_attr "conds" "set")])

(define_insn ""
  [(set (reg:CC_NOOV 24)
	(compare:CC_NOOV (and:SI (match_operand:SI 0 "s_register_operand" "r")
		 		 (match_operand:SI 1 "arm_rhs_operand" "rI"))
			 (const_int 0)))]
  ""
  "*
  return arm_output_asm_insn (\"tst\\t%0, %1\", operands);
"
[(set_attr "conds" "set")])

(define_insn ""
  [(set (reg:CC_NOOV 24)
	(compare:CC_NOOV (and:SI (match_operand:SI 0 "s_register_operand" "r")
		 		 (match_operand:SI 1 "immediate_operand" "K"))
			 (const_int 0)))
   (clobber (match_scratch:SI 3 "=r"))]
  "const_ok_for_arm (~INTVAL (operands[1]))"
  "*
  operands[1] = GEN_INT (~INTVAL (operands[1]));
  return arm_output_asm_insn (\"bics\\t%3, %0, %1\", operands);
"
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
  operands[1] = gen_rtx (CONST_INT, VOIDmode, mask << INTVAL (operands[2]));
  return arm_output_asm_insn (\"tst\\t%0, %1\", operands);
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
  operands[1] = gen_rtx (CONST_INT, VOIDmode, mask << INTVAL (operands[2]));
  arm_output_asm_insn (\"ldrb\\t%3, %0\", operands);
  return arm_output_asm_insn (\"tst\\t%3, %1\", operands);
}
"
[(set_attr "conds" "set")
 (set_attr "length" "2")])

;; constants for op 2 will never be given to these patterns.
(define_insn ""
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(and:DI (not:DI (match_operand:DI 2 "s_register_operand" "r,0"))
		(match_operand:DI 1 "s_register_operand" "0,r")))]
  ""
  "*
  arm_output_asm_insn (\"bic\\t%0, %1, %2\", operands);
  return arm_output_asm_insn (\"bic\\t%R0, %R1, %R2\", operands);
"
[(set_attr "length" "2")])
  
(define_insn ""
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(and:DI (not:DI (zero_extend:DI
			 (match_operand:SI 2 "s_register_operand" "r,r")))
		(match_operand:DI 1 "s_register_operand" "?r,0")))]
  ""
  "*
  arm_output_asm_insn (\"bic\\t%0, %1, %2\", operands);
  if (REGNO (operands[1]) != REGNO (operands[0]))
    return arm_output_asm_insn (\"mov\\t%R0, %R1\", operands);
  return \"\";
"
[(set_attr "length" "2,1")])
  
(define_insn ""
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(and:DI (not:DI (sign_extend:DI
			 (match_operand:SI 2 "s_register_operand" "r,r")))
		(match_operand:DI 1 "s_register_operand" "?r,0")))]
  ""
  "*
  arm_output_asm_insn (\"bic\\t%0, %1, %2\", operands);
  return arm_output_asm_insn (\"bic\\t%R0, %R1, %2, asr #31\", operands);
"
[(set_attr "length" "2")])
  
(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(and:SI (not:SI (match_operand:SI 2 "s_register_operand" "r"))
		(match_operand:SI 1 "s_register_operand" "r")))]
  ""
  "*
  return (arm_output_asm_insn (\"bic\\t%0, %1, %2\", operands));
")

(define_insn ""
  [(set (reg:CC_NOOV 24)
	(compare:CC_NOOV (and:SI
			  (not:SI (match_operand:SI 2 "s_register_operand" "r"))
			  (match_operand:SI 1 "s_register_operand" "r"))
			 (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(and:SI (not:SI (match_dup 2)) (match_dup 1)))]
  ""
  "*
  return (arm_output_asm_insn (\"bics\\t%0, %1, %2\", operands));
"
[(set_attr "conds" "set")])

(define_insn ""
  [(set (reg:CC_NOOV 24)
	(compare:CC_NOOV (and:SI
			  (not:SI (match_operand:SI 2 "s_register_operand" "r"))
			  (match_operand:SI 1 "s_register_operand" "r"))
			 (const_int 0)))
   (clobber (match_scratch:SI 0 "=r"))]
  ""
  "*
  return (arm_output_asm_insn (\"bics\\t%0, %1, %2\", operands));
"
[(set_attr "conds" "set")])

(define_insn "iordi3"
  [(set (match_operand:DI 0 "s_register_operand" "=&r")
	(ior:DI (match_operand:DI 1 "s_register_operand" "%0")
		(match_operand:DI 2 "s_register_operand" "r")))]
  ""
  "*
  arm_output_asm_insn (\"orr\\t%0, %1, %2\", operands);
  return (arm_output_asm_insn (\"orr\\t%R0, %R1, %R2\", operands));
"
[(set_attr "length" "2")])

(define_insn ""
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(ior:DI (zero_extend:DI
		 (match_operand:SI 2 "s_register_operand" "r,r"))
		(match_operand:DI 1 "s_register_operand" "?r,0")))]
  ""
  "*
  arm_output_asm_insn (\"orr\\t%0, %1, %2\", operands);
  if (REGNO (operands[0]) != REGNO (operands[1]))
    return (arm_output_asm_insn (\"mov\\t%R0, %R1\", operands));
  return \"\";
"
[(set_attr "length" "2,1")])

(define_insn ""
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(ior:DI (sign_extend:DI
		 (match_operand:SI 2 "s_register_operand" "r,r"))
		(match_operand:DI 1 "s_register_operand" "?r,0")))]
  ""
  "*
  arm_output_asm_insn (\"orr\\t%0, %1, %2\", operands);
  return (arm_output_asm_insn (\"orr\\t%R0, %R1, %2, asr #31\", operands));
"
[(set_attr "length" "2")])

(define_insn "iorsi3"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(ior:SI (match_operand:SI 1 "s_register_operand" "r")
		(match_operand:SI 2 "arm_rhs_operand" "rI")))]
  ""
  "*
  return (arm_output_asm_insn (\"orr\\t%0, %1, %2\", operands));
")

(define_insn ""
  [(set (reg:CC_NOOV 24)
	(compare:CC_NOOV (ior:SI (match_operand:SI 1 "s_register_operand" "%r")
				 (match_operand:SI 2 "arm_rhs_operand" "rI"))
			 (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(ior:SI (match_dup 1) (match_dup 2)))]
  ""
  "*
  return arm_output_asm_insn (\"orrs\\t%0, %1, %2\", operands);
"
[(set_attr "conds" "set")])

(define_insn ""
  [(set (reg:CC_NOOV 24)
	(compare:CC_NOOV (ior:SI (match_operand:SI 1 "s_register_operand" "%r")
				 (match_operand:SI 2 "arm_rhs_operand" "rI"))
			 (const_int 0)))
   (clobber (match_scratch:SI 0 "=r"))]
  ""
  "*
  return arm_output_asm_insn (\"orrs\\t%0, %1, %2\", operands);
"
[(set_attr "conds" "set")])

(define_insn "xordi3"
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(xor:DI (match_operand:DI 1 "s_register_operand" "%0,0")
		(match_operand:DI 2 "s_register_operand" "r,0")))]
  ""
  "*
  arm_output_asm_insn (\"eor\\t%0, %1, %2\", operands);
  return arm_output_asm_insn (\"eor\\t%R0, %R1, %R2\", operands);
"
[(set_attr "length" "2")])

(define_insn ""
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(xor:DI (zero_extend:DI
		 (match_operand:SI 2 "s_register_operand" "r,r"))
		(match_operand:DI 1 "s_register_operand" "?r,0")))]
  ""
  "*
  arm_output_asm_insn (\"eor\\t%0, %1, %2\", operands);
  if (REGNO (operands[0]) != REGNO (operands[1]))
    return arm_output_asm_insn (\"mov\\t%R0, %R1\", operands);
  return \"\";
"
[(set_attr "length" "2,1")])

(define_insn ""
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(xor:DI (sign_extend:DI
		 (match_operand:SI 2 "s_register_operand" "r,r"))
		(match_operand:DI 1 "s_register_operand" "?r,0")))]
  ""
  "*
  arm_output_asm_insn (\"eor\\t%0, %1, %2\", operands);
  return arm_output_asm_insn (\"eor\\t%R0, %R1, %2, asr #31\", operands);
"
[(set_attr "length" "2")])

(define_insn "xorsi3"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(xor:SI (match_operand:SI 1 "s_register_operand" "r")
		(match_operand:SI 2 "arm_rhs_operand" "rI")))]
  ""
  "*
  return (arm_output_asm_insn (\"eor\\t%0, %1, %2\", operands));
")

(define_insn ""
  [(set (reg:CC_NOOV 24)
	(compare:CC_NOOV (xor:SI (match_operand:SI 1 "s_register_operand" "r")
				 (match_operand:SI 2 "arm_rhs_operand" "rI"))
			 (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(xor:SI (match_dup 1) (match_dup 2)))]
  ""
  "*
  return arm_output_asm_insn (\"eors\\t%0, %1, %2\", operands);
"
[(set_attr "conds" "set")])

(define_insn ""
  [(set (reg:CC_NOOV 24)
	(compare:CC_NOOV (xor:SI (match_operand:SI 0 "s_register_operand" "r")
				 (match_operand:SI 1 "arm_rhs_operand" "rI"))
			 (const_int 0)))]
  ""
  "*
  return arm_output_asm_insn (\"teq\\t%0, %1\", operands);
"
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
  "*
  arm_output_asm_insn (\"orr\\t%0, %1, %2\", operands);
  return arm_output_asm_insn (\"bic\\t%0, %0, %3\", operands);
"
[(set_attr "length" "2")])



;; Minimum and maximum insns

(define_insn "smaxsi3"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r,r")
	(smax:SI (match_operand:SI 1 "s_register_operand" "0,r,?r")
		 (match_operand:SI 2 "arm_rhs_operand" "rI,0,rI")))
   (clobber (reg:CC 24))]
  ""
  "*
  arm_output_asm_insn (\"cmp\\t%1, %2\", operands);
  if (which_alternative != 0)
    arm_output_asm_insn (\"movge\\t%0, %1\", operands);
  if (which_alternative != 1)
    return arm_output_asm_insn (\"movlt\\t%0, %2\", operands);
  return \"\";
"
[(set_attr "conds" "clob")
 (set_attr "length" "2,2,3")])

(define_insn "sminsi3"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r,r")
	(smin:SI (match_operand:SI 1 "s_register_operand" "0,r,?r")
		 (match_operand:SI 2 "arm_rhs_operand" "rI,0,rI")))
   (clobber (reg:CC 24))]
  ""
  "*
  arm_output_asm_insn (\"cmp\\t%1, %2\", operands);
  if (which_alternative != 0)
    arm_output_asm_insn (\"movle\\t%0, %1\", operands);
  if (which_alternative != 1)
    return arm_output_asm_insn (\"movgt\\t%0, %2\", operands);
  return \"\";
"
[(set_attr "conds" "clob")
 (set_attr "length" "2,2,3")])

(define_insn "umaxsi3"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r,r")
	(umax:SI (match_operand:SI 1 "s_register_operand" "0,r,?r")
		 (match_operand:SI 2 "arm_rhs_operand" "rI,0,rI")))
   (clobber (reg:CC 24))]
  ""
  "*
  arm_output_asm_insn (\"cmp\\t%1, %2\", operands);
  if (which_alternative != 0)
    arm_output_asm_insn (\"movcs\\t%0, %1\", operands);
  if (which_alternative != 1)
    return arm_output_asm_insn (\"movcc\\t%0, %2\", operands);
  return \"\";
"
[(set_attr "conds" "clob")
 (set_attr "length" "2,2,3")])

(define_insn "uminsi3"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r,r")
	(umin:SI (match_operand:SI 1 "s_register_operand" "0,r,?r")
		 (match_operand:SI 2 "arm_rhs_operand" "rI,0,rI")))
   (clobber (reg:CC 24))]
  ""
  "*
  arm_output_asm_insn (\"cmp\\t%1, %2\", operands);
  if (which_alternative != 0)
    arm_output_asm_insn (\"movcc\\t%0, %1\", operands);
  if (which_alternative != 1)
    return arm_output_asm_insn (\"movcs\\t%0, %2\", operands);
  return \"\";
"
[(set_attr "conds" "clob")
 (set_attr "length" "2,2,3")])

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
  arm_output_asm_insn (\"cmp\\t%1, %2\", operands);
  arm_output_asm_insn (\"str%d3\\t%1, %0\", operands);
  return arm_output_asm_insn (\"str%D3\\t%2, %0\", operands);
"
[(set_attr "conds" "clob")
 (set_attr "length" "3")
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
  char buf[100];
  enum rtx_code code = GET_CODE (operands[4]);
  char *inst = arithmetic_instr (operands[4], TRUE);

  operands[5] = gen_rtx (minmax_code (operands[5]), SImode, operands[2],
			 operands[3]);
  arm_output_asm_insn (\"cmp\\t%2, %3\", operands);
  sprintf (buf, \"%s%%d5\\t%%0, %%1, %%2\", inst);
  arm_output_asm_insn (buf, operands);
  if (which_alternative != 0 || operands[3] != const0_rtx
      || (code != PLUS && code != MINUS && code != IOR && code != XOR))
  {
    sprintf (buf, \"%s%%D5\\t%%0, %%1, %%3\", inst);
    return arm_output_asm_insn (buf, operands);
  }
  return \"\";
}
"
[(set_attr "conds" "clob")
 (set_attr "length" "3")])


;; Shift and rotation insns

(define_insn "ashlsi3"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(ashift:SI (match_operand:SI 1 "s_register_operand" "r")
		   (match_operand:SI 2 "arm_rhs_operand" "rn")))]
  ""
  "*
  return (output_shifted_move (ASHIFT, operands));
")

(define_insn "ashrsi3"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(ashiftrt:SI (match_operand:SI 1 "s_register_operand" "r")
		     (match_operand:SI 2 "arm_rhs_operand" "rn")))]
  ""
  "*
  return (output_shifted_move (ASHIFTRT, operands));
")

;; lshlsi3 is not defined because shift counts cannot be negative
;; An unnamed pattern is needed for expansion of zero_extend.

(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(lshift:SI (match_operand:SI 1 "s_register_operand" "r")
		   (match_operand:SI 2 "arm_rhs_operand" "rn")))]
  ""
  "*
  return (output_shifted_move (LSHIFT, operands));
")

(define_insn "lshrsi3"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(lshiftrt:SI (match_operand:SI 1 "s_register_operand" "r")
		     (match_operand:SI 2 "arm_rhs_operand" "rn")))]
  ""
  "*
  return (output_shifted_move (LSHIFTRT, operands));
")

;; rotlsi3 is not defined yet to see what happens

(define_insn "rotrsi3"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(rotatert:SI (match_operand:SI 1 "s_register_operand" "r,r")
		     (match_operand:SI 2 "arm_rhs_operand" "r,n")))]
  ""
  "*
  switch (which_alternative)
    {
    case 0:
      return (arm_output_asm_insn (\"mov\\t%0, %1, ror %2\", operands));
    case 1:
      if (INTVAL(operands[2]) > 31)
	operands[2] = gen_rtx (CONST_INT, VOIDmode, INTVAL (operands[2]) % 32);
      return (arm_output_asm_insn (\"mov\\t%0, %1, ror %2\", operands));
    }
")

(define_insn ""
  [(set (reg:CC_NOOV 24)
	(compare:CC_NOOV (match_operator:SI 1 "shift_operator"
			  [(match_operand:SI 2 "s_register_operand" "r")
			   (match_operand:SI 3 "arm_rhs_operand" "rn")])
			 (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(match_op_dup 1 [(match_dup 2) (match_dup 3)]))]
  ""
  "*
{
  char buf[100];

  sprintf (buf, \"movs\\t%%0, %%2, %s %%3\",
	   shift_instr (GET_CODE (operands[1]), &operands[3]));
  return arm_output_asm_insn (buf, operands);
}
"
[(set_attr "conds" "set")])

(define_insn ""
  [(set (reg:CC_NOOV 24)
	(compare:CC_NOOV (match_operator:SI 1 "shift_operator"
			  [(match_operand:SI 2 "s_register_operand" "r")
			   (match_operand:SI 3 "arm_rhs_operand" "rn")])
			 (const_int 0)))
   (clobber (match_scratch:SI 0 "=r"))]
  ""
  "*
{
  char buf[100];

  sprintf (buf, \"movs\\t%%0, %%2, %s %%3\",
	   shift_instr (GET_CODE (operands[1]), &operands[3]));
  return arm_output_asm_insn (buf, operands);
}
"
[(set_attr "conds" "set")])

(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(not:SI (match_operator:SI 1 "shift_operator"
		 [(match_operand:SI 2 "s_register_operand" "r")
		  (match_operand:SI 3 "arm_rhs_operand" "rn")])))]
  ""
  "*
{
  char buf[100];
  sprintf (buf, \"mvn\\t%%0, %%2, %s %%3\",
	   shift_instr (GET_CODE (operands[1]), &operands[3]));
  return arm_output_asm_insn (buf, operands);
}
")

(define_insn ""
  [(set (reg:CC_NOOV 24)
	(compare:CC_NOOV (not:SI (match_operator:SI 1 "shift_operator"
			  [(match_operand:SI 2 "s_register_operand" "r")
			   (match_operand:SI 3 "arm_rhs_operand" "rn")]))
			 (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(not:SI (match_op_dup 1 [(match_dup 2) (match_dup 3)])))]
  ""
  "*
{
  char buf[100];
  sprintf (buf, \"mvns\\t%%0, %%2, %s %%3\",
	   shift_instr (GET_CODE (operands[1]), &operands[3]));
  return arm_output_asm_insn (buf, operands);
}
"
[(set_attr "conds" "set")])

(define_insn ""
  [(set (reg:CC_NOOV 24)
	(compare:CC_NOOV (not:SI (match_operator:SI 1 "shift_operator"
			  [(match_operand:SI 2 "s_register_operand" "r")
			   (match_operand:SI 3 "arm_rhs_operand" "rn")]))
			 (const_int 0)))
   (clobber (match_scratch:SI 0 "=r"))]
  ""
  "*
{
  char buf[100];
  sprintf (buf, \"mvns\\t%%0, %%2, %s %%3\",
	   shift_instr (GET_CODE (operands[1]), &operands[3]));
  return arm_output_asm_insn (buf, operands);
}
"
[(set_attr "conds" "set")])


;; Unary arithmetic insns

(define_insn "negdi2"
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(neg:DI (match_operand:DI 1 "s_register_operand" "?r,0")))]
  ""
  "*
  arm_output_asm_insn (\"rsbs\\t%0, %1, #0\", operands);
  return (arm_output_asm_insn (\"rsc\\t%R0, %R1, #0\", operands));
"
[(set_attr "conds" "clob")
 (set_attr "length" "2")])

(define_insn "negsi2"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(neg:SI (match_operand:SI 1 "s_register_operand" "r")))]
  ""
  "*
  return (arm_output_asm_insn (\"rsb\\t%0, %1, #0\", operands));
")

(define_insn "negsf2"
  [(set (match_operand:SF 0 "s_register_operand" "=f")
	(neg:SF (match_operand:SF 1 "s_register_operand" "f")))]
  ""
  "*
  return (arm_output_asm_insn (\"mnfs\\t%0, %1\", operands));
"
[(set_attr "type" "float")])

(define_insn "negdf2"
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(neg:DF (match_operand:DF 1 "s_register_operand" "f")))]
  ""
  "*
  return (arm_output_asm_insn (\"mnfd\\t%0, %1\", operands));
"
[(set_attr "type" "float")])

(define_insn ""
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(neg:DF (float_extend:DF
		 (match_operand:SF 1 "s_register_operand" "f"))))]
  ""
  "*
  return (arm_output_asm_insn (\"mnfd\\t%0, %1\", operands));
"
[(set_attr "type" "float")])

(define_insn "negxf2"
  [(set (match_operand:XF 0 "s_register_operand" "=f")
	(neg:XF (match_operand:XF 1 "s_register_operand" "f")))]
  "ENABLE_XF_PATTERNS"
  "*
  return (arm_output_asm_insn (\"mnfe\\t%0, %1\", operands));
"
[(set_attr "type" "float")])

;; abssi2 doesn't really clobber the condition codes if a different register
;; is being set.  To keep things simple, assume during rtl manipulations that
;; it does, but tell the final scan operator the truth.  Similarly for
;; (neg (abs...))

(define_insn "abssi2"
  [(set (match_operand:SI 0 "s_register_operand" "=r,&r")
	(abs:SI (match_operand:SI 1 "s_register_operand" "0,r")))
   (clobber (reg 24))]
  ""
  "*
  switch (which_alternative)
    {
    case 0:
      arm_output_asm_insn (\"cmp\\t%0, #0\", operands);
      return arm_output_asm_insn (\"rsblt\\t%0, %0, #0\", operands);
    case 1:
      arm_output_asm_insn (\"eor\\t%0, %1, %1, asr #31\", operands);
      return arm_output_asm_insn (\"sub\\t%0, %0, %1, asr #31\", operands);
    }
"
[(set_attr "conds" "clob,*")
 (set_attr "length" "2")])

(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=r,&r")
	(neg:SI (abs:SI (match_operand:SI 1 "s_register_operand" "0,r"))))
   (clobber (reg 24))]
  ""
  "*
  switch (which_alternative)
    {
    case 0:
      arm_output_asm_insn (\"cmp\\t%0, #0\", operands);
      return arm_output_asm_insn (\"rsbgt\\t%0, %0, #0\", operands);
    case 1:
      arm_output_asm_insn (\"eor\\t%0, %1, %1, asr #31\", operands);
      return arm_output_asm_insn (\"rsb\\t%0, %0, %1, asr #31\", operands);
    }
"
[(set_attr "conds" "clob,*")
 (set_attr "length" "2")])

(define_insn "abssf2"
  [(set (match_operand:SF 0 "s_register_operand" "=f")
	 (abs:SF (match_operand:SF 1 "s_register_operand" "f")))]
  ""
  "*
  return (arm_output_asm_insn (\"abss\\t%0, %1\", operands));
"
[(set_attr "type" "float")])

(define_insn "absdf2"
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(abs:DF (match_operand:DF 1 "s_register_operand" "f")))]
  ""
  "*
  return (arm_output_asm_insn (\"absd\\t%0, %1\", operands));
"
[(set_attr "type" "float")])

(define_insn ""
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(abs:DF (float_extend:DF
		 (match_operand:SF 1 "s_register_operand" "f"))))]
  ""
  "*
  return (arm_output_asm_insn (\"absd\\t%0, %1\", operands));
"
[(set_attr "type" "float")])

(define_insn "absxf2"
  [(set (match_operand:XF 0 "s_register_operand" "=f")
	(abs:XF (match_operand:XF 1 "s_register_operand" "f")))]
  "ENABLE_XF_PATTERNS"
  "*
  return (arm_output_asm_insn (\"abse\\t%0, %1\", operands));
"
[(set_attr "type" "float")])

(define_insn "sqrtsf2"
  [(set (match_operand:SF 0 "s_register_operand" "=f")
	(sqrt:SF (match_operand:SF 1 "s_register_operand" "f")))]
  ""
  "*
  return (arm_output_asm_insn (\"sqts\\t%0, %1\", operands));
"
[(set_attr "type" "float_em")])

(define_insn "sqrtdf2"
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(sqrt:DF (match_operand:DF 1 "s_register_operand" "f")))]
  ""
  "*
  return (arm_output_asm_insn (\"sqtd\\t%0, %1\", operands));
"
[(set_attr "type" "float_em")])

(define_insn ""
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(sqrt:DF (float_extend:DF
		  (match_operand:SF 1 "s_register_operand" "f"))))]
  ""
  "*
  return (arm_output_asm_insn (\"sqtd\\t%0, %1\", operands));
"
[(set_attr "type" "float_em")])

(define_insn "sqrtxf2"
  [(set (match_operand:XF 0 "s_register_operand" "=f")
	(sqrt:XF (match_operand:XF 1 "s_register_operand" "f")))]
  "ENABLE_XF_PATTERNS"
  "*
  return (arm_output_asm_insn (\"sqte\\t%0, %1\", operands));
"
[(set_attr "type" "float_em")])

(define_insn "sinsf2"
  [(set (match_operand:SF 0 "s_register_operand" "=f")
	(unspec:SF [(match_operand:SF 1 "s_register_operand" "f")] 0))]
  ""
  "*
  return arm_output_asm_insn (\"sins\\t%0, %1\", operands);
"
[(set_attr "type" "float_em")])

(define_insn "sindf2"
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(unspec:DF [(match_operand:DF 1 "s_register_operand" "f")] 0))]
  ""
  "*
  return arm_output_asm_insn (\"sind\\t%0, %1\", operands);
"
[(set_attr "type" "float_em")])

(define_insn ""
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(unspec:DF [(float_extend:DF
		     (match_operand:SF 1 "s_register_operand" "f"))] 0))]
  ""
  "*
  return arm_output_asm_insn (\"sind\\t%0, %1\", operands);
"
[(set_attr "type" "float_em")])

(define_insn "sinxf2"
  [(set (match_operand:XF 0 "s_register_operand" "=f")
	(unspec:XF [(match_operand:XF 1 "s_register_operand" "f")] 0))]
  "ENABLE_XF_PATTERNS"
  "*
  return arm_output_asm_insn (\"sine\\t%0, %1\", operands);
"
[(set_attr "type" "float_em")])

(define_insn "cossf2"
  [(set (match_operand:SF 0 "s_register_operand" "=f")
	(unspec:SF [(match_operand:SF 1 "s_register_operand" "f")] 1))]
  ""
  "*
  return arm_output_asm_insn (\"coss\\t%0, %1\", operands);
"
[(set_attr "type" "float_em")])

(define_insn "cosdf2"
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(unspec:DF [(match_operand:DF 1 "s_register_operand" "f")] 1))]
  ""
  "*
  return arm_output_asm_insn (\"cosd\\t%0, %1\", operands);
"
[(set_attr "type" "float_em")])

(define_insn ""
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(unspec:DF [(float_extend:DF
		     (match_operand:SF 1 "s_register_operand" "f"))] 1))]
  ""
  "*
  return arm_output_asm_insn (\"cosd\\t%0, %1\", operands);
"
[(set_attr "type" "float_em")])

(define_insn "cosxf2"
  [(set (match_operand:XF 0 "s_register_operand" "=f")
	(unspec:XF [(match_operand:XF 1 "s_register_operand" "f")] 1))]
  "ENABLE_XF_PATTERNS"
  "*
  return arm_output_asm_insn (\"cose\\t%0, %1\", operands);
"
[(set_attr "type" "float_em")])

(define_insn "one_cmpldi2"
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(not:DI (match_operand:DI 1 "s_register_operand" "?r,0")))]
  ""
  "*
  arm_output_asm_insn (\"mvn\\t%0, %1\", operands);
  return arm_output_asm_insn (\"mvn\\t%R0, %R1\", operands);
"
[(set_attr "length" "2")])

(define_insn "one_cmplsi2"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(not:SI (match_operand:SI 1 "s_register_operand" "r")))]
  ""
  "*
  return (arm_output_asm_insn (\"mvn\\t%0, %1\", operands));
")

(define_insn ""
  [(set (reg:CC_NOOV 24)
	(compare:CC_NOOV (not:SI (match_operand:SI 1 "s_register_operand" "r"))
			 (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(not:SI (match_dup 1)))]
  ""
  "*
  return (arm_output_asm_insn (\"mvns\\t%0, %1\", operands));
"
[(set_attr "conds" "set")])

(define_insn ""
  [(set (reg:CC_NOOV 24)
	(compare:CC_NOOV (not:SI (match_operand:SI 1 "s_register_operand" "r"))
			 (const_int 0)))
   (clobber (match_scratch:SI 0 "=r"))]
  ""
  "*
  return (arm_output_asm_insn (\"mvns\\t%0, %1\", operands));
"
[(set_attr "conds" "set")])

;; Fixed <--> Floating conversion insns

(define_insn "floatsisf2"
  [(set (match_operand:SF 0 "s_register_operand" "=f")
	(float:SF (match_operand:SI 1 "s_register_operand" "r")))]
  ""
  "*
  return (arm_output_asm_insn (\"flts\\t%0, %1\", operands));
"
[(set_attr "type" "r_2_f")])

(define_insn "floatsidf2"
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(float:DF (match_operand:SI 1 "s_register_operand" "r")))]
  ""
  "*
  return (arm_output_asm_insn (\"fltd\\t%0, %1\", operands));
"
[(set_attr "type" "r_2_f")])

(define_insn "floatsixf2"
  [(set (match_operand:XF 0 "s_register_operand" "=f")
	(float:XF (match_operand:SI 1 "s_register_operand" "r")))]
  "ENABLE_XF_PATTERNS"
  "*
  return (arm_output_asm_insn (\"flte\\t%0, %1\", operands));
"
[(set_attr "type" "r_2_f")])

(define_insn "fix_truncsfsi2"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(fix:SI (match_operand:SF 1 "s_register_operand" "f")))]
  ""
  "*
  return arm_output_asm_insn (\"fixz\\t%0, %1\", operands);
"
[(set_attr "type" "f_2_r")])

(define_insn "fix_truncdfsi2"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(fix:SI (match_operand:DF 1 "s_register_operand" "f")))]
  ""
  "*
  return arm_output_asm_insn (\"fixz\\t%0, %1\", operands);
"
[(set_attr "type" "f_2_r")])

(define_insn "fix_truncxfsi2"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(fix:SI (match_operand:XF 1 "s_register_operand" "f")))]
  "ENABLE_XF_PATTERNS"
  "*
  return arm_output_asm_insn (\"fixz\\t%0, %1\", operands);
"
[(set_attr "type" "f_2_r")])

;; Truncation insns

(define_insn "truncdfsf2"
  [(set (match_operand:SF 0 "s_register_operand" "=f")
	(float_truncate:SF
	 (match_operand:DF 1 "s_register_operand" "f")))]
  ""
  "*
  return (arm_output_asm_insn (\"mvfs\\t%0, %1\", operands));
"
[(set_attr "type" "float")])

(define_insn "truncxfsf2"
  [(set (match_operand:SF 0 "s_register_operand" "=f")
	(float_truncate:SF
	 (match_operand:XF 1 "s_register_operand" "f")))]
  "ENABLE_XF_PATTERNS"
  "*
  return (arm_output_asm_insn (\"mvfs\\t%0, %1\", operands));
"
[(set_attr "type" "float")])

(define_insn "truncxfdf2"
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(float_truncate:DF
	 (match_operand:XF 1 "s_register_operand" "f")))]
  "ENABLE_XF_PATTERNS"
  "*
  return (arm_output_asm_insn (\"mvfd\\t%0, %1\", operands));
"
[(set_attr "type" "float")])

;; Zero and sign extension instructions.

(define_insn "zero_extendsidi2"
  [(set (match_operand:DI 0 "s_register_operand" "=r")
        (zero_extend:DI (match_operand:SI 1 "s_register_operand" "r")))]
  ""
  "*
  if (REGNO (operands[1]) != REGNO (operands[0]))
    arm_output_asm_insn (\"mov\\t%0, %1\", operands);
  return arm_output_asm_insn (\"mov\\t%R0, #0\", operands);
"
[(set_attr "length" "2")])

(define_insn "zero_extendqidi2"
  [(set (match_operand:DI 0 "s_register_operand" "=r,r")
	(zero_extend:DI (match_operand:QI 1 "nonimmediate_operand" "r,m")))]
  ""
  "*
  switch (which_alternative)
    {
    case 0:
      arm_output_asm_insn (\"and\\t%0, %1, #255\", operands);
      break;
    case 1:
      arm_output_asm_insn (\"ldrb\\t%0, %1\",operands);
      break;
    }
  return arm_output_asm_insn (\"mov\\t%R0, #0\", operands);
"
[(set_attr "length" "2")
 (set_attr "type" "*,load")])

(define_insn "extendsidi2"
  [(set (match_operand:DI 0 "s_register_operand" "=r")
        (sign_extend:DI (match_operand:SI 1 "s_register_operand" "r")))]
  ""
  "*
  if (REGNO (operands[1]) != REGNO (operands[0]))
    arm_output_asm_insn (\"mov\\t%0, %1\", operands);
  return arm_output_asm_insn (\"mov\\t%R0, %0, asr #31\", operands);
"
[(set_attr "length" "2")])

(define_expand "zero_extendhisi2"
  [(set (match_dup 2)
	(ashift:SI (match_operand:HI 1 "s_register_operand" "")
		   (const_int 16)))
   (set (match_operand:SI 0 "s_register_operand" "")
	(lshiftrt:SI (match_dup 2)
		     (const_int 16)))]
  ""
  "
{ operands[1] = gen_lowpart (SImode, operands[1]);
  operands[2] = gen_reg_rtx (SImode); }")

(define_insn "zero_extendqihi2"
  [(set (match_operand:HI 0 "s_register_operand" "=r")
	(zero_extend:HI
	 (match_operand:QI 1 "s_register_operand" "r")))]
  ""
  "*
  return (arm_output_asm_insn (\"and\\t%0, %1, #255\\t@ zero_extendqihi2\", operands));
")

(define_insn ""
  [(set (reg:CC_NOOV 24)
	(compare:CC_NOOV (match_operand:QI 1 "s_register_operand" "r")
			 (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(zero_extend:HI (match_dup 1)))]
  ""
  "*
  return arm_output_asm_insn (\"ands\\t%0, %1, #255\", operands);
"
[(set_attr "conds" "set")])

(define_insn ""
  [(set (reg:CC_NOOV 24)
	(compare:CC_NOOV (match_operand:QI 0 "s_register_operand" "r")
			 (const_int 0)))]
  ""
  "*
  return arm_output_asm_insn (\"tst\\t%0, #255\", operands);
"
[(set_attr "conds" "set")])

(define_insn "zero_extendqisi2"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(zero_extend:SI
	 (match_operand:QI 1 "nonimmediate_operand" "r,m")))]
  ""
  "*
  switch (which_alternative)
    {
    case 0:
      return (arm_output_asm_insn (\"and\\t%0, %1, #255\\t@ zero_extendqisi2\", operands));
    case 1:
      return (arm_output_asm_insn (\"ldrb\\t%0, %1\\t@ zero_extendqisi2\", operands));
    }
"
[(set_attr "type" "*,load")])

(define_insn ""
  [(set (reg:CC_NOOV 24)
	(compare:CC_NOOV (match_operand:QI 1 "s_register_operand" "r")
			 (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(zero_extend:SI (match_dup 1)))]
  ""
  "*
  return arm_output_asm_insn (\"ands\\t%0, %1, #255\", operands);
"
[(set_attr "conds" "set")])

(define_insn ""
  [(set (reg:CC_NOOV 24)
	(compare:CC_NOOV (match_operand:QI 1 "s_register_operand" "r")
			 (const_int 0)))
   (set (match_operand:QI 0 "s_register_operand" "=r")
	(match_dup 1))]
  ""
  "*
  return arm_output_asm_insn (\"ands\\t%0, %1, #255\", operands);
"
[(set_attr "conds" "set")])

(define_expand "extendhisi2"
  [(set (match_dup 2)
	(ashift:SI (match_operand:HI 1 "s_register_operand" "")
		   (const_int 16)))
   (set (match_operand:SI 0 "s_register_operand" "")
	(ashiftrt:SI (match_dup 2)
		     (const_int 16)))]
  ""
  "
{ operands[1] = gen_lowpart (SImode, operands[1]);
  operands[2] = gen_reg_rtx (SImode); }")

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
  "*
  return (arm_output_asm_insn (\"mvfd\\t%0, %1\", operands));
"
[(set_attr "type" "float")])

(define_insn "extendsfxf2"
  [(set (match_operand:XF 0 "s_register_operand" "=f")
	(float_extend:XF (match_operand:SF 1 "s_register_operand" "f")))]
  "ENABLE_XF_PATTERNS"
  "*
  return (arm_output_asm_insn (\"mvfe\\t%0, %1\", operands));
")

(define_insn "extenddfxf2"
  [(set (match_operand:XF 0 "s_register_operand" "=f")
	(float_extend:XF (match_operand:DF 1 "s_register_operand" "f")))]
  "ENABLE_XF_PATTERNS"
  "*
  return (arm_output_asm_insn (\"mvfe\\t%0, %1\", operands));
"
[(set_attr "type" "float")])


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
;;    return (arm_output_asm_insn (template, operands));
;;  }")


(define_insn "movdi"
  [(set (match_operand:DI 0 "di_operand" "=r,r,r,o<>,r")
	(match_operand:DI 1 "di_operand" "rK,n,o<>,r,F"))]
  ""
  "*
  return (output_move_double (operands));
"
[(set_attr "length" "2,8,2,2,8")
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
      int n = INTVAL (operands[1]);
      rtx tmpreg, tmpreg2;
      int i, n_ones = 0, first = 1, last = 0;

      if (GET_CODE (operands[0]) != REG
          && GET_CODE (operands[0]) != SUBREG)
        abort ();
      for (i = 0; i < 32; i++)
        if (n & 1 << i)
          n_ones++;
      /* These loops go the opposite way around to those in arm.c so that
         the last constant may be more likely to be eliminted into the
         next instruction */

      if (n_ones > 16)
        {
          n = (~n) & 0xffffffff;
          for (i = 30; i >= 0; i -= 2)
            {
              if (n & (3 << i))
                {
                  i -= 6;
                  if (i < 0)
                    i = 0;
                  if ((n & (255 << i)) == n)
                    last = 1;
                  if (first)
                    {
                      rtx equal;
                      rtx insn =
                        emit_insn (gen_movsi (tmpreg = (reload_in_progress
                                                        || reload_completed)
                                                      ? operands[0]
                                                      : gen_reg_rtx (SImode),
                                      equal = gen_rtx (CONST_INT, VOIDmode,
                                                       ~(n & (255 << i)))));
                      first = 0;
                    }
                  else
                    {
                      rtx constant;
                      rtx insn =
                        emit_insn (gen_subsi3 (tmpreg2 = (reload_in_progress
                                                          || reload_completed
                                                          || last)
                                                        ? operands[0]
                                                        : gen_reg_rtx (SImode),
                                               tmpreg,
                                    constant = gen_rtx (CONST_INT, VOIDmode,
                                                        n & (255 << i))));
                      tmpreg = tmpreg2;
                    }
                  n &= ~(255 << i);
                }
            }
        }
      else
        {
          for (i = 30; i >= 0; i -= 2)
            {
              if (n & (3 << i))
                {
                  i -= 6;
                  if (i < 0)
                    i = 0;
                  if ((n & (255 << i)) == n)
                    last = 1;
                  if (first)
                    {
                      rtx equal;
                      rtx insn =
                        emit_insn (gen_movsi (tmpreg = (reload_in_progress
                                                        || reload_completed)
                                                      ? operands[0]
                                                      : gen_reg_rtx (SImode),
                                      equal = gen_rtx (CONST_INT, VOIDmode,
                                                       n & (255 << i))));
                      first = 0;
                    }
                  else
                    {
                      rtx constant;
                      rtx insn =
                        emit_insn (gen_addsi3 (tmpreg2 = (reload_in_progress
                                                          || reload_completed
                                                          || last)
                                                        ? operands[0]
                                                        : gen_reg_rtx (SImode),
                                               tmpreg,
                                    constant = gen_rtx (CONST_INT, VOIDmode,
                                                        n & (255 << i))));
                      tmpreg = tmpreg2;
                    }
                  n &= ~(255 << i);
                }
            }
        }
      DONE;
    }
")

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=r,r,r,m,r")
	(match_operand:SI 1 "general_operand"  "m,K,r,r,S"))]
  "(register_operand (operands[0], SImode)
    && (GET_CODE (operands[1]) != CONST_INT
        || const_ok_for_arm (INTVAL (operands[1]))
        || const_ok_for_arm (~INTVAL (operands[1])))
    && (GET_CODE (operands[1]) != SYMBOL_REF
	|| CONSTANT_ADDRESS_P (operands[1])))
   || register_operand (operands[1], SImode)"
  "*
  switch (which_alternative)
    {
    case 2:
      return (arm_output_asm_insn (\"mov\\t%0, %1\", operands));
    case 1:
      if (!const_ok_for_arm (INTVAL (operands[1])))
	{
	  operands[1] = gen_rtx (CONST_INT, VOIDmode, ~INTVAL (operands[1]));
	  return arm_output_asm_insn (\"mvn\\t%0, %1\", operands);
	}
      return arm_output_asm_insn (\"mov\\t%0, %1\", operands);
    case 0:
      if (GET_CODE (XEXP (operands[1], 0)) == SYMBOL_REF
	  &&  CONSTANT_POOL_ADDRESS_P (XEXP (operands[1], 0)))
	return (arm_output_llc (operands));
      else
	return (arm_output_asm_insn (\"ldr\\t%0, %1\", operands));
    case 3:
      return (arm_output_asm_insn (\"str\\t%1, %0\", operands));
    case 4:
      return output_load_symbol (operands);
    }
"
[(set_attr "length" "2,*,*,*,4")
 (set_attr "type" "load,*,*,store1,*")])

;; If copying one reg to another we can set the condition codes according to
;; its value.  Such a move is common after a return from subroutine and the
;; result is being tested against zero.

(define_insn ""
  [(set (reg:CC 24) (compare (match_operand:SI 1 "s_register_operand" "r")
			     (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=r") (match_dup 1))]
  ""
  "*
  if (GET_CODE (operands[0]) == REG && GET_CODE (operands[1]) == REG
      && REGNO (operands[0]) == REGNO (operands[1]))
    return arm_output_asm_insn (\"cmp\\t%0, #0\", operands);
  return arm_output_asm_insn (\"subs\\t%0, %1, #0\", operands);
"
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

;; Subroutine to store a half word integer constant into memory.
;; Operand 0 is the constant
;; Operand 1 is the destination address in a register (SImode)

(define_expand "storeinthi"
  [;; store the low byte
   (set (mem:QI (match_operand:SI 1 "" "")) (match_operand 0 "" ""))
   ;; store the high byte
   (set (mem:QI (match_dup 3)) (match_dup 2))]
  ""
  "
{
  int value = INTVAL (operands[0]);
  enum rtx_code code = GET_CODE (operands[1]);

  if ((code == PLUS || code == MINUS)
      && (GET_CODE (XEXP (operands[1], 1)) == REG
	  || GET_CODE (XEXP (operands[1], 0)) != REG))
  operands[1] = force_reg (SImode, operands[1]);

  operands[0] = force_reg (QImode, gen_rtx (CONST_INT, VOIDmode, value & 255));
  operands[2] = force_reg (QImode,
			   gen_rtx (CONST_INT, VOIDmode,(value>>8) & 255));
  operands[3] = plus_constant (operands[1], 1);
}
")

(define_expand "movhi"
  [(set (match_operand:HI 0 "general_operand" "")
	(match_operand:HI 1 "general_operand" ""))]
  ""
  "
{
  rtx insn;

  if (reload_in_progress || reload_completed)
    insn = gen_rtx (SET, VOIDmode, operands[0], operands[1]);
  else
    {
      if (GET_CODE (operands[0]) == MEM)
	{
	  if (GET_CODE (operands[1]) == CONST_INT)
	    {
	      insn = gen_storeinthi (operands[1], XEXP (operands[0],0));
	    }
	  else
	    {
	      if (GET_CODE (operands[1]) == MEM)
		operands[1] = force_reg (HImode, operands[1]);
	      insn = gen_storehi (operands[1], XEXP (operands[0], 0));
	    }
	}
      else if (GET_CODE (operands[1]) == CONST_INT
	       && !(const_ok_for_arm (INTVAL (operands[1]))
		   || const_ok_for_arm (~INTVAL (operands[1]))))
	{
	  rtx reg, reg2;

	  /* no need to be clever, this will always take two insns.
	     The top sixteen bits should be all zeros or all ones. */
	  if (INTVAL (operands[1]) < 0)
	    {
	      emit_insn (gen_movsi (reg = gen_reg_rtx (SImode),
				    GEN_INT (INTVAL (operands[1])
					     | ~(0x0ff00))));
	      emit_insn (gen_addsi3 (reg2 = gen_reg_rtx (SImode), reg,
				     GEN_INT (-((~INTVAL (operands[1]))
					        & 0xff))));
	    }
	  else
	    {
	      emit_insn (gen_movsi (reg = gen_reg_rtx (SImode),
				    GEN_INT (INTVAL (operands[1]) & 0xff00)));
	      emit_insn (gen_addsi3 (reg2 = gen_reg_rtx (SImode), reg,
				     GEN_INT (INTVAL (operands[1]) & 0x00ff)));
	    }
	  insn = gen_rtx (SET, HImode, operands[0],
			  gen_rtx (SUBREG, HImode, reg2, 0));
	}
      else
	insn = gen_rtx (SET, VOIDmode, operands[0], operands[1]);
    }

  emit_insn (insn);
  DONE;
}")

;; Pattern to recognise insn generated default case above

(define_insn ""
  [(set (match_operand:HI 0 "general_operand" "=r,r,r,m")
	(match_operand:HI 1 "general_operand"  "r,K,m,r"))]
  "(register_operand (operands[0], HImode)
    && (GET_CODE (operands[1]) != CONST_INT
	|| const_ok_for_arm (INTVAL (operands[1]))
	|| const_ok_for_arm (~INTVAL (operands[1]))))
   || register_operand (operands[1], HImode)"
  "*
  switch (which_alternative)
    {
      case 1:
	if (!const_ok_for_arm (INTVAL (operands[1])))
	  {
	    operands[1] = GEN_INT (~INTVAL (operands[1]));
	    return arm_output_asm_insn (\"mvn\\t%0, %1\", operands);
	  }
	/* fall through */
      case 0:
	return arm_output_asm_insn (\"mov\\t%0, %1\\t@movhi\", operands);
      case 2:
	return arm_output_asm_insn (\"ldr\\t%0, %1\\t@movhi\", operands);
      case 3:
	return arm_output_asm_insn (\"str\\t%1, %0\\t@movhi\", operands);
    }
"
[(set_attr "type" "*,*,load,store1")])

(define_expand "reload_outhi"
  [(parallel [(match_operand:HI 0 "reload_memory_operand" "=o")
	      (match_operand:HI 1 "s_register_operand" "r")
	      (match_operand:SI 2 "s_register_operand" "=&r")])]
  ""
  "
  arm_reload_out_hi (operands);
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
      rtx reg;
      if (GET_CODE (operands[1]) == CONST_INT)
	{
	  emit_insn (gen_movsi (reg = gen_reg_rtx (SImode), operands[1]));
	  operands[1] = gen_rtx (SUBREG, QImode, reg, 0);
	}
    }
  if (GET_CODE (operands[0]) == MEM)
    operands[1] = force_reg (QImode, operands[1]);
")


(define_insn ""
  [(set (match_operand:QI 0 "general_operand" "=r,r,r,m")
	(match_operand:QI 1 "general_operand" "r,K,m,r"))]
  "register_operand (operands[0], QImode)
   || register_operand (operands[1], QImode)"
  "*
  switch (which_alternative)
    {
    case 1:
      if (INTVAL (operands[1]) < 0)
	{
	  operands[1] = GEN_INT (~INTVAL (operands[1]));
	  return arm_output_asm_insn (\"mvn\\t%0, %1\", operands);
	}
    case 0:
      return (arm_output_asm_insn (\"mov\\t%0, %1\", operands));
    case 2:
      return (arm_output_asm_insn (\"ldrb\\t%0, %1\", operands));
    case 3:
      return (arm_output_asm_insn (\"strb\\t%1, %0\", operands));
    }
"
[(set_attr "type" "*,*,load,store1")])

(define_insn "movsf"
  [(set (match_operand:SF 0 "general_operand" "=f,f,f,m,f,r,r,r,m")
	(match_operand:SF 1 "general_operand" "fG,H,m,f,r,f,r,m,r"))]
  ""
  "*
{
  REAL_VALUE_TYPE r;

  switch (which_alternative)
    {
    case 0:
      return arm_output_asm_insn (\"mvfs\\t%0, %1\", operands);
    case 1:
      REAL_VALUE_FROM_CONST_DOUBLE (r, operands[1]);
      r = REAL_VALUE_NEGATE (r);
      operands[1] = CONST_DOUBLE_FROM_REAL_VALUE (r, GET_MODE (operands[1]));
      return arm_output_asm_insn (\"mnfs\\t%0, %1\", operands);
    case 2:
      return arm_output_asm_insn (\"ldfs\\t%0, %1\", operands);
    case 3:
      return arm_output_asm_insn (\"stfs\\t%1, %0\", operands);
    case 4:
      arm_output_asm_insn(\"stmfd\\tsp!, {%1}\", operands);
      return arm_output_asm_insn (\"ldfs\\t%0, [sp],#4\", operands);
    case 5:
      arm_output_asm_insn(\"stfs\\t%1, [sp,#-4]!\", operands);
      return arm_output_asm_insn (\"ldmfd\\tsp!, {%0}\", operands);
    case 6:
      return arm_output_asm_insn (\"mov\\t%0, %1\", operands);
    case 7:
      return arm_output_asm_insn (\"ldr\\t%0, %1\\t@ float\", operands);
    case 8:
      return arm_output_asm_insn (\"str\\t%1, %0\\t@ float\", operands);
  }
}
"
[(set_attr "length" "1,1,1,1,2,2,1,1,1")
 (set_attr "type" "float,float,f_load,f_store,r_mem_f,f_mem_r,*,load,store1")])

(define_expand "movdf"
  [(parallel [(set (match_operand:DF 0 "general_operand" "")
		   (match_operand:DF 1 "general_operand" ""))
	      (clobber (match_scratch:SI 2 ""))])]
  ""
  "
  if (GET_CODE (operands[0]) == MEM)
    operands[1] = force_reg (DFmode, operands[1]);
")

;; Reloading a df mode value stored in integer regs to memory can require a
;; scratch reg.
(define_expand "reload_outdf"
  [(parallel [(set (match_operand:DF 0 "reload_memory_operand" "=o")
		   (match_operand:DF 1 "s_register_operand" "r"))
	      (clobber (match_operand:SI 2 "s_register_operand" "=&r"))])]
  ""
  "")

(define_insn ""
  [(set (match_operand:DF 0 "general_operand" "=r,Q,r,o,f,f,f,f,m,!f,!r,r")
	(match_operand:DF 1 "general_operand" 
	 	"Q,r,?o,?r,?f,!G,!H,m,f,r,f,??r"))
   (clobber (match_scratch:SI 2 "=X,X,X,&r,X,X,X,X,X,X,X,X"))]
  "GET_CODE (operands[0]) != MEM || register_operand (operands[1], DFmode)"
  "*
{
  REAL_VALUE_TYPE r;
  rtx ops[3];

  switch (which_alternative)
    {
    case 0:
      operands[1] = XEXP (operands[1], 0);
      return arm_output_asm_insn (\"ldmia\\t%1, {%0, %R0}\\t@ double\",
				  operands);
    case 1:
      operands[0] = XEXP (operands[0], 0);
      return arm_output_asm_insn (\"stmia\\t%0, {%1, %R1}\\t@ double\",
				  operands);
    case 2:
      ops[0] = operands[0];
      ops[1] = XEXP (XEXP (operands[1], 0), 0);
      ops[2] = XEXP (XEXP (operands[1], 0), 1);
      if (!INTVAL (ops[2]) || const_ok_for_arm (INTVAL (ops[2])))
	arm_output_asm_insn (\"add\\t%0, %1, %2\", ops);
      else
	arm_output_asm_insn (\"sub\\t%0, %1, #%n2\", ops);
      return arm_output_asm_insn (\"ldmia\\t%0, {%0, %R0}\\t@ double\",
				  operands);
    case 3:

      ops[0] = operands[2];
      ops[1] = XEXP (XEXP (operands[0], 0), 0);
      ops[2] = XEXP (XEXP (operands[0], 0), 1);
      if (!INTVAL (ops[2]) || const_ok_for_arm (INTVAL (ops[2])))
	arm_output_asm_insn (\"add\\t%0, %1, %2\", ops);
      else
	arm_output_asm_insn (\"sub\\t%0, %1, #%n2\", ops);
      return arm_output_asm_insn (\"stmia\\t%2, {%1, %R1}\\t@ double\",
				  operands);
    case 4:
    case 5:
      return arm_output_asm_insn (\"mvfd\\t%0, %1\", operands);
    case 6:
      REAL_VALUE_FROM_CONST_DOUBLE (r, operands[1]);
      r = REAL_VALUE_NEGATE (r);
      operands[1] = CONST_DOUBLE_FROM_REAL_VALUE (r, GET_MODE (operands[1]));
      return arm_output_asm_insn (\"mnfd\\t%0, %1\", operands);
    case 7: return arm_output_asm_insn (\"ldfd\\t%0, %1\", operands);
    case 8: return arm_output_asm_insn (\"stfd\\t%1, %0\", operands);
    case 9: return output_mov_double_fpu_from_arm (operands);
    case 10: return output_mov_double_arm_from_fpu (operands);
    case 11: return output_move_double (operands);
    }
}
"
[(set_attr "length" "1,1,2,2,1,1,1,1,1,2,2,2")
 (set_attr "type" 
"load,store2,load,store2,float,float,float,f_load,f_store,r_mem_f,f_mem_r,*")])

(define_insn "movxf"
  [(set (match_operand:XF 0 "general_operand" "=f,f,f,m,f,r,r")
	(match_operand:XF 1 "general_operand" "fG,H,m,f,r,f,r"))]
  "ENABLE_XF_PATTERNS"
  "*
{
  REAL_VALUE_TYPE r;

  switch (which_alternative)
    {
    case 0: return arm_output_asm_insn (\"mvfe\\t%0, %1\", operands);
    case 1:
      REAL_VALUE_FROM_CONST_DOUBLE (r, operands[1]);
      r = REAL_VALUE_NEGATE (r);
      operands[1] = CONST_DOUBLE_FROM_REAL_VALUE (r, GET_MODE (operands[1]));
      return arm_output_asm_insn (\"mnfe\\t%0, %1\", operands);
    case 2: return arm_output_asm_insn (\"ldfe\\t%0, %1\", operands);
    case 3: return arm_output_asm_insn (\"stfe\\t%1, %0\", operands);
    case 4: return output_mov_long_double_fpu_from_arm (operands);
    case 5: return output_mov_long_double_arm_from_fpu (operands);
    case 6: return output_mov_long_double_arm_from_arm (operands);
    }
}
"
[(set_attr "length" "1,1,1,1,2,2,3")
 (set_attr "type" "float,float,f_load,f_store,r_mem_f,f_mem_r,*")])


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

  return arm_output_asm_insn (\"ldmia\\t%0!, {%1-%2}\\t@ load multiple\", ops);
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

  return arm_output_asm_insn (\"ldmia\\t%0, {%1-%2}\\t@ load multiple\", ops);
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

  return arm_output_asm_insn (\"stmia\\t%0!, {%1-%2}\\t@ str multiple\", ops);
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

  return arm_output_asm_insn (\"stmia\\t%0, {%1-%2}\\t@ str multiple\", ops);
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

;; If optimizing, output redundant moves with REG_NOTES on them, this 
;; produces better code.

(define_expand "movstrsi"
  [(set (match_operand:BLK 0 "general_operand" "=m")
        (match_operand:BLK 1 "general_operand" "m"))
   (use (match_operand:SI 2 "immediate_operand" "n"))
   (use (match_operand:SI 3 "immediate_operand" "n"))
   (clobber (reg:SI 0))
   (clobber (reg:SI 1))
   (clobber (reg:SI 2))
   (clobber (reg:SI 3))
   (clobber (match_scratch:SI 4 "=+r"))
   (clobber (match_scratch:SI 5 "=+r"))]
  ""
  "
{
  int words_to_go;
  int i, r;
  rtx const_sxteen = gen_rtx (CONST_INT, SImode, 16);
  rtx src = gen_reg_rtx (SImode);
  rtx dst = gen_reg_rtx (SImode);
  rtx st_src, st_dst, end_src, end_dst, fin_src, fin_dst;
  extern int optimize;

  if (GET_CODE (operands[2]) != CONST_INT
      || GET_CODE (operands[3]) != CONST_INT
      || INTVAL (operands[2]) % 4 != 0
      || INTVAL (operands[2]) < 4
      || INTVAL (operands[2]) > 64
      || INTVAL (operands[3]) < 4
      || INTVAL (operands[3]) % 4 != 0)
    FAIL;
  emit_move_insn (dst, st_dst = force_reg (SImode, XEXP (operands[0], 0)));
  emit_move_insn (src, st_src = force_reg (SImode, XEXP (operands[1], 0)));
  fin_src = src;
  fin_dst = dst;

  for (i = 0, words_to_go = INTVAL (operands[2]) / 4; words_to_go >= 2; i+=4)
    {
      emit_insn (arm_gen_load_multiple (0, words_to_go > 4 ? 4 : words_to_go,
                                        src, TRUE, TRUE));
      emit_insn (arm_gen_store_multiple (0, words_to_go > 4 ? 4 : words_to_go,
                                        dst, TRUE, TRUE));
      if (optimize)
        for (r = (words_to_go > 4) ? 3 : words_to_go - 1; r >= 0; r--)
          {
            rtx note;
            note = emit_move_insn (gen_reg_rtx (SImode),
                                   gen_rtx (REG, SImode, r));
            REG_NOTES (note) = gen_rtx (EXPR_LIST, REG_EQUIV,
                                        gen_rtx (MEM, SImode,
                                              plus_constant (st_src, 4*(i+r))),
                                        REG_NOTES (note));
            REG_NOTES (note) = gen_rtx (EXPR_LIST, REG_EQUIV,
                                        gen_rtx (MEM, SImode,
                                              plus_constant (st_dst, 4*(i+r))),
                                        REG_NOTES (note));
          }
      words_to_go -= words_to_go < 4 ? words_to_go : 4;
    }
  if (words_to_go)
  {
    rtx sreg;

    emit_move_insn (sreg = gen_reg_rtx (SImode), gen_rtx (MEM, SImode, src));
    emit_move_insn (fin_src = gen_reg_rtx (SImode), plus_constant (src, 4));
    emit_move_insn (gen_rtx (MEM, SImode, dst), sreg);
    emit_move_insn (fin_dst = gen_reg_rtx (SImode), plus_constant (dst, 4));
  }
  if (optimize)
    {
      /* Insns for the REG_NOTES: These notes tell the optimiser where the
         index registers have got to so that consecutive block moves of
         contiguous data work efficiently */

      end_src = emit_move_insn (fin_src, fin_src);
      REG_NOTES (end_src) = gen_rtx(EXPR_LIST, REG_EQUAL,
                                  plus_constant (st_src, INTVAL (operands[2])),
                                  REG_NOTES (end_src));
      end_dst = emit_move_insn (fin_dst, fin_dst);
      REG_NOTES (end_dst) = gen_rtx(EXPR_LIST, REG_EQUAL,
                                  plus_constant (st_dst, INTVAL (operands[2])),
                                  REG_NOTES (end_dst));
    }
  DONE;
}
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
	(compare (match_operand:SI 1 "s_register_operand" "r")
		 (match_operand:SI 2 "arm_add_operand" "rL")))]
  ""
  "*
  if (GET_CODE (operands[2]) == CONST_INT
      && !const_ok_for_arm (INTVAL (operands[2])))
    return arm_output_asm_insn (\"cmn\\t%1, #%n2\", operands);
  return arm_output_asm_insn (\"cmp\\t%1, %2\", operands);
"
[(set_attr "conds" "set")])

(define_insn ""
  [(set (match_operand 0 "cc_register" "")
	(compare (match_operand:SI 1 "s_register_operand" "r")
		 (neg:SI (match_operand:SI 2 "s_register_operand" "r"))))]
  ""
  "*
  return arm_output_asm_insn (\"cmn\\t%1, %2\", operands);
"
[(set_attr "conds" "set")])

(define_insn ""
  [(set (match_operand 0 "cc_register" "")
	(compare (match_operand:SI 1 "s_register_operand" "r")
		 (match_operator:SI 2 "shift_operator"
		  [(match_operand:SI 3 "s_register_operand" "r")
		   (match_operand:SI 4 "arm_rhs_operand" "rn")])))]
  ""
  "*
  return output_shift_compare (operands, FALSE);
"
[(set_attr "conds" "set")])

(define_insn ""
  [(set (match_operand 0 "cc_register" "")
	(compare (match_operand:SI 1 "s_register_operand" "r")
		 (neg:SI (match_operator:SI 2 "shift_operator"
			  [(match_operand:SI 3 "s_register_operand" "r")
			   (match_operand:SI 4 "arm_rhs_operand" "rn")]))))]
  ""
  "*
  return output_shift_compare (operands, TRUE);
"
[(set_attr "conds" "set")])

(define_insn ""
  [(set (reg:CCFP 24)
	(compare:CCFP (match_operand:SF 0 "s_register_operand" "f,f")
		      (match_operand:SF 1 "fpu_add_operand" "fG,H")))]
  ""
  "*
{
  REAL_VALUE_TYPE r;

  switch (which_alternative)
    {
    case 0:
      return arm_output_asm_insn (\"cmf\\t%0, %1\", operands);
    case 1:
      REAL_VALUE_FROM_CONST_DOUBLE (r, operands[1]);
      r = REAL_VALUE_NEGATE (r);
      operands[1] = CONST_DOUBLE_FROM_REAL_VALUE (r, GET_MODE (operands[1]));
      return arm_output_asm_insn (\"cnf\\t%0, %1\", operands);
    }
}
"
[(set_attr "conds" "set")
 (set_attr "type" "f_2_r")])

(define_insn ""
  [(set (reg:CCFP 24)
	(compare:CCFP (match_operand:DF 0 "s_register_operand" "f,f")
		      (match_operand:DF 1 "fpu_add_operand" "fG,H")))]
  ""
  "*
{
  REAL_VALUE_TYPE r;

  switch (which_alternative)
    {
    case 0:
      return arm_output_asm_insn (\"cmf\\t%0, %1\", operands);
    case 1:
      REAL_VALUE_FROM_CONST_DOUBLE (r, operands[1]);
      r = REAL_VALUE_NEGATE (r);
      operands[1] = CONST_DOUBLE_FROM_REAL_VALUE (r, GET_MODE (operands[1]));
      return arm_output_asm_insn (\"cnf\\t%0, %1\", operands);
    }
}
"
[(set_attr "conds" "set")
 (set_attr "type" "f_2_r")])

(define_insn ""
  [(set (reg:CCFP 24)
	(compare:CCFP (float_extend:DF
		       (match_operand:SF 0 "s_register_operand" "f,f"))
		      (match_operand:DF 1 "fpu_add_operand" "fG,H")))]
  ""
  "*
{
  REAL_VALUE_TYPE r;

  switch (which_alternative)
    {
    case 0:
      return arm_output_asm_insn (\"cmf\\t%0, %1\", operands);
    case 1:
      REAL_VALUE_FROM_CONST_DOUBLE (r, operands[1]);
      r = REAL_VALUE_NEGATE (r);
      operands[1] = CONST_DOUBLE_FROM_REAL_VALUE (r, GET_MODE (operands[1]));
      return arm_output_asm_insn (\"cnf\\t%0, %1\", operands);
    }
}
"
[(set_attr "conds" "set")
 (set_attr "type" "f_2_r")])

(define_insn ""
  [(set (reg:CCFP 24)
	(compare:CCFP (match_operand:DF 0 "s_register_operand" "f")
		      (float_extend:DF
		       (match_operand:SF 1 "s_register_operand" "f"))))]
  ""
  "*
  return arm_output_asm_insn (\"cmf\\t%0, %1\", operands);
"
[(set_attr "conds" "set")
 (set_attr "type" "f_2_r")])

(define_insn ""
  [(set (reg:CCFP 24)
	(compare:CCFP (match_operand:XF 0 "s_register_operand" "f,f")
		      (match_operand:XF 1 "fpu_add_operand" "fG,H")))]
  "ENABLE_XF_PATTERNS"
  "*
{
  REAL_VALUE_TYPE r;

  switch (which_alternative)
    {
    case 0:
      return arm_output_asm_insn (\"cmf\\t%0, %1\", operands);
    case 1:
      REAL_VALUE_FROM_CONST_DOUBLE (r, operands[1]);
      r = REAL_VALUE_NEGATE (r);
      operands[1] = CONST_DOUBLE_FROM_REAL_VALUE (r, GET_MODE (operands[1]));
      return arm_output_asm_insn (\"cnf\\t%0, %1\", operands);
    }
}
"
[(set_attr "conds" "set")
 (set_attr "type" "f_2_r")])

(define_insn ""
  [(set (reg:CCFPE 24)
	(compare:CCFPE (match_operand:SF 0 "s_register_operand" "f,f")
		       (match_operand:SF 1 "fpu_add_operand" "fG,H")))]
  ""
  "*
{
  REAL_VALUE_TYPE r;

  switch (which_alternative)
    {
    case 0:
      return arm_output_asm_insn (\"cmfe\\t%0, %1\", operands);
    case 1:
      REAL_VALUE_FROM_CONST_DOUBLE (r, operands[1]);
      r = REAL_VALUE_NEGATE (r);
      operands[1] = CONST_DOUBLE_FROM_REAL_VALUE (r, GET_MODE (operands[1]));
      return arm_output_asm_insn (\"cnfe\\t%0, %1\", operands);
    }
}
"
[(set_attr "conds" "set")
 (set_attr "type" "f_2_r")])

(define_insn ""
  [(set (reg:CCFPE 24)
	(compare:CCFPE (match_operand:DF 0 "s_register_operand" "f,f")
		       (match_operand:DF 1 "fpu_add_operand" "fG,H")))]
  ""
  "*
{
  REAL_VALUE_TYPE r;

  switch (which_alternative)
    {
    case 0:
      return arm_output_asm_insn (\"cmfe\\t%0, %1\", operands);
    case 1:
      REAL_VALUE_FROM_CONST_DOUBLE (r, operands[1]);
      r = REAL_VALUE_NEGATE (r);
      operands[1] = CONST_DOUBLE_FROM_REAL_VALUE (r, GET_MODE (operands[1]));
      return arm_output_asm_insn (\"cnfe\\t%0, %1\", operands);
    }
}
"
[(set_attr "conds" "set")
 (set_attr "type" "f_2_r")])

(define_insn ""
  [(set (reg:CCFPE 24)
	(compare:CCFPE (float_extend:DF
			(match_operand:SF 0 "s_register_operand" "f,f"))
		       (match_operand:DF 1 "fpu_add_operand" "fG,H")))]
  ""
  "*
{
  REAL_VALUE_TYPE r;

  switch (which_alternative)
    {
    case 0:
      return arm_output_asm_insn (\"cmfe\\t%0, %1\", operands);
    case 1:
      REAL_VALUE_FROM_CONST_DOUBLE (r, operands[1]);
      r = REAL_VALUE_NEGATE (r);
      operands[1] = CONST_DOUBLE_FROM_REAL_VALUE (r, GET_MODE (operands[1]));
      return arm_output_asm_insn (\"cnfe\\t%0, %1\", operands);
    }
}
"
[(set_attr "conds" "set")
 (set_attr "type" "f_2_r")])

(define_insn ""
  [(set (reg:CCFPE 24)
	(compare:CCFPE (match_operand:DF 0 "s_register_operand" "f")
		       (float_extend:DF
			(match_operand:SF 1 "s_register_operand" "f"))))]
  ""
  "*
  return arm_output_asm_insn (\"cmfe\\t%0, %1\", operands);
"
[(set_attr "conds" "set")
 (set_attr "type" "f_2_r")])

(define_insn ""
  [(set (reg:CCFPE 24)
	(compare:CCFPE (match_operand:XF 0 "s_register_operand" "f,f")
		       (match_operand:XF 1 "fpu_add_operand" "fG,H")))]
  "ENABLE_XF_PATTERNS"
  "*
{
  REAL_VALUE_TYPE r;

  switch (which_alternative)
    {
    case 0:
      return arm_output_asm_insn (\"cmfe\\t%0, %1\", operands);
    case 1:
      REAL_VALUE_FROM_CONST_DOUBLE (r, operands[1]);
      r = REAL_VALUE_NEGATE (r);
      operands[1] = CONST_DOUBLE_FROM_REAL_VALUE (r, GET_MODE (operands[1]));
      return arm_output_asm_insn (\"cnfe\\t%0, %1\", operands);
    }
}
"
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
  "\\t@ deleted compare"
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
  return (arm_output_asm_insn (\"b%d1\\t%l0\", operands));
}"
[(set_attr "conds" "use")])

(define_insn ""
  [(set (pc)
	(if_then_else (match_operator 1 "comparison_operator"
					[(reg 24) (const_int 0)])
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
  return (arm_output_asm_insn (\"b%D1\\t%l0\", operands));
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
  "*
  arm_output_asm_insn (\"mov%d1\\t%0, #1\", operands);
  return arm_output_asm_insn (\"mov%D1\\t%0, #0\", operands);
"
[(set_attr "conds" "use")
 (set_attr "length" "2")])

(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(neg:SI (match_operator:SI 1 "comparison_operator"
		 [(reg 24) (const_int 0)])))]
  ""
  "*
  arm_output_asm_insn (\"mvn%d1\\t%0, #0\", operands);
  return arm_output_asm_insn (\"mov%D1\\t%0, #0\", operands);
"
[(set_attr "conds" "use")
 (set_attr "length" "2")])

(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(not:SI (match_operator:SI 1 "comparison_operator"
		 [(reg 24) (const_int 0)])))]
  ""
  "*
  arm_output_asm_insn (\"mvn%d1\\t%0, #1\", operands);
  return arm_output_asm_insn (\"mov%D1\\t%0, #0\", operands);
"
[(set_attr "conds" "use")
 (set_attr "length" "2")])


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
  return (arm_output_asm_insn (\"b\\t%l0\", operands));
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
  return (output_call (operands));
"
[(set (attr "conds")
      (if_then_else (eq_attr "cpu" "arm6")
		    (const_string "clob")
		    (const_string "nocond")))
;; length is worst case, normally it is only two
 (set_attr "length" "3")
 (set_attr "type" "call")])

(define_insn ""
  [(call (mem:SI (match_operand 0 "memory_operand" "m"))
	 (match_operand 1 "general_operand" "g"))
   (clobber (reg:SI 14))]
  ""
  "*
  return (output_call_mem (operands));
"
[(set (attr "conds")
      (if_then_else (eq_attr "cpu" "arm6")
		    (const_string "clob")
		    (const_string "nocond")))
 (set_attr "length" "3")
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
  return (output_call (&operands[1]));
"
[(set (attr "conds")
      (if_then_else (eq_attr "cpu" "arm6")
		    (const_string "clob")
		    (const_string "nocond")))
 (set_attr "length" "3")
 (set_attr "type" "call")])

(define_insn ""
  [(set (match_operand 0 "" "=rf")
	(call (mem:SI (match_operand 1 "memory_operand" "m"))
	(match_operand 2 "general_operand" "g")))
   (clobber (reg:SI 14))]
  "! CONSTANT_ADDRESS_P (XEXP (operands[1], 0))"
  "*
  return (output_call_mem (&operands[1]));
"
[(set (attr "conds")
      (if_then_else (eq_attr "cpu" "arm6")
		    (const_string "clob")
		    (const_string "nocond")))
 (set_attr "length" "3")
 (set_attr "type" "call")])

;; Allow calls to SYMBOL_REFs specially as they are not valid general addresses
;; The 'a' causes the operand to be treated as an address, i.e. no '#' output.

(define_insn ""
  [(call (mem:SI (match_operand:SI 0 "" "i"))
	 (match_operand:SI 1 "general_operand" "g"))
   (clobber (reg:SI 14))]
  "GET_CODE (operands[0]) == SYMBOL_REF"
  "*
  return (arm_output_asm_insn (\"bl\\t%a0\", operands));
"
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
  "*
  return (arm_output_asm_insn (\"bl\\t%a1\", operands));
"
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
  "*
  return arm_output_asm_insn (\"mov\\tpc, %0\\t@ table jump, label %l1\",
			      operands);
")

(define_insn ""
  [(set (pc)
	(match_operand:SI 0 "memory_operand" "m"))
   (use (label_ref (match_operand 1 "" "")))]
  ""
  "*
  return arm_output_asm_insn (\"ldr\\tpc, %0\\t@ table jump, label %l1\",
			      operands);
"
[(set_attr "type" "load")])

(define_insn "indirect_jump"
  [(set (pc)
	(match_operand:SI 0 "s_register_operand" "r"))]
  ""
  "*
  return arm_output_asm_insn (\"mov\\tpc, %0\\t@ indirect jump\", operands);
")

(define_insn ""
  [(set (pc)
	(match_operand:SI 0 "memory_operand" "m"))]
  ""
  "*
  return arm_output_asm_insn (\"ldr\\tpc, %0\\t@ indirect jump\", operands);
"
[(set_attr "type" "load")])

;; Misc insns

(define_insn "nop"
  [(const_int 0)]
  ""
  "*
  return arm_output_asm_insn (\"mov\\tr0, r0\\t@ nop\", operands);
")

;; Patterns to allow combination of arithmetic, cond code and shifts

(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=r")
        (match_operator:SI 1 "shiftable_operator"
          [(match_operator:SI 3 "shift_operator"
             [(match_operand:SI 4 "s_register_operand" "r")
              (match_operand:SI 5 "nonmemory_operand" "rI")])
           (match_operand:SI 2 "s_register_operand" "r")]))]
  ""
  "*
  return (output_arithmetic_with_shift (operands, TRUE, FALSE));
")

(define_insn ""
  [(set (reg:CC_NOOV 24)
        (compare:CC_NOOV (match_operator:SI 1 "shiftable_operator"
		          [(match_operator:SI 3 "shift_operator"
		            [(match_operand:SI 4 "s_register_operand" "r")
		             (match_operand:SI 5 "nonmemory_operand" "rI")])
		           (match_operand:SI 2 "s_register_operand" "r")])
			 (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(match_op_dup 1 [(match_op_dup 3 [(match_dup 4) (match_dup 5)])
			 (match_dup 2)]))]
  ""
  "*
  return (output_arithmetic_with_shift (operands, TRUE, TRUE));
"
[(set_attr "conds" "set")])

(define_insn ""
  [(set (reg:CC_NOOV 24)
        (compare:CC_NOOV (match_operator:SI 1 "shiftable_operator"
		          [(match_operator:SI 3 "shift_operator"
		            [(match_operand:SI 4 "s_register_operand" "r")
		             (match_operand:SI 5 "nonmemory_operand" "rI")])
		           (match_operand:SI 2 "s_register_operand" "r")])
			 (const_int 0)))
   (clobber (match_scratch:SI 0 "=r"))]
  ""
  "*
  return (output_arithmetic_with_shift (operands, TRUE, TRUE));
"
[(set_attr "conds" "set")])

(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(minus:SI (match_operand:SI 1 "s_register_operand" "r")
		  (match_operator:SI 2 "shift_operator"
		   [(match_operand:SI 3 "s_register_operand" "r")
		    (match_operand:SI 4 "nonmemory_operand" "rn")])))]
  ""
  "*
{
  rtx ops[6];

  ops[0] = operands[0];
  ops[1] = gen_rtx (MINUS, SImode, operands[1], operands[2]);
  ops[2] = operands[1];
  ops[3] = operands[2];
  ops[4] = operands[3];
  ops[5] = operands[4];
  return output_arithmetic_with_shift (ops, FALSE, FALSE);
}
")

(define_insn ""
  [(set (reg:CC_NOOV 24)
	(compare:CC_NOOV (minus:SI (match_operand:SI 1 "s_register_operand" "r")
				   (match_operator:SI 2 "shift_operator"
				    [(match_operand:SI 3 "s_register_operand" "r")
				     (match_operand:SI 4 "nonmemory_operand" "rn")]))
			 (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(minus:SI (match_dup 1) (match_op_dup 2 [(match_dup 3)
						 (match_dup 4)])))]
  ""
  "*
{
  rtx ops[6];

  ops[0] = operands[0];
  ops[1] = gen_rtx (MINUS, SImode, operands[1], operands[2]);
  ops[2] = operands[1];
  ops[3] = operands[2];
  ops[4] = operands[3];
  ops[5] = operands[4];
  return output_arithmetic_with_shift (ops, FALSE, TRUE);
}
"
[(set_attr "conds" "set")])

(define_insn ""
  [(set (reg:CC_NOOV 24)
	(compare:CC_NOOV (minus:SI (match_operand:SI 1 "s_register_operand" "r")
				   (match_operator:SI 2 "shift_operator"
				    [(match_operand:SI 3 "s_register_operand" "r")
				     (match_operand:SI 4 "nonmemory_operand" "rn")]))
			 (const_int 0)))
   (clobber (match_scratch:SI 0 "=r"))]
  ""
  "*
{
  rtx ops[6];

  ops[0] = operands[0];
  ops[1] = gen_rtx (MINUS, SImode, operands[1], operands[2]);
  ops[2] = operands[1];
  ops[3] = operands[2];
  ops[4] = operands[3];
  ops[5] = operands[4];
  return output_arithmetic_with_shift (ops, FALSE, TRUE);
}
"
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
			    (match_operand:SI 4 "" "rn")])
			  (match_operand:SI 2 "" "r"))
		 (match_operand:SI 1 "const_int_operand" "n")))]
  "reload_in_progress"
  "*
{
  char instr[100];
  sprintf (instr, \"add\\t%%0, %%2, %%3, %s %%4\", 
	   shift_instr (GET_CODE (operands[5]), &operands[4]));
  arm_output_asm_insn (instr, operands);
  operands[2] = operands[1];
  operands[1] = operands[0];
  return output_add_immediate (operands);
}"
; we have no idea how long the add_immediate is, it could be up to 4.
[(set_attr "length" "5")])

(define_insn ""
  [(set (reg:CC_NOOV 24)
	(compare:CC_NOOV (plus:SI
			  (plus:SI 
			   (match_operator:SI 5 "shift_operator"
			    [(match_operand:SI 3 "" "r")
			     (match_operand:SI 4 "" "rn")])
			   (match_operand:SI 1 "" "r"))
			  (match_operand:SI 2 "const_int_operand" "n"))
			 (const_int 0)))
   (set (match_operand:SI 0 "" "=&r")
	(plus:SI (plus:SI (match_op_dup 5 [(match_dup 3) (match_dup 4)])
			  (match_dup 1))
		 (match_dup 2)))]
  "reload_in_progress"
  "*
{
  char instr[100];
  sprintf (instr, \"adds\\t%%0, %%0, %%3, %s %%4\",
	   shift_instr (GET_CODE (operands[5]), &operands[4]));
  output_add_immediate (operands);
  return arm_output_asm_insn (instr, operands);
}"
[(set_attr "conds" "set")
 (set_attr "length" "5")])

(define_insn ""
  [(set (reg:CC_NOOV 24)
	(compare:CC_NOOV (plus:SI
			  (plus:SI 
			   (match_operator:SI 5 "shift_operator"
			    [(match_operand:SI 3 "" "r")
			     (match_operand:SI 4 "" "rn")])
			   (match_operand:SI 1 "" "r"))
			  (match_operand:SI 2 "const_int_operand" "n"))
			 (const_int 0)))
   (clobber (match_scratch:SI 0 "=&r"))]
  "reload_in_progress"
  "*
{
  char instr[100];
  sprintf (instr, \"adds\\t%%0, %%0, %%3, %s %%4\",
	   shift_instr (GET_CODE (operands[5]), &operands[4]));
  output_add_immediate (operands);
  return arm_output_asm_insn (instr, operands);
}"
[(set_attr "conds" "set")
 (set_attr "length" "5")])

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
  arm_output_asm_insn (\"mla\\t%0, %2, %1, %3\", operands);
  operands[2] = operands[4];
  operands[1] = operands[0];
  return output_add_immediate (operands);
"
[(set_attr "length" "5")])

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
  return arm_output_asm_insn (\"mlas\\t%0, %3, %4, %0\", operands);
"
[(set_attr "length" "5")
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
  return arm_output_asm_insn (\"mlas\\t%0, %3, %4, %0\", operands);
"
[(set_attr "length" "5")
 (set_attr "conds" "set")])




(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(and:SI (match_operator 1 "comparison_operator"
		 [(reg 24) (const_int 0)])
		(match_operand:SI 2 "s_register_operand" "r")))]
  ""
  "*
  arm_output_asm_insn (\"mov%D1\\t%0, #0\", operands);
  return arm_output_asm_insn (\"and%d1\\t%0, %2, #1\", operands);
"
[(set_attr "conds" "use")
 (set_attr "length" "2")])

(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(ior:SI (match_operator 2 "comparison_operator"
		 [(reg 24) (const_int 0)])
		(match_operand:SI 1 "s_register_operand" "0,?r")))]
  ""
  "*
  if (which_alternative != 0)
    arm_output_asm_insn (\"mov%D2\\t%0, %1\", operands);
  return arm_output_asm_insn (\"orr%d2\\t%0, %1, #1\", operands);
"
[(set_attr "conds" "use")
 (set_attr "length" "1,2")])

(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(match_operator 1 "comparison_operator"
	 [(match_operand:SI 2 "s_register_operand" "r")
	  (match_operand:SI 3 "arm_add_operand" "rL")]))
   (clobber (reg 24))]
  ""
  "*
  if (GET_CODE (operands[1]) == LT && operands[3] == const0_rtx)
    return arm_output_asm_insn (\"mov\\t%0, %2, lsr #31\", operands);
  if (GET_CODE (operands[1]) == GE && operands[3] == const0_rtx)
    {
      arm_output_asm_insn (\"mvn\\t%0, %2\", operands);
      return arm_output_asm_insn (\"mov\\t%0, %0, lsr #31\", operands);
    }
  if (GET_CODE (operands[1]) == NE)
    {
      if (GET_CODE (operands[3]) == CONST_INT
	  && !const_ok_for_arm (INTVAL (operands[3])))
	arm_output_asm_insn (\"adds\\t%0, %2, #%n3\", operands);
      else
        arm_output_asm_insn (\"subs\\t%0, %2, %3\", operands);
      return arm_output_asm_insn (\"movne\\t%0, #1\", operands);
    }
  if (GET_CODE (operands[3]) == CONST_INT
      && !const_ok_for_arm (INTVAL (operands[3])))
    arm_output_asm_insn (\"cmn\\t%2, #%n3\", operands);
  else
    arm_output_asm_insn (\"cmp\\t%2, %3\", operands);
  arm_output_asm_insn (\"mov%D1\\t%0, #0\", operands);
  return arm_output_asm_insn (\"mov%d1\\t%0, #1\", operands);
"
[(set_attr "conds" "clob")
 (set_attr "length" "3")])

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

  arm_output_asm_insn (dominant ? \"cmp\\t%5, %6\" : \"cmp\\t%2, %3\",
		       operands);
  arm_output_asm_insn (\"mov\\t%0, #0\", operands);
  if (GET_CODE (operands[1]) == GET_CODE (operands[4])
      || comparison_dominates_p (GET_CODE (operands[1]),
				 GET_CODE (operands[4]))
      || dominant)
    {
      arm_output_asm_insn (dominant ? \"cmp%D4\\t%2, %3\" : \"cmp%D1\\t%5,%6\",
			   operands);
    }
  else
    {
      arm_output_asm_insn (\"mov%d1\\t%0, #1\", operands);
      arm_output_asm_insn (\"cmp\\t%5, %6\", operands);
    }
  return arm_output_asm_insn (dominant ? \"mov%d1\\t%0, #1\"
			      : \"mov%d4\\t%0, #1\", operands);
}
"
[(set_attr "conds" "clob")
; worst case length
 (set_attr "length" "5")])

(define_split
  [(set (pc)
	(if_then_else (match_operator 5 "equality_operator"
		       [(ior:SI (match_operator 6 "comparison_operator"
				 [(match_operand:SI 0 "s_register_operand" "r")
				  (match_operand:SI 1 "arm_add_operand" "rL")])
				(match_operator 7 "comparison_operator"
				 [(match_operand:SI 2 "s_register_operand" "r")
				  (match_operand:SI 3 "arm_add_operand" "rL")]))
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
	(if_then_else (ne
		       (ior:SI (match_operator 5 "comparison_operator"
				[(match_operand:SI 0 "s_register_operand" "r")
				 (match_operand:SI 1 "arm_add_operand" "rL")])
			       (match_operator 6 "comparison_operator"
				[(match_operand:SI 2 "s_register_operand" "r")
				 (match_operand:SI 3 "arm_rhs_operand" "rL")]))
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

  if (GET_CODE (operands[1]) == CONST_INT
      && !const_ok_for_arm (INTVAL (operands[1])))
    arm_output_asm_insn (\"cmn\\t%0, #%n1\", operands);
  else
    arm_output_asm_insn (\"cmp\\t%0, %1\", operands);
  arm_output_asm_insn (\"b%d5\\t%l4\", operands);
  if (GET_CODE (operands[3]) == CONST_INT
      && !const_ok_for_arm (INTVAL (operands[3])))
    arm_output_asm_insn (\"cmn\\t%2, #%n3\", operands);
  else
    arm_output_asm_insn (\"cmp\\t%2, %3\", operands);
  if (arm_ccfsm_state == 1 || arm_ccfsm_state == 2)
  {
    arm_ccfsm_state += 2;
    return \"\";
  }
  return arm_output_asm_insn (\"b%d6\\t%l4\", operands);
}"
[(set_attr "conds" "jump_clob")
 (set_attr "length" "4")])

(define_insn ""
  [(set (reg:CC 24)
	(compare:CC (ior:CC (match_operator 4 "comparison_operator"
			     [(match_operand:SI 0 "s_register_operand" "r")
			      (match_operand:SI 1 "arm_add_operand" "rL")])
			    (match_operator 5 "comparison_operator"
			     [(match_operand:SI 2 "s_register_operand" "r")
			      (match_operand:SI 3 "arm_add_operand" "rL")]))
		    (const_int 0)))]
  "(GET_CODE (operands[4]) == GET_CODE (operands[5])
    || comparison_dominates_p (GET_CODE (operands[4]), GET_CODE (operands[5]))
    || comparison_dominates_p (GET_CODE (operands[5]), GET_CODE (operands[4])))"
  "*
  if (comparison_dominates_p (GET_CODE (operands[5]), GET_CODE (operands[4])))
    {
      if (GET_CODE (operands[3]) == CONST_INT
	  && !const_ok_for_arm (INTVAL (operands[3])))
	arm_output_asm_insn (\"cmn\\t%2, #%n3\", operands);
      else
	arm_output_asm_insn (\"cmp\\t%2, %3\", operands);
      if (GET_CODE (operands[1]) == CONST_INT
	  && !const_ok_for_arm (INTVAL (operands[1])))
	return arm_output_asm_insn (\"cmn%D5\\t%0, #%n1\", operands);
      return arm_output_asm_insn (\"cmp%D5\\t%0, %1\", operands);
    }
  if (GET_CODE (operands[1]) == CONST_INT
      && !const_ok_for_arm (INTVAL (operands[1])))
    arm_output_asm_insn (\"cmn\\t%0, #%n1\", operands);
  else
    arm_output_asm_insn (\"cmp\\t%0, %1\", operands);
  if (GET_CODE (operands[3]) == CONST_INT
      && !const_ok_for_arm (INTVAL (operands[3])))
    return arm_output_asm_insn (\"cmn%D4\\t%2, #%n3\", operands);
  return arm_output_asm_insn (\"cmp%D4\\t%2, %3\", operands);
"
[(set_attr "conds" "set")
 (set_attr "length" "2")])

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
      if (which_alternative != 0)
	arm_output_asm_insn (\"mov%d4\\t%0, %1\", operands);
      if (which_alternative != 1)
	arm_output_asm_insn (\"mov%D4\\t%0, %2\", operands);
      return \"\";
    }
  if (which_alternative != 0)
    arm_output_asm_insn (\"mov%D4\\t%0, %1\", operands);
  if (which_alternative != 1)
    arm_output_asm_insn (\"mov%d4\\t%0, %2\", operands);
  return \"\";
"
[(set_attr "conds" "use")
 (set_attr "length" "1,1,2")])

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
{
  char *instr = arithmetic_instr (operands[5], TRUE);
  char pattern[100];

  if (GET_CODE (operands[4]) == LT && operands[3] == const0_rtx)
    {
      sprintf (pattern, \"%s\\t%%0, %%1, %%2, lsr #31\", instr);
      return arm_output_asm_insn (pattern, operands);
    }
  arm_output_asm_insn (\"cmp\\t%2, %3\", operands);
  if (GET_CODE (operands[5]) == AND)
    arm_output_asm_insn (\"mov%D4\\t%0, #0\", operands);
  else if (which_alternative != 0)
    arm_output_asm_insn (\"mov%D4\\t%0, %1\", operands);
  sprintf (pattern, \"%s%%d4\\t%%0, %%1, #1\", instr);
  return arm_output_asm_insn (pattern, operands);
}
"
[(set_attr "conds" "clob")
 (set_attr "length" "3")])

(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
        (minus:SI (match_operand:SI 1 "s_register_operand" "0,?r")
		  (match_operator:SI 4 "comparison_operator"
                   [(match_operand:SI 2 "s_register_operand" "r,r")
		    (match_operand:SI 3 "arm_rhs_operand" "rI,rI")])))
   (clobber (reg 24))]
  ""
  "*
  arm_output_asm_insn (\"cmp\\t%2, %3\", operands);
  if (which_alternative != 0)
    arm_output_asm_insn (\"mov%D4\\t%0, %1\", operands);
  return arm_output_asm_insn (\"sub%d4\\t%0, %1, #1\", operands);
"
[(set_attr "conds" "clob")
 (set_attr "length" "2,3")])

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
  arm_output_asm_insn (dominant == 2 ? \"cmp\\t%5, %6\" : \"cmp\\t%2, %3\",
		       operands);
  arm_output_asm_insn (\"mov\\t%0, #1\", operands);
  if (GET_CODE (operands[1]) == GET_CODE (operands[4]) || dominant)
    {
      arm_output_asm_insn (dominant == 2 ? \"cmp%d4\\t%2, %3\"
			   : \"cmp%d1\\t%5, %6\", operands);
    }
  else
    {
      arm_output_asm_insn (\"mov%D1\\t%0, #0\", operands);
      arm_output_asm_insn (\"cmp\\t%5, %6\", operands);
    }
  return arm_output_asm_insn (dominant == 2 ? \"mov%D1\\t%0, #0\" 
			      : \"mov%D4\\t%0, #0\", operands);
}
"
[(set_attr "conds" "clob")
 (set_attr "length" "5")])

(define_split
  [(set (pc)
	(if_then_else (match_operator 1 "equality_operator"
		       [(and:SI (match_operator 2 "comparison_operator"
				 [(match_operand:SI 3 "s_register_operand" "r")
				  (match_operand:SI 4 "arm_add_operand" "rL")])
				(match_operator 0 "comparison_operator"
				 [(match_operand:SI 5 "s_register_operand" "r")
				  (match_operand:SI 6 "arm_add_operand" "rL")]))
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
	(if_then_else (eq
		       (and:SI (match_operator 1 "comparison_operator"
				[(match_operand:SI 2 "s_register_operand" "r")
				 (match_operand:SI 3 "arm_add_operand" "rL")])
			       (match_operator 4 "comparison_operator"
				[(match_operand:SI 5 "s_register_operand" "r")
				 (match_operand:SI 6 "arm_rhs_operand" "rL")]))
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

  if (GET_CODE (operands[3]) == CONST_INT
      && !const_ok_for_arm (INTVAL (operands[3])))
    arm_output_asm_insn (\"cmn\\t%2, #%n3\", operands);
  else
    arm_output_asm_insn (\"cmp\\t%2, %3\", operands);
  arm_output_asm_insn (\"b%D1\\t%l0\", operands);
  if (GET_CODE (operands[6]) == CONST_INT
      && !const_ok_for_arm (INTVAL (operands[6])))
    arm_output_asm_insn (\"cmn\\t%5, #%n6\", operands);
  else
    arm_output_asm_insn (\"cmp\\t%5, %6\", operands);
  if (arm_ccfsm_state == 1 || arm_ccfsm_state == 2)
  {
    arm_ccfsm_state += 2;
    return \"\";
  }
  return arm_output_asm_insn (\"b%D4\\t%l0\", operands);
}"
[(set_attr "conds" "jump_clob")
 (set_attr "length" "4")])

(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(neg:SI (match_operator 3 "comparison_operator"
		 [(match_operand:SI 1 "s_register_operand" "r")
		  (match_operand:SI 2 "arm_rhs_operand" "rI")])))
   (clobber (reg 24))]
  ""
  "*
  if (GET_CODE (operands[3]) == LT && operands[3] == const0_rtx)
    return arm_output_asm_insn (\"mov\\t%0, %1, asr #31\", operands);
  if (GET_CODE (operands[3]) == NE)
    {
      arm_output_asm_insn (\"subs\\t%0, %1, %2\", operands);
      return arm_output_asm_insn (\"mvnne\\t%0, #0\", operands);
    }
  if (GET_CODE (operands[3]) == GT)
    {
      arm_output_asm_insn (\"subs\\t%0, %1, %2\", operands);
      return arm_output_asm_insn (\"mvnne\\t%0, %0, asr #31\", operands);
    }
  arm_output_asm_insn (\"cmp\\t%1, %2\", operands);
  arm_output_asm_insn (\"mov%D3\\t%0, #0\", operands);
  return arm_output_asm_insn (\"mvn%d3\\t%0, #0\", operands);
"
[(set_attr "conds" "clob")
 (set_attr "length" "3")])

(define_insn "movcond"
  [(set (match_operand:SI 0 "s_register_operand" "=r,r,r")
	(if_then_else:SI (match_operator 5 "comparison_operator"
			  [(match_operand:SI 3 "s_register_operand" "r,r,r")
			   (match_operand:SI 4 "arm_add_operand" "rL,rL,rL")])
			 (match_operand:SI 1 "arm_rhs_operand" "0,rI,?rI")
			 (match_operand:SI 2 "arm_rhs_operand" "rI,0,rI")))
   (clobber (reg 24))]
  ""
  "*
  if (GET_CODE (operands[5]) == LT
      && (operands[4] == const0_rtx))
    {
      if (which_alternative != 1 && GET_CODE (operands[4]) == REG)
	{
	  arm_output_asm_insn (\"ands\\t%0, %1, %3, asr #32\", operands);
	  if (operands[2] == const0_rtx)
	    return \"\";
	  return arm_output_asm_insn (\"movcc\\t%0, %2\", operands);
	}
      else if (which_alternative != 0 && GET_CODE (operands[2]) == REG)
	{
	  arm_output_asm_insn (\"bics\\t%0, %2, %3, asr #32\", operands);
	  if (operands[1] == const0_rtx)
	    return \"\";
	  return arm_output_asm_insn (\"movcs\\t%0, %1\", operands);
	}
      /* The only case that falls through to here is when both ops 1 & 2
	 are constants */
    }
  if (GET_CODE (operands[5]) == GE
      && (operands[4] == const0_rtx))
    {
      if (which_alternative != 1 && GET_CODE (operands[1]) == REG)
	{
	  arm_output_asm_insn (\"bics\\t%0, %1, %3, asr #32\", operands);
	  if (operands[2] == const0_rtx)
	    return \"\";
	  return arm_output_asm_insn (\"movcs\\t%0, %2\", operands);
	}
      else if (which_alternative != 0 && GET_CODE (operands[2]) == REG)
	{
	  arm_output_asm_insn (\"ands\\t%0, %2, %3, asr #32\", operands);
	  if (operands[1] == const0_rtx)
	    return \"\";
	  return arm_output_asm_insn (\"movcc\\t%0, %1\", operands);
	}
      /* The only case that falls through to here is when both ops 1 & 2
	 are constants */
    }
  if (GET_CODE (operands[4]) == CONST_INT
      && !const_ok_for_arm (INTVAL (operands[4])))
    arm_output_asm_insn (\"cmn\\t%3, #%n4\", operands);
  else
    arm_output_asm_insn (\"cmp\\t%3, %4\", operands);
  if (which_alternative != 0)
    arm_output_asm_insn (\"mov%d5\\t%0, %1\", operands);
  if (which_alternative != 1)
    arm_output_asm_insn (\"mov%D5\\t%0, %2\", operands);
  return \"\";
"
[(set_attr "conds" "clob")
 (set_attr "length" "2,2,3")])

(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(if_then_else:SI (match_operator 9 "comparison_operator"
			  [(match_operand:SI 5 "s_register_operand" "r")
			   (match_operand:SI 6 "arm_add_operand" "rL")])
			 (match_operator:SI 8 "shiftable_operator"
			  [(match_operand:SI 1 "s_register_operand" "r")
			   (match_operand:SI 2 "arm_rhs_operand" "rI")])
			 (match_operator:SI 7 "shiftable_operator"
			  [(match_operand:SI 3 "s_register_operand" "r")
			   (match_operand:SI 4 "arm_rhs_operand" "rI")])))
   (clobber (reg 24))]
  ""
  "*
{
  char pattern[100];

  if (GET_CODE (operands[6]) == CONST_INT
      && !const_ok_for_arm (INTVAL (operands[6])))
    arm_output_asm_insn (\"cmn\\t%5, #%n6\", operands);
  else
    arm_output_asm_insn (\"cmp\\t%5, %6\", operands);
  sprintf (pattern, \"%s%%d9\\t%%0, %%1, %%2\", arithmetic_instr (operands[8],
								  FALSE));
  arm_output_asm_insn (pattern, operands);
  sprintf (pattern, \"%s%%D9\\t%%0, %%3, %%4\", arithmetic_instr (operands[7],
								  FALSE));
  return arm_output_asm_insn (pattern, operands);
}
"
[(set_attr "conds" "clob")
 (set_attr "length" "3")])

(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(if_then_else:SI (match_operator 6 "comparison_operator"
			  [(match_operand:SI 2 "s_register_operand" "r,r")
			   (match_operand:SI 3 "arm_add_operand" "rL,rL")])
			 (match_operator:SI 7 "shiftable_operator"
			  [(match_operand:SI 4 "s_register_operand" "r,r")
			   (match_operand:SI 5 "arm_rhs_operand" "rI,rI")])
			 (match_operand:SI 1 "arm_rhsm_operand" "0,?rIm")))
   (clobber (reg 24))]
  ""
  "*
{
  char pattern[100];

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
	{
	  arm_output_asm_insn (\"and\\t%0, %5, %2, asr #31\", operands);
	  sprintf (pattern, \"%s\\t%%0, %%4, %%0\",
		   arithmetic_instr (operands[7], FALSE));
	  return arm_output_asm_insn (pattern, operands);
        }
      else if (GET_CODE (operands[6]) == GE)
	{
	  arm_output_asm_insn (\"bic\\t%0, %5, %2, asr #31\", operands);
	  sprintf (pattern, \"%s\\t%%0, %%4, %%0\",
		   arithmetic_instr (operands[7], FALSE));
	  return arm_output_asm_insn (pattern, operands);
        }
    }
  if (GET_CODE (operands[3]) == CONST_INT
      && !const_ok_for_arm (INTVAL (operands[3])))
    arm_output_asm_insn (\"cmn\\t%2, #%n3\", operands);
  else
    arm_output_asm_insn (\"cmp\\t%2, %3\", operands);
  sprintf (pattern, \"%s%%d6\\t%%0, %%4, %%5\", arithmetic_instr (operands[7],
								  FALSE));
  arm_output_asm_insn (pattern, operands);
  if (which_alternative != 0)
    {
      if (GET_CODE (operands[1]) == MEM)
	arm_output_asm_insn (\"ldr%D6\\t%0, %1\", operands);
      else
	arm_output_asm_insn (\"mov%D6\\t%0, %1\", operands);
    }
  return \"\";
}
"
[(set_attr "conds" "clob")
 (set_attr "length" "2,3")])

(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(if_then_else:SI (match_operator 6 "comparison_operator"
			  [(match_operand:SI 4 "s_register_operand" "r,r")
			   (match_operand:SI 5 "arm_add_operand" "rL,rL")])
			 (match_operand:SI 1 "arm_rhsm_operand" "0,?rIm")
			 (match_operator:SI 7 "shiftable_operator"
			  [(match_operand:SI 2 "s_register_operand" "r,r")
			   (match_operand:SI 3 "arm_rhs_operand" "rI,rI")])))
   (clobber (reg 24))]
  ""
  "*
{
  char pattern[100];

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
	{
	  arm_output_asm_insn (\"and\\t%0, %3, %4, asr #31\", operands);
	  sprintf (pattern, \"%s\\t%%0, %%2, %%0\",
		   arithmetic_instr (operands[7], FALSE));
	  return arm_output_asm_insn (pattern, operands);
        }
      else if (GET_CODE (operands[6]) == LT)
	{
	  arm_output_asm_insn (\"bic\\t%0, %3, %4, asr #31\", operands);
	  sprintf (pattern, \"%s\\t%%0, %%2, %%0\",
		   arithmetic_instr (operands[7], FALSE));
	  return arm_output_asm_insn (pattern, operands);
        }
    }
  if (GET_CODE (operands[5]) == CONST_INT
      && !const_ok_for_arm (INTVAL (operands[5])))
    arm_output_asm_insn (\"cmn\\t%4, #%n5\", operands);
  else
    arm_output_asm_insn (\"cmp\\t%4, %5\", operands);
  if (which_alternative != 0)
    {
      if (GET_CODE (operands[1]) == MEM)
	arm_output_asm_insn (\"ldr%d6\\t%0, %1\", operands);
      else
	arm_output_asm_insn (\"mov%d6\\t%0, %1\", operands);
    }
  sprintf (pattern, \"%s%%D6\\t%%0, %%2, %%3\", arithmetic_instr (operands[7],
								  FALSE));
  return arm_output_asm_insn (pattern, operands);
}
"
[(set_attr "conds" "clob")
 (set_attr "length" "2,3")])

(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(if_then_else:SI (match_operator 6 "comparison_operator"
			  [(match_operand:SI 4 "s_register_operand" "r,r")
			   (match_operand:SI 5 "arm_add_operand" "rL,rL")])
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
    arm_output_asm_insn (\"cmn\\t%4, #%n5\", operands);
  else
    arm_output_asm_insn (\"cmp\\t%4, %5\", operands);
  if (GET_CODE (operands[3]) == CONST_INT
      && !const_ok_for_arm (INTVAL (operands[3])))
    arm_output_asm_insn (\"sub%d6\\t%0, %2, #%n3\", operands);
  else
    arm_output_asm_insn (\"add%d6\\t%0, %2, %3\", operands);
  if (which_alternative != 0)
    {
      if (GET_CODE (operands[1]) == MEM)
	arm_output_asm_insn (\"ldr%D6\\t%0, %1\", operands);
      else
	arm_output_asm_insn (\"mov%D6\\t%0, %1\", operands);
    }
  return \"\";
}
"
[(set_attr "conds" "clob")
 (set_attr "length" "2,3")])

(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(if_then_else:SI (match_operator 6 "comparison_operator"
			  [(match_operand:SI 4 "s_register_operand" "r,r")
			   (match_operand:SI 5 "arm_add_operand" "rL,rL")])
			 (match_operand:SI 1 "arm_rhsm_operand" "0,?rIm")
			 (plus:SI
			  (match_operand:SI 2 "s_register_operand" "r,r")
			  (match_operand:SI 3 "arm_add_operand" "rL,rL"))))
   (clobber (reg 24))]
  ""
  "*
{
  if (GET_CODE (operands[5]) == CONST_INT
      && !const_ok_for_arm (INTVAL (operands[5])))
    arm_output_asm_insn (\"cmn\\t%4, #%n5\", operands);
  else
    arm_output_asm_insn (\"cmp\\t%4, %5\", operands);
  if (GET_CODE (operands[3]) == CONST_INT
      && !const_ok_for_arm (INTVAL (operands[3])))
    arm_output_asm_insn (\"sub%D6\\t%0, %2, #%n3\", operands);
  else
    arm_output_asm_insn (\"add%D6\\t%0, %2, %3\", operands);
  if (which_alternative != 0)
    {
      if (GET_CODE (operands[6]) == MEM)
	arm_output_asm_insn (\"ldr%d6\\t%0, %1\", operands);
      else
	arm_output_asm_insn (\"mov%d6\\t%0, %1\", operands);
    }
  return \"\";
}
"
[(set_attr "conds" "clob")
 (set_attr "length" "2,3")])

(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(if_then_else:SI (match_operator 5 "comparison_operator"
			  [(match_operand:SI 3 "s_register_operand" "r,r")
			   (match_operand:SI 4 "arm_add_operand" "rL,rL")])
			 (match_operand:SI 1 "arm_rhs_operand" "0,?rI")
			 (not:SI
			  (match_operand:SI 2 "s_register_operand" "r,r"))))
   (clobber (reg 24))]
  ""
  "#"
[(set_attr "conds" "clob")
 (set_attr "length" "2,3")])

;;  if (GET_CODE (operands[3]) == CONST_INT
;;      && !const_ok_for_arm (INTVAL (operands[3])))
;;    arm_output_asm_insn (\"cmn\\t%2, #%n3\", operands);
;;  else
;;    arm_output_asm_insn (\"cmp\\t%2, %3\", operands);
;;  if (which_alternative != 0)
;;    arm_output_asm_insn (\"mov%d1\\t%0, %4\", operands);
;;  return arm_output_asm_insn (\"mvn%D1\\t%0, %5\", operands);

(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(if_then_else:SI (match_operator 5 "comparison_operator"
			  [(match_operand:SI 3 "s_register_operand" "r,r")
			   (match_operand:SI 4 "arm_add_operand" "rL,rL")])
			 (not:SI
			  (match_operand:SI 2 "s_register_operand" "r,r"))
			 (match_operand:SI 1 "arm_rhs_operand" "0,?rI")))
   (clobber (reg 24))]
  ""
  "*
{
  char pattern[100];

  if (GET_CODE (operands[30]) == CONST_INT
      && !const_ok_for_arm (INTVAL (operands[4])))
    arm_output_asm_insn (\"cmn\\t%3, #%n4\", operands);
  else
    arm_output_asm_insn (\"cmp\\t%3, %4\", operands);
  if (which_alternative != 0)
    arm_output_asm_insn (\"mov%D5\\t%0, %1\", operands);
  return arm_output_asm_insn (\"mvn%d5\\t%0, %2\", operands);

}
"
[(set_attr "conds" "clob")
 (set_attr "length" "2,3")])

(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(if_then_else:SI (match_operator 6 "comparison_operator"
			  [(match_operand:SI 4 "s_register_operand" "r,r")
			   (match_operand:SI 5 "arm_add_operand" "rL,rL")])
			 (match_operator:SI 7 "shift_operator"
			  [(match_operand:SI 2 "s_register_operand" "r,r")
			   (match_operand:SI 3 "arm_rhs_operand" "rn,rn")])
			 (match_operand:SI 1 "arm_rhs_operand" "0,?rI")))
   (clobber (reg 24))]
  ""
  "*
{
  char pattern[100];

  if (GET_CODE (operands[5]) == CONST_INT
      && !const_ok_for_arm (INTVAL (operands[5])))
    arm_output_asm_insn (\"cmn\\t%4, #%n5\", operands);
  else
    arm_output_asm_insn (\"cmp\\t%4, %5\", operands);
  if (which_alternative != 0)
    arm_output_asm_insn (\"mov%D6\\t%0, %1\", operands);
  sprintf (pattern, \"mov%%d6\\t%%0, %%2, %s %%3\", 
	   shift_instr (GET_CODE (operands[7]), &operands[3]));
  return arm_output_asm_insn (pattern, operands);
}
"
[(set_attr "conds" "clob")
 (set_attr "length" "2,3")])

(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(if_then_else:SI (match_operator 6 "comparison_operator"
			  [(match_operand:SI 4 "s_register_operand" "r,r")
			   (match_operand:SI 5 "arm_add_operand" "rL,rL")])
			 (match_operand:SI 1 "arm_rhs_operand" "0,?rI")
			 (match_operator:SI 7 "shift_operator"
			  [(match_operand:SI 2 "s_register_operand" "r,r")
			   (match_operand:SI 3 "arm_rhs_operand" "rn,rn")])))
   (clobber (reg 24))]
  ""
  "*
{
  char pattern[100];

  if (GET_CODE (operands[5]) == CONST_INT
      && !const_ok_for_arm (INTVAL (operands[5])))
    arm_output_asm_insn (\"cmn\\t%4, #%n5\", operands);
  else
    arm_output_asm_insn (\"cmp\\t%4, %5\", operands);
  if (which_alternative != 0)
    arm_output_asm_insn (\"mov%d6\\t%0, %1\", operands);
  sprintf (pattern, \"mov%%D6\\t%%0, %%2, %s %%3\", 
	   shift_instr (GET_CODE (operands[7]), &operands[3]));
  return arm_output_asm_insn (pattern, operands);
}
"
[(set_attr "conds" "clob")
 (set_attr "length" "2,3")])

(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(if_then_else:SI (match_operator 7 "comparison_operator"
			  [(match_operand:SI 5 "s_register_operand" "r")
			   (match_operand:SI 6 "arm_add_operand" "rL")])
			 (match_operator:SI 8 "shift_operator"
			  [(match_operand:SI 1 "s_register_operand" "r")
			   (match_operand:SI 2 "arm_rhs_operand" "rn")])
			 (match_operator:SI 9 "shift_operator"
			  [(match_operand:SI 3 "s_register_operand" "r")
			   (match_operand:SI 4 "arm_rhs_operand" "rI")])))
   (clobber (reg 24))]
  ""
  "*
{
  char pattern[100];

  if (GET_CODE (operands[6]) == CONST_INT
      && !const_ok_for_arm (INTVAL (operands[6])))
    arm_output_asm_insn (\"cmn\\t%5, #%n6\", operands);
  else
    arm_output_asm_insn (\"cmp\\t%5, %6\", operands);
  sprintf (pattern, \"mov%%d7\\t%%0, %%1, %s %%2\", 
	   shift_instr (GET_CODE (operands[8]), &operands[2]));
  arm_output_asm_insn (pattern, operands);
  sprintf (pattern, \"mov%%D7\\t%%0, %%3, %s %%4\", 
	   shift_instr (GET_CODE (operands[9]), &operands[4]));
  return arm_output_asm_insn (pattern, operands);
}
"
[(set_attr "conds" "clob")
 (set_attr "length" "3")])

(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(if_then_else:SI (match_operator 6 "comparison_operator"
			  [(match_operand:SI 4 "s_register_operand" "r")
			   (match_operand:SI 5 "arm_add_operand" "rL")])
			 (not:SI (match_operand:SI 1 "s_register_operand" "r"))
			 (match_operator:SI 7 "shiftable_operator"
			  [(match_operand:SI 2 "s_register_operand" "r")
			   (match_operand:SI 3 "arm_rhs_operand" "rI")])))
   (clobber (reg 24))]
  ""
  "*
{
  char pattern[100];

  if (GET_CODE (operands[5]) == CONST_INT
      && !const_ok_for_arm (INTVAL (operands[5])))
    arm_output_asm_insn (\"cmn\\t%4, #%n5\", operands);
  else
    arm_output_asm_insn (\"cmp\\t%4, %5\", operands);
  arm_output_asm_insn (\"mvn%d6\\t%0, %1\", operands);
  sprintf (pattern, \"%s%%D6\\t%%0, %%2, %%3\", arithmetic_instr (operands[7],
								  FALSE));
  return arm_output_asm_insn (pattern, operands);
}
"
[(set_attr "conds" "clob")
 (set_attr "length" "3")])

(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(if_then_else:SI (match_operator 6 "comparison_operator"
			  [(match_operand:SI 4 "s_register_operand" "r")
			   (match_operand:SI 5 "arm_add_operand" "rL")])
			 (match_operator:SI 7 "shiftable_operator"
			  [(match_operand:SI 2 "s_register_operand" "r")
			   (match_operand:SI 3 "arm_rhs_operand" "rI")])
			 (not:SI (match_operand:SI 1 "s_register_operand" "r"))))
   (clobber (reg 24))]
  ""
  "*
{
  char pattern[100];

  if (GET_CODE (operands[5]) == CONST_INT
      && !const_ok_for_arm (INTVAL (operands[5])))
    arm_output_asm_insn (\"cmn\\t%4, #%n5\", operands);
  else
    arm_output_asm_insn (\"cmp\\t%4, %5\", operands);
  arm_output_asm_insn (\"mvn%D6\\t%0, %1\", operands);
  sprintf (pattern, \"%s%%d6\\t%%0, %%2, %%3\", arithmetic_instr (operands[7],
								  FALSE));
  return arm_output_asm_insn (pattern, operands);
}
"
[(set_attr "conds" "clob")
 (set_attr "length" "3")])

(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(if_then_else:SI (match_operator 5 "comparison_operator"
			  [(match_operand:SI 3 "s_register_operand" "r,r")
			   (match_operand:SI 4 "arm_add_operand" "rL,rL")])
			 (neg:SI
			  (match_operand:SI 2 "s_register_operand" "r,r"))
			 (match_operand:SI 1 "arm_rhs_operand" "0,?rI")))
   (clobber (reg:CC 24))]
  ""
  "*
  if (GET_CODE (operands[4]) == CONST_INT
      && !const_ok_for_arm (INTVAL (operands[4])))
    arm_output_asm_insn (\"cmn\\t%3, #%n4\", operands);
  else
    arm_output_asm_insn (\"cmp\\t%3, %4\", operands);
  if (which_alternative != 0)
    arm_output_asm_insn (\"mov%D5\\t%0, %1\", operands);
  return arm_output_asm_insn (\"rsb%d5\\t%0, %2, #0\", operands);
"
[(set_attr "conds" "clob")
 (set_attr "length" "2,3")])

(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=r,r")
	(if_then_else:SI (match_operator 5 "comparison_operator"
			  [(match_operand:SI 3 "s_register_operand" "r,r")
			   (match_operand:SI 4 "arm_add_operand" "rL,rL")])
			 (match_operand:SI 1 "arm_rhs_operand" "0,?rI")
			 (neg:SI
			  (match_operand:SI 2 "s_register_operand" "r,r"))))
   (clobber (reg:CC 24))]
  ""
  "*
  if (GET_CODE (operands[4]) == CONST_INT
      && !const_ok_for_arm (INTVAL (operands[4])))
    arm_output_asm_insn (\"cmn\\t%3, #%n4\", operands);
  else
    arm_output_asm_insn (\"cmp\\t%3, %4\", operands);
  if (which_alternative != 0)
    arm_output_asm_insn (\"mov%d5\\t%0, %1\", operands);
  return arm_output_asm_insn (\"rsb%D5\\t%0, %2, #0\", operands);
"
[(set_attr "conds" "clob")
 (set_attr "length" "2,3")])

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
  rtx arith[3];
  char pattern[100];
  int val1 = 0, val2 = 0;

  sprintf (pattern, \"%s\\t%%0, %%1, %%2\",
	   arithmetic_instr (operands[1], FALSE));
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
	arm_output_asm_insn (\"ldmia\\t%0, {%1, %2}\", ldm);
      else
	arm_output_asm_insn (\"ldmda\\t%0, {%1, %2}\", ldm);
    }
  else if (val1)
    {
      ldm[0] = XEXP (operands[3], 0);
      if (val1 < val2)
	arm_output_asm_insn (\"ldmda\\t%0, {%1, %2}\", ldm);
      else
	arm_output_asm_insn (\"ldmia\\t%0, {%1, %2}\", ldm);
    }
  else
    {
      ldm[0] = XEXP (operands[2], 0);
      if (val1 < val2)
	arm_output_asm_insn (\"ldmia\\t%0, {%1, %2}\", ldm);
      else
	arm_output_asm_insn (\"ldmda\\t%0, {%1, %2}\", ldm);
    }
  return arm_output_asm_insn (pattern, arith);
}
"
[(set_attr "length" "3")
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
  "*
  return arm_output_asm_insn (\"strb\\t%3, [%0, %2]!\", operands);
"
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
  "*
  return arm_output_asm_insn (\"strb\\t%3, [%0, -%2]!\", operands);
"
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
  "*
  return arm_output_asm_insn (\"ldrb\\t%3, [%0, %2]!\", operands);
"
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
  "*
  return arm_output_asm_insn (\"ldrb\\t%3, [%0, -%2]!\", operands);
"
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
  "*
  return arm_output_asm_insn (\"ldrb\\t%3, [%0, %2]!\\t@ z_extendqisi\",
			      operands);
"
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
  "*
  return arm_output_asm_insn (\"ldrb\\t%3, [%0, -%2]!\\t@ z_extendqisi\",
			      operands);
"
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
  "*
  return arm_output_asm_insn (\"str\\t%3, [%0, %2]!\", operands);
"
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
  "*
  return arm_output_asm_insn (\"str\\t%3, [%0, -%2]!\", operands);
"
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
  "*
  return arm_output_asm_insn (\"ldr\\t%3, [%0, %2]!\", operands);
"
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
  "*
  return arm_output_asm_insn (\"ldr\\t%3, [%0, -%2]!\", operands);
"
[(set_attr "type" "load")])

(define_insn ""
  [(set (match_operand:HI 3 "s_register_operand" "=r")
	(mem:HI (plus:SI (match_operand:SI 1 "s_register_operand" "%0")
			 (match_operand:SI 2 "index_operand" "rJ"))))
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(plus:SI (match_dup 1) (match_dup 2)))]
  "REGNO (operands[0]) != FRAME_POINTER_REGNUM
   && REGNO (operands[1]) != FRAME_POINTER_REGNUM
   && (GET_CODE (operands[2]) != REG
       || REGNO (operands[2]) != FRAME_POINTER_REGNUM)"
  "*
  return arm_output_asm_insn (\"ldr\\t%3, [%0, %2]!\\t@ loadhi\", operands);
"
[(set_attr "type" "load")])

(define_insn ""
  [(set (match_operand:HI 3 "s_register_operand" "=r")
	(mem:HI (minus:SI (match_operand:SI 1 "s_register_operand" "0")
			  (match_operand:SI 2 "s_register_operand" "r"))))
   (set (match_operand:SI 0 "s_register_operand" "=r")
	(minus:SI (match_dup 1) (match_dup 2)))]
  "REGNO (operands[0]) != FRAME_POINTER_REGNUM
   && REGNO (operands[1]) != FRAME_POINTER_REGNUM
   && (GET_CODE (operands[2]) != REG
       || REGNO (operands[2]) != FRAME_POINTER_REGNUM)"
  "*
  return arm_output_asm_insn (\"ldr\\t%3, [%0, -%2]!\\t@ loadhi\", operands);
"
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
  "*
{
  char instr[100];

  sprintf (instr, \"strb\\t%%5, [%%0, %%3, %s %%4]!\",
	   shift_instr (GET_CODE (operands[2]), &operands[4]));
  return arm_output_asm_insn (instr, operands);
}
"
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
  "*
{
  char instr[100];

  sprintf (instr, \"strb\\t%%5, [%%0, -%%3, %s %%4]!\",
	   shift_instr (GET_CODE (operands[2]), &operands[4]));
  return arm_output_asm_insn (instr, operands);
}
"
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
  "*
{
  char instr[100];

  sprintf (instr, \"ldrb\\t%%5, [%%0, %%3, %s %%4]!\",
	   shift_instr (GET_CODE (operands[2]), &operands[4]));
  return arm_output_asm_insn (instr, operands);
}
"
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
  "*
{
  char instr[100];

  sprintf (instr, \"ldrb\\t%%5, [%%0, -%%3, %s %%4]!\",
	   shift_instr (GET_CODE (operands[2]), &operands[4]));
  return arm_output_asm_insn (instr, operands);
}
"
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
  "*
{
  char instr[100];

  sprintf (instr, \"str\\t%%5, [%%0, %%3, %s %%4]!\",
	   shift_instr (GET_CODE (operands[2]), &operands[4]));
  return arm_output_asm_insn (instr, operands);
}
"
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
  "*
{
  char instr[100];

  sprintf (instr, \"str\\t%%5, [%%0, -%%3, %s %%4]!\",
	   shift_instr (GET_CODE (operands[2]), &operands[4]));
  return arm_output_asm_insn (instr, operands);
}
"
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
  "*
{
  char instr[100];

  sprintf (instr, \"ldr\\t%%5, [%%0, %%3, %s %%4]!\",
	   shift_instr (GET_CODE (operands[2]), &operands[4]));
  return arm_output_asm_insn (instr, operands);
}
"
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
  "*
{
  char instr[100];

  sprintf (instr, \"ldr\\t%%5, [%%0, -%%3, %s %%4]!\",
	   shift_instr (GET_CODE (operands[2]), &operands[4]));
  return arm_output_asm_insn (instr, operands);
}
"
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
  "REGNO (operands[0]) != FRAME_POINTER_REGNUM
   && REGNO (operands[1]) != FRAME_POINTER_REGNUM
   && REGNO (operands[3]) != FRAME_POINTER_REGNUM"
  "*
{
  char instr[100];

  sprintf (instr, \"ldr\\t%%5, [%%0, %%3, %s %%4]!\\t@ loadhi\",
	   shift_instr (GET_CODE (operands[2]), &operands[4]));
  return arm_output_asm_insn (instr, operands);
}
"
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
  "REGNO (operands[0]) != FRAME_POINTER_REGNUM
   && REGNO (operands[1]) != FRAME_POINTER_REGNUM
   && REGNO (operands[3]) != FRAME_POINTER_REGNUM"
  "*
{
  char instr[100];

  sprintf (instr, \"ldr\\t%%5, [%%0, -%%3, %s %%4]!\\t@ loadhi\",
	   shift_instr (GET_CODE (operands[2]), &operands[4]));
  return arm_output_asm_insn (instr, operands);
}
"
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
  "*
  return arm_output_asm_insn (\"strb\\t%2, [%0], %1\", operands);
")

(define_peephole
  [(set (match_operand:QI 0 "s_register_operand" "=r")
	(mem:QI (match_operand:SI 1 "s_register_operand" "+r")))
   (set (match_dup 1)
	(plus:SI (match_dup 1) (match_operand:SI 2 "index_operand" "rJ")))]
  "REGNO(operands[0]) != REGNO(operands[1])
   && (GET_CODE (operands[2]) != REG
       || REGNO(operands[0]) != REGNO (operands[2]))"
  "*
  return arm_output_asm_insn (\"ldrb\\t%0, [%1], %2\", operands);
")

(define_peephole
  [(set (mem:SI (match_operand:SI 0 "s_register_operand" "+r"))
	(match_operand:SI 2 "s_register_operand" "r"))
   (set (match_dup 0)
	(plus:SI (match_dup 0) (match_operand:SI 1 "index_operand" "rJ")))]
  ""
  "*
  return arm_output_asm_insn (\"str\\t%2, [%0], %1\", operands);
")

(define_peephole
  [(set (match_operand:HI 0 "s_register_operand" "=r")
	(mem:HI (match_operand:SI 1 "s_register_operand" "+r")))
   (set (match_dup 1)
	(plus:SI (match_dup 1) (match_operand:SI 2 "index_operand" "rJ")))]
  "REGNO(operands[0]) != REGNO(operands[1])
   && (GET_CODE (operands[2]) != REG
       || REGNO(operands[0]) != REGNO (operands[2]))"
  "*
  return arm_output_asm_insn (\"ldr\\t%0, [%1], %2\\t@ loadhi\", operands);
")

(define_peephole
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(mem:SI (match_operand:SI 1 "s_register_operand" "+r")))
   (set (match_dup 1)
	(plus:SI (match_dup 1) (match_operand:SI 2 "index_operand" "rJ")))]
  "REGNO(operands[0]) != REGNO(operands[1])
   && (GET_CODE (operands[2]) != REG
       || REGNO(operands[0]) != REGNO (operands[2]))"
  "*
  return arm_output_asm_insn (\"ldr\\t%0, [%1], %2\", operands);
")

; This pattern is never tried by combine, so do it as a peephole

(define_peephole
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(match_operand:SI 1 "s_register_operand" "r"))
   (set (match_operand 2 "cc_register" "")
	(compare (match_dup 1) (const_int 0)))]
  ""
  "*
  return arm_output_asm_insn (\"subs\\t%0, %1, #0\", operands);
"
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
  "*
  return arm_output_asm_insn (\"ldmia\\t%1, {%4, %3, %2, %0}\\t@ phole ldm\",
			      operands);
")

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
  "*
  return arm_output_asm_insn (\"ldmia\\t%1, {%3, %2, %0}\\t@ phole ldm\",
			      operands);
")

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
  "*
  return arm_output_asm_insn (\"ldmia\\t%1, {%2, %0}\\t@ phole ldm\",
			      operands);
")

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
  "*
  return arm_output_asm_insn (\"stmia\\t%1, {%4, %3, %2, %0}\\t@ phole stm\",
			      operands);
")

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
  "*
  return arm_output_asm_insn (\"stmia\\t%1, {%3, %2, %0}\\t@ phole stm\",
			      operands);
")

(define_peephole
  [(set (mem:SI (plus:SI (match_operand:SI 1 "s_register_operand" "r")
                         (const_int 4)))
        (match_operand:SI 0 "s_register_operand" "r"))
   (set (mem:SI (match_dup 1))
        (match_operand:SI 2 "s_register_operand" "r"))]
  "REGNO (operands[0]) >  REGNO (operands[2])
   && !MEM_VOLATILE_P (SET_DEST (PATTERN (insn)))
   && !MEM_VOLATILE_P (SET_DEST (PATTERN (prev_nonnote_insn (insn))))"
  "*
  return arm_output_asm_insn (\"stmia\\t%1, {%2, %0}\\t@ phole stm\",
			      operands);
")

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
  return (arm_output_asm_insn (\"b\\t%a0\", operands));
}"
[(set (attr "conds")
      (if_then_else (eq_attr "cpu" "arm6")
		    (const_string "clob")
		    (const_string "nocond")))
 (set_attr "length" "2")])

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
  return (arm_output_asm_insn (\"b\\t%a1\", operands));
}"
[(set (attr "conds")
      (if_then_else (eq_attr "cpu" "arm6")
		    (const_string "clob")
		    (const_string "nocond")))
 (set_attr "length" "2")])

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
  return (arm_output_asm_insn (\"b\\t%a1\", operands));
}"
[(set (attr "conds")
      (if_then_else (eq_attr "cpu" "arm6")
		    (const_string "clob")
		    (const_string "nocond")))
 (set_attr "length" "2")])

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
  "GET_CODE (operands[0]) == SYMBOL_REF 
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
	arm_output_asm_insn (\"sub\\tlr, pc, #(8 + . -%l2)\", operands);
      else
	arm_output_asm_insn (\"add\\tlr, pc, #(%l2 - . -8)\", operands);
    }
  else
#endif
    {
      arm_output_asm_insn (\"mov\\tlr, pc\\t@ protect cc\");
      if (backward)
	arm_output_asm_insn (\"sub\\tlr, lr, #(4 + . -%l2)\", operands);
      else
	arm_output_asm_insn (\"add\\tlr, lr, #(%l2 - . -4)\", operands);
    }
  return arm_output_asm_insn (\"b\\t%a0\", operands);
}"
[(set (attr "conds")
      (if_then_else (eq_attr "cpu" "arm6")
		    (const_string "clob")
		    (const_string "nocond")))
 (set (attr "length")
      (if_then_else (eq_attr "cpu" "arm6")
		    (const_int 2)
		    (const_int 3)))])

(define_peephole
  [(parallel[(set (match_operand:SI 0 "s_register_operand" "=r")
		  (call (mem:SI (match_operand:SI 1 "" "i"))
                        (match_operand:SI 2 "general_operand" "g")))
             (clobber (reg:SI 14))])
   (set (pc)
        (label_ref (match_operand 3 "" "")))]
  "GET_CODE (operands[0]) == SYMBOL_REF
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
	arm_output_asm_insn (\"sub\\tlr, pc, #(8 + . -%l3)\", operands);
      else
	arm_output_asm_insn (\"add\\tlr, pc, #(%l3 - . -8)\", operands);
    }
  else
#endif
    {
      arm_output_asm_insn (\"mov\\tlr, pc\\t@ protect cc\");
      if (backward)
	arm_output_asm_insn (\"sub\\tlr, lr, #(4 + . -%l3)\", operands);
      else
	arm_output_asm_insn (\"add\\tlr, lr, #(%l3 - . -4)\", operands);
    }
  return arm_output_asm_insn (\"b\\t%a1\", operands);
}"
[(set (attr "conds")
      (if_then_else (eq_attr "cpu" "arm6")
		    (const_string "clob")
		    (const_string "nocond")))
 (set (attr "length")
      (if_then_else (eq_attr "cpu" "arm6")
		    (const_int 2)
		    (const_int 3)))])

(define_split
  [(set (pc)
	(if_then_else (match_operator 0 "comparison_operator"
		       [(match_operator:SI 1 "shift_operator"
			 [(match_operand:SI 2 "s_register_operand" "r")
			  (match_operand:SI 3 "nonmemory_operand" "rn")])
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
  arm_output_asm_insn (\"ldrb\\t%2, %0\", operands);
  return arm_output_asm_insn (\"cmp\\t%2, %1\", operands);
"
[(set_attr "conds" "set")
 (set_attr "length" "2")
 (set_attr "type" "load")])

(define_expand "save_stack_nonlocal"
  [(match_operand:DI 0 "memory_operand" "")
   (match_operand:SI 1 "s_register_operand" "")]
  ""
  "
{
  /* We also need to save the frame pointer for non-local gotos */
  emit_move_insn (operand_subword (operands[0], 0, 0, DImode),
		  hard_frame_pointer_rtx);
  emit_move_insn (operand_subword (operands[0], 1, 0, DImode), operands[1]);
  DONE;
}")

(define_expand "restore_stack_nonlocal"
  [(match_operand:SI 0 "s_register_operand" "")
   (match_operand:DI 1 "memory_operand" "")]
  ""
  "
{
  /* Restore the frame pointer first, the stack pointer second. */
  emit_move_insn (operands[0], operand_subword (operands[1], 1, 0, DImode));
  emit_move_insn (hard_frame_pointer_rtx, operand_subword (operands[1], 0, 0,
							   DImode));
  DONE;
}")

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
			  [(match_operand 3 "cc_register" "") (const_int 0)])
			 (match_operand:SI 1 "arm_rhs_operand" "0,?rI")
			 (not:SI
			  (match_operand:SI 2 "s_register_operand" "r,r"))))]
  ""
  "*
  if (which_alternative != 0)
    arm_output_asm_insn (\"mov%d4\\t%0, %1\", operands);
  return arm_output_asm_insn (\"mvn%D4\\t%0, %2\", operands);
"
[(set_attr "conds" "use")
 (set_attr "length" "1,2")])

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
  arm_output_asm_insn (\"ands\\t%0, %1, %2\", operands);
  return arm_output_asm_insn (\"mvnne\\t%0, #0\", operands);
"
[(set_attr "conds" "clob")
 (set_attr "length" "2")])

(define_insn ""
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(not:SI
	 (sign_extract:SI (match_operand:SI 1 "s_register_operand" "r")
			  (const_int 1)
			  (match_operand:SI 2 "immediate_operand" "n"))))]
  ""
  "*
  operands[2] = GEN_INT (1 << INTVAL (operands[2]));
  arm_output_asm_insn (\"tst\\t%1, %2\", operands);
  arm_output_asm_insn (\"mvneq\\t%0, #0\", operands);
  return arm_output_asm_insn (\"movne\\t%0, #0\", operands);
"
[(set_attr "conds" "clob")
 (set_attr "length" "3")])
