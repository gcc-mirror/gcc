;;- Machine description for the AT&T DSP1600 for GCC
;;  Copyright (C) 1994, 1995, 1997, 1998, 2001, 2002
;;  Free Software Foundation, Inc.
;;  Contributed by Michael Collison (collison@isisinc.net).

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING.  If not, write to
;; the Free Software Foundation, 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


;;- See file "rtl.def" for documentation on define_insn, match_*, et. al.

;; Attribute specifications

; Type of each instruction.  Default is arithmetic.
; I'd like to write the list as this, but genattrtab won't accept it.
;
; "jump,cond_jump,call,			; flow-control instructions
;  load_i,load, store, move		; Y space address arithmetic instructions
;  malu,special,f3_alu,f3_alu_i		; data arithmetic unit instructions
;  shift_i,shift, bfield_i, bfield	; bit manipulation unit instructions
;  arith,				; integer unit instructions
;  nop

; Classification of each insn.  Some insns of TYPE_BRANCH are multi-word.
(define_attr "type"
  "jump,cond_jump,call,load_i,load,move,store,malu,malu_mul,tstqi,special,special_2,f3_alu,f3_alu_i,f3_alu_i_mult,shift_i,shift,shift_multiple,shift_i_multiple,bfield_i,bfield,nop,ld_short_i,data_move,data_move_i,data_move_memory,data_move_memory_2,data_move_short_i,data_move_multiple,data_move_2,nothing"
  (const_string "malu"))

;; Data arithmetic unit
(define_function_unit "dau" 1 1 (eq_attr "type" "data_move,data_move_i,f3_alu_i") 2 0)

(define_function_unit "dau" 1 1 (eq_attr "type" "special_2") 3 0)

(define_function_unit "dau" 1 1 (eq_attr "type" "data_move_2") 4 0)

;; Bit manipulation
(define_function_unit "bmu" 1 1 (eq_attr "type" "shift_i,shift_i_multiple") 2 0)

(define_function_unit "bmu" 1 1 (eq_attr "type" "shift_multiple") 4 0)

;; Y-memory addressing arithmetic unit
(define_function_unit "yaau" 1 1 (eq_attr "type" "data_move_memory") 2 0)

(define_function_unit "yaau" 1 1 (eq_attr "type" "data_move_memory_2") 4 0)


;;  ....................
;;
;;  Test against 0 instructions
;;
;;  ....................

(define_expand "tsthi"
  [(set (cc0)
        (match_operand:HI 0 "register_operand" ""))]
  ""
  "
{
  dsp16xx_compare_gen = false;
  dsp16xx_compare_op0 = operands[0];
  dsp16xx_compare_op1 = const0_rtx;
  DONE;
}")

(define_insn "tsthi_1"
  [(set (cc0)
	(match_operand:HI 0 "register_operand" "A"))]
  ""
  "%0=%0"
  [(set_attr "type" "malu")])

(define_expand "tstqi"
  [(set (cc0)
        (match_operand:QI 0 "register_operand" ""))]
  ""
  "
{
  dsp16xx_compare_gen = false;
  dsp16xx_compare_op0 = operands[0];
  dsp16xx_compare_op1 = const0_rtx;
  DONE;
}")

(define_split
  [(set (cc0)
	(match_operand:QI 0 "register_operand" "j,q"))
   (clobber (match_scratch:QI 1 "=k,u"))]
  "reload_completed"
  [(set (match_dup 1)
	(const_int 0))
   (parallel [(set (cc0)
		   (match_dup 0))
	      (use (match_dup 1))])]
  "")

(define_insn "tstqi_split"
  [(set (cc0)
	(match_operand:QI 0 "register_operand" "j,q"))
   (use (match_scratch:QI 1 "=k,u"))]
  ""
  "@
   %b0-0
   %b0-0"
  [(set_attr "type" "f3_alu_i,f3_alu_i")])

(define_insn "tstqi_1"
  [(set (cc0)
	(match_operand:QI 0 "register_operand" "j,q"))
   (clobber (match_scratch:QI 1 "=k,u"))]
  ""
  "@
   %1=0\;%b0-0
   %1=0\;%b0-0"
  [(set_attr "type" "tstqi,tstqi")])


;;
;;  ....................
;;
;;  Bit test instructions
;;
;;  ....................

(define_insn ""
  [(set (cc0)
	(and:HI (match_operand:HI 0 "register_operand" "A,!A")
		(match_operand:HI 1 "register_operand" "Z,A")))]
  "" 
  "*
{
	switch (which_alternative)
	{
	   case 0:
	   case 1:
   	      return \"%0&%1\";
           default:
             abort();
         }
}"
  [(set_attr "type" "f3_alu,f3_alu")])


;;(define_insn ""
;;  [(set (cc0)
;;	(and:QI (match_operand:QI 0 "register_operand" "h")
;;		(match_operand:QI 1 "const_int_operand" "I")))]
;;  "" 
;;  "%b0&%H1"
;;  [(set_attr "type" "f3_alu_i")])

;;
;;
;; Compare Instructions
;;

(define_expand "cmphi"
  [(parallel [(set (cc0)
		   (compare (match_operand:HI 0 "general_operand" "")
			    (match_operand:HI 1 "general_operand" "")))
	      (clobber (match_scratch:QI 2 ""))
	      (clobber (match_scratch:QI 3 ""))
	      (clobber (match_scratch:QI 4 ""))
	      (clobber (match_scratch:QI 5 ""))])]
  ""
  "
{
  if (GET_CODE (operands[1]) == CONST_INT)
    operands[1] = force_reg (HImode, operands[1]);

  dsp16xx_compare_gen = true;
  dsp16xx_compare_op0 = operands[0];
  dsp16xx_compare_op1 = operands[1];
  DONE;
}")

(define_insn ""
  [(set (cc0) 
	(compare (match_operand:HI 0 "general_operand" "Z*r*m*i")
		 (match_operand:HI 1 "general_operand" "Z*r*m*i")))
   (clobber (match_scratch:QI 2 "=&A"))
   (clobber (match_scratch:QI 3 "=&A"))
   (clobber (match_scratch:QI 4 "=&A"))
   (clobber (match_scratch:QI 5 "=&A"))]
  "next_cc_user_unsigned (insn)"
  "*
{
  if (GET_CODE(operands[0]) == REG)
    {
      if (REGNO (operands[0]) == REG_Y ||
	  REGNO (operands[0]) == REG_PROD)
	{
	  output_asm_insn (\"a0=%0\", operands);
	}
      else if (IS_YBASE_REGISTER_WINDOW (REGNO (operands[0])))
	output_asm_insn (\"a0=%u0\;a0l=%w0\", operands);
      else
	fatal_error (\"Invalid register for compare\");
    }
  else if (GET_CODE(operands[0]) == CONST_INT)
    output_asm_insn (\"a0=%U0\;a0l=%H0\", operands);
  else if (GET_CODE (operands[0]) == MEM)
    {
      rtx xoperands[2];

      xoperands[0] = gen_rtx_REG (HImode, REG_A0);
      xoperands[1] = operands[0];
      double_reg_from_memory (xoperands);
    }

  if (GET_CODE(operands[1]) == REG)
    {
      if (REGNO (operands[1]) == REG_Y || REGNO (operands[1]) == REG_PROD)
	output_asm_insn (\"a1=%1\", operands);
      else if (IS_YBASE_REGISTER_WINDOW (REGNO (operands[1])))
	output_asm_insn (\"a1=%u1\;a1l=%w1\", operands);
      else
	fatal_error (\"Invalid register for compare\");
    }
  else if (GET_CODE (operands[1]) == MEM)
    {
      rtx xoperands[2];

      xoperands[0] = gen_rtx_REG (HImode, REG_A1);
      xoperands[1] = operands[1];
      double_reg_from_memory (xoperands);
    }
  else if (GET_CODE(operands[1]) == CONST_INT)
    {
      output_asm_insn (\"a1=%U1\;a1l=%H1\", operands);
    }
  
  return \"psw = 0\;a0 - a1\";
}")

(define_insn ""
  [(set (cc0) (compare (match_operand:HI 0 "register_operand" "A,!A")
		       (match_operand:HI 1 "register_operand" "Z,*A")))]
  ""
  "@
   %0-%1
   %0-%1"
  [(set_attr "type" "malu,f3_alu")])

(define_expand "cmpqi"
  [(parallel [(set (cc0)
		   (compare (match_operand:QI 0 "register_operand" "")
			    (match_operand:QI 1 "nonmemory_operand" "")))
	      (clobber (match_operand:QI 2 "register_operand" ""))
	      (clobber (match_operand:QI 3 "register_operand" ""))])]
  ""
  "
 {
  if (operands[0])	/* Avoid unused code warning */
    {
      dsp16xx_compare_gen = true;
      dsp16xx_compare_op0 = operands[0];
      dsp16xx_compare_op1 = operands[1];
      DONE;
    }
 }")

(define_split
  [(set (cc0)
	(compare (match_operand:QI 0 "register_operand" "")
		 (match_operand:QI 1 "register_operand" "")))
   (clobber (match_scratch:QI 2 ""))
   (clobber (match_scratch:QI 3 ""))]
  "reload_completed && next_cc_user_unsigned (insn)"
  [(set (match_dup 2)
	(const_int 0))
   (set (match_dup 3)
	(const_int 0))
   (parallel [(set (cc0)
		   (compare (match_dup 0)
			    (match_dup 1)))
	      (use (match_dup 2))
	      (use (match_dup 3))])]
  "")

(define_split
  [(set (cc0)
	(compare (match_operand:QI 0 "register_operand" "")
		 (match_operand:QI 1 "const_int_operand" "")))
   (clobber (match_scratch:QI 2 ""))
   (clobber (match_scratch:QI 3 ""))]
  "reload_completed && next_cc_user_unsigned (insn)"
  [(set (match_dup 2)
	(const_int 0))
   (parallel [(set (cc0)
		   (compare (match_dup 0)
			    (match_dup 1)))
	      (use (match_dup 2))])]
  "")

(define_insn "cmpqi_split_unsigned_reg"
  [(set (cc0) (compare (match_operand:QI 0 "register_operand"  "k,k,!k,u,u,!u")
                       (match_operand:QI 1 "register_operand"  "w,z,u,w,z,k")))
	(use (match_scratch:QI 2 "=j,j,j,q,q,q"))
	(use (match_scratch:QI 3 "=v,y,q,v,y,j"))]
  "next_cc_user_unsigned (insn)"
  "@
   %2-%3
   %2-%3
   %2-%3
   %2-%3
   %2-%3
   %2-%3"
  [(set_attr "type" "malu,malu,malu,malu,malu,malu")])

(define_insn "cmpqi_split_unsigned_int"
  [(set (cc0) (compare (match_operand:QI 0 "register_operand"  "k,u")
                       (match_operand:QI 1 "const_int_operand" "i,i")))
	(use (match_scratch:QI 2 "=j,q"))]
  "next_cc_user_unsigned (insn)"
  "@
   %0-%H1
   %0-%H1"
  [(set_attr "type" "f3_alu_i,f3_alu_i")])

(define_insn ""
  [(set (cc0) (compare (match_operand:QI 0 "register_operand"  "k,k,!k,k,u,u,!u,u")
                       (match_operand:QI 1 "nonmemory_operand" "w,z,u,i,w,z,k,i")))
	(clobber (match_scratch:QI 2 "=j,j,j,j,q,q,q,q"))
	(clobber (match_scratch:QI 3 "=v,y,q,X,v,y,j,X"))]
  "next_cc_user_unsigned (insn)"
  "@
   %2=0\;%3=0\;%2-%3
   %2=0\;%3=0\;%2-%3
   %2=0\;%3=0\;%2-%3
   %2=0\;%0-%H1
   %2=0\;%3=0\;%2-%3
   %2=0\;%3=0\;%2-%3
   %2=0\;%3=0\;%2-%3
   %2=0\;%0-%H1")

(define_split
  [(set (cc0)
	(compare (match_operand:QI 0 "register_operand" "")
		 (match_operand:QI 1 "register_operand" "")))
   (clobber (match_scratch:QI 2 ""))
   (clobber (match_scratch:QI 3 ""))]
  "reload_completed"
  [(set (match_dup 2)
	(const_int 0))
   (set (match_dup 3)
	(const_int 0))
   (parallel [(set (cc0)
		   (compare (match_dup 0)
			    (match_dup 1)))
	      (use (match_dup 2))
	      (use (match_dup 3))])]
  "")

(define_split
  [(set (cc0)
	(compare (match_operand:QI 0 "register_operand" "")
		 (match_operand:QI 1 "const_int_operand" "")))
   (clobber (match_scratch:QI 2 ""))
   (clobber (match_scratch:QI 3 ""))]
  "reload_completed"
  [(set (match_dup 2)
	(const_int 0))
   (parallel [(set (cc0)
		   (compare (match_dup 0)
			    (match_dup 1)))
	      (use (match_dup 2))])]
  "")

(define_insn "cmpqi_split_reg"
  [(set (cc0) (compare (match_operand:QI 0 "register_operand"  "j,j,!j,q,q,!q")
                       (match_operand:QI 1 "register_operand"  "v,y,q,v,y,j")))
	(use (match_scratch:QI 2 "=k,k,k,u,u,u"))
	(use (match_scratch:QI 3 "=w,z,u,w,z,k"))]
  ""
  "@
   %0-%1
   %0-%1
   %0-%1
   %0-%1
   %0-%1
   %0-%1"
  [(set_attr "type" "malu,malu,malu,malu,malu,malu")])


(define_insn "cmpqi_split_int"
  [(set (cc0) (compare (match_operand:QI 0 "register_operand"  "j,q")
                       (match_operand:QI 1 "const_int_operand" "i,i")))
	(use (match_scratch:QI 2 "=k,u"))]
  ""
  "@
   %b0-%H1
   %b0-%H1"
  [(set_attr "type" "f3_alu_i,f3_alu_i")])

(define_insn ""
  [(set (cc0) (compare (match_operand:QI 0 "register_operand"  "j,j,!j,j,q,q,!q,q")
                       (match_operand:QI 1 "nonmemory_operand" "v,y,q,i,v,y,j,i")))
	(clobber (match_scratch:QI 2 "=k,k,k,k,u,u,u,u"))
	(clobber (match_scratch:QI 3 "=w,z,u,X,w,z,k,X"))]
  ""
  "@
   %2=0\;%3=0\;%0-%1
   %2=0\;%3=0\;%0-%1
   %2=0\;%3=0\;%0-%1
   %2=0\;%b0-%H1
   %2=0\;%3=0\;%0-%1
   %2=0\;%3=0\;%0-%1
   %2=0\;%3=0\;%0-%1
   %2=0\;%b0-%H1")


(define_expand "cmphf"
  [(set (cc0)
	(compare (match_operand:HF 0 "register_operand" "")
		 (match_operand:HF 1 "nonmemory_operand" "")))]
  ""
  "
{
  if (!dsp16xx_cmphf3_libcall)
    dsp16xx_cmphf3_libcall = gen_rtx_SYMBOL_REF (Pmode, CMPHF3_LIBCALL);

   dsp16xx_compare_gen = true;
   dsp16xx_compare_op0 = operands[0];
   dsp16xx_compare_op1 = operands[1];
   emit_library_call (dsp16xx_cmphf3_libcall, 1, HImode, 2,
		      operands[0], HFmode,
		      operands[1], HFmode);
   emit_insn (gen_tsthi_1 (copy_to_reg(hard_libcall_value (HImode))));
   DONE;
}")


;;  ....................
;;
;;  Add instructions
;;
;;  ....................

(define_split
  [(set (match_operand:HI 0 "register_operand" "")
	(plus:HI (match_operand:HI 1 "register_operand" "")
		 (match_operand:HI 2 "const_int_operand" "")))]
  "reload_completed && !ADD_LOW_16(INTVAL(operands[2])) && 
   !ADD_HIGH_16(INTVAL(operands[2]))"
  [(parallel [(set (match_dup 3)
		   (plus:QI (match_dup 4)
			    (match_dup 5)))
	      (clobber (match_dup 6))])

   (parallel [(set (match_dup 6)
		   (plus:QI (match_dup 7)
			    (match_dup 8)))
	      (clobber (match_scratch:QI 9 ""))])]
  "
{
  operands[3] = gen_lowpart(QImode, operands[0]);
  operands[4] = gen_lowpart(QImode, operands[1]);
  operands[5] = gen_rtx (CONST_INT, VOIDmode, INTVAL (operands[2]) & 0xffff);

  operands[6] = gen_highpart(QImode, operands[0]);
  operands[7] = gen_highpart(QImode, operands[0]);
  operands[8] = gen_rtx (CONST_INT, VOIDmode, (((INTVAL (operands[2]) & 0xffff0000) >> 16) & 0xffff));
}")


(define_insn "addhi3"
  [(set (match_operand:HI 0 "register_operand" "=A,A,A,A,A")
	(plus:HI (match_operand:HI 1 "register_operand" "%A,A,A,A,A")
		 (match_operand:HI 2 "nonmemory_operand" "Z,d,L,M,?i")))]
  ""
  "@
   %0=%1+%2
   %0=%1+%2
   %0=%w1+%H2
   %0=%b1+%U2
   %0=%w1+%H2\;%0=%b0+%U2"
  [(set_attr "type" "malu,malu,f3_alu_i,f3_alu_i,f3_alu_i")])

(define_insn ""
  [(set (match_operand:QI 0 "register_operand" "=k,u,!k,!u")
	(plus:QI (plus:QI (match_operand:QI 1 "register_operand" "uk,uk,uk,uk")
			  (match_operand:QI 2 "register_operand" "wz,wz,uk,uk"))
		 (match_operand:QI 3 "immediate_operand" "i,i,i,i")))
   (clobber (match_scratch:QI 4 "=j,q,j,q"))]
  ""
  "@
   %m0=%m1+%m2\;%m0=%0+%H3
   %m0=%m1+%m2\;%m0=%0+%H3
   %m0=%m1+%m2\;%m0=%0+%H3
   %m0=%m1+%m2\;%m0=%0+%H3")

(define_expand "addqi3"
  [(parallel [(set (match_operand:QI 0 "register_operand" "")
		   (plus:QI (match_operand:QI 1 "register_operand" "")
			    (match_operand:QI 2 "nonmemory_operand" "")))
	      (clobber (match_scratch:QI 3 ""))])]
  ""
  "
{
  if (reload_in_progress)
    {
      if (REG_P (operands[1]) && 
	  (REGNO(operands[1]) == STACK_POINTER_REGNUM ||
	   REGNO(operands[1]) == FRAME_POINTER_REGNUM) &&
	  GET_CODE (operands[2]) == CONST_INT)
	{
	  if (REG_P (operands[0]) && IS_ACCUM_REG(REGNO(operands[0])))
	    emit_move_insn (operands[0], operands[1]);

	  operands[1] = operands[0];
	}
    }
}")

	
(define_insn "match_addqi3"
  [(set (match_operand:QI 0 "register_operand" "=a,a,k,u,k,u,!k,!u,j,j,q,q")
	(plus:QI (match_operand:QI 1 "register_operand" "0,0,uk,uk,uk,uk,uk,uk,0,q,0,j")
		 (match_operand:QI 2 "nonmemory_operand" "W,N,i,i,wz,wz,uk,uk,i,i,i,i")))
	(clobber (match_scratch:QI 3 "=X,X,j,q,j,q,j,q,X,k,X,u"))]
  ""
  "*
{
  switch (which_alternative)
    {
    case 0:
      return \"*%0++%2\";

    case 1:
      switch (INTVAL (operands[2]))
	{
	case -1:
	  return \"*%0--\";

	case 1:
	  return \"*%0++\";

	case -2:
	  return \"*%0--\;*%0--\";

	case 2:
	  return \"*%0++\;*%0++\";
        default:
          abort();
	}

    case 2:
    case 3:
        return \"%m0=%1+%H2\";

    case 4:
    case 5:
        return \"%m0=%m1+%m2\";


    case 6:
    case 7:
      return \"%m0=%m1+%m2\";

    case 8:
    case 9:
    case 10:
    case 11:
      return \"%0=%b1+%H2\";
    default:
      abort();
    }
}"
[(set_attr "type" "data_move_memory,data_move_multiple,f3_alu_i,f3_alu_i,f3_alu,f3_alu,f3_alu,f3_alu,f3_alu_i,f3_alu_i,f3_alu_i,f3_alu_i")])


(define_expand "addhf3"
  [(set (match_operand:HF 0 "register_operand" "")
	(plus:HF (match_operand:HF 1 "register_operand" "")
		 (match_operand:HF 2 "nonmemory_operand" "")))]
  ""
  "
{
  if (!dsp16xx_addhf3_libcall)
    dsp16xx_addhf3_libcall = gen_rtx_SYMBOL_REF (Pmode, ADDHF3_LIBCALL);

   emit_library_call (dsp16xx_addhf3_libcall, 1, HFmode, 2,
		      operands[1], HFmode,
		      operands[2], HFmode);
   emit_move_insn (operands[0], hard_libcall_value(HFmode));
   DONE;
}")
		      

;;
;;  ....................
;;
;;  Subtract instructions
;;
;;  ....................

(define_split
  [(set (match_operand:HI 0 "register_operand" "")
	(minus:HI (match_operand:HI 1 "register_operand" "")
		  (match_operand:HI 2 "const_int_operand" "")))]
  "reload_completed && !ADD_LOW_16(INTVAL(operands[2])) && 
   !ADD_HIGH_16(INTVAL(operands[2]))"
  [(parallel [(set (match_dup 3)
		   (minus:QI (match_dup 4)
			     (match_dup 5)))
	      (clobber (match_dup 6))])

   (parallel [(set (match_dup 6)
		   (minus:QI (match_dup 7)
			     (match_dup 8)))
	      (clobber (match_scratch:QI 9 ""))])]
  "
{
  operands[3] = gen_lowpart(QImode, operands[0]);
  operands[4] = gen_lowpart(QImode, operands[1]);
  operands[5] = gen_rtx (CONST_INT, VOIDmode, INTVAL (operands[2]) & 0xffff);

  operands[6] = gen_highpart(QImode, operands[0]);
  operands[7] = gen_highpart(QImode, operands[0]);
  operands[8] = gen_rtx (CONST_INT, VOIDmode, (((INTVAL (operands[2]) & 0xffff0000) >> 16) & 0xffff));
}")


(define_insn "subhi3"
  [(set (match_operand:HI 0 "register_operand" "=A,A,A,A,A")
	(minus:HI (match_operand:HI 1 "register_operand" "A,A,A,A,A")
		  (match_operand:HI 2 "nonmemory_operand" "Z,d,L,M,?i")))]
  ""
  "@
   %0=%1-%2
   %0=%1-%2
   %0=%w1-%H2
   %0=%b1-%U2
   %0=%w1-%H2\;%0=%b0-%U2"
  [(set_attr "type" "malu,malu,f3_alu_i,f3_alu_i,f3_alu_i")])

(define_insn "subqi3"
  [(set (match_operand:QI 0 "register_operand" "=a,k,u,k,u,!k,!u,j,j,q,q")
	(minus:QI (match_operand:QI 1 "register_operand" "0,uk,uk,uk,uk,uk,uk,0,q,0,j")
		 (match_operand:QI 2 "nonmemory_operand" "N,i,i,wz,wz,uk,uk,i,i,i,i")))
	(clobber (match_scratch:QI 3 "=X,j,q,j,q,j,q,X,k,X,u"))]
  ""
  "*
{
  switch (which_alternative)
    {
    case 0:
      switch (INTVAL (operands[2]))
	{
	case 1:
	  return \"*%0--\";
	  
	case -1:
	  return \"*%0++\";

	default:
	  operands[2] = GEN_INT (-INTVAL (operands[2]));
	  
	  if (SHORT_IMMEDIATE(operands[2]))
	    return \"set %3=%H2\;*%0++%3\";
	  else
	    return \"%3=%H2\;*%0++%3\";
	}
      
    case 1:
    case 2:
        return \"%m0=%1-%H2\";

    case 3:
    case 4:
        return \"%m0=%m1-%m2\";

    case 5:
    case 6:
      return \"%m0=%m1-%m2\";

    case 7: case 8:
    case 9: case 10:
      return \"%0=%b1-%H2\";
    default:
      abort();
    }
}"
[(set_attr "type" "data_move_multiple,f3_alu_i,f3_alu_i,f3_alu,f3_alu,f3_alu,f3_alu,f3_alu_i,f3_alu_i,f3_alu_i,f3_alu_i")])

(define_expand "subhf3"
  [(set (match_operand:HF 0 "register_operand" "")
	(minus:HF (match_operand:HF 1 "register_operand" "")
		 (match_operand:HF 2 "nonmemory_operand" "")))]
  ""
  "
{
  if (!dsp16xx_subhf3_libcall)
    dsp16xx_subhf3_libcall = gen_rtx_SYMBOL_REF (Pmode, SUBHF3_LIBCALL);
  
  emit_library_call (dsp16xx_subhf3_libcall, 1, HFmode, 2,
		     operands[1], HFmode,
		     operands[2], HFmode);
  emit_move_insn (operands[0], hard_libcall_value(HFmode));
  DONE;
}")

(define_insn "neghi2"
  [(set (match_operand:HI 0 "register_operand" "=A")
        (neg:HI (match_operand:HI 1 "register_operand" "A")))]
  ""
  "%0=-%1"
  [(set_attr "type" "special")])

(define_expand "neghf2"
  [(set (match_operand:HF 0 "register_operand" "")
	(neg:HF (match_operand:HF 1 "register_operand" "")))]
  ""
  "
{
  rtx result;
  rtx target;

    {
      target = gen_lowpart(HImode, operands[0]);
      result = expand_binop (HImode, xor_optab,
			     gen_lowpart(HImode, operands[1]),
			     GEN_INT(0x80000000), target, 0, OPTAB_WIDEN);
      if (result == 0)
	abort ();

      if (result != target)
	emit_move_insn (result, target);

      /* Make a place for REG_EQUAL.  */
      emit_move_insn (operands[0], operands[0]);
      DONE;
    }
}")

;;
;;  ....................
;;
;;  Multiply instructions
;;

(define_expand "mulhi3"
  [(set (match_operand:HI 0 "register_operand" "")
	(mult:HI (match_operand:HI 1 "register_operand" "")
		 (match_operand:HI 2 "nonmemory_operand" "")))]
  ""
  "
{
  if (!dsp16xx_mulhi3_libcall)
    dsp16xx_mulhi3_libcall = gen_rtx_SYMBOL_REF (Pmode, MULHI3_LIBCALL);

   emit_library_call (dsp16xx_mulhi3_libcall, 1, HImode, 2,
		      operands[1], HImode,
		      operands[2], HImode);
   emit_move_insn (operands[0], hard_libcall_value(HImode));
   DONE;
}")

(define_insn "mulqi3"
  [(set (match_operand:QI 0 "register_operand" "=w")
        (mult:QI (match_operand:QI 1 "register_operand" "%x")
                 (match_operand:QI 2 "register_operand" "y")))
   (clobber (match_scratch:QI 3 "=v"))]
  ""
  "%m0=%1*%2"
  [(set_attr "type" "malu_mul")])

(define_insn "mulqihi3"
  [(set (match_operand:HI 0 "register_operand" "=t")
        (mult:HI (sign_extend:HI (match_operand:QI 1 "register_operand" "%x"))
                 (sign_extend:HI (match_operand:QI 2 "register_operand" "y"))))]
  ""
  "%0=%1*%2"
  [(set_attr "type" "malu_mul")])

(define_expand "mulhf3"
  [(set (match_operand:HF 0 "register_operand" "")
	(mult:HF (match_operand:HF 1 "register_operand" "")
		 (match_operand:HF 2 "nonmemory_operand" "")))]
  ""
  "
{
  if (!dsp16xx_mulhf3_libcall)
    dsp16xx_mulhf3_libcall = gen_rtx_SYMBOL_REF (Pmode, MULHF3_LIBCALL);
  
  emit_library_call (dsp16xx_mulhf3_libcall, 1, HFmode, 2,
		     operands[1], HFmode,
		     operands[2], HFmode);
  emit_move_insn (operands[0], hard_libcall_value(HFmode));
  DONE;
}")



;;
;; *******************
;;
;; Divide Instructions
;;

(define_expand "divhi3"
  [(set (match_operand:HI 0 "register_operand" "")
	(div:HI (match_operand:HI 1 "register_operand" "")
		 (match_operand:HI 2 "nonmemory_operand" "")))]
  ""
  "
{
  if (!dsp16xx_divhi3_libcall)
    dsp16xx_divhi3_libcall = gen_rtx_SYMBOL_REF (Pmode, DIVHI3_LIBCALL);

   emit_library_call (dsp16xx_divhi3_libcall, 1, HImode, 2,
		      operands[1], HImode,
		      operands[2], HImode);
   emit_move_insn (operands[0], hard_libcall_value(HImode));
   DONE;
}")

(define_expand "udivhi3"
  [(set (match_operand:HI 0 "register_operand" "")
	(udiv:HI (match_operand:HI 1 "register_operand" "")
		 (match_operand:HI 2 "nonmemory_operand" "")))]
  ""
  "
{
  if (!dsp16xx_udivhi3_libcall)
    dsp16xx_udivhi3_libcall = gen_rtx_SYMBOL_REF (Pmode, UDIVHI3_LIBCALL);
  
  emit_library_call (dsp16xx_udivhi3_libcall, 1, HImode, 2,
		     operands[1], HImode,
		     operands[2], HImode);
  emit_move_insn (operands[0], hard_libcall_value(HImode));
  DONE;
}")

(define_expand "divqi3"
  [(set (match_operand:QI 0 "register_operand" "")
	(div:QI (match_operand:QI 1 "register_operand" "")
		 (match_operand:QI 2 "nonmemory_operand" "")))]
  ""
  "
{
  if (!dsp16xx_divqi3_libcall)
    dsp16xx_divqi3_libcall = gen_rtx_SYMBOL_REF (Pmode, DIVQI3_LIBCALL);
  
  emit_library_call (dsp16xx_divqi3_libcall, 1, QImode, 2,
		     operands[1], QImode,
		     operands[2], QImode);
  emit_move_insn (operands[0], hard_libcall_value(QImode));
  DONE;
}")

(define_expand "udivqi3"
  [(set (match_operand:QI 0 "register_operand" "")
	(udiv:QI (match_operand:QI 1 "register_operand" "")
		 (match_operand:QI 2 "nonmemory_operand" "")))]
  ""
  "
{
  if (!dsp16xx_udivqi3_libcall)
    dsp16xx_udivqi3_libcall = gen_rtx_SYMBOL_REF (Pmode, UDIVQI3_LIBCALL);

   emit_library_call (dsp16xx_udivqi3_libcall, 1, QImode, 2,
		      operands[1], QImode,
		      operands[2], QImode);
   emit_move_insn (operands[0], hard_libcall_value(QImode));
   DONE;
}")

;;
;;  ....................
;;
;;  Modulo instructions
;;
;;  ....................

(define_expand "modhi3"
  [(set (match_operand:HI 0 "register_operand" "")
	(mod:HI (match_operand:HI 1 "register_operand" "")
		(match_operand:HI 2 "nonmemory_operand" "")))]
  ""
  "
{
  if (!dsp16xx_modhi3_libcall)
    dsp16xx_modhi3_libcall = gen_rtx_SYMBOL_REF (Pmode, MODHI3_LIBCALL);
  
  emit_library_call (dsp16xx_modhi3_libcall, 1, HImode, 2,
		     operands[1], HImode,
		     operands[2], HImode);
  emit_move_insn (operands[0], hard_libcall_value(HImode));
  DONE;
}")

(define_expand "umodhi3"
  [(set (match_operand:HI 0 "register_operand" "")
	(umod:HI (match_operand:HI 1 "register_operand" "")
		 (match_operand:HI 2 "nonmemory_operand" "")))]
  ""
  "
{
  if (!dsp16xx_umodhi3_libcall)
    dsp16xx_umodhi3_libcall = gen_rtx_SYMBOL_REF (Pmode, UMODHI3_LIBCALL);
  
  emit_library_call (dsp16xx_umodhi3_libcall, 1, HImode, 2,
		     operands[1], HImode,
		     operands[2], HImode);
  emit_move_insn (operands[0], hard_libcall_value(HImode));
  DONE;
}")

(define_expand "modqi3"
  [(set (match_operand:QI 0 "register_operand" "")
	(mod:QI (match_operand:QI 1 "register_operand" "")
		(match_operand:QI 2 "nonmemory_operand" "")))]
  ""
  "
{
  if (!dsp16xx_modqi3_libcall)
    dsp16xx_modqi3_libcall = gen_rtx_SYMBOL_REF (Pmode, MODQI3_LIBCALL);
  
  emit_library_call (dsp16xx_modqi3_libcall, 1, QImode, 2,
		     operands[1], QImode,
		     operands[2], QImode);
  emit_move_insn (operands[0], hard_libcall_value(QImode));
  DONE;
}")

(define_expand "umodqi3"
  [(set (match_operand:QI 0 "register_operand" "")
	(umod:QI (match_operand:QI 1 "register_operand" "")
		 (match_operand:QI 2 "nonmemory_operand" "")))]
  ""
  "
{
  if (!dsp16xx_umodqi3_libcall)
    dsp16xx_umodqi3_libcall = gen_rtx_SYMBOL_REF (Pmode, UMODQI3_LIBCALL);
  
  emit_library_call (dsp16xx_umodqi3_libcall, 1, QImode, 2,
		     operands[1], QImode,
		     operands[2], QImode);
  emit_move_insn (operands[0], hard_libcall_value(QImode));
  DONE;
}")

(define_expand "divhf3"
  [(set (match_operand:HF 0 "register_operand" "")
	(div:HF (match_operand:HF 1 "register_operand" "")
		(match_operand:HF 2 "nonmemory_operand" "")))]
  ""
  "
{
  if (!dsp16xx_divhf3_libcall)
    dsp16xx_divhf3_libcall = gen_rtx_SYMBOL_REF (Pmode, DIVHF3_LIBCALL);
  
  emit_library_call (dsp16xx_divhf3_libcall, 1, HFmode, 2,
		     operands[1], HFmode,
		     operands[2], HFmode);
  emit_move_insn (operands[0], hard_libcall_value(HFmode));
  DONE;
}")



;;
;; ********************
;;
;; Logical Instructions
;;

(define_split
  [(set (match_operand:HI 0 "register_operand" "")
	(and:HI (match_operand:HI 1 "register_operand" "")
		 (match_operand:HI 2 "const_int_operand" "")))]
  "reload_completed && !AND_LOW_16(INTVAL(operands[2])) && 
   !AND_HIGH_16(INTVAL(operands[2])) 
   && (REGNO (operands[0]) == REGNO (operands[1]))"
  [(parallel [(set (match_dup 3)
		   (and:QI (match_dup 4)
			   (match_dup 5)))
	      (clobber (match_scratch:QI 6 ""))])
   (parallel [(set (match_dup 7)
		   (and:QI (match_dup 8)
			   (match_dup 9)))
	      (clobber (match_scratch:QI 10 ""))])]
  "
{
  operands[3] = gen_lowpart(QImode, operands[0]);
  operands[4] = gen_lowpart(QImode, operands[1]);
  operands[5] = gen_rtx (CONST_INT, VOIDmode, INTVAL (operands[2]) & 0xffff);

  operands[7] = gen_highpart(QImode, operands[0]);
  operands[8] = gen_highpart(QImode, operands[0]);
  operands[9] = gen_rtx (CONST_INT, VOIDmode, (((INTVAL (operands[2]) & 0xffff0000) >> 16) & 0xffff));
}")

(define_split
  [(set (match_operand:HI 0 "register_operand" "")
	(and:HI (match_operand:HI 1 "register_operand" "")
		(match_operand:HI 2 "const_int_operand" "")))]
  "reload_completed && !AND_LOW_16(INTVAL(operands[2])) && 
   !AND_HIGH_16(INTVAL(operands[2]))
   && (REGNO (operands[0]) != REGNO (operands[1]))"
  [(parallel [(set (match_dup 3)
		   (and:QI (match_dup 4)
			   (match_dup 5)))
	      (clobber (match_dup 6))])
   (parallel [(set (match_dup 6)
		   (and:QI (match_dup 7)
			   (match_dup 8)))
	      (clobber (match_scratch:QI 9 ""))])]
  "
{
  operands[3] = gen_lowpart(QImode, operands[0]);
  operands[4] = gen_lowpart(QImode, operands[1]);
  operands[5] = gen_rtx (CONST_INT, VOIDmode, INTVAL (operands[2]) & 0xffff);

  operands[6] = gen_highpart(QImode, operands[0]);
  operands[7] = gen_highpart(QImode, operands[0]);
  operands[8] = gen_rtx (CONST_INT, VOIDmode, (((INTVAL (operands[2]) & 0xffff0000) >> 16) & 0xffff));
}")

(define_insn "andhi3"
  [(set (match_operand:HI 0 "register_operand" "=A,A,A,A,?A")
        (and:HI (match_operand:HI 1 "register_operand" "%A,!A,A,A,A")
                (match_operand:HI 2 "nonmemory_operand" "Z,A,O,P,i")))]
  ""
  "@
   %0=%1&%2
   %0=%1&%2
   %0=%w1&%H2
   %0=%b1&%U2
   %0=%w1&%H2\;%0=%b0&%U2"
  [(set_attr "type" "f3_alu,f3_alu,f3_alu_i,f3_alu_i,f3_alu_i_mult")])

(define_insn "andqi3"
  [(set (match_operand:QI 0 "register_operand" "=k,u,!k,!u,k,u,k,u,j,q,j,q,j,q,!j,!q")
	(and:QI (match_operand:QI 1 "register_operand" "%uk,uk,uk,uk,0,0,u,k,jq,jq,0,0,q,j,jq,jq")
		(match_operand:QI 2 "nonmemory_operand" "wz,wz,uk,uk,i,i,i,i,yv,yv,i,i,i,i,jq,jq")))
   (clobber (match_scratch:QI 3 "=j,q,j,q,X,X,j,q,k,u,X,X,k,u,k,u"))]
  ""
  "@
    %m0=%m1&%m2
    %m0=%m1&%m2
    %m0=%m1&%m2
    %m0=%m1&%m2
    %m0=%1&%H2
    %m0=%1&%H2
    %m0=%1&%H2
    %m0=%1&%H2
    %m0=%m1&%m2
    %m0=%m1&%m2
    %m0=%b1&%H2
    %m0=%b1&%H2
    %m0=%b1&%H2
    %m0=%b1&%H2
    %m0=%m1&%m2
    %m0=%m1&%m2"
  [(set_attr "type" "f3_alu,f3_alu,f3_alu,f3_alu,f3_alu_i,f3_alu_i,f3_alu_i,f3_alu_i,f3_alu,f3_alu,f3_alu_i,f3_alu_i,f3_alu_i,f3_alu_i,f3_alu,f3_alu")])


(define_split
  [(set (match_operand:HI 0 "register_operand" "")
	(ior:HI (match_operand:HI 1 "register_operand" "")
		(match_operand:HI 2 "const_int_operand" "")))]
  "reload_completed && !SMALL_INTVAL(INTVAL(operands[2])) && 
   !ADD_HIGH_16(INTVAL(operands[2]))
   && (REGNO (operands[0]) == REGNO (operands[1]))"
  [(parallel [(set (match_dup 3)
		   (ior:QI (match_dup 4)
			   (match_dup 5)))
	      (clobber (match_scratch:QI 6 ""))])
   (parallel [(set (match_dup 7)
		   (ior:QI (match_dup 8)
			   (match_dup 9)))
	      (clobber (match_scratch:QI 10 ""))])]
  "
{
  operands[3] = gen_lowpart(QImode, operands[0]);
  operands[4] = gen_lowpart(QImode, operands[1]);
  operands[5] = gen_rtx (CONST_INT, VOIDmode, INTVAL (operands[2]) & 0xffff);

  operands[7] = gen_highpart(QImode, operands[0]);
  operands[8] = gen_highpart(QImode, operands[0]);
  operands[9] = gen_rtx (CONST_INT, VOIDmode, (((INTVAL (operands[2]) & 0xffff0000) >> 16) & 0xffff));
}")

(define_split
  [(set (match_operand:HI 0 "register_operand" "")
	(ior:HI (match_operand:HI 1 "register_operand" "")
		(match_operand:HI 2 "const_int_operand" "")))]
  "reload_completed && !SMALL_INTVAL(INTVAL(operands[2])) && 
   !ADD_HIGH_16(INTVAL(operands[2]))
   && (REGNO (operands[0]) != REGNO (operands[1]))"
  [(parallel [(set (match_dup 3)
		   (ior:QI (match_dup 4)
			   (match_dup 5)))
	      (clobber (match_dup 6))])
   (parallel [(set (match_dup 6)
		   (ior:QI (match_dup 7)
			   (match_dup 8)))
	      (clobber (match_scratch:QI 9 ""))])]
  "
{
  operands[3] = gen_lowpart(QImode, operands[0]);
  operands[4] = gen_lowpart(QImode, operands[1]);
  operands[5] = gen_rtx (CONST_INT, VOIDmode, INTVAL (operands[2]) & 0xffff);

  operands[6] = gen_highpart(QImode, operands[0]);
  operands[7] = gen_highpart(QImode, operands[0]);
  operands[8] = gen_rtx (CONST_INT, VOIDmode, (((INTVAL (operands[2]) & 0xffff0000) >> 16) & 0xffff));
}")


(define_insn "iorhi3"
  [(set (match_operand:HI 0 "register_operand" "=A,A,A,A,?A")
        (ior:HI (match_operand:HI 1 "register_operand" "%A,!A,A,A,A")
                (match_operand:HI 2 "nonmemory_operand" "Z,A,I,M,i")))]
 ""
 "@
   %0=%u1|%u2
   %0=%u1|%u2
   %0=%w1|%H2
   %0=%b1|%U2
   %0=%w1|%H2\;%0=%b0|%U2"
  [(set_attr "type" "f3_alu,f3_alu,f3_alu_i,f3_alu_i,f3_alu_i_mult")])

(define_insn "iorqi3"
  [(set (match_operand:QI 0 "register_operand" "=k,u,!k,!u,k,u,k,u,j,q,j,q,j,q,!j,!q")
	(ior:QI (match_operand:QI 1 "register_operand" "%uk,uk,uk,uk,0,0,u,k,jq,jq,0,0,q,j,jq,jq")
		(match_operand:QI 2 "nonmemory_operand" "wz,wz,uk,uk,i,i,i,i,yv,yv,i,i,i,i,jq,jq")))
   (clobber (match_scratch:QI 3 "=j,q,j,q,X,X,j,q,k,u,X,X,k,u,k,u"))]
  ""
  "@
    %m0=%m1|%m2
    %m0=%m1|%m2
    %m0=%m1|%m2
    %m0=%m1|%m2
    %m0=%1|%H2
    %m0=%1|%H2
    %m0=%1|%H2
    %m0=%1|%H2
    %m0=%m1|%m2
    %m0=%m1|%m2
    %m0=%b1|%H2
    %m0=%b1|%H2
    %m0=%b1|%H2
    %m0=%b1|%H2
    %m0=%m1|%m2
    %m0=%m1|%m2"
  [(set_attr "type" "f3_alu,f3_alu,f3_alu,f3_alu,f3_alu_i,f3_alu_i,f3_alu_i,f3_alu_i,f3_alu,f3_alu,f3_alu_i,f3_alu_i,f3_alu_i,f3_alu_i,f3_alu,f3_alu")])



(define_split
  [(set (match_operand:HI 0 "register_operand" "")
	(xor:HI (match_operand:HI 1 "register_operand" "")
		(match_operand:HI 2 "const_int_operand" "")))]
  "reload_completed && !SMALL_INTVAL(INTVAL(operands[2])) && 
   !ADD_HIGH_16(INTVAL(operands[2]))
   && (REGNO (operands[0]) == REGNO (operands[1]))"
  [(parallel [(set (match_dup 3)
		   (xor:QI (match_dup 4)
			   (match_dup 5)))
	      (clobber (match_scratch:QI 6 ""))])
   (parallel [(set (match_dup 7)
		   (xor:QI (match_dup 8)
			   (match_dup 9)))
	      (clobber (match_scratch:QI 10 ""))])]
  "
{
  operands[3] = gen_lowpart(QImode, operands[0]);
  operands[4] = gen_lowpart(QImode, operands[1]);
  operands[5] = gen_rtx (CONST_INT, VOIDmode, INTVAL (operands[2]) & 0xffff);

  operands[7] = gen_highpart(QImode, operands[0]);
  operands[8] = gen_highpart(QImode, operands[0]);
  operands[9] = gen_rtx (CONST_INT, VOIDmode, (((INTVAL (operands[2]) & 0xffff0000) >> 16) & 0xffff));
}")

(define_split
  [(set (match_operand:HI 0 "register_operand" "")
	(xor:HI (match_operand:HI 1 "register_operand" "")
		(match_operand:HI 2 "const_int_operand" "")))]
  "reload_completed && !SMALL_INTVAL(INTVAL(operands[2])) && 
   !ADD_HIGH_16(INTVAL(operands[2]))
   && (REGNO (operands[0]) != REGNO (operands[1]))"
  [(parallel [(set (match_dup 3)
		   (xor:QI (match_dup 4)
			   (match_dup 5)))
	      (clobber (match_dup 6))])
   (parallel [(set (match_dup 6)
		   (xor:QI (match_dup 7)
			   (match_dup 8)))
	      (clobber (match_scratch:QI 9 ""))])]
  "
{
  operands[3] = gen_lowpart(QImode, operands[0]);
  operands[4] = gen_lowpart(QImode, operands[1]);
  operands[5] = gen_rtx (CONST_INT, VOIDmode, INTVAL (operands[2]) & 0xffff);

  operands[6] = gen_highpart(QImode, operands[0]);
  operands[7] = gen_highpart(QImode, operands[0]);
  operands[8] = gen_rtx (CONST_INT, VOIDmode, (((INTVAL (operands[2]) & 0xffff0000) >> 16) & 0xffff));
}")

(define_insn "xorhi3"
  [(set (match_operand:HI 0 "register_operand" "=A,A,A,A,?A")
        (xor:HI (match_operand:HI 1 "register_operand" "%A,!A,A,A,A")
                (match_operand:HI 2 "nonmemory_operand" "Z,A,I,M,i")))]
  ""
  "@
   %0=%1^%2
   %0=%1^%2
   %0=%w1^%H2
   %0=%b1^%U2
   %0=%w1^%H2\;%0=%b0^%U2"
  [(set_attr "type" "f3_alu,f3_alu,f3_alu_i,f3_alu_i,f3_alu_i_mult")])

(define_insn "xorqi3"
  [(set (match_operand:QI 0 "register_operand" "=k,u,!k,!u,k,u,k,u,j,q,j,q,j,q,!j,!q")
	(xor:QI (match_operand:QI 1 "register_operand" "%uk,uk,uk,uk,0,0,u,k,jq,jq,0,0,q,j,jq,jq")
		(match_operand:QI 2 "nonmemory_operand" "wz,wz,uk,uk,i,i,i,i,yv,yv,i,i,i,i,jq,jq")))
   (clobber (match_scratch:QI 3 "=j,q,j,q,X,X,j,q,k,u,X,X,k,u,k,u"))]
  ""
  "@
    %m0=%m1^%m2
    %m0=%m1^%m2
    %m0=%m1^%m2
    %m0=%m1^%m2
    %m0=%1^%H2
    %m0=%1^%H2
    %m0=%1^%H2
    %m0=%1^%H2
    %m0=%m1^%m2
    %m0=%m1^%m2
    %m0=%b1^%H2
    %m0=%b1^%H2
    %m0=%b1^%H2
    %m0=%b1^%H2
    %m0=%m1^%m2
    %m0=%m1^%m2"
  [(set_attr "type" "f3_alu,f3_alu,f3_alu,f3_alu,f3_alu_i,f3_alu_i,f3_alu_i,f3_alu_i,f3_alu,f3_alu,f3_alu_i,f3_alu_i,f3_alu_i,f3_alu_i,f3_alu,f3_alu")])

(define_insn "one_cmplhi2"
  [(set (match_operand:HI 0 "register_operand" "=A")
        (not:HI (match_operand:HI 1 "register_operand" "A")))]
  ""
  "%0= ~%1"
  [(set_attr "type" "special")])


(define_insn "one_cmplqi2"
  [(set (match_operand:QI 0 "register_operand" "=k,k,u,u,j,j,q,q")
        (not:QI (match_operand:QI 1 "register_operand" "0,u,0,q,0,q,0,j")))
   (clobber (match_scratch:QI 2 "=X,j,X,q,X,k,X,u"))]
  ""
  "@
   %m0= %1 ^ 0xffff
   %m0= %1 ^ 0xffff
   %m0= %1 ^ 0xffff
   %m0= %1 ^ 0xffff
   %m0= %b1 ^ 0xffff
   %m0= %b1 ^ 0xffff
   %m0= %b1 ^ 0xffff
   %m0= %b1 ^ 0xffff"
  [(set_attr "type" "f3_alu_i,f3_alu_i,f3_alu_i,f3_alu_i,f3_alu_i,f3_alu_i,f3_alu_i,f3_alu_i")])


;;
;; MOVE INSTRUCTIONS
;;

(define_split
  [(set (mem:HI (match_operand:QI 0 "register_operand" ""))
	(match_operand:HI 1 "register_operand" ""))]
  "reload_completed && (operands[0] != stack_pointer_rtx)"
  [(set (mem:QI (post_inc:QI (match_dup 0)))
	(match_dup 2))
   (set (mem:QI (post_dec:QI (match_dup 0)))
	(match_dup 3))]
  "
{
  operands[2] = gen_highpart(QImode, operands[1]);
  operands[3] = gen_lowpart(QImode, operands[1]);
}")


(define_split
  [(set (match_operand:HI 0 "register_operand" "")
	(mem:HI (match_operand:QI 1 "register_operand" "")))]
  "reload_completed && (operands[1] != stack_pointer_rtx)"
  [(set (match_dup 2)
	(mem:QI (post_inc:QI (match_dup 1))))
   (set (match_dup 3)
	(mem:QI (post_dec:QI (match_dup 1))))]
  "
{
  operands[2] = gen_highpart(QImode, operands[0]);
  operands[3] = gen_lowpart(QImode, operands[0]);
}")

(define_split
  [(set (mem:HI (post_inc:HI (match_operand:QI 0 "register_operand" "")))
	(match_operand:HI 1 "register_operand" ""))]
  "reload_completed"
  [(set (mem:QI (post_inc:QI (match_dup 0)))
	(match_dup 2))
   (set (mem:QI (post_inc:QI (match_dup 0)))
	(match_dup 3))]
  "
{
  operands[2] = gen_highpart(QImode, operands[1]);
  operands[3] = gen_lowpart(QImode, operands[1]);
}")

(define_split
  [(set (match_operand:HI 0 "register_operand" "")
	(mem:HI (post_inc:HI (match_operand:QI 1 "register_operand" ""))))]
  "reload_completed"
  [(set (match_dup 2)
	(mem:QI (post_inc:QI (match_dup 1))))
   (set (match_dup 3)
	(mem:QI (post_inc:QI (match_dup 1))))]
  "
{
  operands[2] = gen_highpart(QImode, operands[0]);
  operands[3] = gen_lowpart(QImode, operands[0]);
}")


(define_split
  [(set (match_operand:HI 0 "register_operand" "")
	(match_operand:HI 1 "register_operand" ""))]
  "reload_completed && 
   !(IS_ACCUM_REG (REGNO(operands[0])) && 
    (REGNO(operands[1]) == REG_PROD || REGNO(operands[1]) == REG_Y))"
  [(set (match_dup 2)
	(match_dup 3))
   (set (match_dup 4)
	(match_dup 5))]
  "
{
  operands[2] = gen_highpart(QImode, operands[0]);
  operands[3] = gen_highpart(QImode, operands[1]);
  operands[4] = gen_lowpart(QImode, operands[0]);
  operands[5] = gen_lowpart(QImode, operands[1]);
}")

(define_split
  [(set (match_operand:HI 0 "register_operand" "")
	(match_operand:HI 1 "const_int_operand" ""))]
  "reload_completed"
  [(set (match_dup 2)
	(match_dup 3))
   (set (match_dup 4)
	(match_dup 5))]
   "
{
  operands[2] = gen_lowpart(QImode, operands[0]);
  operands[3] = gen_rtx (CONST_INT, VOIDmode, INTVAL (operands[1]) & 0xffff);

  operands[4] = gen_highpart(QImode, operands[0]);
  operands[5] = gen_rtx (CONST_INT, VOIDmode, (((INTVAL (operands[1]) & 0xffff0000) >> 16) & 0xffff));
}")

(define_expand "movhi"
  [(set (match_operand:HI 0 "general_operand" "")
	(match_operand:HI 1 "general_operand" ""))]
  ""
  "
{
  if (emit_move_sequence (operands, HImode))
    DONE;
}")


(define_insn "match_movhi1"
  [(set (match_operand:HI 0 "nonimmediate_operand"  "=A,Z,A,d,d,m,?d,*Y,t,f")
	(match_operand:HI 1 "general_operand"        "d,A,K,i,m,d,*Y,?d,t,f"))]
  "register_operand(operands[0], HImode)
   || register_operand(operands[1], HImode)"
  "*
{
	switch (which_alternative)
        {
		/* register to accumulator */
		case 0:
 		   return \"%0=%1\";
		case 1:
		   return \"%u0=%u1\;%w0=%w1\";
	        case 2:
	           return \"%0=%0^%0\";
		case 3:
		     return \"%u0=%U1\;%w0=%H1\";
		case 4:
		   double_reg_from_memory(operands);
		   return \"\";
		case 5:
		   double_reg_to_memory(operands);
		   return \"\";
		case 6:
		case 7:
		   return \"%u0=%u1\;%w0=%w1\";
		case 8:
		case 9:
		   return \"\";
                default:
                  abort();
        }
}"
[(set_attr "type" "special,data_move_multiple,f3_alu,data_move_multiple,data_move_multiple,data_move_multiple,data_move_multiple,data_move_multiple,nothing,nothing")])


;; NOTE: It is cheaper to do 'y = *r0', than 'r0 = *r0'.

(define_expand "movqi"
  [(set (match_operand:QI 0 "nonimmediate_operand" "")
	(match_operand:QI 1 "general_operand" ""))]
  ""
  "
{
  if (emit_move_sequence (operands, QImode))
    DONE;
}")

;; The movqi pattern with the parallel is used for addqi insns (which have a parallel)
;; that are turned into moveqi insns by the flow phase. This happens when an auto-increment
;; is detected.

(define_insn "match_movqi1"
  [(parallel [(set (match_operand:QI 0 "nonimmediate_operand" "=A,r,aW,c,?D,m<>,e,Y,r,xyz,m<>")
		   (match_operand:QI 1 "general_operand"       "r,A,J,i,m<>,D,Y,e,0,m<>,xyz"))
	      (clobber (match_scratch:QI 2 "=X,X,X,X,X,X,X,X,X,X,X"))])]
  "register_operand(operands[0], QImode)
   || register_operand(operands[1], QImode)"
  "*
{
	switch (which_alternative)
	{
		case 0:
		   /* We have to use the move mneumonic otherwise the 1610 will
		      attempt to transfer all 32-bits of 'y', 'p' or an accumulator
		      , which we don't want */
		   if (REGNO(operands[1]) == REG_Y || REGNO(operands[1]) == REG_PROD
			|| IS_ACCUM_REG(REGNO(operands[1])))
		       return \"move %0=%1\";
		   else
		       return \"%0=%1\";

		case 1:
 		   return \"%0=%1\";

		case 2:
		   return \"set %0=%H1\";

		case 3:
                   return \"%0=%H1\";

		case 4:
		   return \"%0=%1\";

		case 5:
		case 6:
                   return \"%0=%1\";

		case 7:
		   return \"%0=%1\";

		case 8:
		   return \"\";

                case 9: case 10:
		   return \"%0=%1\";
                default:
                  abort();
	}
}"
[(set_attr "type" "data_move,data_move,data_move_short_i,data_move_i,data_move_memory,data_move_memory,data_move_memory,data_move_memory,nothing,malu,malu")])

(define_insn "match_movqi2"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=A,r,aW,c,?D,m<>,e,Y,r,xyz,m<>")
	(match_operand:QI 1 "general_operand"       "r,A,J,i,m<>,D,Y,e,0,m<>,xyz"))]
  "register_operand(operands[0], QImode)
   || register_operand(operands[1], QImode)"
  "*
{
	switch (which_alternative)
	{
		case 0:
		   /* We have to use the move mneumonic otherwise the 1610 will
		      attempt to transfer all 32-bits of 'y', 'p' or an accumulator
		      , which we don't want */
		   if (REGNO(operands[1]) == REG_Y || REGNO(operands[1]) == REG_PROD
			|| IS_ACCUM_REG(REGNO(operands[1])))
		       return \"move %0=%1\";
		   else
		       return \"%0=%1\";

		case 1:
 		   return \"%0=%1\";

		case 2:
		   return \"set %0=%H1\";

		case 3:
                   return \"%0=%H1\";

		case 4:
		   return \"%0=%1\";

		case 5:
		case 6:
                   return \"%0=%1\";

		case 7:
		   return \"%0=%1\";

		case 8:
		   return \"\";

                case 9: case 10:
		   return \"%0=%1\";
                default:
                  abort();
	}
}"
[(set_attr "type" "data_move,data_move,data_move_short_i,data_move_i,data_move_memory,data_move_memory,data_move_memory,data_move_memory,nothing,malu,malu")])

(define_expand "reload_inqi"
  [(set (match_operand:QI 0 "register_operand" "=u")
        (match_operand:QI 1 "sp_operand" ""))
   (clobber (match_operand:QI 2 "register_operand" "=&q"))]
  ""
  "
{
  rtx addr_reg = XEXP (operands[1], 0);
  rtx offset = XEXP (operands[1], 1);

  /* First, move the frame or stack pointer to the accumulator */
  emit_move_insn (operands[0], addr_reg);

  /* Then generate the add insn */
  emit_insn (gen_rtx_PARALLEL
	     (VOIDmode, 
	      gen_rtvec (2,
			 gen_rtx_SET (VOIDmode, operands[0], 
				      gen_rtx_PLUS (QImode, operands[0],
						    offset)),
			 gen_rtx_CLOBBER (VOIDmode, operands[2]))));
  DONE;
}")

(define_expand "reload_inhi"
  [(set (match_operand:HI 0 "register_operand" "=r")
        (match_operand:HI 1 "register_operand" "r"))
   (clobber (match_operand:QI 2 "register_operand" "=&h"))]
  ""
  "
{
  /* Check for an overlap of operand 2 (an accumulator) with
     the msw of operand 0. If we have an overlap we must reverse
     the order of the moves.  */

  if (REGNO(operands[2]) == REGNO(operands[0]))
    {
      emit_move_insn (operands[2], operand_subword (operands[1], 1, 0, HImode));
      emit_move_insn (operand_subword (operands[0], 1, 0, HImode), operands[2]);
      emit_move_insn (operands[2], operand_subword (operands[1], 0, 0, HImode));
      emit_move_insn (operand_subword (operands[0], 0, 0, HImode), operands[2]);
    }
  else
    {
      emit_move_insn (operands[2], operand_subword (operands[1], 0, 0, HImode));
      emit_move_insn (operand_subword (operands[0], 0, 0, HImode), operands[2]);
      emit_move_insn (operands[2], operand_subword (operands[1], 1, 0, HImode));
      emit_move_insn (operand_subword (operands[0], 1, 0, HImode), operands[2]);
    }

  DONE;
}")


(define_expand "reload_outhi"
  [(set (match_operand:HI 0 "register_operand" "=r")
        (match_operand:HI 1 "register_operand" "r"))
   (clobber (match_operand:QI 2 "register_operand" "=&h"))]
  ""
  "
{
	emit_move_insn (operands[2], operand_subword (operands[1], 0, 0, HImode));
	emit_move_insn (operand_subword (operands[0], 0, 0, HImode), operands[2]);
	emit_move_insn (operands[2], operand_subword (operands[1], 1, 0, HImode));
	emit_move_insn (operand_subword (operands[0], 1, 0, HImode), operands[2]);
	DONE;
}")

(define_expand "movstrqi"
  [(parallel [(set (match_operand:BLK 0 "memory_operand" "")
		   (match_operand:BLK 1 "memory_operand" ""))
	      (use (match_operand:QI 2 "const_int_operand" ""))
	      (use (match_operand:QI 3 "const_int_operand" ""))
	      (clobber (match_scratch:QI 4 ""))
	      (clobber (match_dup 5))
	      (clobber (match_dup 6))])]
  ""
  "
{
  rtx addr0, addr1;

  if (GET_CODE (operands[2]) != CONST_INT)
    FAIL;

  if (INTVAL(operands[2]) > 127)
    FAIL;

  addr0 = copy_to_mode_reg (Pmode, XEXP (operands[0], 0));
  addr1 = copy_to_mode_reg (Pmode, XEXP (operands[1], 0));

  operands[5] = addr0;
  operands[6] = addr1;

  operands[0] = change_address (operands[0], VOIDmode, addr0);
  operands[1] = change_address (operands[1], VOIDmode, addr1);
}")

(define_insn ""
  [(set (mem:BLK (match_operand:QI 0 "register_operand" "a"))
	(mem:BLK (match_operand:QI 1 "register_operand" "a")))
   (use (match_operand:QI 2 "const_int_operand" "n"))
   (use (match_operand:QI 3 "immediate_operand" "i"))
   (clobber (match_scratch:QI 4 "=x"))
   (clobber (match_dup 0))
   (clobber (match_dup 1))]
  ""
  "*
{ return output_block_move (operands); }")


;; Floating point move insns


(define_expand "movhf"
  [(set (match_operand:HF 0 "general_operand" "")
	(match_operand:HF 1 "general_operand" ""))]
  ""
  "
{
  if (emit_move_sequence (operands, HFmode))
    DONE;
}")

(define_insn "match_movhf"
  [(set (match_operand:HF 0 "nonimmediate_operand" "=A,Z,d,d,m,d,Y")
	(match_operand:HF 1 "general_operand"       "d,A,F,m,d,Y,d"))]
  ""
  "*
{
	/* NOTE: When loading the register 16 bits at a time we
	   MUST load the high half FIRST (because the 1610 zeros
	   the low half) and then load the low half */

	switch (which_alternative)
        {
		/* register to accumulator */
		case 0:
 		   return \"%0=%1\";
		case 1:
		   return \"%u0=%u1\;%w0=%w1\";
		case 2:
		   output_dsp16xx_float_const(operands);
		   return \"\";
		case 3:
		   double_reg_from_memory(operands);
		   return \"\";
		case 4:
		   double_reg_to_memory(operands);
		   return \"\";
		case 5:
		case 6:
		   return \"%u0=%u1\;%w0=%w1\";
                default:
                  abort();
        }
}"
[(set_attr "type" "move,move,load_i,load,store,load,store")])



(define_expand "reload_inhf"
  [(set (match_operand:HF 0 "register_operand" "=r")
        (match_operand:HF 1 "register_operand" "r"))
   (clobber (match_operand:QI 2 "register_operand" "=&h"))]
  ""
  "
{
  /* Check for an overlap of operand 2 (an accumulator) with
     the msw of operand 0. If we have an overlap we must reverse
     the order of the moves.  */

  if (REGNO(operands[2]) == REGNO(operands[0]))
    {
      emit_move_insn (operands[2], operand_subword (operands[1], 1, 0, HFmode));
      emit_move_insn (operand_subword (operands[0], 1, 0, HFmode), operands[2]);
      emit_move_insn (operands[2], operand_subword (operands[1], 0, 0, HFmode));
      emit_move_insn (operand_subword (operands[0], 0, 0, HFmode), operands[2]);
    }
  else
    {
      emit_move_insn (operands[2], operand_subword (operands[1], 0, 0, HFmode));
      emit_move_insn (operand_subword (operands[0], 0, 0, HFmode), operands[2]);
      emit_move_insn (operands[2], operand_subword (operands[1], 1, 0, HFmode));
      emit_move_insn (operand_subword (operands[0], 1, 0, HFmode), operands[2]);
    }
  
  DONE;
}")

(define_expand "reload_outhf"
  [(set (match_operand:HF 0 "register_operand" "=r")
        (match_operand:HF 1 "register_operand" "r"))
   (clobber (match_operand:QI 2 "register_operand" "=&h"))]
  ""
  "
{
	emit_move_insn (operands[2], operand_subword (operands[1], 0, 0, HFmode));
	emit_move_insn (operand_subword (operands[0], 0, 0, HFmode), operands[2]);
	emit_move_insn (operands[2], operand_subword (operands[1], 1, 0, HFmode));
	emit_move_insn (operand_subword (operands[0], 1, 0, HFmode), operands[2]);
	DONE;
}")


;;
;; CONVERSION INSTRUCTIONS
;;

(define_expand "extendqihi2"
  [(clobber (match_dup 2))
   (set (match_dup 3) (match_operand:QI 1 "register_operand" ""))
   (set (match_operand:HI 0 "register_operand" "")
	(ashift:HI (match_dup 2)
		   (const_int 16)))
   (set (match_dup 0)
	(ashiftrt:HI (match_dup 0) (const_int 16)))]
  ""
  "
{
	operands[2] = gen_reg_rtx (HImode);
	operands[3] = gen_rtx_SUBREG (QImode, operands[2], 1);
}")

(define_insn "internal_extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "=A")
	(sign_extend:HI (match_operand:QI 1 "register_operand" "ku")))]
  "TARGET_BMU"
  "%0 = extracts(%m1, 0x1000)"
[(set_attr "type" "shift_i")])

;;(define_insn "extendqihi2"
;;  [(set (match_operand:HI 0 "register_operand" "=A")
;;        (sign_extend:HI (match_operand:QI 1 "register_operand" "h")))]
;;  ""
;;  "%0 = %1 >> 16")

;;(define_insn "zero_extendqihi2"
;;  [(set (match_operand:HI 0 "register_operand" "=t,f,A,?d,?A")
;;        (zero_extend:HI (match_operand:QI 1 "register_operand" "w,z,ku,A,r")))]
;;  ""
;;  "*
;; {
;;  switch (which_alternative)
;;    {
;;    case 0:
;;    case 1:
;;      return \"%0=0\";
;;
;;    case 2:
;;      if (REGNO(operands[1]) == (REGNO(operands[0]) + 1))
;;        return \"%0=0\";
;;      else
;;	return \"%w0=%1\;%0=0\";
;;    case 3:
;;      return \"%w0=%1\;%0=0\";
;;
;;    case 4:
;;      if (REGNO(operands[1]) == REG_Y || REGNO(operands[1]) == REG_PROD
;;	  || IS_ACCUM_REG(REGNO(operands[1])))
;;	return \"move %w0=%1\;%0=0\";
;;      else
;;	return \"%w0=%1\;%0=0\";
;;    default:
;;      abort();
;;    }
;; }")

;;(define_expand "zero_extendqihi2"
;;  [(clobber (match_dup 2))
;;   (set (match_dup 3) (match_operand:QI 1 "register_operand" ""))
;;   (set (match_operand:HI 0 "register_operand" "")
;;	(ashift:HI (match_dup 2)
;;		   (const_int 16)))
;;   (set (match_dup 0)
;;	(lshiftrt:HI (match_dup 0) (const_int 16)))]
;;  ""
;;  "
;;{
;;	operands[2] = gen_reg_rtx (HImode);
;;	operands[3] = gen_rtx (SUBREG, QImode, operands[2], 1);
;;}")

(define_expand "zero_extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "")
      (zero_extend:HI (match_operand:QI 1 "register_operand" "")))]
  ""
  "")  
	

(define_insn "match_zero_extendqihi_bmu"
  [(set (match_operand:HI 0 "register_operand" "=?*Z,?*Z,?A,A")
        (zero_extend:HI (match_operand:QI 1 "register_operand" "?A,?*Y,*Z*x*a*W*Y,ku")))]
  "TARGET_BMU"
  "*
 {
    switch (which_alternative)
      {
        case 0:
          return \"%w0=%1\;%0=0\";

        case 1:
          return \"%w0=%1\;%0=0\";

        case 2:
          if (REGNO(operands[1]) == (REGNO(operands[0]) + 1))
            return \"%0=0\";
          else if (REGNO(operands[1]) == REG_Y || REGNO(operands[1]) == REG_PROD
	           || IS_ACCUM_REG(REGNO(operands[1])))
          {
            return \"move %w0=%1\;%0=0\";
          }
          else
            return \"%w0=%1\;%0=0\";

	case 3:
	  return \"%0 = extractz(%m1, 0x1000)\";
        default:
          abort();
      }
  }"
  [(set_attr "type" "data_move_2,data_move_2,data_move_2,shift_i")])

(define_insn "match_zero_extendqihi2_nobmu"
  [(set (match_operand:HI 0 "register_operand" "=?Z,?Z,A")
        (zero_extend:HI (match_operand:QI 1 "register_operand" "A,Y,r")))]
  ""
  "*
 {
    switch (which_alternative)
      {
        case 0:
          return \"%w0=%1\;%0=0\";

        case 1:
          return \"%w0=%1\;%0=0\";

        case 2:
          if (REGNO(operands[1]) + 1 == (REGNO(operands[0]) + 1))
            return \"%0=0\";
          else if (REGNO(operands[1]) == REG_Y || REGNO(operands[1]) == REG_PROD
	           || IS_ACCUM_REG(REGNO(operands[1])))
          {
            return \"move %w0=%1\;%0=0\";
          }
          else
            return \"%w0=%1\;%0=0\";
          default:
            abort();
      }
  }"
  [(set_attr "type" "data_move_2,data_move_2,data_move_2")])

;;
;; Floating point conversions
;;
(define_expand "floathihf2"
  [(set (match_operand:HF 0 "register_operand" "")
	(float:HF (match_operand:HI 1 "register_operand" "")))]
  ""
  "
{
  if (!dsp16xx_floathihf2_libcall)
    dsp16xx_floathihf2_libcall = gen_rtx_SYMBOL_REF (Pmode, FLOATHIHF2_LIBCALL);
  
  emit_library_call (dsp16xx_floathihf2_libcall, 1, HFmode, 1,
		     operands[1], HImode);
  emit_move_insn (operands[0], hard_libcall_value(HFmode));
  DONE;
}")

(define_expand "fix_trunchfhi2"
  [(set (match_operand:HI 0 "register_operand" "")
	(fix:HI (match_operand:HF 1 "register_operand" "")))]
  ""
  "
{
  if (!dsp16xx_fixhfhi2_libcall)
    dsp16xx_fixhfhi2_libcall = gen_rtx_SYMBOL_REF (Pmode, FIXHFHI2_LIBCALL);
  
  emit_library_call (dsp16xx_fixhfhi2_libcall, 1, HImode, 1,
		     operands[1], HFmode);
  emit_move_insn (operands[0], hard_libcall_value(HImode));
  DONE;
}")

(define_expand "fixuns_trunchfhi2"
  [(set (match_operand:HI 0 "register_operand" "")
	(unsigned_fix:HI (match_operand:HF 1 "register_operand" "")))]
  ""
  "
{
  rtx reg1 = gen_reg_rtx (HFmode);
  rtx reg2 = gen_reg_rtx (HFmode);
  rtx reg3 = gen_reg_rtx (HImode);
  rtx label1 = gen_label_rtx ();
  rtx label2 = gen_label_rtx ();
  REAL_VALUE_TYPE offset;

  real_2expN (&offset, 31);

  if (reg1)			/* turn off complaints about unreached code */
    {
      emit_move_insn (reg1, CONST_DOUBLE_FROM_REAL_VALUE (offset, HFmode));
      do_pending_stack_adjust ();

      emit_insn (gen_cmphf (operands[1], reg1));
      emit_jump_insn (gen_bge (label1));

      emit_insn (gen_fix_trunchfhi2 (operands[0], operands[1]));
      emit_jump_insn (gen_rtx_SET (VOIDmode, pc_rtx,
				   gen_rtx_LABEL_REF (VOIDmode, label2)));
      emit_barrier ();

      emit_label (label1);
      emit_insn (gen_subhf3 (reg2, operands[1], reg1));
      emit_move_insn (reg3, GEN_INT (0x80000000));;

      emit_insn (gen_fix_trunchfhi2 (operands[0], reg2));
      emit_insn (gen_iorhi3 (operands[0], operands[0], reg3));

      emit_label (label2);

      /* allow REG_NOTES to be set on last insn (labels don't have enough
	 fields, and can't be used for REG_NOTES anyway).  */
      emit_insn (gen_rtx_USE (VOIDmode, stack_pointer_rtx));
      DONE;
    }
}")

;;
;; SHIFT INSTRUCTIONS
;;

(define_insn ""
  [(set (match_operand:HI 0 "register_operand" "=A")
        (ashiftrt:HI (match_operand:HI 1 "register_operand" "A")
                     (const_int 1)))]
  ""
  "%0=%1>>1"
  [(set_attr "type" "special")])

(define_insn ""
  [(set (match_operand:HI 0 "register_operand" "=A")
        (ashiftrt:HI (match_operand:HI 1 "register_operand" "A")
                     (const_int 4)))]
  ""
  "%0=%1>>4"
  [(set_attr "type" "special")])

(define_insn ""
  [(set (match_operand:HI 0 "register_operand" "=A")
        (ashiftrt:HI (match_operand:HI 1 "register_operand" "A")
                     (const_int 8)))]
  ""
  "%0=%1>>8"
  [(set_attr "type" "special")])

(define_insn ""
  [(set (match_operand:HI 0 "register_operand" "=A")
        (ashiftrt:HI (match_operand:HI 1 "register_operand" "A")
                     (const_int 16)))]
  ""
  "%0=%1>>16"
  [(set_attr "type" "special")])

;;
;; Arithmetic Right shift

(define_expand "ashrhi3"
  [(set (match_operand:HI 0 "register_operand" "")
        (ashiftrt:HI (match_operand:HI 1 "register_operand" "")
                     (match_operand:QI 2 "nonmemory_operand" "")))]
  ""
  "
{
  if (!TARGET_BMU)
  {
      /* If we are shifting by a constant we can do it in 1 or more
	 1600 core shift instructions. The core instructions can
	 shift by 1, 4, 8, or 16.  */
      
      if (GET_CODE(operands[2]) == CONST_INT)
	;
      else
      {
	rtx label1 = gen_label_rtx ();
	rtx label2 = gen_label_rtx ();

#if 0
	if (!dsp16xx_ashrhi3_libcall)
	  dsp16xx_ashrhi3_libcall
	    = gen_rtx_SYMBOL_REF (Pmode, ASHRHI3_LIBCALL);

	  emit_library_call (dsp16xx_ashrhi3_libcall, 1, HImode, 2,
			     operands[1], HImode,
			     operands[2], QImode);
	  emit_move_insn (operands[0], hard_libcall_value(HImode));
	  DONE;
#else
	do_pending_stack_adjust ();
	emit_insn (gen_tstqi (operands[2]));
	emit_jump_insn (gen_bne (label1));
	emit_move_insn (operands[0], operands[1]);
	emit_jump_insn (gen_jump (label2));
	emit_barrier ();
	emit_label (label1);

	if (GET_CODE(operands[2]) != MEM)
	  {
	    rtx stack_slot;
	    
	    stack_slot = assign_stack_temp (QImode, GET_MODE_SIZE(QImode), 0);
	    stack_slot = change_address (stack_slot, VOIDmode, XEXP (stack_slot, 0));
	    emit_move_insn (stack_slot, operands[2]);
	    operands[2] = stack_slot;
	  }

	emit_insn (gen_match_ashrhi3_nobmu (operands[0], operands[1], operands[2]));
	emit_label (label2);
	DONE;
#endif
      }
  }
}")

(define_insn "match_ashrhi3_bmu"
  [(set (match_operand:HI 0 "register_operand" "=A,A,A")
        (ashiftrt:HI (match_operand:HI 1 "register_operand" "A,A,!A")
                     (match_operand:QI 2 "nonmemory_operand" "B,I,h")))]
  "TARGET_BMU"
  "@
   %0=%1>>%2
   %0=%1>>%H2
   %0=%1>>%2"
  [(set_attr "type" "shift,shift_i,shift")])

(define_insn "match_ashrhi3_nobmu"
  [(set (match_operand:HI 0 "register_operand" "=A,A")
        (ashiftrt:HI (match_operand:HI 1 "register_operand" "A,0")
                     (match_operand:QI 2 "general_operand" "n,m")))]
  "!TARGET_BMU"
  "*
{
  if (which_alternative == 0)
    {
      emit_1600_core_shift (ASHIFTRT, operands, INTVAL(operands[2]));
      return \"\";
    }
  else
    {
      output_asm_insn (\"cloop=%2\", operands);
      output_asm_insn (\"do 0 {\", operands);
      output_asm_insn (\"%0=%0>>1\", operands);
      return \"}\";
    }
}")
		   


;;
;; Logical Right Shift

(define_insn ""
  [(set (match_operand:HI 0 "register_operand" "=A")
        (lshiftrt:HI (match_operand:HI 1 "register_operand" "A")
                     (const_int 1)))]
  "!TARGET_BMU"
  "%0=%1>>1\;%0=%b0&0x7fff"
  [(set_attr "type" "special")])

(define_insn ""
  [(set (match_operand:HI 0 "register_operand" "=A")
        (lshiftrt:HI (match_operand:HI 1 "register_operand" "A")
                     (const_int 4)))]
  "!TARGET_BMU"
  "%0=%1>>4\;%0=%b0&0x0fff"
  [(set_attr "type" "special")])

(define_insn ""
  [(set (match_operand:HI 0 "register_operand" "=A")
        (lshiftrt:HI (match_operand:HI 1 "register_operand" "A")
                     (const_int 8)))]
  "!TARGET_BMU"
  "%0=%1>>8\;%0=%b0&0x00ff"
  [(set_attr "type" "special")])

(define_insn ""
  [(set (match_operand:HI 0 "register_operand" "=A")
        (lshiftrt:HI (match_operand:HI 1 "register_operand" "A")
                     (const_int 16)))]
  "!TARGET_BMU"
  "%0=%1>>16\;%0=%b0&0x0000"
  [(set_attr "type" "special")])

(define_expand "lshrhi3"
  [(set (match_operand:HI 0 "register_operand" "")
        (lshiftrt:HI (match_operand:HI 1 "register_operand" "")
                     (match_operand:QI 2 "nonmemory_operand" "")))]
  ""
  "
{
  if (!TARGET_BMU)
    {
      /* If we are shifting by a constant we can do it in 1 or more
	 1600 core shift instructions. The core instructions can
	 shift by 1, 4, 8, or 16.  */
      
      if (GET_CODE(operands[2]) == CONST_INT)
	emit_insn (gen_match_lshrhi3_nobmu (operands[0], operands[1], operands[2]));	
      else
	{
	  rtx label1 = gen_label_rtx ();
	  rtx label2 = gen_label_rtx ();
#if 0
	  if (!dsp16xx_lshrhi3_libcall)
	    dsp16xx_lshrhi3_libcall
	      = gen_rtx_SYMBOL_REF (Pmode, LSHRHI3_LIBCALL);
	  
	  emit_library_call (dsp16xx_lshrhi3_libcall, 1, HImode, 2,
			     operands[1], HImode,
			     operands[2], QImode);
	  emit_move_insn (operands[0], hard_libcall_value(HImode));
	  DONE;
#else
	  do_pending_stack_adjust ();
	  emit_insn (gen_tstqi (operands[2]));
	  emit_jump_insn (gen_bne (label1));
	  emit_move_insn (operands[0], operands[1]);
	  emit_jump_insn (gen_jump (label2));
	  emit_barrier ();
	  emit_label (label1);

	  if (GET_CODE(operands[2]) != MEM)
	    {
	      rtx stack_slot;
	    
	      stack_slot = assign_stack_temp (QImode, GET_MODE_SIZE(QImode), 0);
	      stack_slot = change_address (stack_slot, VOIDmode, XEXP (stack_slot, 0));
	      emit_move_insn (stack_slot, operands[2]);
	      operands[2] = stack_slot;
	    }

	  emit_insn (gen_match_lshrhi3_nobmu (operands[0], operands[1], operands[2]));
	  emit_label (label2);
	  DONE;
#endif
	}
    }
}")

(define_insn "match_lshrhi3"
  [(set (match_operand:HI 0 "register_operand" "=A,A,A")
        (lshiftrt:HI (match_operand:HI 1 "register_operand" "A,A,!A")
                     (match_operand:QI 2 "nonmemory_operand" "B,I,h")))]
  "TARGET_BMU"
  "@
   %0=%1>>>%2
   %0=%1>>>%H2
   %0=%1>>>%2"
  [(set_attr "type" "shift,shift_i,shift")])

(define_insn "match_lshrhi3_nobmu"
  [(set (match_operand:HI 0 "register_operand" "=A,A")
        (lshiftrt:HI (match_operand:HI 1 "register_operand" "A,0")
                     (match_operand:QI 2 "general_operand" "n,m")))
   (clobber (match_scratch:QI 3 "=X,Y"))]
  "!TARGET_BMU"
  "*
{
  if (which_alternative == 0)
    {
      emit_1600_core_shift (LSHIFTRT, operands, INTVAL(operands[2]));
      return \"\";
    }
  else
    {
      output_asm_insn (\"%3=psw\;psw=0\",operands);
      output_asm_insn (\"cloop=%2\", operands);
      output_asm_insn (\"do 0 {\", operands);
      output_asm_insn (\"%0=%0>>1\", operands);
      output_asm_insn (\"}\", operands);
      return \"psw=%3\";
    }
}")


;;
;; Arithmetic Left shift

;; Start off with special case arithmetic left shift by 1,4,8 or 16.


(define_insn ""
  [(set (match_operand:HI 0 "register_operand" "=A")
        (ashift:HI (match_operand:HI 1 "register_operand" "A")
                   (const_int 1)))]
  ""
  "%0=%1<<1"
  [(set_attr "type" "special")])

(define_insn ""
  [(set (match_operand:HI 0 "register_operand" "=A")
        (ashift:HI (match_operand:HI 1 "register_operand" "A")
                   (const_int 4)))]
  ""
  "%0=%1<<4"
  [(set_attr "type" "special")])

(define_insn ""
  [(set (match_operand:HI 0 "register_operand" "=A")
        (ashift:HI (match_operand:HI 1 "register_operand" "A")
                   (const_int 8)))]
  ""
  "%0=%1<<8"
  [(set_attr "type" "special")])

(define_insn ""
  [(set (match_operand:HI 0 "register_operand" "=A")
        (ashift:HI (match_operand:HI 1 "general_operand" "A")
                   (const_int 16)))]
  ""
  "%0=%1<<16"
  [(set_attr "type" "special")])



;; Normal Arithmetic Shift Left


(define_expand "ashlhi3"
  [(set (match_operand:HI 0 "register_operand" "")
        (ashift:HI (match_operand:HI 1 "register_operand" "")
                   (match_operand:QI 2 "nonmemory_operand" "")))]
  ""
  "
{
  if (!TARGET_BMU)
  {
      /* If we are shifting by a constant we can do it in 1 or more
	 1600 core shift instructions. The core instructions can
	 shift by 1, 4, 8, or 16.  */
      
      if (GET_CODE(operands[2]) == CONST_INT)
	;
      else
      {
	rtx label1 = gen_label_rtx ();
	rtx label2 = gen_label_rtx ();
#if 0
	if (!dsp16xx_ashlhi3_libcall)
	  dsp16xx_ashlhi3_libcall
	    = gen_rtx_SYMBOL_REF (Pmode, ASHLHI3_LIBCALL);

	  emit_library_call (dsp16xx_ashlhi3_libcall, 1, HImode, 2,
			     operands[1], HImode, operands[2], QImode);
	  emit_move_insn (operands[0], hard_libcall_value(HImode));
	  DONE;
#else
	do_pending_stack_adjust ();
	emit_insn (gen_tstqi (operands[2]));
	emit_jump_insn (gen_bne (label1));
	emit_move_insn (operands[0], operands[1]);
	emit_jump_insn (gen_jump (label2));
	emit_barrier ();
	emit_label (label1);

	if (GET_CODE(operands[2]) != MEM)
	  {
	    rtx stack_slot;
	    
	    stack_slot = assign_stack_temp (QImode, GET_MODE_SIZE(QImode), 0);
	    stack_slot = change_address (stack_slot, VOIDmode, XEXP (stack_slot, 0));
	    emit_move_insn (stack_slot, operands[2]);
	    operands[2] = stack_slot;
	  }
	emit_insn (gen_match_ashlhi3_nobmu (operands[0], operands[1], operands[2]));
	emit_label (label2);
	DONE;
#endif
      }
  }
}")

(define_insn "match_ashlhi3"
  [(set (match_operand:HI 0 "register_operand" "=A,A,A")
        (ashift:HI (match_operand:HI 1 "register_operand" "A,A,A")
                   (match_operand:QI 2 "nonmemory_operand" "B,I,!h")))]
  "TARGET_BMU"
  "@
   %0=%1<<%2\;move %u0=%u0
   %0=%1<<%H2\;move %u0=%u0
   %0=%1<<%2\;move %u0=%u0"
  [(set_attr "type" "shift_multiple,shift_multiple,shift_multiple")])

(define_insn "match_ashlhi3_nobmu"
  [(set (match_operand:HI 0 "register_operand" "=A,A")
        (ashift:HI (match_operand:HI 1 "register_operand" "A,0")
		   (match_operand:QI 2 "general_operand" "n,m")))]
  "!TARGET_BMU"
  "*
{
  if (which_alternative == 0)
    {
      emit_1600_core_shift (ASHIFT, operands, INTVAL(operands[2]));
      return \"\";
    }
  else
    {
      output_asm_insn (\"cloop=%2\", operands);
      output_asm_insn (\"do 0 {\", operands);
      output_asm_insn (\"%0=%0<<1\", operands);
      return \"}\";
    }
}")




(define_insn "extv"
  [(set (match_operand:QI 0 "register_operand" "=k,u")
	(sign_extract:QI (match_operand:QI 1 "register_operand" "ku,ku")
			 (match_operand:QI 2 "const_int_operand" "n,n")
			 (match_operand:QI 3 "const_int_operand" "n,n")))
	(clobber (match_scratch:QI 4 "=j,q"))]
  "TARGET_BMU"
  "*
{
	operands[5]
	= GEN_INT ((INTVAL (operands[2]) << 8) + (INTVAL (operands[3]) & 0xff));
	return \"%m0 = extracts (%m1, %H5)\";
}"
[(set_attr "type" "shift_i")])

(define_insn "extzv"
  [(set (match_operand:QI 0 "register_operand" "=k,u")
	(zero_extract:QI (match_operand:QI 1 "register_operand" "ku,ku")
			 (match_operand:QI 2 "const_int_operand" "n,n")
			 (match_operand:QI 3 "const_int_operand" "n,n")))
	(clobber (match_scratch:QI 4 "=j,q"))]
  "TARGET_BMU"
  "*
{
	operands[5]
	= GEN_INT ((INTVAL (operands[2]) << 8) + (INTVAL (operands[3]) & 0xff));
	return \"%m0 = extractz (%m1, %H5)\";
}"
[(set_attr "type" "shift_i")])

;;
;; conditional instructions
;;

(define_expand "seq"
  [(set (match_operand:QI 0 "register_operand" "")
	(eq:QI (match_dup 1) (const_int 0)))]
  ""
  "
{ 
   if (dsp16xx_compare_gen)
     operands[1] = gen_compare_reg (EQ, dsp16xx_compare_op0, dsp16xx_compare_op1);
   else
     operands[1] = gen_tst_reg (dsp16xx_compare_op0);
}")

(define_expand "sne"
  [(set (match_operand:QI 0 "register_operand" "")
	(ne:QI (match_dup 1) (const_int 0)))]
  ""
  "
{ 
   if (dsp16xx_compare_gen)
     operands[1] = gen_compare_reg (NE, dsp16xx_compare_op0, dsp16xx_compare_op1);
   else
     operands[1] = gen_tst_reg (dsp16xx_compare_op0);
}")


(define_expand "sgt"
  [(set (match_operand:QI 0 "register_operand" "")
	(gt:QI (match_dup 1) (const_int 0)))]
  ""
  "
{ 
   if (dsp16xx_compare_gen)
     operands[1] = gen_compare_reg (GT, dsp16xx_compare_op0, dsp16xx_compare_op1);
   else
     operands[1] = gen_tst_reg (dsp16xx_compare_op0);
}")


(define_expand "slt"
  [(set (match_operand:QI 0 "register_operand" "")
	(lt:QI (match_dup 1) (const_int 0)))]
  ""
  "
{ 
   if (dsp16xx_compare_gen)
     operands[1] = gen_compare_reg (LT, dsp16xx_compare_op0, dsp16xx_compare_op1);
   else
     operands[1] = gen_tst_reg (dsp16xx_compare_op0);
}")

(define_expand "sge"
  [(set (match_operand:QI 0 "register_operand" "")
	(ge:QI (match_dup 1) (const_int 0)))]
  ""
  "
{ 
   if (dsp16xx_compare_gen)
     operands[1] = gen_compare_reg (GE, dsp16xx_compare_op0, dsp16xx_compare_op1);
   else
     operands[1] = gen_tst_reg (dsp16xx_compare_op0);
}")


(define_expand "sle"
  [(set (match_operand:QI 0 "register_operand" "")
	(le:QI (match_dup 1) (const_int 0)))]
  ""
  "
{ 
   if (dsp16xx_compare_gen)
     operands[1] = gen_compare_reg (LE, dsp16xx_compare_op0, dsp16xx_compare_op1);
   else
     operands[1] = gen_tst_reg (dsp16xx_compare_op0);
}")


(define_expand "sgtu"
  [(set (match_operand:QI 0 "register_operand" "")
	(gtu:QI (match_dup 1) (const_int 0)))]
  ""
  "
{ 
   if (dsp16xx_compare_gen)
     operands[1] = gen_compare_reg (GTU, dsp16xx_compare_op0, dsp16xx_compare_op1);
   else
     operands[1] = gen_tst_reg (dsp16xx_compare_op0);
}")


(define_expand "sltu"
  [(set (match_operand:QI 0 "register_operand" "")
	(ltu:QI (match_dup 1) (const_int 0)))]
  ""
  "
{ 
   if (dsp16xx_compare_gen)
     operands[1] = gen_compare_reg (LTU, dsp16xx_compare_op0, dsp16xx_compare_op1);
   else
     operands[1] = gen_tst_reg (dsp16xx_compare_op0);
}")


(define_expand "sgeu"
  [(set (match_operand:QI 0 "register_operand" "")
	(geu:QI (match_dup 1) (const_int 0)))]
  ""
  "
{ 
   if (dsp16xx_compare_gen)
     operands[1] = gen_compare_reg (GEU, dsp16xx_compare_op0, dsp16xx_compare_op1);
   else
     operands[1] = gen_tst_reg (dsp16xx_compare_op0);
}")


(define_expand "sleu"
  [(set (match_operand:QI 0 "register_operand" "")
	(leu:QI (match_dup 1) (const_int 0)))]
  ""
  "
{ 
   if (dsp16xx_compare_gen)
     operands[1] = gen_compare_reg (LEU, dsp16xx_compare_op0, dsp16xx_compare_op1);
   else
     operands[1] = gen_tst_reg (dsp16xx_compare_op0);
}")


(define_insn "scc"
  [(set (match_operand:QI 0 "register_operand" "=jq")
	(match_operator:QI 1 "comparison_operator" [(cc0) (const_int 0)]))]
  ""
  "%0 = 0\;if %C1 %b0 = %b0 + 1"
  [(set_attr "type" "special_2")])

;;
;; Jump Instructions
;;

(define_expand "beq"
  [(set (pc)
	(if_then_else (eq (match_dup 1)
	                  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{ 
   if (dsp16xx_compare_gen)
     operands[1] = gen_compare_reg (EQ, dsp16xx_compare_op0, dsp16xx_compare_op1);
   else
     operands[1] = gen_tst_reg (dsp16xx_compare_op0);
}")

(define_expand "bne"
  [(set (pc)
	(if_then_else (ne (match_dup 1)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{ 
   if (dsp16xx_compare_gen)
     operands[1] = gen_compare_reg (NE, dsp16xx_compare_op0, dsp16xx_compare_op1);
   else
     operands[1] = gen_tst_reg (dsp16xx_compare_op0);
}")


(define_expand "bgt"
  [(set (pc)
	(if_then_else (gt (match_dup 1)
	                  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{ 
   if (dsp16xx_compare_gen)
     operands[1] = gen_compare_reg (GT, dsp16xx_compare_op0, dsp16xx_compare_op1);
   else
     operands[1] = gen_tst_reg (dsp16xx_compare_op0);
}")


(define_expand "bge"
  [(set (pc)
	(if_then_else (ge (match_dup 1)
	                  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{ 
   if (dsp16xx_compare_gen)
     operands[1] = gen_compare_reg (GE, dsp16xx_compare_op0, dsp16xx_compare_op1);
   else
     operands[1] = gen_tst_reg (dsp16xx_compare_op0);
}")


(define_expand "blt"
  [(set (pc)
	(if_then_else (lt (match_dup 1)
	                  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{ 
   if (dsp16xx_compare_gen)
     operands[1] = gen_compare_reg (LT, dsp16xx_compare_op0, dsp16xx_compare_op1);
   else
     operands[1] = gen_tst_reg (dsp16xx_compare_op0);
}")


(define_expand "ble"
  [(set (pc)
	(if_then_else (le (match_dup 1)
	                  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{ 
   if (dsp16xx_compare_gen)
     operands[1] = gen_compare_reg (LE, dsp16xx_compare_op0, dsp16xx_compare_op1);
   else
     operands[1] = gen_tst_reg (dsp16xx_compare_op0);
}")


(define_expand "bgtu"
  [(set (pc)
	(if_then_else (gtu (match_dup 1)
	                   (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{ 
   if (dsp16xx_compare_gen)
     operands[1] = gen_compare_reg (GTU, dsp16xx_compare_op0, dsp16xx_compare_op1);
   else
     operands[1] = gen_tst_reg (dsp16xx_compare_op0);
}")


(define_expand "bgeu"
  [(set (pc)
	(if_then_else (geu (match_dup 1)
	                   (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{ 
   if (dsp16xx_compare_gen)
     operands[1] = gen_compare_reg (GEU, dsp16xx_compare_op0, dsp16xx_compare_op1);
   else
     operands[1] = gen_tst_reg (dsp16xx_compare_op0);
}")


(define_expand "bltu"
  [(set (pc)
	(if_then_else (ltu (match_dup 1)
	                   (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{ 
   if (dsp16xx_compare_gen)
     operands[1] = gen_compare_reg (LTU, dsp16xx_compare_op0, dsp16xx_compare_op1);
   else
     operands[1] = gen_tst_reg (dsp16xx_compare_op0);
}")


(define_expand "bleu"
  [(set (pc)
	(if_then_else (leu (match_dup 1)
	                   (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{ 
   if (dsp16xx_compare_gen)
     operands[1] = gen_compare_reg (LEU, dsp16xx_compare_op0, dsp16xx_compare_op1);
   else
     operands[1] = gen_tst_reg (dsp16xx_compare_op0);
}")


(define_insn ""
  [(set (pc)
	(if_then_else (match_operator 1 "comparison_operator" 
                                      [(cc0) (const_int 0)])
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  "!TARGET_NEAR_JUMP"
  "pt=%l0\;if %C1 goto pt"
  [(set_attr "type" "cond_jump")])

(define_insn ""
  [(set (pc)
	(if_then_else (match_operator 1 "comparison_operator" 
                                      [(cc0) (const_int 0)])
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  "TARGET_NEAR_JUMP"
  "if %C1 goto %l0"
  [(set_attr "type" "cond_jump")])

;;
;; Negated conditional jump instructions.
;; These are necessary because jump optimization can turn
;; direct-conditional branches into reverse-conditional
;; branches.

(define_insn ""
  [(set (pc)
	(if_then_else (match_operator 1 "comparison_operator" 
                                      [(cc0) (const_int 0)])
	              (pc)
		      (label_ref (match_operand 0 "" ""))))]
  "!TARGET_NEAR_JUMP"
  "pt=%l0\;if %I1 goto pt"
  [(set_attr "type" "cond_jump")])

(define_insn ""
  [(set (pc)
	(if_then_else (match_operator 1 "comparison_operator" 
                                      [(cc0) (const_int 0)])
	              (pc)
		      (label_ref (match_operand 0 "" ""))))]
  "TARGET_NEAR_JUMP"
  "if %I1 goto %l0"
  [(set_attr "type" "cond_jump")])


;;
;; JUMPS
;;

(define_insn "jump"
  [(set (pc)
        (label_ref (match_operand 0 "" "")))]
  ""
  "*
   {
	if (TARGET_NEAR_JUMP)
	    return \"goto %l0\";
        else
	    return \"pt=%l0\;goto pt\";
   }"
   [(set_attr "type" "jump")])


(define_insn "indirect_jump"
  [(set (pc) (match_operand:QI 0 "register_operand" "A"))]
  ""
  "pt=%0\;goto pt"
   [(set_attr "type" "jump")])

(define_insn "tablejump"
  [(set (pc) (match_operand:QI 0 "register_operand" "A"))
   (use (label_ref (match_operand 1 "" "")))]
  ""
  "pt=%0\;goto pt"
   [(set_attr "type" "jump")])

;;
;; FUNCTION CALLS
;;

;; Call subroutine with no return value.


(define_expand "call"
  [(parallel [(call (match_operand:QI 0 "" "")
	            (match_operand 1 "" ""))
	     (clobber (reg:QI 24))])]
  ""
  "
{
  if (GET_CODE (operands[0]) == MEM
      && ! call_address_operand (XEXP (operands[0], 0), QImode))
    operands[0] = gen_rtx_MEM (GET_MODE (operands[0]),
			       force_reg (Pmode, XEXP (operands[0], 0)));
}")

(define_insn ""
  [(parallel [(call (mem:QI (match_operand:QI 0 "call_address_operand" "hR"))
	                    (match_operand 1 "" ""))
	      (clobber (reg:QI 24))])]
  ""
  "*
{
    if (GET_CODE (operands[0]) == REG || 
	(GET_CODE(operands[0]) == SYMBOL_REF && !TARGET_NEAR_CALL))
	return \"pt=%0\;call pt\";
    else
        return \"call %0\";
}"
[(set_attr "type" "call")])

;; Call subroutine with return value.

(define_expand "call_value"
  [(parallel [(set (match_operand 0 "register_operand" "=f")
		  (call (match_operand:QI 1 "call_address_operand" "hR")
	                (match_operand:QI 2 "" "")))
	      (clobber (reg:QI 24))])]
  ""
  "
{
  if (GET_CODE (operands[1]) == MEM
      && ! call_address_operand (XEXP (operands[1], 0), QImode))
    operands[1] = gen_rtx_MEM (GET_MODE (operands[1]),
			       force_reg (Pmode, XEXP (operands[1], 0)));
}")

(define_insn ""
  [(parallel [(set (match_operand 0 "register_operand" "=f")
		  (call (mem:QI (match_operand:QI 1 "call_address_operand" "hR"))
	                        (match_operand:QI 2 "" "")))
	      (clobber (reg:QI 24))])]
  ""
  "*
{
    if (GET_CODE (operands[1]) == REG ||
	(GET_CODE(operands[1]) == SYMBOL_REF && !TARGET_NEAR_CALL))
	return \"pt=%1\;call pt\";
    else
        return \"call %1\";
}"
[(set_attr "type" "call")])


(define_expand "untyped_call"
  [(parallel [(call (match_operand 0 "" "")
		    (const_int 0))
	      (match_operand 1 "" "")
	      (match_operand 2 "" "")])]
  ""
  "
{
  int i;

  emit_call_insn (GEN_CALL (operands[0], const0_rtx, NULL, const0_rtx));

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
  "")

(define_insn "nop"
  [(const_int 0)]
  ""
  "nop"
  [(set_attr "type"     "nop")])

;;
;; PEEPHOLE PATTERNS
;;


(define_peephole
  [(parallel [(set (cc0) 
		   (compare (match_operand:QI 0 "register_operand"  "k,k,!k,u,u,!u")
			    (match_operand:QI 1 "register_operand"  "w,z,u,w,z,k")))
	      (use (match_operand:QI 2 "register_operand" "=j,j,j,q,q,q"))
	      (use (match_operand:QI 3 "register_operand" "=v,y,q,v,y,j"))])
   (set (pc)	
	(if_then_else (match_operator 5 "uns_comparison_operator" 
                                      [(cc0) (const_int 0)])
		      (label_ref (match_operand 4 "" ""))
		      (pc)))]
  "!TARGET_NEAR_JUMP"
  "pt=%l4\;%2-%3\;if %C5 goto pt")

(define_peephole
  [(parallel [(set (cc0) 
		   (compare (match_operand:QI 0 "register_operand"  "k,k,!k,u,u,!u")
			    (match_operand:QI 1 "register_operand"  "w,z,u,w,z,k")))
	      (use (match_operand:QI 2 "register_operand" "=j,j,j,q,q,q"))
	      (use (match_operand:QI 3 "register_operand" "=v,y,q,v,y,j"))])
   (set (pc)	
	(if_then_else (match_operator 5 "uns_comparison_operator" 
                                      [(cc0) (const_int 0)])
		      (pc)
		      (label_ref (match_operand 4 "" ""))))]
  "!TARGET_NEAR_JUMP"
  "pt=%l4\;%2-%3\;if %I5 goto pt")


(define_peephole
  [(parallel [(set (cc0) 
		   (compare (match_operand:QI 0 "register_operand"  "k,u")
			    (match_operand:QI 1 "const_int_operand" "i,i")))
	      (use (match_operand:QI 2 "register_operand" "=j,q"))])
   (set (pc)	
	(if_then_else (match_operator 4 "uns_comparison_operator" 
                                      [(cc0) (const_int 0)])
		      (label_ref (match_operand 3 "" ""))
		      (pc)))]
  "!TARGET_NEAR_JUMP"
  "pt=%l3\;%0-%H1\;if %C4 goto pt")

(define_peephole
  [(parallel [(set (cc0) 
		   (compare (match_operand:QI 0 "register_operand"  "k,u")
			    (match_operand:QI 1 "const_int_operand" "i,i")))
	      (use (match_operand:QI 2 "register_operand" "=j,q"))])
   (set (pc)	
	(if_then_else (match_operator 4 "uns_comparison_operator" 
                                      [(cc0) (const_int 0)])
		      (pc)
		      (label_ref (match_operand 3 "" ""))))]
  "!TARGET_NEAR_JUMP"
  "pt=%l3\;%0-%H1\;if %I4 goto pt")

;;
;;; QImode SIGNED COMPARE PEEPHOLE OPTIMIZATIONS
;;

(define_peephole
  [(parallel [(set (cc0) 
		   (compare (match_operand:QI 0 "register_operand"  "j,j,h,q,q,q")
			    (match_operand:QI 1 "register_operand"  "v,y,q,v,y,j")))
	      (use (match_operand:QI 2 "register_operand" "=k,k,k,u,u,u"))
	      (use (match_operand:QI 3 "register_operand" "=w,z,u,w,z,k"))])
   (set (pc)	
	(if_then_else (match_operator 5 "signed_comparison_operator" 
                                      [(cc0) (const_int 0)])
		      (label_ref (match_operand 4 "" ""))
		      (pc)))]
  "!TARGET_NEAR_JUMP"
  "pt=%l4\;%0-%1\;if %C5 goto pt")


(define_peephole
  [(parallel [(set (cc0) 
		   (compare (match_operand:QI 0 "register_operand"  "j,j,j,q,q,q")
			    (match_operand:QI 1 "register_operand"  "v,y,q,v,y,j")))
	      (use (match_operand:QI 2 "register_operand" "=k,k,k,u,u,u"))
	      (use (match_operand:QI 3 "register_operand" "=w,z,u,w,z,k"))])
   (set (pc)	
	(if_then_else (match_operator 5 "signed_comparison_operator" 
                                      [(cc0) (const_int 0)])
		      (pc)
		      (label_ref (match_operand 4 "" ""))))]
  "!TARGET_NEAR_JUMP"
  "pt=%l4\;%0-%1\;if %I5 goto pt")


(define_peephole
  [(parallel [(set (cc0) 
		   (compare (match_operand:QI 0 "register_operand"  "j,q")
			    (match_operand:QI 1 "const_int_operand" "i,i")))
	      (use (match_operand:QI 2 "register_operand" "=k,u"))])
   (set (pc)	
	(if_then_else (match_operator 4 "signed_comparison_operator" 
                                      [(cc0) (const_int 0)])
		      (label_ref (match_operand 3 "" ""))
		      (pc)))]
  "!TARGET_NEAR_JUMP"
  "pt=%l3\;%b0-%H1\;if %C4 goto pt")

(define_peephole
  [(parallel [(set (cc0) 
		   (compare (match_operand:QI 0 "register_operand"  "j,q")
			    (match_operand:QI 1 "const_int_operand" "i,i")))
	      (use (match_operand:QI 2 "register_operand" "=k,u"))])
   (set (pc)	
	(if_then_else (match_operator 4 "signed_comparison_operator" 
                                      [(cc0) (const_int 0)])
		      (pc)
		      (label_ref (match_operand 3 "" ""))))]
  "!TARGET_NEAR_JUMP"
  "pt=%l3\;%b0-%H1\;if %I4 goto pt")

;; TST PEEPHOLE PATTERNS

(define_peephole
  [(parallel [(set (cc0) 
		   (match_operand:QI 0 "register_operand"  "j,q"))
	      (use (match_operand:QI 1 "register_operand" "=k,u"))])
   (set (pc)	
	(if_then_else (match_operator 3 "signed_comparison_operator" 
                                      [(cc0) (const_int 0)])
		      (pc)
		      (label_ref (match_operand 2 "" ""))))]
  "!TARGET_NEAR_JUMP"
  "pt=%l2\;%b0-0\;if %I3 goto pt")

(define_peephole
  [(parallel [(set (cc0) 
		   (match_operand:QI 0 "register_operand"  "j,q"))
	      (use (match_operand:QI 1 "register_operand" "=k,u"))])
   (set (pc)	
	(if_then_else (match_operator 3 "signed_comparison_operator" 
                                      [(cc0) (const_int 0)])
		      (label_ref (match_operand 2 "" ""))
		      (pc)))]
  "!TARGET_NEAR_JUMP"
  "pt=%l2\;%b0-0\;if %C3 goto pt")

;; HImode peephole patterns

(define_peephole
  [(set (cc0) 
	(compare (match_operand:HI 0 "register_operand"  "A,A")
		 (match_operand:HI 1 "register_operand"  "Z,A")))
   (set (pc)	
	(if_then_else (match_operator 3 "signed_comparison_operator" 
                                      [(cc0) (const_int 0)])
		      (label_ref (match_operand 2 "" ""))
		      (pc)))]
  "!TARGET_NEAR_JUMP"
  "pt=%l2\;%0-%1\;if %C3 goto pt")

(define_peephole
  [(set (cc0) 
	(compare (match_operand:HI 0 "register_operand"  "A,A")
		 (match_operand:HI 1 "register_operand"  "Z,A")))
   (set (pc)	
	(if_then_else (match_operator 3 "signed_comparison_operator" 
                                      [(cc0) (const_int 0)])
		      (pc)
		      (label_ref (match_operand 2 "" ""))))]
  "!TARGET_NEAR_JUMP"
  "pt=%l2\;%0-%1\;if %I3 goto pt")
