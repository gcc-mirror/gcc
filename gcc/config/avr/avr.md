;; -*- Mode: Scheme -*-
;;   Machine description for GNU compiler,
;;   for ATMEL AVR micro controllers.
;;   Copyright (C) 1998, 1999, 2000 Free Software Foundation, Inc.
;;   Contributed by Denis Chertykov (denisc@overta.ru)

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

;; Condition code settings.
(define_attr "cc" "none,set_czn,set_zn,set_n,compare,clobber"
  (const_string "none"))

(define_attr "type" "branch,branch1,arith"
  (const_string "arith"))

;; The size of instructions in bytes.
;; XXX may depend from "cc"

(define_attr "length" ""
  (cond [(eq_attr "type" "branch")
         (if_then_else (and (ge (minus (pc) (match_dup 0))
                                (const_int -63))
                            (le (minus (pc) (match_dup 0))
                                (const_int 62)))
                       (const_int 1)
                       (if_then_else (and (ge (minus (pc) (match_dup 0))
                                              (const_int -2045))
                                          (le (minus (pc) (match_dup 0))
                                              (const_int 2045)))
                                     (const_int 2)
                                     (const_int 2)))
         (eq_attr "type" "branch1")
         (if_then_else (and (ge (minus (pc) (match_dup 0))
                                (const_int -62))
                            (le (minus (pc) (match_dup 0))
                                (const_int 61)))
                       (const_int 2)
                       (if_then_else (and (ge (minus (pc) (match_dup 0))
                                              (const_int -2044))
                                          (le (minus (pc) (match_dup 0))
                                              (const_int 2043)))
                                     (const_int 3)
                                     (const_int 3)))]
        (const_int 2)))

(define_insn "*pop1"
  [(set (reg:HI 32) (plus:HI (reg:HI 32) (const_int 1)))]
  ""
  "pop __tmp_reg__"
  [(set_attr "length" "1")])

(define_insn "*pop2"
  [(set (reg:HI 32) (plus:HI (reg:HI 32) (const_int 2)))]
  ""
  "pop __tmp_reg__
	pop __tmp_reg__"
  [(set_attr "length" "2")])

(define_insn "*pop3"
  [(set (reg:HI 32) (plus:HI (reg:HI 32) (const_int 3)))]
  ""
  "pop __tmp_reg__
	pop __tmp_reg__
 	pop __tmp_reg__"
  [(set_attr "length" "3")])

(define_insn "*pop4"
  [(set (reg:HI 32) (plus:HI (reg:HI 32) (const_int 4)))]
  ""
  "pop __tmp_reg__
	pop __tmp_reg__
	pop __tmp_reg__
	pop __tmp_reg__"
  [(set_attr "length" "4")])

(define_insn "*pop5"
  [(set (reg:HI 32) (plus:HI (reg:HI 32) (const_int 5)))]
  ""
  "pop __tmp_reg__
	pop __tmp_reg__
	pop __tmp_reg__
	pop __tmp_reg__
	pop __tmp_reg__"
  [(set_attr "length" "5")])

(define_insn "*pushqi"
  [(set (mem:QI (post_dec (reg:HI 32)))
        (match_operand:QI 0 "nonmemory_operand" "r,L"))]
  "(operands[0] == const0_rtx || register_operand (operands[0], QImode))"
  "@
	push %0
	push __zero_reg__"
  [(set_attr "length" "1,1")])


(define_insn "*pushhi"
  [(set (mem:HI (post_dec (reg:HI 32)))
        (match_operand:HI 0 "nonmemory_operand" "r,L"))]
  "(operands[0] == const0_rtx || register_operand (operands[0], HImode))"
  "@
	push %B0\;push %A0
	push __zero_reg__\;push __zero_reg__"
  [(set_attr "length" "2,2")])

(define_insn "*pushsi"
  [(set (mem:SI (post_dec (reg:HI 32)))
        (match_operand:SI 0 "nonmemory_operand" "r,L"))]
  "(operands[0] == const0_rtx || register_operand (operands[0], SImode))"
  "@
	push %D0\;push %C0\;push %B0\;push %A0
	push __zero_reg__\;push __zero_reg__\;push __zero_reg__\;push __zero_reg__"
  [(set_attr "length" "4,4")])

(define_insn "*pushsf"
  [(set (mem:SF (post_dec (reg:HI 32)))
        (match_operand:SF 0 "register_operand" "r"))]
  ""
  "push %D0
	push %C0
	push %B0
	push %A0"
  [(set_attr "length" "4")])

(define_insn "*mov_r_sp"
  [(set (match_operand:HI 0 "register_operand" "=r")
        (reg:HI 32))]
  ""
  "in %A0,__SP_L__
	in %B0,__SP_H__"
  [(set_attr "length" "2")])

(define_insn "*mov_sp_r"
  [(set (reg:HI 32)
        (match_operand:HI 0 "register_operand" "r"))]
  "!TARGET_NO_INTERRUPTS"
  "in __tmp_reg__,__SREG__
	cli
	out __SP_L__,%A0
	out __SREG__,__tmp_reg__
	out __SP_H__,%B0"
  [(set_attr "length" "5")])

(define_insn "*mov_sp_r_no_interrupts"
  [(set (reg:HI 32)
        (match_operand:HI 0 "register_operand" "r"))]
  "TARGET_NO_INTERRUPTS"
  "out __SP_L__,%A0
	out __SP_H__,%B0"
  [(set_attr "length" "2")])

;;========================================================================
;; move byte
(define_expand "movqi"
  [(set (match_operand:QI 0 "nonimmediate_operand" "")
	(match_operand:QI 1 "general_operand" ""))]
  ""
  "
{
  /* One of the ops has to be in a register */
  if (!register_operand(operand0, QImode)
      && ! (register_operand(operand1, QImode) || const0_rtx == operand1))
    {
      operands[1] = copy_to_mode_reg(QImode, operand1);
    }
 }"); 

(define_insn "*movqi"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=r,r,d,Qm,r,q")
	(match_operand:QI 1 "general_operand"      "r,L,i,rL,Qm,r"))]
  "(register_operand (operands[0],QImode)
    || register_operand (operands[1], QImode) || const0_rtx == operands[1])"
  "*{
    switch (which_alternative)
      {
      case 0:
	return AS2 (mov, %0,%1);
      case 1:
	return AS1 (clr, %0);
      case 2:
	return AS2 (ldi, %0,lo8(%1));
      case 3:
        {
          rtx save1=NULL;
          if (operands[1] == const0_rtx)
            {
              save1 = operands[1];
              operands[1] = zero_reg_rtx;
            }
          output_asm_insn (out_movqi_mr_r (insn,operands,NULL), operands);
          if (save1)
            operands[1] = save1;
        }
        return \"\";
      case 4:
        return out_movqi_r_mr (insn,operands,NULL);
      case 5:
        return (AS2 (in,__tmp_reg__,__SREG__) CR_TAB
	        \"cli\"                       CR_TAB
	        AS2 (out,__SREG__,__tmp_reg__)CR_TAB
	        AS2 (out,%0,%1));
      }
}"
  [(set_attr "length" "1,1,1,5,5,4")
   (set_attr "cc" "none,clobber,none,clobber,clobber,none")])

;;============================================================================
;; move word (16 bit)

(define_expand "movhi"
  [(set (match_operand:HI 0 "nonimmediate_operand" "")
        (match_operand:HI 1 "general_operand"       ""))]
  ""
  "
{
   /* One of the ops has to be in a register */
  if (!register_operand(operand0, HImode)
      && !(register_operand(operand1, HImode)  || const0_rtx == operands[1]))
    {
      operands[1] = copy_to_mode_reg(HImode, operand1);
    }
}")

(define_insn "*movhi"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=r,r,d,r,m")
        (match_operand:HI 1 "general_operand"       "r,L,i,m,rL"))]
  "(register_operand (operands[0],HImode)
    || register_operand (operands[1],HImode) || const0_rtx == operands[1])"
  "*{
  rtx link;
  switch (which_alternative)
    {
    case 0: /* mov r,r */
      if (true_regnum (operands[0]) > true_regnum (operands[1]))
        return (AS2 (mov,%B0,%B1) CR_TAB
	        AS2 (mov,%A0,%A1));
      else
        return (AS2 (mov,%A0,%A1) CR_TAB
	        AS2 (mov,%B0,%B1));
    case 1:  /* mov r,L */
      return (AS1 (clr,%A0) CR_TAB
	      AS1 (clr,%B0));
    case 2: /* mov r,d */
      if (operands[1] == const1_rtx
          && (link = find_reg_note (insn, REG_WAS_0, 0))
	  /* Make sure the insn that stored the 0 is still present.  */
	  && ! INSN_DELETED_P (XEXP (link, 0))
	  && GET_CODE (XEXP (link, 0)) != NOTE
	  /* Make sure cross jumping didn't happen here.  */
	  && no_labels_between_p (XEXP (link, 0), insn)
	  /* Make sure the reg hasn't been clobbered.  */
	  && ! reg_set_between_p (operands[0], XEXP (link, 0), insn))
      /* Fastest way to change a 0 to a 1.  */
        return AS1 (inc,%A0 ; reg_was_0);
      return (AS2 (ldi,%A0,lo8(%1)) CR_TAB
	      AS2 (ldi,%B0,hi8(%1)));
    case 3: /* mov r,m*/
      return out_movhi_r_mr (insn, operands, NULL);
    case 4: /* mov m,r*/
        {
          rtx save1=NULL;
          if (operands[1] == const0_rtx)
            {
              save1 = operands[1];
              operands[1] = zero_reg_rtx;
            }
          output_asm_insn (out_movhi_mr_r (insn,operands,NULL), operands);
          if (save1)
            operands[1] = save1;
        }
        return \"\";
    }
}"
  [(set_attr "length" "2,2,2,4,4")
   (set_attr "cc" "none,set_zn,none,clobber,clobber")])

;;==========================================================================
;; move double word (32 bit)

(define_expand "movsi"
  [(set (match_operand:SI 0 "nonimmediate_operand" "")
        (match_operand:SI 1 "general_operand"  ""))]
  ""
  "
{
  /* One of the ops has to be in a register.  */
  if (!register_operand (operand0, SImode)
      && !(register_operand (operand1, SImode) || const0_rtx == operand1))
    {
      operands[1] = copy_to_mode_reg (SImode, operand1);
    }
}")

(define_insn "*movsi"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=r,r,d,r,Qm")
        (match_operand:SI 1 "general_operand"      "r,L,i,Qm,rL"))]
  "(register_operand (operands[0],SImode)
    || register_operand (operands[1],SImode) || const0_rtx == operands[1])"
  "* return output_movsisf (insn, operands, which_alternative);"
  [(set_attr "length" "4,4,4,8,8")
   (set_attr "cc" "none,set_zn,none,clobber,clobber")])

;; fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
;; move floating point numbers (32 bit)

(define_expand "movsf"
  [(set (match_operand:SF 0 "nonimmediate_operand" "")
        (match_operand:SF 1 "general_operand"  ""))]
  ""
  "
{
  /* One of the ops has to be in a register.  */
  if (!register_operand (operand1, SFmode)
      && !register_operand (operand0, SFmode))
    {
      operands[1] = copy_to_mode_reg (SFmode, operand1);
    }
}")

(define_insn "*movsf"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=r,r,d,r,Qm")
        (match_operand:SF 1 "general_operand"      "r,G,F,Qm,r"))]
  "register_operand (operands[0], SFmode)
   || register_operand (operands[1], SFmode)"
  "* return output_movsisf (insn, operands, which_alternative);"
  [(set_attr "length" "4,4,4,8,8")
   (set_attr "cc" "none,set_zn,none,clobber,clobber")])

;;=========================================================================
;; move string (like memcpy)

(define_expand "movstrhi"
  [(parallel [(set (match_operand:BLK 0 "memory_operand" "")
		   (match_operand:BLK 1 "memory_operand" ""))
	      (use (match_operand:HI 2 "const_int_operand" ""))
	      (use (match_operand:HI 3 "const_int_operand" ""))
	      (clobber (match_dup 4))
	      (clobber (match_dup 5))
	      (clobber (match_dup 6))])]
  ""
  "{
  rtx addr0, addr1;
  int cnt8;

  if (GET_CODE (operands[2]) != CONST_INT)
    FAIL;
  cnt8 = byte_immediate_operand (operands[2], GET_MODE (operands[2]));
  operands[2] = copy_to_mode_reg (cnt8 ? QImode : HImode, operands[2]);
  operands[4] = operands[2];

  addr0 = copy_to_mode_reg (Pmode, XEXP (operands[0], 0));
  addr1 = copy_to_mode_reg (Pmode, XEXP (operands[1], 0));

  operands[5] = addr0;
  operands[6] = addr1;

  operands[0] = gen_rtx (MEM, BLKmode, addr0);
  operands[1] = gen_rtx (MEM, BLKmode, addr1);
}")

(define_insn "*movstrqi_insn"
  [(set (mem:BLK (match_operand:HI 0 "register_operand" "e"))
	(mem:BLK (match_operand:HI 1 "register_operand" "e")))
   (use (match_operand:QI 2 "register_operand" "r"))
   (use (match_operand:QI 3 "const_int_operand" "i"))
   (clobber (match_dup 2))
   (clobber (match_dup 0))
   (clobber (match_dup 1))]
  ""
  "
	ld __tmp_reg__,%a1+
	st %a0+,__tmp_reg__
	dec %2
	brne _PC_-8"
  [(set_attr "length" "4")
   (set_attr "cc" "clobber")])

(define_insn "*movstrhi"
  [(set (mem:BLK (match_operand:HI 0 "register_operand" "e,e"))
	(mem:BLK (match_operand:HI 1 "register_operand" "e,e")))
   (use (match_operand:HI 2 "register_operand" "!w,d"))
   (use (match_operand:HI 3 "const_int_operand" ""))
   (clobber (match_dup 2))
   (clobber (match_dup 0))
   (clobber (match_dup 1))]
  ""
  "*{
     if (which_alternative==0)
       return (AS2 (ld,__tmp_reg__,%a1+) CR_TAB
	       AS2 (st,%a0+,__tmp_reg__)  CR_TAB
	       AS2 (sbiw,%A2,1) CR_TAB
	       AS1 (brne,_PC_-8));
     else
       return (AS2 (ld,__tmp_reg__,%a1+) CR_TAB
	       AS2 (st,%a0+,__tmp_reg__)  CR_TAB
	       AS2 (subi,%A2,1) CR_TAB
	       AS2 (sbci,%B2,0) CR_TAB
	       AS1 (brne,_PC_-10));
}"
  [(set_attr "length" "4,5")
   (set_attr "cc" "clobber,clobber")])

;; =0 =0 =0 =0 =0 =0 =0 =0 =0 =0 =0 =0 =0 =0 =0 =0 =0 =0 =0 =0 =0 =0 =0 =0
;; memset (%0, 0, %1)

(define_expand "clrstrhi"
  [(parallel [(set (match_operand:BLK 0 "memory_operand" "")
		   (const_int 0))
	      (use (match_operand:HI 1 "const_int_operand" ""))
	      (use (match_operand:HI 2 "const_int_operand" "n"))
	      (clobber (match_dup 3))
	      (clobber (match_dup 4))])]
  ""
  "{
  rtx addr0;
  int cnt8;

  if (GET_CODE (operands[1]) != CONST_INT)
    FAIL;

  cnt8 = byte_immediate_operand (operands[1], GET_MODE (operands[1]));
  operands[1] = copy_to_mode_reg (cnt8 ? QImode : HImode, operands[1]);
  operands[3] = operands[1];

  addr0 = copy_to_mode_reg (Pmode, XEXP (operands[0], 0));
  operands[4] = addr0;
  
  operands[0] = gen_rtx (MEM, BLKmode, addr0);
}")

(define_insn "*clrstrqi"
  [(set (mem:BLK (match_operand:HI 0 "register_operand" "e"))
	(const_int 0))
   (use (match_operand:QI 1 "register_operand" "r"))
   (use (match_operand:QI 2 "const_int_operand" "n"))
   (clobber (match_dup 1))
   (clobber (match_dup 0))]
  ""
  "
	st %a0+,__zero_reg__
        dec %1
	brne _PC_-6"
  [(set_attr "length" "3")
   (set_attr "cc" "clobber")])

(define_insn "*clrstrhi"
  [(set (mem:BLK (match_operand:HI 0 "register_operand" "e,e"))
	(const_int 0))
   (use (match_operand:HI 1 "register_operand" "!w,d"))
   (use (match_operand:HI 2 "const_int_operand" "n,n"))
   (clobber (match_dup 1))
   (clobber (match_dup 0))]
  ""
  "*{
     if (which_alternative==0)
       return (AS2 (st,%a0+,__zero_reg__) CR_TAB
	       AS2 (sbiw,%A1,1) CR_TAB
	       AS1 (brne,_PC_-6));
     else
       return (AS2 (st,%a0+,__zero_reg__) CR_TAB
	       AS2 (subi,%A1,1) CR_TAB
	       AS2 (sbci,%B1,0) CR_TAB
	       AS1 (brne,_PC_-8));
}"
  [(set_attr "length" "3,4")
   (set_attr "cc" "clobber,clobber")])

(define_expand "strlenhi"
    [(parallel
      [(set (match_dup 4)
	    (unspec:HI [(match_operand:BLK 1 "memory_operand" "")
			(match_operand:QI 2 "const_int_operand" "")
			(match_operand:HI 3 "immediate_operand" "")] 0))
       (clobber (match_dup 6))])
     (set (match_dup 4) (plus:HI (match_dup 4)
				 (const_int -1)))
     (set (match_operand:HI 0 "register_operand" "")
	  (minus:HI (match_dup 4)
		    (match_dup 5)))]
   ""
   "{
  if (! (GET_CODE (operands[2]) == CONST_INT && INTVAL (operands[2]) == 0))
    FAIL;
  operands[6] = copy_to_mode_reg (Pmode, XEXP (operands[1],0));
  operands[1] = gen_rtx (MEM, BLKmode, operands[6]); 
  operands[5] = operands[6];
  operands[4] = gen_reg_rtx (HImode);
}")

(define_insn "*strlenhi"
  [(set (match_operand:HI 0 "register_operand" "=e")
	(unspec:HI [(mem:BLK (match_operand:HI 1 "register_operand" "%0"))
		    (const_int 0)
		    (match_operand:HI 2 "immediate_operand" "i")] 0))
   (clobber (match_dup 1))]
  ""
  "ld __tmp_reg__,%a0+
	tst __tmp_reg__
	brne _PC_-6"
  [(set_attr "length" "3")
   (set_attr "cc" "clobber")])

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; add bytes

(define_insn "addqi3"
  [(set (match_operand:QI 0 "register_operand" "=r,d,r,r")
        (plus:QI (match_operand:QI 1 "register_operand" "%0,0,0,0")
                 (match_operand:QI 2 "nonmemory_operand" "r,i,P,N")))]
  ""
  "@
	add %0,%2
	subi %0,lo8(-(%2))
	inc %0
	dec %0"
  [(set_attr "length" "1,1,1,1")
   (set_attr "cc" "set_czn,set_czn,set_zn,set_zn")])


(define_expand "addhi3"
  [(set (match_operand:HI 0 "register_operand" "")
	(plus:HI (match_operand:HI 1 "register_operand" "")
		 (match_operand:HI 2 "nonmemory_operand" "")))]
  ""
  "
{
  if (GET_CODE (operands[2]) == CONST_INT)
    {
      short tmp = INTVAL (operands[2]);
      operands[2] = GEN_INT(tmp);
    }
 if (! (reload_completed | reload_in_progress))
   {
     if (REGNO (operands[0]) != REGNO (operands[1])
	 && REGNO (operands[0]) != REGNO (operands[2])&&0)
       {
	 emit_move_insn (operands[0], operands[1]);
	 operands[1] = operands[0];
       }
   }
}")


(define_insn "*addhi3_zero_extend"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(plus:HI (zero_extend:HI
		  (match_operand:QI 1 "register_operand" "r"))
		 (match_operand:HI 2 "register_operand" "0")))]
  ""
  "add %A0,%1
	adc %B0,__zero_reg__"
  [(set_attr "length" "2")
   (set_attr "cc" "set_n")])

(define_insn "*addhi3_zero_extend1"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(plus:HI (match_operand:HI 1 "register_operand" "%0")
		 (zero_extend:HI
		  (match_operand:QI 2 "register_operand" "r"))))]
  ""
  "add %A0,%2
	adc %B0,__zero_reg__"
  [(set_attr "length" "2")
   (set_attr "cc" "set_n")])

(define_insn "*addhi3_zero_extend2"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(plus:HI
	 (zero_extend:HI (match_operand:QI 1 "register_operand" "%0"))
	 (zero_extend:HI (match_operand:QI 2 "register_operand" "r"))))]
  ""
  "add %0,%2
	mov %B0,__zero_reg__
	adc %B0,__zero_reg__"
  [(set_attr "length" "3")
   (set_attr "cc" "set_n")])

(define_insn "*addhi3"
  [(set (match_operand:HI 0 "register_operand" "=r,!w,!w,d,r,r")
 	(plus:HI
 	 (match_operand:HI 1 "register_operand" "%0,0,0,0,0,0")
 	 (match_operand:HI 2 "nonmemory_operand" "r,I,J,i,P,N")))]
  ""
  "@
 	add %A0,%A2\;adc %B0,%B2
 	adiw %A0,%2
 	sbiw %A0,%n2
 	subi %A0,lo8(-(%2))\;sbci %B0,hi8(-(%2))
 	sec\;adc %A0,__zero_reg__\;adc %B0,__zero_reg__
 	sec\;sbc %A0,__zero_reg__\;sbc %B0,__zero_reg__"
  [(set_attr "length" "2,1,1,2,3,3")
   (set_attr "cc" "set_n,set_czn,set_czn,set_czn,set_n,set_n")])

(define_insn "addsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,!w,!w,d,r,r,&*!w,&*!w")
	  (plus:SI
	   (match_operand:SI 1 "register_operand" "%0,0,0,0,0,0,r,r")
	   (match_operand:SI 2 "nonmemory_operand" "r,I,J,i,P,N,#I,#J")))]
  ""
  "@
	add %A0,%A2\;adc %B0,%B2\;adc %C0,%C2\;adc %D0,%D2
	adiw %0,%2\;adc %C0,__zero_reg__\;adc %D0,__zero_reg__
	sbiw %0,%n2\;sbc %C0,__zero_reg__\;sbc %D0,__zero_reg__
	subi %0,lo8(-(%2))\;sbci %B0,hi8(-(%2))\;sbci %C0,hlo8(-(%2))\;sbci %D0,hhi8(-(%2))
	sec\;adc %A0,__zero_reg__\;adc %B0,__zero_reg__\;adc %C0,__zero_reg__\;adc %D0,__zero_reg__
	sec\;sbc %A0,__zero_reg__\;sbc %B0,__zero_reg__\;sbc %C0,__zero_reg__\;sbc %D0,__zero_reg__
	mov %A0,%A1\;mov %B0,%B1\;mov %C0,%C1\;mov %D0,%D1\;adiw %0,%2\;adc %C0,__zero_reg__\;adc %D0,__zero_reg__
	mov %A0,%A1\;mov %B0,%B1\;mov %C0,%C1\;mov %D0,%D1\;sbiw %0,%n2\;sbc %C0,__zero_reg__\;sbc %D0,__zero_reg__"
  [(set_attr "length" "4,3,3,4,5,5,7,7")
   (set_attr "cc" "set_n,set_n,set_czn,set_czn,set_n,set_n,set_n,set_czn")])

;-----------------------------------------------------------------------------
; sub bytes
(define_insn "subqi3"
  [(set (match_operand:QI 0 "register_operand" "=r,d")
        (minus:QI (match_operand:QI 1 "register_operand" "0,0")
                  (match_operand:QI 2 "nonmemory_operand" "r,i")))]
  ""
  "@
	sub %0,%2
	subi %0,lo8(%2)"
  [(set_attr "length" "1,1")
   (set_attr "cc" "set_czn,set_czn")])

(define_insn "subhi3"
  [(set (match_operand:HI 0 "register_operand" "=r,d")
        (minus:HI (match_operand:HI 1 "register_operand" "0,0")
		  (match_operand:HI 2 "nonmemory_operand" "r,i")))]
  ""
  "@
	sub %A0,%A2\;sbc %B0,%B2
	subi %A0,lo8(%2)\;sbci %B0,hi8(%2)"
  [(set_attr "length" "2,2")
   (set_attr "cc" "set_czn,set_czn")])

(define_insn "subsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,d")
        (minus:SI (match_operand:SI 1 "register_operand" "0,0")
                 (match_operand:SI 2 "nonmemory_operand" "r,i")))]
  ""
  "@
	sub %0,%2\;sbc %B0,%B2\;sbc %C0,%C2\;sbc %D0,%D2
	subi %A0,lo8(%2)\;sbci %B0,hi8(%2)\;sbci %C0,hlo8(%2)\;sbci %D0,hhi8(%2)"
  [(set_attr "length" "4,4")
   (set_attr "cc" "set_czn,set_czn")])

;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
; and

(define_insn "andqi3"
  [(set (match_operand:QI 0 "register_operand" "=r,d")
        (and:QI (match_operand:QI 1 "register_operand" "%0,0")
                (match_operand:QI 2 "nonmemory_operand" "r,i")))]
  ""
  "@
	and %0,%2
	andi %0,lo8(%2)"
  [(set_attr "length" "1,1")
   (set_attr "cc" "set_zn,set_zn")])

(define_insn "andhi3"
  [(set (match_operand:HI 0 "register_operand" "=r,d,r")
	  (and:HI (match_operand:HI 1 "register_operand" "%0,0,0")
		  (match_operand:HI 2 "nonmemory_operand" "r,i,M")))
   (clobber (match_scratch:QI 3 "=X,X,&d"))]
  ""
  "*{
  if (which_alternative==0)
    return (AS2 (and,%A0,%A2) CR_TAB
	    AS2 (and,%B0,%B2));
  else if (which_alternative==1)
    {
      if (GET_CODE (operands[2]) == CONST_INT)
        {
	  int mask = INTVAL (operands[2]);
	  if ((mask & 0xff) != 0xff)
	    output_asm_insn (AS2 (andi,%A0,lo8(%2)), operands);
	  if ((mask & 0xff00) != 0xff00)
	    output_asm_insn (AS2 (andi,%B0,hi8(%2)), operands);
	  return \"\";
        }
        return (AS2 (andi,%A0,lo8(%2)) CR_TAB
	        AS2 (andi,%B0,hi8(%2)));
     }
  return (AS2 (ldi,%3,lo8(%2)) CR_TAB
          AS2 (and,%A0,%3)     CR_TAB
          AS1 (clr,%B0));
}"
  [(set_attr "length" "2,2,3")
   (set_attr "cc" "set_n,clobber,clobber")])

(define_insn "andsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,d")
	(and:SI (match_operand:SI 1 "register_operand" "%0,0")
		(match_operand:SI 2 "nonmemory_operand" "r,i")))]
  ""
  "*{
  if (which_alternative==0)
    return (AS2 (and, %0,%2)   CR_TAB
	    AS2 (and, %B0,%B2) CR_TAB
	    AS2 (and, %C0,%C2) CR_TAB
	    AS2 (and, %D0,%D2));
  else if (which_alternative==1)
    {
      if (GET_CODE (operands[2]) == CONST_INT)
        {
	  HOST_WIDE_INT mask = INTVAL (operands[2]);
	  if ((mask & 0xff) != 0xff)
	    output_asm_insn (AS2 (andi,%A0,lo8(%2)), operands);
	  if ((mask & 0xff00) != 0xff00)
	    output_asm_insn (AS2 (andi,%B0,hi8(%2)), operands);
	  if ((mask & 0xff0000UL) != 0xff0000UL)
	    output_asm_insn (AS2 (andi,%C0,hlo8(%2)), operands);
	  if ((mask & 0xff000000UL) != 0xff000000UL)
	    output_asm_insn (AS2 (andi,%D0,hhi8(%2)), operands);
	  return \"\";
        }
      return (AS2 (andi, %A0,lo8(%2))  CR_TAB
              AS2 (andi, %B0,hi8(%2)) CR_TAB
	      AS2 (andi, %C0,hlo8(%2)) CR_TAB
	      AS2 (andi, %D0,hhi8(%2)));
      }
}"
  [(set_attr "length" "4,4")
   (set_attr "cc" "set_n,set_n")])

;;|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
;; ior

(define_insn "iorqi3"
  [(set (match_operand:QI 0 "register_operand" "=r,d")
        (ior:QI (match_operand:QI 1 "register_operand" "%0,0")
                (match_operand:QI 2 "nonmemory_operand" "r,i")))]
  ""
  "@
	or %0,%2
	ori %0,lo8(%2)"
  [(set_attr "length" "1,1")
   (set_attr "cc" "set_zn,set_zn")])

(define_insn "iorhi3"
  [(set (match_operand:HI 0 "register_operand" "=r,d")
	(ior:HI (match_operand:HI 1 "register_operand" "%0,0")
		(match_operand:HI 2 "nonmemory_operand" "r,i")))]
  ""
  "*{
  if (which_alternative==0)
    return (AS2 (or,%A0,%A2) CR_TAB
	    AS2 (or,%B0,%B2));
  if (GET_CODE (operands[2]) == CONST_INT)
     {
	int mask = INTVAL (operands[2]);
	if (mask & 0xff)
	  output_asm_insn (AS2 (ori,%A0,lo8(%2)), operands);
	if (mask & 0xff00)
	  output_asm_insn (AS2 (ori,%B0,hi8(%2)), operands);
	return \"\";
      }
   return (AS2 (ori,%0,lo8(%2)) CR_TAB
	   AS2 (ori,%B0,hi8(%2)));
}"  
  [(set_attr "length" "2,2")
   (set_attr "cc" "set_n,clobber")])

(define_insn "*iorhi3_clobber"
  [(set (match_operand:HI 0 "register_operand" "=r,r")
	(ior:HI (match_operand:HI 1 "register_operand" "%0,0")
		(match_operand:HI 2 "immediate_operand" "M,i")))
   (clobber (match_scratch:QI 3 "=&d,&d"))]
  ""
  "@
	ldi %3,lo8(%2)\;or %A0,%3
	ldi %3,lo8(%2)\;or %A0,%3\;ldi %3,lo8(%2)\;or %B0,%3"
  [(set_attr "length" "2,4")
   (set_attr "cc" "clobber,set_n")])

(define_insn "iorsi3"
  [(set (match_operand:SI 0 "register_operand"        "=r,d")
	(ior:SI (match_operand:SI 1 "register_operand" "%0,0")
		(match_operand:SI 2 "nonmemory_operand" "r,i")))]
  ""
  "*{
  if (which_alternative==0)
    return (AS2 (or, %0,%2)   CR_TAB
	    AS2 (or, %B0,%B2) CR_TAB
	    AS2 (or, %C0,%C2) CR_TAB
	    AS2 (or, %D0,%D2));
  if (GET_CODE (operands[2]) == CONST_INT)
     {
	HOST_WIDE_INT mask = INTVAL (operands[2]);
	if (mask & 0xff)
	  output_asm_insn (AS2 (ori,%A0,lo8(%2)), operands);
	if (mask & 0xff00)
	  output_asm_insn (AS2 (ori,%B0,hi8(%2)), operands);
	if (mask & 0xff0000UL)
	  output_asm_insn (AS2 (ori,%C0,hlo8(%2)), operands);
	if (mask & 0xff000000UL)
	  output_asm_insn (AS2 (ori,%D0,hhi8(%2)), operands);
	return \"\";
      }
  return (AS2 (ori, %A0,lo8(%2))  CR_TAB
	  AS2 (ori, %B0,hi8(%2)) CR_TAB
	  AS2 (ori, %C0,hlo8(%2)) CR_TAB
	  AS2 (ori, %D0,hhi8(%2)));
}"
  [(set_attr "length" "4,4")
   (set_attr "cc" "set_n,clobber")])

(define_insn "*iorsi3_clobber"
  [(set (match_operand:SI 0 "register_operand"        "=r,r")
	(ior:SI (match_operand:SI 1 "register_operand" "%0,0")
		(match_operand:SI 2 "immediate_operand" "M,i")))
   (clobber (match_scratch:QI 3 "=&d,&d"))]
  ""
  "@
	ldi %3,lo8(%2)\;or %A0,%3
	ldi %3,lo8(%2)\;or %A0,%3\;ldi %3,hi8(%2)\;or %B0,%3\;ldi %3,hlo8(%2)\;or %C0,%3\;ldi %3,hhi8(%2)\;or %D0,%3"
  [(set_attr "length" "2,8")
   (set_attr "cc" "clobber,set_n")])

;;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;; xor

(define_insn "xorqi3"
  [(set (match_operand:QI 0 "register_operand" "=r")
        (xor:QI (match_operand:QI 1 "register_operand" "%0")
                (match_operand:QI 2 "register_operand" "r")))]
  ""
  "eor %0,%2"
  [(set_attr "length" "1")
   (set_attr "cc" "set_zn")])

(define_insn "xorhi3"
  [(set (match_operand:HI 0 "register_operand" "=r")
        (xor:HI (match_operand:HI 1 "register_operand" "%0")
                (match_operand:HI 2 "register_operand" "r")))]
  ""
  "eor %0,%2\;eor %B0,%B2"
  [(set_attr "length" "2")
   (set_attr "cc" "set_n")])

(define_insn "xorsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (xor:SI (match_operand:SI 1 "register_operand" "%0")
                (match_operand:SI 2 "register_operand" "r")))]
  ""
  "eor %0,%2
	eor %B0,%B2
	eor %C0,%C2
	eor %D0,%D2"
  [(set_attr "length" "4")
   (set_attr "cc" "set_n")])

;;<< << << << << << << << << << << << << << << << << << << << << << << << << <<
;; arithmetic shift left

(define_insn "ashlqi3"
  [(set (match_operand:QI 0 "register_operand" "=r,!d,r,r")
	(ashift:QI (match_operand:QI 1 "register_operand" "0,0,0,0")
		   (match_operand:QI 2 "general_operand" "r,i,i,Qm")))]
  ""
  "* return ashlqi3_out (insn, operands, NULL);"
  [(set_attr "length" "6,4,6,7")
   (set_attr "cc" "clobber,set_czn,set_czn,clobber")])

(define_insn "ashlhi3"
  [(set (match_operand:HI 0 "register_operand"           "=r,r,r,r,r,r")
	(ashift:HI (match_operand:HI 1 "register_operand" "0,0,r,0,0,0")
		   (match_operand:QI 2 "general_operand"  "r,P,O,K,i,Qm")))
   (clobber (match_scratch:QI 3 "=X,X,X,X,&d,X"))]
  ""
  "* return ashlhi3_out (insn,operands, NULL);"
  [(set_attr "length" "7,2,4,2,5,8")
   (set_attr "cc" "clobber,clobber,clobber,clobber,clobber,clobber")])

(define_insn "ashlsi3"
  [(set (match_operand:SI 0 "register_operand"           "=r,r,r,r,r")
	(ashift:SI (match_operand:SI 1 "register_operand" "0,0,r,0,0")
		   (match_operand:QI 2 "general_operand"  "r,P,O,i,Qm")))
   (clobber (match_scratch:QI 3 "=X,X,X,&d,X"))]
  ""
  "* return ashlsi3_out (insn,operands, NULL);"
  [(set_attr "length" "9,4,4,7,10")
   (set_attr "cc" "clobber,clobber,clobber,clobber,clobber")])

;; >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >>
;; arithmetic shift right

(define_insn "ashrqi3"
  [(set (match_operand:QI 0 "register_operand" "=r,r,r,r,r")
	(ashiftrt:QI (match_operand:QI 1 "register_operand" "0,0,0,0,0")
		     (match_operand:QI 2 "general_operand" "r,P,K,i,Qm")))
   (clobber (match_scratch:QI 3 "=X,X,X,&d,X"))]
  ""
  "* return ashrqi3_out (insn,operands, NULL);"
  [(set_attr "length" "6,1,2,4,7")
   (set_attr "cc" "clobber,clobber,clobber,clobber,clobber")])

(define_insn "ashrhi3"
  [(set (match_operand:HI 0 "register_operand"             "=r,r,r,r,r,r")
	(ashiftrt:HI (match_operand:HI 1 "register_operand" "0,0,0,r,0,0")
		     (match_operand:QI 2 "general_operand"  "r,P,K,O,i,Qm")))
   (clobber (match_scratch:QI 3 "=X,X,X,X,&d,X"))]
  ""
  "* return ashrhi3_out (insn,operands, NULL);"
  [(set_attr "length" "7,2,4,2,5,8")
   (set_attr "cc" "clobber,clobber,clobber,clobber,clobber,clobber")])

(define_insn "ashrsi3"
  [(set (match_operand:SI 0 "register_operand"             "=r,r,r,r,r")
	(ashiftrt:SI (match_operand:SI 1 "register_operand" "0,0,r,0,0")
		     (match_operand:QI 2 "general_operand"  "r,P,O,i,Qm")))
   (clobber (match_scratch:QI 3 "=X,X,X,&d,X"))]
  ""
  "* return ashrsi3_out (insn,operands, NULL);"
  [(set_attr "length" "9,4,6,7,10")
   (set_attr "cc" "clobber,clobber,clobber,clobber,clobber")])

;; >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >>
;; logical shift right

(define_insn "lshrqi3"
  [(set (match_operand:QI 0 "register_operand" "=r,d,r,r")
	(lshiftrt:QI (match_operand:QI 1 "register_operand" "0,0,0,0")
		     (match_operand:QI 2 "general_operand" "r,i,i,Qm")))]
  ""
  "* return lshrqi3_out (insn,operands, NULL);"
  [(set_attr "length" "6,4,6,7")
   (set_attr "cc" "clobber,set_czn,set_czn,clobber")])

(define_insn "lshrhi3"
  [(set (match_operand:HI 0 "register_operand"             "=r,r,r,r,r,r")
	(lshiftrt:HI (match_operand:HI 1 "register_operand" "0,0,0,r,0,0")
		     (match_operand:QI 2 "general_operand"  "r,P,K,O,i,Qm")))
   (clobber (match_scratch:QI 3 "=X,X,X,X,&d,X"))]
  ""
  "* return lshrhi3_out (insn,operands, NULL);"
  [(set_attr "length" "7,2,4,2,5,8")
   (set_attr "cc" "clobber,clobber,clobber,clobber,clobber,clobber")])

(define_insn "lshrsi3"
  [(set (match_operand:SI 0 "register_operand"             "=r,r,r,r,r")
	(lshiftrt:SI (match_operand:SI 1 "register_operand" "0,0,r,0,0")
		     (match_operand:QI 2 "general_operand"  "r,P,O,i,Qm")))
   (clobber (match_scratch:QI 3 "=X,X,X,&d,X"))]
  ""
  "* return lshrsi3_out (insn,operands, NULL);"
  [(set_attr "length" "9,4,4,7,10")
   (set_attr "cc" "clobber,clobber,clobber,clobber,clobber")])

;; abs(x) abs(x) abs(x) abs(x) abs(x) abs(x) abs(x) abs(x) abs(x) abs(x) abs(x)
;; abs

(define_insn "absqi2"
  [(set (match_operand:QI 0 "register_operand" "=r")
        (abs:QI (match_operand:QI 1 "register_operand" "0")))]
  ""
  "sbrc %0,7\;neg %0"
  [(set_attr "length" "2")
   (set_attr "cc" "clobber")])


(define_insn "abssf2"
  [(set (match_operand:SF 0 "register_operand" "=d,r")
        (abs:SF (match_operand:SF 1 "register_operand" "0,0")))]
  ""
  "@
	andi %D0,0x7f
	clt\;bld %D0,7"
  [(set_attr "length" "1,2")
   (set_attr "cc" "clobber,clobber")])

;; 0 - x  0 - x  0 - x  0 - x  0 - x  0 - x  0 - x  0 - x  0 - x  0 - x  0 - x
;; neg

(define_insn "negqi2"
  [(set (match_operand:QI 0 "register_operand" "=r")
        (neg:QI (match_operand:QI 1 "register_operand" "0")))]
  ""
  "neg %0"
  [(set_attr "length" "1")
   (set_attr "cc" "set_zn")])

(define_insn "neghi2"
  [(set (match_operand:HI 0 "register_operand" "=!d,r")
	(neg:HI (match_operand:HI 1 "register_operand" "0,0")))]
  ""
  "@
	com %B0\;neg %A0\;sbci %B0,lo8(-1)
	com %B0\;neg %A0\;sbc %B0,__zero_reg__\;inc %B0"
  [(set_attr "length" "3,4")
   (set_attr "cc" "set_czn,set_n")])

(define_insn "negsi2"
  [(set (match_operand:SI 0 "register_operand" "=!d,r")
	(neg:SI (match_operand:SI 1 "register_operand" "0,0")))]
  ""
  "@
	com %D0\;com %C0\;com %B0\;neg %A0\;sbci %B0,lo8(-1)\;sbci %C0,lo8(-1)\;sbci %D0,lo8(-1)
	com %D0\;com %C0\;com %B0\;neg %A0\;brcs _PC_+8\;sec\;adc %B0,__zero_reg__\;adc %C0,__zero_reg__\;adc %D0,__zero_reg__"
  [(set_attr "length" "7,9")
   (set_attr "cc" "set_czn,clobber")])

;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;; not

(define_insn "one_cmplqi2"
  [(set (match_operand:QI 0 "register_operand" "=r")
        (not:QI (match_operand:QI 1 "register_operand" "0")))]
  ""
  "com %0"
  [(set_attr "length" "1")
   (set_attr "cc" "set_czn")])

(define_insn "one_cmplhi2"
  [(set (match_operand:HI 0 "register_operand" "=r")
        (not:HI (match_operand:HI 1 "register_operand" "0")))]
  ""
  "com %0\;com %B0"
  [(set_attr "length" "2")
   (set_attr "cc" "set_n")])

(define_insn "one_cmplsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (not:SI (match_operand:SI 1 "register_operand" "0")))]
  ""
  "com %0\;com %B0\;com %C0\;com %D0"
  [(set_attr "length" "4")
   (set_attr "cc" "set_n")])

;; xx<---x xx<---x xx<---x xx<---x xx<---x xx<---x xx<---x xx<---x xx<---x
;; sign extend

(define_insn "extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "=r,r")
        (sign_extend:HI (match_operand:QI 1 "register_operand" "0,*r")))]
  ""
  "@
	clr %B0\;sbrc %0,7\;com %B0
	mov %A0,%A1\;clr %B0\;sbrc %A0,7\;com %B0"
  [(set_attr "length" "3,4")
   (set_attr "cc" "set_n,set_n")])

(define_insn "extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
        (sign_extend:SI (match_operand:QI 1 "register_operand" "0,*r")))]
  ""
  "@
	clr %B0\;sbrc %A0,7\;com %B0\;mov %C0,%B0\;mov %D0,%B0
	mov %A0,%A1\;clr %B0\;sbrc %A0,7\;com %B0\;mov %C0,%B0\;mov %D0,%B0"
  [(set_attr "length" "5,6")
   (set_attr "cc" "clobber,clobber")])

(define_insn "extendhisi2"
  [(set (match_operand:SI 0 "register_operand"               "=r,&r")
        (sign_extend:SI (match_operand:HI 1 "register_operand" "0,*r")))]
  ""
  "@
	clr %C0\;sbrc %B0,7\;com %C0\;mov %D0,%C0
	mov %A0,%A1\;mov %B0,%B1\;clr %C0\;sbrc %B0,7\;com %C0\;mov %D0,%C0"
  [(set_attr "length" "4,6")
   (set_attr "cc" "clobber,clobber")])

;; xx<---x xx<---x xx<---x xx<---x xx<---x xx<---x xx<---x xx<---x xx<---x
;; zero extend

(define_insn "zero_extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "=r,r")
        (zero_extend:HI (match_operand:QI 1 "register_operand" "0,*r")))]
  ""
  "@
	clr %B0
	mov %A0,%A1\;clr %B0"
  [(set_attr "length" "1,2")
   (set_attr "cc" "set_n,set_n")])

(define_insn "zero_extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
        (zero_extend:SI (match_operand:QI 1 "register_operand" "0,*r")))]
  ""
  "@
	clr %B0\;clr %C0\;clr %D0
	mov %A0,%A1\;clr %B0\;clr %C0\;clr %D0"
  [(set_attr "length" "3,4")
   (set_attr "cc" "set_n,set_n")])

(define_insn "zero_extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "=r,&r")
        (zero_extend:SI (match_operand:HI 1 "register_operand" "0,*r")))]
  ""
  "@
	clr %C0\;clr %D0
	mov %A0,%A1\;mov %B0,%B1\;clr %C0\;clr %D0"
  [(set_attr "length" "2,4")
   (set_attr "cc" "set_n,set_n")])

;;<=><=><=><=><=><=><=><=><=><=><=><=><=><=><=><=><=><=><=><=><=><=><=><=><=>
;; compare

(define_insn "tstqi"
  [(set (cc0)
        (match_operand:QI 0 "register_operand" "r"))]
  ""
  "tst %0"
  [(set_attr "cc" "compare")
   (set_attr "length" "1")])

(define_insn "*negated_tstqi"
  [(set (cc0)
        (neg:QI (match_operand:QI 0 "register_operand" "r")))]
  ""
  "cp __zero_reg__,%0"
  [(set_attr "cc" "compare")
   (set_attr "length" "1")])

(define_insn "tsthi"
  [(set (cc0)
        (match_operand:HI 0 "register_operand" "!w,r"))]
  ""
  "* return out_tsthi (insn,NULL);"
[(set_attr "cc" "compare,compare")
 (set_attr "length" "1,2")])

(define_insn "*negated_tsthi"
  [(set (cc0)
        (neg:HI (match_operand:HI 0 "register_operand" "r")))]
  ""
  "cp __zero_reg__,%A0
	cpc __zero_reg__,%B0"
[(set_attr "cc" "compare")
 (set_attr "length" "2")])

(define_insn "tstsi"
  [(set (cc0)
        (match_operand:SI 0 "register_operand" "r"))]
  ""
  "* return out_tstsi (insn,NULL);"
  [(set_attr "cc" "compare")
   (set_attr "length" "4")])

(define_insn "*negated_tstsi"
  [(set (cc0)
        (neg:SI (match_operand:SI 0 "register_operand" "r")))]
  ""
  "cp __zero_reg__,%A0
	cpc __zero_reg__,%B0
	cpc __zero_reg__,%C0
	cpc __zero_reg__,%D0"
  [(set_attr "cc" "compare")
   (set_attr "length" "4")])


(define_insn "cmpqi"
  [(set (cc0)
        (compare (match_operand:QI 0 "register_operand"  "r,d")
		 (match_operand:QI 1 "nonmemory_operand" "r,i")))]
  ""
  "@
	cp %0,%1
	cpi %0,lo8(%1)"
  [(set_attr "cc" "compare,compare")
   (set_attr "length" "1,1")])

(define_insn "*cmpqi_sign_extend"
  [(set (cc0)
        (compare (sign_extend:HI
		  (match_operand:QI 0 "register_operand"  "d"))
		 (match_operand:HI 1 "immediate_operand" "M")))]
  ""
  "cpi %0,lo8(%1)"
  [(set_attr "cc" "compare")
   (set_attr "length" "1")])

(define_insn "cmphi"
  [(set (cc0)
	(compare (match_operand:HI 0 "register_operand"  "r,d,d,r,r")
		 (match_operand:HI 1 "nonmemory_operand" "r,M,i,M,i")))
   (clobber (match_scratch:QI 2 "=X,X,&d,&d,&d"))]
  ""
  "*{
  switch (which_alternative)
    {
    case 0:
      return (AS2 (cp,%A0,%A1) CR_TAB
              AS2 (cpc,%B0,%B1));
    case 1:
      if (reg_unused_after (insn, operands[0])
          && INTVAL (operands[1]) >= 0 && INTVAL (operands[1]) <= 63
          && TEST_HARD_REG_CLASS (ADDW_REGS, true_regnum (operands[0])))
        return AS2 (sbiw,%0,%1);
       else
        return (AS2 (cpi,%0,%1) CR_TAB
                AS2 (cpc,%B0,__zero_reg__));
    case 2:
      if (reg_unused_after (insn, operands[0]))
        return (AS2 (subi,%0,lo8(%1))  CR_TAB
                AS2 (sbci,%B0,hi8(%1)));
      else
        return (AS2 (ldi, %2,hi8(%1))  CR_TAB
	        AS2 (cpi, %A0,lo8(%1)) CR_TAB
	        AS2 (cpc, %B0,%2));
   case 3:
      return (AS2 (ldi, %2,lo8(%1))  CR_TAB
	      AS2 (cp, %A0,%2) CR_TAB
	      AS2 (cpc, %B0,__zero_reg__));

   case 4:
      return (AS2 (ldi, %2,lo8(%1))  CR_TAB
              AS2 (cp, %A0,%2)       CR_TAB
              AS2 (ldi, %2,hi8(%1)) CR_TAB
	      AS2 (cpc, %B0,%2));
    }
}" 
  [(set_attr "cc" "compare,compare,compare,compare,compare")
   (set_attr "length" "2,2,3,3,4")])


(define_insn "cmpsi"
  [(set (cc0)
	(compare (match_operand:SI 0 "register_operand"  "r,d,d,r,r")
		 (match_operand:SI 1 "nonmemory_operand" "r,M,i,M,i")))
   (clobber (match_scratch:QI 2 "=X,X,&d,&d,&d"))]
  ""
  "*{
  switch (which_alternative)
    {
    case 0:
      return (AS2 (cp,%A0,%A1) CR_TAB
              AS2 (cpc,%B0,%B1) CR_TAB
	      AS2 (cpc,%C0,%C1) CR_TAB
	      AS2 (cpc,%D0,%D1));
    case 1:
      if (reg_unused_after (insn, operands[0])
          && INTVAL (operands[1]) >= 0 && INTVAL (operands[1]) <= 63
          && TEST_HARD_REG_CLASS (ADDW_REGS, true_regnum (operands[0])))
        return (AS2 (sbiw,%0,%1) CR_TAB
                AS2 (cpc,%C0,__zero_reg__) CR_TAB
                AS2 (cpc,%D0,__zero_reg__));
      else
        return (AS2 (cpi,%A0,lo8(%1))  CR_TAB
                AS2 (cpc,%B0,__zero_reg__) CR_TAB
                AS2 (cpc,%C0,__zero_reg__) CR_TAB
                AS2 (cpc,%D0,__zero_reg__));
    case 2:
      if (reg_unused_after (insn, operands[0]))
        return (AS2 (subi,%A0,lo8(%1))  CR_TAB
                AS2 (sbci,%B0,hi8(%1))  CR_TAB
                AS2 (sbci,%C0,hlo8(%1))  CR_TAB
                AS2 (sbci,%D0,hhi8(%1)));
      else
       return (AS2 (cpi, %A0,lo8(%1))   CR_TAB
	       AS2 (ldi, %2,hi8(%1))  CR_TAB
	       AS2 (cpc, %B0,%2)       CR_TAB
	       AS2 (ldi, %2,hlo8(%1))  CR_TAB
	       AS2 (cpc, %C0,%2)       CR_TAB
	       AS2 (ldi, %2,hhi8(%1)) CR_TAB
	       AS2 (cpc, %D0,%2));
    case 3:
        return (AS2 (ldi,%2,lo8(%1))        CR_TAB
                AS2 (cp,%A0,%2)            CR_TAB
                AS2 (cpc,%B0,__zero_reg__) CR_TAB
                AS2 (cpc,%C0,__zero_reg__) CR_TAB
                AS2 (cpc,%D0,__zero_reg__));
    case 4:
       return (AS2 (ldi, %2,lo8(%1))   CR_TAB
               AS2 (cp, %A0,%2)        CR_TAB
	       AS2 (ldi, %2,hi8(%1))  CR_TAB
	       AS2 (cpc, %B0,%2)       CR_TAB
	       AS2 (ldi, %2,hlo8(%1))  CR_TAB
	       AS2 (cpc, %C0,%2)       CR_TAB
	       AS2 (ldi, %2,hhi8(%1)) CR_TAB
	       AS2 (cpc, %D0,%2));
   }
}"
  [(set_attr "cc" "compare,compare,compare,compare,compare")
   (set_attr "length" "4,4,7,5,8")])

;; ----------------------------------------------------------------------
;; JUMP INSTRUCTIONS
;; ----------------------------------------------------------------------
;; Conditional jump instructions

(define_expand "beq"
  [(set (pc)
        (if_then_else (eq (cc0) (const_int 0))
                      (label_ref (match_operand 0 "" ""))
                      (pc)))]
  ""
  "")

(define_expand "bne"
  [(set (pc)
        (if_then_else (ne (cc0) (const_int 0))
                      (label_ref (match_operand 0 "" ""))
                      (pc)))]
  ""
  "")

(define_expand "bge"
  [(set (pc)
        (if_then_else (ge (cc0) (const_int 0))
                      (label_ref (match_operand 0 "" ""))
                      (pc)))]
  ""
  "")

(define_expand "bgeu"
  [(set (pc)
        (if_then_else (geu (cc0) (const_int 0))
                      (label_ref (match_operand 0 "" ""))
                      (pc)))]
  ""
  "")

(define_expand "blt"
  [(set (pc)
        (if_then_else (lt (cc0) (const_int 0))
                      (label_ref (match_operand 0 "" ""))
                      (pc)))]
  ""
  "")

(define_expand "bltu"
  [(set (pc)
        (if_then_else (ltu (cc0) (const_int 0))
                      (label_ref (match_operand 0 "" ""))
                      (pc)))]
  ""
  "")



/****************************************************************
 AVR not have following conditional jumps: LE,LEU,GT,GTU.
 Convert them all to proper jumps.
*****************************************************************/

(define_expand "ble"
  [(set (pc)
        (if_then_else (le (cc0) (const_int 0))
                      (label_ref (match_operand 0 "" ""))
                      (pc)))]
  ""
  "")

(define_expand "bleu"
  [(set (pc)
        (if_then_else (leu (cc0) (const_int 0))
                      (label_ref (match_operand 0 "" ""))
                      (pc)))]
  ""
  "")

(define_expand "bgt"
  [(set (pc)
        (if_then_else (gt (cc0) (const_int 0))
                      (label_ref (match_operand 0 "" ""))
                      (pc)))]
  ""
  "")

(define_expand "bgtu"
  [(set (pc)
        (if_then_else (gtu (cc0) (const_int 0))
                      (label_ref (match_operand 0 "" ""))
                      (pc)))]
  ""
  "")

(define_insn "*sbrx_branch"
  [(set (pc)
        (if_then_else
	 (match_operator 0 "comparison_operator"
			 [(zero_extract
			   (match_operand:QI 1 "register_operand" "r")
			   (const_int 1)
			   (match_operand 2 "immediate_operand" "n"))
			  (const_int 0)])
	 (label_ref (match_operand 3 "" ""))
	 (pc)))]
  "(GET_CODE (operands[0]) == EQ || GET_CODE (operands[0]) == NE)"
  "* {
       int comp = ((get_attr_length (insn) == 4)
                   ? reverse_condition (GET_CODE (operands[0]))
                   : GET_CODE (operands[0]));
       if (comp == EQ)
         output_asm_insn (AS2 (sbrs,%1,%2), operands);
       else
         output_asm_insn (AS2 (sbrc,%1,%2), operands);
       if (get_attr_length (insn) != 4)
         return AS1 (rjmp,%3);
       return (AS1 (rjmp,_PC_+4) CR_TAB
               AS1 (jmp,%3));
     }"
  [(set (attr "length") (if_then_else (and (ge (minus (pc) (match_dup 3))
					       (const_int -2046))
					   (le (minus (pc) (match_dup 3))
					       (const_int 2046)))
				      (const_int 2)
				      (if_then_else (eq (symbol_ref "AVR_MEGA")
							(const_int 0))
						    (const_int 2)
						    (const_int 4))))
   (set_attr "cc" "clobber")])

(define_insn "*sbrx_and_branchsi"
  [(set (pc)
        (if_then_else
	 (match_operator 0 "comparison_operator"
			 [(and:SI
			   (match_operand:SI 1 "register_operand" "r")
			   (match_operand:SI 2 "immediate_operand" "n"))
			  (const_int 0)])
	 (label_ref (match_operand 3 "" ""))
	 (pc)))]
  "(GET_CODE (operands[0]) == EQ || GET_CODE (operands[0]) == NE)
   && mask_one_bit_p(INTVAL (operands[2]))"
  "* {
       int comp = ((get_attr_length (insn) == 4)
                   ? reverse_condition (GET_CODE (operands[0]))
                   : GET_CODE (operands[0]));
       int bit = mask_one_bit_p(INTVAL (operands[2])) - 1;
       static char buf[] = \"sbrc %A1,0\";
       buf[3] = (comp == EQ ? 's' : 'c');
       buf[6] = bit / 8 + 'A';
       buf[9] = bit % 8 + '0';
       output_asm_insn (buf, operands);

       if (get_attr_length (insn) != 4)
         return AS1 (rjmp,%3);
       return (AS1 (rjmp,_PC_+4) CR_TAB
               AS1 (jmp,%3));
     }"
  [(set (attr "length") (if_then_else (and (ge (minus (pc) (match_dup 3))
					       (const_int -2046))
					   (le (minus (pc) (match_dup 3))
					       (const_int 2046)))
				      (const_int 2)
				      (if_then_else (eq (symbol_ref "AVR_MEGA")
							(const_int 0))
						    (const_int 2)
						    (const_int 4))))
   (set_attr "cc" "clobber")])

(define_insn "*sbrx_and_branchhi"
  [(set (pc)
        (if_then_else
	 (match_operator 0 "comparison_operator"
			 [(and:HI
			   (match_operand:HI 1 "register_operand" "r")
			   (match_operand:HI 2 "immediate_operand" "n"))
			  (const_int 0)])
	 (label_ref (match_operand 3 "" ""))
	 (pc)))]
  "(GET_CODE (operands[0]) == EQ || GET_CODE (operands[0]) == NE)
   && mask_one_bit_p(INTVAL (operands[2]))"
  "* {
       int comp = ((get_attr_length (insn) == 4)
                   ? reverse_condition (GET_CODE (operands[0]))
                   : GET_CODE (operands[0]));
       int bit = mask_one_bit_p(INTVAL (operands[2])) - 1;
       static char buf[] = \"sbrc %A1,0\";
       buf[3] = (comp == EQ ? 's' : 'c');
       buf[6] = bit / 8 + 'A';
       buf[9] = bit % 8 + '0';
       output_asm_insn (buf, operands);

       if (get_attr_length (insn) != 4)
         return AS1 (rjmp,%3);
       return (AS1 (rjmp,_PC_+4) CR_TAB
               AS1 (jmp,%3));
     }"
  [(set (attr "length") (if_then_else (and (ge (minus (pc) (match_dup 3))
					       (const_int -2046))
					   (le (minus (pc) (match_dup 3))
					       (const_int 2046)))
				      (const_int 2)
				      (if_then_else (eq (symbol_ref "AVR_MEGA")
							(const_int 0))
						    (const_int 2)
						    (const_int 4))))
   (set_attr "cc" "clobber")])

;; ************************************************************************
;; Implementation of conditional jumps here.
;;  Compare with 0 (test) jumps
;; ************************************************************************

(define_insn "branch"
  [(set (pc)
        (if_then_else (match_operator 1 "comparison_operator"
                        [(cc0)
                         (const_int 0)])
                      (label_ref (match_operand 0 "" ""))
                      (pc)))]
  "! (GET_CODE (operands[1]) == GT || GET_CODE (operands[1]) == GTU
      || GET_CODE (operands[1]) == LE || GET_CODE (operands[1]) == LEU)"
  "*
   return ret_cond_branch (GET_CODE (operands[1]),
                           avr_jump_mode (operands[0],insn));"
  [(set_attr "type" "branch")
   (set_attr "cc" "clobber")])

(define_insn "difficult_branch"
  [(set (pc)
        (if_then_else (match_operator 1 "comparison_operator"
                        [(cc0)
                         (const_int 0)])
                      (label_ref (match_operand 0 "" ""))
                      (pc)))]
  "(GET_CODE (operands[1]) == GT || GET_CODE (operands[1]) == GTU
    || GET_CODE (operands[1]) == LE || GET_CODE (operands[1]) == LEU)"
  "*
   return ret_cond_branch (GET_CODE (operands[1]),
                           avr_jump_mode (operands[0],insn));"
  [(set_attr "type" "branch1")
   (set_attr "cc" "clobber")])

;; revers branch

(define_insn "rvbranch"
  [(set (pc)
        (if_then_else (match_operator 1 "comparison_operator" [(cc0)
                                                               (const_int 0)])
                      (pc)
                      (label_ref (match_operand 0 "" ""))))]
  "! (GET_CODE (operands[1]) == GT || GET_CODE (operands[1]) == GTU
      || GET_CODE (operands[1]) == LE || GET_CODE (operands[1]) == LEU)"
  "*
   return ret_cond_branch (reverse_condition (GET_CODE (operands[1])),
                           avr_jump_mode (operands[0],insn));"
  [(set_attr "type" "branch1")
   (set_attr "cc" "clobber")])

(define_insn "difficult_rvbranch"
  [(set (pc)
        (if_then_else (match_operator 1 "comparison_operator" [(cc0)
                                                               (const_int 0)])
                      (pc)
                      (label_ref (match_operand 0 "" ""))))]
  "(GET_CODE (operands[1]) == GT || GET_CODE (operands[1]) == GTU
    || GET_CODE (operands[1]) == LE || GET_CODE (operands[1]) == LEU)"
  "*
   return ret_cond_branch (reverse_condition (GET_CODE (operands[1])),
                           avr_jump_mode (operands[0],insn));"
  [(set_attr "type" "branch")
   (set_attr "cc" "clobber")])

;; **************************************************************************
;; Unconditional and other jump instructions.

(define_insn "jump"
  [(set (pc)
        (label_ref (match_operand 0 "" "")))]
  ""
  "*{
  if (AVR_MEGA && get_attr_length (insn) != 1)
    return \"jmp %0\";
  return \"rjmp %0\";
}"
  [(set (attr "length") (if_then_else (and (ge (minus (pc) (match_dup 0))
					       (const_int -2047))
					   (le (minus (pc) (match_dup 0))
					       (const_int 2047)))
				      (const_int 1)
				      (const_int 2)))
   (set_attr "cc" "none")])

;; call

(define_expand "call"
  [(call (match_operand:HI 0 "call_insn_operand" "")
         (match_operand:HI 1 "general_operand" ""))]
  ;; Operand 1 not used on the AVR.
  ""
  "")

;; call value

(define_expand "call_value"
  [(set (match_operand 0 "register_operand" "")
        (call (match_operand:HI 1 "call_insn_operand" "")
              (match_operand:HI 2 "general_operand" "")))]
  ;; Operand 2 not used on the AVR.
  ""
  "")

(define_insn "call_insn"
  [(call (mem:HI (match_operand:HI 0 "nonmemory_operand" "!z,*r,i"))
         (match_operand:HI 1 "general_operand" "X,X,X"))]
;; We don't need in saving Z register because r30,r31 is a call used registers
  ;; Operand 1 not used on the AVR.
  "(register_operand (operands[0], HImode) || CONSTANT_P (operands[0]))"
  "*
{
  if (which_alternative==0)
     return \"icall\";
  else if (which_alternative==1)
     return (AS2 (mov, r30,%A0) CR_TAB
	     AS2 (mov, r31,%B0) CR_TAB
	     \"icall\");
  else if (!AVR_MEGA)
     return AS1(rcall,%c0);   
  return AS1(call,%c0);
}"
  [(set_attr "cc" "clobber,clobber,clobber")
   (set (attr "length")
	(cond [(eq (symbol_ref "which_alternative") (const_int 0))
	       (const_int 1)
	       (eq (symbol_ref "which_alternative") (const_int 0))
	       (const_int 3)
	       (eq (symbol_ref "!AVR_MEGA")
		   (const_int 0))
	       (const_int 2)]
	(const_int 1)))])

(define_insn "call_value_insn"
  [(set (match_operand 0 "register_operand" "=r,r,r")
        (call (mem:HI (match_operand:HI 1 "nonmemory_operand" "!z,*r,i"))
;; We don't need in saving Z register because r30,r31 is a call used registers
              (match_operand:HI 2 "general_operand" "X,X,X")))]
  ;; Operand 2 not used on the AVR.
  "(register_operand (operands[0], VOIDmode) || CONSTANT_P (operands[0]))"
  "*
{
  if (which_alternative==0)
     return \"icall\";
  else if (which_alternative==1)
     return (AS2 (mov, r30,%A1) CR_TAB
	     AS2 (mov, r31,%B1) CR_TAB
	     \"icall\");
  else if (!AVR_MEGA)
     return AS1(rcall,%c1);   
  return AS1(call,%c1);
}"
  [(set_attr "cc" "clobber,clobber,clobber")
   (set (attr "length")
	(cond [(eq (symbol_ref "which_alternative") (const_int 0))
	       (const_int 1)
	       (eq (symbol_ref "which_alternative") (const_int 0))
	       (const_int 3)
	       (eq (symbol_ref "!AVR_MEGA")
		   (const_int 0))
	       (const_int 2)]
	      (const_int 1)))])

(define_insn "nop"
  [(const_int 0)]
  ""
  "nop"
  [(set_attr "cc" "none")
   (set_attr "length" "1")])

; indirect jump
(define_insn "indirect_jump"
  [(set (pc) (match_operand:HI 0 "register_operand" "!z,*r"))]
  ""
  "@
	ijmp
	push %A0\;push %B0\;ret"
  [(set_attr "length" "1,3")
   (set_attr "cc" "none,none")])

;; table jump
(define_expand "tablejump"
  [(parallel [(set (pc) (match_operand:HI 0 "register_operand" ""))
	      (use (label_ref (match_operand 1 "" "")))])]
  "optimize"
  "")

(define_insn "*tablejump"
   [(set (pc) (mem:HI
	       (plus:HI (match_operand:HI 0 "register_operand" "=&z")
			(label_ref (match_operand 2 "" "")))))
    (use (label_ref (match_operand 1 "" "")))]
  ""
  "subi r30,lo8(-(%2))
	sbci r31,hi8(-(%2))
	lpm
	push r0
        adiw r30,1
	lpm
	push r0
        ret"
  [(set_attr "length" "8")
   (set_attr "cc" "clobber")])

(define_expand "casesi"
  [(set (match_dup 6)
	(minus:HI (subreg:HI (match_operand:SI 0 "register_operand" "") 0)
		  (match_operand:HI 1 "register_operand" "")))
   (parallel [(set (cc0)
		   (compare (match_dup 6)
			    (match_operand:HI 2 "register_operand" "")))
	      (clobber (match_scratch:QI 9 ""))])
   
   (set (pc)
	(if_then_else (gtu (cc0)
			   (const_int 0))
		      (label_ref (match_operand 4 "" ""))
		      (pc)))
   (set (match_dup 6)
	(plus:HI (match_dup 6)
		 (match_dup 6)))
;;   (set (match_dup 6)
;;	(plus:HI (match_dup 6) (label_ref (match_operand:HI 3 "" ""))))
		 
   (parallel [(set (pc) (mem:HI
			 (plus:HI (match_dup 6)
				  (label_ref (match_operand:HI 3 "" "")))))
	      (use (label_ref (match_dup 3)))])]
  "!optimize"
  "
{
  operands[6] = gen_reg_rtx (HImode);
}")


;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;; This instructin sets Z flag

(define_insn "sez"
  [(set (cc0) (const_int 0))]
  ""
  "sez"
  [(set_attr "length" "1")
   (set_attr "cc" "compare")])


;; ************************* Peepholes ********************************

(define_peephole
  [(set (match_operand:SI 0 "register_operand" "")
        (plus:SI (match_dup 0)
                 (const_int -1)))
   (parallel
    [(set (cc0)
          (compare (match_dup 0)
		   (const_int -1)))
     (clobber (match_operand:QI 1 "register_operand" ""))])
   (set (pc)
	(if_then_else (ne (cc0) (const_int 0))
		      (label_ref (match_operand 2 "" ""))
		      (pc)))]
  "(test_hard_reg_class (LD_REGS, operands[0])
    && test_hard_reg_class (LD_REGS, operands[1]))"
  "*
{
  if (TEST_HARD_REG_CLASS (ADDW_REGS, true_regnum (operands[0])))
    output_asm_insn (AS2 (sbiw,%0,1) CR_TAB
		     AS2 (sbc,%C0,__zero_reg__) CR_TAB
		     AS2 (sbc,%D0,__zero_reg__) \"\\n\", operands);
  else
    output_asm_insn (AS2 (subi,%A0,1) CR_TAB
		     AS2 (sbc,%B0,__zero_reg__) CR_TAB
		     AS2 (sbc,%C0,__zero_reg__) CR_TAB
		     AS2 (sbc,%D0,__zero_reg__) \"\\n\", operands);
  switch (avr_jump_mode (operands[2],insn))
  {
    case 1:
      return AS1 (brcc,%2);
    case 2:
      return (AS1 (brcs,_PC_+2) CR_TAB
              AS1 (rjmp,%2));
  }
  return (AS1 (brcs,_PC_+4) CR_TAB
          AS1 (jmp,%2));
}")

(define_peephole
  [(set (match_operand:HI 0 "register_operand" "")
        (plus:HI (match_dup 0)
                 (const_int -1)))
   (parallel
    [(set (cc0)
          (compare (match_dup 0)
		   (const_int 65535)))
     (clobber (match_operand:QI 1 "register_operand" ""))])
   (set (pc)
	(if_then_else (ne (cc0) (const_int 0))
		      (label_ref (match_operand 2 "" ""))
		      (pc)))]
  "(test_hard_reg_class (LD_REGS, operands[0])
    && test_hard_reg_class (LD_REGS, operands[1]))"
  "*
{
  if (TEST_HARD_REG_CLASS (ADDW_REGS, true_regnum (operands[0])))
    output_asm_insn (AS2 (sbiw,%0,1), operands);
  else
    output_asm_insn (AS2 (subi,%A0,1) CR_TAB
		     AS2 (sbc,%B0,__zero_reg__) \"\\n\", operands);
  switch (avr_jump_mode (operands[2],insn))
  {
    case 1:
      return AS1 (brcc,%2);
    case 2:
      return (AS1 (brcs,_PC_+2) CR_TAB
              AS1 (rjmp,%2));
  }
  return (AS1 (brcs,_PC_+4) CR_TAB
          AS1 (jmp,%2));
}")

(define_peephole
  [(set (match_operand:QI 0 "register_operand" "")
        (plus:QI (match_dup 0)
                 (const_int -1)))
   (set (cc0)
	(compare (match_dup 0)
		 (const_int -1)))
   (set (pc)
	(if_then_else (ne (cc0) (const_int 0))
		      (label_ref (match_operand 1 "" ""))
		      (pc)))]
  "test_hard_reg_class (LD_REGS, operands[0])"
  "*
{
  output_asm_insn (AS2 (subi,%A0,1), operands);
  switch (avr_jump_mode (operands[1],insn))
  {
    case 1:
      return AS1 (brcc,%1);
    case 2:
      return (AS1 (brcs,_PC_+2) CR_TAB
              AS1 (rjmp,%1));
  }
  return (AS1 (brcs,_PC_+4) CR_TAB
          AS1 (jmp,%1));
}")
					
