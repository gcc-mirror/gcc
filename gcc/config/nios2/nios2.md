;; Machine Description for Altera Nios II.
;; Copyright (C) 2012-2017 Free Software Foundation, Inc.
;; Contributed by Jonah Graham (jgraham@altera.com) and 
;; Will Reece (wreece@altera.com).
;; Contributed by Mentor Graphics, Inc.
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

;; Register numbers
(define_constants
  [
   (FIRST_RETVAL_REGNO     2)	; Return value registers
   (LAST_RETVAL_REGNO      3)	;
   (FIRST_ARG_REGNO        4)	; Argument registers
   (LAST_ARG_REGNO         7)	;

   (TP_REGNO              23)	; Thread pointer register
   (GP_REGNO	          26)	; Global pointer register
   (SP_REGNO	          27)	; Stack pointer register
   (FP_REGNO	          28)	; Frame pointer register
   (EA_REGNO	          29)	; Exception return address register
   (RA_REGNO              31)	; Return address register
   (LAST_GP_REG           31)	; Last general purpose register

   ;; Target register definitions
   (STATIC_CHAIN_REGNUM        12)
   (STACK_POINTER_REGNUM       27)
   (HARD_FRAME_POINTER_REGNUM  28)
   (PC_REGNUM                  37)
   (FRAME_POINTER_REGNUM       38)
   (ARG_POINTER_REGNUM         39)
   (FIRST_PSEUDO_REGISTER      40)
  ]
)

;; Enumeration of UNSPECs

(define_c_enum "unspecv" [
  UNSPECV_BLOCKAGE
  UNSPECV_WRCTL
  UNSPECV_RDCTL
  UNSPECV_FWRX
  UNSPECV_FWRY
  UNSPECV_FRDXLO
  UNSPECV_FRDXHI
  UNSPECV_FRDY
  UNSPECV_CUSTOM_NXX
  UNSPECV_CUSTOM_XNXX
  UNSPECV_LDXIO
  UNSPECV_STXIO
  UNSPECV_RDPRS
  UNSPECV_FLUSHD
  UNSPECV_FLUSHDA
  UNSPECV_WRPIE
  UNSPECV_ENI
  UNSPECV_LDEX
  UNSPECV_LDSEX
  UNSPECV_STEX
  UNSPECV_STSEX
])

(define_c_enum "unspec" [
  UNSPEC_FCOS
  UNSPEC_FSIN
  UNSPEC_FTAN
  UNSPEC_FATAN
  UNSPEC_FEXP
  UNSPEC_FLOG
  UNSPEC_ROUND
  UNSPEC_LOAD_GOT_REGISTER
  UNSPEC_PIC_SYM
  UNSPEC_PIC_CALL_SYM
  UNSPEC_PIC_GOTOFF_SYM
  UNSPEC_LOAD_TLS_IE
  UNSPEC_ADD_TLS_LE
  UNSPEC_ADD_TLS_GD
  UNSPEC_ADD_TLS_LDM
  UNSPEC_ADD_TLS_LDO
  UNSPEC_EH_RETURN
  UNSPEC_SYNC
])


;;  Instruction scheduler

; No schedule info is currently available, using an assumption that no
; instruction can use the results of the previous instruction without
; incuring a stall.

; length of an instruction (in bytes)
(define_attr "length" ""
  (if_then_else (match_test "nios2_cdx_narrow_form_p (insn)")
    (const_int 2)
    (const_int 4)))

(define_attr "type" 
  "unknown,complex,control,alu,cond_alu,st,ld,stwm,ldwm,push,pop,mul,div,\
   custom,add,sub,mov,and,or,xor,neg,not,sll,srl,sra,rol,ror,nop"
  (const_string "complex"))

(define_asm_attributes
 [(set_attr "length" "4")
  (set_attr "type" "complex")])

(define_automaton "nios2")
(automata_option "v")
;(automata_option "no-minimization")
(automata_option "ndfa")

; The nios2 pipeline is fairly straightforward for the fast model.
; Every alu operation is pipelined so that an instruction can
; be issued every cycle.  However, there are still potential
; stalls which this description tries to deal with.

(define_cpu_unit "cpu" "nios2")

(define_insn_reservation "complex" 1
  (eq_attr "type" "complex")
  "cpu")

(define_insn_reservation "control" 1
  (eq_attr "type" "control,pop")
  "cpu")

(define_insn_reservation "alu" 1
  (eq_attr "type" "alu,add,sub,mov,and,or,xor,neg,not")
  "cpu")

(define_insn_reservation "cond_alu" 1
  (eq_attr "type" "cond_alu")
  "cpu")

(define_insn_reservation "st" 1
  (eq_attr "type" "st,stwm,push")
  "cpu")
  
(define_insn_reservation "custom" 1
  (eq_attr "type" "custom")
  "cpu")

; shifts, muls and lds have three cycle latency
(define_insn_reservation "ld" 3
  (eq_attr "type" "ld,ldwm")
  "cpu")

(define_insn_reservation "shift" 3
  (eq_attr "type" "sll,srl,sra,rol,ror")
  "cpu")

(define_insn_reservation "mul" 3
  (eq_attr "type" "mul")
  "cpu")

(define_insn_reservation "div" 1
  (eq_attr "type" "div")
  "cpu")

(include "predicates.md")
(include "constraints.md")


;; Move instructions

(define_mode_iterator M [QI HI SI])

(define_expand "mov<mode>"
  [(set (match_operand:M 0 "nonimmediate_operand" "")
        (match_operand:M 1 "general_operand" ""))]
  ""
{
  if (nios2_emit_move_sequence (operands, <MODE>mode))
    DONE;
})

(define_insn "*high"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (high:SI (match_operand:SI 1 "immediate_operand" "i")))]
  ""
  "movhi\\t%0, %H1"
  [(set_attr "type" "alu")])

(define_insn "*lo_sum"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (lo_sum:SI (match_operand:SI 1 "register_operand"  "r")
                   (match_operand:SI 2 "immediate_operand" "i")))]
  ""
  "addi\\t%0, %1, %L2"
  [(set_attr "type" "alu")])

(define_insn_and_split "movqi_internal"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=m, r,r")
        (match_operand:QI 1 "general_operand"       "rM,m,rI"))]
  "(register_operand (operands[0], QImode)
    || reg_or_0_operand (operands[1], QImode))"
  {
    switch (which_alternative)
      {
      case 0:
	if (get_attr_length (insn) != 2)
	  return "stb%o0\\t%z1, %0";
	else if (const_0_operand (operands[1], QImode))
	  return "stbz.n\\t%z1, %0";
	else
	  return "stb.n\\t%z1, %0";
      case 1:
	return "ldbu%o1%.\\t%0, %1";
      case 2:
	return "mov%i1%.\\t%0, %z1";
      default:
	gcc_unreachable ();
      }
  }
  "(nios2_symbolic_memory_operand_p (operands[0]) 
   || nios2_symbolic_memory_operand_p (operands[1]))"
  [(set (match_dup 0) (match_dup 1))]
  {
    if (nios2_symbolic_memory_operand_p (operands[0]))
      operands[0] = nios2_split_symbolic_memory_operand (operands[0]);
    else
      operands[1] = nios2_split_symbolic_memory_operand (operands[1]);
  }
  [(set_attr "type" "st,ld,mov")])

(define_insn_and_split "movhi_internal"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=m, r,r")
        (match_operand:HI 1 "general_operand"       "rM,m,rI"))]
  "(register_operand (operands[0], HImode)
    || reg_or_0_operand (operands[1], HImode))"
  {
    switch (which_alternative)
      {
      case 0:
        return "sth%o0%.\\t%z1, %0";
      case 1:
        return "ldhu%o1%.\\t%0, %1";
      case 2:
        return "mov%i1%.\\t%0, %z1";
      default:
	gcc_unreachable ();
      }
  }
  "(nios2_symbolic_memory_operand_p (operands[0]) 
   || nios2_symbolic_memory_operand_p (operands[1]))"
  [(set (match_dup 0) (match_dup 1))]
  {
    if (nios2_symbolic_memory_operand_p (operands[0]))
      operands[0] = nios2_split_symbolic_memory_operand (operands[0]);
    else
      operands[1] = nios2_split_symbolic_memory_operand (operands[1]);
  }
  [(set_attr "type" "st,ld,mov")])

(define_insn_and_split "movsi_internal"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=m, r,r,   r")
        (match_operand:SI 1 "general_operand"       "rM,m,rIJK,S"))]
  "(register_operand (operands[0], SImode)
    || reg_or_0_operand (operands[1], SImode))"
  {
    switch (which_alternative)
      {
      case 0:
	if (get_attr_length (insn) != 2)
	  return "stw%o0\\t%z1, %0";
	else if (stack_memory_operand (operands[0], SImode))
	  return "stwsp.n\\t%z1, %0";
	else if (const_0_operand (operands[1], SImode))
	  return "stwz.n\\t%z1, %0";
	else
	  return "stw.n\\t%z1, %0";
      case 1:
	if (get_attr_length (insn) != 2)
	  return "ldw%o1\\t%0, %1";
	else if (stack_memory_operand (operands[1], SImode))
	  return "ldwsp.n\\t%0, %1";
	else
	  return "ldw.n\\t%0, %1";
      case 2:
	return "mov%i1%.\\t%0, %z1";
      case 3:
	return "addi\\t%0, gp, %%gprel(%1)";
      default:
	gcc_unreachable ();
      }
  }
  "(nios2_symbolic_memory_operand_p (operands[0]) 
    || nios2_symbolic_memory_operand_p (operands[1])
    || nios2_large_constant_p (operands[1]))"
  [(set (match_dup 0) (match_dup 1))]
  {
    if (nios2_symbolic_memory_operand_p (operands[0]))
      operands[0] = nios2_split_symbolic_memory_operand (operands[0]);
    else if (nios2_symbolic_memory_operand_p (operands[1]))
      operands[1] = nios2_split_symbolic_memory_operand (operands[1]);
    else
      operands[1] = nios2_split_large_constant (operands[1], operands[0]);
  }
  [(set_attr "type" "st,ld,mov,alu")])

(define_mode_iterator BH [QI HI])
(define_mode_iterator BHW [QI HI SI])
(define_mode_attr bh [(QI "b") (HI "h")])
(define_mode_attr bhw [(QI "b") (HI "h") (SI "w")])
(define_mode_attr bhw_uns [(QI "bu") (HI "hu") (SI "w")])

(define_insn "ld<bhw_uns>io"
  [(set (match_operand:BHW 0 "register_operand" "=r")
        (unspec_volatile:BHW
          [(match_operand:BHW 1 "ldstio_memory_operand" "w")] UNSPECV_LDXIO))]
  ""
  "ld<bhw_uns>io\\t%0, %1"
  [(set_attr "type" "ld")])

(define_expand "ld<bh>io"
  [(set (match_operand:BH 0 "register_operand" "=r")
        (match_operand:BH 1 "ldstio_memory_operand" "w"))]
  ""
{
  rtx tmp = gen_reg_rtx (SImode);
  emit_insn (gen_ld<bh>io_signed (tmp, operands[1]));
  emit_insn (gen_mov<mode> (operands[0], gen_lowpart (<MODE>mode, tmp)));
  DONE;
})

(define_insn "ld<bh>io_signed"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (sign_extend:SI
          (unspec_volatile:BH
            [(match_operand:BH 1 "ldstio_memory_operand" "w")] UNSPECV_LDXIO)))]
  ""
  "ld<bh>io\\t%0, %1"
  [(set_attr "type" "ld")])

(define_insn "st<bhw>io"
  [(set (match_operand:BHW 0 "ldstio_memory_operand" "=w")
        (unspec_volatile:BHW
          [(match_operand:BHW 1 "reg_or_0_operand" "rM")] UNSPECV_STXIO))]
  ""
  "st<bhw>io\\t%z1, %0"
  [(set_attr "type" "st")])


;; QI to [HI, SI] extension patterns are collected together
(define_mode_iterator QX [HI SI])

;; Zero extension patterns
(define_insn_and_split "zero_extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
        (zero_extend:SI (match_operand:HI 1 "nonimmediate_operand" "r,m")))]
  ""
  "@
    andi%.\\t%0, %1, 0xffff
    ldhu%o1%.\\t%0, %1"
  "nios2_symbolic_memory_operand_p (operands[1])"
  [(set (match_dup 0) (zero_extend:SI (match_dup 1)))]
  {
    operands[1] = nios2_split_symbolic_memory_operand (operands[1]);
  }
  [(set_attr "type"     "and,ld")])

(define_insn_and_split "zero_extendqi<mode>2"
  [(set (match_operand:QX 0 "register_operand" "=r,r")
        (zero_extend:QX (match_operand:QI 1 "nonimmediate_operand" "r,m")))]
  ""
  "@
    andi%.\\t%0, %1, 0xff
    ldbu%o1%.\\t%0, %1"
  "nios2_symbolic_memory_operand_p (operands[1])"
  [(set (match_dup 0) (zero_extend:QX (match_dup 1)))]
  {
    operands[1] = nios2_split_symbolic_memory_operand (operands[1]);
  }
  [(set_attr "type"     "and,ld")])

;; Sign extension patterns

(define_insn_and_split "extendhisi2"
  [(set (match_operand:SI 0 "register_operand"                     "=r,r")
        (sign_extend:SI (match_operand:HI 1 "nonimmediate_operand"  "r,m")))]
  ""
  "@
   #
   ldh%o1%.\\t%0, %1"
  "nios2_symbolic_memory_operand_p (operands[1])"
  [(set (match_dup 0) (sign_extend:SI (match_dup 1)))]
  {
    operands[1] = nios2_split_symbolic_memory_operand (operands[1]);
  }
  [(set_attr "type" "alu,ld")])

(define_insn_and_split "extendqi<mode>2"
  [(set (match_operand:QX 0 "register_operand"                     "=r,r")
        (sign_extend:QX (match_operand:QI 1 "nonimmediate_operand"  "r,m")))]
  ""
  "@
   #
   ldb%o1%.\\t%0, %1"
  "nios2_symbolic_memory_operand_p (operands[1])"
  [(set (match_dup 0) (sign_extend:QX (match_dup 1)))]
  {
    operands[1] = nios2_split_symbolic_memory_operand (operands[1]);
  }
  [(set_attr "type" "alu,ld")])

;; Split patterns for register alternative cases.
(define_split
  [(set (match_operand:SI 0 "register_operand" "")
        (sign_extend:SI (match_operand:HI 1 "register_operand" "")))]
  "reload_completed"
  [(set (match_dup 0)
        (and:SI (match_dup 1) (const_int 65535)))
   (set (match_dup 0)
        (xor:SI (match_dup 0) (const_int 32768)))
   (set (match_dup 0)
        (plus:SI (match_dup 0) (const_int -32768)))]
  "operands[1] = gen_lowpart (SImode, operands[1]);")

(define_split
  [(set (match_operand:QX 0 "register_operand" "")
        (sign_extend:QX (match_operand:QI 1 "register_operand" "")))]
  "reload_completed"
  [(set (match_dup 0)
        (and:SI (match_dup 1) (const_int 255)))
   (set (match_dup 0)
        (xor:SI (match_dup 0) (const_int 128)))
   (set (match_dup 0)
        (plus:SI (match_dup 0) (const_int -128)))]
  "operands[0] = gen_lowpart (SImode, operands[0]);
   operands[1] = gen_lowpart (SImode, operands[1]);")


;; Arithmetic Operations

(define_insn "addsi3"
  [(set (match_operand:SI 0 "register_operand"            "=r")
        (plus:SI (match_operand:SI 1 "register_operand"   "%r")
                 (match_operand:SI 2 "add_regimm_operand" "rIT")))]
  ""
{
  return nios2_add_insn_asm (insn, operands);
}
  [(set_attr "type" "add")])

(define_insn "subsi3"
  [(set (match_operand:SI 0 "register_operand"           "=r")
        (minus:SI (match_operand:SI 1 "reg_or_0_operand" "rM")
                  (match_operand:SI 2 "register_operand" "r")))]
  ""
  "sub%.\\t%0, %z1, %2"
  [(set_attr "type" "sub")])

(define_insn "mulsi3"
  [(set (match_operand:SI 0 "register_operand"          "=r")
        (mult:SI (match_operand:SI 1 "register_operand" "%r")
                 (match_operand:SI 2 "arith_operand"    "rI")))]
  "TARGET_HAS_MUL"
  "mul%i2\\t%0, %1, %z2"
  [(set_attr "type" "mul")])

(define_expand "divsi3"
  [(set (match_operand:SI 0 "register_operand"          "=r")
        (div:SI (match_operand:SI 1 "register_operand"   "r")
                (match_operand:SI 2 "register_operand"   "r")))]
  ""
{
  if (!TARGET_HAS_DIV)
    {
      if (TARGET_FAST_SW_DIV)
        {
          nios2_emit_expensive_div (operands, SImode);
          DONE;
        }
      else
        FAIL;
    }
})

(define_insn "divsi3_insn"
  [(set (match_operand:SI 0 "register_operand"            "=r")
        (div:SI (match_operand:SI 1 "register_operand"     "r")
                (match_operand:SI 2 "register_operand"     "r")))]
  "TARGET_HAS_DIV"
  "div\\t%0, %1, %2"
  [(set_attr "type" "div")])

(define_insn "udivsi3"
  [(set (match_operand:SI 0 "register_operand"            "=r")
        (udiv:SI (match_operand:SI 1 "register_operand"    "r")
                 (match_operand:SI 2 "register_operand"    "r")))]
  "TARGET_HAS_DIV"
  "divu\\t%0, %1, %2"
  [(set_attr "type" "div")])

(define_code_iterator EXTEND [sign_extend zero_extend])
(define_code_attr us [(sign_extend "s") (zero_extend "u")])
(define_code_attr mul [(sign_extend "mul") (zero_extend "umul")])

(define_insn "<us>mulsi3_highpart"
  [(set (match_operand:SI 0 "register_operand"                       "=r")
        (truncate:SI
         (lshiftrt:DI
          (mult:DI (EXTEND:DI (match_operand:SI 1 "register_operand"  "r"))
                   (EXTEND:DI (match_operand:SI 2 "register_operand"  "r")))
          (const_int 32))))]
  "TARGET_HAS_MULX"
  "mulx<us><us>\\t%0, %1, %2"
  [(set_attr "type" "mul")])

(define_expand "<mul>sidi3"
  [(set (match_operand:DI 0 "register_operand" "")
	(mult:DI (EXTEND:DI (match_operand:SI 1 "register_operand" ""))
		 (EXTEND:DI (match_operand:SI 2 "register_operand" ""))))]
  "TARGET_HAS_MULX"
{
  rtx hi = gen_reg_rtx (SImode);
  rtx lo = gen_reg_rtx (SImode);

  emit_insn (gen_<us>mulsi3_highpart (hi, operands[1], operands[2]));
  emit_insn (gen_mulsi3 (lo, operands[1], operands[2]));
  emit_move_insn (gen_lowpart (SImode, operands[0]), lo);
  emit_move_insn (gen_highpart (SImode, operands[0]), hi);
  DONE;
})


;;  Negate and ones complement

(define_insn "negsi2"
  [(set (match_operand:SI 0 "register_operand"        "=r")
        (neg:SI (match_operand:SI 1 "register_operand" "r")))]
  ""
{
  if (get_attr_length (insn) == 2)
    return "neg.n\\t%0, %1";
  else
    return "sub\\t%0, zero, %1";
}
  [(set_attr "type" "neg")])

(define_insn "one_cmplsi2"
  [(set (match_operand:SI 0 "register_operand"        "=r")
        (not:SI (match_operand:SI 1 "register_operand" "r")))]
  ""
{
  if (get_attr_length (insn) == 2)
    return "not.n\\t%0, %1";
  else
    return "nor\\t%0, zero, %1";
}
  [(set_attr "type" "not")])


;;  Integer logical Operations

(define_insn "andsi3"
  [(set (match_operand:SI 0 "register_operand"          "=r")
        (and:SI (match_operand:SI 1 "register_operand"  "%r")
                (match_operand:SI 2 "and_operand"     "rJKP")))]
  ""
  "and%x2%.\\t%0, %1, %y2"
  [(set_attr "type" "and")])

(define_code_iterator LOGICAL [ior xor])
(define_code_attr logical_asm [(ior "or") (xor "xor")])

(define_insn "<code>si3"
  [(set (match_operand:SI 0 "register_operand"             "=r")
        (LOGICAL:SI (match_operand:SI 1 "register_operand" "%r")
                    (match_operand:SI 2 "logical_operand" "rJK")))]
  ""
  "<logical_asm>%x2%.\\t%0, %1, %y2"
  [(set_attr "type" "<logical_asm>")])

(define_insn "*norsi3"
  [(set (match_operand:SI 0 "register_operand"                 "=r")
        (and:SI (not:SI (match_operand:SI 1 "register_operand" "%r"))
                (not:SI (match_operand:SI 2 "register_operand"  "r"))))]
  ""
  "nor\\t%0, %1, %2"
  [(set_attr "type" "alu")])


;;  Shift instructions

(define_code_iterator SHIFT  [ashift ashiftrt lshiftrt rotate])
(define_code_attr shift_op   [(ashift "ashl") (ashiftrt "ashr")
                              (lshiftrt "lshr") (rotate "rotl")])
(define_code_attr shift_asm  [(ashift "sll") (ashiftrt "sra")
                              (lshiftrt "srl") (rotate "rol")])

(define_insn "<shift_op>si3"
  [(set (match_operand:SI 0 "register_operand"          "=r")
        (SHIFT:SI (match_operand:SI 1 "register_operand" "r")
                  (match_operand:SI 2 "shift_operand"    "rL")))]
  ""
  "<shift_asm>%i2%.\\t%0, %1, %z2"
  [(set_attr "type" "<shift_asm>")])

(define_insn "rotrsi3"
  [(set (match_operand:SI 0 "register_operand"             "=r")
        (rotatert:SI (match_operand:SI 1 "register_operand" "r")
                     (match_operand:SI 2 "register_operand" "r")))]
  ""
  "ror\\t%0, %1, %2"
  [(set_attr "type" "ror")])

;; Nios II R2 Bit Manipulation Extension (BMX), provides
;; bit merge/insertion/extraction instructions.

(define_insn "*merge"
  [(set (zero_extract:SI (match_operand:SI 0 "register_operand"   "+r")
			 (match_operand:SI 1 "const_shift_operand" "L")
			 (match_operand:SI 2 "const_shift_operand" "L"))
        (zero_extract:SI (match_operand:SI 3 "register_operand"    "r")
                         (match_dup 1) (match_dup 2)))]
  "TARGET_HAS_BMX"
{
  operands[4] = GEN_INT (INTVAL (operands[1]) + INTVAL (operands[2]) - 1);
  return "merge\\t%0, %3, %4, %2";
}
  [(set_attr "type" "alu")])

(define_insn "extzv"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (zero_extract:SI (match_operand:SI 1 "register_operand"    "r")
                         (match_operand:SI 2 "const_shift_operand" "L")
                         (match_operand:SI 3 "const_shift_operand" "L")))]
  "TARGET_HAS_BMX"
{
  operands[4] = GEN_INT (INTVAL (operands[2]) + INTVAL (operands[3]) - 1);
  return "extract\\t%0, %1, %4, %3";
}
  [(set_attr "type" "alu")])

(define_insn "insv"
  [(set (zero_extract:SI (match_operand:SI 0 "register_operand"   "+r")
			 (match_operand:SI 1 "const_shift_operand" "L")
			 (match_operand:SI 2 "const_shift_operand" "L"))
	(match_operand:SI 3 "reg_or_0_operand" "rM"))]
  "TARGET_HAS_BMX"
{
  operands[4] = GEN_INT (INTVAL (operands[1]) + INTVAL (operands[2]) - 1);
  return "insert\\t%0, %z3, %4, %2";
}
  [(set_attr "type" "alu")])



;; Floating point instructions

;; Mode iterator for single/double float
(define_mode_iterator F [SF DF])
(define_mode_attr f [(SF "s") (DF "d")])

;; Basic arithmetic instructions
(define_code_iterator FOP3 [plus minus mult div])
(define_code_attr fop3 [(plus "add") (minus "sub") (mult "mul") (div "div")])

(define_insn "<fop3><mode>3"
  [(set (match_operand:F 0 "register_operand"        "=r")
        (FOP3:F (match_operand:F 1 "register_operand" "r")
                (match_operand:F 2 "register_operand" "r")))]
  "nios2_fpu_insn_enabled (n2fpu_f<fop3><f>)"
  { return nios2_fpu_insn_asm (n2fpu_f<fop3><f>); }
  [(set_attr "type" "custom")])

;; Floating point min/max operations
(define_code_iterator SMINMAX [smin smax])
(define_code_attr minmax [(smin "min") (smax "max")])
(define_insn "<code><mode>3"
  [(set (match_operand:F 0 "register_operand" "=r")
        (SMINMAX:F (match_operand:F 1 "register_operand" "r")
                   (match_operand:F 2 "register_operand" "r")))]
  "nios2_fpu_insn_enabled (n2fpu_f<minmax><f>)"
  { return nios2_fpu_insn_asm (n2fpu_f<minmax><f>); }
  [(set_attr "type" "custom")])

;; These 2-operand FP operations can be collected together
(define_code_iterator FOP2 [abs neg sqrt])
(define_insn "<code><mode>2"
  [(set (match_operand:F 0 "register_operand" "=r")
        (FOP2:F (match_operand:F 1 "register_operand" "r")))]
  "nios2_fpu_insn_enabled (n2fpu_f<code><f>)"
  { return nios2_fpu_insn_asm (n2fpu_f<code><f>); }
  [(set_attr "type" "custom")])

;; X, Y register access instructions
(define_insn "nios2_fwrx"
  [(unspec_volatile [(match_operand:DF 0 "register_operand" "r")] UNSPECV_FWRX)]
  "nios2_fpu_insn_enabled (n2fpu_fwrx)"
  { return nios2_fpu_insn_asm (n2fpu_fwrx); }
  [(set_attr "type" "custom")])

(define_insn "nios2_fwry"
  [(unspec_volatile [(match_operand:SF 0 "register_operand" "r")] UNSPECV_FWRY)]
  "nios2_fpu_insn_enabled (n2fpu_fwry)"
  { return nios2_fpu_insn_asm (n2fpu_fwry); }
  [(set_attr "type" "custom")])

;; The X, Y read insns uses an int iterator
(define_int_iterator UNSPEC_READ_XY [UNSPECV_FRDXLO UNSPECV_FRDXHI
                                     UNSPECV_FRDY])
(define_int_attr read_xy [(UNSPECV_FRDXLO "frdxlo") (UNSPECV_FRDXHI "frdxhi")
                          (UNSPECV_FRDY "frdy")])
(define_insn "nios2_<read_xy>"
  [(set (match_operand:SF 0 "register_operand" "=r")
        (unspec_volatile:SF [(const_int 0)] UNSPEC_READ_XY))]
  "nios2_fpu_insn_enabled (n2fpu_<read_xy>)"
  { return nios2_fpu_insn_asm (n2fpu_<read_xy>); }
  [(set_attr "type" "custom")])

;; Various math functions
(define_int_iterator MATHFUNC
  [UNSPEC_FCOS UNSPEC_FSIN UNSPEC_FTAN UNSPEC_FATAN UNSPEC_FEXP UNSPEC_FLOG])
(define_int_attr mathfunc [(UNSPEC_FCOS "cos") (UNSPEC_FSIN "sin")
                           (UNSPEC_FTAN "tan") (UNSPEC_FATAN "atan")
                           (UNSPEC_FEXP "exp") (UNSPEC_FLOG "log")])

(define_insn "<mathfunc><mode>2"
  [(set (match_operand:F 0 "register_operand" "=r")
        (unspec:F [(match_operand:F 1 "register_operand" "r")] MATHFUNC))]
  "nios2_fpu_insn_enabled (n2fpu_f<mathfunc><f>)"
  { return nios2_fpu_insn_asm (n2fpu_f<mathfunc><f>); }
  [(set_attr "type" "custom")])

;; Converting between floating point and fixed point

(define_code_iterator FLOAT [float unsigned_float])
(define_code_iterator FIX [fix unsigned_fix])

(define_code_attr conv_op [(float "float") (unsigned_float "floatuns")
                           (fix "fix") (unsigned_fix "fixuns")])
(define_code_attr i [(float "i") (unsigned_float "u")
                     (fix "i") (unsigned_fix "u")])

;; Integer to float conversions
(define_insn "<conv_op>si<mode>2"
  [(set (match_operand:F 0 "register_operand" "=r")
        (FLOAT:F (match_operand:SI 1 "register_operand" "r")))]
  "nios2_fpu_insn_enabled (n2fpu_float<i><f>)"
  { return nios2_fpu_insn_asm (n2fpu_float<i><f>); }
  [(set_attr "type" "custom")])

;; Float to integer conversions
(define_insn "<conv_op>_trunc<mode>si2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (FIX:SI (match_operand:F 1 "general_operand" "r")))]
  "nios2_fpu_insn_enabled (n2fpu_fix<f><i>)"
  { return nios2_fpu_insn_asm (n2fpu_fix<f><i>); }
  [(set_attr "type" "custom")])

(define_insn "lroundsfsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(match_operand:SF 1 "general_operand" "r")] UNSPEC_ROUND))]
  "nios2_fpu_insn_enabled (n2fpu_round)"
  { return nios2_fpu_insn_asm (n2fpu_round); }
  [(set_attr "type" "custom")])

(define_insn "extendsfdf2"
  [(set (match_operand:DF 0 "register_operand" "=r")
        (float_extend:DF (match_operand:SF 1 "general_operand" "r")))]
  "nios2_fpu_insn_enabled (n2fpu_fextsd)"
  { return nios2_fpu_insn_asm (n2fpu_fextsd); }
  [(set_attr "type" "custom")])

(define_insn "truncdfsf2"
  [(set (match_operand:SF 0 "register_operand" "=r")
        (float_truncate:SF (match_operand:DF 1 "general_operand" "r")))]
  "nios2_fpu_insn_enabled (n2fpu_ftruncds)"
  { return nios2_fpu_insn_asm (n2fpu_ftruncds); }
  [(set_attr "type" "custom")])



;; Prologue, Epilogue and Return

(define_expand "prologue"
  [(const_int 1)]
  ""
{
  nios2_expand_prologue ();
  DONE;
})

(define_expand "epilogue"
  [(return)]
  ""
{
  nios2_expand_epilogue (false);
  DONE;
})

(define_expand "sibcall_epilogue"
  [(return)]
  ""
{
  nios2_expand_epilogue (true);
  DONE;
})

(define_expand "return"
  [(simple_return)]
  "nios2_can_use_return_insn ()"
{
  if (nios2_expand_return ())
    DONE;
})

(define_insn "simple_return"
  [(simple_return)]
  ""
  "ret%."
  [(set_attr "type" "control")])

;; Block any insns from being moved before this point, since the
;; profiling call to mcount can use various registers that aren't
;; saved or used to pass arguments.

(define_insn "blockage"
  [(unspec_volatile [(const_int 0)] UNSPECV_BLOCKAGE)]
  ""
  ""
  [(set_attr "type" "unknown")
   (set_attr "length" "0")])

;; This is used in compiling the unwind routines.
(define_expand "eh_return"
  [(use (match_operand 0 "general_operand"))]
  ""
{
  if (GET_MODE (operands[0]) != Pmode)
    operands[0] = convert_to_mode (Pmode, operands[0], 0);
  emit_insn (gen_eh_set_ra (operands[0]));
  DONE;
})

;; Modify the return address for EH return.  We can't expand this
;; until we know where it will be put in the stack frame.

(define_insn_and_split "eh_set_ra"
  [(unspec [(match_operand:SI 0 "register_operand" "r")] UNSPEC_EH_RETURN)
   (clobber (match_scratch:SI 1 "=&r"))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
{
  nios2_set_return_address (operands[0], operands[1]);
  DONE;
})


;;  Jumps and calls

; Note that the assembler fixes up any out-of-range branch instructions not
; caught by the compiler branch shortening code.  The sequence emitted by
; the assembler can be very inefficient, but it is correct for PIC code.
; For non-PIC we are better off converting to an absolute JMPI.
;
; Direct calls and sibcalls use the CALL and JMPI instructions, respectively.
; These instructions have an immediate operand that specifies the low 28 bits
; of the PC, effectively allowing direct calls within a 256MB memory segment.
; Per the Nios II Processor Reference Handbook, the linker is not required to
; check or adjust for overflow.

(define_insn "indirect_jump"
  [(set (pc) (match_operand:SI 0 "register_operand" "c"))]
  ""
  "jmp%!\\t%0"
  [(set_attr "type" "control")])

(define_insn "jump"
  [(set (pc)
        (label_ref (match_operand 0 "" "")))]
  ""
  {
    if (get_attr_length (insn) == 2)
      return "br.n\\t%0";
    else if (get_attr_length (insn) == 4)
      return "br\\t%0";
    else
      return "jmpi\\t%0";
  }
  [(set_attr "type" "control")
   (set (attr "length") 
        (if_then_else
	    (and (match_test "TARGET_HAS_CDX")
	         (and (ge (minus (match_dup 0) (pc)) (const_int -1022))
	              (le (minus (match_dup 0) (pc)) (const_int 1022))))
	    (const_int 2)
	    (if_then_else
	        (ior (match_test "flag_pic")
	             (and (ge (minus (match_dup 0) (pc)) (const_int -32764))
	                  (le (minus (match_dup 0) (pc)) (const_int 32764))))
	        (const_int 4)
	        (const_int 8))))])

(define_expand "call"
  [(parallel [(call (match_operand 0 "" "")
                    (match_operand 1 "" ""))
              (clobber (reg:SI RA_REGNO))])]
  ""
  "nios2_adjust_call_address (&operands[0], NULL_RTX);")

(define_expand "call_value"
  [(parallel [(set (match_operand 0 "" "")
                   (call (match_operand 1 "" "")
                         (match_operand 2 "" "")))
              (clobber (reg:SI RA_REGNO))])]
  ""
  "nios2_adjust_call_address (&operands[1], NULL_RTX);")

(define_insn "*call"
  [(call (mem:QI (match_operand:SI 0 "call_operand" "i,r"))
         (match_operand 1 "" ""))
   (clobber (reg:SI RA_REGNO))]
  ""
  "@
   call\\t%0
   callr%.\\t%0"
  [(set_attr "type" "control")])

(define_insn "*call_value"
  [(set (match_operand 0 "" "")
        (call (mem:QI (match_operand:SI 1 "call_operand" "i,r"))
              (match_operand 2 "" "")))
   (clobber (reg:SI RA_REGNO))]
  ""
  "@
   call\\t%1
   callr%.\\t%1"
  [(set_attr "type" "control")])

(define_expand "sibcall"
  [(parallel [(call (match_operand 0 "" "")
                    (match_operand 1 "" ""))
              (return)])]
  ""
  "nios2_adjust_call_address (&operands[0], NULL_RTX);")

(define_expand "sibcall_value"
  [(parallel [(set (match_operand 0 "" "")
                   (call (match_operand 1 "" "")
                         (match_operand 2 "" "")))
              (return)])]
  ""
  "nios2_adjust_call_address (&operands[1], NULL_RTX);")

(define_insn "sibcall_internal"
 [(call (mem:QI (match_operand:SI 0 "call_operand" "i,j"))
        (match_operand 1 "" ""))
  (return)]
  ""
  "@
   jmpi\\t%0
   jmp%!\\t%0"
  [(set_attr "type" "control")])

(define_insn "sibcall_value_internal"
 [(set (match_operand 0 "register_operand" "")
       (call (mem:QI (match_operand:SI 1 "call_operand" "i,j"))
             (match_operand 2 "" "")))
  (return)]
  ""
  "@
   jmpi\\t%1
   jmp%!\\t%1"
  [(set_attr "type" "control")])

(define_expand "tablejump"
  [(parallel [(set (pc) (match_operand 0 "register_operand" "r"))
              (use (label_ref (match_operand 1 "" "")))])]
  ""
{
  if (flag_pic)
    {
      /* Hopefully, CSE will eliminate this copy.  */
      rtx reg1 = copy_addr_to_reg (gen_rtx_LABEL_REF (Pmode, operands[1]));
      rtx reg2 = gen_reg_rtx (SImode);

      emit_insn (gen_addsi3 (reg2, operands[0], reg1));
      operands[0] = reg2;
    }
})

(define_insn "*tablejump"
  [(set (pc)
        (match_operand:SI 0 "register_operand" "c"))
   (use (label_ref (match_operand 1 "" "")))]
  ""
  "jmp%!\\t%0"
  [(set_attr "type" "control")])


;; cstore, cbranch patterns

(define_mode_iterator CM [SI SF DF])

(define_expand "cstore<mode>4"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (match_operator:SI 1 "expandable_comparison_operator"
	  [(match_operand:CM 2 "register_operand")
	   (match_operand:CM 3 "nonmemory_operand")]))]
  ""
{
  if (!nios2_validate_compare (<MODE>mode, &operands[1], &operands[2],
                               &operands[3]))
    FAIL;
})

(define_expand "cbranch<mode>4"
  [(set (pc)
     (if_then_else
       (match_operator 0 "expandable_comparison_operator"
         [(match_operand:CM 1 "register_operand")
          (match_operand:CM 2 "nonmemory_operand")])
       (label_ref (match_operand 3 ""))
       (pc)))]
  ""
{
  if (!nios2_validate_compare (<MODE>mode, &operands[0], &operands[1],
                               &operands[2]))
    FAIL;
  if (GET_MODE_CLASS (<MODE>mode) == MODE_FLOAT
      || !reg_or_0_operand (operands[2], <MODE>mode))
    {
      rtx condreg = gen_reg_rtx (SImode);
      emit_insn (gen_cstore<mode>4
                  (condreg, operands[0], operands[1], operands[2]));
      operands[1] = condreg;
      operands[2] = const0_rtx;
      operands[0] = gen_rtx_fmt_ee (NE, VOIDmode, condreg, const0_rtx);
    }
})

(define_insn "nios2_cbranch"
  [(set (pc)
     (if_then_else
       (match_operator 0 "ordered_comparison_operator"
         [(match_operand:SI 1 "reg_or_0_operand" "rM")
          (match_operand:SI 2 "reg_or_0_operand" "rM")])
       (label_ref (match_operand 3 "" ""))
       (pc)))]
  ""
{
  if (get_attr_length (insn) == 2)
    return "b%0z.n\t%z1, %l3";
  else if (get_attr_length (insn) == 4)
    return "b%0\t%z1, %z2, %l3";
  else if (get_attr_length (insn) == 6)
    return "b%R0z.n\t%z1, .+6;jmpi\t%l3";
  else
    return "b%R0\t%z1, %z2, .+8;jmpi\t%l3";
}
  [(set_attr "type" "control")
   (set (attr "length") 
        (cond
         [(and (match_test "nios2_cdx_narrow_form_p (insn)")
               (ge (minus (match_dup 3) (pc)) (const_int -126))
               (le (minus (match_dup 3) (pc)) (const_int 126)))
          (const_int 2)
          (ior (match_test "flag_pic")
               (and (ge (minus (match_dup 3) (pc)) (const_int -32764))
                    (le (minus (match_dup 3) (pc)) (const_int 32764))))
          (const_int 4)
          (match_test "nios2_cdx_narrow_form_p (insn)")
          (const_int 6)]
         (const_int 8)))])

;; Floating point comparisons
(define_code_iterator FCMP [eq ne gt ge le lt])
(define_insn "nios2_s<code><mode>"
  [(set (match_operand:SI 0 "register_operand"        "=r")
        (FCMP:SI (match_operand:F 1 "register_operand" "r")
                 (match_operand:F 2 "register_operand" "r")))]
  "nios2_fpu_insn_enabled (n2fpu_fcmp<code><f>)"
  { return nios2_fpu_insn_asm (n2fpu_fcmp<code><f>); }
  [(set_attr "type" "custom")])

;; Integer comparisons

(define_code_iterator EQNE [eq ne])
(define_insn "nios2_cmp<code>"
  [(set (match_operand:SI 0 "register_operand"           "=r")
        (EQNE:SI (match_operand:SI 1 "register_operand"  "%r")
                 (match_operand:SI 2 "arith_operand"     "rI")))]
  ""
  "cmp<code>%i2\\t%0, %1, %z2"
  [(set_attr "type" "alu")])

(define_code_iterator SCMP [ge lt])
(define_insn "nios2_cmp<code>"
  [(set (match_operand:SI 0 "register_operand"           "=r")
        (SCMP:SI (match_operand:SI 1 "reg_or_0_operand"  "rM")
                 (match_operand:SI 2 "arith_operand"     "rI")))]
  ""
  "cmp<code>%i2\\t%0, %z1, %z2"
  [(set_attr "type" "alu")])

(define_code_iterator UCMP [geu ltu])
(define_insn "nios2_cmp<code>"
  [(set (match_operand:SI 0 "register_operand"           "=r")
        (UCMP:SI (match_operand:SI 1 "reg_or_0_operand"  "rM")
                 (match_operand:SI 2 "uns_arith_operand" "rJ")))]
  ""
  "cmp<code>%u2\\t%0, %z1, %z2"
  [(set_attr "type" "alu")])



;; Custom instruction patterns.  The operands are intentionally
;; mode-less, to serve as generic carriers of all Altera defined
;; built-in instruction/function types.

(define_insn "custom_nxx"
  [(unspec_volatile [(match_operand 0 "custom_insn_opcode" "N")
                     (match_operand 1 "reg_or_0_operand"  "rM")
                     (match_operand 2 "reg_or_0_operand"  "rM")]
    UNSPECV_CUSTOM_NXX)]
  ""
  "custom\\t%0, zero, %z1, %z2"
  [(set_attr "type" "custom")])

(define_insn "custom_xnxx"
  [(set (match_operand 0 "register_operand"   "=r")
        (unspec_volatile [(match_operand 1 "custom_insn_opcode" "N")
                          (match_operand 2 "reg_or_0_operand"  "rM")
                          (match_operand 3 "reg_or_0_operand"  "rM")] 
	 UNSPECV_CUSTOM_XNXX))]
  ""
  "custom\\t%1, %0, %z2, %z3"
  [(set_attr "type" "custom")])


;;  Misc. patterns

(define_insn "nop"
  [(const_int 0)]
  ""
  "nop%."
  [(set_attr "type" "nop")])

;; Connect 'sync' to 'memory_barrier' standard expand name
(define_expand "memory_barrier"
  [(const_int 0)]
  ""
{
  emit_insn (gen_sync ());
  DONE;
})

;; For the nios2 __builtin_sync built-in function
(define_expand "sync"
  [(set (match_dup 0)
	(unspec:BLK [(match_dup 0)] UNSPEC_SYNC))]
  ""
{
  operands[0] = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (Pmode));
  MEM_VOLATILE_P (operands[0]) = 1;
})

(define_insn "*sync_insn"
  [(set (match_operand:BLK 0 "" "")
	(unspec:BLK [(match_dup 0)] UNSPEC_SYNC))]
  ""
  "sync"
  [(set_attr "type" "control")])

(define_insn "rdctl"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec_volatile:SI [(match_operand:SI 1 "rdwrctl_operand" "O")] 
	 UNSPECV_RDCTL))]
  ""
  "rdctl\\t%0, ctl%1"
  [(set_attr "type" "control")])

(define_insn "wrctl"
  [(unspec_volatile:SI [(match_operand:SI 0 "rdwrctl_operand"  "O")
                        (match_operand:SI 1 "reg_or_0_operand" "rM")] 
    UNSPECV_WRCTL)]
  ""
  "wrctl\\tctl%0, %z1"
  [(set_attr "type" "control")])

(define_insn "rdprs"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec_volatile:SI [(match_operand:SI 1 "rdwrctl_operand" "O")
                             (match_operand:SI 2 "arith_operand"   "U")]
         UNSPECV_RDPRS))]
  ""
  "rdprs\\t%0, %1, %2"
  [(set_attr "type" "control")])

;; Cache Instructions

(define_insn "flushd"
  [(unspec_volatile:SI [(match_operand:SI 0 "ldstio_memory_operand" "w")]
  		        UNSPECV_FLUSHD)]
  ""
  "flushd\\t%0"
  [(set_attr "type" "control")])

(define_insn "flushda"
  [(unspec_volatile:SI [(match_operand:SI 0 "ldstio_memory_operand" "w")]
  		        UNSPECV_FLUSHDA)]
  ""
  "flushda\\t%0"
  [(set_attr "type" "control")])

;; R2 Instructions

(define_insn "wrpie"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec_volatile:SI [(match_operand:SI 1 "register_operand" "r")]
		 	     UNSPECV_WRPIE))]
  "TARGET_ARCH_R2"
  "wrpie\\t%0, %1"
  [(set_attr "type" "control")])

(define_insn "eni"
  [(unspec:VOID [(match_operand 0 "const_int_operand" "i")]
  		 UNSPECV_ENI)]
  "TARGET_ARCH_R2"
  "eni\\t%0"
  [(set_attr "type" "control")])

;; Trap patterns
(define_insn "trap"
  [(trap_if (const_int 1) (const_int 3))]
  ""
  "trap%.\\t3"
  [(set_attr "type" "control")])

(define_insn "ctrapsi4"
  [(trap_if (match_operator 0 "ordered_comparison_operator"
              [(match_operand:SI 1 "reg_or_0_operand" "rM")
               (match_operand:SI 2 "reg_or_0_operand" "rM")])
            (match_operand 3 "const_int_operand" "i"))]
  ""
{
  if (get_attr_length (insn) == 6)
    return "b%R0\\t%z1, %z2, 1f\;trap.n\\t%3\;1:";
  else
    return "b%R0\\t%z1, %z2, 1f\;trap\\t%3\;1:";
}
  [(set_attr "type" "control")
   (set (attr "length")
        (if_then_else (match_test "nios2_cdx_narrow_form_p (insn)")
                      (const_int 6) (const_int 8)))])
  
;; Load the GOT register.
(define_insn "load_got_register"
  [(set (match_operand:SI 0 "register_operand" "=&r")
	 (unspec:SI [(const_int 0)] UNSPEC_LOAD_GOT_REGISTER))
   (set (match_operand:SI 1 "register_operand" "=r")
	 (unspec:SI [(const_int 0)] UNSPEC_LOAD_GOT_REGISTER))]
  ""
  "nextpc\\t%0
\\t1:
\\tmovhi\\t%1, %%hiadj(_gp_got - 1b)
\\taddi\\t%1, %1, %%lo(_gp_got - 1b)"
  [(set_attr "length" "12")])

;; Read thread pointer register
(define_expand "get_thread_pointersi"
  [(match_operand:SI 0 "register_operand" "=r")]
  "TARGET_LINUX_ABI"
{
  emit_move_insn (operands[0], gen_rtx_REG (Pmode, TP_REGNO));
  DONE;
})

;; Synchronization Primitives
(include "sync.md")

;; Include the ldwm/stwm/push.n/pop.n patterns and peepholes.
(include "ldstwm.md")

