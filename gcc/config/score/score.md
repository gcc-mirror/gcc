;;  Machine description for Sunplus S+CORE
;;  Copyright (C) 2005, 2007
;;  Free Software Foundation, Inc.
;;  Contributed by Sunnorth.

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;;- See file "rtl.def" for documentation on define_insn, match_*, et. al.

; branch        conditional branch
; jump          unconditional jump
; call          unconditional call
; load          load instruction(s)
; store         store instruction(s)
; cmp           integer compare
; arith         integer arithmetic instruction
; move          data movement within same register set
; const         load constant
; nop           no operation
; mul           integer multiply
; div           integer divide
; cndmv         conditional moves
; fce           transfer from hi/lo registers
; tce           transfer to   hi/lo registers
; fsr           transfer from special registers
; tsr           transfer to   special registers

(define_constants
  [(CC_REGNUM       33)
   (T_REGNUM        34)
   (RA_REGNUM       3)
   (SP_REGNUM       0)
   (AT_REGNUM       1)
   (FP_REGNUM       2)
   (RT_REGNUM       4)
   (GP_REGNUM       28)
   (EH_REGNUM       29)
   (HI_REGNUM       48)
   (LO_REGNUM       49)
   (CN_REGNUM       50)
   (LC_REGNUM       51)
   (SC_REGNUM       52)])

(define_constants
   [(BITTST         0)
    (CPLOAD         1)
    (CPRESTORE      2)

    (SCB            3)
    (SCW            4)
    (SCE            5)
    (SCLC           6)

    (LCB            7)
    (LCW            8)
    (LCE            9)

    (SFFS           10)])

(define_attr "type"
  "unknown,branch,jump,call,load,store,cmp,arith,move,const,nop,mul,div,cndmv,fce,tce,fsr,tsr,fcr,tcr"
  (const_string "unknown"))

(define_attr "mode" "unknown,QI,HI,SI,DI"
  (const_string "unknown"))

(define_attr "length" "" (const_int 4))

(define_attr "up_c" "yes,no"
  (const_string "no"))

(include "score-generic.md")
(include "predicates.md")

(define_expand "movqi"
  [(set (match_operand:QI 0 "nonimmediate_operand")
        (match_operand:QI 1 "general_operand"))]
  ""
{
  if (MEM_P (operands[0])
      && !register_operand (operands[1], QImode))
    {
      operands[1] = force_reg (QImode, operands[1]);
    }
})

(define_insn "*movqi_insns_score7"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=d,d,d,m,d,*x,d,*a")
        (match_operand:QI 1 "general_operand" "i,d,m,d,*x,d,*a,d"))]
  "(!MEM_P (operands[0]) || register_operand (operands[1], QImode))
   && (TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)"
{
  switch (which_alternative)
    {
    case 0: return score_limm (operands);
    case 1: return score_move (operands);
    case 2: return score_linsn (operands, SCORE_BYTE, false);
    case 3: return score_sinsn (operands, SCORE_BYTE);
    case 4: return TARGET_SCORE7D ? \"mf%1%S0 %0\" : \"mf%1    %0\";
    case 5: return TARGET_SCORE7D ? \"mt%0%S1 %1\" : \"mt%0    %1\";
    case 6: return \"mfsr\t%0, %1\";
    case 7: return \"mtsr\t%1, %0\";
    default: gcc_unreachable ();
    }
}
  [(set_attr "type" "arith,move,load,store,fce,tce,fsr,tsr")
   (set_attr "mode" "QI")])

(define_insn "*movqi_insns_score3"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=d,d,d,m,d,*x,d,*a")
        (match_operand:QI 1 "general_operand" "i,d,m,d,*x,d,*a,d"))]
  "(!MEM_P (operands[0]) || register_operand (operands[1], QImode))
   && (TARGET_SCORE3)"
{
  switch (which_alternative)
    {
    case 0: return score_limm (operands);
    case 1: return \"mv!\t%0, %1\";
    case 2: return score_linsn (operands, SCORE_BYTE, false);
    case 3: return score_sinsn (operands, SCORE_BYTE);
    case 4: return TARGET_SCORE3D ? \"mf%1%S0\t%0\" : \"mf%1\t%0\";
    case 5: return TARGET_SCORE3D ? \"mt%0%S1\t%1\" : \"mt%0\t%1\";
    case 6: return \"mfsr\t%0, %1\";
    case 7: return \"mtsr\t%1, %0\";
    default: gcc_unreachable ();
    }
}
  [(set_attr "type" "arith,move,load,store,fce,tce,fsr,tsr")
   (set_attr "length" "6,2,6,6,4,4,4,4")
   (set_attr "mode" "QI")])

(define_expand "movhi"
  [(set (match_operand:HI 0 "nonimmediate_operand")
        (match_operand:HI 1 "general_operand"))]
  ""
{
  if (MEM_P (operands[0])
      && !register_operand (operands[1], HImode))
    {
      operands[1] = force_reg (HImode, operands[1]);
    }
})

(define_insn "*movhi_insns_score7"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=d,d,d,m,d,*x,d,*a")
        (match_operand:HI 1 "general_operand" "i,d,m,d,*x,d,*a,d"))]
  "(!MEM_P (operands[0]) || register_operand (operands[1], HImode))
   && (TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)"
{
  switch (which_alternative)
    {
    case 0: return score_limm (operands);
    case 1: return score_move (operands);
    case 2: return score_linsn (operands, SCORE_HWORD, false);
    case 3: return score_sinsn (operands, SCORE_HWORD);
    case 4: return TARGET_SCORE7D ? \"mf%1%S0 %0\" : \"mf%1    %0\";
    case 5: return TARGET_SCORE7D ? \"mt%0%S1 %1\" : \"mt%0    %1\";
    case 6: return \"mfsr\t%0, %1\";
    case 7: return \"mtsr\t%1, %0\";
    default: gcc_unreachable ();
    }
}
  [(set_attr "type" "arith,move,load,store,fce,tce,fsr,tsr")
   (set_attr "mode" "HI")])

(define_insn "*movhi_insns_score3"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=d,d,d,m,d,*x,d,*a")
        (match_operand:HI 1 "general_operand" "i,d,m,d,*x,d,*a,d"))]
  "(!MEM_P (operands[0]) || register_operand (operands[1], HImode))
   && (TARGET_SCORE3)"
{
  switch (which_alternative)
    {
    case 0: return score_limm (operands);
    case 1: return \"mv!\t%0, %1\";
    case 2: return score_linsn (operands, SCORE_HWORD, false);
    case 3: return score_sinsn (operands, SCORE_HWORD);
    case 4: return TARGET_SCORE3D ? \"mf%1%S0\t%0\" : \"mf%1\t%0\";
    case 5: return TARGET_SCORE3D ? \"mt%0%S1\t%1\" : \"mt%0\t%1\";
    case 6: return \"mfsr\t%0, %1\";
    case 7: return \"mtsr\t%1, %0\";
    default: gcc_unreachable ();
    }
}
  [(set_attr "type" "arith,move,load,store,fce,tce,fsr,tsr")
   (set_attr "length" "6,2,6,6,4,4,4,4")
   (set_attr "mode" "HI")])

(define_expand "movsi"
  [(set (match_operand:SI 0 "nonimmediate_operand")
        (match_operand:SI 1 "general_operand"))]
  ""
{
  if (MEM_P (operands[0])
      && !register_operand (operands[1], SImode))
    {
      operands[1] = force_reg (SImode, operands[1]);
    }
})

(define_insn "*movsi_insns_score7"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=d,d,d,m,d,*x,d,*a,d,*c")
        (match_operand:SI 1 "general_operand" "i,d,m,d,*x,d,*a,d,*c,d"))]
  "(!MEM_P (operands[0]) || register_operand (operands[1], SImode))
   && (TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)"
{
  switch (which_alternative)
    {
    case 0:
      if (GET_CODE (operands[1]) != CONST_INT)
        return \"la\t%0, %1\";
      else
        return score_limm (operands);
    case 1: return score_move (operands);
    case 2: return score_linsn (operands, SCORE_WORD, false);
    case 3: return score_sinsn (operands, SCORE_WORD);
    case 4: return TARGET_SCORE7D ? \"mf%1%S0 %0\" : \"mf%1    %0\";
    case 5: return TARGET_SCORE7D ? \"mt%0%S1 %1\" : \"mt%0    %1\";
    case 6: return \"mfsr\t%0, %1\";
    case 7: return \"mtsr\t%1, %0\";
    case 8: return \"mfcr\t%0, %1\";
    case 9: return \"mtcr\t%1, %0\";
    default: gcc_unreachable ();
    }
}
  [(set_attr "type" "arith,move,load,store,fce,tce,fsr,tsr,fcr,tcr")
   (set_attr "mode" "SI")])

(define_insn "*movsi_insns_score3"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=ed,e,d,d,m,d,*x,d,*a")
        (match_operand:SI 1 "general_operand" "i,e,d,m,d,*x,d,*a,d"))]
  "(!MEM_P (operands[0]) || register_operand (operands[1], SImode))
   && (TARGET_SCORE3)"
{
  switch (which_alternative)
    {
    case 0:
      if (GET_CODE (operands[1]) != CONST_INT)
        return \"ldi48\t%0, %1\";
      else
        return score_limm (operands);
    case 1: return \"mv!\t%0, %1\";
    case 2: return \"mv!\t%0, %1\";
    case 3: return score_linsn (operands, SCORE_WORD, false);
    case 4: return score_sinsn (operands, SCORE_WORD);
    case 5: return TARGET_SCORE3D ? \"mf%1%S0\t%0\" : \"mf%1\t%0\";
    case 6: return TARGET_SCORE3D ? \"mt%0%S1\t%1\" : \"mt%0\t%1\";
    case 7: return \"mfsr\t%0, %1\";
    case 8: return \"mtsr\t%1, %0\";
    default: gcc_unreachable ();
    }
}
  [(set_attr "type" "arith,move,move,load,store,fce,tce,fsr,tsr")
   (set_attr "length" "6,2,2,6,6,4,4,4,4")
   (set_attr "mode" "SI")])

(define_insn_and_split "movdi"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=d,d,d,m,d,*x")
        (match_operand:DI 1 "general_operand" "i,d,m,d,*x,d"))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
{
  score_movdi (operands);
  DONE;
})

(define_expand "movsf"
  [(set (match_operand:SF 0 "nonimmediate_operand")
        (match_operand:SF 1 "general_operand"))]
  ""
{
  if (MEM_P (operands[0])
      && !register_operand (operands[1], SFmode))
    {
      operands[1] = force_reg (SFmode, operands[1]);
    }
})

(define_insn "*movsf_insns_score7"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=d,d,d,m")
        (match_operand:SF 1 "general_operand" "i,d,m,d"))]
  "(!MEM_P (operands[0]) || register_operand (operands[1], SFmode))
   && (TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)"
{
  switch (which_alternative)
    {
    case 0: return \"li\t%0, %D1\";;
    case 1: return score_move (operands);
    case 2: return score_linsn (operands, SCORE_WORD, false);
    case 3: return score_sinsn (operands, SCORE_WORD);
    default: gcc_unreachable ();
    }
}
  [(set_attr "type" "arith,move,load,store")
   (set_attr "mode" "SI")])

(define_insn "*movsf_insns_score3"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=d,d,d,m")
        (match_operand:SF 1 "general_operand" "i,d,m,d"))]
  "(!MEM_P (operands[0]) || register_operand (operands[1], SFmode))
   && (TARGET_SCORE3)"
{
  switch (which_alternative)
    {
    case 0: return \"li\t%0, %D1\";
    case 1: return \"mv!\t%0, %1\";
    case 2: return score_linsn (operands, SCORE_WORD, false);
    case 3: return score_sinsn (operands, SCORE_WORD);
    default: gcc_unreachable ();
    }
}
  [(set_attr "type" "arith,move,load,store")
   (set_attr "length" "4,2,6,6")
   (set_attr "mode" "SI")])

(define_insn_and_split "movdf"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=d,d,d,m")
        (match_operand:DF 1 "general_operand" "i,d,m,d"))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
{
  score_movdi (operands);
  DONE;
})

(define_expand "addsi3"
  [(set (match_operand:SI 0 "score_register_operand" )
        (plus:SI (match_operand:SI 1 "score_register_operand")
                 (match_operand:SI 2 "arith_operand")))]
  ""
  ""
)

(define_insn "*addsi3_score7"
  [(set (match_operand:SI 0 "register_operand" "=d,d,d,d")
        (plus:SI (match_operand:SI 1 "register_operand" "0,0,d,d")
                 (match_operand:SI 2 "arith_operand" "I,L,N,d")))]
  "(TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)"
{
  switch (which_alternative)
    {
    case 0: return \"addis\t%0, %U2\";
    case 1: return score_select_add_imm (operands, false);
    case 2: return \"addri\t%0, %1, %c2\";
    case 3: return score_select (operands, "add", true, "", false);
    default: gcc_unreachable ();
    }
}
  [(set_attr "type" "arith")
   (set_attr "mode" "SI")])

(define_insn "*addsi3_score3"
  [(set (match_operand:SI 0 "score_register_operand" "=d,d,d,d")
        (plus:SI (match_operand:SI 1 "score_register_operand" "%0,0,d,d")
                 (match_operand:SI 2 "arith_operand" "I,L,N,d")))]
  "(TARGET_SCORE3)"
{
  switch (which_alternative)
    {
    case 0: return \"addis\t%0, %U2\";
    case 1: return score_select_add_imm (operands, false);
    case 2: return \"addri\t%0, %1, %c2\";
    case 3: return score_select (operands, "add", true, "", false);
    default: gcc_unreachable ();
    }
}
  [(set_attr "type" "arith")
   (set_attr "length" "4,4,4,4")
   (set_attr "mode" "SI")])

(define_insn "*addsi3_cmp_score7"
  [(set (reg:CC_NZ CC_REGNUM)
        (compare:CC_NZ (plus:SI
                        (match_operand:SI 1 "register_operand" "0,0,d,d")
                        (match_operand:SI 2 "arith_operand" "I,L,N,d"))
                       (const_int 0)))
   (clobber (match_scratch:SI 0 "=d,d,d,d"))]
  "(TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)"
{
  switch (which_alternative)
    {
    case 0: return \"addis.c\t%0, %U2\";
    case 1: return score_select_add_imm (operands, true);
    case 2: return \"addri.c\t%0, %1, %c2\";
    case 3: return score_select (operands, "add", true, "", true);
    default: gcc_unreachable ();
    }
}
  [(set_attr "type" "arith")
   (set_attr "up_c" "yes")
   (set_attr "mode" "SI")])

(define_insn "*addsi3_cmp_score3"
  [(set (reg:CC_NZ CC_REGNUM)
        (compare:CC_NZ (plus:SI
                        (match_operand:SI 1 "score_register_operand" "0,0,d,d")
                        (match_operand:SI 2 "arith_operand" "I,L,N,d"))
                       (const_int 0)))
   (clobber (match_scratch:SI 0 "=d,d,d,d"))]
  "(TARGET_SCORE3)"
{
  switch (which_alternative)
    {
    case 0: return \"addis.c\t%0, %U2\";
    case 1: return score_select_add_imm (operands, true);
    case 2: return \"addri.c\t%0, %1, %c2\";
    case 3: return score_select (operands, "add", true, "", true);
    default: gcc_unreachable ();
    }
}
  [(set_attr "type" "arith")
   (set_attr "length" "4,4,4,4")
   (set_attr "up_c" "yes")
   (set_attr "mode" "SI")])

(define_insn "*addsi3_ucc_score7"
  [(set (reg:CC_NZ CC_REGNUM)
        (compare:CC_NZ (plus:SI
                        (match_operand:SI 1 "register_operand" "0,0,d,d")
                        (match_operand:SI 2 "arith_operand" "I,L,N,d"))
                       (const_int 0)))
   (set (match_operand:SI 0 "register_operand" "=d,d,d,d")
        (plus:SI (match_dup 1) (match_dup 2)))]
  "(TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)"
{
  switch (which_alternative)
    {
    case 0: return \"addis.c\t%0, %U2\";
    case 1: return score_select_add_imm (operands, true);
    case 2: return \"addri.c\t%0, %1, %c2\";
    case 3: return score_select (operands, "add", true, "", true);
    default: gcc_unreachable ();
    }
}
  [(set_attr "type" "arith")
   (set_attr "up_c" "yes")
   (set_attr "mode" "SI")])

(define_insn "*addsi3_ucc_score3"
  [(set (reg:CC_NZ CC_REGNUM)
        (compare:CC_NZ (plus:SI
                        (match_operand:SI 1 "score_register_operand" "0,0,d,d")
                        (match_operand:SI 2 "arith_operand" "I,L,N,d"))
                       (const_int 0)))
   (set (match_operand:SI 0 "score_register_operand" "=d,d,d,d")
        (plus:SI (match_dup 1) (match_dup 2)))]
  "(TARGET_SCORE3)"
{
  switch (which_alternative)
    {
    case 0: return \"addis.c\t%0, %U2\";
    case 1: return score_select_add_imm (operands, true);
    case 2: return \"addri.c\t%0, %1, %c2\";
    case 3: return score_select (operands, "add", true, "", true);
    default: gcc_unreachable ();
    }
}
  [(set_attr "type" "arith")
   (set_attr "length" "4,4,4,4")
   (set_attr "up_c" "yes")
   (set_attr "mode" "SI")])

(define_expand "adddi3"
  [(parallel
    [(set (match_operand:DI 0 "score_register_operand")
          (plus:DI (match_operand:DI 1 "score_register_operand")
                   (match_operand:DI 2 "score_register_operand")))
    (clobber (reg:CC CC_REGNUM))])]
  ""
  ""
)

(define_insn "*adddi3_score7"
  [(set (match_operand:DI 0 "register_operand" "=e,d")
        (plus:DI (match_operand:DI 1 "register_operand" "0,d")
                 (match_operand:DI 2 "register_operand" "e,d")))
  (clobber (reg:CC CC_REGNUM))]
  "(TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)"
  "@
   add!    %L0, %L2\;addc!   %H0, %H2
   add.c   %L0, %L1, %L2\;addc    %H0, %H1, %H2"
  [(set_attr "type" "arith")
   (set_attr "mode" "DI")])

(define_insn "*adddi3_score3"
  [(set (match_operand:DI 0 "score_register_operand" "=d")
        (plus:DI (match_operand:DI 1 "score_register_operand" "d")
                 (match_operand:DI 2 "score_register_operand" "d")))
  (clobber (reg:CC CC_REGNUM))]
  "(TARGET_SCORE3)"
  "add.c\t%L0, %L1, %L2\;addc\t%H0, %H1, %H2"
  [(set_attr "type" "arith")
   (set_attr "length" "8")
   (set_attr "mode" "DI")])

(define_expand "subsi3"
  [(set (match_operand:SI 0 "score_register_operand")
        (minus:SI (match_operand:SI 1 "score_register_operand")
                  (match_operand:SI 2 "score_register_operand")))]
  ""
  ""
)

(define_insn "*subsi3_score7"
  [(set (match_operand:SI 0 "register_operand" "=d")
        (minus:SI (match_operand:SI 1 "register_operand" "d")
                  (match_operand:SI 2 "register_operand" "d")))]
  "(TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)"
{
  return score_select (operands, "sub", false, "", false);
}
  [(set_attr "type" "arith")
   (set_attr "mode" "SI")])

(define_insn "*subsi3_score3"
  [(set (match_operand:SI 0 "score_register_operand" "=e,d")
        (minus:SI (match_operand:SI 1 "score_register_operand" "0,d")
                  (match_operand:SI 2 "score_register_operand" "e,d")))]
  "(TARGET_SCORE3)"
{
  switch (which_alternative)
    {
    case 0: return \"sub!\t%0, %2\";
    case 1: return score_select (operands, "sub", false, "", false);
    default: gcc_unreachable ();
    }
}
  [(set_attr "type" "arith")
   (set_attr "length" "2,4")
   (set_attr "mode" "SI")])

(define_insn "*subsi3_cmp_score7"
  [(set (reg:CC_NZ CC_REGNUM)
        (compare:CC_NZ (minus:SI (match_operand:SI 1 "register_operand" "d")
                                 (match_operand:SI 2 "register_operand" "d"))
                       (const_int 0)))
   (clobber (match_scratch:SI 0 "=d"))]
  "(TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)"
{
  return score_select (operands, "sub", false, "", true);
}
  [(set_attr "type" "arith")
   (set_attr "up_c" "yes")
   (set_attr "mode" "SI")])

(define_insn "*subsi3_cmp_score3"
  [(set (reg:CC_NZ CC_REGNUM)
        (compare:CC_NZ (minus:SI (match_operand:SI 1 "score_register_operand" "d")
                                 (match_operand:SI 2 "score_register_operand" "d"))
                       (const_int 0)))
   (clobber (match_scratch:SI 0 "=d"))]
  "(TARGET_SCORE3)"
{
  return score_select (operands, "sub", false, "", true);
}
  [(set_attr "type" "arith")
   (set_attr "length" "4")
   (set_attr "up_c" "yes")
   (set_attr "mode" "SI")])

(define_peephole2
  [(set (match_operand:SI 0 "g32reg_operand" "")
        (minus:SI (match_operand:SI 1 "g32reg_operand" "")
                  (match_operand:SI 2 "g32reg_operand" "")))
   (set (reg:CC CC_REGNUM)
        (compare:CC (match_dup 1) (match_dup 2)))]
  ""
  [(set (reg:CC CC_REGNUM)
        (compare:CC (match_dup 1) (match_dup 2)))
   (set (match_dup 0)
        (minus:SI (match_dup 1) (match_dup 2)))])

(define_insn "subsi3_ucc_pcmp"
  [(parallel
    [(set (reg:CC CC_REGNUM)
          (compare:CC (match_operand:SI 1 "score_register_operand" "d")
                      (match_operand:SI 2 "score_register_operand" "d")))
     (set (match_operand:SI 0 "score_register_operand" "=d")
          (minus:SI (match_dup 1) (match_dup 2)))])]
  ""
{
  return score_select (operands, "sub", false, "", true);
}
  [(set_attr "type" "arith")
   (set_attr "length" "4")
   (set_attr "up_c" "yes")
   (set_attr "mode" "SI")])

(define_insn "subsi3_ucc"
  [(set (reg:CC_NZ CC_REGNUM)
        (compare:CC_NZ (minus:SI (match_operand:SI 1 "score_register_operand" "d")
                                 (match_operand:SI 2 "score_register_operand" "d"))
                       (const_int 0)))
   (set (match_operand:SI 0 "score_register_operand" "=d")
        (minus:SI (match_dup 1) (match_dup 2)))]
  ""
{
  return score_select (operands, "sub", false, "", true);
}
  [(set_attr "type" "arith")
   (set_attr "length" "4")
   (set_attr "up_c" "yes")
   (set_attr "mode" "SI")])

(define_expand "subdi3"
  [(parallel
    [(set (match_operand:DI 0 "score_register_operand")
          (minus:DI (match_operand:DI 1 "score_register_operand")
                    (match_operand:DI 2 "score_register_operand")))
     (clobber (reg:CC CC_REGNUM))])]
  ""
  ""
)

(define_insn "*subdi3_score7"
  [(set (match_operand:DI 0 "register_operand" "=e,d")
        (minus:DI (match_operand:DI 1 "register_operand" "0,d")
                  (match_operand:DI 2 "register_operand" "e,d")))
   (clobber (reg:CC CC_REGNUM))]
  "(TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)"
  "@
   sub!    %L0, %L2\;subc    %H0, %H1, %H2
   sub.c   %L0, %L1, %L2\;subc    %H0, %H1, %H2"
  [(set_attr "type" "arith")
   (set_attr "mode" "DI")])

(define_insn "*subdi3_score3"
  [(set (match_operand:DI 0 "score_register_operand" "=d")
        (minus:DI (match_operand:DI 1 "score_register_operand" "d")
                  (match_operand:DI 2 "score_register_operand" "d")))
   (clobber (reg:CC CC_REGNUM))]
  "(TARGET_SCORE3)"
  "sub.c\t%L0, %L1, %L2\;subc\t%H0, %H1, %H2"
  [(set_attr "type" "arith")
   (set_attr "length" "8")
   (set_attr "mode" "DI")])

(define_expand "andsi3"
  [(set (match_operand:SI 0 "score_register_operand")
        (and:SI (match_operand:SI 1 "score_register_operand")
                (match_operand:SI 2 "arith_operand")))]
  ""
  ""
)

(define_insn "*andsi3_score7"
  [(set (match_operand:SI 0 "register_operand" "=d,d,d,d")
        (and:SI (match_operand:SI 1 "register_operand" "0,0,d,d")
                (match_operand:SI 2 "arith_operand" "I,K,M,d")))]
  "(TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)"
{
  switch (which_alternative)
    {
    case 0: return \"andis\t%0, %U2\";
    case 1: return \"andi\t%0, %c2";
    case 2: return \"andri\t%0, %1, %c2\";
    case 3: return score_select (operands, "and", true, "", false);
    default: gcc_unreachable ();
    }
}
  [(set_attr "type" "arith")
   (set_attr "mode" "SI")])

(define_insn "*andsi3_score3"
  [(set (match_operand:SI 0 "score_register_operand" "=e,d,d,d,d")
        (and:SI (match_operand:SI 1 "score_register_operand" "%0,0,0,d,d")
                (match_operand:SI 2 "arith_operand" "e,I,K,M,d")))]
  "(TARGET_SCORE3)"
{
  switch (which_alternative)
    {
    case 0: return \"and!\t%0, %2\";
    case 1: return \"andis\t%0, %U2\";
    case 2: return \"andi\t%0, %c2";
    case 3: return \"andri\t%0, %1, %c2\";
    case 4: return score_select (operands, "and", true, "", false);
    default: gcc_unreachable ();
    }
}
  [(set_attr "type" "arith")
   (set_attr "length" "2,4,4,4,4")
   (set_attr "mode" "SI")])

(define_insn "andsi3_cmp_score7"
  [(set (reg:CC_NZ CC_REGNUM)
        (compare:CC_NZ (and:SI (match_operand:SI 1 "register_operand" "0,0,0,d")
                               (match_operand:SI 2 "arith_operand" "I,K,M,d"))
                       (const_int 0)))
   (clobber (match_scratch:SI 0 "=d,d,d,d"))]
  "(TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)"
{
  switch (which_alternative)
    {
    case 0: return \"andis.c\t%0, %U2\";
    case 1: return \"andi.c\t%0, %c2";
    case 2: return \"andri.c\t%0, %1, %c2\";
    case 3: return score_select (operands, "and", true, "", true);
    default: gcc_unreachable ();
    }
}
  [(set_attr "type" "arith")
   (set_attr "up_c" "yes")
   (set_attr "mode" "SI")])

(define_insn "andsi3_cmp_score3"
  [(set (reg:CC_NZ CC_REGNUM)
        (compare:CC_NZ (and:SI (match_operand:SI 1 "score_register_operand" "0,0,0,d")
                               (match_operand:SI 2 "arith_operand" "I,K,M,d"))
                       (const_int 0)))
   (clobber (match_scratch:SI 0 "=d,d,d,d"))]
  ""
{
  switch (which_alternative)
    {
    case 0: return \"andis.c\t%0, %U2\";
    case 1: return \"andi.c\t%0, %c2";
    case 2: return \"andri.c\t%0, %1, %c2\";
    case 3: return score_select (operands, "and", true, "", true);
    default: gcc_unreachable ();
    }
}
  [(set_attr "type" "arith")
   (set_attr "length" "4,4,4,4")
   (set_attr "up_c" "yes")
   (set_attr "mode" "SI")])

(define_insn "*andsi3_ucc_score7"
  [(set (reg:CC_NZ CC_REGNUM)
        (compare:CC_NZ (and:SI
                        (match_operand:SI 1 "register_operand" "0,0,d,d")
                        (match_operand:SI 2 "arith_operand" "I,K,M,d"))
                       (const_int 0)))
   (set (match_operand:SI 0 "register_operand" "=d,d,d,d")
        (and:SI (match_dup 1) (match_dup 2)))]
  "(TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)"
{
  switch (which_alternative)
    {
    case 0: return \"andis.c\t%0, %U2\";
    case 1: return \"andi.c\t%0, %c2";
    case 2: return \"andri.c\t%0, %1, %c2\";
    case 3: return score_select (operands, "and", true, "", true);
    default: gcc_unreachable ();
    }
}
  [(set_attr "type" "arith")
   (set_attr "up_c" "yes")
   (set_attr "mode" "SI")])

(define_insn "*andsi3_ucc_score3"
  [(set (reg:CC_NZ CC_REGNUM)
        (compare:CC_NZ (and:SI
                        (match_operand:SI 1 "score_register_operand" "0,0,d,d")
                        (match_operand:SI 2 "arith_operand" "I,K,M,d"))
                       (const_int 0)))
   (set (match_operand:SI 0 "score_register_operand" "=d,d,d,d")
        (and:SI (match_dup 1) (match_dup 2)))]
  "(TARGET_SCORE3)"
{
  switch (which_alternative)
    {
    case 0: return \"andis.c\t%0, %U2\";
    case 1: return \"andi.c\t%0, %c2";
    case 2: return \"andri.c\t%0, %1, %c2\";
    case 3: return score_select (operands, "and", true, "", true);
    default: gcc_unreachable ();
    }
}
  [(set_attr "type" "arith")
   (set_attr "length" "4,4,4,4")
   (set_attr "up_c" "yes")
   (set_attr "mode" "SI")])


(define_insn_and_split "*zero_extract_andi"
  [(set (reg:CC CC_REGNUM)
        (compare:CC (zero_extract:SI
                     (match_operand:SI 0 "score_register_operand" "d")
                     (match_operand:SI 1 "const_uimm5" "")
                     (match_operand:SI 2 "const_uimm5" ""))
                    (const_int 0)))]
  ""
  "#"
  ""
  [(const_int 1)]
{
  score_zero_extract_andi (operands);
  DONE;
})

(define_expand "iorsi3"
  [(set (match_operand:SI 0 "score_register_operand")
        (ior:SI (match_operand:SI 1 "score_register_operand")
                (match_operand:SI 2 "arith_operand")))]
  ""
  ""
)

(define_insn "*iorsi3_score7"
  [(set (match_operand:SI 0 "register_operand" "=d,d,d,d")
        (ior:SI (match_operand:SI 1 "register_operand" "0,0,d,d")
                (match_operand:SI 2 "arith_operand" "I,K,M,d")))]
  "(TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)"
{
  switch (which_alternative)
    {
    case 0: return \"oris\t%0, %U2\";
    case 1: return \"ori\t%0, %c2\";
    case 2: return \"orri\t%0, %1, %c2\";
    case 3: return score_select (operands, "or", true, "", false);
    default: gcc_unreachable ();
    }
}
  [(set_attr "type" "arith")
   (set_attr "mode" "SI")])

(define_insn "*iorsi3_score3"
  [(set (match_operand:SI 0 "score_register_operand" "=e,d,d,d,d")
        (ior:SI (match_operand:SI 1 "score_register_operand" "%0,0,0,d,d")
                (match_operand:SI 2 "arith_operand" "e,I,K,M,d")))]
  "(TARGET_SCORE3)"
{
  switch (which_alternative)
    {
    case 0: return \"or!\t%0, %2\";
    case 1: return \"oris\t%0, %U2\";
    case 2: return \"ori\t%0, %c2\";
    case 3: return \"orri\t%0, %1, %c2\";
    case 4: return score_select (operands, "or", true, "", false);
    default: gcc_unreachable ();
    }
}
  [(set_attr "type" "arith")
   (set_attr "length" "2,4,4,4,4")
   (set_attr "mode" "SI")])

(define_insn "*iorsi3_ucc_score7"
  [(set (reg:CC_NZ CC_REGNUM)
        (compare:CC_NZ (ior:SI
                        (match_operand:SI 1 "register_operand" "0,0,d,d")
                        (match_operand:SI 2 "arith_operand" "I,K,M,d"))
                       (const_int 0)))
   (set (match_operand:SI 0 "register_operand" "=d,d,d,d")
        (ior:SI (match_dup 1) (match_dup 2)))]
  "(TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)"
{
  switch (which_alternative)
    {
    case 0: return \"oris.c\t%0, %U2\";
    case 1: return \"ori.c\t%0, %c2\";
    case 2: return \"orri.c\t%0, %1, %c2\";
    case 3: return score_select (operands, "or", true, "", true);
    default: gcc_unreachable ();
    }
}
  [(set_attr "type" "arith")
   (set_attr "up_c" "yes")
   (set_attr "mode" "SI")])

(define_insn "*iorsi3_ucc_score3"
  [(set (reg:CC_NZ CC_REGNUM)
        (compare:CC_NZ (ior:SI
                        (match_operand:SI 1 "score_register_operand" "0,0,d,d")
                        (match_operand:SI 2 "arith_operand" "I,K,M,d"))
                       (const_int 0)))
   (set (match_operand:SI 0 "score_register_operand" "=d,d,d,d")
        (ior:SI (match_dup 1) (match_dup 2)))]
  "(TARGET_SCORE3)"
{
  switch (which_alternative)
    {
    case 0: return \"oris.c\t%0, %U2\";
    case 1: return \"ori.c\t%0, %c2\";
    case 2: return \"orri.c\t%0, %1, %c2\";
    case 3: return score_select (operands, "or", true, "", true);
    default: gcc_unreachable ();
    }
}
  [(set_attr "type" "arith")
   (set_attr "length" "4,4,4,4")
   (set_attr "up_c" "yes")
   (set_attr "mode" "SI")])

(define_insn "*iorsi3_cmp_score7"
  [(set (reg:CC_NZ CC_REGNUM)
        (compare:CC_NZ (ior:SI
                        (match_operand:SI 1 "register_operand" "0,0,d,d")
                        (match_operand:SI 2 "arith_operand" "I,K,M,d"))
                       (const_int 0)))
   (clobber (match_scratch:SI 0 "=d,d,d,d"))]
  "(TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)"
{
  switch (which_alternative)
    {
    case 0: return \"oris.c\t%0, %U2\";
    case 1: return \"ori.c\t%0, %c2\";
    case 2: return \"orri.c\t%0, %1, %c2\";
    case 3: return score_select (operands, "or", true, "", true);
    default: gcc_unreachable ();
    }
}
  [(set_attr "type" "arith")
   (set_attr "up_c" "yes")
   (set_attr "mode" "SI")])

(define_insn "*iorsi3_cmp_score3"
  [(set (reg:CC_NZ CC_REGNUM)
        (compare:CC_NZ (ior:SI
                        (match_operand:SI 1 "score_register_operand" "0,0,d,d")
                        (match_operand:SI 2 "arith_operand" "I,K,M,d"))
                       (const_int 0)))
   (clobber (match_scratch:SI 0 "=d,d,d,d"))]
  "(TARGET_SCORE3)"
{
  switch (which_alternative)
    {
    case 0: return \"oris.c\t%0, %U2\";
    case 1: return \"ori.c\t%0, %c2\";
    case 2: return \"orri.c\t%0, %1, %c2\";
    case 3: return score_select (operands, "or", true, "", true);
    default: gcc_unreachable ();
    }
}
  [(set_attr "type" "arith")
   (set_attr "length" "4,4,4,4")
   (set_attr "up_c" "yes")
   (set_attr "mode" "SI")])

(define_expand "xorsi3"
  [(set (match_operand:SI 0 "score_register_operand")
        (xor:SI (match_operand:SI 1 "score_register_operand")
                (match_operand:SI 2 "score_register_operand")))]
  ""
  ""
)

(define_insn "*xorsi3_score7"
  [(set (match_operand:SI 0 "register_operand" "=d")
        (xor:SI (match_operand:SI 1 "register_operand" "d")
                (match_operand:SI 2 "register_operand" "d")))]
  "(TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)"
{
  return score_select (operands, "xor", true, "", false);
}
  [(set_attr "type" "arith")
   (set_attr "mode" "SI")])

(define_insn "*xorsi3_score3"
  [(set (match_operand:SI 0 "score_register_operand" "=d")
        (xor:SI (match_operand:SI 1 "score_register_operand" "d")
                (match_operand:SI 2 "score_register_operand" "d")))]
  "(TARGET_SCORE3)"
{
  return score_select (operands, "xor", true, "", false);
}
  [(set_attr "type" "arith")
   (set_attr "length" "4")
   (set_attr "mode" "SI")])


(define_insn "*xorsi3_ucc_score7"
  [(set (reg:CC_NZ CC_REGNUM)
        (compare:CC_NZ (xor:SI (match_operand:SI 1 "register_operand" "d")
                               (match_operand:SI 2 "register_operand" "d"))
                       (const_int 0)))
   (set (match_operand:SI 0 "register_operand" "=d")
        (xor:SI (match_dup 1) (match_dup 2)))]
  "(TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)"
{
  return score_select (operands, "xor", true, "", true);
}
  [(set_attr "type" "arith")
   (set_attr "up_c" "yes")
   (set_attr "mode" "SI")])

(define_insn "*xorsi3_ucc_score3"
  [(set (reg:CC_NZ CC_REGNUM)
        (compare:CC_NZ (xor:SI (match_operand:SI 1 "score_register_operand" "d")
                               (match_operand:SI 2 "score_register_operand" "d"))
                       (const_int 0)))
   (set (match_operand:SI 0 "score_register_operand" "=d")
        (xor:SI (match_dup 1) (match_dup 2)))]
  "(TARGET_SCORE3)"
{
  return score_select (operands, "xor", true, "", true);
}
  [(set_attr "type" "arith")
   (set_attr "length" "4")
   (set_attr "up_c" "yes")
   (set_attr "mode" "SI")])


(define_insn "*xorsi3_cmp_score7"
  [(set (reg:CC_NZ CC_REGNUM)
        (compare:CC_NZ (xor:SI (match_operand:SI 1 "register_operand" "d")
                               (match_operand:SI 2 "register_operand" "d"))
                       (const_int 0)))
   (clobber (match_scratch:SI 0 "=d"))]
  ""
{
  return score_select (operands, "xor", true, "", true);
}
  [(set_attr "type" "arith")
   (set_attr "up_c" "yes")
   (set_attr "mode" "SI")])

(define_insn "*xorsi3_cmp_score3"
  [(set (reg:CC_NZ CC_REGNUM)
        (compare:CC_NZ (xor:SI (match_operand:SI 1 "score_register_operand" "d")
                               (match_operand:SI 2 "score_register_operand" "d"))
                       (const_int 0)))
   (clobber (match_scratch:SI 0 "=d"))]
  ""
{
  return score_select (operands, "xor", true, "", true);
}
  [(set_attr "type" "arith")
   (set_attr "length" "4")
   (set_attr "up_c" "yes")
   (set_attr "mode" "SI")])

(define_expand "extendqisi2"
  [(set (match_operand:SI 0 "score_register_operand")
        (sign_extend:SI (match_operand:QI 1 "nonimmediate_operand")))]
  ""
  ""
)

(define_insn "*extendqisi2_score7"
  [(set (match_operand:SI 0 "register_operand" "=d,d")
        (sign_extend:SI (match_operand:QI 1 "nonimmediate_operand" "d,m")))]
  "(TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)"
{
  switch (which_alternative)
    {
    case 0: return \"extsb\t%0, %1\";
    case 1: return score_linsn (operands, SCORE_BYTE, true);
    default: gcc_unreachable ();
    }
}
  [(set_attr "type" "arith,load")
   (set_attr "mode" "SI")])

(define_insn "*extendqisi2_score3"
  [(set (match_operand:SI 0 "score_register_operand" "=d,d")
        (sign_extend:SI (match_operand:QI 1 "nonimmediate_operand" "d,m")))]
  "(TARGET_SCORE3)"
{
  switch (which_alternative)
    {
    case 0: return \"extsb\t%0, %1\";
    case 1: return score_linsn (operands, SCORE_BYTE, true);
    default: gcc_unreachable ();
    }
}
  [(set_attr "type" "arith,load")
   (set_attr "length" "4,4")
   (set_attr "mode" "SI")])

(define_insn "*extendqisi2_ucc_score7"
  [(set (reg:CC_N CC_REGNUM)
        (compare:CC_N (ashiftrt:SI
                       (ashift:SI (match_operand:SI 1 "register_operand" "d")
                                  (const_int 24))
                       (const_int 24))
                      (const_int 0)))
   (set (match_operand:SI 0 "register_operand" "=d")
        (sign_extend:SI (match_operand:QI 2 "register_operand" "0")))]
  "(TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)"
  "extsb.c %0, %1"
  [(set_attr "type" "arith")
   (set_attr "up_c" "yes")
   (set_attr "mode" "SI")])

(define_insn "*extendqisi2_ucc_score3"
  [(set (reg:CC_N CC_REGNUM)
        (compare:CC_N (ashiftrt:SI
                       (ashift:SI (match_operand:SI 1 "score_register_operand" "d")
                                  (const_int 24))
                       (const_int 24))
                      (const_int 0)))
   (set (match_operand:SI 0 "score_register_operand" "=d")
        (sign_extend:SI (match_operand:QI 2 "score_register_operand" "0")))]
  "(TARGET_SCORE3)"
  "extsb.c\t%0, %1"
  [(set_attr "type" "arith")
   (set_attr "length" "4")
   (set_attr "up_c" "yes")
   (set_attr "mode" "SI")])

(define_insn "*extendqisi2_cmp_score7"
  [(set (reg:CC_N CC_REGNUM)
        (compare:CC_N (ashiftrt:SI
                       (ashift:SI (match_operand:SI 1 "register_operand" "d")
                                  (const_int 24))
                       (const_int 24))
                      (const_int 0)))
   (clobber (match_scratch:SI 0 "=d"))]
  "(TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)"
  "extsb.c %0, %1"
  [(set_attr "type" "arith")
   (set_attr "up_c" "yes")
   (set_attr "mode" "SI")])

(define_insn "*extendqisi2_cmp_score3"
  [(set (reg:CC_N CC_REGNUM)
        (compare:CC_N (ashiftrt:SI
                       (ashift:SI (match_operand:SI 1 "score_register_operand" "d")
                                  (const_int 24))
                       (const_int 24))
                      (const_int 0)))
   (clobber (match_scratch:SI 0 "=d"))]
  "(TARGET_SCORE3)"
  "extsb.c\t%0, %1"
  [(set_attr "type" "arith")
   (set_attr "length" "4")
   (set_attr "up_c" "yes")
   (set_attr "mode" "SI")])

(define_expand "extendhisi2"
  [(set (match_operand:SI 0 "score_register_operand")
        (sign_extend:SI (match_operand:HI 1 "nonimmediate_operand")))]
  ""
  ""
)

(define_insn "*extendhisi2_score7"
  [(set (match_operand:SI 0 "register_operand" "=d,d")
        (sign_extend:SI (match_operand:HI 1 "nonimmediate_operand" "d,m")))]
  "(TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)"
{
  switch (which_alternative)
    {
    case 0: return \"extsh\t%0, %1\";
    case 1: return score_linsn (operands, SCORE_HWORD, true);
    default: gcc_unreachable ();
    }
}
  [(set_attr "type" "arith, load")
   (set_attr "mode" "SI")])

(define_insn "*extendhisi2_score3"
  [(set (match_operand:SI 0 "score_register_operand" "=d,d")
        (sign_extend:SI (match_operand:HI 1 "nonimmediate_operand" "d,m")))]
  "(TARGET_SCORE3)"
{
  switch (which_alternative)
    {
    case 0: return \"extsh\t%0, %1\";
    case 1: return score_linsn (operands, SCORE_HWORD, true);
    default: gcc_unreachable ();
    }
}
  [(set_attr "type" "arith, load")
   (set_attr "length" "4,4")
   (set_attr "mode" "SI")])

(define_insn "*extendhisi2_ucc_score7"
  [(set (reg:CC_N CC_REGNUM)
        (compare:CC_N (ashiftrt:SI
                       (ashift:SI (match_operand:SI 1 "register_operand" "d")
                                  (const_int 16))
                       (const_int 16))
                      (const_int 0)))
  (set (match_operand:SI 0 "register_operand" "=d")
       (sign_extend:SI (match_operand:HI 2 "register_operand" "0")))]
  "(TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)"
  "extsh.c %0, %1"
  [(set_attr "type" "arith")
   (set_attr "up_c" "yes")
   (set_attr "mode" "SI")])

(define_insn "*extendhisi2_ucc_score3"
  [(set (reg:CC_N CC_REGNUM)
        (compare:CC_N (ashiftrt:SI
                       (ashift:SI (match_operand:SI 1 "score_register_operand" "d")
                                  (const_int 16))
                       (const_int 16))
                      (const_int 0)))
  (set (match_operand:SI 0 "score_register_operand" "=d")
       (sign_extend:SI (match_operand:HI 2 "score_register_operand" "0")))]
  "(TARGET_SCORE3)"
  "extsh.c\t%0, %1"
  [(set_attr "type" "arith")
   (set_attr "length" "4")
   (set_attr "up_c" "yes")
   (set_attr "mode" "SI")])

(define_insn "*extendhisi2_cmp_score7"
  [(set (reg:CC_N CC_REGNUM)
        (compare:CC_N (ashiftrt:SI
                       (ashift:SI (match_operand:SI 1 "register_operand" "d")
                                  (const_int 16))
                       (const_int 16))
                      (const_int 0)))
   (clobber (match_scratch:SI 0 "=d"))]
  "(TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)"
  "extsh.c %0, %1"
  [(set_attr "type" "arith")
   (set_attr "up_c" "yes")
   (set_attr "mode" "SI")])

(define_insn "*extendhisi2_cmp_score3"
  [(set (reg:CC_N CC_REGNUM)
        (compare:CC_N (ashiftrt:SI
                       (ashift:SI (match_operand:SI 1 "score_register_operand" "d")
                                  (const_int 16))
                       (const_int 16))
                      (const_int 0)))
   (clobber (match_scratch:SI 0 "=d"))]
  "(TARGET_SCORE3)"
  "extsh.c\t%0, %1"
  [(set_attr "type" "arith")
   (set_attr "length" "4")
   (set_attr "up_c" "yes")
   (set_attr "mode" "SI")])

(define_expand "zero_extendqisi2"
  [(set (match_operand:SI 0 "score_register_operand")
        (zero_extend:SI (match_operand:QI 1 "nonimmediate_operand")))]
  ""
  ""
)

(define_insn "*zero_extendqisi2_score7"
  [(set (match_operand:SI 0 "register_operand" "=d,d")
        (zero_extend:SI (match_operand:QI 1 "nonimmediate_operand" "d,m")))]
  "(TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)"
{
  switch (which_alternative)
    {
    case 0: return \"extzb\t%0, %1\";
    case 1: return score_linsn (operands, SCORE_BYTE, false);
    default: gcc_unreachable ();
    }
}
  [(set_attr "type" "arith, load")
   (set_attr "mode" "SI")])

(define_insn "*zero_extendqisi2_score3"
  [(set (match_operand:SI 0 "score_register_operand" "=d,d")
        (zero_extend:SI (match_operand:QI 1 "nonimmediate_operand" "d,m")))]
  "(TARGET_SCORE3)"
{
  switch (which_alternative)
    {
    case 0: return \"extzb\t%0, %1\";
    case 1: return score_linsn (operands, SCORE_BYTE, false);
    default: gcc_unreachable ();
    }
}
  [(set_attr "type" "arith, load")
   (set_attr "length" "4,4")
   (set_attr "mode" "SI")])

(define_insn "*zero_extendqisi2_ucc_score7"
  [(set (reg:CC_N CC_REGNUM)
        (compare:CC_N (lshiftrt:SI
                       (ashift:SI (match_operand:SI 1 "register_operand" "d")
                                  (const_int 24))
                       (const_int 24))
                      (const_int 0)))
   (set (match_operand:SI 0 "register_operand" "=d")
        (zero_extend:SI (match_operand:QI 2 "register_operand" "0")))]
  "(TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)"
  "extzb.c %0, %1"
  [(set_attr "type" "arith")
   (set_attr "up_c" "yes")
   (set_attr "mode" "SI")])

(define_insn "*zero_extendqisi2_ucc_score3"
  [(set (reg:CC_N CC_REGNUM)
        (compare:CC_N (lshiftrt:SI
                       (ashift:SI (match_operand:SI 1 "score_register_operand" "d")
                                  (const_int 24))
                       (const_int 24))
                      (const_int 0)))
   (set (match_operand:SI 0 "score_register_operand" "=d")
        (zero_extend:SI (match_operand:QI 2 "score_register_operand" "0")))]
  "(TARGET_SCORE3)"
  "extzb.c\t%0, %1"
  [(set_attr "type" "arith")
   (set_attr "length" "4")
   (set_attr "up_c" "yes")
   (set_attr "mode" "SI")])

(define_insn "*zero_extendqisi2_cmp_score7"
  [(set (reg:CC_N CC_REGNUM)
        (compare:CC_N (lshiftrt:SI
                       (ashift:SI (match_operand:SI 1 "register_operand" "d")
                                  (const_int 24))
                       (const_int 24))
                      (const_int 0)))
   (clobber (match_scratch:SI 0 "=d"))]
  "(TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)"
  "extzb.c %0, %1"
  [(set_attr "type" "arith")
   (set_attr "up_c" "yes")
   (set_attr "mode" "SI")])

(define_insn "*zero_extendqisi2_cmp_score3"
  [(set (reg:CC_N CC_REGNUM)
        (compare:CC_N (lshiftrt:SI
                       (ashift:SI (match_operand:SI 1 "score_register_operand" "d")
                                  (const_int 24))
                       (const_int 24))
                      (const_int 0)))
   (clobber (match_scratch:SI 0 "=d"))]
  "(TARGET_SCORE3)"
  "extzb.c\t%0, %1"
  [(set_attr "type" "arith")
   (set_attr "length" "4")
   (set_attr "up_c" "yes")
   (set_attr "mode" "SI")])

(define_expand "zero_extendhisi2"
  [(set (match_operand:SI 0 "score_register_operand")
        (zero_extend:SI (match_operand:HI 1 "nonimmediate_operand")))]
  ""
  ""
)

(define_insn "*zero_extendhisi2_score7"
  [(set (match_operand:SI 0 "register_operand" "=d,d")
        (zero_extend:SI (match_operand:HI 1 "nonimmediate_operand" "d,m")))]
  "(TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)"
{
  switch (which_alternative)
    {
    case 0: return \"extzh\t%0, %1\";
    case 1: return score_linsn (operands, SCORE_HWORD, false);
    default: gcc_unreachable ();
    }
}
  [(set_attr "type" "arith, load")
   (set_attr "mode" "SI")])

(define_insn "*zero_extendhisi2_score3"
  [(set (match_operand:SI 0 "score_register_operand" "=d,d")
        (zero_extend:SI (match_operand:HI 1 "nonimmediate_operand" "d,m")))]
  "(TARGET_SCORE3)"
{
  switch (which_alternative)
    {
    case 0: return \"extzh\t%0, %1\";
    case 1: return score_linsn (operands, SCORE_HWORD, false);
    default: gcc_unreachable ();
    }
}
  [(set_attr "type" "arith, load")
   (set_attr "length" "4,4")
   (set_attr "mode" "SI")])

(define_insn "*zero_extendhisi2_ucc_score7"
  [(set (reg:CC_N CC_REGNUM)
        (compare:CC_N (lshiftrt:SI
                       (ashift:SI (match_operand:SI 1 "register_operand" "d")
                                  (const_int 16))
                       (const_int 16))
                      (const_int 0)))
  (set (match_operand:SI 0 "register_operand" "=d")
       (zero_extend:SI (match_operand:HI 2 "register_operand" "0")))]
  "(TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)"
  "extzh.c %0, %1"
  [(set_attr "type" "arith")
   (set_attr "up_c" "yes")
   (set_attr "mode" "SI")])

(define_insn "*zero_extendhisi2_ucc_score3"
  [(set (reg:CC_N CC_REGNUM)
        (compare:CC_N (lshiftrt:SI
                       (ashift:SI (match_operand:SI 1 "score_register_operand" "d")
                                  (const_int 16))
                       (const_int 16))
                      (const_int 0)))
  (set (match_operand:SI 0 "score_register_operand" "=d")
       (zero_extend:SI (match_operand:HI 2 "score_register_operand" "0")))]
  "(TARGET_SCORE3)"
  "extzh.c\t%0, %1"
  [(set_attr "type" "arith")
   (set_attr "length" "4")
   (set_attr "up_c" "yes")
   (set_attr "mode" "SI")])

(define_insn "*zero_extendhisi2_cmp_score7"
  [(set (reg:CC_N CC_REGNUM)
        (compare:CC_N (lshiftrt:SI
                       (ashift:SI (match_operand:SI 1 "register_operand" "d")
                                  (const_int 16))
                       (const_int 16))
                      (const_int 0)))
   (clobber (match_scratch:SI 0 "=d"))]
  "(TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)"
  "extzh.c %0, %1"
  [(set_attr "type" "arith")
   (set_attr "up_c" "yes")
   (set_attr "mode" "SI")])

(define_insn "*zero_extendhisi2_cmp_score3"
  [(set (reg:CC_N CC_REGNUM)
        (compare:CC_N (lshiftrt:SI
                       (ashift:SI (match_operand:SI 1 "score_register_operand" "d")
                                  (const_int 16))
                       (const_int 16))
                      (const_int 0)))
   (clobber (match_scratch:SI 0 "=d"))]
  "(TARGET_SCORE3)"
  "extzh.c\t%0, %1"
  [(set_attr "type" "arith")
   (set_attr "length" "4")
   (set_attr "up_c" "yes")
   (set_attr "mode" "SI")])

(define_expand "mulsi3"
    [(set (match_operand:SI 0 "score_register_operand")
          (mult:SI (match_operand:SI 1 "score_register_operand")
                   (match_operand:SI 2 "score_register_operand")))]
  ""
{
  if (TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)
    emit_insn (gen_mulsi3_score7 (operands[0], operands[1], operands[2]));
  else if (TARGET_SCORE3)
    emit_insn (gen_mulsi3_score3 (operands[0], operands[1], operands[2]));
  DONE;
})

(define_insn "mulsi3_score7"
  [(set (match_operand:SI 0 "register_operand" "=l")
        (mult:SI (match_operand:SI 1 "register_operand" "d")
                 (match_operand:SI 2 "register_operand" "d")))
   (clobber (reg:SI HI_REGNUM))]
  "(TARGET_SCORE5 || TARGET_SCORE7 || TARGET_SCORE7D)"
  "mul     %1, %2"
  [(set_attr "type" "mul")
   (set_attr "mode" "SI")])

(define_insn "mulsi3_score3"
  [(set (match_operand:SI 0 "score_register_operand" "=d")
        (mult:SI (match_operand:SI 1 "score_register_operand" "d")
                 (match_operand:SI 2 "score_register_operand" "d")))]
  "(TARGET_SCORE3)"
  "mulr.l\t%0, %1, %2"
  [(set_attr "type" "mul")
   (set_attr "length" "4")
   (set_attr "mode" "SI")])

(define_expand "mulsidi3"
    [(set (match_operand:DI 0 "score_register_operand")
          (mult:DI (sign_extend:DI
                    (match_operand:SI 1 "score_register_operand"))
                   (sign_extend:DI
                    (match_operand:SI 2 "score_register_operand"))))]
  ""
{
  if (TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)
    emit_insn (gen_mulsidi3_score7 (operands[0], operands[1], operands[2]));
  else if (TARGET_SCORE3)
    emit_insn (gen_mulsidi3_score3 (operands[0], operands[1], operands[2]));
  DONE;
})

(define_insn "mulsidi3_score7"
  [(set (match_operand:DI 0 "register_operand" "=x")
        (mult:DI (sign_extend:DI
                  (match_operand:SI 1 "register_operand" "d"))
                 (sign_extend:DI
                  (match_operand:SI 2 "register_operand" "d"))))]
  "(TARGET_SCORE5 || TARGET_SCORE7 || TARGET_SCORE7D)"
  "mul     %1, %2"
  [(set_attr "type" "mul")
   (set_attr "mode" "DI")])

(define_insn "mulsidi3_score3"
  [(set (match_operand:DI 0 "score_register_operand" "=d")
        (mult:DI (sign_extend:DI
                  (match_operand:SI 1 "score_register_operand" "d"))
                 (sign_extend:DI
                  (match_operand:SI 2 "score_register_operand" "d"))))]
  "(TARGET_SCORE3)"
  "mulr\t%0, %1, %2"
  [(set_attr "type" "mul")
   (set_attr "length" "4")
   (set_attr "mode" "DI")])

(define_expand "umulsidi3"
  [(set (match_operand:DI 0 "score_register_operand")
        (mult:DI (zero_extend:DI
                  (match_operand:SI 1 "score_register_operand"))
                 (zero_extend:DI
                  (match_operand:SI 2 "score_register_operand"))))]
  ""
{
  if (TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)
    emit_insn (gen_umulsidi3_score7 (operands[0], operands[1], operands[2]));
  else if (TARGET_SCORE3)
    emit_insn (gen_umulsidi3_score3 (operands[0], operands[1], operands[2]));
  DONE;
})

(define_insn "umulsidi3_score7"
  [(set (match_operand:DI 0 "register_operand" "=x")
        (mult:DI (zero_extend:DI
                  (match_operand:SI 1 "register_operand" "d"))
                 (zero_extend:DI
                  (match_operand:SI 2 "register_operand" "d"))))]
  "(TARGET_SCORE5 || TARGET_SCORE7 || TARGET_SCORE7D)"
  "mulu    %1, %2"
  [(set_attr "type" "mul")
   (set_attr "mode" "DI")])

(define_insn "umulsidi3_score3"
  [(set (match_operand:DI 0 "score_register_operand" "=d")
        (mult:DI (zero_extend:DI
                  (match_operand:SI 1 "score_register_operand" "d"))
                 (zero_extend:DI
                  (match_operand:SI 2 "score_register_operand" "d"))))]
  "(TARGET_SCORE3)"
  "mulur\t%0, %1, %2"
  [(set_attr "type" "mul")
   (set_attr "length" "4")
   (set_attr "mode" "DI")])

(define_expand "divmodsi4"
  [(parallel
    [(set (match_operand:SI 0 "score_register_operand")
          (div:SI (match_operand:SI 1 "score_register_operand")
                  (match_operand:SI 2 "score_register_operand")))
     (set (match_operand:SI 3 "score_register_operand")
          (mod:SI (match_dup 1) (match_dup 2)))])]
  ""
  ""
)

(define_insn "*divmodsi4_score7"
  [(set (match_operand:SI 0 "register_operand" "=l")
        (div:SI (match_operand:SI 1 "register_operand" "d")
                (match_operand:SI 2 "register_operand" "d")))
   (set (match_operand:SI 3 "register_operand" "=h")
        (mod:SI (match_dup 1) (match_dup 2)))]
  "(TARGET_SCORE5 || TARGET_SCORE7 || TARGET_SCORE7D)"
  "div     %1, %2"
  [(set_attr "type" "div")
   (set_attr "mode" "SI")])

(define_insn "*divmodsi4_score3"
  [(set (match_operand:SI 0 "score_register_operand" "=l")
        (div:SI (match_operand:SI 1 "score_register_operand" "d")
                (match_operand:SI 2 "score_register_operand" "d")))
   (set (match_operand:SI 3 "score_register_operand" "=h")
        (mod:SI (match_dup 1) (match_dup 2)))]
  "(TARGET_SCORE3)"
  "div\t%1, %2"
  [(set_attr "type" "div")
   (set_attr "mode" "SI")])

(define_expand "udivmodsi4"
  [(parallel
    [(set (match_operand:SI 0 "score_register_operand")
          (udiv:SI (match_operand:SI 1 "score_register_operand")
                   (match_operand:SI 2 "score_register_operand")))
     (set (match_operand:SI 3 "score_register_operand")
          (umod:SI (match_dup 1) (match_dup 2)))])]
  ""
  ""
)

(define_insn "*udivmodsi4_score7"
  [(set (match_operand:SI 0 "register_operand" "=l")
        (udiv:SI (match_operand:SI 1 "register_operand" "d")
                 (match_operand:SI 2 "register_operand" "d")))
   (set (match_operand:SI 3 "register_operand" "=h")
        (umod:SI (match_dup 1) (match_dup 2)))]
  "(TARGET_SCORE5 || TARGET_SCORE7 || TARGET_SCORE7D)"
  "divu    %1, %2"
  [(set_attr "type" "div")
   (set_attr "mode" "SI")])

(define_insn "*udivmodsi4_score3"
  [(set (match_operand:SI 0 "score_register_operand" "=l")
        (udiv:SI (match_operand:SI 1 "score_register_operand" "d")
                 (match_operand:SI 2 "score_register_operand" "d")))
   (set (match_operand:SI 3 "score_register_operand" "=h")
        (umod:SI (match_dup 1) (match_dup 2)))]
  "(TARGET_SCORE3)"
  "divu\t%1, %2"
  [(set_attr "type" "div")
   (set_attr "mode" "SI")])

(define_expand "ashlsi3"
  [(set (match_operand:SI 0 "score_register_operand")
        (ashift:SI (match_operand:SI 1 "score_register_operand")
                   (match_operand:SI 2 "arith_operand")))]
  ""
  ""
)

(define_insn "*ashlsi3_score7"
  [(set (match_operand:SI 0 "register_operand" "=d,d")
        (ashift:SI (match_operand:SI 1 "register_operand" "d,d")
                   (match_operand:SI 2 "arith_operand" "J,d")))]
  "(TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)"
  "@
   slli    %0, %1, %c2
   sll     %0, %1, %2"
  [(set_attr "type" "arith")
   (set_attr "mode" "SI")])

(define_insn "*ashlsi3_score3"
  [(set (match_operand:SI 0 "score_register_operand" "=e,d,d")
        (ashift:SI (match_operand:SI 1 "score_register_operand" "0,d,d")
                   (match_operand:SI 2 "arith_operand" "J,J,d")))]
  "(TARGET_SCORE3)"
  "@
   slli!\t%0, %c2
   slli\t%0, %1, %c2
   sll\t%0, %1, %2"
  [(set_attr "type" "arith")
   (set_attr "length" "2,4,4")
   (set_attr "mode" "SI")])

(define_insn "*ashlsi3_ucc_score7"
  [(set (reg:CC_NZ CC_REGNUM)
        (compare:CC_NZ (ashift:SI
                        (match_operand:SI 1 "register_operand" "d,d")
                        (match_operand:SI 2 "arith_operand" "J,d"))
                       (const_int 0)))
   (set (match_operand:SI 0 "register_operand" "=d,d")
        (ashift:SI (match_dup 1) (match_dup 2)))]
  "(TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)"
{
  switch (which_alternative)
    {
    case 0: return score_select (operands, "slli", false, "c", true);
    case 1: return score_select (operands, "sll", false, "", true);
    default: gcc_unreachable ();
    }
}
  [(set_attr "type" "arith")
   (set_attr "up_c" "yes")
   (set_attr "mode" "SI")])

(define_insn "*ashlsi3_ucc_score3"
  [(set (reg:CC_NZ CC_REGNUM)
        (compare:CC_NZ (ashift:SI
                        (match_operand:SI 1 "score_register_operand" "d,d")
                        (match_operand:SI 2 "arith_operand" "J,d"))
                       (const_int 0)))
   (set (match_operand:SI 0 "score_register_operand" "=d,d")
        (ashift:SI (match_dup 1) (match_dup 2)))]
  "(TARGET_SCORE3)"
{
  switch (which_alternative)
    {
    case 0: return score_select (operands, "slli", false, "c", true);
    case 1: return score_select (operands, "sll", false, "", true);
    default: gcc_unreachable ();
    }
}
  [(set_attr "type" "arith")
   (set_attr "length" "4,4")
   (set_attr "up_c" "yes")
   (set_attr "mode" "SI")])

(define_insn "*ashlsi3_cmp_score7"
  [(set (reg:CC_NZ CC_REGNUM)
        (compare:CC_NZ (ashift:SI
                        (match_operand:SI 1 "register_operand" "d,d")
                        (match_operand:SI 2 "arith_operand" "J,d"))
                       (const_int 0)))
   (clobber (match_scratch:SI 0 "=d,d"))]
  "(TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)"
{
  switch (which_alternative)
    {
    case 0: return score_select (operands, "slli", false, "c", true);
    case 1: return score_select (operands, "sll", false, "", true);
    default: gcc_unreachable ();
    }
}
  [(set_attr "type" "arith")
   (set_attr "up_c" "yes")
   (set_attr "mode" "SI")])

(define_insn "*ashlsi3_cmp_score3"
  [(set (reg:CC_NZ CC_REGNUM)
        (compare:CC_NZ (ashift:SI
                        (match_operand:SI 1 "score_register_operand" "d,d")
                        (match_operand:SI 2 "arith_operand" "J,d"))
                       (const_int 0)))
   (clobber (match_scratch:SI 0 "=d,d"))]
  "(TARGET_SCORE3)"
{
  switch (which_alternative)
    {
    case 0: return score_select (operands, "slli", false, "c", true);
    case 1: return score_select (operands, "sll", false, "", true);
    default: gcc_unreachable ();
    }
}
  [(set_attr "type" "arith")
   (set_attr "length" "4,4")
   (set_attr "up_c" "yes")
   (set_attr "mode" "SI")])


(define_expand "ashrsi3"
  [(set (match_operand:SI 0 "score_register_operand")
        (ashiftrt:SI (match_operand:SI 1 "score_register_operand")
                     (match_operand:SI 2 "arith_operand")))]
  ""
  ""
)

(define_insn "*ashrsi3_score7"
  [(set (match_operand:SI 0 "register_operand" "=d,d")
        (ashiftrt:SI (match_operand:SI 1 "register_operand" "d,d")
                     (match_operand:SI 2 "arith_operand" "J,d")))]
  "(TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)"
  "@
   srai    %0, %1, %c2
   sra     %0, %1, %2"
  [(set_attr "type" "arith")
   (set_attr "mode" "SI")])

(define_insn "*ashrsi3_score3"
  [(set (match_operand:SI 0 "score_register_operand" "=d,d")
        (ashiftrt:SI (match_operand:SI 1 "score_register_operand" "d,d")
                     (match_operand:SI 2 "arith_operand" "J,d")))]
  "(TARGET_SCORE3)"
  "@
   srai\t%0, %1, %c2
   sra\t%0, %1, %2"
  [(set_attr "type" "arith")
   (set_attr "length" "4,4")
   (set_attr "mode" "SI")])

(define_insn "*ashrsi3_ucc_score7"
  [(set (reg:CC_NZ CC_REGNUM)
        (compare:CC_NZ (ashiftrt:SI
                        (match_operand:SI 1 "register_operand" "d,d")
                        (match_operand:SI 2 "arith_operand" "J,d"))
                       (const_int 0)))
   (set (match_operand:SI 0 "register_operand" "=d,d")
        (ashiftrt:SI (match_dup 1) (match_dup 2)))]
  "(TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)"
{
  switch (which_alternative)
    {
    case 0: return \"srai.c\t%0, %1, %c2\";
    case 1: return score_select (operands, "sra", false, "", true);
    default: gcc_unreachable ();
    }
}
  [(set_attr "type" "arith")
   (set_attr "up_c" "yes")
   (set_attr "mode" "SI")])

(define_insn "*ashrsi3_ucc_score3"
  [(set (reg:CC_NZ CC_REGNUM)
        (compare:CC_NZ (ashiftrt:SI
                        (match_operand:SI 1 "score_register_operand" "d,d")
                        (match_operand:SI 2 "arith_operand" "J,d"))
                       (const_int 0)))
   (set (match_operand:SI 0 "score_register_operand" "=d,d")
        (ashiftrt:SI (match_dup 1) (match_dup 2)))]
  "(TARGET_SCORE3)"
{
  switch (which_alternative)
    {
    case 0: return \"srai.c\t%0, %1, %c2\";
    case 1: return score_select (operands, "sra", false, "", true);
    default: gcc_unreachable ();
    }
}
  [(set_attr "type" "arith")
   (set_attr "length" "4,4")
   (set_attr "up_c" "yes")
   (set_attr "mode" "SI")])

(define_insn "*ashrsi3_cmp_score7"
  [(set (reg:CC_NZ CC_REGNUM)
        (compare:CC_NZ (ashiftrt:SI
                        (match_operand:SI 1 "register_operand" "d,d")
                        (match_operand:SI 2 "arith_operand" "J,d"))
                       (const_int 0)))
   (clobber (match_scratch:SI 0 "=d,d"))]
  "(TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)"
{
  switch (which_alternative)
    {
    case 0: return \"srai.c\t%0, %1, %c2\";
    case 1: return score_select (operands, "sra", false, "", true);
    default: gcc_unreachable ();
    }
}
  [(set_attr "type" "arith")
   (set_attr "up_c" "yes")
   (set_attr "mode" "SI")])

(define_insn "ashrsi3_cmp_score3"
  [(set (reg:CC_NZ CC_REGNUM)
        (compare:CC_NZ (ashiftrt:SI
                        (match_operand:SI 1 "score_register_operand" "d,d")
                        (match_operand:SI 2 "arith_operand" "J,d"))
                       (const_int 0)))
   (clobber (match_scratch:SI 0 "=d,d"))]
  "(TARGET_SCORE3)"
{
  switch (which_alternative)
    {
    case 0: return \"srai.c\t%0, %1, %c2\";
    case 1: return score_select (operands, "sra", false, "", true);
    default: gcc_unreachable ();
    }
}
  [(set_attr "type" "arith")
   (set_attr "length" "4,4")
   (set_attr "up_c" "yes")
   (set_attr "mode" "SI")])

(define_expand "lshrsi3"
  [(set (match_operand:SI 0 "score_register_operand")
        (lshiftrt:SI (match_operand:SI 1 "score_register_operand")
                     (match_operand:SI 2 "arith_operand")))]
  ""
  ""
)

(define_insn "*lshrsi3_score7"
  [(set (match_operand:SI 0 "register_operand" "=d,d")
        (lshiftrt:SI (match_operand:SI 1 "register_operand" "d,d")
                     (match_operand:SI 2 "arith_operand" "J,d")))]
  "(TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)"
  "@
   srli    %0, %1, %c2
   srl     %0, %1, %2"
  [(set_attr "type" "arith")
   (set_attr "mode" "SI")])

(define_insn "*lshrsi3_score3"
  [(set (match_operand:SI 0 "score_register_operand" "=e,d,d")
        (lshiftrt:SI (match_operand:SI 1 "score_register_operand" "0,d,d")
                     (match_operand:SI 2 "arith_operand" "J,J,d")))]
  "(TARGET_SCORE3)"
  "@
   srli!\t%0, %c2
   srli\t%0, %1, %c2
   srl\t%0, %1, %2"
  [(set_attr "type" "arith")
   (set_attr "length" "2,4,4")
   (set_attr "mode" "SI")])

(define_insn "*lshrsi3_ucc_score7"
  [(set (reg:CC_NZ CC_REGNUM)
        (compare:CC_NZ (lshiftrt:SI
                        (match_operand:SI 1 "register_operand" "d,d")
                        (match_operand:SI 2 "arith_operand" "J,d"))
                       (const_int 0)))
   (set (match_operand:SI 0 "register_operand" "=d,d")
        (lshiftrt:SI (match_dup 1) (match_dup 2)))]
  "(TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)"
{
  switch (which_alternative)
    {
    case 0: return score_select (operands, "srli", false, "c", true);
    case 1: return score_select (operands, "srl", false, "", true);
    default: gcc_unreachable ();
    }
}
  [(set_attr "type" "arith")
   (set_attr "up_c" "yes")
   (set_attr "mode" "SI")])

(define_insn "*lshrsi3_ucc_score3"
  [(set (reg:CC_NZ CC_REGNUM)
        (compare:CC_NZ (lshiftrt:SI
                        (match_operand:SI 1 "score_register_operand" "d,d")
                        (match_operand:SI 2 "arith_operand" "J,d"))
                       (const_int 0)))
   (set (match_operand:SI 0 "score_register_operand" "=d,d")
        (lshiftrt:SI (match_dup 1) (match_dup 2)))]
  "(TARGET_SCORE3)"
{
  switch (which_alternative)
    {
    case 0: return score_select (operands, "srli", false, "c", true);
    case 1: return score_select (operands, "srl", false, "", true);
    default: gcc_unreachable ();
    }
}
  [(set_attr "type" "arith")
   (set_attr "length" "4,4")
   (set_attr "up_c" "yes")
   (set_attr "mode" "SI")])

(define_insn "*lshrsi3_cmp_score7"
  [(set (reg:CC_NZ CC_REGNUM)
        (compare:CC_NZ (lshiftrt:SI
                        (match_operand:SI 1 "register_operand" "d,d")
                        (match_operand:SI 2 "arith_operand" "J,d"))
                       (const_int 0)))
   (clobber (match_scratch:SI 0 "=d,d"))]
  "(TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)"
{
  switch (which_alternative)
    {
    case 0: return score_select (operands, "srli", false, "c", true);
    case 1: return score_select (operands, "srl", false, "", true);
    default: gcc_unreachable ();
    }
}
  [(set_attr "type" "arith")
   (set_attr "up_c" "yes")
   (set_attr "mode" "SI")])

(define_insn "*lshrsi3_cmp_score3"
  [(set (reg:CC_NZ CC_REGNUM)
        (compare:CC_NZ (lshiftrt:SI
                        (match_operand:SI 1 "score_register_operand" "d,d")
                        (match_operand:SI 2 "arith_operand" "J,d"))
                       (const_int 0)))
   (clobber (match_scratch:SI 0 "=d,d"))]
  "(TARGET_SCORE3)"
{
  switch (which_alternative)
    {
    case 0: return score_select (operands, "srli", false, "c", true);
    case 1: return score_select (operands, "srl", false, "", true);
    default: gcc_unreachable ();
    }
}
  [(set_attr "type" "arith")
   (set_attr "length" "4,4")
   (set_attr "up_c" "yes")
   (set_attr "mode" "SI")])

(define_expand "negsi2"
  [(set (match_operand:SI 0 "score_register_operand")
        (neg:SI (match_operand:SI 1 "score_register_operand")))]
  ""
  ""
)

(define_insn "*negsi2_score7"
  [(set (match_operand:SI 0 "register_operand" "=d")
        (neg:SI (match_operand:SI 1 "register_operand" "d")))]
  "(TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)"
  "neg     %0, %1"
  [(set_attr "type" "arith")
   (set_attr "mode" "SI")])

(define_insn "*negsi2_score3"
  [(set (match_operand:SI 0 "score_register_operand" "=d")
        (neg:SI (match_operand:SI 1 "score_register_operand" "d")))]
  "(TARGET_SCORE3)"
  "neg\t%0, %1"
  [(set_attr "type" "arith")
   (set_attr "length" "4")
   (set_attr "mode" "SI")])

(define_insn "*negsi2_cmp_score7"
  [(set (reg:CC_NZ CC_REGNUM)
        (compare:CC_NZ (neg:SI (match_operand:SI 1 "register_operand" "e,d"))
                       (const_int 0)))
   (clobber (match_scratch:SI 0 "=e,d"))]
  "(TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)"
  "@
   neg!    %0, %1
   neg.c   %0, %1"
  [(set_attr "type" "arith")
   (set_attr "up_c" "yes")
   (set_attr "mode" "SI")])

(define_insn "*negsi2_cmp_score3"
  [(set (reg:CC_NZ CC_REGNUM)
        (compare:CC_NZ (neg:SI (match_operand:SI 1 "score_register_operand" "d"))
                       (const_int 0)))
   (clobber (match_scratch:SI 0 "=d"))]
  "(TARGET_SCORE3)"
  "neg.c\t%0, %1"
  [(set_attr "type" "arith")
   (set_attr "length" "4")
   (set_attr "up_c" "yes")
   (set_attr "mode" "SI")])

(define_insn "*negsi2_ucc_score7"
  [(set (reg:CC_NZ CC_REGNUM)
        (compare:CC_NZ (neg:SI (match_operand:SI 1 "register_operand" "e,d"))
                       (const_int 0)))
   (set (match_operand:SI 0 "register_operand" "=e,d")
        (neg:SI (match_dup 1)))]
  "(TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)"
  "@
   neg!    %0, %1
   neg.c   %0, %1"
  [(set_attr "type" "arith")
   (set_attr "up_c" "yes")
   (set_attr "mode" "SI")])

(define_insn "*negsi2_ucc_score3"
  [(set (reg:CC_NZ CC_REGNUM)
        (compare:CC_NZ (neg:SI (match_operand:SI 1 "score_register_operand" "d"))
                       (const_int 0)))
   (set (match_operand:SI 0 "score_register_operand" "=d")
        (neg:SI (match_dup 1)))]
  "(TARGET_SCORE3)"
  "neg.c\t%0, %1"
  [(set_attr "type" "arith")
   (set_attr "length" "4")
   (set_attr "up_c" "yes")
   (set_attr "mode" "SI")])


(define_expand "one_cmplsi2"
  [(set (match_operand:SI 0 "score_register_operand")
        (not:SI (match_operand:SI 1 "score_register_operand")))]
  ""
  ""
)

(define_insn "*one_cmplsi2_score7"
  [(set (match_operand:SI 0 "register_operand" "=d")
        (not:SI (match_operand:SI 1 "register_operand" "d")))]
  "(TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)"
  "not\t%0, %1"
  [(set_attr "type" "arith")
   (set_attr "mode" "SI")])

(define_insn "*one_cmplsi2_score3"
  [(set (match_operand:SI 0 "score_register_operand" "=d")
        (not:SI (match_operand:SI 1 "score_register_operand" "d")))]
  "(TARGET_SCORE3)"
  "not\t%0, %1"
  [(set_attr "type" "arith")
   (set_attr "length" "4")
   (set_attr "mode" "SI")])

(define_insn "*one_cmplsi2_ucc_score7"
  [(set (reg:CC_NZ CC_REGNUM)
        (compare:CC_NZ (not:SI (match_operand:SI 1 "register_operand" "e,d"))
                       (const_int 0)))
   (set (match_operand:SI 0 "register_operand" "=e,d")
        (not:SI (match_dup 1)))]
  "(TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)"
  "@
   not!    %0, %1
   not.c   %0, %1"
  [(set_attr "type" "arith")
   (set_attr "up_c" "yes")
   (set_attr "mode" "SI")])

(define_insn "*one_cmplsi2_ucc_score3"
  [(set (reg:CC_NZ CC_REGNUM)
        (compare:CC_NZ (not:SI (match_operand:SI 1 "score_register_operand" "d"))
                       (const_int 0)))
   (set (match_operand:SI 0 "score_register_operand" "=d")
        (not:SI (match_dup 1)))]
  "(TARGET_SCORE3)"
  "not.c\t%0, %1"
  [(set_attr "type" "arith")
   (set_attr "length" "4")
   (set_attr "up_c" "yes")
   (set_attr "mode" "SI")])

(define_insn "*one_cmplsi2_cmp_score7"
  [(set (reg:CC_NZ CC_REGNUM)
        (compare:CC_NZ (not:SI (match_operand:SI 1 "register_operand" "e,d"))
                       (const_int 0)))
   (clobber (match_scratch:SI 0 "=e,d"))]
  "(TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)"
  "@
   not!    %0, %1
   not.c   %0, %1"
  [(set_attr "type" "arith")
   (set_attr "up_c" "yes")
   (set_attr "mode" "SI")])

(define_insn "*one_cmplsi2_cmp_score3"
  [(set (reg:CC_NZ CC_REGNUM)
        (compare:CC_NZ (not:SI (match_operand:SI 1 "score_register_operand" "d"))
                       (const_int 0)))
   (clobber (match_scratch:SI 0 "=d"))]
  "(TARGET_SCORE3)"
  "not.c\t%0, %1"
  [(set_attr "type" "arith")
   (set_attr "length" "4")
   (set_attr "up_c" "yes")
   (set_attr "mode" "SI")])

(define_expand "rotlsi3"
  [(parallel
    [(set (match_operand:SI 0 "score_register_operand")
          (rotate:SI (match_operand:SI 1 "score_register_operand")
                     (match_operand:SI 2 "arith_operand")))
     (clobber (reg:CC CC_REGNUM))])]
  ""
  ""
)

(define_insn "*rotlsi3_score7"
  [(set (match_operand:SI 0 "register_operand" "=d,d")
        (rotate:SI (match_operand:SI 1 "register_operand" "d,d")
                   (match_operand:SI 2 "arith_operand" "J,d")))
   (clobber (reg:CC CC_REGNUM))]
  "(TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)"
  "@
   roli.c  %0, %1, %c2
   rol.c   %0, %1, %2"
  [(set_attr "type" "arith")
   (set_attr "mode" "SI")])

(define_insn "*rotlsi3_score3"
  [(set (match_operand:SI 0 "score_register_operand" "=d,d")
        (rotate:SI (match_operand:SI 1 "score_register_operand" "d,d")
                   (match_operand:SI 2 "arith_operand" "J,d")))
   (clobber (reg:CC CC_REGNUM))]
  "(TARGET_SCORE3)"
  "@
   roli.c\t%0, %1, %c2
   rol.c\t%0, %1, %2"
  [(set_attr "type" "arith")
   (set_attr "length" "4,4")
   (set_attr "mode" "SI")])

(define_expand "rotrsi3"
  [(parallel
    [(set (match_operand:SI 0 "score_register_operand")
          (rotatert:SI (match_operand:SI 1 "score_register_operand")
                       (match_operand:SI 2 "arith_operand")))
     (clobber (reg:CC CC_REGNUM))])]
  ""
  ""
)

(define_insn "*rotrsi3_score7"
  [(set (match_operand:SI 0 "register_operand" "=d,d")
        (rotatert:SI (match_operand:SI 1 "register_operand" "d,d")
                     (match_operand:SI 2 "arith_operand" "J,d")))
   (clobber (reg:CC CC_REGNUM))]
  "(TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)"
  "@
   rori.c  %0, %1, %c2
   ror.c   %0, %1, %2"
  [(set_attr "type" "arith")
   (set_attr "mode" "SI")])

(define_insn "*rotrsi3_score3"
  [(set (match_operand:SI 0 "score_register_operand" "=d,d")
        (rotatert:SI (match_operand:SI 1 "score_register_operand" "d,d")
                     (match_operand:SI 2 "arith_operand" "J,d")))
   (clobber (reg:CC CC_REGNUM))]
  "(TARGET_SCORE3)"
  "@
   rori.c\t%0, %1, %c2
   ror.c\t%0, %1, %2"
  [(set_attr "type" "arith")
   (set_attr "length" "4,4")
   (set_attr "mode" "SI")])

(define_insn "cbrancheqz"
  [(set (pc) (if_then_else
              (eq (match_operand:SI 0 "score_register_operand" "d")
                  (const_int 0))
              (label_ref (match_operand 1 "" ""))
              (pc)))
   (clobber (reg:CC CC_REGNUM))]
  "(TARGET_SCORE3)"
{
  if (get_attr_length (insn) == 4)
      return \"bcmpeqz\t%0, %1\";
  else
      return \"cmpi!\t%0, 0\;beq!\t%1\";
}
  [(set (attr "length")
     (if_then_else
       (and (ge (minus (match_dup 1) (pc)) (const_int -504))  
            (le (minus (match_dup 1) (pc)) (const_int 502)))
       (const_int 4)
       (const_int 6)))])

(define_insn "cbrancheq"
  [(set (pc) (if_then_else
              (eq (match_operand:SI 0 "score_register_operand" "d")
                  (match_operand:SI 1 "score_register_operand" "d"))
              (label_ref (match_operand 2 "" ""))
              (pc)))
   (clobber (reg:CC CC_REGNUM))]
  "(TARGET_SCORE3)"
{
  if (get_attr_length (insn) == 4)
      return \"bcmpeq\t%0, %1, %2\";
  else
      return \"cmp!\t%0, %1\;beq!\t%2\";
}
  [(set (attr "length")
     (if_then_else
       (and (ge (minus (match_dup 2) (pc)) (const_int -504))  
            (le (minus (match_dup 2) (pc)) (const_int 502)))
       (const_int 4)
       (const_int 6)))])
 
(define_insn "cbranchnez"
  [(set (pc) (if_then_else
              (ne (match_operand:SI 0 "score_register_operand" "d")
                  (const_int 0))
              (label_ref (match_operand 1 "" ""))
              (pc)))
   (clobber (reg:CC CC_REGNUM))]
  "(TARGET_SCORE3)"
{
  if (get_attr_length (insn) == 4)
      return \"bcmpnez\t%0, %1\";
  else
      return \"cmpi!\t%0, 0\;bne\t%1\";
}
  [(set (attr "length")
     (if_then_else
       (and (ge (minus (match_dup 1) (pc)) (const_int -504))  
            (le (minus (match_dup 1) (pc)) (const_int 502)))
       (const_int 4)
       (const_int 6)))])
    
(define_insn "cbranchne"
  [(set (pc) (if_then_else
              (ne (match_operand:SI 0 "score_register_operand" "d")
                  (match_operand:SI 1 "score_register_operand" "d"))
              (label_ref (match_operand 2 "" ""))
              (pc)))
   (clobber (reg:CC CC_REGNUM))]
  "(TARGET_SCORE3)"
{
  if (get_attr_length (insn) == 4)
      return \"bcmpne\t%0, %1, %2\";
  else
      return \"cmp!\t%0, %1\;bne\t%2\";
}
  [(set (attr "length")
     (if_then_else
       (and (ge (minus (match_dup 2) (pc)) (const_int -504))  
            (le (minus (match_dup 2) (pc)) (const_int 502)))
       (const_int 4)
       (const_int 6)))])

(define_expand "cmpsi"
  [(match_operand:SI 0 "score_register_operand")
   (match_operand:SI 1 "arith_operand")]
  ""
{
  cmp_op0 = operands[0];
  cmp_op1 = operands[1];
  DONE;
})

(define_insn "cmpsi_nz_score7"
  [(set (reg:CC_NZ CC_REGNUM)
        (compare:CC_NZ (match_operand:SI 0 "register_operand" "d,e,d")
                       (match_operand:SI 1 "arith_operand" "L,e,d")))]
  "(TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)"
  "@
   cmpi.c  %0, %c1
   cmp!    %0, %1
   cmp.c   %0, %1"
   [(set_attr "type" "cmp")
    (set_attr "up_c" "yes")
    (set_attr "mode" "SI")])

(define_insn "cmpsi_nz_score3"
  [(set (reg:CC_NZ CC_REGNUM)
        (compare:CC_NZ (match_operand:SI 0 "score_register_operand" "d,d,d")
                       (match_operand:SI 1 "arith_operand" "O,L,d")))]
  "(TARGET_SCORE3)"
  "@
   cmpi!\t%0, %c1
   cmpi.c\t%0, %c1
   cmp!\t %0, %1"
   [(set_attr "type" "cmp")
    (set_attr "length" "2,4,2")
    (set_attr "up_c" "yes")
    (set_attr "mode" "SI")])

(define_insn "cmpsi_n_score7"
  [(set (reg:CC_N CC_REGNUM)
        (compare:CC_N (match_operand:SI 0 "register_operand" "d,e,d")
                      (match_operand:SI 1 "arith_operand" "L,e,d")))]
  "(TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)"
  "@
   cmpi.c  %0, %c1
   cmp!    %0, %1
   cmp.c   %0, %1"
   [(set_attr "type" "cmp")
    (set_attr "up_c" "yes")
    (set_attr "mode" "SI")])

(define_insn "cmpsi_n_score3"
  [(set (reg:CC_N CC_REGNUM)
        (compare:CC_N (match_operand:SI 0 "score_register_operand" "d,d,d")
                      (match_operand:SI 1 "arith_operand" "O,L,d")))]
  "(TARGET_SCORE3)"
  "@
   cmpi!\t%0, %c1
   cmpi.c\t%0, %c1
   cmp!\t%0, %1"
   [(set_attr "type" "cmp")
    (set_attr "length" "2,4,2")
    (set_attr "up_c" "yes")
    (set_attr "mode" "SI")])

(define_insn "*cmpsi_to_addsi_score7"
  [(set (reg:CC_NZ CC_REGNUM)
        (compare:CC_NZ (match_operand:SI 1 "register_operand" "0,d")
                       (neg:SI (match_operand:SI 2 "register_operand" "e,d"))))
   (clobber (match_scratch:SI 0 "=e,d"))]
  "(TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)"
  "@
   add!    %0, %2
   add.c   %0, %1, %2"
   [(set_attr "type" "cmp")
    (set_attr "up_c" "yes")
    (set_attr "mode" "SI")])

(define_insn "*cmpsi_to_addsi_score3"
  [(set (reg:CC_NZ CC_REGNUM)
        (compare:CC_NZ (match_operand:SI 1 "score_register_operand" "d")
                       (neg:SI (match_operand:SI 2 "score_register_operand" "d"))))
   (clobber (match_scratch:SI 0 "=d"))]
  "(TARGET_SCORE3)"
  "add.c\t%0, %1, %2"
  [(set_attr "type" "cmp")
   (set_attr "length" "4")
   (set_attr "up_c" "yes")
   (set_attr "mode" "SI")])

(define_insn "cmpsi_cc_score7"
  [(set (reg:CC CC_REGNUM)
        (compare:CC (match_operand:SI 0 "register_operand" "d,e,d")
                    (match_operand:SI 1 "arith_operand" "L,e,d")))]
  "(TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)"
  "@
   cmpi.c  %0, %c1
   cmp!    %0, %1
   cmp.c   %0, %1"
  [(set_attr "type" "cmp")
   (set_attr "up_c" "yes")
   (set_attr "mode" "SI")])

(define_insn "cmpsi_cc_score3"
  [(set (reg:CC CC_REGNUM)
        (compare:CC (match_operand:SI 0 "score_register_operand" "d,d,d")
                    (match_operand:SI 1 "arith_operand" "O,L,d")))]
  "(TARGET_SCORE3)"
  "@
   cmpi!\t%0, %c1
   cmpi.c\t%0, %c1
   cmp!\t%0, %1"
  [(set_attr "type" "cmp")
   (set_attr "length" "2,4,2")
   (set_attr "up_c" "yes")
   (set_attr "mode" "SI")])

(define_expand "beq"
  [(set (pc)
        (if_then_else (eq (reg:CC CC_REGNUM) (const_int 0))
                      (label_ref (match_operand 0 "" ""))
                      (pc)))]
  ""
{
  score_gen_cmp (CCmode);
})

(define_expand "bne"
  [(set (pc)
        (if_then_else (ne (reg:CC CC_REGNUM) (const_int 0))
                      (label_ref (match_operand 0 "" ""))
                      (pc)))]
  ""
{
  score_gen_cmp (CCmode);
})

(define_expand "bgt"
  [(set (pc)
        (if_then_else (gt (reg:CC CC_REGNUM) (const_int 0))
                      (label_ref (match_operand 0 "" ""))
                      (pc)))]
  ""
{
  score_gen_cmp (CCmode);
})

(define_expand "ble"
  [(set (pc)
        (if_then_else (le (reg:CC CC_REGNUM) (const_int 0))
                      (label_ref (match_operand 0 "" ""))
                      (pc)))]
  ""
{
  score_gen_cmp (CCmode);
})

(define_expand "bge"
  [(set (pc)
        (if_then_else (ge (reg:CC CC_REGNUM) (const_int 0))
                      (label_ref (match_operand 0 "" ""))
                      (pc)))]
  ""
{
  score_gen_cmp (CCmode);
})

(define_expand "blt"
  [(set (pc)
        (if_then_else (lt (reg:CC CC_REGNUM) (const_int 0))
                      (label_ref (match_operand 0 "" ""))
                      (pc)))]
  ""
{
  score_gen_cmp (CCmode);
})

(define_expand "bgtu"
  [(set (pc)
        (if_then_else (gtu (reg:CC CC_REGNUM) (const_int 0))
                      (label_ref (match_operand 0 "" ""))
                      (pc)))]
  ""
{
  score_gen_cmp (CCmode);
})

(define_expand "bleu"
  [(set (pc)
        (if_then_else (leu (reg:CC CC_REGNUM) (const_int 0))
                      (label_ref (match_operand 0 "" ""))
                      (pc)))]
  ""
{
  score_gen_cmp (CCmode);
})

(define_expand "bgeu"
  [(set (pc)
        (if_then_else (geu (reg:CC CC_REGNUM) (const_int 0))
                      (label_ref (match_operand 0 "" ""))
                      (pc)))]
  ""
{
  score_gen_cmp (CCmode);
})

(define_expand "bltu"
  [(set (pc)
        (if_then_else (ltu (reg:CC CC_REGNUM) (const_int 0))
                      (label_ref (match_operand 0 "" ""))
                      (pc)))]
  ""
{
  score_gen_cmp (CCmode);
})

(define_insn "*branch_n_score7"
  [(set (pc)
        (if_then_else
         (match_operator 0 "branch_n_operator"
                         [(reg:CC_N CC_REGNUM)
                          (const_int 0)])
         (label_ref (match_operand 1 "" ""))
         (pc)))]
  "(TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)"
  "b%C0    %1"
  [(set_attr "type" "branch")])

(define_insn "*branch_n_score3"
  [(set (pc)
        (if_then_else
         (match_operator 0 "branch_n_operator"
                         [(reg:CC_N CC_REGNUM)
                          (const_int 0)])
         (label_ref (match_operand 1 "" ""))
         (pc)))]
  "(TARGET_SCORE3)"
  "b%C0\t%1"
  [(set_attr "type" "branch")
   (set_attr "length" "4")])

(define_insn "*branch_nz_score7"
  [(set (pc)
        (if_then_else
         (match_operator 0 "branch_nz_operator"
                         [(reg:CC_NZ CC_REGNUM)
                          (const_int 0)])
         (label_ref (match_operand 1 "" ""))
         (pc)))]
  "(TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)"
  "b%C0    %1"
  [(set_attr "type" "branch")])

(define_insn "*branch_nz_score3"
  [(set (pc)
        (if_then_else
         (match_operator 0 "branch_nz_operator"
                         [(reg:CC_NZ CC_REGNUM)
                          (const_int 0)])
         (label_ref (match_operand 1 "" ""))
         (pc)))]
  "(TARGET_SCORE3)"
  "b%C0\t%1"
  [(set_attr "type" "branch")
   (set_attr "length" "4")])


(define_insn "*branch_cc_score7"
  [(set (pc)
        (if_then_else
         (match_operator 0 "comparison_operator"
                         [(reg:CC CC_REGNUM)
                          (const_int 0)])
         (label_ref (match_operand 1 "" ""))
         (pc)))]
  "(TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)"
  "b%C0    %1"
  [(set_attr "type" "branch")])

(define_insn "*branch_cc_score3"
  [(set (pc)
        (if_then_else
         (match_operator 0 "comparison_operator"
                         [(reg:CC CC_REGNUM)
                          (const_int 0)])
         (label_ref (match_operand 1 "" ""))
         (pc)))]
  "(TARGET_SCORE3)"
  "b%C0\t%1"
  [(set_attr "type" "branch")
   (set_attr "length" "4")])

(define_insn "jump"
  [(set (pc)
        (label_ref (match_operand 0 "" "")))]
  ""
{
  if (!flag_pic)
    return \"j\t%0\";
  else
    return \"b\t%0\";
}
  [(set_attr "type" "jump")
   (set_attr "length" "4")])

(define_expand "sibcall"
  [(parallel [(call (match_operand 0 "" "")
                    (match_operand 1 "" ""))
              (use (match_operand 2 "" ""))])]
  ""
{
  score_call (operands, true);
  DONE;
})

(define_insn "sibcall_internal_score7"
  [(call (mem:SI (match_operand:SI 0 "call_insn_operand" "t,Z"))
         (match_operand 1 "" ""))
   (clobber (reg:SI RT_REGNUM))]
  "(TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)
   && SIBLING_CALL_P (insn)"
{
  if (!flag_pic)
    switch (which_alternative)
      {
      case 0: return \"br%S0\t%0\";
      case 1: return \"j\t%0\";
      default: gcc_unreachable ();
      }
  else
    switch (which_alternative)
      {
      case 0: return \"mv\tr29, %0\;br\tr29\";
      case 1: return \"la\tr29, %0\;br\tr29\";
      default: gcc_unreachable ();
      }
}
  [(set_attr "type" "call")])

(define_insn "sibcall_internal_score3"
  [(call (mem:SI (match_operand:SI 0 "call_insn_operand" "t,Z"))
         (match_operand 1 "" ""))
   (clobber (reg:SI RT_REGNUM))]
  "(TARGET_SCORE3) && (SIBLING_CALL_P (insn))"
{
  if (!flag_pic)
    switch (which_alternative)
      {
      case 0: return \"br%S0\t%0\";
      case 1: return \"j\t%0\";
      default: gcc_unreachable ();
      }
  else
    switch (which_alternative)
      {
      case 0: return \"mv!\tr29, %0\;br!\tr29\";
      case 1: return \"ldi48\tr29, %0\;br!\tr29\";
      default: gcc_unreachable ();
      }
}
  [(set_attr "type" "call")
   (set_attr "length" "4,8")])

(define_expand "sibcall_value"
  [(parallel [(set (match_operand 0 "" "")
              (call (match_operand 1 "" "") (match_operand 2 "" "")))
              (use (match_operand 3 "" ""))])]
  ""
{
  score_call_value (operands, true);
  DONE;
})

(define_insn "sibcall_value_internal_score7"
  [(set (match_operand 0 "register_operand" "=d,d")
        (call (mem:SI (match_operand:SI 1 "call_insn_operand" "t,Z"))
              (match_operand 2 "" "")))
   (clobber (reg:SI RT_REGNUM))]
  "(TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)
   && SIBLING_CALL_P (insn)"
{
  if (!flag_pic)
    switch (which_alternative)
      {
      case 0: return \"br%S1\t%1\";
      case 1: return \"j\t%1\";
      default: gcc_unreachable ();
      }
  else
    switch (which_alternative)
      {
      case 0: return \"mv\tr29, %1\;br\tr29\";
      case 1: return \"la\tr29, %1\;br\tr29\";
      default: gcc_unreachable ();
      }
}
  [(set_attr "type" "call")])

(define_insn "sibcall_value_internal_score3"
  [(set (match_operand 0 "score_register_operand" "=d,d")
        (call (mem:SI (match_operand:SI 1 "call_insn_operand" "t,Z"))
              (match_operand 2 "" "")))
   (clobber (reg:SI RT_REGNUM))]
  "(TARGET_SCORE3) && (SIBLING_CALL_P (insn))"
{
  if (!flag_pic)
    switch (which_alternative)
      {
      case 0: return \"br%S1\t%1\";
      case 1: return \"j\t%1\";
      default: gcc_unreachable ();
      }
  else
    switch (which_alternative)
      {
      case 0: return \"mv!\tr29, %1\;br!\tr29\";
      case 1: return \"ldi48\tr29, %1\;br!\tr29\";
      default: gcc_unreachable ();
      }
}
  [(set_attr "length" "4,8")
   (set_attr "type" "call")])

(define_expand "call"
  [(parallel [(call (match_operand 0 "" "") (match_operand 1 "" ""))
              (use (match_operand 2 "" ""))])]
  ""
{
  score_call (operands, false);
  DONE;
})

(define_insn "call_internal_score7"
  [(call (mem:SI (match_operand:SI 0 "call_insn_operand" "d,Z"))
         (match_operand 1 "" ""))
   (clobber (reg:SI RA_REGNUM))]
  "(TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)"
{
  if (!flag_pic)
    switch (which_alternative)
      {
      case 0: return \"brl%S0\t%0\";
      case 1: return \"jl\t%0\";
      default: gcc_unreachable ();
      }
  else
     switch (which_alternative)
      {
      case 0: return \"mv\tr29, %0\;brl\tr29\";
      case 1: return \"la\tr29, %0\;brl\tr29\";
      default: gcc_unreachable ();
      }
}
  [(set_attr "type" "call")])

(define_insn "call_internal_score3"
  [(call (mem:SI (match_operand:SI 0 "call_insn_operand" "d,Z"))
         (match_operand 1 "" ""))
   (clobber (reg:SI RA_REGNUM))]
  "(TARGET_SCORE3)"
{
  if (!flag_pic)
    switch (which_alternative)
      {
      case 0: return \"brl!\t%0\";
      case 1: return \"jl\t%0\";
      default: gcc_unreachable ();
      }
  else
     switch (which_alternative)
      {
      case 0: return \"mv!\tr29, %0\;brl!\tr29\";
      case 1: return \"ldi48\tr29, %0\;brl!\tr29\";
      default: gcc_unreachable ();
      }
}
  [(set_attr "length" "4,8")
   (set_attr "type" "call")])

(define_expand "call_value"
  [(parallel [(set (match_operand 0 "" "")
                   (call (match_operand 1 "" "") (match_operand 2 "" "")))
              (use (match_operand 3 "" ""))])]
  ""
{
  score_call_value (operands, false);
  DONE;
})

(define_insn "call_value_internal_score7"
  [(set (match_operand 0 "register_operand" "=d,d")
        (call (mem:SI (match_operand:SI 1 "call_insn_operand" "d,Z"))
              (match_operand 2 "" "")))
   (clobber (reg:SI RA_REGNUM))]
  "(TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)"
{
  if (!flag_pic)
    switch (which_alternative)
      {
      case 0: return \"brl%S1\t%1\";
      case 1: return \"jl\t%1\";
      default: gcc_unreachable ();
      }
  else
    switch (which_alternative)
      {
      case 0: return \"mv\tr29, %1\;brl\tr29\";
      case 1: return \"la\tr29, %1\;brl\tr29\";
      default: gcc_unreachable ();
      }
}
  [(set_attr "type" "call")])

(define_insn "call_value_internal_score3"
  [(set (match_operand 0 "score_register_operand" "=d,d")
        (call (mem:SI (match_operand:SI 1 "call_insn_operand" "d,Z"))
              (match_operand 2 "" "")))
   (clobber (reg:SI RA_REGNUM))]
  "(TARGET_SCORE3)"
{
  if (!flag_pic)
    switch (which_alternative)
      {
      case 0: return \"brl!\t%1\";
      case 1: return \"jl\t%1\";
      default: gcc_unreachable ();
      }
  else
    switch (which_alternative)
      {
      case 0: return \"mv!\tr29, %1\;brl!\tr29\";
      case 1: return \"ldi48\tr29, %1\;brl!\tr29\";
      default: gcc_unreachable ();
      }
}
  [(set_attr "length" "4,8")
   (set_attr "type" "call")])

(define_expand "indirect_jump"
  [(set (pc) (match_operand 0 "score_register_operand" "d"))]
  ""
{
  rtx dest;
  dest = operands[0];
  if (GET_CODE (dest) != REG
      || GET_MODE (dest) != Pmode)
    operands[0] = copy_to_mode_reg (Pmode, dest);

  emit_jump_insn (gen_indirect_jump_internal_score (operands[0]));
  DONE;
})

(define_insn "indirect_jump_internal_score"
  [(set (pc) (match_operand:SI 0 "score_register_operand" "d"))]
  ""
  "br%S0   %0"
  [(set_attr "type" "jump")])

(define_expand "casesi"
  [(match_operand:SI 0 "score_register_operand" "")   ; index to jump on
   (match_operand:SI 1 "const_int_operand" "")  ; lower bound
   (match_operand:SI 2 "const_int_operand" "")  ; total range
   (match_operand:SI 3 "" "")                   ; table label
   (match_operand:SI 4 "" "")]                  ; Out of range label
  "TARGET_SCORE3"
{
  rtx reg;
  if (operands[1] != const0_rtx)
    {
      reg = gen_reg_rtx (SImode);
      emit_insn (gen_addsi3 (reg, operands[0],
                             GEN_INT (-INTVAL (operands[1]))));
      operands[0] = reg;
    }

  if (!CONST_OK_FOR_LETTER_P (INTVAL (operands[2]), 'L'))
    operands[2] = force_reg (SImode, operands[2]);

  reg = gen_reg_rtx (SImode);
  emit_jump_insn (gen_score3_casesi_internal (operands[0], operands[2],
                                              operands[3], operands[4], reg));
  DONE;
})

(define_insn "score3_casesi_internal"
  [(parallel [(set (pc)
               (if_then_else
                (leu (match_operand:SI 0 "score_register_operand" "e")
                     (match_operand:SI 1 "arith_operand" "dL"))
                (mem:SI (plus:SI (mult:SI (match_dup 0) (const_int 4))
                                 (label_ref (match_operand 2 "" ""))))
                (label_ref (match_operand 3 "" ""))))
              (clobber (reg:CC CC_REGNUM))
              (clobber (match_operand:SI 4 "score_register_operand" "=e"))
              (use (label_ref (match_dup 2)))])]
  "TARGET_SCORE3 && !flag_pic"
  "* 
   return score_output_casesi(operands);
  "
  [(set_attr "length" "20")])

(define_expand "tablejump"
  [(set (pc)
        (match_operand 0 "score_register_operand" "d"))
   (use (label_ref (match_operand 1 "" "")))]
  ""
{
  if (TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)
    emit_jump_insn (gen_tablejump_internal_score7 (operands[0], operands[1]));
  else if (TARGET_SCORE3)
    emit_jump_insn (gen_tablejump_internal_score3 (operands[0], operands[1]));

  DONE;
})

(define_insn "tablejump_internal_score7"
  [(set (pc)
        (match_operand:SI 0 "register_operand" "d"))
   (use (label_ref (match_operand 1 "" "")))]
  "(TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)"
{
  if (flag_pic)
    return \"mv\tr29, %0\;.cpadd\tr29\;br\tr29\";
  else
    return \"br%S0\t%0\";
}
  [(set_attr "type" "jump")])

(define_insn "tablejump_internal_score3"
  [(set (pc)
        (match_operand:SI 0 "score_register_operand" "d"))
   (use (label_ref (match_operand 1 "" "")))]
  "(TARGET_SCORE3)"
{
  if (flag_pic)
    return \"mv!\tr29, %0\;.cpadd\tr29\;br!\tr29\";
  else
    return \"br%S0\t%0\";
}
  [(set_attr "type" "jump")
   (set_attr "length" "8")])

(define_expand "prologue"
  [(const_int 1)]
  ""
{
  score_prologue ();
  DONE;
})

(define_expand "epilogue"
  [(const_int 2)]
  ""
{
  score_epilogue (false);
  DONE;
})

(define_expand "sibcall_epilogue"
  [(const_int 2)]
  ""
{
  score_epilogue (true);
  DONE;
})

(define_insn "return_internal_score7"
  [(return)
   (use (match_operand 0 "pmode_register_operand" "d"))]
  "(TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)"
  "br%S0\t%0")

(define_insn "return_internal_score3"
  [(return)
   (use (match_operand 0 "pmode_register_operand" "d"))]
  "(TARGET_SCORE3)"
  "br%S0\t%0"
  [(set_attr "length" "4")])

(define_insn "nop"
  [(const_int 0)]
  ""
  "#nop!"
)

(define_insn "cpload_score7"
  [(unspec_volatile:SI [(const_int 1)] CPLOAD)]
  "(TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)
   && flag_pic"
  ".cpload\tr29"
)

(define_insn "cpload_score3"
  [(unspec_volatile:SI [(const_int 1)] CPLOAD)]
  "(TARGET_SCORE3) && flag_pic"
  ".cpload\tr29"
  [(set_attr "length" "4")])

(define_insn "cprestore_use_fp_score7"
  [(unspec_volatile:SI [(match_operand:SI 0 "" "")] CPRESTORE)
   (use (reg:SI FP_REGNUM))]
  "(TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)
   && flag_pic"
  ".cprestore\tr2, %0"
)

(define_insn "cprestore_use_fp_score3"
  [(unspec_volatile:SI [(match_operand:SI 0 "" "")] CPRESTORE)
   (use (reg:SI FP_REGNUM))]
  "(TARGET_SCORE3) && flag_pic"
  ".cprestore\tr2, %0"
  [(set_attr "length" "4")])

(define_insn "cprestore_use_sp_score7"
  [(unspec_volatile:SI [(match_operand:SI 0 "" "")] CPRESTORE)
   (use (reg:SI SP_REGNUM))]
  "(TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)
   && flag_pic"
  ".cprestore\tr0, %0"
)

(define_insn "cprestore_use_sp_score3"
  [(unspec_volatile:SI [(match_operand:SI 0 "" "")] CPRESTORE)
   (use (reg:SI SP_REGNUM))]
  "(TARGET_SCORE3) && flag_pic"
  ".cprestore\tr0, %0"
  [(set_attr "length" "4")])

(define_expand "doloop_end"
  [(use (match_operand 0 "" ""))    ; loop pseudo
   (use (match_operand 1 "" ""))    ; iterations; zero if unknown
   (use (match_operand 2 "" ""))    ; max iterations
   (use (match_operand 3 "" ""))    ; loop level
   (use (match_operand 4 "" ""))]   ; label
  "!TARGET_NHWLOOP"
{
  if (INTVAL (operands[3]) > 1)
    FAIL;

  if (GET_MODE (operands[0]) == SImode)
    {
      rtx sr0 = gen_rtx_REG (SImode, CN_REGNUM);
      emit_jump_insn (gen_doloop_end_si (sr0, operands[4]));
    }
  else
    FAIL;

  DONE;
})

(define_insn "doloop_end_si"
  [(set (pc)
        (if_then_else
         (ne (match_operand:SI 0 "sr0_operand" "")
             (const_int 0))
         (label_ref (match_operand 1 "" ""))
         (pc)))
   (set (match_dup 0)
        (plus:SI (match_dup 0)
                 (const_int -1)))
   (clobber (reg:CC CC_REGNUM))]
  "!TARGET_NHWLOOP"
  "bcnz %1"
  [(set_attr "type" "branch")
   (set_attr "length" "4")])

(define_insn "pushsi_score7"
  [(set (match_operand:SI 0 "push_operand" "=<")
        (match_operand:SI 1 "register_operand" "d"))]
  "(TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)"
  "push!\t%1, [r0]"
  [(set_attr "type" "store")
   (set_attr "mode" "SI")])

(define_insn "pushsi_score3"
  [(set (match_operand:SI 0 "push_operand" "=<")
        (match_operand:SI 1 "register_operand" "d"))]
  "(TARGET_SCORE3)"
  "push!\t%1"
  [(set_attr "type" "store")
   (set_attr "length" "2")
   (set_attr "mode" "SI")])

(define_insn "popsi_score7"
  [(set (match_operand:SI 0 "register_operand" "=d")
        (match_operand:SI 1 "pop_operand" ">"))]
  "(TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)"
  "pop!\t%0, [r0]"
  [(set_attr "type" "store")
   (set_attr "mode" "SI")])

(define_insn "popsi_score3"
  [(set (match_operand:SI 0 "register_operand" "=d")
        (match_operand:SI 1 "pop_operand" ">"))]
  "(TARGET_SCORE3)"
  "pop!\t%0"
  [(set_attr "type" "store")
   (set_attr "length" "2")
   (set_attr "mode" "SI")])

(define_expand "load_multiple"
  [(match_par_dup 3 [(set (match_operand:SI 0 "" "")
                          (match_operand:SI 1 "" ""))
                     (use (match_operand:SI 2 "" ""))
                     (clobber (reg:SI 0))])]
  "(TARGET_SCORE3)"
{
  int regno, count, i;

  if (GET_CODE (operands[2]) != CONST_INT
      || INTVAL (operands[2]) < 2
      || GET_CODE (operands[1]) != MEM
      || XEXP (operands[1], 0) != stack_pointer_rtx
      || GET_CODE (operands[0]) != REG)
    FAIL;

  count = INTVAL (operands[2]);
  regno = REGNO (operands[0]);

  operands[3] = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (count));

  for (i = 0; i < count; i++)
    XVECEXP (operands[3], 0, i)
      = gen_rtx_SET (VOIDmode,
                     gen_rtx_REG (SImode, regno + i),
                     gen_rtx_MEM (SImode, gen_rtx_POST_INC (SImode, stack_pointer_rtx)));
})

(define_insn ""
  [(match_parallel 0 "score_load_multiple_operation"
    [(set (match_operand:SI 1 "register_operand" "=d")
          (mem:SI (post_inc:SI (reg:SI SP_REGNUM))))])]
  "(TARGET_SCORE3)"
{
  return score_rpop (operands);
}
  [(set_attr "length" "2")])

(define_expand "store_multiple"
  [(match_par_dup 3 [(set (match_operand:SI 0 "" "")
                          (match_operand:SI 1 "" ""))
                     (use (match_operand:SI 2 "" ""))
                     (clobber (reg:SI 0))])]
  "(TARGET_SCORE3)"
{
  int regno, count, i;

  if (GET_CODE (operands[2]) != CONST_INT
      || INTVAL (operands[2]) < 2
      || GET_CODE (operands[0]) != MEM
      || XEXP (operands[0], 0) != stack_pointer_rtx
      || GET_CODE (operands[1]) != REG)
    FAIL;

  count = INTVAL (operands[2]);
  regno = REGNO (operands[1]);

  operands[3] = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (count));

  for (i = 0; i < count; i++)
    XVECEXP (operands[3], 0, i)
      = gen_rtx_SET (VOIDmode,
                 gen_rtx_MEM (SImode, gen_rtx_PRE_DEC (SImode, stack_pointer_rtx)),
                 gen_rtx_REG (SImode, regno + i));
})

(define_insn ""
  [(match_parallel 0 "score_store_multiple_operation"
    [(set (mem:SI (pre_dec:SI (reg:SI SP_REGNUM)))
          (match_operand:SI 1 "register_operand" "d"))])]
  "(TARGET_SCORE3)"
{
  return score_rpush (operands);
}
  [(set_attr "length" "2")])

(define_peephole2
  [(set (match_operand:SI 0 "g32reg_operand" "")
        (match_operand:SI 1 "loreg_operand" ""))
   (set (match_operand:SI 2 "g32reg_operand" "")
        (match_operand:SI 3 "hireg_operand" ""))]
  ""
  [(parallel
       [(set (match_dup 0) (match_dup 1))
        (set (match_dup 2) (match_dup 3))])])

(define_peephole2
  [(set (match_operand:SI 0 "g32reg_operand" "")
        (match_operand:SI 1 "hireg_operand" ""))
   (set (match_operand:SI 2 "g32reg_operand" "")
        (match_operand:SI 3 "loreg_operand" ""))]
  ""
  [(parallel
       [(set (match_dup 2) (match_dup 3))
        (set (match_dup 0) (match_dup 1))])])

(define_insn "movhilo"
  [(parallel
    [(set (match_operand:SI 0 "register_operand" "=d")
          (match_operand:SI 1 "loreg_operand" ""))
     (set (match_operand:SI 2 "register_operand" "=d")
          (match_operand:SI 3 "hireg_operand" ""))])]
  ""
  "mfcehl\t%2, %0"
  [(set_attr "type" "fce")
   (set_attr "mode" "SI")])

(define_expand "movsicc"
  [(set (match_operand:SI 0 "register_operand" "")
        (if_then_else:SI (match_operator 1 "comparison_operator"
                          [(reg:CC CC_REGNUM) (const_int 0)])
                         (match_operand:SI 2 "register_operand" "")
                         (match_operand:SI 3 "register_operand" "")))]
  ""
{
  score_movsicc (operands);
})

(define_insn "movsicc_internal_score7"
  [(set (match_operand:SI 0 "register_operand" "=d")
        (if_then_else:SI (match_operator 1 "comparison_operator"
                          [(reg:CC CC_REGNUM) (const_int 0)])
                         (match_operand:SI 2 "arith_operand" "d")
                         (match_operand:SI 3 "arith_operand" "0")))]
  "(TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)"
  "mv%C1\t%0, %2"
  [(set_attr "type" "cndmv")
   (set_attr "mode" "SI")])

(define_insn "movsicc_internal_score3"
  [(set (match_operand:SI 0 "register_operand" "=d")
        (if_then_else:SI (match_operator 1 "comparison_operator"
                          [(reg:CC CC_REGNUM) (const_int 0)])
                         (match_operand:SI 2 "arith_operand" "d")
                         (match_operand:SI 3 "arith_operand" "0")))]
  "(TARGET_SCORE3)"
  "mv%G1\t%0, %2"
  [(set_attr "type" "cndmv")
   (set_attr "length" "4")
   (set_attr "mode" "SI")])

(define_insn "zero_extract_bittst_score7"
  [(set (reg:CC_NZ CC_REGNUM)
        (compare:CC_NZ (unspec:SI
                        [(match_operand:SI 0 "register_operand" "*e,d")
                         (match_operand:SI 1 "const_uimm5" "")]
                        BITTST)
                       (const_int 0)))]
  "(TARGET_SCORE5 || TARGET_SCORE5U || TARGET_SCORE7 || TARGET_SCORE7D)"
  "@
   bittst!\t%0, %c1
   bittst.c\t%0, %c1"
  [(set_attr "type" "arith")
   (set_attr "up_c" "yes")
   (set_attr "mode" "SI")])

(define_insn "zero_extract_bittst_score3"
  [(set (reg:CC_NZ CC_REGNUM)
        (compare:CC_NZ (unspec:SI
                        [(match_operand:SI 0 "register_operand" "e,d")
                         (match_operand:SI 1 "const_uimm5" "")]
                        BITTST)
                       (const_int 0)))]
  "(TARGET_SCORE3)"
  "@
   bittst!\t%0, %c1
   bittst.c\t%0, %c1"
  [(set_attr "type" "arith")
   (set_attr "length" "2,4")
   (set_attr "up_c" "yes")
   (set_attr "mode" "SI")])

(define_expand "extzv"
  [(set (match_operand:SI 0 "register_operand" "")
        (zero_extract (match_operand:SI 1 "memory_operand" "")
                      (match_operand:SI 2 "immediate_operand" "")
                      (match_operand:SI 3 "immediate_operand" "")))]
  "((TARGET_SCORE5 || TARGET_SCORE7 || TARGET_SCORE7D)
    && (!TARGET_LITTLE_ENDIAN) && (TARGET_ULS))
   || (TARGET_SCORE3 && TARGET_ULS)"
{
  if (score_unaligned_load (operands))
    DONE;
  else
    FAIL;
})

(define_expand "insv"
  [(set (zero_extract (match_operand:SI 0 "memory_operand" "")
                      (match_operand:SI 1 "immediate_operand" "")
                      (match_operand:SI 2 "immediate_operand" ""))
        (match_operand:SI 3 "register_operand" ""))]
  "((TARGET_SCORE5 || TARGET_SCORE7 || TARGET_SCORE7D)
    && (!TARGET_LITTLE_ENDIAN) && (TARGET_ULS))
   || (TARGET_SCORE3 && TARGET_ULS)"
{
  if (score_unaligned_store (operands))
    DONE;
  else
    FAIL;
})

(define_expand "extv"
  [(set (match_operand:SI 0 "register_operand" "")
        (sign_extract (match_operand:SI 1 "memory_operand" "")
                      (match_operand:SI 2 "immediate_operand" "")
                      (match_operand:SI 3 "immediate_operand" "")))]
  "((TARGET_SCORE5 || TARGET_SCORE7 || TARGET_SCORE7D)
    && (!TARGET_LITTLE_ENDIAN) && (TARGET_ULS))
   || (TARGET_SCORE3 && TARGET_ULS)"
{
  if (score_unaligned_load (operands))
    DONE;
  else
    FAIL;
})

(define_expand "movmemsi"
  [(parallel [(set (match_operand:BLK 0 "general_operand")
                   (match_operand:BLK 1 "general_operand"))
              (use (match_operand:SI 2 ""))
              (use (match_operand:SI 3 "const_int_operand"))])]
  "((TARGET_SCORE5 || TARGET_SCORE7 || TARGET_SCORE7D)
     && (TARGET_ULS))
    || (TARGET_SCORE3 && TARGET_ULS)"
{
  if (score_block_move (operands))
    DONE;
  else
    FAIL;
})

(define_insn "move_lbu_a"
  [(set (match_operand:SI 0 "register_operand" "=d")
        (plus:SI (match_operand:SI 1 "register_operand" "0")
                 (match_operand:SI 2 "const_simm12" "")))
   (set (match_operand:QI 3 "register_operand" "=d")
        (mem:QI (match_dup 1)))]
  ""
  "lbu\t%3, [%1]+, %2"
  [(set_attr "type" "load")
   (set_attr "length" "4")
   (set_attr "mode" "QI")])

(define_insn "move_lhu_a"
  [(set (match_operand:SI 0 "register_operand" "=d")
        (plus:SI (match_operand:SI 1 "register_operand" "0")
                 (match_operand:SI 2 "const_simm12" "")))
   (set (match_operand:HI 3 "register_operand" "=d")
        (mem:HI (match_dup 1)))]
  ""
  "lhu\t%3, [%1]+, %2"
  [(set_attr "type" "load")
   (set_attr "length" "4")
   (set_attr "mode" "HI")])

(define_insn "move_lw_a"
  [(set (match_operand:SI 0 "register_operand" "=d")
        (plus:SI (match_operand:SI 1 "register_operand" "0")
                 (match_operand:SI 2 "const_simm12" "")))
   (set (match_operand:SI 3 "register_operand" "=d")
        (mem:SI (match_dup 1)))]
  ""
  "lw\t%3, [%1]+, %2"
  [(set_attr "type" "load")
   (set_attr "length" "4")
   (set_attr "mode" "SI")])

(define_insn "move_sb_a"
  [(set (match_operand:SI 0 "register_operand" "=d")
        (plus:SI (match_operand:SI 1 "register_operand" "0")
                 (match_operand:SI 2 "const_simm12" "")))
   (set (mem:QI (match_dup 1))
        (match_operand:QI 3 "register_operand" "d"))]
  ""
  "sb\t%3, [%1]+, %2"
  [(set_attr "type" "store")
   (set_attr "length" "4")
   (set_attr "mode" "QI")])

(define_insn "move_sh_a"
  [(set (match_operand:SI 0 "register_operand" "=d")
        (plus:SI (match_operand:SI 1 "register_operand" "0")
                 (match_operand:SI 2 "const_simm12" "")))
   (set (mem:HI (match_dup 1))
        (match_operand:HI 3 "register_operand" "d"))]
  ""
  "sh\t%3, [%1]+, %2"
  [(set_attr "type" "store")
   (set_attr "length" "4")
   (set_attr "mode" "HI")])

(define_insn "move_sw_a"
  [(set (match_operand:SI 0 "register_operand" "=d")
        (plus:SI (match_operand:SI 1 "register_operand" "0")
                 (match_operand:SI 2 "const_simm12" "")))
   (set (mem:SI (match_dup 1))
        (match_operand:SI 3 "register_operand" "d"))]
  ""
  "sw\t%3, [%1]+, %2"
  [(set_attr "type" "store")
   (set_attr "length" "4")
   (set_attr "mode" "SI")])

(define_insn "move_lbu_b"
  [(set (match_operand:SI 0 "register_operand" "=d")
        (plus:SI (match_operand:SI 1 "register_operand" "0")
                 (match_operand:SI 2 "const_simm12" "")))
   (set (match_operand:QI 3 "register_operand" "=d")
        (mem:QI (plus:SI (match_dup 1)
                         (match_dup 2))))]
  ""
  "lbu\t%3, [%1, %2]+"
  [(set_attr "type" "load")
   (set_attr "length" "4")
   (set_attr "mode" "QI")])

(define_insn "move_lhu_b"
  [(set (match_operand:SI 0 "register_operand" "=d")
        (plus:SI (match_operand:SI 1 "register_operand" "0")
                 (match_operand:SI 2 "const_simm12" "")))
   (set (match_operand:HI 3 "register_operand" "=d")
        (mem:HI (plus:SI (match_dup 1)
                         (match_dup 2))))]
  ""
  "lhu\t%3, [%1, %2]+"
  [(set_attr "type" "load")
   (set_attr "length" "4")
   (set_attr "mode" "HI")])

(define_insn "move_lw_b"
  [(set (match_operand:SI 0 "register_operand" "=d")
        (plus:SI (match_operand:SI 1 "register_operand" "0")
                 (match_operand:SI 2 "const_simm12" "")))
   (set (match_operand:SI 3 "register_operand" "=d")
        (mem:SI (plus:SI (match_dup 1)
                         (match_dup 2))))]
  ""
  "lw\t%3, [%1, %2]+"
  [(set_attr "type" "load")
   (set_attr "length" "4")
   (set_attr "mode" "SI")])

(define_insn "move_sb_b"
  [(set (match_operand:SI 0 "register_operand" "=d")
        (plus:SI (match_operand:SI 1 "register_operand" "0")
                 (match_operand:SI 2 "const_simm12" "")))
   (set (mem:QI (plus:SI (match_dup 1)
                         (match_dup 2)))
        (match_operand:QI 3 "register_operand" "d"))]
  ""
  "sb\t%3, [%1, %2]+"
  [(set_attr "type" "store")
   (set_attr "length" "4")
   (set_attr "mode" "QI")])

(define_insn "move_sh_b"
  [(set (match_operand:SI 0 "register_operand" "=d")
        (plus:SI (match_operand:SI 1 "register_operand" "0")
                 (match_operand:SI 2 "const_simm12" "")))
   (set (mem:HI (plus:SI (match_dup 1)
                         (match_dup 2)))
        (match_operand:HI 3 "register_operand" "d"))]
  ""
  "sh\t%3, [%1, %2]+"
  [(set_attr "type" "store")
   (set_attr "length" "4")
   (set_attr "mode" "HI")])

(define_insn "move_sw_b"
  [(set (match_operand:SI 0 "register_operand" "=d")
        (plus:SI (match_operand:SI 1 "register_operand" "0")
                 (match_operand:SI 2 "const_simm12" "")))
   (set (mem:SI (plus:SI (match_dup 1)
                         (match_dup 2)))
        (match_operand:SI 3 "register_operand" "d"))]
  ""
  "sw\t%3, [%1, %2]+"
  [(set_attr "type" "store")
   (set_attr "length" "4")
   (set_attr "mode" "SI")])

(define_insn "move_lcb"
  [(set (match_operand:SI 0 "register_operand" "=d")
        (plus:SI (match_operand:SI 1 "register_operand" "0")
                 (const_int 4)))
   (set (reg:SI LC_REGNUM)
        (unspec:SI [(mem:BLK (match_dup 1))] LCB))]
  "((TARGET_SCORE5 || TARGET_SCORE7 || TARGET_SCORE7D)
     && (!TARGET_LITTLE_ENDIAN) && (TARGET_ULS))
    || (TARGET_SCORE3 && TARGET_ULS)"
  "lcb\t[%1]+"
  [(set_attr "type" "load")
   (set_attr "length" "4")
   (set_attr "mode" "SI")])

(define_insn "move_lcw"
  [(set (match_operand:SI 0 "register_operand" "=d")
        (plus:SI (match_operand:SI 1 "register_operand" "0")
                 (const_int 4)))
   (set (match_operand:SI 2 "register_operand" "=d")
        (unspec:SI [(mem:BLK (match_dup 1))
                    (reg:SI LC_REGNUM)] LCW))
   (set (reg:SI LC_REGNUM)
        (unspec:SI [(mem:BLK (match_dup 1))] LCB))]
  "((TARGET_SCORE5 || TARGET_SCORE7 || TARGET_SCORE7D)
     && (!TARGET_LITTLE_ENDIAN) && (TARGET_ULS))
    || (TARGET_SCORE3 && TARGET_ULS)"
  "lcw\t%2, [%1]+"
  [(set_attr "type" "load")
   (set_attr "length" "4")
   (set_attr "mode" "SI")])

(define_insn "move_lce"
  [(set (match_operand:SI 0 "register_operand" "=d")
        (plus:SI (match_operand:SI 1 "register_operand" "0")
                 (const_int 4)))
   (set (match_operand:SI 2 "register_operand" "=d")
        (unspec:SI [(mem:BLK (match_dup 1))
                    (reg:SI LC_REGNUM)] LCE))]
  "((TARGET_SCORE5 || TARGET_SCORE7 || TARGET_SCORE7D)
     && (!TARGET_LITTLE_ENDIAN) && (TARGET_ULS))
    || (TARGET_SCORE3 && TARGET_ULS)"
  "lce\t%2, [%1]+"
  [(set_attr "type" "load")
   (set_attr "length" "4")
   (set_attr "mode" "SI")])

(define_insn "move_scb"
  [(set (match_operand:SI 0 "register_operand" "=d")
        (plus:SI (match_operand:SI 1 "register_operand" "0")
                 (const_int 4)))
   (set (mem:BLK (match_dup 1))
        (unspec:BLK [(match_operand:SI 2 "register_operand" "d")] SCB))
   (set (reg:SI SC_REGNUM)
        (unspec:SI [(match_dup 2)] SCLC))]
  "((TARGET_SCORE5 || TARGET_SCORE7 || TARGET_SCORE7D)
     && (!TARGET_LITTLE_ENDIAN) && (TARGET_ULS))
    || (TARGET_SCORE3 && TARGET_ULS)"
  "scb\t%2, [%1]+"
  [(set_attr "type" "store")
   (set_attr "length" "4")
   (set_attr "mode" "SI")])

(define_insn "move_scw"
  [(set (match_operand:SI 0 "register_operand" "=d")
        (plus:SI (match_operand:SI 1 "register_operand" "0")
                 (const_int 4)))
   (set (mem:BLK (match_dup 1))
        (unspec:BLK [(match_operand:SI 2 "register_operand" "d")
                     (reg:SI SC_REGNUM)] SCW))
   (set (reg:SI SC_REGNUM)
        (unspec:SI [(match_dup 2)] SCLC))]
  "((TARGET_SCORE5 || TARGET_SCORE7 || TARGET_SCORE7D)
     && (!TARGET_LITTLE_ENDIAN) && (TARGET_ULS))
    || (TARGET_SCORE3 && TARGET_ULS)"
  "scw\t%2, [%1]+"
  [(set_attr "type" "store")
   (set_attr "length" "4")
   (set_attr "mode" "SI")])

(define_insn "move_sce"
  [(set (match_operand:SI 0 "register_operand" "=d")
        (plus:SI (match_operand:SI 1 "register_operand" "0")
                 (const_int 4)))
   (set (mem:BLK (match_dup 1))
        (unspec:BLK [(reg:SI SC_REGNUM)] SCE))]
  "((TARGET_SCORE5 || TARGET_SCORE7 || TARGET_SCORE7D)
     && (!TARGET_LITTLE_ENDIAN) && (TARGET_ULS))
    || (TARGET_SCORE3 && TARGET_ULS)"
  "sce     [%1]+"
  [(set_attr "type" "store")
   (set_attr "length" "4")
   (set_attr "mode" "SI")])

(define_insn "andsi3_extzh"
  [(set (match_operand:SI 0 "register_operand" "=d")
        (and:SI (match_operand:SI 1 "register_operand" "d")
                (const_int 65535)))]
  ""
  "extzh\t%0, %1"
  [(set_attr "type" "arith")
   (set_attr "length" "4")
   (set_attr "mode" "SI")])

(define_insn "clzsi2"
  [(set (match_operand:SI 0 "register_operand" "=d")
        (clz:SI (match_operand:SI 1 "register_operand" "d")))]
  "(TARGET_SCORE7D || TARGET_SCORE3)"
  "clz\t%0, %1"
  [(set_attr "type" "arith")
   (set_attr "mode" "SI")])

(define_insn "smaxsi3"
  [(set (match_operand:SI 0 "register_operand" "=d")
        (smax:SI (match_operand:SI 1 "register_operand" "d")
                 (match_operand:SI 2 "register_operand" "d")))]
  "(TARGET_SCORE7D || TARGET_SCORE3D)"
  "max\t%0, %1, %2"
  [(set_attr "type" "arith")
   (set_attr "mode" "SI")])

(define_insn "sminsi3"
  [(set (match_operand:SI 0 "register_operand" "=d")
        (smin:SI (match_operand:SI 1 "register_operand" "d")
                 (match_operand:SI 2 "register_operand" "d")))]
  "(TARGET_SCORE7D || TARGET_SCORE3D)"
  "min\t%0, %1, %2"
  [(set_attr "type" "arith")
   (set_attr "mode" "SI")])

(define_insn "abssi2"
  [(set (match_operand:SI 0 "register_operand" "=d")
        (abs:SI (match_operand:SI 1 "register_operand" "d")))]
  "(TARGET_SCORE7D || TARGET_SCORE3D)"
  "abs\t%0, %1"
  [(set_attr "type" "arith")
   (set_attr "mode" "SI")])

(define_insn "sffs"
  [(set (match_operand:SI 0 "register_operand" "=d")
        (unspec:SI [(match_operand:SI 1 "register_operand" "d")] SFFS))]
  "(TARGET_SCORE7D || TARGET_SCORE3D)"
  "bitrev\t%0, %1, r0\;clz\t%0, %0\;addi\t%0, 0x1"
  [(set_attr "type" "arith")
   (set_attr "mode" "SI")])

(define_expand "ffssi2"
  [(set (match_operand:SI 0 "register_operand")
        (ffs:SI (match_operand:SI 1 "register_operand")))]
  "(TARGET_SCORE7D || TARGET_SCORE3D)"
{
  emit_insn (gen_sffs (operands[0], operands[1]));
  emit_insn (gen_rtx_SET (VOIDmode, gen_rtx_REG (CC_NZmode, CC_REGNUM),
                          gen_rtx_COMPARE (CC_NZmode, operands[0],
                                           GEN_INT (33))));
  if (TARGET_SCORE7D)
    emit_insn (gen_movsicc_internal_score7 (operands[0],
               gen_rtx_fmt_ee (EQ, VOIDmode, operands[0], GEN_INT (33)),
               GEN_INT (0),
               operands[0]));
  else
    emit_insn (gen_movsicc_internal_score3 (operands[0],
               gen_rtx_fmt_ee (EQ, VOIDmode, operands[0], GEN_INT (33)),
               GEN_INT (0),
               operands[0]));
  DONE;
})

(define_peephole2
  [(set (match_operand:SI 0 "loreg_operand" "")
        (match_operand:SI 1 "register_operand" ""))
   (set (match_operand:SI 2 "hireg_operand" "")
        (match_operand:SI 3 "register_operand" ""))]
  "(TARGET_SCORE7D || TARGET_SCORE3D)"
  [(parallel
       [(set (match_dup 0) (match_dup 1))
        (set (match_dup 2) (match_dup 3))])])

(define_peephole2
  [(set (match_operand:SI 0 "hireg_operand" "")
        (match_operand:SI 1 "register_operand" ""))
   (set (match_operand:SI 2 "loreg_operand" "")
        (match_operand:SI 3 "register_operand" ""))]
  "(TARGET_SCORE7D || TARGET_SCORE3D)"
  [(parallel
       [(set (match_dup 2) (match_dup 3))
        (set (match_dup 0) (match_dup 1))])])

(define_insn "movtohilo"
  [(parallel
       [(set (match_operand:SI 0 "loreg_operand" "=l")
             (match_operand:SI 1 "register_operand" "d"))
        (set (match_operand:SI 2 "hireg_operand" "=h")
             (match_operand:SI 3 "register_operand" "d"))])]
  "(TARGET_SCORE7D || TARGET_SCORE3D)"
  "mtcehl\t%3, %1"
  [(set_attr "type" "fce")
   (set_attr "mode" "SI")])

(define_insn "mulsi3addsi"
  [(set (match_operand:SI 0 "register_operand" "=l,l,d")
        (plus:SI (mult:SI (match_operand:SI 2 "register_operand" "d,d,d")
                          (match_operand:SI 3 "register_operand" "d,d,d"))
                 (match_operand:SI 1 "register_operand" "0,d,l")))
   (clobber (reg:SI HI_REGNUM))]
  "(TARGET_SCORE7D || TARGET_SCORE3D)"
  "@
   mad\t%2, %3
   mtcel%S1\t%1\;mad\t%2, %3
   mad\t%2, %3\;mfcel%S0\t%0"
  [(set_attr "mode" "SI")])

(define_insn "mulsi3subsi"
  [(set (match_operand:SI 0 "register_operand" "=l,l,d")
        (minus:SI (match_operand:SI 1 "register_operand" "0,d,l")
                  (mult:SI (match_operand:SI 2 "register_operand" "d,d,d")
                           (match_operand:SI 3 "register_operand" "d,d,d"))))
   (clobber (reg:SI HI_REGNUM))]
  "(TARGET_SCORE7D || TARGET_SCORE3D)"
  "@
   msb\t%2, %3
   mtcel%S1\t%1\;msb\t%2, %3
   msb\t%2, %3\;mfcel%S0\t%0"
  [(set_attr "mode" "SI")])

(define_insn "mulsidi3adddi"
  [(set (match_operand:DI 0 "register_operand" "=x")
        (plus:DI (mult:DI
                  (sign_extend:DI (match_operand:SI 2 "register_operand" "%d"))
                  (sign_extend:DI (match_operand:SI 3 "register_operand" "d")))
                 (match_operand:DI 1 "register_operand" "0")))]
  "(TARGET_SCORE7D || TARGET_SCORE3D)"
  "mad\t%2, %3"
  [(set_attr "mode" "DI")])

(define_insn "umulsidi3adddi"
  [(set (match_operand:DI 0 "register_operand" "=x")
        (plus:DI (mult:DI
                  (zero_extend:DI (match_operand:SI 2 "register_operand" "%d"))
                  (zero_extend:DI (match_operand:SI 3 "register_operand" "d")))
                 (match_operand:DI 1 "register_operand" "0")))]
  "(TARGET_SCORE7D || TARGET_SCORE3D)"
  "madu\t%2, %3"
  [(set_attr "mode" "DI")])

(define_insn "mulsidi3subdi"
  [(set (match_operand:DI 0 "register_operand" "=x")
        (minus:DI
         (match_operand:DI 1 "register_operand" "0")
         (mult:DI
          (sign_extend:DI (match_operand:SI 2 "register_operand" "%d"))
          (sign_extend:DI (match_operand:SI 3 "register_operand" "d")))))]
  "(TARGET_SCORE7D || TARGET_SCORE3D)"
  "msb\t%2, %3"
  [(set_attr "mode" "DI")])

(define_insn "umulsidi3subdi"
  [(set (match_operand:DI 0 "register_operand" "=x")
        (minus:DI
         (match_operand:DI 1 "register_operand" "0")
         (mult:DI (zero_extend:DI
                   (match_operand:SI 2 "register_operand" "%d"))
                  (zero_extend:DI
                   (match_operand:SI 3 "register_operand" "d")))))]
  "(TARGET_SCORE7D || TARGET_SCORE3D)"
  "msbu\t%2, %3"
  [(set_attr "mode" "DI")])

