;;   Machine description for GNU compiler,
;;   for Atmel AVR micro controllers.
;;   Copyright (C) 1998 - 2011
;;   Free Software Foundation, Inc.
;;   Contributed by Georg Lay (avr@gjlay.de)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The purpose of this file is to provide a light-weight DImode
;; implementation for AVR.  The trouble with DImode is that tree -> RTL
;; lowering leads to really unpleasant code for operations that don't
;; work byte-wise like NEG, PLUS, MINUS, etc.  Defining optabs entries for
;; them won't help because the optab machinery assumes these operations
;; are cheap and does not check if a libgcc implementation is available.
;;
;; The DImode insns are all straight forward -- except movdi.  The approach
;; of this implementation is to provide DImode insns without the burden of
;; introducing movdi.
;; 
;; The caveat is that if there are insns for some mode, there must also be a
;; respective move insn that describes reloads.  Therefore, this
;; implementation uses an accumulator-based model with two hard-coded,
;; accumulator-like registers
;;
;;    A[] = reg:DI 18
;;    B[] = reg:DI 10
;;
;; so that no DImode insn contains pseudos or needs reloading.

(define_constants
  [(ACC_A	18)
   (ACC_B	10)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Addition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_expand "adddi3"
  [(parallel [(match_operand:DI 0 "general_operand" "")
              (match_operand:DI 1 "general_operand" "")
              (match_operand:DI 2 "general_operand" "")])]
  "avr_have_dimode"
  {
    rtx acc_a = gen_rtx_REG (DImode, ACC_A);

    emit_move_insn (acc_a, operands[1]);

    if (s8_operand (operands[2], VOIDmode))
      {
        emit_move_insn (gen_rtx_REG (QImode, REG_X), operands[2]);
        emit_insn (gen_adddi3_const8_insn ());
      }        
    else if (CONST_INT_P (operands[2])
             || CONST_DOUBLE_P (operands[2]))
      {
        emit_insn (gen_adddi3_const_insn (operands[2]));
      }
    else
      {
        emit_move_insn (gen_rtx_REG (DImode, ACC_B), operands[2]);
        emit_insn (gen_adddi3_insn ());
      }

    emit_move_insn (operands[0], acc_a);
    DONE;
  })

(define_insn "adddi3_insn"
  [(set (reg:DI ACC_A)
        (plus:DI (reg:DI ACC_A)
                 (reg:DI ACC_B)))]
  "avr_have_dimode"
  "%~call __adddi3"
  [(set_attr "adjust_len" "call")
   (set_attr "cc" "clobber")])

(define_insn "adddi3_const8_insn"
  [(set (reg:DI ACC_A)
        (plus:DI (reg:DI ACC_A)
                 (sign_extend:DI (reg:QI REG_X))))]
  "avr_have_dimode"
  "%~call __adddi3_s8"
  [(set_attr "adjust_len" "call")
   (set_attr "cc" "clobber")])

(define_insn "adddi3_const_insn"
  [(set (reg:DI ACC_A)
        (plus:DI (reg:DI ACC_A)
                 (match_operand:DI 0 "const_double_operand" "n")))]
  "avr_have_dimode
   && !s8_operand (operands[0], VOIDmode)"
  {
    return avr_out_plus64 (operands[0], NULL);
  }
  [(set_attr "adjust_len" "plus64")
   (set_attr "cc" "clobber")])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subtraction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_expand "subdi3"
  [(parallel [(match_operand:DI 0 "general_operand" "")
              (match_operand:DI 1 "general_operand" "")
              (match_operand:DI 2 "general_operand" "")])]
  "avr_have_dimode"
  {
    rtx acc_a = gen_rtx_REG (DImode, ACC_A);

    emit_move_insn (acc_a, operands[1]);
    emit_move_insn (gen_rtx_REG (DImode, ACC_B), operands[2]);
    emit_insn (gen_subdi3_insn ());
    emit_move_insn (operands[0], acc_a);
    DONE;
  })

(define_insn "subdi3_insn"
  [(set (reg:DI ACC_A)
        (minus:DI (reg:DI ACC_A)
                  (reg:DI ACC_B)))]
  "avr_have_dimode"
  "%~call __subdi3"
  [(set_attr "adjust_len" "call")
   (set_attr "cc" "set_czn")])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Negation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_expand "negdi2"
  [(parallel [(match_operand:DI 0 "general_operand" "")
              (match_operand:DI 1 "general_operand" "")])]
  "avr_have_dimode"
  {
    rtx acc_a = gen_rtx_REG (DImode, ACC_A);

    emit_move_insn (acc_a, operands[1]);
    emit_insn (gen_negdi2_insn ());
    emit_move_insn (operands[0], acc_a);
    DONE;
  })

(define_insn "negdi2_insn"
  [(set (reg:DI ACC_A)
        (neg:DI (reg:DI ACC_A)))]
  "avr_have_dimode"
  "%~call __negdi2"
  [(set_attr "adjust_len" "call")
   (set_attr "cc" "clobber")])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Comparison
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_expand "conditional_jump"
  [(set (pc)
        (if_then_else
         (match_operator 0 "ordered_comparison_operator" [(cc0)
                                                          (const_int 0)])
         (label_ref (match_operand 1 "" ""))
         (pc)))]
  "avr_have_dimode")

(define_expand "cbranchdi4"
  [(parallel [(match_operand:DI 1 "register_operand" "")
              (match_operand:DI 2 "nonmemory_operand" "")
              (match_operator 0 "ordered_comparison_operator" [(cc0)
                                                               (const_int 0)])
              (label_ref (match_operand 3 "" ""))])]
  "avr_have_dimode"
  {
    rtx acc_a = gen_rtx_REG (DImode, ACC_A);

    emit_move_insn (acc_a, operands[1]);

    if (s8_operand (operands[2], VOIDmode))
      {
        emit_move_insn (gen_rtx_REG (QImode, REG_X), operands[2]);
        emit_insn (gen_compare_const8_di2 ());
      }        
    else if (CONST_INT_P (operands[2])
             || CONST_DOUBLE_P (operands[2]))
      {
        emit_insn (gen_compare_const_di2 (operands[2]));
      }
    else
      {
        emit_move_insn (gen_rtx_REG (DImode, ACC_B), operands[2]);
        emit_insn (gen_compare_di2 ());
      }

    emit_jump_insn (gen_conditional_jump (operands[0], operands[3]));
    DONE;
  })

(define_insn "compare_di2"
  [(set (cc0)
        (compare (reg:DI ACC_A)
                 (reg:DI ACC_B)))]
  "avr_have_dimode"
  "%~call __cmpdi2"
  [(set_attr "adjust_len" "call")
   (set_attr "cc" "compare")])

(define_insn "compare_const8_di2"
  [(set (cc0)
        (compare (reg:DI ACC_A)
                 (sign_extend:DI (reg:QI REG_X))))]
  "avr_have_dimode"
  "%~call __cmpdi2_s8"
  [(set_attr "adjust_len" "call")
   (set_attr "cc" "compare")])

(define_insn "compare_const_di2"
  [(set (cc0)
        (compare (reg:DI ACC_A)
                 (match_operand:DI 0 "const_double_operand" "n")))
   (clobber (match_scratch:QI 1 "=&d"))]
  "avr_have_dimode
   && !s8_operand (operands[0], VOIDmode)"
  {
    return avr_out_compare64 (insn, operands, NULL);
  }
  [(set_attr "adjust_len" "compare64")
   (set_attr "cc" "compare")])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shifts and Rotate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_code_iterator di_shifts
  [ashift ashiftrt lshiftrt rotate])

;; Shift functions from libgcc are called without defining these insns,
;; but with them we can describe their reduced register footprint.

;; "ashldi3"
;; "ashrdi3"
;; "lshrdi3"
;; "rotldi3"
(define_expand "<code_stdname>di3"
  [(parallel [(match_operand:DI 0 "general_operand" "")
              (di_shifts:DI (match_operand:DI 1 "general_operand" "")
                            (match_operand:QI 2 "general_operand" ""))])]
  "avr_have_dimode"
  {
    rtx acc_a = gen_rtx_REG (DImode, ACC_A);

    emit_move_insn (acc_a, operands[1]);
    emit_move_insn (gen_rtx_REG (QImode, 16), operands[2]);
    emit_insn (gen_<code_stdname>di3_insn ());
    emit_move_insn (operands[0], acc_a);
    DONE;
  })

(define_insn "<code_stdname>di3_insn"
  [(set (reg:DI ACC_A)
        (di_shifts:DI (reg:DI ACC_A)
                      (reg:QI 16)))]
  "avr_have_dimode"
  "%~call __<code_stdname>di3"
  [(set_attr "adjust_len" "call")
   (set_attr "cc" "clobber")])
