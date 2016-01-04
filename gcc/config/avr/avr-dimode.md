;;   Machine description for GNU compiler,
;;   for Atmel AVR micro controllers.
;;   Copyright (C) 1998-2016 Free Software Foundation, Inc.
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

;; Supported modes that are 8 bytes wide
(define_mode_iterator ALL8 [DI DQ UDQ DA UDA TA UTA])

(define_mode_iterator ALL8U [UDQ UDA UTA])
(define_mode_iterator ALL8S [ DQ  DA  TA])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Addition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; "adddi3"
;; "adddq3" "addudq3"
;; "addda3" "adduda3"
;; "addta3" "adduta3"
(define_expand "add<mode>3"
  [(parallel [(match_operand:ALL8 0 "general_operand" "")
              (match_operand:ALL8 1 "general_operand" "")
              (match_operand:ALL8 2 "general_operand" "")])]
  "avr_have_dimode"
  {
    rtx acc_a = gen_rtx_REG (<MODE>mode, ACC_A);

    avr_fix_inputs (operands, 1 << 2, regmask (<MODE>mode, ACC_A));
    emit_move_insn (acc_a, operands[1]);

    if (DImode == <MODE>mode
        && s8_operand (operands[2], VOIDmode))
      {
        emit_move_insn (gen_rtx_REG (QImode, REG_X), operands[2]);
        emit_insn (gen_adddi3_const8_insn ());
      }
    else if (const_operand (operands[2], GET_MODE (operands[2])))
      {
        emit_insn (gen_add<mode>3_const_insn (operands[2]));
      }
    else
      {
        emit_move_insn (gen_rtx_REG (<MODE>mode, ACC_B), operands[2]);
        emit_insn (gen_add<mode>3_insn ());
      }

    emit_move_insn (operands[0], acc_a);
    DONE;
  })

;; "adddi3_insn"
;; "adddq3_insn" "addudq3_insn"
;; "addda3_insn" "adduda3_insn"
;; "addta3_insn" "adduta3_insn"
(define_insn "add<mode>3_insn"
  [(set (reg:ALL8 ACC_A)
        (plus:ALL8 (reg:ALL8 ACC_A)
                   (reg:ALL8 ACC_B)))]
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

;; "adddi3_const_insn"
;; "adddq3_const_insn" "addudq3_const_insn"
;; "addda3_const_insn" "adduda3_const_insn"
;; "addta3_const_insn" "adduta3_const_insn"
(define_insn "add<mode>3_const_insn"
  [(set (reg:ALL8 ACC_A)
        (plus:ALL8 (reg:ALL8 ACC_A)
                   (match_operand:ALL8 0 "const_operand" "n Ynn")))]
  "avr_have_dimode
   && !s8_operand (operands[0], VOIDmode)"
  {
    return avr_out_plus (insn, operands);
  }
  [(set_attr "adjust_len" "plus")
   (set_attr "cc" "clobber")])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subtraction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; "subdi3"
;; "subdq3" "subudq3"
;; "subda3" "subuda3"
;; "subta3" "subuta3"
(define_expand "sub<mode>3"
  [(parallel [(match_operand:ALL8 0 "general_operand" "")
              (match_operand:ALL8 1 "general_operand" "")
              (match_operand:ALL8 2 "general_operand" "")])]
  "avr_have_dimode"
  {
    rtx acc_a = gen_rtx_REG (<MODE>mode, ACC_A);

    avr_fix_inputs (operands, 1 << 2, regmask (<MODE>mode, ACC_A));
    emit_move_insn (acc_a, operands[1]);

    if (const_operand (operands[2], GET_MODE (operands[2])))
      {
        emit_insn (gen_sub<mode>3_const_insn (operands[2]));
      }
    else
     {
       emit_move_insn (gen_rtx_REG (<MODE>mode, ACC_B), operands[2]);
       emit_insn (gen_sub<mode>3_insn ());
     }

    emit_move_insn (operands[0], acc_a);
    DONE;
  })

;; "subdi3_insn"
;; "subdq3_insn" "subudq3_insn"
;; "subda3_insn" "subuda3_insn"
;; "subta3_insn" "subuta3_insn"
(define_insn "sub<mode>3_insn"
  [(set (reg:ALL8 ACC_A)
        (minus:ALL8 (reg:ALL8 ACC_A)
                    (reg:ALL8 ACC_B)))]
  "avr_have_dimode"
  "%~call __subdi3"
  [(set_attr "adjust_len" "call")
   (set_attr "cc" "set_czn")])

;; "subdi3_const_insn"
;; "subdq3_const_insn" "subudq3_const_insn"
;; "subda3_const_insn" "subuda3_const_insn"
;; "subta3_const_insn" "subuta3_const_insn"
(define_insn "sub<mode>3_const_insn"
  [(set (reg:ALL8 ACC_A)
        (minus:ALL8 (reg:ALL8 ACC_A)
                    (match_operand:ALL8 0 "const_operand" "n Ynn")))]
  "avr_have_dimode"
  {
    return avr_out_plus (insn, operands);
  }
  [(set_attr "adjust_len" "plus")
   (set_attr "cc" "clobber")])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Signed Saturating Addition and Subtraction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_expand "<code_stdname><mode>3"
  [(set (match_operand:ALL8S 0 "general_operand" "")
        (ss_addsub:ALL8S (match_operand:ALL8S 1 "general_operand" "")
                         (match_operand:ALL8S 2 "general_operand" "")))]
  "avr_have_dimode"
  {
    rtx acc_a = gen_rtx_REG (<MODE>mode, ACC_A);

    avr_fix_inputs (operands, 1 << 2, regmask (<MODE>mode, ACC_A));
    emit_move_insn (acc_a, operands[1]);

    if (const_operand (operands[2], GET_MODE (operands[2])))
      {
        emit_insn (gen_<code_stdname><mode>3_const_insn (operands[2]));
      }
    else
      {
        emit_move_insn (gen_rtx_REG (<MODE>mode, ACC_B), operands[2]);
        emit_insn (gen_<code_stdname><mode>3_insn ());
      }

    emit_move_insn (operands[0], acc_a);
    DONE;
  })

(define_insn "<code_stdname><mode>3_insn"
  [(set (reg:ALL8S ACC_A)
        (ss_addsub:ALL8S (reg:ALL8S ACC_A)
                         (reg:ALL8S ACC_B)))]
  "avr_have_dimode"
  "%~call __<code_stdname><mode>3"
  [(set_attr "adjust_len" "call")
   (set_attr "cc" "clobber")])

(define_insn "<code_stdname><mode>3_const_insn"
  [(set (reg:ALL8S ACC_A)
        (ss_addsub:ALL8S (reg:ALL8S ACC_A)
                         (match_operand:ALL8S 0 "const_operand" "n Ynn")))]
  "avr_have_dimode"
  {
    return avr_out_plus (insn, operands);
  }
  [(set_attr "adjust_len" "plus")
   (set_attr "cc" "clobber")])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unsigned Saturating Addition and Subtraction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_expand "<code_stdname><mode>3"
  [(set (match_operand:ALL8U 0 "general_operand" "")
        (us_addsub:ALL8U (match_operand:ALL8U 1 "general_operand" "")
                         (match_operand:ALL8U 2 "general_operand" "")))]
  "avr_have_dimode"
  {
    rtx acc_a = gen_rtx_REG (<MODE>mode, ACC_A);

    avr_fix_inputs (operands, 1 << 2, regmask (<MODE>mode, ACC_A));
    emit_move_insn (acc_a, operands[1]);

    if (const_operand (operands[2], GET_MODE (operands[2])))
      {
        emit_insn (gen_<code_stdname><mode>3_const_insn (operands[2]));
      }
    else
      {
        emit_move_insn (gen_rtx_REG (<MODE>mode, ACC_B), operands[2]);
        emit_insn (gen_<code_stdname><mode>3_insn ());
      }

    emit_move_insn (operands[0], acc_a);
    DONE;
  })

(define_insn "<code_stdname><mode>3_insn"
  [(set (reg:ALL8U ACC_A)
        (us_addsub:ALL8U (reg:ALL8U ACC_A)
                         (reg:ALL8U ACC_B)))]
  "avr_have_dimode"
  "%~call __<code_stdname><mode>3"
  [(set_attr "adjust_len" "call")
   (set_attr "cc" "clobber")])

(define_insn "<code_stdname><mode>3_const_insn"
  [(set (reg:ALL8U ACC_A)
        (us_addsub:ALL8U (reg:ALL8U ACC_A)
                         (match_operand:ALL8U 0 "const_operand" "n Ynn")))]
  "avr_have_dimode"
  {
    return avr_out_plus (insn, operands);
  }
  [(set_attr "adjust_len" "plus")
   (set_attr "cc" "clobber")])

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

;; "cbranchdi4"
;; "cbranchdq4" "cbranchudq4"
;; "cbranchda4" "cbranchuda4"
;; "cbranchta4" "cbranchuta4"
(define_expand "cbranch<mode>4"
  [(parallel [(match_operand:ALL8 1 "register_operand" "")
              (match_operand:ALL8 2 "nonmemory_operand" "")
              (match_operator 0 "ordered_comparison_operator" [(cc0)
                                                               (const_int 0)])
              (label_ref (match_operand 3 "" ""))])]
  "avr_have_dimode"
  {
    rtx acc_a = gen_rtx_REG (<MODE>mode, ACC_A);

    avr_fix_inputs (operands, 1 << 2, regmask (<MODE>mode, ACC_A));
    emit_move_insn (acc_a, operands[1]);

    if (s8_operand (operands[2], VOIDmode))
      {
        emit_move_insn (gen_rtx_REG (QImode, REG_X), operands[2]);
        emit_insn (gen_compare_const8_di2 ());
      }
    else if (const_operand (operands[2], GET_MODE (operands[2])))
      {
        emit_insn (gen_compare_const_<mode>2 (operands[2]));
      }
    else
      {
        emit_move_insn (gen_rtx_REG (<MODE>mode, ACC_B), operands[2]);
        emit_insn (gen_compare_<mode>2 ());
      }

    emit_jump_insn (gen_conditional_jump (operands[0], operands[3]));
    DONE;
  })

;; "compare_di2"
;; "compare_dq2" "compare_udq2"
;; "compare_da2" "compare_uda2"
;; "compare_ta2" "compare_uta2"
(define_insn "compare_<mode>2"
  [(set (cc0)
        (compare (reg:ALL8 ACC_A)
                 (reg:ALL8 ACC_B)))]
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

;; "compare_const_di2"
;; "compare_const_dq2" "compare_const_udq2"
;; "compare_const_da2" "compare_const_uda2"
;; "compare_const_ta2" "compare_const_uta2"
(define_insn "compare_const_<mode>2"
  [(set (cc0)
        (compare (reg:ALL8 ACC_A)
                 (match_operand:ALL8 0 "const_operand" "n Ynn")))
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

;; "ashldi3"   "ashrdi3"   "lshrdi3"   "rotldi3"
;; "ashldq3"   "ashrdq3"   "lshrdq3"   "rotldq3"
;; "ashlda3"   "ashrda3"   "lshrda3"   "rotlda3"
;; "ashlta3"   "ashrta3"   "lshrta3"   "rotlta3"
;; "ashludq3"  "ashrudq3"  "lshrudq3"  "rotludq3"
;; "ashluda3"  "ashruda3"  "lshruda3"  "rotluda3"
;; "ashluta3"  "ashruta3"  "lshruta3"  "rotluta3"
(define_expand "<code_stdname><mode>3"
  [(parallel [(match_operand:ALL8 0 "general_operand" "")
              (di_shifts:ALL8 (match_operand:ALL8 1 "general_operand" "")
                              (match_operand:QI 2 "general_operand" ""))])]
  "avr_have_dimode"
  {
    rtx acc_a = gen_rtx_REG (<MODE>mode, ACC_A);

    avr_fix_inputs (operands, 1 << 2, regmask (<MODE>mode, ACC_A));
    emit_move_insn (acc_a, operands[1]);
    emit_move_insn (gen_rtx_REG (QImode, 16), operands[2]);
    emit_insn (gen_<code_stdname><mode>3_insn ());
    emit_move_insn (operands[0], acc_a);
    DONE;
  })

;; "ashldi3_insn"   "ashrdi3_insn"   "lshrdi3_insn"   "rotldi3_insn"
;; "ashldq3_insn"   "ashrdq3_insn"   "lshrdq3_insn"   "rotldq3_insn"
;; "ashlda3_insn"   "ashrda3_insn"   "lshrda3_insn"   "rotlda3_insn"
;; "ashlta3_insn"   "ashrta3_insn"   "lshrta3_insn"   "rotlta3_insn"
;; "ashludq3_insn"  "ashrudq3_insn"  "lshrudq3_insn"  "rotludq3_insn"
;; "ashluda3_insn"  "ashruda3_insn"  "lshruda3_insn"  "rotluda3_insn"
;; "ashluta3_insn"  "ashruta3_insn"  "lshruta3_insn"  "rotluta3_insn"
(define_insn "<code_stdname><mode>3_insn"
  [(set (reg:ALL8 ACC_A)
        (di_shifts:ALL8 (reg:ALL8 ACC_A)
                        (reg:QI 16)))]
  "avr_have_dimode"
  "%~call __<code_stdname>di3"
  [(set_attr "adjust_len" "call")
   (set_attr "cc" "clobber")])

;; "umulsidi3"
;; "mulsidi3"
(define_expand "<extend_u>mulsidi3"
  [(parallel [(match_operand:DI 0 "register_operand" "")
              (match_operand:SI 1 "general_operand" "")
              (match_operand:SI 2 "general_operand" "")
              ;; Just to mention the iterator 
              (clobber (any_extend:SI (match_dup 1)))])]
  "avr_have_dimode
   && AVR_HAVE_MUL"
  {
    avr_fix_inputs (operands, 1 << 2, regmask (SImode, 22));
    emit_move_insn (gen_rtx_REG (SImode, 22), operands[1]);
    emit_move_insn (gen_rtx_REG (SImode, 18), operands[2]);
    emit_insn (gen_<extend_u>mulsidi3_insn());
    // Use emit_move_insn and not open-coded expand because of missing movdi
    emit_move_insn (operands[0], gen_rtx_REG (DImode, ACC_A));
    DONE;
  })

;; "umulsidi3_insn"
;; "mulsidi3_insn"
(define_insn "<extend_u>mulsidi3_insn"
  [(set (reg:DI ACC_A)
        (mult:DI (any_extend:DI (reg:SI 18))
                 (any_extend:DI (reg:SI 22))))
   (clobber (reg:HI REG_X))
   (clobber (reg:HI REG_Z))]
  "avr_have_dimode
   && AVR_HAVE_MUL"
  "%~call __<extend_u>mulsidi3"
  [(set_attr "adjust_len" "call")
   (set_attr "cc" "clobber")])
