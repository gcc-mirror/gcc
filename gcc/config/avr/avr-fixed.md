;;   Support fixed-point operations for AVR 8-bit microcontrollers.
;;   Copyright (C) 2012-2026 Free Software Foundation, Inc.
;;
;;   Contributed by Sean D'Epagnier  (sean@depagnier.com)
;;                  Georg-Johann Lay (avr@gjlay.de)

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

(define_mode_iterator ALL1Q  [QQ UQQ])
(define_mode_iterator ALL2Q  [HQ UHQ])
(define_mode_iterator ALL2A  [HA UHA])
(define_mode_iterator ALL4A  [SA USA])
(define_mode_iterator ALL2QA [HQ UHQ HA UHA])
(define_mode_iterator ALL4QA [SQ USQ SA USA])
(define_mode_iterator ALL12QA [ QQ   HQ  HA
                               UQQ  UHQ UHA])
(define_mode_iterator ALL124QA [ QQ   HQ  HA  SA  SQ
                                UQQ  UHQ UHA USA USQ])

(define_mode_iterator ALL2S [HQ HA])
(define_mode_iterator ALL4S [SA SQ])
(define_mode_iterator ALL24S  [     HQ  HA  SA  SQ])
(define_mode_iterator ALL124S [ QQ  HQ  HA  SA  SQ])
(define_mode_iterator ALL124U [UQQ UHQ UHA USA USQ])

;;; Conversions

(define_mode_iterator FIXED_A
  [QQ UQQ
   HQ UHQ HA UHA
   SQ USQ SA USA
   DQ UDQ DA UDA
   TA UTA
   QI HI PSI SI DI])

;; Same so that we can build cartesian products.

(define_mode_iterator FIXED_B
  [QQ UQQ
   HQ UHQ HA UHA
   SQ USQ SA USA
   DQ UDQ DA UDA
   TA UTA
   QI HI PSI SI DI])

(define_insn_and_split "fract<FIXED_B:mode><FIXED_A:mode>2"
  [(set (match_operand:FIXED_A 0 "register_operand" "=r")
        (fract_convert:FIXED_A
         (match_operand:FIXED_B 1 "register_operand" "r")))]
  "<FIXED_B:MODE>mode != <FIXED_A:MODE>mode"
  "#"
  "&& reload_completed"
  [(scratch)]
  { DONE_ADD_CCC })

(define_insn "*fract<FIXED_B:mode><FIXED_A:mode>2"
  [(set (match_operand:FIXED_A 0 "register_operand" "=r")
        (fract_convert:FIXED_A
         (match_operand:FIXED_B 1 "register_operand" "r")))
   (clobber (reg:CC REG_CC))]
  "<FIXED_B:MODE>mode != <FIXED_A:MODE>mode
   && reload_completed"
  {
    return avr_out_fract (insn, operands, true, NULL);
  }
  [(set_attr "adjust_len" "sfract")])

(define_insn_and_split "fractuns<FIXED_B:mode><FIXED_A:mode>2"
  [(set (match_operand:FIXED_A 0 "register_operand" "=r")
        (unsigned_fract_convert:FIXED_A
         (match_operand:FIXED_B 1 "register_operand" "r")))]
  "<FIXED_B:MODE>mode != <FIXED_A:MODE>mode"
  "#"
  "&& reload_completed"
  [(scratch)]
  { DONE_ADD_CCC })

(define_insn "*fractuns<FIXED_B:mode><FIXED_A:mode>2"
  [(set (match_operand:FIXED_A 0 "register_operand" "=r")
        (unsigned_fract_convert:FIXED_A
         (match_operand:FIXED_B 1 "register_operand" "r")))
   (clobber (reg:CC REG_CC))]
  "<FIXED_B:MODE>mode != <FIXED_A:MODE>mode
   && reload_completed"
  {
    return avr_out_fract (insn, operands, false, NULL);
  }
  [(set_attr "adjust_len" "ufract")])

;******************************************************************************
;** Saturated Addition and Subtraction
;******************************************************************************

;; Fixme:  It would be nice if we could expand the 32-bit versions to a
;;    transparent libgcc call if $2 is a REG.  Problem is that it is
;;    not possible to describe that addition is commutative.
;;    And defining register classes/constraints for the involved hard
;;    registers and let IRA do the work, yields inacceptable bloated code.
;;    Thus, we have to live with the up to 11 instructions that are output
;;    for these 32-bit saturated operations.

;; "ssaddqq3"  "ssaddhq3"  "ssaddha3"  "ssaddsq3"  "ssaddsa3"
;; "sssubqq3"  "sssubhq3"  "sssubha3"  "sssubsq3"  "sssubsa3"
(define_insn_and_split "<code_stdname><mode>3"
  [(set (match_operand:ALL124S 0 "register_operand"                          "=??d,d")
        (ss_addsub:ALL124S (match_operand:ALL124S 1 "register_operand" "<abelian>0,0")
                           (match_operand:ALL124S 2 "nonmemory_operand"         "r,Ynn")))]
  ""
  "#"
  "&& reload_completed"
  [(scratch)]
  { DONE_ADD_CCC })

(define_insn "*<code_stdname><mode>3"
  [(set (match_operand:ALL124S 0 "register_operand"                          "=??d,d")
        (ss_addsub:ALL124S (match_operand:ALL124S 1 "register_operand" "<abelian>0,0")
                           (match_operand:ALL124S 2 "nonmemory_operand"         "r,Ynn")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return avr_out_plus (insn, operands);
  }
  [(set_attr "adjust_len" "plus")])

;; "usadduqq3"  "usadduhq3"  "usadduha3" "usaddusq3"  "usaddusa3"
;; "ussubuqq3"  "ussubuhq3"  "ussubuha3" "ussubusq3"  "ussubusa3"
(define_insn_and_split "<code_stdname><mode>3"
  [(set (match_operand:ALL124U 0 "register_operand"                          "=??r,d")
        (us_addsub:ALL124U (match_operand:ALL124U 1 "register_operand" "<abelian>0,0")
                           (match_operand:ALL124U 2 "nonmemory_operand"         "r,Ynn")))]
  ""
  "#"
  "&& reload_completed"
  [(scratch)]
  { DONE_ADD_CCC })

(define_insn "*<code_stdname><mode>3"
  [(set (match_operand:ALL124U 0 "register_operand"                          "=??r,d")
        (us_addsub:ALL124U (match_operand:ALL124U 1 "register_operand" "<abelian>0,0")
                           (match_operand:ALL124U 2 "nonmemory_operand"         "r,Ynn")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return avr_out_plus (insn, operands);
  }
  [(set_attr "adjust_len" "plus")])

;******************************************************************************
;** Saturated Negation and Absolute Value
;******************************************************************************

;; Fixme: This will always result in 0.  Dunno why simplify-rtx.cc says
;;   "unknown" on how to optimize this.  libgcc call would be in order,
;;   but the performance is *PLAIN* *HORROR* because the optimizers don't
;;   manage to optimize out MEMCPY that's sprinkled all over fixed-bit.c  */

(define_expand "usneg<mode>2"
  [(parallel [(match_operand:ALL124U 0 "register_operand" "")
              (match_operand:ALL124U 1 "nonmemory_operand" "")])]
  ""
  {
    emit_move_insn (operands[0], CONST0_RTX (<MODE>mode));
    DONE;
  })

(define_insn_and_split "ssnegqq2"
  [(set (match_operand:QQ 0 "register_operand"            "=r")
        (ss_neg:QQ (match_operand:QQ 1 "register_operand"  "0")))]
  ""
  "#"
  "&& reload_completed"
  [(scratch)]
  { DONE_ADD_CCC })

(define_insn "*ssnegqq2"
  [(set (match_operand:QQ 0 "register_operand"            "=r")
        (ss_neg:QQ (match_operand:QQ 1 "register_operand"  "0")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "neg %0\;brvc 0f\;dec %0\;0:"
  [(set_attr "length" "3")])

(define_insn_and_split "ssabsqq2"
  [(set (match_operand:QQ 0 "register_operand"            "=r")
        (ss_abs:QQ (match_operand:QQ 1 "register_operand"  "0")))]
  ""
  "#"
  "&& reload_completed"
  [(scratch)]
  { DONE_ADD_CCC })

(define_insn "*ssabsqq2"
  [(set (match_operand:QQ 0 "register_operand"            "=r")
        (ss_abs:QQ (match_operand:QQ 1 "register_operand"  "0")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "sbrc %0,7\;neg %0\;sbrc %0,7\;dec %0"
  [(set_attr "length" "4")])

;; "ssneghq2"  "ssnegha2"
;; "ssabshq2"  "ssabsha2"
(define_insn_and_split "<code_stdname><mode>2"
  [(set (match_operand:ALL2S 0 "register_operand"                  "={r24}")
        (ss_abs_neg:ALL2S (match_operand:ALL2S 1 "register_operand" "{r24}")))]
  ""
  "#"
  "&& reload_completed"
  [(scratch)]
  { DONE_ADD_CCC })

(define_insn "*<code_stdname><mode>2"
  [(set (reg:ALL2S                   REG_24)
        (ss_abs_neg:ALL2S (reg:ALL2S REG_24)))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "%~call __<code_stdname>_2"
  [(set_attr "type" "xcall")])

;; "ssnegsq2"  "ssnegsa2"
;; "ssabssq2"  "ssabssa2"
(define_insn_and_split "<code_stdname><mode>2"
  [(set (match_operand:ALL4S 0 "register_operand"                  "={r22}")
        (ss_abs_neg:ALL4S (match_operand:ALL4S 1 "register_operand" "{r22}")))]
  ""
  "#"
  "&& reload_completed"
  [(scratch)]
  { DONE_ADD_CCC })

(define_insn "*<code_stdname><mode>2"
  [(set (reg:ALL4S                   REG_22)
        (ss_abs_neg:ALL4S (reg:ALL4S REG_22)))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "%~call __<code_stdname>_4"
  [(set_attr "type" "xcall")])

;******************************************************************************
; mul

;; "mulqq3" "muluqq3"
(define_expand "mul<mode>3"
  [(parallel [(match_operand:ALL1Q 0 "register_operand" "")
              (match_operand:ALL1Q 1 "register_operand" "")
              (match_operand:ALL1Q 2 "register_operand" "")])]
  ""
  {
    emit_insn (AVR_HAVE_MUL
      ? gen_mul<mode>3_enh (operands[0], operands[1], operands[2])
      : gen_mul<mode>3_nomul (operands[0], operands[1], operands[2]));
    DONE;
  })

(define_insn_and_split "mulqq3_enh"
  [(set (match_operand:QQ 0 "register_operand"         "=r")
        (mult:QQ (match_operand:QQ 1 "register_operand" "a")
                 (match_operand:QQ 2 "register_operand" "a")))]
  "AVR_HAVE_MUL"
  "#"
  "&& reload_completed"
  [(scratch)]
  { DONE_ADD_CCC })

(define_insn "*mulqq3_enh"
  [(set (match_operand:QQ 0 "register_operand"         "=r")
        (mult:QQ (match_operand:QQ 1 "register_operand" "a")
                 (match_operand:QQ 2 "register_operand" "a")))
   (clobber (reg:CC REG_CC))]
  "AVR_HAVE_MUL && reload_completed"
  "fmuls %1,%2\;dec r1\;brvs 0f\;inc r1\;0:\;mov %0,r1\;clr __zero_reg__"
  [(set_attr "length" "6")])

(define_insn_and_split "muluqq3_enh"
  [(set (match_operand:UQQ 0 "register_operand"          "=r")
        (mult:UQQ (match_operand:UQQ 1 "register_operand" "r")
                  (match_operand:UQQ 2 "register_operand" "r")))]
  "AVR_HAVE_MUL"
  "#"
  "&& reload_completed"
  [(scratch)]
  { DONE_ADD_CCC })

(define_insn "*muluqq3_enh"
  [(set (match_operand:UQQ 0 "register_operand"          "=r")
        (mult:UQQ (match_operand:UQQ 1 "register_operand" "r")
                  (match_operand:UQQ 2 "register_operand" "r")))
   (clobber (reg:CC REG_CC))]
  "AVR_HAVE_MUL && reload_completed"
  "mul %1,%2\;mov %0,r1\;clr __zero_reg__"
  [(set_attr "length" "3")])

(define_insn_and_split "mulqq3_nomul"
  [;; "*mulqq3.call"
   (set (match_operand:QQ 0 "register_operand"          "={r23}")
        (mult:QQ (match_operand:QQ 1 "register_operand" "%{r24}")
                 (match_operand:QQ 2 "register_operand"  "{r25}")))
   (clobber (match_scratch:QI 3                         "={r22}"))
   (clobber (match_scratch:HI 4                         "={r24}"))]
  "!AVR_HAVE_MUL"
  "#"
  "&& reload_completed"
  [(scratch)]
  { DONE_ADD_CCC })


(define_expand "muluqq3_nomul"
  [(set (reg:UQQ REG_22)
        (match_operand:UQQ 1 "register_operand" ""))
   (set (reg:UQQ REG_24)
        (match_operand:UQQ 2 "register_operand" ""))
   ;; "*umulqihi3.call_split"
   (parallel [(set (reg:HI REG_24)
                   (mult:HI (zero_extend:HI (reg:QI REG_22))
                            (zero_extend:HI (reg:QI REG_24))))
              (clobber (reg:QI REG_21))
              (clobber (reg:HI REG_22))])
   (set (match_operand:UQQ 0 "register_operand" "")
        (reg:UQQ REG_25))]
  "!AVR_HAVE_MUL"
  {
    avr_fix_inputs (operands, 1 << 2, regmask (UQQmode, REG_22));
  })

(define_insn "*mulqq3.call"
  [(set (reg:QQ          REG_23)
        (mult:QQ (reg:QQ REG_24)
                 (reg:QQ REG_25)))
   (clobber (reg:QI REG_22))
   (clobber (reg:HI REG_24))
   (clobber (reg:CC REG_CC))]
  "!AVR_HAVE_MUL && reload_completed"
  "%~call __mulqq3"
  [(set_attr "type" "xcall")])


;; "mulhq3.call"  "muluhq3.call"
;; "mulha3.call"  "muluha3.call"
(define_insn_and_split "mul<mode>3"
  [(set (match_operand:ALL2QA 0 "register_operand"              "={r24}")
        (mult:ALL2QA (match_operand:ALL2QA 1 "register_operand" "%{r18}")
                     (match_operand:ALL2QA 2 "register_operand"  "{r26}")))
   (clobber (match_scratch:HI 3                                 "={r22}"))]
  "AVR_HAVE_MUL"
  "#"
  "&& reload_completed"
  [(scratch)]
  { DONE_ADD_CCC })

(define_insn "*mul<mode>3.call"
  [(set (reg:ALL2QA              REG_24)
        (mult:ALL2QA (reg:ALL2QA REG_18)
                     (reg:ALL2QA REG_26)))
   (clobber (reg:HI REG_22))
   (clobber (reg:CC REG_CC))]
  "AVR_HAVE_MUL && reload_completed"
  "%~call __mul<mode>3"
  [(set_attr "type" "xcall")])


;; On the enhanced core, don't clobber either input and use a separate output.

;; "mulsa3" "mulusa3"
(define_insn_and_split "mul<mode>3"
  [(set (match_operand:ALL4A 0 "register_operand"             "={r24}")
        (mult:ALL4A (match_operand:ALL4A 1 "register_operand" "%{r16}")
                    (match_operand:ALL4A 2 "register_operand"  "{r20}")))]
  "AVR_HAVE_MUL"
  "#"
  "&& reload_completed"
  [(scratch)]
  { DONE_ADD_CCC })

(define_insn "*mul<mode>3.call"
  [(set (reg:ALL4A             REG_24)
        (mult:ALL4A (reg:ALL4A REG_16)
                    (reg:ALL4A REG_20)))
   (clobber (reg:CC REG_CC))]
  "AVR_HAVE_MUL && reload_completed"
  "%~call __mul<mode>3"
  [(set_attr "type" "xcall")])

; / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
; div

(define_code_iterator usdiv  [udiv div])
(define_code_iterator alldiv [udiv div us_div ss_div])

;; "divqq3" "udivuqq3"
(define_insn_and_split "<code><mode>3"
  [(set (match_operand:ALL1Q 0 "register_operand"              "={r24}")
        (usdiv:ALL1Q (match_operand:ALL1Q 1 "register_operand"  "{r25}")
                     (match_operand:ALL1Q 2 "register_operand"  "{r22}")))
   (clobber (match_scratch:QI 3                                "={r25}"))]
  "SIGNED_FIXED_POINT_MODE_P (<MODE>mode)
   == (<CODE> == DIV || <CODE> == SS_DIV)"
  "#"
  "&& reload_completed"
  [(scratch)]
  { DONE_ADD_CCC })

(define_insn "*<code><mode>3.call"
  [(set (reg:ALL1Q              REG_24)
        (usdiv:ALL1Q (reg:ALL1Q REG_25)
                     (reg:ALL1Q REG_22)))
   (clobber (reg:QI REG_25))
   (clobber (reg:CC REG_CC))]
  "reload_completed
   && SIGNED_FIXED_POINT_MODE_P (<MODE>mode)
      == (<CODE> == DIV || <CODE> == SS_DIV)"
  "%~call __<code><mode>3"
  [(set_attr "type" "xcall")])

;; "divhq3" "udivuhq3" "ssdivhq3" "usdivuhq3"
;; "divha3" "udivuha3" "ssdivha3" "usdivuha3"
(define_insn_and_split "<code_stdname><mode>3"
  [(set (match_operand:ALL2QA 0 "register_operand"               "={r24}")
        (alldiv:ALL2QA (match_operand:ALL2QA 1 "register_operand" "{r26}")
                       (match_operand:ALL2QA 2 "register_operand" "{r22}")))
   (clobber (match_scratch:HI 3                                  "={r26}"))
   (clobber (match_scratch:QI 4                                  "={r21}"))]
  "SIGNED_FIXED_POINT_MODE_P (<MODE>mode)
   == (<CODE> == DIV || <CODE> == SS_DIV)"
  "#"
  "&& reload_completed"
  [(scratch)]
  { DONE_ADD_CCC })

;; "*divhq3.call" "*udivuhq3.call" "*ssdivhq3.call" "*usdivuhq3.call"
;; "*divha3.call" "*udivuha3.call" "*ssdivha3.call" "*usdivuha3.call"
(define_insn "*<code_stdname><mode>3.call"
  [(set (reg:ALL2QA                REG_24)
        (alldiv:ALL2QA (reg:ALL2QA REG_26)
                       (reg:ALL2QA REG_22)))
   (clobber (reg:HI REG_26))
   (clobber (reg:QI REG_21))
   (clobber (reg:CC REG_CC))]
  "reload_completed
   && SIGNED_FIXED_POINT_MODE_P (<MODE>mode)
      == (<CODE> == DIV || <CODE> == SS_DIV)"
  "%~call __<code_stdname><mode>3"
  [(set_attr "type" "xcall")])

;; Note the first parameter gets passed in already offset by 2 bytes

;; "divsa3" "udivusa3"
;; "ssdivsa3" "usdivusa3"
(define_insn_and_split "<code_stdname><mode>3"
  [(set (match_operand:ALL4A 0 "register_operand"              "={r22}")
        (alldiv:ALL4A (match_operand:ALL4A 1 "register_operand" "{r24}")
                      (match_operand:ALL4A 2 "register_operand" "{r18}")))
   (clobber (match_scratch:HI 3                                "={r26}"))
   (clobber (match_scratch:HI 4                                "={r30}"))]
  "SIGNED_FIXED_POINT_MODE_P (<MODE>mode)
   == (<CODE> == DIV || <CODE> == SS_DIV)"
  "#"
  "&& reload_completed"
  [(scratch)]
  { DONE_ADD_CCC })

;; "*divsa3.call" "*udivusa3.call" "*ssdivsa3.call" "*usdivusa3.call"
(define_insn "*<code_stdname><mode>3.call"
  [(set (reg:ALL4A               REG_22)
        (alldiv:ALL4A (reg:ALL4A REG_24)
                      (reg:ALL4A REG_18)))
   (clobber (reg:HI REG_26))
   (clobber (reg:HI REG_30))
   (clobber (reg:CC REG_CC))]
  "reload_completed
   && SIGNED_FIXED_POINT_MODE_P (<MODE>mode)
      == (<CODE> == DIV || <CODE> == SS_DIV)"
  "%~call __<code_stdname><mode>3"
  [(set_attr "type" "xcall")])


;******************************************************************************
;** Rounding
;******************************************************************************

;; "roundqq3"  "rounduqq3"
;; "roundhq3"  "rounduhq3"  "roundha3"  "rounduha3"
;; "roundsq3"  "roundusq3"  "roundsa3"  "roundusa3"
(define_expand "round<mode>3"
  [(parallel [(set (match_operand:ALL124QA 0 "register_operand")
                   (unspec:ALL124QA [(match_operand:ALL124QA 1 "register_operand")
                                     (match_operand:HI 2 "nonmemory_operand")]
                                    UNSPEC_ROUND))
              (clobber (scratch:ALL124QA))])]
  ""
  {
    if (CONST_INT_P (operands[2]))
      {
        emit_insn (gen_round<mode>3_const (operands[0], operands[1], operands[2]));
        DONE;
      }

    operands[2] = avr_byte (force_reg (HImode, operands[2]), 0);
  })

;; Expand rounding with known rounding points inline so that the addend / mask
;; will be consumed by operation with immediate operands, and there is no
;; need for a shift with variable offset.

;; "roundqq3_const"  "rounduqq3_const"
;; "roundhq3_const"  "rounduhq3_const"  "roundha3_const"  "rounduha3_const"
;; "roundsq3_const"  "roundusq3_const"  "roundsa3_const"  "roundusa3_const"
(define_insn_and_split "round<mode>3_const"
  [(set (match_operand:ALL124QA 0 "register_operand"                  "=d")
        (unspec:ALL124QA [(match_operand:ALL124QA 1 "register_operand" "0")
                          (match_operand:HI 2 "const_int_operand"      "n")
                          (const_int 0)]
                         UNSPEC_ROUND))]
  ""
  "#"
  "&& reload_completed"
  [(scratch)]
  { DONE_ADD_CCC })

(define_insn "*round<mode>3_const"
  [(set (match_operand:ALL124QA 0 "register_operand"                  "=d")
        (unspec:ALL124QA [(match_operand:ALL124QA 1 "register_operand" "0")
                          (match_operand:HI 2 "const_int_operand"      "n")
                          (const_int 0)]
                         UNSPEC_ROUND))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return avr_out_round (insn, operands);
  }
  [(set_attr "adjust_len" "round")])


;; "*roundqq3.libgcc_split"  "*rounduqq3.libgcc_split"
;; "*roundhq3.libgcc_split"  "*rounduhq3.libgcc_split"
;; "*roundha3.libgcc_split"  "*rounduha3.libgcc_split"
(define_insn_and_split "*round<mode>3.libgcc_split"
  [(set (match_operand:ALL12QA 0 "register_operand"                 "={r24}")
        (unspec:ALL12QA [(match_operand:ALL12QA 1 "register_operand" "{r22}")
                         (match_operand:QI 2 "register_operand"      "{r24}")]
                        UNSPEC_ROUND))
   (clobber (match_scratch:ALL12QA 3                                "={r22}"))]
  ""
  "#"
  "&& reload_completed"
  [(scratch)]
  { DONE_ADD_CCC })

;; "*roundqq3.libgcc"  "*rounduqq3.libgcc"
;; "*roundhq3.libgcc"  "*rounduhq3.libgcc"
;; "*roundha3.libgcc"  "*rounduha3.libgcc"
(define_insn "*round<mode>3.libgcc"
  [(set (reg:ALL12QA                  REG_24)
        (unspec:ALL12QA [(reg:ALL12QA REG_22)
                         (reg:QI      REG_24)] UNSPEC_ROUND))
   (clobber (reg:ALL12QA REG_22))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "%~call __round<mode>3"
  [(set_attr "type" "xcall")])

;; "*roundsq3.libgcc"  "*roundusq3.libgcc"
;; "*roundsa3.libgcc"  "*roundusa3.libgcc"
(define_insn_and_split "*round<mode>3.libgcc_split"
  [(set (match_operand:ALL4QA 0 "register_operand"                "={r22}")
        (unspec:ALL4QA [(match_operand:ALL4QA 1 "register_operand" "{r18}")
                        (match_operand:QI 2 "register_operand"     "{r24}")]
                       UNSPEC_ROUND))
   (clobber (match_scratch:ALL4QA 3                               "={r18}"))]
  ""
  "#"
  "&& reload_completed"
  [(scratch)]
  { DONE_ADD_CCC })

(define_insn "*round<mode>3.libgcc"
  [(set (reg:ALL4QA                 REG_22)
        (unspec:ALL4QA [(reg:ALL4QA REG_18)
                        (reg:QI     REG_24)] UNSPEC_ROUND))
   (clobber (reg:ALL4QA REG_18))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "%~call __round<mode>3"
  [(set_attr "type" "xcall")])


;******************************************************************************
;** Saturated Shift Left
;******************************************************************************

;; These functions are default ABI but are clobbering less registers.

(define_code_iterator sat_ashl  [us_ashift ss_ashift])

;; "usashluqq3"  "ssashlqq3"
;; "usashluhq3"  "ssashlhq3"
;; "usashluha3"  "ssashlha3"
(define_insn_and_split "<code_stdname><mode>3"
  [(set (match_operand:ALL12QA 0 "register_operand"                  "={r24}")
        (sat_ashl:ALL12QA (match_operand:ALL12QA 1 "register_operand" "{r24}")
                          (match_operand:QI 2 "register_operand"      "{r22}")))
   (clobber (match_scratch:QI 3                                      "={r22}"))]
  "SIGNED_FIXED_POINT_MODE_P (<MODE>mode) == (<CODE> == SS_ASHIFT)"
  "#"
  "&& reload_completed"
  [(scratch)]
  { DONE_ADD_CCC })

;; "*usashluqq3"  "*ssashlqq3"
;; "*usashluhq3"  "*ssashlhq3"
;; "*usashluha3"  "*ssashlha3"
(define_insn "*<code_stdname><mode>3"
  [(set (reg:ALL12QA                   REG_24)
        (sat_ashl:ALL12QA (reg:ALL12QA REG_24)
                          (reg:QI      REG_22)))
   (clobber (reg:QI REG_22))
   (clobber (reg:CC REG_CC))]
  "reload_completed
   && SIGNED_FIXED_POINT_MODE_P (<MODE>mode) == (<CODE> == SS_ASHIFT)"
  "%~call __<code_stdname><mode>3"
  [(set_attr "type" "xcall")])

;; "usashlusq3"  "ssashlsq3"
;; "usashlusa3"  "ssashlsa3"
(define_insn_and_split "<code_stdname><mode>3"
  [(set (match_operand:ALL4QA 0 "register_operand"                 "={r22}")
        (sat_ashl:ALL4QA (match_operand:ALL4QA 1 "register_operand" "{r22}")
                         (match_operand:QI 2 "register_operand"     "{r20}")))
   (clobber (match_scratch:QI 3                                    "={r20}"))]
  "SIGNED_FIXED_POINT_MODE_P (<MODE>mode) == (<CODE> == SS_ASHIFT)"
  "#"
  "&& reload_completed"
  [(scratch)]
  { DONE_ADD_CCC })

;; "*usashlusq3"  "*ssashlsq3"
;; "*usashlusa3"  "*ssashlsa3"
(define_insn "*<code_stdname><mode>3"
  [(set (reg:ALL4QA                  REG_22)
        (sat_ashl:ALL4QA (reg:ALL4QA REG_22)
                         (reg:QI     REG_20)))
   (clobber (reg:QI REG_20))
   (clobber (reg:CC REG_CC))]
  "reload_completed
   && SIGNED_FIXED_POINT_MODE_P (<MODE>mode) == (<CODE> == SS_ASHIFT)"
  "%~call __<code_stdname><mode>3"
  [(set_attr "type" "xcall")])
