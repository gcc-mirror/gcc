;;   This file contains instructions that support fixed-point operations
;;   for Atmel AVR micro controllers.
;;   Copyright (C) 2012
;;   Free Software Foundation, Inc.
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

(define_mode_iterator ALL1Q [(QQ "") (UQQ "")])
(define_mode_iterator ALL2Q [(HQ "") (UHQ "")])
(define_mode_iterator ALL2A [(HA "") (UHA "")])
(define_mode_iterator ALL2QA [(HQ "") (UHQ "")
                              (HA "") (UHA "")])
(define_mode_iterator ALL4A [(SA "") (USA "")])

;;; Conversions

(define_mode_iterator FIXED_A
  [(QQ "") (UQQ "")
   (HQ "") (UHQ "") (HA "") (UHA "")
   (SQ "") (USQ "") (SA "") (USA "")
   (DQ "") (UDQ "") (DA "") (UDA "")
   (TA "") (UTA "")
   (QI "") (HI "") (SI "") (DI "")])

;; Same so that be can build cross products

(define_mode_iterator FIXED_B
  [(QQ "") (UQQ "")
   (HQ "") (UHQ "") (HA "") (UHA "")
   (SQ "") (USQ "") (SA "") (USA "")
   (DQ "") (UDQ "") (DA "") (UDA "")
   (TA "") (UTA "")
   (QI "") (HI "") (SI "") (DI "")])

(define_insn "fract<FIXED_B:mode><FIXED_A:mode>2"
  [(set (match_operand:FIXED_A 0 "register_operand" "=r")
        (fract_convert:FIXED_A
         (match_operand:FIXED_B 1 "register_operand" "r")))]
  "<FIXED_B:MODE>mode != <FIXED_A:MODE>mode"
  {
    return avr_out_fract (insn, operands, true, NULL);
  }
  [(set_attr "cc" "clobber")
   (set_attr "adjust_len" "sfract")])

(define_insn "fractuns<FIXED_B:mode><FIXED_A:mode>2"
  [(set (match_operand:FIXED_A 0 "register_operand" "=r")
        (unsigned_fract_convert:FIXED_A
         (match_operand:FIXED_B 1 "register_operand" "r")))]
  "<FIXED_B:MODE>mode != <FIXED_A:MODE>mode"
  {
    return avr_out_fract (insn, operands, false, NULL);
  }
  [(set_attr "cc" "clobber")
   (set_attr "adjust_len" "ufract")])

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

(define_insn "mulqq3_enh"
  [(set (match_operand:QQ 0 "register_operand"         "=r")
        (mult:QQ (match_operand:QQ 1 "register_operand" "a")
                 (match_operand:QQ 2 "register_operand" "a")))]
  "AVR_HAVE_MUL"
  "fmuls %1,%2\;dec r1\;brvs 0f\;inc r1\;0:\;mov %0,r1\;clr __zero_reg__"
  [(set_attr "length" "6")
   (set_attr "cc" "clobber")])

(define_insn "muluqq3_enh"
  [(set (match_operand:UQQ 0 "register_operand"          "=r")
        (mult:UQQ (match_operand:UQQ 1 "register_operand" "r")
                  (match_operand:UQQ 2 "register_operand" "r")))]
  "AVR_HAVE_MUL"
  "mul %1,%2\;mov %0,r1\;clr __zero_reg__"
  [(set_attr "length" "3")
   (set_attr "cc" "clobber")])

(define_expand "mulqq3_nomul"
  [(set (reg:QQ 24)
        (match_operand:QQ 1 "register_operand" ""))
   (set (reg:QQ 25)
        (match_operand:QQ 2 "register_operand" ""))
   ;; "*mulqq3.call"
   (parallel [(set (reg:QQ 23)
                   (mult:QQ (reg:QQ 24)
                            (reg:QQ 25)))
              (clobber (reg:QI 22))
              (clobber (reg:HI 24))])
   (set (match_operand:QQ 0 "register_operand" "")
        (reg:QQ 23))]
  "!AVR_HAVE_MUL")

(define_expand "muluqq3_nomul"
  [(set (reg:UQQ 22)
        (match_operand:UQQ 1 "register_operand" ""))
   (set (reg:UQQ 24)
        (match_operand:UQQ 2 "register_operand" ""))
   ;; "*umulqihi3.call"
   (parallel [(set (reg:HI 24)
                   (mult:HI (zero_extend:HI (reg:QI 22))
                            (zero_extend:HI (reg:QI 24))))
              (clobber (reg:QI 21))
              (clobber (reg:HI 22))])
   (set (match_operand:UQQ 0 "register_operand" "")
        (reg:UQQ 25))]
  "!AVR_HAVE_MUL")

(define_insn "*mulqq3.call"
  [(set (reg:QQ 23)
        (mult:QQ (reg:QQ 24)
                 (reg:QQ 25)))
   (clobber (reg:QI 22))
   (clobber (reg:HI 24))]
  "!AVR_HAVE_MUL"
  "%~call __mulqq3"
  [(set_attr "type" "xcall")
   (set_attr "cc" "clobber")])


;; "mulhq3" "muluhq3"
;; "mulha3" "muluha3"
(define_expand "mul<mode>3"
  [(set (reg:ALL2QA 18)
        (match_operand:ALL2QA 1 "register_operand" ""))
   (set (reg:ALL2QA 26)
        (match_operand:ALL2QA 2 "register_operand" ""))
   ;; "*mulhq3.call.enh"
   (parallel [(set (reg:ALL2QA 24)
                   (mult:ALL2QA (reg:ALL2QA 18)
                                (reg:ALL2QA 26)))
              (clobber (reg:HI 22))])
   (set (match_operand:ALL2QA 0 "register_operand" "")
        (reg:ALL2QA 24))]
  "AVR_HAVE_MUL")

;; "*mulhq3.call"  "*muluhq3.call"
;; "*mulha3.call"  "*muluha3.call"
(define_insn "*mul<mode>3.call"
  [(set (reg:ALL2QA 24)
        (mult:ALL2QA (reg:ALL2QA 18)
                     (reg:ALL2QA 26)))
   (clobber (reg:HI 22))]
  "AVR_HAVE_MUL"
  "%~call __mul<mode>3"
  [(set_attr "type" "xcall")
   (set_attr "cc" "clobber")])


;; On the enhanced core, don't clobber either input and use a separate output

;; "mulsa3" "mulusa3"
(define_expand "mul<mode>3"
  [(set (reg:ALL4A 16)
        (match_operand:ALL4A 1 "register_operand" ""))
   (set (reg:ALL4A 20)
        (match_operand:ALL4A 2 "register_operand" ""))
   (set (reg:ALL4A 24)
        (mult:ALL4A (reg:ALL4A 16)
                    (reg:ALL4A 20)))
   (set (match_operand:ALL4A 0 "register_operand" "")
        (reg:ALL4A 24))]
  "AVR_HAVE_MUL")

;; "*mulsa3.call" "*mulusa3.call"
(define_insn "*mul<mode>3.call"
  [(set (reg:ALL4A 24)
        (mult:ALL4A (reg:ALL4A 16)
                    (reg:ALL4A 20)))]
  "AVR_HAVE_MUL"
  "%~call __mul<mode>3"
  [(set_attr "type" "xcall")
   (set_attr "cc" "clobber")])

; / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
; div

(define_code_iterator usdiv [udiv div])

;; "divqq3" "udivuqq3"
(define_expand "<code><mode>3"
  [(set (reg:ALL1Q 25)
        (match_operand:ALL1Q 1 "register_operand" ""))
   (set (reg:ALL1Q 22)
        (match_operand:ALL1Q 2 "register_operand" ""))
   (parallel [(set (reg:ALL1Q 24)
                   (usdiv:ALL1Q (reg:ALL1Q 25)
                                (reg:ALL1Q 22)))
              (clobber (reg:QI 25))])
   (set (match_operand:ALL1Q 0 "register_operand" "")
        (reg:ALL1Q 24))])

;; "*divqq3.call" "*udivuqq3.call"
(define_insn "*<code><mode>3.call"
  [(set (reg:ALL1Q 24)
        (usdiv:ALL1Q (reg:ALL1Q 25)
                     (reg:ALL1Q 22)))
   (clobber (reg:QI 25))]
  ""
  "%~call __<code><mode>3"
  [(set_attr "type" "xcall")
   (set_attr "cc" "clobber")])

;; "divhq3" "udivuhq3"
;; "divha3" "udivuha3"
(define_expand "<code><mode>3"
  [(set (reg:ALL2QA 26)
        (match_operand:ALL2QA 1 "register_operand" ""))
   (set (reg:ALL2QA 22)
        (match_operand:ALL2QA 2 "register_operand" ""))
   (parallel [(set (reg:ALL2QA 24)
                   (usdiv:ALL2QA (reg:ALL2QA 26)
                                 (reg:ALL2QA 22)))
              (clobber (reg:HI 26))
              (clobber (reg:QI 21))])
   (set (match_operand:ALL2QA 0 "register_operand" "")
        (reg:ALL2QA 24))])

;; "*divhq3.call" "*udivuhq3.call"
;; "*divha3.call" "*udivuha3.call"
(define_insn "*<code><mode>3.call"
  [(set (reg:ALL2QA 24)
        (usdiv:ALL2QA (reg:ALL2QA 26)
                      (reg:ALL2QA 22)))
   (clobber (reg:HI 26))
   (clobber (reg:QI 21))]
  ""
  "%~call __<code><mode>3"
  [(set_attr "type" "xcall")
   (set_attr "cc" "clobber")])

;; Note the first parameter gets passed in already offset by 2 bytes

;; "divsa3" "udivusa3"
(define_expand "<code><mode>3"
  [(set (reg:ALL4A 24)
        (match_operand:ALL4A 1 "register_operand" ""))
   (set (reg:ALL4A 18)
        (match_operand:ALL4A 2 "register_operand" ""))
   (parallel [(set (reg:ALL4A 22)
                   (usdiv:ALL4A (reg:ALL4A 24)
                                (reg:ALL4A 18)))
              (clobber (reg:HI 26))
              (clobber (reg:HI 30))])
   (set (match_operand:ALL4A 0 "register_operand" "")
        (reg:ALL4A 22))])

;; "*divsa3.call" "*udivusa3.call"
(define_insn "*<code><mode>3.call"
  [(set (reg:ALL4A 22)
        (usdiv:ALL4A (reg:ALL4A 24)
                     (reg:ALL4A 18)))
   (clobber (reg:HI 26))
   (clobber (reg:HI 30))]
  ""
  "%~call __<code><mode>3"
  [(set_attr "type" "xcall")
   (set_attr "cc" "clobber")])
