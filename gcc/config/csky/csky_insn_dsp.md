;; C-SKY DSP instruction descriptions.
;; Copyright (C) 2018-2020 Free Software Foundation, Inc.
;; Contributed by C-SKY Microsystems and Mentor Graphics.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; GCC is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.  */

;; ------------------------------------------------------------
;; DSP insns
;; ------------------------------------------------------------

(define_insn "mulsidi3"
  [(set (match_operand:DI			   0 "register_operand" "=y")
	(mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand" "r"))
		 (sign_extend:DI (match_operand:SI 2 "register_operand" "r"))))]
  "TARGET_DSP"
  "muls\t%1, %2"
)

(define_insn "umulsidi3"
  [(set (match_operand:DI			   0 "register_operand" "=y")
	(mult:DI (zero_extend:DI (match_operand:SI 1 "register_operand" "r"))
		 (zero_extend:DI (match_operand:SI 2 "register_operand" "r"))))]
  "TARGET_DSP"
  "mulu\t%1, %2"
)

(define_insn "maddsidi4"
  [(set (match_operand:DI				    0 "register_operand" "=y")
	(plus:DI (mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand" "r"))
			  (sign_extend:DI (match_operand:SI 2 "register_operand" "r")))
		 (match_operand:DI			    3 "register_operand" "0")))]
  "TARGET_DSP"
  "mulsa\t%1, %2"
)

(define_insn "umaddsidi4"
  [(set (match_operand:DI				    0 "register_operand" "=y")
	(plus:DI (mult:DI (zero_extend:DI (match_operand:SI 1 "register_operand" "r"))
			  (zero_extend:DI (match_operand:SI 2 "register_operand" "r")))
		 (match_operand:DI			    3 "register_operand" "0")))]
  "TARGET_DSP"
  "mulua\t%1, %2"
)

(define_insn "msubsidi4"
  [(set (match_operand:DI				     0 "register_operand" "=y")
	(minus:DI (match_operand:DI			     3 "register_operand" "0")
		  (mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand" "r"))
			   (sign_extend:DI (match_operand:SI 2 "register_operand" "r")))))]
  "TARGET_DSP"
  "mulss\t%1, %2"
)

(define_insn "umsubsidi4"
  [(set (match_operand:DI				     0 "register_operand" "=y")
	(minus:DI (match_operand:DI			     3 "register_operand" "0")
		  (mult:DI (zero_extend:DI (match_operand:SI 1 "register_operand" "r"))
			   (zero_extend:DI (match_operand:SI 2 "register_operand" "r")))))]
  "TARGET_DSP"
  "mulus\t%1, %2"
)

(define_insn "*mulall_s16_0"
  [(set (match_operand:SI 0 "register_operand"			 "=r")
	(plus:SI (match_operand:SI 3 "register_operand"		 " 0")
		 (mult:SI (match_operand:SI 1 "register_operand" " r")
			  (match_operand:SI 2 "register_operand" " r"))))]
  "CSKY_ISA_FEATURE (3E3r1)"
  "mula.32.l\t%0, %1, %2"
  [(set_attr "type" "alu")
   (set_attr "length" "4")])

(define_insn "*mulall_s16_1"
  [(set (match_operand:SI 0 "register_operand"			 "=r")
	(plus:SI (mult:SI (match_operand:SI 1 "register_operand" " r")
			  (match_operand:SI 2 "register_operand" " r"))
		 (match_operand:SI 3 "register_operand"		 " 0")))]
  "CSKY_ISA_FEATURE (3E3r1)"
  "mula.32.l\t%0, %1, %2"
  [(set_attr "type" "alu")
   (set_attr "length" "4")])
