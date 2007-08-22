;; Machine Description for shared bits common to IWMMXT and Neon.
;; Copyright (C) 2006, 2007 Free Software Foundation, Inc.
;; Written by CodeSourcery.
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
;; <http://www.gnu.org/licenses/>.

;; Vector Moves

;; All integer and float modes supported by Neon and IWMMXT.
(define_mode_iterator VALL [V2DI V2SI V4HI V8QI V2SF V4SI V8HI V16QI V4SF])

;; All integer and float modes supported by Neon and IWMMXT, except V2DI.
(define_mode_iterator VALLW [V2SI V4HI V8QI V2SF V4SI V8HI V16QI V4SF])

;; All integer modes supported by Neon and IWMMXT
(define_mode_iterator VINT [V2DI V2SI V4HI V8QI V4SI V8HI V16QI])

;; All integer modes supported by Neon and IWMMXT, except V2DI
(define_mode_iterator VINTW [V2SI V4HI V8QI V4SI V8HI V16QI])

(define_expand "mov<mode>"
  [(set (match_operand:VALL 0 "nonimmediate_operand" "")
	(match_operand:VALL 1 "general_operand" ""))]
  "TARGET_NEON
   || (TARGET_REALLY_IWMMXT && VALID_IWMMXT_REG_MODE (<MODE>mode))"
{
})

;; Vector arithmetic. Expanders are blank, then unnamed insns implement
;; patterns separately for IWMMXT and Neon.

(define_expand "add<mode>3"
  [(set (match_operand:VALL 0 "s_register_operand" "")
        (plus:VALL (match_operand:VALL 1 "s_register_operand" "")
                   (match_operand:VALL 2 "s_register_operand" "")))]
  "TARGET_NEON
   || (TARGET_REALLY_IWMMXT && VALID_IWMMXT_REG_MODE (<MODE>mode))"
{
})

(define_expand "sub<mode>3"
  [(set (match_operand:VALL 0 "s_register_operand" "")
        (minus:VALL (match_operand:VALL 1 "s_register_operand" "")
                    (match_operand:VALL 2 "s_register_operand" "")))]
  "TARGET_NEON
   || (TARGET_REALLY_IWMMXT && VALID_IWMMXT_REG_MODE (<MODE>mode))"
{
})

(define_expand "mul<mode>3"
  [(set (match_operand:VALLW 0 "s_register_operand" "")
        (mult:VALLW (match_operand:VALLW 1 "s_register_operand" "")
		    (match_operand:VALLW 2 "s_register_operand" "")))]
  "TARGET_NEON || (<MODE>mode == V4HImode && TARGET_REALLY_IWMMXT)"
{
})

(define_expand "smin<mode>3"
  [(set (match_operand:VALLW 0 "s_register_operand" "")
	(smin:VALLW (match_operand:VALLW 1 "s_register_operand" "")
		    (match_operand:VALLW 2 "s_register_operand" "")))]
  "TARGET_NEON
   || (TARGET_REALLY_IWMMXT && VALID_IWMMXT_REG_MODE (<MODE>mode))"
{
})

(define_expand "umin<mode>3"
  [(set (match_operand:VINTW 0 "s_register_operand" "")
	(umin:VINTW (match_operand:VINTW 1 "s_register_operand" "")
		    (match_operand:VINTW 2 "s_register_operand" "")))]
  "TARGET_NEON
   || (TARGET_REALLY_IWMMXT && VALID_IWMMXT_REG_MODE (<MODE>mode))"
{
})

(define_expand "smax<mode>3"
  [(set (match_operand:VALLW 0 "s_register_operand" "")
	(smax:VALLW (match_operand:VALLW 1 "s_register_operand" "")
		    (match_operand:VALLW 2 "s_register_operand" "")))]
  "TARGET_NEON
   || (TARGET_REALLY_IWMMXT && VALID_IWMMXT_REG_MODE (<MODE>mode))"
{
})

(define_expand "umax<mode>3"
  [(set (match_operand:VINTW 0 "s_register_operand" "")
	(umax:VINTW (match_operand:VINTW 1 "s_register_operand" "")
		    (match_operand:VINTW 2 "s_register_operand" "")))]
  "TARGET_NEON
   || (TARGET_REALLY_IWMMXT && VALID_IWMMXT_REG_MODE (<MODE>mode))"
{
})
