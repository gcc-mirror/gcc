;; Intrinsic patterns description of Andes NDS32 cpu for GNU compiler
;; Copyright (C) 2012-2016 Free Software Foundation, Inc.
;; Contributed by Andes Technology Corporation.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;; ------------------------------------------------------------------------

;; Register Transfer.

(define_insn "unspec_volatile_mfsr"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec_volatile:SI [(match_operand:SI 1 "immediate_operand" "i")] UNSPEC_VOLATILE_MFSR))]
  ""
  "mfsr\t%0, %V1"
  [(set_attr "type"   "misc")
   (set_attr "length"    "4")]
)

(define_insn "unspec_volatile_mfusr"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec_volatile:SI [(match_operand:SI 1 "immediate_operand" "i")] UNSPEC_VOLATILE_MFUSR))]
  ""
  "mfusr\t%0, %V1"
  [(set_attr "type"   "misc")
   (set_attr "length"    "4")]
)

(define_insn "unspec_volatile_mtsr"
  [(unspec_volatile:SI [(match_operand:SI 0 "register_operand" "r")
			(match_operand:SI 1 "immediate_operand" "i")] UNSPEC_VOLATILE_MTSR)]
  ""
  "mtsr\t%0, %V1"
  [(set_attr "type"   "misc")
   (set_attr "length"    "4")]
)

(define_insn "unspec_volatile_mtusr"
  [(unspec_volatile:SI [(match_operand:SI 0 "register_operand" "r")
			(match_operand:SI 1 "immediate_operand" "i")] UNSPEC_VOLATILE_MTUSR)]
  ""
  "mtusr\t%0, %V1"
  [(set_attr "type"   "misc")
   (set_attr "length"    "4")]
)

;; ------------------------------------------------------------------------

;; Interrupt Instructions.

(define_insn "unspec_volatile_setgie_en"
  [(unspec_volatile:SI [(const_int 0)] UNSPEC_VOLATILE_SETGIE_EN)]
  ""
  "setgie.e"
  [(set_attr "type" "misc")]
)

(define_insn "unspec_volatile_setgie_dis"
  [(unspec_volatile:SI [(const_int 0)] UNSPEC_VOLATILE_SETGIE_DIS)]
  ""
  "setgie.d"
  [(set_attr "type" "misc")]
)

;; ------------------------------------------------------------------------

;; Cache Synchronization Instructions

(define_insn "unspec_volatile_isync"
  [(unspec_volatile:SI [(match_operand:SI 0 "register_operand" "r")] UNSPEC_VOLATILE_ISYNC)]
  ""
  "isync\t%0"
  [(set_attr "type" "misc")]
)

(define_insn "unspec_volatile_isb"
  [(unspec_volatile:SI [(const_int 0)] UNSPEC_VOLATILE_ISB)]
  ""
  "isb"
  [(set_attr "type" "misc")]
)

;; ------------------------------------------------------------------------
