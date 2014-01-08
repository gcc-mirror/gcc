;; GCC machine description for picochip
;; Copyright (C) 2008-2014 Free Software Foundation, Inc.
;; Contributed by Picochip Ltd (http://www.picochip.com)
;; Maintained by Daniel Towner (dant@picochip.com) and Hariharan
;; Sandanagobalane (hariharan@picochip.com).
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
;; along with GCC; see the file COPYING3.  If not, see
;; <http://www.gnu.org/licenses/>.

;; The following DFA description schedules instructions for speed.  In
;; addition to the scheduling of instructions to remove stall cycles
;; (e.g., memory load), the scheduler will also pack multiple
;; instructions into a single cycle, using VLIW.

;; Each instruction comes in forms with and without long
;; constants.  The long constant is treated as though it were also an
;; instruction.  Thus, an instruction which used slot0, will use slot0
;; plus one of the other slots for the constant.  This mechanism
;; ensures that it is impossible for 3 instructions to be issued, if
;; one of them has a long constant.  This is necessary, because the
;; encoding of 3 instructions, plus a constant, will overrun the
;; 64-bit limit.

; Extended ALU - Slot 0
(define_insn_reservation "picoAluInsn" 1
  (and (eq_attr "schedType" "speed")
       (and (eq_attr "type" "picoAlu") (eq_attr "longConstant" "false")))
  "slot0")
(define_insn_reservation "picoAluInsnWithConst" 1
  (and (eq_attr "schedType" "speed")
       (and (eq_attr "type" "picoAlu") (eq_attr "longConstant" "true")))
  "(slot0+slot1)|(slot0+slot2)")

; Basic ALU - Slot 0 or 1
(define_insn_reservation "basicAluInsn" 1
  (and (eq_attr "schedType" "speed")
       (and (eq_attr "type" "basicAlu") (eq_attr "longConstant" "false")))
  "(slot0|slot1)")
(define_insn_reservation "basicAluInsnWithConst" 1
  (and (eq_attr "schedType" "speed")
       (and (eq_attr "type" "basicAlu") (eq_attr "longConstant" "true")))
  "(slot0+slot1) | (slot1+slot2) | (slot0+slot2)")

; ALU which must not set flags - Slot 1
(define_insn_reservation "nonCcAluInsn" 1
  (and (eq_attr "schedType" "speed")
       (and (eq_attr "type" "nonCcAlu") (eq_attr "longConstant" "false")))
  "slot1")
(define_insn_reservation "nonCcAluInsnWithConst" 1
  (and (eq_attr "schedType" "speed")
       (and (eq_attr "type" "nonCcAlu") (eq_attr "longConstant" "true")))
  "(slot1+slot0) | (slot1+slot2)")

; Memory - Slot 1
(define_insn_reservation "memInsn" 2
  (and (eq_attr "schedType" "speed")
       (and (eq_attr "type" "mem") (eq_attr "longConstant" "false")))
  "slot1,nothing")
(define_insn_reservation "memInsnWithConst" 2
  (and (eq_attr "schedType" "speed")
       (and (eq_attr "type" "mem") (eq_attr "longConstant" "true")))
  "slot1+(slot0|slot2),nothing")

; Multiply - Slot 2
(define_insn_reservation "mulInsn" 1
  (and (eq_attr "schedType" "speed")
       (and (eq_attr "type" "mul") (eq_attr "longConstant" "false")))
  "slot2")
(define_insn_reservation "mulInsnWithConst" 1
  (and (eq_attr "schedType" "speed")
       (and (eq_attr "type" "mul") (eq_attr "longConstant" "true")))
  "(slot2+slot0)|(slot2+slot1)")

; MAC - Slot 2
(define_insn_reservation "macInsn" 1
  (and (eq_attr "schedType" "speed")
       (and (eq_attr "type" "mac") (eq_attr "longConstant" "false")))
  "slot2")
(define_insn_reservation "macInsnWithConst" 1
  (and (eq_attr "schedType" "speed")
       (and (eq_attr "type" "mac") (eq_attr "longConstant" "true")))
  "(slot2+slot0)|(slot2+slot1)")

; Branch - Real branches use slot2, while macro branches use unknown
; resources.
(define_insn_reservation "branchInsn" 1
  (and (eq_attr "schedType" "speed")
       (and (eq_attr "type" "realBranch") (eq_attr "longConstant" "false")))
  "slot2")
(define_insn_reservation "branchInsnWithConst" 1
  (and (eq_attr "schedType" "speed")
       (and (eq_attr "type" "realBranch") (eq_attr "longConstant" "true")))
  "(slot2+slot0)|(slot2+slot1)")
(define_insn_reservation "branchInsnMacro" 1
  (and (eq_attr "schedType" "speed")
       (eq_attr "type" "realBranch"))
  "(slot0+slot1+slot2)")

; Call instructions use all slots to prevent inadvertent scheduling
; alongside instructions which set R12.

(define_insn_reservation "callInsn" 1
  (and (eq_attr "schedType" "speed") (eq_attr "type" "call"))
  "slot0+slot1+slot2")

; Communications - Slot 1
(define_insn_reservation "commsInsn" 1
  (and (eq_attr "schedType" "speed") (eq_attr "type" "comms"))
  "slot1")

