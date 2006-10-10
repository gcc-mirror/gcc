;;  Machine description for Sunplus S+CORE
;;  Copyright (C) 2005
;;  Free Software Foundation, Inc.
;;  Contributed by Sunnorth.

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING.  If not, write to
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;- See file "rtl.def" for documentation on define_insn, match_*, et. al.

(define_insn "pushsi"
  [(set (match_operand:SI 0 "push_operand" "=<")
        (match_operand:SI 1 "register_operand" "d"))]
  ""
  "push!   %1, [r0]"
  [(set_attr "type" "store")
   (set_attr "mode" "SI")])

(define_insn "popsi"
  [(set (match_operand:SI 0 "register_operand" "=d")
        (match_operand:SI 1 "pop_operand" ">"))]
  ""
  "pop!    %0, [r0]"
  [(set_attr "type" "store")
   (set_attr "mode" "SI")])

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
  "mfcehl  %2, %0"
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
  mdx_movsicc (operands);
})

(define_insn "movsicc_internal"
  [(set (match_operand:SI 0 "register_operand" "=d")
        (if_then_else:SI (match_operator 1 "comparison_operator"
                          [(reg:CC CC_REGNUM) (const_int 0)])
                         (match_operand:SI 2 "register_operand" "d")
                         (match_operand:SI 3 "register_operand" "0")))]
  ""
  "mv%C1   %0, %2"
  [(set_attr "type" "cndmv")
   (set_attr "mode" "SI")])

(define_insn "zero_extract_bittst"
  [(set (reg:CC_NZ CC_REGNUM)
        (compare:CC_NZ (unspec:SI
                        [(match_operand:SI 0 "register_operand" "*e,d")
                         (match_operand:SI 1 "const_bi_operand" "")]
                        BITTST)
                       (const_int 0)))]
  ""
  "@
   bittst!  %0, %c1
   bittst.c %0, %c1"
  [(set_attr "type" "arith")
   (set_attr "up_c" "yes")
   (set_attr "mode" "SI")])

