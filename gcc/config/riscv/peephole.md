;; Peephole optimizations for RISC-V for GNU compiler.
;; Copyright (C) 2011-2024 Free Software Foundation, Inc.
;; Contributed by Andrew Waterman (andrew@sifive.com).

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

;; Simplify (unsigned long)(unsigned int)a << const
(define_peephole2
  [(set (match_operand:DI 0 "register_operand")
	(ashift:DI (match_operand:DI 1 "register_operand")
		   (match_operand 2 "const_int_operand")))
   (set (match_operand:DI 3 "register_operand")
	(lshiftrt:DI (match_dup 0) (match_dup 2)))
   (set (match_operand:DI 4 "register_operand")
	(ashift:DI (match_dup 3) (match_operand 5 "const_int_operand")))]
  "TARGET_64BIT
   && INTVAL (operands[5]) < INTVAL (operands[2])
   && (REGNO (operands[3]) == REGNO (operands[4])
       || peep2_reg_dead_p (3, operands[3]))"
  [(set (match_dup 0)
	(ashift:DI (match_dup 1) (match_dup 2)))
   (set (match_dup 4)
	(lshiftrt:DI (match_dup 0) (match_operand 5)))]
{
  operands[5] = GEN_INT (INTVAL (operands[2]) - INTVAL (operands[5]));
})

;; ZCMP
(define_peephole2
  [(set (match_operand:X 0 "a0a1_reg_operand")
        (match_operand:X 1 "zcmp_mv_sreg_operand"))
   (set (match_operand:X 2 "a0a1_reg_operand")
        (match_operand:X 3 "zcmp_mv_sreg_operand"))]
  "TARGET_ZCMP
   && (REGNO (operands[2]) != REGNO (operands[0]))"
  [(parallel [(set (match_dup 0)
                   (match_dup 1))
              (set (match_dup 2)
                   (match_dup 3))])]
)

(define_peephole2
  [(set (match_operand:X 0 "zcmp_mv_sreg_operand")
        (match_operand:X 1 "a0a1_reg_operand"))
   (set (match_operand:X 2 "zcmp_mv_sreg_operand")
        (match_operand:X 3 "a0a1_reg_operand"))]
  "TARGET_ZCMP
   && (REGNO (operands[0]) != REGNO (operands[2]))
   && (REGNO (operands[1]) != REGNO (operands[3]))"
  [(parallel [(set (match_dup 0)
                   (match_dup 1))
              (set (match_dup 2)
                   (match_dup 3))])]
)
