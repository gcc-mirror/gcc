;; GCC machine description for Alpha synchronization instructions.
;; Copyright (C) 2005-2023 Free Software Foundation, Inc.
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

(define_code_iterator FETCHOP [plus minus ior xor and])
(define_code_attr fetchop_name
  [(plus "add") (minus "sub") (ior "or") (xor "xor") (and "and")])
(define_code_attr fetchop_pred
  [(plus "add_operand") (minus "reg_or_8bit_operand")
   (ior "or_operand") (xor "or_operand") (and "and_operand")])
(define_code_attr fetchop_constr
  [(plus "rKL") (minus "rI") (ior "rIN") (xor "rIN") (and "rINM")])


(define_expand "memory_barrier"
  [(set (match_dup 0)
	(unspec:BLK [(match_dup 0)] UNSPEC_MB))]
  ""
{
  operands[0] = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (Pmode));
  MEM_VOLATILE_P (operands[0]) = 1;
})

(define_insn "*memory_barrier"
  [(set (match_operand:BLK 0)
	(unspec:BLK [(match_dup 0)] UNSPEC_MB))]
  ""
  "mb"
  [(set_attr "type" "mb")])

(define_insn "@load_locked_<mode>"
  [(set (match_operand:I48MODE 0 "register_operand" "=r")
	(unspec_volatile:I48MODE
	  [(match_operand:I48MODE 1 "memory_operand" "m")]
	  UNSPECV_LL))]
  ""
  "ld<modesuffix>_l %0,%1"
  [(set_attr "type" "ld_l")])

(define_insn "@store_conditional_<mode>"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec_volatile:DI [(const_int 0)] UNSPECV_SC))
   (set (match_operand:I48MODE 1 "memory_operand" "=m")
	(match_operand:I48MODE 2 "reg_or_0_operand" "0"))]
  ""
  "st<modesuffix>_c %0,%1"
  [(set_attr "type" "st_c")])

;; The Alpha Architecture Handbook says that it is UNPREDICTABLE whether
;; the lock is cleared by a normal load or store.  This means we cannot
;; expand a ll/sc sequence before reload, lest a register spill is
;; inserted inside the sequence.  It is also UNPREDICTABLE whether the
;; lock is cleared by a TAKEN branch.  This means that we cannot expand
;; a ll/sc sequence containing a branch (i.e. compare-and-swap) until after
;; the final basic-block reordering pass.

(define_expand "atomic_compare_and_swap<mode>"
  [(parallel
     [(set (match_operand:DI 0 "register_operand")	  ;; bool out
	   (unspec_volatile:DI [(const_int 0)] UNSPECV_CMPXCHG))
      (set (match_operand:I48MODE 1 "register_operand")	  ;; val out
	   (unspec_volatile:I48MODE [(const_int 0)] UNSPECV_CMPXCHG))
      (set (match_operand:I48MODE 2 "memory_operand")	  ;; memory
	   (unspec_volatile:I48MODE
	     [(match_dup 2)
	      (match_operand:I48MODE 3 "reg_or_8bit_operand")  ;; expected
	      (match_operand:I48MODE 4 "add_operand")	  ;; desired
	      (match_operand:SI 5 "const_int_operand")	  ;; is_weak
	      (match_operand:SI 6 "const_int_operand")	  ;; succ model
	      (match_operand:SI 7 "const_int_operand")]	  ;; fail model
	     UNSPECV_CMPXCHG))])]
  ""
{
  if (<MODE>mode == SImode)
    {
      operands[3] = convert_modes (DImode, SImode, operands[3], 0);
      operands[4] = convert_modes (DImode, SImode, operands[4], 0);
    }
})

(define_insn_and_split "*atomic_compare_and_swap<mode>"
  [(set (match_operand:DI 0 "register_operand" "=&r")		;; bool out
	(unspec_volatile:DI [(const_int 0)] UNSPECV_CMPXCHG))
   (set (match_operand:I48MODE 1 "register_operand" "=&r")	;; val out
	(unspec_volatile:I48MODE [(const_int 0)] UNSPECV_CMPXCHG))
   (set (match_operand:I48MODE 2 "memory_operand" "+m")		;; memory
	(unspec_volatile:I48MODE
	  [(match_dup 2)
	   (match_operand:DI 3 "reg_or_8bit_operand" "rI")	;; expected
	   (match_operand:DI 4 "add_operand" "rKL")		;; desired
	   (match_operand:SI 5 "const_int_operand")		;; is_weak
	   (match_operand:SI 6 "const_int_operand")		;; succ model
	   (match_operand:SI 7 "const_int_operand")]		;; fail model
	  UNSPECV_CMPXCHG))]
  ""
  "#"
  "epilogue_completed"
  [(const_int 0)]
{
  alpha_split_compare_and_swap (operands);
  DONE;
}
  [(set_attr "type" "multi")])

(define_expand "atomic_compare_and_swap<mode>"
  [(match_operand:DI 0 "register_operand")		;; bool out
   (match_operand:I12MODE 1 "register_operand")		;; val out
   (match_operand:I12MODE 2 "mem_noofs_operand")	;; memory
   (match_operand:I12MODE 3 "register_operand")		;; expected
   (match_operand:I12MODE 4 "add_operand")		;; desired
   (match_operand:SI 5 "const_int_operand")		;; is_weak
   (match_operand:SI 6 "const_int_operand")		;; succ model
   (match_operand:SI 7 "const_int_operand")]		;; fail model
  ""
{
  alpha_expand_compare_and_swap_12 (operands);
  DONE;
})

(define_insn_and_split "@atomic_compare_and_swap<mode>_1"
  [(set (match_operand:DI 0 "register_operand" "=&r")		;; bool out
	(unspec_volatile:DI [(const_int 0)] UNSPECV_CMPXCHG))
   (set (match_operand:DI 1 "register_operand" "=&r")		;; val out
	(zero_extend:DI
	  (unspec_volatile:I12MODE [(const_int 0)] UNSPECV_CMPXCHG)))
   (set (match_operand:I12MODE 2 "mem_noofs_operand" "+w")	;; memory
	(unspec_volatile:I12MODE
	  [(match_dup 2)
	   (match_operand:DI 3 "reg_or_8bit_operand" "rI")	;; expected
	   (match_operand:DI 4 "reg_or_0_operand" "rJ")		;; desired
	   (match_operand:DI 5 "register_operand" "r")		;; align
	   (match_operand:SI 6 "const_int_operand")		;; is_weak
	   (match_operand:SI 7 "const_int_operand")		;; succ model
	   (match_operand:SI 8 "const_int_operand")]		;; fail model
	  UNSPECV_CMPXCHG))
   (clobber (match_scratch:DI 9 "=&r"))]
  ""
  "#"
  "epilogue_completed"
  [(const_int 0)]
{
  alpha_split_compare_and_swap_12 (operands);
  DONE;
}
  [(set_attr "type" "multi")])

(define_insn_and_split "atomic_exchange<mode>"
  [(set (match_operand:I48MODE 0 "register_operand" "=&r")	;; output
	(match_operand:I48MODE 1 "memory_operand" "+m"))	;; memory
   (set (match_dup 1)
	(unspec:I48MODE
	  [(match_operand:I48MODE 2 "add_operand" "rKL")	;; input
	   (match_operand:SI 3 "const_int_operand")]		;; model
	  UNSPEC_XCHG))
   (clobber (match_scratch:I48MODE 4 "=&r"))]
  ""
  "#"
  "epilogue_completed"
  [(const_int 0)]
{
  alpha_split_atomic_exchange (operands);
  DONE;
}
  [(set_attr "type" "multi")])

(define_expand "atomic_exchange<mode>"
  [(match_operand:I12MODE 0 "register_operand")		;; output
   (match_operand:I12MODE 1 "mem_noofs_operand")	;; memory
   (match_operand:I12MODE 2 "reg_or_0_operand")		;; input
   (match_operand:SI 3 "const_int_operand")]		;; model
  ""
{
  alpha_expand_atomic_exchange_12 (operands);
  DONE;
})

(define_insn_and_split "@atomic_exchange<mode>_1"
  [(set (match_operand:DI 0 "register_operand" "=&r")		;; output
	(zero_extend:DI
	  (match_operand:I12MODE 1 "mem_noofs_operand" "+w")))	;; memory
   (set (match_dup 1)
	(unspec:I12MODE
	  [(match_operand:DI 2 "reg_or_8bit_operand" "rI")	;; input
	   (match_operand:DI 3 "register_operand" "r")		;; align
	   (match_operand:SI 4 "const_int_operand")]		;; model
	  UNSPEC_XCHG))
   (clobber (match_scratch:DI 5 "=&r"))]
  ""
  "#"
  "epilogue_completed"
  [(const_int 0)]
{
  alpha_split_atomic_exchange_12 (operands);
  DONE;
}
  [(set_attr "type" "multi")])

(define_insn_and_split "atomic_<fetchop_name><mode>"
  [(set (match_operand:I48MODE 0 "memory_operand" "+m")
	(unspec:I48MODE
	  [(FETCHOP:I48MODE (match_dup 0)
	     (match_operand:I48MODE 1 "<fetchop_pred>" "<fetchop_constr>"))
	   (match_operand:SI 2 "const_int_operand")]
	  UNSPEC_ATOMIC))
   (clobber (match_scratch:I48MODE 3 "=&r"))]
  ""
  "#"
  "epilogue_completed"
  [(const_int 0)]
{
  alpha_split_atomic_op (<CODE>, operands[0], operands[1],
			 NULL, NULL, operands[3],
			 (enum memmodel) INTVAL (operands[2]));
  DONE;
}
  [(set_attr "type" "multi")])

(define_insn_and_split "atomic_nand<mode>"
  [(set (match_operand:I48MODE 0 "memory_operand" "+m")
	(unspec:I48MODE
	  [(not:I48MODE
	     (and:I48MODE (match_dup 0)
	       (match_operand:I48MODE 1 "register_operand" "r")))
	   (match_operand:SI 2 "const_int_operand")]
	  UNSPEC_ATOMIC))
   (clobber (match_scratch:I48MODE 3 "=&r"))]
  ""
  "#"
  "epilogue_completed"
  [(const_int 0)]
{
  alpha_split_atomic_op (NOT, operands[0], operands[1],
			 NULL, NULL, operands[3],
			 (enum memmodel) INTVAL (operands[2]));
  DONE;
}
  [(set_attr "type" "multi")])

(define_insn_and_split "atomic_fetch_<fetchop_name><mode>"
  [(set (match_operand:I48MODE 0 "register_operand" "=&r")
	(match_operand:I48MODE 1 "memory_operand" "+m"))
   (set (match_dup 1)
	(unspec:I48MODE
	  [(FETCHOP:I48MODE (match_dup 1)
	     (match_operand:I48MODE 2 "<fetchop_pred>" "<fetchop_constr>"))
	   (match_operand:SI 3 "const_int_operand")]
	  UNSPEC_ATOMIC))
   (clobber (match_scratch:I48MODE 4 "=&r"))]
  ""
  "#"
  "epilogue_completed"
  [(const_int 0)]
{
  alpha_split_atomic_op (<CODE>, operands[1], operands[2],
			 operands[0], NULL, operands[4],
			 (enum memmodel) INTVAL (operands[3]));
  DONE;
}
  [(set_attr "type" "multi")])

(define_insn_and_split "atomic_fetch_nand<mode>"
  [(set (match_operand:I48MODE 0 "register_operand" "=&r")
	(match_operand:I48MODE 1 "memory_operand" "+m"))
   (set (match_dup 1)
	(unspec:I48MODE
	  [(not:I48MODE
	     (and:I48MODE (match_dup 1)
	       (match_operand:I48MODE 2 "register_operand" "r")))
	   (match_operand:SI 3 "const_int_operand")]
	  UNSPEC_ATOMIC))
   (clobber (match_scratch:I48MODE 4 "=&r"))]
  ""
  "#"
  "epilogue_completed"
  [(const_int 0)]
{
  alpha_split_atomic_op (NOT, operands[1], operands[2],
			 operands[0], NULL, operands[4],
			 (enum memmodel) INTVAL (operands[3]));
  DONE;
}
  [(set_attr "type" "multi")])

(define_insn_and_split "atomic_<fetchop_name>_fetch<mode>"
  [(set (match_operand:I48MODE 0 "register_operand" "=&r")
	(FETCHOP:I48MODE 
	  (match_operand:I48MODE 1 "memory_operand" "+m")
	  (match_operand:I48MODE 2 "<fetchop_pred>" "<fetchop_constr>")))
   (set (match_dup 1)
	(unspec:I48MODE
	  [(FETCHOP:I48MODE (match_dup 1) (match_dup 2))
	   (match_operand:SI 3 "const_int_operand")]
	  UNSPEC_ATOMIC))
   (clobber (match_scratch:I48MODE 4 "=&r"))]
  ""
  "#"
  "epilogue_completed"
  [(const_int 0)]
{
  alpha_split_atomic_op (<CODE>, operands[1], operands[2],
			 NULL, operands[0], operands[4],
			 (enum memmodel) INTVAL (operands[3]));
  DONE;
}
  [(set_attr "type" "multi")])

(define_insn_and_split "atomic_nand_fetch<mode>"
  [(set (match_operand:I48MODE 0 "register_operand" "=&r")
	(not:I48MODE
	  (and:I48MODE (match_operand:I48MODE 1 "memory_operand" "+m")
	    (match_operand:I48MODE 2 "register_operand" "r"))))
   (set (match_dup 1)
	(unspec:I48MODE
	  [(not:I48MODE (and:I48MODE (match_dup 1) (match_dup 2)))
	   (match_operand:SI 3 "const_int_operand")]
	  UNSPEC_ATOMIC))
   (clobber (match_scratch:I48MODE 4 "=&r"))]
  ""
  "#"
  "epilogue_completed"
  [(const_int 0)]
{
  alpha_split_atomic_op (NOT, operands[1], operands[2],
			 NULL, operands[0], operands[4],
			 (enum memmodel) INTVAL (operands[3]));
  DONE;
}
  [(set_attr "type" "multi")])
