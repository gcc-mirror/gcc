;; GCC machine description for C6X synchronization instructions.
;; Copyright (C) 2011-2017 Free Software Foundation, Inc.
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

;; C64X+ has atomic instructions, but they are not atomic on all
;; devices and have other problems.  We use normal loads and stores,
;; and place them in overlapping branch shadows to ensure interrupts
;; are disabled during the sequence, which guarantees atomicity on all
;; single-core systems.

(define_code_iterator FETCHOP [plus minus ior xor and])
(define_code_attr fetchop_name
  [(plus "add") (minus "sub") (ior "ior") (xor "xor") (and "and")])
(define_code_attr fetchop_pred
  [(plus "reg_or_scst5_operand") (minus "register_operand")
   (ior "reg_or_scst5_operand") (xor "reg_or_scst5_operand")
   (and "reg_or_scst5_operand")])
(define_code_attr fetchop_constr
  [(plus "bIs5") (minus "b") (ior "bIs5") (xor "bIs5") (and "bIs5")])
(define_code_attr fetchop_opcode
  [(plus "add") (minus "sub") (ior "or") (xor "xor") (and "and")])
(define_code_attr fetchop_inops02
  [(plus "%2, %0") (minus "%0, %2") (ior "%2, %0") (xor "%2, %0")
   (and "%2, %0")])
(define_code_attr fetchop_inops21
  [(plus "%1, %2") (minus "%2, %1") (ior "%1, %2") (xor "%1, %2")
   (and "%1, %2")])

(define_expand "sync_compare_and_swapsi"
  [(parallel
     [(set (match_operand:SI 0 "register_operand" "")
	   (match_operand:SI 1 "memory_operand" ""))
      (set (match_dup 1)
	   (unspec_volatile:SI
	     [(match_operand:SI 2 "register_operand" "")
	      (match_operand:SI 3 "register_operand" "")]
	     UNSPECV_CAS))
      (clobber (match_scratch:SI 4 ""))])]
  ""
{
})

(define_expand "sync_<fetchop_name>si"
  [(parallel
    [(set (match_operand:SI 0 "memory_operand" "")
	  (unspec:SI
	   [(FETCHOP:SI (match_dup 0)
			(match_operand:SI 1 "<fetchop_pred>" ""))]
	   UNSPEC_ATOMIC))
     (clobber (match_scratch:SI 2 ""))])]
  ""
{
})

(define_expand "sync_old_<fetchop_name>si"
  [(parallel
    [(set (match_operand:SI 0 "register_operand" "")
	  (match_operand:SI 1 "memory_operand" ""))
     (set (match_dup 1)
	  (unspec:SI
	   [(FETCHOP:SI (match_dup 1)
			(match_operand:SI 2 "<fetchop_pred>" ""))]
	   UNSPEC_ATOMIC))
     (clobber (match_scratch:SI 3 ""))])]
  ""
{
})

(define_expand "sync_new_<fetchop_name>si"
  [(parallel
    [(set (match_operand:SI 0 "register_operand" "")
	  (FETCHOP:SI (match_operand:SI 1 "memory_operand" "")
		      (match_operand:SI 2 "<fetchop_pred>" "")))
     (set (match_dup 1)
	  (unspec:SI [(FETCHOP:SI (match_dup 1) (match_dup 2))]
		     UNSPEC_ATOMIC))
     (clobber (match_scratch:SI 3 ""))])]
  ""
{
})

(define_expand "sync_nandsi"
  [(parallel
    [(set (match_operand:SI 0 "memory_operand" "")
	  (unspec:SI
	   [(not:SI (and:SI (match_dup 0)
			    (match_operand:SI 1 "reg_or_scst5_operand" "")))]
	   UNSPEC_ATOMIC))
     (clobber (match_scratch:SI 2 ""))])]
  ""
{
})

(define_expand "sync_old_nandsi"
  [(parallel
    [(set (match_operand:SI 0 "register_operand" "")
	  (match_operand:SI 1 "memory_operand" ""))
     (set (match_dup 1)
	  (unspec:SI
	   [(not:SI (and:SI (match_dup 1)
		    (match_operand:SI 2 "reg_or_scst5_operand" "")))]
	   UNSPEC_ATOMIC))
     (clobber (match_scratch:SI 3 ""))])]
  ""
{
})

(define_expand "sync_new_nandsi"
  [(parallel
    [(set (match_operand:SI 0 "register_operand" "")
	  (not:SI (and:SI (match_operand:SI 1 "memory_operand" "")
			  (match_operand:SI 2 "reg_or_scst5_operand" ""))))
     (set (match_dup 1)
	  (unspec:SI [(not:SI (and:SI (match_dup 1) (match_dup 2)))]
		     UNSPEC_ATOMIC))
     (clobber (match_scratch:SI 3 ""))])]
  ""
{
})

(define_insn "*sync_compare_and_swapsi"
  [(set (match_operand:SI 0 "register_operand" "=&b")
	(match_operand:SI 1 "memory_operand" "+m"))
   (set (match_dup 1)
	(unspec_volatile:SI
	  [(match_operand:SI 2 "register_operand" "B")
	   (match_operand:SI 3 "register_operand" "b")]
	  UNSPECV_CAS))
   (clobber (match_scratch:SI 4 "=&B"))]
  ""
  "0: b .s2 1f ; 0\n\\
   || ldw .d%U1t%U0 %1, %0\n\\
   nop 4\n\\
|| b .s2 2f ; 1\n\\
   cmpeq .l2 %0, %2, %2 ; 5\n\\
1: [%2] stw .d%U1t%U3 %3, %1 ; 6\n\\
2:"
  [(set_attr "type" "atomic")])

(define_insn "sync_<fetchop_name>si_insn"
  [(set (match_operand:SI 0 "memory_operand" "+m")
	(unspec:SI
	  [(FETCHOP:SI (match_dup 0)
	     (match_operand:SI 1 "<fetchop_pred>" "<fetchop_constr>"))]
	  UNSPEC_ATOMIC))
   (clobber (match_scratch:SI 2 "=&B"))]
  ""
  "0: b .s2 1f ; 0\n\\
|| ldw .d%U0t%U2 %0, %2\n\\
   nop 4\n\\
|| b .s2 2f ; 1\n\\
   <fetchop_opcode> .l2 <fetchop_inops21>, %2 ; 5\n\\
1: stw .d%U0t%U2 %2, %0 ; 6\n\\
2:"
  [(set_attr "type" "atomic")])

(define_insn "sync_old_<fetchop_name>si_insn"
  [(set (match_operand:SI 0 "register_operand" "=&b")
	(match_operand:SI 1 "memory_operand" "+m"))
   (set (match_dup 1)
	(unspec:SI
	  [(FETCHOP:SI (match_dup 1)
	     (match_operand:SI 2 "<fetchop_pred>" "<fetchop_constr>"))]
	  UNSPEC_ATOMIC))
   (clobber (match_scratch:SI 3 "=&B"))]
  ""
  "0: b .s2 1f ; 0\n\\
|| ldw .d%U1t%U0 %1, %0\n\\
   nop 4\n\\
|| b .s2 2f ; 1\n\\
   <fetchop_opcode> .l2 <fetchop_inops02>, %3 ; 5\n\\
1: stw .d%U1t%U3 %3, %1 ; 6\n\\
2:"
  [(set_attr "type" "atomic")])

(define_insn "sync_new_<fetchop_name>si_insn"
  [(set (match_operand:SI 0 "register_operand" "=&b")
	(FETCHOP:SI (match_operand:SI 1 "memory_operand" "+m")
	   (match_operand:SI 2 "<fetchop_pred>" "<fetchop_constr>")))
   (set (match_dup 1)
	(unspec:SI
	  [(FETCHOP:SI (match_dup 1)
		       (match_dup 2))]
	  UNSPEC_ATOMIC))
   (clobber (match_scratch:SI 3 "=&B"))]
  ""
  "0: b .s2 1f ; 0\n\\
|| ldw .d%U1t%U0 %1, %0\n\\
   nop 4\n\\
|| b .s2 2f ; 1\n\\
   <fetchop_opcode> .l2 <fetchop_inops02>, %0 ; 5\n\\
1: stw .d%U1t%U0 %0, %1 ; 6\n\\
2:"
  [(set_attr "type" "atomic")])

(define_insn "sync_nandsi_insn"
  [(set (match_operand:SI 0 "memory_operand" "+m")
	(unspec:SI
	  [(not:SI (and:SI (match_dup 0)
			   (match_operand:SI 1 "reg_or_scst5_operand" "bIs5")))]
	  UNSPEC_ATOMIC))
   (clobber (match_scratch:SI 2 "=&B"))]
  ""
  "0: b .s2 1f ; 0\n\\
|| ldw .d%U0t%U2 %0, %2\n\\
   nop 1\n\\
   nop 3\n\\
|| b .s2 2f ; 2\n\\
   and .l2 %1, %2, %2 ; 5\n\\
1: not .l2 %2, %2 ; 6\n\\
   stw .d%U0t%U2 %2, %0 ; 7\n\\
2:"
  [(set_attr "type" "atomic")])

(define_insn "sync_old_nandsi_insn"
  [(set (match_operand:SI 0 "register_operand" "=&b")
	(match_operand:SI 1 "memory_operand" "+m"))
   (set (match_dup 1)
	(unspec:SI
	  [(not:SI (and:SI (match_dup 1)
			   (match_operand:SI 2 "reg_or_scst5_operand" "bIs5")))]
	  UNSPEC_ATOMIC))
   (clobber (match_scratch:SI 3 "=&B"))]
  ""
  "0: b .s2 1f ; 0\n\\
|| ldw .d%U1t%U0 %1, %0\n\\
   nop 1\n\\
   nop 3\n\\
|| b .s2 2f ; 2\n\\
   and .l2 %2, %0, %3 ; 5\n\\
1: not .l2 %3, %3 ; 6\n\\
   stw .d%U1t%U3 %3, %1 ; 7\n\\
2:"
  [(set_attr "type" "atomic")])

(define_insn "sync_new_nandsi_insn"
  [(set (match_operand:SI 0 "register_operand" "=&b")
	(not:SI (and:SI (match_operand:SI 1 "memory_operand" "+m")
			(match_operand:SI 2 "reg_or_scst5_operand" "bIs5"))))
   (set (match_dup 1)
	(unspec:SI
	  [(not:SI (and:SI (match_dup 1) (match_dup 2)))]
	  UNSPEC_ATOMIC))
   (clobber (match_scratch:SI 3 "=&B"))]
  ""
  "0: b .s2 1f ; 0\n\\
|| ldw .d%U1t%U0 %1, %0\n\\
   nop 1\n\\
   nop 3\n\\
|| b .s2 2f ; 2\n\\
   and .l2 %2, %0, %0 ; 5\n\\
1: not .l2 %0, %0 ; 6\n\\
   stw .d%U1t%U0 %0, %1 ; 7\n\\
2:"
  [(set_attr "type" "atomic")])
