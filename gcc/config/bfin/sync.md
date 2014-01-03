;; GCC machine description for Blackfin synchronization instructions.
;; Copyright (C) 2005-2014 Free Software Foundation, Inc.
;; Contributed by Analog Devices.
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

(define_code_iterator FETCHOP [plus minus ior and xor])
(define_code_attr fetchop_name
  [(plus "add") (minus "sub") (ior "ior") (and "and") (xor "xor")])
(define_code_attr fetchop_addr
  [(plus "1072") (minus "1088") (ior "1104") (and "1120") (xor "1136")])

(define_insn "sync_<fetchop_name>si_internal"
  [(set (mem:SI (match_operand:SI 0 "register_operand" "qA"))
	(unspec:SI
	  [(FETCHOP:SI (mem:SI (match_dup 0))
	     (match_operand:SI 1 "register_operand" "q0"))
	   (match_operand:SI 2 "register_no_elim_operand" "a")]
	  UNSPEC_ATOMIC))
   (clobber (match_scratch:SI 3 "=q0"))
   (clobber (match_scratch:SI 4 "=q1"))
   (clobber (reg:SI REG_RETS))]
  "TARGET_SUPPORTS_SYNC_CALLS"
  "call (%2);"
  [(set_attr "type" "call")])

(define_expand "sync_<fetchop_name>si"
  [(parallel
    [(set (match_operand:SI 0 "memory_operand" "+m")
	  (unspec:SI
	   [(FETCHOP:SI (match_dup 0)
			(match_operand:SI 1 "register_operand" "q0"))
	    (match_dup 2)]
	   UNSPEC_ATOMIC))
     (clobber (match_scratch:SI 3 ""))
     (clobber (match_scratch:SI 4 ""))
     (clobber (reg:SI REG_RETS))])]
  "TARGET_SUPPORTS_SYNC_CALLS"
{
  if (!REG_P (XEXP (operands[0], 0)))
    {
      operands[0] = shallow_copy_rtx (operands[0]);
      XEXP (operands[0], 0) = force_reg (Pmode, XEXP (operands[0], 0));
    }
  operands[2] = force_reg (Pmode, GEN_INT (<fetchop_addr>));
})

(define_insn "sync_old_<fetchop_name>si_internal"
  [(set (match_operand:SI 0 "register_operand" "=q1")
	(mem:SI (match_operand:SI 1 "register_operand" "qA")))
   (set (mem:SI (match_dup 1))
	(unspec:SI
	  [(FETCHOP:SI (mem:SI (match_dup 1))
	     (match_operand:SI 2 "register_operand" "q0"))
	   (match_operand:SI 3 "register_no_elim_operand" "a")]
	  UNSPEC_ATOMIC))
   (clobber (match_scratch:SI 4 "=q0"))
   (clobber (reg:SI REG_RETS))]
  "TARGET_SUPPORTS_SYNC_CALLS"
  "call (%3);"
  [(set_attr "type" "call")])

(define_expand "sync_old_<fetchop_name>si"
  [(parallel
    [(set (match_operand:SI 0 "register_operand" "")
	  (match_operand:SI 1 "memory_operand" ""))
     (set (match_dup 1)
	  (unspec:SI
	   [(FETCHOP:SI (match_dup 1)
			(match_operand:SI 2 "register_operand" ""))
	    (match_dup 3)]
	   UNSPEC_ATOMIC))
     (clobber (match_scratch:SI 4 ""))
     (clobber (reg:SI REG_RETS))])]
  "TARGET_SUPPORTS_SYNC_CALLS"
{
  if (!REG_P (XEXP (operands[1], 0)))
    {
      operands[1] = shallow_copy_rtx (operands[1]);
      XEXP (operands[1], 0) = force_reg (Pmode, XEXP (operands[1], 0));
    }
  operands[3] = force_reg (Pmode, GEN_INT (<fetchop_addr>));
})

(define_insn "sync_new_<fetchop_name>si_internal"
  [(set (match_operand:SI 0 "register_operand" "=q0")
	(unspec:SI
	  [(FETCHOP:SI
	    (mem:SI (match_operand:SI 1 "register_operand" "qA"))
	    (match_operand:SI 2 "register_operand" "q0"))
	   (match_operand:SI 3 "register_no_elim_operand" "a")]
	  UNSPEC_ATOMIC))
   (set (mem:SI (match_dup 1))
	(unspec:SI
	  [(FETCHOP:SI (mem:SI (match_dup 1)) (match_dup 2))
	   (match_dup 3)]
	  UNSPEC_ATOMIC))
   (clobber (match_scratch:SI 4 "=q1"))
   (clobber (reg:SI REG_RETS))]
  "TARGET_SUPPORTS_SYNC_CALLS"
  "call (%3);"
  [(set_attr "type" "call")])

(define_expand "sync_new_<fetchop_name>si"
  [(parallel
    [(set (match_operand:SI 0 "register_operand" "")
	  (unspec:SI
	   [(FETCHOP:SI (match_operand:SI 1 "memory_operand" "")
			(match_operand:SI 2 "register_operand" ""))
	    (match_dup 3)]
	   UNSPEC_ATOMIC))
     (set (match_dup 1)
	  (unspec:SI
	   [(FETCHOP:SI (match_dup 1) (match_dup 2))
	    (match_dup 3)]
	   UNSPEC_ATOMIC))
     (clobber (match_scratch:SI 4 ""))
     (clobber (reg:SI REG_RETS))])]
  "TARGET_SUPPORTS_SYNC_CALLS"
{
  if (!REG_P (XEXP (operands[1], 0)))
    {
      operands[1] = shallow_copy_rtx (operands[1]);
      XEXP (operands[1], 0) = force_reg (Pmode, XEXP (operands[1], 0));
    }
  operands[3] = force_reg (Pmode, GEN_INT (<fetchop_addr>));
})

(define_insn "sync_compare_and_swapsi_internal"
  [(set (match_operand:SI 0 "register_operand" "=q0")
	(mem:SI (match_operand:SI 1 "register_operand" "qA")))
   (set (mem:SI (match_dup 1))
	(unspec:SI
	  [(mem:SI (match_dup 1))
	   (match_operand:SI 2 "register_operand" "q1")
	   (match_operand:SI 3 "register_operand" "q2")
	   (match_operand:SI 4 "register_no_elim_operand" "a")]
	  UNSPEC_ATOMIC))
   (clobber (reg:SI REG_RETS))]
  "TARGET_SUPPORTS_SYNC_CALLS"
  "call (%4);"
  [(set_attr "type" "call")])

(define_expand "sync_compare_and_swapsi"
  [(parallel
    [(set (match_operand:SI 0 "register_operand" "")
	  (match_operand:SI 1 "memory_operand" ""))
     (set (match_dup 1)
	  (unspec:SI
	   [(match_dup 1)
	    (match_operand:SI 2 "register_operand" "")
	    (match_operand:SI 3 "register_operand" "")
	    (match_dup 4)]
	   UNSPEC_ATOMIC))
     (clobber (reg:SI REG_RETS))])]
  "TARGET_SUPPORTS_SYNC_CALLS"
{
  if (!REG_P (XEXP (operands[1], 0)))
    {
      operands[1] = shallow_copy_rtx (operands[1]);
      XEXP (operands[1], 0) = force_reg (Pmode, XEXP (operands[1], 0));
    }
  operands[4] = force_reg (Pmode, GEN_INT (0x420));
})
