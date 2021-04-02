;; Predicate definitions for DEC VAX.
;; Copyright (C) 2007-2021 Free Software Foundation, Inc.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; GCC is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;; Return true if OP is a constant zero operand.
(define_predicate "const_zero_operand"
  (match_test "op == CONST0_RTX (mode)"))

;; Special case of a symbolic operand that's used as a
;; operand.

(define_predicate "symbolic_operand"
  (match_code "const,symbol_ref,label_ref"))

(define_predicate "pic_symbolic_operand"
  (and (match_code "const,symbol_ref,label_ref")
       (match_test "!flag_pic
		    || vax_acceptable_pic_operand_p (op, false, true)")))

(define_predicate "nonsymbolic_operand"
  (and (ior (match_test "!flag_pic")
	    (not (match_operand 0 "symbolic_operand")))
       (match_operand 0 "general_operand" "")))

(define_predicate "non_pic_external_memory_operand"
   (match_code "mem")
{
  rtx addr = XEXP (op, 0);
  if (MEM_P (addr))
    addr = XEXP (addr, 0);
  if (GET_CODE (addr) == PLUS)
    addr = XEXP (addr, 1);
  if (MEM_P (addr))
    addr = XEXP (addr, 0);
  if (GET_CODE (addr) == PLUS)
    addr = XEXP (addr, 1);
  return (symbolic_operand (addr, SImode)
	  && !vax_acceptable_pic_operand_p (addr, true, true));
})

(define_predicate "indirect_memory_operand"
   (match_code "mem")
{
  op = XEXP (op, 0);
  if (MEM_P (op))
    return 1;
  if (GET_CODE (op) == PLUS)
    op = XEXP (op, 1);
  return MEM_P (op);
})

(define_predicate "indexed_memory_operand"
   (match_code "mem")
{
  rtx addr = XEXP (op, 0);
  return GET_CODE (addr) != PRE_DEC && GET_CODE (addr) != POST_INC
	 && mode_dependent_address_p (addr, MEM_ADDR_SPACE (op));
})

(define_predicate "illegal_blk_memory_operand"
   (and (match_code "mem")
	(ior (and (match_test "flag_pic")
		  (match_operand 0 "non_pic_external_memory_operand" ""))
	     (ior (match_operand 0 "indexed_memory_operand" "")
		  (ior (match_operand 0 "indirect_memory_operand" "")
		       (match_test "GET_CODE (XEXP (op, 0)) == PRE_DEC"))))))

(define_predicate "illegal_addsub_di_memory_operand"
   (and (match_code "mem")
	(ior (and (match_test "flag_pic")
		  (match_operand 0 "non_pic_external_memory_operand" ""))
	     (ior (match_operand 0 "indexed_memory_operand" "")
		  (ior (match_operand 0 "indirect_memory_operand" "")
		       (match_test "GET_CODE (XEXP (op, 0)) == PRE_DEC"))))))

(define_predicate "nonimmediate_addsub_di_operand"
   (and (match_code "subreg,reg,mem")
	(and (match_operand:DI 0 "nonimmediate_operand" "")
	     (not (match_operand:DI 0 "illegal_addsub_di_memory_operand")))))

(define_predicate "general_addsub_di_operand"
   (and (match_code "const_int,const_double,subreg,reg,mem")
	(and (match_operand:DI 0 "general_operand" "")
	     (not (match_operand:DI 0 "illegal_addsub_di_memory_operand")))))

;; Return 1 if the operand is in volatile memory.  Note that during the
;; RTL generation phase, `memory_operand' does not return TRUE for
;; volatile memory references.  So this function allows us to recognize
;; volatile references where it's safe.
(define_predicate "volatile_mem_operand"
  (and (match_code "mem")
       (match_test "MEM_VOLATILE_P (op)")
       (if_then_else (match_test "reload_completed")
	 (match_operand 0 "memory_operand")
	 (match_test "memory_address_p (mode, XEXP (op, 0))"))))

;; Return 1 if the operand is a volatile or non-volatile memory operand.
(define_predicate "any_memory_operand"
  (ior (match_operand 0 "memory_operand")
       (match_operand 0 "volatile_mem_operand")))

;; Return true if OP is a comparison operator that requires at least CCmode.
(define_predicate "vax_cc_comparison_operator"
  (match_code "geu,gtu,leu,ltu"))

;; Return true if OP is a comparison operator that requires at least CCNmode.
(define_predicate "vax_ccn_comparison_operator"
  (match_code "ge,lt"))

;; Return true if OP is a comparison operator that requires at least CCNZmode.
(define_predicate "vax_ccnz_comparison_operator"
  (match_code "gt,le"))

;; Return true if OP is a comparison operator that requires at least CCZmode.
(define_predicate "vax_ccz_comparison_operator"
  (match_code "ne,eq"))
