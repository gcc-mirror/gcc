;; Predicate definitions for DEC VAX.
;; Copyright (C) 2007, 2009 Free Software Foundation, Inc.
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

;; Special case of a symbolic operand that's used as a
;; operand.

(define_predicate "symbolic_operand"
  (match_code "const,symbol_ref,label_ref"))

(define_predicate "local_symbolic_operand"
  (match_code "const,symbol_ref,label_ref")
{
  if (GET_CODE (op) == LABEL_REF)
    return 1;
  if (GET_CODE (op) == SYMBOL_REF)
    return !flag_pic || SYMBOL_REF_LOCAL_P (op);
  if (GET_CODE (XEXP (XEXP (op, 0), 0)) == LABEL_REF)
    return 1;
  return !flag_pic || SYMBOL_REF_LOCAL_P (XEXP (XEXP (op, 0), 0));
})

(define_predicate "external_symbolic_operand"
  (and (match_code "symbol_ref")
       (not (match_operand 0 "local_symbolic_operand" ""))))

(define_predicate "external_const_operand"
  (and (match_code "const")
       (match_test "GET_CODE (XEXP (XEXP (op, 0), 0)) == SYMBOL_REF
		    && !SYMBOL_REF_LOCAL_P (XEXP (XEXP (op, 0), 0))")))

(define_predicate "nonsymbolic_operand"
  (and (ior (match_test "!flag_pic")
	    (not (match_operand 0 "symbolic_operand")))
       (match_operand 0 "general_operand" "")))

(define_predicate "external_memory_operand"
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
  return external_symbolic_operand (addr, SImode)
	 || external_const_operand (addr, SImode);
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
  op = XEXP (op, 0);
  return GET_CODE (op) != PRE_DEC && GET_CODE (op) != POST_INC
	 && mode_dependent_address_p (op);
})

(define_predicate "illegal_blk_memory_operand"
   (and (match_code "mem")
	(ior (and (match_test "flag_pic")
		  (match_operand 0 "external_memory_operand" ""))
	     (ior (match_operand 0 "indexed_memory_operand" "")
		  (ior (match_operand 0 "indirect_memory_operand" "")
		       (match_test "GET_CODE (XEXP (op, 0)) == PRE_DEC"))))))

(define_predicate "illegal_addsub_di_memory_operand"
   (and (match_code "mem")
	(ior (and (match_test "flag_pic")
		  (match_operand 0 "external_memory_operand" ""))
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
