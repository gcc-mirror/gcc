;; Predicate definitions for Lattice Mico32 architecture.
;; Contributed by Jon Beniston <jon@beniston.com>
;;
;; Copyright (C) 2009-2014 Free Software Foundation, Inc.
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
;;  <http://www.gnu.org/licenses/>.  

(define_predicate "const0_operand"
  (and (match_code "const_int,const_double,const_vector")
       (match_test "op == CONST0_RTX (GET_MODE (op))")))

(define_predicate "constant_K_operand"
  (and (match_code "const_int")
       (match_test "satisfies_constraint_K (op)")))
       
(define_predicate "constant_L_operand"
  (and (match_code "const_int")
       (match_test "satisfies_constraint_L (op)")))

(define_predicate "constant_M_operand"
  (and (match_code "const_int")
       (match_test "satisfies_constraint_M (op)")))

(define_predicate "register_or_zero_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "const0_operand")))

(define_predicate "register_or_K_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "constant_K_operand")))
         
(define_predicate "register_or_L_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "constant_L_operand")))

(define_predicate "register_or_int_operand"
  (ior (match_operand 0 "register_operand")
       (match_code "const_int")))

(define_predicate "reloc_operand"
  (ior (ior (match_code "label_ref")
            (match_code "symbol_ref"))
       (match_code "const")))

(define_predicate "symbolic_operand"
  (ior (match_code "label_ref")
       (match_code "symbol_ref")))
       
(define_predicate "no_pic_small_symbol"
  (match_code "symbol_ref")
{
  return !flag_pic && SYMBOL_REF_SMALL_P (op);
})

(define_predicate "call_operand"
  (ior (match_code "symbol_ref")
       (match_operand 0 "register_operand")))

(define_predicate "movsi_rhs_operand"
  (ior (match_operand 0 "nonimmediate_operand")
       (ior (match_code "const_int")
            (ior (match_test "satisfies_constraint_S (op)")
                 (match_test "satisfies_constraint_Y (op)")))))
