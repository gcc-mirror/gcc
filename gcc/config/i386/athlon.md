;; AMD Athlon Scheduling
;; Copyright (C) 2002 Free Software Foundation, Inc.
;;
;; This file is part of GNU CC.
;;
;; GNU CC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; GNU CC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU CC; see the file COPYING.  If not, write to
;; the Free Software Foundation, 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.  */
(define_attr "athlon_decode" "direct,vector"
  (cond [(eq_attr "type" "call,imul,idiv,other,multi,fcmov,fpspc,str,pop,cld,fcmov")
	   (const_string "vector")
         (and (eq_attr "type" "push")
              (match_operand 1 "memory_operand" ""))
	   (const_string "vector")
         (and (eq_attr "type" "fmov")
	      (and (eq_attr "memory" "load,store")
		   (eq_attr "mode" "XF")))
	   (const_string "vector")]
	(const_string "direct")))

;; The Athlon does contain three pipelined FP units, three integer units and
;; three address generation units. 
;;
;; The predecode logic is determining boundaries of instructions in the 64
;; byte cache line. So the cache line straddling problem of K6 might be issue
;; here as well, but it is not noted in the documentation.
;;
;; Three DirectPath instructions decoders and only one VectorPath decoder
;; is available. They can decode three DirectPath instructions or one VectorPath
;; instruction per cycle.
;; Decoded macro instructions are then passed to 72 entry instruction control
;; unit, that passes
;; it to the specialized integer (18 entry) and fp (36 entry) schedulers.
;;
;; The load/store queue unit is not attached to the schedulers but
;; communicates with all the execution units separately instead.

(define_function_unit "athlon_vectordec" 1 0
  (and (eq_attr "cpu" "athlon")
       (eq_attr "athlon_decode" "vector"))
  1 1)

(define_function_unit "athlon_directdec" 3 0
  (and (eq_attr "cpu" "athlon")
       (eq_attr "athlon_decode" "direct"))
  1 1)

(define_function_unit "athlon_vectordec" 1 0
  (and (eq_attr "cpu" "athlon")
       (eq_attr "athlon_decode" "direct"))
  1 1 [(eq_attr "athlon_decode" "vector")])

(define_function_unit "athlon_ieu" 3 0
  (and (eq_attr "cpu" "athlon")
       (eq_attr "type" "alu1,negnot,alu,icmp,test,imov,imovx,lea,incdec,ishift,ishift1,rotate,rotate1,ibr,call,callv,icmov,cld,pop,setcc,push,pop"))
  1 1)

(define_function_unit "athlon_ieu" 3 0
  (and (eq_attr "cpu" "athlon")
       (eq_attr "type" "str"))
  15 15)

(define_function_unit "athlon_ieu" 3 0
  (and (eq_attr "cpu" "athlon")
       (eq_attr "type" "imul"))
  5 0)

(define_function_unit "athlon_ieu" 3 0
  (and (eq_attr "cpu" "athlon")
       (eq_attr "type" "idiv"))
  42 0)

(define_function_unit "athlon_muldiv" 1 0
  (and (eq_attr "cpu" "athlon")
       (eq_attr "type" "imul"))
  5 0)

(define_function_unit "athlon_muldiv" 1 0
  (and (eq_attr "cpu" "athlon")
       (eq_attr "type" "idiv"))
  42 42)

(define_attr "athlon_fpunits" "none,store,mul,add,muladd,any"
  (cond [(eq_attr "type" "fop,fcmp,fistp")
	   (const_string "add")
         (eq_attr "type" "fmul,fdiv,fpspc,fsgn,fcmov")
	   (const_string "mul")
	 (and (eq_attr "type" "fmov") (eq_attr "memory" "store,both"))
	   (const_string "store")
	 (and (eq_attr "type" "fmov") (eq_attr "memory" "load"))
	   (const_string "any")
         (and (eq_attr "type" "fmov")
              (ior (match_operand:SI 1 "register_operand" "")
                   (match_operand 1 "immediate_operand" "")))
	   (const_string "store")
         (eq_attr "type" "fmov")
	   (const_string "muladd")]
	(const_string "none")))

;; We use latencies 1 for definitions.  This is OK to model colisions
;; in execution units.  The real latencies are modeled in the "fp" pipeline.

;; fsin, fcos: 96-192
;; fsincos: 107-211
;; fsqrt: 19 for SFmode, 27 for DFmode, 35 for XFmode.
(define_function_unit "athlon_fp" 3 0
  (and (eq_attr "cpu" "athlon")
       (eq_attr "type" "fpspc"))
  100 1)

;; 16 cycles for SFmode, 20 for DFmode and 24 for XFmode.
(define_function_unit "athlon_fp" 3 0
  (and (eq_attr "cpu" "athlon")
       (eq_attr "type" "fdiv"))
  24 1)

(define_function_unit "athlon_fp" 3 0
  (and (eq_attr "cpu" "athlon")
       (eq_attr "type" "fop,fmul,fistp"))
  4 1)

;; XFmode loads are slow.
;; XFmode store is slow too (8 cycles), but we don't need to model it, because
;; there are no dependent instructions.

(define_function_unit "athlon_fp" 3 0
  (and (eq_attr "cpu" "athlon")
       (and (eq_attr "type" "fmov")
	    (and (eq_attr "memory" "load")
		 (eq_attr "mode" "XF"))))
  10 1)

(define_function_unit "athlon_fp" 3 0
  (and (eq_attr "cpu" "athlon")
       (eq_attr "type" "fmov,fsgn"))
  2 1)

;; fcmp and ftst instructions
(define_function_unit "athlon_fp" 3 0
  (and (eq_attr "cpu" "athlon")
       (and (eq_attr "type" "fcmp")
	    (eq_attr "athlon_decode" "direct")))
  3 1)

;; fcmpi instructions.
(define_function_unit "athlon_fp" 3 0
  (and (eq_attr "cpu" "athlon")
       (and (eq_attr "type" "fcmp")
	    (eq_attr "athlon_decode" "vector")))
  3 1)

(define_function_unit "athlon_fp" 3 0
  (and (eq_attr "cpu" "athlon")
       (eq_attr "type" "fcmov"))
  7 1)

(define_function_unit "athlon_fp_mul" 1 0
  (and (eq_attr "cpu" "athlon")
       (eq_attr "athlon_fpunits" "mul"))
  1 1)

(define_function_unit "athlon_fp_add" 1 0
  (and (eq_attr "cpu" "athlon")
       (eq_attr "athlon_fpunits" "add"))
  1 1)

(define_function_unit "athlon_fp_muladd" 2 0
  (and (eq_attr "cpu" "athlon")
       (eq_attr "athlon_fpunits" "muladd,mul,add"))
  1 1)

(define_function_unit "athlon_fp_store" 1 0
  (and (eq_attr "cpu" "athlon")
       (eq_attr "athlon_fpunits" "store"))
  1 1)

;; We don't need to model the Address Generation Unit, since we don't model
;; the re-order buffer yet and thus we never schedule more than three operations
;; at time.  Later we may want to experiment with MD_SCHED macros modeling the
;; decoders independently on the functional units.

;(define_function_unit "athlon_agu" 3 0
;  (and (eq_attr "cpu" "athlon")
;       (and (eq_attr "memory" "!none")
;            (eq_attr "athlon_fpunits" "none")))
;  1 1)

;; Model load unit to avoid too long sequences of loads.  We don't need to
;; model store queue, since it is hardly going to be bottleneck.

(define_function_unit "athlon_load" 2 0
  (and (eq_attr "cpu" "athlon")
       (eq_attr "memory" "load,both"))
  1 1)

