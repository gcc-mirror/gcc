;; Pentium Pro/PII Scheduling
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

;; Categorize how many uops an ia32 instruction evaluates to:
;;   one --  an instruction with 1 uop can be decoded by any of the
;;           three decoders.
;;   few --  an instruction with 1 to 4 uops can be decoded only by 
;;	     decoder 0.
;;   many -- a complex instruction may take an unspecified number of
;;	     cycles to decode in decoder 0.

(define_attr "ppro_uops" "one,few,many"
  (cond [(eq_attr "type" "other,multi,call,callv,fpspc,str")
	   (const_string "many")
	 (eq_attr "type" "icmov,fcmov,str,cld")
	   (const_string "few")
	 (eq_attr "type" "imov")
	   (if_then_else (eq_attr "memory" "store,both")
	     (const_string "few")
	     (const_string "one"))
	 (eq_attr "memory" "!none")
	   (const_string "few")
	]
	(const_string "one")))

;;
;; The PPro has an out-of-order core, but the instruction decoders are
;; naturally in-order and asymmetric.  We get best performance by scheduling
;; for the decoders, for in doing so we give the oo execution unit the 
;; most choices.
;;
;; Rough readiness numbers.  Fine tuning happens in i386.c.
;;
;; p0	describes port 0.
;; p01	describes ports 0 and 1 as a pair; alu insns can issue to either.
;; p2	describes port 2 for loads.
;; p34	describes ports 3 and 4 for stores.
;; fpu	describes the fpu accessed via port 0. 
;;	??? It is less than clear if there are separate fadd and fmul units
;;	that could operate in parallel.
;;
;; ??? fxch isn't handled; not an issue until sched3 after reg-stack is real.

(define_function_unit "ppro_p0" 1 0
  (and (eq_attr "cpu" "pentiumpro")
       (eq_attr "type" "ishift,rotate,ishift1,rotate1,lea,ibr,cld"))
  1 1)

(define_function_unit "ppro_p0" 1 0
  (and (eq_attr "cpu" "pentiumpro")
       (eq_attr "type" "imul"))
  4 1)

;; ??? Does the divider lock out the pipe while it works,
;; or is there a disconnected unit?
(define_function_unit "ppro_p0" 1 0
  (and (eq_attr "cpu" "pentiumpro")
       (eq_attr "type" "idiv"))
  17 17)

(define_function_unit "ppro_p0" 1 0
  (and (eq_attr "cpu" "pentiumpro")
       (eq_attr "type" "fop,fsgn,fistp"))
  3 1)

(define_function_unit "ppro_p0" 1 0
  (and (eq_attr "cpu" "pentiumpro")
       (eq_attr "type" "fcmov"))
  2 1)

(define_function_unit "ppro_p0" 1 0
  (and (eq_attr "cpu" "pentiumpro")
       (eq_attr "type" "fcmp"))
  1 1)

(define_function_unit "ppro_p0" 1 0
  (and (eq_attr "cpu" "pentiumpro")
       (eq_attr "type" "fmov"))
  1 1)

(define_function_unit "ppro_p0" 1 0
  (and (eq_attr "cpu" "pentiumpro")
       (eq_attr "type" "fmul"))
  5 1)

(define_function_unit "ppro_p0" 1 0
  (and (eq_attr "cpu" "pentiumpro")
       (eq_attr "type" "fdiv,fpspc"))
  56 1)

(define_function_unit "ppro_p01" 2 0
  (and (eq_attr "cpu" "pentiumpro")
       (eq_attr "type" "!imov,fmov"))
  1 1)

(define_function_unit "ppro_p01" 2 0
  (and (and (eq_attr "cpu" "pentiumpro")
            (eq_attr "type" "imov,fmov"))
       (eq_attr "memory" "none"))
  1 1)

(define_function_unit "ppro_p2" 1 0
  (and (eq_attr "cpu" "pentiumpro")
       (ior (eq_attr "type" "pop")
	    (eq_attr "memory" "load,both")))
  3 1)

(define_function_unit "ppro_p34" 1 0
  (and (eq_attr "cpu" "pentiumpro")
       (ior (eq_attr "type" "push")
	    (eq_attr "memory" "store,both")))
  1 1)

(define_function_unit "fpu" 1 0
  (and (eq_attr "cpu" "pentiumpro")
       (eq_attr "type" "fop,fsgn,fmov,fcmp,fcmov,fistp"))
  1 1)

(define_function_unit "fpu" 1 0
  (and (eq_attr "cpu" "pentiumpro")
       (eq_attr "type" "fmul"))
  5 2)

(define_function_unit "fpu" 1 0
  (and (eq_attr "cpu" "pentiumpro")
       (eq_attr "type" "fdiv,fpspc"))
  56 56)

;; imul uses the fpu.  ??? does it have the same throughput as fmul?
(define_function_unit "fpu" 1 0
  (and (eq_attr "cpu" "pentiumpro")
       (eq_attr "type" "imul"))
  4 1)
