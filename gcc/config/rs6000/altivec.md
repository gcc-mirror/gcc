;; AltiVec patterns.
;; Copyright (C) 2002, 2003 Free Software Foundation, Inc.
;; Contributed by Aldy Hernandez (aldy@quesejoda.com)

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING.  If not, write to the
;; Free Software Foundation, 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

(define_constants
  [(UNSPEC_VSPLTISW	141)
   (UNSPEC_VSPLTISH	140)
   (UNSPEC_VSPLTISB	139)
   ])

;; Generic LVX load instruction.
(define_insn "altivec_lvx_4si"
  [(set (match_operand:V4SI 0 "altivec_register_operand" "=v")
	(match_operand:V4SI 1 "memory_operand" "m"))]
  "TARGET_ALTIVEC"
  "lvx %0,%y1"
  [(set_attr "type" "vecload")])

(define_insn "altivec_lvx_8hi"
  [(set (match_operand:V8HI 0 "altivec_register_operand" "=v")
	(match_operand:V8HI 1 "memory_operand" "m"))]
  "TARGET_ALTIVEC"
  "lvx %0,%y1"
  [(set_attr "type" "vecload")])

(define_insn "altivec_lvx_16qi"
  [(set (match_operand:V16QI 0 "altivec_register_operand" "=v")
	(match_operand:V16QI 1 "memory_operand" "m"))]
  "TARGET_ALTIVEC"
  "lvx %0,%y1"
  [(set_attr "type" "vecload")])

(define_insn "altivec_lvx_4sf"
  [(set (match_operand:V4SF 0 "altivec_register_operand" "=v")
	(match_operand:V4SF 1 "memory_operand" "m"))]
  "TARGET_ALTIVEC"
  "lvx %0,%y1"
  [(set_attr "type" "vecload")])

;; Generic STVX store instruction.
(define_insn "altivec_stvx_4si"
  [(set (match_operand:V4SI 0 "memory_operand" "=m")
	(match_operand:V4SI 1 "altivec_register_operand" "v"))]
  "TARGET_ALTIVEC"
  "stvx %1,%y0"
  [(set_attr "type" "vecstore")])

(define_insn "altivec_stvx_8hi"
  [(set (match_operand:V8HI 0 "memory_operand" "=m")
	(match_operand:V8HI 1 "altivec_register_operand" "v"))]
  "TARGET_ALTIVEC"
  "stvx %1,%y0"
  [(set_attr "type" "vecstore")])

(define_insn "altivec_stvx_16qi"
  [(set (match_operand:V16QI 0 "memory_operand" "=m")
	(match_operand:V16QI 1 "altivec_register_operand" "v"))]
  "TARGET_ALTIVEC"
  "stvx %1,%y0"
  [(set_attr "type" "vecstore")])

(define_insn "altivec_stvx_4sf"
  [(set (match_operand:V4SF 0 "memory_operand" "=m")
	(match_operand:V4SF 1 "altivec_register_operand" "v"))]
  "TARGET_ALTIVEC"
  "stvx %1,%y0"
  [(set_attr "type" "vecstore")])

;; Vector move instructions.
(define_expand "movv4si"
  [(set (match_operand:V4SI 0 "nonimmediate_operand" "")
	(match_operand:V4SI 1 "any_operand" ""))]
  "TARGET_ALTIVEC"
  "{ rs6000_emit_move (operands[0], operands[1], V4SImode); DONE; }")

(define_insn "*movv4si_internal"
  [(set (match_operand:V4SI 0 "nonimmediate_operand" "=m,v,v,o,r,r,v")
	(match_operand:V4SI 1 "input_operand" "v,m,v,r,o,r,W"))]
  "TARGET_ALTIVEC 
   && (register_operand (operands[0], V4SImode) 
       || register_operand (operands[1], V4SImode))"
  "*
{
  switch (which_alternative)
    {
    case 0: return \"stvx %1,%y0\";
    case 1: return \"lvx %0,%y1\";
    case 2: return \"vor %0,%1,%1\";
    case 3: return \"#\";
    case 4: return \"#\";
    case 5: return \"#\";
    case 6: return output_vec_const_move (operands);
    default: abort();
    }
}"
  [(set_attr "type" "vecstore,vecload,vecsimple,store,load,*,*")])

(define_split
  [(set (match_operand:V4SI 0 "nonimmediate_operand" "")
        (match_operand:V4SI 1 "input_operand" ""))]
  "TARGET_ALTIVEC && reload_completed
   && gpr_or_gpr_p (operands[0], operands[1])"
  [(pc)]
{ rs6000_split_multireg_move (operands[0], operands[1]); DONE; })

(define_split
  [(set (match_operand:V4SI 0 "altivec_register_operand" "")
	(match_operand:V4SI 1 "easy_vector_constant_add_self" ""))]
  "TARGET_ALTIVEC && reload_completed"
  [(set (match_dup 0) (match_dup 3))
   (set (match_dup 0)
	(plus:V4SI (match_dup 0)
		   (match_dup 0)))]
  "
{ 
  operands[3] = gen_easy_vector_constant_add_self (operands[1]);
}")    

(define_expand "movv8hi"
  [(set (match_operand:V8HI 0 "nonimmediate_operand" "")
	(match_operand:V8HI 1 "any_operand" ""))]
  "TARGET_ALTIVEC"
  "{ rs6000_emit_move (operands[0], operands[1], V8HImode); DONE; }")

(define_insn "*movv8hi_internal1"
  [(set (match_operand:V8HI 0 "nonimmediate_operand" "=m,v,v,o,r,r,v")
	(match_operand:V8HI 1 "input_operand" "v,m,v,r,o,r,W"))]
  "TARGET_ALTIVEC 
   && (register_operand (operands[0], V8HImode) 
       || register_operand (operands[1], V8HImode))"
  "*
{
   switch (which_alternative)
     {
     case 0: return \"stvx %1,%y0\";
     case 1: return \"lvx %0,%y1\";
     case 2: return \"vor %0,%1,%1\";
     case 3: return \"#\";
     case 4: return \"#\";
     case 5: return \"#\";
     case 6: return output_vec_const_move (operands);
     default: abort ();
     }
}"
  [(set_attr "type" "vecstore,vecload,vecsimple,store,load,*,*")])

(define_split
  [(set (match_operand:V8HI 0 "nonimmediate_operand" "")
        (match_operand:V8HI 1 "input_operand" ""))]
  "TARGET_ALTIVEC && reload_completed
   && gpr_or_gpr_p (operands[0], operands[1])"
  [(pc)]
{ rs6000_split_multireg_move (operands[0], operands[1]); DONE; })

(define_split
  [(set (match_operand:V8HI 0 "altivec_register_operand" "")
	(match_operand:V8HI 1 "easy_vector_constant_add_self" ""))]
  "TARGET_ALTIVEC && reload_completed"
  [(set (match_dup 0) (match_dup 3))
   (set (match_dup 0)
	(plus:V8HI (match_dup 0)
		   (match_dup 0)))]
  "
{
  operands[3] = gen_easy_vector_constant_add_self (operands[1]);
}")

(define_expand "movv16qi"
  [(set (match_operand:V16QI 0 "nonimmediate_operand" "")
	(match_operand:V16QI 1 "any_operand" ""))]
  "TARGET_ALTIVEC"
  "{ rs6000_emit_move (operands[0], operands[1], V16QImode); DONE; }")

(define_insn "*movv16qi_internal1"
  [(set (match_operand:V16QI 0 "nonimmediate_operand" "=m,v,v,o,r,r,v")
	(match_operand:V16QI 1 "input_operand" "v,m,v,r,o,r,W"))]
  "TARGET_ALTIVEC
   && (register_operand (operands[0], V16QImode)
       || register_operand (operands[1], V16QImode))"
  "*
{
  switch (which_alternative)
    {
    case 0: return \"stvx %1,%y0\";
    case 1: return \"lvx %0,%y1\";
    case 2: return \"vor %0,%1,%1\";
    case 3: return \"#\";
    case 4: return \"#\";
    case 5: return \"#\";
    case 6: return output_vec_const_move (operands);
    default: abort ();
    }
}"
  [(set_attr "type" "vecstore,vecload,vecsimple,store,load,*,*")])

(define_split
  [(set (match_operand:V16QI 0 "nonimmediate_operand" "")
        (match_operand:V16QI 1 "input_operand" ""))]
  "TARGET_ALTIVEC && reload_completed
   && gpr_or_gpr_p (operands[0], operands[1])"
  [(pc)]
{ rs6000_split_multireg_move (operands[0], operands[1]); DONE; })

(define_split
  [(set (match_operand:V16QI 0 "altivec_register_operand" "")
	(match_operand:V16QI 1 "easy_vector_constant_add_self" ""))]
  "TARGET_ALTIVEC && reload_completed"
  [(set (match_dup 0) (match_dup 3))
   (set (match_dup 0)
	(plus:V16QI (match_dup 0)
		   (match_dup 0)))]
  "
{
  operands[3] = gen_easy_vector_constant_add_self (operands[1]);
}")

(define_expand "movv4sf"
  [(set (match_operand:V4SF 0 "nonimmediate_operand" "")
	(match_operand:V4SF 1 "any_operand" ""))]
  "TARGET_ALTIVEC"
  "{ rs6000_emit_move (operands[0], operands[1], V4SFmode); DONE; }")

(define_insn "*movv4sf_internal1"
  [(set (match_operand:V4SF 0 "nonimmediate_operand" "=m,v,v,o,r,r,v")
	(match_operand:V4SF 1 "input_operand" "v,m,v,r,o,r,W"))]
  "TARGET_ALTIVEC
   && (register_operand (operands[0], V4SFmode)
       || register_operand (operands[1], V4SFmode))"
  "*
{
  switch (which_alternative)
    {
    case 0: return \"stvx %1,%y0\";
    case 1: return \"lvx %0,%y1\";
    case 2: return \"vor %0,%1,%1\";
    case 3: return \"#\";
    case 4: return \"#\";
    case 5: return \"#\";
    case 6: return output_vec_const_move (operands);
    default: abort ();
    }
}"
  [(set_attr "type" "vecstore,vecload,vecsimple,store,load,*,*")])

(define_split
  [(set (match_operand:V4SF 0 "nonimmediate_operand" "")
        (match_operand:V4SF 1 "input_operand" ""))]
  "TARGET_ALTIVEC && reload_completed
   && gpr_or_gpr_p (operands[0], operands[1])"
  [(pc)]
{ rs6000_split_multireg_move (operands[0], operands[1]); DONE; })

(define_insn "get_vrsave_internal"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI [(reg:SI 109)] 214))]
  "TARGET_ALTIVEC"
  "*
{
  if (TARGET_MACHO)
     return \"mfspr %0,256\";
  else
     return \"mfvrsave %0\";
}"
  [(set_attr "type" "*")])

(define_insn "*set_vrsave_internal"
  [(match_parallel 0 "vrsave_operation"
     [(set (reg:SI 109)
	   (unspec_volatile:SI [(match_operand:SI 1 "register_operand" "r")
				(reg:SI 109)] 30))])]
  "TARGET_ALTIVEC"
  "*
{
  if (TARGET_MACHO)
    return \"mtspr 256,%1\";
  else
    return \"mtvrsave %1\";
}"
  [(set_attr "type" "*")])

;; Simple binary operations.

(define_insn "addv16qi3"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (plus:V16QI (match_operand:V16QI 1 "register_operand" "v")
                    (match_operand:V16QI 2 "register_operand" "v")))]
  "TARGET_ALTIVEC"
  "vaddubm %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "addv8hi3"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (plus:V8HI (match_operand:V8HI 1 "register_operand" "v")
                   (match_operand:V8HI 2 "register_operand" "v")))]
  "TARGET_ALTIVEC"
  "vadduhm %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "addv4si3"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (plus:V4SI (match_operand:V4SI 1 "register_operand" "v")
                   (match_operand:V4SI 2 "register_operand" "v")))]
  "TARGET_ALTIVEC"
  "vadduwm %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "addv4sf3"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
        (plus:V4SF (match_operand:V4SF 1 "register_operand" "v")
	 	   (match_operand:V4SF 2 "register_operand" "v")))]
  "TARGET_ALTIVEC"
  "vaddfp %0,%1,%2"
  [(set_attr "type" "vecfloat")])

(define_insn "altivec_vaddcuw"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")] 35))]
  "TARGET_ALTIVEC"
  "vaddcuw %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vaddubs"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (unspec:V16QI [(match_operand:V16QI 1 "register_operand" "v")
                       (match_operand:V16QI 2 "register_operand" "v")] 36))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] 213))]
  "TARGET_ALTIVEC"
  "vaddubs %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vaddsbs"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (unspec:V16QI [(match_operand:V16QI 1 "register_operand" "v")
                       (match_operand:V16QI 2 "register_operand" "v")] 37))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] 213))]
  "TARGET_ALTIVEC"
  "vaddsbs %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vadduhs"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V8HI 1 "register_operand" "v")
                      (match_operand:V8HI 2 "register_operand" "v")] 38))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] 213))]
  "TARGET_ALTIVEC"
  "vadduhs %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vaddshs"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V8HI 1 "register_operand" "v")
                      (match_operand:V8HI 2 "register_operand" "v")] 39))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] 213))]
  "TARGET_ALTIVEC"
  "vaddshs %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vadduws"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")] 40))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] 213))]
  "TARGET_ALTIVEC"
  "vadduws %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vaddsws"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")] 41))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] 213))]
  "TARGET_ALTIVEC"
  "vaddsws %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "andv4si3"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (and:V4SI (match_operand:V4SI 1 "register_operand" "v")
                  (match_operand:V4SI 2 "register_operand" "v")))]
  "TARGET_ALTIVEC"
  "vand %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vandc"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (and:V4SI (match_operand:V4SI 1 "register_operand" "v")
                  (not:V4SI (match_operand:V4SI 2 "register_operand" "v"))))]
  "TARGET_ALTIVEC"
  "vandc %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vavgub"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (unspec:V16QI [(match_operand:V16QI 1 "register_operand" "v")
                       (match_operand:V16QI 2 "register_operand" "v")] 44))]
  "TARGET_ALTIVEC"
  "vavgub %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vavgsb"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (unspec:V16QI [(match_operand:V16QI 1 "register_operand" "v")
                       (match_operand:V16QI 2 "register_operand" "v")] 45))]
  "TARGET_ALTIVEC"
  "vavgsb %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vavguh"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V8HI 1 "register_operand" "v")
                      (match_operand:V8HI 2 "register_operand" "v")] 46))]
  "TARGET_ALTIVEC"
  "vavguh %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vavgsh"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V8HI 1 "register_operand" "v")
                      (match_operand:V8HI 2 "register_operand" "v")] 47))]
  "TARGET_ALTIVEC"
  "vavgsh %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vavguw"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")] 48))]
  "TARGET_ALTIVEC"
  "vavguw %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vavgsw"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")] 49))]
  "TARGET_ALTIVEC"
  "vavgsw %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vcmpbfp"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SF 1 "register_operand" "v")
                      (match_operand:V4SF 2 "register_operand" "v")] 50))]
  "TARGET_ALTIVEC"
  "vcmpbfp %0,%1,%2"
  [(set_attr "type" "veccmp")])

(define_insn "altivec_vcmpequb"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (unspec:V16QI [(match_operand:V16QI 1 "register_operand" "v")
                       (match_operand:V16QI 2 "register_operand" "v")] 51))]
  "TARGET_ALTIVEC"
  "vcmpequb %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vcmpequh"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V8HI 1 "register_operand" "v")
                      (match_operand:V8HI 2 "register_operand" "v")] 52))]
  "TARGET_ALTIVEC"
  "vcmpequh %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vcmpequw"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")] 53))]
  "TARGET_ALTIVEC"
  "vcmpequw %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vcmpeqfp"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SF 1 "register_operand" "v")
                      (match_operand:V4SF 2 "register_operand" "v")] 54))]
  "TARGET_ALTIVEC"
  "vcmpeqfp %0,%1,%2"
  [(set_attr "type" "veccmp")])

(define_insn "altivec_vcmpgefp"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SF 1 "register_operand" "v")
                      (match_operand:V4SF 2 "register_operand" "v")] 55))]
  "TARGET_ALTIVEC"
  "vcmpgefp %0,%1,%2"
  [(set_attr "type" "veccmp")])

(define_insn "altivec_vcmpgtub"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (unspec:V16QI [(match_operand:V16QI 1 "register_operand" "v")
                       (match_operand:V16QI 2 "register_operand" "v")] 56))]
  "TARGET_ALTIVEC"
  "vcmpgtub %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vcmpgtsb"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (unspec:V16QI [(match_operand:V16QI 1 "register_operand" "v")
                       (match_operand:V16QI 2 "register_operand" "v")] 57))]
  "TARGET_ALTIVEC"
  "vcmpgtsb %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vcmpgtuh"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V8HI 1 "register_operand" "v")
                      (match_operand:V8HI 2 "register_operand" "v")] 58))]
  "TARGET_ALTIVEC"
  "vcmpgtuh %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vcmpgtsh"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V8HI 1 "register_operand" "v")
                      (match_operand:V8HI 2 "register_operand" "v")] 59))]
  "TARGET_ALTIVEC"
  "vcmpgtsh %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vcmpgtuw"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")] 60))]
  "TARGET_ALTIVEC"
  "vcmpgtuw %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vcmpgtsw"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")] 61))]
  "TARGET_ALTIVEC"
  "vcmpgtsw %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vcmpgtfp"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SF 1 "register_operand" "v")
                      (match_operand:V4SF 2 "register_operand" "v")] 62))]
  "TARGET_ALTIVEC"
  "vcmpgtfp %0,%1,%2"
  [(set_attr "type" "veccmp")])

;; Fused multiply add
(define_insn "altivec_vmaddfp"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
	(plus:V4SF (mult:V4SF (match_operand:V4SF 1 "register_operand" "v")
			      (match_operand:V4SF 2 "register_operand" "v"))
	  	   (match_operand:V4SF 3 "register_operand" "v")))]
  "TARGET_ALTIVEC"
  "vmaddfp %0,%1,%2,%3"
  [(set_attr "type" "vecfloat")])

;; We do multiply as a fused multiply-add with an add of a -0.0 vector.

(define_expand "mulv4sf3"
  [(use (match_operand:V4SF 0 "register_operand" ""))
   (use (match_operand:V4SF 1 "register_operand" ""))
   (use (match_operand:V4SF 2 "register_operand" ""))]
  "TARGET_ALTIVEC && TARGET_FUSED_MADD"
  "
{
  rtx neg0;

  /* Generate [-0.0, -0.0, -0.0, -0.0].  */
  neg0 = gen_reg_rtx (V4SFmode);
  emit_insn (gen_altivec_vspltisw_v4sf (neg0, GEN_INT (-1)));
  emit_insn (gen_altivec_vslw_v4sf (neg0, neg0, neg0));

  /* Use the multiply-add.  */
  emit_insn (gen_altivec_vmaddfp (operands[0], operands[1], operands[2],
				  neg0));
  DONE;
}")

;; Fused multiply subtract 
(define_insn "altivec_vnmsubfp"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
	(minus:V4SF (mult:V4SF (match_operand:V4SF 1 "register_operand" "v")
			       (match_operand:V4SF 2 "register_operand" "v"))
	  	    (match_operand:V4SF 3 "register_operand" "v")))]
  "TARGET_ALTIVEC"
  "vnmsubfp %0,%1,%2,%3"
  [(set_attr "type" "vecfloat")])


(define_insn "altivec_vmsumubm"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V16QI 1 "register_operand" "v")
		      (match_operand:V16QI 2 "register_operand" "v")
                      (match_operand:V4SI 3 "register_operand" "v")] 65))]
  "TARGET_ALTIVEC"
  "vmsumubm %0,%1,%2,%3"
  [(set_attr "type" "veccomplex")])

(define_insn "altivec_vmsummbm"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V16QI 1 "register_operand" "v")
		      (match_operand:V16QI 2 "register_operand" "v")
                      (match_operand:V4SI 3 "register_operand" "v")] 66))]
  "TARGET_ALTIVEC"
  "vmsummbm %0,%1,%2,%3"
  [(set_attr "type" "veccomplex")])

(define_insn "altivec_vmsumuhm"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V8HI 1 "register_operand" "v")
		      (match_operand:V8HI 2 "register_operand" "v")
                      (match_operand:V4SI 3 "register_operand" "v")] 67))]
  "TARGET_ALTIVEC"
  "vmsumuhm %0,%1,%2,%3"
  [(set_attr "type" "veccomplex")])

(define_insn "altivec_vmsumshm"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V8HI 1 "register_operand" "v")
		      (match_operand:V8HI 2 "register_operand" "v")
                      (match_operand:V4SI 3 "register_operand" "v")] 68))]
  "TARGET_ALTIVEC"
  "vmsumshm %0,%1,%2,%3"
  [(set_attr "type" "veccomplex")])

(define_insn "altivec_vmsumuhs"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V8HI 1 "register_operand" "v")
		      (match_operand:V8HI 2 "register_operand" "v")
                      (match_operand:V4SI 3 "register_operand" "v")] 69))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] 213))]
  "TARGET_ALTIVEC"
  "vmsumuhs %0,%1,%2,%3"
  [(set_attr "type" "veccomplex")])

(define_insn "altivec_vmsumshs"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V8HI 1 "register_operand" "v")
		      (match_operand:V8HI 2 "register_operand" "v")
                      (match_operand:V4SI 3 "register_operand" "v")] 70))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] 213))]
  "TARGET_ALTIVEC"
  "vmsumshs %0,%1,%2,%3"
  [(set_attr "type" "veccomplex")])

(define_insn "umaxv16qi3"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (umax:V16QI (match_operand:V16QI 1 "register_operand" "v")
                    (match_operand:V16QI 2 "register_operand" "v")))]
  "TARGET_ALTIVEC"
  "vmaxub %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "smaxv16qi3"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (smax:V16QI (match_operand:V16QI 1 "register_operand" "v")
                    (match_operand:V16QI 2 "register_operand" "v")))]
  "TARGET_ALTIVEC"
  "vmaxsb %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "umaxv8hi3"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (umax:V8HI (match_operand:V8HI 1 "register_operand" "v")
                   (match_operand:V8HI 2 "register_operand" "v")))]
  "TARGET_ALTIVEC"
  "vmaxuh %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "smaxv8hi3"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (smax:V8HI (match_operand:V8HI 1 "register_operand" "v")
                   (match_operand:V8HI 2 "register_operand" "v")))]
  "TARGET_ALTIVEC"
  "vmaxsh %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "umaxv4si3"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (umax:V4SI (match_operand:V4SI 1 "register_operand" "v")
                   (match_operand:V4SI 2 "register_operand" "v")))]
  "TARGET_ALTIVEC"
  "vmaxuw %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "smaxv4si3"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (smax:V4SI (match_operand:V4SI 1 "register_operand" "v")
                   (match_operand:V4SI 2 "register_operand" "v")))]
  "TARGET_ALTIVEC"
  "vmaxsw %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "smaxv4sf3"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
        (smax:V4SF (match_operand:V4SF 1 "register_operand" "v")
                   (match_operand:V4SF 2 "register_operand" "v")))]
  "TARGET_ALTIVEC"
  "vmaxfp %0,%1,%2"
  [(set_attr "type" "veccmp")])

(define_insn "altivec_vmhaddshs"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V8HI 1 "register_operand" "v")
		      (match_operand:V8HI 2 "register_operand" "v")
                      (match_operand:V8HI 3 "register_operand" "v")] 71))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] 213))]
  "TARGET_ALTIVEC"
  "vmhaddshs %0,%1,%2,%3"
  [(set_attr "type" "veccomplex")])
(define_insn "altivec_vmhraddshs"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V8HI 1 "register_operand" "v")
		      (match_operand:V8HI 2 "register_operand" "v")
                      (match_operand:V8HI 3 "register_operand" "v")] 72))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] 213))]
  "TARGET_ALTIVEC"
  "vmhraddshs %0,%1,%2,%3"
  [(set_attr "type" "veccomplex")])
(define_insn "altivec_vmladduhm"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V8HI 1 "register_operand" "v")
		      (match_operand:V8HI 2 "register_operand" "v")
                      (match_operand:V8HI 3 "register_operand" "v")] 73))]
  "TARGET_ALTIVEC"
  "vmladduhm %0,%1,%2,%3"
  [(set_attr "type" "veccomplex")])

(define_insn "altivec_vmrghb"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (vec_merge:V16QI (vec_select:V16QI (match_operand:V16QI 1 "register_operand" "v")
					   (parallel [(const_int 8)
					   	      (const_int 9)
					   	      (const_int 10)
					   	      (const_int 11)
					   	      (const_int 12)
					   	      (const_int 13)
						      (const_int 14)
						      (const_int 15)
					   	      (const_int 0)
					   	      (const_int 1)
					   	      (const_int 2)
					   	      (const_int 3)
					   	      (const_int 4)
					   	      (const_int 5)
					   	      (const_int 6)
						      (const_int 7)]))
                      (match_operand:V16QI 2 "register_operand" "v")
		      (const_int 255)))]
  "TARGET_ALTIVEC"
  "vmrghb %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vmrghh"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (vec_merge:V8HI (vec_select:V8HI (match_operand:V8HI 1 "register_operand" "v")
					   (parallel [(const_int 4)
					   	      (const_int 5)
					   	      (const_int 6)
					   	      (const_int 7)
					   	      (const_int 0)
					   	      (const_int 1)
					   	      (const_int 2)
					   	      (const_int 3)]))
                      (match_operand:V8HI 2 "register_operand" "v")
		      (const_int 15)))]
  "TARGET_ALTIVEC"
  "vmrghh %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vmrghw"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (vec_merge:V4SI (vec_select:V4SI (match_operand:V4SI 1 "register_operand" "v")
					 (parallel [(const_int 2)
					 	    (const_int 3)
						    (const_int 0)
						    (const_int 1)]))
                      (match_operand:V4SI 2 "register_operand" "v")
		      (const_int 12)))]
  "TARGET_ALTIVEC"
  "vmrghw %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vmrglb"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (vec_merge:V16QI (vec_select:V16QI (match_operand:V16QI 2 "register_operand" "v")
					   (parallel [(const_int 0)
					   	      (const_int 1)
					   	      (const_int 2)
					   	      (const_int 3)
					   	      (const_int 4)
					   	      (const_int 5)
						      (const_int 6)
						      (const_int 7)
					   	      (const_int 8)
					   	      (const_int 9)
					   	      (const_int 10)
					   	      (const_int 11)
					   	      (const_int 12)
					   	      (const_int 13)
					   	      (const_int 14)
						      (const_int 15)]))
                      (match_operand:V16QI 1 "register_operand" "v")
		      (const_int 255)))]
  "TARGET_ALTIVEC"
  "vmrglb %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vmrglh"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (vec_merge:V8HI (vec_select:V8HI (match_operand:V8HI 2 "register_operand" "v")
					   (parallel [(const_int 0)
					   	      (const_int 1)
					   	      (const_int 2)
					   	      (const_int 3)
					   	      (const_int 4)
					   	      (const_int 5)
					   	      (const_int 6)
					   	      (const_int 7)]))
                      (match_operand:V8HI 1 "register_operand" "v")
		      (const_int 15)))]
  "TARGET_ALTIVEC"
  "vmrglh %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vmrglw"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (vec_merge:V4SI (vec_select:V4SI (match_operand:V4SI 2 "register_operand" "v")
					 (parallel [(const_int 0)
					 	    (const_int 1)
						    (const_int 2)
						    (const_int 3)]))
                      (match_operand:V4SI 1 "register_operand" "v")
		      (const_int 12)))]
  "TARGET_ALTIVEC"
  "vmrglw %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "uminv16qi3"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (umin:V16QI (match_operand:V16QI 1 "register_operand" "v")
                    (match_operand:V16QI 2 "register_operand" "v")))]
  "TARGET_ALTIVEC"
  "vminub %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "sminv16qi3"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (smin:V16QI (match_operand:V16QI 1 "register_operand" "v")
                    (match_operand:V16QI 2 "register_operand" "v")))]
  "TARGET_ALTIVEC"
  "vminsb %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "uminv8hi3"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (umin:V8HI (match_operand:V8HI 1 "register_operand" "v")
                   (match_operand:V8HI 2 "register_operand" "v")))]
  "TARGET_ALTIVEC"
  "vminuh %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "sminv8hi3"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (smin:V8HI (match_operand:V8HI 1 "register_operand" "v")
                   (match_operand:V8HI 2 "register_operand" "v")))]
  "TARGET_ALTIVEC"
  "vminsh %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "uminv4si3"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (umin:V4SI (match_operand:V4SI 1 "register_operand" "v")
                   (match_operand:V4SI 2 "register_operand" "v")))]
  "TARGET_ALTIVEC"
  "vminuw %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "sminv4si3"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (smin:V4SI (match_operand:V4SI 1 "register_operand" "v")
                   (match_operand:V4SI 2 "register_operand" "v")))]
  "TARGET_ALTIVEC"
  "vminsw %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "sminv4sf3"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
        (smin:V4SF (match_operand:V4SF 1 "register_operand" "v")
                   (match_operand:V4SF 2 "register_operand" "v")))]
  "TARGET_ALTIVEC"
  "vminfp %0,%1,%2"
  [(set_attr "type" "veccmp")])

(define_insn "altivec_vmuleub"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V16QI 1 "register_operand" "v")
                      (match_operand:V16QI 2 "register_operand" "v")] 83))]
  "TARGET_ALTIVEC"
  "vmuleub %0,%1,%2"
  [(set_attr "type" "veccomplex")])

(define_insn "altivec_vmulesb"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V16QI 1 "register_operand" "v")
                      (match_operand:V16QI 2 "register_operand" "v")] 84))]
  "TARGET_ALTIVEC"
  "vmulesb %0,%1,%2"
  [(set_attr "type" "veccomplex")])

(define_insn "altivec_vmuleuh"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V8HI 1 "register_operand" "v")
                      (match_operand:V8HI 2 "register_operand" "v")] 85))]
  "TARGET_ALTIVEC"
  "vmuleuh %0,%1,%2"
  [(set_attr "type" "veccomplex")])

(define_insn "altivec_vmulesh"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V8HI 1 "register_operand" "v")
                      (match_operand:V8HI 2 "register_operand" "v")] 86))]
  "TARGET_ALTIVEC"
  "vmulesh %0,%1,%2"
  [(set_attr "type" "veccomplex")])

(define_insn "altivec_vmuloub"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V16QI 1 "register_operand" "v")
                      (match_operand:V16QI 2 "register_operand" "v")] 87))]
  "TARGET_ALTIVEC"
  "vmuloub %0,%1,%2"
  [(set_attr "type" "veccomplex")])

(define_insn "altivec_vmulosb"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V16QI 1 "register_operand" "v")
                      (match_operand:V16QI 2 "register_operand" "v")] 88))]
  "TARGET_ALTIVEC"
  "vmulosb %0,%1,%2"
  [(set_attr "type" "veccomplex")])

(define_insn "altivec_vmulouh"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V8HI 1 "register_operand" "v")
                      (match_operand:V8HI 2 "register_operand" "v")] 89))]
  "TARGET_ALTIVEC"
  "vmulouh %0,%1,%2"
  [(set_attr "type" "veccomplex")])

(define_insn "altivec_vmulosh"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V8HI 1 "register_operand" "v")
                      (match_operand:V8HI 2 "register_operand" "v")] 90))]
  "TARGET_ALTIVEC"
  "vmulosh %0,%1,%2"
  [(set_attr "type" "veccomplex")])

(define_insn "altivec_vnor"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (not:V4SI (ior:V4SI (match_operand:V4SI 1 "register_operand" "v")
                            (match_operand:V4SI 2 "register_operand" "v"))))]
  "TARGET_ALTIVEC"
  "vnor %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "iorv4si3"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (ior:V4SI (match_operand:V4SI 1 "register_operand" "v")
                  (match_operand:V4SI 2 "register_operand" "v")))]
  "TARGET_ALTIVEC"
  "vor %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vpkuhum"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (unspec:V16QI [(match_operand:V8HI 1 "register_operand" "v")
                       (match_operand:V8HI 2 "register_operand" "v")] 93))]
  "TARGET_ALTIVEC"
  "vpkuhum %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vpkuwum"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")] 94))]
  "TARGET_ALTIVEC"
  "vpkuwum %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vpkpx"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")] 95))]
  "TARGET_ALTIVEC"
  "vpkpx %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vpkuhss"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (unspec:V16QI [(match_operand:V8HI 1 "register_operand" "v")
                       (match_operand:V8HI 2 "register_operand" "v")] 96))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] 213))]
  "TARGET_ALTIVEC"
  "vpkuhss %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vpkshss"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (unspec:V16QI [(match_operand:V8HI 1 "register_operand" "v")
                       (match_operand:V8HI 2 "register_operand" "v")] 97))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] 213))]
  "TARGET_ALTIVEC"
  "vpkshss %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vpkuwss"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")] 98))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] 213))]
  "TARGET_ALTIVEC"
  "vpkuwss %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vpkswss"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")] 99))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] 213))]
  "TARGET_ALTIVEC"
  "vpkswss %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vpkuhus"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (unspec:V16QI [(match_operand:V8HI 1 "register_operand" "v")
                       (match_operand:V8HI 2 "register_operand" "v")] 100))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] 213))]
  "TARGET_ALTIVEC"
  "vpkuhus %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vpkshus"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (unspec:V16QI [(match_operand:V8HI 1 "register_operand" "v")
                       (match_operand:V8HI 2 "register_operand" "v")] 101))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] 213))]
  "TARGET_ALTIVEC"
  "vpkshus %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vpkuwus"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")] 102))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] 213))]
  "TARGET_ALTIVEC"
  "vpkuwus %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vpkswus"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")] 103))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] 213))]
  "TARGET_ALTIVEC"
  "vpkswus %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vrlb"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (unspec:V16QI [(match_operand:V16QI 1 "register_operand" "v")
                       (match_operand:V16QI 2 "register_operand" "v")] 104))]
  "TARGET_ALTIVEC"
  "vrlb %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vrlh"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V8HI 1 "register_operand" "v")
                      (match_operand:V8HI 2 "register_operand" "v")] 105))]
  "TARGET_ALTIVEC"
  "vrlh %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vrlw"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")] 106))]
  "TARGET_ALTIVEC"
  "vrlw %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vslb"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (unspec:V16QI [(match_operand:V16QI 1 "register_operand" "v")
                       (match_operand:V16QI 2 "register_operand" "v")] 107))]
  "TARGET_ALTIVEC"
  "vslb %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vslh"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V8HI 1 "register_operand" "v")
                      (match_operand:V8HI 2 "register_operand" "v")] 108))]
  "TARGET_ALTIVEC"
  "vslh %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vslw"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")] 109))]
  "TARGET_ALTIVEC"
  "vslw %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vslw_v4sf"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
        (unspec:V4SF [(match_operand:V4SF 1 "register_operand" "v")
                      (match_operand:V4SF 2 "register_operand" "v")] 109))]
  "TARGET_ALTIVEC"
  "vslw %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vsl"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")] 110))]
  "TARGET_ALTIVEC"
  "vsl %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vslo"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")] 111))]
  "TARGET_ALTIVEC"
  "vslo %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vsrb"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (unspec:V16QI [(match_operand:V16QI 1 "register_operand" "v")
                       (match_operand:V16QI 2 "register_operand" "v")] 112))]
  "TARGET_ALTIVEC"
  "vsrb %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vsrh"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V8HI 1 "register_operand" "v")
                      (match_operand:V8HI 2 "register_operand" "v")] 113))]
  "TARGET_ALTIVEC"
  "vsrh %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vsrw"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")] 114))]
  "TARGET_ALTIVEC"
  "vsrw %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vsrab"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (unspec:V16QI [(match_operand:V16QI 1 "register_operand" "v")
                       (match_operand:V16QI 2 "register_operand" "v")] 115))]
  "TARGET_ALTIVEC"
  "vsrab %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vsrah"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V8HI 1 "register_operand" "v")
                      (match_operand:V8HI 2 "register_operand" "v")] 116))]
  "TARGET_ALTIVEC"
  "vsrah %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vsraw"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")] 117))]
  "TARGET_ALTIVEC"
  "vsraw %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vsr"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")] 118))]
  "TARGET_ALTIVEC"
  "vsr %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vsro"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")] 119))]
  "TARGET_ALTIVEC"
  "vsro %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "subv16qi3"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (minus:V16QI (match_operand:V16QI 1 "register_operand" "v")
                     (match_operand:V16QI 2 "register_operand" "v")))]
  "TARGET_ALTIVEC"
  "vsububm %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "subv8hi3"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (minus:V8HI (match_operand:V8HI 1 "register_operand" "v")
                    (match_operand:V8HI 2 "register_operand" "v")))]
  "TARGET_ALTIVEC"
  "vsubuhm %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "subv4si3"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (minus:V4SI (match_operand:V4SI 1 "register_operand" "v")
                    (match_operand:V4SI 2 "register_operand" "v")))]
  "TARGET_ALTIVEC"
  "vsubuwm %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "subv4sf3"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
        (minus:V4SF (match_operand:V4SF 1 "register_operand" "v")
                    (match_operand:V4SF 2 "register_operand" "v")))]
  "TARGET_ALTIVEC"
  "vsubfp %0,%1,%2"
  [(set_attr "type" "vecfloat")])

(define_insn "altivec_vsubcuw"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")] 124))]
  "TARGET_ALTIVEC"
  "vsubcuw %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vsububs"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (unspec:V16QI [(match_operand:V16QI 1 "register_operand" "v")
                       (match_operand:V16QI 2 "register_operand" "v")] 125))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] 213))]
  "TARGET_ALTIVEC"
  "vsububs %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vsubsbs"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (unspec:V16QI [(match_operand:V16QI 1 "register_operand" "v")
                       (match_operand:V16QI 2 "register_operand" "v")] 126))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] 213))]
  "TARGET_ALTIVEC"
  "vsubsbs %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vsubuhs"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V8HI 1 "register_operand" "v")
                      (match_operand:V8HI 2 "register_operand" "v")] 127))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] 213))]
  "TARGET_ALTIVEC"
  "vsubuhs %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vsubshs"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V8HI 1 "register_operand" "v")
                      (match_operand:V8HI 2 "register_operand" "v")] 128))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] 213))]
  "TARGET_ALTIVEC"
  "vsubshs %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vsubuws"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")] 129))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] 213))]
  "TARGET_ALTIVEC"
  "vsubuws %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vsubsws"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")] 130))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] 213))]
  "TARGET_ALTIVEC"
  "vsubsws %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vsum4ubs"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V16QI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")] 131))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] 213))]
  "TARGET_ALTIVEC"
  "vsum4ubs %0,%1,%2"
  [(set_attr "type" "veccomplex")])

(define_insn "altivec_vsum4sbs"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V16QI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")] 132))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] 213))]
  "TARGET_ALTIVEC"
  "vsum4sbs %0,%1,%2"
  [(set_attr "type" "veccomplex")])

(define_insn "altivec_vsum4shs"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V8HI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")] 133))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] 213))]
  "TARGET_ALTIVEC"
  "vsum4shs %0,%1,%2"
  [(set_attr "type" "veccomplex")])

(define_insn "altivec_vsum2sws"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")] 134))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] 213))]
  "TARGET_ALTIVEC"
  "vsum2sws %0,%1,%2"
  [(set_attr "type" "veccomplex")])

(define_insn "altivec_vsumsws"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")] 135))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] 213))]
  "TARGET_ALTIVEC"
  "vsumsws %0,%1,%2"
  [(set_attr "type" "veccomplex")])

;; Vector xor's
(define_insn "xorv4si3"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (xor:V4SI (match_operand:V4SI 1 "register_operand" "v")
                  (match_operand:V4SI 2 "register_operand" "v")))]
  "TARGET_ALTIVEC"
  "vxor %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "xorv8hi3"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (xor:V8HI (match_operand:V8HI 1 "register_operand" "v")
                  (match_operand:V8HI 2 "register_operand" "v")))]
  "TARGET_ALTIVEC"
  "vxor %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "xorv16qi3"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (xor:V16QI (match_operand:V16QI 1 "register_operand" "v")
		   (match_operand:V16QI 2 "register_operand" "v")))]
  "TARGET_ALTIVEC"
  "vxor %0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_vspltb"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (unspec:V16QI [(match_operand:V16QI 1 "register_operand" "v")
                       (match_operand:QI 2 "immediate_operand" "i")] 136))]
  "TARGET_ALTIVEC"
  "vspltb %0,%1,%2"
  [(set_attr "type" "vecperm")])
;; End of vector xor's

(define_insn "altivec_vsplth"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V8HI 1 "register_operand" "v")
                      (match_operand:QI 2 "immediate_operand" "i")] 137))]
  "TARGET_ALTIVEC"
  "vsplth %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vspltw"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:QI 2 "immediate_operand" "i")] 138))]
  "TARGET_ALTIVEC"
  "vspltw %0,%1,%2"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vspltisb"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (unspec:V16QI [(match_operand:QI 1 "immediate_operand" "i")]
		      UNSPEC_VSPLTISB))]
  "TARGET_ALTIVEC"
  "vspltisb %0,%1"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vspltish"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:QI 1 "immediate_operand" "i")]
		     UNSPEC_VSPLTISH))]
  "TARGET_ALTIVEC"
  "vspltish %0,%1"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vspltisw"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:QI 1 "immediate_operand" "i")]
		     UNSPEC_VSPLTISW))]
  "TARGET_ALTIVEC"
  "vspltisw %0,%1"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vspltisw_v4sf"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
        (unspec:V4SF [(match_operand:QI 1 "immediate_operand" "i")] 142))]
  "TARGET_ALTIVEC"
  "vspltisw %0,%1"
  [(set_attr "type" "vecperm")])

(define_insn "ftruncv4sf2"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
  	(fix:V4SF (match_operand:V4SF 1 "register_operand" "v")))]
  "TARGET_ALTIVEC"
  "vrfiz %0,%1"
  [(set_attr "type" "vecfloat")])

(define_insn "altivec_vperm_4si"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
	(unspec:V4SI [(match_operand:V4SI 1 "register_operand" "v")
		      (match_operand:V4SI 2 "register_operand" "v")
		      (match_operand:V16QI 3 "register_operand" "v")] 144))]
  "TARGET_ALTIVEC"
  "vperm %0,%1,%2,%3"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vperm_4sf"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
	(unspec:V4SF [(match_operand:V4SF 1 "register_operand" "v")
		      (match_operand:V4SF 2 "register_operand" "v")
		      (match_operand:V16QI 3 "register_operand" "v")] 145))]
  "TARGET_ALTIVEC"
  "vperm %0,%1,%2,%3"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vperm_8hi"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
	(unspec:V8HI [(match_operand:V8HI 1 "register_operand" "v")
		      (match_operand:V8HI 2 "register_operand" "v")
		      (match_operand:V16QI 3 "register_operand" "v")] 146))]
  "TARGET_ALTIVEC"
  "vperm %0,%1,%2,%3"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vperm_16qi"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
	(unspec:V16QI [(match_operand:V16QI 1 "register_operand" "v")
		       (match_operand:V16QI 2 "register_operand" "v")
		       (match_operand:V16QI 3 "register_operand" "v")] 147))]
  "TARGET_ALTIVEC"
  "vperm %0,%1,%2,%3"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vrfip"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
        (unspec:V4SF [(match_operand:V4SF 1 "register_operand" "v")] 148))]
  "TARGET_ALTIVEC"
  "vrfip %0,%1"
  [(set_attr "type" "vecfloat")])

(define_insn "altivec_vrfin"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
        (unspec:V4SF [(match_operand:V4SF 1 "register_operand" "v")] 149))]
  "TARGET_ALTIVEC"
  "vrfin %0,%1"
  [(set_attr "type" "vecfloat")])

(define_insn "altivec_vrfim"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
        (unspec:V4SF [(match_operand:V4SF 1 "register_operand" "v")] 150))]
  "TARGET_ALTIVEC"
  "vrfim %0,%1"
  [(set_attr "type" "vecfloat")])

(define_insn "altivec_vcfux"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
        (unspec:V4SF [(match_operand:V4SI 1 "register_operand" "v")
	              (match_operand:QI 2 "immediate_operand" "i")] 151))]
  "TARGET_ALTIVEC"
  "vcfux %0,%1,%2"
  [(set_attr "type" "vecfloat")])

(define_insn "altivec_vcfsx"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
        (unspec:V4SF [(match_operand:V4SI 1 "register_operand" "v")
	              (match_operand:QI 2 "immediate_operand" "i")] 152))]
  "TARGET_ALTIVEC"
  "vcfsx %0,%1,%2"
  [(set_attr "type" "vecfloat")])

(define_insn "altivec_vctuxs"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SF 1 "register_operand" "v")
                      (match_operand:QI 2 "immediate_operand" "i")] 153))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] 213))]
  "TARGET_ALTIVEC"
  "vctuxs %0,%1,%2"
  [(set_attr "type" "vecfloat")])

(define_insn "altivec_vctsxs"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SF 1 "register_operand" "v")
                      (match_operand:QI 2 "immediate_operand" "i")] 154))
   (set (reg:SI 110) (unspec:SI [(const_int 0)] 213))]
  "TARGET_ALTIVEC"
  "vctsxs %0,%1,%2"
  [(set_attr "type" "vecfloat")])

(define_insn "altivec_vlogefp"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
        (unspec:V4SF [(match_operand:V4SF 1 "register_operand" "v")] 155))]
  "TARGET_ALTIVEC"
  "vlogefp %0,%1"
  [(set_attr "type" "vecfloat")])

(define_insn "altivec_vexptefp"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
        (unspec:V4SF [(match_operand:V4SF 1 "register_operand" "v")] 156))]
  "TARGET_ALTIVEC"
  "vexptefp %0,%1"
  [(set_attr "type" "vecfloat")])

(define_insn "altivec_vrsqrtefp"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
        (unspec:V4SF [(match_operand:V4SF 1 "register_operand" "v")] 157))]
  "TARGET_ALTIVEC"
  "vrsqrtefp %0,%1"
  [(set_attr "type" "vecfloat")])

(define_insn "altivec_vrefp"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
        (unspec:V4SF [(match_operand:V4SF 1 "register_operand" "v")] 158))]
  "TARGET_ALTIVEC"
  "vrefp %0,%1"
  [(set_attr "type" "vecfloat")])

(define_insn "altivec_vsel_4si"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SI 1 "register_operand" "v")
                      (match_operand:V4SI 2 "register_operand" "v")
                      (match_operand:V4SI 3 "register_operand" "v")] 159))]
  "TARGET_ALTIVEC"
  "vsel %0,%1,%2,%3"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vsel_4sf"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
        (unspec:V4SF [(match_operand:V4SF 1 "register_operand" "v")
                      (match_operand:V4SF 2 "register_operand" "v")
                      (match_operand:V4SI 3 "register_operand" "v")] 160))]
  "TARGET_ALTIVEC"
  "vsel %0,%1,%2,%3"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vsel_8hi"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V8HI 1 "register_operand" "v")
                      (match_operand:V8HI 2 "register_operand" "v")
                      (match_operand:V8HI 3 "register_operand" "v")] 161))]
  "TARGET_ALTIVEC"
  "vsel %0,%1,%2,%3"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vsel_16qi"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (unspec:V16QI [(match_operand:V16QI 1 "register_operand" "v")
                       (match_operand:V16QI 2 "register_operand" "v")
                       (match_operand:V16QI 3 "register_operand" "v")] 162))]
  "TARGET_ALTIVEC"
  "vsel %0,%1,%2,%3"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vsldoi_4si"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SI 1 "register_operand" "v")
		      (match_operand:V4SI 2 "register_operand" "v")
                      (match_operand:QI 3 "immediate_operand" "i")] 163))]
  "TARGET_ALTIVEC"
  "vsldoi %0,%1,%2,%3"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vsldoi_4sf"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
        (unspec:V4SF [(match_operand:V4SF 1 "register_operand" "v")
		      (match_operand:V4SF 2 "register_operand" "v")
                      (match_operand:QI 3 "immediate_operand" "i")] 164))]
  "TARGET_ALTIVEC"
  "vsldoi %0,%1,%2,%3"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vsldoi_8hi"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V8HI 1 "register_operand" "v")
		      (match_operand:V8HI 2 "register_operand" "v")
                      (match_operand:QI 3 "immediate_operand" "i")] 165))]
  "TARGET_ALTIVEC"
  "vsldoi %0,%1,%2,%3"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vsldoi_16qi"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (unspec:V16QI [(match_operand:V16QI 1 "register_operand" "v")
		       (match_operand:V16QI 2 "register_operand" "v")
		       (match_operand:QI 3 "immediate_operand" "i")] 166))]
  "TARGET_ALTIVEC"
  "vsldoi %0,%1,%2,%3"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vupkhsb"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
  	(unspec:V8HI [(match_operand:V16QI 1 "register_operand" "v")] 167))]
  "TARGET_ALTIVEC"
  "vupkhsb %0,%1"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vupkhpx"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
  	(unspec:V4SI [(match_operand:V8HI 1 "register_operand" "v")] 168))]
  "TARGET_ALTIVEC"
  "vupkhpx %0,%1"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vupkhsh"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
  	(unspec:V4SI [(match_operand:V8HI 1 "register_operand" "v")] 169))]
  "TARGET_ALTIVEC"
  "vupkhsh %0,%1"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vupklsb"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
  	(unspec:V8HI [(match_operand:V16QI 1 "register_operand" "v")] 170))]
  "TARGET_ALTIVEC"
  "vupklsb %0,%1"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vupklpx"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
  	(unspec:V4SI [(match_operand:V8HI 1 "register_operand" "v")] 171))]
  "TARGET_ALTIVEC"
  "vupklpx %0,%1"
  [(set_attr "type" "vecperm")])

(define_insn "altivec_vupklsh"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
  	(unspec:V4SI [(match_operand:V8HI 1 "register_operand" "v")] 172))]
  "TARGET_ALTIVEC"
  "vupklsh %0,%1"
  [(set_attr "type" "vecperm")])

;; AltiVec predicates.

(define_expand "cr6_test_for_zero"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(eq:SI (reg:CC 74)
	       (const_int 0)))]
  "TARGET_ALTIVEC"
  "")	

(define_expand "cr6_test_for_zero_reverse"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(eq:SI (reg:CC 74)
	       (const_int 0)))
   (set (match_dup 0) (minus:SI (const_int 1) (match_dup 0)))]
  "TARGET_ALTIVEC"
  "")

(define_expand "cr6_test_for_lt"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(lt:SI (reg:CC 74)
	       (const_int 0)))]
  "TARGET_ALTIVEC"
  "")

(define_expand "cr6_test_for_lt_reverse"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(lt:SI (reg:CC 74)
	       (const_int 0)))
   (set (match_dup 0) (minus:SI (const_int 1) (match_dup 0)))]
  "TARGET_ALTIVEC"
  "")

;; We can get away with generating the opcode on the fly (%3 below)
;; because all the predicates have the same scheduling parameters.

(define_insn "altivec_predicate_v4si"
  [(set (reg:CC 74)
	(unspec:CC [(match_operand:V4SI 1 "register_operand" "v")
		    (match_operand:V4SI 2 "register_operand" "v")
		    (match_operand 3 "any_operand" "")] 173))
   (clobber (match_scratch:V4SI 0 "=v"))]
  "TARGET_ALTIVEC"
  "%3 %0,%1,%2"
[(set_attr "type" "veccmp")])

(define_insn "altivec_predicate_v4sf"
  [(set (reg:CC 74)
	(unspec:CC [(match_operand:V4SF 1 "register_operand" "v")
		    (match_operand:V4SF 2 "register_operand" "v")
		    (match_operand 3 "any_operand" "")] 174))
   (clobber (match_scratch:V4SF 0 "=v"))]
  "TARGET_ALTIVEC"
  "%3 %0,%1,%2"
[(set_attr "type" "veccmp")])

(define_insn "altivec_predicate_v8hi"
  [(set (reg:CC 74)
	(unspec:CC [(match_operand:V8HI 1 "register_operand" "v")
		    (match_operand:V8HI 2 "register_operand" "v")
		    (match_operand 3 "any_operand" "")] 175))
   (clobber (match_scratch:V8HI 0 "=v"))]
  "TARGET_ALTIVEC"
  "%3 %0,%1,%2"
[(set_attr "type" "veccmp")])

(define_insn "altivec_predicate_v16qi"
  [(set (reg:CC 74)
	(unspec:CC [(match_operand:V16QI 1 "register_operand" "v")
		    (match_operand:V16QI 2 "register_operand" "v")
		    (match_operand 3 "any_operand" "")] 175))
   (clobber (match_scratch:V16QI 0 "=v"))]
  "TARGET_ALTIVEC"
  "%3 %0,%1,%2"
[(set_attr "type" "veccmp")])

(define_insn "altivec_mtvscr"
  [(set (reg:SI 110)
	(unspec_volatile:SI
	 [(match_operand:V4SI 0 "register_operand" "v")] 186))]
  "TARGET_ALTIVEC"
  "mtvscr %0"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_mfvscr"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
	(unspec_volatile:V8HI [(reg:SI 110)] 187))]
  "TARGET_ALTIVEC"
  "mfvscr %0"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_dssall"
  [(unspec [(const_int 0)] 188)]
  "TARGET_ALTIVEC"
  "dssall"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_dss"
  [(unspec [(match_operand:QI 0 "immediate_operand" "i")] 189)]
  "TARGET_ALTIVEC"
  "dss %0"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_dst"
  [(unspec [(match_operand:V4SI 0 "memory_operand" "Q")
	    (match_operand:SI 1 "register_operand" "r")
	    (match_operand:QI 2 "immediate_operand" "i")] 190)]
  "TARGET_ALTIVEC"
  "dst %P0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_dstt"
  [(unspec [(match_operand:V4SI 0 "memory_operand" "Q")
	    (match_operand:SI 1 "register_operand" "r")
	    (match_operand:QI 2 "immediate_operand" "i")] 191)]
  "TARGET_ALTIVEC"
  "dstt %P0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_dstst"
  [(unspec [(match_operand:V4SI 0 "memory_operand" "Q")
	    (match_operand:SI 1 "register_operand" "r")
	    (match_operand:QI 2 "immediate_operand" "i")] 192)]
  "TARGET_ALTIVEC"
  "dstst %P0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_dststt"
  [(unspec [(match_operand:V4SI 0 "memory_operand" "Q")
	    (match_operand:SI 1 "register_operand" "r")
	    (match_operand:QI 2 "immediate_operand" "i")] 193)]
  "TARGET_ALTIVEC"
  "dststt %P0,%1,%2"
  [(set_attr "type" "vecsimple")])

(define_insn "altivec_lvsl"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
	(unspec:V16QI [(match_operand 1 "memory_operand" "m")] 194))]
  "TARGET_ALTIVEC"
  "lvsl %0,%y1"
  [(set_attr "type" "vecload")])

(define_insn "altivec_lvsr"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
	(unspec:V16QI [(match_operand 1 "memory_operand" "m")] 195))]
  "TARGET_ALTIVEC"
  "lvsr %0,%y1"
  [(set_attr "type" "vecload")])

;; Parallel some of the LVE* and STV*'s with unspecs because some have
;; identical rtl but different instructions-- and gcc gets confused.

(define_insn "altivec_lvebx"
  [(parallel
    [(set (match_operand:V16QI 0 "register_operand" "=v")
	  (match_operand:V16QI 1 "memory_operand" "m"))
     (unspec [(const_int 0)] 196)])]
  "TARGET_ALTIVEC"
  "lvebx %0,%y1"
  [(set_attr "type" "vecload")])

(define_insn "altivec_lvehx"
  [(parallel
    [(set (match_operand:V8HI 0 "register_operand" "=v")
	  (match_operand:V8HI 1 "memory_operand" "m"))
     (unspec [(const_int 0)] 197)])]
  "TARGET_ALTIVEC"
  "lvehx %0,%y1"
  [(set_attr "type" "vecload")])

(define_insn "altivec_lvewx"
  [(parallel
    [(set (match_operand:V4SI 0 "register_operand" "=v")
	  (match_operand:V4SI 1 "memory_operand" "m"))
     (unspec [(const_int 0)] 198)])]
  "TARGET_ALTIVEC"
  "lvewx %0,%y1"
  [(set_attr "type" "vecload")])

(define_insn "altivec_lvxl"
  [(parallel
    [(set (match_operand:V4SI 0 "register_operand" "=v")
	  (match_operand:V4SI 1 "memory_operand" "m"))
     (unspec [(const_int 0)] 213)])]
  "TARGET_ALTIVEC"
  "lvxl %0,%y1"
  [(set_attr "type" "vecload")])

(define_insn "altivec_lvx"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
	(match_operand:V4SI 1 "memory_operand" "m"))]
  "TARGET_ALTIVEC"
  "lvx %0,%y1"
  [(set_attr "type" "vecload")])

(define_insn "altivec_stvx"
  [(parallel
    [(set (match_operand:V4SI 0 "memory_operand" "=m")
	  (match_operand:V4SI 1 "register_operand" "v"))
     (unspec [(const_int 0)] 201)])]
  "TARGET_ALTIVEC"
  "stvx %1,%y0"
  [(set_attr "type" "vecstore")])

(define_insn "altivec_stvxl"
  [(parallel
    [(set (match_operand:V4SI 0 "memory_operand" "=m")
	  (match_operand:V4SI 1 "register_operand" "v"))
     (unspec [(const_int 0)] 202)])]
  "TARGET_ALTIVEC"
  "stvxl %1,%y0"
  [(set_attr "type" "vecstore")])

(define_insn "altivec_stvebx"
  [(parallel
    [(set (match_operand:V16QI 0 "memory_operand" "=m")
	  (match_operand:V16QI 1 "register_operand" "v"))
     (unspec [(const_int 0)] 203)])]
  "TARGET_ALTIVEC"
  "stvebx %1,%y0"
  [(set_attr "type" "vecstore")])

(define_insn "altivec_stvehx"
  [(parallel
    [(set (match_operand:V8HI 0 "memory_operand" "=m")
	  (match_operand:V8HI 1 "register_operand" "v"))
     (unspec [(const_int 0)] 204)])]
  "TARGET_ALTIVEC"
  "stvehx %1,%y0"
  [(set_attr "type" "vecstore")])

(define_insn "altivec_stvewx"
  [(parallel
    [(set (match_operand:V4SI 0 "memory_operand" "=m")
	  (match_operand:V4SI 1 "register_operand" "v"))
     (unspec [(const_int 0)] 205)])]
  "TARGET_ALTIVEC"
  "stvewx %1,%y0"
  [(set_attr "type" "vecstore")])

(define_insn "absv16qi2"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
	(abs:V16QI (match_operand:V16QI 1 "register_operand" "v")))
   (clobber (match_scratch:V16QI 2 "=&v"))
   (clobber (match_scratch:V16QI 3 "=&v"))]
  "TARGET_ALTIVEC"
  "vspltisb %2,0\;vsububm %3,%2,%1\;vmaxsb %0,%1,%3"
  [(set_attr "type" "vecsimple")
   (set_attr "length" "12")])

(define_insn "absv8hi2"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (abs:V8HI (match_operand:V8HI 1 "register_operand" "v")))
   (clobber (match_scratch:V8HI 2 "=&v"))
   (clobber (match_scratch:V8HI 3 "=&v"))]
  "TARGET_ALTIVEC"
  "vspltisb %2,0\;vsubuhm %3,%2,%1\;vmaxsh %0,%1,%3"
  [(set_attr "type" "vecsimple")
   (set_attr "length" "12")])

(define_insn "absv4si2"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (abs:V4SI (match_operand:V4SI 1 "register_operand" "v")))
   (clobber (match_scratch:V4SI 2 "=&v"))
   (clobber (match_scratch:V4SI 3 "=&v"))]
  "TARGET_ALTIVEC"
  "vspltisb %2,0\;vsubuwm %3,%2,%1\;vmaxsw %0,%1,%3"
  [(set_attr "type" "vecsimple")
   (set_attr "length" "12")])

(define_insn "absv4sf2"
  [(set (match_operand:V4SF 0 "register_operand" "=v")
        (abs:V4SF (match_operand:V4SF 1 "register_operand" "v")))
   (clobber (match_scratch:V4SF 2 "=&v"))
   (clobber (match_scratch:V4SF 3 "=&v"))]
  "TARGET_ALTIVEC"
  "vspltisw %2,-1\;vslw %3,%2,%2\;vandc %0,%1,%3"
  [(set_attr "type" "vecsimple")
   (set_attr "length" "12")])

(define_insn "altivec_abss_v16qi"
  [(set (match_operand:V16QI 0 "register_operand" "=v")
        (unspec:V16QI [(match_operand:V16QI 1 "register_operand" "v")] 210))
   (clobber (match_scratch:V16QI 2 "=&v"))
   (clobber (match_scratch:V16QI 3 "=&v"))]
  "TARGET_ALTIVEC"
  "vspltisb %2,0\;vsubsbs %3,%2,%1\;vmaxsb %0,%1,%3"
  [(set_attr "type" "vecsimple")
   (set_attr "length" "12")])

(define_insn "altivec_abss_v8hi"
  [(set (match_operand:V8HI 0 "register_operand" "=v")
        (unspec:V8HI [(match_operand:V8HI 1 "register_operand" "v")] 211))
   (clobber (match_scratch:V8HI 2 "=&v"))
   (clobber (match_scratch:V8HI 3 "=&v"))]
  "TARGET_ALTIVEC"
  "vspltisb %2,0\;vsubshs %3,%2,%1\;vmaxsh %0,%1,%3"
  [(set_attr "type" "vecsimple")
   (set_attr "length" "12")])

(define_insn "altivec_abss_v4si"
  [(set (match_operand:V4SI 0 "register_operand" "=v")
        (unspec:V4SI [(match_operand:V4SI 1 "register_operand" "v")] 212))
   (clobber (match_scratch:V4SI 2 "=&v"))
   (clobber (match_scratch:V4SI 3 "=&v"))]
  "TARGET_ALTIVEC"
  "vspltisb %2,0\;vsubsws %3,%2,%1\;vmaxsw %0,%1,%3"
  [(set_attr "type" "vecsimple")
   (set_attr "length" "12")])
