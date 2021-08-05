/* { dg-do compile } */
/* { dg-options "-O2 -mmwait" } */

/* Test calling MWAIT intrinsics from functions with general-regs-only
   target attribute.  */

#include <x86gprintrin.h>

#define _CONCAT(x,y) x ## y

#define test_2(func, type, op1_type, op2_type)				\
  __attribute__ ((target("general-regs-only")))				\
  type _CONCAT(do_,func) (op1_type A, op2_type B)			\
  { return func (A, B); }

#define test_3(func, type, op1_type, op2_type, op3_type)		\
  __attribute__ ((target("general-regs-only")))				\
  type _CONCAT(do_,func) (op1_type A, op2_type B, op3_type C)		\
  { return func (A, B, C); }

#ifndef __iamcu__
/* mwaitintrin.h */
test_3 (_mm_monitor, void, void const *, unsigned int, unsigned int)
test_2 (_mm_mwait, void, unsigned int, unsigned int)
#endif
