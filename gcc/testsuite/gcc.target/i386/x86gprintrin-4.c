/* Same as x86gprintrin-3.c, except converted to use #pragma GCC option.  */
/* { dg-do compile } */
/* { dg-options "-O0 -Werror-implicit-function-declaration -march=x86-64 -mno-sse -mno-mmx" } */
/* { dg-add-options bind_pic_locally } */

/* Test that the intrinsics in <x86gprintrin.h> compile without optimization.
   All of them are defined as inline functions that reference the proper
   builtin functions.

   Defining away "extern" and "__inline" results in all of them being
   compiled as proper functions.  */

#define extern
#define __inline

#define _CONCAT(x,y) x ## y

#define test_0(func, type, imm)		  \
  type _CONCAT(_0,func) (int const I)	  \
	    { return func (imm);  }

#define test_1(func, type, op1_type)   \
  type _CONCAT(_1,func) (op1_type A)    \
	{ return func (A); }

#define test_1r(func, type, op1_type, imm)	    \
  type _CONCAT(_1r,func) (op1_type A, int const I)    \
	{ return func (imm, A); }

#define test_2(func, type, op1_type,  op2_type)	    \
    type _CONCAT(_2,func) (op1_type A, op2_type B)    \
      { return func (A, B);  }

#ifndef DIFFERENT_PRAGMAS
#ifdef __x86_64__
#pragma GCC target ("adx,bmi,bmi2,cmpccxadd,fsgsbase,fxsr,hreset,lwp,lzcnt,popcnt,prefetchi,raoint,rdrnd,rdseed,tbm,rtm,serialize,tsxldtrk,uintr,usermsr,xsaveopt")
#else
#pragma GCC target ("adx,bmi,bmi2,fsgsbase,fxsr,hreset,lwp,lzcnt,popcnt,raoint,rdrnd,rdseed,tbm,rtm,serialize,tsxldtrk,xsaveopt")
#endif
#endif

/* popcnintrin.h (POPCNT).  */
#ifdef DIFFERENT_PRAGMAS
#pragma GCC target ("popcnt")
#endif
#include <popcntintrin.h>

/* x86intrin.h (LWP/BMI/BMI2/TBM/LZCNT). */
#ifdef DIFFERENT_PRAGMAS
#ifdef __x86_64__
#pragma GCC target ("lwp,bmi,bmi2,tbm,lzcnt,usermsr")
#else
#pragma GCC target ("lwp,bmi,bmi2,tbm,lzcnt")
#endif
#endif
#include <x86gprintrin.h>

/* usermsrintrin.h */
#ifdef __x86_64__
test_0 (_urdmsr, unsigned long long, 1)
test_1 (_urdmsr, unsigned long long, unsigned long long)
test_1r (_uwrmsr, void, unsigned long long, 1)
test_2 (_uwrmsr, void, unsigned long long, unsigned long long)
#endif
