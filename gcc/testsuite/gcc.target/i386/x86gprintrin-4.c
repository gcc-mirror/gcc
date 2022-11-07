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

#ifndef DIFFERENT_PRAGMAS
#ifdef __x86_64__
#pragma GCC target ("adx,bmi,bmi2,cmpccxadd,fsgsbase,fxsr,hreset,lwp,lzcnt,popcnt,prefetchi,raoint,rdrnd,rdseed,tbm,rtm,serialize,tsxldtrk,uintr,xsaveopt")
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
#pragma GCC target ("lwp,bmi,bmi2,tbm,lzcnt")
#endif
#include <x86gprintrin.h>
