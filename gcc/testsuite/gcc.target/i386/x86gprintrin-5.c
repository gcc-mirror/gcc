/* { dg-do compile } */
/* { dg-options "-O2 -Werror-implicit-function-declaration -march=x86-64 -mno-sse -mno-mmx" } */
/* { dg-add-options bind_pic_locally } */

/* Test that the intrinsics in <x86gprintrin.h> compile with optimization.
   All of them are defined as inline functions that reference the proper
   builtin functions.

   Defining away "extern" and "__inline" results in all of them being
   compiled as proper functions.  */

#define extern
#define __inline

/* Following intrinsics require immediate arguments. */

/* lwpintrin.h */
#define __builtin_ia32_lwpval32(D2, D1, F) __builtin_ia32_lwpval32 (D2, D1, 1)
#define __builtin_ia32_lwpval64(D2, D1, F) __builtin_ia32_lwpval64 (D2, D1, 1)
#define __builtin_ia32_lwpins32(D2, D1, F) __builtin_ia32_lwpins32 (D2, D1, 1)
#define __builtin_ia32_lwpins64(D2, D1, F) __builtin_ia32_lwpins64 (D2, D1, 1)

/* tbmintrin.h */
#define __builtin_ia32_bextri_u32(X, Y) __builtin_ia32_bextr_u32 (X, 1)
#define __builtin_ia32_bextri_u64(X, Y) __builtin_ia32_bextr_u64 (X, 1)

/* rtmintrin.h */
#define __builtin_ia32_xabort(M) __builtin_ia32_xabort(1)

/* cmpccxadd.h */
#define __builtin_ia32_cmpccxadd(A, B, C, D) __builtin_ia32_cmpccxadd(A, B, C, 1)
#define __builtin_ia32_cmpccxadd64(A, B, C, D) __builtin_ia32_cmpccxadd64(A, B, C, 1)

/* usermsrintrin.h */
#define __builtin_ia32_urdmsr(A) __builtin_ia32_urdmsr(1)
#define __builtin_ia32_uwrmsr(A, B) __builtin_ia32_uwrmsr(1, B)

#ifdef __x86_64__
#pragma GCC target ("adx,bmi,bmi2,clflushopt,clwb,clzero,cmpccxadd,enqcmd,fsgsbase,fxsr,hreset,lwp,lzcnt,mwaitx,pconfig,pku,popcnt,prefetchi,raoint,rdpid,rdrnd,rdseed,tbm,rtm,serialize,sgx,tsxldtrk,uintr,usermsr,xsavec,xsaveopt,xsaves,wbnoinvd")
#else
#pragma GCC target ("adx,bmi,bmi2,clflushopt,clwb,clzero,enqcmd,fsgsbase,fxsr,hreset,lwp,lzcnt,mwaitx,pconfig,pku,popcnt,raoint,rdpid,rdrnd,rdseed,tbm,rtm,serialize,sgx,tsxldtrk,xsavec,xsaveopt,xsaves,wbnoinvd")
#endif

#include <x86gprintrin.h>
