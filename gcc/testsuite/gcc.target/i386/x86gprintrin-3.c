/* { dg-do compile } */
/* { dg-options "-O0 -Werror-implicit-function-declaration -march=x86-64 -madx -mbmi -mbmi2 -mcldemote -mclflushopt -mclwb -mclzero -menqcmd -mfsgsbase -mfxsr -mhreset -mlzcnt -mlwp -mmovdiri -mmwaitx -mpconfig -mpopcnt -mpku -mptwrite -mrdpid -mrdrnd -mrdseed -mrtm -mserialize -msgx -mshstk -mtbm -mtsxldtrk -mwaitpkg -mwbnoinvd -mxsave -mxsavec -mxsaveopt -mxsaves -mraoint -mno-sse -mno-mmx" } */
/* { dg-add-options bind_pic_locally } */
/* { dg-additional-options "-musermsr -mcmpccxadd -mprefetchi -muintr" { target { ! ia32 } } }  */

/* Test that the intrinsics in <x86gprintrin.h> compile without optimization.
   All of them are defined as inline functions that reference the proper
   builtin functions.

   Defining away "extern" and "__inline" results in all of them being compiled
   as proper functions.  */

#define extern
#define __inline

#include <x86gprintrin.h>

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

/* usermsrintrin.h */
#ifdef __x86_64__
test_0 (_urdmsr, unsigned long long, 1)
test_1 (_urdmsr, unsigned long long, unsigned long long)
test_1r (_uwrmsr, void, unsigned long long, 1)
test_2 (_uwrmsr, void, unsigned long long, unsigned long long)
#endif
