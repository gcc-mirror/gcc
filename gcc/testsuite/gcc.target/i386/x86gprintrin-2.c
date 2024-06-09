/* { dg-do compile } */
/* { dg-options "-O2 -Werror-implicit-function-declaration -march=x86-64 -madx -mbmi -mbmi2 -mcldemote -mclflushopt -mclwb -mclzero -menqcmd -mfsgsbase -mfxsr -mhreset -mlzcnt -mlwp -mmovdiri -mmwaitx -mpconfig -mpopcnt -mpku -mptwrite -mrdpid -mrdrnd -mrdseed -mrtm -mserialize -msgx -mshstk -mtbm -mtsxldtrk -mwaitpkg -mwbnoinvd -mxsave -mxsavec -mxsaveopt -mxsaves -mraoint -mno-sse -mno-mmx" } */
/* { dg-add-options bind_pic_locally } */
/* { dg-additional-options "-musermsr -mcmpccxadd -mprefetchi -muintr" { target { ! ia32 } } }  */

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
#define __builtin_ia32_bextri_u32(X, Y) __builtin_ia32_bextri_u32 (X, 1)
#define __builtin_ia32_bextri_u64(X, Y) __builtin_ia32_bextri_u64 (X, 1)

/* rtmintrin.h */
#define __builtin_ia32_xabort(N) __builtin_ia32_xabort(1)

/* cmpccxadd.h */
#define __builtin_ia32_cmpccxadd(A, B, C, D) __builtin_ia32_cmpccxadd(A, B, C, 1)
#define __builtin_ia32_cmpccxadd64(A, B, C, D) __builtin_ia32_cmpccxadd64(A, B, C, 1)

/* usermsrintrin.h */
#define __builtin_ia32_urdmsr(A) __builtin_ia32_urdmsr(1)
#define __builtin_ia32_uwrmsr(A, B) __builtin_ia32_uwrmsr(1, B)

#include <x86gprintrin.h>
