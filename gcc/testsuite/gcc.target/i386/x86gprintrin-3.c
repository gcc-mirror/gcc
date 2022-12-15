/* { dg-do compile } */
/* { dg-options "-O0 -Werror-implicit-function-declaration -march=x86-64 -madx -mbmi -mbmi2 -mcldemote -mclflushopt -mclwb -mclzero -menqcmd -mfsgsbase -mfxsr -mhreset -mlzcnt -mlwp -mmovdiri -mmwaitx -mpconfig -mpopcnt -mpku -mptwrite -mrdpid -mrdrnd -mrdseed -mrtm -mserialize -msgx -mshstk -mtbm -mtsxldtrk -mwaitpkg -mwbnoinvd -mxsave -mxsavec -mxsaveopt -mxsaves -mraoint -mno-sse -mno-mmx" } */
/* { dg-add-options bind_pic_locally } */
/* { dg-additional-options "-mcmpccxadd -mprefetchi -muintr" { target { ! ia32 } } }  */

/* Test that the intrinsics in <x86gprintrin.h> compile without optimization.
   All of them are defined as inline functions that reference the proper
   builtin functions.

   Defining away "extern" and "__inline" results in all of them being compiled
   as proper functions.  */

#define extern
#define __inline

#include <x86gprintrin.h>
