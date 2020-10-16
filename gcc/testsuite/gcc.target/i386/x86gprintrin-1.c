/* Test that <x86gprintrin.h> is usable with -O -std=c89 -pedantic-errors.  */
/* { dg-do compile } */
/* { dg-options "-O -std=c89 -pedantic-errors -march=x86-64 -madx -mbmi -mbmi2 -mcldemote -mclflushopt -mclwb -mclzero -menqcmd -mfsgsbase -mfxsr -mhreset -mlzcnt -mlwp -mmovdiri -mmwaitx -mpconfig -mpopcnt -mpku -mptwrite -mrdpid -mrdrnd -mrdseed -mrtm -mserialize -msgx -mshstk -mtbm -mtsxldtrk -mwaitpkg -mwbnoinvd -mxsave -mxsavec -mxsaveopt -mxsaves -mno-sse -mno-mmx" } */
/* { dg-additional-options "-muintr" { target { ! ia32 } } }  */

#include <x86gprintrin.h>

int dummy;
