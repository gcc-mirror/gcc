/* { dg-do compile { target { ! ia32 } } } */
/* { dg-require-effective-target maybe_x32 } */
/* { dg-options "-maddress-mode=long -mx32 -Ofast -funroll-loops -march=haswell" } */
/* { dg-final { scan-assembler-not "\tvgather" } } */
/* { dg-final { scan-assembler "addr32 vgather" } } */

#include "pr89523-1a.c"
