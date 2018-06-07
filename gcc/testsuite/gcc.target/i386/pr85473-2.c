/* { dg-do compile { target { ! ia32 } } } */
/* { dg-require-effective-target maybe_x32 } */
/* { dg-options "-O2 -mx32 -maddress-mode=long -mmovdir64b" } */
/* { dg-final { scan-assembler "movdir64b\[ \\t\]" } } */

#include "pr85473-1.c"

