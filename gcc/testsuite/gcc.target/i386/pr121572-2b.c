/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -fpic -fplt -mtls-dialect=gnu2" } */

#include "pr121572-2a.c"

/* { dg-final { scan-assembler-times "call\[ \t\]\\*__gmpfr_emax@TLSCALL\\(%(?:r|e)ax\\)" 1 { target { ! ia32 } } } } */
