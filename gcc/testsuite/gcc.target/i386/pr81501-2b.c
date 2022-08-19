/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -fpic -fplt -mtls-dialect=gnu2" } */

#include "pr81501-2a.c"

/* { dg-final { scan-assembler-times "call\[ \t\]\\*e@TLSCALL\\(%(?:r|e)ax\\)" 1 { target { ! ia32 } } } } */
