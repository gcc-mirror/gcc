/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O3 -fpic -fplt -mtls-dialect=gnu2" } */

#include "pr121725-1a.c"

/* { dg-final { scan-assembler-times "call\[ \t\]\\*bfd_error@TLSCALL\\(%(?:r|e)ax\\)" 2 { target { ! ia32 } } } } */
