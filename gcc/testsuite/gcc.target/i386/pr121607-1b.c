/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -fpic -fplt -mtls-dialect=gnu2 -fno-semantic-interposition -fstack-protector" } */

#include "pr121607-1a.c"

/* { dg-final { scan-assembler-times "call\[ \t\]\\*bfd_error@TLSCALL\\(%(?:r|e)ax\\)" 2 { target { ! ia32 } } } } */
