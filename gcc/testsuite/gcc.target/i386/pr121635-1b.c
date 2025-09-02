/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -fpic -fplt -mtls-dialect=gnu2" } */

#include "pr121635-1a.c"

/* { dg-final { scan-assembler-times "call\[ \t\]\\*cordz_next_sample@TLSCALL\\(%(?:r|e)ax\\)" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "call\[ \t\]\\*kIntervalIfDisabled@TLSCALL\\(%(?:r|e)ax\\)" 1 { target { ! ia32 } } } } */
