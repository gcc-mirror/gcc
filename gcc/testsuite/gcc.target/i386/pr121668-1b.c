/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-Og -g -fpic -fplt -mtls-dialect=gnu2" } */

#include "pr121668-1a.c"

/* { dg-final { scan-assembler-times "call\[ \t\]\\*caml_state@TLSCALL\\(%(?:r|e)ax\\)" 1 { target { ! ia32 } } } } */
