/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-Og -fpic -fplt -mtls-dialect=gnu2" } */

#include "pr121694-1a.c"

/* { dg-final { scan-assembler-times "call\[ \t\]\\*_TLS_MODULE_BASE_@TLSCALL\\(%(?:r|e)ax\\)" 1 { target { ! ia32 } } } } */
