/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -march=x86-64-v4 -fpic -fplt -mtls-dialect=gnu2" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */

#include "pr81501-9a.c"

/* { dg-final { scan-assembler-times "vpbroadcastb" 1 } } */
/* { dg-final { scan-assembler-times {lea(l|q)[ \t]+var@TLSDESC\(%rip\), %(e|r)ax} 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "call\[ \t\]\\*var@TLSCALL\\(%(?:r|e)ax\\)" 1 { target { ! ia32 } } } } */
