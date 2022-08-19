/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -march=x86-64-v4 -fpic -fplt -mtls-dialect=gnu2" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target { ! ia32 } } {^\t?\.} } } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */

/*
**foo:
**.LFB[0-9]+:
**...
**	vpbroadcastb	%edi, %zmm0
**...
**	lea(l|q)	var@TLSDESC\(%rip\), %(e|r)ax
**...
**	call	\*var@TLSCALL\(%(e|r)ax\)
**...
*/

#include "pr81501-9a.c"

/* { dg-final { scan-assembler-times "vpbroadcastb" 1 } } */
/* { dg-final { scan-assembler-times "call\[ \t\]\\*var@TLSCALL\\(%(?:r|e)ax\\)" 1 { target { ! ia32 } } } } */
