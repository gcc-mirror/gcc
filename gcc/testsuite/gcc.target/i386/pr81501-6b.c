/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -march=x86-64 -fpic -fplt -mtls-dialect=gnu2" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target { ! ia32 } } {^\t?\.} } } */

/*
**in_dso:
**.LFB[0-9]+:
**...
**	lea(l|q)	bar@TLSDESC\(%rip\), %(e|r)ax
**	mov(l|q)	%(e|r)si, %.*
**...
**	mov(l|q)	%(e|r)dx, %.*
**...
**	movl	%edi, %.*
**...
**	call	\*bar@TLSCALL\(%(e|r)ax\)
**...
**	lea(l|q)	foo@TLSDESC\(%rip\), %(e|r)ax
**...
**	call	\*foo@TLSCALL\(%(e|r)ax\)
**...
*/

#include "pr81501-6a.c"

/* { dg-final { scan-assembler-times "call\[ \t\]\\*foo@TLSCALL\\(%(?:r|e)ax\\)" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "call\[ \t\]\\*bar@TLSCALL\\(%(?:r|e)ax\\)" 1 { target { ! ia32 } } } } */
