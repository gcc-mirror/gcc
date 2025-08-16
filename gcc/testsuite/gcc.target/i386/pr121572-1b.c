/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O0 -fpic -fplt -mtls-dialect=gnu2" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target { ! ia32 } } {^\t?\.} } } */

/*
**bug:
**.LFB[0-9]+:
**...
**	lea[l|q]	tv_cache@TLSDESC\(%rip\), %[e|r]ax
**	movl	\$-1, %edi
**	call	\*tv_cache@TLSCALL\(%[e|r]ax\)
**	mov[l|q]	%[e|r]ax, %[e|r]bx
**	call	val@PLT
**...
*/

#include "pr121572-1a.c"
