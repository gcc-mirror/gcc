/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -march=x86-64-v4 -mtune=znver4" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target *-*-* } {^\t?\.} } } */

/*
**func:
**.LFB[0-9]+:
**	.cfi_startproc
**	kxnorq	%k0, %k0, %k0
**	ret
**	.cfi_endproc
**...
*/

#include "pr126959-2a.c"
