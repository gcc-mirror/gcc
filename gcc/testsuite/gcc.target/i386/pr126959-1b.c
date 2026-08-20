/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64-v4 -mtune=znver4" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target *-*-* } {^\t?\.} } } */

/*
**func1:
**.LFB[0-9]+:
**	.cfi_startproc
**	kxnorb	%k0, %k0, %k0
**	ret
**	.cfi_endproc
**...
*/

/*
**func2:
**.LFB[0-9]+:
**	.cfi_startproc
**	kxnorw	%k0, %k0, %k0
**	ret
**	.cfi_endproc
**...
*/

/*
**func3:
**.LFB[0-9]+:
**	.cfi_startproc
**	kxnord	%k0, %k0, %k0
**	ret
**	.cfi_endproc
**...
*/

#include "pr126959-1a.c"
