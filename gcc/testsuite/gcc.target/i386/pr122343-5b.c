/* { dg-do compile } */
/* { dg-options "-O2 -fomit-frame-pointer -fno-fuse-ops-with-volatile-access" } */
/* { dg-additional-options "-mregparm=1" { target ia32 } } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target *-*-* } {^\t?\.} } } */

/*
**foo:
**.LFB0:
**	.cfi_startproc
**	imull	\$123, %e(di|ax), %e(di|ax)
**	movl	bar(|\(%rip\)), %e(dx|ax)
**	addl	%e(dx|ax), %e(di|ax)
**	movl	%e(di|ax), bar(|\(%rip\))
**	ret
**	.cfi_endproc
**...
*/

#include "pr122343-5a.c"
