/* { dg-do compile } */
/* { dg-options "-O2 -fomit-frame-pointer -fno-fuse-ops-with-volatile-access" } */
/* { dg-additional-options "-mregparm=1" { target ia32 } } */
/* { dg-final { check-function-bodies "**" "*#" "" { target { ! *-*-darwin* } } {^\t?\.} } } */
/* { dg-final { check-function-bodies "*D" "*E" "" { target { *-*-darwin* && lp64 } } {^\t?\.} } } */

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
*#

* Darwin indirects extern accesses
*Dfoo:
*D	movq	_bar@GOTPCREL\(%rip\), %rax
*D	imull	\$123, %edi, %edi
*D	movl	\(%rax\), %edx
*D	addl	%edx, %edi
*D	movl	%edi, \(%rax\)
*D	ret
*D...
*E
*/

#include "pr122343-5a.c"
