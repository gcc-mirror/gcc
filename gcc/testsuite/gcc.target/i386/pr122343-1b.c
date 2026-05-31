/* { dg-do compile } */
/* { dg-options "-O2 -fomit-frame-pointer -fno-fuse-ops-with-volatile-access" } */
/* { dg-additional-options "-mregparm=1" { target ia32 } } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "*#" "" { target { ! *-*-darwin* } } {^\t?\.} } } */
/* { dg-final { check-function-bodies "*D" "*E" "" { target { *-*-darwin* && lp64 } } {^\t?\.} } } */

/*
**foo:
**.LFB0:
**	.cfi_startproc
**	imull	\$123, %e(di|ax), %eax
**	movl	bar(|\(%rip\)), %edx
**	addl	%edx, %eax
**	ret
**	.cfi_endproc
**...
*#

* Darwin indirects the extern
*Dfoo:
*D	movq	_bar@GOTPCREL\(%rip\), %rax
*D	movl	\(%rax\), %edx
*D	imull	\$123, %edi, %eax
*D	addl	%edx, %eax
*D	ret
*D...
*E
*/

#include "pr122343-1a.c"
