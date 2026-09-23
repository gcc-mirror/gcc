/* { dg-do compile } */
/* { dg-options "-O2 -fomit-frame-pointer -fno-fuse-ops-with-volatile-access" } */
/* { dg-final { check-function-bodies "**" "*#" "" { target { ! *-*-darwin* } } {^\t?\.} } } */
/* { dg-final { check-function-bodies "*D" "*E" "" { target *-*-darwin* } {^\t?\.} } } */

/*
**foo:
**.LFB0:
**	.cfi_startproc
**	movl	bar1(|\(%rip\)), %eax
**	movl	bar2(|\(%rip\)), %edx
**	addl	%edx, %eax
**	ret
**	.cfi_endproc
**...
*#
*
* Darwin indirects accesses to externs.
*Dfoo:
*D	movq	_bar1@GOTPCREL\(%rip\), %rax
*D	movq	_bar2@GOTPCREL\(%rip\), %rdx
*D	movl	\(%rax\), %eax
*D	movl	\(%rdx\), %edx
*D	addl	%edx, %eax
*D	ret
*D...
*E
*/

#include "pr122343-2a.c"
