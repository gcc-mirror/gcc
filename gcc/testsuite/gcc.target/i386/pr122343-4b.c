/* { dg-do compile } */
/* { dg-options "-O2 -fomit-frame-pointer -fno-fuse-ops-with-volatile-access" } */
/* { dg-final { check-function-bodies "**" "*#" "" { target { ! *-*-darwin* } } {^\t?\.} } } */
/* { dg-final { check-function-bodies "*D" "*E" "" { target { *-*-darwin* && lp64 } } {^\t?\.} } } */

/*
**foo:
**.LFB0:
**	.cfi_startproc
**	movl	bar(|\(%rip\)), %eax
**	movl	bar(|\(%rip\)), %eax
**...
**	barrier
**...
**	movl	bar(|\(%rip\)), %ecx
**	movl	bar(|\(%rip\)), %edx
**	addl	%ecx, %eax
**	subl	%edx, %eax
**	ret
**	.cfi_endproc
**...
*#

* Darwin indirects extern accesses
*Dfoo:
*D	movq	_bar@GOTPCREL\(%rip\), %rdx
*D	movl	\(%rdx\), %eax
*D	movl	\(%rdx\), %eax
*D...
*D	barrier
*D...
*D	movl	\(%rdx\), %ecx
*D	movl	\(%rdx\), %edx
*D	addl	%ecx, %eax
*D	subl	%edx, %eax
*D	ret
*D...
*E
*/

#include "pr122343-4a.c"
