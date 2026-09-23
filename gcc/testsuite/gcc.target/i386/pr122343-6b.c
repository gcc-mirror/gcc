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
**	imull	\$123, %eax, %edx
**	movl	%edx, bar(|\(%rip\))
**...
**	barrier
**...
**	movl	bar(|\(%rip\)), %ecx
**	movl	bar(|\(%rip\)), %edx
**	addl	%ecx, %eax
**	subl	%eax, %edx
**	movl	%edx, bar(|\(%rip\))
**	ret
**	.cfi_endproc
**...
*#
* Darwin indirects extern accesses
*Dfoo:
*D	movq	_bar@GOTPCREL\(%rip\), %rax
*D	movl	\(%rax\), %edx
*D	movl	\(%rax\), %edx
*D	imull	\$123, %edx, %ecx
*D	movl	%ecx, \(%rax\)
*D...
*D	barrier
*D...
*D	movl	\(%rax\), %esi
*D	movl	\(%rax\), %ecx
*D	addl	%esi, %edx
*D	subl	%edx, %ecx
*D	movl	%ecx, \(%rax\)
*D	ret
*D...
*E
*/

#include "pr122343-6a.c"
