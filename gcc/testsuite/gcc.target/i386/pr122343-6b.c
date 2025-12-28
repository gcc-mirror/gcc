/* { dg-do compile } */
/* { dg-options "-O2 -fomit-frame-pointer -fno-fuse-ops-with-volatile-access" } */
/* { dg-add-options check_function_bodies } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target *-*-* } {^\t?\.} } } */

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
*/

#include "pr122343-6a.c"
