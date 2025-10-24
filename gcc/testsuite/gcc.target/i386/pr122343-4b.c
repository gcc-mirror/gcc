/* { dg-do compile } */
/* { dg-options "-O2 -fomit-frame-pointer -fno-fuse-ops-with-volatile-access" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target *-*-* } {^\t?\.} } } */

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
*/

#include "pr122343-4a.c"
