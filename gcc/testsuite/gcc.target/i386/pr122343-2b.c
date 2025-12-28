/* { dg-do compile } */
/* { dg-options "-O2 -fomit-frame-pointer -fno-fuse-ops-with-volatile-access" } */
/* { dg-add-options check_function_bodies } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target *-*-* } {^\t?\.} } } */

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
*/

#include "pr122343-2a.c"
