/* { dg-do compile } */
/* { dg-options "-O3 -march=x86-64-v2 -std=gnu++17 -fasynchronous-unwind-tables -fdwarf2-cfi-asm" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } {^\t?\.} } } */

/*
**_Z4TestPc:
**.LFB[0-9]+:
**	.cfi_startproc
**	movsbl	-1\(%rdi\), %eax
**	pxor	%xmm1, %xmm1
**	movd	%eax, %xmm0
**	pshufb	%xmm1, %xmm0
**	movups	%xmm0, \(%rdi\)
**	movups	%xmm0, 16\(%rdi\)
**	movups	%xmm0, 32\(%rdi\)
**	movups	%xmm0, 48\(%rdi\)
**	ret
**...
*/

void 
Test (char*s)
{
  __builtin_memset (s, s[-1], 64);
}
