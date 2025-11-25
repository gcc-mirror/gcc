/* { dg-do compile } */
/* { dg-options "-Os -march=x86-64 -mstringop-strategy=vector_loop" } */
/* { dg-add-options check_function_bodies } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target { ! ia32 } } {^\t?\.} } } */

/*
**foo:
**.LFB[0-9]+:
**	.cfi_startproc
**	xorps	%xmm0, %xmm0
**	xorl	%eax, %eax
**	movq	%rax, 48\(%(e|r)di\)
**	movups	%xmm0, \(%(e|r)di\)
**	movups	%xmm0, 16\(%(e|r)di\)
**	movups	%xmm0, 32\(%(e|r)di\)
**	ret
**...
*/

void
foo (char *a)
{
  __builtin_memset (a, 0, 56);
}
