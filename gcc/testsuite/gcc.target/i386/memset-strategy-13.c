/* { dg-do compile } */
/* { dg-options "-Os -mno-sse -mstringop-strategy=unrolled_loop -fasynchronous-unwind-tables -fdwarf2-cfi-asm" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target { ! ia32 } } {^\t?\.} } } */

/*
**foo:
**.LFB[0-9]+:
**	.cfi_startproc
**	xorl	%eax, %eax
**	movq	%rax, \(%(e|r)di\)
**	movq	%rax, 8\(%(e|r)di\)
**	movq	%rax, 16\(%(e|r)di\)
**	movq	%rax, 24\(%(e|r)di\)
**	movq	%rax, 32\(%(e|r)di\)
**	movq	%rax, 40\(%(e|r)di\)
**	movq	%rax, 48\(%(e|r)di\)
**	ret
**...
*/

void
foo (char *a)
{
  __builtin_memset (a, 0, 56);
}
