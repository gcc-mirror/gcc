/* { dg-do compile } */
/* { dg-options "-O2 -fomit-frame-pointer" } */
/* { dg-add-options check_function_bodies } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target { *-*-* } } {^\t?\.} } } */

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
**	movl	bar(|\(%rip\)), %edx
**	addl	%edx, %eax
**	subl	%eax, bar(|\(%rip\))
**	ret
**	.cfi_endproc
**...
*/

extern volatile int bar;

void
foo (void)
{
  int h = bar;
  int r = bar;
  bar = r * 123;
  asm volatile ("barrier" ::: "memory");
  int p = bar;
  p = p + r;
  bar -= p;
}
