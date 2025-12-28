/* { dg-do compile } */
/* { dg-options "-O2 -fomit-frame-pointer" } */
/* { dg-add-options check_function_bodies } */
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
**	addl	bar(|\(%rip\)), %eax
**	subl	bar(|\(%rip\)), %eax
**	ret
**	.cfi_endproc
**...
*/

extern volatile int bar;

int
foo (void)
{
  int h = bar;
  int r = bar;
  asm volatile ("barrier" ::: "memory");
  int p = bar;
  p = p + r;
  return p - bar;
}
