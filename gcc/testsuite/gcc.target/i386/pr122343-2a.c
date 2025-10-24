/* { dg-do compile } */
/* { dg-options "-O2 -fomit-frame-pointer" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target *-*-* } {^\t?\.} } } */

/*
**foo:
**.LFB0:
**	.cfi_startproc
**	movl	bar1(|\(%rip\)), %eax
**	addl	bar2(|\(%rip\)), %eax
**	ret
**	.cfi_endproc
**...
*/

extern volatile int bar1, bar2;

int
foo (void)
{
  return bar1 + bar2;
}
