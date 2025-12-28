/* { dg-do compile } */
/* { dg-options "-O2 -fomit-frame-pointer" } */
/* { dg-additional-options "-mregparm=1" { target ia32 } } */
/* { dg-add-options check_function_bodies } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target *-*-* } {^\t?\.} } } */

/*
**foo:
**.LFB0:
**	.cfi_startproc
**	imull	\$123, %e(di|ax), %eax
**	addl	bar(|\(%rip\)), %eax
**	ret
**	.cfi_endproc
**...
*/

extern volatile int bar;

int
foo (int z)
{
  z *= 123;
  return bar + z;
}
