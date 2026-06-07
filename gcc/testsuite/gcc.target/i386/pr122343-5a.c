/* { dg-do compile } */
/* { dg-options "-O2 -fomit-frame-pointer" } */
/* { dg-additional-options "-mregparm=1" { target ia32 } } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target *-*-* } {^\t?\.} } } */

/*
**foo:
**.LFB0:
**	.cfi_startproc
**	imull	\$123, %e(di|ax), %e(di|ax)
**	addl	%e(di|ax), bar(|\(%rip\))
**	ret
**	.cfi_endproc
**...
*/

extern volatile int bar;

void
foo (int z)
{
  z *= 123;
  bar += z;
}
