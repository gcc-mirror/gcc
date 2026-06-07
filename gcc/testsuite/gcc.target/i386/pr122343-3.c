/* { dg-do compile } */
/* { dg-options "-O2 -fomit-frame-pointer" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target *-*-* } {^\t?\.} } } */

/*
**foo:
**.LFB0:
**	.cfi_startproc
**	addl	\$123, bar(|\(%rip\))
**	ret
**	.cfi_endproc
**...
*/

extern volatile int bar;

void
foo (void)
{
  bar += 123;
}
