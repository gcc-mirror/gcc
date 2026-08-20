/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64-v4" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target *-*-* } {^\t?\.} } } */

/*
**func1:
**.LFB[0-9]+:
**	.cfi_startproc
**	kxnorb	%k0, %k0, %k0
**	ret
**	.cfi_endproc
**...
*/

void
func1 (void)
{
  unsigned char k = -1;
  __asm volatile ("" : : "k" (k));
}

/*
**func2:
**.LFB[0-9]+:
**	.cfi_startproc
**	kxnorw	%k0, %k0, %k0
**	ret
**	.cfi_endproc
**...
*/

void
func2 (void)
{
  unsigned short k = -1;
  __asm volatile ("" : : "k" (k));
}

/*
**func3:
**.LFB[0-9]+:
**	.cfi_startproc
**	kxnord	%k0, %k0, %k0
**	ret
**	.cfi_endproc
**...
*/

void
func3 (void)
{
  unsigned int k = -1;
  __asm volatile ("" : : "k" (k));
}
