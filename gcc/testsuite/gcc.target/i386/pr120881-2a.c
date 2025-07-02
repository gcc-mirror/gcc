/* { dg-do compile { target fentry } } */
/* { dg-options "-O2 -pg" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target "*-*-*" } {^\t?\.} } } */

/*
**f2:
**.LFB[0-9]+:
**	.cfi_startproc
**	call	__fentry__
**...
*/

extern void f1 (void);

void
f2 (int count)
{
  for (int i = 0; i < count; ++i)
    f1 ();
}
