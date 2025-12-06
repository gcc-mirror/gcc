/* Verify that the most likely BB edges are optimized as fallthroughs.  */
/* { dg-do compile } */
/* { dg-options "-O1 -fno-pic -mtune=generic -march=armv8-a" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target *-*-* } {^\t?\.} } } */

/*
**test:
**.LFB[0-9]+:
**	.cfi_startproc
**...
**	cbz	w0, .L[0-9]*
**...
**	bl	f1
**...
**	ret
**.L[0-9]+:
**...
**	ret
**...
*/

int f1(void);

int test(int a)
{
  if (__builtin_expect(!!a, 1)) {
    return f1();
  }
  return a;
}
