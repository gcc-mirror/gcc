/* Verify that the most likely BB edges are optimized as fallthroughs.  */
/* { dg-do compile } */
/* { dg-options "-O1 -fno-pic -march=rv64gc -mabi=lp64d" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target *-*-* } {^\t?\.} } } */

/*
**test:
**...
**	beq	a0,zero,.L[0-9]*
**...
**	call	f1
**...
** (
**	jr	ra
** |
**	ret
** )
**...
**.L[0-9]+:
**...
** (
**	jr	ra
** |
**	ret
** )
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
