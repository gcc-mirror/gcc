/* { dg-do compile } */
/* { dg-additional-options "-O2 -march=armv8.2-a+sve" } */
/* { dg-final { check-function-bodies "**" "" } } */

/* If the compiler mistakenly vectorizes byte order reversal
 * then the resultant code is inevitably less efficient than a
 * rev instruction.  Guard against such regressions.
 */
typedef unsigned char __u8;

/*
** rev3:
**	ldr	w1, \[x1\]
**	rev	w1, w1
**	str	w1, \[x0\]
**	ret
*/
void
rev3 (unsigned char (*__restrict dst)[4],
      const unsigned char (*__restrict src)[4])
{
  (*dst)[0] = (*src)[3];
  (*dst)[1] = (*src)[2];
  (*dst)[2] = (*src)[1];
  (*dst)[3] = (*src)[0];
}
