/* { dg-do compile } */
/* { dg-additional-options "-O2 -march=armv8.2-a+sve" } */
/* { dg-final { check-function-bodies "**" "" } } */

/* If the compiler mistakenly vectorizes byte order reversal
 * then the resultant code is inevitably less efficient than a
 * rev instruction.  Guard against such regressions.
 */
typedef unsigned int __u32;
typedef unsigned char __u8;

/*
** rev:
**	rev	w1, w1
**	str	w1, \[x0\]
**	ret
*/
void
rev (__u8 (*dst)[4], __u32 src)
{
  (*dst)[0] = src >> 24;
  (*dst)[1] = src >> 16;
  (*dst)[2] = src >> 8;
  (*dst)[3] = src >> 0;
}
