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
** rev2:
**	ldr	w0, \[x0\]
**	rev	w0, w0
**	ret
*/
__u32
rev2 (const __u8 (*src)[4])
{
  __u32 dst = 0;

  dst |= (__u32) (*src)[3] << 0;
  dst |= (__u32) (*src)[2] << 8;
  dst |= (__u32) (*src)[1] << 16;
  dst |= (__u32) (*src)[0] << 24;

  return dst;
}
