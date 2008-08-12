/* Test for hexadecimal float rounding: bug 21720.  */
/* { dg-do link } */
/* { dg-options "-O -std=gnu99" } */
/* { dg-skip-if "SPU float rounds towards zero" { spu-*-* } } */

#include <float.h>

extern void link_failure (void);

int
main (void)
{
#if FLT_RADIX == 2 && FLT_MANT_DIG == 24
  if (0x1.0000011p0f == 1)
    link_failure ();
  if (0x1.00000101p0f == 1)
    link_failure ();
  if (0x1.000001001p0f == 1)
    link_failure ();
  if (0x1.0000010001p0f == 1)
    link_failure ();
  if (0x1.00000100001p0f == 1)
    link_failure ();
  if (0x1.000001000001p0f == 1)
    link_failure ();
  if (0x1.0000010000001p0f == 1)
    link_failure ();
  if (0x1.00000100000001p0f == 1)
    link_failure ();
  if (0x1.000001000000001p0f == 1)
    link_failure ();
  if (0x1.0000010000000001p0f == 1)
    link_failure ();
  if (0x1.00000100000000001p0f == 1)
    link_failure ();
  if (0x1.000001000000000001p0f == 1)
    link_failure ();
  if (0x1.0000010000000000001p0f == 1)
    link_failure ();
  if (0x1.00000100000000000001p0f == 1)
    link_failure ();
  if (0x1.000001000000000000001p0f == 1)
    link_failure ();
  if (0x1.0000010000000000000001p0f == 1)
    link_failure ();
  if (0x1.00000100000000000000001p0f == 1)
    link_failure ();
  if (0x1.000001000000000000000001p0f == 1)
    link_failure ();
  if (0x1.0000010000000000000000001p0f == 1)
    link_failure ();
  if (0x1.00000100000000000000000001p0f == 1)
    link_failure ();
  if (0x1.000001000000000000000000001p0f == 1)
    link_failure ();
  if (0x1.0000010000000000000000000001p0f == 1)
    link_failure ();
  if (0x1.00000100000000000000000000001p0f == 1)
    link_failure ();
  if (0x1.000001000000000000000000000001p0f == 1)
    link_failure ();
  if (0x1.0000010000000000000000000000001p0f == 1)
    link_failure ();
  if (0x1.00000100000000000000000000000001p0f == 1)
    link_failure ();
  if (0x1.000001000000000000000000000000001p0f == 1)
    link_failure ();
  if (0x1.0000010000000000000000000000000001p0f == 1)
    link_failure ();
  if (0x1.00000100000000000000000000000000001p0f == 1)
    link_failure ();
  if (0x1.000001000000000000000000000000000001p0f == 1)
    link_failure ();
  if (0x1.0000010000000000000000000000000000001p0f == 1)
    link_failure ();
  if (0x1.00000100000000000000000000000000000001p0f == 1)
    link_failure ();
  if (0x1.000001000000000000000000000000000000001p0f == 1)
    link_failure ();
  if (0x1.0000010000000000000000000000000000000001p0f == 1)
    link_failure ();
  if (0x1.00000100000000000000000000000000000000001p0f == 1)
    link_failure ();
  if (0x1.000001000000000000000000000000000000000001p0f == 1)
    link_failure ();
  if (0x1.0000010000000000000000000000000000000000001p0f == 1)
    link_failure ();
  if (0x1.00000100000000000000000000000000000000000001p0f == 1)
    link_failure ();
  if (0x1.000001000000000000000000000000000000000000001p0f == 1)
    link_failure ();
  if (0x1.0000010000000000000000000000000000000000000001p0f == 1)
    link_failure ();
  if (0x1.00000100000000000000000000000000000000000000001p0f == 1)
    link_failure ();
  if (0x1.000001000000000000000000000000000000000000000001p0f == 1)
    link_failure ();
  if (0x1.0000010000000000000000000000000000000000000000001p0f == 1)
    link_failure ();
  if (0x1.00000100000000000000000000000000000000000000000001p0f == 1)
    link_failure ();
  if (0x1.000001000000000000000000000000000000000000000000001p0f == 1)
    link_failure ();
  if (0x1.0000010000000000000000000000000000000000000000000001p0f == 1)
    link_failure ();
  if (0x1.00000100000000000000000000000000000000000000000000001p0f == 1)
    link_failure ();
  if (0x1.000001000000000000000000000000000000000000000000000001p0f == 1)
    link_failure ();
  if (0x1.0000010000000000000000000000000000000000000000000000001p0f == 1)
    link_failure ();
#endif
  return 0;
}
