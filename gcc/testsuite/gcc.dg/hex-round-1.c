/* Test for hexadecimal float rounding: bug 21720.  */
/* { dg-do link } */
/* { dg-options "-O -std=gnu99" } */

#include <float.h>

extern void link_failure (void);

int
main (void)
{
#if FLT_RADIX == 2 && FLT_MANT_DIG == 24
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
#endif
  return 0;
}
