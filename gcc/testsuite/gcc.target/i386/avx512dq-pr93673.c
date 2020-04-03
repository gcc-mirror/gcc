/* PR target/93673 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512dq" } */

#include <x86intrin.h>

void
foo (__mmask8 *a)
{
  a[0] = _kshiftli_mask8 (a[0], 0);
  a[1] = _kshiftri_mask8 (a[1], 0);
  a[2] = _kshiftli_mask8 (a[2], 1);
  a[3] = _kshiftri_mask8 (a[3], 1);
  a[4] = _kshiftli_mask8 (a[4], 7);
  a[5] = _kshiftri_mask8 (a[5], 7);
  a[6] = _kshiftli_mask8 (a[6], 0x7f);
  a[7] = _kshiftri_mask8 (a[7], 0x7f);
  a[8] = _kshiftli_mask8 (a[8], 0xff);
  a[9] = _kshiftri_mask8 (a[9], 0xff);
}
