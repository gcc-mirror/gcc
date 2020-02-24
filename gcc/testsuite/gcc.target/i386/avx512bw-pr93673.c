/* PR target/93673 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512bw" } */

#include <x86intrin.h>

void
foo (__mmask32 *c, __mmask64 *d)
{
  c[0] = _kshiftli_mask32 (c[0], 0);
  c[1] = _kshiftri_mask32 (c[1], 0);
  c[2] = _kshiftli_mask32 (c[2], 1);
  c[3] = _kshiftri_mask32 (c[3], 1);
  c[4] = _kshiftli_mask32 (c[4], 31);
  c[5] = _kshiftri_mask32 (c[5], 31);
  c[6] = _kshiftli_mask32 (c[6], 0x7f);
  c[7] = _kshiftri_mask32 (c[7], 0x7f);
  c[8] = _kshiftli_mask32 (c[8], 0xff);
  c[9] = _kshiftri_mask32 (c[9], 0xff);
  d[0] = _kshiftli_mask64 (d[0], 0);
  d[1] = _kshiftri_mask64 (d[1], 0);
  d[2] = _kshiftli_mask64 (d[2], 1);
  d[3] = _kshiftri_mask64 (d[3], 1);
  d[4] = _kshiftli_mask64 (d[4], 63);
  d[5] = _kshiftri_mask64 (d[5], 63);
  d[6] = _kshiftli_mask64 (d[6], 0x7f);
  d[7] = _kshiftri_mask64 (d[7], 0x7f);
  d[8] = _kshiftli_mask64 (d[8], 0xff);
  d[9] = _kshiftri_mask64 (d[9], 0xff);
}
