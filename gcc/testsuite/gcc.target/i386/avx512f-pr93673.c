/* PR target/93673 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f" } */

#include <x86intrin.h>

void
foo (__mmask16 *b)
{
  b[0] = _kshiftli_mask16 (b[0], 0);
  b[1] = _kshiftri_mask16 (b[1], 0);
  b[2] = _kshiftli_mask16 (b[2], 1);
  b[3] = _kshiftri_mask16 (b[3], 1);
  b[4] = _kshiftli_mask16 (b[4], 15);
  b[5] = _kshiftri_mask16 (b[5], 15);
  b[6] = _kshiftli_mask16 (b[6], 0x7f);
  b[7] = _kshiftri_mask16 (b[7], 0x7f);
  b[8] = _kshiftli_mask16 (b[8], 0xff);
  b[9] = _kshiftri_mask16 (b[9], 0xff);
}
