/* PR target/94500 */
/* { dg-do run { target avx512bw } } */
/* { dg-options "-O3 -mavx512bw -mprefer-vector-width=512" } */

#define AVX512BW
#include "avx512f-helper.h"

__attribute__((noipa)) signed char
foo (signed char *p)
{
  signed char r = 0;
  int i;
  for (i = 0; i < 256; i++)
    if (p[i] > r) r = p[i];
  return r;
}

signed char buf[256];

static void
TEST (void)
{
  int i;
  for (i = 0; i < 256; i++)
    buf[i] = i - 128;
  if (foo (buf) != 127)
    abort ();
}
