/* { dg-do run } */
/* { dg-options "-O2 -m32" } */

#include <stddef.h>
#include <stdint.h>

__attribute__ ((noipa)) static void
vp9_build_inter_predictor (int a)
{
  int16_t row = a * 2;
  int32_t row_w = (int)((int64_t)row * 16384 >> 14);

  if (row_w != -544)
    __builtin_abort ();
}

int
main ()
{
  vp9_build_inter_predictor (-272);
  return 0;
}
