/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -mavx512fp16 -mprefer-vector-width=512 -masm=att -ftrapping-math" } */
/* { dg-final { scan-assembler "vrndscaleph\[ \t]\+\\\$8,\[^\n\r]*%z" } } */

_Float16 a[32], b[32];

void
test_roundevenf (void)
{
  int i;
  for (i = 0; i < 32; ++i)
    b[i] = __builtin_roundevenf (a[i]);
}
