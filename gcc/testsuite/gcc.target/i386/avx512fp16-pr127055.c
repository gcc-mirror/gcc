/* PR target/127055 */
/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -mavx512fp16 -mprefer-vector-width=512 -masm=att -ftrapping-math" } */
/* { dg-final { scan-assembler "vrndscaleph\[ \t]\+\\\$11,\[^\n\r]*%z" } } */
/* { dg-final { scan-assembler "vrndscaleph\[ \t]\+\\\$10,\[^\n\r]*%z" } } */
/* { dg-final { scan-assembler "vrndscaleph\[ \t]\+\\\$9,\[^\n\r]*%z" } } */

_Float16 a[32], b[32];

void
test_truncf (void)
{
  int i;
  for (i = 0; i < 32; ++i)
    b[i] = __builtin_truncf (a[i]);
}

void
test_ceilf (void)
{
  int i;
  for (i = 0; i < 32; ++i)
    b[i] = __builtin_ceilf (a[i]);
}

void
test_floorf (void)
{
  int i;
  for (i = 0; i < 32; ++i)
    b[i] = __builtin_floorf (a[i]);
}
