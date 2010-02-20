/* { dg-do compile } */
/* { dg-options "-O1 -mxop -ftree-vectorize -fschedule-insns" } */

union {
  int i32[10240];
  long long i64[10240];
} a, b, c;

void imul32_to_64 (void)
{
  int i;

  for (i = 0; i < 10240; i++)
    a.i64[i] = (long long) b.i32[i] * c.i32[i];
}
