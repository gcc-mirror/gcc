/* { dg-do compile } */
/* { dg-options "-mcpu=ultrasparc -mvis" } */

long test_array8 (long a, long b)
{
  return __builtin_vis_array8 (a, b);
}

long test_array16 (long a, long b)
{
  return __builtin_vis_array16 (a, b);
}

long test_array32 (long a, long b)
{
  return __builtin_vis_array32 (a, b);
}

/* { dg-final { scan-assembler "array8\t%" } } */
/* { dg-final { scan-assembler "array16\t%" } } */
/* { dg-final { scan-assembler "array32\t%" } } */
