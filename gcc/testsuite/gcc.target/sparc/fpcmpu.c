/* { dg-do compile } */
/* { dg-options "-mvis4" } */


typedef short vec16 __attribute__((vector_size(8)));
typedef int vec32 __attribute__((vector_size(8)));

long test_fpcmpule16 (vec16 a, vec16 b)
{
  return __builtin_vis_fpcmpule16 (a, b);
}

long test_fpcmpugt16 (vec16 a, vec16 b)
{
  return __builtin_vis_fpcmpugt16 (a, b);
}

long test_fpcmpule32 (vec32 a, vec32 b)
{
  return __builtin_vis_fpcmpule32 (a, b);
}

long test_fpcmpugt32 (vec32 a, vec32 b)
{
  return __builtin_vis_fpcmpugt32 (a, b);
}

/* { dg-final { scan-assembler "fpcmpule16\t%" } } */
/* { dg-final { scan-assembler "fpcmpugt16\t%" } } */
/* { dg-final { scan-assembler "fpcmpule32\t%" } } */
/* { dg-final { scan-assembler "fpcmpugt32\t%" } } */
