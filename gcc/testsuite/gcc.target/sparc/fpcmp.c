/* { dg-do compile } */
/* { dg-options "-mvis4" } */

typedef unsigned char vec8 __attribute__((vector_size(8)));

long test_fpcmple8 (vec8 a, vec8 b)
{
  return __builtin_vis_fpcmple8 (a, b);
}

long test_fpcmpgt8 (vec8 a, vec8 b)
{
  return __builtin_vis_fpcmpgt8 (a, b);
}

/* { dg-final { scan-assembler "fpcmple8\t%" } } */
/* { dg-final { scan-assembler "fpcmpgt8\t%" } } */

