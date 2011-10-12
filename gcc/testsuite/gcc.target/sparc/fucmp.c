/* { dg-do compile } */
/* { dg-options "-mvis3" } */
typedef unsigned char vec8 __attribute__((vector_size(8)));

long test_fucmple8 (vec8 a, vec8 b)
{
  return __builtin_vis_fucmple8 (a, b);
}

long test_fucmpne8 (vec8 a, vec8 b)
{
  return __builtin_vis_fucmpne8 (a, b);
}

long test_fucmpgt8 (vec8 a, vec8 b)
{
  return __builtin_vis_fucmpgt8 (a, b);
}

long test_fucmpeq8 (vec8 a, vec8 b)
{
  return __builtin_vis_fucmpeq8 (a, b);
}

/* { dg-final { scan-assembler "fucmple8\t%" } } */
/* { dg-final { scan-assembler "fucmpne8\t%" } } */
/* { dg-final { scan-assembler "fucmpgt8\t%" } } */
/* { dg-final { scan-assembler "fucmpeq8\t%" } } */
