/* { dg-do compile } */
/* { dg-options "-mvis4b" } */

typedef unsigned char vec8 __attribute__((vector_size(8)));
typedef short vec16 __attribute__((vector_size(8)));
typedef int vec32 __attribute__((vector_size(8)));

long test_fpcmpde8shl (vec8 a, vec8 b)
{
  return __builtin_vis_fpcmpde8shl (a, b, 2);
}

long test_fpcmpde16shl (vec16 a, vec16 b)
{
  return __builtin_vis_fpcmpde16shl (a, b, 2);
}

long test_fpcmpde32shl (vec32 a, vec32 b)
{
  return __builtin_vis_fpcmpde32shl (a, b, 2);
}

/* { dg-final { scan-assembler "fpcmpde8shl\t%" } } */
/* { dg-final { scan-assembler "fpcmpde16shl\t%" } } */
/* { dg-final { scan-assembler "fpcmpde32shl\t%" } } */
