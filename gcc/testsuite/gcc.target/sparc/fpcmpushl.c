/* { dg-do compile } */
/* { dg-options "-mvis4b" } */

typedef unsigned char vec8 __attribute__((vector_size(8)));
typedef short vec16 __attribute__((vector_size(8)));
typedef int vec32 __attribute__((vector_size(8)));

long test_fpcmpule8shl (vec8 a, vec8 b)
{
  return __builtin_vis_fpcmpule8shl (a, b, 2);
}

long test_fpcmpugt8shl (vec8 a, vec8 b)
{
  return __builtin_vis_fpcmpugt8shl (a, b, 2);
}

long test_fpcmpule16shl (vec16 a, vec16 b)
{
  return __builtin_vis_fpcmpule16shl (a, b, 2);
}

long test_fpcmpugt16shl (vec16 a, vec16 b)
{
  return __builtin_vis_fpcmpugt16shl (a, b, 2);
}

long test_fpcmpule32shl (vec32 a, vec32 b)
{
  return __builtin_vis_fpcmpule32shl (a, b, 2);
}

long test_fpcmpugt32shl (vec32 a, vec32 b)
{
  return __builtin_vis_fpcmpugt32shl (a, b, 2);
}

/* { dg-final { scan-assembler "fpcmpule8shl\t%" } } */
/* { dg-final { scan-assembler "fpcmpugt8shl\t%" } } */
/* { dg-final { scan-assembler "fpcmpule16shl\t%" } } */
/* { dg-final { scan-assembler "fpcmpugt16shl\t%" } } */
/* { dg-final { scan-assembler "fpcmpule32shl\t%" } } */
/* { dg-final { scan-assembler "fpcmpugt32shl\t%" } } */
