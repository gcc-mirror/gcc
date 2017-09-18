/* { dg-do compile } */
/* { dg-options "-mvis4b" } */

typedef unsigned char vec8 __attribute__((vector_size(8)));
typedef short vec16 __attribute__((vector_size(8)));
typedef int vec32 __attribute__((vector_size(8)));

long test_fpcmple8shl (vec8 a, vec8 b)
{
  return __builtin_vis_fpcmple8shl (a, b, 2);
}

long test_fpcmpgt8shl (vec8 a, vec8 b)
{
  return __builtin_vis_fpcmpgt8shl (a, b, 2);
}

long test_fpcmpeq8shl (vec8 a, vec8 b)
{
  return __builtin_vis_fpcmpeq8shl (a, b, 2);
}

long test_fpcmpne8shl (vec8 a, vec8 b)
{
  return __builtin_vis_fpcmpne8shl (a, b, 2);
}

long test_fpcmple16shl (vec16 a, vec16 b)
{
  return __builtin_vis_fpcmple16shl (a, b, 2);
}

long test_fpcmpgt16shl (vec16 a, vec16 b)
{
  return __builtin_vis_fpcmpgt16shl (a, b, 2);
}

long test_fpcmpeq16shl (vec16 a, vec16 b)
{
  return __builtin_vis_fpcmpeq16shl (a, b, 2);
}

long test_fpcmpne16shl (vec16 a, vec16 b)
{
  return __builtin_vis_fpcmpne16shl (a, b, 2);
}

long test_fpcmple32shl (vec32 a, vec32 b)
{
  return __builtin_vis_fpcmple32shl (a, b, 2);
}

long test_fpcmpgt32shl (vec32 a, vec32 b)
{
  return __builtin_vis_fpcmpgt32shl (a, b, 2);
}

long test_fpcmpeq32shl (vec32 a, vec32 b)
{
  return __builtin_vis_fpcmpeq32shl (a, b, 2);
}

long test_fpcmpne32shl (vec32 a, vec32 b)
{
  return __builtin_vis_fpcmpne32shl (a, b, 2);
}

/* { dg-final { scan-assembler "fpcmple8shl\t%" } } */
/* { dg-final { scan-assembler "fpcmpgt8shl\t%" } } */
/* { dg-final { scan-assembler "fpcmpeq8shl\t%" } } */
/* { dg-final { scan-assembler "fpcmpne8shl\t%" } } */

/* { dg-final { scan-assembler "fpcmple16shl\t%" } } */
/* { dg-final { scan-assembler "fpcmpgt16shl\t%" } } */
/* { dg-final { scan-assembler "fpcmpeq16shl\t%" } } */
/* { dg-final { scan-assembler "fpcmpne16shl\t%" } } */

/* { dg-final { scan-assembler "fpcmple32shl\t%" } } */
/* { dg-final { scan-assembler "fpcmpgt32shl\t%" } } */
/* { dg-final { scan-assembler "fpcmpeq32shl\t%" } } */
/* { dg-final { scan-assembler "fpcmpne32shl\t%" } } */
