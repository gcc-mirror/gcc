/* { dg-do compile } */
/* { dg-options "-mvis4b" } */

typedef unsigned char vec8 __attribute__((vector_size(8)));
typedef short vec16 __attribute__((vector_size(8)));
typedef int vec32 __attribute__((vector_size(8)));

long test_fpcmpur8shl (vec8 a, vec8 b)
{
  return __builtin_vis_fpcmpur8shl (a, b, 2);
}

long test_fpcmpur16shl (vec16 a, vec16 b)
{
  return __builtin_vis_fpcmpur16shl (a, b, 2);
}

long test_fpcmpur32shl (vec32 a, vec32 b)
{
  return __builtin_vis_fpcmpur32shl (a, b, 2);
}

/* { dg-final { scan-assembler "fpcmpur8shl\t%" } } */
/* { dg-final { scan-assembler "fpcmpur16shl\t%" } } */
/* { dg-final { scan-assembler "fpcmpur32shl\t%" } } */
