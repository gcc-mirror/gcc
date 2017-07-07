/* { dg-do compile } */
/* { dg-options "-mvis4b" } */

typedef unsigned char vec8 __attribute__((vector_size(8)));
typedef short vec16 __attribute__((vector_size(8)));
typedef int vec32 __attribute__((vector_size(8)));

vec8 test_dictunpack8 (double a)
{
  return __builtin_vis_dictunpack8 (a, 6);
}

vec16 test_dictunpack16 (double a)
{
  return __builtin_vis_dictunpack16 (a, 14);
}

vec32 test_dictunpack32 (double a)
{
  return __builtin_vis_dictunpack32 (a, 30);
}

/* { dg-final { scan-assembler "dictunpack\t%" } } */
/* { dg-final { scan-assembler "dictunpack\t%" } } */
/* { dg-final { scan-assembler "dictunpack\t%" } } */
