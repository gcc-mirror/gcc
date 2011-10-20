/* { dg-do compile } */
/* { dg-options "-mvis3" } */
typedef int __v2si __attribute__((vector_size(8)));
typedef int __v1si __attribute__((vector_size(4)));
typedef short __v4hi __attribute__((vector_size(8)));
typedef short __v2hi __attribute__((vector_size(4)));

__v4hi test_fpadds16 (__v4hi x, __v4hi y)
{
  return __builtin_vis_fpadds16 (x, y);
}

__v2hi test_fpadds16s (__v2hi x, __v2hi y)
{
  return __builtin_vis_fpadds16s (x, y);
}

__v4hi test_fpsubs16 (__v4hi x, __v4hi y)
{
  return __builtin_vis_fpsubs16 (x, y);
}

__v2hi test_fpsubs16s (__v2hi x, __v2hi y)
{
  return __builtin_vis_fpsubs16s (x, y);
}

__v2si test_fpadds32 (__v2si x, __v2si y)
{
  return __builtin_vis_fpadds32 (x, y);
}

__v1si test_fpadds32s (__v1si x, __v1si y)
{
  return __builtin_vis_fpadds32s (x, y);
}

__v2si test_fpsubs32 (__v2si x, __v2si y)
{
  return __builtin_vis_fpsubs32 (x, y);
}

__v1si test_fpsubs32s (__v1si x, __v1si y)
{
  return __builtin_vis_fpsubs32s (x, y);
}

/* { dg-final { scan-assembler "fpadds16\t%" } } */
/* { dg-final { scan-assembler "fpadds16s\t%" } } */
/* { dg-final { scan-assembler "fpsubs16\t%" } } */
/* { dg-final { scan-assembler "fpsubs16s\t%" } } */
/* { dg-final { scan-assembler "fpadds32\t%" } } */
/* { dg-final { scan-assembler "fpadds32s\t%" } } */
/* { dg-final { scan-assembler "fpsubs32\t%" } } */
/* { dg-final { scan-assembler "fpsubs32s\t%" } } */
