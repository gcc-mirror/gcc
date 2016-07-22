/* { dg-do compile } */
/* { dg-options "-mvis4" } */
typedef int __v2si __attribute__((vector_size(8)));
typedef short __v4hi __attribute__((vector_size(8)));
typedef unsigned char __v8qi __attribute__((vector_size(8)));

__v8qi test_fpadd8 (__v8qi x, __v8qi y)
{
  return __builtin_vis_fpadd8 (x, y);
}

__v8qi test_fpadds8 (__v8qi x, __v8qi y)
{
  return __builtin_vis_fpadds8 (x, y);
}

__v8qi test_fpaddus8 (__v8qi x, __v8qi y)
{
  return __builtin_vis_fpaddus8 (x, y);
}

__v4hi test_fpaddus16 (__v4hi x, __v4hi y)
{
  return __builtin_vis_fpaddus16 (x, y);
}

__v8qi test_fpsub8 (__v8qi x, __v8qi y)
{
  return __builtin_vis_fpsub8 (x, y);
}

__v8qi test_fpsubs8 (__v8qi x, __v8qi y)
{
  return __builtin_vis_fpsubs8 (x, y);
}

__v8qi test_fpsubus8 (__v8qi x, __v8qi y)
{
  return __builtin_vis_fpsubus8 (x, y);
}

__v4hi test_fpsubus16 (__v4hi x, __v4hi y)
{
  return __builtin_vis_fpsubus16 (x, y);
}

__v8qi test_fpmax8 (__v8qi x, __v8qi y)
{
  return __builtin_vis_fpmax8 (x, y);
}

__v4hi test_fpmax16 (__v4hi x, __v4hi y)
{
  return __builtin_vis_fpmax16 (x, y);
}

__v2si test_fpmax32 (__v2si x, __v2si y)
{
  return __builtin_vis_fpmax32 (x, y);
}

__v8qi test_fpmaxu8 (__v8qi x, __v8qi y)
{
  return __builtin_vis_fpmaxu8 (x, y);
}

__v4hi test_fpmaxu16 (__v4hi x, __v4hi y)
{
  return __builtin_vis_fpmaxu16 (x, y);
}

__v2si test_fpmaxu32 (__v2si x, __v2si y)
{
  return __builtin_vis_fpmaxu32 (x, y);
}

__v8qi test_fpmin8 (__v8qi x, __v8qi y)
{
  return __builtin_vis_fpmin8 (x, y);
}

__v4hi test_fpmin16 (__v4hi x, __v4hi y)
{
  return __builtin_vis_fpmin16 (x, y);
}

__v2si test_fpmin32 (__v2si x, __v2si y)
{
  return __builtin_vis_fpmin32 (x, y);
}

__v8qi test_fpminu8 (__v8qi x, __v8qi y)
{
  return __builtin_vis_fpminu8 (x, y);
}

__v4hi test_fpminu16 (__v4hi x, __v4hi y)
{
  return __builtin_vis_fpminu16 (x, y);
}

__v2si test_fpminu32 (__v2si x, __v2si y)
{
  return __builtin_vis_fpminu32 (x, y);
}

/* { dg-final { scan-assembler "fpadd8\t%" } } */
/* { dg-final { scan-assembler "fpadds8\t%" } } */
/* { dg-final { scan-assembler "fpaddus8\t%" } } */
/* { dg-final { scan-assembler "fpaddus16\t%" } } */
/* { dg-final { scan-assembler "fpsub8\t%" } } */
/* { dg-final { scan-assembler "fpsubs8\t%" } } */
/* { dg-final { scan-assembler "fpsubus8\t%" } } */
/* { dg-final { scan-assembler "fpsubus16\t%" } } */
/* { dg-final { scan-assembler "fpmax8\t%" } } */
/* { dg-final { scan-assembler "fpmax16\t%" } } */
/* { dg-final { scan-assembler "fpmax32\t%" } } */
/* { dg-final { scan-assembler "fpmaxu8\t%" } } */
/* { dg-final { scan-assembler "fpmaxu16\t%" } } */
/* { dg-final { scan-assembler "fpmaxu32\t%" } } */
/* { dg-final { scan-assembler "fpmin8\t%" } } */
/* { dg-final { scan-assembler "fpmin16\t%" } } */
/* { dg-final { scan-assembler "fpmin32\t%" } } */
/* { dg-final { scan-assembler "fpminu8\t%" } } */
/* { dg-final { scan-assembler "fpminu16\t%" } } */
/* { dg-final { scan-assembler "fpminu32\t%" } } */
