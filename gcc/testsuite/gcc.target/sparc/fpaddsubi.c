/* { dg-do compile } */
/* { dg-options "-O -mcpu=ultrasparc -mvis" } */
typedef int __v2si __attribute__((vector_size(8)));
typedef int __v1si __attribute__((vector_size(4)));
typedef short __v4hi __attribute__((vector_size(8)));
typedef short __v2hi __attribute__((vector_size(4)));

extern __v1si foo_x (void);
extern __v1si foo_y (void);

__v4hi test_fpadd16 (__v4hi x, __v4hi y)
{
  return __builtin_vis_fpadd16 (x, y);
}

__v2hi test_fpadd16s (__v2hi x, __v2hi y)
{
  return __builtin_vis_fpadd16s (x, y);
}

__v4hi test_fpsub16 (__v4hi x, __v4hi y)
{
  return __builtin_vis_fpsub16 (x, y);
}

__v2hi test_fpsub16s (__v2hi x, __v2hi y)
{
  return __builtin_vis_fpsub16s (x, y);
}

__v2si test_fpadd32 (__v2si x, __v2si y)
{
  return __builtin_vis_fpadd32 (x, y);
}

__v1si test_fpadd32s (void)
{
  return __builtin_vis_fpadd32s (foo_x (), foo_y ());
}

__v2si test_fpsub32 (__v2si x, __v2si y)
{
  return __builtin_vis_fpsub32 (x, y);
}

__v1si test_fpsub32s (__v1si x, __v1si y)
{
  return __builtin_vis_fpsub32s (foo_x (), foo_y ());
}

/* { dg-final { scan-assembler "fpadd16\t%" }  } */
/* { dg-final { scan-assembler "fpadd16s\t%" }  } */
/* { dg-final { scan-assembler "fpsub16\t%" }  } */
/* { dg-final { scan-assembler "fpsub16s\t%" }  } */
/* { dg-final { scan-assembler "fpadd32\t%" }  } */
/* { dg-final { scan-assembler "fpadd32s\t%" }  } */
/* { dg-final { scan-assembler "fpsub32\t%" }  } */
/* { dg-final { scan-assembler "fpsub32s\t%" }  } */
