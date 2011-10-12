/* { dg-do compile } */
/* { dg-options "-mvis3" } */
typedef int __v2si __attribute__((vector_size(8)));
typedef short __v4hi __attribute__((vector_size(8)));

__v4hi test_fsll16 (__v4hi x, __v4hi y)
{
  return __builtin_vis_fsll16 (x, y);
}

__v4hi test_fslas16 (__v4hi x, __v4hi y)
{
  return __builtin_vis_fslas16 (x, y);
}

__v4hi test_fsrl16 (__v4hi x, __v4hi y)
{
  return __builtin_vis_fsrl16 (x, y);
}

__v4hi test_fsra16 (__v4hi x, __v4hi y)
{
  return __builtin_vis_fsra16 (x, y);
}

__v2si test_fsll32 (__v2si x, __v2si y)
{
  return __builtin_vis_fsll32 (x, y);
}

__v2si test_fslas32 (__v2si x, __v2si y)
{
  return __builtin_vis_fslas32 (x, y);
}

__v2si test_fsrl32 (__v2si x, __v2si y)
{
  return __builtin_vis_fsrl32 (x, y);
}

__v2si test_fsra32 (__v2si x, __v2si y)
{
  return __builtin_vis_fsra32 (x, y);
}

/* { dg-final { scan-assembler "fsll16\t%" } } */
/* { dg-final { scan-assembler "fslas16\t%" } } */
/* { dg-final { scan-assembler "fsrl16\t%" } } */
/* { dg-final { scan-assembler "fsra16\t%" } } */
/* { dg-final { scan-assembler "fsll32\t%" } } */
/* { dg-final { scan-assembler "fslas32\t%" } } */
/* { dg-final { scan-assembler "fsrl32\t%" } } */
/* { dg-final { scan-assembler "fsra32\t%" } } */
