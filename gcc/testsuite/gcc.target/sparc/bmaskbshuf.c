/* { dg-do compile } */
/* { dg-options "-O -mcpu=ultrasparc3 -mvis -mvis2" } */
typedef long long int64_t;
typedef int vec32 __attribute__((vector_size(8)));
typedef short vec16 __attribute__((vector_size(8)));
typedef unsigned char vec8 __attribute__((vector_size(8)));

long test_bmask (long x, long y)
{
  return __builtin_vis_bmask (x, y);
}

vec16 test_bshufv4hi (vec16 x, vec16 y)
{
  return __builtin_vis_bshufflev4hi (x, y);
}

vec32 test_bshufv2si (vec32 x, vec32 y)
{
  return __builtin_vis_bshufflev2si (x, y);
}

vec8 test_bshufv8qi (vec8 x, vec8 y)
{
  return __builtin_vis_bshufflev8qi (x, y);
}

int64_t test_bshufdi (int64_t x, int64_t y)
{
  return __builtin_vis_bshuffledi (x, y);
}

/* { dg-final { scan-assembler "bmask\t%" } } */
/* { dg-final { scan-assembler "bshuffle\t%" } } */
