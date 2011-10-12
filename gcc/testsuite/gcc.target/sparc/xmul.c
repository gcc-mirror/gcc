/* { dg-do compile } */
/* { dg-options "-mvis3" } */
typedef long long int64_t;

int64_t test_umulxhi (int64_t x, int64_t y)
{
  return __builtin_vis_umulxhi (x, y);
}

int64_t test_xmulx (int64_t x, int64_t y)
{
  return __builtin_vis_xmulx (x, y);
}

int64_t test_xmulxhi (int64_t x, int64_t y)
{
  return __builtin_vis_xmulxhi (x, y);
}

/* { dg-final { scan-assembler "umulxhi\t%" } } */
/* { dg-final { scan-assembler "xmulx\t%" } } */
/* { dg-final { scan-assembler "xmulxhi\t%" } } */
