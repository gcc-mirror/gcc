/* PR tree-optimization/103376 */
/* { dg-do compile } */
/* { dg-require-effective-target bswap } */
/* { dg-options "-O2 -fno-tree-vectorize -fdump-tree-optimized" } */
/* { dg-additional-options "-march=z900" { target s390-*-* } } */

static unsigned int
f1 (unsigned int x)
{
  return (x << 24) | (x >> 8);
}

unsigned int
f2 (unsigned *p)
{
  return ((f1 (p[0]) | (p[0] >> 8)) & 0xff000000U) | (p[0] >> 24) | ((p[0] & 0xff00U) << 8) | ((p[0] & 0xff0000U) >> 8);
}

unsigned int
f3 (unsigned *p)
{
  return ((f1 (p[0]) | (p[0] & 0x00ff00ffU)) & 0xff00ff00U) | (f1 (f1 (f1 (p[0]))) & 0x00ff00ffU);
}

unsigned int
f4 (unsigned *p)
{
  return (f1 (p[0]) ^ (p[0] >> 8)) ^ (p[0] >> 24) ^ ((p[0] & 0xff00U) << 8) ^ ((p[0] & 0xff0000U) >> 8);
}

unsigned int
f5 (unsigned *p)
{
  return (((f1 (p[0]) | (p[0] >> 16)) ^ (p[0] >> 8)) & 0xffff0000U) ^ (p[0] >> 24) ^ ((p[0] & 0xff00U) << 8) ^ ((p[0] & 0xff0000U) >> 8);
}

/* { dg-final { scan-tree-dump-times "= __builtin_bswap32 \\\(" 4 "optimized" } } */
