/* { dg-do compile } */
/* { dg-additional-options "-ffast-math -fdump-tree-optimized" } */
/* { dg-additional-options "-mavx2" { target x86_64-*-* i?86-*-* } } */
/* { dg-require-effective-target vect_double } */
/* { dg-require-effective-target vect_unpack } */
/* { dg-require-effective-target vect_intdouble_cvt } */
/* { dg-require-effective-target vect_perm } */

struct pixels
{
  short a,b,c,d;
} *pixels;
struct dpixels
{
  double a,b,c,d;
};

double
test(double *k)
{
  struct dpixels results={};
  for (int u=0; u<1000*16;u++,k--)
    {
      results.a += *k*pixels[u].a;
      results.b += *k*pixels[u].b;
      results.c += *k*pixels[u].c;
      results.d += *k*pixels[u].d;
    }
  return results.a+results.b*2+results.c*3+results.d*4;
}

/* { dg-final { scan-tree-dump "vectorized 1 loops" "vect" } } */
/* { dg-final { scan-tree-dump-times "VEC_PERM" 4 "optimized" { target x86_64-*-* i?86-*-* } } } */
