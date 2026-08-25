/* { dg-do compile } */
/* { dg-additional-options "-mno-avx" { target { x86_64-*-* i?86-*-* } } } */

int b(int c, int d, int e, int f, int g, int h)
{
  long long a = c;
  a += d;
  a += e;
  a += f;
  a += g;
  a += h;
  return a;
}

/* With SSE we should be able to reduce all 6 lanes with V2DI, two
   vec_unpack_lo_expr and one vec_unpack_hi_expr (dumped twice).  */
/* { dg-final { scan-tree-dump-times "vec_unpack" 6 "slp2" { target { x86_64-*-* i?86-*-* } } } } */
