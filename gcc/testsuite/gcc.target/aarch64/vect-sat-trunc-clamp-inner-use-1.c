/* { dg-do compile } */
/* { dg-options "-O3 -march=armv8-a" } */
/* { dg-additional-options "-mmax-vectorization --param=vect-epilogues-nomask=0 -fdump-tree-vect-details" } */

typedef __UINT16_TYPE__ u16;
typedef __INT32_TYPE__ i32;

void
clip (u16 *__restrict out, i32 *__restrict copy,
      const i32 *__restrict in, int n)
{
  for (int i = 0; i < n; ++i)
    {
      i32 x = in[i];
      i32 inner = x > 65535 ? 65535 : x;
      i32 outer = inner < 0 ? 0 : inner;
      out[i] = outer;
      copy[i] = inner;
    }
}

/* { dg-final { scan-tree-dump-times "sat_trunc pattern recognized" 1 "vect" } } */
