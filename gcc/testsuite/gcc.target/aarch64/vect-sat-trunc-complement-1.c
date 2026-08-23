/* { dg-do compile } */
/* { dg-options "-O3 -march=armv8-a" } */
/* { dg-additional-options "-mmax-vectorization --param=vect-epilogues-nomask=0 -fdump-tree-vect-details" } */

typedef __UINT16_TYPE__ u16;
typedef __UINT32_TYPE__ u32;
typedef __INT32_TYPE__ i32;
typedef __INT64_TYPE__ i64;

void
clip_u16 (u16 *__restrict out, const i32 *__restrict in, int n)
{
  for (int i = 0; i < n; ++i)
    {
      i32 x = in[i];
      out[i] = (x & ~65535) ? (~x) >> 31 : x;
    }
}

void
clip_u32 (u32 *__restrict out, const i64 *__restrict in, int n)
{
  for (int i = 0; i < n; ++i)
    {
      i64 x = in[i];
      out[i] = (x & ~(i64) 4294967295LL) ? (~x) >> 63 : x;
    }
}

/* { dg-final { scan-tree-dump-times "sat_trunc pattern recognized" 2 "vect" } } */
