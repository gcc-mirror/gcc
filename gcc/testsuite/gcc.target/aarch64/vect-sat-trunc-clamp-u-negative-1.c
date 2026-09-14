/* { dg-do compile } */
/* { dg-options "-O3 -march=armv8-a" } */
/* { dg-additional-options "-mmax-vectorization --param=vect-epilogues-nomask=0 -fdump-tree-vect-details" } */

typedef __UINT16_TYPE__ u16;
typedef __INT32_TYPE__ i32;

__attribute__((noipa))
void
bad_low (u16 *__restrict out, const i32 *__restrict in, int n)
{
  for (int i = 0; i < n; ++i)
    {
      i32 x = in[i];
      out[i] = x < 1 ? 1 : (x > 65535 ? 65535 : x);
    }
}

__attribute__((noipa))
void
bad_high (u16 *__restrict out, const i32 *__restrict in, int n)
{
  for (int i = 0; i < n; ++i)
    {
      i32 x = in[i];
      out[i] = x > 65534 ? 65534 : (x < 0 ? 0 : x);
    }
}

/* { dg-final { scan-tree-dump-not "sat_trunc pattern recognized" "vect" } } */
