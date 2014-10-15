/* { dg-do run } */
/* { dg-require-effective-target sse4a } */
/* { dg-options "-O2 -msse4a" } */

#include "sse4a-check.h"

#include <ammintrin.h>

typedef union
{
  long long i[2];
  __m128i vec;
} LI;

static long long 
sse4a_test_extrq (long long in)
{
  __m128i v1, v2;
  long long index_length, pad;
  LI v_out;
  index_length = 0x0000000000000810LL; 
  pad = 0x0;
  v1 = _mm_set_epi64x (pad, in);
  v2 = _mm_set_epi64x (pad, index_length); 
  v_out.vec = _mm_extract_si64 (v1, v2);
  return (v_out.i[0]); 
}

static long long 
sse4a_test_extrqi (long long in)
{
  __m128i v1;
  long long pad =0x0;
  LI v_out;
  v1 = _mm_set_epi64x (pad, in);
  v_out.vec = _mm_extracti_si64 (v1, (unsigned int) 0x10,(unsigned int) 0x08);
  return (v_out.i[0]);
}

static int chk (long long i1, long long i2)
{
  int n_fails =0;
  if (i1 != i2) 
    n_fails +=1;
  return n_fails;
}

long long vals_in[5] =
  {
    0x1234567887654321LL,
    0x1456782093002490LL,
    0x2340909123990390LL,
    0x9595959599595999LL,
    0x9099038798000029LL
  };

long long vals_out[5] =
  {
    0x0000000000006543LL,
    0x0000000000000024LL,
    0x0000000000009903LL,
    0x0000000000005959LL,
    0x0000000000000000LL
  };

static void
sse4a_test (void)
{
  int i;
  int fail = 0;
  long long out;

  for (i = 0; i < 5; i += 1)
    {
      out = sse4a_test_extrq (vals_in[i]);
      fail += chk(out, vals_out[i]);

      out = sse4a_test_extrqi (vals_in[i]);
      fail += chk(out, vals_out[i]);
    }

  if (fail != 0)
    abort ();
}
