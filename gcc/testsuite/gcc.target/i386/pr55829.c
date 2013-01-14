/* { dg-do compile } */
/* { dg-options "-O2 -msse3 -fno-expensive-optimizations" } */

typedef double __m128d __attribute__ ((__vector_size__ (16)));

extern double p1[];
extern double p2[];
extern double ck[];
extern int n;

__attribute__((__noinline__, __noclone__)) int chk_pd (double *v1, double *v2)
{
  return v2[n] != v1[n];
}

static inline void sse3_test_movddup_reg_subsume_ldsd (double *i1, double *r)
{
  __m128d t1 = (__m128d){*i1, 0};
  __m128d t2 = __builtin_ia32_shufpd (t1, t1, 0);
  __builtin_ia32_storeupd (r, t2);
}

int sse3_test (void)
{
  int i = 0;
  int fail = 0;
  for (; i < 80; i += 1)
    {
      ck[0] = p1[0];
      fail += chk_pd (ck, p2);
      sse3_test_movddup_reg_subsume_ldsd (p1, p2);
    }
  return fail;
}
