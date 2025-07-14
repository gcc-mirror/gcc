#ifndef HAVE_DEFINED_VF_MULOP_WIDEN_RUN_H
#define HAVE_DEFINED_VF_MULOP_WIDEN_RUN_H

#include <assert.h>

#define N 512

int main ()
{
  T1 f[N];
  T1 in[N];
  T2 out[N];
  T2 out2[N];

  for (int i = 0; i < N; i++) 
    {
      f[i] = LIMIT + i % 8723;
      in[i] = LIMIT + i & 1964;
      out[i] = LIMIT + i & 628;
      out2[i] = LIMIT + i & 628;
      asm volatile ("" ::: "memory");
    }

  TEST_RUN (T1, T2, NAME, out, in, f, N);

  for (int i = 0; i < N; i++)
    assert (out[i] == NEG(((T2) *f * (T2) in[i]) OP out2[i]));

  return 0;
}

#endif
