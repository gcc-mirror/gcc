#ifndef HAVE_DEFINED_VF_BINOP_WIDEN_RUN_H
#define HAVE_DEFINED_VF_BINOP_WIDEN_RUN_H

#include <assert.h>

#define N 512

int main ()
{
  T1 f;
  T1 in[N];
  T2 out[N];
  T2 out2[N];

  f = LIMIT % 8723;
  for (int i = 0; i < N; i++) 
    {
      in[i] = LIMIT + i & 1964;
      out[i] = LIMIT + i & 628;
      out2[i] = LIMIT + i & 628;
      asm volatile ("" ::: "memory");
    }

  TEST_RUN (T1, T2, NAME, out, in, f, N);

  for (int i = 0; i < N; i++)
    assert (out[i] == ((T2) f OP (T2) in[i]));

  return 0;
}

#endif
