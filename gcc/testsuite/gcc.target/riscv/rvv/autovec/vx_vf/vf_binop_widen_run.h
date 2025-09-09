#ifndef HAVE_DEFINED_VF_BINOP_WIDEN_RUN_H
#define HAVE_DEFINED_VF_BINOP_WIDEN_RUN_H

#include <assert.h>

#define N 512

#ifdef SINGLE
#define TIN T2
#else
#define TIN T1
#endif

int main ()
{
  T1 f;
  TIN in[N];
  T2 out[N];

  f = LIMIT % 8723;
  for (int i = 0; i < N; i++) 
    {
      in[i] = LIMIT + i & 1964;
      out[i] = LIMIT + i & 628;
      asm volatile ("" ::: "memory");
    }

  TEST_RUN (T1, T2, NAME, out, in, f, N);

  for (int i = 0; i < N; i++)
    assert (out[i] == ((T2) in[i] OP (T2) f));

  return 0;
}

#endif
