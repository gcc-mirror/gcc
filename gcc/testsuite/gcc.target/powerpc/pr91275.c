/* Test that we generate vpmsumd correctly without a swap error.  */

/* { dg-do run { target { p8vector_hw } } } */
/* { dg-options "-O2 -std=gnu11 -mcpu=power8" } */

#include <altivec.h>

int main() {

  const unsigned long long r0l = 0x8e7dfceac070e3a0;
  vector unsigned long long r0 = (vector unsigned long long) {r0l, 0}, v;
  const vector unsigned long long pd
    = (vector unsigned long long) {0xc2LLU << 56, 0};

  v = __builtin_crypto_vpmsumd ((vector unsigned long long) {r0[0], 0}, pd);

#if __LITTLE_ENDIAN__
  if (v[0] != 0x4000000000000000 || v[1] != 0x65bd7ab605a4a8ff)
    __builtin_abort ();
#else
  if (v[1] != 0x4000000000000000 || v[0] != 0x65bd7ab605a4a8ff)
    __builtin_abort ();
#endif

  return 0;
}
