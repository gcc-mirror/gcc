/* { dg-do compile { target powerpc*-*-* ia64-*-* i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -fschedule-insns -fselective-scheduling -funroll-loops -fno-gcse -fno-if-conversion -fno-ivopts" } */

#define N 4096
int cb[N];
int cc[N];
int cd[N];

void init ()
{
  int i;
  for (i = 0; i < N; ++i) {
    cb[i] = 3 * i - 2048;
    cc[i] = -5 * i + 93;
    cd[i] = i % 2 ? 1 : -1;
  }
}
