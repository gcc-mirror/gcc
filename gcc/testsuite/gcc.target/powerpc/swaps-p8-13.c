/* { dg-do run { target { powerpc64le-*-* } } } */
/* { dg-options "-mdejagnu-cpu=power8 -O3" } */

#include <altivec.h>
void abort ();

#define N 4096
long long ca[N] __attribute__((aligned(16)));
long long cb[N] __attribute__((aligned(16)));
long long cc[N] __attribute__((aligned(16)));
long long cd[N] __attribute__((aligned(16)));
long long x;

__attribute__((noinline)) void foo ()
{
  int i;
  vector long long va, vb, vc, vd, tmp;
  volatile unsigned long long three = 3;
  vector unsigned long long threes = vec_splats (three);
  for (i = 0; i < N; i+=2) {
    vb = vec_vsx_ld (0, (vector long long *)&cb[i]);
    vc = vec_vsx_ld (0, (vector long long *)&cc[i]);
    vd = vec_vsx_ld (0, (vector long long *)&cd[i]);
    tmp = vec_add (vb, vc);
    tmp = vec_sub (tmp, vd);
    tmp = vec_sra (tmp, threes);
    x = vec_extract (tmp, 0);
    vec_vsx_st (tmp, 0, (vector long long *)&ca[i]);
  }
}

__attribute__((noinline)) void init ()
{
  int i;
  for (i = 0; i < N; ++i) {
    cb[i] = 3 * i - 2048;
    cc[i] = -5 * i + 93;
    cd[i] = i + 14;
  }
}

int main ()
{
  int i;
  init ();
  foo ();
  for (i = 0; i < N; ++i)
    if (ca[i] != (-3 * i - 1969) >> 3)
      abort ();
  if (x != ca[N-1])
    abort ();
  return 0;
}
