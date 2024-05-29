/* { dg-do compile } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx -O3" } */
/* { dg-require-effective-target powerpc_vsx } */
/* { dg-final { scan-assembler "lxvd2x" } } */
/* { dg-final { scan-assembler "stxvd2x" } } */
/* { dg-final { scan-assembler "stxsdx" } } */
/* { dg-final { scan-assembler-times "xxpermdi" 1 } } */

/* The only xxpermdi expected is for the vec_splats.  */

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

int main ()
{
  foo ();
  return 0;
}
