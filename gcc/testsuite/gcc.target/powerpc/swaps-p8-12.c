/* { dg-do compile { target le } } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx -O3" } */
/* { dg-require-effective-target powerpc_vsx } */
/* { dg-final { scan-assembler "lxvd2x" } } */
/* { dg-final { scan-assembler "stxvd2x" } } */
/* { dg-final { scan-assembler-not "xxpermdi" } } */

#include "altivec.h"
void abort ();

#define N 4096
int ca[N] __attribute__((aligned(16)));
int cb[N] __attribute__((aligned(16)));
int cc[N] __attribute__((aligned(16)));
int cd[N] __attribute__((aligned(16)));
int hey;

__attribute__((noinline)) void foo ()
{
  int i;
  vector int va, vb, vc, vd, tmp;
  vector unsigned int threes = vec_splat_u32(3);
  for (i = 0; i < N; i+=4) {
    vb = vec_vsx_ld (0, &cb[i]);
    vc = vec_vsx_ld (0, &cc[i]);
    vd = vec_vsx_ld (0, &cd[i]);
    tmp = vec_add (vb, vc);
    tmp = vec_sub (tmp, vd);
    tmp = vec_sra (tmp, threes);
    hey = tmp[3];
    vec_vsx_st (tmp, 0, &ca[i]);
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
  if (hey != ca[N-1])
    abort ();
  return 0;
}
