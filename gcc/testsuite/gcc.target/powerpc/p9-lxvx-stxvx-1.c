/* { dg-do compile { target { powerpc64le-*-* } } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -O3" } */
/* { dg-final { scan-assembler "lxvx" } } */
/* { dg-final { scan-assembler "stxvx" } } */
/* { dg-final { scan-assembler-not "lxvd2x" } } */
/* { dg-final { scan-assembler-not "stxvd2x" } } */
/* { dg-final { scan-assembler-not "xxpermdi" } } */

/* Verify P9 vector loads and stores are used rather than the
   load-swap/swap-store workarounds for P8.  */
#define N 16

signed char ca[N] __attribute__((aligned(16)));
signed char cb[] __attribute__((aligned(16)))
  = {8, 7, 6, 5, 4, 3, 2,  1,  0, -1, -2, -3, -4, -5, -6, -7};
signed char cc[] __attribute__((aligned(16)))
  = {1, 1, 2, 2, 3, 3, 2,  2,  1,  1,  0,  0, -1, -1, -2, -2};

__attribute__((noinline)) void foo ()
{
  int i;
  for (i = 0; i < N; i++) {
    ca[i] = cb[i] - cc[i];
  }
}
