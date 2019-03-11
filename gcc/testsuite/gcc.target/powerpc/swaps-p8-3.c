/* { dg-do compile { target { powerpc64le-*-* } } } */
/* { dg-options "-mdejagnu-cpu=power8 -O3" } */
/* { dg-final { scan-assembler "lxvd2x" } } */
/* { dg-final { scan-assembler "stxvd2x" } } */
/* { dg-final { scan-assembler-not "xxpermdi" } } */

void abort ();

#define N 4096
signed char ca[N] __attribute__((aligned(16)));
signed char cb[N] __attribute__((aligned(16)));
signed char cc[N] __attribute__((aligned(16)));

__attribute__((noinline)) void foo ()
{
  int i;
  for (i = 0; i < N; i++) {
    ca[i] = cb[i] - cc[i];
  }
}

__attribute__((noinline)) void init ()
{
  int i, ii;
  for (i = 0, ii = 0; i < N; ++i, ii = (ii + 1) % 128) {
    cb[i] = ii - 128;
    cc[i] = ii/2 - 64;
  }
}

int main ()
{
  int i, ii;
  init ();
  foo ();
  for (i = 0; i < N; ++i) {
    ii = i % 128;
    if (ca[i] != ii - ii/2 - 64)
      abort ();
  }
  return 0;
}
