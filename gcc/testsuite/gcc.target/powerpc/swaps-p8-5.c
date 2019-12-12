/* { dg-do compile { target { powerpc64le-*-* } } } */
/* { dg-options "-mdejagnu-cpu=power8 -O3" } */
/* { dg-final { scan-assembler "lxvd2x" } } */
/* { dg-final { scan-assembler "stxvd2x" } } */
/* { dg-final { scan-assembler-not "xxpermdi" } } */

void abort ();

#define N 4096
int ca[N] __attribute__((aligned(16)));
int cb[N] __attribute__((aligned(16)));
int cc[N] __attribute__((aligned(16)));
int cd[N] __attribute__((aligned(16)));

__attribute__((noinline)) void foo ()
{
  int i;
  for (i = 0; i < N; i++) {
    ca[i] = ((cb[i] + cc[i]) * cd[i]) >> 3;
  }
}

__attribute__((noinline)) void init ()
{
  int i;
  for (i = 0; i < N; ++i) {
    cb[i] = 3 * i - 2048;
    cc[i] = -5 * i + 93;
    cd[i] = i % 2 ? 1 : -1;
  }
}

int main ()
{
  int i;
  init ();
  foo ();
  for (i = 0; i < N; ++i)
    if (i % 2 == 1 && ca[i] != (-2 * i - 1955) >> 3)
      abort ();
    else if (i % 2 == 0 && ca[i] != (1955 + 2 * i) >> 3)
      abort ();
  return 0;
}
