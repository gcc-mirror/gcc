/* { dg-do run { target { powerpc64le-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power8" } } */
/* { dg-options "-mcpu=power8 -O3" } */

void abort ();

#define N 256
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
  int i;
  for (i = 0; i < N; ++i) {
    cb[i] = i - 128;
    cc[i] = i/2 - 64;
  }
}

int main ()
{
  int i;
  init ();
  foo ();
  for (i = 0; i < N; ++i)
    if (ca[i] != i - i/2 - 64)
      abort ();
  return 0;
}
