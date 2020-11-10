/* { dg-do run } */
/* { dg-require-effective-target p8vector_hw } */
/* { dg-options "-mdejagnu-cpu=power8 -O3" } */

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
