/* { dg-do compile { target { powerpc64le-*-* } } } */
/* { dg-options "-mdejagnu-cpu=power8 -O3" } */
/* { dg-final { scan-assembler "lxvd2x" } } */
/* { dg-final { scan-assembler "stxvd2x" } } */
/* { dg-final { scan-assembler-not "xxpermdi" } } */

void abort();

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

int main ()
{
  signed char cd[] = {7, 6, 4, 3, 1, 0, 0, -1, -1, -2, -2, -3, -3, -4, -4, -5};
  int i;
  foo ();
  for (i = 0; i < N; ++i)
    if (ca[i] != cd[i])
      abort ();
  return 0;
}
