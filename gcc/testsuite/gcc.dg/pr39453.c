/* { dg-do compile { target powerpc*-*-* ia64-*-* i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -fselective-scheduling2 -fsel-sched-pipelining -fsel-sched-pipelining-outer-loops" } */

int foo(int k, int n)
{
  int i;
  for (i = 0; i < n; i += 8) {
    int j;
    for (j = 0; j < n; j += 8) {
      while (k < n) {
        k += 8;
      }
    }
  }
  return k;
}


