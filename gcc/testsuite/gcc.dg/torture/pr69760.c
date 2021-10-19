/* PR tree-optimization/69760 */
/* { dg-do run { target { { *-*-linux* *-*-gnu* *-*-uclinux* } && mmap } } } */

#include <unistd.h>
#include <sys/mman.h>

__attribute__((noinline, noclone)) static void
test_func (double *a, int L, int m, int n, int N)
{
  int i, k;
  for (i = 0; i < N; i++)
    {
      k = i - m;
      if (k >= 0 && k < n)
	a[L * k] = 0.0;
    }
}

int
main ()
{
  char *p;
  int L, m, n, N;
  long l;
  L = 10000000;
  n = 4;
  N = 100 * n;
  long pgsz = sysconf(_SC_PAGESIZE);
  if (pgsz < sizeof (double) || pgsz > L * sizeof (double))
    return 0;
  p = mmap ((void *) 0, L * n * sizeof (double), PROT_NONE,
	    MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (p == MAP_FAILED)
    return 0;
  if (mprotect (p, pgsz, PROT_READ | PROT_WRITE))
    return 0;
  l = (L * sizeof (double)) / pgsz * pgsz;
  if (mprotect (p + l, pgsz, PROT_READ | PROT_WRITE))
    return 0;
  l = (2 * L * sizeof (double)) / pgsz * pgsz;
  if (mprotect (p + l, pgsz, PROT_READ | PROT_WRITE))
    return 0;
  l = (3 * L * sizeof (double)) / pgsz * pgsz;
  if (mprotect (p + l, pgsz, PROT_READ | PROT_WRITE))
    return 0;
  for (m = 0; m < N; m += n)
    test_func ((double *) p, L, m, n, N);
  return 0;
}
