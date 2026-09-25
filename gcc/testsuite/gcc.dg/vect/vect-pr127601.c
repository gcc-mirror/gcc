/* { dg-require-effective-target mmap } */

#include <sys/mman.h>
#include "tree-vect.h"

#define PG 4096

static long __attribute__((noipa))
rev (long *a, int n)
{
  long s = 0;
  for (int i = n - 1; i >= 0; i--)
    s += a[4 * i] + a[4 * i + 1] + a[4 * i + 2];
  return s;
}

int main (void)
{
  check_vect ();

  unsigned char *p = mmap (0, 2 * PG, PROT_READ | PROT_WRITE,
                           MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (p == MAP_FAILED || mprotect (p + PG, PG, PROT_NONE))
    return 0;

  int n = 9;
  unsigned long ne = 4UL * (n - 1) + 3;         /* 35 longs are touched */
  long *a = (long *) (p + PG - ne * sizeof (long));

#pragma GCC novector
  for (unsigned long i = 0; i < ne; i++)
    a[i] = (long) (i * 3 + 1);

  if (rev (a, n) != 1404)
    __builtin_abort ();
  return 0;
}
