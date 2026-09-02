/* { dg-do run { target *-*-linux* *-*-gnu* *-*-uclinux* } } */
/* { dg-additional-options "-fno-tree-vectorize -fno-tree-loop-distribute-patterns" } */

#include <sys/mman.h>
#include <unistd.h>

__attribute__((noipa)) void
f (char *p, unsigned int i, unsigned int n)
{
  p += i;
  do
    {
      *p = 1;
      p += 1;
      i++;
    }
  while (i < n);
}

int
main ()
{
  long pgsz = sysconf (_SC_PAGESIZE);
  char *p = mmap (NULL, pgsz * 2, PROT_READ | PROT_WRITE,
		  MAP_ANONYMOUS | MAP_PRIVATE, 0, 0);
  if (p == MAP_FAILED)
    return 0;
  mprotect (p + pgsz, pgsz, PROT_NONE);
  /* I + 1 is not less than N, so the loop stores to P[4] only.  IVOPTs
     used to compute the bound of the transformed loop from the number of
     iterations N - I - 1, which is (unsigned int) -3 here and gets zero
     extended, so that the loop ran into the protected page.  */
  f (p + 2, 2, 0);
  if (p[4] != 1 || p[5] != 0)
    __builtin_abort ();
  return 0;
}
