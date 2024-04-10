/* { dg-add-options vect_early_break } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target mmap } */

/* { dg-final { scan-tree-dump-not "LOOP VECTORIZED" "vect" } } */

#include <sys/mman.h>
#include <unistd.h>

#include "tree-vect.h"

__attribute__((noipa))
int foo (const char *s, unsigned long n)
{
 unsigned long len = 0;
 while (*s++ && n--)
   ++len;
 return len;
}

int main()
{

  check_vect ();

  long pgsz = sysconf (_SC_PAGESIZE);
  void *p = mmap (NULL, pgsz * 3, PROT_READ|PROT_WRITE,
     MAP_ANONYMOUS|MAP_PRIVATE, 0, 0);
  if (p == MAP_FAILED)
    return 0;
  mprotect (p, pgsz, PROT_NONE);
  mprotect (p+2*pgsz, pgsz, PROT_NONE);
  char *p1 = p + pgsz;
  p1[0] = 1;
  p1[1] = 0;
  foo (p1, 1000);
  p1 = p + 2*pgsz - 2;
  p1[0] = 1;
  p1[1] = 0;
  foo (p1, 1000);
  return 0;
}

