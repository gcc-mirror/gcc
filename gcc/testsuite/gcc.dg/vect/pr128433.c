/* { dg-require-effective-target mmap } */

#include "tree-vect.h"

#include <stdlib.h>
#include <sys/mman.h>
#include <unistd.h>

__attribute__ ((noipa))
static void
copy_float (const float *s, float *d, long n)
{
  for (long i = 0; i < n; ++i)
    {
      d[2 * i] = s[i];
      d[2 * i + 1] = s[i];
    }
}

int
main ()
{
  check_vect ();

  long page = sysconf (_SC_PAGESIZE);
  char *memory = mmap (0, 2 * page, PROT_READ | PROT_WRITE,
                       MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (memory == MAP_FAILED
      || mprotect (memory + page, page, PROT_NONE))
    return 0;

  volatile long count = 1;
  float *s = (float *) (memory + page - count * sizeof (*s));
  float d[2] = { 0, 0 };
  s[0] = 1;
  copy_float (s, d, count);
  if (d[0] != 1 || d[1] != 1)
    abort ();
  return 0;
}
