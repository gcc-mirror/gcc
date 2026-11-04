/* PR128433 */
/* { dg-do run { target { avx512f && mmap } } } */
/* { dg-options "-O3 -mavx512f -mprefer-vector-width=512 --param vect-partial-vector-usage=2" } */

#include "avx512f-check.h"

#include <stdlib.h>
#include <sys/mman.h>
#include <unistd.h>
#ifndef MAP_ANONYMOUS
#define MAP_ANONYMOUS MAP_ANON
#endif
#ifndef MAP_ANON
#define MAP_ANON 0
#endif
#ifndef MAP_FAILED
#define MAP_FAILED ((void *)-1)
#endif

/* The permuted SLP load of s[i] { 0, 0 } accesses only half of a
   vector per vector iteration, we may not use the store loop mask
   (two scalars per iteration) for it.  */

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

static void
avx512f_test (void)
{
  long page = sysconf (_SC_PAGESIZE);
  char *memory = mmap (0, 2 * page, PROT_READ | PROT_WRITE,
		       MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (memory == MAP_FAILED
      || mprotect (memory + page, page, PROT_NONE))
    return;

  volatile long count = 1;
  float *s = (float *) (memory + page - count * sizeof (*s));
  float d[2] = { 0, 0 };
  s[0] = 1;
  copy_float (s, d, count);
  if (d[0] != 1 || d[1] != 1)
    abort ();
}
