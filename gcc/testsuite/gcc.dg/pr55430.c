/* PR middle-end/55430 */
/* { dg-do run { target mmap } } */
/* { dg-options "-O2" } */

#include <stddef.h>
#include <stdio.h>
#include <sys/mman.h>
#ifndef MAP_ANONYMOUS
#define MAP_ANONYMOUS MAP_ANON
#endif
#ifndef MAP_ANON
#define MAP_ANON 0
#endif
#ifndef MAP_FAILED
#define MAP_FAILED ((void *)-1)
#endif
#include <stdlib.h>

struct S
{
  unsigned int s1 : 8;
  unsigned int s2 : 2;
};

__attribute__((noinline, noclone)) int
foo (int x, int y, struct S *z, unsigned int w)
{
  if (z[y].s2 == x && z[y].s1 == w)
    return 1;
  return 0;
}

int
main ()
{
  char *p = mmap (NULL, 131072, PROT_READ | PROT_WRITE,
		  MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (p == MAP_FAILED)
    return 0;
  if (munmap (p + 65536, 65536) < 0)
    return 0;
  if ((65536 / sizeof (struct S)) * sizeof (struct S) != 65536)
    return 0;
  struct S *s = (struct S *) (p + 65536);
  return foo (0, 0, s - 1, 0) != 1;
}
