/* PR tree-optimization/83047 */
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

__attribute__((noipa)) void
foo (char *p, char *q, int r)
{
  char a = q[0];
  if (r || a == '\0')
    return;
  char b = q[1];
  p[0] = a;
  p[1] = b;
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
  p[0] = 'c';
  p[1] = 'd';
  p[65536 - 2] = 'a';
  p[65536 - 1] = 'b';
  volatile int r = 1;
  foo (p, p + 65536 - 2, r);
  if (p[0] != 'c' || p[1] != 'd')
    abort ();
  r = 0;
  foo (p, p + 65536 - 2, r);
  if (p[0] != 'a' || p[1] != 'b')
    abort ();
  p[0] = 'e';
  p[1] = 'f';
  r = 1;
  foo (p, p + 65536 - 1, r);
  if (p[0] != 'e' || p[1] != 'f')
    abort ();
  return 0;
}
