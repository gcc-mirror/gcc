/* { dg-require-effective-target mmap } */

#include <sys/mman.h>
#include <stdio.h>
#include "tree-vect.h"

#define COUNT 320
#define MMAP_SIZE 0x10000
#define ADDRESS 0x1122000000
#define TYPE unsigned short

#ifndef MAP_ANONYMOUS
#define MAP_ANONYMOUS MAP_ANON
#endif

void __attribute__((noinline))
foo (TYPE *__restrict a, TYPE *__restrict b)
{
  int n;

  for (n = 0; n < COUNT; n++)
    a[n] = b[n * 2];
}

int
main (void)
{
  void *x;
  size_t b_offset;

  check_vect ();

  x = mmap ((void *) ADDRESS, MMAP_SIZE, PROT_READ | PROT_WRITE,
	    MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (x == MAP_FAILED)
    {
      perror ("mmap");
      return 1;
    }

  b_offset = MMAP_SIZE - (2 * COUNT - 1) * sizeof (TYPE);
  foo ((unsigned short *) x,
       (unsigned short *) ((char *) x + b_offset));
  return 0;
}

