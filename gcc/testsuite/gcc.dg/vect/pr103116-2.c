/* { dg-require-effective-target mmap } */
/* { dg-additional-options "-mssse3" { target x86_64-*-* i?86-*-* } } */

#include <sys/mman.h>
#include <stdio.h>
#include "tree-vect.h"

#define COUNT 128
#define MMAP_SIZE 0x20000
#define ADDRESS 0x1122000000
#define TYPE unsigned short
#define GROUP_SIZE 2

#ifndef MAP_ANONYMOUS
#define MAP_ANONYMOUS MAP_ANON
#endif

void __attribute__((noipa))
loop (TYPE *restrict x, TYPE *restrict y)
{
  for (int i = 0; i < COUNT; ++i)
    {
      x[i * 8] = y[i * GROUP_SIZE] + 1;
      x[i * 8 + 1] = y[i * GROUP_SIZE] + 2;
      x[i * 8 + 2] = y[i * GROUP_SIZE + 1] + 3;
      x[i * 8 + 3] = y[i * GROUP_SIZE + 1] + 4;
      x[i * 8 + 4] = y[i * GROUP_SIZE] + 5;
      x[i * 8 + 5] = y[i * GROUP_SIZE] + 6;
      x[i * 8 + 6] = y[i * GROUP_SIZE + 1] + 7;
      x[i * 8 + 7] = y[i * GROUP_SIZE + 1] + 8;
    }
}

TYPE x[COUNT * 8];

int
main (void)
{
  void *y;
  TYPE *end_y;

  check_vect ();

  y = mmap ((void *) ADDRESS, MMAP_SIZE, PROT_READ | PROT_WRITE,
            MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (y == MAP_FAILED)
    {
      perror ("mmap");
      return 1;
    }

  end_y = (TYPE *) ((char *) y + MMAP_SIZE);

  loop (x, end_y - COUNT * GROUP_SIZE);

  return 0;
}

/* { dg-final { scan-tree-dump "peeling for gaps insufficient for access" "vect" { target { vect_perm_short } } } } */
