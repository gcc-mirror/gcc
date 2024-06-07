/* { dg-require-effective-target mmap } */

#include <sys/mman.h>
#include <stdio.h>

#define COUNT 128
#define MMAP_SIZE 0x20000
#define ADDRESS 0x1122000000
#define TYPE unsigned int

#ifndef MAP_ANONYMOUS
#define MAP_ANONYMOUS MAP_ANON
#endif

void __attribute__((noipa))
loop (TYPE *restrict x, TYPE *restrict y)
{
  for (int i = 0; i < COUNT; ++i)
    {
      x[i * 4] = y[i * 2] + 1;
      x[i * 4 + 1] = y[i * 2] + 2;
      x[i * 4 + 2] = y[i * 2 + 1] + 3;
      x[i * 4 + 3] = y[i * 2 + 1] + 4;
    }
}

TYPE x[COUNT * 4];

int
main (void)
{
  void *y;
  TYPE *end_y;

  y = mmap ((void *) ADDRESS, MMAP_SIZE, PROT_READ | PROT_WRITE,
            MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (y == MAP_FAILED)
    {
      perror ("mmap");
      return 1;
    }

  end_y = (TYPE *) ((char *) y + MMAP_SIZE);

  loop (x, end_y - COUNT * 2);

  return 0;
}

/* When the target can compose a vector from its half we do not require
   a scalar epilogue, but there's no effective target for this.  */
/* { dg-final { scan-tree-dump "vectorized 1 loops" "vect" { target { vect_perm && vect_int } } } } */
