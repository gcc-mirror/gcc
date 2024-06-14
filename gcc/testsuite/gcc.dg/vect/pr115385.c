/* { dg-require-effective-target mmap } */
/* { dg-additional-options "-mavx2" { target avx2_runtime } } */

#include <sys/mman.h>
#include <stdio.h>

#define COUNT 511
#define MMAP_SIZE 0x20000
#define ADDRESS 0x1122000000
#define TYPE unsigned char

#ifndef MAP_ANONYMOUS
#define MAP_ANONYMOUS MAP_ANON
#endif

void __attribute__((noipa)) foo(TYPE * __restrict x,
                                TYPE *y, int n)
{
  for (int i = 0; i < n; ++i)
    {
      x[16*i+0] = y[3*i+0];
      x[16*i+1] = y[3*i+1];
      x[16*i+2] = y[3*i+2];
      x[16*i+3] = y[3*i+0];
      x[16*i+4] = y[3*i+1];
      x[16*i+5] = y[3*i+2];
      x[16*i+6] = y[3*i+0];
      x[16*i+7] = y[3*i+1];
      x[16*i+8] = y[3*i+2];
      x[16*i+9] = y[3*i+0];
      x[16*i+10] = y[3*i+1];
      x[16*i+11] = y[3*i+2];
      x[16*i+12] = y[3*i+0];
      x[16*i+13] = y[3*i+1];
      x[16*i+14] = y[3*i+2];
      x[16*i+15] = y[3*i+0];
    }
}

void __attribute__((noipa)) bar(TYPE * __restrict x,
                                TYPE *y, int n)
{
  for (int i = 0; i < n; ++i)
    {
      x[16*i+0] = y[5*i+0];
      x[16*i+1] = y[5*i+1];
      x[16*i+2] = y[5*i+2];
      x[16*i+3] = y[5*i+3];
      x[16*i+4] = y[5*i+4];
      x[16*i+5] = y[5*i+0];
      x[16*i+6] = y[5*i+1];
      x[16*i+7] = y[5*i+2];
      x[16*i+8] = y[5*i+3];
      x[16*i+9] = y[5*i+4];
      x[16*i+10] = y[5*i+0];
      x[16*i+11] = y[5*i+1];
      x[16*i+12] = y[5*i+2];
      x[16*i+13] = y[5*i+3];
      x[16*i+14] = y[5*i+4];
      x[16*i+15] = y[5*i+0];
    }
}

TYPE x[COUNT * 16];

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

  foo (x, end_y - COUNT * 3, COUNT);
  bar (x, end_y - COUNT * 5, COUNT);

  return 0;
}

/* We always require a scalar epilogue here but we don't know which
   targets support vector composition this way.  */
