/* { dg-do run } */
/* { dg-additional-options "-O3" } */

#include "tree-vect.h"

void abort (void);

unsigned int a[256];
unsigned char b[256];

int main()
{
  int i, z, x, y;

  check_vect ();

  for(i = 0; i < 256; i++)
    {
      a[i] = i % 5;
      __asm__ volatile ("");
    }

  for (z = 0; z < 16; z++)
    for (y = 0; y < 4; y++)
      for (x = 0; x < 4; x++)
	b[y*64 + z*4 + x] = a[z*16 + y*4 + x];

  if (b[4] != 1)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump "vectorized 1 loop" "vect" { target { vect_pack_trunc } } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
