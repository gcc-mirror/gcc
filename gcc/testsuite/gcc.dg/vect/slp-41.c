/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_pack_trunc } */
/* { dg-require-effective-target vect_unpack } */
/* { dg-require-effective-target vect_hw_misalign } */

#include "tree-vect.h"

void __attribute__((noinline,noclone))
testi (int *p, short *q, int stride, int n)
{
  int i;
  for (i = 0; i < n; ++i)
    {
      q[i*4+0] = p[i*stride+0];
      q[i*4+1] = p[i*stride+1];
      q[i*4+2] = p[i*stride+2];
      q[i*4+3] = p[i*stride+3];
    }
}

void __attribute__((noinline,noclone))
testi2 (int *q, short *p, int stride, int n)
{
  int i;
  for (i = 0; i < n; ++i)
    {
      q[i*4+0] = p[i*stride+0];
      q[i*4+1] = p[i*stride+1];
      q[i*4+2] = p[i*stride+2];
      q[i*4+3] = p[i*stride+3];
    }
}

int ia[256];
short sa[256];

extern void abort (void);

int main()
{
  int i;

  check_vect ();

  for (i = 0; i < 256; ++i)
    {
      ia[i] = sa[i] = i;
       __asm__ volatile ("");
    }
  testi (ia, sa, 8, 32);
  for (i = 0; i < 128; ++i)
    if (sa[i] != ia[(i / 4) * 8 + i % 4])
      abort ();

  for (i = 0; i < 256; ++i)
    {
      ia[i] = sa[i] = i;
       __asm__ volatile ("");
    }
  testi2 (ia, sa, 8, 32);
  for (i = 0; i < 128; ++i)
    if (ia[i] != sa[(i / 4) * 8 + i % 4])
      abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 2 "vect" } } */
