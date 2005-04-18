/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_int_mult } */

#include <stdarg.h>
#include "tree-vect.h"

extern void abort (void);
void u ()
{  
  unsigned int A[4] = {0x08000000,0xffffffff,0xff0000ff,0xf0000001};
  unsigned int B[4] = {0x08000000,0x08000001,0xff0000ff,0xf0000001};
  unsigned int Answer[4] = {0,0xf7ffffff,0x0200fe01,0xe0000001};
  unsigned int C[4];
  int i, j;
  
  for (i=0; i<4; i++)
    C[i] = A[i] * B[i];
  for (i=0; i<4; i++)
    if (C[i] != Answer[i])
      abort ();
}
void s()
{
  signed int A[4] = {0x08000000,0xffffffff,0xff0000ff,0xf0000001};
  signed int B[4] = {0x08000000,0x08000001,0xff0000ff,0xf0000001};
  signed int Answer[4] = {0,0xf7ffffff,0x0200fe01, 0xe0000001};
  signed int C[4];
  int i, j;
  
  for (i=0; i<4; i++)
    C[i] = A[i] * B[i];
  for (i=0; i<4; i++)
    if (C[i] != Answer[i])
      abort ();
}

int main1 ()
{
  u();
  s();
  return 0;
}

int main (void)
{ 
  check_vect ();
  
  return main1 ();
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 2 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
