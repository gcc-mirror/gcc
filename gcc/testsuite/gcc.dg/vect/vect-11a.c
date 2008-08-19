/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_int_mult } */

#include <stdarg.h>
#include "tree-vect.h"

extern void abort (void);

unsigned int A[8] = {0x08000000,0xffffffff,0xff0000ff,0xf0000001,
		     0x08000000,0xffffffff,0xff0000ff,0xf0000001};
unsigned int B[8] = {0x08000000,0x08000001,0xff0000ff,0xf0000001,
		     0x08000000,0x08000001,0xff0000ff,0xf0000001};
unsigned int Answer[8] = {0,0xf7ffffff,0x0200fe01,0xe0000001,
			  0,0xf7ffffff,0x0200fe01,0xe0000001};
unsigned int C[8];

__attribute__ ((noinline))
void u ()
{  
  int i, j;
  
  for (i=0; i<8; i++)
    C[i] = A[i] * B[i];
  for (i=0; i<8; i++)
    if (C[i] != Answer[i])
      abort ();
}

signed int D[8] = {0x08000000,0xffffffff,0xff0000ff,0xf0000001,
		   0x08000000,0xffffffff,0xff0000ff,0xf0000001};
signed int E[8] = {0x08000000,0x08000001,0xff0000ff,0xf0000001,
		   0x08000000,0x08000001,0xff0000ff,0xf0000001};
signed int Dnswer[8] = {0,0xf7ffffff,0x0200fe01, 0xe0000001,
			0,0xf7ffffff,0x0200fe01, 0xe0000001};
signed int F[8];

__attribute__ ((noinline))
void s()
{
  int i, j;
  
  for (i=0; i<8; i++)
    F[i] = D[i] * E[i];
  for (i=0; i<8; i++)
    if (F[i] != Dnswer[i])
      abort ();
}

__attribute__ ((noinline))
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
