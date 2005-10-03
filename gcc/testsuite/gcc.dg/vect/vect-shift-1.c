/* { dg-require-effective-target vect_shift } */

#include <stdarg.h>
#include <signal.h>

extern void abort(void); 

int main ()
{  
  unsigned int A[4] =  {0x08000000,0x08000001,0xff0000ff,0xf0000001};
  unsigned int Answer[4] = {0x01000000,0x01000000,0x01fe0001f,0x1e000000};
  unsigned int B[4];
  int i, j;
  
  for (i=0; i<4; i++)
    B[i] = A[i] >> 3;
  for (i=0; i<4; i++)
    if (B[i] != Answer[i])
      abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
