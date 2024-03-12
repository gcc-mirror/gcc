/* { dg-require-effective-target vect_int } */

#include "tree-vect.h"

extern void abort (void);

__attribute__((noinline, noclone))
void test1(short x[128])
{
    int i;
    for (i=127; i>=0; i--) {
	x[i] = 1234;
    }
}

int main (void)
{
  short x[128 + 32];
  int i;
  
  check_vect ();

  for (i = 0; i < 16; i ++)
    {
      asm ("");
      x[i] = x[i + 144] = 5678;
    }
   
  test1 (x + 16);
  
#pragma GCC novector
  for (i = 0; i < 128; i++)
   if (x[i + 16] != 1234)
     abort ();
  
#pragma GCC novector
  for (i = 0; i < 16; i++)
    if (x[i] != 5678
       || x[i + 144] != 5678)
      abort ();
         
  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
