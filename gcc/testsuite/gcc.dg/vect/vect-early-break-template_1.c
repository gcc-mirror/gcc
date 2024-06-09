#include "tree-vect.h"

#ifndef N
#define N 803
#endif

#ifndef P
#define P 0
#endif

unsigned vect_a[N] = {0};
unsigned vect_b[N] = {0};
  
__attribute__((noipa, noinline))
unsigned test4(unsigned x)
{
 unsigned ret = 0;
 for (int i = 0; i < N; i++)
 {
   vect_b[i] = x + i;
   if (vect_a[i] > x)
     break;
   vect_a[i] = x;
   
 }
 return ret;
}

extern void abort ();

int main ()
{
  check_vect ();

  int x = 1;
  int idx = P;
  vect_a[idx] = x + 1;

  test4(x);

  if (vect_b[idx] != (x + idx))
    abort ();

  if (vect_a[idx] != x + 1)
    abort ();

  if (idx > 0 && vect_a[idx-1] != x)
    abort ();

}
