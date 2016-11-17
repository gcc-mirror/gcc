/* { dg-do run } */
/* { dg-options "-O2 -fcilkplus" } */

#define _FORTIFY_SOURCE 2
#include <string.h>

int sum(int low, int high)
{
  if(low == high) {
    return low;
  }

  int mid = low + (high-low)/2;
  int a = _Cilk_spawn sum(low, mid);
  int b = sum(mid+1, high);

  // Some very expensive computation here
  int foo[64];
  memset(foo, 0, 64*sizeof(int)); // <--- Fails

  _Cilk_sync;

  return a+b;
}

int main(void) {
  if (sum(0, 100) != 5050)
    __builtin_abort ();
  return 0;
}
