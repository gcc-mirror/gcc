/* { dg-do compile } */
#include <limits.h>

#if defined(__LP64__) || defined(_WIN64)
#define DIM (UINT_MAX>>1)+1
#else
#define DIM 65536
#endif

int
sub (int *a)
{
  return a[0];
}

int
main (void)
{
  int a[DIM][DIM];  /* { dg-error "size of array 'a' is too large" } */
  return sub (&a[0][0]);
}
