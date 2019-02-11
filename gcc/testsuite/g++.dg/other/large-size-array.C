/* { dg-do compile } */
#include <limits.h>

#ifdef _WIN64
#define DIM ULLONG_MAX>>1
#else
#ifdef __LP64__
#define DIM UINT_MAX>>1
#else
#define DIM USHRT_MAX>>1
#endif
#endif

int
sub (int *a)
{
  return a[0];
}

int
main (void)
{
  int a[DIM][DIM];  /* { dg-error "exceeds maximum object size" } */
  return sub (&a[0][0]);  /* { dg-error "declared" } */
}


