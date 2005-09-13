/* Assuming we can vectorize char multiplication, here's an execute test.  */

#include <stdarg.h>
#include "tree-vect.h"

extern void abort (void);
void foo()
{
  static unsigned char A[256], B[256], C[256];
  int i;

  for (i = 0; i < 256; ++i)
    A[i] = B[i] = i;

  for (i = 0; i < 256; ++i)
    C[i] = A[i] * B[i];

  for (i = 0; i < 256; ++i)
    if (C[i] != (unsigned char)(i * i))
      abort ();
}

int main()
{
  check_vect ();
  foo();
  return 0;
}

/* { dg-final { cleanup-tree-dump "vect" } } */
