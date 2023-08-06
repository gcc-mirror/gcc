/* { dg-additional-options "-fdump-tree-optimized-details-blocks" } */
/* { dg-require-effective-target vect_shift } */
/* { dg-require-effective-target vect_long_long } */

#include <stdarg.h>
#include "tree-vect.h"

extern void abort(void);

struct s {
    unsigned i : 31;
    char x : 2;
    char a : 4;
};

#define N 32
#define ELT0 {0x7FFFFFFFUL, 3, 0}
#define ELT1 {0x7FFFFFFFUL, 3, 1}
#define ELT2 {0x7FFFFFFFUL, 3, 2}
#define ELT3 {0x7FFFFFFFUL, 3, 3}
#define RES 48
struct s A[N]
  = { ELT0, ELT1, ELT2, ELT3, ELT0, ELT1, ELT2, ELT3,
      ELT0, ELT1, ELT2, ELT3, ELT0, ELT1, ELT2, ELT3,
      ELT0, ELT1, ELT2, ELT3, ELT0, ELT1, ELT2, ELT3,
      ELT0, ELT1, ELT2, ELT3, ELT0, ELT1, ELT2, ELT3};

int __attribute__ ((noipa))
f(struct s *ptr, unsigned n) {
    int res = 0;
    for (int i = 0; i < n; ++i)
      res += ptr[i].a;
    return res;
}

int main (void)
{
  check_vect ();

  if (f(&A[0], N) != RES)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */

/* { dg-final { scan-tree-dump-not "Invalid sum" "optimized" } } */
