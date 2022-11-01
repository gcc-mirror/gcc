/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_shift } */

#include <stdarg.h>
#include "tree-vect.h"

extern void abort(void);

struct s {
    unsigned a : 23; unsigned b : 8;
};

#define N 32
#define ELT0 {0x7FFFFFUL, 0}
#define ELT1 {0x7FFFFFUL, 1}
#define ELT2 {0x7FFFFFUL, 2}
#define ELT3 {0x7FFFFFUL, 3}
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
      res += ptr[i].b;
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
