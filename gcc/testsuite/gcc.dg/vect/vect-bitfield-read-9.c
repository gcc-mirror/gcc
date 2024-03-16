/* { dg-require-effective-target vect_shift } */
/* { dg-require-effective-target vect_long_long } */
/* { dg-additional-options { "-fdump-tree-ifcvt-all" } } */

#include <stdarg.h>
#include "tree-vect.h"

extern void abort(void);

struct s {
    unsigned i : 31;
    char a : 4;
};

#define N 32
#define ELT0 {0x7FFFFFFFUL, 0}
#define ELT1 {0x7FFFFFFFUL, 1}
#define ELT2 {0x7FFFFFFFUL, 2}
#define ELT3 {0x7FFFFFFFUL, 3}
#define ELT4 {0x7FFFFFFFUL, 4}
#define RES 9
struct s A[N]
  = { ELT0, ELT4, ELT2, ELT3, ELT0, ELT1, ELT2, ELT3,
      ELT0, ELT1, ELT2, ELT3, ELT0, ELT1, ELT2, ELT3,
      ELT0, ELT1, ELT2, ELT3, ELT0, ELT1, ELT2, ELT3,
      ELT0, ELT1, ELT2, ELT3, ELT0, ELT1, ELT2, ELT3};

int __attribute__ ((noipa))
f(struct s *ptr, unsigned n) {
    int res = 0;
    for (int i = 0; i < n; ++i)
      {
	if (ptr[i].a)
	  return 9;
	res += ptr[i].a;
      }
    return res;
}

int main (void)
{
  check_vect ();

  if (f(&A[0], N) != RES)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump "Bitfield OK to lower." "ifcvt" } } */

