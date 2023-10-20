/* { dg-require-effective-target vect_shift } */
/* { dg-require-effective-target vect_long_long } */
/* { dg-additional-options { "-fdump-tree-ifcvt-all" } } */

#include <stdarg.h>
#include "tree-vect.h"

extern void abort(void);

struct s {
    char a : 4;
};

#define N 32
#define ELT0 {0}
#define ELT1 {1}
#define ELT2 {2}
#define ELT3 {3}
#define RES 56
struct s A[N]
  = { ELT0, ELT1, ELT2, ELT3, ELT0, ELT1, ELT2, ELT3,
      ELT0, ELT1, ELT2, ELT3, ELT0, ELT1, ELT2, ELT3,
      ELT0, ELT1, ELT2, ELT3, ELT0, ELT1, ELT2, ELT3,
      ELT0, ELT1, ELT2, ELT3, ELT0, ELT1, ELT2, ELT3};

int __attribute__ ((noipa))
f(struct s *ptr, unsigned n) {
    int res = 0;
    for (int i = 0; i < n; ++i)
      {
	switch (ptr[i].a)
	  {
	  case 0:
	    res += ptr[i].a + 1;
	    break;
	  case 1:
	  case 2:
	  case 3:
	    res += ptr[i].a;
	    break;
	  default:
	    return 0;
	  }
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

/* { dg-final { scan-tree-dump-not "Bitfield OK to lower." "ifcvt" } } */


