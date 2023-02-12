/* { dg-require-effective-target vect_int } */
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
#define V 5
struct s A[N];

void __attribute__ ((noipa))
f(struct s *ptr, unsigned n) {
    for (int i = 0; i < n; ++i)
      ptr[i].a = V;
}

void __attribute__ ((noipa))
check_f(struct s *ptr) {
    for (unsigned i = 0; i < N; ++i)
      if (ptr[i].a != V)
	abort ();
}

int main (void)
{
  check_vect ();
  __builtin_memset (&A[0], 0, sizeof(struct s) * N);

  f(&A[0], N);
  check_f (&A[0]);

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */

