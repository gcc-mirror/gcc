/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_shift } */

#include <stdarg.h>
#include "tree-vect.h"
#include <stdbool.h>

extern void abort(void);

typedef struct {
    int  c;
    int  b;
    bool a : 1;
    int  d : 31;
} struct_t;

#define N 16
#define ELT_F { 0xFFFFFFFF, 0xFFFFFFFF, 0, 0x7FFFFFFF }
#define ELT_T { 0xFFFFFFFF, 0xFFFFFFFF, 1, 0x7FFFFFFF }

struct_t vect_false[N] = { ELT_F, ELT_F, ELT_F, ELT_F, ELT_F, ELT_F, ELT_F, ELT_F,
			   ELT_F, ELT_F, ELT_F, ELT_F, ELT_F, ELT_F, ELT_F, ELT_F  };
struct_t vect_true[N]  = { ELT_F, ELT_F, ELT_T, ELT_F, ELT_F, ELT_F, ELT_F, ELT_F,
			   ELT_F, ELT_F, ELT_T, ELT_F, ELT_F, ELT_F, ELT_F, ELT_F  };
int main (void)
{
  unsigned ret = 0;
  for (unsigned i = 0; i < N; i++)
  {
      ret |= vect_false[i].a;
  }
  if (ret)
    abort ();

  for (unsigned i = 0; i < N; i++)
  {
      ret |= vect_true[i].a;
  }
  if (!ret)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 2 loops" 1 "vect" } } */
