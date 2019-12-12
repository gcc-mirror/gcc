/* { dg-require-effective-target vect_int } */

#include "tree-vect.h"

#define DIV(x,y) ((x)/(y))
#define MOD(x,y) ((x)%(y))

#define TEMPLATE(PO2,OP)						\
void __attribute__ ((noipa))						\
f_##PO2##_##OP (int *restrict a, int *restrict b, __INTPTR_TYPE__ n)	\
{									\
  for (__INTPTR_TYPE__ i = 0; i < n; ++i)				\
    a[i] = OP (b[i], (1 << PO2));					\
}
#define TEMPLATES(PO2)	\
TEMPLATE (PO2,DIV);	\
TEMPLATE (PO2,MOD);

TEMPLATES (1);
TEMPLATES (2);
TEMPLATES (3);
TEMPLATES (7);
TEMPLATES (8);
TEMPLATES (10);
TEMPLATES (15);
TEMPLATES (16);
TEMPLATES (20);

typedef void (*func_t) (int *, int *, __INTPTR_TYPE__);
typedef struct {
  int po2;
  func_t div;
  func_t mod;
} fn_t;
const fn_t fns[] = {
#define FN_PAIR(PO2) { PO2, f_##PO2##_DIV, f_##PO2##_MOD }
  FN_PAIR (1),
  FN_PAIR (2),
  FN_PAIR (3),
  FN_PAIR (7),
  FN_PAIR (8),
  FN_PAIR (10),
  FN_PAIR (15),
  FN_PAIR (16),
  FN_PAIR (20),
};

int __attribute__ ((noipa, noinline))
power2 (int x)
{
  return 1 << x;
}

#define N 50

int
main (void)
{
  int a[N], b[N], c[N];

  for (int i = 0; i < (sizeof(fns)/sizeof(fns[0])); i++)
    {
      int p = power2 (fns[i].po2);
      for (int j = 0; j < N; j++)
        a[j] = ((p << 4) * j) / (N - 1) - (p << 5);

      fns[i].div (b, a, N);
      fns[i].mod (c, a, N);

      for (int j = 0; j < N; j++)
	if (a[j] != (b[j] * p + c[j]))
          __builtin_abort ();
    }

  return 0;
}

/* { dg-final { scan-tree-dump {\.DIV_POW2} "vect" { target vect_sdiv_pow2_si } } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loop" 18 "vect" { target vect_sdiv_pow2_si } } } */
