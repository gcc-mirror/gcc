#include "tree-vect.h"

#define N 64

__attribute__ ((noinline)) int
f (int *restrict a, int *restrict b, int *restrict c)
{
  for (int i = 0; i < N; ++i)
    c[i] = a[i] % b[i];
}

#define BASE1 -126
#define BASE2 116

int
main (void)
{
  check_vect ();

  int a[N], b[N], c[N];

  for (int i = 0; i < N; ++i)
    {
      a[i] = BASE1 + i * 5;
      b[i] = BASE2 - i * 4;
      /* b[i] cannot be 0 as that would cause undefined
	 behavior with respect to `% b[i]`. */
      b[i] = b[i] ? b[i] : 1;
      __asm__ volatile ("");
    }

  f (a, b, c);

#pragma GCC novector
  for (int i = 0; i < N; ++i)
    if (c[i] != a[i] % b[i])
      __builtin_abort ();
}

/* { dg-final { scan-tree-dump "vect_recog_mod_var_pattern: detected" "vect" { target vect_int_div } } } */
