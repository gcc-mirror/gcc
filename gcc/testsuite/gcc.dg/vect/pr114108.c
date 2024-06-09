/* { dg-do compile } */

#include "tree-vect.h"

typedef signed char schar;

__attribute__((noipa, noinline, optimize("O3")))
void foo(const schar *a, const schar *b, schar *c, int n)
{
  for (int i = 0; i < n; i++)
    {   
      unsigned u = __builtin_abs (a[i] - b[i]);
      c[i] = u <= 7U ? u : 7U; 
    }   
}


/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" { target aarch64*-*-* } } } */
/* { dg-final { scan-tree-dump "vect_recog_abd_pattern: detected" "vect" { target aarch64*-*-* } } } */
