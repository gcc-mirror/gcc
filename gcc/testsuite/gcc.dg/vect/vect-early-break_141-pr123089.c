/* { dg-add-options vect_early_break } */
/* { dg-require-effective-target vect_early_break_hw } */
/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target avx2_runtime { target { i?86-*-* x86_64-*-* } } } */

/* { dg-additional-options "-O3 -fno-strict-aliasing -march=znver3" { target { i?86-*-* x86_64-*-* } } } */
/* { dg-final { scan-tree-dump "loop vectorized" "vect" { target { i?86-*-* x86_64-*-* } } } } */

#include "tree-vect.h"

struct
{
  int d;
  short e;
} i;

int b;
int *h = &b;

int
main ()
{
  check_vect ();

  short f = 1;
  short *g = &i.e;

a:
  if (*g = 0 & ++f, *h)
    ;
  else
    {
      int c = 0;
      if (f)
        goto a;
      h = &c;
    }

  return 0;
}
