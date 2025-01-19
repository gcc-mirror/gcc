/* { dg-add-options vect_early_break } */
/* { dg-do compile } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-Ofast -fno-vect-cost-model -fdump-tree-vect-details" } */

enum a { b };

struct {
  enum a c;
} d[10], *e;

void f() {
  int g;
  for (g = 0, e = d; g < sizeof(1); g++, e++)
    if (e->c)
      return;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
