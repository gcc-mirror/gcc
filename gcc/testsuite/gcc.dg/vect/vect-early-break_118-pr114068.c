/* { dg-do compile } */
/* { dg-add-options vect_early_break } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-O3" } */

/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" } } */

struct h {
  int b;
  int f;
} k;

void n(int m) {
  struct h a = k;
  for (int o = m; o; ++o) {
    if (a.f)
      __builtin_unreachable();
    if (o > 1)
      __builtin_unreachable();
    *(&k.b + o) = 1;
  }
}
