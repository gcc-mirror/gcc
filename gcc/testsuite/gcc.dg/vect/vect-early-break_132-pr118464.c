/* { dg-do compile } */
/* { dg-add-options vect_early_break } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-O3" } */

int a, b, c, d, e, f;
short g[1];
int main() {
  int h;
  while (a) {
    while (h)
      ;
    for (b = 2; b; b--) {
      while (c)
        ;
      f = g[a];
      if (d)
        break;
    }
    while (e)
      ;
  }
  return 0;
}
