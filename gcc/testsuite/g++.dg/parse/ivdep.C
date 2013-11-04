/* { dg-do compile } */

/* PR other/33426 */

void foo(int n, int *a, int *b, int *c, int *d, int *e) {
  int i, j;
#pragma GCC ivdep
  for (i = 0; ; ++i) { /* { dg-error "missing loop condition in loop with 'GCC ivdep' pragma before ';' token" } */
    a[i] = b[i] + c[i];
  }
}
