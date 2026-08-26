/* { dg-do compile } */
/* { dg-options "-O3 -march=x86-64" } */

extern void bar (const char *, int); 
extern char a, b, d, e, k;
extern int c, f, g, h, i, j;
void
foo (void)
{
  while (a) {
    int *n = &h, *o = &g;
    bar ("%d", f);
    for (; c; c++) {
      int *p = &j;
      for (b = 0; b < 8; b++) {
        e = a > 0 ? a : a << 1;
        k = -e;
        *n &= (k != *p);
      }
      for (; d; d++)
        i || (*o = *p = 2);
    }
  }
}
