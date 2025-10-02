/* { dg-do compile } */

typedef unsigned long a;
int b[] = {};
int **c;
short d(a *e, int f)
{
  *c = &f;
  for (;;)
    asm goto("" : : : : g);
  for (; f; f--) {
    asm goto("" : : : : g);
  g:
    *e ^= b[f + 1];
  }
}
