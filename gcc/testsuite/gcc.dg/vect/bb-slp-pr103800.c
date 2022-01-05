/* { dg-do compile } */
/* { dg-additional-options "-O3" } */

int a;
long b;
extern int c[], d[];
extern _Bool e[];
void f() {
  if (a)
    ;
  for (;;) {
    for (int g = 2; g; g = a)
      d[g] = 0;
    for (int h = 1; h < 13; h++)
      e[h] = b ? (short)c[4 + h - 1] : c[4 + h - 1];
  }
}
