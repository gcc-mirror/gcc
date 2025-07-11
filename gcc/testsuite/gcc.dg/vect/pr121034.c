/* { dg-do compile } */
/* { dg-additional-options "-O3" } */

int b, e;
char c, d;
unsigned g;
int abs(int);
void f() {
  char *a = &d;
  int h;
  for (; e; e++) {
    h = 0;
    for (; h < 16; h++)
      g += __builtin_abs(a[h] - c);
    a += b;
  }
}
