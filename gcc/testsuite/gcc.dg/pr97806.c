/* { dg-do compile } */
/* { dg-options "-O2" } */

int b;
long c;
int g();
void h(long *);
void i(long *);
void d() {
  int e, f = b - e;
  if (g())
    h(&c + f);
  else
    i(&c + f);
  __builtin_memset(0, 0, f * 8);
}
