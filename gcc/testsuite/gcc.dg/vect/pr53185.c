/* { dg-do compile } */
/* { dg-additional-options "-O3" } */

unsigned short a, e;
int *b, *d;
int c;
extern int fn2();
void fn1 () {
  void *f;
  for (;;) {
    fn2 ();
    b = f;
    e = 0;
    for (; e < a; ++e)
      b[e] = d[e * c];
  }
}
