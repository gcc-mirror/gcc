/* Reduced version of c-c++-common/torture/builtin-convertvector-1.c.  */
/* This should NOT ICE */

/* { dg-do compile } */

typedef long b __attribute__((vector_size(256 * sizeof(long))));
typedef double c __attribute__((vector_size(256 * sizeof(double))));
int d;
void e(b *f, c *g) { *g = __builtin_convertvector(*f, c); }
void h() {
  struct {
    b i;
  } j;
  union {
    c i;
    double a[6];
  } k;
  e(&j.i, &k.i);
  if (k.a[d])
    for (;;)
      ;
}
