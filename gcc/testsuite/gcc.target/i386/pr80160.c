/* { dg-do compile } */
/* { dg-options "-O2 -fno-omit-frame-pointer -w" } */
/* { dg-additional-options "-march=pentium-mmx" { target ia32 } } */

typedef struct { long long a; } a_t;
int *a, b;
a_t *e, c;
long long f;
void fn (int);
void fn2 (void);
int fn3 (a_t);
void fn4 (a_t);
a_t foo (long long val) { return (a_t){val}; }
static void
bar (int ka)
{
  unsigned i;
  for (i = 0; i < 512; i++) {
    long d;
    c = (a_t){d};
    fn2 ();
  }
  fn (ka);
}
void
test (void)
{
  a_t g;
  a_t *h, j;
  h = e;
  j = *h;
  if (e == (a_t *) 1) {
    a_t k = {fn3 (j)};
    fn4 (j);
    long l;
    g = foo((long long)b << 2 | l);
    k = g;
    if (j.a != k.a) {
      a_t m = g;
      int n = m.a, o = m.a >> 32;
      asm ("# %0 %1 %2 %3" : "=m"(*a), "+A"(f) : "b"(n), "c"(o));
    }
  }
  bar ((int) h);
}
