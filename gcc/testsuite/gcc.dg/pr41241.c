/* PR bootstrap/41241 */
/* { dg-do compile } */
/* { dg-options "-O2 -fcompare-debug -g" } */
/* { dg-options "-O2 -fcompare-debug -g -march=i586 -mtune=i586 -fomit-frame-pointer" { target { { i?86-*-* x86_64-*-* } && ia32 } } } */
/* { dg-xfail-if "" { powerpc-ibm-aix* } { "*" } { "" } } */

typedef struct { int t1; int t2; int t3; } *T;
typedef struct { int s; } S;

int fn1 (int);
int fn2 (int, int, int);
int fn3 (void);

static S
bar ()
{
  S s = { 0 };
  return s;
}

void
foo (T x)
{
  int a, b, c, d, e;
  T f, g;
  S h;
  a = x->t2;
  fn1 (x->t1);
  h = bar (b);
  c = fn1 (e);
  d = fn3 ();
  f->t3 &= g->t3 |= fn2 (0, b, x->t1);
  fn2 (0, c, d);
  fn2 (0, e, 0);
}
