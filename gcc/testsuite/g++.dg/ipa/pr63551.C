// { dg-options "-O -Wno-psabi" }
// { dg-do compile }

struct A { int a; };
template <typename T, typename V> struct B { V operator[] (T); };
union U { long double ld; void *v; };
A a;

void
bar (U &x)
{
  if (x.v) *reinterpret_cast <A *>(x.v) = a;
}

struct C { C (A) { c.ld = 0; bar (c); } U c; };
struct D { A d, e; void foo () { f[0][d] = e; } B <int, B <A, C> > f; };

void
baz ()
{
  D d;
  d.foo ();
}
