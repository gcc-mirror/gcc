// PR tree-optimization/119491
// { dg-do compile { target { external_musttail && c++11 } } }
// { dg-options "-O2" }

struct A {
  struct B {};
  A () {}
};
void qux ();
unsigned char v;
A w;
void foo (A);

template <typename T>
[[gnu::always_inline]] static inline void
bar (int &)
{
}

[[gnu::always_inline]] static inline void
baz (int *)
{
  int r = 0;
  bar<int> (r);
}

[[gnu::always_inline]] inline void
corge (A)
{
  if (v)
    qux ();
  [[gnu::musttail]] return foo (w);
}

void
freddy (A)
{
  int t;
  baz (&t);
  [[gnu::musttail]] return corge (A{});
}
