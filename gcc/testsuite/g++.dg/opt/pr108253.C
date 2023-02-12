// PR tree-optimization/108253
// { dg-do compile { target c++11 } }
// { dg-options "-O2" }

struct S
{
  int *s;
  S () : s (new int) {}
  S (const S &r) noexcept : s (r.s) { __atomic_fetch_add (r.s, 1, 4); }
};
struct T
{
  explicit T (const S &x) : t (x) {}
  const S t;
};
struct U
{
  operator int () const { new T (u); return 0; }
  S u;
};
bool foo (int matcher);
unsigned long bar (unsigned long pos, unsigned long end_pos);
struct V
{
  alignas (4) char v[4];
};
struct W
{
  void baz ()
  {
    if (!w) __builtin_abort ();
    if (reinterpret_cast <__UINTPTR_TYPE__> (w->v) % 4 != 0) __builtin_abort ();
    __builtin_unreachable ();
  }
  [[gnu::noinline]] void qux (unsigned long) { if (!w) bar (0, x); } 
  V *w = nullptr;
  unsigned x = 0;
};

void
test ()
{
  W w;
  U t;
  if (!foo (t))
    w.baz ();
  w.qux (0);
}
