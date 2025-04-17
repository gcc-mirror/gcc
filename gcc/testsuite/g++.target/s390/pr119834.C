// PR target/119834
// { dg-do compile { target c++11 } }
// { dg-options "-O2 -march=z900" }

int *a;
struct A;
struct B {
  A begin ();
  A end ();
  operator bool * ();
  void operator++ ();
};
template <typename T>
auto operator| (int, T x) -> decltype (x (0));
struct A : B { bool a; };
struct C { A operator () (int); };
enum D {} d;
int e;
void foo ();
struct E {
  template <typename T>
  T *garply ()
  {
    if (d)
      return 0;
    if (e)
      foo ();
    return reinterpret_cast<T *> (f);
  }
  template <typename>
  void bar (long x, bool)
  {
    if (&g - f)
      __builtin_memset (a, 0, x);
    f += x;
  }
  template <typename T>
  T *baz (T *x, long y, bool z = true)
  {
    if (d)
      return nullptr;
    bar<T> ((char *)x + y - f, z);
    return x;
  }
  template <typename T>
  void qux (T x) { baz (x, x->j); }
  char *f, g;
} *h;
struct F {
  template <typename T>
  int corge (T x) { x.freddy (this); return 0; }
  template <typename T>
  int boo (T x) { corge (x); return 0; }
} i;
template <typename T>
struct G {
  template <typename U> friend T operator+ (U, G);
  template <typename U>
  void waldo (F *x, G y, U z) { x->boo (z + y); }
  template <typename... Ts>
  void plugh (E *y, Ts... z) { T *x = y->garply<T> (); x->thud (y, z...); }
};
template <typename T> using H = G<T>;
struct I {
  static constexpr unsigned j = 2;
  void thud (E *x, A y) { x->qux (this); for (auto g : y) ; }
};
H<I> k;
struct J {
  void freddy (F *) { C a; auto b = 0 | a; k.plugh (h, b); }
};
H<J> l;
struct K {
  void freddy () { l.waldo (&i, l, this); }
};
void grault () { K m; m.freddy (); }
