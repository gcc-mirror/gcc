// PR target/69432
// { dg-do compile }
// { dg-options "-O3" }
// { dg-additional-options "-minline-stringops-dynamically" { target i?86-*-* x86_64-*-* } }

template <typename S, typename T, typename U>
void
f1 (S x, T y, U z)
{
  for (; y; --y, ++x)
    *x = z;
}

template <typename S, typename T, typename U>
void f2 (S x, T y, U z)
{
  f1 (x, y, z);
}

struct A {};
struct B { static char f3 (A, unsigned); };

template <typename S, typename U>
void f4 (S, U);

struct C
{
  template <typename S, typename T, typename U>
  static S f5 (S x, T y, U z) { f2 (x, y, z); return S(); }
};

template <typename S, typename T, typename U>
void f6 (S x, T y, U z) { C::f5 (x, y, z); }

template <typename S, typename T, typename U, typename V>
void f7 (S x, T y, U z, V) { f6 (x, y, z); }

struct E
{
  struct D : A { char e; D (A); };
  A f;
  E (int x) : g(f) { f8 (x); }
  ~E ();
  D g;
  void f9 (int x) { x ? B::f3 (g, x) : char (); }
  void f8 (int x) { f9 (x); }
};

struct F : E
{
  F (int x) : E(x) { f10 (x); f4 (this, 0); }
  char h;
  void f10 (int x) { f7 (&g.e, x, h, 0); }
};

long a;

void
test ()
{
  F b(a);
}
