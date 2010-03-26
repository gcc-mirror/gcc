// PR c++/43024
// { dg-options "-O2" }

void foo();

template<int> struct X
{
  enum { e };
  typedef int Y;
};

template<int N = 0> struct A
{
  ~A() { foo(); }
  A() { a<0>(0); }
  template<int> void a(typename X<!X<N>::e>::Y);
  struct B b();
};

struct B
{
  A<> b0, b1, b2, b3;
  B operator+ (const B&);
};

struct C
{
  A<> c0, c1, c2, c3, c4, c5, c6, c7, c8;
};

inline void bar(int i)
{
  A<> a0, a1;
  if (i) a0.b() + a0.b() + a0.b() + a0.b();
}

void baz()
{
  C c;
  bar(0);
}
