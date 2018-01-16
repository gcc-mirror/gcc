// PR c++/34937, 34962
// { dg-require-weak "" }
// { dg-options "" }
// { dg-require-effective-target alloca }

struct A
{
  static const int i;
};

template<int> void foo()
{
  int x[A::i] __attribute((vector_size(8)));
}

template<int> struct B
{
  enum { a, b = a };
  void bar(B<b>) __attribute((weak));
};

void f()
{
  foo<0>();
  B<0> b;
  b.bar (B<B<0>::b>());
}
