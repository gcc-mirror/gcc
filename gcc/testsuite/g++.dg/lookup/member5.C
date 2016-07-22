// PR c++/69753
// { dg-do compile { target c++11 } }

struct B {
  template <class> void bfn ();
};

template <class T>
constexpr int x(T) { return 42; }

template <int I>
struct C
{
  template <class> void cfn ();
};

template <typename T> struct A {
  static B fn(int);
  template <class U> static B ft(U);

  void g()
  {
    auto b = this->fn(42);
    b.bfn<int>();

    auto b2 = this->ft(42);
    b2.bfn<int>();

    auto c = C<x(42)>();
    c.cfn<int>();
  }
};
