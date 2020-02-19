// PR c++/93169 - Wrong-code with a non-constexpr constructor.
// { dg-do run { target c++11 } }
// { dg-options "-O2" }

template <typename T> class B {
  struct C {
    T h;
    constexpr C() {}
    ~C() {}
  } c;
};
struct S {
  int g;
  S() { g = 2; }
};

int
main()
{
  static const B<S> f;
}
