// PR c++/86875
// { dg-do compile { target c++14 } }

template <typename _Tp> using decay_t = _Tp;
template <class Fun> class A {
  Fun fun_;

public:
  template <class T> A(T p1) : fun_(p1) {}
  auto operator()() { fun_(this); }
};

template <class Fun> auto y_combinator(Fun p1) { return A<decay_t<Fun>>(p1); }

int
main()
{
  const unsigned int w = 1;
  auto foo = y_combinator([=](auto) { auto i = +w; });
  foo();
}
