// PR c++/55992
// { dg-do compile { target c++11 } }

template<unsigned N>
struct A {};

template<unsigned MaxP>
struct test
{
  static constexpr unsigned pole(unsigned P)
  { return P>MaxP? MaxP:P; }

  template<unsigned P>
  using my_array = A<pole(P)>;

  template<unsigned P>
  void do_something(my_array<P> const&, my_array<P>);
};
