// PR c++/90748
// { dg-do compile { target c++11 } }

template <class ...Ts> class A
{
  constexpr bool e () { return true; };
  bool f (int() noexcept(this->e())); // { dg-error "this" }
  bool g (int() noexcept(e()));	      // { dg-error "without object" }
};
A<> b;
