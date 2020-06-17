// PR c++/90748
// { dg-do compile { target c++11 } }

template <class ...> class A
{
  void e ();
  bool f (int() noexcept(e));	// { dg-error "::e" }
};
A<> b;
