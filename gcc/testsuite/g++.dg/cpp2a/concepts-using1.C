// PR c++/79591
// { dg-do compile { target c++20 } }

template <class> concept True = true;

// Fine.
namespace X {
  void f(auto) {}
  void f(True auto) {}
}

void f(auto) {}
namespace Y {
  void f(True auto) {}
  using ::f;
  // error: 'template<class auto:3> void f(auto:3)' conflicts with a previous declaration
}
