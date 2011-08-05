// PR c++/47453
// { dg-options "-std=c++0x -pedantic-errors" }

// invalid
int a({0});			// { dg-error "" }

// invalid
int const &b({0});		// { dg-error "" }

// invalid
struct A1 { int a[2]; A1(); };
A1::A1():a({1, 2}) { }		// { dg-error "" }

struct A { explicit A(int, int); A(int, long); };

// invalid
A c({1, 2});			// { dg-error "" }

// valid (by copy constructor).
A d({1, 2L});

// valid
A e{1, 2};

#include <initializer_list>

struct B {
  template<typename ...T>
  B(std::initializer_list<int>, T ...);
};

// invalid (the first phase only considers init-list ctors)
// (for the second phase, no constructor is viable)
B f{1, 2, 3};			// { dg-error "" }

// valid (T deduced to <>).
B g({1, 2, 3});
