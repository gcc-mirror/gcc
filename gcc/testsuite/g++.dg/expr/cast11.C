// { dg-do compile { target c++11 } }
// { dg-options "-Wignored-qualifiers" }
// c++/80544 cast expressions returned cv-qualified prvalues

template<typename T> void f(T&&) { }
template<typename T> void f(T const&&) = delete;

template<typename T> void g(T&&) = delete;
template<typename T> void g(T const&&) { }

struct B { int i; const char c; } b = {};

void f1()
{
  int i = 0;
  f((long const)i);			// { dg-warning "qualifiers ignored" }
  f((int* const)&i);			// { dg-warning "qualifiers ignored" }
  f((int const* const)&i);		// { dg-warning "qualifiers ignored" }
  f((long* const)&i);			// { dg-warning "qualifiers ignored" }

  f(static_cast<long const>(i));	// { dg-warning "qualifiers ignored" }
  f(reinterpret_cast<long const>(&i));	// { dg-warning "qualifiers ignored" }

  f(static_cast<int* const>(&i));	// { dg-warning "qualifiers ignored" }
  f(const_cast<int* const>(&i));	// { dg-warning "qualifiers ignored" }
  f(reinterpret_cast<long* const>(&i));	// { dg-warning "qualifiers ignored" }

  using ptrmem = int B::*;
  f(static_cast<ptrmem const>(&B::i));	// { dg-warning "qualifiers ignored" }
  f(const_cast<ptrmem const>(&B::i));	// { dg-warning "qualifiers ignored" }
  f(reinterpret_cast<ptrmem const>(&B::i)); // { dg-warning "qualifiers ignored" }

  // No warnings, not a cv-qualified type:
  using ptrmem2 = const char B::*;
  f(static_cast<ptrmem2>(&B::c));
  f(const_cast<ptrmem2>(&B::c));
  f(reinterpret_cast<ptrmem2>(&B::c));

  // prvalue of class type can have cv-quals:
  g(static_cast<const B>(b));
}
