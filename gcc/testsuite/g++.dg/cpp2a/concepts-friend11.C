// CWG2596
// { dg-do compile { target c++20 } }

struct Base {};

int foo(Base&) { return 0; } // #0

template<int N>
struct S : Base {
  friend int foo(Base&) requires (N == 1) { return 1; }  // #1
  // friend int foo(Base&) requires (N == 2) { return 3; }  // #2
};

S<1> s1;
S<2> s2;          // OK, no conflict between #1 and #0
int x = foo(s1);  // { dg-error "ambiguous" }
int y = foo(s2);  // OK, selects #0

// ??? currently the foos all mangle the same, so comment out #2
// and only test that #1 isn't multiply defined and overloads with #0.
// The 2596 example does not include #0 and expects both calls to work.
