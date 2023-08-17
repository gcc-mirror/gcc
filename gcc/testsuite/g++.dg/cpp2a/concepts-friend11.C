// CWG2596
// { dg-do compile { target c++20 } }
// { dg-additional-options -fno-implicit-constexpr }

struct Base {};

template<int N>
struct S : Base {
  friend int foo(Base&) requires (N == 1) { return 1; }  // #1
  friend int foo(Base&) requires (N == 2) { return 3; }  // #2

  template <class T>
  friend int bar(Base&) requires (N == 1) { return 1; }
  template <class T>
  friend int bar(Base&) requires (N == 2) { return 3; }
};

S<1> s1;
S<2> s2;          // OK, no conflict between #1 and #2

// { dg-final { scan-assembler "_ZN1SILi1EEF3fooER4Base" } }
int x = foo(s1);  // OK, selects #1
// { dg-final { scan-assembler "_ZN1SILi2EEF3fooER4Base" } }
int y = foo(s2);  // OK, selects #2

// { dg-final { scan-assembler "_ZN1SILi1EEF3barIiEEiR4Base" } }
int x2 = bar<int>(s1);  // OK, selects #1
// { dg-final { scan-assembler "_ZN1SILi2EEF3barIiEEiR4Base" } }
int y2 = bar<int>(s2);  // OK, selects #2
