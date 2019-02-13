// Test that [[no_unique_address]] makes the enclosing class non-layout-POD.
// { dg-do compile { target c++11 } }

struct A {};
struct B1: A {
  int i;
  char c;
};

struct B2 {
  [[no_unique_address]] A a;
  int i;
  char c;
};

struct C1: B1 {
  char d;
};

struct C2: B2 {
  char d;
};

#define SA(X) static_assert((X),#X)
SA(sizeof(C1) == sizeof(C2));
