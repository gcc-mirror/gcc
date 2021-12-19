// PR c++/103681
// { dg-do compile { target c++11 } }
// { dg-additional-options "-fabi-version=16 -Wabi" }

struct A {
  long l;
  char c = -1;
};
struct B : public A {
  char d;
  // { dg-warning "offset" "" { target c++14 } .-1 }
};

#define SA(X) static_assert(X,#X)
SA(sizeof (B) == sizeof (A));
// { dg-error "static assertion" "" { target c++14 } .-1 }

struct X { char d; };
struct B2 : A, X { };
// { dg-warning "offset" "" { target c++14 } .-1 }
SA(sizeof (B2) == sizeof (A));
// { dg-error "static assertion" "" { target c++14 } .-1 }

#if __cplusplus > 201800L

struct C {
  [[no_unique_address]] A a;
  char d;
  // { dg-warning "offset" "" { target c++20 } .-1 }
};
SA(sizeof (C) == sizeof (A));
// { dg-error "static assertion" "" { target c++20 } .-1 }

struct C2 : A, X { };
// { dg-warning "offset" "" { target c++20 } .-1 }
SA(sizeof (B2) == sizeof (A));
// { dg-error "static assertion" "" { target c++20 } .-1 }

#endif /* C++20 */
