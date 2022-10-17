// { dg-do compile { target c++20 } }

#pragma GCC diagnostic ignored "-Winvalid-offsetof"

struct E { };

struct A: virtual E {
  // Deny the vbase offset 0 so it goes at the end.
  [[no_unique_address]] E e1;
  char c;
};

struct B : public A {
  char d;
};

struct C {
  [[no_unique_address]] A a;
  char d;
};

#define SA(X) static_assert ((X),#X)
SA(__builtin_offsetof (B, d) == __builtin_offsetof (C, d));
