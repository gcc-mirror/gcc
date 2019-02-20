// PR c++/88380
// { dg-do compile { target c++14 } }
// { dg-options "" }

struct S {
  char uninit;
  char initialised = 11;
  char variable[];
};

constexpr S p {};
#define SA(X) static_assert((X),#X)
SA(p.initialised == 11);
