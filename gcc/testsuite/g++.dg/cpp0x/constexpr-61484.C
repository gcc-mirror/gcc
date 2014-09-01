// PR c++/61484
// { dg-do compile { target c++11 } }

struct S {
  constexpr S() { }
};

constexpr S s[][1] = {
    { S() }
};
