// PR c++/105191
// { dg-do compile { target c++11 } }

struct A {
  const char* message = "";
};

enum class B { };

struct C {
  A a;
  B b;
};

struct D {
  C cs[1];
};

constexpr D ds[4] = {
  D{},
};
