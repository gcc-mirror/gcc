// PR c++/115900
// { dg-do compile { target c++20 } }

struct A {
    char m;
    constexpr A() { m = 0; }
};

struct C {
  constexpr C(){ };
};

struct B : C {
  A a;
  constexpr B() {}
};

struct D : B { };

static constexpr A a;
static constexpr B b;
static constexpr D d;
