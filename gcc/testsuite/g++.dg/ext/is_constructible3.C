// PR c++/88368
// { dg-do compile { target c++11 } }

struct A {

  struct B {
    int I = 1;
    B() = default;
  };

  static constexpr bool v = __is_constructible (B);

};

void print()  {
  A::B BB;
}
