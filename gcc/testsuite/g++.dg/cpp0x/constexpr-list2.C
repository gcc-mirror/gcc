// PR c++/82461
// { dg-do compile { target c++11 } }

class A {
private:
public:
  constexpr A() {}
  ~A() {}
};

class B {
private:
  A  a;
public:
  constexpr B() : a{} {}
// works
// constexpr B() : a() {}

  ~B() {}
};
