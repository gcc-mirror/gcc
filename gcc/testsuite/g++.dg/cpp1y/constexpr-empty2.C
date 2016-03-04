// { dg-do compile { target c++14 } }

struct A
{
  constexpr A(int) { }
};

struct B: A {
  constexpr B(int i): A(i) { }
  constexpr B(const B& b): A(b) { }
};

struct C {
  B b;
  constexpr C(int i): b(i) { }
  constexpr C(const C&c): b(c.b) {}
};

constexpr int f()
{
  C b1{42};
  C b2{b1};
  b2.b;
  return 42;
}

constexpr int i = f();
