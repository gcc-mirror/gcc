// We used to crash on this instead of giving a decent error.
// { dg-do compile { target c++11 } }

struct A { int i; };

struct B {
  const A *a;
  constexpr B(const A& a): a(&a) { }
};

constexpr B b{A{42}};		// { dg-error "constant|expansion" }
