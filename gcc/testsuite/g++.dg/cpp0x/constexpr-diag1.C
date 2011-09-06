// Test that we explain why a template instantiation isn't constexpr
// { dg-options -std=c++0x }

template <class T>
struct A
{
  T t;
  constexpr int f() { return 42; } // { dg-error "enclosing class" }
};

struct B { B(); operator int(); };

constexpr A<int> ai = { 42 };
constexpr int i = ai.f();

constexpr int b = A<B>().f();	// { dg-error "non-constexpr function" }

template <class T>
constexpr int f (T t) { return 42; } // { dg-error "parameter" }
constexpr int x = f(B());	     // { dg-error "constexpr" }
