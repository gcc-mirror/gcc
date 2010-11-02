// Test that we explain why a template instantiation isn't constexpr
// { dg-options -std=c++0x }
// { dg-prune-output "not a constexpr function" }

template <class T>
struct A
{
  T t;
  constexpr int f() { return 42; }
};

struct B { B(); operator int(); };

constexpr A<int> ai = { 42 };
constexpr int i = ai.f();

constexpr int b = A<B>().f();	// { dg-error "enclosing class" }

template <class T>
constexpr int f (T t) { return 42; }
constexpr int x = f(B());	// { dg-error "parameter" }
