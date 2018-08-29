// Test that we explain why a template instantiation isn't constexpr
// { dg-do compile { target c++11 } }

template <class T>
struct A
{
  T t;
  constexpr int f() const { return 42; } // { dg-error "enclosing class" "" { target c++11_only } }
};

struct B { B(); operator int(); };

constexpr A<int> ai = { 42 };
constexpr int i = ai.f();

constexpr int b = A<B>().f();	// { dg-error "" }

template <class T>
constexpr int f (T t) { return 42; }
constexpr int x = f(B());	     // { dg-error "non-literal" }
