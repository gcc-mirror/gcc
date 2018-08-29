// Test that a prvalue can be used where a glvalue is expected.
// { dg-do compile { target c++11 } }

struct A { virtual void f(); };
struct B : A {};

auto && a = static_cast<A&&>(B());
auto && b = reinterpret_cast<A&&>(B());
auto && c = dynamic_cast<A&&>(B());
auto && d = dynamic_cast<B&&>(static_cast<A&&>(B()));
auto && e = const_cast<B&&>(B());
