// PR c++/92695
// { dg-do compile { target c++20 } }

struct A { virtual int get() = 0; };
struct B : A { constexpr int get() override { return 10; } };
struct D { B b[2]; A* c{&(b[0])}; };
static_assert(D{}.c->get() == 10);
