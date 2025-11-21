// PR c++/119964
// { dg-do compile { target c++14 } }

struct A {
    int i;
    constexpr A(int v) : i(v) {}
    constexpr A(const A&&);
};

constexpr int f() {
    A a(1);
    A b = static_cast<const A&&>( a );
    return b.i;
}

constexpr A::A(const A&&) = default; // { dg-error "does not match the expected signature" }
static_assert(f () == 1, "");
