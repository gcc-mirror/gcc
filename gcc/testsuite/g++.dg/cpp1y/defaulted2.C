// PR c++/119964
// { dg-do compile { target c++14 } }

struct A {
    int i;
    constexpr A(int v) : i(v) {}
    constexpr A(const A&&) = default;  // { dg-error "implicitly deleted" "" { target c++17_down } }
				       // { dg-warning "implicitly deleted" "" { target c++20 } .-1 }
};

constexpr int f() {
    A a(1);
    A b = static_cast<const A&&>( a ); // { dg-error "use of deleted function" }
    return b.i;
}
