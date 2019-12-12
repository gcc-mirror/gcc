// PR c++/79520
// { dg-do compile { target c++14 } }

constexpr int f(int const& x) { return x; }

constexpr struct S {
    int x = 0;
    constexpr S() {(void)f(x); x = 1;}
} s;

static_assert(f(s.x) == 1, "");
