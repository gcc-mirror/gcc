// PR c++/60199
// { dg-do compile { target c++11 } }

void f() {}

static constexpr void (*g1)() = &f;
static constexpr void (*g2)() = f;
struct S {
    static constexpr void (*g3)() = &f;
    static constexpr void (*g4)() = f;
};
