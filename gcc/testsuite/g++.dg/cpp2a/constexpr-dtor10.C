// PR c++/97427
// { dg-do compile { target c++20 } }

struct Foo {
    int n = 1;
    constexpr ~Foo() {
        n = 0;
    }
};

constexpr bool foo() {
    const Foo b;
    return true;
}

static_assert(foo());
