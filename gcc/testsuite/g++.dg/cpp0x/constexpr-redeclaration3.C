// PR c++/111132
// { dg-do compile { target c++11 } }

constexpr bool bar(void) {
    return true;
}

constexpr bool foo() {
    constexpr bool bar(void);
    return bar();
}

static_assert(foo(), "");
