// PR c++/87327
// { dg-do compile { target c++17 } }

template <int N>
struct Foo {
    constexpr auto size() const {
        return N;
    }
};

constexpr int foo() {
    constexpr auto a = Foo<5>{};

    [&] {
        Foo<a.size()> it = {};

        return it;
    }();

    return 42;
}

constexpr int i = foo();
