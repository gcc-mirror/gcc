// PR c++/99885
// { dg-do compile { target c++17 } }

template <auto const& A>
struct Foo {};

template <auto const& A>
struct Bar {
    constexpr auto foo() const -> Foo<A> {
        return {};
    }
};

constexpr int a = 1;
constexpr Bar<a> bar;
Foo foo = bar.foo(); // <-- CTAD failure
