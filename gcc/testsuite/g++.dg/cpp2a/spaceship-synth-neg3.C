// PR c++/92496
// { dg-do compile { target c++2a } }

template<auto V>
struct A {};

struct B {
    constexpr auto operator<=>(const B&) const = default; // { dg-error "" }
    int value;
};

A<B{}> t;
