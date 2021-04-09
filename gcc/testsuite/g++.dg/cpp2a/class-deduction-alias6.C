// PR c++/93295
// { dg-do compile { target c++20 } }

template<typename T, bool B = false>
struct Foo {
    Foo(T) {}
};

template<typename T> Foo(T) -> Foo<T>;
template<typename T> using Bar = Foo<T, true>;
Bar b{0};
