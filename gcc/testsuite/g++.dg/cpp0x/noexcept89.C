// PR c++/117106
// { dg-do compile { target c++11 } }

struct A {
    int x;
    template<class>
    void foo() noexcept(noexcept(x)) {}
    auto bar() -> decltype(foo<int>()) {} // { dg-error "not available until end of class" }
};
