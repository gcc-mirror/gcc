// PR c++/102123
// { dg-do compile { target c++17 } }

template<template<typename...> typename Template, typename... Args>
struct _dummy_forwarder {
    using type = Template<Args...>;
};

template<template<typename...> typename Template, typename... Args>
using dummy_forwarder = typename _dummy_forwarder<Template, Args...>::type;

template<typename T>
struct Test {
    template<typename U> using _dummy = U;

    using Element = dummy_forwarder<_dummy, T>;

    Element _elem;

    constexpr Test(const Element elem) : _elem(elem) { }
};

template<typename T>
Test(T) -> Test<T>;

void test() {
    const auto t = Test(1);
}
