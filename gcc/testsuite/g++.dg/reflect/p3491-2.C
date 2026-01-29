// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test from P3491R3 3.5.4
// https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2025/p3491r3.html#with-expansion-statements

#include <meta>
#include <ranges>
#include <type_traits>

constexpr auto foo() -> std::vector<int> { return {1, 2, 3}; }

consteval void bar() {
    template for (constexpr int I : foo()) {
        // doesn't work
    }		// { dg-error "'foo\\\(\\\)' is not a constant expression because it refers to a result of 'operator new'" }
}

consteval int baz() {
    int r = 0;
    template for (constexpr int I : std::define_static_array(foo())) {
	r += I;
    }
    return r;
}

consteval int qux() {
    int r = 0;
    template for (constexpr int I : [: std::meta::reflect_constant_array(foo()) :]) {
        r += I;
    }
    return r;
}

template <typename>
consteval int fred() {
    constexpr auto [...m] = [: std::meta::reflect_constant_array(foo()) :];
    return (... + m);
}

consteval int garply() {
    int r = 0;
    template for (constexpr int I : (const std::span<const int>)std::define_static_array(foo())) {
	r += I;
    }
    return r;
}

static_assert (baz() == 6);
static_assert (qux() == 6);
static_assert (fred<int>() == 6);
static_assert (garply() == 6);
