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
    }		// { dg-error "modification of '<temporary>' from outside current evaluation is not a constant expression" }
}

consteval int baz() {
    int r = 0;
#if 0
    // TODO: This doesn't work yet.
    template for (constexpr int I : std::define_static_array(foo())) {
	r += I;
    }
#else
    // Ugly workaround for that.
    template for (constexpr int I : (const std::span<const int>)std::define_static_array(foo())) {
	r += I;
    }
#endif
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

static_assert (baz() == 6);
static_assert (qux() == 6);
static_assert (fred<int>() == 6);
