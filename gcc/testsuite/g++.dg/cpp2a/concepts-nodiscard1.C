// PR c++/98019
// { dg-do compile { target c++20 } }
// Don't give [[nodiscard]] warning for an expression requirement.

template <class T, class U> concept same_as = __is_same_as (T, U);

[[nodiscard]] int foo() { return 0; }
[[maybe_unused]] constexpr bool b = requires {
    { foo() } -> same_as<int>;
};
[[maybe_unused]] constexpr auto x = sizeof(foo());
