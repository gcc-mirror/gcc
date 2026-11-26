// PR c++/128386
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection -fsanitize=undefined" }
// Test consteval-only values are stripped before ubsan pass.

void foo() {
    static constexpr auto x = ^^int;
}
