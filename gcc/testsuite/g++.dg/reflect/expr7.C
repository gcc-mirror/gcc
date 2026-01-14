// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test using enum.

// TODO cannot take the reflection of a using-declarator
enum class Color { R, G, B };
struct S { using enum Color; };

static_assert(^^S::R != ^^S::G);
static_assert(^^S::R != ^^Color::R);
static_assert([:^^S::R:] == Color::R);
