// C++ 26 P3394R4 - Annotations for Reflection
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::annotations_of.

#include <meta>
#include <ranges>
#include <vector>

[[=1, =1, =2, =1.0f]] void foo ();
static_assert ((std::meta::annotations_of (^^foo)
		| std::views::transform (std::meta::constant_of)
		| std::ranges::to<std::vector> ())
	       == std::vector { std::meta::reflect_constant (1),
				std::meta::reflect_constant (1),
				std::meta::reflect_constant (2),
				std::meta::reflect_constant (1.0f) });

[[=1]] [[=2]] void bar ();
[[=3]] [[=3]] void bar ();
[[=4L, =42]] void bar ();
static_assert ((std::meta::annotations_of (^^bar)
		| std::views::transform (std::meta::constant_of)
		| std::ranges::to<std::vector> ())
	       == std::vector { std::meta::reflect_constant (1),
				std::meta::reflect_constant (2),
				std::meta::reflect_constant (3),
				std::meta::reflect_constant (3),
				std::meta::reflect_constant (4L),
				std::meta::reflect_constant (42) });
