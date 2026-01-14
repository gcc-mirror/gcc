// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::reflect_constant_string.

#include <meta>
#include <ranges>
#include <span>

using namespace std::meta;

struct V { int a, b, c; };
constexpr auto a = reflect_constant_string (std::vector <int> { 42, 43, 44 });	// { dg-error "'reflect_constant_string' called with 'std::ranges::range_value_t<std::vector<int> >' \\\{aka 'int'\\\} rather than 'char', 'wchar_t', 'char8_t', 'char16_t' or 'char32_t'" }
constexpr auto b = reflect_constant_string (std::vector <float> {});		// { dg-error "'reflect_constant_string' called with 'std::ranges::range_value_t<std::vector<float> >' \\\{aka 'float'\\\} rather than 'char', 'wchar_t', 'char8_t', 'char16_t' or 'char32_t'" }
constexpr auto c = reflect_constant_string (std::vector <V> { V { 1, 2, 3 } });	// { dg-error "'reflect_constant_string' called with 'std::ranges::range_value_t<std::vector<V> >' \\\{aka 'V'\\\} rather than 'char', 'wchar_t', 'char8_t', 'char16_t' or 'char32_t'" }
