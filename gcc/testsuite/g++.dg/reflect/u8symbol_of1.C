// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::u8symbol_of.

#include <meta>

using namespace std::meta;

consteval bool
has_u8symbol_of (operators op)
{
  try { u8symbol_of (op); }
  catch (std::meta::exception &) { return false; }
  return true;
}

static_assert (!has_u8symbol_of (static_cast <operators> (4242)));
static_assert (u8symbol_of (op_new) == std::u8string_view (u8"new"));
static_assert (u8symbol_of (op_delete) == std::u8string_view (u8"delete"));
static_assert (u8symbol_of (op_array_new) == std::u8string_view (u8"new[]"));
static_assert (u8symbol_of (op_array_delete) == std::u8string_view (u8"delete[]"));
static_assert (u8symbol_of (op_co_await) == std::u8string_view (u8"co_await"));
static_assert (u8symbol_of (op_parentheses) == std::u8string_view (u8"()"));
static_assert (u8symbol_of (op_square_brackets) == std::u8string_view (u8"[]"));
static_assert (u8symbol_of (op_arrow) == std::u8string_view (u8"->"));
static_assert (u8symbol_of (op_arrow_star) == std::u8string_view (u8"->*"));
static_assert (u8symbol_of (op_tilde) == std::u8string_view (u8"~"));
static_assert (u8symbol_of (op_exclamation) == std::u8string_view (u8"!"));
static_assert (u8symbol_of (op_plus) == std::u8string_view (u8"+"));
static_assert (u8symbol_of (op_minus) == std::u8string_view (u8"-"));
static_assert (u8symbol_of (op_star) == std::u8string_view (u8"*"));
static_assert (u8symbol_of (op_slash) == std::u8string_view (u8"/"));
static_assert (u8symbol_of (op_percent) == std::u8string_view (u8"%"));
static_assert (u8symbol_of (op_caret) == std::u8string_view (u8"^"));
static_assert (u8symbol_of (op_ampersand) == std::u8string_view (u8"&"));
static_assert (u8symbol_of (op_equals) == std::u8string_view (u8"="));
static_assert (u8symbol_of (op_pipe) == std::u8string_view (u8"|"));
static_assert (u8symbol_of (op_plus_equals) == std::u8string_view (u8"+="));
static_assert (u8symbol_of (op_minus_equals) == std::u8string_view (u8"-="));
static_assert (u8symbol_of (op_star_equals) == std::u8string_view (u8"*="));
static_assert (u8symbol_of (op_slash_equals) == std::u8string_view (u8"/="));
static_assert (u8symbol_of (op_percent_equals) == std::u8string_view (u8"%="));
static_assert (u8symbol_of (op_caret_equals) == std::u8string_view (u8"^="));
static_assert (u8symbol_of (op_ampersand_equals) == std::u8string_view (u8"&="));
static_assert (u8symbol_of (op_pipe_equals) == std::u8string_view (u8"|="));
static_assert (u8symbol_of (op_equals_equals) == std::u8string_view (u8"=="));
static_assert (u8symbol_of (op_exclamation_equals) == std::u8string_view (u8"!="));
static_assert (u8symbol_of (op_less) == std::u8string_view (u8"<"));
static_assert (u8symbol_of (op_greater) == std::u8string_view (u8">"));
static_assert (u8symbol_of (op_less_equals) == std::u8string_view (u8"<="));
static_assert (u8symbol_of (op_greater_equals) == std::u8string_view (u8">="));
static_assert (u8symbol_of (op_spaceship) == std::u8string_view (u8"<=>"));
static_assert (u8symbol_of (op_ampersand_ampersand) == std::u8string_view (u8"&&"));
static_assert (u8symbol_of (op_pipe_pipe) == std::u8string_view (u8"||"));
static_assert (u8symbol_of (op_less_less) == std::u8string_view (u8"<<"));
static_assert (u8symbol_of (op_greater_greater) == std::u8string_view (u8">>"));
static_assert (u8symbol_of (op_less_less_equals) == std::u8string_view (u8"<<="));
static_assert (u8symbol_of (op_greater_greater_equals) == std::u8string_view (u8">>="));
static_assert (u8symbol_of (op_plus_plus) == std::u8string_view (u8"++"));
static_assert (u8symbol_of (op_minus_minus) == std::u8string_view (u8"--"));
static_assert (u8symbol_of (op_comma) == std::u8string_view (u8","));
