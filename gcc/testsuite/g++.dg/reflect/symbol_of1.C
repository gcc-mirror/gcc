// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::symbol_of.

#include <meta>

using namespace std::meta;

consteval bool
has_symbol_of (operators op)
{
  try { symbol_of (op); }
  catch (std::meta::exception &) { return false; }
  return true;
}

static_assert (!has_symbol_of (static_cast <operators> (4242)));
static_assert (symbol_of (op_new) == std::string_view ("new"));
static_assert (symbol_of (op_delete) == std::string_view ("delete"));
static_assert (symbol_of (op_array_new) == std::string_view ("new[]"));
static_assert (symbol_of (op_array_delete) == std::string_view ("delete[]"));
static_assert (symbol_of (op_co_await) == std::string_view ("co_await"));
static_assert (symbol_of (op_parentheses) == std::string_view ("()"));
static_assert (symbol_of (op_square_brackets) == std::string_view ("[]"));
static_assert (symbol_of (op_arrow) == std::string_view ("->"));
static_assert (symbol_of (op_arrow_star) == std::string_view ("->*"));
static_assert (symbol_of (op_tilde) == std::string_view ("~"));
static_assert (symbol_of (op_exclamation) == std::string_view ("!"));
static_assert (symbol_of (op_plus) == std::string_view ("+"));
static_assert (symbol_of (op_minus) == std::string_view ("-"));
static_assert (symbol_of (op_star) == std::string_view ("*"));
static_assert (symbol_of (op_slash) == std::string_view ("/"));
static_assert (symbol_of (op_percent) == std::string_view ("%"));
static_assert (symbol_of (op_caret) == std::string_view ("^"));
static_assert (symbol_of (op_ampersand) == std::string_view ("&"));
static_assert (symbol_of (op_equals) == std::string_view ("="));
static_assert (symbol_of (op_pipe) == std::string_view ("|"));
static_assert (symbol_of (op_plus_equals) == std::string_view ("+="));
static_assert (symbol_of (op_minus_equals) == std::string_view ("-="));
static_assert (symbol_of (op_star_equals) == std::string_view ("*="));
static_assert (symbol_of (op_slash_equals) == std::string_view ("/="));
static_assert (symbol_of (op_percent_equals) == std::string_view ("%="));
static_assert (symbol_of (op_caret_equals) == std::string_view ("^="));
static_assert (symbol_of (op_ampersand_equals) == std::string_view ("&="));
static_assert (symbol_of (op_pipe_equals) == std::string_view ("|="));
static_assert (symbol_of (op_equals_equals) == std::string_view ("=="));
static_assert (symbol_of (op_exclamation_equals) == std::string_view ("!="));
static_assert (symbol_of (op_less) == std::string_view ("<"));
static_assert (symbol_of (op_greater) == std::string_view (">"));
static_assert (symbol_of (op_less_equals) == std::string_view ("<="));
static_assert (symbol_of (op_greater_equals) == std::string_view (">="));
static_assert (symbol_of (op_spaceship) == std::string_view ("<=>"));
static_assert (symbol_of (op_ampersand_ampersand) == std::string_view ("&&"));
static_assert (symbol_of (op_pipe_pipe) == std::string_view ("||"));
static_assert (symbol_of (op_less_less) == std::string_view ("<<"));
static_assert (symbol_of (op_greater_greater) == std::string_view (">>"));
static_assert (symbol_of (op_less_less_equals) == std::string_view ("<<="));
static_assert (symbol_of (op_greater_greater_equals) == std::string_view (">>="));
static_assert (symbol_of (op_plus_plus) == std::string_view ("++"));
static_assert (symbol_of (op_minus_minus) == std::string_view ("--"));
static_assert (symbol_of (op_comma) == std::string_view (","));
