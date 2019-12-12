// PR c++/82782
// { dg-do compile { target c++17 } }

template <const auto& Value>
struct make_char_sequence;

template <int N, const char (&StringLiteral)[N]>
struct make_char_sequence<StringLiteral>
{
  using type = int;
};

template <const auto& StringLiteral>
using make_char_sequence_t = typename make_char_sequence<StringLiteral>::type;

inline constexpr char sample[] = "Sample";

using X = make_char_sequence_t<sample>;
