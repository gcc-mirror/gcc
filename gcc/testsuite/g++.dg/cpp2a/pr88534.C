// { dg-do compile { target c++20 } }
// { dg-options "-g" }

typedef __SIZE_TYPE__ size_t;

namespace std
{

template <typename T, T... I>
struct integer_sequence
{
  typedef T value_type;
  static constexpr size_t size () noexcept { return sizeof...(I); }
};

template <typename T, T N>
using make_integer_sequence = integer_sequence<T, __integer_pack (N)...>;

template <size_t... I>
using index_sequence = integer_sequence<size_t, I...>;

template <size_t N>
using make_index_sequence = make_integer_sequence<size_t, N>;
}

template <typename T, size_t N> struct S
{
  T content[N];
  using char_type = T;
  template <size_t... I>
  constexpr S (const T (&input)[N], std::index_sequence<I...>) noexcept : content{input[I]...} { }
  constexpr S (const T (&input)[N]) noexcept : S (input, std::make_index_sequence<N> ()) { }
  constexpr size_t size () const noexcept
  {
    if (content[N - 1] == '\0')
      return N - 1;
    else
      return N;
  }
  constexpr T operator[] (size_t i) const noexcept
  {
    return content[i];
  }
  constexpr const T *begin () const noexcept
  {
    return content;
  }
  constexpr const T *end () const noexcept
  {
    return content + size ();
  }
};

template <typename T, size_t N> S (const T (&)[N]) -> S<T, N>;

template <S S>
struct F
{
};

auto
foo ()
{
  F<"test"> f;
}
