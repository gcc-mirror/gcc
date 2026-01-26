// PR c++/123640
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

#include <meta>

template<class T>
constexpr std::size_t field_count {
  std::meta::nonstatic_data_members_of(^^T, std::meta::access_context::unchecked()).size()
};

template<std::size_t index, class T>
constexpr std::meta::info field_at {
  std::meta::nonstatic_data_members_of(^^T, std::meta::access_context::unchecked())[index]
};

struct S {
  int a, b, c;
  constexpr bool operator<(this auto&&l, auto&&r) noexcept
  {
    using T = std::remove_cvref_t<decltype (l)>;
    static constexpr auto N{field_count<T>};
    if constexpr (N == 0)
      return false;
    else
      {
	template for (constexpr auto i : std::make_index_sequence<N - 1>{}) // { dg-bogus "constant" "" { xfail *-*-* } }
	  if (l.[:field_at<i,T>:] != r.[:field_at<i,T>:]) [[likely]]
	    return l.[:field_at<i,T>:] < r.[:field_at<i,T>:];
	return l.[:field_at<N - 1, T>:] < r.[:field_at<N - 1, T>:];
      }
  }
};

int
main ()
{
  return S{1,2,3} < S{1,3,2};
}
