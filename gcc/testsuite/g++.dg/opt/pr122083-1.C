// { dg-do compile { target c++20 } }
// { dg-options "-O2 -Wall" }

// Make sure we don't get a -Wnonnull warning here.

#include <compare>

  template<typename Tp>
    constexpr auto
    min_cmp(Tp x, Tp y)
    {
      struct Res {
	Tp M_min;
	decltype(x <=> y) M_cmp;
      };
      auto c = x <=> y;
      if (c > 0)
	return Res{y, c};
      return Res{x, c};
    }


  template<typename InputIter1, typename InputIter2>
    auto
    lexicographical_compare_three_way(InputIter1 first1,
				      InputIter1 last1,
				      InputIter2 first2,
				      InputIter2 last2)
    -> decltype(*first1 <=> *first2)
    {
      const auto [len, lencmp] =
        min_cmp(last1 - first1, last2 - first2);
      if (len)
      {
        const auto blen = len * sizeof(*first1);
        const auto c
          = __builtin_memcmp(&*first1, &*first2, blen) <=> 0;
        if (c != 0)
          return c;
      }
      return lencmp;
    }

auto
test03()
{
  unsigned char a[2] = { 1, 2 };
  unsigned char* p = nullptr;
  return lexicographical_compare_three_way(p, p, a, a+2);
}
