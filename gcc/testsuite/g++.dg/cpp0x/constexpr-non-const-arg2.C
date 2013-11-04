// PR c++/47200
// { dg-options "-std=c++11 -w" }

template < int > struct duration
{
  constexpr int count ();
  static constexpr duration min ();
};

constexpr int
f (duration < 0 > d, duration < 0 > )
{
  return d.count ();
}

static_assert (f (duration < 0 >::min (), duration < 0 > ()), ""); // { dg-error "non-constant|before its definition" }
