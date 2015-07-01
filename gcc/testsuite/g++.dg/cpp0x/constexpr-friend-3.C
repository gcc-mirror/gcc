// PR c++/65977
// { dg-do compile { target c++11 } }

template<__SIZE_TYPE__>
class bitset;

template<__SIZE_TYPE__ N>
constexpr bool operator==(const bitset<N>&, const bitset<N>&) noexcept;

template<__SIZE_TYPE__ N>
class bitset
{
  friend constexpr bool operator== <>(const bitset<N>&,
				      const bitset<N>&) noexcept;
};

template<__SIZE_TYPE__ N>
constexpr bool operator==(const bitset<N>&, const bitset<N>&) noexcept
{
  return true;
}
