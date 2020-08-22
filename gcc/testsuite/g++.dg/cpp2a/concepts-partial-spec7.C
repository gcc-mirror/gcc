// PR c++/92103
// { dg-do compile { target c++20 } }

template<int M>
struct traits
{
  template<int N>
    struct equal_to
    { static constexpr bool value = false; };

  template<int N> requires (M == N)
    struct equal_to<N>
    { static constexpr bool value = true; };

  template<int N> requires (M < 0) || (N < 0)
    struct equal_to<N>
    { };
};

static_assert(traits<0>::equal_to<0>::value);
static_assert(!traits<0>::equal_to<1>::value);
static_assert(traits<-1>::equal_to<0>::value); // { dg-error "not a member" }
