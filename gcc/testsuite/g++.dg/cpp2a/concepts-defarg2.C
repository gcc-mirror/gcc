// PR c++/97052
// { dg-do compile { target c++20 } }

template<typename T, typename U = typename T::type>
concept C = true;

constexpr bool f(C auto) {
  return true;
}

static_assert(f(0));

C auto x = 0;
