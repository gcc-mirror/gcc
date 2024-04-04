// PR c++/114537
// { dg-do compile { target c++20 } }

namespace std {
template<typename T, typename F>
constexpr T
bit_cast (const F& f) noexcept
{
  return __builtin_bit_cast (T, f);
}
}

struct A { signed char b : 1 = 0; signed char c : 7 = 0; };
struct D { unsigned char e; };
constexpr unsigned char f = std::bit_cast<D> (A{}).e;
static_assert (f == 0);
