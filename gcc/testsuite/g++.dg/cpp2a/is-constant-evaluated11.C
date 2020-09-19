// PR c++/97051
// { dg-do compile { target c++20 } }

namespace std {
  constexpr inline bool
  is_constant_evaluated () noexcept
  {
    return __builtin_is_constant_evaluated ();
  }
}

template<typename>
  requires (std::is_constant_evaluated())
constexpr int a = 0;

constexpr int b = a<int>;
