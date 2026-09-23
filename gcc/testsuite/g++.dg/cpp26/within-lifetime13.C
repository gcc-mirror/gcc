// PR c++/127506
// { dg-do compile { target c++20 } }

namespace std {
  template <class T>
  consteval bool
  is_within_lifetime (const T *p) noexcept
  {
    return __builtin_is_within_lifetime (p);
  }
}

struct S { char a; };
constexpr S s = { int b; };	// { dg-error "expected" }

bool
bar ()
{
  return (!std::is_within_lifetime (&s.a));	// { dg-error "is not a constant expression" }
}
