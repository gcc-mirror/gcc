// P3074R7 - trivial unions (was std::uninitialized<T>)
// P3726R2 - Adjustments to Union Lifetime Rules
// { dg-do compile { target c++26 } }

#include <memory>
#include <string>

template <typename T, size_t N>
struct FixedVector {
  union { T storage[N]; };
  size_t size = 0;

  constexpr FixedVector () { std::start_lifetime (storage); }

  constexpr ~FixedVector() { std::destroy(storage, storage + size); }

  constexpr void push_back(T const& v) { ::new (storage + size) T(v); ++size; }
};

constexpr size_t
silly_test ()
{
  FixedVector <std::string, 3> v;
  v.push_back ("some sufficiently longer string");
  return v.size;
}

static_assert (silly_test () == 1);
