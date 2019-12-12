// Like cond-triv1.C, but with the declaration order swapped.
// { dg-do compile { target concepts } }

#include <type_traits>

template <typename T>
class optional
{
  struct empty {};
  union {
    empty _ = { };
    T value;
  };
  bool engaged = false;

public:
  constexpr optional() = default;

  constexpr optional(optional const& o)
    : engaged (o.engaged)
  {
    if (engaged)
      new (&value) T (o.value);
  }
  constexpr optional(optional const&)
    requires std::is_trivially_copy_constructible_v<T>
    = default;

  ~optional()
  {
    if (engaged)
      value.~T();
  }
  ~optional()
    requires std::is_trivially_destructible_v<T>
    = default;

  // ...
};

struct A { A(); A(const A&); ~A(); };

static_assert(std::is_trivially_copy_constructible_v<optional<int>>);
static_assert(!std::is_trivially_copy_constructible_v<optional<A>>);
static_assert(std::is_trivially_destructible_v<optional<int>>);
static_assert(!std::is_trivially_destructible_v<optional<A>>);
