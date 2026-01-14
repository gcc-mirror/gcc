// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test from https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2025/p2996r13.html#compile-time-ticket-counter

#include <meta>

template<int N> struct Helper;

struct TU_Ticket {
  static consteval int latest ()
  {
    int k = 0;
    while (is_complete_type (substitute (^^Helper,
					 { std::meta::reflect_constant (k) })))
      ++k;
    return k;
  }
  static consteval void increment ()
  {
    define_aggregate (substitute (^^Helper,
				  { std::meta::reflect_constant (latest ()) }),
		      {});
  }
};
constexpr int x = TU_Ticket::latest ();  // x initialized to 0.
consteval { TU_Ticket::increment (); }
constexpr int y = TU_Ticket::latest ();  // y initialized to 1.
consteval { TU_Ticket::increment (); }
constexpr int z = TU_Ticket::latest ();  // z initialized to 2.
static_assert (x == 0);
static_assert (y == 1);
static_assert (z == 2);
