// PR c++/125591
// { dg-do compile { target c++20 } }
// { dg-options "" }

template <typename T, typename U>
concept is_same_v = __is_same (T, U);

struct A { int x, y, z; };
struct B { int x; long y; };

template <class V>
consteval bool
foo ()
{
  constexpr auto [...Ms] = V {};		// { dg-warning "structured binding packs only available with" "" { target c++23_down } }
						// { dg-warning "structured binding declaration can be 'constexpr' only with" "" { target c++23_down } .-1 }
  using T = decltype (Ms...[0]);		// { dg-warning "pack indexing only available with" "" { target c++23_down } }
  return (is_same_v<decltype(Ms), T> && ...);
}
