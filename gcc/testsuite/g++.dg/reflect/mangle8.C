// PR c++/125680
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

#include <meta>

struct A { struct { int b; } a[4]; };

template <std::meta::info I>
struct B
{
  consteval static std::meta::info
  foo ()
  {
    if constexpr (is_array_type (I))
      {
	using C = typename [: remove_extent (I) :];
	return ^^C;
      }
    else
      {
        constexpr auto c = type_of (nonstatic_data_members_of (I, std::meta::access_context::current ())[0]);
        using C = typename [: c :];
        return ^^C;
      }
  }
  constexpr static std::meta::info b = foo ();
};

constexpr auto a = B <^^A>::b;
constexpr auto b = B <a>::b;
constexpr auto c = B <b>::b;
