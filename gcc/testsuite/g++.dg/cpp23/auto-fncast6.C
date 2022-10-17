// PR c++/103049
// P0849R8 - auto(x)
// { dg-do compile { target c++20 } }

void f (int);

void
g ()
{
  auto a1 = auto(f); // { dg-error "only available with" "" { target c++20_only } }
  auto a2 = auto{f}; // { dg-error "only available with" "" { target c++20_only } }
  static_assert (__is_same_as (decltype (a1), void(*)(int)));
  static_assert (__is_same_as (decltype (a2), void(*)(int)));
}
