// { dg-additional-options "-fmodules-ts -std=c++20" }

import M;

struct Variant0
{
};


struct Variant2
{
  using element_type = double;
};

struct Variant3
{
  using value_type = float;
};

void f()
{
  using v0 = traits<Variant0>;
  using v1 = traits<Variant0 *>;
  using v2 = traits<Variant2 *>;
  using v3 = traits<Variant3 *>;

  static_assert (v0::variant == 0);
  static_assert (v1::variant == 1);
  static_assert (v2::variant == 2);
  static_assert (v3::variant == 3);
}
