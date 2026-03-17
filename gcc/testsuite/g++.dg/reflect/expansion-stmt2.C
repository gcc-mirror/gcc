// PR c++/124229
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

#include <meta>

enum class E
{
  X [[=1]]
};

template<typename T>
void foo ()
{
}

void
f ()
{
  template for (constexpr auto val : define_static_array (enumerators_of (^^E)))
    {
      constexpr auto a = annotations_of(val)[0];
      using U = [:type_of(a):];
      constexpr auto m1 = extract<U>(a);
      constexpr auto m2 = extract<typename [:type_of(a):]>(a);
    }

  template for (constexpr auto val : define_static_array (enumerators_of (^^E)))
    {
      constexpr auto a = annotations_of(val)[0];
      using U = [:type_of(a):];
      foo<U>();
    }

  [](auto a) {
      using V = [:type_of(^^a):];;
      constexpr auto rt = std::meta::reflect_constant (1);
      constexpr auto m1 = std::meta::extract<V>(rt);
  }(1);
}
