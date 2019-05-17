// { dg-do compile }
// { dg-options "-std=gnu++14 -pedantic" }

template<typename T>
  struct is_float
  {
    static constexpr bool value = false;
  };

template<>
  struct is_float<float>
  {
    static constexpr bool value = true;
  };

template<typename T>
  T
  float_thing(T __x)
  {
    static_assert(is_float<T>::value, ""); // { dg-error "static assertion failed" }
    static_assert(is_float<T>::value); // { dg-error "static assertion failed" }
    return T();
  }

int
main()
{
  float_thing(1);
}

// { dg-warning "'static_assert' without a message only available with " "" { target *-*-* } 21 }
