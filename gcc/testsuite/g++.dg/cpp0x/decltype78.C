// PR c++/81339
// { dg-do compile { target c++11 } }

struct true_type { static const bool value = true; };
struct false_type { static const bool value = false; };

template <typename T>
struct IsDefaultConstructibleT
{
    // using T here (instead of U) should be an error
    template <typename U, typename = decltype(T())> // { dg-error "deleted" }
      static true_type test(void*);

    template <typename U>
      static false_type test(...);

    static constexpr bool value = decltype(test<T>(nullptr))::value;
};

struct S {
  S() = delete;
};

static_assert( IsDefaultConstructibleT<S>::value, "" ); // { dg-error "assertion failed" }
