// PR c++/50508
// { dg-options -std=c++11 }

template <class T>
  struct integral_constant {
    typedef T value_type;
    constexpr operator value_type() { return true; }
  };

static constexpr bool value = integral_constant<bool>()
                              && true;
