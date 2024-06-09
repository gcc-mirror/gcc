// PR c++/114138
// { dg-do compile { target c++23 } }

namespace std {
  template <class T>
  T&& declval() noexcept requires true;

  template <class>
  void declval() noexcept;

  namespace detail {
    struct none_such;
    template <class>
    using none_such_t = none_such;

    template <class T>
      extern const none_such_t<T> _getter_for;

    template <class T>
    using _decay_t = decltype(auto(declval<T>()));

    static_assert(__is_same_as(_decay_t<void>, void));
  }

  template <const auto& Fn, class... Args>
    using _result_of_t = decltype(Fn(declval<Args>()...));

  template <unsigned I, class Tuple>
    using tuple_element_t = _result_of_t<detail::_getter_for<detail::_decay_t<Tuple>>, char(*)[I+1], Tuple>;

  template <class First, class Second>
  struct pair {
    First first;
    Second second;
  };

  template <class>
    inline constexpr bool _is_pair = false;
  
  template <class First, class Second>
    inline constexpr bool _is_pair<pair<First, Second>> = true;

  template <class T>
    concept Pair = _is_pair<decltype(auto(std::declval<T>()))>;

  template <unsigned I, Pair P>
    requires (I <= 1)
  decltype(auto) get(P&& p) noexcept {
    if constexpr (I == 0) {
      return (static_cast<P&&>(p).first);
    } else {
      return (static_cast<P&&>(p).second);
    }
  }

  namespace detail {
    inline constexpr auto _pair_getter =
      []<unsigned J, class Pair>(char(*)[J], Pair&& p) noexcept -> decltype(auto) {
        return std::get<J-1>(static_cast<Pair&&>(p));
      };

    template <class First, class Second>
      inline constexpr auto _getter_for<pair<First, Second>> = _pair_getter;
  }

}

static_assert(__is_same_as(int&, std::tuple_element_t<0, std::pair<int, float>&>));
static_assert(__is_same_as(float&&, std::tuple_element_t<1, std::pair<int, float>&&>));
