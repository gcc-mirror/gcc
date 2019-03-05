// { dg-do compile { target c++17 } }

#include <utility>
#include <tuple>

#define LIFT_FWD(x) std::forward<decltype(x)>(x)

template <typename T>
inline
constexpr
auto
equal(
  T &&t)
{
  return [t = std::forward<T>(t)](const auto& obj)
    -> decltype(obj == t)
    {
      return obj == t;
    };
}

template <typename F, typename T>
struct is_tuple_invocable;

template <typename F, typename ... Ts>
struct is_tuple_invocable<F, std::tuple<Ts...>>
{
  using type = typename std::is_invocable<F, Ts...>::type;
};

template <typename F>
inline
constexpr
auto
compose(
  F&& f
)
  noexcept
-> F
{
  return std::forward<F>(f);
}

namespace detail {
  template <typename F, typename Tail, typename ... T>
  inline
  constexpr
  auto
  compose(
    std::true_type,
    F&& f,
    Tail&& tail,
    T&& ... objs)
  noexcept(noexcept(f(tail(std::forward<T>(objs)...))))
  -> decltype(f(tail(std::forward<T>(objs)...)))
  {
    return f(tail(std::forward<T>(objs)...));
  }
}
template <typename F, typename ... Fs>
inline
constexpr
auto
compose(
  F&& f,
  Fs&&... fs)
{
  return [f = std::forward<F>(f), tail = compose(std::forward<Fs>(fs)...)]
    (auto&& ... objs)
    -> decltype(detail::compose(typename std::is_invocable<decltype(compose(std::forward<Fs>(fs)...)), decltype(objs)...>::type{},
                                f,
                                compose(std::forward<Fs>(fs)...),
                                LIFT_FWD(objs)...))
  {
    using tail_type = decltype(compose(std::forward<Fs>(fs)...));
    
#ifndef NOT_VIA_TUPLE
    using args_type = std::tuple<decltype(objs)...>;
    constexpr auto unitail = typename is_tuple_invocable<tail_type, args_type>::type{};
#else
    constexpr auto unitail = typename std::is_invocable<tail_type, decltype(objs)...>::type{};
#endif

    return detail::compose(unitail,  f, tail, LIFT_FWD(objs)...);
  };
}

template <auto N>
constexpr auto eq = equal(N);

static_assert(compose(eq<3>,
		      std::plus<>{})(1,2),
              "compose is constexpr");
