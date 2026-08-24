// C++29 P3822R2 - Conditional noexcept specifiers in compound requirements
// { dg-do compile { target c++29 } }

#include <concepts>
#include <memory>
#include <type_traits>
#include <utility>

template <typename R, typename... Args>
struct vtbl { virtual auto call (Args &&...) -> R = 0; virtual ~vtbl () noexcept {}; };

template <typename T, typename R, typename... Args>
struct impl : vtbl <R, Args...> {
  impl (auto &&y) : x { std::forward <decltype (y)> (y) } {}
  auto call (Args &&...xs) -> R override { return x (static_cast <Args &&> (xs)...); }
  T x;
};

template <typename F>
struct any_f;

template <typename X, bool noexc, typename R, typename...Args>
concept invocable_r = requires (X x, Args...xs) {
  { x (static_cast <Args> (xs)...) } noexcept (noexc) -> std::convertible_to <R>;
};

template <typename X, typename T>
concept not_same = !std::same_as <std::decay_t <X>, T>;

template <typename R, typename... Args, bool noexc>
struct any_f <R (Args...) noexcept (noexc)> {
  template <not_same <any_f> T>
  any_f (T &&x)
    requires invocable_r <T &, noexc, R, Args...>
    : f (new impl <std::decay_t <T>, R, Args...> (std::forward <T> (x))) {}
  auto operator () (std::convertible_to <Args> auto &&...xs) noexcept (noexc) -> R {
    return f->call (std::forward <decltype (xs)> (xs)...);
  }
  std::unique_ptr <vtbl <R, Args...>> f;
};

int
main ()
{
  any_f <int (long, long) noexcept> x ([] (long, long) noexcept -> int { return 2; });
}
