// // PR c++/65202
// { dg-do compile { target c++14 } }

template <typename T> struct is_move_constructible;
template <typename T> struct is_move_assignable;
template <int, typename T> using enable_if_t = int;
namespace adl {
template <
    typename L, typename R,
    enable_if_t<is_move_constructible<L>() && is_move_assignable<L>(), int>...>
constexpr auto adl_swap(L &l, R &r) -> decltype(swap(l, r)) {
  return;
}
template <typename L, typename R>
auto swap(L &l, R &r) noexcept(noexcept(adl::adl_swap(l, r)))
    -> decltype(adl::adl_swap(l, r));
namespace ns {
template <typename T> struct foo {};
template <typename T> void swap(foo<T> &, foo<T> &);
struct bar;

int main()
{
    foo<ns::bar> f;
    adl::swap(f, f)
} // { dg-error "" }
