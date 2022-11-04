// PR c++/101165 - P2266R1 - Simpler implicit move
// { dg-do compile { target c++23 } }
// Test decltype(auto) more.

template<typename T, typename U>
struct same_type { static const bool value = false; };

template<typename T>
struct same_type<T, T> { static const bool value = true; };

struct Widget {
  int x;
};

Widget wg;

decltype(auto) fn0(Widget&& x) {
    return (::wg);
}
static_assert(same_type<decltype(fn0), Widget& (Widget&&)>::value);

decltype(auto) fn1(Widget&& x) {
    return ::wg;
}
static_assert(same_type<decltype(fn1), Widget (Widget&&)>::value);

decltype(auto) fn2() {
    Widget w;
    return w;
}
static_assert(same_type<decltype(fn2), Widget ()>::value);

decltype(auto) fn3() {
    Widget w;
    return (w); // { dg-warning "reference to local variable" }
}
static_assert(same_type<decltype(fn3), Widget&& ()>::value);

decltype(auto) fn4() {
    Widget w;
    return w.x;
}
static_assert(same_type<decltype(fn4), int ()>::value);

decltype(auto) fn5() {
    Widget w;
    return (w.x); // { dg-warning "reference to local variable" }
}
static_assert(same_type<decltype(fn5), int& ()>::value);
