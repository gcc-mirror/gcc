// PR c++/56450
// { dg-do compile { target c++11 } }

template<typename T>
T&& declval();

template<typename, typename>
struct is_same
{ static constexpr bool value = false; };

template<typename T>
struct is_same<T, T>
{ static constexpr bool value = true; };

struct A { static const int dummy = 0; };

static_assert(is_same<decltype(declval<A>().dummy), const int>::value, "");
static_assert(!is_same<decltype(declval<A>().dummy), const int&>::value, "");
