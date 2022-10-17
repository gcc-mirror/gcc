// { dg-do compile { target c++11 } }
template<bool u> struct bool_c{ static constexpr bool value = u; };
template<class T> auto noexcepty(T t) -> bool_c<noexcept(t())>;
template<class T> auto noexcepty(T t) -> bool_c<noexcept(t())>;
struct foo { void operator()() noexcept; };
static_assert(decltype(noexcepty(foo{}))::value, "");
