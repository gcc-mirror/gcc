// PR c++/63528
// { dg-do compile { target c++14 } }

template<class...>
struct X
{
  constexpr static bool value = true;
};

static_assert(X<int>::value, "");

template <class... Args>
constexpr bool X_v = X<Args...>::value;

static_assert(X_v<int>, "");
