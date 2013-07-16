// PR c++/56913
// { dg-do compile { target c++11 } }

template<typename T>
T &&declval();

template<typename T, typename U,
         typename = decltype((declval<T>().*declval<U>())())>
constexpr bool test(int)
{
  return true;
}

template<typename T, typename U>
constexpr bool test(...)
{
  return false;
}

struct S
{};

static_assert(!test<S, void (S::*)() &>(0), "");
static_assert(test<S, void (S::*)() &&>(0), "");
static_assert(test<S &, void (S::*)() &>(0), "");
static_assert(!test<S &, void (S::*)() &&>(0), "");
