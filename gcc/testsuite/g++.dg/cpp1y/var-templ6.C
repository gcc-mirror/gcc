// { dg-do compile { target c++14 } }

template<typename T>
  constexpr bool Class = __is_class(T);

template<typename T>
  constexpr bool Test = Class<T>;

struct S { };

static_assert(!Test<int>, "");
static_assert(Test<S>, "");
