// PR c++/105908
// { dg-do compile { target c++11 } }

struct test
{
  template <typename T>
  int templated_func();

  template <typename T>
  auto call_templated_func() -> decltype(templated_func<T>());
};

template <typename T>
auto test::call_templated_func() -> decltype(templated_func<T>())
{
  return templated_func<T>();
}
