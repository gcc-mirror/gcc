// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test consteval-only type smuggling.

using info = decltype(^^int);

template<typename T>
constexpr const void *
foo (const T *__location)
{
  const void *__loc = __location;
  return __loc;
}

template<typename T>
constexpr const void *
bar (const T *__location)
{
  const void *__loc = __location;
  return __loc;
}

void
g ()
{
  constexpr static auto r = ^^int;
  constexpr auto x = foo<info>(&r); // { dg-error "pointer into an object of consteval-only type" }
  constexpr auto y = bar<info>(nullptr);
}
