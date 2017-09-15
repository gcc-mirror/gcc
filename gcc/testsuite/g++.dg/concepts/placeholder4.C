// PR c++/66218
// { dg-options "-std=c++17 -fconcepts" }

template <class T, class U>
concept bool Same = __is_same_as(T, U);

template <class T>
concept bool C =
  requires {
    { 0 } -> Same<T>;
  };

template <class T>
struct A {
  template <T t, C c>
  constexpr static bool f() { return true; }
};

static_assert(A<int>::f<1,double>(), "");	// { dg-error "" }
static_assert(A<char>::f<'a',int>(), "");
