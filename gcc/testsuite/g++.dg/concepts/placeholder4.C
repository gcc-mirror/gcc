// PR c++/66218
// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

template <class T, class U>
concept Same = __is_same_as(T, U);

template <class T>
concept C =
  requires { // { dg-message "in requirements" }
    { 0 } -> Same<T>;		// { dg-message "does not satisfy" }
  };

template <class T>
struct A {
  template <T t, C c>
  constexpr static bool f() { return true; }
};

static_assert(A<int>::f<1,double>(), "");	// { dg-error "" }
static_assert(A<char>::f<'a',int>(), "");
