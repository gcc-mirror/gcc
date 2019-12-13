// PR c++/66218
// { dg-do compile { target c++17_only } }
// { dg-options "-fconcepts" }

template <class T, class U>
concept bool Same = __is_same_as(T, U);

template <class T>
concept bool C =
  requires { // { dg-message "in requirements" }
    { 0 } -> Same<T>;		// { dg-message "does not satisfy" }
  };

template <C c>
constexpr bool f() { return true; }

static_assert(f<double>(), "");	// { dg-error "" }
static_assert(f<int>(), "");
