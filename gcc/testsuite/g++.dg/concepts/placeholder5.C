// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

template <class T, class U>
concept bool Same = __is_same_as(T, U);

const int i = 0;
template <class T>
concept bool C =
  requires {
    { &i } -> const Same<T>*;
  };

template <C c>
constexpr bool f() { return true; }

static_assert(f<double>(), "");	// { dg-error "" }
static_assert(f<int>(), "");
