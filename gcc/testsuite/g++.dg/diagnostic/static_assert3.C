// PR c++/97518
// { dg-do compile { target c++17 } }
// { dg-options "-fdiagnostics-show-caret" }

template <typename T, typename U> struct is_same { static constexpr bool value = false; };
template <typename T> struct is_same<T, T> { static constexpr bool value = true; };

/* { dg-begin-multiline-output "" }
  f(0, 1.3);
  ~^~~~~~~~
   { dg-end-multiline-output "" } */

template <typename T, typename U>
void f(T, U)
{
  static_assert(is_same<T, T>::value && is_same<T, U>::value); // { dg-error "56:static assertion failed" }
/* { dg-begin-multiline-output "" }
   static_assert(is_same<T, T>::value && is_same<T, U>::value);
                                                        ^~~~~
   { dg-end-multiline-output "" } */
// { dg-message ".is_same<int, double>::value. evaluates to false" "" { target *-*-* } .-5 }
  static_assert(is_same<U, T>::value && is_same<U, U>::value); // { dg-error "32:static assertion failed" }
/* { dg-begin-multiline-output "" }
   static_assert(is_same<U, T>::value && is_same<U, U>::value);
                                ^~~~~
   { dg-end-multiline-output "" } */
// { dg-message ".is_same<double, int>::value. evaluates to false" "" { target *-*-* } .-5 }
  static_assert(is_same<U, U>::value
		&& is_same<U, T>::value // { dg-error "35:static assertion failed" }
		&& is_same<T, T>::value);
/* { dg-begin-multiline-output "" }
                 && is_same<U, T>::value
                                   ^~~~~
   { dg-end-multiline-output "" } */
// { dg-message ".is_same<double, int>::value. evaluates to false" "" { target *-*-* } .-6 }
}

void g()
{
 f(0, 1.3); // { dg-message " required from here" }
}
