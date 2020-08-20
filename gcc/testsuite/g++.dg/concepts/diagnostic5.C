// { dg-do compile { target c++2a } }
// { dg-additional-options "-fconcepts-diagnostics-depth=2" }

template<typename T>
  concept c1 = requires { typename T::blah; };
// { dg-message "satisfaction of .c1<T>. .with T = char." "" { target *-*-* } .-1 }
// { dg-message ".typename T::blah. is invalid" "" { target *-*-* } .-2 }

template<typename T>
  concept c2 = requires (T x) { *x; };
// { dg-message "satisfaction of .c2<T>. .with T = char." "" { target *-*-* } .-1 }
// { dg-message "in requirements with .T x. .with T = char." "" { target *-*-* } .-2 }
// { dg-message "required expression .* is invalid" "" { target *-*-* } .-3 }

template<typename T>
  concept c3 = __is_same(T, const T) || __is_same(T, int);
// { dg-message "satisfaction of .c3<T>. .with T = char." "" { target *-*-* } .-1 }
// { dg-message "no operand of the disjunction is satisfied" "" { target *-*-* } .-2 }

template<typename T>
  concept c4 = requires (T x) { requires c2<const T> || c2<volatile T>; };
// { dg-message "satisfaction of .c4<T>. .with T = char." "" { target *-*-* } .-1 }
// { dg-message "nested requirement" "" { target *-*-* } .-2 }

template<typename T>
  concept c5 = requires (T x) { { &x } -> c1; };
// { dg-message "satisfaction of .c5<T>. .with T = char." "" { target *-*-* } .-1 }
// { dg-message "in requirements with .T x. .with T = char." "" { target *-*-* } .-2 }

template<typename T>
  requires (c1<T> || c2<T>) || (c3<T> || c4<T>) || c5<T> // { dg-message "49: no operand" }
  // { dg-message ".c1<T>. is unsatisfied because" "" { target *-*-* } .-1 }
  // { dg-message ".c2<T>. is unsatisfied because" "" { target *-*-* } .-2 }
  // { dg-message ".c3<T>. is unsatisfied because" "" { target *-*-* } .-3 }
  // { dg-message ".c4<T>. is unsatisfied because" "" { target *-*-* } .-4 }
  // { dg-message ".c5<T>. is unsatisfied because" "" { target *-*-* } .-5 }
  void foo() { }

void
bar()
{
  foo<char>(); // { dg-error "no match" }
}
