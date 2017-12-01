// PR c++/79228
// { dg-do compile { target c++14 } }
// { dg-options "-Wpedantic" }

template <class,class> struct same;
template <class T> struct same<T,T> { };

int main()
{
  same<decltype(0i),__complex int>{}; // { dg-warning "literal operator" }
  // { dg-message "complex_literals" "" { target *-*-* } .-1 }
  // { dg-message "built-in" "" { target *-*-* } .-2 }

  same<decltype(0.0i),__complex double>{}; // { dg-warning "literal operator" }
  // { dg-message "complex_literals" "" { target *-*-* } .-1 }
  // { dg-message "built-in" "" { target *-*-* } .-2 }

  same<decltype(0.0if),__complex float>{}; // { dg-warning "literal operator" }
  // { dg-message "complex_literals" "" { target *-*-* } .-1 }
  // { dg-message "built-in" "" { target *-*-* } .-2 }

  same<decltype(0.0il),__complex long double>{}; // { dg-warning "literal operator" }
  // { dg-message "complex_literals" "" { target *-*-* } .-1 }
  // { dg-message "built-in" "" { target *-*-* } .-2 }
}
