// P2864R2 - Remove Deprecated Arithmetic Conversion on Enumerations From C++26
// { dg-do compile { target c++20 } }

enum A { a };
enum B { b };

template <auto X, auto Y> decltype (true ? X : Y) f1 () { throw 1; }
// { dg-error "enumerated mismatch in conditional expression: 'A' vs 'B'" "" { target c++26 } .-1 }
// { dg-error "conditional expression between enumeration type 'A' and floating-point type 'double'" "" { target c++26 } .-2 }
// { dg-error "conditional expression between floating-point type 'double' and enumeration type 'A'" "" { target c++26 } .-3 }
template <auto X, auto Y> decltype (X + Y) f2 () { throw 1; }
// { dg-error "arithmetic between different enumeration types 'A' and 'B'" "" { target c++26 } .-1 }
// { dg-error "arithmetic between enumeration type 'A' and floating-point type 'double'" "" { target c++26 } .-2 }
// { dg-error "arithmetic between floating-point type 'double' and enumeration type 'A'" "" { target c++26 } .-3 }
template <auto X, auto Y> decltype (X | Y) f3 () { throw 1; }
// { dg-error "bitwise operation between different enumeration types 'A' and 'B'" "" { target c++26 } .-1 }
template <auto X, auto Y> decltype (X < Y) f4 () { throw 1; }
// { dg-error "comparison between 'enum A' and 'enum B'" "" { target c++26 } .-1 }
// { dg-error "comparison of enumeration type 'A' with floating-point type 'double'" "" { target c++26 } .-2 }
// { dg-error "comparison of floating-point type 'double' with enumeration type 'A'" "" { target c++26 } .-3 }

int
main ()
{
  f1<a, a> ();
  f2<a, a> ();
  f3<b, b> ();
  f4<b, b> ();
  f1<a, b> ();		// { dg-error "no matching function for call to" "" { target c++26 } }
  f2<a, b> ();		// { dg-error "no matching function for call to" "" { target c++26 } }
  f3<a, b> ();		// { dg-error "no matching function for call to" "" { target c++26 } }
  f4<a, b> ();		// { dg-error "no matching function for call to" "" { target c++26 } }
  f1<a, 0.0> ();	// { dg-error "no matching function for call to" "" { target c++26 } }
  f2<a, 0.0> ();	// { dg-error "no matching function for call to" "" { target c++26 } }
  f4<a, 0.0> ();	// { dg-error "no matching function for call to" "" { target c++26 } }
  f1<0.0, a> ();	// { dg-error "no matching function for call to" "" { target c++26 } }
  f2<0.0, a> ();	// { dg-error "no matching function for call to" "" { target c++26 } }
  f4<0.0, a> ();	// { dg-error "no matching function for call to" "" { target c++26 } }
}
