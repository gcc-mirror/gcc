// PR c++/88187
// { dg-do compile }

template <int> struct A;
void f (A ());	// { dg-error "6:variable or field 'f' declared void" "" { target c++14_down } }
		// { dg-error "missing template arguments before '\\(' token" "" { target c++14_down } .-1 }
		// { dg-error "deduced class type 'A' in function return type" "" { target c++17 } .-2 }
