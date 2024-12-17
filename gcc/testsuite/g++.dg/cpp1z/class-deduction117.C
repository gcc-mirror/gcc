// PR c++/116108
// { dg-do compile { target c++11 } }
template <class T>
struct S { const S s = 1; };	// { dg-error "field 's' has incomplete type 'const S<T>'" }
S t{2};				// { dg-error "invalid use of template-name 'S' without an argument list" "" { target c++14_down } }
// { dg-error "class template argument deduction failed" "" { target c++17 } .-1 }
// { dg-error "no matching function for call to 'S\\\(int\\\)'" "" { target c++17 } .-2 }
