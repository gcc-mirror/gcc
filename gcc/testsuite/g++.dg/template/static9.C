// PR c++/17524

template<typename T> struct A
{
  static const T i = 0; // { dg-error "declared void" "void" }
			// { dg-error "invalid|non-literal" "invalid" { target *-*-* } 5 }
};

A<void> a; // { dg-message "instantiated" }
