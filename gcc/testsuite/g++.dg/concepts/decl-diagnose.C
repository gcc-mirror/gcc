// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }
// { dg-prune-output "concept definition syntax is" }

typedef concept int CINT; // { dg-error "'concept' cannot appear in a typedef declaration" }

void f(concept int); // { dg-error "a parameter cannot be declared 'concept'" }

template<typename T>
concept int f2() { return 0; } // { dg-error "function concepts are no longer supported" }
concept bool f3(); // { dg-error "14:function concepts are no longer supported" }
		   // { dg-error "keyword is not allowed" "" { target *-*-* } .-1 }

struct X
{
  template<typename T>
  concept int f4() { return 0; } // { dg-error "cannot be a member" }
  concept f5 = true; // { dg-error "declared 'concept'" }
  template<typename T>
  static concept f6 = true; // { dg-error "declared 'concept'" }
  static concept bool x; // { dg-error "declared 'concept'" }
			 // { dg-error "uninitialized 'const" "" { target *-*-* } .-1 }
			 // { dg-error "keyword is not allowed" "" { target *-*-* } .-2 }
  concept int x2; // { dg-error "declared 'concept'" }
  concept ~X(); // { dg-error "a destructor cannot be 'concept'" }
  concept X(); // { dg-error "a constructor cannot be 'concept'" }
};

concept bool X2; // { dg-error "variable" }
	         // { dg-error "keyword is not allowed" "" { target *-*-* } .-1 }

template<typename T>
  concept bool X3; // { dg-error "variable concepts" }
		   // { dg-error "keyword is not allowed" "" { target *-*-* } .-1 }

struct S {
  template<typename T>
    static concept bool C1 = true; // { dg-error "static data member" }
				   // { dg-error "keyword is not allowed" "" { target *-*-* } .-1 }
};
