// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

typedef concept int CINT; // { dg-error "'concept' cannot appear in a typedef declaration" }

void f(concept int); // { dg-error "a parameter cannot be declared 'concept'" }

template<typename T>
concept int f2() { return 0; } // { dg-error "return type" }
concept bool f3(); // { dg-error "14:concept .f3. has no definition" }

struct X
{
  template<typename T>
  concept int f4() { return 0; } // { dg-error "return type|member function" }
  concept bool f5() { return true; } // { dg-error "member function" }
  template<typename T>
  static concept bool f6() { return true; } // { dg-error "a concept cannot be a member function" }
  static concept bool x; // { dg-error "declared 'concept'" }
			 // { dg-error "uninitialized const" "" { target *-*-* } .-1 }
  concept int x2; // { dg-error "declared 'concept'" }
  concept ~X(); // { dg-error "a destructor cannot be 'concept'" }
  concept X(); // { dg-error "a constructor cannot be 'concept'" }
};

concept bool X2; // { dg-error "non-template variable" }

template<typename T>
  concept bool X3; // { dg-error "has no initializer" }

struct S {
  template<typename T>
    static concept bool C1 = true; // { dg-error "static data member" }
};
