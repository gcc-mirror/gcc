// N5008 :
// dcl.contract.res/p1
// The result-name-introducer of a postcondition-specifier is a declaration. The result-name-introducer introduces
// the identifier as the name of a result binding of the associated function. If a postcondition assertion has a
// result-name-introducer and the return type of the function is cv void, the program is ill-formed.
// { dg-do compile { target c++23 } }
// { dg-additional-options "-fcontracts" }


void f(const int i) post (r: i > 2); // { dg-error "function does not return a value to test" }

template <typename T>
void g() post (r: true ){}; // { dg-error "function does not return a value to test" }

template
void g<int>();

struct S{

  S() post (r : true); // { dg-error "does not return a value to test" }
  void f(const int i) post (r: i > 2); // { dg-error "function does not return a value to test" }
  ~S() post (r : true); // { dg-error "does not return a value to test" }
};
