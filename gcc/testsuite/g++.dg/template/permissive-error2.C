// PR c++/116064
// { dg-additional-options -fpermissive }
// Verify we correctly mark a partial specialization as erroneous
// instead its primary template.

template<class T>
struct A { };

template<class T>
struct A<T*> { // { dg-error "instantiating erroneous template" }
  void f(typename A::type); // { dg-warning "does not name a type" }
};

A<int> a;  // { dg-bogus "" }
A<int*> b; // { dg-message "required from here" }
