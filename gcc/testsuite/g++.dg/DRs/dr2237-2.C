// DR 2237 - Can a template-id name a constructor?
// { dg-options "" }

template<class T>
struct X {
  X<T>(); // { dg-warning "template-id not allowed for constructor" "" { target c++20 } }
  X(int); // OK, injected-class-name used
  ~X<T>(); // { dg-warning "template-id not allowed for destructor" "" { target c++20 } }
};
