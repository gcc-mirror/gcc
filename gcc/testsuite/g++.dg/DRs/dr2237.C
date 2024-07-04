// DR 2237 - Can a template-id name a constructor?

template<class T>
struct X {
  X<T>(); // { dg-error "template-id not allowed for constructor" "" { target c++20 } }
  X(int); // OK, injected-class-name used
  ~X<T>(); // { dg-error "template-id not allowed for destructor" "" { target c++20 } }
};

// ill-formed since DR1435
template<typename T> X<T>::X<T>() {} // { dg-error "names the constructor|as no template constructors" }
template<typename T> X<T>::~X<T>() {} // { dg-error "template-id not allowed for destructor" "" { target c++20 } }

struct Q {
  // ill-formed since DR1435
  template<typename T> friend X<T>::X<T>(); // { dg-error "names the constructor|as no template constructors" }
  template<typename T> friend X<T>::~X<T>(); // { dg-error "template-id not allowed for destructor" "" { target c++20 } }
};
