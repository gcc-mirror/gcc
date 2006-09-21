//PR c++/28303

template<typename T> struct A
{
  typedef struct typename T::X X;       // { dg-error "expected identifier|two or more" }
};

template<typename T> A<T>::X::X() {}    // { dg-error "not a type|forbids declaration" }
