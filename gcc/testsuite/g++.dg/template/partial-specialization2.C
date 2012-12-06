// PR c++/54653

template <class T> struct A;
template <class T> struct A<T*> {
  template <class U> struct B;
};

template <class T> struct A<T*>::B<T*> { }; // { dg-error "too few template-parameter-lists" }
