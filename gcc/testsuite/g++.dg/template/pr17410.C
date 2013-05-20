// PR c++/17410

template <class>
struct Outer {
  template <class> struct Inner {};
}; 

template <class T>
struct A;

template <template <class> class Q, class P>
struct A <Q<P> > {};

template <class T> struct UNRELATED;
template <class T> struct UNRELATED<Outer<void>::Inner<T*> >;

template struct A<Outer<void>::Inner<int*> >;
