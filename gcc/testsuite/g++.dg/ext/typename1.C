// PR c++/6255
// { dg-options "-fpermissive -w" }

template <class T> struct A { typedef int X; };
template <class T> struct B { typedef A<T> Y; void f (typename Y::X); };
template <class T, class T1, class T2, class T3> struct C : public B<T> { void g (typename B<T>::Y::X); };
template class B<int>;
