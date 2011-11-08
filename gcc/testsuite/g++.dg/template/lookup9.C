// PR c++/50848
// { dg-options "-fpermissive" }

template<class T> class A {T& foo;};
template<class T> class B: public A<T> {
  void f(){
    foo(1);			// { dg-message "foo" }
  }
};
template class B<int>;
