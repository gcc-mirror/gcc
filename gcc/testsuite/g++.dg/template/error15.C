// PR c++/16929

template <class T>
class A {
  int x;
};

template <class T>
class B {
protected:
    
  A<T> a; // { dg-message "" }
    
  void f(const A<T> * a1 = &a); // { dg-error "non-static" }
    
  void g(void);
};

template <class T>
void B<T>::g(void) {
  f(); // { dg-error "default argument" }
}

template class B<long>;
