template <class T> struct A {
  template <class U> struct B {
    template <class V> static void f () { }
    void g () { }
  };
};

template <class T, class U>
void f ()
{
  A<T>::template B<U>::template f<T> ();
  typename A<T>::B<U> b;
  typename A<T>::template B<U> b2;
  b.A<T>::template B<U>::~B();
}

template <class T> struct C: public A<T>::B<T> { };
  
int main ()
{
  f<int, char>();
}
