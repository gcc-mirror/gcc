// { dg-do run  }
template <class T> struct A {
  template <class U> void f (U u);
};

A<int> a;

template <class T> template <class U> void A<T>::f (U u) { }

int main()
{
  a.f (24);
}

