// { dg-do run  }
template <class T> struct A {
  template <class U> void f(U);
};

template <int i> struct B { };

template <class T> template <class U>
void A<T>::f (U)
{
  enum { foo };
  B<foo> b;
}

int main ()
{
  A<char> a;
  a.f (42);
}
