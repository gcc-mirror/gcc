template <class T> struct B { };

template <class T> struct A {
  template <class U, class V> int operator () (U u, V v);
  template <class U, class V> void operator () (B<U> u, B<V> v) { }
};

int
main ()
{
  A<int> a;
  B<char> b1;
  B<short> b2;
  a (b1, b2);
}
