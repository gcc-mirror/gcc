// { dg-do run  }
template <class T> struct A {
  template <class U> struct B {
    template <class V> void f (V) { }
    void g () { }
  };
  template <class W> struct B<W*> {
    void h () { }
  };
};

int main ()
{
  A<int>::B<char> b;
  b.f (42);
  b.g ();
  A<double>::B<void*> b2;
  b2.h ();
}
