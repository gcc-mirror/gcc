template <class T> struct A {
  template <class U> struct B {
    template <class V> void f (V) { }
    void g () { }
  };
};

int main ()
{
  A<int>::B<char> b;
  b.f (42);
  b.g ();
}
