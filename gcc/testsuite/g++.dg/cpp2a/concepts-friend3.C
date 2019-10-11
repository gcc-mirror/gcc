// { dg-do compile { target concepts } }

template <class T> concept True = true;

template <True U> struct B { int i = ++U::x; };
template <True U> void f() { ++U::x; }

template <class V> class C
{
  static int x;

  template <True U> friend struct B;
  template <True U> friend void f();
};

int main()
{
  f<C<int>>();
  B<C<int>>();
}
