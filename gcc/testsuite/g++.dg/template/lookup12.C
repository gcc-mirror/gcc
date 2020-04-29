// PR c++/94799 - member template function lookup fails.

template<typename T> struct B {
  void foo ();
  int i;
};

template<typename T>
struct D : public B<T> { };

template<typename T>
void fn (D<T> d)
{
  d.template B<T>::foo ();
  d.template B<T>::i = 42;
  D<T>().template B<T>::foo ();
  d.template D<T>::template B<T>::foo ();
  d.template D<T>::template B<T>::i = 10;
}

int
main ()
{
  D<int> d;
  fn(d);
}
