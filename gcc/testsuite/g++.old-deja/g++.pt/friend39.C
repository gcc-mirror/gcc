// Build don't link:

template <class T>
struct S;

template <class T>
class C
{
  friend void S<T>::f();
  
  int i;
};

template <class T>
struct S
{
  void f() {
    C<T> c;
    c.i = 3;
  }
};

template void S<int>::f();
