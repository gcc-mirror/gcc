template <class T>
class C;

template <class T>
struct S
{
  template <class U>
  void f(U u1, U u2) {}

  template <class U>
  void f(U u)
    {
      C<T> ct;
      ct.i = 3;
    }
};


template <class T>
class C
{
  template <class U>
  friend void S<T>::f(U);

  int i;
};


int main()
{
  S<int> si;
  si.f(3.0);
}
