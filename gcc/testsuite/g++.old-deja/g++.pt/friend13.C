template <class T>
class C;

template <class U>
struct S
{
  template <class V>
  void f(V v)
    {
      C<V> cv;
      cv.i = 3;
    }
};


template <class T>
class C
{
  template <class U>
  template <class V>
  friend void S<U>::f(V);

  int i;
};


int main()
{
  S<int> si;
  si.f(3.0);
  S<long> sl;
  sl.f('c');
}
