// { dg-do assemble  }

template <class U>
class S1
{
  template <class T>
  friend class S2;

  static int i;
};


template <class T>
class S2
{
public:
  static void f() { S1<T>::i = 3; }
};


void g()
{
  S2<double>::f();
  S2<long>::f();
}
