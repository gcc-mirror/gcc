// { dg-do run }
// Test that template friends referring to class template members are
// respected.


template <class T> struct A
{
  int f (T);
  struct AI {
    int f (T);
  };
};

class B
{
  template <class T> friend int A<T>::f (T);
  template <class T> friend struct A<T>::AI;
  int a; // { dg-bogus "" "" { xfail *-*-* } }
public:
  B(): a(0) { }
};

template <class T> int A<T>::f (T)
{
  B b;
  return b.a;
}

template <class T> int A<T>::AI::f (T)
{
  B b;
  return b.a; // { dg-bogus "" "" { xfail *-*-* } }
}

int main ()
{
  A<int> a;
  A<int>::AI ai;

  int r = a.f (0);
  r |= ai.f (0);

  return r;
}
