// { dg-do assemble  }

template<class T>
struct Foo { };

template<class T1, class T2>
struct BT { };

template<class T1, class T2>
struct BT< Foo<T1>, Foo<T2> > { static const int i = 1; };

template<class T1, class T2>
struct BT< T1, Foo<T2> > { static const int i = 2; };

template<class T1, class T2>
struct BT< Foo<T1>, T2 > { static const int i = 3; };

template<class T1, class T2>
int foo(Foo<T1>, Foo<T2>)
{
  return 1;
}

template<class T1, class T2>
int foo(T1, Foo<T2>)
{
  return 2;
}

template<class T1, class T2>
int foo(Foo<T1>, T2)
{
  return 3;
}

void f()
{
  BT< double, Foo<int> >::i;
  BT< Foo<int>, Foo<int> >::i;
  BT< Foo<int>, float >::i;
  foo(1.0, Foo<int>());
  foo(Foo<int>(), Foo<int>());
  foo(Foo<int>(), 1.0);
}
