// { dg-options -std=c++17 }

template <class T>
struct A
{
  template <class U>
  struct B
  {
    template <class V>
    B(T,U,V);
  };
};

A<int>::B b(1,2.0,'\3');

template <class,class> class same;
template <class T> class same<T,T> {};
same<decltype(b), A<int>::B<double>> s;
