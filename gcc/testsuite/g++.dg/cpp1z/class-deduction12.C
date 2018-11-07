// { dg-do compile { target c++17 } }

template <class T>
struct A
{
  template<class U, template<U u> class P>
  A(T,U,P<42>);
};

template <int I> struct B { };

int i;
A a(&i,2,B<42>());

template <class,class> class same;
template <class T> class same<T,T> {};
same<decltype(a), A<int*>> s;
