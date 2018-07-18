// { dg-options -std=c++17 }

template <class T> struct A
{
  template <auto v>    struct Y;
  template <auto* p>   struct Y<p> { using type1 = decltype (p); };
  template <auto** pp> struct Y<pp> { using type2 = decltype (pp); };
};

int i;
int *p;

A<void>::Y<&i>::type1 t1;
A<void>::Y<&p>::type2 t2;

