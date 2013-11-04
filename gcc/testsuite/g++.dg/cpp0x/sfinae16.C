// PR c++/48531
// { dg-options -std=c++11 }

template<class T,
  class = decltype(T())
>
char f(int);

template<class>
double f(...);

struct B2 {
  B2(...);
};

#define SA(X) static_assert ((X), #X);
SA(sizeof(f<B2[2]>(0)) != 1);
