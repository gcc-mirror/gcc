// PR c++/48531
// { dg-options -std=c++11 }

template<class T,
  class = decltype(T())
>
char f(int);

template<class>
char (&f(...))[2];

static_assert(sizeof(f<int[]>(0)) != 1, "Error");
