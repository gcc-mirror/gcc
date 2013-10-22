// PR c++/48530
// { dg-options -std=c++11 }

template<class T, 
  class = decltype(T())
>
char f(int);

template<class>
char (&f(...))[2];

struct DelDtor {
  DelDtor() = default;
  ~DelDtor() = delete;
};

static_assert(sizeof(f<DelDtor>(0)) != 1, "Error");
