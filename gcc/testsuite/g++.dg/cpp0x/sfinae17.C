// PR c++/48530
// { dg-options -std=c++0x }

template<class T,
	 class = decltype(T{})
>
char f(int);

template<class>
char (&f(...))[2];

struct DelDtor {
  ~DelDtor() = delete;
};

static_assert(sizeof(f<DelDtor[2]>(0)) != 1, "Error");

struct A
{
  static DelDtor *p;
};

template<class T,
	 class = decltype(delete T::p, (T*)0)
>
char g(int);

template<class>
char (&g(...))[2];

static_assert(sizeof(g<DelDtor>(0)) != 1, "Error");
