// PR c++/48449
// { dg-do compile { target c++11 } }

template<class T, class = decltype(T())>
char f(int);

template<class>
char (&f(...))[2];

struct A { virtual ~A() = 0; };

static_assert(sizeof(f<int&>(0)) != 1, "Error");
static_assert(sizeof(f<void()>(0)) != 1, "Error");
static_assert(sizeof(f<A>(0)) != 1, "Error");
