// PR c++/48535
// { dg-options -std=c++0x }

template<class T,
 class = decltype(T{})
>
char f(int);

template<class>
char (&f(...))[2];

struct A { virtual ~A() = 0; };

static_assert(sizeof(f<A>(0)) != 1, "Error"); // (a)
static_assert(sizeof(f<void()>(0)) != 1, "Error"); // (b)
static_assert(sizeof(f<int&>(0)) != 1, "Error"); // (d)
static_assert(sizeof(f<const int&>(0)) == 1, "Error"); // (e)
static_assert(sizeof(f<int[]>(0)) != 1, "Error"); // (f)
