// PR c++/48737
// { dg-options "-std=c++11" }

template<class T>
T&& create();

template<class T, class... Args>
decltype(T{create<Args>()...}, char()) f(int);

template<class, class...>
char (&f(...))[2];

static_assert(sizeof(f<int[1], int, int>(0)) != 1, "Error");
