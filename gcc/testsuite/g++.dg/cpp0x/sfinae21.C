// PR c++/48735
// { dg-options "-std=c++0x" }

template<class T, 
 class = decltype(T{})
>
char f(int);

template<class>
char (&f(...))[2];

struct ND { ND() = delete; };

static_assert(sizeof(f<ND[1]>(0)) != 1, "Error");
