// PR c++/85765
// { dg-do compile { target c++11 } }

struct il { il(); il(const il&); };

int* begin(il);

template<class T> T&& declval();

template<class T, class U = decltype(begin(declval<T&>())), decltype(*U(),0) = 0>
U g(T& t, long) { return begin(t); } // #1

template<class T>
int g(T& t, ...); // #2

volatile il a;

auto n = g(a, 0); // calls #1 and ends with a hard error, should call #2
