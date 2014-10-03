// { dg-do compile { target c++14 } }

constexpr int f(int i) { int j = i+1; return j; }

constexpr int i = f(41);

#define SA(X) static_assert((X),#X)

SA(i==42);
