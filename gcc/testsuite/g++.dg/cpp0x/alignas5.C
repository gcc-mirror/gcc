// { dg-do compile { target c++11 } }

#define SA(X) static_assert(X,#X)

enum alignas(16) E {};
SA(alignof(E) == 16);
