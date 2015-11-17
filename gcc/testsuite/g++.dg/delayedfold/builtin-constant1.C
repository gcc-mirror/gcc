// { dg-do compile { target c++11 } }

#define SA(X) static_assert ((X),#X)

int i;

SA(__builtin_constant_p (i == 42 && false));
