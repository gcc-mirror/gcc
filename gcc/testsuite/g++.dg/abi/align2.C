// PR c++/69560
// { dg-do compile { target { ia32 && c++11 } } }

#define SA(X) static_assert ((X), #X)
SA(alignof(double) == 4);
