// PR c++/65509
// { dg-do compile { target c++11 } }

const int i1 = 1;
const int i2 = 2;

#define SA(X) static_assert (X,#X)
SA(&i1 != &i2);
