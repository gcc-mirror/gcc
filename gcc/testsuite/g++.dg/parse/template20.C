// PR c++/28858
// { dg-do compile }

template<int N struct A;  // { dg-error "before" }

bool i = 1 > 0;           // { dg-bogus "" }
int j = i;                // { dg-bogus "" }
