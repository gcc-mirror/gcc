// PR c++/49085

template <class T>
struct A			// { dg-message "not complete" }
{
  int i, j;
  int ar[__builtin_offsetof(A,j)]; // { dg-error "incomplete type" }
};

A<int> a;
