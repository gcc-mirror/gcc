// PR c++/65734
// { dg-do compile { target c++11 } }

template <class T> struct A
{
  T t;
};

typedef A<int> T[4] alignas (2 * alignof (int));
A<int> a[4];

typedef A<char> T2[4] alignas (2 * alignof (int));

#define SA(X) static_assert((X),#X)
SA(alignof (T) == 2 * alignof(int));
SA(alignof (T2) == 2 * alignof(int));
