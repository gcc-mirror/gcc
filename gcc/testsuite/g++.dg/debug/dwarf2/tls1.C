// PR c++/66653
// { dg-options "-gdwarf" }

template <typename T> class A
{
  static __thread T a;
};
