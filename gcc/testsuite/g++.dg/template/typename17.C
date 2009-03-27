// { dg-do compile }

// This should fail as A::foo<0> is not a typename at all.
struct A
{
  template<int> void foo(int i)
  {
    typename A::foo<0>(i1); // { dg-error "" }
  }
};
