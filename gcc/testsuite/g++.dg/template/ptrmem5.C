// { dg-do compile }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 24 Mar 2003 <nathan@codesourcery.com>

// PR 10119 (part). We failed to tsubst the args of a template-id-expr

template <class T, void (T::* const U)()> struct Good
{
  static int const value = 0;
};

struct A
{
  template <typename U> void good ()
  {
    int s_id = Good<A, &A::good<U> >::value;
  }
};


int main()
{
  A().good<int>();
}
