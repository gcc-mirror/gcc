// { dg-do compile }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 25 Aug 2003 <nathan@codesourcery.com>
// Origin pr 11871 Dirk Mueller <mueller@kde.org>

// PR c++/11871 Regression

namespace std
{
  class A
  {
  public:
    enum result
    {
      ok
    };
  };

  template<typename T> class B : public A
    {
    public:
      typedef A::result	result;
    };
}

int main()
{
  for(float result = 1.0;;);
}

