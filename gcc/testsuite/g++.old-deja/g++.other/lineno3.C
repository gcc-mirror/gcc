// { dg-do assemble  }
// Bug: g++ gets confused by the #line directive within a method.
// Contributed by Mark Mitchell <mark@codesourcery.com>

struct S 
{
  void f () 
    {
      int i;
      
















      int j;
    }
};
