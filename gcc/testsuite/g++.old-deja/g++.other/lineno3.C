// Bug: g++ gets confused by the #line directive within a method.
// Contributed by Mark Mitchell <mark@codesourcery.com>
// Build don't link:

struct S 
{
  void f () 
    {
      int i;
      
















      int j;
    }
};
