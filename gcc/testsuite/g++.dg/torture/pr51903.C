// { dg-do compile }
// { dg-options "-O2 -fnon-call-exceptions -fno-guess-branch-probability" }

#include <vector>

void foo ()
{
  std::vector < std::vector< int > > (20000);
}
