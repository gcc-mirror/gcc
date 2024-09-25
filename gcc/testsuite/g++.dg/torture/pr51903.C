// { dg-do compile }
// { dg-options "-O2 -fnon-call-exceptions -fno-guess-branch-probability" }
// { dg-skip-if "requires hosted libstdc++ for vector" { ! hostedlib } }

#include <vector>

void foo ()
{
  std::vector < std::vector< int > > (20000);
}
