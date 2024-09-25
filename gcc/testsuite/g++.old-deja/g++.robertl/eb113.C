// { dg-do assemble  }
// { dg-skip-if "requires hosted libstdc++ for iostream" { ! hostedlib } }
#include <iostream>

class X : public std::streambuf
{
} ;
