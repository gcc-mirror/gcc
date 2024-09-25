// PR c++/91118
// { dg-do compile }
// { dg-additional-options "-fsanitize=undefined" }
// { dg-skip-if "requires hosted libstdc++ for iostream" { ! hostedlib } }

#include <iostream>

void
foo ()
{
#pragma omp parallel default(none) shared(std::cerr)
  std::cerr << "hello" << std::endl;
}
