// PR c++/91118
// { dg-do compile }
// { dg-additional-options "-fsanitize=undefined" }

#include <iostream>

void
foo ()
{
#pragma omp parallel default(none) shared(std::cerr)
  std::cerr << "hello" << std::endl;
}
