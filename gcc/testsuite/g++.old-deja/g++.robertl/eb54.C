// { dg-do run  }
// { dg-skip-if "requires hosted libstdc++ for iomanip" { ! hostedlib } }
#include <iomanip>
#include <iostream>
#include <cstdlib>

int main()
{
  std::cout << std::setbase(3) << std::endl;
  std::exit (0);
}
