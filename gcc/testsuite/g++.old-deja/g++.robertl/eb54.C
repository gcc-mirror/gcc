// { dg-do run  }
#include <iomanip>
#include <iostream>
#include <cstdlib>

int main()
{
  std::cout << std::setbase(3) << std::endl;
  std::exit (0);
}
