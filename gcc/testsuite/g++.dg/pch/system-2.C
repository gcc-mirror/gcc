// { dg-skip-if "requires hosted libstdc++ for iostream in system-2.H" { ! hostedlib } }

#include "system-2.H"

int main() 
{
  std::cout << "hello world!" << std::endl;
  return 0;
}
