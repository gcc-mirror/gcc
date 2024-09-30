// { dg-skip-if "requires hosted libstdc++ for iostream in system-1.H" { ! hostedlib } }

#include "system-1.H"

int main() 
{
  std::cout << "hello world!" << '\n';
  return 0;
}
