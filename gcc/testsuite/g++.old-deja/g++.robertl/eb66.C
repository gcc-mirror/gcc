// { dg-do run  }
// { dg-skip-if "requires hosted libstdc++ for cassert" { ! hostedlib } }
#include <cassert>
#include <iostream>

int bar ()
{
  throw 100;
}

int main ()
{
  int i = 0;
  try
    {
      i = bar ();
    }
  catch (...)
    {
    }

//  std::cout << "i = " << i << std::endl;
  assert (i == 0) ; 
}




