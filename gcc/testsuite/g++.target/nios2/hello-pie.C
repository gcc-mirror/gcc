// { dg-do run { target *-*-linux* } }
// { dg-options "-pie -fpie" }
// { dg-output "Hello, pie World" }

// This test used to give an "FDE encoding" error from the linker due to
// the ABI not having appropriate relocations for PIE.

#include <iostream>

int
main ()
{
  std::cout << "Hello, pie World" << std::endl;
}
