// { dg-additional-options -fmodules-ts }

import "iostream-1_a.H";

// This hack is needed for the moment.  iostream contains a static var
// definition by which it invokes its global ctors.
// static std::ios_base::Init __ioinit;

int main ()
{
  std::cout << "hello world!\n";
  return 0;
}
