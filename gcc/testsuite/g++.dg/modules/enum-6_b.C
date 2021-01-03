// { dg-additional-options -fmodules-ts }

import "enum-6_a.H";

int main ()
{
  return !(getter<int> () == 7);
}
