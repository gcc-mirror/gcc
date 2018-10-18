// { dg-additional-options -fmodules-ts }

import "legacy-1_a.H";

int main ()
{
  return !(frob (-2) == 0);
}
