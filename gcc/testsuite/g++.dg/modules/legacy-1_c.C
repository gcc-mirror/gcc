// { dg-additional-options -fmodules-atom }

import "legacy-1_a.H";

int main ()
{
  return !(frob (-2) == 0);
}
