// { dg-options "-fmodules++ -fmodule-root=." }

import "mod++-decl-3_a";

int main ()
{
  return bink (2) != 4;
}
