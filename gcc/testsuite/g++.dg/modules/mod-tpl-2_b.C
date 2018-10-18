// { dg-additional-options "-fmodules-ts" }

import frob;

int main ()
{
  X<int> x;

  x.frob (3);

  return ! (x.frobber (-3) == 0);
}
