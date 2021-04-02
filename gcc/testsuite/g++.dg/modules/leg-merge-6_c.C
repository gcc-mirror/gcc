// { dg-additional-options -fmodules-ts }

import "leg-merge-6_a.H";
import "leg-merge-6_b.H";

int main ()
{
  X x (75);

  return !(int (x) == 75);
}
