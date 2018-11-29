// { dg-additional-options -fmodules-ts }

import "leg-merge-2_a.H";
import "leg-merge-2_b.H";

int main ()
{
  X *ptr = 0;

  return ptr != 0;
}
