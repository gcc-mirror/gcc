// { dg-additional-options -fmodules-ts }

import "leg-merge-8_a.H";
import "leg-merge-8_b.H";

int main ()
{
  Tpl<1> one;

  return !(one == 1);
}
