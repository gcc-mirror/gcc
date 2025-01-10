// { dg-do run }
// { dg-additional-options "-fmodules-ts" }

import "decomp-2_a.H";

int
main ()
{
  if (a != 1 || b != 2 || c != 3)
    __builtin_abort ();
}
