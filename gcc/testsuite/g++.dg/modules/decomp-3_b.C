// PR c++/118513
// { dg-do run }
// { dg-additional-options "-fmodules-ts" }

import "decomp-3_a.H";

int
main ()
{
  if (x != 42 || y != 43)
    __builtin_abort ();
}
