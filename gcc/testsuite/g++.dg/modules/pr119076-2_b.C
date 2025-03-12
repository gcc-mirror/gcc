// PR c++/119076
// { dg-do run }
// { dg-additional-options "-fmodules-ts" }

import "pr119076-2_a.H";

int
main ()
{
  if (bar () != b || baz () != b)
    __builtin_abort ();
}
