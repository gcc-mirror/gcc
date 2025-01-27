// CWG2867 - Order of initialization for structured bindings.
// { dg-do run }
// { dg-additional-options "-fmodules-ts" }

import "dr2867-2_a.H";

int a, c;

int
main ()
{
  assert (a == 0);
}
