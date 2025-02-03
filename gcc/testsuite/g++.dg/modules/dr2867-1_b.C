// CWG2867 - Order of initialization for structured bindings.
// { dg-do run }
// { dg-additional-options "-fmodules-ts" }

import "dr2867-1_a.H";

int a, c, d, i;

int
main ()
{
  assert (a == 0);
}
