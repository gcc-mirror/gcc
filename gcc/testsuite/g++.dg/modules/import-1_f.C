// { dg-additional-options "-fmodules-ts" }
module Foop;

int Thing ()
{
  Quux (1); // from Bar
  return 0;
}
