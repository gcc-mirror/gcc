// { dg-additional-options "-fmodules-ts -fno-module-lazy" }

template <int I> int fn () 
{
  return I;
}

void f ()
{
  fn<1> ();
}

import "inst-5_a.H";
// no longer need to instantate
