// { dg-additional-options "-fmodules-ts" }

import frob;

namespace 
{
  float *nope;
}

float *q ()
{
  f ();
  return nope;
}
