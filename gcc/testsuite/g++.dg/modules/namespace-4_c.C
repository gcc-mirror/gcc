// { dg-additional-options "-fmodules-ts" }

import frob;

namespace 
{
  float *nope;
}

float *q ()
{
  f (int (*nope));
  return nope;
}
