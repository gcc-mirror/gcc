// { dg-additional-options "-fmodules-ts" }

module frob;

namespace 
{
void *nope; // ok, different nope
}

void *q (int)
{
  f (bool (nope));
  g (static_cast <int *> (nope));
  return nope; // Ok sees above nope
}
