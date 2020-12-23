// { dg-additional-options -fmodules-ts }

import bob;

int frob (__sfinae_types::__two *p)
{
  return p->i;
}
