// { dg-additional-options {-fmodules-ts -Wno-pedantic} }

struct Type {};

import Foo;

bool foo (Type const &t)
{
  return equal (t, t);
}
