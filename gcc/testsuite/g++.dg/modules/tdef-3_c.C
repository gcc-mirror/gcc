// { dg-additional-options -fmodules-ts }

import frob;
import quux;

int main ()
{
  frob f {2};

  return !(foo (&f) == 2);
}
