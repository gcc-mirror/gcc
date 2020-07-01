// { dg-additional-options -fmodules-ts }

import foo;
import hidden;

int main ()
{
  X x (2);

  if (frob (x) != 2)
    return 1;

  if (TPL (x) != 2)
    return 2;

  return 0;
}
