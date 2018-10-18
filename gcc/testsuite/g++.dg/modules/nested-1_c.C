// { dg-additional-options "-fmodules-ts" }

import blinky;

int main ()
{
  X::Inner i (7);

  if (i.getter () != 7)
    return 1;

  return 0;
}
