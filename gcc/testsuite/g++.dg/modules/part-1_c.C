// { dg-additional-options -fmodules-ts }

import foo;

int main ()
{
  if (baz () != -1)
    return 1;

  if (foo (42) != 42)
    return 2;

  return 0;
}
