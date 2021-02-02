// { dg-additional-options "-fmodules-ts" }

import Var;

int main ()
{
  if (counter != 2)
    return 1;
  if (limit != 5)
    return 2;
  static_assert (limit == 5, "huh?");
  return 0;
}
