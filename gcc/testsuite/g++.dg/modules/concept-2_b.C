// { dg-additional-options "-fmodules-ts -fconcepts" }

import foo;

void foo (int i, char c)
{
  static_assert (sizeof (f1 (i)) == sizeof (int));
  static_assert (sizeof (f1 (c)) == sizeof (char));
}
