// { dg-additional-options "-fmodules-ts -std=c++2a" }

import foo;

template <foo::Addable T> T Add (T a, T b)
{
  return a + b;
}

void frob ()
{
  Add (1, 2);
  Add ((int *)0, (int *)0); // { dg-error "no match"  }
}
