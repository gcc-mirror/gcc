// { dg-additional-options "-fmodules-ts -fconcepts" }

import foo;

struct X {};

void foo (int i, X &x)
{
  f1 (i); // ok
  f1 (x); // { dg-error "cannot call" }
}
