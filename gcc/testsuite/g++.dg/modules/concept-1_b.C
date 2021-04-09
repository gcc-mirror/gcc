// { dg-additional-options "-fmodules-ts -fconcepts" }

import foo;

struct X {};

void foo (int i, X &x)
{
  f1 (i); // ok
  f1 (x); // { dg-error "no match" }
}

// { dg-regexp {[^\n]*concept-1_a.C:7:[0-9]*: error: invalid cast[^\n]*\n} }
