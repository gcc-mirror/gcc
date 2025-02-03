// CWG2867 - Order of initialization for structured bindings.
// { dg-do run { target c++20 } }
// { dg-additional-options "-fmodules-ts" }
// { dg-add-options tls }
// { dg-require-effective-target tls_runtime }

import "dr2867-4_a.H";

int a, c;

int
main ()
{
  volatile int u = c1 + c2;
  assert (a == 0);
}
