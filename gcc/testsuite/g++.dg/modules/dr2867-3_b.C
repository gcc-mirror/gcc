// CWG2867 - Order of initialization for structured bindings.
// { dg-do run { target c++20 } }
// { dg-additional-options "-fmodules-ts" }
// { dg-add-options tls }
// { dg-require-effective-target tls_runtime }

import "dr2867-3_a.H";

int a, c, d, i;

int
main ()
{
  volatile int u = c1 + x + y + z + w + c2;
  u += d1 + s + t + u + d2;
  u += c3 + x2 + y2 + z2 + w2 + c4;
  u += d3 + s2 + t2 + u2 + d4;
  assert (a == 0);
}
