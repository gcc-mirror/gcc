// PR c++/91364 - P0388R4: Permit conversions to arrays of unknown bound.
// { dg-do compile { target c++20 } }

using P = int *(*)[3];
using S = const int *const (*)[];
using Q = const int *const (*)[3];
using Qi = const int *[3];
using Q2 = Qi const *;

void
f (P p, S s, Q q, Q2 q2)
{
  s = p;
  s = q;
  s = q2;
}
