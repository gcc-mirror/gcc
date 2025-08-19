/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-vect-all -std=c++14 -O3 -mcpu=neoverse-v2  -msve-vector-bits=128" } */

using a = long;
using b = a;
using c = double;
b d;
c e;
void f() {
  for (b g; g < d; ++g)
    e += g;
}

/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" } } */
