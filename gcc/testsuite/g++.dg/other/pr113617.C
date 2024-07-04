// PR rtl-optimization/113617
// { dg-do link { target c++11 } }
// { dg-options "-O2" }
// { dg-additional-options "-fPIC" { target fpic } } */
// { dg-additional-options "-shared -DSHARED" { target shared } } */
// { dg-additional-sources pr113617-aux.cc }

#include "pr113617.h"

int z;
long xx1;
void corge() {
  A<long long> a;
  a.foo(xx1, 0);
}

typedef unsigned long int VV __attribute__((vector_size (2 * sizeof (long))));
VV vv;
__attribute__((noipa)) static void fn1 (void) {}
__attribute__((noipa)) static void fn2 (void) {}

void
fn3 ()
{
  VV a = { (unsigned long) &fn1, (unsigned long) &fn2 };
  vv = a;
}
