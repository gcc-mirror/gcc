// { dg-additional-options -fmodules-ts }

import M1;

// Force lazy loading of A since gmf-3 does.
auto x1 = f1();
auto x2 = f2();

#include "gmf-3.h"

A a;
B<int> b;
