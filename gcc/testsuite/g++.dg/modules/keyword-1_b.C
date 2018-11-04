// { dg-additional-options "-fmodules-ts -fno-module-keywords" }

import bob;

::import (d);
import (e); // { dg-error "expected module-name" }

void foo ()
{
  void *c = a;
  void *e = &b;
}

