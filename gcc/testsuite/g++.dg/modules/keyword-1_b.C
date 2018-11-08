// { dg-additional-options "-fmodules-ts" }

import bob;

::import (d);
import (e); // { dg-error "expected module-name" }

void foo ()
{
  void *c = a;
  void *e = &b;
}

