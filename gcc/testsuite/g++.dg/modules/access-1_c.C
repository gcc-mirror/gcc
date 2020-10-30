// { dg-additional-options -fmodules-ts }

import Bar;
import Foo;

void foo (Derived *d)
{
  d->m = 1; // { dg-error "inaccessible within this context" }
  static_cast<Base *> (d)->m = 1; //ok
}
