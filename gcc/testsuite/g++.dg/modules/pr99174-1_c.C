// { dg-additional-options -fmodules-ts }

import Foo;
import Foo;
import Bar;

int main ()
{
  Foo ();
  Bar ();
}
