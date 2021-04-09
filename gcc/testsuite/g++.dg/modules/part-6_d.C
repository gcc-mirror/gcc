// { dg-additional-options -fmodules-ts }
module foo;
import :bits;

Foo *Foo::Factory ()
{
  return new Foo ();
}
