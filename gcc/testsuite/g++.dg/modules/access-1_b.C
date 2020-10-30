// { dg-additional-options -fmodules-ts }

export module Bar;
// { dg-module-cmi Bar }

import Foo;

export class Derived : public Base
{
private:
  using Base::m;
};
