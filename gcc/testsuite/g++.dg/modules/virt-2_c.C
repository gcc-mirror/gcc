// { dg-additional-options -fmodules-ts }

import foo;

int Foo ()
{
  Visitor v;

  return !(Visit (&v) == 0);
}

// We do emit Visitor vtable
// andl also we do emit rtti here
// { dg-final { scan-assembler {_ZTVW3foo7Visitor:} } }
// { dg-final { scan-assembler {_ZTIW3foo7Visitor:} } }
// { dg-final { scan-assembler {_ZTSW3foo7Visitor:} } }
