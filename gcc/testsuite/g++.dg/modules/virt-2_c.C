// { dg-additional-options -fmodules-ts }

import foo;

int Foo ()
{
  Visitor v;

  return !(Visit (&v) == 0);
}

// We do emit Visitor vtable
// andl also we do emit rtti here
// { dg-final { scan-assembler {_ZTV7Visitor:} } }
// { dg-final { scan-assembler {_ZTI7Visitor:} } }
// { dg-final { scan-assembler {_ZTS7Visitor:} } }
